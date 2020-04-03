/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: font.c
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
/*******************************************************************************************/


#include "types.h"
#include "memory.h"
#include "string.h"
#include "edit.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "toets.h"
#include "ellipss.h"
#include "nets.h"
#include "calc.h"
#include "calc4.h"
#include "calcdef.h"
#include "math.h"
#include "graphics.h"
#include "calcrect.h"
#include "mainloop.h"
#include "insdel.h"
#include "select.h"
#include "dialogs.h"
#include "stdio.h"
#include "resource.h"
#include "help.h"
#include "files.h"
#include "files2.h"
#include "gerber2.h"
#include "polygon.h"
#include "rules.h"
#include "InputGerber.h"
#include "font.h"
#include "rules.h"
#include "dxf.h"
#include "pcb.h"


#define MAX_SPECIAL_FONTS   32

typedef struct
{
	char FontStr[32];
	HGDIOBJ TextFont;
} SpecialFontRecord;

typedef struct
{
	int32 Info;
	int32 TextChar;
	int32 FontNr;
	int32 Pos;
	int32 Dummy1;
	int32 Dummy2;
} CharCacheRecord;

typedef CharCacheRecord CharCacheArray[200];


CharCacheRecord *CharCache;
uint8 CharInfoMem[16384];
int32 ok, NrFontCacheChars, MemSizeFontCache, MaxFontCacheMemory, MaxNrFontCacheChars, NrSpecialFonts;
char *FontCacheMem;
HGLOBAL GlobalFontCacheMem, FontCachesGlobal;
CharCacheArray *FontCacheChars;
SpecialFontRecord SpecialFonts[MAX_SPECIAL_FONTS];
MAT2 TransFormation;


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemFontChars(int32 count)
{
	HGLOBAL NewMem;

	if (MaxNrFontCacheChars == 0)
	{
		count = max(128, count);

		if ((FontCachesGlobal = GlobalAlloc(GHND, count * sizeof(CharCacheRecord))) == NULL)
			return -1;

		if ((FontCacheChars = (CharCacheArray *) GlobalLock(FontCachesGlobal)) == NULL)
			return -1;

		MaxNrFontCacheChars = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(FontCachesGlobal, count * sizeof(CharCacheRecord), GHND)) == NULL)
			return -1;

		FontCachesGlobal = NewMem;

		if ((FontCacheChars = (CharCacheArray *) GlobalLock(FontCachesGlobal)) == NULL)
			return -1;

		MaxNrFontCacheChars = count;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemFontCacheMem(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxFontCacheMemory == 0)
	{
		if ((GlobalFontCacheMem = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((FontCacheMem = GlobalLock(GlobalFontCacheMem)) == NULL)
			return -1;

		MaxFontCacheMemory = MemSize;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(GlobalFontCacheMem, MemSize, GHND)) == NULL)
			return -1;

		GlobalFontCacheMem = NewMem;

		if ((FontCacheMem = GlobalLock(GlobalFontCacheMem)) == NULL)
			return -1;

		MaxFontCacheMemory = MemSize;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddAreaFillFontCharToCache(int32 TextChar, int32 FontNr, AreaFillRecord * CharAreaFill)
{
	int32 cnt;
	CharCacheRecord *FontChar;
	AreaFillRecord *AreaFill;

	for (cnt = 0; cnt < NrFontCacheChars; cnt++)
	{
		FontChar = &((*FontCacheChars)[cnt]);

		if ((FontChar->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((FontChar->FontNr == FontNr) && (FontChar->TextChar == TextChar))
				break;
		}
	}

	if (cnt < NrFontCacheChars)
	{
		FontChar = &((*FontCacheChars)[cnt]);
		AreaFill = (AreaFillRecord *) & (FontCacheMem[FontChar->Pos]);

		if (AreaFill->MemSize >= CharAreaFill->MemSize)
		{
			memcpy(AreaFill, CharAreaFill, CharAreaFill->MemSize);
			return 0;
		}

		FontChar->Info |= OBJECT_NOT_VISIBLE;
		cnt = NrFontCacheChars;
	}

	if (NrFontCacheChars >= MaxNrFontCacheChars - 1)
		AllocateMemFontChars(MaxNrFontCacheChars + 1024);

	if (CharAreaFill->MemSize + MemSizeFontCache > MaxFontCacheMemory)
		AllocateMemFontCacheMem(CharAreaFill->MemSize + MemSizeFontCache + 16384);

	FontChar = &((*FontCacheChars)[NrFontCacheChars]);
	AreaFill = (AreaFillRecord *) & (FontCacheMem[MemSizeFontCache]);
	memcpy(AreaFill, CharAreaFill, CharAreaFill->MemSize);
	FontChar->Pos = MemSizeFontCache;
	FontChar->FontNr = FontNr;
	FontChar->TextChar = TextChar;
	NrFontCacheChars++;
	MemSizeFontCache += CharAreaFill->MemSize;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetAreaFillFontChar(int32 TextChar, int32 FontNr, AreaFillRecord * CharAreaFill)
{
	int32 cnt;
	CharCacheRecord *FontChar;
	AreaFillRecord *AreaFill;

#ifdef _DEBUG

	if (TextChar == 'e')
		ok = 1;

#endif

	for (cnt = 0; cnt < NrFontCacheChars; cnt++)
	{
		FontChar = &((*FontCacheChars)[cnt]);

		if ((FontChar->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((FontChar->FontNr == FontNr) && (FontChar->TextChar == TextChar))
			{
				AreaFill = (AreaFillRecord *) & (FontCacheMem[FontChar->Pos]);
				memcpy(CharAreaFill, AreaFill, AreaFill->MemSize);
				return 1;
			}
		}
	}

	return MakeAreaFillFontChar(TextChar, FontNr, CharAreaFill);
}

/****************************************************************************
 *  FUNCTION   : IntFromFixed
 *  RETURNS    : int value approximating the FIXED value.
 ****************************************************************************/
int32 IntFromFixed(FIXED f)
{
	if (f.fract >= 0x8000)
		return (f.value + 1);
	else
		return (f.value);
}

/****************************************************************************
 *  FUNCTION   : fxDiv2
 *  RETURNS    : (val1 + val2)/2 for FIXED values
 ****************************************************************************/
FIXED fxDiv2(FIXED fxVal1, FIXED fxVal2)
{
	long l;

	l = (*((int32 *) & (fxVal1)) + *((int32 *) & (fxVal2))) / 2;
	return (*(FIXED *) & l);
}

/****************************************************************************
 *
 *  FUNCTION   : QSpline2Polyline
 *
 *  PURPOSE    : Fake spline cracker.  All it really does is take
 *               the A, B, and C points and pretend they are a polyline,
 *               creating a very unsmooth curve.
 *
 *               In real life, the spline would be digitized.
 *
 *               The calculated points are stored in the given array.
 *
 *  RETURNS    : Number of points added to lpptPolygon.
 *
 *  BONUS INFO : Here is a description of an algorithm to implement
 *               this functionality.
 *
 *** To break up a parabola defined by three points (A,B,C) into straight
 *** line vectors given a maximium error. The maximum error is
 *** 1/resolution * 1/ERRDIV.
 ***
 ***
 ***         B *-_
 ***          /   `-_
 ***         /       `-_
 ***        /           `-_
 ***       /               `-_
 ***      /                   `* C
 ***   A *
 ***
 *** PARAMETERS:
 ***
 *** Ax, Ay contains the x and y coordinates for point A.
 *** Bx, By contains the x and y coordinates for point B.
 *** Cx, Cy contains the x and y coordinates for point C.
 *** hX, hY are handles to the areas where the straight line vectors are going to be put.
 *** count is pointer to a count of how much data has been put into *hX, and *hY.
 ***
 *** F (t) = (1-t)^2 * A + 2 * t * (1-t) * B + t * t * C, t = 0... 1 =>
 *** F (t) = t * t * (A - 2B + C) + t * (2B - 2A) + A  =>
 *** F (t) = alfa * t * t + beta * t + A
 *** Now say that s goes from 0...N, => t = s/N
 *** set: G (s) = N * N * F (s/N)
 *** G (s) = s * s * (A - 2B + C) + s * N * 2 * (B - A) + N * N * A
 *** => G (0) = N * N * A
 *** => G (1) = (A - 2B + C) + N * 2 * (B - A) + G (0)
 *** => G (2) = 4 * (A - 2B + C) + N * 4 * (B - A) + G (0) =
 ***           3 * (A - 2B + C) + 2 * N * (B - A) + G (1)
 ***
 *** D (G (0)) = G (1) - G (0) = (A - 2B + C) + 2 * N * (B - A)
 *** D (G (1)) = G (2) - G (1) = 3 * (A - 2B + C) + 2 * N * (B - A)
 *** DD (G)   = D (G (1)) - D (G (0)) = 2 * (A - 2B + C)
 *** Also, error = DD (G) / 8 .
 *** Also, a subdivided DD = old DD/4.
 ***
 ****************************************************************************/

int QSpline2PolylineNew(LPPOINT lpptBuffer, LPPOINTFX lpqsPoints, int inGY, unsigned int *count, int nAscent);

/****************************************************************************
 *  FUNCTION   : DrawT2Outline
 *
 *  PURPOSE    : Decode the GGO_NATIVE outline, create a polypolygon from it,
 *               and draw it using PolyPolygon.  Color and relative
 *               positioning provided by caller.
 *
 *               Polygon is not actually returned as would be more common
 *               in real usage.  Also, an arbitrary size for the polygon
 *               is specified instead of actually expanding on a need-to-
 *               grow basis.
 *
 *               Error conditions are not handled.
 *
 *  RETURNS    : none.
 ****************************************************************************/
int32 GetFontCharOutline(HDC hDC, LPTTPOLYGONHEADER lpHeader, DWORD size, AreaFillRecord * CharAreaFill)
{
//  TTPOLYGONHEADER ;
//  TTPOLYCURVE ;
	PolygonRecord *CharPolygon, *NewPolygons[16], *Polygon1, *Polygon2;
	LPTTPOLYGONHEADER lpStart;
	LPTTPOLYCURVE lpCurve;
	POINT pt[2000];
	int32 count[50];
	int32 cTotal = 0;			// Total number of points in polypolygon.
	int32 cInCurve;				// Number of points in current curve.
	int32 cCurves = 0;			// Number of curves in polypolygon.
	int32 cInSpline;			// Number of points in digitized spline curve.
	int32 iFirstCurve;			// Index to start point of first curve.
	int32 i = 0, cnt, cnt2;
	int32 i2, ok;
	uint8 *AreaPos, *PolygonPos;
	POINTFX spline[3];
	/*
	typedef struct tagTTPOLYCURVE
	{
	    WORD    wType;
	    WORD    cpfx;
	    POINTFX apfx[1];
	} TTPOLYCURVE, FAR* LPTTPOLYCURVE;

	typedef struct tagTTPOLYGONHEADER
	{
	    DWORD   cb;
	    DWORD   dwType;
	    POINTFX pfxStart;
	} TTPOLYGONHEADER, FAR* LPTTPOLYGONHEADER;
	*/

	lpStart = lpHeader;

	while ((DWORD) lpHeader < (DWORD) (((LPSTR) lpStart) + size))
	{
		if (lpHeader->dwType == TT_POLYGON_TYPE)
		{
			cInCurve = 0;

			// Get to first curve.
			lpCurve = (LPTTPOLYCURVE) (lpHeader + 1);
			iFirstCurve = cTotal;

			while ((DWORD) lpCurve < (DWORD) (((LPSTR) lpHeader) + lpHeader->cb))
			{
				if (lpCurve->wType == TT_PRIM_LINE)
				{
					for (i = 0; i < lpCurve->cpfx; i++)
					{
						pt[cTotal].x = IntFromFixed(lpCurve->apfx[i].x);
						pt[cTotal].y = IntFromFixed(lpCurve->apfx[i].y);
						cTotal++;
						cInCurve++;
					}
				}
				else if (lpCurve->wType == TT_PRIM_QSPLINE)
				{
					// **********************************************
					// Format assumption:
					//   The bytes immediately preceding a POLYCURVE
					//   structure contain a valid POINTFX.
					//
					//   If this is first curve, this points to the
					//      pfxStart of the POLYGONHEADER.
					//   Otherwise, this points to the last point of
					//      the previous POLYCURVE.
					//
					//   In either case, this is representative of the
					//      previous curve's last point.
					// **********************************************
					spline[0] = *(LPPOINTFX) ((LPSTR) lpCurve - sizeof(POINTFX));

					for (i = 0; i < lpCurve->cpfx;)
					{
						// The B point.
						spline[1] = lpCurve->apfx[i++];

						// Calculate the C point.
						if (i == (lpCurve->cpfx - 1))
							spline[2] = lpCurve->apfx[i++];
						else
						{
							// C is midpoint between B and next point.
							spline[2].x = fxDiv2(lpCurve->apfx[i - 1].x, lpCurve->apfx[i].x);
							spline[2].y = fxDiv2(lpCurve->apfx[i - 1].y, lpCurve->apfx[i].y);
						}

						cInSpline = 0;
						QSpline2PolylineNew((LPPOINT) & (pt[cTotal]), spline, -2, &cInSpline, 0);
						cTotal += (uint16) cInSpline;
						cInCurve += (uint16) cInSpline;
//              cInSpline = QSpline2Polyline((LPPOINT)&(pt[cTotal]), spline);
//              cTotal += cInSpline;
//              cInCurve += cInSpline;

						// New A point for next slice of spline.
						spline[0] = spline[2];
					}
				}
				else
					ok = 1;

				// Move on to next curve.
				lpCurve = (LPTTPOLYCURVE) & (lpCurve->apfx[i]);
			}

			// Add points to close curve.
			// Depending on the specific font and glyph being used, these
			// may not always be needed, but it never hurts.
			pt[cTotal].x = lpHeader->pfxStart.x.value;
			pt[cTotal].y = lpHeader->pfxStart.y.value;
			cInCurve++;
			cTotal++;
			/*
			      pt[cTotal].x = pt[iFirstCurve].x;
			      pt[cTotal].y = pt[iFirstCurve].y;
			      cInCurve++;
			      cTotal++;
			*/
			count[cCurves++] = cInCurve;

			// Move on to next polygon.
			lpHeader = (LPTTPOLYGONHEADER) (((LPSTR) lpHeader) + lpHeader->cb);
		}
		else
			ok = 1;
	}

	AreaPos = (uint8 *) CharAreaFill;
	CharPolygon = (PolygonRecord *) ((uint8 *) CharAreaFill + sizeof(AreaFillRecord));
	memset(CharAreaFill, 0, sizeof(AreaFillRecord));
	CharAreaFill->MemSize = sizeof(AreaFillRecord);
	CharAreaFill->minx = 1000000000.0;
	CharAreaFill->miny = 1000000000.0;
	CharAreaFill->maxx = -1000000000.0;
	CharAreaFill->maxy = -1000000000.0;
	ok = 1;
	cnt = 0;
	PolygonPos = (uint8 *) CharPolygon;	//

	for (i = 0; i < cCurves; i++)
	{
		NewPolygons[i] = CharPolygon;
		memset(CharPolygon, 0, sizeof(PolygonInitRecord));
		CharPolygon->NrVertices = count[i];

		for (i2 = 0; i2 < count[i]; i2++)
		{
//      CharPolygon->Points[i2].x=pt[cnt+i2].x;
//      CharPolygon->Points[i2].y=pt[cnt+i2].y;
			CharPolygon->Points[i2].x = (pt[cnt + i2].x) / 800.0;
			CharPolygon->Points[i2].y = (pt[cnt + i2].y) / 800.0;
//      CharPolygon->Points[i2].y=(pt[cnt+i2].y)/800.0+0.4;
		}

		SetMinMaxPolygon(CharPolygon, 0);
		CharAreaFill->minx = min(CharAreaFill->minx, CharPolygon->minx);
		CharAreaFill->miny = min(CharAreaFill->miny, CharPolygon->miny);
		CharAreaFill->maxx = max(CharAreaFill->maxx, CharPolygon->maxx);
		CharAreaFill->maxy = max(CharAreaFill->maxy, CharPolygon->maxy);
		PolygonPos += MemSizePolygon(CharPolygon);
		CharPolygon->PolygonType = 0;
		CharAreaFill->MemSize += MemSizePolygon(CharPolygon);
		CharAreaFill->NrPolygons++;
		CharPolygon = (PolygonRecord *) PolygonPos;

		cnt += count[i];
	}

	for (cnt = 0; cnt < CharAreaFill->NrPolygons; cnt++)
	{
		Polygon1 = NewPolygons[cnt];

		for (cnt2 = cnt + 1; cnt2 < CharAreaFill->NrPolygons; cnt2++)
		{
			Polygon2 = NewPolygons[cnt2];

			if (CheckPolygonCompleetlyInsidePolygon(Polygon2, Polygon1) == 1)
			{
				Polygon2->Special.Test = cnt;
				Polygon2->PolygonType = (Polygon1->PolygonType & 8) ^ 8;
			}
		}
	}

//  CharAreaFill->minx=0.0;
	for (cnt = 0; cnt < CharAreaFill->NrPolygons; cnt++)
	{
		Polygon1 = NewPolygons[cnt];
		MovePolygon(Polygon1, -CharAreaFill->minx, 0.0, 0);
	}

	CharAreaFill->maxx -= CharAreaFill->minx;
	CharAreaFill->minx = 0.0;
	return cCurves;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeAreaFillFontChar(int32 TextChar, int32 FontNr, AreaFillRecord * CharAreaFill)
{
	int32 count, cnt;
	HFONT UsedFont;
	int32 res;
	GLYPHMETRICS GlyphMetrics;
	AreaFillRecord *AreaFill;
	HDC OutputDisplay;
	char *FontStr;

	if ((FontNr == 0) || (FontNr > 16))
		return 0;

	FontStr = Design.UsedFontStr[FontNr - 1];

	for (cnt = 0; cnt < NrSpecialFonts; cnt++)
	{
		if (strcmp(SpecialFonts[cnt].FontStr, FontStr) == 0)
			break;
	}

	if (cnt == NrSpecialFonts)
	{
		if (NrSpecialFonts == MAX_SPECIAL_FONTS)
			return 0;

		UsedFont =
		    CreateFont(1200, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
		               DEFAULT_PITCH, FontStr);

		if (!UsedFont)
			return 0;

		SpecialFonts[cnt].TextFont = UsedFont;
		strcpy(SpecialFonts[cnt].FontStr, FontStr);
		NrSpecialFonts++;
	}
	else
		UsedFont = SpecialFonts[cnt].TextFont;

	AllocateMemTemp3(128 * 1024);
	AreaFill = (AreaFillRecord *) TempMem3;
	TransFormation.eM11.value = 1;
	TransFormation.eM22.value = 1;
	OutputDisplay = GetDC(PCBWindow);
	SelectObject(OutputDisplay, UsedFont);
//  TextChar='o';
//  TextChar=32332;
	count = GetGlyphOutlineW(OutputDisplay, (WCHAR) TextChar, GGO_NATIVE, &GlyphMetrics, 0, NULL, &TransFormation);

	if (count == GDI_ERROR)
	{
		ReleaseDC(PCBWindow, OutputDisplay);
		return 0;
	}

	res =
	    GetGlyphOutlineW(OutputDisplay, (WCHAR) TextChar, GGO_NATIVE, &GlyphMetrics, count, &CharInfoMem,
	                     &TransFormation);

	if (res == GDI_ERROR)
	{
		ReleaseDC(PCBWindow, OutputDisplay);
		return 0;
	}

	res = GetFontCharOutline(OutputDisplay, (LPTTPOLYGONHEADER) & CharInfoMem, count, AreaFill);
	ReleaseDC(PCBWindow, OutputDisplay);

	if (res == 0)
		return 0;

	AddAreaFillFontCharToCache(TextChar, FontNr, AreaFill);
	memcpy(CharAreaFill, AreaFill, AreaFill->MemSize);
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
