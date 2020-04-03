/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: font2.c
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


typedef int32 Int32Array[8192];
typedef uint8 BYTEArray[8192];

typedef struct
{
	int32 FontNr;
	int32 CharCode;
	int32 NrFontPolygons;
	float DiffX, DiffY;
	int32 Dummy[8];
	int32 FontPolygonsPos[8];
} FontRecord;


ObjectRecord NewObject;
ObjectLineRecord NewObjectLine;

ObjectPolygonRecord *CharPolygons[8];
uint8 TempMem3[128 * 1024], InfoMem[16384];
POINT GlyphPoints[2000];

GLYPHMETRICS GlyphMetrics;
MAT2 TransFormation;

extern HDC OutputDisplay;
extern HWND SelectLayerWindow;
extern int32 NrWarnings;


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
void DrawT2Outline(HDC hDC, LPTTPOLYGONHEADER lpHeader, DWORD size)
{
//  TTPOLYGONHEADER ;
//  TTPOLYCURVE ;
	LPTTPOLYGONHEADER lpStart;
	LPTTPOLYCURVE CurveP;
	int32 count[50];
	int32 cTotal = 0;			// Total number of points in polypolygon.
	int32 PointsInCurve;		// Number of points in current curve.
	int32 NrCurves = 0;			// Number of curves in polypolygon.
	int cInSpline;				// Number of points in digitized spline curve.
	int32 FirstCurve;			// Index to start point of first curve.
	int32 i = 0, cnt, cnt2, cnt3, ok;
	POINTFX spline[3];

	lpStart = lpHeader;

	while ((DWORD) lpHeader < (DWORD) (((LPSTR) lpStart) + size))
	{
		if (lpHeader->dwType == TT_POLYGON_TYPE)
		{
			PointsInCurve = 0;

			// Get to first curve.
			CurveP = (LPTTPOLYCURVE) (lpHeader + 1);
			FirstCurve = cTotal;

			while ((DWORD) CurveP < (DWORD) (((LPSTR) lpHeader) + lpHeader->cb))
			{
				if (CurveP->wType == TT_PRIM_LINE)
				{
					for (i = 0; i < CurveP->cpfx; i++)
					{
						GlyphPoints[cTotal].x = IntFromFixed(CurveP->apfx[i].x);
						GlyphPoints[cTotal].y = IntFromFixed(CurveP->apfx[i].y);
						cTotal++;
						PointsInCurve++;
					}
				}
				else if (CurveP->wType == TT_PRIM_QSPLINE)
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
					spline[0] = *(LPPOINTFX) ((LPSTR) CurveP - sizeof(POINTFX));

					for (i = 0; i < CurveP->cpfx;)
					{
						// The B point.
						spline[1] = CurveP->apfx[i++];

						// Calculate the C point.
						if (i == (CurveP->cpfx - 1))
							spline[2] = CurveP->apfx[i++];
						else
						{
							// C is midpoint between B and next point.
							spline[2].x = fxDiv2(CurveP->apfx[i - 1].x, CurveP->apfx[i].x);
							spline[2].y = fxDiv2(CurveP->apfx[i - 1].y, CurveP->apfx[i].y);
						}

						cInSpline = 0;
						QSpline2PolylineNew((LPPOINT) & (GlyphPoints[cTotal]), spline, -2, &cInSpline, 0);
						cTotal += (uint16) cInSpline;
						PointsInCurve += (uint16) cInSpline;
//              cInSpline = QSpline2Polyline((LPPOINT)&(GlyphPoints[cTotal]), spline);
//              cTotal += cInSpline;
//              PointsInCurve += cInSpline;

						// New A point for next slice of spline.
						spline[0] = spline[2];
					}
				}
				else
					ok = 1;

				// Move on to next curve.
				CurveP = (LPTTPOLYCURVE) & (CurveP->apfx[i]);
			}

			// Add points to close curve.
			// Depending on the specific font and glyph being used, these
			// may not always be needed, but it never hurts.
			GlyphPoints[cTotal].x = lpHeader->pfxStart.x.value;
			GlyphPoints[cTotal].y = lpHeader->pfxStart.y.value;
			PointsInCurve++;
			cTotal++;
			/*
			      GlyphPoints[cTotal].x = GlyphPoints[FirstCurve].x;
			      GlyphPoints[cTotal].y = GlyphPoints[FirstCurve].y;
			      PointsInCurve++;
			      cTotal++;
			*/
			count[NrCurves++] = PointsInCurve;

			// Move on to next polygon.
			lpHeader = (LPTTPOLYGONHEADER) (((LPSTR) lpHeader) + lpHeader->cb);
		}
		else
			ok = 1;
	}

	ok = 1;
	cnt3 = 0;

	for (cnt = 0; cnt < NrCurves; cnt++)
	{
		CharPolygons[cnt]->NrVertices = count[cnt];

		for (cnt2 = 0; cnt2 < count[cnt]; cnt2++)
		{
			CharPolygons[cnt]->Points[cnt2].x = GlyphPoints[cnt3 + cnt2].x * 1000.0;
			CharPolygons[cnt]->Points[cnt2].y = GlyphPoints[cnt3 + cnt2].y * 1000.0;
		}

		cnt3 += count[i];
	}

	ok = 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void GetFontPolygon(int32 mode)
{
	int32 count, ok, cnt;
	HFONT PinTextFont;
	int32 res;
	WCHAR TextChar;

	/*
	  PinTextFont=CreateFont(1200,0,0,0,0,0,0,0,ANSI_CHARSET,
	                         OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
	                         FIXED_PITCH,"Courier New");
	  PinTextFont=CreateFont(1200,0,0,0,0,0,0,0,ANSI_CHARSET,
	                         OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
	                         DEFAULT_PITCH,"Arial");
	  PinTextFont=CreateFont(1200,0,0,0,0,0,0,0,ANSI_CHARSET,
	                         OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
	                         DEFAULT_PITCH,"Arial Unicode MS");
	*/
	if (!CharPolygons[0])
	{
		for (cnt = 0; cnt < 8; cnt++)
			CharPolygons[cnt] = (ObjectPolygonRecord *) & TempMem3[cnt * 16384];

		TransFormation.eM11.value = 1;
		TransFormation.eM22.value = 1;
	}

//  TextChar='o';
	TextChar = 32332;
	count = GetGlyphOutlineW(OutputDisplay, TextChar, GGO_NATIVE, &GlyphMetrics, 0, NULL, &TransFormation);
	res = GetGlyphOutlineW(OutputDisplay, TextChar, GGO_NATIVE, &GlyphMetrics, count, &InfoMem, &TransFormation);

	DrawT2Outline(OutputDisplay, (LPTTPOLYGONHEADER) & InfoMem, count);

	ok = 1;

}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
