/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc2.c
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
#include "calc2.h"
#include "draw.h"
#include "math.h"
#include "sch.h"
#include "calc.h"
#include "calcdef.h"
#include "string.h"
#include "utf8.h"

int32 ok;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CircleTestCircleObjects(double CircleX1, double CircleY1, double CircleThickness1, double CircleX2,
                              double CircleY2, double CircleThickness2)
{
	double hulpx, hulpy;
	double hulp;
	hulpx = (CircleX1 - CircleX2);
	hulpy = (CircleY1 - CircleY2);
	hulp = (double) sqrt(hulpx * hulpx + hulpy * hulpy);

	if (hulp <= ((CircleThickness1 + CircleThickness2) * 0.5))
		return 1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 RectTestCircleObjects(double RectX, double RectY, double RectWidth, double RectHeight, double CircleX,
                            double CircleY, double CircleThickness)
{
	double CircleXmin, CircleXmax, CircleYmin, CircleYmax, RectXmin, RectXmax, RectYmin, RectYmax, xa, ya;
	double r, rxmin, rymin, rxmax, rymax;

	CircleThickness = CircleThickness * (double) 0.5;
	RectWidth = RectWidth * (double) 0.5;
	RectHeight = RectHeight * (double) 0.5;

	CircleXmin = CircleX - CircleThickness;
	CircleYmin = CircleY - CircleThickness;
	CircleXmax = CircleX + CircleThickness;
	CircleYmax = CircleY + CircleThickness;

	RectXmin = RectX - RectWidth;
	RectYmin = RectY - RectHeight;
	RectXmax = RectX + RectWidth;
	RectYmax = RectY + RectHeight;

	if ((CircleXmax < RectXmin) || (CircleXmin > RectXmax) || (CircleYmax < RectYmin) || (CircleYmin > RectYmax))
		return 0;

	if ((CircleXmax <= RectXmax) && (CircleXmin >= RectXmin) && (CircleYmax <= RectYmax) && (CircleYmin >= RectYmin))
		return 1;

	r = CircleThickness;
	r = r * r;

	rxmin = RectXmin - CircleX;
	rxmin = rxmin * rxmin;
	rxmax = RectXmax - CircleX;
	rxmax = rxmax * rxmax;
	rymin = RectYmin - CircleY;
	rymin = rymin * rymin;
	rymax = RectYmax - CircleY;
	rymax = rymax * rymax;

	if (rxmin + rymin <= r)
		return 1;

	if (rxmin + rymax <= r)
		return 1;

	if (rxmax + rymin <= r)
		return 1;

	if (rxmax + rymax <= r)
		return 1;

//  RectTestCircleOutline(x1,y1,x2,255);
	r = CircleThickness;
	r = r * r;
	rxmin = RectXmin - CircleX;
	rxmin = r - rxmin * rxmin;
	rxmax = RectXmax - CircleX;
	rxmax = r - rxmax * rxmax;

	if (rxmin >= 0)
	{
		ya = (double) sqrt(rxmin);

		if (((CircleY - ya >= RectYmin) && (CircleY - ya <= RectYmax))
		        || ((CircleY + ya >= RectYmin) && (CircleY + ya <= RectYmax)))
			return 1;
	}

	if (rxmax >= 0)
	{
		ya = (double) sqrt(rxmax);

		if (((CircleY - ya >= RectYmin) && (CircleY - ya <= RectYmax))
		        || ((CircleY + ya >= RectYmin) && (CircleY + ya <= RectYmax)))
			return 1;
	}

	rymin = RectYmin - CircleY;
	rymin = r - rymin * rymin;
	rymax = RectYmax - CircleY;
	rymax = r - rymax * rymax;

	if (rymin >= 0)
	{
		xa = (double) sqrt(rymin);

		if (((CircleX - xa >= RectXmin) && (CircleX - xa <= RectXmax))
		        || ((CircleX + xa >= RectXmin) && (CircleX + xa <= RectXmax)))
			return 1;
	}

	if (rymax >= 0)
	{
		xa = (double) sqrt(rymax);

		if (((CircleX - xa >= RectXmin) && (CircleX - xa <= RectXmax))
		        || ((CircleX + xa >= RectXmin) && (CircleX + xa <= RectXmax)))
			return 1;
	}

	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetCompProperties(InstanceRecord * Instance, LPSTR PropertyID, LPSTR PropertyValue, int32 mode)
{
	int32 cnt = 0, pos, pos2, LengthValue, LengthIdent, Found;
	LPSTR InstanceAttrBuf;

	if (!Instance)
		return -1;

	InstanceAttrBuf = (LPSTR) Instance->Properties;
	pos = 0;
	Found = 0;

	while ((pos < sizeof(Instance->Properties) - 2) && (InstanceAttrBuf[pos] != 0))
	{
		LengthIdent = strlen((LPSTR) & InstanceAttrBuf[pos]);

		if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(Instance->Properties) - 1))
		{
			pos2 = pos + LengthIdent + 1;
			LengthValue = strlen((LPSTR) & InstanceAttrBuf[pos2]);

			if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(Instance->Properties) - 1))
			{
				if ((strcmp("MULTI_ASSY", &InstanceAttrBuf[pos]) != 0) || (Instance->PlacingOption != -1))
				{
					switch (mode & 0x60)
					{
					case 0:
						if (strcmp(PropertyID, &InstanceAttrBuf[pos]) == 0)
						{
							strcpy(PropertyValue, (LPSTR) & InstanceAttrBuf[pos2]);
							return 0;
						}

						break;

					case 0x20:
						if ((mode & 0x1f) == cnt)
						{
							strcpy(PropertyID, (LPSTR) & InstanceAttrBuf[pos]);
							strcpy(PropertyValue, (LPSTR) & InstanceAttrBuf[pos2]);
							return 0;
						}

						break;
					}

					if (cnt < 0x1f)
						cnt++;
				}
			}

			pos += LengthIdent + LengthValue + 2;
		}
		else
			break;
	}

	if (Instance->PlacingOption != -1)
	{
		Found = 0;
		InstanceAttrBuf = (LPSTR) Instance->Properties;
		pos = 0;

		while ((pos < sizeof(Instance->Properties) - 2) && (InstanceAttrBuf[pos] != 0))
		{
			LengthIdent = strlen((LPSTR) & InstanceAttrBuf[pos]);

			if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(Instance->Properties) - 1))
			{
				pos2 = pos + LengthIdent + 1;
				LengthValue = strlen((LPSTR) & InstanceAttrBuf[pos2]);

				if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(Instance->Properties) - 1))
				{
					if (strcmp("MULTI_ASSY", &InstanceAttrBuf[pos]) == 0)
					{
						Found = 1;
						break;
					}
				}

				pos += LengthIdent + LengthValue + 2;
			}
			else
				break;
		}

		if (!Found)
		{
			switch (mode & 0x60)
			{
			case 0:
				if (strcmp(PropertyID, "MULTI_ASSY") == 0)
				{
					strcpy(PropertyValue, "NP");
					return 0;
				}

				break;

			case 0x20:
				if ((mode & 0x1f) == cnt)
				{
					strcpy(PropertyID, "MULTI_ASSY");
					strcpy(PropertyValue, "NP");
					return 0;
				}
			}

			cnt++;
		}
	}

	switch (mode & 0x60)
	{
	case 0:
	case 0x20:
		return -1;

	case 0x40:
		return cnt;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetInstanceRefInfo(InstanceRecord * Instance, double *x, double *y, int32 * Rotation, LPSTR Ref, int32 mode)
{
	int32 SymbolNr, cnt2, InstanceRotation, ObjectMirrorX, ObjectMirrorY, TextRotation, Alignment, MemPos, lengte;
	SymbolsPosRecord *SymbolPos;
	SymbolRecord *Symbol;
	char str[200];
	double x1, y1, x2, y2, hulp, Size;
#ifdef _DEBUG
	int32 res;
#endif

	if (((Instance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
	        || ((Instance->Info & SHEET_SYMBOL) == SHEET_SYMBOL))
		return -1;

#ifdef _DEBUG

	if (stricmp(Instance->Reference, "R502") == 0)
		ok = 1;

#endif
	strcpy(str, Instance->Reference);
	SymbolNr = -1;

	for (cnt2 = 0; cnt2 < Design.NrSymbols; cnt2++)
	{
		SymbolPos = &((*SymbolsPos)[cnt2]);

		if (stricmpUTF8(SymbolPos->SymbolName, Instance->SymbolName) == 0)
			SymbolNr = cnt2;
	}

	if (SymbolNr == -1)
		return -1;
	else
	{
		MemPos = (*SymbolsPos)[SymbolNr].Pos;

		if (MemPos == -1)
			return -1;
		else
		{
			Symbol = (SymbolRecord *) & (SymbolsMem[MemPos]);

			if ((Symbol->Info & MULTIPLE_SYMBOLS) == 0)
			{
				if (Symbol->NrPartsPerPackage > 1)
				{
					strcat(str, " ");
					str[strlen(str) - 1] = (char) ('A' - 1 + Instance->PackagePartNr);
				}
			}
		}
	}

#ifdef _DEBUG

	if (stricmp(str, "CN2") == 0)
		res = 1;

#endif
	InstanceRotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
	ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
	ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);
	TextRotation = (((Instance->RefInfo >> 8) & 1) + InstanceRotation) & 1;
	Alignment = Instance->RefInfo & 0x0f;
	x1 = Instance->RefOriginX;
	y1 = Instance->RefOriginY;

	x2 = Instance->OriginX;
	y2 = Instance->OriginY;

	switch (InstanceRotation)
	{
	case 0:
		if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
		{
			if (ObjectMirrorX == 1)
			{
				x1 = -x1;

				if (TextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (TextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				if (TextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (TextRotation == 1)
					Alignment = TextMirrorX[Alignment];

				y1 = -y1;
			}
		}
		else
		{
//        if (TextRotation==0) Alignment=TextMirrorX[Alignment];
//        if (TextRotation==1) Alignment=TextMirrorX[Alignment];
		}

		break;

	case 1:					//  90
		hulp = x1;
		x1 = -y1;
		y1 = hulp;

		if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
		{
			if (ObjectMirrorX == 1)
			{
				x1 = -x1;

				if (TextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (TextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				y1 = -y1;

				if (TextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (TextRotation == 1)
					Alignment = TextMirrorX[Alignment];
			}
		}
		else
		{
			if (TextRotation == 0)
				Alignment = TextMirrorX[Alignment];

			if (TextRotation == 1)
				Alignment = TextMirrorY[Alignment];
		}

		break;
	}

	x1 += Instance->OriginX;
	y1 += Instance->OriginY;
	lengte = strlen(str);

	if (mode == 0)
	{
		switch (TextRotation)
		{
		case 0:
			switch (Alignment)
			{
			case ALIGN_LEFT_BOTTOM:
				break;

			case ALIGN_RIGHT_BOTTOM:
				x1 -= lengte;
				break;

			case ALIGN_LEFT_CENTRE:
				y1 -= 0.5;
				break;

			case ALIGN_RIGHT_CENTRE:
				x1 -= lengte;
				y1 -= 0.5;
				break;

			case ALIGN_LEFT_TOP:
				y1 -= 1.0;
				break;

			case ALIGN_RIGHT_TOP:
				x1 -= lengte;
				y1 -= 1.0;
				break;
			}

			break;

		case 1:
			switch (Alignment)
			{
			case ALIGN_LEFT_BOTTOM:
				break;

			case ALIGN_RIGHT_BOTTOM:
				y1 -= lengte;
				break;

			case ALIGN_LEFT_CENTRE:
				x1 += 0.5;
				break;

			case ALIGN_RIGHT_CENTRE:
				x1 += 0.5;
				y1 -= lengte;
				break;

			case ALIGN_LEFT_TOP:
				//          x1+=1.0;
				break;

			case ALIGN_RIGHT_TOP:
				x1 += 1.0;
				y1 -= lengte;
				break;
			}

			break;
		}
	}
	else
	{
		Size = 0.9;

		if (TextRotation == 0)
		{
			switch (Alignment)
			{
			case ALIGN_LEFT_BOTTOM:
				x1 += Size * 0.1 * DefFontSize;
				break;

			case ALIGN_RIGHT_BOTTOM:
				x1 -= lengte * Size * DefFontSize * PDFCharWidthFactor;
				break;

			case ALIGN_LEFT_CENTRE:
				x1 += Size * 0.1 * DefFontSize;
				y1 -= Size * 0.45 * DefFontSize;
				break;

			case ALIGN_RIGHT_CENTRE:
				y1 -= Size * 0.45 * DefFontSize;
				x1 -= lengte * Size * DefFontSize * PDFCharWidthFactor;
				break;

			case ALIGN_LEFT_TOP:
				x1 += Size * 0.1 * DefFontSize;
				y1 -= Size * 1.15 * DefFontSize;
				break;

			case ALIGN_RIGHT_TOP:
				y1 -= Size * 1.15 * DefFontSize;
				x1 -= lengte * Size * DefFontSize * PDFCharWidthFactor;
				break;
			}
		}
		else
		{
			x1 += Size * -0.3 * DefFontSize;

			switch (Alignment)
			{
			case ALIGN_LEFT_BOTTOM:
				y1 += Size * 0.1 * DefFontSize;
				break;

			case ALIGN_RIGHT_BOTTOM:
				y1 -= lengte * Size * DefFontSize * PDFCharWidthFactor;
				break;

			case ALIGN_LEFT_CENTRE:
				y1 += Size * 0.1 * DefFontSize;
				x1 += Size * 0.45 * DefFontSize;
				break;

			case ALIGN_RIGHT_CENTRE:
				x1 += Size * 0.45 * DefFontSize;
				y1 -= lengte * Size * DefFontSize * PDFCharWidthFactor;
				break;

			case ALIGN_LEFT_TOP:
				y1 += Size * 0.1 * DefFontSize;
				x1 += Size * 1.15 * DefFontSize;
				break;

			case ALIGN_RIGHT_TOP:
				y1 -= lengte * Size * DefFontSize * PDFCharWidthFactor;
				x1 += Size * 1.15 * DefFontSize;
				break;
			}
		}
	}

	*x = x1;
	*y = y1;
	*Rotation = TextRotation;
	strcpy(Ref, str);
	return 0;
//  DrawPDFStr(x1,y1,(double)0.9,(double)TextRotation*90.0,Alignment,str);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
