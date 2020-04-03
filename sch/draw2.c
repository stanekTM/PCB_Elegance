/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw2.c
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
#include "draw2.h"
#include "draw.h"
#include "calc.h"
#include "calcdef.h"
#include "line2.h"
#include "rect.h"
#include "sch.h"
#include "math.h"
#include "time.h"
#include "ellipss.h"
#include "string.h"
#include "movecomp.h"
#include "graphics.h"
#include "resource.h"
#include "utf8.h"


extern COLORREF GridColor, LineColor, NetPinsColor, SCHColors[64];

extern int32 SelectColorMode;

int32 SelectedColorNr = -1;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawObjects(double OX, double OY, int32 Mode)
{
	int32 cnt;
	ObjectRecord *Object;
	int32 TempBackGroundActive;

	TempBackGroundActive = BackGroundActive;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObject(Object, OX, OY, Mode);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawObject(ObjectRecord * Object, double OX, double OY, int32 Mode)
{
	double xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4, xx2a, yy2a, Xmax, Xmin, Ymax, Ymin, Rotation, Angle, Length;
	int32 ok, NrLines, cnt2, lengte, x1a, y1a, x2a, y2a, x3a, y3a, AddMode, LinePos[16], cnt3, LineMode, TextAlignment,
	      TextRotation, ObjectType;
	uint8 CircleMode;
	char str[MAX_LENGTH_STRING], TextStr[1024], NewPinBusStr[512];
	char TextStrings[64][128];
	char PinBusStrings[16][64];

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	yy2 = Object->y2;

	ObjectType = Object->ObjectType;

	switch (ObjectType)
	{
	case SYMBOL_LINE:
		Xmin = xx1;
		Ymin = yy1;
		Xmax = xx2;
		Ymax = yy2;

		if (Xmin > Xmax)
		{
			Xmin = xx2;
			Xmax = xx1;
		}

		if (Ymin > Ymax)
		{
			Ymin = yy2;
			Ymax = yy1;
		}

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = MultX(xx1 + OX);

			if (ReverseY)
				y1a = MultY(yy1 + OY);
			else
				y1a = Mult(yy1 - Yoffset + OY);

			x2a = MultX(xx2 + OX);

			if (ReverseY)
				y2a = MultY(yy2 + OY);
			else
				y2a = Mult(yy2 - Yoffset + OY);

			LineMode = Object->Info3;
			InitDrawingObjectLines(Mult(Object->Thickness));
			DrawLine(x1a, y1a, x2a, y2a);

			if (Object->Info3 != 0)
			{
				ConvNormalCoorToPolar(xx1, yy1, xx2, yy2, &Angle, &Length);

				if ((Object->Info3 & 1) == 1)
				{
					xx3 = xx1 + cos(Angle) * ARROW_LENGTH;
					yy3 = yy1 + sin(Angle) * ARROW_LENGTH;
					xx4 = xx3;
					yy4 = yy3;
					RotatePointFromOtherPoint(&xx4, &yy4, xx1, yy1, 30.0);
					x3a = MultX(xx4);
					y3a = MultY(yy4);
					DrawLine(x1a, y1a, x3a, y3a);
					xx4 = xx3;
					yy4 = yy3;
					RotatePointFromOtherPoint(&xx4, &yy4, xx1, yy1, -30.0);
					x3a = MultX(xx4);
					y3a = MultY(yy4);
					DrawLine(x1a, y1a, x3a, y3a);
				}

				if ((Object->Info3 & 2) == 2)
				{
					xx3 = xx2 - cos(Angle) * ARROW_LENGTH;
					yy3 = yy2 - sin(Angle) * ARROW_LENGTH;
					xx4 = xx3;
					yy4 = yy3;
					RotatePointFromOtherPoint(&xx4, &yy4, xx2, yy2, 30.0);
					x3a = MultX(xx4);
					y3a = MultY(yy4);
					DrawLine(x2a, y2a, x3a, y3a);
					xx4 = xx3;
					yy4 = yy3;
					RotatePointFromOtherPoint(&xx4, &yy4, xx2, yy2, -30.0);
					x3a = MultX(xx4);
					y3a = MultY(yy4);
					DrawLine(x2a, y2a, x3a, y3a);
				}
			}
		}

		break;

	case SYMBOL_RECT:
		xx2a = xx2 / 2;
		yy2a = yy2 / 2;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = MultX(xx1 + OX);

			if (ReverseY)
				y1a = MultY(yy1 + OY);
			else
				y1a = Mult(yy1 - Yoffset + OY);

			if (Object->Thickness == 0.0)
				InitDrawingObjectRects(0, 1);
			else
				InitDrawingObjectRects(Mult(Object->Thickness), 0);

			rect3(x1a, y1a, Mult(xx2), Mult(yy2));
		}

		break;

	case SYMBOL_CIRCLE:
		xx2a = xx2 / 2;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - xx2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + xx2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			CircleMode = (uint8) Object->Info2;
#ifdef _DEBUG

			if (CircleMode == 255)
				ok = 1;

#endif
			LineMode = Object->Info3;
			x1a = MultX(xx1 + OX);

			if (ReverseY)
				y1a = MultY(yy1 + OY);
			else
				y1a = Mult(yy1 - Yoffset + OY);

			if (Object->Thickness == 0.0)
			{
				InitDrawingObjectCircles(0, 1);
				CircleMode = 255;
			}
			else
				InitDrawingObjectCircles(Mult(Object->Thickness), 0);

			ellips2(x1a, y1a, Mult(xx2), Mult(xx2), CircleMode);
		}

		break;

	case SYMBOL_ARC:
		xx3 = Object->x3;
		yy3 = Object->y3;
		xx4 = Object->x4;
		yy4 = Object->y4;
		xx2a = xx2 / 2;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - xx2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + xx2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = MultX(xx1 + OX);

			if (ReverseY)
				y1a = MultY(yy1 + OY);
			else
				y1a = Mult(yy1 - Yoffset + OY);

			LineMode = Object->Info3;
			InitDrawingObjectArcs(Mult(Object->Thickness));

			if (ReverseY)
				SpecialArc(x1a, y1a, Mult(xx2), Mult(xx2), MultX(xx1 + xx3 + OX), MultY(yy1 + yy3 + OY),
				           MultX(xx1 + xx4 + OX), MultY(yy1 + yy4 + OY));
			else
				SpecialArc(x1a, y1a, Mult(xx2), Mult(xx2), Mult(xx1 + xx3 - Xoffset + OX) + DrawWindowMinX,
				           Mult(yy1 + yy3 - Yoffset + OY), Mult(xx1 + xx4 - Xoffset + OX) + DrawWindowMinX,
				           Mult(yy1 + yy4 - Yoffset + OY));
		}

		break;

	case SYMBOL_TEXT:
		TextAlignment = (Object->Info2 & 0x0f);
		TextRotation = (Object->Info2 >> 4) & 3;
		Rotation = (double) TextRotation *90.0;

		if (ConvertTextString(Object->Text1, TextStr) == -1)
			strcpy(TextStr, Object->Text1);

		Xmin = 1e9;
		Xmax = -1e9;
		Ymin = 1e9;
		Ymax = -1e9;
		Length = strlen(TextStr);
#ifdef _DEBUG

		if (Length > 120)
			ok = 1;

#endif

		if (InRange(xx2, (double) 1.0))
			xx2 = (double) 0.9;

		NrLines = 0;
		memset(TextStrings, 0, sizeof(TextStrings));
		cnt3 = 0;
		cnt2 = cnt3;

		while (cnt3 < Length + 1)
		{
			if ((TextStr[cnt3] == '\r') || ((cnt3 == Length) && (TextStr[cnt3 - 1] != '\n')))
			{
				if (NrLines < 64)
				{
					if (cnt3 - cnt2 > 0)
					{
						strncpy((LPSTR) & TextStrings[NrLines], (LPSTR) & TextStr[cnt2], min(127, cnt3 - cnt2));

						if ((Rotation == 0.0) || (InRangeSpecial(Rotation, 90.0, 0.01)))
						{
							if (Rotation == 0.0)
								GetMinMaxText(xx1, yy1, xx2, 0, 0, TextAlignment, (LPSTR) & TextStrings[NrLines]);
							else
								GetMinMaxText(xx1, yy1, xx2, 0, 1, TextAlignment, (LPSTR) & TextStrings[NrLines]);
						}
						else
						{
							GetMinMaxText2(xx1, yy1, xx2, 0, Rotation, TextAlignment, 0,
							               (LPSTR) & TextStrings[NrLines]);
						}

						Xmin = min(Xmin, TextMinX);
						Ymin = min(Ymin, TextMinY);
						Xmax = max(Xmax, TextMaxX);
						Ymax = max(Ymax, TextMaxY);
					}

					NrLines++;
					xx1 += sin(ANGLE_CONVERT(Rotation)) * xx2;
					yy1 -= cos(ANGLE_CONVERT(Rotation)) * xx2;
				}

				cnt3 += 1;
				cnt2 = cnt3 + 1;
			}

			cnt3++;
		}

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			InitDrawingObjectTexts(Mult(Object->Thickness));
			xx1 = Object->x1;
			yy1 = Object->y1;

			for (cnt3 = 0; cnt3 < NrLines; cnt3++)
			{
				DrawStr(xx1 + OX, yy1 + OY, xx2, TextRotation, TextAlignment, TextStrings[cnt3]);
				xx1 += sin(ANGLE_CONVERT(Rotation)) * xx2;
				yy1 -= cos(ANGLE_CONVERT(Rotation)) * xx2;
			}
		}

		break;

	case SYMBOL_PIN_TEXT:
		TextAlignment = (Object->Info2 & 0x0f);
		TextRotation = (Object->Info2 >> 4) & 3;
		GetMinMaxText(xx1 + OX, yy1 + OY, xx2, 0, TextRotation, TextAlignment, Object->Text1);

		if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
		{
			InitDrawingSymbolPinTexts(Mult(STANDARD_LINE_THICKNESS));

			if (InRange(xx2, (double) 1.0))
				xx2 = (double) 0.9;

			DrawStr(xx1 + OX, yy1 + OY, xx2, TextRotation, TextAlignment, Object->Text1);
		}

		ok = 1;
		break;

	case SYMBOL_POWERPIN_TEXT:
		TextAlignment = (Object->Info2 & 0x0f);
		TextRotation = (Object->Info2 >> 4) & 3;
		strcpy(str, Object->Text1);
		strcat(str, " : ");
		strcat(str, Object->Text2);
		GetMinMaxText(xx1 + OX, yy1 + OY, xx2, 0, TextRotation, TextAlignment, str);

		if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
		{
			InitDrawingSymbolPowerPinTexts(Mult(STANDARD_LINE_THICKNESS));

			if (InRange(xx2, (double) 1.0))
				xx2 = (double) 0.9;

			DrawStr(xx1 + OX, yy1 + OY, xx2, TextRotation, TextAlignment, str);
		}

		ok = 1;
		break;

	case SYMBOL_PINBUS_TEXT:
		TextAlignment = (Object->Info2 & 0x0f);
		TextRotation = (Object->Info2 >> 4) & 1;
		AddMode = 0;

		if (TextRotation)
			AddMode += 4;

		if ((TextAlignment == 6) || (TextAlignment == 8))
			AddMode += 1;		// Mirror X

		if ((TextAlignment == 2) || (TextAlignment == 8))
			AddMode += 2;		// Mirror Y

		strcpy(NewPinBusStr, Object->Text1);
		ConvertPinBusStr(Object, NewPinBusStr, 0);
		lengte = strlen(NewPinBusStr);
		cnt2 = 0;
#ifdef _DEBUG

		if (stricmp(Object->Text2, "a[0:15]") == 0)
			ok = 1;

#endif
		NrLines = 0;
		memset(LinePos, 0, sizeof(LinePos));
		memset(PinBusStrings, 0, sizeof(PinBusStrings));
		LinePos[0] = 0;

		while (cnt2 < lengte)
		{
			if (NewPinBusStr[cnt2] == '\\')
			{
				memmove(&PinBusStrings[NrLines], &NewPinBusStr[LinePos[NrLines]], cnt2 - LinePos[NrLines]);

				if (NrLines < 7)
					NrLines++;

				LinePos[NrLines] = cnt2 + 1;
			}

			cnt2++;
		}

		if (NewPinBusStr[lengte - 1] != '\\')
		{
			memmove(&PinBusStrings[NrLines], &NewPinBusStr[LinePos[NrLines]], cnt2 - LinePos[NrLines]);
			LinePos[NrLines] = cnt2;
			NrLines++;
		}

		xx3 = xx1;
		yy3 = yy1;

		switch (AddMode)
		{
		case 2:				// Rotation = 0 , MirrorY = 1 , MirrorX = 0
		case 3:				// Rotation = 0 , MirrorY = 1 , MirrorX = 1
			yy3 += (double) (NrLines - 1);
			break;

		case 6:				// Rotation = 1 , MirrorY = 1 , MirrorX = 0
		case 7:				// Rotation = 1 , MirrorY = 1 , MirrorX = 1
			xx3 -= (double) (NrLines - 1);
			break;
		}

		InitDrawingSymbolPinBusTexts(Mult(STANDARD_LINE_THICKNESS));

		for (cnt2 = 0; cnt2 < NrLines; cnt2++)
		{
			GetMinMaxText(xx3 + OX, yy3 + OY, xx2, 0, TextRotation, TextAlignment, PinBusStrings[cnt2]);

			if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
			{
				if (InRange(xx2, (double) 1.0))
					xx2 = (double) 0.9;

				DrawStr(xx3 + OX, yy3 + OY, xx2, TextRotation, TextAlignment, PinBusStrings[cnt2]);
			}

			switch (AddMode)
			{
			case 0:			// Rotation = 0 , MirrorY = 0 , MirrorX = 0
			case 1:			// Rotation = 0 , MirrorY = 0 , MirrorX = 1
			case 2:			// Rotation = 0 , MirrorY = 1 , MirrorX = 0
			case 3:			// Rotation = 0 , MirrorY = 1 , MirrorX = 1
				yy3 -= (double) 1.0;
				break;

			case 4:			// Rotation = 1 , MirrorY = 0 , MirrorX = 0
			case 5:			// Rotation = 1 , MirrorY = 0 , MirrorX = 1
			case 6:			// Rotation = 1 , MirrorY = 1 , MirrorX = 0
			case 7:			// Rotation = 1 , MirrorY = 1 , MirrorX = 1
				xx3 += (double) 1.0;
				break;
			}
		}

		ok = 1;
		break;

	}

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawReferenceInstance(InstanceRecord * Instance, double OffsetX, double OffsetY, int32 Mode)
{
	double x1, y1, x2 = 0.0, y2 = 0.0, hulp, minx, miny, maxx, maxy;
	int32 InstanceInfo, TextRotation, Alignment, ObjectMirrorX, ObjectMirrorY, Rotation, cnt, MemPos, SymbolNr;
	char str[MAX_LENGTH_STRING];
	ObjectRecord *Object;
	SymbolRecord *Symbol;
	SymbolsPosRecord *SymbolPos;
#ifdef _DEBUG
	int32 res;
#endif

	InstanceInfo = Instance->Info;

	if ((!EditingSymbol)
	        && (((Instance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
	            || ((InstanceInfo & SHEET_SYMBOL) == SHEET_SYMBOL)))
		return;

	strcpy(str, Instance->Reference);

	if (!EditingSymbol)
	{
		SymbolNr = -1;

		for (cnt = 0; cnt < Design.NrSymbols; cnt++)
		{
			SymbolPos = &((*SymbolsPos)[cnt]);

			if (stricmpUTF8(SymbolPos->SymbolName, Instance->SymbolName) == 0)
				SymbolNr = cnt;
		}

		if (SymbolNr == -1)
			return;

		MemPos = (*SymbolsPos)[SymbolNr].Pos;

		if (MemPos == -1)
			return;

		Symbol = (SymbolRecord *) & (SymbolsMem[MemPos]);

// PackagePartNr
// NrPartsPerPackage
		if ((Symbol->Info & MULTIPLE_SYMBOLS) == 0)
		{
			if (Symbol->NrPartsPerPackage > 1)
			{
				strcat(str, " ");
				str[strlen(str) - 1] = (char) ('A' - 1 + Instance->PackagePartNr);
			}
		}

		/*
		    if ((InstanceInfo & SHEET_SYMBOL) == SHEET_SYMBOL) {
		      if (str[0]==0) {
		        strcpy(str,Instance->PartDescription);
		      }
		    }
		*/
	}

	if ((!EditingSheetSymbol) && (AppendPropertiesToReferences)
	        && ((Instance->Properties[0] != 0) || (Instance->PlacingOption != -1)))
		strcat(str, " (...)");

#ifdef _DEBUG

	if (stricmp(str, "U4") == 0)
		res = 1;

#endif
	Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
	ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
	ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);
	TextRotation = (((Instance->RefInfo >> 8) & 1) + Rotation) & 1;
	Alignment = Instance->RefInfo & 0x0f;
	x1 = Instance->RefOriginX;
	y1 = Instance->RefOriginY;

	if (Mode & 1)
	{
		NrObjects = 0;
		InstanceToObject(Instance, 0.0, 0.0, 0);
		minx = 10000.0;
		miny = 10000.0;
		maxx = -10000.0;
		maxy = -10000.0;

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			switch (Object->ObjectType)
			{
			case SYMBOL_LINE:
				minx = min(min(Object->x1, Object->x2), minx);
				miny = min(min(Object->y1, Object->y2), miny);
				maxx = max(max(Object->x1, Object->x2), maxx);
				maxy = max(max(Object->y1, Object->y2), maxy);
				break;

			case SYMBOL_RECT:
				minx = min((Object->x1 - Object->x2) * 0.5, minx);
				miny = min((Object->y1 - Object->y2) * 0.5, miny);
				maxx = max((Object->x1 + Object->x2) * 0.5, maxx);
				maxy = max((Object->y1 + Object->y2) * 0.5, maxy);
				break;

			case SYMBOL_ARC:
				break;
			}

		}

		x2 = (minx + maxx) * 0.5;
		y2 = (miny + maxy) * 0.5;
	}

	switch (Rotation)
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

	x1 += Instance->OriginX + OffsetX;
	y1 += Instance->OriginY + OffsetY;
	GetMinMaxText(x1, y1, (double) 1.0, 0, TextRotation, Alignment, str);

	if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
	{
		InitDrawingInstanceRefText(Mult(STANDARD_LINE_THICKNESS));
		DrawStr(x1, y1, (double) 0.9, TextRotation, Alignment, str);

		if (Mode & 1)
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));

//    DrawStr(x1,y1,(double)1.0,TextRotation,Alignment,str);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawValueInstance(InstanceRecord * Instance, double OffsetX, double OffsetY, int32 Mode)
{
	double x1, y1, x1a, y1a, x2 = 0.0, y2 = 0.0, hulp, minx, maxx, miny, maxy;
	int32 InstanceInfo, TextRotation, Alignment, ObjectMirrorX, ObjectMirrorY, Rotation, cnt;
	ObjectRecord *Object;
	char str[100];
#ifdef _DEBUG
	int32 ok;
#endif

//  NrObjects=0;
//  Instance=(InstanceRecord *)&((*Instances)[InstanceNr]);
	InstanceInfo = Instance->Info;

#ifdef _DEBUG

	if (Printing)
	{
		if (stricmp(Instance->Value, "19.6608mhz") == 0)
			ok = 1;
	}

#endif

	if ((!EditingSymbol) && ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE))
		return;

	Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
	ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
	ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);
	TextRotation = (((Instance->ValueInfo >> 8) & 1) + Rotation) & 1;
	Alignment = Instance->ValueInfo & 0x0f;
	x1 = Instance->ValueOriginX;
	y1 = Instance->ValueOriginY;
	x1a = x1;
	y1a = y1 - 1.0;

	if (Mode & 1)
	{
		NrObjects = 0;
		InstanceToObject(Instance, 0.0, 0.0, 0);
		minx = 10000.0;
		miny = 10000.0;
		maxx = -10000.0;
		maxy = -10000.0;

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			switch (Object->ObjectType)
			{
			case SYMBOL_LINE:
				minx = min(min(Object->x1, Object->x2), minx);
				miny = min(min(Object->y1, Object->y2), miny);
				maxx = max(max(Object->x1, Object->x2), maxx);
				maxy = max(max(Object->y1, Object->y2), maxy);
				break;

			case SYMBOL_RECT:
				minx = min((Object->x1 - Object->x2) * 0.5, minx);
				miny = min((Object->y1 - Object->y2) * 0.5, miny);
				maxx = max((Object->x1 + Object->x2) * 0.5, maxx);
				maxy = max((Object->y1 + Object->y2) * 0.5, maxy);
				break;

			case SYMBOL_ARC:
				break;
			}

		}

		x2 = (minx + maxx) * 0.5;
		y2 = (miny + maxy) * 0.5;
	}

	switch (Rotation)
	{
	case 0:
		if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
		{
			if (ObjectMirrorX == 1)
			{
				x1 = -x1;
				x1a = -x1a;

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
				y1a = -y1a;
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
		hulp = x1a;
		x1a = -y1a;
		y1a = hulp;

		if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
		{
			if (ObjectMirrorX == 1)
			{
				x1 = -x1;
				x1a = -x1a;

				if (TextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (TextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				y1 = -y1;
				y1a = -y1a;

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

	x1 += Instance->OriginX + OffsetX;
	y1 += Instance->OriginY + OffsetY;
	strcpy(str, Instance->Value);
	/*
	  if ((!EditingSymbol)
	     &&
	     (Instance->PlacingOption!=-1)) strcat(str,"(*)");
	*/
	GetMinMaxText(x1, y1, 1.0, 0, TextRotation, Alignment, str);

	if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
	{
		InitDrawingInstanceValueText(Mult(STANDARD_LINE_THICKNESS));
		DrawStr(x1, y1, 0.9, TextRotation, Alignment, str);

		if (Mode & 1)
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
	}

	if (Instance->PlacingOption != -1)
	{
		x1a += Instance->OriginX + OffsetX;
		y1a += Instance->OriginY + OffsetY;
		strcpy(str, "NP");
		GetMinMaxText(x1, y1, 1.0, 0, TextRotation, Alignment, str);

		if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
		{
			InitDrawingInstanceValueText(Mult(STANDARD_LINE_THICKNESS));
			DrawStr(x1a, y1a, 0.9, TextRotation, Alignment, str);
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawInstance(InstanceRecord * Instance, double OffsetX, double OffsetY, int32 Mode)
{
	int16 Info;
	int32 TempBackGroundActive;

	TempBackGroundActive = BackGroundActive;

	Info = Instance->Info;

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 4)) == (OBJECT_SELECTED | 4)))
	{
		if ((Mode == 0) && ((Info & (OBJECT_SELECTED | 4)) == (OBJECT_SELECTED | 4)))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		NrObjects = 0;
		InstanceToObject(Instance, OffsetX, OffsetY, 0);
		DrawObjects(0.0, 0.0, 0);

		if ((Mode == 0) && ((Info & (OBJECT_SELECTED | 4)) == (OBJECT_SELECTED | 4)))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)))
	{
		if ((Mode == 0) && ((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		if ((Mode == 2) && ((Info & (OBJECT_SELECTED | 5)) == (OBJECT_SELECTED | 1)))
			DrawReferenceInstance(Instance, OffsetX, OffsetY, 1);
		else
			DrawReferenceInstance(Instance, OffsetX, OffsetY, 0);

		if ((Mode == 0) && ((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2)))
	{
		if ((Mode == 0) && ((Info & (OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2)))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		if ((Mode == 2) && ((Info & (OBJECT_SELECTED | 6)) == (OBJECT_SELECTED | 2)))
			DrawValueInstance(Instance, OffsetX, OffsetY, 1);
		else
			DrawValueInstance(Instance, OffsetX, OffsetY, 0);

		if ((Mode == 0) && ((Info & (OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2)))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawInstances(int32 Mode)
{
	int16 cnt;
	InstanceRecord *Instance;

	for (cnt = 0; cnt < min(MaxNrInstances, Design.NrInstances); cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawInstance(Instance, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawNewGrid()
{
	StartDrawingEditingWindow();
	DrawGrid();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawGrid()
//  round
{
	double Grid2;
	uint8 GridBits[1024];
	HBITMAP GridBitmap, old;
	HDC PCBMemoryDC;
	COLORREF GColor;
	int32 Xmin2, Xmax2, Ymin2, Ymax2, Xpixels, Ypixels, cntx, cnty, x, y;

	Grid2 = GridSize;

	Xmin2 = (int32) (PixelToRealOffX(-40) / GridSize) - 1;
	Ymin2 = (int32) (PixelToRealOffY(-40) / GridSize) - 1;
	Xmax2 = (int32) (PixelToRealOffX(DrawWindowMaxX + 40) / GridSize) + 1;
	Ymax2 = (int32) (PixelToRealOffY(DrawWindowMaxY + 40) / GridSize) + 1;

	Xpixels = max(Xmax2 - Xmin2, 1);
	Ypixels = max(Ymax2 - Ymin2, 1);

	if (DrawWindowMaxX / Xpixels > 3)
	{
		SetTextColor(OutputDisplay, GridColor);
		GColor = GetTextColor(OutputDisplay);
		SetBkColor(OutputDisplay, RGB(0, 0, 0));
		memset(&GridBits, 0xff, sizeof(GridBits));

		for (cntx = Xmin2; cntx < Xmax2; cntx++)
		{
			x = Mult(cntx * GridSize - Xoffset) + DrawWindowMinX;

			if ((x >= DrawWindowMinX) && (x < DrawWindowMaxX))
				GridBits[x >> 3] &= 255 - (128 >> (x & 7));
		}

		SetROP2(OutputDisplay, R2_XORPEN);
		GridBitmap = CreateBitmap(3072, 1, 1, 1, &GridBits);
		PCBMemoryDC = CreateCompatibleDC(OutputDisplay);
		old = SelectObject(PCBMemoryDC, GridBitmap);

		for (cnty = Ymin2; cnty < Ymax2; cnty++)
		{
			y = Mult(cnty * GridSize - Yoffset);
			y = DrawWindowMaxY - y - 1;

			if ((y >= DrawWindowMinY) && (y < DrawWindowMaxY))
				BitBlt(OutputDisplay, 0, y, 3072, 1, PCBMemoryDC, 0, 0, SRCINVERT);
		}

		SelectObject(PCBMemoryDC, old);
		DeleteDC(PCBMemoryDC);
		DeleteObject(GridBitmap);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InitSpecialDraw(MEASUREITEMSTRUCT * MeasureItem)
{
	/*
	MEASUREITEMSTRUCT

	    UINT  CtlType;      // type of control
	    UINT  CtlID;        // combo box, list box, or button identifier
	    UINT  itemID;       // menu item, variable-height list box, or combo box identifier
	    UINT  itemWidth;    // width of menu item, in pixels
	    UINT  itemHeight;   // height of single item in list box menu, in pixels
	    DWORD itemData;     // application-defined 32-bit value
	*/
	switch (MeasureItem->CtlType)
	{
	case ODT_MENU:
		MeasureItem->itemWidth = 250;
		MeasureItem->itemHeight = 30;
		break;

	case ODT_LISTBOX:

//    case ODT_LISTVIEW:
		switch (MeasureItem->CtlID)
		{
		case IDC_LIST1:
			MeasureItem->itemHeight = 28;
			break;
		}

		break;
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawSpecialItem(DRAWITEMSTRUCT * DrawItem)
{
	/*
	DRAWITEMSTRUCT

	    UINT  CtlType;
	    UINT  CtlID;
	    UINT  itemID;
	    UINT  itemAction;
	    UINT  itemState;
	    HWND  hwndItem;
	    HDC   hDC;
	    RECT  rcItem;
	    DWORD itemData;
	*/

	HGDIOBJ SavePen, SaveBrush, MenuBrush, SaveFont;
	LOGBRUSH BrushObject;
	HPEN MenuPen, WhitePen, Pen2;
	int32 ItemNr, ok;
	int32 Inverted = 0;
	int32 Redraw;
	char TextStr[100];

// OnDrawItem

	switch (DrawItem->CtlType)
	{
// **********************************************************************************
// Drawing user definied menus
// **********************************************************************************
	case ODT_MENU:
		break;

	case ODT_LISTBOX:
		switch (DrawItem->CtlID)
		{
		case IDC_LIST1:
			ItemNr = DrawItem->itemData;
//          ItemNr=DrawItem->itemID;
			Redraw = 0;

			switch (DrawItem->itemAction)
			{
			case ODA_DRAWENTIRE:
				if (SelectedColorNr == -1)
					Inverted = 0;
				else
				{
					if (SelectedColorNr == ItemNr)
						Inverted = 1;
				}

				Redraw = 1;
				break;

			case ODA_SELECT:
				if ((DrawItem->itemState & ODS_SELECTED) == 0)
					Inverted = 0;
				else
				{
					SelectedColorNr = ItemNr;
					Inverted = 1;
				}

				Redraw = 1;
				break;

			case ODA_FOCUS:
				if (SelectedColorNr != -1)
				{
					Redraw = 1;
					Inverted = 1;
				}
				else
				{
					if ((DrawItem->itemState & ODS_SELECTED) == ODS_SELECTED)
					{
						Inverted = 1;
						Redraw = 1;
					}
				}

				ok = 1;
				break;
			}

			if (Redraw)
			{
				SaveFont = SelectObject(DrawItem->hDC, GetStockObject(DEFAULT_GUI_FONT));
//            SaveFont=SelectObject(DrawItem->hDC,GetStockObject(SYSTEM_FONT));
				SetBkColor(DrawItem->hDC, RGB(0, 0, 0));
				SetTextColor(DrawItem->hDC, RGB(255, 255, 255));
				WhitePen = CreatePen(PS_SOLID, 1, RGB(255, 255, 255));
				Pen2 = CreatePen(PS_SOLID, 2, RGB(192, 192, 192));
				SetROP2(DrawItem->hDC, R2_COPYPEN);
				BrushObject.lbColor = SCHColors[ItemNr];
				BrushObject.lbStyle = BS_SOLID;
				BrushObject.lbHatch = (LONG) NULL;

				MenuBrush = CreateBrushIndirect(&BrushObject);
				MenuPen = CreatePen(PS_SOLID, 1, SCHColors[ItemNr]);
				SaveBrush = SelectObject(DrawItem->hDC, MenuBrush);
				SavePen = SelectObject(DrawItem->hDC, MenuPen);
				Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top, DrawItem->rcItem.left + 180,
				          DrawItem->rcItem.bottom + 1);
				SelectObject(DrawItem->hDC, GetStockObject(WHITE_BRUSH));
				SelectObject(DrawItem->hDC, WhitePen);
				Rectangle(DrawItem->hDC, DrawItem->rcItem.left + 180, DrawItem->rcItem.top, DrawItem->rcItem.right,
				          DrawItem->rcItem.bottom + 1);

				if (Inverted)
				{
					SelectObject(DrawItem->hDC, GetStockObject(NULL_BRUSH));
					SetROP2(DrawItem->hDC, R2_XORPEN);
					SelectObject(DrawItem->hDC, Pen2);
					Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top + 1, DrawItem->rcItem.right,
					          DrawItem->rcItem.bottom + 1);
				}

//            SaveFont=SelectObject(DrawItem->hDC,MenuFont);
				SetBkMode(DrawItem->hDC, TRANSPARENT);
//            SetBkColor(DrawItem->hDC,RGB(0,0,0));
				SetTextColor(DrawItem->hDC, RGB(0, 0, 0));

				switch (ItemNr)
				{
				case BackGroundColorNr:
					strcpy(TextStr, SC(205, "Background"));
					break;

				case WireColorNr:
					strcpy(TextStr, SC(206, "Wire"));
					break;

				case BusColorNr:
					strcpy(TextStr, SC(207, "Bus"));
					break;

				case BusConnectionColorNr:
					strcpy(TextStr, SC(208, "Bus connection"));
					break;

				case GlobalConnectionColorNr:
					strcpy(TextStr, SC(209, "Global connection"));
					break;

				case JunctionColorNr:
					strcpy(TextStr, SC(210, "Junction"));
					break;

				case NetLabelColorNr:
					strcpy(TextStr, SC(108, "Net labels"));
					break;

				case InstanceRefTextColorNr:
					strcpy(TextStr, SC(351, "Component reference"));
					break;

				case InstanceValueTextColorNr:
					strcpy(TextStr, SC(213, "Component value"));
					break;

				case SymbolPinColorNr:
					strcpy(TextStr, SC(214, "Symbol pin"));
					break;

				case SymbolPinBusColorNr:
					strcpy(TextStr, SC(215, "Symbol pinbus"));
					break;

				case SymbolPinTextColorNr:
					strcpy(TextStr, SC(216, "Symbol pintext"));
					break;

				case SymbolPowerPinTextColorNr:
					strcpy(TextStr, SC(217, "Symbol powerpin text"));
					break;

				case SymbolPinBusTextColorNr:
					strcpy(TextStr, SC(218, "Symbol pinbus text"));
					break;

				case SymbolLineColorNr:
					strcpy(TextStr, SC(219, "Symbol line"));
					break;

				case SymbolRectColorNr:
					strcpy(TextStr, SC(220, "Symbol rectangle"));
					break;

				case SymbolCircleColorNr:
					strcpy(TextStr, SC(221, "Symbol circle"));
					break;

				case SymbolArcColorNr:
					strcpy(TextStr, SC(222, "Symbol arc"));
					break;

				case SymbolTextColorNr:
					strcpy(TextStr, SC(223, "Symbol text"));
					break;

				case ObjectLineColorNr:
					strcpy(TextStr, SC(224, "Line"));
					break;

				case ObjectRectColorNr:
					strcpy(TextStr, SC(225, "Rectangle"));
					break;

				case ObjectCircleColorNr:
					strcpy(TextStr, SC(226, "Circle"));
					break;

				case ObjectArcColorNr:
					strcpy(TextStr, SC(227, "Arc"));
					break;

				case ObjectTextColorNr:
					strcpy(TextStr, SC(228, "Text"));
					break;

				case GridColorNr:
					strcpy(TextStr, SC(229, "Grid"));
					break;

				case ButtonInfoColorNr:
					strcpy(TextStr, SC(230, "Button info"));
					break;
				}

				TextOutUTF8(DrawItem->hDC, 185, DrawItem->rcItem.top + 6, TextStr, strlen(TextStr));
				SelectObject(DrawItem->hDC, SavePen);
				SelectObject(DrawItem->hDC, SaveBrush);
				SelectObject(DrawItem->hDC, SaveFont);
				DeleteObject(WhitePen);
				DeleteObject(Pen2);
				DeleteObject(MenuPen);
				DeleteObject(MenuBrush);
			}

			break;
		}

		break;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
