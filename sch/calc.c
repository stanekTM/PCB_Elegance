/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc.c
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
#include "stdio.h"
#include "memory.h"
#include "calc.h"
#include "sch.h"
#include "calcdef.h"
#include "string.h"
#include "math.h"
#include "insdel.h"
#include "mainloop.h"
#include "time.h"
#include "utf8.h"

/*
#define ALIGN_LEFT_BOTTOM                       0            2 . 8
#define ALIGN_LEFT_CENTRE                       1            1 . 7
#define ALIGN_LEFT_TOP                          2            0 . 6
#define ALIGN_RIGHT_BOTTOM                      6
#define ALIGN_RIGHT_CENTRE                      7
#define ALIGN_RIGHT_TOP                         8
*/

double LineCrossX, LineCrossY;

extern int32 DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY;

int32 ok;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void FillPositionObject(ObjectRecord * Object)
{
	double xx1, yy1, xx2, yy2, xx3, yy3, xx2a, yy2a, Xmin, Ymin, Xmax, Ymax, Rotation, MinX2, MinY2, MaxX2, MaxY2;
	char str[MAX_LENGTH_STRING], TextStr[1024], TextStr2[MAX_LENGTH_STRING];
	int32 CircleMode, NrLines, cnt2, cnt3, lengte, AddMode, LinePos[16], TextAlignment, TextRotation, Length;
	char PinBusStrings[16][64];

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	yy2 = Object->y2;

	switch (Object->ObjectType)
	{
	case SYMBOL_RECT:
		xx2a = xx2 * 0.5;
		yy2a = yy2 * 0.5;
		Object->minx = xx1 - xx2a;
		Object->miny = yy1 - yy2a;
		Object->maxx = xx1 + xx2a;
		Object->maxy = yy1 + yy2a;
		break;

	case SYMBOL_LINE:
		Object->minx = min(xx1, xx2);
		Object->miny = min(yy1, yy2);
		Object->maxx = max(xx1, xx2);
		Object->maxy = max(yy1, yy2);
		break;

	case SYMBOL_CIRCLE:
		xx2a = xx2 * 0.5;
		CircleMode = Object->Info2;
		Object->minx = xx1;
		Object->miny = yy1;
		Object->maxx = xx1;
		Object->maxy = yy1;

		if ((CircleMode & 0x0c) > 0)
			Object->minx = xx1 - xx2a;

		if ((CircleMode & 0x03) > 0)
			Object->maxx = xx1 + xx2a;

		if ((CircleMode & 0x09) > 0)
			Object->miny = yy1 - xx2a;

		if ((CircleMode & 0x06) > 0)
			Object->maxy = yy1 + xx2a;

		break;

	case SYMBOL_ARC:
		xx2a = xx2 * 0.5;
		Object->minx = xx1 - xx2a;
		Object->miny = yy1 - xx2a;
		Object->maxx = xx1 + xx2a;
		Object->maxy = yy1 + xx2a;
		break;

	case SYMBOL_TEXT:
	case SYMBOL_PIN_TEXT:
		if (ConvertTextString(Object->Text1, TextStr) == -1)
			strcpy(TextStr, Object->Text1);

		TextAlignment = (Object->Info2 & 0x0f);
		TextRotation = (Object->Info2 >> 4) & 3;
		Rotation = TextRotation * 90.0;
		Xmin = 1e9;
		Xmax = -1e9;
		Ymin = 1e9;
		Ymax = -1e9;
		Length = strlen(TextStr);

		if (InRange(xx2, 1.0))
			xx2 = 0.9;

		NrLines = 0;
		cnt3 = 0;
		cnt2 = cnt3;

		while (cnt3 < Length + 1)
		{
			if ((TextStr[cnt3] == '\r') || ((cnt3 == Length) && (TextStr[cnt3 - 1] != '\n')))
			{
				if (cnt3 - cnt2 > 0)
				{
					memset(TextStr2, 0, sizeof(TextStr2));
					strncpy((LPSTR) & TextStr2, (LPSTR) & TextStr[cnt2], min(127, cnt3 - cnt2));

					if ((Rotation == 0.0) || (InRangeSpecial(Rotation, 90.0, 0.01)))
					{
						if (Rotation == 0.0)
							GetMinMaxText(xx1, yy1, xx2, 0, 0, TextAlignment, TextStr2);
						else
							GetMinMaxText(xx1, yy1, xx2, 0, 1, TextAlignment, TextStr2);
					}
					else
						GetMinMaxText2(xx1, yy1, xx2, 0, Rotation, TextAlignment, 0, TextStr2);

					Xmin = min(Xmin, TextMinX);
					Ymin = min(Ymin, TextMinY);
					Xmax = max(Xmax, TextMaxX);
					Ymax = max(Ymax, TextMaxY);
				}

				xx1 += sin(ANGLE_CONVERT(Rotation)) * xx2;
				yy1 -= cos(ANGLE_CONVERT(Rotation)) * xx2;
				cnt3 += 1;
				cnt2 = cnt3 + 1;
			}

			cnt3++;
		}

		Object->minx = Xmin;
		Object->miny = Ymin;
		Object->maxx = Xmax;
		Object->maxy = Ymax;
		break;

	case SYMBOL_PIN:
	case SYMBOL_PINBUS:
		Object->minx = xx1 - 0.3;
		Object->miny = yy1 - 0.3;
		Object->maxx = xx1 + 0.3;
		Object->maxy = yy1 + 0.3;
		break;

	case SYMBOL_POWERPIN_TEXT:
		strcpy(str, Object->Text1);
		strcat(str, " : ");
		strcat(str, Object->Text2);
		GetMinMaxText(xx1, yy1, xx2, Object->Info3, Object->Info2 >> 4, Object->Info2 & 0x0f, str);
		Object->minx = TextMinX;
		Object->miny = TextMinY;
		Object->maxx = TextMaxX;
		Object->maxy = TextMaxY;
		break;

	case SYMBOL_PINBUS_TEXT:
		MinX2 = 100000000.0;
		MinY2 = 100000000.0;
		MaxX2 = -100000000.0;
		MaxY2 = -100000000.0;

		TextAlignment = (Object->Info2 & 0x0f);
		TextRotation = (Object->Info2 >> 4) & 1;
		AddMode = 0;

		if (TextRotation)
			AddMode += 4;

		if ((TextAlignment == 6) || (TextAlignment == 8))
			AddMode += 1;		// Mirror X

		if ((TextAlignment == 2) || (TextAlignment == 8))
			AddMode += 2;		// Mirror Y

		lengte = strlen(Object->Text1);
		cnt2 = 0;
		NrLines = 0;
		memset(LinePos, 0, sizeof(LinePos));
		memset(PinBusStrings, 0, sizeof(PinBusStrings));
		LinePos[0] = 0;

		while (cnt2 < lengte)
		{
			if (Object->Text1[cnt2] == '\\')
			{
				memmove(&PinBusStrings[NrLines], &Object->Text1[LinePos[NrLines]], cnt2 - LinePos[NrLines]);

				if (NrLines < 7)
					NrLines++;

				LinePos[NrLines] = cnt2 + 1;
			}

			cnt2++;
		}

		if (Object->Text1[lengte - 1] != '\\')
		{
			memmove(&PinBusStrings[NrLines], &Object->Text1[LinePos[NrLines]], cnt2 - LinePos[NrLines]);
			LinePos[NrLines] = cnt2;
			NrLines++;
		}

		xx3 = xx1;
		yy3 = yy1;

		switch (AddMode)
		{
		case 2:				// Rotation = 0 , MirrorY = 1 , MirrorX = 0
		case 3:				// Rotation = 0 , MirrorY = 1 , MirrorX = 1
			yy3 += (NrLines - 1);
			break;

		case 6:				// Rotation = 1 , MirrorY = 1 , MirrorX = 0
		case 7:				// Rotation = 1 , MirrorY = 1 , MirrorX = 1
			xx3 -= (NrLines - 1);
			break;
		}

		if (TextRotation == 0)
			yy1 += 1.0;
		else
			xx1 -= 1.0;

		for (cnt2 = 0; cnt2 < NrLines; cnt2++)
		{
			GetMinMaxText(xx3, yy3, xx2, 0, TextRotation, TextAlignment, PinBusStrings[cnt2]);
			MinX2 = min(MinX2, TextMinX);
			MaxX2 = max(MaxX2, TextMaxX);
			MinY2 = min(MinY2, TextMinY);
			MaxY2 = max(MaxY2, TextMaxY);

			switch (AddMode)
			{
			case 0:			// Rotation = 0 , MirrorY = 0 , MirrorX = 0
			case 1:			// Rotation = 0 , MirrorY = 0 , MirrorX = 1
			case 2:			// Rotation = 0 , MirrorY = 1 , MirrorX = 0
			case 3:			// Rotation = 0 , MirrorY = 1 , MirrorX = 1
				yy3 -= 1.0;
				break;

			case 4:			// Rotation = 1 , MirrorY = 0 , MirrorX = 0
			case 5:			// Rotation = 1 , MirrorY = 0 , MirrorX = 1
			case 6:			// Rotation = 1 , MirrorY = 1 , MirrorX = 0
			case 7:			// Rotation = 1 , MirrorY = 1 , MirrorX = 1
				xx3 += 1.0;
				break;
			}
		}

		Object->minx = MinX2;
		Object->miny = MinY2;
		Object->maxx = MaxX2;
		Object->maxy = MaxY2;
		break;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void FillPositionObjects()
{
	int32 cnt;
	ObjectRecord *Object;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		FillPositionObject(Object);
	}

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SetBoardPosInstances()
{
	int32 cnt, ok;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		if (cnt == 3)
			ok = 1;

		SetBoardPosInstance(cnt);
	}

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 SetBoardPosInstance(int32 InstanceNr)
{
	int32 cnt, cnt2, SymbolNr, MemPos;
	double xmin, ymin, xmax, ymax;
	InstanceRecord *Instance;
//  InstancePositionRecord *InstancePosUnit;
	ObjectRecord *Object;
	SymbolRecord *Symbol;
	SymbolsPosRecord *SymbolPos;
	int32 Changed = 0;

	Instance = (InstanceRecord *) & ((*Instances)[InstanceNr]);

	xmin = 100000000;
	ymin = 100000000;
	xmax = -100000000;
	ymax = -100000000;

	if (!EditingSymbol)
	{
		SymbolNr = -1;

		for (cnt = 0; cnt < Design.NrSymbols; cnt++)
		{
			SymbolPos = &((*SymbolsPos)[cnt]);

			if (stricmpUTF8(SymbolPos->SymbolName, Instance->SymbolName) == 0)
				SymbolNr = cnt;
		}

		//  SymbolNr=(int16)Instance->SymbolNr;
		if (SymbolNr == -1)
			return -1;

		MemPos = (*SymbolsPos)[SymbolNr].Pos;
		Symbol = (SymbolRecord *) & (SymbolsMem[MemPos]);

		NrObjects = 0;
		InstanceToObject(Instance, 0.0, 0.0, 0);
//    InstancePinsToObject(Instance,0.0,0.0,0);

		for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
		{
			Object = &((*Objects)[cnt2]);
			FillPositionObject(Object);
			xmin = min(xmin, Object->minx);
			ymin = min(ymin, Object->miny);
			xmax = max(xmax, Object->maxx);
			ymax = max(ymax, Object->maxy);
			Changed = 1;
#ifdef _DEBUG

			if (xmax > 1000.0)
				ok = 1;

#endif
		}
	}

	if (GetMinMaxInstanceReferenceText(Instance) == 0)
	{
		xmin = min(xmin, TextMinX);
		xmax = max(xmax, TextMaxX);
		ymin = min(ymin, TextMinY);
		ymax = max(ymax, TextMaxY);
		Changed = 1;
	}

	if (GetMinMaxInstanceValueText(Instance) == 0)
	{
		xmin = min(xmin, TextMinX);
		xmax = max(xmax, TextMaxX);
		ymin = min(ymin, TextMinY);
		ymax = max(ymax, TextMaxY);
		Changed = 1;
	}

	Instance->BoardPosMinX = (float) xmin;
	Instance->BoardPosMinY = (float) ymin;
	Instance->BoardPosMaxX = (float) xmax;
	Instance->BoardPosMaxY = (float) ymax;

	if (Changed)
		return 1;

	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InstanceToObject(InstanceRecord * Instance, double OffsetX, double OffsetY, int32 Mode)
{
	int32 MemPos, MemPos2, cnt, cnt2, PowerPinMem, InfoBlockMem, PinMem, NrPinBusses, NrPowerPins, NrPins,
	      NrPartsPerPackage, PinBusMem, SymbolNr, PinTextRotation, PinRot, Rotation, PackagePartNr, FontNr, LineMode,
	      Alignment, CircleMode, InstanceInfo, NrPowerPinOverRules, pos, pos2, LengthIdent, LengthValue, ObjectMirrorX,
	      ObjectMirrorY, Version30, Version10;
	double hulp, x1, y1, x2, y2, x3, y3, x4, y4, OX, OY, Thickness, OriginX, OriginY, FontHeight;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;

	OldObjectLineRecord *OldObjectLine;
	OldObjectRectRecord *OldObjectRect;
	OldObjectCircleRecord *OldObjectCircle;
	OldObjectArcRecord *OldObjectArc;
	OldObjectTextRecord *OldObjectText;


	ObjectRecord *Object;
	SymbolRecord *Symbol;
	SubPinDefsArray *SubPinDefs;
	PowerPinRecord *PowerPin;
	PinRecord *Pin;
	PinBusRecord *PinBus;
	LPSTR PinText, PowerPinText, PowerPinNetName, LabelText, InstanceAttrBuf, PowerPinOverRuleNetName[64],
	      PowerPinOverRuleNewNetName[64];
	SymbolsPosRecord *SymbolPos;

#ifdef _DEBUG

	if (stricmp(Instance->SymbolName, "u109") == 0)
		ok = 1;

#endif
	LineMode = 0;
//  Instance=(InstanceRecord *)&((*Instances)[InstanceNr]);
	InstanceInfo = Instance->Info;
	SymbolNr = -1;

	for (cnt = 0; cnt < Design.NrSymbols; cnt++)
	{
		SymbolPos = &((*SymbolsPos)[cnt]);

		if (stricmpUTF8(SymbolPos->SymbolName, Instance->SymbolName) == 0)
			SymbolNr = cnt;
	}

	if (SymbolNr == -1)
	{
		if (EditingSymbol)
			return;

		Object = &((*Objects)[NrObjects]);
		Object->ObjectType = SYMBOL_RECT;
		Object->x1 = Instance->OriginX + OffsetX;
		Object->y1 = Instance->OriginY + OffsetY;
		Object->x2 = 4;
		Object->y2 = 4;
		Object->Info = 0;
		Object->Info3 = 0;

		if (NrObjects == MaxNrObjects - 1)
		{
			if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
				NrObjects++;
		}
		else
			NrObjects++;

		Object = &((*Objects)[NrObjects]);
		Object->ObjectType = SYMBOL_TEXT;
		Object->x1 = Instance->OriginX + OffsetX - 3.0;
		Object->y1 = Instance->OriginY + OffsetY + 2.0;
		Object->x2 = 1.0;
		Object->Text1 = SC(0, "No symbol");
		Object->Info = 0;
		Object->Info2 = 0;
		Object->Info3 = 0;

		if (NrObjects == MaxNrObjects - 1)
		{
			if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
				NrObjects++;
		}
		else
			NrObjects++;

//    InfoBlockMem+=sizeof(ObjectRectRecord);
		return;
	}

	MemPos = (*SymbolsPos)[SymbolNr].Pos;

	if (MemPos == -1)
		return;

	MemPos2 = MemPos + sizeof(SymbolRecord);
	Symbol = (SymbolRecord *) & (SymbolsMem[MemPos]);
	Version30 = 0;

	if (strcmp(Symbol->SymbolIdent, SymbolCode3) == 0)
		Version30 = 1;

	Version10 = 0;

	if (strcmp(Symbol->SymbolIdent, SymbolCode1) == 0)
		Version10 = 1;

	NrPartsPerPackage = Symbol->NrPartsPerPackage;
	PackagePartNr = min(Instance->PackagePartNr, NrPartsPerPackage);
	SubPinDefs = (SubPinDefsArray *) & (SymbolsMem[MemPos2]);

	Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
	OriginX = Instance->OriginX + OffsetX;
	OriginY = Instance->OriginY + OffsetY;
	ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
	ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);

	NrPowerPins = Symbol->NrPowerPins;
	NrPins = Symbol->NrPins;
	NrPinBusses = Symbol->NrPinBusses;
	PinMem = MemPos2 + Symbol->NrSubPinDefs * sizeof(SubPinDefsType);
	PowerPinMem = PinMem + NrPins * sizeof(PinRecord);
	PinBusMem = PowerPinMem + NrPowerPins * sizeof(PowerPinRecord);
	InfoBlockMem = PinBusMem + NrPinBusses * sizeof(PinBusRecord);

	OX = Symbol->OriginX;
	OY = Symbol->OriginY;

	cnt = NrPins;

	while (cnt > 0)
	{
		Pin = (PinRecord *) & (SymbolsMem[PinMem]);
		Alignment = Pin->NameInfo & 0x0f;
//    FontHeight=Pin->FontHeight;
		FontNr = 0;
		PinRot = (Pin->NameInfo >> 8) & 1;
		PinTextRotation = (PinRot + Rotation) & 1;

		Alignment = Pin->NameInfo & 15;

		if ((Mode & 1) == 0)
		{
			x1 = Pin->NameX - OX;
			y1 = Pin->NameY - OY;
		}
		else
		{
			x1 = Pin->X - OX;
			y1 = Pin->Y - OY;
		}

		if ((fabs(x1) > 90000.0) || (fabs(y1) > 90000.0))
		{
			PinMem += sizeof(PinRecord);
			continue;
		}

		switch (Rotation)
		{
		case 0:
			if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
			{
				if (ObjectMirrorX == 1)
				{
					x1 = -x1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				if (ObjectMirrorY == 1)
				{
					y1 = -y1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorX[Alignment];
				}
			}

			break;

		case 1:				//  90
			hulp = x1;
			x1 = -y1;
			y1 = hulp;

			if ((ObjectMirrorX == 0) && (ObjectMirrorY == 0))
			{
				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

//          if (PinTextRotation==1) Alignment=TextMirrorY[Alignment];
			}

			if (ObjectMirrorX == 1)
			{
				x1 = -x1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				y1 = -y1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorX[Alignment];
			}

			break;
		}

		PinText = (Pin->Name);
		LabelText = (Pin->Label);

		if (NrPartsPerPackage > 1)
			PinText = ((*SubPinDefs)[(NrPins - cnt) * NrPartsPerPackage + PackagePartNr - 1]);

		if (((Pin->NameInfo & TEXT_NOT_VISIBLE) == 0) || ((Mode & 1) == 1))
		{
			Object = &((*Objects)[NrObjects]);

			if ((Mode & 1) == 0)
				Object->ObjectType = SYMBOL_PIN_TEXT;
			else
				Object->ObjectType = SYMBOL_PIN;

			Object->x1 = x1 + OriginX;
			Object->y1 = y1 + OriginY;
			Object->x2 = 1.0;
			Object->Info2 = (int16) (Alignment + (PinTextRotation << 4));

//      Object->Info3=Pin->ConnectionType;
			if ((Symbol->Info & SHEET_SYMBOL) == 0)
				Object->Text1 = PinText;
			else
				Object->Text1 = LabelText;

			Object->PinNr = (int16) (NrPins - cnt);
			Object->Info = 0;

			if (PinTextRotation < 2)
			{
				if (NrObjects == MaxNrObjects - 1)
				{
					if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
						NrObjects++;
				}
				else
					NrObjects++;
			}
		}

		cnt--;
		PinMem += sizeof(PinRecord);
	}

// ************************************************************************
// Draw powerpin text


	NrPowerPinOverRules = 0;
	InstanceAttrBuf = (LPSTR) Instance->Properties;
	pos = 0;

	while ((pos < sizeof(Instance->Properties) - 2) && (NrPowerPinOverRules < 40) && (InstanceAttrBuf[pos] != 0))
	{
		LengthIdent = strlen((LPSTR) & InstanceAttrBuf[pos]);

		if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(Instance->Properties) - 1))
		{
			pos2 = pos + LengthIdent + 1;
			LengthValue = strlen((LPSTR) & InstanceAttrBuf[pos2]);

			if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(Instance->Properties) - 1))
			{
				if (InstanceAttrBuf[pos] == '~')
				{
					PowerPinOverRuleNetName[NrPowerPinOverRules] = (LPSTR) & InstanceAttrBuf[pos + 1];
					PowerPinOverRuleNewNetName[NrPowerPinOverRules] = (LPSTR) & InstanceAttrBuf[pos2];
					NrPowerPinOverRules++;
				}
			}

			pos += LengthIdent + LengthValue + 2;
		}
		else
			pos = sizeof(Instance->Properties);
	}

#ifdef _DEBUG

	if (stricmp(Instance->SymbolName, "pc-at") == 0)
		ok = 1;

#endif

	if ((Mode & 1) == 0)
	{
		cnt = NrPowerPins;

		while (cnt > 0)
		{
			PowerPin = (PowerPinRecord *) & (SymbolsMem[PowerPinMem]);
			Alignment = PowerPin->NameInfo & 0x0f;
			FontHeight = 1.0;
			FontNr = 0;
			PinTextRotation = (((PowerPin->NameInfo >> 8) & 1) + Rotation) & 1;

			Alignment = PowerPin->NameInfo & 15;
			x1 = PowerPin->NameX - OX;
			y1 = PowerPin->NameY - OY;

			if ((fabs(x1) > 90000.0) || (fabs(y1) > 90000.0))
			{
				PowerPinMem += sizeof(PowerPinRecord);
				continue;
			}

			switch (Rotation)
			{
			case 0:
				if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
				{
					if (ObjectMirrorX == 1)
					{
						x1 = -x1;

						if (PinTextRotation == 0)
							Alignment = TextMirrorX[Alignment];

						if (PinTextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					if (ObjectMirrorY == 1)
					{
						y1 = -y1;

						if (PinTextRotation == 0)
							Alignment = TextMirrorY[Alignment];

						if (PinTextRotation == 1)
							Alignment = TextMirrorX[Alignment];
					}
				}

				break;

			case 1:			//  90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;

				if ((ObjectMirrorX == 0) && (ObjectMirrorY == 0))
				{
					if (PinTextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					//          if (PinTextRotation==1) Alignment=TextMirrorY[Alignment];
				}

				if (ObjectMirrorX == 1)
				{
					x1 = -x1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				if (ObjectMirrorY == 1)
				{
					y1 = -y1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorX[Alignment];
				}

				break;
			}

			PowerPinText = (PowerPin->Text);
			PowerPinNetName = (PowerPin->NetName);


			for (cnt2 = 0; cnt2 < NrPowerPinOverRules; cnt2++)
			{
				if (stricmp(PowerPinOverRuleNetName[cnt2], PowerPin->NetName) == 0)
				{
					PowerPinNetName = PowerPinOverRuleNewNetName[cnt2];
					break;
				}
			}


			if ((PowerPin->NameInfo & TEXT_NOT_VISIBLE) == 0)
			{
				Object = &((*Objects)[NrObjects]);
				Object->ObjectType = SYMBOL_POWERPIN_TEXT;
				//      Object->InstanceNr=InstanceNr;
				Object->x1 = x1 + OriginX;
				Object->y1 = y1 + OriginY;
				Object->x2 = 1.0;
				Object->Info2 = (int16) (Alignment + (PinTextRotation << 4));
				Object->Info3 = 0;
				Object->Text1 = (LPSTR) PowerPinNetName;
				Object->Text2 = (LPSTR) PowerPinText;
				Object->PinNr = (int16) (NrPins - cnt);
				//      Object->Instance=Instance;
				Object->Info = 0;

				if (PinTextRotation < 2)
				{
					if (NrObjects == MaxNrObjects - 1)
					{
						if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
							NrObjects++;
					}
					else
						NrObjects++;
				}
			}

			cnt--;
			PowerPinMem += sizeof(PowerPinRecord);
		}
	}

// ************************************************************************
// Draw pin bus text

	cnt = NrPinBusses;

//  if (Mode==1) cnt=0;
	while (cnt > 0)
	{
		PinBus = (PinBusRecord *) & (SymbolsMem[PinBusMem]);
		Alignment = PinBus->NameInfo & 0x0f;

//    FontHeight=Pin->FontHeight;
		if (((PinBus->NameInfo & TEXT_NOT_VISIBLE) == 0) || ((Mode & 1) == 1))
		{
			FontNr = 0;
			PinRot = (PinBus->NameInfo >> 8) & 1;
			PinTextRotation = (PinRot + Rotation) & 1;
			Alignment = PinBus->NameInfo & 15;
			x1 = PinBus->NameX - OX;
			y1 = PinBus->NameY - OY;
			x2 = PinBus->X - OX;
			y2 = PinBus->Y - OY;

			if ((Mode & 1) == 1)
			{
				x1 = x2;
				y1 = y2;
			}

			if ((fabs(x1) > 90000.0) || (fabs(y1) > 90000.0))
			{
				PinBusMem += sizeof(PinBusRecord);
				continue;
			}

			switch (Rotation)
			{
			case 0:
				if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
				{
					if (ObjectMirrorX == 1)
					{
						x1 = -x1;

						if (PinTextRotation == 0)
							Alignment = TextMirrorX[Alignment];

						if (PinTextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					if (ObjectMirrorY == 1)
					{
						y1 = -y1;

						if (PinTextRotation == 0)
							Alignment = TextMirrorY[Alignment];

						if (PinTextRotation == 1)
							Alignment = TextMirrorX[Alignment];
					}
				}

				break;

			case 1:			//  90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;

				if ((ObjectMirrorX == 0) && (ObjectMirrorY == 0))
				{
					if (PinTextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					//          if (PinTextRotation==1) Alignment=TextMirrorY[Alignment];
				}

				if (ObjectMirrorX == 1)
				{
					x1 = -x1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				if (ObjectMirrorY == 1)
				{
					y1 = -y1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorX[Alignment];
				}

				break;
			}

			LabelText = (PinBus->Label);
#ifdef _DEBUG

			if (stricmp(LabelText, "a[0:15]") == 0)
				ok = 1;

#endif
			Object = &((*Objects)[NrObjects]);

			if ((Mode & 1) == 0)
				Object->ObjectType = SYMBOL_PINBUS_TEXT;
			else
				Object->ObjectType = SYMBOL_PINBUS;

			Object->x1 = x1 + OriginX;
			Object->y1 = y1 + OriginY;
			Object->x2 = 1.0;
			Object->Info2 = (int16) (Alignment + (PinTextRotation << 4));
			Object->Info3 = PinBus->NrPins;
			Object->Text1 = PinBus->Text;
			Object->Text2 = LabelText;
			Object->PinNr = (int16) (NrPins - cnt);
			Object->Instance = Instance;
			Object->Info = 0;

			if (PinTextRotation < 2)
			{
				if (NrObjects == MaxNrObjects - 1)
				{
					if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
						NrObjects++;
				}
				else
					NrObjects++;
			}
		}

		cnt--;
		PinBusMem += sizeof(PinBusRecord);
	}

	if ((Mode & 1) == 1)
		return;

// ************************************************************************
// Draw symbol info line

	for (cnt = 0; cnt < Symbol->NrObjectLines; cnt++)
	{
		if (Version30)
		{
			ObjectLine = (ObjectLineRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = ObjectLine->X1 - OX;
			y1 = ObjectLine->Y1 - OY;
			x2 = ObjectLine->X2 - OX;
			y2 = ObjectLine->Y2 - OY;
			Thickness = ObjectLine->Thickness;
			LineMode = ObjectLine->LineMode;
		}
		else
		{
			OldObjectLine = (OldObjectLineRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = OldObjectLine->X1 - OX;
			y1 = OldObjectLine->Y1 - OY;
			x2 = OldObjectLine->X2 - OX;
			y2 = OldObjectLine->Y2 - OY;
			LineMode = OldObjectLine->LineMode;

			if (Version10)
				LineMode = 0;

			Thickness = STANDARD_LINE_THICKNESS;
		}

		if ((fabs(x1) > 90000.0) || (fabs(x2) > 90000.0) || (fabs(y1) > 90000.0) || (fabs(y2) > 90000.0))
		{
			if (Version30)
				InfoBlockMem += sizeof(ObjectLineRecord);
			else
				InfoBlockMem += sizeof(OldObjectLineRecord);

			continue;
		}

		switch (Rotation)
		{
		case 0:
			break;

		case 1:
			hulp = x1;
			x1 = -y1;
			y1 = hulp;
			hulp = x2;
			x2 = -y2;
			y2 = hulp;
			break;

		case 2:
			x1 = -x1;
			y1 = -y1;
			x2 = -x2;
			y2 = -y2;
			break;

		case 3:
			hulp = x1;
			x1 = y1;
			y1 = -hulp;
			hulp = x2;
			x2 = y2;
			y2 = -hulp;
			break;
		}

		if (ObjectMirrorX == 1)
		{
			x1 = -x1;
			x2 = -x2;
		}

		if (ObjectMirrorY == 1)
		{
			y1 = -y1;
			y2 = -y2;
		}

		Object = &((*Objects)[NrObjects]);
		Object->ObjectType = SYMBOL_LINE;
		Object->x1 = x1 + OriginX;
		Object->y1 = y1 + OriginY;
		Object->x2 = x2 + OriginX;
		Object->y2 = y2 + OriginY;
		Object->Thickness = Thickness;
		Object->Info = 0;
		Object->Info3 = (int16) LineMode;

		if (NrObjects == MaxNrObjects - 1)
		{
			if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
				NrObjects++;
		}
		else
			NrObjects++;

		if (Version30)
			InfoBlockMem += sizeof(ObjectLineRecord);
		else
			InfoBlockMem += sizeof(OldObjectLineRecord);
	}

// ************************************************************************
// Draw symbol info rect

	for (cnt = 0; cnt < Symbol->NrObjectRects; cnt++)
	{
		if (Version30)
		{
			ObjectRect = (ObjectRectRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = ObjectRect->CentreX - OX;
			y1 = ObjectRect->CentreY - OY;
			x2 = ObjectRect->Width;
			y2 = ObjectRect->Height;
			Thickness = ObjectRect->Thickness;
			LineMode = ObjectRect->LineMode;
		}
		else
		{
			OldObjectRect = (OldObjectRectRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = OldObjectRect->CentreX - OX;
			y1 = OldObjectRect->CentreY - OY;
			x2 = OldObjectRect->Width;
			y2 = OldObjectRect->Height;
			Thickness = STANDARD_LINE_THICKNESS;
			LineMode = OldObjectRect->LineMode;
		}

		if ((fabs(x1) > 90000.0) || (fabs(y1) > 90000.0))
		{
			if (Version30)
				InfoBlockMem += sizeof(ObjectRectRecord);
			else
				InfoBlockMem += sizeof(OldObjectRectRecord);

			continue;
		}

		switch (Rotation)
		{
		case 0:
			break;

		case 1:
			hulp = x1;
			x1 = -y1;
			y1 = hulp;
			hulp = x2;
			x2 = y2;
			y2 = hulp;
			break;

		case 2:
			x1 = -x1;
			y1 = -y1;
			break;

		case 3:
			hulp = x1;
			x1 = y1;
			y1 = -hulp;
			hulp = x2;
			x2 = y2;
			y2 = hulp;
			break;
		}

		if (ObjectMirrorX == 1)
			x1 = -x1;

		if (ObjectMirrorY == 1)
			y1 = -y1;

		Object = &((*Objects)[NrObjects]);
		Object->ObjectType = SYMBOL_RECT;
		Object->x1 = x1 + OriginX;
		Object->y1 = y1 + OriginY;
		Object->x2 = x2;
		Object->y2 = y2;
		Object->Info = 0;
		Object->Info3 = (int16) LineMode;
		Object->Thickness = Thickness;

		if (NrObjects == MaxNrObjects - 1)
		{
			if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
				NrObjects++;
		}
		else
			NrObjects++;

		if (Version30)
			InfoBlockMem += sizeof(ObjectRectRecord);
		else
			InfoBlockMem += sizeof(OldObjectRectRecord);
	}

// ************************************************************************
// Draw symbol info circle

	for (cnt = 0; cnt < Symbol->NrObjectCircles; cnt++)
	{
		if (Version30)
		{
			ObjectCircle = (ObjectCircleRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = ObjectCircle->CentreX - OX;
			y1 = ObjectCircle->CentreY - OY;
			x2 = ObjectCircle->Diam;
			Thickness = ObjectCircle->Thickness;
			LineMode = ObjectCircle->LineMode;
			CircleMode = ObjectCircle->CircleMode;
		}
		else
		{
			OldObjectCircle = (OldObjectCircleRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = OldObjectCircle->CentreX - OX;
			y1 = OldObjectCircle->CentreY - OY;
			x2 = OldObjectCircle->Diam;
			Thickness = STANDARD_LINE_THICKNESS;
			LineMode = OldObjectCircle->LineMode;
			CircleMode = OldObjectCircle->CircleMode;
		}

		if ((fabs(x1) > 90000.0) || (fabs(y1) > 90000.0))
		{
			if (Version30)
				InfoBlockMem += sizeof(ObjectCircleRecord);
			else
				InfoBlockMem += sizeof(OldObjectCircleRecord);

			continue;
		}

		switch (Rotation)
		{
		case 0:
			break;

		case 1:
			hulp = x1;
			x1 = -y1;
			y1 = hulp;
			CircleMode = CircleRotate90[CircleMode];
			break;

		case 2:
			x1 = -x1;
			y1 = -y1;
			CircleMode = CircleRotate180[CircleMode];
			break;

		case 3:
			hulp = x1;
			x1 = y1;
			y1 = -hulp;
			CircleMode = CircleRotate270[CircleMode];
			break;
		}

		if (ObjectMirrorX == 1)
		{
			x1 = -x1;
			CircleMode = CircleMirrorX[CircleMode];
		}

		if (ObjectMirrorY == 1)
		{
			y1 = -y1;
			CircleMode = CircleMirrorY[CircleMode];
		}

		Object = &((*Objects)[NrObjects]);
		Object->ObjectType = SYMBOL_CIRCLE;
		Object->x1 = x1 + OriginX;
		Object->y1 = y1 + OriginY;
		Object->x2 = x2;
		Object->Info2 = CircleConv[CircleMode];
		Object->Info = 0;
		Object->Info3 = (int16) LineMode;
		Object->Thickness = Thickness;

		if (NrObjects == MaxNrObjects - 1)
		{
			if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
				NrObjects++;
		}
		else
			NrObjects++;

		if (Version30)
			InfoBlockMem += sizeof(ObjectCircleRecord);
		else
			InfoBlockMem += sizeof(OldObjectCircleRecord);
	}

// ************************************************************************
// Draw symbol info arc

	for (cnt = 0; cnt < Symbol->NrObjectArcs; cnt++)
	{
		if (Version30)
		{
			ObjectArc = (ObjectArcRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = ObjectArc->CentreX - OX;
			y1 = ObjectArc->CentreY - OY;
			x4 = ObjectArc->Width;
			y4 = ObjectArc->Height;
			x2 = ObjectArc->StartDiffX;
			y2 = ObjectArc->StartDiffY;
			x3 = ObjectArc->EndDiffX;
			y3 = ObjectArc->EndDiffY;
			Thickness = ObjectArc->Thickness;
			LineMode = ObjectArc->LineMode;
		}
		else
		{
			OldObjectArc = (OldObjectArcRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = OldObjectArc->CentreX - OX;
			y1 = OldObjectArc->CentreY - OY;
			x4 = OldObjectArc->Width;
			y4 = OldObjectArc->Height;
			x2 = OldObjectArc->StartDiffX;
			y2 = OldObjectArc->StartDiffY;
			x3 = OldObjectArc->EndDiffX;
			y3 = OldObjectArc->EndDiffY;
			Thickness = STANDARD_LINE_THICKNESS;
			LineMode = OldObjectArc->LineMode;
		}

		if ((fabs(x1) > 90000.0) || (fabs(y1) > 90000.0))
		{
			if (Version30)
				InfoBlockMem += sizeof(ObjectArcRecord);
			else
				InfoBlockMem += sizeof(OldObjectArcRecord);

			continue;
		}

		switch (Rotation)
		{
		case 0:
			break;

		case 1:
			hulp = x1;
			x1 = -y1;
			y1 = hulp;
			hulp = x2;
			x2 = -y2;
			y2 = hulp;
			hulp = x3;
			x3 = -y3;
			y3 = hulp;
			hulp = x4;
			x4 = y4;
			y4 = hulp;
			break;

		case 2:
			x1 = -x1;
			y1 = -y1;
			x2 = -x2;
			y2 = -y2;
			x3 = -x3;
			y3 = -y3;
			break;

		case 3:
			hulp = x1;
			x1 = y1;
			y1 = -hulp;
			hulp = x2;
			x2 = y2;
			y2 = -hulp;
			hulp = x3;
			x3 = y3;
			y3 = -hulp;
			hulp = x4;
			x4 = y4;
			y4 = hulp;
			break;
		}

		if (ObjectMirrorX == 1)
		{
			x1 = -x1;
			x2 = -x2;
			x3 = -x3;

			hulp = x2;
			x2 = x3;
			x3 = hulp;
			hulp = y2;
			y2 = y3;
			y3 = hulp;
		}

		if (ObjectMirrorY == 1)
		{
			y1 = -y1;
			y2 = -y2;
			y3 = -y3;

			hulp = x2;
			x2 = x3;
			x3 = hulp;
			hulp = y2;
			y2 = y3;
			y3 = hulp;
		}

		Object = &((*Objects)[NrObjects]);
		Object->ObjectType = SYMBOL_ARC;
		Object->x1 = x1 + OriginX;
		Object->y1 = y1 + OriginY;
		Object->x2 = x4;
		Object->y2 = y4;
		Object->x3 = x2;
		Object->y3 = y2;
		Object->x4 = x3;
		Object->y4 = y3;
		Object->Info = 0;
		Object->Info3 = (int16) LineMode;
		Object->Thickness = Thickness;

		if (NrObjects == MaxNrObjects - 1)
		{
			if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
				NrObjects++;
		}
		else
			NrObjects++;

		if (Version30)
			InfoBlockMem += sizeof(ObjectArcRecord);
		else
			InfoBlockMem += sizeof(OldObjectArcRecord);
	}

// ************************************************************************
// Draw symbol info text

	OldObjectText = NULL;
	ObjectText = NULL;

	for (cnt = 0; cnt < Symbol->NrObjectTexts; cnt++)
	{
		if (Version30)
		{
			ObjectText = (ObjectTextRecord *) & (SymbolsMem[InfoBlockMem]);
			x1 = ObjectText->X - OX;
			y1 = ObjectText->Y - OY;
			x2 = ObjectText->FontHeight;
			Thickness = ObjectText->Thickness;

			if (Thickness == 0.0)
				Thickness = 0.1;

			Alignment = ObjectText->TextMode & 0x0f;
			PinTextRotation = (((int32) (ObjectText->Rotation / 90.0) & 1) + Rotation) & 1;
		}
		else
		{
			OldObjectText = (OldObjectTextRecord *) & (SymbolsMem[InfoBlockMem]);
#ifdef _DEBUG

			if (!stricmp(OldObjectText->Text, "t1in"))
				ok = 1;

#endif
			x1 = OldObjectText->X - OX;
			y1 = OldObjectText->Y - OY;
			x2 = OldObjectText->FontHeight;
			Thickness = STANDARD_LINE_THICKNESS;
			Alignment = OldObjectText->TextMode & 0x0f;
			PinTextRotation = (((OldObjectText->TextMode >> 8) & 1) + Rotation) & 1;
		}

		if ((fabs(x1) > 90000.0) || (fabs(y1) > 90000.0))
		{
			if (Version30)
				InfoBlockMem += sizeof(ObjectTextRecord);
			else
				InfoBlockMem += sizeof(OldObjectTextRecord);

			continue;
		}

		switch (Rotation)
		{
		case 0:
			if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
			{
				if (ObjectMirrorX == 1)
				{
					x1 = -x1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				if (ObjectMirrorY == 1)
				{
					y1 = -y1;

					if (PinTextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (PinTextRotation == 1)
						Alignment = TextMirrorX[Alignment];
				}
			}

			break;

		case 1:				//  90
			hulp = x1;
			x1 = -y1;
			y1 = hulp;

			if ((ObjectMirrorX == 0) && (ObjectMirrorY == 0))
			{
				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				//        if (PinTextRotation==1) Alignment=TextMirrorY[Alignment];
			}

			if (ObjectMirrorX == 1)
			{
				x1 = -x1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				y1 = -y1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorX[Alignment];
			}

			break;
		}

		Object = &((*Objects)[NrObjects]);
		Object->ObjectType = SYMBOL_TEXT;
		Object->x1 = x1 + OriginX;
		Object->y1 = y1 + OriginY;
		Object->x2 = x2;

		if (Version30)
			Object->Text1 = (ObjectText->Text);
		else
			Object->Text1 = (OldObjectText->Text);

		Object->Info = 0;
		Object->Info2 = (int16) (Alignment + (PinTextRotation << 4));
		Object->Info3 = 0;
		Object->Thickness = Thickness;

		if ((Mode == 0) && (PinTextRotation < 2))
		{
			if (NrObjects == MaxNrObjects - 1)
			{
				if (AllocateMemObjects(MaxNrObjects + 512, 1) == 0)
					NrObjects++;
			}
			else
				NrObjects++;
		}

		if (Version30)
			InfoBlockMem += sizeof(ObjectTextRecord);
		else
			InfoBlockMem += sizeof(OldObjectTextRecord);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void FindMinMaxDesign(double *MinX, double *MinY, double *MaxX, double *MaxY)
{
	int32 cnt, cnt2, cnt3, lengte, NrLines, AddMode, Length, LinePos[16], TextAlignment, TextRotation, Changed;
	char PinBusStrings[16][64], TextString[1024], TextStr[1024], str[MAX_LENGTH_STRING];
	double x1, y1, x2, y2, x4, y4, xx3, yy3, MinX2, MaxX2, MinY2, MaxY2, Rotation2;
	struct tm FileTime, *today;
	time_t ltime;

	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	BusConnectionRecord *BusConnection;
	NetLabelRecord *NetLabel;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	InstanceRecord *Instance;
	GlobalConnectionRecord *GlobalConnection;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;

	MinX2 = 10000000000.0;
	MinY2 = 10000000000.0;
	MaxX2 = -10000000000.0;
	MaxY2 = -10000000000.0;

	Changed = 0;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if (SetBoardPosInstance(cnt) == 1)
			{
				MinX2 = min(MinX2, Instance->BoardPosMinX);
				MaxX2 = max(MaxX2, Instance->BoardPosMaxX);
				MinY2 = min(MinY2, Instance->BoardPosMinY);
				MaxY2 = max(MaxY2, Instance->BoardPosMaxY);
				Changed = 1;
			}
		}
	}

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MinX2 = min(MinX2, min(Wire->X1, Wire->X2));
				MaxX2 = max(MaxX2, max(Wire->X1, Wire->X2));
				MinY2 = min(MinY2, min(Wire->Y1, Wire->Y2));
				MaxY2 = max(MaxY2, max(Wire->Y1, Wire->Y2));
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MinX2 = min(MinX2, min(Bus->X1, Bus->X2));
				MaxX2 = max(MaxX2, max(Bus->X1, Bus->X2));
				MinY2 = min(MinY2, min(Bus->Y1, Bus->Y2));
				MaxY2 = max(MaxY2, max(Bus->Y1, Bus->Y2));
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MinX2 = min(MinX2, Junction->X);
				MaxX2 = max(MaxX2, Junction->X);
				MinY2 = min(MinY2, Junction->Y);
				MaxY2 = max(MaxY2, Junction->Y);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MinX2 = min(MinX2, BusConnection->X - 2.0);
				MaxX2 = max(MaxX2, BusConnection->X + 2.0);
				MinY2 = min(MinY2, BusConnection->Y - 1.0);
				MaxY2 = max(MaxY2, BusConnection->Y + 1.0);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MinX2 = min(MinX2, GlobalConnection->X - 2.0);
				MaxX2 = max(MaxX2, GlobalConnection->X + 2.0);
				MinY2 = min(MinY2, GlobalConnection->Y - 1.0);
				MaxY2 = max(MaxY2, GlobalConnection->Y + 1.0);
				TextRotation = (GlobalConnection->NameInfo >> 8) & 1;
				GetMinMaxText(GlobalConnection->NameX, GlobalConnection->NameY, 1.0, 0, TextRotation,
				              GlobalConnection->NameInfo & 15, (LPSTR) GlobalConnection->Text);
				MinX2 = min(MinX2, TextMinX);
				MaxX2 = max(MaxX2, TextMaxX);
				MinY2 = min(MinY2, TextMinY);
				MaxY2 = max(MaxY2, TextMaxY);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				TextRotation = (NetLabel->Alignment >> 8) & 0x01;
				TextAlignment = (NetLabel->Alignment & 0x0f);

				GetMinMaxText(NetLabel->ConnectX + NetLabel->TextX, NetLabel->ConnectY + NetLabel->TextY, 1.0, 0,
				              TextRotation, TextAlignment, NetLabel->Name);
				MinX2 = min(MinX2, TextMinX);
				MaxX2 = max(MaxX2, TextMaxX);
				MinY2 = min(MinY2, TextMinY);
				MaxY2 = max(MaxY2, TextMaxY);
				Changed = 1;
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MinX2 = min(MinX2, Pin->X);
				MaxX2 = max(MaxX2, Pin->X);
				MinY2 = min(MinY2, Pin->Y);
				MaxY2 = max(MaxY2, Pin->Y);
				TextRotation = (Pin->NameInfo >> 8) & 1;
				TextAlignment = Pin->NameInfo & 0x0f;
				GetMinMaxText(Pin->NameX, Pin->NameY, 1.0, 0, TextRotation, TextAlignment, (LPSTR) Pin->Name);
				MinX2 = min(MinX2, TextMinX);
				MaxX2 = max(MaxX2, TextMaxX);
				MinY2 = min(MinY2, TextMinY);
				MaxY2 = max(MaxY2, TextMaxY);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				strcpy(str, PowerPin->NetName);
				strcat(str, " : ");
				strcat(str, PowerPin->Text);
				TextRotation = (PowerPin->NameInfo >> 8) & 1;
				TextAlignment = PowerPin->NameInfo & 0x0f;
				GetMinMaxText(PowerPin->NameX, PowerPin->NameY, 1.0, 0, TextRotation, TextAlignment, str);
				MinX2 = min(MinX2, TextMinX);
				MaxX2 = max(MaxX2, TextMaxX);
				MinY2 = min(MinY2, TextMinY);
				MaxY2 = max(MaxY2, TextMaxY);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MinX2 = min(MinX2, PinBus->X);
				MaxX2 = max(MaxX2, PinBus->X);
				MinY2 = min(MinY2, PinBus->Y);
				MaxY2 = max(MaxY2, PinBus->Y);
				TextRotation = (PinBus->NameInfo >> 8) & 1;
				TextAlignment = PinBus->NameInfo & 0x0f;

				AddMode = 0;

				if (TextRotation)
					AddMode += 4;

				if ((TextAlignment == 6) || (TextAlignment == 8))
					AddMode += 1;	// Mirror X

				if ((TextAlignment == 2) || (TextAlignment == 8))
					AddMode += 2;	// Mirror Y

				lengte = strlen(PinBus->Text);
				cnt2 = 0;
				NrLines = 0;
				memset(LinePos, 0, sizeof(LinePos));
				memset(PinBusStrings, 0, sizeof(PinBusStrings));
				LinePos[0] = 0;

				while (cnt2 < lengte)
				{
					if (PinBus->Text[cnt2] == '\\')
					{
						memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], cnt2 - LinePos[NrLines]);

						if (NrLines < 7)
							NrLines++;

						LinePos[NrLines] = cnt2 + 1;
					}

					cnt2++;
				}

				if (PinBus->Text[lengte - 1] != '\\')
				{
					memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], cnt2 - LinePos[NrLines]);
					LinePos[NrLines] = cnt2;
					NrLines++;
				}

				TextRotation = (PinBus->NameInfo >> 8) & 1;
				TextAlignment = PinBus->NameInfo & 0x0f;
				xx3 = PinBus->X;
				yy3 = PinBus->Y;

				switch (AddMode)
				{
				case 2:		// Rotation = 0 , MirrorY = 1 , MirrorX = 0
				case 3:		// Rotation = 0 , MirrorY = 1 , MirrorX = 1
					yy3 += (NrLines - 1);
					break;

				case 6:		// Rotation = 1 , MirrorY = 1 , MirrorX = 0
				case 7:		// Rotation = 1 , MirrorY = 1 , MirrorX = 1
					xx3 -= (NrLines - 1);
					break;
				}

				x1 = PinBus->NameX;
				y1 = PinBus->NameY;
				x2 = 1.0;

				for (cnt2 = 0; cnt2 < NrLines; cnt2++)
				{
					GetMinMaxText(xx3, yy3, x2, 0, TextRotation, TextAlignment, PinBusStrings[cnt]);
					MinX2 = min(MinX2, TextMinX);
					MinY2 = min(MinY2, TextMinY);
					MaxX2 = max(MaxX2, TextMaxX);
					MaxY2 = max(MaxY2, TextMaxY);

					switch (AddMode)
					{
					case 0:	// Rotation = 0 , MirrorY = 0 , MirrorX = 0
					case 1:	// Rotation = 0 , MirrorY = 0 , MirrorX = 1
					case 2:	// Rotation = 0 , MirrorY = 1 , MirrorX = 0
					case 3:	// Rotation = 0 , MirrorY = 1 , MirrorX = 1
						yy3 -= 1.0;
						break;

					case 4:	// Rotation = 1 , MirrorY = 0 , MirrorX = 0
					case 5:	// Rotation = 1 , MirrorY = 0 , MirrorX = 1
					case 6:	// Rotation = 1 , MirrorY = 1 , MirrorX = 0
					case 7:	// Rotation = 1 , MirrorY = 1 , MirrorX = 1
						xx3 += 1.0;
						break;
					}
				}

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MinX2 = min(MinX2, min(ObjectLine->X1, ObjectLine->X2));
			MaxX2 = max(MaxX2, max(ObjectLine->X1, ObjectLine->X2));
			MinY2 = min(MinY2, min(ObjectLine->Y1, ObjectLine->Y2));
			MaxY2 = max(MaxY2, max(ObjectLine->Y1, ObjectLine->Y2));
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x2 = ObjectRect->Width * 0.5;
			y2 = ObjectRect->Height * 0.5;
			MinX2 = min(MinX2, ObjectRect->CentreX - x2);
			MaxX2 = max(MaxX2, ObjectRect->CentreX + x2);
			MinY2 = min(MinY2, ObjectRect->CentreY - y2);
			MaxY2 = max(MaxY2, ObjectRect->CentreY + y2);
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x2 = ObjectCircle->Diam * 0.5;
			MinX2 = min(MinX2, ObjectCircle->CentreX - x2);
			MaxX2 = max(MaxX2, ObjectCircle->CentreX + x2);
			MinY2 = min(MinY2, ObjectCircle->CentreY - x2);
			MaxY2 = max(MaxY2, ObjectCircle->CentreY + x2);
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x2 = ObjectArc->Width * 0.5;
			MinX2 = min(MinX2, ObjectArc->CentreX - x2);
			MaxX2 = max(MaxX2, ObjectArc->CentreX + x2);
			MinY2 = min(MinY2, ObjectArc->CentreY - x2);
			MaxY2 = max(MaxY2, ObjectArc->CentreY + x2);
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x2 = ObjectText->FontHeight;
			TextAlignment = ObjectText->TextMode & 0x0f;
			strcpy(TextStr, ObjectText->Text);

			if (strlen(TextStr) > 0)
			{
				x4 = ObjectText->X;
				y4 = ObjectText->Y;
				x2 = ObjectText->FontHeight;

				if (stricmp(TextStr, "$DATE") == 0)
				{
					/*
					   tm_sec   Seconds after minute (0 – 59)
					   tm_min   Minutes after hour (0 – 59)
					   tm_hour   Hours after midnight (0 – 23)
					   tm_mday   Day of month (1 – 31)
					   tm_mon   Month (0 – 11; January = 0)
					   tm_year   Year (current year minus 1900)
					   tm_wday   Day of week (0 – 6; Sunday = 0)
					   tm_yday   Day of year (0 – 365; January 1 = 0)
					 */
					if (Design.DesignDate.Year != 0)
					{
						FileTime.tm_year = Design.DesignDate.Year;
						FileTime.tm_mon = Design.DesignDate.Month - 1;
						FileTime.tm_mday = Design.DesignDate.Day;
						FileTime.tm_hour = Design.DesignDate.Hour;
						FileTime.tm_min = Design.DesignDate.Minutes;
						FileTime.tm_sec = 0;
						strftime(TextStr, 100, "%B %d, %Y    %X", &FileTime);
						//          strftime(TextStr,100,"%c",&FileTime);
						//          strftime(TextStr,100,"%x  %X",&FileTime);
					}
					else
					{
						time(&ltime);
						today = localtime(&ltime);
						strftime(TextStr, 100, "%B %d, %Y    %X", today);
					}
				}

				Rotation2 = ObjectText->Rotation;
				cnt3 = 0;
				cnt2 = cnt3;
				Length = strlen(TextStr);

				while (cnt3 < Length + 1)
				{
					if ((TextStr[cnt3] == '\r') || ((cnt3 == Length) && (TextStr[cnt3 - 1] != '\n')))
					{
						if (cnt3 - cnt2 > 0)
						{
							memset(TextString, 0, sizeof(TextString));
							strncpy(TextString, (LPSTR) & TextStr[cnt2], min(127, cnt3 - cnt2));

							if ((Rotation2 == 0.0) || (InRangeSpecial(Rotation2, 90.0, 0.01)))
							{
								if (Rotation2 == 0.0)
									GetMinMaxText(x4, y4, x2, 0, 0, TextAlignment, TextString);
								else
									GetMinMaxText(x4, y4, x2, 0, 1, TextAlignment, TextString);
							}
							else
								GetMinMaxText2(x4, y4, x2, 0, Rotation2, TextAlignment, 0, TextString);

							MinX2 = min(MinX2, TextMinX);
							MaxX2 = max(MaxX2, TextMaxX);
							MinY2 = min(MinY2, TextMinY);
							MaxY2 = max(MaxY2, TextMaxY);
							Changed = 1;
						}

						x4 += sin(ANGLE_CONVERT(ObjectText->Rotation)) * x2;
						y4 -= cos(ANGLE_CONVERT(ObjectText->Rotation)) * x2;
						cnt3 += 1;
						cnt2 = cnt3 + 1;
					}

					cnt3++;
				}
			}
		}
	}

	if (!Changed)
	{
		if (!EditingSymbol)
		{
			MaxX2 = 100.0;
			MinX2 = 0.0;
			MaxY2 = 30.0;
			MinY2 = 0.0;
		}
		else
		{
			MaxX2 = 50.0;
			MinX2 = -50.0;
			MaxY2 = 20.0;
			MinY2 = -20.0;
		}
	}

	*MinX = MinX2;
	*MaxX = MaxX2;
	*MinY = MinY2;
	*MaxY = MaxY2;
	VisibleMinX = MinX2;
	VisibleMaxX = MaxX2;
	VisibleMinY = MinY2;
	VisibleMaxY = MaxY2;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ViewWholeDesign(int32 mode)
{
#define  OVERSIZE  1.1
	double hulpx, hulpy, minx, maxx, miny, maxy, divx, divy, cx, cy;

	if (!EditingSymbol)
		SetBoardPosInstances();

	FindMinMaxDesign(&minx, &miny, &maxx, &maxy);

	divx = maxx - minx;
	divy = maxy - miny;
	cx = (maxx + minx) * 0.5;
	cy = (maxy + miny) * 0.5;
	divx = divx * OVERSIZE;
	divy = divy * OVERSIZE;
	hulpx = divx / ClientWindowDivX;
	hulpy = divy / ClientWindowDivY;

	if (hulpx > hulpy)
	{
		hulpx = ClientWindowDivX;
		Factor = hulpx / divx;

		if ((EditingSymbol) && (Factor > 50.0))
		{
			Factor = 50.0;
			divx = hulpx / Factor;
		}

		DisplX = divx;
		DisplY = DisplX * ClientWindowDivY / ClientWindowDivX;
		Xoffset = (cx - DisplX * 0.5);
		Yoffset = (cy - DisplY * 0.5);
	}
	else
	{
		hulpy = ClientWindowDivY;
		Factor = hulpy / divy;

		if ((EditingSymbol) && (Factor > 50.0))
		{
			Factor = 50.0;
			divy = hulpy / Factor;
		}

		DisplY = divy;
		DisplX = DisplY * ClientWindowDivX / ClientWindowDivY;
		Xoffset = (cx - DisplX * 0.5);
		Yoffset = (cy - DisplY * 0.5);
	}

	CheckZoomFactor();

	if ((mode == 1) && (SCHWindow))
	{
		InvalidateRect(SCHWindow, NULL, 0);
		PostMessage(SCHWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
	}

//  SetScrollPageSize();
//  SetScrollPosition();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GetMinMaxText(double X, double Y, double FontSize, int32 FontNr, int32 Rotation, int32 Alignment, LPSTR Str)
{
	double TextSizeX, TextSizeY;

	TextSizeX = FontSize * 0.666666666 * (strlen(Str));
	TextSizeY = FontSize;

	switch (Rotation)
	{
	case 0:
		switch (Alignment)
		{
		case ALIGN_LEFT_BOTTOM:
			TextMinX = X;
			TextMaxX = X + TextSizeX;
			TextMinY = Y;
			TextMaxY = Y + FontSize;
			break;

		case ALIGN_RIGHT_BOTTOM:
			TextMinX = X - TextSizeX;
			TextMaxX = X;
			TextMinY = Y;
			TextMaxY = Y + FontSize;
			break;

		case ALIGN_LEFT_CENTRE:
			TextMinX = X;
			TextMaxX = X + TextSizeX;
			TextMinY = Y - 0.5 * FontSize;
			TextMaxY = Y + 0.7 * FontSize;
			break;

		case ALIGN_RIGHT_CENTRE:
			TextMinX = X - TextSizeX;
			TextMaxX = X;
			TextMinY = Y - 0.5 * FontSize;
			TextMaxY = Y + 0.5 * FontSize;
			break;

		case ALIGN_LEFT_TOP:
			TextMinX = X;
			TextMaxX = X + TextSizeX;
			TextMinY = Y - FontSize;
			TextMaxY = Y;
			break;

		case ALIGN_RIGHT_TOP:
			TextMinX = X - TextSizeX;
			TextMaxX = X;
			TextMinY = Y - FontSize;
			TextMaxY = Y;
			break;
		}

		break;

	case 1:
		switch (Alignment)
		{
		case ALIGN_LEFT_BOTTOM:
			TextMinX = X - FontSize;
			TextMaxX = X;
			TextMinY = Y;
			TextMaxY = Y + TextSizeX;
			break;

		case ALIGN_RIGHT_BOTTOM:
			TextMinX = X - FontSize;
			TextMaxX = X;
			TextMinY = Y - TextSizeX;
			TextMaxY = Y;
			break;

		case ALIGN_LEFT_CENTRE:
			TextMinX = X - 0.5 * FontSize;
			TextMaxX = X + 0.5 * FontSize;
			TextMinY = Y;
			TextMaxY = Y + TextSizeX;
			break;

		case ALIGN_RIGHT_CENTRE:
			TextMinX = X - 0.5 * FontSize;
			TextMaxX = X + 0.5 * FontSize;
			TextMinY = Y - TextSizeX;
			TextMaxY = Y;
			break;

		case ALIGN_LEFT_TOP:
			TextMinX = X;
			TextMaxX = X + FontSize;
			TextMinY = Y;
			TextMaxY = Y + TextSizeX;
			break;

		case ALIGN_RIGHT_TOP:
			TextMinX = X;
			TextMaxX = X + FontSize;
			TextMinY = Y - TextSizeX;
			TextMaxY = Y;
			break;
		}

		break;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GetMinMaxText2(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                    LPSTR Str)
{
	double TextSizeX, TextSizeY;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	TextSizeX = FontSize * 0.9 * DefFontSize * (strlen(Str));
	TextSizeY = FontSize * DefFontSize;

	x1 = 0.0;
	y1 = 0.0;
	x2 = 0.0;
	y2 = TextSizeY;
	x3 = TextSizeX;
	y3 = TextSizeY;
	x4 = TextSizeX;
	y4 = 0.0;
	RotatePoint2(&x1, &y1, Rotation);
	RotatePoint2(&x2, &y2, Rotation);
	RotatePoint2(&x3, &y3, Rotation);
	RotatePoint2(&x4, &y4, Rotation);

	if (Mirror == 1)
	{
		x1 = -x1;
		x2 = -x2;
		x3 = -x3;
		x4 = -x4;
	}

	TextMinX = (X + min(x1, min(x2, min(x3, x4))));
	TextMinY = (Y + min(y1, min(y2, min(y3, y4))));
	TextMaxX = (X + max(x1, max(x2, max(x3, x4))));
	TextMaxY = (Y + max(y1, max(y2, max(y3, y4))));
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 TestLineConnectedToCircle(double x1, double y1, double x2, double y2, double CircleX, double CircleY,
                                double CircleDiam)
{
	double a, E, F, A, B, C, D, x11, y11, x22, y22;

	if (InRange(x1, x2))
	{
		B = -2.0 * CircleY;
		C = (CircleY * CircleY) + (x1 - CircleX) * (x1 - CircleX) - 0.25 * (CircleDiam * CircleDiam);
		D = B * B - 4.0 * C;

		if (D >= 0)
		{
			y11 = (-B + sqrt(D)) / 2.0;
			y22 = (-B - sqrt(D)) / 2.0;

			if ((X1SmallerThenX2(y11, max(y1, y2))) && (X1GreaterThenX2(y11, min(y1, y2))))
				return 1;

			if ((X1SmallerThenX2(y22, max(y1, y2))) && (X1GreaterThenX2(y22, min(y1, y2))))
				return 1;
		}

		return 0;
	}

	if (InRange(y1, y2))
	{
		B = -2.0 * CircleX;
		C = (CircleX * CircleX) + (y1 - CircleY) * (y1 - CircleY) - 0.25 * (CircleDiam * CircleDiam);
		D = B * B - 4.0 * C;

		if (D >= 0)
		{
			x11 = (-B + sqrt(D)) / 2.0;
			x22 = (-B - sqrt(D)) / 2.0;

			if ((X1SmallerThenX2(x11, max(x1, x2))) && (X1GreaterThenX2(x11, min(x1, x2))))
				return 1;

			if ((X1SmallerThenX2(x22, max(x1, x2))) && (X1GreaterThenX2(x22, min(x1, x2))))
				return 1;
		}

		return 0;
	}


	a = (x2 - x1) / (y2 - y1);
	E = -y1 * a + x1;

	F = -(CircleX * CircleX) - (CircleY * CircleY) + 0.25 * (CircleDiam * CircleDiam);

	A = a * a + 1;
	B = 2 * a * E - 2 * a * CircleX - 2 * CircleY;
	C = E * E - 2 * E * CircleX - F;

	D = B * B - 4 * A * C;

	if (D >= 0)
	{
		y11 = (-B + sqrt(D)) / 2.0 / A;
		y22 = (-B - sqrt(D)) / 2.0 / A;

		if ((X1SmallerThenX2(y11, max(y1, y2))) && (X1GreaterThenX2(y11, min(y1, y2))))
			return 1;

		if ((X1SmallerThenX2(y22, max(y1, y2))) && (X1GreaterThenX2(y22, min(y1, y2))))
			return 1;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 LineCrosses(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                  double LineX4, double LineY4)
//  -1  Lines dont cross
//   0  Lines overlap
//   1  Lines cross
{
	double a, b, x, y, minx12, miny12, maxx12, maxy12, minx34, miny34, maxx34, maxy34;

//  LineCrossX=-1000000.0;
//  LineCrossY=-1000000.0;

	minx12 = min(LineX1, LineX2);
	maxx34 = max(LineX3, LineX4);

	if (minx12 > maxx34)
		return 0;

	miny12 = min(LineY1, LineY2);
	maxy34 = max(LineY3, LineY4);

	if (miny12 > maxy34)
		return 0;

	maxx12 = max(LineX1, LineX2);
	minx34 = min(LineX3, LineX4);

	if (maxx12 < minx34)
		return 0;

	miny34 = min(LineY3, LineY4);
	maxy12 = max(LineY1, LineY2);

	if (maxy12 < miny34)
		return 0;

	if (InRange(LineX2, LineX1))
	{
		if (InRange(LineY3, LineY4))
		{
			LineCrossX = LineX1;
			LineCrossY = LineY3;
			return 1;
		}
		else
		{
			if (InRange(LineX4, LineX3))
				return 0;

			b = (LineY4 - LineY3) / (LineX4 - LineX3);
			x = LineX1;
			y = (x - LineX3) * b + LineY3;

			if ((y > maxy12) || (y < miny12))
				return 0;

			if (fabs(b) > 1)
			{	// almost vertical line check y
				if ((y > maxy34) || (y < miny34))
					return 0;
			}
			else
			{	// almost vertical line check x
				if ((x > maxx34) || (x < minx34))
					return 0;
			}
		}
	}
	else
	{
		if (InRange(LineX4, LineX3))
		{
			if (InRange(LineY1, LineY2))
			{
				LineCrossX = LineX3;
				LineCrossY = LineY1;
				return 1;
			}
			else
			{
				a = (LineY2 - LineY1) / (LineX2 - LineX1);
				x = LineX3;
				y = (x - LineX1) * a + LineY1;

				if ((y > maxy34) || (y < miny34))
					return 0;

				if (fabs(a) > 1)
				{	// almost vertical line check y
					if ((y > maxy12) || (y < miny12))
						return 0;
				}
				else
				{	// almost vertical line check x
					if ((x > maxx12) || (x < minx12))
						return 0;
				}
			}
		}
		else
		{
			a = (LineY2 - LineY1) / (LineX2 - LineX1);
			x = a * LineX1;
			x -= LineY1;
			b = (LineY4 - LineY3) / (LineX4 - LineX3);

			if (a == b)
				return 0;

			x += LineY3;
			x += -b * LineX3;
			x = x / (a - b);

			y = (x - LineX1) * a + LineY1;

			if (fabs(a) > 1)
			{	// almost vertical line check y
				if ((y > maxy12) || (y < miny12))
					return 0;

				if (fabs(b) > 1)
				{	// almost vertical line check y
					if ((y > maxy34) || (y < miny34))
						return 0;
				}
				else
				{	// almost vertical line check x
					if ((x > maxx34) || (x < minx34))
						return 0;
				}
			}
			else
			{	// almost vertical line check x
				if ((x > maxx12) || (x < minx12))
					return 0;

				if (fabs(b) > 1)
				{	// almost vertical line check y
					if ((y > maxy34) || (y < miny34))
						return 0;
				}
				else
				{	// almost vertical line check x
					if ((x > maxx34) || (x < minx34))
						return 0;
				}
			}
		}
	}

	LineCrossX = x;
	LineCrossY = y;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 TestLinesConnected(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4,
                         int32 mode)
{
	if ((X1GreaterThenX2(min(x1, x2), max(x3, x4))) || (X1GreaterThenX2(min(y1, y2), max(y3, y4)))
	        || (X1SmallerThenX2(max(x1, x2), min(x3, x4))) || (X1SmallerThenX2(max(y1, y2), min(y3, y4))))
		return 0;


	if (InRange(x1, x2))
	{

// *************************************************************************************
// Vertical wire
// *************************************************************************************

//  Vertical bus
		if (InRange(x3, x4))
			return 1;

//  Horizontal bus
		if (InRange(y3, y4))
		{
			if ((mode == 1) || (InRange(y1, y3)) || (InRange(y2, y3)) || (InRange(x1, x3)) || (InRange(x1, x4)))
				return 1;

			return 0;
		}

//  All angle bus

		if (mode == 0)
		{
			if (TestLineConnectedToCircle(x1, y1, x2, y2, x3, y3, 0.04))
				return 1;

			if (TestLineConnectedToCircle(x1, y1, x2, y2, x4, y4, 0.04))
				return 1;

			if (TestLineConnectedToCircle(x3, y3, x4, y4, x1, y1, 0.04))
				return 1;

			if (TestLineConnectedToCircle(x3, y3, x4, y4, x2, y2, 0.04))
				return 1;
		}
		else
		{
			if (LineCrosses(x1, y1, x2, y2, x3, y3, x4, y4))
				return 1;
		}

		return 0;
	}

	if (InRange(y1, y2))
	{

// *************************************************************************************
// Horizontal wire
// *************************************************************************************

		if (InRange(y3, y4))
			return 1;			//  Horizontal bus

// *************************************************************************************
//  Vertical bus
		if (InRange(x3, x4))
		{
			if ((mode == 1) || (InRange(y1, y3)) || (InRange(y1, y4)) || (InRange(x1, x3)) || (InRange(x2, x3)))
				return 1;

			return 0;
		}

//  All angle bus

		if (mode == 0)
		{
			if (TestLineConnectedToCircle(x1, y1, x2, y2, x3, y3, 0.04))
				return 1;

			if (TestLineConnectedToCircle(x1, y1, x2, y2, x4, y4, 0.04))
				return 1;

			if (TestLineConnectedToCircle(x3, y3, x4, y4, x1, y1, 0.04))
				return 1;

			if (TestLineConnectedToCircle(x3, y3, x4, y4, x2, y2, 0.04))
				return 1;
		}
		else
		{
			if (LineCrosses(x1, y1, x2, y2, x3, y3, x4, y4))
				return 1;
		}

		return 0;
	}

// *************************************************************************************
// All angle wire
// *************************************************************************************

	if (mode == 0)
	{
		if (TestLineConnectedToCircle(x1, y1, x2, y2, x3, y3, 0.04))
			return 1;

		if (TestLineConnectedToCircle(x1, y1, x2, y2, x4, y4, 0.04))
			return 1;

		if (TestLineConnectedToCircle(x3, y3, x4, y4, x1, y1, 0.04))
			return 1;

		if (TestLineConnectedToCircle(x3, y3, x4, y4, x2, y2, 0.04))
			return 1;
	}
	else
	{
		if (LineCrosses(x1, y1, x2, y2, x3, y3, x4, y4))
			return 1;
	}

	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RegroupNetObjects(int32 mode)
{
	int32 cnt;

	InstanceRecord *Instance, *Instance2, NewInstance;
	WireRecord *Wire, *Wire2, NewWire;
	BusRecord *Bus, *Bus2, NewBus;
	BusConnectionRecord *BusConnection, *BusConnection2, NewBusConnection;
	GlobalConnectionRecord *GlobalConnection, *GlobalConnection2, NewGlobalConnection;
	JunctionRecord *Junction, *Junction2, NewJunction;
	NetLabelRecord *NetLabel, *NetLabel2, NewNetLabel;

	NetNrWires = Design.NrWires;
	NetNrBusses = Design.NrBusses;
	NetNrJunctions = Design.NrJunctions;
	NetNrBusConnections = Design.NrBusConnections;
	NetNrGlobalConnections = Design.NrGlobalConnections;
	NetNrInstances = Design.NrInstances;
	NetNrNetLabels = Design.NrNetLabels;



// ********************************************************************************
	cnt = 0;

	while (cnt < NetNrWires)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NetNrWires - 1)
			{
				Wire2 = &((*Wires)[NetNrWires - 1]);

				if ((Wire2->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					memcpy(&NewWire, Wire, sizeof(WireRecord));
					memcpy(Wire, Wire2, sizeof(WireRecord));
					memcpy(Wire2, &NewWire, sizeof(WireRecord));
					cnt++;
				}
			}

			NetNrWires--;
		}
		else
			cnt++;
	}

// ********************************************************************************
	cnt = 0;

	while (cnt < NetNrBusses)
	{
		Bus = &((*Busses)[cnt]);

		if ((Bus->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NetNrBusses - 1)
			{
				Bus2 = &((*Busses)[NetNrBusses - 1]);

				if ((Bus2->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					memcpy(&NewBus, Bus, sizeof(BusRecord));
					memcpy(Bus, Bus2, sizeof(BusRecord));
					memcpy(Bus2, &NewBus, sizeof(BusRecord));
					cnt++;
				}
			}

			NetNrBusses--;
		}
		else
			cnt++;
	}

// ********************************************************************************
	cnt = 0;

	while (cnt < NetNrJunctions)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NetNrJunctions - 1)
			{
				Junction2 = &((*Junctions)[NetNrJunctions - 1]);

				if ((Junction2->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					memcpy(&NewJunction, Junction, sizeof(JunctionRecord));
					memcpy(Junction, Junction2, sizeof(JunctionRecord));
					memcpy(Junction2, &NewJunction, sizeof(JunctionRecord));
					cnt++;
				}
			}

			NetNrJunctions--;
		}
		else
			cnt++;
	}

// ********************************************************************************
	cnt = 0;

	while (cnt < NetNrNetLabels)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NetNrNetLabels - 1)
			{
				NetLabel2 = &((*NetLabels)[NetNrNetLabels - 1]);

				if ((NetLabel2->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					memcpy(&NewNetLabel, NetLabel, sizeof(NetLabelRecord));
					memcpy(NetLabel, NetLabel2, sizeof(NetLabelRecord));
					memcpy(NetLabel2, &NewNetLabel, sizeof(NetLabelRecord));
					cnt++;
				}
			}

			NetNrNetLabels--;
		}
		else
			cnt++;
	}

// ********************************************************************************
	cnt = 0;

	while (cnt < NetNrBusConnections)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NetNrBusConnections - 1)
			{
				BusConnection2 = &((*BusConnections)[NetNrBusConnections - 1]);

				if ((BusConnection2->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					memcpy(&NewBusConnection, BusConnection, sizeof(BusConnectionRecord));
					memcpy(BusConnection, BusConnection2, sizeof(BusConnectionRecord));
					memcpy(BusConnection2, &NewBusConnection, sizeof(BusConnectionRecord));
					cnt++;
				}
			}

			NetNrBusConnections--;
		}
		else
			cnt++;
	}

// ********************************************************************************
	cnt = 0;

	while (cnt < NetNrGlobalConnections)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if ((GlobalConnection->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NetNrGlobalConnections - 1)
			{
				GlobalConnection2 = &((*GlobalConnections)[NetNrGlobalConnections - 1]);

				if ((GlobalConnection2->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					memcpy(&NewGlobalConnection, GlobalConnection, sizeof(GlobalConnectionRecord));
					memcpy(GlobalConnection, GlobalConnection2, sizeof(GlobalConnectionRecord));
					memcpy(GlobalConnection2, &NewGlobalConnection, sizeof(GlobalConnectionRecord));
					cnt++;
				}
			}

			NetNrGlobalConnections--;
		}
		else
			cnt++;
	}

// ********************************************************************************
	cnt = 0;

	while (cnt < NetNrInstances)
	{
		Instance = &((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NetNrInstances - 1)
			{
				Instance2 = &((*Instances)[NetNrInstances - 1]);

				if ((Instance2->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					memcpy(&NewInstance, Instance, sizeof(InstanceRecord));
					memcpy(Instance, Instance2, sizeof(InstanceRecord));
					memcpy(Instance2, &NewInstance, sizeof(InstanceRecord));
					cnt++;
				}
			}

			NetNrInstances--;
		}
		else
			cnt++;
	}

// ********************************************************************************

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetMinMaxInstanceReferenceText(InstanceRecord * Instance)
{
	double x1, y1, hulp;
	int32 PinTextRotation, Alignment, ObjectMirrorX, ObjectMirrorY, Rotation;
	int32 Check = 0;
	char str[MAX_LENGTH_STRING];

	if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) != 0)
		return -1;

	if ((EditingSymbol) && (!EditingSheetSymbol))
		Check = 1;

	if ((!EditingSymbol) && ((Instance->Info & SHEET_SYMBOL) == 0) && ((Instance->RefInfo & TEXT_NOT_VISIBLE) == 0))
		Check = 1;

	if (!Check)
		return -1;

	Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
	ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
	ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);

	PinTextRotation = (((Instance->RefInfo >> 8) & 1) + Rotation) & 1;
	Alignment = Instance->RefInfo & 0x0f;

	x1 = Instance->RefOriginX;
	y1 = Instance->RefOriginY;

	switch (Rotation)
	{
	case 0:
		if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
		{
			if (ObjectMirrorX == 1)
			{
				x1 = -x1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				if (PinTextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (PinTextRotation == 1)
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

				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				y1 = -y1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorX[Alignment];
			}
		}
		else
		{
			if (PinTextRotation == 0)
				Alignment = TextMirrorX[Alignment];

			if (PinTextRotation == 1)
				Alignment = TextMirrorY[Alignment];
		}

		break;
	}

	strcpy(str, Instance->Reference);

	if ((!EditingSheetSymbol) && (AppendPropertiesToReferences)
		&& ((Instance->Properties[0] != 0) || (Instance->PlacingOption != -1)))
		strcat(str, " (...)");

	GetMinMaxText(Instance->OriginX + x1, Instance->OriginY + y1, 1.0, 0, PinTextRotation, Alignment, str);
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetMinMaxInstanceValueText(InstanceRecord * Instance)
{
	double x1, y1, hulp;
	int32 PinTextRotation, Alignment, ObjectMirrorX, ObjectMirrorY, Rotation;
	int32 Check = 0;

	if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) != 0)
		return -1;

	if (EditingSymbol)
		Check = 1;

	if ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == 0)
		Check = 1;

	if (!Check)
		return -1;


	Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
	ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
	ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);

	PinTextRotation = (((Instance->ValueInfo >> 8) & 1) + Rotation) & 1;
	Alignment = Instance->ValueInfo & 0x0f;

	x1 = Instance->ValueOriginX;
	y1 = Instance->ValueOriginY;

	switch (Rotation)
	{
	case 0:
		if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
		{
			if (ObjectMirrorX == 1)
			{
				x1 = -x1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				if (PinTextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (PinTextRotation == 1)
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

				if (PinTextRotation == 0)
					Alignment = TextMirrorX[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorY[Alignment];
			}

			if (ObjectMirrorY == 1)
			{
				y1 = -y1;

				if (PinTextRotation == 0)
					Alignment = TextMirrorY[Alignment];

				if (PinTextRotation == 1)
					Alignment = TextMirrorX[Alignment];
			}
		}
		else
		{
			if (PinTextRotation == 0)
				Alignment = TextMirrorX[Alignment];

			if (PinTextRotation == 1)
				Alignment = TextMirrorY[Alignment];
		}

		break;
	}

	GetMinMaxText(Instance->OriginX + x1, Instance->OriginY + y1, 1.0, 0, PinTextRotation, Alignment, Instance->Value);
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 RectTestLine2(double LineX1, double LineY1, double LineX2, double LineY2)
{
	double LineXmin, LineXmax, LineYmin, LineYmax, DifX, DifY, xa, xb, ya, yb;
	double rico, rico2;

	LineXmin = LineX1;
	LineYmin = LineY1;
	LineXmax = LineX2;
	LineYmax = LineY2;

	if (LineXmin > LineXmax)
	{
		LineXmin = LineX2;
		LineXmax = LineX1;
	}

	if (LineYmin > LineYmax)
	{
		LineYmin = LineY2;
		LineYmax = LineY1;
	}

	if ((X1SmallerThenX2(LineXmax, SearchMinX)) || (X1GreaterThenX2(LineXmin, SearchMaxX))
	        || (X1SmallerThenX2(LineYmax, SearchMinY)) || (X1GreaterThenX2(LineYmin, SearchMaxY)))
		return 0;


	if ((X1SmallerThenX2(LineXmax, SearchMaxX)) && (X1GreaterThenX2(LineXmin, SearchMinX))
	        && (X1SmallerThenX2(LineYmax, SearchMaxY)) && (X1GreaterThenX2(LineYmin, SearchMinY)))
		return 3;


	if ((X1SmallerThenX2(LineX1, SearchMaxX)) && (X1GreaterThenX2(LineX1, SearchMinX))
	        && (X1SmallerThenX2(LineY1, SearchMaxY)) && (X1GreaterThenX2(LineY1, SearchMinY)))
		return 1;

	if ((X1SmallerThenX2(LineX2, SearchMaxX)) && (X1GreaterThenX2(LineX2, SearchMinX))
	        && (X1SmallerThenX2(LineY2, SearchMaxY)) && (X1GreaterThenX2(LineY2, SearchMinY)))
		return 2;

	if ((InRange(LineX1, LineX2)) || (InRange(LineY1, LineY2)))
		return 3;

	DifX = LineX2 - LineX1;
	DifY = LineY2 - LineY1;
	rico = LineY2;
	rico = (rico - LineY1) / (LineX2 - LineX1);
	ya = ((SearchMinX - LineX1) * rico) + LineY1;
	yb = ((SearchMaxX - LineX1) * rico) + LineY1;

	if (((ya > SearchMinY) && (ya < SearchMaxY)) || ((yb > SearchMinY) && (yb < SearchMaxY)))
		return 3;

	rico2 = LineX2;
	rico2 = (rico2 - LineX1) / (LineY2 - LineY1);
	xa = ((SearchMinY - LineY1) * rico2) + LineX1;
	xb = ((SearchMaxY - LineY1) * rico2) + LineX1;

	if (((xa > SearchMinX) && (xa < SearchMaxX)) || ((xb > SearchMinX) && (xb < SearchMaxX)))
		return 3;

	return 0;

	/*

	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	           (y2-y1)
	y = (x-x1)*------- + y1
	           (x2-x1)

	           (x2-x1)
	x = (y-y1)*------- + x1
	           (y2-y1)



	y = x-x1 + y1

	x = y-y1 + x1

	*/

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 LinesOverlap(double Line1X1, double Line1Y1, double Line1X2, double Line1Y2, double Line2X1, double Line2Y1,
                   double Line2X2, double Line2Y2)
{
	double Rect1Xmin, Rect1Xmax, Rect1Ymin, Rect1Ymax, Rect2Xmin, Rect2Xmax, Rect2Ymin, Rect2Ymax;

	Rect1Xmin = min(Line1X1, Line1X2);
	Rect1Xmax = max(Line1X1, Line1X2);
	Rect1Ymin = min(Line1Y1, Line1Y2);
	Rect1Ymax = max(Line1Y1, Line1Y2);

	Rect2Xmin = min(Line2X1, Line2X2);
	Rect2Xmax = max(Line2X1, Line2X2);
	Rect2Ymin = min(Line2Y1, Line2Y2);
	Rect2Ymax = max(Line2Y1, Line2Y2);

// #define X1SmallerThenX2(x1,x2) ( (x1<x2-0.01) ? (1) : (0) )
// #define X1GreaterThenX2(x1,x2) ( (x1>x2+0.01) ? (1) : (0) )

	if ((X1SmallerThenX2(Rect1Xmax, Rect2Xmin)) || (X1SmallerThenX2(Rect1Ymax, Rect2Ymin))
	        || (X1GreaterThenX2(Rect1Xmin, Rect2Xmax)) || (X1GreaterThenX2(Rect1Ymin, Rect2Ymax)))
		return 0;

	if ((NotInRange(Line1X1, Line1X2)) && (NotInRange(Line1Y1, Line1Y2)))
		return 0;

	if ((NotInRange(Line2X1, Line2X2)) && (NotInRange(Line2Y1, Line2Y2)))
		return 0;

	if ((InRange(Line1X1, Line1X2)) && (InRange(Line2X1, Line2X2)))
		return 1;

	if ((InRange(Line1Y1, Line1Y2)) && (InRange(Line2Y1, Line2Y2)))
		return 1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ConvertPinBusStr(ObjectRecord * PinBusObject, LPSTR NewPinBusStr, int32 mode)
{
	int32 res, cnt, cnt2, LineNr;
	InstanceRecord *Instance;
	LPSTR PinBusStr;
	RedefinedPinBusRecord *RedefinedPinBus;
	char str[500], str2[MAX_LENGTH_STRING];

	Instance = PinBusObject->Instance;

	for (cnt = 0; cnt < Design.NrRedefinedPinBusses; cnt++)
	{
		RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);

		if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((stricmpUTF8(RedefinedPinBus->Reference, Instance->Reference) == 0)
			        && (stricmpUTF8(RedefinedPinBus->Name, PinBusObject->Text2) == 0))
			{
				LineNr = 0;
				str[0] = 0;
				PinBusStr = PinBusObject->Text1;

				for (cnt2 = 0; cnt2 < PinBusObject->Info3; cnt2++)
				{
					res = GetPinNameFromPinBus(PinBusStr, str2, PinBusObject->Info3, RedefinedPinBus->Order[cnt2]);

					if (res > LineNr)
						strcat(str, "\\");
					else if (cnt2 > 0)
						strcat(str, ",");

					strcat(str, str2);
					LineNr = res;
				}

				strcpy(NewPinBusStr, str);
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ConvertPointToPolar(double x, double y, double *Distance, double *Angle)
{
	double r1, length, hoek;

	length = sqrt(SQR(x) + SQR(y));
	*Distance = length;

	if (InRangeSpecial(x, 0.0, 0.01))
	{	// Vertical line
		if (y > 0.0)
		{	// Vertical line to top
			hoek = ANGLE_90;
		}
		else
		{	// Vertical line to bottom
			hoek = ANGLE_270;
		}
	}
	else
	{
		r1 = y / x;
		hoek = atan(r1);

		if (r1 > 0.0)
		{
			if (x < 0.0)
				hoek += ANGLE_180;
		}
		else
		{
			if (x > 0.0)
				hoek += ANGLE_360;
			else
				hoek += ANGLE_180;
		}
	}

	if (hoek > ANGLE_360 - 0.001)
		hoek -= ANGLE_360;

	*Angle = (hoek * 180.0 / PI);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double DistancePointToLine(double px, double py, double LineX1, double LineY1, double LineX2, double LineY2)
{
	/*

	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	    (y2-y1)
	a = -------
	    (x2-x1)

	           (y2-y1)                     (y2-y1)
	y = (x-x1)*------- + y1       = (x-x2)*------- + y2
	           (x2-x1)                     (x2-x1)


	y = (x-x1)*a + y1

	           (x2-x1)                     (x2-x1)
	x = (y-y1)*------- + x1       = (y-y2)*------- + x2
	           (y2-y1)                     (y2-y1)

	x = (y-y1)/a + x1

	-----------------------------------------------------------------------

	a = 1

	y = x-x1 + y1     b = -x1 + y1

	x = y-y1 + x1

	a = -1

	y = x1-x + y1     b =  x1 + y1

	x = y1-y + x1

	memcmp

	*/


	double minx, maxx, miny, maxy, DistLine1, DistLine2, a, x2, y2, DistCentre;

	minx = min(LineX1, LineX2);
	maxx = max(LineX1, LineX2);
	miny = min(LineY1, LineY2);
	maxy = max(LineY1, LineY2);

	DistLine1 = CalcLengthLine(LineX1, LineY1, px, py);
	DistLine2 = CalcLengthLine(LineX2, LineY2, px, py);

	if (LineX2 == LineX1)
	{
		DistCentre = fabs(LineX1 - px);

		if ((py >= miny) && (py <= maxy) && (DistCentre < DistLine1) && (DistCentre < DistLine2))
			return DistCentre;

	}
	else
	{
		if (LineY2 == LineY1)
		{
			DistCentre = fabs(LineY1 - py);

			if ((px >= minx) && (px <= maxx) && (DistCentre < DistLine1) && (DistCentre < DistLine2))
				return DistCentre;

		}
		else
		{
			a = ((LineY2 - LineY1) / (LineX2 - LineX1));
			x2 = (LineX1 * a - LineY1 + (px / a) + py) / (a + 1 / a);

			if ((x2 >= minx) && (x2 <= maxx))
			{
				y2 = ((x2 - px) / -a) + py;
				DistCentre = CalcLengthLine(x2, y2, px, py);

				if ((DistCentre < DistLine1) && (DistCentre < DistLine2))
					return DistCentre;
			}
		}
	}

	return min(DistLine1, DistLine2);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetNetLabelIndexFromEndPointLine(double x1, double y1)
{
	int32 cnt;
	double x2, y2;
	NetLabelRecord *NetLabel;

	for (cnt = 0; cnt < min(MaxNrNetLabels, Design.NrNetLabels); cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x2 = NetLabel->ConnectX;
			y2 = NetLabel->ConnectY;

			if ((InRangeSpecial(x1, x2, 0.01)) && (InRangeSpecial(y1, y2, 0.01)))
				return cnt;
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDimensionTextFromLine(double x1, double y1, double x2, double y2, ObjectTextRecord * ObjectText, int32 mode)
{
	double Angle, Length;
	int32 Quadrant;
	double cx, cy, tx, ty, x3, y3, dx, dy, TextAngle, Angle2;
	char str[MAX_LENGTH_STRING];


	x3 = 0.0;
	y3 = 0.0;
	ObjectText->Text[0] = 0;
	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

	if (((mode & 12) != 0) && (Length > 0.0))
	{
		Quadrant = (int32) ((Angle + ANGLE_CONVERT(22.5)) / ANGLE_CONVERT(45.0));
		sprintf(str, "%.2f", Length);
		GetMinMaxText2(0.0, 0.0, ObjectText->FontHeight, 0, 0.0, 0, 0, str);
		dx = TextMaxX - TextMinX;
		dy = TextMaxY - TextMinY;
		TextAngle = Angle;

		if ((mode & 8) == 0)
		{
			if ((mode & 4) == 4)
			{
				if (Length < (TextMaxX - TextMinX) + ObjectText->FontHeight * 2.0)
				{
					cx = x1 + cos(Angle) * (Length + dx * 0.5 + ObjectText->FontHeight * 2.0);
					cy = y1 + sin(Angle) * (Length + dx * 0.5 + ObjectText->FontHeight * 2.0);
					Angle2 = atan(ObjectText->FontHeight * 0.6 / max(Length + dx * 0.5, 1000000.0)) * 180 / PI;
				}
				else
				{
					cx = (x1 + x2) * 0.5;
					cy = (y1 + y2) * 0.5;
					Angle2 = atan(ObjectText->FontHeight * 0.7 / (Length * 0.5)) * 180 / PI;
				}

				tx = cx;
				ty = cy;

				switch (Quadrant)
				{
				case 8:
				case 0:
				case 1:
				case 2:
				case 7:
					RotatePointFromOtherPoint(&tx, &ty, x1, y1, Angle2);
					x3 = tx - dx * 0.5;
					y3 = ty - dy * 0.5;
					RotatePointFromOtherPoint(&x3, &y3, tx, ty, Angle * 180 / PI);
					break;

				case 3:
				case 4:
				case 5:
				case 6:
					RotatePointFromOtherPoint(&tx, &ty, x1, y1, -Angle2);
					x3 = tx + dx * 0.5;
					y3 = ty + dy * 0.5;
					RotatePointFromOtherPoint(&x3, &y3, tx, ty, Angle * 180 / PI);
					TextAngle += ANGLE_CONVERT(180.0);
					break;
				}
			}
		}
		else
		{
			Angle2 = atan(ObjectText->FontHeight * 0.6 / max(Length + dx * 0.5, 1000000.0)) * 180 / PI;
			tx = x1 + cos(Angle) * (Length + dx * 0.5 + ObjectText->FontHeight * 2.0);
			ty = y1 + sin(Angle) * (Length + dx * 0.5 + ObjectText->FontHeight * 2.0);

			switch (Quadrant)
			{
			case 8:
			case 0:
			case 1:
			case 2:
			case 7:
				RotatePointFromOtherPoint(&tx, &ty, x1, y1, Angle2);
				x3 = tx - dx * 0.5;
				y3 = ty - dy * 0.5;
				RotatePointFromOtherPoint(&x3, &y3, tx, ty, Angle * 180 / PI);
				break;

			case 3:
			case 4:
			case 5:
			case 6:
				RotatePointFromOtherPoint(&tx, &ty, x1, y1, -Angle2);
				x3 = tx + dx * 0.5;
				y3 = ty + dy * 0.5;
				RotatePointFromOtherPoint(&x3, &y3, tx, ty, Angle * 180 / PI);
				TextAngle += ANGLE_CONVERT(180.0);
				break;
			}
		}

		ObjectText->X = (float) x3;
		ObjectText->Y = (float) y3;
		strcpy(ObjectText->Text, str);
		ObjectText->Rotation = (float) (TextAngle * 180 / PI);
		return 0;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DimensionToLineSegments(double x1, double y1, double x2, double y2, double *LineSegments, int32 mode)
{
	double Angle, Length, Length2;
	double x1old, y1old, x2old, y2old, x3, y3, x4, y4;
	char str[MAX_LENGTH_STRING];
	int32 SegmentCount = 0;

	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

	if (Length > 0.0)
	{
		if ((mode & 12) == 0)
		{
			LineSegments[SegmentCount++] = x1;
			LineSegments[SegmentCount++] = y1;
			LineSegments[SegmentCount++] = x2;
			LineSegments[SegmentCount++] = y2;

			if ((mode & 1) == 1)
			{
				x3 = x1 + cos(Angle) * Design.ArrowLength;
				y3 = y1 + sin(Angle) * Design.ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint(&x4, &y4, x1, y1, 30.0);
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint(&x4, &y4, x1, y1, -30.0);
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}

			if ((mode & 2) == 2)
			{
				x3 = x2 - cos(Angle) * Design.ArrowLength;
				y3 = y2 - sin(Angle) * Design.ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint(&x4, &y4, x2, y2, 30.0);
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint(&x4, &y4, x2, y2, -30.0);
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}
		}
		else
		{
			if ((mode & 8) == 0)
			{
				sprintf(str, "%.2f", Length);
				GetMinMaxText2(0.0, 0.0, Design.DimensionHeight, 0, 0.0, 0, 0, str);

				if (Length < (TextMaxX - TextMinX) + Design.DimensionHeight * 2.0)
				{
					x1old = x1;
					y1old = y1;
					x2old = x2;
					y2old = y2;
					x1 -= cos(Angle) * Design.ArrowLength;
					y1 -= sin(Angle) * Design.ArrowLength;
					Length2 = Length + (TextMaxX - TextMinX) + Design.DimensionHeight * 2.0;
					x2 = x1old + cos(Angle) * Length2;
					y2 = y1old + sin(Angle) * Length2;
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;

					x3 = x1old - cos(Angle) * Design.ArrowLength;
					y3 = y1old - sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x1old, y1old, 30.0);
					LineSegments[SegmentCount++] = x1old;
					LineSegments[SegmentCount++] = y1old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x1old, y1old, -30.0);
					LineSegments[SegmentCount++] = x1old;
					LineSegments[SegmentCount++] = y1old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;

					x3 = x2old + cos(Angle) * Design.ArrowLength;
					y3 = y2old + sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x2old, y2old, 30.0);
					LineSegments[SegmentCount++] = x2old;
					LineSegments[SegmentCount++] = y2old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x2old, y2old, -30.0);
					LineSegments[SegmentCount++] = x2old;
					LineSegments[SegmentCount++] = y2old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
				}
				else
				{
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					x3 = x1 + cos(Angle) * Design.ArrowLength;
					y3 = y1 + sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x1, y1, 30.0);
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x1, y1, -30.0);
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x3 = x2 - cos(Angle) * Design.ArrowLength;
					y3 = y2 - sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x2, y2, 30.0);
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x2, y2, -30.0);
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
				}
			}
			else
			{
				sprintf(str, "%.2f", Length);
				GetMinMaxText2(0.0, 0.0, Design.DimensionHeight, 0, 0.0, 0, 0, str);
				x2old = x2;
				y2old = y2;
				Length2 = Length + (TextMaxX - TextMinX) + Design.DimensionHeight * 2.0;
				x2 = x1 + cos(Angle) * Length2;
				y2 = y1 + sin(Angle) * Length2;
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				x3 = x2old + cos(Angle) * Design.ArrowLength;
				y3 = y2old + sin(Angle) * Design.ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint(&x4, &y4, x2old, y2old, 30.0);
				LineSegments[SegmentCount++] = x2old;
				LineSegments[SegmentCount++] = y2old;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint(&x4, &y4, x2old, y2old, -30.0);
				LineSegments[SegmentCount++] = x2old;
				LineSegments[SegmentCount++] = y2old;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}
		}
	}

	return SegmentCount / 4;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetNrCrossingFromWirePoint(double x, double y)
{
	InstanceRecord *Instance;
	int32 cnt, cnt2, count;
	ObjectRecord *Object;
	WireRecord *Wire;

	count = 0;

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((InRange(x, Wire->X1)) && (InRange(y, Wire->Y1)))
				count++;

			if ((InRange(x, Wire->X2)) && (InRange(y, Wire->Y2)))
				count++;
		}
	}

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			NrObjects = 0;
			InstanceToObject(Instance, 0.0, 0.0, 1);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->ObjectType == SYMBOL_PIN)
				{
					if ((InRange(Object->x1, x)) && (InRange(Object->y1, y)))
						count++;
				}
			}
		}
	}

	return count;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
