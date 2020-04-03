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
#include "stddef.h"
#include "memory.h"
#include "calc.h"
#include "files.h"
#include "files2.h"
#include "string.h"
#include "math.h"
#include "stdio.h"
#include "instance.h"
#include "utf8.h"
#include "owntime.h"
#include "change.h"

/*
#define ALIGN_LEFT_BOTTOM                       0            2 . 8
#define ALIGN_LEFT_CENTRE                       1            1 . 7
#define ALIGN_LEFT_TOP                          2            0 . 6
#define ALIGN_RIGHT_BOTTOM                      6
#define ALIGN_RIGHT_CENTRE                      7
#define ALIGN_RIGHT_TOP                         8
*/

#define    InRange7(x1,x2) ( (((x1>x2-0.000001) && (x1<x2+0.000001))) ? (1) : (0) )
#define    PI              3.14159265358979262
#define    SQR(x)          ((x)*(x))

extern int32 DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY;

int32 NrSubSheets, ok;

double LineCrossX, LineCrossY;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InstancePinsToObject(InstanceRecord * Instance, int32 Mode)
{
	int32 MemPos, MemPos2, cnt, cnt2, PowerPinMem, PinMem, PinBusMem, Rotation, SymbolNr, NrPinBusses, NrPowerPins,
	      NrPins, NrPartsPerPackage, PackagePartNr, pos, pos2, LengthIdent, LengthValue, NrPowerPinOverRules,
	      ObjectMirrorX, ObjectMirrorY;
	double hulp, x1, y1, OX, OY, OriginX, OriginY;
	LPSTR PinText, PowerPinText, PowerPinNetName, LabelText, InstanceAttrBuf, PowerPinOverRuleNetName[64],
	      PowerPinOverRuleNewNetName[64];
	ObjectRecord *Object;
	SymbolRecord *Symbol;
	SubPinDefsArray *SubPinDefs;
	PowerPinRecord *PowerPin;
	PinRecord *Pin;
	PinBusRecord *PinBus;

	SymbolNr = Instance->AddNr;

	if (SymbolNr == -1)
		return;

//  SymbolNr=(int16)Instance->SymbolNr;
#ifdef _DEBUG

	if (stricmp(Instance->SymbolName, "pc-at") == 0)
		ok = 1;

#endif

	if ((Mode & 2) == 0)
	{
		MemPos = (*SymbolsPos2)[SymbolNr].Pos;
		Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);
		MemPos2 = MemPos + sizeof(SymbolRecord);
		SubPinDefs = (SubPinDefsArray *) & ((*SymbolsMem)[MemPos2]);
	}
	else
	{
		MemPos = (*NewSymbolsPos2)[SymbolNr].Pos;
		Symbol = (SymbolRecord *) & ((*NewSymbolsMem)[MemPos]);
		MemPos2 = MemPos + sizeof(SymbolRecord);
		SubPinDefs = (SubPinDefsArray *) & ((*NewSymbolsMem)[MemPos2]);
	}

	if ((Symbol->Info & MULTIPLE_SYMBOLS) == 0)
		NrPartsPerPackage = Symbol->NrPartsPerPackage;
	else
		NrPartsPerPackage = 1;

	PackagePartNr = min((int16) Instance->PackagePartNr, NrPartsPerPackage);

	Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
	OriginX = Instance->OriginX;
	OriginY = Instance->OriginY;
	ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
	ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);

	NrPowerPins = Symbol->NrPowerPins;
	NrPins = Symbol->NrPins;
	NrPinBusses = Symbol->NrPinBusses;
	PinMem = MemPos2 + Symbol->NrSubPinDefs * sizeof(SubPinDefsType);
	PowerPinMem = PinMem + NrPins * sizeof(PinRecord);
	PinBusMem = PowerPinMem + NrPowerPins * sizeof(PowerPinRecord);
	OX = Symbol->OriginX;
	OY = Symbol->OriginY;

// ************************************************************************
// Pin

	if ((Mode & 1) == 0)
	{
		cnt = NrPins;

		while (cnt > 0)
		{
			if ((Mode & 2) == 0)
				Pin = (PinRecord *) & ((*SymbolsMem)[PinMem]);
			else
				Pin = (PinRecord *) & ((*NewSymbolsMem)[PinMem]);

			x1 = Pin->X - OX;
			y1 = Pin->Y - OY;

			switch (Rotation)
			{
			case 0:
				if (ObjectMirrorX == 1)
					x1 = -x1;

				if (ObjectMirrorY == 1)
					y1 = -y1;

				break;

			case 1:			//  90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;

				if (ObjectMirrorX == 1)
					x1 = -x1;

				if (ObjectMirrorY == 1)
					y1 = -y1;

				break;
			}

			PinText = (Pin->Name);
			LabelText = (Pin->Label);

			if (NrPartsPerPackage > 1)
				PinText = ((*SubPinDefs)[(NrPins - cnt) * NrPartsPerPackage + PackagePartNr - 1]);

			if ((NrObjects + 1 >= MaxNrObjects) && (AllocateMemObjects(MaxNrObjects + 128) != 0))
				return;

			Object = &((*Objects)[NrObjects]);
			Object->ObjectType = SYMBOL_PIN;
			Object->x1 = (float) (x1 + OriginX);
			Object->y1 = (float) (y1 + OriginY);
			Object->x2 = 0.0;
			Object->Info = 0;

			if ((Mode & 2) == 0)
				Object->Info2 = Pin->ConnectionType;
			else
				Object->Info2 = Pin->SwapInfo;

			if ((Instance->Info & SHEET_SYMBOL) == SHEET_SYMBOL)
			{
				Object->Info3 = 1;
				Object->Text1 = Instance->SymbolName;
			}
			else
			{
				Object->Info3 = 0;
				Object->Text1 = PinText;
			}

			Object->Text2 = LabelText;
			Object->PinNr = (int16) (NrPins - cnt);
			Object->Instance = Instance;
			NrObjects++;
			cnt--;
			PinMem += sizeof(PinRecord);
		}
	}

// ************************************************************************
// PowerPin

	if ((Mode & 1) == 1)
	{
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
		cnt = NrPowerPins;

		while (cnt > 0)
		{
			if ((Mode & 2) == 0)
				PowerPin = (PowerPinRecord *) & ((*SymbolsMem)[PowerPinMem]);
			else
				PowerPin = (PowerPinRecord *) & ((*NewSymbolsMem)[PowerPinMem]);

			PowerPinNetName = (PowerPin->NetName);

			for (cnt2 = 0; cnt2 < NrPowerPinOverRules; cnt2++)
			{
				if (stricmpUTF8(PowerPinOverRuleNetName[cnt2], PowerPin->NetName) == 0)
				{
					PowerPinNetName = PowerPinOverRuleNewNetName[cnt2];
					break;
				}
			}

			PowerPinText = (PowerPin->Text);

			if ((NrObjects + 1 >= MaxNrObjects) && (AllocateMemObjects(MaxNrObjects + 128) != 0))
				return;

			Object = &((*Objects)[NrObjects]);
			Object->ObjectType = SYMBOL_POWERPIN;
			Object->x1 = 0.0;
			Object->y1 = 0.0;
			Object->x2 = 0.0;
			Object->Info = 0;
			Object->Info2 = 0;
			Object->Info3 = 0;
			Object->Text1 = PowerPinNetName;
			Object->Text2 = PowerPinText;
			Object->PinNr = (int16) (NrPowerPins - cnt);
			Object->Instance = Instance;
			NrObjects++;
			cnt--;
			PowerPinMem += sizeof(PowerPinRecord);
		}
	}

// ************************************************************************
// Pin bus

	if ((Mode & 1) == 0)
	{
		cnt = NrPinBusses;

		while (cnt > 0)
		{
			if ((Mode & 2) == 0)
				PinBus = (PinBusRecord *) & ((*SymbolsMem)[PinBusMem]);
			else
				PinBus = (PinBusRecord *) & ((*NewSymbolsMem)[PinBusMem]);

			x1 = PinBus->X - OX;
			y1 = PinBus->Y - OY;

			switch (Rotation)
			{
			case 0:
				if (ObjectMirrorX == 1)
					x1 = -x1;

				if (ObjectMirrorY == 1)
					y1 = -y1;

				break;

			case 1:			//  90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;

				if (ObjectMirrorX == 1)
					x1 = -x1;

				if (ObjectMirrorY == 1)
					y1 = -y1;

				break;
			}

			PinText = (PinBus->Text);
			LabelText = (PinBus->Label);

			if ((NrObjects + 1 >= MaxNrObjects) && (AllocateMemObjects(MaxNrObjects + 128) != 0))
				return;

			Object = &((*Objects)[NrObjects]);
			memset(Object, 0, sizeof(ObjectRecord));
			Object->ObjectType = SYMBOL_PINBUS;
			//    Object->InstanceNr=InstanceNr;
			Object->x1 = (float) (x1 + OriginX);
			Object->y1 = (float) (y1 + OriginY);
			Object->x2 = 0.0;
			Object->Info = 0;

			if ((Mode & 2) == 0)
				Object->Info2 = PinBus->ConnectionType;
			else
				Object->Info2 = PinBus->SwapInfo;

			Object->Info3 = PinBus->NrPins;
			Object->Text1 = PinText;
			Object->Text2 = LabelText;
			Object->Instance = Instance;
			Object->PinNr = 0;
			NrObjects++;
			cnt--;
			PinBusMem += sizeof(PinBusRecord);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 TestLineConnectedToCircle(double x1, double y1, double x2, double y2, double CircleX, double CircleY,
                                double CircleDiam)
{
	double a, E, F, A, B, C, D, x11, y11, x22, y22;

	if (InRange(x1, x2))
	{
		B = -2.0 * CircleY;
		C = SQR(CircleY) + SQR(x1 - CircleX) - 0.25 * SQR(CircleDiam);
		D = SQR(B) - 4.0 * C;

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
		C = SQR(CircleX) + SQR(y1 - CircleY) - 0.25 * SQR(CircleDiam);
		D = SQR(B) - 4.0 * C;

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

	F = -SQR(CircleX) - SQR(CircleY) + 0.25 * SQR(CircleDiam);

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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
		return -1;

	miny12 = min(LineY1, LineY2);
	maxy34 = max(LineY3, LineY4);

	if (miny12 > maxy34)
		return -1;

	maxx12 = max(LineX1, LineX2);
	minx34 = min(LineX3, LineX4);

	if (maxx12 < minx34)
		return -1;

	miny34 = min(LineY3, LineY4);
	maxy12 = max(LineY1, LineY2);

	if (maxy12 < miny34)
		return -1;

	/*
	  if ((InRange(LineY2,LineY1))
	     &&
	     (InRange(LineY4,LineY3))) return 0;   // hor

	  if ((InRange(LineX2,LineX1))
	     &&
	     (InRange(LineX4,LineX3))) return 0;   // ver

	  if ((InRange(LineX2-LineX1,LineY2-LineY1))
	     &&
	     (InRange(LineX4-LineX3,LineY4-LineY3))
	     &&
	     (InRange(-LineX3+LineY3,-LineX1+LineY1))) return 0;  // diag2

	  if ((InRange(LineX2-LineX1,LineY1-LineY2))
	     &&
	     (InRange(LineX4-LineX3,LineY3-LineY4))
	     &&
	     (InRange(LineX3+LineY3,LineX1+LineY1))) return 0;    // diag1
	*/

	if ((LineX2 > LineX1 - 0.0001) && (LineX2 < LineX1 + 0.0001))
	{
//  if (InRange7(LineX2,LineX1)) {
		if (InRange7(LineY3, LineY4))
		{
			LineCrossX = LineX1;
			LineCrossY = LineY3;
			return 1;
		}
		else
		{
			if (InRange7(LineX4, LineX3))
				return -1;

			b = (LineY4 - LineY3) / (LineX4 - LineX3);
			x = LineX1;
			y = (x - LineX3) * b + LineY3;

			if ((y > maxy12) || (y < miny12))
				return -1;

			if (fabs(b) > 1)
			{	// almost vertical line check y
				if ((y > maxy34) || (y < miny34))
					return -1;
			}
			else
			{	// almost vertical line check x
				if ((x > maxx34) || (x < minx34))
					return -1;
			}
		}
	}
	else
	{
		if (InRange7(LineX4, LineX3))
		{
			if (InRange7(LineY1, LineY2))
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
					return -1;

				if (fabs(a) > 1)
				{	// almost vertical line check y
					if ((y > maxy12) || (y < miny12))
						return -1;
				}
				else
				{	// almost vertical line check x
					if ((x > maxx12) || (x < minx12))
						return -1;
				}
			}
		}
		else
		{
			if ((LineX2 > LineX1 - 0.01) && (LineX2 < LineX1 + 0.01))
			{
				b = (LineY4 - LineY3) / (LineX4 - LineX3);
				x = LineX1;
				y = (x - LineX3) * b + LineY3;

				if ((y > maxy12) || (y < miny12))
					return -1;
			}
			else
			{
				a = (LineY2 - LineY1) / (LineX2 - LineX1);
				x = a * LineX1;
				x -= LineY1;
				b = (LineY4 - LineY3) / (LineX4 - LineX3);

				if (a == b)
					return -1;

				x += LineY3;
				x += -b * LineX3;
				x = x / (a - b);

				y = (x - LineX1) * a + LineY1;

				if (fabs(a) > 1)
				{	// almost vertical line check y
					if ((y > maxy12) || (y < miny12))
						return -1;

					if (fabs(b) > 1)
					{	// almost vertical line check y
						if ((y > maxy34) || (y < miny34))
							return -1;
					}
					else
					{	// almost vertical line check x
						if ((x > maxx34) || (x < minx34))
							return -1;
					}
				}
				else
				{	// almost vertical line check x
					if ((x > maxx12) || (x < minx12))
						return -1;

					if (fabs(b) > 1)
					{	// almost vertical line check y
						if ((y > maxy34) || (y < miny34))
							return -1;
					}
					else
					{	// almost vertical line check x
						if ((x > maxx34) || (x < minx34))
							return -1;
					}
				}
			}
		}
	}

	LineCrossX = x;
	LineCrossY = y;

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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
			if (((mode & 1) == 1) || (InRange(y1, y3)) || (InRange(y2, y3)) || (InRange(x1, x3)) || (InRange(x1, x4)))
				return 1;

			return 0;
		}

//  All angle bus

		if ((mode & 1) == 0)
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
			if (((mode & 1) == 1) || (InRange(y1, y3)) || (InRange(y1, y4)) || (InRange(x1, x3)) || (InRange(x2, x3)))
				return 1;

			return 0;
		}

//  All angle bus

		if ((mode & 1) == 0)
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetMemSizeSheet(LPSTR FileName, int32 * SheetMemSize, int32 * SymbolMemSize, int32 mode)
{
	int32 result, Designfp, cnt, cnt3, ok, Time1, LoadResult, Pos, Length, TotalSymbolsMem, NrErrors, InstancePos,
	      SymbolNr, MemSize, NrSymbolNames;
	char str[MAX_LENGTH_STRING], SheetName[MAX_LENGTH_STRING], LibName[MAX_LENGTH_STRING], SymbolNames[512][64];
	InstanceRecord Instance;

	GetFilePartFromFileName(SheetName, FileName);
	CutExtensionFileName(SheetName);

	if ((Designfp = FileOpenReadOnlyUTF8(FileName)) == -1)
	{
		sprintf(str, SC(0, "Error in opening file %s\r\n"), FileName);
		AddMessage(str);
//    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		return -1;
	}

	if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
	{
		sprintf(str, SC(1, "Error in reading file %s\r\n"), FileName);
		AddMessage(str);
//    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		return -1;
	}

	if ((stricmp(Design.Identification, SheetCode1) != 0) && (stricmp(Design.Identification, SheetCode2) != 0)
	        && (stricmp(Design.Identification, SheetCode3) != 0) && (stricmp(Design.Identification, SheetCode4) != 0)
	        && (stricmp(Design.Identification, SheetCode5) != 0))
	{
		FileClose(Designfp);
		sprintf(str, SC(2, "%s is not a valid sheet file\r\n"), FileName);
		AddMessage(str);
//    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		return -1;
	}


	MemSize = sizeof(DesignRecord);
	MemSize += Design.NrWires * sizeof(WireRecord);
	MemSize += Design.NrBusses * sizeof(BusRecord);
	MemSize += Design.NrBusConnections * sizeof(BusConnectionRecord);
	MemSize += Design.NrJunctions * sizeof(JunctionRecord);
	MemSize += Design.NrGlobalConnections * sizeof(GlobalConnectionRecord);
	MemSize += Design.NrNetLabels * sizeof(NetLabelRecord);
	MemSize += Design.NrObjectLines * sizeof(ObjectLineRecord);
	MemSize += Design.NrObjectRects * sizeof(ObjectRectRecord);
	MemSize += Design.NrObjectCircles * sizeof(ObjectCircleRecord);
	MemSize += Design.NrObjectArcs * sizeof(ObjectArcRecord);
	MemSize += Design.NrObjectTexts * sizeof(ObjectTextRecord);
	MemSize += Design.NrInstances * sizeof(InstanceRecord);
	MemSize += Design.NrSymbols * sizeof(SymbolsPosRecord);
	MemSize += Design.NrRedefinedPinBusses * sizeof(RedefinedPinBusRecord);
	MemSize += Design.NrOnePinNets * sizeof(OnePinNetRecord);
	InstancePos = Design.NrSymbols * sizeof(SymbolsPosRecord) + sizeof(DesignRecord);

	TotalSymbolsMem = 0;
	NrSymbolNames = 0;
	NrErrors = 0;
	NrSubSheets = 0;
	FileSeek(Designfp, InstancePos);

//  sprintf(Message,"The following symbols were not found/had errors :\r\n\r\n");
	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		if ((stricmp(Design.Identification, SheetCode3) == 0) || (stricmp(Design.Identification, SheetCode4) == 0)
		        || (stricmp(Design.Identification, SheetCode5) == 0))
		{
			if ((stricmp(Design.Identification, SheetCode3) == 0) || (stricmp(Design.Identification, SheetCode4) == 0))
			{
				memset(&Instance, 0, sizeof(InstanceRecord));
				FileRead(Designfp, &Instance, offsetof(OldInstanceRecord, Geometry), &result);
				FileRead(Designfp, &Instance.Geometry,
				         sizeof(OldInstanceRecord) - offsetof(OldInstanceRecord, Geometry), &result);
//        FileRead(Designfp,&Instance,sizeof(InstanceRecord),&result);
			}
			else
				FileRead(Designfp, &Instance, sizeof(InstanceRecord), &result);
		}
		else
		{
			memset(&Instance, 0, sizeof(InstanceRecord));
			FileRead(Designfp, &Instance, offsetof(OldOldInstanceRecord, Geometry), &result);
			FileRead(Designfp, &Instance.Geometry,
			         sizeof(OldOldInstanceRecord) - offsetof(OldOldInstanceRecord, Geometry), &result);


//      FileRead(Designfp,&OldInstance,sizeof(OldInstanceRecord),&result);
//      memmove(&Instance,&OldInstance,sizeof(OldInstanceRecord));
		}

		SymbolNr = -1;

		for (cnt3 = 0; cnt3 < NrSymbolNames; cnt3++)
		{
			if (stricmpUTF8(SymbolNames[cnt3], Instance.SymbolName) == 0)
				SymbolNr = cnt3;
		}

		if (SymbolNr == -1)
		{
			strcpy(SymbolNames[NrSymbolNames++], Instance.SymbolName);
			Time1 = GetDifferenceTimer1inMilliSeconds();
#ifdef _DEBUG

			if (stricmp(Instance.SymbolName, "74hct14") == 0)
				ok = 1;

#endif

			if (stricmpUTF8(SheetName, Instance.SymbolName) == 0)
				LoadResult = SearchSymbol(Instance.SymbolName, LibName, &Pos, &Length, 1);
			else
				LoadResult = SearchSymbol(Instance.SymbolName, LibName, &Pos, &Length, 0);

#ifdef _DEBUG

			if (stricmp(FileName, "F:\\pcb_elegance35\\PcMotherBoard\\sch\\isa-control.sch") == 0)
			{
				ok = 1;
//        sprintf(str2,"%s (%d msec)\n",Instance.SymbolName,GetDifferenceTimer1inMilliSeconds()-Time1);
//        OutputDebugStr(str2);
			}

			if (stricmp(Instance.SymbolName, "led") == 0)
				ok = 1;

#endif

// 0 = Symbol from a library
// 1 = Symbol from global symbol directory
// 2 = Local symbol
// 3 = Local sheet symbol
			if (LoadResult >= 0)
			{
				TotalSymbolsMem += Length;

				if (LoadResult == 3)
				{
					if (NrSubSheets < 32)
					{
						strcpy(TempSheet[NrSubSheets].SheetName, Instance.SymbolName);
						NrSubSheets++;
					}
					else
						ok = 1;
				}
			}
			else
				NrErrors++;
		}
	}

	MemSize += 512 * sizeof(SymbolsPos2Record);
	FileClose(Designfp);
	*SheetMemSize = MemSize;
	*SymbolMemSize = TotalSymbolsMem;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetDesignSheets()
{
	int32 SheetMemSize, SymbolMemSize, SheetCnt, cnt, SheetLevel;
	char FileName[MAX_LENGTH_STRING], str[20];
	static char CurrentDesignFile[MAX_LENGTH_STRING] = "";
	static int32 CurrentNrSheets = 0;

	if (CurrentDesignFile[0] == 0)
	{
		strcpy(CurrentDesignFile, DesignFile);
		CurrentNrSheets = 0;
	}
	else
	{
		if (stricmpUTF8(CurrentDesignFile, DesignFile) != 0)
		{
			strcpy(CurrentDesignFile, DesignFile);
			CurrentNrSheets = 0;
		}
	}

	SheetCnt = 0;
	SheetLevel = 0;
	NrSheets = 0;
	strcpy(Sheets[SheetCnt].SheetName, TopSheetName);
	strcpy(FileName, Sheets[SheetCnt].SheetName);
	Sheets[SheetCnt].Info = 0;
	Sheets[SheetCnt].LinkToAbove = -1;
	Sheets[SheetCnt].Level = 0;
	sprintf(SheetDir, "%s\\sch\\", DesignPath);

	do
	{
		sprintf(FileName, "%s\\sch\\%s.sch", DesignPath, Sheets[SheetCnt].SheetName);
#ifdef _DEBUG

		if (SheetCnt == 8)
			ok = 1;

#endif

		if (GetMemSizeSheet(FileName, &SheetMemSize, &SymbolMemSize, 1) == 0)
		{
			Sheets[SheetCnt].Nr = NrSheets;

			if (SheetCnt == 0)
				NrSheets++;

			Sheets[SheetCnt].Info = 1;
			Sheets[SheetCnt].NrDownSheets = NrSubSheets;
			Sheets[SheetCnt].SheetMemSize = SheetMemSize;
			Sheets[SheetCnt].SymbolMemSize = SymbolMemSize;

			if (NrSubSheets > 0)
			{
				for (cnt = 0; cnt < NrSubSheets; cnt++)
				{
					if (NrSheets < MAX_NR_SHEETS)
					{
						strcpy(Sheets[NrSheets].SheetName, TempSheet[cnt].SheetName);
						Sheets[NrSheets].LinkToAbove = SheetCnt;
						Sheets[NrSheets].Level = Sheets[SheetCnt].Level + 1;
					}

					NrSheets++;
				}
			}
		}
		else
		{
			NrSheets = 0;
			return;
		}

		SheetCnt++;
	}
	while (SheetCnt < NrSheets);

	NrSheets = min(MAX_NR_SHEETS, NrSheets);

	if (CurrentNrSheets != NrSheets)
	{
		sprintf(str, "%d", NrSheets);
		ChangeUserVar("$NR_SHEETS", str, 0);
		CurrentNrSheets = NrSheets;
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetInstanceAttribute(char *InstanceAttrBuf, LPSTR AttributeIdent, LPSTR AttributeValue, int32 mode)
{
	int32 NrAttributes, LengthIdent, LengthValue, pos, pos2;
	InstanceRecord *Instance = NULL;
	NrAttributes = 0;

	if (Instance)
	{
	}

	pos = 0;

	while ((pos < sizeof(Instance->Properties) - 2) && (NrAttributes < 40) && (InstanceAttrBuf[pos] != 0))
	{
		LengthIdent = strlen((LPSTR) & InstanceAttrBuf[pos]);

		if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(Instance->Properties) - 1))
		{
			pos2 = pos + LengthIdent + 1;
			LengthValue = strlen((LPSTR) & InstanceAttrBuf[pos2]);

			if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(Instance->Properties) - 1))
			{
				if (stricmpUTF8((LPSTR) & InstanceAttrBuf[pos], AttributeIdent) == 0)
				{
					strcpy(AttributeValue, (LPSTR) & InstanceAttrBuf[pos2]);
					return 0;
				}
			}

			pos += LengthIdent + LengthValue + 2;
		}
		else
			pos = sizeof(Instance->Properties);
	}

	return -1;
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetProperties(LPSTR InstanceAttrBuf, LPSTR PropertyID, LPSTR PropertyValue, int32 mode)
{
	int32 cnt = 0, pos, pos2, LengthValue, LengthIdent;
	InstanceRecord *Instance = NULL;

	if (Instance)
	{
	};

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
				switch (mode & 0x60)
				{
				case 0:
					if (strcmpUTF8(PropertyID, &InstanceAttrBuf[pos]) == 0)
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

				if (cnt < 0x20)
					cnt++;
			}

			pos += LengthIdent + LengthValue + 2;
		}
		else
			break;
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
