/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edif.c
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
#include "graphics.h"
#include "string.h"
#include "select4.h"
#include "select2.h"
#include "trace2.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "calc.h"
#include "files2.h"
#include "gerber.h"
#include "gerber2.h"
#include "gerber3.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "select3.h"
#include "polygon.h"
#include "mainloop.h"
#include "toets.h"
#include "rect.h"
#include "math.h"
#include "dialogs.h"
#include "time.h"
#include "plot.h"
#include "owntime.h"

#ifndef GCC_COMP

#pragma warning( disable : 4101 )	// unreferenced variable

#endif

int32 NrPadStacks, *PadStackObjectStartNr, *PadStackObjectCount, NrPinObjects, ok;

ObjectRecord PinObjects[256];


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetPadStackNr(int32 mode)
{
	int32 cnt, cnt2, cnt3, Start, Found2;
	ObjectRecord *Object, *PinObject;

	for (cnt2 = 0; cnt2 < NrPinObjects; cnt2++)
		PinObjects[cnt2].Info = -1;

	for (cnt = 0; cnt < NrPadStacks; cnt++)
	{
		Start = PadStackObjectStartNr[cnt];

		for (cnt2 = 0; cnt2 < PadStackObjectCount[cnt]; cnt2++)
		{
			Object = &((*Objects)[Start + cnt2]);
			Object->Info = -1;
		}
	}

	for (cnt = 0; cnt < NrPadStacks; cnt++)
	{
		Start = PadStackObjectStartNr[cnt];

		for (cnt2 = 0; cnt2 < NrPinObjects; cnt2++)
		{
			PinObject = &PinObjects[cnt2];

			if (PinObject->Info >= 0)
			{
				continue;		// already found
			}

			for (cnt3 = 0; cnt3 < PadStackObjectCount[cnt]; cnt3++)
			{
				Object = &((*Objects2)[Start + cnt3]);

				if (PinObject->Info >= 0)
				{
					continue;	// already found
				}

				if (Object->ObjectType != PinObject->ObjectType)
					continue;

				if (Object->Layer != PinObject->Layer)
					continue;

				switch (Object->ObjectType)
				{
				case PIN_PUT_THROUGH_ROUND:	// Put through pin round
					if (InRange(Object->x2, PinObject->x2))
					{
						PinObject->Info = cnt3;
						Object->Info = cnt2;
					}

					break;

				case PIN_PUT_THROUGH_SQUARE:	// Put through pin square
					if (InRange(Object->x2, PinObject->x2))
					{
						PinObject->Info = cnt3;
						Object->Info = cnt2;
					}

					break;

				case PIN_SMD_RECT:	// SMD Pad rect
					if ((InRange(Object->x2, PinObject->x2)) && (InRange(Object->y2, PinObject->y2)))
					{
						PinObject->Info = cnt3;
						Object->Info = cnt2;
					}

					break;

				case PIN_SMD_ROUND:	// SMD Pad round
					if (InRange(Object->x2, PinObject->x2))
					{
						PinObject->Info = cnt3;
						Object->Info = cnt2;
					}

					break;

				case DRILL_UNPLATED:
					if (InRange(Object->x2, PinObject->x2))
					{
						PinObject->Info = cnt3;
						Object->Info = cnt2;
					}

					break;

				case DRILL:
					if (InRange(Object->x2, PinObject->x2))
					{
						PinObject->Info = cnt3;
						Object->Info = cnt2;
					}

					break;

				case PIN_LINE_HOR:
				case PIN_LINE_VER:
				case PIN_LINE_DIAG1:
				case PIN_LINE_DIAG2:
					break;
				}
			}
		}

		Found2 = 1;

		for (cnt2 = 0; cnt2 < NrPinObjects; cnt2++)
		{
			PinObject = &PinObjects[cnt2];

			if (PinObject->Info == -1)
			{
				Found2 = 0;
				break;
			}
		}

		if (Found2)
			return cnt;
	}

	if (NrPadStacks < 32768)
	{
#ifdef _DEBUG

		if (NrPadStacks == 6)
			ok = 1;

		if (NrPinObjects == 1)
		{
			ok = 1;

			if (PinObjects[0].Layer != -1)
				ok = 1;
		}

#endif

		if (NrObjects2 + NrPinObjects >= MaxNrObjects2)
			AllocateMemObjects2(MaxNrObjects2 + 4096);

		PadStackObjectStartNr[NrPadStacks] = NrObjects2;
		PadStackObjectCount[NrPadStacks] = NrPinObjects;
		memcpy(&((*Objects2)[NrObjects2]), PinObjects, NrPinObjects * sizeof(ObjectRecord));
		NrObjects2 += NrPinObjects;
		NrPadStacks++;
		return NrPadStacks - 1;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ExportEdif(int32 mode)
{
	int32 Layer, ok, cnt, cnt2, cnt3, cnt4, Mirror, Start, pos, NrShapes, count, fp, PinOffset, NewPin, MemPosComp,
	      First, ShapeNr, MemPos, PinNr, NrPins, PinsMemPos, ShapePos, NrLayers, Found, PadCount, NetNr, NrPinShapes,
	      *ShapeNrUsed, NrShapesUsed, LengthLine, TempUnits, res, ObjectInclude, ShapeType, Count, ShapeInfo,
	      PowerPlaneLayer, UseMacros, WriteThermalReliefs, OkToPlotObject;
	double x1, y1, Rotation, x = 0.0, y = 0.0, Width, Height;

	ObjectRecord *Object, *Object4, *Object4a;
	//ObjectLineRecord *ObjectLine;
	//ObjectPolygonRecord *ObjectPolygon;
	CompRecord *Comp, *NewComp;
	ShapeRecord *Shape;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	CompPinRecord *CompPin;
	NetRecord *Net;
	NetItemsRecord *NetItem;
	AreaFillRecord *AreaFill, *BoardOutlineAreaFill;
	//AperTureRecord *AperTure;
	PolygonRecord *Polygon;
	//GeomPolygonRecord *GeomPolygon;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], DesignName[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING],
	     str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING], strx1[64], stry1[64], strx2[64],
	     strx3[64], stry2[64];

	strcpy(str, EditFile);
	CutExtensionFileName(str);
	strcpy(FileStr, str);
	GetFilePartFromFileName(DesignName, str);
	strcat(FileStr, ".dsn");

	//nelze vytvoøit soubor **********************************************************************************
	if ((fp = FileOpenWriteUTF8(FileStr)) <= 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -2;
	}
	//********************************************************************************************************

	/*
	(pcb CAD.dsn
	  (resolution MM 1000000)
	  (structure
	    (boundary (rect pcb 0.000000 0.000000 50.800000 38.100000))
	    (rule (width 0.317500) (clearance 0.317500))
	    (via Via1)
	    (layer L1 (type signal))
	    (layer L2 (type signal))
	  )
	*/

	AllocateMemTemp(256 * 1024);
	AllocateSpecialMem(MEM_POINTS, 256 * 1024, (void **) &NewComp);
	AllocateMemObjects2(4096);
	AllocateSpecialMem(MEM_POLYGON_CHECK1, 128 * 1024, (void **) &PadStackObjectStartNr);
	AllocateSpecialMem(MEM_POLYGON_CHECK2, 128 * 1024, (void **) &PadStackObjectCount);

	BoardOutlineAreaFill = (AreaFillRecord *) TempMem;
	res = GetBoardOutlineAreaFill(BoardOutlineAreaFill, 0.0, 1);

	sprintf(str, "(pcb %s.dsn", DesignName);
	WriteLn(fp, str);
	TempUnits = Units;

	if (Units == 0)
		WriteLn(fp, "  (resolution mil 10000)");
	else
		WriteLn(fp, "  (resolution mm 1000000)");

	WriteLn(fp, "  (structure");
	GetUnitsValue(TempUnits, BoardOutlineAreaFill->minx, strx1, 1);
	GetUnitsValue(TempUnits, BoardOutlineAreaFill->miny, stry1, 1);
	GetUnitsValue(TempUnits, BoardOutlineAreaFill->maxx, strx2, 1);
	GetUnitsValue(TempUnits, BoardOutlineAreaFill->maxy, stry2, 1);
	sprintf(str, "    (boundary (rect pcb %s %s %s %s))", strx1, stry1, strx2, stry2);
	WriteLn(fp, str);
	GetUnitsValue(TempUnits, Design.StandardTraceWidth, strx1, 1);
	GetUnitsValue(TempUnits, Design.StandardClearance, stry1, 1);
	sprintf(str, "    (rule (width %s) (clearance %s))", strx1, stry1);
	WriteLn(fp, str);
	WriteLn(fp, "    (via Via1)");

	for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
	{
		sprintf(str, "    (layer L%d (type signal))", cnt + 1);
		WriteLn(fp, str);
	}

	WriteLn(fp, "  )");
	//********************************************************************************************************
	//********************************************************************************************************
	WriteLn(fp, "  (placement");
	NrShapesUsed = 0;
	ShapeNrUsed = (int32 *) ((int32) NewComp + 128 * 1024);

	for (cnt2 = 0; cnt2 < 32768; cnt2++)
		ShapeNrUsed[cnt2] = -1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ShapeNr = (int32) Comp->ShapeNr;

			for (cnt2 = 0; cnt2 < NrShapesUsed; cnt2++)
			{
				if (ShapeNrUsed[cnt2] == ShapeNr)
					break;
			}

			if (cnt2 == NrShapesUsed)
			{
				if (NrShapesUsed < 32768)
					ShapeNrUsed[NrShapesUsed++] = ShapeNr;
			}
		}
	}

	ok = 1;

	for (cnt2 = 0; cnt2 < NrShapesUsed; cnt2++)
	{
		First = 1;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ShapeNr = (int32) Comp->ShapeNr;

				if (ShapeNr != ShapeNrUsed[cnt2])
					continue;

				MemPos = (*Shapes)[ShapeNr].ShapePos;
				Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

				if (First)
				{
					sprintf(str, "    (component %-12s", Shape->ShapeName);
					WriteLn(fp, str);
				}

				First = 0;
				Mirror = ((Comp->CompMode & 8) >> 3);
				Rotation = Comp->Rotation;
				
				//(component J1 (place J1 36.830000 20.320000 front 0))
				
				GetUnitsValue(TempUnits, Comp->CompOriginX, strx1, 1);
				GetUnitsValue(TempUnits, Comp->CompOriginY, stry1, 1);
				sprintf(str, "      (place %-12s %s %s ", Comp->Name, strx1, stry1);

				if (Mirror == 0)
					strcat(str, "front");
				else
					strcat(str, "back");

				if (InRange(Rotation, 0.0))
					sprintf(str2, " 0)");
				else if (InRange(Rotation, 90.0))
					sprintf(str2, " 90)");
				else if (InRange(Rotation, 180.0))
					sprintf(str2, " 180)");
				else if (InRange(Rotation, 270.0))
					sprintf(str2, " 270)");
				else if (InRange(Rotation, 360.0))
					sprintf(str2, " 0)");
				else
					sprintf(str2, " %.2f)", Rotation);

				strcat(str, str2);
				WriteLn(fp, str);
			}
		}

		WriteLn(fp, "    )");
	}

	WriteLn(fp, "  )");
	//********************************************************************************************************
	//********************************************************************************************************
	WriteLn(fp, "  (library");
	PadCount = 0;
	NrObjects2 = 0;

	for (cnt4 = 0; cnt4 < NrShapesUsed; cnt4++)
	{
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ShapeNr = (int32) Comp->ShapeNr;

				if (ShapeNr != ShapeNrUsed[cnt4])
					continue;

				MemPos = (*Shapes)[ShapeNr].ShapePos;
				Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
				memcpy(NewComp, Comp, MemSizeComp(Comp));
				NewComp->CompOriginX = 0.0;
				NewComp->CompOriginY = 0.0;
				NewComp->CompMode = 0;
				NewComp->Rotation = 0.0;
				NrObjects = 0;
				ShapePinsToObject(NewComp, 0.0, 0.0, 0, 0, 0, 0);

				/*
				      image J1
				      (pin Pad1 1 8.890000 0.000000)
				      (pin Pad2 2 6.350000 0.000000)
				*/

				sprintf(str, "    (image %s", Shape->ShapeName);
				WriteLn(fp, str);

				PinNr = -1;
				NewPin = 0;
				NrPinObjects = 0;

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (PinNr != Object->PinNr)
					{
						if (NrPinObjects > 0)
						{
							res = GetPadStackNr(0);
							GetUnitsValue(TempUnits, x, strx1, 1);
							GetUnitsValue(TempUnits, y, stry1, 1);
							sprintf(str, "      (pin Pad%d %s %s %s)", res + 1, str2, strx1, stry1);
							WriteLn(fp, str);
						}

						PinNr = Object->PinNr;
						str2[0] = 0;
						NewPin = 1;
						NrPinObjects = 0;
						CompPinText(Comp, Object->PinNr, 0.0, 0.0, str2);
						x = Object->x1;
						y = Object->y1;
					}
					else
					{
					}

					ObjectInclude = 0;

					switch (Object->ObjectType)
					{
					case PIN_PUT_THROUGH_ROUND:	// Put through pin round
						ObjectInclude = 1;
						break;

					case PIN_PUT_THROUGH_SQUARE:	// Put through pin square
						ObjectInclude = 1;
						break;

					case PIN_SMD_RECT:	// SMD Pad rect
						ObjectInclude = 1;
						break;

					case PIN_SMD_ROUND:	// SMD Pad round
						ObjectInclude = 1;
						break;

					case PIN_LINE_HOR:
					case PIN_LINE_VER:
					case PIN_LINE_DIAG1:
					case PIN_LINE_DIAG2:
					case DRILL_UNPLATED:
					case DRILL:
						break;
					}

					if ((ObjectInclude) && (NrPinObjects < 256))
					{
						memcpy(&PinObjects[NrPinObjects], Object, sizeof(ObjectRecord));
						NrPinObjects++;
					}

					NewPin = 0;
				}

				if (NrPinObjects > 0)
				{
					res = GetPadStackNr(0);
					GetUnitsValue(TempUnits, x, strx1, 1);
					GetUnitsValue(TempUnits, y, stry1, 1);
					sprintf(str, "      (pin Pad%d %s %s %s)", res + 1, str2, strx1, stry1);
					WriteLn(fp, str);
				}

				break;
			}
		}

		WriteLn(fp, "    )");
	}
	
	//********************************************************************************************************
	//********************************************************************************************************
	/*
	    (padstack Via0 (shape (circle L1 1.270000)))
	    (padstack Pad1
	      (shape (rect L1 -0.736600 -0.736600 0.736600 0.736600))
	      (shape (rect L2 -0.736600 -0.736600 0.736600 0.736600))
	    )
	    (padstack Pad2
	      (shape (circle L1 1.473200 0 0))
	      (shape (circle L2 1.473200 0 0))    )
	*/

	GetUnitsValue(TempUnits, Design.DefVia1.ThickNess, strx1, 1);
	sprintf(str, "    (padstack Via0 (shape (circle L1 %s))", strx1);
	WriteLn(fp, str);
	WriteLn(fp, "    (padstack Via1");

	for (Layer = 0; Layer < Design.NrBoardLayers; Layer++)
	{
		sprintf(str, "      (shape (circle L%d %s 0 0))", Layer + 1, strx1);
		WriteLn(fp, str);
	}

	WriteLn(fp, "    )");
	ok = 1;

	for (cnt = 0; cnt < NrPadStacks; cnt++)
	{
		Start = PadStackObjectStartNr[cnt];
		sprintf(str, "    (padstack Pad%d", cnt + 1);
		WriteLn(fp, str);

		for (cnt2 = 0; cnt2 < PadStackObjectCount[cnt]; cnt2++)
		{
			Object = &((*Objects2)[Start + cnt2]);
#ifdef _DEBUG

			if (cnt == 3)
				ok = 1;

#endif
			Object->Info = -1;

			if (Object->Layer != -1)
				Layer = Design.NrBoardLayers - Object->Layer - 1;

			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:	// Put through pin round
				GetUnitsValue(TempUnits, Object->x2, strx2, 1);

				for (Layer = 0; Layer < Design.NrBoardLayers; Layer++)
				{
					sprintf(str, "      (shape (circle L%d %s 0 0))", Layer + 1, strx2);
					WriteLn(fp, str);
				}

				break;

			case PIN_PUT_THROUGH_SQUARE:	// Put through pin square
				GetUnitsValue(TempUnits, Object->x2 * -0.5, strx1, 1);
				GetUnitsValue(TempUnits, Object->y2 * -0.5, stry1, 1);
				GetUnitsValue(TempUnits, Object->x2 * 0.5, strx2, 1);
				GetUnitsValue(TempUnits, Object->y2 * 0.5, stry2, 1);
				GetUnitsValue(TempUnits, Object->x2, strx3, 1);

				for (Layer = 0; Layer < Design.NrBoardLayers; Layer++)
				{
					if ((Layer > 0) && (Layer < Design.NrBoardLayers - 1))
						sprintf(str, "      (shape (circle L%d %s 0 0))", Layer + 1, strx3);
					else
						sprintf(str, "      (shape (rect L%d %s %s %s %s))", Layer + 1, strx1, stry1, strx2, stry2);

					WriteLn(fp, str);
				}

				break;

			case PIN_SMD_RECT:	// SMD Pad rect
				GetUnitsValue(TempUnits, Object->x2 * -0.5, strx1, 1);
				GetUnitsValue(TempUnits, Object->y2 * -0.5, stry1, 1);
				GetUnitsValue(TempUnits, Object->x2 * 0.5, strx2, 1);
				GetUnitsValue(TempUnits, Object->y2 * 0.5, stry2, 1);
				sprintf(str, "      (shape (rect L%d %s %s %s %s))", Layer + 1, strx1, stry1, strx2, stry2);
				WriteLn(fp, str);
				break;

			case PIN_SMD_ROUND:	// SMD Pad round
				GetUnitsValue(TempUnits, Object->x2, strx2, 1);
				sprintf(str, "      (shape (circle L%d %s 0 0))", Layer + 1, strx2);
				WriteLn(fp, str);
				break;

			case DRILL_UNPLATED:
				GetUnitsValue(TempUnits, Object->x2, strx2, 1);
				break;

			case DRILL:
				GetUnitsValue(TempUnits, Object->x2, strx2, 1);
				break;

				/*
				(outline (polygon signal_1 0.0 0.0550 0.0000 0.4100
				0.0000 0.4650 0.0550 0.4650 0.4250 0.4250 0.4650
				0.0550 0.4650 0.0000 0.4100 0.0000 0.0550 0.0550
				0.0000))
				(outline (path signal_1 0.5      0.0 0.0550 0.0000 0.4100))
				*/
			}
		}

		WriteLn(fp, "    )");
	}

	WriteLn(fp, "  )");
	//********************************************************************************************************
	//********************************************************************************************************
	/*
	  (network
	    (net X_1
	      (pins
	        J1-1
	        U4-5
	        U2-12
	        U3-9
	      )
	    )
	*/
	WriteLn(fp, "  (network");

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

		if (cnt == 161)
			ok = 1;

#endif

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
			PinsMemPos = MemPosComp + sizeof(CompRecord);
			ShapeNr = (int32) Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			ShapePos = MemPos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

			if (ShapeNr < 0)
				ok = 1;

			if (stricmpOwn(Comp->Name, "L1") == 0)
				ok = 1;

#endif

			/*
			if (stricmpOwn(Shape->ShapeName,"pci_slot")==0)
			{
				ShapeNr=cnt3;
				MemPos=(*Shapes)[ShapeNr].ShapePos;
				Shape=(ShapeRecord *)&(ShapesMem[MemPos]);
			}
			*/

			PinOffset = Shape->PinOffset;
			NrPins = Shape->NrPins;
			MemPos += PinOffset;
			PinNr = 0;
			Found = 0;

			while (NrPins > 0)
			{
				ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
				NrPinShapes = ShapePad->NrPinShapes;
				MemPos += sizeof(ShapePadRecord);
				CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				NetNr = CompPin->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);
					Net->Count++;
				}
				else
				{
					if (NetNr != -1)
						CompPin->NetNr = -1;
				}
				
				//PinTextFound=(ShapePad->Name);

				for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
				{
					Pad = (PadRecord *) & (ShapesMem[MemPos]);
					ShapeType = Pad->ShapeType;

					if (ShapeType != PIN_ARC)
						MemPos += sizeof(PadRecord);
					else
						MemPos += 48;
				}

				PinsMemPos += sizeof(CompPinRecord);
				NrPins--;
				//PinNr++;
			}
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	AllocateMemNetItems(Count);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
			PinsMemPos = MemPosComp + sizeof(CompRecord);
			ShapeNr = (int32) Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

			/*
			if (stricmpOwn(Shape->ShapeName,"pci_slot")==0)
			{
			ShapeNr=cnt3;
			MemPos=(*Shapes)[ShapeNr].ShapePos;
			Shape=(ShapeRecord *)&(ShapesMem[MemPos]);
			}
			*/

			PinOffset = Shape->PinOffset;
			ShapeInfo = Shape->Info;
			NrPins = Shape->NrPins;
			MemPos += PinOffset;
			ShapePos = MemPos;
			PinNr = 0;
			Found = 0;

			while (NrPins > 0)
			{
				ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
				NrPinShapes = ShapePad->NrPinShapes;
				MemPos += sizeof(ShapePadRecord);
				CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				NetNr = CompPin->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);
					NetItem = &((*NetItems)[Net->Pos]);
					sprintf(NetItem->PinStr, "%s-%s", Comp->Name, (ShapePad->Name));
					NetItem->CompNr = cnt;
					NetItem->Info = 0;
					Net->Pos++;
				}

				for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
				{
					Pad = (PadRecord *) & (ShapesMem[MemPos]);
					ShapeType = Pad->ShapeType;

					if (ShapeType != PIN_ARC)
						MemPos += sizeof(PadRecord);
					else
						MemPos += 48;
				}

				PinsMemPos += sizeof(CompPinRecord);
				NrPins--;
				PinNr++;
			}
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if ((Net->Name[0] != 0) && (Net->Count > 0))
		{
			sprintf(str, "    (net %s", Net->Name);
			WriteLn(fp, str);
			strcpy(str, "      (pins");
			pos = Net->Pos;
			LengthLine = 100;
			First = 1;

			for (cnt2 = 0; cnt2 < Net->Count; cnt2++)
			{
				NetItem = &((*NetItems)[pos + cnt2]);
				strcat(str, " ");
				strcat(str, NetItem->PinStr);
				LengthLine = strlen(str);

				if (LengthLine > 75)
				{
					WriteLn(fp, str);
					strcpy(str, "           ");
				}
			}

			strcat(str, ")");
			WriteLn(fp, str);
			WriteLn(fp, "    )");
		}
	}

	WriteLn(fp, "  )");
	
	//********************************************************************************************************
	//********************************************************************************************************

	WriteLn(fp, ")");
	FileClose(fp);
	//DeAllocateMemObjects2();
	DeallocateSpecialMem(MEM_POINTS);
	DeallocateSpecialMem(MEM_POLYGON_CHECK1);
	DeallocateSpecialMem(MEM_POLYGON_CHECK2);
	DeAllocateMemNetItems();
	DeAllocateMemTemp();
	sprintf(str, "SPECCTRA file %s is created", FileStr);
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
	return 0;
}

//********************************************************************************************************
//********************************************************************************************************
//********************************************************************************************************
//********************************************************************************************************
