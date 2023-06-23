/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: insdel.c
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
#include "insdel.h"
#include "calc.h"
#include "calc3.h"
#include "dialogs.h"
#include "stdio.h"
#include "files2.h"
#include "calc4.h"
#include "graphics.h"
#include "draw.h"
#include "draw2.h"
#include "calcdef.h"
#include "pcb.h"
#include "nets.h"
#include "menus.h"
#include "toets.h"
#include "polygon.h"
#include "movecomp.h"
#include "mainloop.h"
#include "resource.h"
#include "math.h"


typedef int16 int16Array[];

extern HDC OutputDisplay;
extern int32 AreafillDrawMode;

ViaRecord *NewestVia = NULL;
ViaRecord *LastVia = NULL;


// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 ZeroUnusedObjects(int32 mode)
{
	int32 cnt, Layer, count;
	TraceRecord *Trace;
	ViaRecord *Via;
	CompRecord *Comp;
	AreaFillRecord *AreaFill;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	count = 0;

	if (MaxLastActionNr > LastActionNr)
	{
		for (Layer = 0; Layer < 32; Layer++)
		{
			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);

				if (Trace->AddNr >= LastActionNr)
					Trace->AddNr = 0;

				if (Trace->DeleteNr >= LastActionNr)
					Trace->DeleteNr = 0;
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);

				if (Trace->AddNr >= LastActionNr)
					Trace->AddNr = 0;

				if (Trace->DeleteNr >= LastActionNr)
					Trace->DeleteNr = 0;
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);

				if (Trace->AddNr >= LastActionNr)
					Trace->AddNr = 0;

				if (Trace->DeleteNr >= LastActionNr)
					Trace->DeleteNr = 0;
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);

				if (Trace->AddNr >= LastActionNr)
					Trace->AddNr = 0;

				if (Trace->DeleteNr >= LastActionNr)
					Trace->DeleteNr = 0;
			}
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (Comp->AddNr >= LastActionNr)
				Comp->AddNr = 0;

			if (Comp->DeleteNr >= LastActionNr)
				Comp->DeleteNr = 0;
		}

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if (Via->AddNr >= LastActionNr)
				Via->AddNr = 0;

			if (Via->DeleteNr >= LastActionNr)
				Via->DeleteNr = 0;
		}

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (AreaFill->AddNr >= LastActionNr)
				AreaFill->AddNr = 0;

			if (AreaFill->DeleteNr >= LastActionNr)
				AreaFill->DeleteNr = 0;
		}

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (ObjectLine->AddNr >= LastActionNr)
				ObjectLine->AddNr = 0;

			if (ObjectLine->DeleteNr >= LastActionNr)
				ObjectLine->DeleteNr = 0;
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if (ObjectRect->AddNr >= LastActionNr)
				ObjectRect->AddNr = 0;

			if (ObjectRect->DeleteNr >= LastActionNr)
				ObjectRect->DeleteNr = 0;
		}

		/*
		    for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
		      ObjectCircle=&((*ObjectCircles)[cnt]);
		      if (ObjectCircle->AddNr>=LastActionNr) {
		        ObjectCircle->AddNr=0;
		      }
		      if (ObjectCircle->DeleteNr>=LastActionNr) {
		        ObjectCircle->DeleteNr=0;
		      }
		    }
		*/
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (ObjectArc->AddNr >= LastActionNr)
				ObjectArc->AddNr = 0;

			if (ObjectArc->DeleteNr >= LastActionNr)
				ObjectArc->DeleteNr = 0;
		}

		/*
		    for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
		      ObjectText=&((*ObjectTexts)[cnt]);
		      if (ObjectText->AddNr>=LastActionNr) {
		        ObjectText->AddNr=0;
		      }
		      if (ObjectText->DeleteNr>=LastActionNr) {
		        ObjectText->DeleteNr=0;
		      }
		    }
		*/
		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if (ObjectText2->AddNr >= LastActionNr)
				ObjectText2->AddNr = 0;

			if (ObjectText2->DeleteNr >= LastActionNr)
				ObjectText2->DeleteNr = 0;
		}

		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (ObjectPolygon->AddNr >= LastActionNr)
				ObjectPolygon->AddNr = 0;

			if (ObjectPolygon->DeleteNr >= LastActionNr)
				ObjectPolygon->DeleteNr = 0;
		}
	}

	MaxLastActionNr = LastActionNr;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SelectNetsObjectsSelected(uint8 * SelectedNets, int32 NrNets)
{
	int32 Layer, TraceInfo, ViaInfo, cnt, NrTracesSelected, NrViasSelected;
	TraceRecord *Trace;
	ViaRecord *Via;

	memset(SelectedNets, 0, NrNets);

	NrTracesSelected = 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (Trace->NetNr < Design.NrNets)
					SelectedNets[Trace->NetNr] = 1;

				NrTracesSelected++;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (Trace->NetNr < Design.NrNets)
					SelectedNets[Trace->NetNr] = 1;

				NrTracesSelected++;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (Trace->NetNr < Design.NrNets)
					SelectedNets[Trace->NetNr] = 1;

				NrTracesSelected++;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (Trace->NetNr < Design.NrNets)
					SelectedNets[Trace->NetNr] = 1;

				NrTracesSelected++;
			}
		}
	}

	NrViasSelected = 0;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (Via->NetNr < Design.NrNets)
				SelectedNets[Via->NetNr] = 1;

			NrViasSelected++;
		}
	}

	return NrTracesSelected + NrViasSelected;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteObjectsSelected()
{
	int32 Layer, TraceInfo, ViaInfo;
	int32 cnt, cnt2, NrTracesDeleted, NrViasDeleted, AreaFillCount, AreaFillChanged;
	TraceRecord *Trace;
	ViaRecord *Via;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon, *FirstPolygon;
	uint8 *AreaPos, *PolygonPos, *SelectedNets;

	AllocateSpecialMem(MEM_POINTS, (Design.NrNets + 10) * sizeof(uint8), (void **) &SelectedNets);

	FirstPolygon = NULL;
	memset(SelectedNets, 0, Design.NrNets);
	NrTracesDeleted = 0;
	AreaFillChanged = 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Trace->Info &= ~OBJECT_SELECTED;

				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
					SelectedNets[Trace->NetNr] = 1;

				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				NrTracesDeleted++;
				DataBaseChanged = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Trace->Info &= ~OBJECT_SELECTED;

				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
					SelectedNets[Trace->NetNr] = 1;

				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				NrTracesDeleted++;
				DataBaseChanged = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Trace->Info &= ~OBJECT_SELECTED;

				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
					SelectedNets[Trace->NetNr] = 1;

				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				NrTracesDeleted++;
				DataBaseChanged = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Trace->Info &= ~OBJECT_SELECTED;

				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
					SelectedNets[Trace->NetNr] = 1;

				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				NrTracesDeleted++;
				DataBaseChanged = 1;
			}
		}
	}

	NrViasDeleted = 0;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			Via->Info |= OBJECT_NOT_VISIBLE;
			Via->DeleteNr = (int16) LastActionNr;

			if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
				SelectedNets[Via->NetNr] = 1;

			NrViasDeleted++;
			DataBaseChanged = 1;
		}
	}

	ViasSelected = 0;
	AreaFillCount = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
		AreaFillChanged = 0;
		AreaPos = (uint8 *) AreaFill;

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((AreaFill->Info & (OBJECT_NOT_VISIBLE|POWERPLANE)) == POWERPLANE) {
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				if (cnt2 == 0)
					FirstPolygon = DrawPolygon;

				PolygonPos = (uint8 *) DrawPolygon;

//        if ((cnt2>0)
//           &&
				if ((DrawPolygon->PolygonType & 2) == 2)
					AreaFillChanged = 1;

				PolygonPos += MemSizePolygon(DrawPolygon);
				DrawPolygon = (PolygonRecord *) PolygonPos;
			}

			if (AreaFillChanged)
			{
				if ((FirstPolygon->PolygonType & 2) == 2)
				{
					if ((AreaFill->Info & (POWERPLANE)) == 0)
					{
						AreaPos = (uint8 *) AreaFill;
						DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

						for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
						{
							PolygonPos = (uint8 *) DrawPolygon;
							DrawPolygon->PolygonType &= ~2;
							PolygonPos += MemSizePolygon(DrawPolygon);
							DrawPolygon = (PolygonRecord *) PolygonPos;
						}

						ZeroUnusedObjects(0);
						AreaFill->Info |= OBJECT_NOT_VISIBLE;
						AreaFill->DeleteNr = (int16) LastActionNr;

						if ((AreaFill->NetNr >= 0) && (AreaFill->NetNr < Design.NrNets))
							SelectedNets[AreaFill->NetNr] = 1;

						DataBaseChanged = 1;
						AreaFillCount++;
					}
				}
				else
				{
					RemoveDeletionsAreaFill(cnt, 0);
                    //StartDrawingEditingWindow(0);
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

					if ((AreaFill->NetNr >= 0) && (AreaFill->NetNr < Design.NrNets))
						SelectedNets[AreaFill->NetNr] = 1;

					DataBaseChanged = 1;
				}
			}
		}
	}

	if (AreaFillCount > 1)
		MessageBoxOwn(PCBWindow, SC(498, "More then one areafill deleted"), SC(118, "Warning"), MB_APPLMODAL | MB_OK);

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectLine->Info &= ~OBJECT_SELECTED;
			ObjectLine->Info |= OBJECT_NOT_VISIBLE;
			ObjectLine->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectRect->Info &= ~OBJECT_SELECTED;
			ObjectRect->Info |= OBJECT_NOT_VISIBLE;
			ObjectRect->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      ObjectCircle->Info&=~OBJECT_SELECTED;
	      SetBackGroundActive(0);
	      DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	      ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	      ObjectCircle->DeleteNr=LastActionNr;
	      DataBaseChanged=1;
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectArc->Info &= ~OBJECT_SELECTED;
			ObjectArc->Info |= OBJECT_NOT_VISIBLE;
			ObjectArc->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if ((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      ObjectText->Info&=~OBJECT_SELECTED;
	      SetBackGroundActive(0);
	      DrawObjectText(ObjectText,0.0,0.0,0);
	      ObjectText->Info|=OBJECT_NOT_VISIBLE;
	      ObjectText->DeleteNr=LastActionNr;
	      DataBaseChanged=1;
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectText2->Info &= ~OBJECT_SELECTED;
			ObjectText2->Info |= OBJECT_NOT_VISIBLE;
			ObjectText2->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
			ObjectPolygon->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (SelectedNets[cnt] == 1)
		{
			ReCalcConnectionsNet((int32) cnt, 0, 1);
			CheckForEscape();
		}
	}


	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];
		VerTracesSelected[Layer] = 0;
		HorTracesSelected[Layer] = 0;
		Diag1TracesSelected[Layer] = 0;
		Diag2TracesSelected[Layer] = 0;
		TracesSelected[Layer] = 0;
	}

	ViasSelected = 0;
	ConnectionsSelected = 0;
	AllTracesSelected = 0;
	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DeletePolygonsInAreaFillPowerPlane(AreaFillRecord * AreaFill)
{
	int32 cnt, cnt2, Found, AreaFillLength, *AreaFillPos;
	double cx, cy;
	PolygonRecord *Polygon, *NewAreaFillPolygon;
	uint8 *AreaPos, PolygonMap[1000];
	uint8 *AreaFillP, *PolygonPos, *NewAreaPos, *NewAreaFillPolygonPos;
	AreaFillRecord *NewAreaFill2;

	cx = (SearchMinX + SearchMaxX) * 0.5;
	cy = (SearchMinY + SearchMaxY) * 0.5;
	Found = 0;
	memset(&PolygonMap, 0, 1000);
	AreaPos = (uint8 *) AreaFill;
	Polygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) Polygon;

	for (cnt = 0; cnt < AreaFill->NrPolygons; cnt++)
	{
		if (cnt > 0)
		{
			if (((PointInPolygon(Polygon, cx, cy) & 1) == 1) && (Found < 1000))
				PolygonMap[Found++] = 1;
		}

		PolygonPos += MemSizePolygon(Polygon);
		Polygon = (PolygonRecord *) PolygonPos;
	}

	if (Found == 0)
		return 0;

// ***************************************************************************

	if (Design.NrAreaFills >= MaxNrAreaFills)
	{
		if (AllocateMemAreaFills(MaxNrAreaFills + 16) != 0)
			return -1;
	}

	AreaFillLength = NewAreaFill->MemSize;

	if (Design.AreaFillMem + AreaFillLength >= MaxAreaFillMemory)
	{
		if (AllocateMemAreaFillMemory(Design.AreaFillMem + AreaFillLength + 32768) != 0)
			return 0;
	}

	AreaFillPos = &((*AreaFills)[Design.NrAreaFills]);
	AreaFillP = &(AreaFillMem[Design.AreaFillMem]);
	memmove(AreaFillP, NewAreaFill, sizeof(AreaFillRecord));
	NewAreaPos = AreaFillP + sizeof(AreaFillRecord);
	NewAreaFill2 = (AreaFillRecord *) AreaFillP;
	*AreaFillPos = Design.AreaFillMem;

	AreaFillLength = sizeof(AreaFillRecord);
	PolygonPos = (uint8 *) NewAreaFill;
	PolygonPos += sizeof(AreaFillRecord);
	NewAreaFillPolygonPos = AreaFillP + sizeof(AreaFillRecord);

	for (cnt2 = 0; cnt2 < NewAreaFill->NrPolygons; cnt2++)
	{
		Polygon = (PolygonRecord *) PolygonPos;
		NewAreaFillPolygon = (PolygonRecord *) NewAreaFillPolygonPos;
		CopyPolygonToPolygon(Polygon, NewAreaFillPolygon);
		AreaFillLength += MemSizePolygon(Polygon);
		NewAreaFillPolygonPos += MemSizePolygon(Polygon);
		PolygonPos += MemSizePolygon(Polygon);
	}

	NewAreaFill2->MemSize = AreaFillLength;
	NewAreaFill2->AddNr = (int16) LastActionNr;
	Design.NrAreaFills++;
	Design.AreaFillMem += AreaFillLength;
	DataBaseChanged = 1;
	NewAreaFill->Info |= CONNECTIONS_NOT_VISIBLE;
	NewAreaFill->DeleteNr = (int16) LastActionNr;

	return 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddTrace(ObjectRecord * InsertTrace)
{
	int32 Layer, NetNr;
	TraceRecord *Trace;
	NetRecord *Net;

	Net = NULL;
	Layer = InsertTrace->Layer;
	NetNr = InsertTrace->NetNr;

	if ((NetNr >= 0) && (NetNr < Design.NrNets))
		Net = &((*Nets)[NetNr]);

	if ((Layer >= 0) && (Layer < Design.NrBoardLayers))
	{
//    Object3=&((*Objects2)[NrObjects2]);
//    memmove(Object3,InsertTrace,sizeof(ObjectRecord));
//    NrObjects2++;
		switch (InsertTrace->ObjectType)
		{
		case TRACE_HOR:
			if (!MakeRoomHorTraces(Layer))
				return 0;

			Trace = &((*HorTraces[Layer])[Design.NrHorTraces[Layer]]);
			Trace->X = (float) InsertTrace->x1;
			Trace->Y = (float) InsertTrace->y1;
			Trace->Length = (float) fabs(InsertTrace->x2);
			Trace->ThickNess = (float) InsertTrace->y2;
			Trace->NetNr = (int16) InsertTrace->NetNr;
			Trace->Info = 0;

			if ((NetNr >= 0) && (NetNr < Design.NrNets))
				Trace->Info = (int16) (Net->Info & OBJECT_HIGHLITED);

			Trace->Layer = Layer;
			Trace->Clearance = (float) InsertTrace->Clearance;
			Trace->AddNr = (int16) LastActionNr;
			Trace->DeleteNr = 0;
			InsertTrace->TraceNr = Design.NrHorTraces[Layer];
			ZeroUnusedObjects(0);
			Design.NrHorTraces[Layer]++;
			DataBaseChanged = 1;
//        Object3=&((*Objects3)[NrObjects3]);
//        memmove(Object3,InsertTrace,sizeof(ObjectRecord));
//        NrObjects3++;
			return 1;
			break;

		case TRACE_VER:
			if (!MakeRoomVerTraces(Layer))
				return 0;

			Trace = &((*VerTraces[Layer])[Design.NrVerTraces[Layer]]);
			Trace->X = (float) InsertTrace->x1;
			Trace->Y = (float) InsertTrace->y1;
			Trace->Length = (float) fabs(InsertTrace->x2);
			Trace->ThickNess = (float) InsertTrace->y2;
			Trace->NetNr = (int16) InsertTrace->NetNr;
			Trace->Info = 0;

			if ((NetNr >= 0) && (NetNr < Design.NrNets))
				Trace->Info = (int16) (Net->Info & OBJECT_HIGHLITED);

			Trace->Layer = Layer;
			Trace->Clearance = (float) InsertTrace->Clearance;
			Trace->AddNr = (int16) LastActionNr;
			Trace->DeleteNr = 0;
			InsertTrace->TraceNr = Design.NrVerTraces[Layer];
			ZeroUnusedObjects(0);
			Design.NrVerTraces[Layer]++;
			DataBaseChanged = 1;
//        Object3=&((*Objects3)[NrObjects3]);
//        memmove(Object3,InsertTrace,sizeof(ObjectRecord));
//        NrObjects3++;
			return 1;
			break;

		case TRACE_DIAG1:
			if (!MakeRoomDiag1Traces(Layer))
				return 0;

			Trace = &((*Diag1Traces[Layer])[Design.NrDiag1Traces[Layer]]);
			Trace->X = (float) InsertTrace->x1;
			Trace->Y = (float) InsertTrace->y1;
			Trace->Length = (float) fabs(InsertTrace->x2);
			Trace->ThickNess = (float) InsertTrace->y2;
			Trace->NetNr = (int16) InsertTrace->NetNr;
			Trace->Info = 0;

			if ((NetNr >= 0) && (NetNr < Design.NrNets))
				Trace->Info = (int16) (Net->Info & OBJECT_HIGHLITED);

			Trace->Layer = Layer;
			Trace->Clearance = (float) InsertTrace->Clearance;
			Trace->AddNr = (int16) LastActionNr;
			Trace->DeleteNr = 0;
			InsertTrace->TraceNr = Design.NrDiag1Traces[Layer];
			ZeroUnusedObjects(0);
			Design.NrDiag1Traces[Layer]++;
			DataBaseChanged = 1;
//        Object3=&((*Objects3)[NrObjects3]);
//        memmove(Object3,InsertTrace,sizeof(ObjectRecord));
//        NrObjects3++;
			return 1;
			break;

		case TRACE_DIAG2:
			if (!MakeRoomDiag2Traces(Layer))
				return 0;

			Trace = &((*Diag2Traces[Layer])[Design.NrDiag2Traces[Layer]]);
			Trace->X = (float) InsertTrace->x1;
			Trace->Y = (float) InsertTrace->y1;
			Trace->Length = (float) fabs(InsertTrace->x2);
			Trace->ThickNess = (float) InsertTrace->y2;
			Trace->NetNr = (int16) InsertTrace->NetNr;
			Trace->Info = 0;

			if ((NetNr >= 0) && (NetNr < Design.NrNets))
				Trace->Info = (int16) (Net->Info & OBJECT_HIGHLITED);

			Trace->Layer = Layer;
			Trace->Clearance = (float) InsertTrace->Clearance;
			Trace->AddNr = (int16) LastActionNr;
			Trace->DeleteNr = 0;
			InsertTrace->TraceNr = Design.NrDiag2Traces[Layer];
			ZeroUnusedObjects(0);
			Design.NrDiag2Traces[Layer]++;
			DataBaseChanged = 1;
//        Object3=&((*Objects3)[NrObjects3]);
//        memmove(Object3,InsertTrace,sizeof(ObjectRecord));
//        NrObjects3++;
			return 1;
			break;

		case TRACE_ALL_ANGLE:
		case OBJECT_LINE:
			memset(&NewObjectLine, 0, sizeof(NewObjectLine));
			NewObjectLine.X1 = (float) InsertTrace->x1;
			NewObjectLine.Y1 = (float) InsertTrace->y1;
			NewObjectLine.X2 = (float) InsertTrace->x2;
			NewObjectLine.Y2 = (float) InsertTrace->y2;
			NewObjectLine.LineThickNess = (float) InsertTrace->Thickness;
			NewObjectLine.Layer = InsertTrace->Layer;
			NewObjectLine.NetNr = (int16) InsertTrace->NetNr;
			NewObjectLine.AddNr = (int16) LastActionNr;
			NewObjectLine.Clearance = (float) InsertTrace->Clearance;

			if ((NetNr >= 0) && (NetNr < Design.NrNets))
				NewObjectLine.Info = (int16) (Net->Info & OBJECT_HIGHLITED);

			AddObjectLine(&NewObjectLine);
			break;

		case TRACE_ARC:
		case OBJECT_ARC:
			memset(&NewObjectArc, 0, sizeof(NewObjectArc));
			NewObjectArc.CentreX = (float) InsertTrace->x1;
			NewObjectArc.CentreY = (float) InsertTrace->y1;
			NewObjectArc.Width = (float) InsertTrace->x2;
			NewObjectArc.Height = (float) InsertTrace->y2;
			NewObjectArc.StartDiffX = (float) InsertTrace->x3;
			NewObjectArc.StartDiffY = (float) InsertTrace->y3;
			NewObjectArc.EndDiffX = (float) InsertTrace->x4;
			NewObjectArc.EndDiffY = (float) InsertTrace->y4;
			NewObjectArc.LineThickNess = (float) InsertTrace->Thickness;
			NewObjectArc.Clearance = (float) InsertTrace->Clearance;
			NewObjectArc.NetNr = (int16) InsertTrace->NetNr;
			NewObjectArc.Layer = InsertTrace->Layer;
			NewObjectArc.AddNr = (int16) LastActionNr;

			if ((NetNr >= 0) && (NetNr < Design.NrNets))
				NewObjectArc.Info = (int16) (Net->Info & OBJECT_HIGHLITED);

			AddObjectArc(&NewObjectArc);
			break;
		}
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddConnection(ConnectionsRecord * Connection)
{
	ConnectionsRecord *NewConnection;
	int32 ok;
	NetRecord *Net;

	Net = NULL;

	if (Design.NrConnections >= MaxNrConnections)
	{
		if (AllocateMemConnections(MaxNrConnections + 128) != 0)
			return 0;
	}

	if ((InRange(Connection->x1, Connection->x2)) && (InRange(Connection->y1, Connection->y2)))
	{
		ok = 1;
		return 1;
	}

	NewConnection = &((*Connections)[Design.NrConnections]);
	memmove(NewConnection, Connection, sizeof(ConnectionsRecord));

//  NewConnection->Info&=~0x003f;
	NewConnection->Info &= ~(0x003f | OBJECT_SELECTED);
	NewConnection->AddNr = (int16) LastActionNr;
	NewConnection->DeleteNr = 0;

	if ((Connection->NetNr >= 0) && (Connection->NetNr < Design.NrNets))
	{
		Net = &((*Nets)[Connection->NetNr]);
		NewConnection->Info =
		    (int16) ((Net->Info) & (CONNECTIONS_DISABLED | CONNECTIONS_NOT_VISIBLE | OBJECT_HIGHLITED));
	}

	ZeroUnusedObjects(0);
	Design.NrConnections++;
	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddVia(ViaRecord * Via)
{
	ViaRecord *NewVia;
	int32 NetNr;
	NetRecord *Net = NULL;

	if (Design.NrVias >= MaxNrVias)
	{
		if (AllocateMemVias(MaxNrVias + 128) != 0)
			return 0;
	}

	NewVia = &((*Vias)[Design.NrVias]);
	Via->Layer = -1;
	NetNr = Via->NetNr;

	if ((NetNr >= 0) && (NetNr < Design.NrNets))
		Net = &((*Nets)[NetNr]);

	if (NewestVia == NULL)
		NewestVia = NewVia;

	LastVia = NewVia;
	memmove(NewVia, Via, sizeof(ViaRecord));
	NewVia->Info = 0;

	if ((NetNr >= 0) && (NetNr < Design.NrNets))
		NewVia->Info = (int16) (Net->Info & OBJECT_HIGHLITED);

	NewVia->DeleteNr = 0;
	NewVia->AddNr = (int16) LastActionNr;
	DataBaseChanged = 1;
	ZeroUnusedObjects(0);
	Design.NrVias++;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddComp(CompRecord * Comp)
{
	int32 CompLength;
	int32 *CompPos;
	uint8 *CompP;
	CompRecord *Comp2;

	CompLength = sizeof(CompRecord) + (int32) Comp->NrPins * sizeof(CompPinRecord);

	if (Design.NrComps + 1 >= MaxNrComps)
	{
		if (AllocateMemComps(Design.NrComps + 32) != 0)
			return 0;
	}

	if (Design.CompsMem + CompLength >= MaxCompsMemory)
	{
		if (AllocateMemCompsMemory(Design.CompsMem + CompLength + 32768) != 0)
			return 0;
	}

	CompPos = &((*Comps)[Design.NrComps]);
	CompP = &(CompsMem[Design.CompsMem]);
	Comp2 = (CompRecord *) CompP;
	*CompPos = Design.CompsMem;
	memmove(CompP, Comp, CompLength);
	Design.CompsMem += CompLength;
	Comp2->AddNr = (int16) LastActionNr;
	Comp2->DeleteNr = 0;
//  Comp2->Info=0;
	Comp2->Info &= OBJECT_SELECTED | COMPONENT_PROTECTED | 0x3f;
	ZeroUnusedObjects(0);
	SetBoardPosComp(Comp2, 0);
	DataBaseChanged = 1;
	Design.NrComps++;
	VisibleMaxX = max(VisibleMaxX, Comp2->BoardPosMaxX);
	VisibleMinX = min(VisibleMinX, Comp2->BoardPosMinX);
	VisibleMaxY = max(VisibleMaxY, Comp2->BoardPosMaxY);
	VisibleMinY = min(VisibleMinY, Comp2->BoardPosMinY);
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckAreafill(AreaFillRecord * NewAreaFill)
{
	uint8 *PolygonPos;
	PolygonRecord *Polygon;
	int32 Length2, ok, cnt2;

	PolygonPos = (uint8 *) NewAreaFill;
	PolygonPos += sizeof(AreaFillRecord);
	Length2 = sizeof(AreaFillRecord);
	Polygon = (PolygonRecord *) PolygonPos;
	PolygonPos += MemSizePolygon(Polygon);
	Length2 += MemSizePolygon(Polygon);

	for (cnt2 = 1; cnt2 < NewAreaFill->NrPolygons; cnt2++)
	{
		Polygon = (PolygonRecord *) PolygonPos;

		if ((Polygon->NrVertices < 0) || (Polygon->NrVertices > 100000))
		{
			ok = 1;
			return;
		}

		Length2 += MemSizePolygon(Polygon);
		PolygonPos += MemSizePolygon(Polygon);
	}

	if (Length2 != NewAreaFill->MemSize)
	{
		ok = 1;
		return;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 AddAreaFill(int32 mode)
{
	int32 AreaFillLength, cnt2, cnt3, ok, count, PowerPlaneAreaFillNr, *AreaFillPos;
	uint8 *AreaFillP, *PolygonPos, *NewAreaPos, *NewAreaFillPolygonPos, *CheckPolygonPos, PolygonInclude[4000];
	double minx, maxx, miny, maxy, x1, y1;
	AreaFillRecord *NewAreaFill2;
	PolygonRecord *Polygon, *ReorganizedPolygon, *NewAreaFillPolygon, *SurroundPolygon, *CheckPolygon;

#ifdef _DEBUG
	int32 fp;
	char str[MAX_LENGTH_STRING];
	PolygonRecord *FirstPolygon;
#endif

	if (NewAreaFill->MemSize == 0)
		return -2;

	if (Design.NrAreaFills >= MaxNrAreaFills)
	{
		if (AllocateMemAreaFills(MaxNrAreaFills + 16) != 0)
			return -1;
	}

	AreaFillLength = NewAreaFill->MemSize;

	if (Design.AreaFillMem + AreaFillLength >= MaxAreaFillMemory)
	{
		if (AllocateMemAreaFillMemory(Design.AreaFillMem + AreaFillLength + 32768) != 0)
			return 0;
	}

	AreaFillPos = &((*AreaFills)[Design.NrAreaFills]);
	AreaFillP = &(AreaFillMem[Design.AreaFillMem]);
	memmove(AreaFillP, NewAreaFill, sizeof(AreaFillRecord));
	NewAreaPos = AreaFillP + sizeof(AreaFillRecord);
	NewAreaFill2 = (AreaFillRecord *) AreaFillP;
	*AreaFillPos = Design.AreaFillMem;


#ifdef _DEBUG
	FirstPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	CheckAreafill(NewAreaFill);
#endif

	AreaFillLength = sizeof(AreaFillRecord);
	/*
	  if ((mode==0)
	     &&
	     (PowerPlaneAreaFillNr!=-1)) {
	    NewAreaFill->NrPolygons=1;
	    NewAreaFill2->NrPolygons=1;
	  }
	*/
#ifdef _DEBUG
	sprintf(str, "%s\\pcb\\areafill.txt", DesignPath);

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return 0;

#endif

	memset(&PolygonInclude, 1, sizeof(PolygonInclude));
	PolygonPos = (uint8 *) NewAreaFill;
	PolygonPos += sizeof(AreaFillRecord);
	Polygon = (PolygonRecord *) PolygonPos;

	if (Polygon->NrVertices < 3)
		return 0;

	PolygonPos += MemSizePolygon(Polygon);

	for (cnt2 = 1; cnt2 < NewAreaFill->NrPolygons; cnt2++)
	{
		Polygon = (PolygonRecord *) PolygonPos;
		CheckPolygonPos = (uint8 *) NewAreaFill;
		CheckPolygonPos += sizeof(AreaFillRecord);
		CheckPolygon = (PolygonRecord *) CheckPolygonPos;
		CheckPolygonPos += MemSizePolygon(CheckPolygon);

		if (CheckPolygon->NrVertices == 0)
			ok = 1;

		for (cnt3 = 1; cnt3 < NewAreaFill->NrPolygons; cnt3++)
		{
			CheckPolygon = (PolygonRecord *) CheckPolygonPos;
#ifdef _DEBUG

			if (cnt3 == 28)
				ok = 1;

#endif

			if (cnt3 != cnt2)
			{
				if (CheckPolygonCompleetlyInsidePolygon(CheckPolygon, Polygon) == 1)
					PolygonInclude[cnt3] = 0;
			}

			CheckPolygonPos += MemSizePolygon(CheckPolygon);
		}

		PolygonPos += MemSizePolygon(Polygon);
	}

	minx = 1000000000.0;
	miny = 1000000000.0;
	maxx = -1000000000.0;
	maxy = -1000000000.0;


	AllocateSpecialMem(MEM_POLYGON_BIGGER, 256 * 1024, (void **) &ReorganizedPolygon);
	PolygonPos = (uint8 *) NewAreaFill;
	PolygonPos += sizeof(AreaFillRecord);
	NewAreaFillPolygonPos = AreaFillP + sizeof(AreaFillRecord);
	PowerPlaneAreaFillNr = GetPowerPlaneByLayer(NewAreaFill->Layer);
	NewAreaFill2->NrPolygons = 0;

	for (cnt2 = 0; cnt2 < NewAreaFill->NrPolygons; cnt2++)
	{
		Polygon = (PolygonRecord *) PolygonPos;
		NewAreaFillPolygon = (PolygonRecord *) NewAreaFillPolygonPos;
		Polygon->PolygonType &= ~2;	// Not selected
		ReorganizePolygon(Polygon, ReorganizedPolygon, NewAreaFill->Clearance);
		ReorganizedPolygon->PolygonType = Polygon->PolygonType;



		count = ReorganizedPolygon->NrVertices;

		if (cnt2 == 0)
		{
//      NewAreaFill2->NrVerticesStartPolygon=count;
			minx = 1000000000.0;
			miny = 1000000000.0;
			maxx = -1000000000.0;
			maxy = -1000000000.0;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				x1 = (*ReorganizedPolygon).Points[cnt3].x;
				y1 = (*ReorganizedPolygon).Points[cnt3].y;
//        NewAreaFill2->StartPolygon[cnt3].x=x1;
//        NewAreaFill2->StartPolygon[cnt3].y=y1;
				minx = min(minx, x1);
				miny = min(miny, y1);
				maxx = max(maxx, x1);
				maxy = max(maxy, y1);
			}

			NewAreaFill2->minx = minx;
			NewAreaFill2->miny = miny;
			NewAreaFill2->maxx = maxx;
			NewAreaFill2->maxy = maxy;
			SurroundPolygon = ReorganizedPolygon;
		}

		if ((cnt2 == 0) || (PolygonInclude[cnt2] == 1))
		{
#ifdef _DEBUG
			WriteLn(fp, "-------------------------------------------------------------");
			sprintf(str, "Polygon %i (PolygonType %i)", cnt2, ReorganizedPolygon->PolygonType);
			WriteLn(fp, str);
			count = Polygon->NrVertices;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				x1 = (*Polygon).Points[cnt3].x;
				y1 = (*Polygon).Points[cnt3].y;
				sprintf(str, "Polygon points  %14.5f,%14.5f %i  %i", x1, y1, cnt3, cnt2);
				WriteLn(fp, str);
			}

			WriteLn(fp, "-------------------------------------------------------------");
			sprintf(str, "Reorganized polygon %i (PolygonType %i)", cnt2, ReorganizedPolygon->PolygonType);
			WriteLn(fp, str);
			count = ReorganizedPolygon->NrVertices;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				x1 = (*ReorganizedPolygon).Points[cnt3].x;
				y1 = (*ReorganizedPolygon).Points[cnt3].y;
				sprintf(str, "Polygon points  %14.5f,%14.5f %i  %i", x1, y1, cnt3, cnt2);
				WriteLn(fp, str);
			}

#endif
			CopyPolygonToPolygon(ReorganizedPolygon, NewAreaFillPolygon);
			SetMinMaxPolygon(NewAreaFillPolygon, 0);
			NewAreaFillPolygon->PolygonType &= ~2;	// Not selected
			NewAreaFillPolygon->Special.Test = 0;
			NewAreaFillPolygon->Clearance = 0.0;
			AreaFillLength += MemSizePolygon(ReorganizedPolygon);
			NewAreaFillPolygonPos += MemSizePolygon(ReorganizedPolygon);
			NewAreaFill2->NrPolygons++;
		}

		PolygonPos += MemSizePolygon(Polygon);
	}

#ifdef _DEBUG
	sprintf(str, "Areafill minx,miny maxx,maxy  %14.5f,%14.5f %14.5f,%14.5f", minx, miny, maxx, maxy);
	WriteLn(fp, str);
	FileClose(fp);
#endif

	NewAreaFill2->MemSize = AreaFillLength;
	NewAreaFill2->AddNr = (int16) LastActionNr;
	NewAreaFill2->DeleteNr = 0;
	NewAreaFill2->Info &= ~OBJECT_SELECTED;
	ZeroUnusedObjects(0);
	Design.NrAreaFills++;
	Design.AreaFillMem += AreaFillLength;

	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 AreaFillToText(AreaFillRecord * AreaFill, int32 mode)
{
#ifdef _DEBUG
	double minx, maxx, miny, maxy, cx, cy;
	uint8 *PolygonPos, *AreaPos;
	char str[MAX_LENGTH_STRING];
	PolygonRecord *Polygon;
	int32 AreaFillLength, cnt, cnt2, Found, fp;

	if (mode & 2)
	{
		Found = -1;

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				AreaPos = (uint8 *) AreaFill;
				Polygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					PolygonPos = (uint8 *) Polygon;

					if ((Polygon->PolygonType & 2) == 2)
					{
						if (Found == -1)
							Found = cnt;
					}

					PolygonPos += MemSizePolygon(Polygon);
					Polygon = (PolygonRecord *) PolygonPos;
				}
			}
		}

		if (Found == -1)
			return 0;

		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
	}

	if (AreaFill->MemSize == 0)
		return -2;

	AreaFillLength = sizeof(AreaFillRecord);

	switch (mode & 3)
	{
	case 0:
		sprintf(str, "%s\\pcb\\areafill2.txt", DesignPath);
		break;

	case 1:
		sprintf(str, "%s\\pcb\\areafill2a.txt", DesignPath);
		break;

	case 2:
		sprintf(str, "%s\\pcb\\areafill3.txt", DesignPath);
		break;
	}

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return 0;

	minx = AreaFill->minx / 100000.0;
	miny = AreaFill->miny / 100000.0;
	maxx = AreaFill->maxx / 100000.0;
	maxy = AreaFill->maxy / 100000.0;
	sprintf(str, "Areafill minx,miny maxx,maxy  %8.3f,%8.3f %8.3f,%8.3f", minx, miny, maxx, maxy);
	WriteLn(fp, str);
	sprintf(str, "Nr polygons %d", AreaFill->NrPolygons);
	WriteLn(fp, str);
	WriteLn(fp, "------------------------------------------------------------------------------------------");
	PolygonPos = (uint8 *) AreaFill;
	PolygonPos += sizeof(AreaFillRecord);
	Polygon = (PolygonRecord *) PolygonPos;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		Polygon = (PolygonRecord *) PolygonPos;
		minx = Polygon->minx / 100000.0;
		miny = Polygon->miny / 100000.0;
		maxx = Polygon->maxx / 100000.0;
		maxy = Polygon->maxy / 100000.0;
		cx = (minx + maxx) * 0.5;
		cy = (miny + maxy) * 0.5;
		sprintf(str, "Polygon (%3d %3d %2d) minx,miny maxx,maxy  (%8.3f,%8.3f) %8.3f,%8.3f %8.3f,%8.3f", cnt2,
		        Polygon->NrVertices, Polygon->PolygonType, cx, cy, minx, miny, maxx, maxy);
		WriteLn(fp, str);

		if (cnt2 == 0)
			WriteLn(fp, "------------------------------------------------------------------------------------------");

		PolygonPos += MemSizePolygon(Polygon);
	}

	/*
	  PolygonPos=(uint8 *)AreaFill;
	  PolygonPos+=sizeof(AreaFillRecord);
	  Polygon=(PolygonRecord *)PolygonPos;
	  PolygonPos+=MemSizePolygon(Polygon);
	  for (cnt2=1;cnt2<AreaFill->NrPolygons;cnt2++) {
	    Polygon=(PolygonRecord *)PolygonPos;
	    CheckPolygonPos=(uint8 *)AreaFill;
	    CheckPolygonPos+=sizeof(AreaFillRecord);
	    CheckPolygon=(PolygonRecord *)CheckPolygonPos;
	    CheckPolygonPos+=MemSizePolygon(CheckPolygon);
	    if (CheckPolygon->NrVertices==0) {
	      ok=1;
	    }
	    for (cnt3=1;cnt3<NewAreaFill->NrPolygons;cnt3++) {
	      CheckPolygon=(PolygonRecord *)CheckPolygonPos;
	#ifdef _DEBUG
	      if (cnt3==28) {
	        ok=1;
	      }
	#endif
	      if (cnt3!=cnt2) {
	        if (CheckPolygonCompleetlyInsidePolygon(CheckPolygon,Polygon)==1) {
	          PolygonInclude[cnt3]=0;
	        }
	      }
	      CheckPolygonPos+=MemSizePolygon(CheckPolygon);
	    }
	    PolygonPos+=MemSizePolygon(Polygon);
	  }

	  minx= 1000000000.0;
	  miny= 1000000000.0;
	  maxx=-1000000000.0;
	  maxy=-1000000000.0;


	  ReorganizedPolygon=(PolygonRecord *)&Buf;
	  PolygonPos=(uint8 *)NewAreaFill;
	  PolygonPos+=sizeof(AreaFillRecord);
	  NewAreaFillPolygonPos=AreaFillP+sizeof(AreaFillRecord);
	  PowerPlaneAreaFillNr=GetPowerPlaneByLayer(NewAreaFill->Layer);
	  for (cnt2=0;cnt2<NewAreaFill->NrPolygons;cnt2++) {
	    Polygon=(PolygonRecord *)PolygonPos;
	    NewAreaFillPolygon=(PolygonRecord *)NewAreaFillPolygonPos;
	    Polygon->PolygonType&=~2;  // Not selected
	    ReorganizePolygon(Polygon,ReorganizedPolygon,NewAreaFill->Clearance);
	    ReorganizedPolygon->PolygonType=Polygon->PolygonType;



	    count=ReorganizedPolygon->NrVertices;
	    if (cnt2==0) {
	//      NewAreaFill2->NrVerticesStartPolygon=count;
	      minx= 1000000000.0;
	      miny= 1000000000.0;
	      maxx=-1000000000.0;
	      maxy=-1000000000.0;
	      for (cnt3=0;cnt3<count;cnt3++) {
	        x1=(*ReorganizedPolygon).Points[cnt3].x;
	        y1=(*ReorganizedPolygon).Points[cnt3].y;
	//        NewAreaFill2->StartPolygon[cnt3].x=x1;
	//        NewAreaFill2->StartPolygon[cnt3].y=y1;
	        minx=min(minx,x1);
	        miny=min(miny,y1);
	        maxx=max(maxx,x1);
	        maxy=max(maxy,y1);
	      }
	      NewAreaFill2->minx=minx;
	      NewAreaFill2->miny=miny;
	      NewAreaFill2->maxx=maxx;
	      NewAreaFill2->maxy=maxy;
	      SurroundPolygon=ReorganizedPolygon;
	    }
	    if ((cnt2==0)
	       ||
	       (PolygonInclude[cnt2]==1)) {
	#ifdef _DEBUG
	      WriteLn(fp,"-------------------------------------------------------------");
	      sprintf(str,"Polygon %i (PolygonType %i)",cnt2,ReorganizedPolygon->PolygonType);
	      WriteLn(fp,str);
	      count=Polygon->NrVertices;
	      for (cnt3=0;cnt3<count;cnt3++) {
	        x1=(*Polygon).Points[cnt3].x;
	        y1=(*Polygon).Points[cnt3].y;
	        sprintf(str,"Polygon points  %14.5f,%14.5f %i  %i",x1,y1,cnt3,cnt2);
	        WriteLn(fp,str);
	      }
	      WriteLn(fp,"-------------------------------------------------------------");
	      sprintf(str,"Reorganized polygon %i (PolygonType %i)",cnt2,ReorganizedPolygon->PolygonType);
	      WriteLn(fp,str);
	      count=ReorganizedPolygon->NrVertices;
	      for (cnt3=0;cnt3<count;cnt3++) {
	        x1=(*ReorganizedPolygon).Points[cnt3].x;
	        y1=(*ReorganizedPolygon).Points[cnt3].y;
	        sprintf(str,"Polygon points  %14.5f,%14.5f %i  %i",x1,y1,cnt3,cnt2);
	        WriteLn(fp,str);
	      }
	#endif
	      CopyPolygonToPolygon(ReorganizedPolygon,NewAreaFillPolygon);
	      SetMinMaxPolygon(NewAreaFillPolygon,0);
	      NewAreaFillPolygon->PolygonType&=~2;  // Not selected
	      NewAreaFillPolygon->Special.Test=0;
	      NewAreaFillPolygon->Clearance=0.0;
	      AreaFillLength+=MemSizePolygon(ReorganizedPolygon);
	      NewAreaFillPolygonPos+=MemSizePolygon(ReorganizedPolygon);
	      NewAreaFill2->NrPolygons++;
	    }
	    PolygonPos+=MemSizePolygon(Polygon);
	  }

	  sprintf(str,"Areafill minx,miny maxx,maxy  %14.5f,%14.5f %14.5f,%14.5f",minx,miny,maxx,maxy);
	  WriteLn(fp,str);
	*/
	FileClose(fp);

#endif
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeRoomHorTraces(int32 Layer)
{
	if (Design.NrHorTraces[Layer] >= MaxNrHorTraces[Layer])
	{
		if (AllocateMemHorTraces(Layer, MaxNrHorTraces[Layer] + 256) != 0)
			return 0;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeRoomVerTraces(int32 Layer)
{
	if (Design.NrVerTraces[Layer] >= MaxNrVerTraces[Layer])
	{
		if (AllocateMemVerTraces(Layer, MaxNrVerTraces[Layer] + 256) != 0)
			return 0;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeRoomDiag1Traces(int32 Layer)
{
	if (Design.NrDiag1Traces[Layer] >= MaxNrDiag1Traces[Layer])
	{
		if (AllocateMemDiag1Traces(Layer, MaxNrDiag1Traces[Layer] + 256) != 0)
			return 0;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeRoomDiag2Traces(int32 Layer)
{

	if (Design.NrDiag2Traces[Layer] >= MaxNrDiag2Traces[Layer])
	{
		if (AllocateMemDiag2Traces(Layer, MaxNrDiag2Traces[Layer] + 256) != 0)
			return 0;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectLine(ObjectLineRecord * ObjectLine)
{
	ObjectLineRecord *NewObjectLine;

	if (Design.NrObjectLines >= MaxNrObjectLines)
	{
		if (AllocateMemObjectLines(MaxNrObjectLines + 32) != 0)
			return 0;
	}

//  if (!MakeRoomObjectLines(1)) return 0;
	NewObjectLine = &((*ObjectLines)[Design.NrObjectLines]);
	memmove(NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
//  NewObjectLine->Info=OBJECT_ADDED;
	NewObjectLine->AddNr = (int16) LastActionNr;
	NewObjectLine->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectLines++;

	if (NewObjectLine->Layer == BOARD_OUTLINE_LAYER)
	{
//    RecalcBoardSize(0);
	}

	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectRect(ObjectRectRecord * ObjectRect)
{
	ObjectRectRecord *NewObjectRect;

	if (Design.NrObjectRects >= MaxNrObjectRects)
	{
		if (AllocateMemObjectRects(MaxNrObjectRects + 32) != 0)
			return 0;
	}

//  if (!MakeRoomObjectRects(1)) return 0;
	NewObjectRect = &((*ObjectRects)[Design.NrObjectRects]);
	memmove(NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
//  NewObjectRect->Info=OBJECT_ADDED;
	NewObjectRect->AddNr = (int16) LastActionNr;
	NewObjectRect->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectRects++;

	if (NewObjectRect->Layer == BOARD_OUTLINE_LAYER)
	{
//    RecalcBoardSize(0);
	}

	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
/*
int32 AddObjectCircle(ObjectCircleRecord *ObjectCircle)

{
  ObjectCircleRecord *NewObjectCircle;
  ObjectRecord *Object3;

  if (Design.NrObjectCircles>=MaxNrObjectCircles) {
    if (AllocateMemObjectCircles(MaxNrObjectCircles+32)!=0) {
      return 0;
    }
  }
//  if (!MakeRoomObjectCircles(1)) return 0;
  NewObjectCircle=&((*ObjectCircles)[Design.NrObjectCircles]);
  memmove(NewObjectCircle,ObjectCircle,sizeof(ObjectCircleRecord));
//  NewObjectCircle->Info=OBJECT_ADDED;
  NewObjectCircle->AddNr=LastActionNr;
  NewObjectCircle->DeleteNr=0;
  ZeroUnusedObjects(0);
  Design.NrObjectCircles++;
  if (NewObjectCircle->Layer==BOARD_OUTLINE_LAYER) {
//    RecalcBoardSize(0);
  }
  DataBaseChanged=1;
  return 1;
}
*/
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectArc(ObjectArcRecord * ObjectArc)
{
	ObjectArcRecord *NewObjectArc;

	if (Design.NrObjectArcs >= MaxNrObjectArcs)
	{
		if (AllocateMemObjectArcs(MaxNrObjectArcs + 32) != 0)
			return 0;
	}

//  if (!MakeRoomObjectArcs(1)) return 0;
	NewObjectArc = &((*ObjectArcs)[Design.NrObjectArcs]);
	memmove(NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
//  NewObjectArc->Info=OBJECT_ADDED;
	NewObjectArc->AddNr = (int16) LastActionNr;
	NewObjectArc->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectArcs++;

	if (NewObjectArc->Layer == BOARD_OUTLINE_LAYER)
	{
//    RecalcBoardSize(0);
	}

	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
/*
int32 AddObjectText(ObjectTextRecord *ObjectText)

{
  ObjectTextRecord *NewObjectText;
  ObjectRecord *Object3;

#ifdef VERSION25
  return 1;
#endif

  if (strlen(ObjectText->Text)==0) return 0;
  if (Design.NrObjectTexts>=MaxNrObjectTexts) {
    if (AllocateMemObjectTexts(MaxNrObjectTexts+32)!=0) {
      return 0;
    }
  }
//  if (!MakeRoomObjectTexts(1)) return 0;
  NewObjectText=&((*ObjectTexts)[Design.NrObjectTexts]);
  memmove(NewObjectText,ObjectText,sizeof(ObjectTextRecord));
//  NewObjectText->Info=OBJECT_ADDED;
  NewObjectText->AddNr=LastActionNr;
  NewObjectText->DeleteNr=0;
  ZeroUnusedObjects(0);
  Design.NrObjectTexts++;
  DataBaseChanged=1;
  return 1;
}
*/
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 AddObjectText2(ObjectTextRecord2 * ObjectText2)
{
	ObjectTextRecord2 *NewObjectText2;

	if (strlen(ObjectText2->Text) == 0)
		return 0;

	if (Design.NrObjectTexts2 >= MaxNrObjectTexts2)
	{
		if (AllocateMemObjectTexts2(MaxNrObjectTexts2 + 32) != 0)
			return 0;
	}

//  if (!MakeRoomObjectTexts(1)) return 0;
	NewObjectText2 = &((*ObjectTexts2)[Design.NrObjectTexts2]);
	memmove(NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
//  NewObjectText->Info=OBJECT_ADDED;
	NewObjectText2->AddNr = (int16) LastActionNr;
	NewObjectText2->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectTexts2++;
	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectPolygon(ObjectPolygonRecord * ObjectPolygon)
{
	int32 ObjectPolygonLength, *ObjectPolygonPos;
	uint8 *ObjectPolygonP;
	ObjectPolygonRecord *NewObjectPolygon2;

	if (Design.NrObjectPolygons >= MaxNrObjectPolygons)
	{
		if (AllocateMemObjectPolygons(MaxNrObjectPolygons + 128) != 0)
			return -1;
	}

	ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

	if (Design.ObjectPolygonMem + ObjectPolygonLength >= MaxObjectPolygonMemory)
	{
		if (AllocateMemObjectPolygonMemory(Design.ObjectPolygonMem + ObjectPolygonLength + 32768) != 0)
			return 0;
	}

	ObjectPolygonPos = &((*ObjectPolygons)[Design.NrObjectPolygons]);
	ObjectPolygonP = &(ObjectPolygonMem[Design.ObjectPolygonMem]);
	memmove(ObjectPolygonP, ObjectPolygon, ObjectPolygonLength);
	NewObjectPolygon2 = (ObjectPolygonRecord *) ObjectPolygonP;
	*ObjectPolygonPos = Design.ObjectPolygonMem;

	NewObjectPolygon2->AddNr = (int16) LastActionNr;
	NewObjectPolygon2->DeleteNr = 0;
	SetMinMaxObjectPolygon(NewObjectPolygon2, 0);

	ZeroUnusedObjects(0);
	Design.NrObjectPolygons++;
	Design.ObjectPolygonMem += ObjectPolygonLength;

	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ConnectionExists(int32 MinConnectionX1, int32 MinConnectionY1, int32 MinConnectionX2, int32 MinConnectionY2)
{
	int32 cnt;
	int32 Stop;

	cnt = 0;
	Stop = 0;

	while ((cnt < Design.NrConnections) && (!Stop))
	{
		if (((MinConnectionX1 == (*Connections)[cnt].x1) && (MinConnectionY1 == (*Connections)[cnt].y1)
		        && (MinConnectionX2 == (*Connections)[cnt].x2) && (MinConnectionY2 == (*Connections)[cnt].y2))
		        || ((MinConnectionX1 == (*Connections)[cnt].x2) && (MinConnectionY1 == (*Connections)[cnt].y2)
		            && (MinConnectionX2 == (*Connections)[cnt].x1) && (MinConnectionY2 == (*Connections)[cnt].y1)))
			Stop = 1;

		cnt++;
	}

	return Stop;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectsEqual(int32 Object2Nr, int32 Object4Nr)
{
	ObjectRecord *Object1, *Object2;

	Object2 = &((*Objects4)[Object4Nr]);

	if (Object2->Test == 0)
		return 0;

	Object1 = &((*Objects2)[Object2Nr]);

	if (Object1->ObjectType != Object2->ObjectType)
		return 0;

	if (Object1->ObjectType == CONNECTION)
	{
		if (!
		        (((Object1->x1 == Object2->x1) && (Object1->y1 == Object2->y1) && (Object1->x2 == Object2->x2)
		          && (Object1->y2 == Object2->y2)) || ((Object1->x1 == Object2->x2) && (Object1->y1 == Object2->y2)
		                  && (Object1->x2 == Object2->x1) && (Object1->y2 == Object2->y1))))
			return 0;
	}
	else
	{
		if (Object1->Layer != Object2->Layer)
			return 0;

		if (Object1->x1 != Object2->x1)
			return 0;

		if (Object1->y1 != Object2->y1)
			return 0;

		if (Object1->x2 != Object2->x2)
			return 0;

		if (Object1->y2 != Object2->y2)
			return 0;

		if (Object1->Clearance != Object2->Clearance)
			return 0;
	}

	Object1->Test = 0;
	Object2->Test = 0;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UndoObjects()
{
	int32 cnt, cnt2, Layer, DeleteCount, NetNr;
	int32 Changed, ChangedConnection;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	TraceRecord *Trace;
	CompRecord *Comp;
	ViaRecord *Via;
	NetRecord *Net;
	ConnectionsRecord *Connection;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

	if (LastActionNr < 2)
		return;

	if (MaxLastActionNr < LastActionNr)
		MaxLastActionNr = LastActionNr;

	if (LastActionNr > 1)
		LastActionNr--;

	Changed = 0;
	DeleteCount = 0;
	ChangedConnection = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);

		if (Connection->AddNr == LastActionNr)
		{
			if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Connection->Info &= ~(OBJECT_SELECTED | 63);
				Connection->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
				ChangedConnection = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);

		if (Connection->DeleteNr == LastActionNr)
		{
			if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Connection->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 63);
				NetNr = Connection->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);

					if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
						Connection->Info |= OBJECT_HIGHLITED;
				}

				Changed = 1;
				ChangedConnection = 1;
			}
		}
	}

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}
	}

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}
	}

// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = (ViaRecord *) & ((*Vias)[cnt]);

		if (Via->AddNr == LastActionNr)
		{
			if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Via->Info &= ~(OBJECT_SELECTED | 15);
				Via->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = (ViaRecord *) & ((*Vias)[cnt]);

		if (Via->DeleteNr == LastActionNr)
		{
			if ((Via->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Via->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

				if (OkToDrawVias)
				{
					NetNr = Via->NetNr;

					if ((NetNr >= 0) && (NetNr < Design.NrNets))
					{
						Net = &((*Nets)[NetNr]);

						if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
							Via->Info |= OBJECT_HIGHLITED;
					}
				}

				Changed = 1;
			}
		}
	}

// ****************************************************************************************
// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->AddNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectLine->Info &= ~(OBJECT_SELECTED | 15);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	CurrentObjectCode = -1;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->DeleteNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectLine->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->AddNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectRect->Info &= ~(OBJECT_SELECTED | 15);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->DeleteNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectRect->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (ObjectCircle->AddNr == LastActionNr) {
	      if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == 0) {
	        SetBackGroundActive(0);
	        ObjectCircle->Info&=~(OBJECT_SELECTED|15);
	        DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	        ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	        Changed=1;
	      }
	    }
	  }
	  CurrentObjectCode=-1;
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (ObjectCircle->DeleteNr == LastActionNr) {
	      if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE) {
	        ObjectCircle->Info&=~(OBJECT_NOT_VISIBLE|OBJECT_SELECTED|15);
	        DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	        Changed=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->AddNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectArc->Info &= ~(OBJECT_SELECTED | 15);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->DeleteNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectArc->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (ObjectText->AddNr == LastActionNr) {
	      if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == 0) {
	        SetBackGroundActive(0);
	        ObjectText->Info&=~(OBJECT_SELECTED|15);
	        DrawObjectText(ObjectText,0.0,0.0,0);
	        ObjectText->Info|=OBJECT_NOT_VISIBLE;
	        Changed=1;
	      }
	    }
	  }
	  CurrentObjectCode=-1;
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (ObjectText->DeleteNr == LastActionNr) {
	      if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE) {
	        ObjectText->Info&=~(OBJECT_NOT_VISIBLE|OBJECT_SELECTED|15);
	        DrawObjectText(ObjectText,0.0,0.0,0);
	        Changed=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (ObjectText2->AddNr == LastActionNr)
		{
			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectText2->Info &= ~(OBJECT_SELECTED | 15);
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (ObjectText2->DeleteNr == LastActionNr)
		{
			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectText2->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->AddNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectPolygon->Info &= ~(OBJECT_SELECTED | 15);
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->DeleteNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectPolygon->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

// ****************************************************************************************
// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->AddNr == LastActionNr)
		{
			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Comp->Info &= ~(OBJECT_SELECTED | 0x3f);
				Comp->TextVisibility &= ~(1 + 16);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->DeleteNr == LastActionNr)
		{
			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Comp->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 0x3f);
				Comp->TextVisibility &= ~(1 + 16);
				Changed = 1;
			}
		}
	}

// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (AreaFill->AddNr == LastActionNr)
		{
			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				AreaFill->Info &= ~(OBJECT_SELECTED);
				AreaFill->Info |= OBJECT_NOT_VISIBLE;
				AreaPos = (uint8 *) AreaFill;
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
				PolygonPos = (uint8 *) DrawPolygon;

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					DrawPolygon->PolygonType &= 8 + 4 + 1;
					DrawPolygon->Special.Test = 0;
					DrawPolygon->Clearance = 0.0;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;
				}

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (AreaFill->DeleteNr == LastActionNr)
		{
			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				AreaFill->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
				AreaPos = (uint8 *) AreaFill;
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
				PolygonPos = (uint8 *) DrawPolygon;

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					DrawPolygon->PolygonType &= 8 + 4 + 1;
					DrawPolygon->Special.Test = 0;
					DrawPolygon->Clearance = 0.0;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;
				}

				Changed = 1;
			}
		}
	}

// ****************************************************************************************
	if (Changed)
	{
		DataBaseChanged = 1;
		UndoRedoActive = 1;
	}

	if (ChangedConnection)
		CheckFloatingConnections();

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedoObjects()
{
	int32 cnt, cnt2, Layer, DeleteCount, NetNr;
	int32 Changed;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	TraceRecord *Trace;
	CompRecord *Comp;
	ViaRecord *Via;
	NetRecord *Net;
	ConnectionsRecord *Connection;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

	if ((LastActionNr > MaxLastActionNr) || (LastActionNr == 0))
		return;

	Changed = 0;
	DeleteCount = 1;

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (Trace->DeleteNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Trace->Info &= ~(OBJECT_SELECTED | 15);
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Changed = 1;
				}
			}
		}
	}

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (Trace->AddNr == LastActionNr)
			{
				if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				{
					Trace->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);

					if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;

						if ((NetNr >= 0) && (NetNr < Design.NrNets))
						{
							Net = &((*Nets)[NetNr]);

							if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
						}
					}

					Changed = 1;
				}
			}
		}
	}

// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = (ViaRecord *) & ((*Vias)[cnt]);

		if (Via->DeleteNr == LastActionNr)
		{
			if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Via->Info &= ~(OBJECT_SELECTED | 15);
				Via->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = (ViaRecord *) & ((*Vias)[cnt]);

		if (Via->AddNr == LastActionNr)
		{
			if ((Via->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Via->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				NetNr = Via->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);

					if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
						Via->Info |= OBJECT_HIGHLITED;
				}

				Changed = 1;
			}
		}
	}

// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);

		if (Connection->DeleteNr == LastActionNr)
		{
			if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Connection->Info &= ~(OBJECT_SELECTED | 63);
				Connection->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);

		if (Connection->AddNr == LastActionNr)
		{
			if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Connection->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 63);
				NetNr = Connection->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);

					if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
						Connection->Info |= OBJECT_HIGHLITED;
				}

				Changed = 1;
			}
		}
	}

// ****************************************************************************************
// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->DeleteNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectLine->Info &= ~(OBJECT_SELECTED | 15);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->AddNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectLine->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->DeleteNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectRect->Info &= ~(OBJECT_SELECTED | 15);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->AddNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectRect->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (ObjectCircle->DeleteNr == LastActionNr) {
	      if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == 0) {
	        SetBackGroundActive(0);
	        ObjectCircle->Info&=~(OBJECT_SELECTED|15);
	        DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	        ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	        Changed=1;
	      }
	    }
	  }
	  CurrentObjectCode=-1;
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (ObjectCircle->AddNr == LastActionNr) {
	      if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE) {
	        ObjectCircle->Info&=~(OBJECT_NOT_VISIBLE|OBJECT_SELECTED|15);
	        DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	        Changed=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->DeleteNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectArc->Info &= ~(OBJECT_SELECTED | 15);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->AddNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectArc->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (ObjectText->DeleteNr == LastActionNr) {
	      if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == 0) {
	        SetBackGroundActive(0);
	        ObjectText->Info&=~(OBJECT_SELECTED|15);
	        DrawObjectText(ObjectText,0.0,0.0,0);
	        ObjectText->Info|=OBJECT_NOT_VISIBLE;
	        Changed=1;
	      }
	    }
	  }
	  CurrentObjectCode=-1;
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (ObjectText->AddNr == LastActionNr) {
	      if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE) {
	        ObjectText->Info&=~(OBJECT_NOT_VISIBLE|OBJECT_SELECTED|15);
	        DrawObjectText(ObjectText,0.0,0.0,0);
	        Changed=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (ObjectText2->DeleteNr == LastActionNr)
		{
			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectText2->Info &= ~(OBJECT_SELECTED | 15);
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (ObjectText2->AddNr == LastActionNr)
		{
			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectText2->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}


	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->DeleteNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectPolygon->Info &= ~(OBJECT_SELECTED | 15);
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->AddNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectPolygon->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

// ****************************************************************************************
// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->DeleteNr == LastActionNr)
		{
			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Comp->Info &= ~(OBJECT_SELECTED | 0x3f);
				Comp->TextVisibility &= ~(1 + 16);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->AddNr == LastActionNr)
		{
			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Comp->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 0x3f);
				Comp->TextVisibility &= ~(1 + 16);
				Changed = 1;
			}
		}
	}

// ****************************************************************************************

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (AreaFill->DeleteNr == LastActionNr)
		{
			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				AreaFill->Info &= ~(OBJECT_SELECTED);
				AreaFill->Info |= OBJECT_NOT_VISIBLE;
				AreaPos = (uint8 *) AreaFill;
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
				PolygonPos = (uint8 *) DrawPolygon;

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					DrawPolygon->PolygonType &= 8 + 4 + 1;
					DrawPolygon->Special.Test = 0;
					DrawPolygon->Clearance = 0.0;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;
				}

				Changed = 1;
			}
		}
	}

	CurrentObjectCode = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (AreaFill->AddNr == LastActionNr)
		{
			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				AreaFill->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
				AreaPos = (uint8 *) AreaFill;
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
				PolygonPos = (uint8 *) DrawPolygon;

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					DrawPolygon->PolygonType &= 8 + 4 + 1;
					DrawPolygon->Special.Test = 0;
					DrawPolygon->Clearance = 0.0;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;
				}

				Changed = 1;
			}
		}
	}

// ****************************************************************************************
	if (MaxLastActionNr > LastActionNr)
		LastActionNr++;

	if (Changed)
	{
		DataBaseChanged = 1;
		UndoRedoActive = 1;
	}

	ExitDrawing();
	EndDrawingEditingWindow(0);
	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ZeroObjects()
{
	int32 cnt, cnt2, Layer;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	TraceRecord *Trace;
	CompRecord *Comp;
	ViaRecord *Via;
	ConnectionsRecord *Connection;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = (ViaRecord *) & ((*Vias)[cnt]);
		Via->DeleteNr = 0;
		Via->AddNr = 0;
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);
		Connection->DeleteNr = 0;
		Connection->AddNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
		ObjectLine->DeleteNr = 0;
		ObjectLine->AddNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);
		ObjectRect->DeleteNr = 0;
		ObjectRect->AddNr = 0;
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    ObjectCircle->DeleteNr=0;
	    ObjectCircle->AddNr=0;
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ObjectArc->DeleteNr = 0;
		ObjectArc->AddNr = 0;
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    ObjectText->DeleteNr=0;
	    ObjectText->AddNr=0;
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);
		ObjectText2->DeleteNr = 0;
		ObjectText2->AddNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		ObjectPolygon->DeleteNr = 0;
		ObjectPolygon->AddNr = 0;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->DeleteNr = 0;
		Comp->AddNr = 0;
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
		AreaFill->DeleteNr = 0;
		AreaFill->AddNr = 0;
		AreaPos = (uint8 *) AreaFill;
		DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

		for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
		{
			PolygonPos = (uint8 *) DrawPolygon;
			DrawPolygon->PolygonType &= ~2;
			PolygonPos += MemSizePolygon(DrawPolygon);
			DrawPolygon = (PolygonRecord *) PolygonPos;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CalcMemory(int32 mode)
{
#ifdef _DEBUG
	int32 Layer, TraceInfo, ViaInfo, cnt, cnt2, TracesDeleted[32], ViasDeleted, AreaFillMemDeleted, CompsMemDeleted,
	      CompsDeleted, ConnectionsDeleted, TotalMemDeleted, TotalMem, ObjectLinesDeleted, ObjectRectsDeleted,
	      ObjectCirclesDeleted, ObjectArcsDeleted, ObjectTextsDeleted, AreaFillsDeleted, MemoryUse;
	TraceRecord *Trace;
	ViaRecord *Via;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	CompRecord *Comp;
	ConnectionsRecord *Connection;
	AreaFillRecord *AreaFill;
	char str[MAX_LENGTH_STRING];
	char TextBuf[32768];
	TotalMem = 0;
	TotalMemDeleted = 0;

	for (cnt = 0; cnt < 32; cnt++)
		TracesDeleted[cnt] = 0;

	ViasDeleted = 0;
	AreaFillMemDeleted = 0;
	AreaFillsDeleted = 0;
	CompsMemDeleted = 0;
	CompsDeleted = 0;
	ConnectionsDeleted = 0;
	ObjectLinesDeleted = 0;
	ObjectRectsDeleted = 0;
	ObjectCirclesDeleted = 0;
	ObjectArcsDeleted = 0;
	ObjectTextsDeleted = 0;

	for (Layer = 0; Layer < Design.NrBoardLayers; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				TracesDeleted[Layer]++;
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				TracesDeleted[Layer]++;
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				TracesDeleted[Layer]++;
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				TracesDeleted[Layer]++;
		}

		cnt2 =
		    Design.NrVerTraces[Layer] + Design.NrHorTraces[Layer] + Design.NrDiag1Traces[Layer] +
		    Design.NrDiag2Traces[Layer];
		sprintf(str, "Traces layer %i \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i", Layer, cnt2, cnt2 * sizeof(TraceRecord),
		        TracesDeleted[Layer], TracesDeleted[Layer] * sizeof(TraceRecord));

		if (AddToMessageBuf(str) != 0)
			return 0;

		TotalMem += cnt2 * sizeof(TraceRecord);
		TotalMemDeleted += TracesDeleted[Layer] * sizeof(TraceRecord);
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			ViasDeleted++;
	}

	sprintf(str, "Vias             \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i", Design.NrVias,
	        Design.NrVias * sizeof(ViaRecord), ViasDeleted, ViasDeleted * sizeof(ViaRecord));

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.NrVias * sizeof(ViaRecord);
	TotalMemDeleted += ViasDeleted * sizeof(ViaRecord);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
		{
			CompsMemDeleted += sizeof(CompRecord) + Comp->NrPins * sizeof(int32);
			CompsDeleted++;
		}
	}

	sprintf(str, "Components       \tTotal nr\t%i\t\t\tDeleted\t%i ", Design.NrComps, CompsDeleted);

	if (AddToMessageBuf(str) != 0)
		return 0;

	sprintf(str, "Comps memory \tTotal\t%i\t\t\tDeleted\t\t%i", Design.CompsMem, CompsMemDeleted);

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.CompsMem;
	TotalMemDeleted += CompsMemDeleted;
	sprintf(str, "Geometries       \tTotal nr\t%i", Design.NrShapes);

	if (AddToMessageBuf(str) != 0)
		return 0;

	sprintf(str, "Geom memory \tTotal\t%i", Design.ShapesMem);

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.ShapesMem;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);

		if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			ConnectionsDeleted++;
	}

	sprintf(str, "Connections      \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i", Design.NrConnections,
	        Design.NrConnections * sizeof(ConnectionsRecord), ConnectionsDeleted,
	        ConnectionsDeleted * sizeof(ConnectionsRecord));

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.NrConnections * sizeof(ConnectionsRecord);
	TotalMemDeleted += ConnectionsDeleted * sizeof(ConnectionsRecord);

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
		{
			AreaFillMemDeleted += AreaFill->MemSize;
			AreaFillsDeleted++;
		}
	}

	sprintf(str, "AreaFills        \tTotal nr\t%i\t\t\tDeleted\t%i", Design.NrAreaFills, AreaFillsDeleted);

	if (AddToMessageBuf(str) != 0)
		return 0;

	sprintf(str, "AreaFill mem \tTotal\t\t%i\t\tDeleted\t\t%i", Design.AreaFillMem, AreaFillMemDeleted);

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.AreaFillMem;
	TotalMemDeleted += AreaFillMemDeleted;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectLinesDeleted++;
	}

	sprintf(str, "Lines            \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i", Design.NrObjectLines,
	        Design.NrObjectLines * sizeof(ObjectLineRecord), ObjectLinesDeleted,
	        ObjectLinesDeleted * sizeof(ObjectLineRecord));

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.NrObjectLines * sizeof(ObjectLineRecord);
	TotalMemDeleted += ObjectLinesDeleted * sizeof(ObjectLineRecord);

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectRectsDeleted++;
	}

	sprintf(str, "Rectangle        \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i", Design.NrObjectRects,
	        Design.NrObjectRects * sizeof(ObjectRectRecord), ObjectRectsDeleted,
	        ObjectRectsDeleted * sizeof(ObjectRectRecord));

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.NrObjectRects * sizeof(ObjectRectRecord);
	TotalMemDeleted += ObjectRectsDeleted * sizeof(ObjectRectRecord);

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      ObjectCirclesDeleted++;
	    }
	  }
	  sprintf(str,"Circles          \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i",
	              Design.NrObjectCircles,Design.NrObjectCircles*sizeof(ObjectCircleRecord),
	              ObjectCirclesDeleted,
	              ObjectCirclesDeleted*sizeof(ObjectCircleRecord));
	  if (AddToMessageBuf(str)!=0) return 0;
	  TotalMem+=Design.NrObjectCircles*sizeof(ObjectCircleRecord);
	  TotalMemDeleted+=ObjectCirclesDeleted*sizeof(ObjectCircleRecord);
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectArcsDeleted++;
	}

	sprintf(str, "Arcs             \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i", Design.NrObjectArcs,
	        Design.NrObjectArcs * sizeof(ObjectArcRecord), ObjectArcsDeleted,
	        ObjectArcsDeleted * sizeof(ObjectArcRecord));

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.NrObjectArcs * sizeof(ObjectArcRecord);
	TotalMemDeleted += ObjectArcsDeleted * sizeof(ObjectArcRecord);

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectTextsDeleted++;
	}

	sprintf(str, "Texts            \tTotal nr\t%i\t%i\t\tDeleted\t%i\t%i", Design.NrObjectTexts,
	        Design.NrObjectTexts * sizeof(ObjectTextRecord), ObjectTextsDeleted,
	        ObjectTextsDeleted * sizeof(ObjectTextRecord));

	if (AddToMessageBuf(str) != 0)
		return 0;

	TotalMem += Design.NrObjectTexts * sizeof(ObjectTextRecord);
	TotalMemDeleted += ObjectTextsDeleted * sizeof(ObjectTextRecord);

	str[0] = 0;

	if (AddToMessageBuf(str) != 0)
		return 0;

	sprintf(str, "Total memory used   \t%i\t[ %i k ]", TotalMem, TotalMem / 1024);

	if (AddToMessageBuf(str) != 0)
		return 0;

	sprintf(str, "Total memory deleted \t%i\t[ %i k ]", TotalMemDeleted, TotalMemDeleted / 1024);

	if (AddToMessageBuf(str) != 0)
		return 0;

	if (AddToMessageBuf("") != 0)
		return 0;

// *******************************************************************************************************
// *******************************************************************************************************

	MemoryUse = GetUsedMemSize(1, (LPSTR) & TextBuf);

	if (AddToMessageBuf(TextBuf) != 0)
		return 0;

	if (AddToMessageBuf("") != 0)
		return 0;

	sprintf(str, "Total internal memory \t%i\t[ %i k ]", MemoryUse, MemoryUse / 1024);

	if (AddToMessageBuf(str) != 0)
		return 0;

	if (MessageBufPos != 0)
	{
		MessageDialog("Info 1", 0, 0);
		DeAllocateMemMessageBuf();
	}

#endif
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RemoveDesignLayer(int32 Layer)
{
	DesignRecord OldDesign;
	AreaFillRecord *AreaFill;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	int32 cnt;
	char str[MAX_LENGTH_STRING];
	int32 MoveTopObjectsToBottom;

	strcpy(str, SC(499, "This operation can not be undone\r\n\r\n"));
	strcat(str, SC(500, "Are you sure to remove this layer ?"));

	if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OKCANCEL) != IDOK)
		return -1;

	memmove(&OldDesign, &Design, sizeof(DesignRecord));

	if (Design.NrBoardLayers == 1)
		return -1;

	if (Layer >= Design.NrBoardLayers)
		return -1;

	if (Layer < Design.NrBoardLayers - 1)
	{
		memmove(&Design.NrHorTraces[Layer], &Design.NrHorTraces[Layer + 1],
		        (Design.NrBoardLayers - Layer - 1) * sizeof(int32));
		memmove(&Design.NrVerTraces[Layer], &Design.NrVerTraces[Layer + 1],
		        (Design.NrBoardLayers - Layer - 1) * sizeof(int32));
		memmove(&Design.NrDiag1Traces[Layer], &Design.NrDiag1Traces[Layer + 1],
		        (Design.NrBoardLayers - Layer - 1) * sizeof(int32));
		memmove(&Design.NrDiag2Traces[Layer], &Design.NrDiag2Traces[Layer + 1],
		        (Design.NrBoardLayers - Layer - 1) * sizeof(int32));
	}

// *******************************************************************************************************
// *******************************************************************************************************

	MoveTopObjectsToBottom = 0;

	if ((Design.NrBoardLayers == 2) && (Layer == 0))
	{	// Remove bottom layer, and move top layer objects to bottom
		MoveTopObjectsToBottom = 1;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (((ObjectLine->Layer > Layer) && (ObjectLine->Layer < 32))
			        || ((ObjectLine->Layer > Layer + ROUTING_KEEPOUT_LAYER)
			            && (ObjectLine->Layer < ROUTING_KEEPOUT_LAYER + 32)))
				ObjectLine->Layer--;
			else
			{
				if ((ObjectLine->Layer == Layer) || (ObjectLine->Layer == Layer + ROUTING_KEEPOUT_LAYER))
				{
					ObjectLine->Info |= OBJECT_NOT_VISIBLE;
					ObjectLine->AddNr = 0;
					ObjectLine->DeleteNr = 0;
				}
			}

			if (MoveTopObjectsToBottom)
			{
				switch (ObjectLine->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectLine->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_TOP:
					ObjectLine->Layer = SOLD_MASK_BOTTOM;
					break;

				case SILKSCREEN_TOP:
					ObjectLine->Layer = SILKSCREEN_BOTTOM;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (((ObjectRect->Layer > Layer) && (ObjectRect->Layer < 32))
			        || ((ObjectRect->Layer > Layer + ROUTING_KEEPOUT_LAYER)
			            && (ObjectRect->Layer < ROUTING_KEEPOUT_LAYER + 32)))
				ObjectRect->Layer--;
			else
			{
				if ((ObjectRect->Layer == Layer) || (ObjectRect->Layer == Layer + ROUTING_KEEPOUT_LAYER))
				{
					ObjectRect->Info |= OBJECT_NOT_VISIBLE;
					ObjectRect->AddNr = 0;
					ObjectRect->DeleteNr = 0;
				}
			}

			if (MoveTopObjectsToBottom)
			{
				switch (ObjectRect->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectRect->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_TOP:
					ObjectRect->Layer = SOLD_MASK_BOTTOM;
					break;

				case SILKSCREEN_TOP:
					ObjectRect->Layer = SILKSCREEN_BOTTOM;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (((ObjectArc->Layer > Layer) && (ObjectArc->Layer < 32))
			        || ((ObjectArc->Layer > Layer + ROUTING_KEEPOUT_LAYER)
			            && (ObjectArc->Layer < ROUTING_KEEPOUT_LAYER + 32)))
				ObjectArc->Layer--;
			else
			{
				if ((ObjectArc->Layer == Layer) || (ObjectArc->Layer == Layer + ROUTING_KEEPOUT_LAYER))
				{
					ObjectArc->Info |= OBJECT_NOT_VISIBLE;
					ObjectArc->AddNr = 0;
					ObjectArc->DeleteNr = 0;
				}
			}

			if (MoveTopObjectsToBottom)
			{
				switch (ObjectArc->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectArc->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_TOP:
					ObjectArc->Layer = SOLD_MASK_BOTTOM;
					break;

				case SILKSCREEN_TOP:
					ObjectArc->Layer = SILKSCREEN_BOTTOM;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((ObjectText2->Layer > Layer) && (ObjectText2->Layer < 32))
				ObjectText2->Layer--;
			else
			{
				if (ObjectText2->Layer == Layer)
				{
					ObjectText2->Info |= OBJECT_NOT_VISIBLE;
					ObjectText2->AddNr = 0;
					ObjectText2->DeleteNr = 0;
				}
			}

			if (MoveTopObjectsToBottom)
			{
				switch (ObjectText2->Layer)
				{
				case SILKSCREEN_TOP:
					ObjectText2->Layer = SILKSCREEN_BOTTOM;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (((ObjectPolygon->Layer > Layer) && (ObjectPolygon->Layer < 32))
			        || ((ObjectPolygon->Layer > Layer + ROUTING_KEEPOUT_LAYER)
			            && (ObjectPolygon->Layer < ROUTING_KEEPOUT_LAYER + 32)))
				ObjectPolygon->Layer--;
			else
			{
				if ((ObjectPolygon->Layer == Layer) || (ObjectPolygon->Layer == Layer + ROUTING_KEEPOUT_LAYER))
				{
					ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
					ObjectPolygon->AddNr = 0;
					ObjectPolygon->DeleteNr = 0;
				}
			}

			if (MoveTopObjectsToBottom)
			{
				switch (ObjectPolygon->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectPolygon->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_TOP:
					ObjectPolygon->Layer = SOLD_MASK_BOTTOM;
					break;

				case SILKSCREEN_TOP:
					ObjectPolygon->Layer = SILKSCREEN_BOTTOM;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (AreaFill->Layer == Layer)
			{
				AreaFill->Info |= OBJECT_NOT_VISIBLE;
				AreaFill->AddNr = 0;
				AreaFill->DeleteNr = 0;
			}
			else
			{
				if (AreaFill->Layer > Layer)
					AreaFill->Layer--;
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	Design.NrBoardLayers--;

	for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
	{
		if (Design.NrHorTraces[cnt] > MaxNrHorTraces[cnt])
			AllocateMemHorTraces(cnt, Design.NrHorTraces[cnt] + 1000);

		if (Design.NrVerTraces[cnt] > MaxNrVerTraces[cnt])
			AllocateMemVerTraces(cnt, Design.NrVerTraces[cnt] + 1000);

		if (Design.NrDiag1Traces[cnt] > MaxNrDiag1Traces[cnt])
			AllocateMemDiag1Traces(cnt, Design.NrDiag1Traces[cnt] + 1000);

		if (Design.NrDiag2Traces[cnt] > MaxNrDiag2Traces[cnt])
			AllocateMemDiag2Traces(cnt, Design.NrDiag2Traces[cnt] + 1000);
	}

	if (Layer < OldDesign.NrBoardLayers - 1)
	{
		for (cnt = Layer; cnt < OldDesign.NrBoardLayers - 1; cnt++)
		{
			memmove(HorTraces[cnt], HorTraces[cnt + 1], OldDesign.NrHorTraces[cnt + 1] * sizeof(TraceRecord));
			memmove(VerTraces[cnt], VerTraces[cnt + 1], OldDesign.NrVerTraces[cnt + 1] * sizeof(TraceRecord));
			memmove(Diag1Traces[cnt], Diag1Traces[cnt + 1], OldDesign.NrDiag1Traces[cnt + 1] * sizeof(TraceRecord));
			memmove(Diag2Traces[cnt], Diag2Traces[cnt + 1], OldDesign.NrDiag2Traces[cnt + 1] * sizeof(TraceRecord));
		}
	}

	DeAllocateMemDiag2Traces(OldDesign.NrBoardLayers - 1);
	DeAllocateMemDiag1Traces(OldDesign.NrBoardLayers - 1);
	DeAllocateMemVerTraces(OldDesign.NrBoardLayers - 1);
	DeAllocateMemHorTraces(OldDesign.NrBoardLayers - 1);
	Design.NrHorTraces[OldDesign.NrBoardLayers - 1] = 0;
	Design.NrVerTraces[OldDesign.NrBoardLayers - 1] = 0;
	Design.NrDiag1Traces[OldDesign.NrBoardLayers - 1] = 0;
	Design.NrDiag2Traces[OldDesign.NrBoardLayers - 1] = 0;

	NrErrorObjects = 0;
	CurrentDrawingLayer = min(CurrentDrawingLayer, Design.NrBoardLayers - 1);
	SetLayerColors();
	MakeCheckMenu();
	RePaint();
	DataBaseChanged = 1;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddDesignLayer(int32 Layer)
{
	DesignRecord OldDesign;
	AreaFillRecord *AreaFill;
	int32 cnt;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	char str[MAX_LENGTH_STRING];

	strcpy(str, SC(499, "This operation can not be undone\r\n\r\n"));
	strcat(str, SC(501, "Are you sure to add this layer ?"));

	if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OKCANCEL) != IDOK)
		return -1;

	memmove(&OldDesign, &Design, sizeof(DesignRecord));

	if (Layer > Design.NrBoardLayers)
		return -1;

	if (Design.NrBoardLayers > 16)
		return -1;

	if (Layer < Design.NrBoardLayers)
	{
		memmove(&Design.NrHorTraces[Layer + 1], &Design.NrHorTraces[Layer],
		        (Design.NrBoardLayers - Layer) * sizeof(int32));
		memmove(&Design.NrVerTraces[Layer + 1], &Design.NrVerTraces[Layer],
		        (Design.NrBoardLayers - Layer) * sizeof(int32));
		memmove(&Design.NrDiag1Traces[Layer + 1], &Design.NrDiag1Traces[Layer],
		        (Design.NrBoardLayers - Layer) * sizeof(int32));
		memmove(&Design.NrDiag2Traces[Layer + 1], &Design.NrDiag2Traces[Layer],
		        (Design.NrBoardLayers - Layer) * sizeof(int32));
	}

	Design.NrBoardLayers++;

	if (Layer < Design.NrBoardLayers - 1)
	{
		for (cnt = Layer; cnt < Design.NrBoardLayers; cnt++)
		{
			if (Design.NrHorTraces[cnt] > MaxNrHorTraces[cnt])
				AllocateMemHorTraces(cnt, Design.NrHorTraces[cnt] + 1000);

			if (Design.NrVerTraces[cnt] > MaxNrVerTraces[cnt])
				AllocateMemVerTraces(cnt, Design.NrVerTraces[cnt] + 1000);

			if (Design.NrDiag1Traces[cnt] > MaxNrDiag1Traces[cnt])
				AllocateMemDiag1Traces(cnt, Design.NrDiag1Traces[cnt] + 1000);

			if (Design.NrDiag2Traces[cnt] > MaxNrDiag2Traces[cnt])
				AllocateMemDiag2Traces(cnt, Design.NrDiag2Traces[cnt] + 1000);
		}

		for (cnt = OldDesign.NrBoardLayers - 1; cnt >= Layer; cnt--)
		{
			memmove(HorTraces[cnt + 1], HorTraces[cnt], OldDesign.NrHorTraces[cnt] * sizeof(TraceRecord));
			memmove(VerTraces[cnt + 1], VerTraces[cnt], OldDesign.NrVerTraces[cnt] * sizeof(TraceRecord));
			memmove(Diag1Traces[cnt + 1], Diag1Traces[cnt], OldDesign.NrDiag1Traces[cnt] * sizeof(TraceRecord));
			memmove(Diag2Traces[cnt + 1], Diag2Traces[cnt], OldDesign.NrDiag2Traces[cnt] * sizeof(TraceRecord));
		}

// *******************************************************************************************************
// *******************************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (((ObjectLine->Layer >= Layer) && (ObjectLine->Layer < 32))
				        || ((ObjectLine->Layer >= Layer + ROUTING_KEEPOUT_LAYER)
				            && (ObjectLine->Layer < ROUTING_KEEPOUT_LAYER + 32)))
					ObjectLine->Layer++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (((ObjectRect->Layer >= Layer) && (ObjectRect->Layer < 32))
				        || ((ObjectRect->Layer >= Layer + ROUTING_KEEPOUT_LAYER)
				            && (ObjectRect->Layer < ROUTING_KEEPOUT_LAYER + 32)))
					ObjectRect->Layer++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (((ObjectArc->Layer >= Layer) && (ObjectArc->Layer < 32))
				        || ((ObjectArc->Layer >= Layer + ROUTING_KEEPOUT_LAYER)
				            && (ObjectArc->Layer < ROUTING_KEEPOUT_LAYER + 32)))
					ObjectArc->Layer++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if ((ObjectText2->Layer >= Layer) && (ObjectText2->Layer < 32))
					ObjectText2->Layer++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (((ObjectPolygon->Layer >= Layer) && (ObjectPolygon->Layer < 32))
				        || ((ObjectPolygon->Layer >= Layer + ROUTING_KEEPOUT_LAYER)
				            && (ObjectPolygon->Layer < ROUTING_KEEPOUT_LAYER + 32)))
					ObjectPolygon->Layer++;
			}
		}

// *******************************************************************************************************
// *******************************************************************************************************
	}
	else
	{
		AllocateMemHorTraces(Layer, 1000);
		AllocateMemVerTraces(Layer, 1000);
		AllocateMemDiag1Traces(Layer, 1000);
		AllocateMemDiag2Traces(Layer, 1000);
	}

	Design.NrHorTraces[Layer] = 0;
	Design.NrVerTraces[Layer] = 0;
	Design.NrDiag1Traces[Layer] = 0;
	Design.NrDiag2Traces[Layer] = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (AreaFill->Layer >= Layer)
				AreaFill->Layer++;
		}
	}

	MakeCheckMenu();
	SetLayerColors();
	RePaint();
	DataBaseChanged = 1;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SwitchDesignLayer(int32 Layer1, int32 Layer2)
{

	DesignRecord OldDesign;
	AreaFillRecord *AreaFill;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	int32 cnt;
	char str[MAX_LENGTH_STRING];
	int32 SwapObjectsTopBottom;

	if (Design.NrBoardLayers == 1)
		return -1;

	strcpy(str, SC(499, "This operation can not be undone\r\n\r\n"));
	strcat(str, SC(502, "Are you sure to switch these two layers ?"));

	if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OKCANCEL) != IDOK)
		return -1;

	memmove(&OldDesign, &Design, sizeof(DesignRecord));

	Design.NrHorTraces[Layer1] = OldDesign.NrHorTraces[Layer2];
	Design.NrHorTraces[Layer2] = OldDesign.NrHorTraces[Layer1];
	Design.NrVerTraces[Layer1] = OldDesign.NrVerTraces[Layer2];
	Design.NrVerTraces[Layer2] = OldDesign.NrVerTraces[Layer1];
	Design.NrDiag1Traces[Layer1] = OldDesign.NrDiag1Traces[Layer2];
	Design.NrDiag1Traces[Layer2] = OldDesign.NrDiag1Traces[Layer1];
	Design.NrDiag2Traces[Layer1] = OldDesign.NrDiag2Traces[Layer2];
	Design.NrDiag2Traces[Layer2] = OldDesign.NrDiag2Traces[Layer1];

	if (Design.NrHorTraces[Layer1] > MaxNrHorTraces[Layer1])
		AllocateMemHorTraces(Layer1, Design.NrHorTraces[Layer1] + 1000);

	if (Design.NrVerTraces[Layer1] > MaxNrVerTraces[Layer1])
		AllocateMemVerTraces(Layer1, Design.NrVerTraces[Layer1] + 1000);

	if (Design.NrDiag1Traces[Layer1] > MaxNrDiag1Traces[Layer1])
		AllocateMemDiag1Traces(Layer1, Design.NrDiag1Traces[Layer1] + 1000);

	if (Design.NrDiag2Traces[Layer1] > MaxNrDiag2Traces[Layer1])
		AllocateMemDiag2Traces(Layer1, Design.NrDiag2Traces[Layer1] + 1000);

	if (Design.NrHorTraces[Layer2] > MaxNrHorTraces[Layer2])
		AllocateMemHorTraces(Layer2, Design.NrHorTraces[Layer2] + 1000);

	if (Design.NrVerTraces[Layer2] > MaxNrVerTraces[Layer2])
		AllocateMemVerTraces(Layer2, Design.NrVerTraces[Layer2] + 1000);

	if (Design.NrDiag1Traces[Layer2] > MaxNrDiag1Traces[Layer2])
		AllocateMemDiag1Traces(Layer2, Design.NrDiag1Traces[Layer2] + 1000);

	if (Design.NrDiag2Traces[Layer2] > MaxNrDiag2Traces[Layer2])
		AllocateMemDiag2Traces(Layer2, Design.NrDiag2Traces[Layer2] + 1000);

	XchangeMem((uint8 *) HorTraces[Layer1], (uint8 *) HorTraces[Layer2],
	           max(OldDesign.NrHorTraces[Layer1], OldDesign.NrHorTraces[Layer2]) * sizeof(TraceRecord));
	XchangeMem((uint8 *) VerTraces[Layer1], (uint8 *) VerTraces[Layer2],
	           max(OldDesign.NrVerTraces[Layer1], OldDesign.NrVerTraces[Layer2]) * sizeof(TraceRecord));
	XchangeMem((uint8 *) Diag1Traces[Layer1], (uint8 *) Diag1Traces[Layer2],
	           max(OldDesign.NrDiag1Traces[Layer1], OldDesign.NrDiag1Traces[Layer2]) * sizeof(TraceRecord));
	XchangeMem((uint8 *) Diag2Traces[Layer1], (uint8 *) Diag2Traces[Layer2],
	           max(OldDesign.NrDiag2Traces[Layer1], OldDesign.NrDiag2Traces[Layer2]) * sizeof(TraceRecord));

// *******************************************************************************************************
	SwapObjectsTopBottom = 0;

	if (((Layer1 == 0) && (Layer2 == Design.NrBoardLayers - 1))
	        || ((Layer2 == 0) && (Layer1 == Design.NrBoardLayers - 1)))
		SwapObjectsTopBottom = 1;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectLine->Layer == Layer1)
				ObjectLine->Layer = Layer2;
			else
			{
				if (ObjectLine->Layer == Layer2)
					ObjectLine->Layer = Layer1;
			}

			if (ObjectLine->Layer == Layer1 + ROUTING_KEEPOUT_LAYER)
				ObjectLine->Layer = Layer2 + ROUTING_KEEPOUT_LAYER;
			else
			{
				if (ObjectLine->Layer == Layer2 + ROUTING_KEEPOUT_LAYER)
					ObjectLine->Layer = Layer1 + ROUTING_KEEPOUT_LAYER;
			}

			if (SwapObjectsTopBottom)
			{
				switch (ObjectLine->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectLine->Layer = SOLD_MASK_BOTTOM;
					break;

				case SOLD_MASK_BOTTOM:
					ObjectLine->Layer = SOLD_MASK_TOP;
					break;

				case PASTE_MASK_TOP:
					ObjectLine->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_BOTTOM:
					ObjectLine->Layer = SOLD_MASK_TOP;
					break;

				case SILKSCREEN_TOP:
					ObjectLine->Layer = SILKSCREEN_BOTTOM;
					break;

				case SILKSCREEN_BOTTOM:
					ObjectLine->Layer = SILKSCREEN_TOP;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectRect->Layer == Layer1)
				ObjectRect->Layer = Layer2;
			else
			{
				if (ObjectRect->Layer == Layer2)
					ObjectRect->Layer = Layer1;
			}

			if (ObjectRect->Layer == Layer1 + ROUTING_KEEPOUT_LAYER)
				ObjectRect->Layer = Layer2 + ROUTING_KEEPOUT_LAYER;
			else
			{
				if (ObjectRect->Layer == Layer2 + ROUTING_KEEPOUT_LAYER)
					ObjectRect->Layer = Layer1 + ROUTING_KEEPOUT_LAYER;
			}

			if (SwapObjectsTopBottom)
			{
				switch (ObjectRect->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectRect->Layer = SOLD_MASK_BOTTOM;
					break;

				case SOLD_MASK_BOTTOM:
					ObjectRect->Layer = SOLD_MASK_TOP;
					break;

				case PASTE_MASK_TOP:
					ObjectRect->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_BOTTOM:
					ObjectRect->Layer = SOLD_MASK_TOP;
					break;

				case SILKSCREEN_TOP:
					ObjectRect->Layer = SILKSCREEN_BOTTOM;
					break;

				case SILKSCREEN_BOTTOM:
					ObjectRect->Layer = SILKSCREEN_TOP;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectArc->Layer == Layer1)
				ObjectArc->Layer = Layer2;
			else
			{
				if (ObjectArc->Layer == Layer2)
					ObjectArc->Layer = Layer1;
			}

			if (ObjectArc->Layer == Layer1 + ROUTING_KEEPOUT_LAYER)
				ObjectArc->Layer = Layer2 + ROUTING_KEEPOUT_LAYER;
			else
			{
				if (ObjectArc->Layer == Layer2 + ROUTING_KEEPOUT_LAYER)
					ObjectArc->Layer = Layer1 + ROUTING_KEEPOUT_LAYER;
			}

			if (SwapObjectsTopBottom)
			{
				switch (ObjectArc->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectArc->Layer = SOLD_MASK_BOTTOM;
					break;

				case SOLD_MASK_BOTTOM:
					ObjectArc->Layer = SOLD_MASK_TOP;
					break;

				case PASTE_MASK_TOP:
					ObjectArc->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_BOTTOM:
					ObjectArc->Layer = SOLD_MASK_TOP;
					break;

				case SILKSCREEN_TOP:
					ObjectArc->Layer = SILKSCREEN_BOTTOM;
					break;

				case SILKSCREEN_BOTTOM:
					ObjectArc->Layer = SILKSCREEN_TOP;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectText2->Layer == Layer1)
				ObjectText2->Layer = Layer2;
			else
			{
				if (ObjectText2->Layer == Layer2)
					ObjectText2->Layer = Layer1;
			}

			if (SwapObjectsTopBottom)
			{
				switch (ObjectText2->Layer)
				{
				case SILKSCREEN_TOP:
					ObjectText2->Layer = SILKSCREEN_BOTTOM;
					break;

				case SILKSCREEN_BOTTOM:
					ObjectText2->Layer = SILKSCREEN_TOP;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectPolygon->Layer == Layer1)
				ObjectPolygon->Layer = Layer2;
			else
			{
				if (ObjectPolygon->Layer == Layer2)
					ObjectPolygon->Layer = Layer1;
			}

			if (ObjectPolygon->Layer == Layer1 + ROUTING_KEEPOUT_LAYER)
				ObjectPolygon->Layer = Layer2 + ROUTING_KEEPOUT_LAYER;
			else
			{
				if (ObjectPolygon->Layer == Layer2 + ROUTING_KEEPOUT_LAYER)
					ObjectPolygon->Layer = Layer1 + ROUTING_KEEPOUT_LAYER;
			}

			if (SwapObjectsTopBottom)
			{
				switch (ObjectPolygon->Layer)
				{
				case SOLD_MASK_TOP:
					ObjectPolygon->Layer = SOLD_MASK_BOTTOM;
					break;

				case SOLD_MASK_BOTTOM:
					ObjectPolygon->Layer = SOLD_MASK_TOP;
					break;

				case PASTE_MASK_TOP:
					ObjectPolygon->Layer = SOLD_MASK_BOTTOM;
					break;

				case PASTE_MASK_BOTTOM:
					ObjectPolygon->Layer = SOLD_MASK_TOP;
					break;

				case SILKSCREEN_TOP:
					ObjectPolygon->Layer = SILKSCREEN_BOTTOM;
					break;

				case SILKSCREEN_BOTTOM:
					ObjectPolygon->Layer = SILKSCREEN_TOP;
					break;
				}
			}
		}
	}

// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (AreaFill->Layer == Layer1)
				AreaFill->Layer = Layer2;
			else
			{
				if (AreaFill->Layer == Layer2)
					AreaFill->Layer = Layer1;
			}
		}
	}

	SetLayerColors();
	RePaint();
	DataBaseChanged = 1;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MoveCopyObjectsLayers(int32 SourceLayer, int32 DestinationLayer, int32 Action, int32 mode)
{
	int32 cnt, cnt2, CompMode, TempLastActionNr;
	int32 OkToCopy;
	CompRecord *Comp;
	ObjectRecord *Object;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	uint8 PolygonBuf[10204];
	PolygonRecord *DrawPolygon;

	DrawPolygon = (PolygonRecord *) & PolygonBuf;

	/*
	#define SOLD_MASK_BOTTOM                        100
	#define SOLD_MASK_TOP                           101
	#define PASTE_MASK_BOTTOM                       200
	#define PASTE_MASK_TOP                          201
	#define INFO_LAYER                              1500
	#define BOARD_OUTLINE_LAYER                     1600
	#define SILKSCREEN_BOTTOM                       2000
	#define SILKSCREEN_TOP                          2001
	#define PLACEMENT_OUTLINE_LAYER                 3000
	#define PLACEMENT_OUTLINE_LAYER2                3001
	#define COMP_OUTLINE_LAYER                      4000
	#define PCB_TOP                                 5000  -> Design.NrBoardLayers-1
	#define PCB_BOTTOM                              5500  -> 0
	#define DRILL_LAYER                             6000
	#define DRILL_UNPLATED_LAYER                    6500
	#define SPECIALS_LAYER                          7000
	#define INFO_LAYER2                             7500
	#define INFO_LAYER3                             8000
	#define INFO_LAYER4                             8500

	*/
	switch (DestinationLayer)
	{
	case SILKSCREEN_TOP:
	case SILKSCREEN_BOTTOM:
	case INFO_LAYER:
	case INFO_LAYER2:
	case INFO_LAYER3:
	case INFO_LAYER4:
	case BOARD_OUTLINE_LAYER:
	case SOLD_MASK_BOTTOM:
	case SOLD_MASK_TOP:
	case PASTE_MASK_BOTTOM:
	case PASTE_MASK_TOP:
	case DRILL_LAYER:
	case DRILL_UNPLATED_LAYER:
		break;

	default:
		if (IsRoutingKeepoutLayer(DestinationLayer))
		{
		}
		else
		{
			if (DestinationLayer < Design.NrBoardLayers)
			{
			}
			else
				return -1;
		}
	}

	TempLastActionNr = LastActionNr - 1;

	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	memset(&NewObjectRect, 0, sizeof(ObjectRectRecord));
	memset(&NewObjectCircle, 0, sizeof(ObjectCircleRecord));
	memset(&NewObjectArc, 0, sizeof(ObjectArcRecord));
	memset(&NewObjectText2, 0, sizeof(ObjectTextRecord2));
	memset(&NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));
	NewObjectLine.Layer = DestinationLayer;
	NewObjectRect.Layer = DestinationLayer;
	NewObjectCircle.Layer = DestinationLayer;
	NewObjectArc.Layer = DestinationLayer;
	NewObjectText2.Layer = DestinationLayer;
	NewObjectPolygon.Layer = DestinationLayer;
	NewObjectLine.Info = OBJECT_SELECTED | 3;
	NewObjectRect.Info = OBJECT_SELECTED;
	NewObjectArc.Info = OBJECT_SELECTED;
	NewObjectText2.Info = OBJECT_SELECTED;
	NewObjectPolygon.Info = OBJECT_SELECTED;
	CompMode = 0;

	if (((SourceLayer == PLACEMENT_OUTLINE_TOP) || (SourceLayer == PLACEMENT_OUTLINE_BOTTOM)) && ((mode & 1) == 1))
	{
		CompMode = 1;
		NrObjects = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ShapePlacementOutLineToObject(Comp, 0.0, 0.0, 0);
		}
	}

	if (((SourceLayer == SILKSCREEN_TOP) || (SourceLayer == SILKSCREEN_BOTTOM)) && ((mode & 1) == 1))
	{
		CompMode = 1;
		NrObjects = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ShapeCompSilkScreenToObject(Comp, 0.0, 0.0, 0);
				ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, -1, 5);
			}
		}
	}

	if ((IsRoutingKeepoutLayer(SourceLayer)) && ((mode & 1) == 1))
	{
		CompMode = 1;
		NrObjects = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, -1, 4);
		}
	}

	if ((SourceLayer == COMP_OUTLINE_LAYER) && ((mode & 1) == 1))
	{
		CompMode = 1;
		NrObjects = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ShapeCompOutLineToObject(Comp, 0.0, 0.0, 0);
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	if (CompMode == 1)
	{
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (InRange(Object->Clearance, 0.0))
				Object->Clearance = Design.SilkScreenWidth;

			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
				NewObjectLine.X1 = (float) Object->x1;
				NewObjectLine.Y1 = (float) Object->y1;
				NewObjectLine.X2 = (float) Object->x2;
				NewObjectLine.Y2 = (float) Object->y2;
				NewObjectLine.LineThickNess = (float) Object->Thickness;

				if (DestinationLayer < 32)
				{
					NewObjectLine.NetNr = -1;

					if (NewObjectLine.LineThickNess < 2.0)
						NewObjectLine.LineThickNess = Design.StandardTraceWidth;
				}

				if (!IsRoutingKeepoutLayer(DestinationLayer))
					AddObjectLine(&NewObjectLine);

				break;

			case OBJECT_RECT:
				if ((!IsRoutingKeepoutLayer(DestinationLayer)) || ((Object->Info & OBJECT_FILLED) != 0))
				{
					NewObjectRect.CentreX = (float) Object->x1;
					NewObjectRect.CentreY = (float) Object->y1;
					NewObjectRect.Width = (float) Object->x2;
					NewObjectRect.Height = (float) Object->y2;
					NewObjectRect.LineThickNess = (float) Object->Thickness;

					if (NewObjectRect.LineThickNess == 0.0)
						NewObjectRect.Info |= OBJECT_FILLED;

					switch (DestinationLayer)
					{
					case BOARD_OUTLINE_LAYER:
						if (NewObjectRect.LineThickNess == 0.0)
							NewObjectRect.LineThickNess = Design.SilkScreenWidth;

						NewObjectRect.Info &= ~OBJECT_FILLED;
						break;

					case SOLD_MASK_BOTTOM:
					case SOLD_MASK_TOP:
					case PASTE_MASK_BOTTOM:
					case PASTE_MASK_TOP:
						NewObjectRect.LineThickNess = 0.0;
						NewObjectRect.Info |= OBJECT_FILLED;
						break;

					default:
						if (IsRoutingKeepoutLayer(DestinationLayer))
						{
							NewObjectRect.LineThickNess = 0.0;
							NewObjectRect.Info |= OBJECT_FILLED;
						}

						break;
					}

					AddObjectRect(&NewObjectRect);
				}

				break;

			case OBJECT_ARC:
				if ((!IsRoutingKeepoutLayer(DestinationLayer)) || ((Object->Info & OBJECT_FILLED) != 0))
				{
					NewObjectArc.CentreX = (float) Object->x1;
					NewObjectArc.CentreY = (float) Object->y1;
					NewObjectArc.Width = (float) Object->x2;
					NewObjectArc.Height = (float) Object->x2;
					NewObjectArc.StartDiffX = (float) Object->x3;
					NewObjectArc.StartDiffY = (float) Object->y3;
					NewObjectArc.EndDiffX = (float) Object->x4;
					NewObjectArc.EndDiffY = (float) Object->y4;
					NewObjectArc.LineThickNess = (float) Object->Thickness;

					if (NewObjectArc.LineThickNess == 0.0)
						NewObjectArc.Info |= OBJECT_FILLED;

					switch (DestinationLayer)
					{
					case BOARD_OUTLINE_LAYER:
						if (NewObjectArc.LineThickNess == 0.0)
							NewObjectArc.LineThickNess = Design.SilkScreenWidth;

						NewObjectArc.Info &= ~OBJECT_FILLED;
						break;

					case SOLD_MASK_BOTTOM:
					case SOLD_MASK_TOP:
					case PASTE_MASK_BOTTOM:
					case PASTE_MASK_TOP:
						break;

					default:
						if (IsRoutingKeepoutLayer(DestinationLayer))
						{
							NewObjectArc.LineThickNess = 0.0;
							NewObjectArc.Info |= OBJECT_FILLED;
						}

						break;
					}

					AddObjectArc(&NewObjectArc);
				}

				break;

			case OBJECT_TEXT:
			case OBJECT_TEXT2:
				NewObjectText2.X = (float) Object->x1;
				NewObjectText2.Y = (float) Object->y1;
				NewObjectText2.FontHeight = (float) Object->x2;
				NewObjectText2.LineThickNess = (float) Object->Thickness;

				if (Object->Mirror == 0)
					NewObjectText2.TextMode &= ~0x10;
				else
					NewObjectText2.TextMode |= 0x10;

				NewObjectText2.Rotation = (float) Object->RotationAngle;
				strncpy(NewObjectText2.Text, (LPSTR) Object->TraceNr, sizeof(NewObjectText2.Text) - 1);

				switch (DestinationLayer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
				case INFO_LAYER:
				case INFO_LAYER2:
				case INFO_LAYER3:
				case INFO_LAYER4:
					AddObjectText2(&NewObjectText2);
					break;
				}

				break;

			case OBJECT_POLYGON:
				MakePolygonFromObject(Object, DrawPolygon, 0.0, 0.0, 1, 1);
				NewObjectPolygon.NrVertices = min(DrawPolygon->NrVertices, 200);

				for (cnt2 = 0; cnt2 < NewObjectPolygon.NrVertices; cnt2++)
				{
					NewObjectPolygon.Points[cnt2].x = DrawPolygon->Points[cnt2].x;
					NewObjectPolygon.Points[cnt2].y = DrawPolygon->Points[cnt2].y;

					if (Units == 0)
					{
						NewObjectPolygon.Points[cnt2].x += 40 * 2540.0;
						NewObjectPolygon.Points[cnt2].y += 40 * 2540.0;
					}
					else
					{
						NewObjectPolygon.Points[cnt2].x += 100000.0;
						NewObjectPolygon.Points[cnt2].y += 100000.0;
					}
				}

				switch (DestinationLayer)
				{
				case BOARD_OUTLINE_LAYER:
					break;

				default:
					AddObjectPolygon(&NewObjectPolygon);
					break;
				}

				break;
			}
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				Comp->Info &= ~OBJECT_SELECTED;
		}

		PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_OBJECTS, (LPARAM) NULL);
	}
	else
	{
// *******************************************************************************************************
// *******************************************************************************************************
		OkToCopy = 1;

		if ((IsRoutingKeepoutLayer(DestinationLayer)) || (DestinationLayer == DRILL_LAYER)
		        || (DestinationLayer == DRILL_UNPLATED_LAYER))
			OkToCopy = 0;

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectLine->AddNr <= TempLastActionNr))
			{
				if ((OkToCopy)
				        && ((((mode & 2) == 0) && (ObjectLine->Layer == SourceLayer))
				            || (((mode & 2) == 2) && (ObjectLine->Layer != DestinationLayer))))
				{
					memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));

					switch (DestinationLayer)
					{
					case SILKSCREEN_TOP:
					case SILKSCREEN_BOTTOM:
					case INFO_LAYER:
					case INFO_LAYER2:
					case INFO_LAYER3:
					case INFO_LAYER4:
						break;

					default:
						NewObjectLine.LineMode = 0;
						break;
					}

					NewObjectLine.Info |= 3;
					NewObjectLine.Layer = DestinationLayer;

					if (DestinationLayer < 32)
					{
						NewObjectLine.NetNr = -1;

						if (NewObjectLine.LineThickNess < 2.0)
							NewObjectLine.LineThickNess = Design.StandardTraceWidth;
					}

					if (Action == 1)
						NewObjectLine.Info &= ~OBJECT_SELECTED;

					AddObjectLine(&NewObjectLine);
					ObjectLine = &((*ObjectLines)[cnt]);
					ObjectLine->Info &= ~OBJECT_SELECTED;

					if (Action == 1)
					{
						ObjectLine->Info |= OBJECT_NOT_VISIBLE;
						ObjectLine->DeleteNr = (int16) LastActionNr;
					}
				}
				else
					ObjectLine->Info &= ~OBJECT_SELECTED;
			}
		}

// *******************************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectRect->AddNr <= TempLastActionNr))
			{
				OkToCopy = 0;

				if ((mode & 2) == 0)
				{
					if (ObjectRect->Layer == SourceLayer)
						OkToCopy = 1;
				}
				else
				{
					OkToCopy = 1;

					if (ObjectRect->Layer == DestinationLayer)
						OkToCopy = 0;

					if (DestinationLayer == DRILL_LAYER)
						OkToCopy = 0;

					if (DestinationLayer == DRILL_UNPLATED_LAYER)
						OkToCopy = 0;

					if ((IsRoutingKeepoutLayer(DestinationLayer)) && ((ObjectRect->Info & OBJECT_FILLED) == 0))
						OkToCopy = 0;
				}

				if (OkToCopy)
				{
					memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));

					if (Action == 0)
					{
					}

					NewObjectRect.Layer = DestinationLayer;

					if ((NewObjectRect.Info & OBJECT_FILLED) != 0)
						NewObjectRect.LineThickNess = 0.0;

					if (DestinationLayer < 32)
					{
						NewObjectRect.NetNr = -1;

						if ((NewObjectRect.Info & OBJECT_FILLED) == 0)
						{
							if (NewObjectRect.LineThickNess < 2.0)
								NewObjectLine.LineThickNess = Design.StandardTraceWidth;
						}
					}

					if (DestinationLayer == BOARD_OUTLINE_LAYER)
					{
						NewObjectRect.Info &= ~OBJECT_FILLED;

						if (NewObjectRect.LineThickNess < 2.0)
							NewObjectRect.LineThickNess = Design.SilkScreenWidth;
					}

					if (IsRoutingKeepoutLayer(DestinationLayer))
						NewObjectRect.NetNr = -2;

					if (Action == 1)
						NewObjectRect.Info &= ~OBJECT_SELECTED;

					AddObjectRect(&NewObjectRect);
					ObjectRect = &((*ObjectRects)[cnt]);
					ObjectRect->Info &= ~OBJECT_SELECTED;

					if (Action == 1)
					{
						ObjectRect->Info |= OBJECT_NOT_VISIBLE;
						ObjectRect->DeleteNr = (int16) LastActionNr;
					}
				}
				else
					ObjectRect->Info &= ~OBJECT_SELECTED;
			}
		}

// *******************************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectArc->AddNr <= TempLastActionNr))
			{
				OkToCopy = 0;

				if ((mode & 2) == 0)
				{
					if (ObjectArc->Layer == SourceLayer)
						OkToCopy = 1;
				}
				else
				{
					OkToCopy = 1;

					if (ObjectArc->Layer == DestinationLayer)
						OkToCopy = 0;

					if ((DestinationLayer == DRILL_LAYER) || (DestinationLayer == DRILL_UNPLATED_LAYER))
					{
						if ((ObjectArc->Info & OBJECT_FILLED) == 0)
							OkToCopy = 0;
					}

					if ((IsRoutingKeepoutLayer(DestinationLayer)) && ((ObjectArc->Info & OBJECT_FILLED) == 0))
						OkToCopy = 0;
				}

				if (OkToCopy)
				{
					memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));

					if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
						NewObjectArc.Info |= OBJECT_FILLED;

					NewObjectArc.Layer = DestinationLayer;

					if ((NewObjectArc.Info & OBJECT_FILLED) != 0)
						NewObjectArc.LineThickNess = 0.0;

					if ((DestinationLayer == DRILL_LAYER) || (DestinationLayer == DRILL_UNPLATED_LAYER))
					{
						NewObjectArc.Info |= OBJECT_FILLED;
						NewObjectArc.LineThickNess = 0.0;
					}

					if (DestinationLayer == BOARD_OUTLINE_LAYER)
					{
						NewObjectArc.Info &= ~OBJECT_FILLED;

						if (NewObjectArc.LineThickNess < 2.0)
							NewObjectArc.LineThickNess = Design.SilkScreenWidth;
					}

					if (DestinationLayer < 32)
					{
						NewObjectArc.NetNr = -1;

						if ((NewObjectArc.Info & OBJECT_FILLED) == 0)
						{
							if (NewObjectArc.LineThickNess < 2.0)
								NewObjectArc.LineThickNess = Design.StandardTraceWidth;
						}
					}

					if (IsRoutingKeepoutLayer(DestinationLayer))
						NewObjectArc.NetNr = -2;

					if (Action == 1)
						NewObjectArc.Info &= ~OBJECT_SELECTED;

					AddObjectArc(&NewObjectArc);
					ObjectArc = &((*ObjectArcs)[cnt]);
					ObjectArc->Info &= ~OBJECT_SELECTED;

					if (Action == 1)
					{
						ObjectArc->Info |= OBJECT_NOT_VISIBLE;
						ObjectArc->DeleteNr = (int16) LastActionNr;
					}
				}
				else
					ObjectArc->Info &= ~OBJECT_SELECTED;
			}
		}

// *******************************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectPolygon->AddNr <= TempLastActionNr))
			{
				if ((((mode & 2) == 0) && (ObjectPolygon->Layer == SourceLayer))
				        || (((mode & 2) == 2) && (ObjectPolygon->Layer != DestinationLayer)))
				{
					memmove(&NewObjectPolygon, ObjectPolygon, sizeof(ObjectPolygonRecord));
					ObjectPolygon->Info &= ~OBJECT_SELECTED;

					if (DestinationLayer != BOARD_OUTLINE_LAYER)
					{
						NewObjectPolygon.Layer = DestinationLayer;

						if (DestinationLayer < 32)
							NewObjectPolygon.NetNr = -1;

						if (IsRoutingKeepoutLayer(DestinationLayer))
							NewObjectPolygon.NetNr = -2;

						if (Action == 1)
							NewObjectPolygon.Info &= ~OBJECT_SELECTED;

						AddObjectPolygon(&NewObjectPolygon);
					}
					else
					{
						NewObjectLine.Layer = DestinationLayer;
						NewObjectLine.LineThickNess = Design.SilkScreenWidth;

						if (Action == 1)
							NewObjectLine.Info &= ~OBJECT_SELECTED;

						for (cnt2 = 0; cnt2 < ObjectPolygon->NrVertices; cnt2++)
						{
							NewObjectLine.X1 = (float) ObjectPolygon->Points[cnt2].x;
							NewObjectLine.Y1 = (float) ObjectPolygon->Points[cnt2].y;

							if (cnt2 < ObjectPolygon->NrVertices - 1)
							{
								NewObjectLine.X2 = (float) ObjectPolygon->Points[cnt2 + 1].x;
								NewObjectLine.Y2 = (float) ObjectPolygon->Points[cnt2 + 1].y;
							}
							else
							{
								NewObjectLine.X2 = (float) ObjectPolygon->Points[0].x;
								NewObjectLine.Y2 = (float) ObjectPolygon->Points[0].y;
							}

							AddObjectLine(&NewObjectLine);
						}
					}

					ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

					if (Action == 1)
					{
						ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
						ObjectPolygon->DeleteNr = (int16) LastActionNr;
					}
				}
				else
					ObjectPolygon->Info &= ~OBJECT_SELECTED;
			}
		}

		OkToCopy = 0;

		switch (DestinationLayer)
		{
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM:
		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
			OkToCopy = 1;
			break;

		default:
			if (DestinationLayer < 32)
				OkToCopy = 1;

			break;
		}

// *******************************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectText2->AddNr <= TempLastActionNr))
			{
				if ((OkToCopy)
				        && ((((mode & 2) == 0) && (ObjectText2->Layer == SourceLayer))
				            || (((mode & 2) == 2) && (ObjectText2->Layer != DestinationLayer))))
				{
					memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
					ObjectText2->Info &= ~OBJECT_SELECTED;
					NewObjectText2.Layer = DestinationLayer;

					switch (DestinationLayer)
					{
					case SILKSCREEN_TOP:
					case INFO_LAYER:
					case INFO_LAYER2:
					case INFO_LAYER3:
					case INFO_LAYER4:
						NewObjectText2.TextMode &= ~0x10;
						break;

					case SILKSCREEN_BOTTOM:
						NewObjectText2.TextMode |= 0x10;
						break;
					}

					if (DestinationLayer < 32)
						NewObjectText2.NetNr = -1;

					if (Action == 1)
						NewObjectText2.Info &= ~OBJECT_SELECTED;

					AddObjectText2(&NewObjectText2);
					ObjectText2 = &((*ObjectTexts2)[cnt]);

					if (Action == 1)
					{
						ObjectText2->Info |= OBJECT_NOT_VISIBLE;
						ObjectText2->DeleteNr = (int16) LastActionNr;
					}
				}
				else
					ObjectText2->Info &= ~OBJECT_SELECTED;
			}
		}
	}

	RePaint();
	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
