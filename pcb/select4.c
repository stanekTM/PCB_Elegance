/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select4.c
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
#include "mainloop.h"
#include "memory.h"
#include "string.h"
#include "select3.h"
#include "select4.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "calc.h"
#include "calc4.h"
#include "calcdef.h"
#include "math.h"
#include "calc2.h"
#include "calc3.h"
#include "graphics.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "toets.h"

int32 SearchConnectionNr = -1;
int32 DisplayInfoCursorX = -1, DisplayInfoCursorY = -1;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 FindLimitTrace(double StartX, double StartY, double EndX, double EndY, double TraceWidth, double TraceClearance,
                     double *NewEndX, double *NewEndY, int32 NetNr, int32 Layer)
{
	double Length, Angle, Step, Length2, TraceX1, TraceY1, TraceX1a, TraceY1a, TraceX2, TraceY2, NewLengthX, NewLengthY,
	       GridStep;
	int32 cnt, cnt2, FoundObjects, res, count, ok;
	int32 Found = 0;
	ObjectRecord TraceObject, *Object4;
#ifdef _DEBUG
	ObjectRecord *FoundObject;
#endif


//  EndX=109.0e5;
//  EndY=43.0e5;

	memset(&TraceObject, 0, sizeof(ObjectRecord));
	TraceObject.Clearance = TraceClearance;
	TraceObject.NetNr = NetNr;
	TraceObject.Layer = Layer;
	ConvNormalCoorToPolar(StartX, StartY, EndX, EndY, &Angle, &Length);
	GridStep = 0.0;
	Step = AdjustToGrid((1000 * 2540), GridSize);
	TraceX1 = StartX;
	TraceY1 = StartY;
	cnt = 0;

	while ((!Found) && (cnt < 200))
	{
		Length2 = (cnt) * Step;
		TraceX1a = TraceX1 + cos(Angle) * Length2;
		TraceY1a = TraceY1 + sin(Angle) * Length2;
		Length2 = (cnt + 1) * Step;
		TraceX2 = TraceX1 + cos(Angle) * Length2;
		TraceY2 = TraceY1 + sin(Angle) * Length2;
		SearchMinX = min(TraceX1a, TraceX2) - TraceWidth * 0.5 - TraceClearance;
		SearchMaxX = max(TraceX1a, TraceX2) + TraceWidth * 0.5 + TraceClearance;
		SearchMinY = min(TraceY1a, TraceY2) - TraceWidth * 0.5 - TraceClearance;
		SearchMaxY = max(TraceY1a, TraceY2) + TraceWidth * 0.5 + TraceClearance;
		FoundObjects = CopyCopperObjectsFromRectWindowToObjects4(Layer, 4);

		for (cnt2 = 0; cnt2 < NrObjects4; cnt2++)
		{
			Object4 = &((*Objects4)[cnt2]);
			FillPositionObject(Object4);

			if (Object4->NetNr == -2)
				ok = 1;
		}

		if (FoundObjects > 0)
		{
			CreateTraceObjectFromSpecialLine(&TraceObject, TraceX1, TraceY1, Length2, Angle, TraceWidth, 0);
#ifdef _DEBUG

			if (TraceObject.ObjectType <= 0)
				ok = 1;

#endif
			res = CheckObjectOverlappedFromObjects(&TraceObject, 4);

			if (res != -1)
			{	// Found overlapped object -> Zoom in on the current section
#ifdef _DEBUG
				FoundObject = &((*Objects4)[res]);
#endif
				Step *= 0.5;
				Length2 -= Step;

				count = 0;

				while ((count < 200) && (Step > GridSize * 0.25))
				{
					Step *= 0.5;
					CreateTraceObjectFromSpecialLine(&TraceObject, TraceX1, TraceY1, Length2, Angle, TraceWidth, 0);
#ifdef _DEBUG

					if (TraceObject.ObjectType <= 0)
						ok = 1;

#endif
					res = CheckObjectOverlappedFromObjects(&TraceObject, 4);

					if (res != -1)
					{	// Found overlapped object -> Zoom in on the current section
#ifdef _DEBUG
						FoundObject = &((*Objects4)[res]);
#endif
						// Trace overlaps an another object
						// Decrement Trace length
						Length2 -= Step;
					}
					else
					{
						// Trace does not overlap an another object
						// Increment Trace length
						Length2 += Step;
					}

					count++;
				}

				if (count == 200)
					return 0;

#ifdef _DEBUG
				CreateTraceObjectFromSpecialLine(&TraceObject, TraceX1, TraceY1, Length2, Angle, TraceWidth, 0);
				res = CheckObjectOverlappedFromObjects(&TraceObject, 4);
				ok = 1;

				if (res != -1)
					FoundObject = &((*Objects4)[res]);

#endif

				if (Length2 > Length)
				{
					*NewEndX = EndX;
					*NewEndY = EndY;
					return 1;
				}

				TraceX2 = TraceX1 + cos(Angle) * Length2;
				TraceY2 = TraceY1 + sin(Angle) * Length2;

				if (((Angle > ANGLE_45) && (Angle < ANGLE_135)) || ((Angle > ANGLE_225) && (Angle < ANGLE_315)))
				{	// Mostly vertical line
					NewLengthY = AdjustToGrid(TraceY2, GridSize) - TraceY1;
					Length2 = NewLengthY / sin(Angle);
					GridStep = fabs(GridSize / sin(Angle));
				}
				else
				{	// Mostly horizontal line
					NewLengthX = AdjustToGrid(TraceX2, GridSize) - TraceX1;
					Length2 = NewLengthX / cos(Angle);
					GridStep = fabs(GridSize / cos(Angle));
				}

				Length2 += GridStep * 2;
				CreateTraceObjectFromSpecialLine(&TraceObject, TraceX1, TraceY1, Length2, Angle, TraceWidth, 0);
				res = CheckObjectOverlappedFromObjects(&TraceObject, 4);
				count = 0;

				while ((count < 4) && (res != -1))
				{
					Length2 -= GridStep;
					CreateTraceObjectFromSpecialLine(&TraceObject, TraceX1, TraceY1, Length2, Angle, TraceWidth, 0);
					res = CheckObjectOverlappedFromObjects(&TraceObject, 4);
					count++;
				}

				if (count == 4)
					return 0;

				if (Length2 > Length)
				{
					*NewEndX = EndX;
					*NewEndY = EndY;
					return 1;
				}

				*NewEndX = TraceX1 + cos(Angle) * Length2;
				*NewEndY = TraceY1 + sin(Angle) * Length2;
				return 1;

			}
			else
			{	// No overlapped object found
				if (Length2 > Length)
				{
					*NewEndX = EndX;
					*NewEndY = EndY;
					return 1;
				}
			}
		}
		else
		{
			if (Length2 > Length)
			{
				*NewEndX = EndX;
				*NewEndY = EndY;
				return 1;
			}
		}

		cnt++;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectOnlyVias()
{
	int32 TraceInfo, Layer, cnt;
	TraceRecord *Trace;

	StartDrawingEditingWindow(0);

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				{
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
					Trace->Info &= ~OBJECT_SELECTED;
					DrawVerTrace(Trace);

					if (OkToDrawClearances)
					{
						InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
						DrawVerTraceWithClearance(Trace);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				{
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
					Trace->Info &= ~OBJECT_SELECTED;
					DrawHorTrace(Trace);

					if (OkToDrawClearances)
					{
						InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
						DrawHorTraceWithClearance(Trace);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				{
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
					Trace->Info &= ~OBJECT_SELECTED;
					DrawDiag1Trace(Trace);

					if (OkToDrawClearances)
					{
						InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
						DrawDiag1TraceWithClearance(Trace);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				{
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
					Trace->Info &= ~OBJECT_SELECTED;
					DrawDiag2Trace(Trace);

					if (OkToDrawClearances)
					{
						InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
						DrawDiag2TraceWithClearance(Trace);
					}
				}
			}
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow(0);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectOnlyTraces()
{
	int32 ViaInfo, cnt;
	ViaRecord *Via;

	StartDrawingEditingWindow(0);

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Via->Info &= ~OBJECT_SELECTED;

			if (OkToDrawVias)
			{
				DrawVia(Via);
				DrawViaDrill(Via);

				if ((OkToDrawClearances) || (OkToDrawViaClearances))
				{
					InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
					DrawViaWithClearance(Via);
				}
			}
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow(0);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SelectNetViasTraces(int32 mode)
{
	int32 cnt, NetNr, Layer;
	ViaRecord *Via;
	TraceRecord *Trace;

	NetNr = -1;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NetNr = Via->NetNr;
			break;
		}
	}

	if (NetNr == -1)
	{
		for (Layer = 0; Layer < 32; Layer++)
		{
			DrawCode = DrawLayerCode[Layer];

			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;
						break;
					}
				}
			}

			if (NetNr != -1)
				break;

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;
						break;
					}
				}
			}

			if (NetNr != -1)
				break;

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;
						break;
					}
				}
			}

			if (NetNr != -1)
				break;

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
					{
						NetNr = Trace->NetNr;
						break;
					}
				}
			}

			if (NetNr != -1)
				break;
		}
	}

	if (NetNr == -1)
		return -1;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if (((Via->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Via->NetNr == NetNr))
			Via->Info |= OBJECT_SELECTED;
	}

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr) && (DrawCode >= 0)
			        && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				Trace->Info |= OBJECT_SELECTED;
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr) && (DrawCode >= 0)
			        && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				Trace->Info |= OBJECT_SELECTED;
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr) && (DrawCode >= 0)
			        && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				Trace->Info |= OBJECT_SELECTED;
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr) && (DrawCode >= 0)
			        && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				Trace->Info |= OBJECT_SELECTED;
		}
	}

	RePaint();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SelectNetAreaFills(int32 mode)
{
	int32 cnt, cnt2, NetNr;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon, *FirstPolygon;
	uint8 *AreaPos, *PolygonPos;

	NetNr = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((DrawPolygon->PolygonType & 2) == 2)
			{
				NetNr = AreaFill->NetNr;
				break;
			}
		}
	}

	if (NetNr == -1)
		return -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr == NetNr))
		{
			AreaPos = (uint8 *) AreaFill;
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
			FirstPolygon = DrawPolygon;

			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				PolygonPos = (uint8 *) DrawPolygon;
				DrawPolygon->PolygonType &= ~2;
				PolygonPos += MemSizePolygon(DrawPolygon);
				DrawPolygon = (PolygonRecord *) PolygonPos;
			}

			FirstPolygon->PolygonType |= 2;
		}
	}

	RePaint();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectOnlyObjects(int32 mode)
{
	int32 cnt;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	if (mode != 0)
	{
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectLine->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode != 1)
	{
		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectRect->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode != 3)
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectArc->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode != 4)
	{
		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectText2->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode != 5)
	{
		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectPolygon->Info &= ~OBJECT_SELECTED;
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnselectObjects(int32 mode)
{
	int32 cnt;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	if (mode == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectLine->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode == 1)
	{
		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectRect->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode == 3)
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectArc->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode == 4)
	{
		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectText2->Info &= ~OBJECT_SELECTED;
		}
	}

	if (mode == 5)
	{
		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				ObjectPolygon->Info &= ~OBJECT_SELECTED;
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ProtectComponents()
{
	int32 cnt;
	CompRecord *Comp;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Comp->Info |= COMPONENT_PROTECTED;
			Comp->TextVisibility &= ~(1 + 16);
			Comp->Info &= ~OBJECT_SELECTED;
			DataBaseChanged = 1;
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnprotectComponents()
{
	int32 cnt;
	CompRecord *Comp;

	StartDrawingEditingWindow(0);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == COMPONENT_PROTECTED)
		{
			Comp->Info &= ~COMPONENT_PROTECTED;
			DrawComp2(Comp, 0.0, 0.0, 0, 0x200);
			DataBaseChanged = 1;
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow(0);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void FindNextConnection()
{
	int32 cnt, ConnectionInfo, ok;
	double x1, y1, x2, y2, cx, cy;

	ConnectionsRecord *Connection;

	if (SearchConnectionNr == -1)
	{
		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);
			ConnectionInfo = Connection->Info;

			if ((ConnectionInfo & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Connection->x1;
				y1 = Connection->y1;
				x2 = Connection->x2;
				y2 = Connection->y2;
				cx = (x1 + x2) * 0.5;
				cy = (y1 + y2) * 0.5;
				CenterScreenOnPoint(cx, cy, 0);
				SearchConnectionNr = cnt;
				RePaint();
				DrawLineYellow(cx, -1000000000.0, cx, 1000000000.0, BM_DirectToScreen);
				DrawLineYellow(-1000000000.0, cy, 1000000000.0, cy, BM_DirectToScreen);
				return;
			}
		}

		if (SearchConnectionNr == -1)
			MessageBoxOwn(PCBWindow, SC(1060, "Did not found a connection"), SC(24, "Error"), MB_APPLMODAL | MB_OK);

		return;
	}

	cnt = SearchConnectionNr + 1;

	if (cnt >= Design.NrConnections)
		cnt = 0;

	while (cnt != SearchConnectionNr)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Connection->x1;
			y1 = Connection->y1;
			x2 = Connection->x2;
			y2 = Connection->y2;
			cx = (x1 + x2) * 0.5;
			cy = (y1 + y2) * 0.5;
			CenterScreenOnPoint(cx, cy, 0);
			SearchConnectionNr = cnt;
			RePaint();
			DrawLineYellow(cx, -1000000000.0, cx, 1000000000.0, BM_DirectToScreen);
			DrawLineYellow(-1000000000.0, cy, 1000000000.0, cy, BM_DirectToScreen);
			return;
		}

		cnt++;

		if (cnt >= Design.NrConnections)
			cnt = 0;
	}

	ok = 1;

	if (SearchConnectionNr != -1)
	{
		Connection = &((*Connections)[SearchConnectionNr]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Connection->x1;
			y1 = Connection->y1;
			x2 = Connection->x2;
			y2 = Connection->y2;
			cx = (x1 + x2) * 0.5;
			cy = (y1 + y2) * 0.5;
			CenterScreenOnPoint(cx, cy, 0);
			SearchConnectionNr = cnt;
			RePaint();
			DrawLineYellow(cx, -1000000000.0, cx, 1000000000.0, BM_DirectToScreen);
			DrawLineYellow(-1000000000.0, cy, 1000000000.0, cy, BM_DirectToScreen);
		}
	}
}

//***********************************************************************************************************************************
//***************************************** informace myši **************************************************************************
//***********************************************************************************************************************************

int32 GetInfoStr(LPSTR InfoStr, int32 mode)
{
	int32 ok, cnt, cnt2, Layer, CompInfo, Mirror, OkToCheck, Found, NetNr =
	    -1, ActiveCircle, NrNetProperties, NrCompProperties, TestResult;
	double x1a, x2a, y1a, y2a, PosX, PosY, x, y, x1 = 0.0, y1 = 0.0, x2 = 0.0, y2 =
	            0.0, x3, y3, x4, y4, lengte, dikte, ThickNess;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     str5[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING], str7[MAX_LENGTH_STRING], str8[MAX_LENGTH_STRING],
	     str9[MAX_LENGTH_STRING], LayerStr[MAX_LENGTH_STRING], PropertyID[MAX_LENGTH_STRING], 
		 PropertyValue[MAX_LENGTH_STRING];
	ObjectRecord *Object, NewObject;
	CompRecord *Comp;
	TraceRecord *Trace = NULL;
	ViaRecord *Via = NULL;
	NetRecord *Net;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;

	if (FirstPaint)
		return 0;

	if (MousePosX == 10000)
		return 2;

	PosX = x = PixelToRealOffX(MousePosX);
	PosY = y = PixelToRealOffY(DrawWindowMaxY - MousePosY);

	if (mode == 0)
	{
		if (DisplayInfoCursorX != -1)
		{
			if ((abs(DisplayInfoCursorX - MousePosX) > 5) || (abs(DisplayInfoCursorY - MousePosY) > 5))
			{
				DisplayInfoCursorX = -1;
				DisplayInfoCursorY = -1;
				return 2;
			}

			return 3;
		}
	}

	Layer = CurrentDrawingLayer;

	if (Layer == -1)
		Layer = Design.NrBoardLayers - 1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Mirror = ((Comp->CompMode & 8) >> 3);
		OkToCheck = 1;

		CompInfo = Comp->Info;

		if ((CompInfo & OBJECT_NOT_VISIBLE) != 0)
			OkToCheck = 0;

		if (OkToCheck)
		{

#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "J3") == 0)
				ok = 1;

#endif
			x1a = Comp->BoardPosMinX;
			y1a = Comp->BoardPosMinY;
			x2a = Comp->BoardPosMaxX;
			y2a = Comp->BoardPosMaxY;

			if ((PosX > x1a) && (PosX < x2a) && (PosY > y1a) && (PosY < y2a))
			{
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "J3") == 0)
					ok = 1;

#endif
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
				cnt2 = 0;
				Found = -1;
				NewObject.x1 = PosX;
				NewObject.y1 = PosY;
				NewObject.x2 = 100;
				NewObject.ObjectType = PIN_SMD_ROUND;

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
					FillPositionObject(Object);
#ifdef _DEBUG

					if (Object->Layer == 0)
						ok = 1;

#endif

					OkToCheck = 1;

					if (Object->Layer != -1)
					{
						if (Object->Layer == 0)
						{
							if (!OkToDrawBottomPads)
								OkToCheck = 0;
						}
						else
						{
							if (Object->Layer == Design.NrBoardLayers - 1)
							{
								if (!OkToDrawTopPads)
									OkToCheck = 0;
							}
							else
							{
								if (!OkToDrawInnerPads)
									OkToCheck = 0;
							}
						}
					}
					else
					{
						if ((!OkToDrawBottomPads) && (!OkToDrawTopPads) && (!OkToDrawInnerPads))
							OkToCheck = 0;
					}

					if (OkToCheck)
					{
						NewObject.Layer = Layer;

						if (ObjectsConnected(&NewObject, Object))
						{
							if (CompPinText(Comp, Object->PinNr, PosX, PosY, str) == 0)
							{
								GetPinTextFromObject(Object, str2, str3, str6, str8);
								NetNr = Object->NetNr;
								Found = cnt;
								break;
							}
						}
					}
				}

				if (Found == -1)
				{
					Layer = -1;

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
						FillPositionObject(Object);
#ifdef _DEBUG

						if (Object->Layer == 0)
							ok = 1;

						if (Object->ObjectType == PIN_LINE_ALL_ANGLE)
							ok = 1;

#endif
						OkToCheck = 1;

						if (Object->Layer == 0)
						{
							if (!OkToDrawBottomPads)
								OkToCheck = 0;
						}
						else
						{
							if (Object->Layer == Design.NrBoardLayers - 1)
							{
								if (!OkToDrawTopPads)
									OkToCheck = 0;
							}
							else
							{
								if (!OkToDrawInnerPads)
									OkToCheck = 0;
							}
						}

						if (OkToCheck)
						{
							NewObject.Layer = Layer;

							if (ObjectsConnected(&NewObject, Object))
							{
								if (CompPinText(Comp, Object->PinNr, PosX, PosY, str) == 0)
								{
									GetPinTextFromObject(Object, str2, str3, str6, str8);
									NetNr = Object->NetNr;
									Found = cnt;
									break;
								}
							}
						}

						ok = 1;
					}
				}

				if (Found != -1)
				{
					Comp = (CompRecord *) & (CompsMem[(*Comps)[Found]]);
					NrCompProperties = GetCompProperties(Comp, NULL, NULL, 0x40);
					strcpy(str4, "\r\n");
					strcat(str4, "\f\r\n");
					if ((Comp->Value) && (Comp->Value[0]))
					{
						sprintf(str5, SC(91, "Value : %s\r\n"), Comp->Value);
						strcat(str4, str5);
					}

					sprintf(str5, SC(92, "Geometry : %s\r\n"), Comp->ShapeName);
					strcat(str4, str5);

					if ((Comp->PartNr) && (Comp->PartNr[0]))
					{
						sprintf(str5, SC(93, "Part number : %s\r\n"), Comp->PartNr);
						strcat(str4, str5);
					}

					if (NrCompProperties > 0)
					{
						for (cnt2 = 0; cnt2 < NrCompProperties; cnt2++)
						{
							if (GetCompProperties(Comp, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) ==
							        0)
							{
								sprintf(str5, "%s : %s\r\n", PropertyID, PropertyValue);
								strcat(str4, str5);
							}
						}
					}

					str7[0] = 0;

					if ((NetNr >= 0) && (NetNr < Design.NrNets))
					{
						Net = &((*Nets)[NetNr]);
						sprintf(str7, SC(94, "\f\r\nNet : %s [ %d ]\r\n"), Net->Name, Net->NrPins);
						NrNetProperties = GetNetProperties(Net, NULL, NULL, 0x40);

						if (NrNetProperties > 0)
						{
							strcat(str7, SC(95, "Properties\r\n"));

							for (cnt2 = 0; cnt2 < NrNetProperties; cnt2++)
							{
								if (GetNetProperties(Net, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) ==
								        0)
								{
									sprintf(str5, "%s : %s\r\n", PropertyID, PropertyValue);
									strcat(str7, str5);
								}
							}
						}
					}

					sprintf(InfoStr,
				  SC(84, "Component pin\r\n\f\r\nReference : %s\r\nPin name : %s\r\nPosition x,y : %s\r\nClearance : %s\r\nLayer : %s%s%s"),
				  Comp->Name, str, str3, str8, str6, str4, str7);

					DisplayInfoCursorX = MousePosX;
					DisplayInfoCursorY = MousePosY;
					return 1;
				}
			}
		}
	}

// ********************************************************************************************************
	Found = -1;
	SearchMinX = PosX;
	SearchMinY = PosY;
	SearchMaxX = PosX;
	SearchMaxY = PosY;

	if (OkToDrawVias)
	{
		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Via->X;
				y1 = Via->Y;
				ThickNess = Via->ThickNess;

				if (RectTestCircle(x1, y1, ThickNess, 255))
				{
					GetUnitsValue2(Units, x1, str8, 0);
					GetUnitsValue2(Units, y1, str9, 2);
					sprintf(str, "%s , %s", str8, str9);
					GetUnitsValue2(Units, Via->ThickNess, str2, 2);
					GetUnitsValue2(Units, Via->DrillThickNess, str7, 2);
					GetUnitsValue2(Units, Via->Clearance, str3, 2);
					str4[0] = 0;
					str5[0] = 0;

					if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
					{
						Net = &((*Nets)[Via->NetNr]);
						sprintf(str4, SC(94, "\f\r\nNet : %s [ %d ]\r\n"), Net->Name, Net->NrPins);
						NrNetProperties = GetNetProperties(Net, NULL, NULL, 0x40);

						if (NrNetProperties > 0)
						{
							strcat(str5, SC(95, "Properties\r\n"));

							for (cnt2 = 0; cnt2 < NrNetProperties; cnt2++)
							{
								if (GetNetProperties(Net, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) ==
								        0)
								{
									sprintf(str6, "%s : %s\r\n", PropertyID, PropertyValue);
									strcat(str5, str6);
								}
							}
						}
					}

					sprintf(InfoStr, SC(86, "Via\r\n\f\r\nPosition x,y : %s\r\nDiameter : %s\r\nDrill : %s\r\nClearance : %s\r\n%s%s"),
						str, str2, str7, str3, str4, str5);

					DisplayInfoCursorX = MousePosX;
					DisplayInfoCursorY = MousePosY;
					return 1;
				}
			}
		}
	}

// ********************************************************************************************************


	DrawCode = DrawLayerCode[CurrentDrawingLayer];

	if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
	{
		if (Found == -1)
		{
			for (cnt = 0; cnt < Design.NrVerTraces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*VerTraces[CurrentDrawingLayer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					x2 = x1;
					y1 = Trace->Y;
					lengte = Trace->Length;
					y2 = y1 + lengte;
					dikte = Trace->ThickNess;

					if ((RectTestRect2(x1, y1 + (lengte / 2), dikte, lengte)) || (RectTestCircle(x1, y1, dikte, 255))
					        || (RectTestCircle(x2, y2, dikte, 255)))
					{
						Found = 1;
						break;
					}
				}
			}
		}

		if (Found == -1)
		{
			for (cnt = 0; cnt < Design.NrHorTraces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*HorTraces[CurrentDrawingLayer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					lengte = Trace->Length;
					x2 = x1 + lengte;
					y2 = y1;
					dikte = Trace->ThickNess;

					if ((RectTestRect2(x1 + (lengte / 2), y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
					        || (RectTestCircle(x2, y2, dikte, 255)))
					{
						Found = 1;
						break;
					}
				}
			}
		}

		if (Found == -1)
		{
			for (cnt = 0; cnt < Design.NrDiag1Traces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*Diag1Traces[CurrentDrawingLayer])[cnt]);
#ifdef _DEBUG

				if (cnt == 449)
					ok = 1;

#endif

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					lengte = Trace->Length;
					x2 = x1 + lengte;
					y2 = y1 - lengte;
					dikte = Trace->ThickNess;

					if ((RectTestDiag1(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
					        || (RectTestCircle(x2, y2, dikte, 255)))
					{
						Found = 1;
						break;
					}
				}
			}
		}

		if (Found == -1)
		{
			for (cnt = 0; cnt < Design.NrDiag2Traces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*Diag2Traces[CurrentDrawingLayer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					lengte = Trace->Length;
					x2 = x1 + lengte;
					y2 = y1 + lengte;
					dikte = Trace->ThickNess;

					if ((RectTestDiag2(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
					        || (RectTestCircle(x2, y2, dikte, 255)))
					{
						Found = 1;
						break;
					}
				}
			}
		}

		if (Found != -1)
		{
			GetUnitsValue2(Units, x1, str6, 0);
			GetUnitsValue2(Units, y1, str7, 0);
			GetUnitsValue2(Units, x2, str8, 0);
			GetUnitsValue2(Units, y2, str9, 2);
			GetUnitsValue2(Units, Trace->ThickNess, str2, 2);
			GetUnitsValue2(Units, Trace->Clearance, str3, 2);
			sprintf(str, "%s , %s - %s , %s", str6, str7, str8, str9);
			str4[0] = 0;
			str5[0] = 0;

			if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);
				sprintf(str4, SC(94, "\f\r\nNet : %s [ %d ]\r\n"), Net->Name, Net->NrPins);
				NrNetProperties = GetNetProperties(Net, NULL, NULL, 0x40);

				if (NrNetProperties > 0)
				{
					strcat(str5, SC(95, "Properties\r\n"));

					for (cnt2 = 0; cnt2 < NrNetProperties; cnt2++)
					{
						if (GetNetProperties(Net, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) == 0)
						{
							sprintf(str6, "%s : %s\r\n", PropertyID, PropertyValue);
							strcat(str5, str6);
						}
					}
				}
			}

			GetLayerText(CurrentDrawingLayer, LayerStr, 16 + 4);

			sprintf(InfoStr, SC(85, "Trace\r\n\f\r\nPosition x,y : %s\r\nTracewidth : %s\r\nClearance : %s\r\nLayer : %s\r\n%s%s"),
			        str, str2, str3, LayerStr, str4, str5);
			DisplayInfoCursorX = MousePosX;
			DisplayInfoCursorY = MousePosY;
			return 1;
		}
	}

	for (Layer = 0; Layer < 32; Layer++)
	{
		if (Layer == CurrentDrawingLayer)
			continue;

		DrawCode = DrawLayerCode[Layer];

		if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if (Found == -1)
			{
				for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
				{
					Trace = &((*VerTraces[Layer])[cnt]);

					if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
					{
						x1 = Trace->X;
						x2 = x1;
						y1 = Trace->Y;
						lengte = Trace->Length;
						y2 = y1 + lengte;
						dikte = Trace->ThickNess;

						if ((RectTestRect2(x1, y1 + (lengte / 2), dikte, lengte))
						        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
						{
							Found = 1;
							break;
						}
					}
				}
			}

			if (Found == -1)
			{
				for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
				{
					Trace = &((*HorTraces[Layer])[cnt]);

					if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
					{
						x1 = Trace->X;
						y1 = Trace->Y;
						lengte = Trace->Length;
						x2 = x1 + lengte;
						y2 = y1;
						dikte = Trace->ThickNess;

						if ((RectTestRect2(x1 + (lengte / 2), y1, lengte, dikte))
						        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
						{
							Found = 1;
							break;
						}
					}
				}
			}

			if (Found == -1)
			{
				for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
				{
					Trace = &((*Diag1Traces[Layer])[cnt]);

					if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
					{
						x1 = Trace->X;
						y1 = Trace->Y;
						lengte = Trace->Length;
						x2 = x1 + lengte;
						y2 = y1 - lengte;
						dikte = Trace->ThickNess;

						if ((RectTestDiag1(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
						        || (RectTestCircle(x2, y2, dikte, 255)))
						{
							Found = 1;
							break;
						}
					}
				}
			}

			if (Found == -1)
			{
				for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
				{
					Trace = &((*Diag2Traces[Layer])[cnt]);

					if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
					{
						x1 = Trace->X;
						y1 = Trace->Y;
						lengte = Trace->Length;
						x2 = x1 + lengte;
						y2 = y1 + lengte;
						dikte = Trace->ThickNess;

						if ((RectTestDiag2(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
						        || (RectTestCircle(x2, y2, dikte, 255)))
						{
							Found = 1;
							break;
						}
					}
				}
			}

			if (Found != -1)
			{
				GetUnitsValue2(Units, x1, str6, 0);
				GetUnitsValue2(Units, y1, str7, 0);
				GetUnitsValue2(Units, x2, str8, 0);
				GetUnitsValue2(Units, y2, str9, 2);
				GetUnitsValue2(Units, Trace->ThickNess, str2, 2);
				GetUnitsValue2(Units, Trace->Clearance, str3, 2);
				sprintf(str, "%s , %s - %s , %s", str6, str7, str8, str9);
				str4[0] = 0;
				str5[0] = 0;

				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Trace->NetNr]);
					sprintf(str4, SC(94, "\f\r\nNet : %s [ %d ]\r\n"), Net->Name, Net->NrPins);
					NrNetProperties = GetNetProperties(Net, NULL, NULL, 0x40);

					if (NrNetProperties > 0)
					{
						strcat(str5, SC(95, "Properties\r\n"));

						for (cnt2 = 0; cnt2 < NrNetProperties; cnt2++)
						{
							if (GetNetProperties(Net, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) == 0)
							{
								sprintf(str6, "%s : %s\r\n", PropertyID, PropertyValue);
								strcat(str5, str6);
							}
						}
					}
				}

				GetLayerText(Layer, LayerStr, 16 + 4);

				sprintf(InfoStr,
				        SC(85, "Trace\r\n\f\r\nPosition x,y : %s\r\nTracewidth : %s\r\nClearance : %s\r\nLayer : %s\r\n%s%s"), str,
				        str2, str3, LayerStr, str4, str5);
				DisplayInfoCursorX = MousePosX;
				DisplayInfoCursorY = MousePosY;
				return 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
#ifdef _DEBUG

		if (cnt == 106)
			ok = 1;

#endif
		Layer = ObjectLine->Layer;
		DrawCode = -1;
		NetNr = ObjectLine->NetNr;

		if (Layer < 32)
			DrawCode = DrawLayerCode[Layer];

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (DrawCode >= 0)
		        && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
		{
			x1 = ObjectLine->X1;
			y1 = ObjectLine->Y1;
			x2 = ObjectLine->X2;
			y2 = ObjectLine->Y2;

			if ((TestResult = RectTestLine3(x1, y1, x2, y2, ObjectLine->LineThickNess * 0.5)) != 0)
			{
				GetUnitsValue2(Units, x1, str6, 0);
				GetUnitsValue2(Units, y1, str7, 0);
				GetUnitsValue2(Units, x2, str8, 0);
				GetUnitsValue2(Units, y2, str9, 2);
				GetUnitsValue2(Units, ObjectLine->LineThickNess, str2, 2);
				GetUnitsValue2(Units, ObjectLine->Clearance, str3, 2);
				sprintf(str, "%s , %s - %s , %s", str6, str7, str8, str9);
				str4[0] = 0;
				str5[0] = 0;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);
					sprintf(str4, SC(94, "\f\r\nNet : %s [ %d ]\r\n"), Net->Name, Net->NrPins);
					NrNetProperties = GetNetProperties(Net, NULL, NULL, 0x40);

					if (NrNetProperties > 0)
					{
						strcat(str5, SC(95, "Properties\r\n"));

						for (cnt2 = 0; cnt2 < NrNetProperties; cnt2++)
						{
							if (GetNetProperties(Net, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) == 0)
							{
								sprintf(str6, "%s : %s\r\n", PropertyID, PropertyValue);
								strcat(str5, str6);
							}
						}
					}
				}

				GetLayerText(Layer, LayerStr, 16 + 4);

				sprintf(InfoStr,
				        SC(85, "Trace\r\n\f\r\nPosition x,y : %s\r\nTracewidth : %s\r\nClearance : %s\r\nLayer : %s\r\n%s%s"), str,
				        str2, str3, LayerStr, str4, str5);
				DisplayInfoCursorX = MousePosX;
				DisplayInfoCursorY = MousePosY;
				return 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ActiveCircle = 0;
		OkToCheck = 0;

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if ((InRange9(ObjectArc->CentreX, 170.7e5)) && (InRange9(ObjectArc->CentreY, 42.0e5)))
				ok = 1;

#endif
			Layer = ObjectArc->Layer;
			NetNr = ObjectArc->NetNr;

			if (Layer == DRILL_UNPLATED_LAYER)
			{
				if (DrawDrillMode > 0)
					OkToCheck = 1;
			}
			else
			{
				if (Layer < 32)
				{
					DrawCode = DrawLayerCode[Layer];

					if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
						OkToCheck = 1;
				}
				else
				{
					if (Layer == DRILL_LAYER)
					{
						if (DrawDrillMode > 0)
							OkToCheck = 1;
					}
				}
			}

			if (OkToCheck)
			{
				x1 = ObjectArc->CentreX;
				y1 = ObjectArc->CentreY;
				x2 = ObjectArc->Width;
				y2 = ObjectArc->Height;
				x3 = ObjectArc->StartDiffX;
				y3 = ObjectArc->StartDiffY;
				x4 = ObjectArc->EndDiffX;
				y4 = ObjectArc->EndDiffY;
#ifdef _DEBUG

				if (!UnselectAll)
				{
					if ((InRange9(x1, 185.0e5)) && (InRange9(y1, -6.28e5)))
						ok = 1;
				}

#endif

				if (((ObjectArc->Info & OBJECT_FILLED) == OBJECT_FILLED) || (Layer == DRILL_LAYER)
				        || (Layer == DRILL_UNPLATED_LAYER) || ((InRange(x3, x4)) && (InRange(y3, y4)) && (InRange(x2, y2))))
					ActiveCircle = 1;
				
				if (((!ActiveCircle) && (RectTestArc2(x1, y1, x2, y2, x3, y3, x4, y4, ObjectArc->LineThickNess)))
				        || ((ActiveCircle) && (RectTestCircle(x1, y1, x2, 255))))
				{
					if ((Layer != DRILL_LAYER) && (Layer != DRILL_UNPLATED_LAYER)
					        && ((ObjectArc->Info & OBJECT_FILLED) == 0))
					{
						GetUnitsValue2(Units, x1, str6, 0);
						GetUnitsValue2(Units, y1, str7, 2);
						GetUnitsValue2(Units, x2, str8, 2);
						GetUnitsValue2(Units, ObjectArc->LineThickNess, str2, 2);
						GetUnitsValue2(Units, ObjectArc->Clearance, str3, 2);
						sprintf(str, SC(96, "%s , %s\r\nDiameter : %s"), str6, str7, str8); //trasa kruhu
					}
					else
					{
						GetUnitsValue2(Units, x1, str6, 0);
						GetUnitsValue2(Units, y1, str7, 2);
						GetUnitsValue2(Units, ObjectArc->Width, str2, 2);
						GetUnitsValue2(Units, ObjectArc->Clearance, str3, 2);
						sprintf(str, "%s , %s", str6, str7);
					}

					str4[0] = 0;
					str5[0] = 0;

					if ((Layer != DRILL_UNPLATED_LAYER) && (NetNr >= 0) && (NetNr < Design.NrNets))
					{
						Net = &((*Nets)[NetNr]);
						sprintf(str4, SC(94, "\f\r\nNet : %s [ %d ]\r\n"), Net->Name, Net->NrPins);
						NrNetProperties = GetNetProperties(Net, NULL, NULL, 0x40);

						if (NrNetProperties > 0)
						{
							strcat(str5, SC(95, "Properties\r\n"));

							for (cnt2 = 0; cnt2 < NrNetProperties; cnt2++)
							{
								if (GetNetProperties(Net, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) ==
								        0)
								{
									sprintf(str6, "%s : %s\r\n", PropertyID, PropertyValue);
									strcat(str5, str6);
								}
							}
						}
					}

					if (Layer == DRILL_LAYER)
					{
						sprintf(InfoStr, SC(87, "Drill\r\n\f\r\nPosition x,y : %s\r\nDiameter : %s\r\nClearance : %s\r\n%s%s"),
							    str, str2, str3, str4, str5);
					}

					if (Layer == DRILL_UNPLATED_LAYER)
					{
						sprintf(InfoStr, SC(88, "Unplated drill\r\n\f\r\nPosition x,y : %s\r\nDiameter : %s\r\nClearance : %s"),
						        str, str2, str3);
					}

					if (Layer < 32)
					{
						GetLayerText(Layer, LayerStr, 16 + 4);

						if ((ObjectArc->Info & OBJECT_FILLED) == OBJECT_FILLED)
						{
							sprintf(InfoStr, SC(89, "Pad\r\n\f\r\nPosition x,y : %s\r\nDiameter : %s\r\nClearance : %s\r\nLayer : %s\r\n%s%s"),
								str, str2, str3, LayerStr, str4, str5);
						}
						else
						{     //Trasa kruhu
							sprintf(InfoStr, SC(90, "Arc trace\r\n\f\r\nPosition x,y : %s\r\nTracewidth : %s\r\nClearance : %s\r\nLayer : %s\r\n%s%s"),
								str, str2, str3, LayerStr, str4, str5);
						}
					}

					DisplayInfoCursorX = MousePosX;
					DisplayInfoCursorY = MousePosY;
					return 1;
				}
			}
		}
	}

	return 0;
}

//*************************************************************************************************************************************
//*************************************************************************************************************************************
//*************************************************************************************************************************************
