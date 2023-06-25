/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: trace6.c
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
#include "trace6.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "calc.h"
#include "calc3.h"
#include "toets.h"
#include "calcdef.h"
#include "math.h"
#include "calc2.h"
#include "graphics.h"
#include "calcrect.h"
#include "mainloop.h"
#include "insdel.h"
#include "select.h"
#include "select3.h"
#include "dialogs.h"
#include "stdio.h"
#include "polygon.h"
#include "resource.h"
#include "help.h"

int32 SelectedNet, NewNetNr, PinPointLayer, ok;
double PinPointX, PinPointY;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTracesVias(double CurrentX, double CurrentY, int32 mode)
{
	int32 cnt;

	ObjectRecord *Object5, Object5a;

	StartDrawingEditingWindow(BM_DoubleBuffer);
	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < NrObjects5; cnt++)
	{
		Object5 = &((*Objects5)[cnt]);
		memmove(&Object5a, Object5, sizeof(ObjectRecord));
		Object5a.x1 += CurrentX - CurrentX2 - ShiftOffsetX;
		Object5a.y1 += CurrentY - CurrentY2 - ShiftOffsetY;

		switch (Object5->ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			DrawCode = DrawLayerCode[Object5a.Layer];

			if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
			{
				InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
				DrawObjectTraceXor(&Object5a);
			}

			break;

		case VIA_PUT_THROUGH_ROUND:
			InitDrawingObject(VIA_PUT_THROUGH_ROUND, 0, 0, NORMAL_FILLED_AND_PEN1);
//        InitDrawingEmptyPen();
			DrawObject(&Object5a, 2);
			break;
		}
	}

	if (DrawDrillMode > 0)
	{
		for (cnt = 0; cnt < NrObjects5; cnt++)
		{
			Object5 = &((*Objects5)[cnt]);
			memmove(&Object5a, Object5, sizeof(ObjectRecord));
			Object5a.x1 += CurrentX - CurrentX2 - ShiftOffsetX;
			Object5a.y1 += CurrentY - CurrentY2 - ShiftOffsetY;

			switch (Object5->ObjectType)
			{
			case VIA_PUT_THROUGH_ROUND:
//          DrawDrillObject(&Object5a,2);
				break;
			}
		}
	}

	DrawCrossHair(8);

	ExitDrawing();
	EndDrawingEditingWindow(BM_DoubleBuffer);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceCopiedTracesVias(double CurrentX, double CurrentY, int32 mode)
{
	double x1, y1;
	int32 cnt, cnt2, Found;
	NetRecord *Net;
	ObjectRecord *Object, *Object5, Object2, Object5a, FoundObject;
	ViaRecord NewVia, *Via;

	memset(&FoundObject, 0, sizeof(FoundObject));
	CurrentX += ShiftOffsetX;
	CurrentY += ShiftOffsetY;
	SearchMaxX = CurrentX + 50000;
	SearchMinX = CurrentX - 50000;
	SearchMaxY = CurrentY + 50000;
	SearchMinY = CurrentY - 50000;
	Object2.ObjectType = PIN_SMD_ROUND;
	Object2.x1 = CurrentX;
	Object2.y1 = CurrentY;
	Object2.x2 = 100.0;
	Object2.Clearance = 10.0;
	Object2.Layer = PinPointLayer;
	NrObjects4 = 0;
	Found = CopyCompObjectsFromRectWindowToObjects4(-1, 0);
	FoundObject.ObjectType = 0;

	for (cnt2 = 0; cnt2 < NrObjects4; cnt2++)
	{
		Object = &((*Objects4)[cnt2]);

		if (ObjectsConnected(Object, &Object2))
		{
			if (FoundObject.ObjectType == 0)
				memmove(&FoundObject, Object, sizeof(ObjectRecord));
		}
	}

	if (FoundObject.ObjectType != 0)
	{
		CurrentX = FoundObject.x1;
		CurrentY = FoundObject.y1;
		x1 = CurrentX - CurrentX2 - ShiftOffsetX;
		y1 = CurrentY - CurrentY2 - ShiftOffsetY;

		if ((FoundObject.NetNr < 0) || (FoundObject.NetNr >= Design.NrNets))
		{
			MessageBoxOwn(PCBWindow, SC(1061, "Pin is not connected"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
			Beep(1000, 200);
			return;
		}

		NewNetNr = FoundObject.NetNr;
		Net = &((*Nets)[NewNetNr]);

		for (cnt = 0; cnt < NrObjects5; cnt++)
		{
			Object5 = &((*Objects5)[cnt]);
			memmove(&Object5a, Object5, sizeof(ObjectRecord));
			Object5a.x1 += x1;
			Object5a.y1 += y1;
			Object5a.NetNr = NewNetNr;
			FillPositionObject(&Object5a);

			switch (Object5a.ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
				if ((CheckObjectInsideAreaFillPowerPlane(&Object5a) == 0) && (!CheckObjectOverlapped(&Object5a, 0)))
				{
					Object5a.Info = 0;

					if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
						Object5a.Info = OBJECT_HIGHLITED;

					if (!AddTrace(&Object5a))
						return;

					if (RecalcAreafillAfterInsert)
						InsertObjectInAreaFill(&Object5a, Object5a.Layer, Object5a.NetNr, 2);
				}

				break;

			case VIA_PUT_THROUGH_ROUND:
				Via = &((*Vias)[Object5a.TraceNr]);
				memmove(&NewVia, Via, sizeof(ViaRecord));
				NewVia.X = (float) Object5a.x1;
				NewVia.Y = (float) Object5a.y1;
				NewVia.NetNr = (int16) Object5a.NetNr;
				NewVia.Info = 0;

				if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
					NewVia.Info = OBJECT_HIGHLITED;

				if (!AddVia(&NewVia))
					return;

				if (RecalcAreafillAfterInsert)
					InsertObjectInAreaFill(&Object5a, -1, Object5a.NetNr, 2);

				break;
			}
		}

		ReCalcConnectionsNet(NewNetNr, 0, 1);
		RePaint();
	}
	else
	{
		Beep(1000, 200);
		MessageBoxOwn(PCBWindow, SC(1082, "Did not find a pin"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveTracesVias(int32 mode)
{
	int32 NrParams, count, NewMode, FirstShift, ObjectsPlaced;
	double OldX, OldY, CurrentX, CurrentY, ShiftX, ShiftY, ShiftOffsetX2, ShiftOffsetY2, x1, y1, x2, y2;
	ObjectRecord *Object;
	DrawXorFunctionRecord DrawXorFunction;
	ObjectTextRecord2 TypeObject;

	RelX = CurrentDrawX1;
	RelY = CurrentDrawY1;
	ShiftX = 0.0;
	ShiftY = 0.0;

	SelectionEsc = 0;
	ObjectsPlaced = 0;
	/*
	  Rect.left=(int32)DrawWindowMinX+ClientStartX;
	  Rect.top=(int32)DrawWindowMinY+ClientStartY;
	  Rect.right=(int32)DrawWindowMaxX+ClientStartX;
	  Rect.bottom=(int32)DrawWindowMaxY+ClientStartY;
	*/

	RelX = PixelToRealOffX(MousePosX);
	RelY = PixelToRealOffY(DrawWindowMaxY - MousePosY - 1);
	DisplayCursorPosition();

	count = 0;
	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
	ShiftOffsetX2 = 0.0;
	ShiftOffsetY2 = 0.0;
	ShiftOffsetX = 0.0;
	ShiftOffsetY = 0.0;

	Object = &((*Objects4)[0]);

//  ShiftOffsetX=-(CurrentX-AdjustToDrawGrid(Object->x1));
//  ShiftOffsetY=-(CurrentY-AdjustToDrawGrid(Object->y1));
//  ShiftOffsetX=-(CurrentX-AdjustToDrawGrid(PinPointX));
//  ShiftOffsetY=-(CurrentY-AdjustToDrawGrid(PinPointY));
	ShiftOffsetX = -(CurrentX - PinPointX);
	ShiftOffsetY = -(CurrentY - PinPointY);


	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	FirstShift = 1;
	ClipMouseCursor();
	DrawTracesVias(CurrentX, CurrentY, 0);
	SystemBusyMode = 30;
	NewMode = 0;
	DrawXorFunction.Function1 = (FUNCP1) DrawTracesVias;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &NewMode;
	DrawXorFunction.Mode = 1;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &NewMode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			DrawCrossHair(0);

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawTracesVias(OldX, OldY, 0);
				else
				{
					StartDrawingEditingWindow(BM_DoubleBuffer);
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GRAY + DRAW_WITH_PEN_AND_NOT_FILLED);
					SetROP2(OutputDisplay, R2_XORPEN);
					DrawLine(MultX(OldX), 100000, MultX(OldX), -100000);
					DrawLine(100000, MultY(OldY), -100000, MultY(OldY));
					ExitDrawing();
					EndDrawingEditingWindow(BM_DoubleBuffer);
				}

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawTracesVias(OldX, OldY, 0);
				else
				{
					StartDrawingEditingWindow(BM_DoubleBuffer);
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GRAY + DRAW_WITH_PEN_AND_NOT_FILLED);
					SetROP2(OutputDisplay, R2_XORPEN);
					DrawLine(MultX(CurrentX), 100000, MultX(CurrentX), -100000);
					DrawLine(100000, MultY(CurrentY), -100000, MultY(CurrentY));
					ExitDrawing();
					EndDrawingEditingWindow(BM_DoubleBuffer);
				}
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawTracesVias(OldX, OldY, 0);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTracesVias(CurrentX, CurrentY, 0);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawTracesVias(OldX, OldY, 0);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTracesVias(CurrentX, CurrentY, 0);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawTracesVias(OldX, OldY, 0);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTracesVias(CurrentX, CurrentY, 0);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawTracesVias(OldX, OldY, 0);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTracesVias(CurrentX, CurrentY, 0);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{
				ShiftOffsetX -= ShiftX - CurrentX;
				ShiftOffsetY -= ShiftY - CurrentY;
				FirstShift = 1;
				RelX += ShiftOffsetX;
				RelY += ShiftOffsetY;
				DisplayCursorPosition();
			}
		}
		else
		{
			if (FirstShift)
			{
				ShiftX = CurrentX;
				ShiftY = CurrentY;
				FirstShift = 0;
			}
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTracesVias(CurrentX, CurrentY, 0);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTracesVias(OldX, OldY, 0);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTracesVias(CurrentX, CurrentY, 0);
			else
				ObjectsPlaced = 1;
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTracesVias(OldX, OldY, 0);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTracesVias(CurrentX, CurrentY, 0);
			else
				ObjectsPlaced = 1;
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTracesVias(OldX, OldY, 0);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTracesVias(CurrentX, CurrentY, 0);
			else
				ObjectsPlaced = 1;
		}

		if (CheckLeftButton())
		{
			DrawTracesVias(OldX, OldY, 0);
// CurrentX2 , CurrentY2
			PlaceCopiedTracesVias(CurrentX - ShiftOffsetX, CurrentY - ShiftOffsetY, 0);
			CheckInputMessages(0);
			ObjectsPlaced = 1;
			LastActionNr++;
			count++;
			DrawTracesVias(CurrentX, CurrentY, 0);
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTracesVias(OldX, OldY, 0);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(TypeObject));

				if (LineInputDialog(&TypeObject, SC(1083, "Move traces/vias (x,y)"), 0) == 1)
				{
					if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) == 2)
					{
						x1 = ParamsFloat[0];
						y1 = ParamsFloat[1];

//            CentreComponentsX=CentreComponentsX2;
//            CentreComponentsY=CentreComponentsY2;
						if (!ParametersRelative)
						{
							x1 -= ShiftOffsetX2;
							y1 -= ShiftOffsetY2;
						}
						else
						{
							x1 += CurrentX2;
							y1 += CurrentY2;
						}

						x2 = x1 - CurrentX2;
						y2 = y1 - CurrentY2;
//            PlaceMovedObjects(x1,y1,0);
						SelectionEsc = 1;
					}
				}
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				Help("move_traces_vias.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			FirstShift = 1;

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTracesVias(CurrentX, CurrentY, 0);
			else
				ObjectsPlaced = 1;

			ok = 1;
			ClipMouseCursor();
		}
	}

	UnClipMouseCursor();

	if (count > 0)
		LastActionNr--;

	if (!ObjectsPlaced)
		DrawTracesVias(CurrentX, CurrentY, 0);

	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CopyTracesViasNet()
{
	int32 cnt, Layer, ConnectionObjectNr, Error = 0;
	TraceRecord *Trace;
	ObjectRecord *Object5;
	ViaRecord *Via;
	int32 Found2 = 0;
	ObjectRecord *Object3;
	double x1, y1, x2, y2;

	NrObjects5 = 0;
	SelectedNet = -1;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (NrObjects5 + 1 > MaxNrObjects5)
				{
					if (AllocateMemObjects5(MaxNrObjects5 + 128) == -1)
						return;
				}

				Object5 = &((*Objects5)[NrObjects5]);
				CreateTraceObjectFromTrace(Trace, Object5, TRACE_VER, Layer, cnt, 0);
				NrObjects5++;

				if (SelectedNet == -1)
					SelectedNet = Trace->NetNr;
				else
				{
					if (SelectedNet != Trace->NetNr)
						Error = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (NrObjects5 + 1 > MaxNrObjects5)
				{
					if (AllocateMemObjects5(MaxNrObjects5 + 128) == -1)
						return;
				}

				Object5 = &((*Objects5)[NrObjects5]);
				CreateTraceObjectFromTrace(Trace, Object5, TRACE_HOR, Layer, cnt, 0);
				NrObjects5++;

				if (SelectedNet == -1)
					SelectedNet = Trace->NetNr;
				else
				{
					if (SelectedNet != Trace->NetNr)
						Error = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (NrObjects5 + 1 > MaxNrObjects5)
				{
					if (AllocateMemObjects5(MaxNrObjects5 + 128) == -1)
						return;
				}

				Object5 = &((*Objects5)[NrObjects5]);
				CreateTraceObjectFromTrace(Trace, Object5, TRACE_DIAG1, Layer, cnt, 0);
				NrObjects5++;

				if (SelectedNet == -1)
					SelectedNet = Trace->NetNr;
				else
				{
					if (SelectedNet != Trace->NetNr)
						Error = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (NrObjects5 + 1 > MaxNrObjects5)
				{
					if (AllocateMemObjects5(MaxNrObjects5 + 128) == -1)
						return;
				}

				Object5 = &((*Objects5)[NrObjects5]);
				CreateTraceObjectFromTrace(Trace, Object5, TRACE_DIAG2, Layer, cnt, 0);
				NrObjects5++;

				if (SelectedNet == -1)
					SelectedNet = Trace->NetNr;
				else
				{
					if (SelectedNet != Trace->NetNr)
						Error = 1;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (NrObjects5 + 1 > MaxNrObjects5)
			{
				if (AllocateMemObjects5(MaxNrObjects5 + 128) == -1)
					return;
			}

			Object5 = &((*Objects5)[NrObjects5]);
			Object5->x1 = Via->X;
			Object5->y1 = Via->Y;
			Object5->x2 = Via->ThickNess;
			Object5->y2 = Via->DrillThickNess;
			Object5->ObjectType = VIA_PUT_THROUGH_ROUND;
			Object5->TraceNr = cnt;
			Object5->Info = 0;
			Object5->Layer = -1;
			Object5->Clearance = Via->Clearance;
			FillPositionObject(Object5);
			NrObjects5++;

			if (SelectedNet == -1)
				SelectedNet = Via->NetNr;
			else
			{
				if (SelectedNet != Via->NetNr)
					Error = 1;
			}
		}
	}

	if (Error == 1)
	{
		MessageBoxOwn(PCBWindow, SC(1084, "Selected traces/vias have more then one net"), SC(24, "Error"),
		              MB_APPLMODAL | MB_OK);
		Beep(1000, 200);
		return;
	}

	if (NrObjects5 > 0)
	{
		GetObjectsNet(SelectedNet, MODE_OBJECTS3, 0);
		PinPointLayer = 1000;

		for (cnt = 0; cnt < NrObjects5; cnt++)
		{
			Object5 = &((*Objects5)[cnt]);
			x1 = Object5->x1;
			y1 = Object5->y1;

			switch (Object5->ObjectType)
			{
			case TRACE_HOR:
				x2 = Object5->x1 + Object5->x2;
				y2 = Object5->y1;

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x1, y1)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x2, y2)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				break;

			case TRACE_VER:
				x2 = Object5->x1;
				y2 = Object5->y1 + Object5->x2;

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x1, y1)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x2, y2)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				break;

			case TRACE_DIAG1:
				x2 = Object5->x1 + Object5->x2;
				y2 = Object5->y1 - Object5->x2;

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x1, y1)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x2, y2)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				break;

			case TRACE_DIAG2:
				x2 = Object5->x1 + Object5->x2;
				y2 = Object5->y1 + Object5->x2;

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x1, y1)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				if ((ConnectionObjectNr = GetObjectNrFromEndPoint2(Object5->Layer, x2, y2)) != -1)
				{
					Object3 = &((*Objects3)[ConnectionObjectNr]);
					PinPointX = Object3->x1;
					PinPointY = Object3->y1;
					PinPointLayer = Object5->Layer;
					Found2 = 1;
				}

				break;
				/*
				        case VIA_PUT_THROUGH_ROUND:
				          if ((ConnectionObjectNr=GetObjectNrFromEndPoint2(Object5->Layer,x1,y1))!=-1) {
				            Object3=&((*Objects3)[ConnectionObjectNr]);
				            PinPointX=Object3->x1;
				            PinPointY=Object3->y1;
				            Found2=1;
				          }
				          break;
				*/
			}
		}

		if (Found2)
			MoveTracesVias(0);
		else
		{
			Beep(1000, 200);
			MessageBoxOwn(PCBWindow, SC(1082, "Did not find a pin"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
		}
	}

	DeAllocateMemObjects5();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
