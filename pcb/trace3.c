/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: trace3.c
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
#include "windows.h"
#include "string.h"
#include "stdlib.h"
#include "math.h"
#include "stdio.h"
#include "keyswin.h"
#include "toets.h"
#include "commdlg.h"
#include "pcb.h"
#include "calc.h"
#include "calc4.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "mainloop.h"
#include "nets.h"
#include "files.h"
#include "trace2.h"
#include "trace3.h"
#include "trace5.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "draw2.h"
#include "draw3.h"
#include "calcdef.h"
#include "draw.h"
#include "select2.h"
#include "calc3.h"
#include "graphics.h"
#include "help.h"
#include "owntime.h"
#include "dialogs.h"
#include "insdel.h"
#include "polygon.h"
#include "resource.h"
#include "settings.h"

#define TRACE_REPAINT


int32 DrawFirstTrace, DrawSecondTrace, TraceChanged, ok;
double Dir0X, Dir0Y, Dir1X, Dir1Y, Dir2X, Dir2Y, Dir3X, Dir3Y, Dir4X, Dir4Y, Dir5X, Dir5Y, Dir6X, Dir6Y, Dir7X, Dir7Y,
       DirX, DirY, OldX, OldY, OldCurrentTraceWidth, OldCurrentClearance;
int32 dir, dir2, NrPreviousTraces, TracesInserted;
//int32   TraceArcAngleMode=2; // 90 degrees
int32 Dir0OK, Dir1OK, Dir2OK, Dir3OK, Dir4OK, Dir5OK, Dir6OK, Dir7OK, DirOK;
int32 SpecialLeftButtonPressed;
int32 InsertViaKeyPressed;
int32 TraceBackWardsKeyPressed;
int32 ChangeTraceDrawingModeKeyPressed;
int32 FinishedTraceKeyPressed;
int32 EditDesignRulesNetKeyPressed;
int32 TraceDrawingActive;
int32 TraceDrawingInverted;
int32 TraceDrawingMode = 0;
int32 ScrollCount = 0;

TraceRecord NewTrace1, NewTrace2;

ViaRecord NewVia1;

extern int32 ObjectKeepOut, ObjectKeepOut2, TimerValue, TestCounter, CrossHairMode, CrossHairType;


void DrawTrace(double x1, double y1, double x2, double y2, double TraceWidth, double TraceClearanceWidth, int32 NetNr,
               int32 mode);

int32 FindShortestPadNet(double StartX, double StartY, double x1, double y1, double ExcludeX1, double ExcludeY1,
                         double ExcludeX2, double ExcludeY2, double *x2, double *y2, int32 mode);


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void TraceDrawingOff(int32 mode)
{
	ObjectRecord ObjectTrace;
	NetRecord *Net;

	DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance, CurrentDrawingNetNr,
	          TraceDrawingMode | 16);
	DrawCrossObjects(0, 0);		// Display the traces/vias in the normal form

	if (((CurrentWorkingTrace.Info2 & 1) == 1) && (CurrentWorkingTrace.ObjectType != 0))
	{
		StartDrawingEditingWindow();
		DrawCode = DrawLayerCode[CurrentDrawingLayer];
		InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
		memmove(&ObjectTrace, &CurrentWorkingTrace, sizeof(ObjectRecord));
		ObjectTrace.Info &= ~OBJECT_HIGHLITED;

		if ((ObjectTrace.NetNr >= 0) && (ObjectTrace.NetNr < Design.NrNets))
		{
			Net = &((*Nets)[ObjectTrace.NetNr]);

			if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
				ObjectTrace.Info |= OBJECT_HIGHLITED;
		}

		DrawObject(&ObjectTrace, 0);

		if (OkToDrawClearances)
		{
			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObject(&ObjectTrace, 0x40 + 8);
		}

		ExitDrawing();
		EndDrawingEditingWindow();
	}

	TraceDrawingActive = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void TraceDrawingOn(int32 mode)
{
	ObjectRecord ObjectTrace;
	NetRecord *Net;
#ifdef _DEBUG
	int32 ok;
#endif

	DrawCrossObjects(0, 2);		// Display the traces/vias in the marked form

	if (((CurrentWorkingTrace.Info2 & 1) == 1) && (CurrentWorkingTrace.ObjectType != 0))
	{
		StartDrawingEditingWindow();
		DrawCode = DrawLayerCode[CurrentDrawingLayer];
		memmove(&ObjectTrace, &CurrentWorkingTrace, sizeof(ObjectRecord));
		ObjectTrace.Info &= ~OBJECT_HIGHLITED;

		if ((ObjectTrace.NetNr >= 0) && (ObjectTrace.NetNr < Design.NrNets))
		{
			Net = &((*Nets)[ObjectTrace.NetNr]);

			if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
				ObjectTrace.Info |= OBJECT_HIGHLITED;
		}

#ifdef _DEBUG

		if (ObjectTrace.ObjectType == TRACE_ARC)
			ok = 1;

#endif

		SetBackGroundActive(0);

		if ((ObjectTrace.ObjectType == TRACE_ALL_ANGLE) || (ObjectTrace.ObjectType == TRACE_ARC))
			ObjectTrace.Thickness += 2540.0;

		DrawObject(&ObjectTrace, 0);

		if (OkToDrawClearances)
		{
			SetBackGroundActive(0);
			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObject(&ObjectTrace, 0x40 + 8);
		}

		ExitDrawing();
		EndDrawingEditingWindow();
	}

	DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance, CurrentDrawingNetNr,
	          TraceDrawingMode);
	TraceDrawingActive = 1;
//  DrawLineGray(CurrentDrawX2,1000000000.0,
//               CurrentDrawX2,-1000000000.0);
//  DrawLineGray(1000000000.0,CurrentDrawY2,
//               -1000000000.0,CurrentDrawY2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AdjustTraceToObject(int32 mode)
{
	int32 Direction, EndPointObjectNr, OldCurrentDirection;
	double NewX, NewY, OldX2, OldY2;
	ObjectRecord *ConnectObject, *Object3;
#ifdef _DEBUG
	int32 ok;
#endif

	EndPointObjectNr = -1;
	Direction = DrawTrace2.Info2;
	ObjectKeepOut = -1;
	ObjectKeepOut2 = -1;

	if (Direction == -1)
		Direction = DrawTrace1.Info2;

//  if (DrawTrace2.Info2==-1) {
//    ObjectKeepOut=ConnectionObject1.Test;
//  }
	if (CurrentWorkingTrace.ObjectType != 0)
		ObjectKeepOut2 = CurrentWorkingTrace.TraceNr;

	if ((CurrentWorkingTrace.TraceNr >= 0) && (CurrentWorkingTrace.TraceNr < NrObjects3))
		ConnectObject = &((*Objects3)[CurrentWorkingTrace.TraceNr]);

	if (ObjectKeepOut == ObjectKeepOut2)
	{
		ObjectKeepOut = GetObjectNrFromEndPoint(CurrentDrawingLayer, DrawingConnectionX1, DrawingConnectionY1, 3);

		if (ObjectKeepOut == -1)
			ObjectKeepOut = GetObjectNrFromEndPoint(-1, StartFirstDrawingTraceX, StartFirstDrawingTraceY, 8);
	}
	else
	{
		if (ObjectKeepOut == -1)
			ObjectKeepOut = GetObjectNrFromEndPoint(-1, StartFirstDrawingTraceX, StartFirstDrawingTraceY, 8);
	}

#ifdef _DEBUG

	if (ObjectKeepOut != -1)
	{
		Object3 = &((*Objects3)[ObjectKeepOut]);
		ok = 1;
	}

#endif

	switch (DrawTrace1.ObjectType)
	{
	case TRACE_HOR:
	case TRACE_VER:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
		Direction = DrawTrace1.Info2;
		break;

	case TRACE_ARC:
		Direction = -1;
//      Direction=DrawTrace1.Info2 >> 2;
		break;

	case TRACE_ALL_ANGLE:
		Direction = -1;
		break;
	}

	if ((EndPointObjectNr =
	            FindEndPointFromNetObjects(StartFirstDrawingTraceX, StartFirstDrawingTraceY, EndFirstDrawingTraceX,
	                                       EndFirstDrawingTraceY, CurrentTraceWidth, Direction, CurrentDrawingLayer, &NewX,
	                                       &NewY)) != -1)
	{
		Object3 = &((*Objects3)[EndPointObjectNr]);
		DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance, CurrentDrawingNetNr,
		          0);
		OldX2 = OldX;
		OldY2 = OldY;
		CurrentDrawX2 = NewX;
		CurrentDrawY2 = NewY;
		OldX = CurrentDrawX2;
		OldY = CurrentDrawY2;	// CurrentDirection
		OldCurrentDirection = CurrentDirection;
		DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance, CurrentDrawingNetNr,
		          0);

		if (DrawTrace1.x2 == 0.0)
		{
			DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance,
			          CurrentDrawingNetNr, 0);
			OldX = OldX2;
			OldY = OldY2;
			CurrentDirection = OldCurrentDirection;
			DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance,
			          CurrentDrawingNetNr, 0);
		}
		else
			return EndPointObjectNr;
	}

	switch (DrawTrace2.ObjectType)
	{
	case TRACE_HOR:
	case TRACE_VER:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
		Direction = DrawTrace2.Info2;
		break;

	case TRACE_ARC:
		Direction = -1;
//      Direction=DrawTrace2.Info2 >> 2;
		break;

	case TRACE_ALL_ANGLE:
		Direction = -1;
		break;
	}

	if ((EndPointObjectNr =
	            FindEndPointFromNetObjects(StartSecondDrawingTraceX, StartSecondDrawingTraceY, EndSecondDrawingTraceX,
	                                       EndSecondDrawingTraceY, CurrentTraceWidth, Direction, CurrentDrawingLayer, &NewX,
	                                       &NewY)) != -1)
	{
		Object3 = &((*Objects3)[EndPointObjectNr]);
		DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance, CurrentDrawingNetNr,
		          0);
		CurrentDrawX2 = NewX;
		CurrentDrawY2 = NewY;
		OldX = CurrentDrawX2;
		OldY = CurrentDrawY2;
		DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance, CurrentDrawingNetNr,
		          0);
		return EndPointObjectNr;
	}

	return EndPointObjectNr;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 TraceChanging(int32 mode, int32 mode2)
{
	int32 ok, EndPointObjectNr;
	int32 ResultInsertTrace;

	switch (mode)
	{
	case 0:					// LeftButton pressed

//      if (CurrentWorkingTrace.ObjectType==VIA_PUT_THROUGH_ROUND) {
//        CurrentWorkingTrace.ObjectType=0;
//      }
//      Object3=&((*Objects3)[ObjectKeepOut]);
//      if ((CurrentWorkingTrace.Info2!=2)   // Info2=2 -> Draw from guide
//         ||

		switch (TraceDrawingMode)
		{
		case 2:
//          if (InRange(DrawTrace1.x2,0.0)) {
//            return 0;
//          }
			break;
		}

// If mode2 = 0 and the tracelength > 0 try to snap on an object
// If there is an snap object found EndPointObjectNr contains the object nr

#ifdef _DEBUG

		if ((InRange9(CurrentDrawX2, 49.9e5)) && (InRange9(CurrentDrawY2, 29.6e5)))
			ok = 1;

#endif

		EndPointObjectNr = -1;

		if (((mode2 & 1) == 0) && (NotInRange(DrawTrace1.x2, 0.0)))
			EndPointObjectNr = AdjustTraceToObject(0);

#ifdef _DEBUG

		if (EndPointObjectNr != -1)
			ok = 1;

#endif

		if ((mode2 & 4) == 0)
			TraceDrawingOff(0);

		TraceChanged = 1;
		ResultInsertTrace = !InsertTraces(0, EndPointObjectNr);

		if (ResultInsertTrace)
		{
			SelectionEsc = 1;
//        ReCalcConnectionsNet(CurrentDrawingNetNr,0);
		}
		else
		{
			if ((mode2 & 2) == 0)
				TraceDrawingOn(0);
		}

		ok = 1;
		break;

	case 1:
		break;

	case 2:					// Go back
		TraceChanged = 1;

		if (CurrentWorkingTrace.ObjectType != 0)
		{
			DrawTrace1.x2 = 0.0;
			DrawTrace1.ObjectType = CurrentWorkingTrace.ObjectType;
			EndFirstDrawingTraceX = CurrentDrawX1;
			EndFirstDrawingTraceY = CurrentDrawY1;
			ResultInsertTrace = !InsertTraces(0, -1);

			if (ResultInsertTrace)
			{
				SelectionEsc = 1;
//          ReCalcConnectionsNet(CurrentDrawingNetNr,0);
			}
		}

		break;

	case 3:
		if ((NotInRange(CurrentDrawX1, CurrentDrawX2)) || (NotInRange(CurrentDrawY1, CurrentDrawY2)))
			LeftButtonPressed = 1;

		LeftButtonDoublePressed = 1;
		SpecialLeftButtonPressed = 1;
		break;

	case 4:
		if ((DrawTraceUsingGuide) && (DrawTrace1.x2 > 0.0))
		{
			if (!InsertTwoTracesOnGuide(0))
			{
				SelectionEsc = 1;
				ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
			}
		}

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 TryToInsertVia(int32 mode)
{
	int32 res, NewLayer;

	if (Design.NrBoardLayers == 1)
		return -1;

	NewLayer = GetDrawingLayer(1);

	if (NewLayer == -1)
		return -1;

	if ((mode & 2) == 0)
		TraceDrawingOff(0);

	res = InsertVia();

	switch (res)
	{
	case -1:
	case -2:
	case 0:
	case 1:
		if (mode == 0)
			TraceDrawingOn(0);

		break;

	case 2:
		DrawTrace1.x2 = 0.0;
		CurrentWorkingTrace.ObjectType = 0;

		if ((mode & 1) == 0)
			TraceChanging(0, 4 + 1);
		else
			TraceChanging(0, 4 + 2 + 1);

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void TraceDrawing(int32 mode)
{
	/*
	mode :

	0  :  Draw trace from guide
	1  :  Draw trace from trace
	2  :  Draw trace from pin/via

	4  :  Draw arc trace

	*/

	int32 mx, my, ok, ConnectionObjectNr, OldLayer, OldCurrentDrawingLayer, res, NewLayer, OldLastActionNr, OldNetNr,
	      NrTimeValues, TimeValues[16], TempLastActionNr, NewMode1, NewMode2;
	RECT Rect;
	int32 SelectionEscBackup;
	ObjectRecord *ConnectObject;
	NetRecord *Net;
	DrawXorFunctionRecord DrawXorFunction;

	/*

	TraceDrawingMode:

	  0  :  Drawing traces (0/45/90) degrees
	  1  :  All angle trace
	  2  :  Arc trace
	*/
//  TraceDrawingMode=(mode & 7) >> 2;

//  TraceDrawingMode=0;

	memset(&TimeValues, 0, sizeof(TimeValues));
	NrTimeValues = 0;
	ScrollCount = 0;
	RelX = CurrentDrawX1;
	RelY = CurrentDrawY1;
	OkToSwitchDrawingLayers = 0;
	FinishedTraceKeyPressed = 0;
	EditDesignRulesNetKeyPressed = 0;
	TraceDrawingInverted = 0;

	if (CurrentTraceWidth == 0)
		CurrentTraceWidth = Design.StandardTraceWidth;

	if (CurrentClearance == 0)
		CurrentClearance = Design.StandardClearance;

	OldLastActionNr = LastActionNr;
	NrPreviousTraces = 0;
	TracesInserted = 0;
	TraceChanged = 0;
	SelectionEsc = 0;
	memset(&LastAddedObject, 0, sizeof(ObjectRecord));
	Rect.left = (int32) DrawWindowMinX + ClientStartX;
	Rect.top = (int32) DrawWindowMinY + ClientStartY;
	Rect.right = (int32) DrawWindowMaxX + ClientStartX;
	Rect.bottom = (int32) DrawWindowMaxY + ClientStartY;

	CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
	TempLastActionNr = LastActionNr;
	OldX = CurrentDrawX2;
	OldY = CurrentDrawY2;
	CurrentDirection = GetNewDirection(CurrentDrawX1, CurrentDrawY1, CurrentDrawX2, CurrentDrawY2, 0);
	DrawFirstTrace = 0;
	DrawSecondTrace = 0;
//  DrawClearanceForTryingTrace=0;
//  DrawViaInfoForTryingTrace=0;
	memset(&NewTrace1, 0, sizeof(TraceRecord));
	memset(&NewTrace2, 0, sizeof(TraceRecord));

	NewTrace1.NetNr = (int16) CurrentDrawingNetNr;
	NewTrace1.Clearance = (float) CurrentClearance;
	NewTrace1.ThickNess = (float) CurrentTraceWidth;

	NewTrace2.NetNr = (int16) CurrentDrawingNetNr;
	NewTrace2.Clearance = (float) CurrentClearance;
	NewTrace2.ThickNess = (float) CurrentTraceWidth;
	ClipMouseCursor();			//  CrossHairX CrossHairY mode
//  DrawCrossHair(0);
	CrossHairMode = 1;
	TraceDrawingOn(0);
	SystemBusyMode = 1;
	NewMode1 = 16;
	NewMode2 = 0;
	DrawXorFunction.Function4a = (FUNCP4) TraceDrawingOff;
	DrawXorFunction.Function4b = (FUNCP4) TraceDrawingOn;
	DrawXorFunction.Param1[0] = &NewMode1;
	DrawXorFunction.Mode = 3;
	DrawXorFunction.Param2[0] = &NewMode2;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentDrawX2) || (OldY != CurrentDrawY2))
			{
				DrawTrace(CurrentDrawX1, CurrentDrawY1, OldX, OldY, CurrentTraceWidth, CurrentClearance,
				          CurrentDrawingNetNr, TraceDrawingMode);
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				DrawTrace(CurrentDrawX1, CurrentDrawY1, CurrentDrawX2, CurrentDrawY2, CurrentTraceWidth,
				          CurrentClearance, CurrentDrawingNetNr, TraceDrawingMode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				TraceDrawingOff(16);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				TraceDrawingOn(0);
				ScrollCount++;
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
#ifdef _DEBUG

				if (ScrollCount == 0)
					ok = 1;

#endif
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				TraceDrawingOff(16);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				TraceDrawingOn(0);
				ScrollCount++;
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				TraceDrawingOff(16);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				TraceDrawingOn(0);
				ScrollCount++;
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				TraceDrawingOff(16);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				TraceDrawingOn(0);
				ScrollCount++;
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;
			TraceDrawingOn(0);
			ZoomInOutProcessed = 0;
		}

		if (ShiftPressed)
		{
			if ((TraceDrawingMode == 0) && (CheckTimeOutTimer0(500)))
			{
				if (SelectTraceUnderCursor(0) == 0)
				{
					OldNetNr = CurrentDrawingNetNr;
					OldCurrentTraceWidth = CurrentTraceWidth;
					OldCurrentClearance = CurrentClearance;
					OldCurrentDrawingLayer = CurrentDrawingLayer;
					TraceDrawingOff(0);
					MoveOneTrace(1);
					CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
					CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
					OldX = CurrentDrawX2;
					OldY = CurrentDrawY2;
					CurrentDrawingNetNr = OldNetNr;
					CurrentTraceWidth = OldCurrentTraceWidth;
					CurrentClearance = OldCurrentClearance;
					CurrentDrawingLayer = OldCurrentDrawingLayer;
					GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);

					if (!SelectionEsc)
						TraceDrawingOn(1);

					ShiftPressed = 0;
				}
			}
		}

		if (!Focused)
		{
			TraceDrawingOff(0);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				TraceDrawingOn(0);

		}

// ***********************************************************************************
		if ((ZoomActive()) && (!SelectionEsc))
		{
			TraceDrawingOff(0);
			ZoomWindow();
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;

			if (!SelectionEsc)
				TraceDrawingOn(0);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			TraceDrawingOff(0);
			PanWindow();
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;

			if (!SelectionEsc)
				TraceDrawingOn(0);
		}

// ***********************************************************************************
		if (CheckLeftButton())
		{
//      if ((NotInRange(CurrentDrawX1,CurrentDrawX2))
//         ||
//         (NotInRange(CurrentDrawY1,CurrentDrawY2))) {
// DrawTrace1
			TraceChanging(0, 0);
//      }
			CheckInputMessages(0);
			ok = 1;
		}

// ***********************************************************************************
		if (LeftButtonDoublePressed)
		{
			if (TraceDrawingMode == 0)
				TryToInsertVia(0);

			LeftButtonDoublePressed = 0;
		}

// ***********************************************************************************
		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			TraceDrawingOff(0);
			mx = MousePosX;
			my = MousePosY;
			GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
			ConnectionObjectNr = GetObjectNrFromEndPoint(-1, CurrentDrawX1, CurrentDrawY1, 1);
			OkToSwitchDrawingLayers = 0;

			if ((ConnectionObjectNr != -1) && (Design.NrBoardLayers > 1))
			{
				ConnectObject = &((*Objects3)[ConnectionObjectNr]);

				switch (ConnectObject->ObjectType)
				{
				case DRILL:
				case VIA_PUT_THROUGH_ROUND:
				case PIN_PUT_THROUGH_ROUND:
				case PIN_PUT_THROUGH_SQUARE:
				case PIN_PUT_THROUGH_POLYGON:
					OkToSwitchDrawingLayers = 1;
					break;
				}
			}

			OldCurrentDrawingLayer = CurrentDrawingLayer;
			OldLayer = CurrentDrawingLayer;
			TraceMenuPopUp(0);
			RightButtonPressed = 0;
			CheckInputMessages(0);

			if (DrawLayerCode[CurrentDrawingLayer] >= 0x10)
				CurrentDrawingLayer = OldLayer;

			if (EditDesignRulesNetKeyPressed)
			{
//        NetTypesDialog(CurrentDrawingNetNr);
				PcbSettingsDialog(IDD_DIALOG_NETTYPE, CurrentDrawingNetNr);
				Net = &((*Nets)[CurrentDrawingNetNr]);
				CurrentTraceWidth = Net->TraceWidth;
				CurrentClearance = Net->TraceClearance;
				EditDesignRulesNetKeyPressed = 0;
			}

			if (TraceBackWardsKeyPressed)
			{
				DrawTrace1.x2 = 0.0;
				TraceChanging(0, 7);
				TraceBackWardsKeyPressed = 0;
			}

			if (ChangeTraceDrawingModeKeyPressed)
			{
				TraceDrawingMode = (TraceDrawingMode + 1) % 4;
				ChangeTraceDrawingModeKeyPressed = 0;
			}

			if ((OldCurrentDrawingLayer != CurrentDrawingLayer) && ((CurrentWorkingTrace.Info2 & 1) == 1)
			        && (CurrentWorkingTrace.ObjectType != 0))
			{
				NewLayer = CurrentDrawingLayer;
				CurrentDrawingLayer = OldCurrentDrawingLayer;

				if (!OkToRePaintAfterTraceDrawing)
					DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 0);
				else
					DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 1);

				CurrentDrawingLayer = NewLayer;
				ok = 1;
				CurrentWorkingTrace.TraceNr = -1;
				CurrentWorkingTrace.Info2 = 2;
				GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
			}

			if (OldCurrentDrawingLayer != CurrentDrawingLayer)
			{
				RePaint();
				CheckInputMessages(0);
				CheckInputMessages(0);
			}

			SetCursorPos((int32) mx, (int32) my);
			MousePosX = (int32) mx;
			MousePosY = (int32) my;
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;

			if (!SelectionEsc)
				TraceDrawingOn(0);
			else
			{
				if (TraceChanged)
					ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
			}
		}

// ***********************************************************************************
		if (NrFunctionsInBuf > 0)
		{
#ifdef _DEBUG
			NrTimeValues = 0;
			SetTimer1();
#endif
			OldCurrentDrawingLayer = CurrentDrawingLayer;
			UnClipMouseCursor();
			TraceDrawingOff(0);
#ifdef _DEBUG
			TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer1inMilliSeconds();
#endif
			ExecuteKeys();
#ifdef _DEBUG
			TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer1inMilliSeconds();
#endif
			CheckInputMessages(0);
			CheckInputMessages(0);
#ifdef _DEBUG
			TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer1inMilliSeconds();
#endif

			if (SelectionEsc)
			{
				ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
				LastActionNr++;
			}

			if (FinishedTraceKeyPressed)
			{
				TraceChanging(4, 0);
				FinishedTraceKeyPressed = 0;
			}

			if (ChangeTraceDrawingModeKeyPressed)
			{
				TraceDrawingMode = (TraceDrawingMode + 1) % 4;
				ChangeTraceDrawingModeKeyPressed = 0;
			}

			if (TraceBackWardsKeyPressed)
			{
				DrawTrace1.x2 = 0.0;
				TraceChanging(0, 7);
//        TraceChanging(0,5);
				TraceBackWardsKeyPressed = 0;
			}

			if (HelpAsked)
			{
				if (!AddExtraTraceMode)
					Help("trace_popup_menu.htm", 0);
				else
					Help("add_extra_trace.htm", 0);

				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			if (EditDesignRulesNetKeyPressed)
			{
//        NetTypesDialog(CurrentDrawingNetNr);
				PcbSettingsDialog(IDD_DIALOG_NETTYPE, CurrentDrawingNetNr);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				Net = &((*Nets)[CurrentDrawingNetNr]);
				CurrentTraceWidth = Net->TraceWidth;
				CurrentClearance = Net->TraceClearance;
				DrawTrace1.y2 = CurrentTraceWidth;
				DrawTrace1.Clearance = CurrentClearance;
				DrawTrace2.y2 = CurrentTraceWidth;
				DrawTrace2.Clearance = CurrentClearance;
				EditDesignRulesNetKeyPressed = 0;
			}

			if (InsertViaKeyPressed)
			{
				if (Design.NrBoardLayers > 1)
				{
					if ((NotInRange(CurrentDrawX1, CurrentDrawX2)) || (NotInRange(CurrentDrawY1, CurrentDrawY2)))
					{
						TraceChanging(0, 5);
						TryToInsertVia(1);
					}
					else
						TryToInsertVia(3);
				}

				InsertViaKeyPressed = 0;
			}

#ifdef _DEBUG
			TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer2inMilliSeconds();
#endif

			if ((OldCurrentDrawingLayer != CurrentDrawingLayer) && ((CurrentWorkingTrace.Info2 & 1) == 1)
			        && (CurrentWorkingTrace.ObjectType != 0))
			{
				NewLayer = CurrentDrawingLayer;
				CurrentDrawingLayer = OldCurrentDrawingLayer;
				/*
				        DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr);
				*/
				CurrentDrawingLayer = NewLayer;
				ok = 1;
//        CurrentWorkingTrace.TraceNr=-1;
//        CurrentWorkingTrace.Info2=0;
				/*
				        GetObjectsNet(CurrentDrawingNetNr,MODE_OBJECTS3,0);
				*/
			}

#ifdef _DEBUG
			TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer2inMilliSeconds();
#endif

			if (OldCurrentDrawingLayer != CurrentDrawingLayer)
			{
				SelectionEscBackup = SelectionEsc;
//        RePaint();
//        CheckInputMessages(0);
#ifdef _DEBUG
				TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer2inMilliSeconds();
#endif
//        CheckInputMessages(0);
				SelectionEsc = SelectionEscBackup;
			}

			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;
#ifdef _DEBUG
			TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer2inMilliSeconds();
#endif

			if (!SelectionEsc)
				TraceDrawingOn(0);

			ok = 1;
#ifdef _DEBUG
			TimeValues[NrTimeValues++] = (__LINE__) * 10000 + GetDifferenceTimer2inMilliSeconds();
#endif
			res = 1;			// res1
			ClipMouseCursor();
		}
	}

// ***********************************************************************************
#ifndef TRACE_REPAINT

	if (TraceDrawingActive)
		TraceDrawingOff(0);

#endif
	res = 1;
	CrossHairType = 0;
	CrossHairMode = 0;
#ifndef TRACE_REPAINT
	DrawCrossHair(16 + 2);
#else
	RePaint();
#endif
	UnClipMouseCursor();
	CurrentWorkingTrace.Info2 = 0;

	if (TracesInserted > 0)
		LastActionNr--;

	SystemBusyMode = 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTrace(double x1, double y1, double x2, double y2, double TraceWidth, double TraceClearanceWidth, int32 NetNr,
               int32 mode)
/*
mode:

  0  :  Drawing traces (0/45/90) degrees
  1  :  All angle trace
  2  :  Arc trace

x2,y2 : New point to draw trace to

*/
{
	int32 dir, dir2, ok, dv, dv2, oldd, res, NewDirection, cx, cy;
	double divx, divy, divxabs, divyabs, mindivxy, x11, y11, x22, y22, x33, y33, x3, y3, x3a, x3b, ExcludeX1, ExcludeY1,
	       ExcludeX2, ExcludeY2;
	int32 DrawOk = 0;
	ObjectRecord ViaObject;
	NetRecord *Net;

	dir = GetDirection(x1, y1, x2, y2, 0);
	dir2 = GetNewDirection(x1, y1, x2, y2, 0);
	NewDirection = GetDirection(x1, y1, x2, y2, 1);
	oldd = CurrentDirection;

	ExcludeX1 = 0.0;
	ExcludeY1 = 0.0;
	ExcludeX2 = 0.0;
	ExcludeY2 = 0.0;

	if (CurrentDirection >= 0)
	{
		dv = CurrentDirection - dir;

		if (dv < -16)
			dv += 32;

		if (dv > 16)
			dv -= 32;

		dv2 = abs(dv);

		if (dv2 >= 4)
			CurrentDirection = dir2;
	}
	else
		CurrentDirection = dir2;

	divx = x1 - x2;
	divy = y1 - y2;
	divxabs = fabs(divx);
	divyabs = fabs(divy);
	mindivxy = min(divxabs, divyabs);
	x11 = x1;
	y11 = y1;

	StartFirstDrawingTraceX = x1;
	StartFirstDrawingTraceY = y1;
	EndSecondDrawingTraceX = x2;
	EndSecondDrawingTraceY = y2;

	DrawTrace1.Info2 = -1;
	DrawTrace2.Info2 = -1;
	DrawTrace1.Info = 0;
	DrawTrace2.Info = 0;
	DrawTrace1.NetNr = NetNr;
	DrawTrace2.NetNr = NetNr;
	DrawTrace1.Layer = CurrentDrawingLayer;
	DrawTrace2.Layer = CurrentDrawingLayer;
	DrawTrace1.Clearance = TraceClearanceWidth;
	DrawTrace2.Clearance = TraceClearanceWidth;
	DrawTrace2.ObjectType = 0;
	DrawTrace1.Thickness = TraceWidth;
	DrawTrace2.y2 = TraceWidth;

// *******************************************************************************************************
// *******************************************************************************************************
	switch (TraceDrawingMode & 0x0f)
	{
	case 0:					// Drawing traces (0/45/90) degrees
		DrawTrace1.x1 = x1;
		DrawTrace1.y1 = y1;
		DrawTrace1.x2 = 0.0;
		DrawTrace1.ObjectType = TRACE_HOR;

		DrawTrace1.y2 = TraceWidth;
		DrawTrace2.y2 = TraceWidth;
		DrawTrace1.Thickness = TraceWidth;
		DrawTrace2.Thickness = TraceWidth;
		DrawTrace2.ObjectType = 0;


		if (dir >= 0)
		{

			/******************************************************************************************/

			if ((dir == 0) || ((dir == 1) && (CurrentDirection == 0)) || ((dir == 3) && (CurrentDirection == 0))
			        || ((dir == 31) && (CurrentDirection == 0)) || ((dir == 29) && (CurrentDirection == 0)))
			{
				x22 = x1;
				y22 = y2 - divxabs;
				//      Trace1Dir=0;
				DrawTrace1.Info2 = 0;
				DrawTrace1.ObjectType = TRACE_VER;
				DrawTrace1.x1 = x1;
				DrawTrace1.y1 = y1;
				DrawTrace1.x2 = divyabs - divxabs;
				StartSecondDrawingTraceX = x1;
				StartSecondDrawingTraceY = y1 + divyabs - divxabs;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divx < 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=1;
					DrawTrace2.Info2 = 1;
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = x1;
					DrawTrace2.y1 = y2 - divxabs;
					DrawTrace2.x2 = divxabs;
				}

				if (divx > 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=7;
					DrawTrace2.Info2 = 7;
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y2;
					DrawTrace2.x2 = divxabs;
				}
			}

			/******************************************************************************************/

			if ((dir == 4) || ((dir == 1) && (CurrentDirection == 4)) || ((dir == 3) && (CurrentDirection == 4))
			        || ((dir == 5) && (CurrentDirection == 4)) || ((dir == 7) && (CurrentDirection == 4)))
			{
				x22 = x1 + mindivxy;
				y22 = y1 + mindivxy;
				//      Trace1Dir=1;
				DrawTrace1.Info2 = 1;
				DrawTrace1.ObjectType = TRACE_DIAG2;
				DrawTrace1.x1 = x1;
				DrawTrace1.y1 = y1;
				DrawTrace1.x2 = mindivxy;
				StartSecondDrawingTraceX = x1 + mindivxy;
				StartSecondDrawingTraceY = y1 + mindivxy;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divxabs < divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=0;
					DrawTrace2.Info2 = 0;
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = x1 + mindivxy;
					DrawTrace2.y1 = y1 + mindivxy;
					DrawTrace2.x2 = divyabs - mindivxy;
				}

				if (divxabs > divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=2;
					DrawTrace2.Info2 = 2;
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = x1 + mindivxy;
					DrawTrace2.y1 = y1 + mindivxy;
					DrawTrace2.x2 = divxabs - mindivxy;
				}
			}


			/******************************************************************************************/

			if ((dir == 8) || ((dir == 5) && (CurrentDirection == 8)) || ((dir == 7) && (CurrentDirection == 8))
			        || ((dir == 9) && (CurrentDirection == 8)) || ((dir == 11) && (CurrentDirection == 8)))
			{
				x22 = x2 - mindivxy;
				y22 = y1;
				//      Trace1Dir=2;
				DrawTrace1.Info2 = 2;
				DrawTrace1.ObjectType = TRACE_HOR;
				DrawTrace1.x1 = x1;
				DrawTrace1.y1 = y1;
				DrawTrace1.x2 = divxabs - mindivxy;
				StartSecondDrawingTraceX = x1 + divxabs - mindivxy;
				StartSecondDrawingTraceY = y1;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divy < 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=1;
					DrawTrace2.Info2 = 1;
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = x2 - mindivxy;
					DrawTrace2.y1 = y1;
					DrawTrace2.x2 = mindivxy;
				}

				if (divy > 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=3;
					DrawTrace2.Info2 = 3;
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = x2 - mindivxy;
					DrawTrace2.y1 = y1;
					DrawTrace2.x2 = mindivxy;
				}
			}

			/******************************************************************************************/

			if ((dir == 12) || ((dir == 9) && (CurrentDirection == 12)) || ((dir == 11) && (CurrentDirection == 12))
			        || ((dir == 13) && (CurrentDirection == 12)) || ((dir == 15) && (CurrentDirection == 12)))
			{
				x22 = x1 + mindivxy;
				y22 = y1 - mindivxy;
				//      Trace1Dir=3;
				DrawTrace1.Info2 = 3;
				DrawTrace1.ObjectType = TRACE_DIAG1;
				DrawTrace1.x1 = x1;
				DrawTrace1.y1 = y1;
				DrawTrace1.x2 = mindivxy;
				StartSecondDrawingTraceX = x1 + mindivxy;
				StartSecondDrawingTraceY = y1 - mindivxy;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divxabs < divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=4;
					DrawTrace2.Info2 = 4;
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y2;
					DrawTrace2.x2 = divyabs - mindivxy;
				}

				if (divxabs > divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=2;
					DrawTrace2.Info2 = 2;
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = x1 + mindivxy;
					DrawTrace2.y1 = y1 - mindivxy;
					DrawTrace2.x2 = divxabs - mindivxy;
				}
			}


			/******************************************************************************************/

			if ((dir == 16) || ((dir == 13) && (CurrentDirection == 16)) || ((dir == 15) && (CurrentDirection == 16))
			        || ((dir == 17) && (CurrentDirection == 16)) || ((dir == 19) && (CurrentDirection == 16)))
			{
				x22 = x1;
				y22 = y2 + mindivxy;
				//      Trace1Dir=4;
				DrawTrace1.Info2 = 4;
				DrawTrace1.ObjectType = TRACE_VER;
				DrawTrace1.x1 = x1;
				DrawTrace1.y1 = y2 + mindivxy;
				DrawTrace1.x2 = divyabs - mindivxy;
				StartSecondDrawingTraceX = x1;
				StartSecondDrawingTraceY = y2 + mindivxy;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divx < 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=3;
					DrawTrace2.Info2 = 3;
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = x1;
					DrawTrace2.y1 = y2 + mindivxy;
					DrawTrace2.x2 = mindivxy;
				}

				if (divx > 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=5;
					DrawTrace2.Info2 = 5;
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = x1 - divxabs;
					DrawTrace2.y1 = y2;
					DrawTrace2.x2 = divxabs;
				}
			}

			/******************************************************************************************/

			if ((dir == 20) || ((dir == 17) && (CurrentDirection == 20)) || ((dir == 19) && (CurrentDirection == 20))
			        || ((dir == 21) && (CurrentDirection == 20)) || ((dir == 23) && (CurrentDirection == 20)))
			{
				x22 = x1 - mindivxy;
				y22 = y1 - mindivxy;
				//      Trace1Dir=5;
				DrawTrace1.Info2 = 5;
				DrawTrace1.ObjectType = TRACE_DIAG2;
				DrawTrace1.x1 = x1 - mindivxy;
				DrawTrace1.y1 = y1 - mindivxy;
				DrawTrace1.x2 = mindivxy;
				StartSecondDrawingTraceX = x1 - mindivxy;
				StartSecondDrawingTraceY = y1 - mindivxy;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divxabs < divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=4;
					DrawTrace2.Info2 = 4;
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y2;
					DrawTrace2.x2 = divyabs - mindivxy;
				}

				if (divxabs > divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=6;
					DrawTrace2.Info2 = 6;
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y2;
					DrawTrace2.x2 = divxabs - mindivxy;
				}
			}


			/******************************************************************************************/

			if ((dir == 24) || ((dir == 21) && (CurrentDirection == 24)) || ((dir == 23) && (CurrentDirection == 24))
			        || ((dir == 25) && (CurrentDirection == 24)) || ((dir == 27) && (CurrentDirection == 24)))
			{
				x22 = x2 + divyabs;
				y22 = y1;
				//      Trace1Dir=6;
				DrawTrace1.Info2 = 6;
				DrawTrace1.ObjectType = TRACE_HOR;
				DrawTrace1.x1 = x2 + mindivxy;
				DrawTrace1.y1 = y1;
				DrawTrace1.x2 = divxabs - mindivxy;
				StartSecondDrawingTraceX = x2 + mindivxy;
				StartSecondDrawingTraceY = y1;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divy < 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=7;
					DrawTrace2.Info2 = 7;
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y2;
					DrawTrace2.x2 = mindivxy;
				}

				if (divy > 0)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=5;
					DrawTrace2.Info2 = 5;
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y2;
					DrawTrace2.x2 = mindivxy;
				}
			}

			/******************************************************************************************/

			if ((dir == 28) || ((dir == 25) && (CurrentDirection == 28)) || ((dir == 27) && (CurrentDirection == 28))
			        || ((dir == 29) && (CurrentDirection == 28)) || ((dir == 31) && (CurrentDirection == 28)))
			{
				x22 = x1 - mindivxy;
				y22 = y1 + mindivxy;
				//      Trace1Dir=7;
				DrawTrace1.Info2 = 7;
				DrawTrace1.ObjectType = TRACE_DIAG1;
				DrawTrace1.x1 = x1 - mindivxy;
				DrawTrace1.y1 = y1 + mindivxy;
				DrawTrace1.x2 = mindivxy;
				StartSecondDrawingTraceX = x1 - mindivxy;
				StartSecondDrawingTraceY = y1 + mindivxy;
				EndFirstDrawingTraceX = StartSecondDrawingTraceX;
				EndFirstDrawingTraceY = StartSecondDrawingTraceY;

				if (DrawTrace1.x2 > 0)
					DrawOk = 1;

				if (divxabs < divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=0;
					DrawTrace2.Info2 = 0;
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y1 + mindivxy;
					DrawTrace2.x2 = divyabs - mindivxy;
				}

				if (divxabs > divyabs)
				{
					x33 = x2;
					y33 = y2;
					//        Trace2Dir=6;
					DrawTrace2.Info2 = 6;
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = x2;
					DrawTrace2.y1 = y1 + mindivxy;
					DrawTrace2.x2 = divxabs - mindivxy;
				}
			}
		}
		else
		{
			DrawTrace1.x2 = 0.0;
			DrawTrace1.ObjectType = TRACE_HOR;
			ok = 1;
		}

		break;

// *******************************************************************************************************
// *******************************************************************************************************
	case 1:					// All angle trace
		DrawTrace1.ObjectType = TRACE_ALL_ANGLE;
		DrawTrace1.x1 = x1;
		DrawTrace1.y1 = y1;
		DrawTrace1.x2 = x2;
		DrawTrace1.y2 = y2;
		DrawTrace2.ObjectType = 0;
		DrawTrace2.Info2 = -1;
		StartSecondDrawingTraceX = x2;
		StartSecondDrawingTraceY = y2;
		EndFirstDrawingTraceX = StartSecondDrawingTraceX;
		EndFirstDrawingTraceY = StartSecondDrawingTraceY;
		break;

// *******************************************************************************************************
// *******************************************************************************************************
	case 2:					// Arc trace
	case 3:					// Arc trace
		DrawTrace1.ObjectType = 0;
		DrawTrace2.ObjectType = 0;
		DrawTrace1.Info2 = -1;
		DrawTrace2.Info2 = -1;
		DrawTrace1.x2 = 0.0;
		DrawTrace2.x2 = 0.0;
		x3 = 0.0;

		switch (PreviousDirection)
		{	// GetNewDirection
// *******************************************************************************************************
		case 0:				// Direction coming from bottom
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 29) || (dir == 31))
				{	// Left top
					x3 = (y2 + x2) - (y1 + x1);
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1 - x3b;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3b;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 28;
					StartSecondDrawingTraceX = x1 - x3a + x3;
					StartSecondDrawingTraceY = y1 + x3;

					//   VALUE_1_MIN_SQRT05
					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 28;
					}
				}

				if ((dir == 1) || (dir == 3))
				{	// Right top
					x3 = (y2 - x2) - (y1 - x1);
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1 + x3b;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = -x3b;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 4;
					StartSecondDrawingTraceX = x1 + x3b - x3a;
					StartSecondDrawingTraceY = y1 + x3a;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 4;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 25) || (dir == 27) || (dir == 28))
				{	// Left
					x3 = divyabs;
					DrawTrace1.x1 = x1 - x3;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = x3;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = x3;
					DrawTrace1.Info2 = 24;
					StartSecondDrawingTraceX = x1 - x3;
					StartSecondDrawingTraceY = y1 + x3;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 24;
					}
				}

				if ((dir == 4) || (dir == 5) || (dir == 7))
				{	// Right
					x3 = divyabs;
					DrawTrace1.x1 = x1 + x3;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = x3;
					DrawTrace1.x4 = -x3;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 8;
					StartSecondDrawingTraceX = x1 + x3;
					StartSecondDrawingTraceY = y2;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x1 + x3;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 8;
					}
				}

				break;
			}

			break;

// *******************************************************************************************************
		case 4:				// Direction coming from left bottom
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 1) || (dir == 3))
				{	// Top
					x3 = divxabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = x3;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 0;
					StartSecondDrawingTraceX = x2;
					StartSecondDrawingTraceY = y1 + x3a;

					if (y2 > StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = y2 - StartSecondDrawingTraceY;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 0;
					}
				}

				if ((dir == 5) || (dir == 7))
				{	// Right
					x3 = divyabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 8;
					StartSecondDrawingTraceX = x1 + x3a;
					StartSecondDrawingTraceY = y1 + x3;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 8;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 29) || (dir == 31) || (dir == 0))
				{	// Left top
					x3 = divyabs - divxabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 28;
					StartSecondDrawingTraceX = x1;
					StartSecondDrawingTraceY = y1 + x3;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 28;
					}
				}

				if ((dir == 8) || (dir == 9) || (dir == 11))
				{	// Right bottom
					x3 = divxabs - divyabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 12;
					StartSecondDrawingTraceX = x1 + x3a + x3a;
					StartSecondDrawingTraceY = y1;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 12;
					}
				}

				break;
			}

			break;

// *******************************************************************************************************
		case 8:				// Direction coming from left
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 5) || (dir == 7))
				{	// Right top
					x3 = (x2 - y2) - (x1 - y1);
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 + x3b;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = -x3b;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 4;
					StartSecondDrawingTraceX = x1 + x3a;
					StartSecondDrawingTraceY = y1 + x3b - x3a;

					//   VALUE_1_MIN_SQRT05
					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 4;
					}
				}

				if ((dir == 9) || (dir == 11))
				{	// Right bottom
					x3 = (y2 + x2) - (y1 + x1);
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 - x3b;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = x3b;
					DrawTrace1.Info2 = 12;
					StartSecondDrawingTraceX = x1 + x3a;
					StartSecondDrawingTraceY = y1 - x3b + x3a;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 12;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 1) || (dir == 3) || (dir == 4))
				{	// Top
					x3 = divxabs;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 + x3;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = -x3;
					DrawTrace1.x4 = x3;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 0;
					StartSecondDrawingTraceX = x1 + x3;
					StartSecondDrawingTraceY = y1 + x3;

					if (y2 > StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = y2 - StartSecondDrawingTraceY;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 0;
					}
				}

				if ((dir == 12) || (dir == 13) || (dir == 15))
				{	// Bottom
					x3 = divxabs;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 - x3;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = x3;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = x3;
					DrawTrace1.Info2 = 16;
					StartSecondDrawingTraceX = x1 + x3;
					StartSecondDrawingTraceY = y1 - x3;

					if (y2 < StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceY - y2;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 16;
					}
				}

				break;
			}

			break;

// *******************************************************************************************************
		case 12:				// Direction coming from left top
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 9) || (dir == 11))
				{	// Right
					x3 = divyabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = -x3b;
					DrawTrace1.Info2 = 8;
					StartSecondDrawingTraceX = x1 + x3a;
					StartSecondDrawingTraceY = y1 + x3a - x3b;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 8;
					}
				}

				if ((dir == 13) || (dir == 15))
				{	// Bottom
					x3 = divxabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 16;
					StartSecondDrawingTraceX = x1 + x3;
					StartSecondDrawingTraceY = y1 - x3a;

					if (y2 < StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceY - y2;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 16;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 5) || (dir == 7) || (dir == 8))
				{	// Right top
					x3 = divxabs - divyabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 4;
					StartSecondDrawingTraceX = x1 + x3a + x3a;
					StartSecondDrawingTraceY = y1;

					if (y2 > StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = y2 - StartSecondDrawingTraceY;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 4;
					}
				}

				if ((dir == 16) || (dir == 17) || (dir == 19))
				{	// Left bottom
					x3 = divyabs - divxabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 20;
					StartSecondDrawingTraceX = x1;
					StartSecondDrawingTraceY = y1 - x3a - x3a;

					if (y2 < StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceY - y2;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 20;
					}
				}

				break;
			}

			break;

// *******************************************************************************************************
		case 16:				// Direction coming from top
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 13) || (dir == 15))
				{	// Right bottom
					x3 = fabs((y2 + x2) - (y1 + x1));
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1 + x3b;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3b;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 12;
					StartSecondDrawingTraceX = x1 + x3b - x3a;
					StartSecondDrawingTraceY = y1 - x3a;

					//   VALUE_1_MIN_SQRT05
					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 12;
					}
				}

				if ((dir == 17) || (dir == 19))
				{	// Left bottom
					x3 = fabs((y2 - x2) - (y1 - x1));
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1 - x3b;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = x3b;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 20;
					StartSecondDrawingTraceX = x1 - x3b + x3a;
					StartSecondDrawingTraceY = y1 - x3a;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 20;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 9) || (dir == 11) || (dir == 12))
				{	// Right
					x3 = divyabs;
					DrawTrace1.x1 = x1 + x3;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = -x3;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = -x3;
					DrawTrace1.Info2 = 8;
					StartSecondDrawingTraceX = x1 + x3;
					StartSecondDrawingTraceY = y1 - x3;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 8;
					}
				}

				if ((dir == 20) || (dir == 21) || (dir == 23))
				{	// Left
					x3 = divyabs;
					DrawTrace1.x1 = x1 - x3;
					DrawTrace1.y1 = y1;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = -x3;
					DrawTrace1.x4 = x3;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 24;
					StartSecondDrawingTraceX = x1 - x3;
					StartSecondDrawingTraceY = y2;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 24;
					}
				}

				break;
			}

			break;

// *******************************************************************************************************
		case 20:				// Direction coming from right top
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 17) || (dir == 19))
				{	// Bottom
					x3 = divxabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = -x3;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 16;
					StartSecondDrawingTraceX = x2;
					StartSecondDrawingTraceY = y1 - x3a;

					if (y2 < StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceY - y2;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 16;
					}
				}

				if ((dir == 21) || (dir == 23))
				{	// Left
					x3 = divyabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 24;
					StartSecondDrawingTraceX = x1 - x3a;
					StartSecondDrawingTraceY = y1 - x3;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 24;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 13) || (dir == 15) || (dir == 16))
				{	// Right bottom
					x3 = divyabs - divxabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 12;
					StartSecondDrawingTraceX = x1;
					StartSecondDrawingTraceY = y1 - x3;

					if (x2 > StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = x2 - StartSecondDrawingTraceX;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 12;
					}
				}

				if ((dir == 24) || (dir == 25) || (dir == 27))
				{	// Left top
					x3 = divxabs - divyabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 28;
					StartSecondDrawingTraceX = x1 - x3a - x3a;
					StartSecondDrawingTraceY = y1;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 28;
					}
				}

				break;
			}

			break;

// *******************************************************************************************************
		case 24:				// Direction coming from right
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 21) || (dir == 28))
				{	// Left bottom
					x3 = fabs((x2 - y2) - (x1 - y1));
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 - x3b;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = x3b;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 20;
					StartSecondDrawingTraceX = x1 - x3a;
					StartSecondDrawingTraceY = y1 - x3b + x3a;

					//   VALUE_1_MIN_SQRT05
					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 20;
					}
				}

				if ((dir == 25) || (dir == 27))
				{	// Left top
					x3 = fabs((y2 + x2) - (y1 + x1));
					x3a = x3 * SQRT05 / VALUE_SQRT2_MIN_1;
					x3b = x3a * SQRT2;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 + x3b;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = -x3a;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = -x3b;
					DrawTrace1.Info2 = 28;
					StartSecondDrawingTraceX = x1 - x3a;
					StartSecondDrawingTraceY = y1 + x3b - x3a;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_DIAG1;
						DrawTrace2.Info2 = 28;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 17) || (dir == 19) || (dir == 20))
				{	// Bottom
					x3 = divxabs;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 - x3;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = 0.0;
					DrawTrace1.y3 = x3;
					DrawTrace1.x4 = -x3;
					DrawTrace1.y4 = 0.0;
					DrawTrace1.Info2 = 16;
					StartSecondDrawingTraceX = x1 - x3;
					StartSecondDrawingTraceY = y1 - x3;

					if (y2 < StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceY - y2;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 16;
					}
				}

				if ((dir == 28) || (dir == 29) || (dir == 31))
				{	// Top
					x3 = divxabs;
					DrawTrace1.x1 = x1;
					DrawTrace1.y1 = y1 + x3;
					DrawTrace1.x2 = x3 * 2.0;
					DrawTrace1.y2 = x3 * 2.0;
					DrawTrace1.x3 = -x3;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = -x3;
					DrawTrace1.Info2 = 0;
					StartSecondDrawingTraceX = x1 - x3;
					StartSecondDrawingTraceY = y1 + x3;

					if (y2 > StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = y2 - StartSecondDrawingTraceY;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 0;
					}
				}

				break;
			}

			break;

// *******************************************************************************************************
		case 28:				// Direction coming from right bottom
			DrawTrace1.ObjectType = TRACE_ARC;

			switch (TraceDrawingMode)
			{
			case 3:			// 45 degrees
				if ((dir == 25) || (dir == 27))
				{	// Left
					x3 = divyabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = 0.0;
					DrawTrace1.y4 = x3b;
					DrawTrace1.Info2 = 24;
					StartSecondDrawingTraceX = x1 - x3a;
					StartSecondDrawingTraceY = y1 - x3a + x3b;

					if (x2 < StartSecondDrawingTraceX)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceX - x2;
						DrawTrace2.ObjectType = TRACE_HOR;
						DrawTrace2.Info2 = 24;
					}
				}

				if ((dir == 29) || (dir == 31))
				{	// Top
					x3 = divxabs;
					x3a = x3 / VALUE_SQRT2_MIN_1;
					x3b = x3 * SQRT2 / VALUE_SQRT2_MIN_1;	// Radius
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = 0.0;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 0;
					StartSecondDrawingTraceX = x1 - x3;
					StartSecondDrawingTraceY = y1 + x3a;

					if (y2 > StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = y2 - StartSecondDrawingTraceY;
						DrawTrace2.ObjectType = TRACE_VER;
						DrawTrace2.Info2 = 0;
					}
				}

				break;

			case 2:			// 90 degrees
				if ((dir == 21) || (dir == 23) || (dir == 25))
				{	// Left bottom
					x3 = divxabs - divyabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 - x3a;
					DrawTrace1.y1 = y1 - x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = x3a;
					DrawTrace1.Info2 = 20;
					StartSecondDrawingTraceX = x1 - x3a - x3a;
					StartSecondDrawingTraceY = y1;

					if (y2 < StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = x2;
						DrawTrace2.y1 = y2;
						DrawTrace2.x2 = StartSecondDrawingTraceY - y2;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 20;
					}
				}

				if ((dir == 0) || (dir == 1) || (dir == 3))
				{	// Right top
					x3 = divyabs - divxabs;
					x3a = x3 * 0.5;
					x3b = x3 * SQRT05;
					DrawTrace1.x1 = x1 + x3a;
					DrawTrace1.y1 = y1 + x3a;
					DrawTrace1.x2 = x3b * 2.0;
					DrawTrace1.y2 = x3b * 2.0;
					DrawTrace1.x3 = -x3a;
					DrawTrace1.y3 = x3a;
					DrawTrace1.x4 = -x3a;
					DrawTrace1.y4 = -x3a;
					DrawTrace1.Info2 = 4;
					StartSecondDrawingTraceX = x1;
					StartSecondDrawingTraceY = y1 + x3a + x3a;

					if (y2 > StartSecondDrawingTraceY)
					{
						DrawTrace2.x1 = StartSecondDrawingTraceX;
						DrawTrace2.y1 = StartSecondDrawingTraceY;
						DrawTrace2.x2 = y2 - StartSecondDrawingTraceY;
						DrawTrace2.ObjectType = TRACE_DIAG2;
						DrawTrace2.Info2 = 4;
					}
				}

				break;
			}

			break;
		}

		EndFirstDrawingTraceX = StartSecondDrawingTraceX;
		EndFirstDrawingTraceY = StartSecondDrawingTraceY;
		break;
	}

// *******************************************************************************************************
	DrawCode = DrawLayerCode[CurrentDrawingLayer];
	Net = &((*Nets)[CurrentDrawingNetNr]);

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
		DrawTrace1.Info |= OBJECT_HIGHLITED;
	else
		DrawTrace1.Info &= ~OBJECT_HIGHLITED;

	switch (TraceDrawingMode)
	{
	case 0:
		DrawObject(&DrawTrace1, 0x10);
		TraceDrawingInverted = !TraceDrawingInverted;

		/*
		#ifdef _DEBUG
		      GetWindowText(PCBWindow,str,150);
		      if (TraceDrawingInverted) {
		        strcpy(str," : Trace inverted");
		      } else {
		        strcpy(str," : Trace normal");
		      }
		      SetWindowTextUTF8(PCBWindow,str);
		#endif
		*/
		if (DrawClearanceForTryingTrace)
		{
			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObject(&DrawTrace1, 0x40 + 8);
		}

		if (DrawTrace1.x2 == 0)
		{
			StartSecondDrawingTraceX = x1;
			StartSecondDrawingTraceY = y1;
			EndFirstDrawingTraceX = StartSecondDrawingTraceX;
			EndFirstDrawingTraceY = StartSecondDrawingTraceY;
		}

		if ((DrawViaInfoForTryingTrace) && (Design.NrBoardLayers > 1))
		{
			ViaObject.ObjectType = VIA_PUT_THROUGH_ROUND;
			ViaObject.x1 = StartSecondDrawingTraceX;
			ViaObject.y1 = StartSecondDrawingTraceY;
			ViaObject.x2 = CurrentVia.ThickNess;
			ViaObject.Clearance = CurrentVia.Clearance;
			ViaObject.Info = 0;

			if ((DrawTwoTryingTraces) && (DrawTrace2.Info2 != -1))
			{
				ViaObject.x1 = EndSecondDrawingTraceX;
				ViaObject.y1 = EndSecondDrawingTraceY;
			}

			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObject(&ViaObject, 0x40 + 8);
		}

		if (DrawTwoTryingTraces)
		{
			if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
				DrawTrace2.Info |= OBJECT_HIGHLITED;
			else
				DrawTrace2.Info &= ~OBJECT_HIGHLITED;

			DrawTrace2.Layer = CurrentDrawingLayer;
			DrawObject(&DrawTrace2, 0x10);

			if (DrawClearanceForTryingTrace)
			{
				InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
				DrawObject(&DrawTrace2, 0x40 + 8);
			}
		}

		break;

	case 1:					// All angle trace
		DrawObject(&DrawTrace1, 0x10);

		if (DrawClearanceForTryingTrace)
		{
			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObject(&DrawTrace1, 0x48);
		}

		break;

	case 2:					// Arc trace
	case 3:					// Arc trace
		DrawObject(&DrawTrace1, 0);

		if (DrawClearanceForTryingTrace)
		{
			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObject(&DrawTrace1, 0x148);
		}

		if (DrawTwoTryingTraces)
		{
			if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
				DrawTrace2.Info |= OBJECT_HIGHLITED;
			else
				DrawTrace2.Info &= ~OBJECT_HIGHLITED;

			DrawObject(&DrawTrace2, 0x10);

			if (DrawClearanceForTryingTrace)
			{
				InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
				DrawObject(&DrawTrace2, 0x48);
			}
		}

//      InitDrawingObject(0,FIXED_COLOR_LAYER,1,GRAPHICS_GRAY+DRAW_WITH_PEN_AND_NOT_FILLED);
		if (DrawTrace1.x2 != 0.0)
		{
			InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
			cx = MultX(DrawTrace1.x1);
			cy = MultY(DrawTrace1.y1);
			DrawLine(cx - 20, cy, cx + 20, cy);
			DrawLine(cx, cy - 20, cx, cy + 20);
		}

		break;
	}

	if ((Net->Info & OBJECT_HIGHLITED) == 0)
		InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
	else
		InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_HILITED_PEN_AND_NOT_FILLED);

	if (DrawTraceUsingGuide)
	{
		if (DrawTwoTryingTraces)
			DrawLine(MultX(DrawingConnectionX2), MultY(DrawingConnectionY2), MultX(x2), MultY(y2));
		else
		{
			DrawLine(MultX(DrawingConnectionX2), MultY(DrawingConnectionY2), MultX(StartSecondDrawingTraceX),
			         MultY(StartSecondDrawingTraceY));
		}
	}
	else
	{
		switch (CurrentWorkingTrace.ObjectType)
		{
		case TRACE_HOR:
			ExcludeX1 = CurrentWorkingTrace.x1;
			ExcludeY1 = CurrentWorkingTrace.y1;
			ExcludeX2 = ExcludeX1 + CurrentWorkingTrace.x2;
			ExcludeY2 = ExcludeY1;
			break;

		case TRACE_VER:
			ExcludeX1 = CurrentWorkingTrace.x1;
			ExcludeY1 = CurrentWorkingTrace.y1;
			ExcludeX2 = ExcludeX1;
			ExcludeY2 = ExcludeY1 + CurrentWorkingTrace.x2;
			break;

		case TRACE_DIAG1:
			ExcludeX1 = CurrentWorkingTrace.x1;
			ExcludeY1 = CurrentWorkingTrace.y1;
			ExcludeX2 = ExcludeX1 + CurrentWorkingTrace.x2;
			ExcludeY2 = ExcludeY1 - CurrentWorkingTrace.x2;
			break;

		case TRACE_DIAG2:
			ExcludeX1 = CurrentWorkingTrace.x1;
			ExcludeY1 = CurrentWorkingTrace.y1;
			ExcludeX2 = ExcludeX1 + CurrentWorkingTrace.x2;
			ExcludeY2 = ExcludeY1 + CurrentWorkingTrace.x2;
			break;

		case TRACE_ALL_ANGLE:
			ExcludeX1 = CurrentWorkingTrace.x1;
			ExcludeY1 = CurrentWorkingTrace.y1;
			ExcludeX2 = CurrentWorkingTrace.x2;
			ExcludeY2 = CurrentWorkingTrace.y2;
			break;

		case TRACE_ARC:
			GetArcEndPoints(&CurrentWorkingTrace, &ExcludeX1, &ExcludeY1, &ExcludeX2, &ExcludeY2, 0);
			break;
		}

		res = FindShortestPadNet(x1, y1, x2, y2, ExcludeX1, ExcludeY1, ExcludeX2, ExcludeY2, &x3, &y3, 0);

		if (res != -1)
			DrawLine(MultX(x2), MultY(y2), MultX(x3), MultY(y3));
	}

	if ((mode & 16) == 0)
		DrawCrossHair(16 + 8);
	else
		DrawCrossHair(16 + 8 + 2);

#if 0
	InitDrawingObject(0, CROSS_HAIR_LAYER, 1, DRAW_WITH_DASH_PEN_AND_NO_BRUSH);

//  InitDrawingObject(0,FIXED_COLOR_LAYER,1,GRAPHICS_GRAY+DRAW_WITH_PEN_AND_NOT_FILLED);
	if (CrossHairType == 0)
	{
		DrawLine(MultX(x2), 100000, MultX(x2), -100000);
		DrawLine(100000, MultY(y2), -100000, MultY(y2));
	}
	else
	{
		DrawLine(MultX(x2) + 100000, MultY(y2) + 100000, MultX(x2) - 100000, MultY(y2) - 100000);
		DrawLine(MultX(x2) - 100000, MultY(y2) + 100000, MultX(x2) + 100000, MultY(y2) - 100000);
	}

#endif
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindShortestPadNet(double StartX, double StartY, double x1, double y1, double ExcludeX1, double ExcludeY1,
                         double ExcludeX2, double ExcludeY2, double *x2, double *y2, int32 mode)
{

	int32 cnt, Found;
	double Length, le, x1a, y1a, x2a, y2a;
	ObjectRecord *Object;
#ifdef _DEBUG
	int32 ok;
#endif

	x2a = 0.0;
	y2a = 0.0;
	Found = -1;
	Length = 1000000000.0;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
#ifdef _DEBUG

		if (cnt == 18)
			ok = 1;

#endif

		switch (Object->ObjectType)
		{
		case PIN_PUT_THROUGH_ROUND:
		case VIA_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			if ((NotInRange(x1, Object->x1)) || (NotInRange(y1, Object->y1)))
			{
				if ((le = sqrt(SQR(x1 - Object->x1) + SQR(y1 - Object->y1))) < Length)
				{
					Length = le;
					Found = cnt;
					*x2 = Object->x1;
					*y2 = Object->y1;
				}
			}

			break;

		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
		case PIN_LINE_HOR:
		case PIN_LINE_VER:
		case PIN_LINE_DIAG1:
		case PIN_LINE_DIAG2:
			x1a = Object->x1;
			y1a = Object->y1;

			switch (Object->ObjectType)
			{
			case TRACE_HOR:
			case PIN_LINE_HOR:
				x2a = Object->x1 + Object->x2;
				y2a = Object->y1;
				break;

			case PIN_LINE_VER:
			case TRACE_VER:
				x2a = Object->x1;
				y2a = Object->y1 + Object->x2;
				break;

			case TRACE_DIAG1:
			case PIN_LINE_DIAG1:
				x2a = Object->x1 + Object->x2;
				y2a = Object->y1 - Object->x2;
				break;

			case PIN_LINE_DIAG2:
			case TRACE_DIAG2:
				x2a = Object->x1 + Object->x2;
				y2a = Object->y1 + Object->x2;
				break;
			}

			if (((NotInRange(x1a, ExcludeX1)) || (NotInRange(y1a, ExcludeY1)))
			        && ((NotInRange(x2a, ExcludeX2)) || (NotInRange(y2a, ExcludeY2))))
			{
				if ((NotInRange(x1, Object->x1)) || (NotInRange(y1, Object->y1)))
				{
					if ((NotInRange(StartX, Object->x1)) || (NotInRange(StartY, Object->y1)))
					{
						if ((le = sqrt(SQR(x1 - Object->x1) + SQR(y1 - Object->y1))) < Length)
						{
							Length = le;
							Found = cnt;
							*x2 = Object->x1;
							*y2 = Object->y1;
						}
					}
				}

				if ((NotInRange(x1, x2a)) || (NotInRange(y1, y2a)))
				{
					if ((NotInRange(StartX, x2a)) || (NotInRange(StartY, y2a)))
					{
						if ((le = sqrt(SQR(x1 - x2a) + SQR(y1 - y2a))) < Length)
						{
							Length = le;
							Found = cnt;
							*x2 = x2a;
							*y2 = y2a;
						}
					}
				}
			}

			break;

		case PIN_LINE_ALL_ANGLE:
		case TRACE_ALL_ANGLE:
			x1a = Object->x1;
			y1a = Object->y1;
			x2a = Object->x2;
			y2a = Object->y2;

			if (((NotInRange(x1a, ExcludeX1)) || (NotInRange(y1a, ExcludeY1)))
			        && ((NotInRange(x2a, ExcludeX2)) || (NotInRange(y2a, ExcludeY2))) && ((NotInRange(x1, Object->x1))
			                || (NotInRange(y1, Object->y1))))
			{
				if ((NotInRange(StartX, Object->x1)) || (NotInRange(StartY, Object->y1)))
				{
					if ((le = sqrt(SQR(x1 - Object->x1) + SQR(y1 - Object->y1))) < Length)
					{
						Length = le;
						Found = cnt;
						*x2 = Object->x1;
						*y2 = Object->y1;
					}
				}
			}

			x2a = Object->x2;
			y2a = Object->y2;

			if ((NotInRange(x1, x2a)) || (NotInRange(y1, y2a)))
			{
				if ((NotInRange(StartX, x2a)) || (NotInRange(StartY, y2a)))
				{
					if ((le = sqrt(SQR(x1 - x2a) + SQR(y1 - y2a))) < Length)
					{
						Length = le;
						Found = cnt;
						*x2 = x2a;
						*y2 = y2a;
					}
				}
			}

			break;

		case PIN_ARC:
		case TRACE_ARC:
			GetArcEndPoints(Object, &x1a, &y1a, &x2a, &y2a, 0);

			if (((NotInRange(x1a, ExcludeX1)) || (NotInRange(y1a, ExcludeY1)))
			        && ((NotInRange(x2a, ExcludeX2)) || (NotInRange(y2a, ExcludeY2))))
			{
				if ((NotInRange(x1, x1a)) || (NotInRange(y1, y1a)))
				{
					if ((NotInRange(StartX, x1a)) || (NotInRange(StartY, y1a)))
					{
						if ((le = sqrt(SQR(x1 - x1a) + SQR(y1 - y1a))) < Length)
						{
							Length = le;
							Found = cnt;
							*x2 = x1a;
							*y2 = y1a;
						}
					}
				}

				if ((NotInRange(x1, x2a)) || (NotInRange(y1, y2a)))
				{
					if ((NotInRange(StartX, x2a)) || (NotInRange(StartY, y2a)))
					{
						if ((le = sqrt(SQR(x1 - x2a) + SQR(y1 - y2a))) < Length)
						{
							Length = le;
							Found = cnt;
							*x2 = x2a;
							*y2 = y2a;
						}
					}
				}
			}

			break;
		}
	}

#ifdef _DEBUG

	if (Found >= 0)
	{
		Object = &((*Objects3)[Found]);
		ok = 1;
	}

#endif
	return Found;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawNewVia(double x, double y)
{
	ViaRecord NewVia;

	memcpy(&NewVia, &CurrentVia, sizeof(ViaRecord));
	NewVia.X = (float) x;
	NewVia.Y = (float) y;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	DrawVia(&NewVia);
	DrawCrossHair(16 + 8);
	ExitDrawing();
	EndDrawingEditingWindow();
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddNewVia(double x, double y)
{
	ObjectRecord ViaObject;
	ViaRecord *NetVia, NewVia;
	NetRecord *Net;

	if (CurrentDrawingNetNr < 0)
		return -1;

	Net = &((*Nets)[CurrentDrawingNetNr]);

	memset(&ViaObject, 0, sizeof(ViaObject));
	ViaObject.ObjectType = VIA_PUT_THROUGH_ROUND;
	ViaObject.x1 = x;
	ViaObject.y1 = y;
	NetVia = &CurrentVia;
	ViaObject.x2 = CurrentVia.ThickNess;
	ViaObject.y2 = CurrentVia.DrillThickNess;
	ViaObject.Clearance = CurrentVia.Clearance;

	if ((Net->ViaNr > 0) && (Net->ViaNr <= 10))
	{
		NetVia = &Design.DefVia1;
		NetVia += Net->ViaNr - 1;
		ViaObject.x2 = NetVia->ThickNess;
		ViaObject.y2 = NetVia->DrillThickNess;
		ViaObject.Clearance = NetVia->Clearance;
	}

	ViaObject.NetNr = CurrentDrawingNetNr;
	ViaObject.Info = 0;
	ViaObject.Layer = -1;
	FillPositionObject(&ViaObject);
	memmove(&NewVia, NetVia, sizeof(ViaRecord));
	NewVia.X = (float) x;
	NewVia.Y = (float) y;
	NewVia.NetNr = (int16) CurrentDrawingNetNr;
	NewVia.Info = 0;

	if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
		NewVia.Info = OBJECT_HIGHLITED;

	if (AddVia(&NewVia))
	{
		LastActionNr++;

		if (RecalcAreafillAfterInsert)
		{
			if (!OkToRePaintAfterTraceDrawing)
				InsertObjectInAreaFill(&ViaObject, -1, ViaObject.NetNr, 0);
			else
				InsertObjectInAreaFill(&ViaObject, -1, ViaObject.NetNr, 2);
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AddAreaFillVia(int32 mode)
{
	int32 OldCurrentDrawingLayer, cnt, res, OldLastActionNr, ViasInserted, TempLastActionNr;
	RECT Rect;
	AreaFillRecord *AreaFill = NULL;
	PolygonRecord *FirstPolygon;
	DrawXorFunctionRecord DrawXorFunction;

	ScrollCount = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0))
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
				break;
		}
	}

	if (!AreaFill)
		return;

	CurrentDrawingNetNr = AreaFill->NetNr;

	if (CurrentDrawingNetNr < 0)
		return;

	OldLastActionNr = LastActionNr;
	SelectionEsc = 0;
	memset(&LastAddedObject, 0, sizeof(ObjectRecord));
	Rect.left = (int32) DrawWindowMinX + ClientStartX;
	Rect.top = (int32) DrawWindowMinY + ClientStartY;
	Rect.right = (int32) DrawWindowMaxX + ClientStartX;
	Rect.bottom = (int32) DrawWindowMaxY + ClientStartY;

	CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
	TempLastActionNr = LastActionNr;
	OldX = CurrentDrawX2;
	OldY = CurrentDrawY2;
	ClipMouseCursor();			//  CrossHairX CrossHairY mode
//  DrawCrossHair(0);
	CrossHairMode = 1;
	ViasInserted = 0;
	DrawNewVia(CurrentDrawX2, CurrentDrawY2);
	SystemBusyMode = 7;
	DrawXorFunction.Function9 = (FUNCP9) DrawNewVia;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Mode = 8;
	DrawXorFunction.Param2[0] = &CurrentDrawX2;
	DrawXorFunction.Param2[1] = &CurrentDrawY2;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentDrawX2) || (OldY != CurrentDrawY2))
			{
				DrawNewVia(OldX, OldY);
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawNewVia(OldX, OldY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
				ScrollCount++;
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
#ifdef _DEBUG

				if (ScrollCount == 0)
					ok = 1;

#endif
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawNewVia(OldX, OldY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
				ScrollCount++;
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawNewVia(OldX, OldY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
				ScrollCount++;
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				DrawNewVia(OldX, OldY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentDrawX2;
				OldY = CurrentDrawY2;
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
				ScrollCount++;
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;
			DrawNewVia(CurrentDrawX2, CurrentDrawY2);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawNewVia(OldX, OldY);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
		}

// ***********************************************************************************
		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawNewVia(OldX, OldY);
			ZoomWindow();
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;

			if (!SelectionEsc)
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawNewVia(OldX, OldY);
			PanWindow();
			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;

			if (!SelectionEsc)
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);
		}

// ***********************************************************************************
		if (CheckLeftButton())
		{
			DrawNewVia(OldX, OldY);
			AddNewVia(CurrentDrawX2, CurrentDrawY2);
			ViasInserted++;
			CheckInputMessages(0);
			DrawNewVia(CurrentDrawX2, CurrentDrawY2);
			ok = 1;
		}

// ***********************************************************************************
		if (LeftButtonDoublePressed)
			LeftButtonDoublePressed = 0;

// ***********************************************************************************
		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			RightButtonPressed = 0;
		}

// ***********************************************************************************
		if (NrFunctionsInBuf > 0)
		{
			OldCurrentDrawingLayer = CurrentDrawingLayer;
			UnClipMouseCursor();
			DrawNewVia(OldX, OldY);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SelectionEsc)
				LastActionNr++;

			if (HelpAsked)
				HelpAsked = 0;

			CurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentDrawX2;
			OldY = CurrentDrawY2;

			if (!SelectionEsc)
				DrawNewVia(CurrentDrawX2, CurrentDrawY2);

			ClipMouseCursor();
		}
	}

// ***********************************************************************************
	res = 1;
//  TestCounter
	CrossHairType = 0;
	CrossHairMode = 0;
#ifndef TRACE_REPAINT
	DrawCrossHair(16 + 2);
	UnClipMouseCursor();
#else
	UnClipMouseCursor();
	RePaint();
#endif

	if (ViasInserted > 0)
		LastActionNr--;

	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
