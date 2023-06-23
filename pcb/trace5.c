/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: trace5.c
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
#include "calcdef.h"
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
#include "help.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "select3.h"
#include "select4.h"
#include "mainloop.h"
#include "nets.h"
#include "files.h"
#include "trace2.h"
#include "trace5.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "draw2.h"
#include "draw3.h"
#include "calcdef.h"
#include "draw.h"
#include "calc3.h"
#include "calc2.h"
#include "graphics.h"
#include "insdel.h"
#include "resource.h"
#include "dialogs.h"
#include "polygon.h"
#include "settings.h"



static ObjectRecord MovingTrace, MovingTrace1, MovingTrace2, MovingDrawTrace1, MovingDrawTrace2, MovingDrawTrace3;

static double MinX, MinY, MaxX, MaxY, Trace1X1, Trace1Y1, Trace1X2, Trace1Y2, Trace2X1, Trace2Y1, Trace2X2, Trace2Y2,
       MovingTraceX1, MovingTraceY1, MovingTraceX2, MovingTraceY2, MovingTrace1X1, MovingTrace1Y1, MovingTrace1X2,
       MovingTrace1Y2, MovingTrace2X1, MovingTrace2Y1, MovingTrace2X2, MovingTrace2Y2, MovingCurrentDrawX1,
       MovingCurrentDrawY1, MovingCurrentDrawX2, MovingCurrentDrawY2;
static int32 TraceDir, MovingTrace1Dir, MovingTrace2Dir, FoundObject3Nr, FoundObject3Nr1, FoundObject3Nr2, Trace1Dir,
       Trace2Dir, MovingTrace1EndPoint, MovingTrace2EndPoint;

static int32 ChangeCenterTrace, AdjustTrace1X1Y1, AdjustTrace1X2Y2, AdjustTrace2X1Y1, AdjustTrace2X2Y2;
extern HDC OutputDisplay;

int32 ok;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawSpecialTrace(int32 mode);

void CalcSpecialTrace(double divxy);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectsConnected4(ObjectRecord * Object1, ObjectRecord * Object2)
{
	int32 ObjectType1, ObjectType2;
	double x11, y11, x12, y12, x21, y21, lengte1;

	if (Object1->Layer != Object2->Layer)
		return 0;

	ObjectType1 = Object1->ObjectType;
	ObjectType2 = Object2->ObjectType;

	x11 = Object1->x1;
	y11 = Object1->y1;
	x21 = Object2->x1;
	y21 = Object2->y1;
	lengte1 = Object1->x2;

	switch (ObjectType1)
	{
	case TRACE_HOR:
	case PIN_LINE_HOR:
		x12 = x11 + lengte1;
		y12 = y11;
		break;

	case TRACE_VER:
	case PIN_LINE_VER:
		x12 = x11;
		y12 = y11 + lengte1;
		break;

	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:
		x12 = x11 + lengte1;
		y12 = y11 - lengte1;
		break;

	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:
		x12 = x11 + lengte1;
		y12 = y11 + lengte1;
		break;

	default:
		return 0;
	}

	if ((InRange(x11, x21)) && (InRange(y11, y21)))
		return 1;

	if ((InRange(x12, x21)) && (InRange(y12, y21)))
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SpecialTraceDrawing(int32 mode)
{
	NetRecord *Net;
	int32 TempBackGroundActive;

	if ((mode & 1) == 1)
		DrawSpecialTrace(1);

	if ((mode & 2) == 2)
		DrawSpecialTrace(0);

	if ((mode & 4) == 4)
		DrawCrossObjects(0, 1);

	if ((mode & 0x18) != 0)
	{
		Net = &((*Nets)[MovingTrace2.NetNr]);
		StartDrawingEditingWindow(0);
		DrawCode = DrawLayerCode[CurrentDrawingLayer];

		if ((mode & 8) == 8)
			SetROP2(OutputDisplay, R2_COPYPEN);
		else
			SetBackGroundActive(0);

		TempBackGroundActive = BackGroundActive;

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObjectTrace(&MovingTrace1);

		if (OkToDrawClearances)
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObjectTraceWithClearance(&MovingTrace1);
		}

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObjectTrace(&MovingTrace);

		if (OkToDrawClearances)
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObjectTraceWithClearance(&MovingTrace);
		}

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObjectTrace(&MovingTrace2);

		if (OkToDrawClearances)
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			DrawObjectTraceWithClearance(&MovingTrace2);
		}

		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 TestSpecialTrace(int32 mode)
{
	FillPositionObject(&MovingDrawTrace1);
	MovingDrawTrace1.Test = 0;
	FillPositionObject(&MovingDrawTrace2);
	MovingDrawTrace2.Test = 0;
	FillPositionObject(&MovingDrawTrace3);
	MovingDrawTrace3.Test = 0;

	if (mode == 1)
	{
		if ((MovingDrawTrace1.x2 > 0.0)
		        && ((CheckObjectOverlappedFromObjects(&MovingDrawTrace1, 4) != -1)
		            || (CheckObjectInsideAreaFillPowerPlane(&MovingDrawTrace1) == 1)))
		{
//      Object2=&((*Objects2)[FoundNr]);
			return -1;
		}

		if ((MovingDrawTrace2.x2 > 0.0)
		        && ((CheckObjectOverlappedFromObjects(&MovingDrawTrace2, 4) != -1)
		            || (CheckObjectInsideAreaFillPowerPlane(&MovingDrawTrace2) == 1)))
		{
//      Object2=&((*Objects2)[FoundNr]);
			return -1;
		}

		if ((MovingDrawTrace3.x2 > 0.0)
		        && ((CheckObjectOverlappedFromObjects(&MovingDrawTrace3, 4) != -1)
		            || (CheckObjectInsideAreaFillPowerPlane(&MovingDrawTrace3) == 1)))
		{
//      Object2=&((*Objects2)[FoundNr]);
			return -1;
		}

		return 0;
	}
	else
	{
		if ((MovingDrawTrace1.x2 > 0.0)
		        && ((CheckObjectOverlappedFromObjects(&MovingDrawTrace1, 4) != -1)
		            || (CheckObjectInsideAreaFillPowerPlane(&MovingDrawTrace1) == 1)))
			return -1;

		if ((MovingDrawTrace2.x2 > 0.0)
		        && ((CheckObjectOverlappedFromObjects(&MovingDrawTrace2, 4) != -1)
		            || (CheckObjectInsideAreaFillPowerPlane(&MovingDrawTrace2) == 1)))
			return -1;

		if ((MovingDrawTrace3.x2 > 0.0)
		        && ((CheckObjectOverlappedFromObjects(&MovingDrawTrace3, 4) != -1)
		            || (CheckObjectInsideAreaFillPowerPlane(&MovingDrawTrace3) == 1)))
			return -1;

		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 OkToDrawSpecialTraces(double div, int32 mode)
{
	double div2;
	int32 Found = 0;

	if (mode == 0)
	{
		if ((MovingDrawTrace1.x2 + 10.0 > 0.0) && (MovingDrawTrace2.x2 + 10.0 > 0.0)
		        && (MovingDrawTrace3.x2 + 10.0 > 0.0))
		{
			if (TestSpecialTrace(1) == 0)
				return 1;
		}
	}
	else
	{
		CalcSpecialTrace(div);

		if ((MovingDrawTrace1.x2 + 10.0 > 0.0) && (MovingDrawTrace2.x2 + 10.0 > 0.0)
		        && (MovingDrawTrace3.x2 + 10.0 > 0.0))
		{
			if (TestSpecialTrace(0) == 0)
				return 1;
		}

		div2 = div;

		if (div2 < 0.0)
		{
			while ((div2 < 0.0) && (!Found))
			{
				div2 += GridSize;
				CalcSpecialTrace(div2);

				if ((MovingDrawTrace1.x2 + 10.0 > 0.0) && (MovingDrawTrace2.x2 + 10.0 > 0.0)
				        && (MovingDrawTrace3.x2 + 10.0 > 0.0))
				{
					if (TestSpecialTrace(0) == 0)
						Found = 1;
				}
			}
		}
		else
		{
			while ((div2 > 0.0) && (!Found))
			{
				div2 -= GridSize;
				CalcSpecialTrace(div2);

				if ((MovingDrawTrace1.x2 + 10.0 > 0.0) && (MovingDrawTrace2.x2 + 10.0 > 0.0)
				        && (MovingDrawTrace3.x2 + 10.0 > 0.0))
				{
					if (TestSpecialTrace(0) == 0)
						Found = 1;
				}
			}
		}

		if (Found)
			return 1;

	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawSpecialTrace(int32 mode)
{
	NetRecord *Net;


	if (OkToDrawSpecialTraces(0.0, 0))
	{
		StartDrawingEditingWindow(0);

		if (mode == 0)
			SetROP2(OutputDisplay, R2_XORPEN);

		DrawCode = DrawLayerCode[CurrentDrawingLayer];
		Net = &((*Nets)[MovingDrawTrace2.NetNr]);

		if ((Net->Info & OBJECT_HIGHLITED) != 0)
		{
			if ((DrawCode < 0) || (DrawCode > MAX_ACTIVE_DRAWING_LAYERS - 1))
				return;

			InitDrawingObject(TRACE_HOR, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
		}
		else
			InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

		if (mode == 0)
		{
			if (MovingDrawTrace1.x2 > 0)
				DrawObjectTraceXor(&MovingDrawTrace1);

			if (MovingDrawTrace2.x2 > 0)
				DrawObjectTraceXor(&MovingDrawTrace2);

			if (MovingDrawTrace3.x2 > 0)
				DrawObjectTraceXor(&MovingDrawTrace3);
		}
		else
		{
			if (MovingDrawTrace1.x2 > 0)
				DrawObjectTrace(&MovingDrawTrace1);

			if (MovingDrawTrace2.x2 > 0)
				DrawObjectTrace(&MovingDrawTrace2);

			if (MovingDrawTrace3.x2 > 0)
				DrawObjectTrace(&MovingDrawTrace3);
		}

		if ((OkToDrawClearances) || (DrawClearanceForTryingTrace))
		{
			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

			if (MovingDrawTrace1.x2 > 0)
				DrawObjectTraceWithClearance(&MovingDrawTrace1);

			if (MovingDrawTrace2.x2 > 0)
				DrawObjectTraceWithClearance(&MovingDrawTrace2);

			if (MovingDrawTrace3.x2 > 0)
				DrawObjectTraceWithClearance(&MovingDrawTrace3);
		}

		ExitDrawing();
		EndDrawingEditingWindow(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SpecialTraceDrawingFunc(double div_old, int32 Mode)
{
	if (Mode == 0)
	{
		CalcSpecialTrace(div_old);
		SpecialTraceDrawing(2);
		SpecialTraceDrawing(4);
		SpecialTraceDrawing(8);
	}
	else
	{
		SpecialTraceDrawing(0x10);
		SpecialTraceDrawing(4);
		SpecialTraceDrawing(2);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveOneTrace2(int32 mode)
{
	int32 res, res2, res3, res4, TraceChanged, Changed, ShiftPressedAtStart, NewMode1, NewMode2;
	int32 AreaFillChanged = 0;
	double OldX, OldY, div_old, olddivxy, div1 = 0.0;
	TraceRecord *Trace = NULL;
	NetRecord *Net;
	HMENU PopUpMenu;
	DrawXorFunctionRecord DrawXorFunction;

	RelX = MovingCurrentDrawX1;
	RelY = MovingCurrentDrawY1;

	TraceChanged = 0;
	SelectionEsc = 0;

	MovingCurrentDrawX1 = AdjustToDrawGrid(MovingCurrentDrawX1);
	MovingCurrentDrawY1 = AdjustToDrawGrid(MovingCurrentDrawY1);
	MovingCurrentDrawX2 = MovingCurrentDrawX1;
	MovingCurrentDrawY2 = MovingCurrentDrawY1;

	olddivxy = 10.0;
	Net = &((*Nets)[MovingTrace2.NetNr]);
	strcpy(InfoStr, Net->Name);
	RedrawInfoStr(1);

	OldX = MovingCurrentDrawX2;
	OldY = MovingCurrentDrawY2;

	div_old = 0.0;

	ShiftPressedAtStart = ShiftPressed;
	ClipMouseCursor();
	CalcSpecialTrace(div_old);
	SpecialTraceDrawing(0x10);
	SpecialTraceDrawing(4);
	SpecialTraceDrawing(2);
	SystemBusyMode = 20;
	NewMode1 = 0;
	NewMode2 = 1;
	DrawXorFunction.Function3 = (FUNCP3) SpecialTraceDrawingFunc;
	DrawXorFunction.Param1[0] = &div_old;
	DrawXorFunction.Param1[1] = &NewMode1;
	DrawXorFunction.Mode = 2;
	DrawXorFunction.Param2[0] = &div_old;
	DrawXorFunction.Param2[1] = &NewMode2;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			Changed = 0;

			switch (TraceDir)
			{
			case 0:
				if (OldY != MovingCurrentDrawY2)
					Changed = 1;

				div1 = MovingCurrentDrawY2 - MovingCurrentDrawY1;
				break;

			case 1:

//          if ((NotInRange(-OldX+OldY,-MovingCurrentDrawX2+MovingCurrentDrawY2))
//             &&
//             (InRange(OldX-MovingCurrentDrawX2,OldY-MovingCurrentDrawY2))) Changed=1;
				if (OldX != MovingCurrentDrawX2)
					Changed = 1;

				div1 = (MovingCurrentDrawX2 - MovingCurrentDrawX1);
				break;

			case 2:
				if (OldX != MovingCurrentDrawX2)
					Changed = 1;

				div1 = (MovingCurrentDrawX2 - MovingCurrentDrawX1);
				break;

			case 3:
				if (OldX != MovingCurrentDrawX2)
					Changed = 1;

				div1 = (MovingCurrentDrawX2 - MovingCurrentDrawX1);
				break;
			}

// polygon
			if (Changed)
			{
				CalcSpecialTrace(div1);

				if (OkToDrawSpecialTraces(0.0, 0))
				{
//            strcpy(InfoStr,"                                       ");
//            RedrawInfoStr(1);
					CalcSpecialTrace(div_old);
					SpecialTraceDrawing(2);
//          DrawSpecialTrace(0);
					OldX = MovingCurrentDrawX2;
					OldY = MovingCurrentDrawY2;
					CalcSpecialTrace(div1);
					div_old = div1;
					SpecialTraceDrawing(2);
//          DrawSpecialTrace(0);
				}
				else
				{
//            strcpy(InfoStr,"Error                                  ");
//            RedrawInfoStr(1);
				}
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				CalcSpecialTrace(div_old);
				SpecialTraceDrawing(2);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(8);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = MovingCurrentDrawX2;
				OldY = MovingCurrentDrawY2;
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				CalcSpecialTrace(div_old);
				SpecialTraceDrawing(2);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(8);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = MovingCurrentDrawX2;
				OldY = MovingCurrentDrawY2;
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				CalcSpecialTrace(div_old);
				SpecialTraceDrawing(2);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(8);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = MovingCurrentDrawX2;
				OldY = MovingCurrentDrawY2;
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				CalcSpecialTrace(div_old);
				SpecialTraceDrawing(2);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(8);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = MovingCurrentDrawX2;
				OldY = MovingCurrentDrawY2;
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = MovingCurrentDrawX2;
			OldY = MovingCurrentDrawY2;
			SpecialTraceDrawing(0x10);
			SpecialTraceDrawing(4);
			SpecialTraceDrawing(2);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			CalcSpecialTrace(div_old);
			SpecialTraceDrawing(2);
			SpecialTraceDrawing(4);
			SpecialTraceDrawing(8);

			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
			{
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}
			else
			{
			}
		}

// ***********************************************************************************
		if ((ZoomActive()) && (!SelectionEsc))
		{
			CalcSpecialTrace(div_old);
			SpecialTraceDrawing(2);
			SpecialTraceDrawing(4);
			SpecialTraceDrawing(8);
			ZoomWindow();
			MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = MovingCurrentDrawX2;
			OldY = MovingCurrentDrawY2;

			if (!SelectionEsc)
			{
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}
		}

// ***********************************************************************************
		if ((PanActive()) && (!SelectionEsc))
		{
			CalcSpecialTrace(div_old);
			SpecialTraceDrawing(2);
			SpecialTraceDrawing(4);
			SpecialTraceDrawing(8);
			PanWindow();
			MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = MovingCurrentDrawX2;
			OldY = MovingCurrentDrawY2;

			if (!SelectionEsc)
			{
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}
		}

// ***********************************************************************************
		if ((((mode == 0) && (CheckLeftButton())) || ((mode == 1) && (!ShiftPressed))) && (NotInRange(div1, 0.0)))
		{
			CalcSpecialTrace(div_old);
			SpecialTraceDrawing(2);
			SpecialTraceDrawing(4);
			SpecialTraceDrawing(0x10);

			if (OkToDrawSpecialTraces(div1, 1))
			{
//        SpecialTraceDrawing(1);
				TraceChanged = 1;

				if ((Net->Info & OBJECT_HIGHLITED) != 0)
				{
					MovingDrawTrace1.Info |= OBJECT_HIGHLITED;
					MovingDrawTrace2.Info |= OBJECT_HIGHLITED;
					MovingDrawTrace3.Info |= OBJECT_HIGHLITED;
				}

				ZeroUnusedObjects(0);

				if (MovingDrawTrace1.x2 > 0)
				{
//          if ((res=ExtendNewTrace(&MovingDrawTrace1))!=-1) {
//            DeleteAndUnDisplayObjectTrace(res);
//          }
					AddTrace(&MovingDrawTrace1);

					if (RecalcAreafillAfterInsert)
					{
						res2 =
						    InsertObjectInAreaFill(&MovingDrawTrace1, MovingDrawTrace1.Layer, MovingDrawTrace1.NetNr,
						                           2);

						if (res2 == 1)
							AreaFillChanged = 1;
					}
				}

				if (MovingDrawTrace2.x2 > 0)
				{
					if ((res = ExtendNewTrace(&MovingDrawTrace2)) != -1)
					{
						if (!OkToRePaintAfterTraceDrawing)
							DeleteAndUnDisplayObjectTrace(res, 0);
						else
							DeleteAndUnDisplayObjectTrace(res, 1);
					}

					AddTrace(&MovingDrawTrace2);

					if (RecalcAreafillAfterInsert)
					{
//            AreaFill=(AreaFillRecord *)&(AreaFillMem[(*AreaFills)[Design.NrAreaFills-1]]);
						res3 =
						    InsertObjectInAreaFill(&MovingDrawTrace2, MovingDrawTrace2.Layer, MovingDrawTrace2.NetNr,
						                           2);

						if (res3 == 1)
							AreaFillChanged = 1;
					}
				}

				if (MovingDrawTrace3.x2 > 0)
				{
//          if ((res=ExtendNewTrace(&MovingDrawTrace3))!=-1) {
//            DeleteAndUnDisplayObjectTrace(res);
//          }
					AddTrace(&MovingDrawTrace3);

					if (RecalcAreafillAfterInsert)
					{
						res4 =
						    InsertObjectInAreaFill(&MovingDrawTrace3, MovingDrawTrace3.Layer, MovingDrawTrace3.NetNr,
						                           2);

						if (res4 == 1)
							AreaFillChanged = 1;
					}
				}

				if (!OkToRePaintAfterTraceDrawing)
					DrawSpecialTrace(1);

				switch (MovingTrace.ObjectType)
				{
				case TRACE_VER:
					Trace = &((*VerTraces[MovingTrace.Layer])[MovingTrace.TraceNr]);
					break;

				case TRACE_HOR:
					Trace = &((*HorTraces[MovingTrace.Layer])[MovingTrace.TraceNr]);
					break;

				case TRACE_DIAG1:
					Trace = &((*Diag1Traces[MovingTrace.Layer])[MovingTrace.TraceNr]);
					break;

				case TRACE_DIAG2:
					Trace = &((*Diag2Traces[MovingTrace.Layer])[MovingTrace.TraceNr]);
					break;
				}

				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;

				if (MovingTrace1.TraceNr != -1)
				{
					switch (MovingTrace1.ObjectType)
					{
					case TRACE_VER:
						Trace = &((*VerTraces[MovingTrace1.Layer])[MovingTrace1.TraceNr]);
						break;

					case TRACE_HOR:
						Trace = &((*HorTraces[MovingTrace1.Layer])[MovingTrace1.TraceNr]);
						break;

					case TRACE_DIAG1:
						Trace = &((*Diag1Traces[MovingTrace1.Layer])[MovingTrace1.TraceNr]);
						break;

					case TRACE_DIAG2:
						Trace = &((*Diag2Traces[MovingTrace1.Layer])[MovingTrace1.TraceNr]);
						break;
					}

					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16) LastActionNr;
				}

				if (MovingTrace2.TraceNr != -1)
				{
					switch (MovingTrace2.ObjectType)
					{
					case TRACE_VER:
						Trace = &((*VerTraces[MovingTrace2.Layer])[MovingTrace2.TraceNr]);
						break;

					case TRACE_HOR:
						Trace = &((*HorTraces[MovingTrace2.Layer])[MovingTrace2.TraceNr]);
						break;

					case TRACE_DIAG1:
						Trace = &((*Diag1Traces[MovingTrace2.Layer])[MovingTrace2.TraceNr]);
						break;

					case TRACE_DIAG2:
						Trace = &((*Diag2Traces[MovingTrace2.Layer])[MovingTrace2.TraceNr]);
						break;
					}

					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16) LastActionNr;
				}

				if ((AreaFillChanged) || (OkToRePaintAfterTraceDrawing))
				{
					ReCalcConnectionsNet(MovingTrace.NetNr, 0, 1);
					RePaint();
				}
				else
					ReCalcConnectionsNet(MovingTrace.NetNr, 0, 0);

				SelectionEsc = 1;
			}

			CheckInputMessages(0);
		}

// ***********************************************************************************
		if (LeftButtonDoublePressed)
		{
//      SpecialTraceDrawing(0);
//      SpecialTraceDrawing(1);
			LeftButtonDoublePressed = 0;
			CheckInputMessages(0);
		}

// ***********************************************************************************
		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			CalcSpecialTrace(div_old);
			SpecialTraceDrawing(2);
			SpecialTraceDrawing(4);
			SpecialTraceDrawing(8);
			PopUpMenu = CreatePopupMenu();
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(493, "Escape"));
			TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
			               RealWindow.top + MousePosY + 40, 0, PCBWindow, NULL);
			RightButtonPressed = 0;
			DestroyMenu(PopUpMenu);
			CheckInputMessages(0);

			if (!SelectionEsc)
			{
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}
			else
				ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
		}

// ***********************************************************************************
		if (NrFunctionsInBuf > 0)
		{
//      DrawSpecialTrace(0);
			CalcSpecialTrace(div_old);
			SpecialTraceDrawing(2);
			SpecialTraceDrawing(4);
			SpecialTraceDrawing(8);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("drag_one_trace.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}


			MovingCurrentDrawX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			MovingCurrentDrawY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = MovingCurrentDrawX2;
			OldY = MovingCurrentDrawY2;

			if (!SelectionEsc)
			{
				SpecialTraceDrawing(0x10);
				SpecialTraceDrawing(4);
				SpecialTraceDrawing(2);
			}
			else
				ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
		}
	}

// ***********************************************************************************
	UnClipMouseCursor();
	SelectionEsc = 0;
	DrawCrossHair(2);
	UnselectAll = 1;
	SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
	UnselectAll = 0;

	if (DataBaseChanged)
	{
		if (ShiftPressedAtStart)
		{
			DataBaseChanged = 0;
			LastActionNr++;
			FileChanged = 1;
			SetWindowName(1);
		}
	}

	if (mode == 0)
		SystemBusyMode = 0;
	else
		SystemBusyMode = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CalcSpecialTrace(double divxy)
{
	memmove(&MovingDrawTrace1, &MovingTrace1, sizeof(ObjectRecord));
	memmove(&MovingDrawTrace2, &MovingTrace, sizeof(ObjectRecord));
	memmove(&MovingDrawTrace3, &MovingTrace2, sizeof(ObjectRecord));

// ***********************************************************************************
// ***********************************************************************************
// ***********************************************************************************
	switch (TraceDir)
	{
	case 0:

// ***********************************************************************************
		switch (MovingTrace1Dir)
		{
		case 0:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.y1 += divxy;
				MovingDrawTrace1.x2 -= divxy;
			}
			else
				MovingDrawTrace1.x2 += divxy;

			MovingDrawTrace2.y1 += divxy;
			break;

		case 1:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.y1 += divxy;
				MovingDrawTrace1.x2 -= divxy;
			}
			else
				MovingDrawTrace1.x2 += divxy;

			MovingDrawTrace2.x1 += divxy;
			MovingDrawTrace2.y1 += divxy;
			MovingDrawTrace2.x2 -= divxy;
			break;

		case 3:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 -= divxy;
				MovingDrawTrace1.y1 += divxy;
				MovingDrawTrace1.x2 += divxy;
			}
			else
				MovingDrawTrace1.x2 -= divxy;

			MovingDrawTrace2.x1 -= divxy;
			MovingDrawTrace2.y1 += divxy;
			MovingDrawTrace2.x2 += divxy;
			break;
		}

// ***********************************************************************************

		switch (MovingTrace2Dir)
		{
		case 0:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.y1 += divxy;
				MovingDrawTrace3.x2 -= divxy;
			}
			else
				MovingDrawTrace3.x2 += divxy;

//          MovingDrawTrace2.y1+=divxy;
			break;

		case 1:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.y1 += divxy;
				MovingDrawTrace3.x2 -= divxy;
			}
			else
				MovingDrawTrace3.x2 += divxy;

//          MovingDrawTrace2.y1+=divxy;
			MovingDrawTrace2.x2 += divxy;
			break;

		case 3:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 -= divxy;
				MovingDrawTrace3.y1 += divxy;
				MovingDrawTrace3.x2 += divxy;
			}
			else
				MovingDrawTrace3.x2 -= divxy;

//          MovingDrawTrace2.x1-=divxy;
//          MovingDrawTrace2.y1+=divxy;
			MovingDrawTrace2.x2 -= divxy;
			break;
		}

		break;

// ***********************************************************************************
// ***********************************************************************************
// ***********************************************************************************
	case 1:
		switch (MovingTrace1Dir)
		{
		case 0:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.y1 += divxy;
				MovingDrawTrace1.x2 -= divxy;

				if (MovingTrace2Dir == 1)
				{
					MovingDrawTrace1.y1 += divxy;
					MovingDrawTrace1.x2 -= divxy;
				}
			}
			else
			{
				MovingDrawTrace1.x2 += divxy;

				if (MovingTrace2Dir == 1)
					MovingDrawTrace1.x2 += divxy;
			}

			MovingDrawTrace2.y1 += divxy;
			break;

		case 1:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.y1 += divxy;
				MovingDrawTrace1.x2 -= divxy;
			}
			else
				MovingDrawTrace1.x2 += divxy;

			MovingDrawTrace2.x1 += divxy;
			MovingDrawTrace2.y1 += divxy;
			MovingDrawTrace2.x2 -= divxy;
			break;

		case 2:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.x2 -= divxy;

				if (MovingTrace2Dir == 1)
				{
					MovingDrawTrace1.x1 += divxy;
					MovingDrawTrace1.x2 -= divxy;
				}
			}
			else
			{
				MovingDrawTrace1.x2 += divxy;

				if (MovingTrace2Dir == 1)
					MovingDrawTrace1.x2 += divxy;
			}

			MovingDrawTrace2.x1 += divxy;
			MovingDrawTrace2.x2 -= divxy;

			if (MovingTrace2Dir == 1)
			{
				MovingDrawTrace2.x1 += divxy;
				MovingDrawTrace2.x2 -= divxy;
			}

			break;
		}

// ***********************************************************************************
		switch (MovingTrace2Dir)
		{
		case 0:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.y1 += divxy;
				MovingDrawTrace3.x2 -= divxy;

				if (MovingTrace1Dir == 1)
				{
					MovingDrawTrace3.y1 += divxy;
					MovingDrawTrace3.x2 -= divxy;
				}
			}
			else
			{
				MovingDrawTrace3.x2 += divxy;

				if (MovingTrace1Dir == 1)
					MovingDrawTrace3.x2 += divxy;
			}

//          MovingDrawTrace2.y1+=divxy;
			break;

		case 1:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.y1 += divxy;
				MovingDrawTrace3.x2 -= divxy;
			}
			else
				MovingDrawTrace3.x2 += divxy;

			MovingDrawTrace2.x2 += divxy;
			break;

		case 2:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.x2 -= divxy;

				if (MovingTrace1Dir == 1)
				{
					MovingDrawTrace3.x1 += divxy;
					MovingDrawTrace3.x2 -= divxy;
				}
			}
			else
			{
				MovingDrawTrace3.x2 += divxy;

				if (MovingTrace1Dir == 1)
					MovingDrawTrace3.x2 += divxy;
			}

			MovingDrawTrace2.x2 += divxy;

			if (MovingTrace1Dir == 1)
				MovingDrawTrace2.x2 += divxy;

			break;
		}

		break;

// ***********************************************************************************
// ***********************************************************************************
// ***********************************************************************************
	case 2:
		switch (MovingTrace1Dir)
		{
		case 1:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.y1 += divxy;
				MovingDrawTrace1.x2 -= divxy;
			}
			else
				MovingDrawTrace1.x2 += divxy;

			MovingDrawTrace2.y1 += divxy;
			MovingDrawTrace2.x2 -= divxy;
			break;

		case 2:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.x2 -= divxy;
			}
			else
				MovingDrawTrace1.x2 += divxy;

			break;

		case 3:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.y1 -= divxy;
				MovingDrawTrace1.x2 -= divxy;
			}
			else
				MovingDrawTrace1.x2 += divxy;

			MovingDrawTrace2.y1 -= divxy;
			MovingDrawTrace2.x2 += divxy;
			break;
		}

// ***********************************************************************************
		switch (MovingTrace2Dir)
		{
		case 0:
			/*
			//          if (MovingTrace2EndPoint==0) {
			            MovingDrawTrace3.x1+=divxy;
			            MovingDrawTrace3.y1+=divxy;
			            MovingDrawTrace3.x2-=divxy;
			//          } else {
			//            MovingDrawTrace3.x2+=divxy;
			//          }
			*/
			MovingDrawTrace2.x1 += divxy;
//          MovingDrawTrace2.x2+=divxy;
			break;

		case 1:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.y1 += divxy;
				MovingDrawTrace3.x2 -= divxy;
			}
			else
				MovingDrawTrace3.x2 += divxy;

			MovingDrawTrace2.x1 += divxy;
			MovingDrawTrace2.x2 += divxy;
			break;

		case 2:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.x2 -= divxy;
			}
			else
				MovingDrawTrace3.x2 += divxy;

			MovingDrawTrace2.x1 += divxy;
			break;

		case 3:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.y1 -= divxy;
				MovingDrawTrace3.x2 -= divxy;
			}
			else
				MovingDrawTrace3.x2 += divxy;

			MovingDrawTrace2.x1 += divxy;
			MovingDrawTrace2.x2 -= divxy;
			break;
		}

		break;

// ***********************************************************************************
// ***********************************************************************************
// ***********************************************************************************
	case 3:
		switch (MovingTrace1Dir)
		{
		case 0:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.y1 -= divxy;
				MovingDrawTrace1.x2 += divxy;

				if (MovingTrace2Dir == 3)
				{
					MovingDrawTrace1.y1 -= divxy;
					MovingDrawTrace1.x2 += divxy;
				}
			}
			else
			{
				MovingDrawTrace1.x2 -= divxy;

				if (MovingTrace2Dir == 3)
					MovingDrawTrace1.x2 -= divxy;
			}

			MovingDrawTrace2.y1 -= divxy;

			if (MovingTrace2Dir == 3)
				MovingDrawTrace2.y1 -= divxy;

			break;

		case 2:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.x2 -= divxy;

				if (MovingTrace2Dir == 3)
				{
					MovingDrawTrace1.x1 -= divxy;
					MovingDrawTrace1.x2 -= divxy;
				}
			}
			else
			{
				MovingDrawTrace1.x2 += divxy;

				if (MovingTrace2Dir == 3)
					MovingDrawTrace1.x2 += divxy;
			}

			MovingDrawTrace2.x1 += divxy;
			MovingDrawTrace2.x2 -= divxy;

			if (MovingTrace2Dir == 3)
			{
				MovingDrawTrace2.x1 += divxy;
				MovingDrawTrace2.x2 -= divxy;
			}

			break;

		case 3:
			if (MovingTrace1EndPoint == 0)
			{
				MovingDrawTrace1.x1 += divxy;
				MovingDrawTrace1.y1 -= divxy;
				MovingDrawTrace1.x2 -= divxy;
			}
			else
				MovingDrawTrace1.x2 += divxy;

			MovingDrawTrace2.x1 += divxy;
			MovingDrawTrace2.y1 -= divxy;
			MovingDrawTrace2.x2 -= divxy;
			break;
		}

// ***********************************************************************************
		switch (MovingTrace2Dir)
		{
		case 0:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.y1 -= divxy;
				MovingDrawTrace3.x2 += divxy;

				if (MovingTrace1Dir == 3)
				{
					MovingDrawTrace3.y1 -= divxy;
					MovingDrawTrace3.x2 += divxy;
				}
			}
			else
			{
				MovingDrawTrace3.x2 -= divxy;

				if (MovingTrace1Dir == 3)
					MovingDrawTrace3.x2 -= divxy;
			}

//          MovingDrawTrace2.y1-=divxy;
			break;

		case 2:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.x2 -= divxy;

				if (MovingTrace1Dir == 3)
				{
					MovingDrawTrace3.x1 += divxy;
					MovingDrawTrace3.x2 -= divxy;
				}
			}
			else
			{
				MovingDrawTrace3.x2 += divxy;

				if (MovingTrace1Dir == 3)
					MovingDrawTrace3.x2 += divxy;
			}

			MovingDrawTrace2.x2 += divxy;

			if (MovingTrace1Dir == 3)
				MovingDrawTrace2.x2 += divxy;

			break;

		case 3:
			if (MovingTrace2EndPoint == 0)
			{
				MovingDrawTrace3.x1 += divxy;
				MovingDrawTrace3.y1 -= divxy;
				MovingDrawTrace3.x2 -= divxy;
			}
			else
				MovingDrawTrace3.x2 += divxy;

//          MovingDrawTrace2.x1+=divxy;
//          MovingDrawTrace2.y1-=divxy;
			MovingDrawTrace2.x2 += divxy;
			break;
		}

		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveOneTrace(int32 mode)
{
	int32 cnt, Found, Layer, Count1, Count2, OldDrawingLayer, FoundObjects, ok;
	TraceRecord *Trace;
	int32 TempBOOL;
	ObjectRecord *ObjectNet, *Object, Object2, NewTraceObject;
	double cx, cy;

	Trace1Dir = -1;
	Trace2Dir = -1;
	OldDrawingLayer = CurrentDrawingLayer;

	MovingTrace.ObjectType = 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			        && (MovingTrace.ObjectType == 0))
			{
				Trace->Info &= ~OBJECT_SELECTED;
				MovingTrace.Layer = Layer;
				MovingTrace.x1 = Trace->X;
				MovingTrace.y1 = Trace->Y;
				MovingTrace.x2 = Trace->Length;
				MovingTrace.y2 = Trace->ThickNess;
				MovingTrace.NetNr = Trace->NetNr;
				MovingTrace.Clearance = Trace->Clearance;
				MovingTrace.Info = 0;
				MovingTrace.TraceNr = cnt;
				MovingTrace.ObjectType = TRACE_VER;
				TraceDir = 2;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			        && (MovingTrace.ObjectType == 0))
			{
				Trace->Info &= ~OBJECT_SELECTED;
				MovingTrace.Layer = Layer;
				MovingTrace.x1 = Trace->X;
				MovingTrace.y1 = Trace->Y;
				MovingTrace.x2 = Trace->Length;
				MovingTrace.y2 = Trace->ThickNess;
				MovingTrace.NetNr = Trace->NetNr;
				MovingTrace.Clearance = Trace->Clearance;
				MovingTrace.Info = 0;
				MovingTrace.TraceNr = cnt;
				MovingTrace.ObjectType = TRACE_HOR;
				TraceDir = 0;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			        && (MovingTrace.ObjectType == 0))
			{
				Trace->Info &= ~OBJECT_SELECTED;
				MovingTrace.Layer = Layer;
				MovingTrace.x1 = Trace->X;
				MovingTrace.y1 = Trace->Y;
				MovingTrace.x2 = Trace->Length;
				MovingTrace.y2 = Trace->ThickNess;
				MovingTrace.NetNr = Trace->NetNr;
				MovingTrace.Clearance = Trace->Clearance;
				MovingTrace.Info = 0;
				MovingTrace.TraceNr = cnt;
				MovingTrace.ObjectType = TRACE_DIAG1;
				TraceDir = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			        && (MovingTrace.ObjectType == 0))
			{
				Trace->Info &= ~OBJECT_SELECTED;
				MovingTrace.Layer = Layer;
				MovingTrace.x1 = Trace->X;
				MovingTrace.y1 = Trace->Y;
				MovingTrace.x2 = Trace->Length;
				MovingTrace.y2 = Trace->ThickNess;
				MovingTrace.NetNr = Trace->NetNr;
				MovingTrace.Clearance = Trace->Clearance;
				MovingTrace.Info = 0;
				MovingTrace.TraceNr = cnt;
				MovingTrace.ObjectType = TRACE_DIAG2;
				TraceDir = 3;
			}
		}
	}

	if (MovingTrace.ObjectType == 0)
		return;

	DataBaseChanged = 1;
	GetObjectsNet(MovingTrace.NetNr, MODE_OBJECTS3, 0);
	FoundObject3Nr = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		ObjectNet = &((*Objects3)[cnt]);

		if ((ObjectNet->ObjectType == MovingTrace.ObjectType) && (InRange(ObjectNet->x1, MovingTrace.x1))
		        && (InRange(ObjectNet->y1, MovingTrace.y1)) && (InRange(ObjectNet->x2, MovingTrace.x2)))
			FoundObject3Nr = cnt;
	}

// **************************************************************************************
// **************************************************************************************
// Find trace on first point MovingTrace

	Object2.ObjectType = PIN_SMD_ROUND;
	Object2.Layer = MovingTrace.Layer;
	Object2.x1 = MovingTrace.x1;
	Object2.y1 = MovingTrace.y1;
	Object2.x2 = 100.0;
	Object2.y2 = 100.0;
	Object2.Clearance = 1.0;

	FoundObjects1[0].ObjectType = 0;
	FoundObjects2[0].ObjectType = 0;
	Count1 = 0;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		if (cnt != FoundObject3Nr)
		{
			Object = &((*Objects3)[cnt]);

			if ((ObjectIsTrace(Object)) && (ObjectsConnected4(Object, &Object2)))
			{
				memmove(&FoundObjects1[Count1], Object, sizeof(ObjectRecord));

				switch (Object->ObjectType)
				{
				case TRACE_HOR:
					Trace1Dir = 0;
					break;

				case TRACE_VER:
					Trace1Dir = 2;
					break;

				case TRACE_DIAG1:
					Trace1Dir = 1;
					break;

				case TRACE_DIAG2:
					Trace1Dir = 3;
					break;
				}

				if (Count1 == 0)
					FoundObject3Nr1 = cnt;

				if (Count1 < 31)
					Count1++;
			}
		}
	}

// **************************************************************************************
// **************************************************************************************
// Find trace on second point MovingTrace

	Object2.ObjectType = PIN_SMD_ROUND;
	Object2.Layer = MovingTrace.Layer;
	CurrentDrawingLayer = MovingTrace.Layer;

	switch (MovingTrace.ObjectType)
	{
	case TRACE_VER:
		Object2.x1 = MovingTrace.x1;
		Object2.y1 = MovingTrace.y1 + MovingTrace.x2;
		MovingTraceX1 = MovingTrace.x1;
		MovingTraceY1 = MovingTrace.y1;
		MovingTraceX2 = MovingTrace.x1;
		MovingTraceY2 = MovingTrace.y1 + MovingTrace.x2;
		break;

	case TRACE_HOR:
		Object2.x1 = MovingTrace.x1 + MovingTrace.x2;
		Object2.y1 = MovingTrace.y1;
		MovingTraceX1 = MovingTrace.x1;
		MovingTraceY1 = MovingTrace.y1;
		MovingTraceX2 = MovingTrace.x1 + MovingTrace.x2;
		MovingTraceY2 = MovingTrace.y1;
		break;

	case TRACE_DIAG1:
		Object2.x1 = MovingTrace.x1 + MovingTrace.x2;
		Object2.y1 = MovingTrace.y1 - MovingTrace.x2;
		MovingTraceX1 = MovingTrace.x1;
		MovingTraceY1 = MovingTrace.y1;
		MovingTraceX2 = MovingTrace.x1 + MovingTrace.x2;
		MovingTraceY2 = MovingTrace.y1 - MovingTrace.x2;
		break;

	case TRACE_DIAG2:
		Object2.x1 = MovingTrace.x1 + MovingTrace.x2;
		Object2.y1 = MovingTrace.y1 + MovingTrace.x2;
		MovingTraceX1 = MovingTrace.x1;
		MovingTraceY1 = MovingTrace.y1;
		MovingTraceX2 = MovingTrace.x1 + MovingTrace.x2;
		MovingTraceY2 = MovingTrace.y1 + MovingTrace.x2;
		break;
	}

	Object2.x2 = 100.0;
	Object2.y2 = 100.0;
	Object2.Clearance = 1.0;

	Count2 = 0;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		if (cnt != FoundObject3Nr)
		{
			Object = &((*Objects3)[cnt]);

			if ((ObjectIsTrace(Object)) && (ObjectsConnected4(Object, &Object2)))
			{
				memmove(&FoundObjects2[Count2], Object, sizeof(ObjectRecord));

				switch (Object->ObjectType)
				{
				case TRACE_HOR:
					Trace2Dir = 0;
					break;

				case TRACE_VER:
					Trace2Dir = 2;
					break;

				case TRACE_DIAG1:
					Trace2Dir = 1;
					break;

				case TRACE_DIAG2:
					Trace2Dir = 3;
					break;
				}

				if (Count2 == 0)
					FoundObject3Nr2 = cnt;

				if (Count2 < 31)
					Count2++;
			}
		}
	}

	if ((Count1 > 1) || (Count2 > 1))
	{
		ok = 1;
		return;
	}

	if ((Count1 == 0) && (Count2 == 0))
	{
		ok = 1;
		return;
	}

	if ((FoundObjects1[0].ObjectType != 0) && (!ObjectIsTrace(&FoundObjects1[0])))
		return;

	if ((FoundObjects2[0].ObjectType != 0) && (!ObjectIsTrace(&FoundObjects2[0])))
		return;

	Found = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		if ((cnt != FoundObject3Nr) && (cnt != FoundObject3Nr1) && (cnt != FoundObject3Nr2))
		{
			Object = &((*Objects3)[cnt]);

			if ((Object->ObjectType != CONNECTION) && (ObjectsConnected(&MovingTrace, Object)))
			{
				if (Found == -1)
					Found = cnt;
			}
		}
	}

// **************************************************************************************
// **************************************************************************************

	memmove(&MovingTrace1, &FoundObjects1[0], sizeof(ObjectRecord));
	memmove(&MovingTrace2, &FoundObjects2[0], sizeof(ObjectRecord));
	Layer = MovingTrace.Layer;

	if (TraceDir == Trace1Dir)
	{
		switch (MovingTrace.ObjectType)
		{
		case TRACE_HOR:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));
			NewTraceObject.x1 = min(NewTraceObject.x1, MovingTrace1.x1);
			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace1.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*HorTraces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*HorTraces[Layer])[MovingTrace1.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;

		case TRACE_VER:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));
			NewTraceObject.y1 = min(NewTraceObject.y1, MovingTrace1.y1);
			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace1.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*VerTraces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*VerTraces[Layer])[MovingTrace1.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;

		case TRACE_DIAG1:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));

			if (NewTraceObject.x1 > MovingTrace1.x1)
			{
				NewTraceObject.x1 = MovingTrace1.x1;
				NewTraceObject.y1 = MovingTrace1.y1;
			}

			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace1.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*Diag1Traces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*Diag1Traces[Layer])[MovingTrace1.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;

		case TRACE_DIAG2:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));

			if (NewTraceObject.x1 > MovingTrace1.x1)
			{
				NewTraceObject.x1 = MovingTrace1.x1;
				NewTraceObject.y1 = MovingTrace1.y1;
			}

			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace1.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*Diag2Traces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*Diag2Traces[Layer])[MovingTrace1.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;
		}
	}

	if (TraceDir == Trace2Dir)
	{
		switch (MovingTrace.ObjectType)
		{
		case TRACE_HOR:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));
			NewTraceObject.x1 = min(NewTraceObject.x1, MovingTrace2.x1);
			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace2.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*HorTraces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*HorTraces[Layer])[MovingTrace2.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;

		case TRACE_VER:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));
			NewTraceObject.y1 = min(NewTraceObject.y1, MovingTrace2.y1);
			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace2.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*VerTraces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*VerTraces[Layer])[MovingTrace2.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;

		case TRACE_DIAG1:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));

			if (NewTraceObject.x1 > MovingTrace2.x1)
			{
				NewTraceObject.x1 = MovingTrace2.x1;
				NewTraceObject.y1 = MovingTrace2.y1;
			}

			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace2.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*Diag1Traces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*Diag1Traces[Layer])[MovingTrace2.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;

		case TRACE_DIAG2:
			memcpy(&NewTraceObject, &MovingTrace, sizeof(ObjectRecord));

			if (NewTraceObject.x1 > MovingTrace2.x1)
			{
				NewTraceObject.x1 = MovingTrace2.x1;
				NewTraceObject.y1 = MovingTrace2.y1;
			}

			NewTraceObject.x2 = NewTraceObject.x2 + MovingTrace2.x2;
			AddTrace(&NewTraceObject);
			Trace = &((*Diag2Traces[Layer])[MovingTrace.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			Trace = &((*Diag2Traces[Layer])[MovingTrace2.TraceNr]);
			Trace->Info &= ~OBJECT_SELECTED;
			Trace->Info |= OBJECT_NOT_VISIBLE;
			Trace->DeleteNr = (int16) LastActionNr;
			LastActionNr++;
			DataBaseChanged = 1;
			RePaint();
			return;
		}
	}

	switch (MovingTrace1.ObjectType)
	{
	case TRACE_VER:
		MovingTrace1X1 = MovingTrace1.x1;
		MovingTrace1Y1 = MovingTrace1.y1;
		MovingTrace1X2 = MovingTrace1.x1;
		MovingTrace1Y2 = MovingTrace1.y1 + MovingTrace1.x2;
		break;

	case TRACE_HOR:
		MovingTrace1X1 = MovingTrace1.x1;
		MovingTrace1Y1 = MovingTrace1.y1;
		MovingTrace1X2 = MovingTrace1.x1 + MovingTrace1.x2;
		MovingTrace1Y2 = MovingTrace1.y1;
		break;

	case TRACE_DIAG1:
		MovingTrace1X1 = MovingTrace1.x1;
		MovingTrace1Y1 = MovingTrace1.y1;
		MovingTrace1X2 = MovingTrace1.x1 + MovingTrace1.x2;
		MovingTrace1Y2 = MovingTrace1.y1 - MovingTrace1.x2;
		break;

	case TRACE_DIAG2:
		MovingTrace1X1 = MovingTrace1.x1;
		MovingTrace1Y1 = MovingTrace1.y1;
		MovingTrace1X2 = MovingTrace1.x1 + MovingTrace1.x2;
		MovingTrace1Y2 = MovingTrace1.y1 + MovingTrace1.x2;
		break;

	case 0:
		MovingTrace1X1 = MovingTraceX1;
		MovingTrace1Y1 = MovingTraceY1;
		MovingTrace1X2 = MovingTraceX1;
		MovingTrace1Y2 = MovingTraceY1;
		break;
	}

	MovingTrace2X1 = MovingTraceX2;
	MovingTrace2Y1 = MovingTraceY2;
	MovingTrace2X2 = MovingTraceX2;
	MovingTrace2Y2 = MovingTraceY2;

	switch (MovingTrace2.ObjectType)
	{
	case TRACE_VER:
		MovingTrace2X1 = MovingTrace2.x1;
		MovingTrace2Y1 = MovingTrace2.y1;
		MovingTrace2X2 = MovingTrace2.x1;
		MovingTrace2Y2 = MovingTrace2.y1 + MovingTrace2.x2;
		break;

	case TRACE_HOR:
		MovingTrace2X1 = MovingTrace2.x1;
		MovingTrace2Y1 = MovingTrace2.y1;
		MovingTrace2X2 = MovingTrace2.x1 + MovingTrace2.x2;
		MovingTrace2Y2 = MovingTrace2.y1;
		break;

	case TRACE_DIAG1:
		MovingTrace2X1 = MovingTrace2.x1;
		MovingTrace2Y1 = MovingTrace2.y1;
		MovingTrace2X2 = MovingTrace2.x1 + MovingTrace2.x2;
		MovingTrace2Y2 = MovingTrace2.y1 - MovingTrace2.x2;
		break;

	case TRACE_DIAG2:
		MovingTrace2X1 = MovingTrace2.x1;
		MovingTrace2Y1 = MovingTrace2.y1;
		MovingTrace2X2 = MovingTrace2.x1 + MovingTrace2.x2;
		MovingTrace2Y2 = MovingTrace2.y1 + MovingTrace2.x2;
		break;

	case 0:
		MovingTrace2X1 = MovingTraceX2;
		MovingTrace2Y1 = MovingTraceY2;
		MovingTrace2X2 = MovingTraceX2;
		MovingTrace2Y2 = MovingTraceY2;
		break;
	}

// **************************************************************************************
// **************************************************************************************
// Check if three traces are connected to each other

	if (!
	        (((InRange(MovingTraceX1, MovingTrace1X1)) && (InRange(MovingTraceY1, MovingTrace1Y1)))
	         || ((InRange(MovingTraceX1, MovingTrace1X2)) && (InRange(MovingTraceY1, MovingTrace1Y2))))
	        && (((InRange(MovingTraceX2, MovingTrace2X1)) && (InRange(MovingTraceY2, MovingTrace2Y1)))
	            || ((InRange(MovingTraceX2, MovingTrace2X2)) && (InRange(MovingTraceY2, MovingTrace2Y2)))))
		return;

// **************************************************************************************
// **************************************************************************************

	MovingTrace1Dir = -1;
	MovingTrace2Dir = -1;
	MovingTrace1EndPoint = -1;
	MovingTrace2EndPoint = -1;

	switch (MovingTrace1.ObjectType)
	{
	case TRACE_VER:
		MovingTrace1Dir = 0;
		break;

	case TRACE_HOR:
		MovingTrace1Dir = 2;
		break;

	case TRACE_DIAG1:
		MovingTrace1Dir = 3;
		break;

	case TRACE_DIAG2:
		MovingTrace1Dir = 1;
		break;

	case 0:
		MovingTrace1Dir = TraceDir;
		break;
	}

	switch (MovingTrace2.ObjectType)
	{
	case TRACE_VER:
		MovingTrace2Dir = 0;
		break;

	case TRACE_HOR:
		MovingTrace2Dir = 2;
		break;

	case TRACE_DIAG1:
		MovingTrace2Dir = 3;
		break;

	case TRACE_DIAG2:
		MovingTrace2Dir = 1;
		break;

	case 0:
		MovingTrace2Dir = TraceDir;
		break;
	}

	if (MovingTrace1.ObjectType == 0)
	{
		MovingTrace1Dir = (MovingTrace2Dir + 2) & 3;

		switch (MovingTrace1Dir)
		{
		case 0:
			MovingTrace1.ObjectType = TRACE_VER;

			if (MovingTrace.ObjectType == TRACE_DIAG2)
				MovingTrace1EndPoint = 1;
			else
				MovingTrace1EndPoint = 0;

			break;

		case 1:
			MovingTrace1.ObjectType = TRACE_DIAG2;
			MovingTrace1EndPoint = 1;
			break;

		case 2:
			MovingTrace1.ObjectType = TRACE_HOR;
			MovingTrace1EndPoint = 1;
			break;

		case 3:
			MovingTrace1.ObjectType = TRACE_DIAG1;

			if (MovingTrace.ObjectType == TRACE_HOR)
				MovingTrace1EndPoint = 1;
			else
				MovingTrace1EndPoint = 0;

			break;
		}

		MovingTrace1.x1 = MovingTrace1X1;
		MovingTrace1.y1 = MovingTrace1Y1;
		MovingTrace1.x2 = 0.0;
		MovingTrace1.y2 = MovingTrace.y2;
		MovingTrace1.NetNr = MovingTrace.NetNr;
		MovingTrace1.Layer = MovingTrace.Layer;
		MovingTrace1.Clearance = MovingTrace.Clearance;
		MovingTrace1.Info = 0;
		MovingTrace1.TraceNr = -1;
	}

	if (MovingTrace2.ObjectType == 0)
	{
		MovingTrace2Dir = (MovingTrace1Dir + 2) & 3;

		switch (MovingTrace1Dir)
		{
		case 0:
			MovingTrace2.ObjectType = TRACE_HOR;
			MovingTrace2EndPoint = 0;
			break;

		case 1:
			MovingTrace2.ObjectType = TRACE_DIAG1;

			if (MovingTrace.ObjectType == TRACE_HOR)
				MovingTrace2EndPoint = 0;
			else
				MovingTrace2EndPoint = 1;

			break;

		case 2:
			MovingTrace2.ObjectType = TRACE_VER;

			if (MovingTrace.ObjectType == TRACE_DIAG2)
				MovingTrace2EndPoint = 0;
			else
				MovingTrace2EndPoint = 1;

			break;

		case 3:
			MovingTrace2.ObjectType = TRACE_DIAG2;
			MovingTrace2EndPoint = 0;
			break;
		}

		MovingTrace2.x1 = MovingTrace2X1;
		MovingTrace2.y1 = MovingTrace2Y1;
		MovingTrace2.x2 = 0.0;
		MovingTrace2.y2 = MovingTrace.y2;
		MovingTrace2.NetNr = MovingTrace.NetNr;
		MovingTrace2.Layer = MovingTrace.Layer;
		MovingTrace2.Clearance = MovingTrace.Clearance;
		MovingTrace2.Info = 0;
		MovingTrace2.TraceNr = -1;
	}

	if (MovingTrace1.TraceNr != -1)
	{
		if ((InRange(MovingTraceX1, MovingTrace1X1)) && (InRange(MovingTraceY1, MovingTrace1Y1)))
			MovingTrace1EndPoint = 0;
		else
			MovingTrace1EndPoint = 1;
	}

	if (MovingTrace2.TraceNr != -1)
	{
		if ((InRange(MovingTraceX2, MovingTrace2X1)) && (InRange(MovingTraceY2, MovingTrace2Y1)))
			MovingTrace2EndPoint = 0;
		else
			MovingTrace2EndPoint = 1;
	}

// **************************************************************************************
// **************************************************************************************

	CurrentClearance = MovingTrace.Clearance;
	CurrentTraceWidth = MovingTrace.y2;
	CurrentDrawingNetNr = MovingTrace.NetNr;
	CurrentDrawingLayer = MovingTrace.Layer;
	MovingCurrentDrawX1 = MovingTraceX1;
	MovingCurrentDrawY1 = MovingTraceY1;

	switch (MovingTrace.ObjectType)
	{
	case TRACE_VER:
		MovingCurrentDrawY1 += MovingTrace.x2 * 0.5;
		break;

	case TRACE_HOR:
		MovingCurrentDrawX1 += MovingTrace.x2 * 0.5;
		break;

	case TRACE_DIAG1:
		MovingCurrentDrawX1 += MovingTrace.x2 * 0.5;
		MovingCurrentDrawY1 -= MovingTrace.x2 * 0.5;
		break;

	case TRACE_DIAG2:
		MovingCurrentDrawX1 += MovingTrace.x2 * 0.5;
		MovingCurrentDrawY1 += MovingTrace.x2 * 0.5;
		break;
	}

	switch (MovingTrace.ObjectType)
	{
	case TRACE_VER:
		cx = MovingTrace.x1;
		cy = MovingTrace.y1 + MovingTrace.x2 * 0.5;
		break;

	case TRACE_HOR:
		cx = MovingTrace.x1 + MovingTrace.x2 * 0.5;
		cy = MovingTrace.y1;
		break;

	case TRACE_DIAG1:
		cx = MovingTrace.x1 + MovingTrace.x2 * 0.5;
		cy = MovingTrace.y1 - MovingTrace.x2 * 0.5;
		break;

	case TRACE_DIAG2:
		cx = MovingTrace.x1 + MovingTrace.x2 * 0.5;
		cy = MovingTrace.y1 + MovingTrace.x2 * 0.5;
		break;

	default:
		cx = 0.0;
		cy = 0.0;
		break;
	}

	SearchMinX = cx - 50.0e5;
	SearchMaxX = cx + 50.0e5;
	SearchMinY = cy - 50.0e5;
	SearchMaxY = cy + 50.0e5;
	/*
	  SearchMinX=ViewMinX;
	  SearchMinY=ViewMinY;
	  SearchMaxX=ViewMaxX;
	  SearchMaxY=ViewMaxY;
	*/
	FoundObjects = CopyCopperObjectsFromRectWindowToObjects4(MovingTrace.Layer, 4);

	TempBOOL = DrawClearanceForTryingTrace;
	DrawClearanceForTryingTrace = 0;

	if (OldDrawingLayer != CurrentDrawingLayer)
	{
		OldDrawingLayer = CurrentDrawingLayer;
		RePaint();
		CheckInputMessages(0);
	}

	MoveOneTrace2(mode);
	DrawClearanceForTryingTrace = TempBOOL;
	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeObjectTraceWidth()
{
	int32 Layer, TraceInfo, TempLastActionNr, cnt, res, NrTracesDeleted, ClearanceErrorTraces;
	TraceRecord *Trace;
	ObjectRecord TraceObject;
	char str2[MAX_LENGTH_STRING];

	NewValue.Value = CurrentTraceWidth;
	NewValue.MinValue = (0.5 * 2540);
	NewValue.MaxValue = (10000 * 2540);

	if ((res = ValueDialog(0)) == 2)
		return -1;

	ClearanceErrorTraces = 0;
	TempLastActionNr = (int16) LastActionNr - 1;
	NrTracesDeleted = 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				if (NotInRange(Trace->ThickNess, NewValue.Value))
				{
					TraceObject.Layer = Layer;
					TraceObject.x1 = Trace->X;
					TraceObject.y1 = Trace->Y;
					TraceObject.x2 = Trace->Length;
					TraceObject.y2 = NewValue.Value;
					TraceObject.NetNr = Trace->NetNr;
					TraceObject.Clearance = Trace->Clearance;
					TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
					TraceObject.TraceNr = cnt;
					TraceObject.ObjectType = TRACE_VER;
					FillPositionObject(&TraceObject);

					if (!CheckObjectOverlapped(&TraceObject, 0))
					{
						if (AddTrace(&TraceObject))
						{
							if (RecalcAreafillAfterInsert)
								InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

							Trace = &((*VerTraces[Layer])[cnt]);
							Trace->Info &= ~OBJECT_SELECTED;
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;
						}
					}
					else
						ClearanceErrorTraces++;
				}
				else
					Trace->Info &= ~OBJECT_SELECTED;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				if (NotInRange(Trace->ThickNess, NewValue.Value))
				{
					TraceObject.Layer = Layer;
					TraceObject.x1 = Trace->X;
					TraceObject.y1 = Trace->Y;
					TraceObject.x2 = Trace->Length;
					TraceObject.y2 = NewValue.Value;
					TraceObject.NetNr = Trace->NetNr;
					TraceObject.Clearance = Trace->Clearance;
					TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
					TraceObject.TraceNr = cnt;
					TraceObject.ObjectType = TRACE_HOR;
					FillPositionObject(&TraceObject);

					if (!CheckObjectOverlapped(&TraceObject, 0))
					{
						if (AddTrace(&TraceObject))
						{
							if (RecalcAreafillAfterInsert)
								InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

							Trace = &((*HorTraces[Layer])[cnt]);
							Trace->Info &= ~OBJECT_SELECTED;
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;
						}
					}
					else
						ClearanceErrorTraces++;
				}
				else
					Trace->Info &= ~OBJECT_SELECTED;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				if (NotInRange(Trace->ThickNess, NewValue.Value))
				{
					TraceObject.Layer = Layer;
					TraceObject.x1 = Trace->X;
					TraceObject.y1 = Trace->Y;
					TraceObject.x2 = Trace->Length;
					TraceObject.y2 = NewValue.Value;
					TraceObject.NetNr = Trace->NetNr;
					TraceObject.Clearance = Trace->Clearance;
					TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
					TraceObject.TraceNr = cnt;
					TraceObject.ObjectType = TRACE_DIAG1;
					FillPositionObject(&TraceObject);

					if (!CheckObjectOverlapped(&TraceObject, 0))
					{
						if (AddTrace(&TraceObject))
						{
							if (RecalcAreafillAfterInsert)
								InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

							Trace = &((*Diag1Traces[Layer])[cnt]);
							Trace->Info &= ~OBJECT_SELECTED;
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;
						}
					}
					else
						ClearanceErrorTraces++;
				}
				else
					Trace->Info &= ~OBJECT_SELECTED;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				if (NotInRange(Trace->ThickNess, NewValue.Value))
				{
					TraceObject.Layer = Layer;
					TraceObject.x1 = Trace->X;
					TraceObject.y1 = Trace->Y;
					TraceObject.x2 = Trace->Length;
					TraceObject.y2 = NewValue.Value;
					TraceObject.NetNr = Trace->NetNr;
					TraceObject.Clearance = Trace->Clearance;
					TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
					TraceObject.TraceNr = cnt;
					TraceObject.ObjectType = TRACE_DIAG2;
					FillPositionObject(&TraceObject);

					if (!CheckObjectOverlapped(&TraceObject, 0))
					{
						if (AddTrace(&TraceObject))
						{
							if (RecalcAreafillAfterInsert)
								InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

							Trace = &((*Diag2Traces[Layer])[cnt]);
							Trace->Info &= ~OBJECT_SELECTED;
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;
						}
					}
					else
						ClearanceErrorTraces++;
				}
				else
					Trace->Info &= ~OBJECT_SELECTED;
			}
		}
	}

	RePaint();
	MessageBufPos = 0;

	if (ClearanceErrorTraces > 0)
	{
		sprintf(str2, SC(1078, "The width of %i trace(s) can not be changed"), ClearanceErrorTraces);

		if (AddToMessageBuf(str2) != 0)
			return -1;
	}

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(1, "Message"), 0, 0);
		DeAllocateMemMessageBuf();
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeClearance(int32 mode)
{
	int32 Layer, TraceInfo, ViaInfo, TempLastActionNr, ClearanceErrorTraces, ClearanceErrorVias;
	int32 cnt, NrTracesDeleted;
	TraceRecord *Trace;
	ViaRecord *Via, NewVia;
	ObjectRecord TraceObject, ViaObject;
	ObjectLineRecord *ObjectLine, ChangedObjectLine;
	ObjectArcRecord *ObjectArc, ChangedObjectArc;

	char str2[MAX_LENGTH_STRING];

	if ((mode & 1) == 0)
	{
		ClearanceErrorTraces = 0;
		ClearanceErrorVias = 0;
		NewValue.Value = CurrentClearance;
		NewValue.MinValue = (0.1 * 2540);
		NewValue.MaxValue = (10000 * 2540);

		if (ValueDialog(1) == 2)
			return -1;

		TempLastActionNr = (int16) LastActionNr - 1;
		NrTracesDeleted = 0;

		for (Layer = 0; Layer < 32; Layer++)
		{
			DrawCode = DrawLayerCode[Layer];

			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (Trace->AddNr <= TempLastActionNr))
				{
					if (NotInRange(Trace->Clearance, NewValue.Value))
					{
						TraceObject.Layer = Layer;
						TraceObject.x1 = Trace->X;
						TraceObject.y1 = Trace->Y;
						TraceObject.x2 = Trace->Length;
						TraceObject.y2 = Trace->ThickNess;
						TraceObject.NetNr = Trace->NetNr;
						TraceObject.Clearance = NewValue.Value;
						TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
						TraceObject.TraceNr = cnt;
						TraceObject.ObjectType = TRACE_VER;
						FillPositionObject(&TraceObject);

						if (!CheckObjectOverlapped(&TraceObject, 0))
						{
							if (AddTrace(&TraceObject))
							{
								if (RecalcAreafillAfterInsert)
									InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

								Trace = &((*VerTraces[Layer])[cnt]);
								Trace->Info &= ~OBJECT_SELECTED;
								Trace->Info |= OBJECT_NOT_VISIBLE;
								Trace->DeleteNr = (int16) LastActionNr;
							}
						}
						else
							ClearanceErrorTraces++;
					}
					else
						Trace->Info &= ~OBJECT_SELECTED;
				}
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (Trace->AddNr <= TempLastActionNr))
				{
					if (NotInRange(Trace->Clearance, NewValue.Value))
					{
						TraceObject.Layer = Layer;
						TraceObject.x1 = Trace->X;
						TraceObject.y1 = Trace->Y;
						TraceObject.x2 = Trace->Length;
						TraceObject.y2 = Trace->ThickNess;
						TraceObject.NetNr = Trace->NetNr;
						TraceObject.Clearance = NewValue.Value;
						TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
						TraceObject.TraceNr = cnt;
						TraceObject.ObjectType = TRACE_HOR;
						FillPositionObject(&TraceObject);

						if (!CheckObjectOverlapped(&TraceObject, 0))
						{
							if (AddTrace(&TraceObject))
							{
								if (RecalcAreafillAfterInsert)
									InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

								Trace = &((*HorTraces[Layer])[cnt]);

								if (DrawCode < MAX_ACTIVE_DRAWING_LAYERS)
									Trace->Info &= ~OBJECT_SELECTED;

								Trace->Info |= OBJECT_NOT_VISIBLE;
								Trace->DeleteNr = (int16) LastActionNr;
							}
						}
						else
							ClearanceErrorTraces++;
					}
					else
						Trace->Info &= ~OBJECT_SELECTED;
				}
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (Trace->AddNr <= TempLastActionNr))
				{
					if (NotInRange(Trace->Clearance, NewValue.Value))
					{
						TraceObject.Layer = Layer;
						TraceObject.x1 = Trace->X;
						TraceObject.y1 = Trace->Y;
						TraceObject.x2 = Trace->Length;
						TraceObject.y2 = Trace->ThickNess;
						TraceObject.NetNr = Trace->NetNr;
						TraceObject.Clearance = NewValue.Value;
						TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
						TraceObject.TraceNr = cnt;
						TraceObject.ObjectType = TRACE_DIAG1;
						FillPositionObject(&TraceObject);

						if (!CheckObjectOverlapped(&TraceObject, 0))
						{
							if (AddTrace(&TraceObject))
							{
								if (RecalcAreafillAfterInsert)
									InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

								Trace = &((*Diag1Traces[Layer])[cnt]);
								Trace->Info &= ~OBJECT_SELECTED;
								Trace->Info |= OBJECT_NOT_VISIBLE;
								Trace->DeleteNr = (int16) LastActionNr;
							}
						}
						else
							ClearanceErrorTraces++;
					}
					else
						Trace->Info &= ~OBJECT_SELECTED;
				}
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (Trace->AddNr <= TempLastActionNr))
				{
					if (NotInRange(Trace->Clearance, NewValue.Value))
					{
						TraceObject.Layer = Layer;
						TraceObject.x1 = Trace->X;
						TraceObject.y1 = Trace->Y;
						TraceObject.x2 = Trace->Length;
						TraceObject.y2 = Trace->ThickNess;
						TraceObject.NetNr = Trace->NetNr;
						TraceObject.Clearance = NewValue.Value;
						TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
						TraceObject.TraceNr = cnt;
						TraceObject.ObjectType = TRACE_DIAG2;
						FillPositionObject(&TraceObject);

						if (!CheckObjectOverlapped(&TraceObject, 0))
						{
							if (AddTrace(&TraceObject))
							{
								if (RecalcAreafillAfterInsert)
									InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

								Trace = &((*Diag2Traces[Layer])[cnt]);
								Trace->Info &= ~OBJECT_SELECTED;
								Trace->Info |= OBJECT_NOT_VISIBLE;
								Trace->DeleteNr = (int16) LastActionNr;
							}
						}
						else
							ClearanceErrorTraces++;
					}
					else
						Trace->Info &= ~OBJECT_SELECTED;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);
			ViaInfo = Via->Info;

			if (((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Via->AddNr <= TempLastActionNr))
			{
				if (NotInRange(Via->Clearance, NewValue.Value))
				{
					ViaObject.ObjectType = VIA_PUT_THROUGH_ROUND;
					ViaObject.x1 = Via->X;
					ViaObject.y1 = Via->Y;
					ViaObject.Layer = -1;
					//      ViaObject.Layer=Via->Layer;
					ViaObject.x2 = Via->ThickNess;
					ViaObject.NetNr = Via->NetNr;
					ViaObject.Clearance = NewValue.Value;
					FillPositionObject(&ViaObject);

					if (!CheckObjectOverlapped(&ViaObject, 0))
					{
						memmove(&NewVia, Via, sizeof(ViaRecord));
						NewVia.Clearance = (float) NewValue.Value;
						NewVia.Info &= ~OBJECT_SELECTED;

						if (AddVia(&NewVia))
						{
							if (RecalcAreafillAfterInsert)
								InsertObjectInAreaFill(&ViaObject, -1, ViaObject.NetNr, 2);

							Via = &((*Vias)[cnt]);
							Via->Info &= ~OBJECT_SELECTED;
							Via->Info |= OBJECT_NOT_VISIBLE;
							Via->DeleteNr = (int16) LastActionNr;
						}
					}
					else
						ClearanceErrorVias++;
				}
				else
					Via->Info &= ~OBJECT_SELECTED;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (CheckIfLayerHasObjectWithClearances(ObjectLine->Layer)) && (ObjectLine->NetNr >= 0)
			        && (NotInRange(ObjectLine->Clearance, NewValue.Value)) && (ObjectLine->AddNr <= TempLastActionNr))
			{
				ObjectLine->Info &= ~OBJECT_SELECTED;
				memmove(&ChangedObjectLine, ObjectLine, sizeof(ObjectLineRecord));
				ChangedObjectLine.Clearance = (float) NewValue.Value;

				if (AddObjectLine(&ChangedObjectLine))
				{
					ObjectLine = &((*ObjectLines)[cnt]);
					ObjectLine->Info |= OBJECT_NOT_VISIBLE;
					ObjectLine->DeleteNr = (int16) LastActionNr;
				}
			}
		}

		/*
		    for (cnt=0;cnt<Design.NrObjectRects;cnt++) {
		      ObjectRect=&((*ObjectRects)[cnt]);
		      if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
		         &&
		         (CheckIfLayerHasObjectWithClearances(ObjectRect->Layer))
		         &&
		         (NotInRange(ObjectRect->Clearance,NewValue.Value))
		         &&
		         (ObjectRect->AddNr<=TempLastActionNr)) {
		        ObjectRect->Info&=~OBJECT_SELECTED;
		        memmove(&ChangedObjectRect,ObjectRect,sizeof(ObjectRectRecord));
		        ChangedObjectRect.Clearance=NewValue.Value;
		        if (AddObjectRect(&ChangedObjectRect)) {
		          ObjectRect=&((*ObjectRects)[cnt]);
		          SetBackGroundActive(0);
		          DrawObjectRect(ObjectRect,0.0,0.0,0);
		          ObjectRect->Info|=OBJECT_NOT_VISIBLE;
		          ObjectRect->DeleteNr=(int16)LastActionNr;
		          DrawObjectRect(&ChangedObjectRect,0.0,0.0,0);
		        }
		      }
		    }
		*/
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (CheckIfLayerHasObjectWithClearances(ObjectArc->Layer))
			        && (NotInRange(ObjectArc->Clearance, NewValue.Value)) && (ObjectArc->AddNr <= TempLastActionNr))
			{
				ObjectArc->Info &= ~OBJECT_SELECTED;
				memmove(&ChangedObjectArc, ObjectArc, sizeof(ObjectArcRecord));
				ChangedObjectArc.Clearance = (float) NewValue.Value;

				if (AddObjectArc(&ChangedObjectArc))
				{
					ObjectArc = &((*ObjectArcs)[cnt]);
					ObjectArc->Info |= OBJECT_NOT_VISIBLE;
					ObjectArc->DeleteNr = (int16) LastActionNr;
				}
			}
		}

		/*
		    for (cnt=0;cnt<Design.NrObjectPolygons;cnt++) {
		      ObjectPolygon=(ObjectPolygonRecord *)&(ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		      if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
		         &&
		         (CheckIfLayerHasObjectWithClearances(ObjectPolygon->Layer))
		         &&
		         (NotInRange(ObjectPolygon->Clearance,NewValue.Value))
		         &&
		         (ObjectPolygon->AddNr<=TempLastActionNr)) {
		      }
		    }
		*/
		ChangeClearanceAreaFill(NewValue.Value, 0);
		RePaint();
	}
	else
	{
		ChangeClearanceAreaFill(NewValue.Value, 1);
		RePaint();
		return 0;
	}

	MessageBufPos = 0;

	if (ClearanceErrorTraces > 0)
	{
		sprintf(str2, SC(1079, "The clearance of %i trace(s) can not be changed"), ClearanceErrorTraces);

		if (AddToMessageBuf(str2) != 0)
			return -1;
	}

	if (ClearanceErrorVias > 0)
		sprintf(str2, SC(1080, "The clearance of %i via(s) can not be changed"), ClearanceErrorVias);

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(1, "Message"), 0, 0);
		DeAllocateMemMessageBuf();
	}

	/*
	  for (cnt=0;cnt<Design.NrNets;cnt++) {
	    if (HNets[cnt]==1) {
	      ReCalcConnectionsNet((int32)cnt,0);
	      CheckInputMessages(0);
	    }
	  }
	*/
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeVias()
{
	int32 ViaInfo, TempLastActionNr, cnt, ClearanceErrorVias;
	ObjectRecord ViaObject;
	ViaRecord *Via, NewVia, TempVia, *FirstVia;
	char str2[MAX_LENGTH_STRING];

	memmove(&TempVia, &CurrentVia, sizeof(ViaRecord));
	FirstVia = NULL;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (!FirstVia)
				FirstVia = Via;
		}
	}

	if (!FirstVia)
		return;

	memcpy(&CurrentVia, FirstVia, sizeof(ViaRecord));
	PcbSettingsDialog(IDD_DIALOG_VIA, 0);
	ClearanceErrorVias = 0;
	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if (((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Via->AddNr <= TempLastActionNr))
		{
			if ((NotInRange(Via->ThickNess, CurrentVia.ThickNess))
			        || (NotInRange(Via->DrillThickNess, CurrentVia.DrillThickNess))
			        || (NotInRange(Via->SoldMask, CurrentVia.SoldMask))
			        || (NotInRange(Via->ThermalInner, CurrentVia.ThermalInner))
			        || (NotInRange(Via->Clearance, CurrentVia.Clearance))
			        || ((Via->ViaType & 3) != (CurrentVia.ViaType & 3)))
			{
				ViaObject.ObjectType = VIA_PUT_THROUGH_ROUND;
				ViaObject.x1 = Via->X;
				ViaObject.y1 = Via->Y;
				ViaObject.Layer = -1;
				//      ViaObject.Layer=Via->Layer;
				ViaObject.x2 = CurrentVia.ThickNess;
				ViaObject.y2 = CurrentVia.DrillThickNess;
				ViaObject.NetNr = Via->NetNr;
				ViaObject.Clearance = CurrentVia.Clearance;
				FillPositionObject(&ViaObject);

				if (!CheckObjectOverlapped(&ViaObject, 0))
				{
					if (RecalcAreafillAfterInsert)
						InsertObjectInAreaFill(&ViaObject, -1, ViaObject.NetNr, 2);

					memmove(&NewVia, Via, sizeof(ViaRecord));
					NewVia.ThickNess = CurrentVia.ThickNess;
					NewVia.DrillThickNess = CurrentVia.DrillThickNess;
					NewVia.Clearance = CurrentVia.Clearance;
					NewVia.ThermalInner = CurrentVia.ThermalInner;
					NewVia.SoldMask = CurrentVia.SoldMask;
					NewVia.Info &= ~OBJECT_SELECTED;
					NewVia.ViaType &= ~3;
					NewVia.ViaType |= CurrentVia.ViaType & 3;

					if (AddVia(&NewVia))
					{
						Via = &((*Vias)[cnt]);
						Via->Info &= ~OBJECT_SELECTED;
						Via->Info |= OBJECT_NOT_VISIBLE;
						Via->DeleteNr = (int16) LastActionNr;
					}
				}
				else
					ClearanceErrorVias++;
			}
			else
				Via->Info &= ~OBJECT_SELECTED;
		}
	}

	RePaint();
	MessageBufPos = 0;

	if (ClearanceErrorVias > 0)
	{
		sprintf(str2, SC(1081, "%i via(s) can not be changed"), ClearanceErrorVias);

		if (AddToMessageBuf(str2) != 0)
			return;
	}

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(1, "Message"), 0, 0);
		DeAllocateMemMessageBuf();
	}

	memmove(&CurrentVia, &TempVia, sizeof(ViaRecord));
	/*
	  for (cnt=0;cnt<Design.NrNets;cnt++) {
	    if (HNets[cnt]==1) {
	      ReCalcConnectionsNet((int32)cnt,0);
	      CheckInputMessages(0);
	    }
	  }
	*/
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 HighLightNet(int32 mode)
{
	int32 Layer, TraceInfo, ViaInfo, cnt;
	TraceRecord *Trace;
	ViaRecord *Via;
//  ObjectLineRecord *ObjectLine,ChangedObjectLine;


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
				return Trace->NetNr;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Trace->Info &= ~OBJECT_SELECTED;
				return Trace->NetNr;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Trace->Info &= ~OBJECT_SELECTED;
				return Trace->NetNr;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Trace->Info &= ~OBJECT_SELECTED;
				return Trace->NetNr;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Via = &((*Vias)[cnt]);
			Via->Info &= ~OBJECT_SELECTED;
			return Via->NetNr;
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectLines;cnt++) {
	    ObjectLine=&((*ObjectLines)[cnt]);
	    if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (CheckIfLayerHasObjectWithClearances(ObjectLine->Layer))
	       &&
	       (ObjectLine->NetNr>=0)
	       &&
	       (NotInRange(ObjectLine->Clearance,NewValue.Value))
	       &&
	       (ObjectLine->AddNr<=TempLastActionNr)) {
	      ObjectLine->Info&=~OBJECT_SELECTED;
	    }
	  }
	*/

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
