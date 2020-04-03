/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: mainloop.c
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
#include "help.h"
#include "keyswin.h"
#include "toets.h"
#include "commdlg.h"
#include "pcb.h"
#include "calcdef.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "select2.h"
#include "mainloop.h"
#include "nets.h"
#include "import.h"
#include "files.h"
#include "trace2.h"
#include "trace3.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "menus.h"
#include "dialogs.h"
#include "insdel.h"
#include "select.h"
#include "graphics.h"
#include "trace4.h"
#include "trace5.h"
#include "resource.h"
#include "movecomp.h"
#include "move2.h"
#include "polygon.h"
#include "gerber.h"
#include "print.h"
#include "gateswap.h"
#include "dxf.h"
#include "InputGerber.h"
#include "font.h"
#include "edit2.h"
#include "owntime.h"


int32 MaxCollections, px, py, ro, OldMousePosX, OldMousePosY, CrossHairMode, CrossHairType, direction, ok,
      FirstRightButtonPressed, FirstRightButtonDepressed, OldX2, OldY2, NetNrCheck, ZoomActivated, PanActivated,
      MousePosXR, MousePosYR, RightButtonDivTime;

int32 LastDirection = -1;
int32 SpecialLayer = 1;
int32 CrossHairNewStart = 1;
int32 FirstStartTimer2 = 1;
int32 MaxDisplayDiv = 5;
#ifdef _DEBUG
int32 DrawCrossHairCount;
#endif

char FileName[MAX_LENGTH_STRING];
double CrossHairX = 100000.0, CrossHairY;

extern int32 GatePinSwapMode, DebugPaint, PCBWindowKilled, SpecialDebugFile;
extern char GatePinSwapReference[64], GatePinSwapPinName[64];
extern int TimerValue;
extern int32 PaintIntro, FirstButtonPressed, TotalExit, WaitForPaint;
extern LPTSTR CursorType;
extern HDC OutputDisplay;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ObjectSelection()
{
	int32 mode, FirstMousePosX, FirstMousePosY, MouseDivX, MouseDivY, MultiSelectMode;
	double OldX, OldY, CurrentX, CurrentY, x1, y1;

//  CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
//  CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));
	CurrentX = PixelToRealOffX(MousePosX);
	CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
	FirstMousePosX = MousePosX;
	FirstMousePosY = MousePosY;
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;
	OldX = CurrentX;
	OldY = CurrentY;
	MultiSelectMode = 0;
	MouseDivX = 0;
	MouseDivY = 0;

	SelectionEsc = 0;
	mode = 0;

	if (ReplaceSelections)
		mode = 1;

	OldX = CurrentX;
	OldY = CurrentY;

	DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

	SystemBusyMode = 200;
	ClipMouseCursor();
	SetNormalCursor();

	while ((LeftButtonPressed) && (!SelectionEsc))
	{
		if (ShiftPressed)
			mode = 0;

		CurrentX = PixelToRealOffX(MousePosX);
		CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		MouseDivX = abs(MousePosX - FirstMousePosX);
		MouseDivY = abs(MousePosY - FirstMousePosY);
		
		if ((OldX != CurrentX) || (OldY != CurrentY))
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			OldX = CurrentX;
			OldY = CurrentY;

			if (Units == 0)
			{
				x1 = fabs((CurrentX - CurrentX2) / 2540.0);
				y1 = fabs((CurrentY - CurrentY2) / 2540.0);
				sprintf(InfoStr, "%.2f , %.2f thou", x1, y1);
			}
			else
			{
				x1 = fabs((CurrentX - CurrentX2) / 100000.0);
				y1 = fabs((CurrentY - CurrentY2) / 100000.0);
				sprintf(InfoStr, "%.4f , %.4f mm", x1, y1);
			}

			RedrawInfoStr(1);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosX > DrawWindowMaxX - ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollRight(ScrollSize);
			MousePosX -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosY > DrawWindowMaxY - ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollDown(ScrollSize);
			MousePosY -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosX < DrawWindowMinX + ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollLeft(ScrollSize);
			MousePosX += ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosY < DrawWindowMinY + ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollUp(ScrollSize);
//        NrGraphicsObjects
			MousePosY += ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		DisplayCursorPosition();
		MouseChanged = 0;
		CheckInputMessages(0);

		if (!Focused)
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();
			SelectionEsc = 1;
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("selecting_deselecting_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
				SelectionEsc = 1;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			ClipMouseCursor();

			if (!SelectionEsc)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}
	}

	UnClipMouseCursor();

	if (!SelectionEsc)
		DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

	DrawCrossHair(2);
	SetNormalCursor();

	if ((MouseDivX < 3) && (MouseDivY < 3))
		MultiSelectMode = 2;

	if (!SelectionEsc)
	{
		if (SelectionMode != ROUTING_MODE)
		{
			if (mode == 1)
			{
				UnselectAll = 1;
				SelectObjectsFromWindow(CurrentX2, CurrentY2, CurrentX, CurrentY, 0);
				UnselectAll = 0;
			}

			SelectObjectsFromWindow(CurrentX2, CurrentY2, CurrentX, CurrentY, MultiSelectMode);
		}
		else
		{
			if (!AddExtraTraceMode)
				SelectObjectOnPointer(CurrentX2, CurrentY2, CurrentX, CurrentY, 0);
			else
				SelectObjectOnPointer(CurrentX2, CurrentY2, CurrentX, CurrentY, 1);

		}
	}

	SelectionEsc = 0;
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetRectWindow()
{
	int32 mode, FirstMousePosX, FirstMousePosY, MouseDivX, MouseDivY;
	double OldX, OldY, CurrentX, CurrentY, x1, y1;

//  CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
//  CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));

	SelectionEsc = 0;
	mode = 0;
	CurrentX = PixelToRealOffX(MousePosX);
	CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
	FirstMousePosX = MousePosX;
	FirstMousePosY = MousePosY;
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;
	OldX = CurrentX;
	OldY = CurrentY;

	SystemBusyMode = 300;
	ClipMouseCursor();
	SetNormalCursor();

	while ((((mode == 1) && (LeftButtonPressed)) || ((mode == 0) && (!LeftButtonPressed))) && (!SelectionEsc))
	{
		CurrentX = PixelToRealOffX(MousePosX);
		CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		MouseDivX = abs(MousePosX - FirstMousePosX);
		MouseDivY = abs(MousePosY - FirstMousePosY);

//    CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
//    CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));
		if ((OldX != CurrentX) || (OldY != CurrentY))
		{
			DrawCrossHair(0);

			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

			OldX = CurrentX;
			OldY = CurrentY;

			if (Units == 0)
			{
				x1 = fabs((CurrentX - CurrentX2) / 2540.0);
				y1 = fabs((CurrentY - CurrentY2) / 2540.0);
				sprintf(InfoStr, "%.2f , %.2f thou", x1, y1);
			}
			else
			{
				x1 = fabs((CurrentX - CurrentX2) / 100000.0);
				y1 = fabs((CurrentY - CurrentY2) / 100000.0);
				sprintf(InfoStr, "%.4f , %.4f mm", x1, y1);
			}

			RedrawInfoStr(1);

			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosX > DrawWindowMaxX - ScrollEndOfWindow))
		{
			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

			SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			ScrollRight(ScrollSize);
			MousePosX -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosY > DrawWindowMaxY - ScrollEndOfWindow))
		{
			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

			SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
			ScrollDown(ScrollSize);
			MousePosY -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosX < DrawWindowMinX + ScrollEndOfWindow))
		{
			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

			SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			ScrollLeft(ScrollSize);
			MousePosX += ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosY < DrawWindowMinY + ScrollEndOfWindow))
		{
			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

			SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
			ScrollUp(ScrollSize);
//        NrGraphicsObjects
			MousePosY += ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		DisplayCursorPosition();
		MouseChanged = 0;
		CheckInputMessages(0);

		if (!Focused)
		{
			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();
			SelectionEsc = 1;
		}

		if (LeftButtonPressed)
		{
			if (mode == 0)
			{
				mode = 1;
				CurrentX = PixelToRealOffX(MousePosX);
				CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
				CurrentX2 = CurrentX;
				CurrentY2 = CurrentY;
				OldX = CurrentX;
				OldY = CurrentY;
			}
		}

		if (NrFunctionsInBuf > 0)
		{
			if (mode == 1)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("stretch_areafill.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
//        SelectionEsc=1;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			ClipMouseCursor();

			if (!SelectionEsc)
			{
				if (mode == 1)
					DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			}
		}
	}

	UnClipMouseCursor();
	SetNormalCursor();

	if (!SelectionEsc)
	{
		if (mode == 1)
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
	}

	DrawCrossHair(2);

	SystemBusyMode = 0;

	if (!SelectionEsc)
	{
		SearchMinX = min(CurrentX, CurrentX2);
		SearchMinY = min(CurrentY, CurrentY2);
		SearchMaxX = max(CurrentX, CurrentX2);
		SearchMaxY = max(CurrentY, CurrentY2);
		return 1;
	}

	SelectionEsc = 0;
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 ZoomActive()
{
	if (!ZoomActivated)
	{
		if ((MiddleButtonPressed) || ((CtrlPressed) && (LeftButtonPressed)))
		{
			ZoomActivated = 1;
			return 1;
		}
	}
	else
	{
		if ((MiddleButtonPressed) || (LeftButtonPressed))
			return 1;
	}

	ZoomActivated = 0;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PanActive()
{
	if (!PanActivated)
	{
		if ((CtrlPressed) && ((MiddleButtonPressed) || (RightButtonPressed)))
		{
			PanActivated = 1;
			return 1;
		}
	}
	else
	{
		if ((MiddleButtonPressed) || (RightButtonPressed))
			return 1;
	}

	PanActivated = 0;
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void CheckMouseOutOfWindow(int32 mode)
{
//  if (SelectionMode!=1) return ;
	if (!ShiftPressed)
		return;

	if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
	{
		SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
		ScrollRight(ScrollSize);
		MousePosX -= ScrollSizeDrawing;
	}

	if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
	{
		SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
		ScrollDown(ScrollSize);
		MousePosY -= ScrollSizeDrawing;
	}

	if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
	{
		SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
		ScrollLeft(ScrollSize);
		MousePosX += ScrollSizeDrawing;
	}

	if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
	{
		SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
		ScrollUp(ScrollSize);
		MousePosY += ScrollSizeDrawing;
	}
}

//*******************************************************************************************************
//*********************************** spodní pravý pøeklad **********************************************
//*******************************************************************************************************

void SetInfoStr(int32 mode)
{
	char str[MAX_LENGTH_STRING];


	switch (SelectionMode)
	{
	case ROUTING_MODE:
	case MOVE_ONE_TRACE_MODE:
	case MOVING_TRACES_VIAS_MODE:
		switch (CurrentDrawingLayer)
		{
		case -1:
			switch (SelectionMode)
			{
			case ROUTING_MODE:
				sprintf(InfoStr, SC(138, "Routing traces"));
				break;

			case MOVE_ONE_TRACE_MODE:
				sprintf(InfoStr, SC(135, "Drag traces"));
				break;

			case MOVING_TRACES_VIAS_MODE:
				sprintf(InfoStr, SC(136, "Change traces/vias"));
				break;
			}

			break;

		default:
			GetLayerText(CurrentDrawingLayer, str, 3);

			switch (SelectionMode)
			{
			case ROUTING_MODE:
				sprintf(InfoStr, SC(343, "Routing traces %s"), str);
				break;

			case MOVE_ONE_TRACE_MODE:
				sprintf(InfoStr, SC(344, "Drag traces %s"), str);
				break;

			case MOVING_TRACES_VIAS_MODE:
				sprintf(InfoStr, SC(345, "Change traces/vias %s"), str);
				break;
			}

			break;
		}

		break;

	case MOVE_COMPONENTS_MODE:
		strcpy(InfoStr, SC(134, "Move/rotate/change components"));
		break;

	case MOVE_COMPONENT_REFERENCES_MODE:
		strcpy(InfoStr, SC(241, "Modify component references"));
		break;

	case MOVE_COMPONENT_VALUES_MODE:
		strcpy(InfoStr, SC(268, "Modify component values"));
		break;

	case OBJECTS_MODE:
		strcpy(InfoStr, SC(228, "Draw/change objects other layers"));
		break;

	case AREAFILLS_MODE:
		strcpy(InfoStr, SC(176, "Areafills/powerplanes"));
		break;

	case GATE_PINSWAP_MODE:
		strcpy(InfoStr, SC(292, "Gate/pin swap"));
		break;

	case DRAG_TRACES_VIAS_COMPS_MODE: //pùvodní ID_ACTION_DRAG_COMP_TRACE_VIAS

		strcpy(InfoStr, SC(269, "Drag traces/vias/components"));
		break;
	}

	RedrawInfoStr(1);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawCrossHair(int32 mode)
{
	int32 PreviousROP;
	double CurrentX, CurrentY;
#ifdef _DEBUG
	char str[200];
#endif


	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if ((mode & 32) == 32)
		WaitForPaint = 1;
	else
	{
		if (WaitForPaint)
			return;
	}

	switch (mode & 7)
	{
	case 7:
		CrossHairNewStart = 1;
		break;
	}

	if (((mode & 16) == 0) && (OkToDrawCrossHair == 0))
	{
		CrossHairX = CurrentX;
		CrossHairY = CurrentY;
		return;
	}

	if ((mode & 8) == 0)
		StartDrawingEditingWindow();

	InitDrawingObject(0, CROSS_HAIR_LAYER, 1, DRAW_WITH_DASH_PEN_AND_NO_BRUSH);
	PreviousROP = GetROP2(OutputDisplay);
	SetROP2(OutputDisplay, R2_XORPEN);

	if (MousePosX != 10000)
	{
		switch (mode & 7)
		{
		case 0:
			if ((MousePosX > DrawWindowMinX) && ((CrossHairX != CurrentX) || (CrossHairY != CurrentY)))
			{
				if (!CrossHairNewStart)
				{
#ifdef _DEBUG
					sprintf(str, "(%04d) Draw old cross hair\n", DrawCrossHairCount);
					OutputDebugString(str);
					DrawCrossHairCount++;
#endif

					if (CrossHairType == 0)
					{
						DrawLine(MultX(CrossHairX), -100000, MultX(CrossHairX), 100000);
						DrawLine(-100000, MultY(CrossHairY), 100000, MultY(CrossHairY));
					}
					else
					{
						DrawLine(MultX(CrossHairX) + 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) - 100000,
						         MultY(CrossHairY) - 100000);
						DrawLine(MultX(CrossHairX) - 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) + 100000,
						         MultY(CrossHairY) - 100000);
					}
				}

				CrossHairNewStart = 0;
				CrossHairX = CurrentX;
				CrossHairY = CurrentY;
#ifdef _DEBUG
				sprintf(str, "(%04d) Draw new cross hair\n", DrawCrossHairCount);
				OutputDebugString(str);
				DrawCrossHairCount++;
#endif

				if (CrossHairType == 0)
				{
					DrawLine(MultX(CrossHairX), -100000, MultX(CrossHairX), 100000);
					DrawLine(-100000, MultY(CrossHairY), 100000, MultY(CrossHairY));
				}
				else
				{
					DrawLine(MultX(CrossHairX) + 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) - 100000,
					         MultY(CrossHairY) - 100000);
					DrawLine(MultX(CrossHairX) - 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) + 100000,
					         MultY(CrossHairY) - 100000);
				}
			}

			break;

		case 2:
			if (CrossHairNewStart)
				break;

			if (CrossHairType == 0)
			{
				DrawLine(MultX(CrossHairX), -10000, MultX(CrossHairX), 10000);
				DrawLine(-10000, MultY(CrossHairY), 10000, MultY(CrossHairY));
			}
			else
			{
				DrawLine(MultX(CrossHairX) + 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) - 100000,
				         MultY(CrossHairY) - 100000);
				DrawLine(MultX(CrossHairX) - 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) + 100000,
				         MultY(CrossHairY) - 100000);
			}

			CrossHairNewStart = 1;
			break;
		}
	}

	SetROP2(OutputDisplay, PreviousROP);

	if ((mode & 8) == 0)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

void DrawCrossHair2(int32 mode)
{
	double CurrentX, CurrentY;

	if ((mode & 32) == 32)
		WaitForPaint = 1;
	else
	{
		if (WaitForPaint)
			return;
	}

	switch (mode & 7)
	{
	case 6:
		if ((MousePosX != 10000) && (MousePosX > DrawWindowMinX))
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			CrossHairX = CurrentX;
			CrossHairY = CurrentY;
		}

		break;

	case 7:
		CrossHairX = 100000.0;
		CrossHairY = 0.0;
		break;
	}

	if ((CrossHairMode == 0) && (OkToDrawCrossHair == 0))
		return;

	if ((mode & 8) == 0)
		StartDrawingEditingWindow();

	InitDrawingObject(0, CROSS_HAIR_LAYER, 1, DRAW_WITH_DASH_PEN_AND_NO_BRUSH);
	SetROP2(OutputDisplay, R2_XORPEN);

	if ((mode & 7) == 2)
	{
		if (CrossHairType == 0)
		{
			DrawLine(MultX(CrossHairX), -10000, MultX(CrossHairX), 10000);
			DrawLine(-10000, MultY(CrossHairY), 10000, MultY(CrossHairY));
		}
		else
		{
			DrawLine(MultX(CrossHairX) + 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) - 100000,
			         MultY(CrossHairY) - 100000);
			DrawLine(MultX(CrossHairX) - 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) + 100000,
			         MultY(CrossHairY) - 100000);
		}

		if ((mode & 8) == 0)
		{
			ExitDrawing();
			EndDrawingEditingWindow();
		}

		return;
	}

	if ((MousePosX != 10000) && (MousePosX > DrawWindowMinX))
	{
		CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
		CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

		switch (mode & 7)
		{
		case 0:
			if ((CrossHairX != CurrentX) || (CrossHairY != CurrentY))
			{
				if (NotInRange(CrossHairX, 100000.0))
				{
					if (CrossHairType == 0)
					{
						DrawLine(MultX(CrossHairX), -100000, MultX(CrossHairX), 100000);
						DrawLine(-100000, MultY(CrossHairY), 100000, MultY(CrossHairY));
					}
					else
					{
						DrawLine(MultX(CrossHairX) + 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) - 100000,
						         MultY(CrossHairY) - 100000);
						DrawLine(MultX(CrossHairX) - 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) + 100000,
						         MultY(CrossHairY) - 100000);
					}
				}

				CrossHairX = CurrentX;
				CrossHairY = CurrentY;

				if (CrossHairType == 0)
				{
					DrawLine(MultX(CrossHairX), -100000, MultX(CrossHairX), 100000);
					DrawLine(-100000, MultY(CrossHairY), 100000, MultY(CrossHairY));
				}
				else
				{
					DrawLine(MultX(CrossHairX) + 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) - 100000,
					         MultY(CrossHairY) - 100000);
					DrawLine(MultX(CrossHairX) - 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) + 100000,
					         MultY(CrossHairY) - 100000);
				}
			}

			break;

		case 1:
			CrossHairX = CurrentX;
			CrossHairY = CurrentY;

			if (CrossHairType == 0)
			{
				DrawLine(MultX(CrossHairX), -100000, MultX(CrossHairX), 100000);
				DrawLine(-100000, MultY(CrossHairY), 100000, MultY(CrossHairY));
			}
			else
			{
				DrawLine(MultX(CrossHairX) + 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) - 100000,
				         MultY(CrossHairY) - 100000);
				DrawLine(MultX(CrossHairX) - 100000, MultY(CrossHairY) + 100000, MultX(CrossHairX) + 100000,
				         MultY(CrossHairY) - 100000);
			}

			break;

		case 3:
			CrossHairX = CurrentX;
			CrossHairY = CurrentY;
			break;

		case 4:
			CrossHairX = -10000000000.0;
			CrossHairY = -10000000000.0;
			break;

		case 5:
			CrossHairX = 100000.0;
			CrossHairY = 0.0;
			break;
		}
	}

	if ((mode & 8) == 0)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawSpecialXorFunction(DrawXorFunctionRecord * DrawXorFunction, int32 mode)
{
	double p1d, p2d, p3d, p4d, p5d, p6d, p7d;
	int32 p1, p2, p3, p4, p8;

	if (!DrawXorFunction)
		return 0;

	if (mode == 0)
	{
		switch (DrawXorFunction->Mode)
		{
		case 0:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			p3 = *(int32 *) DrawXorFunction->Param1[2];
			DrawXorFunction->Function1(p1d, p2d, p3);
			break;

		case 1:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			p3d = *(double *) DrawXorFunction->Param1[2];
			DrawXorFunction->Function2(p1d, p2d, p3d);
			break;

		case 2:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2 = *(int32 *) DrawXorFunction->Param1[1];
			DrawXorFunction->Function3(p1d, p2);
			break;

		case 3:
			p1 = *(int32 *) DrawXorFunction->Param1[0];
			DrawXorFunction->Function4a(p1);
			break;

		case 4:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			p3 = *(int32 *) DrawXorFunction->Param1[2];
			p4 = *(int32 *) DrawXorFunction->Param1[3];
			DrawXorFunction->Function5(p1d, p2d, p3, p4);
			break;

		case 5:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			p3d = *(double *) DrawXorFunction->Param1[2];
			p4d = *(double *) DrawXorFunction->Param1[3];
			p5d = *(double *) DrawXorFunction->Param1[4];
			p6d = *(double *) DrawXorFunction->Param1[5];
			p7d = *(double *) DrawXorFunction->Param1[6];
			p8 = *(int32 *) DrawXorFunction->Param1[7];
			DrawXorFunction->Function6(p1d, p2d, p3d, p4d, p5d, p6d, p7d, p8);
			break;

		case 6:
			p1 = *(int32 *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			p3d = *(double *) DrawXorFunction->Param1[2];
			p4 = *(int32 *) DrawXorFunction->Param1[3];
			DrawXorFunction->Function7(p1, p2d, p3d, p4);
			break;

		case 7:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			p3d = *(double *) DrawXorFunction->Param1[2];
			p4 = *(int32 *) DrawXorFunction->Param1[3];
			DrawXorFunction->Function8(p1d, p2d, p3d, p4);
			break;

		case 8:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			DrawXorFunction->Function9(p1d, p2d);
			break;
		}
	}

	if (mode == 1)
	{
		switch (DrawXorFunction->Mode)
		{
		case 0:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3 = *(int32 *) DrawXorFunction->Param2[2];
			DrawXorFunction->Function1(p1d, p2d, p3);
			break;

		case 1:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3d = *(double *) DrawXorFunction->Param2[2];
			DrawXorFunction->Function2(p1d, p2d, p3d);
			break;

		case 2:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2 = *(int32 *) DrawXorFunction->Param2[1];
			DrawXorFunction->Function3(p1d, p2);
			break;

		case 3:
			p1 = *(int32 *) DrawXorFunction->Param2[0];
			DrawXorFunction->Function4b(p1);
			break;

		case 4:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3 = *(int32 *) DrawXorFunction->Param2[2];
			p4 = *(int32 *) DrawXorFunction->Param2[3];
			DrawXorFunction->Function5(p1d, p2d, p3, p4);
			break;

		case 5:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3d = *(double *) DrawXorFunction->Param2[2];
			p4d = *(double *) DrawXorFunction->Param2[3];
			p5d = *(double *) DrawXorFunction->Param2[4];
			p6d = *(double *) DrawXorFunction->Param2[5];
			p7d = *(double *) DrawXorFunction->Param2[6];
			p8 = *(int32 *) DrawXorFunction->Param2[7];
			DrawXorFunction->Function6(p1d, p2d, p3d, p4d, p5d, p6d, p7d, p8);
			break;

		case 6:
			p1 = *(int32 *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3d = *(double *) DrawXorFunction->Param2[2];
			p4 = *(int32 *) DrawXorFunction->Param2[3];
			DrawXorFunction->Function7(p1, p2d, p3d, p4);
			break;

		case 7:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3d = *(double *) DrawXorFunction->Param2[2];
			p4 = *(int32 *) DrawXorFunction->Param2[3];
			DrawXorFunction->Function8(p1d, p2d, p3d, p4);
			break;

		case 8:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			DrawXorFunction->Function9(p1d, p2d);
			break;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckLeftButton(void)
{
	static int32 press, unpress;

	if (LeftButtonPressed)
	{
		unpress = 0;

		if (press == 0)
		{
			press = 1;
			return 1;
		}
	}
	else
	{
		press = 0;

		if (unpress == 0)
			unpress = 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckRightButton2(DrawXorFunctionRecord * DrawXorFunction)
{
	int32 DivX, DivY;
//  char  str[200],str2[200];

	if (RightButtonPressed)
	{
		FirstStartTimer2 = 0;
		FirstRightButtonDepressed = 0;

		if (FirstRightButtonPressed == 0)
		{
			/*
			      GetTimeString(1,str2);
			      sprintf(str,"%s | FirstRightButtonPressed\n",str2);
			      OutputDebugString(str);
			*/
			FirstRightButtonPressed = 1;
			SetTimer2();
			MousePosXR = MousePosX;
			MousePosYR = MousePosY;
		}
		else
		{
			if (MousePosX != 10000)
			{
				DivX = MousePosX - MousePosXR;
				DivY = MousePosY - MousePosYR;

				switch (MousePanMultiply)
				{
				case 0:
					break;

				case 1:
					DivX = (DivX * 3) / 2;
					DivY = (DivY * 3) / 2;
					break;

				case 2:
					DivX = DivX * 2;
					DivY = DivY * 2;
					break;

				case 3:
					DivX = DivX * 3;
					DivY = DivY * 3;
					break;

				case 4:
					DivX = DivX * 4;
					DivY = DivY * 4;
					break;

				case 5:
					DivX = DivX * 5;
					DivY = DivY * 5;
					break;

				case 6:
					DivX = DivX * 6;
					DivY = DivY * 6;
					break;
				}

				if ((abs(DivX) > MaxDisplayDiv) || (abs(DivY) > MaxDisplayDiv))
				{
					OkToAddViewPos = 0;
					DrawSpecialXorFunction(DrawXorFunction, 0);
					ScrollAppWindow(DivX, DivY);
					DrawSpecialXorFunction(DrawXorFunction, 1);
					MousePosXR = MousePosX;
					MousePosYR = MousePosY;
				}
			}
		}

//    MenuPopUp();
		CheckInputMessages(0);
	}
	else
	{
		if (FirstStartTimer2)
			return 0;

		if (FirstRightButtonDepressed == 0)
		{
			/*
			      GetTimeString(1,str2);
			      sprintf(str,"%s | FirstRightButtonDepressed\n",str2);
			      OutputDebugString(str);
			*/
			FirstRightButtonDepressed = 1;
			RightButtonDivTime = GetDifferenceTimer2inMilliSeconds();

			if (RightButtonDivTime < 300)
			{
				FirstRightButtonPressed = 0;
				return 1;
			}
			else
			{
				SaveViewPos();
			}
		}

		FirstRightButtonPressed = 0;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void MainLoop()
{
	DrawCrossHair(0);

	if (PanActive())
	{
		DrawMouseInfoOff(0);
		DisplayInfoOff(0);
		PanWindow();
	}

	if (!CtrlPressed)
	{
		OldMousePosX = MousePosX;
		OldMousePosY = MousePosY;

		if (MousePosX < DrawWindowMinX)
		{
			if (Focused)
				ButtonInfo();
		}
		else
		{
			DrawMouseInfoOff(2);
			DisplayInfo(0);
		}
	}

	if (ZoomActive())
	{
		DrawMouseInfoOff(0);
		DisplayInfoOff(0);
		ZoomWindow();
	}

	if (CheckLeftButton())
	{
		DrawMouseInfoOff(0);
		DisplayInfoOff(0);

		if (MousePosX < DrawWindowMinX)
		{
			CheckButtonPressed(0);

			while ((LeftButtonPressed) && (Focused))
				CheckInputMessages(0);

			//LeftButtonPressed = 0;
			CheckButtonPressed(1);
		}
		else
		{
			ObjectSelection();

			if ((RepeatMode == 0) && (LastAction != MOVE_ONE_TRACE_MODE))
				LastAction = 0;

			if (LastAction > 0)
			{
				switch (LastAction)
				{
				case MOVE_ONE_TRACE_MODE:
					MoveOneTrace(0);
					break;

				case MOVE_COMPONENTS_MODE:
					RepeatModeBusy = 1;
					if (GetNrCompsSelected() > 0)
					{
						DrawCrossHair(1);

						if (ComponentConnectionMode == 0)
							MoveComponents(3);
						else
							MoveComponents(1);
					}

					RepeatModeBusy = 0;
					break;

				case MOVE_COMPONENT_REFERENCES_MODE:
					RepeatModeBusy = 1;
					if (GetNrReferencesSelected() == 1)
						MoveSelectedRefs(0, 0);

					RepeatModeBusy = 0;
					break;

				case MOVE_COMPONENT_VALUES_MODE:
					RepeatModeBusy = 1;
					if (GetNrCompValuesSelected() == 1)
						MoveSelectedCompValues(0, 0);

					RepeatModeBusy = 0;
					break;

				case OBJECTS_MODE:
					RepeatModeBusy = 1;
					MoveSelectedSpecialObjects(0, 1);
					RepeatModeBusy = 0;
					break;
				}
			}
		}
	}

	FirstButtonPressed = 0;

	if (MouseChanged)
	{
		if (MousePosX >= DrawWindowMinX)
			DisplayCursorPosition();

		CheckMouseOutOfWindow(0);
		MouseChanged = 0;
	}

	if (CheckRightButton2(NULL) == 1)
		MenuPopUp();

	/*
	  if (RightButtonPressed) {
	    DrawCrossHair(2);
	    DrawMouseInfoOff(0);
	    DisplayInfoOff(0);
	    MenuPopUp();
	    RightButtonPressed=0;
	    CheckInputMessages(0);
	  }
	*/
	if (LeftButtonDoublePressed)
	{
		switch (SelectionMode)
		{
		case OBJECTS_MODE:
			ChangeText(0);
			break;
		}

		LeftButtonDoublePressed = 0;
	}

	if (NrFunctionsInBuf > 0)
	{
		if (SystemBusyMode == 0)
			DrawCrossHair(1);

		DrawMouseInfoOff(0);
		DisplayInfoOff(0);
		ExecuteKeys();
		CheckInputMessages(0);

//    CheckInputMessages(0);
		if (HelpAsked)
		{
			switch (SelectionMode)
			{
			case DRAG_TRACES_VIAS_COMPS_MODE:
				Help("dragging_traces_vias_components.htm", 0);
				break;

			case ROUTING_MODE:
				Help("traces_vias.htm", 0);
				break;

			case MOVE_ONE_TRACE_MODE:
				Help("drag_one_trace.htm", 0);
				break;

			case MOVE_COMPONENTS_MODE:
				Help("components.htm", 0);
				break;

			case MOVE_COMPONENT_REFERENCES_MODE:
				Help("moving_component_references.htm", 0);
				break;

			case MOVE_COMPONENT_VALUES_MODE:
				Help("moving_component_values.htm", 0);
				break;

			case OBJECTS_MODE:
				Help("special_objects.htm", 0);
				break;

			case AREAFILLS_MODE:
				Help("areafills.htm", 0);
				break;

			case MOVING_TRACES_VIAS_MODE:
				Help("change_traces_vias.htm", 0);
				break;

			case GATE_PINSWAP_MODE:
				Help("gate_pin_swap.htm", 0);
				break;
			}

			CheckInputMessages(0);

			while (!Focused)
			{
				CheckInputMessages(0);	// PCBWindow
			}

			CheckInputMessages(0);

			HelpAsked = 0;
		}

		if (SelectionEsc)
			SelectionEsc = 0;

		if (SystemBusyMode == 0)
			DrawCrossHair(1);
	}

	if (DataBaseChanged)
	{
//    UpdateInsDelInfo();
		SetWindowName(1);
		FileChanged = 1;
		DataBaseChanged = 0;

		if (!UndoRedoActive)
			LastActionNr++;

		UndoRedoActive = 0;
	}
}

//*******************************************************************************************************
//***************************** dolní zobrazení pozice **************************************************
//*******************************************************************************************************

void DisplayCursorPosition()
{
	double x, y, x1, y1, x2, y2, x3, y3, x4, y4, x5, x6, y6, Angle, Length;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	if ((MousePosX == 10000) || (MousePosY == 10000) || (MousePosX >= DrawWindowMaxX) || (MousePosY >= DrawWindowMaxY))
		return;

	x = (PixelToRealOffX(MousePosX));
	y = (PixelToRealOffY(DrawWindowMaxY - MousePosY));
	x1 = x;
	y1 = y;
	x4 = x / 100000.0;
	y4 = y / 100000.0;
	x6 = x / 2540.0;
	y6 = y / 2540.0;
	x = AdjustToDrawGrid(x);
	y = AdjustToDrawGrid(y);

	switch (Units)
	{
	case 0:
		x2 = x / 2540.0;
		y2 = y / 2540.0;

		if (MouseCursorOnGrid)
		{
			x3 = (x - RelX) / 2540.0;
			x5 = x - RelX;
			y3 = (y - RelY) / 2540.0;
		}
		else
		{
			x3 = (x1 - RelX) / 2540.0;
			x5 = x1 - RelX;
			y3 = (y1 - RelY) / 2540.0;
		}
		if (NotInRange(x5, 0.0))
		{
			Angle = atan(y3 / x3) * 180 / PI;

			if (Angle < 0.0)
				Angle += 180.0;

			if (y < RelY)
				Angle += 180.0;
		}
		else
		{
			if (y > RelY)
				Angle = 90.0;
			else
				Angle = 270.0;
		}

		Length = sqrt(SQR(x3) + SQR(y3));

		sprintf(str, "absolute x,y %.2f , %.2f thou", x6, y6);

		sprintf(str2, "grid x,y %.2f , %.2f thou", x2, y2);

		sprintf(str3, "relative x,y %.2f , %.2f thou ( %.2f, %.1f )", x3, y3, Length, Angle);

		break;

	case 1:
		x2 = x / 100000.0;
		y2 = y / 100000.0;

		if (MouseCursorOnGrid)
		{
			x3 = (x - RelX) / 100000.0;
			x5 = x - RelX;
			y3 = (y - RelY) / 100000.0;
			
		}
		else
		{
			x3 = (x1 - RelX) / 100000.0;
			x5 = x1 - RelX;
			y3 = (y1 - RelY) / 100000.0;
			
		}
		if (NotInRange(x5, 0.0))
		{
			Angle = atan(y3 / x3) * 180 / PI;

			if (Angle < 0.0)
				Angle += 180.0;

			if (y < RelY)
				Angle += 180.0;
		}
		else
		{
			if (y > RelY)
				Angle = 90.0;
			else
				Angle = 270.0;
		}
		
		Length = sqrt(SQR(x3) + SQR(y3));

		sprintf(str, SC(333, "absolute x,y %.4f , %.4f mm"), x4, y4);

		sprintf(str2, SC(334, "grid x,y %.4f , %.4f mm"), x2, y2);

		sprintf(str3, SC(335, "relative x,y %.4f , %.4f mm ( %.4f , %.1f )"), x3, y3, Length, Angle);

		break;
	}

	strcpy(AbsPosStr, str);
	RedrawAbsPosStr(1);

	if (DebugPaint)
		sprintf(str2, "%i , %i", MousePosX, MousePosY);

	strcpy(AbsGridPosStr, str2);
	RedrawAbsGridPosStr(1);

	strcpy(RelPosStr, str3);
	RedrawRelPosStr(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckInputMessages(int32 DelayInMilleSeconds)
{
	MSG M;
	int32 ok, ExitTimerValue;

	if (DelayInMilleSeconds == 0)
	{
		if (GetMessage(&M, 0, 0, 0))
		{
			TranslateMessage(&M);
			DispatchMessage(&M);
		}
		else
		{
			SelectionEsc = 1;
			AltPressed = 0;
			CtrlPressed = 0;
			ShiftPressed = 0;
			//RightButtonPressed = 0;
			LeftButtonPressed = 0;
			TotalExit = 1;
			ok = 1;
		}

		return;
	}

	ExitTimerValue = TimerValue + DelayInMilleSeconds / 100;

	while (TimerValue < ExitTimerValue)
	{
		if (GetMessage(&M, 0, 0, 0))
		{
			TranslateMessage(&M);
			DispatchMessage(&M);
		}
		else
		{
			SelectionEsc = 1;
			AltPressed = 0;
			CtrlPressed = 0;
			ShiftPressed = 0;
			RightButtonPressed = 0;
			LeftButtonPressed = 0;
			TotalExit = 1;
			ok = 1;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckInputMessages2(void)
{
	MSG M;
	int32 Key;

	while (PeekMessage(&M, 0, 0, 0, PM_REMOVE))
	{
		switch (M.message)
		{
		case WM_RBUTTONDBLCLK:
		case WM_RBUTTONDOWN:
		case WM_RBUTTONUP:
		case WM_LBUTTONDBLCLK:
		case WM_LBUTTONDOWN:
		case WM_LBUTTONUP:
		case WM_MBUTTONDBLCLK:
		case WM_MBUTTONDOWN:
		case WM_MBUTTONUP:
		case WM_PAINT:
		case WM_TIMER:
		case WM_CHAR:
		case WM_COMMAND:
		case WM_KEYDOWN:
		case WM_SYSKEYDOWN:
		case WM_KEYUP:
		case WM_SYSKEYUP:
		case WM_MOUSEMOVE:
			TranslateMessage(&M);
			DispatchMessage(&M);

			if (NrFunctionsInBuf > 0)
			{
				if ((Key = ReadKeyFunction()) == Key_Esc)
					SelectionEsc = 1;
			}

			break;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckInputMessages3(void)
{
	MSG M;

	return (PeekMessage(&M, 0, 0, 0, PM_NOREMOVE));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckForEscape()
{
	if (CheckInputMessages3())
	{
		CheckInputMessages(0);

		if (NrFunctionsInBuf > 0)
			ExecuteKeys();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ZoomIn(int32 mode)
{
	RECT Rect;
	int32 MouseX, MouseY;
	double cx, cy;

	DrawCrossHair(7);

	if (MousePosY >= DrawWindowMaxY)
		mode &= ~1;

	if (((mode & 1) == 1) && (MousePosX != 10000) && (MousePosY != 10000))
	{
		MouseX = MousePosX + 1;
		MouseY = MousePosY - 1;
	}
	else
	{
		MouseX = (DrawWindowMaxX + DrawWindowMinX + 2) / 2;
		MouseY = (DrawWindowMaxY + DrawWindowMinY - 2) / 2;
	}

	if (Factor < 10.0)
	{
		cx = PixelToRealOffX(MouseX);
		cy = PixelToRealOffY(DrawWindowMaxY - MouseY - 1);

		if ((mode & 2) == 0)
			Factor = (Factor / 2) * 3;
		else
			Factor = (Factor / 6) * 8;

		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;

//      Xoffset=cx-PixelToReal(MouseX);
//      Yoffset=cy-PixelToReal(DrawWindowMaxY-MouseY-1);

		DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
		DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Rect.left = (int32) DrawWindowMinX;
		Rect.right = (int32) DrawWindowMaxX;
		Rect.top = (int32) DrawWindowMinY;
		Rect.bottom = (int32) DrawWindowMaxY;
		DisplayCursorPosition();
		InvalidateRect(PCBWindow, &Rect, 0);
		UpdateWindow(PCBWindow);

//    DrawCrossHair(1);
		if ((mode & 2) == 0)
			SetCursorPos(DrawWindowMinX + ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
	}

	ZoomInOutProcessed = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ZoomOut(int32 mode)
{
	RECT Rect;
	int32 MouseX, MouseY;
	double cx, cy;

	DrawCrossHair(7);

	if (MousePosY >= DrawWindowMaxY)
		mode &= ~1;

	if (((mode & 1) == 1) && (MousePosX != 10000) && (MousePosY != 10000))
	{
//    MouseX=MousePosX;
//    MouseY=MousePosY;
		MouseX = (DrawWindowMaxX + DrawWindowMinX + 2) / 2;
		MouseY = (DrawWindowMaxY + DrawWindowMinY - 2) / 2;
	}
	else
	{
		MouseX = (DrawWindowMaxX + DrawWindowMinX + 2) / 2;
		MouseY = (DrawWindowMaxY + DrawWindowMinY - 2) / 2;
	}

	if (Factor > 0.000001)
	{
		cx = PixelToRealOffX(MouseX);
		cy = PixelToRealOffY(DrawWindowMaxY - MouseY - 1);

		if ((mode & 2) == 0)
			Factor = (Factor / 3) * 2;
		else
			Factor = (Factor / 8) * 6;

		/*
		    if ((Factor>(10/sqrt(2.0)/DefFontSize))
		       &&
		       (Factor<20*sqrt(2.0)/DefFontSize)) {
		      if (Factor>(10*sqrt(2.0)/DefFontSize)) Factor=20/DefFontSize;
		      else Factor=10/DefFontSize;
		    }
		*/

		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;

//    Xoffset=cx-PixelToReal(MouseX);
//    Yoffset=cy-PixelToReal(DrawWindowMaxY-MouseY-1);
		DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
		DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
		Rect.left = (int32) DrawWindowMinX;
		Rect.right = (int32) DrawWindowMaxX;
		Rect.top = (int32) DrawWindowMinY;
		Rect.bottom = (int32) DrawWindowMaxY;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinX - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		DisplayCursorPosition();
		InvalidateRect(PCBWindow, &Rect, 0);
		UpdateWindow(PCBWindow);
//    DrawCrossHair(1);
//    SetCursorPos(DrawWindowMinX+ClientStartX+ClientWindowDivX/2,ClientStartY+ClientWindowDivY/2);
	}

	ZoomInOutProcessed = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ViewFull()
{
	RECT Rect;

	DrawCrossHair(7);
	ViewWholeDesign(0);
	Rect.left = (int32) DrawWindowMinX;
	Rect.right = (int32) DrawWindowMaxX;
	Rect.top = (int32) DrawWindowMinY;
	Rect.bottom = (int32) DrawWindowMaxY;
	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
	DisplayCursorPosition();
	InvalidateRect(PCBWindow, &Rect, 0);
	UpdateWindow(PCBWindow);
//  DrawCrossHair(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PreviousView()
{
	RECT Rect;

	if (ViewPosPointer > 1)
	{
		DrawCrossHair(7);
		Xoffset = ViewPos[ViewPosPointer - 2].Xoffset;
		Yoffset = ViewPos[ViewPosPointer - 2].Yoffset;
		Factor = ViewPos[ViewPosPointer - 2].Factor;
		ViewPosPointer--;
		OkToAddViewPos = 0;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Rect.left = (int32) DrawWindowMinX;
		Rect.right = (int32) DrawWindowMaxX;
		Rect.top = (int32) DrawWindowMinY;
		Rect.bottom = (int32) DrawWindowMaxY;
		RedrawAbsPosStr(1);
		RedrawAbsGridPosStr(1);
		RedrawRelPosStr(1);
		InvalidateRect(PCBWindow, &Rect, 0);
		UpdateWindow(PCBWindow);
//    DrawCrossHair(1);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RePaint()
{
	OkToAddViewPos = 0;
	DrawCrossHair(7);
	InvalidateRect(PCBWindow, NULL, 0);
	UpdateWindow(PCBWindow);
//  CheckInputMessages(100);
//  DrawCrossHair(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CenterScreenOnComp(CompRecord * Comp)
{
	double cx, cy, PixelsX, PixelsY, hulp;
	RECT Rect;


	PixelsX = (DrawWindowMaxX - DrawWindowMinX);
	PixelsY = (DrawWindowMaxY - DrawWindowMinY);
	cx = (Comp->BoardPosMaxX + Comp->BoardPosMinX) / 2;
	cy = (Comp->BoardPosMaxY + Comp->BoardPosMinY) / 2;

	DrawCrossHair(2);

	if (((Comp->BoardPosMaxX - Comp->BoardPosMinX) / PixelsX) > ((Comp->BoardPosMaxY - Comp->BoardPosMinY) / PixelsY))
	{
		hulp = (DrawWindowMaxX - DrawWindowMinX);
		hulp = hulp / (Comp->BoardPosMaxX - Comp->BoardPosMinX);
		hulp = hulp * 0.7;
		Factor = min(0.0005, hulp);
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;
	}
	else
	{
// *****************************************************************************************

		hulp = (DrawWindowMaxY - DrawWindowMinY);
		hulp = hulp / (Comp->BoardPosMaxY - Comp->BoardPosMinY);
		hulp = hulp * 0.7;
		Factor = min(0.0005, hulp);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
	}

	DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
	DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
	Rect.left = (int32) DrawWindowMinX;
	Rect.right = (int32) DrawWindowMaxX;
	Rect.top = (int32) DrawWindowMinY;
	Rect.bottom = (int32) DrawWindowMaxY;
	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
	RedrawAbsPosStr(1);
	RedrawAbsGridPosStr(1);
	RedrawRelPosStr(1);
	InvalidateRect(PCBWindow, &Rect, 0);
	UpdateWindow(PCBWindow);
//  DrawCrossHair(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CenterScreenOnWindow(double WindowMinX, double WindowMinY, double WindowMaxX, double WindowMaxY)
{
	double cx, cy, PixelsX, PixelsY, hulp, OldFactor;
	RECT Rect;

	OldFactor = Factor;
	PixelsX = (DrawWindowMaxX - DrawWindowMinX);
	PixelsY = (DrawWindowMaxY - DrawWindowMinY);
	cx = (WindowMaxX + WindowMinX) / 2;
	cy = (WindowMaxY + WindowMinY) / 2;

	DrawCrossHair(2);

	if (((WindowMaxX - WindowMinX) / PixelsX) > ((WindowMaxY - WindowMinY) / PixelsY))
	{
		hulp = (DrawWindowMaxX - DrawWindowMinX);
		hulp = hulp / (WindowMaxX - WindowMinX);
		hulp = hulp * 0.7;
		Factor = min(0.0005, hulp);

		if (Factor > OldFactor)
			Factor = OldFactor;

		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;
	}
	else
	{
// *****************************************************************************************

		hulp = (DrawWindowMaxY - DrawWindowMinY);
		hulp = hulp / (WindowMaxY - WindowMinY);
		hulp = hulp * 0.7;
		Factor = min(0.0005, hulp);

		if (Factor > OldFactor)
			Factor = OldFactor;

		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
	}

	DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
	DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
	Rect.left = (int32) DrawWindowMinX;
	Rect.right = (int32) DrawWindowMaxX;
	Rect.top = (int32) DrawWindowMinY;
	Rect.bottom = (int32) DrawWindowMaxY;
	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
	RedrawAbsPosStr(1);
	RedrawAbsGridPosStr(1);
	RedrawRelPosStr(1);
	InvalidateRect(PCBWindow, &Rect, 0);
	UpdateWindow(PCBWindow);
//  DrawCrossHair(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CenterScreenOnPoint(double cx, double cy, int32 mode)
{
	double PixelsX, PixelsY, hulp;


	PixelsX = (DrawWindowMaxX - DrawWindowMinX);
	PixelsY = (DrawWindowMaxY - DrawWindowMinY);

	hulp = (DrawWindowMaxX - DrawWindowMinX);
	hulp = hulp / 2000000.0;
	hulp = hulp * 0.7;
	Factor = min(0.001, hulp);
	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
	Yoffset = cy - (ViewMaxY - ViewMinY) / 2;

	DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
	DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);

	/*
	  Rect.left=(int32)DrawWindowMinX;
	  Rect.right=(int32)DrawWindowMaxX;
	  Rect.top=(int32)DrawWindowMinY;
	  Rect.bottom=(int32)DrawWindowMaxY;
	  ViewMinX=PixelToRealOffX(DrawWindowMinX-1);
	  ViewMaxX=PixelToRealOffX(DrawWindowMaxX+1);
	  ViewMinY=PixelToRealOffY(DrawWindowMinY-1);
	  ViewMaxY=PixelToRealOffY(DrawWindowMaxY+1-DrawWindowMinY);
	  RedrawAbsPosStr(1);
	  RedrawAbsGridPosStr(1);
	  RedrawRelPosStr(1);
	  InvalidateRect(PCBWindow,&Rect,0);
	  UpdateWindow(PCBWindow);
	*/
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ViewPan(int32 mode)
{
	double cx, cy;
	int32 MouseX, MouseY;
	RECT Rect;

	DrawCrossHair(2);

	if (MousePosY >= DrawWindowMaxY)
		mode = 0;

	if ((mode == 1) && (MousePosX != 10000) && (MousePosY != 10000))
	{
		MouseX = MousePosX;
		MouseY = MousePosY;
	}
	else
	{
		MouseX = (DrawWindowMaxX + DrawWindowMinX + 10) / 2;
		MouseY = (DrawWindowMaxY + DrawWindowMinY + 10) / 2;
	}

	cx = -PixelToReal(((DrawWindowMaxX + DrawWindowMinX) / 2) - MouseX - DrawWindowMinX);
	cy = PixelToReal(((DrawWindowMaxY + DrawWindowMinY) / 2) - MouseY);
	Xoffset += cx;
	Yoffset += cy;
	Rect.left = (int32) DrawWindowMinX;
	Rect.right = (int32) DrawWindowMaxX;
	Rect.top = (int32) DrawWindowMinY;
	Rect.bottom = (int32) DrawWindowMaxY;
	InvalidateRect(PCBWindow, &Rect, 0);
	UpdateWindow(PCBWindow);
//  DrawCrossHair(1);
	SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ExitProgram()
{
	if (SystemBusyMode == 0)
	{
		SelectionEsc = 1;
		SendMessageOwn(PCBWindow, WM_CLOSE, 0, 0);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ExecuteKeys()
{
	int32 res, Key, KeyFunction;

#ifdef _DEBUG
	char str[MAX_LENGTH_STRING];
	double x1, y1, x4, y4;
	int32 ok;
#endif

	Key = ReadKeyFunction();
	KeyFunction = GetFunctionByTranslatedKey(Key, SelectionMode);
	res = 1;

	switch (Key)
	{
	case Key_Esc:
		SelectionEsc = 1;

		switch (SelectionMode)
		{
		case MOVE_ONE_TRACE_MODE:
			break;

		case MOVE_COMPONENT_REFERENCES_MODE:
		case MOVE_COMPONENT_VALUES_MODE:
		case MOVE_COMPONENTS_MODE:
		case OBJECTS_MODE:
			if (LastAction == 0)
				RepeatMode = 0;

			if (RepeatModeBusy == 0)
				RepeatMode = 0;

			LastAction = 0;
			break;

		case GATE_PINSWAP_MODE:
			if (GatePinSwapMode)
			{
				GatePinSwapReference[0] = 0;
				GatePinSwapPinName[0] = 0;
				PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, (LPARAM) NULL);
				CheckInputMessages(0);
				GatePinSwapMode = 0;
				strcpy(InfoStr, SC(292, "Gate/pin swap"));
				RedrawInfoStr(1);
			}

			RepeatMode = 0;
			break;

		default:
			RepeatMode = 0;
		}

		break;

	case Key_Cursor_Up:
		ScrollUp(ScrollSize);
		break;

	case Key_Cursor_Down:
		ScrollDown(ScrollSize);
		break;

	case Key_Cursor_Left:
		ScrollLeft(ScrollSize);
		break;

	case Key_Cursor_Right:
		ScrollRight(ScrollSize);
		break;

	case Key_Del:
		DeleteObjectsSelected();
		break;
#ifdef _DEBUG

	case Key_Ctrl_H:
		if (CursorType != IDC_ARROW)
			CursorType = IDC_ARROW;
		else
			CursorType = IDC_CROSS;

		SetNormalCursor();
		break;

	case Key_Ctrl_I:
		x1 = (PixelToRealOffX(MousePosX));
		y1 = (PixelToRealOffY(DrawWindowMaxY - MousePosY));
		x4 = (x1 / 100000.0) + 0.05;
		y4 = (y1 / 100000.0) + 0.05;
		/*
		            if ((InRange9(Object->x1,131.4e5))
		               &&
		               (InRange9(Object->y1,91.4e5))) {
		              ok=1;
		            }
		*/
		sprintf(str, "        if ((InRange9(Object->x1,%.1fe5))\r\n", x4);
		OutputDebugString(str);
		OutputDebugString("           &&\r\n");
		sprintf(str, "           (InRange9(Object->y1,%.1fe5))) {\r\n", y4);
		OutputDebugString(str);
		OutputDebugString("          ok=1;\r\n");
		OutputDebugString("        }\r\n");
		break;
#endif

	case Key_F7:
#ifdef _DEBUG
		sprintf(str, "  Factor  = %f;\r\n  Xoffset = %f;\r\n  Yoffset = %f;\r\n", Factor, Xoffset, Yoffset);
		OutputDebugString(str);
		Factor = 0.000185;
		Xoffset = 1456673.750000;
		Yoffset = 7409734.000000;
		RePaint();

#endif
//      DebugPaint=!DebugPaint;
//      PostMessage(PCBWindow,WM_COMMAND,(WPARAM)ID_VIEW_REPAINT,0);
		break;

	case Key_F8:
#ifdef _DEBUG
//      CommandAddObjectTexts2(0.3e5,INFO_LAYER,0);
//      ViewAreafillPoint();
		/*
		      cnt2=0;
		      for (cnt=0;cnt<128;cnt++) {
		        str[cnt2++]=128+cnt;
		        str[cnt2++]=' ';
		        if ((cnt % 16) == 15) {
		          str[cnt2++]='\r';
		          str[cnt2++]='\n';
		        }
		      }
		      str[cnt2++]=0;
		      MessageBoxOwn(PCBWindow,str,SC(1,"Message"),MB_APPLMODAL|MB_OK);
		*/
#endif

//      GetMillingPoints(0);
		break;

	case Key_F9:
#ifdef _DEBUG
		ok = 1;
		AddDots(0);
//      ExportEdif(0);

		/*
		      if (SpecialDebugFile) {
		        ExportToOdb(0);
		      }
		      if (CheckSpecialVersion(0)==1) {
		        ExportOutput(0);
		      }
		*/
//      LoadNewFile2(FileName);
//      LoadGerberFile(FileName,0);
//      ConnectivityTest();
		/*
		      if (ComponentPlacementDialog(0)==1) {
		        CheckConnectivity(1);
		      }
		*/
#endif
//      WriteNeutralFile(0);
		break;

	case (int32) ' ':
		SpacePressed = 1;
		break;

	case Key_Alt_X:
		ExitProgram();
		break;

	default:
		if (KeyFunction == 0)
			MessageBeep((UINT) - 1);

		break;
	}

	if (KeyFunction != 0)
		SendMessageOwn(PCBWindow, WM_COMMAND, (WPARAM) (KeyFunction & 0xffff), (LPARAM) 1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
