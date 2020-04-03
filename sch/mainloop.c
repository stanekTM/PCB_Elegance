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
#include "keyswin.h"
#include "toets.h"
#include "commdlg.h"
#include "sch.h"
#include "calcdef.h"
#include "calc.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "mainloop.h"
#include "files.h"
#include "edit.h"
#include "edit2.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "menus.h"
#include "dialogs.h"
#include "insdel.h"
#include "graphics.h"
#include "movecomp.h"
#include "edit.h"
#include "check.h"
#include "change.h"
#include "command.h"
#include "inscomp.h"
#include "print.h"
#include "resource.h"
#include "help.h"
#include "utf8.h"
#include "property.h"
#include "owntime.h"

POINT MousePos;

int32 MaxCollections, px, py, ro, OldMousePosX, OldMousePosY, MousePosXR, MousePosYR, direction, OldX2, OldY2,
      NetNrCheck, DrawingObjects, DrawingCommand, FirstRightButtonDepressed, FirstRightButtonPressed, CrossHairType,
      ZoomActivated, PanActivated, RightButtonDivTime;
int32 FirstStartTimer2 = 1;
int32 LastDirection = -1;
int32 SpecialLayer = 1;
int32 MaxDisplayDiv = 5;
int32 CrossHairNewStart = 1;

RECT CursorWindow;
double CrossHairX, CrossHairY;

extern float GotoXY_X, GotoXY_Y;
extern int32 PaintIntro, TotalExit, PrintingThickness, TimerValue, WaitForPaint;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ObjectSelection()
{
	int32 mode, FirstMousePosX, FirstMousePosY, MouseDivX, MouseDivY;
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

	SelectionEsc = 0;
	mode = 0;

	if (ReplaceSelections)
		mode = 1;

	OldX = CurrentX;
	OldY = CurrentY;

	DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
	SystemBusyMode = 100;

	ClipMouseCursor();

	while ((LeftButtonPressed) && (!SelectionEsc))
	{
		if (ShiftPressed)
			mode = 0;

		CurrentX = PixelToRealOffX(MousePosX);
		CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		MouseDivX = abs(MousePosX - FirstMousePosX);
		MouseDivY = abs(MousePosY - FirstMousePosY);

//    CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
//    CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));
		if ((OldX != CurrentX) || (OldY != CurrentY))
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			OldX = CurrentX;
			OldY = CurrentY;
			x1 = (double) fabs((CurrentX - CurrentX2) / (double) 1.0);
			y1 = (double) fabs((CurrentY - CurrentY2) / (double) 1.0);

			if (GridSize >= (double) 0.0999)
				sprintf(InfoStr, "%.1f , %.1f mm", x1, y1); //pozice info dole
			else
				sprintf(InfoStr, "%.3f , %.3f", x1, y1);

			RedrawInfoStr(1);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosX > DrawWindowMaxX - ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			ScrollRight(ScrollSize);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
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
		CheckInputMessages(0);
		MouseChanged = 0;

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

	if (!SelectionEsc)
	{
		ClipBoardMemPos = 0;

		if (mode == 1)
		{
			UnselectAll = 1;
			SelectObjectsFromWindow(CurrentX2, CurrentY2, CurrentX, CurrentY, 0);
			UnselectAll = 0;
		}

		SelectObjectsFromWindow(CurrentX2, CurrentY2, CurrentX, CurrentY, 0);
	}

	SelectionEsc = 0;
	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawCrossHair(int32 mode)
{
	double CurrentX, CurrentY;

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

	if (((mode & 16) == 0) && (CrossHairVisible == 0))
	{
		CrossHairX = CurrentX;
		CrossHairY = CurrentY;
		return;
	}

	if ((mode & 8) == 0)
		StartDrawingEditingWindow();

	InitDrawingColorWhite2();
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
			p3 = *(int32 *) DrawXorFunction->Param1[2];
			DrawXorFunction->Function2(p1d, p2d, p3);
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
			p3 = *(int32 *) DrawXorFunction->Param2[2];
			DrawXorFunction->Function2(p1d, p2d, p3);
			break;

		case 2:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2 = *(int32 *) DrawXorFunction->Param2[2];
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

int32 CheckRightButton(DrawXorFunctionRecord * DrawXorFunction)
{
	int32 DivX, DivY;
	static int32 FirstRightButtonDepressed;

	if (RightButtonPressed)
	{
		FirstStartTimer2 = 0;
		FirstRightButtonDepressed = 0;

		if (FirstRightButtonPressed == 0)
		{
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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void MainLoop()
{
	double MinX, MaxX, MinY, MaxY;
	DrawCrossHair(0);
	CurrentDrawMode = 0;

	if (PanActive())
	{
		DrawButtonInfoOff(0);
		DisplayInfoOff(0);
		FindMinMaxDesign(&MinX, &MinY, &MaxX, &MaxY);
		Design.BoardWidth = (float) (MaxX - MinX);
		Design.BoardHeight = (float) (MaxY - MinY);
		Design.BoardCentreX = (float) ((MaxX + MinX) / 2);
		Design.BoardCentreY = (float) ((MaxY + MinY) / 2);
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
			DrawButtonInfoOff(1);

		DisplayInfo(0);
	}

	if (ZoomActive())
	{
		DrawButtonInfoOff(0);
		DisplayInfoOff(0);
		FindMinMaxDesign(&MinX, &MinY, &MaxX, &MaxY);
		Design.BoardWidth = (float) (MaxX - MinX);
		Design.BoardHeight = (float) (MaxY - MinY);
		Design.BoardCentreX = (float) ((MaxX + MinX) / 2);
		Design.BoardCentreY = (float) ((MaxY + MinY) / 2);
		ZoomWindow();
	}

	if (CheckLeftButton())
	{
		DrawButtonInfoOff(0);
		DisplayInfoOff(0);

		if (MousePosX < DrawWindowMinX)
		{
			CheckButtonPressed(0);

			while ((LeftButtonPressed) && (Focused))
				CheckInputMessages(0);

			CheckButtonPressed(1);
		}
		else
		{
			switch (MouseActionMode)
			{
			case 0:
				ObjectSelection();

				if (RepeatMode == 0)
					LastAction = 0;

				if (LastAction > 0)
				{
					switch (LastAction)
					{
					case 1:
						MoveSelectedObjects(0, 1);
						break;

					case 2:
						MoveSelectedObjects(1, 1);
						break;
					}
				}

				break;

			case 1:
				MouseActionMode = 2;
				break;
			}
		}
	}
	else
	{
		if (MouseActionMode == 2)
		{
			SCHCommand(SCHWindow, (WPARAM) DrawingCommand, 0);
			MouseActionMode = 0;
			DrawingCommand = 0;
		}
	}

	if (MouseChanged)
	{
		if (MousePosX >= DrawWindowMinX)
			DisplayCursorPosition();

		CheckMouseOutOfWindow(0);
		MouseChanged = 0;
	}

	if (LeftButtonDoublePressed)
	{
		PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_TEXT, (LPARAM) NULL);
		LeftButtonDoublePressed = 0;
	}

	if (CheckRightButton(NULL) == 1)
	{
		DrawCrossHair(2);
		DrawButtonInfoOff(0);
		DisplayInfoOff(0);
		MenuPopUp();
//    RightButtonPressed=0;
		CheckInputMessages(0);
	}

	/*
	  if ((!CtrlPressed)
	     &&
	     (RightButtonPressed)) {
	    DrawCrossHair(2);
	    DrawButtonInfoOff(0);
	    DisplayInfoOff(0);
	    MenuPopUp();
	    RightButtonPressed=0;
	    CheckInputMessages(0) ;
	  }
	*/
	if ((SearchReference) && (Design.NrInstances > 0))
	{
		CenterScreenOnInstance(2);
		SearchReference = 0;
	}

	if ((SearchPartnr) && (Design.NrInstances > 0))
	{
		CenterScreenOnInstance(3);
		SearchPartnr = 0;
	}

	if ((GotoXY_X > -1000.0) && (GotoXY_X < 1000.0) && (GotoXY_Y > -1000.0) && (GotoXY_Y < 1000.0))
	{
		CenterScreen(GotoXY_X, GotoXY_Y, 1);
		GotoXY_X = 100000.0;
		GotoXY_Y = 100000.0;
	}

	if (NrFunctionsInBuf > 0)
	{
		DrawCrossHair(2);
		DrawButtonInfoOff(0);
		DisplayInfoOff(0);
		ExecuteKeys();

		if (HelpAsked)
		{
			Help("Contents.htm", 0);
//      Help(0,3);
//      Help(0,1);
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			HelpAsked = 0;
		}

		if (SelectionEsc)
			SelectionEsc = 0;
	}

	if (DataBaseChanged)
	{
		DataBaseChanged = 0;
		FileChanged = 1;
		SetWindowName(0);

		if (!UndoRedoActive)
			LastActionNr++;

		UndoRedoActive = 0;
	}
}

//***************************************************************************************************************************
//************************** zobrazení pozice absolutní x,y * møížka x,y ****************************************************
//***************************************************************************************************************************

void DisplayCursorPosition()
{
	double x, y, x2, y2, x3, y3;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	if ((MousePosX == 10000) || (MousePosY == 10000))
		return;

	x = ((double) PixelToRealOffX(MousePosX));
	y = ((double) PixelToRealOffY(DrawWindowMaxY - MousePosY));

	x = (double) AdjustToDrawGrid((double) x);
	y = (double) AdjustToDrawGrid((double) y);

	switch (Units)
	{
	case 0:
		x2 = x / 254000.0;
		y2 = y / 254000.0;
		x3 = (x - RelX) / 254000.0;
		y3 = (y - RelY) / 254000.0;

		sprintf(str, "%.3f , %.3f", x2, y2);
		sprintf(str2, "%.0f , %.0f", x3, y3);
		break;

	case 1:
		x2 = PixelToRealOffX(MousePosX);
		y2 = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		x3 = (x - RelX) * 1.0;
		y3 = (y - RelY) * 1.0;

		sprintf(str, SC(441, "absolute x,y %.2f , %.2f mm"), x2, y2);

		if (GridSize >= (double) 0.0999)
		{
			sprintf(str2, SC(442, "grid x,y %.1f , %.1f mm"), x3, y3);
		}
		break;

	case 2:
		x2 = x / 100000.0;
		y2 = y / 100000.0;
		x3 = (x - RelX) / 100000.0;
		y3 = (y - RelY) / 100000.0;

		sprintf(str, "%.3f , %.3f", x2, y2);
		sprintf(str2, "%.3f , %.3f", x3, y3);
		break;
	}

	strcpy(AbsPosStr, str);
	RedrawAbsPosStr(1); //absolutní x, y

	strcpy(RelPosStr, str2);
	RedrawRelPosStr(1); //møížka x,y

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CheckForEscape()
{
	MSG M;

	if (PeekMessage(&M, 0, 0, 0, PM_NOREMOVE))
	{
		CheckInputMessages(0);

		if (NrFunctionsInBuf > 0)
			ExecuteKeys();
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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
			RightButtonPressed = 0;
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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void CheckZoomFactor()
{
	if (Factor < 20)
	{
		if (PrintingThickness == 2)
		{
			PrintingThickness = 1;
			DeleteGraphicObjects();
			CreateDrawObjects(0);
		}
	}

	if (Factor > 20)
	{
		if (PrintingThickness == 1)
		{
			PrintingThickness = 2;
			DeleteGraphicObjects();
			CreateDrawObjects(0);
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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

	if (Factor < 1630.0)
	{
		cx = PixelToRealOffX(MouseX);
		cy = PixelToRealOffY(DrawWindowMaxY - MouseY);

		if ((mode & 2) == 0)
			Factor = (Factor / 2) * 3;
		else
		{
			if ((mode & 4) == 0)
				Factor = (Factor / 7) * 8;
			else
				Factor = (Factor / 6) * 8;
		}

		CheckZoomFactor();
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;
		DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
		DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Rect.left = (int16) DrawWindowMinX;
		Rect.right = (int16) DrawWindowMaxX;
		Rect.top = (int16) DrawWindowMinY;
		Rect.bottom = (int16) DrawWindowMaxY;
		InvalidateRect(SCHWindow, &Rect, 0);
		UpdateWindow(SCHWindow);

		if ((mode & 4) == 0)
			SetCursorPos(DrawWindowMinX + ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
	}

	if ((mode & 1) == 0)
		ZoomInOutProcessed = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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
		MouseX = (DrawWindowMaxX + DrawWindowMinX + 2) / 2;
		MouseY = (DrawWindowMaxY + DrawWindowMinY - 2) / 2;
	}
	else
	{
		MouseX = (DrawWindowMaxX + DrawWindowMinX + 2) / 2;
		MouseY = (DrawWindowMaxY + DrawWindowMinY - 2) / 2;
	}

	if (Factor > 1)
	{
		cx = PixelToRealOffX(MouseX);
		cy = PixelToRealOffY(DrawWindowMaxY - MouseY);

		if ((mode & 2) == 0)
			Factor = (Factor / 3) * 2;
		else
		{
			if ((mode & 4) == 0)
				Factor = (Factor / 8) * 7;
			else
				Factor = (Factor / 8) * 6;
		}

		CheckZoomFactor();

		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;

//    Xoffset=cx-PixelToReal(MouseX);
//    Yoffset=cy-PixelToReal(DrawWindowMaxY-MouseY);
		DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
		DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
		Rect.left = (int16) DrawWindowMinX;
		Rect.right = (int16) DrawWindowMaxX;
		Rect.top = (int16) DrawWindowMinY;
		Rect.bottom = (int16) DrawWindowMaxY;
		InvalidateRect(SCHWindow, &Rect, 0);
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinX - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		UpdateWindow(SCHWindow);

		if ((mode & 4) == 0)
		{
			SetCursorPos(DrawWindowMinX + ClientStartX + ClientWindowDivX / 2,
			             ClientStartY + (ClientWindowDivY / 2) - 2);
		}
	}

	if ((mode & 1) == 0)
		ZoomInOutProcessed = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ViewFull(int32 mode)
{
	RECT Rect;

	DrawCrossHair(7);
	ViewWholeDesign(0);

	Rect.left = (int16) DrawWindowMinX;
	Rect.right = (int16) DrawWindowMaxX;
	Rect.top = (int16) DrawWindowMinY;
	Rect.bottom = (int16) DrawWindowMaxY;
	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

	if (SCHWindow)
	{
		InvalidateRect(SCHWindow, &Rect, 0);
		UpdateWindow(SCHWindow);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


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
	Rect.left = (int16) DrawWindowMinX;
	Rect.right = (int16) DrawWindowMaxX;
	Rect.top = (int16) DrawWindowMinY;
	Rect.bottom = (int16) DrawWindowMaxY;
	InvalidateRect(SCHWindow, &Rect, 0);
	UpdateWindow(SCHWindow);
	SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void PreviousView()
{
	RECT Rect;

	if (ViewPosPointer > 1)
	{
		DrawCrossHair(7);
		Xoffset = ViewPos[ViewPosPointer - 2].Xoffset;
		Yoffset = ViewPos[ViewPosPointer - 2].Yoffset;
		Factor = ViewPos[ViewPosPointer - 2].Factor;
		CheckZoomFactor();
		ViewPosPointer--;
		OkToAddViewPos = 0;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Rect.left = (int16) DrawWindowMinX;
		Rect.right = (int16) DrawWindowMaxX;
		Rect.top = (int16) DrawWindowMinY;
		Rect.bottom = (int16) DrawWindowMaxY;
		InvalidateRect(SCHWindow, &Rect, 0);
		UpdateWindow(SCHWindow);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RePaint()
{
	if (SCHWindow)
	{
		DrawCrossHair(7);
		InvalidateRect(SCHWindow, NULL, 0);
		UpdateWindow(SCHWindow);
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CenterScreenOnInstance(int32 mode)
{
	int32 cnt, Found;
	double cx, cy, PixelsX, PixelsY, hulp;
	InstanceRecord *Instance;
	RECT Rect;

	Found = -1;

	if (EditingSymbol)
		return -1;

	switch (mode & 1)
	{
	case 0:
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = &((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
//           &&
//           ((Instance->RefInfo & TEXT_NOT_VISIBLE) == 0)
			        && (stricmpUTF8(Instance->Reference, SearchCodeString) == 0))
			{
				Found = cnt;
				break;
			}
		}

		break;

	case 1:
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = &((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0) && (Instance->PartNr[0])
			        && (stricmpUTF8(Instance->PartNr, SearchCodeString) == 0))
			{
				Found = cnt;
				break;
			}
		}

		break;
	}

	if (Found == -1)
		return -1;

	Instance = &((*Instances)[Found]);

	if (mode & 2)
		Instance->Info |= OBJECT_SELECTED | 7;

	PixelsX = (double) (DrawWindowMaxX - DrawWindowMinX);
	PixelsY = (double) (DrawWindowMaxY - DrawWindowMinY);
	cx = (Instance->BoardPosMaxX + Instance->BoardPosMinX) / 2;
	cy = (Instance->BoardPosMaxY + Instance->BoardPosMinY) / 2;

	if (((Instance->BoardPosMaxX - Instance->BoardPosMinX) / PixelsX) >
	        ((Instance->BoardPosMaxY - Instance->BoardPosMinY) / PixelsY))
	{
		hulp = (double) (DrawWindowMaxX - DrawWindowMinX);
		hulp = hulp / (Instance->BoardPosMaxX - Instance->BoardPosMinX);
		hulp = hulp * (double) 0.7;
		Factor = min(20, hulp);
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

		hulp = (double) (DrawWindowMaxY - DrawWindowMinY);
		hulp = hulp / (Instance->BoardPosMaxY - Instance->BoardPosMinY);
		hulp = hulp * (double) 0.7;
		Factor = min(20, hulp);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Yoffset = cy - (ViewMaxY - ViewMinY) / 2;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
	}

	DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
	DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
	Rect.left = (int16) DrawWindowMinX;
	Rect.right = (int16) DrawWindowMaxX;
	Rect.top = (int16) DrawWindowMinY;
	Rect.bottom = (int16) DrawWindowMaxY;
	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
	DrawCrossHair(7);
	InvalidateRect(SCHWindow, &Rect, 0);
	UpdateWindow(SCHWindow);
	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CenterScreen(double cx, double cy, int32 mode)
{
	double DisplX, DisplY;

	DrawCrossHair(7);
	Factor = (double) 25.0;
	CheckZoomFactor();
	DisplX = PixelToReal(DrawWindowMaxX - DrawWindowMinX);
	DisplY = PixelToReal(DrawWindowMaxY - DrawWindowMinY);
	Xoffset = cx - DisplX * (double) 0.5;
	Yoffset = cy - DisplY * (double) 0.5;
	PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, (LPARAM) NULL);

	if (mode == 1)
	{
		CheckInputMessages(200);
		DrawCircleWhite(cx, cy, 20, 1);
	}
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
		SendMessage(SCHWindow, WM_CLOSE, 0, 0);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void ExecuteKeys()
{
	int32 Key, KeyFunction;

	Key = ReadKeyFunction();
	KeyFunction = GetFunctionByTranslatedKey(Key, 0);

	switch (Key)
	{
	case Key_Esc:

//        MenuPopUp();
		if (LastAction > 0)
		{
			LastAction = 0;
// Disable repeat mode
		}

		SelectionEsc = 1;
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

	case Key_Shift_F1:
		break;

	case Key_F4:
		break;

	case Key_Ctrl_F4:
		break;

	case Key_F6:
		break;

	case Key_Shift_F6:
		break;

	case Key_F7:
		break;

	case Key_F8:
#ifdef _DEBUG
#endif
		break;

	case Key_F9:
#ifdef _DEBUG
#endif
		break;

	case Key_Shift_F9:
		break;

	case Key_F10:
#ifdef _DEBUG
		AddTextObjectsFromFile(0);
//      sprintf(LineBuf,"Used mem is %i k",GetUsedMemSize(0)/1024);
//      MessageBoxUTF8(SCHWindow,LineBuf,SC(20,"Message"),MB_APPLMODAL|MB_OK);
#endif
		break;

	case Key_Ctrl_F10:
		ReplaceTexts(0);
		break;

	case Key_F11:
		break;

	case Key_F12:
		break;

	case Key_Shift_F12:
//      TestPrint();
		break;
#ifdef _DEBUG

	case (uint16) 'G':
		DrawNewGrid();
		GridSize = (double) 0.01;
		DrawGridSize = GridSize;
		DrawNewGrid();
		break;
#endif

	case Key_Ctrl_C:
		PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_COPY_CLIP, (LPARAM) NULL);
		break;

	case Key_Ctrl_V:
		PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_INSERT_CLIP, (LPARAM) NULL);
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
		SendMessage(SCHWindow, WM_COMMAND, (WPARAM) (KeyFunction & 0xffff), (LPARAM) 1);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
