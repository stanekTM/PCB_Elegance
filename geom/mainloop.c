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
#include "geom.h"
#include "calcdef.h"
#include "calc.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "mainloop.h"
#include "files.h"
#include "edit.h"
#include "ellipss.h"
#include "line2.h"
#include "files2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "menus.h"
#include "dialogs.h"
#include "insdel.h"
#include "select.h"
#include "graphics.h"
#include "resource.h"
#include "movecomp.h"
#include "help.h"
#include "owntime.h"


//#include "wintest.h"
//#include "window.h"
//#include "menus.h"


//       HMENU Menu;
//       HBRUSH BackGroundBrush;
//       RECT DisplayRect;
//       TEXTMETRIC Metrics;
//       OPENFILENAME FileInputInfo;
//       FindReplace:TFindReplace;
//       int32 commerror;
//       uint8 WindowsAttr;
//       HANDLE ResourceHandle,UserFont;

//       WNDCLASS  wc;

#define PanPixels      80

POINT MousePos;
static double CentreSelectedX, CentreSelectedY, CurrentX2, CurrentY2;

int32 MaxCollections, px, py, ro, OldMousePosX, OldMousePosY, OldX2, OldY2, NetNrCheck, MousePosXR, MousePosYR,
      RightButtonDivTime, FirstRightButtonPressed;

int32 FirstStartTimer2 = 1;
int32 direction;
int32 LastDirection = -1;
int32 SpecialLayer = 1;
int32 CrossHairNewStart = 1;
int32 MaxDisplayDiv = 5;
int32 ZoomActivated;
int32 PanActivated;

RECT CursorWindow;

double CrossHairX, CrossHairY;

// *******************************************************************************************************
// *******************************************************************************************************

extern int32 TotalExit, PaintIntro;
extern char LibraryFile[MAX_LENGTH_STRING];
extern HDC OutputDisplay;
extern int32 TimerValue, WaitForPaint;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ScrollGEOMWindow(void);


#ifdef _DEBUG
void MoveSpecial(void);
#endif

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void ObjectSelection()
{
	int32 mode;
	double OldX, OldY, CurrentX, CurrentY, x1, y1;

	CurrentX = PixelToRealOffX(MousePosX);
	CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
//  CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
//  CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;
	OldX = CurrentX;
	OldY = CurrentY;

	SelectionActive = 1;
	SelectionEsc = 0;
	mode = 0;

	if (ReplaceSelections)
		mode = 1;

	OldX = CurrentX;
	OldY = CurrentY;

	DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);

	ClipMouseCursor();

	while ((LeftButtonPressed) && (!SelectionEsc))
	{
		if (ShiftPressed)
			mode = 0;

		CurrentX = PixelToRealOffX(MousePosX);
		CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);

//    CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
//    CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));
		if ((OldX != CurrentX) || (OldY != CurrentY))
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			OldX = CurrentX;
			OldY = CurrentY;

			if (Units == 0)
			{
				x1 = fabs((CurrentX - CurrentX2) / 2540.0);
				y1 = fabs((CurrentY - CurrentY2) / 2540.0);
				sprintf(InfoStr, "%.2f,%.2f", x1, y1);
			}
			else
			{
				x1 = fabs((CurrentX - CurrentX2) / 100000.0);
				y1 = fabs((CurrentY - CurrentY2) / 100000.0);
				sprintf(InfoStr, "%.4f,%.4f", x1, y1);
			}

			RedrawInfoStr(1);
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollRight(ScrollSize);
			SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			MousePosX -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollDown(ScrollSize);
			SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
			MousePosY -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollLeft(ScrollSize);
			SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			MousePosX += ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
		}

		if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
		{
			DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
			ScrollUp(ScrollSize);
//        NrGraphicsObjects
			SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
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

			if (!LeftButtonPressed)
				SelectionEsc = 1;

			if (!SelectionEsc)
				DrawXorWindow(MultX(CurrentX2), MultY(CurrentY2), MultX(OldX), MultY(OldY), 0);
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

	SelectionActive = 0;
	DrawCrossHair(2);

	if (!SelectionEsc)
	{
		if (mode == 1)
		{
			UnselectAll = 1;
			SelectObjectsFromWindow(CurrentX2, CurrentY2, CurrentX, CurrentY, 0);
			UnselectAll = 0;
		}

		SelectObjectsFromWindow(CurrentX2, CurrentY2, CurrentX, CurrentY, mode);
	}

	SelectionEsc = 0;
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
		ScrollRight(ScrollSize);
		SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
		MousePosX -= ScrollSizeDrawing;
	}

	if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
	{
		ScrollDown(ScrollSize);
		SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
		MousePosY -= ScrollSizeDrawing;
	}

	if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
	{
		ScrollLeft(ScrollSize);
		SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
		MousePosX += ScrollSizeDrawing;
	}

	if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
	{
		ScrollUp(ScrollSize);
		SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
		MousePosY += ScrollSizeDrawing;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawSpecialXorFunction(DrawXorFunctionRecord * DrawXorFunction, int32 mode)
{
	double p1d, p2d, p3d, p4d, p5d, p6d, p7d;
	int32 p1, p2, p3, p4, p5, p8;

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

		case 9:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3 = *(int32 *) DrawXorFunction->Param2[2];
			DrawXorFunction->Function10a(p1d, p2d, p3);
			break;

		case 10:
			p1d = *(double *) DrawXorFunction->Param1[0];
			p2d = *(double *) DrawXorFunction->Param1[1];
			p3d = *(double *) DrawXorFunction->Param1[2];
			p4d = *(double *) DrawXorFunction->Param1[3];
			p5 = *(int32 *) DrawXorFunction->Param1[4];
			DrawXorFunction->Function11(p1d, p2d, p3d, p4d, p5);
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

		case 9:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3 = *(int32 *) DrawXorFunction->Param2[2];
			DrawXorFunction->Function10b(p1d, p2d, p3);
			break;

		case 10:
			p1d = *(double *) DrawXorFunction->Param2[0];
			p2d = *(double *) DrawXorFunction->Param2[1];
			p3d = *(double *) DrawXorFunction->Param2[2];
			p4d = *(double *) DrawXorFunction->Param2[3];
			p5 = *(int32 *) DrawXorFunction->Param2[4];
			DrawXorFunction->Function11(p1d, p2d, p3d, p4d, p5);
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MainLoop()
{
	DrawCrossHair(0);

	if (PanActive())
	{
		DrawButtonInfoOff(0);
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
	}

	if (ZoomActive())
	{
		DrawButtonInfoOff(0);
		ZoomWindow();
	}

	if (CheckLeftButton())
	{
		DrawButtonInfoOff(0);

		if (MousePosX < DrawWindowMinX)
		{
			CheckButtonPressed(0);

			while ((LeftButtonPressed) && (Focused))
				CheckInputMessages(0);

			CheckButtonPressed(1);
		}
		else
		{
			ObjectSelection();

			if (RepeatMode == 0)
				LastAction = 0;

			if ((LastAction > 0) && (GetNrSelectObjects() > 0))
			{
				RepeatModeBusy = 1;
				MoveSelectedObjects(0, 0);
				RepeatModeBusy = 0;
			}
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
		PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_EDIT_TEXT, (LPARAM) NULL);
		LeftButtonDoublePressed = 0;
	}

	if (CheckRightButton(NULL) == 1)
	{
//  if (RightButtonPressed) {
		DrawCrossHair(2);
		DrawButtonInfoOff(0);
		MenuPopUp();
//    RightButtonPressed=0;
		CheckInputMessages(0);
	}

	if (NrFunctionsInBuf > 0)
	{
		DrawCrossHair(2);
		DrawButtonInfoOff(0);
		ExecuteKeys();

		if (SelectionEsc)
			SelectionEsc = 0;
	}

	if (HelpAsked)
	{
		if (GetNrSelectObjects() > 0)
			Help("change_objects.htm", 0);
		else
			Help("contents.htm", 0);

		CheckInputMessages(0);

		while (!Focused)
			CheckInputMessages(0);

		CheckInputMessages(0);
		HelpAsked = 0;
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

		if (LastActionNr >= 16384)
			GEOMSystemError = 5000;
	}

	if (GEOMSystemError != 0)
		GEOMSystemError = 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DisplayCursorPosition()
{
	double x, y, x1, y1, x2, y2, x3, y3, x4, y4, x5, Angle, Length;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	if ((MousePosX == 10000) || (MousePosY == 10000))
		return;

	x = (PixelToRealOffX(MousePosX));
	y = (PixelToRealOffY(DrawWindowMaxY - MousePosY));
	x1 = x;
	y1 = y;
	x4 = x / 100000.0;
	y4 = y / 100000.0;
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

		Length = CalcLengthLine(0.0, 0.0, x3, y3);
//      sprintf(str,"%i,%i",MousePosX,MousePosY);
		sprintf(str, SC(274, "(x,y) %.4f,%.4f mm"), x4, y4);
		sprintf(str2, "(grid x,y) %.2f,%.2f thou", x2, y2);
		sprintf(str3, "(rel x,y) %.2f,%.2f [%.1f %.1f] thou", x3, y3, Length, Angle);
		/*
		      strcpy(str3,"                ");
		      for (cnt=0;cnt<16;cnt++) {
		        str3[cnt]=cnt+112;
		      }
		*/
//      sprintf(str3,"(rel x,y) %.2f,%.2f thou",x3,y3);
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

		Length = CalcLengthLine(0.0, 0.0, x3, y3);
		sprintf(str, SC(274, "(x,y) %.4f,%.4f mm"), x4, y4);
		sprintf(str2, SC(277, "(grid x,y) %.4f,%.4f mm"), x2, y2);
//      sprintf(str3,"(rel x,y) %.4f,%.4f mm",x3,y3);
		sprintf(str3, SC(278, "(rel x,y) %.4f,%.4f [%.4f %.1f] mm"), x3, y3, Length, Angle);
		break;
	}

	strcpy(AbsPosStr, str);
	RedrawAbsPosStr(1);
//  if (DebugPaint) {
//    sprintf(str2,"%i,%i",MousePosX,MousePosY);
//  }
	strcpy(AbsGridPosStr, str2);
	RedrawAbsGridPosStr(1);

	strcpy(RelPosStr, str3);
	RedrawRelPosStr(1);
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

	InitDrawingColorWhite(1);
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

	if (Factor < 0.1)
	{
		cx = PixelToRealOffX(MouseX);
		cy = PixelToRealOffY(DrawWindowMaxY - MouseY - 1);

		if ((mode & 2) == 0)
			Factor = (Factor / 2) * 3;
		else
		{
			if ((mode & 4) == 0)
				Factor = (Factor / 7) * 8;
			else
				Factor = (Factor / 6) * 8;
		}

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
		DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
		DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		Rect.left = DrawWindowMinX;
		Rect.right = DrawWindowMaxX;
		Rect.top = DrawWindowMinY;
		Rect.bottom = DrawWindowMaxY;
		InvalidateRect(GEOMWindow, &Rect, 0);
		DisplayCursorPosition();
		UpdateWindow(GEOMWindow);

		if ((mode & 4) == 0)
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
		{
			if ((mode & 4) == 0)
				Factor = (Factor / 8) * 7;
			else
				Factor = (Factor / 8) * 6;
		}

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
		Rect.left = DrawWindowMinX;
		Rect.right = DrawWindowMaxX;
		Rect.top = DrawWindowMinY;
		Rect.bottom = DrawWindowMaxY;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinX - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		InvalidateRect(GEOMWindow, &Rect, 0);
		DisplayCursorPosition();
		UpdateWindow(GEOMWindow);

		if ((mode & 4) == 0)
			SetCursorPos(DrawWindowMinX + ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
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
	Rect.left = DrawWindowMinX;
	Rect.right = DrawWindowMaxX;
	Rect.top = DrawWindowMinY;
	Rect.bottom = DrawWindowMaxY;
	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
	DisplayCursorPosition();
	InvalidateRect(GEOMWindow, &Rect, 0);
	UpdateWindow(GEOMWindow);
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
		Rect.left = DrawWindowMinX;
		Rect.right = DrawWindowMaxX;
		Rect.top = DrawWindowMinY;
		Rect.bottom = DrawWindowMaxY;
		InvalidateRect(GEOMWindow, &Rect, 0);
		DisplayCursorPosition();
		UpdateWindow(GEOMWindow);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void RePaint()
{
	DrawCrossHair(7);
	InvalidateRect(GEOMWindow, NULL, 0);
	PostMessage(GEOMWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnselectAllObjects()
{
	UnselectAll = 1;
	SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
	UnselectAll = 0;
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
	Rect.left = DrawWindowMinX;
	Rect.right = DrawWindowMaxX;
	Rect.top = DrawWindowMinY;
	Rect.bottom = DrawWindowMaxY;
	InvalidateRect(GEOMWindow, &Rect, 0);
	DisplayCursorPosition();
	UpdateWindow(GEOMWindow);
	SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GotoXY()
{
	float x, y;

	ObjectRecord ObjectText;
	memset(&ObjectText, 0, sizeof(ObjectRecord));

	if (TextInputDialog(&ObjectText, 0x10 + 8) == 1)
	{
		if (sscanf((LPSTR) & ObjectText.Text, "%f,%f", &x, &y) == 2)
		{
			CenterScreen(UnitConvert(x, Units), UnitConvert(y, Units));
			RePaint();
		}
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
		SendMessage(GEOMWindow, WM_CLOSE, 0, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void ExecuteKeys()
{
	int32 ok, Key, KeyFunction;

	Key = ReadKeyFunction();
	KeyFunction = GetFunctionByTranslatedKey(Key, 0);

	switch (Key)
	{
	case Key_Esc:
		SelectionEsc = 1;

		switch (SelectionMode)
		{
		case 1:
			if (LastAction == 0)
				RepeatMode = 0;

			if (RepeatModeBusy == 0)
				RepeatMode = 0;

			LastAction = 0;
			break;
		}

		break;

	case (int32) ' ':
		SpacePressed = 1;
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

	case Key_F2:
		break;

	case Key_Shift_F2:
		break;

	case Key_F3:
		break;

	case Key_Shift_F3:
		break;

	case Key_F4:
		break;

	case Key_Shift_F6:
		break;

	case Key_F6:
		ok = 1;
		break;

	case Key_F7:
		break;

	case Key_F8:
		/*
		      TestRect.left=0;
		      TestRect.top=0;
		      TestRect.right=10000;  // 0.01 mm
		      TestRect.bottom=10000;
		      sprintf(str,"GeomTest\0PictureTest\0");
		      BlackPen=CreatePen(PS_SOLID,10,RGB(0,0,0));
		      sprintf(str,"c:\\geom\\test.emf");
		      HdcTest=CreateEnhMetaFile(NULL,str,&TestRect,(LPSTR)&str);
		      res=GetMapMode(HdcTest);
		// MM_HIMETRIC
		      SelectObject(HdcTest,BlackPen);
		      res=MoveToEx(HdcTest,100,100,NULL);
		      res=LineTo(HdcTest,300,200);
		      MetaHandle=CloseEnhMetaFile(HdcTest);
		      GetEnhMetaFileHeader(MetaHandle,65536,(LPENHMETAHEADER)&Buf);
		      EmfInfo=(LPENHMETAHEADER)&Buf;
		      StartDrawingEditingWindow();
		      res=GetWinMetaFileBits(MetaHandle,65536,(uint8 *)&Buf,MM_ANISOTROPIC,OutputDisplay);
		      ExitDrawing();
		      EndDrawingEditingWindow();
		      sprintf(str,"c:\\geom\\test.wmf");
		      fp=FileOpenWriteUTF8(str);
		      FileWrite(fp,&Buf,res,&result);
		      FileClose(fp);
		      res=DeleteEnhMetaFile(MetaHandle);
		*/

		break;

	case Key_F9:
#ifdef _DEBUG
		PrintScreenToBitmap(0);
#endif
		break;

	case Key_Shift_F9:
		break;

	case Key_F10:
#ifdef _DEBUG
		ChangeClearance((6 * 2540.0), 1);
		FileChanged = 1;
		SaveFile(0);

		if (SystemBusyMode == 0)
		{
			SelectionEsc = 1;
			SendMessage(GEOMWindow, WM_CLOSE, 0, 0);
		}

#endif
		break;

	case Key_Ctrl_F10:
		break;

	case Key_Ctrl_F11:
		break;

	case Key_Ctrl_F9:
		break;

	case Key_Shift_F10:
		break;

	case Key_F11:
		break;

	case Key_Shift_F11:
		break;

	case Key_F12:
		break;

	case Key_Shift_F12:
		break;
#ifdef _DEBUG

	case (uint16) 'q':
		MoveSpecial();
		break;
#endif

	case Key_Alt_X:
		ExitProgram();
		break;

	default:
		if (KeyFunction == 0)
			MessageBeep((uint32) - 1);

		break;
	}

	if (KeyFunction != 0)
		SendMessage(GEOMWindow, WM_COMMAND, (WPARAM) (KeyFunction), (LPARAM) 1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
