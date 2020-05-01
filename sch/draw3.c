/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw3.c
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
#include "calc.h"
#include "calcdef.h"
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
#include "help.h"
#include "draw2.h"
#include "draw3.h"
#include "graphics.h"
#include "resource.h"

double NewFactor;

int32 TimeOut, TimeOutInfo = -1, OldButtonNr;
int32 DisplayButtonNr, DisplayInfoTimeOut = -1;
int32 NrButtonInfo = -1;
int32 NrButtonsSymbol = 20;
int32 NrButtonsSheet = 23;
int32 ButtonSizeX = 27;
int32 ButtonSizeY = 22;

char DisplayInfoStr[2048];
RECT ButtonRect, InfoRect;

extern int32 TimerValue;
extern HPEN BackGroundPen;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawXorWindow(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	int32 xmin, xmax, ymin, ymax;

	StartDrawingEditingWindow();

	InitDrawingColorWhite();

	SetROP2(OutputDisplay, R2_XORPEN);

	xmin = min(x1, x2);
	xmax = max(x1, x2);
	ymin = min(y1, y2);
	ymax = max(y1, y2);
	rect2(xmin, ymin, abs(x2 - x1) + 1, abs(y2 - y1) + 1);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void DrawXorWindow2(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	double hulp, CentreX, CentreY, DisplayX, DisplayY, m2;
	int32 x3, y3, x4, y4, difx, cx, cy, cx2, cy2, xmin, xmax, ymin, ymax;

	StartDrawingEditingWindow();

	InitDrawingColorWhite();

	SetROP2(OutputDisplay, R2_XORPEN);

	xmin = min(x1, x2);
	xmax = max(x1, x2);
	ymin = min(y1, y2);
	ymax = max(y1, y2);
	cx2 = (DrawWindowMaxX + DrawWindowMinX) / 2 + DrawWindowMinX;
	cy2 = (DrawWindowMaxY + DrawWindowMinY) / 2;
	CentreX = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
	CentreY = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);
	cx = (x1 + x2) / 2;
	cy = (y1 + y2) / 2;

	if ((x1 <= x2) || (y1 <= y2))
		rect2(xmin, ymin, abs(x2 - x1) + 1, abs(y2 - y1) + 1);
	else
	{
		difx = x1 - x2;

		if (Design.BoardWidth * DisplY / DisplX > Design.BoardHeight)
		{
			DisplayX = (DrawWindowMaxX - DrawWindowMinX);
			DisplayX *= 0.75;
			DisplayY = DisplayX * Design.BoardHeight / Design.BoardWidth;
		}
		else
		{
			DisplayX = (DrawWindowMaxX - DrawWindowMinX);
			DisplayX *= 0.75;
			DisplayY = (DisplayX * DisplY / DisplX);
			DisplayX = (DisplayY * Design.BoardWidth / Design.BoardHeight);
		}

		hulp = (DisplX / Design.BoardWidth * DisplayX);
		x3 = (int32) hulp + difx;
		m2 = x3 / hulp;
//    sprintf(&str,"%.3f",m2);
//      DrawStr(CentreX,CentreY,1.0,0,0,&str);
		NewFactor = Factor / m2;
		hulp = x3;
		hulp = (hulp * (DisplY / DisplX));
		y3 = (int32) hulp;
		hulp = ((CentreX - Design.BoardCentreX) / Design.BoardWidth) * DisplayX;
		x4 = (int32) hulp;
		hulp = ((CentreY - Design.BoardCentreY) / Design.BoardHeight) * DisplayY;
		y4 = (int32) hulp;

		rect3(cx2, cy2, (int32) DisplayX, (int32) DisplayY);
		rect3(cx2 + x4, cy2 - y4, x3, y3);
//    Rectangle(OutputDisplay,x1,y1,x2,y2);
// DisplX DisplY
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ZoomWindow()
{
	double ZoomMinX, ZoomMinY, ZoomMaxX, ZoomMaxY, hulp, cx, cy, PixelsX, PixelsY, CentreX, CentreY, cx2, cy2;
	int32 mode, OldX2, OldY2;
	RECT Rect;

	SelectRectX1 = MousePosX;
	SelectRectX2 = MousePosX;
	SelectRectY1 = MousePosY;
	SelectRectY2 = MousePosY;
	OldX2 = SelectRectX2;
	OldY2 = SelectRectY2;

	if (CurrentDrawMode == 0)
		CurrentDrawMode = 1;

	SelectionEsc = 0;
	mode = 0;
	ClipMouseCursor();

//  SystemBusyMode=200;
	DrawXorWindow2(SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2, mode);

	while ((ZoomActive()) && (!SelectionEsc))
	{
		if (MouseChanged)
		{
			DrawXorWindow2(SelectRectX1, SelectRectY1, OldX2, OldY2, mode);
			OldX2 = MousePosX;
			OldY2 = MousePosY;
			SelectRectX2 = MousePosX;
			SelectRectY2 = MousePosY;
			DrawXorWindow2(SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2, mode);

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (!Focused)
		{
			DrawXorWindow2(SelectRectX1, SelectRectY1, OldX2, OldY2, mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			SelectionEsc = 1;
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawXorWindow2(SelectRectX1, SelectRectY1, OldX2, OldY2, mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("window_based_zooming.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
				SelectionEsc = 1;
			}

			if (!SelectionEsc)
				DrawXorWindow2(SelectRectX1, SelectRectY1, OldX2, OldY2, mode);
		}
	}

	if (!SelectionEsc)
		DrawXorWindow2(SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2, mode);

	UnClipMouseCursor();

	if (!SelectionEsc)
	{
		if ((SelectRectX1 != SelectRectX2) && (SelectRectY1 != SelectRectY2))
		{
			if ((SelectRectX1 <= SelectRectX2) || (SelectRectY1 <= SelectRectY2))
			{

				cx = PixelToRealOffX((SelectRectX1 + SelectRectX2) / 2);
				cy = PixelToRealOffY(DrawWindowMaxY - ((SelectRectY1 + SelectRectY2) / 2) - 1);
				ZoomMinX = PixelToRealOffX(min(SelectRectX1, SelectRectX2));
				ZoomMinY = PixelToRealOffY(DrawWindowMaxY - max(SelectRectY1, SelectRectY2) - 1);
				ZoomMaxX = PixelToRealOffX(max(SelectRectX1, SelectRectX2));
				ZoomMaxY = PixelToRealOffY(DrawWindowMaxY - min(SelectRectY1, SelectRectY2) - 1);
				PixelsX = (DrawWindowMaxX - DrawWindowMinX);
				PixelsY = (DrawWindowMaxY - DrawWindowMinY);

				if (((ZoomMaxX - ZoomMinX) / PixelsX) > ((ZoomMaxY - ZoomMinY) / PixelsY))
				{
					Xoffset = ZoomMinX;
					hulp = (DrawWindowMaxX - DrawWindowMinX);
					hulp = hulp / (ZoomMaxX - ZoomMinX);
					Factor = hulp;
					ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
					ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
					Yoffset = cy - (ViewMaxY - ViewMinY) / 2;
				}
				else
				{
					// *****************************************************************************************

					Yoffset = ZoomMinY;
					hulp = (DrawWindowMaxY - DrawWindowMinY);
					hulp = hulp / (ZoomMaxY - ZoomMinY);
					Factor = hulp;
					ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
					ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);

					Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
				}

				DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
				DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
				//      DisplX=(ZoomMaxX-ZoomMinX);
				//      DisplY=DisplX*DrawWindowMaxY/DrawWindowMaxX;
				Rect.left = DrawWindowMinX;
				Rect.right = DrawWindowMaxX;
				Rect.top = DrawWindowMinY;
				Rect.bottom = DrawWindowMaxY;
				ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
				ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
				ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
				ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
			}
			else
			{
				CentreX = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
				CentreY = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);
				Factor = NewFactor;
				DisplX = PixelToReal(DrawWindowMaxX - DrawWindowMinX);
				DisplY = PixelToReal(DrawWindowMaxY - DrawWindowMinY);
				Xoffset = (CentreX - DisplX / 2);
				Yoffset = (CentreY - DisplY / 2);
				cx2 = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
				cy2 = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);
				Rect.left = DrawWindowMinX;
				Rect.right = DrawWindowMaxX;
				Rect.top = DrawWindowMinY;
				Rect.bottom = DrawWindowMaxY;
				ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
				ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
				ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
				ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
			}

			CheckZoomFactor();
			DrawCrossHair(2);
			InvalidateRect(SCHWindow, &Rect, 0);
			UpdateWindow(SCHWindow);
		}
	}

	SelectionEsc = 0;
//  SystemBusyMode=0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawXorWindow3(int32 x1, int32 y1, int32 width, int32 height, int32 mode)
{
	StartDrawingEditingWindow();
	InitDrawingColorWhite();
	SetROP2(OutputDisplay, R2_XORPEN);
	rect3(x1, y1, width, height);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void PanWindow()
{
	double hulp, CentreX, CentreY, DisplayX, DisplayY;
	int32 mode, OldX2, OldY2, width, height, cx, cy, cx2, cy2;
	RECT Rect;

	cx = (DrawWindowMaxX + DrawWindowMinX) / 2;
	cy = (DrawWindowMaxY + DrawWindowMinY) / 2;
	CentreX = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
	CentreY = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);

	if (Design.BoardWidth * DisplY / DisplX > Design.BoardHeight)
	{
		DisplayX = (DrawWindowMaxX - DrawWindowMinX);
		DisplayX *= 0.75;
		DisplayY = DisplayX * Design.BoardHeight / Design.BoardWidth;
	}
	else
	{
		DisplayX = (DrawWindowMaxX - DrawWindowMinX);
		DisplayX *= 0.75;
		DisplayY = (DisplayX * DisplY / DisplX);
		DisplayX = (DisplayY * Design.BoardWidth / Design.BoardHeight);
	}

	hulp = ((CentreX - Design.BoardCentreX) / Design.BoardWidth) * DisplayX;
	cx2 = (int32) hulp;
	hulp = ((CentreY - Design.BoardCentreY) / Design.BoardHeight) * DisplayY;
	cy2 = (int32) hulp;


	MousePosX = (cx + cx2) + DrawWindowMinX;
	MousePosY = (cy - cy2);
	SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY);

	DrawXorWindow3(cx + DrawWindowMinX, cy, (int32) DisplayX, (int32) DisplayY, 0);

	SelectRectX1 = MousePosX;
	SelectRectX2 = MousePosX;
	SelectRectY1 = MousePosY;
	SelectRectY2 = MousePosY;
	OldX2 = SelectRectX2;
	OldY2 = SelectRectY2;

	if (CurrentDrawMode == 0)
		CurrentDrawMode = 1;

	SelectionEsc = 0;
	mode = 0;
//  SystemBusyMode=300;

	width = (int32) (DisplayX * (DisplX / Design.BoardWidth));
	height = (int32) (width * DisplY / DisplX);
	DrawXorWindow3(MousePosX, MousePosY, width, height, mode);
	ClipMouseCursor();

	while (PanActive())
	{
		if (!SelectionEsc)
		{
			if (MouseChanged)
			{
				DrawXorWindow3(OldX2, OldY2, width, height, mode);
				OldX2 = MousePosX;
				OldY2 = MousePosY;
				SelectRectX2 = MousePosX;
				SelectRectY2 = MousePosY;
				DrawXorWindow3(MousePosX, MousePosY, width, height, mode);

				DisplayCursorPosition();
				MouseChanged = 0;
			}

			CheckInputMessages(0);

			if (!Focused)
			{
				DrawXorWindow2(SelectRectX1, SelectRectY1, OldX2, OldY2, mode);
				UnClipMouseCursor();
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				SelectionEsc = 1;
			}

			if (NrFunctionsInBuf > 0)
			{
				DrawXorWindow3(OldX2, OldY2, width, height, mode);
				UnClipMouseCursor();
				ExecuteKeys();
				CheckInputMessages(0);
				CheckInputMessages(0);

				if (HelpAsked)
				{
					Help("window_based_panning.htm", 0);
					CheckInputMessages(0);

					while (!Focused)
						CheckInputMessages(0);

					CheckInputMessages(0);
					HelpAsked = 0;
					SelectionEsc = 1;
				}

				if (!SelectionEsc)
					DrawXorWindow3(OldX2, OldY2, width, height, mode);
			}
		}
		else
			CheckInputMessages(0);
	}

	UnClipMouseCursor();

	if (!SelectionEsc)
	{
		DrawXorWindow3(MousePosX, MousePosY, width, height, mode);
		hulp = (MousePosX - DrawWindowMinX - (cx - (DisplayX / 2)));
		Xoffset =
		    ((hulp * Design.BoardWidth / DisplayX) - (DisplX * 0.5) + Design.BoardCentreX - Design.BoardWidth / 2);
		hulp = (cy + (DisplayY / 2) - MousePosY);
		Yoffset =
		    ((hulp * Design.BoardHeight / DisplayY) - (DisplY * 0.5) + Design.BoardCentreY - Design.BoardHeight / 2);
//    DisplX=PixelToRealOffX(DrawWindowMaxX)-PixelToRealOffX(DrawWindowMinX);
//    DisplY=PixelToRealOffY(DrawWindowMaxY)-PixelToRealOffY(DrawWindowMinY);
		Rect.left = DrawWindowMinX;
		Rect.right = DrawWindowMaxX;
		Rect.top = DrawWindowMinY;
		Rect.bottom = DrawWindowMaxY;
		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		DrawCrossHair(2);
		InvalidateRect(SCHWindow, &Rect, 0);
		UpdateWindow(SCHWindow);
	}

	SelectionEsc = 0;
//  SystemBusyMode=0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawButtonInfoOff(int32 mode)
{
	if (DisplayButtonNr)
	{
		DrawCrossHair(2);
		InvalidateRect(SCHWindow, &ButtonRect, 0);
		UpdateWindow(SCHWindow);
		DisplayButtonNr = 0;
	}

	if (mode == 1)
		NrButtonInfo = -1;

	if (mode == 2)
		OldButtonNr = -1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ButtonInfo()
{
	int32 NewButtonNr, val, NrButtonsTemp;
	HGDIOBJ SaveFont;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	SIZE NewTextSize;

// String
	if (FirstPaint)
		return;

	val = (MousePosY - 4) / ButtonSizeY;

	if (!EditingSymbol)
		NrButtonsTemp = NrButtonsSheet;
	else
		NrButtonsTemp = NrButtonsSymbol;

	if ((val >= 0) && (val < NrButtonsTemp))
	{
		NewButtonNr = val;

		if (NewButtonNr != NrButtonInfo)
		{
			DrawButtonInfoOff(2);
			NrButtonInfo = NewButtonNr;
			TimeOut = TimerValue;
		}
		else
		{
			if (!DisplayButtonNr)
			{
				if ((OldButtonNr != NewButtonNr) && (TimerValue > TimeOut + 5))
				{
					TimeOut = TimerValue;
					DrawButtonInfoOff(0);
					DisplayButtonNr = 1;
					StartDrawingEditingWindow();
					OldButtonNr = NewButtonNr;
					InitDrawingButtonInfo();
					SetTextColor(OutputDisplay, SCHColors[BackGroundColorNr]);
					SetBkMode(OutputDisplay, TRANSPARENT);
					SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));

					if (!EditingSymbol)
					{
						switch (val)
						{
						case 0:
							strcpy(str, SC(231, "New sheet"));

							if (GetKeyString(str2, ID_FILE_NEW_SHEET, 0x21) == 1)
								strcat(str, str2);

							break;

						case 1:
							strcpy(str, SC(232, "Open sheet"));

							if (GetKeyString(str2, ID_FILE_OPEN, 0x21) == 1)
								strcat(str, str2);

							break;

						case 2:
							strcpy(str, SC(233, "Save sheet"));

							if (GetKeyString(str2, ID_FILE_SAVE, 0x21) == 1)
								strcat(str, str2);

							break;

						case 3:
							strcpy(str, SC(234, "Print using default printer"));

							if (GetKeyString(str2, ID_FILE_PRINT, 0x21) == 1)
								strcat(str, str2);

							break;

						case 4:
							strcpy(str, SC(235, "Add wire"));

							if (GetKeyString(str2, ID_ADDWIRE2, 0x21) == 1)
								strcat(str, str2);

							break;

						case 5:
							strcpy(str, SC(236, "Add bus"));

							if (GetKeyString(str2, ID_ADDBUS2, 0x21) == 1)
								strcat(str, str2);

							break;

						case 6:
							strcpy(str, SC(109, "Add netlabel to wire/bus"));

							if (GetKeyString(str2, ID_ADDNETLABEL, 0x21) == 1)
								strcat(str, str2);

							break;

						case 7:
							strcpy(str, SC(307, "Add bus connection"));

							if (GetKeyString(str2, ID_ADDBUSCONNECTION, 0x21) == 1)
								strcat(str, str2);

							break;

						case 8:
							strcpy(str, SC(238, "Add external output connection"));

							if (GetKeyString(str2, ID_ADDGLOBALCONNECTION_O, 0x21) == 1)
								strcat(str, str2);

							break;

						case 9:
							strcpy(str, SC(239, "Add external input connection"));

							if (GetKeyString(str2, ID_ADDGLOBALCONNECTION_I, 0x21) == 1)
								strcat(str, str2);

							break;

						case 10:
							strcpy(str, SC(240, "Add external Input/Output connection"));

							if (GetKeyString(str2, ID_ADDGLOBALCONNECTION_IO, 0x21) == 1)
								strcat(str, str2);

							break;

						case 11:
							strcpy(str, SC(242, "Add symbol"));

							if (GetKeyString(str2, ID_ADDSYMBOL, 0x21) == 1)
								strcat(str, str2);

							break;

						case 12:
							strcpy(str, SC(243, "Add database component"));

							if (GetKeyString(str2, ID_ADDCOMPONENT, 0x21) == 1)
								strcat(str, str2);

							break;

						case 13:
							strcpy(str, SC(244, "Undo"));

							if (GetKeyString(str2, ID_EDIT_UNDO, 0x21) == 1)
								strcat(str, str2);

							break;

						case 14:
							strcpy(str, SC(245, "Redo"));

							if (GetKeyString(str2, ID_EDIT_REDO, 0x21) == 1)
								strcat(str, str2);

							break;

						case 15:
							strcpy(str, SC(246, "Copy"));

							if (GetKeyString(str2, ID_COPY_OBJECTS, 0x21) == 1)
								strcat(str, str2);

							break;

						case 16:
							strcpy(str, SC(247, "Delete"));

							if (GetKeyString(str2, ID_EDIT_DELETE, 0x21) == 1)
								strcat(str, str2);

							break;

						case 17:
							strcpy(str, SC(248, "Move"));

							if (GetKeyString(str2, ID_MOVE_OBJECTS, 0x21) == 1)
								strcat(str, str2);

							break;

						case 18:
							strcpy(str, SC(249, "Drag"));

							if (GetKeyString(str2, ID_DRAG_OBJECTS, 0x21) == 1)
								strcat(str, str2);

							break;

						case 19:
							strcpy(str, SC(250, "Open subsheet"));

							if (GetKeyString(str2, ID_FILE_EDIT_SHEET, 0x21) == 1)
								strcat(str, str2);

							break;

						case 20:
							strcpy(str, SC(251, "Open sheet symbol"));

							if (GetKeyString(str2, ID_FILE_EDIT_SYMBOL, 0x21) == 1)
								strcat(str, str2);

							break;

						case 21:
							strcpy(str, SC(252, "Goto higher sheet"));

							if (GetKeyString(str2, ID_FILE_SHEETBACK, 0x21) == 1)
								strcat(str, str2);

							break;

						case 22:
							strcpy(str, SC(418, "Unselect all"));

							if (GetKeyString(str2, ID_UNSELECT_ALL, 0x21) == 1)
								strcat(str, str2);

							break;
						}
					}
					else
					{
						switch (val)
						{
						case 0:
							if (EditingSheetSymbol)
							{
								strcpy(str, SC(254, "New sheet symbol"));

								if (GetKeyString(str2, ID_FILE_NEW_SHEETSYMBOL, 0x21) == 1)
									strcat(str, str2);
							}
							else
							{
								strcpy(str, SC(255, "New symbol"));

								if (GetKeyString(str2, ID_FILE_NEW_SYMBOL, 0x21) == 1)
									strcat(str, str2);
							}

							break;

						case 1:
							strcpy(str, SC(256, "Open symbol"));
							
							if (GetKeyString(str2, ID_FILE_OPEN, 0x21) == 1)
								strcat(str, str2);

							break;

						case 2:
							strcpy(str, SC(257, "Save symbol"));

							if (GetKeyString(str2, ID_FILE_SAVE, 0x21) == 1)
								strcat(str, str2);

							break;

						case 3:
							strcpy(str, SC(234, "Print using default printer"));

							if (GetKeyString(str2, ID_FILE_PRINT, 0x21) == 1)
								strcat(str, str2);

							break;

						case 4:
							strcpy(str, SC(57, "Add pin"));

							if (GetKeyString(str2, ID_ADDPIN, 0x21) == 1)
								strcat(str, str2);

							break;

						case 5:
							strcpy(str, SC(81, "Add pin bus"));

							if (GetKeyString(str2, ID_ADDPINBUS, 0x21) == 1)
								strcat(str, str2);

							break;

						case 6:
							strcpy(str, SC(72, "Add power pin"));

							if (GetKeyString(str2, ID_ADDPOWERPIN, 0x21) == 1)
								strcat(str, str2);

							break;

						case 7:
							strcpy(str, SC(260, "Add line"));

							if (GetKeyString(str2, ID_ADDLINE, 0x21) == 1)
								strcat(str, str2);

							break;

						case 8:
							strcpy(str, SC(261, "Add rectangle"));

							if (GetKeyString(str2, ID_ADDRECT2, 0x21) == 1)
								strcat(str, str2);

							break;

						case 9:
							strcpy(str, SC(262, "Add circle"));

							if (GetKeyString(str2, ID_ADDCIRCLE, 0x21) == 1)
								strcat(str, str2);

							break;

						case 10:
							strcpy(str, SC(263, "Add arc"));

							if (GetKeyString(str2, ID_ADDARC, 0x21) == 1)
								strcat(str, str2);

							break;

						case 11:
							strcpy(str, SC(23, "Add text"));

							if (GetKeyString(str2, ID_ADDTEXT, 0x21) == 1)
								strcat(str, str2);

							break;

						case 12:
							strcpy(str, SC(244, "Undo"));

							if (GetKeyString(str2, ID_EDIT_UNDO, 0x21) == 1)
								strcat(str, str2);

							break;

						case 13:
							strcpy(str, SC(245, "Redo"));

							if (GetKeyString(str2, ID_EDIT_REDO, 0x21) == 1)
								strcat(str, str2);

							break;

						case 14:
							strcpy(str, SC(246, "Copy"));

							if (GetKeyString(str2, ID_COPY_OBJECTS, 0x21) == 1)
								strcat(str, str2);

							break;

						case 15:
							strcpy(str, SC(247, "Delete"));

							if (GetKeyString(str2, ID_EDIT_DELETE, 0x21) == 1)
								strcat(str, str2);

							break;

						case 16:
							strcpy(str, SC(248, "Move"));

							if (GetKeyString(str2, ID_MOVE_OBJECTS, 0x21) == 1)
								strcat(str, str2);

							break;

						case 17:
							strcpy(str, SC(249, "Drag"));

							if (GetKeyString(str2, ID_DRAG_OBJECTS, 0x21) == 1)
								strcat(str, str2);

							break;

						case 18:
							strcpy(str, SC(418, "Unselect all"));

							if (GetKeyString(str2, ID_UNSELECT_ALL, 0x21) == 1)
								strcat(str, str2);

							break;

						case 19:
							strcpy(str, SC(264, "Goto above sheet"));

							if (GetKeyString(str2, ID_FILE_SHEETBACK, 0x21) == 1)
								strcat(str, str2);

							break;
						}
					}

					GetTextExtentPoint32(OutputDisplay, str, strlen(str), &NewTextSize);
					ButtonRect.left = 40;
					ButtonRect.right = 40 + NewTextSize.cx + 8;
					ButtonRect.top = NrButtonInfo * ButtonSizeY + 2;
					ButtonRect.bottom = NrButtonInfo * ButtonSizeY + 12 + NewTextSize.cy;
					RoundRect(OutputDisplay, ButtonRect.left, ButtonRect.top, ButtonRect.right, ButtonRect.bottom, 5, 5);
					TextOutUTF8(OutputDisplay, ButtonRect.left + 4, ButtonRect.top + 4, str, strlen(str));
					SelectObject(OutputDisplay, SaveFont);
					ExitDrawing();
					EndDrawingEditingWindow();
				}
			}
			else
			{
				if (TimerValue > TimeOut + ButtonInfoTimeout)
					DrawButtonInfoOff(0);
			}
		}
	}
	else
		DrawButtonInfoOff(1);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CheckButtonPressed(int32 mode)
{
	int32 val, res, NrButtonsTemp;
	HGDIOBJ old;
	HPEN DrawPen;
	HBITMAP ButtonBitmap;
	HDC ButtonMemoryDC;
	POINT PP;

	val = (MousePosY - 4) / ButtonSizeY;

	if (!EditingSymbol)
		NrButtonsTemp = NrButtonsSheet;
	else
		NrButtonsTemp = NrButtonsSymbol;

	if ((val >= 0) && (val < NrButtonsTemp))
	{
		if (mode == 0)
		{
			StartDrawingEditingWindow();

			if (!EditingSymbol)
				ButtonBitmap = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTON_SCH));
			else
				ButtonBitmap = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTON_SYM));

			ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
			old = SelectObject(ButtonMemoryDC, ButtonBitmap);
			SelectClipRgn(OutputDisplay, NULL);
			res =
			    BitBlt(OutputDisplay, 5, ButtonSizeY * val + 5, ButtonSizeX - 4, ButtonSizeY - 3, ButtonMemoryDC, 3,
			           ButtonSizeY * val + 3, SRCCOPY);
			SelectObject(ButtonMemoryDC, old);
			DeleteDC(ButtonMemoryDC);
			DeleteObject(ButtonBitmap);
			SelectObject(OutputDisplay, GetStockObject(BLACK_PEN));
			MoveToEx(OutputDisplay, 3, ButtonSizeY * val + 3, &PP);
			LineTo(OutputDisplay, ButtonSizeX + 1, ButtonSizeY * val + 3);
			MoveToEx(OutputDisplay, 3, ButtonSizeY * val + 3, &PP);
			LineTo(OutputDisplay, 3, ButtonSizeY * (val + 1) + 2);

			DrawPen = CreatePen(PS_SOLID, 1, RGB(128, 128, 128));
			old = SelectObject(OutputDisplay, DrawPen);
			MoveToEx(OutputDisplay, 4, ButtonSizeY * val + 4, &PP);
			LineTo(OutputDisplay, ButtonSizeX + 1, ButtonSizeY * val + 4);
			MoveToEx(OutputDisplay, 4, ButtonSizeY * val + 4, &PP);
			LineTo(OutputDisplay, 4, ButtonSizeY * (val + 1) + 1);
			SelectObject(OutputDisplay, old);

			SelectObject(OutputDisplay, GetStockObject(WHITE_PEN));
			MoveToEx(OutputDisplay, 3, ButtonSizeY * (val + 1) + 2, &PP);
			LineTo(OutputDisplay, ButtonSizeX + 1, ButtonSizeY * (val + 1) + 2);
			MoveToEx(OutputDisplay, ButtonSizeX + 1, ButtonSizeY * val + 4, &PP);
			LineTo(OutputDisplay, ButtonSizeX + 1, ButtonSizeY * (val + 1) + 3);

			SelectClipRgn(OutputDisplay, EditingRegion);
			DeleteObject(DrawPen);
			ExitDrawing();
			EndDrawingEditingWindow();
			return;
		}

		StartDrawingEditingWindow();

		if (!EditingSymbol)
			ButtonBitmap = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTON_SCH));
		else
			ButtonBitmap = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTON_SYM));

		ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
		old = SelectObject(ButtonMemoryDC, ButtonBitmap);
		SelectClipRgn(OutputDisplay, NULL);
		res =
		    BitBlt(OutputDisplay, 1, ButtonSizeY * val + 1, ButtonSizeX + 1, ButtonSizeY + 2, ButtonMemoryDC, 0,
		           ButtonSizeY * val, SRCCOPY);
		SelectObject(ButtonMemoryDC, old);
		DeleteDC(ButtonMemoryDC);
		DeleteObject(ButtonBitmap);
		SelectClipRgn(OutputDisplay, EditingRegion);
		ExitDrawing();
		EndDrawingEditingWindow();

		if (!EditingSymbol)
		{
			switch (val)
			{
			case 0:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_NEW_SHEET, (LPARAM) NULL);
				break;

			case 1:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_OPEN, (LPARAM) NULL);
				break;

			case 2:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_SAVE, (LPARAM) NULL);
				break;

			case 3:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_PRINT, (LPARAM) NULL);
//          PostMessage(SCHWindow,WM_COMMAND,(WPARAM)ID_FILE_PRINT_DEFAULT,(LPARAM)NULL);
				break;

			case 4:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDWIRE2, (LPARAM) NULL);
				break;

			case 5:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDBUS2, (LPARAM) NULL);
				break;

			case 6:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDNETLABEL, (LPARAM) NULL);
				break;

			case 7:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDBUSCONNECTION, (LPARAM) NULL);
				break;

			case 8:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDGLOBALCONNECTION_O, (LPARAM) NULL);
				break;

			case 9:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDGLOBALCONNECTION_I, (LPARAM) NULL);
				break;

			case 10:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDGLOBALCONNECTION_IO, (LPARAM) NULL);
				break;

			case 11:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDSYMBOL, (LPARAM) NULL);
				break;

			case 12:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDCOMPONENT, (LPARAM) NULL);
				break;

			case 13:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_UNDO, (LPARAM) NULL);
				break;

			case 14:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_REDO, (LPARAM) NULL);
				break;

			case 15:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_COPY_OBJECTS, (LPARAM) NULL);
				break;

			case 16:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_DELETE, (LPARAM) NULL);
				break;

			case 17:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_MOVE_OBJECTS, (LPARAM) NULL);
				break;

			case 18:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_DRAG_OBJECTS, (LPARAM) NULL);
				break;

			case 19:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_EDIT_SHEET, (LPARAM) NULL);
				break;

			case 20:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_EDIT_SYMBOL, (LPARAM) NULL);
				break;

			case 21:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_SHEETBACK, (LPARAM) NULL);
				break;

			case 22:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_UNSELECT_ALL, (LPARAM) NULL);
				break;
			}
		}
		else
		{
			switch (val)
			{
			case 0:
				if (EditingSheetSymbol)
					PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_NEW_SHEETSYMBOL, (LPARAM) NULL);
				else
					PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_NEW_SYMBOL, (LPARAM) NULL);

				break;

			case 1:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_OPEN, (LPARAM) NULL);
				break;

			case 2:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_SAVE, (LPARAM) NULL);
				break;

			case 3:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_PRINT, (LPARAM) NULL);
//          PostMessage(SCHWindow,WM_COMMAND,(WPARAM)ID_FILE_PRINT_DEFAULT,(LPARAM)NULL);
				break;

			case 4:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDPIN, (LPARAM) NULL);
				break;

			case 5:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDPINBUS, (LPARAM) NULL);
				break;

			case 6:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDPOWERPIN, (LPARAM) NULL);
				break;

			case 7:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDLINE, (LPARAM) NULL);
				break;

			case 8:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDRECT2, (LPARAM) NULL);
				break;

			case 9:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDCIRCLE, (LPARAM) NULL);
				break;

			case 10:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDARC, (LPARAM) NULL);
				break;

			case 11:
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_ADDTEXT, (LPARAM) NULL);
				break;

			case 12:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_UNDO, (LPARAM) NULL);
				break;

			case 13:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_REDO, (LPARAM) NULL);
				break;

			case 14:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_COPY_OBJECTS, (LPARAM) NULL);
				break;

			case 15:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_DELETE, (LPARAM) NULL);
				break;

			case 16:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_MOVE_OBJECTS, (LPARAM) NULL);
				break;

			case 17:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_DRAG_OBJECTS, (LPARAM) NULL);
				break;

			case 18:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_UNSELECT_ALL, (LPARAM) NULL);
				break;

			case 19:
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_SHEETBACK, (LPARAM) NULL);
				break;
			}
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 DisplayInfoOff(int32 mode)
{
	if (DisplayInfoStr[0] == 0)
		return 0;

	if (TimeOutInfo == -1)
		return 0;

	DrawCrossHair(2);
	InvalidateRect(SCHWindow, &InfoRect, 0);
	UpdateWindow(SCHWindow);
	DisplayInfoStr[0] = 0;
	TimeOutInfo = -1;
	DisplayInfoTimeOut = -1;
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 NewDisplayInfo(LPSTR InfoStr, int32 x, int32 y)
{
	HGDIOBJ SaveFont;
	SIZE NewTextSize = { 0 };
	char str[2048], *strp[30], lastchar = 0;
	int32 lengte, cnt, NrLines, MaxX, MaxY, ox, oy;

	/*
	  if (DisplayInfoStr[0]!=0) {
	    DisplayInfoOff(0);
	    return 0;
	  }
	*/
	TimeOutInfo = TimerValue;

	DrawCrossHair(2);

	StartDrawingEditingWindow();
	InitDrawingButtonInfo();
	SetTextColor(OutputDisplay, SCHColors[BackGroundColorNr]);
	SetBkMode(OutputDisplay, TRANSPARENT);
	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));

	MaxX = 0;
	MaxY = 0;
	NrLines = 0;
	cnt = 0;
	strp[NrLines] = &str[0];
	strcpy(str, InfoStr);
	lengte = min(2000, (int) strlen(str));

	while (cnt < lengte)
	{
		lastchar = str[cnt];

		if (str[cnt] == '\r')
			str[cnt] = 0;

		if (str[cnt] == '\n')
		{
			str[cnt] = 0;

			if (NrLines < 29)
				strp[++NrLines] = &str[cnt + 1];
		}

		cnt++;
	}

	if (lastchar != '\n')
		NrLines++;

	for (cnt = 0; cnt < NrLines; cnt++)
	{
		GetTextExtentPoint32(OutputDisplay, strp[cnt], strlen(strp[cnt]), &NewTextSize);
		MaxX = max(NewTextSize.cx, MaxX);
		MaxY = max(NewTextSize.cy, MaxY);
	}

	MaxY += 3;
	InfoRect.left = x + 10;
	InfoRect.right = x + 10 + MaxX + 8;
	InfoRect.top = y + 8;
	InfoRect.bottom = InfoRect.top + MaxY * NrLines + 8 + NewTextSize.cy;
	RoundRect(OutputDisplay, InfoRect.left, InfoRect.top, InfoRect.right, InfoRect.bottom, 5, 5);
	SelectObject(OutputDisplay, BackGroundPen);

	for (cnt = 0; cnt < NrLines; cnt++)
	{
		if (cnt == 0)
		{
			GetTextExtentPoint32(OutputDisplay, strp[cnt], strlen(strp[cnt]), &NewTextSize);
			ox = (MaxX - NewTextSize.cx) / 2;
			TextOutUTF8(OutputDisplay, InfoRect.left + 4 + ox, InfoRect.top + 4 + MaxY * cnt, strp[cnt], strlen(strp[cnt]));
		}
		else
		{
			if (strcmp(strp[cnt], "\f") == 0)
			{
				oy = InfoRect.top + 8 + MaxY * cnt;
				DrawLine(InfoRect.left + 1, oy, InfoRect.right - 1, oy);
			}
			else
				TextOutUTF8(OutputDisplay, InfoRect.left + 4, InfoRect.top + 4 + MaxY * cnt, strp[cnt], strlen(strp[cnt]));
		}
	}

	SelectObject(OutputDisplay, SaveFont);
	ExitDrawing();
	EndDrawingEditingWindow();
	DrawCrossHair(2);
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 DisplayInfo(int32 mode)
{
	int32 result, ok;

	if (!PopupDisplayVisible)
		return 0;

//  if (!EditingSymbol) return 0;

	if (TimeOutInfo != -1)
		ok = 1;

	result = GetInfoStr(DisplayInfoStr, 0);

	if ((result == 1) || (result == 3))
	{
		if (DisplayInfoStr[0] != 0)
		{
			if (DisplayInfoTimeOut == -1)
				DisplayInfoTimeOut = TimerValue;
			else
			{
				if (TimerValue - DisplayInfoTimeOut > ButtonInfoTimeoutStart)
				{
					if (TimeOutInfo == -1)
						NewDisplayInfo(DisplayInfoStr, MousePosX, MousePosY);
				}
			}
		}

		ok = 1;
	}
	else
	{
		DisplayInfoTimeOut = -1;
		DisplayInfoOff(0);
	}

	if ((TimeOutInfo != -1) && (TimerValue > TimeOutInfo + ButtonInfoTimeout))
	{
		DisplayInfoOff(0);
		GetInfoStr(NULL, 1);
		return 1;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
