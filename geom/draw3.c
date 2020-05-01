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
#include "geom.h"
#include "calc.h"
#include "calcdef.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "mainloop.h"
#include "files.h"
#include "edit.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "help.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "movecomp.h"
#include "graphics.h"
#include "resource.h"
#include "resource2.h"

#define  BUTTON_INFO_TIMEOUT  40

double NewFactor;

int32 TimeOut, OldButtonNr, ok;
int32 DisplayButtonNr = 0;
RECT ButtonRect;
int32 NrButtonInfo = -1;
int32 NrButtons = 21;
int32 ButtonSizeX = 27;
int32 ButtonSizeY = 22;

int32 FirstOutZoom;

// *******************************************************************************************************
// *******************************************************************************************************

extern HDC OutputDisplay;
extern int32 TimerValue;
extern HGDIOBJ PinTextFont;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawXorWindow(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	int32 xmin, xmax, ymin, ymax;

	StartDrawingEditingWindow();
	InitDrawingColorWhite(0);
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawXorWindow2(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	double hulp, CentreX, CentreY, DisplayX, DisplayY, m2, BoardCentreX, BoardCentreY;
	int32 x3, y3, x4, y4, difx, cx, cy, cx2, cy2, xmin, xmax, ymin, ymax;

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
	StartDrawingEditingWindow();
	InitDrawingColorWhite(0);

	if ((x1 <= x2) || (y1 <= y2))
	{
		DisplayX = (DrawWindowMaxX - DrawWindowMinX);
		DisplayX *= 0.75;
		difx = x1 - x2;

		if (BoardWidth * DisplY / DisplX > BoardHeight)
			DisplayY = DisplayX * BoardHeight / BoardWidth;
		else
		{
			DisplayY = (DisplayX * DisplY / DisplX);
			DisplayX = (DisplayY * BoardWidth / BoardHeight);
		}

		SetROP2(OutputDisplay, R2_XORPEN);
		rect2(xmin, ymin, abs(x2 - x1) + 1, abs(y2 - y1) + 1);
	}
	else
	{
		DisplayX = (DrawWindowMaxX - DrawWindowMinX);
		DisplayX *= 0.75;
		difx = x1 - x2;

		if (BoardWidth * DisplY / DisplX > BoardHeight)
			DisplayY = DisplayX * BoardHeight / BoardWidth;
		else
		{
			DisplayY = (DisplayX * DisplY / DisplX);
			DisplayX = (DisplayY * BoardWidth / BoardHeight);
		}

		hulp = (DisplX / BoardWidth * DisplayX);
		x3 = (int32) hulp + difx;
		m2 = x3 / hulp;
		NewFactor = Factor / m2;
		hulp = x3;
		hulp = (hulp * (DisplY / DisplX));

        //BoardCentreX=BoardWidth*0.5+BoardOX;
        //BoardCentreY=BoardHeight*0.5+BoardOY;

		BoardCentreX = BoardOX;
		BoardCentreY = BoardOY;

		y3 = (int32) hulp;
		hulp = ((CentreX - BoardCentreX) / BoardWidth) * DisplayX;
		x4 = (int32) hulp;
		hulp = ((CentreY - BoardCentreY) / BoardHeight) * DisplayY;
		y4 = (int32) hulp;

     //if (FirstOutZoom) {
     //DrawTempObjects(DisplayX,DisplayY);
     //}

		if (FirstOutZoom)
		{
			FirstOutZoom = 0;
			rect3(cx2, cy2, (int32) DisplayX, (int32) DisplayY);
		}

		SetROP2(OutputDisplay, R2_XORPEN);
		rect3(cx2 + x4, cy2 - y4, x3, y3);
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ViewWholeDesign(int32 mode)
{
#define  OVERSIZE  1.1

	double hulpx, hulpy, cx, cy, divx, divy;
	int32 ok;

	FindMinMaxBoard(&BoardOX, &BoardOY, &BoardWidth, &BoardHeight, 0);

	divx = VisibleMaxX - VisibleMinX;
	divy = VisibleMaxY - VisibleMinY;
	cx = (VisibleMaxX + VisibleMinX) * 0.5;
	cy = (VisibleMaxY + VisibleMinY) * 0.5;
	divx = divx * OVERSIZE;
	divy = divy * OVERSIZE;
	hulpx = divx / ClientWindowDivX;
	hulpy = divy / ClientWindowDivY;

	if (hulpx > hulpy)
	{
		hulpx = ClientWindowDivX;
		Factor = hulpx / divx;

		if ((mode == 1) && (Factor > 0.001))
			Factor = 0.001;

		DisplX = divx;
		DisplY = DisplX * ClientWindowDivY / ClientWindowDivX;
		Xoffset = (cx - DisplX * 0.5);
		Yoffset = (cy - DisplY * 0.5);
		ok = 1;
	}
	else
	{
		hulpy = ClientWindowDivY;
		Factor = hulpy / divy;

		if ((mode == 1) && (Factor > 0.001))
			Factor = 0.001;

		DisplY = divy;
		DisplX = DisplY * ClientWindowDivX / ClientWindowDivY;
		Xoffset = (cx - DisplX * 0.5);
		Yoffset = (cy - DisplY * 0.5);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ZoomWindow()
{
	double ZoomMinX, ZoomMinY, ZoomMaxX, ZoomMaxY, hulp, cx, cy, PixelsX, PixelsY, CentreX, CentreY, cx2, cy2;
	int32 OldX2, OldY2, mode;
	RECT Rect;

	SelectRectX1 = MousePosX;
	SelectRectX2 = MousePosX;
	SelectRectY1 = MousePosY;
	SelectRectY2 = MousePosY;
	OldX2 = SelectRectX2;
	OldY2 = SelectRectY2;

    //if (CurrentDrawMode==0) CurrentDrawMode=1;

	FindMinMaxBoard(&BoardOX, &BoardOY, &BoardWidth, &BoardHeight, 0);

	/*
	  BoardWidth=Design.BoardWidth;
	  BoardOX=0;
	  BoardHeight=Design.BoardHeight;
	  BoardOY=0;
	*/

    //BoardWidth-=BoardOX;
    //BoardHeight-=BoardOY;

	SelectionActive = 1;
	SelectionEsc = 0;
	mode = 0;
	FirstOutZoom = 1;
	ClipMouseCursor();
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
			UnClipMouseCursor();
			DrawXorWindow2(SelectRectX1, SelectRectY1, OldX2, OldY2, mode);
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

	SelectionActive = 0;

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

					/*
					          if ((Factor>(10/sqrt(2.0)/DefFontSize))
					             &&
					             (Factor<20*sqrt(2.0)/DefFontSize)) {
					            if (Factor>(10*sqrt(2.0)/DefFontSize)) Factor=20/DefFontSize;
					            else Factor=10/DefFontSize;
					          }
					          if ((Factor>7.5)
					             &&
					             (Factor<28.0)) {
					            if (Factor>14.0) Factor=20.0;
					            else Factor=10.0;
					          }
				   */

					ViewMinY = PixelToRealOffY(-1);
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

					/*
					   if ((Factor>7.5)
					   &&
					   (Factor<28.0)) {
					   if (Factor>14.0) Factor=20.0;
					   else Factor=10.0;
					   }

					   if ((Factor>9.0)
					   &&
					   (Factor<35.0)) {
					   if (Factor>17.0) Factor=25.0;
					   else Factor=12.5;
					   }

					   if ((Factor>(10/sqrt(2.0)/DefFontSize))
					   &&
					   (Factor<20*sqrt(2.0)/DefFontSize)) {
					   if (Factor>(10*sqrt(2.0)/DefFontSize)) Factor=20/DefFontSize;
					   else Factor=10/DefFontSize;
					   }
				   */

					ViewMinX = PixelToRealOffX(-1);
					ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);

					Xoffset = cx - (ViewMaxX - ViewMinX) / 2;
				}

				DisplX = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
				DisplY = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);

				//DisplX=(ZoomMaxX-ZoomMinX);
				//DisplY=DisplX*DrawWindowMaxY/DrawWindowMaxX;

				Rect.left = DrawWindowMinX;
				Rect.right = DrawWindowMaxX;
				Rect.top = DrawWindowMinY;
				Rect.bottom = DrawWindowMaxY;
				ViewMinX = PixelToRealOffX(-1);
				ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
				ViewMinY = PixelToRealOffY(-1);
				ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
			}
			else
			{

				CentreX = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
				CentreY = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);
				Factor = NewFactor;

				/*
				        if ((Factor>(10/sqrt(2.0)/DefFontSize))
				           &&
				           (Factor<20*sqrt(2.0)/DefFontSize)) {
				          if (Factor>(10*sqrt(2.0)/DefFontSize)) Factor=20/DefFontSize;
				          else Factor=10/DefFontSize;
				        }
			   */

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
				ViewMinX = PixelToRealOffX(-1);
				ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
				ViewMinY = PixelToRealOffY(-1);
				ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
			}

			DrawCrossHair(2);
			InvalidateRect(GEOMWindow, &Rect, 0);
			UpdateWindow(GEOMWindow);
		}
	}

	SelectionEsc = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawXorWindow3(int32 x1, int32 y1, int32 width, int32 height, int32 mode)
{
	StartDrawingEditingWindow();
	InitDrawingColorWhite(0);
	SetROP2(OutputDisplay, R2_XORPEN);
	rect3(x1, y1, width, height);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PanWindow()
{
	double hulp, CentreX, CentreY, DisplayX, DisplayY, BoardCentreX, BoardCentreY;
	int32 OldX2, OldY2, width, height, cx, cy, cx2, cy2, mode;
	RECT Rect;

	cx = (DrawWindowMaxX + DrawWindowMinX) / 2;
	cy = (DrawWindowMaxY + DrawWindowMinY) / 2;
	CentreX = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
	CentreY = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);

	FindMinMaxBoard(&BoardOX, &BoardOY, &BoardWidth, &BoardHeight, 0);

    //BoardWidth-=BoardOX;
    //BoardHeight-=BoardOY;

	DisplayX = (DrawWindowMaxX - DrawWindowMinX);
	DisplayX *= 0.75;

	if (BoardWidth * DisplY / DisplX > BoardHeight)
		DisplayY = DisplayX * BoardHeight / BoardWidth;
	else
	{
		DisplayY = (DisplayX * DisplY / DisplX);
		DisplayX = (DisplayY * BoardWidth / BoardHeight);
	}

    //BoardCentreX=BoardWidth*0.5+BoardOX;
    //BoardCentreY=BoardHeight*0.5+BoardOY;

	BoardCentreX = BoardOX;
	BoardCentreY = BoardOY;
	hulp = ((CentreX - BoardCentreX) / BoardWidth) * DisplayX;
	cx2 = (int32) hulp;
	hulp = ((CentreY - BoardCentreY) / BoardHeight) * DisplayY;
	cy2 = (int32) hulp;

    //DrawTempObjects(DisplayX,DisplayY);

	MousePosX = cx + cx2 + DrawWindowMinX;
	MousePosY = cy - cy2;
	SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY);

	DrawCrossHair(2);
	StartDrawingEditingWindow();
	InitDrawingColorWhite(0);
	rect3(cx + DrawWindowMinX, cy, (int32) DisplayX, (int32) DisplayY);
	ExitDrawing();
	EndDrawingEditingWindow();

	SelectRectX1 = MousePosX;
	SelectRectX2 = MousePosX;
	SelectRectY1 = MousePosY;
	SelectRectY2 = MousePosY;
	OldX2 = SelectRectX2;
	OldY2 = SelectRectY2;

    //if (CurrentDrawMode==0) CurrentDrawMode=1;

	SelectionActive = 1;
	SelectionEsc = 0;
	mode = 0;

	width = (int32) (DisplayX * (DisplX / BoardWidth));
	height = (int32) (width * DisplY / DisplX);
	DrawXorWindow3(MousePosX, MousePosY, width, height, mode);

	while (RightButtonPressed)
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
				DrawXorWindow3(OldX2, OldY2, width, height, mode);
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

	SelectionActive = 0;

	if (!SelectionEsc)
	{
		DrawXorWindow3(MousePosX, MousePosY, width, height, mode);
		hulp = (MousePosX - DrawWindowMinX - (cx - (DisplayX / 2)));
		Xoffset = ((hulp * BoardWidth / DisplayX) - (DisplX * 0.5) + BoardCentreX - BoardWidth / 2);
		hulp = (cy + (DisplayY / 2) - MousePosY);
		Yoffset = ((hulp * BoardHeight / DisplayY) - (DisplY * 0.5) + BoardCentreY - BoardHeight / 2);

        //DisplX=PixelToRealOffX(DrawWindowMaxX)-PixelToRealOffX(DrawWindowMinX);
        //DisplY=PixelToRealOffY(DrawWindowMaxY)-PixelToRealOffY(DrawWindowMinY);

		Rect.left = DrawWindowMinX;
		Rect.right = DrawWindowMaxX;
		Rect.top = DrawWindowMinY;
		Rect.bottom = DrawWindowMaxY;
		ViewMinX = PixelToRealOffX(-1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(-1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		DrawCrossHair(2);
		InvalidateRect(GEOMWindow, &Rect, 0);
		DisplayCursorPosition();
		UpdateWindow(GEOMWindow);
	}

	SelectionEsc = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawWindow(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	int32 x3, y3, x4, y4;

	StartDrawingEditingWindow();
	InitDrawingColorWhite(0);
	SetROP2(OutputDisplay, R2_XORPEN);

	if (mode == 0)
	{
		rect2(x1, y1, x2 - x1 + 1, y2 - y1 + 1);

       //Rectangle(OutputDisplay,x1,y1,x2,y2);

	}
	else
	{
		MoveToEx(OutputDisplay, x1, y1, NULL);

		if (((x2 - x1 + y2 - y1) & 1) == 1)
			x2++;

		x3 = (x1 + y1 + x2 - y2) / 2;
		y3 = x3 - x2 + y2;
		x4 = (x1 - y1 + x2 + y2) / 2;
		y4 = x3 - x1 + y1;
		DrawLine(x1, y1, x2, y2);
		DrawLine(x2, y2, x3, y3);
		DrawLine(x3, y3, x4, y4);
		DrawLine(x4, y4, x1, y1);
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CenterScreen(double cx, double cy)
{
	RECT Rect;
	double DisplX, DisplY, hulp;

	hulp = (DrawWindowMaxX - DrawWindowMinX);
	hulp = hulp / 2000000.0;
	hulp = hulp * 0.7;
	Factor = min(0.001, hulp);

	DisplX = PixelToReal(DrawWindowMaxX - DrawWindowMinX);
	DisplY = PixelToReal(DrawWindowMaxY - DrawWindowMinY);
	Xoffset = cx - DisplX * 0.5;
	Yoffset = cy - DisplY * 0.5;
	Rect.left = DrawWindowMinX;
	Rect.right = DrawWindowMaxX;
	Rect.top = DrawWindowMinY;
	Rect.bottom = DrawWindowMaxY;
	ViewMinX = PixelToRealOffX(-1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(-1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

    //DrawCrossHair(7);
    //InvalidateRect(GEOMWindow,&Rect,0);
    //UpdateWindow(GEOMWindow);

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawMeasurementLine(double x1, double y1, double x2, double y2, int32 mode)
{
	double length, Xoff, Yoff, Value;
	char str[MAX_LENGTH_STRING];
	int32 x, y;

	if (mode == 0)
		return;

	StartDrawingEditingWindow();
	InitDrawingColorWhite(0);

	if (mode & 0x20)
		SetROP2(OutputDisplay, R2_COPYPEN);
	else
	{
		SetROP2(OutputDisplay, R2_XORPEN);

		/*
		      SetROP2(OutputDisplay,R2_NOT);
		      SetBkColor(OutputDisplay,RGB(0,0,0));
		      SetBkColor(OutputDisplay,RGB(255,255,255));
		      SetTextColor(OutputDisplay,RGB(255,255,255));
		      SetTextColor(OutputDisplay,RGB(0,0,0));
		      SetBkMode(OutputDisplay,OPAQUE);
		      SelectObject(OutputDisplay,PinTextFont);
	   */

	}

	length = CalcLengthLine(x1, y1, x2, y2);

	if (mode & 0x10)
	{
		x = MultX(x2);
		y = MultY(y2);
		DrawLine(x - 5, y, x + 5, y);
		DrawLine(x, y - 5, x, y + 5);

        //ellips2(x,y,5,5,255);

	}

	x = MultX(x1);
	y = MultY(y1);

	if (Units == 0)
	{

        //x1=fabs((CurrentX-CurrentX2)/2540.0);
        //y1=fabs((CurrentY-CurrentY2)/2540.0);

		sprintf(str, "%.2f thou", length / 2540.0);
	}
	else
		sprintf(str, "%.4f mm", length / 100000.0);

	Value = 16 / Factor;
	Xoff = Value;
	Yoff = -Value;
	DrawLine(x, y, MultX(x2), MultY(y2));
	DrawLine(x - 20, y, x + 20, y);
	DrawLine(x, y - 20, x, y + 20);

    //TextOut(OutputDisplay,MultX(x1+Xoff),MultY(y1+Yoff),str,strlen(str));

	DrawStrWithRotation(x1 + Xoff, y1 + Yoff, Value, 0, 0, 0, str);

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void Measurement(int32 TestMode)
{
	int32 mode, FirstMousePosX, FirstMousePosY, MouseDivX, MouseDivY, MultiSelectMode, Mode2;
	double OldX, OldY, CurrentX, CurrentY, CurrentX2, CurrentY2;
	DrawXorFunctionRecord DrawXorFunction;

    //CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
    //CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));

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

	OldX = CurrentX;
	OldY = CurrentY;

    //DrawXorWindow(MultX(CurrentX2),MultY(CurrentY2),MultX(OldX),MultY(OldY),0);

	SystemBusyMode = 250;
	Mode2 = mode | 0x10;
	DrawXorFunction.Function11 = (FUNCP10) DrawObjects2a;
	DrawXorFunction.Param1[0] = &CurrentX2;
	DrawXorFunction.Param1[1] = &CurrentY2;
	DrawXorFunction.Param1[2] = &OldX;
	DrawXorFunction.Param1[3] = &OldY;
	DrawXorFunction.Param1[4] = &Mode2;
	DrawXorFunction.Mode = 10;
	DrawXorFunction.Param2[0] = &CurrentX2;
	DrawXorFunction.Param2[1] = &CurrentY2;
	DrawXorFunction.Param2[2] = &OldX;
	DrawXorFunction.Param2[3] = &OldY;
	DrawXorFunction.Param2[4] = &Mode2;
	ClipMouseCursor();

	while (!SelectionEsc)
	{
		CurrentX = PixelToRealOffX(MousePosX);
		CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		MouseDivX = abs(MousePosX - FirstMousePosX);
		MouseDivY = abs(MousePosY - FirstMousePosY);

        //CurrentX=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
        //CurrentY=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));

		if ((CurrentX2 != CurrentX) || (CurrentY2 != CurrentY))
		{
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode);
			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;

			/*
			      if (Units==0) {
			        x1=fabs((CurrentX-CurrentX2)/2540.0);
			        y1=fabs((CurrentY-CurrentY2)/2540.0);
			        sprintf(InfoStr,"%.2f,%.2f",x1,y1);
			      } else {
			        x1=fabs((CurrentX-CurrentX2)/100000.0);
			        y1=fabs((CurrentY-CurrentY2)/100000.0);
			        sprintf(InfoStr,"%.4f,%.4f",x1,y1);
			      }
			      RedrawInfoStr(1);
		   */

			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosX > DrawWindowMaxX - ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
			ScrollRight(ScrollSize);
			MousePosX -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosY > DrawWindowMaxY - ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
			ScrollDown(ScrollSize);
			MousePosY -= ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosX < DrawWindowMinX + ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
			ScrollLeft(ScrollSize);
			MousePosX += ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
		}

		if ((MouseDivX > 2) && (MouseDivY > 2) && (MousePosY < DrawWindowMinY + ScrollEndOfWindow))
		{
			SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
			ScrollUp(ScrollSize);

            //NrGraphicsObjects

			MousePosY += ScrollSizeDrawing;
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
		}

		DisplayCursorPosition();
		MouseChanged = 0;
		CheckInputMessages(0);

		if (!Focused)
		{
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();
			SelectionEsc = 1;
		}

		if (CheckLeftButton())
		{
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode);
			mode++;
			CheckInputMessages(0);

			switch (mode)
			{
			case 1:
				OldX = CurrentX;
				OldY = CurrentY;
				DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
				break;

			case 2:
				DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x20);
				SelectionEsc = 1;
				break;
			}

			ok = 1;
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
				SelectionEsc = 1;

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			ClipMouseCursor();

			if (!SelectionEsc)
				DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
		}
	}

	UnClipMouseCursor();
	DrawCrossHair(2);
	SelectionEsc = 0;
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawButtonInfoOff(int32 mode)
{
	if (DisplayButtonNr)
	{
		DrawCrossHair(2);
		InvalidateRect(GEOMWindow, &ButtonRect, 0);
		UpdateWindow(GEOMWindow);
		DisplayButtonNr = 0;
	}

	if (mode == 1)
		NrButtonInfo = -1;

	if (mode == 2)
		OldButtonNr = -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ButtonInfo()
{
	int32 NewButtonNr, val;
	HGDIOBJ SaveFont;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	SIZE NewTextSize;

    //String

	if (FirstPaint)
		return;

	val = (MousePosY - 4) / ButtonSizeY;

	if ((val >= 0) && (val < NrButtons))
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
					OldButtonNr = NewButtonNr;
					StartDrawingEditingWindow();
					InitDrawingButtonInfo();
					SetTextColor(OutputDisplay, RGB(0, 0, 0));
					SetBkMode(OutputDisplay, TRANSPARENT);
					SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));

					switch (val)
					{
					case 0:
						strcpy(str, SC(1, "New geometry"));

						if (GetKeyString(str2, ID_FILE_NEW, 0x21) == 1)
							strcat(str, str2);

						break;

					case 1:
						strcpy(str, SC(2, "Open"));

						if (GetKeyString(str2, ID_FILE_OPEN, 0x21) == 1)
							strcat(str, str2);

						break;

					case 2:
						strcpy(str, SC(98, "Save"));

						if (GetKeyString(str2, ID_FILE_SAVE, 0x21) == 1)
							strcat(str, str2);

						break;

                     //case 3:
                     //strcpy(str,SC(363,"Print"));
                     //break;

					case 3:
						strcpy(str, SC(81, "Create circle SIP (SIL) THT"));

						if (GetKeyString(str2, ID_CREATE_SIL, 0x21) == 1)
							strcat(str, str2);

						break;

					case 4:
						strcpy(str, SC(82, "Create square SIP (SIL) THT"));

						if (GetKeyString(str2, ID_CREATE_SIL2, 0x21) == 1)
							strcat(str, str2);

						break;

					case 5:
						strcpy(str, SC(76, "Create SIP (SIL) SMD Circle pad"));

						if (GetKeyString(str2, ID_CREATE_SIL_SMD_CIRCLE, 0x21) == 1)
							strcat(str, str2);

						break;

					case 6:
						strcpy(str, SC(75, "Create SIP (SIL) SMD Rectangle pad"));

						if (GetKeyString(str2, ID_CREATE_SIL_SMD_RECT, 0x21) == 1)
							strcat(str, str2);

						break;

					case 7:
						strcpy(str, SC(83, "Add unplated drill"));

						if (GetKeyString(str2, ID_ADD_DRILL_UNPLATED, 0x21) == 1)
							strcat(str, str2);

						break;

					case 8:
						strcpy(str, SC(41, "Assign/Remove pins to objects"));

						if (GetKeyString(str2, ID_ASSIGN_PINS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 9:
						strcpy(str, SC(86, "Add line placement outline"));

						if (GetKeyString(str2, ID_ADD_LINE_OBJECT +	PLACEMENT_OUTLINE_LAYER, 0x21) == 1)
							strcat(str, str2);

						break;

					case 10:
						strcpy(str, SC(87, "Add rectangle placement outline"));

						if (GetKeyString(str2, ID_ADD_RECT2_OBJECT + PLACEMENT_OUTLINE_LAYER, 0x21) == 1)
							strcat(str, str2);

						break;

					case 11:
						strcpy(str, SC(88, "Add line component outline"));

						if (GetKeyString(str2, ID_ADD_LINE_OBJECT +	COMP_OUTLINE_LAYER, 0x21) == 1)
							strcat(str, str2);

						break;

					case 12:
						strcpy(str, SC(89, "Add rectangle component outline"));

						if (GetKeyString(str2, ID_ADD_RECT2_OBJECT + COMP_OUTLINE_LAYER, 0x21) == 1)
							strcat(str, str2);

						break;

					case 13:
						strcpy(str, SC(90, "Undo"));

						if (GetKeyString(str2, ID_EDIT_UNDO, 0x21) == 1)
							strcat(str, str2);

						break;

					case 14:
						strcpy(str, SC(91, "Redo"));

						if (GetKeyString(str2, ID_EDIT_REDO, 0x21) == 1)
							strcat(str, str2);

						break;

					case 15:
						strcpy(str, SC(33, "Copy"));

						if (GetKeyString(str2, ID_COPY_OBJECTS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 16:
						strcpy(str, SC(43, "Delete"));

						if (GetKeyString(str2, ID_EDIT_DELETE, 0x21) == 1)
							strcat(str, str2);

						break;

					case 17:
						strcpy(str, SC(93, "Move"));

						if (GetKeyString(str2, ID_MOVE_OBJECTS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 18:
						strcpy(str, SC(109, "Unselect all"));

						if (GetKeyString(str2, ID_UNSELECT_ALL, 0x21) == 1)
							strcat(str, str2);

						break;

					case 19:
						strcpy(str, SC(94, "Select visible layers/objects"));

						if (GetKeyString(str2, ID_VIEW_LAYERS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 20:
						strcpy(str, SC(96, "Info selected objects"));

						if (GetKeyString(str2, ID_VIEW_INFOSELECTEDOBJECTS, 0x21) == 1)
							strcat(str, str2);

						break;
					}

					GetTextExtentPoint32(OutputDisplay, str, strlen(str), &NewTextSize);
					ButtonRect.left = 40;
					ButtonRect.right = 40 + NewTextSize.cx + 5;
					ButtonRect.top = NrButtonInfo * ButtonSizeY + 5;
					ButtonRect.bottom = NrButtonInfo * ButtonSizeY + 23;
					RoundRect(OutputDisplay, ButtonRect.left, ButtonRect.top, ButtonRect.right, ButtonRect.bottom, 5,
					          5);
					TextOut(OutputDisplay, ButtonRect.left + 2, ButtonRect.top + 2, str, strlen(str));
					SelectObject(OutputDisplay, SaveFont);
					ExitDrawing();
					EndDrawingEditingWindow();
				}
			}
			else
			{
				if (TimerValue > TimeOut + BUTTON_INFO_TIMEOUT)
					DrawButtonInfoOff(0);
			}
		}
	}
	else
		DrawButtonInfoOff(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckButtonPressed(int32 mode)
{
	int32 val, res;
	HGDIOBJ old;
	HPEN DrawPen;
	HBITMAP ButtonBitmap;
	HDC ButtonMemoryDC;
	POINT PP;


	val = (MousePosY - 4) / ButtonSizeY;

	if ((val >= 0) && (val < NrButtons))
	{
		if (mode == 0)
		{
			StartDrawingEditingWindow();
			ButtonBitmap = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTONS));
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

			EditingRegion = CreateRectRgn(DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY);
			SelectClipRgn(OutputDisplay, EditingRegion);
			DeleteObject(DrawPen);
			ExitDrawing();
			EndDrawingEditingWindow();
			return;
		}

		StartDrawingEditingWindow();
		ButtonBitmap = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTONS));
		ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
		old = SelectObject(ButtonMemoryDC, ButtonBitmap);
		SelectClipRgn(OutputDisplay, NULL);
		res =
		    BitBlt(OutputDisplay, 1, ButtonSizeY * val + 1, ButtonSizeX + 1, ButtonSizeY + 2, ButtonMemoryDC, 0,
		           ButtonSizeY * val, SRCCOPY);
		SelectObject(ButtonMemoryDC, old);
		DeleteDC(ButtonMemoryDC);
		DeleteObject(ButtonBitmap);
		EditingRegion = CreateRectRgn(DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY);
		SelectClipRgn(OutputDisplay, EditingRegion);
		ExitDrawing();
		EndDrawingEditingWindow();

		switch (val)
		{
		case 0:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_FILE_NEW, (LPARAM) NULL);
			break;

		case 1:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_FILE_OPEN, (LPARAM) NULL);
			break;

		case 2:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_FILE_SAVE, (LPARAM) NULL);
			break;

         //case 3:
         //PostMessage(GEOMWindow,WM_COMMAND,(WPARAM)ID_FILE_PRINT,(LPARAM)NULL);
         //break;

		case 3:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SIL, (LPARAM) NULL);
			break;

		case 4:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SIL2, (LPARAM) NULL);
			break;

		case 5:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SIL_SMD_CIRCLE, (LPARAM) NULL);
			break;

		case 6:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SIL_SMD_RECT, (LPARAM) NULL);
			break;

		case 7:
			DrawButtonInfoOff(0);
			SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_ADD_DRILL_UNPLATED, (LPARAM) NULL);
			break;

		case 8:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_ASSIGN_PINS, (LPARAM) NULL);
			break;

		case 9:
			if (PlacementVisible)
			{
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_ADD_LINE_PLACE_OUTL, (LPARAM) NULL);
			}

			break;

		case 10:
			if (PlacementVisible)
			{
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_ADD_RECT2_PLACE_OUTL, (LPARAM) NULL);
			}

			break;

		case 11:
			if (CompOutlineVisible)
			{
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_ADD_LINE_COMP_OUTL, (LPARAM) NULL);
			}

			break;

		case 12:
			if (CompOutlineVisible)
			{
				DrawButtonInfoOff(0);
				SetCursorPos(ClientStartX + ClientWindowDivX / 2, ClientStartY + ClientWindowDivY / 2);
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_ADD_RECT2_COMP_OUTL, (LPARAM) NULL);
			}

			break;

		case 13:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_EDIT_UNDO, (LPARAM) NULL);
			break;

		case 14:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_EDIT_REDO, (LPARAM) NULL);
			break;

		case 15:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_COPY_OBJECTS, (LPARAM) NULL);
			break;

		case 16:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_EDIT_DELETE, (LPARAM) NULL);
			break;

		case 17:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_MOVE_OBJECTS, (LPARAM) NULL);
			break;

		case 18:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_UNSELECT_ALL, (LPARAM) NULL);
			break;

		case 19:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_VIEW_LAYERS, (LPARAM) NULL);
			break;

		case 20:
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_VIEW_INFOSELECTEDOBJECTS, (LPARAM) NULL);
			break;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
