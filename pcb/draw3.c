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
#include "utf8.h"
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
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "calcdef.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "mainloop.h"
#include "nets.h"
#include "files.h"
#include "trace2.h"
#include "trace3.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "help.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "graphics.h"
#include "resource.h"
#include "polygon.h"
#include "select4.h"

#define  BUTTON_INFO_TIMEOUT  40

#define MultZ(Nr) ( (((Nr))>(0)) ? ((int32)(Fact*(Nr)+0.5)) : ((int32)(Fact*(Nr)-0.5)) )
#define MultZX(Nr) (MultZ(Nr-BoardOX)+DrawWindowMinX+XO)
#define MultZY(Nr) ((int32)WindowY-MultZ(Nr-BoardOY)+YO)

double NewFactor, BoardWidth, BoardHeight, BoardOX, BoardOY;

int32 FirstOutZoom, ok;
int32 MouseEqual = 0;
int32 FirstText = 0;


int32 TimeOut, OldButtonNr, FirstValue, TimeOutInfo = -1, DisplayInfoTimeOut = -1;
int32 CurrentButtonNr = 0;
int32 DisplayButtonNr = 0;
int32 DisplayMouseInfo = 0;
int32 FirstButtonPressed = 0;
RECT ButtonRect, InfoRect;
int32 NrButtonInfo = -1;
int32 NrButtons = 22;
int32 ButtonSizeX = 27;
int32 ButtonSizeY = 22;
int32 OldMouseX = -10000;
int32 OldMouseY = -10000;

char OldText[80], DisplayInfoStr[2048];


extern HDC OutputDisplay;
extern int32 TimerValue;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawXorWindow(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	int32 xmin, xmax, ymin, ymax;

	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
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

void DrawTempObjects(double WindowX, double WindowY)
{
	double x1, y1, x2, y2, Fact;
	int32 XO, YO, cx, cy, cnt, cnt2, cnt3, xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4, ok;
	CompRecord *Comp;
	ObjectRecord *Object;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;

	PolygonObject = (PolygonRecord *) & PolygonBuf;

	cx = (DrawWindowMaxX - DrawWindowMinX) / 2;
	cy = (DrawWindowMaxY - DrawWindowMinY) / 2;
	XO = cx - (int32) (WindowX * 0.5);
	YO = cy - (int32) (WindowY * 0.5);

	DrawCrossHair(2);
	StartDrawingEditingWindow();
	InitDrawingObject(0, BACKGROUND_LAYER, 0, NORMAL_FILLED_AND_PEN1);
	rect3(cx + DrawWindowMinX, cy + 1, (int32) WindowX, (int32) WindowY);
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);

	if (BoardWidth * DisplY / DisplX > BoardHeight)
		Fact = WindowX / BoardWidth;
	else
		Fact = WindowY / BoardHeight;

//  Fact=WindowX/Design.BoardWidth;
//  BOX=Design.BoardOriginX;
//  BOY=Design.BoardOriginY;
	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Comp->BoardPosMinX;
			y1 = Comp->BoardPosMinY;
			x2 = Comp->BoardPosMaxX;
			y2 = Comp->BoardPosMaxY;
			NrObjects = 0;
			ShapePlacementOutLineToObject(Comp, 0.0, 0.0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				switch (Object->ObjectType)
				{
				case OBJECT_LINE:
					xx1 = MultZX(Object->x1);
					yy1 = MultZY(Object->y1);
					xx2 = MultZX(Object->x2);
					yy2 = MultZY(Object->y2);
					DrawLine(xx1, yy1, xx2, yy2);
					break;

				case OBJECT_RECT:
					xx1 = MultZX(Object->x1);
					yy1 = MultZY(Object->y1);
					xx2 = MultZ(Object->x2);
					yy2 = MultZ(Object->y2);
					rect3(xx1, yy1, xx2, yy2);
					break;

				case OBJECT_ARC:
					xx1 = MultZX(Object->x1);
					yy1 = MultZY(Object->y1);
					xx2 = MultZ(Object->x2);
					yy2 = MultZ(Object->y2);
					xx3 = MultZX(Object->x1 + Object->x3);
					yy3 = MultZY(Object->y1 + Object->y3);
					xx4 = MultZX(Object->x1 + Object->x4);
					yy4 = MultZY(Object->y1 + Object->y4);
					SpecialArc(xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4);
					break;

				case OBJECT_POLYGON:
					MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

					for (cnt3 = 0; cnt3 < PolygonObject->NrVertices; cnt3++)
					{
						x1 = PolygonObject->Points[cnt3].x;
						y1 = PolygonObject->Points[cnt3].y;
						PolygonObject->Points[cnt3].x = MultZX(x1);
						PolygonObject->Points[cnt3].y = MultZY(y1);
					}

					DrawFilledPolygon(PolygonObject, 16);
					ok = 1;
					break;

				case OBJECT_CIRCLE:
					xx1 = MultZX(Object->x1);
					yy1 = MultZY(Object->y1);
					xx2 = MultZ(Object->x2);
					ellips2(xx1, yy1, xx2, xx2, CircleConv[(uint8) Object->y2]);
					break;
				}
			}
		}
	}

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
	RECT Rect;


	xmin = min(x1, x2);
	xmax = max(x1, x2);
	ymin = min(y1, y2);
	ymax = max(y1, y2);
	cx2 = (DrawWindowMaxX - DrawWindowMinX) / 2 + DrawWindowMinX;
	cy2 = (DrawWindowMaxY - DrawWindowMinY) / 2;
	CentreX = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
	CentreY = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);
	cx = (x1 + x2) / 2;
	cy = (y1 + y2) / 2;

	if ((x1 <= x2) || (y1 <= y2))
	{
		DisplayX = (DrawWindowMaxX - DrawWindowMinX);
		DisplayX *= 0.75;
		difx = x1 - x2;
		DisplX = PixelToReal(DrawWindowMaxX - DrawWindowMinX);
		DisplY = PixelToReal(DrawWindowMaxY - DrawWindowMinY);

		if (BoardWidth * DisplY / DisplX > BoardHeight)
			DisplayY = DisplayX * BoardHeight / BoardWidth;
		else
		{
			DisplayY = (DisplayX * DisplY / DisplX);
			DisplayX = (DisplayY * BoardWidth / BoardHeight);
		}

		if (!FirstOutZoom)
		{
			FirstOutZoom = 1;
			Rect.left = (int32) (cx2 - (int32) (DisplayX * 0.5) - 1);
			Rect.right = (int32) (cx2 + (int32) (DisplayX * 0.5) + 1);
			Rect.top = (int32) (cy2 - (int32) (DisplayY * 0.5) - 1);
			Rect.bottom = (int32) (cy2 + (int32) (DisplayY * 0.5) + 1);
			InvalidateRect(PCBWindow, &Rect, 0);
			UpdateWindow(PCBWindow);
		}

		StartDrawingEditingWindow();
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
		SetROP2(OutputDisplay, R2_XORPEN);
		rect2(xmin, ymin, abs(x2 - x1) + 1, abs(y2 - y1) + 1);
		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow();
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
		BoardCentreX = BoardWidth * 0.5 + BoardOX;
		BoardCentreY = BoardHeight * 0.5 + BoardOY;

		y3 = (int32) hulp;
		hulp = ((CentreX - BoardCentreX) / BoardWidth) * DisplayX;
		x4 = (int32) hulp;
		hulp = ((CentreY - BoardCentreY) / BoardHeight) * DisplayY;
		y4 = (int32) hulp;

		if (FirstOutZoom)
			DrawTempObjects(DisplayX, DisplayY);

		StartDrawingEditingWindow();
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);

		if (FirstOutZoom)
		{
			FirstOutZoom = 0;
			rect3(cx2, cy2, (int32) DisplayX, (int32) DisplayY);
		}

		SetROP2(OutputDisplay, R2_XORPEN);
		rect3(cx2 + x4, cy2 - y4, x3, y3);
		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ZoomWindow()
{
	double ZoomMinX, ZoomMinY, ZoomMaxX, ZoomMaxY, hulp, cx, cy, PixelsX, PixelsY, CentreX, CentreY, cx2, cy2;
	int32 mode;
	int32 OldX2, OldY2;
	RECT Rect;

	SelectRectX1 = MousePosX;
	SelectRectX2 = MousePosX;
	SelectRectY1 = MousePosY;
	SelectRectY2 = MousePosY;
	OldX2 = SelectRectX2;
	OldY2 = SelectRectY2;
//  if (CurrentDrawMode==0) CurrentDrawMode=1;

	FindMinMaxBoard(&BoardOX, &BoardOY, &BoardWidth, &BoardHeight, 1);
	BoardWidth -= BoardOX;
	BoardHeight -= BoardOY;

	SelectionEsc = 0;
	mode = 0;
	FirstOutZoom = 1;
	DrawXorWindow2(SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2, mode);
	ClipMouseCursor();
	SetNormalCursor();
	mode = 1;

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

	UnClipMouseCursor();

	if (!SelectionEsc)
		DrawXorWindow2(SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2, mode);

	SetNormalCursor();

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
				//      DisplX=(ZoomMaxX-ZoomMinX);
				//      DisplY=DisplX*DrawWindowMaxY/DrawWindowMaxX;
				Rect.left = (int32) DrawWindowMinX;
				Rect.right = (int32) DrawWindowMaxX;
				Rect.top = (int32) DrawWindowMinY;
				Rect.bottom = (int32) DrawWindowMaxY;
				ViewMinX = PixelToRealOffX(-1);
				ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
				ViewMinY = PixelToRealOffY(-1);
				ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
				InvalidateRect(PCBWindow, &Rect, 0);
				DrawCrossHair(2);
				DisplayCursorPosition();
				UpdateWindow(PCBWindow);
				//      PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);

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
				Rect.left = (int32) DrawWindowMinX;
				Rect.right = (int32) DrawWindowMaxX;
				Rect.top = (int32) DrawWindowMinY;
				Rect.bottom = (int32) DrawWindowMaxY;
				ViewMinX = PixelToRealOffX(-1);
				ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
				ViewMinY = PixelToRealOffY(-1);
				ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
				DrawCrossHair(2);
				InvalidateRect(PCBWindow, &Rect, 0);
				DisplayCursorPosition();
				UpdateWindow(PCBWindow);
			}
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
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
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
	int32 mode, OldX2, OldY2, width, height, cx, cy, cx2, cy2;
	RECT Rect;

	cx = (DrawWindowMaxX - DrawWindowMinX) / 2;
	cy = (DrawWindowMaxY - DrawWindowMinY) / 2;
	CentreX = PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2);
	CentreY = PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2);

	FindMinMaxBoard(&BoardOX, &BoardOY, &BoardWidth, &BoardHeight, 1);
	BoardWidth -= BoardOX;
	BoardHeight -= BoardOY;

	DisplayX = (DrawWindowMaxX - DrawWindowMinX);
	DisplayX *= 0.75;
	DisplX = PixelToReal(DrawWindowMaxX - DrawWindowMinX);
	DisplY = PixelToReal(DrawWindowMaxY - DrawWindowMinY);

	if (BoardWidth * DisplY / DisplX > BoardHeight)
		DisplayY = DisplayX * BoardHeight / BoardWidth;
	else
	{
		DisplayY = (DisplayX * DisplY / DisplX);
		DisplayX = (DisplayY * BoardWidth / BoardHeight);
	}

	BoardCentreX = BoardWidth * 0.5 + BoardOX;
	BoardCentreY = BoardHeight * 0.5 + BoardOY;
	hulp = ((CentreX - BoardCentreX) / BoardWidth) * DisplayX;
	cx2 = (int32) hulp;
	hulp = ((CentreY - BoardCentreY) / BoardHeight) * DisplayY;
	cy2 = (int32) hulp;


	DrawTempObjects(DisplayX, DisplayY);

//  MousePosX=(int32)(cx+cx2);
	MousePosX = (int32) (cx + cx2) + DrawWindowMinX;
	MousePosY = (int32) (cy - cy2);
	SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY);


	DrawCrossHair(2);
	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
	rect3(cx + DrawWindowMinX, cy, (int32) DisplayX, (int32) DisplayY);
	ExitDrawing();
	EndDrawingEditingWindow();

//  DrawXorWindow3(cx+DrawWindowMinX,cy,(int32)DisplayX,(int32)DisplayY,0);

	SelectRectX1 = MousePosX;
	SelectRectX2 = MousePosX;
	SelectRectY1 = MousePosY;
	SelectRectY2 = MousePosY;
	OldX2 = SelectRectX2;
	OldY2 = SelectRectY2;
//  if (CurrentDrawMode==0) CurrentDrawMode=1;

	SelectionEsc = 0;
	mode = 0;
	ClipMouseCursor();
	SetNormalCursor();

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

	UnClipMouseCursor();
	SetNormalCursor();

	if (!SelectionEsc)
	{
		DrawXorWindow3(MousePosX, MousePosY, width, height, mode);
		hulp = (MousePosX - DrawWindowMinX - (cx - (DisplayX / 2)));
//    hulp=(MousePosX-DrawWindowMinX-(cx-(DisplayX/2)));
		Xoffset = ((hulp * BoardWidth / DisplayX) - (DisplX * 0.5) + BoardCentreX - BoardWidth / 2);
		hulp = (cy + (DisplayY / 2) - MousePosY);
		Yoffset = ((hulp * BoardHeight / DisplayY) - (DisplY * 0.5) + BoardCentreY - BoardHeight / 2);
//    DisplX=PixelToRealOffX(DrawWindowMaxX)-PixelToRealOffX(DrawWindowMinX);
//    DisplY=PixelToRealOffY(DrawWindowMaxY)-PixelToRealOffY(DrawWindowMinY);
		Rect.left = (int32) DrawWindowMinX;
		Rect.right = (int32) DrawWindowMaxX;
		Rect.top = (int32) DrawWindowMinY;
		Rect.bottom = (int32) DrawWindowMaxY;
		ViewMinX = PixelToRealOffX(-1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(-1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		DrawCrossHair(2);
		InvalidateRect(PCBWindow, &Rect, 0);
		DisplayCursorPosition();
		UpdateWindow(PCBWindow);
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
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_XORPEN);

	if (mode == 0)
		rect2(x1, y1, x2 - x1 + 1, y2 - y1 + 1);
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
//    LineTo(OutputDisplay,x3,y3);
//    LineTo(OutputDisplay,x2,y2);
//    LineTo(OutputDisplay,x4,y4);
//    LineTo(OutputDisplay,x1,y1);
	}

	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CenterScreen(double cx, double cy)
{
	double DisplX, DisplY;

	DisplX = PixelToReal(DrawWindowMaxX - DrawWindowMinX);
	DisplY = PixelToReal(DrawWindowMaxY - DrawWindowMinY);
	Xoffset = cx - DisplX * 0.5;
	Yoffset = cy - DisplY * 0.5;
	PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, (LPARAM) NULL);
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
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);

	if (mode & 0x20)
		SetROP2(OutputDisplay, R2_COPYPEN);
	else
		SetROP2(OutputDisplay, R2_XORPEN);

	length = CalcLengthLine(x1, y1, x2, y2);

	if (mode & 0x10)
	{
		x = MultX(x2);
		y = MultY(y2);
		DrawLine(x - 5, y, x + 5, y);
		DrawLine(x, y - 5, x, y + 5);
	}

	x = MultX(x1);
	y = MultY(y1);

	if (Units == 0)
	{
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
	int32 mode, FirstMousePosX, FirstMousePosY, MouseDivX, MouseDivY, MultiSelectMode;
	double OldX, OldY, CurrentX, CurrentY;

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

//  DrawXorWindow(MultX(CurrentX2),MultY(CurrentY2),MultX(OldX),MultY(OldY),0);

	SystemBusyMode = 250;
	ClipMouseCursor();
	SetNormalCursor();

	while (!SelectionEsc)
	{
		CurrentX = PixelToRealOffX(MousePosX);
		CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		MouseDivX = abs(MousePosX - FirstMousePosX);
		MouseDivY = abs(MousePosY - FirstMousePosY);
		
		if ((CurrentX2 != CurrentX) || (CurrentY2 != CurrentY))
		{
			DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode);
			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;
			
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
//        NrGraphicsObjects
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
			{
				/*
				        Help(IDH_Selecting_deselecting_objects,0);
				        CheckInputMessages(0);
				        while (!Focused) {
				          CheckInputMessages(0);
				        }
				        CheckInputMessages(0);
				        HelpAsked=0;
				*/
				SelectionEsc = 1;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			ClipMouseCursor();

			if (!SelectionEsc)
				DrawMeasurementLine(CurrentX2, CurrentY2, OldX, OldY, mode | 0x10);
		}
	}

	UnClipMouseCursor();
	DrawCrossHair(2);
	SetNormalCursor();
	SelectionEsc = 0;
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawMouseInfoOff(int32 mode)
{
	if (DisplayButtonNr)
	{
		DrawCrossHair(2);
		InvalidateRect(PCBWindow, &ButtonRect, 0);
		UpdateWindow(PCBWindow);
		DisplayButtonNr = 0;
	}

	if (DisplayMouseInfo)
	{
		DisplayMouseInfo = 0;

		if (FirstText)
			strcpy(InfoStr, OldText);

		RedrawInfoStr(1);
	}

	if ((mode & 3) == 1)
		NrButtonInfo = -1;

	if ((mode & 3) == 2)
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

// String InfoStr
	if (FirstPaint)
		return;

	val = (MousePosY - 4) / ButtonSizeY;

	if ((val >= 0) && (val < NrButtons))
	{
		NewButtonNr = val;

		if (NewButtonNr != NrButtonInfo)
		{
			DrawMouseInfoOff(2);
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
					DrawMouseInfoOff(0);
					DisplayButtonNr = 1;
					OldButtonNr = NewButtonNr;
					StartDrawingEditingWindow();
					InitDrawingObject(0, BUTTON_INFO_LAYER, -1, NORMAL_FILLED_AND_PEN1);
					SetTextColor(OutputDisplay, RGB(0, 0, 0));
					SetBkMode(OutputDisplay, TRANSPARENT);
					SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));

					//**********************************************************************************************
                    //*********************** levá tlaèítka ********************************************************
                    //**********************************************************************************************

					switch (val)
					{
					case 0:
						
						strcpy(str, SC(130, "New layout"));

						if (GetKeyString(str2, ID_FILE_NEW, 0x21) == 1)
							strcat(str, str2);

						break;

					case 1:
						strcpy(str, SC(131, "Open"));

						if (GetKeyString(str2, ID_FILE_OPEN, 0x21) == 1)
							strcat(str, str2);

						break;

					case 2:
						strcpy(str, SC(132, "Save"));

						if (GetKeyString(str2, ID_FILE_SAVE, 0x21) == 1)
							strcat(str, str2);

						break;

					case 3:
						strcpy(str, SC(133, "Print screen"));

						if (GetKeyString(str2, ID_FILE_PRINT, 0x21) == 1)
							strcat(str, str2);

						break;

					case 4:
						strcpy(str, SC(134, "Move/rotate/change components"));

						if (GetKeyString(str2, ID_ACTION_MOVE_COMPS2, 0x21) == 1)
							strcat(str, str2);

						break;

					case 5:
						strcpy(str, SC(135, "Drag traces"));

						if (GetKeyString(str2, ID_ACTION_DRAG_TRACE, 0x21) == 1)
							strcat(str, str2);

						break;

					case 6:
						strcpy(str, SC(136, "Change traces/vias"));

						if (GetKeyString(str2, ID_ACTION_DRAG_TRACES_VIAS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 7:
						strcpy(str, SC(138, "Routing traces"));

						if (GetKeyString(str2, ID_ACTION_ROUTE_TRACES, 0x21) == 1)
							strcat(str, str2);

						break;

					case 8:
						strcpy(str, SC(176, "Areafills/powerplanes"));

						if (GetKeyString(str2, ID_ACTION_AREAFILLS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 9:
						strcpy(str, SC(228, "Draw/change objects other layers"));

						if (GetKeyString(str2, ID_ACTION_OBJECTS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 10:
						strcpy(str, SC(241, "Modify component references"));

						if (GetKeyString(str2, ID_ACTION_MOVE_REFS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 11:
						strcpy(str, SC(268, "Modify component values"));

						if (GetKeyString(str2, ID_ACTION_MOVE_COMPVALUES, 0x21) == 1)
							strcat(str, str2);

						break;

					case 12:
						strcpy(str, SC(269, "Drag traces/vias/components"));

						if (GetKeyString(str2, ID_ACTION_DRAG_COMP_TRACE_VIAS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 13:
						strcpy(str, SC(292, "Gate/pin swap"));

						if (GetKeyString(str2, ID_ACTION_GATEPINSWAP, 0x21) == 1)
							strcat(str, str2);

						break;

					case 14:
						strcpy(str, SC(327, "Select layers"));

						if (GetKeyString(str2, ID_VIEW_LAYERS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 15:
						strcpy(str, SC(328, "Undo"));

						if (GetKeyString(str2, ID_EDIT_UNDO, 0x21) == 1)
							strcat(str, str2);

						break;

					case 16:
						strcpy(str, SC(329, "Redo"));

						if (GetKeyString(str2, ID_EDIT_REDO, 0x21) == 1)
							strcat(str, str2);

						break;

					case 17:
						strcpy(str, SC(330, "Copy objects"));

						if (GetKeyString(str2, ID_COPY_OBJECTS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 18:
						strcpy(str, SC(331, "Delete"));

						if (GetKeyString(str2, ID_EDIT_DELETE, 0x21) == 1)
							strcat(str, str2);

						break;

					case 19:
						strcpy(str, SC(332, "Move objects"));

						if (GetKeyString(str2, ID_MOVE_OBJECTS, 0x21) == 1)
							strcat(str, str2);

						break;

					case 20:
						strcpy(str, SC(161, "Unselect all"));

						if (GetKeyString(str2, ID_UNSELECT_ALL, 0x21) == 1)
							strcat(str, str2);

						break;

					case 21:
						strcpy(str, SC(137, "Information on selected objects"));

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
					TextOutUTF8(OutputDisplay, ButtonRect.left + 2, ButtonRect.top + 2, str, strlen(str));
					SelectObject(OutputDisplay, SaveFont);
					ExitDrawing();
					EndDrawingEditingWindow();
				}
			}
			else
			{
				if (TimerValue > TimeOut + BUTTON_INFO_TIMEOUT)
					DrawMouseInfoOff(0);
			}
		}
	}
	else
		DrawMouseInfoOff(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetButtonNr(int32 SelectionMode)
{
	switch (SelectionMode)
	{
	case 0:
		return 12;
		break;

	case 1:
		return 7;
		break;

	case 2:
		return 5;
		break;

	case 3:
		return 4;
		break;

	case 4:
		return 10;
		break;

	case 5:
		return 11;
		break;

	case 6:
		return 9;
		break;

	case 7:
		return 8;
		break;

	case 8:
		return 6;
		break;

	case 9:
		return 13;
		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PressButton(int32 val, int32 mode)
{
	int32 res;
	HGDIOBJ old;
	HPEN DrawPen;
	HBITMAP ButtonBitmap;
	HDC ButtonMemoryDC;
	POINT PP;

	if ((mode & 2) == 0)
		val = GetButtonNr(val);

	if ((mode & 1) == 0)
	{
		StartDrawingEditingWindow();
		SelectClipRgn(OutputDisplay, NULL);
	}

	DrawCrossHair(8 + 2);
	ButtonBitmap = LoadBitmap(PCBClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTONS));
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

	if (mode == 0)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DepressButton(int32 val, int32 mode)
{
	HGDIOBJ old;
	HBITMAP ButtonBitmap;
	HDC ButtonMemoryDC;
	int32 res;

	if (mode == 0)
		val = GetButtonNr(SelectionMode);

	StartDrawingEditingWindow();
	DrawCrossHair(8 + 2);
	ButtonBitmap = LoadBitmap(PCBClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTONS));
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
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckButtonPressed(int32 mode)
{
	int32 cnt, val, val2, res;
	int32 FoundButton = 0;

	if (mode == 1)
		res = 1;

	val = (MousePosY - 4) / ButtonSizeY;

	if ((val >= 0) && (val < NrButtons))
	{
		if ((FirstButtonPressed) && (mode == 0))
			return;

		if (!FirstButtonPressed)
		{
			FirstButtonPressed = 1;
			FirstValue = val;
		}

		val2 = GetButtonNr(SelectionMode);

		if (val != val2)
		{
			for (cnt = 0; cnt < 10; cnt++)
			{
				if (GetButtonNr(cnt) == val)
					FoundButton = 1;
			}

			if (mode == 0)
			{
				if (FoundButton)
					DepressButton(val2, 1);

				PressButton(val, 2);
				return;
			}

			if (mode == 1)
			{
				switch (FirstValue)
				{
				case 0:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_FILE_NEW, (LPARAM) NULL);
					break;

				case 1:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_FILE_OPEN, (LPARAM) NULL);
					break;

				case 2:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_FILE_SAVE, (LPARAM) NULL);
					break;

				case 3:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_FILE_PRINT, (LPARAM) NULL);
					break;

				case 4:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_MOVE_COMPS2, (LPARAM) NULL);
					break;

				case 5:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_DRAG_TRACE, (LPARAM) NULL);
					break;

				case 6:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_DRAG_TRACES_VIAS, (LPARAM) NULL);
					break;

				case 7:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_ROUTE_TRACES, (LPARAM) NULL);
					break;

				case 8:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_AREAFILLS, (LPARAM) NULL);
					break;

				case 9:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_OBJECTS, (LPARAM) NULL);
					break;

				case 10:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_MOVE_REFS, (LPARAM) NULL);
					break;

				case 11:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_MOVE_COMPVALUES, (LPARAM) NULL);
					break;

				case 12:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_DRAG_COMP_TRACE_VIAS, (LPARAM) NULL);
					break;

				case 13:
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_GATEPINSWAP, (LPARAM) NULL);
					break;

				case 14:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_LAYERS, (LPARAM) NULL);
					break;

				case 15:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_EDIT_UNDO, (LPARAM) NULL);
					break;

				case 16:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_EDIT_REDO, (LPARAM) NULL);
					break;

				case 17:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_COPY_OBJECTS, (LPARAM) NULL);
					break;

				case 18:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_EDIT_DELETE, (LPARAM) NULL);
					break;

				case 19:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_MOVE_OBJECTS, (LPARAM) NULL);
					break;

				case 20:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_UNSELECT_ALL, (LPARAM) NULL);
					break;

				case 21:
					DepressButton(FirstValue, 1);
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_INFOSELECTEDOBJECTS, (LPARAM) NULL);
					break;
				}
			}
		}
	}
	else
	{
		switch (FirstValue)
		{
		case 0:
		case 1:
		case 2:
		case 3:
		case 14:
		case 15:
		case 16:
		case 17:
		case 18:
		case 19:
		case 20:
		case 21:
		case 22:
			DepressButton(FirstValue, 1);
			break;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

#if 0

void InfoOnMousePosition()
{
	int32 ok, cnt, cnt2, Layer, CompInfo;
	double x1a, x2a, y1a, y2a, PosX, PosY;
	int32 Found = 0;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	ObjectRecord *Object, NewObject;
	CompRecord *Comp;

	if (FirstPaint)
		return;

	Layer = CurrentDrawingLayer;

	if (Layer == -1)
		Layer = Design.NrBoardLayers - 1;

	if ((!OkToDrawTopPads) && (Layer == Design.NrBoardLayers - 1))
		Layer = 0;
	else
	{
		if ((!OkToDrawBottomPads) && (Layer == 0))
			Layer = Design.NrBoardLayers - 1;
	}


	if ((MousePosX != OldMouseX) || (MousePosY != OldMouseY))
	{
		OldMouseX = MousePosX;
		OldMouseY = MousePosY;
		MouseEqual = 0;
		DrawMouseInfoOff(2);
		TimeOut = TimerValue;
		FirstText = 0;
	}
	else
	{
		if (!DisplayMouseInfo)
		{
			if ((!MouseEqual) && (TimerValue > TimeOut + 8))
			{
				DrawMouseInfoOff(0);
				TimeOut = TimerValue;
				DisplayMouseInfo = 1;
				MouseEqual = 1;
				PosX = PixelToRealOffX(MousePosX);
				PosY = PixelToRealOffY(DrawWindowMaxY - MousePosY);

				for (cnt = 0; cnt < Design.NrComps; cnt++)
				{
					Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
					CompInfo = Comp->Info;

					if ((CompInfo & OBJECT_NOT_VISIBLE) == 0)
					{
#ifdef _DEBUG

						if (stricmpOwn(Comp->Name, "C12") == 0)
							ok = 1;

#endif
						x1a = Comp->BoardPosMinX;
						y1a = Comp->BoardPosMinY;
						x2a = Comp->BoardPosMaxX;
						y2a = Comp->BoardPosMaxY;

						if ((PosX > x1a) && (PosX < x2a) && (PosY > y1a) && (PosY < y2a))
						{
#ifdef _DEBUG

							if (stricmpOwn(Comp->Name, "C11") == 0)
								ok = 1;

#endif
							NrObjects = 0;
							ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
							cnt2 = 0;
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

								NewObject.Layer = Layer;

								if ((!Found) && (ObjectsConnected(&NewObject, Object)))
								{
									if (CompPinText(Comp, Object->PinNr, PosX, PosY, str) == 0)
									{
										Found = 1;
										GetPinTextFromObject(Object, (LPSTR) & str2, (LPSTR) & str3);

										if (!FirstText)
										{
											strcpy(OldText, InfoStr);
											FirstText = 1;
										}

										DrawMouseInfoOff(0);
										DisplayMouseInfo = 1;
										sprintf(InfoStr, "%s - %s       %s", Comp->Name, str, str2);
										strcat(InfoStr, str3);
										RedrawInfoStr(1);
									}
								}
							}

							if (!Found)
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

									NewObject.Layer = Layer;

									if ((!Found) && (ObjectsConnected(&NewObject, Object)))
									{
										if (CompPinText(Comp, Object->PinNr, PosX, PosY, str) == 0)
										{
											Found = 1;
											GetPinTextFromObject(Object, (LPSTR) & str2, (LPSTR) & str3);

											if (!FirstText)
											{
												strcpy(OldText, InfoStr);
												FirstText = 1;
											}

											DrawMouseInfoOff(0);
											DisplayMouseInfo = 1;
											sprintf(InfoStr, "%s - %s       %s", Comp->Name, str, str2);
											strcat(InfoStr, str3);
											RedrawInfoStr(1);
										}
									}

									ok = 1;
								}

								ok = 1;
							}
						}
					}
				}
			}
		}
		else
		{
			if (TimerValue > TimeOut + 50)
			{	// 5 seconds
				DrawMouseInfoOff(0);
			}
		}
	}
}

#endif

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
	InvalidateRect(PCBWindow, &InfoRect, 0);
	UpdateWindow(PCBWindow);
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
	int32 lengte, cnt, NrLines, MaxX, MaxY, ox, oy, height;

	/*
	  if (DisplayInfoStr[0]!=0) {
	    DisplayInfoOff(0);
	    return 0;
	  }
	*/
	TimeOutInfo = TimerValue;

	StartDrawingEditingWindow();
	DrawCrossHair(8 + 2);
	InitDrawingObject(0, BUTTON_INFO_LAYER, -1, NORMAL_FILLED_AND_PEN1);
	SetTextColor(OutputDisplay, GraphicsObjectColor[BackGroundObjectNr]);
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
	height = MaxY * NrLines + 8 + NewTextSize.cy;

	if (y + 8 + height > DrawWindowMaxY)
		y = DrawWindowMaxY - 8 - height;

	InfoRect.left = x + 10;
	InfoRect.right = x + 10 + MaxX + 8;
	InfoRect.top = y + 8;
	InfoRect.bottom = InfoRect.top + MaxY * NrLines + 8 + NewTextSize.cy;
	RoundRect(OutputDisplay, InfoRect.left, InfoRect.top, InfoRect.right, InfoRect.bottom, 5, 5);
//  SelectObject(OutputDisplay,BackGroundPen);
	BackGroundActive = 1;
	InitDrawingObject(0, BUTTON_INFO_LAYER, -1, NORMAL_FILLED_AND_PEN1);

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

	if (mode == 0)
	{
		if (!PopupDisplayVisible)
			return 0;
	}

	if (TimeOutInfo != -1)
		ok = 1;

	result = GetInfoStr(DisplayInfoStr, mode);

	if ((result == 1) || (result == 3))
	{
		if (DisplayInfoStr[0] != 0)
		{
			if (mode == 0)
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
			else
			if (mode == 1)
			{
				if (TimeOutInfo == -1)
					NewDisplayInfo(DisplayInfoStr, MousePosX, MousePosY);

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
		return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
