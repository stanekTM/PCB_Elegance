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
#include "direct.h"
#include "commdlg.h"
#include "design.h"
#include "calc.h"
#include "memory.h"
#include "menus.h"
#include "mainloop.h"
#include "files.h"
#include "files2.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "check.h"
#include "command.h"
#include "resource.h"
#include "utf8.h"

POINT MousePos;

int32 MaxCollections, px, py, ro, OldMousePosX, OldMousePosY, ok;

int32 direction;
int32 LastDirection = -1;
int32 SpecialLayer = 1;

int32 OldX2, OldY2;

int32 NrButtons = 15;
int32 ButtonSizeX = 105;
int32 ButtonSizeY = 81;
int32 ButtonDivX = 135;
int32 ButtonDivY = 109;
int32 ButtonStartX = 15;
int32 ButtonStartY = 15;
int32 ButtonBitmapStartX = 62;
int32 ButtonBitmapStartY = 2;
int32 ButtonBitmapSizeX = 630;
int32 ButtonBitmapSizeY = 327;
int32 NrButtonInfo = -1;

int32 NetNrCheck, DrawingObjects, DrawingCommand, TimeOut, DisplayButtonNr;
RECT ButtonRect, CurrentButtonRect, CursorWindow;

COLORREF LineColor, ButtonBackGroundColor;

extern double CentreSelectedX;
extern int32 MessageHeight, TimerValue, WindowStartX, WindowStartY;
extern int32 WriteLnError;
extern int32 FirstPaint, TotalExit, PaintIntro, UpdateLayout;
extern int32 MessageStartY;
extern ProjectInfoRecord *ProjectInfo;

extern HBITMAP BackgroundBitmap, BackAnnotateButtonBitmap, SymbolButtonBitmap, SchematicButtonBitmap,
       AnnotateButtonBitmap, NetlistButtonBitmap, LayoutButtonBitmap, GeometryButtonBitmap, GerberButtonBitmap,
       BOMButtonBitmap;
extern HFONT NewFont;



//int32 CHECK_LICENSE(Command,mode);


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawBackground(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old;
	int32 res;

	old = SelectObject(ButtonMemoryDC, BackgroundBitmap);
	res = BitBlt(OutputDisplay, ButtonBitmapStartX, ButtonBitmapStartY, ButtonBitmapSizeX, ButtonBitmapSizeY,
	           ButtonMemoryDC, 0, 0, SRCAND);
	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawBackAnnotateButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, BackAnnotateButtonBitmap);
	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 2;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 0;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);
	strcpy(str, SC(128, "Annotate"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 4, str, strlen(str));
	strcpy(str, SC(129, "back"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawSymbolButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, SymbolButtonBitmap);
	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 0;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 1;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}
	//*******************************************************************************************************
	//********************************** symbol editor ******************************************************
	//*******************************************************************************************************
	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);
	strcpy(str, SC(130, "Symbol"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 4, str, strlen(str));
	strcpy(str, SC(131, "editor"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawSchematicButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, SchematicButtonBitmap);
	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 1;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 1;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);
	strcpy(str, SC(132, "Schematic"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 4, str, strlen(str));
	strcpy(str, SC(131, "editor"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawAnnotateButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, AnnotateButtonBitmap);
	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 2;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 1;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);

	strcpy(str, SC(128, "Annotate"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawNetlistButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, NetlistButtonBitmap);
	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 3;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 1;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);

	strcpy(str, SC(133, "Netlist"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawLayoutButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, LayoutButtonBitmap);
	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 4;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 1;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);
	strcpy(str, SC(162, "Layout"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 4, str, strlen(str));
	strcpy(str, SC(131, "editor"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawGeometryButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, GeometryButtonBitmap);
	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 1;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 2;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);
	strcpy(str, SC(32, "Geometry"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 4, str, strlen(str));
	strcpy(str, SC(131, "editor"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawBOMButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, BOMButtonBitmap);

	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 0;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 2;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);

	strcpy(str, SC(134, "Bill of"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 4, str, strlen(str));
	strcpy(str, SC(135, "materials"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}

// *******************************************************************************************************
// *******************************************************************************************************

HGDIOBJ DrawGerberButton(HDC ButtonMemoryDC, int32 mode)
{
	HGDIOBJ old, old2;
	int32 x1, y1, res;
	char str[MAX_LENGTH_STRING];
	SIZE TextSize;

	old = SelectObject(ButtonMemoryDC, GerberButtonBitmap);

	old2 = SelectObject(OutputDisplay, NewFont);
	x1 = ButtonBitmapStartX + ButtonStartX + ButtonDivX * 4;
	y1 = ButtonBitmapStartY + ButtonStartY + ButtonDivY * 2;

	if ((mode & 1) == 1)
	{
		x1++;
		y1++;
	}

	res = BitBlt(OutputDisplay, x1, y1, ButtonBitmapSizeX, ButtonBitmapSizeY, ButtonMemoryDC, 0, 0, SRCCOPY);

	strcpy(str, SC(136, "View"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 4, str, strlen(str));
	strcpy(str, SC(137, "gerber"));
	GetTextExtentPoint32(OutputDisplay, str, strlen(str), &TextSize);
	TextOutUTF8(OutputDisplay, x1 + (ButtonSizeX - TextSize.cx) / 2, y1 + 20, str, strlen(str));
	SelectObject(OutputDisplay, old2);

	return old;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawMainWindow()
{
	int32 minx, maxx, miny, maxy, res, RegType, r, g, b;
	HGDIOBJ old, SavePen, SaveBrush;
	LOGBRUSH TempBrushObject, TempBrushObject2;
	HBRUSH TempBrush, TempBrush2;
	HPEN TempPen, TempPen2;
	HDC ButtonMemoryDC;
	char str[100], KeyValue[100];
	int32 cnt = sizeof(KeyValue);
	RECT Rect;
	HKEY Key;


	minx = DrawWindowMinX;
	maxx = DrawWindowMaxX;
	miny = DrawWindowMinY;
	maxy = DrawWindowMaxY;

	if (!ButtonBackGroundColor)
	{
		res = RegOpenKeyEx(HKEY_CURRENT_USER, "Control Panel\\Colors", 0, KEY_ALL_ACCESS, &Key);

		if (res == ERROR_SUCCESS)
		{
			res = RegQueryValueEx(Key, "Menu", NULL, &RegType, (LPBYTE) & KeyValue, (DWORD *) &cnt);

			if (sscanf(KeyValue, "%d %d %d", &r, &g, &b) == 3)
				ButtonBackGroundColor = RGB(r, g, b);
			else
				ButtonBackGroundColor = RGB(192, 192, 192);
		}
	}

	TempBrushObject.lbHatch = (LONG) NULL;
	TempBrushObject.lbStyle = BS_SOLID;
	TempBrushObject.lbColor = ButtonBackGroundColor;
	TempBrush = CreateBrushIndirect(&TempBrushObject);
	TempPen = CreatePen(PS_SOLID, 1, ButtonBackGroundColor);
	SavePen = SelectObject(OutputDisplay, TempPen);
	SaveBrush = SelectObject(OutputDisplay, TempBrush);
	Rect.left = minx;
	Rect.right = maxx;
	Rect.top = miny;
	Rect.bottom = maxy;
	Rectangle(OutputDisplay, minx, miny, maxx, maxy);
//  FillRect(OutputDisplay,&Rect,GetStockObject(LTGRAY_BRUSH));
	DeleteObject(SavePen);
	DeleteObject(SaveBrush);

	ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
	SelectClipRgn(OutputDisplay, NULL);


	old = DrawBackground(ButtonMemoryDC, 0);
	DrawBackAnnotateButton(ButtonMemoryDC, 0);
	DrawSymbolButton(ButtonMemoryDC, 0);
	DrawSchematicButton(ButtonMemoryDC, 0);
	DrawAnnotateButton(ButtonMemoryDC, 0);
	DrawNetlistButton(ButtonMemoryDC, 0);
	DrawLayoutButton(ButtonMemoryDC, 0);
	DrawGeometryButton(ButtonMemoryDC, 0);
	DrawBOMButton(ButtonMemoryDC, 0);
	DrawGerberButton(ButtonMemoryDC, 0);

	SelectObject(ButtonMemoryDC, old);
	DeleteDC(ButtonMemoryDC);
	SelectObject(OutputDisplay, GetStockObject(NULL_BRUSH));
//  LineColor=RGB(255,255,255);

	TempBrushObject2.lbHatch = (LONG) NULL;
	TempBrushObject2.lbStyle = BS_SOLID;
	TempBrushObject2.lbColor = RGB_Blue;
	TempBrush2 = CreateBrushIndirect(&TempBrushObject2);
	TempPen2 = CreatePen(PS_SOLID, 1, RGB_Blue);


//  Rectangle(OutputDisplay,0,0,DrawWindowMaxX,DrawWindowMaxY);
//  DrawLine(0,DrawWindowMaxY-MessageHeight-6,
//           DrawWindowMaxX,DrawWindowMaxY-MessageHeight-6);
	SelectObject(OutputDisplay, GetStockObject(WHITE_PEN));
//  Rectangle(OutputDisplay,1,MessageStartY-22,
//            DrawWindowMaxX-1,DrawWindowMaxY-1);
	Rectangle(OutputDisplay, 1, 1, DrawWindowMaxX - 1, MessageStartY - 24);
	SelectObject(OutputDisplay, GetStockObject(BLACK_PEN));
	LineColor = RGB_Black;
//  Rectangle(OutputDisplay,0,MessageStartY-23,
//            DrawWindowMaxX,DrawWindowMaxY);
	Rectangle(OutputDisplay, 0, 0, DrawWindowMaxX, MessageStartY - 23);
	SelectObject(OutputDisplay, TempPen2);
	SelectObject(OutputDisplay, TempBrush2);
	Rectangle(OutputDisplay, 2, MessageStartY - 21, DrawWindowMaxX - 2, MessageStartY - 1);
	SelectObject(OutputDisplay, SavePen);
	SelectObject(OutputDisplay, SaveBrush);
	DeleteObject(TempPen);
	DeleteObject(TempPen2);
	DeleteObject(TempBrush);
	DeleteObject(TempBrush2);
	SetBkMode(OutputDisplay, TRANSPARENT);
	SetTextColor(OutputDisplay, RGB_White);
	SelectObject(OutputDisplay, GetStockObject(SYSTEM_FONT));
	sprintf(str, SC(138, "Messages"));
	TextOutUTF8(OutputDisplay, 6, MessageStartY - 20, str, strlen(str));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawButtonInfoOff(int32 mode)
{
	if (DisplayButtonNr)
	{
		InvalidateRect(DESIGNWindow, &ButtonRect, 0);
		UpdateWindow(DESIGNWindow);
	}

	if (mode == 0)
		NrButtonInfo = -1;

	DisplayButtonNr = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetButtonNrOnMousePosition()
{
	int32 cntx, cnty, x1, y1, nr;
	/*
	int16  ButtonSizeX=85;
	int16  ButtonSizeY=65;
	int16  ButtonDivX=155;
	int16  ButtonDivY=93;
	int16  ButtonStartX=33;
	int16  ButtonStartY=22;
	int16  ButtonBitmapStartX=85;
	int16  ButtonBitmapStartY=20;
	int16  ButtonBitmapSizeX=495;
	int16  ButtonBitmapSizeY=204;
	*/
	y1 = ButtonBitmapStartY + ButtonStartY;

	for (cnty = 0; cnty < 3; cnty++)
	{
		x1 = ButtonBitmapStartX + ButtonStartX;

		for (cntx = 0; cntx < 5; cntx++)
		{
			CurrentButtonRect.left = x1;
			CurrentButtonRect.top = y1;
			CurrentButtonRect.right = x1 + ButtonSizeX;
			CurrentButtonRect.bottom = y1 + ButtonSizeY;

			if ((MousePosX > CurrentButtonRect.left) && (MousePosX < CurrentButtonRect.right)
			        && (MousePosY > CurrentButtonRect.top) && (MousePosY < CurrentButtonRect.bottom))
			{
				nr = cnty * 5 + cntx;

				switch (nr)
				{
				case 2:
				case 5:
				case 6:
				case 7:
				case 8:
				case 9:
				case 10:
				case 11:
					return nr;
					break;

				case 14:
					return nr;
					break;
				}
			}

			x1 += ButtonDivX;
		}

		y1 += ButtonDivY;
	}

	return -1;
}

//****************************************************************************************************************************************
//****************************** pøeklad Button Info *************************************************************************************
//****************************************************************************************************************************************

void ButtonInfo()
{
	int32 NewButtonNr, val, sx, sy;
	HGDIOBJ SaveFont, SavePen, SaveBrush;
	char str[100], str2[100];
	LOGBRUSH ButtonInfoBrushObject;
	HBRUSH ButtonInfoBrush;
	HPEN ButtonInfoPen;

	SIZE NewTextSize, NewTextSize2;

// String
	if (FirstPaint)
		return;

	val = GetButtonNrOnMousePosition();

	if (val >= 0)
	{
		NewButtonNr = val;

		if (NewButtonNr != NrButtonInfo)
		{
			DrawButtonInfoOff(1);
			NrButtonInfo = NewButtonNr;
			TimeOut = TimerValue;
		}
		else
		{
			if (TimerValue > TimeOut + 5)
			{
				if (!DisplayButtonNr)
				{
					DisplayButtonNr = 1;
					StartDrawingEditingWindow();

					ButtonInfoBrushObject.lbHatch = (LONG) NULL;
					ButtonInfoBrushObject.lbStyle = BS_SOLID;
					ButtonInfoBrushObject.lbColor = RGB_Yellow;
					ButtonInfoBrush = CreateBrushIndirect(&ButtonInfoBrushObject);
					ButtonInfoPen = CreatePen(PS_SOLID, 1, RGB_White);

					SetTextColor(OutputDisplay, RGB_Black);
					SetBkMode(OutputDisplay, TRANSPARENT);
					SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
					str2[0] = 0;
					NewTextSize2.cx = 0;

					switch (val)
					{
					case 2:
						strcpy(str, SC(139, "Start back annotation"));
						break;

					case 5:
						strcpy(str, SC(140, "Start symbol editor"));
						break;

					case 6:
						strcpy(str, SC(141, "Leftbutton -> Start schematic editor with top sheet"));
						strcpy(str2, SC(142, "Rightbutton -> Start schematic editor with a sheet"));
						break;

					case 7:
						strcpy(str, SC(143, "Generate annotation sheets"));
						break;

					case 8:
						strcpy(str, SC(144, "Generate building netlist/components from sheets"));
						break;

					case 9:
						strcpy(str, SC(145, "Start layout editor with current design"));
						break;

					case 10:
						strcpy(str, SC(109, "Generate bill of materials"));
						break;

					case 11:
						strcpy(str, SC(146, "Start geometry editor"));
						break;

					case 14:
						strcpy(str, SC(147, "Start gerber viewer"));
						break;
					}

					GetTextExtentPoint32(OutputDisplay, str, strlen(str), &NewTextSize);

					if (str2[0] != 0)
						GetTextExtentPoint32(OutputDisplay, str2, strlen(str2), &NewTextSize2);

					sx = 270;
					sy = DrawWindowMaxY - MessageHeight - 72;
					ButtonRect.left = sx;
					ButtonRect.right = sx + max(NewTextSize.cx, NewTextSize2.cx) + 10;
					ButtonRect.top = sy;

					if (str2[0] == 0)
						ButtonRect.bottom = sy + 18;
					else
						ButtonRect.bottom = sy + 36;

					SavePen = SelectObject(OutputDisplay, ButtonInfoPen);
					SaveBrush = SelectObject(OutputDisplay, ButtonInfoBrush);
					RoundRect(OutputDisplay, ButtonRect.left, ButtonRect.top, ButtonRect.right, ButtonRect.bottom, 5,
					          5);
					TextOutUTF8(OutputDisplay, ButtonRect.left + 2, ButtonRect.top + 2, str, strlen(str));

					if (str2[0] != 0)
						TextOutUTF8(OutputDisplay, ButtonRect.left + 2, ButtonRect.top + 16, str2, strlen(str2));

					SelectObject(OutputDisplay, SaveFont);
					SelectObject(OutputDisplay, SavePen);
					SelectObject(OutputDisplay, SaveBrush);
					EndDrawingEditingWindow();
					DeleteObject(ButtonInfoPen);
					DeleteObject(ButtonInfoBrush);
				}
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
	int32 val, sx, sy;
	HGDIOBJ old;
	HPEN DrawPen;
	HDC ButtonMemoryDC;
	POINT PP;

	old = NULL;
	val = GetButtonNrOnMousePosition();

	if ((val >= 0) || (mode == 1))
	{
		if (mode == 0)
		{
			StartDrawingEditingWindow();
			ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
			SelectClipRgn(OutputDisplay, NULL);

			/*
			      old=SelectObject(ButtonMemoryDC,ButtonBitmap);
			      res=BitBlt(OutputDisplay,CurrentButtonRect.left+2,CurrentButtonRect.top+1,
			                 ButtonSizeX-4,ButtonSizeY-3,
			                 ButtonMemoryDC,
			                 CurrentButtonRect.left-ButtonBitmapStartX,
			                 CurrentButtonRect.top-ButtonBitmapStartY,
			                 SRCCOPY);
			*/
			switch (val)
			{
			case 2:
				old = DrawBackAnnotateButton(ButtonMemoryDC, 1);
				break;

			case 5:
				old = DrawSymbolButton(ButtonMemoryDC, 1);
				break;

			case 6:
				old = DrawSchematicButton(ButtonMemoryDC, 1);
				break;

			case 7:
				old = DrawAnnotateButton(ButtonMemoryDC, 1);
				break;

			case 8:
				old = DrawNetlistButton(ButtonMemoryDC, 1);
				break;

			case 9:
				old = DrawLayoutButton(ButtonMemoryDC, 1);
				break;

			case 10:
				old = DrawBOMButton(ButtonMemoryDC, 1);
				break;

			case 11:
				old = DrawGeometryButton(ButtonMemoryDC, 1);
				break;

			case 14:
				old = DrawGerberButton(ButtonMemoryDC, 1);
				break;
			}

			SelectObject(ButtonMemoryDC, old);
			DeleteDC(ButtonMemoryDC);

			sx = CurrentButtonRect.left;
			sy = CurrentButtonRect.top - 3;

			SelectObject(OutputDisplay, GetStockObject(BLACK_PEN));
			MoveToEx(OutputDisplay, sx, sy + 3, &PP);
			LineTo(OutputDisplay, sx + ButtonSizeX, sy + 3);
			MoveToEx(OutputDisplay, sx, sy + 3, &PP);
			LineTo(OutputDisplay, sx, sy + ButtonSizeY + 4);

			DrawPen = CreatePen(PS_SOLID, 1, RGB_Gray);
			old = SelectObject(OutputDisplay, DrawPen);
			MoveToEx(OutputDisplay, sx + 1, sy + 4, &PP);
			LineTo(OutputDisplay, sx + ButtonSizeX, sy + 4);
			MoveToEx(OutputDisplay, sx + 1, sy + 4, &PP);
			LineTo(OutputDisplay, sx + 1, sy + ButtonSizeY + 2);
			SelectObject(OutputDisplay, old);

			SelectObject(OutputDisplay, GetStockObject(WHITE_PEN));
			MoveToEx(OutputDisplay, sx + 1, sy + ButtonSizeY + 3, &PP);
			LineTo(OutputDisplay, sx + ButtonSizeX, sy + ButtonSizeY + 3);
			MoveToEx(OutputDisplay, sx + ButtonSizeX - 1, sy + 4, &PP);
			LineTo(OutputDisplay, sx + ButtonSizeX - 1, sy + ButtonSizeY + 3);

			DeleteObject(DrawPen);

			EndDrawingEditingWindow();
			return;
		}
		else
		{
			StartDrawingEditingWindow();
			ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
			SelectClipRgn(OutputDisplay, NULL);

			old = DrawBackground(ButtonMemoryDC, 0);
			DrawBackAnnotateButton(ButtonMemoryDC, 0);
			DrawSymbolButton(ButtonMemoryDC, 0);
			DrawSchematicButton(ButtonMemoryDC, 0);
			DrawAnnotateButton(ButtonMemoryDC, 0);
			DrawNetlistButton(ButtonMemoryDC, 0);
			DrawLayoutButton(ButtonMemoryDC, 0);
			DrawGeometryButton(ButtonMemoryDC, 0);
			DrawBOMButton(ButtonMemoryDC, 0);
			DrawGerberButton(ButtonMemoryDC, 0);

			SelectObject(ButtonMemoryDC, old);
			DeleteDC(ButtonMemoryDC);
			EndDrawingEditingWindow();

			switch (val)
			{
			case 2:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_BACKANNOTATE, (LPARAM) NULL);
				break;

			case 5:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_SYMBOL_OPEN, (LPARAM) NULL);
				break;

			case 6:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_SHEET_OPEN_TOP_SHEET, (LPARAM) NULL);
				break;

			case 7:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_ANNOTATE, (LPARAM) NULL);
				break;

			case 8:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_BUILD_NETLIST, (LPARAM) NULL);
				break;

			case 9:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_OPEN_LAYOUT_EDITOR, (LPARAM) NULL);
				break;

			case 10:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_EDIT_BILLOFMATERIALS, (LPARAM) NULL);
				break;

			case 11:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_OPEN_GEOM_EDITOR, (LPARAM) NULL);
				break;

			case 14:
				PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_VIEW_GERBER_FILES, (LPARAM) NULL);
				break;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MainLoop()
{
	/*
	  if ((MouseChanged)
	     &&
	     (!WindowMinimized)) {
	     DisplayCursorPosition();
	  }

	*/
	CurrentDrawMode = 0;

	OldMousePosX = MousePosX;
	OldMousePosY = MousePosY;
	ButtonInfo();

	if (LeftButtonPressed)
	{
		DrawButtonInfoOff(0);
		CheckButtonPressed(0);

		while ((LeftButtonPressed) && (Focused))
			CheckInputMessages();

		LeftButtonPressed = 0;
		CheckButtonPressed(1);
	}

	if (MouseChanged)
		MouseChanged = 0;

	if (RightButtonPressed)
	{
		if (DesignActive)
		{
			DrawButtonInfoOff(0);

			if (GetButtonNrOnMousePosition() == 6)
				MenuPopUp(0);

			if (GetButtonNrOnMousePosition() == 9)
				MenuPopUp(1);
		}
		else
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

		RightButtonPressed = 0;
		CheckInputMessages();
	}

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckInputMessages(void)
{
	MSG M;

	if (GetMessage(&M, 0, 0, 0))
	{
		TranslateMessage(&M);
		DispatchMessage(&M);
	}
	else
	{
		SelectionEsc = 1;
		RightButtonPressed = 0;
		LeftButtonPressed = 0;
		TotalExit = 1;
		ok = 1;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MakeNewProject(int32 mode)
{
	int32 fp, cnt, res, Found;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING],
	     FileName3[MAX_LENGTH_STRING], Line[MAX_LENGTH_STRING];

	if ((!SetCurrentDirectoryUTF8(DesignPath)) && (!CreateDirectoryUTF8(DesignPath)))
	{
		MessageBoxUTF8(DESIGNWindow, DesignPath, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\backup", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\pcb", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\pcb\\dxf", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\pcb\\gerber", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\pcb\\hpgl", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\pcb\\backup", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\pcb\\shapes", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\pcb\\shapes\\backup", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\sch", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\sch\\backup", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\sym", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

	sprintf(FileName, "%s\\sym\\backup", DesignPath);

	if ((!SetCurrentDirectoryUTF8(FileName)) && (!CreateDirectoryUTF8(FileName)))
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(148, "Can not create directory"), MB_APPLMODAL | MB_OK);
		return;
	}

// ***********************************************************************************
	strcpy(LayoutFile, DesignName);
	sprintf(FileName, "%s\\%s.dsn", DesignPath, DesignName);
	strcpy(DesignFile, FileName);

	if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(83, "Error in writing file"), MB_APPLMODAL | MB_OK);
		return;
	}

	WriteLn(fp, "");
	strcpy(Line, "[TopSheet]");
	WriteLn(fp, Line);
	WriteLn(fp, "");
	sprintf(Line, "\"%s\"", TopSheetName);
	WriteLn(fp, Line);

	WriteLn(fp, "");
	strcpy(Line, "[LayoutFile]");
	WriteLn(fp, Line);
	WriteLn(fp, "");
	sprintf(Line, "\"%s\"", LayoutFile);
	WriteLn(fp, Line);

	WriteLn(fp, "");
	WriteLn(fp, "[Settings]");
	WriteLn(fp, "");
	sprintf(Line, "SaveSymbolsLocally=%i", SaveSymbolsLocally);
	WriteLn(fp, Line);
	sprintf(Line, "DisableOnePinNetCheck=%i", DisableOnePinNetCheck);
	WriteLn(fp, Line);
	WriteLn(fp, "");
	FileClose(fp);


	if (WriteLnError != 0)
	{
		sprintf(str2, SC(85, "Could not write to file %s"), FileName);
		MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
		return;
	}

// ***********************************************************************************

// Copy ini files

	sprintf(FileName, "%s\\sch.ini", ExePath);
	sprintf(FileName2, "%s\\sch.ini", ProjectPath);
	sprintf(FileName3, "%s\\sch.ini", DesignPath);

	if (FileExistsUTF8(FileName) == 0)
	{
		if (FileExistsUTF8(FileName2) == 0)
			CopyFileUTF8(FileName2, FileName3, 0);
		else
			CopyFileUTF8(FileName, FileName3, 0);
	}

	sprintf(FileName, "%s\\geom.ini", ExePath);
	sprintf(FileName2, "%s\\geom.ini", ProjectPath);
	sprintf(FileName3, "%s\\geom.ini", DesignPath);

	if (FileExistsUTF8(FileName) == 0)
	{
		if (FileExistsUTF8(FileName2) == 0)
			CopyFileUTF8(FileName2, FileName3, 0);
		else
			CopyFileUTF8(FileName, FileName3, 0);
	}

	sprintf(FileName, "%s\\pcb.ini", ExePath);
	sprintf(FileName2, "%s\\pcb.ini", ProjectPath);
	sprintf(FileName3, "%s\\pcb.ini", DesignPath);

	if (FileExistsUTF8(FileName) == 0)
	{
		if (FileExistsUTF8(FileName2) == 0)
			CopyFileUTF8(FileName2, FileName3, 0);
		else
			CopyFileUTF8(FileName, FileName3, 0);
	}

	sprintf(FileName, "%s\\viewplot.ini", ExePath);
	sprintf(FileName2, "%s\\viewplot.ini", ProjectPath);
	sprintf(FileName3, "%s\\viewplot.ini", DesignPath);

	if (FileExistsUTF8(FileName) == 0)
	{
		if (FileExistsUTF8(FileName2) == 0)
			CopyFileUTF8(FileName2, FileName3, 0);
		else
			CopyFileUTF8(FileName, FileName3, 0);
	}

// ***********************************************************************************
// ***********************************************************************************
	sprintf(FileName, "%s\\sch\\%s.sch", DesignPath, TopSheetName);

	if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(83, "Error in writing file"), MB_APPLMODAL | MB_OK);
		return;
	}

	memset(&Design, 0, sizeof(DesignRecord));
	strcpy(Design.Identification, SheetCode1);
	FileWrite(fp, &Design, sizeof(DesignRecord), &res);
	FileClose(fp);

// ***********************************************************************************
// ***********************************************************************************

	sprintf(FileName, "%s\\%s.prp", DesignPath, DesignName);

	if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(83, "Error in writing file"), MB_APPLMODAL | MB_OK);
		return;
	}

	WriteLn(fp, ";");
	WriteLn(fp, ";Example:");
	WriteLn(fp, ";");
	WriteLn(fp, ";clk \"testing\"=\"1\")");
	WriteLn(fp, ";");
	FileClose(fp);

// ***********************************************************************************
// ***********************************************************************************

	sprintf(FileName, "%s\\%s.var", DesignPath, DesignName);

	if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
	{
		MessageBoxUTF8(DESIGNWindow, FileName, SC(83, "Error in writing file"), MB_APPLMODAL | MB_OK);
		return;
	}

	WriteLn(fp, ";");
	WriteLn(fp, ";Example:");
	WriteLn(fp, ";");
	WriteLn(fp, ";$user=\"John Smith\"");
	WriteLn(fp, ";");
	WriteLn(fp, "$NrSheets=\"1\"");
	FileClose(fp);

// ***********************************************************************************
// ***********************************************************************************
	DesignActive = 1;
	sprintf(str, SC(10, "Design %s"), DesignFile);
	SetWindowTextUTF8(DESIGNWindow, str);
	res = sizeof(LastDesigns[0]);

	Found = -1;

	for (cnt = 0; cnt < NrDesigns; cnt++)
	{
		if (stricmpUTF8(DesignFile, LastDesigns[cnt]) == 0)
			Found = cnt;
	}

	if (Found == -1)
	{
		if (NrDesigns > 0)
			memmove(&(LastDesigns[1]), &(LastDesigns[0]), (min(TotalNrDesigns - 1, NrDesigns) * res));

		strncpy(LastDesigns[0], DesignFile, res - 1);
		LastDesigns[0][res - 1] = 0;

		if (NrDesigns < TotalNrDesigns)
			NrDesigns++;
	}
	else
	{
		if (NrDesigns > 1)
		{
			memmove(&LastDesigns[1], &LastDesigns[0], Found * sizeof(LastDesigns[0]));
			strncpy(LastDesigns[0], DesignFile, res - 1);
			LastDesigns[0][res - 1] = 0;
		}
	}

	UpdateFileMenu(1);
	sprintf(str, SC(149, "Design %s created\r\n"), DesignFile);
	AddMessage(str);
//  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
	sprintf(str2,
	        SC(150, "The initial settings of geom.ini,sch.ini,pcb.ini and viewplot.ini will be copied (if exists)"));
	sprintf(str, SC(151, "%s from %s\r\n"), str2, ProjectPath);
	AddMessage(str);
//  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
