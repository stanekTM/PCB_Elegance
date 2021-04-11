/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: geom.c
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
#include "line2.h"
#include "stdio.h"
#include "rect.h"
#include "memory.h"
#include "geom.h"
#include "direct.h"
#include "string.h"
#include "stdlib.h"
#include "toets.h"
#include "insdel.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "calc.h"
#include "io.h"
#include "calcdef.h"
#include "menus.h"
#include "files.h"
#include "files2.h"
#include "dialogs.h"
#include "ellipss.h"
#include "mainloop.h"
#include "resource.h"
#include "command.h"
#include "graphics.h"
#include "utf8.h"
#include "params.h"
#include "owntime.h"
#include "own_process.h"

#define WANT_GETLONGPATHNAME_WRAPPER
#define COMPILE_NEWAPIS_STUBS

#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL                   0x020A
#endif

#define  SCROLL_FACTOR                  2.0
#define  MEMORYMAPPEDSTRING             "MMFILE_PCB_ELEGANCE"

#define PCB_ELEG_ENVIRONMENT_STRING     "PCB_ELEG_ENVIRONMENT"

HCURSOR SystemCursor;
HPALETTE OldPalette;
HCURSOR OldCursor, Cursor1;
HWND GEOMWindow, DialogWindow, MasterWindow, hwndHBar, hwndVBar;
HDC OutputDisplay, OutputDisplay2;
HBITMAP ViewBitmap;
HRSRC FontResourceHandle;
HGLOBAL FontGlobal;
uint8 *FontData;

PAINTSTRUCT PS;
RECT RealWindow, ClientRect, UpdateRect, OldClientRect, OldRealWindow2, RealWindow2, OldRealWindow;
HRGN EditingRegion, TempRegion;


int32 GEOMCursorActive, ThumbChanged;
int32 FirstView, WindowInitialSized;
int32 WindowInitialSized;
int32 FocusedAgain = 1;
int32 FirstPaintMode = 1;
int32 TotalExit;
int32 RepaintBecauseOfResize;
int32 PaintBecauseOfResize;
int32 WindowMaximized;
int32 WindowMinimized;
int32 StartFromLibraryManager;
int32 PaintIntro;

int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY, TimerInfoStr, ScreenSizeX, ScreenSizeY,
      ScrollBarSizeX, ScrollBarSizeY, ScreenStartX, ScreenStartY, HeightScrollBar, WidthScrollBar, OldWindowWidth,
      StartTimerValue, ScreenPosAbsCursor, ScreenPosAbsGridCursor, MousePosOldX, MousePosOldY, ScreenPosRelGridCursor,
      ScreenPosInfoStr;

int32 HeightInfoBar = 21;
int32 WidthButtonBar = 32;
int32 PaintBecauseOfResizeMode;
int32 PaintCount;
int32 ZoomCounter;
int32 TimerValue;
int32 NrPaintMessages;
int32 LibrarySymbolNr = -1;
int32 FileCheck1 = 1234;
int32 WaitForPaint;
int32 WindowWidth2, WindowHeight2, WindowStartX2, WindowStartY2;
int32 OkToUseSharedMemory;
int32 ProjectActive;
int32 ProjectIndexNr = -1;
int32 FoundProjectPath;
int32 FoundDesignPath;
int32 FastPaint = 1;
int32 ViewPixelsX;
int32 ViewPixelsY;

uint32 TimerObject, ClipBoardGeomObjectsID;
uint32 TimerIdentifier = 0x12345678;

double DefaultRuleSolderMask_TH = 0.1e5;
double DefaultRulePasteMask_SMD = 0.0;
double DefaultRulePad = 0.8e5;
double DefaultRuleSolderMask_SMD = 0.1e5;
double DefaultRuleAntiPowerPad = 0.75e5;
double DefaultRuleInnerPad = 0.55e5;
double DefaultRuleClearance = 0.0;
double OldPos;

char IniFile[MAX_LENGTH_STRING], CurrentDir[MAX_LENGTH_STRING], StartDir[MAX_LENGTH_STRING],
     ExecutableDir[MAX_LENGTH_STRING], GeomExecutable[MAX_LENGTH_STRING], ProjectPath[MAX_LENGTH_STRING],
     LibraryFile[MAX_LENGTH_STRING], LanguagePath[MAX_LENGTH_STRING], UserIniFile[MAX_LENGTH_STRING],
	 UserIniFilePath[MAX_LENGTH_STRING];
ProjectInfoRecord *ProjectInfo = NULL;
uint8 *SharedMemory;
HANDLE *SharedMemoryHandle;
WIN32_FIND_DATA FindData;

OSVERSIONINFO OSInfo;

extern int32 ButtonSizeX;
extern int32 ButtonSizeY;
extern int32 NrButtons;
extern COLORREF BackGroundColor;
extern HGDIOBJ BackGroundBrush;

// *******************************************************************************************************

#ifndef CRC_MESSAGE
#define  TIMER_TICK_DIV                 2000
#else
#define  TIMER_TICK_DIV                 50000
#endif

// *******************************************************************************************************

#define   ScreenPosAbsCursorInit         10
#define   ScreenPosAbsGridCursorInit     170
#define   ScreenPosRelGridCursorInit     350
#define   ScreenPosInfoStrInit           590

WNDCLASS GEOMClass = { CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS,
                       (WNDPROC) GEOMWinProc,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       "GEOM"
                     };

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteGraphicObjects(void);

void WriteIniFile(void);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InsertWindowInProject(HWND Window, int32 mode)
{
	int32 cnt;

	if (ProjectInfo == NULL)
		return;

	cnt = 0;

	while ((cnt < 32) && (ProjectInfo->FileTypes[cnt] != 0))
		cnt++;

	if (cnt == 32)
		return;

	ProjectIndexNr = cnt;
	ProjectInfo->FileTypes[cnt] = 3;
	ProjectInfo->WindowHandles[cnt] = Window;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WindowCreate()
{
	ClipCursor(NULL);
	Created = 1;

	Xoffset = 0.0;
	Yoffset = 0.0;
	GEOMCursorActive = 0;
	SystemCursor = LoadCursor(NULL, IDC_ARROW);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WindowDestroy()
{
	PostQuitMessage(0);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AdjustWindowSize()
{
	POINT TestPixel;
	HWND TestWindow1, TestWindow2, TestWindow3, TestWindow4;

	if (WindowInitialSized)
	{
		WindowStartX = WindowStartX2;
		WindowStartY = WindowStartY2;
		WindowWidth = WindowWidth2;
		WindowHeight = WindowHeight2;
	}

	if (WindowWidth == 0)
	{
		WindowStartX = 0;
		WindowStartY = 0;
		WindowWidth = ScreenSizeX;
		WindowHeight = ScreenSizeY;
		return;
	}

	TestPixel.x = WindowStartX;
	TestPixel.y = WindowStartY;
	TestWindow1 = WindowFromPoint(TestPixel);
	TestPixel.x = WindowStartX + WindowWidth - 1;
	TestPixel.y = WindowStartY;
	TestWindow2 = WindowFromPoint(TestPixel);
	TestPixel.x = WindowStartX + WindowWidth - 1;
	TestPixel.y = WindowStartY + WindowHeight - 1;
	TestWindow3 = WindowFromPoint(TestPixel);
	TestPixel.x = WindowStartX;
	TestPixel.y = WindowStartY + WindowHeight - 1;
	TestWindow4 = WindowFromPoint(TestPixel);

	if ((TestWindow1 == NULL) || (TestWindow2 == NULL) || (TestWindow3 == NULL) || (TestWindow4 == NULL))
	{
		WindowStartX = 0;
		WindowStartY = 0;
		WindowWidth = ScreenSizeX;
		WindowHeight = ScreenSizeY;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitDeviceContext()
{
	if (!DCInUse)
	{
		DCInUse = 1;

		if (Painting)
		{
			OutputDisplay = BeginPaint(GEOMWindow, &PS);
			SetArcDirection(OutputDisplay, AD_COUNTERCLOCKWISE);
		}
		else
			OutputDisplay = GetDC(GEOMWindow);

		SetBkMode(OutputDisplay, TRANSPARENT);
		SetROP2(OutputDisplay, R2_COPYPEN);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitDeviceContext2()
{
	if (!DCInUse)
	{
		DCInUse = 1;

		if (Painting)
		{
			OutputDisplay2 = BeginPaint(GEOMWindow, &PS);
			SetArcDirection(OutputDisplay2, AD_COUNTERCLOCKWISE);
		}
		else
			OutputDisplay2 = GetDC(GEOMWindow);

		OutputDisplay = CreateCompatibleDC(0);
		SetArcDirection(OutputDisplay, AD_COUNTERCLOCKWISE);
		SetBkMode(OutputDisplay, TRANSPARENT);
		SetROP2(OutputDisplay, R2_COPYPEN);

		if (ViewPixelsX == 0)
		{
			ViewPixelsX = GetDeviceCaps(OutputDisplay2, HORZRES);
			ViewPixelsY = GetDeviceCaps(OutputDisplay2, VERTRES);
		}

		ViewBitmap = CreateCompatibleBitmap(OutputDisplay2, ViewPixelsX, ViewPixelsY);
		SelectObject(OutputDisplay, ViewBitmap);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void StartDrawingEditingWindow()
{
	InitDeviceContext();
	EditingRegion = CreateRectRgn(DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY);
	SelectClipRgn(OutputDisplay, EditingRegion);
	SetROP2(OutputDisplay, R2_COPYPEN);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DoneDeviceContext()
{
	if (DCInUse)
	{
		if (Painting)
			EndPaint(GEOMWindow, &PS);
		else
			ReleaseDC(GEOMWindow, OutputDisplay);

		DCInUse = 0;
		OutputDisplay = NULL;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DoneDeviceContext2()
{

 //int32 res;

	if (DCInUse)
	{

     //res=GetDIBits(OutputDisplay,ViewBitmap,0,ViewPixelsY,TempMem,BitmapInfo,DIB_RGB_COLORS);

		BitBlt(OutputDisplay2, 0, 0, ViewPixelsX, ViewPixelsY, OutputDisplay, 0, 0, SRCCOPY);
		GdiFlush();

		if (ViewBitmap)
			DeleteObject(ViewBitmap);

		DeleteDC(OutputDisplay);

		if (Painting)
			EndPaint(GEOMWindow, &PS);
		else
			ReleaseDC(GEOMWindow, OutputDisplay2);

		DCInUse = 0;
		ViewBitmap = NULL;
		OutputDisplay = NULL;
		OutputDisplay2 = NULL;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void EndDrawingEditingWindow()
{
	SelectClipRgn(OutputDisplay, NULL);
	DeleteObject(EditingRegion);
	EditingRegion = NULL;
	DoneDeviceContext();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetScrollPageSize()
{
	SCROLLINFO ScrollBar;
	double hulp2, dx, dy;

	memset(&ScrollBar, 0, sizeof(SCROLLINFO));
	ScrollBar.cbSize = sizeof(SCROLLINFO);
	ScrollBar.fMask = SIF_PAGE;

	dx = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
	hulp2 = (dx / ((VisibleMaxX - VisibleMinX) * SCROLL_FACTOR)) * MaxScrollBarX;
	ScrollBarSizeX = (int32) hulp2;
	ScrollBarSizeX = min(ScrollBarSizeX, MaxScrollBarX - 200);
	ScrollBar.nPage = ScrollBarSizeX;
	SetScrollInfo(hwndHBar, SB_CTL, &ScrollBar, 1);

	memset(&ScrollBar, 0, sizeof(SCROLLINFO));
	ScrollBar.cbSize = sizeof(SCROLLINFO);
	ScrollBar.fMask = SIF_PAGE;
	dy = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
	hulp2 = (dy / ((VisibleMaxY - VisibleMinY) * SCROLL_FACTOR)) * MaxScrollBarY;
	ScrollBarSizeY = (int32) hulp2;
	ScrollBarSizeY = min(ScrollBarSizeY, MaxScrollBarY - 200);
	ScrollBar.nPage = ScrollBarSizeY;
	SetScrollInfo(hwndVBar, SB_CTL, &ScrollBar, 1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetScrollPosition()
{
	double dx, dy, ccy, ccx, cx, tx, cy, ty, hulp2;

	dx = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
	dy = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
	ccx = (PixelToRealOffX(DrawWindowMaxX) + PixelToRealOffX(DrawWindowMinX)) * 0.5;
	ccy = (PixelToRealOffY(DrawWindowMaxY) + PixelToRealOffY(DrawWindowMinY)) * 0.5;

	cy = (VisibleMaxY + VisibleMinY) / 2;
	ty = (VisibleMaxY - VisibleMinY) * SCROLL_FACTOR;
	hulp2 = ((ccy - cy) * MaxScrollBarY / ty) + MaxScrollBarY / 2;

	if (hulp2 > MaxScrollBarY)
		hulp2 = MaxScrollBarY;

	if (hulp2 < 0)
		hulp2 = 0.0;

	hulp2 = MaxScrollBarY - hulp2;
	hulp2 = hulp2 * (MaxScrollBarY - ScrollBarSizeY) / MaxScrollBarY;
	SetScrollPos(hwndVBar, SB_CTL, (int32) hulp2, 1);

	cx = (VisibleMaxX + VisibleMinX) / 2;
	tx = (VisibleMaxX - VisibleMinX) * SCROLL_FACTOR;
	hulp2 = ((ccx - cx) * MaxScrollBarX / tx) + MaxScrollBarX / 2;

	if (hulp2 > MaxScrollBarX)
		hulp2 = MaxScrollBarX;

	if (hulp2 < 0)
		hulp2 = 0.0;

	hulp2 = hulp2 * (MaxScrollBarX - ScrollBarSizeX) / MaxScrollBarX;
	SetScrollPos(hwndHBar, SB_CTL, (int32) hulp2, 1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SaveViewPos(void)
{
	if (ViewPosPointer > 0)
	{
		if (ViewPosPointer == 20)
		{
			memmove(&ViewPos[0], &ViewPos[1], 19 * sizeof(ViewPosRecord));
			ViewPosPointer--;
		}

		ViewPos[ViewPosPointer].Xoffset = Xoffset;
		ViewPos[ViewPosPointer].Yoffset = Yoffset;
		ViewPos[ViewPosPointer].Factor = Factor;
		ViewPosPointer++;
	}
	else
	{
		ViewPos[ViewPosPointer].Xoffset = Xoffset;
		ViewPos[ViewPosPointer].Yoffset = Yoffset;
		ViewPos[ViewPosPointer].Factor = Factor;
		ViewPosPointer++;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawMainWindow()
{
	RECT Rect;
	int32 minx, maxx, miny, maxy;
	HRGN hrgn;

	if (FirstView)
	{
		ViewWholeDesign(1);
		FirstView = 0;
	}

	minx = max(UpdateRect.left, DrawWindowMinX);
	maxx = min(UpdateRect.right, DrawWindowMaxX);
	miny = max(UpdateRect.top, DrawWindowMinY);
	maxy = min(UpdateRect.bottom, DrawWindowMaxY);

	hrgn = CreateRectRgn(minx, miny, maxx, maxy);
	SelectClipRgn(OutputDisplay, hrgn);

	Rect.left = minx;
	Rect.right = maxx;
	Rect.top = miny;
	Rect.bottom = maxy;
	FillRect(OutputDisplay, &Rect, BackGroundBrush);

	if (OkToAddViewPos)
		SaveViewPos();

	OkToAddViewPos = 1;
	ViewMinX = PixelToRealOffX(minx - 1);
	ViewMaxX = PixelToRealOffX(maxx + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMaxY - (maxy + 1) - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY - (miny - 1) - 1);



	DrawObjectPolygons(0);
	DrawObjects(0);

	InitDrawingColorGray();
	SetROP2(OutputDisplay, R2_XORPEN);

	DrawLine(-1, DrawWindowMaxY - Mult(-Yoffset) - 1, 4000, DrawWindowMaxY - Mult(-Yoffset) - 1);
	DrawLine(Mult(-Xoffset) + DrawWindowMinX, -10, Mult(-Xoffset) + DrawWindowMinX, 4000);

	/*
	  x=MultX(0.0);
	  y=MultY(0.0);
	  DrawLine(x,y+80,x,y-80);
	  DrawLine(x+80,y,x-80,y);
   */

	if (ViewInsertionPoint)
		DrawInsertionPoint(0);

	if (GridVisible)
		DrawGrid();

	SetScrollPageSize();
	SetScrollPosition();

	ExitDrawing();

    //ValidateRect(GEOMWindow,&Rect);

	SelectClipRgn(OutputDisplay, NULL);
	DeleteObject(hrgn);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawButtons()
{
	RECT Rect;
	HGDIOBJ old;
	HBITMAP ButtonBitmap;
	HDC ButtonMemoryDC;
	int32 res, minx, maxx, miny, maxy;
	HRGN hrgn;

	minx = max(UpdateRect.left, 0);
	maxx = min(UpdateRect.right, DrawWindowMinX);
	miny = max(UpdateRect.top, 0);
	maxy = min(UpdateRect.bottom, ClientRect.bottom - HeightInfoBar);

	hrgn = CreateRectRgn(minx, miny, maxx, maxy);
	SelectClipRgn(OutputDisplay, hrgn);

	Rect.left = 0;
	Rect.right = DrawWindowMinX;
	Rect.top = 0;
	Rect.bottom = ClientRect.bottom - HeightInfoBar;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left++;
	Rect.top++;
	Rect.right--;
	Rect.bottom--;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	ButtonBitmap = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTONS));
	ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
	old = SelectObject(ButtonMemoryDC, ButtonBitmap);
	res = BitBlt(OutputDisplay, 1, 1, 28, ButtonSizeY * NrButtons + 2, ButtonMemoryDC, 0, 0, SRCCOPY);
	SelectObject(ButtonMemoryDC, old);
	DeleteDC(ButtonMemoryDC);
	DeleteObject(ButtonBitmap);
	DeleteObject(hrgn);

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawAbsPosStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosAbsCursor + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ScreenPosAbsGridCursor - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, NULL);

		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));

	Rect.left = LeftTopX;
	Rect.right = RightBottomX;
	Rect.top = LeftTopY;
	Rect.bottom = RightBottomY;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosAbsCursor, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR) & AbsPosStr, strlen(AbsPosStr)); 
	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, EditingRegion);

		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawAbsGridPosStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosAbsGridCursor + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ScreenPosRelGridCursor - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, NULL);

		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));

	Rect.left = LeftTopX;
	Rect.right = RightBottomX;
	Rect.top = LeftTopY;
	Rect.bottom = RightBottomY;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosAbsGridCursor + 2, ClientRect.bottom - (HeightInfoBar - 4),
	           (LPSTR) & AbsGridPosStr, strlen(AbsGridPosStr)); 
	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, EditingRegion);

		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawRelPosStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosRelGridCursor + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ScreenPosInfoStr - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, NULL);

		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));

	Rect.left = LeftTopX;
	Rect.right = RightBottomX;
	Rect.top = LeftTopY;
	Rect.bottom = RightBottomY;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosRelGridCursor + 2, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR) & RelPosStr, strlen(RelPosStr)); 
	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, EditingRegion);

		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawInfoStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosInfoStr + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ClientRect.right - 4;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, NULL);

		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));
	Rect.left = LeftTopX;
	Rect.right = RightBottomX;
	Rect.top = LeftTopY;
	Rect.bottom = RightBottomY;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosInfoStr + 2, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR) & InfoStr, strlen(InfoStr)); //TextOut, LPCSTR
	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:

     //SelectClipRgn(PCBDisplay, EditingRegion);

		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedrawInfoBar()
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 minx, maxx, miny, maxy;
	HRGN hrgn;

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));

	minx = max(UpdateRect.left, 0);
	maxx = min(UpdateRect.right, ClientRect.right);
	miny = max(UpdateRect.top, ClientRect.bottom - HeightInfoBar - HeightScrollBar);
	maxy = min(UpdateRect.bottom, ClientRect.bottom);

	hrgn = CreateRectRgn(minx, miny, maxx, maxy);
	SelectClipRgn(OutputDisplay, hrgn);

	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));
	Rect.left = 0;
	Rect.right = ClientRect.right;
	Rect.top = ClientRect.bottom - HeightInfoBar + 1;
	Rect.bottom = ClientRect.bottom;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	Rect.left = ClientRect.right - WidthScrollBar;
	Rect.right = ClientRect.right;
	Rect.top = ClientRect.bottom - HeightInfoBar - HeightScrollBar - 1;
	Rect.bottom = ClientRect.bottom - HeightInfoBar;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));

	Rect.left = 2;
	Rect.right = ScreenPosAbsGridCursor - 2;
	Rect.top = ClientRect.bottom - 3;
	Rect.bottom = ClientRect.bottom - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left = ScreenPosAbsGridCursor + 1;
	Rect.right = ScreenPosRelGridCursor - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left = ScreenPosRelGridCursor + 1;
	Rect.right = ScreenPosInfoStr - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left = ScreenPosInfoStr + 1;
	Rect.right = ClientRect.right - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));

	Rect.left = 0;
	Rect.right = ClientRect.right;
	Rect.top = ClientRect.bottom - HeightInfoBar;
	Rect.bottom = ClientRect.bottom - HeightInfoBar + 1;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));

	Rect.top = ClientRect.bottom - HeightInfoBar + 3;
	Rect.bottom = ClientRect.bottom - 2;
	Rect.left = ScreenPosAbsGridCursor - 3;
	Rect.right = ScreenPosAbsGridCursor - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left = ScreenPosRelGridCursor - 3;
	Rect.right = ScreenPosRelGridCursor - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left = ScreenPosInfoStr - 3;
	Rect.right = ScreenPosInfoStr - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left = ClientRect.right - 3;
	Rect.right = ClientRect.right - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));

	Rect.left = 1;
	Rect.right = 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));

	Rect.top = ClientRect.bottom - (HeightInfoBar - 3);
	Rect.bottom = ClientRect.bottom - 2;
	Rect.left = ScreenPosAbsGridCursor;
	Rect.right = ScreenPosAbsGridCursor + 1;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));
	Rect.left = ScreenPosRelGridCursor;
	Rect.right = ScreenPosRelGridCursor + 1;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));
	Rect.left = ScreenPosInfoStr;
	Rect.right = ScreenPosInfoStr + 1;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));

	Rect.left = 1;
	Rect.right = ScreenPosAbsGridCursor - 2;
	Rect.top = ClientRect.bottom - HeightInfoBar + 3;
	Rect.bottom = ClientRect.bottom - HeightInfoBar + 4;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));
	Rect.left = ScreenPosAbsGridCursor + 1;
	Rect.right = ScreenPosRelGridCursor - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));
	Rect.left = ScreenPosRelGridCursor + 1;
	Rect.right = ScreenPosInfoStr - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));
	Rect.left = ScreenPosInfoStr + 1;
	Rect.right = ClientRect.right - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));

	RedrawAbsPosStr(2);
	RedrawAbsGridPosStr(2);
	RedrawRelPosStr(2);
	RedrawInfoStr(2);
	SelectObject(OutputDisplay, SaveFont);
	DeleteObject(hrgn);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WindowPaint()
{
	RECT Rect, Rect2;
	int32 ok, res;

	GetClientRect(GEOMWindow, &Rect);

	if (GetUpdateRect(GEOMWindow, &UpdateRect, 0))
	{
		if (PaintBecauseOfResize)
			ok = 1;

		if (!PaintBecauseOfResize)
		{
			Painting = 1;

			if (FastPaint)
				InitDeviceContext2();
			else
				InitDeviceContext();

			if (UpdateRect.bottom >= DrawWindowMaxY + HeightScrollBar)
				RedrawInfoBar();

			if (UpdateRect.left < DrawWindowMinX)
				RedrawButtons();

			if ((UpdateRect.bottom >= DrawWindowMinY) && (UpdateRect.top < DrawWindowMaxY))
				RedrawMainWindow();

			if (FastPaint)
				DoneDeviceContext2();
			else
				DoneDeviceContext();

			Painting = 0;
		}
		else
		{
			Painting = 1;
			InitDeviceContext();
			DoneDeviceContext();
			Painting = 0;
			res = GetUpdateRect(GEOMWindow, &UpdateRect, 0);

			if ((PaintBecauseOfResizeMode & (128 + 64 + 32 + 16 + 8 + 4)) != 0)
			{
				Rect2.left = 0;
				Rect2.right = ClientRect.right;
				Rect2.top = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				Rect2.bottom = ClientRect.bottom;

				/*
				        if ((PaintBecauseOfResizeMode & (4)) != 0) {
				          Rect2.left=OldClientRect.right-WidthScrollBar-1;
				        }
				        if ((PaintBecauseOfResizeMode & (8)) != 0) {
				          Rect2.left=OldClientRect.right-WidthScrollBar-1;
				        }
				        if ((PaintBecauseOfResizeMode & (16+32)) != 0) {
				          Rect2.left=0;
				        }
			   */

				InvalidateRect(GEOMWindow, &Rect2, 0);
				res = GetUpdateRect(GEOMWindow, &UpdateRect, 0);
				Painting = 1;

				if (FastPaint)
					InitDeviceContext2();
				else
					InitDeviceContext();

				RedrawInfoBar();

				if (FastPaint)
					DoneDeviceContext2();
				else
					DoneDeviceContext();

				Painting = 0;
			}

			if ((PaintBecauseOfResizeMode & (128 + 64 + 16 + 2 + 1)) != 0)
			{
				Rect2.left = 0;
				Rect2.right = DrawWindowMinX;
				Rect2.top = 0;
				Rect2.bottom = ClientRect.bottom - HeightInfoBar;

				if ((PaintBecauseOfResizeMode & (16)) != 0)
					Rect2.top = OldClientRect.bottom - HeightInfoBar - 1;

				if ((PaintBecauseOfResizeMode & (128 + 64)) != 0)
					Rect2.top = 0;

				InvalidateRect(GEOMWindow, &Rect2, 0);
				res = GetUpdateRect(GEOMWindow, &UpdateRect, 0);
				Painting = 1;

				if (FastPaint)
					InitDeviceContext2();
				else
					InitDeviceContext();

				RedrawButtons();

				if (FastPaint)
					DoneDeviceContext2();
				else
					DoneDeviceContext();

				Painting = 0;
			}

			if ((PaintBecauseOfResizeMode & (1)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = ClientRect.right;
				Rect2.top = 0;
				Rect2.bottom = abs(ClientRect.bottom - OldClientRect.bottom);
				InvalidateRect(GEOMWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (16)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = ClientRect.right;
				Rect2.top =
				    ClientRect.bottom - HeightInfoBar - HeightScrollBar - abs(ClientRect.bottom - OldClientRect.bottom);
				Rect2.bottom = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				InvalidateRect(GEOMWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (16 + 1)) != 0)
			{
				res = GetUpdateRect(GEOMWindow, &UpdateRect, 0);
				Painting = 1;

				if (FastPaint)
					InitDeviceContext2();
				else
					InitDeviceContext();

				RedrawMainWindow();

				if (FastPaint)
					DoneDeviceContext2();
				else
					DoneDeviceContext();

				Painting = 0;
			}

			if ((PaintBecauseOfResizeMode & (4)) != 0)
			{
				Rect2.left = OldClientRect.right - WidthScrollBar;
				Rect2.right = ClientRect.right - WidthScrollBar;
				Rect2.top = 0;
				Rect2.bottom = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				InvalidateRect(GEOMWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (64)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = DrawWindowMinX + abs(ClientRect.right - OldClientRect.right);
				Rect2.top = 0;
				Rect2.bottom = ClientRect.bottom - HeightInfoBar;
				InvalidateRect(GEOMWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (64 + 4)) != 0)
			{
				res = GetUpdateRect(GEOMWindow, &UpdateRect, 0);
				Painting = 1;

				if (FastPaint)
					InitDeviceContext2();
				else
					InitDeviceContext();

				RedrawMainWindow();

				if (FastPaint)
					DoneDeviceContext2();
				else
					DoneDeviceContext();

				Painting = 0;
			}

         //SetScrollPageSize();
         //SetScrollPosition();

			PaintBecauseOfResizeMode = 0;
		}

		ViewMinX = PixelToRealOffX(-1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(-1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		PaintBecauseOfResize = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ScrollUp(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	switch (SystemBusyMode)
	{
	case 0:
		DrawCrossHair(2);
		break;

	case 3:
		break;

	default:
		DrawCrossHair(16 + 2);
		break;
	}

	plus = PixelToReal(SizeOfScroll);
	Yoffset += plus;
	Rect.left = DrawWindowMinX;
	Rect.top = DrawWindowMinY;
	Rect.right = DrawWindowMaxX;
	Rect.bottom = DrawWindowMaxY;
	Rect2.left = DrawWindowMinX;
	Rect2.top = DrawWindowMinY;
	Rect2.right = DrawWindowMaxX;
	Rect2.bottom = DrawWindowMaxY;
	UpdateWindow(GEOMWindow);
	ScrollWindow(GEOMWindow, 0, SizeOfScroll, &Rect, &Rect2);
	UpdateWindow(GEOMWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ScrollAppWindow(int32 DivX, int32 DivY)
{
	RECT Rect, Rect2;

	switch (SystemBusyMode)
	{
	case 0:
		DrawCrossHair(2);
		break;

	case 3:
		break;

	default:
		DrawCrossHair(16 + 2);
		break;
	}

	Xoffset -= PixelToReal(DivX);
	Yoffset += PixelToReal(DivY);
	Rect.left = DrawWindowMinX;
	Rect.right = DrawWindowMaxX;
	Rect.top = DrawWindowMinY;
	Rect.bottom = DrawWindowMaxY;
	Rect2.left = DrawWindowMinX;
	Rect2.top = DrawWindowMinY;
	Rect2.right = DrawWindowMaxX;
	Rect2.bottom = DrawWindowMaxY;
	UpdateWindow(GEOMWindow);
	ScrollWindow(GEOMWindow, DivX, DivY, &Rect, &Rect2);
	UpdateWindow(GEOMWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ScrollDown(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	switch (SystemBusyMode)
	{
	case 0:
		DrawCrossHair(2);
		break;

	case 3:
		break;

	default:
		DrawCrossHair(16 + 2);
		break;
	}

	plus = PixelToReal(SizeOfScroll);
	Yoffset -= plus;
	Rect.left = DrawWindowMinX;
	Rect.top = DrawWindowMinY;
	Rect.right = DrawWindowMaxX;
	Rect.bottom = DrawWindowMaxY;
	Rect2.left = DrawWindowMinX;
	Rect2.top = DrawWindowMinY;
	Rect2.right = DrawWindowMaxX;
	Rect2.bottom = DrawWindowMaxY;
	UpdateWindow(GEOMWindow);
	ScrollWindow(GEOMWindow, 0, -SizeOfScroll, &Rect, &Rect2);
	UpdateWindow(GEOMWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ScrollLeft(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	switch (SystemBusyMode)
	{
	case 0:
		DrawCrossHair(2);
		break;

	case 3:
		break;

	default:
		DrawCrossHair(16 + 2);
		break;
	}

	plus = PixelToReal(SizeOfScroll);
	Xoffset -= plus;
	Rect.left = DrawWindowMinX;
	Rect.right = DrawWindowMaxX;
	Rect.top = DrawWindowMinY;
	Rect.bottom = DrawWindowMaxY;
	Rect2.left = DrawWindowMinX;
	Rect2.top = DrawWindowMinY;
	Rect2.right = DrawWindowMaxX;
	Rect2.bottom = DrawWindowMaxY;
	UpdateWindow(GEOMWindow);
	ScrollWindow(GEOMWindow, SizeOfScroll, 0, &Rect, &Rect2);
	UpdateWindow(GEOMWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ScrollRight(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	switch (SystemBusyMode)
	{
	case 0:
		DrawCrossHair(2);
		break;

	case 3:
		break;

	default:
		DrawCrossHair(16 + 2);
		break;
	}

	plus = PixelToReal(SizeOfScroll);
	Xoffset += plus;
	Rect.left = DrawWindowMinX;
	Rect.right = DrawWindowMaxX - ScrollSize - 1;
	Rect.right = DrawWindowMaxX;
	Rect.top = DrawWindowMinY;
	Rect.bottom = DrawWindowMaxY;
	Rect2.left = DrawWindowMinX;
	Rect2.top = DrawWindowMinY;
	Rect2.right = DrawWindowMaxX;
	Rect2.bottom = DrawWindowMaxY;
	UpdateWindow(GEOMWindow);
	ScrollWindow(GEOMWindow, -SizeOfScroll, 0, &Rect, &Rect2);
	UpdateWindow(GEOMWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ClipMouseCursor()
{
	RECT Rect;
	POINT P;

	P.x = DrawWindowMinX;
	P.y = DrawWindowMinY;
	ClientToScreen(GEOMWindow, &P);
	Rect.left = P.x;
	Rect.top = P.y;

	P.x = DrawWindowMaxX;
	P.y = DrawWindowMaxY;
	ClientToScreen(GEOMWindow, &P);
	Rect.right = P.x;
	Rect.bottom = P.y;
	ClipCursor(&Rect);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnClipMouseCursor()
{
	ClipCursor(NULL);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawIntro()
{
	int32 x, y, res, IntroBitmapSizeX, IntroBitmapSizeY, IntroBitmapSizeX2, IntroBitmapSizeY2, ScreenSizeX, ScreenSizeY,
	      x2, y2;
	HBITMAP IntroBitmap;
	HBITMAP IntroBitmap2;
	HDC IntroMemoryDC;
	HGDIOBJ old;
	HDC DesktopWindow;

	ScreenSizeX = GetSystemMetrics(SM_CXMAXIMIZED) - 10;
	ScreenSizeY = GetSystemMetrics(SM_CYMAXIMIZED) - 10;

	DesktopWindow = GetDC(NULL);
	IntroBitmapSizeX = 621;
	IntroBitmapSizeY = 422;
	IntroBitmapSizeX2 = 549;
	IntroBitmapSizeY2 = 56;
	IntroBitmap2 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_INTRO35A));
	IntroBitmap = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_INTRO35));
	IntroMemoryDC = CreateCompatibleDC(DesktopWindow);
	old = SelectObject(IntroMemoryDC, IntroBitmap);
	x = (ScreenSizeX) / 2 - IntroBitmapSizeX / 2;
	y = (ScreenSizeY) / 2 - IntroBitmapSizeY / 2;
	x2 = x + (IntroBitmapSizeX - IntroBitmapSizeX2) / 2;
	y2 = y + (IntroBitmapSizeY - IntroBitmapSizeY2) / 2;
	res = BitBlt(DesktopWindow, x, y, IntroBitmapSizeX, IntroBitmapSizeY, IntroMemoryDC, 0, 0, SRCCOPY);
	SelectObject(IntroMemoryDC, old);
	DeleteDC(IntroMemoryDC);
	DeleteObject(IntroBitmap);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LRESULT CALLBACK GEOMWinProc(HWND Window, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	POINT pp;
	double hulp, hulp2;
	HPALETTE hpalT;
	int32 ok, res, divx, divy, MousePosY2, i, v1, v2;
	HDC hdc;
	LRESULT Result;
	double Factor3, dx, dy, cx, cy, ccy, ty, tx;

  //char    str[MAX_LENGTH_STRING];

#ifdef _DEBUG

	if ((Message >= 0x201) && (Message <= 0x209))
		ok = 1;

#endif

	if (Message == ClosingWindowMessage)
	{
		ok = 1;

		if ((WParam == 0) && (LParam == 1))
		{
			if (MasterWindow != NULL)
			{

                //MessageBoxUTF8(NULL,"Send size",SC(48,"Error"),MB_APPLMODAL|MB_OK);

				SendMessage(MasterWindow, WM_COMMAND, (WPARAM) ID_SEND_WINDOW_SIZE1,
				            (LPARAM) MAKELPARAM(RealWindow.left, RealWindow.top));
				SendMessage(MasterWindow, WM_COMMAND, (WPARAM) ID_SEND_WINDOW_SIZE2,
				            (LPARAM) MAKELPARAM(RealWindow.right, RealWindow.bottom));
			}

			PostMessage(GEOMWindow, WM_CLOSE, (WPARAM) NULL, (LPARAM) NULL);
		}
	}

	switch (Message)
	{
	case WM_HELP:

      //Focused=0;

		ok = 1;
		break;

	case WM_CREATE:
		FirstSize = 1;
		WindowCreate();
		break;

	case WM_COMMAND:
		if (HIWORD(LParam) == 0)
			GEOMCommand((int32) WParam, (int32) LParam);

		break;

	case WM_PAINT:
		if (FirstPaint)
		{
			FirstPaint = 0;

            //MessageBoxUTF8(GEOMWindow,"First paint",SC(4,"Message"),MB_APPLMODAL|MB_OK);

			ViewWholeDesign(0);
			FirstPaintMode = 0;
			SetScrollPageSize();
			SetScrollPosition();
			UpdateWindow(hwndHBar);
			UpdateWindow(hwndVBar);
		}

		WindowPaint();
		break;

	case WM_MOUSEWHEEL:
		v1 = LOWORD(WParam);

		if (ZoomMode == 1)
		{
			if (ShiftPressed)
			{
				v2 = (((int16) HIWORD(WParam)) * 5) / 8;

				if (v2 < 0)
				{
					ScrollLeft(-v2);
					ScrollLeft(-v2);
					ScrollLeft(-v2);
					ScrollLeft(-v2);
				}
				else
				{
					ScrollRight(v2);
					ScrollRight(v2);
					ScrollRight(v2);
					ScrollRight(v2);
				}
			}
			else
			{
				v2 = (((int16) HIWORD(WParam)) * 3) / 8;

				if (AltPressed)
				{
					if (v2 > 0)
						ZoomIn(2);
					else
						ZoomOut(2);
				}
				else
				{
					if (v2 < 0)
					{
						ScrollDown(-v2);
						ScrollDown(-v2);
						ScrollDown(-v2);
						ScrollDown(-v2);
					}
					else
					{
						ScrollUp(v2);
						ScrollUp(v2);
						ScrollUp(v2);
						ScrollUp(v2);
					}
				}
			}
		}
		else
		{
			v2 = (((int16) HIWORD(WParam)) * 3) / 8;

			if (v2 > 0)
				ZoomIn(4 + 2);
			else
				ZoomOut(4 + 2);
		}

		res = 1;
		break;

	case WM_VSCROLL:
		if (!AltPressed)
		{
			switch (LOWORD(WParam))
			{
			case SB_LINEUP:
				ScrollUp(30);
				break;

			case SB_LINEDOWN:
				ScrollDown(30);
				break;

			case SB_PAGEUP:
				ScrollUp(ScrollSize);
				break;

			case SB_PAGEDOWN:
				ScrollDown(ScrollSize);
				break;

			case SB_THUMBTRACK:
				hulp2 = (HIWORD(WParam));
				hulp2 = hulp2 * MaxScrollBarY / (MaxScrollBarY - ScrollBarSizeY);
				hulp2 = (MaxScrollBarY - hulp2);
				dy = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
				ccy = (PixelToRealOffY(DrawWindowMaxY) + PixelToRealOffY(DrawWindowMinY)) * 0.5;
				cy = (VisibleMaxY + VisibleMinY) / 2;
				ty = (VisibleMaxY - VisibleMinY) * SCROLL_FACTOR;
				hulp2 = ((hulp2 - MaxScrollBarY / 2) * ty / MaxScrollBarY) - (dy * 0.5) + cy;
				divy = Mult(Yoffset - hulp2);

				if (divy > 0)
					ScrollDown(divy);
				else
				{
					if (divy < 0)
						ScrollUp(-divy);
				}

				break;
			}
		}
		else
		{
			if ((ZoomCounter % 4) == 0)
			{
				switch (LOWORD(WParam))
				{
				case SB_LINEUP:
					if (SystemBusyMode == 0)
						ZoomIn(2);

					break;

				case SB_LINEDOWN:
					if (SystemBusyMode == 0)
						ZoomOut(2);

					break;
				}
			}

			ZoomCounter++;
		}

		break;

	case WM_HSCROLL:
		switch (LOWORD(WParam))
		{
		case SB_LINELEFT:
			ScrollLeft(30);
			break;

		case SB_LINERIGHT:
			ScrollRight(30);
			break;

		case SB_PAGELEFT:
			ScrollLeft(ScrollSize);
			break;

		case SB_PAGERIGHT:
			ScrollRight(ScrollSize);
			break;

		case SB_THUMBTRACK:
			hulp2 = HIWORD(WParam);
			hulp2 = hulp2 * MaxScrollBarX / (MaxScrollBarX - ScrollBarSizeX);
			dx = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
			cx = (VisibleMaxX + VisibleMinX) / 2;
			tx = (VisibleMaxX - VisibleMinX) * SCROLL_FACTOR;
			hulp2 = ((hulp2 - MaxScrollBarX / 2) * tx / MaxScrollBarX) - (dx * 0.5) + cx;
			divx = Mult(Xoffset - hulp2);

			if (divx > 0)
				ScrollLeft(divx);
			else
			{
				if (divx < 0)
					ScrollRight(-divx);
			}

			break;
		}

		break;

	case WM_SIZE:
		if (WParam == SIZE_RESTORED)
			ok = 1;

		if (WParam == SIZE_MINIMIZED)
			WindowMinimized = 1;

		if (WParam == SIZE_MAXIMIZED)
			WindowMaximized = 1;

		memmove(&OldClientRect, &ClientRect, sizeof(RECT));
		memmove(&OldRealWindow, &RealWindow, sizeof(RECT));
		GetClientRect(GEOMWindow, &ClientRect);
		GetWindowRect(GEOMWindow, &RealWindow);

        //sprintf(str,"%4i  %4i  %4i \n",RealWindow.left,OldRealWindow2.left,RealWindow2.left);
        //OutputDebugStr(str);

		DrawWindowMinX = WidthButtonBar;
		DrawWindowMinY = 0;
		DrawWindowMaxX = ClientRect.right - WidthScrollBar;
		DrawWindowMaxY = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
		WindowStartX = RealWindow.left;
		WindowStartY = RealWindow.top;
		WindowWidth = (RealWindow.right - RealWindow.left);
		WindowHeight = (RealWindow.bottom - RealWindow.top);
		ScreenPosAbsCursor = ScreenPosAbsCursorInit;
		ScreenPosAbsGridCursor = ScreenPosAbsGridCursorInit;
		ScreenPosRelGridCursor = ScreenPosRelGridCursorInit;
		ScreenPosInfoStr = ScreenPosInfoStrInit;

		if (WindowWidth > 900)
		{
			ScreenPosAbsGridCursor = ScreenPosAbsGridCursorInit + 30;
			ScreenPosRelGridCursor = ScreenPosRelGridCursorInit + 60;
			ScreenPosInfoStr = ScreenPosInfoStrInit + 120;
		}

		ClientWindowDivX = DrawWindowMaxX - DrawWindowMinX;
		ClientWindowDivY = DrawWindowMaxY - DrawWindowMinY;
		pp.x = 0;
		pp.y = 0;
		ClientToScreen(GEOMWindow, &pp);
		ClientStartX = pp.x;
		ClientStartY = pp.y;

		if (FirstSize == 1)
		{
			FirstSize = 0;
			hulp = (BoardWidth * 1.0 / ClientWindowDivX);
			hulp2 = (BoardHeight * 1.0 / ClientWindowDivY);

			if (hulp > hulp2)
			{
				hulp = (DrawWindowMaxX - DrawWindowMinX);
				Factor = (hulp / BoardWidth);
				DisplX = BoardWidth;
				DisplY = DisplX * ClientWindowDivY / ClientWindowDivX;
			}
			else
			{
				hulp = (DrawWindowMaxY - DrawWindowMinY);
				Factor = (hulp / BoardHeight);
				Factor3 = PixelToReal(Mult(BoardHeight));
				DisplY = BoardHeight;
				DisplX = DisplY * ClientWindowDivX / ClientWindowDivY;
			}
		}
		else
		{
			if ((WParam != SIZE_MAXIMIZED) && (!WindowMinimized) && (!WindowMaximized))
			{
				PaintBecauseOfResize = 1;
				PaintBecauseOfResizeMode = 0;

				if (OldRealWindow2.bottom < RealWindow.bottom)
					PaintBecauseOfResizeMode |= 16;

				if (OldRealWindow2.bottom > RealWindow.bottom)
					PaintBecauseOfResizeMode |= 32;

				if (OldRealWindow2.right < RealWindow.right)
					PaintBecauseOfResizeMode |= 4;

				if (OldRealWindow2.right > RealWindow.right)
					PaintBecauseOfResizeMode |= 8;

				if (OldRealWindow2.left < RealWindow.left)
					PaintBecauseOfResizeMode |= 128;

				if (OldRealWindow2.left > RealWindow.left)
					PaintBecauseOfResizeMode |= 64;

				if (OldRealWindow2.top < RealWindow.top)
					PaintBecauseOfResizeMode |= 2;

				if (OldRealWindow2.top > RealWindow.top)
					PaintBecauseOfResizeMode |= 1;

				if ((PaintBecauseOfResizeMode & (64 + 128)) != 0)
					Xoffset -= PixelToReal(ClientRect.right - OldClientRect.right);

				if ((PaintBecauseOfResizeMode & (16 + 32)) != 0)
					Yoffset -= PixelToReal(ClientRect.bottom - OldClientRect.bottom);
			}
		}

		ScrollSize = (min(DrawWindowMaxX - DrawWindowMinX, DrawWindowMaxY - DrawWindowMinY) / 4) & ~3;;
		ScrollEndOfWindow = 25;

        //ScrollEndOfWindow=ScrollSize/2;

		ScrollSizeDrawing = ScrollSize / 2;
		ViewMinX = PixelToRealOffX(-1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(-1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

		MoveWindow(hwndHBar, DrawWindowMinX, DrawWindowMaxY, DrawWindowMaxX - WidthButtonBar, HeightScrollBar, 0);
		MoveWindow(hwndVBar, DrawWindowMaxX, DrawWindowMinY, WidthScrollBar, DrawWindowMaxY, 0);

		if (hwndHBar != NULL)
		{
			RedrawWindow(hwndHBar, NULL, NULL, RDW_INVALIDATE | RDW_ERASENOW | RDW_ALLCHILDREN);
			RedrawWindow(hwndVBar, NULL, NULL, RDW_INVALIDATE | RDW_ERASENOW | RDW_ALLCHILDREN);
		}

		memmove(&OldRealWindow2, &RealWindow, sizeof(RECT));
		OldWindowWidth = WindowWidth;

		if (WParam != SIZE_MAXIMIZED)
			WindowMaximized = 0;

		if (WParam != SIZE_MINIMIZED)
			WindowMinimized = 0;

		break;

	case WM_MOVE:
		memmove(&OldRealWindow2, &RealWindow2, sizeof(RECT));
		GetWindowRect(GEOMWindow, &RealWindow);
		GetWindowRect(GEOMWindow, &RealWindow2);
		pp.x = 0;
		pp.y = 0;
		ClientToScreen(GEOMWindow, &pp);
		ClientStartX = pp.x;
		ClientStartY = pp.y;
		WindowStartX = RealWindow.left;
		WindowStartY = RealWindow.top;
		break;

	case WM_DRAWITEM:

        //DrawSpecialMenuItem((DRAWITEMSTRUCT *)LParam);

		break;

	case WM_MEASUREITEM:

     //MenuItemSize=(DRAWITEMSTRUCT *)LParam;
     //MenuItemSize->itemWidth=100;
     //MenuItemSize->itemHeight=40;

		ok = 1;
		break;

	case WM_CHAR:
		if ((Focused) && (FocusedAgain))
			KeyChar((uint16) WParam);

		break;

	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
		if ((Focused) && (FocusedAgain))
		{
			KeyDown((uint16) WParam);

			if ((AltPressed) && ((WParam > 0x70) || (WParam == 0x09)))
				return DefWindowProc(Window, Message, WParam, LParam);
		}

		break;

	case WM_KEYUP:
	case WM_SYSKEYUP:
		if ((Focused) && (FocusedAgain))
		{
			KeyUp((uint16) WParam);
			return DefWindowProc(Window, Message, WParam, LParam);
		}

		break;

	case WM_NCHITTEST:
		Result = DefWindowProc(Window, Message, WParam, LParam);

		if (Result != HTCLIENT)
		{
			MousePosX = 10000;
			MousePosY = 10000;
			DrawButtonInfoOff(0);
			ok = 1;
		}

		return Result;

	case WM_MOUSEMOVE:
		if ((Focused) && (FocusedAgain))
		{
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			LeftButtonPressed = 0;
			MiddleButtonPressed = 0;
			RightButtonPressed = 0;

			if (WParam & MK_LBUTTON)
				LeftButtonPressed = 1;

			if (WParam & MK_MBUTTON)
				MiddleButtonPressed = 1;

			if (WParam & MK_RBUTTON)
				RightButtonPressed = 1;

			if ((MousePosOldX != MousePosX) || (MousePosOldY != MousePosY))
			{
				MouseChanged = 1;
				MousePosOldX = MousePosX;
				MousePosOldY = MousePosY;
				ThumbChanged = 0;
			}
		}

		if (GetCursor() != SystemCursor)
			SetCursor(SystemCursor);

		break;

	case WM_LBUTTONDBLCLK:
		if ((Focused) && (FocusedAgain))
		{
			ok = 1;
			LeftButtonDoublePressed = 1;
		}

		break;

	case WM_LBUTTONDOWN:
		MousePosY2 = HIWORD(LParam) + 1;

		if ((Focused) && (FocusedAgain) && (MousePosY2 < DrawWindowMaxY))
		{
			MouseChanged = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			LeftButtonPressed = 1;
			return DefWindowProc(Window, Message, WParam, LParam);
		}

		break;

	case WM_LBUTTONUP:
		if (Focused)
		{
			MouseChanged = 1;
			FocusedAgain = 1;
			LeftButtonPressed = 0;
			LeftButtonDoublePressed = 0;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;

            //LeftPressedOk=1;

			return DefWindowProc(Window, Message, WParam, LParam);
		}

		break;

	case WM_MBUTTONDOWN:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;

			if (GetSystemMetrics(SM_CMOUSEBUTTONS) == 3)
				MiddleButtonPressed = 1;
		}

		break;

	case WM_MBUTTONUP:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			MiddleButtonPressed = 0;
		}

		break;

	case WM_RBUTTONDBLCLK:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			RightButtonPressed = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
		}

		break;

	case WM_RBUTTONDOWN:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			RightButtonPressed = 1;
		}

		break;

	case WM_RBUTTONUP:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			RightButtonPressed = 0;
		}

		break;

	case WM_PALETTECHANGED:
		if ((HWND) WParam == GEOMWindow)
			return 0;

	 // Otherwise, fall through to WM_QUERYNEWPALETTE

	case WM_QUERYNEWPALETTE:

	    // If realizing the palette causes the palette to change, redraw completely
		hdc = GetDC(GEOMWindow);
		hpalT = SelectPalette(hdc, GEOMPalette, 0);
		i = RealizePalette(hdc);	// i == entries that changed
		SelectPalette(hdc, hpalT, 0);
		ReleaseDC(GEOMWindow, hdc);

		// If any palette entries changed, repaint the window
		if (i > 0)
			InvalidateRect(GEOMWindow, NULL, 1);

		return i;

	case WM_SETFOCUS:
		Focused = 1;
		SetCursor(LoadCursor(0, IDC_ARROW));
		WaitForPaint = 0;

      //GEOMCursorActive=1;
      //OldCursor=SetCursor(Cursor1);
      //ShowCursor(1);
      //WindowSetFocus();

		break;

	case WM_KILLFOCUS:

		/*
		        WindowKillFocus();
		        LeftPressedOk=0;
		*/

        //GEOMCursorActive=0;
        //SetCursor(OldCursor);

		Focused = 0;
		DrawCrossHair(32 + 2);
		DrawButtonInfoOff(0);

        //SelectionEsc=1;

		AltPressed = 0;
		CtrlPressed = 0;
		ShiftPressed = 0;
		RightButtonPressed = 0;
		LeftButtonPressed = 0;

        //FocusedAgain=0;

		break;

	case WM_TIMER:
		TimerValue++;
		return DefWindowProc(Window, Message, WParam, LParam);
		break;

	case WM_CLOSE:
		if (SystemBusyMode == 0)
		{
			GetWindowRect(GEOMWindow, &RealWindow);

			if (SaveFile2(0) == 1)
			{
				WriteIniFile();
				DeAllocateMem();

				if (ProjectIndexNr != -1)
				{
					ProjectInfo->FileNames[ProjectIndexNr][0] = 0;
					ProjectInfo->FileInfos[ProjectIndexNr] = 0;
					ProjectInfo->FileTypes[ProjectIndexNr] = 0;
					ProjectInfo->WindowHandles[ProjectIndexNr] = 0;
				}

				if (StartFromLibraryManager)
					PostMessage(MasterWindow, WM_COMMAND, (WPARAM) ID_FILE_EXIT_TO_LIB_GEOM, (LPARAM) LibrarySymbolNr);

				if (WindowInitialSized)
				{
					if (MasterWindow != NULL)
					{

                        //sprintf(str,"Send size to %x",MasterWindow);
                        //MessageBoxUTF8(NULL,str,SC(48,"Error"),MB_APPLMODAL|MB_OK);

						SendMessage(MasterWindow, WM_USER, (WPARAM) ID_SEND_WINDOW_SIZE1,
						            (LPARAM) MAKELPARAM(RealWindow.left, RealWindow.top));
						SendMessage(MasterWindow, WM_USER, (WPARAM) ID_SEND_WINDOW_SIZE2,
						            (LPARAM) MAKELPARAM(RealWindow.right, RealWindow.bottom));
					}
				}

				DestroyWindow(GEOMWindow);
			}
		}

		break;

	case WM_GETMINMAXINFO:
		return DefWindowProc(Window, Message, WParam, LParam);

	case WM_QUIT:
		return 0;

	case WM_DESTROY:
		WindowDestroy();

     //Created=0;

		return 0;

     //case WM_DROPFILES:DragFiles(WParam);
     //break;

	default:
		return DefWindowProc(Window, Message, WParam, LParam);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeValue(int32 * Object, LPSTR str)
{
	int32 Value;

	if ((sscanf(str, "%i", &Value) == 1) && ((Value == 0) || (Value == 1)))
		*Object = Value;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void LoadIniFile(LPSTR FileName, int32 mode)
{

  /*
	mode:

	bit 0 :
	bit 1 : Load [Settings]
	bit 2 : Load [Keys]

 */

	int32 fp, cnt, Length, ParamMode, Value, ok, Key, OldWindowStartX, OldWindowStartY, OldWindowWidth, OldWindowHeight;

	char LineBuf[512], str2[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     str5[MAX_LENGTH_STRING];
	float Value1;
	int32 ChangeGrid = 0;
	int32 ChangeTraces = 0;
	int32 ChangeClearances = 0;

	if (FileName[0] == 0)
		return;

    //MessageBox(NULL,IniFile,"Load ini file from",MB_APPLMODAL+MB_OK);

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return;

	OldWindowStartX = WindowStartX;
	OldWindowStartY = WindowStartY;
	OldWindowWidth = WindowWidth;
	OldWindowHeight = WindowHeight;

	ParamMode = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetSpecialString(LineBuf, str1, 0);
			GetSpecialString(LineBuf, str2, 0);

			if (str1[0] == '[')
			{
				ParamMode = 0;

				if (stricmp(str1, "[Settings]") == 0)
				{
					if (mode & 1)
						ParamMode = 3;
				}

				if (stricmp(str1, "[Keys]") == 0)
				{
					if (mode & 2)
						ParamMode = 4;
				}
			}
			else
			{
				switch (ParamMode)
				{
				case 3:
					if (GetStringValue(str4, str1, str2))
					{
						if (stricmp(str1, "WindowWidth") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowWidth = Value;
						}

						if (stricmp(str1, "WindowHeight") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowHeight = Value;
						}

						if (stricmp(str1, "WindowStartX") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowStartX = Value;
						}

						if (stricmp(str1, "WindowStartY") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowStartY = Value;
						}

						if (stricmp(str1, "Units") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								if (Value == 0)
									Units = 0;

								if (Value == 1)
									Units = 1;
							}
						}

						if (stricmp(str1, "GridSize") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								GridSize = Value1;
						}

						for (cnt = 0; cnt < 32; cnt++)
						{
							sprintf(str5, "PadsVisible%i", cnt);

							if (stricmp(str1, str5) == 0)
								ChangeValue(&PadsVisible[cnt], str2);
						}

						if (stricmp(str1, "SoldMaskPadsBottomVisible") == 0)
							ChangeValue(&SoldMaskPadsBottomVisible, str2);

						if (stricmp(str1, "SoldMaskPadsTopVisible") == 0)
							ChangeValue(&SoldMaskPadsTopVisible, str2);

						if (stricmp(str1, "PastePadsTopVisible") == 0)
							ChangeValue(&PastePadsTopVisible, str2);

						if (stricmp(str1, "PastePadsBottomVisible") == 0)
							ChangeValue(&PastePadsBottomVisible, str2);

						if (stricmp(str1, "PlacementVisible") == 0)
							ChangeValue(&PlacementVisible, str2);

						if (stricmp(str1, "CompOutlineVisible") == 0)
							ChangeValue(&CompOutlineVisible, str2);

						if (stricmp(str1, "SilkScreenTopVisible") == 0)
							ChangeValue(&SilkScreenTopVisible, str2);

						if (stricmp(str1, "SilkScreenBottomVisible") == 0)
							ChangeValue(&SilkScreenBottomVisible, str2);

						for (cnt = 0; cnt < 32; cnt++)
						{
							sprintf(str5, "RoutingKeepoutVisible%i", cnt);

							if (stricmp(str1, str5) == 0)
								ChangeValue(&RoutingKeepoutVisible[cnt], str2);
						}

						if (stricmp(str1, "GeomNameVisible") == 0)
							ChangeValue(&GeomNameVisible, str2);

						if (stricmp(str1, "PowerPadsVisible") == 0)
							ChangeValue(&PowerPadsVisible, str2);

						if (stricmp(str1, "PinNamesVisible") == 0)
							ChangeValue(&PinNamesVisible, str2);

						if (stricmp(str1, "InnerPadsVisible") == 0)
							ChangeValue(&InnerPadsVisible, str2);

						if (stricmp(str1, "DrillVisible") == 0)
							ChangeValue(&DrillVisible, str2);

						if (stricmp(str1, "DrillUnplatedVisible") == 0)
							ChangeValue(&DrillUnplatedVisible, str2);

						if (stricmp(str1, "SnapMode") == 0)
							ChangeValue(&SnapMode, str2);

						if (stricmp(str1, "ZoomMode") == 0)
							ChangeValue(&ZoomMode, str2);

						if (stricmp(str1, "DrawGrid") == 0)
							ChangeValue(&GridVisible, str2);

						if (stricmp(str1, "MousePanMultiply") == 0)
							ChangeValue(&MousePanMultiply, str2);

						if (stricmp(str1, "DrawCrossHair") == 0)
							ChangeValue(&CrossHairVisible, str2);

						if (stricmp(str1, "ClearanceVisible") == 0)
							ChangeValue(&ClearanceVisible, str2);

						if (stricmp(str1, "ViewInsertionPoint") == 0)
							ChangeValue(&ViewInsertionPoint, str2);

						if (stricmp(str1, "DefaultRuleSolderMask_TH") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								DefaultRuleSolderMask_TH = Value1;
						}

						if (stricmp(str1, "DefaultRulePasteMask_SMD") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								DefaultRulePasteMask_SMD = Value1;
						}

						if (stricmp(str1, "DefaultRulePad") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								DefaultRulePad = Value1;
						}

						if (stricmp(str1, "DefaultRuleSolderMask_SMD") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								DefaultRuleSolderMask_SMD = Value1;
						}

						if (stricmp(str1, "DefaultRuleAntiPowerPad") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								DefaultRuleAntiPowerPad = Value1;
						}

						if (stricmp(str1, "DefaultRuleInnerPad") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								DefaultRuleInnerPad = Value1;
						}

						if (stricmp(str1, "DefaultRuleClearance") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								DefaultRuleClearance = Value1;
						}

						if (stricmp(str1, "TraceThickness") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								TraceThickness = Value1;
						}

						if (stricmp(str1, "CurrentClearance") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								CurrentClearance = Value1;
						}

						if (stricmp(str1, "CurrentSilkscreenLine") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								CurrentSilkscreenLine = Value1;
						}

						if (stricmp(str1, "CurrentCompOutLine") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								CurrentCompOutLine = Value1;
						}

						if (stricmp(str1, "CurrentInfoLine") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								CurrentInfoLine = Value1;
						}

						if (stricmp(str1, "CurrentBoardOutLine") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								CurrentBoardOutLine = Value1;
						}

						if (stricmp(str1, "CurrentTextHeight") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
								CurrentTextHeight = Value1;
						}

						for (cnt = 0; cnt < 32; cnt++)
						{
							sprintf(str5, "Grid%i", cnt);

							if (stricmp(str1, str5) == 0)
							{
								if (sscanf(str2, "%f", &Value1) == 1)
								{
									if (!ChangeGrid)
									{
										ChangeGrid = 1;
										NrGridSizes = 0;
									}

									if (NrGridSizes < 32)
										GridSizes[NrGridSizes++] = Value1;
								}
							}
						}

						for (cnt = 0; cnt < 32; cnt++)
						{
							sprintf(str5, "TraceClearances%i", cnt);

							if (stricmp(str1, str5) == 0)
							{
								if (sscanf(str2, "%f", &Value1) == 1)
								{
									if (!ChangeClearances)
									{
										ChangeClearances = 1;
										NrTraceClearances = 0;
									}

									if (NrTraceClearances < 32)
										TraceClearances[NrTraceClearances++] = Value1;
								}
							}
						}

						for (cnt = 0; cnt < 32; cnt++)
						{
							sprintf(str5, "TraceWidth%i", cnt);

							if (stricmp(str1, str5) == 0)
							{
								if (sscanf(str2, "%f", &Value1) == 1)
								{
									if (!ChangeTraces)
									{
										ChangeTraces = 1;
										NrTraceWidths = 0;
									}

									if (NrTraceWidths < 32)
										TraceWidths[NrTraceWidths++] = Value1;
								}
							}
						}

						if (stricmp(str1, "SilkScreenTopColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeSilkScreenTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SilkScreenBottomColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeSilkScreenBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "CompOutlineColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeCompOutlineColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePlacementOutLineColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePlacementOutLineColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsDrillColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsDrillColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsDrillUnplatedColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsDrillUnplatedColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsCompSideColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsTopColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsInnerColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsInnerColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsSoldSideColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsBottomColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePinsInnerColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePinsInnerColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePasteMaskCompSideColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePasteMaskTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePasteMaskTopColorNr") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePasteMaskTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePasteMaskSoldSideColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePasteMaskBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapePasteMaskBottomColorNr") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePasteMaskBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapeSoldMaskCompSideColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeSoldMaskTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapeSoldMaskTopColorNr") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeSoldMaskTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapeSoldMaskSoldSideColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeSoldMaskBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ShapeSoldMaskBottomColorNr") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeSoldMaskBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "GeomNameColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeGeomNameColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "PowerPadColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapePowerPadColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ClearanceColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ClearanceColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ButtonInfoColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ButtonInfoColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "GridColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[GridColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "BoardOutlineColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeBoardOutlineColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "Info1Color") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeInfo1ColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "Info2Color") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeInfo2ColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "Info3Color") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeInfo3ColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "Info4Color") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeInfo4ColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "BackGroundColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[BackGroundColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "RoutingKeepoutTopColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeRoutingKeepoutTopColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "RoutingKeepoutBottomColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeRoutingKeepoutBottomColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "RoutingKeepoutInnerColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GEOMColors[ShapeRoutingKeepoutInnerColorNr] = (COLORREF) Value;
						}

					                /*
						              if (stricmp(str1,"WireBusSelectMode")==0) {
						                if (sscanf(str2,"%i",&Value)==1) {
						                  WireSelectMode=Value;
						                }
						              }
					               */

					}

					break;

				case 4:
					if (GetStringValue(str4, str1, str2))
					{
						if (str2[0] != 0)
						{
							Key = GetKeyValue(str2);

							if (Key != 0)
								SetKeyOnKeyFunctionString(Key, str1);
						}
					}

					break;
				}
			}
		}
	}

	TextFileClose(fp);

	if (!FirstPaintMode)
	{
		ok = 1;
		AdjustWindowSize();
		MoveWindow(GEOMWindow, WindowStartX, WindowStartY, WindowWidth, WindowHeight, 1);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WriteIniFile()
{
	int32 fp, cnt, res, Stop;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	if (WindowInitialSized)
		return;

    //if (MasterWindow!=NULL) return;

	if (LibraryFile[0] != 0)
		return;

	if (FoundDesignPath)
		sprintf(IniFile, "%s\\geom.ini", DesignPath);
	else
	{
		if (FoundProjectPath)
			sprintf(IniFile, "%s\\geom.ini", ProjectPath);
	}

	if (IniFile[0] == 0)
		return;

	if ((fp = FileOpenWriteUTF8(IniFile)) < 0)
		return;

 // ******************************************************************************
	WriteLn(fp, "[Settings]");
	WriteLn(fp, "");

	sprintf(str1, "WindowWidth=%i", WindowWidth);
	WriteLn(fp, str1);
	sprintf(str1, "WindowHeight=%i", WindowHeight);
	WriteLn(fp, str1);
	sprintf(str1, "WindowStartX=%i", WindowStartX);
	WriteLn(fp, str1);
	sprintf(str1, "WindowStartY=%i", WindowStartY);
	WriteLn(fp, str1);
	sprintf(str1, "Units=%i", Units);
	WriteLn(fp, str1);
	sprintf(str1, "GridSize=%f", GridSize);
	WriteLn(fp, str1);

 // ******************************************************************************
	/*
	      if (!PadsSoldVisible) return;
	      if (!PadsCompVisible) return;
	      if (!SoldMaskPadsSoldVisible) return;
	      if (!SoldMaskPadsCompVisible) return;
	      if (!PastePadsCompVisible) return;
	      if (!PastePadsSoldVisible) return;
	      if (!RoutingKeepoutVisible) return;
	      if (!SilkScreenVisible) return;
	      if (!PlacementVisible) return;
	      if (!CompOutlineVisible) return;
	      if (!GeomNameVisible) return ;
	      if (!PowerPadsVisible) return;
	      if (!InnerPadsVisible) return;
	      if (!DrillVisible) return;
	      if (!DrillUnplatedVisible) return;
	      break;
	*/

	sprintf(str1, "SnapMode=%i", SnapMode);
	WriteLn(fp, str1);
	sprintf(str1, "ZoomMode=%i", ZoomMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawGrid=%i", GridVisible);
	WriteLn(fp, str1);
	sprintf(str1, "DrawCrossHair=%i", CrossHairVisible);
	WriteLn(fp, str1);

	for (cnt = 0; cnt < 32; cnt++)
	{
		sprintf(str1, "PadsVisible%i=%d", cnt, PadsVisible[cnt]);
		WriteLn(fp, str1);
	}

	sprintf(str1, "MousePanMultiply=%i", MousePanMultiply);
	WriteLn(fp, str1);
	sprintf(str1, "SoldMaskPadsBottomVisible=%i", SoldMaskPadsBottomVisible);
	WriteLn(fp, str1);
	sprintf(str1, "SoldMaskPadsTopVisible=%i", SoldMaskPadsTopVisible);
	WriteLn(fp, str1);
	sprintf(str1, "PastePadsTopVisible=%i", PastePadsTopVisible);
	WriteLn(fp, str1);
	sprintf(str1, "PastePadsBottomVisible=%i", PastePadsBottomVisible);
	WriteLn(fp, str1);
	sprintf(str1, "SilkScreenTopVisible=%i", SilkScreenTopVisible);
	WriteLn(fp, str1);
	sprintf(str1, "SilkScreenBottomVisible=%i", SilkScreenBottomVisible);
	WriteLn(fp, str1);
	sprintf(str1, "PlacementVisible=%i", PlacementVisible);
	WriteLn(fp, str1);
	sprintf(str1, "CompOutlineVisible=%i", CompOutlineVisible);
	WriteLn(fp, str1);
	sprintf(str1, "GeomNameVisible=%i", GeomNameVisible);
	WriteLn(fp, str1);
	sprintf(str1, "PowerPadsVisible=%i", PowerPadsVisible);
	WriteLn(fp, str1);
	sprintf(str1, "PinNamesVisible=%i", PinNamesVisible);
	WriteLn(fp, str1);
	sprintf(str1, "InnerPadsVisible=%i", InnerPadsVisible);
	WriteLn(fp, str1);
	sprintf(str1, "DrillVisible=%i", DrillVisible);
	WriteLn(fp, str1);
	sprintf(str1, "DrillUnplatedVisible=%i", DrillUnplatedVisible);
	WriteLn(fp, str1);

	for (cnt = 0; cnt < 32; cnt++)
	{
		sprintf(str1, "RoutingKeepoutVisible%i=%d", cnt, RoutingKeepoutVisible[cnt]);
		WriteLn(fp, str1);
	}

	sprintf(str1, "BoardOutlineVisible=%i", BoardOutlineVisible);
	WriteLn(fp, str1);
	sprintf(str1, "Info1Visible=%i", Info1Visible);
	WriteLn(fp, str1);
	sprintf(str1, "Info2Visible=%i", Info2Visible);
	WriteLn(fp, str1);
	sprintf(str1, "Info3Visible=%i", Info3Visible);
	WriteLn(fp, str1);
	sprintf(str1, "Info4Visible=%i", Info4Visible);
	WriteLn(fp, str1);

	sprintf(str1, "ClearanceVisible=%i", ClearanceVisible);
	WriteLn(fp, str1);
	sprintf(str1, "ViewInsertionPoint=%i", ViewInsertionPoint);
	WriteLn(fp, str1);
	sprintf(str1, "DefaultRuleSolderMask_TH=%.1f", DefaultRuleSolderMask_TH);
	WriteLn(fp, str1);
	sprintf(str1, "DefaultRulePasteMask_SMD=%.1f", DefaultRulePasteMask_SMD);
	WriteLn(fp, str1);
	sprintf(str1, "DefaultRulePad=%.1f", DefaultRulePad);
	WriteLn(fp, str1);
	sprintf(str1, "DefaultRuleSolderMask_SMD=%.1f", DefaultRuleSolderMask_SMD);
	WriteLn(fp, str1);
	sprintf(str1, "DefaultRuleAntiPowerPad=%.1f", DefaultRuleAntiPowerPad);
	WriteLn(fp, str1);
	sprintf(str1, "DefaultRuleInnerPad=%.1f", DefaultRuleInnerPad);
	WriteLn(fp, str1);
	sprintf(str1, "DefaultRuleClearance=%.1f", DefaultRuleClearance);
	WriteLn(fp, str1);

	sprintf(str1, "TraceThickness=%.1f", TraceThickness);
	WriteLn(fp, str1);
	sprintf(str1, "CurrentClearance=%.1f", CurrentClearance);
	WriteLn(fp, str1);
	sprintf(str1, "CurrentSilkscreenLine=%.1f", CurrentSilkscreenLine);
	WriteLn(fp, str1);
	sprintf(str1, "CurrentCompOutLine=%.1f", CurrentCompOutLine);
	WriteLn(fp, str1);
	sprintf(str1, "CurrentInfoLine=%.1f", CurrentInfoLine);
	WriteLn(fp, str1);
	sprintf(str1, "CurrentBoardOutLine=%.1f", CurrentBoardOutLine);
	WriteLn(fp, str1);
	sprintf(str1, "CurrentTextHeight=%.1f", CurrentTextHeight);
	WriteLn(fp, str1);

	WriteLn(fp, "");
	sprintf(str1, "SilkScreenTopColor=%i", (int32) GEOMColors[ShapeSilkScreenTopColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SilkScreenBottomColor=%i", (int32) GEOMColors[ShapeSilkScreenBottomColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "CompOutlineColor=%i", (int32) GEOMColors[ShapeCompOutlineColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePlacementOutLineColor=%i", (int32) GEOMColors[ShapePlacementOutLineColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePinsDrillColor=%i", (int32) GEOMColors[ShapePinsDrillColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePinsDrillUnplatedColor=%i", (int32) GEOMColors[ShapePinsDrillUnplatedColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePinsTopColor=%i", (int32) GEOMColors[ShapePinsTopColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePinsInnerColor=%i", (int32) GEOMColors[ShapePinsInnerColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePinsBottomColor=%i", (int32) GEOMColors[ShapePinsBottomColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePasteMaskTopColor=%i", (int32) GEOMColors[ShapePasteMaskTopColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapePasteMaskBottomColor=%i", (int32) GEOMColors[ShapePasteMaskBottomColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapeSoldMaskTopColor=%i", (int32) GEOMColors[ShapeSoldMaskTopColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ShapeSoldMaskBottomColor=%i", (int32) GEOMColors[ShapeSoldMaskBottomColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "GeomNameColor=%i", (int32) GEOMColors[ShapeGeomNameColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "PowerPadColor=%i", (int32) GEOMColors[ShapePowerPadColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ClearanceColor=%i", (int32) GEOMColors[ClearanceColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "RoutingKeepoutTopColor=%i", (int32) GEOMColors[ShapeRoutingKeepoutTopColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "RoutingKeepoutBottomColor=%i", (int32) GEOMColors[ShapeRoutingKeepoutBottomColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "RoutingKeepoutInnerColor=%i", (int32) GEOMColors[ShapeRoutingKeepoutInnerColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "BoardOutlineColor=%i", (int32) GEOMColors[ShapeBoardOutlineColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "Info1Color=%i", (int32) GEOMColors[ShapeInfo1ColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "Info2Color=%i", (int32) GEOMColors[ShapeInfo2ColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "Info3Color=%i", (int32) GEOMColors[ShapeInfo3ColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "Info4Color=%i", (int32) GEOMColors[ShapeInfo4ColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ButtonInfoColor=%i", (int32) GEOMColors[ButtonInfoColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "GridColor=%i", (int32) GEOMColors[GridColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "BackGroundColor=%i", (int32) GEOMColors[BackGroundColorNr]);
	WriteLn(fp, str1);

    //sprintf(str1,"DrawCompReference=%i",OkToDrawCompReference);
    //WriteLn(fp,str1);
    //sprintf(str1,"DrawCompValue=%i",OkToDrawCompValue);
    //WriteLn(fp,str1);

    // ******************************************************************************
    //WriteLn(fp,"");
    //for (cnt=0;cnt<32;cnt++) {
    //sprintf(str1,"Layer%i=%i",cnt,(DrawLayerCode[cnt] >> 4) ^ 1);
    //WriteLn(fp,str1);
    //}

	WriteLn(fp, "");

	for (cnt = 0; cnt < NrGridSizes; cnt++)
	{
		sprintf(str1, "Grid%i=%.1f", cnt, GridSizes[cnt]);
		WriteLn(fp, str1);
	}

	WriteLn(fp, "");

	for (cnt = 0; cnt < NrTraceWidths; cnt++)
	{
		sprintf(str1, "TraceWidth%i=%.1f", cnt, TraceWidths[cnt]);
		WriteLn(fp, str1);
	}

	WriteLn(fp, "");

	for (cnt = 0; cnt < NrTraceClearances; cnt++)
	{
		sprintf(str1, "ClearanceWidth%i=%.1f", cnt, TraceClearances[cnt]);
		WriteLn(fp, str1);
	}

	WriteLn(fp, "");
	WriteLn(fp, "[Keys]");
	WriteLn(fp, "");
	Stop = 0;
	cnt = 0;

	while (!Stop)
	{
		if ((GetKeyString(str2, cnt, 3) == 1) && ((res = GetKeyString(str3, cnt, 5)) != -1))
		{
			if (res == 0)
				str3[0] = 0;

			if (str2[0] != 0)
			{
				strcat(str2, "                                              ");
				str2[36] = 0;
				sprintf(str1, "%s = %s", str2, str3);
				WriteLn(fp, str1);
			}

			cnt++;
		}
		else
			Stop = 1;
	}

	WriteLn(fp, "");
	FileClose(fp);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddGeomLanguageString(int32 ID, LPSTR Text)
{
	int32 Length;
	WCHAR TextW[MAX_LENGTH_STRING];

	Length = strlen(Text);

	if (Length == 0)
		return -1;

	if ((ID < 0) || (ID >= MaxNrGeomNames))
		return -1;

	if (GeomNamesPos + Length + 2 >= GeomNamesBufSize)
		return -1;

	if (!Utf8ToUnicode(Text, TextW, MAX_LENGTH_STRING))
		return -1;

	GeomNamesId[ID] = (LPSTR) & GeomNamesBuf[GeomNamesPos];
	memmove(&GeomNamesBuf[GeomNamesPos], Text, Length + 1);
	GeomNamesPos += Length + 1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddGeomLanguageStrings(LPSTR FileName)
{
	int32 fp, cnt2, str3length, Length, count, StringNr;
	char LineBuf[512], str2[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], LanguageFileName[MAX_LENGTH_STRING],
	     str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING];


	count = 0;

	if (FileName[0] == 0)
		return -1;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
	{

		/*
		    sprintf(str,"Can not read from language file %s, default language used",FileName);
		    MessageBoxUTF8(NULL,str,SC(49,"Warning"),MB_OK);
		*/

		return -1;
	}

	LanguageFileName[0] = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			if (LanguageFileName[0] == 0)
			{
				GetString(LineBuf, str);

				if (LanguagePath[0] != 0) 
					sprintf(LanguageFileName, "%s\\%s", LanguagePath, str);
				else
				 //   sprintf(LanguageFileName, "%s\\%s", ExecutableDir, str); 
					strcpy(LanguageFileName, "");
			}
		}
	}

	TextFileClose(fp);

 // ********************************************************************************************************

	if ((LanguageFileName[0] == 0) || ((fp = TextFileOpenUTF8(LanguageFileName)) < 0))
	{

		/*
		    sprintf(str,"Can not read from language file %s",LanguageFileName);
		    MessageBoxUTF8(NULL,str,"System error",MB_OK);
		    return -2;
		*/

		return -1;
	}

	count = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{

			GetString(LineBuf, str6);
			GetQuoteString2(LineBuf, str2);

			if ((str6[0] != 0) && (sscanf(str6, "%d", &StringNr) == 1))
			{
				DecodeQuotedString(str2, str3);
				str3length = (int32) strlen(str3);

				for (cnt2 = 0; cnt2 < str3length; cnt2++)
				{
					if (((uint8) str3[cnt2] < (uint8) ' ') && (str3[cnt2] != '\r') && (str3[cnt2] != '\t')
					        && (str3[cnt2] != '\n'))
						str3[cnt2] = ' ';
					else
					{
					}
				}

				AddGeomLanguageString(StringNr, str3);
			}
		}
	}

	TextFileClose(fp);

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DecodeParameters(int32 mode)
{
	int32 start, cnt, cnt2, lengte, pos;

	pos = 0;
	lengte = 1000;
	start = 0;

	if (mode == 0)
		start = 1;

	for (cnt = 0; cnt < NrParams; cnt++)
	{
		if (Parameters[cnt].Option[0] != 0)
		{
			if (strnicmp(Parameters[cnt].Option, "a", lengte) == 0)
				PaintIntro = 0;

			if (strnicmp(Parameters[cnt].Option, "d", lengte) == 0)
			{
				strcpy(DesignPath, (LPSTR) & Parameters[cnt].Parameter[pos]);
				FoundDesignPath = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "e", lengte) == 0)
				strcpy(ExePath, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "i", lengte) == 0)
				strcpy(IniFile, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "l", lengte) == 0)
				strcpy(LibraryFile, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "o", lengte) == 0)
				ProjectActive = 1;

			if (strnicmp(Parameters[cnt].Option, "s", lengte) == 0)
			{
				if ((cnt2 = ScanParameters(-1, (LPSTR) & Parameters[cnt].Parameter[pos], 1)) == 4)
				{
					WindowWidth2 = ParamsInt[0];
					WindowHeight2 = ParamsInt[1];
					WindowStartX2 = ParamsInt[2];
					WindowStartY2 = ParamsInt[3];
					WindowInitialSized = 1;
				}
			}

			if (strnicmp(Parameters[cnt].Option, "u", lengte) == 0)
			{
				strcpy(ProjectPath, (LPSTR) & Parameters[cnt].Parameter[pos]);
				FoundProjectPath = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "w", lengte) == 0)
				sscanf((LPSTR) & Parameters[cnt].Parameter[pos], "%x", (int32 *) & MasterWindow);

			if (strnicmp(Parameters[cnt].Option, "z", lengte) == 0)
			{
				StartFromLibraryManager = 1;
				sscanf((LPSTR) & Parameters[cnt].Parameter[pos], "%d", &LibrarySymbolNr);
			}
		}
		else
		{
			if ((mode == 0) && (cnt > 0) && (Parameters[cnt].Parameter[0] != 0))
			{
				if (ConvertFileNameFromCommandLineUTF8(Parameters[cnt].Parameter, EditFile) == -1)
				{
					MessageBoxUTF8(GEOMWindow, Parameters[cnt].Parameter, SC(253, "Error in opening file"),
					               MB_APPLMODAL | MB_OK);
				}
			}
		}
	}
}

void LoadUserIniFile()
{
	int32 fp, Length, ParamMode, Value, res;
	char LineBuf[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], CurrentDir[MAX_LENGTH_STRING],
		str4[MAX_LENGTH_STRING];

	if ((fp = TextFileOpenUTF8(UserIniFile)) < 0)
		return;

	ParamMode = 0;

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
	{
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetSpecialString(LineBuf, str1, 0);
			GetSpecialString(LineBuf, str2, 0);

			if (str1[0] == '[')
			{
				ParamMode = 0;

				//        if (stricmp(str1,"[ExeDirectory]")==0) ParamMode=1;
				//        if (stricmp(str1,"[ProjectPath]")==0) ParamMode=2;
				//        if (stricmp(str1,"[SymbolDirs]")==0) ParamMode=3;
				//        if (stricmp(str1,"[GeometryLibraryPath]")==0) ParamMode=4;
				//      if (stricmp(str1,"[SchematicSymbolLibraryPath]")==0) ParamMode=5;
				if (stricmp(str1, "[LastDesigns]") == 0)
					ParamMode = 1;

				if (stricmp(str1, "[Settings]") == 0)
					ParamMode = 2;
			}
			else
			{
				switch (ParamMode)
				{
				case 1:
					break;

				case 2:
					if (GetStringValue(str4, str1, str2))
					{
//						if (stricmp(str1, "UseLanguage") == 0)
//						{
//							if (sscanf(str2, "%i", &Value) == 1)
//								UseLanguage = Value;
//						}

						if (stricmp(str1, "LanguagePath") == 0)
						{
							if (FileExistsUTF8(str2) != -1)
							{ // Gerbv found
								strcpy(LanguagePath, str2);
							}
							else
								strcpy(LanguagePath, "");
						}
					}

					break;

				case 3:
					break;

				case 4:
					break;

				case 5:
					break;

				case 6:
					break;
				}
			}
		}
	}

	TextFileClose(fp);
//	SetCurrentDirectoryUTF8(CurrentDir);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmd, int nCmdShow)
{
	MSG M;
	int32 ok, count, res, KeySize;
	uint32 EAX, EBX, ECX, EDX, FlagSSE2 = 0;
	char vendor[40], *env;
	char str[MAX_LENGTH_STRING];
	HKEY Key;

	GetCPUID(0x0, &EAX, &EBX, &ECX, &EDX);
	memset(vendor, 0, sizeof(vendor));
	memcpy(vendor, &EBX, 4);
	memcpy(vendor + 4, &EDX, 4);
	memcpy(vendor + 8, &ECX, 4);
	GetCPUID(0x80000000, &EAX, &EBX, &ECX, &EDX);
	GetCPUID(0x1, &EAX, &EBX, &ECX, &EDX);
	FlagSSE2 = (EDX >> 26) & 0x1;

	if (!FlagSSE2)
	{
		MessageBox(NULL, "This version of the layout editor requires a CPU with SSE2 extensions", SC(48, "Error"),
		           MB_APPLMODAL | MB_OK);
		return 0;
	}

	WindowWidth2 = -1;
	WindowHeight2 = -1;
	WindowStartX2 = -1;
	WindowStartY2 = -1;
	InitTimers();

	GetParameters(NULL);
	GetDirFromFileName(ExecutableDir, Parameters[0].Parameter);
	DecodeParameters(0);		// EditFile

	if (ExePath[0] == 0)
		strcpy(ExePath, ExecutableDir);

	strcpy(GeomExecutable, Parameters[0].Parameter);

	GetCurrentDirectoryUTF8(MAX_LENGTH_STRING - 50, StartDir);

	if (ProjectPath[0] != 0)
	{
		if (DesignPath[0] == 0)
			strcpy(DesignPath, ProjectPath);
	}
	else
		strcpy(ProjectPath, ExePath);
	
	if (UserIniFilePath[0] == 0)
	{
		env = getenv(PCB_ELEG_ENVIRONMENT_STRING);

		if (env != NULL)
			strcpy(UserIniFilePath, env);
	}

	if (UserIniFilePath[0] == 0)
	{
		sprintf(str, "Software\\PCB Elegance");

		if ((res = RegOpenKeyEx(HKEY_LOCAL_MACHINE, str, 0, KEY_QUERY_VALUE, &Key)) == ERROR_SUCCESS)
		{
			KeySize = sizeof(UserIniFilePath) - 1;

			if ((res = RegQueryValueEx(Key, "ProjectDir", 0, NULL, (LPBYTE)str, (PDWORD)& KeySize))
				== ERROR_SUCCESS)
			{
				strcpy(UserIniFilePath, str);
				ok = 1;
			}

			RegCloseKey(Key);
		}
	}

	if (UserIniFilePath[0] == 0)
		strcpy(UserIniFilePath, ExePath);

	sprintf(UserIniFile, "%s\\user.ini", UserIniFilePath);
	LoadUserIniFile();
	
    //MessageBox(NULL,DesignPath,"Design path",MB_APPLMODAL+MB_OK);

	ClosingWindowMessage = RegisterWindowMessage("CLOSING_WINDOW");

	sprintf(str, "%s\\LanguageGeom.txt", LanguagePath);

	if (AddGeomLanguageStrings(str) == -2)
		return -1;

	MemoryMain();
	ToetsMain();
	SetCursor(LoadCursor(0, IDC_ARROW));
	SystemCursor = GetCursor();

	ChangeFile(0, 2);

    //MessageBoxUTF8(NULL,EditFile,"edit file",MB_APPLMODAL+MB_OK);

	if (hPrevInstance == 0)
	{
		GEOMClass.hInstance = hInstance;
		GEOMClass.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(ICON1));

        //GEOMClass.hCursor = LoadCursor(0, IDC_ARROW);

		GEOMClass.hbrBackground = 0;

        //GEOMClass.hbrBackground = GetStockObject(BLACK_BRUSH);

		GEOMClass.lpszMenuName = MAKEINTRESOURCE(IDR_MENU1);

        //GEOMClass.lpszMenuName= "IDR_MENU1";

		if (!RegisterClass(&GEOMClass))
			exit(255);
	}

    //GetSystemMetrics   createfont

	if (ProjectActive)
	{
		OkToUseSharedMemory = 0;

		if ((SharedMemoryHandle = OpenFileMapping(FILE_MAP_WRITE, 0, MEMORYMAPPEDSTRING)))
		{
            // The memory mapped file has already been created by another applicatoin (Design manager)
			SharedMemory = (uint8 *) MapViewOfFile(SharedMemoryHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			if (SharedMemory != NULL)
			{
				OkToUseSharedMemory = 1;
				ProjectInfo = (ProjectInfoRecord *) SharedMemory;
			}

			ok = 1;
		}
	}

    //WindowWidth=0;

	memset(&ClientRect, 0, sizeof(RECT));
	ScreenSizeX = GetSystemMetrics(SM_CXMAXIMIZED) - 10;
	ScreenSizeY = GetSystemMetrics(SM_CYMAXIMIZED) - 10;
	ScreenStartX = 0;
	ScreenStartY = 0;

	AdjustWindowSize();
	ClipBoardGeomObjectsID = RegisterClipboardFormat("Geometry objects");
	AllocateMem();
	FontResourceHandle = FindResource(NULL, MAKEINTRESOURCE(IDR_OWNFONT1), "OWNFONT");
	count = SizeofResource(NULL, FontResourceHandle);
	FontGlobal = LoadResource(NULL, FontResourceHandle);
	FontData = LockResource(FontGlobal);
	memmove(Chars, FontData, count);

	OSInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	GetVersionEx(&OSInfo);

	switch (OSInfo.dwPlatformId)
	{
	case VER_PLATFORM_WIN32_NT:
		OperatingSystem = VER_PLATFORM_WIN32_NT;
		break;

	case VER_PLATFORM_WIN32_WINDOWS:
		OperatingSystem = VER_PLATFORM_WIN32_WINDOWS;
		break;

	case VER_PLATFORM_WIN32s:
		OperatingSystem = VER_PLATFORM_WIN32_WINDOWS;
		break;
	}

	HeightScrollBar = GetSystemMetrics(SM_CYHSCROLL), WidthScrollBar = GetSystemMetrics(SM_CXVSCROLL);

    //CreateWindowEx(GEOMClass.lpszClassName,
    //Creatoolbarex
    //CreateStatusWindow

	GEOMWindow = CreateWindow(GEOMClass.lpszClassName, NULL, WS_OVERLAPPEDWINDOW,

                              //WS_OVERLAPPEDWINDOW | WS_HSCROLL | WS_VSCROLL,

	                          WindowStartX, WindowStartY, WindowWidth, WindowHeight, HWND_DESKTOP, 0, hInstance, NULL);

	hwndHBar =
	    CreateWindow("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_HORZ, 0, 0, 0, 0, GEOMWindow, (HMENU) 1, hInstance,
	                 NULL);
	hwndVBar =
	    CreateWindow("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_VERT, 0, 0, 0, 0, GEOMWindow, (HMENU) 2, hInstance,
	                 NULL);

    //CreateStatusWindow
    //MakeMenu((MenuRecord *)&NewMenu);

	TimerObject = SetTimer(GEOMWindow, TimerIdentifier, 100, NULL);
	CheckExpandedCtrlKeys(0);
	MakeMainMenu();

	if (ProjectActive)
		InsertWindowInProject(GEOMWindow, 0);

	MaxScrollBarX = 10000;
	MaxScrollBarY = 10000;
	SetScrollRange(hwndHBar, SB_CTL, 0, MaxScrollBarX, 1);
	SetScrollRange(hwndVBar, SB_CTL, 0, MaxScrollBarY, 1);

    //SetScrollRange(GEOMWindow,SB_VERT,0,MaxScrollBarX,1);
    //SetScrollRange(GEOMWindow,SB_HORZ,0,MaxScrollBarY,1);

	ChangeMenuSelectionMode();
	ChangeMenuRelativeMousePosition();

	if (Units == 0)
		PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MILS, (LPARAM) NULL);
	else
		PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MM, (LPARAM) NULL);

	FirstView = 1;

	ShowWindow(GEOMWindow, nCmdShow);
	UpdateWindow(GEOMWindow);
	SetWindowName(0);
	DrawMenuBar(GEOMWindow);

	if (WindowInitialSized)
	{
		InvalidateRect(GEOMWindow, 0, 1);
		PostMessage(GEOMWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
	}

	while ((!TotalExit) && (GetMessage(&M, 0, 0, 0)))
	{
		TranslateMessage(&M);
		DispatchMessage(&M);

        //while ( (! exit22)
        //&&
        //((NrFunctionsInBuf>0) || (MouseChanged)) ) MainEditFunction(&exit22);
        //if ( exit22 ) PostQuitMessage(0);

		if (Focused)
			MainLoop();
	}

	KillTimer(GEOMWindow, TimerIdentifier);

	if (Created)
		DestroyWindow(GEOMWindow);

	ClipCursor(NULL);

	/*
	  if (SharedMemoryHandle) {
	    Close memory mapped file handle
	    CloseHandle(SharedMemoryHandle);
	  }
	*/

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
