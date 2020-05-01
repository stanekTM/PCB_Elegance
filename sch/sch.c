/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: sch.c
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


#define  _WIN32_WINNT       0x0501

#include "types.h"
#include "line2.h"
#include "stdio.h"
#include "rect.h"
#include "memory.h"
#include "sch.h"
#include "io.h"
#include "string.h"
#include "stdlib.h"
#include "toets.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "calc.h"
#include "calcdef.h"
#include "menus.h"
#include "check.h"
#include "files.h"
#include "files2.h"
#include "inscomp.h"
#include "dialogs.h"
#include "ellipss.h"
#include "mainloop.h"
#include "resource.h"
#include "command.h"
#include "graphics.h"
#include "direct.h"
#include "messages.h"
#include "print.h"
#include "params.h"
#include "utf8.h"
#include "property.h"
#include "owntime.h"
#include "uservar.h"
#include "own_process.h"


#define WANT_GETLONGPATHNAME_WRAPPER
#define COMPILE_NEWAPIS_STUBS

#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL                   0x020A
#endif

#define  SM_XVIRTUALSCREEN              76
#define  SM_YVIRTUALSCREEN              77
#define  SM_CXVIRTUALSCREEN             78
#define  SM_CYVIRTUALSCREEN             79
#define  SM_CMONITORS                   80
#define  SM_SAMEDISPLAYFORMAT           81

#define  NR_MILLI_SECONDS_TIMER         100
#define  MAX_PARAMETERS                 20

#define  SCROLL_FACTOR                  (double)1.0
#define  MEMORYMAPPEDSTRING             "MMFILE_PCB_ELEGANCE"
#define  AtomStrSchematicSelect         "Layout_editor_active_schematic_select"

#define  ScreenPosAbsCursor             3   //10 grafika pozic x,y
#define  ScreenPosAbsGridCursor         200 //80
#define  ScreenPosRelGridCursor         200 //80
#define  ScreenPosInfoStr               376 //150

#define InRangeScreen(x,y) ( ((((x>=AllScreenX1) && (x<AllScreenX2)) || (x==-10000))                 \
                             &&                                                                       \
                             (((y>=AllScreenY1) && (y<AllScreenY2)) || (y==-10000))) ? 1 : 0 )


HCURSOR CurrentCursor;
HPALETTE OldPalette;
HCURSOR OldCursor, Cursor1;
HWND DialogWindow, SCHFrameWindow, HScrollWindow, MasterWindow, hwndHBar, hwndVBar;
HDC DialogDisplay, OutputDisplay2;
HBITMAP ViewBitmap;
HRSRC FontResourceHandle;
PAINTSTRUCT PS;
RECT RealWindow, ClientRect, UpdateRect, ScreenRect, RealWindow2, OldClientRect, OldRealWindow, OldRealWindow2;
HRGN EditingRegion, TempRegion;
HGLOBAL FontGlobal;
int32 SCHCursorActive;
int32 TotalExit = 0;
int32 FocusedAgain = 1;
int32 FirstPaintMode = 1;
int32 ProjectActive = 0;
int32 PaintBecauseOfResize = 0;
int32 WindowMaximized = 0;
int32 WindowMinimized = 0;
int32 StartFromLibraryManager = 0;
int32 DirectPrinting = 0;
int32 FastPaint = 1;
int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY, TimerInfoStr, ScreenSizeX, ScreenSizeY,
      ScreenStartX, ScreenStartY, EditMode, GeomScreenWidth, GeomScreenHeight, HeightScrollBar, WidthScrollBar,
      GeomStartX, GeomStartY, FirstSize, ScrollBarSizeX, ScrollBarSizeY, MousePosOldX, MousePosOldY, AllScreenX1,
      AllScreenY1, AllScreenX2, AllScreenY2;

int32 HeightInfoBar = 21;
int32 WidthButtonBar = 32;
int32 PaintBecauseOfResizeMode = 0;
int32 FirstDraw = 1;
int32 ReSizes = 0;
int32 ZoomCounter = 0;
int32 TimerValue = 0;
int32 NrPaintMessages = 0;
int32 DirectPrintingPaperSize = PAPERSIZE_A4;
int32 DirectPrintingOrientation = ORIENTATION_AUTO;
int32 DirectPrintingFitToPage = 1;
int32 LibrarySymbolNr = -1;
int32 StartupMessages[128];
int32 NrStartupMessages = 0;
int32 CheckSchematic;
int32 WaitForPaint;
int32 ViewPixelsX;
int32 ViewPixelsY;
uint8 *FontData;
UINT TimerObject, ClipBoardSchematicID, ClipBoardSelectionsSchematicID;
UINT TimerIdentifier = 0x12345678;
UINT StartUpMessage;

uint8 ButtonSaveMem[32768];

char IniFile[MAX_LENGTH_STRING], StartDir[MAX_LENGTH_STRING], ExecutableDir[MAX_LENGTH_STRING],
     PrintingParameters[MAX_LENGTH_STRING], SchExecutable[MAX_LENGTH_STRING], MessageStr[MAX_LENGTH_STRING];
ProjectInfoRecord *ProjectInfo;
uint8 *SharedMemory;
HANDLE *SharedMemoryHandle;
int32 OkToUseSharedMemory;
int32 ProjectIndexNr = -1;
WIN32_FIND_DATA FindData;

extern HFONT SaveFont;
extern int32 ButtonSizeX;
extern int32 ButtonSizeY;
extern int32 NrButtonsSheet;
extern int32 NrButtonsSymbol;
extern int32 SymbolDialogInitialX, SymbolDialogInitialY, GeometryDialogInitialX, GeometryDialogInitialY,
       ComponentDialogInitialX, ComponentDialogInitialY;
extern int32 ChangeViewMode, ActiveViewGeometry;
extern char UsedFiles[16][MAX_LENGTH_STRING], PrinterName[MAX_LENGTH_STRING];
extern int32 NrUsedFiles, PrintOptions;

extern COLORREF BackGroundColor;
extern HGDIOBJ BackGroundBrush;


char SchClassName[] = "SCH";
OSVERSIONINFO OSInfo;

int32 Counter0;
float GotoXY_X = 100000.0, GotoXY_Y = 100000.0;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

HWND GetDesignManagersWindow(int32 mode)
{
	HWND DesignWindow;

	int32 cnt;

	cnt = 0;

	while ((cnt < 32) && (ProjectInfo->FileTypes[cnt] != 10))
		cnt++;

	if ((cnt < 32) && (ProjectInfo->WindowHandles[cnt]))
		return ProjectInfo->WindowHandles[cnt];

	DesignWindow = FindWindow("DESIGN", NULL);
	return DesignWindow;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InsertWindowInProject(HWND Window, int32 mode)
{
	int32 cnt;
	DWORD PID, PID2;
	HWND DesignWindow;

	if (ProjectInfo == NULL)
		return;

	cnt = 0;

	while ((cnt < 32) && (ProjectInfo->FileTypes[cnt] != 0))
		cnt++;

	if (cnt == 32)
		return;

	ProjectIndexNr = cnt;
	ProjectInfo->FileTypes[cnt] = 1;
	ProjectInfo->WindowHandles[cnt] = Window;
	// Search for Design managers window
	DesignWindow = GetDesignManagersWindow(0);

	if (DesignWindow)
	{
		PID = GetWindowThreadProcessId(DesignWindow, &PID2);

		if (PID2)
		{
#ifndef SPM
			AllowSetForegroundWindow(PID2);
#endif
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void WindowCreate()
{
	int32 res;

	ClipCursor(NULL);

	StartUpMessage = RegisterWindowMessage("Startup");
	res = PostMessage(NULL, StartUpMessage, (WPARAM) NULL, (LPARAM) NULL);
	Created = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SetNewCursor(double *CurrentX, double *CurrentY)
{
	int32 mx, my;

	mx = max(0, MultX(*CurrentX));
	mx = min(ClientWindowDivX - 1, mx);
	my = max(0, MultY(*CurrentY));
	my = min(ClientWindowDivY - 1, my);
	MousePosX = mx;
	MousePosY = my;
	SetCursorPos(mx + ClientStartX, my + ClientStartY);
	*CurrentX = PixelToRealOffX(mx);
	*CurrentY = PixelToRealOffX(my);
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SetWaitCursor()
{
	SetCursor(LoadCursor(NULL, IDC_WAIT));
	CurrentCursor = GetCursor();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SetNormalCursor()
{
	SetCursor(LoadCursor(0, IDC_ARROW));
	CurrentCursor = GetCursor();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AdjustWindowSize()
{
	POINT TestPixel;
	HWND TestWindow1, TestWindow2, TestWindow3, TestWindow4;

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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InitDeviceContext()
{
	if (!DCInUse)
	{
		DCInUse = 1;

		if (Painting)
		{
			if (!Printing)
				OutputDisplay = BeginPaint(SCHWindow, &PS);
		}
		else
		{
			if (!Printing)
				OutputDisplay = GetDC(SCHWindow);
		}

		SetROP2(OutputDisplay, R2_COPYPEN);

		if (!Printing)
			SetBkMode(OutputDisplay, TRANSPARENT);

		SetPolyFillMode(OutputDisplay, WINDING);
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
			OutputDisplay2 = BeginPaint(SCHWindow, &PS);
			SetArcDirection(OutputDisplay2, AD_COUNTERCLOCKWISE);
		}
		else
			OutputDisplay2 = GetDC(SCHWindow);

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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void StartDrawingEditingWindow()
{
	InitDeviceContext();

	EditingRegion =
	    CreateRectRgn((int16) DrawWindowMinX, (int16) DrawWindowMinY, (int16) DrawWindowMaxX, (int16) DrawWindowMaxY);

	if (!Printing)
		SelectClipRgn(OutputDisplay, EditingRegion);

	SetROP2(OutputDisplay, R2_COPYPEN);
	BackGroundActive = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DoneDeviceContext()
{
	if (DCInUse)
	{
		if (Painting)
			EndPaint(SCHWindow, &PS);
		else
			ReleaseDC(SCHWindow, OutputDisplay);

		OutputDisplay = NULL;
		DCInUse = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DoneDeviceContext2()
{

	if (DCInUse)
	{
		BitBlt(OutputDisplay2, 0, 0, ViewPixelsX, ViewPixelsY, OutputDisplay, 0, 0, SRCCOPY);
		GdiFlush();

		if (ViewBitmap)
			DeleteObject(ViewBitmap);

		DeleteDC(OutputDisplay);

		if (Painting)
			EndPaint(SCHWindow, &PS);
		else
			ReleaseDC(SCHWindow, OutputDisplay2);

		DCInUse = 0;
		ViewBitmap = NULL;
		OutputDisplay = NULL;
		OutputDisplay2 = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void EndDrawingEditingWindow()
{

	if (!Printing)
		SelectClipRgn(OutputDisplay, NULL);

	DeleteObject(EditingRegion);

	DoneDeviceContext();
//  DeleteGraphicObjects();

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SetScrollPosition()
{
	double dx, dy, ccy, ccx, cx, tx, cy, ty, hulp2;

	dx = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
	dy = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
	ccx = (PixelToRealOffX(DrawWindowMaxX) + PixelToRealOffX(DrawWindowMinX)) * (double) 0.5;
	ccy = (PixelToRealOffY(DrawWindowMaxY) + PixelToRealOffY(DrawWindowMinY)) * (double) 0.5;

	cy = (VisibleMaxY + VisibleMinY) / 2;
	ty = (VisibleMaxY - VisibleMinY) * SCROLL_FACTOR;
	hulp2 = ((ccy - cy) * MaxScrollBarY / ty) + MaxScrollBarY / 2;

	if (hulp2 > MaxScrollBarY)
		hulp2 = MaxScrollBarY;

	if (hulp2 < 0)
		hulp2 = (double) 0.0;

	hulp2 = MaxScrollBarY - hulp2;
	hulp2 = hulp2 * (MaxScrollBarY - ScrollBarSizeY) / MaxScrollBarY;
	SetScrollPos(hwndVBar, SB_CTL, (int32) hulp2, 1);

	cx = (VisibleMaxX + VisibleMinX) / 2;
	tx = (VisibleMaxX - VisibleMinX) * SCROLL_FACTOR;
	hulp2 = ((ccx - cx) * MaxScrollBarX / tx) + MaxScrollBarX / 2;

	if (hulp2 > MaxScrollBarX)
		hulp2 = MaxScrollBarX;

	if (hulp2 < 0)
		hulp2 = (double) 0.0;

	hulp2 = hulp2 * (MaxScrollBarX - ScrollBarSizeX) / MaxScrollBarX;
	SetScrollPos(hwndHBar, SB_CTL, (int32) hulp2, 1);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RedrawMainWindow()
{
	RECT Rect;
	int32 minx, maxx, miny, maxy;
	HRGN hrgn;

	minx = max(UpdateRect.left, (int16) DrawWindowMinX);
	maxx = min(UpdateRect.right, (int16) DrawWindowMaxX);
	miny = max(UpdateRect.top, (int16) DrawWindowMinY);
	maxy = min(UpdateRect.bottom, (int16) DrawWindowMaxY);

	hrgn = CreateRectRgn(minx, miny, maxx, maxy);
	SelectClipRgn(OutputDisplay, hrgn);

	Rect.left = minx;
	Rect.right = maxx;
	Rect.top = miny;
	Rect.bottom = maxy;
	FillRect(OutputDisplay, &Rect, BackGroundBrush);

	if (!ActiveViewGeometry)
	{
		if (OkToAddViewPos)
			SaveViewPos();

		OkToAddViewPos = 1;
		ViewMinX = (double) PixelToRealOffX(minx - 1);
		ViewMaxX = (double) PixelToRealOffX(maxx + 1);
		ViewMinY = (double) PixelToRealOffY(DrawWindowMaxY - (maxy + 1) - 1);
		ViewMaxY = (double) PixelToRealOffY(DrawWindowMaxY - (miny - 1) - 1);


		DrawInstances(0);

		if (!EditingSymbol)
		{
			DrawWires(0);
			DrawBusses(0);
			DrawJunctions(0);
			DrawOnePinNets(0);
			DrawBusConnections(0);
			DrawGlobalConnections(0);

			if ((Design.SheetInfo & 1) == 0)
				DrawNetLabels(0);
		}
		else
		{
			DrawPins(0);
			DrawPowerPins(0);
			DrawPinBusses(0);
		}

		DrawObjectLines(0);
		DrawObjectRects(0);
		DrawObjectCircles(0);
		DrawObjectArcs(0);
		DrawObjectTexts(0);

		if (GridVisible)
			DrawGrid();

		SetScrollPageSize();
		SetScrollPosition();

	}

	ExitDrawing();
	SelectClipRgn(OutputDisplay, NULL);
	DeleteObject(hrgn);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RedrawButtons()
{
	RECT Rect;
	HBITMAP ButtonBitmap;
	HDC ButtonMemoryDC;
	int32 res, NrButtonsTemp, minx, maxx, miny, maxy;
	HRGN hrgn;
	HGDIOBJ old;

	minx = max(UpdateRect.left, 0);
	maxx = min(UpdateRect.right, DrawWindowMinX);
	miny = max(UpdateRect.top, 0);
	maxy = min(UpdateRect.bottom, ClientRect.bottom - HeightInfoBar);

	hrgn = CreateRectRgn(minx, miny, maxx, maxy);
	SelectClipRgn(OutputDisplay, hrgn);


	Rect.left = 0;
	Rect.right = (int16) DrawWindowMinX;
	Rect.top = 0;
	Rect.bottom = ClientRect.bottom - HeightInfoBar;
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left++;
	Rect.top++;
	Rect.right--;
	Rect.bottom--;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));

	if (!EditingSymbol)
	{
		ButtonBitmap = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTON_SCH));
		NrButtonsTemp = NrButtonsSheet;
	}
	else
	{
		ButtonBitmap = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTON_SYM));
		NrButtonsTemp = NrButtonsSymbol;
	}

	ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
	old = SelectObject(ButtonMemoryDC, ButtonBitmap);
	res = BitBlt(OutputDisplay, 1, 1, 28, ButtonSizeY * NrButtonsTemp + 2, ButtonMemoryDC, 0, 0, SRCCOPY);
	SelectObject(ButtonMemoryDC, old);
	DeleteDC(ButtonMemoryDC);
	DeleteObject(ButtonBitmap);
	DeleteObject(hrgn);
}

//********************************************************************************************************************************
//*************************** grafika absolutní x,y ******************************************************************************
//********************************************************************************************************************************

void RedrawAbsPosStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosAbsCursor; //ScreenPosAbsCursor + 1
	LeftTopY = ClientRect.bottom - (HeightInfoBar); //(HeightInfoBar - 4)
	RightBottomX = ScreenPosAbsGridCursor - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	if (FirstPaint)
		return;

	switch (Mode)
	{
	case 0:
		SelectClipRgn(OutputDisplay, NULL);
		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);
	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));

    //Rect.left = ScreenPosAbsCursor + 1;

	Rect.right = ScreenPosAbsGridCursor - 3;
	Rect.top = ClientRect.bottom - (HeightInfoBar - 4);
	Rect.bottom = ClientRect.bottom - 3;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosAbsCursor, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR) & AbsPosStr, strlen(AbsPosStr)); //spodní pøeklad absolutní

	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:
		SelectClipRgn(OutputDisplay, EditingRegion);
		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
}

//********************************************************************************************************************************
//********************************************************************************************************************************
//********************************************************************************************************************************

void RedrawAbsGridPosStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosAbsGridCursor + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ScreenPosRelGridCursor - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	if (FirstPaint)
		return;

	switch (Mode)
	{
	case 0:
		SelectClipRgn(OutputDisplay, NULL);
		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);
	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));

	Rect.left = ScreenPosAbsGridCursor + 1;
	Rect.right = ScreenPosRelGridCursor - 3;
	Rect.top = ClientRect.bottom - (HeightInfoBar - 4);
	Rect.bottom = ClientRect.bottom - 3;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosAbsGridCursor + 2, ClientRect.bottom - (HeightInfoBar - 4),
	           (LPSTR) & AbsGridPosStr, strlen(AbsGridPosStr)); //spodní pøeklad 2

	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:

        //SelectClipRgn(OutputDisplay, EditingRegion);

		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
}

//********************************************************************************************************************************
//*************************** grafika møížka x,y *********************************************************************************
//********************************************************************************************************************************

void RedrawRelPosStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosRelGridCursor + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ScreenPosInfoStr - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	if (FirstPaint)
		return;

	switch (Mode)
	{
	case 0:
		SelectClipRgn(OutputDisplay, NULL);
		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);
	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));

	Rect.left = ScreenPosRelGridCursor + 1;
	Rect.right = ScreenPosInfoStr - 3;
	Rect.top = ClientRect.bottom - (HeightInfoBar - 4);
	Rect.bottom = ClientRect.bottom - 3;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosRelGridCursor + 2, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR) & RelPosStr, strlen(RelPosStr)); //spodní pøeklad møížka

	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:
//      SelectClipRgn(OutputDisplay, EditingRegion);
		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
}

//****************************************************************************************************************************************
//****************************** pravý spodní pøeklad vybraných symbolù ******************************************************************
//****************************************************************************************************************************************

void RedrawInfoStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;

	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosInfoStr + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ClientRect.right - 4;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:

        //SelectClipRgn(OutputDisplay, NULL);

		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);
	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));
	Rect.left = ScreenPosInfoStr + 1;
	Rect.right = ClientRect.right - 4;
	Rect.top = ClientRect.bottom - (HeightInfoBar - 4);
	Rect.bottom = ClientRect.bottom - 3;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutUTF8(OutputDisplay, ScreenPosInfoStr + 2, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR)&InfoStr, strlen(InfoStr)); //pravý spodní pøeklad vybraných symbolù

	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:
		SelectClipRgn(OutputDisplay, NULL);

        //SelectClipRgn(OutputDisplay, EditingRegion);

		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RedrawInfoBar()
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 minx, maxx, miny, maxy;
	HRGN hrgn;

	minx = max(UpdateRect.left, 0);
	maxx = min(UpdateRect.right, (int16) ClientRect.right);
	miny = max(UpdateRect.top, ClientRect.bottom - HeightInfoBar - HeightScrollBar);
	maxy = min(UpdateRect.bottom, (int16) ClientRect.bottom);

	hrgn = CreateRectRgn(minx, miny, maxx, maxy);
	SelectClipRgn(OutputDisplay, hrgn);

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
//  SaveFont=SelectObject(OutputDisplay,GetStockObject(SYSTEM_FONT));
//  if (DrawWindowMaxY<ClientRect.bottom-1) {
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
//    FillRect(OutputDisplay,&Rect,GetStockObject(WHITE_BRUSH));
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
//    FillRect(OutputDisplay,&Rect,GetStockObject(WHITE_BRUSH));
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
//    FillRect(OutputDisplay,&Rect,GetStockObject(GRAY_BRUSH));
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
//    FillRect(OutputDisplay,&Rect,GetStockObject(GRAY_BRUSH));
	Rect.left = ScreenPosRelGridCursor + 1;
	Rect.right = ScreenPosInfoStr - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));
	Rect.left = ScreenPosInfoStr + 1;
	Rect.right = ClientRect.right - 2;
	FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));


	RedrawAbsPosStr(2);
	RedrawRelPosStr(2);
	RedrawInfoStr(2);

	SelectObject(OutputDisplay, SaveFont);
	DeleteObject(hrgn);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void WindowPaint(int32 mode)
{
	RECT Rect, Rect2;
	int32 ok, res;

	GetClientRect(SCHWindow, &Rect);

	if (GetUpdateRect(SCHWindow, &UpdateRect, 0))
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

			if (UpdateRect.bottom >= (int16) DrawWindowMaxY + HeightScrollBar)
				RedrawInfoBar();

			if (UpdateRect.left < (int16) DrawWindowMinX)
				RedrawButtons();

			if ((UpdateRect.bottom >= (int16) DrawWindowMinY) && (UpdateRect.top < (int16) DrawWindowMaxY))
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
			res = GetUpdateRect(SCHWindow, &UpdateRect, 0);

			if ((PaintBecauseOfResizeMode & (128 + 64 + 32 + 16 + 8 + 4)) != 0)
			{
				Rect2.left = 0;
				Rect2.right = ClientRect.right;
				Rect2.top = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				Rect2.bottom = ClientRect.bottom;

				if ((PaintBecauseOfResizeMode & (4)) != 0)
					Rect2.left = OldClientRect.right - WidthScrollBar - 1;

				if ((PaintBecauseOfResizeMode & (8)) != 0)
					Rect2.left = OldClientRect.right - WidthScrollBar - 1;

				if ((PaintBecauseOfResizeMode & (16 + 32)) != 0)
					Rect2.left = 0;

				InvalidateRect(SCHWindow, &Rect2, 0);
				res = GetUpdateRect(SCHWindow, &UpdateRect, 0);
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

				InvalidateRect(SCHWindow, &Rect2, 0);
				res = GetUpdateRect(SCHWindow, &UpdateRect, 0);
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
				InvalidateRect(SCHWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (16)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = ClientRect.right;
				Rect2.top =
				    ClientRect.bottom - HeightInfoBar - HeightScrollBar - abs(ClientRect.bottom - OldClientRect.bottom);
				Rect2.bottom = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				InvalidateRect(SCHWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (16 + 1)) != 0)
			{
				res = GetUpdateRect(SCHWindow, &UpdateRect, 0);
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
				InvalidateRect(SCHWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (64)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = DrawWindowMinX + abs(ClientRect.right - OldClientRect.right);
				Rect2.top = 0;
				Rect2.bottom = ClientRect.bottom - HeightInfoBar;
				InvalidateRect(SCHWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (64 + 4)) != 0)
			{
				res = GetUpdateRect(SCHWindow, &UpdateRect, 0);
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

//        SetScrollPageSize();
//        SetScrollPosition();
			PaintBecauseOfResizeMode = 0;
		}

		ViewMinX = (double) PixelToRealOffX(-1);
		ViewMaxX = (double) PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = (double) PixelToRealOffY(-1);
		ViewMaxY = (double) PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		PaintBecauseOfResize = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ScrollUp(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	DrawCrossHair(2);
	plus = PixelToReal(SizeOfScroll);
	Yoffset += plus;
	Rect.left = (int16) DrawWindowMinX;
	Rect.top = (int16) DrawWindowMinY;
	Rect.right = (int16) DrawWindowMaxX;
	Rect.bottom = (int16) DrawWindowMaxY;
	Rect2.left = (int16) DrawWindowMinX;
	Rect2.top = (int16) DrawWindowMinY;
	Rect2.right = (int16) DrawWindowMaxX;
	Rect2.bottom = (int16) DrawWindowMaxY;
	UpdateWindow(SCHWindow);
	ScrollWindow(SCHWindow, 0, SizeOfScroll, &Rect, &Rect2);
	UpdateWindow(SCHWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ScrollDown(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	DrawCrossHair(2);
	plus = PixelToReal(SizeOfScroll);
	Yoffset -= plus;
	Rect.left = (int16) DrawWindowMinX;
	Rect.top = (int16) DrawWindowMinY;
	Rect.right = (int16) DrawWindowMaxX;
	Rect.bottom = (int16) DrawWindowMaxY;
	Rect2.left = (int16) DrawWindowMinX;
	Rect2.top = (int16) DrawWindowMinY;
	Rect2.right = (int16) DrawWindowMaxX;
	Rect2.bottom = (int16) DrawWindowMaxY;
	UpdateWindow(SCHWindow);
	ScrollWindow(SCHWindow, 0, -SizeOfScroll, &Rect, &Rect2);
	UpdateWindow(SCHWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ScrollLeft(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	DrawCrossHair(2);
	plus = PixelToReal(SizeOfScroll);
	Xoffset -= plus;
	Rect.left = (int16) DrawWindowMinX;
	Rect.right = (int16) DrawWindowMaxX;
	Rect.top = (int16) DrawWindowMinY;
	Rect.bottom = (int16) DrawWindowMaxY;
	Rect2.left = (int16) DrawWindowMinX;
	Rect2.top = (int16) DrawWindowMinY;
	Rect2.right = (int16) DrawWindowMaxX;
	Rect2.bottom = (int16) DrawWindowMaxY;
	UpdateWindow(SCHWindow);
	ScrollWindow(SCHWindow, SizeOfScroll, 0, &Rect, &Rect2);
	UpdateWindow(SCHWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ScrollRight(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	DrawCrossHair(2);
	plus = PixelToReal(SizeOfScroll);
	Xoffset += plus;
	Rect.left = DrawWindowMinX;
	Rect.right = (int16) DrawWindowMaxX - ScrollSize - 1;
	Rect.right = (int16) DrawWindowMaxX;
	Rect.top = (int16) DrawWindowMinY;
	Rect.bottom = (int16) DrawWindowMaxY;
	Rect2.left = (int16) DrawWindowMinX;
	Rect2.top = (int16) DrawWindowMinY;
	Rect2.right = (int16) DrawWindowMaxX;
	Rect2.bottom = (int16) DrawWindowMaxY;
	UpdateWindow(SCHWindow);
	ScrollWindow(SCHWindow, -SizeOfScroll, 0, &Rect, &Rect2);
	UpdateWindow(SCHWindow);

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

	DrawCrossHair(2);
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
	UpdateWindow(SCHWindow);
	ScrollWindow(SCHWindow, DivX, DivY, &Rect, &Rect2);
	UpdateWindow(SCHWindow);

	if (SystemBusyMode == 0)
		DrawCrossHair(0);

	DisplayCursorPosition();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ClipMouseCursor()
{
	RECT Rect;
	POINT P;

	P.x = DrawWindowMinX;
	P.y = DrawWindowMinY;
	ClientToScreen(SCHWindow, &P);
	Rect.left = P.x;
	Rect.top = P.y;

	P.x = DrawWindowMaxX;
	P.y = DrawWindowMaxY;
	ClientToScreen(SCHWindow, &P);
	Rect.right = P.x;
	Rect.bottom = P.y;
	ClipCursor(&Rect);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void UnClipMouseCursor()
{
	ClipCursor(NULL);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

LRESULT CALLBACK SCHWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam)
{
	POINT pp;
	double hulp2;
	int32 res, nr, v1, v2, xPos, yPos, ok, hulp3, divx, divy, le, MousePosY2;
	double dx, dy, cx, cy, ccy, ty, tx;
	CREATESTRUCT *CreateInfo;
	LRESULT Result;
	char str[600], str2[600], EditFile2[600];

//  CrtWindow = Window;
	if (NrStartupMessages < 128)
	{
		StartupMessages[NrStartupMessages++] = Message;
#ifdef _DEBUG
		GetMessageString(Message, str);
		ok = 1;
#endif
	}

	if ((Message >= 0x201) && (Message <= 0x209))
		ok = 1;



	switch (Message)
	{
//    case WM_NCCREATE:
//      SCHWindow=Window;
//      return 1;
//      Childs[NrChilds-1];
//      break; WM_NCACTIVATE
//    case WM_HELP:
//      HelpAsked=1;
//      break;
	case WM_HELP:
//      Focused=0;
		ok = 1;
		break;

	case WM_NOTIFY:
		ok = 1;
		break;

	case WM_COMMAND:
		SCHCommand(Window, WParam, (int32) LParam);
		ok = 1;
		break;

	case WM_CREATE:
		CreateInfo = (CREATESTRUCT *) LParam;
		WindowCreate();
		break;

	case WM_PAINT:
		if (FirstPaint)
		{
			FirstPaint = 0;

			if (EditingSymbol)
			{
				SetClassLong(SCHWindow, GCL_HICON, (LONG) LoadIcon(SCHClass.hInstance, MAKEINTRESOURCE(ICON_SYMBOL)));
				SetSymbolMenu();
			}
			else
			{
				SetSheetMenu();
				SetClassLong(SCHWindow, GCL_HICON, (LONG) LoadIcon(SCHClass.hInstance, MAKEINTRESOURCE(ICON_SHEET)));
			}

			AddMenuFiles();
			ViewWholeDesign(0);
			SetScrollPageSize();
			SetScrollPosition();
			UpdateWindow(hwndHBar);
			UpdateWindow(hwndVBar);
			FirstPaintMode = 0;
		}

		WindowPaint(0);
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
					DisplayInfoOff(0);
					ScrollLeft(-v2);
					ScrollLeft(-v2);
					ScrollLeft(-v2);
					ScrollLeft(-v2);
				}
				else
				{
					DisplayInfoOff(0);
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
					{
						if (SystemBusyMode == 0)
							ZoomIn(2);
					}
					else
					{
						if (SystemBusyMode == 0)
							ZoomOut(2);
					}
				}
				else
				{
					if (v2 < 0)
					{
						DisplayInfoOff(0);
						ScrollDown(-v2);
						ScrollDown(-v2);
						ScrollDown(-v2);
						ScrollDown(-v2);
					}
					else
					{
						DisplayInfoOff(0);
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
				hulp2 = (double) (HIWORD(WParam));
				hulp2 = hulp2 * MaxScrollBarY / (MaxScrollBarY - ScrollBarSizeY);
				hulp2 = (double) (MaxScrollBarY - hulp2);
				dy = PixelToRealOffY(DrawWindowMaxY) - PixelToRealOffY(DrawWindowMinY);
				ccy = (PixelToRealOffY(DrawWindowMaxY) + PixelToRealOffY(DrawWindowMinY)) * (double) 0.5;
				cy = (VisibleMaxY + VisibleMinY) / 2;
				ty = (VisibleMaxY - VisibleMinY) * SCROLL_FACTOR;
				hulp2 = ((hulp2 - MaxScrollBarY / 2) * ty / MaxScrollBarY) - (dy * (double) 0.5) + cy;
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
			hulp2 = (double) HIWORD(WParam);
			hulp2 = hulp2 * MaxScrollBarX / (MaxScrollBarX - ScrollBarSizeX);
			dx = PixelToRealOffX(DrawWindowMaxX) - PixelToRealOffX(DrawWindowMinX);
			cx = (VisibleMaxX + VisibleMinX) / 2;
			tx = (VisibleMaxX - VisibleMinX) * SCROLL_FACTOR;
			hulp2 = ((hulp2 - MaxScrollBarX / 2) * tx / MaxScrollBarX) - (dx * (double) 0.5) + cx;
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
		if (WParam == SIZE_MAXIMIZED)
			WindowMaximized = 1;

		if (WParam == SIZE_MINIMIZED)
			WindowMinimized = 1;

		memmove(&OldClientRect, &ClientRect, sizeof(RECT));
		memmove(&OldRealWindow, &RealWindow, sizeof(RECT));
		GetClientRect(SCHWindow, &ClientRect);
		GetWindowRect(SCHWindow, &RealWindow);
//      sprintf(str,"%4i  %4i  %4i \n",RealWindow.left,OldRealWindow2.left,RealWindow2.left);
//      OutputDebugStr(str);
		DrawWindowMinX = WidthButtonBar;
		DrawWindowMinY = 0;
		DrawWindowMaxX = ClientRect.right - WidthScrollBar;
		DrawWindowMaxY = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
		ClientWindowDivX = DrawWindowMaxX - DrawWindowMinX;
		ClientWindowDivY = DrawWindowMaxY - DrawWindowMinY;
		WindowStartX = (int16) RealWindow.left;
		WindowStartY = (int16) RealWindow.top;
		WindowWidth = (int16) (RealWindow.right - RealWindow.left);
		WindowHeight = (int16) (RealWindow.bottom - RealWindow.top);
		ScrollSize = (min(DrawWindowMaxX - DrawWindowMinX, DrawWindowMaxY - DrawWindowMinY) / 4) & ~3;;
		ScrollEndOfWindow = 25;
//      ScrollEndOfWindow=ScrollSize/2;
		ScrollSizeDrawing = ScrollSize / 2;

		if (GeomScreenWidth == 0)
		{
			hulp3 = GetSystemMetrics(SM_CXFULLSCREEN);
			GeomScreenWidth = hulp3 / 3;
			GeomStartX = (hulp3 * 2) / 3 - 4;
			GeomStartY = 0;
			GeomScreenHeight = GetSystemMetrics(SM_CYFULLSCREEN) - 60;
		}

		pp.x = 0;
		pp.y = 0;
		ClientToScreen(SCHWindow, &pp);
		ClientStartX = (int16) pp.x;
		ClientStartY = (int16) pp.y;

		if ((ReSizes >= 2) && (WParam != SIZE_MAXIMIZED) && (!WindowMinimized) && (!WindowMaximized))
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

		ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

		MoveWindow(hwndHBar, DrawWindowMinX, DrawWindowMaxY, DrawWindowMaxX - WidthButtonBar, HeightScrollBar, 0);
		MoveWindow(hwndVBar, DrawWindowMaxX, DrawWindowMinY, WidthScrollBar, DrawWindowMaxY, 0);

		if (hwndHBar != NULL)
		{
			RedrawWindow(hwndHBar, NULL, NULL, RDW_INVALIDATE | RDW_ERASENOW | RDW_ALLCHILDREN);
			RedrawWindow(hwndVBar, NULL, NULL, RDW_INVALIDATE | RDW_ERASENOW | RDW_ALLCHILDREN);
		}

		memmove(&OldRealWindow2, &RealWindow, sizeof(RECT));
		ReSizes++;

		if (WParam != SIZE_MAXIMIZED)
			WindowMaximized = 0;

		if (WParam != SIZE_MINIMIZED)
			WindowMinimized = 0;

		if (WParam == SIZE_MAXIMIZED)
			DrawCrossHair(1);

		break;

	case WM_MOVE:
		memmove(&OldRealWindow2, &RealWindow2, sizeof(RECT));
		GetWindowRect(SCHWindow, &RealWindow);
		GetWindowRect(SCHWindow, &RealWindow2);
		pp.x = 0;
		pp.y = 0;
		ClientToScreen(SCHWindow, &pp);
		ClientStartX = (int16) pp.x;
		ClientStartY = (int16) pp.y;
		WindowStartX = (int16) RealWindow.left;
		WindowStartY = (int16) RealWindow.top;
		break;

	case WM_CHAR:
		if ((Focused) && (FocusedAgain))
			KeyChar((uint16) WParam);

		break;

	case WM_NCHITTEST:
		xPos = LOWORD(LParam);
		yPos = HIWORD(LParam);
		Result = DefWindowProc(Window, Message, WParam, LParam);

		if (Result == HTTOPLEFT)
			res = 1;

		if (Result != HTCLIENT)
		{
			MousePosX = 10000;
			MousePosY = 10000;
			DrawButtonInfoOff(0);
			DisplayInfoOff(0);
//        Beep(1000,100);
//        sprintf(str,"%d %d %d,%d\r\n",Result,Focused,xPos,yPos);
//        OutputDebugStr(str);
			ok = 1;
		}
		else
		{
//        MousePosX=xPos;
//        MousePosY=yPos;
		}

		return Result;

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

	case WM_MOUSEMOVE:
		if ((Focused) && (FocusedAgain))
		{
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;
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
			}
		}

		if (GetCursor() != CurrentCursor)
			SetCursor(CurrentCursor);

		break;

	case WM_LBUTTONDBLCLK:
		if ((Focused) && (FocusedAgain))
		{
			ok = 1;
			LeftButtonDoublePressed = 1;
		}

		break;

	case WM_LBUTTONDOWN:
		MousePosY2 = (int)(short)HIWORD(LParam) + 1;

		if ((Focused) && (FocusedAgain) && (MousePosY2 < DrawWindowMaxY))
		{
			MouseChanged = 1;
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;
			LeftButtonPressed = 1;
		}

		break;

	case WM_LBUTTONUP:
		if (Focused)
		{
			FocusedAgain = 1;
			MouseChanged = 1;
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;
			LeftButtonPressed = 0;
			LeftButtonDoublePressed = 0;
		}

		break;

	case WM_RBUTTONDBLCLK:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;
			RightButtonPressed = 1;
		}

		break;

	case WM_RBUTTONDOWN:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;
			RightButtonPressed = 1;
		}

		break;

	case WM_RBUTTONUP:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;
			RightButtonPressed = 0;
		}

		break;

	case WM_MBUTTONDOWN:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;

			if (GetSystemMetrics(SM_CMOUSEBUTTONS) == 3)
				MiddleButtonPressed = 1;
		}

		break;

	case WM_MBUTTONUP:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = (int)(short)LOWORD(LParam);
			MousePosY = (int)(short)HIWORD(LParam) + 1;
			MiddleButtonPressed = 0;
		}

		break;

	case WM_DROPFILES:
		nr = DragQueryFile((HANDLE) WParam, 0xFFFFFFFF, NULL, 0);

		if (nr == 1)
		{
			DragQueryFile((HANDLE) WParam, 0, str, 150);
			strcpy(EditFile2, str);
			DragFinish((HANDLE) WParam);

			if (SaveFile2(0) == 1)
			{
				ChangeFile(EditFile2, 0);
				ShowWindow(SCHWindow, SW_NORMAL);
				BringWindowToTop(SCHWindow);
			}
		}

		break;

	case WM_SETFOCUS:
		Focused = 1;
		WaitForPaint = 0;
		SetCursor(LoadCursor(0, IDC_ARROW));
		break;

	case WM_KILLFOCUS:
		Focused = 0;
		DrawCrossHair(32 + 2);
		DrawButtonInfoOff(0);
		DisplayInfoOff(0);
//      SelectionEsc=1;

		AltPressed = 0;
		CtrlPressed = 0;
		ShiftPressed = 0;
		RightButtonPressed = 0;
		LeftButtonPressed = 0;
		MiddleButtonPressed = 0;
		LeftButtonDoublePressed = 0;

//    FocusedAgain=0;
		break;
//    case WM_ACTIVATE:
//      break;
//    case WM_DRAWITEM:
//      hulp3=WParam;
//      DrawInfo=LParam;
		break;

	case WM_TIMER:
		TimerValue++;

		if ((TimerValue % 10) == 0)
		{
			if (!EditingSymbol)
			{
				if (ChangeViewMode)
				{
					if (GlobalFindAtom(AtomStrSchematicSelect) != 0)
					{
						if (ViewMode == 0)
						{
							ViewMode = 1;
							WireSelectMode = 1;
						}
					}
					else
					{
						if (ViewMode == 1)
							ViewMode = 0;
					}
				}
			}

			if ((CheckNewProperties(0) == 1) || (CheckNewUserVars(0) == 1))
				RePaint();
		}

		if (CheckSchematic)
		{
			if (TimerValue > 1)
			{
				CheckSchematic = 0;
				SendMessage(SCHWindow, WM_COMMAND, ID_SCHEMATIC_CHECK, 0);
			}
		}

		return DefWindowProc(Window, Message, WParam, LParam);
		break;

	case WM_CLOSE:
		if (SystemBusyMode == 0)
		{
			if (SaveFile2(0) == 1)
			{
				if (IniFile[0] != 0)
					WriteIniFile(IniFile);
				else
				{
					GetDirFromFileName(str, EditFile);
					le = strlen(str);

					if (le > 6)
					{
						strcpy(str2, (LPSTR) & str[le - 3]);

						if (stricmp(str2, "sch") == 0)
						{
							str[le - 3] = 0;
							strcat(str, "sch.ini");
							WriteIniFile(str);
						}
					}
				}

				DeAllocateMemDesign();
				DeAllocateMem();
				GetWindowRect(SCHWindow, &ScreenRect);

				if (StartFromLibraryManager)
					PostMessage(MasterWindow, WM_COMMAND, (WPARAM) ID_FILE_EXIT_TO_LIB_SCH, (LPARAM) LibrarySymbolNr);

				if (ProjectIndexNr != -1)
				{
					ProjectInfo->FileNames[ProjectIndexNr][0] = 0;
					ProjectInfo->FileInfos[ProjectIndexNr] = 0;
					ProjectInfo->FileTypes[ProjectIndexNr] = 0;
					ProjectInfo->WindowHandles[ProjectIndexNr] = 0;
				}

				DestroyWindow(Window);
			}
		}

		return 0;

	case WM_GETMINMAXINFO:
		return DefWindowProc(Window, Message, WParam, LParam);

	case WM_QUIT:
		ok = 1;
		return 0;

	case WM_DESTROY:
		DeleteGraphicObjects();
		PostQuitMessage(0);
		return 0;

	default:
		return DefWindowProc(Window, Message, WParam, LParam);
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void LoadIniFile(LPSTR FileName, int32 mode)
{
	/*
	mode:

	bit 0 :
	bit 1 : Load [Settings]
	bit 2 : Load [Keys]

	*/
	int32 fp, Length, Value, ok, Key, ParamMode, OldWindowStartX, OldWindowStartY, OldWindowWidth, OldWindowHeight;
	char LineBuf[512], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];
	float Value2;

#ifdef _DEBUG
//  sprintf(str1,"Inifile %s",FileName);
//  MessageBoxUTF8(SCHWindow,str1,SchNamesId[16],MB_OK);
#endif

	if (FileName[0] == 0)
		return;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return;

	OldWindowStartX = WindowStartX;
	OldWindowStartY = WindowStartY;
	OldWindowWidth = WindowWidth;
	OldWindowHeight = WindowHeight;

	ParamMode = 0;
	NrUsedFiles = 0;

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

				if (stricmp(str1, "[Files]") == 0)
				{
					if (mode & 1)
						ParamMode = 5;
				}

				if (stricmp(str1, "[Settings]") == 0)
				{
					if (mode & 1)
						ParamMode = 1;
				}

				if (stricmp(str1, "[Printer]") == 0)
				{
					if (mode & 1)
						ParamMode = 2;
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
				case 5:
					if ((NrUsedFiles < 16) && (str1[0] != 0))
					{
						strncpy(UsedFiles[NrUsedFiles], str1, MAX_LENGTH_STRING - 1);
						NrUsedFiles++;
					}

					break;

				case 1:
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

						if (stricmp(str1, "SymbolDialogStartX") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SymbolDialogInitialX = Value;
						}

						if (stricmp(str1, "SymbolDialogStartY") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SymbolDialogInitialY = Value;
						}

						if (stricmp(str1, "GeometryDialogInitialX") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GeometryDialogInitialX = Value;
						}

						if (stricmp(str1, "GeometryDialogInitialY") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GeometryDialogInitialY = Value;
						}

						if (stricmp(str1, "ComponentDialogInitialX") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								ComponentDialogInitialX = Value;
						}

						if (stricmp(str1, "ComponentDialogInitialY") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								ComponentDialogInitialY = Value;
						}

						if (stricmp(str1, "GeomScreenWidth") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GeomScreenWidth = Value;
						}

						if (stricmp(str1, "GeomScreenHeight") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GeomScreenHeight = Value;
						}

						if (stricmp(str1, "GeomStartX") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GeomStartX = Value;
						}

						if (stricmp(str1, "GeomStartY") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GeomStartY = Value;
						}

						if (stricmp(str1, "RepeatMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								RepeatMode = Value;
						}

						if (stricmp(str1, "WireBusSelectMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WireSelectMode = Value;
						}

						if (stricmp(str1, "SelectMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								ReplaceSelections = Value;
						}

						if (stricmp(str1, "ZoomMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								ZoomMode = Value;
						}

						if (stricmp(str1, "MousePanMultiply") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								MousePanMultiply = Value;
						}

						if (stricmp(str1, "DrawGrid") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GridVisible = Value;
						}

						if (stricmp(str1, "GridSize") == 0)
						{
							if (sscanf(str2, "%f", &Value2) == 1)
							{
								if ((Value2 > (float) 10) || (Value2 < (float) 0.01))
									DrawGridSize = GridSize = (double) 1.0; //out of range
								else
									DrawGridSize = GridSize = (double) Value2;
							}
						}

						if (stricmp(str1, "CrossHairVisible") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								CrossHairVisible = Value;
						}

						if (stricmp(str1, "PopupDisplayVisible") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								PopupDisplayVisible = Value;
						}

						if (stricmp(str1, "ButtonInfoTimeout") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								ButtonInfoTimeout = max(5, min(Value, 200));
						}

						if (stricmp(str1, "AppendPropertiesToNetlabel") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								AppendPropertiesToNetlabel = Value;
						}

						if (stricmp(str1, "AppendPropertiesToReferences") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								AppendPropertiesToReferences = Value;
						}

						if (stricmp(str1, "ButtonInfoTimeoutStart") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								ButtonInfoTimeoutStart = max(5, min(Value, 200));
						}

						if (stricmp(str1, "WireColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[WireColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "BusColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[BusColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "BusConnectionColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[BusConnectionColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "GlobalConnectionColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[GlobalConnectionColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "JunctionColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[JunctionColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "NetLabelColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[NetLabelColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "InstanceRefTextColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[InstanceRefTextColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "InstanceValueTextColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[InstanceValueTextColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolPinColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolPinColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolPinBusColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolPinBusColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolPinTextColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolPinTextColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolPowerPinTextColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolPowerPinTextColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolPinBusTextColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolPinBusTextColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolLineColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolLineColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolRectColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolRectColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolCircleColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolCircleColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolArcColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolArcColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "SymbolTextColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[SymbolTextColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ObjectLineColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[ObjectLineColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ObjectRectColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[ObjectRectColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ObjectCircleColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[ObjectCircleColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ObjectCircleColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[ObjectCircleColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ObjectArcColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[ObjectArcColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ObjectTextColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[ObjectTextColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "GridColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[GridColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "ButtonInfoColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[ButtonInfoColorNr] = (COLORREF) Value;
						}

						if (stricmp(str1, "BackGroundColor") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SCHColors[BackGroundColorNr] = (COLORREF) Value;
						}
					}

					break;

				case 2:
					if (GetStringValue(str4, str1, str2))
					{
						if (stricmp(str1, "PrinterName") == 0)
							strcpy(PrinterName, str2);

						if (stricmp(str1, "Options") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								PrintOptions = Value;
						}
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

	if (!InRangeScreen(SymbolDialogInitialX, SymbolDialogInitialY))
	{
		SymbolDialogInitialX = 0;
		SymbolDialogInitialY = 0;
	}

	if (!InRangeScreen(GeometryDialogInitialX, GeometryDialogInitialY))
	{
		GeometryDialogInitialX = 0;
		GeometryDialogInitialY = 0;
	}

	if (!InRangeScreen(ComponentDialogInitialX, ComponentDialogInitialY))
	{
		ComponentDialogInitialX = 0;
		ComponentDialogInitialY = 0;
	}

	if (!FirstPaintMode)
	{
		ok = 1;
		AdjustWindowSize();
		MoveWindow(SCHWindow, WindowStartX, WindowStartY, WindowWidth, WindowHeight, 1);
		RePaint();
	}

#ifdef _DEBUG
	MousePanMultiply = 2;
#endif
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void WriteIniFile(LPSTR FileName)
{
	int32 fp, cnt, res;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	int32 Stop;

	if (MasterWindow != NULL)
		return;

	if ((fp = FileOpenWriteUTF8(FileName)) < 0)
		return;

	WriteLn(fp, "[Files]");
	WriteLn(fp, "");

	for (cnt = 0; cnt < NrUsedFiles; cnt++)
	{
		sprintf(str1, "\"%s\"", UsedFiles[cnt]);
		WriteLn(fp, str1);
	}

// ******************************************************************************
	WriteLn(fp, "");
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
	sprintf(str1, "SymbolDialogStartX=%i", SymbolDialogInitialX);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolDialogStartY=%i", SymbolDialogInitialY);
	WriteLn(fp, str1);
	sprintf(str1, "GeometryDialogInitialX=%i", GeometryDialogInitialX);
	WriteLn(fp, str1);
	sprintf(str1, "GeometryDialogInitialY=%i", GeometryDialogInitialY);
	WriteLn(fp, str1);
	sprintf(str1, "ComponentDialogInitialX=%i", ComponentDialogInitialX);
	WriteLn(fp, str1);
	sprintf(str1, "ComponentDialogInitialY=%i", ComponentDialogInitialY);
	WriteLn(fp, str1);



	sprintf(str1, "GeomScreenWidth=%i", GeomScreenWidth);
	WriteLn(fp, str1);
	sprintf(str1, "GeomScreenHeight=%i", GeomScreenHeight);
	WriteLn(fp, str1);
	sprintf(str1, "GeomStartX=%i", GeomStartX);
	WriteLn(fp, str1);
	sprintf(str1, "GeomStartY=%i", GeomStartY);
	WriteLn(fp, str1);
	sprintf(str1, "RepeatMode=%i", RepeatMode);
	WriteLn(fp, str1);
	sprintf(str1, "WireBusSelectMode=%i", WireSelectMode);
	WriteLn(fp, str1);
	sprintf(str1, "SelectMode=%i", ReplaceSelections);
	WriteLn(fp, str1);
	sprintf(str1, "ZoomMode=%i", ZoomMode);
	WriteLn(fp, str1);
	sprintf(str1, "MousePanMultiply=%i", MousePanMultiply);
	WriteLn(fp, str1);
	sprintf(str1, "DrawGrid=%i", GridVisible);
	WriteLn(fp, str1);
	sprintf(str1, "GridSize=%.1f", GridSize);
	WriteLn(fp, str1);
	sprintf(str1, "CrossHairVisible=%i", CrossHairVisible);
	WriteLn(fp, str1);
	sprintf(str1, "ButtonInfoTimeout=%d", ButtonInfoTimeout);
	WriteLn(fp, str1);
	sprintf(str1, "ButtonInfoTimeoutStart=%d", ButtonInfoTimeoutStart);
	WriteLn(fp, str1);
	sprintf(str1, "PopupDisplayVisible=%d", PopupDisplayVisible);
	WriteLn(fp, str1);
	sprintf(str1, "AppendPropertiesToNetlabel=%d", AppendPropertiesToNetlabel);
	WriteLn(fp, str1);
	sprintf(str1, "AppendPropertiesToReferences=%d", AppendPropertiesToReferences);
	WriteLn(fp, str1);




	WriteLn(fp, "");
// ******************************************************************************

	sprintf(str1, "WireColor=%i", (int32) SCHColors[WireColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "BusColor=%i", (int32) SCHColors[BusColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "BusConnectionColor=%i", (int32) SCHColors[BusConnectionColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "GlobalConnectionColor=%i", (int32) SCHColors[GlobalConnectionColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "JunctionColor=%i", (int32) SCHColors[JunctionColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "NetLabelColor=%i", (int32) SCHColors[NetLabelColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "InstanceRefTextColor=%i", (int32) SCHColors[InstanceRefTextColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "InstanceValueTextColor=%i", (int32) SCHColors[InstanceValueTextColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolPinColor=%i", (int32) SCHColors[SymbolPinColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolPinBusColor=%i", (int32) SCHColors[SymbolPinBusColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolPinTextColor=%i", (int32) SCHColors[SymbolPinTextColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolPowerPinTextColor=%i", (int32) SCHColors[SymbolPowerPinTextColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolPinBusTextColor=%i", (int32) SCHColors[SymbolPinBusTextColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolLineColor=%i", (int32) SCHColors[SymbolLineColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolRectColor=%i", (int32) SCHColors[SymbolRectColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolTextColor=%i", (int32) SCHColors[SymbolTextColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolArcColor=%i", (int32) SCHColors[SymbolArcColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "SymbolCircleColor=%i", (int32) SCHColors[SymbolCircleColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ObjectLineColor=%i", (int32) SCHColors[ObjectLineColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ObjectRectColor=%i", (int32) SCHColors[ObjectRectColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ObjectCircleColor=%i", (int32) SCHColors[ObjectCircleColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ObjectArcColor=%i", (int32) SCHColors[ObjectArcColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ObjectTextColor=%i", (int32) SCHColors[ObjectTextColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "ButtonInfoColor=%i", (int32) SCHColors[ButtonInfoColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "GridColor=%i", (int32) SCHColors[GridColorNr]);
	WriteLn(fp, str1);
	sprintf(str1, "BackGroundColor=%i", (int32) SCHColors[BackGroundColorNr]);
	WriteLn(fp, str1);
	WriteLn(fp, "");
	WriteLn(fp, "");
	WriteLn(fp, "[Printer]");
	WriteLn(fp, "");

	if (PrinterName[0] != 0)
		sprintf(str1, "PrinterName=\"%s\"", PrinterName);
	else
		sprintf(str1, "PrinterName=");

	WriteLn(fp, str1);
	sprintf(str1, "Options=%d", PrintOptions);
	WriteLn(fp, str1);
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
// ********************************************************************************************************

int32 AddSchLanguageString(int32 ID, LPSTR Text)
{
	int32 Length;
	WCHAR TextW[MAX_LENGTH_STRING];

	Length = strlen(Text);

	if (Length == 0)
		return -1;

	if ((ID < 0) || (ID >= MaxNrSchNames))
		return -1;

	if (SchNamesPos + Length + 2 >= SchNamesBufSize)
		return -1;

	if (!Utf8ToUnicode(Text, TextW, MAX_LENGTH_STRING))
		return -1;

	SchNamesId[ID] = (LPSTR) & SchNamesBuf[SchNamesPos];
	memmove(&SchNamesBuf[SchNamesPos], Text, Length + 1);
	SchNamesPos += Length + 1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddSchLanguageStrings(LPSTR FileName)
{

	int32 fp, cnt2, str3length, Length, count, StringNr;
	char LineBuf[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING],
	     LanguageFileName[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING];

	count = 0;

	if (FileName[0] == 0)
		return -1;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
	{
		/*
		    sprintf(str,"Can not read from language file %s, default language used",FileName);
		    MessageBoxUTF8(NULL,str,SC(288,"Warning"),MB_OK);
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
				sprintf(LanguageFileName, "%s\\%s", ExePath, str);
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

				/*
				        cnt2=0;
				        while (cnt2<str3length) {
				          if (str3[cnt2]=='\\') {
				            if (cnt2+1<str3length) {
				              if ((str3[cnt2+1]!='t')
				                 &&
				                 (str3[cnt2+1]!='n')
				                 &&
				                 (str3[cnt2+1]!='r')) {
				                ok=1;
				                str3[cnt2]=' ';
				              }
				            }
				          }
				          cnt2++;
				        }
				*/
				AddSchLanguageString(StringNr, str3);
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
	int32 cnt, lengte, pos;
	float x, y;

	DesignPath[0] = 0;
	DesignFile[0] = 0;
	ProjectPath[0] = 0;

	pos=0;
	lengte=1000;

	for (cnt = 0; cnt < NrParams; cnt++)
	{
		if (Parameters[cnt].Option[0] != 0)
		{
			if (strnicmp(Parameters[cnt].Option, "b", lengte) == 0)
			{
				DirectPrinting = 2;
				strcpy(PrintingParameters, (LPSTR) &Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "c", lengte) == 0)
			{
				CheckSchematic = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "d", lengte) == 0)
			{
				ActiveOnePinNetError = 0;
			}

			if (strnicmp(Parameters[cnt].Option, "e", lengte) == 0)
			{
				strcpy(ExePath, (LPSTR)&Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "g", lengte) == 0)
			{
				EditingProtectedSymbol = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "n", lengte) == 0)
			{
				DirectPrinting = 1;
				strcpy(PrintingParameters, (LPSTR) &Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "o", lengte) == 0)
			{
				ProjectActive = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "p", lengte) == 0)
			{
				strcpy(DesignPath, (LPSTR)&Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "r", lengte) == 0)
			{
				SearchReference = 1;
				memset(&SearchCodeString, 0, sizeof(SearchCodeString));
				strcpy(SearchCodeString, (LPSTR) &Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "r2", lengte) == 0)
			{
				SearchPartnr = 1;
				memset(&SearchCodeString, 0, sizeof(SearchCodeString));
				strcpy(SearchCodeString, (LPSTR) &Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "t0", lengte) == 0)
			{
				EditMode = 0;	// schematic
			}

			if (strnicmp(Parameters[cnt].Option, "t1", lengte) == 0)
			{
				EditMode = 1;	// symbol
			}

			if (strnicmp(Parameters[cnt].Option, "t2", lengte) == 0)
			{
				EditMode = 2;	// sheet symbol
			}

			if (strnicmp(Parameters[cnt].Option, "u", lengte) == 0)
			{
				strcpy(ProjectPath, (LPSTR)&Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "v", lengte) == 0)
			{
				ViewMode = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "w", lengte) == 0)
			{
//				sscanf((LPSTR) &Parameters[cnt].Parameter[pos], "%x", &MasterWindow); //pùvodní

				scanf_s((LPSTR) &Parameters[cnt].Parameter[pos], "%x", &MasterWindow); //opravený
			}

			if (strnicmp(Parameters[cnt].Option, "x", lengte) == 0)
			{
				strcpy(DesignFile, (LPSTR) &Parameters[cnt].Parameter[pos]);
			}

			if (strnicmp(Parameters[cnt].Option, "y", lengte) == 0)
			{
				if (sscanf((LPSTR) &Parameters[cnt].Parameter[pos], "%f,%f", &x, &y) == 2)
				{
					if ((x > -1000.0) && (x < 1000.0) && (y > -1000.0) && (y < 1000.0))
					{
						GotoXY_X = x;
						GotoXY_Y = y;
					}
				}
			}

			if (strnicmp(Parameters[cnt].Option, "z", lengte) == 0)
			{
				StartFromLibraryManager = 1;
				sscanf((LPSTR) &Parameters[cnt].Parameter[pos], "%d", &LibrarySymbolNr);
			}
		}
		else
		{
			if ((mode == 0) && (cnt > 0) && (Parameters[cnt].Parameter[0] != 0))
			{
				if (ConvertFileNameFromCommandLineUTF8(Parameters[cnt].Parameter, EditFile) == -1)
				{
					MessageBoxUTF8(SCHWindow, Parameters[cnt].Parameter, SC(197, "Error in opening file"),
					               MB_APPLMODAL | MB_OK);
				}
			}
		}
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmd, int nCmdShow)
{
	MSG M;
	int32 ok, count, res, x, y, PdfObjectStart;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	uint32 EAX, EBX, ECX, EDX, FlagSSE2 = 0;
	char vendor[40];

	/*
	  OutputDebugStr("\r\n");
	  OutputDebugStr(lpszCmd);
	  OutputDebugStr("\r\n");
	  OutputDebugStr("\r\n");
	*/
	/*

	c:\pcb_elegance\cdio\sch\top.sch /ec:\pcb_elegance /a /o
	getwindowlong
	setwindowlong
	*/
//  MessageBoxW(NULL,GetCommandLineW(),L"Commandline",MB_APPLMODAL+MB_OK);

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
		MessageBox(NULL, "This version of the layout editor requires a CPU with SSE2 extensions", "Error",
		           MB_APPLMODAL | MB_OK);
		return 0;
	}

	InitTimers();
	MasterWindow = 0;
	GetParameters(NULL);
	GetDirFromFileName(ExecutableDir, Parameters[0].Parameter);
	DecodeParameters(0);		// EditFile DesignFile

	if (ExePath[0] == 0)
		strcpy(ExePath, ExecutableDir);

	strcpy(SchExecutable, Parameters[0].Parameter);
	/*
	  sprintf(str,"%s.prm",SchExecutable);
	  AppendStringToTextFile(str,lpszCmd);
	*/
	GetCurrentDirectoryUTF8(MAX_LENGTH_STRING - 50, StartDir);

	if (ProjectPath[0] != 0)
	{
		if (DesignPath[0] == 0)
			strcpy(DesignPath, ProjectPath);
	}
	else
	{
		if (DesignPath[0] == 0)
			strcpy(DesignPath, StartDir);

		strcpy(ProjectPath, ExePath);
	}

//  MessageBox(NULL,DesignPath,"DesignPath",MB_APPLMODAL+MB_OK);

	if (hPrevInstance == 0)
	{
		SCHClass.style = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
		SCHClass.lpfnWndProc = (WNDPROC) SCHWinProc;
		SCHClass.cbClsExtra = 0;
		SCHClass.cbWndExtra = 0;
		SCHClass.hInstance = hInstance;
//    SCHClass.hIcon = LoadIcon(hInstance,MAKEINTRESOURCE(ICON_SYMBOL));
		SCHClass.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(ICON_SHEET));
		SCHClass.hCursor = LoadCursor(0, IDC_ARROW);
		SCHClass.hbrBackground = 0;
//    SCHClass.hbrBackground = GetStockObject(WHITE_BRUSH);
//    SCHClass.hbrBackground = GetStockObject(BLACK_BRUSH);
		SCHClass.lpszClassName = SchClassName;

		if (!RegisterClass(&SCHClass))
			exit(255);
	}

//  res=DaysAfter1900FromFileName("c:\\test.tst");
//  res=GetFilePartFromFileName(str,"c:\\ab\\");

	MemoryMain();
	EditingSymbol = 0;
	LeftButtonPressed = 0;
	MiddleButtonPressed = 0;
	RightButtonPressed = 0;
	LeftButtonDoublePressed = 0;
	ClosingWindowMessage = RegisterWindowMessage("CLOSING_WINDOW");
	IniFile[0] = 0;
	GeomScreenWidth = 0;
	sprintf(str, "%s\\LanguageSch.txt", ExePath);

	if (AddSchLanguageStrings(str) == -2)
		return -1;

	LoadDesignIniFile();

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

	sprintf(GeometrieEditor, "%s\\geom.exe", ExePath);
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

	ToetsMain();
	OutputDisplay = (HDC) - 1;
	SetCursor(LoadCursor(0, IDC_ARROW));
	CurrentCursor = GetCursor();

	InitDialogs();

	ClipCursor(NULL);


	AllocateMem();
	FontResourceHandle = FindResource(NULL, MAKEINTRESOURCE(IDR_OWNFONT1), "OWNFONT");
	count = SizeofResource(NULL, FontResourceHandle);
	FontGlobal = LoadResource(NULL, FontResourceHandle);
	FontData = LockResource(FontGlobal);
	memmove(Chars, FontData, count);


	ok = GetSystemMetrics(SM_CMOUSEBUTTONS);
	memset(&ClientRect, 0, sizeof(RECT));



	AllScreenX1 = GetSystemMetrics(SM_XVIRTUALSCREEN);
	AllScreenY1 = GetSystemMetrics(SM_YVIRTUALSCREEN);
	AllScreenX2 = GetSystemMetrics(SM_CXVIRTUALSCREEN) + AllScreenX1;
	AllScreenY2 = GetSystemMetrics(SM_CYVIRTUALSCREEN) + AllScreenY1;

	ScreenSizeX = GetSystemMetrics(SM_CXMAXIMIZED) - 10;
	ScreenSizeY = GetSystemMetrics(SM_CYMAXIMIZED) - 10;
	ScreenStartX = 0;
	ScreenStartY = 0;

	if (DirectPrinting)
	{
		res = ChangeFile(0, 4 + 2);

		if (res == 0)
		{
			if (DirectPrinting == 1)
			{
				GetCommaString(PrintingParameters, str);

				if (strcmp(str, "$PDF$"))
				{
					GetCommaString(PrintingParameters, str2);
					sscanf(str2, "%d", &DirectPrintingPaperSize);

					if (ProjectInfo != NULL)
						ProjectInfo->PrintingBusy = 1;

					Print(DirectPrintingPaperSize, 0, str, 4);

					if (ProjectInfo != NULL)
					{
						Sleep(200);
						ProjectInfo->PrintingBusy = 0;
					}
				}
				else
				{
					GetCommaString(PrintingParameters, str2);
					sscanf(str2, "%d", &DirectPrintingPaperSize);
					GetCommaString(PrintingParameters, str2);
					sscanf(str2, "%d", &DirectPrintingOrientation);
					GetCommaString(PrintingParameters, str2);
					sscanf(str2, "%d", &DirectPrintingFitToPage);
					GetCommaString(PrintingParameters, str2);
					sscanf(str2, "%d", &PdfObjectStart);

					if (ProjectInfo != NULL)
						ProjectInfo->PrintingBusy = 1;

					ExportToPDF(DirectPrintingPaperSize, DirectPrintingOrientation, DirectPrintingFitToPage,
					            PdfObjectStart, 1);

					if (ProjectInfo != NULL)
					{
						Sleep(200);
						ProjectInfo->PrintingBusy = 0;
					}
				}
			}
			else
			{
				GetCommaString(PrintingParameters, str);
				GetCommaString(PrintingParameters, str2);
				x = 0;
				y = 0;

				if ((str[0] != 0) && (str2[0] != 0))
				{
					x = atoi(str);
					y = atoi(str2);
				}

				if (x == 0)
					x = 1024;

				if (y == 0)
					x = 768;

				ExportToBitmap2(1, x, y);
			}
		}

		return 0;
	}

	res = ChangeFile(0, 2);
	AdjustWindowSize();
//  MessageBox(NULL,DesignPath,"DesignPath",MB_APPLMODAL+MB_OK);

	CheckExpandedCtrlKeys(0);
	MakeDefaultMenu();
	MakeSheetMenu();
	MakeSymbolMenu();


	ClipBoardSchematicID = RegisterClipboardFormat("Schematic file");
	ClipBoardSelectionsSchematicID = RegisterClipboardFormat("Selections Schematic file");
	HeightScrollBar = GetSystemMetrics(SM_CYHSCROLL), WidthScrollBar = GetSystemMetrics(SM_CXVSCROLL);

	SCHWindow =
	    CreateWindowEx(WS_EX_ACCEPTFILES, SchClassName, NULL, WS_OVERLAPPEDWINDOW, WindowStartX, WindowStartY,
	                   WindowWidth, WindowHeight, HWND_DESKTOP, 0, hInstance, NULL);
//  MakeMenu((MenuRecord *)&NewMenu);
	TimerObject = SetTimer(SCHWindow, TimerIdentifier, NR_MILLI_SECONDS_TIMER, NULL);
//  ShowWindow(SCHWindow, nCmdShow);

//  SetSymbolMenu();
	if (ProjectActive)
		InsertWindowInProject(SCHWindow, 0);

	hwndHBar =
	    CreateWindow("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_HORZ, 0, 0, 0, 0, SCHWindow, (HMENU) 1, hInstance,
	                 NULL);
	hwndVBar =
	    CreateWindow("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_VERT, 0, 0, 0, 0, SCHWindow, (HMENU) 2, hInstance,
	                 NULL);

	MaxScrollBarX = 10000;
	MaxScrollBarY = 10000;
	SetScrollRange(hwndHBar, SB_CTL, 0, MaxScrollBarX, 1);
	SetScrollRange(hwndVBar, SB_CTL, 0, MaxScrollBarY, 1);

	ShowWindow(SCHWindow, SW_SHOWNORMAL);
	UpdateWindow(SCHWindow);

	SetWindowName(0);

	while ((!TotalExit) && (GetMessage(&M, 0, 0, 0)))
	{
		TranslateMessage(&M);
		DispatchMessage(&M);

		if (Focused)
			MainLoop();
	}

	KillTimer(SCHWindow, TimerIdentifier);

	DeAllocateMem();
	KillMenus();
	ClipCursor(NULL);

	if (ProjectInfo != NULL)
		ProjectInfo->PrintingBusy = 0;

	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
