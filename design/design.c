/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: design.c
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
#include "design.h"
#include "string.h"
#include "stdlib.h"
#include "calc.h"
#include "direct.h"
#include "menus.h"
#include "line2.h"
#include "files.h"
#include "files2.h"
#include "check.h"
#include "help.h"
#include "ellipss.h"
#include "mainloop.h"
#include "resource.h"
#include "command.h"
#include "utf8.h"
#include "params.h"
#include "owntime.h"
#include "own_process.h"

#define WANT_GETLONGPATHNAME_WRAPPER
#define COMPILE_NEWAPIS_STUBS

//#define TEST

//#define  MAX_PARAMETERS                 20

//#include "newapis.h"

//#define  CRC_MESSAGE

#define MEMORYMAPPEDSTRING                      "MMFILE_PCB_ELEGANCE"
#define DefSharedMemoryLength                   16384


typedef struct
{
	WORD palVersion;
	WORD palNumEntries;
	COLORREF Colors[32];
} DESIGNPaletteRecord;


HCURSOR SystemCursor;
HPALETTE OldPalette;
HCURSOR OldCursor, Cursor1;
HWND DialogWindow, SCHFrameWindow;
HDC DialogDisplay;

PAINTSTRUCT PS;
RECT RealWindow, ClientRect, UpdateRect;
HRGN EditingRegion;

int32 SCHCursorActive;
int32 TotalExit;
int32 FocusedAgain = 1;
int32 EditWindowFocused;
int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY, MessageHeight, TimerValue, WindowStartX,
      WindowStartY, ScreenStartX, ScreenStartY, ScreenSizeX, ScreenSizeY;
int32 MessageStartY;
int32 FirstDraw = 1;
int32 NrSizes;
int32 NrPaintMessages;
UINT TimerObject, ClipID3;
UINT TimerIdentifier = 0x12345678;
UINT StartUpMessage;
DESIGNPaletteRecord DESIGNPaletteObject;
HPALETTE DESIGNPalette, CurrentPalette;
WIN32_FIND_DATA FindData;

//ParameterRecord             Parameters[MAX_PARAMETERS];
//int32                       NrParams;
HCURSOR CurrentCursor;

char ExecutableDir[MAX_LENGTH_STRING];
char TempFile[MAX_LENGTH_STRING];
char StartUpStr[MAX_LENGTH_STRING];
char DesignExecutable[MAX_LENGTH_STRING];
char ProjectPath[MAX_LENGTH_STRING];
char MessageStr[MAX_LENGTH_STRING];

HFONT MessagesFont;
DEVMODE ScreenInfo;
ProjectInfoRecord *ProjectInfo;

HBITMAP BackgroundBitmap, BackAnnotateButtonBitmap, SymbolButtonBitmap, SchematicButtonBitmap, AnnotateButtonBitmap,
        NetlistButtonBitmap, LayoutButtonBitmap, GeometryButtonBitmap, GerberButtonBitmap, BOMButtonBitmap, BitmapDown,
        BitmapUp;

HFONT NewFont;
uint8 *SharedMemory;
HANDLE *SharedMemoryHandle;
int32 OkToUseSharedMemory;


LRESULT FAR PASCAL DESIGNWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam);
LRESULT FAR PASCAL EditWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam);

WNDCLASS DESIGNClass, EditClass;
char DesignClassName[] = "DESIGN";
//char                        DesignStr[]="c:\\sch\\design2.bin";

extern HFONT SaveFont;
extern int32 FirstPaint;

// ********************************************************************************************************
// ********************************************************************************************************

int32 AddDesignLanguageStrings(LPSTR FileName);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SaveOpenFiles(int32 mode)
{
	int32 cnt;

	/*
	typedef struct {
	          int32 OtherInfos[32];
	          int32 FileTypes[32],FileInfos[32];
	          HWND  WindowHandles[32];
	          char  FileNames[32][80];
	        } ProjectInfoRecord ;
	*/
	for (cnt = 0; cnt < 32; cnt++)
	{
		if (ProjectInfo->FileTypes[cnt] != 0)
		{

// FileTypes
// 1 -> sheet,symbol
// 3 -> geometry
// 4 -> layout

			if (ProjectInfo->WindowHandles[cnt] != 0)
			{
				if ((mode & (1 << ProjectInfo->FileTypes[cnt])) != 0)
				{
					if (ProjectInfo->FileTypes[cnt] == 1)
						SendMessage(ProjectInfo->WindowHandles[cnt], WM_COMMAND, ID_GLOBAL_FILE_SAVE2, 0);
					else
						SendMessage(ProjectInfo->WindowHandles[cnt], WM_COMMAND, ID_GLOBAL_FILE_SAVE, 0);
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetWindowName(LPSTR DesignName, int32 mode)
{
	char str[MAX_LENGTH_STRING];

#ifdef TEST

	if (DesignName)
		sprintf(str, "TEST TEST TEST    Design %s     TEST TEST TEST", DesignName);
	else
		sprintf(str, "TEST TEST TEST    Design manager    TEST TEST TEST");

#else

	if (DesignName)
		sprintf(str, SC(10, "Design %s"), DesignName);
	else
		sprintf(str, SC(63, "Design manager PCB Elegance"));

#endif

	if (DESIGNWindow != NULL)
		SetWindowTextUTF8(DESIGNWindow, str);
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ReloadSchematics(int32 mode)
{
	int32 cnt, cnt2;
	char OpenSchematics[32][MAX_LENGTH_STRING], ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;
	STARTUPINFO StartupInfo;
	/*
	typedef struct {
	          int32 OtherInfos[32];
	          int32 FileTypes[32],FileInfos[32];
	          HWND  WindowHandles[32];
	          char  FileNames[32][80];
	        } ProjectInfoRecord ;
	*/

	cnt2 = 0;

	for (cnt = 0; cnt < 32; cnt++)
	{
		if (ProjectInfo->FileTypes[cnt] == 1)
		{

// FileTypes
// 1 -> sheet,symbol
// 3 -> geometry
// 4 -> layout

			if (ProjectInfo->WindowHandles[cnt] != 0)
			{
				strcpy(OpenSchematics[cnt2++], ProjectInfo->FileNames[cnt]);
				SendMessage(ProjectInfo->WindowHandles[cnt], WM_CLOSE, 0, 0);
			}
		}
	}

	for (cnt = 0; cnt < cnt2; cnt++)
	{
		sprintf(ExeFile, "%s\\sch.exe", ExePath);
		sprintf(ExeParams, "\"%s\" \"%s\" /a /o /e \"%s\"", ExeFile, OpenSchematics[cnt], ExePath);
		memset(&StartupInfo, 0, sizeof(StartupInfo));
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
	}
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
		WindowWidth = 798;

		if (ScreenSizeY > 610)
			MessageHeight = 262;
		else
			MessageHeight = 145;

		MessageStartY = 360;
		WindowHeight = MessageHeight + MessageStartY + 50;
		WindowStartX = (ScreenSizeX - WindowWidth) / 2;
		WindowStartY = (ScreenSizeY - WindowHeight) / 2;
		return;
	}
	else
	{
		if (WindowWidth < 812)
			WindowWidth = 812;
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
		MessageStartY = 360;
		WindowWidth = 700;

		if (ScreenSizeY > 610)
			MessageHeight = 262;
		else
			MessageHeight = 145;

		MessageStartY = 360;
		WindowHeight = MessageHeight + MessageStartY + 50;
		WindowStartX = (ScreenSizeX - WindowWidth) / 2;
		WindowStartY = (ScreenSizeY - WindowHeight) / 2;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

HWND GetProjectWindow(LPSTR ActiveFile, int32 FileType, int32 mode)
{
	int32 cnt;

	if (ProjectInfo == NULL)
		return NULL;

	cnt = 0;

	while ((cnt < 32)
	        && ((ProjectInfo->FileTypes[cnt] != FileType)
	            || (stricmpUTF8(ProjectInfo->FileNames[cnt], ActiveFile) != 0)))
		cnt++;

	if (cnt == 32)
		return NULL;

	return ProjectInfo->WindowHandles[cnt];
}

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

	ProjectInfo->FileTypes[cnt] = 10;
	ProjectInfo->WindowHandles[cnt] = Window;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ActivateProjectWindow(HANDLE WindowHandle)
{
	WINDOWPLACEMENT WindowInfo;

	if (!WindowHandle)
		return -1;

	GetWindowPlacement(WindowHandle, &WindowInfo);
	/*
	  if (WindowInfo.showCmd==SW_MINIMIZE) {
	    ShowWindow(WindowHandle,SW_RESTORE);
	  } else {
	    ShowWindow(WindowHandle,SW_SHOW);
	  }
	*/
	SetForegroundWindow(WindowHandle);
//  SetForegroundWindow((HWND)(((ULONG)WindowHandle) | 0x01) );

//  SetActiveWindow(WindowHandle);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CloseOpenFiles(int32 mode)
{
	int32 cnt;

	/*
	typedef struct {
	          int32 OtherInfos[32];
	          int32 FileTypes[32],FileInfos[32];
	          HWND  WindowHandles[32];
	          char  FileNames[32][80];
	        } ProjectInfoRecord ;
	*/
	for (cnt = 0; cnt < 32; cnt++)
	{
		if (ProjectInfo->FileTypes[cnt] != 0)
		{
			if (ProjectInfo->WindowHandles[cnt] != 0)
				SendMessage(ProjectInfo->WindowHandles[cnt], WM_CLOSE, 0, 0);
		}
	}
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

void WindowCreate()
{
	int32 res;

	StartUpMessage = RegisterWindowMessage("Startup");
	res = PostMessage(NULL, StartUpMessage, (WPARAM) NULL, (LPARAM) NULL);

	Created = 1;
	SCHCursorActive = 0;
	SystemCursor = LoadCursor(NULL, IDC_ARROW);
	InsertWindowInProject(DESIGNWindow, 0);
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

void InitDeviceContext()
{
	if (FirstDraw == 1)
		FirstDraw = 0;

	if (!DCInUse)
	{
		DCInUse = 1;

		if (Painting)
			OutputDisplay = BeginPaint(DESIGNWindow, &PS);
		else
			OutputDisplay = GetDC(DESIGNWindow);

		if (!Printing)
			SetBkMode(OutputDisplay, TRANSPARENT);

		SetBkColor(OutputDisplay, RGB(0, 0, 0));
		SetPolyFillMode(OutputDisplay, WINDING);
//    SaveFont = SelectObject(WindowsDisplay, UserFont);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void StartDrawingEditingWindow()
{
	InitDeviceContext();

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DoneDeviceContext()
{
	if (DCInUse)
	{
//    SelectObject(WindowsDisplay, SaveFont);
		if (Painting)
			EndPaint(DESIGNWindow, &PS);
		else
			ReleaseDC(DESIGNWindow, OutputDisplay);

		OutputDisplay = (HDC) - 1;
		DCInUse = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void EndDrawingEditingWindow()
{
	DoneDeviceContext();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void RedrawButtons()
{
	RECT Rect;

	if (DrawWindowMinY > 1)
	{
		Rect.left = DrawWindowMinX;
		Rect.right = DrawWindowMaxX;
		Rect.top = 0;
		Rect.bottom = DrawWindowMinY;
		FillRect(OutputDisplay, &Rect, GetStockObject(BLACK_BRUSH));
		Rect.left++;
		Rect.top++;
		Rect.right--;
		Rect.bottom--;
		FillRect(OutputDisplay, &Rect, GetStockObject(GRAY_BRUSH));

//    ValidateRect(DESIGNWindow,&Rect);
	}

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawIntro(int32 mode)
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
	IntroBitmap2 = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_INTRO35A));
	IntroBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_INTRO35));
	x = (ScreenSizeX) / 2 - IntroBitmapSizeX / 2;
	y = (ScreenSizeY) / 2 - IntroBitmapSizeY / 2;
	x2 = x + (IntroBitmapSizeX - IntroBitmapSizeX2) / 2;
	y2 = y + (IntroBitmapSizeY - IntroBitmapSizeY2) / 2;

	if (mode == 1)
	{
		IntroMemoryDC = CreateCompatibleDC(DesktopWindow);
		old = SelectObject(IntroMemoryDC, IntroBitmap);
		res = BitBlt(DesktopWindow, x, y, IntroBitmapSizeX, IntroBitmapSizeY, IntroMemoryDC, 0, 0, SRCCOPY);
		SelectObject(IntroMemoryDC, old);
		DeleteDC(IntroMemoryDC);
	}
	else
	{
		if ((WindowStartX > x) || (WindowStartY > y) || (x + IntroBitmapSizeX > WindowStartX + WindowWidth)
		        || (y + IntroBitmapSizeY > WindowStartY + WindowHeight))
			InvalidateRect(NULL, NULL, 0);
	}

	DeleteObject(IntroBitmap);
	DeleteObject(IntroBitmap2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WindowPaint()
{
	RECT Rect;

	GetClientRect(DESIGNWindow, &Rect);

	if (GetUpdateRect(DESIGNWindow, &UpdateRect, 0))
	{
		Painting = 1;
		InitDeviceContext();
		RedrawMainWindow();
		DoneDeviceContext();
		Painting = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LRESULT FAR PASCAL EditWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 ok;

	switch (Message)
	{
	case WM_SIZE:
		if (NrSizes > 1)
			ok = 1;

		return DefWindowProc(Window, Message, WParam, LParam);

	default:
		return DefWindowProc(Window, Message, WParam, LParam);
	}

//  return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LRESULT FAR PASCAL DESIGNWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam)
{
	POINT pp;

	RECT Rect;
	int32 ok;
	LRESULT Result;
	CREATESTRUCT *CreateInfo;

#ifdef _DEBUG

//  CrtWindow = Window;
	if ((Message >= 0x201) && (Message <= 0x209))
		ok = 1;

#endif

	switch (Message)
	{
	case WM_HELP:
		break;

	case WM_COMMAND:
		DESIGNCommand(Window, WParam, LParam);
		ok = 1;
		break;

	case WM_CREATE:
		CreateInfo = (CREATESTRUCT *) LParam;
		FirstSize = 1;
		WindowCreate();
		break;

	case WM_PAINT:
		if (FirstPaint)
			FirstPaint = 0;

		WindowPaint();
		break;

	case WM_SIZE:
		if (DESIGNWindow == NULL)
			MessageBoxUTF8(NULL, "DESIGNWindow = 0 ", "Error", MB_APPLMODAL | MB_OK);

		if (NrSizes > 0)
		{
			Rect.left = DrawWindowMaxX - 2;
			Rect.right = DrawWindowMaxX;
			Rect.top = DrawWindowMinY;
			Rect.bottom = DrawWindowMaxY;
			InvalidateRect(DESIGNWindow, &Rect, 0);
			Rect.left = DrawWindowMinX;
			Rect.right = DrawWindowMaxX;
			Rect.top = DrawWindowMaxY - 3;
			Rect.bottom = DrawWindowMaxY;
			InvalidateRect(DESIGNWindow, &Rect, 0);
			PostMessage(DESIGNWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
		}

		GetClientRect(DESIGNWindow, &ClientRect);
		NrSizes++;
		DrawWindowMinX = 0;
		DrawWindowMinY = 0;
		DrawWindowMaxX = ClientRect.right;
		DrawWindowMaxY = ClientRect.bottom;

		ClientWindowDivX = DrawWindowMaxX - DrawWindowMinX;
		ClientWindowDivY = DrawWindowMaxY - DrawWindowMinY;

		if (NrSizes > 1)
			PostMessage(EditWindow, WM_SIZE, SIZE_RESTORED, DrawWindowMaxX + ((DrawWindowMaxY - MessageStartY) << 16));

		GetWindowRect(DESIGNWindow, &RealWindow);
		WindowStartX = RealWindow.left;
		WindowStartY = RealWindow.top;
		WindowWidth = RealWindow.right - RealWindow.left;
		WindowHeight = RealWindow.bottom - RealWindow.top;

		MessageHeight = WindowHeight - 412;

		if (EditWindow)
		{
			MoveWindow(EditWindow, 1, MessageStartY, WindowWidth - 11, MessageHeight + 2, 0);
			RedrawWindow(EditWindow, NULL, NULL, RDW_INVALIDATE | RDW_ERASENOW | RDW_ALLCHILDREN);
		}

		pp.x = 0;
		pp.y = 0;
		ClientToScreen(DESIGNWindow, &pp);
		ClientStartX = pp.x;
		ClientStartY = pp.y;


//      ViewMinX=PixelToRealOffX(-1);
//      ViewMaxX=PixelToRealOffX(DrawWindowMaxX+1);
//      ViewMinY=PixelToRealOffY(-1);
//      ViewMaxY=PixelToRealOffY(DrawWindowMaxY+1-DrawWindowMinY);
		break;

	case WM_MOVE:
		GetWindowRect(DESIGNWindow, &RealWindow);
		WindowStartX = RealWindow.left;
		WindowStartY = RealWindow.top;
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
			MouseChanged = 1;
		}

		if (GetCursor() != CurrentCursor)
			SetCursor(CurrentCursor);

		break;

	case WM_LBUTTONDBLCLK:
		if ((Focused) && (FocusedAgain))
		{
			ok = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			LeftButtonDoublePressed = 1;
		}

		break;

	case WM_LBUTTONDOWN:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			LeftButtonPressed = 1;
		}

		break;

	case WM_LBUTTONUP:
		if (EditWindowFocused)
		{
			FocusedAgain = 1;
//        Focused=1;
			SetFocus(DESIGNWindow);
//        SetActiveWindow(DESIGNWindow);
//        SetForegroundWindow(DESIGNWindow);
		}
		else
		{
			if ((Focused) && (FocusedAgain))
			{
				MouseChanged = 1;
				FocusedAgain = 1;
				MousePosX = LOWORD(LParam);
				MousePosY = HIWORD(LParam) + 1;
				LeftButtonPressed = 0;
				//        return DefWindowProc(Window, Message, WParam, LParam);
			}
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

	/*
	    case WM_MOUSEACTIVATE:
	      Focused=1;
	      FocusedAgain=1;
	      ok=1;
	      return 1;
	      break;
	    case WM_ACTIVATE:
	      if (LOWORD(WParam)==WA_INACTIVE) {
	        DrawButtonInfoOff(0);
	        if ( AltPressed ) AltPressed=0;
	        if ( CtrlPressed ) CtrlPressed=0;
	        if ( ShiftPressed ) ShiftPressed=0;
	        Focused=0;
	        MousePosX=10000;
	        MousePosY=10000;
	      }
	      break;
	*/
	case WM_SETFOCUS:
		EditWindowFocused = 0;
		Focused = 1;
		FocusedAgain = 1;
		break;

	case WM_KILLFOCUS:
		DrawButtonInfoOff(0);
		RightButtonPressed = 0;
		LeftButtonPressed = 0;
		FocusedAgain = 0;
		Focused = 0;

		if ((HWND) WParam != EditWindow)
			EditWindowFocused = 0;
		else
			EditWindowFocused = 1;

		break;

	case WM_TIMER:
		TimerValue++;
		break;

	case WM_GETMINMAXINFO:
		return DefWindowProc(Window, Message, WParam, LParam);

	case WM_CLOSE:
		CloseOpenFiles(0);
		SaveUserIniFile(0);
		DestroyWindow(Window);
		return 0;

	case WM_QUIT:
		ok = 1;
		return 0;

	case WM_DESTROY:
		WindowDestroy();
		return 0;

	default:
		if (Message == StartUpMessage)
		{
//        ChangeFile();
			break;
		}
		else
			return DefWindowProc(Window, Message, WParam, LParam);
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CreateUserIniFile()
{
	char FileName[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING];
	char FileName3[MAX_LENGTH_STRING], LineBuf[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];
	int fp, fp2;
	int32 Length;
	int32 FoundFirstLine = 0;

	if (ExePath[strlen(ExePath) - 1] != '\\')
	{
		sprintf(FileName, "%s\\start.ini", ExePath);
		sprintf(FileName2, "%s\\pcb_lic_limited25.txt", ExePath);
		sprintf(FileName3, "%s\\pcb_eleg_size.txt", ExePath);
	}
	else
	{
		sprintf(FileName, "%sstart.ini", ExePath);
		sprintf(FileName2, "%spcb_lic_limited25.txt", ExePath);
		sprintf(FileName3, "%spcb_eleg_size.txt", ExePath);
	}

	if (FileName[0] == 0)
		return;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return;

	fp2 = 0;

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
	{
		LineBuf[Length] = 0;
		strcpy(str4, LineBuf);

		if ((Length > 1) || (FoundFirstLine))
		{
			FoundFirstLine = 1;

			switch (LineBuf[0])
			{
			case ':':
				if (strnicmp(LineBuf, ":%path%", 7) == 0)
				{
					if (fp2 > 0)
						FileClose(fp2);

					sprintf(str4, "%s%s", ExePath, (LPSTR) & LineBuf[7]);

					if ((fp2 = FileOpenWriteUTF8(str4)) < 0)
					{
						fp2 = 0;
//              return -2;
					}
				}

				break;

			case '%':
				if (strnicmp(LineBuf, "%path%", 6) == 0)
				{
					sprintf(str4, "\"%s%s\"", ExePath, (LPSTR) & LineBuf[6]);
					WriteLn(fp2, str4);
				}

				break;

			default:
				if (fp2 > 0)
					WriteLn(fp2, str4);

				break;
			}
		}

		memset(&LineBuf, 0, sizeof(LineBuf));
	}

	if (fp2 > 0)
		FileClose(fp2);

	TextFileClose(fp);
	DeleteFileUTF8(FileName2);
	DeleteFileUTF8(FileName3);
}

// ********************************************************************************************************
// ******************************* naèten?jazykového souboru *********************************************
// ********************************************************************************************************

int32 AddDesignLanguageString(int32 ID, LPSTR Text)
{
	int32 Length;
	WCHAR TextW[MAX_LENGTH_STRING];

	Length = strlen(Text);

	if (Length == 0)
		return -1;

	if ((ID < 0) || (ID >= MaxNrDesignNames))
		return -1;

	if (DesignNamesPos + Length + 2 >= DesignNamesBufSize)
		return -1;

	if (!Utf8ToUnicode(Text, TextW, MAX_LENGTH_STRING))
		return -1;

	DesignNamesId[ID] = (LPSTR) & DesignNamesBuf[DesignNamesPos];
	memmove(&DesignNamesBuf[DesignNamesPos], Text, Length + 1);
	DesignNamesPos += Length + 1;

	return 0;
}

// ********************************************************************************************************
// ******************************* naèten?jazykového souboru *********************************************
// ********************************************************************************************************

int32 AddDesignLanguageStrings(LPSTR FileName)
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
		    MessageBoxUTF8(NULL,str,SC(16,"Warning"),MB_OK);
		*/
		return -1;
	}

	LanguageFileName[0] = 0;

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
	{
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			if (LanguageFileName[0] == 0)
			{
				GetString(LineBuf, str);
				sprintf(LanguageFileName, "%s\\%s", LanguagePath, str);
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

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
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
				AddDesignLanguageString(StringNr, str3);
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
	char str[MAX_LENGTH_STRING];
	WCHAR EditFileW[MAX_LENGTH_STRING], EditFileW2[MAX_LENGTH_STRING], *FileName;
	int32 start, cnt, lengte, pos;

	pos = 0;
	lengte = 1000;
	start = 0;

	if (mode == 0)
		start = 1;

	for (cnt = start; cnt < NrParams; cnt++)
	{
		if (Parameters[cnt].Option[0] != 0)
		{
			if (strnicmp(Parameters[cnt].Option, "b", lengte) == 0)
				ForceDisableOnePinNetCheck = 1;

			if (strnicmp(Parameters[cnt].Option, "e", lengte) == 0)
				strcpy(ExePath, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "p", lengte) == 0)
			{
				strcpy(str, (LPSTR) & Parameters[cnt].Parameter[pos]);
				ExpandWithEnvironmentStrings(str);
				strcpy(ProjectPath, str);
			}

			if (strnicmp(Parameters[cnt].Option, "s", lengte) == 0)
				CreateUserIniFile();
		}
		else
		{
			if ((mode == 0) && (cnt > 0) && (Parameters[cnt].Parameter[0] != 0))
			{
				if ((Utf8ToUnicode(Parameters[cnt].Parameter, EditFileW, MAX_LENGTH_STRING - 50) == 0)
				        || (GetFullPathNameW(EditFileW, MAX_LENGTH_STRING - 50, EditFileW2, &FileName) == 0)
				        || (UnicodeToUtf8(EditFileW2, EditFile, MAX_LENGTH_STRING - 50) == 0))
				{
					MessageBoxUTF8(DESIGNWindow, Parameters[cnt].Parameter, SC(253, "Error in opening file"),
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
	int32 ok, res, KeySize;
	uint32 EAX, EBX, ECX, EDX, FlagSSE2 = 0;
	char vendor[40];
	DWORD WindowStyle;
	char str[MAX_LENGTH_STRING], *env;
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
		MessageBox(NULL, "This version of the layout editor requires a CPU with SSE2 extensions", "Error",
		           MB_APPLMODAL | MB_OK);
		return 0;
	}

	GetTempDirUTF8(str);
	GetParameters(NULL);
	GetDirFromFileName(ExecutableDir, Parameters[0].Parameter);
	strcpy(DesignExecutable, Parameters[0].Parameter);
	DecodeParameters(0);

	if (ExePath[0] == 0)
		strcpy(ExePath, ExecutableDir);

	if (!CheckExeDir())
		return -1;

	InitTimers();

//  res=TestLineConnectedToCircle(152.0,78.0,152.0,95.0,151.0,94.0,0.04);

	MemoryMain();
#ifdef _DEBUG
//  GetRegistrySoftwareKeys(ExePath);
#endif

	if (ProjectPath[0] == 0)
	{
		env = getenv(PCB_ELEG_ENVIRONMENT_STRING);

		if (env != NULL)
			strcpy(ProjectPath, env);
	}

	if (ProjectPath[0] == 0)
	{
		sprintf(str, "Software\\PCB Elegance");

		if ((res = RegOpenKeyEx(HKEY_LOCAL_MACHINE, str, 0, KEY_QUERY_VALUE, &Key)) == ERROR_SUCCESS)
		{
			KeySize = sizeof(ProjectPath) - 1;

			if ((res = RegQueryValueEx(Key, "ProjectDir", 0, NULL, (LPBYTE)str, (PDWORD)& KeySize))
				== ERROR_SUCCESS)
			{
				strcpy(ProjectPath, str);
				ok = 1;
			}

			RegCloseKey(Key);
		}
	}

	if (ProjectPath[0] == 0)
		strcpy(ProjectPath, ExePath);

	if (CheckProjectPath(ProjectPath) != 0)
		return -1;

	strcpy(DesignPath, ProjectPath);
	sprintf(UserIniFile, "%s\\user.ini", ProjectPath);

//  CheckIfSubPinBusIsPartOfPinBus("hallo[1:31]$#@","hallo[1:2]$#@");

//  GetNameWithOutExtensionFromFileName("dgdfg\\1234.ewfqef",str);

	ScreenStartX = 0;
	ScreenStartY = 0;
	ScreenSizeX = GetSystemMetrics(SM_CXMAXIMIZED) - 10;
	ScreenSizeY = GetSystemMetrics(SM_CYMAXIMIZED) - 10;
	memset(&ClientRect, 0, sizeof(RECT));
	MessageStartY = 360;

	LoadUserIniFile();
	AdjustWindowSize();
	
	sprintf(str, "%s\\LanguageDesign.txt", LanguagePath);

	if (AddDesignLanguageStrings(str) == -2)
		return -1;

	strcpy(WindowStr, SC(63, "Design manager PCB Elegance"));
	SetWindowTextUTF8(DESIGNWindow, WindowStr);
	OutputDisplay = (HDC) - 1;
	SetNormalCursor();

//  InitDialogs();

	PWindowStr = (LPSTR) & WindowStr;

	if (hPrevInstance == 0)
	{
		DESIGNClass.style = 0;
		DESIGNClass.lpfnWndProc = (WNDPROC) DESIGNWinProc;
		DESIGNClass.cbClsExtra = 0;
		DESIGNClass.cbWndExtra = 0;
		DESIGNClass.hInstance = hInstance;
		DESIGNClass.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(ICON1));
		DESIGNClass.hCursor = LoadCursor(0, IDC_ARROW);
		DESIGNClass.hbrBackground = GetStockObject(GRAY_BRUSH);
//    DESIGNClass.hbrBackground = COLOR_APPWORKSPACE+1;
		DESIGNClass.lpszMenuName = MAKEINTRESOURCE(IDR_MENU1);
		DESIGNClass.lpszClassName = DesignClassName;

		if (!RegisterClass(&DESIGNClass))
			exit(255);
	}

	ClipCursor(NULL);

	OkToUseSharedMemory = 0;

	if ((SharedMemoryHandle = OpenFileMapping(FILE_MAP_WRITE, 0, MEMORYMAPPEDSTRING)))
	{
		ok = 1;
	
#ifdef _DEBUG

		Beep(1000, 200);
		MessageBoxUTF8(NULL, SC(23, "Design manager is already opened"), SC(19, "Error"), MB_APPLMODAL | MB_OK);
	
		return 2;


	
		SharedMemory = (uint8 *) MapViewOfFile(SharedMemoryHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

		if (SharedMemory != NULL)
		{
			OkToUseSharedMemory = 1;
			memset(SharedMemory, 0, DefSharedMemoryLength);
			ProjectInfo = (ProjectInfoRecord *) SharedMemory;
			ProjectInfo->PrinterName[0] = 0;
			ProjectInfo->PaperSize = -1;
			ProjectInfo->PrintingBusy = 0;
		}

		/*
		    if (0) {
		      res=UnmapViewOfFile(SharedMemory);
		      res=CloseHandle(SharedMemoryHandle);
		    }
		*/
#endif
	}
	else
	{
// The memory mapped file should be created
#ifdef _WIN64
		SharedMemoryHandle =
			CreateFileMapping((HANDLE)0xFFFFFFFFFFFFFFFF, NULL, PAGE_READWRITE, 0, DefSharedMemoryLength,
				MEMORYMAPPEDSTRING);
#else
		SharedMemoryHandle =
			CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE, 0, DefSharedMemoryLength,
				MEMORYMAPPEDSTRING);
#endif
		if (!SharedMemoryHandle)
		{
// The memory mapped file can not be created
			ok = 1;
			MessageBoxUTF8(NULL, SC(24, "Unknown error occured"), SC(19, "Error"), MB_APPLMODAL | MB_OK);
			return 2;
		}
		else
		{
// The memory mapped file has been created
// Now map the memory mapped file into memory (Getting a pointer to the memory adres)
			SharedMemory = (uint8 *) MapViewOfFile(SharedMemoryHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			if (SharedMemory != NULL)
			{
				OkToUseSharedMemory = 1;
				memset(SharedMemory, 0, DefSharedMemoryLength);
				ProjectInfo = (ProjectInfoRecord *) SharedMemory;
				ProjectInfo->PrinterName[0] = 0;
				ProjectInfo->PaperSize = -1;
				ProjectInfo->PrintingBusy = 0;
			}

			ok = 1;
		}
	}

//    SaveSymbolsLocally=1;
	BackgroundBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_ALL));
	BackAnnotateButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_BACKANNOTATE));
	SymbolButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_SYMBOL));
	SchematicButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_SCHEMATIC));
	AnnotateButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_ANNOTATE));
	NetlistButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_NETLIST));
	LayoutButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_LAYOUT));
	GeometryButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_GEOMETRY));
	BOMButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_BOM));
	GerberButtonBitmap = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_GERBER));
	BitmapUp = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_UP));
	BitmapDown = LoadBitmap(DESIGNClass.hInstance, MAKEINTRESOURCE(BITMAP_DOWN));


	NewFont =
	    CreateFont(16, 0, 0, 0, FW_BOLD, 0, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
	               DEFAULT_QUALITY, "MS Sans Serif");
	MessagesFont =
	    CreateFont(-1, 0, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
	               DEFAULT_QUALITY, "MS Sans Serif");

//  SetDoubleClickTime(200);
	ClipID3 = RegisterClipboardFormat("Save Schematic file");

//  MakeDefaultMenu();
//  MakeSheetMenu();
//  MakeSymbolMenu();

	WindowStyle = WS_OVERLAPPEDWINDOW;
	WindowStyle &= ~(WS_MAXIMIZEBOX);

//  WindowStyle|=WS_THICKFRAME;
	DESIGNWindow =
	    CreateWindow(DesignClassName, PWindowStr, WindowStyle, WindowStartX, WindowStartY, WindowWidth, WindowHeight,
	                 HWND_DESKTOP, 0, hInstance, NULL);
	ShowWindow(DESIGNWindow, nCmdShow);
	EditWindow =
	    CreateWindow("EDIT", NULL, WS_CHILD | ES_READONLY | ES_MULTILINE | WS_SIZEBOX | ES_AUTOVSCROLL | WS_VSCROLL,
//                                 WS_CHILD|SBS_VERT|SBS_RIGHTALIGN,
	                 1, MessageStartY,
//                                     WindowHeight-MessageHeight-50,
	                 WindowWidth - 11, MessageHeight + 2, DESIGNWindow, 0, DESIGNClass.hInstance, NULL);

	ShowWindow(EditWindow, SW_SHOW);
	res = SendMessage(EditWindow, WM_SETFONT, (WPARAM) MessagesFont, 1);
	res = SendMessage(EditWindow, EM_LIMITTEXT, 256 * 1024, 0);

	MakeMainMenu();
	UpdateFileMenu(0);
//  SetSymbolMenu();
	SetWindowName(NULL, 0);
	TimerObject = SetTimer(DESIGNWindow, TimerIdentifier, 100, NULL);
	TimerValue = 0;

//  MakeMainMenu();
//  UpdateWindow(SCHFrameWindow);

	if (EditFile[0] != 0)
	{
		strcpy(DesignFile, EditFile);
		PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_FILE_OPENDESIGN, (LPARAM) 1);
	}

	while ((!TotalExit) && (GetMessage(&M, 0, 0, 0)))
	{
		TranslateMessage(&M);
		DispatchMessage(&M);
		MainLoop();
	}

//  KillTimer(DESIGNWindow,TimerIdentifier);
//  if ( Created ) DestroyWindow(DESIGNWindow);
	ClipCursor(NULL);

//  DeAllocateMem();
//  GraphicsExit();
//  KillMenus();
	DeallocateMem();

	DeleteObject(BackgroundBitmap);
	DeleteObject(BackAnnotateButtonBitmap);
	DeleteObject(SymbolButtonBitmap);
	DeleteObject(SchematicButtonBitmap);
	DeleteObject(AnnotateButtonBitmap);
	DeleteObject(NetlistButtonBitmap);
	DeleteObject(LayoutButtonBitmap);
	DeleteObject(GeometryButtonBitmap);
	DeleteObject(GeometryButtonBitmap);
	DeleteObject(BOMButtonBitmap);
	DeleteObject(NewFont);

	res = UnmapViewOfFile(SharedMemory);
	res = CloseHandle(SharedMemoryHandle);

	return 0;
}

/*
Z:\Producten\-  technical product information-\UcD modulator for UcD180_400_700-Pseries\UcD700\In development\UcD700-Modulator V2
*/

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
