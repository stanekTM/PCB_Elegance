/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: libman.c
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


#ifdef GCC_COMP
#define  WINVER   0x0500
#endif

#include "types.h"
#include "stdio.h"
#include "io.h"
#include "direct.h"
#include "sys/stat.h"
#include "fcntl.h"
#include "commctrl.h"
#include "errno.h"
#include "files2.h"
#include "libman.h"
#include "stdlib.h"
#include "line2.h"
#include "rect.h"
#include "help.h"
#include "resource.h"
#include "params.h"
#include "shlobj.h"
#include "own_process.h"
#include "htmlhelp.h"
#include "utf8.h"
#include "../functionsc/version.h"

#define  SymbolLibraryCode1        "Symbol library version 1.0"
//#define  SymbolLibraryCode2        "Symbol library version 2.0"
#define  SheetCode1                "Sheet version 1.0"
#define  SymbolCode1               "Symb1.0"
#define  SymbolCode2               "Symb2.0"
#define  SymbolCode3               "Symb3.0"
#define  ShapeCode                 "Shape definition 1.0"
#define  ShapeCode2                "Shape definition 1.5"
#define  GeometryLibraryCode1      "Geometry library version 1.0"

#define  MEMORYMAPPEDSTRING        "MMFILE_PCB_ELEGANCE"
#define  MAX_NR_LIB_SYMBOLS        4096
#define  BufSize                   256*1024

#define  MaxNrLibmanNames          1024
#define  LibmanNamesBufSize        32768

#define  SC(Nr,string) (StringConvert(Nr,string))

#define PCB_ELEG_ENVIRONMENT_STRING		"PCB_ELEG_ENVIRONMENT"

PAINTSTRUCT PS;
RECT RealWindow, ClientRect, UpdateRect;
RECT RectListWindow1, RectListWindow2;
HRGN EditingRegion;
HWND LIBMANWindow;
HDC LIBMANDisplay, OutputDisplay;
HFONT NewFont;
int32 TimerValue, WindowWidth, WindowHeigth, SymbolsSelected, DialogMode, NrLibEntries, NrParams, MouseCount, ok, ScreenSizeX, ScreenSizeY, WindowStartX, WindowStartY, DCInUse, Painting, ProjectActive, DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX = 10000, DrawWindowMaxY = 10000, MousePosX, MousePosY, SymbolToBeEditedPos[2], SymbolCountToBeEditedPos[2], OperatingSystem, ReverseY = 1, LibIndex[MAX_NR_LIB_SYMBOLS + 10], OkToUseSharedMemory, BackupMade[2], ProjectIndexNr = -1, TotalNrLibmanNames, FoundInstallation, LibraryMode,	// 0 = symbol   1 = geometry
                                                                                                                                                                                                                                          MaxTempMemory, TempMemoryPos, TextBufPos, TextBufLength, TextLineNr, FileP, FileP2, ListWindowWidth =
                                                                                                                                                                                                                                                  420, ListWindowHeight = 350, ButtonSizeX = 120, ButtonSizeY = 25, ButtonStepX = 150, ButtonStepY = 35;

uint32 TimerObject, TimerIdentifier = 0x12345678, StartUpMessage;
HMENU MainMenu, SubMenu;
char *Buf, ExportDir[MAX_LENGTH_STRING], ExecutableDir[MAX_LENGTH_STRING], VersionStr[MAX_LENGTH_STRING],
     InstallDirStr[MAX_LENGTH_STRING], ProgramFilesDir[MAX_LENGTH_STRING], ProjectDirStr[MAX_LENGTH_STRING],
     ProjectPath[MAX_LENGTH_STRING], DefaultLibraryPath[MAX_LENGTH_STRING], DefaultSymbolPath[MAX_LENGTH_STRING],
     *TextBuf, LibraryText1[MAX_LENGTH_STRING], LibraryText2[MAX_LENGTH_STRING], EditPath[MAX_LENGTH_STRING],
     LibraryFile[2][MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING], NewLibName[MAX_LENGTH_STRING],
     Params[8][MAX_LENGTH_STRING], ExePath[MAX_LENGTH_STRING], SymbolToBeEdited[2][20][MAX_LENGTH_STRING],
     TempPath[MAX_LENGTH_STRING], SymbolImportDir[MAX_LENGTH_STRING], LanguagePath[MAX_LENGTH_STRING], 
	 UserIniFile[MAX_LENGTH_STRING], UserIniFilePath[MAX_LENGTH_STRING];

LibNameRecord LibNames[2][MAX_NR_LIB_SYMBOLS];
uint8 LibNamesDeleted[MAX_NR_LIB_SYMBOLS];
uint8 *SharedMemory;
uint8 *TempMem;
LibRecord Lib[2], NewLib[2];

OSVERSIONINFO OSInfo;

ProjectInfoRecord *ProjectInfo;
HANDLE *SharedMemoryHandle;

HPEN BlackPen, GrayPen, LightGrayPen, WhitePen, EmptyPen;
HGDIOBJ SaveBrush = (HGDIOBJ) - 1;
HGDIOBJ SavePen = (HGDIOBJ) - 1;
HGDIOBJ SaveFont = (HGDIOBJ) - 1;
COLORREF LineColor;
WNDCLASS LIBMANClass, ListClass;

HGLOBAL TextBufGlobal;
HGLOBAL GlobalTempMem;
STARTUPINFO StartupInfo;



HMENU LIBMANMenu, LIBMANMenu1;

// **************************************************************************************************
// **************************************************************************************************

HWND ListWindow1, ListWindow2, CreateNewLibButton1, CreateNewLibButton2, OpenLibButton1, OpenLibButton2, SaveLibButton1,
     SaveLibButton2, CloseLibButton1, CloseLibButton2, ImportSymbolsButton1, ImportSymbolsButton2, ExportSymbolsButton1,
     ExportSymbolsButton2, DeleteSymbolsButton1, DeleteSymbolsButton2, DeselectAllButton1, DeselectAllButton2,
     SelectAllButton1, SelectAllButton2, CopyButton1, CopyButton2, MoveButton1, MoveButton2;

// ********************************************************************************************************
// ********************************************************************************************************

int32 NrLibmanNames, LibmanNamesPos;
LPSTR LibmanNamesId[MaxNrLibmanNames];
char LibmanNamesBuf[LibmanNamesBufSize];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LPSTR StringConvert(int32 Nr, LPSTR str)
{
	int32 StringCount1, StringCount2, ParamCount1, ParamCount2, cnt, Length;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	if (!str)
		return "";

	if ((Nr < 0) || (Nr >= MaxNrLibmanNames) || (LibmanNamesId[Nr] == 0))
		return str;

	strcpy(str1, str);
	strcpy(str2, LibmanNamesId[Nr]);

	StringCount1 = 0;
	ParamCount1 = 0;
	Length = strlen(str1);
	cnt = 0;

	while (cnt < Length)
	{
		if (str1[cnt] == '%')
		{
			if (cnt < Length - 1)
			{
				if (str1[cnt + 1] == 's')
					StringCount1++;

				if (str1[cnt + 1] != '%')
					ParamCount1++;
			}
		}

		cnt++;
	}

	StringCount2 = 0;
	ParamCount2 = 0;
	Length = strlen(str2);
	cnt = 0;

	while (cnt < Length)
	{
		if (str2[cnt] == '%')
		{
			if (cnt < Length - 1)
			{
				if (str2[cnt + 1] == 's')
					StringCount2++;

				if (str2[cnt + 1] != '%')
					ParamCount2++;
			}
		}

		cnt++;
	}

	if ((ParamCount1 != ParamCount2) || (StringCount1 != StringCount2))
	{
		/*
		    sprintf(str3,SC(1000,"Error in string number %d of the language file (Wrong parameters)."),Nr);
		    strcat(str3,SC(1000,"\n\nInstead the english string will be used"));
		    MessageBox(NULL,str3,SC(1000,"System error"),MB_APPLMODAL|MB_OK);
		*/
		return str;
	}

	return LibmanNamesId[Nr];
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

LRESULT FAR PASCAL LIBMANWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam);

int32 LoadSymbolFileNames(int32 ListNr);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void SetDialogItemText(HWND Dialog, int32 DlgItem, LPSTR Text)
{
	SendDlgItemMessageUTF8(Dialog, DlgItem, WM_SETTEXT, 0, (LPARAM) Text);
}

//***************************************************************************************************************
//********************************** O programu IDD_DIALOG_ABOUT ************************************************
//***************************************************************************************************************

int32 CALLBACK AboutDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(0, "About program"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(55, "Library manager PCB Elegance"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, "Web PCB Elegance"); //přidán

		sprintf(str, SC(56, "\r\n Build version %i.%i.%i  ( %s )"), VER_VERSION / 100, VER_VERSION % 100, VER_BUILD, VER_DATE_STR);

#ifdef GCC_COMP
		strcat(str, "\r\n\r\n Compiled with mingw (gcc 4.6.1)");
#endif
#ifdef VC2005
		strcat(str, "\r\n\r\n Compiled with Microsoft Visual Studio 2005");
#endif
#ifdef VC2010
		strcat(str, SC(54, "\r\n\r\n Compiled with Microsoft Visual Studio 2019"));
#endif
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) (LPSTR) str);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			ShellExecute(0, 0, "http://www.pcbelegance.org", 0, 0, SW_SHOW); //přidán
			return about;
		}

		break;
	}

	about = 0;
	return about;
}


// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 PointInRectangle(int32 x, int32 y, RECT * Rect)
{
	if (x < Rect->left)
		return 0;

	if (y < Rect->top)
		return 0;

	if (x >= Rect->right)
		return 0;

	if (y >= Rect->bottom)
		return 0;

	return 1;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


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

	if (LibraryMode == 0)
		ProjectInfo->FileTypes[cnt] = 5;
	else
		ProjectInfo->FileTypes[cnt] = 6;

	ProjectInfo->WindowHandles[cnt] = Window;

}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemTemp(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxTempMemory == 0)
	{
		if ((GlobalTempMem = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((TempMem = GlobalLock(GlobalTempMem)) == NULL)
			return -1;

		MaxTempMemory = MemSize;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(GlobalTempMem, MemSize, GHND)) == NULL)
			return -1;

		GlobalTempMem = NewMem;

		if ((TempMem = GlobalLock(GlobalTempMem)) == NULL)
			return -1;

		MaxTempMemory = MemSize;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemTemp()
{
	if (GlobalTempMem != NULL)
	{
		GlobalUnlock(GlobalTempMem);
		GlobalFree(GlobalTempMem);
		GlobalTempMem = NULL;
		MaxTempMemory = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CopyFileNameToTempMem(LPSTR FileName)
{
	int32 Length;

	Length = strlen(FileName);

	if (Length + TempMemoryPos + 10 > MaxTempMemory)
		AllocateMemTemp(TempMemoryPos + 16384);

	memmove(&TempMem[TempMemoryPos], FileName, Length + 1);
	TempMemoryPos += Length + 1;
	TempMem[TempMemoryPos] = 0;
}

//**************************************************************************************************************
//********************** Exportovat symboly / geometrie IDD_DIALOG_GET_DIR *************************************
//**************************************************************************************************************

int32 CALLBACK GetDirDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res;
	char str2[MAX_LENGTH_STRING], Dir[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(4, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(5, "Select directory"));

		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) (LPSTR) DefaultSymbolPath);
		sprintf(str2, "%s\\*.*", ProjectPath);
		FileSearchHandle = FindFirstFileUTF8(str2, &FileInfo);
		res = 1;

		if (FileSearchHandle == INVALID_HANDLE_VALUE)
			res = 0;

		while (res)
		{
			if (((FileInfo.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) && (wcscmp(FileInfo.cFileName, L".") != 0)
			        && (wcscmp(FileInfo.cFileName, L"..") != 0))
			{
				UnicodeToUtf8(FileInfo.cFileName, str3, MAX_LENGTH_STRING - 100);

				if (LibraryMode == 0)
				{
					sprintf(Dir, "%s\\%s\\sym", ProjectPath, str3);

					if (DirectoryExistsUTF8(Dir) == 0)
						SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) (LPSTR) Dir);
					SetWindowTextUTF8(Dialog, SC(36, "Export symbols")); //přidán
				}
				else
				{
					sprintf(Dir, "%s\\%s\\pcb\\shapes", ProjectPath, str3);

					if (DirectoryExistsUTF8(Dir) == 0)
						SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) (LPSTR) Dir);
					SetWindowTextUTF8(Dialog, SC(53, "Export geometries")); //přidán
				}
			}

			res = FindNextFileW(FileSearchHandle, &FileInfo);
		}

		if (FileSearchHandle != INVALID_HANDLE_VALUE)
			FindClose(FileSearchHandle);

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_SELECTSTRING, 0, (LPARAM) (LPSTR) DefaultSymbolPath);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

			if (res != CB_ERR)
			{
				SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_GETLBTEXT, res, (LPARAM) ExportDir);
				EndDialog(Dialog, 1);
			}
			else
				EndDialog(Dialog, 2);

			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}


int32 GetNewDirectory(LPSTR NewDir, int32 mode)
{
	int32 res;
	res = DialogBox(LIBMANClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_GET_DIR), LIBMANWindow, (DLGPROC) GetDirDialog2);

	if (res == 2)
		return -1;

	return 0;
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
			OutputDisplay = LIBMANDisplay = BeginPaint(LIBMANWindow, &PS);
		else
			OutputDisplay = LIBMANDisplay = GetDC(LIBMANWindow);

		SetBkMode(LIBMANDisplay, OPAQUE);
//    SetBkMode(LIBMANDisplay,TRANSPARENT);
		SetTextColor(LIBMANDisplay, RGB_Black);
		SetBkColor(LIBMANDisplay, RGB_LightGray);
		SetROP2(OutputDisplay, R2_COPYPEN);
//    SaveFont = SelectObject(WindowsDisplay, UserFont);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void StartDrawingEditingWindow()
{
	InitDeviceContext();
	SetROP2(OutputDisplay, R2_COPYPEN);
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
			EndPaint(LIBMANWindow, &PS);
		else
			ReleaseDC(LIBMANWindow, LIBMANDisplay);

		DCInUse = 0;
		LIBMANDisplay = NULL;
		OutputDisplay = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void EndDrawingEditingWindow()
{
	DoneDeviceContext();
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void ExitDrawing()
{
	if (SaveBrush != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SaveBrush);
		SaveBrush = (HGDIOBJ) - 1;
	}

	if (SavePen != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SavePen);
		SavePen = (HGDIOBJ) - 1;
	}

	if (SaveFont != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SaveFont);
		SaveFont = (HGDIOBJ) - 1;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorWhite()
{
	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, WhitePen);
	else
		SelectObject(OutputDisplay, WhitePen);

	LineColor = RGB_White;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorBlack()
{
	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, BlackPen);
	else
		SelectObject(OutputDisplay, BlackPen);

	LineColor = RGB_Black;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorGray()
{
	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, GrayPen);
	else
		SelectObject(OutputDisplay, GrayPen);

	LineColor = RGB_Gray;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorLightGray()
{
	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, LightGrayPen);
	else
		SelectObject(OutputDisplay, LightGrayPen);

	LineColor = RGB_LightGray;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DisplayRectangleIn3dLook(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	if (mode == 1)
		StartDrawingEditingWindow();

	InitDrawingColorWhite();
	DrawLine(x1, y1, x2, y1);
	DrawLine(x2, y1, x2, y2);
	DrawLine(x2, y2, x1, y2);
	DrawLine(x1, y2, x1, y1);

	if (mode == 1)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void rect4a(int32 x1, int32 y1, int32 x2, int32 y2, int32 mode)
{
	if (mode == 1)
		StartDrawingEditingWindow();

	DrawLine(x1, y1, x2, y1);
	DrawLine(x2, y1, x2, y2);
	DrawLine(x2, y2, x1, y2);
	DrawLine(x1, y2, x1, y1);

	if (mode == 1)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void DisplayLibraryText(int32 mode)
{

	if (mode == 1)
		StartDrawingEditingWindow();

	if (SaveFont == (HGDIOBJ) - 1)
		SaveFont = SelectObject(OutputDisplay, NewFont);
	else
		SelectObject(OutputDisplay, NewFont);

	strcpy(LibraryText1, "                                                                           ");

	if (LibraryFile[0][0] != 0)
	{
		if (LibraryMode == 0)
			sprintf(LibraryText1, SC(6, "%s [%i symbols]"), LibraryFile[0], Lib[0].NrLibEntries);
		else
			sprintf(LibraryText1, SC(7, "%s [%i geometries]"), LibraryFile[0], Lib[0].NrLibEntries);
	}

	strcpy(LibraryText2, "                                                                           ");

	if (LibraryFile[1][0] != 0)
	{
		if (LibraryMode == 0)
			sprintf(LibraryText2, SC(6, "%s [%i symbols]"), LibraryFile[1], Lib[1].NrLibEntries);
		else
			sprintf(LibraryText2, SC(7, "%s [%i geometries]"), LibraryFile[1], Lib[1].NrLibEntries);
	}

	TextOutUTF8(OutputDisplay, RectListWindow1.left + 3, RectListWindow1.top - 22, LibraryText1, strlen(LibraryText1));
	TextOutUTF8(OutputDisplay, RectListWindow2.left + 3, RectListWindow2.top - 22, LibraryText2, strlen(LibraryText2));

	if (mode == 1)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void WindowPaint()
{
	RECT Rect;

	GetClientRect(LIBMANWindow, &Rect);

	if (GetUpdateRect(LIBMANWindow, &UpdateRect, 0))
	{
		Painting = 1;
		InitDeviceContext();
		FillRect(LIBMANDisplay, &UpdateRect, GetStockObject(LTGRAY_BRUSH));
		Rect.right = 10000;
		Rect.bottom = 1;
		FillRect(LIBMANDisplay, &Rect, GetStockObject(BLACK_BRUSH));

		InitDrawingColorBlack();
		rect4a(RectListWindow1.left - 7, RectListWindow1.top - 30, RectListWindow1.right + 32,
		       RectListWindow1.bottom + 113, 0);
		rect4a(RectListWindow2.left - 7, RectListWindow2.top - 30, RectListWindow2.right + 32,
		       RectListWindow2.bottom + 113, 0);
		InitDrawingColorWhite();
		rect4a(RectListWindow1.left + 1, RectListWindow1.top - 23, RectListWindow1.right + 24, RectListWindow1.top - 6,
		       0);
		rect4a(RectListWindow2.left + 1, RectListWindow2.top - 23, RectListWindow2.right + 24, RectListWindow2.top - 6,
		       0);
		DisplayLibraryText(0);
		DoneDeviceContext();
		Painting = 0;
	}
}


// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void DeselectList1()
{
	int32 cnt;

	for (cnt = 0; cnt < Lib[0].NrLibEntries; cnt++)
		SendMessage(ListWindow1, LB_SETSEL, 0, cnt);
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************


void DeselectList2()
{
	int32 cnt;

	for (cnt = 0; cnt < Lib[1].NrLibEntries; cnt++)
		SendMessage(ListWindow2, LB_SETSEL, 0, cnt);
}


// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void SelectList1()
{
	int32 cnt;

	for (cnt = 0; cnt < Lib[0].NrLibEntries; cnt++)
		SendMessage(ListWindow1, LB_SETSEL, 1, cnt);
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************


void SelectList2()
{
	int32 cnt;

	for (cnt = 0; cnt < Lib[1].NrLibEntries; cnt++)
		SendMessage(ListWindow2, LB_SETSEL, 1, cnt);
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 MakeBackup(int32 ListNr)
{
	char DirPart[MAX_LENGTH_STRING], FilePart[MAX_LENGTH_STRING], BackupFile1[MAX_LENGTH_STRING];

	if (BackupMade[ListNr])
		return 0;

	BackupMade[ListNr] = 1;

	if ((GetDirFromFileName(DirPart, LibraryFile[ListNr]) == 0)
	        && (GetFilePartFromFileName(FilePart, LibraryFile[ListNr]) == 0))
	{
		sprintf(BackupFile1, "%s\\backup\\%s", DirPart, FilePart);
		CopyFileUTF8(LibraryFile[ListNr], BackupFile1, 0);
	}
	else
		return -2;

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 PutSymbolsIntoList(int32 ListNr, int32 mode)
{

	int32 res, cnt, Libfp, result;

	if (LibraryFile[ListNr][0] == 0)
	{
		MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
		return -1;
	}

	if ((Libfp = FileOpenReadOnlyUTF8(LibraryFile[ListNr])) == -1)
		return -1;

	if (FileRead(Libfp, &Lib[ListNr], sizeof(LibRecord), &result) < 0)
		return -1;

	Lib[ListNr].NrLibEntries = min(MAX_NR_LIB_SYMBOLS, Lib[ListNr].NrLibEntries);

	if (FileRead(Libfp, &LibNames[ListNr], Lib[ListNr].NrLibEntries * sizeof(LibNameRecord), &result) < 0)
		return -1;

	FileClose(Libfp);

	if (ListNr == 0)
		res = SendMessage(ListWindow1, LB_RESETCONTENT, 0, 0);
	else
		res = SendMessage(ListWindow2, LB_RESETCONTENT, 0, 0);

	for (cnt = 0; cnt < MAX_NR_LIB_SYMBOLS; cnt++)
		LibIndex[cnt] = -1;

	for (cnt = 0; cnt < Lib[ListNr].NrLibEntries; cnt++)
	{
		if (ListNr == 0)
			res = SendMessageUTF8(ListWindow1, LB_ADDSTRING, 0, (LPARAM) & LibNames[ListNr][cnt].Text);
		else
			res = SendMessageUTF8(ListWindow2, LB_ADDSTRING, 0, (LPARAM) & LibNames[ListNr][cnt].Text);
	}

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 AddSymbolToList(int32 ListNr, LPSTR SymbolName, int32 mode)
{
	int32 res;

	if (ListNr == 0)
		res = SendMessageUTF8(ListWindow1, LB_FINDSTRING, (WPARAM) - 1, (LPARAM) SymbolName);
	else
		res = SendMessageUTF8(ListWindow2, LB_FINDSTRING, (WPARAM) - 1, (LPARAM) SymbolName);

	if (res != LB_ERR)
		return -1;

	if (ListNr == 0)
		res = SendMessageUTF8(ListWindow1, LB_ADDSTRING, 0, (LPARAM) SymbolName);
	else
		res = SendMessageUTF8(ListWindow2, LB_ADDSTRING, 0, (LPARAM) SymbolName);

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 RemoveSymbolFromList(int32 ListNr, LPSTR SymbolName, int32 mode)
{
	int32 res;

	if (ListNr == 0)
		res = SendMessageUTF8(ListWindow1, LB_FINDSTRING, (WPARAM) - 1, (LPARAM) SymbolName);
	else
		res = SendMessageUTF8(ListWindow2, LB_FINDSTRING, (WPARAM) - 1, (LPARAM) SymbolName);

	if (res == LB_ERR)
		return -1;

	if (ListNr == 0)
		res = SendMessage(ListWindow1, LB_DELETESTRING, res, 0);
	else
		res = SendMessage(ListWindow2, LB_DELETESTRING, res, 0);

	return 0;
}

//*********************************************************************************************************
//******************* Vytvořit novou knihovnu symbolů / geometrie IDD_DIALOG_NEW_LIB **********************
//*********************************************************************************************************

int32 CALLBACK NewLibDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(4, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(48, "Library directory"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(49, "Specify a new library file name"));
		
		if (LibraryMode == 0)
			SetWindowTextUTF8(Dialog, SC(44, "Create a new symbol library")); //přidán
		else
			SetWindowTextUTF8(Dialog, SC(45, "Create a new geometry library")); //přidán
		
		sprintf(str, "%s\\", DefaultLibraryPath);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) (LPSTR) str);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			if (SendDlgItemMessageUTF8
			        (Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) (LPSTR) & DialogTextLine) > 0)
			{
				strcpy((LPSTR) & NewLibName, (LPSTR) & DialogTextLine);
				EndDialog(Dialog, 1);
			}
			else
				EndDialog(Dialog, 2);

			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

void MakeNewLib(int32 ListNr)
{
	int32 res, fp, cnt;
	char str2[MAX_LENGTH_STRING];
	LibNameRecord LibName;

	res = DialogBox(LIBMANClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NEW_LIB), LIBMANWindow, (DLGPROC) NewLibDialog2);

	if (res == 1)
	{
		strcpy(str2, NewLibName);

		if (strchr(str2, '.'))
			CutExtensionFileName(str2);

		if (LibraryMode == 0)
			sprintf(NewLibName, "%s.lib", str2);
		else
			sprintf(NewLibName, "%s.slb", str2);

		if (FileExistsUTF8(NewLibName) == 0)
		{
			sprintf(str2, SC(10, "File %s exists overwerite ?"), NewLibName);

			if (MessageBoxUTF8(LIBMANWindow, str2, SC(9, "Error"), MB_OKCANCEL) != IDOK)
				return;
		}

		if ((fp = FileOpenWriteUTF8(NewLibName)) < 0)
			return;

		memset(&Lib[ListNr], 0, sizeof(LibRecord));

		if (LibraryMode == 0)
			memmove(&Lib[ListNr].Identification, SymbolLibraryCode1, strlen(SymbolLibraryCode1));
		else
			memmove(&Lib[ListNr].Identification, GeometryLibraryCode1, strlen(GeometryLibraryCode1));

		Lib[ListNr].MaxNrLibEntries = 200;
		FileWrite(fp, &Lib[ListNr], sizeof(LibRecord), &res);
		memset(&LibName, 0, sizeof(LibNameRecord));

		for (cnt = 0; cnt < 200; cnt++)
			FileWrite(fp, &LibName, sizeof(LibNameRecord), &res);

		FileClose(fp);
		memset(&LibNames[ListNr], 0, sizeof(LibNames[ListNr]));
		strcpy(LibraryFile[ListNr], NewLibName);
		DisplayLibraryText(1);

//    sprintf(str2,"Symbol library %s [%i symbols]",LibraryFile,Lib[ListNr].NrLibEntries);
//    SetWindowText(LIBMANWindow,str2);

	}

}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void CloseLib(int32 ListNr)
{
	int32 res;

	if (ListNr == 0)
	{
		res = SendMessage(ListWindow1, LB_RESETCONTENT, 0, 0);
		LibraryText1[0] = 0;
	}
	else
	{
		res = SendMessage(ListWindow2, LB_RESETCONTENT, 0, 0);
		LibraryText2[0] = 0;
	}

	LibraryFile[ListNr][0] = 0;
	DisplayLibraryText(1);
}


// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 LoadLib(int32 ListNr)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING];
	int32 fp, result, WrongLibrary = 1;

	str[0] = 0;
	str2[0] = 0;
	TempMem[0] = 0;
	TempMem[1] = 0;

	if (LibraryMode == 0)
	{
		strcpy(str5, SC(11, "Symbol library files"));
		strcpy(str2, "lib");
	}
	else
	{
		strcpy(str5, SC(12, "Geometry library files"));
		strcpy(str2, "slb");
	}

	LibraryFile[ListNr][0] = 0;

	if (GetNewFileUTF8
	        (LIBMANWindow, LIBMANClass.hInstance, (char *) &LibraryFile[ListNr], DefaultLibraryPath, str5, NULL, str5, str2,
	         0) != 0)
		return 0;

	if ((fp = FileOpenReadOnlyUTF8(LibraryFile[ListNr])) < 0)
	{
		sprintf(str2, SC(13, "Error in reading library %s"), LibraryFile[ListNr]);
		MessageBoxUTF8(LIBMANWindow, str2, SC(9, "Error"), MB_OK);
		return -1;
	}

	if (FileRead(fp, &Lib[ListNr], sizeof(LibRecord), &result) < 0)
	{
		sprintf(str2, SC(13, "Error in reading library %s"), LibraryFile[ListNr]);
		MessageBoxUTF8(LIBMANWindow, str2, SC(9, "Error"), MB_OK);
		FileClose(fp);
		return -1;
	}

	if (LibraryMode == 0)
	{
		if (strcmp((LPSTR) & Lib[ListNr].Identification, SymbolLibraryCode1) == 0)
			WrongLibrary = 0;
	}
	else
	{
		if (strcmp((LPSTR) & Lib[ListNr].Identification, GeometryLibraryCode1) == 0)
			WrongLibrary = 0;
	}

	if (WrongLibrary)
	{
		sprintf(str2, SC(14, "Library %s is wrong"), LibraryFile[ListNr]);
		MessageBoxUTF8(LIBMANWindow, str2, SC(9, "Error"), MB_OK);
		FileClose(fp);
		return -1;
	}

	FileClose(fp);
	DisplayLibraryText(1);
	PutSymbolsIntoList(ListNr, 0);

	return 1;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 AddSymbolToLibrary(LPSTR SymbolFile, int32 ListNr, int32 mode)
{
	int32 fp, fp2, fp3, result, LibraryFileSize, res2, LibSymbolDataPos, res, SymbolFileSize, SymbolIndexNr, FilePos,
	      BytesToWrite, cnt;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], SymbolName[MAX_LENGTH_STRING];
	LibNameRecord LibName, ExistingLibName;
	SymbolRecord Symbol;
	ShapeRecord Shape;

	memset(&ExistingLibName, 0, sizeof(ExistingLibName));
	SymbolIndexNr = -1;
	GetNameWithOutExtensionFromFileName(SymbolFile, SymbolName);

	if ((mode & 4) == 4)
	{
		result = strlen(SymbolName);
		SymbolName[result - 3] = 0;
	}

	if (FileExistsUTF8(SymbolFile) != 0)
	{
		if ((mode & 1) == 1)
		{
			if (LibraryMode == 0)
				sprintf(str, SC(15, "Can not open symbol in file %s"), SymbolFile);
			else
				sprintf(str, SC(16, "Can not open geometry in file %s"), SymbolFile);

			MessageBoxUTF8(LIBMANWindow, str, SC(9, "Error"), MB_APPLMODAL | MB_OK);
		}

		return -1;
	}

	if ((fp = FileOpenReadOnlyUTF8(SymbolFile)) <= 0)
		return -1;

	if (LibraryMode == 0)
	{
		if (FileRead(fp, &Symbol, sizeof(SymbolRecord), &result) == 0)
		{
			if ((strcmp((LPSTR) & Symbol, SymbolCode1) != 0) && (strcmp((LPSTR) & Symbol, SymbolCode2) != 0)
			        && (strcmp((LPSTR) & Symbol, SymbolCode3) != 0))
			{
				if ((mode & 1) == 1)
				{
					sprintf(str2, SC(17, "Symbol %s is wrong"), SymbolFile);
					MessageBoxUTF8(LIBMANWindow, str2, SC(9, "Error"), MB_APPLMODAL | MB_OK);
					FileClose(fp);
				}

				return -2;
			}
		}
	}
	else
	{
		if (FileRead(fp, &Shape, sizeof(ShapeRecord), &result) == 0)
		{
			if ((strcmp((LPSTR) & Shape, ShapeCode) != 0) && (strcmp((LPSTR) & Shape, ShapeCode2) != 0))
			{
				if ((mode & 1) == 1)
				{
					sprintf(str2, SC(18, "Shape %s is wrong"), SymbolFile);
					MessageBoxUTF8(LIBMANWindow, str2, SC(9, "Error"), MB_APPLMODAL | MB_OK);
					FileClose(fp);
				}

				return -2;
			}
		}
	}

	FileClose(fp);

	if ((fp = FileOpenReadOnlyUTF8(LibraryFile[ListNr])) <= 0)
		return -3;

	FileClose(fp);

	if ((LibraryFileSize = FileSizeUTF8(LibraryFile[ListNr])) < 0)
		return -100;

	if (LibraryFileSize > 32 * 1040 * 1024)
	{
		if ((mode & 1) == 1)
			MessageBoxUTF8(LIBMANWindow, SC(19, "Library size >32 Mbyte"), SC(9, "Error"), MB_APPLMODAL | MB_OK);

		return -4;
	}

	if ((SymbolFileSize = FileSizeUTF8(SymbolFile)) < 0)
		return -100;

	if ((mode & 1) == 1)
	{
		if (LibraryMode == 0)
		{
			if (SymbolFileSize > 1024 * 1024)
			{
				MessageBoxUTF8(LIBMANWindow, SC(20, "Symbol size >1024 kbyte"), SC(9, "Error"), MB_APPLMODAL | MB_OK);
				return -5;
			}
		}
		else
		{
			if (SymbolFileSize > 4096 * 1024)
			{
				MessageBoxUTF8(LIBMANWindow, SC(21, "Geometry size >4096 kbyte"), SC(9, "Error"), MB_APPLMODAL | MB_OK);
				return -5;
			}
		}
	}

	SymbolName[sizeof(LibName.Text) - 1] = 0;

	if (Lib[ListNr].NrLibEntries >= MAX_NR_LIB_SYMBOLS)
		return -12;

	for (cnt = 0; cnt < Lib[ListNr].NrLibEntries; cnt++)
	{
		if (stricmp(LibNames[ListNr][cnt].Text, SymbolName) == 0)
		{
			if (SymbolIndexNr == -1)
			{
				SymbolIndexNr = cnt;
				memmove(&ExistingLibName, &LibNames[ListNr][cnt], sizeof(LibNameRecord));

				if ((mode & 1) == 1)
				{
					if (LibraryMode == 0)
						sprintf(str, SC(22, "Symbol %s exists, overwrite ?"), SymbolName);
					else
						sprintf(str, SC(23, "Geometry %s exists, overwrite ?"), SymbolName);

					res = MessageBoxUTF8(LIBMANWindow, str, SC(43, "Message"), MB_APPLMODAL | MB_YESNOCANCEL);

					switch (res)
					{
					case IDYES:
						mode |= 2;
						break;

					case IDNO:
						return -10;

					case IDCANCEL:
						return -20;
					}
				}
			}
		}
	}

	if ((mode & 2) && (SymbolIndexNr == -1))
	{
		if (mode & 1)
		{
			if (LibraryMode == 0)
				sprintf(str, SC(24, "Symbol %s not found"), SymbolName);
			else
				sprintf(str, SC(25, "Geometry %s not found"), SymbolName);

			MessageBoxUTF8(LIBMANWindow, str, SC(9, "Error"), MB_APPLMODAL | MB_OK);
		}

		return -10;
	}

	MakeBackup(ListNr);

	if ((mode & 2) == 0)
	{
// **************************************************************************************************
// **************************************************************************************************
//
// Add symbol to library
//
		if (Lib[ListNr].NrLibEntries < Lib[ListNr].MaxNrLibEntries)
		{
			memset(&LibName, 0, sizeof(LibNameRecord));
			strcpy(LibName.Text, SymbolName);

			if (AddSymbolToList(ListNr, SymbolName, 0) == -1)
			{
			}

			LibName.Pos = LibraryFileSize;
			LibName.Length = SymbolFileSize;
			memmove(&LibNames[ListNr][Lib[ListNr].NrLibEntries], &LibName, sizeof(LibNameRecord));
			Lib[ListNr].NrLibEntries++;
			fp = FileOpenUTF8(LibraryFile[ListNr]);
			FileSeek(fp, 0);
			FileWrite(fp, &Lib[ListNr], sizeof(LibRecord), &result);
			FileSeek(fp, sizeof(LibRecord) + (Lib[ListNr].NrLibEntries - 1) * sizeof(LibNameRecord));
			FileWrite(fp, &LibName, sizeof(LibNameRecord), &result);
			FileSeek(fp, LibraryFileSize);
			fp3 = FileOpenReadOnlyUTF8(SymbolFile);
			result = 1;

			while (result > 0)
			{
				FileRead(fp3, Buf, BufSize, &result);

				if (result > 0)
					FileWrite(fp, Buf, result, &res2);
			}

			FileClose(fp3);
			FileClose(fp);
		}
		else
		{
// **************************************************************************************************
// **************************************************************************************************
//
// Add symbol to library, and extend the number of symbols
//
			sprintf(str, "%s\\library.$#@", DefaultLibraryPath);

			if ((fp2 = FileOpenWriteUTF8(str)) > 0)
			{
				fp = FileOpenUTF8(LibraryFile[ListNr]);
				FileSeek(fp, sizeof(LibRecord) + Lib[ListNr].MaxNrLibEntries * sizeof(LibNameRecord));
				Lib[ListNr].MaxNrLibEntries += 100;

				for (cnt = 0; cnt < Lib[ListNr].NrLibEntries; cnt++)
					LibNames[ListNr][cnt].Pos += 100 * sizeof(LibNameRecord);

				strcpy(LibNames[ListNr][Lib[ListNr].NrLibEntries].Text, SymbolName);
				AddSymbolToList(ListNr, SymbolName, 0);

				LibNames[ListNr][Lib[ListNr].NrLibEntries].Pos = LibraryFileSize + 100 * sizeof(LibNameRecord);
				LibNames[ListNr][Lib[ListNr].NrLibEntries].Length = SymbolFileSize;
				Lib[ListNr].NrLibEntries++;

				FileWrite(fp2, &Lib[ListNr], sizeof(LibRecord), &result);
				FileWrite(fp2, &LibNames[ListNr], Lib[ListNr].MaxNrLibEntries * sizeof(LibNameRecord), &result);
				result = 1;

				while (result > 0)
				{
					FileRead(fp, Buf, BufSize, &result);

					if (result > 0)
						FileWrite(fp2, Buf, result, &res2);
				}

				FileClose(fp);
				fp3 = FileOpenReadOnly(SymbolFile);
				result = 1;

				while (result > 0)
				{
					FileRead(fp3, Buf, BufSize, &result);

					if (result > 0)
						FileWrite(fp, Buf, result, &res2);
				}

				FileClose(fp3);
				FileClose(fp2);

				CopyFileUTF8(str, LibraryFile[ListNr], 0);
				DeleteFileUTF8(str);
			}
		}
	}
	else
	{
// **************************************************************************************************
// **************************************************************************************************
//
// Replace symbol in library
//
		sprintf(str, "%s\\library.$#@", DefaultLibraryPath);

		if ((fp2 = FileOpenWriteUTF8(str)) > 0)
		{
			fp = FileOpenUTF8(LibraryFile[ListNr]);
			FilePos = LibNames[ListNr][SymbolIndexNr].Pos;
			LibSymbolDataPos = sizeof(LibRecord) + Lib[ListNr].MaxNrLibEntries * sizeof(LibNameRecord);

			for (cnt = SymbolIndexNr + 1; cnt < Lib[ListNr].NrLibEntries; cnt++)
				LibNames[ListNr][cnt].Pos += SymbolFileSize - ExistingLibName.Length;

			LibNames[ListNr][SymbolIndexNr].Length = SymbolFileSize;
			FileWrite(fp2, &Lib[ListNr], sizeof(LibRecord), &result);
			FileWrite(fp2, &LibNames[ListNr], Lib[ListNr].MaxNrLibEntries * sizeof(LibNameRecord), &result);

			BytesToWrite = FilePos - LibSymbolDataPos;
			FileSeek(fp, LibSymbolDataPos);

			while (BytesToWrite > 0)
			{
				FileRead(fp, Buf, min(BytesToWrite, BufSize), &result);

				if (result > 0)
				{
					FileWrite(fp2, Buf, result, &res2);
					BytesToWrite -= result;
				}
				else
					BytesToWrite = 0;
			}

			fp3 = FileOpenReadOnlyUTF8(SymbolFile);
			result = 1;

			while (result > 0)
			{
				FileRead(fp3, Buf, BufSize, &result);

				if (result > 0)
					FileWrite(fp2, Buf, result, &res2);
			}

			FileClose(fp3);
			FileSeek(fp, FilePos + ExistingLibName.Length);
			result = 1;

			while (result > 0)
			{
				FileRead(fp, Buf, BufSize, &result);

				if (result > 0)
					FileWrite(fp2, Buf, result, &res2);
			}

			FileClose(fp2);
			FileClose(fp);

			CopyFileUTF8(str, LibraryFile[ListNr], 0);
			DeleteFileUTF8(str);
		}
	}

	DisplayLibraryText(1);

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 DeleteSymbols(int32 ListNr)
{
	int32 cnt, cnt2, count, count2, fp, fp2, result, res2, FilePos, FilePos2;
	char str[MAX_LENGTH_STRING];
	int32 Found = 0;

	if (ListNr == 0)
		SymbolsSelected = SendMessage(ListWindow1, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS + 1, (LPARAM) (LPINT) & LibIndex);
	else
		SymbolsSelected = SendMessage(ListWindow2, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS + 1, (LPARAM) (LPINT) & LibIndex);

	if (SymbolsSelected < 1)
		return -1;

	MakeBackup(ListNr);
	memset(&LibNamesDeleted, 0, sizeof(LibNamesDeleted));

	for (cnt = SymbolsSelected - 1; cnt >= 0; cnt--)
	{
		if (ListNr == 0)
			SendMessageUTF8(ListWindow1, LB_GETTEXT, LibIndex[cnt], (LPARAM) str);
		else
			SendMessageUTF8(ListWindow2, LB_GETTEXT, LibIndex[cnt], (LPARAM) str);

		cnt2 = 0;

		while ((cnt2 < Lib[ListNr].NrLibEntries) && (stricmp(LibNames[ListNr][cnt2].Text, str) != 0))
			cnt2++;

		if (cnt2 < Lib[ListNr].NrLibEntries)
		{
			LibNamesDeleted[cnt2] = 1;
			RemoveSymbolFromList(ListNr, str, 0);
			Found = 1;
		}
	}

	if (!Found)
		return -2;

	sprintf(str, "%s\\library.$#@", DefaultLibraryPath);

	if ((fp = FileOpenReadOnlyUTF8(LibraryFile[ListNr])) < 0)
		return -1;

	if ((fp2 = FileOpenWriteUTF8(str)) < 0)
		return -1;

	count = 0;
	FilePos = sizeof(LibRecord) + Lib[ListNr].MaxNrLibEntries * sizeof(LibNameRecord);
	FilePos2 = FilePos;
	FileSeek(fp2, FilePos2);

	for (cnt = 0; cnt < Lib[ListNr].NrLibEntries; cnt++)
	{
		FileSeek(fp, FilePos);

		if (LibNamesDeleted[cnt] == 0)
		{
			if (count != cnt)
				memmove(&LibNames[ListNr][count], &LibNames[ListNr][cnt], sizeof(LibNameRecord));

			LibNames[ListNr][count].Pos = FilePos2;
			count2 = LibNames[ListNr][count].Length;

			while (count2 > 0)
			{
				FileRead(fp, Buf, min(count2, BufSize), &result);

				if (result > 0)
					FileWrite(fp2, Buf, result, &res2);

				count2 -= result;
			}

			FilePos2 += LibNames[ListNr][count].Length;
			count++;
		}

		FilePos += LibNames[ListNr][cnt].Length;
	}

	FileSeek(fp2, 0);
	Lib[ListNr].NrLibEntries = count;
	FileWrite(fp2, &Lib[ListNr], sizeof(LibRecord), &res2);
	memset(&LibNames[ListNr][Lib[ListNr].NrLibEntries], 0,
	       (Lib[ListNr].MaxNrLibEntries - Lib[ListNr].NrLibEntries) * sizeof(LibNameRecord));
	FileWrite(fp2, &LibNames[ListNr], Lib[ListNr].MaxNrLibEntries * sizeof(LibNameRecord), &res2);
	FileClose(fp);
	FileClose(fp2);
	DisplayLibraryText(1);

	CopyFileUTF8(str, LibraryFile[ListNr], 0);
	DeleteFileUTF8(str);
	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 ExportSymbols(int32 ListNr, int32 mode)
{
	int32 res, cnt, cnt2, count, length, fp, fp2, result, res2;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], SymbolFile[MAX_LENGTH_STRING],
	     ExportDir2[MAX_LENGTH_STRING];

	if (ListNr == 0)
		SymbolsSelected = SendMessage(ListWindow1, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS, (LPARAM) (LPINT) & LibIndex);
	else
		SymbolsSelected = SendMessage(ListWindow2, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS, (LPARAM) (LPINT) & LibIndex);

	if (SymbolsSelected < 1)
		return -1;

	if (mode == 1)
	{
		TempMemoryPos = 0;
		TempMem[0] = 0;
		TempMem[1] = 0;
		strcpy(ExportDir2, TempPath);
	}
	else
	{
		res =
		    DialogBox(LIBMANClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_GET_DIR), LIBMANWindow,
		              (DLGPROC) GetDirDialog2);

		if (res == 2)
			return -1;

		strcpy(ExportDir2, ExportDir);
	}

	count = 0;

	for (cnt = 0; cnt < SymbolsSelected; cnt++)
	{
		if (ListNr == 0)
			SendMessageUTF8(ListWindow1, LB_GETTEXT, LibIndex[cnt], (LPARAM) str3);
		else
			SendMessageUTF8(ListWindow2, LB_GETTEXT, LibIndex[cnt], (LPARAM) str3);

		cnt2 = 0;

		while ((cnt2 < Lib[ListNr].NrLibEntries) && (stricmp(LibNames[ListNr][cnt2].Text, str3) != 0))
			cnt2++;

		if (cnt2 < Lib[ListNr].NrLibEntries)
		{
			length = LibNames[ListNr][cnt2].Length;

			if (LibraryMode == 0)
			{
				if (mode == 0)
					sprintf(SymbolFile, "%s.sym", str3);
				else
					sprintf(SymbolFile, "%s__#.sym", str3);
			}
			else
			{
				if (mode == 0)
					sprintf(SymbolFile, "%s.shp", str3);
				else
					sprintf(SymbolFile, "%s__#.shp", str3);
			}

			sprintf(str, "%s\\%s", ExportDir2, SymbolFile);
			sprintf(str2, SC(26, "File %s\\%s exists, overwrite ?"), ExportDir2, SymbolFile);
			fp = -1;

			if ((mode == 1) || (FileExistsUTF8(str) != 0)
			        || (MessageBoxUTF8(LIBMANWindow, str2, SC(9, "Error"), MB_OKCANCEL) == IDOK))
			{
				if (mode == 1)
				{
					if (count == 0)
						CopyFileNameToTempMem(ExportDir2);

					CopyFileNameToTempMem(SymbolFile);
				}

				if ((fp = CheckForWritingAndOpenUTF8(str, length, LIBMANWindow)) > 0)
				{
					if ((fp2 = FileOpenReadOnlyUTF8(LibraryFile[ListNr])) < 0)
						return -1;

					FileSeek(fp2, LibNames[ListNr][cnt2].Pos);

					while (length > 0)
					{
						FileRead(fp2, Buf, min(length, BufSize), &result);

						if (result > 0)
						{
							FileWrite(fp, Buf, result, &res2);
							length -= result;
						}
						else
							length = 0;
					}

					count++;
					FileClose(fp2);
					FileClose(fp);
				}
			}

			ok = 1;
		}
	}

	return count;
}


// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 AddSymbols(int32 ListNr, int32 mode)
{
	LPSTR Dir, FileName, SearchStr;
	int32 res, count;
	char FileName2[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	int32 Stop = 0;

	count = 0;
	Dir = (LPSTR) TempMem;

	if (Dir[0] == 0)
		return -1;

	SearchStr = Dir;
	FileName = (LPSTR) TempMem;
	FileName += strlen(FileName) + 1;

	if (FileName[0] != 0)
		SearchStr += strlen(SearchStr) + 1;

	while (SearchStr[0] != 0)
	{
		if (FileName[0] != 0)
			sprintf(FileName2, "%s\\%s", Dir, FileName);
		else
			sprintf(FileName2, "%s", Dir);

		if (!Stop)
		{
//      MessageBoxUTF8(LIBMANWindow,FileName2,"Add symbol",MB_APPLMODAL|MB_OK);
			if (mode == 0)
				res = AddSymbolToLibrary(FileName2, ListNr, 1);
			else
				res = AddSymbolToLibrary(FileName2, ListNr, 4 + 1);

#if 0
			sprintf(str, "%d", res);
			MessageBoxUTF8(LIBMANWindow, str, "result", MB_APPLMODAL | MB_OK);
#endif

			switch (res)
			{
			case 0:
				count++;
				break;

			case -20:
				Stop = 1;
				break;
			}
		}

		if (mode == 1)
			DeleteFileUTF8(FileName2);

		FileName += strlen(FileName) + 1;
		SearchStr += strlen(SearchStr) + 1;
	}

	if (mode == 0)
	{
		if (count > 0)
		{
			if (LibraryMode == 0)
				sprintf(str2, SC(46, "%d symbols added/replaced for library %s"), count, LibraryFile[ListNr]);
			else
				sprintf(str2, SC(47, "%d geometries added/replaced for library %s"), count, LibraryFile[ListNr]);

			MessageBoxUTF8(LIBMANWindow, str2, SC(43, "Message"), MB_OK);
		}
	}

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 LoadSymbolFileNames(int32 ListNr)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING];

	str[0] = 0;
	str2[0] = 0;
	TempMem[0] = 0;
	TempMem[1] = 0;

	if (LibraryMode == 0)
		strcpy(str5, SC(27, "Symbol files"));
	else
		strcpy(str5, SC(28, "Geometry files"));

	if (LibraryMode == 0)
	{
		if (GetNewFileUTF8
		        (LIBMANWindow, LIBMANClass.hInstance, (char *) TempMem, SymbolImportDir, str5, NULL, str5, "sym", 6) == 0)
			return 1;
	}
	else
	{
		if (GetNewFileUTF8
		        (LIBMANWindow, LIBMANClass.hInstance, (char *) TempMem, SymbolImportDir, str5, NULL, str5, "shp", 6) == 0)
			return 1;
	}

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 EditSymbol(int32 ListNr)
{
	int32 cnt2, length, fp, fp2, result, res2, pos;
	char str[MAX_LENGTH_STRING * 5], SymbolFile[600], ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;

	if (ListNr == 0)
		SymbolsSelected = SendMessage(ListWindow1, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS + 1, (LPARAM) (LPINT) & LibIndex);
	else
		SymbolsSelected = SendMessage(ListWindow2, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS + 1, (LPARAM) (LPINT) & LibIndex);

	if (SymbolsSelected != 1)
		return -1;

	MakeBackup(ListNr);

	if (ListNr == 0)
		SendMessageUTF8(ListWindow1, LB_GETTEXT, LibIndex[0], (LPARAM) str);
	else
		SendMessageUTF8(ListWindow2, LB_GETTEXT, LibIndex[0], (LPARAM) str);

	cnt2 = 0;

	while ((cnt2 < Lib[ListNr].NrLibEntries) && (stricmp(LibNames[ListNr][cnt2].Text, str) != 0))
		cnt2++;

	if (cnt2 < Lib[ListNr].NrLibEntries)
	{
		length = LibNames[ListNr][cnt2].Length;

		if (LibraryMode == 0)
			sprintf(SymbolFile, "%s\\%s.sym", TempPath, str);
		else
			sprintf(SymbolFile, "%s\\%s.shp", TempPath, str);

		if (((fp2 = FileOpenReadOnlyUTF8(LibraryFile[ListNr])) > 0)
		        && ((fp = CheckForWritingAndOpenUTF8(SymbolFile, length, LIBMANWindow)) > 0))
		{
			FileSeek(fp2, LibNames[ListNr][cnt2].Pos);

			while (length > 0)
			{
				FileRead(fp2, Buf, min(length, BufSize), &result);

				if (result > 0)
				{
					FileWrite(fp, Buf, result, &res2);
					length -= result;
				}
				else
					length = 0;
			}

			FileClose(fp);
			FileClose(fp2);

			if (LibraryMode == 0)
			{
				sprintf(ExeFile, "%s\\sch.exe", ExePath);

				if (FileExistsUTF8(ExeFile) == 0)
				{
					if (SymbolToBeEditedPos[ListNr] < 20)
					{
						pos = SymbolToBeEditedPos[ListNr] + 100;

						if (ListNr == 1)
							pos += 100;

//            sprintf(str2,"%s\\sch.exe \"%s\" /e \"%s\" /a /w %x /z %d",ExePath,SymbolFile,ExePath,LIBMANWindow,pos);
						strcpy(SymbolToBeEdited[ListNr][SymbolToBeEditedPos[ListNr]], SymbolFile);
						SymbolToBeEditedPos[ListNr]++;

						sprintf(ExeParams, "\"%s\\sch.exe\" \"%s\" /e \"%s\" /a /w %x /z %d", ExePath, SymbolFile,
						        ExePath, (int32) LIBMANWindow, pos);
						StartupInfo.cb = sizeof(StartupInfo);
						StartupInfo.wShowWindow = SW_SHOW;
						CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);

//            WinExec(str2,SW_SHOW);
					}
				}
			}
			else
			{
				sprintf(ExeFile, "%s\\geom.exe", ExePath);

				if (FileExistsUTF8(ExeFile) == 0)
				{
					if (SymbolToBeEditedPos[ListNr] < 20)
					{
						pos = SymbolToBeEditedPos[ListNr] + 100;

						if (ListNr == 1)
							pos += 200;

//            sprintf(str2,"%s\\geom.exe \"%s\" /e \"%s\" /a /w %x /z %d",ExePath,SymbolFile,ExePath,LIBMANWindow,pos);
//            MessageBox(LIBMANWindow,SymbolFile,"edit geom file",MB_APPLMODAL+MB_OK);
//            MessageBoxUTF8(LIBMANWindow,SymbolFile,"edit geom file(2)",MB_APPLMODAL+MB_OK);
						strcpy(SymbolToBeEdited[ListNr][SymbolToBeEditedPos[ListNr]], SymbolFile);
						SymbolToBeEditedPos[ListNr]++;
						sprintf(ExeParams, "\"%s\\geom.exe\" \"%s\" /e \"%s\" /a /w %x /z %d", ExePath, SymbolFile,
						        ExePath, (int32) LIBMANWindow, pos);
						StartupInfo.cb = sizeof(StartupInfo);
						StartupInfo.wShowWindow = SW_SHOW;
						CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
//            WinExec(str2,SW_SHOW);
					}
				}
			}
		}
	}

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int32 ExitEditingSymbol(int32 SymbolNr)
{
	int32 ListNr2;
	char SymbolFile[MAX_LENGTH_STRING];

	if (SymbolNr < 200)
		ListNr2 = 0;
	else
		ListNr2 = 1;

	SymbolNr %= 100;
	strcpy(SymbolFile, SymbolToBeEdited[ListNr2][SymbolNr]);
	SymbolToBeEdited[ListNr2][SymbolNr][0] = 0;
	DeleteFileUTF8(SymbolFile);
	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************


int32 ReloadSymbolsIntoLibrary(void)
{
	int32 cnt, cnt2;
	char SymbolFile[MAX_LENGTH_STRING];

	for (cnt2 = 0; cnt2 < 1; cnt2++)
	{
		for (cnt = 0; cnt < SymbolToBeEditedPos[cnt2]; cnt++)
		{
			if (SymbolToBeEdited[cnt2][cnt][0] != 0)
			{
				strcpy(SymbolFile, SymbolToBeEdited[cnt2][cnt]);
				AddSymbolToLibrary(SymbolFile, cnt2, 2);
			}
		}
	}

	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void PopupMenu(int32 ListNr, int32 mode)
{
	HMENU PopUpMenu;

	if (ListNr == 0)
		SymbolsSelected = SendMessage(ListWindow1, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS + 1, (LPARAM) (LPINT) & LibIndex);
	else
		SymbolsSelected = SendMessage(ListWindow2, LB_GETSELITEMS, MAX_NR_LIB_SYMBOLS + 1, (LPARAM) (LPINT) & LibIndex);

	if (SymbolsSelected == 1)
	{
		if (mode == 1)
		{
			if (ListNr == 0)
				PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) ID_EDIT_SYMBOL1, (LPARAM) 0);
			else
				PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) ID_EDIT_SYMBOL2, (LPARAM) 0);

			return;
		}

		PopUpMenu = CreatePopupMenu();

		if (LibraryMode == 0)
		{
			if (ListNr == 0)
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_SYMBOL1, SC(29, "Edit symbol"));
			else
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_SYMBOL2, SC(29, "Edit symbol"));
		}
		else
		{
			if (ListNr == 0)
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_SYMBOL1, SC(30, "Edit geometry"));
			else
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_SYMBOL2, SC(30, "Edit geometry"));
		}

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(31, "Exit popup menu"));
		TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
		               RealWindow.top + MousePosY + 40, 0, LIBMANWindow, NULL);
		DestroyMenu(PopUpMenu);
	}

	if (SymbolsSelected > 1)
	{
	}
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

void CreateWindowObjects(HINSTANCE hInstance)
{
	int32 res;
	RECT NewRect;
	WCHAR Wstr[MAX_LENGTH_STRING];

	RectListWindow1.left = 10;
	RectListWindow1.top = 40;
	RectListWindow1.right = ListWindowWidth + RectListWindow1.left;
	RectListWindow1.bottom = ListWindowHeight + RectListWindow1.top;

	ListWindow1 =
	    CreateWindowExW(WS_EX_ACCEPTFILES, L"LISTBOX", L"",
	                    WS_CHILD | WS_BORDER | LBS_SORT | LBS_NOTIFY | WS_VSCROLL | WS_THICKFRAME |
//                               LBS_MULTIPLESEL|
	                    LBS_EXTENDEDSEL | LBS_DISABLENOSCROLL,
// LBS_WANTKEYBOARDINPUT
	                    RectListWindow1.left, RectListWindow1.top, ListWindowWidth, ListWindowHeight, LIBMANWindow,
	                    (HMENU) ID_LIST1, hInstance, NULL);


//  res=SendMessageUTF8(ListWindow1,LB_ADDSTRING,0,(LPARAM)"totx173_ä_поломки");

	ShowWindow(ListWindow1, SW_SHOW);
	GetClientRect(ListWindow1, &NewRect);
	RectListWindow1.right = NewRect.right - NewRect.left + RectListWindow1.left;
	RectListWindow1.bottom = NewRect.bottom - NewRect.top + RectListWindow1.top;

	RectListWindow2.left = ListWindowWidth + ButtonSizeX + 40;
//  RectListWindow2.left=460;
	RectListWindow2.top = 40;
	RectListWindow2.right = ListWindowWidth + RectListWindow1.left;
	RectListWindow2.bottom = ListWindowHeight + RectListWindow1.top;
	ListWindow2 =
	    CreateWindowExW(WS_EX_ACCEPTFILES, L"LISTBOX", L"LIST",
	                    WS_CHILD | WS_BORDER | LBS_SORT | LBS_NOTIFY | WS_VSCROLL | WS_THICKFRAME |
//                               LBS_MULTIPLESEL|
	                    LBS_EXTENDEDSEL | LBS_DISABLENOSCROLL, RectListWindow2.left, RectListWindow2.top,
	                    ListWindowWidth, ListWindowHeight, LIBMANWindow, (HMENU) ID_LIST2, hInstance, NULL);
	ShowWindow(ListWindow2, SW_SHOW);
	GetClientRect(ListWindow2, &NewRect);
	RectListWindow2.right = NewRect.right - NewRect.left + RectListWindow2.left;
	RectListWindow2.bottom = NewRect.bottom - NewRect.top + RectListWindow2.top;
//  Utf8ToUnicode("New lib и",Wstr,200);
	Utf8ToUnicode(SC(32, "New lib"), Wstr, 200);
	CreateNewLibButton1 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 0, RectListWindow1.bottom + 14,
	                  ButtonSizeX, ButtonSizeY, LIBMANWindow, (HMENU) ID_CREATE_LIB1, hInstance, NULL);

	ShowWindow(CreateNewLibButton1, SW_SHOW);
	Utf8ToUnicode(SC(33, "Open lib"), Wstr, 200);
	OpenLibButton1 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX, RectListWindow1.bottom + 14,
	                  ButtonSizeX, ButtonSizeY, LIBMANWindow, (HMENU) ID_OPEN_LIB1, hInstance, NULL);
	ShowWindow(OpenLibButton1, SW_SHOW);
	/*
	  SaveLibButton1 = CreateWindow(L"BUTTON","Save lib",WS_CHILD,
	                                RectListWindow1.left+ButtonStepX*2,RectListWindow1.bottom+10,
	                                ButtonSizeX,ButtonSizeY,
	                                LIBMANWindow,(HMENU)ID_SAVE_LIB1,hInstance,NULL);
	  ShowWindow(SaveLibButton1,SW_SHOW);
	*/
	Utf8ToUnicode(SC(34, "Close lib"), Wstr, 200);
	CloseLibButton1 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 2, RectListWindow1.bottom + 14,
	                  ButtonSizeX, ButtonSizeY, LIBMANWindow, (HMENU) ID_CLOSE_LIB1, hInstance, NULL);
	ShowWindow(CloseLibButton1, SW_SHOW);

	if (LibraryMode == 0)
	{
		Utf8ToUnicode(SC(35, "Import symbols"), Wstr, 200);
		ImportSymbolsButton1 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 1,
		                  RectListWindow1.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_IMPORT_SYMBOLS1, hInstance, NULL);
		ShowWindow(ImportSymbolsButton1, SW_SHOW);
		Utf8ToUnicode(SC(36, "Export symbols"), Wstr, 200);
		ExportSymbolsButton1 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 2,
		                  RectListWindow1.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_EXPORT_SYMBOLS1, hInstance, NULL);
		ShowWindow(ExportSymbolsButton1, SW_SHOW);
	}
	else
	{
		Utf8ToUnicode(SC(52, "Import geometries"), Wstr, 200);
		ImportSymbolsButton1 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 1,
		                  RectListWindow1.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_IMPORT_SYMBOLS1, hInstance, NULL);
		ShowWindow(ImportSymbolsButton1, SW_SHOW);
		Utf8ToUnicode(SC(53, "Export geometries"), Wstr, 200);
		ExportSymbolsButton1 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 2,
		                  RectListWindow1.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_EXPORT_SYMBOLS1, hInstance, NULL);
		ShowWindow(ExportSymbolsButton1, SW_SHOW);
	}

	Utf8ToUnicode(SC(37, "Deselect all"), Wstr, 200);
	DeselectAllButton1 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 0,
	                  RectListWindow1.bottom + ButtonStepY * 2 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
	                  (HMENU) ID_DESELECT_ALL1, hInstance, NULL);
	ShowWindow(DeselectAllButton1, SW_SHOW);
	Utf8ToUnicode(SC(58, "Select all"), Wstr, 200);
	SelectAllButton1 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 1,
	                  RectListWindow1.bottom + ButtonStepY * 2 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
	                  (HMENU) ID_SELECT_ALL1, hInstance, NULL);
	ShowWindow(SelectAllButton1, SW_SHOW);

	if (LibraryMode == 0)
	{
		Utf8ToUnicode(SC(38, "Delete symbols"), Wstr, 200);
		DeleteSymbolsButton1 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 0,
		                  RectListWindow1.bottom + ButtonStepY * 1 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_DELETE_SYMBOLS1, hInstance, NULL);
	}
	else
	{
		Utf8ToUnicode(SC(57, "Delete geometries"), Wstr, 200);
		DeleteSymbolsButton1 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow1.left + ButtonStepX * 0,
		                  RectListWindow1.bottom + ButtonStepY * 1 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_DELETE_SYMBOLS1, hInstance, NULL);
	}

	ShowWindow(DeleteSymbolsButton1, SW_SHOW);

	Utf8ToUnicode(SC(39, u8"Copy ►"), Wstr, 200);
//  Utf8ToUnicode(SC(39,"Copy ->"),Wstr,200);
	CopyButton1 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left - ButtonSizeX - 15, 120, ButtonSizeX, ButtonSizeY,
	                  LIBMANWindow, (HMENU) ID_COPY_SYMBOLS1, hInstance, NULL);
	ShowWindow(CopyButton1, SW_SHOW);
	Utf8ToUnicode(SC(40, u8"Move ►"), Wstr, 200);
//  Utf8ToUnicode(SC(40,"Move ->"),Wstr,200);
	MoveButton1 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left - ButtonSizeX - 15, 160, ButtonSizeX, ButtonSizeY,
	                  LIBMANWindow, (HMENU) ID_MOVE_SYMBOLS1, hInstance, NULL);
	ShowWindow(MoveButton1, SW_SHOW);


	Utf8ToUnicode(SC(32, "New lib"), Wstr, 200);
	CreateNewLibButton2 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 0, RectListWindow2.bottom + 14,
	                  ButtonSizeX, ButtonSizeY, LIBMANWindow, (HMENU) ID_CREATE_LIB2, hInstance, NULL);
	ShowWindow(CreateNewLibButton2, SW_SHOW);
	Utf8ToUnicode(SC(33, "Open lib"), Wstr, 200);
	OpenLibButton2 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX, RectListWindow2.bottom + 14,
	                  ButtonSizeX, ButtonSizeY, LIBMANWindow, (HMENU) ID_OPEN_LIB2, hInstance, NULL);
	ShowWindow(OpenLibButton2, SW_SHOW);
	/*
	  SaveLibButton2 = CreateWindow(L"BUTTON","Save lib",WS_CHILD,
	                                RectListWindow2.left+ButtonStepX*2,RectListWindow2.bottom+10,
	                                ButtonSizeX,ButtonSizeY,
	                                LIBMANWindow,(HMENU)ID_SAVE_LIB2,hInstance,NULL);
	  ShowWindow(SaveLibButton2,SW_SHOW);
	*/
	Utf8ToUnicode(SC(34, "Close lib"), Wstr, 200);
	CloseLibButton2 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 2, RectListWindow2.bottom + 14,
	                  ButtonSizeX, ButtonSizeY, LIBMANWindow, (HMENU) ID_CLOSE_LIB2, hInstance, NULL);
	ShowWindow(CloseLibButton2, SW_SHOW);

	if (LibraryMode == 0)
	{
		Utf8ToUnicode(SC(35, "Import symbols"), Wstr, 200);
		ImportSymbolsButton2 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 1,
		                  RectListWindow2.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_IMPORT_SYMBOLS2, hInstance, NULL);
	}
	else
	{
		Utf8ToUnicode(SC(52, "Import geometries"), Wstr, 200);
		ImportSymbolsButton2 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 1,
		                  RectListWindow2.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_IMPORT_SYMBOLS2, hInstance, NULL);
	}

	ShowWindow(ImportSymbolsButton2, SW_SHOW);

	if (LibraryMode == 0)
	{
		Utf8ToUnicode(SC(36, "Export symbols"), Wstr, 200);
		ExportSymbolsButton2 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 2,
		                  RectListWindow2.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_EXPORT_SYMBOLS2, hInstance, NULL);
	}
	else
	{
		Utf8ToUnicode(SC(53, "Export geometries"), Wstr, 200);
		ExportSymbolsButton2 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 2,
		                  RectListWindow2.bottom + ButtonStepY + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_EXPORT_SYMBOLS2, hInstance, NULL);
	}

	ShowWindow(ExportSymbolsButton2, SW_SHOW);
	Utf8ToUnicode(SC(37, "Deselect all"), Wstr, 200);
	DeselectAllButton2 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 0,
	                  RectListWindow2.bottom + ButtonStepY * 2 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
	                  (HMENU) ID_DESELECT_ALL2, hInstance, NULL);
	ShowWindow(DeselectAllButton2, SW_SHOW);
	Utf8ToUnicode(SC(58, "Select all"), Wstr, 200);
	SelectAllButton2 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 1,
	                  RectListWindow2.bottom + ButtonStepY * 2 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
	                  (HMENU) ID_SELECT_ALL2, hInstance, NULL);
	ShowWindow(SelectAllButton2, SW_SHOW);

	if (LibraryMode == 0)
	{
		Utf8ToUnicode(SC(38, "Delete symbols"), Wstr, 200);
		DeleteSymbolsButton2 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 0,
		                  RectListWindow2.bottom + ButtonStepY * 1 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_DELETE_SYMBOLS2, hInstance, NULL);
	}
	else
	{
		Utf8ToUnicode(SC(57, "Delete geometries"), Wstr, 200);
		DeleteSymbolsButton2 =
		    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left + ButtonStepX * 0,
		                  RectListWindow2.bottom + ButtonStepY * 1 + 14, ButtonSizeX, ButtonSizeY, LIBMANWindow,
		                  (HMENU) ID_DELETE_SYMBOLS2, hInstance, NULL);
	}

	ShowWindow(DeleteSymbolsButton2, SW_SHOW);

	Utf8ToUnicode(SC(41, u8"◄ Copy"), Wstr, 200);
//  Utf8ToUnicode(SC(41,"<- Copy"),Wstr,200);
	CopyButton2 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left - ButtonSizeX - 15, 220, ButtonSizeX, ButtonSizeY,
	                  LIBMANWindow, (HMENU) ID_COPY_SYMBOLS2, hInstance, NULL);
	ShowWindow(CopyButton2, SW_SHOW);
	Utf8ToUnicode(SC(42, u8"◄ Move"), Wstr, 200);
//  Utf8ToUnicode(SC(42,"<- Move"),Wstr,200);
	MoveButton2 =
	    CreateWindowW(L"BUTTON", Wstr, WS_CHILD, RectListWindow2.left - ButtonSizeX - 15, 260, ButtonSizeX, ButtonSizeY,
	                  LIBMANWindow, (HMENU) ID_MOVE_SYMBOLS2, hInstance, NULL);
	ShowWindow(MoveButton2, SW_SHOW);

	res = SendMessage(ListWindow1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(ListWindow2, WM_SETFONT, (WPARAM) NewFont, 1);


	res = SendMessage(CreateNewLibButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(OpenLibButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(SaveLibButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(CloseLibButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(ImportSymbolsButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(ExportSymbolsButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(DeleteSymbolsButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(DeselectAllButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(SelectAllButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(CopyButton1, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(MoveButton1, WM_SETFONT, (WPARAM) NewFont, 1);

	res = SendMessage(CreateNewLibButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(OpenLibButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(SaveLibButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(CloseLibButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(ImportSymbolsButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(ExportSymbolsButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(DeleteSymbolsButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(DeselectAllButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(SelectAllButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(CopyButton2, WM_SETFONT, (WPARAM) NewFont, 1);
	res = SendMessage(MoveButton2, WM_SETFONT, (WPARAM) NewFont, 1);

}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 MakeMenu()
{

#define MENU_ID       MF_ENABLED|MF_STRING

	LIBMANMenu = CreateMenu();
	LIBMANMenu1 = CreateMenu();

//	AppendMenuUTF8(LIBMANMenu, MF_ENABLED | MF_POPUP, (UINT) LIBMANMenu1, "File");
	AppendMenuUTF8(LIBMANMenu, MENU_ID, ID_FILE_EXIT, SC(3, "Exit")); //LIBMANMenu1
	AppendMenuUTF8(LIBMANMenu, MENU_ID, ID_ABOUT, SC(0, "About program"));
	SetMenu(LIBMANWindow, LIBMANMenu);
	return 0;
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

#if 1

void Help(LPSTR Topic, int32 mode)
{
	char str[MAX_LENGTH_STRING];

	sprintf(str, "%s\\libman.chm::/html/%s", ExePath, Topic);

	switch (mode)
	{
	case 0:
		HtmlHelp(LIBMANWindow, (LPCTSTR) str, HH_DISPLAY_TOPIC, 0);
		break;

	case 1:
		break;

	case 2:
		HtmlHelp(0, 0, HH_CLOSE_ALL, 0);
		break;

	case 3:
		HtmlHelp(LIBMANWindow, (LPCTSTR) str, HH_HELP_CONTEXT, (uint32) Topic);
		break;
	}
}

#else

void Help(int32 Command, int32 mode)
{
	char str[MAX_LENGTH_STRING];

	if (ExePath[0] == 0)
		return;

	sprintf(str, "%s\\libman.hlp", ExePath);

	switch (mode)
	{
	case 0:
		WinHelp(LIBMANWindow, (LPCTSTR) str, HELP_CONTEXT, Command);
		break;

	case 1:
		WinHelp(LIBMANWindow, (LPCTSTR) str, HELP_FINDER, 0);
		break;

	case 2:
		WinHelp(LIBMANWindow, (LPCTSTR) str, HELP_QUIT, 0);
		break;

	case 3:
		WinHelp(LIBMANWindow, (LPCTSTR) str, HELP_FINDER, 0);
		break;
	}
}

#endif

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

void LIBMANCommand(HWND hwnd, WPARAM WParam, int32 LParam)
{
	int32 cnt, res, NrFiles;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], Dir[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING];
	WCHAR Wstr[MAX_LENGTH_STRING];

	switch (WParam)
	{
	case ID_FILE_EXIT:
		SendMessage(LIBMANWindow, WM_CLOSE, 0, 0);
		break;

	case ID_OPEN_LIB1:
		LoadLib(0);
		SymbolToBeEditedPos[0] = 0;
		break;

	case ID_OPEN_LIB2:
		LoadLib(1);
		SymbolToBeEditedPos[1] = 0;
		break;

	case ID_CREATE_LIB1:
		MakeNewLib(0);
		SymbolToBeEditedPos[0] = 0;
		break;

	case ID_CREATE_LIB2:
		MakeNewLib(1);
		SymbolToBeEditedPos[0] = 1;
		break;

	case ID_CLOSE_LIB1:
		CloseLib(0);
		SymbolToBeEditedPos[0] = 0;
		break;

	case ID_CLOSE_LIB2:
		CloseLib(1);
		SymbolToBeEditedPos[1] = 0;
		break;

	case ID_IMPORT_SYMBOLS1:
		if (LibraryFile[0][0] != 0)
		{
			if (LoadSymbolFileNames(0) == 1)
				AddSymbols(0, 0);
		}
		else
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);

		break;

	case ID_IMPORT_SYMBOLS2:
		if (LibraryFile[1][0] != 0)
		{
			if (LoadSymbolFileNames(1) == 1)
				AddSymbols(1, 0);
		}
		else
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);

		break;

	case ID_EXPORT_SYMBOLS1:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		res = ExportSymbols(0, 0);

		if (res > 0)
		{
			if (LibraryMode == 0)
				sprintf(str2, SC(1, "%d symbols added to directory %s"), res, ExportDir);
			else
				sprintf(str2, SC(2, "%d geometries added to directory %s"), res, ExportDir);

			MessageBoxUTF8(LIBMANWindow, str2, SC(43, "Message"), MB_OK);
		}

		DeselectList1();
		break;

	case ID_EXPORT_SYMBOLS2:
		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		res = ExportSymbols(1, 0);

		if (res > 0)
		{
			if (LibraryMode == 0)
				sprintf(str2, SC(1, "%d symbols added to directory %s"), res, ExportDir);
			else
				sprintf(str2, SC(2, "%d geometries added to directory %s"), res, ExportDir);

			MessageBoxUTF8(LIBMANWindow, str2, SC(43, "Message"), MB_OK);
		}

		DeselectList2();
		break;

	case ID_DELETE_SYMBOLS1:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		DeleteSymbols(0);
		break;

	case ID_DELETE_SYMBOLS2:
		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		DeleteSymbols(1);
		break;

	case ID_DESELECT_ALL1:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		DeselectList1();
		break;

	case ID_DESELECT_ALL2:
		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		DeselectList2();
		break;

	case ID_SELECT_ALL1:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		SelectList1();
		break;

	case ID_SELECT_ALL2:
		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		SelectList2();
		break;

	case ID_COPY_SYMBOLS1:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		res = ExportSymbols(0, 1);
		AddSymbols(1, 1);
		DeselectList1();
		break;

	case ID_MOVE_SYMBOLS1:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		res = ExportSymbols(0, 1);
		AddSymbols(1, 1);
		DeleteSymbols(0);
		break;

	case ID_COPY_SYMBOLS2:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		res = ExportSymbols(1, 1);
		AddSymbols(0, 1);
		DeselectList2();
		break;

	case ID_MOVE_SYMBOLS2:
		if (LibraryFile[0][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		if (LibraryFile[1][0] == 0)
		{
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);
			return;
		}

		res = ExportSymbols(1, 1);
		AddSymbols(0, 1);
		DeleteSymbols(1);
		break;

	case ID_RIGHT_BUTTON_LIST1:
		PopupMenu(0, 0);
		res = 1;
		break;

	case ID_RIGHT_BUTTON_LIST2:
		PopupMenu(1, 0);
		res = 1;
		break;

	case ID_EDIT_SYMBOL1:
		EditSymbol(0);
		DeselectList1();
		break;

	case ID_EDIT_SYMBOL2:
		EditSymbol(1);
		DeselectList2();
		break;

	case ID_FILE_RELOAD_SYMBOLS:
	case ID_FILE_RELOADGEOMETRIES:
		ReloadSymbolsIntoLibrary();
		break;

	case ID_FILE_EXIT_TO_LIB_SCH:
	case ID_FILE_EXIT_TO_LIB_GEOM:
		ExitEditingSymbol(LParam);
		break;

	case WM_DROPFILES_LIST1:
	case WM_DROPFILES_LIST2:
		if (LibraryFile[WParam - WM_DROPFILES_LIST1][0] != 0)
		{
			NrFiles = DragQueryFileW((HANDLE) LParam, 0xFFFFFFFF, 0, 0);

			if (NrFiles > 0)
			{
				TempMemoryPos = 0;
				TempMem[0] = 0;
				TempMem[1] = 0;

				for (cnt = 0; cnt < NrFiles; cnt++)
				{
					res = DragQueryFileW((HANDLE) LParam, cnt, Wstr, 250);
					UnicodeToUtf8(Wstr, str, MAX_LENGTH_STRING - 50);

					if (cnt == 0)
					{
						GetDirFromFileName(Dir, str);
						CopyFileNameToTempMem(Dir);
					}

					GetFilePartFromFileName(FileName, str);
					CopyFileNameToTempMem(FileName);
				}

				AddSymbols(WParam - WM_DROPFILES_LIST1, 0);
				ok = 1;
			}

			DragFinish((HANDLE) LParam);
			res = 1;
		}
		else
			MessageBoxUTF8(LIBMANWindow, SC(8, "No library file open"), SC(9, "Error"), MB_OK);

		break;
#if 0

	case ID_HELP_CONTENTS:
		Help("contents.htm", 0);
		break;
#endif

	case ID_ABOUT:
		res =
		    DialogBox(LIBMANClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ABOUT), LIBMANWindow,
		              (DLGPROC) AboutDialogBody);
		break;
	}
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************


LRESULT FAR PASCAL LIBMANWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam)
{
	POINT pp;
	int32 x1, y1, res;
	POINT CurrentCursor;

	switch (Message)
	{
	case WM_COMMAND:
		res = 1;

		if (HIWORD(WParam) == 0)
			LIBMANCommand(Window, WParam, LParam);
		else
		{
			switch (LOWORD(WParam))
			{
			case ID_LIST1:		// List window1
				res = CBN_SETFOCUS;
				res = CBN_KILLFOCUS;

				switch (HIWORD(WParam))
				{
				case WM_DROPFILES:
					PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) WM_DROPFILES_LIST1, (LPARAM) WParam);
					break;

				case LBN_SELCHANGE:
					ok = 1;
					break;

				case LBN_DBLCLK:
					PopupMenu(0, 1);
//                res=GetCapture();
					res = 1;
					break;

				case WM_MOUSEMOVE:
					res = 1;
					break;

				}

				break;

			case ID_LIST2:		// List window2
				res = CBN_SETFOCUS;
				res = CBN_KILLFOCUS;

				switch (HIWORD(WParam))
				{
				case WM_DROPFILES:
					PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) WM_DROPFILES_LIST2, (LPARAM) WParam);
					break;

				case LBN_SELCHANGE:
					ok = 1;
					break;

				case LBN_DBLCLK:
					PopupMenu(1, 1);
					res = 1;
					break;
				}

				break;
			}
		}

		break;

	case WM_CREATE:
		break;

	case WM_PAINT:
		WindowPaint();
		break;

	case WM_VSCROLL:
		break;

	case WM_HSCROLL:
		break;

	case WM_SIZE:
		GetClientRect(LIBMANWindow, &ClientRect);
		GetWindowRect(LIBMANWindow, &RealWindow);
		pp.x = 0;
		pp.y = 0;
		ClientToScreen(LIBMANWindow, &pp);
//      ClientStartX=(int16)pp.x;
//      ClientStartY=(int16)pp.y;
		break;

	case WM_MOVE:
		GetWindowRect(LIBMANWindow, &RealWindow);
		pp.x = 0;
		pp.y = 0;
		ClientToScreen(LIBMANWindow, &pp);

//      ClientStartX=(int16)pp.x;
//      ClientStartY=(int16)pp.y;
	/*
	    case WM_CHAR:
	      break;
	    case WM_KEYDOWN:
	    case WM_SYSKEYDOWN:
	      break;
	    case WM_KEYUP:
	    case WM_SYSKEYUP:
	      break;
	*/
	case WM_MOUSEMOVE:
		MousePosX = LOWORD(LParam);
		MousePosY = HIWORD(LParam);
		GetCursorPos(&CurrentCursor);

		if (ScreenToClient(LIBMANWindow, &CurrentCursor))
		{
			x1 = CurrentCursor.x;
			y1 = CurrentCursor.y;
			res = 1;
		}

		MouseCount++;
		res = 1;
		break;

	/*
	    case WM_LBUTTONDBLCLK:
	    case WM_LBUTTONDOWN:
	      res=1;
	      break;
	    case WM_LBUTTONUP:
	      break;
	    case WM_RBUTTONDOWN:
	      ok=1;
	      break;
	    case WM_VKEYTOITEM:
	      res=1;
	      break;
	    case WM_CHARTOITEM:
	      res=1;
	      break;
	    case WM_RBUTTONUP:
	      break;
	    case WM_SETFOCUS:
	      break;
	    case WM_KILLFOCUS:
	      break;
	*/
	case WM_TIMER:
		TimerValue++;
		break;

	case WM_PARENTNOTIFY:
		x1 = LOWORD(LParam);
		y1 = HIWORD(LParam);
		MousePosX = x1;
		MousePosY = y1;

		if (((LOWORD(WParam)) & WM_RBUTTONDOWN) == WM_RBUTTONDOWN)
		{
			if (PointInRectangle(x1, y1, &RectListWindow1))
				PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) ID_RIGHT_BUTTON_LIST1, (LPARAM) NULL);

			if (PointInRectangle(x1, y1, &RectListWindow2))
				PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) ID_RIGHT_BUTTON_LIST2, (LPARAM) NULL);
		}

		res = 1;
		break;

	case WM_CLOSE:
		DestroyWindow(Window);
		return 0;

	case WM_DROPFILES:
//      DragQueryPoint((HANDLE)WParam,&MPoint);
		GetCursorPos(&CurrentCursor);

		if (ScreenToClient(LIBMANWindow, &CurrentCursor))
		{
			x1 = CurrentCursor.x;
			y1 = CurrentCursor.y;

			if (PointInRectangle(x1, y1, &RectListWindow1))
				PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) WM_DROPFILES_LIST1, (LPARAM) WParam);

			if (PointInRectangle(x1, y1, &RectListWindow2))
				PostMessage(LIBMANWindow, WM_COMMAND, (WPARAM) WM_DROPFILES_LIST2, (LPARAM) WParam);
		}

		break;

	case WM_QUIT:
		ok = 1;
		return 0;

	case WM_DESTROY:
		PostQuitMessage(0);
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


void DecodeParameters()
{
	int32 cnt, lengte, pos;


	pos = 0;
	lengte = 1000;

	for (cnt = 0; cnt < NrParams; cnt++)
	{
		if (Parameters[cnt].Option[0] != 0)
		{
			if (strnicmp(Parameters[cnt].Option, "g", lengte) == 0)
				LibraryMode = 1;

			if (strnicmp(Parameters[cnt].Option, "d", lengte) == 0)
				strcpy(ExportDir, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "e", lengte) == 0)
				strcpy(ExePath, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "u", lengte) == 0)
				strcpy(ProjectPath, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "o", lengte) == 0)
				ProjectActive = 1;
		}
		else
		{
			/*
			      if (GetFullPathName(Parameters[cnt].Parameter,150,EditFile,&FileName)==0) {
			        MessageBox(PCBWindow,str,"Error in opening file",MB_APPLMODAL|MB_OK);
			        EditFile[0]=0;
			      }
			*/
		}
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddLibmanLanguageString(int32 ID, LPSTR Text)
{
	int32 Length;

	Length = strlen(Text);

	if (Length == 0)
		return -1;

	if ((ID < 0) || (ID >= MaxNrLibmanNames))
		return -1;

	if (LibmanNamesPos + Length + 2 >= LibmanNamesBufSize)
		return -1;

	LibmanNamesId[ID] = (LPSTR) & LibmanNamesBuf[LibmanNamesPos];
	memmove(&LibmanNamesBuf[LibmanNamesPos], Text, Length + 1);
	LibmanNamesPos += Length + 1;
	TotalNrLibmanNames++;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddLibmanLanguageStrings(LPSTR FileName)
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
		    sprintf(str,SC(1000,"Can not read from language file %s, default language used"),FileName);
		    MessageBox(NULL,str,SC(1000,"Warning"),MB_APPLMODAL|MB_OK);
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
				//	sprintf(LanguageFileName, "%s\\%s", ExecutableDir, str);
					strcpy(LanguageFileName, "");
			}
		}
	}

	TextFileClose(fp);

// ********************************************************************************************************

	if ((LanguageFileName[0] == 0) || ((fp = TextFileOpenUTF8(LanguageFileName)) < 0))
	{
		/*
		    sprintf(str,SC(1000,"Can not read from language file %s"),LanguageFileName);
		    MessageBox(NULL,str,SC(1000,"System error"),MB_APPLMODAL|MB_OK);
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
					if (((BYTE) str3[cnt2] < (BYTE) ' ') && (str3[cnt2] != '\r') && (str3[cnt2] != '\t')
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
				AddLibmanLanguageString(StringNr, str3);
			}
		}
	}

	TextFileClose(fp);

	return 0;
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

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmd, int nCmdShow)
{
	MSG M;
	int32 length, res, KeySize;
	char str[600], str2[600], str3[600], *FileName, *env;
	uint32 EAX, EBX, ECX, EDX, FlagSSE2 = 0;
	char vendor[40];
	HKEY Key;
//	WCHAR Wstr[MAX_LENGTH_STRING];

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
		MessageBox(NULL, "This version of the library manager requires a CPU with SSE2 extensions", SC(9, "Error"),
		           MB_APPLMODAL | MB_OK);
		return 0;
	}

	GetTempPath(150, TempPath);
	length = strlen(TempPath);

	if (TempPath[length - 1] == '\\')
		TempPath[length - 1] = 0;

	strcpy(str, GetCommandLine());

	if (str[0] != '"')
		GetString(str, str2);
	else
		GetQuoteString(str, str2);

	strcpy(str3, str2);

	if (str3[0] != 0)
		GetFullPathName(str3, 250, str2, &FileName);

	GetLongPathNameA(str2, str2, 250);
	GetDirFromFileName(ExecutableDir, str2);

	WindowWidth = ListWindowWidth * 2 + ButtonSizeX + 65;
//  WindowWidth=805;
	WindowHeigth = 550;
	Buf = malloc(BufSize);

	if (hPrevInstance == 0)
	{
		LIBMANClass.style = CS_HREDRAW | CS_VREDRAW;
		LIBMANClass.lpfnWndProc = (WNDPROC) LIBMANWinProc;
		LIBMANClass.cbClsExtra = 0;
		LIBMANClass.cbWndExtra = 0;
		LIBMANClass.hInstance = hInstance;
		LIBMANClass.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(ICON1_1));
		LIBMANClass.hCursor = LoadCursor(0, IDC_ARROW);
		LIBMANClass.hbrBackground = GetStockObject(BLACK_BRUSH);
//    LIBMANClass.hbrBackground = COLOR_APPWORKSPACE+1;
		LIBMANClass.lpszMenuName = MAKEINTRESOURCE(IDR_MENU1);
		LIBMANClass.lpszClassName = "LIBMAN";

		if (!RegisterClass(&LIBMANClass))
			exit(255);
	}

	ClipCursor(NULL);

	BlackPen = CreatePen(PS_SOLID, 1, RGB_Black);
	GrayPen = CreatePen(PS_SOLID, 1, RGB_Gray);
	LightGrayPen = CreatePen(PS_SOLID, 1, RGB_LightGray);
	WhitePen = CreatePen(PS_SOLID, 1, RGB_White);
	EmptyPen = CreatePen(PS_NULL, 0, 0);

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


	OkToUseSharedMemory = 0;

	if ((SharedMemoryHandle = OpenFileMapping(FILE_MAP_WRITE, 0, MEMORYMAPPEDSTRING)))
	{
// The memory mapped file has already been created by another applicatoin (Design manager)
		SharedMemory = (BYTE *) MapViewOfFile(SharedMemoryHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

		if (SharedMemory != NULL)
		{
			OkToUseSharedMemory = 1;
			ProjectInfo = (ProjectInfoRecord *) SharedMemory;
		}

		ok = 1;
	}

//  ToetsMain();
	AllocateMemTemp(32768);
	GetCurrentDirectoryUTF8(200, EditPath);
	ExportDir[0] = 0;
	memset(&ExportDir, 0, sizeof(ExportDir));
	memset(&LibraryFile, 0, sizeof(LibraryFile));
	GetParameters(lpszCmd);
	DecodeParameters(lpszCmd);

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

	if (LanguagePath[0] != 0)
		sprintf(str, "%s\\LanguageLibman.txt", LanguagePath);
	else
		//sprintf(str, "%s\\LanguageLibman.txt", ExecutableDir);
		strcpy(str, "");

	if (AddLibmanLanguageStrings(str) == -2)
		return -1;

	if (ExePath[0] == 0)
	{
		strcpy(DefaultLibraryPath, EditPath);
		strcpy(ExePath, EditPath);
	}
	else
	{
		if (ProjectPath[0] == 0)
			strcpy(ProjectPath, ExePath);

		if (DefaultLibraryPath[0] == 0)
		{
			if (LibraryMode == 0)
			{
				sprintf(DefaultLibraryPath, "%s\\lib", ProjectPath);
				sprintf(DefaultSymbolPath, "%s\\sym", ProjectPath);
			}
			else
			{
				sprintf(DefaultLibraryPath, "%s\\shplib", ProjectPath);
				sprintf(DefaultSymbolPath, "%s\\shapes", ProjectPath);
			}
		}
	}

	if (LibraryMode == 0)
		sprintf(SymbolImportDir, "%s\\sym", ExePath);
	else
		sprintf(SymbolImportDir, "%s\\shapes", ExePath);

//  Utf8ToUnicode("Mibrary manager geometries_ä_Диап_поломки",Wstr,200);

//  Utf8ToUnicode("Mibrary manager geometries_ä_Диап_поломкиend_śč",Wstr,200);
	ScreenSizeX = GetSystemMetrics(SM_CXMAXIMIZED) - 10;
	ScreenSizeY = GetSystemMetrics(SM_CYMAXIMIZED) - 10;
	WindowStartX = (ScreenSizeX - WindowWidth) / 2;
	WindowStartY = (ScreenSizeY - WindowHeigth) / 2;
	GetStockObject(DEFAULT_GUI_FONT);
#if 1
	NewFont =
	    CreateFont(16, 0, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
	               DEFAULT_QUALITY, "Arial");
#else
	NewFont =
	    CreateFont(-1, 0, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
	               DEFAULT_QUALITY, "MS Sans Serif");
#endif
	LIBMANWindow =
	    CreateWindowExW(0, L"LIBMAN", L"", WS_OVERLAPPEDWINDOW, WindowStartX, WindowStartY, WindowWidth, WindowHeigth,
	                    HWND_DESKTOP, 0, hInstance, NULL);
	ShowWindow(LIBMANWindow, nCmdShow);
	MakeMenu();

#ifdef _WIN64
	if (LibraryMode == 0)
		SetClassLong(LIBMANWindow, GCLP_HICON, (LONG)LoadIcon(LIBMANClass.hInstance, MAKEINTRESOURCE(ICON1_1)));
	else
		SetClassLong(LIBMANWindow, GCLP_HICON, (LONG)LoadIcon(LIBMANClass.hInstance, MAKEINTRESOURCE(ICON1_2)));
#else
	if (LibraryMode == 0)
		SetClassLong(LIBMANWindow, GCL_HICON, (LONG) LoadIcon(LIBMANClass.hInstance, MAKEINTRESOURCE(ICON1_1)));
	else
		SetClassLong(LIBMANWindow, GCL_HICON, (LONG) LoadIcon(LIBMANClass.hInstance, MAKEINTRESOURCE(ICON1_2)));
#endif

	if (ProjectActive)
		InsertWindowInProject(LIBMANWindow, 0);

	CreateWindowObjects(hInstance);

	if (LibraryMode == 0)
		res = SetWindowTextUTF8(LIBMANWindow, SC(50, "Symbols library manager PCB Elegance"));
	else
		res = SetWindowTextUTF8(LIBMANWindow, SC(51, "Geometries library manager PCB Elegance"));

//  Utf8ToUnicode("Mibrary manager geometries_ä_Диап_поломкиend_śč",Wstr,200);
//  res=SetWindowTextW(LIBMANWindow,Wstr);
//  res=SetWindowTextUTF8(LIBMANWindow,"Mibrary manager geometries_ä_Диап_поломки");
//  res=SendMessageUTF8(LIBMANWindow ,WM_SETTEXT,0,(LPARAM)"Mibrary manager geometries_ä_Диап_поломки");

	TimerValue = 0;

	while (GetMessage(&M, 0, 0, 0))
	{
		TranslateMessage(&M);
		DispatchMessage(&M);
	}

	DeleteObject(NewFont);
//  Help(0,2);
	DeAllocateMemTemp();
	return 0;
}

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
