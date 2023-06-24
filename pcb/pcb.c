/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: pcb.c
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


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <windows.h>
#include <commctrl.h>

#include "calc.h"
#include "direct.h"
#include "calcdef.h"
#include "calc4.h"
#include "command.h"
#include "dialogs.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "files.h"
#include "files2.h"
#include "gerber.h"
#include "gerber2.h"
#include "graphics.h"
#include "graphics.h"
#include "help.h"
#include "import.h"
#include "line2.h"
#include "mainloop.h"
#include "memory.h"
#include "menus.h"
#include "nets.h"
#include "pcb.h"
#include "polygon.h"
#include "rect.h"
#include "resource.h"
#include "select3.h"
#include "stdio.h"
#include "fcntl.h"
#include "io.h"
#include "stdlib.h"
#include "string.h"
#include "time.h"
#include "toets.h"
#include "types.h"
#include "insdel.h"
#include "gateswap.h"
#include "winnt.h"
#include "wincon.h"
#include "stdlib.h"
#include "time.h"
#include "paint.h"
#include "owntime.h"
#include "calc3.h"
#include "params.h"
#include "uservar.h"
#include "own_process.h"
#include "version.h"


#define  _WIN32_WINNT       0x0501
//#define  WINVER             0x0501

#define WANT_GETLONGPATHNAME_WRAPPER
#define COMPILE_NEWAPIS_STUBS

//#define  CRC_MESSAGE

#define PCB_TOP                         5000
#define PCB_BOTTOM                      5500


#define  NR_MILLI_SECONDS_TIMER         100

#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL                   0x020A
#endif

#define  SCROLL_FACTOR                  1.0
#define  MEMORYMAPPEDSTRING             "MMFILE_PCB_ELEGANCE"

#define PCB_ELEG_ENVIRONMENT_STRING     "PCB_ELEG_ENVIRONMENT"
/*
#define  ScreenPosAbsCursorInit         10
#define  ScreenPosAbsGridCursorInit     170
#define  ScreenPosRelGridCursorInit     350
#define  ScreenPosInfoStrInit           590
*/
#define  ScreenPosAbsCursorInit         80
#define  ScreenPosAbsGridCursorInit     (ScreenPosAbsCursorInit+160)
#define  ScreenPosRelGridCursorInit     (ScreenPosAbsGridCursorInit+180)
#define  ScreenPosInfoStrInit           (ScreenPosRelGridCursorInit+240)

HCURSOR CurrentCursor;
HPALETTE OldPalette;
HCURSOR OldCursor, Cursor1;
HWND PCBWindow, DialogWindow, HorScrollBar, hwndHBar, hwndVBar;
HDC DialogDisplay, OutputDisplay2;
HRSRC FontResourceHandle;
HGLOBAL FontGlobal;
HBITMAP ViewBitmap;
uint8 *FontData;

PAINTSTRUCT PS;
RECT RealWindow, ClientRect, UpdateRect, OldClientRect, OldRealWindow2, RealWindow2, OldRealWindow;
HRGN EditingRegion, TempRegion;

int32 PCBCursorActive;
int32 TotalExit;
int32 FocusedAgain = 1;
int32 FirstPaintMode = 1;
int32 MakeNewDesign;
int32 DebugPaint;
int32 PaintBecauseOfResize;
int32 TimerUpdateNetlist;
int32 WindowMaximized;
int32 WindowMinimized;
int32 CompMemoryError;
int32 FileCheck1 = 1234;
int32 Counter0;
int32 ViewFullScreenMode;
int32 SpecialDebugFile;
int32 RedrawAbsPosStrCnt;
int32 OkToSpecialDrawing;

ATOM LayoutEditorAtom, TestAtom;
//__int64                     CurrentFrequency,Counter0;
int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY, ScreenSizeX, ScreenSizeY, ScreenStartX,
      ScreenStartY, ScrollBarSizeX, ScrollBarSizeY, HeightScrollBar, WidthScrollBar, StartTimerValue, ok,
      ScreenPosAbsCursor, ScreenPosAbsGridCursor, ScreenPosRelGridCursor, ScreenPosInfoStr;

int32 HeightInfoBar = 21;
int32 WidthButtonBar = 32;
int32 PaintBecauseOfResizeMode;
int32 OldWindowWidth;
int32 TestCounter;
int32 TestCounter2;
int32 ZoomCounter;
int32 TimerValue;
int32 NrPaintMessages;
int32 WaitForPaint;
int32 TotalNrPcbNames;
int32 CheckReference;
int32 CheckPartNr;
int32 MousePosOldX;
int32 MousePosOldY;
int32 ViewPixelsX;
int32 ViewPixelsY;

UINT TimerObject, ClipID2, ClosingWindowMessage, ClipBoardLayoutTracesViasID, ClipBoardLayoutComponentsID;

UINT TimerIdentifier = 0x12345678;

char IniFile[MAX_LENGTH_STRING];
char StartDir[MAX_LENGTH_STRING];
char ExecutableDir[MAX_LENGTH_STRING];
char PcbExecutable[MAX_LENGTH_STRING];
char PcbExecutableName[MAX_LENGTH_STRING];
char MessageStr[MAX_LENGTH_STRING];
char SearchForReferenceString[MAX_LENGTH_STRING];
char SearchForPartNrString[MAX_LENGTH_STRING];
char ExecuteCommands[MAX_LENGTH_STRING];
WCHAR StartDirW[MAX_LENGTH_STRING];
LPTSTR CursorType = IDC_ARROW;
char LanguagePath[MAX_LENGTH_STRING], UserIniFile[MAX_LENGTH_STRING], UserIniFilePath[MAX_LENGTH_STRING];
//LPTSTR                      CursorType=IDC_CROSS;

ProjectInfoRecord *ProjectInfo = NULL;
uint8 *SharedMemory;
HANDLE *SharedMemoryHandle;
int32 OkToUseSharedMemory;
int32 ProjectActive;
int32 ProjectIndexNr = -1;

extern int32 ButtonSizeX, ButtonSizeY, NrCachePolygonObjectsUpdate;
extern int32 NrButtons, CurrentButtonNr, AreafillDrawMode, PrintToBitmap;
extern HDC OutputDisplay;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;

extern COLORREF BackGroundColor;
extern HGDIOBJ BackGroundBrush;
extern int32 Printing, ReverseY;


OSVERSIONINFO OSInfo;

WNDCLASS PCBClass = { CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS,
                      (WNDPROC) PCBWinProc,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      "PCB"
                    };


uint8 CodedPcbExecutableName[128] = {
	0xA1, 0x14, 0xED, 0x9F, 0xA1, 0xBA, 0x3F, 0xB6,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13,
	0xC3, 0x1B, 0xD8, 0x97, 0x0B, 0x38, 0xBD, 0x13
};

typedef int32 int32Array[1000];

typedef struct
{
	uint8 SearchIdent[16];
	char TimeString[32];
	int32 ProgramVersion;
	int32 ProgramBuild;
	int32 Options;
} ProgramInfoRecord;

#pragma pack(1)

static ProgramInfoRecord ProgramInfo = { {
		0xe2, 0x03, 0x29, 0x6e, 0x74, 0x5f, 0x9a, 0x3b,
		0xe6, 0x81, 0xc8, 0x40, 0xdd, 0x62, 0xb5, 0x1f
	},
	VER_DATE_STR,
	VER_VERSION,
	VER_BUILD,
#ifndef SSE2
	0,
#else
	1,
#endif
};

#pragma pack()


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void WriteIniFile(void);

extern void DialogResources(int32 DialogResourceValue); 

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
	int32 cnt, PID;
	DWORD PID2 = 0;
	HWND DesignWindow;

	if (ProjectInfo == NULL)
		return;

	cnt = 0;

	while ((cnt < 32) && (ProjectInfo->FileTypes[cnt] != 0))
		cnt++;

	if (cnt == 32)
		return;

	ProjectIndexNr = cnt;
	ProjectInfo->FileTypes[cnt] = 4;
	ProjectInfo->WindowHandles[cnt] = Window;
	DesignWindow = GetDesignManagersWindow(0);

	if (DesignWindow)
	{
		PID = GetWindowThreadProcessId(DesignWindow, &PID2);

		if (PID2)
			AllowSetForegroundWindow(PID2);
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
	SetCursor(LoadCursor(0, CursorType));
	CurrentCursor = GetCursor();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void WindowCreate()
{
	ClipCursor(NULL);
	Created = 1;
	PCBCursorActive = 0;
	SetScrollRange(PCBWindow, SB_HORZ, -5000, 5000, 1);
	SetScrollRange(PCBWindow, SB_VERT, -5000, 5000, 1);
	SetScrollPos(PCBWindow, SB_HORZ, 0, 1);
	SetScrollPos(PCBWindow, SB_VERT, 0, 1);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ResetAllObjects()
{
	int32 count, cnt, cnt2, Layer, MemPos, ok, NrBoardOutlineObjects, MemSize, AddedSpace, CompSize, CompMemSize,
	      NrPins, PinsMemPos, MemPosComp, lengte2, StopCnt, TextRotation, Mirror, AreaFillCorrupt, ShapeNr, AreaFillError;
	uint8 *CompPos1, *CompPos2;
	double MinX, MinY, MaxX, MaxY;
	DesignRecord OldDesign;
	TraceRecord *Trace;
	ViaRecord *Via;
	ShapeRecord *Shape = NULL;
	NetRecord *Net;
	CompRecord *Comp, *Comp2;
	CompPinRecord *CompPin;
	ConnectionsRecord *Connection;
	AreaFillRecord *AreaFill;
	ObjectLineRecord *ObjectLine, NewObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

//  SetBoardPosComps();
//  ViewWholeDesign(0);

	memmove(&OldDesign, &Design, sizeof(DesignRecord));
	NrBoardOutlineObjects = 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			Trace->Info &= ~(OBJECT_SELECTED | OBJECT_HIGHLITED);

			if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);
				Trace->Info |= Net->Info & OBJECT_HIGHLITED;
			}

			if (Trace->ThickNess < 100.0)
				Trace->ThickNess = 2000.0;

			if (Trace->Length < 0.0)
				Trace->Length = -Trace->Length;

			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			Trace->Info &= ~(OBJECT_SELECTED | OBJECT_HIGHLITED);

			if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);
				Trace->Info |= Net->Info & OBJECT_HIGHLITED;
			}

			if (Trace->ThickNess < 100.0)
				Trace->ThickNess = 2000.0;

			if (Trace->Length < 0.0)
				Trace->Length = -Trace->Length;

			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			Trace->Info &= ~(OBJECT_SELECTED | OBJECT_HIGHLITED);

			if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);
				Trace->Info |= Net->Info & OBJECT_HIGHLITED;
			}

			if (Trace->ThickNess < 100.0)
				Trace->ThickNess = 2000.0;

			if (Trace->Length < 0.0)
				Trace->Length = -Trace->Length;

			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			Trace->Info &= ~(OBJECT_SELECTED | OBJECT_HIGHLITED);

			if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);
				Trace->Info |= Net->Info & OBJECT_HIGHLITED;
			}

			if (Trace->ThickNess < 100.0)
				Trace->ThickNess = 2000.0;

			if (Trace->Length < 0.0)
				Trace->Length = -Trace->Length;

			Trace->DeleteNr = 0;
			Trace->AddNr = 0;
		}

	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		Via->DeleteNr = 0;
		Via->AddNr = 0;
		Via->Layer = -1;
		Via->Info &= ~(OBJECT_SELECTED | OBJECT_HIGHLITED);

		if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
		{
			Net = &((*Nets)[Via->NetNr]);
			Via->Info |= Net->Info & OBJECT_HIGHLITED;

			if (Via->ThickNess < 100.0)
				Via->ThickNess = 2000.0;

			if (Via->DrillThickNess < 100.0)
				Via->DrillThickNess = 1000.0;
		}
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (Net->Name[0] != 0)
		{
			Net->Info &= CONNECTIONS_NOT_VISIBLE | CONNECTIONS_DISABLED | OBJECT_HIGHLITED;
			Net->NrPins = CountNrPinsNet(cnt);

			if (Net->NrPins < 1)
				ok = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		Connection->DeleteNr = 0;
		Connection->AddNr = 0;
		Connection->Info = 0;

		if ((Connection->NetNr >= 0) && (Connection->NetNr < Design.NrNets))
		{
			Net = &((*Nets)[Connection->NetNr]);
			Connection->Info |= Net->Info & (CONNECTIONS_NOT_VISIBLE | CONNECTIONS_DISABLED | OBJECT_HIGHLITED);
		}

	}

	VisibleMaxX = -10000e5;
	VisibleMaxY = -10000e5;
	VisibleMinX = 10000e5;
	VisibleMinY = 10000e5;

// ********************************************************************************************************
// ********************************************************************************************************

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->DeleteNr = 0;
		Comp->AddNr = 0;
		Comp->Info &= 0x00ff;
		Comp->TextVisibility &= ~(16 + 1);
		ShapeNr = (int32) Comp->ShapeNr;

		if (ShapeNr != -1)
		{
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
//      Comp->NrPins=Shape->NrPins;
		}

#ifdef _DEBUG

		if (stricmpOwn(Comp->Name, "T16") == 0)
			ok = 1;

		if (cnt == 161)
			ok = 1;

#endif

		if (cnt < Design.NrComps - 1)
			Comp2 = (CompRecord *) & (CompsMem[(*Comps)[cnt + 1]]);
		else
			Comp2 = (CompRecord *) & (CompsMem[Design.CompsMem]);

		Comp2->TextVisibility &= ~(0x88);
		Comp2->TextVisibility |= (Comp2->CompMode & 8) * 0x11;

		CompSize = (uint32) Comp2 - (uint32) Comp;
		NrPins = (CompSize - sizeof(CompRecord)) / 4;

		if (NrPins < Comp->NrPins)
		{
			CompMemoryError = 1;
			AddedSpace = (Comp->NrPins - NrPins) * 4;

			if (cnt < Design.NrComps - 1)
			{
				CompMemSize = Design.CompsMem - (*Comps)[cnt + 1];
				CompPos1 = (uint8 *) Comp2;
				CompPos2 = CompPos1 + AddedSpace;
				memmove(CompPos2, CompPos1, CompMemSize);
			}

			Design.CompsMem += AddedSpace;

			for (cnt2 = cnt + 1; cnt2 < Design.NrComps; cnt2++)
				(*Comps)[cnt2] += AddedSpace;

			MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
			PinsMemPos = MemPosComp + sizeof(CompRecord);

			for (cnt2 = 0; cnt2 < Comp->NrPins; cnt2++)
			{
				CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				CompPin->NetNr = -1;
				PinsMemPos += sizeof(CompPinRecord);
			}
		}

		ok = 1;

		if (Comp->Rotation > 359.9)
			Comp->Rotation -= 360.0;

		if (Comp->Rotation < 0.0)
			Comp->Rotation += 360.0;

		SetBoardPosComp(Comp, 0);

		VisibleMaxX = max(VisibleMaxX, Comp->BoardPosMaxX);
		VisibleMinX = min(VisibleMinX, Comp->BoardPosMinX);
		VisibleMaxY = max(VisibleMaxY, Comp->BoardPosMaxY);
		VisibleMinY = min(VisibleMinY, Comp->BoardPosMinY);

	}

	VisibleMaxX = max(VisibleMaxX, Design.BoardOriginX + Design.BoardWidth);
	VisibleMinX = min(VisibleMinX, Design.BoardOriginX);
	VisibleMaxY = max(VisibleMaxY, Design.BoardOriginY + Design.BoardHeight);
	VisibleMinY = min(VisibleMinY, Design.BoardOriginY);

// ********************************************************************************************************
// ********************************************************************************************************


	AreaFillError = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
		AreaFill->DeleteNr = 0;
		AreaFill->AddNr = 0;
		AreaFill->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
		AreaFillCorrupt = 0;
		AreaPos = (uint8 *) AreaFill;
		count = sizeof(AreaFillRecord);
		MemSize = sizeof(AreaFillRecord);

		if (AreaFill->NrPolygons < 1)
			AreaFill->NrPolygons = 1;

		DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

		if (CheckPolygon(DrawPolygon) == 0)
		{
			AreaFill->Info = OBJECT_NOT_VISIBLE;
			DataBaseChanged = 1;
		}

		PolygonPos = (uint8 *) DrawPolygon;
		StopCnt = -1;

		for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
		{
			if (MemSize < AreaFill->MemSize)
			{
				count = DrawPolygon->NrVertices;
#ifdef _DEBUG

				if (count == 131072)
					ok = 1;

#endif
				MemSize += MemSizePolygon(DrawPolygon);
				SetMinMaxPolygon(DrawPolygon, 0);
				DrawPolygon->PolygonType &= ~2;
				DrawPolygon->Special.Test = 0;
				DrawPolygon->Clearance = 0.0;

				if (MemSize >= AreaFill->MemSize)
					StopCnt = cnt2 + 1;
				else
				{
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;
				}
			}
			else
			{
				ok = 1;
				AreaFill->Info |= OBJECT_NOT_VISIBLE;
			}

			if (MemSize > MaxAreaFillMemory - (*AreaFills)[cnt])
			{
				AreaFill->Info = OBJECT_NOT_VISIBLE;
				DataBaseChanged = 1;
				ok = 1;
			}
		}

		if (AreaFill->NrPolygons > StopCnt)
		{
			AreaPos = (uint8 *) AreaFill;
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
			DrawPolygon->PolygonType |= 2;
			AreaFillError = 1;
			AreaFill->NrPolygons = StopCnt;
		}

		MinX = 1000000000.0;
		MinY = 1000000000.0;
		MaxX = -1000000000.0;
		MaxY = -1000000000.0;

		count = AreaFill->NrVerticesStartPolygon;

		for (cnt2 = 0; cnt2 < count; cnt2++)
		{
			MinX = min(MinX, AreaFill->StartPolygon[cnt2].x);
			MinY = min(MinY, AreaFill->StartPolygon[cnt2].y);
			MaxX = max(MaxX, AreaFill->StartPolygon[cnt2].x);
			MaxY = max(MaxY, AreaFill->StartPolygon[cnt2].y);
		}

		AreaFill->minx = MinX;
		AreaFill->miny = MinY;
		AreaFill->maxx = MaxX;
		AreaFill->maxy = MaxY;
#ifdef _DEBUG

		if ((MaxX - MinX == 0.0) || (MaxY - MinY == 0.0))
			ok = 1;

#endif
	}

	if (AreaFillError)
		MessageBoxOwn(PCBWindow, SC(943, "Error in areafills"), SC(24, "Error"), MB_APPLMODAL | MB_OK);

// ********************************************************************************************************
// ********************************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
		ObjectLine->DeleteNr = 0;
		ObjectLine->AddNr = 0;
		ObjectLine->Info = 0;

		switch (ObjectLine->Layer)
		{
		case PCB_BOTTOM:
			ObjectLine->Layer = 0;
			break;

		case PCB_TOP:
			ObjectLine->Layer = Design.NrBoardLayers - 1;
			break;
		}

		if (ObjectLine->Layer == BOARD_OUTLINE_LAYER)
			NrBoardOutlineObjects++;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);
		ObjectRect->DeleteNr = 0;
		ObjectRect->AddNr = 0;
		ObjectRect->Info &= OBJECT_FILLED;

		switch (ObjectRect->Layer)
		{
		case PCB_BOTTOM:
			ObjectRect->Layer = 0;
			break;

		case PCB_TOP:
			ObjectRect->Layer = Design.NrBoardLayers - 1;
			break;
		}

		if (ObjectRect->Layer == BOARD_OUTLINE_LAYER)
			NrBoardOutlineObjects++;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ObjectArc->DeleteNr = 0;
		ObjectArc->AddNr = 0;
		ObjectArc->Info &= OBJECT_FILLED;

		if ((ObjectArc->Info & OBJECT_FILLED) == 0)
		{
			if ((ObjectArc->StartDiffX == 0.0) && (ObjectArc->StartDiffY == 0.0))
			{
				ObjectArc->StartDiffX = 1e6;
				ObjectArc->StartDiffY = 1e5;
			}

			if ((ObjectArc->EndDiffX == 0.0) && (ObjectArc->EndDiffY == 0.0))
			{
				ObjectArc->EndDiffX = -1e5;
				ObjectArc->EndDiffY = -1e6;
			}
		}

		switch (ObjectArc->Layer)
		{
		case PCB_BOTTOM:
			ObjectArc->Layer = 0;
			break;

		case PCB_TOP:
			ObjectArc->Layer = Design.NrBoardLayers - 1;
			break;
		}

		if (ObjectArc->Layer == BOARD_OUTLINE_LAYER)
			NrBoardOutlineObjects++;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);
		ObjectText->DeleteNr = 0;
		ObjectText->AddNr = 0;
		ObjectText->Info = 0;
	}

// ********************************************************************************************************
// ********************************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);
		ObjectText2->DeleteNr = 0;
		ObjectText2->AddNr = 0;
		ObjectText2->Info = 0;

		switch (ObjectText2->Layer)
		{
		case PCB_BOTTOM:
			ObjectText2->Layer = 0;
			break;

		case PCB_TOP:
			ObjectText2->Layer = Design.NrBoardLayers - 1;
			break;
		}

		if (ObjectText2->Rotation > 359.9)
			ObjectText2->Rotation -= 360.0;

		if (ObjectText2->Rotation < 0.0)
			ObjectText2->Rotation += 360.0;
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		ObjectPolygon->DeleteNr = 0;
		ObjectPolygon->AddNr = 0;
		ObjectPolygon->Info = 0;

		switch (ObjectPolygon->Layer)
		{
		case PCB_BOTTOM:
			ObjectPolygon->Layer = 0;
			break;

		case PCB_TOP:
			ObjectPolygon->Layer = Design.NrBoardLayers - 1;
			break;
		}
	}

// ********************************************************************************************************
// ********************************************************************************************************

	if (strcmp(Design.Identification, PCBCode) == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			ObjectText = &((*ObjectTexts)[cnt]);
			TextRotation = (ObjectText->TextMode >> 8) & 0x03;
			Mirror = (ObjectText->TextMode & 0x10) >> 4;
			lengte2 = strlen(ObjectText->Text);
#ifdef _DEBUG

			if (Mirror == 1)
				ok = 1;

#endif

			if (Mirror == 0)
			{
				switch (TextRotation)
				{
				case 1:
					break;

				case 2:
					ObjectText->TextMode &= ~0x300;
					ObjectText->X -= (float) (lengte2 * ObjectText->FontHeight * DefFontSize * 0.9);
					ObjectText->Y -= (float) (ObjectText->FontHeight * 1.0 * DefFontSize);
					ObjectText->X += (float) (ObjectText->FontHeight * 0.2 * DefFontSize);
					break;

				case 3:
					ObjectText->TextMode &= ~0x300;
					ObjectText->TextMode |= 0x100;
					ObjectText->Y -= (float) (lengte2 * ObjectText->FontHeight * DefFontSize * 0.9);
					ObjectText->X += (float) (ObjectText->FontHeight * 1.0 * DefFontSize);
					ObjectText->Y += (float) (ObjectText->FontHeight * 0.2 * DefFontSize);
					break;
				}
			}
			else
			{
				switch (TextRotation)
				{
				case 1:
					ObjectText->X -= (float) (ObjectText->FontHeight * 0.1 * DefFontSize);
					ObjectText->Y -= (float) (ObjectText->FontHeight * 0.1 * DefFontSize);
					ObjectText->TextMode &= ~0x300;
					ObjectText->TextMode |= 0x300;
					break;

				case 2:
					ObjectText->TextMode &= ~0x300;
					ObjectText->X += (float) (lengte2 * ObjectText->FontHeight * DefFontSize * 0.9);
					ObjectText->Y -= (float) (ObjectText->FontHeight * 1.0 * DefFontSize);
					ObjectText->X -= (float) (ObjectText->FontHeight * 0.1 * DefFontSize);
					break;

				case 3:
					ObjectText->TextMode &= ~0x300;
					ObjectText->TextMode |= 0x300;
					ObjectText->Y += (float) (lengte2 * ObjectText->FontHeight * DefFontSize * 0.9);
					ObjectText->X -= (float) (ObjectText->FontHeight * 1.0 * DefFontSize);
					ObjectText->Y -= (float) (ObjectText->FontHeight * 0.2 * DefFontSize);
					break;
				}
			}
		}
	}

#ifdef _DEBUG

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (cnt == 888)
			ok = 1;
	}

#endif

// ********************************************************************************************************
// ********************************************************************************************************
	/*
	  if (strcmp(Design.Identification,PCBCode2)==0) {
	    for (cnt=0;cnt<Design.NrObjectLines;cnt++) {
	      ObjectLine=&((*ObjectLines)[cnt]);
	      ObjectLine->NetNr=-1;
	    }
	    for (cnt=0;cnt<Design.NrObjectRects;cnt++) {
	      ObjectRect=&((*ObjectRects)[cnt]);
	      ObjectRect->NetNr=-1;
	    }
	    for (cnt=0;cnt<Design.NrObjectArcs;cnt++) {
	      ObjectArc=&((*ObjectArcs)[cnt]);
	      ObjectArc->NetNr=-1;
	    }
	  }
	*/
// ********************************************************************************************************
// ********************************************************************************************************
	SetLayerColors();

	if (NrBoardOutlineObjects == 0)
	{
		NrObjects = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 3);
		}

		if (NrObjects == 0)
		{
			memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
			NewObjectLine.Layer = BOARD_OUTLINE_LAYER;
			NewObjectLine.LineThickNess = Design.SilkScreenWidth;

			if (InRange(NewObjectLine.LineThickNess, 0.0))
				NewObjectLine.LineThickNess = (8.0 * 2540);

			NewObjectLine.X1 = OldDesign.BoardOriginX;
			NewObjectLine.Y1 = OldDesign.BoardOriginY;
			NewObjectLine.X2 = OldDesign.BoardOriginX;
			NewObjectLine.Y2 = OldDesign.BoardOriginY + OldDesign.BoardHeight;
			AddObjectLine(&NewObjectLine);
			memmove(&NewObjectLine.X1, &NewObjectLine.X2, 2 * sizeof(NewObjectLine.X1));
			NewObjectLine.X2 = OldDesign.BoardOriginX + OldDesign.BoardWidth;
			NewObjectLine.Y2 = OldDesign.BoardOriginY + OldDesign.BoardHeight;
			AddObjectLine(&NewObjectLine);
			memmove(&NewObjectLine.X1, &NewObjectLine.X2, 2 * sizeof(NewObjectLine.X1));
			NewObjectLine.X2 = OldDesign.BoardOriginX + OldDesign.BoardWidth;
			NewObjectLine.Y2 = OldDesign.BoardOriginY;
			AddObjectLine(&NewObjectLine);
			memmove(&NewObjectLine.X1, &NewObjectLine.X2, 2 * sizeof(NewObjectLine.X1));
			NewObjectLine.X2 = OldDesign.BoardOriginX;
			NewObjectLine.Y2 = OldDesign.BoardOriginY;
			AddObjectLine(&NewObjectLine);
		}
	}

	if (Design.BoardOutlineWidth == 0.0)
		Design.BoardOutlineWidth = Design.SilkScreenWidth;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void WindowDestroy()
{
	PostQuitMessage(0);
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
			OutputDisplay = BeginPaint(PCBWindow, &PS);
			SetArcDirection(OutputDisplay, AD_COUNTERCLOCKWISE);
		}
		else
			OutputDisplay = GetDC(PCBWindow);

//    SetBkMode(OutputDisplay,OPAQUE);
		SetBkMode(OutputDisplay, TRANSPARENT);
		SetROP2(OutputDisplay, R2_COPYPEN);
//    SaveFont = SelectObject(WindowsDisplay, UserFont);
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
			OutputDisplay2 = BeginPaint(PCBWindow, &PS);
			SetArcDirection(OutputDisplay2, AD_COUNTERCLOCKWISE);
		}
		else
			OutputDisplay2 = GetDC(PCBWindow);

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

void StartDrawingEditingWindow(int32 BufferMode)
{
	RECT ClipRect;

	BufferMode &= BM_Mask;

	switch (BufferMode)
	{
	    case BM_DirectToScreen:
			InitDeviceContext();
		break;
	
	    case BM_MultiStart:
	    case BM_DoubleBuffer:
			InitDeviceContext2();
		    // copy screen to buffer
		    BitBlt(OutputDisplay, 0, 0, ViewPixelsX, ViewPixelsY, OutputDisplay2, 0, 0, SRCCOPY);
		break;

	    case BM_MultiContinue:
	    case BM_MultiEnd:
		//return;
		break;
	}

	EditingRegion = CreateRectRgn(DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY);
	SelectClipRgn(OutputDisplay, EditingRegion);
	GetClipBox(OutputDisplay, &ClipRect);
	SetROP2(OutputDisplay, R2_COPYPEN);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DoneDeviceContext2()
{
	int32 res;

	if (DCInUse)
	{
		OutputDebugString("EndDrawingEditingWindow: BM_MultiEnd\n");
        //res=GetDIBits(OutputDisplay,ViewBitmap,0,ViewPixelsY,TempMem,BitmapInfo,DIB_RGB_COLORS);
		BitBlt(OutputDisplay2, 0, 0, ViewPixelsX, ViewPixelsY, OutputDisplay, 0, 0, SRCCOPY);
		GdiFlush();

		if (ViewBitmap)
			DeleteObject(ViewBitmap);

		DeleteDC(OutputDisplay);

		if (Painting)
			EndPaint(PCBWindow, &PS);
		else
			ReleaseDC(PCBWindow, OutputDisplay2);

		DCInUse = 0;
		ViewBitmap = NULL;
		OutputDisplay = NULL;
		OutputDisplay2 = NULL;
		CurrentObjectCode = -1;
	}
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
			EndPaint(PCBWindow, &PS);
		else
			ReleaseDC(PCBWindow, OutputDisplay);

		DCInUse = 0;
		OutputDisplay = NULL;
		OutputDisplay = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void EndDrawingEditingWindow(int32 BufferMode)
{
	SelectClipRgn(OutputDisplay, NULL);
	DeleteObject(EditingRegion);
	EditingRegion = NULL;

	BufferMode &= BM_Mask;
	
	switch (BufferMode)
	{
	    case BM_DirectToScreen:
		//SelectClipRgn(OutputDisplay, NULL);
		//DeleteObject(EditingRegion);
		//EditingRegion = NULL;
		DoneDeviceContext();
		break;
		
		case BM_MultiStart:
		case BM_MultiContinue:
		break;
		
		case BM_MultiEnd:
		case BM_DoubleBuffer:
	    //OutputDebugString("EndDrawingEditingWindow: BM_MultiEnd/n");
		//SelectClipRgn(OutputDisplay, NULL);
		//DeleteObject(EditingRegion);
	    //EditingRegion = NULL;
		DoneDeviceContext2();
		break;
	}
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

void SpecialDrawing()
{
	double x1a, y1a;
	ObjectRecord TestObject;
	uint8 PolygonBuf[8192];
	PolygonRecord *TestPolygon;
	ObjectArcRecord ObjectArc;

	TestPolygon = (PolygonRecord *) & PolygonBuf;
	memset(&TestObject, 0, sizeof(TestObject));
	memset(&ObjectArc, 0, sizeof(ObjectArc));
	memset(&PolygonBuf, 0, sizeof(PolygonBuf));
	TestObject.ObjectType = OBJECT_ARC;
	x1a = 176e5;
	y1a = 134e5;
	TestObject.x1 = x1a;
	TestObject.y1 = y1a;
	TestObject.x2 = 10e5;
	TestObject.y2 = 80e5;
	TestObject.x3 = 0e5;
	TestObject.y3 = 10e5;
	TestObject.x4 = 10e5;
	TestObject.y4 = 0e5;
	TestObject.Thickness = 0.2e5;
	ObjectArc.CentreX = (float) TestObject.x1;
	ObjectArc.CentreY = (float) TestObject.y1;
	ObjectArc.Width = (float) TestObject.x2;
	ObjectArc.Height = (float) TestObject.y2;
	ObjectArc.StartDiffX = (float) TestObject.x3;
	ObjectArc.StartDiffY = (float) TestObject.y3;
	ObjectArc.EndDiffX = (float) TestObject.x4;
	ObjectArc.EndDiffY = (float) TestObject.y4;
	ObjectArc.LineThickNess = (float) TestObject.Thickness;
	ObjectArc.Layer = INFO_LAYER;
//  DrawObjectArc(&ObjectArc,0.0,0.0,0);
	MakePolygonFromObject(&TestObject, TestPolygon, 0.0, 0, 0, 1);
	DrawFilledPolygon(TestPolygon, 4 + 2);
	DrawTestPolygon3(TestPolygon, 4);
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_RED + DRAW_WITH_PEN_AND_NOT_FILLED);
	DrawLine(MultX(TestObject.x1), MultY(TestObject.y1), MultX(TestObject.x1 + TestObject.x3 * 100),
	         MultY(TestObject.y1 + TestObject.y3 * 100));
	DrawLine(MultX(TestObject.x1), MultY(TestObject.y1), MultX(TestObject.x1 + TestObject.x4 * 100),
	         MultY(TestObject.y1 + TestObject.y4 * 100));
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
	int32 x, y, res, minx, maxx, miny, maxy;
	HRGN hrgn;

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
	InitDrawingObject(0, BACKGROUND_LAYER, 0, NORMAL_FILLED_AND_PEN1);
	res = FillRect(OutputDisplay, &Rect, GraphicsObjectBrush[BackGroundObjectNr]);

	if (OkToSpecialDrawing)
	{
		SpecialDrawing();

		if (GridVisible)
			DrawGrid();

		SetScrollPageSize();
		SetScrollPosition();
	}
	else
	{
		if (!DebugPaint)
		{
			ReDrawing = 1;
			if (OkToAddViewPos)
				SaveViewPos();

			OkToAddViewPos = 1;
			ViewMinX = PixelToRealOffX(minx - 1);
			ViewMaxX = PixelToRealOffX(maxx + 1);
			ViewMinY = PixelToRealOffY(DrawWindowMaxY - (maxy + 1) - 1);
			ViewMaxY = PixelToRealOffY(DrawWindowMaxY - (miny - 1) - 1);

// ********************************************************************************************************

			DrawObjectLines(32);
			DrawObjectRects(32);
			DrawObjectArcs(32, 0);
			DrawObjectTexts2(32);
			DrawObjectPolygons(32);

			if ((DrawSoldMaskBottomMode == 2) || (DrawSoldMaskTopMode == 2))
				DrawViaSoldMaskPads(0);

// ********************************************************************************************************
// Draw areafills layer != CurrentDrawingLayer
			DrawAreaFills(16);

			if (OkToDrawClearances)
				DrawAreaFills(16 + 8);

// Draw pads on layer != CurrentDrawingLayer
			DrawComps(16);

			if (OkToDrawClearances)
				DrawPinsComps(16 + 8);

// Draw traces layer != CurrentDrawingLayer
			DrawTraces(16);

			if (OkToDrawClearances)
				DrawTracesWithClearance(16);

// Draw arc/all angle traces layer != CurrentDrawingLayer
			DrawObjectLines(16);
			DrawObjectRects(16);
			DrawObjectArcs(16, 0);
			DrawObjectTexts2(16);
			DrawObjectPolygons(16);

			if (OkToDrawClearances)
			{
				DrawObjectLines(16 + 8);
				DrawObjectRects(16 + 8);
				DrawObjectArcs(16 + 8, 0);
				DrawObjectPolygons(16 + 8);
			}

// ********************************************************************************************************
// Draw areafills layer == CurrentDrawingLayer
			DrawAreaFills(0);
			CurrentObjectCode = -1;

			if (OkToDrawClearances)
				DrawAreaFills(8);

// Draw pads on layer == CurrentDrawingLayer
			DrawComps(0);

			if (OkToDrawClearances)
				DrawPinsComps(8);

// Draw traces layer == CurrentDrawingLayer
			DrawTraces(0);

			if (OkToDrawClearances)
				DrawTracesWithClearance(0);

// Draw arc/all angle traces layer == CurrentDrawingLayer
			DrawObjectLines(0);
			DrawObjectRects(0);
			DrawObjectArcs(0, 0);
			DrawObjectTexts2(0);
			DrawObjectPolygons(0);

			if (OkToDrawClearances)
			{
				DrawObjectLines(8);
				DrawObjectRects(8);
				DrawObjectArcs(8, 0);
				DrawObjectPolygons(8);
			}

// ********************************************************************************************************
			if (OkToDrawVias)
				DrawVias(0);

			if ((OkToDrawVias) && ((OkToDrawClearances) || (OkToDrawViaClearances)))
				DrawVias(8);

			if (OkToDrawConnections)
				DrawConnections(0);

			DrawErrorObjects();

			if (DrawDrillMode > 0)
			{
				DrawObjectArcs(0, 1);
				DrawDrillsComps(0);
				DrawViaDrills(0);

				if (OkToDrawClearances)
				{
					DrawObjectArcs(8, 1);
					DrawDrillsComps(8);
				}
			}

			if (SelectionMode == GATE_PINSWAP_MODE)
			{
				GetGateSwapInfo(0, 0, 0, 0, 0, 0, 4);
				GetPinSwapInfo(0, 0, 0, 0, 0, 0, 4);
			}

			InitDrawingObject(0, BOARD_OUTLINE_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);

			x = MultX(0.0);
			y = MultY(0.0);
			DrawLine(x, y + 40, x, y - 40);
			DrawLine(x + 40, y, x - 40, y);

			if (GridVisible)
				DrawGrid();

			SetScrollPageSize();
			SetScrollPosition();
		}
	}

	ExitDrawing();

	ReDrawing = 0;
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
	Rect.right = (int32) DrawWindowMinX;
	Rect.top = 0;
	Rect.bottom = ClientRect.bottom - HeightInfoBar;
	res = FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	Rect.left++;
	Rect.top++;
	Rect.right--;
	Rect.bottom--;
	res = FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));


	ButtonBitmap = LoadBitmap(PCBClass.hInstance, MAKEINTRESOURCE(BITMAP_BUTTONS));
	ButtonMemoryDC = CreateCompatibleDC(OutputDisplay);
	old = SelectObject(ButtonMemoryDC, ButtonBitmap);
	res = BitBlt(OutputDisplay, 1, 1, 28, ButtonSizeY * NrButtons + 2, ButtonMemoryDC, 0, 0, SRCCOPY);
	SelectObject(ButtonMemoryDC, old);
	DeleteDC(ButtonMemoryDC);
	DeleteObject(ButtonBitmap);
	PressButton(SelectionMode, 1);
	DeleteObject(hrgn);
}

//*****************************************************************************************************************************
//*****************************************************************************************************************************
//*****************************************************************************************************************************

void RedrawLayerStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY, Index = 0;
	char LayerStr[32];

	LeftTopX = 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 1);
	RightBottomX = ScreenPosAbsCursor - 3;
	RightBottomY = ClientRect.bottom - 2;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:
		break;

	case 1:
		InitDeviceContext();
		break;
	}

	res = SelectClipRgn(OutputDisplay, TempRegion);

	if (CurrentDrawingLayer != -1)
	{
		GetLayerText(CurrentDrawingLayer, LayerStr, 4);
		Index = ViewLayer1ObjectNr + (DrawLayerCode[CurrentDrawingLayer] & 0x0f);
	}
	else
		sprintf(LayerStr, "None");

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));
	SetTextColor(OutputDisplay, RGB(255, 255, 255));
	SetBkColor(OutputDisplay, RGB(0, 0, 0));

	Rect.left = LeftTopX;
	Rect.right = RightBottomX;
	Rect.top = LeftTopY;
	Rect.bottom = RightBottomY;

	if (CurrentDrawingLayer == -1)
		FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	else
		FillRect(OutputDisplay, &Rect, GraphicsObjectBrush[Index]);

	SetBkMode(OutputDisplay, OPAQUE);
	TextOutUTF8(OutputDisplay, 20, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR) & LayerStr, strlen(LayerStr));

	SetBkMode(OutputDisplay, TRANSPARENT);

	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:
		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RedrawAbsPosStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosAbsCursor + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ScreenPosAbsGridCursor - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:
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
	TextOutUTF8(OutputDisplay, ScreenPosAbsCursor + 2, ClientRect.bottom - (HeightInfoBar - 4), 
		       (LPSTR) & AbsPosStr, strlen(AbsPosStr)); 

	SelectObject(OutputDisplay, SaveFont);

	switch (Mode)
	{
	case 0:
		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
	RedrawAbsPosStrCnt++;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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

	switch (Mode)
	{
	case 0:
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
		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RedrawRelPosStr(int32 Mode)
{
	RECT Rect, ClipRect, ClipRect2;
	HGDIOBJ SaveFont;
	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;

	LeftTopX = ScreenPosRelGridCursor + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ScreenPosInfoStr - 3;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

	switch (Mode)
	{
	case 0:
		break;

	case 1:
		InitDeviceContext();
		break;
	}

	GetClipBox(OutputDisplay, &ClipRect);
	res = SelectClipRgn(OutputDisplay, TempRegion);
	GetClipBox(OutputDisplay, &ClipRect2);

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
		break;

	case 1:
		DoneDeviceContext();
		break;
	}

	res = DeleteObject(TempRegion);
	TempRegion = NULL;
	res = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RedrawInfoStr(int32 Mode)
{
	RECT Rect;
	HGDIOBJ SaveFont;
	int32 res, LeftTopX, LeftTopY, RightBottomX, RightBottomY;
	WCHAR InfoStrUC[MAX_LENGTH_UC_STRING];

	if (Utf8ToUnicode(InfoStr, InfoStrUC, MAX_LENGTH_STRING - 1) == 0)
		return;

	LeftTopX = ScreenPosInfoStr + 1;
	LeftTopY = ClientRect.bottom - (HeightInfoBar - 4);
	RightBottomX = ClientRect.right - 4;
	RightBottomY = ClientRect.bottom - 3;
	TempRegion = CreateRectRgn(LeftTopX, LeftTopY, RightBottomX, RightBottomY);

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
	Rect.left = LeftTopX;
	Rect.right = RightBottomX;
	Rect.top = LeftTopY;
	Rect.bottom = RightBottomY;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	TextOutW(OutputDisplay, ScreenPosInfoStr + 2, ClientRect.bottom - (HeightInfoBar - 4), 
		    (LPCWSTR) & InfoStrUC, wcslen(InfoStrUC));

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
	res = 1;
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

	SaveFont = SelectObject(OutputDisplay, GetStockObject(DEFAULT_GUI_FONT));

	minx = max(UpdateRect.left, 0);
	maxx = min(UpdateRect.right, (int32) ClientRect.right);
	miny = max(UpdateRect.top, ClientRect.bottom - HeightInfoBar - HeightScrollBar);
	maxy = min(UpdateRect.bottom, (int32) ClientRect.bottom);

	hrgn = CreateRectRgn(minx, miny, maxx, maxy);
	SelectClipRgn(OutputDisplay, hrgn);

	SetTextColor(OutputDisplay, RGB(0, 0, 0));
	SetBkColor(OutputDisplay, RGB(192, 192, 192));
	Rect.left = 0;
	Rect.right = ClientRect.right;
	Rect.top = ClientRect.bottom - HeightInfoBar;
	Rect.bottom = ClientRect.bottom;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));
	Rect.left = ClientRect.right - WidthScrollBar;
	Rect.right = ClientRect.right;
	Rect.top = ClientRect.bottom - HeightInfoBar - HeightScrollBar - 1;
	Rect.bottom = ClientRect.bottom - HeightInfoBar;
	FillRect(OutputDisplay, &Rect, GetStockObject(LTGRAY_BRUSH));

	Rect.left = ScreenPosAbsCursor + 2;
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

	Rect.left = ScreenPosAbsCursor;
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

	Rect.left = ScreenPosAbsCursor;
	Rect.right = ScreenPosAbsCursor + 2;
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

	Rect.left = ScreenPosAbsCursor + 1;
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

	RedrawLayerStr(2);
	RedrawAbsPosStr(2);
	RedrawAbsGridPosStr(2);
	RedrawRelPosStr(2);
	RedrawInfoStr(2);
	SelectObject(OutputDisplay, SaveFont);
	DeleteObject(hrgn);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void WindowPaint()
{
	RECT Rect, Rect2;
	int32 ok, res;

	GetClientRect(PCBWindow, &Rect);

	if (GetUpdateRect(PCBWindow, &UpdateRect, 0))
	{
		if (PaintBecauseOfResize)
			ok = 1;

		if (!PaintBecauseOfResize)
		{
			Painting = 1;
			
			InitDeviceContext2();

			if (UpdateRect.bottom >= (int32) DrawWindowMaxY + HeightScrollBar)
				RedrawInfoBar();

			if (UpdateRect.left < (int32) DrawWindowMinX)
				RedrawButtons();

			if ((UpdateRect.bottom >= (int32) DrawWindowMinY) && (UpdateRect.top < (int32) DrawWindowMaxY))
				RedrawMainWindow();

				DoneDeviceContext2();

			Painting = 0;
		}
		else
		{
			Painting = 1;
			InitDeviceContext();
			DoneDeviceContext();
			Painting = 0;
			res = GetUpdateRect(PCBWindow, &UpdateRect, 0);

			if ((PaintBecauseOfResizeMode & (128 + 64 + 32 + 16 + 8 + 4)) != 0)
			{
				Rect2.left = 0;
				Rect2.right = ClientRect.right;
				Rect2.top = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				Rect2.bottom = ClientRect.bottom;
				InvalidateRect(PCBWindow, &Rect2, 0);
				res = GetUpdateRect(PCBWindow, &UpdateRect, 0);
				Painting = 1;

			    InitDeviceContext2();

				RedrawInfoBar();

                DoneDeviceContext2();

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

				InvalidateRect(PCBWindow, &Rect2, 0);
				res = GetUpdateRect(PCBWindow, &UpdateRect, 0);
				Painting = 1;

                InitDeviceContext2();

				RedrawButtons();

                DoneDeviceContext2();

				Painting = 0;
			}

			if ((PaintBecauseOfResizeMode & (1)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = ClientRect.right;
				Rect2.top = 0;
				Rect2.bottom = abs(ClientRect.bottom - OldClientRect.bottom);
				InvalidateRect(PCBWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (16)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = ClientRect.right;
				Rect2.top =
				    ClientRect.bottom - HeightInfoBar - HeightScrollBar - abs(ClientRect.bottom - OldClientRect.bottom);
				Rect2.bottom = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				InvalidateRect(PCBWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (16 + 1)) != 0)
			{
				res = GetUpdateRect(PCBWindow, &UpdateRect, 0);
				Painting = 1;

                InitDeviceContext2();

				RedrawMainWindow();

                DoneDeviceContext2();

				Painting = 0;
			}

			if ((PaintBecauseOfResizeMode & (4)) != 0)
			{
				Rect2.left = OldClientRect.right - WidthScrollBar;
				Rect2.right = ClientRect.right - WidthScrollBar;
				Rect2.top = 0;
				Rect2.bottom = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
				InvalidateRect(PCBWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (64)) != 0)
			{
				Rect2.left = DrawWindowMinX;
				Rect2.right = DrawWindowMinX + abs(ClientRect.right - OldClientRect.right);
				Rect2.top = 0;
				Rect2.bottom = ClientRect.bottom - HeightInfoBar;
				InvalidateRect(PCBWindow, &Rect2, 0);
			}

			if ((PaintBecauseOfResizeMode & (64 + 4)) != 0)
			{
				res = GetUpdateRect(PCBWindow, &UpdateRect, 0);
				Painting = 1;

                InitDeviceContext2();

				RedrawMainWindow();

                DoneDeviceContext2();

				Painting = 0;
			}

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

void ScrollUp(int32 SizeOfScroll)
{
	RECT Rect, Rect2;
	double plus;

	DrawCrossHair(16 + 2);
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
	UpdateWindow(PCBWindow);
	ScrollWindow(PCBWindow, 0, SizeOfScroll, &Rect, &Rect2);
	UpdateWindow(PCBWindow);

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

	DrawCrossHair(16 + 2);
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
	UpdateWindow(PCBWindow);
	ScrollWindow(PCBWindow, 0, -SizeOfScroll, &Rect, &Rect2);
	UpdateWindow(PCBWindow);

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

	DrawCrossHair(16 + 2);
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
	UpdateWindow(PCBWindow);
	ScrollWindow(PCBWindow, SizeOfScroll, 0, &Rect, &Rect2);
	UpdateWindow(PCBWindow);

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

	DrawCrossHair(16 + 2);
	plus = PixelToReal(SizeOfScroll);
	Xoffset += plus;
	Rect.left = DrawWindowMinX;
	Rect.right = DrawWindowMaxX;
	Rect.top = DrawWindowMinY;
	Rect.bottom = DrawWindowMaxY;
	Rect2.left = DrawWindowMinX;
	Rect2.top = DrawWindowMinY;
	Rect2.right = DrawWindowMaxX;
	Rect2.bottom = DrawWindowMaxY;
	UpdateWindow(PCBWindow);
	ScrollWindow(PCBWindow, -SizeOfScroll, 0, &Rect, &Rect2);
	UpdateWindow(PCBWindow);

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

	DrawCrossHair(16 + 2);
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
	UpdateWindow(PCBWindow);
	ScrollWindow(PCBWindow, DivX, DivY, &Rect, &Rect2);
	UpdateWindow(PCBWindow);

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
	ClientToScreen(PCBWindow, &P);
	Rect.left = P.x;
	Rect.top = P.y;

	P.x = DrawWindowMaxX;
	P.y = DrawWindowMaxY;
	ClientToScreen(PCBWindow, &P);
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


LRESULT CALLBACK PCBWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam)
{
	POINT pp;
	double hulp, hulp2;
	int32 v1, v2, ok, res, MousePosY2, hulp3, divx, divy, CommandToExecute;
	LRESULT Result;
	double dx, dy, cx, cy, ty, tx, Factor3;
/*
#ifdef _DEBUG
	char str[200], str2[200];

	if ((Message >= 0x201) && (Message <= 0x209))
		ok = 1;

	if ((TimerValue > 20) && (Message != WM_TIMER) && (Message != WM_NCHITTEST) && (Message != WM_CHAR)
	        && (Message != WM_NCACTIVATE) && (Message != WM_WINDOWPOSCHANGED) && (Message != WM_WINDOWPOSCHANGING)
	        && (Message != WM_GETTEXT) && (Message != WM_SETCURSOR) && (Message != WM_KEYDOWN) && (Message != WM_KEYUP)
	        && (Message != WM_MOUSEACTIVATE) && (Message != WM_MOUSEMOVE))
		ok = 1;

	if ((Message != WM_TIMER) && (Message != WM_GETTEXT) && (Message != WM_SETTEXT) && (Message != WM_GETICON)
	        && (Message != WM_SETCURSOR) && (Message != WM_NCHITTEST))
	{
		GetTimeString(1, str2);
		sprintf(str, "%s | Message 0x%04x, WPARAM = %d,%d LPARAM = %d,%d\n", str2, Message, LOWORD(WParam),
		        HIWORD(WParam), LOWORD(LParam), HIWORD(LParam));
		OutputDebugString(str);
	}

#endif
*/
	switch (Message)
	{
	case WM_HELP:
		Focused = 0;
		ok = 1;
		break;

	case WM_CREATE:
		FirstSize = 1;
		WindowCreate();
		break;

	case WM_USER:
		PCBCommand(WParam, (int32) LParam);
		break;

	case WM_COMMAND:
		if (HIWORD(LParam) == 0)
			PCBCommand(WParam, (int32) LParam);

		break;

	case WM_PAINT:
		if (FirstPaint)
		{
			FirstPaint = 0;

			if (StartWithMaximumView)
				ViewWholeDesign(0);
			else
				ViewWholeDesign(2);

			FirstPaintMode = 0;

			if (MakeNewDesign)
				PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_FILE_NEW, (LPARAM) 1);

			MakeNewDesign = 0;
			SetScrollPageSize();
			SetScrollPosition();
			UpdateWindow(hwndHBar);
			UpdateWindow(hwndVBar);
		}

		WindowPaint();
		break;

	case WM_MOUSEWHEEL:
		v1 = LOWORD(WParam);

//      Beep(500,100);
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
						//            Beep(1500,100);
					}
					else
					{
						DisplayInfoOff(0);
						ScrollUp(v2);
						ScrollUp(v2);
						ScrollUp(v2);
						ScrollUp(v2);
						//            Beep(1000,100);
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

//      InsertFunction(1);
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
		if (WParam == SIZE_MINIMIZED)
			WindowMinimized = 1;

		if (WParam == SIZE_MAXIMIZED)
			WindowMaximized = 1;

		memmove(&OldClientRect, &ClientRect, sizeof(RECT));
		memmove(&OldRealWindow, &RealWindow, sizeof(RECT));
		GetClientRect(PCBWindow, &ClientRect);
		GetWindowRect(PCBWindow, &RealWindow);
		DrawWindowMinX = WidthButtonBar;
		DrawWindowMinY = 0;
		DrawWindowMaxX = ClientRect.right - WidthScrollBar;
		DrawWindowMaxY = ClientRect.bottom - HeightInfoBar - HeightScrollBar;
		WindowStartX = (int32) RealWindow.left;
		WindowStartY = (int32) RealWindow.top;
		WindowWidth = (int32) (RealWindow.right - RealWindow.left);
		WindowHeight = (int32) (RealWindow.bottom - RealWindow.top);
		ScreenPosAbsCursor = ScreenPosAbsCursorInit;
		ScreenPosAbsGridCursor = ScreenPosAbsGridCursorInit;
		ScreenPosRelGridCursor = ScreenPosRelGridCursorInit;
		ScreenPosInfoStr = ScreenPosInfoStrInit;

		if (WindowWidth > 900)
		{
			ScreenPosAbsGridCursor = ScreenPosAbsGridCursorInit + 30;
			ScreenPosRelGridCursor = ScreenPosRelGridCursorInit + 60;
			ScreenPosInfoStr = ScreenPosInfoStrInit + 120;
#ifdef _DEBUG
			ScreenPosAbsGridCursor = ScreenPosAbsGridCursorInit + 60;
			ScreenPosRelGridCursor = ScreenPosRelGridCursorInit + 90;
			ScreenPosInfoStr = ScreenPosInfoStrInit + 150;
#endif
		}

		ClientWindowDivX = DrawWindowMaxX - DrawWindowMinX;
		ClientWindowDivY = DrawWindowMaxY - DrawWindowMinY;
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
		ClientToScreen(PCBWindow, &pp);
		ClientStartX = (int32) pp.x;
		ClientStartY = (int32) pp.y;

		if (FirstSize == 1)
		{
			FirstSize = 0;
			hulp = Design.BoardWidth / DrawWindowMaxX;
			hulp2 = Design.BoardHeight / DrawWindowMaxY;

			if (hulp > hulp2)
			{
				hulp = (DrawWindowMaxX - DrawWindowMinX);
				Factor = (hulp / Design.BoardWidth);
				DisplX = Design.BoardWidth;
				DisplY = DisplX * DrawWindowMaxY / DrawWindowMaxX;
			}
			else
			{
				hulp = (DrawWindowMaxY - DrawWindowMinY);
				Factor = (hulp / Design.BoardHeight);
				Factor3 = PixelToReal(Mult(Design.BoardHeight));
				DisplY = Design.BoardHeight;
				DisplX = DisplY * DrawWindowMaxX / DrawWindowMaxY;
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
		GetWindowRect(PCBWindow, &RealWindow);
		GetWindowRect(PCBWindow, &RealWindow2);
		pp.x = 0;
		pp.y = 0;
		ClientToScreen(PCBWindow, &pp);
		ClientStartX = (int32) pp.x;
		ClientStartY = (int32) pp.y;
		WindowStartX = (int32) RealWindow.left;
		WindowStartY = (int32) RealWindow.top;
		break;

	case WM_COMPAREITEM:
		ok = 1;
		break;

	case WM_DRAWITEM:
		DrawSpecialItem((DRAWITEMSTRUCT *) LParam);
		break;

	case WM_MEASUREITEM:
		InitSpecialDraw((MEASUREITEMSTRUCT *) LParam);
		ok = 1;
		break;

	case WM_CHAR:
		if (Focused)
			KeyChar(LOWORD(WParam));

		break;

	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
		if (Focused)
		{
			KeyDown(LOWORD(WParam));

			if ((AltPressed) && ((WParam > 0x70) || (WParam == 0x09)))
				return DefWindowProc(Window, Message, WParam, LParam);
		}

		break;

	case WM_CAPTURECHANGED:
		res = 1;
		break;

	case WM_NCMOUSEMOVE:
		res = 1;
		break;

	case WM_KEYUP:
	case WM_SYSKEYUP:
		if ((Focused) && (FocusedAgain))
		{
			KeyUp(LOWORD(WParam));
			return DefWindowProc(Window, Message, WParam, LParam);
		}

		break;

	case WM_NCHITTEST:
		Result = 0;
		Result = DefWindowProc(Window, Message, WParam, LParam);

		if (Result != HTCLIENT)
		{
			MousePosX = 10000;
			MousePosY = 10000;
			DrawMouseInfoOff(0);
			DisplayInfoOff(0);
			ok = 1;
		}

		ok = 1;
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
			}
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
		MousePosY2 = HIWORD(LParam) + 1;

		if ((Focused) && (FocusedAgain) && (MousePosY2 < DrawWindowMaxY))
		{
			MouseChanged = 1;
			FocusedAgain = 1;
			LeftButtonPressed = 0;
			LeftButtonDoublePressed = 0;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
			return DefWindowProc(Window, Message, WParam, LParam);
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
			RightButtonPressed = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
		}

		break;

	case WM_RBUTTONUP:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			RightButtonPressed = 0;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
		}

		break;

	case WM_MBUTTONDOWN:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MiddleButtonPressed = 1;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
		}

		break;

	case WM_MBUTTONUP:
		if ((Focused) && (FocusedAgain))
		{
			MouseChanged = 1;
			MiddleButtonPressed = 0;
			MousePosX = LOWORD(LParam);
			MousePosY = HIWORD(LParam) + 1;
		}

		break;

	case WM_SETFOCUS:
		Focused = 1;
		WaitForPaint = 0;
		SetCursor(LoadCursor(0, CursorType));
		break;

	case WM_KILLFOCUS:
		Focused = 0;
		DrawCrossHair(32 + 2);
		DrawMouseInfoOff(0);
		DisplayInfoOff(0);
		UnClipMouseCursor();
		AltPressed = 0;
		CtrlPressed = 0;
		ShiftPressed = 0;
		RightButtonPressed = 0;
		LeftButtonPressed = 0;
		break;

	case WM_TIMER:
		TimerValue++;
		CheckClipBoard();

		if ((TimerValue % ((1 * 1000) / NR_MILLI_SECONDS_TIMER)) == 0)
		{
			if (ProjectIndexNr != -1)
			{
				if ((ProjectInfo->OtherInfos[0] != 0) && ((Design.NrComps > 0) || (Design.NrNets > 0))
				        && (!TimerUpdateNetlist))
				{
					PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_FILE_UPDATENETLISTCOMPONENTS, (LPARAM) NULL);
					TimerUpdateNetlist = 1;
				}
			}
		}

		if ((TimerValue % ((10 * 60 * 1000) / NR_MILLI_SECONDS_TIMER)) == 0)
			NrCachePolygonObjectsUpdate = 1;

		if ((TimerValue % (60 * 10)) == 5)
		{
			if (ExecuteCommands[0] != 0)
			{
				if (sscanf(ExecuteCommands, "%d", &CommandToExecute) == 1)
				{
					if (CommandToExecute > 0)
						PostMessage(PCBWindow, WM_COMMAND, (WPARAM) CommandToExecute, (LPARAM) NULL);
				}

				ExecuteCommands[0] = 0;
			}
		}

		if ((TimerValue > 1) && (CheckReference) && (SearchForReferenceString[0] != 0))
		{
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_CENTER_ON_COMPONENT, (LPARAM) 2);
			CheckReference = 0;
		}

		if ((TimerValue > 1) && (CheckPartNr) && (SearchForPartNrString[0] != 0))
		{
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_CENTER_ON_COMP_PARTNR, (LPARAM) 2);
			CheckPartNr = 0;
		}

		if ((TimerValue % 10) == 0)
		{
			if (CheckNewUserVars(0) == 1)
				RePaint();
		}

		return DefWindowProc(Window, Message, WParam, LParam);
		break;

	case WM_CLOSE:
		if (SystemBusyMode == 0)
		{
			if (SaveFile2(0) == 1)
			{
				WriteIniFile();
				DeAllocateMem();
				Help(0, 2);

				if (ProjectIndexNr != -1)
				{
					ProjectInfo->FileNames[ProjectIndexNr][0] = 0;
					ProjectInfo->FileInfos[ProjectIndexNr] = 0;
					ProjectInfo->FileTypes[ProjectIndexNr] = 0;
					ProjectInfo->WindowHandles[ProjectIndexNr] = 0;
				}

				DestroyWindow(PCBWindow);
			}
		}

		break;

	case WM_GETMINMAXINFO:
		ok = 1;
		return DefWindowProc(Window, Message, WParam, LParam);

	case WM_QUIT:
		ok = 1;
		return 0;

	case WM_DESTROY:
		WindowDestroy();
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


void ChangeValue(int32 * Object, LPSTR str)
{
	int32 Value, res;

	if (((res = sscanf(str, "%i", &Value)) == 1) && ((Value == 0) || (Value == 1)))
		*Object = Value;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void LoadIniFile(LPSTR FileName, int32 mode)
{
	int32 fp, cnt, Length, Value, res, ok, Key, ParamMode, OldWindowStartX, OldWindowStartY, OldWindowWidth,
	      OldWindowHeight, ChangeGrid = 0, ChangeTraces = 0, ChangeClearances = 0;
	char LineBuf[512], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING],
	     str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING];
	float Value1;

	if (FileName[0] == 0)
		return;

	OldWindowStartX = WindowStartX;
	OldWindowStartY = WindowStartY;
	OldWindowWidth = WindowWidth;
	OldWindowHeight = WindowHeight;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return;

	ParamMode = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString(LineBuf, str1);
			GetString(LineBuf, str2);

			if (str1[0] == '[')
			{
				ParamMode = 0;

				if (stricmp(str1, "[ExeDirectory]") == 0)
				{
					if (mode & 1)
						ParamMode = 1;
				}

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
				case 1:
					break;

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

						if (stricmp(str1, "CurrentDrawingLayer") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								CurrentDrawingLayer = Value;
						}

						if (stricmp(str1, "GerberNumberMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								GerberInfo.GerberNumberMode = Value;
						}

						if (stricmp(str1, "GerbvProject") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								if (Value == 0)
									GerberInfo.GerbvProject = 0;
								
								if (Value == 1)
									GerberInfo.GerbvProject = 1;
							}
						}

						if (stricmp(str1, "GridSize") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								UserGridSize = Value1;
								GridSize = Value1;

								if (GridSize < 10.0)
								{
									if (Units == 0)
										GridSize = 5 * 2540.0;
									else
										GridSize = 0.1 * 10e4;

								}
							}
						}

						if (stricmp(str1, "CompGridSize") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								CompGridSize = Value1;

								if (CompGridSize < 10.0)
								{
									if (Units == 0)
										CompGridSize = 5 * 2540.0;
									else
										CompGridSize = 0.1 * 10e4;
								}
							}
						}

						if (stricmp(str1, "TraceGridSize") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								TraceGridSize = Value1;

								if (TraceGridSize < 10.0)
								{
									if (Units == 0)
										TraceGridSize = 5 * 2540.0;
									else
										TraceGridSize = 0.1 * 10e4;
								}
							}
						}

						if (stricmp(str1, "AreafillGridSize") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								AreafillGridSize = Value1;

								if (AreafillGridSize < 10.0)
								{
									if (Units == 0)
										AreafillGridSize = 5 * 2540.0;
									else
										AreafillGridSize = 0.1 * 10e4;
								}
							}
						}

						if (stricmp(str1, "StartWithMaximumView") == 0)
							ChangeValue(&StartWithMaximumView, str2);

						if (stricmp(str1, "MoveCompAutoZoom") == 0)
							ChangeValue(&MoveCompAutoZoom, str2);

						if (stricmp(str1, "ButtonInfoTimeout") == 0)
							ChangeValue(&ButtonInfoTimeout, str2);

						if (stricmp(str1, "ButtonInfoTimeoutStart") == 0)
							ChangeValue(&ButtonInfoTimeoutStart, str2);

						if (stricmp(str1, "PopupDisplayVisible") == 0)
							ChangeValue(&PopupDisplayVisible, str2);

						if (stricmp(str1, "ViewOffsetX") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								ViewOffsetX = Value1;
							}
						}

						if (stricmp(str1, "ViewOffsetY") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								ViewOffsetY = Value1;
							}
						}

						if (stricmp(str1, "ViewScale") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								ViewScale = Value1;
							}
						}

						if (stricmp(str1, "PlotPen1") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								GerberInfo.PenSizes[0] = Value1;
							}
						}

						if (stricmp(str1, "PlotPen2") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								GerberInfo.PenSizes[1] = Value1;
							}
						}

						if (stricmp(str1, "BitmapExportResolution") == 0)
						{
							if (sscanf(str2, "%f", &Value1) == 1)
							{
								Value1 = (float) atof(str2);
								GerberInfo.BitmapExportResolution = Value1;
							}
						}

						if (stricmp(str1, "BitmapExportSaveMode") == 0)
						{
							if (sscanf(str2, "%d", &Value) == 1)
								GerberInfo.BitmapExportSaveMode = Value;
						}

						if (stricmp(str1, "ZoomMode") == 0)
						{
							if (sscanf(str2, "%d", &Value) == 1)
								ZoomMode = Value;
						}

						if (stricmp(str1, "FastPaint") == 0) //obsolete
						{
							if (sscanf(str2, "%d", &Value) == 1)
								FastPaint = Value;
						}

						if (stricmp(str1, "MousePanMultiply") == 0)
						{
							if (sscanf(str2, "%d", &Value) == 1)
								MousePanMultiply = Value;
						}

						if (stricmp(str1, "DrawGrid") == 0)
							ChangeValue(&GridVisible, str2);

						if (stricmp(str1, "CompSnapMode") == 0)
							ChangeValue(&CompSnapMode, str2);

						if (stricmp(str1, "SnapMode") == 0)
							ChangeValue(&SnapMode, str2);

						if (stricmp(str1, "DrawAreaFills") == 0)
							ChangeValue(&OkToDrawAreaFills, str2);

						if (stricmp(str1, "DrawAreaFillMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								AreafillDrawMode = Value;

								if (AreafillDrawMode > 1)
									AreafillDrawMode = 0;
							}
						}

						if (stricmp(str1, "DrawClearances") == 0)
							ChangeValue(&OkToDrawClearances, str2);

						if (stricmp(str1, "DrawCompOutline") == 0)
							ChangeValue(&OkToDrawCompOutline, str2);

						if (stricmp(str1, "DrawConnections") == 0)
							ChangeValue(&OkToDrawConnections, str2);

						if (stricmp(str1, "DrawSoldMaskBottomMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								DrawSoldMaskBottomMode = Value;

								if ((DrawSoldMaskBottomMode < 0) || (DrawSoldMaskBottomMode > 2))
									DrawSoldMaskBottomMode = 1;
							}
						}

						if (stricmp(str1, "DrawSoldMaskTopMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								DrawSoldMaskTopMode = Value;

								if ((DrawSoldMaskTopMode < 0) || (DrawSoldMaskTopMode > 2))
									DrawSoldMaskTopMode = 1;
							}
						}

						if (stricmp(str1, "DrawPasteMaskBottomMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								DrawPasteMaskBottomMode = Value;

								if ((DrawPasteMaskBottomMode < 0) || (DrawPasteMaskBottomMode > 2))
									DrawPasteMaskBottomMode = 1;
							}
						}

						if (stricmp(str1, "DrawPasteMaskTopMode") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								DrawPasteMaskTopMode = Value;

								if ((DrawPasteMaskTopMode < 0) || (DrawPasteMaskTopMode > 2))
									DrawPasteMaskTopMode = 1;
							}
						}

						if (stricmp(str1, "DrawDrillType") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
								DrawDrillMode = Value;

								if ((DrawDrillMode < 0) || (DrawDrillMode > 3))
									DrawDrillMode = 3;
							}
						}

						if (stricmp(str1, "DrawInnerPads") == 0)
							ChangeValue(&OkToDrawInnerPads, str2);

						if (stricmp(str1, "DrawTopPads") == 0)
							ChangeValue(&OkToDrawTopPads, str2);

						if (stricmp(str1, "DrawBottomPads") == 0)
							ChangeValue(&OkToDrawBottomPads, str2);

						if (stricmp(str1, "DrawBottomComponents") == 0)
							ChangeValue(&DrawBottomComponents, str2);

						if (stricmp(str1, "DrawTopComponents") == 0)
							ChangeValue(&DrawBottomComponents, str2);

						if (stricmp(str1, "DrawCompPlacement") == 0)
							ChangeValue(&OkToDrawCompPlacement, str2);

						if (stricmp(str1, "DrawRoutingKeepoutTop") == 0)
							ChangeValue(&OkToDrawRoutingKeepoutTop, str2);

						if (stricmp(str1, "DrawRoutingKeepoutBottom") == 0)
							ChangeValue(&OkToDrawRoutingKeepoutBottom, str2);

						if (stricmp(str1, "DrawRoutingKeepoutInner") == 0)
							ChangeValue(&OkToDrawRoutingKeepoutInner, str2);

						if (stricmp(str1, "DrawSilkScreenTop") == 0)
							ChangeValue(&OkToDrawSilkScreenTop, str2);

						if (stricmp(str1, "DrawSilkScreenBottom") == 0)
							ChangeValue(&OkToDrawSilkScreenBottom, str2);

						if (stricmp(str1, "DrawInfoObjects") == 0)
							ChangeValue(&OkToDrawInfoObjects, str2);

						if (stricmp(str1, "DrawBoardOutline") == 0)
							ChangeValue(&OkToDrawBoardOutline, str2);

						if (stricmp(str1, "DrawInfo2Objects") == 0)
							ChangeValue(&OkToDrawInfo2Objects, str2);

						if (stricmp(str1, "DrawInfo3Objects") == 0)
							ChangeValue(&OkToDrawInfo3Objects, str2);

						if (stricmp(str1, "DrawInfo4Objects") == 0)
							ChangeValue(&OkToDrawInfo4Objects, str2);

						if (stricmp(str1, "DrawVias") == 0)
							ChangeValue(&OkToDrawVias, str2);

						if (stricmp(str1, "DrawViaClearances") == 0)
							ChangeValue(&OkToDrawViaClearances, str2);

						if (stricmp(str1, "DrawCompReference") == 0)
							ChangeValue(&OkToDrawCompReference, str2);

						if (stricmp(str1, "DrawCompValue") == 0)
							ChangeValue(&OkToDrawCompValue, str2);

						if (stricmp(str1, "DrawTwoTryingTraces") == 0)
							ChangeValue(&DrawTwoTryingTraces, str2);

						if (stricmp(str1, "DrawCrossHair") == 0)
							ChangeValue(&OkToDrawCrossHair, str2);

						if (stricmp(str1, "RepeatModeActive") == 0)
							ChangeValue(&RepeatModeActive, str2);

						if (stricmp(str1, "RePaintAfterCompMove") == 0)
							ChangeValue(&OkToRePaintAfterCompMove, str2);

						if (stricmp(str1, "RePaintAfterTraceDrawing") == 0)
							ChangeValue(&OkToRePaintAfterTraceDrawing, str2);

						if (stricmp(str1, "DoNotShowAreafillThinLinesError") == 0)
							ChangeValue(&DoNotShowAreafillThinLinesError, str2);

						if (stricmp(str1, "RecalcAreafillAfterInsert") == 0)
							ChangeValue(&RecalcAreafillAfterInsert, str2);

						if (stricmp(str1, "MouseCursorOnGrid") == 0)
							ChangeValue(&MouseCursorOnGrid, str2);

						if (stricmp(str1, "SelectionMode") == 0)
							ChangeValue(&ReplaceSelections, str2);

						if (stricmp(str1, "ComponentConnectionMode") == 0)
							ChangeValue(&ComponentConnectionMode, str2);

						if (stricmp(str1, "ViewSingleLayer") == 0)
							ChangeValue(&ViewSingleLayer, str2);

						for (cnt = 0; cnt < 32; cnt++)
						{
							sprintf(str5, "Layer%i", cnt);

							if (stricmp(str1, str5) == 0)
							{
								if (sscanf(str2, "%i", &Value) == 1)
								{
									if (Value == 0)
										DrawLayerCode[cnt] |= 0x10;
									else
										DrawLayerCode[cnt] &= ~0x10;
								}
							}
						}

						for (cnt = 0; cnt < 30; cnt++)
						{
							sprintf(str5, "Grid%i", cnt);

							if (stricmp(str1, str5) == 0)
							{
								if (sscanf(str2, "%f", &Value1) == 1)
								{
									Value1 = (float) atof(str2);

									if (!ChangeGrid)
									{
										ChangeGrid = 1;
										NrGridSizes = 0;
									}

									if (NrGridSizes < 30)
										GridSizes[NrGridSizes++] = Value1;
								}
							}
						}

						for (cnt = 0; cnt < 30; cnt++)
						{
							sprintf(str5, "ClearanceWidth%i", cnt);

							if (stricmp(str1, str5) == 0)
							{
								if (sscanf(str2, "%f", &Value1) == 1)
								{
									Value1 = (float) atof(str2);

									if (!ChangeClearances)
									{
										ChangeClearances = 1;
										NrClearanceWidths = 0;
									}

									if (NrClearanceWidths < 30)
										ClearanceWidths[NrClearanceWidths++] = Value1;
								}
							}
						}

						for (cnt = 0; cnt < 30; cnt++)
						{
							sprintf(str5, "TraceWidth%i", cnt);

							if (stricmp(str1, str5) == 0)
							{
								if (sscanf(str2, "%f", &Value1) == 1)
								{
									Value1 = (float) atof(str2);

									if (!ChangeTraces)
									{
										ChangeTraces = 1;
										NrTraceWidths = 0;
									}

									if (NrTraceWidths < 30)
										TraceWidths[NrTraceWidths++] = Value1;
								}
							}
						}

						for (cnt = 0; cnt < NrLayerObjects; cnt++)
						{
							if (LayerObjectCodes[cnt].TextStr[0] != 0)
							{
								sprintf(str3, "%sColor", LayerObjectCodes[cnt].TextStr);

								if (stricmp(str1, str3) == 0)
								{
									if (sscanf(str2, "%i", &Value) == 1)
										PCBColors[LayerObjectCodes[cnt].ColorNr] = (COLORREF) Value;
								}
							}
						}

						for (cnt = 0; cnt < NrLayerObjects; cnt++)
						{
							if (LayerObjectCodes[cnt].TextStr[0] != 0)
							{
								sprintf(str3, "%sColor2", LayerObjectCodes[cnt].TextStr);

								if (stricmp(str1, str3) == 0)
								{
									if (sscanf(str2, "%i", &Value) == 1)
										PCBColors2[LayerObjectCodes[cnt].ColorNr] = (COLORREF) Value;
								}
							}
						}

						for (cnt = 0; cnt < NrLayerObjects; cnt++)
						{
							if (LayerObjectCodes[cnt].TextStr[0] != 0)
							{
								sprintf(str3, "%sBrush", LayerObjectCodes[cnt].TextStr);

								if (stricmp(str1, str3) == 0)
								{
									if (sscanf(str2, "%i", &Value) == 1)
										PCBObjectCodes[LayerObjectCodes[cnt].ColorNr] = (COLORREF) Value;
								}
							}
						}
					}

					break;

				case 4:
					if (GetStringValue(str4, str1, str2))
					{
						if (str2[0] != 0)
						{
#ifdef _DEBUG

							if (stricmp(str1, "FunctionZoomIn") == 0)
								ok = 1;

#endif
							Key = GetKeyValue(str2);

							if (Key != 0)
							{
								res = SetKeyOnKeyFunctionString(Key, str1);
#ifdef _DEBUG

								if (res == -1)
								{
									sprintf(str5, "Error in assigning key %s to function %s", str2, str1);
									MessageBoxOwn(PCBWindow, str5, SC(1, "Message"), MB_APPLMODAL | MB_OK);
								}

#endif
							}
						}
					}

					break;
				}
			}
		}
	}

	TextFileClose(fp);

	for (cnt = 0; cnt < 8; cnt++)
	{
		PCBColors[ViewLayer1InNetObjectNr + cnt] = PCBColors[ViewLayer1ObjectNr + cnt];
		PCBColors[ViewLayer1HilitedInNetObjectNr + cnt] = PCBColors[ViewLayer1HilitedObjectNr + cnt];
		PCBColors2[ViewLayer1InNetObjectNr + cnt] = PCBColors2[ViewLayer1ObjectNr + cnt];
		PCBColors2[ViewLayer1HilitedInNetObjectNr + cnt] = PCBColors2[ViewLayer1HilitedObjectNr + cnt];
	}

	GridSize = CompGridSize;

	if (!FirstPaintMode)
	{
		ok = 1;
		AdjustWindowSize();
		MoveWindow(PCBWindow, WindowStartX, WindowStartY, WindowWidth, WindowHeight, 1);
		RePaint();
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void WriteIniFile()
{
	int32 fp, cnt, res, Stop;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

//  if (EditFile[0]==0) return;

	if (FoundDesignPath)
		sprintf(IniFile, "%s\\pcb.ini", DesignPath);
	else
	{
		if (FoundProjectPath)
			sprintf(IniFile, "%s\\pcb.ini", ProjectPath);
	}

	if (IniFile[0] == 0)
		return;

	if ((fp = FileOpenWriteUTF8(IniFile)) <= 0)
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
	sprintf(str1, "GeometryDialogInitialX=%i", GeometryDialogInitialX);
	WriteLn(fp, str1);
	sprintf(str1, "GeometryDialogInitialY=%i", GeometryDialogInitialY);
	WriteLn(fp, str1);
	sprintf(str1, "GeomStartX=%i", GeomStartX);
	WriteLn(fp, str1);
	sprintf(str1, "GeomStartY=%i", GeomStartY);
	WriteLn(fp, str1);
	sprintf(str1, "GeomScreenWidth=%i", GeomScreenWidth);
	WriteLn(fp, str1);
	sprintf(str1, "GeomScreenHeight=%i", GeomScreenHeight);
	WriteLn(fp, str1);
	sprintf(str1, "Units=%i", Units);
	WriteLn(fp, str1);

	if (CurrentDrawingLayer == -1)
		ok = 1;

	if (GerberInfo.GerberNumberMode == 0)
	{
		if (Units == 0)
			GerberInfo.GerberNumberMode = 4;
		else
			GerberInfo.GerberNumberMode = 4 + 8;
	}

	sprintf(str1, "GerberNumberMode=%i", GerberInfo.GerberNumberMode);
	WriteLn(fp, str1);
	sprintf(str1, "GerbvProject=%i", GerberInfo.GerbvProject);
	WriteLn(fp, str1);
	sprintf(str1, "CurrentDrawingLayer=%i", CurrentDrawingLayer);
	WriteLn(fp, str1);
	sprintf(str1, "GridSize=%f", UserGridSize);
	WriteLn(fp, str1);
	sprintf(str1, "TraceGridSize=%f", TraceGridSize);
	WriteLn(fp, str1);
	sprintf(str1, "CompGridSize=%f", CompGridSize);
	WriteLn(fp, str1);
	sprintf(str1, "AreafillGridSize=%f", AreafillGridSize);
	WriteLn(fp, str1);

	sprintf(str1, "AreafillPen1=%f", GerberInfo.AreaFillPen1);
	WriteLn(fp, str1);
	sprintf(str1, "AreafillPen2=%f", GerberInfo.AreaFillPen2);
	WriteLn(fp, str1);

	sprintf(str1, "StartWithMaximumView=%i", StartWithMaximumView);
	WriteLn(fp, str1);
	sprintf(str1, "MoveCompAutoZoom=%i", MoveCompAutoZoom);
	WriteLn(fp, str1);
	sprintf(str1, "ButtonInfoTimeout=%i", ButtonInfoTimeout);
	WriteLn(fp, str1);
	sprintf(str1, "ButtonInfoTimeoutStart=%i", ButtonInfoTimeoutStart);
	WriteLn(fp, str1);
	sprintf(str1, "PopupDisplayVisible=%i", PopupDisplayVisible);
	WriteLn(fp, str1);

	sprintf(str1, "ViewOffsetX=%f", Xoffset);
	WriteLn(fp, str1);
	sprintf(str1, "ViewOffsetY=%f", Yoffset);
	WriteLn(fp, str1);
	sprintf(str1, "ViewScale=%.9f", Factor);
	WriteLn(fp, str1);
	sprintf(str1, "PlotPen1=%f", GerberInfo.PenSizes[0]);
	WriteLn(fp, str1);
	sprintf(str1, "PlotPen2=%f", GerberInfo.PenSizes[1]);
	WriteLn(fp, str1);
	sprintf(str1, "BitmapExportResolution=%f", GerberInfo.BitmapExportResolution);
	WriteLn(fp, str1);
	sprintf(str1, "BitmapExportSaveMode=%d", GerberInfo.BitmapExportSaveMode);
	WriteLn(fp, str1);


// ******************************************************************************

	sprintf(str1, "ZoomMode=%i", ZoomMode);
	WriteLn(fp, str1);
	sprintf(str1, "FastPaint=%i", FastPaint);
	WriteLn(fp, str1);
	sprintf(str1, "MousePanMultiply=%i", MousePanMultiply);
	WriteLn(fp, str1);
	sprintf(str1, "DrawGrid=%i", GridVisible);
	WriteLn(fp, str1);
	sprintf(str1, "SnapMode=%i", SnapMode);
	WriteLn(fp, str1);
	sprintf(str1, "CompSnapMode=%i", CompSnapMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawAreaFills=%i", OkToDrawAreaFills);
	WriteLn(fp, str1);

	if (AreafillDrawMode > 1)
		AreafillDrawMode = 0;

	sprintf(str1, "DrawAreaFillMode=%i", AreafillDrawMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawClearances=%i", OkToDrawClearances);
	WriteLn(fp, str1);
	sprintf(str1, "DrawCompOutline=%i", OkToDrawCompOutline);
	WriteLn(fp, str1);
	sprintf(str1, "DrawConnections=%i", OkToDrawConnections);
	WriteLn(fp, str1);
	sprintf(str1, "DrawDrillType=%i", DrawDrillMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawInnerPads=%i", OkToDrawInnerPads);
	WriteLn(fp, str1);
	sprintf(str1, "DrawTopPads=%i", OkToDrawTopPads);
	WriteLn(fp, str1);
	sprintf(str1, "DrawBottomPads=%i", OkToDrawBottomPads);
	WriteLn(fp, str1);
	sprintf(str1, "DrawTopComponents=%i", DrawTopComponents);
	WriteLn(fp, str1);
	sprintf(str1, "DrawBottomComponents=%i", DrawBottomComponents);
	WriteLn(fp, str1);
	sprintf(str1, "DrawCompPlacement=%i", OkToDrawCompPlacement);
	WriteLn(fp, str1);
	sprintf(str1, "DrawSilkScreenTop=%i", OkToDrawSilkScreenTop);
	WriteLn(fp, str1);
	sprintf(str1, "DrawSilkScreenBottom=%i", OkToDrawSilkScreenBottom);
	WriteLn(fp, str1);
	sprintf(str1, "DrawSoldMaskBottomMode=%i", DrawSoldMaskBottomMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawSoldMaskTopMode=%i", DrawSoldMaskTopMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawPasteMaskBottomMode=%i", DrawPasteMaskBottomMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawPasteMaskTopMode=%i", DrawPasteMaskTopMode);
	WriteLn(fp, str1);
	sprintf(str1, "DrawInfoObjects=%i", OkToDrawInfoObjects);
	WriteLn(fp, str1);
	sprintf(str1, "DrawBoardOutline=%i", OkToDrawBoardOutline);
	WriteLn(fp, str1);
	sprintf(str1, "DrawInfo2Objects=%i", OkToDrawInfo2Objects);
	WriteLn(fp, str1);
	sprintf(str1, "DrawInfo3Objects=%i", OkToDrawInfo3Objects);
	WriteLn(fp, str1);
	sprintf(str1, "DrawInfo4Objects=%i", OkToDrawInfo4Objects);
	WriteLn(fp, str1);
	sprintf(str1, "DrawRoutingKeepoutTop=%i", OkToDrawRoutingKeepoutTop);
	WriteLn(fp, str1);
	sprintf(str1, "DrawRoutingKeepoutBottom=%i", OkToDrawRoutingKeepoutBottom);
	WriteLn(fp, str1);
	sprintf(str1, "DrawRoutingKeepoutInner=%i", OkToDrawRoutingKeepoutInner);
	WriteLn(fp, str1);
	sprintf(str1, "DrawVias=%i", OkToDrawVias);
	WriteLn(fp, str1);
	sprintf(str1, "DrawViaClearances=%i", OkToDrawViaClearances);
	WriteLn(fp, str1);
	sprintf(str1, "DrawCompReference=%i", OkToDrawCompReference);
	WriteLn(fp, str1);
	sprintf(str1, "DrawCompValue=%i", OkToDrawCompValue);
	WriteLn(fp, str1);
	sprintf(str1, "DrawTwoTryingTraces=%i", DrawTwoTryingTraces);
	WriteLn(fp, str1);
	sprintf(str1, "DrawCrossHair=%i", OkToDrawCrossHair);
	WriteLn(fp, str1);
	sprintf(str1, "RepeatModeActive=%i", RepeatModeActive);
	WriteLn(fp, str1);
	sprintf(str1, "RePaintAfterCompMove=%i", OkToRePaintAfterCompMove);
	WriteLn(fp, str1);
	sprintf(str1, "RePaintAfterTraceDrawing=%i", OkToRePaintAfterTraceDrawing);
	WriteLn(fp, str1);
	sprintf(str1, "DoNotShowAreafillThinLinesError=%i", DoNotShowAreafillThinLinesError);
	WriteLn(fp, str1);
	sprintf(str1, "SelectionMode=%i", ReplaceSelections);
	WriteLn(fp, str1);
	sprintf(str1, "RecalcAreafillAfterInsert=%i", RecalcAreafillAfterInsert);
	WriteLn(fp, str1);
	sprintf(str1, "MouseCursorOnGrid=%i", MouseCursorOnGrid);
	WriteLn(fp, str1);
	sprintf(str1, "ComponentConnectionMode=%i", ComponentConnectionMode);
	WriteLn(fp, str1);
	sprintf(str1, "ViewSingleLayer=%i", ViewSingleLayer);
	WriteLn(fp, str1);


// ******************************************************************************

	WriteLn(fp, "");

	for (cnt = 0; cnt < 32; cnt++)
	{
		sprintf(str1, "Layer%i=%i", cnt, (DrawLayerCode[cnt] >> 4) ^ 1);
		WriteLn(fp, str1);
	}

	WriteLn(fp, "");

	for (cnt = 0; cnt < NrGridSizes; cnt++)
	{
		sprintf(str1, "Grid%i=%.3f", cnt, GridSizes[cnt]);
		WriteLn(fp, str1);
	}

	WriteLn(fp, "");

	for (cnt = 0; cnt < NrTraceWidths; cnt++)
	{
		sprintf(str1, "TraceWidth%i=%.1f", cnt, TraceWidths[cnt]);
		WriteLn(fp, str1);
	}

	WriteLn(fp, "");

	for (cnt = 0; cnt < NrClearanceWidths; cnt++)
	{
		sprintf(str1, "ClearanceWidth%i=%.1f", cnt, ClearanceWidths[cnt]);
		WriteLn(fp, str1);
	}

	WriteLn(fp, "");
// ******************************************************************************

	for (cnt = 0; cnt < NrLayerObjects; cnt++)
	{
		if (LayerObjectCodes[cnt].TextStr[0] != 0)
		{
			sprintf(str1, "%sColor=%i", LayerObjectCodes[cnt].TextStr,
			        (int32) PCBColors[LayerObjectCodes[cnt].ColorNr]);
			WriteLn(fp, str1);
		}
	}

	for (cnt = 0; cnt < NrLayerObjects; cnt++)
	{
		if (LayerObjectCodes[cnt].TextStr[0] != 0)
		{
			sprintf(str1, "%sColor2=%i", LayerObjectCodes[cnt].TextStr,
			        (int32) PCBColors2[LayerObjectCodes[cnt].ColorNr]);
			WriteLn(fp, str1);
		}
	}

	for (cnt = 0; cnt < NrLayerObjects; cnt++)
	{
		if (LayerObjectCodes[cnt].TextStr[0] != 0)
		{
			sprintf(str1, "%sBrush=%i", LayerObjectCodes[cnt].TextStr, PCBObjectCodes[LayerObjectCodes[cnt].ColorNr]);
			WriteLn(fp, str1);
		}
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

	WriteLn(fp, "");
	FileClose(fp);
}

void DialogResources(int32 DialogResourceValue) 
{
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddPcbLanguageString(int32 ID, LPSTR Text)
{
	int32 Length;
	WCHAR TextW[MAX_LENGTH_STRING];

	Length = strlen(Text);

	if (Length == 0)
		return -1;

	if ((ID < 0) || (ID >= MaxNrPcbNames))
		return -1;

	if (PcbNamesPos + Length + 2 >= PcbNamesBufSize)
		return -1;

	if (!Utf8ToUnicode(Text, TextW, MAX_LENGTH_STRING))
		return -1;

	PcbNamesId[ID] = (LPSTR) & PcbNamesBuf[PcbNamesPos];
	memmove(&PcbNamesBuf[PcbNamesPos], Text, Length + 1);
	PcbNamesPos += Length + 1;
	TotalNrPcbNames++;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddPcbLanguageStrings(LPSTR FileName)
{

	int32 fp, cnt2, str3length, count, StringNr, Length;
	char LineBuf[512], str2[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], LanguageFileName[MAX_LENGTH_STRING],
	     str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 ok;
#endif

	count = 0;

	if (FileName[0] == 0)
		return -1;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
	{
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
		    sprintf(str,"Can not read from language file %s",LanguageFileName);
		    MessageBoxOwn(NULL,str,"System error",MB_APPLMODAL|MB_OK);
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
#ifdef _DEBUG

				if (StringNr == 1025)
					ok = 1;

#endif
				DecodeQuotedString(str2, str3);
				str3length = (int32) strlen(str3);

				for (cnt2 = 0; cnt2 < str3length; cnt2++)
				{
					if (((uint8) str3[cnt2] < (uint8) ' ') && (str3[cnt2] != '\r') && (str3[cnt2] != '\t')
					        && (str3[cnt2] != '\f') && (str3[cnt2] != '\n'))
						str3[cnt2] = ' ';
				}

				AddPcbLanguageString(StringNr, str3);
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
	char *FileName;
	int32 cnt, lengte, pos;

	pos = 0;
	lengte = 1000;

	for (cnt = 0; cnt < NrParams; cnt++)
	{
		if (Parameters[cnt].Option[0] != 0)
		{
			if (strnicmp(Parameters[cnt].Option, "e", lengte) == 0)
				strcpy(ExePath, (LPSTR) & Parameters[cnt].Parameter[pos]);

			if (strnicmp(Parameters[cnt].Option, "o", lengte) == 0)
				ProjectActive = 1;

			if (strnicmp(Parameters[cnt].Option, "r", lengte) == 0)
			{
				strcpy(SearchForReferenceString, (LPSTR) & Parameters[cnt].Parameter[pos]);
				CheckReference = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "r2", lengte) == 0)
			{
				strcpy(SearchForPartNrString, (LPSTR) & Parameters[cnt].Parameter[pos]);
				CheckPartNr = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "u", lengte) == 0)
			{
				strcpy(ProjectPath, (LPSTR) & Parameters[cnt].Parameter[pos]);
				FoundProjectPath = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "x", lengte) == 0)
			{
				strcpy(DesignFile, (LPSTR) & Parameters[cnt].Parameter[pos]);
				GetDirFromFileName(DesignPath, DesignFile);
				FoundDesignPath = 1;
			}

			if (strnicmp(Parameters[cnt].Option, "z", lengte) == 0)
				strcpy(ExecuteCommands, (LPSTR) & Parameters[cnt].Parameter[pos]);
		}
		else
		{
			if ((mode == 0) && (cnt > 0) && (Parameters[cnt].Parameter[0] != 0))
			{
				if (GetFullPathName(Parameters[cnt].Parameter, MAX_LENGTH_STRING - 50, EditFile, &FileName) == 0)
				{
					MessageBoxOwn(PCBWindow, Parameters[cnt].Parameter, SC(253, "Error in opening file"),
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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmd, int32 nCmdShow)
{
	MSG M;
	int32 count, code, res, KeySize;
	uint32 EAX, EBX, ECX, EDX, FlagSSE2 = 0;
	char str[MAX_LENGTH_STRING], *TestCommandLine, vendor[40], *env;
	HKEY Key;

#ifdef _DEBUG
	HMENU EditMenu, MainMenu;
	ok = 1;
#endif

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

	QueryPerformanceCounter((LARGE_INTEGER *) & SpecialCounterProgramStart);
	GetLocalTime(&CurrentDateProgramStart);
	TestCommandLine = GetCommandLine();
	GetParameters(NULL);
	GetDirFromFileName(ExecutableDir, Parameters[0].Parameter);
	DecodeParameters(0);		// EditFile

	if (ExePath[0] == 0)
		strcpy(ExePath, ExecutableDir);

	strcpy(PcbExecutable, Parameters[0].Parameter);

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

	strcpy(ExportDir, ProjectPath);

	WindowWidth = 0;
	WindowHeight = 0;
	WindowStartX = 0;
	WindowStartY = 0;
	IniFile[0] = 0;
	FastPaint = 1;
	LeftButtonPressed = 0;
	MiddleButtonPressed = 0;
	RightButtonPressed = 0;
	srand((unsigned) time(NULL));

	InitTimers();
	MemoryMain();
	ToetsMain();
	LoadDesignIniFile();

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
		sprintf(str, "%s\\LanguagePcb.txt", LanguagePath);
	else
		//sprintf(str, "%s\\LanguagePcb.txt", ExecutableDir);
		strcpy(str, "");

	if (AddPcbLanguageStrings(str) == -2)
		return -1;

	strcpy(EmptyNet.Name, SC(538, "<NONE>"));
	sprintf(str, "%s\\special_debug_bflw3nlncwecwnwfgre", ExePath);

	if (FileExistsUTF8(str) == 0)
		SpecialDebugFile = 1;

	if (ProjectActive)
	{
		OkToUseSharedMemory = 0;

		if ((SharedMemoryHandle = OpenFileMapping(FILE_MAP_WRITE, 0, MEMORYMAPPEDSTRING)))
		{
			SharedMemory = (uint8 *) MapViewOfFile(SharedMemoryHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			if (SharedMemory != NULL)
			{
				OkToUseSharedMemory = 1;
				ProjectInfo = (ProjectInfoRecord *) SharedMemory;
			}

			ok = 1;
		}
	}

	SetCursor(LoadCursor(0, CursorType));
	CurrentCursor = GetCursor();
	FontResourceHandle = FindResource(NULL, MAKEINTRESOURCE(IDR_OWNFONT1), "OWNFONT");
	count = SizeofResource(NULL, FontResourceHandle);
	FontGlobal = LoadResource(NULL, FontResourceHandle);
	FontData = LockResource(FontGlobal);
	memmove(Chars, FontData, count);
	code = 127 - 33;
	(*Chars)[code].NrPolyLines = 1;
	(*Chars)[code].Line[0] = (float) 0.0;
	(*Chars)[code].Line[1] = (float) 0.1;
	(*Chars)[code].Line[2] = (float) 0.4;
	(*Chars)[code].Line[3] = (float) 0.0;
	(*Chars)[code].Line[4] = (float) 0.1;
	(*Chars)[code].Line[5] = (float) 1.3;
	(*Chars)[code].Line[6] = (float) 0.0;
	(*Chars)[code].Line[7] = (float) 0.6;
	(*Chars)[code].Line[8] = (float) 1.3;
	(*Chars)[code].Line[9] = (float) 0.0;
	(*Chars)[code].Line[10] = (float) 0.6;
	(*Chars)[code].Line[11] = (float) 0.4;
	(*Chars)[code].Line[12] = (float) 0.0;
	(*Chars)[code].Line[13] = (float) 0.1;
	(*Chars)[code].Line[14] = (float) 0.4;
	(*Chars)[code].Line[15] = (float) 1.0;

	if (hPrevInstance == 0)
	{
		PCBClass.hInstance = hInstance;
		PCBClass.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(ICON_PCB));
		PCBClass.hCursor = LoadCursor(0, CursorType);
		PCBClass.hbrBackground = 0;
		PCBClass.lpszMenuName = MAKEINTRESOURCE(IDR_MENU1);

		if (!RegisterClass(&PCBClass))
			exit(255);
	}

	LoadBitMaps();
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

	ScreenSizeX = GetSystemMetrics(SM_CXMAXIMIZED) - 10;
	ScreenSizeY = GetSystemMetrics(SM_CYMAXIMIZED) - 10;
	ScreenStartX = 0;
	ScreenStartY = 0;

	ChangeFile(NULL, 2);
	AdjustWindowSize();

	ClipID2 = RegisterClipboardFormat("Selections Schematic file");
	ClipBoardLayoutTracesViasID = RegisterClipboardFormat("Traces/vias layout file");
	ClipBoardLayoutComponentsID = RegisterClipboardFormat("Components layout file");

	ClosingWindowMessage = RegisterWindowMessage("CLOSING_WINDOW");
	memset(&ClientRect, 0, sizeof(RECT));

	if ((TestAtom = GlobalFindAtom(AtomStrSchematicSelect)) != 0)
		GlobalDeleteAtom(TestAtom);

	HeightScrollBar = GetSystemMetrics(SM_CYHSCROLL), WidthScrollBar = GetSystemMetrics(SM_CXVSCROLL);

	if (CompMemoryError)
		MessageBoxOwn(PCBWindow, SC(946, "Error in components\r\n\r\nUpdate the netlist"), SC(24, "Error"), MB_OK);

	ok = 1;
	PCBWindow = CreateWindow(PCBClass.lpszClassName, 0,
//                           WS_OVERLAPPEDWINDOW | WS_VSCROLL,
	                         WS_OVERLAPPEDWINDOW, WindowStartX, WindowStartY, WindowWidth, WindowHeight, HWND_DESKTOP,
	                         0, hInstance, NULL);
	TimerObject = SetTimer(PCBWindow, TimerIdentifier, NR_MILLI_SECONDS_TIMER, NULL);

	hwndHBar =
	    CreateWindow("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_HORZ, 0, 0, 0, 0, PCBWindow, (HMENU) 1, hInstance,
	                 NULL);
	hwndVBar =
	    CreateWindow("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_VERT, 0, 0, 0, 0, PCBWindow, (HMENU) 2, hInstance,
	                 NULL);

	if (ProjectActive)
		InsertWindowInProject(PCBWindow, 0);

	MaxScrollBarX = 10000;
	MaxScrollBarY = 10000;
	SetScrollRange(hwndHBar, SB_CTL, 0, MaxScrollBarX, 1);
	SetScrollRange(hwndVBar, SB_CTL, 0, MaxScrollBarY, 1);


	CheckExpandedCtrlKeys(0);
	MakeMainMenu();
	MakeCheckMenu();

#ifdef _DEBUG
	MainMenu = GetMenu(PCBWindow);
	EditMenu = GetSubMenu(MainMenu, 1);
	AppendMenuUTF8(EditMenu, MF_ENABLED | MF_STRING, ID_MEMORY_USE, SC(505, "Memory use"));
#endif

	if (Units == 0)
		PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MILS, (LPARAM) NULL);
	else
		PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MM, (LPARAM) NULL);

	PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_MOVE_COMPS2, (LPARAM) 1);
	ShowWindow(PCBWindow, nCmdShow);
	UpdateWindow(PCBWindow);
	SetWindowName(0);

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(24, "Error"), 0, 0);
		DeAllocateMemMessageBuf();
	}

	while ((!TotalExit) && (GetMessage(&M, 0, 0, 0)))
	{
		TranslateMessage(&M);
		DispatchMessage(&M);

		if (Focused)
			MainLoop();
	}

	if (LayoutEditorAtom)
		GlobalDeleteAtom(LayoutEditorAtom);

	KillTimer(PCBWindow, TimerIdentifier);

	if (Created)
		DestroyWindow(PCBWindow);

	ClipCursor(NULL);

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
