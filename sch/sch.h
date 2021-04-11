/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: sch.h
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


#ifndef _SCH

#define _SCH

#include "types.h"

LRESULT CALLBACK SCHWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam);

extern WNDCLASS SCHClass;
extern HWND SCHWindow, DialogWindow;
extern PAINTSTRUCT PS;
extern RECT RealWindow, ClientRect;
extern HRGN EditingRegion;
extern HMENU PopUpMenu, MainMenu;
extern int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY;


void CloseApplication(LPSTR SourceFile, int32 LineNr);

int32 CheckEditorVersion(void);

void SetWaitCursor(void);

void SetNormalCursor(void);

int32 SetNewCursor(double *CurrentX, double *CurrentY);

void DoneDeviceContext(void);

void InitDeviceContext(void);

HWND GetDesignManagersWindow(int32 mode);

void StartDrawingEditingWindow(void);

void EndDrawingEditingWindow(void);

void RedrawAbsPosStr(int32 Mode);

void RedrawRelPosStr(int32 Mode);

void RedrawDeltaPosStr(int32 Mode);

void RedrawInfoStr(int32 Mode);

void RedrawMainWindow(void);

void RedrawButtons(void);

void RedrawInfoBar(void);

void WindowPaint(int32 mode);

void InitTraces(void);

void WindowCreate(void);

void CheckEditFileName(void);

void ScrollUp(int32 SizeOfScroll);

void ScrollDown(int32 SizeOfScroll);

void ScrollLeft(int32 SizeOfScroll);

void ScrollRight(int32 SizeOfScroll);

void WindowDestroy(void);

void WriteIniFile(LPSTR FileName);

void ClipMouseCursor(void);

void UnClipMouseCursor(void);

void SetScrollPageSize(void);

void SetScrollPosition(void);

void ScrollAppWindow(int32 DivX, int32 DivY);

void SaveViewPos(void);

void LoadUserIniFile(void);

#endif
