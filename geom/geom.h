/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: geom.h
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


#ifndef _GEOM

#define _GEOM

#include "types.h"


LRESULT CALLBACK GEOMWinProc(HWND Window, uint32 Message, WPARAM WParam, LPARAM LParam);

extern WNDCLASS GEOMClass;
extern HWND GEOMWindow, DialogWindow;
extern PAINTSTRUCT PS;
extern RECT RealWindow, ClientRect;
extern HRGN EditingRegion;
extern HMENU PopUpMenu, MainMenu;
extern int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY;


void DoneDeviceContext(void);

void InitDeviceContext(void);

void StartDrawingEditingWindow(void);

void EndDrawingEditingWindow(void);

void RedrawAbsPosStr(int32 Mode);

void RedrawAbsGridPosStr(int32 Mode);

void RedrawRelPosStr(int32 Mode);

void RedrawInfoStr(int32 Mode);

void RedrawMainWindow(void);

void RedrawButtons(void);

void RedrawInfoBar(void);

void LoadIniFile(LPSTR FileName, int32 mode);

void ScrollUp(int32 SizeOfScroll);

void ScrollDown(int32 SizeOfScroll);

void ScrollLeft(int32 SizeOfScroll);

void ScrollRight(int32 SizeOfScroll);

void ScrollAppWindow(int32 DivX, int32 DivY);

void ClipMouseCursor(void);

void UnClipMouseCursor(void);

void SetScrollPageSize(void);

void SetScrollPosition(void);

void SaveViewPos(void);

void LoadUserIniFile(void);

#endif
