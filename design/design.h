/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: design.h
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

#define RGB_Black                               RGB(  0,  0,  0)
#define RGB_DarkGray                            RGB( 64, 64, 64)
#define RGB_MidGray                             RGB( 96, 96, 96)
#define RGB_Gray                                RGB(128,128,128)
#define RGB_LightGray                           RGB(192,192,192)
#define RGB_LightGray2                          RGB(216,216,216)
#define RGB_White                               RGB(255,255,255)

#define RGB_Red                                 RGB(255,  0,  0)
#define RGB_Blue                                RGB(  0,  0,255)
#define RGB_Green                               RGB(  0,255,  0)

#define RGB_Yellow                              RGB(255,255,  0)
#define RGB_Cyan                                RGB(  0,255,255)
#define RGB_Magenta                             RGB(255,  0,255)

#define RGB_DarkMagenta                         RGB(170,  0,170)
#define RGB_DarkCyan                            RGB(  0,170,170)
#define RGB_DarkBlue                            RGB(  0,  0,170)
#define RGB_DarkRed                             RGB(170,  0,  0)
#define RGB_DarkGreen                           RGB(  0,170,  0)
#define RGB_DarkMagenta                         RGB(170,  0,170)

#define RGB_LightRed                            RGB(255,102,102)
#define RGB_LightBlue                           RGB(120,120,255)
#define RGB_LightGreen                          RGB(102,255,102)
#define RGB_LightMagenta                        RGB(255,102,255)
#define RGB_LightBrown                          RGB(192,128,64 )
#define RGB_Orange                              RGB(255,128,0  )
#define RGB_Pink                                RGB(236,151,227)
#define RGB_DarkPink                            RGB(207, 37,190)
#define RGB_LightPink                           RGB(255,200,240)
#define RGB_Brown                               RGB(128,64 ,0  )
#define RGB_LightOrange                         RGB(223,188,96 )
#define RGB_Violet                              RGB(128,0  ,128)

#define PCB_ELEG_ENVIRONMENT_STRING             "PCB_ELEG_ENVIRONMENT"


LRESULT FAR PASCAL SCHWinProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam);

extern WNDCLASS DESIGNClass;
extern HWND DESIGNWindow, DialogWindow;
extern PAINTSTRUCT PS;
extern RECT RealWindow, ClientRect;
extern HRGN EditingRegion;
extern HMENU PopUpMenu, MainMenu;
extern int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY;

void SetWindowName(LPSTR DesignName, int32 mode);

void DoneDeviceContext(void);

void InitDeviceContext(void);

int32 ActivateProjectWindow(HANDLE WindowHandle);

HWND GetProjectWindow(LPSTR ActiveFile, int32 FileType, int32 mode);

void StartDrawingEditingWindow(void);

void EndDrawingEditingWindow(void);

void SetWaitCursor(void);

void SetNormalCursor(void);

void CreateSheetWindow(void);

void RedrawMainWindow(void);

void WindowPaint(void);

void WindowCreate(void);

void WindowDestroy(void);

void SaveOpenFiles(int32 mode);

void ReloadSchematics(int32 mode);

void CloseOpenFiles(int32 mode);

#endif
