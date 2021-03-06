/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: mainloop.h
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



#ifndef _MAINLOOP

#define _MAINLOOP

extern int32 MaxCollections;

void DisplayCursorPosition(void);

int32 ZoomActive(void);

int32 PanActive(void);

void DrawCrossHair(int32 mode);

void CheckInputMessages(int32 DelayInMilleSeconds);

void CheckForEscape(void);

int32 CheckLeftButton(void);

int32 CheckRightButton(DrawXorFunctionRecord * DrawXorFunction);

void MainLoop(void);

void MouseSelection(void);

void ExecuteKeys(void);

void ScrollSCHWindow(void);

void CheckZoomFactor(void);

void ZoomIn(int32 mode);

void ZoomOut(int32 mode);

void ViewFull(int32 mode);

void PreviousView(void);

void RePaint(void);

void ViewPan(int32 mode);

void ExitProgram(void);

int32 CenterScreenOnInstance(int32 mode);

void CenterScreen(double cx, double cy, int32 mode);

#endif
