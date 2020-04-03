/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select.h
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


#ifndef _SELECT

#define _SELECT


#include "types.h"

void SelectObjectsFromWindow(double x1, double y1, double x2, double y2, int32 mode);

void SelectInstancesFromRectWindow(int32 Mode);

void SelectWiresFromRectWindow(int32 Mode);

void SelectBussesFromRectWindow(int32 Mode);

void SelectJunctionsFromRectWindow(int32 Mode);

void SelectOnePinNetsFromRectWindow(int32 Mode);

void SelectNetLabelsFromRectWindow(int32 Mode);

void SelectBusConnectionsFromRectWindow(int32 Mode);

void SelectObjectLinesFromRectWindow(int32 Mode);

void SelectObjectRectsFromRectWindow(int32 Mode);

void SelectObjectCirclesFromRectWindow(int32 Mode);

void SelectObjectArcsFromRectWindow(int32 Mode);

void SelectObjectTextsFromRectWindow(int32 Mode);

void SelectGlobalConnectionsFromRectWindow(int32 Mode);

void SelectPinBussesFromRectWindow(int32 Mode);

void SelectPinsFromRectWindow(int32 Mode);

void SelectPowerPinsFromRectWindow(int32 Mode);

void GetNrSelections(void);

int32 ExtendSelections(int32 Mode);

void ChangeSelections(int32 Mode, int32 Selected);

void ExportText(int32 mode);

int32 GetFirstSymbolSheetSelected(void);

void SearchText(int32 mode);

int32 GetInfoStr(LPSTR InfoStr, int32 mode);

#endif
