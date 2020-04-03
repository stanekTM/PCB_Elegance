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

void SelectTracesFromRectWindow(void);

void SelectCompsFromRectWindow(int32 mode);

void SelectViasFromRectWindow(void);

void SelectConnectionsFromRectWindow(void);

void SelectReferencesFromRectWindow(int32 Mode);

void SelectCompValuesFromRectWindow(int32 Mode);

void SelectAreaFillsFromRectWindow(int32 Mode);

void SelectObjectLinesFromRectWindow(int32 Mode);

void SelectObjectRectsFromRectWindow(int32 Mode);

void SelectObjectCirclesFromRectWindow(int32 Mode);

void SelectObjectArcsFromRectWindow(int32 Mode);

void SelectObjectTexts2FromRectWindow(int32 Mode);

void SelectObjectPolygonsFromRectWindow(int32 Mode);

int32 CopySelectedTracesToObjects4(int32 mode, int32 SelectedLayer);

int32 CopySelectedViasToObjects4(int32 mode, int32 SelectedLayer);

int32 CopySelectedConnectionsToObjects4(int32 mode, int32 SelectedLayer);

void GetMaxRectSelectedObjects(int32 mode, double *MinX, double *MinY, double *MaxX, double *MaxY);

int32 IndexSelectedObjects(int32 CopyMode, int32 * SelectedObjects, int32 MaxSelectedObjects, int32 mode);

int32 GetNrObjectSelections(int32 Layer, int32 mode);

int32 GetNrTracesSelected(void);

int32 GetNrViasSelected(void);

int32 GetNrConnectionsSelected(void);

int32 GetNrCompsSelected(void);

int32 GetNrReferencesSelected(void);

int32 GetNrCompValuesSelected(void);

int32 GetNrAreaFillsSelected(void);

int32 GetNrPowerPlanes(void);

void GetInfoSelectedObjects(int32 mode);

int32 SelectOnlyObjectOnLayer(int32 Layer, int32 mode);

int32 UnselectObjectOnLayer(int32 Layer, int32 mode);

#endif
