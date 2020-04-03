/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: movecomp.h
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


#ifndef _MOVECOMP

#define _MOVECOMP

#include "types.h"

void MoveSelectedObjects(int32 Mode, int32 Count);

void MoveSelectedObjectsToZero(int32 Mode, int32 Count);

void DrawSelectedObjects(double CurrentX, double CurrentY, int32 Mode);

void PlaceRotatedFlippedComponents(int32 Mode);

void MoveAllObjects(double x, double y);

int32 AssignObjectsToPin(ObjectRecord * ObjectText);

void ChangeTraceWidth(double value);

void ChangeClearance(double value, int32 mode);

void ChangeLineWidth(double value);

void Changetext(void);

void ChangetextHeight(void);

int32 ChangeParamsObjects(int32 mode);

void ChangeTraceLayer(int32 NewLayer);

void CopyObjectsToSilkscreen(void);

void CopyObjectsToOtherLayer(int32 Layer, int32 mode);

void CopyOnMultipleCoordinates(void);

void ChangeClearanceAll(double value);

void ScaleSelectedObjects(int32 mode);

int32 AdjustOffsetForSnap(double CursorX, double CursorY, double GridX, double GridY, double divx, double divy,
                          double *ShiftOffsetX, double *ShiftOffsetY, int32 mode);

int32 GetPolygonFromLines(int32 mode);

int32 UnselectFirstObject(int32 mode);

#endif
