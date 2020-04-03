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

void MoveComponents(int32 mode);

void DrawConnecions(double CurrentX, double CurrentY);

void MoveComponents(int32 mode);

int32 RegroupComponents(int32 mode);

void PlaceMovedObjects(double CurrentX, double CurrentY, double Rotation, int32 mode);

void SwitchLayerComp(int32 Layer);

void UnselectComp(int32 mode);

int32 CheckFloatingConnections(void);

int32 EditShapeComponent(int32 mode);

int32 ImportComponentPositions(int32 mode);

int32 AlignComponents(int32 mode);

int32 MoveComponentsOneGridPosition(int32 mode);

int32 AdjustOffsetForSnap(double CursorX, double CursorY, double GridX, double GridY, double divx, double divy,
                          double *ShiftOffsetX, double *ShiftOffsetY, int32 mode);

#endif
