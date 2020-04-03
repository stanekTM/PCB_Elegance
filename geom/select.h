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

void SelectObjectsFromWindow(double x1a, double y1a, double x2a, double y2a, int32 mode);

int32 ObjectLayerVisible(int32 Layer);

int32 FindFirstObjectUnderCursor(double x, double y);

int32 GetNrSelectObjects(void);

void ChangeSelections(int32 Modifier);

void GetMinMaxSelectedObjects(void);

void FindMinMaxBoard(double *OriginX, double *OriginY, double *Width, double *Height, int32 mode);

void ObjectsInfo(void);

int32 AddToMessageBuf(LPSTR Line);

int32 GetObjectSize(ObjectRecord * Object, double *MinX, double *MinY, double *MaxX, double *MaxY);

#endif
