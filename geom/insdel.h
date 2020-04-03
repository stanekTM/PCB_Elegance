/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: insdel.h
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



#ifndef _INSDEL

#define _INSDEL

#include "types.h"

int32 ZeroUnusedObjects(int32 mode);

void UndoObjects(void);

void RedoObjects(void);

void DeleteObjects(void);

int32 AddObject(ObjectRecord * Object);

int32 AddObjectPolygon(ObjectPolygonRecord * ObjectPolygon);

int32 AddObjectPolygon2(int32 PolygonIndex);

int32 AddPinInfo(PinInfoRecord * PinInfoObject);

int32 InsertDialogObjects(int32 DialogMode);

void InsertObjects2(double OffsetX, double OffsetY);

void AddSILOnObjectsSelected(int32 mode);

#endif
