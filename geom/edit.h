/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edit.h
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


#ifndef _TRACE4

#define _TRACE4

#include "types.h"

void CommandAddObjects(int32 ObjectType, double LineThickNess, int32 Layer, int32 ObjectMode);

void CommandAddMultipleObjects(int32 mode);

void CommandChangedCrosses(double OX, double OY, int32 Mode);

int32 CopyObjectsToClipBoard(int32 mode);

int32 CopyObjectsFromClipBoard(int32 mode);

int32 ChangeNrLayers(int32 mode);

int32 ChangeGeometryHeight(int32 mode);

void PrintScreenToBitmap(int32 mode);

int32 RenamePin(int32 mode);

#endif
