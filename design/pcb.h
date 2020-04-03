/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: pcb.h
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


#ifndef _PCB

#define _PCB

#include "types.h"

int32 LoadShape(LPSTR ShapeName);

void ShapePinsToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation, int32 ObjectArrayNr,
                       int32 SelectedLayer, int32 mode);

void ShapePlacementOutLineToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation);

void ShapeCompOutLineToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation);

void ShapeCompSilkScreenToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation);

void ShapeOtherToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation, int32 SelectedLayer,
                        int32 mode);

#endif // _PCB
