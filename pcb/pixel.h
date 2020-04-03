/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: pixel.h
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


#ifndef _PIXEL

#define _PIXEL

#include "types.h"

void DeleteAperturesLayer(int32 Layer, int32 mode);

void UpdatePlotObjectsLayer(int32 Layer, int32 mode);

void DeleteObjectsLayer(int32 Layer, int32 mode);

int32 ChangeToolDrills(int32 mode);

int32 FillWithLines(int32 Layer, int32 mode);

int32 ChangeHPGLPen(int32 mode);

int32 ChangeMillingPen(int32 mode);

int32 ChangeParamsObjects(int32 mode);

int32 RemoveDeletedMillingLines(int32 mode);

int32 GetMillingPoints(int32 Layer, int32 mode);

int32 GetMillingPointsBitmap(int32 Layer, int32 mode);

int32 BitmapPointsToLines(float HResolution, float VResolution, float PenSize, int32 Layer, int32 mode);

int32 BitmapPointsToLines3(int32 Layer, int32 mode);

#endif
