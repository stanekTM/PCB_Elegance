/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select2.h
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


#ifndef _SELECT2

#define _SELECT2


#include "types.h"

void SelectObjectOnPointer(double x11, double y11, double x22, double y22, int32 mode);

void DrawTraceFromGuide(double PointX, double PointY);

void DrawTraceFromTrace(double PointX, double PointY);

void DrawTraceFromPinOrVia(double PointX, double PointY);

int32 FindEndPointFromNetObjects(double StartX, double StartY, double EndX, double EndY, double ThickNess,
                                 int32 Direction, int32 Layer, double *NewX, double *NewY);

int32 FindObjectNrFromNetObjects(ObjectRecord * SearchObject);

int32 SelectTraceUnderCursor(int32 mode);

#endif
