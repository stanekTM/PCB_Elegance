/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw2.h
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


#ifndef _DRAW2

#define _DRAW2

#include "types.h"
#include "owntypes.h"

void DrawObjects(int32 Mode);

void DrawPinTextObject(ObjectRecord * Object, int32 mode);

void DrawObjects2(double ox, double oy, int32 Mode);

void DrawObjects2a(double ox, double oy, double ox2, double oy2, int32 Mode);

void DrawObjectPolygons(int32 Mode);

void DrawObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OX, double OY, int32 Mode);

void DrawPolygonObjectWithClearance(ObjectPolygonRecord * ObjectPolygon, double OX, double OY, int32 Mode);

void DrawObject(ObjectRecord * Object, double ox, double oy, int32 Mode);

void DrawObjectWithClearance(ObjectRecord * Object, double ox, double oy, int32 Mode);

void DrawText2(double x1, double y1, LPSTR TextStr);

void DrawGrid(void);

void InitSpecialDraw(MEASUREITEMSTRUCT * MeasureItem);

void DrawSpecialItem(DRAWITEMSTRUCT * DrawItem);

#endif
