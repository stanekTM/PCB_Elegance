/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: line2.h
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


#ifndef _LINE2

#define _LINE2

#include "owntypes.h"

void DrawLine(int32 x1, int32 y1, int32 x2, int32 y2);

void DrawHorLine(int32 x1, int32 y1, int32 x2, int32 Height, int32 mode);

void DrawVerLine(int32 x1, int32 y1, int32 y2, int32 Height, int32 mode);

void DrawDiag1Line(int32 x1, int32 y1, int32 x2, int32 Height, int32 mode);

void DrawDiag2Line(int32 x1, int32 y1, int32 x2, int32 Height, int32 mode);

void DrawLine2(int32 x1, int32 y1, int32 x2, int32 y2, int32 Height, int32 mode);

#endif
