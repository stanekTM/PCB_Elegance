/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc2.h
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



#ifndef _CALC2

#define _CALC2

#include "owntypes.h"
#include "types.h"

int32 CircleTestCircleObjects(double CircleX1, double CircleY1, double CircleThickness1, double CircleX2,
                              double CircleY2, double CircleThickness2);

int32 RectTestCircleObjects(double RectX, double RectY, double RectWidth, double RectHeight, double CircleX,
                            double CircleY, double CircleThickness);


int32 GetCompProperties(InstanceRecord * Instance, LPSTR PropertyID, LPSTR PropertyValue, int32 mode);

int32 GetInstanceRefInfo(InstanceRecord * Instance, double *x, double *y, int32 * Rotation, LPSTR Ref, int32 mode);

#endif
