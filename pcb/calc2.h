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


int32 ObjectsConnected(ObjectRecord * Object1a, ObjectRecord * Object2a);

int32 ObjectsConnected2(ObjectRecord * Object1, ObjectRecord * Object2);

int32 ObjectsConnected3(ObjectRecord * Object1, ObjectRecord * Object2, double *Length);

int32 ObjectsConnected4(ObjectRecord * Object1, ObjectRecord * Object2);

double ObjectsDistance(ObjectRecord * Object1, ObjectRecord * Object2);

#endif
