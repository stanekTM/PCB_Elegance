/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: trace2.h
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


#ifndef _TRACE2

#define _TRACE2

#include "types.h"

int32 InsertTraces(int32 mode, int32 EndPointObjectNr);

int32 InsertTwoTracesOnGuide(int32 mode);

int32 InsertVia(void);

int32 ExtendNewTrace(ObjectRecord * DrawTrace);

void SwapNetsTraces(void);

void DeleteAndUnDisplayObjectTrace(int32 ObjectNr, int32 RePaint);

int32 SwitchToNearestDrawingLayer(int32 mode);

#endif
