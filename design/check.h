/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: check.h
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


#ifndef _CHECK

#define _CHECK

#include "types.h"

int32 CheckWiresAndBusses(int32 SheetMode);

int32 CheckNetLabels(int32 SheetMode, double *cx, double *cy);

int32 CheckBusConnections(int32 SheetMode, double *cx, double *cy);

int32 CheckUnusedBusConnections(int32 SheetMode, double *cx, double *cy);

int32 CheckGlobalConnections(int32 SheetMode, double *cx, double *cy);

int32 CheckInstances(int32 SheetMode, double *cx, double *cy);

int32 CheckUnusedGlobalConnections(int32 SheetMode, double *cx, double *cy);

void ClearErrors(int32 mode);

int32 CheckSheetForErrors(LPSTR Filename, int32 SheetMode, double *cx, double *cy);

void CheckClipBoard(void);

int32 LineCrosses(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                  double LineX4, double LineY4);

#endif
