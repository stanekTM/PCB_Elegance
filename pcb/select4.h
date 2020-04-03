/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select4.h
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

#ifndef _SELECT4

#define _SELECT4

#include "types.h"

int32 FindLimitTrace(double StartX, double StartY, double EndX, double EndY, double TraceWidth, double TraceClearance,
                     double *NewEndX, double *NewEndY, int32 NetNr, int32 Layer);

void ProtectComponents(void);

void UnprotectComponents(void);

void FindNextConnection(void);

int32 GetInfoStr(LPSTR InfoStr, int32 mode);

void SelectOnlyTraces(void);

void SelectOnlyVias(void);

int32 SelectNetViasTraces(int32 mode);

int32 SelectNetAreaFills(int32 mode);

void SelectOnlyObjects(int32 mode);

void UnselectObjects(int32 mode);


#endif
