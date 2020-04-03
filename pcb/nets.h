/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: nets.h
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


#ifndef _NETS

#define _NETS

#include "types.h"

int32 GetObjectNrFromEndPoint(int32 Layer, double x, double y, int32 mode);

int32 CountNrObjectsEndPoint(int32 Layer, double x, double y, int32 mode);

int32 GetObjectNrFromEndPoint2(int32 Layer, double x, double y);

int32 GetViaObjectNrFromEndPoint(double x, double y);

void GetObjectsNet(int32 NetNr, int32 ObjectArrayNr, int32 Mode);

void DeleteAndUndisplayConnectionsNet(int32 NetNr, int32 mode);

void AddPowerPlaneObjects3(int32 PowerNetNr, int32 mode);

int32 CheckNet(int32 NetNr, int32 mode);

int32 InsertConnections(int32 NetNr, int32 mode);

void ReCalcConnectionsNet(int32 NetNr, int32 mode, int32 RePaint);

void DeleteNet(int32 NetNr);

void DeleteTracesViasNetSelectedTrace(void);

void DeleteTracesVias(void);

void CheckConnectivity(int32 mode);

int32 GetNrObjectsAllNets(void);

int32 GetNetNrSelectedTrace(void);

int32 ShowFirstUnroutedNet(int32 mode);

int32 ChangeNetType(int32 NetNr, double TraceWidth, double Clearance, int32 mode);

int32 ChangeDesignRulesPcb(int32 mode);

#endif
