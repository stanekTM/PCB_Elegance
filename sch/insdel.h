/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: insdel.h
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



#ifndef _INSDEL

#define _INSDEL

#include "types.h"

InstanceRecord *FindFirstInstance(int32 mode);

int32 ZeroUnusedObjects(int32 mode);

void RemoveDeletedObjects(void);

void DeleteSelectedObjects(int32 Mode);

int32 AddObject(ObjectRecord * Object);

int32 AddSymbol(SymbolRecord * Symbol);

int32 AddInstance(InstanceRecord * Instance);

int32 AddWire(WireRecord * Wire);

int32 CommandAddTryingWire(WireRecord * Wire, int32 Mode);

int32 AddBus(BusRecord * Bus);

int32 CommandAddTryingBus(BusRecord * Bus, int32 Mode);

int32 AddJunction(JunctionRecord * Junction);

int32 AddOnePinNet(OnePinNetRecord * OnePinNet);

int32 AddNetLabel(NetLabelRecord * NetLabel);

int32 AddBusConnection(BusConnectionRecord * BusConnection);

int32 AddPin(PinRecord * Pin);

int32 AddPowerPin(PowerPinRecord * PowerPin);

int32 AddPinBus(PinBusRecord * PinBus);

int32 AddRedefinedPinBus(RedefinedPinBusRecord * RedefinedPinBus);

int32 AddGlobalConnection(GlobalConnectionRecord * GlobalConnection);

int32 AddObjectLine(ObjectLineRecord * ObjectLine);

int32 AddObjectRect(ObjectRectRecord * ObjectRect);

int32 AddObjectCircle(ObjectCircleRecord * ObjectCircle);

int32 AddObjectArc(ObjectArcRecord * ObjectArc);

int32 AddObjectText(ObjectTextRecord * ObjectText);

void UndoObjects(void);

void RedoObjects(void);

void MemInfo(int32 * TotalMemSize, int32 * DeletedMemSize);

int32 RebuildJunctions(int32 mode, double *FailedJunctionX, double *FailedJunctionY);

int32 PlaceJunctionOnEndPoint(double x, double y, int32 mode);

#endif
