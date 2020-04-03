/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edit.h
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


#ifndef _EDIT

#define _EDIT

#include "types.h"

void CommandAddWires(int32 Mode);

void CommandAddBusses(int32 Mode);

void CommandAddJunction(int32 Mode);

void CommandAddOnePinNet(int32 Mode);

void CommandAddNetLabels(int32 Mode);

void CommandAddMultipleNetLabels(int32 Mode);

void CommandAddBusConnection(int32 Mode);

void CommandAddObjectRect(int32 Mode);

void CommandAddObjectCircle(int32 Mode);

void CommandAddObjectArc(int32 Mode);

void CommandAddObjectText(int32 Mode);

void CommandAddObjectTextNumbers(int32 Mode);

int32 CopyToClipBoard(void);

int32 CopyFromClipBoard(void);

int32 CopySelectionsToClipBoard(void);

int32 WireBusEndPointReached(double CurrentX, double CurrentY, int32 mode);

#endif
