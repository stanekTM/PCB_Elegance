/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edit2.h
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


#ifndef _EDIT2

#define _EDIT2

#include "types.h"

void CommandAddPins(int32 Mode);

void CommandAddPowerPins(int32 Mode);

void CommandAddPinBus(int32 Mode);

int32 GetSymbol(LPSTR SymbolName, LPSTR SymbolDir, LPSTR LibName, int32 Mode);

int32 EditInstanceInfo(int32 mode);

void CommandAddGlobalConnection(int32 Mode);

void AddWireLabel(int32 Mode);

int32 AddTextObjectsFromFile(int32 mode);

int32 ReplaceTexts(int32 mode);

#endif
