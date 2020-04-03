/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: toets.h
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


#ifndef _TOETS

#define _TOETS

#include "windows.h"
#include "types.h"


// ******************************************************************************
// ******************************************************************************

/*   global toets variables */


extern int32 TranslateKey, AltPressed, CtrlPressed, ShiftPressed;

extern int32 NrFunctionsInBuf;

/*   global toets functions */

void DecodeKey(int32 Toets);

void KeyChar(int32 Vtkey);

void KeyDown(int32 Vtkey);

void KeyUp(int32 Vtkey);

int32 KeyPressed(void);

int32 ReadKeyFunction(void);

void InsertFunction(int32 Func);

int32 GetKeyValue(LPSTR KeyString);

int32 GetKeyString(LPSTR KeyString, int32 Function, int32 mode);

int32 GetSystemFunction(int32 KeyFunction, int32 mode);

int32 GetFunctionByTranslatedKey(int32 Key, int32 SystemMode);

int32 SetKeyOnKeyFunctionString(int32 Key, LPSTR FunctionString);

int32 CheckExpandedCtrlKeys(int32 mode);

int32 LoadDefaultKeys(int32 mode);

void ToetsMain(void);

#endif
