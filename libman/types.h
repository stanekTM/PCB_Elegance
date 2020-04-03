/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: types.h
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


#ifndef _TYPES

#define _TYPES

#include "windows.h"
#include "owntypes.h"

typedef struct
{
	char SymbolIdent[8];
	int32 MemSize, Revision, NrPins, NrPowerPins, NrPinBusses, NrPartsPerPackage, NrSubPinDefs, NrObjectLines,
	      NrObjectCircles, NrObjectRects, NrObjectArcs, NrObjectTexts, Info;
	float OriginX, OriginY, RefOriginX, RefOriginY, ValueOriginX, ValueOriginY;
	char Name[32], InterfaceName[32], InitialReference[8], Description[64];
	/*

	If more then one NrPartsPerPackage define the substitute pins

	Part1-Info Part2-Info ..
	PinName1-SubSymbol PinName1-Part1 PinName1-Part2 ..
	PinName2-SubSymbol PinName2-Part1 PinName2-Part2 ..
	etc

	int16     Part1-Info;
	char      PinName2-SubSymbol[10];
	char      PinName1-Part1[10];

	*/
} SymbolRecord;


typedef struct
{
	char Identification[32];
	char ShapeName[32];
	int32 Revision;
	int32 CompOutLineOffset, PinOffset, SilkScreenOffset, PasteSoldMaskOffset;
	int32 NrPlacementOutLines, NrCompOutLines, NrPins, NrSilkScreenOutLines, NrPasteSoldMaskObjects;
	float InsertionX, InsertionY, ShapeHeight, PasteMaskCompSide, SoldMaskCompSide, PasteMaskSoldSide, SoldMaskSoldSide,
	      ShapeNameHeight, ShapeNameOriginX, ShapeNameOriginY;
	int16 Info, AddNr, DeleteNr, Dummy;
	int32 ShapeNameRotation;
} ShapeRecord;


typedef struct
{
	char Identification[32], EditingPerson[32];
	int32 FileVersion, Revision, NrLibEntries, MaxNrLibEntries;
} LibRecord;

typedef struct
{
	char Text[32];
	int32 Pos, Length;
} LibNameRecord;


/***********************************************************************/

typedef struct
{
	int32 OtherInfos[32];
	int32 FileTypes[32], FileInfos[32];
	HWND WindowHandles[32];
	char FileNames[32][80];
} ProjectInfoRecord;

#endif
