/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: memory.c
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



#include "types.h"
#include "memory.h"
#include "files.h"
#include "stdio.h"
#include "files2.h"
#include "ctype.h"
#include "string.h"
#include "graphics.h"
#include "utf8.h"


#define        MAX_SPECIAL_MEM        32


int32 ClientWindowDivX, ClientWindowDivY, CurrentDrawMode, RepeatMode, WireSelectMode, ViewMode, MousePanMultiply,
      PopupDisplayVisible, ButtonInfoTimeout, ButtonInfoTimeoutStart, ZoomInOutProcessed, SaveSymbolsLocally, ZoomMode,
      ActiveOnePinNetError = 1, CurrentDirection;

double SearchMinX, SearchMinY, SearchMaxX, SearchMaxY, TextMinX, TextMinY, TextMaxX, TextMaxY, VisibleMaxX, VisibleMinX,
       VisibleMaxY, VisibleMinY;

int32 WindowWidth, WindowHeight, MousePosX, MousePosY, WindowStartX, WindowStartY, ScrollSize, ScrollSizeDrawing,
      SelectionMode, LastActionNr, ScrollEndOfWindow, MaxLastActionNr, MaxScrollBarX, MaxScrollBarY, MouseActionMode,
      EditingProtectedSymbol, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2;


int32 MaxNrWires, MaxNrBusses, MaxNrJunctions, MaxNrOnePinNets, MaxNrBusConnections, MaxNrNetLabels, MaxNrObjectLines,
      MaxNrObjectRects, MaxNrObjectCircles, MaxNrObjectArcs, MaxNrObjectTexts, MaxNrGlobalConnections, MaxNrSheetSymbols,
      MaxNrInstances, MaxNrSubPinDefs, NrSubPinDefs,
//                            MaxNrSubSheets,
      MaxNrPins, MaxNrPinBusses, MaxNrRedefinedPinBusses, MaxNrPowerPins, MaxNrExtraPins, MaxNrObjects, MaxNrObjects2,
      MaxNrObjects5, MaxNrStandardObjects, MessageBufMemSize, MessageBufPos, ClipBoardMemSize, ClipBoardMemPos,
      MaxTempMemory, MaxTempMemory2, TempMemorySize, TempMemorySize2, MaxSpecialMemory[32], MaxSymbolsMemSize;

int32 NetNrWires, NetNrBusses, NetNrJunctions, NetNrBusConnections, NetNrGlobalConnections, NetNrInstances,
      NetNrNetLabels;


int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, MiddleButtonPressed, RightButtonPressed,
      UnselectAll, LeftButtonDoublePressed, DataBaseChanged, ReplaceSelections, GridVisible, BackGroundActive,
      MemoryAllocated, HelpAsked, SearchReplaceOptions, EditingSheetSymbol, DisableOnePinNetCheck, Printing,
      EditingSymbol, FileChanged, UndoRedoActive, SelectionEsc, FirstPaint, SearchReference, SearchPartnr,
      AppendPropertiesToNetlabel, AppendPropertiesToReferences, OkToAddViewPos;

double Xoffset, Yoffset, Factor, RelX, RelY, CentreSelectedX, CentreSelectedY, CurrentX2, CurrentY2, GridSize,
       DrawGridSize;

int32 FirstSize, NrObjects, NrObjects2, NrObjects3, NrObjects4, NrStandardObjects, NrObjects5, SystemBusyMode,
      MouseOnGridPosX, MouseOnGridPosY, CurrentObjectCode, Units, LastAction, ViewPosPointer, NrWireLabels,
      NrGeometryLibraries, NrLibFiles, NrSchematicSymbolLibraries, OperatingSystem, SCHSystemError, CrossHairVisible,
      ButtonInfoTimeout, NestingLevel;


int32 Debugfp;
UINT ClosingWindowMessage;
HGLOBAL GlobalClipBoardMem, GlobalSpecialMem[MAX_SPECIAL_MEM];
uint8 *ClipBoardMem, *SpecialMem[MAX_SPECIAL_MEM];
uint8 *TempMem, *TempMem2;

int32 ReverseY = 1;

ViewPosArray ViewPos;
LastTracePosArray LastTracePos;

double DisplX, DisplY;

char *MessageBuf;
char WindowStr[MAX_LENGTH_STRING];
char AbsPosStr[MAX_LENGTH_STRING];
char RelPosStr[MAX_LENGTH_STRING];
char AbsGridPosStr[MAX_LENGTH_STRING];
char InfoStr[MAX_LENGTH_STRING];
char DesignPath[MAX_LENGTH_STRING];
char DesignFile[MAX_LENGTH_STRING];
char StartDir[MAX_LENGTH_STRING];
char ExePath[MAX_LENGTH_STRING];
char SearchCodeString[20];
char GeometrieEditor[MAX_LENGTH_STRING];
char NewSymbolName[MAX_LENGTH_STRING];
char NewSymbolDir[MAX_LENGTH_STRING];
char NewLibName[MAX_LENGTH_STRING];
char ProjectPath[MAX_LENGTH_STRING];
char SchematicSymbolLibraries[16][MAX_LENGTH_STRING];
char GeometryLibraries[16][MAX_LENGTH_STRING];
char SearchString[MAX_LENGTH_STRING];
char ReplaceString[MAX_LENGTH_STRING];
char EditPath[MAX_LENGTH_STRING];
char EditFile[MAX_LENGTH_STRING];
char LibNames[128][MAX_LENGTH_STRING];
char WireLabel[64][32];
char NestingFiles[8][MAX_LENGTH_STRING];

int32 WiresSelected, BussesSelected, BusConnectionsSelected, JunctionsSelected, OnePinNetsSelected, NetLabelsSelected,
      ObjectLinesSelected, ObjectRectsSelected, ObjectCirclesSelected, ObjectArcsSelected, ObjectTextsSelected,
      GlobalConnectionsSelected, GlobalConnectionTextsSelected, InstancesSelected, InstanceRefsSelected,
      InstanceValuesSelected, PinBussesSelected, PowerPinsSelected, PinsSelected;


ObjectArray *Objects;
StandardObjectArray *StandardObjects;
Object2Array *Objects2, *Objects3, *Objects4;
Object5Array *Objects5;
DesignRecord Design;
WiresArray *Wires;
BussesArray *Busses;
BusConnectionsArray *BusConnections;
JunctionsArray *Junctions;
OnePinNetsArray *OnePinNets;
NetLabelsArray *NetLabels;

ObjectLinesArray *ObjectLines;
ObjectRectsArray *ObjectRects;
ObjectCirclesArray *ObjectCircles;
ObjectArcsArray *ObjectArcs;
ObjectTextsArray *ObjectTexts;
SymbolsPosArray *SymbolsPos;
GlobalConnectionsArray *GlobalConnections;
SymbolRecord DesignSymbol;
PinsArray *Pins;
SubPinDefsArray *SubPinDefs;
PinBusArray *PinBusses;
RedefinedPinBusArray *RedefinedPinBusses;

PowerPinsArray *PowerPins;
SubPinDefsNameRecord SubPinDefsNames[32];

char SymbolAttributesIdent[MaxNrSymbolAttributes][64];
char SymbolAttributesValue[MaxNrSymbolAttributes][64];
int32 NrSymbolAttributes;

InstancesArray *Instances;
CharsArray *Chars;

ObjectRecord NewObject;
WireRecord NewWire;
BusRecord NewBus;
JunctionRecord NewJunction;
OnePinNetRecord NewOnePinNet;
BusConnectionRecord NewBusConnection;
NetLabelRecord NewNetLabel;
ObjectLineRecord NewObjectLine;
ObjectRectRecord NewObjectRect;
ObjectCircleRecord NewObjectCircle;
ObjectArcRecord NewObjectArc;
ObjectTextRecord NewObjectText;
InstanceRecord NewInstance;
PinRecord NewPin;
PinBusRecord NewPinBus;
PowerPinRecord NewPowerPin;
GlobalConnectionRecord NewGlobalConnection;
CompSelectArray *CompSelects;
CompSelectRecord *CompSelect;


uint8 *SymbolsMem;
HGLOBAL DesignsGlobal, WiresGlobal, BussesGlobal, BusConnectionsGlobal, JunctionsGlobal, OnePinNetsGlobal,
        InstancesGlobal, NetLabelsGlobal, ObjectLinesGlobal, ObjectRectsGlobal, ObjectCirclesGlobal, ObjectArcsGlobal,
        ObjectTextsGlobal, SymbolsMemGlobal, SymbolsPosGlobal, MessageBufGlobal, SubPinDefsGlobal, SymbolsGlobal,
        PinsGlobal, PowerPinsGlobal, PinBussesGlobal, RedefinedPinBussesGlobal, GlobalConnectionsGlobal, GlobalTempMem,
        GlobalTempMem2, CompSelectGlobal, ObjectsGlobal, Objects2Global, Objects5Global, StandardObjectsGlobal, CharsGlobal;

uint8 CircleConv[16] = { 0, 3, 12, 15, 0x30, 0, 0x3c, 0, 0xc0, 0xc3, 0, 0, 0xf0, 0, 0, 0xff };
uint8 CircleMirrorX[16] = { 0, 8, 4, 12, 2, 0, 6, 0, 1, 9, 0, 0, 3, 0, 0, 15 };
uint8 CircleMirrorY[16] = { 0, 2, 1, 3, 8, 0, 9, 0, 4, 6, 0, 0, 12, 0, 0, 15 };
uint8 CircleRotate90[16] = { 0, 8, 1, 9, 2, 0, 3, 0, 4, 12, 0, 0, 6, 0, 0, 15 };
uint8 CircleRotate180[16] = { 0, 4, 8, 12, 1, 0, 9, 0, 2, 6, 0, 0, 3, 0, 0, 15 };
uint8 CircleRotate270[16] = { 0, 2, 4, 6, 8, 0, 12, 0, 1, 3, 0, 0, 9, 0, 0, 15 };
uint8 TextMirrorX[9] = { 6, 7, 8, 3, 4, 5, 0, 1, 2 };
uint8 TextMirrorY[9] = { 2, 1, 0, 5, 4, 3, 8, 7, 6 };
uint8 TextRotate90[9] = { 0, 3, 0, 7, 4, 1, 6, 5, 2 };
uint8 TextRotate180[9] = { 8, 7, 6, 5, 4, 3, 2, 1, 0 };
uint8 TextRotate270[9] = { 2, 5, 8, 1, 4, 7, 0, 3, 6 };

HWND SCHWindow;
HDC OutputDisplay;
WNDCLASS SCHClass;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 NrSchNames, SchNamesPos;
LPSTR SchNamesId[MaxNrSchNames];


char SchNamesBuf[SchNamesBufSize];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LPSTR StringConvert(int32 Nr, LPSTR str)
{
	int32 StringCount1, StringCount2, ParamCount1, ParamCount2, cnt, Length;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	if (!str)
		return "";

	if ((Nr < 0) || (Nr >= MaxNrSchNames) || (SchNamesId[Nr] == 0))
		return str;

	strcpy(str1, str);
	strcpy(str2, SchNamesId[Nr]);

	StringCount1 = 0;
	ParamCount1 = 0;
	Length = strlen(str1);
	cnt = 0;

	while (cnt < Length)
	{
		if (str1[cnt] == '%')
		{
			if (cnt < Length - 1)
			{
				if (str1[cnt + 1] == 's')
					StringCount1++;

				if (str1[cnt + 1] != '%')
					ParamCount1++;
			}
		}

		cnt++;
	}

	StringCount2 = 0;
	ParamCount2 = 0;
	Length = strlen(str2);
	cnt = 0;

	while (cnt < Length)
	{
		if (str2[cnt] == '%')
		{
			if (cnt < Length - 1)
			{
				if (str2[cnt + 1] == 's')
					StringCount2++;

				if (str2[cnt + 1] != '%')
					ParamCount2++;
			}
		}

		cnt++;
	}

	if ((ParamCount1 != ParamCount2) || (StringCount1 != StringCount2))
	{
		sprintf(str3, "Error in string number %d of the language file (Wrong parameters).", Nr);
		strcat(str3, "\n\nInstead the english string will be used");
		MessageBoxUTF8(NULL, str3, "System error", MB_APPLMODAL | MB_OK);
		return str;
	}

	return SchNamesId[Nr];
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GetStringKomma(LPSTR Str, LPSTR Result)
{
	int32 cnt, l;
	memset(Result, 0, 200);
	l = strlen(Str);

	cnt = 0;

	while ((cnt < l) && (Str[cnt] != ','))
		cnt++;

//  cnt2=0;
//  while ((cnt2<l)
//        &&
//        (Str[cnt2]==' ')) cnt2++;
	if (cnt > 0)
		memmove(Result, Str, cnt);

	if (cnt >= l - 1)
	{
		Str[0] = 0;
		return;
	}

	if (l - cnt > 0)
		memmove(Str, &Str[cnt + 1], l - cnt);

	l = strlen(Result);
	cnt = l - 1;
//  while ((cnt>=0)
//        &&
//        (Result[cnt]==' ')) cnt--;
//  Result[cnt+1]=0;
	Result[l] = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckString(LPSTR str, int32 mode)
{
	int32 cnt, lengte;
	int32 Digits = 0;
	int32 Alphas = 0;
	char ch;

	lengte = (int32) strlen(str);

	for (cnt = 0; cnt < lengte; cnt++)
	{
		ch = str[cnt];

		switch (mode)
		{
		case 0:
			if ((!isalnum(str[cnt])) && (str[cnt] != ','))
				return -1;

			break;

		case 1:
			if ((!isalnum(str[cnt])) && (str[cnt] != '/') && (str[cnt] != ','))
				return -1;

			break;

		case 2:
			if (cnt == 0)
			{
				if (isdigit(str[cnt]))
					return -1;

				if (str[cnt] == '_')
					return -1;
			}

			if ((!isalnum(str[cnt])) && (str[cnt] != '_'))
				return -1;

			break;

		case 3:
			if (!isdigit(str[cnt]))
				return -1;

			break;

		case 4:
			if (!isalpha(str[cnt]))
				return -1;

			break;

		case 5:
			if (!isalnum(str[cnt]))
				return -1;

			if (isalpha(str[cnt]))
				Alphas = 1;

			if (isdigit(str[cnt]))
				Digits = 1;

			break;

		case 6:
			if ((ch == '\'') || (ch == '"')
//           ||
//           (ch=='/')
			        || (ch == '\\') || (ch == '$') || (ch == '^') || (ch == '`') || (ch <= ' ') || (ch > '~'))
				return -1;

			break;
		}
	}

	if ((lengte > 0) && (mode == 5) && (Alphas) && (Digits) && (isdigit(str[0])))
		return -1;

	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckReference(LPSTR str)
{
	int32 cnt, count, length;

	if ((length = strlen(str)) == 0)
		return -1;

	if (CheckString(str, 2) == -1)
		return -1;

	count = 0;

	for (cnt = 0; cnt < length; cnt++)
	{
		if (str[cnt] == '?')
			count++;
	}

	if (count > 1)
		return -1;

	if ((count == 1) && (str[length - 1] != '?'))
		return -1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckNetName(LPSTR str)
{
	int32 length;

	if ((length = strlen(str)) == 0)
		return -1;

//  if (str[0]=='-') return -1;
	if (str[0] == '/')
		return -1;

//  if (isdigit(str[0]))  return -1;
	return CheckString(str, 6);

//  return CheckString(str,2);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckPinName(LPSTR str, int32 PowerPin)
{
	int32 length;

	if ((length = strlen(str)) == 0)
		return -1;

	if (str[0] == '/')
		return -1;

	if ((!PowerPin) && (str[0] == '-'))
		return -1;

//  if (isdigit(str[0]))  return -1;
	return CheckString(str, 6);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetPinNameFromPinBus(LPSTR PinBusName, LPSTR PinName, int32 NrPins, int32 Index)
{
	int32 lengte, cnt, cnt2, pos1, pos2, LineNr;
	int32 FoundKomma;
	char PreviousChar;

	LineNr = 0;
	lengte = strlen(PinBusName);

	if (lengte == 0)
		return -1;

	if (Index < 0)
		return -1;

	cnt = 0;
	cnt2 = 0;
	pos1 = 0;
	pos2 = 0;
	PreviousChar = 0;
	FoundKomma = 1;

	while ((cnt < lengte) && (cnt2 < Index + 1))
	{
		if ((PinBusName[cnt] == ',') || (PinBusName[cnt] == '\\'))
		{
			FoundKomma = 1;
			pos2 = cnt;

			if ((cnt < lengte - 1) && (PinBusName[cnt + 1] == '\\'))
				cnt++;

			cnt2++;
		}
		else
		{
			if (FoundKomma)
			{
				pos1 = cnt;
				FoundKomma = 0;

				if (PreviousChar == '\\')
					LineNr++;
			}
		}

		PreviousChar = PinBusName[cnt];

		if (cnt2 != Index + 1)
			cnt++;
	}

	if (cnt == lengte)
	{
		pos2 = cnt;
		cnt2++;
	}

	if (cnt2 != Index + 1)
		return -1;

	memmove(PinName, &PinBusName[pos1], pos2 - pos1);
	PinName[pos2 - pos1] = 0;
	return LineNr;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckLabelName(LPSTR str)
{
	int32 length;

	if ((length = strlen(str)) == 0)
		return -1;

//  if (str[0]=='-') return -1;
	if (str[0] == '/')
		return -1;

//  if (isdigit(str[0]))  return -1;
	return CheckString(str, 6);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckPinNames(LPSTR str)
{
	char str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	int32 length;

	if ((length = strlen(str)) == 0)
		return -1;

	if (length > 250)
		return -1;

	memset(&str2, 0, MAX_LENGTH_STRING);
	memmove(&str2, str, length);

	while (str2[0] != 0)
	{
		memset(&str3, 0, MAX_LENGTH_STRING);
		GetStringKomma(str2, str3);

		if (CheckPinName(str3, 0) == -1)
			return -1;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetNrPinNames(LPSTR str)
{
	char str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	int32 length, count;

	count = 0;

	if ((length = strlen(str)) == 0)
		return -1;

	if (length > 250)
		return -1;

	memset(&str2, 0, MAX_LENGTH_STRING);
	memmove(&str2, str, length);

	while (str2[0] != 0)
	{
		memset(&str3, 0, MAX_LENGTH_STRING);
		GetStringKomma(str2, str3);

		if (CheckPinName(str3, 0) == -1)
			return -1;

		count++;
	}

	return count;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckGeometrie(LPSTR str)
{
	return CheckString(str, 2);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetUsedMemSize(int32 mode)
{
	int32 MemSize = 0;

	if (WiresGlobal != 0)
		MemSize += GlobalSize(WiresGlobal);

	if (BussesGlobal != 0)
		MemSize += GlobalSize(BussesGlobal);

	if (BusConnectionsGlobal != 0)
		MemSize += GlobalSize(BusConnectionsGlobal);

	if (JunctionsGlobal != 0)
		MemSize += GlobalSize(JunctionsGlobal);

	if (InstancesGlobal != 0)
		MemSize += GlobalSize(InstancesGlobal);

	if (NetLabelsGlobal != 0)
		MemSize += GlobalSize(NetLabelsGlobal);

	if (ObjectLinesGlobal != 0)
		MemSize += GlobalSize(ObjectLinesGlobal);

	if (ObjectRectsGlobal != 0)
		MemSize += GlobalSize(ObjectRectsGlobal);

	if (ObjectCirclesGlobal != 0)
		MemSize += GlobalSize(ObjectCirclesGlobal);

	if (ObjectArcsGlobal != 0)
		MemSize += GlobalSize(ObjectArcsGlobal);

	if (ObjectTextsGlobal != 0)
		MemSize += GlobalSize(ObjectTextsGlobal);

	if (SymbolsMemGlobal != 0)
		MemSize += GlobalSize(SymbolsMemGlobal);

	if (SymbolsPosGlobal != 0)
		MemSize += GlobalSize(SymbolsPosGlobal);

	if (MessageBufGlobal != 0)
		MemSize += GlobalSize(MessageBufGlobal);

	if (SubPinDefsGlobal != 0)
		MemSize += GlobalSize(SubPinDefsGlobal);

	if (SymbolsGlobal != 0)
		MemSize += GlobalSize(SymbolsGlobal);

	if (PinsGlobal != 0)
		MemSize += GlobalSize(PinsGlobal);

	if (PowerPinsGlobal != 0)
		MemSize += GlobalSize(PowerPinsGlobal);

	if (PinBussesGlobal != 0)
		MemSize += GlobalSize(PinBussesGlobal);

	if (GlobalConnectionsGlobal != 0)
		MemSize += GlobalSize(GlobalConnectionsGlobal);

	if (ObjectsGlobal != 0)
		MemSize += GlobalSize(ObjectsGlobal);

	if (Objects2Global != 0)
		MemSize += GlobalSize(Objects2Global);

	if (Objects5Global != 0)
		MemSize += GlobalSize(Objects5Global);

	if (CharsGlobal != 0)
		MemSize += GlobalSize(CharsGlobal);

	return MemSize;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemStandardObjects(int32 Count, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrStandardObjects >= Count)
		return 1;

	if (Count > LimitMaxNrStandardObjects)
		return -2;

	if (MaxNrStandardObjects == 0)
	{
		if ((StandardObjectsGlobal = GlobalAlloc(GHND, Count * sizeof(StandardObjectRecord))) == NULL)
			return -1;

		if ((StandardObjects = (StandardObjectArray *) GlobalLock(StandardObjectsGlobal)) == NULL)
			return -1;

		MaxNrStandardObjects = Count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(StandardObjectsGlobal, Count * sizeof(StandardObjectRecord), GHND)) == NULL)
			return -1;

		StandardObjectsGlobal = NewMem;

		if ((StandardObjects = (StandardObjectArray *) GlobalLock(StandardObjectsGlobal)) == NULL)
			return -1;

		MaxNrStandardObjects = Count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects(int32 Count, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrObjects >= Count)
		return 1;

	if (Count > LimitMaxNrObjects)
		return -2;

	if (MaxNrObjects == 0)
	{
		if ((ObjectsGlobal = GlobalAlloc(GHND, Count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects = (ObjectArray *) GlobalLock(ObjectsGlobal)) == NULL)
			return -1;

		MaxNrObjects = Count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectsGlobal, Count * sizeof(ObjectRecord), GHND)) == NULL)
			return -1;

		ObjectsGlobal = NewMem;

		if ((Objects = (ObjectArray *) GlobalLock(ObjectsGlobal)) == NULL)
			return -1;

		MaxNrObjects = Count;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects2(int32 Count, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrObjects2 >= Count)
		return 1;

	if (Count > LimitMaxNrObjects)
		return -2;

	if (MaxNrObjects2 == 0)
	{
		if ((Objects2Global = GlobalAlloc(GHND, Count * sizeof(Object2Record))) == NULL)
			return -1;

		if ((Objects2 = (Object2Array *) GlobalLock(Objects2Global)) == NULL)
			return -1;

		MaxNrObjects2 = Count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Objects2Global, Count * sizeof(Object2Record), GHND)) == NULL)
			return -1;

		Objects2Global = NewMem;

		if ((Objects2 = (Object2Array *) GlobalLock(Objects2Global)) == NULL)
			return -1;

		MaxNrObjects2 = Count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects5(int32 Count)
{
	HGLOBAL NewMem;

	if (MaxNrObjects5 == 0)
	{
		Count = max(64, Count);

		if ((Objects5Global = GlobalAlloc(GHND, Count * sizeof(Object5Record))) == NULL)
			return -1;

		if ((Objects5 = (Object5Array *) GlobalLock(Objects5Global)) == NULL)
			return -1;

		MaxNrObjects5 = Count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Objects5Global, Count * sizeof(Object5Record), GHND)) == NULL)
			return -1;

		Objects5Global = NewMem;

		if ((Objects5 = (Object5Array *) GlobalLock(Objects5Global)) == NULL)
			return -1;

		MaxNrObjects5 = Count;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void AllocateMem()
{
	int32 count, count2, error;

	error = 0;
	count = 0;
	count2 = 0;

	if ((!error) && ((CharsGlobal = GlobalAlloc(GHND, (int32) sizeof(CharsArray))) == NULL))
		error = 1;

	count++;

	if (AllocateMemObjects(500, 0) == -1)
		error = 1;

	if ((!error) && ((Chars = (CharsArray *) GlobalLock(CharsGlobal)) == NULL))
		error = 1;

	count2++;

// **********************************************************************************************

	if (error)
	{
#ifdef VC40
		LastError = GetLastError();
#endif
	}

	CreateDrawObjects(0);

}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemWires(int32 NrWires, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrWires > NrWires)
		return 1;

	if (NrWires > LimitMaxNrWires)
		return -2;

	NrWires = max(100, NrWires);

	if (MaxNrWires == 0)
	{
		if ((WiresGlobal = GlobalAlloc(GHND, NrWires * sizeof(WireRecord))) == NULL)
			return -1;

		if ((Wires = (WiresArray *) GlobalLock(WiresGlobal)) == NULL)
			return -1;

		MaxNrWires = NrWires;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(WiresGlobal, NrWires * sizeof(WireRecord), GHND | GMEM_ZEROINIT)) == NULL)
			return -1;

		WiresGlobal = NewMem;

		if ((Wires = (WiresArray *) GlobalLock(WiresGlobal)) == NULL)
			return -1;

		MaxNrWires = NrWires;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemBusses(int32 NrBusses, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrBusses > NrBusses)
		return 1;

	if (NrBusses > LimitMaxNrBusses)
		return -2;

	NrBusses = max(NrBusses, 100);

	if (MaxNrBusses == 0)
	{
		if ((BussesGlobal = GlobalAlloc(GHND, NrBusses * sizeof(BusRecord))) == NULL)
			return -1;

		if ((Busses = (BussesArray *) GlobalLock(BussesGlobal)) == NULL)
			return -1;

		MaxNrBusses = NrBusses;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(BussesGlobal, NrBusses * sizeof(BusRecord), GHND)) == NULL)
			return -1;

		BussesGlobal = NewMem;

		if ((Busses = (BussesArray *) GlobalLock(BussesGlobal)) == NULL)
			return -1;

		MaxNrBusses = NrBusses;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemJunctions(int32 NrJunctions, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrJunctions > NrJunctions)
		return 1;

	if (NrJunctions > LimitMaxNrJunctions)
		return -2;

	NrJunctions = max(NrJunctions, 100);

	if (MaxNrJunctions == 0)
	{
		if ((JunctionsGlobal = GlobalAlloc(GHND, NrJunctions * sizeof(JunctionRecord))) == NULL)
			return -1;

		if ((Junctions = (JunctionsArray *) GlobalLock(JunctionsGlobal)) == NULL)
			return -1;

		MaxNrJunctions = NrJunctions;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(JunctionsGlobal, NrJunctions * sizeof(JunctionRecord), GHND)) == NULL)
			return -1;

		JunctionsGlobal = NewMem;

		if ((Junctions = (JunctionsArray *) GlobalLock(JunctionsGlobal)) == NULL)
			return -1;

		MaxNrJunctions = NrJunctions;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemOnePinNets(int32 NrOnePinNets, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrOnePinNets > NrOnePinNets)
		return 1;

	if (NrOnePinNets > LimitMaxNrOnePinNets)
		return -2;

	NrOnePinNets = max(NrOnePinNets, 100);

	if (MaxNrOnePinNets == 0)
	{
		if ((OnePinNetsGlobal = GlobalAlloc(GHND, NrOnePinNets * sizeof(OnePinNetRecord))) == NULL)
			return -1;

		if ((OnePinNets = (OnePinNetsArray *) GlobalLock(OnePinNetsGlobal)) == NULL)
			return -1;

		MaxNrOnePinNets = NrOnePinNets;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(OnePinNetsGlobal, NrOnePinNets * sizeof(OnePinNetRecord), GHND)) == NULL)
			return -1;

		OnePinNetsGlobal = NewMem;

		if ((OnePinNets = (OnePinNetsArray *) GlobalLock(OnePinNetsGlobal)) == NULL)
			return -1;

		MaxNrOnePinNets = NrOnePinNets;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemNetLabels(int32 NrNetLabels, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrNetLabels > NrNetLabels)
		return 1;

	if (NrNetLabels > LimitMaxNrNetLabels)
		return -2;

	NrNetLabels = max(NrNetLabels, 100);

	if (MaxNrNetLabels == 0)
	{
		if ((NetLabelsGlobal = GlobalAlloc(GHND, NrNetLabels * sizeof(NetLabelRecord))) == NULL)
			return -1;

		if ((NetLabels = (NetLabelsArray *) GlobalLock(NetLabelsGlobal)) == NULL)
			return -1;

		MaxNrNetLabels = NrNetLabels;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(NetLabelsGlobal, NrNetLabels * sizeof(NetLabelRecord), GHND)) == NULL)
			return -1;

		NetLabelsGlobal = NewMem;

		if ((NetLabels = (NetLabelsArray *) GlobalLock(NetLabelsGlobal)) == NULL)
			return -1;

		MaxNrNetLabels = NrNetLabels;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemBusConnections(int32 NrBusConnections, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrBusConnections > NrBusConnections)
		return 1;

	if (NrBusConnections > LimitMaxNrBusConnections)
		return -2;

	NrBusConnections = max(NrBusConnections, 100);

	if (MaxNrBusConnections == 0)
	{
		if ((BusConnectionsGlobal = GlobalAlloc(GHND, NrBusConnections * sizeof(BusConnectionRecord))) == NULL)
			return -1;

		if ((BusConnections = (BusConnectionsArray *) GlobalLock(BusConnectionsGlobal)) == NULL)
			return -1;

		MaxNrBusConnections = NrBusConnections;
	}
	else
	{
		if ((NewMem =
		            GlobalReAlloc(BusConnectionsGlobal, NrBusConnections * sizeof(BusConnectionRecord), GHND)) == NULL)
			return -1;

		BusConnectionsGlobal = NewMem;

		if ((BusConnections = (BusConnectionsArray *) GlobalLock(BusConnectionsGlobal)) == NULL)
			return -1;

		MaxNrBusConnections = NrBusConnections;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemGlobalConnections(int32 NrGlobalConnections, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrGlobalConnections > NrGlobalConnections)
		return 1;

	if (NrGlobalConnections > LimitMaxNrGlobalConnections)
		return -2;

	NrGlobalConnections = max(NrGlobalConnections, 100);

	if (MaxNrGlobalConnections == 0)
	{
		if ((GlobalConnectionsGlobal = GlobalAlloc(GHND, NrGlobalConnections * sizeof(GlobalConnectionRecord))) == NULL)
			return -1;

		if ((GlobalConnections = (GlobalConnectionsArray *) GlobalLock(GlobalConnectionsGlobal)) == NULL)
			return -1;

		MaxNrGlobalConnections = NrGlobalConnections;
	}
	else
	{
		if ((NewMem =
		            GlobalReAlloc(GlobalConnectionsGlobal, NrGlobalConnections * sizeof(GlobalConnectionRecord),
		                          GHND)) == NULL)
			return -1;

		GlobalConnectionsGlobal = NewMem;

		if ((GlobalConnections = (GlobalConnectionsArray *) GlobalLock(GlobalConnectionsGlobal)) == NULL)
			return -1;

		MaxNrGlobalConnections = NrGlobalConnections;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectLines(int32 NrObjectLines, int32 mode)
{
	HGLOBAL NewMem;
	DWORD LastError;

	if (MaxNrObjectLines > NrObjectLines)
		return 1;

	if (NrObjectLines > LimitMaxNrObjectLines)
		return -2;

	NrObjectLines = max(NrObjectLines, 100);

	if (MaxNrObjectLines == 0)
	{
		if ((ObjectLinesGlobal = GlobalAlloc(GHND, NrObjectLines * sizeof(ObjectLineRecord))) == NULL)
			return -1;

		if ((ObjectLines = (ObjectLinesArray *) GlobalLock(ObjectLinesGlobal)) == NULL)
			return -1;

		MaxNrObjectLines = NrObjectLines;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectLinesGlobal, NrObjectLines * sizeof(ObjectLineRecord), GHND)) == NULL)
		{
			LastError = GetLastError();
			return -1;
		}

		ObjectLinesGlobal = NewMem;

		if ((ObjectLines = (ObjectLinesArray *) GlobalLock(ObjectLinesGlobal)) == NULL)
			return -1;

		MaxNrObjectLines = NrObjectLines;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectRects(int32 NrObjectRects, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrObjectRects > NrObjectRects)
		return 1;

	if (NrObjectRects > LimitMaxNrObjectRects)
		return -2;

	NrObjectRects = max(NrObjectRects, 100);

	if (MaxNrObjectRects == 0)
	{
		if ((ObjectRectsGlobal = GlobalAlloc(GHND, NrObjectRects * sizeof(ObjectRectRecord))) == NULL)
			return -1;

		if ((ObjectRects = (ObjectRectsArray *) GlobalLock(ObjectRectsGlobal)) == NULL)
			return -1;

		MaxNrObjectRects = NrObjectRects;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectRectsGlobal, NrObjectRects * sizeof(ObjectRectRecord), GHND)) == NULL)
			return -1;

		ObjectRectsGlobal = NewMem;

		if ((ObjectRects = (ObjectRectsArray *) GlobalLock(ObjectRectsGlobal)) == NULL)
			return -1;

		MaxNrObjectRects = NrObjectRects;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectCircles(int32 NrObjectCircles, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrObjectCircles > NrObjectCircles)
		return 1;

	if (NrObjectCircles > LimitMaxNrObjectCircles)
		return -2;

	NrObjectCircles = max(100, NrObjectCircles);

	if (MaxNrObjectCircles == 0)
	{
		if ((ObjectCirclesGlobal = GlobalAlloc(GHND, NrObjectCircles * sizeof(ObjectCircleRecord))) == NULL)
			return -1;

		if ((ObjectCircles = (ObjectCirclesArray *) GlobalLock(ObjectCirclesGlobal)) == NULL)
			return -1;

		MaxNrObjectCircles = NrObjectCircles;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectCirclesGlobal, NrObjectCircles * sizeof(ObjectCircleRecord), GHND)) == NULL)
			return -1;

		ObjectCirclesGlobal = NewMem;

		if ((ObjectCircles = (ObjectCirclesArray *) GlobalLock(ObjectCirclesGlobal)) == NULL)
			return -1;

		MaxNrObjectCircles = NrObjectCircles;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectArcs(int32 NrObjectArcs, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrObjectArcs > NrObjectArcs)
		return 1;

	if (NrObjectArcs > LimitMaxNrObjectArcs)
		return -2;

	NrObjectArcs = max(NrObjectArcs, 100);

	if (MaxNrObjectArcs == 0)
	{
		if ((ObjectArcsGlobal = GlobalAlloc(GHND, NrObjectArcs * sizeof(ObjectArcRecord))) == NULL)
			return -1;

		if ((ObjectArcs = (ObjectArcsArray *) GlobalLock(ObjectArcsGlobal)) == NULL)
			return -1;

		MaxNrObjectArcs = NrObjectArcs;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectArcsGlobal, NrObjectArcs * sizeof(ObjectArcRecord), GHND)) == NULL)
			return -1;

		ObjectArcsGlobal = NewMem;

		if ((ObjectArcs = (ObjectArcsArray *) GlobalLock(ObjectArcsGlobal)) == NULL)
			return -1;

		MaxNrObjectArcs = NrObjectArcs;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectTexts(int32 NrObjectTexts, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrObjectTexts > NrObjectTexts)
		return 1;

	if (NrObjectTexts > LimitMaxNrObjectTexts)
		return -2;

	NrObjectTexts = max(NrObjectTexts, 40);

	if (MaxNrObjectTexts == 0)
	{
		if ((ObjectTextsGlobal = GlobalAlloc(GHND, NrObjectTexts * sizeof(ObjectTextRecord))) == NULL)
			return -1;

		if ((ObjectTexts = (ObjectTextsArray *) GlobalLock(ObjectTextsGlobal)) == NULL)
			return -1;

		MaxNrObjectTexts = NrObjectTexts;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectTextsGlobal, NrObjectTexts * sizeof(ObjectTextRecord), GHND)) == NULL)
			return -1;

		ObjectTextsGlobal = NewMem;

		if ((ObjectTexts = (ObjectTextsArray *) GlobalLock(ObjectTextsGlobal)) == NULL)
			return -1;

		MaxNrObjectTexts = NrObjectTexts;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemPins(int32 NrPins, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrPins > NrPins)
		return 1;

	if (NrPins > LimitMaxNrPins)
		return -2;

	NrPins = max(100, NrPins);

	if (MaxNrPins == 0)
	{
		if ((PinsGlobal = GlobalAlloc(GHND, NrPins * sizeof(PinRecord))) == NULL)
			return -1;

		if ((Pins = (PinsArray *) GlobalLock(PinsGlobal)) == NULL)
			return -1;

		MaxNrPins = NrPins;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(PinsGlobal, NrPins * sizeof(PinRecord), GHND)) == NULL)
		{
		}

		PinsGlobal = NewMem;

		if ((Pins = (PinsArray *) GlobalLock(PinsGlobal)) == NULL)
			return -1;

		MaxNrPins = NrPins;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemPowerPins(int32 NrPowerPins, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrPowerPins > NrPowerPins)
		return 1;

	if (NrPowerPins > LimitMaxNrPowerPins)
		return -2;

	NrPowerPins = max(NrPowerPins, 10);

	if (MaxNrPowerPins == 0)
	{
		if ((PowerPinsGlobal = GlobalAlloc(GHND, NrPowerPins * sizeof(PowerPinRecord))) == NULL)
			return -1;

		if ((PowerPins = (PowerPinsArray *) GlobalLock(PowerPinsGlobal)) == NULL)
			return -1;

		MaxNrPowerPins = NrPowerPins;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(PowerPinsGlobal, NrPowerPins * sizeof(PowerPinRecord), GHND)) == NULL)
			return -1;

		PowerPinsGlobal = NewMem;

		if ((PowerPins = (PowerPinsArray *) GlobalLock(PowerPinsGlobal)) == NULL)
			return -1;

		MaxNrPowerPins = NrPowerPins;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemPinBusses(int32 NrPinBusses, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrPinBusses > NrPinBusses)
		return 1;

	if (NrPinBusses > LimitMaxNrPinBusses)
		return -2;

	NrPinBusses = max(10, NrPinBusses);

	if (MaxNrPinBusses == 0)
	{
		if ((PinBussesGlobal = GlobalAlloc(GHND, NrPinBusses * sizeof(PinBusRecord))) == NULL)
			return -1;

		if ((PinBusses = (PinBusArray *) GlobalLock(PinBussesGlobal)) == NULL)
			return -1;

		MaxNrPinBusses = NrPinBusses;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(PinBussesGlobal, NrPinBusses * sizeof(PinBusRecord), GHND)) == NULL)
			return -1;

		PinBussesGlobal = NewMem;

		if ((PinBusses = (PinBusArray *) GlobalLock(PinBussesGlobal)) == NULL)
			return -1;

		MaxNrPinBusses = NrPinBusses;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemRedefinedPinBusses(int32 NrRedefinedPinBusses, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrRedefinedPinBusses > NrRedefinedPinBusses)
		return 1;

	if (NrRedefinedPinBusses > LimitMaxNrPinBusses)
		return -2;

	NrRedefinedPinBusses = max(NrRedefinedPinBusses, 10);

	if (MaxNrRedefinedPinBusses == 0)
	{
		if ((RedefinedPinBussesGlobal =
		            GlobalAlloc(GHND, NrRedefinedPinBusses * sizeof(RedefinedPinBusRecord))) == NULL)
			return -1;

		if ((RedefinedPinBusses = (RedefinedPinBusArray *) GlobalLock(RedefinedPinBussesGlobal)) == NULL)
			return -1;

		MaxNrRedefinedPinBusses = NrRedefinedPinBusses;
	}
	else
	{
		if ((NewMem =
		            GlobalReAlloc(RedefinedPinBussesGlobal, NrRedefinedPinBusses * sizeof(PinBusRecord), GHND)) == NULL)
			return -1;

		RedefinedPinBussesGlobal = NewMem;

		if ((RedefinedPinBusses = (RedefinedPinBusArray *) GlobalLock(RedefinedPinBussesGlobal)) == NULL)
			return -1;

		MaxNrRedefinedPinBusses = NrRedefinedPinBusses;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemInstances(int32 NrInstances, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrInstances > NrInstances)
		return 1;

	if (NrInstances > LimitMaxNrInstances)
		return -2;

	NrInstances = max(NrInstances, 20);

	if (MaxNrInstances == 0)
	{
		if ((InstancesGlobal = GlobalAlloc(GHND, NrInstances * sizeof(InstanceRecord))) == NULL)
			return -1;

		if ((Instances = (InstancesArray *) GlobalLock(InstancesGlobal)) == NULL)
			return -1;

		MaxNrInstances = NrInstances;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(InstancesGlobal, NrInstances * sizeof(InstanceRecord), GHND)) == NULL)
			return -1;

		InstancesGlobal = NewMem;

		if ((Instances = (InstancesArray *) GlobalLock(InstancesGlobal)) == NULL)
			return -1;

		MaxNrInstances = NrInstances;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemSubPinDefs(int32 NrSubPinDefs, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrSubPinDefs > NrSubPinDefs)
		return 1;

	if (NrSubPinDefs > LimitMaxNrSubPinDefs)
		return -2;

	if (MaxNrSubPinDefs == 0)
	{
		NrSubPinDefs = max(NrSubPinDefs, 100);

		if ((SubPinDefsGlobal = GlobalAlloc(GHND, NrSubPinDefs * sizeof(SubPinDefsType))) == NULL)
			return -1;

		if ((SubPinDefs = (SubPinDefsArray *) GlobalLock(SubPinDefsGlobal)) == NULL)
			return -1;

		MaxNrSubPinDefs = NrSubPinDefs;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(SubPinDefsGlobal, NrSubPinDefs * sizeof(SubPinDefsType), GHND)) == NULL)
			return -1;

		SubPinDefsGlobal = NewMem;

		if ((SubPinDefs = (SubPinDefsArray *) GlobalLock(SubPinDefsGlobal)) == NULL)
			return -1;

		MaxNrSubPinDefs = NrSubPinDefs;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemSheetSymbolsPos(int32 NrSheetSymbols, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxNrSheetSymbols > NrSheetSymbols)
		return 1;

	if (NrSheetSymbols > LimitMaxNrSheetSymbols)
		return -2;

	NrSheetSymbols = max(NrSheetSymbols, 20);

	if (MaxNrSheetSymbols == 0)
	{
		if ((SymbolsPosGlobal = GlobalAlloc(GHND, NrSheetSymbols * sizeof(SymbolsPosRecord))) == NULL)
			return -1;

		if ((SymbolsPos = (SymbolsPosArray *) GlobalLock(SymbolsPosGlobal)) == NULL)
			return -1;

		MaxNrSheetSymbols = NrSheetSymbols;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(SymbolsPosGlobal, NrSheetSymbols * sizeof(SymbolsPosRecord), GHND)) == NULL)
			return -1;

		SymbolsPosGlobal = NewMem;

		if ((SymbolsPos = (SymbolsPosArray *) GlobalLock(SymbolsPosGlobal)) == NULL)
			return -1;

		MaxNrSheetSymbols = NrSheetSymbols;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemSheetSymbols(int32 MemSheetSymbols, int32 mode)
{
	HGLOBAL NewMem;

	if (MaxSymbolsMemSize > MemSheetSymbols)
		return 1;

	if (MemSheetSymbols > LimitMaxSymbolsMemory)
		return -2;

	if (MaxSymbolsMemSize == 0)
	{
		if ((SymbolsMemGlobal = GlobalAlloc(GHND, MemSheetSymbols)) == NULL)
			return -1;

		if ((SymbolsMem = GlobalLock(SymbolsMemGlobal)) == NULL)
			return -1;

		MaxSymbolsMemSize = MemSheetSymbols;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(SymbolsMemGlobal, MemSheetSymbols, GHND)) == NULL)
			return -1;

		SymbolsMemGlobal = NewMem;

		if ((SymbolsMem = GlobalLock(SymbolsMemGlobal)) == NULL)
			return -1;

		MaxSymbolsMemSize = MemSheetSymbols;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemTemp(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxTempMemory == 0)
	{
		if ((GlobalTempMem = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((TempMem = GlobalLock(GlobalTempMem)) == NULL)
			return -1;

		MaxTempMemory = MemSize;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(GlobalTempMem, MemSize, GHND)) == NULL)
			return -1;

		GlobalTempMem = NewMem;

		if ((TempMem = GlobalLock(GlobalTempMem)) == NULL)
			return -1;

		MaxTempMemory = MemSize;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemTemp2(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxTempMemory2 == 0)
	{
		if ((GlobalTempMem2 = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((TempMem2 = GlobalLock(GlobalTempMem2)) == NULL)
			return -1;

		MaxTempMemory2 = MemSize;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(GlobalTempMem2, MemSize, GHND)) == NULL)
			return -1;

		GlobalTempMem2 = NewMem;

		if ((TempMem2 = GlobalLock(GlobalTempMem2)) == NULL)
			return -1;

		MaxTempMemory2 = MemSize;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMemStandardObjects()
{


}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMemTemp()
{
	if (GlobalTempMem != NULL)
	{
		GlobalUnlock(GlobalTempMem);
		GlobalFree(GlobalTempMem);
		GlobalTempMem = NULL;
		MaxTempMemory = 0;
		TempMemorySize = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMemTemp2()
{
	if (GlobalTempMem2 != NULL)
	{
		GlobalUnlock(GlobalTempMem2);
		GlobalFree(GlobalTempMem2);
		GlobalTempMem2 = NULL;
		MaxTempMemory2 = 0;
		TempMemorySize2 = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMem()
{
	if (Objects5Global != NULL)
	{
		GlobalUnlock(Objects5Global);
		GlobalFree(Objects5Global);
		Objects5Global = NULL;
		MaxNrObjects5 = 0;
		NrObjects5 = 0;
	}

	if (Objects2Global != NULL)
	{
		GlobalUnlock(Objects2Global);
		GlobalFree(Objects2Global);
		Objects2Global = NULL;
		MaxNrObjects2 = 0;
		NrObjects2 = 0;
	}

	if (GlobalClipBoardMem != NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		GlobalFree(GlobalClipBoardMem);
		ClipBoardMemSize = 0;
		ClipBoardMemPos = 0;
		GlobalClipBoardMem = NULL;
	}

	GlobalUnlock(ObjectsGlobal);
	GlobalUnlock(CharsGlobal);
	GlobalFree(ObjectsGlobal);
	GlobalFree(CharsGlobal);

	DeleteGraphicObjects();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemDesign()
{
	if (Objects5Global != NULL)
	{
		GlobalUnlock(Objects5Global);
		GlobalFree(Objects5Global);
		Objects5Global = NULL;
		MaxNrObjects5 = 0;
		NrObjects5 = 0;
	}

	DeAllocateMemMessageBuf();

	if (GlobalConnectionsGlobal != NULL)
	{
		GlobalUnlock(GlobalConnectionsGlobal);
		GlobalFree(GlobalConnectionsGlobal);
		GlobalConnectionsGlobal = 0;
	}

	if (InstancesGlobal != NULL)
	{
		GlobalUnlock(InstancesGlobal);
		GlobalFree(InstancesGlobal);
		InstancesGlobal = 0;
	}

	if (SymbolsMemGlobal != NULL)
	{
		GlobalUnlock(SymbolsMemGlobal);
		GlobalFree(SymbolsMemGlobal);
		SymbolsMemGlobal = 0;
	}

	if (SymbolsPosGlobal != NULL)
	{
		GlobalUnlock(SymbolsPosGlobal);
		GlobalFree(SymbolsPosGlobal);
		SymbolsPosGlobal = 0;
	}

	if (ObjectTextsGlobal != NULL)
	{
		GlobalUnlock(ObjectTextsGlobal);
		GlobalFree(ObjectTextsGlobal);
		ObjectTextsGlobal = 0;
	}

	if (ObjectArcsGlobal != NULL)
	{
		GlobalUnlock(ObjectArcsGlobal);
		GlobalFree(ObjectArcsGlobal);
		ObjectArcsGlobal = NULL;
	}

	if (ObjectCirclesGlobal != NULL)
	{
		GlobalUnlock(ObjectCirclesGlobal);
		GlobalFree(ObjectCirclesGlobal);
		ObjectCirclesGlobal = NULL;
	}

	if (ObjectRectsGlobal != NULL)
	{
		GlobalUnlock(ObjectRectsGlobal);
		GlobalFree(ObjectRectsGlobal);
		ObjectRectsGlobal = NULL;
	}

	if (ObjectLinesGlobal != NULL)
	{
		GlobalUnlock(ObjectLinesGlobal);
		GlobalFree(ObjectLinesGlobal);
		ObjectLinesGlobal = NULL;
	}

	if (NetLabelsGlobal != NULL)
	{
		GlobalUnlock(NetLabelsGlobal);
		GlobalFree(NetLabelsGlobal);
		NetLabelsGlobal = NULL;
	}

	if (JunctionsGlobal != NULL)
	{
		GlobalUnlock(JunctionsGlobal);
		GlobalFree(JunctionsGlobal);
		JunctionsGlobal = NULL;
	}

	if (OnePinNetsGlobal != NULL)
	{
		GlobalUnlock(OnePinNetsGlobal);
		GlobalFree(OnePinNetsGlobal);
		OnePinNetsGlobal = NULL;
	}

	if (BusConnectionsGlobal != NULL)
	{
		GlobalUnlock(BusConnectionsGlobal);
		GlobalFree(BusConnectionsGlobal);
		BusConnectionsGlobal = NULL;
	}

	if (BussesGlobal != NULL)
	{
		GlobalUnlock(BussesGlobal);
		GlobalFree(BussesGlobal);
		BussesGlobal = NULL;
	}

	if (WiresGlobal != NULL)
	{
		GlobalUnlock(WiresGlobal);
		GlobalFree(WiresGlobal);
		WiresGlobal = NULL;
	}

	if (RedefinedPinBussesGlobal != NULL)
	{
		GlobalUnlock(RedefinedPinBussesGlobal);
		GlobalFree(RedefinedPinBussesGlobal);
		RedefinedPinBussesGlobal = NULL;
	}

	if (SubPinDefsGlobal != NULL)
	{
		GlobalUnlock(SubPinDefsGlobal);
		GlobalFree(SubPinDefsGlobal);
		SubPinDefsGlobal = NULL;
	}

	if (PowerPinsGlobal != NULL)
	{
		GlobalUnlock(PowerPinsGlobal);
		GlobalFree(PowerPinsGlobal);
		PowerPinsGlobal = NULL;
	}

	if (PinBussesGlobal != NULL)
	{
		GlobalUnlock(PinBussesGlobal);
		GlobalFree(PinBussesGlobal);
		PinBussesGlobal = NULL;
	}

	if (PinsGlobal != NULL)
	{
		GlobalUnlock(PinsGlobal);
		GlobalFree(PinsGlobal);
		PinsGlobal = NULL;
	}


	MaxNrWires = 0;
	MaxNrBusses = 0;
	MaxNrJunctions = 0;
	MaxNrOnePinNets = 0;
	MaxNrNetLabels = 0;
	MaxNrBusConnections = 0;
	MaxNrGlobalConnections = 0;
	MaxNrObjectLines = 0;
	MaxNrObjectRects = 0;
	MaxNrObjectCircles = 0;
	MaxNrObjectArcs = 0;
	MaxNrObjectTexts = 0;
	MaxNrSheetSymbols = 0;
	MaxNrInstances = 0;
	MaxSymbolsMemSize = 0;
	MaxNrRedefinedPinBusses = 0;
	MaxNrPins = 0;
	MaxNrPowerPins = 0;
	MaxNrPinBusses = 0;
	MaxNrSubPinDefs = 0;

	memset(&Design, 0, sizeof(DesignRecord));
	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 NewDesign(int32 mode)
{

	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
	memset(&Design, 0, sizeof(DesignRecord));

	if ((mode & 2) == 2)
	{
		if (AllocateMemWires(DefMaxNrWires, 0) != 0)
			return 0;

		if (AllocateMemBusses(DefMaxNrBusses, 0) != 0)
			return 0;

		if (AllocateMemJunctions(DefMaxNrJunctions, 0) != 0)
			return 0;

		if (AllocateMemOnePinNets(DefMaxNrOnePinNets, 0) != 0)
			return 0;

		if (AllocateMemNetLabels(DefMaxNrNetLabels, 0) != 0)
			return 0;

		if (AllocateMemBusConnections(DefMaxNrBusConnections, 0) != 0)
			return 0;

		if (AllocateMemGlobalConnections(DefMaxNrGlobalConnections, 0) != 0)
			return 0;

		if (AllocateMemObjectLines(DefMaxNrObjectLines, 0) != 0)
			return 0;

		if (AllocateMemObjectRects(DefMaxNrObjectRects, 0) != 0)
			return 0;

		if (AllocateMemObjectCircles(DefMaxNrObjectCircles, 0) != 0)
			return 0;

		if (AllocateMemObjectArcs(DefMaxNrObjectArcs, 0) != 0)
			return 0;

		if (AllocateMemObjectTexts(DefMaxNrObjectTexts, 0) != 0)
			return 0;

		if (AllocateMemInstances(DefMaxNrInstances, 0) != 0)
			return 0;

		if (AllocateMemSheetSymbolsPos(DefMaxNrSheetSymbols, 0) != 0)
			return 0;

		if (AllocateMemSheetSymbols(DefMaxSymbolsMemory, 0) != 0)
			return 0;
	}

	Design.DimensionHeight = DIMENSION_HEIGHT;
	Design.ArrowLength = ARROW_LENGTH;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 NewSymbol(int32 mode)
{
	InstanceRecord *Instance;
	char str[MAX_LENGTH_STRING];

	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
	memset(&Design, 0, sizeof(DesignRecord));

	if ((mode & 2) == 2)
	{
		if (AllocateMemPins(DefMaxNrPins, 0) != 0)
			return 0;

		if (AllocateMemPowerPins(DefMaxNrPowerPins, 0) != 0)
			return 0;

		if (AllocateMemPinBusses(DefMaxNrPinBusses, 0) != 0)
			return 0;

		if (AllocateMemSubPinDefs(DefMaxNrSubPinDefs, 0) != 0)
			return 0;

		if (AllocateMemObjectLines(DefMaxNrObjectLines, 0) != 0)
			return 0;

		if (AllocateMemObjectRects(DefMaxNrObjectRects, 0) != 0)
			return 0;

		if (AllocateMemObjectCircles(DefMaxNrObjectCircles, 0) != 0)
			return 0;

		if (AllocateMemObjectArcs(DefMaxNrObjectArcs, 0) != 0)
			return 0;

		if (AllocateMemObjectTexts(DefMaxNrObjectTexts, 0) != 0)
			return 0;
	}

	if (AllocateMemInstances(1, 0) != 0)
		return 0;

	Instance = (InstanceRecord *) & ((*Instances)[0]);
	memset(Instance, 0, sizeof(InstanceRecord));
	Instance->PlacingOption = -1;

	if ((mode & 1) == 1)
	{
		GetNameWithOutExtensionFromFileName(EditFile, str);
		strcpy(Instance->Value, str);
		strcpy(Instance->Reference, "?");
	}

	Instance->ValueOriginY = (double) -2.0;
	Instance->RefOriginY = (double) -1.0;
	Design.NrInstances = 1;
	Design.DimensionHeight = DIMENSION_HEIGHT;
	Design.ArrowLength = ARROW_LENGTH;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 NewSheetSymbol(int32 mode)
{
	InstanceRecord *Instance;
	char str[MAX_LENGTH_STRING];


	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
	memset(&Design, 0, sizeof(DesignRecord));

	if ((mode & 2) == 2)
	{
		if (AllocateMemPins(DefMaxNrPins, 0) != 0)
			return 0;

		if (AllocateMemObjectLines(DefMaxNrObjectLines, 0) != 0)
			return 0;

		if (AllocateMemObjectRects(DefMaxNrObjectRects, 0) != 0)
			return 0;

		if (AllocateMemObjectCircles(DefMaxNrObjectCircles, 0) != 0)
			return 0;

		if (AllocateMemObjectArcs(DefMaxNrObjectArcs, 0) != 0)
			return 0;

		if (AllocateMemObjectTexts(DefMaxNrObjectTexts, 0) != 0)
			return 0;
	}

	if (AllocateMemInstances(1, 0) != 0)
		return 0;

	Instance = (InstanceRecord *) & ((*Instances)[0]);
	memset(Instance, 0, sizeof(InstanceRecord));
	Instance->RefInfo |= TEXT_NOT_VISIBLE;

	if ((mode & 1) == 1)
	{
		GetNameWithOutExtensionFromFileName(EditFile, str);
		strcpy(Instance->Value, str);
	}

	Instance->ValueOriginY = (double) -10.0;
	Design.NrInstances = 1;
	Design.DimensionHeight = DIMENSION_HEIGHT;
	Design.ArrowLength = ARROW_LENGTH;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemMessageBuf(int32 MemSize)
{
	HGLOBAL NewMem;

	if (MessageBufMemSize == 0)
	{
		MemSize = max(64 * 1024, MemSize);

		if ((MessageBufGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((MessageBuf = (char *) GlobalLock(MessageBufGlobal)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(MessageBufGlobal, MemSize, GHND)) == NULL)
			return -1;

		MessageBufGlobal = NewMem;

		if ((MessageBuf = (char *) GlobalLock(MessageBufGlobal)) == NULL)
			return -1;
	}

	MessageBufMemSize = MemSize;
//  MaxNrObjects=Count;
	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemClipBoard(int32 MemSize)
{
	HGLOBAL NewMem;

	if (GlobalClipBoardMem == NULL)
	{
		MemSize = max(MemSize, 16384);

		if ((GlobalClipBoardMem = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
			return -1;

		ClipBoardMemSize = MemSize;
	}
	else
	{
		if (ClipBoardMemSize < MemSize)
		{
			if ((NewMem = GlobalReAlloc(GlobalClipBoardMem, MemSize, GHND)) == NULL)
				return -1;

			GlobalClipBoardMem = NewMem;

			if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
				return -1;

			ClipBoardMemSize = MemSize;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateSpecialMem(int32 Index, int32 MemSize, void **MemP)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxSpecialMemory[Index] == 0)
	{
		if ((GlobalSpecialMem[Index] = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((SpecialMem[Index] = GlobalLock(GlobalSpecialMem[Index])) == NULL)
			return -1;

		*MemP = SpecialMem[Index];
		MaxSpecialMemory[Index] = MemSize;
	}
	else
	{
		if (MemSize > MaxSpecialMemory[Index])
		{
			if ((NewMem = GlobalReAlloc(GlobalSpecialMem[Index], MemSize, GHND)) == NULL)
				return -1;

			GlobalSpecialMem[Index] = NewMem;

			if ((SpecialMem[Index] = GlobalLock(GlobalSpecialMem[Index])) == NULL)
				return -1;

			MaxSpecialMemory[Index] = MemSize;
		}

		*MemP = SpecialMem[Index];
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemMessageBuf()
{
	if (MessageBufGlobal != NULL)
	{
		GlobalUnlock(MessageBufGlobal);
		GlobalFree(MessageBufGlobal);
		MessageBufMemSize = 0;
		MessageBufGlobal = NULL;
		MessageBufPos = 0;
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddToMessageBuf(LPSTR Line)
{
	int32 lengte;

	lengte = strlen(Line);

	if (lengte + 2 + MessageBufPos >= MessageBufMemSize)
	{
		if (AllocateMemMessageBuf(MessageBufMemSize + 256 * 1024) != 0)
			return -1;
	}

	if (MessageBufPos == 0)
		MessageBuf[0] = 0;

	if (lengte > 0)
	{
		memcpy((LPSTR) & MessageBuf[MessageBufPos], Line, lengte);
		MessageBufPos += lengte;
	}

	strcpy((LPSTR) & MessageBuf[MessageBufPos], "\r\n");
	MessageBufPos += 2;
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void MemoryMain()
{
	WindowStartX = 0;
	WindowStartY = 0;
	WindowWidth = 0;
	WindowHeight = 0;

	Created = 0;
	MouseChanged = 0;
	Painting = 0;
	DCInUse = 0;
	SelectionEsc = 0;
	UnselectAll = 0;

	DataBaseChanged = 0;
	GridVisible = 1;
	OkToAddViewPos = 1;
	BackGroundActive = 0;

	SystemBusyMode = 0;
	SelectionMode = 0;
	WireSelectMode = 1;

	MouseActionMode = 0;
	NrObjects3 = 0;
	NrObjects4 = 0;
	MaxNrObjects5 = 0;
	NrObjects5 = 0;
	ClipBoardMemSize = 0;
	RelX = (double) 0.0;
	RelY = (double) 0.0;
	GridSize = (double) 1.0;
//  GridSize                       = 100*2540.0;
	DrawGridSize = (double) 1.0;
//  DrawGridSize                   = 100*2540.0;
	ScrollSize = 100;
	ScrollSizeDrawing = 50;
	ScrollEndOfWindow = 20;
	MouseOnGridPosX = -1000000000;
	MouseOnGridPosY = -1000000000;
	CurrentObjectCode = -1;
	LastActionNr = 1;
	MaxLastActionNr = 1;
	AbsPosStr[0] = 0;
	RelPosStr[0] = 0;
	AbsGridPosStr[0] = 0;
	InfoStr[0] = 0;
	Units = 1;
	ViewPosPointer = 0;
	LastAction = 0;
	CurrentDrawMode = 0;
	RepeatMode = 0;
	NestingLevel = 0;

	Printing = 0;
	ReplaceSelections = 1;
	MemoryAllocated = 0;
	UndoRedoActive = 0;
	HelpAsked = 0;
	FileChanged = 0;
	FirstPaint = 1;
	PopupDisplayVisible = 1;
	AppendPropertiesToNetlabel = 1;
	AppendPropertiesToReferences = 1;
	Objects2Global = NULL;
	GlobalClipBoardMem = NULL;
	ClipBoardMemPos = 0;
	ZoomMode = 0;

	NewObjectText.Text[0] = 0;

	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
	memset(&Design, 0, sizeof(DesignRecord));
	Xoffset = (double) 0.0;
	Yoffset = (double) 0.0;
	Factor = (double) 10.0;
	DisplX = (double) 100.0;
	DisplY = (double) 100.0;

	VisibleMinX = (double) -100;
	VisibleMinY = (double) -100;
	VisibleMaxX = (double) 100;
	VisibleMaxY = (double) 100;


	MaxNrObjectLines = 0;
	MaxNrObjectRects = 0;
	MaxNrObjectCircles = 0;
	MaxNrObjectArcs = 0;
	MaxNrObjectTexts = 0;
	MaxNrPins = 0;
	MaxNrPowerPins = 0;
	MaxNrPinBusses = 0;
	MaxNrSubPinDefs = 0;
	MaxNrInstances = 0;
	MaxNrWires = 0;
	MaxNrBusses = 0;
	MaxNrJunctions = 0;
	MaxNrOnePinNets = 0;
	MaxNrNetLabels = 0;
	MaxNrBusConnections = 0;
	MaxNrGlobalConnections = 0;
	MaxNrSheetSymbols = 0;
	MaxSymbolsMemSize = 0;

	MousePosX = 10000;
	MousePosY = 10000;

	MousePanMultiply = 2;

	MessageBufMemSize = 0;
	MessageBufGlobal = NULL;
	MessageBufPos = 0;
	ButtonInfoTimeout = 40;
	ButtonInfoTimeoutStart = 10;

	GraphicsMain();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
