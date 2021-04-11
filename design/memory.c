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
#include "string.h"
#include "stdio.h"
#include "utf8.h"
#include "ctype.h"


#define        MAX_SPECIAL_MEM        32


int32 ClientWindowDivX, ClientWindowDivY, CurrentDrawMode, RepeatMode, NrDefaultSymbolDirs, CurrentDirection, Trace1Dir,
      Trace2Dir;

double SearchMinX, SearchMinY, SearchMaxX, SearchMaxY, TextMinX, TextMinY, TextMaxX, TextMaxY, CurrentDrawX1,
       CurrentDrawY1, CurrentDrawX2, CurrentDrawY2, StartSecondDrawingTraceX, StartSecondDrawingTraceY, DrawingTraceX1,
       DrawingTraceY1, DrawingTraceX2, DrawingTraceY2, DrawingConnectionX1, DrawingConnectionY1, DrawingConnectionX2,
       DrawingConnectionY2;

int32 WindowWidth, WindowHeight, MousePosX, MousePosY, ScrollSize, ScrollSizeDrawing, SelectionMode, LastActionNr,
      ScrollEndOfWindow, MinActionNr, MaxLastActionNr, CompRotationForMoving, NrDesigns, SaveSymbolsLocally,
      DeleteInfoCount, MouseActionMode, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1, SelectRectX2,
      SelectRectY2;


int32 MaxNrComponents, MessageMemSize, ObjectTextBufMemSize, ObjectTextBufPos, NetInfoSize, NrInt32Objects1,
      MaxNrInt32Objects1, NrInt32Objects2, MaxNrInt32Objects2, NrObjectsInfo, MaxNrObjectsInfo, TotalNrNets,
      PowerPinNetPos, ClipBoardMemSize, ClipBoardMemPos, MaxNrObjects, MaxNrObjects2, MaxNrObjects3, MaxNrObjects4,
      MaxNrObjects5, MaxNrObjects6, MaxTempMemory, TempMemorySize, MaxTempMemory2, MaxSpecialMemory[32], TempMemorySize2;

int32 NetNrWires, NetNrBusses, NetNrJunctions, NetNrBusConnections, NetNrGlobalConnections, NetNrInstances,
      NetNrNetLabels;


int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, RightButtonPressed, UnselectAll,
      LeftButtonDoublePressed, CompsSelectionMode, DataBaseChanged, MovingComponents, ReplaceSelections,
      ForceDisableOnePinNetCheck, MouseCursorOnGrid, FirstWrite, CreateTopSheet, CreateLayoutFile, DrawOnGrid,
      GridVisible, FirstPaint, TraceCoorsSwitched, MemoryAllocated, DrawTwoTryingTraces, HelpAsked,
      InsertTwoTracesFromDrawing, EditingSheetSymbol, ReDrawing, DrawSCH, DrawTraceUsingGuide, Printing, EditingSymbol,
      FileChanged, MouseTraceDrawing, UndoRedoActive, SelectionActive, SelectionEsc, ExecuteZoom, TraceMode,
      EmptyBrushActive, FillObjects, DrawCompPlacement, DrawCompOutline, DesignActive, UsingPartNumbers,
      DisableOnePinNetCheck, DrawCompReference, DrawCompValue, CompRotationChanged, OkToAddViewPos, IncDeleteNr,
	  UseGerbv, UseLanguage;

ObjectRecord DrawWire1, DrawWire2, LimitObject, WireObject1, WireObject2, CurrentWorkingWire, NullObject;

uint8 *ClipBoardMem;
HGLOBAL GlobalClipBoardMem;

double Xoffset, Yoffset, Factor, Factor2, RelX, RelY, CurrentFontSize, GridSize, DrawGridSize;

int32 FirstSize, NrObjects, NrObjects2, NrObjects3, NrObjects4, NrObjects5, NrObjects6, MaxNrNewInstances,
      NrNewInstances, MaxNrNewSymbols, NrNewSymbols, MouseOnGridPosX, MouseOnGridPosY, NrSearchDirs, CurrentObjectCode,
      CurrentColorCode, CurrentFontCode, Units, Last, LastAction, MemoryAllocationMode, ViewPosPointer,
      LastTracePosPointer, NrWireLabels, LibNameNr, SymbolDirNameNr, NrLibFiles, NrSymbolDirs, CurrentNet, SCHSystemError,
      NrSelectionsDialog, AnnotateStart, AnnotateEnd, AnnotateMode, NestingLevel, DesignBufMemSize, NrSheets,
      NrComponents, AnnotateMode, PaperSize, NrGeometryLibraries, NrSchematicSymbolLibraries, NrNetInfos,
      NewSymbolsMemSize, MaxNewSymbolsMemSize, CurrentFontNr, CurrentFontRotation;

int32 Debugfp;

HGLOBAL GlobalClipBoardMem, GlobalSpecialMem[MAX_SPECIAL_MEM];
uint8 *ClipBoardMem, *DesignBuf, *TempMem, *TempMem2, *SpecialMem[MAX_SPECIAL_MEM];
int32Array *ObjectsPosition1, *ObjectsPosition2;
ObjectsInfoArray *ObjectsInfo;

ViewPosArray ViewPos;
LastTracePosArray LastTracePos;

double DisplX, DisplY;


char WindowStr[MAX_LENGTH_STRING];
char DesignPath[MAX_LENGTH_STRING];
char ExePath[MAX_LENGTH_STRING];
char ProjectPath[MAX_LENGTH_STRING];
char LanguagePath[MAX_LENGTH_STRING];
char SheetDir[MAX_LENGTH_STRING];
char GeometrieLibraryPath[MAX_LENGTH_STRING];
char LibraryPath[MAX_LENGTH_STRING];
char DefaultGeomLibraryPath[MAX_LENGTH_STRING];
char DefaultLibraryPath[MAX_LENGTH_STRING];
char EditFile[MAX_LENGTH_STRING];
char DesignFile[MAX_LENGTH_STRING];
char LayoutFile[MAX_LENGTH_STRING];
char DesignName[MAX_LENGTH_STRING];
char DesignDir[MAX_LENGTH_STRING];
char UserIniFile[MAX_LENGTH_STRING];
char DefaultSymbolDirs[8][MAX_LENGTH_STRING];
char TopSheetName[MAX_LENGTH_STRING];
char SymbolDirs[8][MAX_LENGTH_STRING];
char LastDesigns[TotalNrDesigns][MAX_LENGTH_STRING];
char GeometryLibraries[16][MAX_LENGTH_STRING];
char SchematicSymbolLibraries[16][MAX_LENGTH_STRING];
char GerbvPath[MAX_LENGTH_STRING];


char *MessageBuf;
char *ObjectTextBuf;
char PrinterName[MAX_LENGTH_STRING];
char ExcludeInBOMID[MAX_LENGTH_STRING];
char ExcludeInBOMValue[MAX_LENGTH_STRING];
LPSTR PWindowStr;

SheetRecord Sheets[MAX_NR_SHEETS], TempSheet[32];
ObjectArray *Objects, *Objects6;
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
RedefinedPinBusArray *RedefinedPinBusses;
SymbolsPosArray *SymbolsPos;
SymbolsPos2Array *SymbolsPos2, *NewSymbolsPos2;
//SubSheetsArray              *SubSheets;
GlobalConnectionsArray *GlobalConnections;
SymbolRecord DesignSymbol;
PinsArray *Pins;
SubPinDefsArray *SubPinDefs;
PinBusArray *PinBusses;
PowerPinsArray *PowerPins;
ComponentArray *Components;
NetInfoArray *NetInfos;
int16Array *NetOutputs;

InstancesArray *Instances, *NewInstances;
CharsArray *Chars;
//DeleteInfoArray             DeleteInfo;

ObjectRecord NewObject;
WireRecord NewWire;
BusRecord NewBus;
JunctionRecord NewJunction;
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
RedefinedPinBusRecord RedefinedNewPinBus;
PowerPinRecord NewPowerPin;
GlobalConnectionRecord NewGlobalConnection;


uint8 TextMirrorX[9] = { 6, 7, 8, 3, 4, 5, 0, 1, 2 };
uint8 TextMirrorY[9] = { 2, 1, 0, 5, 4, 3, 8, 7, 6 };

uint8(*SymbolsMem)[];
uint8(*NewSymbolsMem)[];

HGLOBAL DesignBufGlobal, ComponentsGlobal, ObjectsGlobal, Objects2Global, Objects3Global, Objects4Global,
        Objects5Global, Objects6Global, ObjectTextBufGlobal, NetInfoGlobal, Int32Objects1Global, Int32Objects2Global,
        ObjectsInfoGlobal, GlobalTempMem, GlobalTempMem2, MessageGlobal, NewInstancesGlobal, NewSymbolsPos2Global,
        NewSymbolsGlobal;
//                            ObjectsGlobal3,
//                            ObjectsGlobal4


HWND DESIGNWindow, EditWindow;
HDC SCHDisplay2, OutputDisplay;
BOMRecord BOMInfo;


// ********************************************************************************************************
// ********************************************************************************************************

int32 NrDesignNames, DesignNamesPos;
LPSTR DesignNamesId[MaxNrDesignNames];


char DesignNamesBuf[DesignNamesBufSize];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


LPSTR StringConvert(int32 Nr, LPSTR str)
{
	int32 StringCount1, StringCount2, ParamCount1, ParamCount2, cnt, Length;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	if (!str)
	{
		return "";
//    MessageBoxUTF8(NULL,"string pointer is zero","System error",MB_APPLMODAL|MB_OK);
	}

	if ((Nr < 0) || (Nr >= MaxNrDesignNames) || (DesignNamesId[Nr] == 0))
		return str;

	strcpy(str1, str);
	strcpy(str2, DesignNamesId[Nr]);

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
		/*
		    sprintf(str3,"Error in string number %d of the language file (Wrong parameters).",Nr);
		    strcat(str3,"\n\nInstead the english string will be used");
		    MessageBoxUTF8(NULL,str3,"System error",MB_APPLMODAL|MB_OK);
		*/
		return str;
	}

	return DesignNamesId[Nr];
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetStringKomma(LPSTR Str, LPSTR Result)
{
	int32 cnt, l;
	memset(Result, 0, MAX_LENGTH_STRING);
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPinNames(LPSTR str)
{
	char str2[300], str3[300];
	int32 length;

	if ((length = strlen(str)) == 0)
		return -1;

	if (length > 250)
		return -1;

	memset(&str2, 0, 300);
	memmove(&str2, str, length);

	while (str2[0] != 0)
	{
		memset(&str3, 0, 300);
		GetStringKomma(str2, str3);

		if (CheckPinName(str3, 0) == -1)
			return -1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrPinNames(LPSTR str)
{
	char str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	int32 length, count;

	count = 0;

	if ((length = strlen(str)) == 0)
		return -1;

	if (length > MAX_LENGTH_STRING - 50)
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrLabelsBusName(LPSTR BusName)
{
	int32 lengte, cnt, pos1, pos2, pos3, num1, num2;

	lengte = strlen(BusName);

	if (lengte == 0)
		return -1;

	if (isdigit(BusName[0]))
		return -1;

	cnt = 0;

	while ((cnt < lengte) && (BusName[cnt] != '[') && (BusName[cnt] != ':') && (BusName[cnt] != ']'))
		cnt++;

	if (cnt == lengte)
		return 0;

	if (BusName[cnt] != '[')
		return -1;

	cnt = 0;

	while ((cnt < lengte) && (BusName[cnt] != '['))
		cnt++;

	if (cnt == 0)
		return -1;

	pos1 = cnt + 1;

	while ((cnt < lengte) && (BusName[cnt] != ':'))
		cnt++;

	if (cnt == lengte)
		return -1;

	pos2 = cnt + 1;

	while ((cnt < lengte) && (BusName[cnt] != ']'))
		cnt++;

	if (cnt == lengte)
		return -1;

	pos3 = cnt;

	num1 = 0;
	num2 = 0;

	cnt = pos1;

	while ((cnt < lengte) && (BusName[cnt] != ':'))
	{
		if (!isdigit(BusName[cnt]))
			return -1;

		num1 = num1 * 10 + BusName[cnt] - 48;
		cnt++;
	}

	cnt = pos2;

	while ((cnt < lengte) && (BusName[cnt] != ']'))
	{
		if (!isdigit(BusName[cnt]))
			return -1;

		num2 = num2 * 10 + BusName[cnt] - 48;
		cnt++;
	}

	if (num2 < num1)
		return -1;

	cnt = 0;
	return (num2 - num1 + 1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetLabelBusNameByIndex(LPSTR BusName, LPSTR LabelName, int32 Index)
{
	int32 lengte, cnt, pos1, pos2, pos3, num1, num2, res;
	char str[MAX_LENGTH_STRING];

	if (GetNrLabelsBusName(BusName) < 1)
		return -1;

	lengte = strlen(BusName);

	cnt = 0;

	while ((cnt < lengte) && (BusName[cnt] != '['))
		cnt++;

	pos1 = cnt;

	while ((cnt < lengte) && (BusName[cnt] != ':'))
		cnt++;

	pos2 = cnt;

	while ((cnt < lengte) && (BusName[cnt] != ']'))
		cnt++;

	pos3 = cnt + 1;

	num1 = 0;
	num2 = 0;

	cnt = pos1 + 1;

	while ((cnt < lengte) && (BusName[cnt] != ':'))
	{
		num1 = num1 * 10 + BusName[cnt] - 48;
		cnt++;
	}

	cnt = pos2;

	while ((cnt < lengte) && (BusName[cnt] != ']'))
	{
		num2 = num2 * 10 + BusName[cnt] - 48;
		cnt++;
	}

	if (Index >= (num2 - num1 + 1))
		return -1;

	strcpy(LabelName, BusName);
	LabelName[pos1] = 0;
	sprintf(str, "%i", num1 + Index);
	strcat(LabelName, str);
	res = strlen(LabelName);

	if (pos3 == lengte)
		return 0;

	memmove(&LabelName[res], &BusName[pos3], lengte - pos3 + 1);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckIfSubPinBusIsPartOfPinBus(LPSTR PinBusName, LPSTR SubPinBusName)
{
	int32 PinBusCount, SubPinBusCount, cnt, cnt2;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	int32 Found;

	PinBusCount = GetNrLabelsBusName(PinBusName);
	SubPinBusCount = GetNrLabelsBusName(SubPinBusName);

	if ((PinBusCount < 1) || (SubPinBusCount < 1) || (PinBusCount > 64) || (SubPinBusCount > 64)
	        || (SubPinBusCount > PinBusCount))
		return -1;

	for (cnt = 0; cnt < SubPinBusCount; cnt++)
	{
		if (GetLabelBusNameByIndex(SubPinBusName, str2, cnt) == -1)
			return -1;

		Found = 0;
		cnt2 = 0;

		while ((cnt2 < PinBusCount) && (!Found))
		{
			if (GetLabelBusNameByIndex(PinBusName, str1, cnt2) == -1)
				return -1;

			if (stricmpUTF8(str1, str2) == 0)
				Found = 1;

			cnt2++;
		}

		if (!Found)
			return -1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetPinNameFromPinBus(LPSTR PinBusName, LPSTR PinName, int32 NrPins, int32 Index)
{
	int32 lengte, cnt, cnt2, pos1, pos2, LineNr;
	int32 FoundKomma;
	char PreviousChar;

	LineNr = 0;
	pos2 = 0;
	lengte = strlen(PinBusName);

	if (lengte == 0)
		return -1;

	if (Index < 0)
		return -1;

	cnt = 0;
	cnt2 = 0;
	pos1 = 0;
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckGeometrie(LPSTR str)
{
	return CheckString(str, 2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemMessage(int32 MemSize)
{
	HGLOBAL NewMem;


	if (MessageMemSize == 0)
	{
		MemSize = max(65536, MemSize);

		if ((MessageGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((MessageBuf = (char *) GlobalLock(MessageGlobal)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(MessageGlobal, MemSize, GHND)) == NULL)
			return -1;

		MessageGlobal = NewMem;

		if ((MessageBuf = (char *) GlobalLock(MessageGlobal)) == NULL)
			return -1;
	}

	MessageMemSize = MemSize;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjectTextBuf(int32 MemSize)
{
	HGLOBAL NewMem;


	if (ObjectTextBufMemSize == 0)
	{
		MemSize = max(65536, MemSize);

		if ((ObjectTextBufGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((ObjectTextBuf = (char *) GlobalLock(ObjectTextBufGlobal)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectTextBufGlobal, MemSize, GHND)) == NULL)
			return -1;

		ObjectTextBufGlobal = NewMem;

		if ((ObjectTextBuf = (char *) GlobalLock(ObjectTextBufGlobal)) == NULL)
			return -1;
	}

	ObjectTextBufMemSize = MemSize;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemNetInfo(int32 NetInfoCount)
{
	HGLOBAL NewMem;


	if (NetInfoSize == 0)
	{
		NetInfoCount = max(1024, NetInfoCount);

		if ((NetInfoGlobal = GlobalAlloc(GHND, NetInfoCount * sizeof(NetInfoRecord))) == NULL)
			return -1;

		if ((NetInfos = (NetInfoArray *) GlobalLock(NetInfoGlobal)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(NetInfoGlobal, NetInfoCount * sizeof(NetInfoRecord), GHND)) == NULL)
			return -1;

		NetInfoGlobal = NewMem;

		if ((NetInfos = (NetInfoArray *) GlobalLock(NetInfoGlobal)) == NULL)
			return -1;
	}

	NetInfoSize = NetInfoCount;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjectsInfo(int32 Count)
{
	HGLOBAL NewMem;


	if (MaxNrObjectsInfo == 0)
	{
		Count = max(4096, Count);

		if ((ObjectsInfoGlobal = GlobalAlloc(GHND, Count * sizeof(ObjectsInfoRecord))) == NULL)
			return -1;

		if ((ObjectsInfo = (ObjectsInfoArray *) GlobalLock(ObjectsInfoGlobal)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectsInfoGlobal, Count * sizeof(ObjectsInfoRecord), GHND)) == NULL)
			return -1;

		ObjectsInfoGlobal = NewMem;

		if ((ObjectsInfo = (ObjectsInfoArray *) GlobalLock(ObjectsInfoGlobal)) == NULL)
			return -1;
	}

	MaxNrObjectsInfo = Count;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjectsPosition1(int32 Count)
{
	HGLOBAL NewMem;


	if (MaxNrInt32Objects1 == 0)
	{
		Count = max(4096, Count);

		if ((Int32Objects1Global = GlobalAlloc(GHND, Count * sizeof(int32))) == NULL)
			return -1;

		if ((ObjectsPosition1 = (int32Array *) GlobalLock(Int32Objects1Global)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Int32Objects1Global, Count * sizeof(int32), GHND)) == NULL)
			return -1;

		Int32Objects1Global = NewMem;

		if ((ObjectsPosition1 = (int32Array *) GlobalLock(Int32Objects1Global)) == NULL)
			return -1;
	}

	MaxNrInt32Objects1 = Count;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjectsPosition2(int32 Count)
{
	HGLOBAL NewMem;


	if (MaxNrInt32Objects2 == 0)
	{
		Count = max(4096, Count);

		if ((Int32Objects2Global = GlobalAlloc(GHND, Count * sizeof(int32))) == NULL)
			return -1;

		if ((ObjectsPosition2 = (int32Array *) GlobalLock(Int32Objects2Global)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Int32Objects2Global, Count * sizeof(int32), GHND)) == NULL)
			return -1;

		Int32Objects2Global = NewMem;

		if ((ObjectsPosition2 = (int32Array *) GlobalLock(Int32Objects2Global)) == NULL)
			return -1;
	}

	MaxNrInt32Objects2 = Count;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemDesignBuf(int32 MemSize)
{
	HGLOBAL NewMem;

	if (DesignBufMemSize == 0)
	{
		MemSize = max(64 * 1024, MemSize);

		if ((DesignBufGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((DesignBuf = (uint8 *) GlobalLock(DesignBufGlobal)) == NULL)
			return -1;

		DesignBufMemSize = MemSize;
	}
	else
	{
		if (DesignBufMemSize < MemSize)
		{
			if ((NewMem = GlobalReAlloc(DesignBufGlobal, MemSize, GHND)) == NULL)
				return -1;

			DesignBufGlobal = NewMem;

			if ((DesignBuf = (uint8 *) GlobalLock(DesignBufGlobal)) == NULL)
				return -1;

			DesignBufMemSize = MemSize;
		}
	}

//  MaxNrObjects=Count;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemComponents(int32 NrComponents)
{
	MaxNrComponents = max(128, MaxNrComponents);

	if ((ComponentsGlobal = GlobalAlloc(GHND, NrComponents * sizeof(ComponentRecord))) == NULL)
		return -1;

	if ((Components = (ComponentArray *) GlobalLock(ComponentsGlobal)) == NULL)
		return -1;

//  ComponentsMemSize=MemSize;
	MaxNrComponents = NrComponents;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjects(int32 Count)
{
	HGLOBAL NewMem;

	if (MaxNrObjects == 0)
	{
		Count = max(128, Count);

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjects2(int32 Count)
{
	HGLOBAL NewMem;

	if (MaxNrObjects2 == 0)
	{
		Count = max(512, Count);

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjects3(int32 Count)
{
	HGLOBAL NewMem;

	if (MaxNrObjects3 == 0)
	{
		Count = max(64, Count);

		if ((Objects3Global = GlobalAlloc(GHND, Count * sizeof(Object2Record))) == NULL)
			return -1;

		if ((Objects3 = (Object2Array *) GlobalLock(Objects3Global)) == NULL)
			return -1;

		MaxNrObjects3 = Count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Objects3Global, Count * sizeof(Object2Record), GHND)) == NULL)
			return -1;

		Objects3Global = NewMem;

		if ((Objects3 = (Object2Array *) GlobalLock(Objects3Global)) == NULL)
			return -1;

		MaxNrObjects3 = Count;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjects4(int32 Count)
{
	HGLOBAL NewMem;

	if (MaxNrObjects4 == 0)
	{
		Count = max(64, Count);

		if ((Objects4Global = GlobalAlloc(GHND, Count * sizeof(Object2Record))) == NULL)
			return -1;

		if ((Objects4 = (Object2Array *) GlobalLock(Objects4Global)) == NULL)
			return -1;

		MaxNrObjects4 = Count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Objects4Global, Count * sizeof(Object2Record), GHND)) == NULL)
			return -1;

		Objects4Global = NewMem;

		if ((Objects4 = (Object2Array *) GlobalLock(Objects4Global)) == NULL)
			return -1;

		MaxNrObjects4 = Count;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjects6(int32 Count)
{
	HGLOBAL NewMem;

	if (MaxNrObjects6 == 0)
	{
		Count = max(256, Count);

		if ((Objects6Global = GlobalAlloc(GHND, Count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects6 = (ObjectArray *) GlobalLock(Objects6Global)) == NULL)
			return -1;

		MaxNrObjects6 = Count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Objects6Global, Count * sizeof(ObjectRecord), GHND)) == NULL)
			return -1;

		Objects6Global = NewMem;

		if ((Objects6 = (ObjectArray *) GlobalLock(Objects6Global)) == NULL)
			return -1;

		MaxNrObjects6 = Count;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemClipBoard(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (GlobalClipBoardMem == NULL)
	{
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemNewSymbols(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (NewSymbolsGlobal == NULL)
	{
		if ((NewSymbolsGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((NewSymbolsMem = GlobalLock(NewSymbolsGlobal)) == NULL)
			return -1;

		MaxNewSymbolsMemSize = MemSize;
	}
	else
	{
		if (MaxNewSymbolsMemSize < MemSize)
		{
			if ((NewMem = GlobalReAlloc(NewSymbolsGlobal, MemSize, GHND)) == NULL)
				return -1;

			NewSymbolsGlobal = NewMem;

			if ((NewSymbolsMem = GlobalLock(NewSymbolsGlobal)) == NULL)
				return -1;

			MaxNewSymbolsMemSize = MemSize;
		}
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemNewSymbolsPos2(int32 NrSymbols)
{
	HGLOBAL NewMem;

	NrSymbols = max(NrSymbols, 256);

	if (NewSymbolsPos2Global == NULL)
	{
		if ((NewSymbolsPos2Global = GlobalAlloc(GHND, NrSymbols * sizeof(SymbolsPos2Record))) == NULL)
			return -1;

		if ((NewSymbolsPos2 = GlobalLock(NewSymbolsPos2Global)) == NULL)
			return -1;

		MaxNrNewSymbols = NrSymbols;
	}
	else
	{
		if (MaxNrNewSymbols < NrSymbols)
		{
			if ((NewMem = GlobalReAlloc(NewSymbolsPos2Global, NrSymbols * sizeof(SymbolsPos2Record), GHND)) == NULL)
				return -1;

			NewSymbolsPos2Global = NewMem;

			if ((NewSymbolsPos2 = GlobalLock(NewSymbolsPos2Global)) == NULL)
				return -1;

			MaxNrNewSymbols = NrSymbols;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemNewInstances(int32 NrInstances)
{
	HGLOBAL NewMem;

	NrInstances = max(NrInstances, 128);

	if (MaxNrNewInstances >= NrInstances)
		return 1;

	if (NrInstances > LimitMaxNrInstances)
		return -2;

	if (NewInstancesGlobal == NULL)
	{
		if ((NewInstancesGlobal = GlobalAlloc(GHND, NrInstances * sizeof(InstanceRecord))) == NULL)
			return -1;

		if ((NewInstances = (InstancesArray *) GlobalLock(NewInstancesGlobal)) == NULL)
			return -1;

		MaxNrNewInstances = NrInstances;
	}
	else
	{
		if ((NewMem =
		            GlobalReAlloc(NewInstancesGlobal, NrInstances * sizeof(InstanceRecord),
		                          GMEM_MOVEABLE | GMEM_ZEROINIT)) == NULL)
			return -1;

		NewInstancesGlobal = NewMem;

		if ((NewInstances = (InstancesArray *) GlobalLock(NewInstancesGlobal)) == NULL)
			return -1;

		MaxNrNewInstances = NrInstances;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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
		if (MaxTempMemory < MemSize)
		{
			if ((NewMem = GlobalReAlloc(GlobalTempMem, MemSize, GHND)) == NULL)
				return -1;

			GlobalTempMem = NewMem;

			if ((TempMem = GlobalLock(GlobalTempMem)) == NULL)
				return -1;

			MaxTempMemory = MemSize;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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
		if (MaxTempMemory2 < MemSize)
		{
			if ((NewMem = GlobalReAlloc(GlobalTempMem2, MemSize, GHND)) == NULL)
				return -1;

			GlobalTempMem2 = NewMem;

			if ((TempMem2 = GlobalLock(GlobalTempMem2)) == NULL)
				return -1;

			MaxTempMemory2 = MemSize;
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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMem()
{
	/*
	  if (AllocateMemObjects(128)!=0) {
	    return 0;
	  }
	  if (AllocateMemObjects2(512)!=0) {
	    return 0;
	  }
	  if (AllocateMemDesignBuf(64*1024)) {
	    return 0;
	  }
	*/
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeallocateMem()
{
	if (Objects2Global != NULL)
	{
		GlobalUnlock(Objects2Global);
		GlobalFree(Objects2Global);
		Objects2Global = NULL;
		NrObjects2 = 0;
		MaxNrObjects2 = 0;
	}

	if (Objects3Global != NULL)
	{
		GlobalUnlock(Objects3Global);
		GlobalFree(Objects3Global);
		Objects3Global = NULL;
		NrObjects3 = 0;
		MaxNrObjects3 = 0;
	}

	if (Objects4Global != NULL)
	{
		GlobalUnlock(Objects4Global);
		GlobalFree(Objects4Global);
		Objects4Global = NULL;
		NrObjects4 = 0;
		MaxNrObjects4 = 0;
	}

	if (ObjectsGlobal != NULL)
	{
		GlobalUnlock(ObjectsGlobal);
		GlobalFree(ObjectsGlobal);
		ObjectsGlobal = NULL;
		NrObjects = 0;
		MaxNrObjects = 0;
	}

	if (Objects5Global != NULL)
	{
		GlobalUnlock(Objects5Global);
		GlobalFree(Objects5Global);
		Objects5Global = NULL;
		NrObjects5 = 0;
		MaxNrObjects5 = 0;
	}

	if (Objects6Global != NULL)
	{
		GlobalUnlock(Objects6Global);
		GlobalFree(Objects6Global);
		Objects6Global = NULL;
		NrObjects6 = 0;
		MaxNrObjects6 = 0;
	}

	if (DesignBufGlobal != NULL)
	{
		GlobalUnlock(DesignBufGlobal);
		GlobalFree(DesignBufGlobal);
		DesignBufGlobal = NULL;
		DesignBufMemSize = 0;
	}

	/*
	  if (MessageGlobal!=NULL) {
	    MessageGlobal=NULL;
	    GlobalUnlock(MessageGlobal);
	    GlobalFree(MessageGlobal);
	    MessageMemSize=0;
	  }
	*/
	if (ObjectTextBufGlobal != NULL)
	{
		GlobalUnlock(ObjectTextBufGlobal);
		GlobalFree(ObjectTextBufGlobal);
		ObjectTextBufGlobal = NULL;
		ObjectTextBufMemSize = 0;
		ObjectTextBufPos = 0;
	}

	if (NetInfoGlobal != NULL)
	{
		GlobalUnlock(NetInfoGlobal);
		GlobalFree(NetInfoGlobal);
		NetInfoGlobal = NULL;
		NetInfoSize = 0;
		NrNetInfos = 0;
	}

	if (Int32Objects1Global != NULL)
	{
		GlobalUnlock(Int32Objects1Global);
		GlobalFree(Int32Objects1Global);
		Int32Objects1Global = NULL;
		MaxNrInt32Objects1 = 0;
		NrInt32Objects1 = 0;
	}

	if (Int32Objects2Global != NULL)
	{
		GlobalUnlock(Int32Objects2Global);
		GlobalFree(Int32Objects2Global);
		Int32Objects2Global = NULL;
		MaxNrInt32Objects2 = 0;
		NrInt32Objects2 = 0;
	}

	if (ObjectsInfo != NULL)
	{
		GlobalUnlock(ObjectsInfo);
		GlobalFree(ObjectsInfo);
		ObjectsInfo = NULL;
		MaxNrObjectsInfo = 0;
		NrObjectsInfo = 0;
	}

	if (GlobalClipBoardMem != NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		GlobalFree(GlobalClipBoardMem);
		ClipBoardMemSize = 0;
		ClipBoardMemPos = 0;
		GlobalClipBoardMem = NULL;
	}

	if (ComponentsGlobal != NULL)
	{
		GlobalUnlock(ComponentsGlobal);
		GlobalFree(ComponentsGlobal);
		ComponentsGlobal = NULL;
		MaxNrComponents = 0;
	}

	if (NewSymbolsGlobal != NULL)
	{
		GlobalUnlock(NewSymbolsGlobal);
		GlobalFree(NewSymbolsGlobal);
		NewSymbolsGlobal = NULL;
		MaxNewSymbolsMemSize = 0;
	}

	if (NewSymbolsPos2Global != NULL)
	{
		GlobalUnlock(NewSymbolsPos2Global);
		GlobalFree(NewSymbolsPos2Global);
		NewSymbolsPos2Global = NULL;
		MaxNrNewSymbols = 0;
	}

	if (NewInstancesGlobal != NULL)
	{
		GlobalUnlock(NewInstancesGlobal);
		GlobalFree(NewInstancesGlobal);
		NewInstancesGlobal = NULL;
		MaxNrNewInstances = 0;
	}

	if (GlobalTempMem != NULL)
	{
		GlobalUnlock(GlobalTempMem);
		GlobalFree(GlobalTempMem);
		GlobalTempMem = NULL;
		MaxTempMemory = 0;
		TempMemorySize = 0;
	}

	if (GlobalTempMem2 != NULL)
	{
		GlobalUnlock(GlobalTempMem2);
		GlobalFree(GlobalTempMem2);
		GlobalTempMem2 = NULL;
		MaxTempMemory2 = 0;
		TempMemorySize2 = 0;
	}

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MemoryMain()
{
	Created = 0;
	IncDeleteNr = 0;
	MouseChanged = 0;
	Painting = 0;
	DCInUse = 0;
	SelectionActive = 0;
	SelectionEsc = 0;
	UnselectAll = 0;

	DataBaseChanged = 0;
	MouseTraceDrawing = 0;
	DrawCompPlacement = 1;
	DrawCompOutline = 0;
	DrawCompReference = 0;
	DrawCompValue = 0;
	GridVisible = 1;
	TraceMode = 1;
	ReDrawing = 0;
	DrawTwoTryingTraces = 1;
	DrawTraceUsingGuide = 0;
	DrawSCH = 1;
	MovingComponents = 0;
	CompRotationChanged = 0;
	MouseCursorOnGrid = 1;
	InsertTwoTracesFromDrawing = 1;
	OkToAddViewPos = 1;
	UsingPartNumbers = 0;
	SaveSymbolsLocally = 1;

	SelectionMode = 0;
	CurrentNet = 0;
	SCHSystemError = 0;
	MouseActionMode = 0;
	ScrollSize = 100;
	ScrollSizeDrawing = 100;
	ScrollEndOfWindow = 20;
	Units = 1;
	ViewPosPointer = 0;
	LastTracePosPointer = 0;
	NrDesigns = 0;
	LastAction = 0;
	CurrentDrawMode = 0;
	RepeatMode = 0;
	NestingLevel = 0;
	NrSheets = 0;
	AnnotateMode = 0;
	MousePosX = 10000;
	MousePosY = 10000;

	ClipBoardMemSize = 0;
	MessageMemSize = 0;
	ObjectTextBufMemSize = 0;
	ObjectTextBufPos = 0;
	MaxNrInt32Objects1 = 0;
	NrInt32Objects1 = 0;
	MaxNrInt32Objects2 = 0;
	NrInt32Objects2 = 0;
	NrObjectsInfo = 0;
	MaxNrObjectsInfo = 0;
	NrNetInfos = 0;
	NrObjects = 0;
	NrObjects2 = 0;
	NrObjects3 = 0;
	NrObjects4 = 0;
	NrObjects5 = 0;
	NrObjects6 = 0;
	MaxNrObjects = 0;
	MaxNrObjects2 = 0;
	MaxNrObjects3 = 0;
	MaxNrObjects4 = 0;
	MaxNrObjects5 = 0;
	MaxNrObjects6 = 0;
	DesignBufMemSize = 0;
	NrDefaultSymbolDirs = 0;
	MaxNrNewInstances = 0;
	NrNewInstances = 0;
	NewSymbolsMemSize = 0;
	MaxNewSymbolsMemSize = 0;
	MaxNrNewSymbols = 0;
	NrNewSymbols = 0;
	MaxTempMemory = 0;
	TempMemorySize = 0;
	MaxTempMemory2 = 0;
	TempMemorySize2 = 0;

	HelpAsked = 0;
	DesignActive = 0;
	FirstPaint = 1;
	CreateTopSheet = 0;
	CreateLayoutFile = 0;

	PaperSize = -1;
	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
	memset(&Design, 0, sizeof(DesignRecord));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
