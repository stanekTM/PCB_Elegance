/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: memory.h
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


#ifndef _MEMORY

#define _MEMORY

#include "owntypes.h"
#include "types.h"

#define MaxNrSchNames                             1500
#define SchNamesBufSize                           32768

extern int32 ClientWindowDivX, ClientWindowDivY, CurrentDrawMode, RepeatMode, WireSelectMode, ViewMode,
       PopupDisplayVisible, ZoomMode, ButtonInfoTimeoutStart, ZoomInOutProcessed, SaveSymbolsLocally, MousePanMultiply,
       SystemBusyMode, ActiveOnePinNetError, CurrentDirection;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY, VisibleMaxX, VisibleMinX, VisibleMaxY, VisibleMinY, SearchMinX,
       SearchMinY, SearchMaxX, SearchMaxY;

extern int32 WindowWidth, WindowHeight, MousePosX, MousePosY, WindowStartX, WindowStartY, ScrollSize, ScrollSizeDrawing,
       ScrollEndOfWindow, MaxScrollBarX, MaxScrollBarY, SelectionMode, LastActionNr, MaxLastActionNr,
       EditingProtectedSymbol, MouseActionMode, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1, SelectRectX2,
       SelectRectY2;

extern int32 MaxNrWires, MaxNrBusses, MaxNrBusConnections, MaxNrJunctions, MaxNrOnePinNets, MaxNrNetLabels,
       MaxNrObjectLines, MaxNrObjectRects, MaxNrObjectCircles, MaxNrObjectArcs, MaxNrObjectTexts, MaxNrSheetSymbols,
       MaxNrGlobalConnections, MaxNrInstances, MaxNrSubPinDefs, NrSubPinDefs,
//                                MaxNrSubSheets,
       MaxNrPins, MaxNrPinBusses, MaxNrRedefinedPinBusses, MaxNrPowerPins, MaxNrExtraPins, MaxNrObjects, MaxNrObjects2,
       MaxNrObjects5, MaxNrStandardObjects, MessageBufMemSize, MessageBufPos, ClipBoardMemSize, ClipBoardMemPos,
       MaxTempMemory, MaxTempMemory2, TempMemorySize, TempMemorySize2, MaxSymbolsMemSize;

extern int32 NetNrWires, NetNrBusses, NetNrJunctions, NetNrBusConnections, NetNrGlobalConnections, NetNrInstances,
       NetNrNetLabels;

extern int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, MiddleButtonPressed,
       RightButtonPressed, UnselectAll, LeftButtonDoublePressed, SearchReference, SearchPartnr, SearchReplaceOptions,
       DataBaseChanged, GridVisible, ReplaceSelections, EditingSheetSymbol, Printing, EditingSymbol, FileChanged,
       MemoryAllocated, UndoRedoActive, HelpAsked, SelectionEsc, DisableOnePinNetCheck, OkToSwitchDrawingLayers,
       OkToAddViewPos, FirstPaint, BackGroundActive;

extern double Xoffset, Yoffset, Factor, RelX, RelY, CentreSelectedX, CentreSelectedY, CurrentX2, CurrentY2, GridSize,
       DrawGridSize;

extern int32 FirstSize, NrObjects, NrObjects2, NrObjects3, NrObjects4, NrStandardObjects, NrObjects5, NrConnections,
       MouseOnGridPosX, MouseOnGridPosY, CurrentObjectCode, CurrentGuideNr, Units, LastSheetNr, LastAction, NrWireLabels,
       NrLibFiles, NrSchematicSymbolLibraries, NrGeometryLibraries, OperatingSystem, ViewPosPointer, NestingLevel,
       SystemBusyMode, CrossHairVisible, ButtonInfoTimeout, AppendPropertiesToNetlabel, AppendPropertiesToReferences,
       SCHSystemError;

extern UINT ClosingWindowMessage;

extern int32 ReverseY;
extern int Debugfp;
extern HGLOBAL GlobalClipBoardMem;
extern uint8 *ClipBoardMem;

extern uint8 CircleConv[16];
extern uint8 CircleMirrorX[16];
extern uint8 CircleMirrorY[16];
extern uint8 CircleRotate90[16];
extern uint8 CircleRotate180[16];
extern uint8 CircleRotate270[16];
extern uint8 TextMirrorX[9];
extern uint8 TextMirrorY[9];
extern uint8 TextRotate90[9];
extern uint8 TextRotate180[9];
extern uint8 TextRotate270[9];

extern ViewPosArray ViewPos;
extern LastTracePosArray LastTracePos;

extern double DisplX, DisplY;

extern char WindowStr[MAX_LENGTH_STRING];
extern char AbsPosStr[MAX_LENGTH_STRING];
extern char RelPosStr[MAX_LENGTH_STRING];
extern char AbsGridPosStr[MAX_LENGTH_STRING];
extern char InfoStr[MAX_LENGTH_STRING];
extern char DesignPath[MAX_LENGTH_STRING];
extern char DesignFile[MAX_LENGTH_STRING];
extern char ProjectPath[MAX_LENGTH_STRING];
extern char ExePath[MAX_LENGTH_STRING];
extern char StartDir[MAX_LENGTH_STRING];
extern char EditPath[MAX_LENGTH_STRING];
extern char NewSymbolName[MAX_LENGTH_STRING];
extern char NewSymbolDir[MAX_LENGTH_STRING];
extern char NewLibName[MAX_LENGTH_STRING];
extern char SearchCodeString[20];
extern char GeometrieEditor[MAX_LENGTH_STRING];
extern char GeometryLibraries[16][MAX_LENGTH_STRING];
extern char SchematicSymbolLibraries[16][MAX_LENGTH_STRING];
extern char SearchString[MAX_LENGTH_STRING];
extern char ReplaceString[MAX_LENGTH_STRING];
extern char EditFile[MAX_LENGTH_STRING];
extern char LibNames[128][MAX_LENGTH_STRING];
extern char WireLabel[64][32];
extern char NestingFiles[8][MAX_LENGTH_STRING];
extern char *MessageBuf;

// extern NetInfoArray             *NetInfo;

extern int32 WiresSelected, BussesSelected, BusConnectionsSelected, JunctionsSelected, OnePinNetsSelected,
       NetLabelsSelected, ObjectLinesSelected, ObjectRectsSelected, ObjectCirclesSelected, ObjectArcsSelected,
       ObjectTextsSelected, GlobalConnectionsSelected, GlobalConnectionTextsSelected, InstancesSelected,
       InstanceRefsSelected, InstanceValuesSelected,
//                                SubSheetsSelected,
       BussesSelected, PinBussesSelected, PowerPinsSelected, PinsSelected;

extern ObjectArray *Objects;
extern StandardObjectArray *StandardObjects;
extern Object2Array *Objects2, *Objects3, *Objects4;
extern Object5Array *Objects5;
extern DesignRecord Design;
extern WiresArray *Wires;
extern BussesArray *Busses;
extern BusConnectionsArray *BusConnections;
extern JunctionsArray *Junctions;
extern OnePinNetsArray *OnePinNets;
extern NetLabelsArray *NetLabels;

extern uint8 *SymbolsMem;
extern InstancesArray *Instances;
extern ObjectLinesArray *ObjectLines;
extern ObjectRectsArray *ObjectRects;
extern ObjectCirclesArray *ObjectCircles;
extern ObjectArcsArray *ObjectArcs;
extern ObjectTextsArray *ObjectTexts;
extern SymbolsPosArray *SymbolsPos;
//extern SubSheetsArray           *SubSheets;
extern GlobalConnectionsArray *GlobalConnections;
extern SymbolRecord DesignSymbol;
extern SubPinDefsArray *SubPinDefs;
extern PinsArray *Pins;
extern PinBusArray *PinBusses;
extern RedefinedPinBusArray *RedefinedPinBusses;
extern PowerPinsArray *PowerPins;
extern SubPinDefsNameRecord SubPinDefsNames[32];
extern CompSelectArray *CompSelects;
extern CompSelectRecord *CompSelect;

extern ObjectRecord NewObject;
extern WireRecord NewWire;
extern BusRecord NewBus;
extern JunctionRecord NewJunction;
extern OnePinNetRecord NewOnePinNet;
extern BusConnectionRecord NewBusConnection;
extern NetLabelRecord NewNetLabel;
extern ObjectLineRecord NewObjectLine;
extern ObjectRectRecord NewObjectRect;
extern ObjectCircleRecord NewObjectCircle;
extern ObjectArcRecord NewObjectArc;
extern ObjectTextRecord NewObjectText;
extern InstanceRecord NewInstance;
extern PinRecord NewPin;
extern PowerPinRecord NewPowerPin;
extern PinBusRecord NewPinBus;
extern GlobalConnectionRecord NewGlobalConnection;

extern char SymbolAttributesIdent[MaxNrSymbolAttributes][64];
extern char SymbolAttributesValue[MaxNrSymbolAttributes][64];
extern int32 NrSymbolAttributes;

extern CharsArray *Chars;
extern uint8 TempRotation[4000];
extern uint8 *TempMem, *TempMem2;

extern uint8 Buf[];

extern uint8 CircleConv[16];
extern HWND hwndMDIClient;
extern HWND SCHWindow;
extern int32 NrChilds;
extern WindowSheetRecord WindowSheets;
extern WNDCLASS SCHClass;
extern WNDCLASS SCHClientClass;
extern HDC OutputDisplay;

// ********************************************************************************************************
// ********************************************************************************************************

extern int32 NrSchNames, SchNamesPos;
extern LPSTR SchNamesId[MaxNrSchNames];

extern char SchNamesBuf[SchNamesBufSize];


// ********************************************************************************************************
// ********************************************************************************************************

LPSTR StringConvert(int32 Nr, LPSTR str);

int32 CheckString(LPSTR str, int32 mode);

int32 CheckReference(LPSTR str);

int32 CheckNetName(LPSTR str);

int32 CheckLabelName(LPSTR str);

int32 CheckPinName(LPSTR str, int32 PowerPin);

int32 CheckPinNames(LPSTR str);

void GetStringKomma(LPSTR Str, LPSTR Result);

int32 GetPinNameFromPinBus(LPSTR PinBusName, LPSTR PinName, int32 NrPins, int32 Index);

int32 CheckGeometrie(LPSTR str);

int32 GetNrPinNames(LPSTR str);

int32 GetUsedMemSize(int32 mode);

int32 AllocateMemObjects(int32 Count, int32 mode);

int32 AllocateMemStandardObjects(int32 Count, int32 mode);

int32 AllocateMemObjects2(int32 Count, int32 mode);

int32 AllocateMemObjects5(int32 Count);

int32 AllocateMemWires(int32 NrWires, int32 mode);

int32 AllocateMemBusses(int32 NrBusses, int32 mode);

int32 AllocateMemJunctions(int32 NrJunctions, int32 mode);

int32 AllocateMemOnePinNets(int32 NrOnePinNets, int32 mode);

int32 AllocateMemNetLabels(int32 NrNetLabels, int32 mode);

int32 AllocateMemBusConnections(int32 NrBusConnections, int32 mode);

int32 AllocateMemGlobalConnections(int32 NrGlobalConnections, int32 mode);

int32 AllocateMemObjectLines(int32 NrObjectLines, int32 mode);

int32 AllocateMemObjectRects(int32 NrObjectRects, int32 mode);

int32 AllocateMemObjectCircles(int32 NrObjectCircles, int32 mode);

int32 AllocateMemObjectArcs(int32 NrObjectArcs, int32 mode);

int32 AllocateMemObjectTexts(int32 NrObjectTexts, int32 mode);

int32 AllocateMemPins(int32 NrPins, int32 mode);

int32 AllocateMemPowerPins(int32 NrPowerPins, int32 mode);

int32 AllocateMemPinBusses(int32 NrPinBusses, int32 mode);

int32 AllocateMemRedefinedPinBusses(int32 NrRedefinedPinBusses, int32 mode);

int32 AllocateMemInstances(int32 NrInstances, int32 mode);

int32 AllocateMemSubPinDefs(int32 NrSubPinDefs, int32 mode);

int32 AllocateMemSheetSymbolsPos(int32 NrSheetSymbols, int32 mode);

int32 AllocateMemSheetSymbols(int32 MemSheetSymbols, int32 mode);

void AllocateMem(void);

void AllocateMemSymbol(int32 Mode);

int32 AllocateMemTemp(int32 MemSize);

int32 AllocateMemTemp2(int32 MemSize);

void DeAllocateMemDesign(void);

void DeAllocateMem(void);

void DeAllocateMemTemp(void);

void DeAllocateMemTemp2(void);

void DeAllocateMemStandardObjects(void);

int32 AllocateMemClipBoard(int32 MemSize);

int32 NewDesign(int32 mode);

int32 NewSymbol(int32 mode);

int32 NewSheetSymbol(int32 mode);

void MemoryMain(void);

int32 AllocateMemMessageBuf(int32 MemSize);

int32 AllocateSpecialMem(int32 Index, int32 MemSize, void **MemP);

void DeAllocateMemMessageBuf(void);

int32 AddToMessageBuf(LPSTR Line);

int32 AddSchLanguageString(int32 ID, LPSTR Text);

int32 AddSchDialogLanguageString(LPSTR SearchText, LPSTR ReplaceText);

#endif
