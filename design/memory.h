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


#define MaxNrDesignNames               1500
#define DesignNamesBufSize             32768
#define SeparatorString                "***********************************************************************************************************************************************************************************\r\n"


extern int32 ClientWindowDivX, ClientWindowDivY, CurrentDrawMode, RepeatMode, CurrentDirection, Trace1Dir, Trace2Dir,
       NrDefaultSymbolDirs;

extern double CurrentDrawX1, CurrentDrawY1, CurrentDrawX2, CurrentDrawY2, TextMinX, TextMinY, TextMaxX, TextMaxY,
       SearchMinX, SearchMinY, SearchMaxX, SearchMaxY, SearchX1, SearchY1, SearchX2, SearchY2, ConnectionX1, ConnectionY1,
       ConnectionX2, ConnectionY2, StartSecondDrawingTraceX, StartSecondDrawingTraceY, DrawingTraceX1, DrawingTraceY1,
       DrawingTraceX2, DrawingTraceY2, DrawingConnectionX1, DrawingConnectionY1, DrawingConnectionX2, DrawingConnectionY2;

extern int32 WindowWidth, WindowHeight, MousePosX, MousePosY, ScrollSize, ScrollSizeDrawing, ScrollEndOfWindow,
       SelectionMode, LastActionNr, DrawDrillMode, MinActionNr, CompRotationForMoving, NrDesigns, MaxLastActionNr,
       SaveSymbolsLocally, DeleteInfoCount, MouseActionMode, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1,
       SelectRectX2, SelectRectY2;

extern int32 MaxNrComponents, MessageMemSize, ObjectTextBufMemSize, ObjectTextBufPos, NetInfoSize, TotalNrNets,
       MaxNrNewInstances, NrNewInstances, NewSymbolsMemSize, MaxNewSymbolsMemSize, PowerPinNetPos, NrInt32Objects1,
       MaxNrInt32Objects1, NrInt32Objects2, MaxNrInt32Objects2, NrObjectsInfo, ClipBoardMemSize, ClipBoardMemPos,
       MaxNrObjectsInfo, MaxNrObjects, MaxNrObjects2, MaxNrObjects3, MaxNrObjects4, MaxNrObjects5, MaxNrObjects6,
       MaxTempMemory, TempMemorySize, MaxTempMemory2, TempMemorySize2;


extern int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, RightButtonPressed, CreateTopSheet,
       CreateLayoutFile, UnselectAll, LeftButtonDoublePressed, CompsSelectionMode, DataBaseChanged,
       InsertTwoTracesFromDrawing, ViasSelectionMode, MovingComponents, MouseCursorOnGrid, TracesSelectionMode,
       CutDrawingTraceToFit, DrawOnGridForLimitTrace, GridVisible, DrawOnGrid, DrawDrill, DrawPowerPads, ReDrawing,
       DrawSCH, ReserveMemoryForPolygons, ConnectionCoorsSwitched, TraceCoorsSwitched, DrawTwoTryingTraces,
       ReplaceSelections, EditingSheetSymbol, ForceDisableOnePinNetCheck, Printing, EditingSymbol, FirstWrite, FileChanged,
       MemoryAllocated, UndoRedoActive, HelpAsked, SelectionActive, SelectionEsc, ExecuteZoom, TraceMode, EmptyBrushActive,
       FillObjects, DrawClearances, DrawSpecial, DrawCompPlacement, DrawCompOutline, DrawViaClearances, DrawCompReference,
       DrawCompValue, CompRotationChanged, OkToSwitchDrawingLayers, OkToAddViewPos, CurrentDrawingNetHilited,
       DrawViaInfoForTryingTrace, DesignActive, UsingPartNumbers, DisableOnePinNetCheck, DrawTraceUsingGuide, IncDeleteNr,
       DrawTraceLimitInfo, UseGerbv;

extern ObjectRecord DrawWire1, DrawWire2, LimitObject, WireObject1, WireObject2, CurrentWorkingWire, NullObject;

extern double Xoffset, Yoffset, Factor, Factor2, RelX, RelY, GridSize, CurrentFontSize, DrawGridSize;

extern int32 FirstSize, NrObjects, NrObjects2, NrObjects3, NrObjects4, NrObjects5, NrObjects6, MaxSpecialMemory[32],
       MaxNrNewSymbols, NrNewSymbols, NrConnections, MouseOnGridPosX, MouseOnGridPosY, CurrentObjectCode, CurrentGuideNr,
       Units, LastSheetNr, LastAction, NrSearchDirs, NrWireLabels, LibNameNr, SymbolDirNameNr, NrLibFiles, NrSymbolDirs,
       ViewPosPointer, LastTracePosPointer, AnnotateStart, AnnotateEnd, AnnotateMode, NestingLevel, DesignBufMemSize,
       NrSheets, NrComponents, AnnotateMode, NrGeometryLibraries, NrSchematicSymbolLibraries, NrNetInfos, PaperSize,
       CurrentNet, SCHSystemError, NrSelectionsDialog;

extern int Debugfp;
extern HGLOBAL GlobalClipBoardMem;
extern uint8 *ClipBoardMem, *DesignBuf, *TempMem, *TempMem2;
extern int32Array *ObjectsPosition1, *ObjectsPosition2;
extern ObjectsInfoArray *ObjectsInfo;

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
extern char DesignPath[MAX_LENGTH_STRING];
extern char ExePath[MAX_LENGTH_STRING];
extern char ProjectPath[MAX_LENGTH_STRING];
extern char SheetDir[MAX_LENGTH_STRING];
extern char GeometrieLibraryPath[MAX_LENGTH_STRING];
extern char LibraryPath[MAX_LENGTH_STRING];
extern char DefaultLibraryPath[MAX_LENGTH_STRING];
extern char DefaultGeomLibraryPath[MAX_LENGTH_STRING];
extern char EditFile[MAX_LENGTH_STRING];
extern char DefaultSymbolDirs[8][MAX_LENGTH_STRING];
extern char DesignFile[MAX_LENGTH_STRING];
extern char LayoutFile[MAX_LENGTH_STRING];
extern char SymbolDirs[8][MAX_LENGTH_STRING];
extern char DesignDir[MAX_LENGTH_STRING];
extern char DesignName[MAX_LENGTH_STRING];
extern char LastDesigns[TotalNrDesigns][MAX_LENGTH_STRING];
extern char TopSheetName[MAX_LENGTH_STRING];
extern char UserIniFile[MAX_LENGTH_STRING];
extern char PrinterName[MAX_LENGTH_STRING];
extern char GeometryLibraries[16][MAX_LENGTH_STRING];
extern char SchematicSymbolLibraries[16][MAX_LENGTH_STRING];
extern SheetRecord Sheets[MAX_NR_SHEETS], TempSheet[32];
extern char *MessageBuf;
extern char *ObjectTextBuf;
extern char ExcludeInBOMID[MAX_LENGTH_STRING];
extern char ExcludeInBOMValue[MAX_LENGTH_STRING];
extern char GerbvPath[MAX_LENGTH_STRING];


extern LPSTR PWindowStr;

// extern NetInfoArray             *NetInfo;


extern ObjectArray *Objects, *Objects6;
extern Object2Array *Objects2, *Objects3, *Objects4;
extern Object5Array *Objects5;
extern DesignRecord Design;
extern WiresArray *Wires;
extern BussesArray *Busses;
extern BusConnectionsArray *BusConnections;
extern JunctionsArray *Junctions;
extern OnePinNetsArray *OnePinNets;
extern NetLabelsArray *NetLabels;

extern uint8(*SymbolsMem)[], (*NewSymbolsMem)[];
extern InstancesArray *Instances, *NewInstances;
extern ObjectLinesArray *ObjectLines;
extern ObjectRectsArray *ObjectRects;
extern ObjectCirclesArray *ObjectCircles;
extern ObjectArcsArray *ObjectArcs;
extern ObjectTextsArray *ObjectTexts;
extern RedefinedPinBusArray *RedefinedPinBusses;
extern SymbolsPosArray *SymbolsPos;
extern SymbolsPos2Array *SymbolsPos2, *NewSymbolsPos2;
//extern SubSheetsArray           *SubSheets;
extern GlobalConnectionsArray *GlobalConnections;
extern SymbolRecord DesignSymbol;
extern SubPinDefsArray *SubPinDefs;
extern PinsArray *Pins;
extern PinBusArray *PinBusses;
extern PowerPinsArray *PowerPins;
extern ComponentArray *Components;
extern NetInfoArray *NetInfos;

extern ObjectRecord NewObject;
extern WireRecord NewWire;
extern BusRecord NewBus;
extern JunctionRecord NewJunction;
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

extern CharsArray *Chars;
extern uint8 TempRotation[4000];

#ifndef VC15
extern uint8 Buf[];
#else
extern uint8 Buf[];
#endif

extern uint8 CircleConv[16];
extern HWND DESIGNWindow, EditWindow;
extern HDC OutputDisplay;

extern BOMRecord BOMInfo;

// ********************************************************************************************************
// ********************************************************************************************************

extern int32 NrDesignNames, DesignNamesPos;
extern LPSTR DesignNamesId[MaxNrDesignNames];

extern char DesignNamesBuf[DesignNamesBufSize];

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

LPSTR StringConvert(int32 Nr, LPSTR str);

int32 CheckString(LPSTR str, int32 mode);

int32 CheckReference(LPSTR str);

int32 CheckNetName(LPSTR str);

int32 CheckIfSubPinBusIsPartOfPinBus(LPSTR PinBusName, LPSTR SubPinBusName);

int32 CheckLabelName(LPSTR str);

int32 CheckPinName(LPSTR str, int32 PowerPin);

int32 CheckPinNames(LPSTR str);

int32 GetNrLabelsBusName(LPSTR BusName);

int32 GetLabelBusNameByIndex(LPSTR BusName, LPSTR LabelName, int32 Index);

void GetStringKomma(LPSTR Str, LPSTR Result);

int32 GetPinNameFromPinBus(LPSTR PinBusName, LPSTR PinName, int32 NrPins, int32 Index);

int32 AllocateMemObjects(int32 Count);

int32 AllocateMemObjects2(int32 Count);

int32 AllocateMemObjects3(int32 Count);

int32 AllocateMemObjects4(int32 Count);

int32 AllocateMemObjects5(int32 Count);

int32 AllocateMemObjects6(int32 Count);

int32 AllocateMemObjectsPosition1(int32 Count);

int32 AllocateMemObjectsInfo(int32 Count);

int32 AllocateMemObjectsPosition2(int32 Count);

int32 AllocateMemDesignBuf(int32 MemSize);

int32 AllocateMemComponents(int32 NrComponents);

//int32 AllocateMemMessage(int32 MemSize);

int32 AllocateMemNetInfo(int32 NetInfoCount);

int32 AllocateMem(void);

int32 AllocateMemObjectTextBuf(int32 MemSize);

void DeallocateMem(void);

int32 AllocateMemClipBoard(int32 MemSize);

int32 AllocateMemNewInstances(int32 NrInstances);

int32 AllocateMemNewSymbols(int32 MemSize);

int32 AllocateMemNewSymbolsPos2(int32 NrSymbols);

int32 AllocateMemTemp(int32 MemSize);

int32 AllocateMemTemp2(int32 MemSize);

int32 AllocateSpecialMem(int32 Index, int32 MemSize, void **MemP);

void DeAllocateMemTemp(void);

void DeAllocateMemTemp2(void);

void MemoryMain(void);

#endif
