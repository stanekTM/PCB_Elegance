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


#define MaxNrPcbNames                             2048
#define PcbNamesBufSize                           49152

extern int32 ClientWindowDivX, ClientWindowDivY, NrParams, RepeatMode, LastAction, CurrentDirection, PreviousDirection,
       CurrentGuideNr, SystemBusyMode, DrawDrillMode, RepeatModeActive, SaveSymbolsLocally, ButtonInfoTimeout,
       ButtonInfoTimeoutStart, PopupDisplayVisible, StartWithMaximumView, ZoomInOutProcessed, FastPaint, MoveCompAutoZoom,
       ZoomMode, MousePanMultiply, DrawLayerCode[32], DrawCode, RepeatModeBusy;
extern double CurrentDrawX1, CurrentDrawY1, CurrentDrawX2, CurrentDrawY2, CurrentX2, CurrentY2, ViewOffsetX,
       ViewOffsetY, ViewScale, VisibleMaxX, VisibleMinX, VisibleMaxY, VisibleMinY, SearchMinX, SearchMinY, SearchMaxX,
       SearchMaxY, SearchX1, SearchY1, SearchX2, SearchY2, ConnectionX1, ConnectionY1, ConnectionX2, ConnectionY2,
       StartFirstDrawingTraceX, StartFirstDrawingTraceY, StartSecondDrawingTraceX, StartSecondDrawingTraceY,
       EndSecondDrawingTraceX, EndSecondDrawingTraceY, EndFirstDrawingTraceX, EndFirstDrawingTraceY, ShiftOffsetX,
       ShiftOffsetY, TraceWidths[30], ClearanceWidths[30], MinDrillForThermalRelief, EndPreviousTraceX, EndPreviousTraceY,
       StartPreviousTraceX, StartPreviousTraceY, DrawingConnectionX1, DrawingConnectionY1, DrawingConnectionX2,
       DrawingConnectionY2;

extern double GridSizes[30], GridSize, UserGridSize, CompGridSize, TraceGridSize, AreafillGridSize;


extern int32 WindowWidth, WindowHeight, WindowStartX, WindowStartY, MousePosX, MousePosY, MaxScrollBarX, MaxScrollBarY,
       ScrollSize, ScrollSizeDrawing, ScrollEndOfWindow, SelectionMode, LastActionNr, CurrentDrawingLayer,
       CurrentDrawingNetNr, ActionCountVias, MaxLastActionNr, DrawingTraceX1, DrawingTraceY1, DrawingTraceX2,
       DrawingTraceY2, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2;

extern int32 NrGraphicsLayers, GraphicsLayers[256];


extern int32 ViasSelected, CompsSelected, ConnectionsSelected, AllTracesSelected, TracesSelected[32],
       HorTracesSelected[32], VerTracesSelected[32], Diag1TracesSelected[32], Diag2TracesSelected[32],
       TraceLayersSelectable[32], ObjectLinesSelected, ObjectRectsSelected, ObjectRectsFilledSelected,
       ObjectCirclesSelected, ObjectCirclesFilledSelected, ObjectArcsSelected, ObjectTextsSelected, ObjectPolygonsSelected,
       MaxNrHorTraces[32], MaxNrVerTraces[32], MaxNrDiag1Traces[32], MaxNrDiag2Traces[32], MaxNrNetsPos[32], MaxNrVias,
       MaxNrViasPos, MaxNrConnectionsPos, MaxNrConnections, MaxNrNetConnections, MaxNrNets, MaxNrObjects, MaxNrObjects2,
       MaxNrObjects3, MaxNrObjects4, MaxNrObjects5, MaxNrObjects6, MaxNrErrorObjects, MaxNrComps, MaxNrShapes,
       MaxNrNetTests, MaxNrNetTests2, MaxNrObjectTexts2, MaxNrObjectPolygons, MaxObjectPolygonMemory, MaxNrAreaFills,
       MaxNrObjectLines, MaxNrObjectRects, MaxNrObjectCircles, MaxNrObjectArcs, MaxNrObjectTexts, MaxNrPolygon8Objects,
       MaxNrVerticesPolygon, MaxLayerPlotObjects[33], NrLayerPlotObjects[33], ClipBoardMemSize, ClipBoardMemPos,
       MaxTempMemory, TempMemorySize, MaxTempMemory2, TempMemorySize2, MaxTempMemory3, TempMemorySize3, MaxCompsMemory,
       MaxShapesMemory, MessageBufMemSize, MessageBufPos, MaxAreaFillMemoryTemp, MaxAreaFillMemory;

extern int32 GeometryDialogInitialX, GeometryDialogInitialY, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY;

extern int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, MiddleButtonPressed,
       RightButtonPressed, UnselectAll, LeftButtonDoublePressed, DataBaseChanged, MovingComponents, MouseCursorOnGrid,
       GridVisible, DrawPowerPads, ReDrawing, DrawTwoTryingTraces, FileChanged, SpacePressed, HelpAsked,
       RecalcAreafillAfterInsert, BackGroundActive, SelectionEsc, OkToSwitchDrawingLayers, OkToAddViewPos,
       ParametersRelative, ActiveSchematicSelect, OkToUseSharedMemory, DrawClearanceForTryingTrace, ReplaceSelections,
       AddExtraTraceMode, DrawViaInfoForTryingTrace, UndoRedoActive, FirstPaint, LinesAllDirection, DrawTraceUsingGuide,
       DrawTraceLimitInfo, OkToDrawAreaFills, OkToDrawAreaFillWithHatches, OkToDrawClearances, OkToDrawCompOutline,
       OkToDrawConnections, OkToDrawInnerPads, OkToDrawTopPads, OkToDrawBottomPads, OkToDrawCompPlacement,
       OkToDrawPowerPlanes, OkToDrawSilkScreenBottom, OkToDrawSilkScreenTop, OkToDrawObjects, OkToDrawInfoObjects,
       OkToDrawInfo2Objects, OkToDrawInfo3Objects, OkToDrawInfo4Objects, OkToDrawBoardOutline, OkToDrawVias,
       OkToDrawViaClearances, OkToDrawErrors, OkToDrawWarnings, OkToDrawRoutingKeepoutTop, OkToDrawRoutingKeepoutBottom,
       OkToDrawRoutingKeepoutInner, OkToRePaintAfterCompMove, OkToRePaintAfterTraceDrawing,
       DoNotShowAreafillThinLinesError, DrawTopComponents, DrawBottomComponents, OkToDrawCrossHair, OkToDrawCompReference,
       OkToDrawCompValue;

extern int32 DrawSoldMaskBottomMode, DrawSoldMaskTopMode, DrawPasteMaskBottomMode, DrawPasteMaskTopMode,
       ViewSingleLayer;

extern double Xoffset, Yoffset, Factor, RelX, RelY, CurrentTraceWidth, CurrentClearance, TraceWidthUser[10],
       ClearanceWidthUser[10];

#ifdef GCC_COMP
extern float ParamsFloat[256];
#else
extern float ParamsFloat[256];
#endif

extern int32 FirstSize, NrObjects, NrObjects2, NrObjects3, NrObjects4, NrObjects5, NrObjects6, NrErrorObjects,
       NrGridSizes, NrTraceWidths, NrClearanceWidths, CurrentErrorNr, ComponentConnectionMode, MouseOnGridPosX,
       MouseOnGridPosY, CurrentGuideNr, Units, SnapMode, CompSnapMode, ViewPosPointer, LastTracePosPointer,
       FoundProjectPath, FoundDesignPath, CurrentNet, PCBSystemError;
extern int32 CurrentObjectCode;
extern int32 *IndexVerTraces[32], *IndexHorTraces[32], *IndexDiag1Traces[32], *IndexDiag2Traces[32], *IndexVias,
       *IndexConnections;

extern int32 NrIndexesVerTraces[32], NrIndexesHorTraces[32], NrIndexesDiag1Traces[32], NrIndexesDiag2Traces[32],
       NrNetItems, MaxNrNetItems, NrNetItems2, MaxNrNetItems2, NrIndexesVias, NrIndexesConnections, NrGerberLayers,
       GerberLayers[64], NrFoundObjects1, NrFoundObjects2, NrGeometryLibraries;

extern int64 SpecialCounterProgramStart;
extern SYSTEMTIME CurrentDateProgramStart;

extern int32 CircleConv[16], CircleMirrorX[16], CircleMirrorY[16];
extern int32 CircleRotate90[16], CircleRotate180[16], CircleRotate270[16];
extern uint8 *SharedMemory;
extern HANDLE *SharedMemoryHandle;
extern uint8 *ClipBoardMem, *TempMem, *TempMem2, *TempMem3;

extern int Debugfp, OperatingSystem;

//extern ActionArray          NrObjectsPerActionHorTraces[32],
//                            NrObjectsPerActionVerTraces[32],
//                            NrObjectsPerActionDiag1Traces[32],
//                            NrObjectsPerActionDiag2Traces[32],
//                            NrObjectsPerActionConnections,
//                            NrObjectsPerActionVias;

extern ViewPosArray ViewPos;

extern ObjectRecord DrawTrace1, DrawTrace2, LimitObject, TraceObject1, TraceObject2, CurrentWorkingTrace, NullObject,
       ConnectionObject1, ConnectionObject2, OverlapObject, LastAddedObject, LayerPlotObjects[33],
       FoundObjects1[MaxFoundObjects], FoundObjects2[MaxFoundObjects];

extern ValueRecord NewValue;

extern ViaRecord CurrentVia;

extern double DisplX, DisplY;

extern GerberInfoRecord GerberInfo;
extern PDFInfoRecord PDFInfo;

extern char AbsPosStr[MAX_LENGTH_STRING];
extern char RelPosStr[MAX_LENGTH_STRING];
extern char AbsGridPosStr[MAX_LENGTH_STRING];
extern char InfoStr[MAX_LENGTH_STRING];
extern char DesignPath[MAX_LENGTH_STRING];
extern char ExePath[MAX_LENGTH_STRING];
extern char ExportDir[MAX_LENGTH_STRING];
extern char EditFile[MAX_LENGTH_STRING];
extern char StartDir[MAX_LENGTH_STRING];
extern char ProjectPath[MAX_LENGTH_STRING];
extern char AperTureFile[MAX_LENGTH_STRING];
extern char Params[8][MAX_LENGTH_STRING];
extern char DesignFile[MAX_LENGTH_STRING];
extern char GeometryLibraries[16][MAX_LENGTH_STRING];

extern WCHAR StrW16[512], TextStrings[64][128];
extern char TextStrings2[64][128];

extern int32 LastUsedDrawingLayers[2];

extern TracesArray *HorTraces[32], *VerTraces[32], *Diag1Traces[32], *Diag2Traces[32];

extern ObjectArray *Objects, *Objects2, *Objects3, *Objects4, *Objects5, *Objects6, *ErrorObjects;
extern ViaArray *Vias;
extern NetArray *Nets;
extern ObjectLinesArray *ObjectLines;
extern ObjectRectsArray *ObjectRects;
extern ObjectCirclesArray *ObjectCircles;
extern ObjectArcsArray *ObjectArcs;
extern ObjectTextsArray *ObjectTexts;
extern ObjectTextsArray2 *ObjectTexts2;
extern ConnectionsArray *Connections;
extern NetConnectionsArray *NetConnections;
extern uint8 *ShapesMem, *CompsMem;
extern ShapesArray *Shapes;
extern CompsArray *Comps;
extern CharsArray *Chars;
extern AreaFillsArray *AreaFills;
extern uint8 *AreaFillMem;
extern uint8 *AreaFillMemTemp;
extern uint8 *AreaFillMemTemp2;
extern char *MessageBuf;
extern PolygonRecord *BufferPolygon, *ResultPolygon, *ExtraPolygon, *NewPolygon, *WorkPolygon[MAX_WORK_POLGONS];

extern ObjectPolygonsArray *ObjectPolygons;
extern uint8 *ObjectPolygonMem;
extern ObjectPolygonRecord NewObjectPolygon;

extern ObjectTextRecord2 NewObjectText2;
extern ObjectLineRecord NewObjectLine;
extern ObjectRectRecord NewObjectRect;
extern ObjectCircleRecord NewObjectCircle;
extern ObjectArcRecord NewObjectArc;
extern ObjectTextRecord NewObjectText;
extern NetItemsArray *NetItems, *NetItems2;
extern NetRecord EmptyNet;

//extern DeleteInfoArray      DeleteInfo;

extern AreaFillRecord *NewAreaFill, *TempAreaFill;

extern HGLOBAL GlobalClipBoardMem;
extern uint8 *ClipBoardMem;

extern DesignRecord Design, NewDesign;

// ********************************************************************************************************
// ********************************************************************************************************

extern int32 NrPcbNames, PcbNamesPos;
extern LPSTR PcbNamesId[MaxNrPcbNames];

extern char PcbNamesBuf[PcbNamesBufSize];


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

LPSTR StringConvert(int32 Nr, LPSTR str);

int32 GetUsedMemSize(int32 mode, LPSTR MemoryTextBuf);

void DeAllocateMemDesign(void);

int32 AllocateMem(void);

void DeAllocateMem(void);

void MemoryMain(void);

void DeAllocateMemObjects4(void);

void DeAllocateMemObjects5(void);

void DeAllocateMemObjects6(void);

int32 AllocateMemHorTraces(int32 Layer, int32 count);

int32 AllocateMemVerTraces(int32 Layer, int32 count);

int32 AllocateMemDiag1Traces(int32 Layer, int32 count);

int32 AllocateMemDiag2Traces(int32 Layer, int32 count);

int32 AllocateMemVias(int32 count);

int32 AllocateMemObjectLines(int32 count);

int32 AllocateMemObjectRects(int32 count);

int32 AllocateMemObjectCircles(int32 count);

int32 AllocateMemObjectArcs(int32 count);

int32 AllocateMemObjectTexts(int32 count);

int32 AllocateMemObjects(int32 count);

int32 AllocateMemObjects2(int32 count);

int32 AllocateMemObjects3(int32 count);

int32 AllocateMemObjects4(int32 count);

int32 AllocateMemObjects5(int32 count);

int32 AllocateMemObjects6(int32 count);

int32 AllocateMemErrorObjects(int32 count);

int32 AllocateMemComps(int32 count);

int32 AllocateMemCompsMemory(int32 count);

int32 AllocateMemAreaFills(int32 count);

int32 AllocateMemAreaFillMemoryTemp(int32 count);

int32 AllocateMemAreaFillMemory(int32 count);

int32 AllocateMemShapes(int32 count);

int32 AllocateMemShapesMemory(int32 count);

int32 AllocateMemConnections(int32 count);

int32 AllocateMemNetConnections(int32 count);

int32 AllocateMemNets(int32 count);

int32 AllocateMemPolygons(int32 NrVerticesPolygon);

void DeAllocateMemPolygons(void);

void DeAllocateMemAreaFills(void);

int32 AllocateMemMessageBuf(int32 MemSize);

void DeAllocateMemMessageBuf(void);

int32 AddToMessageBuf(LPSTR Line);

int32 AllocateMemConnectionsPos(int32 count);

int32 AllocateMemClipBoard(int32 MemSize);

int32 AllocateMemTemp(int32 MemSize);

int32 AllocateMemTemp2(int32 MemSize);

int32 AllocateMemTemp3(int32 MemSize);

int32 AllocateSpecialMem(int32 Index, int32 MemSize, void **MemP);

int32 DeallocateSpecialMem(int32 Index);

void DeAllocateMemTemp(void);

void DeAllocateMemTemp2(void);

void DeAllocateMemTemp3(void);

void DeAllocateMemHorTraces(int32 Layer);

void DeAllocateMemVerTraces(int32 Layer);

void DeAllocateMemDiag1Traces(int32 Layer);

void DeAllocateMemDiag2Traces(int32 Layer);

int32 AllocateMemNetItems(int32 count);

int32 AllocateMemNetItems2(int32 count);

void DeAllocateMemNetItems(void);

int32 AllocateMemObjectTexts2(int32 count);

int32 AllocateMemObjectPolygonMemory(int32 count);

int32 AllocateMemObjectPolygons(int32 count);

int32 CopyStrToClipboardOwn(HWND Window, LPSTR StrToCopy);

void SaveMemory(void);

#endif
