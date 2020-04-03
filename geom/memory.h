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

#define MaxNrGeomNames                             1500
#define MaxNrGeomNamesDialog                       400
#define GeomNamesDialogBufSize                     8192
#define GeomNamesBufSize                           32768



extern int32 ClientWindowDivX, ClientWindowDivY;

extern double SearchMinX, SearchMinY, SearchMaxX, SearchMaxY, VisibleMinX, VisibleMinY, VisibleMaxX, VisibleMaxY,
       SelectedMinX, SelectedMinY, SelectedMaxX, SelectedMaxY, CurrentClearance, TraceThickness, CurrentSilkscreenLine,
       CurrentCompOutLine, TraceWidths[32], TraceClearances[32], CurrentBoardOutLine, CurrentTextHeight, CurrentInfoLine,
       GridSizes[32], SilkscreenLines[32], CompOutLines[32], BoardOutLines[32], InfoLines[32];

extern int32 WindowWidth, WindowHeight, MousePosX, MousePosY, MaxScrollBarX, MaxScrollBarY, WindowStartX, WindowStartY,
       ScrollSize, ScrollSizeDrawing, ScrollEndOfWindow, SelectionMode, LastActionNr, DrawDrillMode, MaxLastActionNr,
       ZoomMode, ZoomInOutProcessed, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2;

extern int32 MaxNrObjects, NrObjects, MaxNrObjects2, NrObjects2, CrossHairType, SystemBusyMode, SnapMode,
       MaxNrPinObjects, NrPinObjects, NrPadLayers, MaxNrVerticesPolygon, CrossHairVisible, NrTraceWidths,
       NrTraceClearances, MessageBufMemSize, NrSilkscreenLines, NrCompOutLines, NrGridSizes, NrBoardOutLines, NrInfoLines,
       NrObjectPolygons, MaxNrObjectPolygons, ObjectPolygonMemorySize, MaxObjectPolygonMemory, NrMappableObjectPolygons,
       MappableObjectPolygonStart, MessageBufPos, NrPinObjectsInfo, MaxShapesMem, NrPadLayerObjectsSelected[32],
       NrDrillsSelected, NrDrillsUnplatedSelected, NrAntiPowerpadsSelected, NrPadsInnerSelected, NrSilkTopObjectsSelected,
       NrSilkBottomObjectsSelected, NrCompOutlinesSelected, NrPlacemOutlinesSelected, NrMaskTopObjectsSelected,
       NrMaskBottomObjectsSelected, NrPasteTopObjectsSelected, NrPasteBottomObjectsSelected, NrLinesSelected,
       NrRectsSelected, NrCirclesSelected, NrArcsSelected, NrTextsSelected, NrPolygonsSelected, GeomNameSelected,
       NrBoardOutlinesSelected, NrInfo1ObjectsSelected, NrInfo2ObjectsSelected, NrInfo3ObjectsSelected,
       NrInfo4ObjectsSelected, NrRoutingKeepoutsSelected[32];


extern int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, MiddleButtonPressed,
       RightButtonPressed, UnselectAll, LeftButtonDoublePressed, DataBaseChanged, FileChanged, MouseCursorOnGrid,
       FinishPolygon, GridVisible, FirstPaint, UndoRedoActive, BackGroundActive, SelectionActive, SelectionEsc,
       OkToAddViewPos, PadsVisible[32], RoutingKeepoutVisible[32], PastePadsTopVisible, PastePadsBottomVisible,
       SoldMaskPadsTopVisible, SoldMaskPadsBottomVisible, PlacementVisible, CompOutlineVisible, PowerPadsVisible,
       PinNamesVisible, InnerPadsVisible, SilkScreenTopVisible, SilkScreenBottomVisible, BoardOutlineVisible, Info1Visible,
       Info2Visible, Info3Visible, Info4Visible, ClearanceVisible, DrillVisible, DrillUnplatedVisible, ViewInsertionPoint,
       GeomNameVisible, MousePanMultiply, SpacePressed, ParametersRelative, OkToUseSharedMemory, FoundDesignPath,
       FoundProjectPath, ReplaceSelections;

extern int32 ReverseY;
extern int32 HelpAsked;

extern double Xoffset, Yoffset, Factor, RelX, RelY, GridSize, BoardWidth, BoardHeight, BoardOX, BoardOY, TextMinX,
       TextMinY, TextMaxX, TextMaxY, CenterMoveX, CenterMoveY, DimensionHeight, ArrowLength;
extern float ParamsFloat[32];

extern int32 FirstSize, NrObjects, CurrentObjectCode, Units, CurrentFontCode, ViewPosPointer, RepeatMode, LastAction,
       GEOMSystemError, ClipBoardMemSize, ClipBoardMemPos, MaxAreaFillMemoryTemp, MaxAreaFillMemoryTemp3, MaxTempMemory,
       MaxTempMemory2, ParamsInt[8], RepeatModeBusy;


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


extern uint8 *SharedMemory, *AreaFillMemTemp, *AreaFillMemTemp2, *ClipBoardMem;
extern HANDLE *SharedMemoryHandle;

extern int Debugfp, OperatingSystem;
extern uint32 ClosingWindowMessage;

extern ViewPosArray ViewPos;

extern ObjectRecord NewObject;

extern double DisplX, DisplY;

extern char AbsPosStr[MAX_LENGTH_STRING];
extern char RelPosStr[MAX_LENGTH_STRING];
extern char AbsGridPosStr[MAX_LENGTH_STRING];
extern char InfoStr[MAX_LENGTH_STRING];
extern char DesignPath[MAX_LENGTH_STRING];
extern char ExePath[MAX_LENGTH_STRING];
extern char EditFile[MAX_LENGTH_STRING];
extern CharsArray *Chars;
extern uint8 *ShapesMem;
extern HGLOBAL GlobalClipBoardMem;

extern ObjectArray *Objects, *Objects2;
extern ObjectPolygonsArray *ObjectPolygons;
extern uint8 *ObjectPolygonMem, *TempMem, *TempMem2;
extern ObjectPolygonRecord *NewObjectPolygon;

extern PinInfoArray *PinInfos;
extern AreaFillRecord *NewAreaFill, *TempAreaFill;
extern PolygonRecord *BufferPolygon, *ResultPolygon, *ExtraPolygon, *NewPolygon, PolygonBuf, *WorkPolygon[8];

extern GeomCreateRecord BGAGeom, PGAGeom, SOICGeom, DILGeom, SILGeom, SIL_SMD_RECTGeom, SIL_SMD_CIRCLEGeom, QUADGeom;

extern uint8 Buf[];
extern uint8 *MessageBuf;

extern DesignRecord Design;

extern ShapeRecord Shape;

// ********************************************************************************************************
// ********************************************************************************************************

extern int32 NrGeomNames, GeomNamesPos;
extern LPSTR GeomNamesId[MaxNrGeomNames];

extern char GeomNamesBuf[GeomNamesBufSize];

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

LPSTR StringConvert(int32 Nr, LPSTR str);

void AllocateMem(void);

void DeAllocateMem(void);

void DeAllocateMemGeometrie(void);

int32 DeAllocateMemShapes(void);

int32 AllocateMemObjects(int32 count);

int32 AllocateMemObjects2(int32 count);

int32 AllocateMemPinObjects(int32 count);

int32 AllocateMemShapes(int32 ShapesMemSize);

int32 AllocateMemMessageBuf(int32 MemSize);

int32 AllocateMemObjectPolygons(int32 count);

int32 AllocateMemObjectPolygonMemory(int32 count);

int32 AllocateMemPolygons(int32 NrVerticesPolygon);

int32 AllocateMemTemp(int32 MemSize);

int32 AllocateMemAreaFillMemoryTemp(int32 count);

int32 AllocateSpecialMem(int32 Index, int32 MemSize, void **MemP);

int32 DeAllocateMemShapes(void);

void DeAllocateMemPolygons(void);

void DeAllocateMemAreaFills(void);

void DeAllocateMemAreaFillsTemp(void);

void DeAllocateMemMessageBuf(void);

void DeAllocateMemTemp(void);

void MemoryMain(void);

#endif
