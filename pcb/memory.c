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
#include "files2.h"
#include "string.h"
#include "calc.h"
#include "gateswap.h"
#include "graphics.h"
#include "memory.h"
#include "stdio.h"


#define LimitMaxNrTraces                          131072
#define LimitMaxNrVias                            65536
#define LimitMaxNrConnections                     32768
#define LimitMaxNrNetConnections                  16384
#define LimitMaxNrNets                            65536
#define LimitMaxNrObjects                         32768
#define LimitMaxNrObjects2                        32768
#define LimitMaxNrExtObjects                      8192
#define LimitMaxNrComps                           16384
#define LimitMaxNrShapes                          1024
#define LimitMaxNrNetTests                        4096
#define LimitMaxNrNetTests2                       4096
#define LimitMaxNrAreaFills                       4096
#define LimitMaxNrViasPos                         16384
#define LimitMaxNrConnectionsPos                  16384

#define LimitCompsMemory                          16384*1024
#define LimitShapesMemory                         16384*1024
#define LimitMaxAreaFillMemory                    32768*1024

#define LimitMaxNrObjectLines                     2048*1024
#define LimitMaxNrObjectCircles                   1024*1024
#define LimitMaxNrObjectRects                     1024*1024
#define LimitMaxNrObjectArcs                      1024*1024
#define LimitMaxNrObjectTexts                     256*1024
#define LimitMaxNrObjectTexts2                    256*1024

#define LimitMaxObjectPolygonMemory               4096*1024
#define LimitMaxNrObjectPolygons                  4096

#define MAX_SPECIAL_MEM                           64

int32 ClientWindowDivX, ClientWindowDivY, NrParams, RepeatMode, RepeatModeActive =
    1, LastAction, CurrentDirection, PreviousDirection, PopupDisplayVisible =
        1, StartWithMaximumView, SaveSymbolsLocally, MoveCompAutoZoom, ZoomMode, MousePanMultiply, ButtonInfoTimeout =
            40, ButtonInfoTimeoutStart =
                10, CurrentGuideNr, AreafillDrawMode, SystemBusyMode, DrawDrillMode, ZoomInOutProcessed, FastPaint,
                DrawLayerCode[32], DrawCode, RepeatModeBusy;

double SearchMinX, SearchMinY, SearchMaxX, SearchMaxY, CurrentDrawX1, CurrentDrawY1, CurrentDrawX2, CurrentDrawY2,
       ShiftOffsetX, ShiftOffsetY, VisibleMaxX, VisibleMinX, VisibleMaxY, VisibleMinY, CurrentX2, CurrentY2, ViewOffsetX,
       ViewOffsetY, ViewScale, SearchX1, SearchY1, SearchX2, SearchY2, ConnectionX1, ConnectionY1, ConnectionX2,
       ConnectionY2, StartFirstDrawingTraceX, StartFirstDrawingTraceY, StartSecondDrawingTraceX, StartSecondDrawingTraceY,
       EndSecondDrawingTraceX, EndSecondDrawingTraceY, EndFirstDrawingTraceX, EndFirstDrawingTraceY, TraceWidths[30],
       ClearanceWidths[30], MinDrillForThermalRelief, StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX,
       EndPreviousTraceY, DrawingConnectionX1, DrawingConnectionY1, DrawingConnectionX2, DrawingConnectionY2;

#ifdef GCC_COMP
float ParamsFloat[256];
#else
float ParamsFloat[256];
#endif

double GridSizes[30], GridSize, UserGridSize, CompGridSize, TraceGridSize, AreafillGridSize;

int32 WindowWidth, WindowHeight, MousePosX, MousePosY, WindowStartX, WindowStartY, MaxScrollBarX, MaxScrollBarY,
      ScrollSize, ScrollSizeDrawing, SelectionMode, LastActionNr, ScrollEndOfWindow, CurrentDrawingLayer, MaxLastActionNr,
      CurrentDrawingNetNr, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2;

int32 NrGraphicsLayers, GraphicsLayers[256];


int32 ViasSelected, CompsSelected, ConnectionsSelected, ObjectLinesSelected, ObjectRectsSelected,
      ObjectRectsFilledSelected, ObjectCirclesSelected, ObjectCirclesFilledSelected, ObjectArcsSelected,
      ObjectTextsSelected, ObjectPolygonsSelected, AllTracesSelected, TracesSelected[32], HorTracesSelected[32],
      VerTracesSelected[32], Diag1TracesSelected[32], Diag2TracesSelected[32], TraceLayersSelectable[32],
      MaxNrHorTraces[32], MaxNrVerTraces[32], MaxNrDiag1Traces[32], MaxNrDiag2Traces[32], MaxNrNetsPos[32], MaxNrVias,
      MaxNrViasPos, MaxNrConnections, MaxNrConnectionsPos, MaxNrNetConnections, MaxNrNets, MaxNrObjects, MaxNrObjects2,
      MaxNrObjects3, MaxNrObjects4, MaxNrObjects5, MaxNrObjects6, MaxNrErrorObjects, MaxNrComps, MaxNrShapes,
      MaxNrNetTests, MaxNrNetTests2, MaxNrAreaFills, MaxNrObjectLines, MaxNrObjectRects, MaxNrObjectCircles,
      MaxNrObjectArcs, MaxNrObjectTexts, MaxNrObjectTexts2, MaxNrObjectPolygons, MaxObjectPolygonMemory,
      MaxNrPolygon8Objects, MaxNrVerticesPolygon, MaxCompsMemory, MaxTempMemory, TempMemorySize, MaxTempMemory2,
      TempMemorySize2, MaxTempMemory3, TempMemorySize3, MaxSpecialMemory[97], MaxLayerPlotObjects[33],
      NrLayerPlotObjects[33], ClipBoardMemSize, ClipBoardMemPos, MaxShapesMemory, MessageBufMemSize, MessageBufPos,
      MaxAreaFillMemoryTemp, MaxAreaFillMemory;


int32 GeometryDialogInitialX, GeometryDialogInitialY, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY;

int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, MiddleButtonPressed, RightButtonPressed,
      UnselectAll, LeftButtonDoublePressed, DataBaseChanged, MovingComponents, MouseCursorOnGrid, GridVisible,
      BackGroundActive, DrawTwoTryingTraces, FileChanged, SpacePressed, HelpAsked, RecalcAreafillAfterInsert, ReDrawing,
      DrawTraceUsingGuide, SelectionEsc, OkToSwitchDrawingLayers, OkToAddViewPos, ParametersRelative,
      ActiveSchematicSelect, ReplaceSelections, AddExtraTraceMode, OkToUseSharedMemory, DrawClearanceForTryingTrace,
      DrawViaInfoForTryingTrace, UndoRedoActive, FirstPaint, LinesAllDirection, DrawTraceLimitInfo, OkToDrawAreaFills,
      OkToDrawAreaFillWithHatches, OkToDrawClearances, OkToDrawCompOutline, OkToDrawConnections, OkToDrawInnerPads,
      OkToDrawTopPads, OkToDrawBottomPads, OkToDrawCompPlacement, OkToDrawPowerPlanes, OkToDrawSilkScreenBottom,
      OkToDrawSilkScreenTop, OkToDrawObjects, OkToDrawInfoObjects, OkToDrawInfo2Objects, OkToDrawInfo3Objects,
      OkToDrawInfo4Objects, OkToDrawBoardOutline, OkToDrawRoutingKeepoutTop, OkToDrawRoutingKeepoutBottom,
      OkToDrawRoutingKeepoutInner, OkToDrawVias, OkToDrawViaClearances, OkToDrawWarnings, OkToDrawErrors,
      OkToRePaintAfterCompMove, OkToRePaintAfterTraceDrawing, DoNotShowAreafillThinLinesError, DrawTopComponents =
          1, DrawBottomComponents = 1, OkToDrawCrossHair, OkToDrawCompReference, OkToDrawCompValue;

int32 DrawSoldMaskBottomMode, DrawSoldMaskTopMode, DrawPasteMaskBottomMode, DrawPasteMaskTopMode, ViewSingleLayer;

int64 SpecialCounterProgramStart;
SYSTEMTIME CurrentDateProgramStart;

ObjectRecord LimitObject, TraceObject1, TraceObject2, DrawTrace1, DrawTrace2, CurrentWorkingTrace, NullObject,
             ConnectionObject1, ConnectionObject2, OverlapObject, LastAddedObject, LayerPlotObjects[33],
             FoundObjects1[MaxFoundObjects], FoundObjects2[MaxFoundObjects];
ValueRecord NewValue;
ViaRecord CurrentVia;

double Xoffset, Yoffset, Factor, RelX, RelY, CurrentTraceWidth, CurrentClearance, TextMinX, TextMinY, TextMaxX,
       TextMaxY, TraceWidthUser[10], ClearanceWidthUser[10];

int32 FirstSize, NrObjects, NrObjects2, NrObjects3, NrObjects4, NrObjects5, NrObjects6, NrErrorObjects, CurrentErrorNr,
      ComponentConnectionMode, MaxNrSelectedObjectsIndex, NrGridSizes, NrTraceWidths, NrClearanceWidths, MouseOnGridPosX,
      MouseOnGridPosY, CurrentGuideNr, Units, SnapMode, CompSnapMode, ViewPosPointer, LastTracePosPointer,
      FoundProjectPath, FoundDesignPath, CurrentNet, PCBSystemError;

int32 CurrentObjectCode;

int32 *IndexVerTraces[32], *IndexHorTraces[32], *IndexDiag1Traces[32], *IndexDiag2Traces[32], *IndexVias,
      *IndexConnections, NrNetItems, MaxNrNetItems, NrNetItems2, MaxNrNetItems2, NrIndexesVerTraces[32],
      NrIndexesHorTraces[32], NrIndexesDiag1Traces[32], NrIndexesDiag2Traces[32], NrIndexesVias, NrIndexesConnections,
      NrGerberLayers, GerberLayers[64], NrFoundObjects1, NrFoundObjects2, NrGeometryLibraries;

int Debugfp, OperatingSystem;


ViewPosArray ViewPos;

double DisplX, DisplY;

uint8 *SharedMemory;
HGLOBAL GlobalClipBoardMem;
uint8 *ClipBoardMem;
HANDLE *SharedMemoryHandle;

char AbsPosStr[MAX_LENGTH_STRING];
char RelPosStr[MAX_LENGTH_STRING];
char AbsGridPosStr[MAX_LENGTH_STRING];
char InfoStr[MAX_LENGTH_STRING];
char DesignPath[MAX_LENGTH_STRING];
char ExePath[MAX_LENGTH_STRING];
char StartDir[MAX_LENGTH_STRING];
char EditFile[MAX_LENGTH_STRING];
char ExportDir[MAX_LENGTH_STRING];
char ProjectPath[MAX_LENGTH_STRING];
char AperTureFile[MAX_LENGTH_STRING];
char DesignFile[MAX_LENGTH_STRING];
char Params[8][MAX_LENGTH_STRING];
char *MessageBuf;
char GeometryLibraries[16][MAX_LENGTH_STRING];
NetRecord EmptyNet;

GerberInfoRecord GerberInfo;
PDFInfoRecord PDFInfo;

int32 LastUsedDrawingLayers[2], *SelectedObjectsIndex;

AreaFillRecord *NewAreaFill, *TempAreaFill;

TracesArray *HorTraces[32], *VerTraces[32], *Diag1Traces[32], *Diag2Traces[32];
int32 DebugTest1;
ObjectArray *Objects, *Objects2, *Objects3, *Objects4, *Objects5, *Objects6, *ErrorObjects;
ViaArray *Vias;
NetArray *Nets;
ConnectionsArray *Connections;
NetConnectionsArray *NetConnections;
ShapesArray *Shapes;
CompsArray *Comps;
AreaFillsArray *AreaFills;
CharsArray *Chars;

ObjectLinesArray *ObjectLines;
ObjectRectsArray *ObjectRects;
ObjectCirclesArray *ObjectCircles;
ObjectArcsArray *ObjectArcs;
ObjectTextsArray *ObjectTexts;

ObjectLineRecord NewObjectLine;
ObjectRectRecord NewObjectRect;
ObjectCircleRecord NewObjectCircle;
ObjectArcRecord NewObjectArc;
ObjectTextRecord NewObjectText;

ObjectPolygonsArray *ObjectPolygons;
uint8 *ObjectPolygonMem;
ObjectPolygonRecord NewObjectPolygon;
ObjectTextsArray2 *ObjectTexts2;
ObjectTextRecord2 NewObjectText2;

NetItemsArray *NetItems, *NetItems2;

AreaFillsArray *AreaFills;
PolygonRecord *BufferPolygon, *ResultPolygon, *ExtraPolygon, *NewPolygon, *WorkPolygon[MAX_WORK_POLGONS];

WCHAR StrW16[512], TextStrings[64][128];
char TextStrings2[64][128];
uint8 *ShapesMem, *CompsMem, *AreaFillMem, *TempMem, *TempMem2, *TempMem3, *AreaFillMemTemp, *AreaFillMemTemp2,
      *SpecialMem[MAX_SPECIAL_MEM];
HGLOBAL HorTracesGlobal[32], VerTracesGlobal[32], Diag1TracesGlobal[32], Diag2TracesGlobal[32], ObjectsGlobal,
        Objects2Global, Objects3Global, Objects4Global, Objects5Global, Objects6Global, ErrorObjectsGlobal,
        SelectedObjectsIndexGlobal, ViasGlobal, NetsGlobal, NetsPosGlobal[32], ViasPosGlobal, ConnectionsPosGlobal,
        ConnectionsGlobal, NetConnectionsGlobal, NetTestGlobal, NetTest2Global, CompsGlobal, ShapesGlobal, ShapesMemGlobal,
        CompsMemGlobal, ObjectTextsGlobal2, ObjectPolygonMemGlobal, ObjectPolygonsGlobal, AreaFillMemGlobal,
        AreaFillMemoryTempGlobal, AreaFillMemoryTemp2Global, AreaFillMemoryTemp3Global, AreaFillsGlobal,
        BufferPolygonGlobal, ResultPolygonGlobal, ExtraPolygonGlobal, NewPolygonGlobal, MessageBufGlobal, GlobalTempMem,
        GlobalTempMem2, GlobalTempMem3, GlobalSpecialMem[MAX_SPECIAL_MEM], WorkPolygonGlobal[MAX_WORK_POLGONS],
        ObjectLinesGlobal, ObjectRectsGlobal, ObjectCirclesGlobal, ObjectArcsGlobal, ObjectTextsGlobal, NetItemsGlobal,
        NetItems2Global, CharsGlobal;

int32 CircleConv[16] = { 0, 3, 12, 15, 0x30, 0, 0x3c, 0, 0xc0, 0xc3, 0, 0, 0xf0, 0, 0, 0xff };

int32 CircleMirrorX[16] = { 0, 8, 4, 12, 2, 0, 6, 0, 1, 9, 0, 0, 3, 0, 0, 15 };
int32 CircleMirrorY[16] = { 0, 2, 1, 3, 8, 0, 9, 0, 4, 6, 0, 0, 12, 0, 0, 15 };
int32 CircleRotate90[16] = { 0, 8, 1, 9, 2, 0, 3, 0, 4, 12, 0, 0, 6, 0, 0, 15 };
int32 CircleRotate180[16] = { 0, 4, 8, 12, 1, 0, 9, 0, 2, 6, 0, 0, 3, 0, 0, 15 };
int32 CircleRotate270[16] = { 0, 2, 4, 6, 8, 0, 12, 0, 1, 3, 0, 0, 9, 0, 0, 15 };

DesignRecord Design, NewDesign;

#ifdef KEY
extern uint32 TimerTicks1, TimerTickDiv;
#endif

extern HWND PCBWindow;

// ********************************************************************************************************
// ********************************************************************************************************

int32 NrPcbNames, PcbNamesPos;
LPSTR PcbNamesId[MaxNrPcbNames];


char PcbNamesBuf[PcbNamesBufSize];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


LPSTR StringConvert(int32 Nr, LPSTR str)
{
	int32 StringCount1, StringCount2, ParamCount1, ParamCount2, cnt, Length;
	char str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	if (!str)
		return "";

	if ((Nr < 0) || (Nr >= MaxNrPcbNames) || (PcbNamesId[Nr] == 0))
		return str;

	strcpy(str1, str);
	strcpy(str2, PcbNamesId[Nr]);

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
		    MessageBoxOwn(NULL,str3,"System error",MB_APPLMODAL|MB_OK);
		*/
		return str;
	}

	return PcbNamesId[Nr];
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetUsedMemSize(int32 mode, LPSTR MemoryTextBuf)
{
	char str[MAX_LENGTH_STRING];
	int32 cnt;
	int32 MemSize = 0;

	int32 ObjectsGlobalMemSize = 0;
	int32 Objects2GlobalMemSize = 0;
	int32 Objects3GlobalMemSize = 0;
	int32 Objects4GlobalMemSize = 0;
	int32 Objects5GlobalMemSize = 0;
	int32 Objects6GlobalMemSize = 0;
	int32 ErrorObjectsGlobalMemSize = 0;
	int32 SelectedObjectsIndexGlobalMemSize = 0;
	int32 ViasGlobalMemSize = 0;
	int32 NetsGlobalMemSize = 0;
	int32 ViasPosGlobalMemSize = 0;
	int32 ConnectionsPosGlobalMemSize = 0;
	int32 ConnectionsGlobalMemSize = 0;
	int32 NetConnectionsGlobalMemSize = 0;
	int32 NetTestGlobalMemSize = 0;
	int32 NetTest2GlobalMemSize = 0;
	int32 CompsGlobalMemSize = 0;
	int32 ShapesGlobalMemSize = 0;
	int32 ShapesMemGlobalMemSize = 0;
	int32 CompsMemGlobalMemSize = 0;
	int32 AreaFillMemGlobalMemSize = 0;
	int32 AreaFillMemoryTempGlobalMemSize = 0;
	int32 AreaFillMemoryTemp2GlobalMemSize = 0;
	int32 AreaFillsGlobalMemSize = 0;
	int32 BufferPolygonGlobalMemSize = 0;
	int32 ResultPolygonGlobalMemSize = 0;
	int32 ExtraPolygonGlobalMemSize = 0;
	int32 NewPolygonGlobalMemSize = 0;
	int32 MessageBufGlobalMemSize = 0;
	int32 GlobalTempMemMemSize = 0;
	int32 GlobalTempMem2MemSize = 0;
	int32 ObjectLinesGlobalMemSize = 0;
	int32 ObjectRectsGlobalMemSize = 0;
	int32 ObjectCirclesGlobalMemSize = 0;
	int32 ObjectArcsGlobalMemSize = 0;
	int32 ObjectTextsGlobalMemSize = 0;
	int32 ObjectTextsGlobal2MemSize = 0;
	int32 ObjectPolygonMemGlobalMemSize = 0;
	int32 ObjectPolygonsGlobalMemSize = 0;
	int32 CharsGlobalMemSize = 0;
	int32 NetItemsGlobalMemSize = 0;
	int32 NetItems2GlobalMemSize = 0;
	int32 HorTracesGlobalMemSize[32];
	int32 VerTracesGlobalMemSize[32];
	int32 Diag1TracesGlobalMemSize[32];
	int32 Diag2TracesGlobalMemSize[32];
	int32 NetsPosGlobalMemSize[32];
	int32 WorkPolygonGlobalMemSize[MAX_WORK_POLGONS];

	int32 DesignSaveMemGlobalMemSize = 0;
	int32 ObjectTextBufGlobalMemSize = 0;
	int32 GatePinSwapGlobalMemSize = 0;
	int32 AperTuresGlobalMemSize = 0;
	int32 DrillAperTuresGlobalMemSize = 0;
	int32 LoadedAperTuresGlobalMemSize = 0;
	int32 Nets2GlobalMemSize = 0;
	int32 ActionNrGlobalMemSize = 0;
	int32 hClipMemoryMemSize = 0;
	int32 NewGlobalClipBoardMemMemSize = 0;

	for (cnt = 0; cnt < 32; cnt++)
	{
		HorTracesGlobalMemSize[cnt] = 0;
		VerTracesGlobalMemSize[cnt] = 0;
		Diag1TracesGlobalMemSize[cnt] = 0;
		Diag2TracesGlobalMemSize[cnt] = 0;
		NetsPosGlobalMemSize[cnt] = 0;
	}

	for (cnt = 0; cnt < MAX_WORK_POLGONS; cnt++)
		WorkPolygonGlobalMemSize[cnt] = 0;


	if (ObjectsGlobal != 0)
		ObjectsGlobalMemSize = GlobalSize(ObjectsGlobal);

	if (Objects2Global != 0)
		Objects2GlobalMemSize = GlobalSize(Objects2Global);

	if (Objects3Global != 0)
		Objects3GlobalMemSize = GlobalSize(Objects3Global);

	if (Objects4Global != 0)
		Objects4GlobalMemSize = GlobalSize(Objects4Global);

	if (Objects5Global != 0)
		Objects5GlobalMemSize = GlobalSize(Objects5Global);

	if (Objects6Global != 0)
		Objects6GlobalMemSize = GlobalSize(Objects6Global);

	if (ErrorObjectsGlobal != 0)
		ErrorObjectsGlobalMemSize = GlobalSize(ErrorObjectsGlobal);

	if (SelectedObjectsIndexGlobal != 0)
		SelectedObjectsIndexGlobalMemSize = GlobalSize(SelectedObjectsIndexGlobal);

	if (ViasGlobal != 0)
		ViasGlobalMemSize = GlobalSize(ViasGlobal);

	if (NetsGlobal != 0)
		NetsGlobalMemSize = GlobalSize(NetsGlobal);

	if (ViasPosGlobal != 0)
		ViasPosGlobalMemSize = GlobalSize(ViasPosGlobal);

	if (ConnectionsPosGlobal != 0)
		ConnectionsPosGlobalMemSize = GlobalSize(ConnectionsPosGlobal);

	if (ConnectionsGlobal != 0)
		ConnectionsGlobalMemSize = GlobalSize(ConnectionsGlobal);

	if (NetConnectionsGlobal != 0)
		NetConnectionsGlobalMemSize = GlobalSize(NetConnectionsGlobal);

	if (NetTestGlobal != 0)
		NetTestGlobalMemSize = GlobalSize(NetTestGlobal);

	if (NetTest2Global != 0)
		NetTest2GlobalMemSize = GlobalSize(NetTest2Global);

	if (CompsGlobal != 0)
		CompsGlobalMemSize = GlobalSize(CompsGlobal);

	if (ShapesGlobal != 0)
		ShapesGlobalMemSize = GlobalSize(ShapesGlobal);

	if (ShapesMemGlobal != 0)
		ShapesMemGlobalMemSize = GlobalSize(ShapesMemGlobal);

	if (CompsMemGlobal != 0)
		CompsMemGlobalMemSize = GlobalSize(CompsMemGlobal);

	if (AreaFillMemGlobal != 0)
		AreaFillMemGlobalMemSize = GlobalSize(AreaFillMemGlobal);

	if (AreaFillMemoryTempGlobal != 0)
		AreaFillMemoryTempGlobalMemSize = GlobalSize(AreaFillMemoryTempGlobal);

	if (AreaFillMemoryTemp2Global != 0)
		AreaFillMemoryTemp2GlobalMemSize = GlobalSize(AreaFillMemoryTemp2Global);

	if (AreaFillsGlobal != 0)
		AreaFillsGlobalMemSize = GlobalSize(AreaFillsGlobal);

	if (BufferPolygonGlobal != 0)
		BufferPolygonGlobalMemSize = GlobalSize(BufferPolygonGlobal);

	if (ResultPolygonGlobal != 0)
		ResultPolygonGlobalMemSize = GlobalSize(ResultPolygonGlobal);

	if (ExtraPolygonGlobal != 0)
		ExtraPolygonGlobalMemSize = GlobalSize(ExtraPolygonGlobal);

	if (NewPolygonGlobal != 0)
		NewPolygonGlobalMemSize = GlobalSize(NewPolygonGlobal);

	if (MessageBufGlobal != 0)
		MessageBufGlobalMemSize = GlobalSize(MessageBufGlobal);

	if (GlobalTempMem != 0)
		GlobalTempMemMemSize = GlobalSize(GlobalTempMem);

	if (GlobalTempMem2 != 0)
		GlobalTempMem2MemSize = GlobalSize(GlobalTempMem2);

	if (ObjectLinesGlobal != 0)
		ObjectLinesGlobalMemSize = GlobalSize(ObjectLinesGlobal);

	if (ObjectRectsGlobal != 0)
		ObjectRectsGlobalMemSize = GlobalSize(ObjectRectsGlobal);

	if (ObjectCirclesGlobal != 0)
		ObjectCirclesGlobalMemSize = GlobalSize(ObjectCirclesGlobal);

	if (ObjectArcsGlobal != 0)
		ObjectArcsGlobalMemSize = GlobalSize(ObjectArcsGlobal);

	if (ObjectTextsGlobal != 0)
		ObjectTextsGlobalMemSize = GlobalSize(ObjectTextsGlobal);

	if (ObjectTextsGlobal2 != 0)
		ObjectTextsGlobal2MemSize = GlobalSize(ObjectTextsGlobal2);

	if (ObjectPolygonMemGlobal != 0)
		ObjectPolygonMemGlobalMemSize = GlobalSize(ObjectPolygonMemGlobal);

	if (ObjectPolygonsGlobal != 0)
		ObjectPolygonsGlobalMemSize = GlobalSize(ObjectPolygonsGlobal);

	if (CharsGlobal != 0)
		CharsGlobalMemSize = GlobalSize(CharsGlobal);

	if (NetItemsGlobal != 0)
		NetItemsGlobalMemSize = GlobalSize(NetItemsGlobal);

	if (NetItems2Global != 0)
		NetItems2GlobalMemSize = GlobalSize(NetItems2Global);

	for (cnt = 0; cnt < 32; cnt++)
	{
		if (HorTracesGlobal[cnt] != 0)
			HorTracesGlobalMemSize[cnt] = GlobalSize(HorTracesGlobal[cnt]);

		if (VerTracesGlobal[cnt] != 0)
			VerTracesGlobalMemSize[cnt] = GlobalSize(VerTracesGlobal[cnt]);

		if (Diag1TracesGlobal[cnt] != 0)
			Diag1TracesGlobalMemSize[cnt] = GlobalSize(Diag1TracesGlobal[cnt]);

		if (Diag2TracesGlobal[cnt] != 0)
			Diag2TracesGlobalMemSize[cnt] = GlobalSize(Diag2TracesGlobal[cnt]);

		if (NetsPosGlobal[cnt] != 0)
			NetsPosGlobalMemSize[cnt] = GlobalSize(NetsPosGlobal[cnt]);
	}

	for (cnt = 0; cnt < 8; cnt++)
	{
		if (WorkPolygonGlobal[cnt] != 0)
			WorkPolygonGlobalMemSize[cnt] = GlobalSize(WorkPolygonGlobal[cnt]);
	}

	MemSize += ObjectsGlobalMemSize;
	MemSize += Objects2GlobalMemSize;
	MemSize += Objects3GlobalMemSize;
	MemSize += Objects4GlobalMemSize;
	MemSize += Objects5GlobalMemSize;
	MemSize += Objects6GlobalMemSize;
	MemSize += ErrorObjectsGlobalMemSize;
	MemSize += SelectedObjectsIndexGlobalMemSize;
	MemSize += ViasGlobalMemSize;
	MemSize += NetsGlobalMemSize;
	MemSize += ViasPosGlobalMemSize;
	MemSize += ConnectionsPosGlobalMemSize;
	MemSize += ConnectionsGlobalMemSize;
	MemSize += NetConnectionsGlobalMemSize;
	MemSize += NetTestGlobalMemSize;
	MemSize += NetTest2GlobalMemSize;
	MemSize += CompsGlobalMemSize;
	MemSize += ShapesGlobalMemSize;
	MemSize += ShapesMemGlobalMemSize;
	MemSize += CompsMemGlobalMemSize;
	MemSize += AreaFillMemGlobalMemSize;
	MemSize += AreaFillMemoryTempGlobalMemSize;
	MemSize += AreaFillMemoryTemp2GlobalMemSize;
	MemSize += AreaFillsGlobalMemSize;
	MemSize += BufferPolygonGlobalMemSize;
	MemSize += ResultPolygonGlobalMemSize;
	MemSize += ExtraPolygonGlobalMemSize;
	MemSize += NewPolygonGlobalMemSize;
	MemSize += MessageBufGlobalMemSize;
	MemSize += GlobalTempMemMemSize;
	MemSize += GlobalTempMem2MemSize;
	MemSize += ObjectLinesGlobalMemSize;
	MemSize += ObjectRectsGlobalMemSize;
	MemSize += ObjectCirclesGlobalMemSize;
	MemSize += ObjectArcsGlobalMemSize;
	MemSize += ObjectTextsGlobalMemSize;
	MemSize += ObjectTextsGlobal2MemSize;
	MemSize += ObjectPolygonMemGlobalMemSize;
	MemSize += ObjectPolygonsGlobalMemSize;
	MemSize += CharsGlobalMemSize;
	MemSize += NetItemsGlobalMemSize;
	MemSize += NetItems2GlobalMemSize;

	for (cnt = 0; cnt < 32; cnt++)
	{
		MemSize += HorTracesGlobalMemSize[cnt];
		MemSize += VerTracesGlobalMemSize[cnt];
		MemSize += Diag1TracesGlobalMemSize[cnt];
		MemSize += Diag2TracesGlobalMemSize[cnt];
		MemSize += NetsPosGlobalMemSize[cnt];
	}

	for (cnt = 0; cnt < 8; cnt++)
		MemSize += WorkPolygonGlobalMemSize[cnt];

	MemSize += DesignSaveMemGlobalMemSize;
	MemSize += ObjectTextBufGlobalMemSize;
	MemSize += GatePinSwapGlobalMemSize;
	MemSize += ObjectTextBufGlobalMemSize;
	MemSize += AperTuresGlobalMemSize;
	MemSize += DrillAperTuresGlobalMemSize;
	MemSize += LoadedAperTuresGlobalMemSize;
	MemSize += Nets2GlobalMemSize;
	MemSize += ActionNrGlobalMemSize;
	MemSize += hClipMemoryMemSize;
	MemSize += NewGlobalClipBoardMemMemSize;

	if (mode == 1)
	{
		MemoryTextBuf[0] = 0;
		sprintf(str, "Memory size Objects                   \t%i\t[ %i k ]\r\n", ObjectsGlobalMemSize,
		        ObjectsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Objects2                  \t%i\t[ %i k ]\r\n", Objects2GlobalMemSize,
		        Objects2GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Objects3                  \t%i\t[ %i k ]\r\n", Objects3GlobalMemSize,
		        Objects3GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Objects4                  \t%i\t[ %i k ]\r\n", Objects4GlobalMemSize,
		        Objects4GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Objects5                  \t%i\t[ %i k ]\r\n", Objects5GlobalMemSize,
		        Objects5GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Objects6                  \t%i\t[ %i k ]\r\n", Objects6GlobalMemSize,
		        Objects6GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ErrorObjects              \t%i\t[ %i k ]\r\n", ErrorObjectsGlobalMemSize,
		        ErrorObjectsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size SelectedObjectsIndex      \t%i\t[ %i k ]\r\n", SelectedObjectsIndexGlobalMemSize,
		        SelectedObjectsIndexGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Vias                      \t%i\t[ %i k ]\r\n", ViasGlobalMemSize,
		        ViasGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Nets                      \t%i\t[ %i k ]\r\n", NetsGlobalMemSize,
		        NetsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Connections               \t%i\t[ %i k ]\r\n", ConnectionsGlobalMemSize,
		        ConnectionsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size NetConnections            \t%i\t[ %i k ]\r\n", NetConnectionsGlobalMemSize,
		        NetConnectionsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Comps                     \t%i\t[ %i k ]\r\n", CompsGlobalMemSize,
		        CompsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Shapes                    \t%i\t[ %i k ]\r\n", ShapesGlobalMemSize,
		        ShapesGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ShapesMem                 \t%i\t[ %i k ]\r\n", ShapesMemGlobalMemSize,
		        ShapesMemGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size CompsMem                  \t%i\t[ %i k ]\r\n", CompsMemGlobalMemSize,
		        CompsMemGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size AreaFillMem               \t%i\t[ %i k ]\r\n", AreaFillMemGlobalMemSize,
		        AreaFillMemGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size AreaFillMemoryTemp        \t%i\t[ %i k ]\r\n", AreaFillMemoryTempGlobalMemSize,
		        AreaFillMemoryTempGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size AreaFillMemoryTemp2       \t%i\t[ %i k ]\r\n", AreaFillMemoryTemp2GlobalMemSize,
		        AreaFillMemoryTemp2GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size AreaFills                 \t%i\t[ %i k ]\r\n", AreaFillsGlobalMemSize,
		        AreaFillsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size BufferPolygon             \t%i\t[ %i k ]\r\n", BufferPolygonGlobalMemSize,
		        BufferPolygonGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ResultPolygon             \t%i\t[ %i k ]\r\n", ResultPolygonGlobalMemSize,
		        ResultPolygonGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ExtraPolygon              \t%i\t[ %i k ]\r\n", ExtraPolygonGlobalMemSize,
		        ExtraPolygonGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size NewPolygon                \t%i\t[ %i k ]\r\n", NewPolygonGlobalMemSize,
		        NewPolygonGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size MessageBuf                \t%i\t[ %i k ]\r\n", MessageBufGlobalMemSize,
		        MessageBufGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size TempMem                   \t%i\t[ %i k ]\r\n", GlobalTempMemMemSize,
		        GlobalTempMemMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size TempMem2                  \t%i\t[ %i k ]\r\n", GlobalTempMem2MemSize,
		        GlobalTempMem2MemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectLines               \t%i\t[ %i k ]\r\n", ObjectLinesGlobalMemSize,
		        ObjectLinesGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectRects               \t%i\t[ %i k ]\r\n", ObjectRectsGlobalMemSize,
		        ObjectRectsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectCircles             \t%i\t[ %i k ]\r\n", ObjectCirclesGlobalMemSize,
		        ObjectCirclesGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectArcs                \t%i\t[ %i k ]\r\n", ObjectArcsGlobalMemSize,
		        ObjectArcsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectTexts               \t%i\t[ %i k ]\r\n", ObjectTextsGlobalMemSize,
		        ObjectTextsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectTexts2              \t%i\t[ %i k ]\r\n", ObjectTextsGlobal2MemSize,
		        ObjectTextsGlobal2MemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectPolygonMem          \t%i\t[ %i k ]\r\n", ObjectPolygonMemGlobalMemSize,
		        ObjectPolygonMemGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectPolygons            \t%i\t[ %i k ]\r\n", ObjectPolygonsGlobalMemSize,
		        ObjectPolygonsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Chars                     \t%i\t[ %i k ]\r\n", CharsGlobalMemSize,
		        CharsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size NetItems                  \t%i\t[ %i k ]\r\n", NetItemsGlobalMemSize,
		        NetItemsGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size NetItems2                 \t%i\t[ %i k ]\r\n", NetItems2GlobalMemSize,
		        NetItems2GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);

		for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
		{
			sprintf(str, "Memory size HorTraces%02i         \t%i\t[ %i k ]\r\n", cnt, HorTracesGlobalMemSize[cnt],
			        HorTracesGlobalMemSize[cnt] / 1024);
			strcat(MemoryTextBuf, str);
			sprintf(str, "Memory size VerTraces%02i         \t%i\t[ %i k ]\r\n", cnt, VerTracesGlobalMemSize[cnt],
			        VerTracesGlobalMemSize[cnt] / 1024);
			strcat(MemoryTextBuf, str);
			sprintf(str, "Memory size Diag1Traces%02i       \t%i\t[ %i k ]\r\n", cnt, Diag1TracesGlobalMemSize[cnt],
			        Diag1TracesGlobalMemSize[cnt] / 1024);
			strcat(MemoryTextBuf, str);
			sprintf(str, "Memory size Diag2Traces%02i       \t%i\t[ %i k ]\r\n", cnt, Diag2TracesGlobalMemSize[cnt],
			        Diag2TracesGlobalMemSize[cnt] / 1024);
			strcat(MemoryTextBuf, str);
			sprintf(str, "Memory size NetsPos%02i           \t%i\t[ %i k ]\r\n", cnt, NetsPosGlobalMemSize[cnt],
			        NetsPosGlobalMemSize[cnt] / 1024);
			strcat(MemoryTextBuf, str);
		}

		for (cnt = 0; cnt < 8; cnt++)
		{
			sprintf(str, "Memory size WorkPolygon%i            \t%i\t[ %i k ]\r\n", cnt, WorkPolygonGlobalMemSize[cnt],
			        WorkPolygonGlobalMemSize[cnt] / 1024);
			strcat(MemoryTextBuf, str);
		}

		sprintf(str, "Memory size DesignSaveMem             \t%i\t[ %i k ]\r\n", DesignSaveMemGlobalMemSize,
		        DesignSaveMemGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ObjectTextBuf             \t%i\t[ %i k ]\r\n", ObjectTextBufGlobalMemSize,
		        ObjectTextBufGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size GatePinSwap               \t%i\t[ %i k ]\r\n", GatePinSwapGlobalMemSize,
		        GatePinSwapGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size AperTures                 \t%i\t[ %i k ]\r\n", AperTuresGlobalMemSize,
		        AperTuresGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size DrillAperTures            \t%i\t[ %i k ]\r\n", DrillAperTuresGlobalMemSize,
		        DrillAperTuresGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size LoadedAperTures           \t%i\t[ %i k ]\r\n", LoadedAperTuresGlobalMemSize,
		        LoadedAperTuresGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size Nets2                     \t%i\t[ %i k ]\r\n", Nets2GlobalMemSize,
		        Nets2GlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size ActionNr                  \t%i\t[ %i k ]\r\n", ActionNrGlobalMemSize,
		        ActionNrGlobalMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size hClipMemory               \t%i\t[ %i k ]\r\n", hClipMemoryMemSize,
		        hClipMemoryMemSize / 1024);
		strcat(MemoryTextBuf, str);
		sprintf(str, "Memory size NewClipBoardMem           \t%i\t[ %i k ]\r\n", NewGlobalClipBoardMemMemSize,
		        NewGlobalClipBoardMemMemSize / 1024);
		strcat(MemoryTextBuf, str);
	}



	return MemSize;
}



// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemHorTraces(int32 Layer, int32 count)
{
	HGLOBAL NewMem;

	if ((Layer < 0) || (Layer >= 32))
		return -1;

	if (count > LimitMaxNrTraces)
		return -2;

	if (MaxNrHorTraces[Layer] == 0)
	{
		count = max(1024, count);

		if ((HorTracesGlobal[Layer] = GlobalAlloc(GHND, count * sizeof(TraceRecord))) == NULL)
			return -1;

		if ((HorTraces[Layer] = (TracesArray *) GlobalLock(HorTracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrHorTraces[Layer] = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(HorTracesGlobal[Layer], count * sizeof(TraceRecord), GHND)) == NULL)
			return -1;

		HorTracesGlobal[Layer] = NewMem;

		if ((HorTraces[Layer] = (TracesArray *) GlobalLock(HorTracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrHorTraces[Layer] = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemVerTraces(int32 Layer, int32 count)
{
	HGLOBAL NewMem;

	if ((Layer < 0) || (Layer >= 32))
		return -1;

	if (count > LimitMaxNrTraces)
		return -2;

	if (MaxNrVerTraces[Layer] == 0)
	{
		count = max(1024, count);

		if ((VerTracesGlobal[Layer] = GlobalAlloc(GHND, count * sizeof(TraceRecord))) == NULL)
			return -1;

		if ((VerTraces[Layer] = (TracesArray *) GlobalLock(VerTracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrVerTraces[Layer] = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(VerTracesGlobal[Layer], count * sizeof(TraceRecord), GHND)) == NULL)
			return -1;

		VerTracesGlobal[Layer] = NewMem;

		if ((VerTraces[Layer] = (TracesArray *) GlobalLock(VerTracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrVerTraces[Layer] = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemDiag1Traces(int32 Layer, int32 count)
{
	HGLOBAL NewMem;

	if ((Layer < 0) || (Layer >= 32))
		return -1;

	if (count > LimitMaxNrTraces)
		return -2;

	if (MaxNrDiag1Traces[Layer] == 0)
	{
		count = max(1024, count);

		if ((Diag1TracesGlobal[Layer] = GlobalAlloc(GHND, count * sizeof(TraceRecord))) == NULL)
			return -1;

		if ((Diag1Traces[Layer] = (TracesArray *) GlobalLock(Diag1TracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrDiag1Traces[Layer] = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Diag1TracesGlobal[Layer], count * sizeof(TraceRecord), GHND)) == NULL)
			return -1;

		Diag1TracesGlobal[Layer] = NewMem;

		if ((Diag1Traces[Layer] = (TracesArray *) GlobalLock(Diag1TracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrDiag1Traces[Layer] = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemDiag2Traces(int32 Layer, int32 count)
{
	HGLOBAL NewMem;

	if ((Layer < 0) || (Layer >= 32))
		return -1;

	if (count > LimitMaxNrTraces)
		return -2;

	if (MaxNrDiag2Traces[Layer] == 0)
	{
		count = max(1024, count);

		if ((Diag2TracesGlobal[Layer] = GlobalAlloc(GHND, count * sizeof(TraceRecord))) == NULL)
			return -1;

		if ((Diag2Traces[Layer] = (TracesArray *) GlobalLock(Diag2TracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrDiag2Traces[Layer] = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Diag2TracesGlobal[Layer], count * sizeof(TraceRecord), GHND)) == NULL)
			return -1;

		Diag2TracesGlobal[Layer] = NewMem;

		if ((Diag2Traces[Layer] = (TracesArray *) GlobalLock(Diag2TracesGlobal[Layer])) == NULL)
			return -1;

		MaxNrDiag2Traces[Layer] = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemVias(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrVias)
		return -2;

	if (MaxNrVias == 0)
	{
		count = max(1024, count);

		if ((ViasGlobal = GlobalAlloc(GHND, count * sizeof(ViaRecord))) == NULL)
			return -1;

		if ((Vias = (ViaArray *) GlobalLock(ViasGlobal)) == NULL)
			return -1;

		MaxNrVias = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ViasGlobal, count * sizeof(ViaRecord), GHND)) == NULL)
			return -1;

		ViasGlobal = NewMem;

		if ((Vias = (ViaArray *) GlobalLock(ViasGlobal)) == NULL)
			return -1;

		MaxNrVias = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectLines(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjectLines)
		return -2;

	if (MaxNrObjectLines == 0)
	{
		count = max(256, count);

		if ((ObjectLinesGlobal = GlobalAlloc(GHND, count * sizeof(ObjectLineRecord))) == NULL)
			return -1;

		if ((ObjectLines = (ObjectLinesArray *) GlobalLock(ObjectLinesGlobal)) == NULL)
			return -1;

		MaxNrObjectLines = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectLinesGlobal, count * sizeof(ObjectLineRecord), GHND)) == NULL)
			return -1;

		ObjectLinesGlobal = NewMem;

		if ((ObjectLines = (ObjectLinesArray *) GlobalLock(ObjectLinesGlobal)) == NULL)
			return -1;

		MaxNrObjectLines = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectRects(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjectRects)
		return -2;

	if (MaxNrObjectRects == 0)
	{
		count = max(64, count);

		if ((ObjectRectsGlobal = GlobalAlloc(GHND, count * sizeof(ObjectRectRecord))) == NULL)
			return -1;

		if ((ObjectRects = (ObjectRectsArray *) GlobalLock(ObjectRectsGlobal)) == NULL)
			return -1;

		MaxNrObjectRects = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectRectsGlobal, count * sizeof(ObjectRectRecord), GHND)) == NULL)
			return -1;

		ObjectRectsGlobal = NewMem;

		if ((ObjectRects = (ObjectRectsArray *) GlobalLock(ObjectRectsGlobal)) == NULL)
			return -1;

		MaxNrObjectRects = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectCircles(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjectCircles)
		return -2;

	if (MaxNrObjectCircles == 0)
	{
		count = max(64, count);

		if ((ObjectCirclesGlobal = GlobalAlloc(GHND, count * sizeof(ObjectCircleRecord))) == NULL)
			return -1;

		if ((ObjectCircles = (ObjectCirclesArray *) GlobalLock(ObjectCirclesGlobal)) == NULL)
			return -1;

		MaxNrObjectCircles = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectCirclesGlobal, count * sizeof(ObjectCircleRecord), GHND)) == NULL)
			return -1;

		ObjectCirclesGlobal = NewMem;

		if ((ObjectCircles = (ObjectCirclesArray *) GlobalLock(ObjectCirclesGlobal)) == NULL)
			return -1;

		MaxNrObjectCircles = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectArcs(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjectArcs)
		return -2;

	if (MaxNrObjectArcs == 0)
	{
		count = max(32, count);

		if ((ObjectArcsGlobal = GlobalAlloc(GHND, count * sizeof(ObjectArcRecord))) == NULL)
			return -1;

		if ((ObjectArcs = (ObjectArcsArray *) GlobalLock(ObjectArcsGlobal)) == NULL)
			return -1;

		MaxNrObjectArcs = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectArcsGlobal, count * sizeof(ObjectArcRecord), GHND)) == NULL)
			return -1;

		ObjectArcsGlobal = NewMem;

		if ((ObjectArcs = (ObjectArcsArray *) GlobalLock(ObjectArcsGlobal)) == NULL)
			return -1;

		MaxNrObjectArcs = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectTexts(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjectTexts)
		return -2;

	if (MaxNrObjectTexts == 0)
	{
		count = max(128, count);

		if ((ObjectTextsGlobal = GlobalAlloc(GHND, count * sizeof(ObjectTextRecord))) == NULL)
			return -1;

		if ((ObjectTexts = (ObjectTextsArray *) GlobalLock(ObjectTextsGlobal)) == NULL)
			return -1;

		MaxNrObjectTexts = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectTextsGlobal, count * sizeof(ObjectTextRecord), GHND)) == NULL)
			return -1;

		ObjectTextsGlobal = NewMem;

		if ((ObjectTexts = (ObjectTextsArray *) GlobalLock(ObjectTextsGlobal)) == NULL)
			return -1;

		MaxNrObjectTexts = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjects)
		return -2;

	if (MaxNrObjects == 0)
	{
		count = max(count, 256);

		if ((ObjectsGlobal = GlobalAlloc(GHND, count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects = (ObjectArray *) GlobalLock(ObjectsGlobal)) == NULL)
			return -1;

		MaxNrObjects = count;
	}
	else
	{
		if (count > MaxNrObjects)
		{
			if ((NewMem = GlobalReAlloc(ObjectsGlobal, count * sizeof(ObjectRecord), GHND)) == NULL)
				return -1;

			ObjectsGlobal = NewMem;

			if ((Objects = (ObjectArray *) GlobalLock(ObjectsGlobal)) == NULL)
				return -1;

			MaxNrObjects = count;
		}
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects2(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjects)
		return -2;

	if (MaxNrObjects2 == 0)
	{
		count = max(count, 256);

		if ((Objects2Global = GlobalAlloc(GHND, count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects2 = (ObjectArray *) GlobalLock(Objects2Global)) == NULL)
			return -1;

		MaxNrObjects2 = count;
	}
	else
	{
		if (count > MaxNrObjects2)
		{
			if ((NewMem = GlobalReAlloc(Objects2Global, count * sizeof(ObjectRecord), GHND)) == NULL)
				return -1;

			Objects2Global = NewMem;

			if ((Objects2 = (ObjectArray *) GlobalLock(Objects2Global)) == NULL)
				return -1;

			MaxNrObjects2 = count;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects3(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjects)
		return -2;

	if (MaxNrObjects3 == 0)
	{
		count = max(count, 256);

		if ((Objects3Global = GlobalAlloc(GHND, count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects3 = (ObjectArray *) GlobalLock(Objects3Global)) == NULL)
			return -1;

		MaxNrObjects3 = count;
	}
	else
	{
		if (count > MaxNrObjects3)
		{
			if ((NewMem = GlobalReAlloc(Objects3Global, count * sizeof(ObjectRecord), GHND)) == NULL)
				return -1;

			Objects3Global = NewMem;

			if ((Objects3 = (ObjectArray *) GlobalLock(Objects3Global)) == NULL)
				return -1;

			MaxNrObjects3 = count;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects4(int32 count)
{
	HGLOBAL NewMem;

	if (count > 512 * 1024)
		return -2;

	if (MaxNrObjects4 == 0)
	{
		count = max(count, 256);

		if ((Objects4Global = GlobalAlloc(GHND, count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects4 = (ObjectArray *) GlobalLock(Objects4Global)) == NULL)
			return -1;

		MaxNrObjects4 = count;
	}
	else
	{
		if (count > MaxNrObjects4)
		{
			if ((NewMem = GlobalReAlloc(Objects4Global, count * sizeof(ObjectRecord), GHND)) == NULL)
				return -1;

			Objects4Global = NewMem;

			if ((Objects4 = (ObjectArray *) GlobalLock(Objects4Global)) == NULL)
				return -1;

			MaxNrObjects4 = count;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects5(int32 count)
{
	HGLOBAL NewMem;

	if (count > 512 * 1024)
		return -2;

	if (MaxNrObjects5 == 0)
	{
		count = max(count, 256);

		if ((Objects5Global = GlobalAlloc(GHND, count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects5 = (ObjectArray *) GlobalLock(Objects5Global)) == NULL)
			return -1;

		MaxNrObjects5 = count;
	}
	else
	{
		if (count > MaxNrObjects5)
		{
			if ((NewMem = GlobalReAlloc(Objects5Global, count * sizeof(ObjectRecord), GHND)) == NULL)
				return -1;

			Objects5Global = NewMem;

			if ((Objects5 = (ObjectArray *) GlobalLock(Objects5Global)) == NULL)
				return -1;

			MaxNrObjects5 = count;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjects6(int32 count)
{
	HGLOBAL NewMem;

	if (count > 512 * 1024)
		return -2;

	if (MaxNrObjects6 == 0)
	{
		count = max(count, 256);

		if ((Objects6Global = GlobalAlloc(GHND, count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((Objects6 = (ObjectArray *) GlobalLock(Objects6Global)) == NULL)
			return -1;

		MaxNrObjects6 = count;
	}
	else
	{
		if (count > MaxNrObjects6)
		{
			if ((NewMem = GlobalReAlloc(Objects6Global, count * sizeof(ObjectRecord), GHND)) == NULL)
				return -1;

			Objects6Global = NewMem;

			if ((Objects6 = (ObjectArray *) GlobalLock(Objects6Global)) == NULL)
				return -1;

			MaxNrObjects6 = count;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemErrorObjects(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjects)
		return -2;

	if (MaxNrErrorObjects == 0)
	{
		count = max(count, 256);

		if ((ErrorObjectsGlobal = GlobalAlloc(GHND, count * sizeof(ObjectRecord))) == NULL)
			return -1;

		if ((ErrorObjects = (ObjectArray *) GlobalLock(ErrorObjectsGlobal)) == NULL)
			return -1;

		MaxNrErrorObjects = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ErrorObjectsGlobal, count * sizeof(ObjectRecord), GHND)) == NULL)
			return -1;

		ErrorObjectsGlobal = NewMem;

		if ((ErrorObjects = (ObjectArray *) GlobalLock(ErrorObjectsGlobal)) == NULL)
			return -1;

		MaxNrErrorObjects = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemComps(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrComps)
		return -2;

	if (MaxNrComps == 0)
	{
		count = max(count, 256);

		if ((CompsGlobal = GlobalAlloc(GHND, count * sizeof(int32))) == NULL)
			return -1;

		if ((Comps = (CompsArray *) GlobalLock(CompsGlobal)) == NULL)
			return -1;

		MaxNrComps = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(CompsGlobal, count * sizeof(int32), GHND)) == NULL)
			return -1;

		CompsGlobal = NewMem;

		if ((Comps = (CompsArray *) GlobalLock(CompsGlobal)) == NULL)
			return -1;

		MaxNrComps = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemCompsMemory(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitCompsMemory)
		return -2;

	if (MaxCompsMemory == 0)
	{
		count = max(count, 32768);

		if ((CompsMemGlobal = GlobalAlloc(GHND, count)) == NULL)
			return -1;

		if ((CompsMem = (uint8 *) GlobalLock(CompsMemGlobal)) == NULL)
			return -1;

		MaxCompsMemory = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(CompsMemGlobal, count, GHND)) == NULL)
			return -1;

		CompsMemGlobal = NewMem;

		if ((CompsMem = (uint8 *) GlobalLock(CompsMemGlobal)) == NULL)
			return -1;

		MaxCompsMemory = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemShapes(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrShapes)
		return -2;

	if (MaxNrShapes == 0)
	{
		count = max(count, 256);

		if ((ShapesGlobal = GlobalAlloc(GHND, count * sizeof(ShapeInfoRecord))) == NULL)
			return -1;

		if ((Shapes = (ShapesArray *) GlobalLock(ShapesGlobal)) == NULL)
			return -1;

		MaxNrShapes = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ShapesGlobal, count * sizeof(ShapeInfoRecord), GHND)) == NULL)
			return -1;

		ShapesGlobal = NewMem;

		if ((Shapes = (ShapesArray *) GlobalLock(ShapesGlobal)) == NULL)
			return -1;

		MaxNrShapes = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemShapesMemory(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitCompsMemory)
		return -2;

	if (MaxShapesMemory == 0)
	{
		count = max(count, 32768);

		if ((ShapesMemGlobal = GlobalAlloc(GHND, count)) == NULL)
			return -1;

		if ((ShapesMem = (uint8 *) GlobalLock(ShapesMemGlobal)) == NULL)
			return -1;

		MaxShapesMemory = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ShapesMemGlobal, count, GHND)) == NULL)
			return -1;

		ShapesMemGlobal = NewMem;

		if ((ShapesMem = (uint8 *) GlobalLock(ShapesMemGlobal)) == NULL)
			return -1;

		MaxShapesMemory = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemConnections(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrConnections)
		return -2;

	if (MaxNrConnections == 0)
	{
		count = max(count, 256);

		if ((ConnectionsGlobal = GlobalAlloc(GHND, count * sizeof(ConnectionsRecord))) == NULL)
			return -1;

		if ((Connections = (ConnectionsArray *) GlobalLock(ConnectionsGlobal)) == NULL)
			return -1;

		MaxNrConnections = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ConnectionsGlobal, count * sizeof(ConnectionsRecord), GHND)) == NULL)
			return -1;

		ConnectionsGlobal = NewMem;

		if ((Connections = (ConnectionsArray *) GlobalLock(ConnectionsGlobal)) == NULL)
			return -1;

		MaxNrConnections = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemNetConnections(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrNetConnections)
		return -2;

	if (MaxNrNetConnections == 0)
	{
		count = max(count, 256);

		if ((NetConnectionsGlobal = GlobalAlloc(GHND, count * sizeof(ConnectionsRecord))) == NULL)
			return -1;

		if ((NetConnections = (NetConnectionsArray *) GlobalLock(NetConnectionsGlobal)) == NULL)
			return -1;

		MaxNrNetConnections = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(NetConnectionsGlobal, count * sizeof(ConnectionsRecord), GHND)) == NULL)
			return -1;

		NetConnectionsGlobal = NewMem;

		if ((NetConnections = (NetConnectionsArray *) GlobalLock(NetConnectionsGlobal)) == NULL)
			return -1;

		MaxNrNetConnections = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemNets(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrNets)
		return -2;

	if (MaxNrNets == 0)
	{
		count = max(count, 256);

		if ((NetsGlobal = GlobalAlloc(GHND, count * sizeof(NetRecord))) == NULL)
			return -1;

		if ((Nets = (NetArray *) GlobalLock(NetsGlobal)) == NULL)
			return -1;

		MaxNrNets = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(NetsGlobal, count * sizeof(NetRecord), GHND)) == NULL)
			return -1;

		NetsGlobal = NewMem;

		if ((Nets = (NetArray *) GlobalLock(NetsGlobal)) == NULL)
			return -1;

		MaxNrNets = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemAreaFillMemory(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxAreaFillMemory)
		return -2;

	if (MaxAreaFillMemory == 0)
	{
		count = max(count, 3172);

		if ((AreaFillMemGlobal = GlobalAlloc(GHND, count)) == NULL)
			return -1;

		if ((AreaFillMem = (uint8 *) GlobalLock(AreaFillMemGlobal)) == NULL)
			return -1;

		MaxAreaFillMemory = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(AreaFillMemGlobal, count, GHND)) == NULL)
			return -1;

		AreaFillMemGlobal = NewMem;

		if ((AreaFillMem = (uint8 *) GlobalLock(AreaFillMemGlobal)) == NULL)
			return -1;

		MaxAreaFillMemory = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectPolygonMemory(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxObjectPolygonMemory)
		return -2;

	if (MaxObjectPolygonMemory == 0)
	{
		count = max(count, 3172);

		if ((ObjectPolygonMemGlobal = GlobalAlloc(GHND, count)) == NULL)
			return -1;

		if ((ObjectPolygonMem = (uint8 *) GlobalLock(ObjectPolygonMemGlobal)) == NULL)
			return -1;

		MaxObjectPolygonMemory = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectPolygonMemGlobal, count, GHND)) == NULL)
			return -1;

		ObjectPolygonMemGlobal = NewMem;

		if ((ObjectPolygonMem = (uint8 *) GlobalLock(ObjectPolygonMemGlobal)) == NULL)
			return -1;

		MaxObjectPolygonMemory = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


int32 AllocateMemObjectPolygons(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjectPolygons)
		return -2;

	if (MaxNrObjectPolygons == 0)
	{
		count = max(count, 64);

		if ((ObjectPolygonsGlobal = GlobalAlloc(GHND, count * sizeof(int32))) == NULL)
			return -1;

		if ((ObjectPolygons = (ObjectPolygonsArray *) GlobalLock(ObjectPolygonsGlobal)) == NULL)
			return -1;

		MaxNrObjectPolygons = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectPolygonsGlobal, count * sizeof(int32), GHND)) == NULL)
			return -1;

		ObjectPolygonsGlobal = NewMem;

		if ((ObjectPolygons = (ObjectPolygonsArray *) GlobalLock(ObjectPolygonsGlobal)) == NULL)
			return -1;

		MaxNrObjectPolygons = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemObjectTexts2(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjectTexts2)
		return -2;

	if (MaxNrObjectTexts2 == 0)
	{
		count = max(128, count);

		if ((ObjectTextsGlobal2 = GlobalAlloc(GHND, count * sizeof(ObjectTextRecord2))) == NULL)
			return -1;

		if ((ObjectTexts2 = (ObjectTextsArray2 *) GlobalLock(ObjectTextsGlobal2)) == NULL)
			return -1;

		MaxNrObjectTexts2 = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectTextsGlobal2, count * sizeof(ObjectTextRecord2), GHND)) == NULL)
			return -1;

		ObjectTextsGlobal2 = NewMem;

		if ((ObjectTexts2 = (ObjectTextsArray2 *) GlobalLock(ObjectTextsGlobal2)) == NULL)
			return -1;

		MaxNrObjectTexts2 = count;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


int32 AllocateMemAreaFillMemoryTemp(int32 count)
{
	HGLOBAL NewMem;
	DWORD Error, Size, Size2;

	if (count > LimitMaxAreaFillMemory)
		return -2;

	count = max(count, 256 * 1024);

	if (MaxAreaFillMemoryTemp == 0)
	{
//    count=max(count,3172);
		if ((AreaFillMemoryTempGlobal = GlobalAlloc(GHND, count)) == NULL)
			return -1;

		if ((AreaFillMemTemp = (uint8 *) GlobalLock(AreaFillMemoryTempGlobal)) == NULL)
			return -1;

		if ((AreaFillMemoryTemp2Global = GlobalAlloc(GHND, count)) == NULL)
			return -1;

		if ((AreaFillMemTemp2 = (uint8 *) GlobalLock(AreaFillMemoryTemp2Global)) == NULL)
			return -1;

		MaxAreaFillMemoryTemp = count;
		Size = GlobalSize(AreaFillMemoryTempGlobal);
	}
	else
	{
//    count+=32768;
		if (count > MaxAreaFillMemoryTemp)
		{
			Size2 = GlobalSize(AreaFillMemoryTemp2Global);
			Size = GlobalSize(AreaFillMemoryTempGlobal);

			//    GlobalUnlock(AreaFillMemoryTempGlobal);
			if ((NewMem = GlobalReAlloc(AreaFillMemoryTempGlobal, count, GHND)) == NULL)
			{
				Error = GetLastError();
				return -1;
			}

			AreaFillMemoryTempGlobal = NewMem;

			if ((AreaFillMemTemp = (uint8 *) GlobalLock(AreaFillMemoryTempGlobal)) == NULL)
				return -1;

			if ((NewMem = GlobalReAlloc(AreaFillMemoryTemp2Global, count, GHND)) == NULL)
				return -1;

			AreaFillMemoryTemp2Global = NewMem;

			if ((AreaFillMemTemp2 = (uint8 *) GlobalLock(AreaFillMemoryTemp2Global)) == NULL)
				return -1;

			MaxAreaFillMemoryTemp = count;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


int32 AllocateMemAreaFills(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrAreaFills)
		return -2;

	if (MaxNrAreaFills == 0)
	{
		count = max(count, 64);

		if ((AreaFillsGlobal = GlobalAlloc(GHND, count * sizeof(int32))) == NULL)
			return -1;

		if ((AreaFills = (AreaFillsArray *) GlobalLock(AreaFillsGlobal)) == NULL)
			return -1;

		MaxNrAreaFills = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(AreaFillsGlobal, count * sizeof(int32), GHND)) == NULL)
			return -1;

		AreaFillsGlobal = NewMem;

		if ((AreaFills = (AreaFillsArray *) GlobalLock(AreaFillsGlobal)) == NULL)
			return -1;

		MaxNrAreaFills = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


int32 AllocateMemPolygons(int32 NrVerticesPolygon)
{
	int32 cnt, MemSize;
	HGLOBAL NewMem;

	if (MaxNrVerticesPolygon == 0)
	{
		NrVerticesPolygon = max(2048, NrVerticesPolygon);
		MemSize = NrVerticesPolygon * sizeof(PointRecord) + sizeof(PolygonInitRecord) + 100;

		if ((BufferPolygonGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((BufferPolygon = (PolygonRecord *) GlobalLock(BufferPolygonGlobal)) == NULL)
			return -1;

		if ((ResultPolygonGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((ResultPolygon = (PolygonRecord *) GlobalLock(ResultPolygonGlobal)) == NULL)
			return -1;

		if ((ExtraPolygonGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((ExtraPolygon = (PolygonRecord *) GlobalLock(ExtraPolygonGlobal)) == NULL)
			return -1;

		if ((NewPolygonGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((NewPolygon = (PolygonRecord *) GlobalLock(NewPolygonGlobal)) == NULL)
			return -1;

		for (cnt = 0; cnt < MAX_WORK_POLGONS; cnt++)
		{
			if ((WorkPolygonGlobal[cnt] = GlobalAlloc(GHND, MemSize)) == NULL)
				return -1;

			if ((WorkPolygon[cnt] = (PolygonRecord *) GlobalLock(WorkPolygonGlobal[cnt])) == NULL)
				return -1;
		}

		MaxNrVerticesPolygon = NrVerticesPolygon;
	}
	else
	{
		if (NrVerticesPolygon > MaxNrVerticesPolygon)
		{
			MemSize = NrVerticesPolygon * sizeof(PointRecord) + sizeof(PolygonInitRecord) + 100;

			if ((NewMem = GlobalReAlloc(BufferPolygonGlobal, MemSize, GHND)) == NULL)
				return -1;

			BufferPolygonGlobal = NewMem;

			if ((BufferPolygon = (PolygonRecord *) GlobalLock(BufferPolygonGlobal)) == NULL)
				return -1;

			if ((NewMem = GlobalReAlloc(ResultPolygonGlobal, MemSize, GHND)) == NULL)
				return -1;

			ResultPolygonGlobal = NewMem;

			if ((ResultPolygon = (PolygonRecord *) GlobalLock(ResultPolygonGlobal)) == NULL)
				return -1;

			if ((NewMem = GlobalReAlloc(ExtraPolygonGlobal, MemSize, GHND)) == NULL)
				return -1;

			ExtraPolygonGlobal = NewMem;

			if ((ExtraPolygon = (PolygonRecord *) GlobalLock(ExtraPolygonGlobal)) == NULL)
				return -1;

			if ((NewMem = GlobalReAlloc(NewPolygonGlobal, MemSize, GHND)) == NULL)
				return -1;

			NewPolygonGlobal = NewMem;

			if ((NewPolygon = (PolygonRecord *) GlobalLock(NewPolygonGlobal)) == NULL)
				return -1;

			for (cnt = 0; cnt < MAX_WORK_POLGONS; cnt++)
			{
				if ((NewMem = GlobalReAlloc(WorkPolygonGlobal[cnt], MemSize, GHND)) == NULL)
					return -1;

				WorkPolygonGlobal[cnt] = NewMem;

				if ((WorkPolygon[cnt] = (PolygonRecord *) GlobalLock(WorkPolygonGlobal[cnt])) == NULL)
					return -1;
			}

			MaxNrVerticesPolygon = NrVerticesPolygon;
		}
	}

	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemClipBoard(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (ClipBoardMemSize == 0)
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
		if ((NewMem = GlobalReAlloc(GlobalTempMem, MemSize, GHND)) == NULL)
			return -1;

		GlobalTempMem = NewMem;

		if ((TempMem = GlobalLock(GlobalTempMem)) == NULL)
			return -1;

		MaxTempMemory = MemSize;
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
		if ((NewMem = GlobalReAlloc(GlobalTempMem2, MemSize, GHND)) == NULL)
			return -1;

		GlobalTempMem2 = NewMem;

		if ((TempMem2 = GlobalLock(GlobalTempMem2)) == NULL)
			return -1;

		MaxTempMemory2 = MemSize;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemTemp3(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxTempMemory3 == 0)
	{
		if ((GlobalTempMem3 = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((TempMem3 = GlobalLock(GlobalTempMem3)) == NULL)
			return -1;

		MaxTempMemory3 = MemSize;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(GlobalTempMem3, MemSize, GHND)) == NULL)
			return -1;

		GlobalTempMem3 = NewMem;

		if ((TempMem3 = GlobalLock(GlobalTempMem3)) == NULL)
			return -1;

		MaxTempMemory3 = MemSize;
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

int32 DeallocateSpecialMem(int32 Index)
{
	if ((Index >= 0) && (Index < MAX_SPECIAL_MEM) && (GlobalSpecialMem[Index] != NULL))
	{
		GlobalUnlock(GlobalSpecialMem[Index]);
		GlobalFree(GlobalSpecialMem[Index]);
		GlobalSpecialMem[Index] = NULL;
		MaxSpecialMemory[Index] = 0;
		SpecialMem[Index] = NULL;
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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemTemp3()
{
	if (GlobalTempMem3 != NULL)
	{
		GlobalUnlock(GlobalTempMem3);
		GlobalFree(GlobalTempMem3);
		GlobalTempMem3 = NULL;
		MaxTempMemory3 = 0;
		TempMemorySize3 = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemSelectedObjectsIndex()
{
	GlobalUnlock(SelectedObjectsIndexGlobal);
	GlobalFree(SelectedObjectsIndexGlobal);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMem()
{
	if ((CharsGlobal = GlobalAlloc(GHND, (int32) sizeof(CharsArray))) == NULL)
		return -1;

	if ((Chars = (CharsArray *) GlobalLock(CharsGlobal)) == NULL)
		return -1;

	if (AllocateMemObjects(1024) != 0)
		return -1;

	if (AllocateMemObjects2(1024) != 0)
		return -1;

	if (AllocateMemObjects3(1024) != 0)
		return -1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemHorTraces(int32 Layer)
{
	if (HorTracesGlobal[Layer] != NULL)
	{
		GlobalUnlock(HorTracesGlobal[Layer]);
		GlobalFree(HorTracesGlobal[Layer]);
		HorTracesGlobal[Layer] = NULL;
		MaxNrHorTraces[Layer] = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemVerTraces(int32 Layer)
{
	if (VerTracesGlobal[Layer] != NULL)
	{
		GlobalUnlock(VerTracesGlobal[Layer]);
		GlobalFree(VerTracesGlobal[Layer]);
		VerTracesGlobal[Layer] = NULL;
		MaxNrVerTraces[Layer] = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemDiag1Traces(int32 Layer)
{
	if (Diag1TracesGlobal[Layer] != NULL)
	{
		GlobalUnlock(Diag1TracesGlobal[Layer]);
		GlobalFree(Diag1TracesGlobal[Layer]);
		Diag1TracesGlobal[Layer] = NULL;
		MaxNrDiag1Traces[Layer] = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemDiag2Traces(int32 Layer)
{
	if (Diag2TracesGlobal[Layer] != NULL)
	{
		GlobalUnlock(Diag2TracesGlobal[Layer]);
		GlobalFree(Diag2TracesGlobal[Layer]);
		Diag2TracesGlobal[Layer] = NULL;
		MaxNrDiag2Traces[Layer] = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemDesign()
{
	int32 cnt, Layer;

	/***********************************************************************************************/

	if (ObjectTextsGlobal != NULL)
	{
		GlobalUnlock(ObjectTextsGlobal);
		GlobalFree(ObjectTextsGlobal);
		ObjectTextsGlobal = NULL;
		MaxNrObjectTexts = 0;
	}

	if (ObjectArcsGlobal != NULL)
	{
		GlobalUnlock(ObjectArcsGlobal);
		GlobalFree(ObjectArcsGlobal);
		ObjectArcsGlobal = NULL;
		MaxNrObjectArcs = 0;
	}

	if (ObjectCirclesGlobal != NULL)
	{
		GlobalUnlock(ObjectCirclesGlobal);
		GlobalFree(ObjectCirclesGlobal);
		ObjectCirclesGlobal = NULL;
		MaxNrObjectCircles = 0;
	}

	if (ObjectRectsGlobal != NULL)
	{
		GlobalUnlock(ObjectRectsGlobal);
		GlobalFree(ObjectRectsGlobal);
		ObjectRectsGlobal = NULL;
		MaxNrObjectRects = 0;
	}

	if (ObjectLinesGlobal != NULL)
	{
		GlobalUnlock(ObjectLinesGlobal);
		GlobalFree(ObjectLinesGlobal);
		ObjectLinesGlobal = NULL;
		MaxNrObjectLines = 0;
	}

	if (AreaFillMemGlobal != NULL)
	{
		GlobalUnlock(AreaFillMemGlobal);
		GlobalFree(AreaFillMemGlobal);
		AreaFillMemGlobal = NULL;
		MaxAreaFillMemory = 0;
	}

	if (ShapesMemGlobal != NULL)
	{
		GlobalUnlock(ShapesMemGlobal);
		GlobalFree(ShapesMemGlobal);
		ShapesMemGlobal = NULL;
		MaxShapesMemory = 0;
	}

	if (CompsMemGlobal != NULL)
	{
		GlobalUnlock(CompsMemGlobal);
		GlobalFree(CompsMemGlobal);
		CompsMemGlobal = NULL;
		MaxCompsMemory = 0;
	}

	if (NetTest2Global != NULL)
	{
		GlobalUnlock(NetTest2Global);
		GlobalFree(NetTest2Global);
		NetTest2Global = NULL;
		MaxNrNetTests2 = 0;
	}

	if (NetTestGlobal != NULL)
	{
		GlobalUnlock(NetTestGlobal);
		GlobalFree(NetTestGlobal);
		NetTestGlobal = NULL;
		MaxNrNetTests = 0;
	}

	if (NetConnectionsGlobal != NULL)
	{
		GlobalUnlock(NetConnectionsGlobal);
		GlobalFree(NetConnectionsGlobal);
		NetConnectionsGlobal = NULL;
		MaxNrNetConnections = 0;
	}

	if (ConnectionsGlobal != NULL)
	{
		GlobalUnlock(ConnectionsGlobal);
		GlobalFree(ConnectionsGlobal);
		ConnectionsGlobal = NULL;
		MaxNrConnections = 0;
	}

	if (ViasPosGlobal != NULL)
	{
		GlobalUnlock(ViasPosGlobal);
		GlobalFree(ViasPosGlobal);
		ViasPosGlobal = NULL;
		MaxNrViasPos = 0;
	}

	if (ConnectionsPosGlobal != NULL)
	{
		GlobalUnlock(ConnectionsPosGlobal);
		GlobalFree(ConnectionsPosGlobal);
		ConnectionsPosGlobal = NULL;
		MaxNrConnectionsPos = 0;
	}

	for (cnt = 31; cnt >= 0; cnt--)
	{
		if (NetsPosGlobal[cnt] != NULL)
		{
			GlobalUnlock(NetsPosGlobal[cnt]);
			GlobalFree(NetsPosGlobal[cnt]);
			NetsPosGlobal[cnt] = NULL;
			MaxNrNetsPos[cnt] = 0;
		}
	}

	if (NetsGlobal != NULL)
	{
		GlobalUnlock(NetsGlobal);
		GlobalFree(NetsGlobal);
		NetsGlobal = NULL;
		MaxNrNets = 0;
	}

	if (ViasGlobal != NULL)
	{
		GlobalUnlock(ViasGlobal);
		GlobalFree(ViasGlobal);
		ViasGlobal = NULL;
		MaxNrVias = 0;
	}

	if (AreaFillsGlobal != NULL)
	{
		GlobalUnlock(AreaFillsGlobal);
		GlobalFree(AreaFillsGlobal);
		AreaFillsGlobal = NULL;
		MaxNrAreaFills = 0;
	}

	if (ShapesGlobal != NULL)
	{
		GlobalUnlock(ShapesGlobal);
		GlobalFree(ShapesGlobal);
		ShapesGlobal = NULL;
		MaxNrShapes = 0;
	}

	if (CompsGlobal != NULL)
	{
		GlobalUnlock(CompsGlobal);
		GlobalFree(CompsGlobal);
		CompsGlobal = NULL;
		MaxNrComps = 0;
	}

	for (Layer = 31; Layer >= 0; Layer--)
	{
		DeAllocateMemDiag2Traces(Layer);
		DeAllocateMemDiag1Traces(Layer);
		DeAllocateMemVerTraces(Layer);
		DeAllocateMemHorTraces(Layer);
	}

	memset(&Design, 0, sizeof(DesignRecord));

	/***********************************************************************************************/


}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemObjects4()
{
	if (Objects4Global != NULL)
	{
		GlobalUnlock(Objects4Global);
		GlobalFree(Objects4Global);
		MaxNrObjects4 = 0;
		Objects4Global = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemObjects5()
{
	if (Objects5Global != NULL)
	{
		GlobalUnlock(Objects5Global);
		GlobalFree(Objects5Global);
		MaxNrObjects5 = 0;
		Objects5Global = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemObjects6()
{
	if (Objects6Global != NULL)
	{
		GlobalUnlock(Objects6Global);
		GlobalFree(Objects6Global);
		MaxNrObjects6 = 0;
		Objects6Global = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemErrorObjects()
{
	if (ErrorObjectsGlobal != NULL)
	{
		GlobalUnlock(ErrorObjectsGlobal);
		GlobalFree(ErrorObjectsGlobal);
		MaxNrErrorObjects = 0;
		ErrorObjectsGlobal = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void DeAllocateMemPolygons()
{
	int32 cnt;

	for (cnt = 3; cnt >= 0; cnt--)
	{
		if (WorkPolygonGlobal[cnt] != NULL)
		{
			GlobalUnlock(WorkPolygonGlobal[cnt]);
			GlobalFree(WorkPolygonGlobal[cnt]);
			WorkPolygonGlobal[cnt] = NULL;
		}
	}

	if (NewPolygonGlobal != NULL)
	{
		GlobalUnlock(NewPolygonGlobal);
		GlobalFree(NewPolygonGlobal);
		NewPolygonGlobal = NULL;
	}

	if (ExtraPolygonGlobal != NULL)
	{
		GlobalUnlock(ExtraPolygonGlobal);
		GlobalFree(ExtraPolygonGlobal);
		ExtraPolygonGlobal = NULL;
	}

	if (ResultPolygonGlobal != NULL)
	{
		GlobalUnlock(ResultPolygonGlobal);
		GlobalFree(ResultPolygonGlobal);
		ResultPolygonGlobal = NULL;
	}

	if (BufferPolygonGlobal != NULL)
	{
		GlobalUnlock(BufferPolygonGlobal);
		GlobalFree(BufferPolygonGlobal);
		BufferPolygonGlobal = NULL;
		MaxNrVerticesPolygon = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemAreaFills()
{
	if (AreaFillMemoryTemp2Global != NULL)
	{
		GlobalUnlock(AreaFillMemoryTemp2Global);
		GlobalFree(AreaFillMemoryTemp2Global);
		AreaFillMemoryTemp2Global = NULL;
		MaxAreaFillMemoryTemp = 0;
	}

	if (AreaFillMemoryTempGlobal != NULL)
	{
		GlobalUnlock(AreaFillMemoryTempGlobal);
		GlobalFree(AreaFillMemoryTempGlobal);
		AreaFillMemoryTempGlobal = NULL;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


int32 AllocateMemNetItems(int32 count)
{
	HGLOBAL NewMem;

	if (count > 128 * 1024)
		return -2;

	if (MaxNrNetItems == 0)
	{
		count = max(count, 256);

		if ((NetItemsGlobal = GlobalAlloc(GHND, count * sizeof(NetItemsRecord))) == NULL)
			return -1;

		if ((NetItems = (NetItemsArray *) GlobalLock(NetItemsGlobal)) == NULL)
			return -1;

		MaxNrNetItems = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(NetItemsGlobal, count * sizeof(NetItemsRecord), GHND)) == NULL)
			return -1;

		NetItemsGlobal = NewMem;

		if ((NetItems = (NetItemsArray *) GlobalLock(NetItemsGlobal)) == NULL)
			return -1;

		MaxNrNetItems = count;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemNetItems2(int32 count)
{
	HGLOBAL NewMem;

	if (count > 128 * 1024)
		return -2;

	if (MaxNrNetItems2 == 0)
	{
		count = max(count, 256);

		if ((NetItems2Global = GlobalAlloc(GHND, count * sizeof(NetItemsRecord))) == NULL)
			return -1;

		if ((NetItems2 = (NetItemsArray *) GlobalLock(NetItems2Global)) == NULL)
			return -1;

		MaxNrNetItems2 = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(NetItems2Global, count * sizeof(NetItemsRecord), GHND)) == NULL)
			return -1;

		NetItems2Global = NewMem;

		if ((NetItems2 = (NetItemsArray *) GlobalLock(NetItems2Global)) == NULL)
			return -1;

		MaxNrNetItems2 = count;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMemNetItems()
{
	if (NetItemsGlobal != NULL)
	{
		GlobalUnlock(NetItemsGlobal);
		GlobalFree(NetItemsGlobal);
		NetItemsGlobal = NULL;
		MaxNrNetItems = 0;
	}

	if (NetItems2Global != NULL)
	{
		GlobalUnlock(NetItems2Global);
		GlobalFree(NetItems2Global);
		NetItems2Global = NULL;
		MaxNrNetItems2 = 0;
	}

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

void DeAllocateMemMessageBuf()
{
	if (MessageBufGlobal != NULL)
	{
		GlobalUnlock(MessageBufGlobal);
		GlobalFree(MessageBufGlobal);
	}

	MessageBufMemSize = 0;
	MessageBufGlobal = NULL;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddToMessageBuf(LPSTR Line)
{
	int32 lengte;
#ifdef _DEBUG
	int32 ok;
#endif

	lengte = strlen(Line);
#ifdef _DEBUG

	if (stricmpOwn(Line, "-0.571 w") == 0)
		ok = 1;

#endif

	if (lengte + 2 + MessageBufPos >= MessageBufMemSize)
	{
		if (AllocateMemMessageBuf(MessageBufMemSize + 256 * 1024) != 0)
			return -1;
	}

	if (MessageBufPos == 0)
		MessageBuf[0] = 0;

	if (lengte > 0)
		strcat((LPSTR) & MessageBuf[MessageBufPos], Line);

	strcat((LPSTR) & MessageBuf[MessageBufPos], "\r\n");
	MessageBufPos += lengte + 2;
	return 0;
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 CopyStrToClipboard(LPSTR StrToCopy)
{
	HANDLE hGlobalMemory;
	LPSTR ClipBuf;
	int32 Length;

	Length = strlen(StrToCopy);

	if (OpenClipboard(PCBWindow))
	{
		hGlobalMemory = GlobalAlloc(GHND, max(16 * 1024, Length + 1024));
		ClipBuf = GlobalLock(hGlobalMemory);
		strcpy(ClipBuf, StrToCopy);
		GlobalUnlock(hGlobalMemory);
		EmptyClipboard();
		SetClipboardData(CF_TEXT, hGlobalMemory);
		CloseClipboard();
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeAllocateMem()
{
	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();
	DeAllocateMemDesign();
	DeAllocateMemMessageBuf();
	DeAllocateMemObjects4();
	DeAllocateMemErrorObjects();
	DeAllocateMemTemp();

	if (ObjectsGlobal != NULL)
	{
		GlobalUnlock(ObjectsGlobal);
		GlobalFree(ObjectsGlobal);
		MaxNrObjects = 0;
		ObjectsGlobal = NULL;
	}

	if (Objects2Global != NULL)
	{
		GlobalUnlock(Objects2Global);
		GlobalFree(Objects2Global);
		MaxNrObjects2 = 0;
		Objects2Global = NULL;
	}

	if (Objects3Global != NULL)
	{
		GlobalUnlock(Objects3Global);
		GlobalFree(Objects3Global);
		MaxNrObjects3 = 0;
		Objects3Global = NULL;
	}

	GlobalUnlock(CharsGlobal);
	GlobalFree(CharsGlobal);

	if (GlobalClipBoardMem != NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		GlobalFree(GlobalClipBoardMem);
		ClipBoardMemSize = 0;
		ClipBoardMemPos = 0;
		GlobalClipBoardMem = NULL;
	}

	DeAllocateMemNetItems();
	DeallocateMemObjectTextBuf();
	DeallocateMemObjectTextBuf();
	/***********************************************************************************************/

	DeleteGraphicObjects();
}



// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SaveMemory()
{
	LPSTR BufP = (LPSTR) 0x400000;

	int fp, result;

	fp = FileOpenWriteUTF8("pcb.bin");

	if (fp >= 0)
	{
		FileWrite(fp, BufP, 0x133 * 4096, &result);
		FileClose(fp);
	}

}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void MemoryMain()
{
	int32 cnt;

	DebugTest1 = 0x21436587;
#ifdef _DEBUG
	DrawTraceLimitInfo = 1;
#endif
	OkToAddViewPos = 1;
	ReplaceSelections = 1;

	OkToDrawAreaFills = 1;
	OkToDrawAreaFillWithHatches = 1;
	OkToDrawCompOutline = 1;
	OkToDrawConnections = 1;
	OkToDrawInnerPads = 1;
	OkToDrawTopPads = 1;
	OkToDrawBottomPads = 1;
	OkToDrawCompPlacement = 1;
	OkToDrawPowerPlanes = 1;
	OkToDrawSilkScreenTop = 1;
	OkToDrawObjects = 1;
	OkToDrawInfoObjects = 1;
	OkToDrawBoardOutline = 1;
	OkToDrawRoutingKeepoutTop = 1;
	OkToDrawRoutingKeepoutBottom = 1;
	OkToDrawRoutingKeepoutInner = 1;

	OkToDrawInfo2Objects = 1;
	OkToDrawInfo3Objects = 1;
	OkToDrawInfo4Objects = 1;
	OkToDrawVias = 1;
	OkToDrawCompReference = 1;
	RecalcAreafillAfterInsert = 1;

	DrawSoldMaskBottomMode = 1;
	DrawSoldMaskTopMode = 1;
	DrawPasteMaskBottomMode = 1;
	DrawPasteMaskTopMode = 1;
	GridVisible = 1;
	DrawTwoTryingTraces = 1;
	FirstPaint = 1;
	ComponentConnectionMode = 1;

	SelectionMode = MOVE_COMPONENTS_MODE;

	GridSize = (25.0 * 2540.0);
	UserGridSize = (25.0 * 2540.0);
	ScrollSize = 150;
	ScrollSizeDrawing = ScrollSize;
	ScrollEndOfWindow = 40;
	MouseOnGridPosX = -1000000000;
	MouseOnGridPosY = -1000000000;
	CurrentObjectCode = -1;
	CurrentDrawingLayer = -1;
	DrawDrillMode = 3;
	LastActionNr = 1;
	MaxLastActionNr = 1;
	CurrentErrorNr = -2;
	MousePosX = 10000;
	MousePosY = 10000;
	SnapMode = 1;
	CompSnapMode = 1;
	MoveCompAutoZoom = 1;

	LastUsedDrawingLayers[0] = -1;
	LastUsedDrawingLayers[1] = -1;
	CurrentGuideNr = -1;
	GlobalClipBoardMem = NULL;

	MousePanMultiply = 2;

	GeometryDialogInitialX = -1;
	GeometryDialogInitialY = -1;
	ZoomMode = 0;
	MousePanMultiply = 2;
	CurrentVia.ThickNess = (32.0 * 2540.0);
	CurrentVia.DrillThickNess = (18.0 * 2540.0);
	CurrentVia.Clearance = (6.0 * 2540.0);
	CurrentVia.ThermalInner = (32.0 * 2540.0);
	CurrentTraceWidth = (6.0 * 2540.0);
	CurrentClearance = (6.0 * 2540.0);

	TraceWidthUser[0] = (8.0 * 2540.0);
	TraceWidthUser[1] = (6.0 * 2540.0);
	TraceWidthUser[2] = (6.0 * 2540.0);
	TraceWidthUser[3] = (6.0 * 2540.0);

	ClearanceWidthUser[0] = (8.0 * 2540.0);
	ClearanceWidthUser[1] = (6.0 * 2540.0);
	ClearanceWidthUser[2] = (6.0 * 2540.0);
	ClearanceWidthUser[3] = (6.0 * 2540.0);


	GridSizes[0] = 0.5 * 2540.0;
	GridSizes[1] = 1.0 * 2540.0;
	GridSizes[2] = 2.0 * 2540.0;
	GridSizes[3] = 5.0 * 2540.0;
	GridSizes[4] = 10.0 * 2540.0;
	GridSizes[5] = 12.0 * 2540.0;
	GridSizes[6] = 12.5 * 2540.0;
	GridSizes[7] = 20.0 * 2540.0;
	GridSizes[8] = 25.0 * 2540.0;
	GridSizes[9] = 50.0 * 2540.0;
	GridSizes[10] = 100.0 * 2540.0;
	GridSizes[11] = 200.0 * 2540.0;
	GridSizes[12] = 10.0 * 100.0;
	GridSizes[13] = 20.0 * 100.0;
	GridSizes[14] = 50.0 * 100.0;
	GridSizes[15] = 100.0 * 100.0;
	GridSizes[16] = 200.0 * 100.0;
	GridSizes[17] = 500.0 * 100.0;
	GridSizes[18] = 1000.0 * 100.0;
	GridSizes[19] = 2000.0 * 100.0;
	GridSizes[20] = 5000.0 * 100.0;
	NrGridSizes = 21;

	TraceWidths[0] = (6.0 * 2540.0);
	TraceWidths[1] = (8.0 * 2540.0);
	TraceWidths[2] = (10.0 * 2540.0);
	TraceWidths[3] = (12.0 * 2540.0);
	TraceWidths[4] = (12.5 * 2540.0);
	TraceWidths[5] = (20.0 * 2540.0);
	TraceWidths[6] = (25.0 * 2540.0);
	TraceWidths[7] = (30.0 * 2540.0);
	TraceWidths[8] = (40.0 * 2540.0);
	TraceWidths[9] = (50.0 * 2540.0);
	TraceWidths[10] = (60.0 * 2540.0);
	TraceWidths[11] = (80.0 * 2540.0);
	TraceWidths[12] = (100.0 * 2540.0);
	NrTraceWidths = 13;

	TraceGridSize = 25 * 2540.0;
	AreafillGridSize = 5 * 2540.0;
	CompGridSize = 5 * 2540.0;

	ClearanceWidths[0] = (6.0 * 2540.0);
	ClearanceWidths[1] = (8.0 * 2540.0);
	ClearanceWidths[2] = (10.0 * 2540.0);
	ClearanceWidths[3] = (12.0 * 2540.0);
	ClearanceWidths[4] = (12.5 * 2540.0);
	ClearanceWidths[5] = (20.0 * 2540.0);
	ClearanceWidths[6] = (25.0 * 2540.0);
	ClearanceWidths[7] = (30.0 * 2540.0);
	ClearanceWidths[8] = (40.0 * 2540.0);
	ClearanceWidths[9] = (50.0 * 2540.0);
	ClearanceWidths[10] = (60.0 * 2540.0);
	ClearanceWidths[11] = (80.0 * 2540.0);
	ClearanceWidths[12] = (100.0 * 2540.0);
	NrClearanceWidths = 13;

	NrGraphicsLayers = 0;
	GraphicsLayers[NrGraphicsLayers++] = SOLD_MASK_BOTTOM;
	GraphicsLayers[NrGraphicsLayers++] = SOLD_MASK_TOP;
	GraphicsLayers[NrGraphicsLayers++] = PASTE_MASK_BOTTOM;
	GraphicsLayers[NrGraphicsLayers++] = PASTE_MASK_TOP;
	GraphicsLayers[NrGraphicsLayers++] = BACKGROUND_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = SWAP_PINS_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = SWAP_GATE_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = INFO_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = BOARD_OUTLINE_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = SILKSCREEN_BOTTOM;
	GraphicsLayers[NrGraphicsLayers++] = SILKSCREEN_TOP;
	GraphicsLayers[NrGraphicsLayers++] = COMP_REF_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = COMP_VALUE_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = PLACEMENT_OUTLINE_TOP;
	GraphicsLayers[NrGraphicsLayers++] = PLACEMENT_OUTLINE_BOTTOM;
	GraphicsLayers[NrGraphicsLayers++] = PLACEMENT_OUTLINE_TOP2;
	GraphicsLayers[NrGraphicsLayers++] = PLACEMENT_OUTLINE_BOTTOM2;
	GraphicsLayers[NrGraphicsLayers++] = CONNECTIONS_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = NET_PINS_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = NET_PINS_LAYER2;
	GraphicsLayers[NrGraphicsLayers++] = COMP_OUTLINE_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = FIXED_COLOR_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = CLEARANCE_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = ERROR_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = WARNING_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = DRILL_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = DRILL_UNPLATED_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = VIA_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = VIA_DRILL_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = BUTTON_INFO_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = POLYGON_DRAW_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = SPECIALS_LAYER;
	GraphicsLayers[NrGraphicsLayers++] = INFO_LAYER2;
	GraphicsLayers[NrGraphicsLayers++] = INFO_LAYER3;
	GraphicsLayers[NrGraphicsLayers++] = INFO_LAYER4;

	for (cnt = 0; cnt < 32; cnt++)
		GraphicsLayers[NrGraphicsLayers++] = cnt;

	for (cnt = 0; cnt < 32; cnt++)
		GraphicsLayers[NrGraphicsLayers++] = ROUTING_KEEPOUT_LAYER + cnt;

	MinDrillForThermalRelief = (60000);

	VisibleMinX = -10e5;
	VisibleMinY = -10e5;
	VisibleMaxX = 10e5;
	VisibleMaxY = 10e5;

	GraphicsMain();
	AllocateMem();

	for (cnt = 0; cnt < 32; cnt++)
	{
		DrawLayerCode[cnt] = 0;
		TracesSelected[cnt] = 0;
	}

	AllTracesSelected = 0;

	memset(&HorTracesSelected, 0, sizeof(HorTracesSelected));
	memset(&VerTracesSelected, 0, sizeof(VerTracesSelected));
	memset(&Diag1TracesSelected, 0, sizeof(Diag1TracesSelected));
	memset(&Diag2TracesSelected, 0, sizeof(Diag2TracesSelected));

	memset(&TraceLayersSelectable[0], 0xff, sizeof(TraceLayersSelectable));
	Design.BoardOriginX = (1000 * 2540.0);
	Design.BoardOriginY = (1000 * 2540.0);
	Design.BoardWidth = (200 * 100000.0);
	Design.BoardHeight = (100 * 100000.0);


	NewDesign.BoardOriginX = (1000 * 2540.0);
	NewDesign.BoardOriginY = (1000 * 2540.0);
	NewDesign.BoardWidth = (200 * 100000.0);
	NewDesign.BoardHeight = (100 * 100000.0);
	NewDesign.StandardTraceWidth = (8 * 2540.0);
	NewDesign.StandardClearance = (8 * 2540.0);
	NewDesign.MaximumClearance = (8 * 2540.0);
	NewDesign.SilkScreenWidth = (8 * 2540.0);
	NewDesign.BoardOutlineWidth = (8 * 2540.0);
	NewDesign.NrBoardLayers = 2;
	NewDesign.ArrowLength = ARROW_LENGTH;

	CurrentVia.ThickNess = (50 * 2540.0);
	CurrentVia.DrillThickNess = (0.7 * 100000.0);
	CurrentVia.Clearance = (8 * 2540.0);
	CurrentVia.ThermalInner = (50 * 2540.0);
	CurrentVia.SoldMask = (54 * 2540.0);

	GerberInfo.GerberNumberMode = 0;
	GerberInfo.GerberOutputMode = 1;
	GerberInfo.GerbvProject = 1;
	GerberInfo.ReverseLayerNumbering = 1;
	GerberInfo.AutoApertureGeneration = 1;
	GerberInfo.PenSizes[0] = 30000.0;
	GerberInfo.NrPlotPens = 1;
	GerberInfo.PenSpeed = 20.0;
	GerberInfo.PlotMode = 0;
	GerberInfo.PlotBoardOutline = 0;
	GerberInfo.DrillOutputOption = 1;	// Include drill tools
	GerberInfo.AreaFillPen1 = (8 * 2540.0);
	GerberInfo.AreaFillPen2 = (8 * 2540.0);
	GerberInfo.BitmapExportResolution = (2540000.0 / 600);	// 600 dpi

	PDFInfo.PaperSize = PAPERSIZE_A4;
	PDFInfo.PaperOrientation = ORIENTATION_AUTO;

	Units = 1; //UNITS_MM
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
