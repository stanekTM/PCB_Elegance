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
#include "geom.h"
#include "string.h"
#include "graphics.h"


#define LimitMaxNrObjects               32768
#define LimitMaxNrPinObjects            4096
#define LimitMaxNrPinObjectInfos        4096
#define LimitMaxObjectPolygonMemory     4096*1024
#define LimitMaxNrObjectPolygons        4096
#define LimitMaxAreaFillMemory          16384*1024

double SearchMinX, SearchMinY, SearchMaxX, SearchMaxY, VisibleMinX, VisibleMinY, VisibleMaxX, VisibleMaxY,
       CurrentClearance, TraceThickness, CurrentSilkscreenLine, CurrentBoardOutLine, CurrentTextHeight, CurrentInfoLine,
       CurrentCompOutLine, TraceWidths[32], TraceClearances[32], GridSizes[32], SilkscreenLines[32], CompOutLines[32],
       BoardOutLines[32], InfoLines[32];

int32 WindowWidth, WindowHeight, MousePosX, MousePosY, WindowStartX, WindowStartY, MaxScrollBarX, MaxScrollBarY,
      ZoomInOutProcessed, ScrollSize, ScrollSizeDrawing, SelectionMode, LastActionNr, ScrollEndOfWindow, DrawDrillMode,
      MaxLastActionNr, ZoomMode, ClientStartX, ClientStartY, SelectRectX1, SelectRectY1, SelectRectX2, SelectRectY2;


int32 ClientWindowDivX, ClientWindowDivY, CrossHairType, SystemBusyMode, NrPadLayers, CrossHairVisible, MaxNrObjects,
      NrObjects, MaxNrObjects2, NrObjects2, MaxNrPinObjects, NrPinObjects, NrTraceWidths, NrTraceClearances,
      MessageBufMemSize, NrSilkscreenLines, NrCompOutLines, NrGridSizes, NrBoardOutLines, NrInfoLines, NrObjectPolygons,
      MaxNrObjectPolygons, MaxSpecialMemory[32], ObjectPolygonMemorySize, SnapMode, NrMappableObjectPolygons,
      MappableObjectPolygonStart, MessageBufPos, NrPinObjectsInfo, MaxShapesMem, NrPadLayerObjectsSelected[32],
      NrDrillsSelected, NrDrillsUnplatedSelected, NrAntiPowerpadsSelected, NrPadsInnerSelected, NrSilkTopObjectsSelected,
      NrSilkBottomObjectsSelected, NrCompOutlinesSelected, NrPlacemOutlinesSelected, NrMaskTopObjectsSelected,
      NrMaskBottomObjectsSelected, NrPasteTopObjectsSelected, NrPasteBottomObjectsSelected, NrBoardOutlinesSelected,
      NrInfo1ObjectsSelected, NrInfo2ObjectsSelected, NrInfo3ObjectsSelected, NrInfo4ObjectsSelected, NrLinesSelected,
      NrRectsSelected, NrCirclesSelected, NrArcsSelected, NrTextsSelected, NrPolygonsSelected, GeomNameSelected,
      NrRoutingKeepoutsSelected[32];


int32 Created, Painting, DCInUse, MouseChanged, Focused, LeftButtonPressed, MiddleButtonPressed, RightButtonPressed,
      Printing, UnselectAll, LeftButtonDoublePressed, DataBaseChanged, FileChanged, FirstPaint, MouseCursorOnGrid,
      BackGroundActive, DrawOnGrid, GridVisible, FinishPolygon, SelectionActive, SelectionEsc, OkToAddViewPos,
      UndoRedoActive, SpacePressed, ParametersRelative, ReplaceSelections, OkToUseSharedMemory, PadsVisible[32],
      RoutingKeepoutVisible[32], PastePadsTopVisible, SoldMaskPadsTopVisible, PastePadsBottomVisible,
      SoldMaskPadsBottomVisible, PlacementVisible, CompOutlineVisible, PowerPadsVisible, PinNamesVisible,
      InnerPadsVisible, SilkScreenTopVisible, SilkScreenBottomVisible, BoardOutlineVisible, Info1Visible, Info2Visible,
      Info3Visible, Info4Visible, ClearanceVisible, DrillVisible, GeomNameVisible, DrillUnplatedVisible,
      ViewInsertionPoint;

ObjectRecord NewObject;

int32 ReverseY = 1;
int32 HelpAsked = 0;

double Xoffset, Yoffset, Factor, RelX, RelY, GridSize, BoardWidth, BoardHeight, BoardOX, BoardOY, TextMinX, TextMinY,
       TextMaxX, TextMaxY, CenterMoveX, CenterMoveY, DimensionHeight, ArrowLength;
float ParamsFloat[32];

int32 FirstSize, NrObjects, NrObjects2, CurrentObjectCode, Units, CurrentFontCode, ClipBoardMemSize, ClipBoardMemPos,
      MaxNrObjectPolygons, MaxObjectPolygonMemory, MaxNrPolygon8Objects, MaxTempMemory, MaxTempMemory2,
      MaxAreaFillMemoryTemp, MaxNrVerticesPolygon, ViewPosPointer, GEOMSystemError, RepeatMode, LastAction,
      MousePanMultiply, ParamsInt[8], RepeatModeBusy;

int32 Debugfp, OperatingSystem;
uint32 ClosingWindowMessage;

ViewPosArray ViewPos;

double DisplX, DisplY;

uint8 *SharedMemory, *AreaFillMemTemp, *AreaFillMemTemp2, *SpecialMem[32];
HANDLE *SharedMemoryHandle;

char AbsPosStr[MAX_LENGTH_STRING];
char RelPosStr[MAX_LENGTH_STRING];
char AbsGridPosStr[MAX_LENGTH_STRING];
char InfoStr[MAX_LENGTH_STRING];
char DesignPath[MAX_LENGTH_STRING];
char ExePath[MAX_LENGTH_STRING];
char EditFile[MAX_LENGTH_STRING];
uint8 *ShapesMem, *MessageBuf;

DesignRecord Design;

ShapeRecord Shape;

ObjectArray *Objects, *Objects2;
PinInfoArray *PinInfos;
CharsArray *Chars;
AreaFillRecord *NewAreaFill, *TempAreaFill;

ObjectPolygonsArray *ObjectPolygons;
uint8 *ObjectPolygonMem, *TempMem, *TempMem2;
ObjectPolygonRecord *NewObjectPolygon;

PolygonRecord *BufferPolygon, *ResultPolygon, *ExtraPolygon, *NewPolygon, *WorkPolygon[8];

GeomCreateRecord BGAGeom, PGAGeom, SOICGeom, DILGeom, SILGeom, SIL_SMD_RECTGeom, SIL_SMD_CIRCLEGeom, QUADGeom;

HGLOBAL GlobalClipBoardMem;
uint8 *ClipBoardMem;

HGLOBAL ObjectsGlobal, Objects2Global, PinObjectsGlobal, PinObjectsInfoGlobal, ShapesMemGlobal, MessageBufGlobal,
        ObjectPolygonMemGlobal, ObjectPolygonsGlobal, BufferPolygonGlobal, ResultPolygonGlobal, ExtraPolygonGlobal,
        NewPolygonGlobal, GlobalSpecialMem[32], GlobalTempMem, GlobalTempMem2, AreaFillMemoryTempGlobal,
        AreaFillMemoryTemp2Global, WorkPolygonGlobal[8], CharsGlobal;

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

uint8 Buf[256 * 1024];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 NrGeomNames, GeomNamesPos;
LPSTR GeomNamesId[MaxNrGeomNames];


char GeomNamesBuf[GeomNamesBufSize];

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

	if ((Nr < 0) || (Nr >= MaxNrGeomNames) || (GeomNamesId[Nr] == 0))
		return str;

	strcpy(str1, str);
	strcpy(str2, GeomNamesId[Nr]);

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

	return GeomNamesId[Nr];
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void AllocateMem()
{
	int32 count, count2;
	int32 error;

	error = 0;
	count = 0;
	count2 = 0;

	if ((!error) && ((CharsGlobal = GlobalAlloc(GHND, (int32) sizeof(CharsArray))) == NULL))
		error = 1;

	if ((!error) && ((Chars = (CharsArray *) GlobalLock(CharsGlobal)) == NULL))
		error = 1;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemObjects(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjects)
		return -2;

	if (MaxNrObjects == 0)
	{
		count = max(256, count);

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 AllocateMemObjects2(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrObjects)
		return -2;

	if (MaxNrObjects2 == 0)
	{
		count = max(256, count);

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemPinObjects(int32 count)
{
	HGLOBAL NewMem;

	if (count > LimitMaxNrPinObjects)
		return -2;

	if (MaxNrPinObjects == 0)
	{
		count = max(64, count);

		if ((PinObjectsGlobal = GlobalAlloc(GHND, count * sizeof(PinInfoRecord))) == NULL)
			return -1;

		if ((PinInfos = (PinInfoArray *) GlobalLock(PinObjectsGlobal)) == NULL)
			return -1;

		MaxNrPinObjects = count;
	}
	else
	{
		if (count > MaxNrPinObjects)
		{
			if ((NewMem = GlobalReAlloc(PinObjectsGlobal, count * sizeof(PinInfoRecord), GHND)) == NULL)
				return -1;

			PinObjectsGlobal = NewMem;

			if ((PinInfos = (PinInfoArray *) GlobalLock(PinObjectsGlobal)) == NULL)
				return -1;

			MaxNrPinObjects = count;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemShapes(int32 ShapesMemSize)
{
	HGLOBAL NewMem;

	if (MaxShapesMem == 0)
	{
		ShapesMemSize = max(ShapesMemSize, 65536);

		if ((ShapesMemGlobal = GlobalAlloc(GHND, ShapesMemSize)) == NULL)
			return -1;

		if ((ShapesMem = (uint8 *) GlobalLock(ShapesMemGlobal)) == NULL)
			return -1;

		MaxShapesMem = ShapesMemSize;
	}
	else
	{
		if (ShapesMemSize > MaxShapesMem)
		{
			if ((NewMem = GlobalReAlloc(ShapesMemGlobal, ShapesMemSize, GHND)) == NULL)
				return -1;

			ShapesMemGlobal = NewMem;

			if ((ShapesMem = (uint8 *) GlobalLock(ShapesMemGlobal)) == NULL)
				return -1;

			MaxShapesMem = ShapesMemSize;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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
		if (count > MaxNrObjectPolygons)
		{
			if ((NewMem = GlobalReAlloc(ObjectPolygonsGlobal, count * sizeof(int32), GHND)) == NULL)
				return -1;

			ObjectPolygonsGlobal = NewMem;

			if ((ObjectPolygons = (ObjectPolygonsArray *) GlobalLock(ObjectPolygonsGlobal)) == NULL)
				return -1;

			MaxNrObjectPolygons = count;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemPolygons(int32 NrVerticesPolygon)
{
	int32 cnt, MemSize;
	HGLOBAL NewMem;

	if (MaxNrVerticesPolygon == 0)
	{
		NrVerticesPolygon = max(512, NrVerticesPolygon);
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

		for (cnt = 0; cnt < 4; cnt++)
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

			for (cnt = 0; cnt < 4; cnt++)
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
		if (MemSize > MaxTempMemory)
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
		if (MemSize > MaxTempMemory2)
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemAreaFillMemoryTemp(int32 count)
{
	HGLOBAL NewMem;
	DWORD Error, Size;

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
//      Size2=GlobalSize(AreaFillMemoryTemp2Global);
//      Size=GlobalSize(AreaFillMemoryTempGlobal);
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DeAllocateMemShapes()
{
	GlobalUnlock(ShapesMemGlobal);
	GlobalFree(ShapesMemGlobal);
	MaxShapesMem = 0;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemMessageBuf(int32 MemSize)
{
	HGLOBAL NewMem;

	if (MessageBufMemSize == 0)
	{
		MemSize = max(64 * 1024, MemSize);

		if ((MessageBufGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((MessageBuf = (uint8 *) GlobalLock(MessageBufGlobal)) == NULL)
			return -1;

		MessageBufMemSize = MemSize;
	}
	else
	{
		if (MemSize > MessageBufMemSize)
		{
			if ((NewMem = GlobalReAlloc(MessageBufGlobal, MemSize, GHND)) == NULL)
				return -1;

			MessageBufGlobal = NewMem;

			if ((MessageBuf = (uint8 *) GlobalLock(MessageBufGlobal)) == NULL)
				return -1;

			MessageBufMemSize = MemSize;
		}
	}

//  MaxNrObjects=Count;
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMemMessageBuf()
{
	if (MessageBufGlobal != NULL)
	{
		GlobalUnlock(MessageBufGlobal);
		GlobalFree(MessageBufGlobal);
	}

	MessageBufMemSize = 0;
	MessageBufPos = 0;
	MessageBufGlobal = NULL;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMemGeometrie()
{
	if (PinObjectsGlobal != NULL)
	{
		GlobalUnlock(PinObjectsGlobal);
		GlobalFree(PinObjectsGlobal);
		PinObjectsGlobal = NULL;
		NrPinObjects = 0;
		MaxNrPinObjects = 0;
	}

	if (Objects2Global != NULL)
	{
		GlobalUnlock(Objects2Global);
		GlobalFree(Objects2Global);
		Objects2Global = NULL;
		NrObjects2 = 0;
		MaxNrObjects2 = 0;
	}

	if (ObjectsGlobal != NULL)
	{
		GlobalUnlock(ObjectsGlobal);
		GlobalFree(ObjectsGlobal);
		ObjectsGlobal = NULL;
		NrObjects = 0;
		MaxNrObjects = 0;
	}

	if (MessageBufGlobal != NULL)
	{
		GlobalUnlock(MessageBufGlobal);
		GlobalFree(MessageBufGlobal);
		MessageBufGlobal = NULL;
		MessageBufMemSize = 0;
	}
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
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMem()
{
	DeAllocateMemGeometrie();
	GlobalUnlock(CharsGlobal);
	GlobalFree(CharsGlobal);
	DeleteGraphicObjects();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void MemoryMain()
{
	int32 cnt;

	Created = 0;
	MouseChanged = 0;
	Painting = 0;
	DCInUse = 0;
	SelectionActive = 0;
	SelectionEsc = 0;
	UnselectAll = 0;
	FileChanged = 0;


	DataBaseChanged = 0;
	GridVisible = 1;
	MouseCursorOnGrid = 0;
	OkToAddViewPos = 1;
	OkToUseSharedMemory = 0;
	Printing = 0;
	SpacePressed = 0;
	ReplaceSelections = 1;
	ViewInsertionPoint = 0;
	BackGroundActive = 0;

	for (cnt = 0; cnt < 32; cnt++)
	{
		PadsVisible[cnt] = 1;
		RoutingKeepoutVisible[cnt] = 1;
	}

	NrPadLayers = 2;
	PastePadsTopVisible = 1;
	PastePadsBottomVisible = 1;
	SoldMaskPadsTopVisible = 1;
	SoldMaskPadsBottomVisible = 1;
	PlacementVisible = 1;
	CompOutlineVisible = 1;
	PowerPadsVisible = 1;
	PinNamesVisible = 1;
	InnerPadsVisible = 1;
	SilkScreenTopVisible = 1;
	SilkScreenBottomVisible = 1;
	BoardOutlineVisible = 1;
	Info1Visible = 1;
	Info2Visible = 1;
	Info3Visible = 1;
	Info4Visible = 1;
	FinishPolygon = 0;

	ClearanceVisible = 0;
	DrillVisible = 1;
	GeomNameVisible = 1;
	DrillUnplatedVisible = 1;
	FirstPaint = 1;
	ViewInsertionPoint = 0;
	UndoRedoActive = 0;
	ZoomMode = 0;
	SelectionMode = 1;
	GEOMSystemError = 0;
	RepeatMode = 0;
	RelX = 0.0;
	RelY = 0.0;
	GridSize = (200.0 * 100.0);
	ScrollSize = 100;
	ScrollSizeDrawing = 40;
	ScrollEndOfWindow = 40;
	DrawDrillMode = 0;
	LastActionNr = 1;
	MaxLastActionNr = 1;
	LastAction = 0;
	AbsPosStr[0] = 0;
	AbsGridPosStr[0] = 0;
	RelPosStr[0] = 0;
	InfoStr[0] = 0;
	Units = 0;
	ViewPosPointer = 0;
	CrossHairType = 0;
	SystemBusyMode = 0;
	SnapMode = 0;
	DimensionHeight = DIMENSION_HEIGHT;
	ArrowLength = ARROW_LENGTH;

	MousePosX = 10000;
	MousePosY = 10000;

	NrObjects = 0;
	MaxNrObjects = 0;
	NrObjects2 = 0;
	MaxNrObjects2 = 0;
	MaxNrPinObjects = 0;
	NrPinObjects = 0;
	NrPinObjectsInfo = 0;
	MaxShapesMem = 0;
	MessageBufMemSize = 0;

	NrMappableObjectPolygons = 0;
	MappableObjectPolygonStart = 0;
	MousePanMultiply = 2;

	GridSizes[0] = (0.5 * 2540.0);
	GridSizes[1] = (1.0 * 2540.0);
	GridSizes[2] = (2.0 * 2540.0);
	GridSizes[3] = (5.0 * 2540.0);
	GridSizes[4] = (10.0 * 2540.0);
	GridSizes[5] = (20.0 * 2540.0);
	GridSizes[6] = (25.0 * 2540.0);
	GridSizes[7] = (50.0 * 2540.0);
	GridSizes[8] = (100.0 * 2540.0);
	GridSizes[9] = (200.0 * 2540.0);
	GridSizes[10] = (10.0 * 100.0);
	GridSizes[11] = (20.0 * 100.0);
	GridSizes[12] = (50.0 * 100.0);
	GridSizes[13] = (100.0 * 100.0);
	GridSizes[14] = (200.0 * 100.0);
	GridSizes[15] = (500.0 * 100.0);
	GridSizes[16] = (1000.0 * 100.0);
	GridSizes[17] = (2000.0 * 100.0);
	GridSizes[18] = (5000.0 * 100.0);
	NrGridSizes = 19;

	TraceWidths[0] = (2.0 * 2540.0);
	TraceWidths[1] = (4.0 * 2540.0);
	TraceWidths[2] = (6.0 * 2540.0);
	TraceWidths[3] = (8.0 * 2540.0);
	TraceWidths[4] = (10.0 * 2540.0);
	TraceWidths[5] = (12.0 * 2540.0);
	TraceWidths[6] = (12.5 * 2540.0);
	TraceWidths[7] = (20.0 * 2540.0);
	TraceWidths[8] = (30.0 * 2540.0);
	TraceWidths[9] = (40.0 * 2540.0);
	TraceWidths[10] = (50.0 * 2540.0);
	TraceWidths[11] = (60.0 * 2540.0);
	TraceWidths[12] = (80.0 * 2540.0);
	TraceWidths[13] = (100.0 * 2540.0);
	NrTraceWidths = 14;

	TraceClearances[0] = (4.0 * 2540.0);
	TraceClearances[1] = (5.0 * 2540.0);
	TraceClearances[2] = (6.0 * 2540.0);
	TraceClearances[3] = (7.0 * 2540.0);
	TraceClearances[4] = (8.0 * 2540.0);
	TraceClearances[5] = (9.0 * 2540.0);
	TraceClearances[6] = (10.0 * 2540.0);
	TraceClearances[7] = (11.0 * 2540.0);
	TraceClearances[8] = (12.0 * 2540.0);
	NrTraceClearances = 9;

	SilkscreenLines[0] = (4.0 * 2540.0);
	SilkscreenLines[1] = (5.0 * 2540.0);
	SilkscreenLines[2] = (6.0 * 2540.0);
	SilkscreenLines[3] = (7.0 * 2540.0);
	SilkscreenLines[4] = (8.0 * 2540.0);
	SilkscreenLines[5] = (9.0 * 2540.0);
	SilkscreenLines[6] = (10.0 * 2540.0);
	SilkscreenLines[7] = (11.0 * 2540.0);
	SilkscreenLines[8] = (12.0 * 2540.0);
	NrSilkscreenLines = 9;

	CompOutLines[0] = (4.0 * 2540.0);
	CompOutLines[1] = (5.0 * 2540.0);
	CompOutLines[2] = (6.0 * 2540.0);
	CompOutLines[3] = (7.0 * 2540.0);
	CompOutLines[4] = (8.0 * 2540.0);
	CompOutLines[5] = (9.0 * 2540.0);
	CompOutLines[6] = (10.0 * 2540.0);
	CompOutLines[7] = (11.0 * 2540.0);
	CompOutLines[8] = (12.0 * 2540.0);
	NrCompOutLines = 9;

	InfoLines[0] = (4.0 * 2540.0);
	InfoLines[1] = (5.0 * 2540.0);
	InfoLines[2] = (6.0 * 2540.0);
	InfoLines[3] = (7.0 * 2540.0);
	InfoLines[4] = (8.0 * 2540.0);
	InfoLines[5] = (9.0 * 2540.0);
	InfoLines[6] = (10.0 * 2540.0);
	InfoLines[7] = (11.0 * 2540.0);
	InfoLines[8] = (12.0 * 2540.0);
	NrInfoLines = 9;

	BoardOutLines[0] = (4.0 * 2540.0);
	BoardOutLines[1] = (5.0 * 2540.0);
	BoardOutLines[2] = (6.0 * 2540.0);
	BoardOutLines[3] = (7.0 * 2540.0);
	BoardOutLines[4] = (8.0 * 2540.0);
	BoardOutLines[5] = (9.0 * 2540.0);
	BoardOutLines[6] = (10.0 * 2540.0);
	BoardOutLines[7] = (11.0 * 2540.0);
	BoardOutLines[8] = (12.0 * 2540.0);
	NrBoardOutLines = 9;

	CurrentClearance = (8 * 2540.0);
	CurrentSilkscreenLine = (8 * 2540.0);
	CurrentCompOutLine = (8 * 2540.0);
	CurrentBoardOutLine = (8 * 2540.0);
	CurrentInfoLine = (8 * 2540.0);
	TraceThickness = (6 * 2540.0);
	CurrentTextHeight = (100 * 2540.0);

	memset(&Shape, 0, sizeof(ShapeRecord));

	memset(&BGAGeom, 0, sizeof(GeomCreateRecord));
	BGAGeom.Pitch = (50 * 2540);
	BGAGeom.Pad = (25 * 2540);
	BGAGeom.Mask = (29 * 2540);
	BGAGeom.Paste = (25 * 2540);
	BGAGeom.NrPinsX = 10;
	BGAGeom.NrPinsY = 10;
	BGAGeom.Pin1Square = 1;
	memset(&PGAGeom, 0, sizeof(GeomCreateRecord));
	PGAGeom.Pitch = (100 * 2540);
	PGAGeom.Pad = (60 * 2540);
	PGAGeom.PowerPad = (80 * 2540);
	PGAGeom.InnerPad = (60 * 2540);
	PGAGeom.Mask = (64 * 2540);
	PGAGeom.Drill = (30 * 2540);
	PGAGeom.NrPinsX = 10;
	PGAGeom.NrPinsY = 10;
	PGAGeom.InsertInnerPad = 1;
	PGAGeom.InsertPowerPad = 1;
	PGAGeom.Pin1Square = 1;
	memset(&SOICGeom, 0, sizeof(GeomCreateRecord));
	SOICGeom.PadX = (60 * 2540);
	SOICGeom.PadY = (30 * 2540);
	SOICGeom.NrPins = 10;
	SOICGeom.PitchX = (300 * 2540);
	SOICGeom.PitchY = (50 * 2540);
	SOICGeom.MaskX = (64 * 2540);
	SOICGeom.MaskY = (34 * 2540);
	SOICGeom.PasteX = (60 * 2540);
	SOICGeom.PasteY = (30 * 2540);
	memset(&DILGeom, 0, sizeof(GeomCreateRecord));
	DILGeom.PitchX = (300 * 2540);
	DILGeom.PitchY = (100 * 2540);
	DILGeom.Pad = (60 * 2540);
	DILGeom.PowerPad = (80 * 2540);
	DILGeom.InnerPad = (60 * 2540);
	DILGeom.Mask = (64 * 2540);
	DILGeom.Drill = (30 * 2540);
	DILGeom.NrPins = 10;
	DILGeom.InsertInnerPad = 1;
	DILGeom.InsertPowerPad = 1;
	DILGeom.Pin1Square = 1;
	memset(&SILGeom, 0, sizeof(GeomCreateRecord));
	SILGeom.PinInc = 1;
	SILGeom.NrPins = 1;
	SILGeom.Pitch = (100 * 2540);
	SILGeom.InsertPowerPad = 1;
	SILGeom.InsertInnerPad = 1;
	SILGeom.Pad = (50 * 2540);
	SILGeom.Mask = (56 * 2540);
	SILGeom.Drill = (0.8 * 100000);
//  SILGeom.PowerPad=(60*2540);
//  SILGeom.InnerPad=(50*2540);
	SILGeom.Start2 = 1;
	memset(&SIL_SMD_RECTGeom, 0, sizeof(GeomCreateRecord));
	SIL_SMD_RECTGeom.PinInc = 1;
	SIL_SMD_RECTGeom.NrPins = 1;
	SIL_SMD_RECTGeom.Pitch = (100 * 2540);
	SIL_SMD_RECTGeom.Start2 = 1;
	SIL_SMD_RECTGeom.MaskX = (88 * 2540);
	SIL_SMD_RECTGeom.MaskY = (48 * 2540);
	SIL_SMD_RECTGeom.PasteX = (80 * 2540);
	SIL_SMD_RECTGeom.PasteY = (40 * 2540);
	SIL_SMD_RECTGeom.PadX = (80 * 2540);
	SIL_SMD_RECTGeom.PadY = (40 * 2540);
	SIL_SMD_RECTGeom.Layer = NrPadLayers - 1;
	memset(&SIL_SMD_CIRCLEGeom, 0, sizeof(GeomCreateRecord));
	SIL_SMD_CIRCLEGeom.PinInc = 1;
	SIL_SMD_CIRCLEGeom.NrPins = 1;
	SIL_SMD_CIRCLEGeom.Pitch = (100 * 2540);
	SIL_SMD_CIRCLEGeom.Start2 = 1;
	SIL_SMD_CIRCLEGeom.Layer = NrPadLayers - 1;
	SIL_SMD_CIRCLEGeom.Paste = (50 * 2540);
	SIL_SMD_CIRCLEGeom.Pad = (50 * 2540);
	SIL_SMD_CIRCLEGeom.Mask = (56 * 2540);
	memset(&QUADGeom, 0, sizeof(GeomCreateRecord));
	QUADGeom.PadX = (60 * 2540);
	QUADGeom.PadY = (30 * 2540);
	QUADGeom.NrPinsX = 20;
	QUADGeom.NrPinsY = 10;
	QUADGeom.Pitch = (50 * 2540);
	QUADGeom.DistX = (1080 * 2540);
	QUADGeom.DistY = (600 * 2540);
	QUADGeom.MaskX = (64 * 2540);
	QUADGeom.MaskY = (34 * 2540);
	QUADGeom.PasteX = (60 * 2540);
	QUADGeom.PasteY = (30 * 2540);
	QUADGeom.Start2 = 1;

	VisibleMinX = -100e5;
	VisibleMinY = -100e5;
	VisibleMaxX = 100e5;
	VisibleMaxY = 100e5;


	AllocateSpecialMem(MEM_OBJECT_POLYGON, 128 * 1024, (void **) &NewObjectPolygon);
	GraphicsMain();
	CreateDrawObjects();
	FilesMain();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
