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

#define SC(Nr,string) (StringConvert(Nr,string))

#define TRACE_HOR                               0x0100	// 256
#define TRACE_VER                               0x0200	// 512
#define TRACE_DIAG1                             0x0300	// 768
#define TRACE_DIAG2                             0x0400	// 1024

/*
#define TRACE                                   0x0500  // 1280
#define TRACE_ALL_ANGLE                         0x0500  // 1280
#define TRACE_ARC                               0x0600  // 1536
*/

#define PIN_PUT_THROUGH_ROUND                   0x0800	// 2048
#define PIN_PUT_THROUGH_ROUND_POWER             0x0810	// 2064
#define PIN_PUT_THROUGH_SQUARE                  0x0820	// 2080
#define PIN_PUT_THROUGH_ROUND_INNER_PAD         0x0830	// 2096
#define PIN_PUT_THROUGH_POLYGON                 0x0840	// 2112
#define PIN_PUT_THROUGH_BUTTERFLY               0x0850	// 2128

#define PIN_SMD_RECT                            0x0900	// 2304
#define PIN_SMD_ROUND                           0x0980	// 2432
#define PIN_SMD_BUTTERFLY                       0x09A0	// 2464
#define PIN_SMD_POLYGON                         0x09B0	// 2480

#define PIN_LINE_HOR                            0x0A00	// 2560
#define PIN_LINE_VER                            0x0B00	// 2816
#define PIN_LINE_DIAG1                          0x0C00	// 3072
#define PIN_LINE_DIAG2                          0x0D00	// 3328
#define PIN_ARC                                 0x0D80	// 3456
#define PIN_LINE_ALL_ANGLE                      0x0DC0	// 3488

#define DRILL                                   0x0E00	// 3584
#define DRILL_UNPLATED                          0x0E10	// 3600
#define SOLD_MASK                               0x0F00	// 3840
#define PASTE_MASK                              0x0F10	// 3856

#define OBJECT_LINE                             0x1000	// 4096
#define OBJECT_POLYLINE                         0x1100	// 4352
#define OBJECT_RECT                             0x1200	// 4608
#define OBJECT_CIRCLE                           0x1300	// 4864
#define OBJECT_ARC                              0x1400	// 5120
#define OBJECT_TEXT                             0x1500	// 5376
#define OBJECT_TEXT2                            0x1600	// 5632
#define OBJECT_POLYGON                          0x1700	// 5888
#define OBJECT_BUTTERFLY                        0x1800	// 6144

#define ROUTING_KEEPOUT_RECT                    0x2000	// 8192
#define VIA_PUT_THROUGH_ROUND                   0x3000	// 12288
#define CONNECTION                              0x4000	// 16384
#define SILKSCREEN_TEXT                         0x6000	// 24576
#define NIKS                                    0x7000	// 28672
#define COMP_OBJECT                             0x7800	// 30720

//  Net info

#define ALIGN_LEFT_BOTTOM                       0
#define ALIGN_LEFT_CENTRE                       1
#define ALIGN_LEFT_TOP                          2
#define ALIGN_CENTRE_BOTTOM                     3
#define ALIGN_CENTRE_CENTRE                     4
#define ALIGN_CENTRE_TOP                        5
#define ALIGN_RIGHT_BOTTOM                      6
#define ALIGN_RIGHT_CENTRE                      7
#define ALIGN_RIGHT_TOP                         8

#define CONNECTIONS_DISABLED                    0x0001
#define CONNECTIONS_NOT_VISIBLE                 0x0002
#define NET_HILITED                             0x0004
#define SMD_DEVICE                              0x0100


//  Object info

#define SHAPE_TYPE_FILLED                       0x0020
#define OBJECT_SPECIAL_DRAW                     0x0040
#define OBJECT_TO_BE_DELETED                    0x0100
#define OBJECT_DISABLED                         0x0400
#define OBJECT_NOT_VISIBLE                      0x0800



#define OBJECT_SAVED                            0x1000
#define OBJECT_SELECTED                         0x2000

#define PAD_BOTTOM_LAYER                        0
#define PAD_TOP_LAYER                           1
#define SOLD_MASK_BOTTOM_LAYER                  100
#define SOLD_MASK_TOP_LAYER                     101
#define PASTE_MASK_BOTTOM_LAYER                 200
#define PASTE_MASK_TOP_LAYER                    201
#define INFO_LAYER                              1500
#define BOARD_OUTLINE_LAYER                     1600
#define SILKSCREEN_TOP_LAYER                    2000
#define SILKSCREEN_BOTTOM_LAYER                 2001
#define PLACEMENT_OUTLINE_LAYER                 3000
#define COMP_OUTLINE_LAYER                      4000
#define PIN_TEXT_LAYER                          5000
#define GEOM_NAME_LAYER                         6000
#define DRILL_LAYER                             4100
#define DRILL_UNPLATED_LAYER                    4200
#define DRILL_UNPLATED_LAYER2                   6500
#define INNER_PAD_LAYER                         4300
#define POWER_PAD_LAYER                         4400
#define POLYGON_DRAW_LAYER                      6900
#define SPECIALS_LAYER                          7000
#define INFO_LAYER2                             7500
#define INFO_LAYER3                             8000
#define INFO_LAYER4                             8500
#define ROUTING_KEEPOUT_LAYER                   9000
#define OBJECT_SELECTED_LAYER                   9500

#define NET_VISIBLE                             1
#define MODE_OBJECTS2                           2
#define MODE_OBJECTS3                           3

#define ShapeCode                               "Shape definition 1.0"
#define ShapeCode2                              "Shape definition 1.5"
#define ShapeCode3                              "Shape definition 2.0"
#define LibraryCode1                            "Geometry library version 1.0"

#define DefFontSize                             (float)0.833333333333

#define DefMaxNrObjects                         512
#define DefMaxNrObjects2                        128
#define DefMaxNrPinObjects                      1024
#define DefSharedMemoryLength                   2048
#define DefMaxNrObjectPolygons                  512

#define DefShapeMemory                          256*1024

#define MaxNrActions                            100

#define WIN32S                                  1
#define WINDOWS95                               2
#define WINDOWSNT35                             3
#define WINDOWSNT351                            4
#define WINDOWSNT40                             5

#define ARROW_LENGTH                            254000.0
#define DIMENSION_HEIGHT                        254000.0

#define SUB_POLYGON_MAGIC                       0x44cb8e2c

#define MEM_POINTS                              0
#define MEM_POLYGON1                            1
#define MEM_POLYGON2                            2
#define MEM_OBJECT_POLYGON                      3

typedef struct
{
	float BoardOriginX, BoardOriginY, BoardWidth, BoardHeight;
} DesignRecord;

typedef struct
{
	char Identification[28];
	int32 MemSize;
	char ShapeName[32];
	int32 Revision;
	int32 CompOutLineOffset, PinOffset, SilkScreenOffset, OtherObjectsOffset;
	int32 NrPlacementOutLines, NrCompOutLines, NrPins, NrSilkScreenOutLines, NrOtherObjects;
// Position 104 = 0x68
	float InsertionX, InsertionY, ShapeHeight;
// Position 116 = 0x74
	int32 NrPolygons;
	int32 PolygonOffset;
	int32 NrLayers;
	int32 Dummy2;
	float ShapeNameHeight, ShapeNameOriginX, ShapeNameOriginY;
	int16 Info, AddNr, DeleteNr, Dummy;
	int32 ShapeNameRotation;
} ShapeRecord;

typedef struct
{
	int32 NetNr;
} CompPinRecord;

typedef struct
{
	int32 PinType, StartNr, NrObjects;
	char PinText[12];
} PinInfoRecord;

typedef PinInfoRecord PinInfoArray[DefMaxNrPinObjects];

typedef int32 PinObjectsInfoArray[DefMaxNrPinObjects];

/*
typedef struct {
          int16 ObjectType,Test;
          int32 Layer,PinNr;
          float x1,y1,x2,y2,Clearance,
                minx,miny,maxx,maxy;
          int32 ObjectNr;
          int16 Info,Info2;
        } ObjectRecord ;
*/

typedef struct
{
	int32 ObjectType, Test, AddNr, DeleteNr, Layer, PinNr, Info, Info2;
	double x1, y1, x2, y2, x3, y3, x4, y4, Clearance, Thickness, RotationAngle;
	uint8 *Address;
	char Text[76];
} ObjectRecord;

typedef ObjectRecord ObjectArray[DefMaxNrObjects];


typedef struct
{
	int32 ObjectType, Test, AddNr, DeleteNr, Layer, PinNr, Info, Info2;
	double x1, y1, x2, y2, x3, y3, x4, y4, Clearance, Thickness;
	char Text[1024];
} ObjectRecord2;

//typedef ExtObjectRecord ExtObjectArray[DefMaxNrExtObjects];

typedef int16 ActionArray[MaxNrActions];

typedef struct
{
	int32 ObjectType, Test, Layer, TraceNr, Info, Info2, NetNr, CompNr, PinNr, CompNr2;
	double x1, y1, x2, y2, Clearance, minx, miny, maxx, maxy;
	LPSTR TextP;
} WriteObjectRecord;

typedef struct
{
	char Name[10];
	int16 NrPinShapes;
} ShapePadRecord;


typedef struct
{
	int32 ShapeType, Layer;
	float X, Y, Width, Height;
	union NotUsedObject5
	{
		float Extra1;
		float Thickness;
		uint32 AddressOffset;
	} Special;
	float Extra2, Clearance;
} PadRecord;

typedef struct
{
	int32 ShapeType, Layer;
	float X, Y, Width, Height, X1, Y1, X2, Y2, Thickness;
} Pad2Record;

typedef float ShapeLinesArray[128];


typedef struct
{
	int32 NrPolyLines;
	float Line[51];
} CharRecord;

typedef CharRecord CharsArray[94];

typedef struct
{
	double Xoffset, Yoffset, Factor;
} ViewPosRecord;


typedef ViewPosRecord ViewPosArray[20];


typedef struct
{
	double MaskX, MaskY, Mask, PasteX, PasteY, Paste, Drill, DistX, DistY, PadX, Pad, PadY, PowerPad, InnerPad, Pitch,
	       PitchX, PitchY, Clearance;
	int32 NrPinsX, NrPinsY, NrPins, Start2, PinInc, Units;
	int32 InsertPowerPad, InsertInnerPad, Pin1Square, Layer;
	char Start1[12];
} GeomCreateRecord;


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

typedef struct
{
	int32 OtherInfos[32];
	int32 FileTypes[32], FileInfos[32];
	HWND WindowHandles[32];
	char FileNames[32][200];
	char PrinterName[160];
	char TempStr1[40];
	int32 PaperSize;
	int32 PrintingBusy;
	int32 PrintingError;
} ProjectInfoRecord;

typedef struct
{
	double x, y;
} PointRecord;

typedef PointRecord PointsArray[512];

typedef struct
{
	float x, y;
} PointRecord2;

typedef PointRecord2 PointsArray2[512];

typedef struct
{
	int32 NrVertices, PolygonType, Test;
	double Clearance, minx, miny, maxx, maxy;
} PolygonInitRecord;

typedef struct
{
	int32 NrVertices, PolygonType;
	union NotUsed
	{
		int32 Test;
		int32 Layer;
	} Special;
	double Clearance, minx, miny, maxx, maxy;
	PointsArray Points;
} PolygonRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 BlockNr;
	int32 Layer;
	int32 PinNr;
	int32 NrVertices, PolygonNr;
	int32 ShapePolygonPosition;
	float Clearance;
	int32 NrSubPolygons, NrVerticesMainPolygon;
	int32 NotUsed3, NotUsed4;
	double OffsetX, OffsetY;
	double minx, miny, maxx, maxy;
} ObjectPolygonInitRecord;


typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 BlockNr;
	int32 Layer, PinNr;
	int32 NrVertices, PolygonNr;
	int32 ShapePolygonPosition;
	float Clearance;
	int32 NrSubPolygons, NrVerticesMainPolygon;
	int32 NotUsed3, NotUsed4;
	double OffsetX, OffsetY;
	double minx, miny, maxx, maxy;
	PointsArray Points;
} ObjectPolygonRecord;

typedef struct
{
	int32 Magic, NrVertices;
	int32 NotUsed1, NotUsed2;
	double minx, miny, maxx, maxy;
	PointsArray Points;
} ObjectSubPolygonRecord;

typedef struct
{
	int32 Magic, NrVertices;
	int32 NotUsed1, NotUsed2;
	double minx, miny, maxx, maxy;
} ObjectSubPolygonInitRecord;

typedef int32 ObjectPolygonsArray[DefMaxNrObjectPolygons];

typedef struct
{
	int16 AreaFillType, NetNr, Info, AddNr, DeleteNr, FillType;
	int32 MemSize, NrPolygons;
	float SurroundThickNess, Clearance;
	double minx, miny, maxx, maxy;
	float ThermalReliefThickness, ThermalReliefDistance;
	float niks1, niks2, niks3, niks4;
	int32 Layer;
	int32 NrVerticesStartPolygon;
	PointsArray2 StartPolygon;
} AreaFillRecord;

typedef int32(*FUNCP1) (double, double, int32);
typedef int32(*FUNCP2) (double, double, double);
typedef int32(*FUNCP3) (double, int32);
typedef int32(*FUNCP4) (int32);
typedef int32(*FUNCP5) (double, double, int32, int32);
typedef int32(*FUNCP6) (double, double, double, double, double, double, double, int32);
typedef int32(*FUNCP7) (int32, double, double, int32);
typedef int32(*FUNCP8) (double, double, double, int32);
typedef int32(*FUNCP9) (double, double);
typedef int32(*FUNCP10) (double, double, double, double, int32);

typedef struct
{
	FUNCP1 Function1;
	FUNCP2 Function2;
	FUNCP3 Function3;
	FUNCP4 Function4a;
	FUNCP4 Function4b;
	FUNCP5 Function5;
	FUNCP6 Function6;
	FUNCP7 Function7;
	FUNCP8 Function8;
	FUNCP9 Function9;
	FUNCP1 Function10a;
	FUNCP1 Function10b;
	FUNCP10 Function11;
	void *Param1[8];
	void *Param2[8];
	int32 Mode;
} DrawXorFunctionRecord;

#pragma pack(2)

typedef struct
{
	int16 Identifier;
	int32 FileSize;
	int32 Reserved1;
	int32 StartOfDataOffset;
	int32 BitmapHeaderSize;
	int32 Width;
	int32 Height;
	int16 NrOfPlanes;
	int16 BitsPerPixel;
	int32 CompressionType, BitmapDataSize, HResolutionInPixelsPerMeter, VResolutionInPixelsPerMeter, NrColors1,
	      NrImportedColors;
} BmpHeaderRecord;

#pragma pack()


#endif
