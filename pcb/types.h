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
#include "commctrl.h"
#include "utf8.h"

#define SC(Nr,string) (StringConvert(Nr,string))

#define TRACE_HOR                               0x0100
#define TRACE_VER                               0x0200	// 512
#define TRACE_DIAG1                             0x0300	// 768
#define TRACE_DIAG2                             0x0400	// 1024

#define TRACE_ALL_ANGLE                         0x0500	// 1280
#define TRACE_ARC                               0x0600	// 1536

#define PIN_PUT_THROUGH_ROUND                   0x0800	// 2048
#define PIN_PUT_THROUGH_ROUND_POWER             0x0810	// 2064
#define PIN_PUT_THROUGH_SQUARE                  0x0820	// 2080
#define PIN_PUT_THROUGH_ROUND_INNER_PAD         0x0830	// 2096
#define PIN_PUT_THROUGH_POLYGON                 0x0840	// 2112

#define PIN_SMD_RECT                            0x0900	// 2304
#define PIN_SMD_ROUND                           0x0980	// 2432
#define FIDUCIAL                                0x0990	// 2448
#define PIN_SMD_POLYGON                         0x09B0	// 2480
#define PIN_BIG_POLYGON                         0x09C0	// 2496

#define PIN_LINE_HOR                            0x0A00	// 2560
#define PIN_LINE_VER                            0x0B00	// 2816
#define PIN_LINE_DIAG1                          0x0C00	// 3072
#define PIN_LINE_DIAG2                          0x0D00	// 3328
#define PIN_ARC                                 0x0D80	// 3456
#define PIN_LINE_ALL_ANGLE                      0x0DC0	// 3520

#define DRILL                                   0x0E00	// 3584
#define DRILL_UNPLATED                          0x0E10	// 3600

#define OBJECT_LINE                             0x1000	// 4096
#define OBJECT_POLYLINE                         0x1100	// 4352
#define OBJECT_RECT                             0x1200	// 4608
#define OBJECT_CIRCLE                           0x1300	// 4864
#define OBJECT_ARC                              0x1400	// 5120
#define OBJECT_TEXT                             0x1500	// 5376
#define OBJECT_TEXT2                            0x1600	// 5632
#define OBJECT_POLYGON                          0x1700	// 5888
#define THERMAL_RELIEF_ROUND                    0x1800	// 5888

#define ROUTING_KEEPOUT_RECT                    0x2000	// 8192
#define VIA_PUT_THROUGH_ROUND                   0x3000	// 12288
#define CONNECTION                              0x4000	// 16384
#define AREAFILL                                0x6800
#define AREAFILL2                               0x6880
#define AREAFILL_FROM_BIG_POLYGON               0x6900
#define NIKS                                    0x7000
#define COMP_OBJECT                             0x7800
#define ODB_VIA_OBJECT                          0x7900
#define ODB_COMP_OBJECT                         0x7910
#define ODB_PLANE_OBJECT                        0x7920
#define ODB_SUB_NET_OBJECT                      0x7930

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


//  Object info

#define POWERPLANE                              0x0100
#define AREAFILL_WITH_THERMAL_RELIEF            0x0200
#define AREAFILL_WITH_NO_VIA_THERMAL_RELIEF     0x0400

#define SMD_DEVICE                              0x0100

#define CONNECTIONS_DISABLED                    0x0040
#define CONNECTIONS_NOT_VISIBLE                 0x0080
#define COMPONENT_PROTECTED                     0x0080
#define FIDUCIAL_OPTION                         0x0080

#define OBJECT_WITH_CLEARANCE                   0x0100
#define OBJECT_FILLED                           0x0200
#define OBJECT_DONE                             0x0400
#define OBJECT_WARNING                          0x0800
#define OBJECT_NOT_VISIBLE                      0x1000

#define OBJECT_HIGHLITED                        0x2000
#define OBJECT_SELECTED                         0x4000
#define OBJECT_CHANGED                          (OBJECT_HIGHLITED+\
                                                COMPONENT_PROTECTED+\
                                                OBJECT_SELECTED)

#define NET_VISIBLE                             1
#define MODE_OBJECTS2                           2
#define MODE_OBJECTS3                           3

#define UNSELECT_ALL                            20000
#define UNSELECT_NETNR                          20001
#define SELECT_NETNR                            20002
#define UNSELECT_REF                            20003
#define SELECT_REF                              20004

#define LibraryCode1                            "Geometry library version 1.0"
#define PCBCode                                 "PCB definition 1.0"
#define PCBCode2                                "PCB definition 2.0"
#define PCBCode3                                "PCB definition 2.5"
#define PCBCode4                                "PCB definition 3.0"

#define DefMaxNrTraces                          1024
#define DefMaxNrVias                            1024
#define DefMaxNrConnections                     1024
#define DefMaxNrNetConnections                  800
#define DefMaxNrNets                            512
#define DefMaxNrObjects                         512
#define DefMaxNrObjects2                        256
#define DefMaxNrExtObjects                      512
#define DefMaxNrComps                           512
#define DefMaxNrShapes                          64
#define DefMaxNrNetTests                        800
#define DefMaxNrAreaFills                       128
#define DefSharedMemoryLength                   2048

#define DefCompsMemory                          256*1024
#define DefShapesMemory                         256*1024
#define DefAreaFillMemory                       256*1024

#define NrDeleteInfo                            100
#define MaxNrActions                            100

#define DefMaxNrObjectLines                     256
#define DefMaxNrObjectCircles                   128
#define DefMaxNrObjectRects                     128
#define DefMaxNrObjectArcs                      64
#define DefMaxNrObjectTexts                     128


#define DefMaxNrObjectTexts2                    128
#define DefMaxNrObjectPolygons                  128
#define DefObjectPolygonMemory                  128*1024


#define MaxFoundObjects                         100

#define WIN32S                                  1
#define WINDOWS95                               2
#define WINDOWSNT35                             3
#define WINDOWSNT351                            4
#define WINDOWSNT40                             5


#define DRAG_TRACES_VIAS_COMPS_MODE             0
#define ROUTING_MODE                            1
#define MOVE_ONE_TRACE_MODE                     2
#define MOVE_COMPONENTS_MODE                    3
#define MOVE_COMPONENT_REFERENCES_MODE          4
#define MOVE_COMPONENT_VALUES_MODE              5
#define OBJECTS_MODE                            6
#define AREAFILLS_MODE                          7
#define MOVING_TRACES_VIAS_MODE                 8
#define GATE_PINSWAP_MODE                       9


// Layer range
//
// 100  .. 12700  ( increments 100)
// 101  .. 12701  ( increments 100)

#define SOLD_MASK_BOTTOM                        100
#define SOLD_MASK_TOP                           101
#define PASTE_MASK_BOTTOM                       200
#define PASTE_MASK_TOP                          201
#define SHAPE_PINS_BOTTOM                       300
#define SHAPE_PINS_TOP                          400
#define SHAPE_PINS_INNER                        500
#define POLYLINE_LAYER                          600
#define UNCONNECTED_PADS_TOP_LAYER              700
#define UNCONNECTED_PADS_BOTTOM_LAYER           800
#define UNCONNECTED_PADS_INNER_LAYER            900
#define BACKGROUND_LAYER                        1000
#define SWAP_PINS_LAYER                         1100
#define SWAP_GATE_LAYER                         1200
#define INFO_LAYER                              1500
#define BOARD_OUTLINE_LAYER                     1600
#define BOARD_OUTLINE_KEEPOUT_LAYER             1601
#define ASSEMBLY_TOP_LAYER                      1700
#define ASSEMBLY_BOTTOM_LAYER                   1701
#define TOP_COMP_LAYER                          1750
#define BOTTOM_COMP_LAYER                       1751
#define DOCUMENT_LAYER                          1800
#define SILKSCREEN_BOTTOM                       2000
#define SILKSCREEN_TOP                          2001
#define SILKSCREEN_TOP_REFS                     2101
#define SILKSCREEN_TOP_VALUES                   2201
#define SILKSCREEN_BOTTOM_REFS                  2100
#define SILKSCREEN_BOTTOM_VALUES                2200
#define COMP_REF_LAYER                          2800
#define COMP_VALUE_LAYER                        2900
#define PLACEMENT_OUTLINE_TOP                   3000
#define PLACEMENT_OUTLINE_BOTTOM                3001
#define PLACEMENT_OUTLINE_TOP2                  3010
#define PLACEMENT_OUTLINE_BOTTOM2               3011
#define CONNECTIONS_LAYER                       3100
#define NET_PINS_LAYER                          3200
#define NET_PINS_LAYER2                         3300
#define COMP_OUTLINE_LAYER                      4000
#define COMP_OUTLINE_LAYER_TOP                  4000
#define COMP_OUTLINE_LAYER_BOTTOM               4001
#define FIXED_COLOR_LAYER                       5600
#define CLEARANCE_LAYER                         5700
#define ERROR_LAYER                             5800
#define WARNING_LAYER                           5900
#define DRILL_LAYER                             6000
#define DRILL_UNPLATED_LAYER                    6500
#define VIA_LAYER                               6600
#define VIA_DRILL_LAYER                         6700
#define BUTTON_INFO_LAYER                       6800
#define POLYGON_DRAW_LAYER                      6900
#define SPECIALS_LAYER                          7000
#define CROSS_HAIR_LAYER                        7100
#define INFO_LAYER2                             7500
#define INFO_LAYER3                             8000
#define INFO_LAYER4                             8500
#define GRID_LAYER                              8600
#define AREAFILL_DASH_PEN_LAYER                 8700
#define ROUTING_KEEPOUT_LAYER                   9000
#define ROUTING_KEEPOUT_BOTTOM                  9100
#define ROUTING_KEEPOUT_TOP                     9101
#define ROUTING_KEEPOUT_INNER                   9102

#define VIA_SOLDMASK_TOP                        1
#define VIA_SOLDMASK_BOTTOM                     2

#define PAPERSIZE_A1                            1
#define PAPERSIZE_A2                            2
#define PAPERSIZE_A3                            3
#define PAPERSIZE_A4                            4
#define PAPERSIZE_A5                            5
#define PAPERSIZE_B4                            10
#define PAPERSIZE_B5                            11
#define PAPERSIZE_B4_JIS                        20
#define PAPERSIZE_B5_JIS                        21
#define PAPERSIZE_LEGAL                         30
#define PAPERSIZE_LETTER                        31
#define ORIENTATION_PORTRAIT                    1
#define ORIENTATION_LANDSCAPE                   2
#define ORIENTATION_AUTO                        3

#define ARROW_LENGTH                            254000.0
#define DIMENSION_HEIGHT                        254000.0

#define TRUETYPE_FONT_ADD_EXTRA_X               0.2
#define TRUETYPE_FONT_SPACE_EXTRA_X             0.35

#define MENU_CODE_START                         10000

#define SUB_POLYGON_MAGIC                       0x44cb8e2c

#define ShapeCode                               "Shape definition 1.0"
#define ShapeCode2                              "Shape definition 1.5"
#define ShapeCode3                              "Shape definition 2.0"

#define MAX_WORK_POLGONS                        32

#define MEM_POLYGON_BIGGER                      0
#define MEM_POLYGON_BIGGER2                     1
#define MEM_POINTS                              2
#define MEM_POLYGON_CHECK1                      3
#define MEM_POLYGON_CHECK2                      4
#define MEM_POLYGON_CHECK3                      5
#define MEM_POLYGON_CHECK4                      6
#define MEM_AREAFILL1                           7
#define MEM_AREAFILL2                           8
#define MEM_AREAFILL3                           9
#define MEM_AREAFILL4                           10
#define MEM_BOARDOUTLINE_AREAFILL               11
#define MEM_PLOT_LINES_BUF                      12
#define MEM_PLOT_POINTS_MAP                     13
#define MEM_POWERPLANE_AREAFILL                 14
#define MEM_POLYGON                             15
#define MEM_TRUETYPE_AREAFILL                   16
#define MEM_APERTURE_LINES                      17
#define MEM_APERTURE_TEXT                       18
#define MEM_NETINFO                             19
#define MEM_NET_SELECTED                        20
#define MEM_GERBER_LINEBUF                      21
#define MEM_COMP_INFO                           22
#define MEM_OBJECT_SELECTED                     23
#define MEM_USERVARS                            24
#define MEM_PROBLEM_POLYGON1                    25
#define MEM_PROBLEM_POLYGON2                    26
#define MEM_PDFBUF                              27
#define MEM_LAYER_OBJECTS                       64


#define AtomStrSchematicSelect                  "Layout_editor_active_schematic_select"

typedef struct
{
	double x, y;
} PointRecord;

typedef PointRecord PointsArray[200];

typedef struct
{
	float x, y;
} PointRecord2;

typedef PointRecord2 PointsArray2[200];

typedef POINT PointsArray3[200];

typedef struct
{
	int16 ViaType, NetNr;
	float X, Y, ThickNess, DrillThickNess, ThermalInner, Clearance, SoldMask;
	int32 Layer;
	int16 Info, AddNr, DeleteNr, Dummy;
} ViaRecord;

typedef struct
{
	char Identification[32], EditingPerson[32];
	int32 FileVersion, Revision;
	float BoardOutlineKeepOut, BoardOriginX, BoardOriginY, BoardWidth, BoardHeight, StandardTraceWidth,
	      StandardClearance, MaximumClearance, PowerPlaneBorder, SilkScreenWidth;
	int32 NrBoardLayers, NrTraceLayers, NrPowerPlanes;
	int32 NrShapes, NrNets, NrComps, NrAreaFills, NrPins;
	int32 NrHorTraces[32], NrVerTraces[32], NrDiag1Traces[32], NrDiag2Traces[32];
	int32 LayerInfo[32];
	int32 NrAreaFillsInPowerPlane[32];
	char LayerText[32][32];
	int32 NrObjectTexts2, NrObjectPolygons, ObjectPolygonMem;
	float DimensionHeight;
	float ArrowLength;
	float BoardOutlineWidth;
	int32 Unused[26];
	ViaRecord DefVia1, DefVia2, DefVia3, DefVia4, DefVia5;
	ViaRecord DefVia6, DefVia7, DefVia8, DefVia9, DefVia10;
	char UsedFontStr[16][32];
	uint8 Reserved[1096];
	int32 NrVias, NrConnections, NrObjectLines, NrObjectCircles, NrObjectRects, NrObjectArcs, NrObjectTexts;
	struct DesignDateStruct
	{
		int32 Year;
		uint8 Month, Day, Hour, Minutes;
	} DesignDate;
	int32 ShapesMem, CompsMem, AreaFillMem;
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
	int32 ShapeType, Layer;
	float X, Y, Width, Height;
	union NotUsedObject3
	{
		float Extra1;
		float Thickness;
		uint32 AddressOffset;
	} Special;
	float Extra2, Clearance;
} PadRecord;

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
	PointsArray Points;
} GeomPolygonRecord;

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
} GeomPolygonInitRecord;


typedef struct
{
	int32 Magic, NrVertices;
	int32 NotUsed1, NotUsed2;
	double minx, miny, maxx, maxy;
	PointsArray Points;
} GeomSubPolygonRecord;

typedef struct
{
	int32 Magic, NrVertices;
	int32 NotUsed1, NotUsed2;
	double minx, miny, maxx, maxy;
} GeomSubPolygonInitRecord;

typedef struct
{
	char Name[10];
	int16 NrPinShapes;
} ShapePadRecord;


typedef struct
{
	float X, Y, Length;
	int16 NetNr, Info;
	int32 Layer;
	float ThickNess, Clearance;
	int16 AddNr, DeleteNr;
} TraceRecord;

typedef TraceRecord TracesArray[DefMaxNrTraces];


typedef ViaRecord ViaArray[DefMaxNrVias];

typedef struct
{
	int32 MinX, MaxX, MinY, MaxY, Layer, Info, MemPos;
} MinMaxRecord;

typedef struct
{
	float x1, y1, x2, y2;
	int32 ObjectNr;
	int16 NetNr, Layer, Comp1Nr, Comp1PinNr, Comp2Nr, Comp2PinNr, Info, AddNr, DeleteNr, Extra1;
} ConnectionsRecord;

typedef ConnectionsRecord ConnectionsArray[DefMaxNrConnections];
typedef ConnectionsRecord NetConnectionsArray[DefMaxNrNetConnections];

typedef struct
{
	int32 NetNr;
} CompPinRecord;



typedef struct
{
	char Name[44];
	int32 NetInfo, NrPins, Count, Pos;
	int16 Info, AddNr, DeleteNr, Dummy;
	float TraceWidth, TraceClearance;
	int32 ViaNr;
	char Properties[252];
} NetRecord;

typedef struct
{
	char Name[44];
	int32 NetInfo, NrPins, Count, Pos;
	int16 Info, AddNr, DeleteNr, Dummy;
	float TraceWidth, TraceClearance;
} OldNetRecord;

typedef NetRecord NetArray[DefMaxNrNets];

typedef struct
{	// 96 bytes
	int32 ObjectType, Test, Layer, ObjectType2, Mirror, PinCount, TraceNr, Info, Info2, NetNr, CompNr, PinNr, CompNr2;
	double x1, y1, x2, y2, x3, y3, x4, y4, Clearance, minx, miny, maxx, maxy, Thickness, RotationAngle;
	uint8 *Address;
} ObjectRecord;

typedef ObjectRecord ObjectArray[DefMaxNrObjects];

typedef struct
{
	int16 Info, AddNr, DeleteNr, NetNr;
	int32 Layer;
	float X1, Y1, X2, Y2, LineThickNess;
	float Clearance;
	int32 LineMode;
} ObjectLineRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr, NetNr;
	int32 Layer;
	float X1, Y1, X2, Y2, LineThickNess;
} OldObjectLineRecord;

typedef ObjectLineRecord ObjectLinesArray[DefMaxNrObjectLines];

typedef struct
{
	int16 Info, AddNr, DeleteNr, NetNr;
	int32 Layer;
	float CentreX, CentreY, Width, Height, LineThickNess;
	float Clearance, Dummy;
} ObjectRectRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr, NetNr;
	int32 Layer;
	float CentreX, CentreY, Width, Height, LineThickNess;
} OldObjectRectRecord;

typedef ObjectRectRecord ObjectRectsArray[DefMaxNrObjectRects];

typedef struct
{
	int16 Info, AddNr, DeleteNr, NetNr;
	int16 LineMode, CircleMode;
	int32 Layer;
	float CentreX, CentreY, Diam, LineThickNess;
	float Clearance, Dummy;
} ObjectCircleRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int32 Layer;
	int16 NetNr, LineMode, CircleMode;
	float CentreX, CentreY, Diam, LineThickNess;
} OldObjectCircleRecord;

typedef ObjectCircleRecord ObjectCirclesArray[DefMaxNrObjectCircles];

typedef struct
{
	int16 Info, AddNr, DeleteNr, NetNr;
	int32 Layer;
	float CentreX, CentreY, StartDiffX, StartDiffY, EndDiffX, EndDiffY, Width, Height, LineThickNess;
	float Clearance, Dummy;
} ObjectArcRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int32 Layer;
	int16 NetNr;
	float CentreX, CentreY, StartDiffX, StartDiffY, EndDiffX, EndDiffY, Width, Height, LineThickNess;
} OldObjectArcRecord;

typedef ObjectArcRecord ObjectArcsArray[DefMaxNrObjectArcs];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NetNr, TextMode, Dummy;
	int32 Layer;
	float X, Y, FontHeight, LineThickNess;
	char Text[64];
} ObjectTextRecord;

typedef ObjectTextRecord ObjectTextsArray[DefMaxNrObjectTexts];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NetNr, TextMode, Dummy;
	int32 Layer;
	float X, Y, FontHeight, LineThickNess, Rotation;
	char Text[64];
} OldObjectTextRecord2;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NetNr, TextMode, FontNr;
	int32 Layer;
	float X, Y, FontHeight, LineThickNess, Rotation;
	char Text[256];
} ObjectTextRecord2;

typedef ObjectTextRecord2 ObjectTextsArray2[DefMaxNrObjectTexts2];

typedef int32 ObjectPolygonsArray[DefMaxNrAreaFills];


typedef struct
{
	int32 ShapePos;
	char ShapeName[32];
} ShapeInfoRecord;


typedef ShapeInfoRecord ShapesArray[DefMaxNrShapes];
typedef int32 CompsArray[DefMaxNrComps];
typedef int32 AreaFillsArray[DefMaxNrAreaFills];

/*
typedef struct {
          int16 ShapeType,Layer;
          int32 X,Y,Width,Height,Clearance;
          int16 NetNr;
          char  PinName[14];
        } PinDrawRecord ;

#ifdef DesignSizeBig
typedef PinDrawRecord PinDrawArray[4096];
#else
typedef PinDrawRecord PinDrawArray[1024];
#endif


typedef struct {
          int32 X1,X2,Y1,Y2;
          int16 Layer,Info;
          int32 MemPos;
        } CheckBufRecord ;

#ifdef DesignSizeBig
typedef CheckBufRecord CheckBufArray[8192];
#else
typedef CheckBufRecord CheckBufArray[2048];
#endif
*/


typedef float ShapeLinesArray[128];


typedef struct
{
	int32 MemSize, PinOffset;
	int16 ShapeNr, NrPins, CompMode, Info, AddNr, DeleteNr, Info2, Info3, Info4, Info5;
	float CompOriginX, CompOriginY, CompHeight, PlacementOriginX, PlacementOriginY, PlacementWidth, PlacementHeight;
	int32 TextVisibility;
	float Rotation, CompNameRotation, CompValueRotation, Dummy;
	float CompNameOriginX, CompNameOriginY, CompNameHeight, CompNamePenThickNess, CompValueOriginX, CompValueOriginY,
	      CompValueHeight, CompValuePenThickNess, BoardPosMinX, BoardPosMinY, BoardPosMaxX, BoardPosMaxY, DummyX1,
	      DummyY1, DummyX2, DummyY2, PinMaximumClearance;
	char Name[16], Value[32], PartNr[32], PartDescription[32], ShapeName[32], Sheet[32], PCBGroup[16], Options[16];
	char Properties[256];
} CompRecord;

typedef struct
{
	int32 MemSize, PinOffset;
	int16 ShapeNr, NrPins, CompMode, Info, AddNr, DeleteNr, Info2, Info3, Info4, Info5;
	float CompOriginX, CompOriginY, CompHeight, PlacementOriginX, PlacementOriginY, PlacementWidth, PlacementHeight;
	int16 TextVisibility, TextRotation;
	float CompNameOriginX, CompNameOriginY, CompNameHeight, CompNamePenThickNess, CompValueOriginX, CompValueOriginY,
	      CompValueHeight, CompValuePenThickNess, BoardPosMinX, BoardPosMinY, BoardPosMaxX, BoardPosMaxY, BoardPosDiagX1,
	      BoardPosDiagY1, BoardPosDiagX2, BoardPosDiagY2, PinMaximumClearance;
	char Name[16], Value[32], PartNr[32], PartDescription[32], ShapeName[32], Sheet[32], PCBGroup[16],
	     BestuckungOptie[16];
} OldCompRecord;

typedef struct
{
	int32 NrPolyLines;
	float Line[51];
} CharRecord;

typedef CharRecord CharsArray[95];

typedef struct
{
	int32 HorTracesPos;
	int32 VerTracesPos;
	int32 Diag1TracesPos;
	int32 Diag2TracesPos;
} NetPosRecord;

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
	double Value, MinValue, MaxValue, Value2, MinValue2, MaxValue2;
} ValueRecord;

typedef struct
{
	double Xoffset, Yoffset, Factor;
} ViewPosRecord;


typedef ViewPosRecord ViewPosArray[20];

typedef struct
{
	double TraceX, TraceY, Xoffset, Yoffset, Factor;
} LastTracePosRecord;

typedef LastTracePosRecord LastTracePosArray[20];

typedef int NetInfoArray[DefMaxNrNets];


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
	int16 NetNr;
	int32 Layer;
	int32 NrVertices, PolygonType;
	double minx, miny, maxx, maxy;
	PointsArray Points;
} ObjectPolygonRecord;

typedef struct
{
	int32 NrVertices, PolygonType, Test;
	double Clearance, minx, miny, maxx, maxy;
	PointsArray2 Points;
} PolygonRecord2;

typedef struct
{
	int32 NrVertices, PolygonType, Test;
	double Clearance, minx, miny, maxx, maxy;
	PointRecord Points[8];
} Polygon8Record;

typedef struct
{
	int32 NrVertices, PolygonType, Test;
	double Clearance, minx, miny, maxx, maxy;
} PolygonInitRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 BlockNr;
	int32 Layer;
	int32 NrVertices, PolygonType;
	double minx, miny, maxx, maxy;
} ObjectPolygonInitRecord;

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


typedef struct
{
	int32 AperTureNr, AperTureCode, Info, Info2, Info3, Info4, Used, Used2, Mirror, SpecialType;
	double x, y, x2, y2, x3, Rotation, HoleSize;
	uint8 *Address;
} AperTureRecord;

typedef AperTureRecord AperTureArray[400];


typedef struct
{
	int32 NetNr, CompNr, PinNr, Info;
	char PinStr[20];
} NetItemsRecord;

typedef NetItemsRecord NetItemsArray[1000];

typedef struct
{
	int32 PlotMode;
	int32 GerberNumberMode;
	int32 GerberOutputMode;
	int32 AutoApertureGeneration;
	int32 Invert;
	int32 NrPlotPens;
	int32 OutputNeutral;
	int32 PlotBoardOutline;
	int32 DrillOutputOption;
	int32 DrillAsGerber;
	int32 PlotBoardOutlineSingle;
	int32 BitmapExportSaveMode;
	int32 ReverseLayerNumbering;
	int32 GerbvProject;
	double ScaleFactor, PenSizes[16], PenSpeed, Xoffset, Yoffset, BitmapExportResolution, AreaFillPen1, AreaFillPen2;
	char TextLine[8][MAX_LENGTH_STRING];
	char PcbMaterial[MAX_LENGTH_STRING];
	char PcbThickness[MAX_LENGTH_STRING];
	char PcbCopperThickness[MAX_LENGTH_STRING];
	char Extra1[MAX_LENGTH_STRING * 3];
} GerberInfoRecord;


typedef struct
{
	int32 PaperSize;
	int32 PaperOrientation;
	int32 PaperFitToPage;
	int32 Invert;
	int32 PlotBoardOutline;
	int32 PlotBoardOutlineSingle;
	int32 ComponentReferenceMode;
	double ScaleFactor, PenSpeed, Xoffset, Yoffset;
	char TextLine[8][MAX_LENGTH_STRING];
} PDFInfoRecord;


typedef struct
{
	int32 NrTraces, NrVias, NrThruHolePads, NrSmdPads, Info, Object3Nr;
	double x, y;
} TracePointRecord;

typedef TracePointRecord TracePointsArray[1000];


typedef struct
{
	int32 ObjectType, Info2, Info3, Info4, GateNr, SwapInfo;
} GatePinSwapRecord;

typedef GatePinSwapRecord GatePinSwapArray[200];

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


#pragma pack(1)

typedef struct
{
	char Identifier[6];
	int16 Width;
	int16 Height;
	uint8 ColorMode;
	uint8 BackGroundColorIndex;
	uint8 Zero;
	uint8 Color0[3];
	uint8 Color1[3];
} GifHeaderRecord;

#pragma pack()

typedef struct
{
	int32 ColorNr, Layer;
	char TextStr[40];
} LayerObjectCodesRecord;

typedef struct
{
	int32 Units;
	int32 ValuePartNr;
	int32 NotPlaced;
	int32 SmdThroughHole;
} CompPosOutputRecord;

typedef int32(*FUNCP1) (double, double, int32);
typedef int32(*FUNCP2) (double, double, double);
typedef int32(*FUNCP3) (double, int32);
typedef int32(*FUNCP4) (int32);
typedef int32(*FUNCP5) (double, double, int32, int32);
typedef int32(*FUNCP6) (double, double, double, double, double, double, double, int32);
typedef int32(*FUNCP7) (int32, double, double, int32);
typedef int32(*FUNCP8) (double, double, double, int32);
typedef int32(*FUNCP9) (double, double);

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
	void *Param1[8];
	void *Param2[8];
	int32 Mode;
} DrawXorFunctionRecord;

#endif
