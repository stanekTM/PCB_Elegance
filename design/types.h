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

#define TotalNrDesigns                          24

#define CONNECTION_PASSIVE                      0x0000
#define CONNECTION_INPUT                        0x0001
#define CONNECTION_OUTPUT                       0x0002
#define CONNECTION_TRISTATE                     0x0003
#define CONNECTION_IO                           0x0004
#define CONNECTION_OC                           0x0005
#define CONNECTION_POWER                        0x0010

#define CONNECTION_UNDEFINED                    0x0000
#define CONNECTION_TTL                          0x0001
#define CONNECTION_LVTTL                        0x0002
#define CONNECTION_CMOS                         0x0003
#define CONNECTION_ANALOG                       0x0004
#define CONNECTION_POWER                        0x0010

#define POWER_CONNECTION                        CONNECTION_POWER*257


#define WIRE                                    0x0100
#define BUS                                     0x0200
#define BUS_INFO                                0x0280
#define JUNCTION                                0x0300
#define BUS_CONNECTION                          0x0400
#define GLOBAL_CONNECTION                       0x0500
#define NET_LABEL                               0x0600
#define PIN_OBJECT                              0x0700
#define ONE_PIN_NET                             0x0800
#define SYMBOL                                  0x1000
#define SYMBOL_PIN                              0x1100
#define SYMBOL_PIN_TEXT                         0x1200
#define SYMBOL_POWERPIN                         0x1300
#define SYMBOL_POWERPIN_TEXT                    0x1400
#define SYMBOL_PINBUS                           0x1500
#define SYMBOL_PINBUS_TEXT                      0x1600
#define SYMBOL_LINE                             0x1700
#define SYMBOL_RECT                             0x1800
#define SYMBOL_CIRCLE                           0x1900
#define SYMBOL_ARC                              0x1A00
#define SYMBOL_TEXT                             0x1B00

#define INSTANCE                                0x1C00

#define INFO_LINE                               0x2000
#define INFO_TEXT                               0x2100
#define INFO_RECT                               0x2200
#define INFO_ARC                                0x2300
#define INFO_CIRCLE                             0x2400

#define INSTANCE_REF_TEXT                       0x3000
#define INSTANCE_REF_VALUE                      0x3100


#define TEXT_NOT_VISIBLE                        0x1000

#define OBJECT_ROTATE90                         0x0001
#define OBJECT_MIRRORX                          0x0010
#define OBJECT_MIRRORY                          0x0020
#define NO_GEOMETRY                             0x0400


#define OBJECT_VERTIX1_SELECTED                 0x0001
#define OBJECT_VERTIX2_SELECTED                 0x0002

#define OBJECT_DONE                             0x0010
#define OBJECT_DONE2                            0x0020
#define MULTIPLE_SYMBOLS                        0x0040
#define OBJECT_ERROR                            0x0080

#define OBJECT_DELETED                          0x0100
#define OBJECT_UNDO                             0x0200
#define SHEET_SYMBOL                            0x0400
#define OBJECT_NOT_VISIBLE                      0x0800

#define OBJECT_PROTECTED                        0x1000
#define OBJECT_CHANGED_TO_SELECTED              0x2000


#define DEMO_VERSION_PINCOUNT                   (200*314)
#define DEMO_VERSION_ARCCOUNT                   (10*314)
#define SMALL_VERSION_PINCOUNT                  (500*314)
#define VERSION1_PINCOUNT                       (1000*314)
#define VERSION2_PINCOUNT                       (2000*314)


#define NET_VISIBLE                             1
#define MODE_OBJECTS2                           2
#define MODE_OBJECTS3                           3

#define ALIGN_LEFT_BOTTOM                       0
#define ALIGN_LEFT_CENTRE                       1
#define ALIGN_LEFT_TOP                          2
#define ALIGN_CENTRE_BOTTOM                     3
#define ALIGN_CENTRE_CENTRE                     4
#define ALIGN_CENTRE_TOP                        5
#define ALIGN_RIGHT_BOTTOM                      6
#define ALIGN_RIGHT_CENTRE                      7
#define ALIGN_RIGHT_TOP                         8


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
#define INNER_PAD_LAYER                         4300
#define POWER_PAD_LAYER                         4400
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

#define OBJECT_WITH_CLEARANCE                   0x0100
#define OBJECT_FILLED                           0x0200
//#define OBJECT_DONE                             0x0400
#define OBJECT_WARNING                          0x0800
//#define OBJECT_NOT_VISIBLE                      0x1000

#define OBJECT_HIGHLITED                        0x2000
#define OBJECT_SELECTED                         0x4000
#define OBJECT_CHANGED                          (OBJECT_HIGHLITED+\
                                                COMPONENT_PROTECTED+\
                                                OBJECT_SELECTED)

#define REF_WITH_LEADING_ZERO                   8
#define REF_WITH_TWO_LEADING_ZEROS              16
#define REF_WITH_THREE_LEADING_ZEROS            24

#define PI                                      3.14159265358979262
#define ANGLE_45                                (PI*0.25)
#define ANGLE_90                                (PI*0.5)
#define ANGLE_135                               (PI*0.75)
#define ANGLE_180                               (PI*1.0)
#define ANGLE_225                               (PI*1.25)
#define ANGLE_270                               (PI*1.5)
#define ANGLE_315                               (PI*1.75)
#define ANGLE_360                               (PI*2.0)

#define SQRT05                                  0.70710678118654752
#define VALUE_1_MIN_SQRT05                      0.29289321881345248
#define VALUE_SQRT2_MIN_1                       0.41421356237309504
#define SQRT2                                   1.41421356237309504
#define SQR(x)                                  ((x)*(x))
#define ANGLE_CONVERT(Rotation)                 (((Rotation)*PI/180.0))

#define BusSizeX                                (float)2.0
#define BusSizeY                                (float)0.3

#define DefFontSize                             (float)0.833333333333

#define STANDARD_LINE_THICKNESS                 (float)0.1
#define STANDARD_WIRE_THICKNESS                 (float)0.1
#define STANDARD_BUS_THICKNESS                  (float)0.3

#define MAX_NR_SHEETS                           256

#define MAX_NR_BOM_COLUMNS                      32

#define DefMaxNrWires                           1024	// 28
#define DefMaxNrBusses                          128	// 28
#define DefMaxNrJunctions                       256	// 16
#define DefMaxNrOnePinNets                      256	// 16
#define DefMaxNrBusConnections                  512	// 16
#define DefMaxNrNetLabels                       1024	// 64

#define DefMaxNrObjectLines                     256	// 24
#define DefMaxNrObjectCircles                   128	// 20
#define DefMaxNrObjectRects                     128	// 24
#define DefMaxNrObjectArcs                      64	// 40
#define DefMaxNrObjectTexts                     128	// 84

#define DefMaxNrSheetSymbols                    128	// 16
#define DefMaxNrInstances                       1024	// 272
#define DefMaxSymbolsMemory                     32*1024	// 65536
//                                                          -----------
//                                                             120k

#define DefMaxNrGlobalConnections               256
#define DefMaxNrPins                            1024
#define DefMaxNrPowerPins                       32
#define DefMaxNrSubPinDefs                      400
#define DefMaxNrPinBusses                       20
#define DefMaxNrObjects                         1024
#define DefMaxNrObjects2                        512

#define LimitMaxNrWires                         8192
#define LimitMaxNrBusses                        4096
#define LimitMaxNrJunctions                     1024
#define LimitMaxNrBusConnections                8192
#define LimitMaxNrNetLabels                     8192

#define LimitMaxNrObjectLines                   8192
#define LimitMaxNrObjectCircles                 2048
#define LimitMaxNrObjectRects                   2048
#define LimitMaxNrObjectArcs                    2048
#define LimitMaxNrObjectTexts                   2048

#define LimitMaxNrSheetSymbols                  512
#define LimitMaxNrInstances                     8192
#define LimitMaxSymbolsMemory                   1024*1024
#define LimitMaxNrGlobalConnections             4096
#define LimitMaxNrPins                          4096
#define LimitMaxNrPowerPins                     512
#define LimitMaxNrSubPinDefs                    400
#define LimitMaxNrPinBusses                     512

#define LimitMaxNrObjects                       16384

#define NrDeleteInfo                            100
#define MaxNrActions                            100

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

#define LibraryCode1                            "Symbol library version 1.0"
#define LibraryCode2                            "Geometry library version 1.0"
#define SheetCode1                              "Sheet version 1.0"
#define SheetCode2                              "Sheet version 2.0"
#define SheetCode3                              "Sheet version 3.0"
#define SheetCode4                              "Sheet version 4.0"
#define SheetCode5                              "Sheet version 5.0"
#define SymbolCode1                             "Symb1.0"
#define SymbolCode2                             "Symb2.0"
#define SymbolCode3                             "Symb3.0"
#define ShapeCode                               "Shape definition 1.0"
#define ShapeCode2                              "Shape definition 1.5"
#define ShapeCode3                              "Shape definition 2.0"


#define MEM_OBJECTS2                            0
#define MEM_ANNOTATE                            1
#define MEM_SHAPES                              2
#define MEM_SHAPES_MEM                          3
#define MEM_PDF_OBJECTS                         4
#define MEM_LIBNAMES                            5


#define NotInRange(x1,x2) ( (((x1>x2-0.01) && (x1<x2+0.01))) ? (0) : (1) )
#define    InRange(x1,x2) ( (((x1>x2-0.01) && (x1<x2+0.01))) ? (1) : (0) )
#define    InRange9(x1,x2)  ( ((((x1)>(x2)-20000.0)  && ((x1)<(x2)+20000.0)))  ? (1)  : (0) )
#define X1SmallerThenX2(x1,x2) ( (x1<x2-0.01) ? (1) : (0) )
#define X1GreaterThenX2(x1,x2) ( (x1>x2+0.01) ? (1) : (0) )
#define INT64SHIFT(x) ((int64)(((int64)1) << x))
#define  InRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (1) : (0) )
#define  NotInRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (0) : (1) )

typedef struct
{
	char Identification[32], EditingPerson[32];
	int32 FileVersion, Revision;
	float BoardCentreX, BoardCentreY, BoardWidth, BoardHeight;
	int32 NrSymbols, NrInstances, NrWires, NrBusses, NrBusConnections, NrNetLabels, NrJunctions, NrGlobalConnections,
	      NrObjectLines, NrObjectCircles, NrObjectRects, NrObjectArcs, NrObjectTexts, SymbolsMem, NrRedefinedPinBusses;
	float ArrowLength, DimensionHeight;
	int32 NrOnePinNets;
	int32 AnnotateStartNumber;
	uint8 Unused[220];
	char LastInstCode[8];
	struct DesignDateStruct
	{
		int32 Year;
		uint8 Month, Day, Hour, Minutes;
	} DesignDate;
} DesignRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NrPins, PackagePartNr;
	int16 RefInfo, ValueInfo, SymbolInfo;
	int32 Code1, Code2, Code3, Error, PlacingOption;
	float OriginX, OriginY, RefOriginX, RefOriginY, ValueOriginX, ValueOriginY, BoardPosMinX, BoardPosMinY,
	      BoardPosMaxX, BoardPosMaxY;
	char Reference[8], InstCode[8], SymbolName[32], Value[32], PartNr[32], Geometry[32], PartDescription[64];
} OldOldInstanceRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NrPins, PackagePartNr;
	int16 RefInfo, ValueInfo, SymbolInfo;
	int32 Code1, Code2, Code3, Error, PlacingOption;
	float OriginX, OriginY, RefOriginX, RefOriginY, ValueOriginX, ValueOriginY, BoardPosMinX, BoardPosMinY,
	      BoardPosMaxX, BoardPosMaxY;
	char Reference[8], InstCode[8], SymbolName[32], Value[32], PartNr[32], Geometry[32], PartDescription[64],
	     Properties[256];
} OldInstanceRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NrPins, PackagePartNr;
	int16 RefInfo, ValueInfo, SymbolInfo;
	int32 Code1, Code2, Code3, Error, PlacingOption;
	float OriginX, OriginY, RefOriginX, RefOriginY, ValueOriginX, ValueOriginY, BoardPosMinX, BoardPosMinY,
	      BoardPosMaxX, BoardPosMaxY;
	char Reference[8], InstCode[8], SymbolName[32], Value[32], PartNr[128], Geometry[32], PartDescription[64],
	     Properties[256];
} InstanceRecord;

typedef InstanceRecord InstancesArray[DefMaxNrInstances];

typedef struct
{
	int32 Pos, Length;
	int16 Info, AddNr, DeleteNr, LocalSymbol;
	int32 Code1, Code2, Code3, Code4;
	char SymbolName[32];
} SymbolsPosRecord;

typedef SymbolsPosRecord SymbolsPosArray[DefMaxNrSheetSymbols];

typedef struct
{
	int32 Pos, Length;
	char LibName[256];
	char SymbolName[32];
} SymbolsPos2Record;

typedef SymbolsPos2Record SymbolsPos2Array[DefMaxNrSheetSymbols];

typedef struct
{
	char SymbolIdent[8];
	int32 MemSize, Revision, NrPins, NrPowerPins, NrPinBusses, NrPartsPerPackage, NrSubPinDefs, NrObjectLines,
	      NrObjectCircles, NrObjectRects, NrObjectArcs, NrObjectTexts, Info;
	float OriginX, OriginY, RefOriginX, RefOriginY, ValueOriginX, ValueOriginY;
	char Name[32], InterfaceName[32], InitialReference[8], Description[64];
} SymbolRecord;


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
	int16 Info, AddNr, DeleteNr;
	int16 NameInfo, ConnectionType, SwapInfo;
	float X, Y, NameX, NameY;
	char Name[12];
	char Label[24];
} PinRecord;

typedef PinRecord PinsArray[DefMaxNrPins];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NameInfo;
	float NameX, NameY;
	char NetName[32];
	char Text[128];
} PowerPinRecord;

typedef PowerPinRecord PowerPinsArray[DefMaxNrPowerPins];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 NameInfo, SwapInfo, ConnectionType, NrPins, Dummy;
	float X, Y, NameX, NameY;
	char Label[32];
	char Text[372];
} PinBusRecord;

typedef PinBusRecord PinBusArray[DefMaxNrPinBusses];

typedef struct
{
	int16 Info, AddNr, DeleteNr, Info2;
	int32 Unused[6];
	char Reference[32];
	char Name[32];
	uint8 Order[64];
} RedefinedPinBusRecord;

typedef RedefinedPinBusRecord RedefinedPinBusArray[DefMaxNrPinBusses];


typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 ConnectionType;
	float X, Y;
	float NameX, NameY;
	int16 NameInfo, NetNr;
	char Text[28];
} GlobalConnectionRecord;

typedef GlobalConnectionRecord GlobalConnectionsArray[DefMaxNrGlobalConnections];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 ObjectType, LineMode, Dummy;
	float X1, Y1, X2, Y2;
} OldObjectLineRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr, LineMode;
	float Thickness;
	float Dummy;
	float X1, Y1, X2, Y2;
} ObjectLineRecord;

typedef ObjectLineRecord ObjectLinesArray[DefMaxNrObjectLines];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 ObjectType, LineMode, Dummy;
	float CentreX, CentreY, Width, Height;
} OldObjectRectRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr, LineMode;
	float Thickness;
	float Dummy;
	float CentreX, CentreY, Width, Height;
} ObjectRectRecord;

typedef ObjectRectRecord ObjectRectsArray[DefMaxNrObjectRects];

typedef struct
{
	int16 Info, AddNr, DeleteNr, LineMode;
	int32 CircleMode;
	float Thickness;
	float Dummy;
	float CentreX, CentreY, Diam;
} ObjectCircleRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 ObjectType, LineMode, CircleMode;
	float CentreX, CentreY, Diam;
} OldObjectCircleRecord;

typedef ObjectCircleRecord ObjectCirclesArray[DefMaxNrObjectCircles];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 ObjectType, LineMode, Dummy;
	float CentreX, CentreY, StartDiffX, StartDiffY, EndDiffX, EndDiffY, Width, Height;
} OldObjectArcRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr, LineMode;
	float Dummy;
	float Thickness;
	float CentreX, CentreY, StartDiffX, StartDiffY, EndDiffX, EndDiffY, Width, Height;
} ObjectArcRecord;

typedef ObjectArcRecord ObjectArcsArray[DefMaxNrObjectArcs];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 ObjectType, TextMode, Dummy;
	float X, Y, FontHeight;
	char Text[32];
} OldObjectTextRecord;

typedef struct
{
	int16 Info, AddNr, DeleteNr, TextMode;
	float Rotation;
	float Thickness;
	float Dummy;
	float X, Y, FontHeight;
	char Text[256];
} ObjectTextRecord;

typedef ObjectTextRecord ObjectTextsArray[DefMaxNrObjectTexts];

typedef struct
{
	int16 Info, AddNr, DeleteNr;
	int16 ObjectType;
} ObjectInfoRecord;


/*

PinConnectionType :

0    : Passive
1    : Input
2    : Output
3    : Tristate
4    : I/O
5    : OC
6    : Power

PinConnectionLogic :

0    : Passive
1    : TTL
2    : LVTTL
3    : CMOS
4    : Analog
16   : Power


*/

typedef struct
{
	float X1, Y1, X2, Y2;
	int32 NetLabelPointer;
	int16 NetNr, Info, AddNr, DeleteNr;
} WireRecord;

typedef WireRecord WiresArray[DefMaxNrWires];

typedef struct
{
	float X1, Y1, X2, Y2;
	int32 NetLabelPointer;
	int16 NetNr, Info, AddNr, DeleteNr;
} BusRecord;

typedef BusRecord BussesArray[DefMaxNrBusses];


typedef struct
{
	float X, Y;
	int16 NetNr, Info, AddNr, DeleteNr;
} JunctionRecord;

typedef JunctionRecord JunctionsArray[DefMaxNrJunctions];

typedef struct
{
	float X, Y;
	int16 Test, Info, AddNr, DeleteNr;
} OnePinNetRecord;

typedef OnePinNetRecord OnePinNetsArray[DefMaxNrOnePinNets];

typedef struct
{
	float X, Y, TextX, TextY;
	int16 NetNr, Alignment, Info, Dummy, AddNr, DeleteNr;
} BusConnectionRecord;

typedef BusConnectionRecord BusConnectionsArray[DefMaxNrBusConnections];

typedef struct
{
	float ConnectX, ConnectY;
	float TextX, TextY;
	int16 Alignment, LabelType, Info, NetNr, AddNr, DeleteNr;
	char Name[36];
} NetLabelRecord;

typedef NetLabelRecord NetLabelsArray[DefMaxNrNetLabels];

typedef struct
{
	int32 ObjectType, ObjectType2, Alignment, Info, Info2, Info3, Info4, Info5, NetNr, PinInfo, PinNr, Layer, CompNr,
	      Mirror, PinCount, TraceNr;
	double x1, y1, x2, y2, x3, y3, x4, y4, Clearance, RotationAngle, Thickness, minx, miny, maxx, maxy;
	InstanceRecord *Instance;
	LPSTR ShapeName;
	uint8 *Address;
	LPSTR Text1;
	LPSTR Text2;
} ObjectRecord;

typedef ObjectRecord ObjectArray[DefMaxNrObjects];

typedef struct
{
	int32 ObjectType, Info, Info2, Info3, SheetNr, NetNr, Info4, Info5, PlacingOption, NewNetNr, ObjectNr, Text1, Text2,
	      Text3, Properties, Description, RefNum, Value;
	double x1, y1, x2, y2;
} Object2Record;

typedef Object2Record Object2Array[DefMaxNrObjects];

typedef struct
{
	float x1, y1, x2, y2;
	int16 ObjectType, SheetNr;
	int32 FirstObjectNr, BusNr, NetNr;
} Object5Record;

typedef Object5Record Object5Array[DefMaxNrObjects];

typedef char SubPinDefsType[12];

typedef SubPinDefsType SubPinDefsArray[100];

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


/***********************************************************************/

typedef struct
{
	int32 x1, y1, x2, y2;
} PolyLineRecord;

typedef PolyLineRecord PolyLineArray[16];

typedef struct
{
	int32 NrPolyLines;
	float Line[51];
} CharRecord;

typedef CharRecord CharsArray[94];

typedef struct
{
	float Xoffset, Yoffset, Factor;
} ViewPosRecord;


typedef ViewPosRecord ViewPosArray[20];

typedef struct
{
	float TraceX, TraceY, Xoffset, Yoffset, Factor;

} LastTracePosRecord;

typedef LastTracePosRecord LastTracePosArray[20];

// typedef int NetInfoArray[DefMaxNrNets];

typedef struct
{
	float Xoffset, Yoffset, Factor, DisplX, DisplY;
	HWND WindowHandle;
	char WindowName[80];
} WindowSheetRecord;

typedef struct
{
	float x1, y1;
	int32 Start, Step, Count;
} ObjectNumbersRecord;

typedef struct
{
	char SheetName[80];
	int32 Nr, Level, LinkToAbove, NrDownSheets, InstancePos, InstancePos2, NrInstances, SheetMemSize, NetPos, NrNets,
	      SymbolMemSize, Info;
	HMENU SubMenu;
} SheetRecord;

typedef struct
{
	char SymbolName[32];
	char InterfaceName[32];
	char Value[32];
	char PartNr[128];
	char Description[64];
	char Geometry[32];
	char Reference[8];
	float RealCompValue, x, y;
	int32 SheetNr, InstanceNr, SymbolInfo, Info, MultiSymbolNr, PlacingOption, Error, RefNum, MultiPartNr, RefCodeNr,
	      NrSymbolsPerComponent, PackagePartNr, NrPartsPerPackage;
	uint32 CrcPackageBytes;
	char Properties[256];
} ComponentRecord;

typedef ComponentRecord ComponentArray[2000];

typedef struct
{
	char SymbolName[32];
	char Value[32];
	char RefName[8];
	int32 Count, LastRef, PartsPerPackage, SheetCount, UsedParts;
} PartNameRecord;

typedef PartNameRecord PartNameArray[2000];

typedef struct
{
	char SymbName[32];
	char SymbInterfaceNames[8][32];
	char RefName[32][8];
	char Reference[8];
	uint8 SymbInterfaceNamesOccupation[32];
	int32 NrSymbInterfaceNames;
	int16 ReferenceNr[32];
} MultipleSymbolRecord;

typedef MultipleSymbolRecord MultipleSymbolArray[32];

typedef struct
{
	int32 Pos, Info, Progress, SheetNr, NetLabelObjectNr, GlobalConnObjectNr, Used, PinBusObjectNr,
	      BusConnectionObjectNr, NrPinsPinBus, PinBusCount;
	int64 Presence;
	LPSTR PowerNetName;
} NetInfoRecord;

typedef NetInfoRecord NetInfoArray[2000];

typedef struct
{
	int32 Name, Pos;
	int16 Info, Info2, PinBusCode, PinBusNr;
} ObjectsInfoRecord;

typedef ObjectsInfoRecord ObjectsInfoArray[2000];

typedef int16 int16Array[2000];
typedef int32 int32Array[2000];

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
	int32 BoardOutlineSize;
	float BoardOriginX, BoardOriginY, BoardWidth, BoardHeight, StandardTraceWidth, StandardClearance, MaximumClearance,
	      PowerPlaneBorder, SilkScreenWidth;
	int32 NrBoardLayers, NrTraceLayers, NrPowerPlanes;
	int32 NrShapes, NrNets, NrComps, NrAreaFills, NrPins;
	int32 NrHorTraces[32], NrVerTraces[32], NrDiag1Traces[32], NrDiag2Traces[32];
	int32 LayerInfo[32];
	int32 NrAreaFillsInPowerPlane[32];
	char LayerText[32][32];
	int32 Unused[32];
	ViaRecord DefVia1, DefVia2, DefVia3, DefVia4;
	uint8 Reserved[1872];
	int32 NrVias, NrConnections, NrObjectLines, NrObjectCircles, NrObjectRects, NrObjectArcs, NrObjectTexts;
	struct DesignDateStruct2
	{
		int32 Year;
		uint8 Month, Day, Hour, Minutes;
	} DesignDate;
	int32 ShapesMem, CompsMem, AreaFillMem;
} PCBDesignRecord;

typedef int32 CompsArray[10000];

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
	int32 NetNr;
} CompPinRecord;

typedef struct
{
	int32 GeometryIndex;
	char OrcadFile[200];
	char OutputDirectory[200];
	char GeometryConversionFileName[200];
} OrcadConversionRecord;

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

typedef float ShapeLinesArray[128];

typedef struct
{
	int32 ShapePos, ObjectPos, NrObjects, ShapeSMD;
	char ShapeName[32];
} ShapeInfoRecord;

typedef ShapeInfoRecord ShapesArray[256];

typedef struct
{
	double x, y;
} PointRecord;

typedef PointRecord PointsArray[200];

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
	int32 ColumnUsed, NrColumnsUsed, PropertyIndex, NrUniqueProperties, BOMSortNr, ColumnsUsed[MAX_NR_BOM_COLUMNS],
	      ColumnWidth[MAX_NR_BOM_COLUMNS];
	char ColumnStr[MAX_NR_BOM_COLUMNS][32], UniqueProperties[16][32];
} BOMRecord;

#endif
