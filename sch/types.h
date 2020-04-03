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
#include "utf8.h"
#include "commctrl.h"

#define SC(Nr,string) (StringConvert(Nr,string))

#define  LibraryCode1   "Symbol library version 1.0"
#define  SheetCode1     "Sheet version 1.0"
#define  SheetCode2     "Sheet version 2.0"
#define  SheetCode3     "Sheet version 3.0"
#define  SheetCode4     "Sheet version 4.0"
#define  SheetCode5     "Sheet version 5.0"
#define  SymbolCode1    "Symb1.0"
#define  SymbolCode2    "Symb2.0"
#define  SymbolCode3    "Symb3.0"

#define  CharWidthFactor                        0.8
#define  STANDARD_LINE_THICKNESS                0.1
#define  STANDARD_WIRE_THICKNESS                0.1
#define  STANDARD_BUS_THICKNESS                 0.3

//  Net info

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


#define STANDARD_OBJECT_LINE                    0x1000	// 4096
#define STANDARD_OBJECT_RECT                    0x1200	// 4608
#define STANDARD_OBJECT_CIRCLE                  0x1300	// 4864
#define STANDARD_OBJECT_ARC                     0x1400	// 5120
#define STANDARD_OBJECT_TEXT                    0x1500	// 5376

#define INSTANCE_REF_TEXT                       0x3000
#define INSTANCE_REF_VALUE                      0x3100


#define TEXT_NOT_VISIBLE                        0x1000

#define OBJECT_ROTATE90                         0x0001
#define OBJECT_MIRRORX                          0x0010
#define OBJECT_MIRRORY                          0x0020
#define NO_GEOMETRY                             0x0400

#define CHANGED_GEOMETRY                        0x0800
#define CHANGED_REFERENCE                       0x1000
#define CHANGED_VALUE                           0x2000
#define CHANGED_PLACING_OPTION                  0x4000

#define OBJECT_VERTIX1_SELECTED                 0x0001
#define OBJECT_VERTIX2_SELECTED                 0x0002

#define OBJECT_DONE                             0x0010
#define OBJECT_DONE2                            0x0020
#define MULTIPLE_SYMBOLS                        0x0040
#define OBJECT_ERROR                            0x0080

#define OBJECT_DELETED                          0x0100
#define SHEET_SYMBOL                            0x0400
#define OBJECT_NOT_VISIBLE                      0x0800

#define OBJECT_PROTECTED                        0x1000
#define OBJECT_SELECTED                         0x2000


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

#define BusSizeX                                (float)2.0
#define BusSizeY                                (float)0.3

#define DefFontSize                             (float)0.833333333333
#define PDFCharWidthFactor                      0.8


#define DefMaxNrWires                           1024	// 28
#define DefMaxNrBusses                          128	// 28
#define DefMaxNrJunctions                       512	// 16
#define DefMaxNrOnePinNets                      256	// 16
#define DefMaxNrBusConnections                  256	// 16
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


#define LimitMaxNrWires                         32768
#define LimitMaxNrBusses                        32768
#define LimitMaxNrJunctions                     32768
#define LimitMaxNrOnePinNets                    32768
#define LimitMaxNrBusConnections                32768
#define LimitMaxNrNetLabels                     32768

#define LimitMaxNrObjectLines                   32768
#define LimitMaxNrObjectCircles                 32768
#define LimitMaxNrObjectRects                   32768
#define LimitMaxNrObjectArcs                    32768
#define LimitMaxNrObjectTexts                   8192

#define LimitMaxNrSheetSymbols                  512
#define LimitMaxNrInstances                     8192
#define LimitMaxSymbolsMemory                   1024*1024
#define LimitMaxNrGlobalConnections             4096
#define LimitMaxNrPins                          4096
#define LimitMaxNrPowerPins                     512
#define LimitMaxNrSubPinDefs                    400
#define LimitMaxNrPinBusses                     512

#define LimitMaxNrObjects                       16384
#define LimitMaxNrStandardObjects               16384

#define MaxCompSelect                           4000

#define MaxNrSymbolAttributes                   40

#define NotInRange(x1,x2) ( (((x1>x2-0.01) && (x1<x2+0.01))) ? (0) : (1) )
#define    InRange(x1,x2) ( (((x1>x2-0.01) && (x1<x2+0.01))) ? (1) : (0) )
#define X1SmallerThenX2(x1,x2) ( (x1<x2-0.01) ? (1) : (0) )
#define X1GreaterThenX2(x1,x2) ( (x1>x2+0.01) ? (1) : (0) )

#define ARROW_LENGTH                            1.0
#define DIMENSION_HEIGHT                        1.0


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


#define MEM_OBJECTS2                            0
#define MEM_OBJECTS3                            1
#define MEM_PROPERTIES                          2
#define MEM_LIBNAMES                            3
#define MEM_USERVARS                            4
#define MEM_COMPSELECT                          5
#define MEM_USERVARS2                           6

// *******************************************************************************************************
// *******************************************************************************************************

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
	int32 SheetInfo;
	uint8 Unused[216];
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
4    : Input/Output
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
	int32 ObjectType, Alignment, Info, Info2, Info3, Info4, Info5, NetNr, PinInfo, PinNr;
	double x1, y1, x2, y2, x3, y3, x4, y4, minx, miny, maxx, maxy, Thickness;
	InstanceRecord *Instance;
	LPSTR Text1;
	LPSTR Text2;
	LPSTR Text3;
} ObjectRecord;

typedef ObjectRecord ObjectArray[DefMaxNrObjects];

typedef struct
{
	int32 ObjectType, Alignment, Info, Info2, Info3, Info4, ObjectNr;
	double x1, y1, x2, y2;
	InstanceRecord *Instance;
	LPSTR Text1;
	LPSTR Text2;
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
	double Xoffset, Yoffset, Factor;
} ViewPosRecord;


typedef ViewPosRecord ViewPosArray[20];

typedef struct
{
	double TraceX, TraceY, Xoffset, Yoffset, Factor;

} LastTracePosRecord;

typedef LastTracePosRecord LastTracePosArray[20];

// typedef int NetInfoArray[DefMaxNrNets];

typedef struct
{
	double Xoffset, Yoffset, Factor, DisplX, DisplY;
	HWND WindowHandle;
	char WindowName[80];
} WindowSheetRecord;

typedef struct
{
	double x1, y1;
	int32 Start, Step, Count;
} ObjectNumbersRecord;

typedef char SearchSymbolResultsArray[512][128];

typedef struct
{
	double x, y;
} PointRecord;

typedef struct
{
	int32 ObjectType, Alignment, Info, Info2, Layer;
	double x1, y1, x2, y2, x3, y3, x4, y4, Rotation, Thickness, Clearance, minx, miny, maxx, maxy;
	char Text[176];
} StandardObjectRecord;

typedef StandardObjectRecord StandardObjectArray[4096];

typedef struct
{
	int32 Info, Info1, Info2, Code1, Code2;
	char Name[48];
	TVINSERTSTRUCT TreeItemInfo;
	HTREEITEM TreeItem;
} CompSelectRecord;

typedef CompSelectRecord CompSelectArray[MaxCompSelect];

typedef struct
{
	int32 Index;
	char Name[16];
} SubPinDefsNameRecord;

typedef struct
{
	char ID[32];
	char Value[64];
} UserVarRecord;

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
