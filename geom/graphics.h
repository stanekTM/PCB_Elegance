/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: graphics.h
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


#ifndef _GRAPHICS

#define _GRAPHICS

#include "owntypes.h"
#include "windows.h"

#define ShapeSilkScreenTopObjectCode            100
#define ShapeSilkScreenTopObjectCode2           101
#define ShapeSilkScreenBottomObjectCode         102
#define ShapeSilkScreenBottomObjectCode2        103
#define ShapePlacementOutLineObjectCode         104
#define ShapePinsDrillUnplatedObjectCode        105
#define ShapePinsDrillObjectCode                106
#define ShapePinsDrillUnplatedObjectCode2       107
#define ShapePinsDrillObjectCode2               108
#define ShapePinsTopObjectCode                  109
#define ShapePinsBottomObjectCode               110
#define ShapePinsInnerObjectCode                111
#define ShapeSoldMaskTopObjectCode              112
#define ShapeSoldMaskBottomObjectCode           113
#define ShapePasteMaskTopObjectCode             114
#define ShapePasteMaskBottomObjectCode          115
#define ShapePinsTopObjectCode2                 116
#define ShapePinsBottomObjectCode2              117
#define ShapePinsInnerObjectCode2               118
#define ShapeSoldMaskTopObjectCode2             119
#define ShapeSoldMaskBottomObjectCode2          120
#define ShapePasteMaskTopObjectCode2            121
#define ShapePasteMaskBottomObjectCode2         122
#define SpecialDrawObjectCode                   123
#define ShapeCompOutlineObjectCode              124
#define ShapeCompOutlineObjectCode2             125
#define ShapeRoutingKeepoutTopObjectCode        126
#define ShapeRoutingKeepoutTopObjectCode2       127
#define ShapeRoutingKeepoutBottomObjectCode     128
#define ShapeRoutingKeepoutBottomObjectCode2    129
#define ShapeRoutingKeepoutInnerObjectCode      130
#define ShapeRoutingKeepoutInnerObjectCode2     131
#define ShapePowerPadObjectCode                 132
#define ShapeGeomNameObjectCode                 133
#define ShapeBoardOutlineObjectCode             134
#define ShapeInfo1ObjectCode                    135
#define ShapeInfo2ObjectCode                    136
#define ShapeInfo3ObjectCode                    137
#define ShapeInfo4ObjectCode                    138
#define ShapeInfo1ObjectCode2                   139
#define ShapeInfo2ObjectCode2                   140
#define ShapeInfo3ObjectCode2                   141
#define ShapeInfo4ObjectCode2                   142

#define PinTextFontCode                         200


#define ClearanceObjectCode                     200
#define StippelObjectCode                       201
#define YellowObjectCode                        202
#define MagentaObjectCode                       203
#define WhiteObjectCode                         204
#define WhiteObjectCode2                        205
#define BlackObjectCode                         206
#define GrayObjectCode                          207
#define ButtonInfoObjectCode                    208
#define BackGroundObjectCode1                   209
#define BackGroundObjectCode2                   210
#define YellowObjectCode2                       211

#define NrColors                                19

#define ShapeSilkScreenTopColorNr               0
#define ShapeSilkScreenBottomColorNr            1
#define ShapeCompOutlineColorNr                 2
#define ShapePlacementOutLineColorNr            3
#define ShapePinsDrillColorNr                   4
#define ShapePinsDrillUnplatedColorNr           5
#define ShapePinsTopColorNr                     6
#define ShapePinsBottomColorNr                  7
#define ShapePinsInnerColorNr                   8
#define ShapePasteMaskTopColorNr                9
#define ShapePasteMaskBottomColorNr             10
#define ShapeSoldMaskTopColorNr                 11
#define ShapeSoldMaskBottomColorNr              12
#define ShapeRoutingKeepoutTopColorNr           13
#define ShapeRoutingKeepoutBottomColorNr        14
#define ShapeRoutingKeepoutInnerColorNr         15
#define ShapeGeomNameColorNr                    16
#define ShapePowerPadColorNr                    17
#define ClearanceColorNr                        18
#define ButtonInfoColorNr                       19
#define GridColorNr                             20
#define BackGroundColorNr                       21
#define PinTextColorNr                          22
#define ShapeBoardOutlineColorNr                23
#define ShapeInfo1ColorNr                       24
#define ShapeInfo2ColorNr                       25
#define ShapeInfo3ColorNr                       26
#define ShapeInfo4ColorNr                       27

typedef struct
{
	uint16 palVersion;
	uint16 palNumEntries;
	COLORREF Colors[64];
} GEOMPaletteRecord;




extern int32 DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY;
extern int32 CurrentObjectCode, CurrentFontCode;
extern double ViewMinX, ViewMinY, ViewMaxX, ViewMaxY;

extern HPALETTE GEOMPalette, CurrentPalette;
extern HDC OutputDisplay;

extern COLORREF GEOMColors[64];

void CreateDrawObjects(void);

void DeleteGraphicObjects(void);

void InitDrawingObject(int32 ObjectType, int32 Layer, int32 ThickNess, int32 mode);

void InitDrawingClearance(void);

void InitDrawingEmptyPen(void);

void InitDrawingColor(COLORREF Color);

void InitDrawingColorBlack(void);

void InitDrawingColorYellow(void);

void InitDrawingColorYellow2(void);

void InitDrawingColorGray(void);

void InitDrawingColorMagenta(void);

void InitDrawingColorWhite(int32 mode);

void InitDrawingStippelPen(void);

void InitDrawingButtonInfo(void);

void InitDrawingBackGroundBrush(void);

void DrawLineWhite(double x1, double y1, double x2, double y2, int32 mode);

void ExitDrawing(void);

void DrawGridCursor(int32 x, int32 y);

void DrawLineYellow(double x1, double y1, double x2, double y2);

void DrawLineRed(double x1, double y1, double x2, double y2);

void DrawLineGreen(double x1, double y1, double x2, double y2);

void DrawLineWhite3(double x1, double y1, double x2, double y2);

void DrawLineYellow3(double x1, double y1, double x2, double y2);

void DrawLineRed3(double x1, double y1, double x2, double y2);

void DrawLineGreen3(double x1, double y1, double x2, double y2);

void SetBackGroundActive(int32 mode);

void InitDrawingBackGround(int32 mode, int32 ThickNess);

void GraphicsMain(void);

void LoadDefaultColors(void);

#endif
