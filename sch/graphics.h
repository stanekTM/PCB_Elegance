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

#include "types.h"


#define WireObjectCode                          100
#define BusObjectCode                           101
#define JunctionObjectCode                      102
#define NetLabelObjectCode                      103
#define BusConnectionObjectCode                 104
#define BusConnectionObjectCode2                105
#define GlobalConnectionObjectCode              106
#define GlobalConnectionObjectCode2             107
#define SymbolPinObjectCode                     108
#define SymbolPinBusObjectCode                  109
#define SymbolPinTextObjectCode                 110
#define SymbolPowerPinTextObjectCode            111
#define SymbolPinBusTextObjectCode              112

#define SymbolLinesObjectCode                   113
#define SymbolRectsObjectCode                   114
#define SymbolCirclesObjectCode                 115
#define SymbolArcsObjectCode                    116
#define SymbolTextsObjectCode                   117
#define InstanceRefTextObjectCode               118
#define InstanceValueTextObjectCode             119
#define ObjectLinesObjectCode                   120
#define ObjectRectsObjectCode                   121
#define ObjectCirclesObjectCode                 122
#define ObjectRectsObjectCode2                  123
#define ObjectCirclesObjectCode2                124
#define ObjectArcsObjectCode                    125
#define ObjectTextsObjectCode                   126

#define SpecialFontObjectCode                   127
#define StippelObjectCode                       128
#define YellowObjectCode                        129
#define MagentaObjectCode                       130
#define WhiteObjectCode                         131
#define BlackObjectCode                         132
#define GrayObjectCode                          133
#define SpecialDrawObjectCode                   134
#define ButtonInfoObjectCode                    135
#define BackGroundObjectCode1                   136
#define BackGroundObjectCode2                   137
#define White2ObjectCode                        138
#define OnePinNetObjectCode                     139

#define NetLabelTextColorCode                   100
#define BusConnectionTextColorCode              101
#define SymbolPinTextColorCode                  102
#define SymbolTextColorCode                     103
#define InstanceRefTextColorCode                104
#define ObjectTextColorCode                     105
#define WhiteColorCode                          106
#define BlackColorCode                          107

#define NetLabelTextFontCode                    100
#define BusConnectionTextFontCode               101
#define SymbolPinTextFontCode                   102
#define SymbolTextFontCode                      103
#define InstanceRefTextFontCode                 104


#define NrColors                                26

#define WireColorNr                             0
#define BusColorNr                              1
#define BusConnectionColorNr                    2
#define GlobalConnectionColorNr                 3
#define JunctionColorNr                         4
#define NetLabelColorNr                         5
#define InstanceRefTextColorNr                  6
#define InstanceValueTextColorNr                7
#define SymbolPinColorNr                        8
#define SymbolPinBusColorNr                     9
#define SymbolPinTextColorNr                    10
#define SymbolPowerPinTextColorNr               11
#define SymbolPinBusTextColorNr                 12
#define SymbolLineColorNr                       13
#define SymbolRectColorNr                       14
#define SymbolCircleColorNr                     15
#define SymbolArcColorNr                        16
#define SymbolTextColorNr                       17
#define ObjectLineColorNr                       18
#define ObjectRectColorNr                       19
#define ObjectCircleColorNr                     20
#define ObjectArcColorNr                        21
#define ObjectTextColorNr                       22
#define ButtonInfoColorNr                       23
#define GridColorNr                             24
#define BackGroundColorNr                       25
#define OnePinNetColorNr                        26


extern int32 DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY, PrintingBusThickness;
extern double ViewMinX, ViewMinY, ViewMaxX, ViewMaxY, Factor, Xoffset, Yoffset;

extern HDC SCHDisplay;

extern COLORREF SCHColors[64];

void AddGraphicsObject(HGDIOBJ * Object);

void CreateDrawObjects(int32 mode);

void CreateObjectColorsScreen(void);

void CreateObjectColorsPrint(int32 mode);

void DeleteGraphicObjects(void);

void InitDrawingWires(int32 Thickness);

void InitDrawingBusses(int32 ThickNess);

void InitDrawingBusConnections(int32 Thickness);

void InitDrawingGlobalConnections(int32 Thickness);

//void InitDrawingBusConnectionsText(void);

void InitDrawingJunctions(void);

void InitDrawingOnePinNets(int32 Thickness);

void InitDrawingNetLabels(int32 Thickness);

void InitTextColor(COLORREF TextColor);

void InitDrawingColorWhite(void);

void RestoreBrush(void);

void InitDrawingSymbolPins();

void InitDrawingSymbolPinBusses();

void InitDrawingSymbolPinTexts(int32 Thickness);

void InitDrawingSymbolPowerPinTexts(int32 Thickness);

void InitDrawingSymbolPinBusTexts(int32 Thickness);

void InitDrawingSymbolLines(int32 Thickness);

void InitDrawingSymbolRects(int32 Thickness);

void InitDrawingSymbolCircles(int32 Thickness);

void InitDrawingSymbolArcs(int32 Thickness);

void InitDrawingSymbolTexts(int32 Thickness);

void InitDrawingInstanceRefText(int32 Thickness);

void InitDrawingInstanceValueText(int32 Thickness);

void InitDrawingObjectLines(int32 Thickness);

void InitDrawingObjectRects(int32 Thickness, int32 Filled);

void InitDrawingObjectCircles(int32 Thickness, int32 Filled);

void InitDrawingObjectArcs(int32 Thickness);

void InitDrawingObjectTexts(int32 Thickness);

void InitDrawingColor(COLORREF Color);

void InitDrawingColorBlack(void);

void InitDrawingColorYellow(void);

void InitDrawingColorGray(void);

void InitDrawingColorMagenta(void);

void InitDrawingColorWhite(void);

void InitDrawingColorWhite2(void);

void InitDrawingClearance(void);

void InitDrawingStippelPen(void);

void InitDrawingButtonInfo(void);

void ExitDrawing(void);

void DrawGridCursor(int16 x, int16 y);

void DrawLineWhite(double x1, double y1, double x2, double y2, int32 mode);

void DrawLineYellow(double x1, double y1, double x2, double y2);

void DrawCircleWhite(double x, double y, double dikte, int32 mode);

void LoadDefaultColors(void);

void LoadDefaultColors2(void);

void SetBackGroundActive(int32 mode);

void InitDrawingBackGround(int32 mode, int32 ThickNess);

void GraphicsMain(void);

#endif
