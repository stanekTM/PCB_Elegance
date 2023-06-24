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


#define NORMAL_FILLED_AND_PEN1                  0x00
#define NORMAL_FILLED_AND_NO_PEN                0x01
#define HILITED_NORMAL_FILLED_AND_PEN1          0x02
#define HILITED_NORMAL_FILLED_AND_NO_PEN        0x03
#define IN_NET_FILLED_AND_PEN1                  0x04
#define IN_NET_FILLED_AND_NO_PEN                0x05
#define HILITED_IN_NET_FILLED_AND_PEN1          0x06
#define HILITED_IN_NET_FILLED_AND_NO_PEN        0x07
#define UNCONNECTED_PADS_FILLED_AND_PEN1        0x08
#define DRAW_WITH_DASH_PEN_AND_NO_BRUSH         0x09

#define DRAW_WITH_PEN_AND_NOT_FILLED            0x10
#define DRAW_WITH_HILITED_PEN_AND_NOT_FILLED    0x12
#define DRAW_WITH_PEN_AND_NO_BRUSH              0x13
#define DRAW_WITH_PEN_AND_BACKGROUND_BRUSH      0x14
#define DRAW_WITH_WHITE_PEN_AND_NOT_FILLED      0x15

#define USE_LAYER_DRAW_CODE                     0x400

#define MAX_ACTIVE_DRAWING_LAYERS               8

#define DefFontSize                             0.833333333333


#define ViewLayer1ObjectNr                      0
#define ViewLayer2ObjectNr                      1
#define ViewLayer3ObjectNr                      2
#define ViewLayer4ObjectNr                      3
#define ViewLayer5ObjectNr                      4
#define ViewLayer6ObjectNr                      5
#define ViewLayer7ObjectNr                      6
#define ViewLayer8ObjectNr                      7
#define ViewLayer9ObjectNr                      8
#define ViewLayer10ObjectNr                     9
#define ViewLayer11ObjectNr                     10
#define ViewLayer12ObjectNr                     11
#define ViewLayer13ObjectNr                     12
#define ViewLayer14ObjectNr                     13
#define ViewLayer15ObjectNr                     14
#define ViewLayer16ObjectNr                     15
#define ViewLayer1HilitedObjectNr               16
#define ViewLayer2HilitedObjectNr               17
#define ViewLayer3HilitedObjectNr               18
#define ViewLayer4HilitedObjectNr               19
#define ViewLayer5HilitedObjectNr               20
#define ViewLayer6HilitedObjectNr               21
#define ViewLayer7HilitedObjectNr               22
#define ViewLayer8HilitedObjectNr               23
#define ViewLayer9HilitedObjectNr               24
#define ViewLayer10HilitedObjectNr              25
#define ViewLayer11HilitedObjectNr              26
#define ViewLayer12HilitedObjectNr              27
#define ViewLayer13HilitedObjectNr              28
#define ViewLayer14HilitedObjectNr              29
#define ViewLayer15HilitedObjectNr              30
#define ViewLayer16HilitedObjectNr              31
#define ViewLayer1InNetObjectNr                 32
#define ViewLayer2InNetObjectNr                 33
#define ViewLayer3InNetObjectNr                 34
#define ViewLayer4InNetObjectNr                 35
#define ViewLayer5InNetObjectNr                 36
#define ViewLayer6InNetObjectNr                 37
#define ViewLayer7InNetObjectNr                 38
#define ViewLayer8InNetObjectNr                 39
#define ViewLayer9InNetObjectNr                 40
#define ViewLayer10InNetObjectNr                41
#define ViewLayer11InNetObjectNr                42
#define ViewLayer12InNetObjectNr                43
#define ViewLayer13InNetObjectNr                44
#define ViewLayer14InNetObjectNr                45
#define ViewLayer15InNetObjectNr                46
#define ViewLayer16InNetObjectNr                47
#define ViewLayer1HilitedInNetObjectNr          48
#define ViewLayer2HilitedInNetObjectNr          49
#define ViewLayer3HilitedInNetObjectNr          50
#define ViewLayer4HilitedInNetObjectNr          51
#define ViewLayer5HilitedInNetObjectNr          52
#define ViewLayer6HilitedInNetObjectNr          53
#define ViewLayer7HilitedInNetObjectNr          54
#define ViewLayer8HilitedInNetObjectNr          55
#define ViewLayer9HilitedInNetObjectNr          56
#define ViewLayer10HilitedInNetObjectNr         57
#define ViewLayer11HilitedInNetObjectNr         58
#define ViewLayer12HilitedInNetObjectNr         59
#define ViewLayer13HilitedInNetObjectNr         60
#define ViewLayer14HilitedInNetObjectNr         61
#define ViewLayer15HilitedInNetObjectNr         62
#define ViewLayer16HilitedInNetObjectNr         63
#define ConnectionsObjectNr                     64
#define ConnectionsHilitedObjectNr              65
#define NetPinsObjectNr                         66
#define NetPinsObject2Nr                        67
#define SilkScreenTopObjectNr                   68
#define SilkScreenBottomObjectNr                69
#define ReferenceObjectNr                       70
#define CompValueObjectNr                       71
#define ShapePlacementOutLineTopObjectNr        72
#define ShapePlacementOutLineBottomObjectNr     73
#define ShapeCompOutLineTopObjectNr             74
#define ShapeCompOutLineBottomObjectNr          75
#define ShapePinsTopObjectNr                    76
#define ShapePinsBottomObjectNr                 77
#define ShapePinsInnerObjectNr                  78
#define ShapePinsTopHilitedObjectNr             79
#define ShapePinsBottomHilitedObjectNr          80
#define ShapePinsInnerHilitedObjectNr           81
#define ShapePinsDrillObjectNr                  82
#define ShapePinsDrillUnplatedObjectNr          83
#define ViaPinsObjectNr                         84
#define ViaPinsHilitedObjectNr                  85
#define ViaPinsInNetObjectNr                    86
#define ViaPinsHilitedInNetObjectNr             87
#define ViaPinsDrillObjectNr                    88
#define ObjectsInfoObjectNr                     89
#define ClearanceObjectNr                       90
#define StippelObjectNr                         91
#define PolylineObjectNr                        92
#define ErrorObjectNr                           93
#define WarningObjectNr                         94
#define GridObjectNr                            95
#define ButtonInfoObjectNr                      96
#define BackGroundObjectNr                      97
#define BoardOutlineObjectNr                    98
#define SwappablePinsGateObjectNr               99
#define SwappableGatePinsObjectNr               100
#define PasteMaskTopObjectNr                    101
#define PasteMaskBottomObjectNr                 102
#define SoldMaskTopObjectNr                     103
#define SoldMaskBottomObjectNr                  104
#define ObjectsInfo2ObjectNr                    105
#define ObjectsInfo3ObjectNr                    106
#define ObjectsInfo4ObjectNr                    107
#define UnconnectedPadsTopObjectNr              108
#define UnconnectedPadsBottomObjectNr           109
#define UnconnectedPadsInnerObjectNr            110
#define RoutingKeepoutTopObjectNr               111
#define RoutingKeepoutBottomObjectNr            112
#define RoutingKeepoutInnerObjectNr             113
#define BoardOutlineKeepOutObjectNr             114


#define YellowObjectNr                          115
#define MagentaObjectNr                         116
#define WhiteObjectNr                           117
#define BlackObjectNr                           118
#define GrayObjectNr                            119
#define RedObjectNr                             120
#define GreenObjectNr                           121
#define BlueObjectNr                            122
#define WhitePenObjectNr                        123
#define EmptyPenObjectNr                        124
#define EmptyBrushObjectNr                      125
#define BackgroundDashPenObjectNr               126
#define CrossHairObjectNr                       127

#define NrLayerObjects                          128

#define GRAPHICS_YELLOW                         0x100
#define GRAPHICS_BLUE                           0x200
#define GRAPHICS_MAGENTA                        0x300
#define GRAPHICS_WHITE                          0x400
#define GRAPHICS_BLACK                          0x500
#define GRAPHICS_GRAY                           0x600
#define GRAPHICS_RED                            0x700
#define GRAPHICS_GREEN                          0x800



// ****************************************************************************************************
// ****************************************************************************************************

#define RGB_Black                               RGB(  0,  0,  0)
#define RGB_DarkGray                            RGB( 64, 64, 64)
#define RGB_MidGray                             RGB( 96, 96, 96)
#define RGB_Gray                                RGB(128,128,128)
#define RGB_LightGray                           RGB(192,192,192)
#define RGB_White                               RGB(255,255,255)

#define RGB_Red                                 RGB(255,  0,  0)
#define RGB_Blue                                RGB(  0,  0,255)
#define RGB_Green                               RGB(  0,255,  0)

#define RGB_Yellow                              RGB(255,255,  0)
#define RGB_Cyan                                RGB(  0,255,255)
#define RGB_Magenta                             RGB(255,  0,255)
#define RGB_Magenta3                            RGB(255,128,255)

#define RGB_Red2                                RGB(128,  0,  0)
#define RGB_Blue2                               RGB(  0,  0,128)
#define RGB_Green2                              RGB(  0,128,  0)

#define RGB_Yellow2                             RGB(128,128,  0)
#define RGB_Cyan2                               RGB(  0,128,128)
#define RGB_Magenta2                            RGB(128,  0,128)

#define RGB_DarkMagenta                         RGB(170,  0,170)
#define RGB_DarkCyan                            RGB(  0,170,170)
#define RGB_DarkBlue                            RGB(  0,  0,170)
#define RGB_DarkRed                             RGB(170,  0,  0)
#define RGB_DarkGreen                           RGB(  0,170,  0)
#define RGB_DarkMagenta                         RGB(170,  0,170)

#define RGB_LightRed                            RGB(255,120,120)
#define RGB_LightBlue                           RGB(140,140,255)
#define RGB_LightGreen                          RGB(120,255,120)
#define RGB_LightMagenta                        RGB(255,120,255)
#define RGB_LightBrown                          RGB(192,128,64 )
#define RGB_DarkBrown                           RGB(64 ,48 ,26 )
#define RGB_Orange                              RGB(255,165,  0)
#define RGB_Pink                                RGB(255,192,203)
#define RGB_DarkPink                            RGB(207, 37,190)
#define RGB_LightPink                           RGB(255,210,225)
#define RGB_Brown                               RGB(128,64 ,0  )
#define RGB_LightOrange                         RGB(223,188,96 )
#define RGB_Violet                              RGB(238,130,238)

// New colours
#define RGB_AzureBlue                           RGB( 45, 91,255)
#define RGB_StrongPink                          RGB(255,128,255)
#define RGB_TealBlue                            RGB( 47,154,159)
#define RGB_BrownOrange                         RGB(168,116, 45)
#define RGB_IndigoBlue                          RGB( 87, 24,254)
#define RGB_HeliotropePink                      RGB(191, 75,254)
#define RGB_LightAzureBlue                      RGB(128,214,255)
#define RGB_LightStrongPink                     RGB(255,191,255)
#define RGB_VenetianRed                         RGB(180, 30, 30)
#define RGB_PumpkinOrange                       RGB(255,128, 64)
#define RGB_ScarletRed                          RGB(222, 47,  3)
#define RGB_IndianRed                           RGB(182, 78, 80)
#define RGB_LightAzureBlue2                     RGB(168,216,255)
#define RGB_MauveViolet                         RGB(223,188,252)
#define RGB_PeachOrange                         RGB(255,207,106)
#define RGB_AmberOrange                         RGB(251,202, 55)
#define RGB_DarkAmberOrange                     RGB(232,175,  4)
#define RGB_DarkYellow                          RGB(210,210,  0)
#define RGB_MidDarkGray                         RGB( 82, 82, 82)
#define RGB_LightSepiaBrown                     RGB(121, 90, 49)
#define RGB_SepiaBrown                          RGB( 92, 69, 37)
#define RGB_Yellow3                             RGB(160,160,  0)

// ****************************************************************************************************
// ****************************************************************************************************


extern int32 DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY;

extern int32 CurrentObjectCode;

extern COLORREF GraphicsObjectColor[192];
extern COLORREF GraphicsObjectColor2[192];
extern COLORREF PCBColors[192];
extern COLORREF PCBColors2[192];
extern HBRUSH GraphicsObjectBrush[192];
extern HPEN GraphicsObjectPen[192];
extern int32 GraphicsObjectCodes[192];
extern int32 PCBObjectCodes[192];
extern int32 GraphicsObjectPenThickness[192];
extern LayerObjectCodesRecord LayerObjectCodes[NrLayerObjects];


extern double ViewMinX, ViewMinY, ViewMaxX, ViewMaxY;

extern HPALETTE PCBPalette, CurrentPalette;
extern HDC OutputDisplay;

void CreateDrawObjects(int32 mode);

void FillBrushOnColorObjectNr(LOGBRUSH* Brush, int32 ObjectColorNr);

void GetGraphicsPenBrush(int32 mode);

int32 InitDrawingObject(int32 ObjectType, int32 Layer, int32 ThickNess, int32 mode);

void InitDrawingEmptyPen(void);

void ExitDrawing(void);

void DrawLineWhite(double x1, double y1, double x2, double y2, int32 BufferMode);

void DrawLineYellow(double x1, double y1, double x2, double y2, int32 BufferMode);

void DrawLineRed(double x1, double y1, double x2, double y2, int32 BufferMode);

void DrawLineGreen(double x1, double y1, double x2, double y2, int32 BufferMode);

void DrawLineGray(double x1, double y1, double x2, double y2, int32 BufferMode);

void DrawCircleWhite(double x, double y, double dikte, int32 BufferMode);

void DrawCircleYellow(double x, double y, double dikte, int32 BufferMode);

void DrawCircleGray(double x, double y, double dikte, int32 BufferMode);

void DeleteGraphicObjects(void);

void SetBackGroundActive(int32 mode);

void GraphicsMain(void);

void LoadDefaultColors(void);

#endif