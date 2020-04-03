/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: graphics.c
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


#include  "types.h"
#include  "graphics.h"
#include  "calcdef.h"
#include  "line2.h"

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

#define RGB_DarkMagenta                         RGB(170,  0,170)
#define RGB_DarkCyan                            RGB(  0,170,170)
#define RGB_DarkBlue                            RGB(  0,  0,170)
#define RGB_DarkRed                             RGB(170,  0,  0)
#define RGB_DarkGreen                           RGB(  0,170,  0)
#define RGB_DarkMagenta                         RGB(170,  0,170)

#define RGB_LightRed                            RGB(255,102,102)
#define RGB_LightBlue                           RGB(120,120,255)
#define RGB_LightGreen                          RGB(102,255,102)
#define RGB_LightMagenta                        RGB(255,102,255)
#define RGB_LightBrown                          RGB(192,128,64 )
#define RGB_LightBrown2                         RGB(213,172,132)
#define RGB_Orange                              RGB(255,128,0  )
#define RGB_Pink                                RGB(236,151,227)
#define RGB_DarkPink                            RGB(207, 37,190)
#define RGB_LightPink                           RGB(255,200,240)
#define RGB_Brown                               RGB(128,64 ,0  )
#define RGB_LightOrange                         RGB(223,188,96 )
#define RGB_Violet                              RGB(128,0  ,128)

#define ColorMacro(Color)                       (Color)


COLORREF BlackColor, DarkGrayColor, GrayColor, LightGrayColor, WhiteColor, RedColor, BlueColor, GreenColor, YellowColor,
         CyanColor, MagentaColor, DarkMagentaColor, DarkCyanColor, DarkBlueColor, DarkRedColor, DarkGreenColor,
         DarkMagentaColor, LightRedColor, LightBlueColor, LightGreenColor, LightMagentaColor, OrangeColor, PinkColor,
         DarkPinkColor, BrownColor, VioletColor, TextColor, LineColor, ShapeSilkScreenTopColor, ShapeSilkScreenBottomColor,
         ShapeCompOutlineColor, ShapePlacementOutLineColor, ShapePinsDrillColor, ShapePinsDrillUnplatedColor,
         ShapePinsTopColor, ShapePinsBottomColor, ShapePinsInnerColor, ShapePasteMaskTopColor, ShapePasteMaskBottomColor,
         ShapeSoldMaskTopColor, ShapeSoldMaskBottomColor, ShapeRoutingKeepoutTopColor, ShapeRoutingKeepoutBottomColor,
         ShapeRoutingKeepoutInnerColor, ShapeBoardOutlineColor, ShapeInfo1Color, ShapeInfo2Color, ShapeInfo3Color,
         ShapeInfo4Color, ShapeGeomNameColor, ShapePowerPadColor, ClearanceColor, ButtonInfoColor, GridColor,
         BackGroundColor, PinTextColor, StippelColor;

COLORREF GEOMColors[64];

LOGBRUSH BrushObject, BrushObjectMagenta, BrushObjectYellow, ShapePinsTopBrushObject, ShapePinsBottomBrushObject,
         ShapePinsInnerBrushObject, ShapePasteMaskTopBrushObject, ShapePasteMaskBottomBrushObject,
         ShapeSoldMaskTopBrushObject, ShapeSoldMaskBottomBrushObject, ShapeRoutingKeepoutTopBrushObject,
         ShapeRoutingKeepoutBottomBrushObject, ShapeRoutingKeepoutInnerBrushObject, ShapePowerPadBrushObject,
         ShapeInfo1BrushObject, ShapeInfo2BrushObject, ShapeInfo3BrushObject, ShapeInfo4BrushObject,
         ShapeSilkScreenTopBrushObject, ShapeSilkScreenBottomBrushObject, ShapeCompOutlineBrushObject, EmptyBrushObject,
         ButtonInfoBrushObject, BackGroundBrushObject, WhiteBrushObject;

uint32 BrushMode;

HGDIOBJ ShapePinsTopBrush, ShapePinsBottomBrush, ShapePinsInnerBrush, ShapePasteMaskTopBrush, ShapePasteMaskBottomBrush,
        ShapeSoldMaskTopBrush, ShapeSoldMaskBottomBrush, ShapeInfo1Brush, ShapeInfo2Brush, ShapeInfo3Brush, ShapeInfo4Brush,
        ShapeSilkScreenTopBrush, ShapeSilkScreenBottomBrush, ShapeCompOutlineBrush, ShapeRoutingKeepoutTopBrush,
        ShapeRoutingKeepoutBottomBrush, ShapeRoutingKeepoutInnerBrush, ShapePowerPadBrush, ButtonInfoBrush, LayerBrush,
        BackGroundBrush, EmptyBrush, WhiteBrush, Brush, SaveBrush, SaveFont, PinTextFont, BrushMagenta, BrushYellow;

HPEN ShapeSilkScreenTopPen, ShapeSilkScreenBottomPen, ShapeCompOutlinePen, ShapePlacementOutLinePen, DrawPen, SavePen,
     EmptyPen, BlackPen, WhitePen, WhitePen2, GrayPen, YellowPen, YellowPen2, ShapePinsTopPen, ShapePinsBottomPen,
     ShapePinsInnerPen, ShapePinsDrillPen, ShapePinsDrillUnplatedPen, ShapePasteMaskTopPen, ShapePasteMaskBottomPen,
     ShapeSoldMaskTopPen, ShapeSoldMaskBottomPen, ShapeRoutingKeepoutTopPen, ShapeRoutingKeepoutBottomPen,
     ShapeRoutingKeepoutInnerPen, ShapeBoardOutlinePen, ShapeInfo1Pen, ShapeInfo2Pen, ShapeInfo3Pen, ShapeInfo4Pen,
     ShapeGeomNamePen, StippelPen, ShapePowerPadPen, AreaFillPen, ButtonInfoPen, BackGroundPen, BackGroundPen2,
     ClearancePen;

HPALETTE GEOMPalette, CurrentPalette;
GEOMPaletteRecord GEOMPaletteObject;
HDC OutputDisplay;

HGDIOBJ *GraphicsObjects[200];


int32 NrGraphicsObjects, SilkScreenTopLineThickNess, SilkScreenBottomLineThickNess, BackGroundPenThickNess2,
      Info1ThickNess, Info2ThickNess, Info3ThickNess, Info4ThickNess, ShapePasteMaskTopThickNess,
      ShapePasteMaskBottomThickNess, ShapeSoldMaskTopThickNess, ShapeSoldMaskBottomThickNess, ShapePinsBottomThickNess,
      ShapePinsTopThickNess, ShapePinsInnerThickNess, BoardOutlineThickNess, SelectColorMode, GeomNameLineThickNess, ok,
      CompOutLineThickNess;

// ****************************************************************************************************
// ****************************************************************************************************

extern double Factor, Xoffset, Yoffset;
extern int32 Printing, BackGroundActive, NrPadLayers, PixelsPerInchX;
extern HDC OutputDisplay;

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void StartDrawingEditingWindow(void);

void EndDrawingEditingWindow(void);

void DeleteGraphicObjects(void);

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void AddGraphicsObject(HGDIOBJ * Object)
{
#ifdef _DEBUG

	if (*Object == NULL)
		ok = 1;

#endif

	if (NrGraphicsObjects >= 200)
		return;

	GraphicsObjects[NrGraphicsObjects] = Object;
	NrGraphicsObjects++;
#ifdef _DEBUG

	if (NrGraphicsObjects > 27)
		ok = 1;

#endif
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DeleteGraphicObjects()
{
	int32 cnt;
	HGDIOBJ *Point;

//  First=*(GraphicsObjects[0]);
	for (cnt = 0; cnt < NrGraphicsObjects; cnt++)
	{
		Point = GraphicsObjects[cnt];

		if (*Point == NULL)
			ok = 1;

		if (!DeleteObject(*(GraphicsObjects[cnt])))
		{
//      Obj=GetObjectType(*(GraphicsObjects[cnt]));
			ok = 1;
		}

		*(GraphicsObjects[cnt]) = (HGDIOBJ) NULL;
	}

	NrGraphicsObjects = 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void ChangeGraphicObject(HGDIOBJ OldObject, HGDIOBJ NewObject)
{
	int32 cnt, res, Found = 0, ObjectType;
	HGDIOBJ CurrentPenSelected;

	if (OutputDisplay != NULL)
	{
		if ((ObjectType = GetObjectType(OldObject)) == OBJ_PEN)
		{
			CurrentPenSelected = GetCurrentObject(OutputDisplay, OBJ_PEN);

			if (OldObject == CurrentPenSelected)
			{
				SelectObject(OutputDisplay, EmptyPen);
				res = 1;
			}
		}
	}

#ifdef _DEBUG

	if (NrGraphicsObjects >= 200)
		ok = 1;

#endif
	cnt = 0;

	while ((cnt < NrGraphicsObjects) && (!Found))
	{
		if (OldObject == *(GraphicsObjects[cnt]))
			Found = 1;
		else
			cnt++;
	}

	if (Found)
	{
		DeleteObject(*(GraphicsObjects[cnt]));
		*(GraphicsObjects[cnt]) = NewObject;
	}
	else
		ok = 1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CreateDrawObjects()
{
	BackGroundColor = ColorMacro(GEOMColors[BackGroundColorNr]);
	ShapeSilkScreenTopColor = ColorMacro(GEOMColors[ShapeSilkScreenTopColorNr]);
	ShapeSilkScreenBottomColor = ColorMacro(GEOMColors[ShapeSilkScreenBottomColorNr]);
	ShapeCompOutlineColor = ColorMacro(GEOMColors[ShapeCompOutlineColorNr]);
	ShapePlacementOutLineColor = ColorMacro(GEOMColors[ShapePlacementOutLineColorNr]);
	ShapePinsDrillColor = ColorMacro(GEOMColors[ShapePinsDrillColorNr]);
	ShapePinsDrillUnplatedColor = ColorMacro(GEOMColors[ShapePinsDrillUnplatedColorNr]);
	ShapePinsInnerColor = ColorMacro(GEOMColors[ShapePinsInnerColorNr]);
	ShapePasteMaskTopColor = ColorMacro(GEOMColors[ShapePasteMaskTopColorNr]);
	ShapePasteMaskBottomColor = ColorMacro(GEOMColors[ShapePasteMaskBottomColorNr]);
	ShapeSoldMaskTopColor = ColorMacro(GEOMColors[ShapeSoldMaskTopColorNr]);
	ShapeSoldMaskBottomColor = ColorMacro(GEOMColors[ShapeSoldMaskBottomColorNr]);
	ShapeRoutingKeepoutTopColor = ColorMacro(GEOMColors[ShapeRoutingKeepoutTopColorNr]);
	ShapeRoutingKeepoutBottomColor = ColorMacro(GEOMColors[ShapeRoutingKeepoutBottomColorNr]);
	ShapeRoutingKeepoutInnerColor = ColorMacro(GEOMColors[ShapeRoutingKeepoutInnerColorNr]);
	ShapeBoardOutlineColor = ColorMacro(GEOMColors[ShapeBoardOutlineColorNr]);
	ShapeInfo1Color = ColorMacro(GEOMColors[ShapeInfo1ColorNr]);
	ShapeInfo2Color = ColorMacro(GEOMColors[ShapeInfo2ColorNr]);
	ShapeInfo3Color = ColorMacro(GEOMColors[ShapeInfo3ColorNr]);
	ShapeInfo4Color = ColorMacro(GEOMColors[ShapeInfo4ColorNr]);

	if (!Printing)
	{
		ShapeGeomNameColor = ColorMacro(GEOMColors[ShapeGeomNameColorNr]);
		ShapePinsTopColor = ColorMacro(GEOMColors[ShapePinsTopColorNr]);
		ShapePinsBottomColor = ColorMacro(GEOMColors[ShapePinsBottomColorNr]);
	}
	else
	{
		ShapeGeomNameColor = RGB_Black;
		ShapePinsTopColor = RGB_Black;
		ShapePinsBottomColor = RGB_Black;
	}

	ShapePowerPadColor = ColorMacro(GEOMColors[ShapePowerPadColorNr]);
	ClearanceColor = ColorMacro(GEOMColors[ClearanceColorNr]);
	ButtonInfoColor = ColorMacro(GEOMColors[ButtonInfoColorNr]);
	GridColor = ColorMacro(GEOMColors[GridColorNr]);
	PinTextColor = ColorMacro(GEOMColors[PinTextColorNr]);

	SelectColorMode = R2_WHITE;

	if (GEOMColors[BackGroundColorNr] == 0x00ffffff)
		SelectColorMode = R2_NOTMERGEPEN;

// ****************************************************************************************************
// ****************************************************************************************************

	EmptyBrushObject.lbHatch = (LONG) NULL;
	EmptyBrushObject.lbStyle = BS_NULL;
	EmptyBrushObject.lbColor = (LONG) NULL;

	BackGroundBrushObject.lbHatch = (LONG) NULL;
	BackGroundBrushObject.lbStyle = BS_SOLID;
	BackGroundBrushObject.lbColor = BackGroundColor;

	ButtonInfoBrushObject.lbHatch = (LONG) NULL;
	ButtonInfoBrushObject.lbStyle = BS_SOLID;
	ButtonInfoBrushObject.lbColor = ColorMacro(ButtonInfoColor);


	ButtonInfoBrush = CreateBrushIndirect(&ButtonInfoBrushObject);
	BackGroundBrush = CreateBrushIndirect(&BackGroundBrushObject);
	EmptyBrush = CreateBrushIndirect(&EmptyBrushObject);
	EmptyPen = CreatePen(PS_NULL, 0, 0);
	BlackPen = CreatePen(PS_SOLID, 1, ColorMacro(RGB_Black));
	WhitePen = CreatePen(PS_SOLID, 1, ColorMacro(RGB_White));
	WhitePen2 = CreatePen(PS_DOT, 1, ColorMacro(RGB_White));
	GrayPen = CreatePen(PS_SOLID, 1, ColorMacro(RGB_Gray));
	YellowPen = CreatePen(PS_SOLID, 1, ColorMacro(RGB_Yellow));
	YellowPen2 = CreatePen(PS_DASH, 1, ColorMacro(RGB_Yellow));
	ButtonInfoPen = CreatePen(PS_SOLID, 1, ColorMacro(RGB_White));
	BackGroundPen = CreatePen(PS_SOLID, 1, ColorMacro(BackGroundColor));

	AddGraphicsObject((HGDIOBJ *) & ButtonInfoBrush);
	AddGraphicsObject((HGDIOBJ *) & BackGroundBrush);
	AddGraphicsObject((HGDIOBJ *) & EmptyBrush);
	AddGraphicsObject((HGDIOBJ *) & EmptyPen);
	AddGraphicsObject((HGDIOBJ *) & BlackPen);
	AddGraphicsObject((HGDIOBJ *) & WhitePen);
	AddGraphicsObject((HGDIOBJ *) & WhitePen2);
	AddGraphicsObject((HGDIOBJ *) & GrayPen);
	AddGraphicsObject((HGDIOBJ *) & YellowPen);
	AddGraphicsObject((HGDIOBJ *) & ButtonInfoPen);
	AddGraphicsObject((HGDIOBJ *) & BackGroundPen);

// ****************************************************************************************************
// ****************************************************************************************************


	ShapePinsTopBrushObject.lbHatch = (LONG) NULL;
//  ShapePinsTopBrushObject.lbHatch        = HS_HORIZONTAL;
	ShapePinsBottomBrushObject.lbHatch = (LONG) NULL;
	ShapePinsInnerBrushObject.lbHatch = HS_HORIZONTAL;
	ShapePasteMaskTopBrushObject.lbHatch = HS_FDIAGONAL;
	ShapePasteMaskBottomBrushObject.lbHatch = HS_BDIAGONAL;
	ShapeSoldMaskTopBrushObject.lbHatch = HS_VERTICAL;
	ShapeSoldMaskBottomBrushObject.lbHatch = HS_DIAGCROSS;
	ShapePowerPadBrushObject.lbHatch = HS_BDIAGONAL;
	ShapeRoutingKeepoutTopBrushObject.lbHatch = HS_BDIAGONAL;
	ShapeRoutingKeepoutBottomBrushObject.lbHatch = HS_VERTICAL;

	ShapePinsTopBrushObject.lbStyle = BS_SOLID;
//  ShapePinsTopBrushObject.lbStyle        = BS_HATCHED;
	ShapePinsBottomBrushObject.lbStyle = BS_SOLID;
	ShapePinsInnerBrushObject.lbStyle = BS_HATCHED;
	ShapePasteMaskTopBrushObject.lbStyle = BS_HATCHED;
	ShapePasteMaskBottomBrushObject.lbStyle = BS_HATCHED;
	ShapeSoldMaskTopBrushObject.lbStyle = BS_HATCHED;
	ShapeSoldMaskBottomBrushObject.lbStyle = BS_HATCHED;
	ShapeRoutingKeepoutTopBrushObject.lbStyle = BS_HATCHED;
	ShapeRoutingKeepoutBottomBrushObject.lbStyle = BS_HATCHED;
	ShapePowerPadBrushObject.lbStyle = BS_HATCHED;

	ShapePinsTopBrushObject.lbColor = ShapePinsTopColor;
	ShapePinsBottomBrushObject.lbColor = ShapePinsBottomColor;
	ShapePinsInnerBrushObject.lbColor = ShapePinsInnerColor;
	ShapePasteMaskTopBrushObject.lbColor = ShapePasteMaskTopColor;
	ShapePasteMaskBottomBrushObject.lbColor = ShapePasteMaskBottomColor;
	ShapeSoldMaskTopBrushObject.lbColor = ShapeSoldMaskTopColor;
	ShapeSoldMaskBottomBrushObject.lbColor = ShapeSoldMaskBottomColor;
	ShapeRoutingKeepoutTopBrushObject.lbColor = ShapeRoutingKeepoutTopColor;
	ShapeRoutingKeepoutBottomBrushObject.lbColor = ShapeRoutingKeepoutBottomColor;
	ShapeRoutingKeepoutInnerBrushObject.lbColor = ShapeRoutingKeepoutInnerColor;
	ShapePowerPadBrushObject.lbColor = ShapePowerPadColor;

	ShapePinsTopBrush = CreateBrushIndirect(&ShapePinsTopBrushObject);
	ShapePinsBottomBrush = CreateBrushIndirect(&ShapePinsBottomBrushObject);
	ShapePinsInnerBrush = CreateBrushIndirect(&ShapePinsInnerBrushObject);
	ShapePasteMaskTopBrush = CreateBrushIndirect(&ShapePasteMaskTopBrushObject);
	ShapePasteMaskBottomBrush = CreateBrushIndirect(&ShapePasteMaskBottomBrushObject);
	ShapeSoldMaskTopBrush = CreateBrushIndirect(&ShapeSoldMaskTopBrushObject);
	ShapeSoldMaskBottomBrush = CreateBrushIndirect(&ShapeSoldMaskBottomBrushObject);
	ShapeRoutingKeepoutTopBrush = CreateBrushIndirect(&ShapeRoutingKeepoutTopBrushObject);
	ShapeRoutingKeepoutBottomBrush = CreateBrushIndirect(&ShapeRoutingKeepoutBottomBrushObject);
	ShapeRoutingKeepoutInnerBrush = CreateBrushIndirect(&ShapeRoutingKeepoutInnerBrushObject);
	ShapePowerPadBrush = CreateBrushIndirect(&ShapePowerPadBrushObject);

	ShapeInfo1BrushObject.lbHatch = (LONG) NULL;
	ShapeInfo2BrushObject.lbHatch = (LONG) NULL;
	ShapeInfo3BrushObject.lbHatch = (LONG) NULL;
	ShapeInfo4BrushObject.lbHatch = (LONG) NULL;
	ShapeSilkScreenTopBrushObject.lbHatch = (LONG) NULL;
	ShapeSilkScreenBottomBrushObject.lbHatch = (LONG) NULL;
	ShapeCompOutlineBrushObject.lbHatch = (LONG) NULL;
	ShapeInfo1BrushObject.lbStyle = BS_SOLID;
	ShapeInfo2BrushObject.lbStyle = BS_SOLID;
	ShapeInfo3BrushObject.lbStyle = BS_SOLID;
	ShapeInfo4BrushObject.lbStyle = BS_SOLID;
	ShapeSilkScreenTopBrushObject.lbStyle = BS_SOLID;
	ShapeSilkScreenBottomBrushObject.lbStyle = BS_SOLID;
	ShapeCompOutlineBrushObject.lbStyle = BS_SOLID;
	ShapeInfo1BrushObject.lbColor = ShapeInfo1Color;
	ShapeInfo2BrushObject.lbColor = ShapeInfo2Color;
	ShapeInfo3BrushObject.lbColor = ShapeInfo3Color;
	ShapeInfo4BrushObject.lbColor = ShapeInfo4Color;
	ShapeSilkScreenTopBrushObject.lbColor = ShapeSilkScreenTopColor;
	ShapeSilkScreenBottomBrushObject.lbColor = ShapeSilkScreenBottomColor;
	ShapeCompOutlineBrushObject.lbColor = ShapeCompOutlineColor;

	ShapeInfo1Brush = CreateBrushIndirect(&ShapeInfo1BrushObject);
	ShapeInfo2Brush = CreateBrushIndirect(&ShapeInfo2BrushObject);
	ShapeInfo3Brush = CreateBrushIndirect(&ShapeInfo3BrushObject);
	ShapeInfo4Brush = CreateBrushIndirect(&ShapeInfo4BrushObject);
	ShapeSilkScreenTopBrush = CreateBrushIndirect(&ShapeSilkScreenTopBrushObject);
	ShapeSilkScreenBottomBrush = CreateBrushIndirect(&ShapeSilkScreenBottomBrushObject);
	ShapeCompOutlineBrush = CreateBrushIndirect(&ShapeCompOutlineBrushObject);

	ShapePinsDrillPen = CreatePen(PS_SOLID, 1, ShapePinsDrillColor);
	ShapePinsDrillUnplatedPen = CreatePen(PS_SOLID, 1, ShapePinsDrillUnplatedColor);
	ShapeRoutingKeepoutTopPen = CreatePen(PS_SOLID, 1, ShapeRoutingKeepoutTopColor);
	ShapeRoutingKeepoutBottomPen = CreatePen(PS_SOLID, 1, ShapeRoutingKeepoutBottomColor);
	ShapeRoutingKeepoutInnerPen = CreatePen(PS_SOLID, 1, ShapeRoutingKeepoutInnerColor);
	ShapeBoardOutlinePen = CreatePen(PS_SOLID, 1, ShapeBoardOutlineColor);
	ShapePowerPadPen = CreatePen(PS_SOLID, 1, ShapePowerPadColor);

	ShapePlacementOutLinePen = CreatePen(PS_SOLID, 1, ShapePlacementOutLineColor);
	ClearancePen = CreatePen(PS_SOLID, 1, ClearanceColor);

	if (!Printing)
	{
		PinTextFont =
		    CreateFont(20, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
		               FIXED_PITCH, "Courier New");
	}
	else
	{
		if (PixelsPerInchX < 400)
		{
			PinTextFont =
			    CreateFont(30, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
			               FIXED_PITCH, "Courier New");
		}
		else
		{
			if (PixelsPerInchX < 800)
			{
				PinTextFont =
				    CreateFont(50, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
				               FIXED_PITCH, "Courier New");
			}
			else
			{
				if (PixelsPerInchX < 1600)
				{
					PinTextFont =
					    CreateFont(80, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
					               PROOF_QUALITY, FIXED_PITCH, "Courier New");
				}
			}
		}
	}

//  PinTextFont                  = GetStockObject(DEFAULT_GUI_FONT);

	AddGraphicsObject((HGDIOBJ *) & ShapePinsTopBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapePinsBottomBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapePinsInnerBrush);

	AddGraphicsObject((HGDIOBJ *) & ShapePasteMaskTopBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapePasteMaskBottomBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapeSoldMaskTopBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapeSoldMaskBottomBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapeRoutingKeepoutTopBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapeRoutingKeepoutBottomBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapeRoutingKeepoutInnerBrush);
	AddGraphicsObject((HGDIOBJ *) & ShapePowerPadBrush);

	AddGraphicsObject((HGDIOBJ *) & ShapePinsDrillPen);
	AddGraphicsObject((HGDIOBJ *) & ShapePinsDrillUnplatedPen);
	AddGraphicsObject((HGDIOBJ *) & ShapeRoutingKeepoutTopPen);
	AddGraphicsObject((HGDIOBJ *) & ShapeRoutingKeepoutBottomPen);
	AddGraphicsObject((HGDIOBJ *) & ShapeRoutingKeepoutInnerPen);
	AddGraphicsObject((HGDIOBJ *) & ShapePowerPadPen);

	AddGraphicsObject((HGDIOBJ *) & ShapePlacementOutLinePen);
	AddGraphicsObject((HGDIOBJ *) & ShapeBoardOutlinePen);

	AddGraphicsObject((HGDIOBJ *) & ClearancePen);
	AddGraphicsObject((HGDIOBJ *) & PinTextFont);

}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingEmptyPen()
{
	SavePen = SelectObject(OutputDisplay, EmptyPen);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorYellow()
{
	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, YellowPen);
	else
		SelectObject(OutputDisplay, YellowPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = YellowObjectCode;
	LineColor = ColorMacro(RGB_Yellow);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorYellow2()
{
	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, YellowPen2);
	else
		SelectObject(OutputDisplay, YellowPen2);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = YellowObjectCode2;
	LineColor = ColorMacro(RGB_Yellow);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorWhite(int32 mode)
{
	if (mode == 0)
	{
		if (SavePen == (HGDIOBJ) - 1)
			SavePen = SelectObject(OutputDisplay, WhitePen);
		else
			SelectObject(OutputDisplay, WhitePen);

		CurrentObjectCode = WhiteObjectCode;
	}
	else
	{
		if (SavePen == (HGDIOBJ) - 1)
			SavePen = SelectObject(OutputDisplay, WhitePen2);
		else
			SelectObject(OutputDisplay, WhitePen2);

		CurrentObjectCode = WhiteObjectCode2;
	}

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	LineColor = ColorMacro(RGB_White);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorGray()
{
	COLORREF Color;

	Color = ColorMacro(RGB_Gray);

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, GrayPen);
	else
		SelectObject(OutputDisplay, GrayPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = GrayObjectCode;
	LineColor = ColorMacro(RGB_Gray);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingButtonInfo()
{

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, ButtonInfoPen);
	else
		SelectObject(OutputDisplay, ButtonInfoPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, ButtonInfoBrush);
	else
		SelectObject(OutputDisplay, ButtonInfoBrush);

	CurrentObjectCode = ButtonInfoObjectCode;
	LineColor = ButtonInfoColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingClearance()
{
	if (BackGroundActive)
	{
		InitDrawingBackGround(1, 1);
		return;
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, ClearancePen);
	else
		SelectObject(OutputDisplay, ClearancePen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = ClearanceObjectCode;
	LineColor = ClearanceColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawLineWhite(double x1, double y1, double x2, double y2, int32 mode)
{
	if ((mode & 4) == 0)
		StartDrawingEditingWindow();

	if ((mode & 2) == 2)
		InitDrawingColorWhite(0);
	else
		InitDrawingColorWhite(1);

	if ((mode & 1) == 1)
		SetROP2(OutputDisplay, R2_XORPEN);

	DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));

	if ((mode & 4) == 0)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingPrintingPen()
{
	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, BlackPen);
	else
		SelectObject(OutputDisplay, BlackPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = BlackObjectCode;
	LineColor = RGB_Black;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingObject(int32 ObjectType, int32 Layer, int32 ThickNess, int32 mode)
/*

mode   1     Clearance
       2     Only pen

Layer  0     Solder side
       1     Comp side
       100   Sold mask solder side
       101   Sold mask comp side
       200   Paste mask solder side
       201   Paste mask comp side

       300   Through hole

       1000  Routing keep out

       2000  Silkscreen

       3000  Placement outline

       4000  Comp outline


*/
{

	HGDIOBJ NewPen;
	int32 TempThickness, PenChanged;

	PenChanged = 0;
	TempThickness = ThickNess;

	if ((mode & 1) == 1)
	{
		if (BackGroundActive)
		{
			InitDrawingBackGround(0, 0);
			return;
		}

		if (CurrentObjectCode != ClearanceObjectCode)
		{
			if (SavePen == (HGDIOBJ) - 1)
				SavePen = SelectObject(OutputDisplay, ClearancePen);
			else
				SelectObject(OutputDisplay, ClearancePen);

			if (SaveBrush == (HGDIOBJ) - 1)
				SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
			else
				SelectObject(OutputDisplay, EmptyBrush);

			CurrentObjectCode = ClearanceObjectCode;
			LineColor = ClearanceColor;
		}

		return;
	}


	switch (Layer)
	{
	case DRILL_LAYER:
		if (BackGroundActive)
		{
			InitDrawingBackGround(0, 0);
			return;
		}

		if ((mode & 2) == 0)
		{
			if (CurrentObjectCode != ShapePinsDrillObjectCode)
			{
				if (SavePen == (HGDIOBJ) - 1)
					SavePen = SelectObject(OutputDisplay, ShapePinsDrillPen);
				else
					SelectObject(OutputDisplay, ShapePinsDrillPen);

				if (SaveBrush == (HGDIOBJ) - 1)
					SaveBrush = SelectObject(OutputDisplay, BackGroundBrush);
				else
					SelectObject(OutputDisplay, BackGroundBrush);

				CurrentObjectCode = ShapePinsDrillObjectCode;
				LineColor = ShapePinsDrillColor;
			}
		}
		else
		{
			if (CurrentObjectCode != ShapePinsDrillObjectCode2)
			{
				if (SavePen == (HGDIOBJ) - 1)
					SavePen = SelectObject(OutputDisplay, ShapePinsDrillPen);
				else
					SelectObject(OutputDisplay, ShapePinsDrillPen);

				if (SaveBrush == (HGDIOBJ) - 1)
					SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
				else
					SelectObject(OutputDisplay, EmptyBrush);

				CurrentObjectCode = ShapePinsDrillObjectCode2;
				LineColor = ShapePinsDrillColor;
			}
		}

		break;

	case DRILL_UNPLATED_LAYER:
		if (BackGroundActive)
		{
			InitDrawingBackGround(0, 0);
			return;
		}

		if ((mode & 2) == 0)
		{
			if (CurrentObjectCode != ShapePinsDrillUnplatedObjectCode)
			{
				if (SavePen == (HGDIOBJ) - 1)
					SavePen = SelectObject(OutputDisplay, ShapePinsDrillUnplatedPen);
				else
					SelectObject(OutputDisplay, ShapePinsDrillUnplatedPen);

				if (SaveBrush == (HGDIOBJ) - 1)
					SaveBrush = SelectObject(OutputDisplay, BackGroundBrush);
				else
					SelectObject(OutputDisplay, BackGroundBrush);

				CurrentObjectCode = ShapePinsDrillUnplatedObjectCode;
				LineColor = ShapePinsDrillUnplatedColor;
			}
		}
		else
		{
			if (CurrentObjectCode != ShapePinsDrillUnplatedObjectCode2)
			{
				if (SavePen == (HGDIOBJ) - 1)
					SavePen = SelectObject(OutputDisplay, ShapePinsDrillUnplatedPen);
				else
					SelectObject(OutputDisplay, ShapePinsDrillUnplatedPen);

				if (SaveBrush == (HGDIOBJ) - 1)
					SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
				else
					SelectObject(OutputDisplay, EmptyBrush);

				CurrentObjectCode = ShapePinsDrillUnplatedObjectCode2;
				LineColor = ShapePinsDrillUnplatedColor;
			}
		}

		break;

	case POWER_PAD_LAYER:
		if (BackGroundActive)
		{
			InitDrawingBackGround(0, 0);
			return;
		}

		if (CurrentObjectCode != ShapePowerPadObjectCode)
		{
			if (SavePen == (HGDIOBJ) - 1)
				SavePen = SelectObject(OutputDisplay, ShapePowerPadPen);
			else
				SelectObject(OutputDisplay, ShapePowerPadPen);

			if (SaveBrush == (HGDIOBJ) - 1)
				SaveBrush = SelectObject(OutputDisplay, ShapePowerPadBrush);
			else
				SelectObject(OutputDisplay, ShapePowerPadBrush);

			CurrentObjectCode = ShapePowerPadObjectCode;
			LineColor = ShapePowerPadColor;
		}

		break;

	case INNER_PAD_LAYER:
		if (BackGroundActive)
		{
			InitDrawingBackGround(0, 0);
			return;
		}

		if (CurrentObjectCode != ShapePinsInnerObjectCode)
		{
			if (ShapePinsInnerPen == (HGDIOBJ) NULL)
			{
				ShapePinsInnerThickNess = ThickNess;

				if ((ShapePinsInnerPen = CreatePen(PS_SOLID, 1, ShapePinsInnerColor)) == NULL)
					ok = 1;

				AddGraphicsObject((HGDIOBJ *) & ShapePinsInnerPen);
			}

			if (SavePen == (HGDIOBJ) - 1)
				SavePen = SelectObject(OutputDisplay, ShapePinsInnerPen);
			else
				SelectObject(OutputDisplay, ShapePinsInnerPen);

			if (SaveBrush == (HGDIOBJ) - 1)
				SaveBrush = SelectObject(OutputDisplay, ShapePinsInnerBrush);
			else
				SelectObject(OutputDisplay, ShapePinsInnerBrush);

			CurrentObjectCode = ShapePinsInnerObjectCode;
			LineColor = ShapePinsInnerColor;
		}

		break;

	case SOLD_MASK_BOTTOM_LAYER:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_LINE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeSoldMaskBottomObjectCode)
				{
					ThickNess = 1;

					if ((ShapeSoldMaskBottomThickNess != ThickNess) || (!ShapeSoldMaskBottomPen))
					{
						if (ShapeSoldMaskBottomPen == (HGDIOBJ) NULL)
						{
							ShapeSoldMaskBottomThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;

							if ((ShapeSoldMaskBottomPen =
							            CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskBottomColor)) == NULL)
								ok = 1;

							AddGraphicsObject((HGDIOBJ *) & ShapeSoldMaskBottomPen);
						}
						else
						{
							if (ShapeSoldMaskBottomThickNess != ThickNess)
							{
								ShapeSoldMaskBottomThickNess = ThickNess;
								ThickNess = (ThickNess - 1) | 1;
								NewPen = CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskBottomColor);
								ChangeGraphicObject(ShapeSoldMaskBottomPen, NewPen);
								ShapeSoldMaskBottomPen = NewPen;
							}
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeSoldMaskBottomPen);
					else
						SelectObject(OutputDisplay, ShapeSoldMaskBottomPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeSoldMaskBottomBrush);
					else
						SelectObject(OutputDisplay, ShapeSoldMaskBottomBrush);

					CurrentObjectCode = ShapeSoldMaskBottomObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeSoldMaskBottomObjectCode2)
				        || (ShapeSoldMaskBottomThickNess != ThickNess))
				{
					if (ShapeSoldMaskBottomPen == (HGDIOBJ) NULL)
					{
						ShapeSoldMaskBottomThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapeSoldMaskBottomPen = CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskBottomColor)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapeSoldMaskBottomPen);
					}
					else
					{
						if (ShapeSoldMaskBottomThickNess != ThickNess)
						{
							ShapeSoldMaskBottomThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskBottomColor);
							ChangeGraphicObject(ShapeSoldMaskBottomPen, NewPen);
							ShapeSoldMaskBottomPen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeSoldMaskBottomPen);
					else
						SelectObject(OutputDisplay, ShapeSoldMaskBottomPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeSoldMaskBottomObjectCode2;
				}
			}

			LineColor = ShapeSoldMaskBottomColor;
			break;
		}

		break;

	case SOLD_MASK_TOP_LAYER:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_LINE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeSoldMaskTopObjectCode)
				{
					ThickNess = 1;

					if ((ShapeSoldMaskTopThickNess != ThickNess) || (!ShapeSoldMaskTopPen))
					{
						if (ShapeSoldMaskTopPen == (HGDIOBJ) NULL)
						{
							ShapeSoldMaskTopThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;

							if ((ShapeSoldMaskTopPen = CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskTopColor)) == NULL)
								ok = 1;

							AddGraphicsObject((HGDIOBJ *) & ShapeSoldMaskTopPen);
						}
						else
						{
							if (ShapeSoldMaskTopThickNess != ThickNess)
							{
								ShapeSoldMaskTopThickNess = ThickNess;
								ThickNess = (ThickNess - 1) | 1;
								NewPen = CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskTopColor);
								ChangeGraphicObject(ShapeSoldMaskTopPen, NewPen);
								ShapeSoldMaskTopPen = NewPen;
							}
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeSoldMaskTopPen);
					else
						SelectObject(OutputDisplay, ShapeSoldMaskTopPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeSoldMaskTopBrush);
					else
						SelectObject(OutputDisplay, ShapeSoldMaskTopBrush);

					CurrentObjectCode = ShapeSoldMaskTopObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeSoldMaskTopObjectCode2) || (ShapeSoldMaskTopThickNess != ThickNess))
				{
					if (ShapeSoldMaskTopPen == (HGDIOBJ) NULL)
					{
						ShapeSoldMaskTopThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapeSoldMaskTopPen = CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskTopColor)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapeSoldMaskTopPen);
					}
					else
					{
						if (ShapeSoldMaskTopThickNess != ThickNess)
						{
							ShapeSoldMaskTopThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeSoldMaskTopColor);
							ChangeGraphicObject(ShapeSoldMaskTopPen, NewPen);
							ShapeSoldMaskTopPen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeSoldMaskTopPen);
					else
						SelectObject(OutputDisplay, ShapeSoldMaskTopPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapePinsBottomObjectCode2;
				}
			}

			LineColor = ShapeSoldMaskTopColor;
			break;
		}

		break;

	case PASTE_MASK_BOTTOM_LAYER:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_LINE:
		case OBJECT_ARC:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapePasteMaskBottomObjectCode)
				{
					ThickNess = 1;

					if ((ShapePasteMaskBottomThickNess != ThickNess) || (!ShapePasteMaskBottomPen))
					{
						if (ShapePasteMaskBottomPen == (HGDIOBJ) NULL)
						{
							ShapePasteMaskBottomThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;

							if ((ShapePasteMaskBottomPen =
							            CreatePen(PS_SOLID, ThickNess, ShapePasteMaskBottomColor)) == NULL)
								ok = 1;

							AddGraphicsObject((HGDIOBJ *) & ShapePasteMaskBottomPen);
						}
						else
						{
							if (ShapePasteMaskBottomThickNess != ThickNess)
							{
								ShapePasteMaskBottomThickNess = ThickNess;
								ThickNess = (ThickNess - 1) | 1;
								NewPen = CreatePen(PS_SOLID, ThickNess, ShapePasteMaskBottomColor);
								ChangeGraphicObject(ShapePasteMaskBottomPen, NewPen);
								ShapePasteMaskBottomPen = NewPen;
							}
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapePasteMaskBottomPen);
					else
						SelectObject(OutputDisplay, ShapePasteMaskBottomPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapePasteMaskBottomBrush);
					else
						SelectObject(OutputDisplay, ShapePasteMaskBottomBrush);

					CurrentObjectCode = ShapePasteMaskBottomObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapePasteMaskBottomObjectCode2)
				        || (ShapePasteMaskBottomThickNess != ThickNess))
				{
					if (ShapePasteMaskBottomPen == (HGDIOBJ) NULL)
					{
						ShapePasteMaskBottomThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapePasteMaskBottomPen =
						            CreatePen(PS_SOLID, ThickNess, ShapePasteMaskBottomColor)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapePasteMaskBottomPen);
					}
					else
					{
						if (ShapePasteMaskBottomThickNess != ThickNess)
						{
							ShapePasteMaskBottomThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapePasteMaskBottomColor);
							ChangeGraphicObject(ShapePasteMaskBottomPen, NewPen);
							ShapePasteMaskBottomPen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapePasteMaskBottomPen);
					else
						SelectObject(OutputDisplay, ShapePasteMaskBottomPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapePasteMaskBottomObjectCode2;
				}
			}

			LineColor = ShapePasteMaskBottomColor;
			break;
		}

		break;

	case PASTE_MASK_TOP_LAYER:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_LINE:
		case OBJECT_ARC:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapePasteMaskTopObjectCode)
				{
					ThickNess = 1;

					if ((ShapePasteMaskTopThickNess != ThickNess) || (!ShapePasteMaskTopPen))
					{
						if (ShapePasteMaskTopPen == (HGDIOBJ) NULL)
						{
							ShapePasteMaskTopThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;

							if ((ShapePasteMaskTopPen = CreatePen(PS_SOLID, ThickNess, ShapePasteMaskTopColor)) == NULL)
								ok = 1;

							AddGraphicsObject((HGDIOBJ *) & ShapePasteMaskTopPen);
						}
						else
						{
							if (ShapePasteMaskTopThickNess != ThickNess)
							{
								ShapePasteMaskTopThickNess = ThickNess;
								ThickNess = (ThickNess - 1) | 1;
								NewPen = CreatePen(PS_SOLID, ThickNess, ShapePasteMaskTopColor);
								ChangeGraphicObject(ShapePasteMaskTopPen, NewPen);
								ShapePasteMaskTopPen = NewPen;
							}
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapePasteMaskTopPen);
					else
						SelectObject(OutputDisplay, ShapePasteMaskTopPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapePasteMaskTopBrush);
					else
						SelectObject(OutputDisplay, ShapePasteMaskTopBrush);

					CurrentObjectCode = ShapePasteMaskTopObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapePasteMaskTopObjectCode2) || (ShapePasteMaskTopThickNess != ThickNess))
				{
					if (ShapePasteMaskTopPen == (HGDIOBJ) NULL)
					{
						ShapePasteMaskTopThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapePasteMaskTopPen = CreatePen(PS_SOLID, ThickNess, ShapePasteMaskTopColor)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapePasteMaskTopPen);
					}
					else
					{
						if (ShapePasteMaskTopThickNess != ThickNess)
						{
							ShapePasteMaskTopThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapePasteMaskTopColor);
							ChangeGraphicObject(ShapePasteMaskTopPen, NewPen);
							ShapePasteMaskTopPen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapePasteMaskTopPen);
					else
						SelectObject(OutputDisplay, ShapePasteMaskTopPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapePasteMaskTopObjectCode2;
				}
			}

			LineColor = ShapePasteMaskTopColor;
			break;
		}

		break;

	case SILKSCREEN_TOP_LAYER:
		switch (ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeSilkScreenTopObjectCode)
				{
					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, EmptyPen);
					else
						SelectObject(OutputDisplay, EmptyPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeSilkScreenTopBrush);
					else
						SelectObject(OutputDisplay, ShapeSilkScreenTopBrush);

					CurrentObjectCode = ShapeSilkScreenTopObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeSilkScreenTopObjectCode2) || (SilkScreenTopLineThickNess != ThickNess))
				{
					if (ShapeSilkScreenTopPen == (HGDIOBJ) NULL)
					{
						SilkScreenTopLineThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;
						ShapeSilkScreenTopPen = CreatePen(PS_SOLID, ThickNess, ShapeSilkScreenTopColor);
						AddGraphicsObject((HGDIOBJ *) & ShapeSilkScreenTopPen);
					}
					else
					{
						if (SilkScreenTopLineThickNess != ThickNess)
						{
							SilkScreenTopLineThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeSilkScreenTopColor);
							ChangeGraphicObject(ShapeSilkScreenTopPen, NewPen);
							ShapeSilkScreenTopPen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeSilkScreenTopPen);
					else
						SelectObject(OutputDisplay, ShapeSilkScreenTopPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeSilkScreenTopObjectCode2;
				}
			}

			LineColor = ShapeSilkScreenTopColor;
			break;
		}

		break;

	case SILKSCREEN_BOTTOM_LAYER:
		switch (ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeSilkScreenBottomObjectCode)
				{
					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, EmptyPen);
					else
						SelectObject(OutputDisplay, EmptyPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeSilkScreenBottomBrush);
					else
						SelectObject(OutputDisplay, ShapeSilkScreenBottomBrush);

					CurrentObjectCode = ShapeSilkScreenBottomObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeSilkScreenBottomObjectCode2)
				        || (SilkScreenBottomLineThickNess != ThickNess))
				{
					if (ShapeSilkScreenBottomPen == (HGDIOBJ) NULL)
					{
						SilkScreenBottomLineThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;
						ShapeSilkScreenBottomPen = CreatePen(PS_SOLID, ThickNess, ShapeSilkScreenBottomColor);
						AddGraphicsObject((HGDIOBJ *) & ShapeSilkScreenBottomPen);
					}
					else
					{
						if (SilkScreenBottomLineThickNess != ThickNess)
						{
							SilkScreenBottomLineThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeSilkScreenBottomColor);
							ChangeGraphicObject(ShapeSilkScreenBottomPen, NewPen);
							ShapeSilkScreenBottomPen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeSilkScreenBottomPen);
					else
						SelectObject(OutputDisplay, ShapeSilkScreenBottomPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeSilkScreenBottomObjectCode2;
				}
			}

			LineColor = ShapeSilkScreenBottomColor;
			break;
		}

		break;

	case PLACEMENT_OUTLINE_LAYER:
		switch (ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				InitDrawingBackGround(1, 1);
				return;
			}

			if (CurrentObjectCode != ShapePlacementOutLineObjectCode)
			{
				if (SavePen == (HGDIOBJ) - 1)
					SavePen = SelectObject(OutputDisplay, ShapePlacementOutLinePen);
				else
					SelectObject(OutputDisplay, ShapePlacementOutLinePen);

				if (SaveBrush == (HGDIOBJ) - 1)
					SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
				else
					SelectObject(OutputDisplay, EmptyBrush);

				CurrentObjectCode = ShapePlacementOutLineObjectCode;
				LineColor = ShapePlacementOutLineColor;
			}

			break;
		}

		break;

	case COMP_OUTLINE_LAYER:
		switch (ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeCompOutlineObjectCode)
				{
					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, EmptyPen);
					else
						SelectObject(OutputDisplay, EmptyPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeCompOutlineBrush);
					else
						SelectObject(OutputDisplay, ShapeCompOutlineBrush);

					CurrentObjectCode = ShapeCompOutlineObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeCompOutlineObjectCode2) || (CompOutLineThickNess != ThickNess))
				{
					if (ShapeCompOutlinePen == (HGDIOBJ) NULL)
					{
						CompOutLineThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapeCompOutlinePen = CreatePen(PS_SOLID, ThickNess, ShapeCompOutlineColor)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapeCompOutlinePen);
					}
					else
					{
						if (CompOutLineThickNess != ThickNess)
						{
							CompOutLineThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeCompOutlineColor);
							ChangeGraphicObject(ShapeCompOutlinePen, NewPen);
							ShapeCompOutlinePen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeCompOutlinePen);
					else
						SelectObject(OutputDisplay, ShapeCompOutlinePen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeCompOutlineObjectCode2;
				}
			}

			LineColor = ShapeCompOutlineColor;
			break;
		}

		break;

	case BOARD_OUTLINE_LAYER:
		switch (ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
			if (BackGroundActive)
			{
				InitDrawingBackGround(1, ThickNess);
				return;
			}

			if ((CurrentObjectCode != ShapeBoardOutlineObjectCode) || (BoardOutlineThickNess != ThickNess))
			{
				if (ShapeBoardOutlinePen == (HGDIOBJ) NULL)
				{
					BoardOutlineThickNess = ThickNess;
					ThickNess = (ThickNess - 1) | 1;

					if ((ShapeBoardOutlinePen = CreatePen(PS_SOLID, ThickNess, ShapeBoardOutlineColor)) == NULL)
						ok = 1;

					AddGraphicsObject((HGDIOBJ *) & ShapeBoardOutlinePen);
				}
				else
				{
					if (BoardOutlineThickNess != ThickNess)
					{
						BoardOutlineThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;
						NewPen = CreatePen(PS_SOLID, ThickNess, ShapeBoardOutlineColor);
						ChangeGraphicObject(ShapeBoardOutlinePen, NewPen);
						ShapeBoardOutlinePen = NewPen;
					}
				}

				if (SavePen == (HGDIOBJ) - 1)
					SavePen = SelectObject(OutputDisplay, ShapeBoardOutlinePen);
				else
					SelectObject(OutputDisplay, ShapeBoardOutlinePen);

				if (SaveBrush == (HGDIOBJ) - 1)
					SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
				else
					SelectObject(OutputDisplay, EmptyBrush);

				CurrentObjectCode = ShapeBoardOutlineObjectCode;
				LineColor = ShapeBoardOutlineColor;
			}

			break;
		}

		break;

	case INFO_LAYER:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeInfo1ObjectCode)
				{
					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, EmptyPen);
					else
						SelectObject(OutputDisplay, EmptyPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeInfo1Brush);
					else
						SelectObject(OutputDisplay, ShapeInfo1Brush);

					CurrentObjectCode = ShapeInfo1ObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeInfo1ObjectCode2) || (Info1ThickNess != ThickNess))
				{
					if (ShapeInfo1Pen == (HGDIOBJ) NULL)
					{
						Info1ThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapeInfo1Pen = CreatePen(PS_SOLID, ThickNess, ShapeInfo1Color)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapeInfo1Pen);
					}
					else
					{
						if (Info1ThickNess != ThickNess)
						{
							Info1ThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeInfo1Color);
							ChangeGraphicObject(ShapeInfo1Pen, NewPen);
							ShapeInfo1Pen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeInfo1Pen);
					else
						SelectObject(OutputDisplay, ShapeInfo1Pen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeInfo1ObjectCode2;
				}
			}

			LineColor = ShapeInfo1Color;
			break;
		}

		break;

	case INFO_LAYER2:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeInfo2ObjectCode)
				{
					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, EmptyPen);
					else
						SelectObject(OutputDisplay, EmptyPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeInfo2Brush);
					else
						SelectObject(OutputDisplay, ShapeInfo2Brush);

					CurrentObjectCode = ShapeInfo2ObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeInfo2ObjectCode2) || (Info2ThickNess != ThickNess))
				{
					if (ShapeInfo2Pen == (HGDIOBJ) NULL)
					{
						Info2ThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapeInfo2Pen = CreatePen(PS_SOLID, ThickNess, ShapeInfo2Color)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapeInfo2Pen);
					}
					else
					{
						if (Info2ThickNess != ThickNess)
						{
							Info2ThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeInfo2Color);
							ChangeGraphicObject(ShapeInfo2Pen, NewPen);
							ShapeInfo2Pen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeInfo2Pen);
					else
						SelectObject(OutputDisplay, ShapeInfo2Pen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeInfo2ObjectCode2;
				}
			}

			LineColor = ShapeInfo2Color;
			break;
		}

		break;

	case INFO_LAYER3:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeInfo3ObjectCode)
				{
					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, EmptyPen);
					else
						SelectObject(OutputDisplay, EmptyPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeInfo3Brush);
					else
						SelectObject(OutputDisplay, ShapeInfo3Brush);

					CurrentObjectCode = ShapeInfo3ObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeInfo3ObjectCode2) || (Info3ThickNess != ThickNess))
				{
					if (ShapeInfo3Pen == (HGDIOBJ) NULL)
					{
						Info3ThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapeInfo3Pen = CreatePen(PS_SOLID, ThickNess, ShapeInfo3Color)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapeInfo3Pen);
					}
					else
					{
						if (Info3ThickNess != ThickNess)
						{
							Info3ThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeInfo3Color);
							ChangeGraphicObject(ShapeInfo3Pen, NewPen);
							ShapeInfo3Pen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeInfo3Pen);
					else
						SelectObject(OutputDisplay, ShapeInfo3Pen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeInfo3ObjectCode2;
				}
			}

			LineColor = ShapeInfo3Color;
			break;
		}

		break;

	case INFO_LAYER4:
		switch (ObjectType)
		{
		case OBJECT_POLYGON:
		case OBJECT_LINE:
		case OBJECT_RECT:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				if (ThickNess == 0)
					InitDrawingBackGround(0, 0);
				else
					InitDrawingBackGround(1, ThickNess);

				return;
			}

			if (ThickNess == 0)
			{
				if (CurrentObjectCode != ShapeInfo4ObjectCode)
				{
					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, EmptyPen);
					else
						SelectObject(OutputDisplay, EmptyPen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, ShapeInfo4Brush);
					else
						SelectObject(OutputDisplay, ShapeInfo4Brush);

					CurrentObjectCode = ShapeInfo4ObjectCode;
				}
			}
			else
			{
				if ((CurrentObjectCode != ShapeInfo4ObjectCode2) || (Info4ThickNess != ThickNess))
				{
					if (ShapeInfo4Pen == (HGDIOBJ) NULL)
					{
						Info4ThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;

						if ((ShapeInfo4Pen = CreatePen(PS_SOLID, ThickNess, ShapeInfo4Color)) == NULL)
							ok = 1;

						AddGraphicsObject((HGDIOBJ *) & ShapeInfo4Pen);
					}
					else
					{
						if (Info4ThickNess != ThickNess)
						{
							Info4ThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;
							NewPen = CreatePen(PS_SOLID, ThickNess, ShapeInfo4Color);
							ChangeGraphicObject(ShapeInfo4Pen, NewPen);
							ShapeInfo4Pen = NewPen;
						}
					}

					if (SavePen == (HGDIOBJ) - 1)
						SavePen = SelectObject(OutputDisplay, ShapeInfo4Pen);
					else
						SelectObject(OutputDisplay, ShapeInfo4Pen);

					if (SaveBrush == (HGDIOBJ) - 1)
						SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
					else
						SelectObject(OutputDisplay, EmptyBrush);

					CurrentObjectCode = ShapeInfo4ObjectCode2;
				}
			}

			LineColor = ShapeInfo4Color;
			break;
		}

		break;

	case GEOM_NAME_LAYER:
		switch (ObjectType)
		{
		case OBJECT_TEXT:
			if (BackGroundActive)
			{
				InitDrawingBackGround(1, ThickNess);
				return;
			}

			if ((CurrentObjectCode != ShapeGeomNameObjectCode) || (GeomNameLineThickNess != ThickNess))
			{
				if (ShapeGeomNamePen == (HGDIOBJ) NULL)
				{
					GeomNameLineThickNess = ThickNess;
					ThickNess = (ThickNess - 1) | 1;
					ShapeGeomNamePen = CreatePen(PS_SOLID, ThickNess, ShapeGeomNameColor);
					AddGraphicsObject((HGDIOBJ *) & ShapeGeomNamePen);
				}
				else
				{
					if (GeomNameLineThickNess != ThickNess)
					{
						GeomNameLineThickNess = ThickNess;
						ThickNess = (ThickNess - 1) | 1;
						NewPen = CreatePen(PS_SOLID, ThickNess, ShapeGeomNameColor);
						ChangeGraphicObject(ShapeGeomNamePen, NewPen);
						ShapeGeomNamePen = NewPen;
					}
				}

				if (SavePen == (HGDIOBJ) - 1)
					SavePen = SelectObject(OutputDisplay, ShapeGeomNamePen);
				else
					SelectObject(OutputDisplay, ShapeGeomNamePen);

				if (SaveBrush == (HGDIOBJ) - 1)
					SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
				else
					SelectObject(OutputDisplay, EmptyBrush);

				CurrentObjectCode = ShapeGeomNameObjectCode;
				LineColor = ShapeGeomNameColor;
			}

			break;
		}

		break;

	case PIN_TEXT_LAYER:
		if (CurrentFontCode != PinTextFontCode)
		{
			if (SaveFont == (HGDIOBJ) - 1)
				SaveFont = SelectObject(OutputDisplay, PinTextFont);
			else
				SelectObject(OutputDisplay, PinTextFont);

			CurrentFontCode = PinTextFontCode;
		}

		if (BackGroundActive)
		{
			if (TextColor != BackGroundColor)
				SetTextColor(OutputDisplay, BackGroundColor);

			TextColor = BackGroundColor;
			BackGroundActive = 0;
//        SetBkColor(OutputDisplay,BackGroundColor);
		}
		else
		{
			if (TextColor != PinTextColor)
			{
				SetTextColor(OutputDisplay, PinTextColor);
				TextColor = PinTextColor;
			}

//        SetBkColor(OutputDisplay,BackGroundColor);
		}

		break;

	case POLYGON_DRAW_LAYER:
		if (BackGroundActive)
		{
			InitDrawingBackGround(1, 1);
			return;
		}

		InitDrawingColorWhite(0);
		break;

	default:
		if (Layer < 32)
		{
			if (Layer == 0)
			{	// Bottom
				switch (ObjectType)
				{
				case OBJECT_POLYGON:
				case OBJECT_LINE:
				case OBJECT_RECT:
				case OBJECT_CIRCLE:
				case OBJECT_ARC:
					if (BackGroundActive)
					{
						if (ThickNess == 0)
							InitDrawingBackGround(0, 0);
						else
							InitDrawingBackGround(1, ThickNess);

						return;
					}

					if (ThickNess == 0)
					{
						if (CurrentObjectCode != ShapePinsBottomObjectCode)
						{
							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, EmptyPen);
							else
								SelectObject(OutputDisplay, EmptyPen);

							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, ShapePinsBottomBrush);
							else
								SelectObject(OutputDisplay, ShapePinsBottomBrush);

							CurrentObjectCode = ShapePinsBottomObjectCode;
						}
					}
					else
					{
						if ((CurrentObjectCode != ShapePinsBottomObjectCode2)
						        || (ShapePinsBottomThickNess != ThickNess))
						{
							if (ShapePinsBottomPen == (HGDIOBJ) NULL)
							{
								ShapePinsBottomThickNess = ThickNess;
								ThickNess = (ThickNess - 1) | 1;

								if ((ShapePinsBottomPen = CreatePen(PS_SOLID, ThickNess, ShapePinsBottomColor)) == NULL)
									ok = 1;

								AddGraphicsObject((HGDIOBJ *) & ShapePinsBottomPen);
							}
							else
							{
								if (ShapePinsBottomThickNess != ThickNess)
								{
									ShapePinsBottomThickNess = ThickNess;
									ThickNess = (ThickNess - 1) | 1;
									NewPen = CreatePen(PS_SOLID, ThickNess, ShapePinsBottomColor);
									ChangeGraphicObject(ShapePinsBottomPen, NewPen);
									ShapePinsBottomPen = NewPen;
								}
							}

							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, ShapePinsBottomPen);
							else
								SelectObject(OutputDisplay, ShapePinsBottomPen);

							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
							else
								SelectObject(OutputDisplay, EmptyBrush);

							CurrentObjectCode = ShapePinsBottomObjectCode2;
						}
					}

					LineColor = ShapePinsBottomColor;
					break;
				}
			}

			if (Layer == NrPadLayers - 1)
			{	// Top
				switch (ObjectType)
				{
				case OBJECT_POLYGON:
				case OBJECT_RECT:
				case OBJECT_LINE:
				case OBJECT_CIRCLE:
				case OBJECT_ARC:
					if (BackGroundActive)
					{
						if (ThickNess == 0)
							InitDrawingBackGround(0, 0);
						else
							InitDrawingBackGround(1, ThickNess);

						return;
					}

					if (ThickNess == 0)
					{
						if (CurrentObjectCode != ShapePinsTopObjectCode)
						{
							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, EmptyPen);
							else
								SelectObject(OutputDisplay, EmptyPen);

							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, ShapePinsTopBrush);
							else
								SelectObject(OutputDisplay, ShapePinsTopBrush);

							CurrentObjectCode = ShapePinsTopObjectCode;
						}
					}
					else
					{
						if ((CurrentObjectCode != ShapePinsTopObjectCode2) || (ShapePinsTopThickNess != ThickNess))
						{
							if (ShapePinsTopPen == (HGDIOBJ) NULL)
							{
								ShapePinsTopThickNess = ThickNess;
								ThickNess = (ThickNess - 1) | 1;

								if ((ShapePinsTopPen = CreatePen(PS_SOLID, ThickNess, ShapePinsTopColor)) == NULL)
									ok = 1;

								AddGraphicsObject((HGDIOBJ *) & ShapePinsTopPen);
							}
							else
							{
								if (ShapePinsTopThickNess != ThickNess)
								{
									ShapePinsTopThickNess = ThickNess;
									ThickNess = (ThickNess - 1) | 1;
									NewPen = CreatePen(PS_SOLID, ThickNess, ShapePinsTopColor);
									ChangeGraphicObject(ShapePinsTopPen, NewPen);
									ShapePinsTopPen = NewPen;
								}
							}

							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, ShapePinsTopPen);
							else
								SelectObject(OutputDisplay, ShapePinsTopPen);

							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
							else
								SelectObject(OutputDisplay, EmptyBrush);

							CurrentObjectCode = ShapePinsTopObjectCode2;
						}
					}

					LineColor = ShapePinsTopColor;
					break;
				}
			}

			if (CheckIfInnerLayer(Layer))
			{
				switch (ObjectType)
				{
				case OBJECT_POLYGON:
				case OBJECT_RECT:
				case OBJECT_LINE:
				case OBJECT_CIRCLE:
				case OBJECT_ARC:
					if (BackGroundActive)
					{
						if (ThickNess == 0)
							InitDrawingBackGround(0, 0);
						else
							InitDrawingBackGround(1, ThickNess);

						return;
					}

					if (TempThickness == 0)
					{
						ThickNess = 1;

						if (CurrentObjectCode != ShapePinsInnerObjectCode)
						{
							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, ShapePinsInnerBrush);
							else
								SelectObject(OutputDisplay, ShapePinsInnerBrush);

							PenChanged = 1;
						}
					}
					else
					{
						if ((CurrentObjectCode != ShapePinsInnerObjectCode2) || (ShapePinsInnerThickNess != ThickNess))
						{
							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
							else
								SelectObject(OutputDisplay, EmptyBrush);

							PenChanged = 1;
						}
					}

					if (PenChanged)
					{
						if (ShapePinsInnerPen == (HGDIOBJ) NULL)
						{
							ShapePinsInnerThickNess = ThickNess;
							ThickNess = (ThickNess - 1) | 1;

							if ((ShapePinsInnerPen = CreatePen(PS_SOLID, ThickNess, ShapePinsInnerColor)) == NULL)
								ok = 1;

							AddGraphicsObject((HGDIOBJ *) & ShapePinsInnerPen);
						}
						else
						{
							if (ShapePinsInnerThickNess != ThickNess)
							{
								ShapePinsInnerThickNess = ThickNess;
								ThickNess = (ThickNess - 1) | 1;
								NewPen = CreatePen(PS_SOLID, ThickNess, ShapePinsInnerColor);
								ChangeGraphicObject(ShapePinsInnerPen, NewPen);
								ShapePinsInnerPen = NewPen;
							}
						}

						if (SavePen == (HGDIOBJ) - 1)
							SavePen = SelectObject(OutputDisplay, ShapePinsInnerPen);
						else
							SelectObject(OutputDisplay, ShapePinsInnerPen);
					}

					if (TempThickness == 0)
						CurrentObjectCode = ShapePinsInnerObjectCode;
					else
						CurrentObjectCode = ShapePinsInnerObjectCode2;

					LineColor = ShapePinsInnerColor;
				}
			}
		}
		else
		{
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			{
				if (Layer == ROUTING_KEEPOUT_LAYER + NrPadLayers - 1)
				{	// Top
					switch (ObjectType)
					{
					case OBJECT_POLYGON:
					case OBJECT_CIRCLE:
						if (BackGroundActive)
						{
							InitDrawingBackGround(0, 0);
							return;
						}

						if (CurrentObjectCode != ShapeRoutingKeepoutTopObjectCode)
						{
							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, ShapeRoutingKeepoutTopPen);
							else
								SelectObject(OutputDisplay, ShapeRoutingKeepoutTopPen);

							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, ShapeRoutingKeepoutTopBrush);
							else
								SelectObject(OutputDisplay, ShapeRoutingKeepoutTopBrush);

							CurrentObjectCode = ShapeRoutingKeepoutTopObjectCode;
							LineColor = ShapeRoutingKeepoutTopColor;
						}

						break;

					case OBJECT_RECT:
						if (BackGroundActive)
						{
							InitDrawingBackGround(0, 0);
							return;
						}

						if (CurrentObjectCode != ShapeRoutingKeepoutTopObjectCode)
						{
							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, ShapeRoutingKeepoutTopPen);
							else
								SelectObject(OutputDisplay, ShapeRoutingKeepoutTopPen);

							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, ShapeRoutingKeepoutTopBrush);
							else
								SelectObject(OutputDisplay, ShapeRoutingKeepoutTopBrush);

							CurrentObjectCode = ShapeRoutingKeepoutTopObjectCode;
							LineColor = ShapeRoutingKeepoutTopColor;
						}

						break;
					}
				}

				if (Layer == ROUTING_KEEPOUT_LAYER)
				{	// Bottom
					switch (ObjectType)
					{
					case OBJECT_POLYGON:
					case OBJECT_CIRCLE:
					case OBJECT_RECT:
						if (BackGroundActive)
						{
							InitDrawingBackGround(0, 0);
							return;
						}

						if (CurrentObjectCode != ShapeRoutingKeepoutBottomObjectCode)
						{
							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, ShapeRoutingKeepoutBottomPen);
							else
								SelectObject(OutputDisplay, ShapeRoutingKeepoutBottomPen);

							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, ShapeRoutingKeepoutBottomBrush);
							else
								SelectObject(OutputDisplay, ShapeRoutingKeepoutBottomBrush);

							CurrentObjectCode = ShapeRoutingKeepoutBottomObjectCode;
							LineColor = ShapeRoutingKeepoutBottomColor;
						}

						break;
					}
				}

				if (CheckIfInnerLayer(Layer - ROUTING_KEEPOUT_LAYER))
				{	// Routing keepout inner layer
					switch (ObjectType)
					{
					case OBJECT_POLYGON:
					case OBJECT_CIRCLE:
					case OBJECT_RECT:
						if (BackGroundActive)
						{
							InitDrawingBackGround(0, 0);
							return;
						}

						if (CurrentObjectCode != ShapeRoutingKeepoutInnerObjectCode)
						{
							if (SavePen == (HGDIOBJ) - 1)
								SavePen = SelectObject(OutputDisplay, EmptyPen);
							else
								SelectObject(OutputDisplay, EmptyPen);

//                  if (SavePen==(HGDIOBJ)-1) SavePen=SelectObject(OutputDisplay,ShapeRoutingKeepoutInnerPen);
//                  else SelectObject(OutputDisplay,ShapeRoutingKeepoutInnerPen);
							if (SaveBrush == (HGDIOBJ) - 1)
								SaveBrush = SelectObject(OutputDisplay, ShapeRoutingKeepoutInnerBrush);
							else
								SelectObject(OutputDisplay, ShapeRoutingKeepoutInnerBrush);

							CurrentObjectCode = ShapeRoutingKeepoutInnerObjectCode;
							LineColor = ShapeRoutingKeepoutInnerColor;
						}

						break;
					}
				}
			}
		}

		break;

	case OBJECT_SELECTED_LAYER:
		InitDrawingColorWhite(0);
		break;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void SetBackGroundActive(int32 mode)
{
	BackGroundActive = 1;
	CurrentObjectCode = 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingBackGroundBrush()
{
	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, BackGroundBrush);
	else
		SelectObject(OutputDisplay, BackGroundBrush);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingBackGround(int32 mode, int32 ThickNess)
{
	HPEN NewPen;

	if (mode == 0)
	{
		if (ThickNess == 0)
			ThickNess = 1;
	}

	if (((mode == 1) && (CurrentObjectCode != BackGroundObjectCode2))
	        || ((mode == 0) && (CurrentObjectCode != BackGroundObjectCode1)))
	{
		if (BackGroundPenThickNess2 != ThickNess)
		{
			if (BackGroundPen2 == (HGDIOBJ) NULL)
			{
				BackGroundPenThickNess2 = ThickNess;
				ThickNess |= 1;
				BackGroundPen2 = CreatePen(PS_SOLID, ThickNess, BackGroundColor);
				AddGraphicsObject((HGDIOBJ *) & BackGroundPen2);
			}
			else
			{
				if (BackGroundPenThickNess2 != ThickNess)
				{
					BackGroundPenThickNess2 = ThickNess;
					ThickNess = (ThickNess - 1) | 1;
					NewPen = CreatePen(PS_SOLID, ThickNess, BackGroundColor);
					ChangeGraphicObject(BackGroundPen2, NewPen);
					BackGroundPen2 = NewPen;
				}
			}
		}

		if (SavePen == (HGDIOBJ) - 1)
			SavePen = SelectObject(OutputDisplay, BackGroundPen2);
		else
			SelectObject(OutputDisplay, BackGroundPen2);
	}

	if (mode == 1)
	{
		if (CurrentObjectCode != BackGroundObjectCode2)
		{
			if (SaveBrush == (HGDIOBJ) - 1)
				SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
			else
				SelectObject(OutputDisplay, EmptyBrush);

			CurrentObjectCode = BackGroundObjectCode2;
			LineColor = BackGroundColor;
		}
	}
	else
	{
		if (CurrentObjectCode != BackGroundObjectCode1)
		{
			if (SaveBrush == (HGDIOBJ) - 1)
				SaveBrush = SelectObject(OutputDisplay, BackGroundBrush);
			else
				SelectObject(OutputDisplay, BackGroundBrush);

			CurrentObjectCode = BackGroundObjectCode1;
			LineColor = BackGroundColor;
		}
	}

	BackGroundActive = 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void ExitDrawing()
{
	if (SaveBrush != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SaveBrush);
		SaveBrush = (HGDIOBJ) - 1;
		CurrentObjectCode = 0;
	}

	if (SavePen != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SavePen);
		SavePen = (HGDIOBJ) - 1;
		CurrentObjectCode = 0;
	}

	if (SaveFont != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SaveFont);
		SaveFont = (HGDIOBJ) - 1;
		CurrentFontCode = 0;
	}

	TextColor = (COLORREF) - 1;
	/*
	  SilkScreenLineThickNess=-1;
	  CompOutLineThickNess=-1;
	  Info1ThickNess=-1;
	  Info2ThickNess=-1;
	  Info3ThickNess=-1;
	  Info4ThickNess=-1;
	  BoardOutlineThickNess=-1;
	  GeomNameLineThickNess=-1;
	  BackGroundPenThickNess2=-1;
	  ShapePasteMaskTopThickNess=-1;
	  ShapePasteMaskBottomThickNess=-1;
	  ShapeSoldMaskTopThickNess=-1;
	  ShapeSoldMaskBottomThickNess=-1;
	  ShapePinsBottomThickNess=-1;
	  ShapePinsTopThickNess=-1;

	*/
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void LoadDefaultColors()
{

	GEOMColors[ShapeSilkScreenTopColorNr] = RGB_LightBrown;
	GEOMColors[ShapeSilkScreenBottomColorNr] = RGB_LightBrown2;
	GEOMColors[ShapeCompOutlineColorNr] = RGB_Brown;
	GEOMColors[ShapePlacementOutLineColorNr] = RGB_Yellow;
	GEOMColors[ShapePinsDrillColorNr] = RGB_DarkRed;
	GEOMColors[ShapePinsDrillUnplatedColorNr] = RGB_LightBlue;
	GEOMColors[ShapePinsTopColorNr] = RGB_DarkCyan;
	GEOMColors[ShapePinsBottomColorNr] = RGB_LightGray;
	GEOMColors[ShapePinsInnerColorNr] = RGB_DarkPink;
	GEOMColors[ShapePasteMaskTopColorNr] = RGB_Pink;
	GEOMColors[ShapePasteMaskBottomColorNr] = RGB_Green;
	GEOMColors[ShapeSoldMaskTopColorNr] = RGB_Gray;
	GEOMColors[ShapeSoldMaskBottomColorNr] = RGB_Blue;
	GEOMColors[ShapeGeomNameColorNr] = RGB_Yellow;
	GEOMColors[ShapePowerPadColorNr] = RGB_LightRed;
	GEOMColors[ShapeRoutingKeepoutTopColorNr] = RGB_LightGreen;
	GEOMColors[ShapeRoutingKeepoutBottomColorNr] = RGB_LightMagenta;
	GEOMColors[ShapeRoutingKeepoutInnerColorNr] = RGB_Magenta;
	GEOMColors[ShapeBoardOutlineColorNr] = RGB_Gray;
	GEOMColors[ShapeInfo1ColorNr] = RGB_Cyan;
	GEOMColors[ShapeInfo2ColorNr] = RGB_Green;
	GEOMColors[ShapeInfo3ColorNr] = RGB_LightGreen;
	GEOMColors[ShapeInfo4ColorNr] = RGB_LightBlue;
	GEOMColors[ClearanceColorNr] = RGB_Gray;
	GEOMColors[ButtonInfoColorNr] = RGB_Yellow;
	GEOMColors[GridColorNr] = RGB_Gray;
	GEOMColors[BackGroundColorNr] = RGB_Black;
	GEOMColors[PinTextColorNr] = RGB_White;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void GraphicsMain()
{
	SaveBrush = (HGDIOBJ) - 1;
	SaveFont = (HGDIOBJ) - 1;
	SavePen = (HGDIOBJ) - 1;
	CurrentPalette = (HPALETTE) 0;

	LoadDefaultColors();

	SilkScreenTopLineThickNess = -1;
	SilkScreenBottomLineThickNess = -1;
	CompOutLineThickNess = -1;
	GeomNameLineThickNess = -1;
	Info1ThickNess = -1;
	Info2ThickNess = -1;
	Info3ThickNess = -1;
	Info4ThickNess = -1;
	BoardOutlineThickNess = -1;
	BackGroundPenThickNess2 = -1;
	ShapePasteMaskTopThickNess = -1;
	ShapePasteMaskBottomThickNess = -1;
	ShapeSoldMaskTopThickNess = -1;
	ShapeSoldMaskBottomThickNess = -1;
	ShapePinsBottomThickNess = -1;
	ShapePinsTopThickNess = -1;
	ShapePinsInnerThickNess = -1;
	CurrentObjectCode = -1;
	CurrentFontCode = -1;
	NrGraphicsObjects = 0;
	TextColor = (COLORREF) - 10;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
