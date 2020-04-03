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
#include  "string.h"
#include  "ellipss.h"


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
#define RGB_Orange                              RGB(255,128,0  )
#define RGB_Pink                                RGB(236,151,227)
#define RGB_DarkPink                            RGB(207, 37,190)
#define RGB_LightPink                           RGB(255,200,240)
#define RGB_Brown                               RGB(128,64 ,0  )
#define RGB_LightOrange                         RGB(223,188,96 )
#define RGB_Violet                              RGB(128,0  ,128)

#define ALIGN_LEFT_BOTTOM                       0
#define ALIGN_LEFT_CENTRE                       1
#define ALIGN_LEFT_TOP                          2
#define ALIGN_CENTRE_BOTTOM                     3
#define ALIGN_CENTRE_CENTRE                     4
#define ALIGN_CENTRE_TOP                        5
#define ALIGN_RIGHT_BOTTOM                      6
#define ALIGN_RIGHT_CENTRE                      7
#define ALIGN_RIGHT_TOP                         8


COLORREF BlackColor, DarkGrayColor, GrayColor, LightGrayColor, WhiteColor, RedColor, BlueColor, GreenColor, YellowColor,
         CyanColor, MagentaColor, DarkMagentaColor, DarkCyanColor, DarkBlueColor, DarkRedColor, DarkGreenColor,
         DarkMagentaColor, LightRedColor, LightBlueColor, LightGreenColor, LightMagentaColor, OrangeColor, PinkColor,
         DarkPinkColor, BrownColor, VioletColor, GridColor, LineColor, BackGroundColor, WireColor, BusColor,
         BusConnectionColor, GlobalConnectionColor, JunctionColor, OnePinNetColor, NetLabelColor, InstanceRefTextColor,
         InstanceValueTextColor, SymbolPinColor, SymbolPinBusColor, SymbolPinTextColor, SymbolPowerPinTextColor,
         SymbolPinBusTextColor, SymbolLinesColor, SymbolRectsColor, SymbolTextsColor, SymbolArcsColor, SymbolCirclesColor,
         ObjectLinesColor, ObjectRectsColor, ObjectCirclesColor, ObjectArcsColor, ObjectTextsColor, ButtonInfoColor;

COLORREF SCHColors[64];


LOGBRUSH JunctionBrushObject, SymbolPinBrushObject, SymbolPinBusBrushObject, BusConnectionBrushObject,
         GlobalConnectionBrushObject, SpecialObjectsBrushObject, ButtonInfoBrushObject, WhiteBrushObject,
         BackGroundBrushObject, BlackBrushObject, EmptyBrushObject;

UINT BrushMode;

HGDIOBJ SavePen, SaveBrush, TempBrush;

HBRUSH EmptyBrush, WhiteBrush, BlackBrush, SymbolPinBrush, BackGroundBrush, SymbolPinBusBrush, ButtonInfoBrush,
       JunctionBrush, BusConnectionBrush, SpecialObjectsBrush, GlobalConnectionBrush;

HPEN DrawPen, WirePen, WhitePen, WhitePen2, BlackPen, EmptyPen, BusPen, BackGroundPen, BackGroundPen2, ButtonInfoPen,
     BusConnectionPen, GlobalConnectionPen, NetLabelPen, JunctionPen, OnePinNetPen, SymbolPinPen, SymbolPowerPinPen,
     SymbolPinBusPen, SymbolPinTextPen, SymbolPowerPinTextPen, SymbolPinBusTextPen, SymbolLinesPen, SymbolRectsPen,
     SymbolCirclesPen, SymbolArcsPen, SymbolTextsPen, InstanceRefTextPen, InstanceValueTextPen, ObjectLinesPen,
     ObjectRectsPen, ObjectCirclesPen, ObjectArcsPen, ObjectTextsPen;

HFONT SaveFont, SpecialFont;

int32 PrintingThickness, BackGroundPenThickness2, WireThickness, BusThickness, BusConnectionThickness,
      GlobalConnectionThickness, NetLabelThickness, OnePinNetThickness, InstanceRefTextThickness,
      InstanceValueTextThickness, SymbolPinTextThickness, SymbolPinBusTextThickness, SymbolPowerPinTextThickness,
      SymbolLinesThickness, SymbolRectsThickness, SymbolCirclesThickness, SymbolArcsThickness, SymbolTextsThickness,
      ObjectLinesThickness, ObjectRectsThickness, ObjectCirclesThickness, ObjectArcsThickness, ObjectTextsThickness,
      SelectColorMode, NrGraphicsObjects, ok;

HGDIOBJ *GraphicsObjects[200];



extern int32 BackGroundActive, Printing, EditingSymbol, CurrentObjectCode;
extern HDC OutputDisplay;


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void StartDrawingEditingWindow(void);

void EndDrawingEditingWindow(void);

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CreateObjectColorsScreen()
{
	BackGroundColor = SCHColors[BackGroundColorNr];
	WireColor = SCHColors[WireColorNr];
	BusColor = SCHColors[BusColorNr];
	BusConnectionColor = SCHColors[BusConnectionColorNr];
	GlobalConnectionColor = SCHColors[GlobalConnectionColorNr];
	JunctionColor = SCHColors[JunctionColorNr];
	OnePinNetColor = SCHColors[OnePinNetColorNr];
	NetLabelColor = SCHColors[NetLabelColorNr];
	InstanceRefTextColor = SCHColors[InstanceRefTextColorNr];
	InstanceValueTextColor = SCHColors[InstanceValueTextColorNr];
	SymbolPinColor = SCHColors[SymbolPinColorNr];
	SymbolPinBusColor = SCHColors[SymbolPinBusColorNr];
	SymbolPinTextColor = SCHColors[SymbolPinTextColorNr];
	SymbolPowerPinTextColor = SCHColors[SymbolPowerPinTextColorNr];
	SymbolPinBusTextColor = SCHColors[SymbolPinBusTextColorNr];
	SymbolLinesColor = SCHColors[SymbolLineColorNr];
	SymbolRectsColor = SCHColors[SymbolRectColorNr];
	SymbolCirclesColor = SCHColors[SymbolCircleColorNr];
	SymbolArcsColor = SCHColors[SymbolArcColorNr];
	SymbolTextsColor = SCHColors[SymbolTextColorNr];
	ObjectLinesColor = SCHColors[ObjectLineColorNr];
	ObjectRectsColor = SCHColors[ObjectRectColorNr];
	ObjectCirclesColor = SCHColors[ObjectCircleColorNr];
	ObjectArcsColor = SCHColors[ObjectArcColorNr];
	ObjectTextsColor = SCHColors[ObjectTextColorNr];
	ButtonInfoColor = SCHColors[ButtonInfoColorNr];
	GridColor = SCHColors[GridColorNr];
	SelectColorMode = R2_WHITE;

	if (SCHColors[BackGroundColorNr] == RGB_White)
		SelectColorMode = R2_NOTMERGEPEN;

//    if (!Printing) SetROP2(OutputDisplay,SelectColorMode);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CreateObjectColorsPrint(int32 mode)
{
	if (mode == 1)
	{
		BackGroundColor = SCHColors[BackGroundColorNr];
		WireColor = SCHColors[WireColorNr];
		BusColor = SCHColors[BusColorNr];
		BusConnectionColor = SCHColors[BusConnectionColorNr];
		GlobalConnectionColor = SCHColors[GlobalConnectionColorNr];
		JunctionColor = SCHColors[JunctionColorNr];
		OnePinNetColor = SCHColors[OnePinNetColorNr];
		NetLabelColor = SCHColors[NetLabelColorNr];
		InstanceRefTextColor = SCHColors[InstanceRefTextColorNr];
		InstanceValueTextColor = SCHColors[InstanceValueTextColorNr];
		SymbolPinColor = SCHColors[SymbolPinColorNr];
		SymbolPinBusColor = SCHColors[SymbolPinBusColorNr];
		SymbolPinTextColor = SCHColors[SymbolPinTextColorNr];
		SymbolPowerPinTextColor = SCHColors[SymbolPowerPinTextColorNr];
		SymbolPinBusTextColor = SCHColors[SymbolPinBusTextColorNr];
		SymbolLinesColor = SCHColors[SymbolLineColorNr];
		SymbolRectsColor = SCHColors[SymbolRectColorNr];
		SymbolCirclesColor = SCHColors[SymbolCircleColorNr];
		SymbolArcsColor = SCHColors[SymbolArcColorNr];
		SymbolTextsColor = SCHColors[SymbolTextColorNr];
		ObjectLinesColor = SCHColors[ObjectLineColorNr];
		ObjectRectsColor = SCHColors[ObjectRectColorNr];
		ObjectCirclesColor = SCHColors[ObjectCircleColorNr];
		ObjectArcsColor = SCHColors[ObjectArcColorNr];
		ObjectTextsColor = SCHColors[ObjectTextColorNr];
		ButtonInfoColor = SCHColors[ButtonInfoColorNr];
		GridColor = SCHColors[GridColorNr];
	}
	else
	{
		BackGroundColor = RGB_White;
		WireColor = RGB_Black;
		BusColor = RGB_Black;
		BusConnectionColor = RGB_Black;
		GlobalConnectionColor = RGB_Black;
		JunctionColor = RGB_Black;
		OnePinNetColor = RGB_Black;
		NetLabelColor = RGB_Black;
		InstanceRefTextColor = RGB_Black;
		InstanceValueTextColor = RGB_Black;
		SymbolPinColor = RGB_Black;
		SymbolPinBusColor = RGB_Black;
		SymbolPinTextColor = RGB_Black;
		SymbolPowerPinTextColor = RGB_Black;
		SymbolPinBusTextColor = RGB_Black;
		SymbolLinesColor = RGB_Black;
		SymbolRectsColor = RGB_Black;
		SymbolCirclesColor = RGB_Black;
		SymbolArcsColor = RGB_Black;
		SymbolTextsColor = RGB_Black;
		ObjectLinesColor = RGB_Black;
		ObjectRectsColor = RGB_Black;
		ObjectCirclesColor = RGB_Black;
		ObjectArcsColor = RGB_Black;
		ObjectTextsColor = RGB_Black;

		GridColor = RGB_Black;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CreateDrawObjects(int32 mode)
{
	if ((mode & 2) == 0)
	{
		if (!Printing)
			CreateObjectColorsScreen();
		else
			CreateObjectColorsPrint(mode);
	}
	else
		CreateObjectColorsPrint(0);

	EmptyBrushObject.lbHatch = (LONG) NULL;
	EmptyBrushObject.lbStyle = BS_NULL;
	EmptyBrushObject.lbColor = (LONG) NULL;
	EmptyBrush = CreateBrushIndirect(&EmptyBrushObject);
	AddGraphicsObject((HGDIOBJ *) & EmptyBrush);

	BackGroundBrushObject.lbHatch = (LONG) NULL;
	BackGroundBrushObject.lbStyle = BS_SOLID;
	BackGroundBrushObject.lbColor = BackGroundColor;
	BackGroundBrush = CreateBrushIndirect(&BackGroundBrushObject);
	AddGraphicsObject((HGDIOBJ *) & BackGroundBrush);
	BackGroundPen = CreatePen(PS_SOLID, 1, BackGroundColor);
	AddGraphicsObject((HGDIOBJ *) & BackGroundPen);
	EmptyPen = CreatePen(PS_NULL, 0, 0);
	AddGraphicsObject((HGDIOBJ *) & EmptyPen);

//  BlackPen   = CreatePen(PS_SOLID,1,ColorMacro(RGB_Black));
	WhitePen = CreatePen(PS_SOLID, 1, RGB_White);
	AddGraphicsObject((HGDIOBJ *) & WhitePen);
	WhitePen2 = CreatePen(PS_DOT, 1, RGB_White);
	AddGraphicsObject((HGDIOBJ *) & WhitePen2);


	if (EditingSymbol)
	{
		SymbolPinBrushObject.lbColor = SymbolPinColor;
		SymbolPinBrushObject.lbHatch = (LONG) NULL;
		SymbolPinBrushObject.lbStyle = BS_SOLID;

		SymbolPinBusBrushObject.lbColor = SymbolPinBusColor;
		SymbolPinBusBrushObject.lbHatch = (LONG) NULL;
		SymbolPinBusBrushObject.lbStyle = BS_SOLID;

		SymbolPinBrush = CreateBrushIndirect(&SymbolPinBrushObject);
		SymbolPinBusBrush = CreateBrushIndirect(&SymbolPinBusBrushObject);

		AddGraphicsObject((HGDIOBJ *) & SymbolPinBrush);
		AddGraphicsObject((HGDIOBJ *) & SymbolPinBusBrush);
	}
	else
	{
		GlobalConnectionBrushObject.lbColor = GlobalConnectionColor;
		GlobalConnectionBrushObject.lbHatch = (LONG) NULL;
		GlobalConnectionBrushObject.lbStyle = BS_SOLID;

		JunctionBrushObject.lbColor = JunctionColor;
		JunctionBrushObject.lbHatch = (LONG) NULL;
		JunctionBrushObject.lbStyle = BS_SOLID;


		BusConnectionBrushObject.lbColor = BusConnectionColor;
		BusConnectionBrushObject.lbHatch = (LONG) NULL;
		BusConnectionBrushObject.lbStyle = BS_SOLID;

		JunctionPen = CreatePen(PS_SOLID, 1, JunctionColor);
		OnePinNetPen = CreatePen(PS_SOLID, 1, JunctionColor);

		BusConnectionBrush = CreateBrushIndirect(&BusConnectionBrushObject);
		GlobalConnectionBrush = CreateBrushIndirect(&GlobalConnectionBrushObject);
		JunctionBrush = CreateBrushIndirect(&JunctionBrushObject);

		AddGraphicsObject((HGDIOBJ *) & JunctionPen);
		AddGraphicsObject((HGDIOBJ *) & BusConnectionBrush);
		AddGraphicsObject((HGDIOBJ *) & GlobalConnectionBrush);
		AddGraphicsObject((HGDIOBJ *) & JunctionBrush);
	}

	SpecialObjectsBrushObject.lbColor = ObjectLinesColor;
	SpecialObjectsBrushObject.lbHatch = (LONG) NULL;
	SpecialObjectsBrushObject.lbStyle = BS_SOLID;
	SpecialObjectsBrush = CreateBrushIndirect(&SpecialObjectsBrushObject);
	AddGraphicsObject((HGDIOBJ *) & SpecialObjectsBrush);

	ButtonInfoBrushObject.lbHatch = (LONG) NULL;
	ButtonInfoBrushObject.lbStyle = BS_SOLID;
	ButtonInfoBrushObject.lbColor = SCHColors[ButtonInfoColorNr];
	ButtonInfoBrush = CreateBrushIndirect(&ButtonInfoBrushObject);
	AddGraphicsObject((HGDIOBJ *) & ButtonInfoBrush);


	ButtonInfoPen = CreatePen(PS_SOLID, 1, RGB_White);

	AddGraphicsObject((HGDIOBJ *) & ButtonInfoPen);

}

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
	GraphicsObjects[NrGraphicsObjects] = Object;
	NrGraphicsObjects++;
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
#ifdef _DEBUG

		if (*Point == NULL)
			ok = 1;

#endif

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
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorWhite()
{
	if (BackGroundActive)
	{
		InitDrawingBackGround(0, 0);
		return;
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, WhitePen);
	else
		SelectObject(OutputDisplay, WhitePen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = WhiteObjectCode;
	LineColor = RGB_White;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingColorWhite2()
{
	if (BackGroundActive)
	{
		InitDrawingBackGround(0, 0);
		return;
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, WhitePen2);
	else
		SelectObject(OutputDisplay, WhitePen2);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = White2ObjectCode;
	LineColor = RGB_White;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingWires(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == WireObjectCode) && (WireThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((WirePen == (HGDIOBJ) NULL) || (WireThickness != Thickness))
	{
		WireThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (WirePen == (HGDIOBJ) NULL)
		{
			WirePen = CreatePen(PS_SOLID, Thickness, WireColor);
			AddGraphicsObject((HGDIOBJ *) & WirePen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, WireColor);
			ChangeGraphicObject(WirePen, NewPen);
			WirePen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, WirePen);
	else
		SelectObject(OutputDisplay, WirePen);

	CurrentObjectCode = WireObjectCode;
	LineColor = WireColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingBusses(int32 Thickness)
{
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	if ((CurrentObjectCode == BusObjectCode) && (BusThickness == Thickness))
		return;

	if ((BusPen == (HGDIOBJ) NULL) || (BusThickness != Thickness))
	{
		BusThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (BusPen == (HGDIOBJ) NULL)
		{
			BusPen = CreatePen(PS_SOLID, Thickness, BusColor);
			AddGraphicsObject((HGDIOBJ *) & BusPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, BusColor);
			ChangeGraphicObject(BusPen, NewPen);
			BusPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, BusPen);
	else
		SelectObject(OutputDisplay, BusPen);

	CurrentObjectCode = BusObjectCode;
	LineColor = BusColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingJunctions()
{
	if (BackGroundActive)
	{
		InitDrawingBackGround(0, 0);
		return;
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, JunctionPen);
	else
		SelectObject(OutputDisplay, JunctionPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, JunctionBrush);
	else
		SelectObject(OutputDisplay, JunctionBrush);

	CurrentObjectCode = JunctionObjectCode;
	LineColor = JunctionColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingOnePinNets(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == OnePinNetObjectCode) && (OnePinNetThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((OnePinNetPen == (HGDIOBJ) NULL) || (OnePinNetThickness != Thickness))
	{
		OnePinNetThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (OnePinNetPen == (HGDIOBJ) NULL)
		{
			OnePinNetPen = CreatePen(PS_SOLID, Thickness, OnePinNetColor);
			AddGraphicsObject((HGDIOBJ *) & OnePinNetPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, OnePinNetColor);
			ChangeGraphicObject(OnePinNetPen, NewPen);
			OnePinNetPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, OnePinNetPen);
	else
		SelectObject(OutputDisplay, OnePinNetPen);

	CurrentObjectCode = OnePinNetObjectCode;
	LineColor = OnePinNetColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingBusConnections(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if (OldThickness == 0)
	{
		if ((CurrentObjectCode == BusConnectionObjectCode) && (BusConnectionThickness == Thickness))
			return;
	}
	else
	{
		if (CurrentObjectCode == BusConnectionObjectCode2)
			return;
	}

	if (Thickness == 0)
		Thickness = 1;

	if ((BusConnectionPen == (HGDIOBJ) NULL) || (BusConnectionThickness != Thickness))
	{
		BusConnectionThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (BusConnectionPen == (HGDIOBJ) NULL)
		{
			BusConnectionPen = CreatePen(PS_SOLID, Thickness, BusConnectionColor);
			AddGraphicsObject((HGDIOBJ *) & BusConnectionPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, BusConnectionColor);
			ChangeGraphicObject(BusConnectionPen, NewPen);
			BusConnectionPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, BusConnectionPen);
	else
		SelectObject(OutputDisplay, BusConnectionPen);

	if (OldThickness == 0)
	{
		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, BusConnectionBrush);
		else
			SelectObject(OutputDisplay, BusConnectionBrush);

		CurrentObjectCode = BusConnectionObjectCode;
	}
	else
	{
		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
		else
			SelectObject(OutputDisplay, EmptyBrush);

		CurrentObjectCode = BusConnectionObjectCode2;
	}

	LineColor = BusConnectionColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingGlobalConnections(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if (OldThickness == 0)
	{
		if ((CurrentObjectCode == GlobalConnectionObjectCode) && (GlobalConnectionThickness == Thickness))
			return;
	}
	else
	{
		if (CurrentObjectCode == GlobalConnectionObjectCode2)
			return;
	}

	if (Thickness == 0)
		Thickness = 1;

	if ((GlobalConnectionPen == (HGDIOBJ) NULL) || (GlobalConnectionThickness != Thickness))
	{
		GlobalConnectionThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (GlobalConnectionPen == (HGDIOBJ) NULL)
		{
			GlobalConnectionPen = CreatePen(PS_SOLID, Thickness, GlobalConnectionColor);
			AddGraphicsObject((HGDIOBJ *) & GlobalConnectionPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, GlobalConnectionColor);
			ChangeGraphicObject(GlobalConnectionPen, NewPen);
			GlobalConnectionPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, GlobalConnectionPen);
	else
		SelectObject(OutputDisplay, GlobalConnectionPen);

	if (OldThickness == 0)
	{
		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, GlobalConnectionBrush);
		else
			SelectObject(OutputDisplay, GlobalConnectionBrush);

		CurrentObjectCode = GlobalConnectionObjectCode;
	}
	else
	{
		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
		else
			SelectObject(OutputDisplay, EmptyBrush);

		CurrentObjectCode = GlobalConnectionObjectCode2;
	}

	LineColor = GlobalConnectionColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingNetLabels(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == NetLabelObjectCode) && (NetLabelThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((NetLabelPen == (HGDIOBJ) NULL) || (NetLabelThickness != Thickness))
	{
		NetLabelThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (NetLabelPen == (HGDIOBJ) NULL)
		{
			NetLabelPen = CreatePen(PS_SOLID, Thickness, NetLabelColor);
			AddGraphicsObject((HGDIOBJ *) & NetLabelPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, NetLabelColor);
			ChangeGraphicObject(NetLabelPen, NewPen);
			NetLabelPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, NetLabelPen);
	else
		SelectObject(OutputDisplay, NetLabelPen);

	CurrentObjectCode = NetLabelObjectCode;
	LineColor = NetLabelColor;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolPins()
{
	if (BackGroundActive)
	{
		InitDrawingBackGround(0, 0);
		return;
	}

	if (SymbolPinPen == (HGDIOBJ) NULL)
	{
		SymbolPinPen = CreatePen(PS_SOLID, 1, SymbolPinColor);
		AddGraphicsObject((HGDIOBJ *) & SymbolPinPen);
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolPinPen);
	else
		SelectObject(OutputDisplay, SymbolPinPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, SymbolPinBrush);
	else
		SelectObject(OutputDisplay, SymbolPinBrush);

	CurrentObjectCode = SymbolPinObjectCode;
	LineColor = SymbolPinColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolPinBusses()
{
	if (BackGroundActive)
	{
		InitDrawingBackGround(0, 0);
		return;
	}

	if (SymbolPinBusPen == (HGDIOBJ) NULL)
	{
		SymbolPinBusPen = CreatePen(PS_SOLID, 1, SymbolPinBusColor);
		AddGraphicsObject((HGDIOBJ *) & SymbolPinBusPen);
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolPinBusPen);
	else
		SelectObject(OutputDisplay, SymbolPinBusPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, SymbolPinBusBrush);
	else
		SelectObject(OutputDisplay, SymbolPinBusBrush);

	CurrentObjectCode = SymbolPinBusObjectCode;
	LineColor = SymbolPinBusColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolPowerPinTexts(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolPowerPinTextObjectCode) && (SymbolPowerPinTextThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolPowerPinTextPen == (HGDIOBJ) NULL) || (SymbolPowerPinTextThickness != Thickness))
	{
		SymbolPowerPinTextThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolPowerPinTextPen == (HGDIOBJ) NULL)
		{
			SymbolPowerPinTextPen = CreatePen(PS_SOLID, Thickness, SymbolPowerPinTextColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolPowerPinTextPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolPowerPinTextColor);
			ChangeGraphicObject(SymbolPowerPinTextPen, NewPen);
			SymbolPowerPinTextPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolPowerPinTextPen);
	else
		SelectObject(OutputDisplay, SymbolPowerPinTextPen);

	CurrentObjectCode = SymbolPowerPinTextObjectCode;
	LineColor = SymbolPowerPinTextColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolPinBusTexts(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolPinBusTextObjectCode) && (SymbolPinBusTextThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolPinBusTextPen == (HGDIOBJ) NULL) || (SymbolPinBusTextThickness != Thickness))
	{
		SymbolPinBusTextThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolPinBusTextPen == (HGDIOBJ) NULL)
		{
			SymbolPinBusTextPen = CreatePen(PS_SOLID, Thickness, SymbolPinBusTextColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolPinBusTextPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolPinBusTextColor);
			ChangeGraphicObject(SymbolPinBusTextPen, NewPen);
			SymbolPinBusTextPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolPinBusTextPen);
	else
		SelectObject(OutputDisplay, SymbolPinBusTextPen);

	CurrentObjectCode = SymbolPinBusTextObjectCode;
	LineColor = SymbolPinBusTextColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolPinTexts(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolPinTextObjectCode) && (SymbolPinTextThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolPinTextPen == (HGDIOBJ) NULL) || (SymbolPinTextThickness != Thickness))
	{
		SymbolPinTextThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolPinTextPen == (HGDIOBJ) NULL)
		{
			SymbolPinTextPen = CreatePen(PS_SOLID, Thickness, SymbolPinTextColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolPinTextPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolPinTextColor);
			ChangeGraphicObject(SymbolPinTextPen, NewPen);
			SymbolPinTextPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolPinTextPen);
	else
		SelectObject(OutputDisplay, SymbolPinTextPen);

	CurrentObjectCode = SymbolPinTextObjectCode;
	LineColor = SymbolPinTextColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolLines(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolLinesObjectCode) && (SymbolLinesThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolLinesPen == (HGDIOBJ) NULL) || (SymbolLinesThickness != Thickness))
	{
		SymbolLinesThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolLinesPen == (HGDIOBJ) NULL)
		{
			SymbolLinesPen = CreatePen(PS_SOLID, Thickness, SymbolLinesColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolLinesPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolLinesColor);
			ChangeGraphicObject(SymbolLinesPen, NewPen);
			SymbolLinesPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolLinesPen);
	else
		SelectObject(OutputDisplay, SymbolLinesPen);

	CurrentObjectCode = SymbolLinesObjectCode;
	LineColor = SymbolLinesColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolRects(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolRectsObjectCode) && (SymbolRectsThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolRectsPen == (HGDIOBJ) NULL) || (SymbolRectsThickness != Thickness))
	{
		SymbolRectsThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolRectsPen == (HGDIOBJ) NULL)
		{
			SymbolRectsPen = CreatePen(PS_SOLID, Thickness, SymbolRectsColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolRectsPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolRectsColor);
			ChangeGraphicObject(SymbolRectsPen, NewPen);
			SymbolRectsPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolRectsPen);
	else
		SelectObject(OutputDisplay, SymbolRectsPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = SymbolRectsObjectCode;
	LineColor = SymbolRectsColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolCircles(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolCirclesObjectCode) && (SymbolCirclesThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolCirclesPen == (HGDIOBJ) NULL) || (SymbolCirclesThickness != Thickness))
	{
		SymbolCirclesThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolCirclesPen == (HGDIOBJ) NULL)
		{
			SymbolCirclesPen = CreatePen(PS_SOLID, Thickness, SymbolCirclesColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolCirclesPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolCirclesColor);
			ChangeGraphicObject(SymbolCirclesPen, NewPen);
			SymbolCirclesPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolCirclesPen);
	else
		SelectObject(OutputDisplay, SymbolCirclesPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = SymbolCirclesObjectCode;
	LineColor = SymbolCirclesColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolArcs(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolArcsObjectCode) && (SymbolArcsThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolArcsPen == (HGDIOBJ) NULL) || (SymbolArcsThickness != Thickness))
	{
		SymbolArcsThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolArcsPen == (HGDIOBJ) NULL)
		{
			SymbolArcsPen = CreatePen(PS_SOLID, Thickness, SymbolArcsColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolArcsPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolArcsColor);
			ChangeGraphicObject(SymbolArcsPen, NewPen);
			SymbolArcsPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolArcsPen);
	else
		SelectObject(OutputDisplay, SymbolArcsPen);

	CurrentObjectCode = SymbolArcsObjectCode;
	LineColor = SymbolArcsColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSymbolTexts(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == SymbolTextsObjectCode) && (SymbolTextsThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((SymbolTextsPen == (HGDIOBJ) NULL) || (SymbolTextsThickness != Thickness))
	{
		SymbolTextsThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (SymbolTextsPen == (HGDIOBJ) NULL)
		{
			SymbolTextsPen = CreatePen(PS_SOLID, Thickness, SymbolTextsColor);
			AddGraphicsObject((HGDIOBJ *) & SymbolTextsPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, SymbolTextsColor);
			ChangeGraphicObject(SymbolTextsPen, NewPen);
			SymbolTextsPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, SymbolTextsPen);
	else
		SelectObject(OutputDisplay, SymbolTextsPen);

	CurrentObjectCode = SymbolTextsObjectCode;
	LineColor = SymbolTextsColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingInstanceRefText(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == InstanceRefTextObjectCode) && (InstanceRefTextThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((InstanceRefTextPen == (HGDIOBJ) NULL) || (InstanceRefTextThickness != Thickness))
	{
		InstanceRefTextThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (InstanceRefTextPen == (HGDIOBJ) NULL)
		{
			InstanceRefTextPen = CreatePen(PS_SOLID, Thickness, InstanceRefTextColor);
			AddGraphicsObject((HGDIOBJ *) & InstanceRefTextPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, InstanceRefTextColor);
			ChangeGraphicObject(InstanceRefTextPen, NewPen);
			InstanceRefTextPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, InstanceRefTextPen);
	else
		SelectObject(OutputDisplay, InstanceRefTextPen);

	CurrentObjectCode = InstanceRefTextObjectCode;
	LineColor = InstanceRefTextColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingInstanceValueText(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == InstanceValueTextObjectCode) && (InstanceValueTextThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((InstanceValueTextPen == (HGDIOBJ) NULL) || (InstanceValueTextThickness != Thickness))
	{
		InstanceValueTextThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (InstanceValueTextPen == (HGDIOBJ) NULL)
		{
			InstanceValueTextPen = CreatePen(PS_SOLID, Thickness, InstanceValueTextColor);
			AddGraphicsObject((HGDIOBJ *) & InstanceValueTextPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, InstanceValueTextColor);
			ChangeGraphicObject(InstanceValueTextPen, NewPen);
			InstanceValueTextPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, InstanceValueTextPen);
	else
		SelectObject(OutputDisplay, InstanceValueTextPen);

	CurrentObjectCode = InstanceValueTextObjectCode;
	LineColor = InstanceValueTextColor;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingObjectLines(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == ObjectLinesObjectCode) && (ObjectLinesThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((ObjectLinesPen == (HGDIOBJ) NULL) || (ObjectLinesThickness != Thickness))
	{
		ObjectLinesThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (ObjectLinesPen == (HGDIOBJ) NULL)
		{
			ObjectLinesPen = CreatePen(PS_SOLID, Thickness, ObjectLinesColor);
			AddGraphicsObject((HGDIOBJ *) & ObjectLinesPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, ObjectLinesColor);
			ChangeGraphicObject(ObjectLinesPen, NewPen);
			ObjectLinesPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, ObjectLinesPen);
	else
		SelectObject(OutputDisplay, ObjectLinesPen);

	CurrentObjectCode = ObjectLinesObjectCode;
	LineColor = ObjectLinesColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingObjectRects(int32 Thickness, int32 Filled)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		if (Filled)
			InitDrawingBackGround(0, 0);
		else
			InitDrawingBackGround(1, Thickness);

		return;
	}

	if (Filled)
	{
		if (CurrentObjectCode == ObjectRectsObjectCode2)
			return;

		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, SpecialObjectsBrush);
		else
			SelectObject(OutputDisplay, SpecialObjectsBrush);

		CurrentObjectCode = ObjectRectsObjectCode2;
	}
	else
	{
		OldThickness = Thickness;

		if ((CurrentObjectCode == ObjectRectsObjectCode) && (ObjectRectsThickness == Thickness))
			return;

		if (Thickness == 0)
			Thickness = 1;

		if ((ObjectRectsPen == (HGDIOBJ) NULL) || (ObjectRectsThickness != Thickness))
		{
			ObjectRectsThickness = Thickness;
			Thickness = (Thickness - 1) | 1;

			if (ObjectRectsPen == (HGDIOBJ) NULL)
			{
				ObjectRectsPen = CreatePen(PS_SOLID, Thickness, ObjectRectsColor);
				AddGraphicsObject((HGDIOBJ *) & ObjectRectsPen);
			}
			else
			{
				NewPen = CreatePen(PS_SOLID, Thickness, ObjectRectsColor);
				ChangeGraphicObject(ObjectRectsPen, NewPen);
				ObjectRectsPen = NewPen;
			}
		}

		if (SavePen == (HGDIOBJ) - 1)
			SavePen = SelectObject(OutputDisplay, ObjectRectsPen);
		else
			SelectObject(OutputDisplay, ObjectRectsPen);

		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
		else
			SelectObject(OutputDisplay, EmptyBrush);

		CurrentObjectCode = ObjectRectsObjectCode;
	}

	LineColor = ObjectRectsColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingObjectCircles(int32 Thickness, int32 Filled)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		if (Filled)
			InitDrawingBackGround(0, 0);
		else
			InitDrawingBackGround(1, Thickness);

		return;
	}

	if (Filled)
	{
		if (CurrentObjectCode == ObjectCirclesObjectCode2)
			return;

		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, SpecialObjectsBrush);
		else
			SelectObject(OutputDisplay, SpecialObjectsBrush);

		CurrentObjectCode = ObjectCirclesObjectCode2;
	}
	else
	{
		OldThickness = Thickness;

		if ((CurrentObjectCode == ObjectCirclesObjectCode) && (ObjectCirclesThickness == Thickness))
			return;

		if (Thickness == 0)
			Thickness = 1;

		if ((ObjectCirclesPen == (HGDIOBJ) NULL) || (ObjectCirclesThickness != Thickness))
		{
			ObjectCirclesThickness = Thickness;
			Thickness = (Thickness - 1) | 1;

			if (ObjectCirclesPen == (HGDIOBJ) NULL)
			{
				ObjectCirclesPen = CreatePen(PS_SOLID, Thickness, ObjectCirclesColor);
				AddGraphicsObject((HGDIOBJ *) & ObjectCirclesPen);
			}
			else
			{
				NewPen = CreatePen(PS_SOLID, Thickness, ObjectCirclesColor);
				ChangeGraphicObject(ObjectCirclesPen, NewPen);
				ObjectCirclesPen = NewPen;
			}
		}

		if (SavePen == (HGDIOBJ) - 1)
			SavePen = SelectObject(OutputDisplay, ObjectCirclesPen);
		else
			SelectObject(OutputDisplay, ObjectCirclesPen);

		if (SaveBrush == (HGDIOBJ) - 1)
			SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
		else
			SelectObject(OutputDisplay, EmptyBrush);

		CurrentObjectCode = ObjectCirclesObjectCode;
	}

	LineColor = ObjectCirclesColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingObjectArcs(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == ObjectArcsObjectCode) && (ObjectArcsThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((ObjectArcsPen == (HGDIOBJ) NULL) || (ObjectArcsThickness != Thickness))
	{
		ObjectArcsThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (ObjectArcsPen == (HGDIOBJ) NULL)
		{
			ObjectArcsPen = CreatePen(PS_SOLID, Thickness, ObjectArcsColor);
			AddGraphicsObject((HGDIOBJ *) & ObjectArcsPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, ObjectArcsColor);
			ChangeGraphicObject(ObjectArcsPen, NewPen);
			ObjectArcsPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, ObjectArcsPen);
	else
		SelectObject(OutputDisplay, ObjectArcsPen);

	CurrentObjectCode = ObjectArcsObjectCode;
	LineColor = ObjectArcsColor;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingObjectTexts(int32 Thickness)
{
	int32 OldThickness;
	HGDIOBJ NewPen;

	if (BackGroundActive)
	{
		InitDrawingBackGround(1, Thickness);
		return;
	}

	OldThickness = Thickness;

	if ((CurrentObjectCode == ObjectTextsObjectCode) && (ObjectTextsThickness == Thickness))
		return;

	if (Thickness == 0)
		Thickness = 1;

	if ((ObjectTextsPen == (HGDIOBJ) NULL) || (ObjectTextsThickness != Thickness))
	{
		ObjectTextsThickness = Thickness;
		Thickness = (Thickness - 1) | 1;

		if (ObjectTextsPen == (HGDIOBJ) NULL)
		{
			ObjectTextsPen = CreatePen(PS_SOLID, Thickness, ObjectTextsColor);
			AddGraphicsObject((HGDIOBJ *) & ObjectTextsPen);
		}
		else
		{
			NewPen = CreatePen(PS_SOLID, Thickness, ObjectTextsColor);
			ChangeGraphicObject(ObjectTextsPen, NewPen);
			ObjectTextsPen = NewPen;
		}
	}

	if (SavePen == (HGDIOBJ) - 1)
		SavePen = SelectObject(OutputDisplay, ObjectTextsPen);
	else
		SelectObject(OutputDisplay, ObjectTextsPen);

	if (SaveBrush == (HGDIOBJ) - 1)
		SaveBrush = SelectObject(OutputDisplay, EmptyBrush);
	else
		SelectObject(OutputDisplay, EmptyBrush);

	CurrentObjectCode = ObjectTextsObjectCode;
	LineColor = ObjectTextsColor;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingButtonInfo()
{
	if (BackGroundActive)
	{
		InitDrawingBackGround(0, 0);
		return;
	}

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

void SetBackGroundActive(int32 mode)
{
	BackGroundActive = 1;
	CurrentObjectCode = 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingBackGround(int32 mode, int32 ThickNess)
{
	HPEN NewPen;

	if ((mode == 1) && (ThickNess > 0))
	{
		if ((CurrentObjectCode != BackGroundObjectCode2) || (BackGroundPenThickness2 != ThickNess))
		{
			if (BackGroundPen2 == (HGDIOBJ) NULL)
			{
				BackGroundPenThickness2 = ThickNess;
				ThickNess |= 1;
				BackGroundPen2 = CreatePen(PS_SOLID, ThickNess, BackGroundColor);
				AddGraphicsObject((HGDIOBJ *) & BackGroundPen2);
			}
			else
			{
				if (BackGroundPenThickness2 != ThickNess)
				{
					BackGroundPenThickness2 = ThickNess;
					ThickNess |= 1;
					NewPen = CreatePen(PS_SOLID, ThickNess, BackGroundColor);
					ChangeGraphicObject(BackGroundPen2, NewPen);
					BackGroundPen2 = NewPen;
				}
			}

			if (SavePen == (HGDIOBJ) - 1)
				SavePen = SelectObject(OutputDisplay, BackGroundPen2);
			else
				SelectObject(OutputDisplay, BackGroundPen2);

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
			if (SavePen == (HGDIOBJ) - 1)
				SavePen = SelectObject(OutputDisplay, BackGroundPen);
			else
				SelectObject(OutputDisplay, BackGroundPen);

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

void DrawLineWhite(double x1, double y1, double x2, double y2, int32 mode)
{
	if ((mode & 4) == 0)
		StartDrawingEditingWindow();

	if ((mode & 2) == 0)
		InitDrawingColorWhite2();
	else
		InitDrawingColorWhite();

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

void DrawCircleWhite(double x, double y, double dikte, int32 mode)
{
	StartDrawingEditingWindow();
	InitDrawingColorWhite();
	SetROP2(OutputDisplay, R2_XORPEN);

//  DrawLine(Mult(x1-Xoffset),DrawWindowMaxY-Mult(y1-Yoffset)-1,Mult(x2-Xoffset),DrawWindowMaxY-Mult(y2-Yoffset)-1);
	if (mode == 0)
		ellips2(MultX(x), MultY(y), Mult(dikte), Mult(dikte), 255);
	else
		ellips2(MultX(x), MultY(y), (int32) dikte, (int32) dikte, 255);

	ExitDrawing();
	EndDrawingEditingWindow();
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
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void LoadDefaultColors()
{

	SCHColors[BackGroundColorNr] = RGB_Black;
	SCHColors[WireColorNr] = RGB_Orange;
	SCHColors[BusColorNr] = RGB_Blue;
	SCHColors[BusConnectionColorNr] = RGB_Pink;
	SCHColors[GlobalConnectionColorNr] = RGB_LightGray;
	SCHColors[JunctionColorNr] = RGB_Cyan;
	SCHColors[OnePinNetColorNr] = RGB_Cyan;
	SCHColors[NetLabelColorNr] = RGB_Yellow;
	SCHColors[InstanceRefTextColorNr] = RGB_Red;
	SCHColors[InstanceValueTextColorNr] = RGB_LightRed;
	SCHColors[SymbolPinColorNr] = RGB_Cyan;
	SCHColors[SymbolPinBusColorNr] = RGB_DarkCyan;
	SCHColors[SymbolPinTextColorNr] = RGB_Yellow;
	SCHColors[SymbolPowerPinTextColorNr] = RGB_LightGreen;
	SCHColors[SymbolPinBusTextColorNr] = RGB_Orange;
	SCHColors[SymbolLineColorNr] = RGB_Cyan;
	SCHColors[SymbolRectColorNr] = RGB_Cyan;
	SCHColors[SymbolCircleColorNr] = RGB_Cyan;
	SCHColors[SymbolArcColorNr] = RGB_Cyan;
	SCHColors[SymbolTextColorNr] = RGB_Green;
	SCHColors[ObjectLineColorNr] = RGB_Cyan;
	SCHColors[ObjectRectColorNr] = RGB_Cyan;
	SCHColors[ObjectCircleColorNr] = RGB_Cyan;
	SCHColors[ObjectArcColorNr] = RGB_Cyan;
	SCHColors[ObjectTextColorNr] = RGB_Green;
	SCHColors[ButtonInfoColorNr] = RGB_Yellow;
	SCHColors[GridColorNr] = RGB_Gray;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void LoadDefaultColors2()
{

	SCHColors[BackGroundColorNr] = RGB_White;
	SCHColors[WireColorNr] = RGB_Blue;
	SCHColors[BusColorNr] = RGB_LightGray;
	SCHColors[BusConnectionColorNr] = RGB_DarkPink;
	SCHColors[GlobalConnectionColorNr] = RGB_Gray;
	SCHColors[JunctionColorNr] = RGB_DarkGray;
	SCHColors[OnePinNetColorNr] = RGB_DarkGray;
	SCHColors[NetLabelColorNr] = RGB_DarkMagenta;
	SCHColors[InstanceRefTextColorNr] = RGB_Red;
	SCHColors[InstanceValueTextColorNr] = RGB_DarkPink;
	SCHColors[SymbolPinColorNr] = RGB_Cyan;
	SCHColors[SymbolPinBusColorNr] = RGB_DarkCyan;
	SCHColors[SymbolPinTextColorNr] = RGB_DarkCyan;
	SCHColors[SymbolPowerPinTextColorNr] = RGB_Green;
	SCHColors[SymbolPinBusTextColorNr] = RGB_Gray;
	SCHColors[SymbolLineColorNr] = RGB_Red;
	SCHColors[SymbolRectColorNr] = RGB_Red;
	SCHColors[SymbolCircleColorNr] = RGB_Red;
	SCHColors[SymbolArcColorNr] = RGB_Red;
	SCHColors[SymbolTextColorNr] = RGB_DarkGreen;
	SCHColors[ObjectLineColorNr] = RGB_Brown;
	SCHColors[ObjectRectColorNr] = RGB_Brown;
	SCHColors[ObjectCircleColorNr] = RGB_Brown;
	SCHColors[ObjectArcColorNr] = RGB_Brown;
	SCHColors[ObjectTextColorNr] = RGB_DarkGreen;
	SCHColors[ButtonInfoColorNr] = RGB_Yellow;
	SCHColors[GridColorNr] = RGB_Gray;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void GraphicsMain()
{
	LoadDefaultColors();
	SaveBrush = (HGDIOBJ) - 1;
	SavePen = (HGDIOBJ) - 1;
	SaveFont = (HGDIOBJ) - 1;
	WireThickness = -1;
	BusThickness = -1;
	BusConnectionThickness = -1;
	GlobalConnectionThickness = -1;
	NetLabelThickness = -1;
	InstanceRefTextThickness = -1;
	InstanceValueTextThickness = -1;
	SymbolPinTextThickness = -1;
	SymbolPinBusTextThickness = -1;
	SymbolPowerPinTextThickness = -1;
	SymbolLinesThickness = -1;
	SymbolRectsThickness = -1;
	SymbolCirclesThickness = -1;
	SymbolArcsThickness = -1;
	SymbolTextsThickness = -1;
	ObjectLinesThickness = -1;
	ObjectRectsThickness = -1;
	ObjectCirclesThickness = -1;
	ObjectArcsThickness = -1;
	ObjectTextsThickness = -1;

	BackGroundPenThickness2 = -1;
	CurrentObjectCode = -1;
	NrGraphicsObjects = 0;
	PrintingThickness = 1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
