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
#include  "stdio.h"
#include  "memory.h"
#include  "graphics.h"
#include  "calcdef.h"
#include  "line2.h"
#include  "resource.h"
#include  "ellipss.h"
#include  "brushes.h"



#define SWAP_RGB(color) ((((color) >> 16) & 0xFF) + \
                        ((((color) >> 8) & 0xFF) << 8) + \
                        (((color) & 0xFF) << 16))

COLORREF PCBColors[192];
COLORREF PCBColors2[192];

COLORREF LineColor;


HGDIOBJ SaveBrush;
HPEN SavePen;
HFONT PinTextFont, SaveFont;

COLORREF GraphicsObjectColor[192];
COLORREF GraphicsObjectColor2[192];
LOGBRUSH GraphicsObjectBrushObject[192];
HBRUSH GraphicsObjectBrush[192], EmptyBrush;
HPEN GraphicsObjectPen[192], EmptyPen;
int32 GraphicsObjectCodes[192];
int32 GraphicsObjectPenThickness[192];
int32 PCBObjectCodes[192];
int32 ok;
BITMAPINFO *BitmapBrushInfo[192];
uint8 BitmapBrushMem[192][256];

HDC OutputDisplay;

HGDIOBJ *GraphicsObjects[600];

int32 NrGraphicsObjects, SelectColorMode;

HGDIOBJ SelectionBrushObject;
LOGBRUSH SelectionBrush;


LayerObjectCodesRecord LayerObjectCodes[NrLayerObjects] = {
	ViewLayer1ObjectNr, 0, "ViewLayer1",
	ViewLayer2ObjectNr, 1, "ViewLayer2",
	ViewLayer3ObjectNr, 2, "ViewLayer3",
	ViewLayer4ObjectNr, 3, "ViewLayer4",
	ViewLayer5ObjectNr, 4, "ViewLayer5",
	ViewLayer6ObjectNr, 5, "ViewLayer6",
	ViewLayer7ObjectNr, 6, "ViewLayer7",
	ViewLayer8ObjectNr, 7, "ViewLayer8",
	ViewLayer9ObjectNr, 8, "",
	ViewLayer10ObjectNr, 9, "",
	ViewLayer11ObjectNr, 10, "",
	ViewLayer12ObjectNr, 11, "",
	ViewLayer13ObjectNr, 12, "",
	ViewLayer14ObjectNr, 13, "",
	ViewLayer15ObjectNr, 14, "",
	ViewLayer16ObjectNr, 15, "",
	ViewLayer1HilitedObjectNr, 0, "ViewLayer1Hilited",
	ViewLayer2HilitedObjectNr, 1, "ViewLayer2Hilited",
	ViewLayer3HilitedObjectNr, 2, "ViewLayer3Hilited",
	ViewLayer4HilitedObjectNr, 3, "ViewLayer4Hilited",
	ViewLayer5HilitedObjectNr, 4, "ViewLayer5Hilited",
	ViewLayer6HilitedObjectNr, 5, "ViewLayer6Hilited",
	ViewLayer7HilitedObjectNr, 6, "ViewLayer7Hilited",
	ViewLayer8HilitedObjectNr, 7, "ViewLayer8Hilited",
	ViewLayer9HilitedObjectNr, 8, "",
	ViewLayer10HilitedObjectNr, 9, "",
	ViewLayer11HilitedObjectNr, 10, "",
	ViewLayer12HilitedObjectNr, 11, "",
	ViewLayer13HilitedObjectNr, 12, "",
	ViewLayer14HilitedObjectNr, 13, "",
	ViewLayer15HilitedObjectNr, 14, "",
	ViewLayer16HilitedObjectNr, 15, "",
	ViewLayer1InNetObjectNr, 0, "ViewLayer1InNet",
	ViewLayer2InNetObjectNr, 1, "ViewLayer2InNet",
	ViewLayer3InNetObjectNr, 2, "ViewLayer3InNet",
	ViewLayer4InNetObjectNr, 3, "ViewLayer4InNet",
	ViewLayer5InNetObjectNr, 4, "ViewLayer5InNet",
	ViewLayer6InNetObjectNr, 5, "ViewLayer6InNet",
	ViewLayer7InNetObjectNr, 6, "ViewLayer7InNet",
	ViewLayer8InNetObjectNr, 7, "ViewLayer8InNet",
	ViewLayer9InNetObjectNr, 8, "",
	ViewLayer10InNetObjectNr, 9, "",
	ViewLayer11InNetObjectNr, 10, "",
	ViewLayer12InNetObjectNr, 11, "",
	ViewLayer13InNetObjectNr, 12, "",
	ViewLayer14InNetObjectNr, 13, "",
	ViewLayer15InNetObjectNr, 14, "",
	ViewLayer16InNetObjectNr, 15, "",
	ViewLayer1HilitedInNetObjectNr, 0, "ViewLayer1HilitedInNet",
	ViewLayer2HilitedInNetObjectNr, 1, "ViewLayer2HilitedInNet",
	ViewLayer3HilitedInNetObjectNr, 2, "ViewLayer3HilitedInNet",
	ViewLayer4HilitedInNetObjectNr, 3, "ViewLayer4HilitedInNet",
	ViewLayer5HilitedInNetObjectNr, 4, "ViewLayer5HilitedInNet",
	ViewLayer6HilitedInNetObjectNr, 5, "ViewLayer6HilitedInNet",
	ViewLayer7HilitedInNetObjectNr, 6, "ViewLayer7HilitedInNet",
	ViewLayer8HilitedInNetObjectNr, 7, "ViewLayer8HilitedInNet",
	ViewLayer9HilitedInNetObjectNr, 8, "",
	ViewLayer10HilitedInNetObjectNr, 9, "",
	ViewLayer11HilitedInNetObjectNr, 10, "",
	ViewLayer12HilitedInNetObjectNr, 11, "",
	ViewLayer13HilitedInNetObjectNr, 12, "",
	ViewLayer14HilitedInNetObjectNr, 13, "",
	ViewLayer15HilitedInNetObjectNr, 14, "",
	ViewLayer16HilitedInNetObjectNr, 15, "",
	ConnectionsObjectNr, CONNECTIONS_LAYER, "Connections",
	ConnectionsHilitedObjectNr, CONNECTIONS_LAYER, "ConnectionsHilited",
	NetPinsObjectNr, NET_PINS_LAYER, "NetPins",
	NetPinsObject2Nr, NET_PINS_LAYER2, "NetPins2",
	SilkScreenTopObjectNr, SILKSCREEN_TOP, "SilkScreenTop",
	SilkScreenBottomObjectNr, SILKSCREEN_BOTTOM, "SilkScreenBottom",
	ReferenceObjectNr, COMP_REF_LAYER, "Reference",
	CompValueObjectNr, COMP_VALUE_LAYER, "CompValue",
	ShapePlacementOutLineTopObjectNr, PLACEMENT_OUTLINE_TOP, "ShapePlacementOutLineTop",
	ShapePlacementOutLineBottomObjectNr, PLACEMENT_OUTLINE_BOTTOM, "ShapePlacementOutLineBottom",
	ShapeCompOutLineTopObjectNr, COMP_OUTLINE_LAYER_TOP, "ShapeCompOutLineTop",
	ShapeCompOutLineBottomObjectNr, COMP_OUTLINE_LAYER_BOTTOM, "ShapeCompOutLineBottom",
	ShapePinsTopObjectNr, SHAPE_PINS_TOP, "ShapePinsTop",
	ShapePinsBottomObjectNr, SHAPE_PINS_BOTTOM, "ShapePinsBottom",
	ShapePinsInnerObjectNr, SHAPE_PINS_INNER, "ShapePinsInner",
	ShapePinsTopHilitedObjectNr, SHAPE_PINS_TOP, "ShapePinsTopHilited",
	ShapePinsBottomHilitedObjectNr, SHAPE_PINS_BOTTOM, "ShapePinsBottomHilited",
	ShapePinsInnerHilitedObjectNr, SHAPE_PINS_INNER, "ShapePinsInnerHilited",
	ShapePinsDrillObjectNr, DRILL_LAYER, "ShapePinsDrill",
	ShapePinsDrillUnplatedObjectNr, DRILL_UNPLATED_LAYER, "ShapePinsDrillUnplated",
	ViaPinsObjectNr, VIA_LAYER, "ViaPins",
	ViaPinsHilitedObjectNr, VIA_LAYER, "ViaPinsHilited",
	ViaPinsInNetObjectNr, VIA_LAYER, "ViaPinsInNet",
	ViaPinsHilitedInNetObjectNr, VIA_LAYER, "ViaPinsHilitedInNet",
	ViaPinsDrillObjectNr, VIA_DRILL_LAYER, "ViaPinsDrill",
	ObjectsInfoObjectNr, INFO_LAYER, "ObjectsInfo",
	ClearanceObjectNr, CLEARANCE_LAYER, "Clearance",
	StippelObjectNr, 0, "",
	PolylineObjectNr, POLYLINE_LAYER, "DrawPolyline",
	ErrorObjectNr, ERROR_LAYER, "Error",
	WarningObjectNr, WARNING_LAYER, "Warning",
	GridObjectNr, GRID_LAYER, "Grid",
	ButtonInfoObjectNr, BUTTON_INFO_LAYER, "ButtonInfo",
	BackGroundObjectNr, BACKGROUND_LAYER, "BackGround",
	BoardOutlineObjectNr, BOARD_OUTLINE_LAYER, "BoardOutline",
	SwappablePinsGateObjectNr, SWAP_PINS_LAYER, "SwappablePinsGate",
	SwappableGatePinsObjectNr, SWAP_GATE_LAYER, "SwappableGatePins",
	PasteMaskTopObjectNr, PASTE_MASK_TOP, "PasteMaskTop",
	PasteMaskBottomObjectNr, PASTE_MASK_BOTTOM, "PasteMasBottom",
	SoldMaskTopObjectNr, SOLD_MASK_TOP, "SoldMaskTop",
	SoldMaskBottomObjectNr, SOLD_MASK_BOTTOM, "SoldMaskBottom",
	ObjectsInfo2ObjectNr, INFO_LAYER2, "ObjectsInfo2",
	ObjectsInfo3ObjectNr, INFO_LAYER3, "ObjectsInfo3",
	ObjectsInfo4ObjectNr, INFO_LAYER4, "ObjectsInfo4",
	UnconnectedPadsTopObjectNr, UNCONNECTED_PADS_TOP_LAYER, "UnconnectedPadsTop",
	UnconnectedPadsBottomObjectNr, UNCONNECTED_PADS_BOTTOM_LAYER, "UnconnectedPadsBottom",
	UnconnectedPadsInnerObjectNr, UNCONNECTED_PADS_INNER_LAYER, "UnconnectedPadsInner",
	RoutingKeepoutTopObjectNr, ROUTING_KEEPOUT_TOP, "RoutingKeepoutTop",
	RoutingKeepoutBottomObjectNr, ROUTING_KEEPOUT_BOTTOM, "RoutingKeepoutBottom",
	RoutingKeepoutInnerObjectNr, ROUTING_KEEPOUT_INNER, "RoutingKeepoutInner",
	BoardOutlineKeepOutObjectNr, BOARD_OUTLINE_KEEPOUT_LAYER, "BoardOutlineKeepout",
	CrossHairObjectNr, CROSS_HAIR_LAYER, "CrossHair",

};

extern double Factor, Xoffset, Yoffset;
extern int32 Printing, BackGroundActive;
extern int32 DrawLayerCode[32], DrawCode, DrawDrillMode;
extern DesignRecord Design;
extern int Debugfp, OperatingSystem;
extern HWND PCBWindow;

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


void MakeHiliteColor(int32 r, int32 g, int32 b, int32 * r2, int32 * g2, int32 * b2)
{
	*r2 = min(255, (int32) ((float) r * 1.4));
	*g2 = min(255, (int32) ((float) g * 1.4));
	*b2 = min(255, (int32) ((float) b * 1.4));
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


int32 MakeHiliteColor2(int32 rgb)
{
	int32 r, g, b, r2, g2, b2;

	r = (rgb >> 16) & 0xff;
	g = (rgb >> 8) & 0xff;
	b = rgb & 0xff;
	/*
	  r2=min(255,(int32)((float)r*1.6));
	  g2=min(255,(int32)((float)g*1.6));
	  b2=min(255,(int32)((float)b*1.6));
	*/
	r2 = min(255, r + ((255 - r) / 2));
	g2 = min(255, g + ((255 - g) / 2));
	b2 = min(255, b + ((255 - b) / 2));
	/*
	  if (r<50) {
	    r2=min(255,r+120);
	  } else {
	    if (r<170) {
	      r2=min(255,r+60);
	    } else {
	      r2=min(255,r+40);
	    }
	  }
	  if (g<50) {
	    g2=min(255,g+120);
	  } else {
	    if (r<170) {
	      g2=min(255,g+60);
	    } else {
	      g2=min(255,g+40);
	    }
	  }
	  if (b<50) {
	    b2=min(255,b+120);
	  } else {
	    if (b<170) {
	      b2=min(255,b+60);
	    } else {
	      b2=min(255,b+40);
	    }
	  }
	*/
	return ((r2 << 16) + (g2 << 8) + b2);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DeleteGraphicObjects()
{
	int32 cnt;

	for (cnt = 0; cnt < NrGraphicsObjects; cnt++)
	{
		DeleteObject(*(GraphicsObjects[cnt]));
		*(GraphicsObjects[cnt]) = (HGDIOBJ) NULL;
	}

	NrGraphicsObjects = 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void AddGraphicsObject(HGDIOBJ * Object)
{
	char MessageStr[MAX_LENGTH_STRING];

	GraphicsObjects[NrGraphicsObjects] = Object;
	NrGraphicsObjects++;

	if (NrGraphicsObjects > 600)
	{
		sprintf(MessageStr, "Graphics error 1");
		MessageBoxOwn(NULL, MessageStr, SC(1, "Message"), MB_APPLMODAL | MB_OK);
		ok = 1;
	}
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void ChangeGraphicObject(HGDIOBJ * OldObject, HGDIOBJ NewObject)
{
	int32 cnt, Found = 0;

	HGDIOBJ TestObject;
	cnt = 0;

	while ((cnt < NrGraphicsObjects) && (!Found))
	{
		TestObject = GraphicsObjects[cnt];

		if (*OldObject == *(GraphicsObjects[cnt]))
			Found = 1;
//    if (OldObject==GraphicsObjects[cnt]) Found=1;
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

void FillBrushOnColorObjectNr(LOGBRUSH * Brush, int32 ObjectColorNr)
{
	memmove(Brush, &GraphicsObjectBrushObject[ObjectColorNr], sizeof(LOGBRUSH));
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CreateNewPen(HPEN * NewPen, int32 ThickNess, COLORREF Color, int32 mode)
{
	switch (mode & 0x0f)
	{
	case 0:
		*NewPen = CreatePen(PS_SOLID, ThickNess, Color);
		break;

	case 1:
		*NewPen = CreatePen(PS_DASHDOT, ThickNess, Color);
		break;

	case 2:
		*NewPen = CreatePen(PS_DOT, ThickNess, Color);
		break;

	case 7:
		*NewPen = CreatePen(PS_NULL, 0, 0);
		break;
	}

	if ((mode & 0x1000) == 0)
		AddGraphicsObject((HGDIOBJ *) NewPen);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CreateNewBrush(HBRUSH * NewBrush, int32 GraphicsObjectNr, int32 mode)
{
	typedef uint8 ByteArray[1000];
	BITMAPV4HEADER *BitmapHeader;
	COLORREF *BitmapColors;
	uint64 MemPos;
	uint8 *BitmapBits;
	LOGBRUSH *BrushObject;

#ifdef _DEBUG

	if (GraphicsObjectNr == UnconnectedPadsTopObjectNr)
		ok = 1;

	if (GraphicsObjectNr == 33)
		ok = 1;

	if (mode == 0x10)
		ok = 1;

#endif

	switch (mode & 0x1FF)
	{
	case 0:
	case 0x100:
		BrushObject = &GraphicsObjectBrushObject[GraphicsObjectNr];

		if ((mode & 0x1FF) == 0)
		{
			BrushObject->lbColor = GraphicsObjectColor[GraphicsObjectNr];
			BrushObject->lbHatch = (LONG) NULL;
			BrushObject->lbStyle = BS_SOLID;
		}
		else
		{
			BrushObject->lbColor = (LONG) NULL;
			BrushObject->lbHatch = (LONG) NULL;
			BrushObject->lbStyle = BS_NULL;
		}

		*NewBrush = CreateBrushIndirect(BrushObject);

		if ((mode & 0x1000) == 0)
			AddGraphicsObject((HGDIOBJ *) NewBrush);

		return;
	}

	if (OperatingSystem == VER_PLATFORM_WIN32_NT)
	{
		BitmapHeader = (BITMAPV4HEADER *) & BitmapBrushInfo[GraphicsObjectNr]->bmiHeader;
		MemPos = (uint64) BitmapBrushInfo[GraphicsObjectNr];
		MemPos += BitmapHeader->bV4Size;
		BitmapColors = (COLORREF *) MemPos;
		BitmapBits = (uint8 *) & BitmapColors[2];

		if ((uint32) GraphicsObjectColor2[GraphicsObjectNr] == 0x01000000)
			BitmapColors[0] = GraphicsObjectColor[BackGroundObjectNr];
		else
			BitmapColors[0] = SWAP_RGB(GraphicsObjectColor2[GraphicsObjectNr]);

		BitmapColors[1] = SWAP_RGB(GraphicsObjectColor[GraphicsObjectNr]);
		memmove(BitmapBits, &SpecialBrushBitmapBytes[(mode & 0x1F) - 1], 64);
		*NewBrush = CreateDIBPatternBrushPt(BitmapBrushInfo[GraphicsObjectNr], DIB_RGB_COLORS);
	}
	else
	{
		BrushObject = &GraphicsObjectBrushObject[GraphicsObjectNr];
		BrushObject->lbColor = GraphicsObjectColor[GraphicsObjectNr];
		BrushObject->lbStyle = BS_HATCHED;

		switch (mode & 0x1FF)
		{
		case 9:
			BrushObject->lbHatch = (LONG) HS_BDIAGONAL;
			break;

		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 11:
		case 12:
			BrushObject->lbHatch = (LONG) HS_CROSS;
			break;

		case 3:
		case 13:
		case 14:
		case 15:
			BrushObject->lbHatch = (LONG) HS_DIAGCROSS;
			break;

		case 10:
			BrushObject->lbHatch = (LONG) HS_FDIAGONAL;
			break;

		case 2:
			BrushObject->lbHatch = (LONG) HS_HORIZONTAL;
			break;

		case 1:
		default:
			BrushObject->lbHatch = (LONG) HS_VERTICAL;
			break;
		}

		*NewBrush = CreateBrushIndirect(BrushObject);
	}


	if ((mode & 0x1000) == 0)
		AddGraphicsObject((HGDIOBJ *) NewBrush);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void GetGraphicsPenBrush(int32 mode)
{
	HGDIOBJ CurrentBrush, CurrentPen;
	uint8 ObjectBrushBuf[1024];
	uint8 ObjectPenBuf[1024];
	int32 res, ok;
	LOGBRUSH *CurrentBrushObject;
	LOGPEN *CurrentPenObject;

	CurrentPen = GetCurrentObject(OutputDisplay, OBJ_PEN);
	res = GetObject(CurrentPen, 1024, &ObjectPenBuf);
	CurrentPenObject = (LOGPEN *) & ObjectPenBuf;

	CurrentBrush = GetCurrentObject(OutputDisplay, OBJ_BRUSH);
	res = GetObject(CurrentBrush, 1024, &ObjectBrushBuf);
	CurrentBrushObject = (LOGBRUSH *) & ObjectBrushBuf;
	ok = 1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void SetNewGraphicsMode(int32 GraphicsObjectNr, int32 ThickNess, int32 BrushMode, int32 PenMode)
{
	HPEN NewPen;
	int32 CurrentThickness, CurrentGraphicsObjectNr, NewColor2, NewPenMode, NewBrushMode, GraphicsPenObjectNr,
	      GraphicsBrushObjectNr, NewGraphicsObjectCode, CurrentGraphicsObjectCode, res, ChangePen, ChangeBrush, ModifyPen,
	      ModifyBrush;
#ifdef _DEBUG
	LOGPEN *CurrentPenObject;
	LOGBRUSH *CurrentBrushObject;
	HGDIOBJ CurrentBrush, CurrentPen;
	uint8 ObjectBrushBuf[1024];
	uint8 ObjectPenBuf[1024];
#endif

	NewPen = NULL;
#ifdef _DEBUG

	if (GraphicsObjectNr == BackgroundDashPenObjectNr)
		ok = 1;

#endif

	if (CurrentObjectCode == 0)
		CurrentObjectCode = (uint32) - 1;

	CurrentGraphicsObjectCode = GraphicsObjectCodes[GraphicsObjectNr];
	NewGraphicsObjectCode = GraphicsObjectNr | ((PenMode << 20) | (BrushMode << 16));

	CurrentGraphicsObjectNr = CurrentObjectCode & 0xFF;
//  CurrentThickness=CurrentGraphicsObjectCode & 0xFFF;
	CurrentThickness = GraphicsObjectPenThickness[GraphicsObjectNr];

	if (ThickNess < 1)
		ThickNess = 1;

	NewPenMode = 0;
	NewBrushMode = 0;
	NewColor2 = 0;
	ChangePen = 0;
	ChangeBrush = 0;
	ModifyPen = 0;
	ModifyBrush = 0;
	GraphicsPenObjectNr = GraphicsObjectNr;

	if ((GraphicsObjectNr == CurrentGraphicsObjectNr) && (CurrentObjectCode == NewGraphicsObjectCode)
	        && (CurrentThickness == ThickNess))
		return;

	CurrentGraphicsObjectCode &= ~(0x010000FF);
	CurrentGraphicsObjectCode |= GraphicsObjectNr;

// ****************************************************************************************************
// ****************************************************************************************************

	if (GraphicsObjectNr != CurrentGraphicsObjectNr)
		ChangePen = 1;

	switch (PenMode)
	{
	case 1:					// No pen
		GraphicsPenObjectNr = EmptyPenObjectNr;
		break;

	case 3:					// White pen
		GraphicsPenObjectNr = WhitePenObjectNr;
		break;
	}

	if ((((CurrentGraphicsObjectCode >> 20) & 0x0F) != PenMode) || (CurrentThickness != ThickNess)
	        || (GraphicsObjectPen[GraphicsPenObjectNr] == NULL))
		ModifyPen = 1;

	if (ModifyPen)
	{
		CurrentGraphicsObjectCode &= ~(0x0F << 20);

		switch (PenMode)
		{
		case 0:				// Normal pen
		case 2:				// Dotted pen
			switch (PenMode)
			{
			case 0:			// Normal pen
				NewPenMode = 0;
				break;

			case 2:			// Dotted pen
				NewPenMode = 2;
				break;
			}

			if (GraphicsObjectPen[GraphicsObjectNr] == NULL)
			{	// Create pen for the first time
				CreateNewPen(&GraphicsObjectPen[GraphicsObjectNr], ThickNess, GraphicsObjectColor[GraphicsObjectNr],
				             NewPenMode);
			}
			else
			{	// Change pen
				CreateNewPen(&NewPen, ThickNess, GraphicsObjectColor[GraphicsObjectNr], 0x1000 + NewPenMode);
				ChangeGraphicObject((HGDIOBJ *) & GraphicsObjectPen[GraphicsObjectNr], NewPen);
				GraphicsObjectPen[GraphicsObjectNr] = NewPen;
			}

			GraphicsObjectPenThickness[GraphicsObjectNr] = ThickNess;

			switch (PenMode)
			{
			case 0:			// Normal pen
				break;

			case 2:			// Dotted pen
				CurrentGraphicsObjectCode |= (1 << 20);
				break;
			}

			break;

		case 1:				// No pen
			ThickNess = 1;
			NewPenMode = 7;

			if (GraphicsObjectPen[EmptyPenObjectNr] == NULL)
			{	// Create pen for the first time
				CreateNewPen(&GraphicsObjectPen[EmptyPenObjectNr], ThickNess, GraphicsObjectColor[EmptyPenObjectNr],
				             NewPenMode);
			}

			GraphicsObjectPenThickness[GraphicsObjectNr] = ThickNess;
			CurrentGraphicsObjectCode |= (1 << 20);
			break;

		case 3:				// White pen
			ThickNess = 1;
			NewPenMode = 0;

			if (GraphicsObjectPen[WhitePenObjectNr] == NULL)
			{	// Create pen for the first time
				CreateNewPen(&GraphicsObjectPen[WhitePenObjectNr], ThickNess, RGB_White, NewPenMode);
			}

			GraphicsObjectPenThickness[GraphicsObjectNr] = ThickNess;
			CurrentGraphicsObjectCode |= (3 << 20);
			break;
		}
	}

	if ((ChangePen) || (ModifyPen))
	{
		if (OutputDisplay != NULL)
		{
			if (SavePen == (HGDIOBJ) - 1)
				SavePen = SelectObject(OutputDisplay, GraphicsObjectPen[GraphicsPenObjectNr]);
			else
				SelectObject(OutputDisplay, GraphicsObjectPen[GraphicsPenObjectNr]);
		}
	}

#ifdef _DEBUG
	ok = 1;

	if (0)
	{
//    if (GraphicsObjectNr==ViewLayer2InNetObjectNr) {
		ok = 1;
		CurrentPen = GetCurrentObject(OutputDisplay, OBJ_PEN);
		res = GetObject(CurrentPen, 1024, &ObjectPenBuf);
		CurrentPenObject = (LOGPEN *) & ObjectPenBuf;
//    }
	}

#endif

// ****************************************************************************************************
// ****************************************************************************************************

	GraphicsBrushObjectNr = GraphicsObjectNr;

	if ((GraphicsObjectNr != CurrentGraphicsObjectNr) || (GraphicsObjectNr == ViewLayer2InNetObjectNr))
		ChangeBrush = 1;

	switch (BrushMode)
	{
	case 0:					// Normal brush
		break;

	case 1:					// No brush
		GraphicsBrushObjectNr = EmptyBrushObjectNr;
		break;

	case 2:					// Background brush
		GraphicsBrushObjectNr = BackGroundObjectNr;
		break;
	}

	if ((((CurrentGraphicsObjectCode >> 16) & 0x0F) != BrushMode)
	        || (GraphicsObjectBrush[GraphicsBrushObjectNr] == (HGDIOBJ) NULL))
		ModifyBrush = 1;

#ifdef _DEBUG

	if (GraphicsObjectNr == ViewLayer2InNetObjectNr)
	{
		ok = 1;

		if (BrushMode == 2)
			ok = 1;
	}

#endif

	if (ModifyBrush)
	{
		CurrentGraphicsObjectCode &= ~(0x0F << 16);

		switch (BrushMode)
		{
		case 0:				// Normal brush
		case 2:				// Background brush
			switch (BrushMode)
			{
			case 0:			// Normal brush
				break;

			case 2:			// Background brush
				GraphicsObjectNr = BackGroundObjectNr;
				break;
			}

			switch (BrushMode)
			{
			case 0:			// Normal brush
				NewBrushMode = (CurrentGraphicsObjectCode >> 8) & 0xFF;
				break;

			case 2:			// Background brush
				NewBrushMode = 0;
				break;
			}

			if (GraphicsObjectBrush[GraphicsBrushObjectNr] == (HGDIOBJ) NULL)
				CreateNewBrush(&GraphicsObjectBrush[GraphicsBrushObjectNr], GraphicsBrushObjectNr, NewBrushMode);

			switch (BrushMode)
			{
			case 0:			// Normal brush
				break;

			case 2:			// Background brush
				CurrentGraphicsObjectCode |= 2 << 16;
				break;
			}

			break;

		case 1:				// No brush

//        if ((GraphicsObjectNr==EmptyBrushObjectNr)
//           &&
			if (GraphicsObjectBrush[GraphicsBrushObjectNr] == (HGDIOBJ) NULL)
				CreateNewBrush(&GraphicsObjectBrush[GraphicsBrushObjectNr], GraphicsObjectNr, 0x100);

			CurrentGraphicsObjectCode |= 1 << 16;
			break;
		}
	}

	if ((ChangeBrush) || (ModifyBrush))
	{
		if (OutputDisplay != NULL)
		{
			if (SaveBrush == (HGDIOBJ) - 1)
				SaveBrush = SelectObject(OutputDisplay, GraphicsObjectBrush[GraphicsBrushObjectNr]);
			else
				SelectObject(OutputDisplay, GraphicsObjectBrush[GraphicsBrushObjectNr]);
		}
	}

	GraphicsObjectCodes[GraphicsObjectNr] = CurrentGraphicsObjectCode;

	if (OutputDisplay != NULL)
	{
		LineColor = GraphicsObjectColor[GraphicsPenObjectNr];
		CurrentObjectCode = CurrentGraphicsObjectCode;
	}

#ifdef _DEBUG
	ok = 1;

	if (0)
	{
//  if (GraphicsObjectNr==ViewLayer2InNetObjectNr) {
		ok = 1;
		CurrentBrush = GetCurrentObject(OutputDisplay, OBJ_BRUSH);
		res = GetObject(CurrentBrush, 1024, &ObjectBrushBuf);
		CurrentBrushObject = (LOGBRUSH *) & ObjectBrushBuf;

		if (BrushMode == 2)
			ok = 1;

//    }
	}

#endif
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingEmptyPen()
{
	SetNewGraphicsMode(EmptyPenObjectNr, 0, 0, 1);
}



// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CreateDrawObjects(int32 mode)
{
	typedef uint8 ByteArray[1000];
	int32 cnt;
	BITMAPV4HEADER *BitmapHeader;
	uint32 PenMode, BrushMode;


	memset(&GraphicsObjectColor, 0, sizeof(GraphicsObjectColor));
	memset(&GraphicsObjectBrushObject, 0, sizeof(GraphicsObjectBrushObject));
	memset(&GraphicsObjectBrush, 0, sizeof(GraphicsObjectBrush));
	memset(&GraphicsObjectPen, 0, sizeof(GraphicsObjectPen));
	memset(&BitmapBrushMem, 0, sizeof(BitmapBrushMem));


// ****************************************************************************************************
// ****************************************************************************************************

	for (cnt = 0; cnt < NrLayerObjects; cnt++)
	{
		if (cnt != BackgroundDashPenObjectNr)
		{
			GraphicsObjectColor[cnt] = PCBColors[cnt];
			GraphicsObjectColor2[cnt] = PCBColors2[cnt];
			GraphicsObjectCodes[cnt] = PCBObjectCodes[cnt];
		}
		else
		{
			GraphicsObjectColor[cnt] = PCBColors[BackGroundObjectNr];
			GraphicsObjectColor2[cnt] = PCBColors2[BackGroundObjectNr];
			GraphicsObjectCodes[cnt] = PCBObjectCodes[BackGroundObjectNr];
		}

		BitmapBrushInfo[cnt] = (BITMAPINFO *) & BitmapBrushMem[cnt];
		BitmapHeader = (BITMAPV4HEADER *) & BitmapBrushInfo[cnt]->bmiHeader;
		BitmapHeader->bV4Size = sizeof(BITMAPV4HEADER);
		BitmapHeader->bV4Planes = 1;
		BitmapHeader->bV4BitCount = 1;
		BitmapHeader->bV4V4Compression = BI_RGB;

		if (OperatingSystem == VER_PLATFORM_WIN32_NT)
		{
			BitmapHeader->bV4Width = 16;
			BitmapHeader->bV4Height = 16;
		}
		else
		{
			BitmapHeader->bV4Width = 8;
			BitmapHeader->bV4Height = 8;
		}
	}

	SelectColorMode = R2_WHITE;

	if (PCBColors[BackGroundObjectNr] == RGB_White)
		SelectColorMode = R2_NOTMERGEPEN;

	for (cnt = 0; cnt < NrLayerObjects; cnt++)
	{
#ifdef _DEBUG

		if (cnt == BackgroundDashPenObjectNr)
			ok = 1;

#endif
		PenMode = (GraphicsObjectCodes[cnt] >> 20) & 0xF;
		BrushMode = (GraphicsObjectCodes[cnt] >> 16) & 0xF;

		switch (cnt)
		{
		case WhitePenObjectNr:
			SetNewGraphicsMode(cnt, 1, 1, 3);
			break;

		case EmptyPenObjectNr:
			SetNewGraphicsMode(cnt, 1, 0, 1);
			break;

		case EmptyBrushObjectNr:
			SetNewGraphicsMode(cnt, 1, 1, 0);
			break;

		default:
			SetNewGraphicsMode(cnt, GraphicsObjectPenThickness[cnt], BrushMode, PenMode);
			break;
		}
	}

	EmptyPen = GraphicsObjectPen[EmptyPenObjectNr];
	EmptyBrush = GraphicsObjectBrush[EmptyBrushObjectNr];

	PinTextFont =
	    CreateFont(20, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
	               FIXED_PITCH, "Courier New");

	AddGraphicsObject((HGDIOBJ *) & PinTextFont);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 InitDrawingObject(int32 ObjectType, int32 Layer, int32 ThickNess, int32 mode)
{
	int32 Found = 0, result = 0, ObjectCodeNr, DrawCode2, BrushMode, PenMode;

	if (BackGroundActive)
	{
		ok = 1;

		switch (mode & 0x1f)
		{
		case DRAW_WITH_PEN_AND_NOT_FILLED:
		case DRAW_WITH_HILITED_PEN_AND_NOT_FILLED:
		case DRAW_WITH_PEN_AND_NO_BRUSH:
		case DRAW_WITH_WHITE_PEN_AND_NOT_FILLED:
//      case DRAW_WITH_PEN_AND_BACKGROUND_BRUSH:
			SetNewGraphicsMode(BackGroundObjectNr, ThickNess, 1, 0);
			break;

		case NORMAL_FILLED_AND_NO_PEN:
			SetNewGraphicsMode(BackGroundObjectNr, 0, 0, 1);
			break;

		default:
			SetNewGraphicsMode(BackGroundObjectNr, 0, 0, 0);
			break;
		}

		BackGroundActive = 0;
		return 0;
	}

	PenMode = 0;
	BrushMode = 0;
	ObjectCodeNr = -1;

	switch (mode & 0x1f)
	{
	case NORMAL_FILLED_AND_PEN1:
		ThickNess = 1;
		PenMode = 0;
		BrushMode = 0;
		break;

	case NORMAL_FILLED_AND_NO_PEN:
		ThickNess = 0;
		PenMode = 1;
		BrushMode = 0;
		break;

	case DRAW_WITH_PEN_AND_NOT_FILLED:
		PenMode = 0;
		BrushMode = 1;
		break;

	case DRAW_WITH_PEN_AND_NO_BRUSH:
		PenMode = 0;
		BrushMode = 1;
		break;

	case DRAW_WITH_PEN_AND_BACKGROUND_BRUSH:
		PenMode = 0;
		BrushMode = 2;
		break;

	case HILITED_NORMAL_FILLED_AND_PEN1:
		ThickNess = 1;
		PenMode = 0;
		BrushMode = 0;
		break;

	case IN_NET_FILLED_AND_PEN1:
		ThickNess = 1;
		PenMode = 0;
		BrushMode = 0;
		break;

	case HILITED_IN_NET_FILLED_AND_PEN1:
		ThickNess = 1;
		PenMode = 0;
		BrushMode = 0;
		break;

	case DRAW_WITH_WHITE_PEN_AND_NOT_FILLED:
		ThickNess = 1;
		PenMode = 3;
		BrushMode = 0;
		break;

	case UNCONNECTED_PADS_FILLED_AND_PEN1:
		ThickNess = 1;
		PenMode = 0;
		BrushMode = 3;
		break;

	case DRAW_WITH_DASH_PEN_AND_NO_BRUSH:
		PenMode = 2;
		BrushMode = 1;
		break;
	}

	switch (Layer)
	{
	case SHAPE_PINS_BOTTOM:
		if (((mode & 0x07) != HILITED_NORMAL_FILLED_AND_PEN1) && ((mode & 0x07) != HILITED_NORMAL_FILLED_AND_NO_PEN))
			ObjectCodeNr = ShapePinsBottomObjectNr;
		else
			ObjectCodeNr = ShapePinsBottomHilitedObjectNr;

		Found = 1;
		break;

	case SHAPE_PINS_TOP:
		if (((mode & 0x07) != HILITED_NORMAL_FILLED_AND_PEN1) && ((mode & 0x07) != HILITED_NORMAL_FILLED_AND_NO_PEN))
			ObjectCodeNr = ShapePinsTopObjectNr;
		else
			ObjectCodeNr = ShapePinsTopHilitedObjectNr;

		Found = 1;
		break;

	case SHAPE_PINS_INNER:
		if (((mode & 0x07) != HILITED_NORMAL_FILLED_AND_PEN1) && ((mode & 0x07) != HILITED_NORMAL_FILLED_AND_NO_PEN))
			ObjectCodeNr = ShapePinsInnerObjectNr;
		else
			ObjectCodeNr = ShapePinsInnerHilitedObjectNr;

		Found = 1;
		break;

	case VIA_LAYER:
		switch (mode & 0x07)
		{
		case NORMAL_FILLED_AND_PEN1:
		case NORMAL_FILLED_AND_NO_PEN:
			ObjectCodeNr = ViaPinsObjectNr;
			Found = 1;
			break;

		case HILITED_NORMAL_FILLED_AND_PEN1:
			ObjectCodeNr = ViaPinsHilitedObjectNr;
			Found = 1;
			break;

		case IN_NET_FILLED_AND_PEN1:
			ObjectCodeNr = ViaPinsInNetObjectNr;
			Found = 1;
			break;

		case HILITED_IN_NET_FILLED_AND_PEN1:
			ObjectCodeNr = ViaPinsHilitedInNetObjectNr;
			Found = 1;
			break;

		case IN_NET_FILLED_AND_NO_PEN:
			ObjectCodeNr = ViaPinsInNetObjectNr;
			Found = 1;
			break;

		case HILITED_IN_NET_FILLED_AND_NO_PEN:
			ObjectCodeNr = ViaPinsHilitedInNetObjectNr;
			Found = 1;
			break;
		}

		break;

	case UNCONNECTED_PADS_TOP_LAYER:
		ObjectCodeNr = UnconnectedPadsTopObjectNr;
		Found = 1;
		break;

	case UNCONNECTED_PADS_BOTTOM_LAYER:
		ObjectCodeNr = UnconnectedPadsBottomObjectNr;
		Found = 1;
		break;

	case UNCONNECTED_PADS_INNER_LAYER:
		ObjectCodeNr = UnconnectedPadsInnerObjectNr;
		Found = 1;
		break;

	case SOLD_MASK_BOTTOM:
		ObjectCodeNr = SoldMaskBottomObjectNr;
		Found = 1;
		break;

	case SOLD_MASK_TOP:
		ObjectCodeNr = SoldMaskTopObjectNr;
		Found = 1;
		break;

	case PASTE_MASK_BOTTOM:
		ObjectCodeNr = PasteMaskBottomObjectNr;
		Found = 1;
		break;

	case PASTE_MASK_TOP:
		ObjectCodeNr = PasteMaskTopObjectNr;
		Found = 1;
		break;

	case INFO_LAYER:
		ObjectCodeNr = ObjectsInfoObjectNr;
		Found = 1;
		break;

	case INFO_LAYER2:
		ObjectCodeNr = ObjectsInfo2ObjectNr;
		Found = 1;
		break;

	case INFO_LAYER3:
		ObjectCodeNr = ObjectsInfo3ObjectNr;
		Found = 1;
		break;

	case INFO_LAYER4:
		ObjectCodeNr = ObjectsInfo4ObjectNr;
		Found = 1;
		break;

	case BOARD_OUTLINE_LAYER:
		ObjectCodeNr = BoardOutlineObjectNr;
		Found = 1;
		break;

	case BOARD_OUTLINE_KEEPOUT_LAYER:
		ObjectCodeNr = BoardOutlineKeepOutObjectNr;
		Found = 1;
		break;

	case SILKSCREEN_BOTTOM:
		ObjectCodeNr = SilkScreenBottomObjectNr;
		Found = 1;
		break;

	case SILKSCREEN_TOP:
		ObjectCodeNr = SilkScreenTopObjectNr;
		Found = 1;
		break;

	case BACKGROUND_LAYER:
		ObjectCodeNr = BackGroundObjectNr;
		Found = 1;
		break;

	case SWAP_PINS_LAYER:
		ObjectCodeNr = SwappablePinsGateObjectNr;
		Found = 1;
		break;

	case SWAP_GATE_LAYER:
		ObjectCodeNr = SwappableGatePinsObjectNr;
		Found = 1;
		break;

	case COMP_REF_LAYER:
		ObjectCodeNr = ReferenceObjectNr;
		Found = 1;
		break;

	case COMP_VALUE_LAYER:
		ObjectCodeNr = CompValueObjectNr;
		Found = 1;
		break;

	case PLACEMENT_OUTLINE_TOP:
		ObjectCodeNr = ShapePlacementOutLineTopObjectNr;
		Found = 1;
		break;

	case PLACEMENT_OUTLINE_BOTTOM:
		ObjectCodeNr = ShapePlacementOutLineBottomObjectNr;
		Found = 1;
		break;

	case PLACEMENT_OUTLINE_TOP2:
		ObjectCodeNr = ShapePlacementOutLineTopObjectNr;
		PenMode = 2;
		Found = 1;
		break;

	case PLACEMENT_OUTLINE_BOTTOM2:
		ObjectCodeNr = ShapePlacementOutLineBottomObjectNr;
		PenMode = 2;
		Found = 1;
		break;

	case CONNECTIONS_LAYER:
		if ((mode & 0x1f) != DRAW_WITH_HILITED_PEN_AND_NOT_FILLED)
			ObjectCodeNr = ConnectionsObjectNr;
		else
			ObjectCodeNr = ConnectionsHilitedObjectNr;

		Found = 1;
		break;

	case COMP_OUTLINE_LAYER_TOP:
		ObjectCodeNr = ShapeCompOutLineTopObjectNr;
		Found = 1;
		break;

	case COMP_OUTLINE_LAYER_BOTTOM:
		ObjectCodeNr = ShapeCompOutLineBottomObjectNr;
		Found = 1;
		break;

	case FIXED_COLOR_LAYER:
		switch (mode & 0xf00)
		{
		case GRAPHICS_YELLOW:
			ObjectCodeNr = YellowObjectNr;
			Found = 1;
			break;

		case GRAPHICS_BLUE:
			ObjectCodeNr = BlueObjectNr;
			Found = 1;
			break;

		case GRAPHICS_MAGENTA:
			ObjectCodeNr = MagentaObjectNr;
			Found = 1;
			break;

		case GRAPHICS_WHITE:
			ObjectCodeNr = WhiteObjectNr;
			Found = 1;
			break;

		case GRAPHICS_BLACK:
			ObjectCodeNr = BlackObjectNr;
			Found = 1;
			break;

		case GRAPHICS_GRAY:
			ObjectCodeNr = GrayObjectNr;
			Found = 1;
			break;

		case GRAPHICS_RED:
			ObjectCodeNr = RedObjectNr;
			Found = 1;
			break;

		case GRAPHICS_GREEN:
			ObjectCodeNr = GreenObjectNr;
			Found = 1;
			break;
		}

		if (PenMode == 3)
		{
			ObjectCodeNr = WhitePenObjectNr;
			BrushMode = 1;
			Found = 1;
		}

		break;

	case CLEARANCE_LAYER:
		ObjectCodeNr = ClearanceObjectNr;
		Found = 1;
		break;

	case ERROR_LAYER:
		ObjectCodeNr = ErrorObjectNr;
		Found = 1;
		break;

	case WARNING_LAYER:
		ObjectCodeNr = WarningObjectNr;
		Found = 1;
		break;

	case DRILL_LAYER:
		ObjectCodeNr = ShapePinsDrillObjectNr;
		Found = 1;
		break;

	case DRILL_UNPLATED_LAYER:
		ObjectCodeNr = ShapePinsDrillUnplatedObjectNr;
		Found = 1;
		break;

	case VIA_DRILL_LAYER:
		ObjectCodeNr = ViaPinsDrillObjectNr;
		Found = 1;
		break;

	case BUTTON_INFO_LAYER:
		ObjectCodeNr = ButtonInfoObjectNr;
		PenMode = 3;			// White pen
		Found = 1;
		break;

	case POLYGON_DRAW_LAYER:
		ObjectCodeNr = PolylineObjectNr;
		Found = 1;
		break;

	case CROSS_HAIR_LAYER:
		ObjectCodeNr = CrossHairObjectNr;
		Found = 1;
		break;

	case SPECIALS_LAYER:
		break;

	case NET_PINS_LAYER:
		switch (mode & 0x03)
		{
		case 0:				// NetPins in the net color
			ObjectCodeNr = NetPinsObjectNr;
			Found = 1;
			break;

		case 1:				// NetPins in the powerplane color
			ObjectCodeNr = NetPinsObject2Nr;
			Found = 1;
			break;
		}

		break;

	case AREAFILL_DASH_PEN_LAYER:
		ObjectCodeNr = BackgroundDashPenObjectNr;
		PenMode = 2;
		BrushMode = 0;
		ThickNess = 1;
		Found = 1;
		break;

	default:
		if ((Layer >= 0) && (Layer < 32))
		{
			if ((mode & USE_LAYER_DRAW_CODE) == 0)
				DrawCode2 = Layer;
			else
				DrawCode2 = DrawLayerCode[Layer];

			switch (mode & 0x1f)
			{
			case DRAW_WITH_PEN_AND_NOT_FILLED:
				switch (ObjectType)
				{
				case PIN_ARC:
				case PIN_LINE_ALL_ANGLE:
					break;

				default:
					ObjectCodeNr = ViewLayer1ObjectNr + DrawCode2;
					Found = 1;
					break;
				}

				break;
			}
		}

		if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		{
			if (Layer == ROUTING_KEEPOUT_LAYER)
			{	// Bottom
				ObjectCodeNr = RoutingKeepoutBottomObjectNr;
			}
			else
			{
				if (Layer == ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1)
				{	// top
					ObjectCodeNr = RoutingKeepoutTopObjectNr;
				}
				else
				{	// Inner layers
					ObjectCodeNr = RoutingKeepoutInnerObjectNr;
				}
			}

			Found = 1;
		}
	}

	if (Found)
	{
		SetNewGraphicsMode(ObjectCodeNr, ThickNess, BrushMode, PenMode);
		return result;
	}

// ****************************************************************************************************
// ****************************************************************************************************

	switch (ObjectType)
	{
	case TRACE_HOR:
	case TRACE_VER:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
	case TRACE_ALL_ANGLE:
	case TRACE_ARC:
		if ((mode & USE_LAYER_DRAW_CODE) == 0)
			DrawCode2 = Layer;
		else
			DrawCode2 = DrawLayerCode[Layer];

		switch (mode & 0x1f)
		{
		case NORMAL_FILLED_AND_PEN1:
			ObjectCodeNr = ViewLayer1ObjectNr + DrawCode2;
			Found = 1;
			break;

		case HILITED_NORMAL_FILLED_AND_PEN1:
			ObjectCodeNr = ViewLayer1HilitedObjectNr + DrawCode2;
			Found = 1;
			break;

		case NORMAL_FILLED_AND_NO_PEN:
			ObjectCodeNr = ViewLayer1ObjectNr + DrawCode2;
			Found = 1;
			break;

		case IN_NET_FILLED_AND_PEN1:
			ObjectCodeNr = ViewLayer1InNetObjectNr + DrawCode2;
			PenMode = 2;
			BrushMode = 0;
//          BrushMode=1;
			Found = 1;
			ThickNess = 1;
			break;

		case HILITED_IN_NET_FILLED_AND_PEN1:
			ObjectCodeNr = ViewLayer1HilitedInNetObjectNr + DrawCode2;
			PenMode = 2;
			Found = 1;
			break;
		}

		break;

	case PIN_PUT_THROUGH_POLYGON:
	case PIN_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
	case PIN_ARC:
	case PIN_LINE_ALL_ANGLE:
		if (Layer != 0)
		{
			switch (mode & 0x1f)
			{
			case NORMAL_FILLED_AND_PEN1:
			case DRAW_WITH_PEN_AND_NOT_FILLED:
			case NORMAL_FILLED_AND_NO_PEN:
				ObjectCodeNr = ShapePinsTopObjectNr;
				Found = 1;
				break;
			}
		}
		else
		{
			switch (mode & 0x1f)
			{
			case NORMAL_FILLED_AND_PEN1:
			case DRAW_WITH_PEN_AND_NOT_FILLED:
			case NORMAL_FILLED_AND_NO_PEN:
				ObjectCodeNr = ShapePinsBottomObjectNr;
				Found = 1;
				break;
			}
		}

		break;

	case PIN_PUT_THROUGH_ROUND_POWER:
		break;

	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
		break;

	case VIA_PUT_THROUGH_ROUND:
		switch (mode & 0x07)
		{
		case NORMAL_FILLED_AND_PEN1:
		case NORMAL_FILLED_AND_NO_PEN:
			ObjectCodeNr = ViaPinsObjectNr;
			Found = 1;
			break;

		case HILITED_NORMAL_FILLED_AND_PEN1:
			ObjectCodeNr = ViaPinsHilitedObjectNr;
			Found = 1;
			break;

		case IN_NET_FILLED_AND_PEN1:
			ObjectCodeNr = ViaPinsInNetObjectNr;
			Found = 1;
			break;

		case HILITED_IN_NET_FILLED_AND_PEN1:
			ObjectCodeNr = ViaPinsHilitedInNetObjectNr;
			Found = 1;
			break;
		}

		break;

	case DRILL:
		break;

	case DRILL_UNPLATED:
		break;

	case OBJECT_LINE:
		break;

	case OBJECT_POLYLINE:
		break;

	case OBJECT_RECT:
		break;

	case OBJECT_CIRCLE:
		break;

	case OBJECT_ARC:
		break;

	case OBJECT_TEXT:
		break;

	case OBJECT_TEXT2:
		break;

	case OBJECT_POLYGON:
		break;

	case ROUTING_KEEPOUT_RECT:
		break;

	case CONNECTION:
		break;

	case AREAFILL:
		break;

	case NIKS:
		break;

	case COMP_OBJECT:
		break;
	}

#ifdef _DEBUG

	if (!Found)
		ok = 1;

#endif

	if (Found)
	{
		SetNewGraphicsMode(ObjectCodeNr, ThickNess, BrushMode, PenMode);
		return result;
	}

	return result;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void SetBackGroundActive(int32 mode)
{
	BackGroundActive = 1;
	CurrentObjectCode = -1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawLineYellow(double x1, double y1, double x2, double y2)
{
	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_MERGEPEN);
	DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
	         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawLineWhite(double x1, double y1, double x2, double y2)
{
	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_MERGEPEN);
	DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
	         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
	ExitDrawing();
	EndDrawingEditingWindow();

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawLineRed(double x1, double y1, double x2, double y2)
{

	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_RED + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_COPYPEN);
	DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
	         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
	ExitDrawing();
	EndDrawingEditingWindow();

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DrawLineGray(double x1, double y1, double x2, double y2)
{

	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GRAY + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_XORPEN);
	DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
	         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
	ExitDrawing();
	EndDrawingEditingWindow();

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawLineGreen(double x1, double y1, double x2, double y2)
{

	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_COPYPEN);
	DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
	         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
	ExitDrawing();
	EndDrawingEditingWindow();

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawCircleWhite(double x, double y, double dikte)
{
	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_COPYPEN);
//  DrawLine(Mult(x1-Xoffset),DrawWindowMaxY-Mult(y1-Yoffset)-1,Mult(x2-Xoffset),DrawWindowMaxY-Mult(y2-Yoffset)-1);
	ellips2(Mult(x - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y - Yoffset) - 1, Mult(dikte), Mult(dikte), 255);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawCircleYellow(double x, double y, double dikte)
{
	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_COPYPEN);
//  DrawLine(Mult(x1-Xoffset),DrawWindowMaxY-Mult(y1-Yoffset)-1,Mult(x2-Xoffset),DrawWindowMaxY-Mult(y2-Yoffset)-1);
	ellips2(Mult(x - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y - Yoffset) - 1, Mult(dikte), Mult(dikte), 255);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawCircleGray(double x, double y, double dikte)
{
	StartDrawingEditingWindow();
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GRAY + DRAW_WITH_PEN_AND_NOT_FILLED);
	SetROP2(OutputDisplay, R2_COPYPEN);
//  DrawLine(Mult(x1-Xoffset),DrawWindowMaxY-Mult(y1-Yoffset)-1,Mult(x2-Xoffset),DrawWindowMaxY-Mult(y2-Yoffset)-1);
	ellips2(Mult(x - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y - Yoffset) - 1, Mult(dikte), Mult(dikte), 255);
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
		CurrentObjectCode = -1;
	}

	if (SavePen != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SavePen);
		SavePen = (HGDIOBJ) - 1;
		CurrentObjectCode = -1;
	}

	if (SaveFont != (HGDIOBJ) - 1)
	{
		SelectObject(OutputDisplay, SaveFont);
		SaveFont = (HGDIOBJ) - 1;
		CurrentObjectCode = -1;
	}
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void LoadDefaultColors()
{
	int32 cnt;

	PCBColors[ViewLayer1ObjectNr] = RGB_StrongPink;
	PCBColors[ViewLayer2ObjectNr] = RGB_AzureBlue;
	PCBColors[ViewLayer3ObjectNr] = RGB_TealBlue;
	PCBColors[ViewLayer4ObjectNr] = RGB_BrownOrange;
	PCBColors[ViewLayer5ObjectNr] = RGB_DarkGreen;
	PCBColors[ViewLayer6ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer7ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer8ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer9ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer10ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer11ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer12ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer13ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer14ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer15ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer16ObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer1HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer1ObjectNr]);	//  RGB_LightRed;
	PCBColors[ViewLayer2HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer2ObjectNr]);	//  RGB_LightBlue;
	PCBColors[ViewLayer3HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer3ObjectNr]);	//  RGB_LightPink;
	PCBColors[ViewLayer4HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer4ObjectNr]);	//  RGB_LightOrange;
	PCBColors[ViewLayer5HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer5ObjectNr]);	//  RGB_Green;
	PCBColors[ViewLayer6HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer6ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer7HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer7ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer8HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer8ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer9HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer9ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer10HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer10ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer11HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer11ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer12HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer12ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer13HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer13ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer14HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer14ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer15HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer15ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer16HilitedObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer16ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer1InNetObjectNr] = RGB_StrongPink;
	PCBColors[ViewLayer2InNetObjectNr] = RGB_AzureBlue;
	PCBColors[ViewLayer3InNetObjectNr] = RGB_TealBlue;
	PCBColors[ViewLayer4InNetObjectNr] = RGB_BrownOrange;
	PCBColors[ViewLayer5InNetObjectNr] = RGB_DarkGreen;
	PCBColors[ViewLayer6InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer7InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer8InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer9InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer10InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer11InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer12InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer13InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer14InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer15InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer16InNetObjectNr] = RGB_Magenta;
	PCBColors[ViewLayer1HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer1ObjectNr]);	//  RGB_LightRed;
	PCBColors[ViewLayer2HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer2ObjectNr]);	//  RGB_LightBlue;
	PCBColors[ViewLayer3HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer3ObjectNr]);	//  RGB_LightPink;
	PCBColors[ViewLayer4HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer4ObjectNr]);	//  RGB_LightOrange;
	PCBColors[ViewLayer5HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer5ObjectNr]);	//  RGB_Green;
	PCBColors[ViewLayer6HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer6ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer7HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer7ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer8HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer8ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer9HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer9ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer10HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer10ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer11HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer11ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer12HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer12ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer13HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer13ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer14HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer14ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer15HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer15ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ViewLayer16HilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViewLayer16ObjectNr]);	//  RGB_LightMagenta;
	PCBColors[ConnectionsObjectNr] = RGB_DarkGreen;
	PCBColors[ConnectionsHilitedObjectNr] = RGB_Green;
	PCBColors[NetPinsObjectNr] = RGB_Yellow;
	PCBColors[NetPinsObject2Nr] = RGB_Red;
	PCBColors[SilkScreenTopObjectNr] = RGB_ScarletRed;
	PCBColors[SilkScreenBottomObjectNr] = RGB_IndianRed;
	PCBColors[ReferenceObjectNr] = RGB_LightGray;
	PCBColors[CompValueObjectNr] = RGB_DarkYellow;
	PCBColors[ShapePlacementOutLineTopObjectNr] = RGB_LightSepiaBrown;
	PCBColors[ShapePlacementOutLineBottomObjectNr] = RGB_SepiaBrown;
	PCBColors[ShapeCompOutLineTopObjectNr] = RGB_LightGray;
	PCBColors[ShapeCompOutLineBottomObjectNr] = RGB_MidDarkGray;
	PCBColors[ShapePinsTopObjectNr] = RGB_IndigoBlue;
	PCBColors[ShapePinsBottomObjectNr] = RGB_HeliotropePink;
	PCBColors[ShapePinsInnerObjectNr] = RGB_DarkGray;
	PCBColors[ShapePinsTopHilitedObjectNr] = MakeHiliteColor2(PCBColors[ShapePinsTopObjectNr]);
	PCBColors[ShapePinsBottomHilitedObjectNr] = MakeHiliteColor2(PCBColors[ShapePinsBottomObjectNr]);
	PCBColors[ShapePinsInnerHilitedObjectNr] = MakeHiliteColor2(PCBColors[ShapePinsInnerObjectNr]);
	PCBColors[ShapePinsDrillObjectNr] = RGB_VenetianRed;
	PCBColors[ShapePinsDrillUnplatedObjectNr] = RGB_PumpkinOrange;
	PCBColors[ViaPinsObjectNr] = RGB(113, 113, 0);

	PCBColors[ViaPinsHilitedObjectNr] = MakeHiliteColor2(PCBColors[ViaPinsObjectNr]);
//  PCBColors[ViaPinsHilitedObjectNr             ] = RGB(187,187,0);
	PCBColors[ViaPinsInNetObjectNr] = PCBColors[ViaPinsObjectNr];
	PCBColors[ViaPinsHilitedInNetObjectNr] = MakeHiliteColor2(PCBColors[ViaPinsInNetObjectNr]);
	PCBColors[ViaPinsDrillObjectNr] = RGB_DarkRed;
	PCBColors[ObjectsInfoObjectNr] = RGB_Cyan;
	PCBColors[ClearanceObjectNr] = RGB_Gray;
	PCBColors[StippelObjectNr] = RGB_Gray;
	PCBColors[PolylineObjectNr] = RGB_Yellow;
	PCBColors[ErrorObjectNr] = RGB_Yellow;
	PCBColors[WarningObjectNr] = RGB_Yellow;
	PCBColors[GridObjectNr] = RGB_MidGray;
	PCBColors[ButtonInfoObjectNr] = RGB_Yellow;
	PCBColors[BackGroundObjectNr] = RGB_Black;
	PCBColors[BoardOutlineObjectNr] = RGB_Gray;
	PCBColors[BoardOutlineKeepOutObjectNr] = RGB_Yellow3;

	PCBColors[SwappablePinsGateObjectNr] = RGB_Yellow;
	PCBColors[SwappableGatePinsObjectNr] = RGB_LightMagenta;
	PCBColors[PasteMaskTopObjectNr] = RGB_LightAzureBlue;
	PCBColors[PasteMaskBottomObjectNr] = RGB_LightStrongPink;
	PCBColors[SoldMaskTopObjectNr] = PCBColors[ShapePinsTopObjectNr];
	PCBColors[SoldMaskBottomObjectNr] = PCBColors[ShapePinsBottomObjectNr];
//  PCBColors[SoldMaskTopObjectNr                ] = RGB_Red;
//  PCBColors[SoldMaskBottomObjectNr             ] = RGB_Blue;
	PCBColors[ObjectsInfo2ObjectNr] = RGB_LightAzureBlue2;
	PCBColors[ObjectsInfo3ObjectNr] = RGB_MauveViolet;
	PCBColors[ObjectsInfo4ObjectNr] = RGB_PeachOrange;
	PCBColors[UnconnectedPadsTopObjectNr] = RGB_IndigoBlue;
	PCBColors[UnconnectedPadsBottomObjectNr] = RGB_HeliotropePink;
	PCBColors[UnconnectedPadsInnerObjectNr] = RGB_DarkGray;
	PCBColors[RoutingKeepoutTopObjectNr] = RGB_DarkRed;
	PCBColors[RoutingKeepoutBottomObjectNr] = RGB_DarkRed;
	PCBColors[RoutingKeepoutInnerObjectNr] = RGB_DarkGreen;
	PCBColors[CrossHairObjectNr] = RGB_White;

	PCBColors[YellowObjectNr] = RGB_Yellow;
	PCBColors[MagentaObjectNr] = RGB_Magenta;
	PCBColors[WhiteObjectNr] = RGB_White;
	PCBColors[BlackObjectNr] = RGB_Black;
	PCBColors[GrayObjectNr] = RGB_Gray;
	PCBColors[RedObjectNr] = RGB_Red;
	PCBColors[GreenObjectNr] = RGB_Green;
	PCBColors[BlueObjectNr] = RGB_Blue;

	memset(&PCBObjectCodes, 0, sizeof(PCBObjectCodes));

	for (cnt = 0; cnt < 16; cnt++)
	{
//    PCBObjectCodes[ViewLayer1InNetObjectNr+cnt]=(uint32)((0 << 8)+(2 << 20));
//    PCBObjectCodes[ViewLayer1HilitedInNetObjectNr+cnt]=(uint32)((0 << 8)+(2 << 20));
//    PCBObjectCodes[ViewLayer1InNetObjectNr+cnt]=(uint32)((13 << 8)+(2 << 20));
//    PCBObjectCodes[ViewLayer1HilitedInNetObjectNr+cnt]=(uint32)((13 << 8)+(2 << 20));
		PCBObjectCodes[ViewLayer1InNetObjectNr + cnt] = (uint32) ((13 << 8) + (2 << 20) + (1 << 16));
		PCBObjectCodes[ViewLayer1HilitedInNetObjectNr + cnt] = (uint32) ((13 << 8) + (2 << 20) + (1 << 16));
	}

	PCBObjectCodes[ViaPinsInNetObjectNr] = (13 << 8);
	PCBObjectCodes[ViaPinsHilitedInNetObjectNr] = (13 << 8);

	PCBObjectCodes[RoutingKeepoutTopObjectNr] = (8 << 8);
	PCBObjectCodes[RoutingKeepoutBottomObjectNr] = (9 << 8);

	PCBObjectCodes[UnconnectedPadsTopObjectNr] = (11 << 8);
	PCBObjectCodes[UnconnectedPadsBottomObjectNr] = (11 << 8);
	PCBObjectCodes[UnconnectedPadsInnerObjectNr] = (11 << 8);

	PCBObjectCodes[PasteMaskTopObjectNr] = (7 << 8);
	PCBObjectCodes[PasteMaskBottomObjectNr] = (7 << 8);

	PCBObjectCodes[ShapePinsInnerObjectNr] = (8 << 8);
//  PCBObjectCodes[ViaPinsObjectNr]               =(8 << 8);
	PCBObjectCodes[BoardOutlineKeepOutObjectNr] = (16 << 8);

	PCBObjectCodes[ErrorObjectNr] = (15 << 8);
	PCBObjectCodes[BackgroundDashPenObjectNr] = (2 << 20) + (1 << 16);

	for (cnt = 0; cnt < 192; cnt++)
	{
		PCBObjectCodes[cnt] |= (1 << 24);
		PCBColors2[cnt] = (COLORREF) 0x01000000;	// Background color
	}

	PCBColors2[UnconnectedPadsTopObjectNr] = RGB_LightAzureBlue;
	PCBColors2[UnconnectedPadsBottomObjectNr] = RGB_LightStrongPink;
	PCBColors2[UnconnectedPadsInnerObjectNr] = RGB_Yellow;
	PCBColors2[PasteMaskTopObjectNr] = RGB_AmberOrange;
	PCBColors2[PasteMaskBottomObjectNr] = RGB_DarkAmberOrange;


}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void GraphicsMain()
{

	SaveBrush = (HGDIOBJ) - 1;
	SavePen = (HGDIOBJ) - 1;
	SaveFont = (HGDIOBJ) - 1;
	CurrentObjectCode = -1;
	LoadDefaultColors();

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
