/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw2.c
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



#include "types.h"
#include "utf8.h"
#include "memory.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "calc.h"
#include "calc3.h"
#include "help.h"
#include "calc4.h"
#include "calcdef.h"
#include "line2.h"
#include "rect.h"
#include "mainloop.h"
#include "pcb.h"
#include "math.h"
#include "toets.h"
#include "ellipss.h"
#include "string.h"
#include "graphics.h"
#include "resource.h"
#include "resource2.h"
#include "stdio.h"
//#include "commctrl.h"
#include "polygon.h"

#define Mult2(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr))) : ((int32)(Factor*(Nr))) )

#define AREAFILL_DRAWN                    0x0040

int32 ok;

extern int32 Printing, ReverseY;
extern HDC OutputDisplay;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;

extern int32 SelectedColorNr, SelectColorMode;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

#ifdef GCC_COMP
inline int32 GetLayerToDrawOn(int32 ObjectLayer, int32 mode)
#else
__forceinline int32 GetLayerToDrawOn(int32 ObjectLayer, int32 mode)
#endif
{
	if (ObjectLayer < 0)
		return -1;


	if (!ViewSingleLayer)
	{
		if (ObjectLayer == 0)
		{	// Draw on bottom layer
			if (OkToDrawBottomPads)
				return SHAPE_PINS_BOTTOM;
			else
			{
				if ((mode & 1) == 0)
				{
					if (OkToDrawTopPads)
						return SHAPE_PINS_TOP;
					else
					{
						if (OkToDrawInnerPads)
						{	// Draw on inner layer
							return SHAPE_PINS_INNER;
						}
					}
				}
			}
		}
		else
		{	// Draw on top layer
			if (Design.NrBoardLayers > 1)
			{
				if (ObjectLayer == Design.NrBoardLayers - 1)
				{
					if (OkToDrawTopPads)
						return SHAPE_PINS_TOP;
					else
					{
						if ((mode & 1) == 0)
						{
							if (OkToDrawBottomPads)
								return SHAPE_PINS_BOTTOM;
							else
							{
								if (OkToDrawInnerPads)
								{	// Draw on inner layer
									return SHAPE_PINS_INNER;
								}
							}
						}
					}
				}
				else
				{
					if (CheckIfInnerLayer(ObjectLayer))
					{
						if (OkToDrawInnerPads)
						{	// Draw on inner layer
							return SHAPE_PINS_INNER;
						}
						else
						{
							if ((mode & 1) == 0)
							{
								if (OkToDrawBottomPads)
									return SHAPE_PINS_BOTTOM;
								else
								{
									if (OkToDrawTopPads)
										return SHAPE_PINS_TOP;
								}
							}
						}
					}
				}
			}
		}
	}
	else
	{
		if (ObjectLayer == 0)
		{	// Draw on bottom layer
			if (ObjectLayer == CurrentDrawingLayer)
			{
				if (OkToDrawBottomPads)
					return SHAPE_PINS_BOTTOM;
			}
		}
		else
		{	// Draw on top layer
			if (Design.NrBoardLayers > 1)
			{
				if (ObjectLayer == Design.NrBoardLayers - 1)
				{
					if (ObjectLayer == CurrentDrawingLayer)
					{
						if (OkToDrawTopPads)
							return SHAPE_PINS_TOP;
					}
				}
				else
				{
					if (CheckIfInnerLayer(ObjectLayer))
					{
						if (ObjectLayer == CurrentDrawingLayer)
						{
							if (OkToDrawInnerPads)
								return SHAPE_PINS_INNER;
						}
					}
				}
			}
		}
	}

	return -1;					// Nothing to draw
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void ReDisplayNet(int32 mode)
{
	ObjectRecord *Object;
	int32 cnt, ok;

	CurrentObjectCode = -1;
	ok = 1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		if ((Object->Layer == CurrentDrawingLayer) || (Object->Layer == -1))
			continue;

		switch (Object->ObjectType & ~1)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			if ((mode & 2) == 2)
			{	// Hatched
				if (Mult(Object->y2) < 12)
					SetROP2(OutputDisplay, R2_XORPEN);
				else
					SetROP2(OutputDisplay, R2_COPYPEN);

				DrawObject(Object, 0x20);

				if (0)
					GetGraphicsPenBrush(0);
			}
			else
				DrawObject(Object, 0);

			break;

		case TRACE_ALL_ANGLE:
		case TRACE_ARC:
			if ((mode & 2) == 2)
			{	// Hatched
				if (Mult(Object->Thickness) < 12)
					SetROP2(OutputDisplay, R2_XORPEN);
				else
					SetROP2(OutputDisplay, R2_COPYPEN);

				DrawObject(Object, 0x20);
			}
			else
				DrawObject(Object, 0);

			break;
		}
	}

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		switch (Object->ObjectType & ~1)
		{
		case VIA_PUT_THROUGH_ROUND:
			if ((mode & 2) == 0)
				DrawObject(Object, 0);
			else
				DrawObject(Object, 0x20);

			break;
		}
	}

	if ((OkToDrawClearances) && ((mode & 2) == 0))
	{
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object = &((*Objects3)[cnt]);

			if (Object->Layer == CurrentDrawingLayer)
				continue;

			switch (Object->ObjectType & ~1)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
			case TRACE_ALL_ANGLE:
			case TRACE_ARC:
				DrawObject(Object, 0x40 + 8);
				break;
			}
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************
	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		if (Object->Layer != CurrentDrawingLayer)
			continue;

		switch (Object->ObjectType & ~1)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			if ((mode & 2) == 2)
			{	// Hatched
				if (Mult(Object->y2) < 12)
					SetROP2(OutputDisplay, R2_XORPEN);
				else
					SetROP2(OutputDisplay, R2_COPYPEN);

				DrawObject(Object, 0x20);

				if (0)
					GetGraphicsPenBrush(0);
			}
			else
				DrawObject(Object, 0);

			break;

		case TRACE_ALL_ANGLE:
		case TRACE_ARC:
			if ((mode & 2) == 2)
			{	// Hatched
				if (Mult(Object->Thickness) < 12)
					SetROP2(OutputDisplay, R2_XORPEN);
				else
					SetROP2(OutputDisplay, R2_COPYPEN);

				DrawObject(Object, 0x20);
			}
			else
				DrawObject(Object, 0);

			break;
		}
	}

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		switch (Object->ObjectType & ~1)
		{
		case VIA_PUT_THROUGH_ROUND:
			if ((mode & 2) == 0)
				DrawObject(Object, 0);
			else
				DrawObject(Object, 0x20);

			break;
		}
	}

	if ((OkToDrawClearances) && ((mode & 2) == 0))
	{
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object = &((*Objects3)[cnt]);

			if (Object->Layer != CurrentDrawingLayer)
				continue;

			switch (Object->ObjectType & ~1)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
			case TRACE_ALL_ANGLE:
			case TRACE_ARC:
				DrawObject(Object, 0x40 + 8);
				break;
			}
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************
	if (((OkToDrawClearances) || (OkToDrawViaClearances)) && ((mode & 2) == 0))
	{
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object = &((*Objects3)[cnt]);

			if ((Object->ObjectType & ~1) == VIA_PUT_THROUGH_ROUND)
				DrawObject(Object, 0x40 + 8);
		}
	}

	if (DrawDrillMode > 0)
	{
		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object = &((*Objects3)[cnt]);

			switch (Object->ObjectType & ~1)
			{
			case VIA_PUT_THROUGH_ROUND:
				if ((mode & 2) == 0)
					DrawDrillObject(Object, 0);
			}
		}
	}

	ok = 1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawCrossObjects(int32 Repaint, int32 Mode)
{
	int32 cnt, PenType;
	double x1, y1, x2, y2, x, y, dx, dy, divx, MinPixel;
#ifdef _DEBUG
	int32 res;
#endif

	ObjectRecord *Object;
	double dikte;
	divx = PixelToReal(1);

	if (!Repaint)
		StartDrawingEditingWindow();

//  if (Mode==0) {
//    if (divx>10000.0) DrawPen=CreatePen(PS_SOLID,1,NetPinsColor);
//  GetStockObject
//    if (divx>0.1) DrawPen=CreatePen(PS_SOLID,1,RGB(0,0,0));
//    else DrawPen=CreatePen(PS_SOLID,2,RGB(0,0,0));
//    LineColor=RGB(0,0,0);
//  }
	PenType = 0;
	InitDrawingObject(0, NET_PINS_LAYER, 3, DRAW_WITH_PEN_AND_NOT_FILLED);

	MinPixel = PixelToReal(3);
	SetROP2(OutputDisplay, R2_XORPEN);

// ColorMacro(RGB_Red);
	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		if (Object->Layer != -1)
		{
			if (DrawLayerCode[Object->Layer] & 0x10)
				continue;

			if ((Object->Layer == 0) && (!OkToDrawBottomPads))
				continue;

			if ((Object->Layer == Design.NrBoardLayers - 1) && (!OkToDrawTopPads))
				continue;
		}

		if (PenType == 0)
		{
			if ((Object->ObjectType & 1) == 1)
			{
				InitDrawingObject(0, NET_PINS_LAYER, 3, DRAW_WITH_PEN_AND_NOT_FILLED);
				PenType = 1;
			}
		}
		else
		{
			if ((Object->ObjectType & 1) == 0)
			{
				InitDrawingObject(0, NET_PINS_LAYER, 3, DRAW_WITH_PEN_AND_NOT_FILLED);
				PenType = 0;
			}
		}

//    DrawObject(Object,Mode);
		switch (Object->ObjectType & ~1)
		{
		case PIN_LINE_HOR:
			dikte = Object->y2;
			x = Object->x1;
			y = Object->y1;
			dx = max(dikte / 2, MinPixel);
			x1 = x - dx;
			y1 = y - dx;
			x2 = x + Object->x2 + dx;
			y2 = y + dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));

			x1 = x - dx;
			y1 = y + dx;
			x2 = x + Object->x2 + dx;
			y2 = y - dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			break;

		case PIN_LINE_VER:
			dikte = Object->y2;
			x = Object->x1;
			y = Object->y1;
			dx = max(dikte / 2, MinPixel);
			x1 = x - dx;
			y1 = y - dx;
			x2 = x + dx;
			y2 = y + Object->x2 + dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			x1 = x + dx;
			y1 = y - dx;
			x2 = x - dx;
			y2 = y + Object->x2 + dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			break;

		case PIN_LINE_DIAG1:
			dikte = Object->y2;
			x = Object->x1;
			y = Object->y1;
			dx = max(dikte / 2, MinPixel);
			x1 = x - dx;
			y1 = y - dx;
			x2 = x + Object->x2 + dx;
			y2 = y - Object->x2 + dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			x1 = x + dx;
			y1 = y + dx;
			x2 = x + Object->x2 - dx;
			y2 = y - Object->x2 - dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			break;

		case PIN_LINE_DIAG2:
			dikte = Object->y2;
			x = Object->x1;
			y = Object->y1;
			dx = max(dikte / 2, MinPixel);
			x1 = x + dx;
			y1 = y - dx;
			x2 = x + Object->x2 - dx;
			y2 = y + Object->x2 + dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			x1 = x - dx;
			y1 = y + dx;
			x2 = x + Object->x2 + dx;
			y2 = y + Object->x2 - dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			break;

		case DRILL:
		case PIN_PUT_THROUGH_ROUND:
		case VIA_PUT_THROUGH_ROUND:
		case PIN_SMD_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
			dikte = Object->x2;
			x = Object->x1;
			y = Object->y1;
#ifdef _DEBUG

			if ((InRange9(x, 136.5e5)) && (InRange9(y, 77.8e5)))
				res = 1;

#endif
			dx = max(dikte / 2, MinPixel);
			x1 = x - dx;
			y1 = y - dx;
			x2 = x + dx;
			y2 = y + dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			x1 = x - dx;
			y1 = y + dx;
			x2 = x + dx;
			y2 = y - dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			break;

		case PIN_PUT_THROUGH_POLYGON:
		case PIN_SMD_POLYGON:
			dikte = 2e5;
			x = Object->x1;
			y = Object->y1;
#ifdef _DEBUG

			if ((InRange9(x, 136.5e5)) && (InRange9(y, 77.8e5)))
				res = 1;

#endif
			dx = max(dikte / 2, MinPixel);
			x1 = x - dx;
			y1 = y - dx;
			x2 = x + dx;
			y2 = y + dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			x1 = x - dx;
			y1 = y + dx;
			x2 = x + dx;
			y2 = y - dx;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			break;

		case PIN_SMD_RECT:
			x = Object->x1;
			y = Object->y1;
			dx = max((Object->x2 / 2) * 1.3, MinPixel);
			dy = max((Object->y2 / 2) * 1.3, MinPixel);
			x1 = x - dx;
			y1 = y - dy;
			x2 = x + dx;
			y2 = y + dy;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			x1 = x - dx;
			y1 = y + dy;
			x2 = x + dx;
			y2 = y - dy;
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
			break;
		}
	}

	if ((Mode & 1) == 0)
	{
		SetROP2(OutputDisplay, R2_COPYPEN);

		if ((Mode & 2) == 0)
		{
			ReDisplayNet(0);	// Display the traces/vias in the normal form
		}
		else
		{
			ReDisplayNet(2);	// Display the traces in the marked form
		}

//    SetROP2(OutputDisplay,R2_XORPEN);
	}

	if (!Repaint)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
	else
		SetROP2(OutputDisplay, R2_COPYPEN);

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawSpecialObject(ObjectRecord * Object, int32 Number, int32 Mode)
{
	int32 x, y, x1b, y1b, x2b, y2b, x3b, y3b, x4b, y4b, Height2, le;

	double dikte;
	char str[10];
	POINT LinePoints[10];

//  divx=PixelToReal(1);

	dikte = Object->x2;

	switch (Object->ObjectType)
	{
	case PIN_PUT_THROUGH_POLYGON:
	case PIN_SMD_POLYGON:
		dikte = 2e5;
		break;
	}

	switch (Object->ObjectType)
	{
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
		dikte = Object->y2;

	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_SMD_ROUND:
	case PIN_SMD_RECT:
	case PIN_PUT_THROUGH_SQUARE:
	case PIN_PUT_THROUGH_POLYGON:
	case PIN_SMD_POLYGON:
		x = MultX(Object->x1);
		y = MultY(Object->y1);

		if ((Mode & 1) == 0)
		{
			Height2 = Mult(dikte * 0.33333);
			x1b = x;
			y1b = y + Height2;
			x2b = x - Height2;
			y2b = y;
			x3b = x;
			y3b = y - Height2;
			x4b = x + Height2;
			y4b = y;
			LinePoints[0].x = (int32) x1b;
			LinePoints[0].y = (int32) y1b;
			LinePoints[1].x = (int32) x2b;
			LinePoints[1].y = (int32) y2b;
			LinePoints[2].x = (int32) x3b;
			LinePoints[2].y = (int32) y3b;
			LinePoints[3].x = (int32) x4b;
			LinePoints[3].y = (int32) y4b;
			LinePoints[4].x = (int32) x1b;
			LinePoints[4].y = (int32) y1b;
			Polygon(OutputDisplay, LinePoints, 5);

			if ((Mode & 2) == 2)
			{
				SetTextColor(OutputDisplay, RGB(0, 0, 0));
				sprintf(str, "%i", Number);
				le = strlen(str);
				TextOutUTF8(OutputDisplay, x - 4 - ((le / 2) * 8), y - 8, str, le);
			}
		}
		else
		{
			Height2 = Mult(dikte * 1.5);
			ellips2(x, y, Height2, Height2, 255);

		}

		break;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawObjectsWithColorMagenta(int32 Mode)
{
	int32 cnt;
	ObjectRecord *Object;

	StartDrawingEditingWindow();

	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_MAGENTA + DRAW_WITH_PEN_AND_NOT_FILLED);

//  NrObjects=min(1,NrObjects);
	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);
		DrawObject(Object, Mode | 2);
	}

	ExitDrawing();

	EndDrawingEditingWindow();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawObjects2WithColor(int32 Mode)
{
	int32 cnt;
	ObjectRecord *Object;

	StartDrawingEditingWindow();

	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_MAGENTA + DRAW_WITH_PEN_AND_NOT_FILLED);

	SetROP2(OutputDisplay, R2_COPYPEN);

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);
		DrawObject(Object, Mode | 2);
	}

	ExitDrawing();

	EndDrawingEditingWindow();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawObjects(int32 Mode)
{
	int32 cnt;
	ObjectRecord *Object;
	int32 TempBackGroundActive;

	TempBackGroundActive = BackGroundActive;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObject(Object, Mode);
	}
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawErrorObjects()
{
	int32 cnt, cnt2;
	ObjectRecord *Object, *Object2, NewObject;
	uint8 PolygonBuf2[10240], *AreaPos;
	double x1, y1, x2, y2, dd;
	PolygonRecord *PolygonObject, *AreaFillPolygon;
	AreaFillRecord *AreaFill;
#ifdef _DEBUG
	int32 ok;
#endif

	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	if ((NrErrorObjects == 0) || (!OkToDrawErrors))
		return;

	dd = 50000.0;
	SetROP2(OutputDisplay, R2_COPYPEN);

	if (CurrentErrorNr == -1)
	{
		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
				AreaFill->Info &= ~AREAFILL_DRAWN;
		}

		for (cnt = 0; cnt < NrErrorObjects; cnt++)
		{
			Object = &((*ErrorObjects)[cnt]);

			if ((cnt & 1) == 0)
				Object2 = &((*ErrorObjects)[cnt + 1]);
			else
				Object2 = &((*ErrorObjects)[cnt - 1]);

			if ((OkToDrawWarnings) || (((Object->Info2 & 1) == 0) && ((Object2->Info2 & 1) == 0)))
			{
				if (Object->ObjectType != AREAFILL)
				{
#ifdef _DEBUG

					if (Object->ObjectType == OBJECT_LINE)
						ok = 1;

#endif

					if ((Object->Info2 & 1) == 0)
						InitDrawingObject(0, ERROR_LAYER, 0, NORMAL_FILLED_AND_PEN1);
					else
						InitDrawingObject(0, WARNING_LAYER, 0, NORMAL_FILLED_AND_PEN1);

					switch (Object->ObjectType)
					{
					case OBJECT_LINE:
					case TRACE_ALL_ANGLE:
					case OBJECT_ARC:
					case TRACE_ARC:
					case PIN_ARC:
					case PIN_LINE_ALL_ANGLE:
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
						DrawPolygonDirect(PolygonObject, 0);
						break;

					case DRILL:
					case DRILL_UNPLATED:
						memmove(&NewObject, Object, sizeof(ObjectRecord));
						NewObject.ObjectType = PIN_PUT_THROUGH_ROUND;
						DrawObject(&NewObject, 0x140);
						break;

					case PIN_SMD_RECT:
						if (Object->ObjectType2 == OBJECT_TEXT2)
						{
							x1 = Object->x1;
							y1 = Object->y1;
							x2 = Object->x2 * 0.5;
							y2 = Object->y2 * 0.5;
							memmove(&NewObject, Object, sizeof(ObjectRecord));
							NewObject.Thickness = 0.2e5;
							NewObject.ObjectType = OBJECT_LINE;
							NewObject.x1 = x1 - x2;
							NewObject.y1 = y1 - y2;
							NewObject.x2 = x1 + x2;
							NewObject.y2 = y1 - y2;
							DrawObject(&NewObject, 0x140);
							NewObject.x1 = x1 + x2;
							NewObject.y1 = y1 - y2;
							NewObject.x2 = x1 + x2;
							NewObject.y2 = y1 + y2;
							DrawObject(&NewObject, 0x140);
							NewObject.x1 = x1 + x2;
							NewObject.y1 = y1 + y2;
							NewObject.x2 = x1 - x2;
							NewObject.y2 = y1 + y2;
							DrawObject(&NewObject, 0x140);
							NewObject.x1 = x1 - x2;
							NewObject.y1 = y1 + y2;
							NewObject.x2 = x1 - x2;
							NewObject.y2 = y1 - y2;
							DrawObject(&NewObject, 0x140);
						}
						else
						{
							memmove(&NewObject, Object, sizeof(ObjectRecord));

							if (NewObject.Layer >= ROUTING_KEEPOUT_LAYER)
								NewObject.Layer -= ROUTING_KEEPOUT_LAYER;

							DrawObject(&NewObject, 0x140);
						}

						break;

					default:
						memmove(&NewObject, Object, sizeof(ObjectRecord));

						if (NewObject.Layer >= ROUTING_KEEPOUT_LAYER)
							NewObject.Layer -= ROUTING_KEEPOUT_LAYER;

						DrawObject(&NewObject, 0x140);
						break;
					}
				}
				else
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object->TraceNr]]);

					if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | AREAFILL_DRAWN)) == 0)
					{
						AreaPos = (uint8 *) AreaFill;
						AreaFillPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

						if ((Object->Info2 & 1) == 0)
						{
//              InitDrawingObject(0,ERROR_LAYER,0,NORMAL_FILLED_AND_PEN1);
							InitDrawingObject(0, ERROR_LAYER, 3, DRAW_WITH_PEN_AND_NOT_FILLED);
						}
						else
						{
//              InitDrawingObject(0,WARNING_LAYER,0,NORMAL_FILLED_AND_PEN1);
							InitDrawingObject(0, WARNING_LAYER, 3, DRAW_WITH_PEN_AND_NOT_FILLED);
						}

						for (cnt2 = 0; cnt2 < AreaFillPolygon->NrVertices; cnt2++)
						{
							x1 = (*AreaFillPolygon).Points[cnt2].x;
							y1 = (*AreaFillPolygon).Points[cnt2].y;

							if (cnt2 < AreaFillPolygon->NrVertices - 1)
							{
								x2 = (*AreaFillPolygon).Points[cnt2 + 1].x;
								y2 = (*AreaFillPolygon).Points[cnt2 + 1].y;
							}
							else
							{
								x2 = (*AreaFillPolygon).Points[0].x;
								y2 = (*AreaFillPolygon).Points[0].y;
							}

							DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
							DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
						}
					}

					AreaFill->Info |= AREAFILL_DRAWN;
				}
			}

//      Object->Clearance=0.0;
//      MakePolygonFromObject(Object,PolygonObject,0.0,0,1);
//      DrawTestPolygon(PolygonObject,3);
		}

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
				AreaFill->Info &= ~AREAFILL_DRAWN;
		}
	}
	else
	{
		if (CurrentErrorNr * 2 < NrErrorObjects)
		{
			Object = &((*ErrorObjects)[CurrentErrorNr * 2]);
			Object2 = &((*ErrorObjects)[CurrentErrorNr * 2 + 1]);

//      FillPositionObject(Object);
//      FillPositionObject(Object2);
			if ((OkToDrawWarnings) || (((Object->Info2 & 1) == 0) && ((Object2->Info2 & 1) == 0)))
			{
				if (Object->ObjectType != AREAFILL)
				{
					if ((Object->Info2 & 1) == 0)
						InitDrawingObject(0, ERROR_LAYER, 0, NORMAL_FILLED_AND_PEN1);
					else
						InitDrawingObject(0, WARNING_LAYER, 0, NORMAL_FILLED_AND_PEN1);

#ifdef _DEBUG

					if (Object->ObjectType == DRILL)
						ok = 1;

#endif

					switch (Object->ObjectType)
					{
					case OBJECT_LINE:
					case TRACE_ALL_ANGLE:
					case OBJECT_ARC:
					case TRACE_ARC:
					case PIN_ARC:
					case PIN_LINE_ALL_ANGLE:
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
						DrawPolygonDirect(PolygonObject, 0);
						break;

					case DRILL:
					case DRILL_UNPLATED:
						memmove(&NewObject, Object, sizeof(ObjectRecord));
						NewObject.ObjectType = PIN_PUT_THROUGH_ROUND;
						DrawObject(&NewObject, 0x140);
						break;

					default:
						memmove(&NewObject, Object, sizeof(ObjectRecord));

						if (NewObject.Layer >= ROUTING_KEEPOUT_LAYER)
							NewObject.Layer -= ROUTING_KEEPOUT_LAYER;

						DrawObject(&NewObject, 0x140);
						break;
					}
				}
				else
				{
					if ((Object->Info2 & 1) == 0)
						InitDrawingObject(0, ERROR_LAYER, 0, NORMAL_FILLED_AND_PEN1);
					else
						InitDrawingObject(0, WARNING_LAYER, 0, NORMAL_FILLED_AND_PEN1);

					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object->TraceNr]]);
					AreaPos = (uint8 *) AreaFill;
					AreaFillPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

					for (cnt2 = 0; cnt2 < AreaFillPolygon->NrVertices; cnt2++)
					{
						x1 = (*AreaFillPolygon).Points[cnt2].x;
						y1 = (*AreaFillPolygon).Points[cnt2].y;

						if (cnt2 < AreaFillPolygon->NrVertices - 1)
						{
							x2 = (*AreaFillPolygon).Points[cnt2 + 1].x;
							y2 = (*AreaFillPolygon).Points[cnt2 + 1].y;
						}
						else
						{
							x2 = (*AreaFillPolygon).Points[0].x;
							y2 = (*AreaFillPolygon).Points[0].y;
						}

						DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
						DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
					}
				}

				if (Object2->ObjectType != AREAFILL)
				{
					if ((Object2->Info2 & 1) == 0)
						InitDrawingObject(0, ERROR_LAYER, 0, NORMAL_FILLED_AND_PEN1);
					else
						InitDrawingObject(0, WARNING_LAYER, 0, NORMAL_FILLED_AND_PEN1);

					switch (Object2->ObjectType)
					{
					case OBJECT_LINE:
					case TRACE_ALL_ANGLE:
					case OBJECT_ARC:
					case TRACE_ARC:
					case PIN_ARC:
					case PIN_LINE_ALL_ANGLE:
						MakePolygonFromObject(Object2, PolygonObject, 0.0, 0.0, 1, 1);
						DrawPolygonDirect(PolygonObject, 0);
						break;

					case DRILL:
					case DRILL_UNPLATED:
						memmove(&NewObject, Object2, sizeof(ObjectRecord));
						NewObject.ObjectType = PIN_PUT_THROUGH_ROUND;
						DrawObject(&NewObject, 0x140);
						break;

					default:
						memmove(&NewObject, Object2, sizeof(ObjectRecord));

						if (NewObject.Layer >= ROUTING_KEEPOUT_LAYER)
							NewObject.Layer -= ROUTING_KEEPOUT_LAYER;

						DrawObject(&NewObject, 0x140);
						break;
					}
				}
				else
				{
					if ((Object2->Info2 & 1) == 0)
					{
//              InitDrawingObject(0,ERROR_LAYER,0,NORMAL_FILLED_AND_PEN1);
						InitDrawingObject(0, ERROR_LAYER, 3, DRAW_WITH_PEN_AND_NOT_FILLED);
					}
					else
					{
//              InitDrawingObject(0,WARNING_LAYER,0,NORMAL_FILLED_AND_PEN1);
						InitDrawingObject(0, WARNING_LAYER, 3, DRAW_WITH_PEN_AND_NOT_FILLED);
					}

					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object2->TraceNr]]);
					AreaPos = (uint8 *) AreaFill;
					AreaFillPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

					for (cnt2 = 0; cnt2 < AreaFillPolygon->NrVertices; cnt2++)
					{
						x1 = (*AreaFillPolygon).Points[cnt2].x;
						y1 = (*AreaFillPolygon).Points[cnt2].y;

						if (cnt2 < AreaFillPolygon->NrVertices - 1)
						{
							x2 = (*AreaFillPolygon).Points[cnt2 + 1].x;
							y2 = (*AreaFillPolygon).Points[cnt2 + 1].y;
						}
						else
						{
							x2 = (*AreaFillPolygon).Points[0].x;
							y2 = (*AreaFillPolygon).Points[0].y;
						}

						DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
						DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
					}
				}
			}
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawCompPinObjects(CompRecord * Comp, int32 mode)
{
	int32 cnt, Layer, ThroughHoleDrawLayer, NewDrawLayer, UnconnectedPadMode, Hilited = 0;
	ObjectRecord *Object;
	int32 TempBackGroundActive;
	NetRecord *Net;

	Layer = CurrentDrawingLayer;

	if (Layer == -1)
		Layer = Design.NrBoardLayers - 1;

#ifdef _DEBUG

	if (mode & 2)
		ok = 1;

#endif
	ThroughHoleDrawLayer = GetLayerToDrawOn(Layer, 0);

#ifdef _DEBUG

	if (stricmp(Comp->Name, "U1") == 0)
	{
		ok = 1;

		if ((mode & 0x100) == 0)
			ok = 1;
	}

#endif

	TempBackGroundActive = BackGroundActive;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		Object->Info &= ~OBJECT_DONE;
	}

	if ((mode & 0x302) == 0x100)
	{
// ****************************************************************************************************
// ****************************************************************************************************
// Drawing pads layer non active layer first, and no through hole pins
		TempBackGroundActive = BackGroundActive;

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if ((Object->Layer == -1) || (Object->Layer == Layer))
				continue;

			Hilited = 0;

			if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Object->NetNr]);

				if (Net->Info & OBJECT_HIGHLITED)
					Hilited = 0x1000;
			}

			switch (Object->ObjectType)
			{
			case OBJECT_POLYGON:
			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
#ifdef _DEBUG
				if (stricmpOwn(Comp->Name, "J3") == 0)
				{
					ok = 1;

					if (Object->ObjectType == PIN_PUT_THROUGH_POLYGON)
						ok = 1;

					if (Object->ObjectType == PIN_PUT_THROUGH_SQUARE)
						ok = 1;
				}

				if (Object->ObjectType == PIN_PUT_THROUGH_POLYGON)
					ok = 1;

				if (cnt == 8)
					ok = 1;

#endif
				UnconnectedPadMode = 0;

				if (Object->PinCount == 1)
				{
					UnconnectedPadMode = 0x80;
					Hilited = 0;
				}

				// Check pads on a fixed layer
				NewDrawLayer = GetLayerToDrawOn(Object->Layer, 0);

				switch (NewDrawLayer)
				{
				case SHAPE_PINS_BOTTOM:
					if (TempBackGroundActive)
						SetBackGroundActive(0);

					DrawObject(Object, mode | 0x400 | UnconnectedPadMode | Hilited);
					Object->Info |= OBJECT_DONE;
					break;

				case SHAPE_PINS_TOP:
					if (TempBackGroundActive)
						SetBackGroundActive(0);

					DrawObject(Object, mode | 0x800 | UnconnectedPadMode | Hilited);
					Object->Info |= OBJECT_DONE;
					break;

				case SHAPE_PINS_INNER:
					if (TempBackGroundActive)
						SetBackGroundActive(0);

					DrawObject(Object, mode | 0xC00 | UnconnectedPadMode | Hilited);
					Object->Info |= OBJECT_DONE;
					break;
				}

				break;
			}
		}

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);
#ifdef _DEBUG

			if (Object->ObjectType == PIN_LINE_ALL_ANGLE)
				ok = 1;

#endif

			if ((Object->Layer == -1) || (Object->Layer == Layer))
				continue;

			Hilited = 0;

			if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Object->NetNr]);

				if (Net->Info & OBJECT_HIGHLITED)
					Hilited = 0x1000;
			}

			switch (Object->ObjectType)
			{
			case OBJECT_POLYGON:
			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
				break;

			default:
				UnconnectedPadMode = 0;

				if (Object->PinCount == 1)
				{
					UnconnectedPadMode = 0x80;
					Hilited = 0;
				}

				NewDrawLayer = GetLayerToDrawOn(Object->Layer, 0);

				switch (NewDrawLayer)
				{
				case SHAPE_PINS_BOTTOM:
					if (TempBackGroundActive)
						SetBackGroundActive(0);

					DrawObject(Object, mode | 0x400 | UnconnectedPadMode | Hilited);
					Object->Info |= OBJECT_DONE;
					break;

				case SHAPE_PINS_TOP:
					if (TempBackGroundActive)
						SetBackGroundActive(0);

#ifdef _DEBUG

					if (stricmp(Comp->Name, "U1") == 0)
						ok = 1;

#endif
					DrawObject(Object, mode | 0x800 | UnconnectedPadMode | Hilited);
					Object->Info |= OBJECT_DONE;
					break;

				case SHAPE_PINS_INNER:
					if (TempBackGroundActive)
						SetBackGroundActive(0);

					DrawObject(Object, mode | 0xC00 | UnconnectedPadMode | Hilited);
					Object->Info |= OBJECT_DONE;
					break;
				}

				break;
			}
		}

		return;
	}

// ****************************************************************************************************
// ****************************************************************************************************
// Drawing pads active layer and through hole pins
#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "R8") == 0)
		ok = 1;

#endif

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
#ifdef _DEBUG

		if (Object->ObjectType == PIN_LINE_ALL_ANGLE)
			ok = 1;

#endif

		if ((mode & 0x200) == 0)
		{
			if ((Object->Layer != -1) && (Object->Layer != Layer))
				continue;
		}

		Hilited = 0;

		if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
		{
			Net = &((*Nets)[Object->NetNr]);

			if (Net->Info & OBJECT_HIGHLITED)
				Hilited = 0x1000;
		}

		switch (Object->ObjectType)
		{
		case OBJECT_POLYGON:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			UnconnectedPadMode = 0;

			if (Object->PinCount == 1)
			{
				UnconnectedPadMode = 0x80;
				Hilited = 0;
			}

			if (Object->Layer == -1)
				NewDrawLayer = ThroughHoleDrawLayer;
			else
				NewDrawLayer = GetLayerToDrawOn(Object->Layer, 0);

			switch (NewDrawLayer)
			{
			case SHAPE_PINS_BOTTOM:
				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, mode | 0x400 | UnconnectedPadMode | Hilited);
				Object->Info |= OBJECT_DONE;
				break;

			case SHAPE_PINS_TOP:
				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, mode | 0x800 | UnconnectedPadMode | Hilited);
				Object->Info |= OBJECT_DONE;
				break;

			case SHAPE_PINS_INNER:
				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, mode | 0xC00 | UnconnectedPadMode | Hilited);
				Object->Info |= OBJECT_DONE;
				break;
			}

			break;
		}
	}

#ifdef _DEBUG

	if (mode & 2)
		ok = 1;

#endif

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
#ifdef _DEBUG

		if (Object->ObjectType == PIN_LINE_ALL_ANGLE)
			ok = 1;

#endif

		if ((mode & 0x200) == 0)
		{
			if ((Object->Layer != -1) && (Object->Layer != Layer))
				continue;
		}

		Hilited = 0;

		if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
		{
			Net = &((*Nets)[Object->NetNr]);

			if (Net->Info & OBJECT_HIGHLITED)
				Hilited = 0x1000;
		}

		switch (Object->ObjectType)
		{
		case OBJECT_POLYGON:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			break;

		default:
			UnconnectedPadMode = 0;

			if (Object->PinCount == 1)
			{
				UnconnectedPadMode = 0x80;
				Hilited = 0;
			}

			if (Object->Layer == -1)
				NewDrawLayer = ThroughHoleDrawLayer;
			else
				NewDrawLayer = GetLayerToDrawOn(Object->Layer, 0);

			switch (NewDrawLayer)
			{
			case SHAPE_PINS_BOTTOM:
				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, mode | 0x400 | UnconnectedPadMode | Hilited);
				Object->Info |= OBJECT_DONE;
				break;

			case SHAPE_PINS_TOP:
				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, mode | 0x800 | UnconnectedPadMode | Hilited);
				Object->Info |= OBJECT_DONE;
				break;

			case SHAPE_PINS_INNER:
				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, mode | 0xC00 | UnconnectedPadMode | Hilited);
				Object->Info |= OBJECT_DONE;
				break;
			}

			break;
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingThroughHolePinLayer(int32 NewDrawLayer, int32 Mode)
{
	int32 Hilited = 0;

	if (Mode & 0x1000)
		Hilited = 1;

	if ((Mode & 0x40) == 0)
	{	// Initdrawing
		if ((Mode & 2) == 0)
		{	// Normal drawing
			if ((Mode & 0x80) == 0)
			{	// Normal drawing
				if (!Hilited)
				{
					switch (Mode & 0xC00)
					{
					case 0x400:	// Bottom layer
						InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, NORMAL_FILLED_AND_PEN1);
						break;

					case 0x800:	// Top layer
						InitDrawingObject(0, SHAPE_PINS_TOP, 0, NORMAL_FILLED_AND_PEN1);
						break;

					case 0xC00:	// Inner layer
						InitDrawingObject(0, SHAPE_PINS_INNER, 0, NORMAL_FILLED_AND_PEN1);
						break;
					}
				}
				else
				{
					switch (Mode & 0xC00)
					{
					case 0x400:	// Bottom layer
						InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, HILITED_NORMAL_FILLED_AND_PEN1);
						break;

					case 0x800:	// Top layer
						InitDrawingObject(0, SHAPE_PINS_TOP, 0, HILITED_NORMAL_FILLED_AND_PEN1);
						break;

					case 0xC00:	// Inner layer
						InitDrawingObject(0, SHAPE_PINS_INNER, 0, HILITED_NORMAL_FILLED_AND_PEN1);
						break;
					}
				}
			}
			else
			{	// Unconnected pads fill
				switch (Mode & 0xC00)
				{
				case 0x400:	// Bottom layer
					InitDrawingObject(0, UNCONNECTED_PADS_BOTTOM_LAYER, 0, NORMAL_FILLED_AND_PEN1);
					break;

				case 0x800:	// Top layer
					InitDrawingObject(0, UNCONNECTED_PADS_TOP_LAYER, 0, NORMAL_FILLED_AND_PEN1);
					break;

				case 0xC00:	// Inner layer
					InitDrawingObject(0, UNCONNECTED_PADS_INNER_LAYER, 0, NORMAL_FILLED_AND_PEN1);
					break;
				}
			}
		}
		else
		{	// XOR drawing -> Null pen
			switch (Mode & 0xC00)
			{
			case 0x400:		// Bottom layer
				InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, NORMAL_FILLED_AND_NO_PEN);
				break;

			case 0x800:		// Top layer
				InitDrawingObject(0, SHAPE_PINS_TOP, 0, NORMAL_FILLED_AND_NO_PEN);
				break;

			case 0xC00:		// Inner layer
				InitDrawingObject(0, SHAPE_PINS_INNER, 0, NORMAL_FILLED_AND_NO_PEN);
				break;
			}
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void InitDrawingSmdPinLayer(int32 NewDrawLayer, int32 Thickness, int32 Mode)
{
	int32 Hilited = 0;

	if (Mode & 0x1000)
		Hilited = 1;

	if ((Mode & 0x40) == 0)
	{	// No initdrawing
		if ((Mode & 2) == 0)
		{	// Normal drawing
			if ((Mode & 0x80) == 0)
			{	// Normal drawing
				if (Thickness == 0)
				{
					if (!Hilited)
					{
						switch (NewDrawLayer)
						{
						case SHAPE_PINS_BOTTOM:
							InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, NORMAL_FILLED_AND_PEN1);
							break;

						case SHAPE_PINS_TOP:
							InitDrawingObject(0, SHAPE_PINS_TOP, 0, NORMAL_FILLED_AND_PEN1);
							break;

						case SHAPE_PINS_INNER:
							InitDrawingObject(0, SHAPE_PINS_INNER, 0, NORMAL_FILLED_AND_PEN1);
							break;
						}
					}
					else
					{
						switch (NewDrawLayer)
						{
						case SHAPE_PINS_BOTTOM:
							InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, HILITED_NORMAL_FILLED_AND_PEN1);
							break;

						case SHAPE_PINS_TOP:
							InitDrawingObject(0, SHAPE_PINS_TOP, 0, HILITED_NORMAL_FILLED_AND_PEN1);
							break;

						case SHAPE_PINS_INNER:
							InitDrawingObject(0, SHAPE_PINS_INNER, 0, HILITED_NORMAL_FILLED_AND_PEN1);
							break;
						}
					}
				}
				else
				{
					switch (NewDrawLayer)
					{
					case SHAPE_PINS_BOTTOM:
						InitDrawingObject(0, SHAPE_PINS_BOTTOM, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
						break;

					case SHAPE_PINS_TOP:
						InitDrawingObject(0, SHAPE_PINS_TOP, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
						break;

					case SHAPE_PINS_INNER:
						InitDrawingObject(0, SHAPE_PINS_INNER, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
						break;
					}
				}
			}
			else
			{	// Unconnected pads fill
				if (Thickness == 0)
				{
					switch (NewDrawLayer)
					{
					case SHAPE_PINS_BOTTOM:
						InitDrawingObject(0, UNCONNECTED_PADS_BOTTOM_LAYER, 0, NORMAL_FILLED_AND_PEN1);
						break;

					case SHAPE_PINS_TOP:
						InitDrawingObject(0, UNCONNECTED_PADS_TOP_LAYER, 0, NORMAL_FILLED_AND_PEN1);
						break;

					case SHAPE_PINS_INNER:
						InitDrawingObject(0, UNCONNECTED_PADS_INNER_LAYER, 0, NORMAL_FILLED_AND_PEN1);
						break;
					}
				}
				else
				{
					switch (NewDrawLayer)
					{
					case SHAPE_PINS_BOTTOM:
						InitDrawingObject(0, UNCONNECTED_PADS_BOTTOM_LAYER, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
						break;

					case SHAPE_PINS_TOP:
						InitDrawingObject(0, UNCONNECTED_PADS_TOP_LAYER, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
						break;

					case SHAPE_PINS_INNER:
						InitDrawingObject(0, UNCONNECTED_PADS_INNER_LAYER, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
						break;
					}
				}
			}
		}
		else
		{
			if (Thickness == 0)
			{
				if (!Hilited)
				{
					switch (NewDrawLayer)
					{
					case SHAPE_PINS_BOTTOM:
						InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, NORMAL_FILLED_AND_NO_PEN);
						break;

					case SHAPE_PINS_TOP:
						InitDrawingObject(0, SHAPE_PINS_TOP, 0, NORMAL_FILLED_AND_NO_PEN);
						break;

					case SHAPE_PINS_INNER:
						InitDrawingObject(0, SHAPE_PINS_INNER, 0, NORMAL_FILLED_AND_NO_PEN);
						break;
					}
				}
				else
				{
					switch (NewDrawLayer)
					{
					case SHAPE_PINS_BOTTOM:
						InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, HILITED_NORMAL_FILLED_AND_NO_PEN);
						break;

					case SHAPE_PINS_TOP:
						InitDrawingObject(0, SHAPE_PINS_TOP, 0, HILITED_NORMAL_FILLED_AND_NO_PEN);
						break;

					case SHAPE_PINS_INNER:
						InitDrawingObject(0, SHAPE_PINS_INNER, 0, HILITED_NORMAL_FILLED_AND_NO_PEN);
						break;
					}
				}
			}
			else
			{
				switch (NewDrawLayer)
				{
				case SHAPE_PINS_BOTTOM:
					InitDrawingObject(0, SHAPE_PINS_BOTTOM, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
					break;

				case SHAPE_PINS_TOP:
					InitDrawingObject(0, SHAPE_PINS_TOP, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
					break;

				case SHAPE_PINS_INNER:
					InitDrawingObject(0, SHAPE_PINS_INNER, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED);
					break;
				}
			}
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawPinPutThroughRound(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01   = 1     Draw with double line
	          0x02   = 2     Xor
	          0x04   = 4     Background
	          0x08   = 8     clearance
	          0x10   = 16    Trace xor
	          0x20   = 32    Trace hatch
	          0x40   = 64    No initdrawing
	          0x80   = 0x80  Unconnected apd
	          0x100  = 0x100 Error objects
	          0x600  = 0x000 Draw pads on normal layer
	          0x600  = 0x200 Draw pads on bottom layer
	          0x600  = 0x400 Draw pads on top    layer
	          0x600  = 0x600 Draw pads on inner  layer
	          0x1000 = 0x1000 Hilited
	*/

	double xx1, yy1, xx2, yy2, xx3, Xmax, Xmin, Ymax, Ymin, yy2a, xxx2, yyy2, Clearance, Thickness;
	int32 x1a, y1a;

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;
	xx3 = Object->x3;

	x1a = MultX(xx1);
	y1a = MultY(yy1);
	Clearance = Object->Clearance;
	Thickness = Object->Thickness;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	if ((Mode & 0xC00) == 0xC00)
	{	// Draw inner pads
		if (NotInRange(xx3, 0.0))
		{
			xx2 = xx3;
			xxx2 = xx3;

			if ((Mode & 8) == 8)
				xxx2 += Clearance * 2;
		}
	}

	yy2a = xx2 / 2;

	if ((Mode & 8) == 8)
		yy2a += Clearance;

	Xmin = xx1 - yy2a;
	Ymin = yy1 - yy2a;
	Xmax = xx1 + yy2a;
	Ymax = yy1 + yy2a;

	if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
		return;

	InitDrawingThroughHolePinLayer(0, Mode);
	ellips2(x1a, y1a, Mult(xxx2), Mult(xxx2), 255);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawPinPutThroughSquare(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01   = 1     Draw with double line
	          0x02   = 2     Xor
	          0x04   = 4     Background
	          0x08   = 8     clearance
	          0x10   = 16    Trace xor
	          0x20   = 32    Trace hatch
	          0x40   = 64    No initdrawing
	          0x80   = 0x80  Unconnected apd
	          0x100  = 0x100 Error objects
	          0x600  = 0x000 Draw pads on normal layer
	          0x600  = 0x200 Draw pads on bottom layer
	          0x600  = 0x400 Draw pads on top    layer
	          0x600  = 0x600 Draw pads on inner  layer
	          0x1000 = 0x1000 Hilited
	*/

	double xx1, yy1, xx2, yy2, xx3, Xmax, Xmin, Ymax, Ymin, yy2a, xxx2, yyy2, Clearance, Thickness;
	int32 x1a, y1a;
	int32 DrawCircle = 0;

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;
	xx3 = Object->x3;

	x1a = MultX(xx1);
	y1a = MultY(yy1);
	Clearance = Object->Clearance;
	Thickness = Object->Thickness;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	if ((Mode & 0xC00) == 0xC00)
	{	// Draw inner pads
		DrawCircle = 1;
		xx2 = xx3;
		xxx2 = xx3;
		xx2 = Object->x3;
		xxx2 = xx2;

		if ((Mode & 8) == 8)
			xxx2 += Clearance * 2;
	}

	yy2a = xx2 / 2;

	if ((Mode & 8) == 8)
		yy2a += Clearance;

	Xmin = xx1 - yy2a;
	Ymin = yy1 - yy2a;
	Xmax = xx1 + yy2a;
	Ymax = yy1 + yy2a;

	if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
		return;

	InitDrawingThroughHolePinLayer(0, Mode);

	if (!DrawCircle)
		rect3(x1a, y1a, Mult(xxx2), Mult(xxx2));
	else
		ellips2(x1a, y1a, Mult(xxx2), Mult(xxx2), 255);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawPinPutThroughPolygon(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01   = 1     Draw with double line
	          0x02   = 2     Xor
	          0x04   = 4     Background
	          0x08   = 8     clearance
	          0x10   = 16    Trace xor
	          0x20   = 32    Trace hatch
	          0x40   = 64    No initdrawing
	          0x80   = 0x80  Unconnected apd
	          0x100  = 0x100 Error objects
	          0x600  = 0x000 Draw pads on normal layer
	          0x600  = 0x200 Draw pads on bottom layer
	          0x600  = 0x400 Draw pads on top    layer
	          0x600  = 0x600 Draw pads on inner  layer
	          0x1000 = 0x1000 Hilited
	*/

	double xx1, yy1, xx2, yy2, xx3, Xmax, Xmin, Ymax, Ymin, yy2a, xxx2, yyy2, Clearance, Thickness;
	int32 x1a, y1a;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;
	AreaFillRecord *AreaFill;

	PolygonObject = (PolygonRecord *) & PolygonBuf;
	/*
	  NewDrawLayer=GetLayerToDrawOn(Object->Layer,1);
	  if (NewDrawLayer<=0) {
	    return;
	  }
	*/
	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;
	xx3 = Object->x3;

	x1a = MultX(xx1);
	y1a = MultY(yy1);
	Clearance = Object->Clearance;
	Thickness = Object->Thickness;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	if ((Mode & 0xC00) == 0xC00)
	{	// Draw inner pads
		if (NotInRange(xx3, (float) 0.0))
		{
			xx2 = xx3;
			xxx2 = xx3;
			xx2 = Object->x3;
			xxx2 = xx2;

			if ((Mode & 8) == 8)
				xxx2 += Clearance * 2;
		}

		yy2a = xx2 / 2;

		if ((Mode & 1) == 1)
			yy2a += Clearance;

		Xmin = xx1 - yy2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + yy2a;
		Ymax = yy1 + yy2a;

		if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
			return;

		InitDrawingThroughHolePinLayer(0, Mode);
		ellips2(x1a, y1a, Mult(xxx2), Mult(xxx2), 255);
		return;
	}

	if (CheckObjectIsBigPolygon(Object))
	{
		if ((Mode & 8) == 8)
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, Object->Clearance, 0);
		else
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);

		InitDrawingThroughHolePinLayer(0, Mode);

		if ((Mode & 8) == 8)
			DrawPinAreaFill(AreaFill, 2);
		else
			DrawPinAreaFill(AreaFill, 0);

		return;
	}

	if ((Mode & 8) == 8)
		MakePolygonFromObject(Object, PolygonObject, Object->Clearance, 0.0, 1, 1);
	else
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

	Xmin = PolygonObject->minx;
	Ymin = PolygonObject->miny;
	Xmax = PolygonObject->maxx;
	Ymax = PolygonObject->maxy;

	if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
		return;

	InitDrawingThroughHolePinLayer(0, Mode);
	DrawFilledPolygon(PolygonObject, 0);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawViaPutThroughRound(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01  = 1     Draw with double line
	          0x02  = 2     Xor
	          0x04  = 4     Background
	          0x08  = 8     clearance
	          0x10  = 16    Trace xor
	          0x20  = 32    Trace hatch
	          0x40  = 64    No initdrawing
	          0x80  = 0x80  Unconnected apd
	          0x100 = 0x100 Error objects
	          0x600 = 0x000 Draw pads on normal layer
	          0x600 = 0x200 Draw pads on bottom layer
	          0x600 = 0x400 Draw pads on top    layer
	          0x600 = 0x600 Draw pads on inner  layer
	*/

	double xx1, yy1, xx2, yy2, xx3, Xmax, Xmin, Ymax, Ymin, yy2a, xxx2, yyy2, Clearance, Thickness;
	int32 x1a, y1a;

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;
	xx3 = Object->x3;

	x1a = MultX(xx1);
	y1a = MultY(yy1);
	Clearance = Object->Clearance;
	Thickness = Object->Thickness;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	if (!OkToDrawVias)
		return;

	yy2a = xx2 / 2;

	if ((Mode & 8) == 8)
		yy2a += Clearance;

	Xmin = xx1 - yy2a;
	Ymin = yy1 - yy2a;
	Xmax = xx1 + yy2a;
	Ymax = yy1 + yy2a;

	if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
		return;

	if ((Mode & 0x40) == 0)
	{	// No initdrawing
		if ((Mode & 0x20) == 0x20)
		{	// Trace hatch
			if ((Object->Info & OBJECT_HIGHLITED) == 0)
				InitDrawingObject(0, VIA_LAYER, 0, IN_NET_FILLED_AND_NO_PEN);
			else
				InitDrawingObject(0, VIA_LAYER, 0, HILITED_IN_NET_FILLED_AND_NO_PEN);
		}

		if ((Mode & 0x20) == 0)
		{
			if ((Object->Info & OBJECT_HIGHLITED) == 0)
				InitDrawingObject(0, VIA_LAYER, 0, NORMAL_FILLED_AND_NO_PEN);
			else
				InitDrawingObject(0, VIA_LAYER, 0, HILITED_NORMAL_FILLED_AND_NO_PEN);
		}
	}

	/*
	  if ((Mode & 2)==2) {
	    InitDrawingEmptyPen();
	  }
	*/
	ellips2(x1a, y1a, Mult(xxx2), Mult(xxx2), 255);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawSmdRound(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01   = 1     Draw with double line
	          0x02   = 2     Xor
	          0x04   = 4     Background
	          0x08   = 8     clearance
	          0x10   = 16    Trace xor
	          0x20   = 32    Trace hatch
	          0x40   = 64    No initdrawing
	          0x80   = 0x80  Unconnected apd
	          0x100  = 0x100 Error objects
	          0x600  = 0x000 Draw pads on normal layer
	          0x600  = 0x200 Draw pads on bottom layer
	          0x600  = 0x400 Draw pads on top    layer
	          0x600  = 0x600 Draw pads on inner  layer
	          0x1000 = 0x1000 Hilited
	*/

	double xx1, yy1, xx2, yy2, xx3, Xmax, Xmin, Ymax, Ymin, yy2a, xxx2, yyy2, Clearance, Thickness;
	int32 x1a, y1a, x2a, NewDrawLayer;

	NewDrawLayer = GetLayerToDrawOn(Object->Layer, 1);

	if (NewDrawLayer <= 0)
		return;

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;
	xx3 = Object->x3;

	x1a = MultX(xx1);
	y1a = MultY(yy1);
	Clearance = Object->Clearance;
	Thickness = Object->Thickness;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	yy2a = xx2 / 2;

	if ((Mode & 8) == 8)
		yy2a += Clearance;

	Xmin = xx1 - yy2a;
	Ymin = yy1 - yy2a;
	Xmax = xx1 + yy2a;
	Ymax = yy1 + yy2a;

	if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
		return;

	x2a = max(1, Mult(xxx2));
	InitDrawingSmdPinLayer(NewDrawLayer, 0, Mode);

	ellips2(x1a, y1a, x2a, x2a, 255);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawSmdRect(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01   = 1     Draw with double line
	          0x02   = 2     Xor
	          0x04   = 4     Background
	          0x08   = 8     clearance
	          0x10   = 16    Trace xor
	          0x20   = 32    Trace hatch
	          0x40   = 64    No initdrawing
	          0x80   = 0x80  Unconnected apd
	          0x100  = 0x100 Error objects
	          0x600  = 0x000 Draw pads on normal layer
	          0x600  = 0x200 Draw pads on bottom layer
	          0x600  = 0x400 Draw pads on top    layer
	          0x600  = 0x600 Draw pads on inner  layer
	          0x1000 = 0x1000 Hilited
	*/

	double xx1, yy1, xx2, yy2, Xmax, Xmin, Ymax, Ymin, xx3, xx2a, yy2a, xxx2, yyy2, Clearance, Thickness;
	int32 x1a, y1a, NewDrawLayer;

	NewDrawLayer = GetLayerToDrawOn(Object->Layer, 1);

	if (NewDrawLayer <= 0)
		return;

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;
	xx3 = Object->x3;

	x1a = MultX(xx1);
	y1a = MultY(yy1);
	Clearance = Object->Clearance;
	Thickness = Object->Thickness;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}


	xx2a = xx2 / 2;
	yy2a = yy2 / 2;

	if ((Mode & 8) == 8)
	{
		xx2a += Clearance;
		yy2a += Clearance;
	}

	Xmin = xx1 - xx2a;
	Ymin = yy1 - yy2a;
	Xmax = xx1 + xx2a;
	Ymax = yy1 + yy2a;

	if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
		return;

	InitDrawingSmdPinLayer(NewDrawLayer, 0, Mode);
	rect3(x1a, y1a, Mult(xxx2), Mult(yyy2));
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawSmdTraceLine(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01   = 1     Draw with double line
	          0x02   = 2     Xor
	          0x04   = 4     Background
	          0x08   = 8     clearance
	          0x10   = 16    Trace xor
	          0x20   = 32    Trace hatch
	          0x40   = 64    No initdrawing
	          0x80   = 0x80  Unconnected apd
	          0x100  = 0x100 Error objects
	          0x600  = 0x000 Draw pads on normal layer
	          0x600  = 0x200 Draw pads on bottom layer
	          0x600  = 0x400 Draw pads on top    layer
	          0x600  = 0x600 Draw pads on inner  layer
	          0x1000 = 0x1000 Hilited
	*/

	double xx1, yy1, xx2, yy2, yy2a, yy2b, xx3, yy3, xx4, yy4, Xmax, Xmin, Ymax, Ymin, xxx2, yyy2, Clearance, Thickness;
	int32 x1a, y1a, x2a, y2a, yyy2a, NewDrawLayer;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;

	NewDrawLayer = GetLayerToDrawOn(Object->Layer, 1);

	if (NewDrawLayer <= 0)
		return;

	PolygonObject = (PolygonRecord *) & PolygonBuf;
	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;
	xx3 = Object->x3;
	yy3 = 0.0;
	xx4 = 0.0;
	yy4 = 0.0;
	x1a = MultX(xx1);
	y1a = MultY(yy1);
	Clearance = Object->Clearance;
	Thickness = Object->Thickness;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	if ((Mode & 8) == 8)
	{
		yy2 += Clearance * 2;
		yyy2a = Mult(yyy2);

		if (yyy2a <= 8)
			return;
	}

	Xmin = 0.0;
	Ymin = 0.0;
	Xmax = 0.0;
	Ymax = 0.0;
	yy2a = yy2 / 2;

	switch (Object->ObjectType)
	{
	case PIN_LINE_HOR:
		Xmin = xx1 - yy2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2 + yy2a;
		Ymax = yy1 + yy2a;
		break;

	case PIN_LINE_VER:
		Xmin = xx1 - yy2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + yy2a;
		Ymax = yy1 + xx2 + yy2a;
		break;

	case PIN_LINE_DIAG1:
		Xmin = xx1 - yy2;
		yy2b = yy1 - xx2;
		Ymin = yy2b - yy2a;
		Xmax = xx1 + xx2 + yy2a;
		Ymax = yy1 + yy2a;
		break;

	case PIN_LINE_DIAG2:
		Xmin = xx1 - yy2a;
		yy2b = yy1 + xx2;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2 + yy2a;
		Ymax = yy2b + yy2a;
		break;

	case PIN_LINE_ALL_ANGLE:
		if ((Mode & 8) == 8)
		{
			Thickness += Clearance * 2;

			if (Mult(Thickness) <= 8)
				break;
		}

		Xmin = min(xx1, xx2) - Thickness * 0.5;
		Ymin = min(yy1, yy2) - Thickness * 0.5;
		Xmax = max(xx1, xx2) + Thickness * 0.5;
		Ymax = max(yy1, yy2) + Thickness * 0.5;
		break;

	case PIN_ARC:
		if ((Mode & 8) == 8)
		{
			Thickness += Clearance * 2;

			if (Mult(Thickness) <= 8)
				break;
		}

		xx3 = Object->x3;
		yy3 = Object->y3;
		xx4 = Object->x4;
		yy4 = Object->y4;
		Xmin = xx1 - (max(xx2, yy2) + Thickness) * 0.5;
		Ymin = yy1 - (max(xx2, yy2) + Thickness) * 0.5;
		Xmax = xx1 + (max(xx2, yy2) + Thickness) * 0.5;
		Ymax = yy1 + (max(xx2, yy2) + Thickness) * 0.5;
		break;
	}

	if ((Xmax < ViewMinX) || (Xmin > ViewMaxX) || (Ymax < ViewMinY) || (Ymin > ViewMaxY))
		return;

	InitDrawingSmdPinLayer(NewDrawLayer, 0, Mode);

	switch (Object->ObjectType)
	{
	case PIN_LINE_HOR:
		if ((Mode & 0x2) != 0)
		{	// xor
			DrawHorLine(x1a, Mult(yy1 - Yoffset), MultX(xx1 + xx2), Mult(yyy2), 1);
		}
		else
			DrawHorLine(x1a, Mult(yy1 - Yoffset), MultX(xx1 + xx2), Mult(yyy2), 0);

		break;

	case PIN_LINE_VER:
		if ((Mode & 0x2) != 0)
		{	// xor
			DrawVerLine(x1a, Mult(yy1 - Yoffset), Mult(yy1 + xx2 - Yoffset), Mult(yyy2), 1);
		}
		else
			DrawVerLine(x1a, Mult(yy1 - Yoffset), Mult(yy1 + xx2 - Yoffset), Mult(yyy2), 0);

		break;

	case PIN_LINE_DIAG1:
		if ((Mode & 0x2) != 0)
		{	// xor
			DrawDiag1Line(x1a, Mult(yy1 - Yoffset), MultX(xx1 + xx2), Mult(yyy2), 1);
		}
		else
			DrawDiag1Line(x1a, Mult(yy1 - Yoffset), MultX(xx1 + xx2), Mult(yyy2), 0);

		break;

	case PIN_LINE_DIAG2:
		if ((Mode & 0x2) != 0)
		{	// xor
			DrawDiag2Line(x1a, Mult(yy1 - Yoffset), MultX(xx1 + xx2), Mult(yyy2), 1);
		}
		else
			DrawDiag2Line(x1a, Mult(yy1 - Yoffset), MultX(xx1 + xx2), Mult(yyy2), 0);

		break;

	case PIN_LINE_ALL_ANGLE:
		x2a = MultX(xx2);
		y2a = MultY(yy2);

		if ((Mode & 0x80) == 0)
		{
			if ((Mode & 8) == 0)
			{
//              MakePolygonFromObject(Object,PolygonObject,0.0,0.0,1,1);
//              DrawFilledPolygon(PolygonObject,0);
				InitDrawingSmdPinLayer(NewDrawLayer, Mult(Thickness), Mode);
				DrawLine(x1a, y1a, x2a, y2a);
			}
			else
			{
				MakePolygonFromObject(Object, PolygonObject, Clearance, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);
			}
		}
		else
		{
			if ((Mode & 8) == 0)
				MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
			else
				MakePolygonFromObject(Object, PolygonObject, Clearance, 0.0, 1, 1);

			DrawFilledPolygon(PolygonObject, 0);
		}

		break;

	case PIN_ARC:
		if ((Mode & 0x80) == 0)
		{
			if ((Mode & 8) == 0)
			{
				InitDrawingSmdPinLayer(NewDrawLayer, Mult(Thickness), Mode);
				SpecialArc(x1a, y1a, Mult(xx2) + 1, Mult(yy2) + 1, MultX(xx1 + xx3), MultY(yy1 + yy3), MultX(xx1 + xx4),
				           MultY(yy1 + yy4));
//                MakePolygonFromObject(Object,PolygonObject,0.0,0.0,1,1);
//                DrawFilledPolygon(PolygonObject,0);
			}
			else
			{
				MakePolygonFromObject(Object, PolygonObject, Clearance, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);
			}
		}
		else
		{
			if ((Mode & 8) == 0)
				MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
			else
				MakePolygonFromObject(Object, PolygonObject, Clearance, 0.0, 1, 1);

			DrawFilledPolygon(PolygonObject, 0);
		}

		break;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawSmdPolygon(ObjectRecord * Object, int32 Mode)
{
	/*
	  Mode  = 0x01  = 1     Draw with double line
	          0x02  = 2     Xor
	          0x04  = 4     Background
	          0x08  = 8     clearance
	*/

	int32 NewDrawLayer;
	AreaFillRecord *AreaFill;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;

	NewDrawLayer = GetLayerToDrawOn(Object->Layer, 1);

	if (NewDrawLayer <= 0)
		return;

	if (CheckObjectIsBigPolygon(Object))
	{
		if ((Mode & 8) == 8)
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, Object->Clearance, 0);
		else
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);

		InitDrawingSmdPinLayer(NewDrawLayer, 0, Mode);

		if ((Mode & 8) == 8)
			DrawPinAreaFillClearance(AreaFill, 0);
		else
			DrawPinAreaFill(AreaFill, 0);
	}
	else
	{
		PolygonObject = (PolygonRecord *) & PolygonBuf;

		if ((Mode & 8) == 8)
			MakePolygonFromObject(Object, PolygonObject, Object->Clearance, 0.0, 1, 1);
		else
			MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		if (IsPolygonVisible(PolygonObject, 0))
		{
			InitDrawingSmdPinLayer(NewDrawLayer, 0, Mode);
			DrawFilledPolygon(PolygonObject, 0);
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawObject(ObjectRecord * Object, int32 Mode)
{
	double xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4, xx2a, yy2a, yy2b, Xmax, Xmin, Ymax, Ymin, Angle1, Length1, Angle2,
	       Length2, xxx2, yyy2, Clearance, Thickness, RotationAngle;
	int32 yyy2a, Layer, x1a, y1a, x2a, y2a, x3, y3, x4, y4, ArcMode, tx1, ty1, tx2, ty2, tdikte, res, NewDrawLayer,
	      Mirror;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;
#ifdef _DEBUG
	int32 ok;
#endif
	/*
	  Mode  = 0x01   = 1     Draw with double line
	          0x02   = 2     Xor
	          0x04   = 4     Background
	          0x08   = 8     clearance
	          0x10   = 16    Trace xor
	          0x20   = 32    Trace hatch
	          0x40   = 64    No initdrawing
	          0x80   = 0x80  Unconnected apd
	          0x100  = 0x100 Error objects
	          0xC00  = 0x000 Draw pads on normal layer
	          0xC00  = 0x400 Draw pads on bottom layer
	          0xC00  = 0x800 Draw pads on top    layer
	          0xC00  = 0xC00 Draw pads on inner  layer
	          0x1000 = 0x1000 Hilited
	*/

#ifdef _DEBUG

	if (Object->Layer == SOLD_MASK_BOTTOM)
		ok = 1;

#endif

	res = 0;
	PolygonObject = (PolygonRecord *) & PolygonBuf;
	Layer = Object->Layer;
	NewDrawLayer = GetLayerToDrawOn(Object->Layer, 1);
	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xx3 = Object->x3;
	x1a = MultX(xx1);

	if (ReverseY)
		y1a = MultY(yy1);
	else
		y1a = Mult(yy1 - Yoffset);

	Clearance = Object->Clearance;
	Thickness = Object->Thickness;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	switch (Object->ObjectType)
	{
	case PIN_PUT_THROUGH_ROUND:
		DrawPinPutThroughRound(Object, Mode);
		break;

	case PIN_PUT_THROUGH_SQUARE:
		DrawPinPutThroughSquare(Object, Mode);
		break;

	case PIN_PUT_THROUGH_POLYGON:
		DrawPinPutThroughPolygon(Object, Mode);
		break;

	case VIA_PUT_THROUGH_ROUND:
		DrawViaPutThroughRound(Object, Mode);
		break;

	case PIN_SMD_ROUND:
		DrawSmdRound(Object, Mode);
		break;

	case PIN_SMD_RECT:
		DrawSmdRect(Object, Mode);
		break;

	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
	case PIN_LINE_ALL_ANGLE:
	case PIN_ARC:
		DrawSmdTraceLine(Object, Mode);
		break;

	case PIN_SMD_POLYGON:
		DrawSmdPolygon(Object, Mode);
		break;

// ***********************************************************************************
// ***********************************************************************************
	case TRACE_HOR:
		if ((Object->Layer >= 0) && (Object->Layer < 32) && ((DrawCode = DrawLayerCode[Object->Layer]) >= 0)
		        && ((DrawCode = DrawLayerCode[Object->Layer]) < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if ((Mode & 8) == 8)
			{
				yy2 += Clearance * 2;
				yyy2a = Mult(yyy2);

				if (yyy2a <= 8)
					break;
			}

			yy2a = yy2 / 2;
			Xmin = xx1 - yy2a;
			Ymin = yy1 - yy2a;
			Xmax = xx1 + xx2 + yy2a;
			Ymax = yy1 + yy2a;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				tx1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
				ty1 = Mult2(yy1 - Yoffset);
				tx2 = Mult2(xx1 + xx2 - Xoffset) + DrawWindowMinX;
				tdikte = Mult2(yyy2);

				if ((Mode & 0x40) == 0)
				{	// No initdrawing
					if ((Mode & 0x20) == 0x20)
					{	// Trace hatch
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
						{
#ifdef _DEBUG

							if (GetROP2(OutputDisplay) == R2_XORPEN)
								ok = 1;

#endif
							InitDrawingObject(Object->ObjectType, DrawCode, 0, IN_NET_FILLED_AND_PEN1);
						}
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_IN_NET_FILLED_AND_PEN1);
					}

					if ((Mode & 0x20) == 0)
					{
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
							InitDrawingObject(Object->ObjectType, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}
				}

				if ((Mode & 0x20) == 0)
				{
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawHorLine(tx1, ty1, tx2, tdikte, 0);
					}
					else
						DrawHorLine(tx1, ty1, tx2, tdikte, 1);
				}
				else
				{	// Trace hatch
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawHorLine(tx1, ty1, tx2, tdikte, 0);
					}
					else
						DrawHorLine(tx1, ty1, tx2, tdikte, 3);
				}
			}
		}

		break;

	case TRACE_VER:
		if ((Object->Layer >= 0) && (Object->Layer < 32) && ((DrawCode = DrawLayerCode[Object->Layer]) >= 0)
		        && ((DrawCode = DrawLayerCode[Object->Layer]) < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if ((Mode & 8) == 8)
			{
				yy2 += Clearance * 2;
				yyy2a = Mult(yyy2);

				if (yyy2a <= 8)
					break;
			}

			yy2a = yy2 / 2;
			Xmin = xx1 - yy2a;
			Ymin = yy1 - yy2a;
			Xmax = xx1 + yy2a;
			Ymax = yy1 + xx2 + yy2a;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				tx1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
				ty1 = Mult2(yy1 - Yoffset);
				ty2 = Mult2(yy1 + xx2 - Yoffset);
				tdikte = Mult2(yyy2);

				if ((Mode & 0x40) == 0)
				{	// No initdrawing
					if ((Mode & 0x20) == 0x20)
					{	// Trace hatch
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
							InitDrawingObject(Object->ObjectType, DrawCode, 0, IN_NET_FILLED_AND_PEN1);
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_IN_NET_FILLED_AND_PEN1);
					}

					if ((Mode & 0x20) == 0)
					{
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
							InitDrawingObject(Object->ObjectType, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}
				}

				if ((Mode & 0x20) == 0)
				{
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawVerLine(tx1, ty1, ty2, tdikte, 0);
					}
					else
						DrawVerLine(tx1, ty1, ty2, tdikte, 1);
				}
				else
				{	// Trace hatch
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawVerLine(tx1, ty1, ty2, tdikte, 0);
					}
					else
						DrawVerLine(tx1, ty1, ty2, tdikte, 3);
				}
			}
		}

		break;

	case TRACE_DIAG1:
		if ((Object->Layer >= 0) && (Object->Layer < 32) && ((DrawCode = DrawLayerCode[Object->Layer]) >= 0)
		        && ((DrawCode = DrawLayerCode[Object->Layer]) < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if ((Mode & 8) == 8)
			{
				yy2 += Clearance * 2;
				yyy2a = Mult(yyy2);

				if (yyy2a <= 8)
					break;
			}

			yy2a = yy2 / 2;
			Xmin = xx1 - yy2;
			yy2b = yy1 - xx2;
			Ymin = yy2b - yy2a;
			Xmax = xx1 + xx2 + yy2a;
			Ymax = yy1 + yy2a;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				tx1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
				ty1 = Mult2(yy1 - Yoffset);
				tx2 = Mult2(xx1 + xx2 - Xoffset) + DrawWindowMinX;
				tdikte = Mult2(yyy2);

				if ((Mode & 0x40) == 0)
				{	// No initdrawing
					if ((Mode & 0x20) == 0x20)
					{	// Trace hatch
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
							InitDrawingObject(Object->ObjectType, DrawCode, 0, IN_NET_FILLED_AND_PEN1);
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_IN_NET_FILLED_AND_PEN1);
					}

					if ((Mode & 0x20) == 0)
					{
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
							InitDrawingObject(Object->ObjectType, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}
				}

				if ((Mode & 0x20) == 0)
				{
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawDiag1Line(tx1, ty1, tx2, tdikte, 0);
					}
					else
						DrawDiag1Line(tx1, ty1, tx2, tdikte, 1);
				}
				else
				{	// Trace hatch
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawDiag1Line(tx1, ty1, tx2, tdikte, 0);
					}
					else
						DrawDiag1Line(tx1, ty1, tx2, tdikte, 3);
				}
			}
		}

		break;

	case TRACE_DIAG2:
		if ((Object->Layer >= 0) && (Object->Layer < 32) && ((DrawCode = DrawLayerCode[Object->Layer]) >= 0)
		        && ((DrawCode = DrawLayerCode[Object->Layer]) < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if ((Mode & 8) == 8)
			{
				yy2 += Clearance * 2;
				yyy2a = Mult(yyy2);

				if (yyy2a <= 8)
					break;
			}

			yy2a = yy2 / 2;
			Xmin = xx1 - yy2a;
			yy2b = yy1 + xx2;
			Ymin = yy1 - yy2a;
			Xmax = xx1 + xx2 + yy2a;
			Ymax = yy2b + yy2a;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				tx1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
				ty1 = Mult2(yy1 - Yoffset);
				tx2 = Mult2(xx1 + xx2 - Xoffset) + DrawWindowMinX;
				tdikte = Mult2(yyy2);

				if ((Mode & 0x40) == 0)
				{	// No initdrawing
					if ((Mode & 0x20) == 0x20)
					{	// Trace hatch
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
							InitDrawingObject(Object->ObjectType, DrawCode, 0, IN_NET_FILLED_AND_PEN1);
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_IN_NET_FILLED_AND_PEN1);
					}

					if ((Mode & 0x20) == 0)
					{
						if ((Object->Info & OBJECT_HIGHLITED) == 0)
							InitDrawingObject(Object->ObjectType, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
						else
							InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}
				}

				if ((Mode & 0x20) == 0)
				{
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawDiag2Line(tx1, ty1, tx2, tdikte, 0);
					}
					else
						DrawDiag2Line(tx1, ty1, tx2, tdikte, 1);
				}
				else
				{	// Trace hatch
					if ((Mode & 0x10) == 0)
					{	// Trace xor
						DrawDiag2Line(tx1, ty1, tx2, tdikte, 0);
					}
					else
						DrawDiag2Line(tx1, ty1, tx2, tdikte, 3);
				}
			}
		}

		break;

	case TRACE_ALL_ANGLE:
		if ((Object->Layer >= 0) && (Object->Layer < 32) && ((DrawCode = DrawLayerCode[Object->Layer]) >= 0)
		        && ((DrawCode = DrawLayerCode[Object->Layer]) < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if ((Mode & 8) == 8)
			{
				Thickness += Clearance * 2;

				if (Mult(Thickness) <= 8)
					break;
			}

			Xmin = min(xx1, xx2) - Thickness * 0.5;
			Ymin = min(yy1, yy2) - Thickness * 0.5;
			Xmax = max(xx1, xx2) + Thickness * 0.5;
			Ymax = max(yy1, yy2) + Thickness * 0.5;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				x2a = MultX(xx2);

				if (ReverseY)
					y2a = MultY(yy2);
				else
					y2a = Mult(yy2 - Yoffset);

				if ((Mode & 0x40) == 0)
				{	// No initdrawing
					if ((Mode & 0x20) == 0x20)
					{	// Trace hatch
						if ((Mode & 0x100) == 0)
						{	// Draw filled
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
								InitDrawingObject(Object->ObjectType, DrawCode, 0, IN_NET_FILLED_AND_PEN1);
							else
								InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_IN_NET_FILLED_AND_PEN1);
						}
						else
						{	// Draw with a thick pen
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
							{
								InitDrawingObject(Object->ObjectType, DrawCode, Mult(Thickness),
								                  DRAW_WITH_PEN_AND_NOT_FILLED);
							}
							else
							{
								InitDrawingObject(Object->ObjectType, DrawCode, Mult(Thickness),
								                  DRAW_WITH_HILITED_PEN_AND_NOT_FILLED);
							}
						}
					}

					if ((Mode & 0x20) == 0)
					{
						if ((Mode & 0x100) == 0x100)
						{	// Draw filled
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
								InitDrawingObject(Object->ObjectType, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
							else
								InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
						}
						else
						{	// Draw with a thick pen
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
							{
								InitDrawingObject(Object->ObjectType, DrawCode, Mult(Thickness),
								                  DRAW_WITH_PEN_AND_NOT_FILLED);
							}
							else
							{
								InitDrawingObject(Object->ObjectType, DrawCode, Mult(Thickness),
								                  DRAW_WITH_HILITED_PEN_AND_NOT_FILLED);
							}
						}
					}
				}

				if ((Mode & 0x28) == 0)
					DrawLine(x1a, y1a, x2a, y2a);
				else
				{
					if ((Mode & 8) == 8)
						MakePolygonFromObject(Object, PolygonObject, Clearance, 0.0, 1, 1);
					else
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

					DrawFilledPolygon(PolygonObject, 0);
				}
			}
		}

		break;

	case TRACE_ARC:
		if ((Object->Layer >= 0) && (Object->Layer < 32) && ((DrawCode = DrawLayerCode[Object->Layer]) >= 0)
		        && ((DrawCode = DrawLayerCode[Object->Layer]) < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if ((Mode & 8) == 8)
			{
				Thickness += Clearance * 2;

				if (Mult(Thickness) <= 8)
					break;
			}

			xx3 = Object->x3;
			yy3 = Object->y3;
			xx4 = Object->x4;
			yy4 = Object->y4;
			Xmin = xx1 - (xx2 + Thickness) * 0.5;
			Ymin = yy1 - (yy2 + Thickness) * 0.5;
			Xmax = xx1 + (xx2 + Thickness) * 0.5;
			Ymax = yy1 + (yy2 + Thickness) * 0.5;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				if ((Mode & 0x40) == 0)
				{	// No initdrawing
					if ((Mode & 0x20) == 0x20)
					{	// Trace hatch
						if ((Mode & 0x100) == 0)
						{	// Draw filled
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
								InitDrawingObject(Object->ObjectType, DrawCode, 0, IN_NET_FILLED_AND_PEN1);
							else
								InitDrawingObject(Object->ObjectType, DrawCode, 0, HILITED_IN_NET_FILLED_AND_PEN1);
						}
						else
						{	// Draw with a thick pen
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
							{
								InitDrawingObject(Object->ObjectType, DrawCode, Mult(Thickness),
								                  DRAW_WITH_PEN_AND_NOT_FILLED);
							}
							else
							{
								InitDrawingObject(Object->ObjectType, DrawCode, Mult(Thickness),
								                  DRAW_WITH_HILITED_PEN_AND_NOT_FILLED);
							}
						}
					}

					if ((Mode & 0x20) == 0)
					{
						if ((Mode & 0x100) == 0x100)
						{	// Draw filled
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
								InitDrawingObject(0, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
							else
								InitDrawingObject(0, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
						}
						else
						{	// Draw with a thick pen
							if ((Object->Info & OBJECT_HIGHLITED) == 0)
								InitDrawingObject(0, DrawCode, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
							else
								InitDrawingObject(0, DrawCode, Mult(Thickness), DRAW_WITH_HILITED_PEN_AND_NOT_FILLED);
						}
					}
				}

				if ((Mode & 0x28) == 0)
				{
					SpecialArc(x1a, y1a, Mult(xx2) + 1, Mult(yy2) + 1, MultX(xx1 + xx3), MultY(yy1 + yy3),
					           MultX(xx1 + xx4), MultY(yy1 + yy4));
				}
				else
				{
					if ((Mode & 8) == 8)
						MakePolygonFromObject(Object, PolygonObject, Clearance, 0.0, 1, 1);
					else
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

					DrawFilledPolygon(PolygonObject, 0);
				}
			}
		}

		break;

// ***********************************************************************************
// ***********************************************************************************
	case OBJECT_LINE:
		if (Object->Info2 == 0)
		{
			Xmin = min(xx1, xx2) - Thickness * 0.5;
			Ymin = min(yy1, yy2) - Thickness * 0.5;
			Xmax = max(xx1, xx2) + Thickness * 0.5;
			Ymax = max(yy1, yy2) + Thickness * 0.5;
		}
		else
		{
			Xmin = min(xx1, xx2) - Thickness * 0.5;
			Ymin = min(yy1, yy2) - Thickness * 0.5;
			Xmax = max(xx1, xx2) + Thickness * 0.5;
			Ymax = max(yy1, yy2) + Thickness * 0.5;
		}

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x2a = MultX(xx2);
			y2a = MultY(yy2);

			if ((Mode & 0x40) == 0)
			{	// No initdrawing
				if ((Mode & 1) == 0)
					res = InitDrawingObject(0, Layer, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
				else
					res = InitDrawingObject(0, Layer & ~1, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
			}

			DrawLine(x1a, y1a, x2a, y2a);

			if ((Mode & 1) == 1)
			{
				if (fabs(xx2 - xx1) > fabs(yy2 - yy1))
					DrawLine(x1a, y1a - 1, x2a, y2a - 1);
				else
					DrawLine(x1a + 1, y1a, x2a + 1, y2a);
			}
		}

		break;

	case OBJECT_RECT:
		xx2a = xx2 / 2;
		yy2a = yy2 / 2;
		Xmin = xx1 - xx2a - Thickness * 0.5;
		Ymin = yy1 - yy2a - Thickness * 0.5;
		Xmax = xx1 + xx2a + Thickness * 0.5;
		Ymax = yy1 + yy2a + Thickness * 0.5;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			if ((Mode & 0x40) == 0)
			{	// No initdrawing
				if ((Object->Info & OBJECT_FILLED) == 0)
				{
					if ((Mode & 1) == 0)
						res = InitDrawingObject(0, Layer, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
					else
						res = InitDrawingObject(0, Layer & ~1, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
				}
				else
				{
					if ((Mode & 2) == 0)
						res = InitDrawingObject(0, Layer, 0, NORMAL_FILLED_AND_PEN1);
					else
					{	// XOR object
						res = InitDrawingObject(0, Layer, 0, NORMAL_FILLED_AND_NO_PEN);
					}
				}
			}

			rect3(x1a, y1a, Mult(xx2), Mult(yy2));

			if (((Object->Info & OBJECT_FILLED) == 0) && ((Mode & 1) == 1))
				rect3(x1a, y1a, Mult(xx2) - 2, Mult(yy2) - 2);
		}

		break;

	case OBJECT_CIRCLE:
		xx2a = xx2 / 2;
		yy2a = xx2 / 2;
		Xmin = xx1 - xx2a - Thickness * 0.5;
		Ymin = yy1 - yy2a - Thickness * 0.5;
		Xmax = xx1 + xx2a + Thickness * 0.5;
		Ymax = yy1 + yy2a + Thickness * 0.5;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			if ((Mode & 0x40) == 0)
			{	// No initdrawing
				if ((Object->Info & OBJECT_FILLED) == 0)
				{
					if ((Mode & 1) == 0)
						res = InitDrawingObject(0, Layer, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
					else
						res = InitDrawingObject(0, Layer & ~1, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
				}
				else
				{
					res = InitDrawingObject(0, Layer, 0, NORMAL_FILLED_AND_PEN1);

					if ((Mode & 2) == 2)
						InitDrawingEmptyPen();
				}
			}

			ellips2(x1a, y1a, Mult(xx2), Mult(xx2), CircleConv[(int32) yy2]);

			if (((Object->Info & OBJECT_FILLED) == 0) && ((Mode & 1) == 1))
				ellips2(x1a, y1a, Mult(xx2) - 2, Mult(xx2) - 2, CircleConv[(int32) yy2]);
		}

		break;

	case OBJECT_ARC:
		xx3 = Object->x3;
		yy3 = Object->y3;
		xx4 = Object->x4;
		yy4 = Object->y4;
		Xmin = xx1 - (xx2 + Thickness) * 0.5;
		Ymin = yy1 - (yy2 + Thickness) * 0.5;
		Xmax = xx1 + (xx2 + Thickness) * 0.5;
		Ymax = yy1 + (yy2 + Thickness) * 0.5;
#ifdef _DEBUG

		if (Layer == INFO_LAYER)
			ok = 1;

#endif

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			if ((Mode & 0x40) == 0)
			{	// No initdrawing
				if ((Object->Info & OBJECT_FILLED) == 0)
				{
					if ((Mode & 1) == 0)
						InitDrawingObject(0, Layer, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
					else
						InitDrawingObject(0, Layer & ~1, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
				}
				else
				{
					if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
						InitDrawingObject(0, Layer, 0, DRAW_WITH_PEN_AND_BACKGROUND_BRUSH);
					else
					{
						InitDrawingObject(0, Layer, 0, NORMAL_FILLED_AND_PEN1);

						if ((Mode & 2) == 2)
							InitDrawingEmptyPen();
					}
				}
			}

			if ((Object->Info & OBJECT_FILLED) == 0)
			{
				x3 = MultX(xx1 + xx3);
				y3 = MultY(yy1 + yy3);
				x4 = MultX(xx1 + xx4);
				y4 = MultY(yy1 + yy4);
				ConvNormalCoorToPolar(xx1, yy1, xx1 + xx3, yy1 + yy3, &Angle1, &Length1);
				ConvNormalCoorToPolar(xx1, yy1, xx1 + xx4, yy1 + yy4, &Angle2, &Length2);

				if (Angle2 < Angle1)
					Angle2 += ANGLE_360;

#ifdef _DEBUG

				if (Angle2 - Angle1 < ANGLE_CONVERT(2.0))
					ok = 1;

#endif

				if ((Angle2 - Angle1 < ANGLE_90) && (Angle2 - Angle1 > 0.0)
				        && ((abs(x3 - x4) < 2) || (abs(y3 - y4) < 2)))
				{
					if (((abs(x3 - x4) < 2) && (abs(y3 - y4) < 2)) || (Angle2 - Angle1 < ANGLE_CONVERT(10.0)))
						ArcMode = 0;
					else
						ArcMode = 1;
				}
				else
				{
					if ((Angle2 - Angle1 > 0.0) && (Angle2 - Angle1 < ANGLE_CONVERT(3.0)))
						ArcMode = 0;
					else
						ArcMode = 1;
				}

				if (ArcMode == 0)
					DrawLine(x3, y3, x4, y4);
				else
				{
					if (ReverseY)
					{
						SpecialArc(x1a, y1a, Mult(xx2) + 1, Mult(yy2) + 1, x3, y3, x4, y4);

						if ((Mode & 1) == 1)
							SpecialArc(x1a, y1a, Mult(xx2) - 1, Mult(yy2) - 1, x3, y3, x4, y4);
					}
					else
					{
						SpecialArc(x1a, y1a, Mult(xx2) + 1, Mult(yy2) + 1, MultX(xx1 + xx3), MultY2(yy1 + yy3),
						           MultX(xx1 + xx4), MultY2(yy1 + yy4));

						if ((Mode & 1) == 1)
							SpecialArc(x1a, y1a, Mult(xx2) - 1, Mult(yy2) - 1, MultX(xx1 + xx3), MultY2(yy1 + yy3),
							           MultX(xx1 + xx4), MultY2(yy1 + yy4));
					}
				}
			}
			else
				ellips2(x1a, y1a, Mult(xx2), Mult(xx2), 255);
		}

		break;

	case OBJECT_TEXT:
		RotationAngle = Object->RotationAngle;
		Mirror = Object->Mirror;
		GetMinMaxText2(xx1, yy1, xx2, 0, RotationAngle, 0, Mirror, (LPSTR) Object->TraceNr);
#ifdef _DEBUG

		if (stricmpOwn((LPSTR) Object->TraceNr, "text0") == 0)
			ok = 1;

#endif

		if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
		{
			if ((Mode & 0x40) == 0)
			{	// No initdrawing
				if ((Mode & 1) == 0)
					res = InitDrawingObject(0, Layer, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
				else
					res = InitDrawingObject(0, Layer & ~1, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
			}

			DrawStrWithRotation(xx1, yy1, xx2, RotationAngle, 0, Mirror, (LPSTR) Object->TraceNr);
		}

		break;

	case OBJECT_POLYGON:
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

//      Thickness=Object->Clearance;
		if ((PolygonObject->maxx >= ViewMinX) && (PolygonObject->minx <= ViewMaxX) && (PolygonObject->maxy >= ViewMinY)
		        && (PolygonObject->miny <= ViewMaxY))
		{
			if ((Mode & 0x40) == 0)
			{	// No initdrawing
				if ((Object->Info & OBJECT_FILLED) == 0)
				{
					if ((Mode & 1) == 0)
						res = InitDrawingObject(0, Layer, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
					else
						res = InitDrawingObject(0, Layer & ~1, Mult(Thickness), DRAW_WITH_PEN_AND_NOT_FILLED);
				}
				else
				{
					res = InitDrawingObject(0, Layer, 0, NORMAL_FILLED_AND_PEN1);

					if ((Mode & 2) == 2)
						InitDrawingEmptyPen();
				}
			}

			DrawFilledPolygon(PolygonObject, 0);
		}

		break;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DrawDrillObject(ObjectRecord * Object, int32 Mode)
{
	double xx1, yy1, xx2, yy2, xx3, yy2a, Xmax, Xmin, Ymax, Ymin, xxx2, yyy2, Clearance;
	int32 x2, xxx2a, yyy2a, Layer, x1a, y1a;

	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	*/

	if (DrawDrillMode == 0)
		return;

	if ((Mode & 8) == 8)
	{
		switch (Object->ObjectType)
		{
		case PIN_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
		case VIA_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_POLYGON:
			return;
		}
	}

	Layer = Object->Layer;
	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xx3 = Object->x3;
	x1a = Mult(xx1 - Xoffset) + DrawWindowMinX;

	if (ReverseY)
		y1a = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
	else
		y1a = Mult(yy1 - Yoffset);

	Clearance = Object->Clearance;
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;

	if ((Mode & 8) == 8)
	{
		xxx2 += Clearance * 2;
		yyy2 += Clearance * 2;
	}

	switch (Object->ObjectType)
	{
	case DRILL:
		yy2a = xx2 / 2;

		if ((Mode & 8) == 8)
			yy2a += Clearance;

		Xmin = xx1 - yy2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + yy2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			yyy2a = Mult(xxx2);

			switch (DrawDrillMode)
			{
			case 0:
//              ellips2(x1a,y1a,Mult(yyy2),Mult(yyy2),255);
				break;

			case 1:
				if (yyy2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, yyy2a, yyy2a, 255);
					x2 = Mult(xxx2 * SQRT05 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a - x2, x1a + x2, y1a + x2);
						DrawLine(x1a - x2, y1a + x2, x1a + x2, y1a - x2);
					}
				}

				break;

			case 2:
				if (yyy2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, yyy2a, yyy2a, 255);
					x2 = Mult(xxx2 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a, x1a + x2, y1a);
						DrawLine(x1a, y1a + x2, x1a, y1a - x2);
					}
				}

				break;

			case 3:
				if (yyy2a > 3)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						if ((Mode & 2) == 0)
							InitDrawingObject(0, DRILL_LAYER, 0, DRAW_WITH_PEN_AND_BACKGROUND_BRUSH);
						else
							InitDrawingObject(0, DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, yyy2a, yyy2a, 255);
				}

				break;
			}
		}

		break;

	case DRILL_UNPLATED:
		yy2a = xx2 / 2;

		if ((Mode & 8) == 8)
			yy2a += Clearance;

		Xmin = xx1 - yy2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + yy2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			xxx2a = Mult(xxx2);

			switch (DrawDrillMode)
			{
			case 0:
//                ellips2(x1a,y1a,Mult(yyy2),Mult(yyy2),255);
				break;

			case 1:
				if (xxx2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, DRILL_UNPLATED_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, xxx2a, xxx2a, 255);
					x2 = Mult(xxx2 * SQRT05 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a - x2, x1a + x2, y1a + x2);
						DrawLine(x1a - x2, y1a + x2, x1a + x2, y1a - x2);
					}
				}

				break;

			case 2:
				if (xxx2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, DRILL_UNPLATED_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, xxx2a, xxx2a, 255);
					x2 = Mult(xxx2 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a, x1a + x2, y1a);
						DrawLine(x1a, y1a + x2, x1a, y1a - x2);
					}
				}

				break;

			case 3:
				if (xxx2a > 2)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						if ((Mode & 2) == 0)
							InitDrawingObject(0, DRILL_UNPLATED_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
						else
							InitDrawingObject(0, DRILL_UNPLATED_LAYER, 0, DRAW_WITH_PEN_AND_BACKGROUND_BRUSH);
					}

					ellips2(x1a, y1a, xxx2a, xxx2a, 255);
				}

				break;
			}
		}

		break;

	case PIN_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
	case PIN_PUT_THROUGH_POLYGON:
		if (Object->ObjectType == PIN_PUT_THROUGH_POLYGON)
		{
			xxx2 = Object->x2;
			yy2 = Object->y2;
			yyy2 = yy2;

			if ((Mode & 8) == 8)
			{
				xxx2 += Clearance * 2;
				yyy2 += Clearance * 2;
			}
		}

		yy2a = yy2 / 2;
		Xmin = xx1 - yy2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + yy2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			yyy2a = Mult(yyy2);

			switch (DrawDrillMode)
			{
			case 0:
//              ellips2(x1a,y1a,Mult(yyy2),Mult(yyy2),255);
				break;

			case 1:
				if (yyy2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, yyy2a, yyy2a, 255);
					x2 = Mult(yyy2 * SQRT05 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a - x2, x1a + x2, y1a + x2);
						DrawLine(x1a - x2, y1a + x2, x1a + x2, y1a - x2);
					}
				}

				break;

			case 2:
				if (yyy2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, yyy2a, yyy2a, 255);
					x2 = Mult(yyy2 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a, x1a + x2, y1a);
						DrawLine(x1a, y1a + x2, x1a, y1a - x2);
					}
				}

				break;

			case 3:
				if (yyy2a > 2)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						if ((Mode & 2) == 0)
						{
							InitDrawingObject(0, DRILL_LAYER, 0, DRAW_WITH_PEN_AND_BACKGROUND_BRUSH);
							ellips2(x1a, y1a, yyy2a, yyy2a, 255);
						}
						else
						{
							if ((OkToDrawTopPads) || (OkToDrawInnerPads))
							{
								InitDrawingObject(0, SHAPE_PINS_TOP, 0, NORMAL_FILLED_AND_NO_PEN);
								ellips2(x1a, y1a, yyy2a, yyy2a, 255);
							}
							else
							{
								if (OkToDrawBottomPads)
								{
									InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, NORMAL_FILLED_AND_NO_PEN);
									ellips2(x1a, y1a, yyy2a, yyy2a, 255);
								}
							}
						}
					}
				}

				break;
			}
		}

		break;

	case VIA_PUT_THROUGH_ROUND:
		yy2a = yy2 / 2;

		if ((Mode & 8) == 8)
			yy2a += Clearance;

		Xmin = xx1 - yy2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + yy2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			yyy2a = Mult(yyy2);

			switch (DrawDrillMode)
			{
			case 0:
//              ellips2(x1a,y1a,Mult(yyy2),Mult(yyy2),255);
				break;

			case 1:
				if (yyy2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, VIA_DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, yyy2a, yyy2a, 255);
					x2 = Mult(yyy2 * SQRT05 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a - x2, x1a + x2, y1a + x2);
						DrawLine(x1a - x2, y1a + x2, x1a + x2, y1a - x2);
					}
				}

				break;

			case 2:
				if (yyy2a > 6)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						InitDrawingObject(0, VIA_DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
					}

					ellips2(x1a, y1a, yyy2a, yyy2a, 255);
					x2 = Mult(yyy2 * 0.5);

					if (x2 > 1)
					{
						DrawLine(x1a - x2, y1a, x1a + x2, y1a);
						DrawLine(x1a, y1a + x2, x1a, y1a - x2);
					}
				}

				break;

			case 3:
				if (yyy2a > 2)
				{
					if ((Mode & 0x40) == 0)
					{	// No initdrawing
						if ((Mode & 2) == 0)
						{
							InitDrawingObject(0, VIA_DRILL_LAYER, 0, DRAW_WITH_PEN_AND_BACKGROUND_BRUSH);
							ellips2(x1a, y1a, yyy2a, yyy2a, 255);
						}
						else
						{
							if ((OkToDrawTopPads) || (OkToDrawInnerPads))
							{
								InitDrawingObject(0, SHAPE_PINS_TOP, 0, NORMAL_FILLED_AND_NO_PEN);
								ellips2(x1a, y1a, yyy2a, yyy2a, 255);
							}
							else
							{
								if (OkToDrawBottomPads)
								{
									InitDrawingObject(0, SHAPE_PINS_BOTTOM, 0, NORMAL_FILLED_AND_NO_PEN);
									ellips2(x1a, y1a, yyy2a, yyy2a, 255);
								}
							}
						}
					}
				}

				break;
			}
		}

		break;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawOutlineComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode)
{
	int32 CompInfo, Mirror, TempBackGroundActive, cnt2;
	ObjectRecord *Object;
	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	*/
#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "L13") == 0)
		ok = 1;

#endif

	Mirror = (Comp->TextVisibility & 8) >> 3;

	if (ViewSingleLayer)
	{
		if (Mirror == 0)
		{
			if ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0x10)
				return;
		}
		else
		{
			if ((DrawLayerCode[0] & 0x10) == 0x10)
				return;
		}
	}

	NrObjects = 0;
	ShapeCompOutLineToObject(Comp, OffsetX, OffsetY, Rotation);
	CompInfo = Comp->Info;


	if ((Mode & 2) == 0)
	{
		if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		{
			SetROP2(OutputDisplay, SelectColorMode);
			TempBackGroundActive = BackGroundActive;

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Mode & 0x200) == 0)
				{
					if (Mode & 0x100)
					{
						if (CurrentDrawingLayer == 0)
						{
							if (Object->Layer == COMP_OUTLINE_LAYER_BOTTOM)
								continue;
						}
						else
						if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
						{
							if (Object->Layer == COMP_OUTLINE_LAYER_TOP)
								continue;
						}
					}
					else
					{
						if (CurrentDrawingLayer == 0)
						{
							if (Object->Layer == COMP_OUTLINE_LAYER_TOP)
								continue;
						}
						else
						if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
						{
							if (Object->Layer == COMP_OUTLINE_LAYER_BOTTOM)
								continue;
						}
					}
				}

				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, Mode | 1);
			}

			SetROP2(OutputDisplay, R2_COPYPEN);
			return;
		}
	}

	TempBackGroundActive = BackGroundActive;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);

		if ((Mode & 0x200) == 0)
		{
			if (Mode & 0x100)
			{
				if (CurrentDrawingLayer == 0)
				{
					if (Object->Layer == COMP_OUTLINE_LAYER_BOTTOM)
						continue;
				}
				else
				if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
				{
					if (Object->Layer == COMP_OUTLINE_LAYER_TOP)
						continue;
				}
			}
			else
			{
				if (CurrentDrawingLayer == 0)
				{
					if (Object->Layer == COMP_OUTLINE_LAYER_TOP)
						continue;
				}
				else
				if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
				{
					if (Object->Layer == COMP_OUTLINE_LAYER_BOTTOM)
						continue;
				}
			}
		}

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObject(Object, Mode);
	}


	/*


	  if ((Mode & 2)==0) {
	    if ((CompInfo & OBJECT_SELECTED) == OBJECT_SELECTED) {
	      SetROP2(OutputDisplay,SelectColorMode);
	      DrawObjects(Mode|1);
	      SetROP2(OutputDisplay,R2_COPYPEN);
	      return ;
	    }
	  }
	  DrawObjects(Mode);
	*/
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawSilkScreenComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode)
{
	int32 Mirror, cnt2, TempBackGroundActive;
	ObjectRecord *Object;
	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	*/
#ifdef _DEBUG

	if (stricmp(Comp->Name, "L1") == 0)
	{
		ok = 1;

		if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
			ok = 1;
	}

#endif

	Mirror = (Comp->TextVisibility & 8) >> 3;

	if ((!OkToDrawSilkScreenTop) && (!OkToDrawSilkScreenBottom))
		return;

	if (ViewSingleLayer)
	{
		if (Mirror == 0)
		{
			if ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0x10)
				return;
		}
		else
		{
			if ((DrawLayerCode[0] & 0x10) == 0x10)
				return;
		}
	}


	NrObjects = 0;
	ShapeCompSilkScreenToObject(Comp, OffsetX, OffsetY, Rotation);
	ShapeOtherToObject(Comp, OffsetX, OffsetY, Rotation, -1, 5);

	if ((Mode & 2) == 0)
	{
		if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		{
			SetROP2(OutputDisplay, SelectColorMode);
			TempBackGroundActive = BackGroundActive;

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (((Object->Layer == SILKSCREEN_TOP) && (OkToDrawSilkScreenTop))
				        || ((Object->Layer == SILKSCREEN_BOTTOM) && (OkToDrawSilkScreenBottom)))
				{
					if ((Mode & 0x200) == 0)
					{
						if (Mode & 0x100)
						{
							if (CurrentDrawingLayer == 0)
							{
								if (Object->Layer == SILKSCREEN_BOTTOM)
									continue;
							}
							else
							if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
							{
								if (Object->Layer == SILKSCREEN_TOP)
									continue;
							}
						}
						else
						{
							if (CurrentDrawingLayer == 0)
							{
								if (Object->Layer == SILKSCREEN_TOP)
									continue;
							}
							else
							if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
							{
								if (Object->Layer == SILKSCREEN_BOTTOM)
									continue;
							}
						}
					}

					if (TempBackGroundActive)
						SetBackGroundActive(0);

					DrawObject(Object, Mode | 1);
				}
			}

			SetROP2(OutputDisplay, R2_COPYPEN);
			return;
		}
	}

	TempBackGroundActive = BackGroundActive;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);

		if (((Object->Layer == SILKSCREEN_TOP) && (OkToDrawSilkScreenTop))
		        || ((Object->Layer == SILKSCREEN_BOTTOM) && (OkToDrawSilkScreenBottom)))
		{
			if ((Mode & 0x200) == 0)
			{
				if (Mode & 0x100)
				{
					if (CurrentDrawingLayer == 0)
					{
						if (Object->Layer == SILKSCREEN_BOTTOM)
							continue;
					}
					else
					if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
					{
						if (Object->Layer == SILKSCREEN_TOP)
							continue;
					}
				}
				else
				{
					if (CurrentDrawingLayer == 0)
					{
						if (Object->Layer == SILKSCREEN_TOP)
							continue;
					}
					else
					if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
					{
						if (Object->Layer == SILKSCREEN_BOTTOM)
							continue;
					}
				}
			}

			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawObject(Object, Mode);
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawPlacementOutlineComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode)
{
	int32 CompInfo, Mirror, cnt2, TempBackGroundActive;
	ObjectRecord *Object;
#ifdef _DEBUG
	int32 ok;
#endif
	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	*/

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "U100") == 0)
		ok = 1;

#endif

	TempBackGroundActive = BackGroundActive;
	Mirror = (Comp->TextVisibility & 8) >> 3;

	if (ViewSingleLayer)
	{
		if (Mirror == 0)
		{
			if ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0x10)
				return;
		}
		else
		{
			if ((DrawLayerCode[0] & 0x10) == 0x10)
				return;
		}
	}

	NrObjects = 0;
	ShapePlacementOutLineToObject(Comp, OffsetX, OffsetY, Rotation);
	CompInfo = Comp->Info;

	if ((Mode & 2) == 0)
	{
		if ((CompInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
		{
			SetROP2(OutputDisplay, SelectColorMode);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Mode & 0x200) == 0)
				{
					if (Mode & 0x100)
					{
						if (CurrentDrawingLayer == 0)
						{
							if (Object->Layer == PLACEMENT_OUTLINE_BOTTOM)
								continue;
						}
						else
						if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
						{
							if (Object->Layer == PLACEMENT_OUTLINE_TOP)
								continue;
						}
					}
					else
					{
						if (CurrentDrawingLayer == 0)
						{
							if (Object->Layer == PLACEMENT_OUTLINE_TOP)
								continue;
						}
						else
						if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
						{
							if (Object->Layer == PLACEMENT_OUTLINE_BOTTOM)
								continue;
						}
					}
				}

				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawObject(Object, Mode | 1);
			}

			SetROP2(OutputDisplay, R2_COPYPEN);
			return;
		}
	}

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);

		if ((Mode & 0x200) == 0)
		{
			if (Mode & 0x100)
			{
				if (CurrentDrawingLayer == 0)
				{
					if (Object->Layer == PLACEMENT_OUTLINE_BOTTOM)
						continue;
				}
				else
				if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
				{
					if (Object->Layer == PLACEMENT_OUTLINE_TOP)
						continue;
				}
			}
			else
			{
				if (CurrentDrawingLayer == 0)
				{
					if (Object->Layer == PLACEMENT_OUTLINE_TOP)
						continue;
				}
				else
				if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
				{
					if (Object->Layer == PLACEMENT_OUTLINE_BOTTOM)
						continue;
				}
			}
		}

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObject(Object, Mode);
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawDrillsComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode)
{
	int32 cnt, DrawCode, Mirror, TempBackGroundActive;
	ObjectRecord *Object;
#ifdef _DEBUG
	int32 ok;
#endif
	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	*/

	Mirror = ((Comp->CompMode & 8) >> 3);
	/*
	  if (!Mirror) {
	    if (!DrawTopComponents) {
	      return;
	    }
	  } else {
	    if (!DrawBottomComponents) {
	      return;
	    }
	  }
	*/
	NrObjects = 0;
	ShapePinsToObject(Comp, OffsetX, OffsetY, Rotation, 0, 0, 3);

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "J3") == 0)
		ok = 1;

#endif

	if (DrawDrillMode == 0)
		return;

	if (!ViewSingleLayer)
	{
		TempBackGroundActive = BackGroundActive;

		if (((OkToDrawTopPads) && (Design.NrBoardLayers > 1)) || (OkToDrawBottomPads)
		        || ((OkToDrawInnerPads) && (Design.NrBoardLayers > 2)))
		{
			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawDrillObject(Object, Mode);
			}
		}
	}
	else
	{
		DrawCode = DrawLayerCode[CurrentDrawingLayer];
		TempBackGroundActive = BackGroundActive;

		if (((OkToDrawTopPads) && (Design.NrBoardLayers > 1) && ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0))
		        || ((OkToDrawBottomPads) && ((DrawLayerCode[0] & 0x10) == 0)) || ((OkToDrawInnerPads)
		                && (Design.NrBoardLayers > 2)
		                && ((DrawCode & 0x10) == 0)))
		{
			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawDrillObject(Object, Mode);
			}
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawReferenceComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode)
/*
  Mode  = 0x01 = 1   Draw with double line
          0x02 = 2   Xor
          0x04 = 4   Background
          0x08 = 8   clearance
          0x10 = 16  Trace xor
          0x20 = 32  Trace hatch
          0x40 = 64  No initdrawing
          0x80 = 128 Draw pads on bottom layer
*/
{
	double x2, y2, h2, TextRotation, RotationAngle, RotationAngle2;
	int32 lengte, MemPos, ShapeNr, Mirror;
	ShapeRecord *Shape;
#ifdef _DEBUG
	int32 ok;
#endif

//     TextVisibility
//
//     0x001   selected
//     0x002   rotated
//     0x004   rotated
//     0x008   mirrored
//     0x100   visible
//     0x400   rotated (45)

	if ((Comp->TextVisibility & 0x100) != 0)
		return;

//  Mirror=(Comp->CompMode & 8) >> 3;

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "Z101") == 0)
		ok = 1;

	if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		ok = 1;

	if (Rotation == 8)
		ok = 1;

#endif

	Mirror = (Comp->TextVisibility & 8) >> 3;

	if (!ViewSingleLayer)
	{
		if (((Mirror == 0) && (!OkToDrawSilkScreenTop)) || ((Mirror == 1) && (!OkToDrawSilkScreenBottom)))
			return;

		if ((Mode & 0x200) == 0)
		{
			if (Mode & 0x100)
			{
				if ((CurrentDrawingLayer == 0) && (!Mirror) && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
				else
				if ((CurrentDrawingLayer == Design.NrBoardLayers - 1) && (Mirror)
				        && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
			}
			else
			{
				if ((CurrentDrawingLayer == 0) && (!Mirror) && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
				else
				if ((CurrentDrawingLayer == Design.NrBoardLayers - 1) && (Mirror)
				        && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
			}
		}
	}
	else
	{
		if (Mirror == 0)
		{
			if ((!OkToDrawSilkScreenTop) || ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0x10))
				return;
		}
		else
		{
			if ((!OkToDrawSilkScreenBottom) || ((DrawLayerCode[0] & 0x10) == 0x10))
				return;
		}
	}

	ShapeNr = Comp->ShapeNr;

	if (ShapeNr == -1)
		return;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	lengte = strlen(Comp->Name);
	h2 = Comp->CompNameHeight;

	if (h2 == 0)
		h2 = Shape->ShapeNameHeight;

	x2 = Comp->CompNameOriginX;
	y2 = Comp->CompNameOriginY;



	if ((Mode & 8) == 0)
	{
		TextRotation = Comp->CompNameRotation;
		RotationAngle = Rotation + Comp->Rotation;
		RotationAngle2 = TextRotation;

		if (Mirror == 0)
			RotationAngle2 += RotationAngle;
		else
		{
			x2 = -x2;
			RotationAngle2 -= RotationAngle;
		}

		if (RotationAngle2 > 360)
			RotationAngle2 -= 360;

		if (RotationAngle2 < 0)
			RotationAngle2 += 360;

		RotatePoint2(&x2, &y2, RotationAngle);
		x2 += Comp->CompOriginX + OX;
		y2 += Comp->CompOriginY + OY;
	}
	else
	{
		TextRotation = Comp->CompNameRotation + Rotation;
		RotationAngle = Comp->Rotation;
//    RotationAngle = Rotation+Comp->Rotation;
		RotationAngle2 = TextRotation;

		if (Mirror == 0)
			RotationAngle2 += RotationAngle;
		else
		{
			x2 = -x2;
			RotationAngle2 -= RotationAngle;
		}

		if (RotationAngle2 > 360)
			RotationAngle2 -= 360;

		if (RotationAngle2 < 0)
			RotationAngle2 += 360;

		RotatePoint2(&x2, &y2, RotationAngle);
		x2 += Comp->CompOriginX + OX;
		y2 += Comp->CompOriginY + OY;
	}


	GetMinMaxText2(x2, y2, h2, 0, RotationAngle2, 0, Mirror, Comp->Name);

	if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
	{
		/*
		    if ((Comp->TextVisibility & 0x1) == 0x1) {
		      InitDrawingObject(0,PLACEMENT_OUTLINE_LAYER,0,DRAW_WITH_PEN_AND_NOT_FILLED);
		    } else {
		*/
		InitDrawingObject(0, COMP_REF_LAYER, Mult(Comp->CompNamePenThickNess), DRAW_WITH_PEN_AND_NOT_FILLED);

		if (((Mode == 0x200) || (Mode == 0)) && ((Comp->TextVisibility & 1) == 1))
			SetROP2(OutputDisplay, SelectColorMode);

		DrawStrWithRotation(x2, y2, h2, RotationAngle2, 0, Mirror, Comp->Name);

		if (((Mode == 0x200) || (Mode == 0)) && ((Comp->TextVisibility & 1) == 1))
			SetROP2(OutputDisplay, R2_COPYPEN);
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawValueComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode)
/*
  Mode  = 0x01 = 1   Draw with double line
          0x02 = 2   Xor
          0x04 = 4   Background
          0x08 = 8   clearance
          0x10 = 16  Trace xor
          0x20 = 32  Trace hatch
          0x40 = 64  No initdrawing
          0x80 = 128 Draw pads on bottom layer
*/
{
	double x2, y2, h2, TextRotation, RotationAngle, RotationAngle2;
	int32 lengte, MemPos, ShapeNr, Mirror;
	ShapeRecord *Shape;

//     TextVisibility
//
//     0x010   selected
//     0x020   rotated
//     0x040   rotated
//     0x080   mirrored
//     0x200   visible
//     0x800   rotated (45)

	if ((Comp->TextVisibility & 0x200) != 0)
		return;

	Mirror = (Comp->TextVisibility & 0x80) >> 7;

	if (!ViewSingleLayer)
	{
		if (((Mirror == 0) && (!OkToDrawSilkScreenTop)) || ((Mirror == 1) && (!OkToDrawSilkScreenBottom)))
			return;

		if ((Mode & 0x200) == 0)
		{
			if (Mode & 0x100)
			{
				if ((CurrentDrawingLayer == 0) && (!Mirror) && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
				else
				if ((CurrentDrawingLayer == Design.NrBoardLayers - 1) && (Mirror)
				        && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
			}
			else
			{
				if ((CurrentDrawingLayer == 0) && (!Mirror) && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
				else
				if ((CurrentDrawingLayer == Design.NrBoardLayers - 1) && (Mirror)
				        && ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10))
					return;
			}
		}
	}
	else
	{
		if (Mirror == 0)
		{
			if ((!OkToDrawSilkScreenTop) || ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0x10))
				return;
		}
		else
		{
			if ((!OkToDrawSilkScreenBottom) || ((DrawLayerCode[0] & 0x10) == 0x10))
				return;
		}
	}

	ShapeNr = Comp->ShapeNr;

	if (ShapeNr == -1)
		return;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	lengte = strlen(Comp->Value);
	h2 = Comp->CompValueHeight;

	if (h2 == 0)
		h2 = Shape->ShapeNameHeight;

	x2 = Comp->CompValueOriginX;
	y2 = Comp->CompValueOriginY;

	if ((Mode & 8) == 0)
	{

		TextRotation = Comp->CompValueRotation;
		RotationAngle = Rotation + Comp->Rotation;
		RotationAngle2 = TextRotation;

		if (Mirror == 0)
			RotationAngle2 += RotationAngle;
		else
		{
			x2 = -x2;
			RotationAngle2 -= RotationAngle;
		}

		if (RotationAngle2 > 360)
			RotationAngle2 -= 360;

		if (RotationAngle2 < 0)
			RotationAngle2 += 360;

		RotatePoint2(&x2, &y2, RotationAngle);

		x2 += Comp->CompOriginX + OX;
		y2 += Comp->CompOriginY + OY;

	}
	else
	{

		TextRotation = Comp->CompValueRotation + Rotation;
		RotationAngle = Comp->Rotation;
//    RotationAngle2 = TextRotation;
		RotationAngle2 = TextRotation;

		if (Mirror == 0)
			RotationAngle2 += RotationAngle;
		else
		{
			x2 = -x2;
			RotationAngle2 -= RotationAngle;
		}

		if (RotationAngle2 > 360)
			RotationAngle2 -= 360;

		if (RotationAngle2 < 0)
			RotationAngle2 += 360;

		RotatePoint2(&x2, &y2, RotationAngle);

		x2 += Comp->CompOriginX + OX;
		y2 += Comp->CompOriginY + OY;
	}

	GetMinMaxText2(x2, y2, h2, 0, RotationAngle2, 0, Mirror, Comp->Value);

	if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
	{
		/*
		    if ((Comp->TextVisibility & 0x10) == 0x10) {
		      InitDrawingObject(0,PLACEMENT_OUTLINE_LAYER,0,DRAW_WITH_PEN_AND_NOT_FILLED);
		    } else {
		*/
		InitDrawingObject(0, COMP_VALUE_LAYER, Mult(Comp->CompValuePenThickNess), DRAW_WITH_PEN_AND_NOT_FILLED);

		if (((Mode == 0x200) || (Mode == 0)) && ((Comp->TextVisibility & 0x10) == 0x10))
			SetROP2(OutputDisplay, SelectColorMode);

		DrawStrWithRotation(x2, y2, h2, RotationAngle2, 0, Mirror, Comp->Value);

		if (((Mode == 0x200) || (Mode == 0)) && ((Comp->TextVisibility & 0x10) == 0x10))
			SetROP2(OutputDisplay, R2_COPYPEN);
	}
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawPinsComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode)
{
#ifdef _DEBUG
	int32 ok;

	if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		ok = 1;

	if (stricmpOwn(Comp->Name, "U1") == 0)
		ok = 1;

//  if (stricmpOwn(Shape->ShapeName,"pga321_026_zif")==0) {
//    ok=1;
//  }
#endif

	NrObjects = 0;
	ShapePinsToObject(Comp, OffsetX, OffsetY, Rotation, 0, 0, 1);
	DrawCompPinObjects(Comp, Mode);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawDrillsComps(int32 Mode)
{
	int32 cnt;
	CompRecord *Comp;
	int32 TempBackGroundActive;

	TempBackGroundActive = BackGroundActive;

	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	*/
	if ((Mode & 8) == 0)
	{
		if (!Printing)
			SetROP2(OutputDisplay, R2_COPYPEN);
	}
	else
	{
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		if (!Printing)
			SetROP2(OutputDisplay, R2_COPYPEN);

//    SetROP2(OutputDisplay,R2_XORPEN);
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((Mode & 8) == 8)
				Mode |= 0x40;

			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawDrillsComp(Comp, 0.0, 0.0, 0, Mode);
		}
	}

//  ExitDrawing();
//  TraceDrawMode=oldmode;

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawPinsComps(int32 Mode)
{
	int32 cnt;
	CompRecord *Comp;
	int32 TempBackGroundActive;

	TempBackGroundActive = BackGroundActive;

	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	*/
	if ((Mode & 8) == 0)
	{
		if (!Printing)
			SetROP2(OutputDisplay, R2_COPYPEN);
	}
	else
	{
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		if (!Printing)
			SetROP2(OutputDisplay, R2_COPYPEN);

//    SetROP2(OutputDisplay,R2_XORPEN);
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((Mode & 8) == 8)
				Mode |= 0x40;

			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawPinsComp(Comp, 0.0, 0.0, 0, Mode);
		}
	}

//  ExitDrawing();
//  TraceDrawMode=oldmode;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPadsSoldPasteMaskComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode)
{
	int32 cnt2;
	ObjectRecord *Object;
	int32 TempBackGroundActive, OkToDraw;

	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	          0x100= 256 Layer != CurrentDrawingLayer
	*/
#ifdef _DEBUG
	int32 ok;

	if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		ok = 1;

	if (stricmpOwn(Comp->Name, "U1") == 0)
	{
		ok = 1;

		if ((Mode & 0x100) == 0)
			ok = 1;
	}

//  if (stricmpOwn(Shape->ShapeName,"pga321_026_zif")==0) {
//    ok=1;
//  }
#endif

	NrObjects = 0;
	ShapeOtherToObject(Comp, OX, OY, Rotation, -1, 1);

	TempBackGroundActive = BackGroundActive;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);
		OkToDraw = 0;

		switch (Object->Layer)
		{
		case SOLD_MASK_BOTTOM:
			if (DrawSoldMaskBottomMode == 2)
			{
				if ((Mode & 0x100) == 0)
				{
					if (CurrentDrawingLayer == 0)
						OkToDraw = 1;
				}
				else
				{
					if (CurrentDrawingLayer != 0)
						OkToDraw = 1;
				}
			}

			break;

		case SOLD_MASK_TOP:
			if (DrawSoldMaskTopMode == 2)
			{
				if ((Mode & 0x100) == 0)
				{
					if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
						OkToDraw = 1;
				}
				else
				{
					if (CurrentDrawingLayer != Design.NrBoardLayers - 1)
						OkToDraw = 1;
				}
			}

			break;

		case PASTE_MASK_BOTTOM:
			if (DrawPasteMaskBottomMode == 2)
			{
				if ((Mode & 0x100) == 0)
				{
					if (CurrentDrawingLayer == 0)
						OkToDraw = 1;
				}
				else
				{
					if (CurrentDrawingLayer != 0)
						OkToDraw = 1;
				}
			}

			break;

		case PASTE_MASK_TOP:
			if (DrawPasteMaskTopMode == 2)
			{
				if ((Mode & 0x100) == 0)
				{
					if (CurrentDrawingLayer == Design.NrBoardLayers - 1)
						OkToDraw = 1;
				}
				else
				{
					if (CurrentDrawingLayer != Design.NrBoardLayers - 1)
						OkToDraw = 1;
				}
			}

			break;
		}

		if (OkToDraw)
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawObject(Object, Mode);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawInfoObjectsComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode)
{
	int32 cnt2;
	ObjectRecord *Object;
	int32 TempBackGroundActive, OkToDraw;

	NrObjects = 0;
	ShapeOtherToObject(Comp, OX, OY, Rotation, -1, 2);

	TempBackGroundActive = BackGroundActive;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);
		OkToDraw = 0;

		switch (Object->Layer)
		{
		case INFO_LAYER:
			if (OkToDrawInfoObjects)
				OkToDraw = 1;

			break;

		case INFO_LAYER2:
			if (OkToDrawInfo2Objects)
				OkToDraw = 1;

			break;

		case INFO_LAYER3:
			if (OkToDrawInfo3Objects)
				OkToDraw = 1;

			break;

		case INFO_LAYER4:
			if (OkToDrawInfo4Objects)
				OkToDraw = 1;

			break;
		}

		if (OkToDraw)
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawObject(Object, Mode);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawBoardOutlineObjectsComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode)
{
	int32 cnt2;
	ObjectRecord *Object, NewObject;
	int32 TempBackGroundActive;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;


	if (!OkToDrawBoardOutline)
		return;

	memset(&PolygonBuf, 0, sizeof(PolygonBuf));
	PolygonObject = (PolygonRecord *) & PolygonBuf;

	memset(&NewObject, 0, sizeof(NewObject));
	NrObjects = 0;
	ShapeOtherToObject(Comp, OX, OY, Rotation, -1, 3);


	if ((Mode & 2) == 0)
	{
		if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		{
			SetROP2(OutputDisplay, SelectColorMode);
			DrawObjects(Mode | 1);
			SetROP2(OutputDisplay, R2_COPYPEN);
			return;
		}
	}

//  DrawObjects(Mode);


	if (Design.BoardOutlineKeepOut != 0.0)
	{
		TempBackGroundActive = BackGroundActive;
		InitDrawingObject(0, BOARD_OUTLINE_KEEPOUT_LAYER, 0, NORMAL_FILLED_AND_PEN1);

		for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
		{
			Object = &((*Objects)[cnt2]);

			if (TempBackGroundActive)
				SetBackGroundActive(0);

			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
				NewObject.x1 = Object->x1;
				NewObject.y1 = Object->y1;
				NewObject.x2 = Object->x2;
				NewObject.y2 = Object->y2;
				NewObject.ObjectType = TRACE_ALL_ANGLE;
				NewObject.Thickness = Design.BoardOutlineKeepOut;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);
				break;

			case OBJECT_RECT:
				NewObject.ObjectType = TRACE_ALL_ANGLE;
				NewObject.Thickness = Design.BoardOutlineKeepOut;
				NewObject.x1 = Object->x1 - Object->x2 * 0.5;
				NewObject.y1 = Object->y1 - Object->y2 * 0.5;
				NewObject.x2 = Object->x1 - Object->x2 * 0.5;
				NewObject.y2 = Object->y1 + Object->y2 * 0.5;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);

				NewObject.x1 = Object->x1 - Object->x2 * 0.5;
				NewObject.y1 = Object->y1 + Object->y2 * 0.5;
				NewObject.x2 = Object->x1 + Object->x2 * 0.5;
				NewObject.y2 = Object->y1 + Object->y2 * 0.5;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);

				NewObject.x1 = Object->x1 + Object->x2 * 0.5;
				NewObject.y1 = Object->y1 + Object->y2 * 0.5;
				NewObject.x2 = Object->x1 + Object->x2 * 0.5;
				NewObject.y2 = Object->y1 - Object->y2 * 0.5;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);

				NewObject.x1 = Object->x1 + Object->x2 * 0.5;
				NewObject.y1 = Object->y1 - Object->y2 * 0.5;
				NewObject.x2 = Object->x1 - Object->x2 * 0.5;
				NewObject.y2 = Object->y1 - Object->y2 * 0.5;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);
				break;

			case OBJECT_ARC:
				NewObject.x1 = Object->x1;
				NewObject.y1 = Object->y1;
				NewObject.x2 = Object->x2;
				NewObject.y2 = Object->y2;
				NewObject.x3 = Object->x3;
				NewObject.y3 = Object->y3;
				NewObject.x4 = Object->x4;
				NewObject.y4 = Object->y4;
				NewObject.ObjectType = TRACE_ARC;
				NewObject.Thickness = Design.BoardOutlineKeepOut;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
				DrawFilledPolygon(PolygonObject, 0);
				break;
			}
		}
	}

	TempBackGroundActive = BackGroundActive;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObject(Object, Mode);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawRoutingKeepoutObjectsComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode)
{
	int32 cnt, cnt2, ok, Layer;
	ObjectRecord *Object;
	int32 TempBackGroundActive;


#ifdef _DEBUG

	if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		ok = 1;

	if (stricmpOwn(Comp->Name, "Z100") == 0)
		ok = 1;

//  if (stricmpOwn(Shape->ShapeName,"pga321_026_zif")==0) {
//    ok=1;
//  }
#endif

	if ((!OkToDrawRoutingKeepoutTop) && (!OkToDrawRoutingKeepoutBottom) && (!OkToDrawRoutingKeepoutInner))
		return;

	/*
	  if (((DrawLayerCode[Design.NrBoardLayers-1] & 0x10) == 0x10)
	     &&
	     ((DrawLayerCode[0] & 0x10) == 0x10)) {
	    return;
	  }
	*/
#ifdef _DEBUG

	if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
		ok = 1;

	if (stricmpOwn(Comp->Name, "Z100") == 0)
		ok = 1;

//  if (stricmpOwn(Shape->ShapeName,"pga321_026_zif")==0) {
//    ok=1;
//  }
#endif

	NrObjects = 0;
	ShapeOtherToObject(Comp, OX, OY, Rotation, -1, 4);

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		Object->Info &= ~OBJECT_DONE;
	}

	// Drawing routing keepout non active layers first
	TempBackGroundActive = BackGroundActive;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);
		Layer = Object->Layer;

		if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		{
			Layer -= ROUTING_KEEPOUT_LAYER;

			if (Layer != CurrentDrawingLayer)
			{
				if (Layer == 0)
				{
					if ((OkToDrawRoutingKeepoutBottom) && ((DrawLayerCode[0] & 0x10) == 0))
					{
						if (TempBackGroundActive)
							SetBackGroundActive(0);

						DrawObject(Object, Mode);
					}
				}
				else
				{
					if (Layer == Design.NrBoardLayers - 1)
					{
						if ((OkToDrawRoutingKeepoutTop) && ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0))
						{
							if (TempBackGroundActive)
								SetBackGroundActive(0);

							DrawObject(Object, Mode);
						}
					}
					else
					{
						if ((OkToDrawRoutingKeepoutInner) && ((DrawLayerCode[Layer] & 0x10) == 0))
						{
							if (TempBackGroundActive)
								SetBackGroundActive(0);

							DrawObject(Object, Mode);
							Object->Info |= OBJECT_DONE;
						}
					}
				}
			}
		}
		else
			ok = 1;
	}

	// Drawing routing keepout active layer
	TempBackGroundActive = BackGroundActive;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);
		Layer = Object->Layer - ROUTING_KEEPOUT_LAYER;

		if (((Object->Info & OBJECT_DONE) == 0) && (Layer == CurrentDrawingLayer))
		{
			if (Layer == 0)
			{
				if ((OkToDrawRoutingKeepoutBottom) && ((DrawLayerCode[0] & 0x10) == 0))
				{
					if (TempBackGroundActive)
						SetBackGroundActive(0);

					DrawObject(Object, Mode);
				}
			}
			else
			{
				if (Layer == Design.NrBoardLayers - 1)
				{
					if ((OkToDrawRoutingKeepoutTop) && ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) == 0))
					{
						if (TempBackGroundActive)
							SetBackGroundActive(0);

						DrawObject(Object, Mode);
					}
				}
				else
				{
					if ((OkToDrawRoutingKeepoutInner) && ((DrawLayerCode[Layer] & 0x10) == 0))
					{
						if (TempBackGroundActive)
							SetBackGroundActive(0);

						DrawObject(Object, Mode);
						Object->Info |= OBJECT_DONE;
					}
				}
			}
		}
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DrawComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode)
{
	/*
	  Mode  = 0x01 = 1   Draw with double line
	          0x02 = 2   Xor
	          0x04 = 4   Background
	          0x08 = 8   clearance
	          0x10 = 16  Trace xor
	          0x20 = 32  Trace hatch
	          0x40 = 64  No initdrawing
	          0x80 = 128 Draw pads on bottom layer
	          0x100= 256 Layer != CurrentDrawingLayer
	*/
	int32 Mirror, TempBackGroundActive;
#ifdef _DEBUG
	int32 ok;
#endif

	Mirror = ((Comp->CompMode & 8) >> 3);
#ifdef _DEBUG

	if (stricmp(Comp->Name, "U1") == 0)
	{
		ok = 1;					// CurrentDrawingLayer
	}

#endif

	TempBackGroundActive = BackGroundActive;

	if (TempBackGroundActive)
		SetBackGroundActive(0);

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "U105") == 0)
		ok = 1;

#endif

	if ((Mode & 2) == 0)
	{
		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
			DrawInfoObjectsComp(Comp, OffsetX, OffsetY, Rotation, Mode);
	}

	if (OkToDrawBoardOutline)
	{
		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawBoardOutlineObjectsComp(Comp, OffsetX, OffsetY, Rotation, Mode);
		}
	}

	BackGroundActive = 0;

	if ((OkToDrawRoutingKeepoutTop) || (OkToDrawRoutingKeepoutBottom) || (OkToDrawRoutingKeepoutInner))
	{
		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawRoutingKeepoutObjectsComp(Comp, OffsetX, OffsetY, Rotation, Mode);
		}
	}

	BackGroundActive = 0;

	if (OkToDrawCompOutline)
	{
		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawOutlineComp(Comp, OffsetX, OffsetY, Rotation, Mode);
		}
	}

	BackGroundActive = 0;

	if ((OkToDrawSilkScreenTop) || (OkToDrawSilkScreenBottom))
	{
#ifdef _DEBUG

		if (stricmpOwn(Comp->Name, "C132") == 0)
			ok = 1;

#endif

		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawSilkScreenComp(Comp, OffsetX, OffsetY, Rotation, Mode);
		}
	}

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "K7") == 0)
		ok = 1;

	if ((Mode & 2) == 2)
		ok = 1;

#endif
	BackGroundActive = 0;

	if ((Mode & 2) == 0)
	{
		if ((DrawSoldMaskBottomMode == 2) || (DrawSoldMaskTopMode == 2) || (DrawPasteMaskBottomMode == 2)
		        || (DrawPasteMaskTopMode == 2))
		{
			if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
			{
				if (TempBackGroundActive)
					SetBackGroundActive(0);

				DrawPadsSoldPasteMaskComp(Comp, OffsetX, OffsetY, Rotation, Mode);
			}
		}
	}

	BackGroundActive = 0;

	if ((OkToDrawInnerPads) || (OkToDrawTopPads) || (OkToDrawBottomPads))
	{
		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawPinsComp(Comp, OffsetX, OffsetY, Rotation, Mode);
	}

	BackGroundActive = 0;

//  if (((Mode & 2) == 0)
//     &&
//     (OkToDrawDrills)) {
	if (DrawDrillMode > 0)
	{
#ifdef _DEBUG

		if (stricmpOwn(Comp->Name, "J4") == 0)
			ok = 1;

#endif

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawDrillsComp(Comp, OffsetX, OffsetY, Rotation, Mode);
	}

	BackGroundActive = 0;

#ifdef _DEBUG

	if (stricmp(Comp->Name, "C263") == 0)
	{
		ok = 1;					// CurrentDrawingLayer
	}

#endif

	if (OkToDrawCompValue)
	{
		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawValueComp(Comp, OffsetX, OffsetY, Rotation, Mode);
		}
	}

	BackGroundActive = 0;

	if (OkToDrawCompReference)
	{
		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawReferenceComp(Comp, OffsetX, OffsetY, Rotation, Mode);
		}
	}

	BackGroundActive = 0;

	if (OkToDrawCompPlacement)
	{
		if (((!Mirror) && (DrawTopComponents)) || ((Mirror) && (DrawBottomComponents)))
		{
			if (TempBackGroundActive)
				SetBackGroundActive(0);

			DrawPlacementOutlineComp(Comp, OffsetX, OffsetY, Rotation, Mode);
		}
	}

	BackGroundActive = 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawComp2(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode)
{
	/*
	  Mode  =  1   Draw with double line (clearance)
	           2   xor,black
	           4   Highlighted
	           8   clearance
	           16  Trace xor
	*/
	int32 TempBackGroundActive;

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "T23") == 0)
		ok = 1;

#endif
	TempBackGroundActive = BackGroundActive;

	if (OkToDrawCompOutline)
	{
		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawOutlineComp(Comp, OffsetX, OffsetY, Rotation, Mode);
	}

	if ((OkToDrawSilkScreenTop) || (OkToDrawSilkScreenBottom))
	{
		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawSilkScreenComp(Comp, OffsetX, OffsetY, Rotation, Mode);
	}

	if (OkToDrawCompValue)
	{
		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawValueComp(Comp, 0.0, 0.0, 0, Mode & 0x201);
	}

	if (OkToDrawCompReference)
	{
		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawReferenceComp(Comp, 0.0, 0.0, 0, Mode & 0x201);
	}

	if (OkToDrawBoardOutline)
	{
		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawBoardOutlineObjectsComp(Comp, 0.0, 0.0, Rotation, Mode & 0x201);
	}

	if (OkToDrawCompPlacement)
	{
		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawPlacementOutlineComp(Comp, OffsetX, OffsetY, Rotation, Mode);
	}

	BackGroundActive = 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DrawComps(int32 Mode)
{
	int32 cnt;
	CompRecord *Comp;

	/*
	  Mode  == 16   Layer != CurrentDrawingLayer
	  Mode  ==  8   With clearance
	*/
	if (Mode & 16)
		Mode = (Mode & ~16) + 0x100;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			DrawComp(Comp, 0.0, 0.0, 0, Mode);
	}

//  ExitDrawing();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DrawGrid()
//  round
{
	double Grid2;
	uint8 GridBits[512];
	HBITMAP GridBitmap, old;
	HDC PCBMemoryDC;
	int32 Xmin2, Xmax2, Ymin2, Ymax2;
	int32 Xpixels, Ypixels, cntx, cnty, x, y;

//  return;
	Grid2 = GridSize;

	Xmin2 = (int32) (PixelToRealOffX(-40) / GridSize) - 1;
	Ymin2 = (int32) (PixelToRealOffY(-40) / GridSize) - 1;
	Xmax2 = (int32) (PixelToRealOffX(DrawWindowMaxX + 40) / GridSize) + 1;
	Ymax2 = (int32) (PixelToRealOffY(DrawWindowMaxY + 40) / GridSize) + 1;

	/*
	  if (Xmin2<-10000) Xmin2=-10000;
	  if (Ymin2<-10000) Ymin2=-10000;
	  if (Xmax2>10000) Xmax2=10000;
	  if (Ymax2>10000) Ymax2=10000;
	*/
	Xpixels = min(max(Xmax2 - Xmin2, 1), 3000);
	Ypixels = min(max(Ymax2 - Ymin2, 1), 3000);
	SetTextColor(OutputDisplay, GraphicsObjectColor[GridObjectNr]);
	SetBkColor(OutputDisplay, RGB(0, 0, 0));

	if (DrawWindowMaxX / Xpixels > 3)
	{
		memset(&GridBits, 0xff, sizeof(GridBits));

		for (cntx = Xmin2; cntx < Xmax2; cntx++)
		{
			x = Mult(cntx * GridSize - Xoffset) + DrawWindowMinX;

			if ((x >= DrawWindowMinX) && (x < DrawWindowMaxX))
				GridBits[x >> 3] &= 255 - (128 >> (x & 7));
		}

		SetROP2(OutputDisplay, R2_XORPEN);
		GridBitmap = CreateBitmap(2048, 1, 1, 1, &GridBits);
		PCBMemoryDC = CreateCompatibleDC(OutputDisplay);
		old = SelectObject(PCBMemoryDC, GridBitmap);

		for (cnty = Ymin2; cnty < Ymax2; cnty++)
		{
			y = Mult(cnty * GridSize - Yoffset);
			y = DrawWindowMaxY - y - 1;

			if ((y >= DrawWindowMinY) && (y < DrawWindowMaxY))
			{
				BitBlt(OutputDisplay, 0, (int32) y, 2048, 1, PCBMemoryDC, 0, 0, SRCINVERT);
				/*
				        for (cntx=Xmin2;cntx<Xmax2;cntx++) {
				          x=Mult(cntx*GridSize-Xoffset);
				          if ((x>=DrawWindowMinX)
				             &&
				             (x<DrawWindowMaxX))
				            SetPixel(OutputDisplay,x,y,White);
				        }
				*/
			}
		}

		SelectObject(PCBMemoryDC, old);
		DeleteDC(PCBMemoryDC);
		DeleteObject(GridBitmap);
	}
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawTryingObjectCross(double CurrentX, double CurrentY, double MinX, double MinY, double MaxX, double MaxY,
                           int32 Mode)
{
	double cx, cy, dx, dy;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
	DrawLine(MultX(CurrentX), 4000, MultX(CurrentX), -4000);
	DrawLine(4000, MultY(CurrentY), -4000, MultY(CurrentY));
	cx = (MinX + MaxX) * 0.5;
	cy = (MinY + MaxY) * 0.5;
	dx = cx - CurrentX;
	dy = cy - CurrentY;

	if (Mode == 2)
	{
		DrawLine(MultX(MinX), MultY(MinY), MultX(MaxX), MultY(MinY));
		DrawLine(MultX(MaxX), MultY(MinY), MultX(MaxX), MultY(MaxY));
		DrawLine(MultX(MaxX), MultY(MaxY), MultX(MinX), MultY(MaxY));
		DrawLine(MultX(MinX), MultY(MaxY), MultX(MinX), MultY(MinY));
		DrawLine(MultX(CurrentX), MultY(CurrentY), MultX(cx), MultY(cy));
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CommandSelectPoint(double *OX, double *OY, double MinX, double MinY, double MaxX, double MaxY, int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 LeftButtonExit, CoordinateSet;

	SelectionEsc = 0;
	CoordinateSet = 0;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	LeftButtonExit = 0;

	OldX = CurrentX;
	OldY = CurrentY;
	DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
	ClipMouseCursor();
	SystemBusyMode = 8;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
				ScrollRight(ScrollSize);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
				ScrollDown(ScrollSize);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
				ScrollLeft(ScrollSize);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
				ScrollUp(ScrollSize);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
			}

//      InitInfoStr(CurrentX,CurrentY);
			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (!Focused)
		{
			DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);

		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
			*OX = CurrentX;
			*OY = CurrentY;
			SelectionEsc = 1;
			OldX = CurrentX;
			OldY = CurrentY;
			CoordinateSet = 1;
			CheckInputMessages(0);
		}

		if (RightButtonPressed)
		{
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectCross(OldX, OldY, MinX, MinY, MaxX, MaxY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help(0, 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectCross(CurrentX, CurrentY, MinX, MinY, MaxX, MaxY, Mode);

			ClipMouseCursor();
		}
	}

	UnClipMouseCursor();
	SystemBusyMode = 0;

	if (CoordinateSet)
		return 1;

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
