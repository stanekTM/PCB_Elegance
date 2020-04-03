/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw.c
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
#include "graphics.h"
#include "windows.h"
#include "memory.h"
#include "draw.h"
#include "draw2.h"
#include "calcdef.h"
#include "line2.h"
#include "rect.h"
#include "pcb.h"
#include "math.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "ellipss.h"
#include "string.h"
#include "toets.h"
#include "mainloop.h"
#include "stdio.h"
#include "polygon.h"
#include "font.h"

//#define Mult2(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr))) : ((int32)(Factor*(Nr))) )
#define Mult2(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr))) : ((int32)(Factor*(Nr))) )
#define Mult3(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr)+0.25)) : ((int32)(Factor*(Nr)-0.25)) )

double ViewMinX, ViewMinY, ViewMaxX, ViewMaxY;
int32 NrCrosses, ok;

extern int32 Printing, ReverseY;
extern COLORREF LineColor;
extern HGDIOBJ SpecialPen, SavePen, SaveBrush, EmptyBrush;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern double LineCrossX, LineCrossY;
extern HDC OutputDisplay;
extern int32 AreafillDrawMode, SelectColorMode;

// *******************************************************************************************************
// *******************************************************************************************************

int32 PaintAreaFill(AreaFillRecord * AreaFill, int32 mode);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void ViewWholeDesign(int32 mode)
{
#define  OVERSIZE  1.1

	double hulpx, hulpy, cx, cy, divx, divy, minx, miny, maxx, maxy;
	int32 ok;

	if ((mode == 2) && (ViewScale != 0.0))
	{
		Xoffset = ViewOffsetX;
		Yoffset = ViewOffsetY;
		Factor = ViewScale;
		return;
	}

	FindMinMaxBoard(&minx, &miny, &maxx, &maxy, 1);
	divx = maxx - minx;
	divy = maxy - miny;
	cx = (maxx + minx) * 0.5;
	cy = (maxy + miny) * 0.5;
	divx = divx * OVERSIZE;
	divy = divy * OVERSIZE;
	hulpx = divx / ClientWindowDivX;
	hulpy = divy / ClientWindowDivY;

	if (hulpx > hulpy)
	{
		hulpx = ClientWindowDivX;
		Factor = hulpx / divx;
		DisplX = divx;
		DisplY = DisplX * ClientWindowDivY / ClientWindowDivX;
		Xoffset = (cx - DisplX * 0.5);
		Yoffset = (cy - DisplY * 0.5);
		ok = 1;
	}
	else
	{
		hulpy = ClientWindowDivY;
		Factor = hulpy / divy;
		DisplY = divy;
		DisplX = DisplY * ClientWindowDivX / ClientWindowDivY;
		Xoffset = (cx - DisplX * 0.5);
		Yoffset = (cy - DisplY * 0.5);
	}

	if (mode == 1)
	{
		InvalidateRect(PCBWindow, NULL, 0);
		PostMessage(PCBWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTraces(int32 Mode)
{
	int32 Layer;

	if ((Mode & 16) == 16)
	{
		for (Layer = 0; Layer < 32; Layer++)
		{
			if (Layer != CurrentDrawingLayer)
			{
				DrawCode = DrawLayerCode[Layer];

				if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				{
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
					DrawHorTraces(Layer, Mode);
					DrawVerTraces(Layer, Mode);
					DrawDiag1Traces(Layer, Mode);
					DrawDiag2Traces(Layer, Mode);
				}
			}
		}

		return;
	}

	if (CurrentDrawingLayer == -1)
		return;

	DrawCode = DrawLayerCode[CurrentDrawingLayer];

	if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
	{
		InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
		DrawHorTraces(CurrentDrawingLayer, Mode);
		DrawVerTraces(CurrentDrawingLayer, Mode);
		DrawDiag1Traces(CurrentDrawingLayer, Mode);
		DrawDiag2Traces(CurrentDrawingLayer, Mode);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTracesWithClearance(int32 Mode)
{
	int32 Layer;

	if ((Mode & 16) == 16)
	{
		for (Layer = 0; Layer < 32; Layer++)
		{
			if (Layer != CurrentDrawingLayer)
			{
				DrawCode = DrawLayerCode[Layer];

				if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				{
					InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
					DrawHorTracesWithClearance(Layer, 0);
					DrawVerTracesWithClearance(Layer, 0);
					DrawDiag1TracesWithClearance(Layer, 0);
					DrawDiag2TracesWithClearance(Layer, 0);
				}
			}
		}

		return;
	}

	if (CurrentDrawingLayer == -1)
		return;

	DrawCode = DrawLayerCode[CurrentDrawingLayer];

	if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
	{
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawHorTracesWithClearance(CurrentDrawingLayer, 0);
		DrawVerTracesWithClearance(CurrentDrawingLayer, 0);
		DrawDiag1TracesWithClearance(CurrentDrawingLayer, 0);
		DrawDiag2TracesWithClearance(CurrentDrawingLayer, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVerTrace(TraceRecord * Trace)
{
	double xx1, yy1, yy2, yy2a, dikte2;
	int32 x1, y1, y2, dikte;
	int32 TraceInfo;

	xx1 = Trace->X;
	yy2a = Trace->ThickNess * 0.5;

	if ((xx1 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
	{
		yy1 = Trace->Y;
		yy2 = yy1 + Trace->Length;

		if ((yy2 + yy2a >= ViewMinY) && (yy1 - yy2a <= ViewMaxY))
		{
			x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
			y1 = Mult2(yy1 - Yoffset);
			y2 = Mult2(yy2 - Yoffset);
			dikte2 = Trace->ThickNess;
			dikte = Mult2(dikte2);
			TraceInfo = Trace->Info;

			if ((TraceInfo & OBJECT_CHANGED) != 0)
			{
				if ((TraceInfo & OBJECT_SELECTED) != 0)
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
						InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

					if (!Printing)
						SetROP2(OutputDisplay, SelectColorMode);

					DrawVerLine(x1, y1, y2, dikte, 0);

					if (!Printing)
						SetROP2(OutputDisplay, R2_COPYPEN);
				}
				else
				{
					if ((TraceInfo & OBJECT_HIGHLITED) != 0)
					{
						if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1HilitedObjectNr + DrawCode])
							InitDrawingObject(TRACE_HOR, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}

					DrawVerLine(x1, y1, y2, dikte, 0);
//          Trace->Info&=~(OBJECT_CHANGED_TO_NORMAL|OBJECT_CHANGED_SELECTED_TO_BLACK);
				}
			}
			else
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

				DrawVerLine(x1, y1, y2, dikte, 0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVerTrace2(TraceRecord * Trace, int32 mode)
{
	double xx1, yy1, yy2, yy2a, dikte2;
	int32 x1, y1, y2, dikte;

	xx1 = Trace->X;
	yy2a = Trace->ThickNess * 0.5;
	yy1 = Trace->Y;
	yy2 = yy1 + Trace->Length;
	x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
	y1 = Mult2(yy1 - Yoffset);
	y2 = Mult2(yy2 - Yoffset);
	dikte2 = Trace->ThickNess;
	dikte = Mult2(dikte2);
	DrawVerLine(x1, y1, y2, dikte, mode);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVerTraceWithClearance(TraceRecord * Trace)
{
	double xx1, yy1, yy2, yy2a, dikte2, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, y2, dikte;
	int32 TraceInfo;

	xx1 = Trace->X;
	yy1 = Trace->Y;
	yy2 = yy1 + Trace->Length;
	dikte2 = Trace->ThickNess + Trace->Clearance * 2;
	yy2a = dikte2 * 0.5;
	Xmin = xx1 - yy2a;
	Ymin = yy1 - yy2a;
	Xmax = xx1 + yy2a;
	Ymax = yy2 + yy2a;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
		y1 = Mult2(yy1 - Yoffset);
		y2 = Mult2(yy2 - Yoffset);
		dikte = Mult2(dikte2);
		TraceInfo = Trace->Info;
		DrawVerLine(x1, y1, y2, dikte, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVerTraces(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt;

	for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
	{
		Trace = &((*VerTraces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			DrawVerTrace(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVerTracesWithClearance(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt, dikte;
	double dikte2;

//  SetROP2(OutputDisplay,R2_XORPEN);
	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

	for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
	{
		Trace = &((*VerTraces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			dikte2 = Trace->ThickNess + Trace->Clearance * 2;
			dikte = Mult2(dikte2);

			if (dikte > 8)
				DrawVerTraceWithClearance(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawHorTrace(TraceRecord * Trace)
{

	double xx1, yy1, xx2, yy2a, dikte2;
	int32 x1, y1, x2, dikte;
	int32 TraceInfo;

	yy1 = Trace->Y;
	yy2a = Trace->ThickNess * 0.5;

	if ((yy1 + yy2a >= ViewMinY) && (yy1 - yy2a <= ViewMaxY))
	{
		xx1 = Trace->X;
		xx2 = xx1 + Trace->Length;
		dikte2 = Trace->ThickNess;

		if ((xx2 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
		{
			x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
			y1 = Mult2(yy1 - Yoffset);
			x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
			dikte = Mult2(dikte2);
			TraceInfo = Trace->Info;

			if ((TraceInfo & OBJECT_CHANGED) != 0)
			{
				if ((TraceInfo & OBJECT_SELECTED) != 0)
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
						InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

					if (!Printing)
						SetROP2(OutputDisplay, SelectColorMode);

					DrawHorLine(x1, y1, x2, dikte, 0);

					if (!Printing)
						SetROP2(OutputDisplay, R2_COPYPEN);
				}
				else
				{
					if ((TraceInfo & OBJECT_HIGHLITED) != 0)
					{
						if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1HilitedObjectNr + DrawCode])
							InitDrawingObject(TRACE_HOR, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}

					DrawHorLine(x1, y1, x2, dikte, 0);
//          Trace->Info&=~(OBJECT_CHANGED_TO_NORMAL|OBJECT_CHANGED_SELECTED_TO_BLACK);
				}
			}
			else
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

				DrawHorLine(x1, y1, x2, dikte, 0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawHorTrace2(TraceRecord * Trace, int32 mode)
{

	double xx1, yy1, xx2, yy2a, dikte2;
	int32 x1, y1, x2, dikte;

	yy1 = Trace->Y;
	yy2a = Trace->ThickNess * 0.5;
	xx1 = Trace->X;
	xx2 = xx1 + Trace->Length;
	dikte2 = Trace->ThickNess;
	x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
	y1 = Mult2(yy1 - Yoffset);
	x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
	dikte = Mult2(dikte2);
	DrawHorLine(x1, y1, x2, dikte, mode);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawHorTraceWithClearance(TraceRecord * Trace)
{
	double xx1, yy1, xx2, yy2a, dikte2, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, x2, dikte;

	xx1 = Trace->X;
	yy1 = Trace->Y;
	xx2 = xx1 + Trace->Length;
	dikte2 = Trace->ThickNess + Trace->Clearance * 2;
	yy2a = dikte2 / 2;
	Xmin = xx1 - yy2a;
	Ymin = yy1 - yy2a;
	Xmax = xx2 + yy2a;
	Ymax = yy1 + yy2a;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
		y1 = Mult2(yy1 - Yoffset);
		x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
		dikte = Mult2(dikte2);
		DrawHorLine(x1, y1, x2, dikte, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawHorTraces(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt;

	for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
	{
		Trace = &((*HorTraces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			DrawHorTrace(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawHorTracesWithClearance(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt, dikte;
	double dikte2;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

//  SetROP2(OutputDisplay,R2_XORPEN);
	for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
	{
		Trace = &((*HorTraces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			dikte2 = Trace->ThickNess + Trace->Clearance * 2;
			dikte = Mult2(dikte2);

			if (dikte > 8)
				DrawHorTraceWithClearance(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag1Trace(TraceRecord * Trace)
{
	double xx1, yy1, xx2, yy2, yy2a, dikte2;
	int32 x1, y1, x2, dikte, y2;
	int32 TraceInfo;

	xx1 = Trace->X;
	xx2 = xx1 + Trace->Length;
	dikte2 = Trace->ThickNess;
	yy2a = Trace->ThickNess * 0.5;

	if ((xx2 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
	{
		yy1 = Trace->Y;
		yy2 = yy1 - Trace->Length;

		if ((yy1 + yy2a >= ViewMinY) && (yy1 - (xx2 - xx1) - yy2a <= ViewMaxY))
		{
			x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
			y1 = Mult2(yy1 - Yoffset);
			x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
			y2 = Mult2(yy2 - Yoffset);

			if (y1 - y2 < x2 - x1)
				x2--;

			dikte = Mult2(dikte2);
			TraceInfo = Trace->Info;

			if ((TraceInfo & OBJECT_CHANGED) != 0)
			{
				if ((TraceInfo & OBJECT_SELECTED) != 0)
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
						InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

					if (!Printing)
						SetROP2(OutputDisplay, SelectColorMode);

					DrawDiag1Line(x1, y1, x2, dikte, 0);

					if (!Printing)
						SetROP2(OutputDisplay, R2_COPYPEN);
				}
				else
				{
					if ((TraceInfo & OBJECT_HIGHLITED) != 0)
					{
						if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1HilitedObjectNr + DrawCode])
							InitDrawingObject(TRACE_HOR, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}

					DrawDiag1Line(x1, y1, x2, dikte, 0);
//          Trace->Info&=~(OBJECT_CHANGED_TO_NORMAL|OBJECT_CHANGED_SELECTED_TO_BLACK);
				}
			}
			else
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

				DrawDiag1Line(x1, y1, x2, dikte, 0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag1Trace2(TraceRecord * Trace, int32 mode)
{

	double xx1, yy1, xx2, yy2, yy2a, dikte2;
	int32 x1, y1, x2, dikte, y2;

	xx1 = Trace->X;
	xx2 = xx1 + Trace->Length;
	dikte2 = Trace->ThickNess;
	yy2a = Trace->ThickNess * 0.5;
	yy1 = Trace->Y;
	yy2 = yy1 - Trace->Length;
	x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
	y1 = Mult2(yy1 - Yoffset);
	x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
	y2 = Mult2(yy2 - Yoffset);

	if (y1 - y2 < x2 - x1)
		x2--;

	dikte = Mult2(dikte2);
	DrawDiag1Line(x1, y1, x2, dikte, mode);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag1TraceWithClearance(TraceRecord * Trace)
{

	double xx1, yy1, xx2, yy2, yy2a, dikte2, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, x2, y2, dikte;

	xx1 = Trace->X;
	yy1 = Trace->Y;
	xx2 = xx1 + Trace->Length;
	yy2 = yy1 - Trace->Length;
	dikte2 = Trace->ThickNess + Trace->Clearance * 2;
	yy2a = dikte2 * 0.5;
	Xmin = xx1 - yy2a;
	Ymin = yy1 - (xx2 - xx1) - yy2a;
	Xmax = xx2 + yy2a;
	Ymax = yy1 + yy2a;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
		y1 = Mult2(yy1 - Yoffset);
		x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
		y2 = Mult2(yy2 - Yoffset);

		if (y1 - y2 < x2 - x1)
			x2--;

		dikte = Mult2(dikte2);
		DrawDiag1Line(x1, y1, x2, dikte, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag1Traces(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt;

	for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
	{
		Trace = &((*Diag1Traces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			DrawDiag1Trace(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag1TracesWithClearance(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt, dikte;
	double dikte2;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

//  SetROP2(OutputDisplay,R2_XORPEN);
	for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
	{
		Trace = &((*Diag1Traces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			dikte2 = Trace->ThickNess + Trace->Clearance * 2;
			dikte = Mult2(dikte2);

			if (dikte > 8)
				DrawDiag1TraceWithClearance(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag2Trace(TraceRecord * Trace)
{

	double xx1, yy1, xx2, yy2, yy2a, dikte2;
	int32 x1, y1, x2, y2, dikte;
	int32 TraceInfo;

	xx1 = Trace->X;
	xx2 = xx1 + Trace->Length;
	dikte2 = Trace->ThickNess;
	yy2a = Trace->ThickNess * 0.5;

	if ((xx2 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
	{
		yy1 = Trace->Y;
		yy2 = yy1 - Trace->Length;

		if ((yy1 + (xx2 - xx1) + yy2a >= ViewMinY) && (yy1 - yy2a <= ViewMaxY))
		{
			x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
			y1 = Mult2(yy1 - Yoffset);
			x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
			y2 = Mult2(yy2 - Yoffset);

			if (y2 - y1 < x2 - x1)
				x2--;

			dikte = Mult2(dikte2);
			TraceInfo = Trace->Info;

			if ((TraceInfo & OBJECT_CHANGED) != 0)
			{
				if ((TraceInfo & OBJECT_SELECTED) != 0)
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
						InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

					if (!Printing)
						SetROP2(OutputDisplay, SelectColorMode);

					DrawDiag2Line(x1, y1, x2, dikte, 0);

					if (!Printing)
						SetROP2(OutputDisplay, R2_COPYPEN);
				}
				else
				{
					if ((TraceInfo & OBJECT_HIGHLITED) != 0)
					{
						if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1HilitedObjectNr + DrawCode])
							InitDrawingObject(TRACE_HOR, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
					}

					DrawDiag2Line(x1, y1, x2, dikte, 0);
//          Trace->Info&=~(OBJECT_CHANGED_TO_NORMAL|OBJECT_CHANGED_SELECTED_TO_BLACK);
				}
			}
			else
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViewLayer1ObjectNr + DrawCode])
					InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

				DrawDiag2Line(x1, y1, x2, dikte, 0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag2Trace2(TraceRecord * Trace, int32 mode)
{

	double xx1, yy1, xx2, yy2, yy2a, dikte2;
	int32 x1, y1, x2, y2, dikte;

	xx1 = Trace->X;
	xx2 = xx1 + Trace->Length;
	dikte2 = Trace->ThickNess;
	yy2a = Trace->ThickNess * 0.5;
	yy1 = Trace->Y;
	yy2 = yy1 + Trace->Length;
	x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
	y1 = Mult2(yy1 - Yoffset);
	x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
	y2 = Mult2(yy2 - Yoffset);

	if (y2 - y1 < x2 - x1)
		x2--;

	dikte = Mult2(dikte2);
	DrawDiag2Line(x1, y1, x2, dikte, mode);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag2TraceWithClearance(TraceRecord * Trace)
{
	double xx1, yy1, xx2, yy2, yy2a, dikte2, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, x2, y2, dikte;

	xx1 = Trace->X;
	yy1 = Trace->Y;
	xx2 = xx1 + Trace->Length;
	yy2 = yy1 + Trace->Length;
	dikte2 = Trace->ThickNess + Trace->Clearance * 2;
	yy2a = dikte2 * 0.5;
	Xmin = xx1 - yy2a;
	Ymin = yy1 - yy2a;
	Xmax = xx2 + yy2a;
	Ymax = yy1 + (xx2 - xx1) + yy2a;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = Mult2(xx1 - Xoffset) + DrawWindowMinX;
		y1 = Mult2(yy1 - Yoffset);
		x2 = Mult2(xx2 - Xoffset) + DrawWindowMinX;
		y2 = Mult2(yy2 - Yoffset);

		if (y2 - y1 < x2 - x1)
			x2--;

		dikte = Mult2(dikte2);
		DrawDiag2Line(x1, y1, x2, dikte, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag2Traces(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt;

	for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
	{
		Trace = &((*Diag2Traces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			DrawDiag2Trace(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawDiag2TracesWithClearance(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	int32 cnt, dikte;
	double dikte2;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

//  SetROP2(OutputDisplay,R2_XORPEN);
	for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
	{
		Trace = &((*Diag2Traces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
//    if ((Trace->Info & (OBJECT_NOT_VISIBLE|3)) == 0) {
			dikte2 = Trace->ThickNess + Trace->Clearance * 2;
			dikte = Mult2(dikte2);

			if (dikte > 8)
				DrawDiag2TraceWithClearance(Trace);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectTrace(ObjectRecord * ObjectTrace)
{
	TraceRecord HulpTrace;
	NetRecord *Net;



	memset(&HulpTrace, 0, sizeof(TraceRecord));
	HulpTrace.X = (float) ObjectTrace->x1;
	HulpTrace.Y = (float) ObjectTrace->y1;
	HulpTrace.Length = (float) ObjectTrace->x2;
	HulpTrace.ThickNess = (float) ObjectTrace->y2;

	if ((ObjectTrace->NetNr >= 0) && (ObjectTrace->NetNr < Design.NrNets))
	{
		Net = &((*Nets)[ObjectTrace->NetNr]);

		if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
			HulpTrace.Info |= OBJECT_HIGHLITED;
	}

	HulpTrace.Info |= (ObjectTrace->Info & OBJECT_SELECTED);
	DrawCode = DrawLayerCode[ObjectTrace->Layer];

	if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
	{
		switch (ObjectTrace->ObjectType)
		{
		case TRACE_HOR:
			DrawHorTrace(&HulpTrace);
			break;

		case TRACE_VER:
			DrawVerTrace(&HulpTrace);
			break;

		case TRACE_DIAG1:
			DrawDiag1Trace(&HulpTrace);
			break;

		case TRACE_DIAG2:
			DrawDiag2Trace(&HulpTrace);
			break;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectTraceXor(ObjectRecord * ObjectTrace)
{
	TraceRecord HulpTrace;

	memset(&HulpTrace, 0, sizeof(TraceRecord));
	HulpTrace.X = (float) ObjectTrace->x1;
	HulpTrace.Y = (float) ObjectTrace->y1;
	HulpTrace.Length = (float) ObjectTrace->x2;
	HulpTrace.ThickNess = (float) ObjectTrace->y2;
	DrawCode = DrawLayerCode[ObjectTrace->Layer];

	if ((DrawCode >= 0) & (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
	{
		switch (ObjectTrace->ObjectType)
		{
		case TRACE_HOR:
			DrawHorTrace2(&HulpTrace, 1);
			break;

		case TRACE_VER:
			DrawVerTrace2(&HulpTrace, 1);
			break;

		case TRACE_DIAG1:
			DrawDiag1Trace2(&HulpTrace, 1);
			break;

		case TRACE_DIAG2:
			DrawDiag2Trace2(&HulpTrace, 1);
			break;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectTraceWithClearance(ObjectRecord * ObjectTrace)
{
	TraceRecord HulpTrace;

	memset(&HulpTrace, 0, sizeof(TraceRecord));
	HulpTrace.X = (float) ObjectTrace->x1;
	HulpTrace.Y = (float) ObjectTrace->y1;
	HulpTrace.Length = (float) ObjectTrace->x2;
	HulpTrace.ThickNess = (float) ObjectTrace->y2;
	HulpTrace.Clearance = (float) ObjectTrace->Clearance;

	switch (ObjectTrace->ObjectType)
	{
	case TRACE_HOR:
		DrawHorTraceWithClearance(&HulpTrace);
		break;

	case TRACE_VER:
		DrawVerTraceWithClearance(&HulpTrace);
		break;

	case TRACE_DIAG1:
		DrawDiag1TraceWithClearance(&HulpTrace);
		break;

	case TRACE_DIAG2:
		DrawDiag2TraceWithClearance(&HulpTrace);
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVia(ViaRecord * Via)
{
	double xx1, yy1, yy2, dikte, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, ThickNess, ViaInfo;

	xx1 = Via->X;
	yy1 = Via->Y;
	dikte = Via->ThickNess;
//  Drill=Via->DrillThickNess;
	yy2 = dikte / 2;
	Xmin = xx1 - yy2;
	Ymin = yy1 - yy2;
	Xmax = xx1 + yy2;
	Ymax = yy1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = Mult(xx1 - Xoffset) + DrawWindowMinX;
		y1 = Mult(yy1 - Yoffset);
		ThickNess = Mult(dikte);
		ViaInfo = Via->Info;

		if ((ViaInfo & OBJECT_CHANGED) != 0)
		{
			if ((ViaInfo & OBJECT_SELECTED) != 0)
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViaPinsObjectNr])
					InitDrawingObject(VIA_PUT_THROUGH_ROUND, 0, 0, NORMAL_FILLED_AND_PEN1);

				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);

				ellips2(x1, DrawWindowMaxY - y1 - 1, ThickNess, ThickNess, 255);

				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
			}
			else
			{
				if ((ViaInfo & OBJECT_HIGHLITED) != 0)
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ViaPinsHilitedObjectNr])
						InitDrawingObject(VIA_PUT_THROUGH_ROUND, 0, 0, HILITED_NORMAL_FILLED_AND_PEN1);
				}

				ellips2(x1, DrawWindowMaxY - y1 - 1, ThickNess, ThickNess, 255);
//      Via->Info&=~(OBJECT_CHANGED_TO_NORMAL|OBJECT_CHANGED_SELECTED_TO_BLACK);
			}
		}
		else
		{
			if (OkToDrawVias)
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViaPinsObjectNr])
					InitDrawingObject(VIA_PUT_THROUGH_ROUND, 0, 0, NORMAL_FILLED_AND_PEN1);

				if (ReverseY)
					ellips2(x1, DrawWindowMaxY - y1 - 1, ThickNess, ThickNess, 255);
				else
					ellips2(x1, y1, ThickNess, ThickNess, 255);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawViaDrill(ViaRecord * Via)
{
	double xx1, yy1, yy2, dikte, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, x2, ThickNess, yyy2a;

	if (DrawDrillMode == 0)
		return;

	if (!OkToDrawVias)
		return;

	xx1 = Via->X;
	yy1 = Via->Y;
	dikte = Via->DrillThickNess;
	yy2 = dikte / 2;
	Xmin = xx1 - yy2;
	Ymin = yy1 - yy2;
	Xmax = xx1 + yy2;
	Ymax = yy1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = MultX(xx1);
		y1 = MultY(yy1);
		ThickNess = Mult(dikte);
		yyy2a = Mult(dikte);

		switch (DrawDrillMode)
		{
		case 0:
			break;

		case 1:
			if (yyy2a > 6)
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViaPinsDrillObjectNr])
					InitDrawingObject(0, VIA_DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);

				ellips2(x1, y1, yyy2a, yyy2a, 255);
				x2 = Mult(dikte * SQRT05 * 0.5);

				if (x2 > 1)
				{
					DrawLine(x1 - x2, y1 - x2, x1 + x2, y1 + x2);
					DrawLine(x1 - x2, y1 + x2, x1 + x2, y1 - x2);
				}
			}

			break;

		case 2:
			if (yyy2a > 6)
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViaPinsDrillObjectNr])
					InitDrawingObject(0, VIA_DRILL_LAYER, 0, DRAW_WITH_PEN_AND_NO_BRUSH);

				ellips2(x1, y1, yyy2a, yyy2a, 255);
				x2 = Mult(dikte * 0.5);

				if (x2 > 1)
				{
					DrawLine(x1 - x2, y1, x1 + x2, y1);
					DrawLine(x1, y1 + x2, x1, y1 - x2);
				}
			}

			break;

		case 3:
			if (yyy2a > 2)
			{
				if (CurrentObjectCode != GraphicsObjectCodes[ViaPinsDrillObjectNr])
					InitDrawingObject(0, VIA_DRILL_LAYER, 0, DRAW_WITH_PEN_AND_BACKGROUND_BRUSH);

				ellips2(x1, y1, yyy2a, yyy2a, 255);
			}

			break;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVia2(ViaRecord * Via)
{
	double xx1, yy1, yy2, dikte, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, ThickNess, x2;

	xx1 = Via->X;
	yy1 = Via->Y;
	dikte = Via->ThickNess;
//  Drill=Via->DrillThickNess;
	yy2 = dikte / 2;
	Xmin = xx1 - yy2;
	Ymin = yy1 - yy2;
	Xmax = xx1 + yy2;
	Ymax = yy1 + yy2;
	x1 = Mult(xx1 - Xoffset) + DrawWindowMinX;
	y1 = Mult(yy1 - Yoffset);
	ThickNess = Mult(dikte);
	ellips2(x1, DrawWindowMaxY - y1 - 1, ThickNess, ThickNess, 255);
	x2 = Mult(Via->DrillThickNess);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectVia(ObjectRecord * ObjectVia)
{
	ViaRecord HulpVia;
	NetRecord *Net;

	Net = &((*Nets)[ObjectVia->NetNr]);
	memset(&HulpVia, 0, sizeof(ViaRecord));
	HulpVia.X = (float) ObjectVia->x1;
	HulpVia.Y = (float) ObjectVia->y1;
	HulpVia.ThickNess = (float) ObjectVia->x2;
	HulpVia.DrillThickNess = (float) ObjectVia->y2;

	if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
		HulpVia.Info = OBJECT_HIGHLITED;

	DrawVia(&HulpVia);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawViaWithClearance(ViaRecord * Via)
{
	double xx1, yy1, yy2, dikte, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, ThickNess;

	xx1 = Via->X;
	yy1 = Via->Y;
	dikte = Via->ThickNess + Via->Clearance * 2;
//  Drill=Via->DrillThickNess;
	yy2 = dikte / 2;
	Xmin = xx1 - yy2;
	Ymin = yy1 - yy2;
	Xmax = xx1 + yy2;
	Ymax = yy1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = Mult(xx1 - Xoffset) + DrawWindowMinX;
		y1 = Mult(yy1 - Yoffset);
		ThickNess = Mult(dikte);
		ellips2(x1, DrawWindowMaxY - y1 - 1, ThickNess, ThickNess, 255);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawViaWithPowerPads(ViaRecord * Via)
{
	double xx1, yy1, yy2, dikte, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, ThickNess;

	xx1 = Via->X;
	yy1 = Via->Y;
	dikte = Via->ThermalInner;
//  Drill=Via->DrillThickNess;
	yy2 = dikte / 2;
	Xmin = xx1 - yy2;
	Ymin = yy1 - yy2;
	Xmax = xx1 + yy2;
	Ymax = yy1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1 = Mult(xx1 - Xoffset) + DrawWindowMinX;
		y1 = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
		ThickNess = Mult(dikte);
		ellips2(x1, y1, ThickNess, ThickNess, 255);
//    for (cnt=0;cnt<10000000;cnt++) ;
//    while (!KeyPressed()) CheckInputMessages();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawVias(int32 Mode)
{
	int32 cnt;
	ViaRecord *Via;

	switch (Mode)
	{
	case 0:
		InitDrawingObject(VIA_PUT_THROUGH_ROUND, 0, 0, NORMAL_FILLED_AND_PEN1);
		break;

	case 8:
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
		break;
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			switch (Mode)
			{
			case 0:
				DrawVia(Via);
				break;

			case 8:
				DrawViaWithClearance(Via);
				break;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawViaDrills(int32 Mode)
{
	int32 cnt;
	ViaRecord *Via;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
			DrawViaDrill(Via);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawViaSoldMaskPads(int32 Mode)
{
	int32 cnt, ViaType;
	ViaRecord *Via;
	ObjectRecord Object;
#ifdef _DEBUG
	int32 ok;
#endif

	memset(&Object, 0, sizeof(ObjectRecord));
	Object.ObjectType = OBJECT_CIRCLE;
	Object.Info = OBJECT_FILLED;

	if (DrawSoldMaskBottomMode == 2)
	{
		Object.Layer = SOLD_MASK_BOTTOM;

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);
			ViaType = Via->ViaType & 3;
			ViaType &= ~VIA_SOLDMASK_BOTTOM;
#ifdef _DEBUG

			if (ViaType != 0)
				ok = 1;

#endif

			if (((Via->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0) && (ViaType == 0))
			{
				Object.x1 = Via->X;
				Object.y1 = Via->Y;
				Object.x2 = Via->SoldMask;
				Object.y2 = 15.0;
				DrawObject(&Object, 0);
			}
		}
	}

	if (DrawSoldMaskTopMode == 2)
	{
		Object.Layer = SOLD_MASK_TOP;

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);
			ViaType = Via->ViaType & 3;
			ViaType &= ~VIA_SOLDMASK_TOP;
#ifdef _DEBUG

			if (ViaType != 0)
				ok = 1;

#endif

			if (((Via->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0) && (ViaType == 0))
			{
				Object.x1 = Via->X;
				Object.y1 = Via->Y;
				Object.x2 = Via->SoldMask;
				Object.y2 = 15.0;
				DrawObject(&Object, 0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawSoldPastePadsVia(ViaRecord * Via)
{
	ObjectRecord Object;
	int32 TempBackGroundActive;

	memset(&Object, 0, sizeof(ObjectRecord));
	Object.ObjectType = OBJECT_CIRCLE;
	TempBackGroundActive = BackGroundActive;

	if (DrawSoldMaskBottomMode == 2)
	{
		Object.Layer = SOLD_MASK_BOTTOM;
		Object.x1 = Via->X;
		Object.y1 = Via->Y;
		Object.x2 = Via->SoldMask;
		Object.y2 = 15.0;
		DrawObject(&Object, 0);
	}

	if (DrawSoldMaskTopMode == 2)
	{
		Object.Layer = SOLD_MASK_TOP;
		Object.x1 = Via->X;
		Object.y1 = Via->Y;
		Object.x2 = Via->SoldMask;
		Object.y2 = 15.0;

		if (TempBackGroundActive)
			SetBackGroundActive(0);

		DrawObject(&Object, 0);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawConnection(ConnectionsRecord * Connection)
{
	double x1, y1, x2, y2, Xmax, Xmin, Ymax, Ymin;
	int32 ConnectionInfo;
	int32 x1a, y1a, x2a, y2a;

	if (!OkToDrawConnections)
		return;

	if ((Connection->Info & (CONNECTIONS_NOT_VISIBLE | OBJECT_HIGHLITED)) == CONNECTIONS_NOT_VISIBLE)
		return;

	x1 = Connection->x1;
	y1 = Connection->y1;
	x2 = Connection->x2;
	y2 = Connection->y2;

	if (x1 < x2)
	{
		Xmin = x1;
		Xmax = x2;
	}
	else
	{
		Xmin = x2;
		Xmax = x1;
	}

	if (y1 < y2)
	{
		Ymin = y1;
		Ymax = y2;
	}
	else
	{
		Ymin = y2;
		Ymax = y1;
	}

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1a = MultX(x1);
		y1a = MultY(y1);
		x2a = MultX(x2);
		y2a = MultY(y2);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & OBJECT_CHANGED) != 0)
		{
			if ((ConnectionInfo & OBJECT_SELECTED) != 0)
			{
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);

				DrawLine(x1a, y1a, x2a, y2a);

				/*
				        if (fabs(y1-y2)>fabs(x1-x2)) DrawLine(x1a+1,y1a,x2a+1,y2a);
				        else DrawLine(x1a,y1a-1,x2a,y2a-1);
				*/
//        SetROP2(OutputDisplay,R2_COPYPEN);
				if (!Printing)
					SetROP2(OutputDisplay, R2_XORPEN);
			}
			else
			{
				if ((ConnectionInfo & OBJECT_HIGHLITED) != 0)
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ConnectionsHilitedObjectNr])
						InitDrawingObject(0, CONNECTIONS_LAYER, 1, DRAW_WITH_HILITED_PEN_AND_NOT_FILLED);

//          SetROP2(OutputDisplay,R2_COPYPEN);
					if ((InRange(x1, x2)) && (InRange(y1, y2)))
						ellips2(x1a, y1a, Mult(10000.0), Mult(10000.0), 255);
					else
					{
						DrawLine(x1a, y1a, x2a, y2a);
//            if (fabs(y1-y2)>fabs(x1-x2)) DrawLine(Mult(x1-Xoffset)+1,y1a,x2a+1,y2a);
//            else DrawLine(Mult(x1-Xoffset),DrawWindowMaxY-Mult(y1-Yoffset)-2,x2a,DrawWindowMaxY-Mult(y2-Yoffset)-2);
					}
				}
			}
		}
		else
		{
			if (CurrentObjectCode != GraphicsObjectCodes[ConnectionsObjectNr])
				InitDrawingObject(0, CONNECTIONS_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);

			if ((InRange(x1, x2)) && (InRange(y1, y2)))
				ellips2(x1a, y1a, Mult(10000.0), Mult(10000.0), 255);
			else
			{
				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
				}
				else
					DrawLine(x1a, y1a, x2a, y2a);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawConnection2(ConnectionsRecord * Connection)
{
	double x1, y1, x2, y2, Xmax, Xmin, Ymax, Ymin;
	int32 x1a, y1a, x2a, y2a;

	if (!OkToDrawConnections)
		return;

	x1 = Connection->x1;
	y1 = Connection->y1;
	x2 = Connection->x2;
	y2 = Connection->y2;

	if (x1 < x2)
	{
		Xmin = x1;
		Xmax = x2;
	}
	else
	{
		Xmin = x2;
		Xmax = x1;
	}

	if (y1 < y2)
	{
		Ymin = y1;
		Ymax = y2;
	}
	else
	{
		Ymin = y2;
		Ymax = y1;
	}

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
//    ConnectionInfo=Connection->Info;
		x1a = Mult(x1 - Xoffset) + DrawWindowMinX;
		y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;
		x2a = Mult(x2 - Xoffset) + DrawWindowMinX;
		y2a = DrawWindowMaxY - Mult(y2 - Yoffset) - 1;
		SetBackGroundActive(0);
		InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawLine(x1a, y1a, x2a, y2a);
		/*
		    if (fabs(y1-y2)>fabs(x1-x2)) {
		      DrawLine(x1a+1,y1a,x2a+1,y2a);
		      DrawLine(x1a-1,y1a,x2a-1,y2a);
		    } else {
		      DrawLine(x1a,y1a-1,x2a,y2a-1);
		      DrawLine(x1a,y1a+1,x2a,y2a+1);
		    }
		*/
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawConnections(int32 Mode)
{
	int32 cnt;
	ConnectionsRecord *Connection;

	if (!OkToDrawConnections)
		return;

	InitDrawingObject(0, CONNECTIONS_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);

	if (!Printing)
		SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (cnt == 518)
				ok = 1;

#endif
			DrawConnection(Connection);
		}

//    if ((Connection->Info & (OBJECT_NOT_VISIBLE|OBJECT_NOT_VISIBLE|3)) == 0) DrawConnection(Connection);
	}

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

//  ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectLines(int32 Mode)
{
	int32 cnt;
	ObjectLineRecord *ObjectLine;
	ObjectRecord NewObject;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;
	/*
	  Mode  == 32   Other objects
	  Mode  == 16   Layer != CurrentDrawingLayer
	  Mode  ==  8   With clearance
	*/

	/*
	  res=R2_COPYPEN; // 13
	  res=R2_XORPEN;  // 7
	  res=R2_WHITE;   // 16
	  res=GetROP2(OutputDisplay);
	*/
	memset(&PolygonBuf, 0, sizeof(PolygonBuf));
	PolygonObject = (PolygonRecord *) & PolygonBuf;

	if ((Mode & 32) == 32)
	{
		if ((Design.BoardOutlineKeepOut != 0.0) && (OkToDrawObjectsLayer(BOARD_OUTLINE_LAYER) != -1))
		{
			InitDrawingObject(0, BOARD_OUTLINE_KEEPOUT_LAYER, 0, NORMAL_FILLED_AND_PEN1);
			memset(&NewObject, 0, sizeof(NewObject));

			for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
			{
				ObjectLine = &((*ObjectLines)[cnt]);

				if ((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (ObjectLine->Layer == BOARD_OUTLINE_LAYER)
					{
						NewObject.x1 = ObjectLine->X1;
						NewObject.y1 = ObjectLine->Y1;
						NewObject.x2 = ObjectLine->X2;
						NewObject.y2 = ObjectLine->Y2;
						NewObject.ObjectType = TRACE_ALL_ANGLE;
						NewObject.Thickness = Design.BoardOutlineKeepOut * 2.0;
						MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
						DrawFilledPolygon(PolygonObject, 0);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectLine->Layer) == 0))
				DrawObjectLine(ObjectLine, 0.0, 0.0, 0);
		}

		return;
	}

	if ((Mode & 8) == 8)
	{	// With clearances
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		if ((Mode & 16) == 0)
		{
			for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
			{
				ObjectLine = &((*ObjectLines)[cnt]);

				if (((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectLine->Layer) == 0)
				        && (CheckIfLayerHasObjectWithClearances(ObjectLine->Layer))
				        && (ObjectLine->Layer == CurrentDrawingLayer))
					DrawObjectLine(ObjectLine, 0.0, 0.0, 8 + 2);
			}
		}
		else
		{
			for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
			{
				ObjectLine = &((*ObjectLines)[cnt]);

				if (((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectLine->Layer) == 0)
				        && (CheckIfLayerHasObjectWithClearances(ObjectLine->Layer))
				        && (ObjectLine->Layer != CurrentDrawingLayer))
					DrawObjectLine(ObjectLine, 0.0, 0.0, 8 + 2);
			}
		}

		return;
	}

	if ((Mode & 16) == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectLine->Layer) == 0)
			        && (CheckIfCopperLayer(ObjectLine->Layer)) && (ObjectLine->Layer == CurrentDrawingLayer))
			{
#ifdef _DEBUG

				if (cnt == 106)
					ok = 1;

#endif
				DrawObjectLine(ObjectLine, 0.0, 0.0, 0);
			}
		}

		return;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectLine->Layer) == 0)
		        && (CheckIfCopperLayer(ObjectLine->Layer)) && (ObjectLine->Layer != CurrentDrawingLayer))
		{
#ifdef _DEBUG

			if (cnt == 106)
				ok = 1;

#endif
			DrawObjectLine(ObjectLine, 0.0, 0.0, 0);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectLine(ObjectLineRecord * ObjectLine, double OX, double OY, int32 Mode)
{
	// Mode
	//
	//  0x00  Normal
	//  0x01  XOR
	//  0x02  No initdrawing
	//  0x08  Clearance

	double x1, y1, x2, y2, Xmax, Xmin, Ymax, Ymin, Clearance;
	int32 Info, x1a, y1a, x2a, y2a, Layer, Thickness, cnt2, SegmentCount, LineSegments;
	uint8 PolygonBuf[10240];
	double LineBuf[128];
	PolygonRecord *PolygonObject;
	ObjectRecord NewObject;
	PolygonObject = (PolygonRecord *) & PolygonBuf;

	if (OkToDrawObjectsLayer(ObjectLine->Layer) == -1)
		return;

	Info = ObjectLine->Info;
	x1 = ObjectLine->X1;
	y1 = ObjectLine->Y1;
	x2 = ObjectLine->X2;
	y2 = ObjectLine->Y2;

	switch (Info & 3)
	{
	case 0:
	case 3:
		x1 += OX;
		y1 += OY;
		x2 += OX;
		y2 += OY;
		break;

	case 1:
		x1 += OX;
		y1 += OY;
		break;

	case 2:
		x2 += OX;
		y2 += OY;
		break;
	}

	if (x1 < x2)
	{
		Xmin = x1;
		Xmax = x2;
	}
	else
	{
		Xmin = x2;
		Xmax = x1;
	}

	if (y1 < y2)
	{
		Ymin = y1;
		Ymax = y2;
	}
	else
	{
		Ymin = y2;
		Ymax = y1;
	}

	Xmin -= ObjectLine->LineThickNess * 0.5;
	Ymin -= ObjectLine->LineThickNess * 0.5;
	Xmax += ObjectLine->LineThickNess * 0.5;
	Ymax += ObjectLine->LineThickNess * 0.5;

	if ((Mode & 8) == 8)
	{
		Clearance = 0.0;

		if ((ObjectLine->Layer >= 0) && (ObjectLine->Layer < Design.NrBoardLayers))
			Clearance = max(ObjectLine->Clearance, Design.StandardClearance);

		Xmin -= Clearance * 0.5;
		Ymin -= Clearance * 0.5;
		Xmax += Clearance * 0.5;
		Ymax += Clearance * 0.5;
	}

	if (ObjectLine->LineMode != 0)
	{
		Xmax += Design.DimensionHeight * 20.0;
		Ymax += Design.DimensionHeight * 20.0;
		Xmin -= Design.DimensionHeight * 20.0;
		Ymin -= Design.DimensionHeight * 20.0;
	}

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		Layer = ObjectLine->Layer;
		Thickness = Mult(ObjectLine->LineThickNess);

		if ((Mode & 2) == 0)
			InitDrawingObject(0, Layer, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED | USE_LAYER_DRAW_CODE);

		if (((Mode & 1) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		/*
		    if ((x1==x2)
		       &&
		       (y1==y2)) {
		      x1a=Mult(x1-Xoffset)+DrawWindowMinX;
		      if (ReverseY) y1a=DrawWindowMaxY-Mult(y1-Yoffset)-1;
		      else y1a=Mult(y1-Yoffset);
		      x2a=Mult(x2-Xoffset)+DrawWindowMinX;
		      if (ReverseY) y2a=DrawWindowMaxY-Mult(y2-Yoffset)-1;
		      else y2a=Mult(y1-Yoffset);
		      ellips2(x1a,y1a,Mult(1.0),Mult(1.0),255);
		    } else {
		*/
		if ((Mode & 8) == 0)
		{
			if (ObjectLine->LineMode != 0)
			{
				LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, ObjectLine->LineMode);
				SegmentCount = 0;

				for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
				{
					x1a = MultX(LineBuf[SegmentCount]);
					SegmentCount++;
					y1a = MultY(LineBuf[SegmentCount]);
					SegmentCount++;
					x2a = MultX(LineBuf[SegmentCount]);
					SegmentCount++;
					y2a = MultY(LineBuf[SegmentCount]);
					SegmentCount++;
					DrawLine(x1a, y1a, x2a, y2a);
				}
			}
			else
			{
				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);
				DrawLine(x1a, y1a, x2a, y2a);
			}
		}
		else
		{
			NewObject.x1 = ObjectLine->X1;
			NewObject.y1 = ObjectLine->Y1;
			NewObject.x2 = ObjectLine->X2;
			NewObject.y2 = ObjectLine->Y2;
			NewObject.Info = 0;
			NewObject.Test = 0;
			NewObject.Clearance = max(Design.MaximumClearance, ObjectLine->Clearance);
			NewObject.NetNr = ObjectLine->NetNr;
			NewObject.Layer = ObjectLine->Layer;
			NewObject.ObjectType = TRACE_ALL_ANGLE;
			NewObject.Thickness = ObjectLine->LineThickNess;
			MakePolygonFromObject(&NewObject, PolygonObject, NewObject.Clearance, 0.0, 1, 1);
			DrawFilledPolygon(PolygonObject, 0);
		}

		if (((Mode & 1) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectRects(int32 Mode)
{
	int32 cnt;
	ObjectRectRecord *ObjectRect;
	ObjectRecord NewObject;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;
	/*
	  Mode  == 32   Other objects
	  Mode  == 16   Layer != CurrentDrawingLayer
	  Mode  ==  8   With clearance
	*/

	memset(&PolygonBuf, 0, sizeof(PolygonBuf));
	PolygonObject = (PolygonRecord *) & PolygonBuf;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

	if ((Mode & 32) == 32)
	{
		if ((Design.BoardOutlineKeepOut != 0.0) && (OkToDrawObjectsLayer(BOARD_OUTLINE_LAYER) != -1))
		{
			InitDrawingObject(0, BOARD_OUTLINE_KEEPOUT_LAYER, 0, NORMAL_FILLED_AND_PEN1);
			memset(&NewObject, 0, sizeof(NewObject));

			for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
			{
				ObjectRect = &((*ObjectRects)[cnt]);

				if ((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (ObjectRect->Layer == BOARD_OUTLINE_LAYER)
					{
						NewObject.ObjectType = TRACE_ALL_ANGLE;
						NewObject.Thickness = Design.BoardOutlineKeepOut * 2.0;

						NewObject.x1 = ObjectRect->CentreX - ObjectRect->Width * 0.5;
						NewObject.y1 = ObjectRect->CentreY - ObjectRect->Height * 0.5;
						NewObject.x2 = ObjectRect->CentreX - ObjectRect->Width * 0.5;
						NewObject.y2 = ObjectRect->CentreY + ObjectRect->Height * 0.5;
						MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
						DrawFilledPolygon(PolygonObject, 0);

						NewObject.x1 = ObjectRect->CentreX - ObjectRect->Width * 0.5;
						NewObject.y1 = ObjectRect->CentreY + ObjectRect->Height * 0.5;
						NewObject.x2 = ObjectRect->CentreX + ObjectRect->Width * 0.5;
						NewObject.y2 = ObjectRect->CentreY + ObjectRect->Height * 0.5;
						MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
						DrawFilledPolygon(PolygonObject, 0);

						NewObject.x1 = ObjectRect->CentreX + ObjectRect->Width * 0.5;
						NewObject.y1 = ObjectRect->CentreY + ObjectRect->Height * 0.5;
						NewObject.x2 = ObjectRect->CentreX + ObjectRect->Width * 0.5;
						NewObject.y2 = ObjectRect->CentreY - ObjectRect->Height * 0.5;
						MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
						DrawFilledPolygon(PolygonObject, 0);

						NewObject.x1 = ObjectRect->CentreX + ObjectRect->Width * 0.5;
						NewObject.y1 = ObjectRect->CentreY - ObjectRect->Height * 0.5;
						NewObject.x2 = ObjectRect->CentreX - ObjectRect->Width * 0.5;
						NewObject.y2 = ObjectRect->CentreY - ObjectRect->Height * 0.5;
						MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
						DrawFilledPolygon(PolygonObject, 0);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if (((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectRect->Layer) == 0)
			        && (!CheckIfCopperLayer(ObjectRect->Layer)))
				DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
		}

		return;
	}

	if ((Mode & 8) == 8)
	{	// With clearances
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		if ((Mode & 16) == 0)
		{
			for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
			{
				ObjectRect = &((*ObjectRects)[cnt]);

				if (((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectRect->Layer) == 0)
				        && (ObjectRect->Layer == CurrentDrawingLayer) && (CheckIfCopperLayer(ObjectRect->Layer)))
					DrawObjectRect(ObjectRect, 0.0, 0.0, 8 + 2);
			}

			return;
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if (((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectRect->Layer) == 0)
			        && (ObjectRect->Layer != CurrentDrawingLayer) && (CheckIfCopperLayer(ObjectRect->Layer)))
				DrawObjectRect(ObjectRect, 0.0, 0.0, 8 + 2);
		}

		return;
	}

	if ((Mode & 16) == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if (((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectRect->Layer) == 0)
			        && (ObjectRect->Layer == CurrentDrawingLayer) && (CheckIfCopperLayer(ObjectRect->Layer)))
				DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
		}

		return;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectRect->Layer) == 0)
		        && (ObjectRect->Layer != CurrentDrawingLayer) && (CheckIfCopperLayer(ObjectRect->Layer)))
			DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectRect(ObjectRectRecord * ObjectRect, double OX, double OY, int32 Mode)
{
	// Mode
	//
	//  0x00  Normal
	//  0x01  XOR,Filled
	//  0x02  No initdrawing

	double x1, y1, x2, y2, xx2, yy2, Xmax, Xmin, Ymax, Ymin, Clearance;
	int32 Info, x1a, y1a, Thickness, Layer;

	if (OkToDrawObjectsLayer(ObjectRect->Layer) == -1)
		return;

	Clearance = 0.0;

	if ((ObjectRect->Layer >= 0) && (ObjectRect->Layer < Design.NrBoardLayers))
		Clearance = max(ObjectRect->Clearance, Design.StandardClearance);

	y1a = 0;
	x1 = ObjectRect->CentreX + OX;
	y1 = ObjectRect->CentreY + OY;
	x2 = ObjectRect->Width;
	y2 = ObjectRect->Height;

	if ((Mode & 8) == 8)
	{
		x2 += Clearance * 2.0;
		y2 += Clearance * 2.0;
	}

	xx2 = x2 / 2;
	yy2 = y2 / 2;
	Xmin = x1 - xx2;
	Ymin = y1 - yy2;
	Xmax = x1 + xx2;
	Ymax = y1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		Layer = ObjectRect->Layer;
		Thickness = Mult(ObjectRect->LineThickNess);

		if ((Mode & 2) == 0)
		{
			if ((ObjectRect->Info & OBJECT_FILLED) == OBJECT_FILLED)
			{
				if ((Mode & 1) == 1)
					InitDrawingObject(TRACE_HOR, Layer, 0, NORMAL_FILLED_AND_NO_PEN | USE_LAYER_DRAW_CODE);
				else
					InitDrawingObject(TRACE_HOR, Layer, 0, NORMAL_FILLED_AND_PEN1 | USE_LAYER_DRAW_CODE);
			}
			else
				InitDrawingObject(0, Layer, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED | USE_LAYER_DRAW_CODE);
		}

		x1a = Mult(x1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;

		Info = ObjectRect->Info;

		if (((Mode & 1) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if ((Info & OBJECT_SELECTED) == 0)
			rect3(x1a, y1a, max(2, Mult(x2)), max(2, Mult(y2)));
		else
			rect4(x1a, y1a, max(2, Mult(x2)), max(2, Mult(y2)));

		if (((Mode & 1) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectArcs(int32 Mode, int32 Mode2)
{
	int32 cnt;
	ObjectArcRecord *ObjectArc;
	ObjectRecord NewObject;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;

	memset(&PolygonBuf, 0, sizeof(PolygonBuf));
	PolygonObject = (PolygonRecord *) & PolygonBuf;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

	/*
	  Mode  == 32   Other objects
	  Mode  == 16   Layer != CurrentDrawingLayer
	  Mode  ==  8   With clearance
	*/

	if (Mode2 & 1)
	{
		if ((Mode & 8) == 8)
		{	// With clearances
			InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

			for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
				        && (CheckIfDrillLayer(ObjectArc->Layer)))
					DrawObjectArc(ObjectArc, 0.0, 0.0, 8 + 2);
			}
		}
		else
		{
			for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
				        && (CheckIfDrillLayer(ObjectArc->Layer)))
					DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
			}
		}

		return;
	}

	if ((Mode & 32) == 32)
	{
		if ((Design.BoardOutlineKeepOut != 0.0) && (OkToDrawObjectsLayer(BOARD_OUTLINE_LAYER) != -1))
		{
			InitDrawingObject(0, BOARD_OUTLINE_KEEPOUT_LAYER, 0, NORMAL_FILLED_AND_PEN1);
			memset(&NewObject, 0, sizeof(NewObject));

			for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if ((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (ObjectArc->Layer == BOARD_OUTLINE_LAYER)
					{
						NewObject.x1 = ObjectArc->CentreX;
						NewObject.y1 = ObjectArc->CentreY;
						NewObject.x2 = ObjectArc->Width;
						NewObject.y2 = ObjectArc->Height;
						NewObject.x3 = ObjectArc->StartDiffX;
						NewObject.y3 = ObjectArc->StartDiffY;
						NewObject.x4 = ObjectArc->EndDiffX;
						NewObject.y4 = ObjectArc->EndDiffY;
						NewObject.ObjectType = TRACE_ARC;
						NewObject.Thickness = Design.BoardOutlineKeepOut * 2.0;
						MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);
						DrawFilledPolygon(PolygonObject, 0);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
			        && (!CheckIfCopperLayer(ObjectArc->Layer)))
				DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
		}

		return;
	}

	if ((Mode & 8) == 8)
	{	// With clearances
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		if ((Mode & 16) == 0)
		{
			for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
				        && (CheckIfLayerHasObjectWithClearances(ObjectArc->Layer)))
					DrawObjectArc(ObjectArc, 0.0, 0.0, 8 + 2);
			}
		}
		else
		{
			for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
				        && (CheckIfLayerHasObjectWithClearances(ObjectArc->Layer)))
					DrawObjectArc(ObjectArc, 0.0, 0.0, 8 + 2);
			}
		}

		return;
	}

	if ((Mode & 16) == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
			        && (CheckIfCopperLayer(ObjectArc->Layer)) && (ObjectArc->Layer == CurrentDrawingLayer))
				DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
		}

		return;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
		        && (CheckIfCopperLayer(ObjectArc->Layer)) && (ObjectArc->Layer != CurrentDrawingLayer))
			DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectArc(ObjectArcRecord * ObjectArc, double OX, double OY, int32 Mode)
{
	// Mode
	//
	//  0x00  Normal
	//  0x01  XOR
	//  0x02  No initdrawing
	//  0x08  Clearance

	double x1, y1, x2, y2, x3, y3, x4, y4, xx2, yy2, xx2a, yy2a, Xmax, Xmin, Ymax, Ymin, Clearance;
	int32 Info, Thickness, Layer, x1a, y1a, x2a;
	uint8 PolygonBuf[16384];
	PolygonRecord *PolygonObject;
	ObjectRecord NewObject;
	PolygonObject = (PolygonRecord *) & PolygonBuf;

	Layer = ObjectArc->Layer;

	if (OkToDrawObjectsLayer(Layer) == -1)
		return;

	Clearance = 0.0;

	if ((Mode & 8) == 8)
	{
		if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER)
		        || ((Layer >= 0) && (ObjectArc->Layer < Design.NrBoardLayers)))
			Clearance = max(ObjectArc->Clearance, Design.StandardClearance);
	}

	x1 = ObjectArc->CentreX + OX;
	y1 = ObjectArc->CentreY + OY;
	x2 = ObjectArc->Width;
	y2 = ObjectArc->Height;
	xx2 = x2 * 0.5;
	xx2a = xx2;
	yy2 = y2 * 0.5;
	yy2a = yy2;

	if ((Mode & 8) == 8)
	{
		xx2a += Clearance * 2.0;
		yy2a += Clearance * 2.0;
	}

//  yy2=y2/2;
	Xmin = x1 - xx2a;
	Ymin = y1 - yy2a;
	Xmax = x1 + xx2a;
	Ymax = y1 + yy2a;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		Thickness = Mult(ObjectArc->LineThickNess);

		Info = ObjectArc->Info;

		if ((Mode & 2) == 0)
		{
			if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
			{
				if ((Info & OBJECT_SELECTED) == 0)
					InitDrawingObject(0, Layer, 0, DRAW_WITH_PEN_AND_BACKGROUND_BRUSH);
				else
					InitDrawingObject(0, Layer, 0, DRAW_WITH_PEN_AND_NO_BRUSH);
			}
			else
			{
				if ((Info & OBJECT_FILLED) == OBJECT_FILLED)
				{
					if ((Mode & 1) == 1)
						InitDrawingObject(TRACE_HOR, Layer, 0, NORMAL_FILLED_AND_NO_PEN | USE_LAYER_DRAW_CODE);
					else
					{
//            InitDrawingObject(TRACE_HOR,Layer,0,NORMAL_FILLED_AND_NO_PEN|USE_LAYER_DRAW_CODE);
						InitDrawingObject(TRACE_HOR, Layer, 0, NORMAL_FILLED_AND_PEN1 | USE_LAYER_DRAW_CODE);
					}
				}
				else
					InitDrawingObject(0, Layer, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED | USE_LAYER_DRAW_CODE);
			}
		}

//    if (InitDrawingObjectsLayer(Layer,Thickness,0)==-1) return;
		x3 = ObjectArc->StartDiffX;
		y3 = ObjectArc->StartDiffY;
		x4 = ObjectArc->EndDiffX;
		y4 = ObjectArc->EndDiffY;

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
		{
			if ((Mode & 8) == 8)
				x2 += Clearance * 2;

			ellips2(MultX(x1), MultY(y1), Mult(x2), Mult(x2), 255);

			if ((Mode & 8) == 0)
			{
				x1a = MultX(x1);
				y1a = MultY(y1);

				switch (DrawDrillMode)
				{
				case 0:
					break;

				case 1:
					x2a = Mult(x2 * SQRT05 * 0.5);

					if (x2a > 1)
					{
						DrawLine(x1a - x2a, y1a - x2a, x1a + x2a, y1a + x2a);
						DrawLine(x1a - x2a, y1a + x2a, x1a + x2a, y1a - x2a);
					}

					break;

				case 2:
					x2a = Mult(x2 * 0.5);

					if (x2a > 1)
					{
						DrawLine(x1a - x2a, y1a, x1a + x2a, y1a);
						DrawLine(x1a, y1a + x2a, x1a, y1a - x2a);
					}

					break;
				}
			}
		}
		else
		{
			if ((ObjectArc->Info & OBJECT_FILLED) == 0)
			{
				if ((Mode & 8) == 0)
				{
					SpecialArc(MultX(x1), MultY(y1), Mult(x2) + 1, Mult(y2) + 1, MultX(x1 + x3), MultY(y1 + y3),
					           MultX(x1 + x4), MultY(y1 + y4));
				}
				else
				{
					NewObject.x1 = ObjectArc->CentreX;
					NewObject.y1 = ObjectArc->CentreY;
					NewObject.x2 = ObjectArc->Width;
					NewObject.y2 = ObjectArc->Height;
					NewObject.x3 = ObjectArc->StartDiffX;
					NewObject.y3 = ObjectArc->StartDiffY;
					NewObject.x4 = ObjectArc->EndDiffX;
					NewObject.y4 = ObjectArc->EndDiffY;
					NewObject.Info = 0;
					NewObject.Test = 0;
					NewObject.Layer = ObjectArc->Layer;
					NewObject.ObjectType = TRACE_ARC;
					NewObject.NetNr = ObjectArc->NetNr;
					NewObject.Clearance = max(Design.MaximumClearance, ObjectArc->Clearance);
					NewObject.Thickness = ObjectArc->LineThickNess;
					MakePolygonFromObject(&NewObject, PolygonObject, NewObject.Clearance, 0.0, 1, 1);
					DrawFilledPolygon(PolygonObject, 0);
				}
			}
			else
			{
				if ((Mode & 8) == 8)
					x2 += Clearance * 2;

				ellips2(MultX(x1), MultY(y1), Mult(x2), Mult(x2), 255);
			}
		}

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectTexts2(int32 Mode)
{
	int32 cnt;
	ObjectTextRecord2 *ObjectText2;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

	/*
	  Mode  == 32   Other objects
	  Mode  == 16   Layer != CurrentDrawingLayer
	  Mode  ==  8   With clearance
	*/

	if ((Mode & 32) == 32)
	{
		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if (((ObjectText2->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectText2->Layer) == 0)
			        && (!CheckIfCopperLayer(ObjectText2->Layer)))
				DrawObjectText2(ObjectText2, 0.0, 0.0, 0.0, 0);
		}

		return;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectText2->Layer) == 0)
		        && (CheckIfCopperLayer(ObjectText2->Layer)))
			DrawObjectText2(ObjectText2, 0.0, 0.0, 0.0, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectText2(ObjectTextRecord2 * ObjectText2, double OX, double OY, double NewRotation, int32 Mode)
{
	// Mode
	//
	//  0x00  Normal
	//  0x01  XOR
	//  0x02  No initdrawing

	double x1, y1, x2, y2, x3, x4, y4, Xmax, Xmin, Ymax, Ymin, Rotation;
	int32 Info, TextAlignment, Mirror, FontNr, cnt, MaxCountX, Thickness, Layer, NrLines, Length, ok;

	if (OkToDrawObjectsLayer(ObjectText2->Layer) == -1)
		return;

	if ((Length = strlen(ObjectText2->Text)) == 0)
		return;

	x1 = ObjectText2->X;
	y1 = ObjectText2->Y;
	x2 = x1 + OX;
	y2 = y1 + OY;
	x4 = x2;
	y4 = y2;
	x3 = ObjectText2->FontHeight;
	Rotation = ObjectText2->Rotation;
	TextAlignment = ObjectText2->TextMode & 0x0f;
	Mirror = (ObjectText2->TextMode & 0x10) >> 4;
	FontNr = ObjectText2->FontNr;
	Xmin = 1e9;
	Xmax = -1e9;
	Ymin = 1e9;
	Ymax = -1e9;

	if ((NrLines = ConvertObjectTextToStrings(ObjectText2->Text, FontNr, &MaxCountX, ObjectText2->Layer)) == 0)
		return;

	x2 = x1 + OX;
	y2 = y1 + OY;

	for (cnt = 0; cnt < NrLines; cnt++)
	{
		if (FontNr == 0)
			GetMinMaxText2(x2, y2, x3, FontNr, Rotation + NewRotation, TextAlignment, Mirror, TextStrings2[cnt]);
		else
			GetMinMaxText2b(x2, y2, x3, FontNr, Rotation + NewRotation, TextAlignment, Mirror, TextStrings[cnt]);

		Xmin = min(Xmin, TextMinX);
		Ymin = min(Ymin, TextMinY);
		Xmax = max(Xmax, TextMaxX);
		Ymax = max(Ymax, TextMaxY);

		if (FontNr == 0)
		{
			if (Mirror == 0)
				x2 += sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.1;
			else
				x2 -= sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.1;

			y2 -= cos(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.1;
		}
		else
		{
			if (Mirror == 0)
				x2 += sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.4;
			else
				x2 -= sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.4;

			y2 -= cos(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.4;
		}
	}

	ok = 1;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		Layer = ObjectText2->Layer;
#ifdef _DEBUG

		if (Layer == SOLD_MASK_TOP)
			ok = 1;

#endif
		Info = ObjectText2->Info;

		if (FontNr == 0)
		{
			Thickness = Mult(ObjectText2->LineThickNess);

			if ((Mode & 2) == 0)
				InitDrawingObject(0, Layer, Thickness, DRAW_WITH_PEN_AND_NOT_FILLED | USE_LAYER_DRAW_CODE);
		}
		else
		{
			if ((Mode & 2) == 0)
				InitDrawingObject(0, Layer, 1, DRAW_WITH_PEN_AND_NOT_FILLED | USE_LAYER_DRAW_CODE);
		}

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

//    DrawStr2(x2,y2,x3,4*2540.0,TextRotation,TextAlignment,ObjectText2->Text);
		x2 = x1 + OX;
		y2 = y1 + OY;

		for (cnt = 0; cnt < NrLines; cnt++)
		{
			if (FontNr == 0)
				DrawStrWithRotation(x2, y2, x3, Rotation + NewRotation, 0, Mirror, TextStrings2[cnt]);
			else
			{
				DrawTrueTypeStrWithRotation(x2, y2, x3, FontNr, Rotation + NewRotation, 0, Mirror, TextStrings[cnt],
				                            Layer, Mode);
			}

			if (FontNr == 0)
			{
				if (Mirror == 0)
					x2 += sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.1;
				else
					x2 -= sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.1;

				y2 -= cos(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.1;
			}
			else
			{
				if (Mirror == 0)
					x2 += sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.4;
				else
					x2 -= sin(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.4;

				y2 -= cos(ANGLE_CONVERT(Rotation + NewRotation)) * x3 * 1.4;
			}
		}

//    InitDrawingColorWhite();
//    ellips2(Mult(x2-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(y2-Yoffset)-1,5,5,255);
		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectPolygons(int32 Mode)
{
	int32 cnt;
	ObjectPolygonRecord *ObjectPolygon;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

	/*
	  Mode  == 32   Other objects
	  Mode  == 16   Layer != CurrentDrawingLayer
	  Mode  ==  8   With clearance
	*/

	if ((Mode & 32) == 32)
	{
		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectPolygon->Layer) == 0)
			        && (!CheckIfCopperLayer(ObjectPolygon->Layer)))
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
		}

		return;
	}

	if ((Mode & 8) == 8)
	{	// With clearances
		InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

		if ((Mode & 16) == 0)
		{
			for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

				if (((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0)
				        && (OkToDrawObjectsLayer(ObjectPolygon->Layer) == 0) && (CheckIfCopperLayer(ObjectPolygon->Layer))
				        && (ObjectPolygon->Layer == CurrentDrawingLayer))
					DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 8);
			}
		}
		else
		{
			for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

				if (((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0)
				        && (OkToDrawObjectsLayer(ObjectPolygon->Layer) == 0) && (CheckIfCopperLayer(ObjectPolygon->Layer))
				        && (ObjectPolygon->Layer != CurrentDrawingLayer))
					DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 8);
			}
		}

		return;
	}

	if ((Mode & 16) == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
#ifdef _DEBUG

			if (cnt == 6)
				ok = 1;

#endif
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectPolygon->Layer) == 0)
			        && (CheckIfCopperLayer(ObjectPolygon->Layer)) && (ObjectPolygon->Layer == CurrentDrawingLayer))
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
		}

		return;
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0) && (OkToDrawObjectsLayer(ObjectPolygon->Layer) == 0)
		        && (CheckIfCopperLayer(ObjectPolygon->Layer)) && (ObjectPolygon->Layer != CurrentDrawingLayer))
			DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OX, double OY, int32 Mode)
{
	// Mode
	//
	//  0x00  Normal
	//  0x01  XOR
	//  0x02  No initdrawing

	int32 Info, Layer, cnt3, x, y, count, MemSize;
	PointsArray3 *Points;
	PolygonRecord *NewPolygon, *BiggerPolygon;

	if (OkToDrawObjectsLayer(ObjectPolygon->Layer) == -1)
		return;

	count = ObjectPolygon->NrVertices;
	AllocateSpecialMem(MEM_POINTS, count * sizeof(POINT) + 16384, (void **) &Points);

	if ((ObjectPolygon->maxx + OX >= ViewMinX) && (ObjectPolygon->minx + OX <= ViewMaxX)
	        && (ObjectPolygon->maxy + OY >= ViewMinY) && (ObjectPolygon->miny + OY <= ViewMaxY))
	{
		Layer = ObjectPolygon->Layer;

		if ((Mode & 8) == 0)
		{
			if (Layer < 32)
				InitDrawingObject(TRACE_HOR, Layer, 0, NORMAL_FILLED_AND_PEN1 | USE_LAYER_DRAW_CODE);
			else
				InitDrawingObject(0, Layer, 0, NORMAL_FILLED_AND_PEN1 | USE_LAYER_DRAW_CODE);

			Info = ObjectPolygon->Info;

			if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
			{
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);
			}

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				x = MultX((*ObjectPolygon).Points[cnt3].x + OX);

				if (ReverseY)
					y = MultY((*ObjectPolygon).Points[cnt3].y + OY);
				else
					y = MultY2((*ObjectPolygon).Points[cnt3].y + OY);

				(*Points)[cnt3].x = x;
				(*Points)[cnt3].y = y;
			}

			Polygon(OutputDisplay, (POINT *) Points, count);

			if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
			{
				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
			}
		}
		else
		{
			MemSize = MemSizeObjectPolygon(ObjectPolygon) + 10240;
			AllocateSpecialMem(MEM_POLYGON_BIGGER, MemSize, (void **) &BiggerPolygon);
			AllocateSpecialMem(MEM_POLYGON_BIGGER2, MemSize, (void **) &NewPolygon);
			memset(NewPolygon, 0, MemSize);
			NewPolygon->NrVertices = count;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				NewPolygon->Points[cnt3].x = (*ObjectPolygon).Points[cnt3].x + OX;
				NewPolygon->Points[cnt3].y = (*ObjectPolygon).Points[cnt3].y + OX;
			}

			MakeBiggerSmallerPolygon(NewPolygon, BiggerPolygon, Design.MaximumClearance, 0);
			DrawFilledPolygon(BiggerPolygon, 0);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPolygonDirect(PolygonRecord * ObjectPolygon, int32 mode)
{
	int32 count;

	if ((ObjectPolygon->maxx >= ViewMinX) && (ObjectPolygon->minx <= ViewMaxX) && (ObjectPolygon->maxy >= ViewMinY)
	        && (ObjectPolygon->miny <= ViewMaxY))
	{

		count = ObjectPolygon->NrVertices;
		DrawOwnPolygon((double *) &((*ObjectPolygon).Points), count, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetLayerColors()
{
	int32 cnt, cnt2, ok;

	DrawLayerCode[0] &= ~0x0f;

	if (Design.NrBoardLayers > 1)
	{
		DrawLayerCode[Design.NrBoardLayers - 1] &= ~0x0f;
		DrawLayerCode[Design.NrBoardLayers - 1] |= 1;
	}

	if (Design.NrBoardLayers <= MAX_ACTIVE_DRAWING_LAYERS)
	{
		for (cnt = 1; cnt < Design.NrBoardLayers - 1; cnt++)
		{
			DrawLayerCode[cnt] &= ~0x0f;
			DrawLayerCode[cnt] |= cnt + 1;
		}
	}
	else
	{
		cnt2 = 0;

		for (cnt = 1; cnt < Design.NrBoardLayers - 1; cnt++)
		{
			DrawLayerCode[cnt] &= ~0x0f;
			DrawLayerCode[cnt] |= cnt2 + 2;
			cnt2++;

			if (cnt2 == 4)
				cnt2 = 0;
		}
	}

	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawStrWithRotation(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror, char *str)
{
	int32 cnt, cnt2, cnt4, NrPolyLines, lengte, count, oldx, oldy, hulp, code, res, xx1, yy1, xx2, yy2, xx3, yy3, xx4,
	      yy4, hx, hy;
	double x1d, y1d, NewLine, x1, y1, tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4, incX, incY;
	POINT LinePoints[20];
	WCHAR StrW16[512], *str2;
#ifdef _DEBUG
	int32 ok;

	if (InRange(Rotation, 135.0))
		ok = 1;

#endif

	hulp = Mult(Size);

	if ((hulp < 8) && (ReverseY))
	{
		if ((InRange2(Rotation, 0.0)) || (InRange2(Rotation, 90.0)) || (InRange2(Rotation, 180.0))
		        || (InRange2(Rotation, 270.0)))
		{
			xx1 = Mult((TextMaxX + TextMinX) * 0.5 - Xoffset) + DrawWindowMinX;

			if (ReverseY)
				yy1 = DrawWindowMaxY - Mult((TextMaxY + TextMinY) * 0.5 - Yoffset) - 1;
			else
				yy1 = Mult(TextMinY - Yoffset);

			xx2 = Mult(TextMaxX - TextMinX) - 1;
			yy2 = Mult(TextMaxY - TextMinY) - 1;
			rect3(xx1, yy1, xx2, yy2);

			if ((yy2 <= 2) || (xx2 <= 2))
				rect3(xx1, yy1, xx2 - 2, yy2 - 2);
		}
		else
		{
			GetMinMaxText2(x, y, Size, 0, 0.0, Alignment, Mirror, str);
			tx1 = TextMinX;
			ty1 = TextMinY;
			tx2 = TextMaxX;
			ty2 = TextMinY;
			tx3 = TextMaxX;
			ty3 = TextMaxY;
			tx4 = TextMinX;
			ty4 = TextMaxY;

			if (Mirror == 1)
				Rotation = -Rotation;

			RotatePointFromOtherPoint2(&tx1, &ty1, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx2, &ty2, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx3, &ty3, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx4, &ty4, x, y, Rotation);
			xx1 = MultX(tx1);

			if (ReverseY)
				yy1 = MultY(ty1);
			else
				yy1 = Mult(ty1 - Yoffset);

			xx2 = MultX(tx2);

			if (ReverseY)
				yy2 = MultY(ty2);
			else
				yy2 = Mult(ty2 - Yoffset);

			xx3 = MultX(tx3);

			if (ReverseY)
				yy3 = MultY(ty3);
			else
				yy3 = Mult(ty3 - Yoffset);

			xx4 = MultX(tx4);

			if (ReverseY)
				yy4 = MultY(ty4);
			else
				yy4 = Mult(ty4 - Yoffset);

			LinePoints[0].x = (int32) xx1;
			LinePoints[0].y = (int32) yy1;
			LinePoints[1].x = (int32) xx2;
			LinePoints[1].y = (int32) yy2;
			LinePoints[2].x = (int32) xx3;
			LinePoints[2].y = (int32) yy3;
			LinePoints[3].x = (int32) xx4;
			LinePoints[3].y = (int32) yy4;
			Polygon(OutputDisplay, LinePoints, 4);
		}

		return;
	}

	/*
	  LineSegments=TextStringToLineSegments(x,y,Size,Rotation,0,Mirror,str,(double *)&LineBuf);
	  SegmentCount=0;
	  for (cnt2=0;cnt2<LineSegments;cnt2++) {
	    xx1=MultX(LineBuf[SegmentCount]);
	    SegmentCount++;
	    yy1=MultY(LineBuf[SegmentCount]);
	    SegmentCount++;
	    xx2=MultX(LineBuf[SegmentCount]);
	    SegmentCount++;
	    yy2=MultY(LineBuf[SegmentCount]);
	    SegmentCount++;
	    MoveToEx(OutputDisplay,xx1,yy1,NULL);
	    LineTo(OutputDisplay,xx2,yy2);
	    SetPixel(OutputDisplay,xx2,yy2,LineColor);
	  }


	*/
	lengte = MultiByteToWideChar(CP_UTF8, 0, str, strlen(str), StrW16, 511);

	if (lengte == 0)
		return;

	StrW16[lengte] = 0;
	Size *= DefFontSize;
	incX = 0.9 * Size;
	incY = 0.0;
	RotatePoint2(&incX, &incY, Rotation);
	lengte = (int32) wcslen(StrW16);
	str2 = StrW16;

	for (cnt4 = 0; cnt4 < lengte; cnt4++)
	{
		code = (*str2);
//    code='+';
		oldx = -100000000;
		oldy = -100000000;
		count = 0;

//    code=127;
		if (code != 32)
		{
			if ((code < 32) || (code > 126))
				code = 127;

			code -= 33;
			NrPolyLines = (*Chars)[code].NrPolyLines;
			cnt2 = 0;

			for (cnt = 0; cnt < NrPolyLines; cnt++)
			{
				count = 0;

				do
				{
					x1d = ((*Chars)[code].Line[cnt2 + 1]);
					y1d = ((*Chars)[code].Line[cnt2 + 2]);
					y1d -= 0.4;
					x1 = x1d * Size;
					y1 = y1d * Size;
					RotatePoint2(&x1, &y1, Rotation);

					if (Mirror == 1)
						x1 = -x1;

//            RotateFlipPoint(&x1,&y1,0.0,0.0,Rotation);
					hx = (Mult(x1 + x - Xoffset) + DrawWindowMinX);

					if (ReverseY)
						hy = (DrawWindowMaxY - Mult(y1 + y - Yoffset) - 1);
					else
						hy = (Mult(y1 + y - Yoffset));

					LinePoints[count].x = (int) hx;
					LinePoints[count].y = (int) hy;
					count++;
					cnt2 += 3;
					NewLine = ((*Chars)[code].Line[cnt2]);
				}
				while (InRange2(NewLine, 0.0));

				res = Polyline(OutputDisplay, (POINT *) & LinePoints, (int) count);
				SetPixel(OutputDisplay, (int) LinePoints[count - 1].x, (int) LinePoints[count - 1].y, LineColor);
			}
		}

		if (Mirror == 0)
		{
			x += incX;
			y += incY;
		}
		else
		{
			x -= incX;
			y += incY;
		}

		str2++;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawStrWithRotation2(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror, WCHAR * str)
{
	int32 cnt, cnt2, cnt4, NrPolyLines, lengte, count, oldx, oldy, hulp, code, res, xx1, yy1, xx2, yy2, xx3, yy3, xx4,
	      yy4, hx, hy;
	double x1d, y1d, NewLine, x1, y1, tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4, incX, incY;
	POINT LinePoints[20];
	WCHAR *str2;
#ifdef _DEBUG
	int32 ok;

	if (InRange(Rotation, 135.0))
		ok = 1;

#endif

	hulp = Mult(Size);

	if ((hulp < 8) && (ReverseY))
	{
		if ((InRange2(Rotation, 0.0)) || (InRange2(Rotation, 90.0)) || (InRange2(Rotation, 180.0))
		        || (InRange2(Rotation, 270.0)))
		{
			xx1 = Mult((TextMaxX + TextMinX) * 0.5 - Xoffset) + DrawWindowMinX;

			if (ReverseY)
				yy1 = DrawWindowMaxY - Mult((TextMaxY + TextMinY) * 0.5 - Yoffset) - 1;
			else
				yy1 = Mult(TextMinY - Yoffset);

			xx2 = Mult(TextMaxX - TextMinX) - 1;
			yy2 = Mult(TextMaxY - TextMinY) - 1;
			rect3(xx1, yy1, xx2, yy2);

			if ((yy2 <= 2) || (xx2 <= 2))
				rect3(xx1, yy1, xx2 - 2, yy2 - 2);
		}
		else
		{
			GetMinMaxText2b(x, y, Size, 0, 0.0, Alignment, Mirror, str);
			tx1 = TextMinX;
			ty1 = TextMinY;
			tx2 = TextMaxX;
			ty2 = TextMinY;
			tx3 = TextMaxX;
			ty3 = TextMaxY;
			tx4 = TextMinX;
			ty4 = TextMaxY;

			if (Mirror == 1)
				Rotation = -Rotation;

			RotatePointFromOtherPoint2(&tx1, &ty1, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx2, &ty2, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx3, &ty3, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx4, &ty4, x, y, Rotation);
			xx1 = MultX(tx1);

			if (ReverseY)
				yy1 = MultY(ty1);
			else
				yy1 = Mult(ty1 - Yoffset);

			xx2 = MultX(tx2);

			if (ReverseY)
				yy2 = MultY(ty2);
			else
				yy2 = Mult(ty2 - Yoffset);

			xx3 = MultX(tx3);

			if (ReverseY)
				yy3 = MultY(ty3);
			else
				yy3 = Mult(ty3 - Yoffset);

			xx4 = MultX(tx4);

			if (ReverseY)
				yy4 = MultY(ty4);
			else
				yy4 = Mult(ty4 - Yoffset);

			LinePoints[0].x = (int32) xx1;
			LinePoints[0].y = (int32) yy1;
			LinePoints[1].x = (int32) xx2;
			LinePoints[1].y = (int32) yy2;
			LinePoints[2].x = (int32) xx3;
			LinePoints[2].y = (int32) yy3;
			LinePoints[3].x = (int32) xx4;
			LinePoints[3].y = (int32) yy4;
			Polygon(OutputDisplay, LinePoints, 4);
		}

		return;
	}

	/*
	  LineSegments=TextStringToLineSegments(x,y,Size,Rotation,0,Mirror,str,(double *)&LineBuf);
	  SegmentCount=0;
	  for (cnt2=0;cnt2<LineSegments;cnt2++) {
	    xx1=MultX(LineBuf[SegmentCount]);
	    SegmentCount++;
	    yy1=MultY(LineBuf[SegmentCount]);
	    SegmentCount++;
	    xx2=MultX(LineBuf[SegmentCount]);
	    SegmentCount++;
	    yy2=MultY(LineBuf[SegmentCount]);
	    SegmentCount++;
	    MoveToEx(OutputDisplay,xx1,yy1,NULL);
	    LineTo(OutputDisplay,xx2,yy2);
	    SetPixel(OutputDisplay,xx2,yy2,LineColor);
	  }


	*/

	Size *= DefFontSize;
	incX = 0.9 * Size;
	incY = 0.0;
	RotatePoint2(&incX, &incY, Rotation);
	str2 = str;
	lengte = (int32) wcslen(str);

	for (cnt4 = 0; cnt4 < lengte; cnt4++)
	{
		code = (*str2);
//    code='+';
		oldx = -100000000;
		oldy = -100000000;
		count = 0;

//    code=127;
		if (code != 32)
		{
			if ((code < 32) || (code > 126))
				code = 127;

			code -= 33;
			NrPolyLines = (*Chars)[code].NrPolyLines;
			cnt2 = 0;

			for (cnt = 0; cnt < NrPolyLines; cnt++)
			{
				count = 0;

				do
				{
					x1d = ((*Chars)[code].Line[cnt2 + 1]);
					y1d = ((*Chars)[code].Line[cnt2 + 2]);
					y1d -= 0.4;
					x1 = x1d * Size;
					y1 = y1d * Size;
					RotatePoint2(&x1, &y1, Rotation);

					if (Mirror == 1)
						x1 = -x1;

//            RotateFlipPoint(&x1,&y1,0.0,0.0,Rotation);
					hx = (Mult(x1 + x - Xoffset) + DrawWindowMinX);

					if (ReverseY)
						hy = (DrawWindowMaxY - Mult(y1 + y - Yoffset) - 1);
					else
						hy = (Mult(y1 + y - Yoffset));

					LinePoints[count].x = (int) hx;
					LinePoints[count].y = (int) hy;
					count++;
					cnt2 += 3;
					NewLine = ((*Chars)[code].Line[cnt2]);
				}
				while (InRange2(NewLine, 0.0));

				res = Polyline(OutputDisplay, (POINT *) & LinePoints, (int) count);
				SetPixel(OutputDisplay, (int) LinePoints[count - 1].x, (int) LinePoints[count - 1].y, LineColor);
			}
		}

		if (Mirror == 0)
		{
			x += incX;
			y += incY;
		}
		else
		{
			x -= incX;
			y += incY;
		}

		str2++;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTrueTypeStrWithRotation(double x, double y, double Size, int32 FontNr, double Rotation, int32 Alignment,
                                 int32 Mirror, WCHAR * str, int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt4, lengte, count, oldx, oldy, hulp, code, DrawChar, xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4;
	double tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4, incX, incY;
	POINT LinePoints[20];
	char *FontStr;
	uint8 *PolygonPos;
	PolygonRecord *DrawPolygon;
	AreaFillRecord *AreaFill;
	WCHAR *str2;
#ifdef _DEBUG
	int32 ok;

	if (InRange(Rotation, 135.0))
		ok = 1;

#endif

	if ((FontNr == 0) || (FontNr > 16))
		return;

	FontStr = Design.UsedFontStr[FontNr - 1];

	if (FontStr[0] == 0)
		return;

	hulp = Mult(Size);

	if ((hulp < 8) && (ReverseY))
	{
		GetMinMaxText2b(x, y, Size, FontNr, 0.0, Alignment, Mirror, str);
		tx1 = TextMinX;
		ty1 = TextMinY;
		tx2 = TextMaxX;
		ty2 = TextMinY;
		tx3 = TextMaxX;
		ty3 = TextMaxY;
		tx4 = TextMinX;
		ty4 = TextMaxY;

		if (Mirror == 1)
			Rotation = -Rotation;

		RotatePointFromOtherPoint2(&tx1, &ty1, x, y, Rotation);
		RotatePointFromOtherPoint2(&tx2, &ty2, x, y, Rotation);
		RotatePointFromOtherPoint2(&tx3, &ty3, x, y, Rotation);
		RotatePointFromOtherPoint2(&tx4, &ty4, x, y, Rotation);
		xx1 = MultX(tx1);

		if (ReverseY)
			yy1 = MultY(ty1);
		else
			yy1 = Mult(ty1 - Yoffset);

		xx2 = MultX(tx2);

		if (ReverseY)
			yy2 = MultY(ty2);
		else
			yy2 = Mult(ty2 - Yoffset);

		xx3 = MultX(tx3);

		if (ReverseY)
			yy3 = MultY(ty3);
		else
			yy3 = Mult(ty3 - Yoffset);

		xx4 = MultX(tx4);

		if (ReverseY)
			yy4 = MultY(ty4);
		else
			yy4 = Mult(ty4 - Yoffset);

		LinePoints[0].x = (int32) xx1;
		LinePoints[0].y = (int32) yy1;
		LinePoints[1].x = (int32) xx2;
		LinePoints[1].y = (int32) yy2;
		LinePoints[2].x = (int32) xx3;
		LinePoints[2].y = (int32) yy3;
		LinePoints[3].x = (int32) xx4;
		LinePoints[3].y = (int32) yy4;
		Polygon(OutputDisplay, LinePoints, 4);
		return;
	}

	incX = 0.0;
	incY = 0.0;
	str2 = str;
	lengte = (int32) wcslen(str);
	AllocateSpecialMem(MEM_TRUETYPE_AREAFILL, 128 * 1024, (void **) &AreaFill);

	for (cnt4 = 0; cnt4 < lengte; cnt4++)
	{
		code = (*str2);
//    code='+';
		oldx = -100000000;
		oldy = -100000000;
		count = 0;
		DrawChar = 0;

//    code=127;
		if ((code != ' ') && (code != '\t'))
		{
			if (GetAreaFillFontChar((int32) code, FontNr, AreaFill) == 0)
				break;

			DrawChar = 1;
		}
		else
		{
			incX = TRUETYPE_FONT_SPACE_EXTRA_X * Size;
			incY = 0.0;
			RotatePoint2(&incX, &incY, Rotation);
		}

		if (DrawChar)
		{
			incX = (AreaFill->maxx - AreaFill->minx + TRUETYPE_FONT_ADD_EXTRA_X) * Size;
			incY = 0.0;
			RotatePoint2(&incX, &incY, Rotation);
			DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
			PolygonPos = (uint8 *) DrawPolygon;

			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				count = DrawPolygon->NrVertices;
#ifdef _DEBUG

				if ((DrawPolygon->PolygonType & 8) == 8)
					ok = 1;

				if (cnt2 == 6)
					ok = 1;

#endif

				for (cnt = 0; cnt < count; cnt++)
				{
					DrawPolygon->Points[cnt].x *= Size;
					DrawPolygon->Points[cnt].y *= Size;
					RotatePoint2(&DrawPolygon->Points[cnt].x, &DrawPolygon->Points[cnt].y, Rotation);

					if (Mirror == 1)
						DrawPolygon->Points[cnt].x = -DrawPolygon->Points[cnt].x;

					DrawPolygon->Points[cnt].x += x;
					DrawPolygon->Points[cnt].y += y;
				}

				if ((DrawPolygon->PolygonType & 8) == 8)
					SetBackGroundActive(1);

				if ((mode & 2) == 0)
				{
					if ((mode & 1) == 1)
						InitDrawingObject(TRACE_HOR, Layer, 0, NORMAL_FILLED_AND_NO_PEN | USE_LAYER_DRAW_CODE);
					else
						InitDrawingObject(TRACE_HOR, Layer, 0, NORMAL_FILLED_AND_PEN1 | USE_LAYER_DRAW_CODE);
				}

				DrawOwnPolygon((double *) &((*DrawPolygon).Points), count, 0);
				PolygonPos += MemSizePolygon(DrawPolygon);
				DrawPolygon = (PolygonRecord *) PolygonPos;
			}
		}

		if (Mirror == 0)
		{
			x += incX;
			y += incY;
		}
		else
		{
			x -= incX;
			y += incY;
		}

		str2++;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawAreaFills(int32 Mode)
{
	int32 cnt;
	AreaFillRecord *AreaFill;

	if (CurrentDrawingLayer == -1)
		return;

	DrawCode = DrawLayerCode[CurrentDrawingLayer];

	if ((DrawCode < 0) || (DrawCode >= MAX_ACTIVE_DRAWING_LAYERS))
		return;

	if (Mode & 8)
	{
		if ((Mode & 16) == 16)
		{
			for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

				if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer != CurrentDrawingLayer))
					DrawAreaFillClearance(AreaFill, 0);
			}
		}
		else
		{
			for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

				if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == CurrentDrawingLayer))
					DrawAreaFillClearance(AreaFill, 0);
			}
		}

		return;
	}

	DrawCode = DrawLayerCode[CurrentDrawingLayer];

	if ((Mode & 16) == 16)
	{
		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer != CurrentDrawingLayer))
				DrawAreaFill(AreaFill, Mode);
		}

		return;
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == CurrentDrawingLayer))
			DrawAreaFill(AreaFill, Mode);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawAreaFill(AreaFillRecord * AreaFill, int32 Mode)
{
	int32 cnt2, count, ok, MaxRoundings, Layer, VerticeCount;
	double ThickNess;
	PolygonRecord *DrawPolygon, *BiggerPolygon, *FirstPolygon;
	uint8 *AreaPos, *PolygonPos;

// Mode
//
// 0  = Normal
// 2  = Draw areafill contours only

// DrawPolygon->PolygonType  =  1   ->  polygon selected
// DrawPolygon->PolygonType  =  2   ->  polygon is thermal relief
// DrawPolygon->PolygonType  =  4   ->  polygon is cut out

	ThickNess = 5080;
	MaxRoundings = 6;
	AllocateSpecialMem(MEM_POLYGON_BIGGER, 128 * 1024, (void **) &BiggerPolygon);

	if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) != 0)
		return;

	if (!OkToDrawAreaFills)
		return;

	if (((Mode & 1) == 1) && (AreaFill->NetNr == -1))
		return;

	Layer = AreaFill->Layer;
	DrawCode = DrawLayerCode[Layer];

	if ((DrawCode < 0) || (DrawCode >= MAX_ACTIVE_DRAWING_LAYERS))
		return;

	ok = 1;

	if (!IsAreaFillVisible(AreaFill, 0))
		return;

	// *******************************************************************************************************

	if (((AreaFill->Info & (OBJECT_SELECTED | POWERPLANE)) == 0) && ((Mode & 2) == 0) && (AreafillDrawMode == 0))
	{

		if ((AreaFill->Info & OBJECT_HIGHLITED) != 0)
			InitDrawingObject(TRACE_HOR, DrawCode, 0, HILITED_NORMAL_FILLED_AND_PEN1);
		else
			InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

		AreaPos = (uint8 *) AreaFill;
		count = sizeof(AreaFillRecord);
		DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) DrawPolygon;

		if (IsPolygonVisible(DrawPolygon, 0))
		{
#ifdef _DEBUG

			if (DrawPolygon->NrVertices > 24576)
				ok = 1;

#endif
			DrawOwnPolygon((double *) &((*DrawPolygon).Points), DrawPolygon->NrVertices, 0);
		}

		// *******************************************************************************************************

		SetBackGroundActive(0);
		//  InitDrawingObject(TRACE_HOR,DrawCode,0,NORMAL_FILLED_AND_NO_PEN);
		InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;

		for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
		{
			count = DrawPolygon->NrVertices;
#ifdef _DEBUG

			if ((DrawPolygon->PolygonType & 8) == 8)
				ok = 1;

			if (cnt2 == 6)
				ok = 1;

#endif

			if (((DrawPolygon->PolygonType & 8) == 0) && (IsPolygonVisible(DrawPolygon, 0)))
				DrawOwnPolygon((double *) &((*DrawPolygon).Points), count, 0);

			PolygonPos += MemSizePolygon(DrawPolygon);
			DrawPolygon = (PolygonRecord *) PolygonPos;
		}
	}

// **********************************************************************************
#ifdef _DEBUG

	if ((Mode & 2) == 2)
		ok = 1;

#endif
	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;
	FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	InitDrawingObject(0, DrawCode, 1, DRAW_WITH_PEN_AND_NOT_FILLED);

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		count = DrawPolygon->NrVertices;
		VerticeCount = count;
#ifdef _DEBUG

		if (count > 100000)
			ok = 1;

#endif
		InitDrawingObject(0, DrawCode, 1, DRAW_WITH_PEN_AND_NOT_FILLED);

		if ((!Printing) && ((Mode & 1) == 0))
		{
			if (((FirstPolygon->PolygonType & 2) == 2) || ((DrawPolygon->PolygonType & 2) == 2))
				SetROP2(OutputDisplay, SelectColorMode);
		}

		DrawOwnPolygon((double *) &((*DrawPolygon).Points), VerticeCount, 0);

		if (((FirstPolygon->PolygonType & 2) == 0) && ((DrawPolygon->PolygonType & 2) == 0)
		        && ((DrawPolygon->PolygonType & (8 + 2)) == 8))
		{
			InitDrawingObject(0, AREAFILL_DASH_PEN_LAYER, 0, 0);
			DrawOwnPolygon((double *) &((*DrawPolygon).Points), VerticeCount, 16);
		}

		if ((!Printing) && ((Mode & 1) == 0))
		{
			if (((FirstPolygon->PolygonType & 2) == 2) || ((DrawPolygon->PolygonType & 2) == 2))
				SetROP2(OutputDisplay, R2_COPYPEN);
		}

// ***************************************************************************************
// ***************************************************************************************
		if ((AreaFill->Info & (POWERPLANE)) == 0)
			ok = 1;

// ***************************************************************************************
// ***************************************************************************************
		PolygonPos += sizeof(PolygonInitRecord) + count * sizeof(PointRecord);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	if ((!Printing) && ((Mode & 1) == 0) && (FirstPolygon->PolygonType & 2))
	{
//    DrawOwnPolygon((double *)&((*FirstPolygon).Points),VerticeCount,0);
		InitDrawingObject(0, DrawCode, 1, DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawOwnPolygonFloat((float *) &AreaFill->StartPolygon, AreaFill->NrVerticesStartPolygon, 0);
		InitDrawingObject(0, AREAFILL_DASH_PEN_LAYER, 0, 0);
		SetROP2(OutputDisplay, SelectColorMode);
		DrawOwnPolygonFloat((float *) &AreaFill->StartPolygon, AreaFill->NrVerticesStartPolygon, 16);
		SetROP2(OutputDisplay, R2_COPYPEN);
	}

	ok = 1;
// **********************************************************************************
	/*
	  InitDrawingColorYellow();
	  SetROP2(OutputDisplay,R2_COPYPEN);
	  count=AreaFill->NrVerticesStartPolygon;
	  for (cnt3=0;cnt3<count;cnt3++) {
	    x1=AreaFill->StartPolygon[cnt3].x;
	    y1=AreaFill->StartPolygon[cnt3].y;
	    if (cnt3<count-1) {
	      x2=AreaFill->StartPolygon[cnt3+1].x;
	      y2=AreaFill->StartPolygon[cnt3+1].y;
	    } else {
	      x2=AreaFill->StartPolygon[0].x;
	      y2=AreaFill->StartPolygon[0].y;
	    }
	    if ((max(x1,x2)>=ViewMinX)
	       &&
	       (min(x1,x2)<=ViewMaxX)
	       &&
	       (max(y1,y2)>=ViewMinY)
	       &&
	       (min(y1,y2)<=ViewMaxY)) {
	      DrawLine(MultX(x1),MultY(y1),MultX(x2),MultY(y2));
	    }
	  }
	*/
// **********************************************************************************
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawAreaFillClearance(AreaFillRecord * AreaFill, int32 Mode)
{
	int32 cnt2, count, ok, Layer, VerticeCount;
	double *PolygonPoints, Clearance;
	PolygonRecord *DrawPolygon, *BiggerPolygon, *FirstPolygon;
	uint8 *AreaPos, *PolygonPos;

// DrawPolygon->PolygonType  =  1   ->  polygon selected
// DrawPolygon->PolygonType  =  2   ->  polygon is thermal relief
// DrawPolygon->PolygonType  =  4   ->  polygon is cut out

	if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) != 0)
		return;

	if (!OkToDrawAreaFills)
		return;

	/*
	  if (AreaFill->NetNr==-1) {
	    return;
	  }
	*/
	Clearance = max(AreaFill->Clearance, Design.MaximumClearance);
	Layer = AreaFill->Layer;
	DrawCode = DrawLayerCode[Layer];

	if ((DrawCode < 0) || (DrawCode >= MAX_ACTIVE_DRAWING_LAYERS))
		return;

	ok = 1;

	if (!IsAreaFillVisible(AreaFill, 0))
		return;

// **********************************************************************************
	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);

	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;
	FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		count = DrawPolygon->NrVertices;
		VerticeCount = count;
		PolygonPoints = (double *) &((*DrawPolygon).Points);

		if (IsPolygonVisible(DrawPolygon, 0))
		{
			AllocateSpecialMem(MEM_POLYGON_BIGGER, count * sizeof(PointRecord) + 16384, (void **) &BiggerPolygon);
			BiggerPolygon->NrVertices = 0;

			if (cnt2 == 0)
			{
				MakeBiggerSmallerPolygon(DrawPolygon, BiggerPolygon, Clearance * 2.0, 0);
				SetMinMaxPolygon(BiggerPolygon, 0);
				VerticeCount = BiggerPolygon->NrVertices;
			}
			else
			{
				if (((DrawPolygon->PolygonType & 5) == 0) || ((DrawPolygon->PolygonType & 5) == 5))
				{
					MakeBiggerSmallerPolygon(DrawPolygon, BiggerPolygon, Clearance * 2.0, 1);
					SetMinMaxPolygon(BiggerPolygon, 0);
					VerticeCount = BiggerPolygon->NrVertices;
				}
			}

			if ((BiggerPolygon->NrVertices > 0) && (IsPolygonVisible(BiggerPolygon, 0)))
				DrawOwnPolygon((double *) &((*BiggerPolygon).Points), VerticeCount, 0);
		}

		PolygonPos += sizeof(PolygonInitRecord) + count * sizeof(PointRecord);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPinAreaFill(AreaFillRecord * AreaFill, int32 Mode)
{
	int32 cnt2, count;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

// Mode
//
// 0  = Normal
// 1  = Draw areafill with clearances
// 2  = Draw areafill contours only

	if (!IsAreaFillVisible(AreaFill, 0))
		return;

	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	if (IsPolygonVisible(DrawPolygon, 0))
		DrawOwnPolygon((double *) &((*DrawPolygon).Points), DrawPolygon->NrVertices, 0);

	SetBackGroundActive(0);
//  InitDrawingObject(TRACE_HOR,DrawCode,0,NORMAL_FILLED_AND_NO_PEN);
	InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);
	PolygonPos += MemSizePolygon(DrawPolygon);
	DrawPolygon = (PolygonRecord *) PolygonPos;

	for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		count = DrawPolygon->NrVertices;

		if (IsPolygonVisible(DrawPolygon, 0))
			DrawOwnPolygon((double *) &((*DrawPolygon).Points), count, 0);

		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPinAreaFillClearance(AreaFillRecord * AreaFill, int32 Mode)
{
	int32 cnt2, count, ok;
	PolygonRecord *DrawPolygon, *BiggerPolygon;
	uint8 *AreaPos, *PolygonPos;

	InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		count = DrawPolygon->NrVertices;
		AllocateSpecialMem(MEM_POLYGON_BIGGER, count * sizeof(PointRecord) + 16384, (void **) &BiggerPolygon);
		BiggerPolygon->NrVertices = 0;

		if (cnt2 == 0)
			MakeBiggerSmallerPolygon(DrawPolygon, BiggerPolygon, AreaFill->Clearance * 2.0, 0);
		else
			MakeBiggerSmallerPolygon(DrawPolygon, BiggerPolygon, AreaFill->Clearance * 2.0, 1);

		SetMinMaxPolygon(BiggerPolygon, 0);
		count = BiggerPolygon->NrVertices;
		DrawOwnPolygon((double *) &((*BiggerPolygon).Points), count, 0);
		PolygonPos += sizeof(PolygonInitRecord) + count * sizeof(PointRecord);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
