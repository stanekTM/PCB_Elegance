/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: polygon.c
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <windows.h>
#include <commdlg.h>

#include "types.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "calcdef.h"
#include "dialogs.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "files2.h"
#include "graphics.h"
#include "help.h"
#include "insdel.h"
#include "keyswin.h"
#include "line2.h"
#include "mainloop.h"
#include "memory.h"
#include "menus.h"
#include "nets.h"
#include "nets2.h"
#include "pcb.h"
#include "polygon.h"
#include "rect.h"
#include "resource.h"
#include "select.h"
#include "select3.h"
#include "toets.h"
#include "trace2.h"
#include "trace3.h"
#include "move3.h"
#include "owntime.h"


#define MAX_NR_CROSSES           MAX_WORK_POLGONS*2
//#define MAX_NR_CROSSES           8

typedef struct
{
	int32 LineNrPolygonA, LineNrPolygonB, Test;
	double CrossX, CrossY;
} CrossPointRecord;

typedef struct
{
	PointRecord CrossPoints[MAX_WORK_POLGONS];
	int32 LineNrPolygonB1, LineNrPolygonA2;
	int32 LineNrPolygonB3, LineNrPolygonA4;
	int32 Count1, Count2, Count3, Count4, Mode;
} VerticeCopyRecord;

typedef struct
{
	int32 LineNrPolygonA, LineNrPolygonB, Test;
	double CrossX, CrossY;
} CrossPoint2Record;

typedef struct
{
	PointRecord CrossPoints[MAX_NR_CROSSES];
	int32 LineNrPolygon[MAX_NR_CROSSES], Count[MAX_NR_CROSSES], Mode[MAX_NR_CROSSES];
} VerticeCopy2Record;

typedef struct
{
	PointRecord CrossPoint;
	double Distance;
	int32 Pos, Test;
} DistanceOnSameLineRecord;

CrossPointRecord CrossPoints[MAX_NR_CROSSES];
int32 PolygonObjectType, ObjectNr, NrCrossPoints, PowerPlaneNetNr, StartPolygon = 0, ok;

PolygonRecord *CurrentPolygon, *ProblemPolygon1, *ProblemPolygon2;
uint8 PolygonBuf[10240];

ObjectLineRecord AreaFillObjectHorLine = { 0, 0, 0, 0, 0, 0.0, 0.0, (80 * 2540.0), (40 * 2540.0),
                                           (40 * 2540.0)
                                         };
ObjectLineRecord AreaFillObjectVerLine = { 0, 0, 0, 0, 0, 0.0, 0.0, (40 * 2540.0), (80 * 2540.0),
                                           (40 * 2540.0)
                                         };
ObjectRectRecord AreaFillObjectRect = { 0, 0, 0, 0, 0, 0.0, 0.0,
                                        (40 * 2540.0), (40 * 2540.0), 0.0
                                      };
ObjectArcRecord AreaFillObjectArc = { 0, 0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0,
                                      0.0, 0.0,
                                      (40 * 2540.0), (40 * 2540.0), 0.0
                                    };

VerticeCopyRecord VerticeCopy;
VerticeCopy2Record VerticeCopy2;
// Realloc GlobalAlloc


double LineCrossX, LineCrossY, DrawPolygonLineThickNess, TempAreaFillClearance, ShiftX, ShiftY, OldShiftX, OldShiftY,
       OldX, OldY, CurrentX, CurrentY, NewAreaFillClearance;
static double LineX1, LineY1, LineX2, LineY2, FirstX, FirstY;
//static double  CurrentX2,CurrentY2;
extern HDC OutputDisplay;
extern int32 AreafillDrawMode, SelectColorMode;
extern int32 TraceBackWardsKeyPressed, CrossHairMode;

extern int32 Printing, ReverseY, CrossHairType, SpecialDebugFile;
extern HBITMAP BitMapCross1, BitMapCross2;

//uint8   PolygonBuf2[10000];
char Pstr[8][100];
DistanceOnSameLineRecord DistanceOnSameLine[MAX_NR_CROSSES];

double OldValue = 10000;
double OldValue2 = 0.1;

int32 FinishPolygon, TestDraw, PolygonDrawObjectType;

#ifdef _DEBUG
int32 DrawTryingPolylineCount, MouseChangedCount;
#endif

AreaFillRecord *DefaultAreaFill;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygonInsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon);

int32 GetNewWorkPolygon(PolygonRecord * NewPolygonObject, int32 NrPolygons, int32 mode);

int32 FindSelectedAreaFill(int32 mode);

int32 DeleteFromAreaFill(int32 AreaFillNr, int32 mode);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double GetNewRandomValue(int32 mode)
{
	double RandValue = 0.05;
	int32 NrTries = 10000;

	while ((NrTries)
	        && (((RandValue = (((double) rand() / RAND_MAX))) < 0.1) || (RandValue - OldValue2 < 0.0005)
	            || (RandValue - OldValue2 > 0.0015)))
		NrTries--;

	OldValue2 = RandValue;
	return RandValue;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
/*
void DrawAreaFillStartPolygon(AreaFillRecord * AreaFill, int32 mode)
{
	int32 count, cnt;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;

	count = AreaFill->NrVerticesStartPolygon;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = AreaFill->StartPolygon[cnt].x;
		y1 = AreaFill->StartPolygon[cnt].y;

		if (cnt < count - 1)
		{
			x2 = AreaFill->StartPolygon[cnt + 1].x;
			y2 = AreaFill->StartPolygon[cnt + 1].y;
		}
		else
		{
			x2 = AreaFill->StartPolygon[0].x;
			y2 = AreaFill->StartPolygon[0].y;
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

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			switch (mode)
			{
			case 0:
				DrawLineRed(x1, y1, x2, y2);
				break;

			case 1:
				DrawLineGreen(x1, y1, x2, y2);
				break;

			case 2:
				DrawLineYellow(x1, y1, x2, y2);
				break;

			case 3:
				InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_RED + DRAW_WITH_PEN_AND_NOT_FILLED);
				DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
				         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
				break;

			case 4:
				DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
				         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
				break;

			case 5:
				// OutputDisplay
				InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);
				SetROP2(OutputDisplay, R2_COPYPEN);
				DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
				         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
				break;

			}
		}
	}
}
*/
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTestPolygon(PolygonRecord * Polygon, int32 mode)
{
	int32 count, cnt;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;

	count = Polygon->NrVertices;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*Polygon).Points[cnt].x;
		y1 = (*Polygon).Points[cnt].y;

		if (cnt < count - 1)
		{
			x2 = (*Polygon).Points[cnt + 1].x;
			y2 = (*Polygon).Points[cnt + 1].y;
		}
		else
		{
			x2 = (*Polygon).Points[0].x;
			y2 = (*Polygon).Points[0].y;
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

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			switch (mode)
			{
			case 0:
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
				break;

			case 1:
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
				break;

			case 2:
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
				break;

			case 3:
				InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_RED + DRAW_WITH_PEN_AND_NOT_FILLED);
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
				break;

			case 4:
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
				break;

			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
/*
void DrawTestPolygon2(PolygonRecord * Polygon, int32 mode)
{
	int32 count, cnt;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;

	count = Polygon->NrVertices;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*Polygon).Points[cnt].x;
		y1 = (*Polygon).Points[cnt].y;

		if (cnt < count - 1)
		{
			x2 = (*Polygon).Points[cnt + 1].x;
			y2 = (*Polygon).Points[cnt + 1].y;
		}
		else
		{
			x2 = (*Polygon).Points[0].x;
			y2 = (*Polygon).Points[0].y;
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

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			StartDrawingEditingWindow(0);
			InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			SetROP2(OutputDisplay, SelectColorMode);
			ellips2(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1, 3, 3, 255);
			
			ExitDrawing();
			EndDrawingEditingWindow(0);

			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
					DrawLineYellow(x1, y1, x2, y2);
				else
					DrawLineGreen(x1, y1, x2, y2);
			}
			else
			{
				if ((mode & 1) == 0)
					DrawLineYellow(x1, y1, x2, y2);
				else
					DrawLineGreen(x1, y1, x2, y2);
			}
		}
	}
}
*/
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingPolygon(int32 dikte, int32 mode)
{

	PolygonRecord *DrawPolygon2;
	int32 cnt3, count;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;


	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
	count = DrawPolygon2->NrVertices;

	if (count == 0)
		return;

	StartDrawingEditingWindow(BM_DoubleBuffer);

	if (mode == 0)
		SetROP2(OutputDisplay, R2_XORPEN);
	else
		SetBackGroundActive(0);

//  if (mode==0) SetROP2(OutputDisplay,R2_COPYPEN);
//  dikte=Mult(NewObjectLine.LineThickNess);
	InitDrawingObject(0, POLYGON_DRAW_LAYER, dikte, DRAW_WITH_PEN_AND_NOT_FILLED);

//  InitDrawingAreaFills(0);
	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		x1 = (*DrawPolygon2).Points[cnt3].x;
		y1 = (*DrawPolygon2).Points[cnt3].y;
		x2 = (*DrawPolygon2).Points[cnt3 + 1].x;
		y2 = (*DrawPolygon2).Points[cnt3 + 1].y;

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
			DrawLine(Mult(x1 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y1 - Yoffset) - 1,
			         Mult(x2 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(y2 - Yoffset) - 1);
		}
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow(BM_DoubleBuffer);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTestPolygon3(PolygonRecord * Polygon, int32 mode)
{
	int32 count, cnt;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;
	char str[MAX_LENGTH_STRING];

	if ((mode & 4) == 0)
		StartDrawingEditingWindow(0);

	count = Polygon->NrVertices;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*Polygon).Points[cnt].x;
		y1 = (*Polygon).Points[cnt].y;

		if (cnt < count - 1)
		{
			x2 = (*Polygon).Points[cnt + 1].x;
			y2 = (*Polygon).Points[cnt + 1].y;
		}
		else
		{
			x2 = (*Polygon).Points[0].x;
			y2 = (*Polygon).Points[0].y;
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

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			SetROP2(OutputDisplay, SelectColorMode);
			ellips2(MultX(x1), MultY(y1), 5, 5, 255);
			sprintf(str, "%i", cnt);
			
			SetROP2(OutputDisplay, R2_COPYPEN);

			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
				else
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);
			}
			else
			{
				if ((mode & 1) == 0)
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
				else
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);
			}

			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
		}
	}

	if ((mode & 4) == 0)
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTestPolygon4(PolygonRecord * Polygon, double dikte, int32 mode)
{
	int32 count, cnt, thickness;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;
	char str[MAX_LENGTH_STRING];

	if ((mode & 4) == 0)
		StartDrawingEditingWindow(0);

	thickness = Mult(dikte);
	count = Polygon->NrVertices;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*Polygon).Points[cnt].x;
		y1 = (*Polygon).Points[cnt].y;

		if (cnt < count - 1)
		{
			x2 = (*Polygon).Points[cnt + 1].x;
			y2 = (*Polygon).Points[cnt + 1].y;
		}
		else
		{
			x2 = (*Polygon).Points[0].x;
			y2 = (*Polygon).Points[0].y;
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

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
			SetROP2(OutputDisplay, SelectColorMode);
			SetTextColor(OutputDisplay, RGB_White);
			ellips2(MultX(x1), MultY(y1), thickness, thickness, 255);
			sprintf(str, "%i", cnt);
			
			TextOutUTF8(OutputDisplay, MultX(x1) + 2, MultY(y1) + 7, str, strlen(str));
			SetROP2(OutputDisplay, R2_COPYPEN);

			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
				else
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);
			}
			else
			{
				if ((mode & 1) == 0)
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
				else
					InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);
			}

			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
		}
	}

	if ((mode & 4) == 0)
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawFilledPolygon(PolygonRecord * FilledPolygon, int32 mode)
{

	int32 cnt, x, y, count;
	PointsArray3 *Points;

	count = FilledPolygon->NrVertices;
	AllocateSpecialMem(MEM_POINTS, count * sizeof(POINT) + 16384, (void **) &Points);

	if ((mode & 8) == 8)
		StartDrawingEditingWindow(0);

	switch (mode & 7)
	{
	case 4:
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + NORMAL_FILLED_AND_PEN1);
		break;

	case 5:
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_RED + NORMAL_FILLED_AND_PEN1);
		break;

	case 6:
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_BLUE + NORMAL_FILLED_AND_PEN1);
		break;

	case 7:
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + NORMAL_FILLED_AND_PEN1);
		break;
	}

	for (cnt = 0; cnt < count; cnt++)
	{
		if ((mode & 16) == 0)
		{
			x = MultX((*FilledPolygon).Points[cnt].x);
			y = MultY((*FilledPolygon).Points[cnt].y);
			(*Points)[cnt].x = x;
			(*Points)[cnt].y = y;
		}
		else
		{
			(*Points)[cnt].x = (int32) (*FilledPolygon).Points[cnt].x;
			(*Points)[cnt].y = (int32) (*FilledPolygon).Points[cnt].y;
		}
	}

	/*
	    if (((mode & 1) == 0)
	       &&
	       ((FilledPolygon->PolygonType & OBJECT_SELECTED) == OBJECT_SELECTED)) {
	      if (!Printing) SetROP2(OutputDisplay,SelectColorMode);
	    }
	*/
#ifdef _DEBUG

	if (0)
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);

#endif
	Polygon(OutputDisplay, (POINT *) Points, count);

	/*
	    if (((mode & 1) == 0)
	       &&
	       ((FilledPolygon->PolygonType & OBJECT_SELECTED) == OBJECT_SELECTED)) {
	      if (!Printing) SetROP2(OutputDisplay,R2_COPYPEN);
	    }
	*/
	if ((mode & 8) == 8)
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawOwnPolygon(double *PolygonPoints, int32 count, int32 mode)
{
	int32 cnt;
	PointsArray3 *Points;
	double *PointP, *dx1, *dy1, *dx2, *dy2, *FirstX, *FirstY;

	AllocateSpecialMem(MEM_POINTS, count * sizeof(POINT) + 16384, (void **) &Points);

	if ((mode & 8) == 8)
		StartDrawingEditingWindow(0);

	if (ReverseY)
	{
		if ((mode & 16) == 0)
		{
			PointP = PolygonPoints;

			for (cnt = 0; cnt < count; cnt++)
			{
				(*Points)[cnt].x = MultX(*PointP);
				PointP++;
				(*Points)[cnt].y = MultY(*PointP);
				PointP++;
			}

			Polygon(OutputDisplay, (POINT *) Points, count);
		}
		else
		{
			dx1 = PolygonPoints;
			dy1 = dx1 + 1;
			FirstX = dx1;
			FirstY = dy1;

			for (cnt = 0; cnt < count; cnt++)
			{
				if (cnt < count - 1)
				{
					dx2 = dx1 + 2;
					dy2 = dy1 + 2;
				}
				else
				{
					dx2 = FirstX;
					dy2 = FirstY;
				}

				DrawLine(MultX(*dx1), MultY(*dy1), MultX(*dx2), MultY(*dy2));
				dx1 += 2;
				dy1 += 2;
			}
		}
	}
	else
	{
		if ((mode & 16) == 0)
		{
			PointP = PolygonPoints;

			for (cnt = 0; cnt < count; cnt++)
			{
				(*Points)[cnt].x = MultX(*PointP);
				PointP++;
				(*Points)[cnt].y = Mult(*PointP - Yoffset);
				PointP++;
			}

			Polygon(OutputDisplay, (POINT *) Points, count);
		}
		else
		{
			dx1 = PolygonPoints;
			dy1 = dx1 + 1;
			FirstX = dx1;
			FirstY = dy1;

			for (cnt = 0; cnt < count; cnt++)
			{
				if (cnt < count - 1)
				{
					dx2 = dx1 + 2;
					dy2 = dy1 + 2;
				}
				else
				{
					dx2 = FirstX;
					dy2 = FirstY;
				}

				DrawLine(MultX(*dx1), Mult(*dy1 - Yoffset), MultX(*dx2), Mult(*dy2 - Yoffset));
				dx1 += 2;
				dy1 += 2;
			}
		}
	}

	if ((mode & 8) == 8)
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawOwnPolygonFloat(float *PolygonPoints, int32 count, int32 mode)
{
	int32 cnt;
	PointsArray3 *Points;
	float *PointP, *dx1, *dy1, *dx2, *dy2, *FirstX, *FirstY;

	AllocateSpecialMem(MEM_POINTS, count * sizeof(POINT) + 16384, (void **) &Points);

	if ((mode & 8) == 8)
		StartDrawingEditingWindow(0);

	if (ReverseY)
	{
		if ((mode & 16) == 0)
		{
			PointP = PolygonPoints;

			for (cnt = 0; cnt < count; cnt++)
			{
				(*Points)[cnt].x = MultX(*PointP);
				PointP++;
				(*Points)[cnt].y = MultY(*PointP);
				PointP++;
			}

			Polygon(OutputDisplay, (POINT *) Points, count);
		}
		else
		{
			dx1 = PolygonPoints;
			dy1 = dx1 + 1;
			FirstX = dx1;
			FirstY = dy1;

			for (cnt = 0; cnt < count; cnt++)
			{
				if (cnt < count - 1)
				{
					dx2 = dx1 + 2;
					dy2 = dy1 + 2;
				}
				else
				{
					dx2 = FirstX;
					dy2 = FirstY;
				}

				DrawLine(MultX(*dx1), MultY(*dy1), MultX(*dx2), MultY(*dy2));
				dx1 += 2;
				dy1 += 2;
			}
		}
	}
	else
	{
		if ((mode & 16) == 0)
		{
			PointP = PolygonPoints;

			for (cnt = 0; cnt < count; cnt++)
			{
				(*Points)[cnt].x = MultX(*PointP);
				PointP++;
				(*Points)[cnt].y = Mult(*PointP - Yoffset);
				PointP++;
			}

			Polygon(OutputDisplay, (POINT *) Points, count);
		}
		else
		{
			dx1 = PolygonPoints;
			dy1 = dx1 + 1;
			FirstX = dx1;
			FirstY = dy1;

			for (cnt = 0; cnt < count; cnt++)
			{
				if (cnt < count - 1)
				{
					dx2 = dx1 + 2;
					dy2 = dy1 + 2;
				}
				else
				{
					dx2 = FirstX;
					dy2 = FirstY;
				}

				DrawLine(MultX(*dx1), Mult(*dy1 - Yoffset), MultX(*dx2), Mult(*dy2 - Yoffset));
				dx1 += 2;
				dy1 += 2;
			}
		}
	}

	if ((mode & 8) == 8)
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ConvertPoint(double CurrentX, double CurrentY, double *NewX, double *NewY)
{
	double divx, divy;

	*NewX = CurrentX;
	*NewY = CurrentY;
	divx = CurrentX - CurrentX2;
	divy = CurrentY - CurrentY2;

	if (divx > 0)
	{
		if (divy > 0)
		{
			if (divx > divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (divx * 3 < divy)
					*NewX = CurrentX2;
				else
				{
					if (divx > divy)
						*NewY = CurrentY2 + divx;
					else
						*NewX = CurrentX2 + divy;
				}
			}
		}
		else
		{
			if (divx > -divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (divx * 3 < -divy)
					*NewX = CurrentX2;
				else
				{
					if (divx > -divy)
						*NewY = CurrentY2 - divx;
					else
						*NewX = CurrentX2 - divy;
				}
			}
		}
	}
	else
	{
		if (divy > 0)
		{
			if (-divx > divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (-divx * 3 < divy)
					*NewX = CurrentX2;
				else
				{
					if (-divx > divy)
						*NewY = CurrentY2 - divx;
					else
						*NewX = CurrentX2 - divy;
				}
			}
		}
		else
		{
			if (-divx > -divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (-divx * 3 < -divy)
					*NewX = CurrentX2;
				else
				{
					if (-divx < -divy)
						*NewY = CurrentY2 + divx;
					else
						*NewX = CurrentX2 + divy;
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingPolygonObject(double CurrentX, double CurrentY, int32 Mode)
{
	int32 dikte, dikte2, x1a, y1a;
	PolygonRecord *DrawPolygon2;
	ObjectLineRecord AreaFillObjectLine;
#ifdef _DEBUG
	char str[200];
#endif

	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
#ifdef _DEBUG
	sprintf(str, "(%04d) Draw trying polyline\n", DrawTryingPolylineCount);
	OutputDebugString(str);
	DrawTryingPolylineCount++;
#endif

	switch (PolygonObjectType)
	{
	case OBJECT_POLYLINE:
		if (((Mode & 1) == 0) && (NewObjectLine.Clearance == 0.0))
		{
			if ((Mode & 16) == 0)
				DrawCrossHair(16);
			else
				DrawCrossHair(16 + 2);

			return;
		}

		StartDrawingEditingWindow(BM_DoubleBuffer);
		SetROP2(OutputDisplay, R2_XORPEN);
		NewObjectLine.X1 = (float) CurrentX2;
		NewObjectLine.Y1 = (float) CurrentY2;
		NewObjectLine.X2 = (float) CurrentX;
		NewObjectLine.Y2 = (float) CurrentY;
		/*
		      NewObjectLine.X1=CurrentX2+12.0;
		      NewObjectLine.Y1=CurrentY2+11.0;
		      NewObjectLine.X2=CurrentX+10.0;
		      NewObjectLine.Y2=CurrentY+9.0;
		*/
		x1a = MultX(CurrentX);
		y1a = MultY(CurrentY);
		dikte = Mult(NewObjectLine.LineThickNess);
		InitDrawingObject(0, POLYGON_DRAW_LAYER, dikte, DRAW_WITH_PEN_AND_NOT_FILLED);
		dikte2 = Mult(NewObjectLine.Clearance);

		if ((Mode & 1) == 0)
		{
			ellips2(x1a, y1a, dikte2, dikte2, 255);

			if ((Mode & 16) == 0)
				DrawCrossHair(16 + 8);
			else
				DrawCrossHair(16 + 8 + 2);

			ExitDrawing();
			EndDrawingEditingWindow(BM_DoubleBuffer);
			return;
		}

		DrawLine(x1a, y1a, MultX(CurrentX2), MultY(CurrentY2));

		if (DrawPolygon2->NrVertices > 0)
			DrawLine(x1a, y1a, MultX(FirstX), MultY(FirstY));

		ellips2(x1a, y1a, dikte2, dikte2, 255);

		if ((Mode & 16) == 0)
			DrawCrossHair(16 + 8);
		else
			DrawCrossHair(16 + 8 + 2);

		ExitDrawing();
		EndDrawingEditingWindow(BM_DoubleBuffer);
		break;

	case OBJECT_RECT:
		StartDrawingEditingWindow(BM_DoubleBuffer);
		SetROP2(OutputDisplay, R2_XORPEN);
		InitDrawingObject(0, POLYGON_DRAW_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);
		AreaFillObjectRect.CentreX = (float) CurrentX;
		AreaFillObjectRect.CentreY = (float) CurrentY;
		DrawObjectRect(&AreaFillObjectRect, 0.0, 0.0, 2);
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) - 3, MultX(CurrentX) + 3, MultY(CurrentY) + 3);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) + 3, MultX(CurrentX) + 3, MultY(CurrentY) - 3);
		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow(BM_DoubleBuffer);
		break;

	case OBJECT_CIRCLE:
		StartDrawingEditingWindow(BM_DoubleBuffer);
		SetROP2(OutputDisplay, R2_XORPEN);
		InitDrawingObject(0, POLYGON_DRAW_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);

		AreaFillObjectArc.CentreX = (float) CurrentX;
		AreaFillObjectArc.CentreY = (float) CurrentY;
		DrawObjectArc(&AreaFillObjectArc, 0.0, 0.0, 2);

		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) - 3, MultX(CurrentX) + 3, MultY(CurrentY) + 3);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) + 3, MultX(CurrentX) + 3, MultY(CurrentY) - 3);

		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow(BM_DoubleBuffer);
		break;

	case PIN_LINE_HOR:
		StartDrawingEditingWindow(BM_DoubleBuffer);
		SetROP2(OutputDisplay, R2_XORPEN);
		dikte = Mult(AreaFillObjectHorLine.Y2);
		InitDrawingObject(0, POLYGON_DRAW_LAYER, dikte, DRAW_WITH_PEN_AND_NOT_FILLED);
		memmove(&AreaFillObjectLine, &AreaFillObjectHorLine, sizeof(ObjectLineRecord));
		AreaFillObjectHorLine.X1 = (float) CurrentX;
		AreaFillObjectHorLine.Y1 = (float) CurrentY;
		AreaFillObjectLine.X1 = AreaFillObjectHorLine.X1;
		AreaFillObjectLine.Y1 = AreaFillObjectHorLine.Y1;
		AreaFillObjectLine.X2 = AreaFillObjectHorLine.X1 + AreaFillObjectHorLine.X2;
		AreaFillObjectLine.Y2 = AreaFillObjectHorLine.Y1;
		DrawObjectLine(&AreaFillObjectLine, 0.0, 0.0, 2);
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) - 3, MultX(CurrentX) + 3, MultY(CurrentY) + 3);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) + 3, MultX(CurrentX) + 3, MultY(CurrentY) - 3);
		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow(BM_DoubleBuffer);
		break;

	case PIN_LINE_VER:
		StartDrawingEditingWindow(BM_DoubleBuffer);
		SetROP2(OutputDisplay, R2_XORPEN);
		dikte = Mult(AreaFillObjectVerLine.X2);
		InitDrawingObject(0, POLYGON_DRAW_LAYER, dikte, DRAW_WITH_PEN_AND_NOT_FILLED);
		memmove(&AreaFillObjectLine, &AreaFillObjectVerLine, sizeof(ObjectLineRecord));
		AreaFillObjectVerLine.X1 = (float) CurrentX;
		AreaFillObjectVerLine.Y1 = (float) CurrentY;
		AreaFillObjectLine.X1 = AreaFillObjectVerLine.X1;
		AreaFillObjectLine.Y1 = AreaFillObjectVerLine.Y1;
		AreaFillObjectLine.X2 = AreaFillObjectVerLine.X1;
		AreaFillObjectLine.Y2 = AreaFillObjectVerLine.Y1 + AreaFillObjectVerLine.Y2;
		DrawObjectLine(&AreaFillObjectLine, 0.0, 0.0, 2);
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) - 3, MultX(CurrentX) + 3, MultY(CurrentY) + 3);
		DrawLine(MultX(CurrentX) - 3, MultY(CurrentY) + 3, MultX(CurrentX) + 3, MultY(CurrentY) - 3);
		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow(BM_DoubleBuffer);
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddTryingPolygonLine()
{
	int32 ok, count, dikte;
	PolygonRecord *DrawPolygon2;
    //PolygonObjectType
	StartDrawingEditingWindow(0);
	dikte = Mult(NewObjectLine.LineThickNess);
	InitDrawingObject(0, POLYGON_DRAW_LAYER, dikte, DRAW_WITH_PEN_AND_NOT_FILLED);

	if ((InRange(NewObjectLine.X1, NewObjectLine.X2)) && (InRange(NewObjectLine.Y1, NewObjectLine.Y2)))
		ok = 1;

	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
	count = DrawPolygon2->NrVertices;

	if (count == 0)
	{
		(*DrawPolygon2).Points[count].x = NewObjectLine.X1;
		(*DrawPolygon2).Points[count].y = NewObjectLine.Y1;
		(*DrawPolygon2).Points[count + 1].x = NewObjectLine.X2;
		(*DrawPolygon2).Points[count + 1].y = NewObjectLine.Y2;
//    FirstX=NewObjectLine.X1;
//    FirstY=NewObjectLine.Y1;
		count = 1;
	}
	else
	{
		if (count < 195)
		{
			(*DrawPolygon2).Points[count + 1].x = NewObjectLine.X2;
			(*DrawPolygon2).Points[count + 1].y = NewObjectLine.Y2;
			count++;
		}
	}

	DrawPolygon2->NrVertices = count;
	DrawLine(MultX(NewObjectLine.X1), MultY(NewObjectLine.Y1), MultX(NewObjectLine.X2), MultY(NewObjectLine.Y2));

//  NewObjectLine.Info=OBJECT_ADDED;
//  if (AddObjectLine(&NewObjectLine)) DrawObjectLine(&NewObjectLine,0.0,0.0,0);

	DrawCrossHair(8);
	ExitDrawing();

	EndDrawingEditingWindow(0);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetSizeCutoutObjectAreaFill(LPSTR str, int32 ObjectType)
{
	switch (ObjectType)
	{
	case OBJECT_POLYLINE:
		break;

	case OBJECT_RECT:
		if (Units == 0)
			sprintf(str, " [ %.1f x %.1f ]", AreaFillObjectRect.Width / 2540.0, AreaFillObjectRect.Height / 2540.0);
		else
			sprintf(str, " [ %.4f x %.4f ]", AreaFillObjectRect.Width / 100000.0, AreaFillObjectRect.Height / 100000.0);

		break;

	case OBJECT_CIRCLE:
		if (Units == 0)
			sprintf(str, " [ %.1f ]", AreaFillObjectArc.Width / 2540.0);
		else
			sprintf(str, " [ %.4f ]", AreaFillObjectArc.Width / 100000.0);

		break;

	case PIN_LINE_HOR:
		if (Units == 0)
		{
			sprintf(str, " [ %.1f x %.1f ]", (AreaFillObjectHorLine.X2 + AreaFillObjectHorLine.Y2) / 2540.0,
			        AreaFillObjectHorLine.Y2 / 2540.0);
		}
		else
		{
			sprintf(str, " [ %.4f x %.4f ]", (AreaFillObjectHorLine.X2 + AreaFillObjectHorLine.Y2) / 100000.0,
			        AreaFillObjectHorLine.Y2 / 100000.0);
		}

		break;

	case PIN_LINE_VER:
		if (Units == 0)
		{
			sprintf(str, " [ %.1f x %.1f ]", AreaFillObjectVerLine.X2 / 2540.0,
			        (AreaFillObjectVerLine.X2 + AreaFillObjectVerLine.Y2) / 2540.0);
		}
		else
		{
			sprintf(str, " [ %.4f x %.4f ]", AreaFillObjectVerLine.X2 / 100000.0,
			        (AreaFillObjectVerLine.X2 + AreaFillObjectVerLine.Y2) / 100000.0);
		}

		break;

	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PolygonLinesDrawFunc1(int32 Mode2)
{
	if ((!ShiftPressed) || (PolygonDrawObjectType == OBJECT_POLYLINE))
		DrawTryingPolygonObject(OldX, OldY, Mode2);
	else
		DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);

	DrawCrossObjects(0, 1);

	if (PolygonDrawObjectType == OBJECT_POLYLINE)
		DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PolygonLinesDrawFunc2(int32 Mode2)
{
	double NewX, NewY;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if ((PolygonDrawObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
	{
		ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
		CurrentX = NewX;
		CurrentY = NewY;
	}

	OldX = CurrentX;
	OldY = CurrentY;

	if (!SelectionEsc)
	{
		if (PolygonDrawObjectType == OBJECT_POLYLINE)
			DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);

		DrawCrossObjects(0, 1);

		if ((!ShiftPressed) || (PolygonDrawObjectType == OBJECT_POLYLINE))
			DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
		else
			DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CommandAddPolygonLines(double LineThickNess, int32 Layer, int32 Mode, int32 ObjectType)
{
	int32 cnt, cnt2, cnt3, ok, InsertMode, OkToAdd, Mode2, count, Mode3, CurrentAreaFillNr, AreaFillDeletionCount,
	      count2;
	double x1, y1, NewX, NewY, x5, y5, hoek, RandValue;
	int32 CircleRoundings = 24;
	int32 FirstShift = 1, LeftButtonFinishPolygon = 0;
	char TextLine[2048], str[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	LPSTR TextP, TextP2;
	PolygonRecord *DrawPolygon2;
	HMENU PopUpMenu;
	NetRecord *Net;
	DrawXorFunctionRecord DrawXorFunction;

	Net = &((*Nets)[CurrentDrawingNetNr]);
	CurrentAreaFillNr = 0;
	ShiftX = 0.0;
	ShiftY = 0.0;
	OldShiftX = 0.0;
	OldShiftY = 0.0;
	RandValue = 0.0;
	PopUpMenu = NULL;

//  PopUpMenu=CreatePopupMenu();
//  AppendMenuUTF8(PopUpMenu,MF_ENABLED|MF_STRING,ID_POPUP_ESCAPE,SC(493,"Escape"));

	strcpy(str3, InfoStr);
	PolygonObjectType = ObjectType;
	AreaFillDeletionCount = 1;

	switch (Mode & 0x0f)
	{
	case 0:
		break;

	case 1:
		CurrentAreaFillNr = FindSelectedAreaFill(0);

		if (CurrentAreaFillNr == -1)
			return -1;

		break;

	case 2:
		if ((CurrentAreaFillNr = GetPowerPlaneByLayer(Layer)) == -1)
			return -1;

		break;
	}

	OldValue2 = 0.1123;

	SelectionEsc = 0;
	FinishPolygon = 0;
//  CurrentDrawMode=1;

	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	if ((ObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
	{
		ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
		CurrentX = NewX;
		CurrentY = NewY;
	}

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	switch (ObjectType)
	{
	case OBJECT_POLYLINE:
		CrossHairMode = 1;
		break;

	case OBJECT_RECT:
		AreaFillObjectRect.CentreX = (float) CurrentX2;
		AreaFillObjectRect.CentreY = (float) CurrentY2;
		AreaFillObjectRect.Layer = Layer;
		break;

	case OBJECT_ARC:
	case OBJECT_CIRCLE:
		AreaFillObjectArc.CentreX = (float) CurrentX2;
		AreaFillObjectArc.CentreY = (float) CurrentX2;
		AreaFillObjectArc.Layer = Layer;
		break;

	case PIN_LINE_HOR:
		AreaFillObjectHorLine.X1 = (float) CurrentX2;
		AreaFillObjectHorLine.Y1 = (float) CurrentY2;
		AreaFillObjectHorLine.Layer = Layer;
		break;

	case PIN_LINE_VER:
		AreaFillObjectVerLine.X1 = (float) CurrentX2;
		AreaFillObjectVerLine.Y1 = (float) CurrentY2;
		AreaFillObjectVerLine.Layer = Layer;
		break;
	}

	x1 = CurrentX;
	y1 = CurrentY;
	OkToAdd = 0;
	Mode2 = 0;

	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
	memset(DrawPolygon2, 0, sizeof(PolygonRecord));
	/*
	  if ((Mode==1)
	     &&
	     (StartPolygon==1)) {
	    CurrentPolygon=(PolygonRecord *)&PolygonBuf2;
	    CopyPolygonToPolygon(CurrentPolygon,DrawPolygon);
	    MakeNewAreaFill();
	    return;
	  }

	  DrawPolygon->Points[0].x=1422412;
	  DrawPolygon->Points[0].y=6395731;
	  DrawPolygon->Points[1].x=6131570;
	  DrawPolygon->Points[1].y=6395729;
	  DrawPolygon->Points[2].x=6131570;
	  DrawPolygon->Points[2].y=1859289;
	  DrawPolygon->Points[3].x=1422410;
	  DrawPolygon->Points[3].y=1859289;


	  DrawPolygon->NrVertices=4;
	  MakeNewAreaFill();
	  return;
	*/


	OldX = CurrentX;
	OldY = CurrentY;
	InsertMode = 0;
	count2 = 0;
	ClipMouseCursor();
	DrawCrossObjects(0, 1);

	NewObjectLine.LineThickNess = (float) LineThickNess;
	StartPolygon = 1;

	if (ObjectType != OBJECT_POLYLINE)
		DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

	str[0] = 0;
	GetSizeCutoutObjectAreaFill(str, ObjectType);
	sprintf(InfoStr, "%s  %s", Net->Name, str);
	RedrawInfoStr(1);
	SystemBusyMode = 100;
	PolygonDrawObjectType = ObjectType;
	DrawXorFunction.Function4a = (FUNCP4) PolygonLinesDrawFunc1;
	DrawXorFunction.Function4b = (FUNCP4) PolygonLinesDrawFunc2;
	DrawXorFunction.Param1[0] = &Mode2;
	DrawXorFunction.Mode = 3;
	DrawXorFunction.Param2[0] = &Mode2;
	ZoomInOutProcessed = 0;

	while ((!SelectionEsc) && (!FinishPolygon))
	{
// ****************************************************************************
		if (MouseChanged)
		{
#ifdef _DEBUG
			sprintf(str, "(%04d) Mouse changed\n", MouseChangedCount);
			OutputDebugString(str);
			MouseChangedCount++;
#endif

			if (NewAreaFillClearance > 0.0)
			{
				TempAreaFillClearance = NewAreaFillClearance;
				NewObjectLine.Clearance = (float) TempAreaFillClearance;
				NewAreaFillClearance = -1.0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((ObjectType == OBJECT_POLYLINE) && (Mode2 > 0) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if ((!ShiftPressed) || (ObjectType == OBJECT_POLYLINE))
					DrawTryingPolygonObject(OldX, OldY, Mode2);
				else
					DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				else
				{
					switch (ObjectType)
					{
					case OBJECT_POLYLINE:
						break;

					case OBJECT_RECT:
//              AreaFillObjectRect.Width=fabs(OldWidth-(ShiftX-CurrentX)*1.0);
//              AreaFillObjectRect.Height=fabs(OldHeight-(ShiftY-CurrentY)*1.0);
						AreaFillObjectRect.Width = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectRect.Height = (float) fabs((ShiftY - CurrentY) * 2.0);
						break;

					case OBJECT_CIRCLE:
						AreaFillObjectArc.Width = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectArc.Height = (float) fabs((ShiftX - CurrentX) * 2.0);
						break;

					case PIN_LINE_HOR:
						AreaFillObjectHorLine.X2 = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectHorLine.Y2 = (float) fabs((ShiftY - CurrentY) * 2.0);
						break;

					case PIN_LINE_VER:
						AreaFillObjectVerLine.X2 = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectVerLine.Y2 = (float) fabs((ShiftY - CurrentY) * 2.0);
						break;
					}

					str[0] = 0;
					GetSizeCutoutObjectAreaFill(str, ObjectType);
					sprintf(InfoStr, "%s  %s", Net->Name, str);
					RedrawInfoStr(1);
					DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);
				}
			}

// ****************************************************************************
			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				PolygonLinesDrawFunc1(Mode2 | 16);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				PolygonLinesDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				PolygonLinesDrawFunc1(Mode2 | 16);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				PolygonLinesDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				PolygonLinesDrawFunc1(Mode2 | 16);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				PolygonLinesDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				PolygonLinesDrawFunc1(Mode2 | 16);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				PolygonLinesDrawFunc2(Mode2);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

// ****************************************************************************
		if (ObjectType != OBJECT_POLYLINE)
		{
			if (!ShiftPressed)
			{
				if (!FirstShift)
				{
					DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);
					FirstShift = 1;
					DisplayCursorPosition();
					CurrentX = OldShiftX;
					CurrentY = OldShiftY;
					SetNewCursor(&CurrentX, &CurrentY);
					OldX = CurrentX;
					OldY = CurrentY;
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				}
			}
			else
			{
				if (FirstShift)
				{
					ShiftX = CurrentX;
					ShiftY = CurrentY;
					OldShiftX = CurrentX;
					OldShiftY = CurrentY;
					FirstShift = 0;

					switch (ObjectType)
					{
					case OBJECT_RECT:
						CurrentX += AreaFillObjectRect.Height * 0.5;
						CurrentY -= AreaFillObjectRect.Width * 0.5;
						break;

					case OBJECT_CIRCLE:
						CurrentX += AreaFillObjectArc.Width * 0.5;
						CurrentY -= AreaFillObjectArc.Width * 0.5;
						break;

					case PIN_LINE_HOR:
						CurrentX += AreaFillObjectHorLine.X2 * 0.5;
						CurrentY -= AreaFillObjectHorLine.Y2 * 0.5;
						break;

					case PIN_LINE_VER:
						CurrentX += AreaFillObjectVerLine.X2 * 0.5;
						CurrentY -= AreaFillObjectVerLine.Y2 * 0.5;
						break;
					}

					SetNewCursor(&CurrentX, &CurrentY);
					OldX = CurrentX;
					OldY = CurrentY;
				}
				else
				{
				}
			}
		}

		CheckInputMessages(0);
// ****************************************************************************

		if (ZoomInOutProcessed)
		{
			PolygonLinesDrawFunc2(Mode2);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			PolygonLinesDrawFunc1(Mode2 | 16);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();
			PolygonLinesDrawFunc2(Mode2);
		}

// ****************************************************************************
		if ((ZoomActive()) && (!SelectionEsc))
		{
			PolygonLinesDrawFunc1(Mode2 | 16);
			ZoomWindow();
			PolygonLinesDrawFunc2(Mode2);
		}

// ****************************************************************************
		if ((PanActive()) && (!SelectionEsc))
		{
			PolygonLinesDrawFunc1(Mode2 | 16);
			PanWindow();
			PolygonLinesDrawFunc2(Mode2);
		}

// ****************************************************************************
		if (CheckLeftButton())
		{
			DrawTryingPolygonObject(OldX, OldY, Mode2 | 16);
			RandValue = GetNewRandomValue(0);
			
			switch (ObjectType)
			{
			case OBJECT_POLYLINE:
				if (Mode2 > 0)
				{
					if ((NotInRange(NewObjectLine.X1, NewObjectLine.X2))
					        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
					{
						CommandAddTryingPolygonLine();

						if ((DrawPolygon2->NrVertices > 1) && (InRange(FirstX, CurrentX))
						        && (InRange(FirstY, CurrentY)))
						{
							FinishPolygon = 1;
							LeftButtonFinishPolygon = 1;
							InsertMode = 1;
						}
					}

					CurrentX2 = CurrentX;
					CurrentY2 = CurrentY;
				}
				else
				{
					CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
					CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
					CurrentX2 = CurrentX;
					CurrentY2 = CurrentY;
					FirstX = CurrentX2;
					FirstY = CurrentY2;

					if ((ObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
					{
						ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
						CurrentX = NewX;
						CurrentY = NewY;
					}

					OldX = CurrentX;
					OldY = CurrentY;
				}

				Mode2 = min(1, Mode2 + 1);

				if (!FinishPolygon)
				{
					//        DrawTryingPolygon(0);
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				}

				break;

			case OBJECT_RECT:
			case OBJECT_CIRCLE:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
				AreaFillDeletionCount = CurrentAreaFillNr;

				switch (ObjectType)
				{
				case OBJECT_RECT:
					x5 = AreaFillObjectRect.Width * 0.5;
					x5 += RandValue;
					y5 = AreaFillObjectRect.Height * 0.5;
					y5 += RandValue;
					DrawPolygon2->Points[0].x = AreaFillObjectRect.CentreX + x5;
					DrawPolygon2->Points[0].y = AreaFillObjectRect.CentreY + y5;
					DrawPolygon2->Points[1].x = AreaFillObjectRect.CentreX + x5;
					DrawPolygon2->Points[1].y = AreaFillObjectRect.CentreY - y5;
					DrawPolygon2->Points[2].x = AreaFillObjectRect.CentreX - x5;
					DrawPolygon2->Points[2].y = AreaFillObjectRect.CentreY - y5;
					DrawPolygon2->Points[3].x = AreaFillObjectRect.CentreX - x5;
					DrawPolygon2->Points[3].y = AreaFillObjectRect.CentreY + y5;
					DrawPolygon2->NrVertices = 4;

					if ((InRange(AreaFillObjectRect.Width, 0.0)) || (InRange(AreaFillObjectRect.Height, 0.0)))
						DrawPolygon2->NrVertices = 0;

					break;

				case OBJECT_CIRCLE:
					for (cnt = 0; cnt < CircleRoundings; cnt++)
					{
						DrawPolygon2->Points[cnt].x = AreaFillObjectArc.CentreX;
						DrawPolygon2->Points[cnt].y = AreaFillObjectArc.CentreY;
						hoek = cnt;
						hoek *= ANGLE_360 / CircleRoundings;
						x5 = cos(hoek);
						x5 *= AreaFillObjectArc.Width * 0.5;
						x5 += RandValue;
						y5 = sin(hoek);
						y5 *= AreaFillObjectArc.Width * 0.5;
						y5 += RandValue;
						DrawPolygon2->Points[cnt].x += x5;
						DrawPolygon2->Points[cnt].y += y5;
					}

					DrawPolygon2->NrVertices = CircleRoundings;

					if (InRange(AreaFillObjectArc.Width, 0.0))
						DrawPolygon2->NrVertices = 0;

					break;

				case PIN_LINE_HOR:
					cnt2 = 0;

					for (cnt = CircleRoundings / 4; cnt < ((CircleRoundings * 5) / 4) + 1; cnt++)
					{
						hoek = cnt;
						hoek *= ANGLE_360 / CircleRoundings;
						x5 = cos(hoek);
						x5 *= AreaFillObjectHorLine.Y2 * 0.5;
//                x5+=AreaFillDeletionCount*24.5;
						y5 = sin(hoek);
						y5 *= AreaFillObjectHorLine.Y2 * 0.5;

//                y5+=AreaFillDeletionCount*24.5;
						if (cnt == ((CircleRoundings * 3) / 4))
						{
							DrawPolygon2->Points[cnt2].x = AreaFillObjectHorLine.X1;
							DrawPolygon2->Points[cnt2].y = AreaFillObjectHorLine.Y1;
							DrawPolygon2->Points[cnt2].x += x5;
							DrawPolygon2->Points[cnt2].y += y5;
							cnt2++;
						}

						DrawPolygon2->Points[cnt2].x = AreaFillObjectHorLine.X1;
						DrawPolygon2->Points[cnt2].y = AreaFillObjectHorLine.Y1;

						if (cnt >= ((CircleRoundings * 3) / 4))
							DrawPolygon2->Points[cnt2].x += AreaFillObjectHorLine.X2;

						DrawPolygon2->Points[cnt2].x += x5;
						DrawPolygon2->Points[cnt2].y += y5;
						cnt2++;
					}

					DrawPolygon2->NrVertices = CircleRoundings + 2;

					if (InRange(AreaFillObjectHorLine.Y2, 0.0))
						DrawPolygon2->NrVertices = 0;

					break;

				case PIN_LINE_VER:
					cnt2 = 0;

					for (cnt = 0; cnt < CircleRoundings + 1; cnt++)
					{
						hoek = cnt;
						hoek *= ANGLE_360 / CircleRoundings;
						x5 = cos(hoek);
						x5 *= AreaFillObjectVerLine.X2 * 0.5;
						x5 += RandValue;
						y5 = sin(hoek);
						y5 *= AreaFillObjectVerLine.X2 * 0.5;
						y5 += RandValue;

						if (cnt == (CircleRoundings * 2) / 4)
						{
							DrawPolygon2->Points[cnt2].x = AreaFillObjectVerLine.X1;
							DrawPolygon2->Points[cnt2].y = AreaFillObjectVerLine.Y1;
							DrawPolygon2->Points[cnt2].y += AreaFillObjectVerLine.Y2;
							DrawPolygon2->Points[cnt2].x += x5;
							DrawPolygon2->Points[cnt2].y += y5;
							cnt2++;
						}

						DrawPolygon2->Points[cnt2].x = AreaFillObjectVerLine.X1;
						DrawPolygon2->Points[cnt2].y = AreaFillObjectVerLine.Y1;

						if (cnt < ((CircleRoundings * 2) / 4))
							DrawPolygon2->Points[cnt2].y += AreaFillObjectVerLine.Y2;

						DrawPolygon2->Points[cnt2].x += x5;
						DrawPolygon2->Points[cnt2].y += y5;
						cnt2++;
					}

					DrawPolygon2->NrVertices = CircleRoundings + 2;

					if (InRange(AreaFillObjectArc.Width, 0.0))
						DrawPolygon2->NrVertices = 0;

					break;
				}

				if (DrawPolygon2->NrVertices > 0)
				{
					DrawCrossObjects(0, 1);

					if (DeleteFromAreaFill(CurrentAreaFillNr, 0) == 0)
					{
						CheckInputMessages(0);
						CurrentAreaFillNr = Design.NrAreaFills - 1;
						DrawCrossObjects(0, 1);
						DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
//              AreaFillDeletionCount++;
						LastActionNr++;
						count2++;
					}
					else
						SelectionEsc = 1;
				}
				else
				{
					Beep(1000, 200);
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				}

				break;
			}

			CheckInputMessages(0);
		}

// ****************************************************************************
		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingPolygonObject(OldX, OldY, Mode2 | 16);

			switch (ObjectType)
			{
			case OBJECT_POLYLINE:
				PopUpMenu = CreatePopupMenu();

				if (DrawPolygon2->NrVertices > 1)
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_FINISH_POLYGON, SC(990, "Finish"));

				if (LinesAllDirection)
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_45_DIR,
					              SC(991, "Draw with 45/90 degrees directions"));
				}
				else
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_ALL_DIR,
					              SC(992, "Draw in all directions"));
				}

				if (DrawPolygon2->NrVertices > 0)
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_TRACE_BACKWARDS,
					              SC(993, "Goto previous point"));
				}

				if (Mode == 0)
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_CLEARANCE_WIDTH_DIALOG,
					              SC(1274, "Change drawing thickness"));
				}

				if (CrossHairType == 1)
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_BITMAP, ID_VIEW_CROSS_TYPE1, (LPSTR) BitMapCross1);
				else
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_BITMAP, ID_VIEW_CROSS_TYPE2, (LPSTR) BitMapCross2);

				break;

			case OBJECT_RECT:
			case OBJECT_CIRCLE:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
				PopUpMenu = CreatePopupMenu();
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(493, "Escape"));
				break;
			}

			TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
			               RealWindow.top + MousePosY + 40, 0, PCBWindow, NULL);
			DestroyMenu(PopUpMenu);
//      RightButtonPressed=0;
			CheckInputMessages(0);

			if (!SelectionEsc)
				DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

			ok = 1;
		}

// ****************************************************************************
		if (NrFunctionsInBuf > 0)
		{
			PolygonLinesDrawFunc1(Mode2 | 16);
			ClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				switch (ObjectType)
				{
				case OBJECT_POLYLINE:
					AllocateSpecialMem(MEM_NET_SELECTED, 32 * 1024, (void **) &TextP);
					*TextP = 0;

					if (TextInputDialog2(TextP, " (x1,y1,x2,y2,x3,y3,x4,y4, .... )", 0) == 1)
					{
						int32 polyParametersRelative = 0;

						CheckInputMessages(200);
						cnt = 0;
						DrawPolygon2->NrVertices = 0;

						while ((*TextP != 0) && (DrawPolygon2->NrVertices < 200))
						{
							TextP2 = TextP;
							memset(&str, 0, sizeof(str));
							cnt3 = 0;

							while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
							{
								str[cnt3++] = *TextP;
								str[cnt3] = 0;
								TextP++;
							}

							if (*TextP == ',')
								TextP++;

							if (*TextP == '\r')
								TextP++;

							if (*TextP != 0)
							{
								str[cnt3++] = ',';
								str[cnt3] = 0;

								while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
								{
									str[cnt3++] = *TextP;
									str[cnt3] = 0;
									TextP++;
								}

								if (*TextP == ',')
									TextP++;

								if (*TextP == '\r')
									TextP++;

								if (((NrParams = ScanParameters(-1, str, 0)) >= 2) && ((NrParams & 1) == 0))
								{
									if (ParametersRelative)
										polyParametersRelative = 1;

									for (cnt2 = 0; cnt2 < NrParams / 2; cnt2++)
									{
										if (!polyParametersRelative)
										{

											if (DrawPolygon2->NrVertices < 200)
											{
												(*DrawPolygon2).Points[cnt].x = ParamsFloat[cnt2 * 2];
												(*DrawPolygon2).Points[cnt].y = ParamsFloat[cnt2 * 2 + 1];
												cnt++;
												DrawPolygon2->NrVertices++;
											}
										}
										else
										{
											if (DrawPolygon2->NrVertices < 200)
											{
												(*DrawPolygon2).Points[cnt].x = RelX + ParamsFloat[cnt2 * 2];
												(*DrawPolygon2).Points[cnt].y = RelY + ParamsFloat[cnt2 * 2 + 1];
												cnt++;
												DrawPolygon2->NrVertices++;
											}
										}
									}
								}
							}
						}

						if (DrawPolygon2->NrVertices > 2)
						{
							FinishPolygon = 1;
							InsertMode = 2;
						}
					}

					DeallocateSpecialMem(MEM_NET_SELECTED);
					break;

				case OBJECT_RECT:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(995, "Rectangle size width, height")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 2)
						{
							AreaFillObjectRect.Width = (float) ParamsFloat[0];
							AreaFillObjectRect.Height = (float) ParamsFloat[1];
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
							sprintf(InfoStr, "%s  %s", Net->Name, str);
							RedrawInfoStr(1);
						}
					}

					break;

				case OBJECT_ARC:
				case OBJECT_CIRCLE:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(996, "Circle diameter")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 1)
						{
							AreaFillObjectArc.Width = (float) ParamsFloat[0];
							AreaFillObjectArc.Height = (float) ParamsFloat[0];
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
							sprintf(InfoStr, "%s  %s", Net->Name, str);
							RedrawInfoStr(1);
						}
					}

					break;

				case PIN_LINE_HOR:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(997, "Horizontal trace size width,height")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 2)
						{
							AreaFillObjectHorLine.X2 = (float) max(0.0, ParamsFloat[0] - ParamsFloat[1]);
							AreaFillObjectHorLine.Y2 = (float) ParamsFloat[1];
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
							sprintf(InfoStr, "%s  %s", Net->Name, str);
							RedrawInfoStr(1);
						}
					}

					break;

				case PIN_LINE_VER:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(998, "Vertical trace size width,height")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 2)
						{
							AreaFillObjectVerLine.X2 = (float) ParamsFloat[0];
							AreaFillObjectVerLine.Y2 = (float) max(0.0, ParamsFloat[1] - ParamsFloat[0]);
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
							sprintf(InfoStr, "%s  %s", Net->Name, str);
							RedrawInfoStr(1);
						}
					}

					break;
				}

				SpacePressed = 0;
			}

			if (TraceBackWardsKeyPressed)
			{
				if (ObjectType == OBJECT_POLYLINE)
				{
					if (DrawPolygon2->NrVertices > 0)
					{
						count = DrawPolygon2->NrVertices;
						CurrentX2 = (*DrawPolygon2).Points[count - 1].x;
						CurrentY2 = (*DrawPolygon2).Points[count - 1].y;
						DrawPolygon2->NrVertices--;
					}
				}

				TraceBackWardsKeyPressed = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((ObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (HelpAsked)
			{
				switch (Mode)
				{
				case 0:
					Help("add_areafills.htm", 0);
					break;

				case 1:
					Help("cut_from_areafill.htm", 0);
					break;

				case 2:
					Help("cut_from_powerplane.htm", 0);
					break;
				}

				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			if (SelectionEsc)
			{
				NewObjectLine.X2 = (float) x1;
				NewObjectLine.Y2 = (float) y1;

				if (ObjectType == OBJECT_POLYLINE)
					DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 1);
			}
			else
			{
				switch (ObjectType)
				{
				case OBJECT_POLYLINE:
					if ((InsertMode < 2) && (!ZoomInOutProcessed))
						PolygonLinesDrawFunc2(Mode2);

					/*
					              DrawTryingPolygon(Mult(NewObjectLine.LineThickNess),0);
					              DrawCrossObjects(0,1);
					              DrawTryingPolygonObject(CurrentX,CurrentY,Mode2);
					*/
					break;

				case OBJECT_RECT:
				case OBJECT_CIRCLE:
				case PIN_LINE_HOR:
				case PIN_LINE_VER:
					if (!ZoomInOutProcessed)
						PolygonLinesDrawFunc2(Mode2);

					/*
					            DrawCrossObjects(0,1);
					            if (!ShiftPressed) {
					              DrawTryingPolygonObject(CurrentX,CurrentY,Mode2);
					            } else {
					              DrawTryingPolygonObject(ShiftX,ShiftY,Mode2);
					            }
					*/
					break;
				}
			}

			ClipMouseCursor();
		}
	}

// ****************************************************************************

	switch (ObjectType)
	{
	case OBJECT_POLYLINE:
		if (FinishPolygon)
		{
			if (InsertMode == 0)
			{
				DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

				if ((!LeftButtonFinishPolygon) || (NotInRange(NewObjectLine.X1, NewObjectLine.X2))
				        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
					CommandAddTryingPolygonLine();
			}

			if (InsertMode < 2)
			{
				if (DrawPolygon2->NrVertices > 2)
				{
					if (ObjectType == OBJECT_POLYLINE)
						DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 1);

					DrawCrossObjects(0, 1);
					OkToAdd = 1;
				}
			}
			else
				OkToAdd = 1;

			if ((OkToAdd == 1) && (Mode & 0x0f))
			{
				for (cnt = 0; cnt < DrawPolygon2->NrVertices; cnt++)
				{
					DrawPolygon2->Points[cnt].x += RandValue;
					DrawPolygon2->Points[cnt].y += RandValue;
				}

				Mode3 = 0;

				if (Mode & 0x10)
					Mode3 = 2;

				DeleteFromAreaFill(CurrentAreaFillNr, Mode3);
			}
		}

		break;

	case OBJECT_RECT:
	case OBJECT_CIRCLE:
		break;
	}

	UnClipMouseCursor();
	sprintf(InfoStr, str3);
	RedrawInfoStr(1);

	if (count2 > 0)
		LastActionNr--;

	SystemBusyMode = 0;
	CrossHairMode = 0;
	DrawCrossHair(16 + 2);
	SetMinMaxPolygon(DrawPolygon2, 0);

	if (OkToAdd == 1)
		return 0;

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindNextNrPolygonB(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MAX_NR_CROSSES];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonB < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonB;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt < NrCrossPoints - 1)
			return Sort[cnt + 1];
		else
			return Sort[0];
	}
	else
		ok = 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindPreviousNrPolygonB(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MAX_NR_CROSSES];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonB < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonB;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt > 0)
			return Sort[cnt - 1];
		else
			return Sort[NrCrossPoints - 1];
	}
	else
	{
		ok = 1;
		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindCrossIndexNrPolygonB(int32 Nr)
{
	int32 cnt;

	cnt = 0;

	while ((cnt < NrCrossPoints) && (CrossPoints[cnt].LineNrPolygonB != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
		return cnt;
	else
		return 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindNextNrPolygonA(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MAX_NR_CROSSES];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonA < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonA;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt < NrCrossPoints - 1)
			return Sort[cnt + 1];
		else
			return Sort[0];
	}
	else
	{
		ok = 1;
		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindPreviousNrPolygonA(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MAX_NR_CROSSES];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonA < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonA;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt > 0)
			return Sort[cnt - 1];
		else
			return Sort[NrCrossPoints - 1];
	}
	else
	{
		ok = 1;
		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindCrossIndexNrPolygonA(int32 Nr)
{
	int32 cnt;

	cnt = 0;

	while ((cnt < NrCrossPoints) && (CrossPoints[cnt].LineNrPolygonA != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
		return cnt;
	else
		return 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CollectCrossesPolygons2(PolygonRecord * PolygonObject, PolygonRecord * ResultPolygon, int32 mode)
{
	int32 cnt4, cnt3, cnt2, cnt, count, count2, res, NrCrosses, InsertPos, Pos, CountOnSameLine, Found, NrTries,
	      LineCnt2, LineCnt3, LineCnt2a, LineCnt3a, DisabledPos, NextPoint;
	double *x1, *y1, *x2, *y2, *FirstX, *FirstY, x3, y3, x4, y4, MinDist, DivX, DivY, cx, cy, prevx, prevy;
	PointRecord NewPoint;
	int32 Changed = 1;
#ifdef _DEBUG
	char str[MAX_LENGTH_STRING];
#endif

	NrCrosses = 0;
	count = 0;
	Pos = 0;
	prevx = 0.0;
	prevy = 0.0;
	CountOnSameLine = 0;

	CopyPolygonToPolygon(ResultPolygon, WorkPolygon[0]);
	CopyPolygonToPolygon(PolygonObject, WorkPolygon[1]);


	NrTries = 0;

	while ((Changed) && (NrTries < 2))
	{
		Changed = 0;
		NrCrosses = 0;
		count2 = PolygonObject->NrVertices;

		for (cnt3 = 0; cnt3 < count2; cnt3++)
		{
			x3 = PolygonObject->Points[cnt3].x;
			y3 = PolygonObject->Points[cnt3].y;

			if (cnt3 < count2 - 1)
			{
				x4 = PolygonObject->Points[cnt3 + 1].x;
				y4 = PolygonObject->Points[cnt3 + 1].y;
			}
			else
			{
				x4 = PolygonObject->Points[0].x;
				y4 = PolygonObject->Points[0].y;
			}

			count = ResultPolygon->NrVertices;
			FirstX = (double *) &(ResultPolygon->Points);
			FirstY = FirstX + 1;
			x1 = FirstX;
			y1 = FirstY;

			for (cnt2 = 0; cnt2 < count; cnt2++)
			{
				if (cnt2 < count - 1)
				{
					x2 = x1 + 2;
					y2 = y1 + 2;
				}
				else
				{
					x2 = FirstX;
					y2 = FirstY;
				}

				res = LineCrosses(*x1, *y1, *x2, *y2, x3, y3, x4, y4);

				if (res == 1)
				{
					if (NrCrosses < MAX_NR_CROSSES)
					{
						CrossPoints[NrCrosses].LineNrPolygonA = cnt2;
						CrossPoints[NrCrosses].LineNrPolygonB = cnt3;
						CrossPoints[NrCrosses].CrossX = LineCrossX;
						CrossPoints[NrCrosses].CrossY = LineCrossY;
						CrossPoints[NrCrosses].Test = 0;
					}

					NrCrosses++;
				}
				else
				{
					if (res == 0)
					{
						res = LineCrosses(*x1, *y1, *x2, *y2, x3, y3, x4, y4);
						return -1;
					}
				}

				x1 += 2;
				y1 += 2;
			}
		}

		if (NrCrosses > MAX_NR_CROSSES)
			return NrCrosses;

		if ((NrCrosses & 1) == 1)
		{
			res = 1;
#ifdef _DEBUG

			if (0)
			{
				PolygonToObjectLines(PolygonObject, INFO_LAYER3, ResultPolygon, INFO_LAYER4, 0);
				PolygonVerticesToMessage(PolygonObject, ResultPolygon);
			}

#endif

			for (cnt = 0; cnt < NrCrosses; cnt++)
			{
#ifdef _DEBUG
				sprintf(str, "Cross nr %i = %.5f,%.5f\n", cnt, CrossPoints[cnt].CrossX, CrossPoints[cnt].CrossY);
				OutputDebugString(str);
#endif
				LineCnt2 = CrossPoints[cnt].LineNrPolygonA;
				LineCnt2a = LineCnt2;

				if (LineCnt2a < count - 1)
					LineCnt2a++;
				else
					LineCnt2a = 0;

#ifdef _DEBUG
				sprintf(str, "   line 1 %.5f,%.5f to %.5f,%.5f\n", ResultPolygon->Points[LineCnt2].x,
				        ResultPolygon->Points[LineCnt2].y, ResultPolygon->Points[LineCnt2a].x,
				        ResultPolygon->Points[LineCnt2a].y);
				OutputDebugString(str);
				DrawLineRed(PolygonObject->Points[LineCnt2].x, PolygonObject->Points[LineCnt2].y,
					        PolygonObject->Points[LineCnt2a].x, PolygonObject->Points[LineCnt2a].y, BM_DirectToScreen);
#endif

				LineCnt3 = CrossPoints[cnt].LineNrPolygonB;
				LineCnt3a = LineCnt3;

				if (LineCnt3a < count2 - 1)
					LineCnt3a++;
				else
					LineCnt3a = 0;

#ifdef _DEBUG
				sprintf(str, "   line 2 %.5f,%.5f to %.5f,%.5f\n", PolygonObject->Points[LineCnt3].x,
				        PolygonObject->Points[LineCnt3].y, PolygonObject->Points[LineCnt3a].x,
				        PolygonObject->Points[LineCnt3a].y);
				OutputDebugString(str);
				DrawLineGreen(PolygonObject->Points[LineCnt3].x, PolygonObject->Points[LineCnt3].y,
					          PolygonObject->Points[LineCnt3a].x, PolygonObject->Points[LineCnt3a].y, BM_DirectToScreen);

				DrawCircleYellow(CrossPoints[cnt].CrossX, CrossPoints[cnt].CrossY, 1000.0, BM_DirectToScreen);
#endif
			}

			cnt = 0;

			while (cnt < NrCrosses - 1)
			{
				cnt2 = cnt + 1;

				while (cnt2 < NrCrosses)
				{
					if ((InRange6(CrossPoints[cnt].CrossX, CrossPoints[cnt2].CrossX))
					        && (InRange6(CrossPoints[cnt].CrossY, CrossPoints[cnt2].CrossY)))
					{
						res = 1;

						if (cnt2 < NrCrosses - 1)
						{
							memmove(&CrossPoints[cnt2], &CrossPoints[cnt2 + 1],
							        sizeof(CrossPointRecord) * (NrCrosses - cnt2 - 1));
						}

						NrCrosses--;
					}

					cnt2++;
				}

				cnt++;
			}
		}

		if ((NrCrosses & 1) == 1)
			return NrCrosses;

		DisabledPos = 10000000;
		cnt = 0;

		while (cnt < NrCrosses - 1)
		{

// **********************************************************************************************
// **********************************************************************************************

			cnt2 = cnt + 1;
			CountOnSameLine = 0;

			while (cnt2 < NrCrosses)
			{
				if (CrossPoints[cnt].LineNrPolygonA == CrossPoints[cnt2].LineNrPolygonA)
				{
					CountOnSameLine++;
					Pos = cnt2;
				}

				cnt2++;
			}

			if (CountOnSameLine == 1)
			{
				NewPoint.x = (CrossPoints[cnt].CrossX + CrossPoints[Pos].CrossX) / 2;
				NewPoint.y = (CrossPoints[cnt].CrossY + CrossPoints[Pos].CrossY) / 2;
				NextPoint = CrossPoints[cnt].LineNrPolygonA + 1;

//        if (NextPoint==ResultPolygon->NrVertices) NextPoint=0;
				if (NextPoint >= 10000000)
					res = 1;

				InsertVertice(NextPoint, ResultPolygon, (PointsArray *) & NewPoint);

				for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
				{
					if (CrossPoints[cnt3].LineNrPolygonA > CrossPoints[cnt].LineNrPolygonA)
						CrossPoints[cnt3].LineNrPolygonA++;
				}

				Changed = 1;
			}
			else
			{
				if (CountOnSameLine > 1)
				{
					InsertPos = CrossPoints[cnt].LineNrPolygonA;
					cx = ResultPolygon->Points[InsertPos].x;
					cy = ResultPolygon->Points[InsertPos].y;
					CountOnSameLine = 0;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt].CrossX;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt].CrossY;
					DivX = cx - CrossPoints[cnt].CrossX;
					DivY = cy - CrossPoints[cnt].CrossY;
					DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
					DistanceOnSameLine[CountOnSameLine].Pos = cnt;
					DistanceOnSameLine[CountOnSameLine].Test = 0;
					CountOnSameLine++;

					cnt2 = cnt + 1;

					while (cnt2 < NrCrosses)
					{
						if (CrossPoints[cnt].LineNrPolygonA == CrossPoints[cnt2].LineNrPolygonA)
						{
							DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt2].CrossX;
							DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt2].CrossY;
							DivX = cx - CrossPoints[cnt2].CrossX;
							DivY = cy - CrossPoints[cnt2].CrossY;
							DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
							DistanceOnSameLine[CountOnSameLine].Pos = cnt2;
							DistanceOnSameLine[CountOnSameLine].Test = 0;
							CountOnSameLine++;
						}

						cnt2++;
					}

					MinDist = 1000000000.0;
					Found = -1;
// Find first cross closest to start of line
					cnt4 = 0;

					while (cnt4 < CountOnSameLine)
					{
						if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
						{
							Found = cnt4;
							MinDist = DistanceOnSameLine[cnt4].Distance;
						}

						cnt4++;
					}

					if (Found != -1)
					{
						DistanceOnSameLine[Found].Test = 1;
						Pos = DistanceOnSameLine[Found].Pos;
						prevx = CrossPoints[Pos].CrossX;
						prevy = CrossPoints[Pos].CrossY;
						CrossPoints[Pos].LineNrPolygonA = DisabledPos++;
//            CrossPoints[Pos].LineNrPolygonA=10000000;
					}

// Insert point between next crosses and the previous cross
					cnt3 = 0;

					while (cnt3 < CountOnSameLine - 1)
					{
						MinDist = 1000000000.0;
						Found = -1;
						cnt4 = 0;

						while (cnt4 < CountOnSameLine)
						{
							if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
							{
								Found = cnt4;
								MinDist = DistanceOnSameLine[cnt4].Distance;
							}

							cnt4++;
						}

						if (Found != -1)
						{
							DistanceOnSameLine[Found].Test = 1;
							Pos = DistanceOnSameLine[Found].Pos;
							NewPoint.x = (prevx + CrossPoints[Pos].CrossX) / 2;
							NewPoint.y = (prevy + CrossPoints[Pos].CrossY) / 2;
							prevx = CrossPoints[Pos].CrossX;
							prevy = CrossPoints[Pos].CrossY;
							InsertVertice(InsertPos + cnt3 + 1, ResultPolygon, (PointsArray *) & NewPoint);
							CrossPoints[Pos].LineNrPolygonA = DisabledPos++;
							Changed = 1;
						}

						cnt3++;
					}

					for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
					{
						if ((CrossPoints[cnt3].LineNrPolygonA > InsertPos)
						        && (CrossPoints[cnt3].LineNrPolygonA < 10000000))
							CrossPoints[cnt3].LineNrPolygonA += CountOnSameLine - 1;
					}
				}
			}

// **********************************************************************************************
// **********************************************************************************************

			cnt2 = cnt + 1;
			CountOnSameLine = 0;

			while (cnt2 < NrCrosses)
			{
				if (CrossPoints[cnt].LineNrPolygonB == CrossPoints[cnt2].LineNrPolygonB)
				{
					CountOnSameLine++;
					Pos = cnt2;
				}

				cnt2++;
			}

			if (CountOnSameLine == 1)
			{
				NewPoint.x = (CrossPoints[cnt].CrossX + CrossPoints[Pos].CrossX) / 2;
				NewPoint.y = (CrossPoints[cnt].CrossY + CrossPoints[Pos].CrossY) / 2;
				NextPoint = CrossPoints[cnt].LineNrPolygonB + 1;

				if (NextPoint >= 10000000)
					res = 1;

				InsertVertice(NextPoint, PolygonObject, (PointsArray *) & NewPoint);

				for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
				{
					if (CrossPoints[cnt3].LineNrPolygonB > CrossPoints[cnt].LineNrPolygonB)
						CrossPoints[cnt3].LineNrPolygonB++;
				}

				Changed = 1;
			}
			else
			{
				if (CountOnSameLine > 1)
				{
					InsertPos = CrossPoints[cnt].LineNrPolygonB;
					cx = PolygonObject->Points[InsertPos].x;
					cy = PolygonObject->Points[InsertPos].y;
					CountOnSameLine = 0;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt].CrossX;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt].CrossY;
					DivX = cx - CrossPoints[cnt].CrossX;
					DivY = cy - CrossPoints[cnt].CrossY;
					DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
					DistanceOnSameLine[CountOnSameLine].Pos = cnt;
					DistanceOnSameLine[CountOnSameLine].Test = 0;
					CountOnSameLine++;

					cnt2 = cnt + 1;

					while (cnt2 < NrCrosses)
					{
						if (CrossPoints[cnt].LineNrPolygonB == CrossPoints[cnt2].LineNrPolygonB)
						{
							DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt2].CrossX;
							DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt2].CrossY;
							DivX = cx - CrossPoints[cnt2].CrossX;
							DivY = cy - CrossPoints[cnt2].CrossY;
							DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
							DistanceOnSameLine[CountOnSameLine].Pos = cnt2;
							DistanceOnSameLine[CountOnSameLine].Test = 0;
							CountOnSameLine++;
						}

						cnt2++;
					}

					MinDist = 1000000000.0;
					Found = -1;
// Find first cross closest to start of line
					cnt4 = 0;

					while (cnt4 < CountOnSameLine)
					{
						if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
						{
							Found = cnt4;
							MinDist = DistanceOnSameLine[cnt4].Distance;
						}

						cnt4++;
					}

					if (Found != -1)
					{
						DistanceOnSameLine[Found].Test = 1;
						Pos = DistanceOnSameLine[Found].Pos;
						prevx = CrossPoints[Pos].CrossX;
						prevy = CrossPoints[Pos].CrossY;
						CrossPoints[Pos].LineNrPolygonB = DisabledPos++;
//            CrossPoints[Pos].LineNrPolygonB=10000000;
					}

// Insert point between next crosses and the previous cross
					cnt3 = 0;

					while (cnt3 < CountOnSameLine - 1)
					{
						MinDist = 1000000000.0;
						Found = -1;
						cnt4 = 0;

						while (cnt4 < CountOnSameLine)
						{
							if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
							{
								Found = cnt4;
								MinDist = DistanceOnSameLine[cnt4].Distance;
							}

							cnt4++;
						}

						if (Found != -1)
						{
							DistanceOnSameLine[Found].Test = 1;
							Pos = DistanceOnSameLine[Found].Pos;
							NewPoint.x = (prevx + CrossPoints[Pos].CrossX) / 2;
							NewPoint.y = (prevy + CrossPoints[Pos].CrossY) / 2;
							prevx = CrossPoints[Pos].CrossX;
							prevy = CrossPoints[Pos].CrossY;
							InsertVertice(InsertPos + cnt3 + 1, PolygonObject, (PointsArray *) & NewPoint);
							CrossPoints[Pos].LineNrPolygonB = DisabledPos++;
							Changed = 1;
						}

						cnt3++;
					}

					for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
					{
						if ((CrossPoints[cnt3].LineNrPolygonB > InsertPos)
						        && (CrossPoints[cnt3].LineNrPolygonB < 10000000))
							CrossPoints[cnt3].LineNrPolygonB += CountOnSameLine - 1;
					}

//          DrawTestPolygon2(PolygonObject,0);
					if (ObjectNr == 37)
					{
						cnt3 = 0;
//            return 6;
					}
				}
			}

			cnt++;
		}

		NrTries++;

		if (CountOnSameLine > 2)
		{
//      DrawTestPolygon(PolygonObject,0);
//      return NrCrosses;
//      return 6;
		}

		if (mode == 1)
			return NrCrosses;
	}

	for (cnt = 0; cnt < NrCrosses; cnt++)
	{
		if ((CrossPoints[cnt].LineNrPolygonA > 10000) || (CrossPoints[cnt].LineNrPolygonA < -10000)
		        || (CrossPoints[cnt].LineNrPolygonB > 10000) || (CrossPoints[cnt].LineNrPolygonB < -10000))
		{
#ifdef _DEBUG
			DrawTestPolygon(WorkPolygon[0], 2);
			DrawTestPolygon(WorkPolygon[1], 1);
#endif
//      DrawTestPolygon3(PolygonObject,0);
//      DrawTestPolygon3(ResultPolygon,1);
			res = 1;
			return 100;
		}
	}

	return NrCrosses;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetPolygonInsertPointsForAppendDelete(PolygonRecord * CopyPolygon, PolygonRecord * AppendPolygon, int32 mode)
{
	int32 NrVerticesCopyPolygon, NrVerticesAppendPolygon, NextCrossNr, CurrentPos, cnt, NextNr, LineNrPolygonA,
	      LineNrPolygonB, PolygonCount, StartNr, TotalPolygons, NextPolygonALineNr, NextPolygonBLineNr, res, count;
	double CheckX, CheckY;

//          PointRecord CrossPoints[MAX_NR_CROSSES];
//          int32 LineNrPolygon[MAX_NR_CROSSES],Count[MAX_NR_CROSSES],Mode[MAX_NR_CROSSES];
//        } VerticeCopy2Record;

	PolygonCount = 0;
	CurrentPos = 0;
	StartNr = 0;
	TotalPolygons = 1;
	NrVerticesCopyPolygon = CopyPolygon->NrVertices;
	NrVerticesAppendPolygon = AppendPolygon->NrVertices;

	while (PolygonCount < NrCrossPoints)
	{

		VerticeCopy2.Mode[PolygonCount] = TotalPolygons;
		CrossPoints[CurrentPos].Test = 1;
		VerticeCopy2.CrossPoints[PolygonCount].x = CrossPoints[CurrentPos].CrossX;
		VerticeCopy2.CrossPoints[PolygonCount].y = CrossPoints[CurrentPos].CrossY;
		LineNrPolygonB = CrossPoints[CurrentPos].LineNrPolygonB;
		NextPolygonBLineNr = (LineNrPolygonB + 1) % NrVerticesAppendPolygon;
		CheckX = AppendPolygon->Points[NextPolygonBLineNr].x;
		CheckY = AppendPolygon->Points[NextPolygonBLineNr].y;
		res = PointInPolygon(CopyPolygon, CheckX, CheckY);

		if ((res & 1) == mode)
		{	// Point PolygonBLineNr+1 outside PolygonA (mode = 0) Append
			// Point PolygonBLineNr+1 inside  PolygonA (mode = 1) Delete
			NextNr = FindNextNrPolygonB(LineNrPolygonB);
			NextCrossNr = FindCrossIndexNrPolygonB(NextNr);

			if ((count = NextNr - LineNrPolygonB) < 0)
				count += NrVerticesAppendPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = NextPolygonBLineNr;
			VerticeCopy2.Count[PolygonCount] = count;
		}
		else
		{	// Point PolygonBLineNr+1 inside  PolygonA    (mode = 0) Append
			// Point PolygonBLineNr+1 outside PolygonA    (mode = 1) Delete
			NextNr = FindPreviousNrPolygonB(LineNrPolygonB);
			NextCrossNr = FindCrossIndexNrPolygonB(NextNr);

			if ((count = LineNrPolygonB - NextNr) < 0)
				count += NrVerticesAppendPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = LineNrPolygonB;
			VerticeCopy2.Count[PolygonCount] = -count;
		}

// *********************************************************************************************
// *********************************************************************************************

		PolygonCount++;
		CurrentPos = NextCrossNr;
		CrossPoints[CurrentPos].Test = 1;
		VerticeCopy2.Mode[PolygonCount] = TotalPolygons;
		VerticeCopy2.CrossPoints[PolygonCount].x = CrossPoints[CurrentPos].CrossX;
		VerticeCopy2.CrossPoints[PolygonCount].y = CrossPoints[CurrentPos].CrossY;
		LineNrPolygonA = CrossPoints[CurrentPos].LineNrPolygonA;
		NextPolygonALineNr = (LineNrPolygonA + 1) % NrVerticesCopyPolygon;
		CheckX = CopyPolygon->Points[NextPolygonALineNr].x;
		CheckY = CopyPolygon->Points[NextPolygonALineNr].y;
		res = PointInPolygon(AppendPolygon, CheckX, CheckY);

		if ((res & 1) == 0)
		{	// Point PolygonALineNr+1 outside PolygonB
			NextNr = FindNextNrPolygonA(LineNrPolygonA);
			NextCrossNr = FindCrossIndexNrPolygonA(NextNr);

			if ((count = NextNr - LineNrPolygonA) < 0)
				count += NrVerticesCopyPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = NextPolygonALineNr;
			VerticeCopy2.Count[PolygonCount] = count;
		}
		else
		{	// Point PolygonALineNr+1 inside  PolygonB
			NextNr = FindPreviousNrPolygonA(LineNrPolygonA);
			NextCrossNr = FindCrossIndexNrPolygonA(NextNr);

			if ((count = LineNrPolygonA - NextNr) < 0)
				count += NrVerticesCopyPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = LineNrPolygonA;
			VerticeCopy2.Count[PolygonCount] = -count;
		}

		PolygonCount++;


		if (StartNr == NextCrossNr)
		{
			if (PolygonCount < NrCrossPoints)
			{
// Find next unused cross nr
				TotalPolygons++;
				cnt = 0;

				while ((cnt < NrCrossPoints) && (CrossPoints[cnt].Test == 1))
					cnt++;

				if (cnt < NrCrossPoints)
				{
					StartNr = cnt;
					CurrentPos = cnt;
				}
			}
		}
		else
			CurrentPos = NextCrossNr;
	}

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		if ((VerticeCopy2.Count[cnt] > 10000) || (VerticeCopy2.Count[cnt] < -10000))
			res = 1;
	}

	return TotalPolygons;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void IncludePolygonInPolygon2(PolygonRecord * StartPolygon, PolygonRecord * AppendDeletePolygon)
{
	int32 NrVerticesStartPolygon, NrVerticesAppendDeletePolygon, ResultP, NextCrossNr, WorkPolygonNr, cnt, res, count;
	PointRecord *StartPolygonPoints, *AppendDeletePolygonPoints, *ResultPolygonPoint, *CrossPoint;

	NrVerticesStartPolygon = StartPolygon->NrVertices;
	NrVerticesAppendDeletePolygon = AppendDeletePolygon->NrVertices;

	if ((NrVerticesStartPolygon == 0) || (NrVerticesAppendDeletePolygon == 0))
		res = 1;

	ResultP = 0;
	WorkPolygonNr = 0;
	StartPolygonPoints = (PointRecord *) & (StartPolygon->Points[0]);
	AppendDeletePolygonPoints = (PointRecord *) & (AppendDeletePolygon->Points[0]);

// ***********************************************************************************************
// ***********************************************************************************************

	NextCrossNr = 0;

	for (cnt = 0; cnt < 4; cnt++)
		WorkPolygon[cnt]->NrVertices = 0;

	ResultPolygon = (PolygonRecord *) WorkPolygon[WorkPolygonNr];
	ResultPolygonPoint = (PointRecord *) & (WorkPolygon[WorkPolygonNr]->Points[0]);

	for (cnt = 0; cnt < (NrCrossPoints / 2); cnt++)
	{

// Insert first cross point

		CrossPoint = (PointRecord *) & (VerticeCopy2.CrossPoints[NextCrossNr]);
		AppendVertices(0, 1, 1, CrossPoint, ResultPolygonPoint, 0);
		ResultP++;
		ResultPolygonPoint++;

		// Insert points polygonB (Append Delete)

		count = VerticeCopy2.Count[NextCrossNr];

		if (count > 0)
		{
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesAppendDeletePolygon,
			               AppendDeletePolygonPoints, ResultPolygonPoint, 0);
		}
		else
		{
			count = -count;
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesAppendDeletePolygon,
			               AppendDeletePolygonPoints, ResultPolygonPoint, 1);
		}

		ResultPolygonPoint += count;
		ResultP += count;

		// ***********************************************************************************************
		// ***********************************************************************************************

		// Insert second cross point

		NextCrossNr++;
		CrossPoint = (PointRecord *) & (VerticeCopy2.CrossPoints[NextCrossNr]);
		AppendVertices(0, 1, 1, CrossPoint, ResultPolygonPoint, 0);
		ResultP++;
		ResultPolygonPoint++;

		// Insert points polygonA ()

		count = VerticeCopy2.Count[NextCrossNr];

		if (count > 0)
		{
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesStartPolygon, StartPolygonPoints,
			               ResultPolygonPoint, 0);
		}
		else
		{
			count = -count;
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesStartPolygon, StartPolygonPoints,
			               ResultPolygonPoint, 1);
		}

		ResultPolygonPoint += count;
		ResultP += count;
		ResultPolygon->NrVertices = ResultP;
		NextCrossNr++;

		// ***********************************************************************************************
		// ***********************************************************************************************

		if ((NextCrossNr < NrCrossPoints) && (VerticeCopy2.Mode[NextCrossNr - 1] != VerticeCopy2.Mode[NextCrossNr]))
		{
			WorkPolygonNr++;
			ResultP = 0;
			ResultPolygon = (PolygonRecord *) WorkPolygon[WorkPolygonNr];
			ResultPolygonPoint = (PointRecord *) & (WorkPolygon[WorkPolygonNr]->Points[0]);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNewWorkPolygon(PolygonRecord * NewPolygonObject, int32 NrPolygons, int32 mode)
{
	int32 cnt, MaxNr, MaxCount, res;
	double Area, MaxArea;
	int32 FirstPolygonCheckOk;

	MaxCount = 0;
	MaxNr = 0;
	MaxArea = 0.0;

	for (cnt = 0; cnt < NrPolygons; cnt++)
	{
		SetMinMaxPolygon(WorkPolygon[cnt], 0);
		Area = (WorkPolygon[cnt]->maxx - WorkPolygon[cnt]->minx) * (WorkPolygon[cnt]->maxy - WorkPolygon[cnt]->miny);

		if (Area > MaxArea)
		{
			MaxArea = Area;
			MaxNr = cnt;
		}

		/*
		    if (WorkPolygon[cnt]->NrVertices>MaxCount) {
		      MaxCount=WorkPolygon[cnt]->NrVertices;
		      MaxNr=cnt;
		    }
		*/
	}

	if (mode == 0)
	{	// Check smaller polygon is inside a big polygon
		FirstPolygonCheckOk = 1;

		for (cnt = 0; cnt < NrPolygons; cnt++)
		{
			if (cnt != MaxNr)
			{
				if (CheckPolygonCompleetlyInsidePolygon(WorkPolygon[cnt], WorkPolygon[MaxNr]) == 0)
					FirstPolygonCheckOk = 0;
			}
		}

		if (FirstPolygonCheckOk)
		{	// The polygon with the max NrVertices encloses the other polygons
			CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);

			if (NrPolygons == 1)
				return 0;
			else
				return 1;
		}
		else
		{	// Find the polygon which encloses the others
			res = 1;

			switch (NrPolygons)
			{
			case 1:
				break;

			case 2:
				if (CheckPolygonCompleetlyInsidePolygon(WorkPolygon[MaxNr], WorkPolygon[MaxNr ^ 1]) == 0)
				{
					CopyPolygonToPolygon(WorkPolygon[MaxNr ^ 1], NewPolygonObject);
					return 1;
				}
				else
				{
					CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);
					return 1;
				}

				break;

			default:
				CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);
				return 1;
			}
		}
	}
	else
	{
		CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);

		if (NrPolygons == 1)
			return 0;
		else
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MergePolygon(PolygonRecord * PolygonObject, int32 Mode)
{
	int32 cnt2, count, NrCrosses, TotalCount, res, NrPolygons, ok, VerticesCount, MemSizeAreaFill, PolygonResult, Mem2,
	      SubPolygonChanged, PolygonMemSize;
	PolygonRecord *SubPolygon, *SurroundPolygon, *AreaFillPolygon;
//  AreaFillRecord *NewAreaFill;
	int32 Changed, OverlappedPolygon;
	uint8 *PolygonPos, *AreaFillPolygonPos;

// Mode = 0  -> Normal
// Mode = 1  -> Merge with a thermal relief cut out

//  Result2Polygon=ResultPolygon;   // Due to compiler error 1001

// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion

	SetMinMaxPolygon(PolygonObject, 0);
	OverlappedPolygon = 0;
	VerticesCount = PolygonObject->NrVertices + 256;

	if (VerticesCount >= MaxNrVerticesPolygon)
	{
		if (AllocateMemPolygons(VerticesCount) != 0)
			return -100;
	}

	CopyPolygonToPolygon(PolygonObject, NewPolygon);
	NewPolygon->PolygonType = Mode;
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if (cnt2 == 0)
		{
			if ((PolygonObject->maxx + 100 < SubPolygon->minx) || (PolygonObject->minx - 100 > SubPolygon->maxx)
			        || (PolygonObject->maxy + 100 < SubPolygon->miny) || (PolygonObject->miny - 100 > SubPolygon->maxy))
			{
				return 5;		// Cut out polygon does not overlap the main polygon
			}
		}
		else
		{
			if ((SubPolygon->PolygonType & 8) == 0)
			{
				if ((PolygonObject->maxx + 100 < SubPolygon->minx) || (PolygonObject->minx - 100 > SubPolygon->maxx)
				        || (PolygonObject->maxy + 100 < SubPolygon->miny) || (PolygonObject->miny - 100 > SubPolygon->maxy))
					OverlappedPolygon = 1;
			}
		}

		SubPolygon->Special.Test = 0;
		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	/*
	  if (!OverlappedPolygon) {
	// Cut out polygon does not overlap any other cut out polygon -> just append polygon
	    MemSizeAreaFill=TempAreaFill->MemSize+MemSizePolygon(PolygonObject);
	    if (MemSizeAreaFill>=MaxAreaFillMemoryTemp) {
	      if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill)!=0) {
	        return -105;
	      }
	      NewAreaFill=(AreaFillRecord *)AreaFillMemTemp;
	      TempAreaFill=(AreaFillRecord *)AreaFillMemTemp2;
	    }
	    count=TempAreaFill->MemSize;
	    memmove(NewAreaFill,TempAreaFill,count);
	    SubPolygon=(PolygonRecord *)((uint8 *)NewAreaFill+count);
	    CopyPolygonToPolygon(PolygonObject,SubPolygon);
	    SetMinMaxPolygon(SubPolygon,0);
	    SubPolygon->PolygonType&=4+1;
	    NewAreaFill->MemSize+=MemSizePolygon(SubPolygon);
	    NewAreaFill->NrPolygons++;
	    return 2;
	  }
	*/
	Changed = 1;
	TotalCount = 0;
	SubPolygonChanged = 0;


// *********************************************************************************
// If PolygonObject is inside an other deletion polygon
// of the areafill then exit
//

	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	Mem2 = sizeof(AreaFillRecord);
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if (cnt2 > 0)
		{
			if ((SubPolygon->PolygonType & 8) == 0)
			{
				if (!
				        ((PolygonObject->maxx + 100 < SubPolygon->minx) || (PolygonObject->minx - 100 > SubPolygon->maxx)
				         || (PolygonObject->maxy + 100 < SubPolygon->miny)
				         || (PolygonObject->miny - 100 > SubPolygon->maxy)))
				{
					if (CheckPolygonCompleetlyInsidePolygon(PolygonObject, SubPolygon) == 1)
					{
						//        if (ObjectNr==523) {
						//          DrawTestPolygon(PolygonObject,0);  // red
						//          DrawTestPolygon(SubPolygon,1);  // green
						//          while (!KeyPressed()) CheckInputMessages(0);
						//          ReadKeyFunction();
						//        }
						return 3;
					}
				}
			}
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		Mem2 += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	ok = 1;

// *********************************************************************************
// If a deletion polygon of the areafill is inside PolygonObject
// then deletion polygon is deleted
//
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if ((cnt2 > 0) && ((SubPolygon->PolygonType & 8) == 0))
		{
			if (!
			        ((PolygonObject->maxx + 100 < SubPolygon->minx) || (PolygonObject->minx - 100 > SubPolygon->maxx)
			         || (PolygonObject->maxy + 100 < SubPolygon->miny) || (PolygonObject->miny - 100 > SubPolygon->maxy)))
			{
				if (CheckPolygonCompleetlyInsidePolygon(SubPolygon, PolygonObject) == 1)
				{
//        if (ObjectNr==523) {
//          DrawTestPolygon(PolygonObject,0);  // red
//          DrawTestPolygon(SubPolygon,1);  // green
//          while (!KeyPressed()) CheckInputMessages(0);
//          ReadKeyFunction();
//        }
					SubPolygon->Special.Test = 1;

					if (((Mode & 5) == 5) && ((SubPolygon->PolygonType & 5) == 5))
						SubPolygon->Special.Test = 2;

					SubPolygonChanged = 1;
				}
			}
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	ok = 1;

// *********************************************************************************
// Find the resulting Deletion polygon out of merging the polygon8 object
// with the other Deletion polygon from the areafill
// The resulting Deletion polygon will be NewPolygon
// *********************************************************************************

	while ((Changed) && (TotalCount < 10000))
	{
		Changed = 0;
		SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) SubPolygon;

//    if (ObjectNr==10) {
//      res=1;
//      DrawTestPolygon(NewPolygon,0);  // red
//    }
		for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
		{
			count = SubPolygon->NrVertices;

			if ((cnt2 > 0) && ((SubPolygon->PolygonType & 8) == 0) && (SubPolygon->Special.Test == 0))
			{
				/*
				        if ((ObjectNr==120)
				           &&
				           (cnt2==22)) {
				          res=1;
				          DrawTestPolygon(SubPolygon,1);  // green
				          DrawTestPolygon(NewPolygon,0);  // red
				          while (!KeyPressed()) CheckInputMessages(0);
				          ReadKeyFunction();
				          InvalidateRect(PCBWindow,NULL,0);
				          PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
				          CheckInputMessages(0);
				        }
				*/
				VerticesCount = SubPolygon->NrVertices + 256;

				if (VerticesCount >= MaxNrVerticesPolygon)
				{
					if (AllocateMemPolygons(VerticesCount) != 0)
						return -100;
				}

				CopyPolygonToPolygon(SubPolygon, ExtraPolygon);

				if ((ExtraPolygon->maxx + 100 < NewPolygon->minx) || (ExtraPolygon->minx - 100 > NewPolygon->maxx)
				        || (ExtraPolygon->maxy + 100 < NewPolygon->miny) || (ExtraPolygon->miny - 100 > NewPolygon->maxy))
					NrCrosses = 0;
				else
					NrCrosses = CollectCrossesPolygons2(ExtraPolygon, NewPolygon, 0);

#ifdef _DEBUG

				if (NrCrosses > 8)
					ok = 1;

#endif

				if ((NrCrosses > 0) && (NrCrosses <= MAX_NR_CROSSES) && ((NrCrosses & 1) == 0))
				{
					NrCrossPoints = NrCrosses;

// *********************************************************************************
//  Get Insert points for appending polygon ExtraPolygon to NewPolygon
// *********************************************************************************
					NrPolygons = GetPolygonInsertPointsForAppendDelete(NewPolygon, ExtraPolygon, 0);

// NrPolygons is the resulting nr of polygons after appending

// *********************************************************************************
// ExtraPolygon is merged with the old NewPolygon into NewPolygon and will be deleted
// *********************************************************************************
					VerticesCount = NewPolygon->NrVertices + ExtraPolygon->NrVertices + 256;

					if (VerticesCount >= MaxNrVerticesPolygon)
					{
						if (AllocateMemPolygons(VerticesCount) != 0)
							return -101;
					}

					IncludePolygonInPolygon2(NewPolygon, ExtraPolygon);
// *********************************************************************************
// If there are more resulting polygon (NrPolygon>1) the largest polygon
// will be put in NewPolygon
					GetNewWorkPolygon(NewPolygon, NrPolygons, 0);

					if (Mode == 1)
						ok = 1;

					if ((SubPolygon->PolygonType & 5) == 1)
					{
// bit 0 = 0 polygon deletion because of thermal relief
						NewPolygon->PolygonType &= ~4;	// -> User deletion -> thermal relief
					}

//          if (ObjectNr==10) {
//            DrawTestPolygon(NewPolygon,2);
//            while (!KeyPressed()) CheckInputMessages(0);
//            ReadKeyFunction();
//          }
					SubPolygon->Special.Test = 1;

					if (((Mode & 5) == 5) && ((SubPolygon->PolygonType & 5) == 5))
						SubPolygon->Special.Test = 2;

					Changed = 1;
					SubPolygonChanged = 2;
// ********************************************************************************************
// ********************************************************************************************
				}
				else
				{
					if (NrCrosses > MAX_NR_CROSSES)
					{
						PolygonMemSize = MemSizePolygon(ExtraPolygon);
						AllocateSpecialMem(MEM_PROBLEM_POLYGON1, PolygonMemSize, (void **) &ProblemPolygon1);
						PolygonMemSize = MemSizePolygon(NewPolygon);
						AllocateSpecialMem(MEM_PROBLEM_POLYGON2, PolygonMemSize, (void **) &ProblemPolygon2);
						CopyPolygonToPolygon(ExtraPolygon, ProblemPolygon1);
						CopyPolygonToPolygon(NewPolygon, ProblemPolygon2);
					}

					if (NrCrosses != 0)
					{
						if (NrCrosses == -1)
							return -10;

						if (NrCrosses == -2)
							return -11;

						return -20;
					}
				}

// ********************************************************************************************
// ********************************************************************************************
			}

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}

		TotalCount++;
	}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
//
// Merge the surround polygon with the already merged deletion polygon NewPolygon
//
// ********************************************************************************************

	SurroundPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	VerticesCount = SurroundPolygon->NrVertices + 256;

	if (VerticesCount >= MaxNrVerticesPolygon)
	{
		if (AllocateMemPolygons(VerticesCount) != 0)
			return -102;
	}

	CopyPolygonToPolygon(SurroundPolygon, ExtraPolygon);

	/*
	  if (ObjectNr==119) {
	    DrawTestPolygon(SurroundPolygon,0);  // red
	    DrawTestPolygon(NewPolygon,1);  // green
	    while (!KeyPressed()) CheckInputMessages(0);
	    ReadKeyFunction();
	    return -3;
	    res=1;
	  }
	*/
	if ((ExtraPolygon->maxx + 100 < NewPolygon->minx) || (ExtraPolygon->minx - 100 > NewPolygon->maxx)
	        || (ExtraPolygon->maxy + 100 < NewPolygon->miny) || (ExtraPolygon->miny - 100 > NewPolygon->maxy))
		NrCrosses = 0;
	else
		NrCrosses = CollectCrossesPolygons2(NewPolygon, ExtraPolygon, 0);

#ifdef _DEBUG

	if (NrCrosses > 8)
		ok = 1;

#endif

	if ((NrCrosses > 0) && (NrCrosses <= MAX_NR_CROSSES) && ((NrCrosses & 1) == 0))
	{
		NrCrossPoints = NrCrosses;
// *********************************************************************************
//  Get Delete points for deleting polygon NewPolygon from ExtraPolygon
// *********************************************************************************
		NrPolygons = GetPolygonInsertPointsForAppendDelete(ExtraPolygon, NewPolygon, 1);

// NrPolygons is the resulting nr of polygons after appending

// *********************************************************************************
// NewPolygon is merged with the old ExtraPolygon into ExtraPolygon (Surround polygon)
// *********************************************************************************
		VerticesCount = NewPolygon->NrVertices + ExtraPolygon->NrVertices + 256;

		if (VerticesCount >= MaxNrVerticesPolygon)
		{
			if (AllocateMemPolygons(VerticesCount) != 0)
				return -103;
		}

#ifdef _DEBUG

		if (0)
		{
			PolygonToObjectLines(ExtraPolygon, INFO_LAYER3, NewPolygon, INFO_LAYER4, 0);

			if (0)
				PolygonVerticesToMessage(PolygonObject, ResultPolygon);
		}

#endif
		IncludePolygonInPolygon2(ExtraPolygon, NewPolygon);
#ifdef _DEBUG

		if (0)
		{
			PolygonToObjectLines(ExtraPolygon, INFO_LAYER2, 0, 0, 0);

			if (0)
				PolygonVerticesToMessage(PolygonObject, ResultPolygon);
		}

#endif

// *********************************************************************************
// If there are more resulting polygons (NrPolygon>1) the largest polygon
// will be put in ResultPolygon
		PolygonResult = GetNewWorkPolygon(ResultPolygon, NrPolygons, 1);
#ifdef _DEBUG

		if (0)
		{
			PolygonToObjectLines(ResultPolygon, INFO_LAYER2, 0, 0, 0);

			if (0)
				PolygonVerticesToMessage(PolygonObject, ResultPolygon);
		}

#endif
// ********************************************************************************************
// PolygonResult = 0 when one result polygon exist
//           and = 1 when two result polygon exists
// ********************************************************************************************

//    if (ObjectNr==10) {
//      DrawTestPolygon(PolygonObject,0);  // red
//      DrawTestPolygon(SurroundPolygon,1);  // green
//      DrawTestPolygon(ResultPolygon,2);  // yellow
//      while (!KeyPressed()) CheckInputMessages(0);
//      ReadKeyFunction();
//    }

// ********************************************************************************************
// ********************************************************************************************
//
// Start building the new areafill out of not deleted SubPolygons and
// the new SurroundPolygon ResultPolygon
//
// ********************************************************************************************

		MemSizeAreaFill = sizeof(AreaFillRecord);
		MemSizeAreaFill += MemSizePolygon(ResultPolygon);
		SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) SubPolygon;

		for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
		{
			if ((cnt2 > 0) && (SubPolygon->Special.Test == 0))
				MemSizeAreaFill += MemSizePolygon(SubPolygon);

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}

		MemSizeAreaFill += MemSizePolygon(ResultPolygon) + 3172;

		if (MemSizeAreaFill >= MaxAreaFillMemoryTemp)
		{
			if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill) != 0)
				return -104;

			NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
			TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
		}

// ********************************************************************************************
// ********************************************************************************************

		memmove(NewAreaFill, TempAreaFill, sizeof(AreaFillRecord));
		NewAreaFill->MemSize = sizeof(AreaFillRecord);

		AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
		AreaFillPolygonPos = (uint8 *) AreaFillPolygon;
// Copy ResultPolygon to NewAreaFill

		NewAreaFill->NrPolygons = 1;
		CopyPolygonToPolygon(ResultPolygon, AreaFillPolygon);
		SetMinMaxPolygon(AreaFillPolygon, 0);

		NewAreaFill->MemSize += MemSizePolygon(ResultPolygon);
		AreaFillPolygonPos += MemSizePolygon(ResultPolygon);
		AreaFillPolygon = (PolygonRecord *) AreaFillPolygonPos;


// ********************************************************************************************
// ********************************************************************************************


// Copy not deleted SubPolygons to NewAreaFill

		SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) SubPolygon;

		for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
		{
			if ((cnt2 > 0) && (SubPolygon->Special.Test != 1))
			{
				res = 0;

				if (SubPolygon->Special.Test == 2)
				{
					if ((SubPolygon->PolygonType & 5) == 5)
					{
						SubPolygon->PolygonType &= ~5;
						SubPolygon->PolygonType |= 8;
					}
				}

#ifdef _DEBUG

				if ((SubPolygon->PolygonType & 8) == 8)
					ok = 1;

#endif

				if (PolygonResult != 0)
				{
//          Result2Polygon=ResultPolygon;
					res = CheckPolygonInsidePolygon(SubPolygon, (PolygonRecord *) ResultPolygon);

//          InvalidateRect(PCBWindow,NULL,0);
//          PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
//          CheckInputMessages(0);
//          DrawTestPolygon(SubPolygon,0);  // red
//          DrawTestPolygon(ResultPolygon,1);  // green
//          while (!KeyPressed()) CheckInputMessages(0);
//          ReadKeyFunction();

				}

				if ((PolygonResult == 0) || (res == 1))
				{
					NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
					NewAreaFill->NrPolygons++;

					if (ObjectNr == 431)
						ok = 1;

					CopyPolygonToPolygon(SubPolygon, AreaFillPolygon);
					AreaFillPolygon->PolygonType &= 8 + 4 + 1;
					AreaFillPolygonPos += MemSizePolygon(SubPolygon);
					AreaFillPolygon = (PolygonRecord *) AreaFillPolygonPos;
				}
			}

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}

		return 1;
	}
	else
	{
#ifdef _DEBUG

		if (NrCrosses > 8)
			ok = 1;

#endif

		if (NrCrosses != 0)
		{
			ok = 1;

			if (NrCrosses > MAX_NR_CROSSES)
			{
				PolygonMemSize = MemSizePolygon(ExtraPolygon);
				AllocateSpecialMem(MEM_PROBLEM_POLYGON1, PolygonMemSize, (void **) &ProblemPolygon1);
				PolygonMemSize = MemSizePolygon(NewPolygon);
				AllocateSpecialMem(MEM_PROBLEM_POLYGON2, PolygonMemSize, (void **) &ProblemPolygon2);
				CopyPolygonToPolygon(ExtraPolygon, ProblemPolygon1);
				CopyPolygonToPolygon(NewPolygon, ProblemPolygon2);
			}

//      NrCrosses=CollectCrossesPolygons2(NewPolygon,ExtraPolygon,0);
			if (NrCrosses == -1)
				return -12;

			if (NrCrosses == -2)
				return -13;

			return -60;
		}
	}

// ********************************************************************************************
// ********************************************************************************************
//
// Start building the new areafill out of not deleted SubPolygons and
// the merged Deletion polygon NewPolygon and the not changed
// SurroundPolygon ResultPolygon
//
// ********************************************************************************************

//  CheckX=NewPolygon->Points[0].x;
//  CheckY=NewPolygon->Points[0].y;
//  res=PointInPolyLine(SurroundPolygon,CheckX,CheckY);

	SetMinMaxPolygon(NewPolygon, 0);
	SetMinMaxPolygon(SurroundPolygon, 0);
#ifdef _DEBUG

	if (SurroundPolygon->NrVertices < 3)
	{
		ok = 1;
		return -60;
	}

#endif

	if (CheckPolygonInsidePolygon(NewPolygon, SurroundPolygon) == 1)
	{	// NewPolygon in SurroundPolygon

// ********************************************************************************************
// ********************************************************************************************
		MemSizeAreaFill = sizeof(AreaFillRecord);
		MemSizeAreaFill += MemSizePolygon(NewPolygon);
		MemSizeAreaFill += MemSizePolygon(SurroundPolygon);
		SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) SubPolygon;

		for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
		{
			if ((cnt2 > 0) && (SubPolygon->Special.Test != 1))
				MemSizeAreaFill += MemSizePolygon(SubPolygon);

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}

		MemSizeAreaFill += MemSizePolygon(NewPolygon) + 3172;

		if (MemSizeAreaFill >= MaxAreaFillMemoryTemp)
		{
			if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill) != 0)
				return -105;

			NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
			TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
		}

// ********************************************************************************************
// ********************************************************************************************


		if (SubPolygonChanged)
		{	// Copy the SubPolygons (Test=0) to the new AreaFill

			memmove(NewAreaFill, TempAreaFill, sizeof(AreaFillRecord));
			NewAreaFill->MemSize = sizeof(AreaFillRecord);
			AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
			AreaFillPolygonPos = (uint8 *) AreaFillPolygon;
			NewAreaFill->NrPolygons = 0;

// Copy not deleted SubPolygons to NewAreaFill
			SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
			PolygonPos = (uint8 *) SubPolygon;

			for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
			{
#ifdef _DEBUG

				if ((SubPolygon->PolygonType & 8) == 8)
					ok = 1;

#endif

				if (SubPolygon->Special.Test != 1)
				{
					if (SubPolygon->Special.Test == 2)
					{
						if ((SubPolygon->PolygonType & 5) == 5)
						{
							SubPolygon->PolygonType &= ~5;
							SubPolygon->PolygonType |= 8;
						}
					}

					NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
					NewAreaFill->NrPolygons++;
					CopyPolygonToPolygon(SubPolygon, AreaFillPolygon);
					AreaFillPolygon->PolygonType &= 8 + 4 + 1;
					AreaFillPolygonPos += MemSizePolygon(SubPolygon);
					AreaFillPolygon = (PolygonRecord *) AreaFillPolygonPos;
				}

				PolygonPos += MemSizePolygon(SubPolygon);
				SubPolygon = (PolygonRecord *) PolygonPos;
			}

			if (((Mode & 5) == 5) && (SubPolygonChanged == 2))
				NewPolygon->PolygonType &= ~5;

			CopyPolygonToPolygon(NewPolygon, AreaFillPolygon);
			SetMinMaxPolygon(AreaFillPolygon, 0);
			NewAreaFill->MemSize += MemSizePolygon(NewPolygon);
			NewAreaFill->NrPolygons++;

			if (SubPolygonChanged == 2)
				return 1;
			else
				return 6;
		}
		else
		{	// Just append NewPolygon
			count = TempAreaFill->MemSize;
			memmove(NewAreaFill, TempAreaFill, count);
			SubPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + count);
			CopyPolygonToPolygon(NewPolygon, SubPolygon);
			SetMinMaxPolygon(SubPolygon, 0);
			SubPolygon->PolygonType &= 4 + 1;
			NewAreaFill->MemSize += MemSizePolygon(NewPolygon);
			NewAreaFill->NrPolygons++;
//        SubPolygon=(PolygonRecord *)((uint8 *)AreaFill+AreaFill->MemSize);
//        CopyPolygonToPolygon(NewPolygon,SubPolygon);
//        AreaFill->MemSize+=MemSizePolygon(NewPolygon);
//        AreaFill->NrPolygons++;
			return 4;
		}
	}
	else
	{
//    DrawTestPolygon(SurroundPolygon,0);  // red
//    DrawTestPolygon(NewPolygon,1);  // green
//    DrawTestPolygon(PolygonObject,2);  // green
//    while (!KeyPressed()) CheckInputMessages(0);
//    ReadKeyFunction();
		ok = 1;
	}

	return 2;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RemoveObjectsWithSameNetNr(int32 NetNr, int32 StartPinObjects, int32 Found, double Clearance)
{
	int32 cnt, cnt2;
	PolygonRecord *PolygonObject, *PolygonObject2;
	ObjectRecord *Object, *Object2;
	uint8 PolygonBuf2[10240];
	uint8 PolygonBuf3[10240];

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	PolygonObject2 = (PolygonRecord *) & PolygonBuf3;

	for (cnt = StartPinObjects; cnt < Found; cnt++)
	{
		Object = &((*Objects4)[cnt]);

		if ((Object->NetNr == NetNr) && ((Object->Info2 & 1) == 0))
		{
			switch (Object->ObjectType)
			{
//          case PIN_PUT_THROUGH_ROUND:
			case PIN_SMD_ROUND:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:

//          case PIN_PUT_THROUGH_SQUARE:
			case PIN_SMD_RECT:
				MakePolygonFromObject(Object, PolygonObject, Clearance + 10000.0, 0.0, 1, 0);

				for (cnt2 = cnt + 1; cnt2 < Found; cnt2++)
				{
					Object2 = &((*Objects4)[cnt2]);

					if ((Object2->NetNr == NetNr) && ((Object2->Info2 & 1) == 0))
					{
						switch (Object2->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:
						case PIN_PUT_THROUGH_SQUARE:
						case DRILL:
							MakePolygonFromObject(Object2, PolygonObject2, Clearance, 0.0, 1, 0);

							if (CheckPolygonCompleetlyInsidePolygon(PolygonObject2, PolygonObject) == 1)
								Object2->Info2 |= 1;

							break;

						case PIN_SMD_ROUND:
						case PIN_LINE_HOR:
						case PIN_LINE_VER:
						case PIN_LINE_DIAG1:
						case PIN_LINE_DIAG2:
						case PIN_SMD_RECT:
							if (Object->Layer == Object2->Layer)
							{
								MakePolygonFromObject(Object2, PolygonObject2, Clearance, 0.0, 1, 0);

								if (CheckPolygonCompleetlyInsidePolygon(PolygonObject2, PolygonObject) == 1)
									Object2->Info2 |= 1;
							}

							break;
						}
					}
				}

				break;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeNewAreaFill(int32 NetNr, int32 Layer, int32 Info, double ThickNess, double ThermalReliefThickness,
                      double ThermalReliefDistance, double Clearance, int32 mode)
{

	int32 ok, cnt, count, Found, res, obje, cnt5, TryCount, StartPinObjects;
	double minx, maxx, miny, maxy, x1, y1, x2, y2, RandValue;
	PolygonRecord *AreaFillPolygon, *SmallerPolygon, *DrawPolygon2, *PolygonObject, *PolygonObject2;
	ObjectRecord *Object, ErrorObject;
	int32 FoundError;
	uint8 PolygonBuf2[10240];
	uint8 PolygonBuf3[10240];
	char str2[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 fp;
	char str[MAX_LENGTH_STRING];
#endif

//  CurrentPolygon=(PolygonRecord *)&PolygonBuf2;
	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
	AllocateSpecialMem(MEM_POLYGON_BIGGER, 128 * 1024, (void **) &SmallerPolygon);
//  CopyPolygonToPolygon(DrawPolygon,CurrentPolygon);
	memset(&ErrorObject, 0, sizeof(ErrorObject));

	if (AllocateMemAreaFillMemoryTemp(0) != 0)
		return 0;

	RandValue = GetNewRandomValue(0);

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
	memset(NewAreaFill, 0, sizeof(AreaFillRecord));
	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	PolygonObject2 = (PolygonRecord *) & PolygonBuf3;
	NewAreaFill->Layer = Layer;
	NewAreaFill->NrPolygons = 1;
	NewAreaFill->NetNr = (int16) NetNr;
	NewAreaFill->Info |= Info & (AREAFILL_WITH_THERMAL_RELIEF | AREAFILL_WITH_NO_VIA_THERMAL_RELIEF);
	NewAreaFill->Clearance = (float) Clearance;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);
	NewAreaFill->ThermalReliefThickness = (float) ThermalReliefThickness;
	NewAreaFill->ThermalReliefDistance = (float) ThermalReliefDistance;

	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon->NrVertices = DrawPolygon2->NrVertices;
	AreaFillPolygon->PolygonType = 0;
	AreaFillPolygon->Special.Test = 0;
	NewAreaFill->NrVerticesStartPolygon = DrawPolygon2->NrVertices;
	AreaFillPolygon->Clearance = DrawPolygon2->Clearance;
	minx = 1000000000.0;
	miny = 1000000000.0;
	maxx = -1000000000.0;
	maxy = -1000000000.0;

	count = DrawPolygon2->NrVertices;

	if (!CheckNoCrossesInPolygon(DrawPolygon2))
		return 0;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*DrawPolygon2).Points[cnt].x;
		y1 = (*DrawPolygon2).Points[cnt].y;
		(*NewAreaFill).StartPolygon[cnt].x = (float) x1;
		(*NewAreaFill).StartPolygon[cnt].y = (float) y1;
	}

#ifdef _DEBUG
	sprintf(str, "%s\\pcb\\polygon.txt", DesignPath);

	if ((fp = FileOpenWriteUTF8(str)) > 0)
	{

		for (cnt = 0; cnt < count; cnt++)
		{
			x1 = (*DrawPolygon2).Points[cnt].x;
			y1 = (*DrawPolygon2).Points[cnt].y;

			if (Units == 0)
				sprintf(str, "%.1f , %.1f", x1 / 2540, y1 / 2540);
			else
				sprintf(str, "%.2f , %.2f", x1 / 100000, y1 / 100000);

			if (cnt < count - 1)
				strcat(str, ",");

			WriteToFile(fp, str);
		}

		FileClose(fp);
	}

#endif


	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*DrawPolygon2).Points[cnt].x;
		y1 = (*DrawPolygon2).Points[cnt].y;
		(*AreaFillPolygon).Points[cnt].x = x1;
		(*AreaFillPolygon).Points[cnt].y = y1;

		if (cnt < count - 1)
		{
			x2 = (*DrawPolygon2).Points[cnt + 1].x;
			y2 = (*DrawPolygon2).Points[cnt + 1].y;
		}
		else
		{
			x2 = (*DrawPolygon2).Points[0].x;
			y2 = (*DrawPolygon2).Points[0].y;
		}

		minx = min(minx, x1);
		miny = min(miny, y1);
		maxx = max(maxx, x1);
		maxy = max(maxy, y1);
		/*
		    if (cnt<4) {
		      sprintf(str,"  DrawPolygon->Points[%i].x=%.0f;\n",cnt,x1);
		      OutputDebugString(str);
		      sprintf(str,"  DrawPolygon->Points[%i].y=%.0f;\n",cnt,y1);
		      OutputDebugString(str);
		    }
		*/
	}

	SetMinMaxPolygon(AreaFillPolygon, 0);

	if (NotInRange(ThickNess, 0.0))
	{
// ************************************************************************************
// Make AreaFill with a smaller StartPolygon

		if (MakeBiggerSmallerPolygon(AreaFillPolygon, SmallerPolygon, ThickNess, 1) == -1)
			return 0;

		if (!CheckNoCrossesInPolygon(SmallerPolygon))
			return 0;

		/*
		    StartDrawingEditingWindow(0);
		    DrawTestPolygon(SmallerPolygon,1);
		    ExitDrawing();
		    EndDrawingEditingWindow(0);
		    DeAllocateMemAreaFills();
		    return 0;
		*/
		CopyPolygonToPolygon(SmallerPolygon, AreaFillPolygon);
		minx = 1000000000.0;
		miny = 1000000000.0;
		maxx = -1000000000.0;
		maxy = -1000000000.0;
		count = AreaFillPolygon->NrVertices;

		for (cnt = 0; cnt < count; cnt++)
		{
			x1 = (*AreaFillPolygon).Points[cnt].x;
			y1 = (*AreaFillPolygon).Points[cnt].y;

			if (cnt < count - 1)
			{
				x2 = (*AreaFillPolygon).Points[cnt + 1].x;
				y2 = (*AreaFillPolygon).Points[cnt + 1].y;
			}
			else
			{
				x2 = (*AreaFillPolygon).Points[0].x;
				y2 = (*AreaFillPolygon).Points[0].y;
			}

			minx = min(minx, x1);
			miny = min(miny, y1);
			maxx = max(maxx, x1);
			maxy = max(maxy, y1);
		}

		SetMinMaxPolygon(AreaFillPolygon, 0);
	}

	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);

	NewAreaFill->minx = minx;
	NewAreaFill->miny = miny;
	NewAreaFill->maxx = maxx;
	NewAreaFill->maxy = maxy;
	SetWaitCursor();

	FoundError = 0;

	if (InRange(ThickNess, 0.0))
	{
		SearchMinX = (minx - 1e5);
		SearchMinY = (miny - 1e5);
		SearchMaxX = (maxx + 1e5);
		SearchMaxY = (maxy + 1e5);

		NrObjects4 = 0;
		Found = 0;
		StartPinObjects = 0;

		if (NetNr >= 0)
		{
			Found += CopyTracesFromRectWindowToObjects4(Layer, 4);
			Found += CopyViasFromRectWindowToObjects4(Layer, 4);
			StartPinObjects = Found;
//      Found+=CopyAreafillsFromRectWindowToObjects4(Layer,4);
			Found += CopyCompObjectsFromRectWindowToObjects4(Layer, 4);
			Found += CopyOtherObjectsFromRectWindowToObjects4(Layer, 4);
		}

		if (AllocateMemPolygons(0) != 0)
		{
			SetNormalCursor();
			return 0;
		}

		for (cnt = 0; cnt < Found; cnt++)
		{
			Object = &((*Objects4)[cnt]);
			Object->Info2 = 0;
		}

		if (NetNr >= 0)
			RemoveObjectsWithSameNetNr(NetNr, StartPinObjects, Found, Clearance);

		cnt = 0;
		obje = -1;
		
		SelectionEsc = 0;

		while ((!FoundError) && (!SelectionEsc) && (cnt < Found))
		{
			if (Found > 20)
			{
				sprintf(InfoStr, "%i, %i", cnt, Found);
				RedrawInfoStr(1);
			}

			CheckForEscape();

			ObjectNr = cnt;
			Object = &((*Objects4)[cnt]);

			if (Object->NetNr != NetNr)
			{
#ifdef _DEBUG

				if (Object->ObjectType == TRACE_DIAG1)
					ok = 1;
				
				if (cnt == 22)
					ok = 1;

#endif
				res = -1;
				TryCount = 0;

				while ((res < 0) && (TryCount < 10))
				{
//        MakePolygonFromObject(Object,PolygonObject,AreaFillToChange->Clearance,
//                              ((TryCount+RandValue)*45.34),Found,0);
					MakePolygonFromObject(Object, PolygonObject, Clearance, (cnt + 1.0 + RandValue) + (TryCount * 10.0),
					                      Found, 2);
					memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion

					/*
					          if (ObjectNr>=21) {
					            StartDrawingEditingWindow(0);
					            InitDrawingBackGround(0,0);
					            DrawAreaFill(AreaFillToChange,1);
					            AreaFillToChange->Info|=OBJECT_NOT_VISIBLE;
					//        InvalidateRect(PCBWindow,NULL,0);
					//        PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
					//        CheckInputMessages(0);
					//        CheckInputMessages(0);
					//        AreaFillToChange->Info&=~OBJECT_NOT_VISIBLE;
					            InitDrawingAreaFills(0);
					            NewAreaFill->Info&=~(OBJECT_SELECTED);
					            DrawAreaFill(NewAreaFill,1);
					            ExitDrawing();
					            DrawTestPolygon(PolygonObject,1);  // green
					            while (!KeyPressed()) CheckInputMessages(0);
					            ReadKeyFunction();
					          }
					*/
					res = MergePolygon(PolygonObject, 0);
					TryCount++;
				}

#ifdef _DEBUG

//      CheckForEscape();
				if (TryCount == 10)
					ok = 1;

				if (res == 3)
					ok = 1;

#endif

				if (res < 0)
				{
					FoundError = 1;
					memmove(&ErrorObject, Object, sizeof(ObjectRecord));
				}
				else
				{
					/*
					          if (ObjectNr>61) {
					            InvalidateRect(PCBWindow,NULL,0);
					            PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
					            CheckInputMessages(0);
					            StartDrawingEditingWindow(0);
					            InitDrawingAreaFills(0);
					            DrawAreaFill(NewAreaFill,0);
					            DrawTestPolygon(PolygonObject,1);  // green
					            ExitDrawing();
					            while (!KeyPressed()) CheckInputMessages(0);
					            ReadKeyFunction();
					          }
					*/
				}
			}
			else
			{
				if (((NewAreaFill->Info & (AREAFILL_WITH_THERMAL_RELIEF | POWERPLANE)) == AREAFILL_WITH_THERMAL_RELIEF)
				        && ((Object->ObjectType != VIA_PUT_THROUGH_ROUND)
				            || ((NewAreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF) == 0))
				        && (NotInRange(NewAreaFill->ThermalReliefDistance, 0.0))
				        && (NotInRange(NewAreaFill->ThermalReliefThickness, 0.0)) && ((Object->Info2 & 1) == 0))
				{
					Object->x3 = NewAreaFill->ThermalReliefDistance + 20.0;
					Object->x2 += 20.0;
					Object->y2 += 20.0;

					for (cnt5 = 0; cnt5 < 4; cnt5++)
					{
						if (CopyThermalReliefInPolygon
						        (Object, PolygonObject, NewAreaFill->ThermalReliefThickness, (cnt + 1), Found, cnt5) == 0)
						{
							memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion
							res = MergePolygon(PolygonObject, 1);

							if (res < 0)
							{
								FoundError = 1;
								memmove(&ErrorObject, Object, sizeof(ObjectRecord));
							}
						}
					}
				}
			}

			cnt++;

			if (FoundError)
				ok = 1;
		}
	}

	if (FoundError)
		ok = 1;

	if (SelectionEsc)
		ok = 1;

	if (!FoundError)
	{
		if (!SelectionEsc)
		{
			if (!AddAreaFill(0))
			{
				FoundError = 1;
				MessageBufPos = 0;
				sprintf(str2, SC(999, "Error in calculating areafill\r\n"));

				if (AddToMessageBuf(str2) != 0)
				{
					SetNormalCursor();
					return 0;
				}

				MessageDialog("", 0, 0);
				DeAllocateMemMessageBuf();
			}
		}
		else
			SelectionEsc = 0;
	}
	else
	{
		if (!SelectionEsc)
		{
			MessageBufPos = 0;
			sprintf(str2, SC(1000, "Coordinates %.5f,%.5f\r\n"), ErrorObject.x1 / 100000, ErrorObject.y1 / 100000);

			if (AddToMessageBuf(str2) != 0)
			{
				SetNormalCursor();
				return 0;
			}

			MessageDialog(SC(1001, "Areafill to complex for excluding object"), 0, 0);
			DeAllocateMemMessageBuf();
		}
	}

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();
// MaxAreaFillMemory
	SetNormalCursor();

	if (!FoundError)
		return 1;
	else
		return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RebuildAreaFill(AreaFillRecord * AreaFillToChange, int32 mode)
{
	int32 ok, cnt, cnt2, count, cnt5, UsePolygon, mc, StartPinObjects, TryCount, res, MemSizePolygonObject, Found,
	      Layer, NetNr;
	double RandValue, x1a, y1a, x1, y1, x2, y2;
	PolygonRecord *AreaFillPolygon, *SubPolygon, *NewSubPolygon, *PreviousPolygon, *ObjectDrawPolygon, *PolygonObject;
	ObjectRecord *Object, ErrorObject;
	AreaFillRecord *ObjectAreaFill;
	int32 FoundError;
	uint8 *PolygonPos, *NewPolygonPos, *AreaPos;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

#ifdef _DEBUG
	AreaFillRecord *AreaFill2;
	PolygonRecord *PolygonObject2;
	int32 fp;
#endif

	mc = 0;
	memset(&ErrorObject, 0, sizeof(ErrorObject));

	RandValue = GetNewRandomValue(0);

	if (AllocateMemAreaFillMemoryTemp(AreaFillToChange->MemSize + 3172) != 0)
		return 0;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	memmove(NewAreaFill, AreaFillToChange, sizeof(AreaFillRecord));
	NewAreaFill->Info &= ~(OBJECT_SELECTED);
	NewAreaFill->NrPolygons = 0;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	SearchMinX = 1000000000.0;
	SearchMinY = 1000000000.0;
	SearchMaxX = -1000000000.0;
	SearchMaxY = -1000000000.0;

	count = NewAreaFill->NrVerticesStartPolygon;
	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon->NrVertices = count;
	NewAreaFill->NrPolygons = 1;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1a = NewAreaFill->StartPolygon[cnt].x;
		y1a = NewAreaFill->StartPolygon[cnt].y;
		(*AreaFillPolygon).Points[cnt].x = x1a;
		(*AreaFillPolygon).Points[cnt].y = y1a;
		SearchMinX = min(SearchMinX, x1a);
		SearchMinY = min(SearchMinY, y1a);
		SearchMaxX = max(SearchMaxX, x1a);
		SearchMaxY = max(SearchMaxY, y1a);
	}

	SetMinMaxPolygon(AreaFillPolygon, 0);
	SearchMinX -= 3e5;
	SearchMinY -= 3e5;
	SearchMaxX += 3e5;
	SearchMaxY += 3e5;
	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);



// ****************************************************************************

	Layer = NewAreaFill->Layer;
	NetNr = NewAreaFill->NetNr;


	NrObjects4 = 0;
	Found = 0;
	Found += CopyTracesFromRectWindowToObjects4(Layer, 4);
	Found += CopyViasFromRectWindowToObjects4(Layer, 4);
	StartPinObjects = Found;
	Found += CopyAreafillsFromRectWindowToObjects4(Layer, 4);
	Found += CopyCompObjectsFromRectWindowToObjects4(Layer, 4);
	Found += CopyOtherObjectsFromRectWindowToObjects4(Layer, 4);
#ifdef _DEBUG
	sprintf(str, "%s\\pcb\\objects.txt", DesignPath);

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return -5;

#endif

	if (AllocateMemPolygons(0) != 0)
		return 0;

	for (cnt = 0; cnt < Found; cnt++)
	{
		Object = &((*Objects4)[cnt]);
		Object->Info2 = 0;
	}

	MemSizePolygonObject = 128 * 1024;
	PolygonObject = (PolygonRecord *) malloc(MemSizePolygonObject);

	if (!PolygonObject)
		return -6;

	RemoveObjectsWithSameNetNr(NetNr, StartPinObjects, Found, AreaFillToChange->Clearance);
	
	FoundError = 0;
	SetTimer1();
	SetWaitCursor();
	cnt = 0;
	SelectionEsc = 0;

	while ((!FoundError) && (!SelectionEsc) && (cnt < Found))
	{
		if (Found > 20)
		{
			sprintf(InfoStr, "%i, %i", cnt, Found);
			RedrawInfoStr(1);
		}

		CheckForEscape();

		Object = &((*Objects4)[cnt]);
		ObjectNr = cnt;
#ifdef _DEBUG

		if (cnt == 143)
			ok = 1;

		if (cnt == Found - 1)
			ok = 1;

#endif

//    if ((Object->x1>
		if (Object->NetNr != NetNr)
		{
#ifdef _DEBUG

			if (Object->ObjectType == OBJECT_LINE)
				ok = 1;

			//    Comp=(CompRecord *)Object->TextP;
			if (cnt == 523)
				ok = 1;

			if ((InRange9(Object->x1, 170.5e5)) && (InRange9(Object->y1, 73.0e5)))
			{
				res = 1;

				if (Object->ObjectType == TRACE_VER)
					ok = 1;
			}

#endif
			res = -1;
			TryCount = 0;

			while ((res < 0) && (TryCount < 10))
			{
				if (Object->ObjectType == AREAFILL2)
				{
					if (Object->Test * sizeof(PointRecord) + sizeof(PolygonInitRecord) > (uint32) MemSizePolygonObject)
					{
						MemSizePolygonObject = Object->Test * sizeof(PointRecord) + sizeof(PolygonInitRecord);
						PolygonObject = (PolygonRecord *) realloc(PolygonObject, MemSizePolygonObject);

						if (!PolygonObject)
							return -8;
					}

#ifdef _DEBUG
					AreaFill2 = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object->TraceNr]]);
					PolygonObject2 = (PolygonRecord *) ((uint8 *) AreaFill2 + sizeof(AreaFillRecord));

					if ((ObjectNr == 143) && (TryCount == 0))
					{
						if (1)
							PolygonToObjectLines(PolygonObject2, INFO_LAYER3, 0, 0, 0);
					}

#endif
				}

				MakePolygonFromObject(Object, PolygonObject, AreaFillToChange->Clearance,
				                      (cnt + 1.0 + RandValue) + (TryCount * 10.0), Found, 2);
				memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion

#ifdef _DEBUG

				if ((ObjectNr == 143) && (TryCount == 0))
				{
					ok = 1;

					if (0)
						AreafillToObjectLines(NewAreaFill, INFO_LAYER2, PolygonObject, INFO_LAYER4, 0);

					if (1)
						PolygonToObjectLines(PolygonObject, INFO_LAYER4, 0, 0, 0);
				}

#endif
#if 0

				if (ObjectNr >= 602)
				{
					StartDrawingEditingWindow(0);
					SetBackGroundActive(0);
					DrawAreaFill(AreaFillToChange, 0);
					AreaFillToChange->Info |= OBJECT_NOT_VISIBLE;
					//        InvalidateRect(PCBWindow,NULL,0);
					//        PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
					//        CheckInputMessages(0);
					//        CheckInputMeassages(0);
					//        AreaFillToChange->Info&=~OBJECT_NOT_VISIBLE;
//          InitDrawingAreaFills(0);
					NewAreaFill->Info &= ~(OBJECT_SELECTED);
					DrawAreaFill(NewAreaFill, 2);
					DrawTestPolygon(PolygonObject, 3);	// red
					ExitDrawing();

					while (!KeyPressed())
						CheckInputMessages(0);

					ReadKeyFunction();
					ok = 1;
				}

#endif
				res = MergePolygon(PolygonObject, 0);
#ifdef _DEBUG

				if ((ObjectNr >= 143) && (TryCount == 0))
				{
					ok = 1;

					if (0)
						AreafillToObjectLines(NewAreaFill, INFO_LAYER3, PolygonObject, INFO_LAYER4, 0);
				}

				if (0)
					AreafillToObjectLines(NewAreaFill, INFO_LAYER3, 0, 0, 0);

#endif
				TryCount++;
			}



//      CheckForEscape();
			if (TryCount == 10)
				ok = 1;

			if (res < 0)
			{
				FoundError = 1;
				memmove(&ErrorObject, Object, sizeof(ObjectRecord));
			}
			else
			{

			}
		}
		else
		{
#ifdef _DEBUG

			if (Object->ObjectType == VIA_PUT_THROUGH_ROUND)
				ok = 1;

#endif

			if (((NewAreaFill->Info & (AREAFILL_WITH_THERMAL_RELIEF | POWERPLANE)) == AREAFILL_WITH_THERMAL_RELIEF)
			        && ((Object->ObjectType != VIA_PUT_THROUGH_ROUND)
			            || ((NewAreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF) == 0))
			        && (NotInRange(NewAreaFill->ThermalReliefDistance, 0.0))
			        && (NotInRange(NewAreaFill->ThermalReliefThickness, 0.0)) && ((Object->Info2 & 1) == 0))
			{
				Object->x3 = NewAreaFill->ThermalReliefDistance + 20.0;
				Object->x2 += 20.0;
				Object->y2 += 20.0;

				for (cnt5 = 0; cnt5 < 4; cnt5++)
				{
					res = -1;
					TryCount = 0;

					while ((res < 0) && (TryCount < 10))
					{

						if (CopyThermalReliefInPolygon
						        (Object, PolygonObject, NewAreaFill->ThermalReliefThickness,
						         (cnt + 1 + RandValue + (TryCount * 10.0)), Found, cnt5) == 0)
						{
							memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion
							res = MergePolygon(PolygonObject, 1);

							if (res < 0)
							{
								FoundError = 1;
								memmove(&ErrorObject, Object, sizeof(ObjectRecord));
								ErrorObject.ObjectType2 = 1;
								TryCount++;
#ifdef _DEBUG
								/*
								              AreaFillToChange->Info|=OBJECT_NOT_VISIBLE;
								              InvalidateRect(PCBWindow,NULL,0);
								              PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
								              CheckInputMessages(0);
								              CheckInputMessages(0);
								              AreaFillToChange->Info&=~OBJECT_NOT_VISIBLE;
								              StartDrawingEditingWindow(0);
								              InitDrawingAreaFills(0);
								              DrawAreaFill(NewAreaFill,0);
								              DrawTestPolygon(PolygonObject,1);  // green
								              ExitDrawing();
								              while (!KeyPressed()) CheckInputMessages(0);
								              ReadKeyFunction();
								*/
#endif
							}
						}
						else
							res = 0;

						/*
						            if (ObjectNr>6) {
						              AreaFillToChange->Info|=OBJECT_NOT_VISIBLE;
						              InvalidateRect(PCBWindow,NULL,0);
						              PostMessage(PCBWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
						              CheckInputMessages(0);
						              CheckInputMessages(0);
						              AreaFillToChange->Info&=~OBJECT_NOT_VISIBLE;
						              StartDrawingEditingWindow(0);
						              InitDrawingAreaFills(0);
						              DrawAreaFill(NewAreaFill,0);
						              DrawTestPolygon(PolygonObject,1);  // green
						              ExitDrawing();
						              while (!KeyPressed()) CheckInputMessages(0);
						              ReadKeyFunction();
						            }
						*/
					}
				}
			}
		}

		cnt++;
	}

	if (PolygonObject)
		free(PolygonObject);

	if (SelectionEsc)
		ok = 1;

	PreviousPolygon = NULL;
	ok = 1;

// ****************************************************************************
// Copy user deletion polygons to the new areafill
	if ((mode & 1) == 0)
	{
		PolygonPos = (uint8 *) ((uint8 *) AreaFillToChange + sizeof(AreaFillRecord));
		SubPolygon = (PolygonRecord *) PolygonPos;
		NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + NewAreaFill->MemSize);
		NewSubPolygon = (PolygonRecord *) NewPolygonPos;

		for (cnt2 = 0; cnt2 < AreaFillToChange->NrPolygons; cnt2++)
		{
			if (cnt2 > 0)
			{
#ifdef _DEBUG

				if ((SubPolygon->PolygonType & 8) == 8)
					ok = 1;

				if ((SubPolygon->PolygonType & (8 + 5)) == 5)
					ok = 1;

#endif

				if (((SubPolygon->PolygonType & 8) == 8)	// User deletion polygon that crosses the
				        ||			// main polygon
				        ((SubPolygon->PolygonType & 5) == 5))
				{	// User deletion polygon encapsulated in
					// the main polygon
					UsePolygon = 1;

					if (PreviousPolygon)
					{
						if (PreviousPolygon->NrVertices == SubPolygon->NrVertices)
						{
							if ((InRange
							        ((PreviousPolygon->minx + PreviousPolygon->maxx) * 0.5,
							         (SubPolygon->minx + SubPolygon->maxx) * 0.5))
							        &&
							        (InRange
							         ((PreviousPolygon->miny + PreviousPolygon->maxy) * 0.5,
							          (SubPolygon->miny + SubPolygon->maxy) * 0.5)))
							{
								ok = 1;
								UsePolygon = 0;
							}
						}
					}

					if (UsePolygon)
					{
						memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
						res = MergePolygon(SubPolygon, 5);

//          res=2;
// res = 1 : SubPolygon crossed the main polygon or deletion polygon(s)
// res = 2 : Error
// res = 3 : SubPolygon inside a deletion polygon
// res = 4 : SubPolygon appended
// res = 5 : SubPolygon does not overlap main polygon
						if (res > 0)
						{
							if ((res == 1) && ((SubPolygon->PolygonType & 8) == 8))
							{
								NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + NewAreaFill->MemSize);
								NewSubPolygon = (PolygonRecord *) NewPolygonPos;
								CopyPolygonToPolygon(SubPolygon, NewSubPolygon);
								NewSubPolygon->PolygonType = 8;
								NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
								NewAreaFill->NrPolygons++;
							}
						}
						else
						{
							FoundError = 1;
							ErrorObject.x1 = (SubPolygon->minx + SubPolygon->maxx) * 0.5;
							ErrorObject.y1 = (SubPolygon->miny + SubPolygon->maxy) * 0.5;
							//            memmove(&ErrorObject,Object,sizeof(ObjectRecord));
						}
					}
				}
			}

			if (cnt2 > 0)
				PreviousPolygon = SubPolygon;

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}
	}

// ****************************************************************************
#ifdef _DEBUG
//  CheckAreafill(NewAreaFill);
#endif


	if (!FoundError)
	{
		if (!SelectionEsc)
		{
			if (AddAreaFill(0))
			{
			}
			else
				FoundError = 1;
		}
	}
	else
	{
		if (!SelectionEsc)
		{
			MessageBufPos = 0;

			if (ErrorObject.ObjectType == AREAFILL2)
			{
				ObjectAreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[ErrorObject.TraceNr]]);
				AreaPos = (uint8 *) ObjectAreaFill;
				ObjectDrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
				x1 = ObjectDrawPolygon->Points[0].x;
				y1 = ObjectDrawPolygon->Points[0].y;

				if (Units == 0)
				{
					x2 = ObjectDrawPolygon->Points[0].x / 2540.0;
					y2 = ObjectDrawPolygon->Points[0].y / 2540.0;
					sprintf(str2, "Coordinates ( first areafill point ) %.2f , %.2f\r\n", x2, y2);
				}
				else
				{
					x2 = ObjectDrawPolygon->Points[0].x / 100000.0;
					y2 = ObjectDrawPolygon->Points[0].y / 100000.0;
					sprintf(str2, "Coordinates ( first areafill point ) %.5f , %.5f\r\n", x2, y2);
				}
			}
			else
			{
				x1 = ErrorObject.x1;
				y1 = ErrorObject.y1;

				if (Units == 0)
				{
					x2 = ErrorObject.x1 / 2540.0;
					y2 = ErrorObject.y1 / 2540.0;

					if (ErrorObject.ObjectType2 == 1)
						sprintf(str2, "Coordinates thermal relief object %.2f , %.2f\r\n", x2, y2);
					else
						sprintf(str2, "Coordinates object %.2f , %.2f\r\n", x2, y2);
				}
				else
				{
					x2 = ErrorObject.x1 / 100000.0;
					y2 = ErrorObject.y1 / 100000.0;

					if (ErrorObject.ObjectType2 == 1)
						sprintf(str2, "Coordinates thermal relief object %.5f , %.5f\r\n", x2, y2);
					else
						sprintf(str2, "Coordinates object %.5f , %.5f\r\n", x2, y2);
				}
			}

			if (AddToMessageBuf(str2) != 0)
			{
				if (MaxNrObjects4 > 4096)
					DeAllocateMemObjects4();

				DeAllocateMemPolygons();
				DeAllocateMemAreaFills();
				SetNormalCursor();
				return 0;
			}

			MessageDialog(SC(1001, "Areafill to complex for excluding object"), 1, 0);
			DeAllocateMemMessageBuf();
			CenterScreenOnPoint(x1, y1, 0);
			RePaint();
			CheckInputMessages(200);
			StartDrawingEditingWindow(0);
			InitDrawingObject(0, CROSS_HAIR_LAYER, 1, DRAW_WITH_DASH_PEN_AND_NO_BRUSH);
			SetROP2(OutputDisplay, R2_XORPEN);
			DrawLine(MultX(x1), -10000, MultX(x1), 10000);
			DrawLine(-10000, MultY(y1), 10000, MultY(y1));
			ExitDrawing();
			EndDrawingEditingWindow(0);

			if ((ProblemPolygon1) && (ProblemPolygon2))
			{
				strcpy(str, SC(786, "Do you want to copy the problem polygon outlines to layers INFO3,4"));

				if (MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_YESNO) == IDYES)
				{
					PolygonToObjectLines(ProblemPolygon1, INFO_LAYER3, 0, 0, 0);
					PolygonToObjectLines(ProblemPolygon2, INFO_LAYER4, 0, 0, 0);
					RePaint();
				}
			}
		}
	}

#ifdef _DEBUG
	FileClose(fp);
#endif

	if (SpecialDebugFile)
	{
		sprintf(str, " ( %d ms )", GetDifferenceTimer1inMilliSeconds());
		strcat(InfoStr, str);
		RedrawInfoStr(1);
	}

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();
// MaxAreaFillMemory
	SetNormalCursor();

	if (!FoundError)
	{
		if (SelectionEsc)
		{
			SelectionEsc = 0;
			return 2;
		}
		else
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MovePolygon(PolygonRecord * Polygon, double OffsetX, double OffsetY, int32 mode)
{
	int32 count, cnt3;

	count = Polygon->NrVertices;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		(*Polygon).Points[cnt3].x += OffsetX;
		(*Polygon).Points[cnt3].y += OffsetY;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OffsetX, double OffsetY, int32 mode)
{
	int32 count, cnt3;

	count = ObjectPolygon->NrVertices;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		(*ObjectPolygon).Points[cnt3].x += OffsetX;
		(*ObjectPolygon).Points[cnt3].y += OffsetY;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotateObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double CentreX, double CentreY, double Rotation,
                         int32 mode)
{
	int32 count, cnt3;

	count = ObjectPolygon->NrVertices;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		RotatePointFromOtherPoint2(&(*ObjectPolygon).Points[cnt3].x, &(*ObjectPolygon).Points[cnt3].y, CentreX, CentreY,
		                           Rotation);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PlaceMovedAreafill(int32 AreafillNr, double CurrentX, double CurrentY, double CurrentX2, double CurrentY2,
                         double CentreSelectedX, double CentreSelectedY, double Rotation, int32 mode)
{
	int32 cnt, cnt2, count, mc;
	double RandValue, x1a, y1a, cx, cy;
	AreaFillRecord *AreaFill;
	PolygonRecord *AreaFillPolygon, *SubPolygon, *NewSubPolygon;
	uint8 *PolygonPos, *NewPolygonPos;


	mc = 0;

	RandValue = GetNewRandomValue(0);

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

	if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
		return 0;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	memmove(NewAreaFill, AreaFill, sizeof(AreaFillRecord));
	NewAreaFill->Info &= ~(OBJECT_SELECTED);
	NewAreaFill->NrPolygons = 0;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	SearchMinX = 1000000000.0;
	SearchMinY = 1000000000.0;
	SearchMaxX = -1000000000.0;
	SearchMaxY = -1000000000.0;

	cx = AreaFill->StartPolygon[0].x;
	cy = AreaFill->StartPolygon[0].y;
	count = NewAreaFill->NrVerticesStartPolygon;
	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon->NrVertices = count;
	NewAreaFill->NrPolygons = 1;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1a = NewAreaFill->StartPolygon[cnt].x;
		y1a = NewAreaFill->StartPolygon[cnt].y;
		RotatePointFromOtherPoint2(&x1a, &y1a, CentreSelectedX, CentreSelectedY, Rotation);
		x1a += CurrentX - CurrentX2;
		y1a += CurrentY - CurrentY2;
		NewAreaFill->StartPolygon[cnt].x = (float) x1a;
		NewAreaFill->StartPolygon[cnt].y = (float) y1a;
		(*AreaFillPolygon).Points[cnt].x = x1a;
		(*AreaFillPolygon).Points[cnt].y = y1a;
		SearchMinX = min(SearchMinX, x1a);
		SearchMinY = min(SearchMinY, y1a);
		SearchMaxX = max(SearchMaxX, x1a);
		SearchMaxY = max(SearchMaxY, y1a);
	}

	NewAreaFill->minx = SearchMinX;
	NewAreaFill->miny = SearchMinY;
	NewAreaFill->maxx = SearchMaxX;
	NewAreaFill->maxy = SearchMaxY;
	SetMinMaxPolygon(AreaFillPolygon, 0);


// ****************************************************************************
// Copy user deleted polygons to the new areafill
	PolygonPos = (uint8 *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	SubPolygon = (PolygonRecord *) PolygonPos;
	NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;
	NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);
	NewPolygonPos += MemSizePolygon(NewSubPolygon);
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		if ((cnt2 > 0) && ((SubPolygon->PolygonType & (8 + 4)) != 0))
		{
			CopyPolygonToPolygon(SubPolygon, NewSubPolygon);
			RotatePolygon(NewSubPolygon, CentreSelectedX, CentreSelectedY, Rotation, 0);
			MovePolygon(NewSubPolygon, CurrentX - CurrentX2, CurrentY - CurrentY2, 0);
			SetMinMaxPolygon(NewSubPolygon, 0);
			NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
			NewPolygonPos += MemSizePolygon(NewSubPolygon);
			NewSubPolygon = (PolygonRecord *) NewPolygonPos;
			NewAreaFill->NrPolygons++;
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	if (AddAreaFill(0))
	{
		if ((mode & 2) == 0)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);
			AreaFill->DeleteNr = (int16) LastActionNr;
			AreaFill->Info |= OBJECT_NOT_VISIBLE;
		}

		if ((mode & 1) == 1)
		{
			AreafillNr = Design.NrAreaFills - 1;
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
			{
				if ((!IsLayerPowerPlane(AreaFill->Layer)) && (RebuildAreaFill(AreaFill, 0) == 1))
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);
					AreaFill->DeleteNr = 0;
					AreaFill->AddNr = 0;
					AreaFill->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}

		AreafillNr = Design.NrAreaFills - 1;
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

		if (AreaFill->NetNr != -1)
			ReCalcConnectionsNet(AreaFill->NetNr, 0, 1);

		RePaint();
	}

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();
	return 0;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 PlaceMovedAreafills(double CurrentX, double CurrentY, double CurrentX2, double CurrentY2, double CentreSelectedX,
                          double CentreSelectedY, double Rotation, uint8 * SelectedNetsForMove, int32 RepaintMode)
{
	int32 cnt, cnt2, cnt3, count, mc, AreafillNr;
	double RandValue, x1a, y1a;
	AreaFillRecord *AreaFill;
	PolygonRecord *AreaFillPolygon, *SubPolygon, *NewSubPolygon;
	PolygonRecord *FirstPolygon;
	int32 AreafillChanged;
	uint8 *PolygonPos, *NewPolygonPos;

	mc = 0;
	AreafillChanged = 0;
	RandValue = GetNewRandomValue(0);

	for (cnt3 = 0; cnt3 < Design.NrAreaFills; cnt3++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt3]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
			{
				if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
					return 0;

				NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
				TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
				memmove(NewAreaFill, AreaFill, sizeof(AreaFillRecord));
				NewAreaFill->Info &= ~(OBJECT_SELECTED);
				NewAreaFill->NrPolygons = 0;
				NewAreaFill->MemSize = sizeof(AreaFillRecord);

				SearchMinX = 1000000000.0;
				SearchMinY = 1000000000.0;
				SearchMaxX = -1000000000.0;
				SearchMaxY = -1000000000.0;
				count = NewAreaFill->NrVerticesStartPolygon;
				AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
				AreaFillPolygon->NrVertices = count;
				NewAreaFill->NrPolygons = 1;

				for (cnt = 0; cnt < count; cnt++)
				{
					x1a = NewAreaFill->StartPolygon[cnt].x;
					y1a = NewAreaFill->StartPolygon[cnt].y;
					RotatePointFromOtherPoint2(&x1a, &y1a, CentreSelectedX, CentreSelectedY, Rotation);
					x1a += CurrentX - CurrentX2;
					y1a += CurrentY - CurrentY2;
					NewAreaFill->StartPolygon[cnt].x = (float) x1a;
					NewAreaFill->StartPolygon[cnt].y = (float) y1a;
					(*AreaFillPolygon).Points[cnt].x = x1a;
					(*AreaFillPolygon).Points[cnt].y = y1a;
					SearchMinX = min(SearchMinX, x1a);
					SearchMinY = min(SearchMinY, y1a);
					SearchMaxX = max(SearchMaxX, x1a);
					SearchMaxY = max(SearchMaxY, y1a);
				}

#ifdef _DEBUG
				/*
				        DrawAreaFillStartPolygon(NewAreaFill,5);
				        while (!KeyPressed()) CheckInputMessages(0);
				        ReadKeyFunction();
				*/
#endif
				NewAreaFill->minx = SearchMinX;
				NewAreaFill->miny = SearchMinY;
				NewAreaFill->maxx = SearchMaxX;
				NewAreaFill->maxy = SearchMaxY;
				SetMinMaxPolygon(AreaFillPolygon, 0);


				// ****************************************************************************
				// Copy user deleted polygons to the new areafill
				PolygonPos = (uint8 *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
				SubPolygon = (PolygonRecord *) PolygonPos;
				NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
				NewSubPolygon = (PolygonRecord *) NewPolygonPos;
				NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);
				NewPolygonPos += MemSizePolygon(NewSubPolygon);
				NewSubPolygon = (PolygonRecord *) NewPolygonPos;

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					if ((cnt2 > 0) && ((SubPolygon->PolygonType & (8 + 4)) != 0))
					{
						CopyPolygonToPolygon(SubPolygon, NewSubPolygon);
						RotatePolygon(NewSubPolygon, CentreSelectedX, CentreSelectedY, Rotation, 0);
						MovePolygon(NewSubPolygon, CurrentX - CurrentX2, CurrentY - CurrentY2, 0);
						SetMinMaxPolygon(NewSubPolygon, 0);
						NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
						NewPolygonPos += MemSizePolygon(NewSubPolygon);
						NewSubPolygon = (PolygonRecord *) NewPolygonPos;
						NewAreaFill->NrPolygons++;
					}

					PolygonPos += MemSizePolygon(SubPolygon);
					SubPolygon = (PolygonRecord *) PolygonPos;
				}

				if (AddAreaFill(0))
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt3]]);
					AreaFill->DeleteNr = (int16) LastActionNr;
					AreaFill->Info |= OBJECT_NOT_VISIBLE;
					AreafillNr = Design.NrAreaFills - 1;
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

					if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
					{
						if ((!IsLayerPowerPlane(AreaFill->Layer)) && (RebuildAreaFill(AreaFill, 0) == 1))
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);
							AreaFill->DeleteNr = 0;
							AreaFill->AddNr = 0;
							AreaFill->Info |= OBJECT_NOT_VISIBLE;
						}
					}

					AreafillChanged = 1;
					AreafillNr = Design.NrAreaFills - 1;
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

					if (AreaFill->NetNr != -1)
						SelectedNetsForMove[AreaFill->NetNr] = 1;
				}
			}
		}
	}

	if (AreafillChanged)
		RepaintMode = 1;

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();

	return RepaintMode;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PlaceStretchedAreafill(int32 AreafillNr, double CurrentX, double CurrentY, int32 mode)
{
	int32 cnt, cnt2, count, mc;
	double x1, y1, RandValue, x1a, y1a, cx, cy, SearchMinX2, SearchMinY2, SearchMaxX2, SearchMaxY2;
	AreaFillRecord *AreaFill;
	PolygonRecord *AreaFillPolygon, *SubPolygon, *NewSubPolygon;
	uint8 *PolygonPos, *NewPolygonPos;

	mc = 0;

	RandValue = GetNewRandomValue(0);

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

	if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
		return 0;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	memmove(NewAreaFill, AreaFill, sizeof(AreaFillRecord));
	NewAreaFill->Info &= ~(OBJECT_SELECTED);
	NewAreaFill->NrPolygons = 0;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	SearchMinX2 = 1000000000.0;
	SearchMinY2 = 1000000000.0;
	SearchMaxX2 = -1000000000.0;
	SearchMaxY2 = -1000000000.0;

	cx = AreaFill->StartPolygon[0].x;
	cy = AreaFill->StartPolygon[0].y;
	count = NewAreaFill->NrVerticesStartPolygon;
	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon->NrVertices = count;
	NewAreaFill->NrPolygons = 1;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1a = NewAreaFill->StartPolygon[cnt].x;
		y1a = NewAreaFill->StartPolygon[cnt].y;

		if ((x1a >= SearchMinX) && (x1a <= SearchMaxX) && (y1a >= SearchMinY) && (y1a <= SearchMaxY))
		{
			x1a += CurrentX;
			y1a += CurrentY;
		}

		NewAreaFill->StartPolygon[cnt].x = (float) x1a;
		NewAreaFill->StartPolygon[cnt].y = (float) y1a;
//    (*AreaFillPolygon).Points[cnt].x=x1a;
//    (*AreaFillPolygon).Points[cnt].y=y1a;
		SearchMinX2 = min(SearchMinX2, x1a);
		SearchMinY2 = min(SearchMinY2, y1a);
		SearchMaxX2 = max(SearchMaxX2, x1a);
		SearchMaxY2 = max(SearchMaxY2, y1a);
	}

	NewAreaFill->minx = SearchMinX2;
	NewAreaFill->miny = SearchMinY2;
	NewAreaFill->maxx = SearchMaxX2;
	NewAreaFill->maxy = SearchMaxY2;
	SetMinMaxPolygon(AreaFillPolygon, 0);


// ****************************************************************************
// Copy user deleted polygons to the new areafill
	PolygonPos = (uint8 *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	SubPolygon = (PolygonRecord *) PolygonPos;
	NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;
	NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);
	NewPolygonPos += MemSizePolygon(NewSubPolygon);
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		if ((cnt2 > 0) && ((SubPolygon->PolygonType & (8 + 4)) != 0))
		{
			CopyPolygonToPolygon(SubPolygon, NewSubPolygon);
			count = NewSubPolygon->NrVertices;

			for (cnt = 0; cnt < count; cnt++)
			{
				x1 = (*NewSubPolygon).Points[cnt].x;
				y1 = (*NewSubPolygon).Points[cnt].y;

				if ((x1 >= SearchMinX) && (x1 <= SearchMaxX) && (y1 >= SearchMinY) && (y1 <= SearchMaxY))
				{
					(*NewSubPolygon).Points[cnt].x += CurrentX;
					(*NewSubPolygon).Points[cnt].y += CurrentY;
				}
			}

			SetMinMaxPolygon(NewSubPolygon, 0);
			NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
			NewPolygonPos += MemSizePolygon(NewSubPolygon);
			NewSubPolygon = (PolygonRecord *) NewPolygonPos;
			NewAreaFill->NrPolygons++;
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	if (AddAreaFill(0))
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);
		AreaFill->DeleteNr = (int16) LastActionNr;
		AreaFill->Info |= OBJECT_NOT_VISIBLE;

		if (mode == 1)
		{
			AreafillNr = Design.NrAreaFills - 1;
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
			{
				if ((!IsLayerPowerPlane(AreaFill->Layer)) && (RebuildAreaFill(AreaFill, 0) == 1))
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);
					AreaFill->DeleteNr = 0;
					AreaFill->AddNr = 0;
					AreaFill->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}

		AreafillNr = Design.NrAreaFills - 1;
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

		if (AreaFill->NetNr != -1)
			ReCalcConnectionsNet(AreaFill->NetNr, 0, 1);

		RePaint();
	}

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();
	return 0;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 GetAreaFillToRebuild(int32 mode)
{
	int32 cnt, Found;
	AreaFillRecord *AreaFill;
	PolygonRecord *FirstPolygon;

	Found = -1;

	if (!OkToDrawAreaFills)
		return 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0))
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
				Found = cnt;
		}
	}

	if (Found == -1)
		return -1;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);

	if (IsLayerPowerPlane(AreaFill->Layer))
		return -1;

	if (RebuildAreaFill(AreaFill, mode & 1) == 1)
	{
		ZeroUnusedObjects(0);
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
		AreaFill->DeleteNr = (int16) LastActionNr;
		AreaFill->Info |= OBJECT_NOT_VISIBLE;
		/*
		      for (cnt=0;cnt<Design.NrAreaFills;cnt++) {
		        AreaFill=(AreaFillRecord *)&(AreaFillMem[(*AreaFills)[cnt]]);
		        MemSize=sizeof(AreaFillRecord);
		        PolygonPos=(uint8 *)AreaFill;
		        PolygonPos+=sizeof(AreaFillRecord);
		        Polygon=(PolygonRecord *)PolygonPos;
		        for (cnt2=0;cnt2<AreaFill->NrPolygons;cnt2++) {
		          if (cnt2==340) {
		            ok=1;
		          }
		          MemSize+=MemSizePolygon(Polygon);
		          PolygonPos+=MemSizePolygon(Polygon);
		          Polygon=(PolygonRecord *)PolygonPos;
		        }
		        ok=1;
		      }
		*/
		RePaint();
//    ReCalcConnectionsNet(AreaFill->NetNr,0);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RemoveDeletionsAreaFill(int32 AreaFillNr, int32 mode)
{
	int32 cnt2, MemSizeAreaFill;
	PolygonRecord *SubPolygon, *NewSubPolygon;
	AreaFillRecord *AreaFillToChange;
	int32 FoundError = 0;
	int32 AreaFillChanged;
	uint8 *AreaPos, *PolygonPos, *NewAreaFillPos;

	if (!OkToDrawAreaFills)
		return 0;

// ***************************************************************************************
// ***************************************************************************************
	AreaFillToChange = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);

	if (AllocateMemAreaFillMemoryTemp(AreaFillToChange->MemSize + 3172) != 0)
		return 0;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
	memmove(NewAreaFill, AreaFillToChange, sizeof(AreaFillRecord));
	NewAreaFill->MemSize = sizeof(AreaFillRecord);
	NewAreaFillPos = (uint8 *) NewAreaFill + sizeof(AreaFillRecord);
	NewAreaFill->NrPolygons = 0;
	MemSizeAreaFill = sizeof(AreaFillRecord);
	AreaFillChanged = 0;
	AreaPos = (uint8 *) AreaFillToChange;
	SubPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < AreaFillToChange->NrPolygons; cnt2++)
	{
		if ((cnt2 == 0) || ((SubPolygon->PolygonType & 2) == 0))
		{
			NewSubPolygon = (PolygonRecord *) NewAreaFillPos;
			MemSizeAreaFill += MemSizePolygon(SubPolygon);

			if (MemSizeAreaFill > MaxAreaFillMemoryTemp)
			{
				if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill + 3172) != 0)
					return -105;

				NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
				TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
				NewAreaFillPos = (uint8 *) NewAreaFill + NewAreaFill->MemSize;
				NewSubPolygon = (PolygonRecord *) NewAreaFillPos;
			}

			CopyPolygonToPolygon(SubPolygon, NewSubPolygon);
			NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);
			NewAreaFillPos += MemSizePolygon(NewSubPolygon);
			NewAreaFill->NrPolygons++;
		}
		else
		{
			SubPolygon->PolygonType &= ~2;
			AreaFillChanged = 1;
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	if (AreaFillChanged)
	{
		if (AddAreaFill(0))
		{
			AreaFillToChange = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);
			AreaFillToChange->Info |= OBJECT_NOT_VISIBLE;
			AreaFillToChange->DeleteNr = (int16) LastActionNr;
//      RePaint();
		}
		else
			FoundError = 1;
	}

	DeAllocateMemAreaFills();

	if (!FoundError)
		return 1;
	else
		return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RemoveUnusedDeletionsAreaFill(int32 mode)
{
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DeleteFromAreaFill(int32 AreaFillNr, int32 mode)
{
	int32 cnt, count, res, FoundError, NewAreaFillNr;
	uint8 *NewAreaFillPos;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon2, *NewSubPolygon;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);
	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
	SetMinMaxPolygon(DrawPolygon2, 0);
//  DeletionPolygon=(PolygonRecord *)(&Buf[131072]);
	FoundError = 0;


//  RandValue*=10.4321;

	count = DrawPolygon2->NrVertices;

	/*
	  for (cnt=0;cnt<count;cnt++) {
	    x1=(*DrawPolygon2).Points[cnt].x;
	    y1=(*DrawPolygon2).Points[cnt].y;
	    (*DeletionPolygon).Points[cnt].x=x1;
	    (*DeletionPolygon).Points[cnt].y=y1;
	  }
	  DeletionPolygon->NrVertices=count;
	*/
	if (((mode & 2) == 0) && (CheckPolygonOverlapAreaFill(DrawPolygon2, AreaFill) == 0))
		return 0;

	if (AllocateMemAreaFillMemoryTemp(max(AreaFill->MemSize + 16384, 256 * 1024)) != 0)
		return -1;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);

#ifdef _DEBUG
	AreaFillToText(AreaFill, 0);
#endif

	if (AllocateMemPolygons(0) != 0)
		return -1;

	if (mode & 2)
	{
		memset(NewAreaFill, 0, 128 * 1024);
		NewAreaFill->NrPolygons = 1;
		NewAreaFill->MemSize = sizeof(AreaFillRecord);
		NewSubPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
		/*
		    NewSubPolygon->NrVertices=4;
		    NewSubPolygon->Points[0].x= 0.5e9;
		    NewSubPolygon->Points[0].y= 0.5e9;
		    NewSubPolygon->Points[1].x=-0.5e9;
		    NewSubPolygon->Points[1].y= 0.5e9;
		    NewSubPolygon->Points[2].x=-0.5e9;
		    NewSubPolygon->Points[2].y=-0.5e9;
		    NewSubPolygon->Points[3].x= 0.5e9;
		    NewSubPolygon->Points[3].y=-0.5e9;
		    NewAreaFill->MemSize+=MemSizePolygon(NewSubPolygon);
		    NewSubPolygon=(PolygonRecord *)((uint8 *)NewSubPolygon+MemSizePolygon(NewSubPolygon));
		*/
		count = AreaFill->NrVerticesStartPolygon;
		NewSubPolygon->NrVertices = count;
		NewSubPolygon->PolygonType = 0;

		for (cnt = 0; cnt < count; cnt++)
		{
			NewSubPolygon->Points[cnt].x = AreaFill->StartPolygon[cnt].x;
			NewSubPolygon->Points[cnt].y = AreaFill->StartPolygon[cnt].y;
		}

		SetMinMaxPolygon(NewSubPolygon, 0);
		NewAreaFill->minx = NewSubPolygon->minx;
		NewAreaFill->miny = NewSubPolygon->miny;
		NewAreaFill->maxx = NewSubPolygon->maxx;
		NewAreaFill->maxy = NewSubPolygon->maxy;
#ifdef _DEBUG

		if (1)
			PolygonToObjectLines(NewSubPolygon, INFO_LAYER2, DrawPolygon2, INFO_LAYER3, 0);

#endif
		NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);
		memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
		res = MergePolygon(DrawPolygon2, 1);

		if (res < 0)
		{
			DeAllocateMemPolygons();
			DeAllocateMemAreaFills();
			return 0;
		}

		memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
		memmove(NewAreaFill, AreaFill, AreaFill->MemSize);
		NewSubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
#ifdef _DEBUG

		if (1)
			PolygonToObjectLines(NewSubPolygon, INFO_LAYER4, 0, 0, 0);

#endif
		count = NewSubPolygon->NrVertices;

		if (count > 200)
		{
			DeAllocateMemPolygons();
			DeAllocateMemAreaFills();
			return 0;
		}

		for (cnt = 0; cnt < count; cnt++)
		{
			NewAreaFill->StartPolygon[cnt].x = (float) NewSubPolygon->Points[cnt].x;
			NewAreaFill->StartPolygon[cnt].y = (float) NewSubPolygon->Points[cnt].y;
		}

		NewAreaFill->NrVerticesStartPolygon = count;

		if (!AddAreaFill(0))
		{
			DeAllocateMemPolygons();
			DeAllocateMemAreaFills();
			return 0;
		}

		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);
		AreaFill->Info |= OBJECT_NOT_VISIBLE;
		AreaFill->DeleteNr = (int16) LastActionNr;

		NewAreaFillNr = Design.NrAreaFills - 1;
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[NewAreaFillNr]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			if ((!IsLayerPowerPlane(AreaFill->Layer)) && (RebuildAreaFill(AreaFill, 0) == 1))
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[NewAreaFillNr]]);
				AreaFill->DeleteNr = 0;
				AreaFill->AddNr = 0;
				AreaFill->Info |= OBJECT_NOT_VISIBLE;
			}
		}

		DeAllocateMemPolygons();
		DeAllocateMemAreaFills();
		return 0;
	}

	memmove(AreaFillMemTemp2, AreaFill, AreaFill->MemSize);
// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion
	res = MergePolygon(DrawPolygon2, 5);
#ifdef _DEBUG
	AreaFillToText(NewAreaFill, 1);
#endif

	if (res == -5)
		MessageBoxOwn(PCBWindow, SC(1002, "To complex"), SC(24, "Error"), MB_APPLMODAL | MB_OKCANCEL);

	if (res < 0)
		FoundError = 1;

// res = 1 : SubPolygon crossed the main polygon or deletion polygon(s)
// res = 2 : Error
// res = 3 : SubPolygon inside a deletion polygon
// res = 4 : SubPolygon appended
// res = 5 : SubPolygon does not overlap main polygon
// res = 6 : SubPolygon
	if ((res == 1) || (res == 2) || (res == 3))
	{
		NewAreaFillPos = (uint8 *) NewAreaFill + NewAreaFill->MemSize;
		NewSubPolygon = (PolygonRecord *) NewAreaFillPos;
		DrawPolygon2->PolygonType = 8;
		CopyPolygonToPolygon(DrawPolygon2, NewSubPolygon);
		NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);
		NewAreaFill->NrPolygons++;
	}

	if (!FoundError)
	{
		if (!SelectionEsc)
		{
			if (!AddAreaFill(0))
				FoundError = 1;
		}
		else
		{
			SelectionEsc = 0;
			FoundError = 1;
		}
	}
	else
		ok = 1;

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();

	if (FoundError)
		return -1;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);
	AreaFill->Info |= OBJECT_NOT_VISIBLE;
	AreaFill->DeleteNr = (int16) LastActionNr;

	if (OkToDrawAreaFills)
		RePaint();

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindSelectedAreaFill(int32 mode)
{
	int32 cnt;
	AreaFillRecord *AreaFill;
	PolygonRecord *FirstPolygon;

	if (!OkToDrawAreaFills)
		return -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (mode == 0)
		{
			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
			{
				FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				if ((FirstPolygon->PolygonType & 2) == 2)
					return cnt;
			}
		}
		else
		{
			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE)
			{
				FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				if ((FirstPolygon->PolygonType & 2) == 2)
					return cnt;
			}
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DeleteFromObjectPolygon(int32 ObjectPolygonNr, int32 mode)
{
	ObjectPolygonRecord *ObjectPolygon, *ObjectPolygon2;
	int32 cnt, ok, res, count, Layer, TryCount;
	double x1a, y1a;
	PolygonRecord *AreaFillPolygon, *DrawPolygon2;

	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);

	ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[ObjectPolygonNr]]);
	Layer = ObjectPolygon->Layer;

	if (AllocateMemAreaFillMemoryTemp(128 * 1024) != 0)
		return -1;

	if (AllocateMemPolygons(0) != 0)
		return -1;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	NewAreaFill->Info &= ~(OBJECT_SELECTED);
	NewAreaFill->NrPolygons = 1;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	count = ObjectPolygon->NrVertices;
	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	memset(AreaFillPolygon, 0, sizeof(PolygonRecord));
	AreaFillPolygon->NrVertices = count;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1a = (*ObjectPolygon).Points[cnt].x;
		y1a = (*ObjectPolygon).Points[cnt].y;
		(*AreaFillPolygon).Points[cnt].x = x1a;
		(*AreaFillPolygon).Points[cnt].y = y1a;
	}

	SetMinMaxPolygon(AreaFillPolygon, 0);
	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);

	res = -1;
	TryCount = 0;

	while ((res < 0) && (TryCount < 10))
	{
		memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
		res = MergePolygon(DrawPolygon2, 0);
		TryCount++;
	}

	if (TryCount == 10)
		ok = 1;

	if (res >= 0)
	{
		AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
		memset(&NewObjectPolygon, 0, sizeof(NewObjectPolygon));
		NewObjectPolygon.NrVertices = AreaFillPolygon->NrVertices;
		NewObjectPolygon.minx = AreaFillPolygon->minx;
		NewObjectPolygon.maxx = AreaFillPolygon->maxx;
		NewObjectPolygon.miny = AreaFillPolygon->miny;
		NewObjectPolygon.maxy = AreaFillPolygon->maxy;
		NewObjectPolygon.Layer = Layer;
		NewObjectPolygon.Info = OBJECT_SELECTED;
		memmove(&NewObjectPolygon.Points, &((*AreaFillPolygon).Points),
		        AreaFillPolygon->NrVertices * sizeof(PointRecord));

		if (AddObjectPolygon(&NewObjectPolygon))
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[ObjectPolygonNr]]);
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
			ObjectPolygon->DeleteNr = (int16) LastActionNr;
			ObjectPolygon2 =
			    (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[Design.NrObjectPolygons - 1]]);
		}

		RePaint();
	}

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();
	SetNormalCursor();

	if (res >= 0)
		return 0;
	else
		return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CommandAddToAreaFill(int32 mode)
{
	int32 cnt, cnt2, Found, NetNr, AreaFillNr, res;
	double x1a, y1a;
	AreaFillRecord *AreaFill;
	uint8 *PolygonPos, *NewPolygonPos;
	PolygonRecord *FirstPolygon, *AreaFillPolygon, *SubPolygon, *DrawPolygon2, *NewSubPolygon;

	if (!OkToDrawAreaFills)
		return 0;

	Found = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
				Found = cnt;
		}
	}

	if (Found == -1)
		return 0;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
	NetNr = AreaFill->NetNr;

	if (CommandAddPolygonLines(0.0, -1, 0, OBJECT_POLYLINE) == -1)
		return 0;

	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);

	if (CheckPolygonOverlapAreaFill(DrawPolygon2, AreaFill) == 0)
		return 0;

	if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 32768) != 0)
		return 0;

	AllocateMemPolygons(512);
	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	memset(NewAreaFill, 0, sizeof(AreaFillRecord));
	NewAreaFill->NrVerticesStartPolygon = 4;
	NewAreaFill->StartPolygon[0].x = -1e9;
	NewAreaFill->StartPolygon[0].y = -1e9;
	NewAreaFill->StartPolygon[1].x = 1e9;
	NewAreaFill->StartPolygon[1].y = -1e9;
	NewAreaFill->StartPolygon[2].x = 1e9;
	NewAreaFill->StartPolygon[2].y = 1e9;
	NewAreaFill->StartPolygon[3].x = -1e9;
	NewAreaFill->StartPolygon[3].y = 1e9;
	NewAreaFill->minx = -1e9;
	NewAreaFill->miny = -1e9;
	NewAreaFill->maxx = 1e9;
	NewAreaFill->maxy = 1e9;
	NewAreaFill->NrPolygons = 2;
	NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;
	NewSubPolygon->Points[0].x = -1e9;
	NewSubPolygon->Points[0].y = -1e9;
	NewSubPolygon->Points[1].x = 1e9;
	NewSubPolygon->Points[1].y = -1e9;
	NewSubPolygon->Points[2].x = 1e9;
	NewSubPolygon->Points[2].y = 1e9;
	NewSubPolygon->Points[3].x = -1e9;
	NewSubPolygon->Points[3].y = 1e9;
	NewSubPolygon->NrVertices = 4;
	NewSubPolygon->PolygonType = 0;
	SetMinMaxPolygon(NewSubPolygon, 0);
	NewAreaFill->MemSize = sizeof(AreaFillRecord);
	NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);

	NewPolygonPos += MemSizePolygon(NewSubPolygon);
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;
	NewSubPolygon->NrVertices = AreaFill->NrVerticesStartPolygon;

	for (cnt = 0; cnt < AreaFill->NrVerticesStartPolygon; cnt++)
	{
		NewSubPolygon->Points[cnt].x = AreaFill->StartPolygon[cnt].x;
		NewSubPolygon->Points[cnt].y = AreaFill->StartPolygon[cnt].y;
	}

	NewSubPolygon->PolygonType = 0;
	SetMinMaxPolygon(NewSubPolygon, 0);
	NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);

	memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
	res = MergePolygon(DrawPolygon2, 0);

	if ((res < 0) || (res == 3) || (res == 4))
		return -1;

	NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;
	NewPolygonPos += MemSizePolygon(NewSubPolygon);
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;

	if (NewSubPolygon->NrVertices > 200)
		return -1;

	DrawPolygon2->NrVertices = NewSubPolygon->NrVertices;

	for (cnt = 0; cnt < NewSubPolygon->NrVertices; cnt++)
	{
		x1a = NewSubPolygon->Points[cnt].x;
		y1a = NewSubPolygon->Points[cnt].y;
		DrawPolygon2->Points[cnt].x = x1a;
		DrawPolygon2->Points[cnt].y = y1a;
	}


	memmove(NewAreaFill, AreaFill, sizeof(AreaFillRecord));
	NewAreaFill->Info &= ~(OBJECT_SELECTED);
	NewAreaFill->NrPolygons = 0;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	SearchMinX = 1000000000.0;
	SearchMinY = 1000000000.0;
	SearchMaxX = -1000000000.0;
	SearchMaxY = -1000000000.0;

	NewAreaFill->NrPolygons = 1;
	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon->NrVertices = DrawPolygon2->NrVertices;
	NewAreaFill->NrVerticesStartPolygon = DrawPolygon2->NrVertices;

	for (cnt = 0; cnt < DrawPolygon2->NrVertices; cnt++)
	{
		x1a = DrawPolygon2->Points[cnt].x;
		y1a = DrawPolygon2->Points[cnt].y;
		NewAreaFill->StartPolygon[cnt].x = (float) x1a;
		NewAreaFill->StartPolygon[cnt].y = (float) y1a;
		SearchMinX = min(SearchMinX, x1a);
		SearchMinY = min(SearchMinY, y1a);
		SearchMaxX = max(SearchMaxX, x1a);
		SearchMaxY = max(SearchMaxY, y1a);
		AreaFillPolygon->Points[cnt].x = x1a;
		AreaFillPolygon->Points[cnt].y = y1a;
	}

	SetMinMaxPolygon(AreaFillPolygon, 0);
	NewAreaFill->minx = SearchMinX;
	NewAreaFill->miny = SearchMinY;
	NewAreaFill->maxx = SearchMaxX;
	NewAreaFill->maxy = SearchMaxY;
	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);


// ****************************************************************************
// Copy user deleted polygons to the new areafill
	PolygonPos = (uint8 *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	SubPolygon = (PolygonRecord *) PolygonPos;
	NewPolygonPos = (uint8 *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;
	NewAreaFill->MemSize += MemSizePolygon(NewSubPolygon);
	NewPolygonPos += MemSizePolygon(NewSubPolygon);
	NewSubPolygon = (PolygonRecord *) NewPolygonPos;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		if ((cnt2 > 0) && ((SubPolygon->PolygonType & (8 + 4)) != 0))
		{
			CopyPolygonToPolygon(SubPolygon, NewSubPolygon);
			NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
			NewPolygonPos += MemSizePolygon(NewSubPolygon);
			NewSubPolygon = (PolygonRecord *) NewPolygonPos;
			NewAreaFill->NrPolygons++;
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	ZeroUnusedObjects(0);
	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
	AreaFill->Info &= ~(OBJECT_SELECTED);
	AreaFill->Info |= OBJECT_NOT_VISIBLE;
	AreaFill->DeleteNr = (int16) LastActionNr;

	if (AddAreaFill(0))
	{
		AreaFillNr = Design.NrAreaFills - 1;
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);

		if ((RebuildAreaFill(AreaFill, 0) == 1))
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreaFillNr]]);
			AreaFill->DeleteNr = 0;
			AreaFill->Info |= OBJECT_NOT_VISIBLE;
			RePaint();
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandCutFromAreaFill(int32 ObjectType, int32 mode)
{
	int32 cnt, Found, NetNr;
	AreaFillRecord *AreaFill;
	NetRecord *Net;
	PolygonRecord *FirstPolygon;

	if (!OkToDrawAreaFills)
		return;

	Found = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
				Found = cnt;
		}
	}

	if (Found == -1)
		return;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);

	NetNr = AreaFill->NetNr;
	GetObjectsNet((int32) NetNr, MODE_OBJECTS3, 0);

	if (NetNr >= 0)
		Net = &((*Nets)[NetNr]);
	else
		Net = &EmptyNet;

	CurrentDrawingNetNr = NetNr;
	strcpy(InfoStr, Net->Name);
	RedrawInfoStr(1);

	if (mode == 0)
		CommandAddPolygonLines(0.0, AreaFill->Layer, 1, ObjectType);
	else
		CommandAddPolygonLines(0.0, AreaFill->Layer, 0x11, ObjectType);

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);

	if (AreaFill->NetNr != -1)
	{
		ReCalcConnectionsNet(AreaFill->NetNr, 0, 1);
		RePaint();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetPowerPlaneToCutFrom(int32 Layer, double ThickNess, int32 ObjectType, int32 mode)
{
	AreaFillRecord *AreaFill;
	int32 cnt, count, Found, NetNr;
	double x1, y1;
	PolygonRecord *AreaFillPolygon, *BiggerPolygon, *DrawPolygon2;
	NetRecord *Net;

	if (!OkToDrawAreaFills)
		return;

	if ((Found = GetPowerPlaneByLayer(Layer)) == -1)
		return;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);

	if (mode == 0)
	{
		NetNr = AreaFill->NetNr;

		if (AreaFill->NetNr == -1)
			return;

		GetObjectsNet((int32) NetNr, MODE_OBJECTS3, 0);
		Net = &((*Nets)[NetNr]);
		CurrentDrawingNetNr = NetNr;
		strcpy(InfoStr, Net->Name);
		RedrawInfoStr(1);

		CommandAddPolygonLines(0.0, Layer, 2, ObjectType);
	}
	else
	{
		DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
		count = DrawPolygon2->NrVertices;
		AllocateSpecialMem(MEM_POLYGON_BIGGER, 128 * 1024, (void **) &BiggerPolygon);
		AllocateSpecialMem(MEM_POLYGON_BIGGER2, 128 * 1024, (void **) &AreaFillPolygon);

		for (cnt = 0; cnt < count; cnt++)
		{
			x1 = (*DrawPolygon2).Points[cnt].x;
			y1 = (*DrawPolygon2).Points[cnt].y;
			(*AreaFillPolygon).Points[cnt].x = x1;
			(*AreaFillPolygon).Points[cnt].y = y1;
		}

		AreaFillPolygon->NrVertices = count;

		if (MakeBiggerSmallerPolygon(AreaFillPolygon, BiggerPolygon, ThickNess, 0) == -1)
			return;

		if (!CheckNoCrossesInPolygon(BiggerPolygon))
			return;

		for (cnt = 0; cnt < count; cnt++)
		{
			x1 = (*BiggerPolygon).Points[cnt].x;
			y1 = (*BiggerPolygon).Points[cnt].y;
			(*DrawPolygon2).Points[cnt].x = x1;
			(*DrawPolygon2).Points[cnt].y = y1;
		}

		if (DeleteFromAreaFill(Found, 1) == 0)
			return;

		/*
		    AreaFill=(AreaFillRecord *)&(AreaFillMem[(*AreaFills)[Found]]);
		    StartDrawingEditingWindow(0);
		    InitDrawingBackGround();
		    DrawAreaFill(AreaFill,1);
		    AreaFill->Info|=OBJECT_NOT_VISIBLE;
		    AreaFill->DeleteNr=(int16)LastActionNr;
		    AreaFill=(AreaFillRecord *)&(AreaFillMem[(*AreaFills)[Design.NrAreaFills-1]]);
		    DrawAreaFill(AreaFill,0);
		    ExitDrawing();
		    EndDrawingEditingWindow(0);
		*/
	}

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);

	if (AreaFill->NetNr != -1)
	{
		ReCalcConnectionsNet(AreaFill->NetNr, 0, 1);
		RePaint();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddNewAreaFill(int32 Layer, int32 mode)
{
	double ThickNess, MaxClearance;
	NetRecord *Net, *Net2, *PowerNet;
	int32 NetNr, Found, cnt, res;
	uint8 AreaFillMem2[16384];
	AreaFillRecord *AreaFill;
	char str[MAX_LENGTH_STRING];
	PolygonRecord *BiggerPolygon, *DrawPolygon, *AreaFillPolygon;

	if (!OkToDrawAreaFills)
		return 0;

	DefaultAreaFill = (AreaFillRecord *) & AreaFillMem2;
	memset(DefaultAreaFill, 0, sizeof(AreaFillRecord));
	DefaultAreaFill->Clearance = Design.StandardClearance;
	DefaultAreaFill->Layer = Layer;
	DefaultAreaFill->ThermalReliefThickness = Design.StandardClearance;
	DefaultAreaFill->ThermalReliefDistance = (12 * 2540);
	DefaultAreaFill->Info = AREAFILL_WITH_THERMAL_RELIEF | AREAFILL_WITH_NO_VIA_THERMAL_RELIEF;

	PowerPlaneNetNr = -1;

	if (CheckIfLayerIsAPowerPlane(Layer))
	{
		if ((Found = GetPowerPlaneByLayer(Layer)) != -1)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
			PowerPlaneNetNr = AreaFill->NetNr;
			PowerNet = &((*Nets)[PowerPlaneNetNr]);
		}

		DefaultAreaFill->SurroundThickNess = (float) CurrentClearance;

		if (mode == 0)
		{
			if (AreaFillDialog(DefaultAreaFill, 0) == -1)
				return 0;
		}
		else
			DefaultAreaFill->NetNr = -1;

		ThickNess = DefaultAreaFill->SurroundThickNess;
	}
	else
	{
		if (mode == 0)
		{
			if (AreaFillDialog(DefaultAreaFill, 1) == -1)
				return 0;
		}
		else
			DefaultAreaFill->NetNr = -1;

		ThickNess = 0.0;
//    ThickNess=DefaultAreaFill->Clearance;
	}

	NetNr = DefaultAreaFill->NetNr;

	if (NetNr >= 0)
	{
		GetObjectsNet((int32) NetNr, MODE_OBJECTS3, 0);
		Net = &((*Nets)[NetNr]);
		CurrentDrawingNetNr = NetNr;

		if (PowerPlaneNetNr != -1)
			AddPowerPlaneObjects3(PowerPlaneNetNr, 0);

		strcpy(InfoStr, Net->Name);
		RedrawInfoStr(1);
	}
	else
		DefaultAreaFill->Clearance = 0.0;

	if (CommandAddPolygonLines(ThickNess, -1, 0, OBJECT_POLYLINE) == -1)
		return 0;

	if (NetNr >= 0)
	{
		DrawPolygon = (PolygonRecord *) (&PolygonBuf);
		AllocateSpecialMem(MEM_POLYGON_BIGGER, 128 * 1024, (void **) &BiggerPolygon);

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0) && (AreaFill->Layer == Layer)
			        && (AreaFill->NetNr != CurrentDrawingNetNr))
			{
				if (AreaFill->NetNr >= 0)
					Net2 = &((*Nets)[AreaFill->NetNr]);
				else
					Net2 = &EmptyNet;

				MaxClearance = max(DefaultAreaFill->Clearance, AreaFill->Clearance);
				MakeBiggerSmallerPolygon(DrawPolygon, BiggerPolygon, MaxClearance * 1.99, 0);
				res = CheckPolygonOverlapAreaFill(BiggerPolygon, AreaFill);

				if (res == 1)
				{
					AreaFillPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
					strcpy(str,
					       SC(789,
					          "Overlapping areafills\r\n\r\nDo you want to copy the problem polygon outlines to layers INFO3,4"));

					if (MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_YESNO) == IDYES)
					{
						PolygonToObjectLines(AreaFillPolygon, INFO_LAYER3, 0, 0, 0);
						PolygonToObjectLines(BiggerPolygon, INFO_LAYER4, 0, 0, 0);
					}

					return 0;
				}
			}
		}
	}

if (MakeNewAreaFill
	        (DefaultAreaFill->NetNr, Layer, DefaultAreaFill->Info, DefaultAreaFill->SurroundThickNess,
	         DefaultAreaFill->ThermalReliefThickness, DefaultAreaFill->ThermalReliefDistance, DefaultAreaFill->Clearance,
	         0) == 1)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Design.NrAreaFills - 1]]);
		AreaFill->FillType = 2;

		if (AreaFill->NetNr != -1)
			ReCalcConnectionsNet(AreaFill->NetNr, 0, 1);

		RePaint();

		if (NetNr >= 0)
		{
			if (NotInRange(DefaultAreaFill->SurroundThickNess, 0.0))
				GetPowerPlaneToCutFrom(Layer, ThickNess, OBJECT_POLYLINE, 1);
		}
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetAreaFillNrPowerPlane(int32 Layer)
{
	int32 cnt;
	AreaFillRecord *AreaFill;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE) && (AreaFill->Layer == Layer))
			return cnt;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckIfLayerIsAPowerPlane(int32 Layer)
{
	int32 cnt;
	AreaFillRecord *AreaFill;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE) && (AreaFill->Layer == Layer))
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectInsideAreaFillPowerPlane(ObjectRecord * Object)
{
	int32 cnt;
	AreaFillRecord *AreaFill;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE) && (AreaFill->Layer == Object->Layer))
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InsertObjectInAreaFill(ObjectRecord * Object, int32 Layer, int32 NetNr, int32 mode)
{
	int32 cnt, cnt2, res, NrAreaFills, Mask, Mask2;
	double RandValue, MaxClearance;
	AreaFillRecord *AreaFill;
	uint8 *AreaFillPos;
	PolygonRecord *SurroundPolygon;
	PolygonRecord *PolygonObject;
	ObjectRecord Object2;
	int32 AreaFillChanged = 0;
	int32 OkToIncludeObject;

	OldValue2 = 0.1234;

	PolygonObject = (PolygonRecord *) & PolygonBuf;
	Mask = OBJECT_NOT_VISIBLE | POWERPLANE;
	Mask2 = 0;
	NrAreaFills = Design.NrAreaFills;

	for (cnt = 0; cnt < NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
#ifdef _DEBUG

		if (AreaFill->NetNr == 65)
			ok = 1;

#endif
		AreaFillChanged = 0;
		OkToIncludeObject = 0;

		if (((AreaFill->Info & Mask) == Mask2) && (AreaFill->NetNr != -1)
		        && ((Layer == -1) || (AreaFill->Layer == Layer)))
		{
// ****************************************************************************

			RandValue = GetNewRandomValue(0);
			MaxClearance = max(AreaFill->Clearance, Object->Clearance);

			if (AreaFill->NetNr != NetNr)
			{
				MakePolygonFromObject(Object, PolygonObject, MaxClearance, RandValue, 1, 0);
				OkToIncludeObject = 1;
			}
			else
			{
				if (((AreaFill->Info & (AREAFILL_WITH_THERMAL_RELIEF | POWERPLANE)) == AREAFILL_WITH_THERMAL_RELIEF)
				        && (NotInRange(AreaFill->ThermalReliefDistance, 0.0))
				        && (NotInRange(AreaFill->ThermalReliefThickness, 0.0)))
				{
					memmove(&Object2, Object, sizeof(ObjectRecord));
					Object2.Clearance = 0.0;
					MakePolygonFromObject(&Object2, PolygonObject, AreaFill->ThermalReliefDistance, RandValue, 1, 0);
					OkToIncludeObject = 1;
				}
			}

// ****************************************************************************
			if ((OkToIncludeObject) && (PolygonObject->minx < AreaFill->maxx) && (PolygonObject->miny < AreaFill->maxy)
			        && (PolygonObject->maxx > AreaFill->minx) && (PolygonObject->maxy > AreaFill->miny)
			        && (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1))
			{
				if (AreaFill->NetNr != NetNr)
				{
// ****************************************************************************
					AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172);
					AllocateMemPolygons(512);
					NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
					TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
					AreaFillPos = (uint8 *) AreaFill;
					AreaFillPos += sizeof(AreaFillRecord);
					SurroundPolygon = (PolygonRecord *) AreaFillPos;
					memmove(AreaFillMemTemp2, AreaFill, AreaFill->MemSize);
					res = MergePolygon(PolygonObject, 0);
					AreaFillChanged = 1;
				}
				else
				{
// ****************************************************************************
					if ((Object->ObjectType != VIA_PUT_THROUGH_ROUND)
					        || ((AreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF) == 0))
					{
						Object->x3 = AreaFill->ThermalReliefDistance + 20.0;
						Object->x2 += 20.0;
						Object->y2 += 20.0;
						AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172);
						AllocateMemPolygons(512);
						NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
						TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
						memmove(NewAreaFill, AreaFill, AreaFill->MemSize);

						for (cnt2 = 0; cnt2 < 4; cnt2++)
						{
							if (CopyThermalReliefInPolygon
							        (Object, PolygonObject, NewAreaFill->ThermalReliefThickness, RandValue, 1, cnt2) == 0)
							{
								NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
								TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
								memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
								res = MergePolygon(PolygonObject, 1);
								AreaFillChanged = 1;
							}

							RandValue = GetNewRandomValue(0);
						}
					}
				}

// ****************************************************************************
				if (AreaFillChanged)
				{
					if (AddAreaFill(0))
					{
						AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

						if ((mode & 1) == 1)
						{
							ExitDrawing();
							EndDrawingEditingWindow(0);
						}

						AreaFill->Info |= OBJECT_NOT_VISIBLE;

						if (AreaFill->AddNr == (int16) LastActionNr)
						{
							AreaFill->DeleteNr = 0;
							AreaFill->AddNr = 0;
						}
						else
							AreaFill->DeleteNr = (int16) LastActionNr;

						if ((mode & 2) == 0)
						{
							if (OkToDrawAreaFills)
							{
								RePaint();

								if ((mode & 1) == 1)
									StartDrawingEditingWindow(0);
							}
						}
					}
				}

// ****************************************************************************
			}
		}
	}

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();

	if (AreaFillChanged)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AddPowerPlane(int32 Layer)
{
	int32 res;
	double xmin, xmax, ymin, ymax;
	NetRecord *Net;
	AreaFillRecord *BoardAreaFill, *AreaFill, *BoardOutlineAreaFill;
	PolygonRecord *AreaFillPolygon;
	char str[MAX_LENGTH_STRING];

	if (!OkToDrawAreaFills)
		return;

	AllocateMemTemp(256 * 1024);
	BoardAreaFill = (AreaFillRecord *) & TempMem[128 * 1024];
	AreaFillPolygon = (PolygonRecord *) & TempMem[128 * 1024 + sizeof(AreaFillRecord)];
	memset(BoardAreaFill, 0, sizeof(AreaFillRecord));
	memset(AreaFillPolygon, 0, sizeof(PolygonRecord));
	AreaFillPolygon->NrVertices = 4;
	BoardAreaFill->Info |= POWERPLANE | AREAFILL_WITH_THERMAL_RELIEF;
	BoardAreaFill->NrPolygons = 1;
	BoardAreaFill->Clearance = Design.StandardClearance;
	BoardAreaFill->FillType = 2;
	BoardAreaFill->ThermalReliefThickness = Design.StandardClearance;
	BoardAreaFill->ThermalReliefDistance = (12 * 2540);
	BoardAreaFill->SurroundThickNess = (100 * 2540);

	if (AreaFillDialog(BoardAreaFill, 2) == -1)
		return;

	BoardOutlineAreaFill = (AreaFillRecord *) TempMem;
	res = GetBoardOutlineAreaFill(BoardOutlineAreaFill, BoardAreaFill->SurroundThickNess, 1);

	switch (res)
	{
	case 0:
		BoardOutlineAreaFill->Info |= POWERPLANE | AREAFILL_WITH_THERMAL_RELIEF;
		BoardOutlineAreaFill->Clearance = BoardAreaFill->Clearance;
		BoardOutlineAreaFill->FillType = 2;
		BoardOutlineAreaFill->ThermalReliefThickness = BoardAreaFill->ThermalReliefThickness;
		BoardOutlineAreaFill->ThermalReliefDistance = BoardAreaFill->ThermalReliefDistance;
		BoardOutlineAreaFill->NetNr = BoardAreaFill->NetNr;
		BoardAreaFill = (AreaFillRecord *) TempMem;
		break;

	case -1:
		strcpy(str,
		       SC(1003, "Board outline (Not existing/errors) will not be used for calculating the powerplane.\n\n"));
		strcat(str, SC(1004, "Instead the initial PCB size will be used"));

		if (MessageBoxOwn(PCBWindow, str, SC(118, "Warning"), MB_APPLMODAL | MB_OK) != IDOK)
		{
			DeAllocateMemTemp();
			return;
		}

		xmin = Design.BoardOriginX + BoardAreaFill->SurroundThickNess;
		ymin = Design.BoardOriginY + BoardAreaFill->SurroundThickNess;
		xmax = Design.BoardOriginX + Design.BoardWidth - BoardAreaFill->SurroundThickNess;
		ymax = Design.BoardOriginY + Design.BoardHeight - BoardAreaFill->SurroundThickNess;
		BoardAreaFill->MemSize = sizeof(AreaFillRecord) + MemSizePolygon(AreaFillPolygon);
		AreaFillPolygon->Points[0].x = xmin;
		AreaFillPolygon->Points[0].y = ymin;
		AreaFillPolygon->Points[1].x = xmin;
		AreaFillPolygon->Points[1].y = ymax;
		AreaFillPolygon->Points[2].x = xmax;
		AreaFillPolygon->Points[2].y = ymax;
		AreaFillPolygon->Points[3].x = xmax;
		AreaFillPolygon->Points[3].y = ymin;
		SetMinMaxPolygon(AreaFillPolygon, 0);

		BoardAreaFill->NrVerticesStartPolygon = 4;
		BoardAreaFill->StartPolygon[0].x = (float) xmin;
		BoardAreaFill->StartPolygon[0].y = (float) ymin;
		BoardAreaFill->StartPolygon[1].x = (float) xmin;
		BoardAreaFill->StartPolygon[1].y = (float) ymax;
		BoardAreaFill->StartPolygon[2].x = (float) xmax;
		BoardAreaFill->StartPolygon[2].y = (float) ymax;
		BoardAreaFill->StartPolygon[3].x = (float) xmax;
		BoardAreaFill->StartPolygon[3].y = (float) ymin;

		BoardAreaFill->minx = xmin;
		BoardAreaFill->miny = ymin;
		BoardAreaFill->maxx = xmax;
		BoardAreaFill->maxy = ymax;
		break;

	case -2:
		return;
	}

	AllocateMemAreaFillMemoryTemp(3172);
	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	BoardAreaFill->Layer = Layer;
	memmove(AreaFillMemTemp, BoardAreaFill, BoardAreaFill->MemSize);
	AddAreaFill(0);

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Design.NrAreaFills - 1]]);
//  AreaFill->Info=POWERPLANE;
	AreaFill->DeleteNr = 0;
//  AreaFill->AddNr=0;

	DeAllocateMemAreaFills();
	DeAllocateMemTemp();

	if (AreaFill->NetNr >= 0)
		Net = &((*Nets)[AreaFill->NetNr]);
	else
		Net = &EmptyNet;

	if ((AreaFill->NetNr >= 0) && ((Net->Info & CONNECTIONS_DISABLED) == 0))
	{
		Net->Info ^= CONNECTIONS_DISABLED;
		DeleteAndUndisplayConnectionsNet(AreaFill->NetNr, 1);
	}

	NrObjects3 = 0;
	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RemovePowerPlane(int32 Layer)
{
	int32 cnt;
	AreaFillRecord *AreaFill;
	int32 OkToRePaint;

	OkToRePaint = 0;

	if (!OkToDrawAreaFills)
		return;

	StartDrawingEditingWindow(0);

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
		{
			ZeroUnusedObjects(0);
			AreaFill->Info |= OBJECT_NOT_VISIBLE;
			AreaFill->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			OkToRePaint = 1;
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow(0);

	if (OkToRePaint)
		RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeAreaFillPowerPlane(int32 Layer)
{
	int32 Found, res;
	AreaFillRecord *AreaFill, *AreaFill2;
	uint8 AreaFillBuf[4096];

	if (!OkToDrawAreaFills)
		return 0;

	Found = -1;
	Found = GetPowerPlaneByLayer(Layer);

	if (Found == -1)
		return -1;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
	AreaFill2 = (AreaFillRecord *) & AreaFillBuf;
	memmove(AreaFill2, AreaFill, sizeof(AreaFillRecord));
	res = ChangeAreaFillDialog(AreaFill2);

	if (res == 2)
		return -1;

	memmove(AreaFill, AreaFill2, sizeof(AreaFillRecord));
	res = 0;

	if (AreaFill->MemSize > MaxAreaFillMemoryTemp)
	{
		if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
			res = -1;
	}

	if (res == 0)
	{
		NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
		TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
		memmove(NewAreaFill, AreaFill, AreaFill->MemSize);

		if (AddAreaFill(0))
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
			AreaFill->Info |= OBJECT_NOT_VISIBLE;
			AreaFill->DeleteNr = (int16) LastActionNr;
		}
	}

	DeAllocateMemAreaFills();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeAreaFill(int32 mode)
{
	int32 cnt, res, res2;
	AreaFillRecord *AreaFill, *AreaFill2;
	int32 First = 1;
	PolygonRecord *FirstPolygon;
	uint8 AreaFillBuf[4096];

	res2 = 0;

	if (!OkToDrawAreaFills)
		return 0;

	AreaFill2 = (AreaFillRecord *) & AreaFillBuf;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
			{
				if (First)
				{
					if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
						return -1;

					NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
					TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
					memmove(AreaFill2, AreaFill, sizeof(AreaFillRecord));
					res2 = ChangeAreaFillDialog(AreaFill2);

					if (res2 == 2)
					{
						DeAllocateMemAreaFills();
						return -1;
					}

					First = 0;
				}

				res = 0;

				if (res == 0)
				{
					if (res2 == 1)
					{
						AreaFill->ThermalReliefThickness = AreaFill2->ThermalReliefThickness;
						AreaFill->ThermalReliefDistance = AreaFill2->ThermalReliefDistance;
						AreaFill->Info &= ~(AREAFILL_WITH_THERMAL_RELIEF | AREAFILL_WITH_NO_VIA_THERMAL_RELIEF);
						AreaFill->Info |=
						    (AreaFill2->Info & (AREAFILL_WITH_THERMAL_RELIEF | AREAFILL_WITH_NO_VIA_THERMAL_RELIEF));
					}

					if ((!IsLayerPowerPlane(AreaFill->Layer)) && (RebuildAreaFill(AreaFill, 0) == 1))
					{
						ZeroUnusedObjects(0);
						AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
						AreaFill->ThermalReliefThickness = AreaFill2->ThermalReliefThickness;
						AreaFill->ThermalReliefDistance = AreaFill2->ThermalReliefDistance;
						AreaFill->DeleteNr = (int16) LastActionNr;
						AreaFill->Info |= OBJECT_NOT_VISIBLE;
						RePaint();
					}
				}
			}
		}
	}

	DeAllocateMemAreaFills();
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeClearanceAreaFill(double Clearance, int32 mode)
{
	int32 cnt, Found;
	AreaFillRecord *AreaFill, *AreaFill2;
	double OldClearance;
	uint8 AreaFillBuf[4096];
	PolygonRecord *FirstPolygon;


	if (!OkToDrawAreaFills)
		return 0;

	if (mode == 1)
	{
		Found = -1;

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
			{
				FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				if ((FirstPolygon->PolygonType & 2) == 2)
				{
					if (Found == -1)
						Found = cnt;
				}
			}
		}

		if (Found != -1)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
			NewValue.Value = AreaFill->Clearance;
		}
		else
			NewValue.Value = CurrentClearance;

		NewValue.MinValue = (0.1 * 2540);
		NewValue.MaxValue = (10000 * 2540);

		if (ValueDialog(1) == 2)
			return -1;

		Clearance = NewValue.Value;
	}

	AreaFill2 = (AreaFillRecord *) & AreaFillBuf;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
			{
				OldClearance = AreaFill->Clearance;
				AreaFill->Clearance = (float) Clearance;

				if ((RebuildAreaFill(AreaFill, 0) == 1))
				{
					ZeroUnusedObjects(0);
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
					AreaFill->Clearance = (float) OldClearance;
					AreaFill->DeleteNr = (int16) LastActionNr;
					AreaFill->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}
	}

	DeAllocateMemAreaFills();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MergeAreafills(int32 mode)
{

	int32 cnt, cnt2, res, NetNr, Layer, count, NrAreaFills, pos, pos2, ok, ThermalReliefs, MergeAreaFillNrs[100];
	AreaFillRecord *AreaFill, *AreaFill3;
	double minx, maxx, miny, maxy, x1, y1, Clearance = -100.0, ThermalReliefThickness = -100.0, ThermalReliefDistance =
	            -100.0;
	PolygonRecord *AreaFillPolygon, *Polygon2, *DrawPolygon, *PolygonObject;
	int32 FoundError;
	uint8 *PolygonPos, PolygonBuf2[10240], PolygonBuf3[10240];
	PolygonRecord *FirstPolygon;
	char str[MAX_LENGTH_STRING];

	if (!OkToDrawAreaFills)
		return 0;

	NetNr = -1;
	Layer = -1;
	ThermalReliefs = -1;
	NrAreaFills = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
			{
				if (NetNr != -1)
				{
					if (NetNr != AreaFill->NetNr)
					{
						MessageBoxOwn(PCBWindow, SC(1005, "Different nets areafills"), SC(24, "Error"),
						              MB_APPLMODAL | MB_OK);
						return -1;
					}
				}
				else
					NetNr = AreaFill->NetNr;

				if (Layer != -1)
				{
					if (Layer != AreaFill->Layer)
					{
						MessageBoxOwn(PCBWindow, SC(1006, "Different layers areafills"), SC(24, "Error"),
						              MB_APPLMODAL | MB_OK);
						return -1;
					}
				}
				else
					Layer = AreaFill->Layer;

				if (Clearance < 0.0)
					Clearance = AreaFill->Clearance;
				else
				{
					if (NotInRange(Clearance, AreaFill->Clearance))
					{
						if
							(MessageBoxOwn(PCBWindow, SC(1007, "Different clearances areafills\n\nOk to continue"), SC(1, "Message"),
						         MB_APPLMODAL | MB_OKCANCEL) == IDCANCEL)
							return -1;
					}
				}

				if (ThermalReliefs != -1)
				{
					if (((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) == AREAFILL_WITH_THERMAL_RELIEF)
					        && (ThermalReliefs == 0))
					{
						strcpy(str,
						       SC(1008,
						          "Different thermal reliefs areafills\r\n\r\nUse with thermal reliefs (YES), or no thermal reliefs (NO) ?"));

						if ((res =
						            MessageBoxOwn(PCBWindow, str, SC(1, "Message"),
						                          MB_APPLMODAL | MB_YESNOCANCEL)) == IDCANCEL)
							return -1;

						if (res == IDYES)
						{
							ThermalReliefThickness = AreaFill->ThermalReliefThickness;
							ThermalReliefDistance = AreaFill->ThermalReliefDistance;
							ThermalReliefs = 1;
						}
						else
						{
						}
					}
				}
				else
				{
					ThermalReliefs = 0;

					if ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) == AREAFILL_WITH_THERMAL_RELIEF)
						ThermalReliefs = 1;
				}

				if ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) == AREAFILL_WITH_THERMAL_RELIEF)
				{
					if (ThermalReliefThickness < 0.0)
						ThermalReliefThickness = AreaFill->ThermalReliefThickness;
					else
					{
						if (NotInRange(ThermalReliefThickness, AreaFill->ThermalReliefThickness))
						{
							strcpy(str,
							       SC(1292,
							          "Different thermal reliefs areafills\r\n\r\nUse the largest thermal relief thickness?"));

							if ((res =
							            MessageBoxOwn(PCBWindow, str, SC(1, "Message"),
							                          MB_APPLMODAL | MB_YESNOCANCEL)) == IDCANCEL)
								return -1;

							if (res == IDYES)
								ThermalReliefThickness = max(ThermalReliefThickness, AreaFill->ThermalReliefThickness);
							else
								ThermalReliefThickness = min(ThermalReliefThickness, AreaFill->ThermalReliefThickness);
						}
					}

					if (ThermalReliefDistance < 0.0)
						ThermalReliefDistance = AreaFill->ThermalReliefDistance;
					else
					{
						if (NotInRange(ThermalReliefDistance, AreaFill->ThermalReliefDistance))
						{
							strcpy(str,
							       SC(1293,
							          "Different thermal reliefs areafills\r\n\r\nUse the largest thermal distance ?"));

							if ((res =
							            MessageBoxOwn(PCBWindow, str, SC(1, "Message"),
							                          MB_APPLMODAL | MB_YESNOCANCEL)) == IDCANCEL)
								return -1;

							if (res == IDYES)
								ThermalReliefDistance = max(ThermalReliefDistance, AreaFill->ThermalReliefDistance);
							else
								ThermalReliefDistance = min(ThermalReliefDistance, AreaFill->ThermalReliefDistance);
						}
					}
				}

				if (NrAreaFills < 99)
					MergeAreaFillNrs[NrAreaFills++] = cnt;
			}
		}
	}

	if (NrAreaFills < 2)
		return -2;

// ****************************************************************************
// ****************************************************************************

	DrawPolygon = (PolygonRecord *) (&PolygonBuf);

	if (AllocateMemAreaFillMemoryTemp(0) != 0)
		return 0;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	memset(NewAreaFill, 0, sizeof(AreaFillRecord));
	NewAreaFill->NetNr = (int16) NetNr;
	NewAreaFill->Layer = Layer;

	if (IsLayerPowerPlane(NewAreaFill->Layer))
		return 0;

	NewAreaFill->Clearance = (float) max(0.0, Clearance);

	if (ThermalReliefs == 1)
	{
		NewAreaFill->Info |= AREAFILL_WITH_THERMAL_RELIEF;
		NewAreaFill->ThermalReliefThickness = (float) max(0.0, ThermalReliefThickness);
		NewAreaFill->ThermalReliefDistance = (float) max(0.0, ThermalReliefDistance);
	}

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	DrawPolygon = (PolygonRecord *) & PolygonBuf3;
	DrawPolygon->NrVertices = 4;
	count = 4;
	(*DrawPolygon).Points[0].x = -1000000000.0;
	(*DrawPolygon).Points[0].y = -1000000000.0;
	(*DrawPolygon).Points[1].x = -1000000000.0;
	(*DrawPolygon).Points[1].y = 1000000000.0;
	(*DrawPolygon).Points[2].x = 1000000000.0;
	(*DrawPolygon).Points[2].y = 1000000000.0;
	(*DrawPolygon).Points[3].x = 1000000000.0;
	(*DrawPolygon).Points[3].y = -1000000000.0;
	NewAreaFill->NrPolygons = 1;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon->NrVertices = count;
	NewAreaFill->NrVerticesStartPolygon = count;

	memmove(&((*NewAreaFill).StartPolygon), &((*DrawPolygon).Points), min(200 * 8, count * 8));

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*DrawPolygon).Points[cnt].x;
		y1 = (*DrawPolygon).Points[cnt].y;
		(*AreaFillPolygon).Points[cnt].x = x1;
		(*AreaFillPolygon).Points[cnt].y = y1;
	}

	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);
	SetMinMaxPolygon(AreaFillPolygon, 0);

	FoundError = 0;

	if (AllocateMemPolygons(0) != 0)
		return 0;

	for (cnt2 = 0; cnt2 < NrAreaFills; cnt2++)
	{
		AreaFill3 = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[MergeAreaFillNrs[cnt2]]]);
		count = AreaFill3->NrVerticesStartPolygon;
		PolygonObject->NrVertices = count;

		for (cnt = 0; cnt < count; cnt++)
		{
			(*PolygonObject).Points[cnt].x = AreaFill3->StartPolygon[cnt].x + (cnt2 * 0.01);
			(*PolygonObject).Points[cnt].y = AreaFill3->StartPolygon[cnt].y + (cnt2 * 0.015);
		}

		memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
		res = MergePolygon(PolygonObject, 0);

		if (res < 0)
		{
			FoundError = 1;

			for (cnt = 0; cnt < count; cnt++)
			{
				(*PolygonObject).Points[cnt].x = AreaFill3->StartPolygon[cnt].x + (cnt2 * 0.005);
				(*PolygonObject).Points[cnt].y = AreaFill3->StartPolygon[cnt].y + (cnt2 * 0.01);
			}

			memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
			res = MergePolygon(PolygonObject, 0);
			FoundError = 0;

			if (res < 0)
				FoundError = 1;
		}

		if (FoundError)
		{
			MessageBoxOwn(PCBWindow, SC(1010, "Areafills to complex"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
			ok = 1;
		}
	}

	if (SelectionEsc)
		ok = 1;

	if (FoundError)
		return 1;

// ****************************************************************************
// ****************************************************************************

	if (NewAreaFill->NrPolygons > 2)
	{
		MessageBoxOwn(PCBWindow, SC(1011, "Areafills do not overlap"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
		DeAllocateMemPolygons();
		DeAllocateMemAreaFills();
		return -1;
	}


	Polygon2 = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) Polygon2;
	PolygonPos += MemSizePolygon(Polygon2);
	Polygon2 = (PolygonRecord *) PolygonPos;

	if (Polygon2->NrVertices > 200)
	{
		MessageBoxOwn(PCBWindow, SC(1010, "Areafills to complex"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
		DeAllocateMemPolygons();
		DeAllocateMemAreaFills();
		return -1;
	}

	NewAreaFill->NrPolygons = 0;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);
	count = Polygon2->NrVertices;
	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon->NrVertices = count;
	NewAreaFill->NrVerticesStartPolygon = count;

	minx = 1000000000.0;
	miny = 1000000000.0;
	maxx = -1000000000.0;
	maxy = -1000000000.0;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*Polygon2).Points[cnt].x;
		y1 = (*Polygon2).Points[cnt].y;
		(*NewAreaFill).StartPolygon[cnt].x = (float) x1;
		(*NewAreaFill).StartPolygon[cnt].y = (float) y1;
		minx = min(minx, x1);
		miny = min(miny, y1);
		maxx = max(maxx, x1);
		maxy = max(maxy, y1);
	}

	NewAreaFill->minx = minx;
	NewAreaFill->miny = miny;
	NewAreaFill->maxx = maxx;
	NewAreaFill->maxy = maxy;

	if (!AddAreaFill(0))
		return -1;

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();

	cnt = Design.NrAreaFills - 1;
	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
	pos = (*AreaFills)[cnt];

	if (RebuildAreaFill(AreaFill, 0) == 1)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
		pos2 = (*AreaFills)[Design.NrAreaFills - 1];
		memmove(&AreaFillMem[pos], &AreaFillMem[pos2], Design.AreaFillMem - pos2);
		Design.NrAreaFills--;
		(*AreaFills)[Design.NrAreaFills - 1] = pos;
		Design.AreaFillMem -= sizeof(AreaFillRecord);

//    AreaFill->Info|=OBJECT_NOT_VISIBLE;
//    AreaFill->DeleteNr=(int16)LastActionNr;
		ZeroUnusedObjects(0);
		for (cnt2 = 0; cnt2 < NrAreaFills; cnt2++)
		{
			AreaFill3 = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[MergeAreaFillNrs[cnt2]]]);
			AreaFill3->Info |= OBJECT_NOT_VISIBLE;
			AreaFill3->DeleteNr = (int16) LastActionNr;
		}

		if (AreaFill->NetNr != -1)
			ReCalcConnectionsNet(AreaFill->NetNr, 0, 1);

		RePaint();
	}
	else
		Design.NrAreaFills--;


	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ViewVerticesAreaFill(int32 mode)
{
	int32 cnt, cnt2, Found, count;
	AreaFillRecord *AreaFill;
	PolygonRecord *FirstPolygon, *DrawPolygon, *FoundPolygon;
	NetRecord *Net;
	int32 StartPolygon;
	uint8 *AreaPos, *PolygonPos;
	double x1a, y1a;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 ShowVertices, res;
#endif

	FoundPolygon = NULL;
	StartPolygon = 0;
	Found = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			GetLayerText(AreaFill->Layer, str2, 4);

			if ((AreaFill->Info & (POWERPLANE)) == 0)
			{
				FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				if (AreaFill->NetNr >= 0)
					Net = &((*Nets)[AreaFill->NetNr]);
				else
					Net = &EmptyNet;

				if ((FirstPolygon->PolygonType & 2) == 2)
				{
					sprintf(str3, SC(1094, "Areafill\tLayer %s\tNet %s\tNr vertices %i"), str2, Net->Name,
					        FirstPolygon->NrVertices);

					if (FoundPolygon == NULL)
					{
						FoundPolygon = FirstPolygon;
						StartPolygon = 1;
						Found = cnt;
					}
				}
				else
				{
					AreaPos = (uint8 *) AreaFill;
					DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
					PolygonPos = (uint8 *) DrawPolygon;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;

					for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
					{
						if ((DrawPolygon->PolygonType & 2) == 2)
						{
							if (FoundPolygon == NULL)
							{
								FoundPolygon = DrawPolygon;
								Found = cnt;
							}

							sprintf(str3, SC(1095, "Areafill cut out\tLayer %s\tNet %s\tNr vertices %i"), str2,
							        Net->Name, DrawPolygon->NrVertices);

							if ((DrawPolygon->PolygonType & 5) == 1)
								strcat(str3, SC(1014, " (Thermal relief)"));
						}

						PolygonPos += MemSizePolygon(DrawPolygon);
						DrawPolygon = (PolygonRecord *) PolygonPos;
					}
				}
			}
		}
	}

	if (FoundPolygon == NULL)
		return -1;

	count = FoundPolygon->NrVertices;
	MessageBufPos = 0;

	if (StartPolygon)
	{
		sprintf(str2, SC(1015, "Main polygon [ %d vertices ]"), count);

		if (AddToMessageBuf(str2) != 0)
			return 0;
	}

	str2[0] = 0;

	for (cnt = 0; cnt < min(3000, count); cnt++)
	{
		x1a = FoundPolygon->Points[cnt].x;
		y1a = FoundPolygon->Points[cnt].y;

		if (Units == 0)
			sprintf(str, "%.1f , %.1f,", x1a / 2540.0, y1a / 2540.0);
		else
			sprintf(str, "%.4f , %.4f,", x1a / 100000.0, y1a / 100000.0);

		strcat(str2, str);

		if ((cnt % 5) == 4)
		{
			if (AddToMessageBuf(str2) != 0)
				return 0;

			str2[0] = 0;
		}
	}

	if (str2[0] != 0)
	{
		if (AddToMessageBuf(str2) != 0)
			return 0;
	}

	if (count > 3000)
	{
		if (AddToMessageBuf(SC(1016, ".... (Only the first 3000 vertices are listed)\r\n")) != 0)
			return 0;
	}

	if (StartPolygon)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
		FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
		count = AreaFill->NrVerticesStartPolygon;
		sprintf(str2, SC(1017, "\r\n\r\nVertices start polygon (%d)\r\n\r\n"), count);

		if (AddToMessageBuf(str2) != 0)
			return 0;

		str2[0] = 0;

		for (cnt = 0; cnt < count; cnt++)
		{
			x1a = AreaFill->StartPolygon[cnt].x;
			y1a = AreaFill->StartPolygon[cnt].y;

			if (Units == 0)
				sprintf(str, "%.1f , %.1f,", x1a / 2540.0, y1a / 2540.0);
			else
				sprintf(str, "%.4f , %.4f,", x1a / 100000.0, y1a / 100000.0);

			strcat(str2, str);

			if ((cnt % 5) == 4)
			{
				if (AddToMessageBuf(str2) != 0)
					return 0;

				str2[0] = 0;
			}
		}

		if (str2[0] != 0)
		{
			if (AddToMessageBuf(str2) != 0)
				return 0;
		}

#ifdef _DEBUG

		if (AddToMessageBuf("\r\n\r\n") != 0)
			return 0;

		DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) DrawPolygon;
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;

		for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
		{
			ShowVertices = 0;
			count = DrawPolygon->NrVertices;

			if ((DrawPolygon->PolygonType & 8) == 8)
			{
				ShowVertices = 1;
				sprintf(str2, "\r\n\r\nUser deletion [ %d ] [ %d vertices ]", cnt2, count);

				if (AddToMessageBuf(str2) != 0)
					return 0;
			}
			else
			{
				if ((DrawPolygon->PolygonType & 5) != 0)
				{
					ShowVertices = 1;

					if ((DrawPolygon->PolygonType & 5) == 1)
						sprintf(str2, "\r\n\r\nThermal relief [ %d ] [ %d vertices ]", cnt2, count);

					if ((DrawPolygon->PolygonType & 5) == 5)
						sprintf(str2, "\r\n\r\nUser deletion (inside) [ %d ] [ %d vertices ]", cnt2, count);

					if ((DrawPolygon->PolygonType & 5) == 4)
						res = 1;

					if (AddToMessageBuf(str2) != 0)
						return 0;
				}
				else
				{
					ShowVertices = 1;

					if ((DrawPolygon->PolygonType & 5) == 0)
						sprintf(str2, "\r\n\r\nFixed deletion [ %d ] [ %d vertices ]", cnt2, count);

					if (AddToMessageBuf(str2) != 0)
						return 0;
				}
			}

			if (ShowVertices)
			{
				str2[0] = 0;

				for (cnt = 0; cnt < count; cnt++)
				{
					x1a = DrawPolygon->Points[cnt].x;
					y1a = DrawPolygon->Points[cnt].y;

					if (Units == 0)
						sprintf(str, "%.1f , %.1f,", x1a / 2540.0, y1a / 2540.0);
					else
						sprintf(str, "%.4f , %.4f,", x1a / 100000.0, y1a / 100000.0);

					strcat(str2, str);

					if ((cnt % 5) == 4)
					{
						if (AddToMessageBuf(str2) != 0)
							return 0;

						str2[0] = 0;
					}
				}

				if (str2[0] != 0)
				{
					if (AddToMessageBuf(str2) != 0)
						return 0;
				}
			}

			PolygonPos += MemSizePolygon(DrawPolygon);
			DrawPolygon = (PolygonRecord *) PolygonPos;
		}

#endif
	}

	MessageDialog(str3, 7, 0);
	DeAllocateMemMessageBuf();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ViewVerticesObjectPolygon(int32 mode)
{
	int32 cnt, count;
	double x1a, y1a;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	ObjectPolygonRecord *ObjectPolygon, *FoundPolygon;

	FoundPolygon = NULL;

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (FoundPolygon == NULL)
				FoundPolygon = ObjectPolygon;
		}
	}

	if (FoundPolygon == NULL)
		return -1;

	count = FoundPolygon->NrVertices;
	sprintf(str3, SC(1018, "Polygon: %i  Vertices"), count);
	str2[0] = 0;
	MessageBufPos = 0;

	for (cnt = 0; cnt < min(3000, count); cnt++)
	{
		x1a = FoundPolygon->Points[cnt].x;
		y1a = FoundPolygon->Points[cnt].y;

		if (Units == 0)
			sprintf(str, "%.1f , %.1f,", x1a / 2540.0, y1a / 2540.0);
		else
			sprintf(str, "%.4f , %.4f,", x1a / 100000.0, y1a / 100000.0);

		strcat(str2, str);

		if ((cnt % 5) == 4)
		{
			if (AddToMessageBuf(str2) != 0)
				return 0;

			str2[0] = 0;
		}
	}

	if (str2[0] != 0)
	{
		if (AddToMessageBuf(str2) != 0)
			return 0;
	}

	if (count > 3000)
	{
		if (AddToMessageBuf(SC(1016, ".... (Only the first 3000 vertices are listed)\r\n")) != 0)
			return 0;
	}


	MessageDialog(str3, 0, 0);
	DeAllocateMemMessageBuf();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddTryingObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double LineThickNess)
{
	int32 cnt, count;

	memset(&NewObjectLine, 0, sizeof(NewObjectLine));

	if (LineThickNess == 0.0)
		AddObjectPolygon(ObjectPolygon);
	else
	{
		NewObjectLine.LineThickNess = (float) LineThickNess;
		NewObjectLine.NetNr = ObjectPolygon->NetNr;
		NewObjectLine.Layer = ObjectPolygon->Layer;
		count = ObjectPolygon->NrVertices;

		for (cnt = 0; cnt < count; cnt++)
		{
			NewObjectLine.X1 = (float) ObjectPolygon->Points[cnt].x;
			NewObjectLine.Y1 = (float) ObjectPolygon->Points[cnt].y;

			if (cnt < count - 1)
			{
				NewObjectLine.X2 = (float) ObjectPolygon->Points[cnt + 1].x;
				NewObjectLine.Y2 = (float) ObjectPolygon->Points[cnt + 1].y;
			}
			else
			{
				NewObjectLine.X2 = (float) ObjectPolygon->Points[0].x;
				NewObjectLine.Y2 = (float) ObjectPolygon->Points[0].y;
			}

			AddObjectLine(&NewObjectLine);
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectPolygonLinesDrawFunc1(int32 Mode2)
{
	DrawTryingPolygonObject(OldX, OldY, Mode2);
	DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectPolygonLinesDrawFunc2(int32 Mode2)
{
	double NewX, NewY;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if (!LinesAllDirection)
	{
		ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
		CurrentX = NewX;
		CurrentY = NewY;
	}

	OldX = CurrentX;
	OldY = CurrentY;

	if (!SelectionEsc)
	{
		DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);
		DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddObjectPolygon(double LineThickNess, int32 Layer, int32 Mode)
{
	int32 cnt, cnt2, cnt3, ok, InsertMode, res, OkToAdd, Mode2, count, count2;
	double x1, y1, NewX, NewY, RandValue;
	char TextLine[2048], str[500], str3[MAX_LENGTH_STRING];
	LPSTR TextP, TextP2;
	int32 LeftButtonFinishPolygon = 0;
	PolygonRecord *DrawPolygon2;
	DrawXorFunctionRecord DrawXorFunction;

	strcpy(str3, InfoStr);

	SelectionEsc = 0;
	FinishPolygon = 0;

	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	NewObjectLine.LineThickNess = (float) LineThickNess;
	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	if (!LinesAllDirection)
	{
		ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
		CurrentX = NewX;
		CurrentY = NewY;
	}

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	x1 = CurrentX;
	y1 = CurrentY;
	OkToAdd = 0;
	Mode2 = 0;

	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
	memset(DrawPolygon2, 0, sizeof(PolygonRecord));
	PolygonObjectType = OBJECT_POLYLINE;

	OldX = CurrentX;
	OldY = CurrentY;
	InsertMode = 0;
	count2 = 0;
	ClipMouseCursor();

	StartPolygon = 1;
	SystemBusyMode = 110;
	DrawXorFunction.Function4a = (FUNCP4) ObjectPolygonLinesDrawFunc1;
	DrawXorFunction.Function4b = (FUNCP4) ObjectPolygonLinesDrawFunc2;
	DrawXorFunction.Param1[0] = &Mode2;
	DrawXorFunction.Mode = 3;
	DrawXorFunction.Param2[0] = &Mode2;
	ZoomInOutProcessed = 0;

	while ((!SelectionEsc) && (!FinishPolygon))
	{
// ****************************************************************************
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((Mode2 > 0) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingPolygonObject(OldX, OldY, Mode2);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
			}

// ****************************************************************************
			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				ObjectPolygonLinesDrawFunc1(Mode2);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				ObjectPolygonLinesDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				ObjectPolygonLinesDrawFunc1(Mode2);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				ObjectPolygonLinesDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				ObjectPolygonLinesDrawFunc1(Mode2);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				ObjectPolygonLinesDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				ObjectPolygonLinesDrawFunc1(Mode2);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				ObjectPolygonLinesDrawFunc2(Mode2);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);
// ****************************************************************************

		if (ZoomInOutProcessed)
		{
			ObjectPolygonLinesDrawFunc2(Mode2);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			ObjectPolygonLinesDrawFunc1(Mode2);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();
			ObjectPolygonLinesDrawFunc2(Mode2);
		}

// ****************************************************************************
		if ((ZoomActive()) && (!SelectionEsc))
		{
			ObjectPolygonLinesDrawFunc1(Mode2);
			ZoomWindow();
			ObjectPolygonLinesDrawFunc2(Mode2);
		}

// ****************************************************************************
		if ((PanActive()) && (!SelectionEsc))
		{
			ObjectPolygonLinesDrawFunc1(Mode2);
			PanWindow();
			ObjectPolygonLinesDrawFunc2(Mode2);
		}

// ****************************************************************************
		if (CheckLeftButton())
		{
			DrawTryingPolygonObject(OldX, OldY, Mode2);
			RandValue = GetNewRandomValue(0);
			
			if (Mode2 > 0)
			{
				if ((NotInRange(NewObjectLine.X1, NewObjectLine.X2))
				        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
				{
					CommandAddTryingPolygonLine();

					if ((DrawPolygon2->NrVertices > 1) && (InRange(FirstX, CurrentX)) && (InRange(FirstY, CurrentY)))
					{
						FinishPolygon = 1;
						LeftButtonFinishPolygon = 1;
						InsertMode = 1;
					}
				}

				CurrentX2 = CurrentX;
				CurrentY2 = CurrentY;
			}
			else
			{
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				CurrentX2 = CurrentX;
				CurrentY2 = CurrentY;
				FirstX = CurrentX2;
				FirstY = CurrentY2;

				if (!LinesAllDirection)
				{
					ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
			}
			
			//Mode2++;
			Mode2 = min(1, Mode2 + 1);

			if (!FinishPolygon)
			{
//        DrawTryingPolygon(0);
				DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
			}

			CheckInputMessages(0);
		}

// ****************************************************************************
		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingPolygonObject(OldX, OldY, Mode2);
			PopUpMenu = CreatePopupMenu();

			if (DrawPolygon2->NrVertices > 1)
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_FINISH_POLYGON, SC(990, "Finish"));

			if (LinesAllDirection)
			{
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_45_DIR,
				              SC(991, "Draw with 45/90 degrees directions"));
			}
			else
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_ALL_DIR, SC(992, "Draw in all directions"));

			if (DrawPolygon2->NrVertices > 0)
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_TRACE_BACKWARDS, SC(993, "Goto previous point"));

			TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
			               RealWindow.top + MousePosY + 40, 0, PCBWindow, NULL);
			DestroyMenu(PopUpMenu);
//      RightButtonPressed=0;
			CheckInputMessages(0);

			if (!SelectionEsc)
				DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

			ok = 1;
		}

// ****************************************************************************
		if (NrFunctionsInBuf > 0)
		{
			ObjectPolygonLinesDrawFunc1(Mode2);
			ClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TextLine, 0, sizeof(TextLine));
				AllocateSpecialMem(MEM_NET_SELECTED, 32 * 1024, (void **) &TextP);
				*TextP = 0;

				if (LineThickNess == 0.0)
					strcpy(str, SC(1019, "Add polygon points (x1,y1,x2,y2,x3,y3,x4,y4, .... )"));
				else
					strcpy(str, SC(1107, "Add polyline points (x1,y1,x2,y2,x3,y3,x4,y4, .... )"));

				if (TextInputDialog2(TextP, str, 0) == 1)
				{
					int32 polyParametersRelative = 0;

					CheckInputMessages(200);
					cnt = 0;
					DrawPolygon2->NrVertices = 0;

					while ((*TextP != 0) && (DrawPolygon2->NrVertices < 200))
					{
						TextP2 = TextP;
						memset(&str, 0, sizeof(str));
						cnt3 = 0;

						while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
						{
							str[cnt3++] = *TextP;
							str[cnt3] = 0;
							TextP++;
						}

						if (*TextP == ',')
							TextP++;

						if (*TextP == '\r')
							TextP++;

						if (*TextP != 0)
						{
							str[cnt3++] = ',';
							str[cnt3] = 0;

							while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
							{
								str[cnt3++] = *TextP;
								str[cnt3] = 0;
								TextP++;
							}

							if (*TextP == ',')
								TextP++;

							if (*TextP == '\r')
								TextP++;

							if (((NrParams = ScanParameters(-1, str, 0)) >= 2) && ((NrParams & 1) == 0))
							{
								if (ParametersRelative)
									polyParametersRelative = 1;

								for (cnt2 = 0; cnt2 < NrParams / 2; cnt2++)
								{
									if (!polyParametersRelative)
									{
										if (DrawPolygon2->NrVertices < 200)
										{
											(*DrawPolygon2).Points[cnt].x = ParamsFloat[cnt2 * 2];
											(*DrawPolygon2).Points[cnt].y = ParamsFloat[cnt2 * 2 + 1];
											cnt++;
											DrawPolygon2->NrVertices++;
										}
									}
									else
									{
										if (DrawPolygon2->NrVertices < 200)
										{
											(*DrawPolygon2).Points[cnt].x = RelX + ParamsFloat[cnt2 * 2];
											(*DrawPolygon2).Points[cnt].y = RelY + ParamsFloat[cnt2 * 2 + 1];
											cnt++;
											DrawPolygon2->NrVertices++;
										}
									}
								}
							}
						}
					}

					if (DrawPolygon2->NrVertices > 2)
					{
						FinishPolygon = 1;
						InsertMode = 2;
					}
				}

				DeallocateSpecialMem(MEM_NET_SELECTED);
				SpacePressed = 0;
			}

			if (TraceBackWardsKeyPressed)
			{
				if (DrawPolygon2->NrVertices > 0)
				{
					count = DrawPolygon2->NrVertices;
					CurrentX2 = (*DrawPolygon2).Points[count - 1].x;
					CurrentY2 = (*DrawPolygon2).Points[count - 1].y;
					DrawPolygon2->NrVertices--;
				}

				TraceBackWardsKeyPressed = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if (!LinesAllDirection)
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (HelpAsked)
			{
				switch (Mode)
				{
				case 0:
//            Help(IDH_Add_areafills,0);
					break;
				}

				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			if (SelectionEsc)
			{
				NewObjectLine.X2 = (float) x1;
				NewObjectLine.Y2 = (float) y1;
				DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 1);
			}
			else
			{
				if (InsertMode < 2)
				{
					DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				}
			}

			ClipMouseCursor();
		}
	}

// ****************************************************************************

	if (FinishPolygon)
	{
		if (InsertMode == 0)
		{
			DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

			if ((!LeftButtonFinishPolygon) || (NotInRange(NewObjectLine.X1, NewObjectLine.X2))
			        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
				CommandAddTryingPolygonLine();
		}

		if (InsertMode < 2)
		{
			if (DrawPolygon2->NrVertices > 2)
			{
				DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 1);
				OkToAdd = 1;
			}
		}
		else
			OkToAdd = 1;
	}

	UnClipMouseCursor();
	sprintf(InfoStr, str3);
	RedrawInfoStr(1);
	SystemBusyMode = 0;

	if (OkToAdd == 1)
	{
		SetMinMaxPolygon(DrawPolygon2, 0);
		memset(&NewObjectPolygon, 0, sizeof(NewObjectPolygon));
		NewObjectPolygon.NrVertices = DrawPolygon2->NrVertices;
		NewObjectPolygon.minx = DrawPolygon2->minx;
		NewObjectPolygon.maxx = DrawPolygon2->maxx;
		NewObjectPolygon.miny = DrawPolygon2->miny;
		NewObjectPolygon.maxy = DrawPolygon2->maxy;
		NewObjectPolygon.Layer = Layer;

		if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			NewObjectPolygon.NetNr = -2;
		else
			NewObjectPolygon.NetNr = -1;

		memmove(&NewObjectPolygon.Points, &((*DrawPolygon2).Points), DrawPolygon2->NrVertices * sizeof(PointRecord));
		CommandAddTryingObjectPolygon(&NewObjectPolygon, LineThickNess);
		res = 1;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 CutFromPolygonDrawFunc1(int32 Mode2)
{
	if ((!ShiftPressed) || (PolygonDrawObjectType == OBJECT_POLYLINE))
		DrawTryingPolygonObject(OldX, OldY, Mode2);
	else
		DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);

	if (PolygonDrawObjectType == OBJECT_POLYLINE)
		DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CutFromPolygonDrawFunc2(int32 Mode2)
{
	double NewX, NewY;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if ((PolygonDrawObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
	{
		ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
		CurrentX = NewX;
		CurrentY = NewY;
	}

	OldX = CurrentX;
	OldY = CurrentY;

	if (PolygonDrawObjectType == OBJECT_POLYLINE)
		DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);

	if ((!ShiftPressed) || (PolygonDrawObjectType == OBJECT_POLYLINE))
		DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
	else
		DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandCutFromPolygon(int32 ObjectType)
{
	ObjectPolygonRecord *ObjectPolygon;
	int32 cnt, cnt2, cnt3, ok, InsertMode, OkToAdd, Mode2, count, FoundPolygonNr, count2, CircleRoundings = 24;
	double NewX, NewY, x1, y1, OldShiftX, OldShiftY, x5, y5, hoek, RandValue;
	char TextLine[2048], str[500], str3[MAX_LENGTH_STRING];
	LPSTR TextP, TextP2;
	int32 FirstShift = 1, LeftButtonFinishPolygon = 0;
	PolygonRecord *DrawPolygon2;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;
	OldShiftX = 0.0;
	OldShiftY = 0.0;
	RandValue = 0.0;
	FoundPolygonNr = -1;

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (FoundPolygonNr == -1)
				FoundPolygonNr = cnt;
		}
	}

	if (FoundPolygonNr == -1)
		return;

	strcpy(str3, InfoStr);
	PolygonObjectType = ObjectType;
	OldValue2 = 0.1123;

	SelectionEsc = 0;
	FinishPolygon = 0;
//  CurrentDrawMode=1;

	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	if ((ObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
	{
		ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
		CurrentX = NewX;
		CurrentY = NewY;
	}

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	switch (ObjectType)
	{
	case OBJECT_POLYLINE:
		break;

	case OBJECT_RECT:
		AreaFillObjectRect.CentreX = (float) CurrentX2;
		AreaFillObjectRect.CentreY = (float) CurrentY2;
		break;

	case OBJECT_CIRCLE:
		AreaFillObjectArc.CentreX = (float) CurrentX2;
		AreaFillObjectArc.CentreY = (float) CurrentX2;
		break;

	case PIN_LINE_HOR:
		AreaFillObjectHorLine.X1 = (float) CurrentX2;
		AreaFillObjectHorLine.Y1 = (float) CurrentY2;
		break;

	case PIN_LINE_VER:
		AreaFillObjectVerLine.X1 = (float) CurrentX2;
		AreaFillObjectVerLine.Y1 = (float) CurrentY2;
		break;
	}

	PolygonDrawObjectType = ObjectType;
	x1 = CurrentX;
	y1 = CurrentY;
	OkToAdd = 0;
	Mode2 = 0;

	DrawPolygon2 = (PolygonRecord *) (&PolygonBuf);
	memset(DrawPolygon2, 0, sizeof(PolygonRecord));
	/*
	  if ((Mode==1)
	     &&
	     (StartPolygon==1)) {
	    CurrentPolygon=(PolygonRecord *)&PolygonBuf2;
	    CopyPolygonToPolygon(CurrentPolygon,DrawPolygon);
	    MakeNewAreaFill();
	    return;
	  }

	  DrawPolygon->Points[0].x=1422412;
	  DrawPolygon->Points[0].y=6395731;
	  DrawPolygon->Points[1].x=6131570;
	  DrawPolygon->Points[1].y=6395729;
	  DrawPolygon->Points[2].x=6131570;
	  DrawPolygon->Points[2].y=1859289;
	  DrawPolygon->Points[3].x=1422410;
	  DrawPolygon->Points[3].y=1859289;


	  DrawPolygon->NrVertices=4;
	  MakeNewAreaFill();
	  return;
	*/


	OldX = CurrentX;
	OldY = CurrentY;
	InsertMode = 0;
	count2 = 0;
	ClipMouseCursor();

	NewObjectLine.LineThickNess = 0.0;
	StartPolygon = 1;

	if (ObjectType != OBJECT_POLYLINE)
		DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

	str[0] = 0;
	GetSizeCutoutObjectAreaFill(str, ObjectType);
	SystemBusyMode = 120;
	DrawXorFunction.Function4a = (FUNCP4) CutFromPolygonDrawFunc1;
	DrawXorFunction.Function4b = (FUNCP4) CutFromPolygonDrawFunc2;
	DrawXorFunction.Param1[0] = &Mode2;
	DrawXorFunction.Mode = 3;
	DrawXorFunction.Param2[0] = &Mode2;
	ZoomInOutProcessed = 0;

	while ((!SelectionEsc) && (!FinishPolygon))
	{
// ****************************************************************************
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((ObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if ((!ShiftPressed) || (ObjectType == OBJECT_POLYLINE))
					DrawTryingPolygonObject(OldX, OldY, Mode2);
				else
					DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				else
				{
					switch (ObjectType)
					{
					case OBJECT_POLYLINE:
						break;

					case OBJECT_RECT:
//              AreaFillObjectRect.Width=fabs(OldWidth-(ShiftX-CurrentX)*1.0);
//              AreaFillObjectRect.Height=fabs(OldHeight-(ShiftY-CurrentY)*1.0);
						AreaFillObjectRect.Width = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectRect.Height = (float) fabs((ShiftY - CurrentY) * 2.0);
						break;

					case OBJECT_CIRCLE:
						AreaFillObjectArc.Width = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectArc.Height = (float) fabs((ShiftX - CurrentX) * 2.0);
						break;

					case PIN_LINE_HOR:
						AreaFillObjectHorLine.X2 = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectHorLine.Y2 = (float) fabs((ShiftY - CurrentY) * 2.0);
						break;

					case PIN_LINE_VER:
						AreaFillObjectVerLine.X2 = (float) fabs((ShiftX - CurrentX) * 2.0);
						AreaFillObjectVerLine.Y2 = (float) fabs((ShiftY - CurrentY) * 2.0);
						break;
					}

					str[0] = 0;
					GetSizeCutoutObjectAreaFill(str, ObjectType);
					DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);
				}
			}

// ****************************************************************************
			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				CutFromPolygonDrawFunc1(Mode2);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CutFromPolygonDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				CutFromPolygonDrawFunc1(Mode2);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CutFromPolygonDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				CutFromPolygonDrawFunc1(Mode2);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CutFromPolygonDrawFunc2(Mode2);
			}

// ****************************************************************************
			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				CutFromPolygonDrawFunc1(Mode2);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CutFromPolygonDrawFunc2(Mode2);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

// ****************************************************************************
		if (ObjectType != OBJECT_POLYLINE)
		{
			if (!ShiftPressed)
			{
				if (!FirstShift)
				{
					DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);
					FirstShift = 1;
					DisplayCursorPosition();
					CurrentX = OldShiftX;
					CurrentY = OldShiftY;
					SetNewCursor(&CurrentX, &CurrentY);
					OldX = CurrentX;
					OldY = CurrentY;
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				}
			}
			else
			{
				if (FirstShift)
				{
					ShiftX = CurrentX;
					ShiftY = CurrentY;
					OldShiftX = CurrentX;
					OldShiftY = CurrentY;
					FirstShift = 0;

					switch (ObjectType)
					{
					case OBJECT_RECT:
						CurrentX += AreaFillObjectRect.Height * 0.5;
						CurrentY -= AreaFillObjectRect.Width * 0.5;
						break;

					case OBJECT_ARC:
						CurrentX += AreaFillObjectArc.Width * 0.5;
						CurrentY -= AreaFillObjectArc.Width * 0.5;
						break;

					case PIN_LINE_HOR:
						CurrentX += AreaFillObjectHorLine.X2 * 0.5;
						CurrentY -= AreaFillObjectHorLine.Y2 * 0.5;
						break;

					case PIN_LINE_VER:
						CurrentX += AreaFillObjectVerLine.X2 * 0.5;
						CurrentY -= AreaFillObjectVerLine.Y2 * 0.5;
						break;
					}

					SetNewCursor(&CurrentX, &CurrentY);
					OldX = CurrentX;
					OldY = CurrentY;
				}
				else
				{
				}
			}
		}

		CheckInputMessages(0);
// ****************************************************************************

		if (ZoomInOutProcessed)
		{
			CutFromPolygonDrawFunc2(Mode2);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			CutFromPolygonDrawFunc1(Mode2);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();
			CutFromPolygonDrawFunc2(Mode2);
		}

// ****************************************************************************
		if ((ZoomActive()) && (!SelectionEsc))
		{
			CutFromPolygonDrawFunc1(Mode2);
			ZoomWindow();
			CutFromPolygonDrawFunc2(Mode2);
		}

// ****************************************************************************
		if ((PanActive()) && (!SelectionEsc))
		{
			CutFromPolygonDrawFunc1(Mode2);
			PanWindow();
			CutFromPolygonDrawFunc2(Mode2);
		}

// ****************************************************************************
		if (CheckLeftButton())
		{
			DrawTryingPolygonObject(OldX, OldY, Mode2);
			RandValue = GetNewRandomValue(0);
			
			switch (ObjectType)
			{
			case OBJECT_POLYLINE:
				if (Mode2 > 0)
				{
					if ((NotInRange(NewObjectLine.X1, NewObjectLine.X2))
					        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
					{
						CommandAddTryingPolygonLine();

						if ((DrawPolygon2->NrVertices > 1) && (InRange(FirstX, CurrentX))
						        && (InRange(FirstY, CurrentY)))
						{
							FinishPolygon = 1;
							LeftButtonFinishPolygon = 1;
							InsertMode = 1;
						}
					}

					CurrentX2 = CurrentX;
					CurrentY2 = CurrentY;
				}
				else
				{
					CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
					CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
					CurrentX2 = CurrentX;
					CurrentY2 = CurrentY;
					FirstX = CurrentX2;
					FirstY = CurrentY2;

					if ((ObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
					{
						ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
						CurrentX = NewX;
						CurrentY = NewY;
					}

					OldX = CurrentX;
					OldY = CurrentY;
				}

				Mode2++;

				if (!FinishPolygon)
				{
					//        DrawTryingPolygon(0);
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				}

				break;

			case OBJECT_RECT:
			case OBJECT_CIRCLE:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
				switch (ObjectType)
				{
				case OBJECT_RECT:
					x5 = AreaFillObjectRect.Width * 0.5;
					x5 += RandValue;
					y5 = AreaFillObjectRect.Height * 0.5;
					y5 += RandValue;
					DrawPolygon2->Points[0].x = AreaFillObjectRect.CentreX + x5;
					DrawPolygon2->Points[0].y = AreaFillObjectRect.CentreY + y5;
					DrawPolygon2->Points[1].x = AreaFillObjectRect.CentreX + x5;
					DrawPolygon2->Points[1].y = AreaFillObjectRect.CentreY - y5;
					DrawPolygon2->Points[2].x = AreaFillObjectRect.CentreX - x5;
					DrawPolygon2->Points[2].y = AreaFillObjectRect.CentreY - y5;
					DrawPolygon2->Points[3].x = AreaFillObjectRect.CentreX - x5;
					DrawPolygon2->Points[3].y = AreaFillObjectRect.CentreY + y5;
					DrawPolygon2->NrVertices = 4;

					if ((InRange(AreaFillObjectRect.Width, 0.0)) || (InRange(AreaFillObjectRect.Height, 0.0)))
						DrawPolygon2->NrVertices = 0;

					break;

				case OBJECT_CIRCLE:
					for (cnt = 0; cnt < CircleRoundings; cnt++)
					{
						DrawPolygon2->Points[cnt].x = AreaFillObjectArc.CentreX;
						DrawPolygon2->Points[cnt].y = AreaFillObjectArc.CentreY;
						hoek = cnt;
						hoek *= ANGLE_360 / CircleRoundings;
						x5 = cos(hoek);
						x5 *= AreaFillObjectArc.Width * 0.5;
						x5 += RandValue;
						y5 = sin(hoek);
						y5 *= AreaFillObjectArc.Width * 0.5;
						y5 += RandValue;
						DrawPolygon2->Points[cnt].x += x5;
						DrawPolygon2->Points[cnt].y += y5;
					}

					DrawPolygon2->NrVertices = CircleRoundings;

					if (InRange(AreaFillObjectArc.Width, 0.0))
						DrawPolygon2->NrVertices = 0;

					break;

				case PIN_LINE_HOR:
					cnt2 = 0;

					for (cnt = CircleRoundings / 4; cnt < ((CircleRoundings * 5) / 4) + 1; cnt++)
					{
						hoek = cnt;
						hoek *= ANGLE_360 / CircleRoundings;
						x5 = cos(hoek);
						x5 *= AreaFillObjectHorLine.Y2 * 0.5;
//                x5+=AreaFillDeletionCount*24.5;
						y5 = sin(hoek);
						y5 *= AreaFillObjectHorLine.Y2 * 0.5;

//                y5+=AreaFillDeletionCount*24.5;
						if (cnt == ((CircleRoundings * 3) / 4))
						{
							DrawPolygon2->Points[cnt2].x = AreaFillObjectHorLine.X1;
							DrawPolygon2->Points[cnt2].y = AreaFillObjectHorLine.Y1;
							DrawPolygon2->Points[cnt2].x += x5;
							DrawPolygon2->Points[cnt2].y += y5;
							cnt2++;
						}

						DrawPolygon2->Points[cnt2].x = AreaFillObjectHorLine.X1;
						DrawPolygon2->Points[cnt2].y = AreaFillObjectHorLine.Y1;

						if (cnt >= ((CircleRoundings * 3) / 4))
							DrawPolygon2->Points[cnt2].x += AreaFillObjectHorLine.X2;

						DrawPolygon2->Points[cnt2].x += x5;
						DrawPolygon2->Points[cnt2].y += y5;
						cnt2++;
					}

					DrawPolygon2->NrVertices = CircleRoundings + 2;

					if (InRange(AreaFillObjectHorLine.Y2, 0.0))
						DrawPolygon2->NrVertices = 0;

					break;

				case PIN_LINE_VER:
					cnt2 = 0;

					for (cnt = 0; cnt < CircleRoundings + 1; cnt++)
					{
						hoek = cnt;
						hoek *= ANGLE_360 / CircleRoundings;
						x5 = cos(hoek);
						x5 *= AreaFillObjectVerLine.X2 * 0.5;
						x5 += RandValue;
						y5 = sin(hoek);
						y5 *= AreaFillObjectVerLine.X2 * 0.5;
						y5 += RandValue;

						if (cnt == (CircleRoundings * 2) / 4)
						{
							DrawPolygon2->Points[cnt2].x = AreaFillObjectVerLine.X1;
							DrawPolygon2->Points[cnt2].y = AreaFillObjectVerLine.Y1;
							DrawPolygon2->Points[cnt2].y += AreaFillObjectVerLine.Y2;
							DrawPolygon2->Points[cnt2].x += x5;
							DrawPolygon2->Points[cnt2].y += y5;
							cnt2++;
						}

						DrawPolygon2->Points[cnt2].x = AreaFillObjectVerLine.X1;
						DrawPolygon2->Points[cnt2].y = AreaFillObjectVerLine.Y1;

						if (cnt < ((CircleRoundings * 2) / 4))
							DrawPolygon2->Points[cnt2].y += AreaFillObjectVerLine.Y2;

						DrawPolygon2->Points[cnt2].x += x5;
						DrawPolygon2->Points[cnt2].y += y5;
						cnt2++;
					}

					DrawPolygon2->NrVertices = CircleRoundings + 2;

					if (InRange(AreaFillObjectVerLine.Y2, 0.0))
						DrawPolygon2->NrVertices = 0;

					break;
				}

				if (DrawPolygon2->NrVertices > 0)
				{
					if (DeleteFromObjectPolygon(FoundPolygonNr, 0) == 0)
					{
						CheckInputMessages(0);
						DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
						FoundPolygonNr = Design.NrObjectPolygons - 1;
//              AreaFillDeletionCount++;
						LastActionNr++;
						count2++;
					}
					else
						SelectionEsc = 1;
				}
				else
				{
					Beep(1000, 200);
					DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
				}

				break;
			}

			CheckInputMessages(0);
		}

// ****************************************************************************
		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingPolygonObject(OldX, OldY, Mode2);

			switch (ObjectType)
			{
			case OBJECT_POLYLINE:
				PopUpMenu = CreatePopupMenu();

				if (DrawPolygon2->NrVertices > 1)
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_FINISH_POLYGON, SC(990, "Finish"));

				if (LinesAllDirection)
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_45_DIR,
					              SC(991, "Draw with 45/90 degrees directions"));
				}
				else
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_ALL_DIR,
					              SC(992, "Draw in all directions"));
				}

				if (DrawPolygon2->NrVertices > 0)
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_TRACE_BACKWARDS,
					              SC(993, "Goto previous point"));
				}

				break;

			case OBJECT_RECT:
			case OBJECT_CIRCLE:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
				PopUpMenu = CreatePopupMenu();
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(493, "Escape"));
				break;
			}

			TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
			               RealWindow.top + MousePosY + 40, 0, PCBWindow, NULL);
			DestroyMenu(PopUpMenu);
//      RightButtonPressed=0;
			CheckInputMessages(0);

			if (!SelectionEsc)
				DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

			ok = 1;
		}

// ****************************************************************************
		if (NrFunctionsInBuf > 0)
		{
			CutFromPolygonDrawFunc1(Mode2);
			ClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				switch (ObjectType)
				{
				case OBJECT_POLYLINE:
					memset(&TextLine, 0, sizeof(TextLine));
					AllocateSpecialMem(MEM_NET_SELECTED, 32 * 1024, (void **) &TextP);
					*TextP = 0;

					if (TextInputDialog2(TextP, SC(1019, "Add polygon points (x1,y1,x2,y2,x3,y3,x4,y4, .... )"), 0) == 1)
					{
						CheckInputMessages(200);
						cnt = 0;
						DrawPolygon2->NrVertices = 0;

						while ((*TextP != 0) && (DrawPolygon2->NrVertices < 200))
						{
							TextP2 = TextP;
							memset(&str, 0, sizeof(str));
							cnt3 = 0;

							while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
							{
								str[cnt3++] = *TextP;
								str[cnt3] = 0;
								TextP++;
							}

							if (*TextP == ',')
								TextP++;

							if (*TextP == '\r')
								TextP++;

							if (*TextP != 0)
							{
								str[cnt3++] = ',';
								str[cnt3] = 0;

								while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
								{
									str[cnt3++] = *TextP;
									str[cnt3] = 0;
									TextP++;
								}

								if (*TextP == ',')
									TextP++;

								if (*TextP == '\r')
									TextP++;

								if (((NrParams = ScanParameters(-1, str, 0)) >= 2) && ((NrParams & 1) == 0))
								{
									for (cnt2 = 0; cnt2 < NrParams / 2; cnt2++)
									{
										if (DrawPolygon2->NrVertices < 200)
										{ //fixme
											(*DrawPolygon2).Points[cnt].x = ParamsFloat[cnt2 * 2];
											(*DrawPolygon2).Points[cnt].y = ParamsFloat[cnt2 * 2 + 1];
											cnt++;
											DrawPolygon2->NrVertices++;
										}
									}
								}
							}
						}

						if (DrawPolygon2->NrVertices > 2)
						{
							FinishPolygon = 1;
							InsertMode = 2;
						}
					}

					DeallocateSpecialMem(MEM_NET_SELECTED);
					break;

				case OBJECT_RECT:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(995, "Rectangle size width,height")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 2)
						{
							AreaFillObjectRect.Width = (float) ParamsFloat[0];
							AreaFillObjectRect.Height = (float) ParamsFloat[1];
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
						}
					}

					break;

				case OBJECT_ARC:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(996, "Circle diameter")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 1)
						{
							AreaFillObjectArc.Width = (float) ParamsFloat[0];
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
						}
					}

					break;

				case PIN_LINE_HOR:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(997, "Horizontal trace size width,height")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 2)
						{
							AreaFillObjectHorLine.X2 = (float) max(0.0, ParamsFloat[0] - ParamsFloat[1]);
							AreaFillObjectHorLine.Y2 = (float) ParamsFloat[1];
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
						}
					}

					break;

				case PIN_LINE_VER:
					memset(&TextLine, 0, sizeof(TextLine));

					if (LineInputDialogLong(TextLine, SC(998, "Vertical trace size width,height")) == 1)
					{
						if ((NrParams = ScanParameters(-1, TextLine, 0)) == 2)
						{
							AreaFillObjectVerLine.X2 = (float) ParamsFloat[0];
							AreaFillObjectVerLine.Y2 = (float) max(0.0, ParamsFloat[1] - ParamsFloat[0]);
							str[0] = 0;
							GetSizeCutoutObjectAreaFill(str, ObjectType);
						}
					}

					break;
				}

				SpacePressed = 0;
			}

			if (TraceBackWardsKeyPressed)
			{
				if (ObjectType == OBJECT_POLYLINE)
				{
					if (DrawPolygon2->NrVertices > 0)
					{
						count = DrawPolygon2->NrVertices;
						CurrentX2 = (*DrawPolygon2).Points[count - 1].x;
						CurrentY2 = (*DrawPolygon2).Points[count - 1].y;
						DrawPolygon2->NrVertices--;
					}
				}

				TraceBackWardsKeyPressed = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((ObjectType == OBJECT_POLYLINE) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (HelpAsked)
			{
//        Help(IDH_Add_areafills,0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			if (SelectionEsc)
			{
				NewObjectLine.X2 = (float) x1;
				NewObjectLine.Y2 = (float) y1;

				if (ObjectType == OBJECT_POLYLINE)
					DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 1);
			}
			else
			{
				switch (ObjectType)
				{
				case OBJECT_POLYLINE:
					if (InsertMode < 2)
					{
						DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 0);
						DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
					}

					break;

				case OBJECT_RECT:
				case OBJECT_CIRCLE:
				case PIN_LINE_HOR:
				case PIN_LINE_VER:
					if (!ShiftPressed)
						DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);
					else
						DrawTryingPolygonObject(ShiftX, ShiftY, Mode2);

					break;
				}
			}

			ClipMouseCursor();
		}
	}

// ****************************************************************************

	switch (ObjectType)
	{
	case OBJECT_POLYLINE:
		if (FinishPolygon)
		{
			if (InsertMode == 0)
			{
				DrawTryingPolygonObject(CurrentX, CurrentY, Mode2);

				if ((!LeftButtonFinishPolygon) || (NotInRange(NewObjectLine.X1, NewObjectLine.X2))
				        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
					CommandAddTryingPolygonLine();
			}

			if (InsertMode < 2)
			{
				if (DrawPolygon2->NrVertices > 2)
				{
					if (ObjectType == OBJECT_POLYLINE)
						DrawTryingPolygon(Mult(NewObjectLine.LineThickNess), 1);

					OkToAdd = 1;
				}
			}
			else
				OkToAdd = 1;

			if (OkToAdd == 1)
			{
				for (cnt = 0; cnt < DrawPolygon2->NrVertices; cnt++)
				{
					DrawPolygon2->Points[cnt].x += RandValue;
					DrawPolygon2->Points[cnt].y += RandValue;
				}

				DeleteFromObjectPolygon(FoundPolygonNr, 0);
			}
		}

		break;

	case OBJECT_RECT:
	case OBJECT_CIRCLE:
		break;
	}

	UnClipMouseCursor();
	sprintf(InfoStr, str3);
	RedrawInfoStr(1);

	if (count2 > 0)
		LastActionNr--;

	SystemBusyMode = 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ViewAreafillPoint()
{
	int32 cnt, cnt3, res, res2, count, count2, MemSize;
	double xx1, yy1, xx2, yy2, ThickNess, x11, y11, x22, y22, Length;
	PolygonRecord *ChangedPolygon, *DrawPolygon;
	uint8 *PolygonPos;
	AreaFillRecord *AreaFill, *AreaFill2;
#ifdef _DEBUG
	int32 ok;
	double NewLength;
#endif

	AreaFill2 = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((DrawPolygon->PolygonType & 2) == 2)
			{
				if (AreaFill2 == 0)
					AreaFill2 = AreaFill;
			}
		}
	}

	if (AreaFill2 == 0)
		return;

	StartDrawingEditingWindow(0);
//  InitDrawingShapePinsTop();
	SetROP2(OutputDisplay, R2_COPYPEN);
	DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill2 + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;
	ThickNess = GerberInfo.AreaFillPen1;

	for (cnt = 0; cnt < AreaFill2->NrPolygons; cnt++)
	{
		count = DrawPolygon->NrVertices;

		if (cnt == 0)
			InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);
		else
			InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);

		MemSize = MemSizePolygon(DrawPolygon + 8192);
		AllocateSpecialMem(MEM_POLYGON_BIGGER, MemSize, (void **) &ChangedPolygon);

		if (cnt == 0)
			MakeBiggerSmallerPolygon(DrawPolygon, ChangedPolygon, ThickNess * 1.01, 1);
		else
			MakeBiggerSmallerPolygon(DrawPolygon, ChangedPolygon, ThickNess * 1.01, 0);

		count2 = ChangedPolygon->NrVertices;

		for (cnt3 = 0; cnt3 < count2; cnt3++)
		{
			x11 = (*ChangedPolygon).Points[cnt3].x;
			y11 = (*ChangedPolygon).Points[cnt3].y;

			if (cnt3 < count2 - 1)
			{
				x22 = (*ChangedPolygon).Points[cnt3 + 1].x;
				y22 = (*ChangedPolygon).Points[cnt3 + 1].y;
			}
			else
			{
				x22 = (*ChangedPolygon).Points[0].x;
				y22 = (*ChangedPolygon).Points[0].y;
			}

			Length = CalcLengthLine(x11, y11, x22, y22);
//      ellips2(MultX(x11),MultY(y11),Mult(ThickNess),Mult(ThickNess),255);

//      DrawLine(MultX(x11),MultY(y11),MultX(x22),MultY(y22));
//      ellips2(MultX(x11),MultY(y11),Mult(ThickNess),Mult(ThickNess),255);
//      ellips2(MultX(x22),MultY(y22),Mult(ThickNess),Mult(ThickNess),255);





			if ((res =
			            CheckLineCrossesWithAreaFill(x11, y11, x22, y22, AreaFill2, &xx1, &yy1, &xx2, &yy2, ThickNess * 0.99,
			                    0)) != -1)
			{
				if (res == 0)
				{
					xx1 = x11;
					yy1 = y11;
					xx2 = x22;
					yy2 = y22;
				}
				else
				{
#ifdef _DEBUG
					Length = CalcLengthLine(x11, y11, x22, y22);
					NewLength = CalcLengthLine(xx1, yy1, xx2, yy2);

					if ((Length > 2e5) && (NewLength < 1e5))
					{
//                DrawLineWhite(xx1,yy1,xx2,yy2);
//                DrawLineYellow(x11,y11,x22,y22);
						DrawLine(MultX(x11), MultY(y11), MultX(x22), MultY(y22));
						ellips2(MultX(x11), MultY(y11), Mult(ThickNess), Mult(ThickNess), 255);
						ellips2(MultX(x22), MultY(y22), Mult(ThickNess), Mult(ThickNess), 255);
						ok = 1;
					}

#endif
				}

//        DrawLine(MultX(xx1),MultY(yy1),MultX(xx2),MultY(yy2));
//        ellips2(MultX(xx1),MultY(yy1),Mult(ThickNess),Mult(ThickNess),255);
//        ellips2(MultX(xx2),MultY(yy2),Mult(ThickNess),Mult(ThickNess),255);
			}

			if (res != 0)
			{
				if ((res2 =
				            CheckLineCrossesWithAreaFill(x22, y22, x11, y11, AreaFill2, &xx1, &yy1, &xx2, &yy2,
				                    ThickNess * 0.99, 0)) != -1)
				{
					if (res2 == 0)
					{
						xx1 = x11;
						yy1 = y11;
						xx2 = x22;
						yy2 = y22;
					}
					else
					{
#ifdef _DEBUG
						Length = CalcLengthLine(x11, y11, x22, y22);
						NewLength = CalcLengthLine(xx1, yy1, xx2, yy2);

						if ((Length > 2e5) && (NewLength < 1e5))
						{
							ok = 1;
//                  DrawLineWhite(xx1,yy1,xx2,yy2);
//                  DrawLineYellow(x11,y11,x22,y22);
							DrawLine(MultX(x11), MultY(y11), MultX(x22), MultY(y22));
							ellips2(MultX(x11), MultY(y11), Mult(ThickNess), Mult(ThickNess), 255);
							ellips2(MultX(x22), MultY(y22), Mult(ThickNess), Mult(ThickNess), 255);
						}

#endif
					}

//          DrawLine(MultX(xx1),MultY(yy1),MultX(xx2),MultY(yy2));
//          ellips2(MultX(xx1),MultY(yy1),Mult(ThickNess),Mult(ThickNess),255);
//          ellips2(MultX(xx2),MultY(yy2),Mult(ThickNess),Mult(ThickNess),255);
				}
			}






		}

		/*
		    for (cnt3=0;cnt3<count;cnt3++) {
		      x11=(*DrawPolygon).Points[cnt3].x;
		      y11=(*DrawPolygon).Points[cnt3].y;
		      ellips2(MultX(x11),MultY(y11),Mult(ThickNess),Mult(ThickNess),255);
		    }
		*/
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	ExitDrawing();
	EndDrawingEditingWindow(0);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MirrorAreafill(int32 mode)
{
	int32 cnt, cnt2, AreafillNrToMirror, AreafillNr, count;
	double CentreSelectedX, CentreSelectedY;
	AreaFillRecord *AreaFill;
	uint8 *PolygonPos;
	PolygonRecord *SubPolygon;

	AreafillNrToMirror = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			SubPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if (((SubPolygon->PolygonType & 2) == 2) && (AreafillNrToMirror == -1))
				AreafillNrToMirror = cnt;
		}
	}

	if (AreafillNrToMirror == -1)
		return;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNrToMirror]]);
	CentreSelectedX = AdjustToGrid((AreaFill->maxx + AreaFill->minx) / 2, GridSize);
	CentreSelectedY = AdjustToGrid((AreaFill->maxy + AreaFill->miny) / 2, GridSize);

	if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
		return;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
	memmove(NewAreaFill, AreaFill, AreaFill->MemSize);

	count = NewAreaFill->NrVerticesStartPolygon;

	for (cnt = 0; cnt < count; cnt++)
	{
		if (mode == 0)
			NewAreaFill->StartPolygon[cnt].x += (float) (2.0 * (CentreSelectedX - NewAreaFill->StartPolygon[cnt].x));
		else
			NewAreaFill->StartPolygon[cnt].y += (float) (2.0 * (CentreSelectedY - NewAreaFill->StartPolygon[cnt].y));
	}

	PolygonPos = ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	SubPolygon = (PolygonRecord *) PolygonPos;

	for (cnt2 = 0; cnt2 < NewAreaFill->NrPolygons; cnt2++)
	{
		count = SubPolygon->NrVertices;

		for (cnt = 0; cnt < count; cnt++)
		{
			if (mode == 0)
				(*SubPolygon).Points[cnt].x += 2 * (CentreSelectedX - (*SubPolygon).Points[cnt].x);
			else
				(*SubPolygon).Points[cnt].y += 2 * (CentreSelectedY - (*SubPolygon).Points[cnt].y);
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	if (AddAreaFill(0))
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNrToMirror]]);
		AreaFill->DeleteNr = (int16) LastActionNr;
		AreaFill->Info |= OBJECT_NOT_VISIBLE;
		AreafillNr = Design.NrAreaFills - 1;
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			if ((!IsLayerPowerPlane(AreaFill->Layer)) && (RebuildAreaFill(AreaFill, 0) == 1))
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);
				AreaFill->DeleteNr = 0;
				AreaFill->AddNr = 0;
				AreaFill->Info |= OBJECT_NOT_VISIBLE;
			}
		}

		AreafillNr = Design.NrAreaFills - 1;
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNr]]);

		if (AreaFill->NetNr != -1)
			ReCalcConnectionsNet(AreaFill->NetNr, 0, 1);

		RePaint();
	}

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
