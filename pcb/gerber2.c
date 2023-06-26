/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: gerber2.c
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
#include "circle.h"
#include "stdio.h"
#include "memory.h"
#include "graphics.h"
#include "string.h"
#include "select4.h"
#include "select2.h"
#include "trace2.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "files2.h"
#include "gerber.h"
#include "gerber2.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "math.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "select3.h"
#include "polygon.h"
#include "mainloop.h"
#include "toets.h"
#include "rect.h"
#include "plot.h"


int32 LeadingZeroSuppression = 1;
int32 PenPlotMode, PlotDrawingOk;
int32 RoundingNr, RoundingCnt, PolygonNr, OkToDrawPolygon, Calcs4 = 0;

extern double LastGerberX, LastGerberY, AreafillPenSize1, AreafillPenSize2, PowerPlanePenSize1, PowerPlanePenSize2;

extern int32 Gerberfp, Drillfp, NrCalcs1, NrCalcs2, NrCalcs3, Plotfp, CurrentAperTureNr, FillAperTureNr,
       GerberLineairInterpolation, DcodeTrace, DcodeRoundPad, DcodeRectPad, DcodeDrill, DcodeThermalRelief;


extern AperTureArray *AperTures, *AperTures2, *DrillAperTures;

double DesignBoardOriginX, DesignBoardOriginY, DesignBoardWidth, DesignBoardHeight, CurrentPlotPenSize;
char LineBufGerber[MAX_LENGTH_STRING];


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteGerberString(LPSTR GerberString, int32 mode)
{

	if (mode == 0)
	{
		if ((strlen(LineBufGerber) + strlen(GerberString)) > 85)
		{
			WriteLn(Gerberfp, LineBufGerber);
			LineBufGerber[0] = 0;
		}

		strcat(LineBufGerber, GerberString);
	}
	else
	{
		strcat(LineBufGerber, GerberString);
		WriteLn(Gerberfp, LineBufGerber);
		LineBufGerber[0] = 0;
	}

	return 0;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 GetGerberValueInString(double ValueX, double ValueY, LPSTR GerberString, int32 mode)
{
	double UnitsValue;
	int32 RoundedValueX, RoundedValueY;
	char StrX[MAX_LENGTH_STRING], StrY[MAX_LENGTH_STRING];

	if ((GerberInfo.GerberNumberMode & 8) == 0)
	{	// INCH
		UnitsValue = 2540000.0;

		switch (GerberInfo.GerberNumberMode & 7)
		{
		case 2:				// 2 2
			UnitsValue *= 0.01;
			break;

		case 3:				// 2 3
			UnitsValue *= 0.001;
			break;

		case 4:				// 2 4
			UnitsValue *= 0.0001;
			break;

		case 5:				// 2 5
			UnitsValue *= 0.00001;
			break;

		case 6:				// 2 6
			UnitsValue *= 0.000001;
			break;
		}
	}
	else
	{	// MM
		UnitsValue = 100000.0;

		switch (GerberInfo.GerberNumberMode & 7)
		{
		case 2:				// 3 2
			UnitsValue *= 0.01;
			break;

		case 3:				// 3 3
			UnitsValue *= 0.001;
			break;

		case 4:				// 3 4
			UnitsValue *= 0.0001;
			break;

		case 5:				// 4 2
			UnitsValue *= 0.01;
			break;

		case 6:				// 4 3
			UnitsValue *= 0.001;
			break;

		case 7:				// 4 4
			UnitsValue *= 0.0001;
			break;
		}
	}

	RoundedValueX = (int32) (RoundValue(ValueX, UnitsValue) / UnitsValue);
	RoundedValueY = (int32) (RoundValue(ValueY, UnitsValue) / UnitsValue);

	if ((mode & 1) == 0)
	{
		GerberString[0] = 0;

		if (NotInRange(ValueX, LastGerberX))
		{
			sprintf(StrX, "X%i", RoundedValueX);
			strcat(GerberString, StrX);
			LastGerberX = ValueX;
		}

		if (NotInRange(ValueY, LastGerberY))
		{
			sprintf(StrY, "Y%i", RoundedValueY);
			strcat(GerberString, StrY);
			LastGerberY = ValueY;
		}
	}
	else
		sprintf(GerberString, "I%iJ%i", RoundedValueX, RoundedValueY);

	return 0;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 GerberWriteLine(double x1, double y1, double x2, double y2)
{
// X038803Y076503D02*X039797Y076503D01*

	double x, y, UnitsValue;
	int32 ValueX1, ValueY1, ValueX2, ValueY2;
	AperTureRecord *AperTure;
	char str[MAX_LENGTH_STRING], Prefix[MAX_LENGTH_STRING], StrX[MAX_LENGTH_STRING], StrY[MAX_LENGTH_STRING],
	     StrX2[MAX_LENGTH_STRING], StrY2[MAX_LENGTH_STRING];

	if (GerberInfo.Invert)
	{
		x1 = (DesignBoardOriginX + DesignBoardWidth - x1);
		x2 = (DesignBoardOriginX + DesignBoardWidth - x2);
	}

// ****************************************************************************
	if (!PenPlotMode)
	{
		Prefix[0] = 0;

		if (GerberLineairInterpolation == 0)
		{
			GerberLineairInterpolation = 1;
			strcpy(Prefix, "G01");
		}

		AperTure = &((*AperTures)[CurrentAperTureNr]);

		/*
		    switch (GerberInfo.GerberNumberMode) {
		      case 0:  // 2 3 mode
		*/
		if ((GerberInfo.GerberNumberMode & 8) == 0)
		{	// INCH
			UnitsValue = 2540000.0;

			switch (GerberInfo.GerberNumberMode & 7)
			{
			case 2:			// 2 2
				UnitsValue *= 0.01;
				break;

			case 3:			// 2 3
				UnitsValue *= 0.001;
				break;

			case 4:			// 2 4
				UnitsValue *= 0.0001;
				break;

			case 5:			// 2 5
				UnitsValue *= 0.00001;
				break;

			case 6:			// 2 6
				UnitsValue *= 0.000001;
				break;
			}
		}
		else
		{	// MM
			UnitsValue = 100000.0;

			switch (GerberInfo.GerberNumberMode & 7)
			{
			case 2:			// 3 2
				UnitsValue *= 0.01;
				break;

			case 3:			// 3 3
				UnitsValue *= 0.001;
				break;

			case 4:			// 3 4
				UnitsValue *= 0.0001;
				break;

			case 5:			// 4 2
				UnitsValue *= 0.01;
				break;

			case 6:			// 4 3
				UnitsValue *= 0.001;
				break;

			case 7:			// 4 4
				UnitsValue *= 0.0001;
				break;
			}
		}

		ValueX1 = (int32) (RoundValue(x1, UnitsValue) / UnitsValue);
		ValueY1 = (int32) (RoundValue(y1, UnitsValue) / UnitsValue);
		ValueX2 = (int32) (RoundValue(x2, UnitsValue) / UnitsValue);
		ValueY2 = (int32) (RoundValue(y2, UnitsValue) / UnitsValue);

// ****************************************************************************
// ****************************************************************************
// Leading zero suppression
		if ((NotInRange(x1, LastGerberX)) || (NotInRange(y1, LastGerberY)))
		{
			str[0] = 0;

			if (NotInRange(x1, LastGerberX))
			{
				sprintf(StrX, "X%i", ValueX1);
				strcat(str, StrX);
			}

			if (NotInRange(y1, LastGerberY))
			{
				sprintf(StrY, "Y%i", ValueY1);
				strcat(str, StrY);
			}

			strcat(str, "D02*\r\n");

			if (NotInRange(x1, x2))
			{
				sprintf(StrX2, "X%i", ValueX2);
				strcat(str, StrX2);
			}

			if (NotInRange(y1, y2))
			{
				sprintf(StrY2, "Y%i", ValueY2);
				strcat(str, StrY2);
			}

			strcat(str, "D01*");
		}
		else
		{
			str[0] = 0;

			if (NotInRange(x2, LastGerberX))
			{
				sprintf(StrX2, "X%i", ValueX2);
				strcat(str, StrX2);
			}

			if (NotInRange(y2, LastGerberY))
			{
				sprintf(StrY2, "Y%i", ValueY2);
				strcat(str, StrY2);
			}

			strcat(str, "D01*");
		}

		if (Prefix[0] != 0)
			WriteGerberString(Prefix, 0);

		WriteGerberString(str, 1);
	}
	else
	{
// ****************************************************************************
// ****************************************************************************
		if ((NotInRange(x1, LastGerberX)) || (NotInRange(y1, LastGerberY)))
		{
			x = (x1 + GerberInfo.Xoffset) * GerberInfo.ScaleFactor;
			y = (y1 + GerberInfo.Yoffset) * GerberInfo.ScaleFactor;
			ValueX1 = (int32) ((x + 2500 * 0.5) / 2500);
			ValueY1 = (int32) ((y + 2500 * 0.5) / 2500);
			x = (x2 + GerberInfo.Xoffset) * GerberInfo.ScaleFactor;
			y = (y2 + GerberInfo.Yoffset) * GerberInfo.ScaleFactor;
			ValueX2 = (int32) ((x + 2500 * 0.5) / 2500);
			ValueY2 = (int32) ((y + 2500 * 0.5) / 2500);
			sprintf(str, "PU%i,%i;PD;PA%i,%i;", ValueX1, ValueY1, ValueX2, ValueY2);
			WriteGerberString(str, 0);
		}
		else
		{
			x = (x2 + GerberInfo.Xoffset) * GerberInfo.ScaleFactor;
			y = (y2 + GerberInfo.Yoffset) * GerberInfo.ScaleFactor;
			ValueX2 = (int32) ((x + 2500 * 0.5) / 2500);
			ValueY2 = (int32) ((y + 2500 * 0.5) / 2500);
			sprintf(str, "PA%i,%i;", ValueX2, ValueY2);
			WriteGerberString(str, 0);
		}
	}

	LastGerberX = x2;
	LastGerberY = y2;
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GerberPlotObject(ObjectRecord * Object, int32 mode)
{
// X038803Y076503D02*X039797Y076503D01*
	int32 res, MemSizeAreaFill;
	AperTureRecord *AperTure;
	AreaFillRecord *AreaFill;
	double x1, y1, x2, y2, Thickness, x1a, y1a, x2a, y2a, x3a, y3a, x4a, y4a;
	char str[MAX_LENGTH_STRING];
	uint8 PolygonBuf2[10240];
	PolygonRecord *PolygonObject, *Polygon;
#ifdef _DEBUG
	int32 ok;
#endif

	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	if (!PenPlotMode)
	{
		if (GerberLineairInterpolation == 0)
		{
			GerberLineairInterpolation = 1;
			WriteGerberString("G01", 0);
		}
	}

	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	Thickness = Object->Thickness;
	res = 0;
	AperTure = &((*AperTures)[CurrentAperTureNr]);

	if (!PenPlotMode)
	{
		switch (Object->ObjectType)
		{
		case PIN_LINE_HOR:
		case TRACE_HOR:
			res = GerberWriteLine(x1, y1, x1 + x2, y1);
			break;

		case PIN_LINE_VER:
		case TRACE_VER:
			res = GerberWriteLine(x1, y1, x1, y1 + x2);
			break;

		case PIN_LINE_DIAG1:
		case TRACE_DIAG1:
			res = GerberWriteLine(x1, y1, x1 + x2, y1 - x2);
			break;

		case PIN_LINE_DIAG2:
		case TRACE_DIAG2:
			res = GerberWriteLine(x1, y1, x1 + x2, y1 + x2);
			break;

		case OBJECT_LINE:
		case PIN_LINE_ALL_ANGLE:
		case TRACE_ALL_ANGLE:
			res = GerberWriteLine(x1, y1, x2, y2);
			break;

		case OBJECT_ARC:
		case PIN_ARC:
		case TRACE_ARC:
			DrawGerberArc(Object);
			break;

		case OBJECT_RECT:
			if ((Object->Info & OBJECT_FILLED) != 0)
			{
				if (GerberInfo.Invert)
					x1 = (DesignBoardOriginX + DesignBoardWidth - x1);

				GetGerberValueInString(x1, y1, (LPSTR) & str, 0);
				strcat(str, "D03*");
				WriteGerberString(str, 1);
			}
			else
			{
				x2 *= 0.5;
				y2 *= 0.5;
				x1a = x1 - x2;
				y1a = y1 - y2;
				x2a = x1 + x2;
				y2a = y1 - y2;
				x3a = x1 + x2;
				y3a = y1 + y2;
				x4a = x1 - x2;
				y4a = y1 + y2;

				GerberWriteLine(x1a, y1a, x2a, y2a);
				GerberWriteLine(x2a, y2a, x3a, y3a);
				GerberWriteLine(x3a, y3a, x4a, y4a);
				GerberWriteLine(x4a, y4a, x1a, y1a);
			}

			break;

		case PIN_SMD_POLYGON:
		case OBJECT_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			if (CheckObjectIsBigPolygon(Object))
			{
				GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);

				if ((!PenPlotMode) && ((GerberInfo.PlotMode & 1) == 0))
					res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 1);
				else
					res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 0);

//          res=PlotAreaFillToGerber(AreaFill,AreafillPenSize1,AreafillPenSize2,1);
			}
			else
			{
				MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

				if ((Object->ObjectType2 != 0) && (Object->ObjectType != PIN_PUT_THROUGH_POLYGON)
				        && ((Object->Info & OBJECT_FILLED) == 0))
				{
					GerberWriteLine(PolygonObject->Points[0].x, PolygonObject->Points[0].y, PolygonObject->Points[1].x,
					                PolygonObject->Points[1].y);
					GerberWriteLine(PolygonObject->Points[1].x, PolygonObject->Points[1].y, PolygonObject->Points[2].x,
					                PolygonObject->Points[2].y);
					GerberWriteLine(PolygonObject->Points[2].x, PolygonObject->Points[2].y, PolygonObject->Points[3].x,
					                PolygonObject->Points[3].y);
					GerberWriteLine(PolygonObject->Points[3].x, PolygonObject->Points[3].y, PolygonObject->Points[0].x,
					                PolygonObject->Points[0].y);
				}
				else
				{
					MemSizeAreaFill = MemSizePolygon(PolygonObject) + 10240;
					AllocateSpecialMem(MEM_AREAFILL1, MemSizeAreaFill, (void **) &AreaFill);
					memset(AreaFill, 0, MemSizeAreaFill);
					Polygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
					CopyPolygonToPolygon(PolygonObject, Polygon);
					Polygon->PolygonType = 0;
					AreaFill->NrPolygons = 1;
					SetMinMaxPolygon(Polygon, 0);
					AreaFill->minx = Polygon->minx;
					AreaFill->miny = Polygon->miny;
					AreaFill->maxx = Polygon->maxx;
					AreaFill->maxy = Polygon->maxy;

					if ((!PenPlotMode) && ((GerberInfo.PlotMode & 1) == 0))
						res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 1);
					else
						res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 0);
				}
			}

			break;

		default:
			if (GerberInfo.Invert)
				x1 = (DesignBoardOriginX + DesignBoardWidth - x1);

			GetGerberValueInString(x1, y1, (LPSTR) & str, 0);
			strcat(str, "D03*");
			WriteGerberString(str, 1);
			break;
		}
	}
	else
	{
		switch (Object->ObjectType)
		{
		case DRILL:
		case DRILL_UNPLATED:
			res = DrawGerberCircleOpenPad(x1, y1, min(Object->x2, 120000.0), 50000.0, CurrentPlotPenSize);
//        if (res==-1) CompPinPlotErrors++;
			Object->y2 = 15.0;
			Object->x2 -= CurrentPlotPenSize;
			Object->Clearance = CurrentPlotPenSize;
			res = DrawGerberCircle(Object);
			break;

		case PIN_PUT_THROUGH_ROUND:
			if (NotInRange(y2, 0.0))
				res = DrawGerberCircleOpenPad(x1, y1, x2, min(50000, y2), CurrentPlotPenSize);
			else
				res = DrawGerberCirclePad(x1, y1, x2, CurrentPlotPenSize);

			break;

		case PIN_SMD_ROUND:
			if ((Object->Info & 4) == 0)
				res = DrawGerberCirclePad(x1, y1, x2, CurrentPlotPenSize);
			else
			{
				Object->x3 = AperTure->x2;
				Object->y2 = (AperTure->y - AperTure->x) * 0.5;
				res = DrawGerberThermalRelief(Object);
			}

			break;

		case PIN_PUT_THROUGH_SQUARE:
			if (NotInRange(y2, 0.0))
				res = DrawGerberSquareOpenPad(x1, y1, x2, min(50000, y2), CurrentPlotPenSize);
			else
				res = DrawGerberRectPad(x1, y1, x2, x2, CurrentPlotPenSize);

			break;

		case PIN_SMD_RECT:
			res = DrawGerberRectPad(x1, y1, x2, y2, CurrentPlotPenSize);
			break;

		case VIA_PUT_THROUGH_ROUND:
			if (NotInRange(y2, 0.0))
				res = DrawGerberCircleOpenPad(x1, y1, x2, min(50000, y2), CurrentPlotPenSize);
			else
				res = DrawGerberCirclePad(x1, y1, x2, CurrentPlotPenSize);

			break;

		case PIN_LINE_HOR:
		case TRACE_HOR:
			res = DrawGerberLine(x1, y1, x1 + x2, y1, y2, CurrentPlotPenSize);
			break;

		case PIN_LINE_VER:
		case TRACE_VER:
			res = DrawGerberLine(x1, y1, x1, y1 + x2, y2, CurrentPlotPenSize);
			break;

		case PIN_LINE_DIAG1:
		case TRACE_DIAG1:
			res = DrawGerberLine(x1, y1, x1 + x2, y1 - x2, y2, CurrentPlotPenSize);
			break;

		case PIN_LINE_DIAG2:
		case TRACE_DIAG2:
			res = DrawGerberLine(x1, y1, x1 + x2, y1 + x2, y2, CurrentPlotPenSize);
			break;

		case OBJECT_LINE:
			res = DrawGerberLine(x1, y1, x2, y2, Thickness, CurrentPlotPenSize);
			break;

		case OBJECT_ARC:
			if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
				res = DrawGerberCirclePad(x1, y1, x2, CurrentPlotPenSize);
			else
				DrawGerberArc(Object);

			break;

		case OBJECT_RECT:
			if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
				res = DrawGerberRectPad(x1, y1, x2, y2, CurrentPlotPenSize);
			else
			{
				x2 *= 0.5;
				y2 *= 0.5;
				x1a = x1 - x2;
				y1a = y1 - y2;
				x2a = x1 + x2;
				y2a = y1 - y2;
				x3a = x1 + x2;
				y3a = y1 + y2;
				x4a = x1 - x2;
				y4a = y1 + y2;
				res = DrawGerberLine(x1a, y1a, x2a, y2a, Thickness, CurrentPlotPenSize);
				res = DrawGerberLine(x2a, y2a, x3a, y3a, Thickness, CurrentPlotPenSize);
				res = DrawGerberLine(x3a, y3a, x4a, y4a, Thickness, CurrentPlotPenSize);
				res = DrawGerberLine(x4a, y4a, x1a, y1a, Thickness, CurrentPlotPenSize);
			}

			break;

		case OBJECT_CIRCLE:
			if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
				res = DrawGerberCirclePad(x1, y1, x2, CurrentPlotPenSize);

			break;

		case PIN_SMD_POLYGON:
		case OBJECT_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			if (CheckObjectIsBigPolygon(Object))
				GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
			else
			{
				MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
				MemSizeAreaFill = MemSizePolygon(PolygonObject) + 10240;
				AllocateSpecialMem(MEM_AREAFILL1, MemSizeAreaFill, (void **) &AreaFill);
				memset(AreaFill, 0, MemSizeAreaFill);
				Polygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
				CopyPolygonToPolygon(PolygonObject, Polygon);
				Polygon->PolygonType = 0;
				AreaFill->NrPolygons = 1;
				SetMinMaxPolygon(Polygon, 0);
				AreaFill->minx = Polygon->minx;
				AreaFill->miny = Polygon->miny;
				AreaFill->maxx = Polygon->maxx;
				AreaFill->maxy = Polygon->maxy;
			}

			if ((!PenPlotMode) && ((GerberInfo.PlotMode & 1) == 0))
				res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 1);
			else
				res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 0);

			break;
		}
	}

#ifdef _DEBUG

	if (res == -1)
		ok = 1;

#endif
	return res;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberCirclePad(double x1, double y1, double ThickNess, double PenSize)
{
	double CircleDiam, x3, y3, x4, y4, x, y, d;
	int32 NrRounds, cnt, cnt2, xx, yy, dd;
	int32 Stop = 0;
	char str[MAX_LENGTH_STRING];

	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

	if (PenSize > ThickNess + 10.0)
		return -1;

	if (PenPlotMode)
	{
		if (GerberInfo.Invert)
			x1 = (DesignBoardOriginX + DesignBoardWidth - x1);

		x = (x1 + GerberInfo.Xoffset) * GerberInfo.ScaleFactor;
		y = (y1 + GerberInfo.Yoffset) * GerberInfo.ScaleFactor;
		xx = (int32) ((x + 2500 * 0.5) / 2500);
		yy = (int32) ((y + 2500 * 0.5) / 2500);
		sprintf(str, "PU%i,%i;PD;", xx, yy);
		WriteGerberString(str, 0);
	}

	NrRounds = (int32) ((ThickNess - PenSize - 100) / (PenSize * 1.8) + 1);
	CircleDiam = (ThickNess - PenSize) * 0.5;
	cnt = NrRounds;
	x4 = x1 + CircleDiam;
	y4 = y1;
	cnt = 0;

	while (!Stop)
	{
		if (CircleDiam < ((PenSize + 10.0) * 0.4))
		{
			CircleDiam = min((ThickNess - PenSize) * 0.5, (PenSize + 10.0) * 0.4);
			Stop = 1;
		}

		if ((cnt > 0) && (!PenPlotMode))
		{
			GerberWriteLine(x3, y3, x4, y4);
			x3 = x4;
			y3 = y4;
		}

		if (!PenPlotMode)
		{
			for (cnt2 = 31; cnt2 >= 0; cnt2--)
			{
				x3 = x4;
				y3 = y4;
				x4 = x1 + CircleDiam * CirclePosX[cnt2];
				y4 = y1 + CircleDiam * CirclePosY[cnt2];
				GerberWriteLine(x3, y3, x4, y4);
			}
		}
		else
		{
			d = CircleDiam * GerberInfo.ScaleFactor;
			dd = (int32) ((d + 2500 * 0.5) / 2500);
			dd = max(dd, 1);
			sprintf(str, "CI%i;", dd);
			WriteGerberString(str, 0);
		}

		x3 = x4;
		y3 = y4;
		CircleDiam -= PenSize * (0.8);

		if (CircleDiam < 0.0)
			CircleDiam = 0.0;

//    ellips2(MultX(x4),MultY(y4),Mult(PenSize),Mult(PenSize),255);
//    DrawLine(MultX(x3),MultY(y3),MultX(x4),MultY(y4));
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberCircleOpenPad(double x1, double y1, double ThickNess, double HoleSize, double PenSize)
{
	double CircleDiam, x3, y3, x4, y4, x, y, d;
	int32 cnt, cnt2, xx, yy, dd;
	int32 Stop = 0;
	char str[MAX_LENGTH_STRING];

	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

	HoleSize = min(HoleSize, 50000);

	if (PenSize > ThickNess + 10.0)
		return -1;

	if (PenPlotMode)
	{
		if (GerberInfo.Invert)
			x1 = (DesignBoardOriginX + DesignBoardWidth - x1);

		x = (x1 + GerberInfo.Xoffset) * GerberInfo.ScaleFactor;
		y = (y1 + GerberInfo.Yoffset) * GerberInfo.ScaleFactor;
		xx = (int32) ((x + 2500 * 0.5) / 2500);
		yy = (int32) ((y + 2500 * 0.5) / 2500);
		sprintf(str, "PU%i,%i;", xx, yy);
		WriteGerberString(str, 0);
	}

	CircleDiam = (ThickNess - PenSize) * 0.5;

	cnt = 0;
	x4 = x1 + CircleDiam;
	y4 = y1;

	while (!Stop)
	{
		if (CircleDiam < ((HoleSize + PenSize + (float) 10.0) * 0.5))
		{
			CircleDiam = min((ThickNess - PenSize) * (float) 0.5, (HoleSize + PenSize) * (float) 0.5);
			Stop = 1;
		}

		if ((!PenPlotMode) && (cnt > 0))
		{
			x4 = x1 + CircleDiam;
			y4 = y1;
			GerberWriteLine(x3, y3, x4, y4);
		}

		if (!PenPlotMode)
		{
			for (cnt2 = 31; cnt2 >= 0; cnt2--)
			{
				x3 = x4;
				y3 = y4;
				x4 = x1 + CircleDiam * CirclePosX[cnt2];
				y4 = y1 + CircleDiam * CirclePosY[cnt2];
				GerberWriteLine(x3, y3, x4, y4);
			}

			x3 = x4;
			y3 = y4;
		}
		else
		{
			d = CircleDiam * GerberInfo.ScaleFactor;
			dd = (int32) ((d + 2500 * 0.5) / 2500);
			sprintf(str, "CI%i;", dd);
			WriteGerberString(str, 0);
		}

		CircleDiam -= PenSize * (0.9 * 0.5);
		cnt++;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberSquareOpenPad(double x1, double y1, double ThickNess, double HoleSize, double PenSize)
{

	double RectX, x3, y3, x4, y4, x, y, d, CircleDiam;
	int32 cnt, cnt2, xx, yy, dd;
	int32 Stop = 0;
	char str[MAX_LENGTH_STRING];

	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

	HoleSize = min(HoleSize, 50000);

	if (PenSize > ThickNess + 10.0)
		return -1;

	if (PenPlotMode)
	{
		x = (x1 + GerberInfo.Xoffset) * GerberInfo.ScaleFactor;
		y = (y1 + GerberInfo.Yoffset) * GerberInfo.ScaleFactor;
		xx = (int32) ((x + 2500 * 0.5) / 2500);
		yy = (int32) ((y + 2500 * 0.5) / 2500);
		sprintf(str, "PU%i,%i;", xx, yy);
		WriteGerberString(str, 0);
	}

	cnt = 0;
	RectX = (ThickNess - PenSize) * 0.5;
	x4 = x1 + RectX;
	y4 = y1 + RectX;

	while (!Stop)
	{
		if (RectX < ((HoleSize + PenSize + (float) 10.0) * 0.5))
		{
			RectX = min((ThickNess - PenSize) * (float) 0.5, (HoleSize + PenSize) * (float) 0.5);
			Stop = 1;
		}

		if (cnt > 0)
		{
			x4 = x1 + RectX;
			y4 = y1 + RectX;
			GerberWriteLine(x3, y3, x4, y4);
		}

		x3 = x4;
		y3 = y4;
		x4 = x1 - RectX;
		y4 = y1 + RectX;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		x4 = x1 - RectX;
		y4 = y1 - RectX;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		x4 = x1 + RectX;
		y4 = y1 - RectX;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		x4 = x1 + RectX;
		y4 = y1 + RectX;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		RectX -= PenSize * (0.8);
		cnt++;
	}

	CircleDiam = (HoleSize + PenSize) * 0.5;

	if (!PenPlotMode)
	{
		for (cnt2 = 31; cnt2 >= 0; cnt2--)
		{
			x3 = x4;
			y3 = y4;
			x4 = x1 + CircleDiam * CirclePosX[cnt2];
			y4 = y1 + CircleDiam * CirclePosY[cnt2];
			GerberWriteLine(x3, y3, x4, y4);
		}
	}
	else
	{
		if (GerberInfo.Invert)
			x1 = (DesignBoardOriginX + DesignBoardWidth - x1);

		x = (x1 + GerberInfo.Xoffset) * GerberInfo.ScaleFactor;
		y = (y1 + GerberInfo.Yoffset) * GerberInfo.ScaleFactor;
		d = CircleDiam * GerberInfo.ScaleFactor;
		xx = (int32) ((x + 2500 * 0.5) / 2500);
		yy = (int32) ((y + 2500 * 0.5) / 2500);
		dd = (int32) ((d + 2500 * 0.5) / 2500);
		sprintf(str, "PU%i,%i;CI%i;", xx, yy, dd);
		WriteGerberString(str, 0);
	}

	return 0;
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberRectPad(double x1, double y1, double PadSizeX, double PadSizeY, double PenSize)
{
	double RectX, RectY, x3, y3, x4, y4;
	int32 Stop = 0;
	int32 cnt;

	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

	if (PenPlotMode)
	{
		if (PenSize > PadSizeX + 10.0)
			return -1;

		if (PenSize > PadSizeY + 10.0)
			return -1;
	}

	cnt = 0;
	RectX = (PadSizeX - PenSize) * 0.5;
	RectY = (PadSizeY - PenSize) * 0.5;
	x4 = x1 + RectX;
	y4 = y1 + RectY;

	while (!Stop)
	{
		if (RectX < ((PenSize + 10.0) * 0.4))
		{
			RectX = min((PadSizeX - PenSize) * 0.5, PenSize * 0.4);
			Stop = 1;
		}

		if (RectY < ((PenSize + 10.0) * 0.4))
		{
			RectY = min((PadSizeY - PenSize) * 0.5, PenSize * 0.4);
			Stop = 1;
		}

		if (cnt > 0)
		{
			x4 = x1 + RectX;
			y4 = y1 + RectY;
			GerberWriteLine(x3, y3, x4, y4);
		}

		x3 = x4;
		y3 = y4;
		x4 = x1 - RectX;
		y4 = y1 + RectY;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		x4 = x1 - RectX;
		y4 = y1 - RectY;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		x4 = x1 + RectX;
		y4 = y1 - RectY;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		x4 = x1 + RectX;
		y4 = y1 + RectY;
		GerberWriteLine(x3, y3, x4, y4);
		x3 = x4;
		y3 = y4;
		RectX -= PenSize * (0.8);
		RectY -= PenSize * (0.8);

		if (RectX < 0.0)
			RectX = 0.0;

		if (RectY < 0.0)
			RectY = 0.0;

		cnt++;
	}

	return 0;
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberCircle(ObjectRecord * Object)
{
	int32 NrSegments, cnt2, CircleMode, LineSegments, SegmentCount;
	double x1, y1, x2, y2, LineBuf[4096];

	if (PenPlotMode)
	{
		if (CurrentPlotPenSize > Object->x2 + 10.0)
			return -1;

		if (CurrentPlotPenSize > Object->Thickness + 10.0)
			return -1;
	}

	NrSegments = 32;

	if (Object->x2 > (400 * 2540))
		NrSegments = 64;

	if (Object->x2 > (1000 * 2540))
		NrSegments = 128;

	CircleMode = (int32) (Object->y2 + 0.1);

	LineSegments = CircleToLineSegments(Object->x1, Object->y1, Object->x2, CircleMode, (double *) &LineBuf);
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{

		x1 = LineBuf[SegmentCount];
		SegmentCount++;
		y1 = LineBuf[SegmentCount];
		SegmentCount++;
		x2 = LineBuf[SegmentCount];
		SegmentCount++;
		y2 = LineBuf[SegmentCount];
		SegmentCount++;

		if (!PenPlotMode)
			GerberWriteLine(x1, y1, x2, y2);
		else
			DrawGerberLine(x1, y1, x2, y2, Object->Clearance, CurrentPlotPenSize);
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberThermalRelief(ObjectRecord * Object)
{
	int32 NrSegments, cnt, cnt3, VertixCount;
	double x1, y1, x3, y3, x4, y4, CircleR, StartAngle, EndAngle, IncAngle, Angle, VertexR, PointX[33], PointY[33];

// Object->y2   Drawing thickness

	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

	if (PenPlotMode)
	{
		if (CurrentPlotPenSize > Object->y2 + 10.0)
			return -1;

//    if (CurrentPlotPenSize>Object->Clearance+10.0) return -1;
	}

	NrSegments = 8;

	if (Object->x2 > (200000))
		NrSegments = 16;

	if (Object->x2 > (600000))
		NrSegments = 32;

	x1 = Object->x1;
	y1 = Object->y1;
	CircleR = Object->x2 * 0.5;
	VertexR = CircleR + Object->y2 * 0.5;
	StartAngle = asin((Object->y2 + Object->x3) * 0.5 / VertexR);
	EndAngle = ANGLE_90 - StartAngle;
	IncAngle = (EndAngle - StartAngle) / NrSegments;
	Angle = StartAngle;

	for (cnt = 0; cnt < NrSegments + 1; cnt++)
	{
		PointX[cnt] = (cos(Angle) * VertexR);
		PointY[cnt] = (sin(Angle) * VertexR);
		Angle += IncAngle;
	}

	VertixCount = 0;

	for (cnt3 = 0; cnt3 < 4; cnt3++)
	{
		switch (cnt3)
		{
		case 0:
			x4 = x1 + PointX[0];
			y4 = y1 + PointY[0];
			break;

		case 1:
			x4 = x1 - PointX[0];
			y4 = y1 + PointY[0];
			break;

		case 2:
			x4 = x1 - PointX[0];
			y4 = y1 - PointY[0];
			break;

		case 3:
			x4 = x1 + PointX[0];
			y4 = y1 - PointY[0];
			break;
		}

		VertixCount++;

		for (cnt = 0; cnt < NrSegments; cnt++)
		{
			x3 = x4;
			y3 = y4;

			switch (cnt3)
			{
			case 0:
				x4 = x1 + PointX[cnt + 1];
				y4 = y1 + PointY[cnt + 1];
				break;

			case 1:
				x4 = x1 - PointX[cnt + 1];
				y4 = y1 + PointY[cnt + 1];
				break;

			case 2:
				x4 = x1 - PointX[cnt + 1];
				y4 = y1 - PointY[cnt + 1];
				break;

			case 3:
				x4 = x1 + PointX[cnt + 1];
				y4 = y1 - PointY[cnt + 1];
				break;
			}

			VertixCount++;

			if (!PenPlotMode)
				GerberWriteLine(x3, y3, x4, y4);
			else
				DrawGerberLine(x3, y3, x4, y4, Object->y2, CurrentPlotPenSize);
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberArc(ObjectRecord * Object)
{
	int32 NrSegments, cnt2, LineSegments, SegmentCount;
	double x1, y1, x2, y2, x3, y3, x4, y4, Length, Angle1, Angle2, LineBuf[4096];
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

#ifdef _DEBUG
	int32 ok;
#endif

	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2 * 0.5;
	y2 = Object->y2 * 0.5;

	if ((!PenPlotMode) && (InRangeSpecial(x2, y2, 0.01e5)))
	{
		x3 = Object->x3;
		y3 = Object->y3;
		x4 = Object->x4;
		y4 = Object->y4;

		if (GerberInfo.Invert)
		{
			x1 = (DesignBoardOriginX + DesignBoardWidth - x1);
			x3 = -Object->x4;
			y3 = Object->y4;
			x4 = -Object->x3;
			y4 = Object->y3;
		}

#ifdef _DEBUG

		if ((InRange9(x1, 253.0e5)) && (InRange9(y1, 225.9e5)))
			ok = 1;

		if (InRange9(y1, 213.3e5))
			ok = 1;

#endif
		ConvertPointToPolar(x3, y3, &Length, &Angle1);
		ConvertPointToPolar(x4, y4, &Length, &Angle2);

		if (InRange2(Angle1, Angle2))
			Angle2 -= 0.1;

		x3 = x2 * cos(ANGLE_CONVERT(Angle1));
		y3 = y2 * sin(ANGLE_CONVERT(Angle1));
		x4 = x2 * cos(ANGLE_CONVERT(Angle2));
		y4 = y2 * sin(ANGLE_CONVERT(Angle2));
		GetGerberValueInString(x1 + x3, y1 + y3, (LPSTR) & str2, 0);
		strcat(str2, "D02*");
		WriteGerberString(str2, 1);	// Set starting point arc

		if (GerberLineairInterpolation == 1)
			GerberLineairInterpolation = 0;

		WriteGerberString("G03", 0);
		GetGerberValueInString(x1 + x4, y1 + y4, (LPSTR) & str, 0);
		GetGerberValueInString(-x3, -y3, (LPSTR) & str2, 1);
		WriteGerberString(str, 0);
		WriteGerberString(str2, 0);
		WriteGerberString("D01*", 1);
		LastGerberX = -1000000000.0;
		LastGerberY = -1000000000.0;
		return 0;
	}


	NrSegments = 32;

	if (PenPlotMode)
	{
		if (CurrentPlotPenSize > Object->x2 + 10.0)
			return -1;

		if (CurrentPlotPenSize > Object->y2 + 10.0)
			return -1;

		if ((Object->Info & OBJECT_FILLED) == 0)
		{
			if (CurrentPlotPenSize > Object->Thickness + 10.0)
				return -1;
		}
	}

	if (Object->x2 > (400 * 2540))
		NrSegments = 64;

	if (Object->x2 > (1000 * 2540))
		NrSegments = 128;

#ifdef _DEBUG

	if ((InRange9(x1, 253.0e5)) && (InRange9(y1, 225.9e5)))
		ok = 1;

	if (InRange9(y1, 213.3e5))
		ok = 1;

#endif

	LineSegments =
	    ArcToLineSegments(Object->x1, Object->y1, Object->x2, Object->y2, Object->x3, Object->y3, Object->x4,
	                      Object->y4, (double *) &LineBuf, 1);
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{
		x1 = LineBuf[SegmentCount];
		SegmentCount++;
		y1 = LineBuf[SegmentCount];
		SegmentCount++;
		x2 = LineBuf[SegmentCount];
		SegmentCount++;
		y2 = LineBuf[SegmentCount];
		SegmentCount++;

		if (!PenPlotMode)
			GerberWriteLine(x1, y1, x2, y2);
		else
			DrawGerberLine(x1, y1, x2, y2, Object->Thickness, CurrentPlotPenSize);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawGerberLine(double x1, double y1, double x2, double y2, double ThickNess, double PenSize)
{
	double x1a, y1a, x2a, y2a, CircleDiam2, x3, y3, x4, y4, CircleDiam, CircleDiamX, CircleDiamY, hoek, Rico;
	int32 NrRounds, cnt, cnt2, Step;

	if (PenSize > ThickNess + 10.0)
		return -1;

	NrRounds = (int32) ((ThickNess + PenSize - 100) / (PenSize * 0.9));

	if (ThickNess < PenSize * 1.91)
		NrRounds = 2;

	if (PenPlotMode)
	{
		if ((ThickNess > PenSize * 0.9) && (ThickNess < PenSize * 1.1))
		{
			GerberWriteLine(x1, y1, x2, y2);
			return 0;
		}
	}
	else
	{
		if (InRange(ThickNess, PenSize))
			GerberWriteLine(x1, y1, x2, y2);
	}

	Step = 4;

	if (NrRounds > 4)
		Step = 2;

	if (NrRounds > 8)
		Step = 1;

// *************************************************************************
// *************************************************************************
	if (InRange3(x1, x2))
	{	// Vertical
		x1a = x1;
		y1a = min(y1, y2);
		y2a = max(y1, y2);
		CircleDiam = (ThickNess - PenSize) * 0.5;
		cnt = NrRounds;
		x4 = x1a + CircleDiam;
		y4 = y2a;
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GREEN + DRAW_WITH_PEN_AND_NOT_FILLED);

		while (cnt > 0)
		{
			x3 = x4;
			y3 = y4;
			x4 = x1a + CircleDiam;
			y4 = y1a;
			GerberWriteLine(x3, y3, x4, y4);

			if (cnt > 1)
			{
				for (cnt2 = 32 - Step; cnt2 >= 16; cnt2 -= Step)
				{
					x3 = x4;
					y3 = y4;
					x4 = x1a + CircleDiam * CirclePosX[cnt2];
					y4 = y1a + CircleDiam * CirclePosY[cnt2];
					GerberWriteLine(x3, y3, x4, y4);
				}

				x3 = x4;
				y3 = y4;
				x4 = x1a - CircleDiam;
				y4 = y2a;
				GerberWriteLine(x3, y3, x4, y4);

				for (cnt2 = 16 - Step; cnt2 >= 0; cnt2 -= Step)
				{
					x3 = x4;
					y3 = y4;
					x4 = x1a + CircleDiam * CirclePosX[cnt2];
					y4 = y2a + CircleDiam * CirclePosY[cnt2];
					GerberWriteLine(x3, y3, x4, y4);
				}

				if (cnt - 2 > 0)
				{
					x3 = x4;
					y3 = y4;
					CircleDiam -= PenSize * 0.9;

					if (CircleDiam < 0)
						CircleDiam = 0.0;

					x4 = x1a + CircleDiam;
					y4 = y2a;
					GerberWriteLine(x3, y3, x4, y4);
				}
			}

			cnt -= 2;
		}

		return 0;
	}

// *************************************************************************
// *************************************************************************
	if (InRange3(y1, y2))
	{	// Horizontal
		x1a = min(x1, x2);
		x2a = max(x1, x2);
		y1a = y1;
		CircleDiam = (ThickNess - PenSize) * 0.5;
		cnt = NrRounds;
		x4 = x1a;
		y4 = y1a + CircleDiam;

		while (cnt > 0)
		{
			x3 = x4;
			y3 = y4;
			x4 = x2a;
			y4 = y1a + CircleDiam;
			GerberWriteLine(x3, y3, x4, y4);

			if (cnt > 1)
			{
				for (cnt2 = 8 + 32; cnt2 >= 24; cnt2 -= Step)
				{
					x3 = x4;
					y3 = y4;
					x4 = x2a + CircleDiam * CirclePosX[cnt2];
					y4 = y1a + CircleDiam * CirclePosY[cnt2];
					GerberWriteLine(x3, y3, x4, y4);
				}

				x3 = x4;
				y3 = y4;
				x4 = x1a;
				y4 = y1a - CircleDiam;
				GerberWriteLine(x3, y3, x4, y4);

				for (cnt2 = 24 - Step; cnt2 >= 8; cnt2 -= Step)
				{
					x3 = x4;
					y3 = y4;
					x4 = x1a + CircleDiam * CirclePosX[cnt2];
					y4 = y1a + CircleDiam * CirclePosY[cnt2];
					GerberWriteLine(x3, y3, x4, y4);
				}

				if (cnt - 2 > 0)
				{
					x3 = x4;
					y3 = y4;
					CircleDiam -= PenSize * 0.9;

					if (CircleDiam < 0)
						CircleDiam = 0.0;

					x4 = x1a;
					y4 = y1a + CircleDiam;
					GerberWriteLine(x3, y3, x4, y4);
				}
			}

			cnt -= 2;
		}

		return 0;
	}

	x1a = x1;
	y1a = y1;
	x2a = x2;
	y2a = y2;

	if (x1 > x2)
	{
		x1a = x2;
		y1a = y2;
		x2a = x1;
		y2a = y1;
	}

	if (InRange3(x2 - x1, fabs(y1 - y2)))
	{
// *************************************************************************
// *************************************************************************
		if (y1 > y2)
		{	// Diag1
			CircleDiam = (ThickNess - PenSize) * 0.5;
			CircleDiam2 = CircleDiam * sqrt(0.5);
			cnt = NrRounds;
			x4 = x1a + CircleDiam2;
			y4 = y1a + CircleDiam2;

			while (cnt > 0)
			{
				x3 = x4;
				y3 = y4;
				x4 = x2a + CircleDiam2;
				y4 = y2a + CircleDiam2;
				GerberWriteLine(x3, y3, x4, y4);

				if (cnt > 1)
				{
					for (cnt2 = 4 + 32; cnt2 >= 20; cnt2 -= Step)
					{
						x3 = x4;
						y3 = y4;
						x4 = x2a + CircleDiam * CirclePosX[cnt2];
						y4 = y2a + CircleDiam * CirclePosY[cnt2];
						GerberWriteLine(x3, y3, x4, y4);
					}

					x3 = x4;
					y3 = y4;
					x4 = x1a - CircleDiam2;
					y4 = y1a - CircleDiam2;
					GerberWriteLine(x3, y3, x4, y4);

					for (cnt2 = 20; cnt2 >= 4; cnt2 -= Step)
					{
						x3 = x4;
						y3 = y4;
						x4 = x1a + CircleDiam * CirclePosX[cnt2];
						y4 = y1a + CircleDiam * CirclePosY[cnt2];
						GerberWriteLine(x3, y3, x4, y4);
					}

					if (cnt - 2 > 0)
					{
						x3 = x4;
						y3 = y4;
						CircleDiam -= PenSize * (0.9);

						if (CircleDiam < 0)
							CircleDiam = 0.0;

						CircleDiam2 = CircleDiam * sqrt(0.5);
						x4 = x1a + CircleDiam2;
						y4 = y1a + CircleDiam2;
						GerberWriteLine(x3, y3, x4, y4);
					}
				}

				cnt -= 2;
			}

			return 0;
		}
		else
		{	// Diag2
// *************************************************************************
// *************************************************************************
			CircleDiam = (ThickNess - PenSize) * 0.5;
			CircleDiam2 = CircleDiam * sqrt(0.5);
			cnt = NrRounds;
			x4 = x1a - CircleDiam2;
			y4 = y1a + CircleDiam2;

			while (cnt > 0)
			{
				x3 = x4;
				y3 = y4;
				x4 = x2a - CircleDiam2;
				y4 = y2a + CircleDiam2;
				GerberWriteLine(x3, y3, x4, y4);

				if (cnt > 1)
				{
					for (cnt2 = 12 + 32; cnt2 >= 28; cnt2 -= Step)
					{
						x3 = x4;
						y3 = y4;
						x4 = x2a + CircleDiam * CirclePosX[cnt2];
						y4 = y2a + CircleDiam * CirclePosY[cnt2];
						GerberWriteLine(x3, y3, x4, y4);
					}

					x3 = x4;
					y3 = y4;
					x4 = x1a + CircleDiam2;
					y4 = y1a - CircleDiam2;
					GerberWriteLine(x3, y3, x4, y4);

					for (cnt2 = 28; cnt2 >= 12; cnt2 -= Step)
					{
						x3 = x4;
						y3 = y4;
						x4 = x1a + CircleDiam * CirclePosX[cnt2];
						y4 = y1a + CircleDiam * CirclePosY[cnt2];
						GerberWriteLine(x3, y3, x4, y4);
					}

					if (cnt - 2 > 0)
					{
						x3 = x4;
						y3 = y4;
						CircleDiam -= PenSize * (0.9);

						if (CircleDiam < 0)
							CircleDiam = 0.0;

						CircleDiam2 = CircleDiam * sqrt(0.5);
						x4 = x1a - CircleDiam2;
						y4 = y1a + CircleDiam2;
						GerberWriteLine(x3, y3, x4, y4);
					}
				}

				cnt -= 2;
			}

			return 0;

		}
	}

// *************************************************************************
// *************************************************************************
	Rico = (y2 - y1) / (x2 - x1);
	hoek = (atan(Rico) + ANGLE_90);
	CircleDiam = (ThickNess - PenSize) * 0.5;
	CircleDiamX = CircleDiam * (cos(hoek));
	CircleDiamY = CircleDiam * (sin(hoek));
	cnt = NrRounds;
	x4 = x1a + CircleDiamX;
	y4 = y1a + CircleDiamY;

	while (cnt > 0)
	{
		x3 = x4;
		y3 = y4;
		x4 = x2a + CircleDiamX;
		y4 = y2a + CircleDiamY;
		GerberWriteLine(x3, y3, x4, y4);

		if (cnt > 1)
		{
			for (cnt2 = Step; cnt2 < 16 + Step; cnt2 += Step)
			{
				x3 = x4;
				y3 = y4;
				x4 = x2a + CircleDiam * (cos(hoek - cnt2 * ANGLE_180 / 16));
				y4 = y2a + CircleDiam * (sin(hoek - cnt2 * ANGLE_180 / 16));
				GerberWriteLine(x3, y3, x4, y4);
			}

			x3 = x4;
			y3 = y4;
			x4 = x1a - CircleDiamX;
			y4 = y1a - CircleDiamY;
			GerberWriteLine(x3, y3, x4, y4);

			for (cnt2 = Step; cnt2 < 16 + Step; cnt2 += Step)
			{
				x3 = x4;
				y3 = y4;
				x4 = x1a + CircleDiam * (cos(hoek - cnt2 * ANGLE_180 / 16 + ANGLE_180));
				y4 = y1a + CircleDiam * (sin(hoek - cnt2 * ANGLE_180 / 16 + ANGLE_180));
				GerberWriteLine(x3, y3, x4, y4);
			}

			if (cnt - 2 > 0)
			{
				x3 = x4;
				y3 = y4;
				CircleDiam -= PenSize * (0.9);

				if (CircleDiam < 0)
					CircleDiam = 0.0;

				CircleDiamX = CircleDiam * (cos(hoek));
				CircleDiamY = CircleDiam * (sin(hoek));
				x4 = x1a + CircleDiamX;
				y4 = y1a + CircleDiamY;
				GerberWriteLine(x3, y3, x4, y4);
			}
		}

		cnt -= 2;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 DrawGerberStr(double x, double y, double Size, double LineThickness, int32 Rotation, int32 Alignment,
                    int32 Mirror, char *str)
{
	int32 cnt2, LineSegments, SegmentCount;
	double x1, y1, x2, y2, LineBuf[4096];

	/*
	  if ((PenPlotMode)
	     &&
	     (LineThickness>CurrentPlotPenSize+10.0)) return -1;
	*/


	LineSegments = TextStringToLineSegments2(x, y, Size, 0.0, 0, Mirror, str, (double *) &LineBuf);
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{
		x1 = LineBuf[SegmentCount];
		SegmentCount++;
		y1 = LineBuf[SegmentCount];
		SegmentCount++;
		x2 = LineBuf[SegmentCount];
		SegmentCount++;
		y2 = LineBuf[SegmentCount];
		SegmentCount++;

		if (!PenPlotMode)
			GerberWriteLine(x1, y1, x2, y2);
		else
			DrawGerberLine(x1, y1, x2, y2, LineThickness, CurrentPlotPenSize);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 DrawGerberStr2(double x, double y, double Size, double LineThickness, double Rotation, int32 Alignment,
                     int32 Mirror, char *str)
{
	int32 cnt2, LineSegments, SegmentCount;
	double x1, y1, x2, y2, LineBuf[4096];
#ifdef _DEBUG
	int32 ok;
#endif

	/*
	  if ((PenPlotMode)
	     &&
	     (LineThickness>CurrentPlotPenSize+10.0)) return -1;
	*/


	LineSegments = TextStringToLineSegments2(x, y, Size, Rotation, 0, Mirror, str, (double *) &LineBuf);
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{
		x1 = LineBuf[SegmentCount];
		SegmentCount++;
		y1 = LineBuf[SegmentCount];
		SegmentCount++;
		x2 = LineBuf[SegmentCount];
		SegmentCount++;
		y2 = LineBuf[SegmentCount];
		SegmentCount++;
#ifdef _DEBUG

		if (CalcLengthLine(x1, y1, x2, y2) == 0.0)
			ok = 1;

#endif

		if (!PenPlotMode)
			GerberWriteLine(x1, y1, x2, y2);
		else
			DrawGerberLine(x1, y1, x2, y2, LineThickness, CurrentPlotPenSize);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPlotLine2(double x1, double y1, double x2, double y2)
{
	int32 ok;
	char str[MAX_LENGTH_STRING];


	sprintf(str, "%14.5f,%14.5f - %14.5f,%14.5f  %i  %i  %i", x1, y1, x2, y2, PolygonNr, RoundingNr, RoundingCnt);

	if ((InRange4(x1, 133.07e5)) && (InRange4(y1, 75.83e5)) && (InRange4(x2, 133.27e5)) && (InRange4(y2, 75.99e5)))
		ok = 1;

//  WriteLn(Plotfp,str);

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckLineCrossesCircle2(double x1, double y1, double x2, double y2, double cx, double cy, double ThickNess)
{
	double R, R2, a1, dx, dy, one_diva1, divx1, divy1, divx2, divy2;
	int32 Vert12;

	a1 = 0.0;
	R = ThickNess * 0.5;
	/*
	  if (min(x1,x2)>cx+R) return 0;
	  if (max(x1,x2)<cx-R) return 0;
	  if (min(y1,y2)>cy+R) return 0;
	  if (max(y1,y2)<cy-R) return 0;
	*/
	R2 = R * R;

	divx1 = (cx - x1) * (cx - x1);
	divy1 = (cy - y1) * (cy - y1);
	divx2 = (cx - x2) * (cx - x2);
	divy2 = (cy - y2) * (cy - y2);

	if ((divx1 + divy1 < R2) || (divx2 + divy2 < R2))
		return 1;

	Vert12 = 0;

	if (x1 != x2)
	{
		a1 = (y2 - y1) / (x2 - x1);

		if ((a1 < -100000) || (a1 > 100000))
			Vert12 = 1;
	}
	else
		Vert12 = 1;

	if (Vert12)
	{
		if (divx1 > R2)
			return 0;

		if ((cy > min(y1, y2)) && (cy < max(y1, y2)))
			return 1;

		return 0;
	}
	else
	{
		if (y1 == y2)
		{
			if (divy1 > R2)
				return 0;

			if ((cx > min(x1, x2)) && (cx < max(x1, x2)))
				return 1;

			return 0;
		}
		else
		{
			if (fabs(a1) < 0.001)
			{
				dx = cx;
				dy = y1;
			}
			else
			{
				one_diva1 = -1 / a1;
				dx = (cx * one_diva1 - cy - x1 * a1 + y1) / (one_diva1 - a1);
				dy = (dx - cx) * one_diva1 + cy;
			}

			if ((dx > min(x1, x2)) && (dx < max(x1, x2)) && (((cx - dx) * (cx - dx) + (cy - dy) * (cy - dy)) < R2))
				return 1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PlotAreaFillToGerber(AreaFillRecord * AreaFill, double Thickness1, double Thickness2, int32 mode)
{
	typedef uint8 ByteArray[1000];

	int32 cnt, cnt2, cnt3, cnt4, res, res2, count, count2, ok, NrLines, NrSegmentLines, FirstCutOutNr, MaxRoundings,
	      NrPoints, AperTureNr, NrShortenLines, MemSize, Found, LineOn, FoundFirstLine, AreaFillChanged, DrawWithTwoPens,
	      LineShorten;
	double *x1, *y1, *x2, *y2, *FirstX, *FirstY, *LinesBuf, *LinesBufCopy, LineMinX, LineMaxX, PointX, PointY, x3, y3,
	       x4, y4, minx, miny, maxx, maxy, LineX1, LineX2, Thickness_1, NewThickNess, x33, y33, R, xx1, yy1, xx2, yy2,
	       SearchMinY, SearchMaxY, IncY, PaintY, x11, y11, x22, y22;
	PolygonRecord *Polygon, *ChangedPolygon, *DrawPolygon, *PolygonObject;
	uint8 *AreaPos, *PolygonPos, *PointsMap, *PointsMapCopy;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	AperTureRecord *AperTure;
	ObjectRecord *Object;
	CompRecord *Comp;
	ViaRecord *Via;
	uint8 PolygonBuf2[10240];
	AreaFillRecord *AreaFillToGerber;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	AreaFillChanged = 0;
	DrawWithTwoPens = 0;
	xx1 = 0.0;
	yy1 = 0.0;
	xx2 = 0.0;
	yy2 = 0.0;
	LineX1 = 0.0;
	res2 = 0;
	AreaFillToGerber = AreaFill;
	strcpy(str2, InfoStr);


	if ((PenPlotMode) && ((mode & 3) == 0))
	{
		// Create addional drill holes for pins and vias for easy drilling
		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if (((Via->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr == Via->NetNr)
			        && (Via->DrillThickNess > MinDrillForThermalRelief))
			{
				Object = &((*Objects)[0]);
				Object->x1 = Via->X;
				Object->y1 = Via->Y;
				Object->x2 = Via->ThickNess;
				Object->ObjectType = VIA_PUT_THROUGH_ROUND;
				FillPositionObject(Object);

				if ((AreaFill->maxx >= Object->minx) && (AreaFill->minx <= Object->maxx)
				        && (AreaFill->maxy >= Object->miny) && (AreaFill->miny <= Object->maxy))
				{
					Object->x2 = min(Object->y2, 50000);
					MakePolygonFromObject(Object, PolygonObject, 0.0, 0.00001, 1, 0);

					if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
					{
						if (!AreaFillChanged)
						{
							if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
								return -2;

							NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
							TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

							if (AllocateMemPolygons(0) != 0)
								return -2;
						}

						if (!AreaFillChanged)
							memmove(AreaFillMemTemp2, AreaFill, AreaFill->MemSize);
						else
							memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);

						res = MergePolygon(PolygonObject, 0);

						if (res < 0)
							return res - 1000;

						AreaFillChanged = 1;
					}
				}
			}
		}

		// *****************************************************************************************************************

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
					FillPositionObject(Object);
					Found = -1;

					switch (Object->ObjectType)
					{
					case DRILL_UNPLATED:
					case DRILL:
					case PIN_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_SQUARE:
					case PIN_PUT_THROUGH_POLYGON:
						if ((AreaFill->NetNr == Object->NetNr) && (AreaFill->Layer == Object->Layer)
						        && (AreaFill->maxx >= Object->minx) && (AreaFill->minx <= Object->maxx)
						        && (AreaFill->maxy >= Object->miny) && (AreaFill->miny <= Object->maxy))
						{
							Object->ObjectType = PIN_PUT_THROUGH_ROUND;

							if ((Object->ObjectType != DRILL) && (Object->ObjectType != DRILL_UNPLATED))
								Object->x2 = Object->y2;

							Object->x2 = min(Object->x2, 50000);
							MakePolygonFromObject(Object, PolygonObject, 0.0, 0.00001, 1, 0);

							if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
							{
								if (!AreaFillChanged)
								{
									if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
										return -2;

									NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
									TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

									if (AllocateMemPolygons(0) != 0)
										return -2;
								}

								if (!AreaFillChanged)
									memmove(AreaFillMemTemp2, AreaFill, AreaFill->MemSize);
								else
									memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);

								res = MergePolygon(PolygonObject, 1);

								if (res < 0)
									return res - 1000;

								AreaFillChanged = 1;
							}
						}

						break;
					}
				}
			}
		}
	}
	else
	{
//    AreaFillToGerber2(AreaFill,0);
//    return 1;
	}

	if (AreaFillChanged)
		AreaFillToGerber = NewAreaFill;

// *******************************************************************************************************
// *******************************************************************************************************

#ifdef _DEBUG

	if (AreaFillToGerber->NrPolygons == 34)
		ok = 1;

#endif

	if ((mode & 1) == 1)
	{
		AreaPos = (uint8 *) AreaFillToGerber;
		DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) DrawPolygon;
		FirstCutOutNr = 0;

		for (cnt2 = 0; cnt2 < AreaFillToGerber->NrPolygons; cnt2++)
		{
			if (cnt2 == 1)
				FirstCutOutNr = 1;

			if ((DrawPolygon->PolygonType & 8) == 0)
			{
				if (cnt2 == 0)
					sprintf(str, "%%LPD*%%G36*");
				else
				{
					if (FirstCutOutNr == 1)
						sprintf(str, "%%LPC*%%G36*");
					else
						sprintf(str, "G36*");
				}

				WriteGerberString(str, 1);
				LastGerberX = -1e10;
				LastGerberY = -1e10;
				count = DrawPolygon->NrVertices;

				for (cnt3 = 0; cnt3 < count; cnt3++)
				{
					x11 = (*DrawPolygon).Points[cnt3].x;
					y11 = (*DrawPolygon).Points[cnt3].y;

					if (GerberInfo.Invert)
						x11 = (DesignBoardOriginX + DesignBoardWidth - x11);

					GetGerberValueInString(x11, y11, (LPSTR) & str, 0);

					if (cnt3 == 0)
						strcat(str, "D02*");
					else
						strcat(str, "D01*");

					WriteGerberString(str, 1);
				}

				x11 = (*DrawPolygon).Points[0].x;
				y11 = (*DrawPolygon).Points[0].y;

				if (GerberInfo.Invert)
					x11 = (DesignBoardOriginX + DesignBoardWidth - x11);

				GetGerberValueInString(x11, y11, (LPSTR) & str, 0);
				strcat(str, "D01*");
				WriteGerberString(str, 1);
				sprintf(str, "G37*");
				WriteGerberString(str, 1);
			}
			else
			{
#ifdef _DEBUG

				if (cnt2 == 0)
					ok = 1;

#endif
			}

			PolygonPos += MemSizePolygon(DrawPolygon);
			DrawPolygon = (PolygonRecord *) PolygonPos;
		}

		sprintf(str, "%%LPD*%%");
		WriteGerberString(str, 1);
		return 0;
	}

	/*
	#ifdef _DEBUG
	  WriteLn(Plotfp,"-----------------------------------------------------------------------");
	  sprintf(str,"Areafill");
	  WriteLn(Plotfp,str);
	  WriteLn(Plotfp,"-----------------------------------------------------------------------");
	#endif
	*/
	AperTureNr = CheckTraceAperTure(Thickness1);
	AperTure = &((*AperTures)[AperTureNr]);
	CurrentAperTureNr = AperTureNr;

	if (!PenPlotMode)
	{
		sprintf(str, "G54D%i*", AperTure->AperTureCode);
		WriteGerberString(str, 1);
	}
	else
	{
		DrawWithTwoPens = 0;

//    WriteGerberString("");
		if (GerberInfo.PenSizes[1] > 0.0)
		{
			sprintf(str, "PU;SP1;");
			DrawWithTwoPens = 1;
		}
		else
			sprintf(str, "PU;");

		WriteGerberString(str, 0);
	}

	MaxRoundings = (int32) (Thickness2 / (Thickness1 * 0.6));
//  MaxRoundings=2;
	AreaPos = (uint8 *) AreaFillToGerber;
	count = sizeof(AreaFillRecord);

	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt2 = 0; cnt2 < AreaFillToGerber->NrPolygons; cnt2++)
	{
		count = DrawPolygon->NrVertices;
		minx = 1e9;
		miny = 1e9;
		maxx = -1e9;
		maxy = -1e9;

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			minx = min(minx, (*DrawPolygon).Points[cnt3].x);
			miny = min(miny, (*DrawPolygon).Points[cnt3].y);
			maxx = max(maxx, (*DrawPolygon).Points[cnt3].x);
			maxy = max(maxy, (*DrawPolygon).Points[cnt3].y);
		}

		DrawPolygon->minx = minx;
		DrawPolygon->miny = miny;
		DrawPolygon->maxx = maxx;
		DrawPolygon->maxy = maxy;
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	minx = AreaFillToGerber->minx;
	miny = AreaFillToGerber->miny;
	maxx = AreaFillToGerber->maxx;
	maxy = AreaFillToGerber->maxy;

	NrShortenLines = 0;
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;
//  AreaFillToGerber->NrPolygons=1;
	cnt2 = 0;
	SelectionEsc = 0;

	while ((cnt2 < AreaFillToGerber->NrPolygons) && (!SelectionEsc))
	{

//  for (cnt2=0;cnt2<AreaFillToGerber->NrPolygons;cnt2++) {

		count = DrawPolygon->NrVertices;

		if ((DrawPolygon->PolygonType & 8) == 0)
		{

			if (SelectionEsc)
				ok = 1;

			if ((cnt2 % 20) == 0)
				CheckForEscape();

			//    MaxRoundings=1;
			/*
			   #ifdef _DEBUG
			   count2=DrawPolygon->NrVertices;
			   for (cnt3=0;cnt3<count2;cnt3++) {
			   x11=(*DrawPolygon).Points[cnt3].x;
			   y11=(*DrawPolygon).Points[cnt3].y;
			   sprintf(str,"Points  %14.5f,%14.5f %i %i",x11,y11,cnt2,cnt3);
			   WriteLn(Plotfp,str);
			   }
			   WriteLn(Plotfp,"-----------------------------------------------------------------------");
			   #endif
			 */
			for (cnt4 = 0; cnt4 < MaxRoundings; cnt4++)
			{
				NewThickNess = (1 + (cnt4 * 1.6)) * Thickness1;
				/*
				   #ifdef _DEBUG
				   sprintf(str,"Rounding  %i",cnt4);
				   WriteLn(Plotfp,str);
				   #endif
				 */
#ifdef _DEBUG

				if (DrawPolygon->NrVertices == 0)
					ok = 1;

#endif
				sprintf(InfoStr, "%s  %i (%i)   %d (%i)", str2, cnt2, AreaFillToGerber->NrPolygons - 1, cnt4,
				        MaxRoundings);
				RedrawInfoStr(1);
				MemSize = MemSizePolygon(DrawPolygon) + 10240;
				AllocateSpecialMem(MEM_POLYGON_BIGGER, MemSize, (void **) &ChangedPolygon);

				if (cnt2 == 0)
					MakeBiggerSmallerPolygon(DrawPolygon, ChangedPolygon, NewThickNess, 1);
				else
					MakeBiggerSmallerPolygon(DrawPolygon, ChangedPolygon, NewThickNess, 0);

				count2 = ChangedPolygon->NrVertices;

				for (cnt3 = 0; cnt3 < count2; cnt3++)
				{
					x33 = (*ChangedPolygon).Points[cnt3].x;
					y33 = (*ChangedPolygon).Points[cnt3].y;
					/*
					#ifdef _DEBUG
					          sprintf(str,"Rounding points  %14.5f,%14.5f %i  %i",x33,y33,cnt3,cnt4);
					          WriteLn(Plotfp,str);
					#endif
					*/
				}

				for (cnt3 = 0; cnt3 < count2; cnt3++)
				{
					PolygonNr = cnt2;
					RoundingNr = cnt4;
					RoundingCnt = cnt3;

					x11 = (*ChangedPolygon).Points[cnt3].x;
					y11 = (*ChangedPolygon).Points[cnt3].y;

					if (cnt3 < count - 1)
					{
						x22 = (*ChangedPolygon).Points[cnt3 + 1].x;
						y22 = (*ChangedPolygon).Points[cnt3 + 1].y;
					}
					else
					{
						x22 = (*ChangedPolygon).Points[0].x;
						y22 = (*ChangedPolygon).Points[0].y;
					}

					if (mode == 0)
					{
						//           &&
						//           (cnt4!=0)) {
						//            res=0;
						/*
						#ifdef _DEBUG
						            OkToDrawPolygon=0;
						            if ((cnt4==2)
						               &&
						               (cnt2==288)
						               &&
						               (cnt3==21)) {
						              DrawLineGreen(x11,y11,x22,y22);
						              ok=1;
						              OkToDrawPolygon=1;
						            }
						#endif
						*/
						LineShorten = 0;

						if ((res =
						            CheckLineCrossesWithAreaFill(x11, y11, x22, y22, AreaFillToGerber, &xx1, &yy1, &xx2, &yy2,
						                    Thickness1 * 0.99, 0)) != -1)
						{
							if (res == 0)
							{
								xx1 = x11;
								yy1 = y11;
								xx2 = x22;
								yy2 = y22;
								LineShorten = 1;
							}
							else
							{
								/*
								#ifdef _DEBUG
								                Length=CalcLengthLine(x11,y11,x22,y22);
								                NewLength=CalcLengthLine(xx1,yy1,xx2,yy2);
								                if ((cnt4==0)
								                   &&
								                   (Length>2e5)
								                   &&
								                   (NewLength<1e5)) {
								  //                DrawLineWhite(xx1,yy1,xx2,yy2);
								  //                DrawLineYellow(x11,y11,x22,y22);
								                  ok=1;
								                }
								  #endif
								  */
							}

#ifdef _DEBUG
							CheckPlotLine2(xx1, yy1, xx2, yy2);
#endif

							if (cnt2 != -1)
								GerberWriteLine(xx1, yy1, xx2, yy2);

							AperTure->Used++;
						}

						if (res != 0)
						{
							if ((res2 =
							            CheckLineCrossesWithAreaFill(x22, y22, x11, y11, AreaFillToGerber, &xx1, &yy1, &xx2,
							                    &yy2, Thickness1 * 0.99, 0)) != -1)
							{
								if (res2 == 0)
								{
									xx1 = x11;
									yy1 = y11;
									xx2 = x22;
									yy2 = y22;
									LineShorten = 1;
								}
								else
								{
									/*
									#ifdef _DEBUG
									                  Length=CalcLengthLine(x11,y11,x22,y22);
									                  NewLength=CalcLengthLine(xx1,yy1,xx2,yy2);
									                  if ((cnt4==0)
									                     &&
									                     (Length>2e5)
									                     &&
									                     (NewLength<1e5)) {
									                    ok=1;
									  //                  DrawLineWhite(xx1,yy1,xx2,yy2);
									  //                  DrawLineYellow(x11,y11,x22,y22);
									                  }
									#endif
									*/
								}

#ifdef _DEBUG
								CheckPlotLine2(xx1, yy1, xx2, yy2);
#endif

								if (cnt2 != -1)
									GerberWriteLine(xx1, yy1, xx2, yy2);

								AperTure->Used++;
							}
						}

						/*
						#ifdef _DEBUG
						            if ((cnt4==0)
						               &&
						               (res==-1)
						               &&
						               (res2==-1)) {
						              Length=CalcLengthLine(x11,y11,x22,y22);
						              if (Length>1e5) {
						  //              DrawLineYellow(x11,y11,x22,y22);
						              }
						            }
						            if ((cnt2==0)
						               &&
						               (LineShorten)
						               &&
						               (InRange(y11,y22))
						               &&
						               (fabs(x22-x11)>10e5)) {
						              NrShortenLines++;
						            }
						#endif
						*/
					}
					else
					{
#ifdef _DEBUG
						CheckPlotLine2(x11, y11, x22, y22);
#endif

						if (cnt2 != -1)
							GerberWriteLine(xx1, yy1, xx2, yy2);

						AperTure->Used++;
					}
				}

				/*
				#ifdef _DEBUG
				        WriteLn(Plotfp,"-----------------------------------------------------------------------");
				#endif
				*/
			}
		}

//    sprintf(InfoStr,"Areafill Layer %i [%i (%i)]",AreaFill->Layer,cnt2,AreaFill->NrPolygons);
//    RedrawInfoStr(1);
		PolygonPos += sizeof(PolygonInitRecord) + count * sizeof(PointRecord);
		DrawPolygon = (PolygonRecord *) PolygonPos;
		cnt2++;
	}

	if (SelectionEsc)
	{
		MessageBoxOwn(PCBWindow, SC(423, "Plotting areafill has been stopped"), SC(118, "Warning"),
		              MB_APPLMODAL | MB_OK);
	}

// *****************************************************************************************************************
// *****************************************************************************************************************

	AperTureNr = CheckTraceAperTure(Thickness2);
	AperTure = &((*AperTures)[AperTureNr]);
	CurrentAperTureNr = AperTureNr;

	if (!PenPlotMode)
	{
		sprintf(str, "G54D%i*", AperTure->AperTureCode);
		WriteGerberString(str, 1);
	}
	else
	{
//    WriteGerberString("");
		if (GerberInfo.PenSizes[1] > 0.0)
			sprintf(str, "PU;SP2;");
		else
			sprintf(str, "PU;");

		WriteGerberString(str, 0);
	}


//  Thickness_2=Thickness1*0.5;
	Thickness_1 = Thickness2 * 0.25;
	SearchMaxY = maxy + Thickness2 * 0.025;
	PaintY = maxy - Thickness2 * 0.51;

	if (!PenPlotMode)
	{
		IncY = Thickness2 * 0.8;
		NrLines = (int32) ((maxy - miny) / (Thickness2 * 0.8));
	}
	else
	{
		IncY = Thickness2 * 0.8;
		NrLines = (int32) ((maxy - miny) / (Thickness2 * 0.8));
	}

	AreaPos = (uint8 *) AreaFillToGerber;

//  NrLines=5;

	NrLines++;
	cnt4 = 0;

//  cnt4=NrLines;

	AllocateSpecialMem(MEM_PLOT_LINES_BUF, 256 * 1024, (void **) &LinesBufCopy);
	AllocateSpecialMem(MEM_PLOT_POINTS_MAP, 128 * 1024, (void **) &PointsMapCopy);

	while ((cnt4 < NrLines) && (!SelectionEsc))
	{
		if (cnt4 == NrLines - 1)
		{
			PaintY = miny + Thickness2 * 0.51;
			SearchMaxY = miny + Thickness2 * 1.025;
		}

		PolygonNr = -1;
		RoundingNr = -1;
		RoundingCnt = cnt4;

		sprintf(InfoStr, "%s  %i (%i)", str2, cnt4, NrLines - 1);
		RedrawInfoStr(1);

		if ((cnt4 % 20) == 0)
			CheckForEscape();

		SearchMinY = SearchMaxY - Thickness2 * 1.05;
		LineMinX = 1e9;
		LineMaxX = -1e9;
		NrSegmentLines = 0;
		Polygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) Polygon;
		LinesBuf = LinesBufCopy;

		for (cnt = 0; cnt < AreaFillToGerber->NrPolygons; cnt++)
		{
			count = Polygon->NrVertices;

			if ((Polygon->maxy + 100 > SearchMinY) && (Polygon->miny - 100 < SearchMaxY))
			{
				x1 = (double *) (*Polygon).Points;
				y1 = x1 + 1;
				FirstX = x1;
				FirstY = y1;

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

					if ((min(*y1, *y2) < SearchMaxY) && (max(*y1, *y2) > SearchMinY) && (NrSegmentLines < 8192))
					{
						*LinesBuf++ = *x1;
						*LinesBuf++ = *y1;
						*LinesBuf++ = *x2;
						*LinesBuf++ = *y2;
						NrSegmentLines++;
						LineMinX = min(LineMinX, min(*x1, *x2));
						LineMaxX = max(LineMaxX, max(*x1, *x2));
					}

					x1 += 2;
					y1 += 2;
				}
			}

			PolygonPos += MemSizePolygon(Polygon);
			Polygon = (PolygonRecord *) PolygonPos;
		}

		NrPoints = (int32) ((LineMaxX - LineMinX) / Thickness_1) + 8;
		PointX = LineMinX - Thickness2;
		PointsMap = PointsMapCopy;
		memset(PointsMap, 0, 128 * 1024);
		PointY = PaintY;
		R = Thickness2 * 0.5;

		for (cnt2 = 0; cnt2 < min(NrPoints, 128 * 1024); cnt2++)
		{
			LinesBuf = LinesBufCopy;

			for (cnt3 = 0; cnt3 < NrSegmentLines; cnt3++)
			{
				x3 = *LinesBuf++;
				y3 = *LinesBuf++;
				x4 = *LinesBuf++;
				y4 = *LinesBuf++;
				Calcs4++;

				if ((min(x3, x4) < PointX + R) && (max(x3, x4) > PointX - R) && (min(y3, y4) < PointY + R)
				        && (max(y3, y4) > PointY - R)
				        && (CheckLineCrossesCircle2(x3, y3, x4, y4, PointX, PointY, Thickness2)))
					*PointsMap = 1;
			}

			PointsMap++;
			PointX += Thickness_1;
		}

		LineOn = 1;
		FoundFirstLine = 0;

		if ((cnt4 & 1) == 0)
		{
// *****************************************************************************************************************
// Filling line from left to right
			PointX = LineMinX - Thickness2;
			PointsMap = PointsMapCopy;

			for (cnt2 = 0; cnt2 < min(NrPoints, 128 * 1024); cnt2++)
			{
				if (*PointsMap++ == 1)
				{
					if (LineOn)
					{
						LineOn = 0;

						if (FoundFirstLine)
						{
							LineX2 = PointX - Thickness_1;

							if (CheckPointInAreaFill(LineX1, PointY, AreaFillToGerber) == 1)
							{
#ifdef _DEBUG
								CheckPlotLine2(LineX1, PointY, LineX2, PointY);
#endif

								if (!PenPlotMode)
									GerberWriteLine(LineX1, PointY, LineX2, PointY);
								else
								{
									if (!DrawWithTwoPens)
										DrawGerberLine(LineX1, PointY, LineX2, PointY, Thickness1, Thickness2);
									else
										DrawGerberLine(LineX1, PointY, LineX2, PointY, Thickness2, Thickness2);
								}

								AperTure->Used++;
							}
						}
					}
				}
				else
				{
					if (!LineOn)
					{
						LineOn = 1;
						FoundFirstLine = 1;
						LineX1 = PointX;
					}
				}

				PointX += Thickness_1;
			}
		}
		else
		{
// *****************************************************************************************************************
// Filling line from right to left
//      PointsMap=(uint8 *)&Buf[131072+NrPoints];
			PointsMap = PointsMapCopy + NrPoints;
			PointX = LineMinX - Thickness2 + (float) NrPoints *Thickness_1;

			for (cnt2 = 0; cnt2 < min(NrPoints, 128 * 1024); cnt2++)
			{
				if (*PointsMap-- == 1)
				{
					if (LineOn)
					{
						LineOn = 0;

						if (FoundFirstLine)
						{
							LineX2 = PointX + Thickness_1;

							if (CheckPointInAreaFill(LineX1, PointY, AreaFillToGerber) == 1)
							{
#ifdef _DEBUG
								CheckPlotLine2(LineX1, PointY, LineX2, PointY);
#endif

								if (!PenPlotMode)
									GerberWriteLine(LineX1, PointY, LineX2, PointY);
								else
								{
									if (!DrawWithTwoPens)
										DrawGerberLine(LineX1, PointY, LineX2, PointY, Thickness1, Thickness2);
									else
										DrawGerberLine(LineX1, PointY, LineX2, PointY, Thickness2, Thickness2);
								}

								AperTure->Used++;
							}
						}
					}
				}
				else
				{
					if (!LineOn)
					{
						LineOn = 1;
						FoundFirstLine = 1;
						LineX1 = PointX;
					}
				}

				PointX -= Thickness_1;
			}
		}

// *****************************************************************************************************************
		SearchMaxY -= IncY;
		PaintY -= IncY;
//    sprintf(InfoStr,"Areafill Layer %i [%i (%i)]",AreaFill->Layer,cnt4,NrLines);
//    RedrawInfoStr(1);
		cnt4++;
	}

	if (SelectionEsc)
	{
		MessageBoxOwn(PCBWindow, SC(423, "Plotting areafill has been stopped"), SC(118, "Warning"),
		              MB_APPLMODAL | MB_OK);
	}

	if (AreaFillChanged)
	{
		DeAllocateMemAreaFills();
		DeAllocateMemPolygons();
	}

//  Calcs4
	sprintf(InfoStr, "%s", str2);
	RedrawInfoStr(1);
// NrCalcs1,NrCalcs2,NrCalcs3
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PowerPlaneToGerber(int32 Layer, double Thickness1, double Thickness2, int32 mode)
{
	int32 cnt, cnt2, cnt3, count, ok, NrLayerPolygons, NrDeletionPolygons, PowerPlaneNr, LayerPolygonsInfo[256],
	      DeletionPolygonsInfo[256];
	double x11, y11, xmin, xmax, ymin, ymax;
	PolygonRecord *LayerPolygons[256], *DeletionPolygons[256], *DeletionPolygon, *DrawPolygon, *PowerPlanePolygon,
	              *TempPolygon;
	uint8 *AreaPos, *TempAreaPos, *PolygonPos;
	AreaFillRecord *AreaFill, *PowerAreaFill, *TempAreaFill;

	AllocateSpecialMem(MEM_POWERPLANE_AREAFILL, 128 * 1024, (void **) &TempAreaFill);
	TempAreaFill->NrPolygons = 2;
	TempAreaFill->NetNr = 0;
	TempAreaFill->Layer = Layer;
	TempAreaFill->Clearance = 0.0;
	TempAreaFill->MemSize = sizeof(AreaFillRecord);
	xmin = DesignBoardOriginX - 100 * 2540.0;
	ymin = DesignBoardOriginY - 100 * 2540.0;
	xmax = DesignBoardOriginX + DesignBoardWidth + 100 * 2540.0;
	ymax = DesignBoardOriginY + DesignBoardHeight + 100 * 2540.0;
	TempAreaPos = (uint8 *) TempAreaFill;
	TempAreaPos += sizeof(AreaFillRecord);
	TempPolygon = (PolygonRecord *) TempAreaPos;
	TempPolygon->Points[0].x = xmin;
	TempPolygon->Points[0].y = ymin;
	TempPolygon->Points[1].x = xmin;
	TempPolygon->Points[1].y = ymax;
	TempPolygon->Points[2].x = xmax;
	TempPolygon->Points[2].y = ymax;
	TempPolygon->Points[3].x = xmax;
	TempPolygon->Points[3].y = ymin;
	TempAreaFill->minx = xmin;
	TempAreaFill->miny = ymin;
	TempAreaFill->maxx = xmax;
	TempAreaFill->maxy = ymax;
	TempPolygon->NrVertices = 4;
	TempAreaPos += MemSizePolygon(TempPolygon);
	TempAreaFill->MemSize += MemSizePolygon(TempPolygon);

	TempPolygon = (PolygonRecord *) TempAreaPos;


	PowerPlaneNr = GetPowerPlaneByLayer(Layer);

	if (PowerPlaneNr == -1)
		return -1;

	PowerAreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[PowerPlaneNr]]);

	NrDeletionPolygons = 0;
	AreaPos = (uint8 *) PowerAreaFill;
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt2 = 0; cnt2 < PowerAreaFill->NrPolygons; cnt2++)
	{
		if (cnt2 > 0)
		{
			DeletionPolygons[NrDeletionPolygons] = DrawPolygon;
			DeletionPolygonsInfo[NrDeletionPolygons++] = 0;
		}

		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	AreaPos = (uint8 *) PowerAreaFill;
	PowerPlanePolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	CopyPolygonToPolygon(PowerPlanePolygon, TempPolygon);
	TempAreaFill->MemSize += MemSizePolygon(TempPolygon);
	TempAreaPos += MemSizePolygon(TempPolygon);
	TempPolygon = (PolygonRecord *) TempAreaPos;

	NrLayerPolygons = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((cnt != PowerPlaneNr) && ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
		{
			AreaPos = (uint8 *) AreaFill;
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));


			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				if (cnt2 == 0)
				{
					LayerPolygons[NrLayerPolygons] = DrawPolygon;
					LayerPolygonsInfo[NrLayerPolygons++] = 0;
				}
				else
				{
//           ((DrawPolygon->PolygonType & 1) == 1)) {
					DeletionPolygons[NrDeletionPolygons] = DrawPolygon;
					DeletionPolygonsInfo[NrDeletionPolygons++] = 0;
				}

				count = DrawPolygon->NrVertices;
				PolygonPos = (uint8 *) DrawPolygon;
				PolygonPos += MemSizePolygon(DrawPolygon);
				DrawPolygon = (PolygonRecord *) PolygonPos;
			}
		}
	}

#ifdef _DEBUG

	if (Layer == 1)
		ok = 1;

#endif

	for (cnt = 0; cnt < NrLayerPolygons; cnt++)
	{
		if (LayerPolygonsInfo[cnt] == 0)
		{
			DrawPolygon = LayerPolygons[cnt];

			if (CheckPolygonOverlapAreaFill(DrawPolygon, TempAreaFill))
			{
				CopyPolygonToPolygon(DrawPolygon, TempPolygon);
				TempAreaFill->MemSize += MemSizePolygon(TempPolygon);
				TempAreaPos += MemSizePolygon(TempPolygon);
				TempPolygon = (PolygonRecord *) TempAreaPos;
				TempAreaFill->NrPolygons++;
				LayerPolygonsInfo[cnt] = 1;
			}
		}
	}

#ifdef _DEBUG

	if (Layer == 1)
		ok = 1;

#endif

	PlotAreaFillToGerber(TempAreaFill, Thickness1, Thickness2, mode & 1);

	ok = 1;

	for (cnt2 = 0; cnt2 < NrDeletionPolygons; cnt2++)
	{
		if (DeletionPolygonsInfo[cnt2] == 0)
		{
			DeletionPolygon = DeletionPolygons[cnt2];
			count = DeletionPolygon->NrVertices;
			xmin = 1e9;
			ymin = 1e9;
			xmax = -1e9;
			ymax = -1e9;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				x11 = DeletionPolygon->Points[cnt3].x;
				y11 = DeletionPolygon->Points[cnt3].y;
				xmin = min(xmin, x11);
				ymin = min(ymin, y11);
				xmax = max(xmax, x11);
				ymax = max(ymax, y11);
			}

			TempAreaFill->NrPolygons = 1;
			TempAreaFill->NetNr = 0;
			TempAreaFill->Layer = Layer;
			TempAreaFill->Clearance = 0.0;
			TempAreaFill->MemSize = sizeof(AreaFillRecord);
			TempAreaFill->minx = xmin;
			TempAreaFill->miny = ymin;
			TempAreaFill->maxx = xmax;
			TempAreaFill->maxy = ymax;
			TempAreaPos = (uint8 *) TempAreaFill;
			TempAreaPos += sizeof(AreaFillRecord);
			TempPolygon = (PolygonRecord *) TempAreaPos;
			CopyPolygonToPolygon(DeletionPolygon, TempPolygon);
			TempAreaPos += MemSizePolygon(DeletionPolygon);
			TempAreaFill->MemSize += MemSizePolygon(DeletionPolygon);

			for (cnt = 0; cnt < NrLayerPolygons; cnt++)
			{
				if (LayerPolygonsInfo[cnt] == 0)
				{
					DrawPolygon = LayerPolygons[cnt];

					if (CheckPolygonCompleetlyInsidePolygon(DrawPolygon, DeletionPolygon))
					{
						TempPolygon = (PolygonRecord *) TempAreaPos;
						CopyPolygonToPolygon(DrawPolygon, TempPolygon);
						TempAreaFill->MemSize += MemSizePolygon(DrawPolygon);
						TempAreaPos += MemSizePolygon(DrawPolygon);
						LayerPolygonsInfo[cnt] = 1;
						TempAreaFill->NrPolygons++;
					}
				}
			}

			PlotAreaFillToGerber(TempAreaFill, Thickness1, Thickness2, mode & 1);
			DeletionPolygonsInfo[cnt2] = 1;
		}
	}

	return 1;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 GerberPlotPolygon(PolygonRecord * DrawPolygon, int32 mode)
{
	int32 count, cnt3;
	char str[MAX_LENGTH_STRING];
	double x11, y11;

	if ((DrawPolygon->PolygonType & 8) == 0)
		sprintf(str, "%%LPD*%%G36*");
	else
		sprintf(str, "%%LPC*%%G36*");

	WriteGerberString(str, 1);
	LastGerberX = -1e10;
	LastGerberY = -1e10;
	count = DrawPolygon->NrVertices;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		x11 = (*DrawPolygon).Points[cnt3].x;
		y11 = (*DrawPolygon).Points[cnt3].y;

		if (GerberInfo.Invert)
			x11 = (DesignBoardOriginX + DesignBoardWidth - x11);

		GetGerberValueInString(x11, y11, (LPSTR) & str, 0);

		if (cnt3 == 0)
			strcat(str, "D02*");
		else
			strcat(str, "D01*");

		WriteGerberString(str, 1);
	}

	x11 = (*DrawPolygon).Points[0].x;
	y11 = (*DrawPolygon).Points[0].y;

	if (GerberInfo.Invert)
		x11 = (DesignBoardOriginX + DesignBoardWidth - x11);

	GetGerberValueInString(x11, y11, (LPSTR) & str, 0);
	strcat(str, "D01*");
	WriteGerberString(str, 1);
	sprintf(str, "G37*");
	WriteGerberString(str, 1);

	if ((mode & 1) == 0)
	{
		sprintf(str, "%%LPD*%%");
		WriteGerberString(str, 1);
	}

	return 0;
}

// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 GerberPlotSpecialAreaFill(AreaFillRecord * AreaFill, int32 mode)
{
	int32 cnt2, cnt3;
	uint8 *PolygonPos, *PolygonPos2, *DestPolygonPos;
	PolygonRecord *DrawPolygon, *DrawPolygon2, *DestPolygon;
	AreaFillRecord *TempAreaFill;

	AllocateSpecialMem(MEM_POWERPLANE_AREAFILL, max(AreaFill->MemSize, 128 * 1024), (void **) &TempAreaFill);
	memset(TempAreaFill, 0, sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) AreaFill + sizeof(AreaFillRecord);
	DrawPolygon = (PolygonRecord *) PolygonPos;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		if (DrawPolygon->PolygonType & 8)
		{
			DestPolygonPos = (uint8 *) TempAreaFill + sizeof(AreaFillRecord);
			DestPolygon = (PolygonRecord *) DestPolygonPos;
			TempAreaFill->NrPolygons = 1;
			CopyPolygonToPolygon(DrawPolygon, DestPolygon);
			TempAreaFill->minx = DestPolygon->minx;
			TempAreaFill->miny = DestPolygon->miny;
			TempAreaFill->maxx = DestPolygon->maxx;
			TempAreaFill->maxy = DestPolygon->maxy;
			DestPolygonPos += MemSizePolygon(DestPolygon);
			TempAreaFill->MemSize = sizeof(AreaFillRecord) + MemSizePolygon(DestPolygon);
			DestPolygon = (PolygonRecord *) DestPolygonPos;

			PolygonPos2 = (uint8 *) AreaFill + sizeof(AreaFillRecord);
			DrawPolygon2 = (PolygonRecord *) PolygonPos2;

			for (cnt3 = 0; cnt3 < AreaFill->NrPolygons; cnt3++)
			{
				if ((DrawPolygon2->PolygonType & 8) == 0)
				{
					if (CheckPolygonCompleetlyInsidePolygon(DrawPolygon2, DrawPolygon) == 1)
					{
						CopyPolygonToPolygon(DrawPolygon2, DestPolygon);
						DestPolygonPos += MemSizePolygon(DestPolygon);
						DestPolygon = (PolygonRecord *) DestPolygonPos;
						TempAreaFill->MemSize += MemSizePolygon(DestPolygon);
						TempAreaFill->NrPolygons++;
					}
				}

				PolygonPos2 += MemSizePolygon(DrawPolygon2);
				DrawPolygon2 = (PolygonRecord *) PolygonPos2;
			}

			if (PenPlotMode)
				PlotAreaFillToGerber(TempAreaFill, AreafillPenSize1, AreafillPenSize2, 2);
			else
				PlotAreaFillToGerber(TempAreaFill, AreafillPenSize1, AreafillPenSize2, 2);
		}

		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
