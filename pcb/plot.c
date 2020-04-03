/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: plot.c
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
#include "stdio.h"
#include "resource.h"
#include "calc.h"
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
#include "nets2.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "select3.h"
#include "polygon.h"
#include "mainloop.h"
#include "toets.h"
#include "rect.h"
#include "time.h"
#include "plot.h"
#include "dialogs.h"
#include "owntime.h"
#include "font.h"
#include "ctype.h"
#include "../functionsc/version.h"


#define COMP_OUTPUT_POS_VALUE         8
#define COMP_OUTPUT_POS_X             35
#define COMP_OUTPUT_POS_Y             45
#define COMP_OUTPUT_POS_ROTATION      55
#define COMP_OUTPUT_POS_LAYER         62
#define COMP_OUTPUT_POS_COMP_TYPE     70

#define DASH_STRING               "--------------------------------------------------------------------------"
#define SPACE_STRING              "                                                                          "

extern AperTureArray *AperTures, *AperTures2, *DrillAperTures;
extern HGLOBAL AperTuresGlobal, DrillAperTuresGlobal;
extern AperTureArray *LoadedAperTures;
extern HGLOBAL LoadedAperTuresGlobal;

extern AperTureRecord LastRoundPadAperTure, LastRectPadAperTure, LastPlatedDrillAperTure, LastUnPlatedDrillAperTure,
       LastThermalReliefAperTure, LastTraceAperTure, NewAperTure, LastPolygonPadAperTure;

extern AperTureRecord LastButterflyPadAperTure;

extern int32 MaxNrAperTures, NrAperTures, NrDrillAperTures, NrAperTures2, NrLoadedAperTures;

extern int32 GerberMode;

extern int32 DcodeTrace, DcodeRoundPad, DcodeRectPad, DcodeDrill, DcodeThermalRelief, DcodePolygonPad,
       DcodeButterflyPad, PlotErrors, InfoLayerPlotWarnings, BoardOutlinePlotWarnings;

extern int Gerberfp, Drillfp, Plotfp;

extern double TextMinX, TextMinY, TextMaxX, TextMaxY, CurrentPlotPenSize, TextPen, DesignBoardOriginX,
       DesignBoardOriginY, DesignBoardWidth, DesignBoardHeight;

extern int32 PlotDrawingOk, PenPlotMode;
extern char LineBufGerber[MAX_LENGTH_STRING], GerberListFilename[MAX_LENGTH_STRING];

int32 ObjectsApertureIndex[512], ObjectsApertureCount[512];
int32 CurrentAperTureNr, ok;


extern int32 ObjectsNetHorTraces, ObjectsNetVerTraces, ObjectsNetDiag1Traces, ObjectsNetDiag2Traces, ObjectsNetVias,
       ObjectsNetPins, NrTracePoints, ObjectsNetConnecions;

extern int32 ProjectIndexNr;
extern ProjectInfoRecord *ProjectInfo;

int32 TracePointsNumbers[512];

TracePointsArray *TracePoints;

CompPosOutputRecord CompPosOutput;

double LastGerberX, LastGerberY;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MakeString(LPSTR outstr, LPSTR instr, int32 length, int32 mode)
{
	char str[MAX_LENGTH_STRING];

	if (length < 2)
	{
		outstr[0] = 0;
		return;
	}

	memset(str, 0, sizeof(str));

	if (length - (int32) strlen(instr) > 0)
	{
		if ((mode & 1) == 0)
			strncpy(str, SPACE_STRING, length - strlen(instr));
		else
			str[0] = ' ';

		sprintf(outstr, "%s%s", instr, str);
	}
	else
	{
		strncpy(outstr, instr, length - 1);
		outstr[length - 1] = 0;
		strcat(outstr, " ");
	}

	switch (mode & 3)
	{
	case 0:
		outstr[strlen(outstr) - 1] = ' ';
		break;

	case 1:
		outstr[strlen(outstr) - 1] = '\t';
		break;

	case 2:
		outstr[strlen(outstr) - 1] = '|';
		break;

	case 3:
		outstr[strlen(outstr) - 1] = '+';
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WriteDrillApertureFile(int32 fp, int32 mode);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemAperTures(int32 count)
{
	HGLOBAL NewMem;

	if (count > 2000)
		return -2;

	if (MaxNrAperTures == 0)
	{
		count = max(count, 64);

		if ((AperTuresGlobal = GlobalAlloc(GHND, count * sizeof(AperTureRecord))) == NULL)
			return -1;

		if ((AperTures = (AperTureArray *) GlobalLock(AperTuresGlobal)) == NULL)
			return -1;

		if ((DrillAperTuresGlobal = GlobalAlloc(GHND, count * sizeof(AperTureRecord))) == NULL)
			return -1;

		if ((DrillAperTures = (AperTureArray *) GlobalLock(DrillAperTuresGlobal)) == NULL)
			return -1;

		if ((LoadedAperTuresGlobal = GlobalAlloc(GHND, count * sizeof(AperTureRecord))) == NULL)
			return -1;

		if ((LoadedAperTures = (AperTureArray *) GlobalLock(AperTuresGlobal)) == NULL)
			return -1;

		MaxNrAperTures = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(DrillAperTuresGlobal, count * sizeof(AperTureRecord), GHND)) == NULL)
			return -1;

		DrillAperTuresGlobal = NewMem;

		if ((DrillAperTures = (AperTureArray *) GlobalLock(DrillAperTuresGlobal)) == NULL)
			return -1;

		if ((NewMem = GlobalReAlloc(AperTuresGlobal, count * sizeof(AperTureRecord), GHND)) == NULL)
			return -1;

		AperTuresGlobal = NewMem;

		if ((AperTures = (AperTureArray *) GlobalLock(AperTuresGlobal)) == NULL)
			return -1;

		if ((NewMem = GlobalReAlloc(LoadedAperTuresGlobal, count * sizeof(AperTureRecord), GHND)) == NULL)
			return -1;

		LoadedAperTuresGlobal = NewMem;

		if ((LoadedAperTures = (AperTureArray *) GlobalLock(LoadedAperTuresGlobal)) == NULL)
			return -1;

		MaxNrAperTures = count;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMemAperTures()
{
	if (AperTuresGlobal != NULL)
	{
		GlobalUnlock(AperTuresGlobal);
		GlobalFree(AperTuresGlobal);
		GlobalUnlock(DrillAperTuresGlobal);
		GlobalFree(DrillAperTuresGlobal);
		MaxNrAperTures = 0;
		AperTuresGlobal = NULL;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteAperTures()
{
	NrAperTures = 0;

	LastTraceAperTure.AperTureNr = 0;
	LastTraceAperTure.x = 0.0;

	LastRoundPadAperTure.AperTureNr = 0;
	LastRoundPadAperTure.x = 0.0;

	LastRectPadAperTure.AperTureNr = 0;
	LastRectPadAperTure.x = 0.0;
	LastRectPadAperTure.y = 0.0;

	LastButterflyPadAperTure.AperTureNr = 0;
	LastButterflyPadAperTure.x = 0.0;

	LastPlatedDrillAperTure.AperTureNr = 0;
	LastPlatedDrillAperTure.x = 0.0;

	LastUnPlatedDrillAperTure.AperTureNr = 0;
	LastUnPlatedDrillAperTure.x = 0.0;
	memset(AperTures, 0, sizeof(AperTureRecord) * MaxNrAperTures);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckTraceAperTure(double ThickNess)
{
	int32 cnt, ok;
	AperTureRecord *AperTure;

#ifdef _DEBUG

	if (InRange4(ThickNess, 3.048e5))
		ok = 1;

#endif

	if (ThickNess < 100.0)
		ok = 1;

	if (NrAperTures > 0)
	{
		if (InRange8(LastTraceAperTure.x, ThickNess))
		{
			AperTure = &((*AperTures)[LastTraceAperTure.AperTureNr]);
			AperTure->Used++;
			return LastTraceAperTure.AperTureNr;
		}

		cnt = 0;

		while ((cnt < NrAperTures)
		        && (((*AperTures)[cnt].Info != GERBER_TRACE) || (NotInRange8((*AperTures)[cnt].x, ThickNess))))
			cnt++;

		if (cnt < NrAperTures)
		{
			AperTure = &((*AperTures)[cnt]);
			AperTure->Used++;
		}
		else
		{
			if (NrAperTures + 1 >= MaxNrAperTures)
			{
				if (AllocateMemAperTures(NrAperTures + 64) != 0)
					return -1;
			}

			AperTure = &((*AperTures)[cnt]);
#ifdef _DEBUG

			if (DcodeTrace == 402)
				ok = 1;

#endif
			AperTure->Info = GERBER_TRACE;
			AperTure->Info3 = 0;
			AperTure->x = ThickNess;
			AperTure->AperTureNr = cnt;
			AperTure->Used = 1;
			AperTure->HoleSize = 0.0;
			AperTure->AperTureCode = DcodeTrace;
			DcodeTrace++;
			NrAperTures++;
		}

		LastTraceAperTure.x = ThickNess;
		LastTraceAperTure.AperTureNr = cnt;
		return cnt;
	}

	if (NrAperTures + 1 >= MaxNrAperTures)
	{
		if (AllocateMemAperTures(NrAperTures + 64) != 0)
			return -1;
	}

	AperTure = &((*AperTures)[0]);
	AperTure->Info = GERBER_TRACE;
	AperTure->x = ThickNess;
	AperTure->AperTureNr = 0;
	AperTure->Used = 1;
	AperTure->Info3 = 0;
	AperTure->HoleSize = 0.0;
	AperTure->AperTureCode = DcodeTrace;
	DcodeTrace++;
	LastTraceAperTure.AperTureNr = 0;
	LastTraceAperTure.x = ThickNess;
	LastRoundPadAperTure.x = 0.0;
	LastRectPadAperTure.x = 0.0;
	LastThermalReliefAperTure.x = 0.0;
	LastPolygonPadAperTure.Address = NULL;
	NrAperTures++;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckRoundPadAperTure(double ThickNess)
{
	int32 cnt, ok;
	AperTureRecord *AperTure;

	if (ThickNess < 100.0)
		ok = 1;

	if (NrAperTures > 0)
	{
		if (InRange8(LastRoundPadAperTure.x, ThickNess))
		{
			AperTure = &((*AperTures)[LastRoundPadAperTure.AperTureNr]);
			AperTure->Used++;
			return LastRoundPadAperTure.AperTureNr;
		}

		cnt = 0;

		while ((cnt < NrAperTures)
		        && (((*AperTures)[cnt].Info != GERBER_PAD_ROUND) || (NotInRange8((*AperTures)[cnt].x, ThickNess))))
			cnt++;

		if (cnt < NrAperTures)
		{
			AperTure = &((*AperTures)[cnt]);
			AperTure->Used++;
		}
		else
		{
			if (NrAperTures + 1 >= MaxNrAperTures)
			{
				if (AllocateMemAperTures(NrAperTures + 64) != 0)
					return -1;
			}

			AperTure = &((*AperTures)[cnt]);
			AperTure->Info = GERBER_PAD_ROUND;
			AperTure->Info3 = 0;
			AperTure->x = ThickNess;
			AperTure->AperTureNr = cnt;
			AperTure->AperTureCode = DcodeRoundPad;
			DcodeRoundPad++;
			AperTure->Used = 1;
			NrAperTures++;
		}

		LastRoundPadAperTure.x = ThickNess;
		LastRoundPadAperTure.AperTureNr = cnt;
		return cnt;
	}

	if (NrAperTures + 1 >= MaxNrAperTures)
	{
		if (AllocateMemAperTures(NrAperTures + 64) != 0)
			return -1;
	}

	AperTure = &((*AperTures)[0]);
	AperTure->Info = GERBER_PAD_ROUND;
	AperTure->x = ThickNess;
	AperTure->AperTureNr = 0;
	AperTure->Used = 1;
	AperTure->AperTureCode = DcodeRoundPad;
	DcodeRoundPad++;
	AperTure->Info3 = 0;
	LastRoundPadAperTure.AperTureNr = 0;
	LastRoundPadAperTure.x = ThickNess;
	LastThermalReliefAperTure.x = 0.0;
	LastTraceAperTure.x = 0.0;
	LastButterflyPadAperTure.x = 0.0;
	LastRectPadAperTure.x = 0.0;
	LastRectPadAperTure.y = 0.0;
	LastPolygonPadAperTure.Address = NULL;
	NrAperTures++;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckThermalReliefAperTure(double InsideDiameter, double OutsideDiameter, double CrossHairThickness)
{
	int32 cnt;
	AperTureRecord *AperTure;

	if (NrAperTures > 0)
	{
		if ((InRange8(LastThermalReliefAperTure.x, InsideDiameter))
		        && (InRange8(LastThermalReliefAperTure.y, OutsideDiameter))
		        && (InRange8(LastThermalReliefAperTure.x2, CrossHairThickness)))
		{
			AperTure = &((*AperTures)[LastThermalReliefAperTure.AperTureNr]);
			AperTure->Used++;
			return LastThermalReliefAperTure.AperTureNr;
		}

		cnt = 0;

		while ((cnt < NrAperTures)
		        && (((*AperTures)[cnt].Info != GERBER_PAD_THERMAL_RELIEF)
		            || (NotInRange8((*AperTures)[cnt].x, InsideDiameter))
		            || (NotInRange8((*AperTures)[cnt].y, OutsideDiameter))
		            || (NotInRange8((*AperTures)[cnt].x2, CrossHairThickness))))
			cnt++;

		if (cnt < NrAperTures)
		{
			AperTure = &((*AperTures)[cnt]);
			AperTure->Used++;
		}
		else
		{
			if (NrAperTures + 1 >= MaxNrAperTures)
			{
				if (AllocateMemAperTures(NrAperTures + 64) != 0)
					return -1;
			}

			AperTure = &((*AperTures)[cnt]);
			AperTure->Info = GERBER_PAD_THERMAL_RELIEF;
			AperTure->Info3 = 0;
			AperTure->x = InsideDiameter;
			AperTure->y = OutsideDiameter;
			AperTure->x2 = CrossHairThickness;
			AperTure->AperTureNr = cnt;
			AperTure->AperTureCode = DcodeThermalRelief;
			DcodeThermalRelief++;
			AperTure->Used = 1;
			NrAperTures++;
		}

		LastThermalReliefAperTure.x = InsideDiameter;
		LastThermalReliefAperTure.y = OutsideDiameter;
		LastThermalReliefAperTure.x2 = CrossHairThickness;
		LastThermalReliefAperTure.AperTureNr = cnt;
		return cnt;
	}

	if (NrAperTures + 1 >= MaxNrAperTures)
	{
		if (AllocateMemAperTures(NrAperTures + 64) != 0)
			return -1;
	}

	AperTure = &((*AperTures)[0]);
	AperTure->Info = GERBER_PAD_THERMAL_RELIEF;
	AperTure->x = InsideDiameter;
	AperTure->y = OutsideDiameter;
	AperTure->x2 = CrossHairThickness;
	AperTure->AperTureNr = 0;
	AperTure->Used = 1;
	AperTure->AperTureCode = DcodeThermalRelief;
	DcodeThermalRelief++;
	AperTure->Info3 = 0;
	LastThermalReliefAperTure.AperTureNr = 0;
	LastThermalReliefAperTure.x = InsideDiameter;
	LastThermalReliefAperTure.y = OutsideDiameter;
	LastThermalReliefAperTure.x2 = CrossHairThickness;
	LastTraceAperTure.x = 0.0;
	LastButterflyPadAperTure.x = 0.0;
	LastRoundPadAperTure.x = 0.0;
	LastRectPadAperTure.y = 0.0;
	LastRectPadAperTure.y = 0.0;
	LastPolygonPadAperTure.Address = NULL;
	NrAperTures++;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckRectPadAperTure(double x, double y)
{
	int32 cnt, ok;
	AperTureRecord *AperTure;

	if (x < 100.0)
		ok = 1;

	if (y < 100.0)
		ok = 1;

	if (NrAperTures > 0)
	{
		if ((InRange8(LastRectPadAperTure.x, x)) && (InRange8(LastRectPadAperTure.y, y)))
		{
			AperTure = &((*AperTures)[LastRectPadAperTure.AperTureNr]);
			AperTure->Used++;
			return LastRectPadAperTure.AperTureNr;
		}

		cnt = 0;

		while ((cnt < NrAperTures)
		        && (((*AperTures)[cnt].Info != GERBER_PAD_RECT) || (NotInRange8((*AperTures)[cnt].x, x))
		            || (NotInRange8((*AperTures)[cnt].y, y))))
			cnt++;

		if (cnt < NrAperTures)
		{
			AperTure = &((*AperTures)[cnt]);
			AperTure->Used++;
		}
		else
		{
			if (NrAperTures + 1 >= MaxNrAperTures)
			{
				if (AllocateMemAperTures(NrAperTures + 64) != 0)
					return -1;
			}

			AperTure = &((*AperTures)[cnt]);
#ifdef _DEBUG

			if (cnt == 1)
				ok = 1;

			if (DcodeRectPad == 252)
				ok = 1;

#endif
			AperTure->Info = GERBER_PAD_RECT;
			AperTure->x = x;
			AperTure->y = y;
			AperTure->AperTureNr = cnt;
			AperTure->Used = 1;
			AperTure->Info3 = 0;
			AperTure->AperTureCode = DcodeRectPad;
			DcodeRectPad++;
			NrAperTures++;
		}

		LastRectPadAperTure.x = x;
		LastRectPadAperTure.y = y;
		LastRectPadAperTure.AperTureNr = cnt;
		return cnt;
	}

	if (NrAperTures + 1 >= MaxNrAperTures)
	{
		if (AllocateMemAperTures(NrAperTures + 64) != 0)
			return -1;
	}

	AperTure = &((*AperTures)[0]);
	AperTure->Info = GERBER_PAD_RECT;
	AperTure->x = x;
	AperTure->y = y;
	AperTure->AperTureNr = 0;
	AperTure->Used = 1;
	AperTure->Info3 = 0;
	AperTure->AperTureCode = DcodeRectPad;
	DcodeRectPad++;
	LastRectPadAperTure.AperTureNr = 0;
	LastRectPadAperTure.x = x;
	LastRectPadAperTure.y = y;
	LastThermalReliefAperTure.x = 0.0;
	LastRoundPadAperTure.x = 0.0;
	LastButterflyPadAperTure.x = 0.0;
	LastTraceAperTure.x = 0.0;
	LastPolygonPadAperTure.Address = NULL;
	NrAperTures++;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygonAperTure(int32 ObjectType, double x2, double y2, double Rotation, int32 Mirror, uint8 * Address)
{
	int32 cnt, Found;
	AperTureRecord *AperTure;

	/*

	%AMMPOLYGON1*
	4,1,NrVertices+1,
	X1,Y1,
	..
	..
	X(NrVertices-1),Y(NrVertices-1),
	X1,Y1,
	$1*%


	%ADD18MPOLYGON1,Rotation*%

	%ADD18MPOLYGON1,45.0*%

	*/
	if (NrAperTures > 0)
	{
		if (ObjectType == 0)
		{
			if ((InRange2(LastPolygonPadAperTure.Rotation, Rotation)) && (LastPolygonPadAperTure.Mirror == Mirror)
			        && (LastPolygonPadAperTure.Address == Address))
			{
				AperTure = &((*AperTures)[LastRoundPadAperTure.AperTureNr]);
				AperTure->Used++;
				return LastPolygonPadAperTure.AperTureNr;
			}
		}
		else
		{
			if ((InRange2(LastPolygonPadAperTure.Rotation, Rotation)) && (LastPolygonPadAperTure.Mirror == Mirror)
			        && (InRange(LastPolygonPadAperTure.x2, x2)) && (InRange(LastPolygonPadAperTure.y2, y2))
			        && (LastPolygonPadAperTure.SpecialType == ObjectType))
			{
				AperTure = &((*AperTures)[LastRoundPadAperTure.AperTureNr]);
				AperTure->Used++;
				return LastPolygonPadAperTure.AperTureNr;
			}
		}

		Found = -1;

		for (cnt = 0; cnt < NrAperTures; cnt++)
		{
			if ((*AperTures)[cnt].Info == GERBER_PAD_POLYGON)
			{
				if (ObjectType != 0)
				{
					if (((*AperTures)[cnt].SpecialType == ObjectType) && (InRange((*AperTures)[cnt].x2, x2))
					        && (InRange((*AperTures)[cnt].y2, y2)) && (InRange2((*AperTures)[cnt].Rotation, Rotation))
					        && ((*AperTures)[cnt].Mirror == Mirror))
					{
						if (Found == -1)
							Found = cnt;
					}
				}
				else
				{
					if ((InRange2((*AperTures)[cnt].Rotation, Rotation)) && ((*AperTures)[cnt].Mirror == Mirror)
					        && ((*AperTures)[cnt].Address == Address))
					{
						if (Found == -1)
							Found = cnt;
					}
				}
			}
		}

		cnt = Found;

		if (Found == -1)
			cnt = NrAperTures;

		if (cnt < NrAperTures)
		{
			AperTure = &((*AperTures)[cnt]);
			AperTure->Used++;
		}
		else
		{
			if (NrAperTures + 1 >= MaxNrAperTures)
			{
				if (AllocateMemAperTures(NrAperTures + 64) != 0)
					return -1;
			}

			AperTure = &((*AperTures)[cnt]);
			AperTure->Info = GERBER_PAD_POLYGON;
			AperTure->Info3 = 0;
			AperTure->x2 = x2;
			AperTure->y2 = y2;
			AperTure->SpecialType = ObjectType;
			AperTure->Rotation = Rotation;
			AperTure->Mirror = Mirror;
			AperTure->Address = Address;
			AperTure->AperTureNr = cnt;
			AperTure->AperTureCode = DcodePolygonPad;
			DcodePolygonPad++;
			AperTure->Used = 1;
			NrAperTures++;
		}

		LastPolygonPadAperTure.x2 = x2;
		LastPolygonPadAperTure.y2 = y2;
		LastPolygonPadAperTure.SpecialType = ObjectType;
		LastPolygonPadAperTure.Rotation = Rotation;
		LastPolygonPadAperTure.Mirror = Mirror;
		LastPolygonPadAperTure.Address = Address;
		LastPolygonPadAperTure.AperTureNr = cnt;
		return cnt;
	}

	if (NrAperTures + 1 >= MaxNrAperTures)
	{
		if (AllocateMemAperTures(NrAperTures + 64) != 0)
			return -1;
	}

	AperTure = &((*AperTures)[0]);
	AperTure->Info = GERBER_PAD_POLYGON;
	AperTure->x2 = x2;
	AperTure->y2 = y2;
	AperTure->SpecialType = ObjectType;
	AperTure->Rotation = Rotation;
	AperTure->Mirror = Mirror;
	AperTure->Address = Address;
	AperTure->AperTureNr = 0;
	AperTure->Used = 1;
	AperTure->AperTureCode = DcodePolygonPad;
	DcodePolygonPad++;
	AperTure->Info3 = 0;
	LastPolygonPadAperTure.AperTureNr = 0;
	LastPolygonPadAperTure.x2 = x2;
	LastPolygonPadAperTure.y2 = y2;
	LastPolygonPadAperTure.SpecialType = ObjectType;
	LastPolygonPadAperTure.Rotation = Rotation;
	LastPolygonPadAperTure.Mirror = Mirror;
	LastPolygonPadAperTure.Address = Address;
	LastThermalReliefAperTure.x = 0.0;
	LastTraceAperTure.x = 0.0;
	LastButterflyPadAperTure.x = 0.0;
	LastRoundPadAperTure.x = 0.0;
	LastRectPadAperTure.x = 0.0;
	LastRectPadAperTure.y = 0.0;
	NrAperTures++;
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPlatedDrillAperTure(double ThickNess)
{
	int32 cnt, ok;
	AperTureRecord *DrillAperTure;

	if (ThickNess < 100.0)
		ok = 1;

	if (NrDrillAperTures > 0)
	{
		if (InRange8(LastPlatedDrillAperTure.x, ThickNess))
		{
			DrillAperTure = &((*DrillAperTures)[LastPlatedDrillAperTure.AperTureNr]);
			DrillAperTure->Used++;
			return LastPlatedDrillAperTure.AperTureNr;
		}

		cnt = 0;

		while ((cnt < NrDrillAperTures)
		        && (((*DrillAperTures)[cnt].Info != GERBER_DRILL_PLATED)
		            || (NotInRange8((*DrillAperTures)[cnt].x, ThickNess))))
			cnt++;

		if (cnt < NrDrillAperTures)
		{
			DrillAperTure = &((*DrillAperTures)[cnt]);
			DrillAperTure->Used++;
		}
		else
		{
			if (NrDrillAperTures + 1 >= MaxNrAperTures)
			{
				if (AllocateMemAperTures(NrDrillAperTures + 64) != 0)
					return -1;
			}

			DrillAperTure = &((*DrillAperTures)[cnt]);
			DrillAperTure->Info = GERBER_DRILL_PLATED;
			DrillAperTure->Info3 = 0;
			DrillAperTure->x = ThickNess;
			DrillAperTure->AperTureNr = cnt;
			DrillAperTure->AperTureCode = DcodeDrill;
			DcodeDrill++;
			DrillAperTure->Used = 1;
			NrDrillAperTures++;
		}

		LastPlatedDrillAperTure.x = ThickNess;
		LastPlatedDrillAperTure.AperTureNr = cnt;
		return cnt;
	}

	if (NrDrillAperTures + 1 >= MaxNrAperTures)
	{
		if (AllocateMemAperTures(NrDrillAperTures + 64) != 0)
			return -1;
	}

	DrillAperTure = &((*DrillAperTures)[0]);
	DrillAperTure->Info = GERBER_DRILL_PLATED;
	DrillAperTure->x = ThickNess;
	DrillAperTure->AperTureNr = 0;
	DrillAperTure->Used = 1;
	DrillAperTure->AperTureCode = DcodeDrill;
	DcodeDrill++;
	DrillAperTure->Info3 = 0;
	LastPlatedDrillAperTure.AperTureNr = 0;
	LastPlatedDrillAperTure.x = ThickNess;
	LastUnPlatedDrillAperTure.x = 0.0;
	NrDrillAperTures++;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckUnPlatedDrillAperTure(double ThickNess)
{
	int32 cnt, ok;
	AperTureRecord *DrillAperTure;

	if (ThickNess < 100.0)
		ok = 1;

	if (NrDrillAperTures > 0)
	{
		if (InRange8(LastUnPlatedDrillAperTure.x, ThickNess))
		{
			DrillAperTure = &((*DrillAperTures)[LastUnPlatedDrillAperTure.AperTureNr]);
			DrillAperTure->Used++;
			return LastUnPlatedDrillAperTure.AperTureNr;
		}

		cnt = 0;

		while ((cnt < NrDrillAperTures)
		        && (((*DrillAperTures)[cnt].Info != GERBER_DRILL_UNPLATED)
		            || (NotInRange8((*DrillAperTures)[cnt].x, ThickNess))))
			cnt++;

		if (cnt < NrDrillAperTures)
		{
			DrillAperTure = &((*DrillAperTures)[cnt]);
			DrillAperTure->Used++;
		}
		else
		{
			if (NrDrillAperTures + 1 >= MaxNrAperTures)
			{
				if (AllocateMemAperTures(NrDrillAperTures + 64) != 0)
					return -1;
			}

			DrillAperTure = &((*DrillAperTures)[cnt]);
			DrillAperTure->Info = GERBER_DRILL_UNPLATED;
			DrillAperTure->Info3 = 0;
			DrillAperTure->x = ThickNess;
			DrillAperTure->AperTureNr = cnt;
			DrillAperTure->AperTureCode = DcodeDrill;
			DcodeDrill++;
			DrillAperTure->Used = 1;
			NrDrillAperTures++;
		}

		LastUnPlatedDrillAperTure.x = ThickNess;
		LastUnPlatedDrillAperTure.AperTureNr = cnt;
		return cnt;
	}

	if (NrDrillAperTures + 1 >= MaxNrAperTures)
	{
		if (AllocateMemAperTures(NrDrillAperTures + 64) != 0)
			return -1;
	}

	DrillAperTure = &((*DrillAperTures)[0]);
	DrillAperTure->Info = GERBER_DRILL_UNPLATED;
	DrillAperTure->x = ThickNess;
	DrillAperTure->AperTureNr = 0;
	DrillAperTure->Used = 1;
	DrillAperTure->AperTureCode = DcodeDrill;
	DcodeDrill++;
	DrillAperTure->Info3 = 0;
	LastUnPlatedDrillAperTure.AperTureNr = 0;
	LastUnPlatedDrillAperTure.x = ThickNess;
	LastPlatedDrillAperTure.x = 0.0;
	NrDrillAperTures++;
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetTraceChain(int32 mode)
{
	int32 ok, cnt, cnt2, NrTracePointsNumbers, ObjectNr3, MinNrTraces, TraceCnt;

	ObjectRecord *Object;
	double NewX, NewY, TraceLength, x1, y1, x2, y2;
	TracePointRecord *TracePoint;

	int32 Found, Stop;

	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &TracePoints);
	NrTracePoints = 0;
	NrTracePointsNumbers = 0;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		if ((Object->Info & 3) == 0)
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;

			switch (Object->ObjectType)
			{
			case TRACE_HOR:
				if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
				{
					InsertObjectPoint(x1, y1);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;

				if ((cnt2 = GetTracePointNr(x1 + x2, y1)) == -1)
				{
					InsertObjectPoint(x1 + x2, y1);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;
				break;

			case TRACE_VER:
				if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
				{
					InsertObjectPoint(x1, y1);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;

				if ((cnt2 = GetTracePointNr(x1, y1 + x2)) == -1)
				{
					InsertObjectPoint(x1, y1 + x2);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;
				break;

			case TRACE_DIAG1:
				if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
				{
					InsertObjectPoint(x1, y1);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;

				if ((cnt2 = GetTracePointNr(x1 + x2, y1 - x2)) == -1)
				{
					InsertObjectPoint(x1 + x2, y1 - x2);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;
				break;

			case TRACE_DIAG2:
				if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
				{
					InsertObjectPoint(x1, y1);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;

				if ((cnt2 = GetTracePointNr(x1 + x2, y1 + x2)) == -1)
				{
					InsertObjectPoint(x1 + x2, y1 + x2);
					TracePoint = &((*TracePoints)[NrTracePoints - 1]);
				}
				else
					TracePoint = &((*TracePoints)[cnt2]);

				TracePoint->NrTraces++;
				break;
			}
		}
	}

// Find first point which is connected to only one trace
	Found = 0;
	TraceLength = 0.0;
	TraceCnt = -1;
	MinNrTraces = 10000;

	for (cnt = 0; cnt < NrTracePoints; cnt++)
	{
		TracePoint = &((*TracePoints)[cnt]);

		if (TracePoint->NrTraces < MinNrTraces)
		{
			MinNrTraces = TracePoint->NrTraces;
			TraceCnt = cnt;
		}
	}

	if (TraceCnt == -1)
		return 0;

	cnt = TraceCnt;
	TracePointsNumbers[NrTracePointsNumbers++] = cnt;

	Stop = 0;

	while (!Stop)
	{
		TracePoint = &((*TracePoints)[cnt]);
		ObjectNr3 = GetTraceObjectNrFromEndPoint(TracePoint->x, TracePoint->y, &NewX, &NewY, 0);

		if (ObjectNr3 != -1)
		{
			Object = &((*Objects3)[ObjectNr3]);
			Object->Info |= 1;
			cnt = GetTracePointNr(NewX, NewY);

			if (cnt != -1)
				TracePointsNumbers[NrTracePointsNumbers++] = cnt;
			else
				Stop = 1;
		}
		else
			Stop = 1;
	}

	ok = 1;
	return NrTracePointsNumbers;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 TracePenPlotOutput(int32 Layer, int32 NrTraceObjects)
{

	int32 ok, cnt, cnt2, cnt3, Count, Start, End, NrObjects6, NrTracePointsNumbers, ObjectNr3, NrCopperObjects;
	double x1, y1, x2a, y2a, x2, y2, x3, y3, TraceDivX, TraceDivY, d, d2;
	ObjectRecord *Object3, *Object3a, *Object4, *Object6;
	ObjectArray *Objects6;
	NetRecord *Net;
	int32 AddLine, LineChanged;

	sprintf(InfoStr, SC(947, "Penplot output traces layer %i "), Layer);
	RedrawInfoStr(1);

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Count = 0;
		Net->Pos = 0;
	}

	NrCopperObjects = NrTraceObjects;

	if (NrObjects4 + NrCopperObjects + 1 > MaxNrObjects4)
	{
		if (AllocateMemObjects4(NrObjects4 + NrCopperObjects + 1) == -1)
			return -1;
	}

	Objects6 = (ObjectArray *) & ((*Objects4)[NrObjects4]);
	NrObjects6 = 0;

	for (cnt = 0; cnt < NrCopperObjects; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);

		if (Object4->ObjectType == 0)
			ok = 1;

		if ((Object4->NetNr >= 0) && (Object4->NetNr < Design.NrNets) && (Object4->ObjectType != 0))
		{
			Net = &((*Nets)[Object4->NetNr]);
			Net->Count++;
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	for (cnt = 0; cnt < NrCopperObjects; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);

		if ((Object4->NetNr >= 0) && (Object4->NetNr < Design.NrNets) && (Object4->ObjectType != 0))
		{
			Net = &((*Nets)[Object4->NetNr]);
			Object6 = &((*Objects6)[Net->Pos]);
			memmove(Object6, Object4, sizeof(ObjectRecord));
			Object6->Info = 0;
			Net->Pos++;
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	NrObjects6 = Count;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if ((cnt % 100) == 0)
			CheckInputMessages(0);

		Start = Net->Pos;
		End = Net->Pos + Net->Count;
		NrObjects3 = 0;

		for (cnt2 = Start; cnt2 < End; cnt2++)
		{
			Object6 = &((*Objects6)[cnt2]);

			if (NrObjects3 >= MaxNrObjects3 - 1)
			{
				if (AllocateMemObjects3(MaxNrObjects3 + 128) == -1)
					return -1;
			}

			Object3 = &((*Objects3)[NrObjects3++]);
			memmove(Object3, Object6, sizeof(ObjectRecord));
			Object3->TraceNr = cnt2;
			Object3->Layer = 0;
			Object3->Info = 0;

			switch (Object3->ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
				break;

			default:
				Object6->Info = 2;
			}
		}

		while ((NrTracePointsNumbers = GetTraceChain(0)) > 0)
		{
			x1 = (*TracePoints)[TracePointsNumbers[0]].x;
			y1 = (*TracePoints)[TracePointsNumbers[0]].y;

			for (cnt2 = 1; cnt2 < NrTracePointsNumbers; cnt2++)
			{
				if (((x1 > 87.1e5) && (x1 < 87.2e5)) && ((y1 > 50.2e5) && (y1 < 50.3e5)))
					ok = 1;

				x2 = (*TracePoints)[TracePointsNumbers[cnt2]].x;
				y2 = (*TracePoints)[TracePointsNumbers[cnt2]].y;
				x2a = x2;
				y2a = y2;
				ObjectNr3 = GetTraceObjectNrFromEndPoint(x1, y1, &x3, &y3, 1);

				if (((x2 > 87.1e5) && (x2 < 87.2e5)) && ((y2 > 50.2e5) && (y2 < 50.3e5)))
					ok = 1;

				if (ObjectNr3 == -1)
					ObjectNr3 = GetTraceObjectNrFromEndPoint(x2, y2, &x3, &y3, 1);

				if (ObjectNr3 >= 0)
				{
					Object3 = &((*Objects3)[ObjectNr3]);
					Object3->Info |= 2;
					Object6 = &((*Objects6)[Object3->TraceNr]);
					Object6->Info = 2;
					AddLine = 1;
					TraceDivX = x2 - x1;
					TraceDivY = y2 - y1;

					for (cnt3 = 0; cnt3 < NrObjects3; cnt3++)
					{
						Object3a = &((*Objects3)[cnt3]);

						switch (Object3a->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:

//              case PIN_SMD_ROUND:
						case VIA_PUT_THROUGH_ROUND:
						case PIN_PUT_THROUGH_SQUARE:
							LineChanged = 0;
							d = (min(Object3a->y2, 50000) + max(CurrentPlotPenSize, Object3->y2)) * 0.5;

							if ((InRange4(Object3a->x1, x1)) && (InRange4(Object3a->y1, y1)))
							{
								if ((NotInRange(TraceDivX, 0.0)) && (NotInRange(TraceDivY, 0.0)))
								{
									d2 = d * sqrt(0.5);

									if ((fabs(TraceDivX) > d2) && (fabs(TraceDivY) > d2))
									{
										if (TraceDivX > 0)
											x1 += d2;
										else
											x1 -= d2;

										if (TraceDivY > 0)
											y1 += d2;
										else
											y1 -= d2;
									}
									else
										AddLine = 0;

									LineChanged = 1;
								}
								else
								{
									if (NotInRange(TraceDivX, 0.0))
									{
										if (fabs(TraceDivX) > d)
										{
											if (TraceDivX > 0)
												x1 += d;
											else
												x1 -= d;
										}
										else
											AddLine = 0;

										LineChanged = 1;
									}

									if (NotInRange(TraceDivY, 0.0))
									{
										if (fabs(TraceDivY) > d)
										{
											if (TraceDivY > 0)
												y1 += d;
											else
												y1 -= d;
										}
										else
											AddLine = 0;

										LineChanged = 1;
									}
								}
							}

							if ((InRange4(Object3a->x1, x2)) && (InRange4(Object3a->y1, y2)))
							{
								if ((NotInRange(TraceDivX, 0.0)) && (NotInRange(TraceDivY, 0.0)))
								{
									d2 = d * sqrt(0.5);

									if ((fabs(TraceDivX) > d2) && (fabs(TraceDivY) > d2))
									{
										if (TraceDivX > 0)
											x2 -= d2;
										else
											x2 += d2;

										if (TraceDivY > 0)
											y2 -= d2;
										else
											y2 += d2;
									}
									else
										AddLine = 0;

									LineChanged = 1;
								}
								else
								{
									if (NotInRange(TraceDivX, 0.0))
									{
										if (fabs(TraceDivX) > d)
										{
											if (TraceDivX > 0)
												x2 -= d;
											else
												x2 += d;
										}
										else
											AddLine = 0;

										LineChanged = 1;
									}

									if (NotInRange(TraceDivY, 0.0))
									{
										if (fabs(TraceDivY) > d)
										{
											if (TraceDivY > 0)
												y2 -= d;
											else
												y2 += d;
										}
										else
											AddLine = 0;

										LineChanged = 1;
									}
								}
							}

							if (!LineChanged)
							{
							}

							break;
						}
					}

					if (AddLine)
					{
						if (DrawGerberLine(x1, y1, x2, y2, Object3->y2, CurrentPlotPenSize) == -1)
							PlotErrors++;
					}
				}

				x1 = x2a;
				y1 = y2a;
			}
		}

	}

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object6 = &((*Objects6)[cnt]);

		if ((Object6->Info & 2) == 0)
			ok = 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectsGerberOutput(int32 Layer, int32 ObjectStartNr)
{

	int32 cnt, cnt2, cnt3, cntx, cnty, cnty2, Divisions, TempObjects4Pos, count, Start, End, Length, NrLines, FontNr,
	      MaxCountX, code, lengte, cnt4, cnt5, cnt6, Mirror, DrawChar, SegmentCount, LineSegments, NrTempObjects4;
	double sx, sy, x1, y1, x2, y2, x3, y3, x4, y4, BoardDivX, BoardDivY, RotationAngle, incX, incY;
	ObjectRecord *Object4, *Object5;
	PolygonRecord *DrawPolygon;
	AperTureRecord *AperTure;
	AreaFillRecord *AreaFill;
	WCHAR *str2;
	uint8 *PolygonPos;
	char str[1024];
	double LineBuf[128];
	LPSTR TextP;
	ObjectArray *Objects5;

	sx = DesignBoardOriginX;
	sy = DesignBoardOriginY;
	Divisions = 6;
	BoardDivX = DesignBoardWidth / Divisions;
	BoardDivY = DesignBoardHeight / Divisions;

	for (cnt = 0; cnt < NrAperTures; cnt++)
	{
		AperTure = &((*AperTures)[cnt]);
		AperTure->Used2 = 0;
	}

	Start = ObjectStartNr;
	End = NrObjects4;

	if (NrObjects4 + End - Start + 1 > MaxNrObjects4)
	{
		if (AllocateMemObjects4(NrObjects4 + End - Start + 1) == -1)
			return -1;
	}

	Objects5 = (ObjectArray *) & ((*Objects4)[NrObjects4]);
	memset(Objects5, 0, sizeof(ObjectRecord) * (End - Start + 1));

	for (cnt = 0; cnt < 512; cnt++)
		ObjectsApertureCount[cnt] = 0;

	for (cnt = Start; cnt < End; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);

		if ((Object4->Info2 != -1) && (Object4->Info2 < NrAperTures))
			ObjectsApertureCount[Object4->Info2]++;
	}

	ObjectsApertureIndex[0] = 0;

	for (cnt = 0; cnt < 510; cnt++)
		ObjectsApertureIndex[cnt + 1] = ObjectsApertureIndex[cnt] + ObjectsApertureCount[cnt];

	for (cnt = Start; cnt < End; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);

		if ((Object4->Info2 != -1) && (Object4->Info2 < NrAperTures))
		{
			switch (Object4->ObjectType)
			{
			case OBJECT_POLYGON:
				Object5 = &((*Objects5)[ObjectsApertureIndex[Object4->Info2]]);

				if ((Object4->Info & OBJECT_FILLED) == 0)
				{
					ObjectsApertureIndex[Object4->Info2]++;
					memcpy(Object5, Object4, sizeof(ObjectRecord));
				}
				else
				{
					if (GerberPlotObject(Object4, 0) == -1)
						PlotErrors++;

					memcpy(Object5, Object4, sizeof(ObjectRecord));
					Object4->Info |= 1;
				}

				break;

			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
				Object5 = &((*Objects5)[ObjectsApertureIndex[Object4->Info2]]);
				ObjectsApertureIndex[Object4->Info2]++;

				if (GerberPlotObject(Object4, 0) == -1)
					PlotErrors++;

				Object4->Info |= 1;
				memcpy(Object5, Object4, sizeof(ObjectRecord));
				break;

			default:
				Object5 = &((*Objects5)[ObjectsApertureIndex[Object4->Info2]]);
				ObjectsApertureIndex[Object4->Info2]++;
				memcpy(Object5, Object4, sizeof(ObjectRecord));
#ifdef _DEBUG

				switch (Object5->ObjectType)
				{
				case OBJECT_TEXT:
				case OBJECT_TEXT2:
					if (stricmpOwn((LPSTR) Object5->TraceNr, "info1_0") == 0)
						ok = 1;

					break;
				}

#endif
				break;
			}
		}
		else
			ok = 1;
	}

	ObjectsApertureIndex[0] = 0;

	for (cnt = 0; cnt < 511; cnt++)
		ObjectsApertureIndex[cnt + 1] = ObjectsApertureIndex[cnt] + ObjectsApertureCount[cnt];

#ifdef _DEBUG

	for (cnt = NrObjects4; cnt < End - Start + 1; cnt++)
	{
		Object5 = &((*Objects5)[cnt]);

		if (Object5->ObjectType == 0)
			ok = 1;
	}

#endif
	TempObjects4Pos = NrObjects4;

	for (cnt2 = 0; cnt2 < NrAperTures; cnt2++)
	{
		if (ObjectsApertureCount[cnt2] > 0)
		{
			if ((cnt2 % 100) == 0)
				CheckInputMessages(0);

			AperTure = &((*AperTures)[cnt2]);
			CurrentAperTureNr = cnt2;
#ifdef _DEBUG

			if (AperTure->AperTureCode == 109)
				ok = 1;

#endif

			if (!PenPlotMode)
			{
				sprintf(str, "G54D%i*", AperTure->AperTureCode);
				WriteGerberString(str, 1);
			}

			count = 0;

			for (cntx = 0; cntx < Divisions; cntx++)
			{
				for (cnty = 0; cnty < Divisions; cnty++)
				{
					cnty2 = cnty;

					if ((cntx & 1) == 1)
						cnty2 = Divisions - cnty - 1;

					SearchMinX = sx + (cntx) * BoardDivX - (1 * 2540);
					SearchMinY = sy + (cnty2) * BoardDivY - (1 * 2540);
					SearchMaxX = sx + (cntx + 1) * BoardDivX + (1 * 2540);
					SearchMaxY = sy + (cnty2 + 1) * BoardDivY + (1 * 2540);

					NrTempObjects4 = 0;
					Start = ObjectsApertureIndex[cnt2];
					End = ObjectsApertureIndex[cnt2] + ObjectsApertureCount[cnt2];

					for (cnt = Start; cnt < End; cnt++)
					{
						Object5 = &((*Objects5)[cnt]);
#ifdef _DEBUG

						if ((InRange9(Object5->x1, -88.7e5)) && (InRange9(Object5->y1, 85.7e5)))
							ok = 1;

						if ((Object5->minx <= SearchMaxX) && (Object5->maxx >= SearchMinX)
						        && (Object5->miny <= SearchMaxY) && (Object5->maxy >= SearchMinY))
						{
						}
						else
							ok = 1;

#endif

						if (((Object5->Info & 1) == 0) && (Object5->minx <= SearchMaxX) && (Object5->maxx >= SearchMinX)
						        && (Object5->miny <= SearchMaxY) && (Object5->maxy >= SearchMinY))
						{
							Object5->Info |= 1;
							count++;
							x1 = Object5->x1;
							y1 = Object5->y1;
							x2 = Object5->x2;
							y2 = Object5->y2;

							switch (Object5->ObjectType)
							{
							case PIN_LINE_HOR:
								x2 = x1 + Object5->x2;
								y2 = y1;
								break;

							case PIN_LINE_VER:
								x2 = x1;
								y2 = y1 + Object5->x2;
								break;

							case PIN_LINE_DIAG1:
								x2 = x1 + Object5->x2;
								y2 = y1 - Object5->x2;
								break;

							case PIN_LINE_DIAG2:
								x2 = x1 + Object5->x2;
								y2 = y1 + Object5->x2;
								break;
							}

#ifdef _DEBUG

							if ((InRange9(x1, 180.6e5)) && (InRange9(y1, 78.2e5)))
								ok = 1;

#endif

							if (PenPlotMode)
							{
								if (((Object5->Info & OBJECT_FILLED) == 0)
								        && (Object5->Thickness < CurrentPlotPenSize * 0.7))
								{
									if (Object5->Thickness < CurrentPlotPenSize * 1.3)
										Object5->Thickness = CurrentPlotPenSize;
								}
								else
								{
// Error CurrentPlotPenSize to thick
									switch (Object5->Layer)
									{
									case SILKSCREEN_BOTTOM:
									case SILKSCREEN_TOP:
										if ((Object5->Info & OBJECT_FILLED) == 0)
										{
//                        PlotErrors++;
										}
										else
											Object5->Thickness = CurrentPlotPenSize;

										break;

									case BOARD_OUTLINE_LAYER:
									case COMP_OUTLINE_LAYER:
									case COMP_OUTLINE_LAYER + 1:
									case INFO_LAYER:
									case INFO_LAYER2:
									case INFO_LAYER3:
									case INFO_LAYER4:
										Object5->Thickness = CurrentPlotPenSize;
										break;

									default:
										if (Object5->Layer < 32)
										{
//                        Object5->ObjectType=0;
											if ((Object5->Info & OBJECT_FILLED) == 0)
											{
//                          PlotErrors++;
											}
										}

										break;
									}
								}
							}

							switch (Object5->ObjectType)
							{
							case OBJECT_LINE:
								if (Object5->Test != 0)
								{
									LineSegments =
									    DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, Object5->Test);
									SegmentCount = 0;

									for (cnt3 = 0; cnt3 < LineSegments; cnt3++)
									{
										x3 = LineBuf[SegmentCount++];
										y3 = LineBuf[SegmentCount++];
										x4 = LineBuf[SegmentCount++];
										y4 = LineBuf[SegmentCount++];

										if (!PenPlotMode)
											GerberWriteLine(x3, y3, x4, y4);
										else
											DrawGerberLine(x3, y3, x4, y4, Object5->Thickness, CurrentPlotPenSize);
									}
								}
								else
								{
									if (!PenPlotMode)
										GerberWriteLine(x1, y1, x2, y2);
									else
										DrawGerberLine(x1, y1, x2, y2, Object5->Thickness, CurrentPlotPenSize);
								}

								break;

							case TRACE_ALL_ANGLE:
							case PIN_LINE_ALL_ANGLE:
							case PIN_LINE_HOR:
							case PIN_LINE_VER:
							case PIN_LINE_DIAG1:
							case PIN_LINE_DIAG2:
								if (!PenPlotMode)
								{
									if (DIFF_GERBER(x1, y1) < DIFF_GERBER(x2, y2))
										GerberWriteLine(x1, y1, x2, y2);
									else
										GerberWriteLine(x2, y2, x1, y1);
								}
								else
									DrawGerberLine(x1, y1, x2, y2, Object5->Thickness, CurrentPlotPenSize);

								break;

							case PIN_SMD_ROUND:
							case PIN_SMD_RECT:
							case OBJECT_RECT:
							case VIA_PUT_THROUGH_ROUND:
							case PIN_PUT_THROUGH_ROUND:
							case PIN_PUT_THROUGH_SQUARE:
							case DRILL:
							case DRILL_UNPLATED:
								if (GerberPlotObject(Object5, 0) == -1)
									PlotErrors++;

								break;

							case OBJECT_CIRCLE:
								if ((Object5->Info & OBJECT_FILLED) == 0)
									DrawGerberCircle(Object5);
								else
								{
									if (GerberPlotObject(Object5, 0) == -1)
										PlotErrors++;
								}

								break;

							case OBJECT_ARC:
							case PIN_ARC:
							case TRACE_ARC:
								if ((Object5->Info & OBJECT_FILLED) == 0)
									DrawGerberArc(Object5);
								else
								{
									if (GerberPlotObject(Object5, 0) == -1)
										PlotErrors++;
								}

								break;

							case OBJECT_TEXT:
							case OBJECT_TEXT2:
								TextP = (LPSTR) Object5->TraceNr;
#ifdef _DEBUG

								if (stricmpOwn(TextP, "u100") == 0)
									ok = 1;

#endif
								RotationAngle = Object5->RotationAngle;
								Mirror = Object5->Mirror;
								memset(&str, 0, sizeof(str));
								strncpy(str, TextP, 512);
								Length = strlen(str);
								x1 = Object5->x1;
								y1 = Object5->y1;
								x2 = Object5->x2;
								x4 = 0.0;
								y4 = 0.0;
								FontNr = Object5->Test >> 16;
								NrLines = ConvertObjectTextToStrings(TextP, FontNr, &MaxCountX, Object5->Layer);

								for (cnt3 = 0; cnt3 < NrLines; cnt3++)
								{
									if (FontNr == 0)
									{
										DrawGerberStr2(x1, y1, x2, Object5->Thickness, RotationAngle, 0, Mirror,
										               TextStrings2[cnt3]);
									}
									else
									{
										incX = 0.0;
										incY = 0.0;
										x4 = x1;
										y4 = y1;
										str2 = (WCHAR *) & TextStrings[cnt3];
										lengte = (int32) wcslen(str2);
										AllocateSpecialMem(MEM_TRUETYPE_AREAFILL, 128 * 1024, (void **) &AreaFill);

										for (cnt4 = 0; cnt4 < lengte; cnt4++)
										{
											code = (*str2);
											//    code='+';
											count = 0;
											DrawChar = 0;

											//    code=127;
											if ((code != ' ') && (code != '\t'))
											{
												if (GetAreaFillFontChar(code, FontNr, AreaFill) == 0)
													break;

												DrawChar = 1;
											}
											else
											{
												incX = TRUETYPE_FONT_SPACE_EXTRA_X * x2;
												incY = 0.0;
												RotatePoint2(&incX, &incY, RotationAngle);
											}

											if (DrawChar)
											{
												incX =
												    (AreaFill->maxx - AreaFill->minx + TRUETYPE_FONT_ADD_EXTRA_X) * x2;
												incY = 0.0;
												RotatePoint2(&incX, &incY, RotationAngle);
												DrawPolygon =
												    (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
												PolygonPos = (uint8 *) DrawPolygon;

												for (cnt5 = 0; cnt5 < AreaFill->NrPolygons; cnt5++)
												{
													count = DrawPolygon->NrVertices;
#ifdef _DEBUG

													if ((DrawPolygon->PolygonType & 8) == 8)
														ok = 1;

													if (cnt2 == 6)
														ok = 1;

#endif

													for (cnt6 = 0; cnt6 < count; cnt6++)
													{
														DrawPolygon->Points[cnt6].x *= x2;
														DrawPolygon->Points[cnt6].y *= x2;
														RotatePoint2(&DrawPolygon->Points[cnt6].x,
														             &DrawPolygon->Points[cnt6].y, RotationAngle);

														if (Mirror == 1)
															DrawPolygon->Points[cnt6].x = -DrawPolygon->Points[cnt6].x;

														DrawPolygon->Points[cnt6].x += x4;
														DrawPolygon->Points[cnt6].y += y4;
													}

													if ((!PenPlotMode) && ((GerberInfo.PlotMode & 1) == 0))
													{
														if (cnt6 < count - 1)
															GerberPlotPolygon(DrawPolygon, 1);
														else
															GerberPlotPolygon(DrawPolygon, 0);
													}

													PolygonPos += MemSizePolygon(DrawPolygon);
													DrawPolygon = (PolygonRecord *) PolygonPos;
												}

												if ((PenPlotMode) || (GerberInfo.PlotMode & 1))
													GerberPlotSpecialAreaFill(AreaFill, 0);
											}

											if (Mirror == 0)
											{
												x4 += incX;
												y4 += incY;
											}
											else
											{
												x4 -= incX;
												y4 += incY;
											}

											str2++;
										}
									}

									if (FontNr == 0)
									{
										if (Mirror == 0)
											x1 += sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.1;
										else
											x1 -= sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.1;

										y1 -= cos(ANGLE_CONVERT(RotationAngle)) * x2 * 1.1;
									}
									else
									{
										if (Mirror == 0)
											x1 += sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.4;
										else
											x1 -= sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.4;

										y1 -= cos(ANGLE_CONVERT(RotationAngle)) * x2 * 1.4;
									}
								}

								break;

							case OBJECT_POLYGON:
								if ((Object5->Info & OBJECT_FILLED) == 0)
								{
									if (GerberPlotObject(Object5, 0) == -1)
										PlotErrors++;
								}
								else
									ok = 1;

								break;

							case PIN_SMD_POLYGON:
							case PIN_PUT_THROUGH_POLYGON:
								ok = 1;
								break;

							default:
								ok = 1;
								break;
							}
						}
					}
				}
			}
		}

		ok = 1;
	}

#ifdef _DEBUG

	if (NrObjects5 > 0)
	{
		for (cnt = Start; cnt < End; cnt++)
		{
			Object5 = &((*Objects5)[cnt]);

			if ((Object5->Info & 1) == 0)
				ok = 1;
		}
	}

#endif

	sprintf(str, SC(948, "Objects layer %d"), Layer);
	AddPerformanceValue(str);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteGerberText(int32 Layer, int32 mode)
{
	AperTureRecord *AperTure;
	int32 cnt, Mirror;
	int32 FoundText = 0;
	double x1, y1, x2, y2, MaxDivX, TextHeight;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], TextLine[8][MAX_LENGTH_STRING];
	struct tm *today;
	time_t ltime;

	LastTraceAperTure.x = 10.0;

	for (cnt = 0; cnt < 8; cnt++)
	{
		if (GerberInfo.TextLine[cnt] != 0)
			FoundText = 1;
	}

	memset(&TextLine, 0, sizeof(TextLine));

	if (!FoundText)
		return -1;

	CurrentAperTureNr = CheckTraceAperTure(max((6.0 * 2540.0), Design.SilkScreenWidth));

	if ((CurrentAperTureNr < 0) || (CurrentAperTureNr >= NrAperTures))
		return -2;

	AperTure = &((*AperTures)[CurrentAperTureNr]);

	if (!PenPlotMode)
	{
		sprintf(str, "G54D%i*", AperTure->AperTureCode);
		WriteGerberString(str, 1);
	}

	for (cnt = 0; cnt < 8; cnt++)
	{
		strcpy(TextLine[cnt], GerberInfo.TextLine[cnt]);

		if (GerberInfo.TextLine[cnt] != 0)
		{
			if (stricmpOwn(GerberInfo.TextLine[cnt], "$DesignName") == 0)
			{
				GetFilePartFromFileName(str2, EditFile);
				CutExtensionFileName(str2);
				strcpy(TextLine[cnt], str2);
			}

			if (stricmpOwn(GerberInfo.TextLine[cnt], "$Date") == 0)
			{
				time(&ltime);
				today = localtime(&ltime);
				strftime(str2, 100, "%B %d, %Y %X", today);
//        strftime(str2,100,"%#c",today);
//        strftime(str2,100,"%X   %x",today);
				strcpy(TextLine[cnt], str2);
			}

			if (stricmpOwn(GerberInfo.TextLine[cnt], "$Layer") == 0)
			{
				if (!GerberInfo.ReverseLayerNumbering)
					GetLayerTextObjects(Layer, TextLine[cnt], 64);
				else
					GetLayerTextObjects(Layer, TextLine[cnt], 64 + 16);

//        GetLayerTextObjects(Layer,TextLine[cnt],0);
			}
		}
	}

	TextHeight = (200 * 2540);
	FindMinMaxBoard(&x1, &y1, &x2, &y2, 1);
	y1 -= (TextHeight * 1.5);
	Mirror = 0;

	if (GerberInfo.Invert)
		Mirror = 1;

	for (cnt = 0; cnt < 4; cnt++)
	{
		if (TextLine[cnt] != 0)
		{
			if (!PenPlotMode)
				DrawGerberStr(x1, y1, TextHeight * 0.8, TextPen, 0, 0, Mirror, (LPSTR) TextLine[cnt]);
			else
				DrawGerberStr(x1, y1, TextHeight * 0.8, CurrentPlotPenSize, 0, 0, Mirror, (LPSTR) TextLine[cnt]);
		}

		y1 -= TextHeight;
	}

	MaxDivX = -10000;

	for (cnt = 0; cnt < 4; cnt++)
	{
		if (TextLine[cnt + 4] != 0)
		{
			GetMinMaxText2(x1, y1, TextHeight * 0.8, 0, 0.0, 0, 0, (LPSTR) TextLine[cnt + 4]);
			MaxDivX = max(MaxDivX, TextMaxX - TextMinX);
		}
	}

	if (MaxDivX < -1000)
		return 0;

	x1 = Design.BoardOriginX + Design.BoardWidth - MaxDivX;
	y1 = Design.BoardOriginY - (TextHeight * 1.5);

	for (cnt = 0; cnt < 4; cnt++)
	{
		if (TextLine[cnt + 4] != 0)
		{
			if (!PenPlotMode)
				DrawGerberStr(x1, y1, TextHeight * 0.8, TextPen, 0, 0, Mirror, (LPSTR) TextLine[cnt + 4]);
			else
				DrawGerberStr(x1, y1, TextHeight * 0.8, CurrentPlotPenSize, 0, 0, Mirror, (LPSTR) TextLine[cnt + 4]);
		}

		y1 -= TextHeight;
	}

// GerberInfo.TextLine
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrillOutput()
{
	int32 cnt, cnt2, Drillfp;
	ObjectRecord *Object, *Object4;
	CompRecord *Comp;
	ViaRecord *Via;
	ObjectArcRecord *ObjectArc;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], FilePart[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING];

	if (AllocateMemAperTures(1024) != 0)
		return -1;

	NrDrillAperTures = 0;

	sprintf(InfoStr, SC(949, "Drills export"));
	RedrawInfoStr(1);
	GetFilePartFromFileName(FilePart, EditFile);
	CutExtensionFileName(FilePart);

	if (GerberInfo.DrillOutputOption == 0)
	{
		sprintf(FileStr, "%s\\pcb\\gerber\\Drills.drl", DesignPath, FilePart); //název souboru
		
		sprintf(str, "\"Drills.drl\"");                      //pøidán tisk do gerberfiles.txt
		AppendStringToTextFileUTF8(GerberListFilename, str); //pøidán tisk do gerberfiles.txt

		//********************************************** nelze vytvoøit soubor **************************************************
		if ((Drillfp = FileOpenWriteUTF8(FileStr)) <= 0)
		{
			sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
			return -2;
		}
		//***********************************************************************************************************************

		sprintf(str, "G90");
		WriteLn(Drillfp, str);
		sprintf(str, "M72");
		WriteLn(Drillfp, str);
	}
	else
	{
		sprintf(FileStr, "%s\\pcb\\gerber\\Drills.ncd", DesignPath, FilePart); //název souboru

		if ((GerberInfo.GerberNumberMode & 8) == 0)
		{	// INCH
			switch (GerberInfo.GerberNumberMode & 7)
			{
			case 2:			// 2 2
				sprintf(str, "\"Drills.ncd\" FSLI22", FilePart);
				break;

			case 3:			// 2 3
				sprintf(str, "\"Drills.ncd\" FSLI23", FilePart);
				break;

			case 4:			// 2 4
				sprintf(str, "\"Drills.ncd\" FSLI24", FilePart);
				break;

			case 5:			// 2 5
				sprintf(str, "\"Drills.ncd\" FSLI25", FilePart);
				break;

			case 6:			// 2 6
				sprintf(str, "\"Drills.ncd\" FSLI26", FilePart);
				break;
			}
		}
		else
		{
			switch (GerberInfo.GerberNumberMode & 7)
			{
			case 2:			// 3 2
				sprintf(str, "\"Drills.ncd\" FSLM32", FilePart);
				break;

			case 3:			// 3 3
				sprintf(str, "\"Drills.ncd\" FSLM33", FilePart);
				break;

			case 4:			// 3 4
				sprintf(str, "\"Drills.ncd\" FSLM34", FilePart);
				break;

			case 5:			// 4 2
				sprintf(str, "\"Drills.ncd\" FSLM42", FilePart);
				break;

			case 6:			// 4 3
				sprintf(str, "\"Drills.ncd\" FSLM43", FilePart);
				break;

			case 7:			// 4 4
				sprintf(str, "\"Drills.ncd\" FSLM44", FilePart);
				break;
			}

			AppendStringToTextFileUTF8(GerberListFilename, str);
		}

		//********************************************** nelze vytvoøit soubor *********************************************************
		if ((Drillfp = FileOpenWriteUTF8(FileStr)) <= 0)
		{
			sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
			return -2;
		}
		//******************************************************************************************************************************
	}

	NrObjects4 = 0;

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

				switch (Object->ObjectType)
				{
				case DRILL_UNPLATED:
				case DRILL:
				case PIN_PUT_THROUGH_ROUND:
				case PIN_PUT_THROUGH_SQUARE:
				case PIN_PUT_THROUGH_POLYGON:
					if (NrObjects4 + 1 > MaxNrObjects4)
					{
						if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
							return -1;
					}

					if ((Object->ObjectType == DRILL_UNPLATED) || (Object->ObjectType == DRILL))
						Object->y2 = Object->x2;

					Object4 = &((*Objects4)[NrObjects4]);
					memmove(Object4, Object, sizeof(ObjectRecord));
					Object4->Test = 0;
					Object4->Info = 0;

					if (Object->ObjectType == DRILL_UNPLATED)
						Object4->Info2 = CheckUnPlatedDrillAperTure(Object->y2);
					else
						Object4->Info2 = CheckPlatedDrillAperTure(Object->y2);

					NrObjects4++;
					FillPositionObject(Object4);
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (NrObjects4 + 1 > MaxNrObjects4)
			{
				if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
					return -1;
			}

			Object4 = &((*Objects4)[NrObjects4]);
			Object4->x1 = Via->X;
			Object4->y1 = Via->Y;
			Object4->x2 = Via->ThickNess;
			Object4->ObjectType = VIA_PUT_THROUGH_ROUND;
			Object4->Info = 0;
			Object4->Test = 0;
			Object4->TraceNr = -1;
			Object4->Info2 = CheckPlatedDrillAperTure(Via->DrillThickNess);
			FillPositionObject(Object4);
			NrObjects4++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectArc->Layer == DRILL_LAYER)
			{
				if (NrObjects4 + 1 > MaxNrObjects4)
				{
					if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
						return -1;
				}

				Object4 = &((*Objects4)[NrObjects4]);
				Object4->x1 = ObjectArc->CentreX;
				Object4->y1 = ObjectArc->CentreY;
				Object4->x2 = ObjectArc->Width;
				Object4->y2 = 0.0;
				Object4->Clearance = 0.0;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->TraceNr = -1;
				Object4->Layer = ObjectArc->Layer;
				Object4->Info2 = CheckPlatedDrillAperTure(ObjectArc->Width);
				Object4->ObjectType = DRILL;
				FillPositionObject(Object4);
				NrObjects4++;
			}

			if (ObjectArc->Layer == DRILL_UNPLATED_LAYER)
			{
				if (NrObjects4 + 1 > MaxNrObjects4)
				{
					if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
						return -1;
				}

				Object4 = &((*Objects4)[NrObjects4]);
				Object4->x1 = ObjectArc->CentreX;
				Object4->y1 = ObjectArc->CentreY;
				Object4->x2 = ObjectArc->Width;
				Object4->y2 = 0.0;
				Object4->Clearance = 0.0;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->TraceNr = -1;
				Object4->Layer = ObjectArc->Layer;
				Object4->Info2 = CheckUnPlatedDrillAperTure(ObjectArc->Width);
				Object4->ObjectType = DRILL_UNPLATED;
				FillPositionObject(Object4);
				NrObjects4++;
			}
		}
	}

	WriteLn(Drillfp, "M48");

	if (GerberInfo.DrillOutputOption == 0)
		WriteLn(Drillfp, "%");
	else
	{
		if ((GerberInfo.GerberNumberMode & 8) == 0)
		{	// INCH
			WriteLn(Drillfp, "INCH,TZ");
		}
		else
			WriteLn(Drillfp, "METRIC,TZ");

		WriteDrillApertureFile(Drillfp, 1);
	}

	GetFilePartFromFileName(FilePart, FileStr);
	sprintf(str, "M47, File: %s", FilePart);
	WriteLn(Drillfp, str);

	if ((GerberInfo.GerberNumberMode & 8) == 0)
	{
		WriteLn(Drillfp, "M72");	// INCH
	}
	else
	{
		WriteLn(Drillfp, "M71");	// MM
	}

	WriteLn(Drillfp, "G90");
	WriteLn(Drillfp, "G05");
	ExcellonDrillOutput(Drillfp);
	WriteLn(Drillfp, "M30");
	FileClose(Drillfp);
	WriteDrillApertureFile(Drillfp, 0);

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	DeAllocateMemAperTures();
	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void ConvertTabToComma(LPSTR str)
{
	int32 cnt;

	for (cnt = 0; cnt < (int32) strlen(str); cnt++)
	{
		if (str[cnt] == '\t')
			str[cnt] = ',';
	}
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 CompPositionOutput(int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, MemPos, ShapeNr, mirror, hulp, fp = 0, result, fp2 =
	            0, LastCompNr, MinObjectNr, MinNr, LastNr, LengthStr, NrRefNums, NrRefCodes, NewMode, NrCompProperties,
	            NotPlaced = 0, Fiducial, FoundFiducials, BottomComponents;
	CompRecord *Comp;
	double x1, y1, ix, iy, Rotation;
	int32 RefNums[16384], Index[MAX_LENGTH_STRING];
	ShapeRecord *Shape;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     str5[MAX_LENGTH_STRING], RefCodes[100][12], FileStr[MAX_LENGTH_STRING], FileStr2[MAX_LENGTH_STRING],
	     PropertyID[MAX_LENGTH_STRING], PropertyValue[MAX_LENGTH_STRING];
	LPSTR RefName;
	struct tm *today;
	time_t ltime;
#ifdef _DEBUG
	int32 ok;
#endif

	MinObjectNr = 0;

	result =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COMP_POS_OUPUT), PCBWindow,
	                 (DLGPROC) CompPositionOutputDialog2);

	if (result == 0)
		return -1;

	sprintf(FileStr, "%s\\pcb\\CompPosition.txt", DesignPath); //název souboru
	sprintf(FileStr2, "%s\\pcb\\CompPosition.csv", DesignPath); //název souboru

	//********************************************** nelze vytvoøit soubor ******************************************************
	if ((fp = FileOpenWriteUTF8(FileStr)) <= 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -2;
	}
	//********************************************** nelze vytvoøit soubor ******************************************************
	if ((fp2 = FileOpenWriteUTF8(FileStr2)) <= 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr2);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -2;
	}
	//***************************************************************************************************************************

	FoundFiducials = 0;
	BottomComponents = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		mirror = ((Comp->CompMode & 8) >> 3);
		Comp->Info3 = 0;
		NotPlaced = 0;
		Fiducial = 0;

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((mode & 4) == 0)
			{
				if (mirror == 1)
					BottomComponents = 1;

				NrCompProperties = GetCompProperties(Comp, NULL, NULL, 0x40);

				if (NrCompProperties > 0)
				{
					for (cnt2 = 0; cnt2 < NrCompProperties; cnt2++)
					{
						if (GetCompProperties(Comp, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) == 0)
						{
							if (stricmp(PropertyID, "FIDUCIAL") == 0)
								FoundFiducials = 1;
						}
					}
				}
			}
		}
	}

	GetFilePartFromFileName(str2, EditFile);
	CutExtensionFileName(str2);
	time(&ltime);
	today = localtime(&ltime);
	strftime(str3, 100, "%B %d, %Y %X", today);
	sprintf(str, "# Pick and place data from design: %s   Date: %s", str2, str3);
	WriteLn(fp, str);
	WriteLn(fp, "# Coordinates SMD components: Center of Footprint");

	switch (CompPosOutput.Units & 3)
	{
	case 0:
		WriteLn(fp, "# Units: thou");
		break;

	case 1:
		WriteLn(fp, "# Units: mm");
		break;

	case 2:
		WriteLn(fp, "# Units: inch");
		break;
	}

	WriteLn(fp, "# Option: PartNumber Option: Not placed comps");
	WriteLn(fp, "# Rotation is counter clock wise");
	WriteLn(fp, "#");

	for (NewMode = 0; NewMode < 5; NewMode++)
	{
		NrRefCodes = 0;

		switch (NewMode)
		{
		case 0:
			if (FoundFiducials)
			{
				WriteLn(fp, "######    Top side #####");
				WriteLn(fp, "# Board fiducials Top");
				WriteLn(fp, "# Ref  Position X Y          Rotation Comp value            SMD/THT   NP");
			}

//          WriteLn(fp,"# ------------------------------------------------------------------------------------");
			break;

		case 1:
			WriteLn(fp, "######    Top side     #####");
			WriteLn(fp, "# Ref  Position X Y          Rotation Comp value            SMD/THT   NP");
//          WriteLn(fp,"# ------------------------------------------------------------------------------------");
			break;

		case 2:
			if (BottomComponents)
			{
				WriteLn(fp, "######    Bottom side  #####");
				WriteLn(fp, "# Ref   Position (    X         Y )  Rot  Comp value          SMD/THT   NP");
			}

//          WriteLn(fp,"# ------------------------------------------------------------------------------------");
			break;

		case 3:
			WriteLn(fp2, "######    Top side     #####");
			WriteLn(fp2, "Ref,Position X,Y,Rotation,Comp value,SMD/THT,NP");
			break;

		case 4:
			if (BottomComponents)
			{
				WriteLn(fp2, "######    Bottom side  #####");
				WriteLn(fp2, "Ref,Position X,Y,Rotation,Comp value,SMD/THT,NP");
			}

			break;
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
			mirror = ((Comp->CompMode & 8) >> 3);
			Comp->Info3 = 0;
			Comp->Info5 = -1;
			NotPlaced = 0;
			Fiducial = 0;

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrCompProperties = GetCompProperties(Comp, NULL, NULL, 0x40);

				if (NrCompProperties > 0)
				{
					for (cnt2 = 0; cnt2 < NrCompProperties; cnt2++)
					{
						if (GetCompProperties(Comp, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt2) == 0)
						{
							if (stricmp(PropertyID, "FIDUCIAL") == 0)
								Fiducial = 1;

							if (stricmp(PropertyID, "MULTI_ASSY") == 0)
							{
								if (stricmp(PropertyValue, "NP") == 0)
									NotPlaced = 1;
							}
						}
					}
				}

#ifdef _DEBUG

				if (NewMode == 2)
				{
					if (stricmp(Comp->Name, "TP3") == 0)
					{
//            Rotation=115.67;
						ok = 1;
					}
				}

#endif

				switch (NewMode)
				{
				case 0:
					if (!Fiducial)
						continue;

					break;

				case 1:
					if (Fiducial)
						continue;

					if (mirror == 1)
						continue;

					break;

				case 2:
					if (Fiducial)
						continue;

					if (mirror == 0)
						continue;

					break;

				case 3:
					if (Fiducial)
						continue;

					if (mirror == 1)
						continue;

					break;

				case 4:
					if (Fiducial)
						continue;

					if (mirror == 0)
						continue;

					break;
				}

#ifdef _DEBUG

				if (NewMode == 2)
					ok = 1;

#endif
				RefName = Comp->Name;
				LengthStr = strlen(RefName);
				cnt2 = 0;

				while ((cnt2 < LengthStr) && (isalpha(RefName[cnt2])))
					cnt2++;

				memset(&str, 0, 100);
				strncpy(str, RefName, cnt2);
				cnt3 = 0;

				while ((cnt3 < NrRefCodes) && (stricmpOwn(RefCodes[cnt3], str) != 0))
					cnt3++;

				if (cnt3 >= NrRefCodes)
				{
					cnt4 = 0;

					while ((cnt4 < NrRefCodes) && (stricmpOwn(RefCodes[cnt4], str) < 0))
						cnt4++;

					if (cnt4 < NrRefCodes)
					{
						memmove(&RefCodes[cnt4 + 1], &RefCodes[cnt4], (NrRefCodes - cnt4) * 12);
						memmove(&Index[cnt4 + 1], &Index[cnt4], (NrRefCodes - cnt4) * sizeof(int32));
					}

					strcpy(RefCodes[cnt4], str);
					Comp->Info5 = (int16) NrRefCodes;
					Index[cnt4] = NrRefCodes;
					NrRefCodes++;
				}
				else
					Comp->Info5 = (int16) Index[cnt3];

				sscanf(&(RefName[cnt2]), "%i", &hulp);
				Comp->Info4 = (int16) hulp;
			}
		}

		for (cnt2 = 0; cnt2 < NrRefCodes; cnt2++)
		{
			NrRefNums = 0;

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->Info5 == Index[cnt2]) && (NrRefNums < 16384))
					RefNums[NrRefNums++] = cnt;
			}

			LastNr = -1;

			for (cnt = 0; cnt < NrRefNums; cnt++)
			{
				MinNr = 1000000;

				for (cnt3 = 0; cnt3 < NrRefNums; cnt3++)
				{
					Comp = (CompRecord *) & (CompsMem[(*Comps)[RefNums[cnt3]]]);

					if (Comp->Info3 == 0)
					{
						if ((Comp->Info4 != -1) && (Comp->Info4 < MinNr))
						{
							MinObjectNr = RefNums[cnt3];
							MinNr = Comp->Info4;
						}
					}
				}

				if (MinNr != 1000000)
				{
					Comp = (CompRecord *) & (CompsMem[(*Comps)[MinObjectNr]]);
					Comp->Info3 = 1;

					if (MinNr != LastNr)
					{
						LastNr = MinNr;
						LastCompNr = MinObjectNr;
						ShapeNr = (int32) Comp->ShapeNr;

						if (ShapeNr == -1)
						{
							FileClose(fp);
							return -1;
						}

						MemPos = (*Shapes)[ShapeNr].ShapePos;
						Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
						Rotation = Comp->Rotation;
#ifdef _DEBUG

						if (stricmp(Comp->Name, "U1") == 0)
						{
							//            Rotation=115.67;
							ok = 1;

							if (NewMode == 2)
								ok = 1;
						}

#endif
						str5[0] = 0;

						switch (CompPosOutput.SmdThroughHole)
						{
						case 0:	// Only SMD
							if ((Shape->Info & SMD_DEVICE) == SMD_DEVICE)
								strcpy(str5, "SMD");
							else
								continue;

							break;

						case 1:	// SMD and THT
							if ((Shape->Info & SMD_DEVICE) == SMD_DEVICE)
								strcpy(str5, "SMD");
							else
								strcpy(str5, "THT");

							break;
						}

						x1 = 0.0;
						y1 = 0.0;
						mirror = ((Comp->CompMode & 8) >> 3);
						ix = Shape->InsertionX;
						iy = Shape->InsertionY;

						if (mirror == 1)
							ix = -ix;

						RotatePoint2(&ix, &iy, Rotation);
						//          RotateFlipPoint(&ix,&iy,x1,y1,Rotation);
						ix += Comp->CompOriginX;
						iy += Comp->CompOriginY;

						if (CompPosOutput.ValuePartNr == 0)
							strcpy(str4, Comp->Value);
						else
							strcpy(str4, Comp->PartNr);

						for (cnt4 = 0; cnt4 < (int) strlen(str4); cnt4++)
						{
							if (str4[cnt4] == ' ')
								str4[cnt4] = '_';
						}

						if (NewMode < 3)
						{
							switch (CompPosOutput.Units & 3)
							{
							case 0:
								sprintf(str, "%-5s   %10.2f%10.2f  %6.2f  %-21s %-18s", Comp->Name, ix / 2540,
								        iy / 2540, Rotation, str4, str5);
								break;

							case 1:
								sprintf(str, "%-5s   %10.3f%10.3f  %6.2f  %-21s %-18s", Comp->Name, ix / 100000,
								        iy / 100000, Rotation, str4, str5);
								break;

							case 2:
								sprintf(str, "%-5s   %10.5f%10.5f  %6.2f  %-21s %-18s", Comp->Name, ix / 2540000,
								        iy / 2540000, Rotation, str4, str5);
								break;
							}

							if ((CompPosOutput.NotPlaced) && (NotPlaced))
								strcat(str, " NP");

							WriteLn(fp, str);
						}
						else
						{
							switch (CompPosOutput.Units & 3)
							{
							case 0:
								sprintf(str, "%s\t%.2f\t%.2f\t%.2f\t%s\t%s", Comp->Name, ix / 2540, iy / 2540, Rotation,
								        str4, str5);
								break;

							case 1:
								sprintf(str, "%s\t%.3f\t%.3f\t%.2f\t%s\t%s", Comp->Name, ix / 100000, iy / 100000,
								        Rotation, str4, str5);
								break;

							case 2:
								sprintf(str, "%s\t%.5f\t%.5f\t%.2f\t%s\t%s", Comp->Name, ix / 2540000, iy / 2540000,
								        Rotation, str4, str5);
								break;
							}

							if ((CompPosOutput.NotPlaced) && (NotPlaced))
								strcat(str, "\tNP");

							ConvertTabToComma(str);
							WriteLn(fp2, str);
						}
					}
				}
			}
		}
	}

	FileClose(fp);
	FileClose(fp2);

	//********************************************** nelze vytvoøit soubor *********************************************************
	if (WriteLnError != 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}
	//******************************************************************************************************************************
   
	//*************************************************************************************************
	//************************************* Následující soubory vygenerovány **************************
	//*************************************************************************************************

	sprintf(str, SC(514,"The following files are generated.\n\n"));
	
	strcat(str, SC(515, "Output component positions as text lines :\n\n"));
	sprintf(str2, "%s", FileStr);
	strcat(str, str2);
	sprintf(FileStr, "%s\\pcb\\comp_pos.txt", DesignPath);

	strcat(str, SC(516, "\n\n\n\nOutput component positions to be used in a spreadsheet :\n\n"));
	sprintf(str2, "%s", FileStr2);
	strcat(str, str2);
	sprintf(FileStr2, "%s\\pcb\\comp_pos.csv", DesignPath);
	
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

void WriteComponents(int Filefp)
{
	int32 cnt, cnt2, cnt3, cnt4, ShapeNr, hulp, LastCompNr, MinObjectNr, MinNr, LastNr, LengthStr, NrRefNums,
	      NrRefCodes, RefNums[16384], Index[100];
	CompRecord *Comp;
	char str[MAX_LENGTH_STRING], RefCodes[100][12];
	LPSTR RefName;

	WriteLn(Filefp, "# Components");

	MinObjectNr = 0;
	NrRefCodes = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->Info3 = 0;

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			RefName = Comp->Name;
			LengthStr = strlen(RefName);
			cnt2 = 0;

			while ((cnt2 < LengthStr) && (isalpha(RefName[cnt2])))
				cnt2++;

			memset(&str, 0, 100);
			strncpy(str, RefName, cnt2);
			cnt3 = 0;

			while ((cnt3 < NrRefCodes) && (stricmpOwn(RefCodes[cnt3], str) != 0))
				cnt3++;

			if (cnt3 >= NrRefCodes)
			{
				cnt4 = 0;

				while ((cnt4 < NrRefCodes) && (stricmpOwn(RefCodes[cnt4], str) < 0))
					cnt4++;

				if (cnt4 < NrRefCodes)
				{
					memmove(&RefCodes[cnt4 + 1], &RefCodes[cnt4], (NrRefCodes - cnt4) * 12);
					memmove(&Index[cnt4 + 1], &Index[cnt4], (NrRefCodes - cnt4) * sizeof(int32));
				}

				strcpy(RefCodes[cnt4], str);
				Comp->Info5 = (int16) NrRefCodes;
				Index[cnt4] = NrRefCodes;
				NrRefCodes++;
			}
			else
				Comp->Info5 = (int16) Index[cnt3];

			sscanf(&(RefName[cnt2]), "%i", &hulp);
			Comp->Info4 = (int16) hulp;
		}
	}

	for (cnt2 = 0; cnt2 < NrRefCodes; cnt2++)
	{
		NrRefNums = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->Info5 == Index[cnt2]) && (NrRefNums < 16384))
				RefNums[NrRefNums++] = cnt;
		}

		LastNr = -1;

		for (cnt = 0; cnt < NrRefNums; cnt++)
		{
			MinNr = 1000000;

			for (cnt3 = 0; cnt3 < NrRefNums; cnt3++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[RefNums[cnt3]]]);

				if (Comp->Info3 == 0)
				{
					if ((Comp->Info4 != -1) && (Comp->Info4 < MinNr))
					{
						MinObjectNr = RefNums[cnt3];
						MinNr = Comp->Info4;
					}
				}
			}

			if (MinNr != 1000000)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[MinObjectNr]]);
				Comp->Info3 = 1;

				if (MinNr != LastNr)
				{
					LastNr = MinNr;
					LastCompNr = MinObjectNr;
					ShapeNr = (int32) Comp->ShapeNr;

					if (ShapeNr == -1)
						return;

					sprintf(str, "COMP");
					strcat(str, "                                                              ");
					memmove(&str[6], Comp->Name, strlen(Comp->Name));
					memmove(&str[31], Comp->ShapeName, strlen(Comp->ShapeName));
					strcat(str, Comp->Value);
					WriteLn(Filefp, str);
				}
			}
		}
	}

	return;
}

//******************************************************************************************************************************
//************************************** generování souboru Layers.txt ve složce gerber ****************************************
//******************************************************************************************************************************

void WriteLayerInfo()
{
	int32 cnt3, Layer, res, fp;
	char str[MAX_LENGTH_STRING * 3], str2[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING];
	struct tm *today;
	time_t ltime;
	double minx, miny, maxx, maxy;
	
	strcpy(str, EditFile);
	CutExtensionFileName(str);
	GetFilePartFromFileName(FileName, str);
	//sprintf(FileStr, "%s\\pcb\\gerber\\", DesignPath); //pùvodní název souboru
	//strcat(FileStr, "Layers");
	//strcat(FileStr, ".txt");

	sprintf(FileStr, "%s\\pcb\\gerber\\Layers.txt", DesignPath); //nový název souboru
	
    //********************************************** nelze vytvoøit soubor ******************************************************
	if ((fp = FileOpenWriteUTF8(FileStr)) <= 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return;
	}
	//***************************************************************************************************************************

	time(&ltime);
	today = localtime(&ltime);
	strftime(str2, 100, "%B %d, %Y %X", today);

	sprintf(str, SC(148, "Layout  %s\n\nDate : %s"), FileName, str2);
	WriteLn(fp, str);
	WriteLn(fp, "");
	sprintf(str, "Designed with PCB elegance %d.%d", VER_VERSION / 100, VER_VERSION % 100);
	WriteLn(fp, str);
	WriteLn(fp, "");
	WriteLn(fp, "---------------------------------------------------------------------");
	WriteLn(fp, SC(958, "Gerber files :"));
	WriteLn(fp, "");
	strcpy(str, SC(367, "Filename"));
	strcat(str, "                      ");
	str[30] = 0;
	strcpy(str2, SC(959, "Polarity"));
	strcat(str2, "                      ");
	str2[12] = 0;
	strcat(str, str2);
	strcat(str, SC(960, "Layer name"));
	WriteLn(fp, str);
	WriteLn(fp, "---------------------------------------------------------------------");

	for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
	{
		Layer = GerberLayers[cnt3];

		switch (Layer)
		{
		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM:
		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
		case BOARD_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			GetLayerTextObjects(Layer, str, 5);
			GetLayerTextObjects(Layer, str2, 3);
			strcat(str, ".ger                         ");
			str[30] = 0;
			strcat(str, "P           ");
			strcat(str, str2);
			WriteLn(fp, str);
			break;
		}

		if ((Layer >= 0) && (Layer < 32))
		{
			if (!GerberInfo.ReverseLayerNumbering)
			{
				GetLayerTextObjects(Layer, str, 64 + 5);
				GetLayerText(Layer, str2, 64 + 6);
			}
			else
			{
				GetLayerTextObjects(Layer, str, 64 + 16 + 5);
				GetLayerText(Layer, str2, 64 + 16 + 6);
			}

			strcat(str, ".ger                         ");
			str[30] = 0;

			if (CheckIfLayerIsAPowerPlane(Layer))
				strcat(str, "N           ");
			else
				strcat(str, "P           ");

			strcat(str, str2);
			WriteLn(fp, str);
		}

		if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		{
			if (!GerberInfo.ReverseLayerNumbering)
			{
				GetLayerTextObjects(Layer, str, 5);
				GetLayerTextObjects(Layer, str2, 4);
			}
			else
			{
				GetLayerTextObjects(Layer, str, 16 + 5);
				GetLayerTextObjects(Layer, str2, 16 + 4);
			}

			strcat(str, ".ger                         ");
			str[30] = 0;
			strcat(str, "P           ");
			strcat(str, str2);
			WriteLn(fp, str);
		}
	}

	if (GerberInfo.GerberOutputMode == 0)
	{
		WriteLn(fp, "");
		strcpy(str, SC(961, "Aperture file (Wheel table)"));
		strcat(str, "                             ");
		str[42] = 0;
		strcat(str, "gerber.txt");
		WriteLn(fp, str);
	}

	WriteLn(fp, "");

	for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
	{
		Layer = GerberLayers[cnt3];

		switch (Layer)
		{
		case DRILL_LAYER:
			WriteLn(fp, "");
			WriteLn(fp, "---------------------------------------------------------------------");

			if (GerberInfo.DrillOutputOption == 0)
			{
				WriteLn(fp, SC(962, "Drills files :"));
				WriteLn(fp, "---------------------------------------------------------------------");
				sprintf(str, "Drills.drl", FileName); //layers.txt
				strcat(str, "                                          ");
				str[42] = 0;
				strcat(str, SC(963, "Drills file"));
				WriteLn(fp, str);
				strcpy(str, "Drills"); //layers.txt
				strcat(str, ".txt                                         ");
				str[42] = 0;
				strcat(str, SC(965, "Drills tool file"));
				WriteLn(fp, str);
			}
			else
			{
				WriteLn(fp, SC(962, "Drills file :"));
				WriteLn(fp, "---------------------------------------------------------------------");
				sprintf(str, "Drills.ncd", FileName); //layers.txt
				strcat(str, "                                          ");
				str[42] = 0;
				strcat(str, SC(963, "Drills file"));
				WriteLn(fp, str);
			}

			break;
		}
	}

	if (GerberInfo.DrillAsGerber)
	{
		sprintf(str, "Drills.ger"); //layers.txt
		strcat(str, "                                          ");
		str[42] = 0;
		strcat(str, SC(1161, "Drill file (gerber output)"));
		WriteLn(fp, str);
	}

	WriteLn(fp, "");


	if (GerberInfo.GerberOutputMode == 0)
	{
		WriteLn(fp, "---------------------------------------------------------------------");
		WriteLn(fp, SC(966, "Gerber output specification :"));
		WriteLn(fp, "---------------------------------------------------------------------");
		WriteLn(fp, "");

		switch (GerberInfo.GerberNumberMode & 7)
		{
		case 0:
			sprintf(str, SC(262, "Number format"));
			strcat(str, " : 2 3");
			WriteLn(fp, str);
			break;

		case 1:
			sprintf(str, SC(262, "Number format"));
			strcat(str, " : 2 4");
			WriteLn(fp, str);
			break;

		case 2:
			sprintf(str, SC(262, "Number format"));
			strcat(str, " : 2 5");
			WriteLn(fp, str);
			break;
		}

		strcpy(str, SC(967, "Suppression"));
		strcat(str, "   : ");
		strcat(str, SC(968, "Leading zero"));
		WriteLn(fp, str);
		strcpy(str, SC(969, "Coordinates"));
		strcat(str, "   : ");
		strcat(str, SC(970, "Absolute"));
		WriteLn(fp, str);

	}

	WriteLn(fp, "");

	if (GerberInfo.OutputNeutral == 1)
	{
		WriteLn(fp, "---------------------------------------------------------------------");
		WriteLn(fp, SC(971, "Test information :"));
		WriteLn(fp, "---------------------------------------------------------------------");
		WriteLn(fp, "");
		sprintf(str, "NeutralFile.neu", FileName); //layers.txt
		strcat(str, "                                 ");
		str[36] = 0;
		strcat(str, SC(972, "Neutral file (PCB testing)"));
		WriteLn(fp, str);
	}

	WriteLn(fp, "");
	strcpy(str, "PCB");
	strcat(str, " ");
	strcat(str, SC(973, "Specifications :"));
	WriteLn(fp, "---------------------------------------------------------------------");
	WriteLn(fp, str);
	WriteLn(fp, "---------------------------------------------------------------------");
	WriteLn(fp, "");

	sprintf(str2, SC(974, "PCB size (width, height)"));
	strcat(str2, "                                      ");
	str2[36] = 0;

	res = FindMinMaxBoard(&minx, &miny, &maxx, &maxy, 0);

	sprintf(str, "%s: %11.4f, %11.4f thou", str2, (maxx - minx) / 2540, (maxy - miny) / 2540);
	WriteLn(fp, str);
	sprintf(str, "%s: %12.5f, %12.5f inch", str2, (maxx - minx) / 2540000, (maxy - miny) / 2540000);
	WriteLn(fp, str);
	sprintf(str, "%s: %10.3f, %10.3f mm", str2, (maxx - minx) / 100000, (maxy - miny) / 100000);
	WriteLn(fp, str);
	WriteLn(fp, "");
	sprintf(str2, SC(976, "PCB origin (x, y)"));
	strcat(str2, "                                      ");
	str2[36] = 0;
	sprintf(str, "%s: %11.4f, %11.4f thou", str2, Design.BoardOriginX / 2540, Design.BoardOriginY / 2540);
	WriteLn(fp, str);
	sprintf(str, "%s: %12.5f, %12.5f inch", str2, Design.BoardOriginX / 2540000, Design.BoardOriginY / 2540000);
	WriteLn(fp, str);
	sprintf(str, "%s: %10.3f, %10.3f mm", str2, Design.BoardOriginX / 100000, Design.BoardOriginY / 100000);
	WriteLn(fp, str);
	WriteLn(fp, "");
	WriteLn(fp, "");

	sprintf(str, SC(977, "Board thickness"));
	strcat(str, "                                      ");
	str[36] = 0;
	strcat(str, ": ");
	strcat(str, GerberInfo.PcbThickness);
	WriteLn(fp, str);

	sprintf(str, SC(978, "Material"));
	strcat(str, "                                      ");
	str[36] = 0;
	strcat(str, ": ");
	strcat(str, GerberInfo.PcbMaterial);
	WriteLn(fp, str);

	sprintf(str, SC(979, "Copper thickness"));
	strcat(str, "                                      ");
	str[36] = 0;
	strcat(str, ": ");
	strcat(str, GerberInfo.PcbCopperThickness);
	WriteLn(fp, str);

	sprintf(str, SC(980, "Number of layers"));
	strcat(str, "                                      ");
	str[36] = 0;
	sprintf(str2, ": %i", Design.NrBoardLayers);
	strcat(str, str2);
	WriteLn(fp, str);

	sprintf(str, SC(981, "Minimal Trace width"));
	strcat(str, "                                      ");
	str[36] = 0;
	sprintf(str2, ": %.0f thou (%.4f mm)", Design.StandardTraceWidth / 2540, Design.StandardTraceWidth / 100000);
	strcat(str, str2);  
	WriteLn(fp, str);
	sprintf(str, SC(982, "Minimal Clearance"));
	strcat(str, "                                      ");
	str[36] = 0;
	sprintf(str2, ": %.0f thou (%.4f mm)", Design.StandardClearance / 2540, Design.StandardClearance / 100000);
	strcat(str, str2);
	WriteLn(fp, str);

	int32 Extra1Count;
	int32 StrCount = 0;
	int32 LineChars = 0;
	
	if (GerberInfo.Extra1[0] != 0)
	{
		for (Extra1Count = 0; Extra1Count < min(sizeof(str), sizeof(GerberInfo.Extra1)); Extra1Count++)
		{
			if (GerberInfo.Extra1[Extra1Count] == ':')
			{
				for (cnt3 = LineChars; cnt3 < 36; cnt3++)
				{
					str[StrCount++] = ' ';
				}
			}
			
			str[StrCount] = GerberInfo.Extra1[Extra1Count];

			if (str[StrCount] == 0)
				break;

			LineChars++;
			if (str[StrCount] == '\n')
				LineChars = 0;

			StrCount++;
		}

		WriteLn(fp, str);
	}

	FileClose(fp);

	//********************************************** nelze vytvoøit soubor ******************************************************
	if (WriteLnError != 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
	}
	//***************************************************************************************************************************
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 WriteNeutralFile(int32 mode)
{

	int32 Count, cnt2, cnt3, PinOffset, MemPos, cnt, NrPins, PinNr, NetCount, Layer2, PinsMemPos, MemPosComp, ShapeInfo,
	      NrPinShapes, ShapeType, ShapeNr, NetNr, fp;
	double x1, y1, x2, y2;
	int32 Found;
	CompRecord *Comp;
	ObjectRecord *Object;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	ShapeRecord *Shape;
	CompPinRecord *CompPin;
	NetRecord *Net;
	NetItemsRecord *NetItem;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING];
	ViaRecord *Via;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Count = 0;
		Net->Pos = 0;
	}

//  cnt3=LoadShape("pci_slot2");

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
			PinsMemPos = MemPosComp + sizeof(CompRecord);
			ShapeNr = (int32) Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
//      if (stricmpOwn(Shape->ShapeName,"pci_slot")==0) {
//        ShapeNr=cnt3;
//        MemPos=(*Shapes)[ShapeNr].ShapePos;
//        Shape=(ShapeRecord *)&(ShapesMem[MemPos]);
//      }
			PinOffset = Shape->PinOffset;
			NrPins = Shape->NrPins;
			MemPos += PinOffset;
			PinNr = 0;
			Found = 0;

			while (NrPins > 0)
			{
				ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
				NrPinShapes = ShapePad->NrPinShapes;
				MemPos += sizeof(ShapePadRecord);
				CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				NetNr = CompPin->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);
					Net->Count++;
				}

//        PinTextFound=(ShapePad->Name);
				for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
				{
					Pad = (PadRecord *) & (ShapesMem[MemPos]);
					ShapeType = Pad->ShapeType;

					if (ShapeType != PIN_ARC)
						MemPos += sizeof(PadRecord);
					else
						MemPos += 48;
				}

				PinsMemPos += sizeof(CompPinRecord);
				NrPins--;
//        PinNr++;
			}
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	if (AllocateMemNetItems(Count) == -1)
		return -1;

	NrNetItems = Count;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
			PinsMemPos = MemPosComp + sizeof(CompRecord);
			ShapeNr = (int32) Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			
			/*
			if (stricmpOwn(Shape->ShapeName,"pci_slot")==0)
			{
				ShapeNr=cnt3;
				MemPos=(*Shapes)[ShapeNr].ShapePos;
				Shape=(ShapeRecord *)&(ShapesMem[MemPos]);
			}
			*/
			
			PinOffset = Shape->PinOffset;
			ShapeInfo = Shape->Info;
			NrPins = Shape->NrPins;
			MemPos += PinOffset;
			PinNr = 0;
			Found = 0;

			while (NrPins > 0)
			{
				ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
				NrPinShapes = ShapePad->NrPinShapes;
				MemPos += sizeof(ShapePadRecord);
				CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				NetNr = CompPin->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);
					NetItem = &((*NetItems)[Net->Pos]);
					strcpy(NetItem->PinStr, ShapePad->Name);
					NetItem->CompNr = cnt;
					NetItem->PinNr = CompPinNr(Comp, NetItem->PinStr);
					Net->Pos++;
				}

				for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
				{
					Pad = (PadRecord *) & (ShapesMem[MemPos]);
					ShapeType = Pad->ShapeType;

					if (ShapeType != PIN_ARC)
						MemPos += sizeof(PadRecord);
					else
						MemPos += 48;
				}

				PinsMemPos += sizeof(CompPinRecord);
				NrPins--;
				PinNr++;
			}
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}
	
	strcpy(FileStr, EditFile);
	CutExtensionFileName(FileStr);
	//strcat(FileStr, ".neu"); //pùvodní název souboru
	
	sprintf(FileStr, "%s\\pcb\\gerber\\NeutralFile.neu", DesignPath); //nový název souboru
	
	//********************************************** nelze vytvoøit soubor *********************************************************
	if ((fp = FileOpenWriteUTF8(FileStr)) <= 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -2;
	}
	//******************************************************************************************************************************
	
	sprintf(str, "# Neutral file %s ", EditFile);
	WriteLn(fp, str);
	strcpy(str, EditFile);
	CutExtensionFileName(str);
	GetFilePartFromFileName(str2, str);
	WriteLn(fp, "#");
	WriteLn(fp, "# BOARD INFORMATION");

	if ((mode & 1) == 0)
	{
		WriteLn(fp, "B_UNITS Mils");
		sprintf(str, "BOARD %s OFFSET x:%.1f y:%.1f ORIENTATION    0", str2, Design.BoardOriginX / 2540.0,
		        Design.BoardOriginY / 2540.0);
	}
	else
	{
		WriteLn(fp, "B_UNITS Mm");
		sprintf(str, "BOARD %s OFFSET x:%.4f y:%.4f ORIENTATION    0", str2, Design.BoardOriginX / 100000.0,
		        Design.BoardOriginY / 100000.0);
	}

	WriteLn(fp, str);
//  WriteLn(fp,"");
	WriteLn(fp, "#");
	WriteLn(fp, "# NETS INFORMATION");
	WriteLn(fp, "#");

	cnt2 = 0;

	for (cnt2 = 0; cnt2 < Design.NrNets; cnt2++)
	{
		Net = &((*Nets)[cnt2]);
		NetCount = Net->Count;

		if ((Net->Name[0] != 0) && (NetCount > 0))
		{
			for (cnt = 0; cnt < NetCount; cnt++)
			{
				NetItem = &((*NetItems)[Net->Pos + cnt]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[NetItem->CompNr]]);
				sprintf(str3, "%s-%s                         ", Comp->Name, NetItem->PinStr);
				str3[16] = 0;

				if (cnt == 0)
				{
					sprintf(str, "NET %s", Net->Name);
					WriteLn(fp, str);
				}

				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

				for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
				{
					Object = &((*Objects)[cnt3]);

					if (Object->NetNr == cnt2)
					{
						if (NetItem->PinNr == Object->PinNr)
						{
							switch (Object->ObjectType)
							{
							case PIN_PUT_THROUGH_ROUND:
								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd%03.0f    0", str3, Object->x1 / 2540,
									        Object->y1 / 2540, Object->x2 / 2540, Object->y2 / 2540);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd%03.0f    0", str3, Object->x1 / 100000,
									        Object->y1 / 100000, Object->x2 / 1000, Object->y2 / 1000);
								}

								WriteLn(fp, str);
								break;

							case PIN_PUT_THROUGH_SQUARE:
								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0fsq%03.0f    0", str3, Object->x1 / 2540,
									        Object->y1 / 2540, Object->x2 / 2540, Object->y2 / 2540);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0fsq%03.0f    0", str3, Object->x1 / 100000,
									        Object->y1 / 100000, Object->x2 / 100000, Object->y2 / 100000);
								}

								WriteLn(fp, str);
								break;

							case PIN_PUT_THROUGH_POLYGON:
								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd%03.0f    0", str3, Object->x1 / 2540,
									        Object->y1 / 2540, 16.0, 16.0);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd%03.0f    0", str3, Object->x1 / 100000,
									        Object->y1 / 100000, 0.4e3, 0.4e3);
								}

								WriteLn(fp, str);
								break;

							case PIN_SMD_ROUND:
								Layer2 = Design.NrBoardLayers - Object->Layer;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, Object->x1 / 2540,
									        Object->y1 / 2540, Object->x2 / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, Object->x1 / 100000,
									        Object->y1 / 100000, Object->x2 / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_LINE_HOR:
								Layer2 = Design.NrBoardLayers - Object->Layer;
								x2 = Object->x2 * 0.5;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3,
									        (Object->x1 + Object->x2 * 0.5) / 2540, Object->y1 / 2540,
									        Object->y2 / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3,
									        (Object->x1 + Object->x2 * 0.5) / 100000, Object->y1 / 100000,
									        Object->y2 / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_LINE_VER:
								Layer2 = Design.NrBoardLayers - Object->Layer;
								x2 = Object->x2 * 0.5;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, Object->x1 / 2540,
									        (Object->y1 + Object->x2 * 0.5) / 2540, Object->y2 / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, Object->x1 / 100000,
									        (Object->y1 + Object->x2 * 0.5) / 100000, Object->y2 / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_LINE_DIAG1:
								Layer2 = Design.NrBoardLayers - Object->Layer;
								x2 = Object->x2 * 0.5;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3,
									        (Object->x1 + Object->x2 * 0.5) / 2540,
									        (Object->y1 - Object->x2 * 0.5) / 2540, Object->y2 / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3,
									        (Object->x1 + Object->x2 * 0.5) / 100000,
									        (Object->y1 - Object->x2 * 0.5) / 100000, Object->y2 / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_LINE_DIAG2:
								x2 = Object->x2 * 0.5;
								Layer2 = Design.NrBoardLayers - Object->Layer;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3,
									        (Object->x1 + Object->x2 * 0.5) / 2540,
									        (Object->y1 + Object->x2 * 0.5) / 2540, Object->y2 / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3,
									        (Object->x1 + Object->x2 * 0.5) / 100000,
									        (Object->y1 + Object->x2 * 0.5) / 100000, Object->y2 / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_LINE_ALL_ANGLE:
								Layer2 = Design.NrBoardLayers - Object->Layer;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, Object->x1 / 2540,
									        Object->y1 / 2540, Object->Thickness / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, Object->x1 / 100000,
									        Object->y1 / 100000, Object->Thickness / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_ARC:
								GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);
								Layer2 = Design.NrBoardLayers - Object->Layer;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, x1 / 2540, y1 / 2540,
									        Object->Thickness / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, x1 / 100000,
									        y1 / 100000, Object->Thickness / 1000, Layer2);
								}

								WriteLn(fp, str);

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, x2 / 2540, y2 / 2540,
									        Object->Thickness / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, x2 / 100000,
									        y2 / 100000, Object->Thickness / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_SMD_RECT:
								Layer2 = Design.NrBoardLayers - Object->Layer;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f smd_%03.0fx%03.0f %i", str3, Object->x1 / 2540,
									        Object->y1 / 2540, Object->x2 / 2540, Object->y2 / 2540, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f smd_%03.0fx%03.0f %i", str3, Object->x1 / 100000,
									        Object->y1 / 100000, Object->x2 / 1000, Object->y2 / 1000, Layer2);
								}

								WriteLn(fp, str);
								break;

							case PIN_SMD_POLYGON:
								Layer2 = Design.NrBoardLayers - Object->Layer;

								if ((mode & 1) == 0)
								{
									sprintf(str, "N_PIN %s %7.1f %7.1f smd_%03.0frd%03.0f    %d", str3,
									        Object->x1 / 2540, Object->y1 / 2540, 16.0, 16.0, Layer2);
								}
								else
								{
									sprintf(str, "N_PIN %s %8.4f %8.4f smd_%03.0frd%03.0f    %d", str3,
									        Object->x1 / 100000, Object->y1 / 100000, 0.4e3, 0.4e3, Layer2);
								}

								WriteLn(fp, str);
								break;
							}
						}
					}
				}
			}

// Insert vias
			for (cnt3 = 0; cnt3 < Design.NrVias; cnt3++)
			{
				Via = &((*Vias)[cnt3]);

				if (((Via->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Via->NetNr == cnt2))
				{
					if ((mode & 1) == 0)
					{
						sprintf(str, "N_VIA                  %7.1f %7.1f %03.0frd%03.0f    1    %d", Via->X / 2540,
						        Via->Y / 2540, Via->ThickNess / 2540, Via->DrillThickNess / 2540, Design.NrBoardLayers);
					}
					else
					{
						sprintf(str, "N_VIA                  %8.4f %8.4f %03.0frd%03.0f    1    %d", Via->X / 100000,
						        Via->Y / 100000, Via->ThickNess / 1000, Via->DrillThickNess / 1000,
						        Design.NrBoardLayers);
					}

					WriteLn(fp, str);
				}
			}
		}
	}

	cnt2 = 100000;

// Insert objects one pin nets
	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->Info & OBJECT_NOT_VISIBLE)
			continue;

#ifdef _DEBUG

		if (stricmp(Comp->Name, "U110") == 0)
			ok = 1;

#endif
		NrObjects = 0;
		ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

		for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
		{
			Object = &((*Objects)[cnt3]);

			if (Object->NetNr == -1)
			{
				sprintf(str, "NET N$%d", cnt2);
				WriteLn(fp, str);
				cnt2++;

				switch (Object->ObjectType)
				{
				case PIN_PUT_THROUGH_ROUND:
					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd%03.0f    0", str3, Object->x1 / 2540,
						        Object->y1 / 2540, Object->x2 / 2540, Object->y2 / 2540);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd%03.0f    0", str3, Object->x1 / 100000,
						        Object->y1 / 100000, Object->x2 / 1000, Object->y2 / 1000);
					}

					WriteLn(fp, str);
					break;

				case PIN_PUT_THROUGH_SQUARE:
					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0fsq%03.0f    0", str3, Object->x1 / 2540,
						        Object->y1 / 2540, Object->x2 / 2540, Object->y2 / 2540);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0fsq%03.0f    0", str3, Object->x1 / 100000,
						        Object->y1 / 100000, Object->x2 / 100000, Object->y2 / 100000);
					}

					WriteLn(fp, str);
					break;

				case PIN_PUT_THROUGH_POLYGON:
					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd%03.0f    0", str3, Object->x1 / 2540,
						        Object->y1 / 2540, 16.0, 16.0);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd%03.0f    0", str3, Object->x1 / 100000,
						        Object->y1 / 100000, 0.4e3, 0.4e3);
					}

					WriteLn(fp, str);
					break;

				case PIN_SMD_ROUND:
					Layer2 = Design.NrBoardLayers - Object->Layer;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, Object->x1 / 2540,
						        Object->y1 / 2540, Object->x2 / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, Object->x1 / 100000,
						        Object->y1 / 100000, Object->x2 / 1000, Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_LINE_HOR:
					Layer2 = Design.NrBoardLayers - Object->Layer;
					x2 = Object->x2 * 0.5;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3,
						        (Object->x1 + Object->x2 * 0.5) / 2540, Object->y1 / 2540, Object->y2 / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3,
						        (Object->x1 + Object->x2 * 0.5) / 100000, Object->y1 / 100000, Object->y2 / 1000,
						        Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_LINE_VER:
					Layer2 = Design.NrBoardLayers - Object->Layer;
					x2 = Object->x2 * 0.5;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, Object->x1 / 2540,
						        (Object->y1 + Object->x2 * 0.5) / 2540, Object->y2 / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, Object->x1 / 100000,
						        (Object->y1 + Object->x2 * 0.5) / 100000, Object->y2 / 1000, Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_LINE_DIAG1:
					Layer2 = Design.NrBoardLayers - Object->Layer;
					x2 = Object->x2 * 0.5;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3,
						        (Object->x1 + Object->x2 * 0.5) / 2540, (Object->y1 - Object->x2 * 0.5) / 2540,
						        Object->y2 / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3,
						        (Object->x1 + Object->x2 * 0.5) / 100000, (Object->y1 - Object->x2 * 0.5) / 100000,
						        Object->y2 / 1000, Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_LINE_DIAG2:
					x2 = Object->x2 * 0.5;
					Layer2 = Design.NrBoardLayers - Object->Layer;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3,
						        (Object->x1 + Object->x2 * 0.5) / 2540, (Object->y1 + Object->x2 * 0.5) / 2540,
						        Object->y2 / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3,
						        (Object->x1 + Object->x2 * 0.5) / 100000, (Object->y1 + Object->x2 * 0.5) / 100000,
						        Object->y2 / 1000, Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_LINE_ALL_ANGLE:
					Layer2 = Design.NrBoardLayers - Object->Layer;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, Object->x1 / 2540,
						        Object->y1 / 2540, Object->Thickness / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, Object->x1 / 100000,
						        Object->y1 / 100000, Object->Thickness / 1000, Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_ARC:
					GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);
					Layer2 = Design.NrBoardLayers - Object->Layer;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, x1 / 2540, y1 / 2540,
						        Object->Thickness / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, x1 / 100000, y1 / 100000,
						        Object->Thickness / 1000, Layer2);
					}

					WriteLn(fp, str);

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f %03.0frd       %i", str3, x2 / 2540, y2 / 2540,
						        Object->Thickness / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f %03.0frd       %i", str3, x2 / 100000, y2 / 100000,
						        Object->Thickness / 1000, Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_SMD_RECT:
					Layer2 = Design.NrBoardLayers - Object->Layer;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f smd_%03.0fx%03.0f %i", str3, Object->x1 / 2540,
						        Object->y1 / 2540, Object->x2 / 2540, Object->y2 / 2540, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f smd_%03.0fx%03.0f %i", str3, Object->x1 / 100000,
						        Object->y1 / 100000, Object->x2 / 1000, Object->y2 / 1000, Layer2);
					}

					WriteLn(fp, str);
					break;

				case PIN_SMD_POLYGON:
					Layer2 = Design.NrBoardLayers - Object->Layer;

					if ((mode & 1) == 0)
					{
						sprintf(str, "N_PIN %s %7.1f %7.1f smd_%03.0frd%03.0f    %d", str3, Object->x1 / 2540,
						        Object->y1 / 2540, 16.0, 16.0, Layer2);
					}
					else
					{
						sprintf(str, "N_PIN %s %8.4f %8.4f smd_%03.0frd%03.0f    %d", str3, Object->x1 / 100000,
						        Object->y1 / 100000, 0.4e3, 0.4e3, Layer2);
					}

					WriteLn(fp, str);
					break;
				}
			}
		}
	}

	FileClose(fp);

	//********************************************** nelze vytvoøit soubor *********************************************************
	if (WriteLnError != 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
	}
	//******************************************************************************************************************************

	DeAllocateMemNetItems();
	return 0;
}


// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 ExcellonDrillOutput(int32 Drillfp)
{

	int32 ok, cnt, cnt2, cntx, cnty, cnty2, Divisions, count, Start, End, NrTempObjects4, DrillApertureIndex[512],
	      DrillApertureCount[512];
	double sx, sy, x1, y1, BoardDivX, BoardDivY;
	ObjectRecord *Object4, *Object5;
	AperTureRecord *DrillAperTure;
	char str[MAX_LENGTH_STRING];
	ObjectArray *Objects5;

	sx = DesignBoardOriginX;
	sy = DesignBoardOriginY;
	Divisions = 6;
	BoardDivX = DesignBoardWidth / Divisions;
	BoardDivY = DesignBoardHeight / Divisions;

	if (NrObjects4 * 2 + 1 > MaxNrObjects4)
	{
		if (AllocateMemObjects4(NrObjects4 * 2 + 1) == -1)
			return -1;
	}

	Objects5 = (ObjectArray *) & ((*Objects4)[NrObjects4]);

	for (cnt = 0; cnt < 512; cnt++)
		DrillApertureCount[cnt] = 0;

	for (cnt = 0; cnt < NrObjects4; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);

		if ((Object4->Info2 != -1) && (Object4->Info2 < NrDrillAperTures))
			DrillApertureCount[Object4->Info2]++;
	}

	DrillApertureIndex[0] = 0;

	for (cnt = 0; cnt < 510; cnt++)
		DrillApertureIndex[cnt + 1] = DrillApertureIndex[cnt] + DrillApertureCount[cnt];

	for (cnt = 0; cnt < NrObjects4; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);

		if ((Object4->Info2 != -1) && (Object4->Info2 < NrDrillAperTures))
		{
			Object5 = &((*Objects5)[DrillApertureIndex[Object4->Info2]]);
			DrillApertureIndex[Object4->Info2]++;
			memmove(Object5, Object4, sizeof(ObjectRecord));
		}
	}

	DrillApertureIndex[0] = 0;

	for (cnt = 0; cnt < 511; cnt++)
		DrillApertureIndex[cnt + 1] = DrillApertureIndex[cnt] + DrillApertureCount[cnt];


	for (cnt2 = 0; cnt2 < NrDrillAperTures; cnt2++)
	{
		if (DrillApertureCount[cnt2] > 0)
		{
			DrillAperTure = &((*DrillAperTures)[cnt2]);
			CurrentAperTureNr = cnt2;
			sprintf(str, "T%i", DrillAperTure->AperTureCode);
			WriteLn(Drillfp, str);
			count = 0;

			for (cntx = 0; cntx < Divisions; cntx++)
			{
				for (cnty = 0; cnty < Divisions; cnty++)
				{
					cnty2 = cnty;

					if ((cntx & 1) == 1)
						cnty2 = Divisions - cnty - 1;

					SearchMinX = sx + (cntx) * BoardDivX - (1 * 2540);
					SearchMinY = sy + (cnty2) * BoardDivY - (1 * 2540);
					SearchMaxX = sx + (cntx + 1) * BoardDivX + (1 * 2540);
					SearchMaxY = sy + (cnty2 + 1) * BoardDivY + (1 * 2540);

					NrTempObjects4 = 0;
					Start = DrillApertureIndex[cnt2];
					End = DrillApertureIndex[cnt2] + DrillApertureCount[cnt2];

					for (cnt = Start; cnt < End; cnt++)
					{
						Object5 = &((*Objects5)[cnt]);

						if (((Object5->Info & 1) == 0) && (Object5->minx <= SearchMaxX) && (Object5->maxx >= SearchMinX)
						        && (Object5->miny <= SearchMaxY) && (Object5->maxy >= SearchMinY))
						{
							Object5->Info |= 1;
							x1 = Object5->x1;
							y1 = Object5->y1;

							if (GerberInfo.Invert)
								x1 = (DesignBoardOriginX + DesignBoardWidth - x1);

							GetGerberValueInString(x1, y1, str, 0);
							WriteLn(Drillfp, str);
						}
					}
				}
			}
		}

		ok = 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindMinApertureDrill()
{
	int32 cnt, Found;
	AperTureRecord *DrillAperTure;
	double MinDrill;

	MinDrill = 1000000000;
	Found = -1;

	for (cnt = 0; cnt < NrDrillAperTures; cnt++)
	{
		DrillAperTure = &((*DrillAperTures)[cnt]);

		if (DrillAperTure->Info4 == 0)
		{
			if (DrillAperTure->x < MinDrill)
			{
				Found = cnt;
				MinDrill = DrillAperTure->x;
			}
		}
	}

	return Found;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindMinApertureDCode()
{
	int32 cnt, MinDCode, Found;
	AperTureRecord *AperTure;

	MinDCode = 10000;
	Found = -1;

	for (cnt = 0; cnt < NrAperTures; cnt++)
	{
		AperTure = &((*AperTures)[cnt]);

		if (AperTure->Info4 == 0)
		{
			if (AperTure->AperTureCode < MinDCode)
			{
				Found = cnt;
				MinDCode = AperTure->AperTureCode;
			}
		}
	}

	return Found;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WriteApertureFile(int32 mode)
{
	typedef struct
	{
		uint8 n1[6];
		uint8 TypeAper, n2, DrillCode, n3;
		uint16 AperCode;
		uint8 n4[4];
		int32 breedte, hoogte;
		uint8 zeros[40];
	} NewAperTureRecord;

	AperTureRecord *AperTure;
	int32 cnt, fp, NrTempAperTures, LastInfo;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];

	if (GerberInfo.GerberOutputMode == 1)
		return;

	sprintf(str, "%s\\pcb\\gerber.txt", DesignPath);
	sprintf(str, "%s\\pcb\\gerber\\gerber.txt", DesignPath);


	//********************************************** nelze vytvoøit soubor *********************************************************
	if ((fp = FileOpenWriteUTF8(str)) <= 0)
	{
		sprintf(str2, SC(405, "Could not write to file\n\n%s"), str);
		MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return;
	}
	//******************************************************************************************************************************

	NrTempAperTures = 0;

	for (cnt = 0; cnt < NrAperTures; cnt++)
	{
		AperTure = &((*AperTures)[cnt]);

		if (AperTure->Used > 0)
			AperTure->Info4 = 0;
		else
			AperTure->Info4 = 1;
	}

	LastInfo = -1;

	while ((cnt = FindMinApertureDCode()) != -1)
	{
		AperTure = &((*AperTures)[cnt]);
		AperTure->Info4 = 1;

		if ((LastInfo != -1) && (LastInfo != AperTure->Info))
		{
//      WriteLn(fp,";");
		}

		switch (AperTure->Info)
		{
		case GERBER_TRACE:
			strcpy(str4, SC(443, "Round"));
			strcat(str4, "           ");
			str4[11] = 0;
			sprintf(str3, "D%-3i    %s%7.2f            0.0           %i", AperTure->AperTureCode, str4,
			        AperTure->x / 2540.0, AperTure->Used);

			break;

		case GERBER_PAD_ROUND:
			strcpy(str4, SC(443, "Round"));
			strcat(str4, "           ");
			str4[11] = 0;
			sprintf(str3, "D%-3i    %s%7.2f            0.0           %i", AperTure->AperTureCode, str4,
			        AperTure->x / 2540.0, AperTure->Used);
			break;

		case GERBER_PAD_RECT:
			strcpy(str4, SC(583, "Rectangle"));
			strcat(str4, "           ");
			str4[11] = 0;
			sprintf(str3, "D%-3i    %s%7.2f         %7.2f          %i", AperTure->AperTureCode, str4,
			        AperTure->x / 2540.0, AperTure->y / 2540.0, AperTure->Used);
			break;
		}

		LastInfo = AperTure->Info;
		NrTempAperTures++;
		WriteLn(fp, str3);
	}

	FileClose(fp);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindMinApertureDrillTool()
{
	int32 cnt, Found, MinDrillTool;
	AperTureRecord *DrillAperTure;

	MinDrillTool = 1000;
	Found = -1;

	for (cnt = 0; cnt < NrDrillAperTures; cnt++)
	{
		DrillAperTure = &((*DrillAperTures)[cnt]);

		if (DrillAperTure->Info4 == 0)
		{
			if (DrillAperTure->AperTureCode < MinDrillTool)
			{
				Found = cnt;
				MinDrillTool = DrillAperTure->AperTureCode;
			}
		}
	}

	return Found;
}

//*********************************************************************************************************************
//************************************* generování souboru "vrtání.txt" ve složce gerber ******************************
//*********************************************************************************************************************

void WriteDrillApertureFile(int32 fp, int32 mode)
{
	typedef struct
	{
		uint8 n1[6];
		uint8 breedte[4];
		uint8 n2[2];
		uint8 zeros[10];
	} NewDrillAperTureRecord;

	AperTureRecord *DrillAperTure;
	NewDrillAperTureRecord NewDrillAperTures[128];
	uint8 zeros[8];
	int32 cnt, NrTempDrillAperTures, breedte2;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], NumberString[MAX_LENGTH_STRING];
	char FileName[MAX_LENGTH_STRING];

	memset(&zeros, 0, 8);
	NrTempDrillAperTures = 0;

	for (cnt = 0; cnt < NrDrillAperTures; cnt++)
	{
		DrillAperTure = &((*DrillAperTures)[cnt]);

		if (DrillAperTure->Used > 0)
		{
			memset(&NewDrillAperTures[NrTempDrillAperTures], 0, sizeof(NewDrillAperTureRecord));
			NewDrillAperTures[NrTempDrillAperTures].n2[0] = 1;
			NewDrillAperTures[NrTempDrillAperTures].n2[1] = 0x0c;
			breedte2 = (int32) (DrillAperTure->x * 10);
			memmove(NewDrillAperTures[NrTempDrillAperTures].breedte, &breedte2, 4);
			NrTempDrillAperTures++;
		}
	}

	NewDrillAperTures[0].n1[0] = 0x74;
	NewDrillAperTures[0].n1[1] = 2;
	NewDrillAperTures[0].n1[2] = (uint8) NrTempDrillAperTures;
	NewDrillAperTures[0].n1[4] = 1;
	NewDrillAperTures[0].n1[5] = 1;
	/*
	  sprintf(str,"%s\\gerber\\drill.rck",DesignPath);
	  if ((fp=FileOpenWrite(str))<=0) {
	    return;
	  }
	  res=sizeof(NewDrillAperTureRecord);
	  for (cnt=0;cnt<NrTempDrillAperTures;cnt++) {
	    FileWrite(fp,&NewDrillAperTures[cnt],22,&res);
	  }
	  FileWrite(fp,&zeros,6,&res);
	  FileClose(fp);
	*/

	if (mode == 0)
	{
		strcpy(str, EditFile);
		CutExtensionFileName(str);
		GetFilePartFromFileName(FileName, str);
		//sprintf(str, "%s\\pcb\\gerber\\", DesignPath); //pùvodní název souboru
		//strcat(str, "Drills");
		//strcat(str, ".txt");

		sprintf(str, "%s\\pcb\\gerber\\DrillTools.txt", DesignPath); //nový název souboru

		//********************************************** nelze vytvoøit soubor *****************************************************
		if ((fp = FileOpenWriteUTF8(str)) <= 0)
		{
			sprintf(str2, SC(405, "Could not write to file\n\n%s"), str);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
			return;
		}
		//***************************************************************************************************************************

		sprintf(str2, SC(986, "Drill tools [ %s ]"), FileName);
		WriteLn(fp, str2);
		WriteLn(fp, ""); //pùvodnì bylo všude jakoby ignorovat øádek ";"
		strcpy(str2, SC(987, "Tool nr   Type      Diam mm     thou        Nr drills"));
		WriteLn(fp, str2);
		WriteLn(fp, "-------------------------------------------------------");
	}

	for (cnt = 0; cnt < NrDrillAperTures; cnt++)
	{
		DrillAperTure = &((*DrillAperTures)[cnt]);

		if (DrillAperTure->Used > 0)
			DrillAperTure->Info4 = 0;
		else
			DrillAperTure->Info4 = 1;
	}

	while ((cnt = FindMinApertureDrillTool()) != -1)
	{

//      while ((cnt=FindMinApertureDrill())!=-1) {

		DrillAperTure = &((*DrillAperTures)[cnt]);
		DrillAperTure->Info4 = 1;

		if (mode == 0)
		{
			switch (DrillAperTure->Info)
			{
			case GERBER_DRILL_PLATED:
				strcpy(str3, SC(988, "Plated"));
				break;

			case GERBER_DRILL_UNPLATED:
				strcpy(str3, SC(989, "Unplated"));
				break;
			}

			strcat(str3, "           ");
			str3[12] = 0;
			sprintf(str2, " T%-3i       %s%6.3f   %6.2f       %5i", DrillAperTure->AperTureCode, str3,
			        (DrillAperTure->x + 500) / 100000.0, DrillAperTure->x / 2540.0, DrillAperTure->Used);
			WriteLn(fp, str2);
		}
		else
		{
			if ((GerberInfo.GerberNumberMode & 8) == 0)
			{	// INCH
				sprintf(NumberString, "%.6f", DrillAperTure->x / 2540000.0);
			}
			else
				sprintf(NumberString, "%.3f", DrillAperTure->x / 100000.0);

			if (NumberString[0] == '0')
				sprintf(str2, "T%dC%s", DrillAperTure->AperTureCode, (LPSTR) & NumberString[1]);
			else
				sprintf(str2, "T%dC%s", DrillAperTure->AperTureCode, NumberString);

			WriteLn(fp, str2);
		}
	}

	if (mode == 0)
		FileClose(fp);
	else
		WriteLn(fp, "%");
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
