/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: plot.h
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


#ifndef _PLOT

#define _PLOT

#include "types.h"

#define  DIFF_GERBER(x,y) (SQR(LastGerberX-x)+SQR(LastGerberY-y))
#define  GERBER_TRACE                    100
#define  GERBER_PAD_ROUND                200
#define  GERBER_PAD_RECT                 400
#define  GERBER_PAD_BUTTERFLY            600
#define  GERBER_PAD_THERMAL_RELIEF       650
#define  GERBER_PAD_POLYGON              700
#define  GERBER_DRILL_PLATED             100
#define  GERBER_DRILL_UNPLATED           200


int32 AllocateMemAperTures(int32 count);

void DeleteAperTures(void);

void WriteApertureFile(int32 mode);

int32 CheckTraceAperTure(double ThickNess);

void DeAllocateMemAperTures(void);

int32 CheckPlatedDrillAperTure(double ThickNess);

int32 CheckUnPlatedDrillAperTure(double ThickNess);

int32 CheckRoundPadAperTure(double ThickNess);

int32 CheckRectPadAperTure(double x, double y);

int32 CheckThermalReliefAperTure(double InsideDiameter, double OutsideDiameter, double CrossHairThickness);

int32 CheckPolygonAperTure(int32 ObjectType, double x2, double y2, double Rotation, int32 Mirror, uint8 * Address);

int32 TraceGerberOutput(int32 Layer);

int32 TracePenPlotOutput(int32 Layer, int32 NrTraceObjects);

int32 ViaGerberOutput(int32 Layer);

int32 CompPinGerberOutput(int32 Layer);

int32 PasteSoldMaskGerberOutput(int32 Layer);

int32 ObjectsGerberOutput(int32 Layer, int32 ObjectStartNr);

int32 WriteGerberText(int32 Layer, int32 mode);

int32 DrillOutput(void);

int32 CompPositionOutput(int32 mode);

void WriteComponents(int Filefp);

void WriteLayerInfo(void);

int32 WriteNeutralFile(int32 mode);

int32 ExcellonDrillOutput(int32 Drillfp);

#endif
