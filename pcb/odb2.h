/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: odb2.h
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


#ifndef _ODB2_H

#define _ODB2_H

#include "types.h"

#define  ODB_TRACE                    100
#define  ODB_PAD_ROUND                200
#define  ODB_PAD_RECT                 400
#define  ODB_PAD_BUTTERFLY            600
#define  ODB_PAD_THERMAL_RELIEF       650
#define  ODB_PAD_POLYGON              700
#define  ODB_DRILL_PLATED             100
#define  ODB_DRILL                    110
#define  ODB_DRILL_UNPLATED           200

typedef struct
{
	int32 AperTureNr, AperTureCode, Info, Info2, Info3, Info4, Used, Used2, Mirror, SpecialType;
	double x, y, x2, y2, x3, Rotation, HoleSize;
	uint8 *Address;
	char Name[128];
	char NewName[128];
} ApertureMacroObjectObjectRecord;

int32 OdbPrepareTraces(int32 Layer, int32 mode);

int32 OdbPrepareVias(int32 Layer, int32 mode);

int32 OdbPrepareComponentPins(int32 Layer, int32 mode);

int32 OdbPrepareComponentPinsViasSolderPasteObjects(int32 Layer, int32 mode);

int32 OdbPrepareComponentInfoObjects(int32 Layer, int32 mode);

int32 OdbPrepareComponentInfoObjects(int32 Layer, int32 mode);

int32 OdbPrepareComponentOutlineObjects(int32 Layer, int32 CompOutlineLayers, int32 mode);

int32 OdbPrepareComponentPlacementOutlineObjects(int32 Layer, int32 mode);

int32 OdbPrepareComponentBoardOutlineObjects(int32 Layer, int32 mode);

int32 OdbPrepareComponentRoutingKeepoutObjects(int32 Layer, int32 mode);

int32 OdbPrepareComponentSilkScreenObjects(int32 Layer, int32 mode);

int32 OdbPrepareSpecialObjects(int32 Layer, int32 mode);

int32 PlotAreaFillToOdb(AreaFillRecord * AreaFill);

int32 PlotPolygonToOdb(PolygonRecord * DrawPolygon, int32 mode);

int32 PowerPlaneToOdb(int32 Layer);

int32 DrawOdbStr(double x, double y, double Size, int32 ApertureNr, double Rotation, int32 Alignment, int32 Mirror,
                 char *str);

int32 ExportSpecialOdbSymbols(LPSTR OdbDir, int32 mode);

#endif
