/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: polygon.h
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


#ifndef _POLYGON

#define _POLYGON

#include "owntypes.h"

int32 SetMinMaxObjectPolygon(ObjectPolygonRecord * ObjectPolygon, int32 mode);

int32 MemSizeObjectPolygon(ObjectPolygonRecord * ObjectPolygon);

int32 MemSizePolygon(PolygonRecord * ObjectPolygon);

void CopyPolygonToPolygon(PolygonRecord * SrcPolygon, PolygonRecord * DestPolygon);

void MoveObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OffsetX, double OffsetY, int32 mode);

int32 CheckNoCrossesInPolygon(PolygonRecord * Polygon);

int32 PointInObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double px, double py);

int32 CheckPolygonCompleetlyInsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon);

int32 CheckPolygonCompleetlyOutsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon);

int32 CheckPolygonCrossesOtherPolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon);

int32 ObjectPolygonInSearchArea(ObjectPolygonRecord * ObjectPolygon);

int32 PointInPolygon(PolygonRecord * Polygon, double px, double py);

int32 CheckMappableObjectPolygon(int32 ObjectPolygonNr, int32 mode);

int32 ViewVerticesObjectPolygon(int32 mode);

int32 MakeBiggerSmallerObjectPolygon(ObjectPolygonRecord * Polygon, ObjectPolygonRecord * BiggerPolygon,
                                     double Thickness, int32 mode);

void MakePolygonFromPlotObject(ObjectRecord * Object, PolygonRecord * PolygonObject, double Clearance,
                               int32 CircleRoundings, int32 mode);

int32 SetMinMaxPolygon(PolygonRecord * PolygonObject, int32 mode);

int32 DeleteFromObjectPolygon(int32 mode);

int32 MergeObjectsToPolygon(int32 mode);

#endif
