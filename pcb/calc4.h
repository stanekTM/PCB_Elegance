/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc4.h
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


#ifndef _CALC4

#define _CALC4

#include "types.h"

#define LineCrosses(LineX1,LineY1,LineX2,LineY2,LineX3,LineY3,LineX4,LineY4)   \
  (                                                                            \
     ((min(LineX1,LineX2)>max(LineX3,LineX4))                                  \
     ||                                                                        \
     (min(LineY1,LineY2)>max(LineY3,LineY4))                                   \
     ||                                                                        \
     (max(LineX1,LineX2)<min(LineX3,LineX4))                                   \
     ||                                                                        \
     (max(LineY1,LineY2)<min(LineY3,LineY4))) ?                                \
     (-1):(LineCrossesNew(LineX1,LineY1,LineX2,LineY2,                         \
                          LineX3,LineY3,LineX4,LineY4))                        \
  )


#define  InRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (1) : (0) )


int32 PointInPolygon(PolygonRecord * Polygon, double px, double py);

int32 PolygonInSearchArea(PolygonRecord * Polygon);

int32 ObjectPolygonInSearchArea(ObjectPolygonRecord * ObjectPolygon);

int32 PointInObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double px, double py);

int32 RectangleInPolyLine(PolygonRecord * Polygon, double rxmin, double rymin, double rxmax, double rymax);

int32 LineCrossesNew(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                     double LineX4, double LineY4);

int32 LineCrosses2(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                   double LineX4, double LineY4, double *CenterX, double *CenterY, int32 mode);

void MakePolygonFromObject(ObjectRecord * Object, PolygonRecord * PolygonObject, double Clearance, double Pos,
                           int32 MaxPos, int32 mode);

int32 MemSizePolygon(PolygonRecord * Polygon);

int32 MemSizeObjectPolygon(ObjectPolygonRecord * ObjectPolygon);

void CopyPolygonToPolygon(PolygonRecord * SrcPolygon, PolygonRecord * DestPolygon);

void DeleteVertices(int32 Start, int32 Count, PointsArray * Points, int32 NrVertices);

void InsertVertice(int32 ResultStart, PolygonRecord * ResultPolygon, PointsArray * InsertPoints);

void AppendVertices(int32 InsertStart, int32 Count, int32 NrVerticesInsert, PointRecord * InsertPoints,
                    PointRecord * ResultPoints, int32 mode);

int32 SetMinMaxPolygon(PolygonRecord * PolygonObject, int32 mode);

int32 SetMinMaxObjectPolygon(ObjectPolygonRecord * ObjectPolygon, int32 mode);

int32 CheckPolygonInsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon);

int32 CheckPolygonCompleetlyInsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon);

int32 CheckPolygonCompleetlyOutsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon);

int32 CheckPolygonOverlapAreaFill(PolygonRecord * PolygonObject, AreaFillRecord * AreaFill);

int32 CheckPolygonInsideAreaFill(PolygonRecord * PolygonObject, AreaFillRecord * AreaFill, int32 mode);

int32 CheckNoCrossesInPolygon(PolygonRecord * Polygon);

int32 CheckNoCrossesInObjectPolygon(ObjectPolygonRecord * ObjectPolygon);

int32 ReorganizePolygon(PolygonRecord * Polygon, PolygonRecord * ResultingPolygon, double TrimPointsDistance);

int32 CheckDeletionsAreaFill(int32 mode);

int32 MakeBiggerSmallerPolygon(PolygonRecord * Polygon, PolygonRecord * BiggerPolygon, double Thickness, int32 mode);

int32 MakeBiggerSmallerPolygon2(PolygonRecord * Polygon, PolygonRecord * BiggerPolygon, double Thickness, int32 mode);


void RotatePolygon(PolygonRecord * Polygon, double CentreX, double CentreY, double Rotation, int32 mode);

void MirrorPolygon(PolygonRecord * Polygon, int32 mode);

int32 CheckPolygon(PolygonRecord * Polygon);

int32 CheckAreaFillsConnected(AreaFillRecord * AreaFill, AreaFillRecord * AreaFill2);

int32 GetBoardOutlineAreaFill(AreaFillRecord * BoardOutlineAreaFill, double KeepOut, int32 mode);

void GetArcEndPoints(ObjectRecord * Object, double *px1, double *py1, double *px2, double *py2, int32 mode);

void GetArcEndPoints2(ObjectArcRecord * ObjectArc, double *px1, double *py1, double *px2, double *py2, int32 mode);

int32 CheckObjectIsBigPolygon(ObjectRecord * Object);

int32 CheckObjectIsPolygonWithOpenSpots(ObjectRecord * Object);

int32 GetAreaFillFromBigPolygonObject(ObjectRecord * Object, AreaFillRecord ** AreaFill, double Clearance, int32 mode);

int32 GetFirstPointPolygonObject(ObjectRecord * Object, double *x, double *y, int32 mode);

int32 GetPolygonDirection(PolygonRecord * PolygonObject);

#endif
