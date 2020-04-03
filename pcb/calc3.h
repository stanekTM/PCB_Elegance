/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc3.h
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


#ifndef _CALC3

#define _CALC3

#include "types.h"

int32 CheckObjectOverlapped(ObjectRecord * Object, int32 mode);

int32 CheckObjectOverlappedFromObjects(ObjectRecord * Object, int32 mode);

int32 ObjectIsTrace(ObjectRecord * Object);

int32 PointConnectedToCenterObject(double x, double y, ObjectRecord * Object);

int32 GetPowerPlaneByLayer(int32 Layer);

int32 IsLayerPowerPlane(int32 Layer);

int32 GetLayerText(int32 Layer, LPSTR TextStr, int32 mode);

int32 GetLayerTextObjects(int32 LayerObjectNr, LPSTR LayerText, int32 mode);

int32 CheckLineCrossesWithAreaFill(double x1, double y1, double x2, double y2, AreaFillRecord * AreaFill, double *NewX1,
                                   double *NewY1, double *NewX2, double *NewY2, double Thickness, int32 mode);

int32 CheckLineCrossesCircle(double x1, double y1, double x2, double y2, double cx, double cy, double ThickNess);

int32 CheckFillingLine(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4,
                       double *NewX, double *NewY, double ThickNess);

int32 CopyThermalReliefInPolygon(ObjectRecord * Object, PolygonRecord * Polygon, double ThickNess, double Pos,
                                 int32 MaxPos, int32 mode);

int32 CheckPointInAreaFill(double x, double y, AreaFillRecord * AreaFill);

int32 ComplexAreaFill(AreaFillRecord * AreaFill);

int32 RectTestObject(ObjectRecord * Object, int32 mode);

void FillPositionObject(ObjectRecord * Object);

void FillPositionObjectWithClearance(ObjectRecord * Object, double Clearance);

void MakePolygonFromSpecialLine(double x1, double y1, double x2, double y2, ObjectPolygonRecord * ObjectPolygon,
                                double Thickness1, double Thickness2, int32 mode);

int32 MakeObjectFromCompRef(CompRecord * Comp, ObjectRecord * Object, int32 Mode);

int32 MakeObjectFromCompValue(CompRecord * Comp, ObjectRecord * Object, int32 Mode);

int32 GetArcAngle(ObjectRecord * Object, double *Angle1, double *Angle2);

int32 GetCoordinatesObjectTrace(ObjectRecord * Object, double *x1, double *y1, double *x2, double *y2);

int32 AdjustOffsetForSnapOnGerberObject(double CursorX, double CursorY, double GridX, double GridY, double divx,
                                        double divy, double *ShiftOffsetX, double *ShiftOffsetY, int32 mode);

int32 CheckIfCopperObjectArc(ObjectArcRecord * ObjectArc, int32 mode);

int32 CreateTraceObjectFromSpecialLine(ObjectRecord * TraceObject, double StartX, double StartY, double Length,
                                       double Angle, double TraceWidth, int32 mode);

int32 CreateViaObjectFromVia(ViaRecord * Via, ObjectRecord * ViaObject, int32 mode);

int32 CreateArcObjectFromArc(ObjectArcRecord * ObjectArc, ObjectRecord * ArcObject, int32 mode);

int32 AdjustOffsetForSnapOnObject(ObjectRecord * Object, double CursorX, double CursorY, double GridX, double GridY,
                                  double divx, double divy, double *ShiftOffsetX, double *ShiftOffsetY, int32 mode);

void CreateTraceObjectFromTrace(TraceRecord * Trace, ObjectRecord * TraceObject, int32 ObjectType, int32 Layer,
                                int32 TraceNr, int32 mode);

int32 GetDrawingLayer(int32 mode);

int32 GetCompProperties(CompRecord * Comp, LPSTR PropertyID, LPSTR PropertyValue, int32 mode);

int32 GetNetProperties(NetRecord * Net, LPSTR PropertyID, LPSTR PropertyValue, int32 mode);

#endif
