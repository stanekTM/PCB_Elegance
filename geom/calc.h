/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc.h
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


#ifndef _CALC

#define _CALC

#include "owntypes.h"

int32 ScanParameters(int32 NrParameters, LPSTR Str, int32 mode);

void GetMinMaxText(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                   LPSTR Str);

int32 GetDirection(double x1, double y1, double x2, double y2, ObjectRecord * TraceObject);

int32 RectTestLine2(double LineX1, double LineY1, double LineX2, double LineY2, double Thickness);

int32 CheckNoCrossesInObjectPolygon(ObjectPolygonRecord * ObjectPolygon);

void SetCursorFromRealPosition(double x, double y);

void GetArcEndPoints(ObjectRecord * Object, double *px1, double *py1, double *px2, double *py2, int32 mode);

int32 RectTestArc2(ObjectRecord * Object);

int32 RectTestCircle2(ObjectRecord * Object);

int32 RectTestText2(double x1, double y1, double x2, double Rotation, int32 Mirror, LPSTR Text);

void MakePolygonFromSpecialLine(double x1, double y1, double x2, double y2, ObjectPolygonRecord * ObjectPolygon,
                                double Thickness1, double Thickness2, int32 mode);

int32 TextStringToLineSegments(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror,
                               LPSTR str, double *LineSegments);

int32 ObjectToLineSegments(ObjectRecord * Object, double *LineSegments, int32 mode);

int32 CalcDistanceBetweenObjects(int32 mode);

int32 GetArcAngle(ObjectRecord * Object, double *Angle1, double *Angle2);

int32 DimensionToLineSegments(double x1, double y1, double x2, double y2, double *LineSegments, int32 mode);

int32 GetDimensionTextFromLine(double x1, double y1, double x2, double y2, ObjectRecord * ObjectText2, int32 mode);

#endif
