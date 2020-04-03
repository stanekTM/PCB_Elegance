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

void FindMinMaxDesign(double *MinX, double *MinY, double *MaxX, double *MaxY);

void GetMinMaxText(double X, double Y, double FontSize, int32 FontNr, int32 Rotation, int32 Alignment, LPSTR Str);

void GetMinMaxText2(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                    LPSTR Str);

void InstanceToObject(InstanceRecord * Instance, double OffsetX, double OffsetY, int32 Mode);

void InstancePinsToObject(InstanceRecord * Instance, double OffsetX, double OffsetY, int32 Mode);

void FillPositionObject(ObjectRecord * Object);

void FillPositionObjects(void);

void SetBoardPosInstances(void);

int32 SetBoardPosInstance(int32 InstanceNr);

void ViewWholeDesign(int32 mode);

int32 TestLineConnectedToCircle(double x1, double y1, double x2, double y2, double CircleX, double CircleY,
                                double CircleDiam);

int32 TestLinesConnected(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4,
                         int32 mode);


int32 GetMinMaxInstanceValueText(InstanceRecord * Instance);

int32 GetMinMaxInstanceReferenceText(InstanceRecord * Instance);

int32 RectTestLine2(double LineX1, double LineY1, double LineX2, double LineY2);

int32 LinesOverlap(double Line1X1, double Line1Y1, double Line1X2, double Line1Y2, double Line2X1, double Line2Y1,
                   double Line2X2, double Line2Y2);

int32 ConvertPinBusStr(ObjectRecord * PinBusObject, LPSTR NewPinBusStr, int32 mode);

void ConvertPointToPolar(double x, double y, double *Distance, double *Angle);

double DistancePointToLine(double px, double py, double LineX1, double LineY1, double LineX2, double LineY2);

int32 GetNetLabelIndexFromEndPointLine(double x1, double y1);

int32 GetDimensionTextFromLine(double x1, double y1, double x2, double y2, ObjectTextRecord * ObjectText, int32 mode);

int32 DimensionToLineSegments(double x1, double y1, double x2, double y2, double *LineSegments, int32 mode);

int32 GetNrCrossingFromWirePoint(double x, double y);

#endif
