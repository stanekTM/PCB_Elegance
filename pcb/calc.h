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
#include "types.h"

int32 FindMinMaxBoard(double *MinX, double *MinY, double *MaxX, double *MaxY, int32 mode);

void SetBoardPosComp(CompRecord * Comp, int32 mode);

void SetBoardPosComps(void);

int32 MemSizeComp(CompRecord * Comp);

void ShapeCompOutLineToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation);

void ShapeCompSilkScreenToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation);

void ShapePlacementOutLineToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation);

void ShapeOtherToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation, int32 SelectedLayer,
                        int32 mode);

void ShapePinsToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation, int32 ObjectArrayNr,
                       int32 SelectedLayer, int32 mode);

int32 CompPinText(CompRecord * Comp, int32 PinNrToFind, double x, double y, LPSTR PinText);

int32 CompPinNr(CompRecord * Comp, LPSTR PinText);

int32 GetShapeCompPinNrByPinText(ShapeRecord * Shape, LPSTR PinText);

double DistancePointToConnection(double px, double py, double ConnectionX1, double ConnectionY1, double ConnectionX2,
                                 double ConnectionY2);

double DistancePointToTrace(double px, double py, double TraceX1, double TraceY1, double TraceX2, double TraceY2);

double DistancePointToArc(double px, double py, double x1, double y1, double x2, double y2, double x3, double y3,
                          double x4, double y4);

void CalcDirection(double x1, double y1, double x2, double y2);

int32 GetDirection(double x1, double y1, double x2, double y2, int32 mode);

int32 GetNewDirection(double x1, double y1, double x2, double y2, int32 mode);

void GetMinMaxText(double X, double Y, double FontSize, int32 FontNr, int32 Rotation, int32 Alignment, int32 Mirror,
                   LPSTR Str);

void GetMinMaxText2(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                    LPSTR Str);

void GetMinMaxText2a(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                     LPSTR Str);

void GetMinMaxText2b(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                     WCHAR * Str);

int32 CheckObjectCount(int32 MaxObjectCount, int32 mode, int32 * NrPinsUsed);

int32 RecalcBoardSize(int32 mode);

int32 GetTextMinMaxCompReference(CompRecord * Comp);

int32 GetTextMinMaxCompValue(CompRecord * Comp);

int32 OkToDrawObjectsLayer(int32 Layer);

int32 CircleToLineSegments(double x1, double y1, double Thickness, int32 CircleMode, double *LineSegments);

int32 ArcToLineSegments(double x1, double y1, double Width, double Height, double x2a, double y2a, double x2b,
                        double y2b, double *LineSegments, int32 mode);

int32 DimensionToLineSegments(double x1, double y1, double x2, double y2, double *LineSegments, int32 mode);

int32 GetDimensionTextFromLine(double x1, double y1, double x2, double y2, ObjectTextRecord2 * ObjectText2, int32 mode);

int32 TextStringToLineSegments(double x, double y, double Size, int32 Rotation, int32 Alignment, int32 Mirror,
                               LPSTR str, double *LineSegments);

int32 TextStringToLineSegments2(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror,
                                LPSTR str, double *LineSegments);


int32 CheckObjectArcIsCircle(ObjectArcRecord * ObjectArc, int32 mode);

void PrintObjectInText(ObjectRecord * Object);

int32 RectTestText2(double x1, double y1, double x2, double Rotation, int32 Mirror, LPSTR Text);

int32 RectTestText2a(double x1, double y1, double x2, double Rotation, int32 Mirror, LPSTR Text);

int32 RectTestText2b(double x1, double y1, double x2, double Rotation, int32 Mirror, int32 FontNr, WCHAR * Text);

int32 RectTestArc2(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4,
                   double Thickness);

int32 RectTestPolygon(PolygonRecord * PolygonObject);

int32 GetComponentPinLayer(CompRecord * Comp);

int32 ConvertObjectTextToStrings(LPSTR TextP, int32 FontNr, int32 * MaxCountX, int32 Layer);

int32 GetPinTextFromObject(ObjectRecord * Object, LPSTR NetText, LPSTR PinText, LPSTR LayerText, LPSTR ClearanceText);

#endif
