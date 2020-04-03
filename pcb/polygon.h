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

void DrawTestPolygon(PolygonRecord * Polygon, int32 mode);

void DrawTestPolygon3(PolygonRecord * Polygon, int32 mode);

void DrawTestPolygon4(PolygonRecord * Polygon, double dikte, int32 mode);

void DrawFilledPolygon(PolygonRecord * FilledPolygon, int32 mode);

void DrawOwnPolygon(double *PolygonPoints, int32 count, int32 mode);

void DrawOwnPolygonFloat(float *PolygonPoints, int32 count, int32 mode);

double GetNewRandomValue(int32 mode);

void DrawAreaFillStartPolygon(AreaFillRecord * Areafill, int32 mode);

int32 CommandAddPolygonLines(double LineThickNess, int32 Layer, int32 Mode, int32 ObjectType);

int32 CheckPolyLinePoint(PointsArray * Points, int32 NrVertices, double px, double py);

void SinglePolygonFromNet(int32 NetNr);

void CommandCutFromAreaFill(int32 ObjectType, int32 mode);

void CommandCutFromPolygon(int32 ObjectType);

int32 AddNewAreaFill(int32 Layer, int32 mode);

int32 MergePolygon(PolygonRecord * PolygonObject, int32 Mode);

void InsertAreaFillPowerPlanes(void);

int32 CheckIfLayerIsAPowerPlane(int32 Layer);

int32 InsertObjectInAreaFill(ObjectRecord * Object, int32 Layer, int32 NetNr, int32 mode);

int32 GetAreaFillToRebuild(int32 mode);

void GetPowerPlaneToCutFrom(int32 Layer, double ThickNess, int32 ObjectType, int32 mode);

int32 GetPowerPlaneToRemoveDeletions(int32 Layer);

int32 CheckObjectInsideAreaFillPowerPlane(ObjectRecord * Object);

int32 GetAreaFillNrPowerPlane(int32 Layer);

void AddPowerPlane(int32 Layer);

void RemovePowerPlane(int32 Layer);

int32 RemoveDeletionsAreaFill(int32 AreaFillNr, int32 mode);

int32 ChangeAreaFillPowerPlane(int32 Layer);

int32 ChangeAreaFill(int32 mode);

int32 MergeAreafills(int32 mode);

int32 ViewVerticesAreaFill(int32 mode);

int32 ViewVerticesObjectPolygon(int32 mode);

int32 PlaceMovedAreafill(int32 AreafillNr, double CurrentX, double CurrentY, double CurrentX2, double CurrentY2,
                         double CentreSelectedX, double CentreSelectedY, double Rotation, int32 mode);

int32 PlaceMovedAreafills(double CurrentX, double CurrentY, double CurrentX2, double CurrentY2, double CentreSelectedX,
                          double CentreSelectedY, double Rotation, uint8 * SelectedNetsForMove, int32 RepaintMode);

int32 PlaceStretchedAreafill(int32 AreafillNr, double CurrentX, double CurrentY, int32 mode);

int32 ChangeClearanceAreaFill(double Clearance, int32 mode);

void CommandAddObjectPolygon(double LineThickNess, int32 Layer, int32 Mode);

void MovePolygon(PolygonRecord * Polygon, double OffsetX, double OffsetY, int32 mode);

void MoveObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OffsetX, double OffsetY, int32 mode);

void RotateObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double CentreX, double CentreY, double Rotation,
                         int32 mode);

void MirrorAreafill(int32 mode);

int32 CommandAddToAreaFill(int32 mode);

#endif
