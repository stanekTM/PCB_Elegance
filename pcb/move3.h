/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: move3.h
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


#ifndef _MOVE3

#define _MOVE3

#include "types.h"

void ChangePCB(int32 mode);

void RotatePCB(double Rotation);

int32 MirrorText(int32 mode);

void ChangeGeometryComp(int32 Mode);

void AddSpecialObjectsFromAsciiFile(int32 Layer, int32 mode);

void PutSpecialObjectsInAsciiFile(int32 Layer, int32 mode);

int32 DrawSelectedAreafills(double CurrentX, double CurrentY, double CurrentX2, double CurrentY2,
                            double CentreSelectedX, double CentreSelectedY, double Rotation, int32 mode);

int32 CopyArrayObjects(int32 mode);

int32 MoveSelectedAreafill(int32 mode);

int32 MoveStretchedAreafill(int32 mode);

void MirrorObjects(int32 mode);

void AreafillToObjectLines(AreaFillRecord * AreaFill, int32 ObjectLayer1, PolygonRecord * TestPolygon,
                           int32 ObjectLayer2, int32 mode);

void PolygonToObjectLines(PolygonRecord * DrawPolygon, int32 ObjectLayer1, PolygonRecord * TestPolygon,
                          int32 ObjectLayer2, int32 mode);

void PolygonVerticesToMessage(PolygonRecord * DrawPolygon, PolygonRecord * DrawPolygon2);

void CopyAreafillStartPolygonToInfo4(int32 mode);

#endif
