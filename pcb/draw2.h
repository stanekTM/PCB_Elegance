/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw2.h
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


#ifndef _DRAW2

#define _DRAW2

#include "types.h"
#include "owntypes.h"


void DrawObjectsWithColorMagenta(int32 Mode);

void DrawObjects2WithColorMagenta(int32 Mode);

void DrawSpecialObject(ObjectRecord * Object, int32 Number, int32 Mode);

void ReDisplayNet(int32 mode);

void DrawObjects(int32 Mode);

void DrawObject2(Polygon8Record * Object, int32 Mode);

void DrawObject(ObjectRecord * Object, int32 Mode);

void DrawDrillObject(ObjectRecord * Object, int32 Mode);

void DrawCrossObjects(int32 Repaint, int32 Mode);

void DrawCrossObjectsClear(int32 Repaint, int32 Mode);

void DrawOutlineComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode);

void DrawSilkScreenComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode);

void DrawReferenceComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode);

void DrawPlacementOutlineComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode);

void DrawValueComp(CompRecord * Comp, double OX, double OY, double Rotation, int32 Mode);

void DrawPinsComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode);

void DrawDrillsComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode);

void DrawDrillsComps(int32 Mode);

void DrawTextComps(int32 Mode);

void DrawPinsComps(int32 Mode);

void DrawComp(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode);

void DrawComp2(CompRecord * Comp, double OffsetX, double OffsetY, double Rotation, int32 Mode);

void DrawComps(int32 Mode);

void DrawOutlineComps(int32 Mode);

void DrawSilkScreenComps(int32 Mode);

void DrawPlacementOutlineComps(int32 Mode);

void DrawReferenceComps(int32 Mode);

void DrawValueComps(int32 Mode);

void DrawGrid(void);

void DrawCurrentWorkingTraceOn(int32 mode);

void DrawCurrentWorkingTraceOff(int32 mode);

void DrawErrorObjects(void);

int32 CommandSelectPoint(double *OX, double *OY, double MinX, double MinY, double MaxX, double MaxY, int32 Mode);

#endif
