/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: move2.h
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


#ifndef _MOVE2

#define _MOVE2

#include "types.h"

void MoveSelectedSpecialObjects(int32 Mode, int32 Count);

void PlaceRotatedSpecialObjects(int32 Mode);

void ScaleSelectedSpecialObjects(int32 mode);

void MoveSelectedRefs(int32 Mode, int32 Count);

void MoveSelectedCompValues(int32 Mode, int32 Count);

void PlaceRotatedComponentReferences(double Rotation, int32 mode);

void PlaceRotatedComponentValues(double Rotation, int32 mode);

void ChangeVisibilityCompRefsValues(int32 Mode);

void ChangeMirrorCompRefsValues(int32 Mode);

void ChangeTextHeightCompRefsValues(int32 Mode);

void ChangeLineWidthCompRefsValues(int32 Mode);

void ChangeCompValue(void);

void ChangeText(int32 Mode);

void ChangeTextHeight(int32 Mode);

void ChangeLineWidth(int32 Mode);

void ChangeCircleDiameter(int32 mode);

void ChangeRectangle(int32 mode);

void ChangeArc(int32 mode);

int32 AssignSelectedSpecialObjectsToNet(int32 mode);

int32 EditSchematicComponent(int32 mode);

int32 SearchForComponents(LPSTR SearchString, int32 mode);

int32 ChangeNetAreaFills(int32 mode);

int32 ChangeNetTracesVias(int32 mode);

#endif
