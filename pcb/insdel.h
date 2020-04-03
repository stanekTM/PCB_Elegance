/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: insdel.h
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



#ifndef _INSDEL

#define _INSDEL

#include "types.h"

int32 SelectNetsObjectsSelected(uint8 * SelectedNets, int32 NrNets);

int32 ZeroUnusedObjects(int32 mode);

void DeleteObjectsSelected(void);

int32 DeletePolygonsInAreaFillPowerPlane(AreaFillRecord * AreaFill);

int32 AddTrace(ObjectRecord * InsertTrace);

int32 AddConnection(ConnectionsRecord * Connection);

int32 AddVia(ViaRecord * Via);

int32 AddComp(CompRecord * Comp);

void CheckAreafill(AreaFillRecord * NewAreaFill);

int32 AddAreaFill(int32 mode);

int32 MakeRoomHorTraces(int32 Layer);

int32 MakeRoomVerTraces(int32 Layer);

int32 MakeRoomDiag1Traces(int32 Layer);

int32 MakeRoomDiag2Traces(int32 Layer);

int32 MakeRoomConnections(int32 NrExtraConnections);

int32 MakeRoomVias(int32 NrExtraVias);

int32 MakeRoomComps(int32 CompLength);

int32 MakeRoomAreaFills(int32 AreaFillLength);

void SearchCommonObjectsInObjects2AndObjects4(void);

void DeleteTracesViasSelected(void);

void UpdateInsDelInfo(void);

int32 AddObjectLine(ObjectLineRecord * ObjectLine);

int32 AddObjectRect(ObjectRectRecord * ObjectRect);

int32 AddObjectCircle(ObjectCircleRecord * ObjectCircle);

int32 AddObjectArc(ObjectArcRecord * ObjectArc);

int32 AddObjectText(ObjectTextRecord * ObjectText);

int32 AddObjectText2(ObjectTextRecord2 * ObjectText2);

int32 AddObjectPolygon(ObjectPolygonRecord * ObjectPolygon);

void UndoObjects(void);

void RedoObjects(void);

void ZeroObjects(void);

int32 CalcMemory(int32 mode);

int32 RemoveDesignLayer(int32 Layer);

int32 AddDesignLayer(int32 Layer);

int32 SwitchDesignLayer(int32 Layer1, int32 Layer2);

int32 MoveCopyObjectsLayers(int32 SourceLayer, int32 DestinationLayer, int32 Action, int32 mode);

int32 AreaFillToText(AreaFillRecord * AreaFill, int32 mode);

#endif
