/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select3.h
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


#ifndef _SELECT3

#define _SELECT3


#include "types.h"

int32 CopyTracesFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyViasFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyAreafillsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyConnectionsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyCompObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyCompBigPolygonObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyOtherObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyCopperObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

int32 CopyAllObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode);

void UnhiliteTraces(void);

void UnhiliteVias(void);

void UnhiliteComps(void);

void UnhiliteConnections(void);

void UnhiliteNets(void);

void HiliteNetsFromDialog(int32 NrNetsSelected, int32 mode, int32 * NetInfo, uint8 * SelectedNets);

void HideConnectionsNetsFromDialog(int32 NrNetsSelected, int32 * NetInfo, uint8 * SelectedNets);

void DisableConnectionsNets(int32 NrNetsSelected, int32 mode, int32 * NetInfo, uint8 * SelectedNets);

void UnselectTracesViasNet(int32 NrNetsSelected, int32 * NetInfo, uint8 * SelectedNets);

void ViewAllConnections(void);

void DeleteNets(int32 NrNetsSelected, int32 * NetInfo);

void UnViewAllConnections(void);

int32 CopySelectionsToClipBoard(void);

int32 CopySelectionsFromClipBoard(void);

void CheckClipBoard(void);

void HiliteVisibleConnections(int32 mode);

void ChangeNetsHilite(int32 NetNr, int32 Hilite);

void ComponentProtection(int32 NetsSelected, int32 * NetInfo, uint8 * SelectedComponents);

int32 HideSelectedConnections(void);

#endif
