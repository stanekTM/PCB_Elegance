/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: gerber3.h
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


#ifndef _GERBER3

#define _GERBER3

#include "types.h"

int32 GerberPrepareTraces(int32 Layer, int32 mode);

int32 GerberPrepareVias(int32 Layer, int32 mode);

int32 GerberPrepareComponentPins(int32 Layer, int32 mode);

int32 GerberPrepareComponentPinsViasSolderPasteObjects(int32 Layer, int32 mode);

int32 GerberPrepareComponentInfoObjects(int32 Layer, int32 mode);

int32 GerberPrepareComponentInfoObjects(int32 Layer, int32 mode);

int32 GerberPrepareComponentOutlineObjects(int32 Layer, int32 CompOutlineLayers, int32 mode);

int32 GerberPrepareComponentBoardOutlineObjects(int32 Layer, int32 mode);

int32 GerberPrepareComponentRoutingKeepoutObjects(int32 Layer, int32 mode);

int32 GerberPrepareComponentSilkScreenObjects(int32 Layer, int32 mode);

int32 GerberPrepareSpecialObjects(int32 Layer, int32 mode);

#endif
