/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: instance.h
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


#ifndef _INSTANCE

#define _INSTANCE

#include "owntypes.h"

int32 Annotate(int32 mode, int32 MaxNrRefsPerSheet);

int32 AnnotateRestart(int32 mode, int32 MaxNrRefsPerSheet);

int32 AnnotateAppend(int32 mode, int32 MaxNrRefsPerSheet);

int32 CheckRefInstances(int32 Mode);

int32 GetSheetByReference(LPSTR Reference);

int32 GetSheetByPartnr(LPSTR Partnr);

int32 BillOfMaterials(int32 mode);

int32 CopySymbolsToProject(int32 Mode);

int32 CopyShapesToProject(int32 Mode);

int32 CreateGatePinSwapInfo(int32 Mode);

int32 WriteSchematics(int32 mode);

#endif
