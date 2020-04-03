/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: dialogs.h
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



#ifndef _DIALOGS

#define _DIALOGS


#include "types.h"

int32 MessageDialog(LPSTR InfoLine, int32 Mode);

int32 TextInputDialog(ObjectRecord * ObjectText, int32 Mode);

int32 TextInputDialog2(LPSTR Text, LPSTR DialogText, int32 Mode);

int32 LineInputDialog(ObjectRecord2 * ObjectText, LPSTR DialogText);

GeomCreateRecord *GetGeomValueRecord(int32 DialogMode);

int32 CreateGeomDialog(int32 Mode);

int32 ViewObjectsDialog(int32 Mode);

int32 AssignPinDialog(ObjectRecord * ObjectText, int32 Mode);

int32 AssignValuesDialog(int32 Mode);

int32 SelectNewGeomtrieDialog(int32 Mode);

int32 GridDialog(int32 mode);

int32 AboutDialog(void);

int32 AddTextObjectsDialog(int32 Mode);

int32 ColorDialog(int32 mode);

int32 Registration(void);

int32 EditPadRules(void);

#endif
