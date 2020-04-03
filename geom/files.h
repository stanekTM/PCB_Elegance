/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: files.h
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


#ifndef _FILES

#define _FILES

#include  "owntypes.h"

int32 SaveFile(int32 mode);

int32 SaveFile2(int32 mode);

int32 SaveFileName(int32 Mode);

int32 CheckPads(int32 mode);

void InitNewShape(LPSTR Name, int32 mode);

void FilesMain(void);

int32 LoadDesign();

int32 LoadBitmapFile(LPSTR EditFile);

void GetSymbolNameFromFileName(LPSTR FileName, LPSTR SymbolName);

int32 CollectObjects(void);

void SetWindowName(int32 mode);

void ChangeFile(LPSTR FileName, int32 mode);

int32 ImportBitmapAsPin(void);

#endif
