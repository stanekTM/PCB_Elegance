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


void SetWindowName(int32 mode);

int32 LoadNewFile(int32 mode);

int32 SaveFileName(int32 Mode);

int32 CopySymbolFromLibraryToFile(LPSTR SymbolName, LPSTR SymbolFileName);

int32 GetSymbolAttribute(SymbolRecord * Symbol, LPSTR AttributeIdent, LPSTR AttributeValue, int32 mode);

int32 AddSymbolAttribute(SymbolRecord * Symbol, LPSTR AttributeIdent, LPSTR AttributeValue, int32 mode);

int32 RemoveSymbolAttribute(SymbolRecord * Symbol, LPSTR AttributeIdent, int32 mode);

int32 LoadDesign();

int32 LoadSymbol();

int32 SaveFile(int32 mode);

int32 SaveFile2(int32 mode);

int32 SearchSymbol(LPSTR SymbolName, LPSTR FileName, int32 * Pos, int32 * Length, int32 mode);

int32 CheckFileName(LPSTR CheckFile, int32 FileSize, int32 Mode);

int32 ChangeFile(LPSTR FileName, int32 mode);

void InitLoadedObjects(int32 Mode);

int32 GetCurrentEditFile(LPSTR FileName);

int32 ReloadSymbols(int32 mode);

int32 CheckWildCard(LPSTR SearchString, LPSTR Name);

void LoadDesignIniFile(void);

#endif
