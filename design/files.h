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

int32 CheckPins(int32 mode);

int32 LoadFileName(void);

int32 SaveFileName(int32 Mode);

int32 LoadDesignOnly(LPSTR FileName);

int32 LoadSymbolOnly(LPSTR FileName);

int32 LoadSymbol(LPSTR FileName);

int32 SaveFile(int32 mode);

int32 SearchSymbol(LPSTR SymbolName, LPSTR FileName, int32 * Pos, int32 * Length, int32 mode);

void GetSymbolNameFromFileName(LPSTR FileName, LPSTR SymbolName);

void SelectColors(void);

void ChangeFile(int32 mode);

void InitLoadedObjects(int32 Mode);

int32 GetCurrentEditFile(LPSTR FileName);

int32 LoadDesign(void);

int32 LoadSheetInMemory(int32 SheetNr, int32 mode);

int32 MakeBackup(LPSTR CurrentFile);

void LoadSchematicIniFile(int32 mode);

int32 CheckExtensionFile(LPSTR FileName);

int32 TextInputDialog(LPSTR TextLine, int32 Mode);

int32 ProjectDialog(int32 Mode);

int32 NewProjectDialog(int32 Mode);

int32 BOMDialog(int32 Mode);

int32 AnnotationDialog(int32 Mode, int32 * MaxNrRefsPerSheet);

int32 AboutDialog(void);

int32 LoadOrcadFileName(LPSTR OrcadFile, int32 mode);

int32 OrcadConversion(int32 mode);

void PrintAllSheets(int32 mode);

void SaveUserIniFile(int32 mode);

void LoadUserIniFile(void);

void LoadDesignIniFile(void);

void SaveDesignIniFile(void);

int32 AddMessage(LPSTR str);

int32 CheckProjectPath(char* NewProjectPath);

int32 ConfigurePathsDialog(int32 Mode);


#endif
