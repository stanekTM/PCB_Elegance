/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: menus.c
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


#include "types.h"
#include "menus.h"
#include "calc.h"
#include "stdio.h"
#include "memory.h"
#include "mainloop.h"
#include "stdlib.h"
#include "resource.h"
#include "design.h"
#include "utf8.h"


#define PopUpMenuX     30
#define PopUpMenuY     30

HMENU DESIGNMenu, DESIGNMenu1, DESIGNMenu1a, DESIGNMenu2, DESIGNMenu3, DESIGNMenu4, DESIGNMenu5, DESIGNMenu6,
      DESIGNMenu6a, DESIGNMenu6b, DESIGNMenu7, DESIGNMenu8, DESIGNMenu9, DESIGNMenu10;

HMENU PopUpMenu, DefaultMenu, DefaultMenu1, DefaultMenu2, DefaultMenu3, DefaultMenu4, SheetMenu, SheetMenu1, SheetMenu2,
      SheetMenu3, SheetMenu4, SheetMenu5, SheetMenu6, SheetMenu7, SheetMenu8, SheetMenu9, SymbolMenu, SymbolMenu1,
      SymbolMenu2, SymbolMenu3, SymbolMenu4, SymbolMenu5, SymbolMenu6, SymbolMenu7, SymbolMenu8, SymbolMenu9;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MenuPopUp(int32 mode)
{
	int32 cnt, NrMenus;
	char str[MAX_LENGTH_STRING];
	HMENU PopUpMenu, PopUpMenu1 = NULL;

	GetDesignSheets();
	NrMenus = 0;
	PopUpMenu = CreatePopupMenu();

	if (mode == 0)
	{
		PopUpMenu1 = CreatePopupMenu();

		for (cnt = 0; cnt < NrSheets; cnt++)
		{
			strcpy(str, Sheets[cnt].SheetName);
			_strupr(str);
			AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SHEET_OPEN_TOP_SHEET + cnt, str);
		}

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu1, SC(152, "Open sheet"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_SHEET_OPEN_REF, SC(153, "Open sheet on reference"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_SHEET_OPEN_PARTNR, SC(154, "Open sheet on part nr"));
	}
	else
	{
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LAYOUT_OPEN_REF, SC(269, "Open layout on reference"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LAYOUT_OPEN_PARTNR, SC(270, "Open layout on part nr"));
	}

	AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, 0, SC(265, "Exit"));
	TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40, 0,
	               DESIGNWindow, NULL);
	DestroyMenu(PopUpMenu);

	if (PopUpMenu1)
		DestroyMenu(PopUpMenu1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UpdateFileMenu(int32 mode)
{
	HMENU FileMenu, MainMenu;
	int32 cnt, res, NrItems;

	MainMenu = GetMenu(DESIGNWindow);
	FileMenu = GetSubMenu(MainMenu, 0);
//  res=AppendMenuUTF8(FileMenu,MF_ENABLED|MF_SEPARATOR,0,0);

	NrItems = 12;
#ifdef _DEBUG
	NrItems = 13;
#endif

	for (cnt = NrItems + TotalNrDesigns + 1; cnt >= NrItems; cnt--)
		res = DeleteMenu(FileMenu, cnt, MF_BYPOSITION);


	for (cnt = 0; cnt < NrDesigns; cnt++)
		res = AppendMenuUTF8(FileMenu, MF_ENABLED | MF_STRING, ID_DESIGNS + cnt, LastDesigns[cnt]);

	AppendMenuUTF8(FileMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(FileMenu, MF_ENABLED | MF_STRING, ID_FILE_EXIT, SC(265, "Exit"));

	DrawMenuBar(DESIGNWindow);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MakeMainMenu()
{

#define MENU_ID       MF_ENABLED|MF_STRING

	DESIGNMenu = CreateMenu();

// *****************************************************************************************
	DESIGNMenu1 = CreateMenu();
	AppendMenuUTF8(DESIGNMenu, MF_ENABLED | MF_POPUP, (UINT) DESIGNMenu1, SC(155, "File"));
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_NEWDESIGN, SC(156, "New design"));
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_OPENDESIGN, SC(157, "Open design"));
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_CLOSEDESIGN, SC(158, "Close design"));
	AppendMenuUTF8(DESIGNMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_PRINT_ALL_SHEETS, SC(159, "Print all sheets"));
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_PRINT_ALL_SHEETS_PDF, SC(218, "Print all the schematics to one PDF file"));
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_COPYSYMBOLSSHAPESLOCAL, SC(160, "Copy symbols/geometries local"));
	AppendMenuUTF8(DESIGNMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_ORCAD_CONV, SC(69, "Convert ORCAD schematic"));
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_FILE_ORCAD_CONV2, SC(74, "Convert ORCAD library"));
	AppendMenuUTF8(DESIGNMenu1, MENU_ID, ID_VIEW_GERBER_FILES, SC(163, "Gerber viewer"));
	AppendMenuUTF8(DESIGNMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);

// *****************************************************************************************

	DESIGNMenu2 = CreateMenu();
	AppendMenuUTF8(DESIGNMenu, MF_ENABLED | MF_POPUP, (UINT) DESIGNMenu2, SC(164, "Edit"));
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_ANNOTATE, SC(143, "Generate annotation sheets"));
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_EDIT_CHECK, SC(166, "Check schematics"));
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_BUILD_NETLIST, SC(144, "Generate building netlist/components from sheets"));
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_EDIT_BILLOFMATERIALS, SC(109, "Generate bill of materials"));
	AppendMenuUTF8(DESIGNMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_EDIT_DESIGNSETTINGS, SC(38, "Project settings"));
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_CHANGE_INSTANCES, SC(263, "Change components"));
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_EDIT_CREATEGATEPINSWAPINFO, SC(168, "Create gate/pin swap info"));
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_EDIT_VARS, SC(276, "Edit user variables file"));
	AppendMenuUTF8(DESIGNMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_EDIT_CONFIGUREPATHS, SC(280, "Configure paths"));
#ifdef _DEBUG
	AppendMenuUTF8(DESIGNMenu2, MENU_ID, ID_WRITE_SCHEMATICS, SC(45, "Load schematics"));
#endif

// *****************************************************************************************
	AppendMenuUTF8(DESIGNMenu, MENU_ID, ID_LIBRARYMANAGER_SYMBOLS, SC(169, "Library manager symbols"));
	AppendMenuUTF8(DESIGNMenu, MENU_ID, ID_LIBRARYMANAGER_GEOMETRIES, SC(170, "Library manager geometries"));
	AppendMenuUTF8(DESIGNMenu, MENU_ID, ID_MANUAL, SC(279, "Manual"));

// *****************************************************************************************
	DESIGNMenu5 = CreateMenu();
	AppendMenuUTF8(DESIGNMenu, MF_ENABLED | MF_POPUP, (UINT) DESIGNMenu5, SC(36, "Help"));
	AppendMenuUTF8(DESIGNMenu5, MENU_ID, ID_HELP_TOPICS, SC(171, "Topics"));
	AppendMenuUTF8(DESIGNMenu5, MENU_ID, ID_HELP_GETTING_STARTED, SC(172, "Getting started"));
	AppendMenuUTF8(DESIGNMenu5, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(DESIGNMenu5, MENU_ID, ID_HELP_ABOUT, SC(62, "About program"));

// *****************************************************************************************
	SetMenu(DESIGNWindow, DESIGNMenu);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
