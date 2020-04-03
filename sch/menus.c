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
#include "memory.h"
#include "mainloop.h"
#include "stdlib.h"
#include "resource.h"
#include "sch.h"
#include "calc.h"
#include "stdio.h"
#include "files.h"
#include "toets.h"
#include "inscomp.h"
#include "select.h"
#include "utf8.h"

#define PopUpMenuX     30
#define PopUpMenuY     30

HMENU PopUpMenu, MainMenu, DefaultMenu, DefaultMenu1, DefaultMenu2, DefaultMenu3, DefaultMenu4, EditMenu, HelpMenu,
      SheetMenu, SheetMenu1, SheetMenu2, SheetMenu3, SheetMenu4, SheetMenu5, SheetMenu6, SheetMenu7, SheetMenu8,
      SheetMenu9, SymbolMenu, SymbolMenu1, SymbolMenu2, SymbolMenu3, SymbolMenu4, SymbolMenu5, SymbolMenu6, SymbolMenu7,
      SymbolMenu8, SymbolMenu9;

HBITMAP BitMapCircle_Filled;
HBITMAP BitMapCircle_F;
HBITMAP BitMapCircle_3;
HBITMAP BitMapCircle_6;
HBITMAP BitMapCircle_C;
HBITMAP BitMapCircle_9;
HBITMAP BitMapCircle_1;
HBITMAP BitMapCircle_2;
HBITMAP BitMapCircle_4;
HBITMAP BitMapCircle_8;
HBITMAP BitMapLine1;
HBITMAP BitMapLine2;
HBITMAP BitMapLine3;
HBITMAP BitMapLine4;
HBITMAP BitMapRect_1;
HBITMAP BitMapRect_2;
HBITMAP BitMapDimension1;
HBITMAP BitMapDimension2;

char SelectedSymbolNameToEdit[MAX_LENGTH_STRING];
int32 ok;

extern char UsedFiles[16][MAX_LENGTH_STRING];
extern int32 NrUsedFiles;
extern int32 ProjectIndexNr;
extern ProjectInfoRecord *ProjectInfo;

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void LoadBitMapsPopup()
{
	BitMapCircle_F = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_F));
	BitMapCircle_3 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_3));
	BitMapCircle_6 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_6));
	BitMapCircle_C = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_C));
	BitMapCircle_9 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_9));
	BitMapCircle_1 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_1));
	BitMapCircle_2 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_2));
	BitMapCircle_4 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_4));
	BitMapCircle_8 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_8));
	BitMapCircle_Filled = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_FILLED));
	BitMapLine1 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_LINE1));
	BitMapLine2 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_LINE2));
	BitMapLine3 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_LINE3));
	BitMapLine4 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_LINE4));
	BitMapDimension1 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_DIMENSION1));
	BitMapDimension2 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_DIMENSION2));
	BitMapRect_1 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_RECT_1));
	BitMapRect_2 = LoadBitmap(SCHClass.hInstance, MAKEINTRESOURCE(BITMAP_RECT_2));


}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DestroyBitMapsPopup()
{
	DeleteObject(BitMapCircle_F);
	DeleteObject(BitMapCircle_3);
	DeleteObject(BitMapCircle_6);
	DeleteObject(BitMapCircle_C);
	DeleteObject(BitMapCircle_9);
	DeleteObject(BitMapCircle_1);
	DeleteObject(BitMapCircle_2);
	DeleteObject(BitMapCircle_4);
	DeleteObject(BitMapCircle_8);
	DeleteObject(BitMapCircle_Filled);
	DeleteObject(BitMapLine1);
	DeleteObject(BitMapLine2);
	DeleteObject(BitMapLine3);
	DeleteObject(BitMapLine4);
	DeleteObject(BitMapDimension1);
	DeleteObject(BitMapDimension2);
	DeleteObject(BitMapRect_1);
	DeleteObject(BitMapRect_2);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 OwnAppendMenu(HMENU Menu, UINT MenuOptions, UINT MenuId, LPCTSTR Text)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	int32 res, Mask;

	ok = MF_STRING;
	Mask = MF_SEPARATOR | MF_BITMAP | MF_OWNERDRAW | MF_POPUP;

	if ((MenuOptions & Mask) == 0)
	{
		if (Text == NULL)
			res = 1;

		sprintf(str, Text);

		if (GetKeyString(str2, MenuId, 0x11) == 1)
			strcat(str, str2);

		AppendMenu(Menu, MenuOptions, MenuId, str);
	}
	else
		AppendMenu(Menu, MenuOptions, MenuId, Text);

	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void MenuPopUp()
{
	int32 cnt, cnt2, cnt3, cnt4, Pos, Length, LoadResult, First, Found;
	char str[MAX_LENGTH_STRING], SymbolName[MAX_LENGTH_STRING], LibName[MAX_LENGTH_STRING];
	RedefinedPinBusRecord *RedefinedPinBus;
	ObjectRecord *Object;
	HMENU PopUpMenu, PopUpMenu1, PopUpMenu11, PopUpMenu12, PopUpMenu2, PopUpMenu21, PopUpMenu22, PopUpMenu3, PopUpMenu4,
	      PopUpMenu5, PopUpMenu6, CompMenu;

	InstanceRecord *Instance;

	LoadBitMapsPopup();

	PopUpMenu = 0;
	PopUpMenu1 = 0;
	PopUpMenu11 = 0;
	PopUpMenu12 = 0;
	PopUpMenu2 = 0;
	PopUpMenu21 = 0;
	PopUpMenu22 = 0;
	PopUpMenu3 = 0;
	PopUpMenu4 = 0;
	PopUpMenu5 = 0;
	PopUpMenu6 = 0;
	CompMenu = 0;
	GetNrSelections();

	if ((WiresSelected + BussesSelected + InstancesSelected + InstanceRefsSelected + InstanceValuesSelected +
	        BusConnectionsSelected + OnePinNetsSelected + JunctionsSelected + NetLabelsSelected + ObjectLinesSelected +
	        ObjectRectsSelected + ObjectCirclesSelected + ObjectArcsSelected + ObjectTextsSelected +
	        GlobalConnectionsSelected + GlobalConnectionTextsSelected + PinBussesSelected + PowerPinsSelected +
	        PinsSelected) == 0)
	{


		PopUpMenu = CreatePopupMenu();
		PopUpMenu1 = CreatePopupMenu();
		PopUpMenu2 = CreatePopupMenu();
		PopUpMenu3 = CreatePopupMenu();

		if (EditingSymbol)
		{
			if (!EditingSheetSymbol)
			{
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDPIN, SC(57, "Add pin"));
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDPOWERPIN, SC(72, "Add power pin"));
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDPINBUS, SC(81, "Add pin bus"));
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_GATEPINSWAP, SC(305, "Edit gate/pin swap"));
			}
			else
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDPIN, SC(59, "Add pin label"));

//      AppendMenuUTF8(PopUpMenu,MF_ENABLED|MF_STRING,ID_EDITSYMBOLNAME,"Edit symbol name");
//      AppendMenuUTF8(PopUpMenu,MF_ENABLED|MF_STRING,ID_EDITSYMBOLREF,"Edit symbol ref");
		}
		else
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDWIRE, SC(235, "Add wire"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDBUS, SC(236, "Add bus"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDBUSCONNECTION, SC(307, "Add bus connection"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDSYMBOL, SC(242, "Add symbol"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDSYMBOL_SHORTCUT, SC(308, "Add symbol with search"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDCOMPONENT, SC(243, "Add database component"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu3, SC(237, "Add external connections"));
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_ADDGLOBALCONNECTION_I, SC(46, "Input"));
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_ADDGLOBALCONNECTION_O, SC(47, "Output"));
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_ADDGLOBALCONNECTION_IO, SC(48, "Input/Output"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDJUNCTION, SC(306, "Add wire connection"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDONEPINNET, SC(466, "Add one pinnet mark"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDWIRELABEL, SC(314, "Add netlabel + wire"));
		}

		if (NestingLevel > 0)
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_FILE_SHEETBACK, SC(252, "Goto higher sheet"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		}

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu1, SC(315, "Add special"));
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_ADDLINE, SC(224, "Line"));
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_BITMAP, ID_ADD_ARROW1, (LPSTR) BitMapLine2);
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_BITMAP, ID_ADD_ARROW2, (LPSTR) BitMapLine3);
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_BITMAP, ID_ADD_ARROW3, (LPSTR) BitMapLine4);
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_BITMAP, ID_ADD_DIMENSION, (LPSTR) BitMapDimension1);
//    AppendMenuUTF8(PopUpMenu1,MF_ENABLED|MF_BITMAP,ID_ADD_DIMENSION2,(LPSTR)BitMapDimension2);

//    AppendMenuUTF8(PopUpMenu1,MF_ENABLED|MF_STRING,ID_ADDRECT,"Rect"));
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_BITMAP, ID_ADDRECT2, (LPSTR) BitMapRect_1);
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_BITMAP, ID_ADDRECT3, (LPSTR) BitMapRect_2);
//    AppendMenuUTF8(PopUpMenu1,MF_ENABLED|MF_STRING,ID_ADDRECT2,"Rect"));
//    AppendMenuUTF8(PopUpMenu1,MF_ENABLED|MF_STRING,ID_ADDRECT3,"Rect filled"));
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu2, SC(226, "Circle"));
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_FILLED, (LPSTR) BitMapCircle_Filled);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE, (LPSTR) BitMapCircle_F);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_3, (LPSTR) BitMapCircle_3);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_6, (LPSTR) BitMapCircle_6);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_C, (LPSTR) BitMapCircle_C);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_9, (LPSTR) BitMapCircle_9);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_1, (LPSTR) BitMapCircle_1);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_2, (LPSTR) BitMapCircle_2);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_4, (LPSTR) BitMapCircle_4);
		AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_BITMAP, ID_ADDCIRCLE_8, (LPSTR) BitMapCircle_8);

		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_ADDARC, SC(227, "Arc"));
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_ADDTEXT, SC(228, "Text"));
		AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_ADDTEXTNUM, SC(130, "Add text numbers incremental"));

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_INSERT_CLIP, SC(319, "Paste objects from clipboard"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_UNDO, SC(244, "Undo"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_REDO, SC(245, "Redo"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_SELECT_COMPS_BY_LIST,
		              SC(491, "Component selections by list"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_UNSELECT_ALL, SC(418, "Unselect all"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_VIEWFULL, SC(320, "Full screen"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_PREVIOUS_VIEW, SC(321, "Previous view"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_REPAINT, SC(322, "Repaint"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_GOTOXY, SC(468, "Goto (x,y)"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(1, "Exit"));
		TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40, 0,
		               SCHWindow, NULL);
	}
	else
	{
		PopUpMenu = CreatePopupMenu();
		PopUpMenu1 = CreatePopupMenu();
		PopUpMenu2 = CreatePopupMenu();
		PopUpMenu3 = CreatePopupMenu();
		PopUpMenu4 = CreatePopupMenu();
		PopUpMenu5 = CreatePopupMenu();
		PopUpMenu6 = CreatePopupMenu();
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_MOVE_OBJECTS, SC(248, "Move"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_DRAG_OBJECTS, SC(249, "Drag"));

		if ((WiresSelected + BussesSelected + BusConnectionsSelected + JunctionsSelected + NetLabelsSelected +
		        ObjectLinesSelected + ObjectRectsSelected + ObjectCirclesSelected + ObjectArcsSelected +
		        ObjectTextsSelected + GlobalConnectionsSelected + GlobalConnectionTextsSelected == 0)
		        && (InstancesSelected > 0) && (GetFirstSymbolSheetSelected() >= 0) && (DesignPath[0] != 0))
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_FILE_EDIT_SHEET, SC(324, "Edit sheet selected symbol"));
		}
		else
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS, SC(246, "Copy"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu3, SC(325, "Copy multiple"));
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS2, "2");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS3, "3");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS4, "4");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS5, "5");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS6, "6");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS7, "7");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS8, "8");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS9, "9");
			AppendMenuUTF8(PopUpMenu3, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS10, "10");
		}

		if ((!EditingSymbol)
		        && (((WiresSelected == 1) && (NetLabelsSelected == 0))
		            || ((WiresSelected == 0) && (NetLabelsSelected == 1))))
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_PROPERTY, SC(330, "Add/modify net properties"));

		if (InstancesSelected == 1)
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord*) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					if ((Instance->Info & 4) == 4)
					{
						strcpy(SymbolName, Instance->SymbolName);

						LoadResult = SearchSymbol(SymbolName, LibName, &Pos, &Length, 0);

						if (LoadResult == 1)
						{
							strcpy(SelectedSymbolNameToEdit, SymbolName);

							sprintf(str, SC(131, "Edit symbol %s"), SymbolName); //název symbolu
//							sprintf(str, SC(131, "Edit symbol %s"), Instance->Reference); //název reference

							AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
							AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_FILE_EDIT_SYMBOL, str);
						}
// ****************************************************************************************************
// ****************************************************************************************************
						NrObjects = 0;
						InstanceToObject(Instance, 0.0, 0.0, 0);
						First = 1;
						cnt4 = 0;

						for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
						{
							Object = &((*Objects)[cnt2]);

							if (Object->ObjectType == SYMBOL_PINBUS_TEXT)
							{
								for (cnt3 = 0; cnt3 < Design.NrRedefinedPinBusses; cnt3++)
								{
									RedefinedPinBus = &((*RedefinedPinBusses)[cnt3]);

									if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
									{
										if ((stricmpUTF8(RedefinedPinBus->Reference, Instance->Reference) == 0)
										        && (stricmpUTF8(RedefinedPinBus->Name, Object->Text2) == 0))
										{
											if (First)
											{
												AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu4,
												              SC(180, "Edit pinbus reorder"));
												First = 0;
											}

											sprintf(str, "%s  %s", Instance->Reference, RedefinedPinBus->Name);
											AppendMenuUTF8(PopUpMenu4, MF_ENABLED | MF_STRING,
											              ID_EDIT_PINBUSREORDER + cnt4, str);
										}
									}
								}

								cnt4++;
							}
						}

						cnt4 = 0;
						First = 1;

						for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
						{
							Object = &((*Objects)[cnt2]);

							if (Object->ObjectType == SYMBOL_PINBUS_TEXT)
							{
								Found = 0;

								for (cnt3 = 0; cnt3 < Design.NrRedefinedPinBusses; cnt3++)
								{
									RedefinedPinBus = &((*RedefinedPinBusses)[cnt3]);

									if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
									{
										if ((stricmpUTF8(RedefinedPinBus->Reference, Instance->Reference) == 0)
										        && (stricmpUTF8(RedefinedPinBus->Name, Object->Text2) == 0))
											Found = 1;
									}
								}

								if (!Found)
								{
									if (First)
									{
										AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu5,
										              SC(182, "Change pinbus reorder"));
										First = 0;
									}

									sprintf(str, "%s  %s", Instance->Reference, Object->Text2);
									AppendMenuUTF8(PopUpMenu5, MF_ENABLED | MF_STRING, ID_ADD_PINBUSREORDER + cnt4, str);
									cnt4++;
								}
							}
						}
					}

					if ((!EditingSymbol) && (Instance->Reference[0] != 0) && (Instance->Reference[0] != '?'))
					{
						cnt2 = strlen(Instance->Reference);

						if (Instance->Reference[cnt2 - 1] != '?')
						{
							sprintf(str, SC(132, "Open layout centered on reference %s"), Instance->Reference);
							AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
							AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_LAYOUT_ON_REF, str);
						}
					}
				}
			}

		}

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_COPY_CLIP, SC(329, "Copy to clipboard"));

		if (WiresSelected + BussesSelected > 0)
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDNETLABEL, SC(109, "Add netlabel to wire/bus"));

			if (WiresSelected > 1)
			{
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ADDNETLABELS,
				              SC(331, "Add incremental netlabels to wires"));
			}
		}

//    if (!EditingSymbol) {
//      AppendMenuUTF8(PopUpMenu,MF_ENABLED|MF_STRING,ID_MENU_EDIT_INSTANCEINFO,"Edit component parameters\te");
//    }
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_DELETE, SC(247, "Delete"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ROTATE_OBJECTS, SC(332, "Rotate counterclockwise"));

		if (!EditingSymbol)
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ROTATE_OBJECTS2, SC(333, "Rotate at any angle"));

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_SCALE_OBJECTS, SC(328, "Scale"));

		if ((ObjectTextsSelected > 1) || (NetLabelsSelected > 1) || (PinBussesSelected > 1) || (PowerPinsSelected > 1)
		        || (PinsSelected > 1))
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ALIGN_OBJECTS_LEFT, SC(335, "Align text objects left"));
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ALIGN_OBJECTS_RIGHT,
			              SC(336, "Align text objects right"));
		}

		if (InstancesSelected > 1)
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_MULTIPLE_INSTANCES,
			              SC(488, "Edit multiple components"));
		}

		if (NetLabelsSelected == 2)
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_SWAP_NETLABELS, SC(485, "Swap netlabels"));

		if (PinsSelected == 2)
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_SWAP_PINS, SC(486, "Swap pins"));

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_LINE_WIDTH, SC(337, "Change line width"));

		if (ObjectLinesSelected > 0)
		{
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu6, SC(338, "Change line style"));
			AppendMenuUTF8(PopUpMenu6, MF_ENABLED | MF_BITMAP, ID_CHANGE_LINE_STYLE1, (LPSTR) BitMapLine1);
			AppendMenuUTF8(PopUpMenu6, MF_ENABLED | MF_BITMAP, ID_CHANGE_LINE_STYLE2, (LPSTR) BitMapLine2);
			AppendMenuUTF8(PopUpMenu6, MF_ENABLED | MF_BITMAP, ID_CHANGE_LINE_STYLE3, (LPSTR) BitMapLine3);
			AppendMenuUTF8(PopUpMenu6, MF_ENABLED | MF_BITMAP, ID_CHANGE_LINE_STYLE4, (LPSTR) BitMapLine4);
		}

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_MIRRORX_OBJECTS, SC(339, "Mirror x"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_MIRRORY_OBJECTS, SC(340, "Mirror y"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_TEXT, SC(341, "Edit"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_TEXT_HEIGHT, SC(342, "Change text height"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_UNSELECT_ALL, SC(418, "Unselect all"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu1, SC(343, "Unselect"));

//    AppendMenuUTF8(PopUpMenu1,MF_ENABLED|MF_STRING,ID_UNSEL_ALL,"All");
		if (!EditingSymbol)
		{
			if (WiresSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_WIRES, SC(344, "Wires"));

			if (BussesSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_BUSSES, SC(207, "Busses"));

			if (NetLabelsSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_NETLABELS, SC(108, "Net labels"));

//      if (JunctionsSelected>0) {
//        AppendMenuUTF8(PopUpMenu1,MF_ENABLED|MF_STRING,ID_UNSEL_JUNCTIONS,"Junctions");
//      }
			if (BusConnectionsSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_BUSCONN, SC(347, "Bus connections"));

			if (GlobalConnectionsSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_GLOBALCONN, SC(348, "External connections"));

			if (GlobalConnectionTextsSelected > 0)
			{
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_GLOBALCONN_TEXT,
				              SC(349, "Text external connections"));
			}

			if (InstancesSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_INST, SC(350, "Components"));

			if (InstanceRefsSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_INST_REF, SC(351, "Component references"));

			if (InstanceValuesSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_INST_VALUE, SC(352, "Component values"));
		}
		else
		{
			if (PinsSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_PINS, SC(78, "Pins"));

			if (PowerPinsSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_POWERPINS, SC(353, "Power pins"));

			if (PinBussesSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_PINBUSSES, SC(354, "Pin busses"));

//      if (PinsSelected+PowerPinsSelected+PinBussesSelected>0) {
//        AppendMenuUTF8(PopUpMenu1,MF_ENABLED|MF_STRING,ID_UNSEL_PINTEXTS,SC(40,"Pin Text"));
//      }
			if (InstanceRefsSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_INST_REF, SC(212, "Symbol reference"));

			if (InstanceValuesSelected > 0)
				AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_INST_VALUE, SC(99, "Symbol name"));
		}

		if (ObjectLinesSelected > 0)
			AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_LINES, SC(356, "Lines"));

		if (ObjectRectsSelected > 0)
			AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_RECTS, SC(357, "Rectangles"));

		if (ObjectCirclesSelected > 0)
			AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_CIRCLES, SC(358, "Circles"));

		if (ObjectArcsSelected > 0)
			AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_ARCS, SC(359, "Arcs"));

		if (ObjectTextsSelected > 0)
			AppendMenuUTF8(PopUpMenu1, MF_ENABLED | MF_STRING, ID_UNSEL_TEXTS, SC(360, "Texts"));

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_POPUP, (UINT) PopUpMenu2, SC(361, "Select only"));

		if (!EditingSymbol)
		{
			if (WiresSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_WIRES, SC(344, "Wires"));

			if (BussesSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_BUSSES, SC(207, "Busses"));

			if (NetLabelsSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_NETLABELS, SC(108, "Net labels"));

//      if (JunctionsSelected>0) {
//        AppendMenuUTF8(PopUpMenu2,MF_ENABLED|MF_STRING,ID_SEL_ONLY_JUNCTIONS,"Junctions");
//      }
			if (BusConnectionsSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_BUSCONN, SC(347, "Bus connections"));

			if (GlobalConnectionsSelected > 0)
			{
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_GLOBALCONN,
				              SC(348, "External connections"));
			}

			if (GlobalConnectionTextsSelected > 0)
			{
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_GLOBALCONN_TEXT,
				              SC(349, "Text external connections"));
			}

			if (InstancesSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_INST, SC(350, "Components"));

			if (InstanceRefsSelected > 0)
			{
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_INST_REF,
				              SC(351, "Component references"));
			}

			if (InstanceValuesSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_INST_VALUE, SC(352, "Component values"));

			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_SELECT_COMPS_BY_LIST,
			              SC(491, "Select components by list"));
		}
		else
		{
			if (PinsSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_PINS, SC(78, "Pins"));

			if (PowerPinsSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_POWERPINS, SC(353, "Power pins"));

			if (PinBussesSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_PINBUSSES, SC(354, "Pin busses"));

			if (PinsSelected + PinBussesSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_PINTEXTS, SC(40, "Pin Text"));

			if (InstanceRefsSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_INST_REF, SC(212, "Symbol reference"));

			if (InstanceValuesSelected > 0)
				AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_INST_VALUE, SC(99, "Symbol name"));
		}

		if (ObjectLinesSelected > 0)
			AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_LINES, SC(356, "Lines"));

		if (ObjectRectsSelected > 0)
			AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_RECTS, SC(357, "Rectangles"));

		if (ObjectCirclesSelected > 0)
			AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_CIRCLES, SC(358, "Circles"));

		if (ObjectArcsSelected > 0)
			AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_ARCS, SC(359, "Arcs"));

		if (ObjectTextsSelected > 0)
			AppendMenuUTF8(PopUpMenu2, MF_ENABLED | MF_STRING, ID_SEL_ONLY_TEXTS, SC(360, "Texts"));

		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_GOTOXY, SC(468, "Goto (x,y)"));
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
		AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, 0, SC(1, "Exit"));
		TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40, 0,
		               SCHWindow, NULL);
	}

	DestroyBitMapsPopup();
	DeleteCompMenu();
	DestroyMenu(PopUpMenu);

	if (PopUpMenu1 != 0)
		DestroyMenu(PopUpMenu1);

	if (PopUpMenu11 != 0)
		DestroyMenu(PopUpMenu11);

	if (PopUpMenu12 != 0)
		DestroyMenu(PopUpMenu12);

	if (PopUpMenu2 != 0)
		DestroyMenu(PopUpMenu2);

	if (PopUpMenu21 != 0)
		DestroyMenu(PopUpMenu21);

	if (PopUpMenu22 != 0)
		DestroyMenu(PopUpMenu22);

	if (PopUpMenu3 != 0)
		DestroyMenu(PopUpMenu3);

	if (PopUpMenu4 != 0)
		DestroyMenu(PopUpMenu4);

	if (PopUpMenu5 != 0)
		DestroyMenu(PopUpMenu5);

	if (PopUpMenu6 != 0)
		DestroyMenu(PopUpMenu6);

	if (CompMenu != 0)
		DestroyMenu(CompMenu);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void MakeDefaultMenu()
{
	DefaultMenu = CreateMenu();
	OwnAppendMenu(DefaultMenu, MF_ENABLED | MF_STRING, 0x001, SC(363, "Test"));

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void AddMenuFiles()
{
	int32 cnt;
	HMENU MainMenu, FileMenu;


	MainMenu = GetMenu(SCHWindow);
	FileMenu = GetSubMenu(MainMenu, 0);

	for (cnt = 0; cnt < 16; cnt++)
		DeleteMenu(FileMenu, ID_FILE_USED_FILES + cnt, MF_BYCOMMAND);

	for (cnt = 0; cnt < NrUsedFiles; cnt++)
		OwnAppendMenu(FileMenu, MF_ENABLED | MF_STRING, ID_FILE_USED_FILES + cnt, UsedFiles[cnt]);

	DrawMenuBar(SCHWindow);
}

//**************************************************************************************************************
//**************************** menu schema editoru *************************************************************
//**************************************************************************************************************

void MakeSheetMenu()
{
	SheetMenu = CreateMenu();
	SheetMenu1 = CreateMenu();
	SheetMenu2 = CreateMenu();
	EditMenu = SheetMenu2;
	HelpMenu = CreateMenu();
	SheetMenu3 = CreateMenu();
	SheetMenu5 = CreateMenu();
	SheetMenu7 = CreateMenu();
	AppendMenuUTF8(SheetMenu, MF_ENABLED | MF_POPUP, (UINT) SheetMenu1, SC(364, "File"));
	AppendMenuUTF8(SheetMenu, MF_ENABLED | MF_POPUP, (UINT) SheetMenu2, SC(341, "Edit"));

//  AppendMenuUTF8(SheetMenu,MF_ENABLED|MF_POPUP,(UINT) SheetMenu3,"Level");

	AppendMenuUTF8(SheetMenu, MF_ENABLED | MF_POPUP, (UINT) SheetMenu5, SC(366, "View"));
	AppendMenuUTF8(SheetMenu, MF_ENABLED | MF_POPUP, (UINT) HelpMenu, SC(3, "Help"));
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_STRING, ID_HELP_TOPICS, SC(368, "Topics"));
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_STRING, ID_HELP_CONTENTS, SC(369, "Getting started"));
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_STRING, ID_HELP_ABOUT, SC(406, "About program"));

	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEET, SC(231, "New sheet"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEET2, SC(370, "New sheet in new window"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SYMBOL, SC(255, "New symbol"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SYMBOL2, SC(371, "New symbol in new window"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEETSYMBOL, SC(254, "New sheet symbol"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEETSYMBOL2,
	              SC(372, "New sheet symbol in new window"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_OPEN, SC(232, "Open list"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_OPEN2, SC(374, "Open sheet/symbol in new window"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_SAVE, SC(233, "Save list"));

//	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_POPUP, (UINT) SheetMenu7, SC(259, "Save with errors"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_SAVE_WITH_ERRORS, SC(259, "Save with errors")); //SheetMenu7

	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_SAVEAS, SC(376, "Save as"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_RELOAD_SYMBOLS, SC(378, "Reload symbols"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_POPUP, (UINT) SheetMenu3, SC(379, "Print"));
	AppendMenuUTF8(SheetMenu3, MF_ENABLED | MF_STRING, ID_FILE_PRINT, SC(380, "Print in black"));
	AppendMenuUTF8(SheetMenu3, MF_ENABLED | MF_STRING, ID_FILE_PRINT_COLOR, SC(381, "Print in color"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_EXPORT_BITMAPS, SC(382, "Export to bitmap format"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_EXPORT_PDF, SC(383, "Export to PDF"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_EXPORT_DXF, SC(384, "Export to DXF format"));
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_IMPORT_DXF, SC(385, "Import from DXF file"));

//  AppendMenuUTF8(SheetMenu1,MF_ENABLED|MF_STRING,ID_MENU_FILE_DELETE_UNDO,"Delete undo buffer");
//  AppendMenuUTF8(SheetMenu1,MF_ENABLED|MF_SEPARATOR,0,0);

	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu1, MF_ENABLED | MF_STRING, ID_FILE_EXIT, SC(1, "Exit"));

//  AppendMenuUTF8(SheetMenu1,MF_ENABLED|MF_SEPARATOR,0,0);
//  AppendMenuUTF8(SheetMenu2,MF_ENABLED|MF_STRING,ID_MENU_EDIT_ANNOTATE,"Annotate");

	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_CHECK, SC(386, "Check"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_SEARCH_TEXT, SC(30, "Search for any text"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_SEARCH_NEXT_TEXT, SC(416, "Search next text"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_UNPROTECT, SC(387, "Unprotect symbols"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_PROTECT, SC(388, "Protect symbols"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_DELETE_PINBUSREORDER, SC(389, "Delete pinbus reorders"));

//  AppendMenuUTF8(SheetMenu3,MF_ENABLED|MF_STRING,ID_MENU_LEVEL_UP,"Up");
//  AppendMenuUTF8(SheetMenu3,MF_ENABLED|MF_STRING,ID_MENU_LEVEL_DOWN,"Down");
//  AppendMenuUTF8(SheetMenu4,MF_ENABLED|MF_STRING,ID_MENU_MODE_SCROLL,"Scroll (pixels)");
//  AppendMenuUTF8(SheetMenu4,MF_ENABLED|MF_STRING,ID_MENU_MODE_COLORS,"Colors");

	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_ZERORELATIVECURSOR, SC(395, "Zero relative cursor"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_CLEAR_REFS, SC(396, "Clear references (R1 -> R?)"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_ADDCOMPONENT, SC(243, "Add database component"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_VARS, SC(159, "Edit user variables file"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_COPY_CLIP, SC(329, "Copy to clipboard"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_INSERT_CLIP, SC(397, "Paste from clipboard"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_EDIT_GOTOXY, SC(468, "Goto (x,y)"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_REPLACE_TEXTS, SC(211, "Replace texts"));
	AppendMenuUTF8(SheetMenu2, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECTS_FROM_TEXTFILE, SC(253, "Import text from file"));

	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_ZOOMIN, SC(398, "Zoom in"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_ZOOMOUT, SC(399, "Zoom out"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_VIEWFULL, SC(320, "Full screen"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_PAN, SC(401, "Pan window"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_REPAINT, SC(322, "Repaint"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_PREVIOUS_VIEW, SC(321, "Previous view"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_CHANGECOLORS, SC(146, "Change colors"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_LOADDEFAULTCOLORS,
	              SC(404, "Black background"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_LOADDEFAULTCOLORS2,
	              SC(405, "White background"));
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SheetMenu5, MF_ENABLED | MF_STRING, ID_VIEW_OPTIONS, SC(453, "Sheet options"));
}

//**************************************************************************************************************
//**************************** menu symbol editoru *************************************************************
//**************************************************************************************************************

void MakeSymbolMenu()
{
	SymbolMenu = CreateMenu();
	SymbolMenu1 = CreateMenu();
	SymbolMenu2 = CreateMenu();
	EditMenu = SymbolMenu2;
	HelpMenu = CreateMenu();
	SymbolMenu3 = CreateMenu();
	SymbolMenu5 = CreateMenu();
	SymbolMenu6 = CreateMenu();
	AppendMenuUTF8(SymbolMenu, MF_ENABLED | MF_POPUP, (UINT) SymbolMenu1, SC(364, "File"));
	AppendMenuUTF8(SymbolMenu, MF_ENABLED | MF_POPUP, (UINT) SymbolMenu2, SC(341, "Edit"));
	AppendMenuUTF8(SymbolMenu, MF_ENABLED | MF_POPUP, (UINT) SymbolMenu5, SC(366, "View"));
	AppendMenuUTF8(SymbolMenu, MF_ENABLED | MF_POPUP, (UINT) HelpMenu, SC(3, "Help"));
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_STRING, ID_HELP_TOPICS, SC(368, "Topics"));
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_STRING, ID_HELP_CONTENTS, SC(369, "Getting started"));
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(HelpMenu, MF_ENABLED | MF_STRING, ID_HELP_ABOUT, SC(406, "About program"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEET, SC(231, "New sheet"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEET2, SC(370, "New sheet in new window"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SYMBOL, SC(255, "New symbol"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SYMBOL2, SC(371, "New symbol in new window"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEETSYMBOL, SC(254, "New sheet symbol"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_NEW_SHEETSYMBOL2,
	              SC(372, "New sheet symbol in new window"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_OPEN, SC(256, "Open symbol"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_SAVE, SC(257, "Save symbol"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_SAVEAS, SC(376, "Save as"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_POPUP, (UINT) SheetMenu3, SC(379, "Print"));
	AppendMenuUTF8(SymbolMenu3, MF_ENABLED | MF_STRING, ID_FILE_PRINT, SC(380, "Print in black"));
	AppendMenuUTF8(SymbolMenu3, MF_ENABLED | MF_STRING, ID_FILE_PRINT_COLOR, SC(381, "Print in color"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_EXPORT_BITMAPS, SC(382, "Export to bitmap format"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_EXPORT_PDF, SC(383, "Export to PDF"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_EXPORT_DXF, SC(384, "Export to DXF format"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_IMPORT_DXF, SC(385, "Import from DXF file"));

//  AppendMenuUTF8(SymbolMenu1,MF_ENABLED|MF_STRING,ID_MENU_FILE_DELETE_UNDO,"Delete undo buffer");
//  AppendMenuUTF8(SymbolMenu1,MF_ENABLED|MF_SEPARATOR,0,0);

	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_STRING, ID_FILE_EXIT, SC(1, "Exit"));
	AppendMenuUTF8(SymbolMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);

//  AppendMenuUTF8(SymbolMenu1,MF_ENABLED|MF_SEPARATOR,0,0);

	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_POPUP, (UINT) SymbolMenu6, SC(407, "Parts"));
	AppendMenuUTF8(SymbolMenu6, MF_ENABLED | MF_STRING, ID_EDIT_PARTS_NR, SC(36, "Total number of parts"));
	AppendMenuUTF8(SymbolMenu6, MF_ENABLED | MF_STRING, ID_EDIT_PARTS_PINS, SC(78, "Pins"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_SYMBOLNAMES, SC(99, "Symbol name"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_SYMBOLINFO, SC(128, "Symbol info"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_EXPORTTEXT, SC(409, "Export text"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_SEARCH_TEXT, SC(30, "Search for any text"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_SEARCH_NEXT_TEXT, SC(416, "Search next text"));

	if (!EditingSheetSymbol)
		AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_GATEPINSWAP, SC(305, "Edit gate/pin swap"));

	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_CHECK, SC(386, "Check"));

//  AppendMenuUTF8(SymbolMenu4,MF_ENABLED|MF_STRING,ID_MENU_MODE_SCROLL,"Scroll (pixels)");
//  AppendMenuUTF8(SymbolMenu4,MF_ENABLED|MF_STRING,ID_MENU_MODE_COLORS,"Colors");

	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_ZERORELATIVECURSOR, SC(395, "Zero relative cursor"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_COPY_CLIP, SC(329, "Copy to clipboard"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_INSERT_CLIP, SC(397, "Paste from clipboard"));
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SymbolMenu2, MF_ENABLED | MF_STRING, ID_EDIT_GOTOXY, SC(468, "Goto (x,y)"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_ZOOMIN, SC(398, "Zoom in"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_ZOOMOUT, SC(399, "Zoom out"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_VIEWFULL, SC(320, "Full screen"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_PAN, SC(401, "Pan window"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_REPAINT, SC(322, "Repaint"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_PREVIOUS_VIEW, SC(321, "Previous view"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_CHANGECOLORS, SC(146, "Change colors"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_LOADDEFAULTCOLORS,
	              SC(404, "Black background"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_LOADDEFAULTCOLORS2,
	              SC(405, "White background"));
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_SEPARATOR, 0, 0);
	AppendMenuUTF8(SymbolMenu5, MF_ENABLED | MF_STRING, ID_VIEW_OPTIONS, SC(464, "Symbol options"));
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void SetDefaultMenu()
{
	SetMenu(SCHWindow, DefaultMenu);
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 AddKeyFunctionToMenuItem(HMENU SubMenu, int32 SystemFunction)
{
	int32 res, cnt, Length;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	res = GetMenuString(SubMenu, SystemFunction, (LPTSTR) & str, 80, MF_BYCOMMAND);

	if (res == 0)
		return -1;

	res = GetKeyString(str3, SystemFunction, 1);

	if (res == 0)
		return -1;

	memmove(&str3[0], &str3[4], 100);
	Length = strlen(str3);

	for (cnt = 0; cnt < Length; cnt++)
	{
		if (str3[cnt] == '_')
			str3[cnt] = ' ';
	}

	Length = strlen(str);

	for (cnt = 0; cnt < Length; cnt++)
	{
		if (str[cnt] == 9)
			str[cnt] = 0;
	}

	sprintf(str2, "%s\t%s", str, str3);
	ModifyMenu(SubMenu, SystemFunction, MF_BYCOMMAND | MF_STRING, SystemFunction, (LPTSTR) & str2);
	return 0;
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 AddKeyFunctionToMain(int32 KeyFunction)
{
	HMENU SubMenu1, SubMenu2, MainMenu;
	int32 res, cnt, cnt2;

	MainMenu = GetMenu(SCHWindow);

	for (cnt = 0; cnt < 16; cnt++)
	{
		SubMenu1 = GetSubMenu(MainMenu, cnt);

		if (SubMenu1 != 0)
		{
			res = AddKeyFunctionToMenuItem(SubMenu1, KeyFunction);

			if (res == 1)
				return 1;

			for (cnt2 = 0; cnt2 < 32; cnt2++)
			{
				SubMenu2 = GetSubMenu(SubMenu1, cnt2);

				if (SubMenu2 != 0)
				{
					res = AddKeyFunctionToMenuItem(SubMenu2, KeyFunction);

					if (res == 1)
						return 1;
				}
			}
		}
	}

	DrawMenuBar((HWND) MainMenu);
	return 0;
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 UpdateKeysMainMenu(int32 mode)
{
	HMENU SubMenu1, SubMenu2, MainMenu;
	int32 res, cnt, cnt2, cnt3, ok;

	MainMenu = GetMenu(SCHWindow);

	for (cnt = 0; cnt < 16; cnt++)
	{
		if (cnt == 4)
			ok = 1;

		SubMenu1 = GetSubMenu(MainMenu, cnt);

		if (SubMenu1 != 0)
		{
			for (cnt2 = 0; cnt2 < 32; cnt2++)
			{
				SubMenu2 = GetSubMenu(SubMenu1, cnt2);

				if (SubMenu2 != 0)
				{
					for (cnt3 = 0; cnt3 < 32; cnt3++)
					{
						res = (int32) GetMenuItemID(SubMenu2, cnt3);

						if (res != -1)
							AddKeyFunctionToMenuItem(SubMenu1, res);
					}
				}
				else
				{
					res = (int32) GetMenuItemID(SubMenu1, cnt2);

					if (res != -1)
						AddKeyFunctionToMenuItem(SubMenu1, res);
				}
			}
		}
		else
		{
			res = (int32) GetMenuItemID(MainMenu, cnt);

			if (res != -1)
				AddKeyFunctionToMenuItem(SubMenu1, res);
		}
	}

	DrawMenuBar((HWND) MainMenu);
	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void SetSheetMenu()
{
	SetMenu(SCHWindow, SheetMenu);
	UpdateKeysMainMenu(0);
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void SetSymbolMenu()
{
	SetMenu(SCHWindow, SymbolMenu);
	UpdateKeysMainMenu(0);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void KillMenus()
{
	DestroyMenu(DefaultMenu);
	DestroyMenu(SheetMenu);
	DestroyMenu(SymbolMenu);
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
