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
#include "graphics.h"
#include "memory.h"
#include "select.h"
#include "mainloop.h"
#include "stdlib.h"
#include "resource.h"
#include "resource2.h"
#include "geom.h"
#include "toets.h"
#include "stdio.h"
#include "calcdef.h"
#include "utf8.h"

#define PopUpMenuX     30
#define PopUpMenuY     30

HMENU PopUpMenu, MainMenu, GEOMMenu;

HMENU GEOMMenu, GEOMMenu1, GEOMMenu1a, GEOMMenu2, GEOMMenu3, GEOMMenu4, GEOMMenu5, GEOMMenu6, GEOMMenu7, GEOMMenu8,
      GEOMMenu9, GEOMMenu10;

HBITMAP BitMapCircle_F;
HBITMAP BitMapCircle_3;
HBITMAP BitMapCircle_6;
HBITMAP BitMapCircle_C;
HBITMAP BitMapCircle_9;
HBITMAP BitMapCircle_1;
HBITMAP BitMapCircle_2;
HBITMAP BitMapCircle_4;
HBITMAP BitMapCircle_8;
HBITMAP BitMapLine2;
HBITMAP BitMapLine3;
HBITMAP BitMapLine4;
HBITMAP BitMapDimension1;
HBITMAP BitMapDimension2;


// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

void LoadBitMapsPopup()
{
	BitMapCircle_F = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_F));
	BitMapCircle_3 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_3));
	BitMapCircle_6 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_6));
	BitMapCircle_C = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_C));
	BitMapCircle_9 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_9));
	BitMapCircle_1 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_1));
	BitMapCircle_2 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_2));
	BitMapCircle_4 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_4));
	BitMapCircle_8 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_CIRCLE_8));
	BitMapLine2 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_LINE2));
	BitMapLine3 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_LINE3));
	BitMapLine4 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_LINE4));
	BitMapDimension1 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_DIMENSION1));
	BitMapDimension2 = LoadBitmap(GEOMClass.hInstance, MAKEINTRESOURCE(BITMAP_DIMENSION2));
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

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
	DeleteObject(BitMapLine2);
	DeleteObject(BitMapLine3);
	DeleteObject(BitMapLine4);
	DeleteObject(BitMapDimension1);
	DeleteObject(BitMapDimension2);
}


// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 OwnAppendMenu(HMENU Menu, uint32 MenuOptions, uint32 MenuId, LPSTR Text)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	int32 res, ok, Mask;

	ok = MF_STRING;
	Mask = MF_SEPARATOR | MF_BITMAP | MF_OWNERDRAW | MF_POPUP;

	if ((MenuOptions & Mask) == 0)
	{
		if (Text == NULL)
			res = 1;

		sprintf(str, Text);

		if (GetKeyString(str2, MenuId, 0x11) == 1)
			strcat(str, str2);

		AppendMenuUTF8(Menu, MenuOptions, MenuId, str);
	}
	else
		AppendMenuUTF8(Menu, MenuOptions, MenuId, Text);

	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void MenuPopUp()
{
	if (GetNrSelectObjects() > 0)
		MenuPopUpSelected();
	else
		MenuPopUp1();
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

void MenuPopUpSelected()
{
	int32 Layer;
	int32 CheckMenuItem;
	char TextStr[MAX_LENGTH_STRING];
	HMENU PopUpMenu, PopUpMenu1, PopUpMenu1a, PopUpMenu2, PopUpMenu3, PopUpMenu4, PopUpMenu5, PopUpMenu5a, PopUpMenu6,
	      PopUpMenu7;


	PopUpMenu = CreatePopupMenu();
	PopUpMenu1 = CreatePopupMenu();
	PopUpMenu1a = CreatePopupMenu();
	PopUpMenu2 = CreatePopupMenu();
	PopUpMenu3 = CreatePopupMenu();
	PopUpMenu4 = CreatePopupMenu();
	PopUpMenu5 = CreatePopupMenu();
	PopUpMenu5a = CreatePopupMenu();
	PopUpMenu6 = CreatePopupMenu();
	PopUpMenu7 = CreatePopupMenu();
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_MOVE_OBJECTS, SC(93, "Move"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS, SC(33, "Copy"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu7, SC(280, "Copy special"));
	OwnAppendMenu(PopUpMenu7, MF_ENABLED | MF_STRING, ID_COPY_TO_CLIPBOARD, SC(281, "Copy to clipboard"));
	OwnAppendMenu(PopUpMenu7, MF_ENABLED | MF_STRING, ID_COPY_FROM_CLIPBOARD, SC(282, "Copy from clipboard"));
	OwnAppendMenu(PopUpMenu7, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu6, SC(283, "Copy multiple"));
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE2, "2");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE3, "3");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE4, "4");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE5, "5");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE6, "6");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE7, "7");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE8, "8");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE9, "9");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE10, "10");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE11, "11");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE12, "12");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE13, "13");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE14, "14");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE15, "15");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE16, "16");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE17, "17");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE18, "18");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE19, "19");
	OwnAppendMenu(PopUpMenu6, MF_ENABLED | MF_STRING, ID_COPY_OBJECTS_MULTIPLE20, "20");
	OwnAppendMenu(PopUpMenu7, MF_ENABLED | MF_SEPARATOR, 0, NULL);
	OwnAppendMenu(PopUpMenu7, MF_ENABLED | MF_STRING, ID_COPY_COOR, SC(284, "Copy on multiple coordinates"));
	OwnAppendMenu(PopUpMenu7, MF_ENABLED | MF_STRING, ID_COPY_TO_SIL, SC(285, "Add SIL based on selected objects"));
	OwnAppendMenu(PopUpMenu7, MF_ENABLED | MF_STRING, ID_COPY_TO_SILSMD,
	              SC(286, "Add SIL SMD based on selected objects"));
//  OwnAppendMenu(PopUpMenu,MF_ENABLED|MF_STRING,ID_ROT_90_ZERO,"Rotate objects (0,0)");
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu1a, SC(287, "Modify objects"));
	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_ROTATE_OBJECTS, SC(288, "Rotate objects"));
	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_ROTATE_OBJECTS2, SC(289, "Rotate objects at any angle"));
	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_SCALE_OBJECTS, SC(290, "Scale objects"));
//  OwnAppendMenu(PopUpMenu,MF_ENABLED|MF_STRING,ID_ROT_270,"Rotate objects 270");
	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_MIRRORX_OBJECTS, SC(291, "Flip objects x"));
	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_MIRRORY_OBJECTS, SC(292, "Flip objects y"));

	if (NrLinesSelected > 0)
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_LINES_TO_POLYGON, SC(293, "Convert lines into polygon"));

	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_SEPARATOR, 0, NULL);
	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu3, SC(294, "Special move/centering"));
	OwnAppendMenu(PopUpMenu3, MF_ENABLED | MF_STRING, ID_ZERO_CENTER, SC(295, "Mark center selected objects"));
	OwnAppendMenu(PopUpMenu3, MF_ENABLED | MF_STRING, ID_MOVE_OBJECTS2,
	              SC(296, "Move objects centered to previous selected objects"));
	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_RENAME_PIN, SC(291, "Rename pin"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_INFOSELECTEDOBJECTS, SC(297, "Info objects"));

	if (NrPolygonsSelected > 0)
		OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_VERTICES_POLYGON, SC(298, "View vertices polygon"));

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_CALC_DISTANCE, SC(299, "Calculate distance between objects"));
	CheckMenuItem = 0;

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		if (NrPadLayerObjectsSelected[Layer] > 0)
			CheckMenuItem = 1;
	}

	if ((CheckMenuItem) || (NrDrillsSelected > 0))
	{
		OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_ASSIGN_PINS, SC(41, "Assign/Remove pins to objects"));
		OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_ASSIGN_PINS2, SC(42, "Assign pins to objects (Auto increment)"));
		OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_RENAME_PIN, SC(141, "Rename pin"));
	}

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, NULL);
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_DELETE, SC(301, "Delete objects"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, NULL);
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu5, SC(302, "Copy to a different layer"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);
		OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_PADS + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_PASTE_TOP, SC(7, "Paste mask top"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_SOLDER_TOP, SC(5, "Solder mask top"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_PASTE_BOTTOM, SC(8, "Paste mask bottom"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_SOLDER_BOTTOM, SC(6, "Solder mask bottom"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_SILKSCREEN_TOP, SC(9, "Silkscreen top"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_SILKSCREEN_BOTTOM, SC(10, "Silkscreen bottom"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_COMP_OUTLINE, SC(11, "Component outline"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_PLACEMENT_OUTLINE, SC(13, "Placement outline"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_BOARD_OUTLINE, SC(12, "Board outline"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_DRILLS, SC(14, "Drill plated"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_DRILLS_UNPLATED, SC(15, "Drill unplated"));
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_POWER_PADS, SC(16, "Anti power pad"));

	if (NrPadLayers == 2)
		OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_COPY_TO_INNER_PADS, SC(17, "Inner pad"));

	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_INFO1, "Info 1");
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_INFO2, "Info 2");
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_INFO3, "Info 3");
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_INFO4, "Info 4");
	OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_SEPARATOR, 0, 0);

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer + ROUTING_KEEPOUT_LAYER, TextStr, 1);
		OwnAppendMenu(PopUpMenu5, MF_ENABLED | MF_STRING, ID_COPY_TO_ROUTING_KEEPOUT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu5a, SC(303, "Move to a different layer"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);
		OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_PADS + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_PASTE_TOP, SC(7, "Paste mask top"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_SOLDER_TOP, SC(5, "Solder mask top"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_PASTE_BOTTOM, SC(8, "Paste mask bottom"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_SOLDER_BOTTOM, SC(6, "Solder mask bottom"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_SILKSCREEN_TOP, SC(9, "Silkscreen top"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_SILKSCREEN_BOTTOM, SC(10, "Silkscreen bottom"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_COMP_OUTLINE, SC(11, "Component outline"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_PLACEMENT_OUTLINE, SC(13, "Placement outline"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_BOARD_OUTLINE, SC(12, "Board outline"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_DRILLS, SC(14, "Drill plated"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_DRILLS_UNPLATED, SC(15, "Drill unplated"));
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_POWER_PADS, SC(16, "Anti power pad"));

	if (NrPadLayers == 2)
		OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_INNER_PADS, SC(17, "Inner pad"));

	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_INFO1, "Info 1");
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_INFO2, "Info 2");
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_INFO3, "Info 3");
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_INFO4, "Info 4");
	OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_SEPARATOR, 0, 0);

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer + ROUTING_KEEPOUT_LAYER, TextStr, 1);
		OwnAppendMenu(PopUpMenu5a, MF_ENABLED | MF_STRING, ID_MOVE_TO_ROUTING_KEEPOUT + Layer, TextStr);
	}


	CheckMenuItem = 0;

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		if (NrPadLayerObjectsSelected[Layer] > 0)
			CheckMenuItem = 1;
	}

	if ((NrLinesSelected > 0) || (CheckMenuItem) || (NrDrillsUnplatedSelected > 0) || (NrDrillsSelected > 0)
	        || (NrPolygonsSelected > 0))
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_CLEAR_WIDTH, SC(304, "Change clearance"));

	if ((NrRectsSelected > 0) || (NrCirclesSelected > 0) || (NrArcsSelected > 0) || (NrLinesSelected > 0)
	        || (NrTextsSelected > 0))
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_LINE_WIDTH, SC(32, "Change line width"));

	if (NrRectsSelected > 0)
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_CHANGE_RECTS, SC(305, "Change width,height rectangles"));

	if (NrCirclesSelected > 0)
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_CHANGE_CIRCLES, SC(306, "Change diameter Circles"));

	if (NrArcsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_CHANGE_ARCS, SC(307, "Change diameter arc"));
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_CHANGE_ARCS2, SC(308, "Change angles arc"));
	}

	if (NrTextsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_EDIT_TEXT, SC(36, "Change text"));
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_CHANGE_TEXT_HEIGHT, SC(309, "Change text height"));
	}

	OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_EDIT_CUT_FROM_OBJECT, SC(310, "Cut from object"));

	if (NrRectsSelected + NrCirclesSelected + NrArcsSelected + NrPolygonsSelected + NrLinesSelected > 1)
		OwnAppendMenu(PopUpMenu1a, MF_ENABLED | MF_STRING, ID_EDIT_MERGE_OBJECTS, SC(311, "Merge objects to polygon"));

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu1, SC(312, "Select only"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if (NrPadLayerObjectsSelected[Layer] > 0)
			OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_PADS + Layer, TextStr);
	}

	if (NrDrillsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_DRILL, SC(14, "Drill plated"));

	if (NrDrillsUnplatedSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_DRILL_UNPLATED, SC(15, "Drill unplated"));

	if ((NrPadLayers == 2) && (NrPadsInnerSelected > 0))
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_INNERPADS, SC(17, "Inner pad"));

	if (NrAntiPowerpadsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_ANTI_POWERPADS, SC(16, "Anti power pad"));

	//  OwnAppendMenu(PopUpMenu1,MF_ENABLED|MF_STRING,ID_SELECT_ONLY_BUTTERFLY,"Power plane connection");

	if (NrSilkTopObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_SILK_TOP, SC(9, "Silkscreen top"));

	if (NrSilkBottomObjectsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_SILK_BOTTOM, SC(10, "Silkscreen bottom"));
	}

	if (NrCompOutlinesSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_COMP_OUTLINE, SC(11, "Component outline"));
	}

	if (NrPlacemOutlinesSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_PLACEM_OUTLINE, SC(13, "Placement outline"));
	}

	if (NrMaskTopObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_MASK_TOP, SC(5, "Solder mask top"));

	if (NrMaskBottomObjectsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_MASK_BOTTOM, SC(6, "Solder mask bottom"));
	}

	if (NrPasteTopObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_PASTE_TOP, SC(7, "Paste mask top"));

	if (NrPasteBottomObjectsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_PASTE_BOTTOM, SC(8, "Paste mask bottom"));
	}

	if (NrBoardOutlinesSelected > 0)
	{
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_BOARD_OUTLINE, SC(12, "Board outline"));
	}

	if (NrInfo1ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_INFO1, "Info 1");

	if (NrInfo2ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_INFO2, "Info 2");

	if (NrInfo3ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_INFO3, "Info 3");

	if (NrInfo4ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_INFO4, "Info 4");

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer + ROUTING_KEEPOUT_LAYER, TextStr, 1);

		if (NrRoutingKeepoutsSelected[Layer] > 0)
			OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_ROUT_KEEPOUT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenu1, MF_SEPARATOR, 0, 0);

	if (NrLinesSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_LINES, SC(328, "Lines"));

	if (NrRectsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_RECTS, SC(329, "Rectangles"));

	if (NrCirclesSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_CIRCLES, SC(330, "Circles"));

	if (NrArcsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_ARCS, SC(331, "Arcs"));

	if (NrTextsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_TEXT, SC(332, "Texts"));

	if (NrPolygonsSelected > 0)
		OwnAppendMenu(PopUpMenu1, MF_ENABLED | MF_STRING, ID_SELECT_ONLY_TEXT, SC(333, "Polygons"));

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenu2, SC(334, "Unselect"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if (NrPadLayerObjectsSelected[Layer] > 0)
			OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_PADS + Layer, TextStr);
	}

	if ((NrPadLayers == 2) && (NrPadsInnerSelected > 0))
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_INNER_PADS, SC(17, "Inner pad"));

	if (NrDrillsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_DRILL, SC(14, "Drill plated"));

	if (NrDrillsUnplatedSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_DRILL_UNPLATED, SC(15, "Drill unplated"));

	if (NrAntiPowerpadsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_ANTI_POWERPADS, SC(16, "Anti power pad"));

	//  OwnAppendMenu(PopUpMenu2,MF_ENABLED|MF_STRING,ID_UNSELECT_BUTTERFLY,"Power plane connection");

	if (NrSilkTopObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_SILK_TOP, SC(9, "Silkscreen top"));

	if (NrSilkBottomObjectsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_SILK_BOTTOM, SC(10, "Silkscreen bottom"));
	}

	if (NrCompOutlinesSelected > 0)
	{
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_COMP_OUTLINE, SC(11, "Component outline"));
	}

	if (NrPlacemOutlinesSelected > 0)
	{
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_PLACEM_OUTLINE, SC(13, "Placement outline"));
	}

	if (NrMaskTopObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_MASK_TOP, SC(5, "Solder mask top"));

	if (NrMaskBottomObjectsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_MASK_BOTTOM, SC(6, "Solder mask bottom"));
	}

	if (NrPasteTopObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_PASTE_TOP, SC(7, "Paste mask top"));

	if (NrPasteBottomObjectsSelected > 0)
	{
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_PASTE_BOTTOM, SC(8, "Paste mask bottom"));
	}

	if (NrBoardOutlinesSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_BOARD_OUTLINE, SC(12, "Board outline"));

	if (NrInfo1ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_INFO1, "Info 1");

	if (NrInfo2ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_INFO2, "Info 2");

	if (NrInfo3ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_INFO3, "Info 3");

	if (NrInfo4ObjectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_INFO4, "Info 4");

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer + ROUTING_KEEPOUT_LAYER, TextStr, 1);

		if (NrRoutingKeepoutsSelected[Layer] > 0)
			OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_ROUT_KEEPOUT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenu2, MF_SEPARATOR, 0, 0);

	if (NrLinesSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_LINES, SC(328, "Lines"));

	if (NrRectsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_RECTS, SC(329, "Rectangles"));

	if (NrCirclesSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_CIRCLES, SC(330, "Circles"));

	if (NrArcsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_ARCS, SC(331, "Arcs"));

	if (NrTextsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_TEXT, SC(332, "Texts"));

	if (NrPolygonsSelected > 0)
		OwnAppendMenu(PopUpMenu2, MF_ENABLED | MF_STRING, ID_UNSELECT_POLYGONS, SC(333, "Polygons"));

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_UNSELECT_FIRST_OBJECT, SC(129, "Unselect first object"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, NULL);
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_UNSELECT_ALL, SC(109, "Unselect all"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(45, "Exit"));
	TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
	               RealWindow.top + MousePosY + 40, 0, GEOMWindow, NULL);
	DestroyMenu(PopUpMenu);
	DestroyMenu(PopUpMenu1);
	DestroyMenu(PopUpMenu1a);
	DestroyMenu(PopUpMenu2);
	DestroyMenu(PopUpMenu3);
	DestroyMenu(PopUpMenu4);
	DestroyMenu(PopUpMenu5);
	DestroyMenu(PopUpMenu5a);
	DestroyMenu(PopUpMenu6);
	DestroyMenu(PopUpMenu7);
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************


void MenuPopUp1()
{

	int32 Layer;
	char str[MAX_LENGTH_STRING], TextStr[MAX_LENGTH_STRING];

	HMENU PopUpMenu, PopUpMenuPad, PopUpMenuPadRect, PopUpMenuPadCircle, PopUpMenuLine, PopUpMenuRect, PopUpMenuRect2,
	      PopUpMenuCircle, PopUpMenuCircleSilk, PopUpMenuCircleSilk2, PopUpMenuCirclePlace, PopUpMenuCircleComp,
	      PopUpMenuArc, PopUpMenuText, PopUpMenuCircleBoardOutline, PopUpMenuCircleInfo1, PopUpMenuCircleInfo2,
	      PopUpMenuCircleInfo3, PopUpMenuCircleInfo4, PopUpMenuTraceSold, PopUpMenuTraceComp, PopUpMenuTrace,
	      PopUpMenuPolygon, PopUpMenuPolyline, PopUpMenuDrill, PopUpMenuArrow1, PopUpMenuArrow2, PopUpMenuArrow3,
	      PopUpMenuDimension1, PopUpMenuDimension2, PopUpMenuArrowDimension;


	LoadBitMapsPopup();

	PopUpMenu = CreatePopupMenu();
	PopUpMenuPad = CreatePopupMenu();
	PopUpMenuPadRect = CreatePopupMenu();
	PopUpMenuPadCircle = CreatePopupMenu();
	PopUpMenuLine = CreatePopupMenu();
	PopUpMenuRect = CreatePopupMenu();
	PopUpMenuRect2 = CreatePopupMenu();
	PopUpMenuCircle = CreatePopupMenu();
	PopUpMenuCircleSilk = CreatePopupMenu();
	PopUpMenuCircleSilk2 = CreatePopupMenu();
	PopUpMenuCirclePlace = CreatePopupMenu();
	PopUpMenuCircleComp = CreatePopupMenu();
	PopUpMenuCircleBoardOutline = CreatePopupMenu();
	PopUpMenuCircleInfo1 = CreatePopupMenu();
	PopUpMenuCircleInfo2 = CreatePopupMenu();
	PopUpMenuCircleInfo3 = CreatePopupMenu();
	PopUpMenuCircleInfo4 = CreatePopupMenu();
	PopUpMenuArc = CreatePopupMenu();
	PopUpMenuText = CreatePopupMenu();
	PopUpMenuTraceSold = CreatePopupMenu();
	PopUpMenuTraceComp = CreatePopupMenu();
	PopUpMenuTrace = CreatePopupMenu();
	PopUpMenuDrill = CreatePopupMenu();
	PopUpMenuPolygon = CreatePopupMenu();
	PopUpMenuPolyline = CreatePopupMenu();
	PopUpMenuArrow1 = CreatePopupMenu();
	PopUpMenuArrow2 = CreatePopupMenu();
	PopUpMenuArrow3 = CreatePopupMenu();
	PopUpMenuDimension1 = CreatePopupMenu();
	PopUpMenuDimension2 = CreatePopupMenu();
	PopUpMenuArrowDimension = CreatePopupMenu();

// *****************************************************************************************
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuPad, SC(338, "Add pad"));
//  OwnAppendMenu(PopUpMenuPad,MF_ENABLED|MF_STRING,ID_ADD_THROUGHOLE,"Through hole");
	OwnAppendMenu(PopUpMenuPad, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuPadRect, SC(339, "Rectangle"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if ((!CheckIfInnerLayer(Layer)) && (PadsVisible[Layer] != 0))
			OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_SEPARATOR, 0, 0);

	if (PastePadsTopVisible)
	{
		Layer = PASTE_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, SC(7, "Paste mask top"));
	}

	if (SoldMaskPadsTopVisible)
	{
		Layer = SOLD_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, SC(5, "Solder mask top"));
	}

	if (PastePadsBottomVisible)
	{
		Layer = PASTE_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, SC(8, "Paste mask bottom"));
	}

	if (SoldMaskPadsBottomVisible)
	{
		Layer = SOLD_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, SC(6, "Solder mask bottom"));
	}

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_PAD_OBJECT + Layer, "Info 4");
	}

	OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_SEPARATOR, 0, 0);

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer + ROUTING_KEEPOUT_LAYER, TextStr, 1);

		if (RoutingKeepoutVisible[Layer] != 0)
		{
			OwnAppendMenu(PopUpMenuPadRect, MF_ENABLED | MF_STRING, ID_ADD_RECT_OBJECT + ROUTING_KEEPOUT_LAYER + Layer,
			              TextStr);
		}
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenuPad, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuPadCircle, SC(340, "Circle"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if (PadsVisible[Layer] != 0)
			OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, TextStr);
	}

	if (NrPadLayers == 2)
	{
		Layer = INNER_PAD_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(17, "Inner pad"));
	}

	OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_SEPARATOR, 0, 0);

	if (PowerPadsVisible)
	{
		Layer = POWER_PAD_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(16, "Anti power pad"));
	}

	if (PastePadsTopVisible)
	{
		Layer = PASTE_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(7, "Paste mask top"));
	}

	if (SoldMaskPadsTopVisible)
	{
		Layer = SOLD_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(5, "Solder mask top"));
	}

	if (PastePadsBottomVisible)
	{
		Layer = PASTE_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(8, "Paste mask bottom"));
	}

	if (SoldMaskPadsBottomVisible)
	{
		Layer = SOLD_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(6, "Solder mask bottom"));
	}

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + Layer, "Info 4");
	}

	OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_SEPARATOR, 0, 0);

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer + ROUTING_KEEPOUT_LAYER, TextStr, 1);

		if (RoutingKeepoutVisible[Layer] != 0)
		{
			OwnAppendMenu(PopUpMenuPadCircle, MF_ENABLED | MF_STRING,
			              ID_ADD_CIRCLE_PAD_OBJECT + ROUTING_KEEPOUT_LAYER + Layer, TextStr);
		}
	}

// *****************************************************************************************

	OwnAppendMenu(PopUpMenuPad, MF_ENABLED | MF_STRING, ID_CREATE_SIL, SC(104, "SIP (SIL) THT square/circle"));
	OwnAppendMenu(PopUpMenuPad, MF_ENABLED | MF_STRING, ID_CREATE_SIL_SMD_RECT, SC(84, "SIP (SIL) SMD rectangle"));
	OwnAppendMenu(PopUpMenuPad, MF_ENABLED | MF_STRING, ID_CREATE_SIL_SMD_CIRCLE, SC(85, "SIP (SIL) SMD circle"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuLine, SC(345, "Add line"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if (PadsVisible[Layer] != 0)
			OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_SEPARATOR, 0, 0);

	if (SoldMaskPadsTopVisible)
	{
		Layer = SOLD_MASK_TOP_LAYER;
		sprintf(str, SC(5, "Solder mask top"));
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, str);
	}

	if (PastePadsTopVisible)
	{
		Layer = PASTE_MASK_TOP_LAYER;
		sprintf(str, SC(7, "Paste mask top"));
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, str);
	}

	if (PastePadsBottomVisible)
	{
		Layer = PASTE_MASK_BOTTOM_LAYER;
		sprintf(str, SC(8, "Paste mask bottom"));
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, str);
	}

	if (SoldMaskPadsBottomVisible)
	{
		Layer = SOLD_MASK_BOTTOM_LAYER;
		sprintf(str, SC(6, "Solder mask bottom"));
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, str);
	}

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (BoardOutlineVisible)
	{
		Layer = BOARD_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, SC(12, "Board outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuLine, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT + Layer, "Info 4");
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuArrowDimension, SC(346, "Add arrow/dimension"));
	OwnAppendMenu(PopUpMenuArrowDimension, MF_ENABLED | MF_BITMAP | MF_POPUP, (uint32) PopUpMenuArrow1,
	              (LPSTR) BitMapLine2);

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, SC(10, "Silkscreen bottom"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, SC(11, "Component outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuArrow1, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW1 + Layer, "Info 4");
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenuArrowDimension, MF_ENABLED | MF_BITMAP | MF_POPUP, (uint32) PopUpMenuArrow2,
	              (LPSTR) BitMapLine3);

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, SC(10, "Silkscreen bottom"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, SC(11, "Component outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuArrow2, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW2 + Layer, "Info 4");
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenuArrowDimension, MF_ENABLED | MF_BITMAP | MF_POPUP, (uint32) PopUpMenuArrow3,
	              (LPSTR) BitMapLine4);

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, SC(10, "Silkscreen bottom"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, SC(11, "Component outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuArrow3, MF_ENABLED | MF_STRING, ID_ADD_LINE_OBJECT_ARROW3 + Layer, "Info 4");
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenuArrowDimension, MF_ENABLED | MF_BITMAP | MF_POPUP, (uint32) PopUpMenuDimension1,
	              (LPSTR) BitMapDimension1);

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuDimension1, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT + Layer, "Info 4");
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenuArrowDimension, MF_ENABLED | MF_BITMAP | MF_POPUP, (uint32) PopUpMenuDimension2,
	              (LPSTR) BitMapDimension2);

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, SC(10, "Silkscreen bottom"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, SC(11, "Component outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuDimension2, MF_ENABLED | MF_STRING, ID_ADD_DIMENSION_OBJECT2 + Layer, "Info 4");
	}

// *****************************************************************************************
//  OwnAppendMenu(PopUpMenu,MF_ENABLED|MF_POPUP,(uint32) PopUpMenuRect,SC(347,"Add rectangle"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuRect2, SC(348, "Add rectangle"));

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,SC(9,"Silkscreen top"));
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,SC(10,"Silkscreen bottom"));
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (BoardOutlineVisible)
	{
		Layer = BOARD_OUTLINE_LAYER;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,SC(12,"Board outline"));
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, SC(12, "Board outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,"Info 1");
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,"Info 2");
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,"Info 3");
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,"Info 4");
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, "Info 4");
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,SC(13,"Placement outline"));
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
//    OwnAppendMenu(PopUpMenuRect,MF_ENABLED|MF_STRING,ID_ADD_RECT_OBJECT+Layer,SC(11,"Component outline"));
		OwnAppendMenu(PopUpMenuRect2, MF_ENABLED | MF_STRING, ID_ADD_RECT2_OBJECT + Layer, SC(11, "Component outline"));
	}

// *****************************************************************************************

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircle, SC(349, "Add circle"));

	if (SilkScreenTopVisible)
	{
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleSilk, SC(9, "Silkscreen top"));
		Layer = SILKSCREEN_TOP_LAYER;

		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleSilk, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (SilkScreenBottomVisible)
	{
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleSilk2, SC(10, "Silkscreen bottom"));
		Layer = SILKSCREEN_BOTTOM_LAYER;

		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleSilk2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCirclePlace, SC(13, "Placement outline"));
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCirclePlace, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleComp, SC(11, "Component outline"));
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleComp, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (BoardOutlineVisible)
	{
		Layer = BOARD_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleBoardOutline, SC(12, "Board outline"));

		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleBoardOutline, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleInfo1, "Info 1");

		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleInfo1, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleInfo2, "Info 2");

		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleInfo2, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleInfo3, "Info 3");

		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleInfo3, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuCircle, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuCircleInfo4, "Info 4");

		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_F + Layer,
		              (LPSTR) BitMapCircle_F);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_3 + Layer,
		              (LPSTR) BitMapCircle_3);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_6 + Layer,
		              (LPSTR) BitMapCircle_6);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_C + Layer,
		              (LPSTR) BitMapCircle_C);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_9 + Layer,
		              (LPSTR) BitMapCircle_9);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_1 + Layer,
		              (LPSTR) BitMapCircle_1);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_2 + Layer,
		              (LPSTR) BitMapCircle_2);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_4 + Layer,
		              (LPSTR) BitMapCircle_4);
		OwnAppendMenu(PopUpMenuCircleInfo4, MF_ENABLED | MF_BITMAP, ID_ADD_CIRCLE_OBJECT_8 + Layer,
		              (LPSTR) BitMapCircle_8);
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuArc, SC(350, "Add arc"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if ((!CheckIfInnerLayer(Layer)) && (PadsVisible[Layer] != 0))
			OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_SEPARATOR, 0, 0);

	if (PastePadsTopVisible)
	{
		Layer = PASTE_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(7, "Paste mask top"));
	}

	if (SoldMaskPadsTopVisible)
	{
		Layer = SOLD_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(5, "Solder mask top"));
	}

	if (PastePadsBottomVisible)
	{
		Layer = PASTE_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(8, "Paste mask bottom"));
	}

	if (SoldMaskPadsBottomVisible)
	{
		Layer = SOLD_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(6, "Solder mask bottom"));
	}

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(13, "Placement outline"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (BoardOutlineVisible)
	{
		Layer = BOARD_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, SC(12, "Board outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuArc, MF_ENABLED | MF_STRING, ID_ADD_ARC_OBJECT + Layer, "Info 4");
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuText, SC(35, "Add text"));

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

//  if (PlacementVisible) {
//    OwnAppendMenu(PopUpMenuText,MF_ENABLED|MF_STRING,ID_ADD_TEXT_PLACE_OUTL,SC(13,"Placement outline"));
//  }
	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (SoldMaskPadsTopVisible)
	{
		Layer = SOLD_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, SC(5, "Solder mask top"));
	}

	if (SoldMaskPadsBottomVisible)
	{
		Layer = SOLD_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, SC(6, "Solder mask bottom"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuText, MF_ENABLED | MF_STRING, ID_ADD_TEXT_OBJECT + Layer, "Info 4");
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuPolygon, SC(351, "Add polygon"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if ((!CheckIfInnerLayer(Layer)) && (PadsVisible[Layer] != 0))
			OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_SEPARATOR, 0, 0);

	if (PastePadsTopVisible)
	{
		Layer = PASTE_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, SC(7, "Paste mask top"));
	}

	if (SoldMaskPadsTopVisible)
	{
		Layer = SOLD_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, SC(5, "Solder mask top"));
	}

	if (PastePadsBottomVisible)
	{
		Layer = PASTE_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, SC(8, "Paste mask bottom"));
	}

	if (SoldMaskPadsBottomVisible)
	{
		Layer = SOLD_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, SC(6, "Solder mask bottom"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING, ID_ADD_POLYGON_OBJECT + Layer, "Info 4");
	}

	OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_SEPARATOR, 0, 0);

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer + ROUTING_KEEPOUT_LAYER, TextStr, 1);

		if (RoutingKeepoutVisible[Layer] != 0)
		{
			OwnAppendMenu(PopUpMenuPolygon, MF_ENABLED | MF_STRING,
			              ID_ADD_POLYGON_OBJECT + ROUTING_KEEPOUT_LAYER + Layer, TextStr);
		}
	}

// *****************************************************************************************
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuPolyline, SC(352, "Add polyline"));

	for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
	{
		GetLayerText(Layer, TextStr, 0);

		if ((!CheckIfInnerLayer(Layer)) && (PadsVisible[Layer] != 0))
			OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, TextStr);
	}

	OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_SEPARATOR, 0, 0);

	if (PastePadsTopVisible)
	{
		Layer = PASTE_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(7, "Paste mask top"));
	}

	if (SoldMaskPadsTopVisible)
	{
		Layer = SOLD_MASK_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(5, "Solder mask top"));
	}

	if (PastePadsBottomVisible)
	{
		Layer = PASTE_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(8, "Paste mask bottom"));
	}

	if (SoldMaskPadsBottomVisible)
	{
		Layer = SOLD_MASK_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(6, "Solder mask bottom"));
	}

	if (CompOutlineVisible)
	{
		Layer = COMP_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(11, "Component outline"));
	}

	if (PlacementVisible)
	{
		Layer = PLACEMENT_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(13, "Placement outline"));
	}

	if (SilkScreenTopVisible)
	{
		Layer = SILKSCREEN_TOP_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(9, "Silkscreen top"));
	}

	if (SilkScreenBottomVisible)
	{
		Layer = SILKSCREEN_BOTTOM_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(10, "Silkscreen bottom"));
	}

	if (BoardOutlineVisible)
	{
		Layer = BOARD_OUTLINE_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, SC(12, "Board outline"));
	}

	if (Info1Visible)
	{
		Layer = INFO_LAYER;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, "Info 1");
	}

	if (Info2Visible)
	{
		Layer = INFO_LAYER2;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, "Info 2");
	}

	if (Info3Visible)
	{
		Layer = INFO_LAYER3;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, "Info 3");
	}

	if (Info4Visible)
	{
		Layer = INFO_LAYER4;
		OwnAppendMenu(PopUpMenuPolyline, MF_ENABLED | MF_STRING, ID_ADD_POLYLINE_OBJECT + Layer, "Info 4");
	}

// *****************************************************************************************

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_POPUP, (uint32) PopUpMenuDrill, SC(136, "Add drill hole"));

	if (DrillVisible)
	{
		OwnAppendMenu(PopUpMenuDrill, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + DRILL_LAYER, SC(137, "Plated"));
	}

	if (DrillUnplatedVisible)
	{
		OwnAppendMenu(PopUpMenuDrill, MF_ENABLED | MF_STRING, ID_ADD_CIRCLE_PAD_OBJECT + DRILL_UNPLATED_LAYER, SC(138, "Unplated"));
	}

    //OwnAppendMenu(PopUpMenu,MF_ENABLED|MF_STRING,ID_ADD_ROUTING_KEEPOUT,"Routing keepout");

	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_UNSELECT_ALL, SC(109, "Unselect all"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_GOTOXY, SC(128, "Goto x,y"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_MEASUREMENT, SC(30, "Measurement"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_PREVIOUS_VIEW, SC(356, "Previous view"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_VIEWFULL, SC(92, "View whole design"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_VIEW_REPAINT, SC(358, "Repaint"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_UNDO, SC(90, "Undo"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_EDIT_REDO, SC(91, "Redo"));
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(45, "Exit"));


	TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
	               RealWindow.top + MousePosY + 40, 0, GEOMWindow, NULL);

	DestroyBitMapsPopup();
	DestroyMenu(PopUpMenuArrow1);
	DestroyMenu(PopUpMenuArrow2);
	DestroyMenu(PopUpMenuArrow3);
	DestroyMenu(PopUpMenuDimension1);
	DestroyMenu(PopUpMenuDimension2);
	DestroyMenu(PopUpMenuArrowDimension);
	DestroyMenu(PopUpMenu);
	DestroyMenu(PopUpMenuPad);
	DestroyMenu(PopUpMenuPadRect);
	DestroyMenu(PopUpMenuPadCircle);
	DestroyMenu(PopUpMenuLine);
	DestroyMenu(PopUpMenuRect);
	DestroyMenu(PopUpMenuRect2);
	DestroyMenu(PopUpMenuCircle);
	DestroyMenu(PopUpMenuCircleSilk);
	DestroyMenu(PopUpMenuCircleSilk2);
	DestroyMenu(PopUpMenuCirclePlace);
	DestroyMenu(PopUpMenuCircleComp);
	DestroyMenu(PopUpMenuArc);
	DestroyMenu(PopUpMenuText);
	DestroyMenu(PopUpMenuTraceSold);
	DestroyMenu(PopUpMenuTraceComp);
	DestroyMenu(PopUpMenuTrace);
	DestroyMenu(PopUpMenuDrill);
	DestroyMenu(PopUpMenuCircleBoardOutline);
	DestroyMenu(PopUpMenuCircleInfo1);
	DestroyMenu(PopUpMenuCircleInfo2);
	DestroyMenu(PopUpMenuCircleInfo3);
	DestroyMenu(PopUpMenuCircleInfo4);
	DestroyMenu(PopUpMenuPolygon);
	DestroyMenu(PopUpMenuPolyline);
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

void ChangeMenuSelectionMode()
{
	HMENU EditMenu;

	MainMenu = GetMenu(GEOMWindow);
	EditMenu = GetSubMenu(MainMenu, 4);
	CheckMenuItem(EditMenu, ID_MODE_SELECTION_APPEND, MF_BYCOMMAND | MF_UNCHECKED);
	CheckMenuItem(EditMenu, ID_MODE_SELECTION_REPLACE, MF_BYCOMMAND | MF_UNCHECKED);

	if (ReplaceSelections == 1)
		CheckMenuItem(EditMenu, ID_MODE_SELECTION_REPLACE, MF_BYCOMMAND | MF_CHECKED);
	else
		CheckMenuItem(EditMenu, ID_MODE_SELECTION_APPEND, MF_BYCOMMAND | MF_CHECKED);

	DrawMenuBar(GEOMWindow);
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

void ChangeMenuRelativeMousePosition()
{
	HMENU EditMenu;

	MainMenu = GetMenu(GEOMWindow);
	EditMenu = GetSubMenu(MainMenu, 3);
	CheckMenuItem(EditMenu, ID_VIEW_RELATIVEPOSITIONONGRID, MF_BYCOMMAND | MF_UNCHECKED);

	if (MouseCursorOnGrid == 1)
		CheckMenuItem(EditMenu, ID_VIEW_RELATIVEPOSITIONONGRID, MF_BYCOMMAND | MF_CHECKED);

	DrawMenuBar(GEOMWindow);
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

void MakeMainMenu()
{

#define MENU_ID       MF_ENABLED|MF_STRING

	GEOMMenu = CreateMenu();

// *****************************************************************************************
	GEOMMenu1 = CreateMenu();
	OwnAppendMenu(GEOMMenu, MF_ENABLED | MF_POPUP, (uint32) GEOMMenu1, SC(436, "File"));
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_FILE_NEW, SC(1, "New geometry"));
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_FILE_OPEN, SC(2, "Open"));
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_FILE_SAVE, SC(98, "Save"));
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_FILE_SAVEAS, SC(99, "Save as"));
	GEOMMenu1a = CreateMenu();
//	OwnAppendMenu(GEOMMenu1, MF_ENABLED | MF_POPUP, (uint32) GEOMMenu1a, SC(100, "Save with errors"));
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_SAVE_WITH_ERRORS, SC(100, "Save with errors")); //GEOMMenu1a

	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_IMPORT_PIN_BITMAP, SC(165, "Import bitmap"));
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_IMPORT_DXF, SC(166, "Import DXF file"));
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_EXPORT_DXF, SC(167, "Export to DXF"));

	OwnAppendMenu(GEOMMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_FILE_PRINT, SC(363, "Print"));
	OwnAppendMenu(GEOMMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(GEOMMenu1, MENU_ID, ID_FILE_EXIT, SC(45, "Exit"));

// *****************************************************************************************
	GEOMMenu2 = CreateMenu();
	OwnAppendMenu(GEOMMenu, MF_ENABLED | MF_POPUP, (uint32) GEOMMenu2, SC(437, "Edit"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_UNDO, SC(90, "Undo"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_REDO, SC(91, "Redo"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_MOVE_ORIGIN, SC(365, "Set origin"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_MOVE_ORIGIN2, SC(366, "Set origin to center selected objects"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_INSERTPOINT, SC(367, "Set insertion point"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_INSERTPOINT2, SC(368, "Set insertion point to center selected objects"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_SETTINGS_THICKNESS, SC(117, "Rules of geometry"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_ASSIGN_PINS, SC(41, "Assign/Remove pins to objects"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_ASSIGN_PINS2, SC(42, "Assign pins to objects (Auto increment)"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_GEOMNAME, SC(37, "Change geometry name"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_CHECK_GEOMETRY, SC(370, "Check geometry"));
	/*
	  OwnAppendMenu(GEOMMenu2 ,MENU_ID,ID_EDIT_COPYTOSILKSCREEN          ,"Copy to silkscreen");
	*/
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_NRLAYERS, SC(371, "Nr layers"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_SHAPEHEIGHT, SC(105, "Geometry height"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_PAD_RULES, SC(372, "Pad rules"));
	OwnAppendMenu(GEOMMenu2, MENU_ID, ID_EDIT_ZERORELATIVECURSOR, SC(440, "Zero relative cursor"));

// *****************************************************************************************
	GEOMMenu3 = CreateMenu();
	OwnAppendMenu(GEOMMenu, MF_ENABLED | MF_POPUP, (uint32) GEOMMenu3, SC(438, "Units"));
	OwnAppendMenu(GEOMMenu3, MENU_ID, ID_SETTINGS_UNITS_MILS, "thou");
	OwnAppendMenu(GEOMMenu3, MENU_ID, ID_SETTINGS_UNITS_MM, "mm");

// *****************************************************************************************
	GEOMMenu4 = CreateMenu();
	OwnAppendMenu(GEOMMenu, MF_ENABLED | MF_POPUP, (uint32) GEOMMenu4, SC(376, "View"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_ZOOMIN, SC(377, "Zoom in"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_ZOOMOUT, SC(378, "Zoom out"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_PAN, SC(379, "Pan window"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_VIEWFULL, SC(92, "View whole design"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_REPAINT, SC(358, "Repaint"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_PREVIOUS_VIEW, SC(356, "Previous view"));
	OwnAppendMenu(GEOMMenu4, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_CHANGEGRID, SC(381, "Change grid"));
	OwnAppendMenu(GEOMMenu4, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_INFOSELECTEDOBJECTS, SC(96, "Info selected objects"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_LAYERS, SC(94, "Select visible layers/objects"));
	OwnAppendMenu(GEOMMenu4, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_CHANGECOLORS, SC(382, "Change colors"));
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_LOADDEFAULTCOLORS, SC(383, "Load default colors"));
	OwnAppendMenu(GEOMMenu4, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(GEOMMenu4, MENU_ID, ID_VIEW_RELATIVEPOSITIONONGRID, SC(384, "Relative position on grid"));


// *****************************************************************************************
	GEOMMenu5 = CreateMenu();
	OwnAppendMenu(GEOMMenu, MF_ENABLED | MF_POPUP, (uint32) GEOMMenu5, SC(385, "Selection mode"));
	OwnAppendMenu(GEOMMenu5, MENU_ID, ID_MODE_SELECTION_REPLACE, SC(386, "Replacement"));
	OwnAppendMenu(GEOMMenu5, MENU_ID, ID_MODE_SELECTION_APPEND, SC(387, "Appending"));

// *****************************************************************************************
	GEOMMenu6 = CreateMenu();
	OwnAppendMenu(GEOMMenu, MF_ENABLED | MF_POPUP, (uint32) GEOMMenu6, SC(47, "Help"));
	OwnAppendMenu(GEOMMenu6, MENU_ID, ID_HELP_TOPICS, SC(388, "Topics"));
	OwnAppendMenu(GEOMMenu6, MENU_ID, ID_HELP_CONTENTS, SC(389, "Getting started"));
	OwnAppendMenu(GEOMMenu6, MENU_ID, ID_HELP_ON_COMMAND, SC(390, "Help on current mode (command)"));
	OwnAppendMenu(GEOMMenu6, MF_ENABLED | MF_SEPARATOR, 0, 0);
	OwnAppendMenu(GEOMMenu6, MENU_ID, ID_HELP_ABOUT, SC(127, "About program"));

// *****************************************************************************************
	SetMenu(GEOMWindow, GEOMMenu);
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
