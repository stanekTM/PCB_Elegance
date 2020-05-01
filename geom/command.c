/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: command.c
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
#include "windows.h"
#include "string.h"
#include "stdlib.h"
#include "math.h"
#include "stdio.h"
#include "keyswin.h"
#include "toets.h"
#include "commdlg.h"
#include "geom.h"
#include "calc.h"
#include "calcdef.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "mainloop.h"
#include "files.h"
#include "files2.h"
#include "edit.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "print.h"
#include "help.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "resource.h"
#include "resource2.h"
#include "command.h"
#include "insdel.h"
#include "dialogs.h"
#include "graphics.h"
#include "movecomp.h"
#include "select.h"
#include "graphics.h"
#include "dxf.h"
#include "polygon.h"
#include "utf8.h"

int32 TraceBackWardsKeyPressed = 0;

char DirPath[MAX_LENGTH_STRING];

extern int32 LinesAllDirection, ProjectActive;
extern ProjectInfoRecord *ProjectInfo;
extern char LibraryFile[MAX_LENGTH_STRING], ProjectPath[MAX_LENGTH_STRING];


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GEOMCommand(int32 WParam, int32 LParam)
{
	int32 Layer, cnt, res, OldUnits;
	double x1, y1, x2, y2, val, LineWidth, LineAngle, LineLength;
	float value;
	ObjectRecord *Object, TypeObject;
	ObjectRecord2 TypeObject2;
	HMENU MainMenu, SubMenu;

    //sprintf(str,"WParam %08X , LParam %08X",WParam,LParam);
    //MessageBoxUTF8(GEOMWindow,str,"Params",MB_APPLMODAL|MB_OK);

	if (SystemBusyMode != 0)
		res = 1;

	switch (WParam)
	{
	case ID_ESCAPE:
	case ID_VIEW_ZOOMIN:
	case ID_VIEW_ZOOMOUT:
	case ID_VIEW_VIEWFULL:
	case ID_VIEW_PREVIOUS_VIEW:
	case ID_VIEW_PAN:
	case ID_VIEW_REPAINT:
	case ID_VIEW_GRIDONOFF:
	case ID_VIEW_CHANGEGRID:
	case ID_EDIT_ZERORELATIVECURSOR:
	case ID_HELP_ON_COMMAND:
	case ID_SETTINGS_CHANGE_UNITS:
	case ID_SETTINGS_UNITS_MM:
	case ID_SETTINGS_UNITS_MILS:
	case ID_LINES_ALL_DIR:
	case ID_LINES_45_DIR:
	case ID_FINISH_POLYGON:
	case ID_TRACE_BACKWARDS:
	case ID_CHANGE_SNAP_MODE_ON:
	case ID_CHANGE_SNAP_MODE_OFF:
	case ID_DRAW_CROSS_HAIR_OFF:
	case ID_DRAW_CROSS_HAIR_ON:
		break;
	
	default:
		if (SystemBusyMode != 0)
			return;
	}


	if ((WParam >= ID_SETTINGS_GRID) && (WParam < ID_SETTINGS_GRID + 100))
	{
		GridSize = GridSizes[WParam - ID_SETTINGS_GRID];
		InvalidateRect(GEOMWindow, NULL, 0);
		PostMessage(GEOMWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
	}

 // ********************************************************************************************************
 // ********************************************************************************************************

	if ((WParam >= ID_ADD_LINE_OBJECT) && (WParam < ID_ADD_LINE_OBJECT + 0x400000))
	{

     // Layer range
     // 100  .. 12700  ( increments 100)
     // 101  .. 12701  ( increments 100)

		Layer = WParam & 0xFFFF;
		LineWidth = (8 * 2540);

		switch (Layer)
		{
		case SILKSCREEN_BOTTOM_LAYER:
		case SILKSCREEN_TOP_LAYER:
			LineWidth = CurrentSilkscreenLine;
			break;

		case COMP_OUTLINE_LAYER:
			LineWidth = CurrentCompOutLine;
			break;

		case BOARD_OUTLINE_LAYER:
			LineWidth = CurrentBoardOutLine;
			break;

		case PLACEMENT_OUTLINE_LAYER:
			LineWidth = 0.0;
			break;

		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
			LineWidth = CurrentInfoLine;
			break;

		case SOLD_MASK_BOTTOM_LAYER:
		case SOLD_MASK_TOP_LAYER:
		case PASTE_MASK_BOTTOM_LAYER:
		case PASTE_MASK_TOP_LAYER:
			LineWidth = TraceThickness;
			break;

		default:
			if (Layer < 32)
				LineWidth = TraceThickness;
			else
			{
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					LineWidth = 0.0;
			}

			break;
		}

		switch (WParam & 0x3FF0000)
		{
		case ID_ADD_LINE_OBJECT:
			CommandAddObjects(OBJECT_LINE, LineWidth, Layer, 0);
			break;

		case ID_ADD_LINE_OBJECT_ARROW1:
			CommandAddObjects(OBJECT_LINE, LineWidth, Layer, 1);
			break;

		case ID_ADD_LINE_OBJECT_ARROW2:
			CommandAddObjects(OBJECT_LINE, LineWidth, Layer, 2);
			break;

		case ID_ADD_LINE_OBJECT_ARROW3:
			CommandAddObjects(OBJECT_LINE, LineWidth, Layer, 3);
			break;

		case ID_ADD_DIMENSION_OBJECT:
			CommandAddObjects(OBJECT_LINE, LineWidth, Layer, 4);
			break;

		case ID_ADD_DIMENSION_OBJECT2:
			CommandAddObjects(OBJECT_LINE, LineWidth, Layer, 8);
			break;

		case ID_ADD_RECT_PAD_OBJECT:
			CommandAddObjects(OBJECT_RECT, 0.0, Layer, 0);
			break;

		case ID_ADD_RECT_OBJECT:
			CommandAddObjects(OBJECT_RECT, LineWidth, Layer, 0);
			break;

		case ID_ADD_RECT2_OBJECT:
			CommandAddObjects(OBJECT_RECT, LineWidth, Layer, 2);
			break;

		case ID_ADD_CIRCLE_PAD_OBJECT:
			CommandAddObjects(OBJECT_CIRCLE, 0.0, Layer, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_1:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 1);
			break;

		case ID_ADD_CIRCLE_OBJECT_2:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 2);
			break;

		case ID_ADD_CIRCLE_OBJECT_3:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 3);
			break;

		case ID_ADD_CIRCLE_OBJECT_4:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 4);
			break;

		case ID_ADD_CIRCLE_OBJECT_6:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 6);
			break;

		case ID_ADD_CIRCLE_OBJECT_8:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 8);
			break;

		case ID_ADD_CIRCLE_OBJECT_9:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 9);
			break;

		case ID_ADD_CIRCLE_OBJECT_C:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 12);
			break;

		case ID_ADD_CIRCLE_OBJECT_F:
			CommandAddObjects(OBJECT_CIRCLE, LineWidth, Layer, 15);
			break;

		case ID_ADD_ARC_OBJECT:
			CommandAddObjects(OBJECT_ARC, LineWidth, Layer, 0);
			break;

		case ID_ADD_TEXT_OBJECT:
			CommandAddObjects(OBJECT_TEXT, LineWidth, Layer, 0);
			break;

		case ID_ADD_POLYGON_OBJECT:
			CommandAddObjects(OBJECT_POLYGON, 0.0, Layer, 0);
			break;

		case ID_ADD_POLYLINE_OBJECT:
			CommandAddObjects(OBJECT_POLYGON, LineWidth, Layer, 1);
			break;
		}

		return;
	}

 // ********************************************************************************************************
 // ********************************************************************************************************

	switch (WParam)
	{
	case ID_FILE_NEW:
		res = SelectNewGeomtrieDialog(0);

		if (res != -1)
		{
			switch (res)
			{
			case 0:
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_GEOMETRIE, (LPARAM) NULL);
				break;

			case 1:
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_DIL, (LPARAM) NULL);
				break;

			case 2:
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_PGA, (LPARAM)NULL);
				break;

			case 3:
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SOIC, (LPARAM)NULL);
				break;

			case 4:
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_QUAD, (LPARAM) NULL);
				break;

			case 5:
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_BGA, (LPARAM) NULL);
				break;
			}
		}

		break;

	case ID_FILE_OPEN:
		if (SaveFile2(0) == 1)
		{
			if (DirPath[0] == 0)
			{
				if (FoundDesignPath)
				{
					sprintf(DirPath, "%s\\pcb\\shapes", DesignPath);

					if (DirectoryExists(DirPath))
						sprintf(DirPath, "%s\\shapes", ProjectPath);
				}
				else
					sprintf(DirPath, "%s\\shapes", ProjectPath);
			}

			if (GetNewFileUTF8 //otevøít
			(GEOMWindow, NULL, EditFile, DirPath, SC(116, "Geometry files"), NULL, SC(115, "Open geometry file"), "shp", 0) == 0)
			{
				if (ProjectActive)
				{
					cnt = 0;

					while ((cnt < 32)
					        && ((ProjectInfo->FileTypes[cnt] != 3)
					            || (stricmpUTF8(ProjectInfo->FileNames[cnt], EditFile) != 0)))
						cnt++;

					if (cnt < 32)
					{
						if (ShowWindow(ProjectInfo->WindowHandles[cnt], SW_RESTORE))
							SetForegroundWindow(ProjectInfo->WindowHandles[cnt]);

						return;
					}
				}

				OldUnits = Units;
				ChangeFile(EditFile, 0);
				Units = OldUnits;
				RePaint();
			}
		}

		break;

	case ID_FILE_SAVE:
	case ID_GLOBAL_FILE_SAVE:
		if (SaveFile(0) == -1)
			MessageBoxUTF8(GEOMWindow, SC(29, "Error in saving file"), SC(48, "Error"), MB_APPLMODAL | MB_OK); //chyba uložit

		break;

	case ID_FILE_SAVEAS:
		if (SaveFile(4) == -1)
			MessageBoxUTF8(GEOMWindow, SC(29, "Error in saving file"), SC(48, "Error"), MB_APPLMODAL | MB_OK); //chyba uložit jako

		break;

	case ID_SAVE_WITH_ERRORS:
		if (SaveFile(2) == -1)
			MessageBoxUTF8(GEOMWindow, SC(29, "Error in saving file"), SC(48, "Error"), MB_APPLMODAL | MB_OK); //chyba uložit s chybami

		break;

	case ID_FILE_EXIT:
		ExitProgram();
		break;

	case ID_FILE_PRINT:
		Print(0);
		break;

	case ID_IMPORT_PIN_BITMAP:
		ImportBitmapAsPin();
		break;

	case ID_IMPORT_DXF:
		ImportDXF();
		break;

	case ID_EXPORT_DXF:
		ExportDXF(0);
		break;

	case ID_CREATE_GEOMETRIE:
		if (SaveFile2(0) == 1)
		{
			DeAllocateMemGeometrie();
			InitNewShape("noname", 0);
			ViewFull();
		}

		break;

	case ID_CREATE_DIL:
		if (SaveFile2(0) == 1)
		{
			DILGeom.Clearance = CurrentClearance;

			if (CreateGeomDialog(IDD_DIALOG_DIL) == 1)
			{
				InsertDialogObjects(IDD_DIALOG_DIL);
				RePaint();
				EditFile[0] = 0;
			}
		}

		break;

	case ID_CREATE_QUAD:
		if (SaveFile2(0) == 1)
		{
			QUADGeom.Clearance = CurrentClearance;

			if (CreateGeomDialog(IDD_DIALOG_QUAD) == 1)
			{
				InsertDialogObjects(IDD_DIALOG_QUAD);
				RePaint();
				EditFile[0] = 0;
			}
		}

		break;

	case ID_CREATE_BGA:
		if (SaveFile2(0) == 1)
		{
			BGAGeom.Clearance = CurrentClearance;

			if (CreateGeomDialog(IDD_DIALOG_BGA) == 1)
			{
				InsertDialogObjects(IDD_DIALOG_BGA);
				RePaint();
				EditFile[0] = 0;
			}
		}

		break;

	case ID_CREATE_PGA:
		if (SaveFile2(0) == 1)
		{
			PGAGeom.Clearance = CurrentClearance;

			if (CreateGeomDialog(IDD_DIALOG_PGA) == 1)
			{
				InsertDialogObjects(IDD_DIALOG_PGA);
				RePaint();
				EditFile[0] = 0;
			}
		}

		break;

	case ID_CREATE_SOIC:
		if (SaveFile2(0) == 1)
		{
			SOICGeom.Clearance = CurrentClearance;

			if (CreateGeomDialog(IDD_DIALOG_SOIC) == 1)
			{
				InsertDialogObjects(IDD_DIALOG_SOIC);
				RePaint();
				EditFile[0] = 0;
			}
		}

		break;

	case ID_CREATE_SIL:
		SILGeom.Clearance = CurrentClearance;

		if (Units == 0)
			SILGeom.Units = 1;
		else
			SILGeom.Units = 0;

		if (CreateGeomDialog(IDD_DIALOG_SIL) == 1)
		{
			if (InsertDialogObjects(IDD_DIALOG_SIL) == 0)
			{
				CommandAddMultipleObjects(0);
				RePaint();
			}
		}

		break;

	case ID_CREATE_SIL2:
		SILGeom.Clearance = CurrentClearance;

		if (Units == 0)
			SILGeom.Units = 1;
		else
			SILGeom.Units = 0;

		if (CreateGeomDialog(IDD_DIALOG_SIL2) == 1)
		{
			if (InsertDialogObjects(IDD_DIALOG_SIL) == 0)
			{
				CommandAddMultipleObjects(0);
				RePaint();
			}
		}

		break;

	case ID_CREATE_SIL_SMD_RECT:
		SIL_SMD_RECTGeom.Clearance = CurrentClearance;

		if (CreateGeomDialog(IDD_DIALOG_SIL_SMD_RECT) == 1)
		{
			if (InsertDialogObjects(IDD_DIALOG_SIL_SMD_RECT) == 0)
			{
				CommandAddMultipleObjects(1);
				RePaint();
			}
		}

		break;

	case ID_CREATE_SIL_SMD_CIRCLE:
		SIL_SMD_CIRCLEGeom.Clearance = CurrentClearance;

		if (CreateGeomDialog(IDD_DIALOG_SIL_SMD_CIRCLE) == 1)
		{
			if (InsertDialogObjects(IDD_DIALOG_SIL_SMD_CIRCLE) == 0)
			{
				CommandAddMultipleObjects(2);
				RePaint();
			}
		}

		break;

	case ID_EDIT_MOVE_ORIGIN:
		CommandChangedCrosses(0.0, 0.0, 0);
		break;

	case ID_EDIT_MOVE_ORIGIN2:
		GetMinMaxSelectedObjects();
		x1 = (SelectedMinX + SelectedMaxX) / 2;
		y1 = (SelectedMinY + SelectedMaxY) / 2;
		CommandChangedCrosses(x1, y1, 2);
		break;

	case ID_EDIT_INSERTPOINT:
		CommandChangedCrosses(0.0, 0.0, 1);
		break;

	case ID_EDIT_INSERTPOINT2:
		GetMinMaxSelectedObjects();
		x1 = (SelectedMinX + SelectedMaxX) / 2;
		y1 = (SelectedMinY + SelectedMaxY) / 2;
		Shape.InsertionX = (float) x1;
		Shape.InsertionY = (float) y1;
		DataBaseChanged = 1;
		break;

	case ID_EDIT_NRLAYERS:
		ChangeNrLayers(0);
		break;

	case ID_EDIT_SHAPEHEIGHT:
		ChangeGeometryHeight(0);
		break;

	case ID_EDIT_PAD_RULES:
		EditPadRules();
		break;

	case ID_MODE_SELECTION_REPLACE:
		ReplaceSelections = 1;
		ChangeMenuSelectionMode();
		break;

	case ID_MODE_SELECTION_APPEND:
		ReplaceSelections = 0;
		ChangeMenuSelectionMode();
		break;

     // *****************************************************************************************
	case ID_ADD_RECT_PAD_TOP:
		CommandAddObjects(OBJECT_RECT, 0.0, NrPadLayers - 1, 0);
		break;

	case ID_ADD_RECT_PAD_TOP_PASTE:
		CommandAddObjects(OBJECT_RECT, 0.0, PASTE_MASK_TOP_LAYER, 0);
		break;

	case ID_ADD_RECT_PAD_TOP_MASK:
		CommandAddObjects(OBJECT_RECT, 0.0, SOLD_MASK_TOP_LAYER, 0);
		break;

	case ID_ADD_RECT_PAD_BOTTOM:
		CommandAddObjects(OBJECT_RECT, 0.0, 0, 0);
		break;

	case ID_ADD_RECT_PAD_BOTTOM_PASTE:
		CommandAddObjects(OBJECT_RECT, 0.0, PASTE_MASK_BOTTOM_LAYER, 0);
		break;

	case ID_ADD_RECT_PAD_BOTTOM_MASK:
		CommandAddObjects(OBJECT_RECT, 0.0, SOLD_MASK_BOTTOM_LAYER, 0);
		break;

     // *****************************************************************************************
	case ID_ADD_CIRCLE_PAD_TOP:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, NrPadLayers - 1, 0);
		break;

	case ID_ADD_CIRCLE_PAD_TOP_PASTE:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, PASTE_MASK_TOP_LAYER, 0);
		break;

	case ID_ADD_CIRCLE_PAD_TOP_MASK:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, SOLD_MASK_TOP_LAYER, 0);
		break;

	case ID_ADD_CIRCLE_PAD_BOTTOM:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, 0, 0);
		break;

	case ID_ADD_CIRCLE_PAD_BOTTOM_PASTE:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, PASTE_MASK_BOTTOM_LAYER, 0);
		break;

	case ID_ADD_CIRCLE_PAD_BOTTOM_MASK:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, SOLD_MASK_BOTTOM_LAYER, 0);
		break;

	case ID_ADD_CIRCLE_PAD_ANTI_POWER:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, POWER_PAD_LAYER, 0);
		break;

	case ID_ADD_DRILL:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, DRILL_LAYER, 0);
		break;

	case ID_ADD_DRILL_UNPLATED:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, DRILL_UNPLATED_LAYER, 0);
		break;

     // *****************************************************************************************
	case ID_ADD_LINE_SILK_TOP:
		CommandAddObjects(OBJECT_LINE, CurrentSilkscreenLine, SILKSCREEN_TOP_LAYER, LParam * 0x100);
		break;

	case ID_ADD_LINE_PLACE_OUTL:
		CommandAddObjects(OBJECT_LINE, 0.0, PLACEMENT_OUTLINE_LAYER, LParam * 0x100);
		break;

	case ID_ADD_LINE_COMP_OUTL:
		CommandAddObjects(OBJECT_LINE, CurrentCompOutLine, COMP_OUTLINE_LAYER, LParam * 0x100);
		break;

	case ID_ADD_LINE_TOP:
		CommandAddObjects(OBJECT_LINE, TraceThickness, 1, 0);
		break;

	case ID_ADD_LINE_BOTTOM:
		CommandAddObjects(OBJECT_LINE, TraceThickness, 0, 0);
		break;

	case ID_ADD_LINE_BOTTOM_PASTE:
		CommandAddObjects(OBJECT_LINE, TraceThickness, PASTE_MASK_BOTTOM_LAYER, 0);
		break;

	case ID_ADD_LINE_TOP_PASTE:
		CommandAddObjects(OBJECT_LINE, TraceThickness, PASTE_MASK_TOP_LAYER, 0);
		break;

	case ID_ADD_LINE_BOTTOM_MASK:
		CommandAddObjects(OBJECT_LINE, TraceThickness, SOLD_MASK_BOTTOM_LAYER, 0);
		break;

	case ID_ADD_LINE_TOP_MASK:
		CommandAddObjects(OBJECT_LINE, TraceThickness, SOLD_MASK_TOP_LAYER, 0);
		break;

     // *****************************************************************************************
	case ID_ADD_RECT_SILK_TOP:
		CommandAddObjects(OBJECT_RECT, CurrentSilkscreenLine, SILKSCREEN_TOP_LAYER, LParam);
		break;

	case ID_ADD_RECT_PLACE_OUTL:
		CommandAddObjects(OBJECT_RECT, 0.0, PLACEMENT_OUTLINE_LAYER, LParam);
		break;

	case ID_ADD_RECT_COMP_OUTL:
		CommandAddObjects(OBJECT_RECT, CurrentCompOutLine, COMP_OUTLINE_LAYER, LParam);
		break;

	case ID_ADD_RECT2_SILK_TOP:
		CommandAddObjects(OBJECT_RECT, CurrentSilkscreenLine, SILKSCREEN_TOP_LAYER, 2 + LParam);
		break;

	case ID_ADD_RECT2_PLACE_OUTL:
		CommandAddObjects(OBJECT_RECT, 0.0, PLACEMENT_OUTLINE_LAYER, 2 + LParam);
		break;

	case ID_ADD_RECT2_COMP_OUTL:
		CommandAddObjects(OBJECT_RECT, CurrentCompOutLine, COMP_OUTLINE_LAYER, 2 + LParam);
		break;

     // *****************************************************************************************
	case ID_ADD_CIRCLE_SILK_TOP_F:
		CommandAddObjects(OBJECT_CIRCLE, CurrentSilkscreenLine, SILKSCREEN_TOP_LAYER, 15 + LParam * 16);
		break;

	case ID_ADD_CIRCLE_PLACE_F:
		CommandAddObjects(OBJECT_CIRCLE, 0.0, PLACEMENT_OUTLINE_LAYER, 15 + LParam * 16);
		break;

	case ID_ADD_CIRCLE_COMP_F:
		CommandAddObjects(OBJECT_CIRCLE, CurrentCompOutLine, COMP_OUTLINE_LAYER, 15 + LParam * 16);
		break;

     // *****************************************************************************************
	case ID_ADD_TEXT_SILK_TOP:
		CommandAddObjects(OBJECT_TEXT, CurrentSilkscreenLine, SILKSCREEN_TOP_LAYER, 0);
		break;

	case ID_ADD_TEXT_COMP_OUTL:
		CommandAddObjects(OBJECT_TEXT, CurrentCompOutLine, COMP_OUTLINE_LAYER, 0);
		break;

     // *****************************************************************************************
	case ID_SETTINGS_UNITS_MILS:
		Units = 0;
		DisplayCursorPosition();
		MainMenu = GetMenu(GEOMWindow);
		SubMenu = GetSubMenu(MainMenu, 2);
		CheckMenuItem(SubMenu, 0, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 1, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 0, MF_BYPOSITION | MF_CHECKED);
		DrawMenuBar(GEOMWindow);
		break;

	case ID_SETTINGS_UNITS_MM:
		Units = 1;
		DisplayCursorPosition();
		MainMenu = GetMenu(GEOMWindow);
		SubMenu = GetSubMenu(MainMenu, 2);
		CheckMenuItem(SubMenu, 0, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 1, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 1, MF_BYPOSITION | MF_CHECKED);
		DrawMenuBar(GEOMWindow);
		break;

	case ID_SETTINGS_THICKNESS:
		AssignValuesDialog(0);
		break;

	case ID_VIEW_LAYERS:
		if (ViewObjectsDialog(0) == 1)
		{
			InvalidateRect(GEOMWindow, NULL, 0);
			PostMessage(GEOMWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
		}

		break;

	case ID_CLEAR_WIDTH:
		memset(&TypeObject2, 0, sizeof(ObjectRecord2));

		switch (Units)
		{
		case 0:
			sprintf(TypeObject.Text, "%.1f", CurrentClearance / 2540.0);
			break;

		case 1:
			sprintf(TypeObject.Text, "%.4f", CurrentClearance / 100000.0);
			break;
		}

		if (LineInputDialog(&TypeObject2, SC(31, "Change clearance width")) == 1)
		{
			if ((sscanf(TypeObject2.Text, "%f", &value)) == 1)
			{
				switch (Units)
				{
				case 0:
					ChangeClearance(value * 2540.0, 0);
					break;

				case 1:
					ChangeClearance(value * 100000.0, 0);
					break;
				}
			}
		}

		RepeatMode = 0;
		break;

	/*
	    case ID_TRACE_TO_TOP:
	      ChangeTraceLayer(1);
	      RepeatMode=0;
	      break;
	    case ID_TRACE_TO_BOTTOM:
	      ChangeTraceLayer(0);
	      RepeatMode=0;
	      break;
	    case ID_TRACE_TO_SOLD_MASK_TOP:
	      ChangeTraceLayer(SOLD_MASK_TOP_LAYER);
	      RepeatMode=0;
	      break;
	    case ID_TRACE_TO_SOLD_MASK_BOTTOM:
	      ChangeTraceLayer(SOLD_MASK_BOTTOM_LAYER);
	      RepeatMode=0;
	      break;
	    case ID_TRACE_TO_PASTE_MASK_TOP:
	      ChangeTraceLayer(PASTE_MASK_TOP_LAYER);
	      RepeatMode=0;
	      break;
	    case ID_TRACE_TO_PASTE_MASK_BOTTOM:
	      ChangeTraceLayer(PASTE_MASK_BOTTOM_LAYER);
	      RepeatMode=0;
	      break;
	*/
    //case ID_TRACE_WIDTH:
	/*
	      memset(&TypeObject,0,sizeof(ObjectRecord));
	      switch (Units) {
	        case 0:
	          sprintf(TypeObject.Text,"%.1f",TraceThickness/2540.0);
	          break;
	        case 1:
	          sprintf(TypeObject.Text,"%.4f",TraceThickness/100000.0);
	          break;
	      }
	      if (LineInputDialog(&TypeObject,"Change trace width")==1) {
	        if ((sscanf(TypeObject.Text,"%f",&value))==1) {
	          switch (Units) {
	            case 0:
	              ChangeTraceWidth(value*2540.0);
	              break;
	            case 1:
	              ChangeTraceWidth(value*100000.0);
	              break;
	          }
	        }
	      }
	      RepeatMode=0;
	      break;
	*/

	case ID_LINE_WIDTH:
		memset(&TypeObject2, 0, sizeof(ObjectRecord2));
		value = -10.0;

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if ((value < -9.0) && ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED))
			{
				switch (Object->ObjectType)
				{
				case OBJECT_LINE:
				case OBJECT_RECT:
				case OBJECT_ARC:
				case OBJECT_CIRCLE:
					if ((Object->Thickness != 0.0) && (Object->Layer != PLACEMENT_OUTLINE_LAYER))
						value = (float) Object->Thickness;

					break;

				case OBJECT_TEXT:
					value = (float) Object->Thickness;
					break;
				}
			}
		}

		if (value > 0.0)
		{
			switch (Units)
			{
			case 0:
				sprintf(TypeObject2.Text, "%.1f", value / 2540.0);
				break;

			case 1:
				sprintf(TypeObject2.Text, "%.4f", value / 100000.0);
				break;
			}

			if (LineInputDialog(&TypeObject2, SC(32, "Change line width")) == 1)
			{
				if ((sscanf(TypeObject2.Text, "%f", &value)) == 1)
				{
					switch (Units)
					{
					case 0:
						ChangeLineWidth(value * 2540.0);
						break;

					case 1:
						ChangeLineWidth(value * 100000.0);
						break;
					}
				}
			}
		}

		RepeatMode = 0;
		break;

	case ID_EDIT_TEXT:
		Changetext();
		RepeatMode = 0;
		break;

	case ID_CHANGE_TEXT_HEIGHT:
		ChangetextHeight();
		RePaint();
		RepeatMode = 0;
		break;

	case ID_CHANGE_CIRCLES:
		ChangeParamsObjects(0);
		RePaint();
		RepeatMode = 0;
		break;

	case ID_CHANGE_RECTS:
		ChangeParamsObjects(1);
		RePaint();
		RepeatMode = 0;
		break;

	case ID_CHANGE_ARCS:
		ChangeParamsObjects(2);
		RePaint();
		RepeatMode = 0;
		break;

	case ID_CHANGE_ARCS2:
		ChangeParamsObjects(3);
		RePaint();
		RepeatMode = 0;
		break;

	case ID_EDIT_UNDO:
		UndoObjects();
		RePaint();
		RepeatMode = 0;
		break;

	case ID_EDIT_REDO:
		RedoObjects();
		RePaint();
		RepeatMode = 0;
		break;

	case ID_CHECK_GEOMETRY:
		CheckPads(1);
		break;

	case ID_COPY_TO_CLIPBOARD:
		CopyObjectsToClipBoard(0);
		break;

	case ID_COPY_FROM_CLIPBOARD:
		CopyObjectsFromClipBoard(0);
		break;

	case ID_VIEW_REPAINT:
		OkToAddViewPos = 0;
		RePaint();
		break;

	case ID_VIEW_ZOOMIN:
		ZoomIn(LParam);
		break;

	case ID_VIEW_ZOOMOUT:
		ZoomOut(LParam);
		break;

	case ID_VIEW_PAN:
		ViewPan(LParam);
		break;

	case ID_VIEW_CHANGEGRID:
		GridDialog(0);
		break;

	case ID_VIEW_VIEWFULL:
		ViewFull();
		break;

	case ID_VIEW_PREVIOUS_VIEW:
		PreviousView();
		break;

	case ID_ASSIGN_PINS:
		if (AssignPinDialog(&NewObject, 0) == 1)
		{
			AssignObjectsToPin(&NewObject);
			RePaint();
		}

		break;

	case ID_ASSIGN_PINS2:
		if (AssignPinDialog(&NewObject, 1) == 1)
		{
			AssignObjectsToPin(&NewObject);
			RePaint();
		}

		break;

	case ID_RENAME_PIN:
		RenamePin(0);
		break;

	case ID_VIEW_INFOSELECTEDOBJECTS:
		ObjectsInfo();
		RepeatMode = 0;
		break;

	case ID_LINES_45_DIR:
		LinesAllDirection = 0;
		break;

	case ID_LINES_ALL_DIR:
		LinesAllDirection = 1;
		break;

	case ID_FINISH_POLYGON:
		FinishPolygon = 1;
		break;

	case ID_TRACE_BACKWARDS:
		TraceBackWardsKeyPressed = 1;
		break;

	case ID_EDIT_GEOMNAME:
		CommandAddObjects(OBJECT_TEXT, CurrentSilkscreenLine, GEOM_NAME_LAYER, 0);
		break;

	/*
	    case ID_EDIT_COPYTOSILKSCREEN:
	      CopyObjectsToSilkscreen();
	      break;
	*/

	case ID_VIEW_RELATIVEPOSITIONONGRID:
		MouseCursorOnGrid = !MouseCursorOnGrid;
		ChangeMenuRelativeMousePosition();
		break;

	case ID_VIEW_CHANGECOLORS:
		ColorDialog(0);
		break;

	case ID_VIEW_LOADDEFAULTCOLORS:
		DeleteGraphicObjects();
		LoadDefaultColors();
		CreateDrawObjects();
		RePaint();
		break;

	 //*************************** Right button popup menu ************************

	case ID_ESCAPE:
		SelectionEsc = 1;
		break;

	case ID_SETTINGS_CHANGE_UNITS:
		if (Units == 0)
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MM, (LPARAM) NULL);
		else
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MILS, (LPARAM) NULL);

		break;

	case ID_MOVE_OBJECTS:
		RepeatMode = 1;
		LastAction = 1;
		MoveSelectedObjects(0, 0);
		RePaint();
		break;

	case ID_MOVE_OBJECTS2:
		RepeatMode = 0;
		MoveSelectedObjectsToZero(0, 0);
		RePaint();
		break;

	case ID_SCALE_OBJECTS:
		ScaleSelectedObjects(0);
		RePaint();
		break;

	case ID_ROTATE_OBJECTS2:
		PlaceRotatedFlippedComponents(32);
		RePaint();
		break;

	case ID_ROTATE_OBJECTS:
		PlaceRotatedFlippedComponents(1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_EDIT_ROT_90_ZERO:
		PlaceRotatedFlippedComponents(3 + 16);
		RePaint();
		RepeatMode = 0;
		break;

	case ID_EDIT_CUT_FROM_OBJECT:
		DeleteFromObjectPolygon(0);
		RePaint();
		break;

	case ID_EDIT_MERGE_OBJECTS:
		MergeObjectsToPolygon(0);
		RePaint();
		break;

	case ID_LINES_TO_POLYGON:
		GetPolygonFromLines(0);
		RePaint();
		break;

	/*
	    case ID_ROTATE180_OBJECTS:
	      PlaceRotatedFlippedComponents(2);
	      RepeatMode=0;
	      break;
	    case ID_ROTATE270_OBJECTS:
	      PlaceRotatedFlippedComponents(3);
	      RepeatMode=0;
	      break;
	*/

	case ID_MIRRORX_OBJECTS:
		PlaceRotatedFlippedComponents(4);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MIRRORY_OBJECTS:
		PlaceRotatedFlippedComponents(8);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_VIEW_MEASUREMENT:
		Measurement(0);
		break;

	case ID_EDIT_GOTOXY:
		GotoXY();
		break;

	case ID_CALC_DISTANCE:
		CalcDistanceBetweenObjects(0);
		break;

	case ID_EDIT_ZERORELATIVECURSOR:
		if (MouseCursorOnGrid)
		{
			RelX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			RelY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
		}
		else
		{
			RelX = PixelToRealOffX(MousePosX);
			RelY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		}

		val = PixelToReal(10);
		DrawLineWhite(RelX, RelY + val, RelX, RelY - val, 0);
		DrawLineWhite(RelX + val, RelY, RelX - val, RelY, 0);
		DisplayCursorPosition();
		break;

	case ID_EDIT_ZERORELATIVECURSOR_SNAP:
		x1 = PixelToRealOffX(MousePosX);
		y1 = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		x2 = 0.0;
		y2 = 0.0;
		AdjustOffsetForSnap(x1, y1, 0.0, 0.0, 0.0, 0.0, &x2, &y2, 1);
		RelX = x1 + x2;
		RelY = y1 + y2;
		SetCursorFromRealPosition(RelX, RelY);
		val = PixelToReal(10);
		DrawLineWhite(RelX, RelY + val, RelX, RelY - val, 0);
		DrawLineWhite(RelX + val, RelY, RelX - val, RelY, 0);
		DisplayCursorPosition();
		break;

	case ID_LENGTH_TO_RELATIVE_CURSOR:
		x1 = PixelToRealOffX(MousePosX);
		y1 = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		AdjustOffsetForSnap(x1, y1, 0.0, 0.0, 0.0, 0.0, &x2, &y2, 1);
		x1 += x2;
		y1 += y2;
		ConvNormalCoorToPolar(RelX, RelY, x1, y1, &LineAngle, &LineLength);
		LineLength = ConvertUnits(LineLength, Units);
		LineAngle *= 180 / PI;

		switch (Units)
		{
		case 0:
			sprintf(InfoStr, "Length %.2f thou  Angle %.2f", LineLength, LineAngle);
			break;

		case 1:
			sprintf(InfoStr, SC(34, "Length %.4f mm  Angle %.2f"), LineLength, LineAngle);
			break;
		}

		RedrawInfoStr(1);
		val = PixelToReal(10);
		DrawLineWhite(RelX, RelY + val, RelX, RelY - val, 0);
		DrawLineWhite(RelX + val, RelY, RelX - val, RelY, 0);
		DrawLineWhite(x1, y1 + val, x1, y1 - val, 0);
		DrawLineWhite(x1 + val, y1, x1 - val, y1, 0);
		SetCursorFromRealPosition(x1, y1);
		DisplayCursorPosition();
		break;

	case ID_EDIT_DELETE:
		DeleteObjects();
		RePaint();
		RepeatMode = 0;
		break;

	case ID_COPY_TO_SIL:
		AddSILOnObjectsSelected(0);
		RepeatMode = 0;
		break;

	case ID_COPY_TO_SILSMD:
		AddSILOnObjectsSelected(1);
		RepeatMode = 0;
		break;

	case ID_COPY_OBJECTS:
		MoveSelectedObjects(1, 1);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE2:
		MoveSelectedObjects(1, 2);
		break;

	case ID_COPY_OBJECTS_MULTIPLE3:
		MoveSelectedObjects(1, 3);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE4:
		MoveSelectedObjects(1, 4);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE5:
		MoveSelectedObjects(1, 5);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE6:
		MoveSelectedObjects(1, 6);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE7:
		MoveSelectedObjects(1, 7);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE8:
		MoveSelectedObjects(1, 8);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE9:
		MoveSelectedObjects(1, 9);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE10:
		MoveSelectedObjects(1, 10);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE11:
		MoveSelectedObjects(1, 11);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE12:
		MoveSelectedObjects(1, 12);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE13:
		MoveSelectedObjects(1, 13);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE14:
		MoveSelectedObjects(1, 14);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE15:
		MoveSelectedObjects(1, 15);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE16:
		MoveSelectedObjects(1, 16);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE17:
		MoveSelectedObjects(1, 17);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE18:
		MoveSelectedObjects(1, 18);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE19:
		MoveSelectedObjects(1, 19);
		RePaint();
		break;

	case ID_COPY_OBJECTS_MULTIPLE20:
		MoveSelectedObjects(1, 20);
		RePaint();
		break;

	 //******************************************************************************************************
	 //******************************************************************************************************
	case ID_COPY_TO_PADS:
	case ID_COPY_TO_PADS_1:
	case ID_COPY_TO_PADS_2:
	case ID_COPY_TO_PADS_3:
	case ID_COPY_TO_PADS_4:
	case ID_COPY_TO_PADS_5:
	case ID_COPY_TO_PADS_6:
	case ID_COPY_TO_PADS_7:
	case ID_COPY_TO_PADS_8:
	case ID_COPY_TO_PADS_9:
	case ID_COPY_TO_PADS_10:
	case ID_COPY_TO_PADS_11:
	case ID_COPY_TO_PADS_12:
	case ID_COPY_TO_PADS_13:
	case ID_COPY_TO_PADS_14:
	case ID_COPY_TO_PADS_15:
	case ID_COPY_TO_PADS_16:
	case ID_COPY_TO_PADS_17:
	case ID_COPY_TO_PADS_18:
	case ID_COPY_TO_PADS_19:
	case ID_COPY_TO_PADS_20:
	case ID_COPY_TO_PADS_21:
	case ID_COPY_TO_PADS_22:
	case ID_COPY_TO_PADS_23:
	case ID_COPY_TO_PADS_24:
	case ID_COPY_TO_PADS_25:
	case ID_COPY_TO_PADS_26:
	case ID_COPY_TO_PADS_27:
	case ID_COPY_TO_PADS_28:
	case ID_COPY_TO_PADS_29:
	case ID_COPY_TO_PADS_30:
	case ID_COPY_TO_PADS_31:
		Layer = WParam - ID_COPY_TO_PADS;
		CopyObjectsToOtherLayer(Layer, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_PASTE_TOP:
		CopyObjectsToOtherLayer(PASTE_MASK_TOP_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_PASTE_BOTTOM:
		CopyObjectsToOtherLayer(PASTE_MASK_BOTTOM_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_SOLDER_TOP:
		CopyObjectsToOtherLayer(SOLD_MASK_TOP_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_SOLDER_BOTTOM:
		CopyObjectsToOtherLayer(SOLD_MASK_BOTTOM_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_SILKSCREEN_TOP:
		CopyObjectsToOtherLayer(SILKSCREEN_TOP_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_SILKSCREEN_BOTTOM:
		CopyObjectsToOtherLayer(SILKSCREEN_BOTTOM_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_COMP_OUTLINE:
		CopyObjectsToOtherLayer(COMP_OUTLINE_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_BOARD_OUTLINE:
		CopyObjectsToOtherLayer(BOARD_OUTLINE_LAYER, 0);
		RepeatMode = 0;
		break;

	case ID_COPY_TO_ROUTING_KEEPOUT:
	case ID_COPY_TO_ROUTING_KEEPOUT_1:
	case ID_COPY_TO_ROUTING_KEEPOUT_2:
	case ID_COPY_TO_ROUTING_KEEPOUT_3:
	case ID_COPY_TO_ROUTING_KEEPOUT_4:
	case ID_COPY_TO_ROUTING_KEEPOUT_5:
	case ID_COPY_TO_ROUTING_KEEPOUT_6:
	case ID_COPY_TO_ROUTING_KEEPOUT_7:
	case ID_COPY_TO_ROUTING_KEEPOUT_8:
	case ID_COPY_TO_ROUTING_KEEPOUT_9:
	case ID_COPY_TO_ROUTING_KEEPOUT_10:
	case ID_COPY_TO_ROUTING_KEEPOUT_11:
	case ID_COPY_TO_ROUTING_KEEPOUT_12:
	case ID_COPY_TO_ROUTING_KEEPOUT_13:
	case ID_COPY_TO_ROUTING_KEEPOUT_14:
	case ID_COPY_TO_ROUTING_KEEPOUT_15:
	case ID_COPY_TO_ROUTING_KEEPOUT_16:
	case ID_COPY_TO_ROUTING_KEEPOUT_17:
	case ID_COPY_TO_ROUTING_KEEPOUT_18:
	case ID_COPY_TO_ROUTING_KEEPOUT_19:
	case ID_COPY_TO_ROUTING_KEEPOUT_20:
	case ID_COPY_TO_ROUTING_KEEPOUT_21:
	case ID_COPY_TO_ROUTING_KEEPOUT_22:
	case ID_COPY_TO_ROUTING_KEEPOUT_23:
	case ID_COPY_TO_ROUTING_KEEPOUT_24:
	case ID_COPY_TO_ROUTING_KEEPOUT_25:
	case ID_COPY_TO_ROUTING_KEEPOUT_26:
	case ID_COPY_TO_ROUTING_KEEPOUT_27:
	case ID_COPY_TO_ROUTING_KEEPOUT_28:
	case ID_COPY_TO_ROUTING_KEEPOUT_29:
	case ID_COPY_TO_ROUTING_KEEPOUT_30:
	case ID_COPY_TO_ROUTING_KEEPOUT_31:
		Layer = WParam - ID_COPY_TO_ROUTING_KEEPOUT;
		CopyObjectsToOtherLayer(Layer + ROUTING_KEEPOUT_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_INFO1:
		CopyObjectsToOtherLayer(INFO_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_INFO2:
		CopyObjectsToOtherLayer(INFO_LAYER2, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_INFO3:
		CopyObjectsToOtherLayer(INFO_LAYER3, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_INFO4:
		CopyObjectsToOtherLayer(INFO_LAYER4, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_PLACEMENT_OUTLINE:
		CopyObjectsToOtherLayer(PLACEMENT_OUTLINE_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_DRILLS:
		CopyObjectsToOtherLayer(DRILL_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_DRILLS_UNPLATED:
		CopyObjectsToOtherLayer(DRILL_UNPLATED_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_POWER_PADS:
		CopyObjectsToOtherLayer(POWER_PAD_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_COPY_TO_INNER_PADS:
		CopyObjectsToOtherLayer(INNER_PAD_LAYER, 0);
		RepeatMode = 0;
		RePaint();
		break;

	 //******************************************************************************************************
	 //******************************************************************************************************
	case ID_MOVE_TO_PADS:
	case ID_MOVE_TO_PADS_1:
	case ID_MOVE_TO_PADS_2:
	case ID_MOVE_TO_PADS_3:
	case ID_MOVE_TO_PADS_4:
	case ID_MOVE_TO_PADS_5:
	case ID_MOVE_TO_PADS_6:
	case ID_MOVE_TO_PADS_7:
	case ID_MOVE_TO_PADS_8:
	case ID_MOVE_TO_PADS_9:
	case ID_MOVE_TO_PADS_10:
	case ID_MOVE_TO_PADS_11:
	case ID_MOVE_TO_PADS_12:
	case ID_MOVE_TO_PADS_13:
	case ID_MOVE_TO_PADS_14:
	case ID_MOVE_TO_PADS_15:
	case ID_MOVE_TO_PADS_16:
	case ID_MOVE_TO_PADS_17:
	case ID_MOVE_TO_PADS_18:
	case ID_MOVE_TO_PADS_19:
	case ID_MOVE_TO_PADS_20:
	case ID_MOVE_TO_PADS_21:
	case ID_MOVE_TO_PADS_22:
	case ID_MOVE_TO_PADS_23:
	case ID_MOVE_TO_PADS_24:
	case ID_MOVE_TO_PADS_25:
	case ID_MOVE_TO_PADS_26:
	case ID_MOVE_TO_PADS_27:
	case ID_MOVE_TO_PADS_28:
	case ID_MOVE_TO_PADS_29:
	case ID_MOVE_TO_PADS_30:
	case ID_MOVE_TO_PADS_31:
		Layer = WParam - ID_MOVE_TO_PADS;
		CopyObjectsToOtherLayer(Layer, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_PASTE_TOP:
		CopyObjectsToOtherLayer(PASTE_MASK_TOP_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_PASTE_BOTTOM:
		CopyObjectsToOtherLayer(PASTE_MASK_BOTTOM_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_SOLDER_TOP:
		CopyObjectsToOtherLayer(SOLD_MASK_TOP_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_SOLDER_BOTTOM:
		CopyObjectsToOtherLayer(SOLD_MASK_BOTTOM_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_SILKSCREEN_TOP:
		CopyObjectsToOtherLayer(SILKSCREEN_TOP_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_SILKSCREEN_BOTTOM:
		CopyObjectsToOtherLayer(SILKSCREEN_BOTTOM_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_COMP_OUTLINE:
		CopyObjectsToOtherLayer(COMP_OUTLINE_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_BOARD_OUTLINE:
		CopyObjectsToOtherLayer(BOARD_OUTLINE_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_ROUTING_KEEPOUT:
	case ID_MOVE_TO_ROUTING_KEEPOUT_1:
	case ID_MOVE_TO_ROUTING_KEEPOUT_2:
	case ID_MOVE_TO_ROUTING_KEEPOUT_3:
	case ID_MOVE_TO_ROUTING_KEEPOUT_4:
	case ID_MOVE_TO_ROUTING_KEEPOUT_5:
	case ID_MOVE_TO_ROUTING_KEEPOUT_6:
	case ID_MOVE_TO_ROUTING_KEEPOUT_7:
	case ID_MOVE_TO_ROUTING_KEEPOUT_8:
	case ID_MOVE_TO_ROUTING_KEEPOUT_9:
	case ID_MOVE_TO_ROUTING_KEEPOUT_10:
	case ID_MOVE_TO_ROUTING_KEEPOUT_11:
	case ID_MOVE_TO_ROUTING_KEEPOUT_12:
	case ID_MOVE_TO_ROUTING_KEEPOUT_13:
	case ID_MOVE_TO_ROUTING_KEEPOUT_14:
	case ID_MOVE_TO_ROUTING_KEEPOUT_15:
	case ID_MOVE_TO_ROUTING_KEEPOUT_16:
	case ID_MOVE_TO_ROUTING_KEEPOUT_17:
	case ID_MOVE_TO_ROUTING_KEEPOUT_18:
	case ID_MOVE_TO_ROUTING_KEEPOUT_19:
	case ID_MOVE_TO_ROUTING_KEEPOUT_20:
	case ID_MOVE_TO_ROUTING_KEEPOUT_21:
	case ID_MOVE_TO_ROUTING_KEEPOUT_22:
	case ID_MOVE_TO_ROUTING_KEEPOUT_23:
	case ID_MOVE_TO_ROUTING_KEEPOUT_24:
	case ID_MOVE_TO_ROUTING_KEEPOUT_25:
	case ID_MOVE_TO_ROUTING_KEEPOUT_26:
	case ID_MOVE_TO_ROUTING_KEEPOUT_27:
	case ID_MOVE_TO_ROUTING_KEEPOUT_28:
	case ID_MOVE_TO_ROUTING_KEEPOUT_29:
	case ID_MOVE_TO_ROUTING_KEEPOUT_30:
	case ID_MOVE_TO_ROUTING_KEEPOUT_31:
		Layer = WParam - ID_MOVE_TO_ROUTING_KEEPOUT;
		CopyObjectsToOtherLayer(Layer + ROUTING_KEEPOUT_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_INFO1:
		CopyObjectsToOtherLayer(INFO_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_INFO2:
		CopyObjectsToOtherLayer(INFO_LAYER2, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_INFO3:
		CopyObjectsToOtherLayer(INFO_LAYER3, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_INFO4:
		CopyObjectsToOtherLayer(INFO_LAYER4, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_PLACEMENT_OUTLINE:
		CopyObjectsToOtherLayer(PLACEMENT_OUTLINE_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_DRILLS:
		CopyObjectsToOtherLayer(DRILL_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_DRILLS_UNPLATED:
		CopyObjectsToOtherLayer(DRILL_UNPLATED_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_POWER_PADS:
		CopyObjectsToOtherLayer(POWER_PAD_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_MOVE_TO_INNER_PADS:
		CopyObjectsToOtherLayer(INNER_PAD_LAYER, 1);
		RepeatMode = 0;
		RePaint();
		break;

	 //******************************************************************************************************
	 //******************************************************************************************************
	case ID_COPY_COOR:
		CopyOnMultipleCoordinates();
		RePaint();
		break;

	case ID_VIEW_VERTICES_POLYGON:
		ViewVerticesObjectPolygon(0);
		break;

	case ID_ZERO_CENTER:
		GetMinMaxSelectedObjects();
		x1 = (SelectedMinX + SelectedMaxX) / 2;
		y1 = (SelectedMinY + SelectedMaxY) / 2;
		CenterMoveX = x1;
		CenterMoveY = y1;
		DisplayCursorPosition();
		RepeatMode = 0;
		break;

	case ID_CHANGE_SNAP_MODE_ON:
		SnapMode |= 1;
		break;

	case ID_CHANGE_SNAP_MODE_OFF:
		SnapMode &= ~1;
		break;

	case ID_UNSELECT_ALL:
		UnselectAllObjects();
		RepeatMode = 0;
		break;

	case ID_SELECT_ONLY_PADS:
	case ID_SELECT_ONLY_PADS_1:
	case ID_SELECT_ONLY_PADS_2:
	case ID_SELECT_ONLY_PADS_3:
	case ID_SELECT_ONLY_PADS_4:
	case ID_SELECT_ONLY_PADS_5:
	case ID_SELECT_ONLY_PADS_6:
	case ID_SELECT_ONLY_PADS_7:
	case ID_SELECT_ONLY_PADS_8:
	case ID_SELECT_ONLY_PADS_9:
	case ID_SELECT_ONLY_PADS_10:
	case ID_SELECT_ONLY_PADS_11:
	case ID_SELECT_ONLY_PADS_12:
	case ID_SELECT_ONLY_PADS_13:
	case ID_SELECT_ONLY_PADS_14:
	case ID_SELECT_ONLY_PADS_15:
	case ID_SELECT_ONLY_PADS_16:
	case ID_SELECT_ONLY_PADS_17:
	case ID_SELECT_ONLY_PADS_18:
	case ID_SELECT_ONLY_PADS_19:
	case ID_SELECT_ONLY_PADS_20:
	case ID_SELECT_ONLY_PADS_21:
	case ID_SELECT_ONLY_PADS_22:
	case ID_SELECT_ONLY_PADS_23:
	case ID_SELECT_ONLY_PADS_24:
	case ID_SELECT_ONLY_PADS_25:
	case ID_SELECT_ONLY_PADS_26:
	case ID_SELECT_ONLY_PADS_27:
	case ID_SELECT_ONLY_PADS_28:
	case ID_SELECT_ONLY_PADS_29:
	case ID_SELECT_ONLY_PADS_30:
	case ID_SELECT_ONLY_PADS_31:
	case ID_SELECT_ONLY_ANTI_POWERPADS:
	case ID_SELECT_ONLY_INNERPADS:
	case ID_SELECT_ONLY_BUTTERFLY:
	case ID_SELECT_ONLY_INFO1:
	case ID_SELECT_ONLY_INFO2:
	case ID_SELECT_ONLY_INFO3:
	case ID_SELECT_ONLY_INFO4:
	case ID_SELECT_ONLY_SILK_TOP:
	case ID_SELECT_ONLY_SILK_BOTTOM:
	case ID_SELECT_ONLY_COMP_OUTLINE:
	case ID_SELECT_ONLY_PLACEM_OUTLINE:
	case ID_SELECT_ONLY_MASK_TOP:
	case ID_SELECT_ONLY_MASK_BOTTOM:
	case ID_SELECT_ONLY_PASTE_TOP:
	case ID_SELECT_ONLY_PASTE_BOTTOM:
	case ID_SELECT_ONLY_LINES:
	case ID_SELECT_ONLY_RECTS:
	case ID_SELECT_ONLY_CIRCLES:
	case ID_SELECT_ONLY_ARCS:
	case ID_SELECT_ONLY_TEXT:
	case ID_SELECT_ONLY_ROUT_KEEPOUT:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_1:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_2:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_3:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_4:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_5:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_6:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_7:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_8:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_9:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_10:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_11:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_12:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_13:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_14:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_15:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_16:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_17:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_18:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_19:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_20:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_21:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_22:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_23:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_24:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_25:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_26:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_27:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_28:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_29:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_30:
	case ID_SELECT_ONLY_ROUT_KEEPOUT_31:
	case ID_SELECT_ONLY_DRILL:
	case ID_SELECT_ONLY_DRILL_UNPLATED:
	case ID_SELECT_ONLY_TRACES:
	case ID_UNSELECT_PADS:
	case ID_UNSELECT_PADS_1:
	case ID_UNSELECT_PADS_2:
	case ID_UNSELECT_PADS_3:
	case ID_UNSELECT_PADS_4:
	case ID_UNSELECT_PADS_5:
	case ID_UNSELECT_PADS_6:
	case ID_UNSELECT_PADS_7:
	case ID_UNSELECT_PADS_8:
	case ID_UNSELECT_PADS_9:
	case ID_UNSELECT_PADS_10:
	case ID_UNSELECT_PADS_11:
	case ID_UNSELECT_PADS_12:
	case ID_UNSELECT_PADS_13:
	case ID_UNSELECT_PADS_14:
	case ID_UNSELECT_PADS_15:
	case ID_UNSELECT_PADS_16:
	case ID_UNSELECT_PADS_17:
	case ID_UNSELECT_PADS_18:
	case ID_UNSELECT_PADS_19:
	case ID_UNSELECT_PADS_20:
	case ID_UNSELECT_PADS_21:
	case ID_UNSELECT_PADS_22:
	case ID_UNSELECT_PADS_23:
	case ID_UNSELECT_PADS_24:
	case ID_UNSELECT_PADS_25:
	case ID_UNSELECT_PADS_26:
	case ID_UNSELECT_PADS_27:
	case ID_UNSELECT_PADS_28:
	case ID_UNSELECT_PADS_29:
	case ID_UNSELECT_PADS_30:
	case ID_UNSELECT_PADS_31:
	case ID_UNSELECT_ANTI_POWERPADS:
	case ID_UNSELECT_INNER_PADS:
	case ID_UNSELECT_BUTTERFLY:
	case ID_UNSELECT_SILK_TOP:
	case ID_UNSELECT_SILK_BOTTOM:
	case ID_UNSELECT_COMP_OUTLINE:
	case ID_UNSELECT_PLACEM_OUTLINE:
	case ID_UNSELECT_MASK_TOP:
	case ID_UNSELECT_MASK_BOTTOM:
	case ID_UNSELECT_PASTE_TOP:
	case ID_UNSELECT_PASTE_BOTTOM:
	case ID_UNSELECT_LINES:
	case ID_UNSELECT_RECTS:
	case ID_UNSELECT_CIRCLES:
	case ID_UNSELECT_ARCS:
	case ID_UNSELECT_TEXT:
	case ID_UNSELECT_TRACES:
	case ID_UNSELECT_ROUT_KEEPOUT:
	case ID_UNSELECT_ROUT_KEEPOUT_1:
	case ID_UNSELECT_ROUT_KEEPOUT_2:
	case ID_UNSELECT_ROUT_KEEPOUT_3:
	case ID_UNSELECT_ROUT_KEEPOUT_4:
	case ID_UNSELECT_ROUT_KEEPOUT_5:
	case ID_UNSELECT_ROUT_KEEPOUT_6:
	case ID_UNSELECT_ROUT_KEEPOUT_7:
	case ID_UNSELECT_ROUT_KEEPOUT_8:
	case ID_UNSELECT_ROUT_KEEPOUT_9:
	case ID_UNSELECT_ROUT_KEEPOUT_10:
	case ID_UNSELECT_ROUT_KEEPOUT_11:
	case ID_UNSELECT_ROUT_KEEPOUT_12:
	case ID_UNSELECT_ROUT_KEEPOUT_13:
	case ID_UNSELECT_ROUT_KEEPOUT_14:
	case ID_UNSELECT_ROUT_KEEPOUT_15:
	case ID_UNSELECT_ROUT_KEEPOUT_16:
	case ID_UNSELECT_ROUT_KEEPOUT_17:
	case ID_UNSELECT_ROUT_KEEPOUT_18:
	case ID_UNSELECT_ROUT_KEEPOUT_19:
	case ID_UNSELECT_ROUT_KEEPOUT_20:
	case ID_UNSELECT_ROUT_KEEPOUT_21:
	case ID_UNSELECT_ROUT_KEEPOUT_22:
	case ID_UNSELECT_ROUT_KEEPOUT_23:
	case ID_UNSELECT_ROUT_KEEPOUT_24:
	case ID_UNSELECT_ROUT_KEEPOUT_25:
	case ID_UNSELECT_ROUT_KEEPOUT_26:
	case ID_UNSELECT_ROUT_KEEPOUT_27:
	case ID_UNSELECT_ROUT_KEEPOUT_28:
	case ID_UNSELECT_ROUT_KEEPOUT_29:
	case ID_UNSELECT_ROUT_KEEPOUT_30:
	case ID_UNSELECT_ROUT_KEEPOUT_31:
	case ID_UNSELECT_DRILL:
	case ID_UNSELECT_DRILL_UNPLATED:
	case ID_UNSELECT_INFO1:
	case ID_UNSELECT_INFO2:
	case ID_UNSELECT_INFO3:
	case ID_UNSELECT_INFO4:
		ChangeSelections(WParam);
		RepeatMode = 0;
		RePaint();
		break;

	case ID_UNSELECT_FIRST_OBJECT:
		UnselectFirstObject(0);
		break;

     // ************************************************************************************
	case ID_HELP_ON_COMMAND:
		HelpAsked = 1;
		break;

	case ID_HELP_TOPICS:
		Help("geometry_editor.htm", 0);
		break;

	case ID_HELP_CONTENTS:
		Help("contents.htm", 0);
		break;

	case ID_HELP_ABOUT:
		AboutDialog();
		break;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
