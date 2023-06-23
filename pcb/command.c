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
#include "calcdef.h"
#include "stdio.h"
#include "keyswin.h"
#include "toets.h"
#include "commdlg.h"
#include "pcb.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "select3.h"
#include "select4.h"
#include "mainloop.h"
#include "nets.h"
#include "nets2.h"
#include "files.h"
#include "files2.h"
#include "trace2.h"
#include "trace3.h"
#include "trace4.h"
#include "trace5.h"
#include "trace6.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "insdel.h"
#include "resource.h"
#include "resource2.h"
#include "command.h"
#include "insdel.h"
#include "dialogs.h"
#include "import.h"
#include "graphics.h"
#include "movecomp.h"
#include "rules.h"
#include "move2.h"
#include "move3.h"
#include "polygon.h"
#include "print.h"
#include "gerber.h"
#include "help.h"
#include "plot.h"
#include "gateswap.h"
#include "dxf.h"
#include "InputGerber.h"
#include "edit.h"
#include "edit2.h"
#include "paint.h"
#include "owntime.h"
#include "geomsave.h"
#include "settings.h"
#include "odb.h"


int32 Size1, Size2, ok;

STARTUPINFO StartupInfo;

extern int32 FinishPolygon, TraceCheck, TraceChanged, TraceBackWardsKeyPressed, InsertViaKeyPressed,
       FinishedTraceKeyPressed, ChangeTraceWidthKeyPressed, ChangeTraceDrawingModeKeyPressed, EditDesignRulesNetKeyPressed,
       DefaultCrcError, ProjectIndexNr, ProjectActive, CrossHairType, CurrentButtonNr, TraceDrawingMode;

extern char GatePinSwapReference[64], GatePinSwapPinName[64], MessageStr[MAX_LENGTH_STRING],
       SearchForReferenceString[MAX_LENGTH_STRING], SearchForPartNrString[MAX_LENGTH_STRING];

extern ProjectInfoRecord *ProjectInfo;
extern double NewAreaFillClearance, TempAreaFillClearance;

extern ATOM LayoutEditorAtom;


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 InitNewDesign(int32 mode);

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void PCBCommand(WPARAM WParam, LPARAM LParam)
{
	int32 Layer, cnt, Found, res, CompNr, NetNr, count, NrCompsSelected, NrReferencesSelected, NrValuesSelected, lengte,
	      SrcLayer, DestLayer;
	double val, TraceLength, TempGridSize, LineWidth;
	NetRecord *Net;
	int32 QuitCommand;
	HMENU MainMenu, SubMenu;
	ObjectTextRecord2 ReferenceText;
	CompRecord *Comp;
	uint8 *SelectedNets;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], *windir, ExeFile[MAX_LENGTH_STRING * 2],
	     ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;

	if ((WParam >= ID_EXT_RESOURCE) && (WParam < ID_EXT_RESOURCE + 4096))
		WParam = GetExtendedResource(WParam - ID_EXT_RESOURCE);

	if (SystemBusyMode != 0)
		res = 1;

	QuitCommand = 0;

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
	case ID_VIEW_OPTIONS:
	case ID_EDIT_ZERORELATIVECURSOR:
	case ID_HELP_ON_COMMAND:
	case ID_VIEW_CENTER_ON_COMPONENT:
	case ID_VIEW_COMPREF_ON_OFF:
	case ID_VIEW_COMPVALUE_ON_OFF:
	case ID_VIEW_DRILLS_ON_OFF:
	case ID_SETTINGS_GRID:
	case ID_SETTINGS_CHANGE_UNITS:
	case ID_SETTINGS_UNITS_MILS:
	case ID_SETTINGS_UNITS_MM:
	case ID_SWITCH_CLEARANCE_ON_OF:
	case ID_SWITCH_NEAREST_LAYER:
	case ID_CHANGE_SNAP_MODE_ON:
	case ID_CHANGE_SNAP_MODE_OFF:
	case ID_VIEW_CROSS_TYPE1:
	case ID_VIEW_CROSS_TYPE2:
		ok = 1;
		break;

	default:
		if (SystemBusyMode == 0)
		{
			if ((WParam < ID_SELECT_LAYER + ID_SWITCH_TO_LAYER)
			        && (WParam >= ID_SELECT_LAYER + ID_SWITCH_TO_LAYER + 0x100))
				QuitCommand = 1;
		}
		else
		{
			res = 1;

			switch (SystemBusyMode)
			{
			case 1:			// Routing traces
				if ((WParam >= ID_SELECT_LAYER + ID_SWITCH_TO_LAYER)
				        && (WParam < ID_SELECT_LAYER + ID_SWITCH_TO_LAYER + 0x100))
					break;

				if ((WParam >= ID_CLEARANCE_WIDTH) && (WParam < ID_CLEARANCE_WIDTH + 0x100))
					break;

				if ((WParam >= ID_TRACE_WIDTH) && (WParam < ID_TRACE_WIDTH + 0x100))
					break;

				switch (WParam)
				{
				case ID_TRACE_FINISH:
				case ID_TRACE_BACKWARDS:
				case ID_TRACE_WIDTH_DIALOG:
				case ID_INSERT_VIA:
				case ID_CLEARANCE_WIDTH_DIALOG:
				case ID_SWITCH_VIAINFO_ON_OFF:
				case ID_SWITCH_TWO_TRACES_ON_OFF:
				case ID_SWITCH_TRYING_CLEAR_ON_OFF:
				case ID_HIGHLIGHT_NET:
				case ID_EDIT_DELETE:
				case ID_SWITCH_NEAREST_LAYER:
				case ID_EDIT_TRACE_CHECK_OFF:
				case ID_EDIT_DESIGN_RULES_NET:
				case ID_EDIT_TEXT:
				case ID_VIEW_LAYERS:
				case ID_CHANGE_TRACE_MODE:
				case ID_TRACE_MODE_NORMAL:
				case ID_TRACE_MODE_ARC45:
				case ID_TRACE_MODE_ARC90:
				case ID_TRACE_MODE_ALL_ANGLE:
//              case ID_ACTION_DRAG_TRACE:
//              case ID_ACTION_DRAG_TRACES_VIAS:
					res = 1;
					break;

				default:
					QuitCommand = 1;
					res = 1;
				}

				break;

			case 10:			// Add object lines
			case 11:			// Add object rectangles
			case 12:			// Add object circles
			case 13:			// Add object arcs
			case 14:			// Add object texts
				switch (WParam)
				{
				case ID_LINES_45_DIR:
				case ID_LINES_ALL_DIR:
					break;

				default:
					QuitCommand = 1;
					res = 1;
				}

				break;

			case 2:			// Move/drag traces/vias/components
			case 3:			// Move/drag special objects
			case 4:			// Move component refs
			case 5:			// Move component values
			case 200:			// Object selection
			case 20:			// Drag one trace
			case 30:			// Copy traces/vias
				QuitCommand = 1;
				res = 1;
				break;

			case 100:			// Add areafills, cut out of areafills
			case 110:			// Add areafills, cut out of areafills
			case 120:			// Cut from object polygons
				switch (WParam)
				{
				case ID_TRACE_BACKWARDS:
				case ID_FINISH_POLYGON:
				case ID_LINES_45_DIR:
				case ID_LINES_ALL_DIR:
				case ID_CLEARANCE_WIDTH_DIALOG:
					ok = 1;
					break;

				default:
					QuitCommand = 1;
					res = 1;
				}

				break;

			default:
				QuitCommand = 1;
				res = 1;
			}
		}
	}

	if (QuitCommand)
	{
		MessageBeep((UINT) - 1);
		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
	if ((WParam >= ID_SELECT_LAYER) && (WParam < ID_SELECT_LAYER + 0x1000))
	{
		Layer = WParam & 0x00ff;

		if (Layer < Design.NrBoardLayers)
		{
			switch (WParam & 0x0f00)
			{
			case ID_SWITCH_TO_LAYER:
				if ((ViewSingleLayer) || ((DrawLayerCode[Layer] < 0x10) && (CurrentDrawingLayer != Layer)))
				{
					LastUsedDrawingLayers[0] = CurrentDrawingLayer;
					CurrentDrawingLayer = Layer;

					if (ViewSingleLayer)
					{
						for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
						{
							DrawLayerCode[cnt] |= 0x10;
							DrawLayerCode[CurrentDrawingLayer] &= ~0x10;
						}
					}

					if ((SelectionMode == ROUTING_MODE) && (SystemBusyMode == 0))
					{
						RePaint();
						CheckInputMessages(0);

						switch (CurrentDrawingLayer)
						{
						case -1:
							sprintf(InfoStr, SC(138, "Routing traces"));
							break;

						default:
							GetLayerText(CurrentDrawingLayer, str, 3);
							sprintf(InfoStr, SC(343, "Routing traces %s"), str);
							break;
						}

						RedrawInfoStr(1);
					}
				}

				break;

			case ID_ADD_AREAFILL:
				AddNewAreaFill(Layer, 0);
				break;

			case ID_ADD_AREAFILL2:
				AddNewAreaFill(Layer, 1);
				break;

			case ID_DRAW_ONLY_ON_LAYER:
				memset(&TraceLayersSelectable[0], 0, sizeof(TraceLayersSelectable));
				TraceLayersSelectable[Layer] = -1;
				break;

			case ID_REBUILD_POWERPLANE:
//          GetAreaFillPowerPlaneToRebuild(Layer);
				break;

			case ID_CHANGE_CLEARANCE_POWERPLANE:
				break;

			case ID_CUT_POLYLINE_POWERPLANE:
				GetPowerPlaneToCutFrom(Layer, 0.0, OBJECT_POLYLINE, 0);
				break;

			case ID_CUT_CIRCLE_POWERPLANE:
				GetPowerPlaneToCutFrom(Layer, 0.0, OBJECT_CIRCLE, 0);
				break;

			case ID_CUT_RECTANGLE_POWERPLANE:
				GetPowerPlaneToCutFrom(Layer, 0.0, OBJECT_RECT, 0);
				break;

			case ID_CUT_VER_TRACE_POWERPLANE:
				GetPowerPlaneToCutFrom(Layer, 0.0, PIN_LINE_VER, 0);
				break;

			case ID_CUT_HOR_TRACE_POWERPLANE:
				GetPowerPlaneToCutFrom(Layer, 0.0, PIN_LINE_HOR, 0);
				break;

			case ID_CHANGE_POWERPLANE:
				ChangeAreaFillPowerPlane(Layer);
				break;

			case ID_CHECK_LAYERS:
				SetWaitCursor();
				CheckDesignRules(Layer, 0);
				SetNormalCursor();
				break;

			case ID_REMOVE_POWERPLANE:
				RemovePowerPlane(Layer);
//          GetPowerPlaneToCutFrom(Layer,0.0,0);
				break;

			case ID_ADD_POWERPLANE:
				AddPowerPlane(Layer);
//          GetPowerPlaneToCutFrom(Layer,0.0,0);
				break;
			}
		}

		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
	
	if ((WParam >= ID_TRACE_WIDTH) && (WParam < ID_TRACE_WIDTH + 80))
	{
		CurrentTraceWidth = TraceWidths[WParam - ID_TRACE_WIDTH];
		Net = &((*Nets)[CurrentDrawingNetNr]);

		if (Units == 0)
		{
			sprintf(InfoStr, "%s     ( %.1f , %.1f thou )", Net->Name, CurrentTraceWidth / 2540.0,
			        CurrentClearance / 2540.0);
		}
		else
		{
			sprintf(InfoStr, "zzz%s     ( %.4f , %.4f mm )", Net->Name, CurrentTraceWidth / 100000.0,
			        CurrentClearance / 100000.0);
		}

		RedrawInfoStr(1);
		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
	
	if ((WParam >= ID_CLEARANCE_WIDTH) && (WParam < ID_CLEARANCE_WIDTH + 80))
	{
		CurrentClearance = ClearanceWidths[WParam - ID_CLEARANCE_WIDTH];
		Net = &((*Nets)[CurrentDrawingNetNr]);

		if (Units == 0)
		{
			sprintf(InfoStr, "%s     ( %.1f , %.1f thou )", Net->Name, CurrentTraceWidth / 2540.0,
			        CurrentClearance / 2540.0);
		}
		else
		{
			sprintf(InfoStr, "zzz%s     ( %.4f , %.4f mm )", Net->Name, CurrentTraceWidth / 100000.0,
			        CurrentClearance / 100000.0);
		}

		RedrawInfoStr(1);
		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
	
	if ((WParam >= ID_ADD_LINE_OBJECT) && (WParam < ID_ADD_LINE_OBJECT + 0x400000))
	
	{
		Layer = WParam & 0xFFFF;
		LineWidth = (8 * 2540);

		switch (Layer)
		{
		case SILKSCREEN_BOTTOM:
		case SILKSCREEN_TOP:
		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
			LineWidth = Design.SilkScreenWidth;

			if (LineWidth == 0.0)
				LineWidth = (8 * 2540);

			break;

		case BOARD_OUTLINE_LAYER:
			LineWidth = Design.BoardOutlineWidth;

			if (LineWidth == 0.0)
				LineWidth = (8 * 2540);

			break;

		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
			LineWidth = Design.StandardTraceWidth;

			if (LineWidth == 0.0)
				LineWidth = (12 * 2540);

			break;

		case DRILL_LAYER:
		case DRILL_UNPLATED_LAYER:
			LineWidth = 0.0;
			break;

		default:
			if (Layer < 32)
			{
				LineWidth = Design.StandardTraceWidth;

				if (LineWidth == 0.0)
					LineWidth = (8 * 2540);
			}
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
			CommandAddObjectLines(LineWidth, Layer, 0);
			break;

		case ID_ADD_LINE_OBJECT_ARROW1:
			CommandAddObjectLines(LineWidth, Layer, 1);
			break;

		case ID_ADD_LINE_OBJECT_ARROW2:
			CommandAddObjectLines(LineWidth, Layer, 2);
			break;

		case ID_ADD_LINE_OBJECT_ARROW3:
			CommandAddObjectLines(LineWidth, Layer, 3);
			break;

		case ID_ADD_DIMENSION_OBJECT:
			CommandAddObjectLines(LineWidth, Layer, 4);
			break;

		case ID_ADD_DIMENSION_OBJECT2:
			CommandAddObjectLines(LineWidth, Layer, 8);
			break;

		case ID_ADD_RECT_OBJECT:
			CommandAddObjectRects(LineWidth, Layer, 0);
			break;

		case ID_ADD_RECT2_OBJECT:
			CommandAddObjectRects(LineWidth, Layer, 1);
			break;

		case ID_ADD_RECT_PAD_OBJECT:
			CommandAddObjectRects(LineWidth, Layer, 2);
			break;

		case ID_ADD_CIRCLE_PAD_OBJECT:
			CommandAddObjectArcs(LineWidth, Layer, 100, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_1:
			CommandAddObjectArcs(LineWidth, Layer, 1, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_2:
			CommandAddObjectArcs(LineWidth, Layer, 2, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_3:
			CommandAddObjectArcs(LineWidth, Layer, 3, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_4:
			CommandAddObjectArcs(LineWidth, Layer, 4, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_6:
			CommandAddObjectArcs(LineWidth, Layer, 6, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_8:
			CommandAddObjectArcs(LineWidth, Layer, 8, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_9:
			CommandAddObjectArcs(LineWidth, Layer, 9, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_C:
			CommandAddObjectArcs(LineWidth, Layer, 12, 0);
			break;

		case ID_ADD_CIRCLE_OBJECT_F:
			CommandAddObjectArcs(LineWidth, Layer, 15, 0);
			break;

		case ID_ADD_ARC_OBJECT:
			CommandAddObjectArcs(LineWidth, Layer, 0, 0);
			break;

		case ID_ADD_TEXT2_OBJECT:
			CommandAddObjectTexts2(LineWidth, Layer, 0);
			break;

		case ID_ADD_POLYGON_OBJECT:
			CommandAddObjectPolygon(0.0, Layer, 0);
			break;

		case ID_ADD_POLYLINE_OBJECT:
			CommandAddObjectPolygon(LineWidth, Layer, 0);
			break;
		}
	}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

	switch (WParam)
	{
	case ID_ACTION_DRAG_COMP_TRACE_VIAS:
	case ID_ACTION_ROUTE_TRACES:
	case ID_ACTION_DRAG_TRACE:
	case ID_ACTION_MOVE_COMPS:
	case ID_ACTION_MOVE_COMPS2:
	case ID_ACTION_MOVE_REFS:
	case ID_ACTION_MOVE_COMPVALUES:
	case ID_ACTION_OBJECTS:
	case ID_ACTION_AREAFILLS:
	case ID_ACTION_DRAG_TRACES_VIAS:
		if (SelectionMode == GATE_PINSWAP_MODE)
		{
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, (LPARAM) NULL);
			CheckInputMessages(0);
		}

		break;
	}

#ifdef KEY
	SPECIALCRCCHECK(0);
#endif


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

	if ((WParam >= ID_MOVE_OTHER_LAYER) && (WParam < ID_MOVE_OTHER_LAYER + 0x10000))
	{
		DecompressSrcDestLayer(WParam - ID_MOVE_OTHER_LAYER, &SrcLayer, &DestLayer);
		MoveCopyObjectsLayers(SrcLayer, DestLayer, 1, 2);
	}

	if ((WParam >= ID_COPY_OTHER_LAYER) && (WParam < ID_COPY_OTHER_LAYER + 0x10000))
	{
		DecompressSrcDestLayer(WParam - ID_COPY_OTHER_LAYER, &SrcLayer, &DestLayer);
		MoveCopyObjectsLayers(SrcLayer, DestLayer, 0, 2);
	}

	if ((WParam >= ID_COPY_COMP_OTHER_LAYER) && (WParam < ID_COPY_COMP_OTHER_LAYER + 0x10000))
	{
		DecompressSrcDestLayer(WParam - ID_COPY_COMP_OTHER_LAYER, &SrcLayer, &DestLayer);
		MoveCopyObjectsLayers(SrcLayer, DestLayer, 0, 1);
	}

// ********************************************************************************************************
// ********************************************************************************************************

	ok = 1;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
	switch (WParam)
	{
	case ID_ACTION_DRAG_COMP_TRACE_VIAS:
		if (SelectionMode != DRAG_TRACES_VIAS_COMPS_MODE)
		{
//        DepressButton(0,0);
//        PressButton(0,0);
		}

		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		SelectionMode = DRAG_TRACES_VIAS_COMPS_MODE;
		RepeatMode = 0;
		SetInfoStr(0);
		TempGridSize = GridSize;

		if (NotInRange(TraceGridSize, 0.0))
		{
			if (NotInRange(GridSize, TraceGridSize))
				TempGridSize = TraceGridSize;
		}

		if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			GridSize = TempGridSize;
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}
		else
			GridSize = TempGridSize;

		CurrentButtonNr = 3;
		break;

	case ID_ACTION_ROUTE_TRACES:
		if (SelectionMode != ROUTING_MODE)
		{
			DepressButton(0, 0);
			PressButton(ROUTING_MODE, 0);
		}

		LastAction = 0;
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		SelectionMode = ROUTING_MODE;
		RepeatMode = 0;
		TempGridSize = GridSize;

		if (NotInRange(TraceGridSize, 0.0))
		{
			if (NotInRange(GridSize, TraceGridSize))
				TempGridSize = TraceGridSize;
		}

		if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			GridSize = TempGridSize;
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}
		else
			GridSize = TempGridSize;

		SetInfoStr(0);
		break;

	case ID_ACTION_DRAG_TRACE:
		if (SelectionMode != MOVE_ONE_TRACE_MODE)
		{
			DepressButton(0, 0);
			PressButton(MOVE_ONE_TRACE_MODE, 0);
		}

		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		SelectionMode = MOVE_ONE_TRACE_MODE;

		if (RepeatModeActive)
			RepeatMode = 1;

		LastAction = 2;
		TempGridSize = GridSize;

		if (NotInRange(TraceGridSize, 0.0))
		{
			if (NotInRange(GridSize, TraceGridSize))
				TempGridSize = TraceGridSize;
		}

		if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			GridSize = TempGridSize;
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}
		else
			GridSize = TempGridSize;

		SetInfoStr(0);
		break;

	case ID_ACTION_MOVE_COMPS:
	case ID_ACTION_MOVE_COMPS2:
		if (SelectionMode != MOVE_COMPONENTS_MODE)
		{
			DepressButton(0, 0);
			PressButton(MOVE_COMPONENTS_MODE, 0);
		}

		LastAction = 0;
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
//      SelectViasFromRectWindow();
//      SelectTracesFromRectWindow();
		UnselectAll = 0;
		SelectionMode = MOVE_COMPONENTS_MODE;
		RepeatMode = 0;

		TempGridSize = GridSize;

		if (NotInRange(CompGridSize, 0.0))
		{
			if (NotInRange(GridSize, CompGridSize))
				TempGridSize = CompGridSize;
		}

		if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			GridSize = TempGridSize;
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}
		else
			GridSize = TempGridSize;

		SetInfoStr(0);
		break;

	case ID_ACTION_MOVE_REFS:
		if (!OkToDrawCompReference)
		{
			MessageBoxOwn(PCBWindow, SC(143, "Component references are not visible"), SC(24, "Error"),
			              MB_APPLMODAL | MB_OK);

			if (SelectionMode != MOVE_COMPONENT_REFERENCES_MODE)
			{
				DepressButton(GetButtonNr(4), 1);
				PressButton(SelectionMode, 0);
			}

			break;
		}

		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		LastAction = 0;
		RepeatMode = 0;
		SelectionMode = MOVE_COMPONENT_REFERENCES_MODE;

		if (GridVisible)
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}

		if (Units == 0)
			GridSize = 2540.0;
		else
			GridSize = 2000.0;

		if (GridVisible)
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}

		SetInfoStr(0);
		break;

	case ID_ACTION_MOVE_COMPVALUES:
		if (!OkToDrawCompValue)
		{
			MessageBoxOwn(PCBWindow, SC(144, "Component values are not visible"), SC(24, "Error"),
			              MB_APPLMODAL | MB_OK);

			if (SelectionMode != MOVE_COMPONENT_VALUES_MODE)
			{
				DepressButton(GetButtonNr(MOVE_COMPONENT_VALUES_MODE), 1);
				PressButton(SelectionMode, 0);
			}

			break;
		}

		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
//      LastAction=MOVE_COMPONENT_VALUES_MODE;
//      RepeatMode=1;
		LastAction = 0;
		RepeatMode = 0;
		SelectionMode = MOVE_COMPONENT_VALUES_MODE;

		if (GridVisible)
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}

		if (Units == 0)
			GridSize = 2540.0;
		else
			GridSize = 2000.0;

		if (GridVisible)
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}

		SetInfoStr(0);
		break;

	case ID_ACTION_OBJECTS:
		if (SelectionMode != OBJECTS_MODE)
		{
			DepressButton(0, 0);
			PressButton(OBJECTS_MODE, 0);
		}

		LastAction = 0;
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		SelectionMode = OBJECTS_MODE;
		RepeatMode = 0;
		TempGridSize = GridSize;

		if (NotInRange(UserGridSize, 0.0))
		{
			if (NotInRange(GridSize, UserGridSize))
				TempGridSize = UserGridSize;
		}

		if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			GridSize = TempGridSize;
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}
		else
			GridSize = TempGridSize;

		SetInfoStr(0);
		break;

	case ID_ACTION_AREAFILLS:
		if (SelectionMode != AREAFILLS_MODE)
		{
			DepressButton(0, 0);
			PressButton(AREAFILLS_MODE, 0);
		}

		LastAction = 0;
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		SelectionMode = AREAFILLS_MODE;
		RepeatMode = 0;
		TempGridSize = GridSize;

		if (NotInRange(AreafillGridSize, 0.0))
		{
			if (NotInRange(GridSize, AreafillGridSize))
				TempGridSize = AreafillGridSize;
		}

		if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			GridSize = TempGridSize;
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}
		else
			GridSize = TempGridSize;

		SetInfoStr(0);
		break;

	case ID_ACTION_DRAG_TRACES_VIAS:
		if (SelectionMode != MOVING_TRACES_VIAS_MODE)
		{
			DepressButton(0, 0);
			PressButton(MOVING_TRACES_VIAS_MODE, 0);
		}

		LastAction = 0;
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		SelectionMode = MOVING_TRACES_VIAS_MODE;

		if (RepeatModeActive)
			RepeatMode = 1;

		TempGridSize = GridSize;

		if (NotInRange(TraceGridSize, 0.0))
		{
			if (NotInRange(GridSize, TraceGridSize))
				TempGridSize = TraceGridSize;
		}

		if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
		{
			StartDrawingEditingWindow(BM_DirectToScreen);
			DrawGrid();
			GridSize = TempGridSize;
			DrawGrid();
			ExitDrawing();
			EndDrawingEditingWindow(BM_DirectToScreen);
		}
		else
			GridSize = TempGridSize;

		SetInfoStr(0);
		break;

	case ID_ACTION_GATEPINSWAP:
		if (LoadGatePinSwapInfo(0) < 0)
		{
			sprintf(str, SC(145, "Gate/pin swap file %s\\pcb\\gatepin.swp does not exist"), DesignPath);
			MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_OK);

			if (SelectionMode != GATE_PINSWAP_MODE)
			{
				DepressButton(GetButtonNr(GATE_PINSWAP_MODE), 1);
				PressButton(SelectionMode, 0);
			}

			break;
		}

		sprintf(str, "%s\\pcb\\gatepin.ban", DesignPath);

		if (FileExistsUTF8(str) == 0)
			res = BackAnnotationDialog(0);
		else
			res = IDOK;

		if (res)
		{
			if (res == IDDELETE)
				DeleteFileUTF8(str);

			/*
			        if (SelectionMode!=GATE_PINSWAP_MODE) {
			          DepressButton(GetButtonNr(GATE_PINSWAP_MODE),1);
			          PressButton(SelectionMode,0);
			        }
			*/
			if (SelectionMode != GATE_PINSWAP_MODE)
			{
				DepressButton(0, 0);
				PressButton(GATE_PINSWAP_MODE, 0);
			}

			LastAction = 0;
			UnselectAll = 1;
			SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
			UnselectAll = 0;
			GatePinSwapReference[0] = 0;
			GatePinSwapPinName[0] = 0;
			SelectionMode = GATE_PINSWAP_MODE;
			TempGridSize = GridSize;

			if (NotInRange(UserGridSize, 0.0))
			{
				if (NotInRange(GridSize, UserGridSize))
					TempGridSize = UserGridSize;
			}

			if ((GridVisible) && (NotInRange(TempGridSize, GridSize)))
			{
				StartDrawingEditingWindow(BM_DirectToScreen);
				DrawGrid();
				GridSize = TempGridSize;
				DrawGrid();
				ExitDrawing();
				EndDrawingEditingWindow(BM_DirectToScreen);
			}
			else
				GridSize = TempGridSize;

			RepeatMode = 0;
			SelectGatePinSwap(1);
		}
		else
		{
			if (SelectionMode != GATE_PINSWAP_MODE)
			{
				DepressButton(GetButtonNr(GATE_PINSWAP_MODE), 1);
				PressButton(SelectionMode, 0);
			}
		}

		break;

	case ID_SWITCH_TRACE_MENU:
		switch (SelectionMode)
		{
		case DRAG_TRACES_VIAS_COMPS_MODE:
		case MOVE_COMPONENTS_MODE:
		case MOVE_COMPONENT_REFERENCES_MODE:
		case MOVE_COMPONENT_VALUES_MODE:
		case OBJECTS_MODE:
		case AREAFILLS_MODE:
		case MOVING_TRACES_VIAS_MODE:
		case GATE_PINSWAP_MODE:
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_ROUTE_TRACES, (LPARAM) 1);
			break;

		case ROUTING_MODE:
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_DRAG_TRACE, (LPARAM) 1);
			break;

		case MOVE_ONE_TRACE_MODE:
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_DRAG_TRACES_VIAS, (LPARAM) 1);
			break;
		}

		break;

// ********************************************************************************************************
// ********************************************************************************************************
	case ID_ESCAPE:
		SelectionEsc = 1;
		RepeatMode = 0;
		break;

	case ID_UNSELECT_ALL:
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		break;

	case ID_FILE_NEW:
		res = 0;

		if (FileChanged)
		{

			sprintf(str, SC(141, "The layout file has changed.\n\n%s\n\nDo you want to update it ?"), EditFile); //nový

			res = MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNOCANCEL);



//			res = MessageBoxOwn(PCBWindow, SC(141, "The layout file has changed.\n\nDo you want to update it ?"), //pùvodní
//				SC(1, "Message"), MB_APPLMODAL | MB_YESNOCANCEL);
			
			if (res == IDYES)
			{
				if ((res = SaveFile(0)) == -1)
				{
					MessageBoxOwn(PCBWindow, EditFile, SC(147, "Error in saving file"), MB_APPLMODAL | MB_OK);
					break;
				}
			}
			else
			{
			if (res == IDCANCEL)
					break;
			}
			
		}

		InitBoardValues(1);

		if (NewDesignDialog() != -1)
		{
			DeAllocateMemDesign();
			memmove(&Design, &NewDesign, sizeof(DesignRecord));
			LastActionNr = 1;
			MaxLastActionNr = 1;

			if (LParam == 0)
				EditFile[0] = 0;

			InitNewDesign(0);
			DeleteGraphicObjects();
			CreateDrawObjects(0);
			SetLayerColors();
			CurrentDrawingLayer = 0;
			ViewFull();
			FileChanged = 1;
			SetWindowName(1);
		}

		break;

	case ID_FILE_OPEN:
		if (SaveFile2(0) == 1)
			LoadNewFile();

		break;

	case ID_FILE_SAVE:
	case ID_GLOBAL_FILE_SAVE:
		if (SaveFile(0) == -1)
			MessageBoxOwn(PCBWindow, EditFile, SC(147, "Error in saving file"), MB_APPLMODAL | MB_OK);

		break;

	case ID_FILE_SAVEAS:
		if (SaveFile(1) == -1)
			MessageBoxOwn(PCBWindow, EditFile, SC(147, "Error in saving file"), MB_APPLMODAL | MB_OK);

		break;

	case ID_FILE_SAVEAS_GEOM:
		SaveLayoutAsGeometry(0);
		SetNormalCursor();
		break;

	case ID_FILE_OUTPUTGERBERDRILL:
		OutputGerberDrill();
		break;

	case ID_FILE_PRINTPLOTFILES:
		SetWaitCursor();
		PlotOutput(0);
		SetNormalCursor();
		break;

	case ID_FILE_OUTPUTPENPLOT:
		PenPlotOutput(0);
		break;

	case ID_FILE_OUTPUT_ODB:
		ExportToOdb(0);
		break;

	case ID_FILE_INPUT_GERBER:
		ImportTracesVias(0);
		break;

	case ID_FILE_IMPORT_COMPS:
		ImportComponentPositions(0);
		break;

	case ID_FILE_IMPORT_COMPS2:
		ImportComponentPositions(1);
		break;

	case ID_FILE_EXPORT_BITMAP:
		ExportOutput(0);
		break;

	case ID_FILE_EXPORT_PDF:
		ExportToPDF(PAPERSIZE_A4, ORIENTATION_AUTO, 0, 0);
		break;

	case ID_FILE_OUTPUT_COMPPOS:
		CompPositionOutput(0);
		break;

	case ID_FILE_PRINT:
		SetWaitCursor();
		OutputToPrinter(0);
		SetNormalCursor();
		break;

	case ID_FILE_IMPORTNETLIST:
		ImportNetList(0);
		break;

	case ID_FILE_EXIT:
		ExitProgram();
		break;

	case ID_FILE_UPDATENETLISTCOMPONENTS:
		UpdateNetList(0);
		break;

	case ID_FILE_RELOADGEOMETRIES:
		LoadShapes(1);
		DataBaseChanged = 1;
		RePaint();
		break;

	case ID_FILE_OUTPUTNETLIST:
		MakeNetlistFromDesign(1);
		break;

	case ID_FILE_IMPORT_DXF1:
		ImportDXF(INFO_LAYER);
		break;

	case ID_FILE_IMPORT_DXF2:
		ImportDXF(INFO_LAYER2);
		break;

	case ID_FILE_IMPORT_DXF3:
		ImportDXF(INFO_LAYER3);
		break;

	case ID_FILE_IMPORT_DXF4:
		ImportDXF(INFO_LAYER4);
		break;

	case ID_FILE_EXPORT_DXF:
		ExportDXF(0);
		break;

	case ID_FILE_IMPORT_BITMAP:
		ImportBitmap(0, 0);
		break;

	case ID_SELECT_BY_PARTNR:
		SearchForComponents(NULL, 16 + 0);
		break;

	case ID_SELECT_BY_GEOMETRY:
		SearchForComponents(NULL, 16 + 1);
		break;

	case ID_SELECT_BY_VALUE:
		SearchForComponents(NULL, 16 + 2);
		break;

	case ID_DESELECT_BY_PARTNR:
		SearchForComponents(NULL, 0);
		break;

	case ID_DESELECT_BY_GEOMETRY:
		SearchForComponents(NULL, 1);
		break;

	case ID_DESELECT_BY_VALUE:
		SearchForComponents(NULL, 2);
		break;

	case ID_SELECT_BY_PARTNR2:
		SearchForComponents(NULL, 32 + 16);
		break;

	case ID_SELECT_BY_GEOMETRY2:
		SearchForComponents(NULL, 32 + 16 + 1);
		break;

	case ID_SELECT_BY_VALUE2:
		SearchForComponents(NULL, 32 + 16 + 2);
		break;

	case ID_DESELECT_BY_PARTNR2:
		SearchForComponents(NULL, 32);
		break;

	case ID_DESELECT_BY_GEOMETRY2:
		SearchForComponents(NULL, 32 + 1);
		break;

	case ID_DESELECT_BY_VALUE2:
		SearchForComponents(NULL, 32 + 2);
		break;

	case ID_SELECT_COMPS_BY_LIST:
		ComponentSelectionDialog(0);
		break;

// ********************************************************************************************************
// ********************************************************************************************************
	case ID_INSERT_FROM_CLIPBOARD:
		switch (SelectionMode)
		{
		case MOVE_COMPONENTS_MODE:
			CopyComponentsFromClipBoard(0);
			break;

		case MOVING_TRACES_VIAS_MODE:
			count = CopyTracesViasFromClipBoard(0);

			if (count > 0)
				ImportTracesVias(1);

			break;

		case DRAG_TRACES_VIAS_COMPS_MODE:
		case MOVE_COMPONENT_REFERENCES_MODE:
		case MOVE_COMPONENT_VALUES_MODE:
		case OBJECTS_MODE:
		case AREAFILLS_MODE:
		case GATE_PINSWAP_MODE:
		case ROUTING_MODE:
		case MOVE_ONE_TRACE_MODE:
			break;
		}

		break;

	case ID_COPY_TO_CLIPBOARD:
		switch (SelectionMode)
		{
		case MOVE_COMPONENTS_MODE:
			CopyComponentsToClipBoard(0);
			break;

		case DRAG_TRACES_VIAS_COMPS_MODE:
			CopyTracesViasToClipBoard(0);
			break;

		case MOVE_COMPONENT_REFERENCES_MODE:
		case MOVE_COMPONENT_VALUES_MODE:
		case OBJECTS_MODE:
		case AREAFILLS_MODE:
			break;

		case MOVING_TRACES_VIAS_MODE:
			CopyTracesViasToClipBoard(0);
			break;

		case GATE_PINSWAP_MODE:
		case ROUTING_MODE:
		case MOVE_ONE_TRACE_MODE:
			break;
		}

		break;

// ********************************************************************************************************
// ********************************************************************************************************
	case ID_CHECK_CONNECTIVITY:
		SetWaitCursor();
		CheckConnectivity(0);
		SetNormalCursor();
		break;

	case ID_CHECK_CONNECTIVITYNET:
		if ((Found = SelectNetDialog()) != -1)
			ReCalcConnectionsNet((int32) Found, 1, 1);

		break;

	case ID_CHECK_LAYER_ALL:
		SetWaitCursor();
		CheckDesignRules(-1, 0);
		SetNormalCursor();
		break;

// ********************************************************************************************************
// ********************************************************************************************************
	case ID_VIEW_ZOOMIN:
		ZoomIn(LParam);
		break;

	case ID_VIEW_ZOOMOUT:
		ZoomOut(LParam);
		break;

	case ID_VIEW_PAN:
		ViewPan(LParam);
		break;

	case ID_VIEW_VIEWFULL:
		ViewFull();
		break;

	case ID_VIEW_LAYERS:
		PcbSettingsDialog(IDD_DIALOG_VIEW, 0);
		break;

	case ID_VIEW_GRIDONOFF:
		StartDrawingEditingWindow(BM_DirectToScreen);
		GridVisible = !GridVisible;
		DrawGrid();
		EndDrawingEditingWindow(BM_DirectToScreen);
		break;

	case ID_VIEW_HILITEVISIBLECONN:
		HiliteVisibleConnections(0);
		break;

	case ID_VIEW_UNHILITEVISIBLECONN:
		HiliteVisibleConnections(1);
		break;

	case ID_VIEW_OPTIONS:
		PcbSettingsDialog(IDD_DIALOG_SETTINGS, 0);
		break;

	case ID_VIEW_CHANGECOLORS:
		PcbSettingsDialog(IDD_DIALOG_COLOR, 0);
		break;

	case ID_VIEW_DRILLS_ON_OFF:
		switch (DrawDrillMode)
		{
		case 0:
			DrawDrillMode = 3;
			break;

		case 1:
		case 2:
		case 3:
			DrawDrillMode = 0;
			break;
		}

		RePaint();
		break;

	case ID_VIEW_LOADDEFAULTCOLORS:
		DeleteGraphicObjects();
		LoadDefaultColors();
		CreateDrawObjects(0);
		RePaint();
		break;

	case ID_VIEW_SELECTERROR:
		if (NrErrorObjects > 1)
		{
			if ((Found = SelectErrorDialog()) > -2)
			{
				CurrentErrorNr = Found;
				ViewDesignRuleError(0);
			}
		}

		break;

	case ID_VIEW_INFOSELECTEDOBJECTS:
		GetInfoSelectedObjects(0);
		break;

	case ID_VIEW_VIEWALLCONNECTIONS:
		ViewAllConnections();
		break;

	case ID_VIEW_HIDEALLCONNECTIONS:
		UnViewAllConnections();
		break;

	case ID_VIEW_UNHILITEALL:
		UnhiliteTraces();
		UnhiliteVias();
		UnhiliteComps();
		UnhiliteConnections();
		UnhiliteNets();
		RePaint();
		break;

	case ID_VIEW_HILITEDNETS:
		PcbSettingsDialog(IDD_DIALOG_NETINFO, 0);
//      HiliteNetsDialog();
		break;

	case ID_VIEW_HIDECONNECTIONSNETS:
		PcbSettingsDialog(IDD_DIALOG_NETINFO, 0);
//      HideConnectionsNetsDialog();
		break;

	case ID_VIEW_DISABLECONNECTIONSNETS:
		PcbSettingsDialog(IDD_DIALOG_NETINFO, 0);
//		DisableConnectionsNetsDialog();
		break;

	case ID_VIEW_UNSELECTTRACESVIASNET:
		PcbSettingsDialog(IDD_DIALOG_NETINFO, 0); //zmìna, pùvodní bylo "1"
//      UnselectTracesViasNetDialog();
		break;

	case ID_VIEW_VIEWWITH300DPI:
		Factor = (300.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH360DPI:
		Factor = (360.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH600DPI:
		Factor = (600.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH720DPI:
		Factor = (720.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH1200DPI:
		Factor = (1200.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH1000DPI:
		Factor = (1000.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH1440DPI:
		Factor = (1440.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH2000DPI:
		Factor = (2000.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_VIEWWITH2400DPI:
		Factor = (2400.0 / 2540000.0);
		RePaint();
		break;

	case ID_VIEW_CROSS_TYPE1:
		CrossHairType = 0;
		break;

	case ID_VIEW_CROSS_TYPE2:
		CrossHairType = 1;
		break;

	case ID_VIEW_CENTER_ON_COMPONENT:
		switch (LParam)
		{
		case 0:
		case 1:
			memset(&ReferenceText, 0, sizeof(ObjectTextRecord));

			if (CompRefDialog(&ReferenceText, SC(149, "Center view on component")) != 1)
				break;

			break;

		case 2:
			strcpy(ReferenceText.Text, SearchForReferenceString);
			break;

		case 3:
			strcpy(ReferenceText.Text, ProjectInfo->TempStr1);
			break;

		case 4:
			strcpy(ReferenceText.Text, SearchForReferenceString);
			break;
		}

		if ((CompNr = FindCompNr(ReferenceText.Text, 0)) != -1)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);

			if ((SelectionMode == MOVE_COMPONENTS_MODE) && ((Comp->Info & (COMPONENT_PROTECTED)) == 0))
			{
				switch (LParam)
				{
				case 2:
				case 3:
					Comp->Info |= OBJECT_SELECTED;
					break;
				}
			}

			CenterScreenOnComp(Comp);
			CheckInputMessages(0);
			CheckInputMessages(0);
		}
		else
			MessageBoxOwn(PCBWindow, ReferenceText.Text, SC(150, "Component does not exist"), MB_APPLMODAL | MB_OK);

		break;

	case ID_VIEW_CENTER_ON_COMP_PARTNR:
		switch (LParam)
		{
		case 0:
		case 1:
			memset(&ReferenceText, 0, sizeof(ObjectTextRecord));

			if (CompRefDialog(&ReferenceText, SC(350, "Center view on component with partnr")) != 1)
				break;

			break;

		case 2:
			strcpy(ReferenceText.Text, SearchForPartNrString);
			break;

		case 3:
			strcpy(ReferenceText.Text, ProjectInfo->TempStr1);
			break;
		}

		if ((CompNr = FindCompNr(ReferenceText.Text, 1)) != -1)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);

			if ((Comp->Info & (COMPONENT_PROTECTED)) == 0)
			{
			}

			switch (LParam)
			{
			case 2:
			case 3:
				Comp->Info |= OBJECT_SELECTED;
				break;
			}

			CenterScreenOnComp(Comp);
			CheckInputMessages(0);
			CheckInputMessages(0);
		}
		else
			MessageBoxOwn(PCBWindow, ReferenceText.Text, SC(150, "Component does not exist"), MB_APPLMODAL | MB_OK);

		break;

	case ID_VIEW_CENTER_ON_CONNECTION:
		FindNextConnection();
		break;

	case ID_VIEW_COMPREF_ON_OFF:
		OkToDrawCompReference = !OkToDrawCompReference;
		RePaint();
		break;

	case ID_VIEW_COMPVALUE_ON_OFF:
		OkToDrawCompValue = !OkToDrawCompValue;
		RePaint();
		break;

	case ID_VIEW_DISPLAY_INFO:
		DisplayInfo(1);
		break;

	case ID_SWITCH_VIAINFO_ON_OFF:
		DrawViaInfoForTryingTrace = !DrawViaInfoForTryingTrace;
		break;

	case ID_SWITCH_TWO_TRACES_ON_OFF:
		DrawTwoTryingTraces = !DrawTwoTryingTraces;
		break;

	case ID_SWITCH_TRYING_CLEAR_ON_OFF:
		DrawClearanceForTryingTrace = !DrawClearanceForTryingTrace;
		break;

	case ID_SWITCH_CLEARANCE_ON_OF:
		OkToDrawClearances = !OkToDrawClearances;
		RePaint();
		break;

	case ID_CHANGE_SNAP_MODE_ON:
		SnapMode = 1;
		break;

	case ID_CHANGE_SNAP_MODE_OFF:
		SnapMode = 0;
		break;

// ********************************************************************************************************
// ********************************************************************************************************
	case ID_SETTINGS_CHANGE_UNITS:
		if (Units == 0)
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MM, (LPARAM) NULL);
		else
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MILS, (LPARAM) NULL);

		break;

	case ID_SETTINGS_UNITS_MILS:
		Units = 0;
		DisplayCursorPosition();
		MainMenu = GetMenu(PCBWindow);
		SubMenu = GetSubMenu(MainMenu, 2);
		CheckMenuItem(SubMenu, 0, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 1, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 0, MF_BYPOSITION | MF_CHECKED);
		DrawMenuBar(PCBWindow);

		if (SelectionMode == ROUTING_MODE)
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_TRACE_WIDTH_DIALOG, (LPARAM) 2);

		break;

	case ID_SETTINGS_UNITS_MM:
		Units = 1;
		DisplayCursorPosition();
		MainMenu = GetMenu(PCBWindow);
		SubMenu = GetSubMenu(MainMenu, 2);
		CheckMenuItem(SubMenu, 0, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 1, MF_BYPOSITION | MF_UNCHECKED);
		CheckMenuItem(SubMenu, 1, MF_BYPOSITION | MF_CHECKED);
		DrawMenuBar(PCBWindow);

		if (SelectionMode == ROUTING_MODE)
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_TRACE_WIDTH_DIALOG, (LPARAM) 2);

		break;

// ********************************************************************************************************
// ********************************************************************************************************
	case ID_HELP_TOPICS:
		Help("layout_editor.htm", 0);
		break;

	case ID_HELP_CONTENTS:
		Help("contents.htm", 0);
		break;

	case ID_HELP_ON_COMMAND:
		HelpAsked = 1;
		break;

	case ID_HELP_ABOUT:
		AboutDialog();
		break;

// ********************************************************************************************************
// ********************************************************************************************************
	case ID_ENABLE_ALLCONNECTIONS:
		AllocateSpecialMem(MEM_NET_SELECTED, (Design.NrNets + 10) * sizeof(uint8), (void **) &SelectedNets);
		DisableConnectionsNets(0, 1, NULL, SelectedNets);
		DeallocateSpecialMem(MEM_NET_SELECTED);
		break;

	case ID_NETS_TRACECLEARANCENET:
//      NetTypesDialog(-1);
		PcbSettingsDialog(IDD_DIALOG_NETTYPE, -1);
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
		DrawLineWhite(RelX, RelY + val, RelX, RelY - val, BM_MultiStart);
		DrawLineWhite(RelX + val, RelY, RelX - val, RelY, BM_MultiEnd);
		DisplayCursorPosition();
		break;

	case ID_SWAP_COMPONENTS:
		PlaceMovedObjects(0.0, 0.0, 0.0, 1);
		break;

	case ID_SWITCH_NEAREST_LAYER:
		SwitchToNearestDrawingLayer(0);
		break;

	case ID_EDIT_PROTECT_COMP:
		PcbSettingsDialog(IDD_DIALOG_COMPINFO, 0); //zmìna odkazu na chránìné komponenty
		break;

	case ID_EDIT_GEOMETRIE:
		EditShapeComponent(0);
		break;

	case ID_EDIT_TRACE_CHECK_OFF:
		TraceCheck = !TraceCheck;
		break;

	case ID_EDIT_ALIGN_COMP:
		AlignComponents(0);
		break;

	case ID_EDIT_DESIGN_RULES_NET:
		if (SelectionMode == MOVING_TRACES_VIAS_MODE)
		{
			NetNr = GetNetNrSelectedTrace();
			PcbSettingsDialog(IDD_DIALOG_NETTYPE, NetNr);
//        NetTypesDialog(NetNr);
		}

		if (SelectionMode == ROUTING_MODE)
			EditDesignRulesNetKeyPressed = 1;

		break;

	case ID_DELETE_TRACESVIASNET_FUNC:
		PcbSettingsDialog(IDD_DIALOG_NETINFO, 0); //zmìna, pùvodní bylo "2"
//      DeleteTracesViasNetDialog();
		break;

	case ID_EDIT_SCHEMATIC_ON_REF:
		EditSchematicComponent(0);
		break;

	case ID_LINES_45_DIR:
		LinesAllDirection = 0;
		break;

	case ID_LINES_ALL_DIR:
		LinesAllDirection = 1;
		break;

	case ID_DELETE_TRACESVIASNET:
		DeleteTracesViasNetSelectedTrace();
		break;

	case ID_GET_TRACELENGTH:
		NetNr = GetNetNrSelectedTrace();

		if ((NetNr >= 0) && (NetNr < Design.NrNets))
		{
			GetObjectsNet((int32) NetNr, 3, 1);
			Net = &((*Nets)[NetNr]);
			TraceLength = GetTraceLengthNet(0);

			if (TraceLength > -1)
			{
				sprintf(str, "%.1f thou  =  %.4f mm", TraceLength / 2540, TraceLength / 100000);
				sprintf(str2, SC(152, "Trace length net %s"), Net->Name);
				MessageBoxOwn(PCBWindow, str, str2, MB_APPLMODAL | MB_OK);
			}
			else
				MessageBoxOwn(PCBWindow, "", SC(153, "Error in calculating trace length"), MB_APPLMODAL | MB_OK);
		}

		break;

	case ID_ACTION_ACTIVESCHEMATICSELECT:
		// DataBaseChanged
		ActiveSchematicSelect = !ActiveSchematicSelect;
		MainMenu = GetMenu(PCBWindow);
		SubMenu = GetSubMenu(MainMenu, 6);
		CheckMenuItem(SubMenu, ID_ACTION_ACTIVESCHEMATICSELECT, MF_BYCOMMAND | MF_UNCHECKED);

		if (ActiveSchematicSelect == 1)
		{
			CheckMenuItem(SubMenu, ID_ACTION_ACTIVESCHEMATICSELECT, MF_BYCOMMAND | MF_CHECKED);

			if (GlobalFindAtom(AtomStrSchematicSelect) == 0)
			{
				LayoutEditorAtom = GlobalAddAtom(AtomStrSchematicSelect);
//          Beep(2000,500);
			}
		}
		else
			GlobalDeleteAtom(LayoutEditorAtom);

		break;

	case ID_TRACE_WIDTH_DIALOG:
		if (LParam < 2)
		{
			NewValue.Value = (float) TraceWidthUser[0];
			NewValue.MinValue = (float) (0.1 * 2540);
			NewValue.MaxValue = (float) (10000 * 2540);

			if (ValueDialog(0) == 1)
			{
				TraceWidthUser[0] = NewValue.Value;
				CurrentTraceWidth = TraceWidthUser[0];
			}
		}

		Net = &((*Nets)[CurrentDrawingNetNr]);

		if (Units == 0)
		{
			sprintf(InfoStr, "%s     ( %.1f , %.1f thou )", Net->Name, CurrentTraceWidth / 2540.0,
			        CurrentClearance / 2540.0);
		}
		else
		{
			sprintf(InfoStr, "zzz%s     ( %.4f , %.4f mm )", Net->Name, CurrentTraceWidth / 100000.0,
			        CurrentClearance / 100000.0);
		}

		RedrawInfoStr(1);
		break;

	case ID_CLEARANCE_WIDTH_DIALOG:
		if (SystemBusyMode != 100)
		{
			NewValue.Value = ClearanceWidthUser[0];
			NewValue.MinValue = (0.1 * 2540);
			NewValue.MaxValue = (10000 * 2540);

			if (ValueDialog(1) == 1)
			{
				ClearanceWidthUser[0] = NewValue.Value;
				CurrentClearance = ClearanceWidthUser[0];
			}

			Net = &((*Nets)[CurrentDrawingNetNr]);

			if (Units == 0)
			{
				sprintf(InfoStr, "%s     ( %.1f , %.1f thou )", Net->Name, CurrentTraceWidth / 2540.0,
				        CurrentClearance / 2540.0);
			}
			else
			{
				sprintf(InfoStr, "zzz%s     ( %.4f , %.4f mm )", Net->Name, CurrentTraceWidth / 100000.0,
				        CurrentClearance / 100000.0);
			}

			RedrawInfoStr(1);
		}
		else
		{
			NewValue.Value = TempAreaFillClearance;
			NewValue.MinValue = (float) (0.1 * 2540);
			NewValue.MaxValue = (float) (10000 * 2540);

			if (ValueDialog(1) == 1)
				NewAreaFillClearance = NewValue.Value;
		}

		break;

	case ID_TRACE_FINISH:
		FinishedTraceKeyPressed = 1;
		break;

	case ID_SELECT_ONLY_TRACES:
		SelectOnlyTraces();
		break;

	case ID_SELECT_ONLY_VIAS:
		SelectOnlyVias();
		break;

	case ID_SELECT_NET_VIA_TRACES:
		SelectNetViasTraces(0);
		break;

	case ID_SELECT_NET_AREAFILLS:
		SelectNetAreaFills(0);
		break;

	case ID_COMPREF_HIDE:
		if (OkToDrawCompReference)
			ChangeVisibilityCompRefsValues(0);

		break;

	case ID_COMPREF_VISIBLE:
		if (OkToDrawCompReference)
			ChangeVisibilityCompRefsValues(1);

		break;

	case ID_COMPREF_ONTOP:
		if (OkToDrawCompReference)
			ChangeMirrorCompRefsValues(0);

		break;

	case ID_COMPREF_ONBOTTOM:
		if (OkToDrawCompReference)
			ChangeMirrorCompRefsValues(1);

		break;

	case ID_COMPREF_HEIGHT:
		if (OkToDrawCompReference)
			ChangeTextHeightCompRefsValues(0);

		break;

	case ID_COMPREF_LINEWIDTH:
		if (OkToDrawCompReference)
			ChangeLineWidthCompRefsValues(0);

		break;

	case ID_COMPVALUE_HIDE:
		if (OkToDrawCompValue)
			ChangeVisibilityCompRefsValues(2);

		break;

	case ID_COMPVALUE_VISIBLE:
		if (OkToDrawCompValue)
			ChangeVisibilityCompRefsValues(3);

		break;

	case ID_COMPVALUE_ONTOP:
		if (OkToDrawCompValue)
			ChangeMirrorCompRefsValues(2);

		break;

	case ID_COMPVALUE_ONBOTTOM:
		if (OkToDrawCompValue)
			ChangeMirrorCompRefsValues(3);

		break;

	case ID_COMPVALUE_HEIGHT:
		if (OkToDrawCompValue)
			ChangeTextHeightCompRefsValues(2);

		break;

	case ID_COMPVALUE_LINEWIDTH:
		if (OkToDrawCompValue)
			ChangeLineWidthCompRefsValues(2);

		break;

	case ID_CHANGE_AREAFILL:
		ChangeAreaFill(0);
		break;

	case ID_ADD_VIA:
		AddAreaFillVia(0);
		break;

	case ID_AREAFILL_MERGE:
		MergeAreafills(0);
		break;

	case ID_EDIT_UNDO:
		UndoObjects();
		break;

	case ID_EDIT_REDO:
		RedoObjects();
		break;

	case ID_NEXT_ERROR:
		if ((OkToDrawErrors) && (NrErrorObjects > 0))
		{
			CurrentErrorNr++;

			if (CurrentErrorNr * 2 == NrErrorObjects)
				CurrentErrorNr = 0;

			ViewDesignRuleError(0);
			ok = -1;
		}

		break;

	case ID_EDIT_TEXT:
		switch (SelectionMode)
		{
		case MOVE_COMPONENT_VALUES_MODE:
			NrCompsSelected = GetNrCompsSelected();

			if (NrCompsSelected == 1)
			{
				if (OkToDrawCompValue)
					ChangeCompValue();
			}

			break;

		case OBJECTS_MODE:
			if (OkToDrawObjects)
				ChangeText(0);

			break;

		case ROUTING_MODE:
			EditDesignRulesNetKeyPressed = 1;
			break;
		}

		break;

	case ID_EDIT_CHANGE_DESIGN_RULES:
		PcbSettingsDialog(IDD_DIALOG_DESIGN_RULES, 0);
		break;

	case ID_VIEW_MEASUREMENT:
		Measurement(0);
		break;

	case ID_VIEW_REPAINT:
		OkToAddViewPos = 0;
		InvalidateRect(PCBWindow, NULL, 0);
		PostMessage(PCBWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
		break;

	case ID_VIEW_PREVIOUS_VIEW:
		PreviousView();
		break;

	case ID_EDIT_SELECTIONMODE:
		ReplaceSelections = !ReplaceSelections;
		break;

	case ID_EDIT_VIADEFINITION:
		PcbSettingsDialog(IDD_DIALOG_VIA, 0);
		break;

	case ID_EDIT_MOVE_PCB:
		ChangePCB(0);
		break;

	case ID_EDIT_ROTATE_PCB_90:
		RotatePCB(90.0);
		break;

	case ID_EDIT_ROTATE_PCB_180:
		RotatePCB(180.0);
		break;

	case ID_EDIT_ROTATE_PCB_270:
		RotatePCB(270.0);
		break;

	case ID_CHANGE_VIA:
		ChangeVias();
		break;

	case ID_EDIT_GOTOXY:
		GotoXY();
		break;

	case ID_EDIT_VARS:
		if (!ProjectActive)
			break;

		windir = getenv("windir");

		if (!windir)
			break;

		sprintf(ExeFile, "%s\\notepad.exe", windir);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		if (DesignFile[0] == 0)
			break;

		strcpy(str, DesignFile);
		lengte = strlen(str);

		if (lengte < 5)
			break;

		str[lengte - 4] = 0;
		sprintf(ExeParams, "\"%s\" \"%s.var\"", ExeFile, str);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		break;

	case ID_MOVE_COMPBYREF:
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		memset(&ReferenceText, 0, sizeof(ObjectTextRecord));

		if (CompRefDialog(&ReferenceText, SC(154, "Move component")) == 1)
		{
			if ((CompNr = FindCompNr(ReferenceText.Text, 0)) != -1)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);

				if ((Comp->Info & (COMPONENT_PROTECTED)) == 0)
				{
					if (SelectionMode == 3)
					{
						StartDrawingEditingWindow(BM_DoubleBuffer);
						Comp->Info |= OBJECT_SELECTED;
						SetROP2(OutputDisplay, R2_COPYPEN);
						DrawComp(Comp, 0.0, 0.0, 0, 0x200);
						ExitDrawing();
						EndDrawingEditingWindow(BM_DoubleBuffer);
						PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_MOVE_OBJECTS, (LPARAM) NULL);
					}
				}
			}
			else
				MessageBoxOwn(PCBWindow, ReferenceText.Text, SC(150, "Component does not exist"), MB_APPLMODAL | MB_OK);
		}

		break;

	case ID_MOVE_MULTIPLE_COMPS:
		if (ComponentPlacementDialog(0) == 1)
			PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, 0);

		break;

	case ID_DRAG_OBJECTS:
		switch (SelectionMode)
		{
		case DRAG_TRACES_VIAS_COMPS_MODE:
			MoveComponents(3);
			LastAction = 0;
			break;

		case MOVE_ONE_TRACE_MODE:
			MoveOneTrace(0);
			break;

		case MOVING_TRACES_VIAS_MODE:
			MoveComponents(2);
			LastAction = 0;
		}

		break;

	case ID_MOVE_OBJECTS:
		switch (SelectionMode)
		{
		case MOVE_COMPONENTS_MODE:
			if (ComponentConnectionMode == 0)
				MoveComponents(3);
			else
				MoveComponents(1);

			LastAction = MOVE_COMPONENTS_MODE;

			if (RepeatModeActive)
				RepeatMode = 1;

			break;

		case MOVE_COMPONENT_REFERENCES_MODE:
			NrReferencesSelected = GetNrReferencesSelected();
			MoveSelectedRefs(0, 0);

			if (NrReferencesSelected == 1)
				LastAction = MOVE_COMPONENT_REFERENCES_MODE;

			if (RepeatModeActive)
				RepeatMode = 1;

			break;

		case MOVE_COMPONENT_VALUES_MODE:
			NrValuesSelected = GetNrCompValuesSelected();
			MoveSelectedCompValues(0, 0);

			if (NrValuesSelected == 1)
				LastAction = MOVE_COMPONENT_VALUES_MODE;

			if (RepeatModeActive)
				RepeatMode = 1;

			break;

		case OBJECTS_MODE:
			MoveSelectedSpecialObjects(0, 1);
			LastAction = OBJECTS_MODE;

			if (RepeatModeActive)
				RepeatMode = 1;

			break;

		case AREAFILLS_MODE:
			MoveSelectedAreafill(0);
			break;
		}

		break;

	case ID_ROTATE_OBJECTS:
		switch (SelectionMode)
		{
		case MOVE_COMPONENTS_MODE:
			MoveComponents(4 + 1);
			break;

		case OBJECTS_MODE:
			PlaceRotatedSpecialObjects(0);
			break;

		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
			PlaceRotatedComponentReferences(90.0, 0);
			break;

		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			PlaceRotatedComponentValues(90.0, 0);
			break;
		}

		break;

	case ID_ROTATE_OBJECTS2:
		switch (SelectionMode)
		{
		case MOVE_COMPONENTS_MODE:
			MoveComponents(8);
			break;

		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
			PlaceRotatedComponentReferences(0.0, 2);
			break;

		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			PlaceRotatedComponentValues(0.0, 2);
			break;
		}

		break;

	case ID_ROTATE_OBJECTS3:
		switch (SelectionMode)
		{
		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
			PlaceRotatedComponentReferences(0.0, 3);
			break;

		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			PlaceRotatedComponentValues(0.0, 3);
			break;
		}

		break;

	case ID_ROTATE_COMPONENTS_EACH_45:
		PlaceMovedObjects(0.0, 0.0, 45.0, 2);
		break;

	case ID_ROTATE_COMPONENTS_EACH_90:
		PlaceMovedObjects(0.0, 0.0, 90.0, 2);
		break;

	case ID_ROTATE_COMPONENTS_EACH_ANGLE:
		PlaceMovedObjects(0.0, 0.0, 0.0, 3);
		break;

	case ID_ROTATE0:
		switch (SelectionMode)
		{
		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
			PlaceRotatedComponentReferences(0.0, 1);
			break;

		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			PlaceRotatedComponentValues(0.0, 1);
			break;
		}

		break;

	case ID_ROTATE90:
		switch (SelectionMode)
		{
		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
			PlaceRotatedComponentReferences(90.0, 1);
			break;

		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			PlaceRotatedComponentValues(90.0, 1);
			break;
		}

		break;

	case ID_ROTATE180:
		switch (SelectionMode)
		{
		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
			PlaceRotatedComponentReferences(180.0, 1);
			break;

		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			PlaceRotatedComponentValues(180.0, 1);
			break;
		}

		break;

	case ID_ROTATE270:
		switch (SelectionMode)
		{
		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
			PlaceRotatedComponentReferences(270.0, 1);
			break;

		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			PlaceRotatedComponentValues(270.0, 1);
			break;
		}

		break;

	case ID_MOVE_COMPS_GRID_UP:
		MoveComponentsOneGridPosition(0);
		break;

	case ID_MOVE_COMPS_GRID_DOWN:
		MoveComponentsOneGridPosition(1);
		break;

	case ID_MOVE_COMPS_GRID_LEFT:
		MoveComponentsOneGridPosition(2);
		break;

	case ID_MOVE_COMPS_GRID_RIGHT:
		MoveComponentsOneGridPosition(3);
		break;

	case ID_COPY_OBJECTS:
		switch (SelectionMode)
		{
		case MOVE_COMPONENTS_MODE:
			MoveSelectedSpecialObjects(2, 1);
			LastAction = 0;
			break;

		case OBJECTS_MODE:
			MoveSelectedSpecialObjects(2, 1);
			LastAction = 0;
			break;

		case MOVING_TRACES_VIAS_MODE:
			CopyTracesViasNet();
			break;

		case AREAFILLS_MODE:
			MoveSelectedAreafill(1);
			break;
		}

		break;

	case ID_COPY_ARRAY_OBJECTS:
		CopyArrayObjects(0);
		break;

	case ID_COPY_POLAR_OBJECTS:
		CopyArrayObjects(1);
		break;

	case ID_MIRRORX_OBJECTS:
		switch (SelectionMode)
		{
		case AREAFILLS_MODE:
			MirrorAreafill(0);
			break;

		case OBJECTS_MODE:
			MirrorObjects(0);
			break;
		}

		break;

	case ID_MIRRORY_OBJECTS:
		switch (SelectionMode)
		{
		case AREAFILLS_MODE:
			MirrorAreafill(1);
			break;

		case OBJECTS_MODE:
			MirrorObjects(1);
			break;
		}

		break;

	case ID_SCALE_OBJECTS:
		if (SelectionMode == OBJECTS_MODE)
			ScaleSelectedSpecialObjects(0);

		break;

	case ID_ASSIGN_OBJECTS_TO_NET:
		AssignSelectedSpecialObjectsToNet(0);
		break;

	case ID_ASSIGN_OBJECTS_TO_NET2:
		AssignSelectedSpecialObjectsToNet(1);
		break;

	case ID_MIRROR_TEXT:
		if (SelectionMode == OBJECTS_MODE)
			MirrorText(0);

	case ID_SWAP_TRACES:
		SwapNetsTraces();
		break;

	case ID_REGROUP_COMP:
		RegroupComponents(0);
		break;

	case ID_PROTECT_COMP:
		ProtectComponents();
		break;

	case ID_CHANGE_GEOMETRIE:
		if (SelectionMode == MOVE_COMPONENTS_MODE)
			ChangeGeometryComp(0);

		break;

	case ID_CUT_CIRCLE_POLYGON:
		CommandCutFromPolygon(OBJECT_CIRCLE);
		break;

	case ID_CUT_RECTANGLE_POLYGON:
		CommandCutFromPolygon(OBJECT_RECT);
		break;

	case ID_CUT_POLYLINE_POLYGON:
		CommandCutFromPolygon(OBJECT_POLYLINE);
		break;

	case ID_CUT_HOR_TRACE_POLYGON:
		CommandCutFromPolygon(PIN_LINE_HOR);
		break;

	case ID_CUT_VER_TRACE_POLYGON:
		CommandCutFromPolygon(PIN_LINE_VER);
		break;

	case ID_CALC_AREA_POLYGON:
		CommandCalculateAreaPolygon(0);
		break;

	case ID_POLYGON_TO_SOURCE_CODE:
		PolygonToSourceCode(0);
		break;
/*
	case ID_HILITE_SINGLE_NET_ON_OFF:
		Net = &((*Nets)[CurrentDrawingNetNr]);

		if ((Net->Info & OBJECT_HIGHLITED) == 0)
		{
			ChangeNetsHilite(CurrentDrawingNetNr, 1);
			Net->Info |= OBJECT_HIGHLITED;
		}
		else
		{
			ChangeNetsHilite(CurrentDrawingNetNr, 0);
			Net->Info &= ~OBJECT_HIGHLITED;
		}

		break;
*/

	case ID_CHANGE_TRACE_WIDTH:
		ChangeObjectTraceWidth();
		break;

	case ID_HIGHLIGHT_NET:
		switch (SelectionMode)
		{
		case ROUTING_MODE:
			Net = &((*Nets)[CurrentDrawingNetNr]);

			if ((Net->Info & OBJECT_HIGHLITED) == 0)
			{
				ChangeNetsHilite(CurrentDrawingNetNr, 1);
				Net->Info |= OBJECT_HIGHLITED;
			}
			else
			{
				ChangeNetsHilite(CurrentDrawingNetNr, 0);
				Net->Info &= ~OBJECT_HIGHLITED;
			}

			break;

		case MOVING_TRACES_VIAS_MODE:
			res = HighLightNet(0);

			if (res != -1)
			{
				Net = &((*Nets)[res]);

				if ((Net->Info & OBJECT_HIGHLITED) == 0)
				{
					ChangeNetsHilite(res, 1);
					Net->Info |= OBJECT_HIGHLITED;
				}
				else
				{
					ChangeNetsHilite(res, 0);
					Net->Info &= ~OBJECT_HIGHLITED;
				}
			}

			break;
		}

		break;

	case ID_CHANGE_TEXT_HEIGHT:
		ChangeTextHeight(0);
		break;

	case ID_CHANGE_TEXT:
		ChangeText(0);
		break;

	case ID_CHANGE_LINE_WIDTH:
		ChangeLineWidth(0);
		break;

	case ID_CHANGE_CIRCLE_OBJECT:
		ChangeCircleDiameter(0);
		break;

	case ID_CHANGE_ARC_OBJECT:
		ChangeArc(0);
		break;

	case ID_CHANGE_ARC_OBJECT2:
		ChangeArc(1);
		break;

	case ID_CHANGE_RECT_OBJECT:
		ChangeRectangle(0);
		break;

	case ID_CHANGE_CLEARANCE_WIDTH:
		ChangeClearance(0);
		break;

	case ID_CHANGE_CLEARANCE_AREAFILL:
		ChangeClearance(1);
		break;

	case ID_CHANGE_NET_AREAFILL:
		ChangeNetAreaFills(0);
		break;

	case ID_CHANGE_NET_VIAS:
		ChangeNetTracesVias(0);
		break;

	case ID_ADD_POLYLINE_TO_AREAFILL:
		CommandAddToAreaFill(0);
		break;

	case ID_CUT_CIRCLE_AREAFILL:
		CommandCutFromAreaFill(OBJECT_CIRCLE, 0);
		break;

	case ID_CUT_RECTANGLE_AREAFILL:
		CommandCutFromAreaFill(OBJECT_RECT, 0);
		break;

	case ID_CUT_POLYLINE_AREAFILL:
		CommandCutFromAreaFill(OBJECT_POLYLINE, 0);
		break;

	case ID_CUT_AREAFILL_START_POLYGON:
		CommandCutFromAreaFill(OBJECT_POLYLINE, 1);
		break;

	case ID_CUT_HOR_TRACE_AREAFILL:
		CommandCutFromAreaFill(PIN_LINE_HOR, 0);
		break;

	case ID_CUT_VER_TRACE_AREAFILL:
		CommandCutFromAreaFill(PIN_LINE_VER, 0);
		break;

	case ID_CUT_FROM_AREAFILL:
		break;

	case ID_REBUILD_AREAFILL:
		GetAreaFillToRebuild(0);
		break;

	case ID_REBUILD_AREAFILL2:
		GetAreaFillToRebuild(1);
		break;

	case ID_STRETCH_AREAFILL:
		MoveStretchedAreafill(0);
		break;

	case ID_AREAFILL_COPY_TO_INFO4:
		CopyAreafillStartPolygonToInfo4(0);
		break;

	case ID_AREAFILL_TO_TEXT:
		AreaFillToText(NULL, 2);
		break;

	case ID_VIEW_VERTICES:
		switch (SelectionMode)
		{
		case OBJECTS_MODE:
			ViewVerticesObjectPolygon(0);
			break;

		case AREAFILLS_MODE:
			ViewVerticesAreaFill(0);
			break;
		}

		break;

	case ID_EDIT_DELETE:
		switch (SelectionMode)
		{
		case ROUTING_MODE:
			DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 0);
			DataBaseChanged = 1;
			SelectionEsc = 1;
			TraceChanged = 1;
			break;

		default:
			DeleteObjectsSelected();
			break;
		}

		break;

	case ID_FINISH_POLYGON:
		FinishPolygon = 1;
		break;

	case ID_MOVE_COMP_TO_TOP:
		SwitchLayerComp(1);
		break;

	case ID_MOVE_COMP_TO_BOTTOM:
		SwitchLayerComp(0);
		break;

	case ID_ADD_EXTRA_TRACE:
		AddExtraTraceMode = 1;
		break;

	case ID_FIND_UNROUTED_NET:
		if (SelectionMode == ROUTING_MODE)
			ShowFirstUnroutedNet(0);

		break;

	case ID_RESET_FIRST_UNROUTED_NET:
		if (SelectionMode == ROUTING_MODE)
			ShowFirstUnroutedNet(1);

		break;

	case ID_HIDE_CONNECTIONS:
		HideSelectedConnections();
		break;

	case ID_TRACE_BACKWARDS:
		TraceBackWardsKeyPressed = 1;
		break;

	case ID_CHANGE_TRACE_MODE:
//      if (SystemBusyMode==0) {
//        PostMessage(PCBWindow,WM_COMMAND,(WPARAM)ID_ACTION_AREAFILLS,(LPARAM)NULL);
//      } else {
		ChangeTraceDrawingModeKeyPressed = 1;
//      }
		break;

	case ID_TRACE_MODE_NORMAL:
		ChangeTraceDrawingModeKeyPressed = 1;
		TraceDrawingMode = 3;
		break;

	case ID_TRACE_MODE_ARC45:
		ChangeTraceDrawingModeKeyPressed = 1;
		TraceDrawingMode = 2;
		break;

	case ID_TRACE_MODE_ARC90:
		TraceDrawingMode = 1;
		ChangeTraceDrawingModeKeyPressed = 1;
		break;

	case ID_TRACE_MODE_ALL_ANGLE:
		ChangeTraceDrawingModeKeyPressed = 1;
		TraceDrawingMode = 0;
		break;

	case ID_INSERT_VIA:
		InsertViaKeyPressed = 1;
		break;

	case ID_DRAW_ON_ALL_LAYERS:
		memset(&TraceLayersSelectable[0], 0xff, sizeof(TraceLayersSelectable));
		break;

	case ID_LAYER_INSERT:
		InsertLayer(0);
		break;

	case ID_LAYER_REMOVE:
		RemoveLayer(0);
		break;

	case ID_LAYER_SWITCH:
		SwitchLayer(0);
		break;

	case ID_MEMORY_USE:
		CalcMemory(0);
		break;

	case ID_SEL_ONLY_BOARDOUTLINE:
		SelectOnlyObjectOnLayer(BOARD_OUTLINE_LAYER, 0);
		break;

	case ID_SEL_ONLY_INFO:
		SelectOnlyObjectOnLayer(INFO_LAYER, 0);
		break;

	case ID_SEL_ONLY_SILK_TOP:
		SelectOnlyObjectOnLayer(SILKSCREEN_TOP, 0);
		break;

	case ID_SEL_ONLY_SILK_BOT:
		SelectOnlyObjectOnLayer(SILKSCREEN_BOTTOM, 0);
		break;

	case ID_SEL_ONLY_ROUT_KEEPOUT:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER1:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER2:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER3:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER4:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER5:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER6:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER7:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER8:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER9:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER10:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER11:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER12:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER13:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER14:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER15:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER16:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER17:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER18:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER19:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER20:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER21:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER22:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER23:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER24:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER25:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER26:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER27:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER28:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER29:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER30:
	case ID_SEL_ONLY_ROUT_KEEP_LAYER31:
		SelectOnlyObjectOnLayer(WParam - ID_SEL_ONLY_ROUT_KEEPOUT + ROUTING_KEEPOUT_LAYER, 0);
		break;

	case ID_SEL_ONLY_PCB_LAYER:
	case ID_SEL_ONLY_PCB_LAYER1:
	case ID_SEL_ONLY_PCB_LAYER2:
	case ID_SEL_ONLY_PCB_LAYER3:
	case ID_SEL_ONLY_PCB_LAYER4:
	case ID_SEL_ONLY_PCB_LAYER5:
	case ID_SEL_ONLY_PCB_LAYER6:
	case ID_SEL_ONLY_PCB_LAYER7:
	case ID_SEL_ONLY_PCB_LAYER8:
	case ID_SEL_ONLY_PCB_LAYER9:
	case ID_SEL_ONLY_PCB_LAYER10:
	case ID_SEL_ONLY_PCB_LAYER11:
	case ID_SEL_ONLY_PCB_LAYER12:
	case ID_SEL_ONLY_PCB_LAYER13:
	case ID_SEL_ONLY_PCB_LAYER14:
	case ID_SEL_ONLY_PCB_LAYER15:
	case ID_SEL_ONLY_PCB_LAYER16:
	case ID_SEL_ONLY_PCB_LAYER17:
	case ID_SEL_ONLY_PCB_LAYER18:
	case ID_SEL_ONLY_PCB_LAYER19:
	case ID_SEL_ONLY_PCB_LAYER20:
	case ID_SEL_ONLY_PCB_LAYER21:
	case ID_SEL_ONLY_PCB_LAYER22:
	case ID_SEL_ONLY_PCB_LAYER23:
	case ID_SEL_ONLY_PCB_LAYER24:
	case ID_SEL_ONLY_PCB_LAYER25:
	case ID_SEL_ONLY_PCB_LAYER26:
	case ID_SEL_ONLY_PCB_LAYER27:
	case ID_SEL_ONLY_PCB_LAYER28:
	case ID_SEL_ONLY_PCB_LAYER29:
	case ID_SEL_ONLY_PCB_LAYER30:
	case ID_SEL_ONLY_PCB_LAYER31:
		SelectOnlyObjectOnLayer(WParam - ID_SEL_ONLY_PCB_LAYER, 0);
		break;

	case ID_SEL_ONLY_DRILLS:
		SelectOnlyObjectOnLayer(DRILL_LAYER, 0);
		break;

	case ID_SEL_ONLY_DRILLS_UNPLATED:
		SelectOnlyObjectOnLayer(DRILL_UNPLATED_LAYER, 0);
		break;

	case ID_SEL_ONLY_INFO2:
		SelectOnlyObjectOnLayer(INFO_LAYER2, 0);
		break;

	case ID_SEL_ONLY_INFO3:
		SelectOnlyObjectOnLayer(INFO_LAYER3, 0);
		break;

	case ID_SEL_ONLY_INFO4:
		SelectOnlyObjectOnLayer(INFO_LAYER4, 0);
		break;

	case ID_SEL_ONLY_PASTE_BOTTOM:
		SelectOnlyObjectOnLayer(PASTE_MASK_BOTTOM, 0);
		break;

	case ID_SEL_ONLY_PASTE_TOP:
		SelectOnlyObjectOnLayer(PASTE_MASK_TOP, 0);
		break;

	case ID_SEL_ONLY_SOLD_BOTTOM:
		SelectOnlyObjectOnLayer(SOLD_MASK_BOTTOM, 0);
		break;

	case ID_SEL_ONLY_SOLD_TOP:
		SelectOnlyObjectOnLayer(SOLD_MASK_TOP, 0);
		break;

	case ID_SEL_ONLY_LINES:
		SelectOnlyObjects(0);
		break;

	case ID_SEL_ONLY_RECTANGLES:
		SelectOnlyObjects(1);
		break;

	case ID_SEL_ONLY_CIRCLES:
		SelectOnlyObjects(2);
		break;

	case ID_SEL_ONLY_ARCS:
		SelectOnlyObjects(3);
		break;

	case ID_SEL_ONLY_TEXTS:
		SelectOnlyObjects(4);
		break;

	case ID_SEL_ONLY_TEXTS2:
		SelectOnlyObjects(4);
		break;

	case ID_SEL_ONLY_POLYGONS:
		SelectOnlyObjects(5);
		break;

	case ID_UNSEL_BOARDOUTLINE:
		UnselectObjectOnLayer(BOARD_OUTLINE_LAYER, 0);
		break;

	case ID_UNSEL_INFO:
		UnselectObjectOnLayer(INFO_LAYER, 0);
		break;

	case ID_UNSEL_SILK_TOP:
		UnselectObjectOnLayer(SILKSCREEN_TOP, 0);
		break;

	case ID_UNSEL_SILK_BOT:
		UnselectObjectOnLayer(SILKSCREEN_BOTTOM, 0);
		break;

	case ID_UNSEL_ROUT_KEEPOUT:
	case ID_UNSEL_ROUT_KEEP_LAYER1:
	case ID_UNSEL_ROUT_KEEP_LAYER2:
	case ID_UNSEL_ROUT_KEEP_LAYER3:
	case ID_UNSEL_ROUT_KEEP_LAYER4:
	case ID_UNSEL_ROUT_KEEP_LAYER5:
	case ID_UNSEL_ROUT_KEEP_LAYER6:
	case ID_UNSEL_ROUT_KEEP_LAYER7:
	case ID_UNSEL_ROUT_KEEP_LAYER8:
	case ID_UNSEL_ROUT_KEEP_LAYER9:
	case ID_UNSEL_ROUT_KEEP_LAYER10:
	case ID_UNSEL_ROUT_KEEP_LAYER11:
	case ID_UNSEL_ROUT_KEEP_LAYER12:
	case ID_UNSEL_ROUT_KEEP_LAYER13:
	case ID_UNSEL_ROUT_KEEP_LAYER14:
	case ID_UNSEL_ROUT_KEEP_LAYER15:
	case ID_UNSEL_ROUT_KEEP_LAYER16:
	case ID_UNSEL_ROUT_KEEP_LAYER17:
	case ID_UNSEL_ROUT_KEEP_LAYER18:
	case ID_UNSEL_ROUT_KEEP_LAYER19:
	case ID_UNSEL_ROUT_KEEP_LAYER20:
	case ID_UNSEL_ROUT_KEEP_LAYER21:
	case ID_UNSEL_ROUT_KEEP_LAYER22:
	case ID_UNSEL_ROUT_KEEP_LAYER23:
	case ID_UNSEL_ROUT_KEEP_LAYER24:
	case ID_UNSEL_ROUT_KEEP_LAYER25:
	case ID_UNSEL_ROUT_KEEP_LAYER26:
	case ID_UNSEL_ROUT_KEEP_LAYER27:
	case ID_UNSEL_ROUT_KEEP_LAYER28:
	case ID_UNSEL_ROUT_KEEP_LAYER29:
	case ID_UNSEL_ROUT_KEEP_LAYER30:
	case ID_UNSEL_ROUT_KEEP_LAYER31:
		UnselectObjectOnLayer(WParam - ID_UNSEL_ROUT_KEEPOUT + ROUTING_KEEPOUT_LAYER, 0);
		break;

	case ID_UNSEL_PCB_LAYER:
	case ID_UNSEL_PCB_LAYER1:
	case ID_UNSEL_PCB_LAYER2:
	case ID_UNSEL_PCB_LAYER3:
	case ID_UNSEL_PCB_LAYER4:
	case ID_UNSEL_PCB_LAYER5:
	case ID_UNSEL_PCB_LAYER6:
	case ID_UNSEL_PCB_LAYER7:
	case ID_UNSEL_PCB_LAYER8:
	case ID_UNSEL_PCB_LAYER9:
	case ID_UNSEL_PCB_LAYER10:
	case ID_UNSEL_PCB_LAYER11:
	case ID_UNSEL_PCB_LAYER12:
	case ID_UNSEL_PCB_LAYER13:
	case ID_UNSEL_PCB_LAYER14:
	case ID_UNSEL_PCB_LAYER15:
	case ID_UNSEL_PCB_LAYER16:
	case ID_UNSEL_PCB_LAYER17:
	case ID_UNSEL_PCB_LAYER18:
	case ID_UNSEL_PCB_LAYER19:
	case ID_UNSEL_PCB_LAYER20:
	case ID_UNSEL_PCB_LAYER21:
	case ID_UNSEL_PCB_LAYER22:
	case ID_UNSEL_PCB_LAYER23:
	case ID_UNSEL_PCB_LAYER24:
	case ID_UNSEL_PCB_LAYER25:
	case ID_UNSEL_PCB_LAYER26:
	case ID_UNSEL_PCB_LAYER27:
	case ID_UNSEL_PCB_LAYER28:
	case ID_UNSEL_PCB_LAYER29:
	case ID_UNSEL_PCB_LAYER30:
	case ID_UNSEL_PCB_LAYER31:
		UnselectObjectOnLayer(WParam - ID_UNSEL_PCB_LAYER, 0);
		break;

	case ID_UNSEL_DRILLS:
		UnselectObjectOnLayer(0, 0);
		break;

	case ID_UNSEL_DRILLS_UNPLATED:
		UnselectObjectOnLayer(0, 0);
		break;

	case ID_UNSEL_INFO2:
		UnselectObjectOnLayer(INFO_LAYER2, 0);
		break;

	case ID_UNSEL_INFO3:
		UnselectObjectOnLayer(INFO_LAYER3, 0);
		break;

	case ID_UNSEL_INFO4:
		UnselectObjectOnLayer(INFO_LAYER4, 0);
		break;

	case ID_UNSEL_PASTE_BOTTOM:
		UnselectObjectOnLayer(PASTE_MASK_BOTTOM, 0);
		break;

	case ID_UNSEL_PASTE_TOP:
		UnselectObjectOnLayer(PASTE_MASK_TOP, 0);
		break;

	case ID_UNSEL_SOLD_BOTTOM:
		UnselectObjectOnLayer(SOLD_MASK_BOTTOM, 0);
		break;

	case ID_UNSEL_SOLD_TOP:
		UnselectObjectOnLayer(SOLD_MASK_TOP, 0);
		break;

	case ID_UNSEL_LINES:
		UnselectObjects(0);
		break;

	case ID_UNSEL_RECTANGLES:
		UnselectObjects(1);
		break;

	case ID_UNSEL_CIRCLES:
		UnselectObjects(2);
		break;

	case ID_UNSEL_ARCS:
		UnselectObjects(3);
		break;

	case ID_UNSEL_TEXTS:
		UnselectObjects(4);
		break;

	case ID_UNSEL_TEXTS2:
		UnselectObjects(4);
		break;

	case ID_UNSEL_POLYGONS:
		UnselectObjects(5);
		break;

	case ID_SEND_WINDOW_SIZE1:
		/*
		              PostMessage(MasterWindow,WM_COMMAND,(WPARAM)ID_SEND_WINDOW_SIZE1,
		                          (LPARAM)MAKELPARAM(RealWindow.left,RealWindow.top));
		              PostMessage(MasterWindow,WM_COMMAND,(WPARAM)ID_SEND_WINDOW_SIZE2,
		                          (LPARAM)MAKELPARAM(RealWindow.right,RealWindow.bottom));
		                                      str,GeomScreenWidth,GeomScreenHeight,
		                                      GeomStartX,GeomStartY,InitStr,ExePath);
		*/
		Size1 = LParam;
		break;

	case ID_SEND_WINDOW_SIZE2:
		Size2 = LParam;
		GeomScreenWidth = LOWORD(Size2) - LOWORD(Size1);
		GeomScreenHeight = HIWORD(Size2) - HIWORD(Size1);
		GeomStartX = LOWORD(Size1);
		GeomStartY = HIWORD(Size1);
		res = 1;
		break;

	case ID_UNSELECT_COMP_TOP:
		UnselectComp(1);
		break;

	case ID_UNSELECT_COMP_BOTTOM:
		UnselectComp(0);
		break;
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
