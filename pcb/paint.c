/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: paint.c
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
#include "paint.h"
#include "memory.h"
#include "dialogs.h"
#include "stdlib.h"
#include "stdio.h"
#include "select.h"
#include "select3.h"
#include "resource.h"
#include "resource2.h"
#include "pcb.h"
#include "io.h"
#include "import.h"
#include "files2.h"
#include "nets.h"
#include "help.h"
#include "calc3.h"
#include "calc.h"
#include "draw2.h"
#include "menus.h"
#include "stdio.h"
#include "insdel.h"
#include "calcdef.h"
#include "graphics.h"
#include "mainloop.h"
#include "print.h"
#include "commctrl.h"
#include "dialogs.h"

#define  COLOR_LIST_BOX_WIDTH    180
#define  IDC_LIST1               20500 //pøidán


int32 DialogResult, DialogMode, NrSelectedLayers, SelectedLayers[32], ErrorNr, TempUnits, SelectedObjectNr,
      SelectedObjectCode;

int32 GridChanged, ok;

double TempGridSize;

int32 DrawItemCount = 0;


extern int32 GerberMode, SelectedObjectNr, AreafillDrawMode;
//extern int32    GraphicsObjectCodes[192];
//extern int32    PCBObjectCodes[192];
extern UINT ClosingWindowMessage;
//extern COLORREF PCBColors[192];
//extern COLORREF PCBColors2[192];
extern int32 ProjectActive;



COLORREF GetNewColor(int32 mode, COLORREF InitialColor, HWND CurrentWindow);

int32 ViewAreafillObjectsDialog(int32 Mode);

int32 ViewDrillTypeDialog(int32 Mode);

int32 ViewSoldPasteObjectsDialog(int32 Mode);


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 CALLBACK ErrorDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, Layer, TabStops[9];
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     FillStr[MAX_LENGTH_STRING];
	ObjectRecord *Object1, *Object2;
	AreaFillRecord *AreaFill;
	NetRecord *Net;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetWindowTextUTF8(Dialog, SC(902, "View error"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		
		TabStops[0] = 15;
		TabStops[1] = 165;
		TabStops[2] = 195;
		TabStops[3] = 225;
		TabStops[4] = 375;
		TabStops[5] = 405;
		TabStops[6] = 435;
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETTABSTOPS, 7, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETHORIZONTALEXTENT, 1200, 0);
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) SC(903, "View all errors")));

		for (cnt = 0; cnt < NrErrorObjects / 2; cnt++)
		{
			Object1 = &((*ErrorObjects)[cnt * 2]);
			Layer = Object1->Layer;
			GetLayerTextObjects(Layer, str4, 1);

			if (Units == 0)
				sprintf(str3, "%.1f , %.1f thou", Object1->x1 / 2540.0, Object1->y1 / 2540.0);
			else
				sprintf(str3, "%.4f , %.4f mm", Object1->x1 / 100000.0, Object1->y1 / 100000.0);

			str[0] = 0;

			switch (Object1->ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
				sprintf(str, SC(906, "Trace %s\t%s"), str3, str4);
				break;

			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
				sprintf(str, SC(907, "Pad trace %s\t%s"), str3, str4);
				break;

			case PIN_PUT_THROUGH_ROUND:
				sprintf(str, SC(908, "THT pad %s\t\t"), str3);
				break;

			case PIN_PUT_THROUGH_SQUARE:
				sprintf(str, SC(909, "THT square pad %s\t\t"), str3);
				break;

			case PIN_PUT_THROUGH_POLYGON:
				sprintf(str, SC(910, "THT polygon pad %s\t\t"), str3);
				break;

			case DRILL:
				sprintf(str, SC(911, "Drill hole pad %s\t\t"), str3);
				break;

			case VIA_PUT_THROUGH_ROUND:
				sprintf(str, SC(912, "Via %s\t\t"), str3);
				break;

			case DRILL_UNPLATED:
				sprintf(str, SC(913, "Unplated drill %s\t\t"), str3);
				break;

			case PIN_SMD_ROUND:
				sprintf(str, SC(914, "Round pad %s\t%s"), str3, str4);
				break;

			case PIN_SMD_RECT:
				sprintf(str, SC(915, "Rect pad %s\t%s"), str3, str4);
				break;

			case PIN_SMD_POLYGON:
				sprintf(str, SC(916, "Polygon pad %s\t%s"), str3, str4);
				break;

			case PIN_LINE_ALL_ANGLE:
				sprintf(str, SC(917, "Pin trace %s\t%s"), str3, str4);
				break;

			case TRACE_ALL_ANGLE:
				sprintf(str, SC(906, "Trace %s\t%s"), str3, str4);
				break;

			case PIN_ARC:
				sprintf(str, SC(918, "Pin arc %s\t%s"), str3, str4);
				break;

			case TRACE_ARC:
				sprintf(str, SC(919, "Trace arc %s\t%s"), str3, str4);
				break;

			case OBJECT_LINE:
				if ((Object1->Info2 & 2) == 2)
					sprintf(str, SC(920, "Thin line %s\t"), str3);
				else
					sprintf(str, SC(921, "Board outline keep out\t\t"));

				break;

			case AREAFILL:
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object1->TraceNr]]);

				if ((AreaFill->Info & (POWERPLANE)) == POWERPLANE)
					sprintf(str, SC(922, "Powerplane\t%s"), str4);
				else
					sprintf(str, SC(923, "Areafill %s\t%s"), str3, str4);

				break;
			}

			Object2 = &((*ErrorObjects)[cnt * 2 + 1]);
			Layer = Object2->Layer;

			if (Units == 0)
				sprintf(str3, "%.1f , %.1f thou", Object2->x1 / 2540.0, Object2->y1 / 2540.0);
			else
				sprintf(str3, "%.4f , %.4f mm", Object2->x1 / 100000.0, Object2->y1 / 100000.0);

			GetLayerTextObjects(Layer, str4, 1);
			str2[0] = 0;

			switch (Object2->ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
				sprintf(str2, SC(906, "Trace %s\t%s"), str3, str4);
				break;

			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
				sprintf(str2, SC(907, "Pad trace %s\t%s"), str3, str4);
				break;

			case PIN_PUT_THROUGH_ROUND:
				sprintf(str2, SC(908, "THT pad %s\t\t"), str3);
				break;

			case PIN_PUT_THROUGH_SQUARE:
				sprintf(str2, SC(909, "THT square pad %s\t\t"), str3);
				break;

			case PIN_PUT_THROUGH_POLYGON:
				sprintf(str2, SC(910, "THT polygon pad %s\t\t"), str3);
				break;

			case DRILL:
				sprintf(str2, SC(911, "Drill hole pad %s\t\t"), str3);
				break;

			case VIA_PUT_THROUGH_ROUND:
				sprintf(str2, SC(912, "Via %s\t\t"), str3);
				break;

			case DRILL_UNPLATED:
				sprintf(str2, SC(913, "Unplated drill %s\t\t"), str3);
				break;

			case PIN_SMD_ROUND:
				sprintf(str2, SC(914, "Round pad %s\t%s"), str3, str4);
				break;

			case PIN_SMD_RECT:
				if (Object2->ObjectType2 != OBJECT_TEXT2)
					sprintf(str2, SC(915, "Rect pad %s\t%s"), str3, str4);
				else
				{
					if (Units == 0)
					{
						sprintf(str3, "%.1f , %.1f thou", (Object2->x1 - Object2->x2 * 0.5) / 2540.0,
						        (Object2->y1 - Object2->y2 * 0.5) / 2540.0);
					}
					else
					{
						sprintf(str3, "%.4f , %.4f mm", (Object2->x1 - Object2->x2 * 0.5) / 100000.0,
						        (Object2->y1 - Object2->y2 * 0.5) / 100000.0);
					}

					sprintf(str2, "Text %s\t%s", str3, str4);
				}

				break;

			case PIN_SMD_POLYGON:
				sprintf(str2, SC(916, "Polygon pad %s\t%s"), str3, str4);
				break;

			case PIN_LINE_ALL_ANGLE:
				sprintf(str2, SC(917, "Pin trace %s\t%s"), str3, str4);
				break;

			case TRACE_ALL_ANGLE:
				if (Object2->ObjectType2 != OBJECT_TEXT2)
					sprintf(str2, SC(906, "Trace %s\t%s"), str3, str4);
				else
					sprintf(str2, "Text %s\t%s", str3, str4);

				break;

			case PIN_ARC:
				sprintf(str2, SC(918, "Pin arc %s\t%s"), str3, str4);
				break;

			case TRACE_ARC:
				sprintf(str2, SC(919, "Trace arc %s\t%s"), str3, str4);
				break;

			case OBJECT_LINE:
				if ((Object2->Info2 & 2) == 2)
					sprintf(str2, SC(920, "Thin line %s\t"), str3);
				else
					sprintf(str2, SC(921, "Board outline keep out\t\t"));

				break;

			case AREAFILL:
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object2->TraceNr]]);

				if ((AreaFill->Info & (POWERPLANE)) == POWERPLANE)
					sprintf(str2, SC(922, "Powerplane\t%s"), str4);
				else
					sprintf(str2, SC(923, "Areafill %s\t%s"), str3, str4);

				break;

			case 0:
				Net = &((*Nets)[Object2->NetNr]);

				if (Object2->Test == 1)
					sprintf(str2, SC(924, "Net trace width\t%s"), Net->Name);
				else
					sprintf(str2, SC(925, "Net clearance\t%s"), Net->Name);

				break;
			}

			strcat(str, "\t");
			strcat(str, str2);

//        strcat(str," (Not completly surrounded with copper)");

			sprintf(FillStr, "E\t%s", str);

			if ((Object1->ObjectType == AREAFILL) && ((Object2->Info2 & 1) == 1))
			{
				strcat(str, SC(926, " (Not completely surrounded with copper)"));
				sprintf(FillStr, "W\t%s", str);
			}

			if ((Object2->ObjectType == AREAFILL) && ((Object1->Info2 & 1) == 1))
			{
				strcat(str, SC(926, " (Not completely surrounded with copper)"));
				sprintf(FillStr, "W\t%s", str);
			}

			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) (FillStr));
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, CurrentErrorNr + 1, 0);
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			ErrorNr = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (ErrorNr == LB_ERR)
				ErrorNr = -2;
			else
			{
				ErrorNr--;
				OkToDrawErrors = 1;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("view_design_rule_errors.htm", 0);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 SelectErrorDialog()
{
	int32 res;

	ErrorNr = -2;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ERRORS), PCBWindow, (DLGPROC) ErrorDialogBody);
	return ErrorNr;
}

void DeleteTracesViasNetDialog(void)
{
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void InitSpecialDraw(MEASUREITEMSTRUCT * MeasureItem)
{
	int32 Command;

	/*
	MEASUREITEMSTRUCT

	    UINT  CtlType;      // type of control
	    UINT  CtlID;        // combo box, list box, or button identifier
	    UINT  itemID;       // menu item, variable-height list box, or combo box identifier
	    UINT  itemWidth;    // width of menu item, in pixels
	    UINT  itemHeight;   // height of single item in list box menu, in pixels
	    DWORD itemData;     // application-defined 32-bit value
	*/
	switch (MeasureItem->CtlType)
	{
	case ODT_MENU:
		Command = MeasureItem->itemID;

		if ((Command >= ID_EXT_RESOURCE) && (Command < ID_EXT_RESOURCE + 4096))
			Command = GetExtendedResource(Command - ID_EXT_RESOURCE);

		if ((Command >= ID_SELECT_LAYER) && (Command < ID_SELECT_LAYER + 0x1000))
		{
			MeasureItem->itemWidth = 250;
			MeasureItem->itemHeight = 30;
		}
		else
		{
			switch (Command)
			{
			case IDD_MENU_ITEM1:
				switch (SelectionMode)
				{
				case DRAG_TRACES_VIAS_COMPS_MODE:
					MeasureItem->itemWidth = 210;
					break;

				case ROUTING_MODE:
					MeasureItem->itemWidth = 170;
					break;

				case MOVE_ONE_TRACE_MODE:
					MeasureItem->itemWidth = 120;
					break;

				case MOVE_COMPONENTS_MODE:
					MeasureItem->itemWidth = 180;
					break;

				case MOVE_COMPONENT_REFERENCES_MODE:
					MeasureItem->itemWidth = 170;
					break;

				case MOVE_COMPONENT_VALUES_MODE:
					MeasureItem->itemWidth = 150;
					break;

				case OBJECTS_MODE:
					MeasureItem->itemWidth = 120;
					break;

				case AREAFILLS_MODE:
					MeasureItem->itemWidth = 160;
					break;

				case MOVING_TRACES_VIAS_MODE:
					MeasureItem->itemWidth = 140;
					break;

				case GATE_PINSWAP_MODE:
					MeasureItem->itemWidth = 140;
					break;
				}

				MeasureItem->itemHeight = 32;
				break;
			}
		}

		break;

	case ODT_LISTBOX:
	case ODT_LISTVIEW:
		switch (MeasureItem->CtlID)
		{
		case IDD_COLOR_LIST1:
			MeasureItem->itemHeight = 28;
			break;

		case IDD_INSERT_LAYER_LIST:
			MeasureItem->itemHeight = 28;
			break;
		}

		break;
	}
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void DrawSpecialItem(DRAWITEMSTRUCT * DrawItem)
{
	/*
	DRAWITEMSTRUCT

	    UINT  CtlType;
	    UINT  CtlID;
	    UINT  itemID;
	    UINT  itemAction;
	    UINT  itemState;
	    HWND  hwndItem;
	    HDC   hDC;
	    RECT  rcItem;
	    DWORD itemData;
	*/

	HGDIOBJ SavePen, SaveBrush, MenuBrush, SaveFont;
	COLORREF MenuColor;
	LOGBRUSH BrushObject;
	HPEN MenuPen, WhitePen, Pen2;
	int32 Layer, ObjectNr, ItemNr, ItemNr2, ok, MenuHeadNr, Command;
	int32 Inverted = 0;
	int32 Redraw, OkToDraw;
	POINT Points[6];
	RECT NewRect;
	char TextStr[MAX_LENGTH_STRING];

	MenuColor = GraphicsObjectColor[ViewLayer1ObjectNr];
	ItemNr2 = 0;

	switch (DrawItem->CtlType)
	{
// **********************************************************************************
// Drawing user definied menus
// **********************************************************************************
	case ODT_MENU:
		Command = DrawItem->itemID;

		if ((Command >= ID_EXT_RESOURCE) && (Command < ID_EXT_RESOURCE + 4096))
			Command = GetExtendedResource(Command - ID_EXT_RESOURCE);

		if ((Command >= ID_SELECT_LAYER) && (Command < ID_SELECT_LAYER + 0x1000))
		{
			Layer = Command & 0x00ff;
			ObjectNr = DrawItem->itemData & 0x000f;
			MenuColor = GraphicsObjectColor[ViewLayer1ObjectNr + ObjectNr];

			switch (DrawItem->itemAction)
			{
			case ODA_DRAWENTIRE:
				Inverted = 0;
				break;

			case ODA_SELECT:
				if ((DrawItem->itemState & ODS_SELECTED) == 0)
					Inverted = 0;
				else
					Inverted = 1;

				break;
			}

			//  if (Inverted) MenuColor^=0x00ffffff; PolyLine

			BrushObject.lbColor = MenuColor;
			BrushObject.lbStyle = BS_SOLID;
			BrushObject.lbHatch = (LONG) NULL;

			MenuBrush = CreateBrushIndirect(&BrushObject);

			//MenuPen = CreatePen(PS_SOLID, 1, MenuColor);

			MenuPen = CreatePen(PS_SOLID, 1, MenuColor);
			SaveBrush = SelectObject(DrawItem->hDC, MenuBrush);
			SavePen = SelectObject(DrawItem->hDC, MenuPen);
			Rectangle(DrawItem->hDC, DrawItem->rcItem.left + 20, DrawItem->rcItem.top, DrawItem->rcItem.right,
			          DrawItem->rcItem.bottom);
			Points[0].x = 5;
			Points[0].y = DrawItem->rcItem.top + 8;
			Points[1].x = 5;
			Points[1].y = DrawItem->rcItem.top + 20;
			Points[2].x = 17;
			Points[2].y = DrawItem->rcItem.top + 14;
			SelectObject(DrawItem->hDC, GetStockObject(LTGRAY_BRUSH));
			SelectObject(DrawItem->hDC, GetStockObject(NULL_PEN));

//        if (!Inverted) {
//          SelectObject(DrawItem->hDC,GetStockObject(LTGRAY_BRUSH));
//        } else {
//          SelectObject(DrawItem->hDC,GetStockObject(GRAY_BRUSH));
//        }

			Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top, DrawItem->rcItem.left + 20,
			          DrawItem->rcItem.bottom);

			if (Inverted)
			{
				SelectObject(DrawItem->hDC, GetStockObject(WHITE_BRUSH));
				Polygon(DrawItem->hDC, (POINT *) & Points, 3);
			}

			/*
			        MenuFont=CreateFont(18,0,0,0,0,0,0,0,ANSI_CHARSET,
			                            OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
			                            FIXED_PITCH,"Arial");
			*/
//        MenuFont=CreateFont(16,0,0,0,0,0,0,0,ANSI_CHARSET,
//                            OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
//                            FIXED_PITCH,"Courier New");

//        SaveFont=SelectObject(DrawItem->hDC,MenuFont);
//
			SaveFont = SelectObject(DrawItem->hDC, GetStockObject(DEFAULT_GUI_FONT));

//        SetBkColor(DrawItem->hDC,MenuColor);

			SetBkColor(DrawItem->hDC, RGB(0, 0, 0));

//        SetBkColor(DrawItem->hDC,RGB(255,255,255));
//        SetTextColor(DrawItem->hDC,MenuColor);
//        SetTextColor(DrawItem->hDC,RGB(0,0,0));

			SetTextColor(DrawItem->hDC, RGB(255, 255, 255));
			GetLayerText(Layer, (LPSTR) & TextStr, 0);
			TextOutUTF8(DrawItem->hDC, 30, DrawItem->rcItem.top + 9, TextStr, strlen(TextStr));
			SelectObject(DrawItem->hDC, SaveFont);
			DeleteObject(MenuPen);
			DeleteObject(MenuBrush);

//        DeleteObject(MenuFont);

		}
		else
		{
			switch (Command)
			{
			case IDD_MENU_ITEM1:
				switch (DrawItem->itemAction)
				{
				case ODA_DRAWENTIRE:
					Inverted = 0;
					break;

				case ODA_SELECT:
					if ((DrawItem->itemState & ODS_SELECTED) == 0)
						Inverted = 0;
					else
						Inverted = 1;

					break;
				}

				MenuHeadNr = DrawItem->itemData;

				memmove(&NewRect, &DrawItem->rcItem, sizeof(RECT));
				FillRect(DrawItem->hDC, &NewRect, GetStockObject(LTGRAY_BRUSH));
				NewRect.bottom -= 3;
				NewRect.right--;
				NewRect.top++;
				NewRect.left++;
				FillRect(DrawItem->hDC, &NewRect, GetStockObject(BLACK_BRUSH));

//            NewRect.right--;
//            NewRect.bottom--;
//            NewRect.top++;
//            NewRect.left++;

				NewRect.right--;
				NewRect.bottom--;
				FillRect(DrawItem->hDC, &NewRect, GetStockObject(WHITE_BRUSH));
				NewRect.top++;
				NewRect.left++;

//            NewRect.top++;
//            NewRect.left++;

				FillRect(DrawItem->hDC, &NewRect, GetStockObject(DKGRAY_BRUSH));
				NewRect.right--;
				NewRect.bottom--;

//            NewRect.top++;
//            NewRect.left++;

				FillRect(DrawItem->hDC, &NewRect, GetStockObject(LTGRAY_BRUSH));

				/*
				            MenuFont=CreateFont(20,0,0,0,0,0,0,0,ANSI_CHARSET,
				                                OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
				                                FIXED_PITCH,"Arial");
				*/
//            MenuFont=CreateFont(16,0,0,0,0,0,0,0,0,DEFAULT_CHARSET,OUT_DEFAULT_PRECIS,
//                                CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,
//                                "MS Sans Serif");

				SaveFont = SelectObject(DrawItem->hDC, GetStockObject(DEFAULT_GUI_FONT));
				SetBkMode(DrawItem->hDC, TRANSPARENT);
				SetTextColor(DrawItem->hDC, RGB(0, 0, 0));

//            SetTextColor(DrawItem->hDC,RGB(192,192,192));
//            SetBkColor(DrawItem->hDC,RGB(255,255,255));

				//**************************************************************************************************
				//************************ kontextová šedá nabídka *************************************************
				//**************************************************************************************************

				switch (MenuHeadNr)
				{
				case 0:
					strcpy(TextStr, SC(269, "Drag traces/vias/components"));
					break;

				case 1:
					strcpy(TextStr, SC(138, "Routing traces"));
					break;

				case 2:
					strcpy(TextStr, SC(135, "Drag traces"));
					break;

				case 3:
					strcpy(TextStr, SC(134, "Move/rotate/change components"));
					break;

				case 4:
					strcpy(TextStr, SC(241, "Modify component references"));
					break;

				case 5:
					strcpy(TextStr, SC(268, "Modify component values"));
					break;

				case 6:
					strcpy(TextStr, SC(228, "Draw/change objects other layers"));
					break;

				case 7:
					strcpy(TextStr, SC(176, "Areafills/powerplanes"));
					break;

				case 8:
					strcpy(TextStr, SC(136, "Change traces/vias"));
					break;

				case 9:
					strcpy(TextStr, SC(292, "Gate/pin swap"));
					break;
				}

				TextOutUTF8(DrawItem->hDC, 13, DrawItem->rcItem.top + 6, TextStr, strlen(TextStr)); //text v šedé
				SelectObject(DrawItem->hDC, SaveFont);

//            DeleteObject(MenuFont);

				break;
			}
		}

		break;

// *******************************************************************************************************
// *******************************************************************************************************
	case ODT_LISTBOX:
		switch (DrawItem->CtlID)
		{
		case IDD_COLOR_LIST1:
			ItemNr = DrawItem->itemData;

//          ItemNr=DrawItem->itemID;

			Redraw = 0;

			switch (DrawItem->itemAction)
			{
			case ODA_DRAWENTIRE:
				if (SelectedObjectNr == -1)
					Inverted = 0;
				else
				{
					if (SelectedObjectNr == ItemNr)
						Inverted = 1;
				}

				Redraw = 1;
				break;

			case ODA_SELECT:
				if ((DrawItem->itemState & ODS_SELECTED) == 0)
					Inverted = 0;
				else
				{
					SelectedObjectNr = ItemNr;
					Inverted = 1;
				}

				Redraw = 1;
				break;

			case ODA_FOCUS:
				if (SelectedObjectNr != -1)
				{
					Redraw = 1;
					Inverted = 1;
				}
				else
				{
					if ((DrawItem->itemState & ODS_SELECTED) == ODS_SELECTED)
					{
						Inverted = 1;
						Redraw = 1;
					}
				}

				ok = 1;
				break;
			}

			if (Redraw)
			{
//            SaveFont=SelectObject(DrawItem->hDC,GetStockObject(SYSTEM_FONT));

				SaveFont = SelectObject(DrawItem->hDC, GetStockObject(DEFAULT_GUI_FONT));

//            SaveFont=SelectObject(DrawItem->hDC,GetStockObject(SYSTEM_FONT));

				SetBkColor(DrawItem->hDC, RGB(0, 0, 0));
				SetTextColor(DrawItem->hDC, RGB(255, 255, 255));
				WhitePen = CreatePen(PS_SOLID, 1, RGB(255, 255, 255));
				Pen2 = CreatePen(PS_SOLID, 2, RGB(192, 192, 192));
				SetROP2(DrawItem->hDC, R2_COPYPEN);
#ifdef _DEBUG

				if (ItemNr == 0)
					ok = 1;

#endif
				MenuBrush = GraphicsObjectBrush[ItemNr];
				MenuPen = CreatePen(PS_SOLID, 1, PCBColors[ItemNr]);
				SaveBrush = SelectObject(DrawItem->hDC, MenuBrush);
				SavePen = SelectObject(DrawItem->hDC, MenuPen);

				switch (ItemNr)
				{
				case ViewLayer1ObjectNr:
				case ViewLayer2ObjectNr:
				case ViewLayer3ObjectNr:
				case ViewLayer4ObjectNr:
				case ViewLayer5ObjectNr:
				case ViewLayer6ObjectNr:
				case ViewLayer7ObjectNr:
				case ViewLayer8ObjectNr:
				case ViewLayer9ObjectNr:
				case ViewLayer10ObjectNr:
				case ViewLayer11ObjectNr:
				case ViewLayer12ObjectNr:
				case ViewLayer13ObjectNr:
				case ViewLayer14ObjectNr:
				case ViewLayer15ObjectNr:
				case ViewLayer16ObjectNr:
				case ConnectionsObjectNr:
				case ViaPinsObjectNr:
				case ShapePinsTopObjectNr:
				case ShapePinsBottomObjectNr:
				case ShapePinsInnerObjectNr:
				case ViewLayer1InNetObjectNr:
				case ViewLayer2InNetObjectNr:
				case ViewLayer3InNetObjectNr:
				case ViewLayer4InNetObjectNr:
				case ViewLayer5InNetObjectNr:
				case ViewLayer6InNetObjectNr:
				case ViewLayer7InNetObjectNr:
				case ViewLayer8InNetObjectNr:
				case ViewLayer9InNetObjectNr:
				case ViewLayer10InNetObjectNr:
				case ViewLayer11InNetObjectNr:
				case ViewLayer12InNetObjectNr:
				case ViewLayer13InNetObjectNr:
				case ViewLayer14InNetObjectNr:
				case ViewLayer15InNetObjectNr:
				case ViewLayer16InNetObjectNr:
				case ViaPinsInNetObjectNr:
					Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top, DrawItem->rcItem.left + 90,
					          DrawItem->rcItem.bottom + 1);
					SelectObject(DrawItem->hDC, SavePen);
					SelectObject(DrawItem->hDC, SaveBrush);
					DeleteObject(MenuPen);

					switch (ItemNr)
					{
					case ViewLayer1ObjectNr:
					case ViewLayer2ObjectNr:
					case ViewLayer3ObjectNr:
					case ViewLayer4ObjectNr:
					case ViewLayer5ObjectNr:
					case ViewLayer6ObjectNr:
					case ViewLayer7ObjectNr:
					case ViewLayer8ObjectNr:
					case ViewLayer9ObjectNr:
					case ViewLayer10ObjectNr:
					case ViewLayer11ObjectNr:
					case ViewLayer12ObjectNr:
					case ViewLayer13ObjectNr:
					case ViewLayer14ObjectNr:
					case ViewLayer15ObjectNr:
					case ViewLayer16ObjectNr:
					case ViewLayer1InNetObjectNr:
					case ViewLayer2InNetObjectNr:
					case ViewLayer3InNetObjectNr:
					case ViewLayer4InNetObjectNr:
					case ViewLayer5InNetObjectNr:
					case ViewLayer6InNetObjectNr:
					case ViewLayer7InNetObjectNr:
					case ViewLayer8InNetObjectNr:
					case ViewLayer9InNetObjectNr:
					case ViewLayer10InNetObjectNr:
					case ViewLayer11InNetObjectNr:
					case ViewLayer12InNetObjectNr:
					case ViewLayer13InNetObjectNr:
					case ViewLayer14InNetObjectNr:
					case ViewLayer15InNetObjectNr:
					case ViewLayer16InNetObjectNr:
						ItemNr2 = ItemNr + 16;
						break;

					case ConnectionsObjectNr:
					case ViaPinsObjectNr:
					case ViaPinsInNetObjectNr:
						ItemNr2 = ItemNr + 1;
						break;

					case ShapePinsTopObjectNr:
					case ShapePinsBottomObjectNr:
					case ShapePinsInnerObjectNr:
						ItemNr2 = ItemNr + 3;
						break;
					}

					MenuBrush = GraphicsObjectBrush[ItemNr2];
					MenuPen = CreatePen(PS_SOLID, 1, PCBColors[ItemNr2]);

					SaveBrush = SelectObject(DrawItem->hDC, MenuBrush);
					SavePen = SelectObject(DrawItem->hDC, MenuPen);
					Rectangle(DrawItem->hDC, DrawItem->rcItem.left + 90, DrawItem->rcItem.top,
					          DrawItem->rcItem.left + COLOR_LIST_BOX_WIDTH, DrawItem->rcItem.bottom + 1);

					SetBkMode(DrawItem->hDC, OPAQUE);
					strcpy(TextStr, SC(933, "Highlighted"));
					TextOutUTF8(DrawItem->hDC, DrawItem->rcItem.left + 100, DrawItem->rcItem.top + 6, TextStr,
					        strlen(TextStr));
					break;

				default:
					Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top,
					          DrawItem->rcItem.left + COLOR_LIST_BOX_WIDTH, DrawItem->rcItem.bottom + 1);
				}

				SelectObject(DrawItem->hDC, GetStockObject(WHITE_BRUSH));
				SelectObject(DrawItem->hDC, WhitePen);
				Rectangle(DrawItem->hDC, DrawItem->rcItem.left + COLOR_LIST_BOX_WIDTH, DrawItem->rcItem.top,
				          DrawItem->rcItem.right, DrawItem->rcItem.bottom + 1);

				if (Inverted)
				{
					SelectObject(DrawItem->hDC, GetStockObject(NULL_BRUSH));
					SetROP2(DrawItem->hDC, R2_XORPEN);
					SelectObject(DrawItem->hDC, Pen2);
					Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top + 1, DrawItem->rcItem.right,
					          DrawItem->rcItem.bottom + 1);
				}

//            SaveFont=SelectObject(DrawItem->hDC,MenuFont);
				SetBkMode(DrawItem->hDC, TRANSPARENT);
//            SetBkColor(DrawItem->hDC,RGB(0,0,0));
				SetTextColor(DrawItem->hDC, RGB(0, 0, 0));
#ifdef _DEBUG

				if (ItemNr == NetPinsObject2Nr)
					ok = 1;

#endif
				Layer = ItemNr;

				switch (ItemNr)
				{
				case ViewLayer1ObjectNr:
					Layer = 0;
					break;

				case ViewLayer2ObjectNr:
					Layer = Design.NrBoardLayers - 1;
					break;

				case ViewLayer3ObjectNr:
				case ViewLayer4ObjectNr:
				case ViewLayer5ObjectNr:
				case ViewLayer6ObjectNr:
				case ViewLayer7ObjectNr:
				case ViewLayer8ObjectNr:
				case ViewLayer9ObjectNr:
				case ViewLayer10ObjectNr:
				case ViewLayer11ObjectNr:
				case ViewLayer12ObjectNr:
				case ViewLayer13ObjectNr:
				case ViewLayer14ObjectNr:
				case ViewLayer15ObjectNr:
				case ViewLayer16ObjectNr:
					Layer = ItemNr - 1;
					break;
				}

#ifdef _DEBUG

				if (ItemNr == RoutingKeepoutTopObjectNr)
					ok = 1;

#endif
				GetLayerTextObjects(Layer, TextStr, 0x104);

				switch (ItemNr)
				{
				case ViewLayer1InNetObjectNr:
				case ViewLayer2InNetObjectNr:
				case ViewLayer3InNetObjectNr:
				case ViewLayer4InNetObjectNr:
				case ViewLayer5InNetObjectNr:
				case ViewLayer6InNetObjectNr:
				case ViewLayer7InNetObjectNr:
				case ViewLayer8InNetObjectNr:
				case ViewLayer9InNetObjectNr:
				case ViewLayer10InNetObjectNr:
				case ViewLayer11InNetObjectNr:
				case ViewLayer12InNetObjectNr:
				case ViewLayer13InNetObjectNr:
				case ViewLayer14InNetObjectNr:
				case ViewLayer15InNetObjectNr:
				case ViewLayer16InNetObjectNr:
				case ViaPinsInNetObjectNr:
					strcat(TextStr, SC(934, " ( In a net )"));
					break;
				}

				TextOutUTF8(DrawItem->hDC, COLOR_LIST_BOX_WIDTH + 5, DrawItem->rcItem.top + 6, TextStr, strlen(TextStr)); //pøeklad barvy
				SelectObject(DrawItem->hDC, SavePen);
				SelectObject(DrawItem->hDC, SaveBrush);
				SelectObject(DrawItem->hDC, SaveFont);
				DeleteObject(WhitePen);
				DeleteObject(Pen2);
				DeleteObject(MenuPen);
			}

			break;

		case IDD_INSERT_LAYER_LIST:
			DrawItemCount++;
			OkToDraw = 0;
			Layer = DrawItem->itemData;

//          Layer=DrawItem->itemID & 0x00ff;

			if (Layer == 0)
				MenuColor = GraphicsObjectColor[ViewLayer1ObjectNr];
			else
			{
				if (Layer < 100)
				{
					if (Layer == Design.NrBoardLayers - 1)
						MenuColor = GraphicsObjectColor[ViewLayer2ObjectNr];
					else
						MenuColor = GraphicsObjectColor[ViewLayer1ObjectNr + Layer + 1];
				}
				else
					MenuColor = RGB(255, 255, 255);
			}

// DRAWITEMSTRUCT
			switch (DrawItem->itemAction)
			{
			case ODA_DRAWENTIRE:
				Inverted = 0;
				OkToDraw = 1;
				break;

			case ODA_FOCUS:
				break;

			case ODA_SELECT:
				if ((DrawItem->itemState & ODS_SELECTED) == 0)
				{
					Inverted = 0;
					OkToDraw = 1;
				}
				else
				{
					Inverted = 1;
					OkToDraw = 1;
				}

				break;
			}

			//  if (Inverted) MenuColor^=0x00ffffff; PolyLine
			if (OkToDraw)
			{
				BrushObject.lbColor = MenuColor;
				BrushObject.lbStyle = BS_SOLID;
				BrushObject.lbHatch = (LONG) NULL;

				MenuBrush = CreateBrushIndirect(&BrushObject);
				MenuPen = CreatePen(PS_SOLID, 1, MenuColor);
				SaveBrush = SelectObject(DrawItem->hDC, MenuBrush);
				SavePen = SelectObject(DrawItem->hDC, MenuPen);
				Rectangle(DrawItem->hDC, DrawItem->rcItem.left + 20, DrawItem->rcItem.top, DrawItem->rcItem.right,
				          DrawItem->rcItem.bottom);
				Points[0].x = 5;
				Points[0].y = DrawItem->rcItem.top + 3;
				Points[1].x = 16;
				Points[1].y = DrawItem->rcItem.top + 3;
				Points[2].x = 16;
				Points[2].y = DrawItem->rcItem.top + 14;
				SelectObject(DrawItem->hDC, GetStockObject(WHITE_BRUSH));
				SelectObject(DrawItem->hDC, GetStockObject(NULL_PEN));
				//        if (!Inverted) {
				//          SelectObject(DrawItem->hDC,GetStockObject(LTGRAY_BRUSH));
				//        } else {
				//          SelectObject(DrawItem->hDC,GetStockObject(GRAY_BRUSH));
				//        }
				Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top, DrawItem->rcItem.left + 21,
				          DrawItem->rcItem.bottom + 1);

				if (Inverted)
				{
					SelectObject(DrawItem->hDC, GetStockObject(BLACK_BRUSH));
					Polygon(DrawItem->hDC, (POINT *) & Points, 3);
				}

				/*
				            MenuFont=CreateFont(18,0,0,0,0,0,0,0,ANSI_CHARSET,
				                                OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
				                                FIXED_PITCH,"Arial");
				*/
				//        MenuFont=CreateFont(16,0,0,0,0,0,0,0,ANSI_CHARSET,
				//                            OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
				//                            FIXED_PITCH,"Courier New");
				SaveFont = SelectObject(DrawItem->hDC, GetStockObject(DEFAULT_GUI_FONT));
				//        SetBkColor(DrawItem->hDC,MenuColor);
				SetBkColor(DrawItem->hDC, RGB(0, 0, 0));
				//        SetBkColor(DrawItem->hDC,RGB(255,255,255));
				//        SetTextColor(DrawItem->hDC,MenuColor);
				//        SetTextColor(DrawItem->hDC,RGB(0,0,0));
				SetTextColor(DrawItem->hDC, RGB(255, 255, 255));
				GetLayerText(Layer, (LPSTR) & TextStr, 0);

				if (Layer != 100)
					TextOutUTF8(DrawItem->hDC, 30, DrawItem->rcItem.top + 6, TextStr, strlen(TextStr));

				SelectObject(DrawItem->hDC, SaveFont);
//            DeleteObject(MenuFont);
			}

			break;
		}

		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK InsertLayerDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, ok;
	DRAWITEMSTRUCT TempDrawItem;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(935, "Layer insert"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(266, "Layers"));

//      for (cnt=0;cnt<Design.NrBoardLayers;cnt++) {
//        res=SendDlgItemMessageOwn(Dialog,IDD_INSERT_LAYER_LIST,LB_ADDSTRING,0,cnt);
//      }
		for (cnt = Design.NrBoardLayers - 1; cnt >= 0; cnt--)
			res = SendDlgItemMessage(Dialog, IDD_INSERT_LAYER_LIST, LB_ADDSTRING, 0, cnt);

		res = SendDlgItemMessage(Dialog, IDD_INSERT_LAYER_LIST, LB_ADDSTRING, 0, 100);
		return about;

	case WM_DRAWITEM:
		DrawSpecialItem((DRAWITEMSTRUCT *) LParam);
		break;

	case WM_MEASUREITEM:
		InitSpecialDraw((MEASUREITEMSTRUCT *) LParam);
		ok = 1;
		break;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_COLOR_LIST1:
			res = HIWORD(WParam);

			switch (HIWORD(WParam))
			{
			case LBN_SELCHANGE:
				if ((res = SendDlgItemMessageOwn(Dialog, IDD_INSERT_LAYER_LIST, LB_GETCURSEL, 0, 0)) != CB_ERR)
				{
					memset(&TempDrawItem, 0, sizeof(DRAWITEMSTRUCT));
					TempDrawItem.CtlType = ODT_LISTBOX;
					TempDrawItem.CtlID = IDD_INSERT_LAYER_LIST;
					TempDrawItem.itemData = res;
					TempDrawItem.itemAction = ODA_DRAWENTIRE;
					DrawSpecialItem(&TempDrawItem);
				}

				break;
			}

			break;

		case IDOK:
			if ((res = SendDlgItemMessageOwn(Dialog, IDD_INSERT_LAYER_LIST, LB_GETCURSEL, 0, 0)) != CB_ERR)
			{
				EndDialog(Dialog, Design.NrBoardLayers - res);
				return about;
			}

			break;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}


int32 InsertLayer(int32 mode)
{
	int32 res;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_INSERT_LAYER), PCBWindow,
	                 (DLGPROC) InsertLayerDialogBody);

	if (res == -1)
		return -1;

	AddDesignLayer(res);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 CALLBACK LayerDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(266, "Layers"));

		if (DialogMode == 0)
			sprintf(str, SC(743, "Remove layer"));
		else
			sprintf(str, SC(936, "Swap layers"));

		SetWindowTextUTF8(Dialog, str);

//      for (cnt=0;cnt<Design.NrBoardLayers;cnt++) {
//        res=SendDlgItemMessageOwn(Dialog,IDD_INSERT_LAYER_LIST,LB_ADDSTRING,0,cnt);
//      }
		for (cnt = Design.NrBoardLayers - 1; cnt >= 0; cnt--)
		{
			GetLayerText(cnt, str, 0);
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			NrSelectedLayers = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, 32, (LPARAM) & SelectedLayers);
			EndDialog(Dialog, NrSelectedLayers);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 RemoveLayer(int32 mode)
{
	int32 res;

	DialogMode = 0;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LAYER), PCBWindow, (DLGPROC) LayerDialogBody);

	if (res == -1)
		return -1;

	if (res > 1)
	{
		MessageBoxOwn(PCBWindow, SC(937, "Select only one layer"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	if (res == 1)
		RemoveDesignLayer(Design.NrBoardLayers - SelectedLayers[0] - 1);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SwitchLayer(int32 mode)
{
	int32 res;

	DialogMode = 1;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LAYER), PCBWindow, (DLGPROC) LayerDialogBody);

	if (res == -1)
		return -1;

	if (res != 2)
	{
		MessageBoxOwn(PCBWindow, SC(938, "Select two layers"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	SwitchDesignLayer(Design.NrBoardLayers - SelectedLayers[0] - 1, Design.NrBoardLayers - SelectedLayers[1] - 1);
	return 0;
}


//***********************************************************************************************
//******************************* Importovat gerber soubor **************************************
//***********************************************************************************************


int32 CALLBACK Layer2DialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(266, "Layers"));
		sprintf(str, SC(391, "Import gerber file"));

		SetWindowTextUTF8(Dialog, str);

		for (cnt = Design.NrBoardLayers - 1; cnt >= 0; cnt--)
		{
			GetLayerText(cnt, str, 0);
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		}

		res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "Info 1");
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "Info 2");
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "Info 3");
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "Info 4");
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
			EndDialog(Dialog, res);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SelectLayer(int32 mode)
{
	int32 res;

	DialogMode = 0;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LAYER2), PCBWindow, (DLGPROC) Layer2DialogBody);

	if (res == -1)
		return -1;

	if (res >= Design.NrBoardLayers)
	{
		switch (res - Design.NrBoardLayers)
		{
		case 0:
			res = INFO_LAYER;
			break;

		case 1:
			res = INFO_LAYER2;
			break;

		case 2:
			res = INFO_LAYER3;
			break;

		case 3:
			res = INFO_LAYER4;
			break;
		}
	}
	else
		res = Design.NrBoardLayers - res - 1;

	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GotoXY()
{
	float x, y;

	ObjectTextRecord2 ObjectText2;
	memset(&ObjectText2, 0, sizeof(ObjectTextRecord2));

	if (TextInputDialog(&ObjectText2, 0x10 + 6) == 1)
	{
		if (sscanf((LPSTR) & ObjectText2.Text, "%f,%f", &x, &y) == 2)
		{
			CenterScreenOnPoint(UnitsConvert(Units, x), UnitsConvert(Units, y), 0);
			RePaint();
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
