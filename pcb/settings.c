/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: settings.c
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
#include "resource.h"
#include "dialogs.h"
#include "memory.h"
#include "calcdef.h"
#include "utf8.h"
#include "calcdef.h"
#include "pcb.h"
#include "mainloop.h"
#include "calc.h"
#include "calc3.h"
#include "stdio.h"
#include "files2.h"
#include "graphics.h"
#include "help.h"
#include "draw3.h"
#include "draw2.h"
#include "files.h"
#include "nets.h"
#include "menus.h"
#include "dialogs.h"
#include "paint.h"
#include "insdel.h"


#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL                   0x020A
#endif

typedef struct
{
	LPSTR Name;
	DLGPROC TabProc;
	HWND ControlList;
	int32 ResourceID;
	int32 ActiveWhenSystemBusyMode;
	int32 DialogMode;
	HWND DialogControlHandles[32];
	int32 DialogControlIds[32];
	int32 NrDialogControls;
	int32 CurrentDialogControlNr;
}

TabInfoRecord;

int32 CALLBACK MainPcbSettingsProc(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
void PcbSettingsTabControlSetup(HWND TabControl);

//int32 CALLBACK PcbSettingsGridDialog(HWND Dialog,UINT Message,WPARAM WParam,LPARAM LParam);
int32 CALLBACK PcbSettingsViewObjectsDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
int32 CALLBACK PcbSettingsColorDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
int32 CALLBACK PcbSettingsOptionsDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
int32 CALLBACK PcbSettingsViaDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
int32 CALLBACK PcbSettingsProtectComponentsDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
int32 CALLBACK PcbSettingsNetTypeDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
int32 CALLBACK PcbSettingsNetInfoDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);
int32 CALLBACK PcbSettingsChangeDesignRulesDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);

TabInfoRecord PcbSettingsTabInfo[20], *PcbSettingsActiveTab;
char OldText[80], DisplayInfoStr[2048];
uint8 *SelectedNets;
int32 ok, TempUnits, GridChanged, DialogMode, DesignSettingsNrTabs, SelectedObjectCode, SelectedObjectNr,
      PcbSettingsNrTabs, TraceNumbering, AperTuresChanged, NrDrillLayers, SelectedColorNr, PenPlotUnits,
      EditPenPlotDiametersMode, NetInfoMode, MousePosX_S, MousePosY_S, TotalCompCount, CompSearchCnt, SpecialNetNr,
      DesignSettingsFocusWindowNr, AvailableComps, NetsSelected, PcbSettingsFocusWindowNr, *NetInfo, NewMouseWheelMessage,
      TotalNetCount, MessageToBeSend, DesignOptionsSave, PcbSettingsDialogWidth, PcbSettingsDialogHeight, OwnTimerValue;

COLORREF NewPCBColors[192];
int32 SelectColorInList;

int32 SpecialWindowInDialogStartX = 3;
int32 SpecialWindowInDialogStartY = 25;

uint32 TimerObject;

double TempGridSize, TempPenSizes[16], TempTraceWidth, TempClearance;

DesignRecord TempDesign;

RECT DialogWindowRect;
HWND PcbSettingsMainDialog;
HWND PcbSettingsTabMainControl;
RECT PcbSettingsRect, RealPcbSettingsWindow, RealPcbSettingsWindow2, InfoRect;
char SearchString1[200], SearchString2[200], SearchString3[200], CompSearchString[200];
PAINTSTRUCT PS;

int32 DialogLayerInfoStartX, DialogLayerInfoStartY, DialogLayerInfoEndX, DialogLayerInfoEndY, DialogLayerInfoPixelsX,
      DialogLayerInfoPixelsY, SearchCnt1, SearchCnt2, SearchCnt3, FirstPaintViewLayersWindow, NetDialogMode,
      LayerInfoOffsetY, LayerOffsetNr, PcbSettingsDialogMode, LeftButtonPressed_S, RightButtonPressed_S, TextStart,
      ButtonStartX, ActiveLayerOnMouse, FileTypeStartX;

extern int32 ColorMode, InvertedColorMode, OkToDrawHairLines, LastActionForDesignResized, AreafillDrawMode;
extern HGDIOBJ BackGroundBrush, BackGroundPen4, WhitePen, ButtonInfoBrush;

extern HPEN EmptyPen, ButtonLightPen, ButtonDarkShadowPen, ButtonShadowPen, ButtonHilightPen, ButtonShadowFillPen;
extern HGDIOBJ ButtonBrush, DialogBackGroundBrush;
extern HBITMAP BitmapViewSelect;
extern HBITMAP BitmapViewVisible;
extern COLORREF BackGroundColor;
extern HANDLE FileOpenBitmap;
extern HWND PCBWindow;

//**********************************************************************************************************************
//********************************************** nastaveni pcb - bez diakrtiky *****************************************
//**********************************************************************************************************************

int32 PcbSettingsDialog(int32 DialogActive, int32 mode)
{
	int32 cnt, cnt2, cnt3, res;
	MSG msg;

#ifdef _DEBUG
	
	char str[200],text[512];

#endif
	
	ok = 1;

	for (cnt = 0; cnt < 20; cnt++)
	{
		PcbSettingsTabInfo[cnt].CurrentDialogControlNr = 0;
		PcbSettingsTabInfo[cnt].NrDialogControls = 0;
		memset(&PcbSettingsTabInfo[cnt].DialogControlHandles, 0, sizeof(PcbSettingsTabInfo[cnt].DialogControlHandles));
		memset(&PcbSettingsTabInfo[cnt].DialogControlIds, 0, sizeof(PcbSettingsTabInfo[cnt].DialogControlIds));
	}

	PcbSettingsActiveTab = NULL;

	cnt = 0;

	for (cnt2 = 0; cnt2 < 2; cnt2++)
	{

		//*****************************************************************************************************
        //********************************************** Barvy ************************************************
        //*****************************************************************************************************

		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_COLOR)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_COLOR)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(36, "Colors"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsColorDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_COLOR;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_COLOR;
			cnt++;
		}

		//*****************************************************************************************************
		//********************************************** Pravidla rozlozeni ***********************************
		//*****************************************************************************************************

		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_DESIGN_RULES)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_DESIGN_RULES)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(37, "Layout rules"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsChangeDesignRulesDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_DESIGN_RULES;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_DESIGN_RULES;
			cnt++;
		}

		//*****************************************************************************************************
        //********************************************** Pruchodky ********************************************
        //*****************************************************************************************************

		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_VIA)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_VIA)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(42, "Via"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsViaDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_VIA;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_VIA;
			cnt++;
		}
		
		//*****************************************************************************************************
        //********************************************** Pravidla site ****************************************
        //*****************************************************************************************************
		
		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_NETTYPE)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_NETTYPE)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(39, "Design rules Nets"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsNetTypeDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_NETTYPE;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_NETTYPE;

			if (DialogActive != IDD_DIALOG_NETTYPE)
				SpecialNetNr = -1;
			else
				SpecialNetNr = mode;

			cnt++;
		}
		
		//*****************************************************************************************************
        //********************************************** Site/pripojeni ***************************************
        //*****************************************************************************************************

		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_NETINFO)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_NETINFO)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(164, "Nets/connections"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsNetInfoDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_NETINFO;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_NETINFO;
			NetInfoMode = mode;
			cnt++;
		}

		//*****************************************************************************************************
        //********************************************** Chranene komponenty **********************************
        //*****************************************************************************************************

		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_COMPINFO)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_COMPINFO)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(346, "Component protection"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsProtectComponentsDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_COMPINFO;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_COMPINFO;
			cnt++;
		}

		//*****************************************************************************************************
        //********************************************** Vrstvy ***********************************************
        //*****************************************************************************************************

		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_VIEW)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_VIEW)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(266, "Layers"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsViewObjectsDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_VIEW;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_VIEW;
			cnt++;
		}

		//*****************************************************************************************************
        //********************************************** Moznosti *********************************************
        //*****************************************************************************************************

		if (((cnt2 == 0) && (DialogActive != IDD_DIALOG_SETTINGS)) || ((cnt2 == 1) && (DialogActive == IDD_DIALOG_SETTINGS)))
		{
			PcbSettingsTabInfo[cnt].Name = (SC(348, "Options"));
			PcbSettingsTabInfo[cnt].TabProc = (DLGPROC) PcbSettingsOptionsDialog;
			PcbSettingsTabInfo[cnt].ResourceID = IDD_DIALOG_SETTINGS;
			PcbSettingsTabInfo[cnt].ActiveWhenSystemBusyMode = 1;
			PcbSettingsTabInfo[cnt].DialogMode = IDD_DIALOG_SETTINGS;
			cnt++;
		}
	}

	PcbSettingsNrTabs = cnt;
	PcbSettingsDialogWidth = 400;
	PcbSettingsDialogHeight = 250;


	//*********************************** vytvoøit dialogové okno *******************************************************

	PcbSettingsMainDialog =
	    CreateDialog(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PCB_SETTINGS), NULL, (DLGPROC) MainPcbSettingsProc);

	if (PcbSettingsMainDialog == NULL)
	{

		//************************************ Informoval vás o selhání *************************************************

		MessageBox(NULL, TEXT("Application failed to load!"), TEXT("Error"), MB_OK | MB_ICONEXCLAMATION);
		
		//Nastavte návratovou hodnotu
		return 0;
	}
	else
	{
		ShowWindow(PcbSettingsMainDialog, SW_SHOW);
		UpdateWindow(PcbSettingsMainDialog);
	}

	GetWindowRect(PcbSettingsMainDialog, &PcbSettingsRect);

	TimerObject = SetTimer(PcbSettingsMainDialog, 0x2b7acd78, 100, NULL);
	OwnTimerValue = 0;

//  PcbSettingsDialogWidth=DesignSettingsRect.right-DesignSettingsRect.left-150;
//  PcbSettingsDialogHeight=DesignSettingsRect.bottom-DesignSettingsRect.top-150;

//  ShowWindow(ViewplotSettingsTabInfo[0].ControlList,SW_SHOW);
	if (SystemBusyMode == 0)
		SystemBusyMode = 1001;

	while (GetMessage(&msg, NULL, 0, 0))
	{
#ifdef _DEBUG2
		
		if (msg.message != 0x113)
		{	// WM_TIMER
			sprintf(str, "PcbSettings loop: Message 0x%04x, WPARAM = %d,%d LPARAM = %d,%d\n", msg.message,
			        LOWORD(msg.wParam), HIWORD(msg.wParam), LOWORD(msg.lParam), HIWORD(msg.lParam));
//      OutputDebugString(str);
		}
		else
		{
			/*
			      OwnTimerValue++;
			      sprintf(str,"PcbSettings loop: Message 0x%04x, WPARAM = %d,%d LPARAM = %d,%d\n",
			              msg.message,LOWORD(msg.wParam),HIWORD(msg.wParam),
			              LOWORD(msg.lParam),HIWORD(msg.lParam));
			      OutputDebugString(str);
			*/
		}

#endif

		switch (msg.message)
		{
		case WM_KEYDOWN:
		case WM_KEYUP:
			if (LOWORD(msg.wParam) == 9)
			{	// TAB key
				if (msg.message == WM_KEYDOWN)
				{
					if ((PcbSettingsActiveTab) && (PcbSettingsActiveTab->TabProc == PcbSettingsNetInfoDialog)
					        && (NetInfoMode == 0))
					{
						cnt3 = PcbSettingsActiveTab->CurrentDialogControlNr;

						if (cnt3 < PcbSettingsActiveTab->NrDialogControls - 1)
							cnt3++;
						else
							cnt3 = 0;

						PcbSettingsActiveTab->CurrentDialogControlNr = cnt3;

						if (PcbSettingsActiveTab->DialogControlHandles[cnt3])
						{
							res =
							    DefDlgProc(PcbSettingsActiveTab->ControlList, WM_NEXTDLGCTL,
							               (WPARAM) PcbSettingsActiveTab->DialogControlHandles[cnt3], 1);
						}
					}
				}

				continue;
			}

			break;
		}

		if ((!IsWindow(PcbSettingsMainDialog)) || (!IsDialogMessage(PcbSettingsMainDialog, &msg)))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	SystemBusyMode = 0;

	if (MessageToBeSend)
	{
		PostMessage(PCBWindow, WM_COMMAND, MessageToBeSend, 0);
		MessageToBeSend = 0;
	}

	return (int) msg.wParam;
}

//**********************************************************************************************************************
//**********************************************************************************************************************
//**********************************************************************************************************************

int32 CALLBACK MainPcbSettingsProc(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 res, res2, cnt, cnt2;
	NMHDR *nmhdr = NULL;
#ifdef _DEBUG2
	char str[200];

	switch (Message)
	{
	case 15:
		break;

	case WM_MOUSEWHEEL:
		sprintf(str, "MainPcbSettingsProc: Message WM_MOUSEWHEEL=0x%04x, WPARAM = %d,%d LPARAM = %d,%d\n", Message,
		        LOWORD(WParam), HIWORD(WParam), LOWORD(LParam), HIWORD(LParam));
//      OutputDebugString(str);
		break;

	case WM_CHAR:
	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
	case WM_KEYUP:
	case WM_SYSKEYUP:
	case WM_SETFOCUS:
	case WM_KILLFOCUS:
		sprintf(str, "MainPcbSettingsProc: Message 0x%04x, WPARAM = %d,%d LPARAM = %d,%d\n", Message, LOWORD(WParam),
		        HIWORD(WParam), LOWORD(LParam), HIWORD(LParam));
		OutputDebugString(str);
		break;
	}

#endif

	switch (Message)
	{
	case WM_INITDIALOG:
		PcbSettingsMainDialog = Dialog;
		SetWindowTextUTF8(Dialog, SC(139, "Pcb settings"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));

		PcbSettingsTabMainControl = GetDlgItem(Dialog, IDC_TAB1);
		PcbSettingsTabControlSetup(PcbSettingsTabMainControl);

		SendDlgItemMessage(Dialog, IDC_TAB1, TCM_SETCURSEL, 0, 0);

		PcbSettingsDialogMode = IDD_DIALOG_SETTINGS;

		ok = 1;

		break;

	case WM_NOTIFY:
		nmhdr = (NMHDR *) LParam;

		if (nmhdr->hwndFrom == PcbSettingsTabMainControl)
		{
			switch (nmhdr->code)
			{
			case TCN_SELCHANGE:
				// SystemBusyMode
				res = SendMessage(nmhdr->hwndFrom, (UINT) TCM_GETCURFOCUS, 0, 0);
				res2 = SendMessage(nmhdr->hwndFrom, (UINT) TCM_GETCURSEL, 0, 0);

				if ((SystemBusyMode)
				        && (PcbSettingsTabInfo[PcbSettingsNrTabs - res2 - 1].ActiveWhenSystemBusyMode == 0))
					break;

				cnt = 0;

				while (PcbSettingsTabInfo[cnt].Name)
				{
					ShowWindow(PcbSettingsTabInfo[cnt].ControlList, SW_HIDE);
					cnt++;
				}

				PcbSettingsFocusWindowNr = PcbSettingsNrTabs - res2 - 1;
				PcbSettingsActiveTab = &PcbSettingsTabInfo[PcbSettingsFocusWindowNr];
				ShowWindow(PcbSettingsActiveTab->ControlList, SW_SHOW);
				SendMessage(PcbSettingsActiveTab->ControlList, WM_SETFOCUS, 0, 0);

				if (PcbSettingsActiveTab->TabProc == PcbSettingsNetInfoDialog)
				{
					ok = 1;
					cnt2 = PcbSettingsActiveTab->CurrentDialogControlNr;

					if (PcbSettingsActiveTab->DialogControlHandles[cnt2])
					{
						res =
						    DefDlgProc(PcbSettingsActiveTab->ControlList, WM_NEXTDLGCTL,
						               (WPARAM) PcbSettingsActiveTab->DialogControlHandles[cnt2], 1);
					}
				}

				break;
			}
		}

		break;
		/*
		    case WM_CHAR:
		    case WM_KEYDOWN:
		    case WM_SYSKEYDOWN:
		    case WM_KEYUP:
		    case WM_SYSKEYUP:
		      return 1;
		*/
#if 0

	case WM_MOUSEWHEEL:
		if (NewMouseWheelMessage)
		{
#ifdef _DEBUG
//        sprintf(str,"MainPcbSettingsProc: Send mouse wheel message\n");
//        OutputDebugString(str);
#endif
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_MOUSEWHEEL, WParam, LParam);
			NewMouseWheelMessage = 0;
			return 1;
		}

		NewMouseWheelMessage = 1;
		return 1;
#endif

	case WM_SIZE:
	case WM_MOVE:

		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			break;

		case IDCANCEL:
			SendMessage(Dialog, WM_CLOSE, 0, 0);
			break;
		}

		break;

	case WM_SETFOCUS:
		ok = 1;
		break;

	case WM_KILLFOCUS:
		ok = 1;
		break;

	case WM_CLOSE:
		cnt = 0;

		while (PcbSettingsTabInfo[cnt].Name)
		{
			DestroyWindow(PcbSettingsTabInfo[cnt].ControlList);
			cnt++;
		}

		DestroyWindow(Dialog);
		break;

	case WM_TIMER:
		OwnTimerValue++;
		break;

	case WM_DESTROY:
		PostQuitMessage(0);
		break;
	}

	return 0;
}

//**********************************************************************************************************************
//**********************************************************************************************************************
//**********************************************************************************************************************

void PcbSettingsTabControlSetup(HWND TabControl)
{
	int32 cnt, res, Width, Height, x, y;
	RECT Rect;

	TC_ITEM tcItem = { 0 };

	Width = 0;
	Height = 0;
	cnt = 0;
	tcItem.mask = TCIF_STATE | TCIF_TEXT;

	while (PcbSettingsTabInfo[cnt].Name)
	{

		tcItem.pszText = PcbSettingsTabInfo[cnt].Name;
		res = SendMessage(TabControl, (UINT) TCM_INSERTITEM, 0, (LPARAM) & tcItem);

		if (res == -1)
		{
			MessageBox(TabControl, "TabControl Insertfailed", "Error", 0);
			return;
		}

		PcbSettingsActiveTab = &PcbSettingsTabInfo[cnt];
		PcbSettingsTabInfo[cnt].ControlList =
		    CreateDialog(PCBClass.hInstance, MAKEINTRESOURCE(PcbSettingsTabInfo[cnt].ResourceID), TabControl,
		                 (DLGPROC) PcbSettingsTabInfo[cnt].TabProc);


		if (PcbSettingsTabInfo[cnt].ControlList == NULL)
		{
			// Notified your about the failure
			MessageBox(NULL, "IDD_TAB_PAGE_1 failed to load!", "Error", MB_OK | MB_ICONEXCLAMATION);
		}
		else
		{
			GetWindowRect(PcbSettingsTabInfo[cnt].ControlList, &Rect);
			Width = max(Width, Rect.right - Rect.left);
			Height = max(Height, Rect.bottom - Rect.top);
			ShowWindow(PcbSettingsTabInfo[cnt].ControlList, SW_HIDE);
			UpdateWindow(PcbSettingsTabInfo[cnt].ControlList);
		}

		cnt++;
	}

//  ShowWindow(PcbSettingsTabInfo[0].ControlList,SW_SHOW);
	ShowWindow(PcbSettingsTabInfo[PcbSettingsNrTabs - 1].ControlList, SW_SHOW);
	GetWindowRect(PcbSettingsMainDialog, &Rect);
	PcbSettingsDialogWidth = Width;
	PcbSettingsDialogHeight = Height;
	Width += SpecialWindowInDialogStartX;
	Width += GetSystemMetrics(SM_CXFIXEDFRAME) * 2;
	Height += SpecialWindowInDialogStartY;
	Height += GetSystemMetrics(SM_CYSIZE) + GetSystemMetrics(SM_CYFIXEDFRAME) * 2 + 1;
	x = GetSystemMetrics(SM_CXMAXIMIZED) / 2;
	y = GetSystemMetrics(SM_CYMAXIMIZED) / 2;
	x -= Width / 2;
	y -= Height / 2;
	MoveWindow(PcbSettingsMainDialog, x, y, Width, Height, 1);
	PcbSettingsFocusWindowNr = PcbSettingsNrTabs - 1;
	PcbSettingsActiveTab = &PcbSettingsTabInfo[PcbSettingsFocusWindowNr];
}

//**********************************************************************************************************************
//**********************************************************************************************************************
//**********************************************************************************************************************

COLORREF GetNewColor(int32 mode, COLORREF InitialColor, HWND CurrentWindow)
{
	CHOOSECOLOR ColorRecord;
	COLORREF CustomColors[16];

	memset(&ColorRecord, 0, sizeof(CHOOSECOLOR));
	ColorRecord.lStructSize = sizeof(CHOOSECOLOR);
	ColorRecord.hwndOwner = CurrentWindow;
	ColorRecord.rgbResult = InitialColor;
	ColorRecord.lpCustColors = (COLORREF *) & CustomColors;
	ColorRecord.Flags = CC_FULLOPEN | CC_RGBINIT;

	if (ChooseColor(&ColorRecord))
		return ColorRecord.rgbResult;

	return (COLORREF) - 1;
}

//**********************************************************************************************************************
//****************************************** barvy *********************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsColorDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, ok, res2, res3, cr, cg, cb, Index;
	COLORREF NewColor, TempGridColor;
	int32 DialogPixelsX, DialogPixelsY;
	RECT Rect;
	int32 Selection;

	res3 = 0;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		memmove(&NewPCBColors, &PCBColors, sizeof(PCBColors));
		SelectedObjectNr = -1;
		SelectColorInList = -1;
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(1100, "Brushes"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(1101, "Second brush color"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO17, SC(1102, "Background color"));
		SetDialogItemTextUTF8(Dialog, ID_CHANGE_COLOR, SC(880, "Change color"));
		SetDialogItemTextUTF8(Dialog, ID_CHANGE_COLOR2, SC(1103, "Change second color"));

		SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_RESETCONTENT, 0, 0);
		
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, BackGroundObjectNr);

		if (Design.NrBoardLayers > 1)
			res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ViewLayer2ObjectNr);

		if (Design.NrBoardLayers > 2)
		{
			for (cnt = Design.NrBoardLayers - 1; cnt >= 2; cnt--)
				res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ViewLayer1ObjectNr + cnt);
		}

		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ViewLayer1ObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapePinsTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapePinsBottomObjectNr);

		if (Design.NrBoardLayers > 2)
			res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapePinsInnerObjectNr);

		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, UnconnectedPadsTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, UnconnectedPadsBottomObjectNr);

		if (Design.NrBoardLayers > 2)
			res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, UnconnectedPadsInnerObjectNr);

		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapePinsDrillObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapePinsDrillUnplatedObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ConnectionsObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ViaPinsObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ViaPinsInNetObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, NetPinsObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, NetPinsObject2Nr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, SilkScreenTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, SilkScreenBottomObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ReferenceObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, CompValueObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapePlacementOutLineTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapePlacementOutLineBottomObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapeCompOutLineTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ShapeCompOutLineBottomObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ViaPinsDrillObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ObjectsInfoObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ObjectsInfo2ObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ObjectsInfo3ObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ObjectsInfo4ObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, PasteMaskTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, PasteMaskBottomObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, SoldMaskTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, SoldMaskBottomObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ClearanceObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ErrorObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, GridObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, ButtonInfoObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, BoardOutlineObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, BoardOutlineKeepOutObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, SwappablePinsGateObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, SwappableGatePinsObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, RoutingKeepoutTopObjectNr);
		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, RoutingKeepoutBottomObjectNr);

		if (Design.NrBoardLayers > 2)
			res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, RoutingKeepoutInnerObjectNr);

		res = SendDlgItemMessage(Dialog, IDD_COLOR_LIST1, LB_ADDSTRING, 0, CrossHairObjectNr);
		SelectedObjectNr = -1;

		for (cnt = 0; cnt < 32; cnt++)
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1 + cnt, BM_SETCHECK, 0, 0);

		if ((res = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETCURSEL, 0, 0)) != CB_ERR)
			ok = 1;

		if (SelectColorInList != -1)
		{
			SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_SETCURSEL, SelectColorInList, 0);
			res2 = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETITEMDATA, SelectColorInList, 0);
			Index = ((PCBObjectCodes[res2] >> 8) & 0xFF) + IDC_RADIO1;
			SendDlgItemMessageOwn(Dialog, Index, BM_SETCHECK, 1, 0);

			if (PCBColors2[res2] == (COLORREF) 0x01000000)
				SendDlgItemMessageOwn(Dialog, IDC_RADIO17, BM_SETCHECK, 1, 0);
			else
				SendDlgItemMessageOwn(Dialog, IDC_RADIO18, BM_SETCHECK, 1, 0);
		}

		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
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
			if ((res = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETCURSEL, 0, 0)) != CB_ERR)
			{
				SelectColorInList = res;

				for (cnt = 0; cnt < 18; cnt++)
					SendDlgItemMessageOwn(Dialog, IDC_RADIO1 + cnt, BM_SETCHECK, 0, 0);

				res2 = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETITEMDATA, res, 0);
				Index = ((PCBObjectCodes[res2] >> 8) & 0xFF) + IDC_RADIO1;
				SendDlgItemMessageOwn(Dialog, Index, BM_SETCHECK, 1, 0);

				if (PCBColors2[res2] == (COLORREF) 0x01000000)
					SendDlgItemMessageOwn(Dialog, IDC_RADIO17, BM_SETCHECK, 1, 0);
				else
					SendDlgItemMessageOwn(Dialog, IDC_RADIO18, BM_SETCHECK, 1, 0);
			}

			break;

		case IDC_RADIO1:
		case IDC_RADIO2:
		case IDC_RADIO3:
		case IDC_RADIO4:
		case IDC_RADIO5:
		case IDC_RADIO6:
		case IDC_RADIO7:
		case IDC_RADIO8:
		case IDC_RADIO9:
		case IDC_RADIO10:
		case IDC_RADIO11:
		case IDC_RADIO12:
		case IDC_RADIO13:
		case IDC_RADIO14:
		case IDC_RADIO15:
		case IDC_RADIO16:
			if ((Selection = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETCURSEL, 0, 0)) != CB_ERR)
			{
				SelectedObjectCode = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETITEMDATA, Selection, 0);
				Index = LOWORD(WParam) - IDC_RADIO1;
				PCBObjectCodes[SelectedObjectCode] = Index << 8;
				DeleteGraphicObjects();
				CreateDrawObjects(0);
				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				CheckInputMessages(0);
				CheckInputMessages(0);
				SendMessageOwn(Dialog, WM_INITDIALOG, 0, 0);
				SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_SETCURSEL, Selection, 0);
				SendMessageOwn(Dialog, WM_COMMAND, IDD_COLOR_LIST1, 0);
			}
			else
			{
				for (cnt = 0; cnt < 18; cnt++)
					SendDlgItemMessageOwn(Dialog, IDC_RADIO1 + cnt, BM_SETCHECK, 0, 0);
			}

			break;

		case IDC_RADIO17:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO17, BM_SETCHECK, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_RADIO18, BM_SETCHECK, 0, 0);

			if ((Selection = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETCURSEL, 0, 0)) != CB_ERR)
			{
				SelectedObjectCode = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETITEMDATA, Selection, 0);
				Index = LOWORD(WParam) - IDC_RADIO17;
				PCBColors2[SelectedObjectCode] = (COLORREF) 0x01000000;
				DeleteGraphicObjects();
				CreateDrawObjects(0);
				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				CheckInputMessages(0);
				CheckInputMessages(0);
				SelectColorInList = Selection;
				SendMessageOwn(Dialog, WM_INITDIALOG, 0, 0);
				SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_SETCURSEL, Selection, 0);
				SendMessageOwn(Dialog, WM_COMMAND, IDD_COLOR_LIST1, 0);
			}
			else
			{
				for (cnt = 0; cnt < 18; cnt++)
					SendDlgItemMessageOwn(Dialog, IDC_RADIO1 + cnt, BM_SETCHECK, 0, 0);
			}

			break;

		case ID_CHANGE_COLOR:
			if ((Selection = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETCURSEL, 0, 0)) != CB_ERR)
			{
				SelectColorInList = Selection;
				res2 = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETITEMDATA, Selection, 0);
				NewColor = GetNewColor(0, PCBColors[res2], Dialog);

				if (NewColor != -1)
				{
					switch (res2)
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
					case ShapePinsTopObjectNr:
					case ShapePinsBottomObjectNr:
					case ShapePinsInnerObjectNr:
						switch (res2)
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
							PCBColors[res2 - ViewLayer1ObjectNr + ViewLayer1InNetObjectNr] = NewColor;
							break;
						}

						PCBColors[res2] = NewColor;
						cb = (NewColor >> 16) & 0xff;
						cg = (NewColor >> 8) & 0xff;
						cr = NewColor & 0xff;
						cb = min(255, cb + ((255 - cb) / 2));
						cg = min(255, cg + ((255 - cg) / 2));
						cr = min(255, cr + ((255 - cr) / 2));

						/*
						                  cb+=120;
						                  cg+=120;
						                  cr+=120;
						*/
						switch (res2)
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
							res3 = res2 + 16;
							break;

						case ConnectionsObjectNr:
						case ViaPinsObjectNr:
						case ViaPinsInNetObjectNr:
							res3 = res2 + 1;
							break;

						case ShapePinsTopObjectNr:
						case ShapePinsBottomObjectNr:
						case ShapePinsInnerObjectNr:
							res3 = res2 + 3;
							break;
						}

						PCBColors[res3] = (min(cb, 255) << 16) + (min(cg, 255) << 8) + min(cr, 255);

						switch (res2)
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
							PCBColors[res3 - ViewLayer1ObjectNr + ViewLayer1InNetObjectNr] =
							    (min(cb, 255) << 16) + (min(cg, 255) << 8) + min(cr, 255);
							break;
						}

						break;

					case GridObjectNr:
						TempGridColor = NewColor ^ PCBColors[BackGroundObjectNr];
						PCBColors[res2] = TempGridColor;
						break;

					default:
						PCBColors[res2] = NewColor;
						break;
					}

					DeleteGraphicObjects();
					CreateDrawObjects(0);
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
					CheckInputMessages(0);
					CheckInputMessages(0);
					SendMessageOwn(Dialog, WM_INITDIALOG, 0, 0);
					SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_SETCURSEL, Selection, 0);
					SendMessageOwn(Dialog, WM_COMMAND, IDD_COLOR_LIST1, 0);
				}
			}

			break;

		case ID_CHANGE_COLOR2:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO17, BM_SETCHECK, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_RADIO18, BM_SETCHECK, 1, 0);

			if ((Selection = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETCURSEL, 0, 0)) != CB_ERR)
			{
				res2 = SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_GETITEMDATA, Selection, 0);
				NewColor = GetNewColor(0, PCBColors2[res2], Dialog);

				if (NewColor != -1)
				{
					switch (res2)
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
						switch (res2)
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
							PCBColors2[res2 - ViewLayer1ObjectNr + ViewLayer1InNetObjectNr] = NewColor;
							break;
						}

						PCBColors2[res2] = NewColor;
						cb = (NewColor >> 16) & 0xff;
						cg = (NewColor >> 8) & 0xff;
						cr = NewColor & 0xff;
						cb = min(255, cb + ((255 - cb) / 2));
						cg = min(255, cg + ((255 - cg) / 2));
						cr = min(255, cr + ((255 - cr) / 2));

						switch (res2)
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
							res3 = res2 + 16;
							break;

						case ConnectionsObjectNr:
						case ViaPinsObjectNr:
						case ViaPinsInNetObjectNr:
							res3 = res2 + 1;
							break;

						case ShapePinsTopObjectNr:
						case ShapePinsBottomObjectNr:
						case ShapePinsInnerObjectNr:
							res3 = res2 + 3;
							break;
						}

						PCBColors2[res3] = (min(cb, 255) << 16) + (min(cg, 255) << 8) + min(cr, 255);

						switch (res2)
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
							PCBColors2[res3 - ViewLayer1ObjectNr + ViewLayer1InNetObjectNr] =
							    (min(cb, 255) << 16) + (min(cg, 255) << 8) + min(cr, 255);
							break;
						}

						break;

					case GridObjectNr:
						TempGridColor = NewColor ^ PCBColors2[BackGroundObjectNr];
						PCBColors2[res2] = TempGridColor;
						break;

					default:
						PCBColors2[res2] = NewColor;
						break;
					}

					DeleteGraphicObjects();
					CreateDrawObjects(0);
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
					CheckInputMessages(0);
					CheckInputMessages(0);
					SendMessageOwn(Dialog, WM_INITDIALOG, 0, 0);
					SendDlgItemMessageOwn(Dialog, IDD_COLOR_LIST1, LB_SETCURSEL, Selection, 0);
					SendMessageOwn(Dialog, WM_COMMAND, IDD_COLOR_LIST1, 0);
				}
			}

			break;

		case IDHELP:
			Help("change_colors.htm", 0);
			return about;

		case IDOK:
		case IDAPPLY:
			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			return 1;

		case IDCANCEL:
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}

		break;
	}

	about = 0;
	return about;
}

//**********************************************************************************************************************
//******************************************* vrstvy *******************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsViewObjectsDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, res, res2, Layer, found;
	char TextStr[MAX_LENGTH_STRING];
	int32 DialogPixelsX, DialogPixelsY;
	RECT Rect;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(882, "Other layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(883, "Trace layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(610, "Components"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(218, "Areafills"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(964, "Drills"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(126, "Solder mask top"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(128, "Paste mask top"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(127, "Solder mask bottom"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(129, "Paste mask bottom"));

		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(311, "Top"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(312, "Bottom"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(884, "Pads top"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(885, "Pads bottom"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(886, "Inner pads"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(46, "Component references"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(47, "Component values"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(1272, "Placement outline"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(887, "Component outline"));

		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, DrawTopComponents, 0);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, DrawBottomComponents, 1);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawTopPads, 2);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawBottomPads, 3);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawInnerPads, 4);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompReference, 5);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompValue, 6);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompPlacement, 7);
		SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompOutline, 8);

		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) SC(43, "Board outline"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) SC(122, "Silkscreen top"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) SC(29, "Silkscreen bottom"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) SC(44, "Keepout top"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) SC(45, "Keepout bottom"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) SC(60, "Keepout inner"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) "Info 1");
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) "Info 2");
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) "Info 3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_ADDSTRING, 0, (LPARAM) "Info 4");

		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawBoardOutline, 0);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenTop, 1);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenBottom, 2);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutTop, 3);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutBottom, 4);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutInner, 5);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfoObjects, 6);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo2Objects, 7);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo3Objects, 8);
		SendDlgItemMessage(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo4Objects, 9);

		SetDialogItemTextUTF8(Dialog, IDC_CHECK4, SC(41, "Vias"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK7, SC(74, "Clearances"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK15, SC(756, "Connections"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK16, SC(888, "Errors"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK17, SC(118, "Warnings"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK25, SC(618, "Only one layer active"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK26, SC(77, "Ruler"));

		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(892, "Solder/paste mask pads"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON4, SC(161, "Deselect all"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON5, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON6, SC(893, "Default"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON7, SC(161, "Deselect all"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON8, SC(160, "Select all"));
		
		SetWindowTextUTF8(Dialog, SC(894, "Viewable objects"));

		SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_SETCHECK, OkToDrawVias, 0);
		SendDlgItemMessageOwn(Dialog, IDC_CHECK7, BM_SETCHECK, OkToDrawClearances, 0);
		SendDlgItemMessageOwn(Dialog, IDC_CHECK15, BM_SETCHECK, OkToDrawConnections, 0);
		SendDlgItemMessageOwn(Dialog, IDC_CHECK16, BM_SETCHECK, OkToDrawErrors, 0);
		SendDlgItemMessageOwn(Dialog, IDC_CHECK17, BM_SETCHECK, OkToDrawWarnings, 0);
		SendDlgItemMessageOwn(Dialog, IDC_CHECK26, BM_SETCHECK, OkToDrawCrossHair, 0);

		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(898, "None"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(899, "Only extra objects"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(207, "All"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM) SC(898, "None"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM) SC(899, "Only extra objects"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM) SC(207, "All"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO3, CB_ADDSTRING, 0, (LPARAM) SC(898, "None"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO3, CB_ADDSTRING, 0, (LPARAM) SC(899, "Only extra objects"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO3, CB_ADDSTRING, 0, (LPARAM) SC(207, "All"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO4, CB_ADDSTRING, 0, (LPARAM) SC(898, "None"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO4, CB_ADDSTRING, 0, (LPARAM) SC(899, "Only extra objects"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO4, CB_ADDSTRING, 0, (LPARAM) SC(207, "All"));
		
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, DrawSoldMaskTopMode, 0);
		SendDlgItemMessage(Dialog, IDC_COMBO2, CB_SETCURSEL, DrawPasteMaskTopMode, 0);
		SendDlgItemMessage(Dialog, IDC_COMBO3, CB_SETCURSEL, DrawSoldMaskBottomMode, 0);
		SendDlgItemMessage(Dialog, IDC_COMBO4, CB_SETCURSEL, DrawPasteMaskBottomMode, 0);

		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);

		for (Layer = Design.NrBoardLayers - 1; Layer >= 0; Layer--)
		{
			GetLayerText(Layer, TextStr, 0);
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) TextStr);

			if (DrawLayerCode[Layer] < 0x10)
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, res);
		}

		if (ViewSingleLayer)
			SendDlgItemMessage(Dialog, IDC_CHECK25, BM_SETCHECK, ViewSingleLayer, 0);

		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(1193, "Off"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(895, "Solid"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(896, "Surround"));

		if (OkToDrawAreaFills)
		{
			switch (AreafillDrawMode)
			{
			case 1:
				SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 2, 0);
				break;

			default:
				AreafillDrawMode = 0;
				SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
				break;
			}
		}
		else
			SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 0, 0);

		SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) SC(1193, "Off"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) SC(895, "Solid"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) SC(1194, "Crosshair x"));
		SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) SC(1195, "Crosshair +"));

		switch (DrawDrillMode)
		{
		case 0:				// Off
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 0, 0);
			break;

		case 1:				// Cross hair 45 degrees
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 2, 0);
			break;

		case 2:				// Cross hair
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 3, 0);
			break;

		case 3:				// Filled with black hole
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 1, 0);
			break;
		}

		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		ok = 1;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_LIST2:		// Areafills
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);

				switch (res)
				{
				case 0:
					AreafillDrawMode = 0;
					OkToDrawAreaFills = 0;
					break;

				case 1:
					AreafillDrawMode = 0;
					OkToDrawAreaFills = 1;
					break;

				case 2:
					AreafillDrawMode = 1;
					OkToDrawAreaFills = 1;
					break;
				}

				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			}

			break;

		case IDC_LIST3:		// Drill
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETCURSEL, 0, 0);

				switch (res)
				{
				case 0:
					DrawDrillMode = 0;
					break;

				case 1:
					DrawDrillMode = 3;
					break;

				case 2:
					DrawDrillMode = 1;
					break;

				case 3:
					DrawDrillMode = 2;
					break;
				}

				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			}

			break;

		case IDC_LIST4:		// Component layers
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				for (cnt = 0; cnt < 9; cnt++)
				{
					res = SendDlgItemMessage(Dialog, IDC_LIST4, LB_GETSEL, cnt, 0);

					switch (cnt)
					{
					case 0:
						if (res)
							DrawTopComponents = 1;
						else
							DrawTopComponents = 0;

						break;

					case 1:
						if (res)
							DrawBottomComponents = 1;
						else
							DrawBottomComponents = 0;

						break;

					case 2:
						if (res)
							OkToDrawTopPads = 1;
						else
							OkToDrawTopPads = 0;

						break;

					case 3:
						if (res)
							OkToDrawBottomPads = 1;
						else
							OkToDrawBottomPads = 0;

						break;

					case 4:
						if (res)
							OkToDrawInnerPads = 1;
						else
							OkToDrawInnerPads = 0;

						break;

					case 5:
						if (res)
							OkToDrawCompReference = 1;
						else
							OkToDrawCompReference = 0;

						break;

					case 6:
						if (res)
							OkToDrawCompValue = 1;
						else
							OkToDrawCompValue = 0;

						break;

					case 7:
						if (res)
							OkToDrawCompPlacement = 1;
						else
							OkToDrawCompPlacement = 0;

						break;

					case 8:
						if (res)
							OkToDrawCompOutline = 1;
						else
							OkToDrawCompOutline = 0;

						break;
					}
				}

				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			}

			break;

		case IDC_LIST5:		// Other layers
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				for (cnt = 0; cnt < 10; cnt++)
				{
					res = SendDlgItemMessage(Dialog, IDC_LIST5, LB_GETSEL, cnt, 0);

					switch (cnt)
					{
					case 0:
						if (res)
							OkToDrawBoardOutline = 1;
						else
							OkToDrawBoardOutline = 0;

						break;

					case 1:
						if (res)
							OkToDrawSilkScreenTop = 1;
						else
							OkToDrawSilkScreenTop = 0;

						break;

					case 2:
						if (res)
							OkToDrawSilkScreenBottom = 1;
						else
							OkToDrawSilkScreenBottom = 0;

						break;

					case 3:
						if (res)
							OkToDrawRoutingKeepoutTop = 1;
						else
							OkToDrawRoutingKeepoutTop = 0;

						break;

					case 4:
						if (res)
							OkToDrawRoutingKeepoutBottom = 1;
						else
							OkToDrawRoutingKeepoutBottom = 0;

						break;

					case 5:
						if (res)
							OkToDrawRoutingKeepoutInner = 1;
						else
							OkToDrawRoutingKeepoutInner = 0;

						break;

					case 6:
						if (res)
							OkToDrawInfoObjects = 1;
						else
							OkToDrawInfoObjects = 0;

						break;

					case 7:
						if (res)
							OkToDrawInfo2Objects = 1;
						else
							OkToDrawInfo2Objects = 0;

						break;

					case 8:
						if (res)
							OkToDrawInfo3Objects = 1;
						else
							OkToDrawInfo3Objects = 0;

						break;

					case 9:
						if (res)
							OkToDrawInfo4Objects = 1;
						else
							OkToDrawInfo4Objects = 0;

						break;
					}
				}

				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			}

			break;

		case IDC_COMBO1:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

				if (res != CB_ERR)
				{
					DrawSoldMaskTopMode = res;
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				}
			}

			break;

		case IDC_COMBO2:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessage(Dialog, IDC_COMBO2, CB_GETCURSEL, 0, 0);

				if (res != CB_ERR)
				{
					DrawPasteMaskTopMode = res;
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				}
			}

			break;

		case IDC_COMBO3:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessage(Dialog, IDC_COMBO3, CB_GETCURSEL, 0, 0);

				if (res != CB_ERR)
				{
					DrawSoldMaskBottomMode = res;
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				}
			}

			break;

		case IDC_COMBO4:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessage(Dialog, IDC_COMBO4, CB_GETCURSEL, 0, 0);

				if (res != CB_ERR)
				{
					DrawPasteMaskBottomMode = res;
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				}
			}

			break;

		case IDC_CHECK25:		// Only one layer active
			if (SendDlgItemMessage(Dialog, IDC_CHECK25, BM_GETCHECK, 0, 0))
			{
				for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
					DrawLayerCode[cnt] |= 0x10;

				SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 0, -1);
				SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, 0);
				DrawLayerCode[Design.NrBoardLayers - 1] &= ~0x10;

				if ((CurrentDrawingLayer != -1) && (DrawLayerCode[CurrentDrawingLayer] >= 0x10))
					CurrentDrawingLayer = GetDrawingLayer(4 + 2);

				if (DrawLayerCode[0] & 0x10)
				{
					OkToDrawBottomPads = 0;
					OkToDrawSilkScreenBottom = 0;
					DrawBottomComponents = 0;
					OkToDrawRoutingKeepoutBottom = 0;
				}
				else
				{
					OkToDrawBottomPads = 1;
					OkToDrawSilkScreenBottom = 1;
					DrawBottomComponents = 1;
					OkToDrawRoutingKeepoutBottom = 0;
				}

				if (DrawLayerCode[Design.NrBoardLayers - 1] & 0x10)
				{
					OkToDrawTopPads = 0;
					OkToDrawSilkScreenTop = 0;
					DrawTopComponents = 0;
					OkToDrawRoutingKeepoutTop = 0;
				}
				else
				{
					OkToDrawTopPads = 1;
					OkToDrawSilkScreenTop = 1;
					DrawTopComponents = 1;
					OkToDrawRoutingKeepoutTop = 1;
				}

				if (Design.NrBoardLayers > 2)
				{
					OkToDrawInnerPads = 0;
					OkToDrawRoutingKeepoutInner = 0;
				}

				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawBottomComponents, 1);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenBottom, 2);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawTopComponents, 0);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawTopPads, 2);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawBottomPads, 3);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenTop, 1);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawInnerPads, 4);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutTop, 3);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutBottom, 4);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutInner, 5);
				ViewSingleLayer = 1;
				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			}
			else
			{
				for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
					DrawLayerCode[cnt] &= ~0x10;

				SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, -1);
				OkToDrawBottomPads = 1;
				OkToDrawSilkScreenBottom = 1;
				DrawBottomComponents = 1;
				OkToDrawTopPads = 1;
				OkToDrawSilkScreenTop = 1;
				DrawTopComponents = 1;
				OkToDrawInnerPads = 1;
				OkToDrawRoutingKeepoutTop = 1;
				OkToDrawRoutingKeepoutBottom = 1;
				OkToDrawRoutingKeepoutInner = 1;
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawBottomComponents, 1);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenBottom, 2);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawTopComponents, 0);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawTopPads, 2);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawBottomPads, 3);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenTop, 1);
				SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawInnerPads, 4);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutTop, 3);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutBottom, 4);
				SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutInner, 5);
				ViewSingleLayer = 0;
				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			}

			ok = 1;
			break;

		case IDOK:
		case IDAPPLY:
			OkToDrawCrossHair = SendDlgItemMessageOwn(Dialog, IDC_CHECK26, BM_GETCHECK, 0, 0);

			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			return 1;

		case IDC_CHECK4:		//
		case IDC_CHECK7:		//
		case IDC_CHECK15:		//
		case IDC_CHECK16:		//
		case IDC_CHECK17:		//
		case IDC_CHECK26:		//
			OkToDrawVias = SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_GETCHECK, 0, 0);
			OkToDrawClearances = SendDlgItemMessageOwn(Dialog, IDC_CHECK7, BM_GETCHECK, 0, 0);
			OkToDrawConnections = SendDlgItemMessageOwn(Dialog, IDC_CHECK15, BM_GETCHECK, 0, 0);
			OkToDrawErrors = SendDlgItemMessageOwn(Dialog, IDC_CHECK16, BM_GETCHECK, 0, 0);
			OkToDrawWarnings = SendDlgItemMessageOwn(Dialog, IDC_CHECK17, BM_GETCHECK, 0, 0);
			OkToDrawCrossHair = SendDlgItemMessageOwn(Dialog, IDC_CHECK26, BM_GETCHECK, 0, 0);
			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			break;

		case IDC_LIST1:		// Trace layers
			res = HIWORD(WParam);	// LParam

			if (SendDlgItemMessage(Dialog, IDC_CHECK25, BM_GETCHECK, 0, 0) == 1)
			{	// Only one layer active
				if (res == LBN_SELCHANGE)
				{
					found = -1;

					for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
					{
						Layer = Design.NrBoardLayers - cnt - 1;

						if ((DrawLayerCode[Layer] & 0x10) == 0)
						{
							found = cnt;
							break;
						}
					}

					found++;

					if (found == Design.NrBoardLayers)
						found = 0;

					for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
					{
						Layer = Design.NrBoardLayers - cnt - 1;

						if (found == cnt)
						{
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, cnt);
							DrawLayerCode[Layer] &= ~0x10;
						}
						else
						{
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 0, cnt);
							DrawLayerCode[Layer] |= 0x10;
						}
					}

					if ((CurrentDrawingLayer != -1) && (DrawLayerCode[CurrentDrawingLayer] >= 0x10))
						CurrentDrawingLayer = GetDrawingLayer(4 + 2);

					if (DrawLayerCode[0] & 0x10)
					{
						OkToDrawBottomPads = 0;
						OkToDrawSilkScreenBottom = 0;
						DrawBottomComponents = 0;
						OkToDrawRoutingKeepoutBottom = 0;
					}
					else
					{
						OkToDrawBottomPads = 1;
						OkToDrawSilkScreenBottom = 1;
						DrawBottomComponents = 1;
						OkToDrawRoutingKeepoutBottom = 1;
					}

					if (DrawLayerCode[Design.NrBoardLayers - 1] & 0x10)
					{
						OkToDrawTopPads = 0;
						OkToDrawSilkScreenTop = 0;
						DrawTopComponents = 0;
						OkToDrawRoutingKeepoutTop = 0;
					}
					else
					{
						OkToDrawTopPads = 1;
						OkToDrawSilkScreenTop = 1;
						DrawTopComponents = 1;
						OkToDrawRoutingKeepoutTop = 1;
					}

					if (Design.NrBoardLayers > 2)
					{
						if ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) && (DrawLayerCode[0] & 0x10))
						{
							OkToDrawInnerPads = 1;
							OkToDrawRoutingKeepoutInner = 1;
//                  OkToDrawSilkScreenBottom=1;
							DrawBottomComponents = 1;
							DrawTopComponents = 1;
//                  OkToDrawSilkScreenTop=1;
						}
						else
						{
							OkToDrawInnerPads = 0;
							OkToDrawRoutingKeepoutInner = 0;
						}
					}

					SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawBottomComponents, 1);
					SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenBottom, 2);
					SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawTopComponents, 0);
					SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawTopPads, 2);
					SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawBottomPads, 3);
					SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenTop, 1);
					SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawInnerPads, 4);
					SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutTop, 3);
					SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutBottom, 4);
					SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutInner, 5);
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				}
			}
			else
			{
				if (res == LBN_SELCHANGE)
				{
					for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
					{
						Layer = Design.NrBoardLayers - cnt - 1;

						if (SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0))
						{
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, cnt);
							DrawLayerCode[Layer] &= ~0x10;
						}
						else
						{
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 0, cnt);
							DrawLayerCode[Layer] |= 0x10;
						}
					}

					if (CurrentDrawingLayer != -1)
					{
						if (DrawLayerCode[CurrentDrawingLayer] >= 0x10)
							CurrentDrawingLayer = GetDrawingLayer(4 + 2);
					}
					else
						CurrentDrawingLayer = GetDrawingLayer(4 + 2);

					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				}
			}

			break;

		case IDC_BUTTON4:		// Unselect all
			OkToDrawVias = 0;
			OkToDrawClearances = 0;
			OkToDrawConnections = 0;
			OkToDrawErrors = 0;
			OkToDrawWarnings = 0;
			OkToDrawCrossHair = 0;
			DrawSoldMaskBottomMode = 0;
			DrawSoldMaskTopMode = 0;
			DrawPasteMaskBottomMode = 0;
			DrawPasteMaskTopMode = 0;
			DrawDrillMode = 0;	// Filled with black hole
			DrawTopComponents = 0;
			DrawBottomComponents = 0;
			OkToDrawTopPads = 0;
			OkToDrawBottomPads = 0;
			OkToDrawInnerPads = 0;
			OkToDrawCompReference = 0;
			OkToDrawCompValue = 0;
			OkToDrawCompPlacement = 0;
			OkToDrawCompOutline = 0;
			OkToDrawBoardOutline = 0;
			OkToDrawSilkScreenTop = 0;
			OkToDrawSilkScreenBottom = 0;
			OkToDrawRoutingKeepoutTop = 0;
			OkToDrawRoutingKeepoutBottom = 0;
			OkToDrawRoutingKeepoutInner = 0;
			OkToDrawInfoObjects = 0;
			OkToDrawInfo2Objects = 0;
			OkToDrawInfo3Objects = 0;
			OkToDrawInfo4Objects = 0;
			OkToDrawAreaFills = 0;
			DrawSoldMaskTopMode = 0;
			DrawPasteMaskTopMode = 0;
			DrawSoldMaskBottomMode = 0;
			DrawPasteMaskBottomMode = 0;
			SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_SETCHECK, OkToDrawVias, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK7, BM_SETCHECK, OkToDrawClearances, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK15, BM_SETCHECK, OkToDrawConnections, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK16, BM_SETCHECK, OkToDrawErrors, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK17, BM_SETCHECK, OkToDrawWarnings, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK26, BM_SETCHECK, OkToDrawCrossHair, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawTopComponents, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawBottomComponents, 1);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawTopPads, 2);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawBottomPads, 3);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawInnerPads, 4);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompReference, 5);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompValue, 6);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompPlacement, 7);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompOutline, 8);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawBoardOutline, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenTop, 1);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenBottom, 2);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutTop, 3);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutBottom, 4);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutInner, 5);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfoObjects, 6);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo2Objects, 7);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo3Objects, 8);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo4Objects, 9);
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, DrawSoldMaskTopMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO2, CB_SETCURSEL, DrawPasteMaskTopMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO3, CB_SETCURSEL, DrawSoldMaskBottomMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO4, CB_SETCURSEL, DrawPasteMaskBottomMode, 0);

			switch (DrawDrillMode)
			{
			case 0:			// Off
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 0, 0);
				break;

			case 1:			// Cross hair 45 degrees
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 2, 0);
				break;

			case 2:			// Cross hair
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 3, 0);
				break;

			case 3:			// Filled with black hole
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 1, 0);
				break;
			}

			if (OkToDrawAreaFills)
			{
				switch (AreafillDrawMode)
				{
				case 1:
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 2, 0);
					break;

				default:
					AreafillDrawMode = 0;
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
					break;
				}
			}
			else
				SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 0, 0);

			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			break;

		case IDC_BUTTON5:		// Select all
			OkToDrawVias = 1;
			OkToDrawClearances = 1;
			OkToDrawConnections = 1;
			OkToDrawErrors = 0;
			OkToDrawWarnings = 0;
			OkToDrawCrossHair = 0;
			DrawSoldMaskBottomMode = 2;
			DrawSoldMaskTopMode = 2;
			DrawPasteMaskBottomMode = 2;
			DrawPasteMaskTopMode = 2;
			DrawDrillMode = 3;	// Filled with black hole
			DrawTopComponents = 1;
			DrawBottomComponents = 1;
			OkToDrawTopPads = 1;
			OkToDrawBottomPads = 1;
			OkToDrawInnerPads = 1;
			OkToDrawCompReference = 1;
			OkToDrawCompValue = 1;
			OkToDrawCompPlacement = 1;
			OkToDrawCompOutline = 1;
			OkToDrawBoardOutline = 1;
			OkToDrawSilkScreenTop = 1;
			OkToDrawSilkScreenBottom = 1;
			OkToDrawRoutingKeepoutTop = 1;
			OkToDrawRoutingKeepoutBottom = 1;
			OkToDrawRoutingKeepoutInner = 1;
			OkToDrawInfoObjects = 1;
			OkToDrawInfo2Objects = 1;
			OkToDrawInfo3Objects = 1;
			OkToDrawInfo4Objects = 1;
			OkToDrawAreaFills = 1;
			AreafillDrawMode = 0;
			DrawSoldMaskTopMode = 2;
			DrawPasteMaskTopMode = 2;
			DrawSoldMaskBottomMode = 2;
			DrawPasteMaskBottomMode = 2;
			SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_SETCHECK, OkToDrawVias, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK7, BM_SETCHECK, OkToDrawClearances, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK15, BM_SETCHECK, OkToDrawConnections, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK16, BM_SETCHECK, OkToDrawErrors, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK17, BM_SETCHECK, OkToDrawWarnings, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK26, BM_SETCHECK, OkToDrawCrossHair, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawTopComponents, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawBottomComponents, 1);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawTopPads, 2);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawBottomPads, 3);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawInnerPads, 4);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompReference, 5);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompValue, 6);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompPlacement, 7);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompOutline, 8);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawBoardOutline, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenTop, 1);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenBottom, 2);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutTop, 3);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutBottom, 4);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutInner, 5);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfoObjects, 6);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo2Objects, 7);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo3Objects, 8);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo4Objects, 9);
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, DrawSoldMaskTopMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO2, CB_SETCURSEL, DrawPasteMaskTopMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO3, CB_SETCURSEL, DrawSoldMaskBottomMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO4, CB_SETCURSEL, DrawPasteMaskBottomMode, 0);

			switch (DrawDrillMode)
			{
			case 0:			// Off
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 0, 0);
				break;

			case 1:			// Cross hair 45 degrees
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 2, 0);
				break;

			case 2:			// Cross hair
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 3, 0);
				break;

			case 3:			// Filled with black hole
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 1, 0);
				break;
			}

			if (OkToDrawAreaFills)
			{
				switch (AreafillDrawMode)
				{
				case 1:
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 2, 0);
					break;

				default:
					AreafillDrawMode = 0;
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
					break;
				}
			}
			else
				SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 0, 0);

			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			break;

		case IDC_BUTTON6:		// Default
			OkToDrawVias = 1;
			OkToDrawClearances = 0;
			OkToDrawConnections = 1;
			OkToDrawErrors = 0;
			OkToDrawWarnings = 0;
			OkToDrawCrossHair = 0;
			DrawSoldMaskBottomMode = 1;
			DrawSoldMaskTopMode = 1;
			DrawPasteMaskBottomMode = 1;
			DrawPasteMaskTopMode = 1;
			DrawDrillMode = 3;	// Filled with black hole
			DrawTopComponents = 1;
			DrawBottomComponents = 1;
			OkToDrawTopPads = 1;
			OkToDrawBottomPads = 1;
			OkToDrawInnerPads = 1;
			OkToDrawCompReference = 1;
			OkToDrawCompValue = 0;
			OkToDrawCompPlacement = 1;
			OkToDrawCompOutline = 1;
			OkToDrawBoardOutline = 1;
			OkToDrawSilkScreenTop = 1;
			OkToDrawSilkScreenBottom = 1;
			OkToDrawRoutingKeepoutTop = 0;
			OkToDrawRoutingKeepoutBottom = 0;
			OkToDrawRoutingKeepoutInner = 0;
			OkToDrawInfoObjects = 1;
			OkToDrawInfo2Objects = 1;
			OkToDrawInfo3Objects = 1;
			OkToDrawInfo4Objects = 1;
			OkToDrawAreaFills = 1;
			AreafillDrawMode = 0;
			DrawSoldMaskTopMode = 1;
			DrawPasteMaskTopMode = 1;
			DrawSoldMaskBottomMode = 1;
			DrawPasteMaskBottomMode = 1;

			SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_SETCHECK, OkToDrawVias, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK7, BM_SETCHECK, OkToDrawClearances, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK15, BM_SETCHECK, OkToDrawConnections, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK16, BM_SETCHECK, OkToDrawErrors, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK17, BM_SETCHECK, OkToDrawWarnings, 0);
			SendDlgItemMessageOwn(Dialog, IDC_CHECK26, BM_SETCHECK, OkToDrawCrossHair, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawTopComponents, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, DrawBottomComponents, 1);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawTopPads, 2);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawBottomPads, 3);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawInnerPads, 4);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompReference, 5);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompValue, 6);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompPlacement, 7);
			SendDlgItemMessageOwn(Dialog, IDC_LIST4, LB_SETSEL, OkToDrawCompOutline, 8);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawBoardOutline, 0);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenTop, 1);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawSilkScreenBottom, 2);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutTop, 3);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutBottom, 4);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawRoutingKeepoutInner, 5);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfoObjects, 6);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo2Objects, 7);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo3Objects, 8);
			SendDlgItemMessageOwn(Dialog, IDC_LIST5, LB_SETSEL, OkToDrawInfo4Objects, 9);
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, DrawSoldMaskTopMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO2, CB_SETCURSEL, DrawPasteMaskTopMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO3, CB_SETCURSEL, DrawSoldMaskBottomMode, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO4, CB_SETCURSEL, DrawPasteMaskBottomMode, 0);

			switch (DrawDrillMode)
			{
			case 0:			// Off
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 0, 0);
				break;

			case 1:			// Cross hair 45 degrees
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 2, 0);
				break;

			case 2:			// Cross hair
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 3, 0);
				break;

			case 3:			// Filled with black hole
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 1, 0);
				break;
			}

			if (OkToDrawAreaFills)
			{
				switch (AreafillDrawMode)
				{
				case 1:
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 2, 0);
					break;

				default:
					AreafillDrawMode = 0;
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
					break;
				}
			}
			else
				SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 0, 0);

			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			break;

		case IDC_BUTTON7:		// Deselect all
			for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 0, cnt);
				DrawLayerCode[cnt] |= 0x10;
			}

			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			break;

		case IDC_BUTTON8:		// Select all
			for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, cnt);
				DrawLayerCode[cnt] &= ~0x10;
			}

			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			break;

		case IDCANCEL:
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}

		break;
	}

	about = 0;
	return about;
}

//**********************************************************************************************************************
//****************************************** možnosti ******************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsOptionsDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, res, res2, DialogPixelsX, DialogPixelsY;
	double NewGridSize, TempGridSize2, TempGridSize3, TempGridSize4, value1;
	char str[MAX_LENGTH_STRING];
	RECT Rect;

	NewGridSize = (10 * 2540.0);
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(927, "Default grid"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(928, "Grid when moving components"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(929, "Grid when drawing traces"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(1198, "Grid when drawing areafills"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(70, "Grid"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(793, "Selection modes"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(1201, "Snap modes"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(1203, "Several settings"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(1205, "Mouse cursor Info display"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(1206, "Sec"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(1206, "Sec"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(1207, "Activate after"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(1208, "Visible for"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC14, SC(267, "Pan sensivity"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC30, SC(1211, "Repeat"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC31, SC(273, "Zoom mode"));

		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(761, "Recalculate ratsnets after move"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(762, "Intelligent ratsnets dragging"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK4, SC(746, "Snap on for special objects"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(1110, "Snap on first comp location"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK6, SC(1177, "Repaint after every trace draw"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK7, SC(1178, "Repaint after every component move"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK8, SC(1179, "Do not show areafill thin lines errors"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK9, SC(745, "Recalc areafill after inserting an object"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK10, SC(790, "View relative position on grid"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK11, SC(1209, "Start with maximum view"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK12, SC(1210, "Activated"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK13, SC(1304, "Move/rotate component with auto zoom"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK27, SC(511, "On"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(794, "Replacement"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(795, "Appending"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(1331, "Use mouse scroll wheel for zoom in/out"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(1332, "Press the Alt key and the mouse scroll wheel for zoom in/out"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(1149, "Set"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(1149, "Set"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(1149, "Set"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON4, SC(1149, "Set"));
		SetWindowTextUTF8(Dialog, SC(349, "Options"));
		TempUnits = Units;
		TempGridSize = GridSize;
		SetDialogValue(Dialog, IDC_EDIT1, UserGridSize);
		SetDialogValue(Dialog, IDC_EDIT3, CompGridSize);
		SetDialogValue(Dialog, IDC_EDIT5, TraceGridSize);
		SetDialogValue(Dialog, IDC_EDIT7, AreafillGridSize);
		SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);

		for (cnt = 0; cnt < NrGridSizes; cnt++)
		{
			value1 = GridSizes[cnt];

			if (TempUnits == 0)
				sprintf(str, "%.4f", value1 / 2540);
			else
				sprintf(str, "%.7f", value1 / 100000);

			StripAppendingZeros(str, 0);
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);

			if (InRange(GridSize, value1))
				res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, res, 0);
		}

		GridChanged = 0;

		SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, ComponentConnectionMode, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK7, BM_SETCHECK, OkToRePaintAfterCompMove, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK9, BM_SETCHECK, RecalcAreafillAfterInsert, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK8, BM_SETCHECK, DoNotShowAreafillThinLinesError, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK6, BM_SETCHECK, OkToRePaintAfterTraceDrawing, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK10, BM_SETCHECK, MouseCursorOnGrid, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK11, BM_SETCHECK, StartWithMaximumView, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK12, BM_SETCHECK, PopupDisplayVisible, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK13, BM_SETCHECK, MoveCompAutoZoom, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK27, BM_SETCHECK, RepeatModeActive, 0);

		SendDlgItemMessage(Dialog, IDC_CHECK5, BM_SETCHECK, CompSnapMode, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK4, BM_SETCHECK, SnapMode, 0);
		SendDlgItemMessage(Dialog, IDC_RADIO1, BM_SETCHECK, ReplaceSelections, 0);
		SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, !ReplaceSelections, 0);
		SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, !ZoomMode, 0);
		SendDlgItemMessage(Dialog, IDC_RADIO4, BM_SETCHECK, ZoomMode, 0);
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "1");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "1.5");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "2");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "3");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "4");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "5");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "6");

		switch (MousePanMultiply)
		{
		case 0:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) "1");
			break;

		case 1:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) "1.5");
			break;

		case 2:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) "2");
			break;

		case 3:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) "3");
			break;

		case 4:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) "4");
			break;

		case 5:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) "5");
			break;

		case 6:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) "6");
			break;
		}

		SetDialogIntValue(Dialog, IDC_EDIT9, ButtonInfoTimeoutStart / 10);
		SetDialogIntValue(Dialog, IDC_EDIT10, ButtonInfoTimeout / 10);
		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_LIST1:
			res = HIWORD(WParam);
			res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if ((res == 1) && (res2 != LB_ERR))
			{
				TempGridSize = GridSizes[res2];
//            SetDialogValue2(Dialog,IDC_EDIT1,TempGridSize);
			}

			break;

		case IDOK:
		case IDAPPLY:
			TempGridSize = GetDialogValue(Dialog, IDC_EDIT1);

			if ((TempGridSize > 0.0) && (TempGridSize < 10))
			{
				MessageBoxOwn(PCBWindow, SC(930, "Size of grid >=100nm"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			UserGridSize = TempGridSize;
			TempGridSize = GetDialogValue(Dialog, IDC_EDIT3);

			if ((TempGridSize > 0.0) && (TempGridSize < 10))
			{
				MessageBoxOwn(PCBWindow, SC(931, "Size of component grid >=100nm"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			CompGridSize = TempGridSize;
			TempGridSize = GetDialogValue(Dialog, IDC_EDIT5);

			if ((TempGridSize > 0.0) && (TempGridSize < 10))
			{
				MessageBoxOwn(PCBWindow, SC(932, "Size of trace grid >=100nm"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			TraceGridSize = TempGridSize;
			TempGridSize = GetDialogValue(Dialog, IDC_EDIT7);

			if ((TempGridSize > 0.0) && (TempGridSize < 10))
			{
				MessageBoxOwn(PCBWindow, SC(1199, "Size of areafill grid >=100nm"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

			if ((res >= 0) && (res < 7))
				MousePanMultiply = res;

			AreafillGridSize = TempGridSize;

			if (GridChanged)
			{
				switch (SelectionMode)
				{
				case MOVE_COMPONENTS_MODE:	// Components
				case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
				case MOVE_COMPONENT_VALUES_MODE:	// Component values
					GridSize = CompGridSize;
					break;

				case MOVE_ONE_TRACE_MODE:
				case MOVING_TRACES_VIAS_MODE:	// traces/vias
				case ROUTING_MODE:
				case DRAG_TRACES_VIAS_COMPS_MODE:
					GridSize = TraceGridSize;
					break;

				case OBJECTS_MODE:	// Other objects
				case GATE_PINSWAP_MODE:
					GridSize = UserGridSize;
					break;

				case AREAFILLS_MODE:	// areafills
					GridSize = AreafillGridSize;
					break;
				}

				if (GridVisible)
				{
					RePaint();
					SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
				}
			}

			CompSnapMode = SendDlgItemMessage(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0);
			SnapMode = SendDlgItemMessage(Dialog, IDC_CHECK4, BM_GETCHECK, 0, 0);
			ComponentConnectionMode = SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0);
			DoNotShowAreafillThinLinesError = SendDlgItemMessage(Dialog, IDC_CHECK8, BM_GETCHECK, 0, 0);
			OkToRePaintAfterCompMove = SendDlgItemMessage(Dialog, IDC_CHECK7, BM_GETCHECK, 0, 0);
			OkToRePaintAfterTraceDrawing = SendDlgItemMessage(Dialog, IDC_CHECK6, BM_GETCHECK, 0, 0);
			RecalcAreafillAfterInsert = SendDlgItemMessage(Dialog, IDC_CHECK9, BM_GETCHECK, 0, 0);
			MouseCursorOnGrid = SendDlgItemMessage(Dialog, IDC_CHECK10, BM_GETCHECK, 0, 0);
			ReplaceSelections = SendDlgItemMessage(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0);
			ZoomMode = SendDlgItemMessage(Dialog, IDC_RADIO4, BM_GETCHECK, 0, 0);
			StartWithMaximumView = SendDlgItemMessage(Dialog, IDC_CHECK11, BM_GETCHECK, 0, 0);
			PopupDisplayVisible = SendDlgItemMessage(Dialog, IDC_CHECK12, BM_GETCHECK, 0, 0);
			MoveCompAutoZoom = SendDlgItemMessage(Dialog, IDC_CHECK13, BM_GETCHECK, 0, 0);
			RepeatModeActive = SendDlgItemMessage(Dialog, IDC_CHECK27, BM_GETCHECK, 0, 0);
			GetDialogIntValue(Dialog, IDC_EDIT9, &ButtonInfoTimeoutStart);
			ButtonInfoTimeoutStart *= 10;
			GetDialogIntValue(Dialog, IDC_EDIT10, &ButtonInfoTimeout);
			ButtonInfoTimeout *= 10;

			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			return 1;

		case IDD_UNITS:
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
			TempGridSize = GetDialogValue(Dialog, IDC_EDIT1);
			TempGridSize2 = GetDialogValue(Dialog, IDC_EDIT3);
			TempGridSize3 = GetDialogValue(Dialog, IDC_EDIT5);
			TempGridSize4 = GetDialogValue(Dialog, IDC_EDIT7);
			TempUnits ^= 1;
			SetDialogValue2(Dialog, IDC_EDIT1, TempGridSize);
			SetDialogValue2(Dialog, IDC_EDIT3, TempGridSize2);
			SetDialogValue2(Dialog, IDC_EDIT5, TempGridSize3);
			SetDialogValue2(Dialog, IDC_EDIT7, TempGridSize4);

			for (cnt = 0; cnt < NrGridSizes; cnt++)
			{
				value1 = GridSizes[cnt];

				if (TempUnits == 0)
					sprintf(str, "%.4f", value1 / 2540);
				else
					sprintf(str, "%.7f", value1 / 100000);

				StripAppendingZeros(str, 0);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);

				if (InRange(TempGridSize, value1))
					res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, res, 0);
			}

			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
			break;

		case IDC_BUTTON1:
			res = HIWORD(WParam);
			res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (res == 0)
			{
				if (res2 != LB_ERR)
					TempGridSize = GridSizes[res2];
				else
					TempGridSize = GetDialogValue(Dialog, IDC_EDIT1);

				SetDialogValue2(Dialog, IDC_EDIT1, TempGridSize);
				GridChanged = 1;
			}

			break;

		case IDC_BUTTON2:
			res = HIWORD(WParam);
			res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (res == 0)
			{
				if (res2 != LB_ERR)
					TempGridSize = GridSizes[res2];
				else
					TempGridSize = GetDialogValue(Dialog, IDC_EDIT3);

				SetDialogValue2(Dialog, IDC_EDIT3, TempGridSize);
				GridChanged = 1;
			}

			break;

		case IDC_BUTTON3:
			res = HIWORD(WParam);
			res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (res == 0)
			{
				if (res2 != LB_ERR)
					TempGridSize = GridSizes[res2];
				else
					TempGridSize = GetDialogValue(Dialog, IDC_EDIT5);

				SetDialogValue2(Dialog, IDC_EDIT5, TempGridSize);
				GridChanged = 1;
			}

			break;

		case IDC_BUTTON4:
			res = HIWORD(WParam);
			res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (res == 0)
			{
				if (res2 != LB_ERR)
					TempGridSize = GridSizes[res2];
				else
					TempGridSize = GetDialogValue(Dialog, IDC_EDIT7);

				SetDialogValue2(Dialog, IDC_EDIT7, TempGridSize);
				GridChanged = 1;
			}

			break;

		case IDC_EDIT1:
			res = HIWORD(WParam);

			if (res == 256)
				res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, (WPARAM) - 1, 0);

			ok = 1;
			break;

		case IDC_EDIT3:
			res = HIWORD(WParam);

			if (res == 256)
				res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, (WPARAM) - 1, 0);

			ok = 1;
			break;

		case IDC_EDIT5:
			res = HIWORD(WParam);

			if (res == 256)
				res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, (WPARAM) - 1, 0);

			ok = 1;
			break;

		case IDC_EDIT7:
			res = HIWORD(WParam);

			if (res == 256)
				res2 = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, (WPARAM) - 1, 0);

			ok = 1;
			break;

		case IDHELP:
			Help("options.htm", 0);
			return about;

		case IDCANCEL:
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}

		break;
	}

	about = 0;
	return about;
}

//**********************************************************************************************************************
//************************************** pravidla rozložení ************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsChangeDesignRulesDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	double value;
	int32 DialogPixelsX, DialogPixelsY;
	RECT Rect;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(170, "Trace width"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(211, "Clearance"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(230, "Silkscreen width"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(16, "Board outline keep out"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(237, "Board outline width"));

		TempUnits = Units;
		memcpy(&NewDesign, &Design, sizeof(DesignRecord));
		SetDialogValue2(Dialog, IDD_NEW_EDIT5, NewDesign.StandardTraceWidth);
		SetDialogValue2(Dialog, IDD_NEW_EDIT6, NewDesign.StandardClearance);
		SetDialogValue2(Dialog, IDD_NEW_EDIT7, NewDesign.SilkScreenWidth);
		SetDialogValue2(Dialog, IDD_NEW_EDIT9, NewDesign.BoardOutlineKeepOut);
		SetDialogValue2(Dialog, IDD_NEW_EDIT8, NewDesign.BoardOutlineWidth);

		SendDlgItemUnits(Dialog, IDD_NEW_UN4, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN5, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN6, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN7, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN8, TempUnits);

		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		ok = 1;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			value = GetDialogValue(Dialog, IDD_NEW_EDIT5);
			NewDesign.StandardTraceWidth = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT6);
			NewDesign.StandardClearance = (float) value;
			NewDesign.MaximumClearance = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT7);
			NewDesign.SilkScreenWidth = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT9);
			NewDesign.BoardOutlineKeepOut = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT8);
			NewDesign.BoardOutlineWidth = (float) value;
			TempUnits ^= 1;
			SetDialogValue2(Dialog, IDD_NEW_EDIT5, NewDesign.StandardTraceWidth);
			SetDialogValue2(Dialog, IDD_NEW_EDIT6, NewDesign.StandardClearance);
			SetDialogValue2(Dialog, IDD_NEW_EDIT7, NewDesign.SilkScreenWidth);
			SetDialogValue2(Dialog, IDD_NEW_EDIT9, NewDesign.BoardOutlineKeepOut);
			SetDialogValue2(Dialog, IDD_NEW_EDIT8, NewDesign.BoardOutlineWidth);
			SendDlgItemUnits(Dialog, IDD_NEW_UN4, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN5, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN6, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN7, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN8, TempUnits);
			break;

		case IDOK:
		case IDAPPLY:
			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT5)) < 100.0)
			{
				MessageBoxOwn(PCBWindow, SC(174, "Wrong value for trace width"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.StandardTraceWidth = (float) value;

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT6)) < 100.0)
			{
				MessageBoxOwn(PCBWindow, SC(175, "Wrong value for clearance"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.StandardClearance = (float) value;
			NewDesign.MaximumClearance = (float) value;

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT7)) < 100.0)
			{
				MessageBoxOwn(PCBWindow, SC(243, "Wrong value for silkscreen width"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.SilkScreenWidth = (float) value;

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT9)) < 0.0)
			{
				MessageBoxOwn(PCBWindow, SC(244, "Wrong value for board outline keep out"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.BoardOutlineKeepOut = (float) value;

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT8)) < 0.0)
			{
				MessageBoxOwn(PCBWindow, SC(245, "Wrong value for board outline width"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.BoardOutlineWidth = (float) value;
			ChangeDesignRulesPcb(0);

			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			return 1;

		case IDHELP:
			Help("change_design_rules.htm", 0);
			return about;

		case IDCANCEL:
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}

		break;
	}

	about = 0;
	return about;
}

//**********************************************************************************************************************
//***************************************** prùchodky ******************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsViaDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res;
	ViaRecord *Via;
	double value1, value2, value3, value4, value5;
	int32 DialogPixelsX, DialogPixelsY;
	RECT Rect;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(233, "Pad size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(234, "Drill diameter"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(211, "Clearance"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(235, "Anti power pad"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(266, "Layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(1145, "Via"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(236, "Solder mask"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(232, "Define vias"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(238, "Top layer"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(239, "Bottom layer"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(240, "Top/bottom layer"));

		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF01, SC(248, "Get via definition 1"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF02, SC(249, "Get via definition 2"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF03, SC(250, "Get via definition 3"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF04, SC(251, "Get via definition 4"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF05, SC(1133, "Get via definition 5"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF06, SC(1134, "Get via definition 6"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF07, SC(1135, "Get via definition 7"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF08, SC(1136, "Get via definition 8"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF09, SC(1137, "Get via definition 9"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_GET_DEF10, SC(1138, "Get via definition 10"));

		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF01, SC(252, "Set via definition 1"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF02, SC(253, "Set via definition 2"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF03, SC(254, "Set via definition 3"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF04, SC(255, "Set via definition 4"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF05, SC(1139, "Set via definition 5"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF06, SC(1140, "Set via definition 6"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF07, SC(1141, "Set via definition 7"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF08, SC(1142, "Set via definition 8"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF09, SC(1143, "Set via definition 9"));
		SetDialogItemTextUTF8(Dialog, IDD_VIA_SET_DEF10, SC(1144, "Set via definition 10"));

		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");

		switch (DialogMode)
		{
		case 0:
			SetWindowTextUTF8(Dialog, SC(256, "Define/set current via"));
			break;

		case 1:
			SetWindowTextUTF8(Dialog, SC(257, "Change vias"));
			break;
		}

		TempUnits = Units;
		SetDialogValue2(Dialog, IDC_EDIT1, CurrentVia.ThickNess);
		SetDialogValue2(Dialog, IDC_EDIT3, CurrentVia.DrillThickNess);
		SetDialogValue2(Dialog, IDC_EDIT5, CurrentVia.Clearance);
		SetDialogValue2(Dialog, IDC_EDIT7, CurrentVia.ThermalInner);
		SetDialogValue2(Dialog, IDC_EDIT9, CurrentVia.SoldMask);
		SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);

		switch (CurrentVia.ViaType & 3)
		{
		case 0:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
			break;

		case VIA_SOLDMASK_TOP:	// Only top
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
			break;

		case VIA_SOLDMASK_BOTTOM:	// Only bottom
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
			break;
		}

		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			value1 = GetDialogValue(Dialog, IDC_EDIT1);
			value2 = GetDialogValue(Dialog, IDC_EDIT3);
			value3 = GetDialogValue(Dialog, IDC_EDIT5);
			value4 = GetDialogValue(Dialog, IDC_EDIT7);
			value5 = GetDialogValue(Dialog, IDC_EDIT9);
			TempUnits ^= 1;
			SetDialogValue2(Dialog, IDC_EDIT1, value1);
			SetDialogValue2(Dialog, IDC_EDIT3, value2);
			SetDialogValue2(Dialog, IDC_EDIT5, value3);
			SetDialogValue2(Dialog, IDC_EDIT7, value4);
			SetDialogValue2(Dialog, IDC_EDIT9, value5);
			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);
			break;

		case IDD_VIA_GET_DEF01:
		case IDD_VIA_GET_DEF02:
		case IDD_VIA_GET_DEF03:
		case IDD_VIA_GET_DEF04:
		case IDD_VIA_GET_DEF05:
		case IDD_VIA_GET_DEF06:
		case IDD_VIA_GET_DEF07:
		case IDD_VIA_GET_DEF08:
		case IDD_VIA_GET_DEF09:
		case IDD_VIA_GET_DEF10:
			res = LOWORD(WParam) - IDD_VIA_GET_DEF01;
			Via = &Design.DefVia1;
			Via += res;
			SetDialogValue2(Dialog, IDC_EDIT1, Via->ThickNess);
			SetDialogValue2(Dialog, IDC_EDIT3, Via->DrillThickNess);
			SetDialogValue2(Dialog, IDC_EDIT5, Via->Clearance);
			SetDialogValue2(Dialog, IDC_EDIT7, Via->ThermalInner);
			SetDialogValue2(Dialog, IDC_EDIT9, Via->SoldMask);

			switch (Via->ViaType & 3)
			{
			case 0:
				SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
				break;

			case VIA_SOLDMASK_TOP:
				SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
				break;

			case VIA_SOLDMASK_BOTTOM:
				SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
				break;
			}

			break;

		case IDD_VIA_SET_DEF01:
		case IDD_VIA_SET_DEF02:
		case IDD_VIA_SET_DEF03:
		case IDD_VIA_SET_DEF04:
		case IDD_VIA_SET_DEF05:
		case IDD_VIA_SET_DEF06:
		case IDD_VIA_SET_DEF07:
		case IDD_VIA_SET_DEF08:
		case IDD_VIA_SET_DEF09:
		case IDD_VIA_SET_DEF10:
			res = LOWORD(WParam) - IDD_VIA_SET_DEF01;
			Via = &Design.DefVia1;
			Via += res;
			value1 = GetDialogValue(Dialog, IDC_EDIT1);
			value2 = GetDialogValue(Dialog, IDC_EDIT3);
			value3 = GetDialogValue(Dialog, IDC_EDIT5);
			value4 = GetDialogValue(Dialog, IDC_EDIT7);
			value5 = GetDialogValue(Dialog, IDC_EDIT9);

			if ((value1 < (float) (2540.0)) || (value2 < (float) (2540.0)) || (value3 < (float) (254.0)))
			{
				MessageBoxOwn(PCBWindow, SC(246, "Wrong values"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (value2 > value1 + (float) 10)
			{
				MessageBoxOwn(PCBWindow, SC(247, "Drill diameter > via pad"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			DataBaseChanged = 1;
			Via->ThickNess = (float) value1;
			Via->DrillThickNess = (float) value2;
			Via->Clearance = (float) value3;
			Via->ThermalInner = (float) value4;
			Via->SoldMask = (float) value5;
			Via->ViaType &= ~3;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_GETCHECK, 1, 0) == BST_CHECKED)
				Via->ViaType |= VIA_SOLDMASK_TOP;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_GETCHECK, 1, 0) == BST_CHECKED)
				Via->ViaType |= VIA_SOLDMASK_BOTTOM;

			break;

		case IDOK:
		case IDAPPLY:
			res = 0;
			value1 = GetDialogValue(Dialog, IDC_EDIT1);
			value2 = GetDialogValue(Dialog, IDC_EDIT3);
			value3 = GetDialogValue(Dialog, IDC_EDIT5);
			value4 = GetDialogValue(Dialog, IDC_EDIT7);
			value5 = GetDialogValue(Dialog, IDC_EDIT9);

			if ((value1 < (2540.0)) || (value2 < (2540.0)) || (value3 < (254.0)))
			{
				MessageBoxOwn(PCBWindow, SC(246, "Wrong values"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (value2 > value1 + 10)
			{
				MessageBoxOwn(PCBWindow, SC(247, "Drill diameter > via pad"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			CurrentVia.ThickNess = (float) value1;
			CurrentVia.DrillThickNess = (float) value2;
			CurrentVia.Clearance = (float) value3;
			CurrentVia.ThermalInner = (float) value4;
			CurrentVia.SoldMask = (float) value5;
			CurrentVia.ViaType &= ~3;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_GETCHECK, 1, 0) == BST_CHECKED)
				CurrentVia.ViaType |= VIA_SOLDMASK_TOP;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_GETCHECK, 1, 0) == BST_CHECKED)
				CurrentVia.ViaType |= VIA_SOLDMASK_BOTTOM;

			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			return 1;

		case IDHELP:
			Help("via_definition.htm", 0);
			return about;

		case IDCANCEL:
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}

		break;
	}

	about = 0;
	return about;
}

//**********************************************************************************************************************
//************************************* chránìné komponenty ************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsProtectComponentsDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, Found, cnt, res, cnt2, First, count, cnt3, TabStops[5];
	char str[200], str2[200], str3[200], str4[200], str5[200];
	CompRecord *Comp;
	int32 DialogPixelsX, DialogPixelsY;
	RECT Rect;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(1326, "Search component"));
		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_UNSELECTALL, SC(161, "Unselect all"));
		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				cnt2++;
		}

		if (cnt2 > 100)
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_INITSTORAGE, cnt2, cnt2 * 80);

		AvailableComps = cnt2;
		count = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (Comp->Info & COMPONENT_PROTECTED)
					count++;
			}
		}

		sprintf(str, SC(180, "Protected components [ %i of %i ]"), count, cnt2); //upraveno
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, str);
		TabStops[0] = 700;
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, EM_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);
		First = -1;
		TotalCompCount = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				sprintf(str, "%s\t%s", Comp->Name, Comp->Value);

				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) (str));

				if (res >= 0)
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETITEMDATA, res, cnt);

				TotalCompCount++;
			}
		}

		for (cnt = 0; cnt < TotalCompCount; cnt++)
		{
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

			if (res != LB_ERR)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[res]]);

				if (Comp->Info & COMPONENT_PROTECTED)
				{
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, cnt);

					if (First == -1)
						First = cnt;
				}
			}
		}

		if (First != -1)
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTOPINDEX, First, 0);

		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		ok = 1;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
		return about;
#ifdef _DEBUG

//    case WM_MOUSEWHEEL:
	case WM_CHAR:
	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
	case WM_KEYUP:
	case WM_SYSKEYUP:
	case WM_SETFOCUS:
	case WM_KILLFOCUS:
		sprintf(str, "PcbSettingsProtectComponentsDialog: Message 0x%04x, WPARAM = %d,%d LPARAM = %d,%d\n", Message,
		        LOWORD(WParam), HIWORD(WParam), LOWORD(LParam), HIWORD(LParam));
		OutputDebugString(str);
		break;
#endif

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
		case IDAPPLY:
			for (cnt = 0; cnt < TotalCompCount; cnt++)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

				if (res != LB_ERR)
				{
					Comp = (CompRecord *) & (CompsMem[(*Comps)[res]]);
					Comp->Info &= ~COMPONENT_PROTECTED;

					if (SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0))
						Comp->Info |= COMPONENT_PROTECTED;
				}
			}

			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			return 1;

		case IDD_UNSELECTALL:
			for (cnt = 0; cnt < TotalCompCount; cnt++)
				SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 0, (LPARAM) cnt);

			break;

		case IDD_SELECTALL:
			for (cnt = 0; cnt < TotalCompCount; cnt++)
				SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) cnt);

			break;
			
//************************************* hledat komponent ************************************************************

		case IDC_BUTTON1:
			if (SendDlgItemMessage(Dialog, IDC_EDIT1, WM_GETTEXT, 100, (LPARAM) str))
			{
				Found = -1;

				if (stricmpUTF8(CompSearchString, str))
					CompSearchCnt = -1;

				strcpy(CompSearchString, str);
				cnt3 = strlen(CompSearchString);

				for (cnt = SearchCnt1 + 1; cnt < TotalCompCount; cnt++)
				{
					str2[0] = 0;
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str2);
					strcpy(str4, str2);
					GetString(str2, str3);

					if (strnicmpUTF8(str, str3, min(cnt3, (int32) strlen(str3))) == 0)
					{
						Found = cnt;
						break;
					}
					else
					{
						GetString(str2, str3);

						if (strnicmpUTF8(str, str3, min(cnt3, (int32) strlen(str3))) == 0)
						{
							Found = cnt;
							break;
						}
					}
				}

				if (Found != -1)
				{
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTOPINDEX, Found, 0);
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCARETINDEX, Found, 0);
					CompSearchCnt = Found;
				}
				else
				{
					if (CompSearchCnt > -1)
					{
						CompSearchCnt = -1;
						Found = -1;

						for (cnt = CompSearchCnt + 1; cnt < TotalCompCount; cnt++)
						{
							str2[0] = 0;
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str2);
							GetString(str2, str3);

							if (strnicmpUTF8(str, str3, strlen(str3)) == 0)
							{
								Found = cnt;
								break;
							}
							else
							{
								GetString(str2, str3);

								if (strnicmpUTF8(str, str3, min(cnt3, (int32) strlen(str3))) == 0)
								{
									Found = cnt;
									break;
								}
							}
						}

						if (Found != -1)
						{
							CompSearchCnt = Found;
							strcpy(CompSearchString, str);
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTOPINDEX, Found, 0);
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCARETINDEX, Found, 0);
						}
						else
						{
							sprintf(str5, SC(1330, "%s not found"), str);
							MessageBoxOwn(PCBWindow, str5, SC(1, "Message"), MB_APPLMODAL | MB_OK);
						}
					}
					else
					{
						sprintf(str5, SC(1330, "%s not found"), str);
						MessageBoxOwn(PCBWindow, str5, SC(1, "Message"), MB_APPLMODAL | MB_OK);
					}
				}
			}

//*********************************************** nápovìda ******************************************************

			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			return 1;

		case IDHELP:
			Help("component_Protection.htm", 0);
			return about;

		case IDCANCEL:
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}

		break;
	}

	about = 0;
	return about;
}

//**********************************************************************************************************************
//*********************************** tabulky pravidla sítì ************************************************************
//**********************************************************************************************************************

int32 FillListBoxFromNets(HWND Dialog, int32 ListboxNets, int32 ListboxHeader, int32 mode, int32 Units)
{
	int32 cnt, res;
	char str[MAX_LENGTH_STRING];
	NetRecord *Net;
	double UnitValue = 2540.0;
	int32 TabStops[4];


	if (Units == 1)
		UnitValue = 1e5;

	SendDlgItemMessage(Dialog, ListboxNets, LB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(Dialog, ListboxHeader, LB_RESETCONTENT, 0, 0);

	if (mode == 0)
	{
		TabStops[0] = 110;
		TabStops[1] = 140;
		TabStops[2] = 175;
		TabStops[3] = 500;
		SendDlgItemMessageOwn(Dialog, ListboxHeader, LB_ADDSTRING, 0,
		                      (LPARAM) SC(1152, "Net name\tWidth\tClear\tVia nr"));
	}
	else
	{
		TabStops[0] = 30;
		TabStops[1] = 60;
		TabStops[2] = 175;
		TabStops[3] = 550;
		SendDlgItemMessageOwn(Dialog, ListboxHeader, LB_ADDSTRING, 0,
		                      (LPARAM) SC(1169, "Width\tClear\tNet name\tVia nr"));
	}

	SendDlgItemMessage(Dialog, ListboxNets, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
	SendDlgItemMessage(Dialog, ListboxHeader, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (Net->Name[0] != 0)
		{
			if (mode == 0)
			{
				//**************** Seøadit podle názvu sítì *************************************************************
				if (Net->ViaNr > 0)
				{
					sprintf(str, "%s\t%.4f\t%.4f\t%d", Net->Name, Net->TraceWidth / UnitValue,
						Net->TraceClearance / UnitValue, Net->ViaNr, cnt);
				}
				else
				{
					sprintf(str, "%s\t%.4f\t%.4f\t%d", Net->Name, Net->TraceWidth / UnitValue,
						Net->TraceClearance / UnitValue, cnt);
				}
			}
			else
			{
				//**************** Seøadit podle šíøky trasy ************************************************************
				if (Net->ViaNr > 0)
				{
					sprintf(str, "%.4f\t%.4f\t%s\t%d", Net->TraceWidth / UnitValue, Net->TraceClearance / UnitValue,
						Net->Name, Net->ViaNr, cnt);
				}
				else
				{
					sprintf(str, "%.4f\t%.4f\t%s\t%d", Net->TraceWidth / UnitValue, Net->TraceClearance / UnitValue,
						Net->Name, cnt);
				}
			}

			res = SendDlgItemMessageOwn(Dialog, ListboxNets, LB_ADDSTRING, 0, (LPARAM) (str));

			if (res >= 0)
				res = SendDlgItemMessage(Dialog, ListboxNets, LB_SETITEMDATA, res, cnt);
		}
	}

	return 0;
}

//**********************************************************************************************************************
//*************************************** pravidla sítì ****************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsNetTypeDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res, cnt, SortMode, ViaNr, First, NetNr, NetChanged;
	char str[MAX_LENGTH_STRING];
	NetRecord *Net;
	ViaRecord *Via;
	double TempViaThickNess, TempViaDrillThickNess, TempViaSoldMask;
	int32 DialogPixelsX, DialogPixelsY;
	RECT Rect;

	SortMode = 0;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(1151, "Trace/Clearance width"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(1150, "Trace"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(1097, "Selection"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(1145, "Via"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(1146, "Pad size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(1147, "Drill size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(1148, "Solder mask"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(211, "Clearance"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(1098, "Sort by netname"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(1099, "Sort by trace width"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_DESELECTALL, SC(161, "Deselect all"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(1327, "Set only net(s) design rule"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(1328, "Set net(s) design rule and also\r\nchange all traces/vias/areafills"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(1149, "Set"));


		
		if (InRange(TempTraceWidth, 0.0))
			TempTraceWidth = Design.StandardTraceWidth;

		if (InRange(TempClearance, 0.0))
			TempClearance = Design.StandardClearance;

		TempUnits = Units;
		SetDialogValue(Dialog, IDC_EDIT1, TempTraceWidth);
		SetDialogValue(Dialog, IDC_EDIT3, TempClearance);
		SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);

		switch (NetDialogMode)
		{
		case 0:

			sprintf(str, SC(173, "Shown [ %d nets ]"), Design.NrNets);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC9, str);
			break;
		}

		SortMode = 0;
		SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
		
		TotalNetCount = 0;

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if (Net->Name[0] != 0)
				TotalNetCount++;
		}

		if (TotalNetCount > 100)
			res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_INITSTORAGE, TotalNetCount, TotalNetCount * 60);

		FillListBoxFromNets(Dialog, IDC_LIST3, IDC_LIST1, SortMode, TempUnits);
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(1153, "Current"));

		for (cnt = 1; cnt < 11; cnt++) //upraveno
		{
			sprintf(str, "%d", cnt);
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) str);
		}

		if (SpecialNetNr != -1)
		{
			str[0] = 0;
			NetNr = -1;

			for (cnt = 0; cnt < TotalNetCount; cnt++)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETITEMDATA, cnt, 0);

				if (res != LB_ERR)
				{
					if (res == LParam)
						NetNr = res;
				}
			}

			if (NetNr != -1)
			{
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SELITEMRANGE, 1, MAKELPARAM(cnt, cnt));
				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTOPINDEX, max(0, cnt - 18), 0);
				Net = &((*Nets)[NetNr]);
				SetDialogValue(Dialog, IDC_EDIT1, Net->TraceWidth);
				SetDialogValue(Dialog, IDC_EDIT3, Net->TraceClearance);

				if ((Net->ViaNr >= 0) && (Net->ViaNr <= 10))
				{
					res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, Net->ViaNr, 0);

					if (Net->ViaNr > 0)
					{
						Via = &Design.DefVia1;
						Via += Net->ViaNr - 1;
					}
					else
						Via = &CurrentVia;

					SetDialogValue(Dialog, IDC_EDIT5, Via->ThickNess);
					SetDialogValue(Dialog, IDC_EDIT7, Via->DrillThickNess);
					SetDialogValue(Dialog, IDC_EDIT9, Via->SoldMask);
					SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
					SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
					SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);
				}
			}
		}

		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_LIST3:
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, (WPARAM) - 1, 0);
				NetsSelected = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSELCOUNT, 0, 0);

				if (NetsSelected == 1)
				{
					NetNr = -1;

					for (cnt = 0; cnt < TotalNetCount; cnt++)
					{
						if (SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSEL, cnt, 0))
						{
							res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETITEMDATA, cnt, 0);

							if (res != LB_ERR)
							{
								NetNr = res;
								break;
							}
						}
					}

					if ((NetNr >= 0) && (NetNr < Design.NrNets))
					{
						Net = &((*Nets)[NetNr]);
						SetDialogValue(Dialog, IDC_EDIT1, Net->TraceWidth);
						SetDialogValue(Dialog, IDC_EDIT3, Net->TraceClearance);

						if ((Net->ViaNr >= 0) && (Net->ViaNr <= 10))
						{
							res = SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, Net->ViaNr, 0);

							if (Net->ViaNr > 0)
							{
								Via = &Design.DefVia1;
								Via += Net->ViaNr - 1;
							}
							else
								Via = &CurrentVia;

							SetDialogValue(Dialog, IDC_EDIT5, Via->ThickNess);
							SetDialogValue(Dialog, IDC_EDIT7, Via->DrillThickNess);
							SetDialogValue(Dialog, IDC_EDIT9, Via->SoldMask);
							SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
							SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
							SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);
						}
					}

					res = 1;
				}
				else
				{
					SendDlgItemMessageOwn(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) "");
					SendDlgItemMessageOwn(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) "");
					SendDlgItemMessageOwn(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) "");
					SendDlgItemMessageOwn(Dialog, IDC_EDIT8, WM_SETTEXT, 0, (LPARAM) "");
					SendDlgItemMessageOwn(Dialog, IDC_EDIT9, WM_SETTEXT, 0, (LPARAM) "");
					SendDlgItemMessageOwn(Dialog, IDC_EDIT10, WM_SETTEXT, 0, (LPARAM) "");
				}
			}

			break;

		case IDD_SELECTALL:
			SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_SELITEMRANGE, 1, MAKELPARAM(0, TotalNetCount));
			break;

		case IDD_DESELECTALL:
			SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_SELITEMRANGE, 0, MAKELPARAM(0, TotalNetCount));
			break;

		case IDD_UNITS:
			TempTraceWidth = GetDialogValue(Dialog, IDC_EDIT1);
			TempClearance = GetDialogValue(Dialog, IDC_EDIT3);
			TempViaThickNess = GetDialogValue(Dialog, IDC_EDIT5);
			TempViaDrillThickNess = GetDialogValue(Dialog, IDC_EDIT7);
			TempViaSoldMask = GetDialogValue(Dialog, IDC_EDIT9);
			TempUnits ^= 1;
			SetDialogValue(Dialog, IDC_EDIT1, TempTraceWidth);
			SetDialogValue(Dialog, IDC_EDIT3, TempClearance);
			SetDialogValue(Dialog, IDC_EDIT5, TempViaThickNess);
			SetDialogValue(Dialog, IDC_EDIT7, TempViaDrillThickNess);
			SetDialogValue(Dialog, IDC_EDIT9, TempViaSoldMask);
			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);
			FillListBoxFromNets(Dialog, IDC_LIST3, IDC_LIST1, SortMode, TempUnits);
			break;

		case IDC_BUTTON1:		// Change trace width/clearance for nets
		case IDC_BUTTON2:		// Change trace width/clearance for nets including all traces/via/areafills
			NetsSelected = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSELCOUNT, 0, 0);

			if (NetsSelected > 0)
			{
				TempTraceWidth = GetDialogValue(Dialog, IDC_EDIT1);
				TempClearance = GetDialogValue(Dialog, IDC_EDIT3);

				if (TempTraceWidth < (float) 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(174, "Wrong value for trace width"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				if (TempClearance < (float) 10.0)
				{
					MessageBoxOwn(PCBWindow, SC(175, "Wrong value for clearance"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				if (LOWORD(WParam) == IDC_BUTTON2)
				{
					strcpy(str, SC(179, "This operation can not be undone"));
					strcat(str, "\r\n");
					strcat(str, SC(401, "Do you want to continue ?"));
					res = MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO);
				}
				else
					res = IDYES;

				if (res == IDNO)
					return about;

				NetChanged = 0;
				First = -1;
				AllocateSpecialMem(MEM_NETINFO, (Design.NrNets + 10) * sizeof(int32), (void **) &NetInfo);

				for (cnt = 0; cnt < TotalNetCount; cnt++)
				{
					NetInfo[cnt] = 0;

					if (SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSEL, cnt, 0))
					{
						NetInfo[cnt] = 1;

						if (First == -1)
							First = cnt;

						NetNr = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETITEMDATA, cnt, 0);

						if (NetNr != LB_ERR)
						{
							if (LOWORD(WParam) == IDC_BUTTON2)
							{
								// Change trace width/clearance for nets including all traces/via/areafills
								ChangeNetType(NetNr, TempTraceWidth, TempClearance, 0);
							}
							else
							{
								// Change trace width/clearance for nets
								ChangeNetType(NetNr, TempTraceWidth, TempClearance, 1);
							}

							DataBaseChanged = 1;
							NetChanged = 1;
						}
					}
				}

				if (NetChanged)
				{
					FillListBoxFromNets(Dialog, IDC_LIST3, IDC_LIST1, SortMode, TempUnits);

					for (cnt = 0; cnt < TotalNetCount; cnt++)
					{
						if (NetInfo[cnt])
							SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETSEL, 1, cnt);
					}

					SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTOPINDEX, max(0, First - 18), 0);
				}

				DeallocateSpecialMem(MEM_NETINFO);
				RePaint();
				SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			}

			break;

		case IDC_BUTTON3:		// Change via for net(s)
			ViaNr = SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
			NetsSelected = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSELCOUNT, 0, 0);
			NetChanged = 0;
			First = -1;
			AllocateSpecialMem(MEM_NETINFO, (Design.NrNets + 10) * sizeof(int32), (void **) &NetInfo);

			for (cnt = 0; cnt < TotalNetCount; cnt++)
			{
				NetInfo[cnt] = 0;

				if (SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSEL, cnt, 0))
				{
					NetInfo[cnt] = 1;

					if (First == -1)
						First = cnt;

					NetNr = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETITEMDATA, cnt, 0);

					if ((NetNr != LB_ERR) && (NetNr >= 0) && (NetNr < Design.NrNets))
					{
						Net = &((*Nets)[NetNr]);
						Net->ViaNr = ViaNr;
						DataBaseChanged = 1;
						NetChanged = 1;
					}
				}
			}

			if (NetChanged)
			{
				FillListBoxFromNets(Dialog, IDC_LIST3, IDC_LIST1, SortMode, TempUnits);

				for (cnt = 0; cnt < TotalNetCount; cnt++)
				{
					if (NetInfo[cnt])
						SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETSEL, 1, cnt);
				}

				SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTOPINDEX, max(0, First - 18), 0);
			}

			DeallocateSpecialMem(MEM_NETINFO);

			break;

		case IDC_LIST2:
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);

			if (res > 0)
			{
				Via = &Design.DefVia1;
				Via += res - 1;
			}
			else
				Via = &CurrentVia;

			SetDialogValue(Dialog, IDC_EDIT5, Via->ThickNess);
			SetDialogValue(Dialog, IDC_EDIT7, Via->DrillThickNess);
			SetDialogValue(Dialog, IDC_EDIT9, Via->SoldMask);
			SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);
			break;

		case IDC_RADIO1:
			SortMode = 0;
			FillListBoxFromNets(Dialog, IDC_LIST3, IDC_LIST1, SortMode, TempUnits);
			break;

		case IDC_RADIO2:
			SortMode = 1;
			FillListBoxFromNets(Dialog, IDC_LIST3, IDC_LIST1, SortMode, TempUnits);
			break;

		case IDOK:
		case IDAPPLY:
			NetsSelected = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSELCOUNT, 0, 0);

			if ((SpecialNetNr != -1) && (NetsSelected == 1))
			{
				TempTraceWidth = GetDialogValue(Dialog, IDC_EDIT1);
				TempClearance = GetDialogValue(Dialog, IDC_EDIT3);

				if (TempTraceWidth < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(174, "Wrong value for trace width"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				if (TempClearance < 10.0)
				{
					MessageBoxOwn(PCBWindow, SC(175, "Wrong value for clearance"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				for (cnt = 0; cnt < TotalNetCount; cnt++)
				{
					if (SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSEL, cnt, 0))
					{
						NetNr = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETITEMDATA, cnt, 0);

						if (NetNr != LB_ERR)
						{
							ChangeNetType(NetNr, TempTraceWidth, TempClearance, 1);
							break;
						}
					}
				}
			}

			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			return 1;

		case IDHELP:
			Help("change_design_rules_nets.htm", 0);
			return about;

		case IDCANCEL:
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}
	}

	about = 0;
	return about;
}

//**********************************************************************************************************************
//******************************** neaktivní blok preprocesoru *********************************************************
//**********************************************************************************************************************

#if 0

void NetTypesDialog(int32 NetNr)
{
	int32 res, cnt, cnt2, index, Layer, LicenseResult, PcbFeaures;
	AreaFillRecord *AreaFill;
	NetRecord *Net;
	TraceRecord *Trace;
	ViaRecord *Via;
	char str[1024];
	NetDialogMode = 0;

	AllocateSpecialMem(MEM_NET_SELECTED, (Design.NrNets + 10) * sizeof(uint8), (void **) &SelectedNets);
	AllocateSpecialMem(MEM_NETINFO, (Design.NrNets + 10) * sizeof(int32), (void **) &NetInfo);

	LicenseResult = CheckLicense(&PcbFeaures, 1);
	res =
	    OwnDialogBoxParam(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NETTYPE), PCBWindow,
	                      (DLGPROC) PcbSettingsNetTypeDialog, NetNr);

	if (res == 1)
	{
		if (NetsSelected > 0)
		{
			sprintf(str, SC(1121, "The design rules of the selected nets have been changed\r\n\r\n"));
			strcat(str,
			       SC(785,
			          "Do you also want to change the width/clearance of already existing traces/vias from the selected nets\r\n\r\n"));
			strcat(str,
			       SC(177, "The clearance of the traces/vias/areafills of the selected nets will be modified,\r\n"));
			strcat(str, SC(178, "and the width of the traces of the selected nets will be modified\r\n\r\n"));
			strcat(str, SC(179, "This operation can not be undone"));
			res = MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO);

			switch (res)
			{
			case IDYES:
				for (cnt2 = 0; cnt2 < NetsSelected; cnt2++)
				{
					index = (int32) NetInfo[cnt2];
					Net = &((*Nets)[index]);

					Net->TraceWidth = (float) TempTraceWidth;
					Net->TraceClearance = (float) TempClearance;
					DataBaseChanged = 1;

					// *******************************************************************************************************
					for (Layer = 0; Layer < 32; Layer++)
					{
						for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
						{
							Trace = &((*VerTraces[Layer])[cnt]);

							if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == index))
							{
								Trace->ThickNess = (float) TempTraceWidth;
								Trace->Clearance = (float) TempClearance;
							}
						}

						for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
						{
							Trace = &((*HorTraces[Layer])[cnt]);

							if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == index))
							{
								Trace->ThickNess = (float) TempTraceWidth;
								Trace->Clearance = (float) TempClearance;
							}
						}

						for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
						{
							Trace = &((*Diag1Traces[Layer])[cnt]);

							if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == index))
							{
								Trace->ThickNess = (float) TempTraceWidth;
								Trace->Clearance = (float) TempClearance;
							}
						}

						for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
						{
							Trace = &((*Diag2Traces[Layer])[cnt]);

							if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == index))
							{
								Trace->ThickNess = (float) TempTraceWidth;
								Trace->Clearance = (float) TempClearance;
							}
						}
					}

// *******************************************************************************************************
					for (cnt = 0; cnt < Design.NrVias; cnt++)
					{
						Via = &((*Vias)[cnt]);

						if (((Via->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Via->NetNr == index))
							Via->Clearance = (float) TempClearance;
					}

// *******************************************************************************************************
					for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
					{
						AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

						if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr == index))
							AreaFill->Clearance = (float) TempClearance;
					}
				}

				break;

			case IDNO:
				for (cnt2 = 0; cnt2 < NetsSelected; cnt2++)
				{
					index = (int32) NetInfo[cnt2];
					Net = &((*Nets)[index]);
					Net->TraceWidth = (float) TempTraceWidth;
					Net->TraceClearance = (float) TempClearance;
					DataBaseChanged = 1;
				}

				break;
			}

			RePaint();
		}
	}

	DeallocateSpecialMem(MEM_NET_SELECTED);
	DeallocateSpecialMem(MEM_NETINFO);
}

int32 ProtectComponentsDialog(void)
{
	return int32();
}

#endif

//**********************************************************************************************************************
//************************************* sítì/pøipojení *****************************************************************
//**********************************************************************************************************************

int32 CALLBACK PcbSettingsNetInfoDialog(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 cnt, cnt2, cnt3, cnt4, res, Found, Layer, NetNr, count, *NetInfo;
	NetRecord *Net;
	ConnectionsRecord *Connection;
	ViaRecord *Via;
	AreaFillRecord *AreaFill;
	TraceRecord *Trace;
	char str[200], str2[200];
	int32 DialogPixelsX, DialogPixelsY;
	RECT Rect;

	switch (Message)
	{
	case WM_INITDIALOG:
		if (NetInfoMode == 0)
		{
			SetWindowLong(Dialog, GWL_EXSTYLE, WS_EX_CONTROLPARENT);

			if (PcbSettingsActiveTab)
			{
				if (PcbSettingsActiveTab->DialogControlHandles[0] == NULL)
				{
					PcbSettingsActiveTab->DialogControlHandles[0] = GetDlgItem(Dialog, IDC_LIST1);
					PcbSettingsActiveTab->DialogControlHandles[1] = GetDlgItem(Dialog, IDC_LIST2);
					PcbSettingsActiveTab->DialogControlHandles[2] = GetDlgItem(Dialog, IDC_LIST3);
					PcbSettingsActiveTab->DialogControlIds[0] = IDC_LIST1;
					PcbSettingsActiveTab->DialogControlIds[1] = IDC_LIST2;
					PcbSettingsActiveTab->DialogControlIds[2] = IDC_LIST3;
					PcbSettingsActiveTab->NrDialogControls = 3;
				}
			}

			PcbSettingsActiveTab->CurrentDialogControlNr = 0;
			DefDlgProc(Dialog, WM_NEXTDLGCTL, (WPARAM) PcbSettingsActiveTab->DialogControlHandles[0], 1);
		}

		AllocateSpecialMem(MEM_NET_SELECTED, (Design.NrNets + 10) * sizeof(uint8), (void **) &SelectedNets);
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDAPPLY, SC(151, "Apply"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(1321, "Search net"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(1321, "Search net"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(1321, "Search net"));
		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL2, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL3, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_UNSELECTALL, SC(161, "Unselect all"));
		SetDialogItemTextUTF8(Dialog, IDD_UNSELECTALL2, SC(161, "Unselect all"));
		SetDialogItemTextUTF8(Dialog, IDD_UNSELECTALL3, SC(161, "Unselect all"));

		switch (NetInfoMode)
		{
		case 0:				// Nets disable,highlight,invisble
			sprintf(str, SC(1322, "Highlited nets [ %i nets ]"), Design.NrNets);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, str);
			sprintf(str, SC(1323, "Net connections visible [ %i nets ]"), Design.NrNets);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC2, str);
			sprintf(str, SC(1324, "Disable connections nets [ %i nets ]"), Design.NrNets);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC3, str);
			break;

		case 1:				// Unselect traces/vias nets
			sprintf(str, SC(1325, "Selected nets [ %i nets ]"), Design.NrNets);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, str);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC2, "");
			SetDialogItemTextUTF8(Dialog, IDC_STATIC3, "");
			break;

		case 2:				// Delete nets objects selected
			sprintf(str, SC(1325, "Selected nets [ %i nets ]"), Design.NrNets);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, str);
			SetDialogItemTextUTF8(Dialog, IDC_STATIC2, "");
			SetDialogItemTextUTF8(Dialog, IDC_STATIC3, "");
			break;
		}

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if (Net->Name[0] != 0)
				cnt2++;
		}

		if (cnt2 > 100)
		{
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_INITSTORAGE, cnt2, cnt2 * 60);
			res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_INITSTORAGE, cnt2, cnt2 * 60);
			res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_INITSTORAGE, cnt2, cnt2 * 60);
		}

		cnt2 = -1;
		cnt3 = -1;
		cnt4 = -1;

		switch (NetInfoMode)
		{
		case 0:				// Nets disable,highlight,invisble
			TotalNetCount = 0;

			for (cnt = 0; cnt < Design.NrNets; cnt++)
			{
				Net = &((*Nets)[cnt]);

				if (Net->Name[0] != 0)
				{
					res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));

					if (res >= 0)
						res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETITEMDATA, res, cnt);

					res = SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));

					if (res >= 0)
						SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETITEMDATA, res, cnt);

					res = SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));

					if (res >= 0)
						SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETITEMDATA, res, cnt);

					TotalNetCount++;
				}
			}

			for (cnt = 0; cnt < TotalNetCount; cnt++)
			{
				NetNr = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

				if (NetNr != LB_ERR)
				{
					Net = &((*Nets)[NetNr]);

					if (Net->Info & OBJECT_HIGHLITED)
					{
						SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, cnt);

						if (cnt2 == -1)
							cnt2 = cnt;
					}
				}

				NetNr = SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETITEMDATA, cnt, 0);

				if (NetNr != LB_ERR)
				{
					Net = &((*Nets)[NetNr]);

					if ((Net->Info & CONNECTIONS_NOT_VISIBLE) == 0)
					{
						SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETSEL, 1, cnt);

						if (cnt3 == -1)
							cnt3 = cnt;
					}
				}

				NetNr = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETITEMDATA, cnt, 0);

				if (NetNr != LB_ERR)
				{
					Net = &((*Nets)[NetNr]);

					if (Net->Info & CONNECTIONS_DISABLED)
					{
						SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETSEL, 1, cnt);

						if (cnt4 == -1)
							cnt4 = cnt;
					}
				}
			}

			if (cnt2 < 0)
				cnt2 = 0;

			if (cnt3 < 0)
				cnt3 = 0;

			if (cnt4 < 0)
				cnt4 = 0;

			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCARETINDEX, cnt2, 0);
			res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCARETINDEX, cnt3, 0);
			res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCARETINDEX, cnt4, 0);

			break;

		case 1:				// Unselect traces/vias nets
			TotalNetCount = 0;

			for (cnt = 0; cnt < Design.NrNets; cnt++)
			{
				Net = &((*Nets)[cnt]);

				if (Net->Name[0] != 0)
				{
					res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));

					if (res >= 0)
						res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETITEMDATA, res, cnt);

					TotalNetCount++;
				}
			}

			count = SelectNetsObjectsSelected(SelectedNets, Design.NrNets);

			for (cnt = 0; cnt < TotalNetCount; cnt++)
			{
				NetNr = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

				if (NetNr != LB_ERR)
				{
					if (SelectedNets[NetNr])
					{
						SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, cnt);

						if (cnt2 == -1)
							cnt2 = cnt;
					}
				}
			}

			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTOPINDEX, cnt2, 0);
			break;

		case 2:				// Delete nets objects selected
			TotalNetCount = 0;

			for (cnt = 0; cnt < Design.NrNets; cnt++)
			{
				Net = &((*Nets)[cnt]);

				if (Net->Name[0] != 0)
				{
					res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));

					if (res >= 0)
						res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETITEMDATA, res, cnt);

					TotalNetCount++;
				}
			}

			break;
		}

		GetWindowRect(Dialog, &Rect);
		DialogPixelsX = Rect.right - Rect.left;
		DialogPixelsY = Rect.bottom - Rect.top;
		MoveWindow(Dialog, SpecialWindowInDialogStartX, SpecialWindowInDialogStartY,
		           DialogPixelsX + SpecialWindowInDialogStartX, DialogPixelsY + SpecialWindowInDialogStartY, 1);
		SearchCnt1 = -1;
		SearchCnt2 = -1;
		SearchCnt3 = -1;
		return 1;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDAPPLY:
		case IDOK:

			switch (NetInfoMode)
			{
			case 0:			// Nets disable,highlight,invisble
				ok = 1;
				SetWaitCursor();
				AllocateSpecialMem(MEM_NETINFO, TotalNetCount * sizeof(int32), (void **) &NetInfo);
				memset(NetInfo, 0, sizeof(int32) * TotalNetCount);

				for (cnt = 0; cnt < TotalNetCount; cnt++)
				{
					NetNr = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

					if (NetNr != LB_ERR)
					{
						Net = &((*Nets)[NetNr]);

						if (Net->Name[0] != 0)
						{
							NetInfo[NetNr] = 0;

							if (SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0))
							{
								// Selected
								if ((Net->Info & OBJECT_HIGHLITED) == 0)
								{
									NetInfo[NetNr] |= OBJECT_HIGHLITED;
									DataBaseChanged = 1;
									Net->Info ^= OBJECT_HIGHLITED;
								}
							}
							else
							{
								// Not selected
								if (Net->Info & OBJECT_HIGHLITED)
								{
									DataBaseChanged = 1;
									Net->Info ^= OBJECT_HIGHLITED;
								}
							}

							if (SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETSEL, cnt, 0))
							{
								// Selected
								if (Net->Info & CONNECTIONS_NOT_VISIBLE)
								{
									Net->Info ^= CONNECTIONS_NOT_VISIBLE;
									DataBaseChanged = 1;
								}
							}
							else
							{
								// Not selected
								if ((Net->Info & CONNECTIONS_NOT_VISIBLE) == 0)
								{
									NetInfo[NetNr] |= CONNECTIONS_NOT_VISIBLE;
									Net->Info ^= CONNECTIONS_NOT_VISIBLE;
									DataBaseChanged = 1;
								}
							}
						}
					}
				}

				for (Layer = 0; Layer < 32; Layer++)
				{
					DrawCode = DrawLayerCode[Layer];

					for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
					{
						Trace = &((*VerTraces[Layer])[cnt]);

						if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets))
						{
							if (NetInfo[Trace->NetNr] & OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
							else
								Trace->Info &= ~OBJECT_HIGHLITED;
						}
					}

					for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
					{
						Trace = &((*HorTraces[Layer])[cnt]);

						if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets))
						{
							if (NetInfo[Trace->NetNr] & OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
							else
								Trace->Info &= ~OBJECT_HIGHLITED;
						}
					}

					for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
					{
						Trace = &((*Diag1Traces[Layer])[cnt]);

						if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets))
						{
							if (NetInfo[Trace->NetNr] & OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
							else
								Trace->Info &= ~OBJECT_HIGHLITED;
						}
					}

					for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
					{
						Trace = &((*Diag2Traces[Layer])[cnt]);

						if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets))
						{
							if (NetInfo[Trace->NetNr] & OBJECT_HIGHLITED)
								Trace->Info |= OBJECT_HIGHLITED;
							else
								Trace->Info &= ~OBJECT_HIGHLITED;
						}
					}
				}

				for (cnt = 0; cnt < Design.NrVias; cnt++)
				{
					Via = &((*Vias)[cnt]);

					if (((Via->Info & OBJECT_NOT_VISIBLE) == 0) && (Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
					{
						if (NetInfo[Via->NetNr] & OBJECT_HIGHLITED)
							Via->Info |= OBJECT_HIGHLITED;
						else
							Via->Info &= ~OBJECT_HIGHLITED;
					}
				}

				for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

					if (((AreaFill->Info & OBJECT_NOT_VISIBLE) == 0) && (AreaFill->NetNr >= 0)
					        && (AreaFill->NetNr < Design.NrNets))
					{
						if (NetInfo[AreaFill->NetNr] & OBJECT_HIGHLITED)
							AreaFill->Info |= OBJECT_HIGHLITED;
						else
							AreaFill->Info &= ~OBJECT_HIGHLITED;
					}
				}

				for (cnt = 0; cnt < Design.NrConnections; cnt++)
				{
					Connection = &((*Connections)[cnt]);

					if (((Connection->Info & OBJECT_NOT_VISIBLE) == 0) && (Connection->NetNr >= 0)
					        && (Connection->NetNr < Design.NrNets))
					{
						if (NetInfo[Connection->NetNr] & CONNECTIONS_NOT_VISIBLE)
							Connection->Info |= CONNECTIONS_NOT_VISIBLE;
						else
							Connection->Info &= ~CONNECTIONS_NOT_VISIBLE;
					}
				}

				DeallocateSpecialMem(MEM_NETINFO);

				for (cnt = 0; cnt < TotalNetCount; cnt++)
				{
					NetNr = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

					if (NetNr != LB_ERR)
					{
						Net = &((*Nets)[NetNr]);

						if (Net->Name[0] != 0)
						{
							if (SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETSEL, cnt, 0))
							{
								if ((Net->Info & CONNECTIONS_DISABLED) == 0)
								{
									DeleteAndUndisplayConnectionsNet(NetNr, 1);
									DataBaseChanged = 1;
									Net->Info |= CONNECTIONS_DISABLED;
								}
							}
							else
							{
								if (Net->Info & CONNECTIONS_DISABLED)
								{
									DataBaseChanged = 1;
									DeleteAndUndisplayConnectionsNet(NetNr, 1);
									sprintf(InfoStr, SC(1062, "Calculating guides for net %s"), Net->Name);
									RedrawInfoStr(1);
									CheckNet(NetNr, 0);
									InsertConnections(NetNr, 0);

									for (cnt2 = 0; cnt2 < Design.NrConnections; cnt2++)
									{
										Connection = &((*Connections)[cnt2]);

										if (((Connection->Info & OBJECT_NOT_VISIBLE) == 0)
										        && (Connection->NetNr == NetNr))
										{
											Connection->AddNr = (int16) LastActionNr;

											if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
												Connection->Info |= OBJECT_HIGHLITED;
										}
									}

									Net->Info &= ~CONNECTIONS_DISABLED;
								}
							}
						}
					}
				}

				SetNormalCursor();
				break;

			case 1:			// Unselect traces/vias nets
				for (cnt = 0; cnt < TotalNetCount; cnt++)
				{
					NetNr = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

					if (NetNr != LB_ERR)
					{
						if (SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0))
						{
							if (SelectedNets[NetNr])
							{
								SelectedNets[NetNr] = 0;	// No change
							}
							else
							{
								SelectedNets[NetNr] = 1;	// Select net
							}
						}
						else
						{
							if (SelectedNets[NetNr])
							{
								SelectedNets[NetNr] = 1;	// Deselect net
							}
							else
							{
								SelectedNets[NetNr] = 0;	// No change
							}
						}
					}
				}

				for (Layer = 0; Layer < 32; Layer++)
				{
					DrawCode = DrawLayerCode[Layer];

					for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
					{
						Trace = &((*VerTraces[Layer])[cnt]);

						if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr]))
						{
							Trace->Info = (int16) (Trace->Info ^ OBJECT_SELECTED);
							DataBaseChanged = 1;
						}
					}

					for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
					{
						Trace = &((*HorTraces[Layer])[cnt]);

						if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr]))
						{
							Trace->Info = (int16) (Trace->Info ^ OBJECT_SELECTED);
							DataBaseChanged = 1;
						}
					}

					for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
					{
						Trace = &((*Diag1Traces[Layer])[cnt]);

						if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr]))
						{
							Trace->Info = (int16) (Trace->Info ^ OBJECT_SELECTED);
							DataBaseChanged = 1;
						}
					}

					for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
					{
						Trace = &((*Diag2Traces[Layer])[cnt]);

						if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr >= 0)
						        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr]))
						{
							Trace->Info = (int16) (Trace->Info ^ OBJECT_SELECTED);
							DataBaseChanged = 1;
						}
					}
				}

				for (cnt = 0; cnt < Design.NrVias; cnt++)
				{
					Via = &((*Vias)[cnt]);

					if (((Via->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Via->NetNr >= 0) && (Via->NetNr < Design.NrNets)
					        && (SelectedNets[Via->NetNr]))
					{
						Via->Info = (int16) (Via->Info ^ OBJECT_SELECTED);
						DataBaseChanged = 1;
					}
				}

				for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

					if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr >= 0)
					        && (AreaFill->NetNr < Design.NrNets) && (SelectedNets[AreaFill->NetNr]))
					{
						AreaFill->Info = (int16) (AreaFill->Info ^ OBJECT_SELECTED);
						DataBaseChanged = 1;
					}
				}

				break;

			case 2:			// Delete nets objects selected
				for (cnt = 0; cnt < TotalNetCount; cnt++)
				{
					if (SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0))
					{
						NetNr = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, cnt, 0);

						if (NetNr != LB_ERR)
						{
							DeleteNet(NetNr);
							ReCalcConnectionsNet(NetNr, 0, 1);
							DataBaseChanged = 1;
						}
					}
				}

				break;
			}

			if (LOWORD(WParam) == IDOK)
				SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);

			DeallocateSpecialMem(MEM_NET_SELECTED);
			SetWindowName(1);
//          UpdatePlotObjectsFromNets(0);
			RePaint();
			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
//          EndDialog(Dialog,1);
			return 1;

		case IDD_SELECTALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, -1);
			return 1;

		case IDD_UNSELECTALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 0, -1);
			return 1;

		case IDD_SELECTALL2:
			SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETSEL, 1, -1);
			return 1;

		case IDD_UNSELECTALL2:
			SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETSEL, 0, -1);
			return 1;

		case IDD_SELECTALL3:
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETSEL, 1, -1);
			return 1;

		case IDD_UNSELECTALL3:
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETSEL, 0, -1);
			return 1;

		case IDC_BUTTON1:
			if (SendDlgItemMessage(Dialog, IDC_EDIT1, WM_GETTEXT, 100, (LPARAM) str))
			{
				Found = -1;

				if (stricmpUTF8(SearchString1, str))
					SearchCnt1 = -1;

				strcpy(SearchString1, str);
				cnt3 = strlen(SearchString1);

				for (cnt = SearchCnt1 + 1; cnt < TotalNetCount; cnt++)
				{
					str2[0] = 0;
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str2);

					if (strnicmpUTF8(str, str2, min(cnt3, (int32) strlen(str2))) == 0)
						Found = cnt;
				}

				if (Found)
				{
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTOPINDEX, Found, 0);
					SearchCnt1 = Found;
				}
				else
				{
					if (SearchCnt1 > -1)
					{
						SearchCnt1 = -1;
						Found = -1;

						for (cnt = SearchCnt1 + 1; cnt < TotalNetCount; cnt++)
						{
							str2[0] = 0;
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str2);

							if (strnicmpUTF8(str, str2, (int32) strlen(str2)) == 0)
								Found = cnt;
						}

						if (Found)
						{
							strcpy(SearchString1, str);
							SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTOPINDEX, Found, 0);
						}
					}
				}
			}

			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			return 1;

		case IDC_BUTTON2:
			if (SendDlgItemMessage(Dialog, IDC_EDIT2, WM_GETTEXT, 100, (LPARAM) str))
			{
				Found = -1;

				if (stricmpUTF8(SearchString2, str))
					SearchCnt2 = -1;

				strcpy(SearchString2, str);
				cnt3 = strlen(SearchString2);

				for (cnt = SearchCnt2 + 1; cnt < TotalNetCount; cnt++)
				{
					str2[0] = 0;
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETTEXT, cnt, (LPARAM) str2);

					if (strnicmpUTF8(str, str2, min(cnt3, (int32) strlen(str2))) == 0)
						Found = cnt;
				}

				if (Found)
				{
					SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETTOPINDEX, Found, 0);
					SearchCnt2 = Found;
				}
				else
				{
					if (SearchCnt2 > -1)
					{
						SearchCnt2 = -1;
						Found = -1;

						for (cnt = SearchCnt2 + 1; cnt < TotalNetCount; cnt++)
						{
							str2[0] = 0;
							SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETTEXT, cnt, (LPARAM) str2);

							if (strnicmpUTF8(str, str2, (int32) strlen(str2)) == 0)
								Found = cnt;
						}

						if (Found)
						{
							strcpy(SearchString2, str);
							SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETTOPINDEX, Found, 0);
						}
					}
				}
			}

			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			return 1;

		case IDC_BUTTON3:
			if (SendDlgItemMessage(Dialog, IDC_EDIT3, WM_GETTEXT, 100, (LPARAM) str))
			{
				Found = -1;

				if (stricmpUTF8(SearchString3, str))
					SearchCnt3 = -1;

				strcpy(SearchString3, str);
				cnt3 = strlen(SearchString3);

				for (cnt = SearchCnt3 + 1; cnt < TotalNetCount; cnt++)
				{
					str2[0] = 0;
					SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETTEXT, cnt, (LPARAM) str2);

					if (strnicmpUTF8(str, str2, min(cnt3, (int32) strlen(str2))) == 0)
						Found = cnt;
				}

				if (Found)
				{
					SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTOPINDEX, Found, 0);
					SearchCnt3 = Found;
				}
				else
				{
					if (SearchCnt3 > -1)
					{
						SearchCnt3 = -1;
						Found = -1;

						for (cnt = SearchCnt3 + 1; cnt < TotalNetCount; cnt++)
						{
							str2[0] = 0;
							SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETTEXT, cnt, (LPARAM) str2);

							if (strnicmpUTF8(str, str2, (int32) strlen(str2)) == 0)
								Found = cnt;
						}

						if (Found)
						{
							strcpy(SearchString3, str);
							SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTOPINDEX, Found, 0);
						}
					}
				}
			}

			SendMessage(PcbSettingsTabInfo[PcbSettingsFocusWindowNr].ControlList, WM_SETFOCUS, 0, 0);
			return 1;

		case IDHELP:
			return 1;

		case IDCANCEL:

			DeallocateSpecialMem(MEM_NET_SELECTED);
			SendMessage(PcbSettingsMainDialog, (UINT) WM_CLOSE, 0, 0);
			return 1;
		}

		break;
	}

	return 0;
}

//**********************************************************************************************************************
//***************************************** konec **********************************************************************
//**********************************************************************************************************************