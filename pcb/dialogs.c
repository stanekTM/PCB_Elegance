/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: dialogs.c
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
#include "utf8.h"
#include "memory.h"
#include "dialogs.h"
#include "stdlib.h"
#include "stdio.h"
#include "select.h"
#include "select3.h"
#include "resource.h"
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
#include "dialogs.h"
#include "ctype.h"
#include "../functionsc/version.h"

int32 NetDialogMode, NetsSelected, DialogResult, DialogMode, CurrentFontNr, ErrorNr, TempUnits, NetSelected,
      SelectedColorNr, OldValue4, OldValue5, NrTempGerberLayers, LayerCodes[64], LayerInfo[64], PlotLayers[512],
      NrPlotLayers, NrBitmapExportResolutions = 11, *NetInfo, NrPageOutputIds =
                  11, ok, NrGeometryLibFiles, CurrentGeometryLibraryNr, CurrentGeometryNrInLibrary;

int32 PageOutputIds[11] = { PAPERSIZE_A1,
                            PAPERSIZE_A2,
                            PAPERSIZE_A3,
                            PAPERSIZE_A4,
                            PAPERSIZE_A5,
                            PAPERSIZE_B4,
                            PAPERSIZE_B5,
                            PAPERSIZE_B4_JIS,
                            PAPERSIZE_B5_JIS,
                            PAPERSIZE_LEGAL,
                            PAPERSIZE_LETTER
                          };

int32 BitmapExportResolutions[20] = { 300, 400, 508, 600, 1000, 1016, 1200, 2000, 2400, 4000, 4800 };

double TempTraceWidth, TempClearance, RotationCentreX, RotationCentreY, RotationValue, OldValue1, OldValue2, OldValue3;

char DialogTextLine[2048], GerberLayerStr[26+16][128], GeometryLibNames[64][160], SearchString2[MAX_LENGTH_STRING],
     TempGeometrieName[MAX_LENGTH_STRING], GeometrieEditor[MAX_LENGTH_STRING];

uint8 *SelectedNets, *SelectedComponents;

LPSTR InitDialogTextLine, DialogWindowText, LineInputDialogText, WorkingTextLong, MultiLineText, ViewText;

ObjectTextRecord2 *WorkingObjectText, OldWorkingObjectText;

STARTUPINFO StartupInfo;
AreaFillRecord *DialogAreaFill;

RECT DialogWindowRect;
int32 GeomViewActive, ActiveViewGeometry, CompSortMode, OldCompSortMode, NrUsedComps;

// *******************************************************************************************************
// *******************************************************************************************************

extern int32 GerberMode, SelectedColorNr, AreafillDrawMode;
extern UINT ClosingWindowMessage;
extern int32 ProjectActive;
extern char SearchForReferenceString[MAX_LENGTH_STRING];
extern CompPosOutputRecord CompPosOutput;
extern double AreafillPenSize1, AreafillPenSize2;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

INT_PTR OwnDialogBox(HINSTANCE hInstance, LPCTSTR Resource, HWND Window, DLGPROC DialogBody)
{


	return DialogBoxW(hInstance, (WCHAR *) Resource, Window, DialogBody);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

INT_PTR OwnDialogBoxParam(HINSTANCE hInstance, LPCTSTR Resource, HWND Window, DLGPROC DialogBody, LPARAM LParam)
{
	return DialogBoxParamW(hInstance, (WCHAR *) Resource, Window, DialogBody, LParam);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK NetsDialogBody2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, cnt2, res, Found;
	char str[MAX_LENGTH_STRING];
	NetRecord *Net;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(158, "Net selection dialog"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		switch (NetDialogMode)
		{
		case 0:
			sprintf(str, SC(159, "Select net [ %i nets ]"), Design.NrNets);
			SetWindowTextUTF8(Dialog, str);
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
			res = SendDlgItemMessage(Dialog, IDD_LISTBOX_NETS, LB_INITSTORAGE, cnt2, cnt2 * 60);

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if (Net->Name[0] != 0)
				res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			Found = (int32) SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_GETCURSEL, 0, 0);

			if (Found != LB_ERR)
			{
				SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_GETTEXT, Found, (LPARAM) DialogTextLine);

				for (cnt = 0; cnt < Design.NrNets; cnt++)
				{
					Net = &((*Nets)[cnt]);

					if (Net->Name[0] != 0)
					{
						if (stricmpOwn(Net->Name, DialogTextLine) == 0)
							NetSelected = cnt;
					}
				}
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("check_connectivity.htm", 0);
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

int32 SelectNetDialog()
{
	int32 res;

	NetDialogMode = 0;
	NetSelected = -1;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NETS1), PCBWindow, (DLGPROC) NetsDialogBody2);
	return NetSelected;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK NetsDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, res, cnt2, First, count, TabStops[5];
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	NetRecord *Net;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_DESELECTALL, SC(161, "Deselect all"));

		

		switch (NetDialogMode)
		{
		case 0:
			sprintf(str, SC(162, "Highlight/unhighlight net(s) [ %i nets ]"), Design.NrNets);
			SetWindowTextUTF8(Dialog, str);
			break;

		case 1:
			sprintf(str, SC(163, "Hide connections net(s) [ %i nets ]"), Design.NrNets);
			SetWindowTextUTF8(Dialog, str);
			break;

		case 2:
			sprintf(str, SC(1324, "Disable connections nets [ %i nets ]"), Design.NrNets);
			SetWindowTextUTF8(Dialog, str);
			break;

		case 3:
			sprintf(str, SC(165, "Unselect traces/vias [ %i nets ]"), Design.NrNets);
			SetWindowTextUTF8(Dialog, str);
			break;

		case 5:
			sprintf(str, SC(166, "Delete traces/vias net [ %i nets ]"), Design.NrNets);
			SetWindowTextUTF8(Dialog, str);
			break;
		}

		TabStops[0] = 700;
		TabStops[1] = 1500;
		SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, EM_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);
		count = 0;
		First = -1;

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if ((Net->Name[0] != 0) && (Net->NrPins > 1))
				cnt2++;
		}

		if (cnt2 > 100)
			res = SendDlgItemMessage(Dialog, IDD_LISTBOX_NETS, LB_INITSTORAGE, cnt2, cnt2 * 60);

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if ((Net->Name[0] != 0) && (Net->NrPins > 1))
			{
				count++;
				sprintf(str, "%s\t\t\t\t\t\t\t%i", Net->Name, cnt);
				res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_ADDSTRING, 0, (LPARAM) (str));

				switch (NetDialogMode)
				{
				case 0:
					if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
					{
						cnt2 =
						    SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_FINDSTRING, (WPARAM) - 1,
						                          (LPARAM) (LPCTSTR) & str);
						res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_SETSEL, 1, cnt2);
					}

					break;

				case 1:
					if ((Net->Info & CONNECTIONS_NOT_VISIBLE) == CONNECTIONS_NOT_VISIBLE)
					{
						cnt2 =
						    SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_FINDSTRING, (WPARAM) - 1,
						                          (LPARAM) (LPCTSTR) & str);
						res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_SETSEL, 1, cnt2);
					}

					break;

				case 2:
					if ((Net->Info & CONNECTIONS_DISABLED) == CONNECTIONS_DISABLED)
					{
						cnt2 =
						    SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_FINDSTRING, (WPARAM) - 1,
						                          (LPARAM) (LPCTSTR) & str);
						res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_SETSEL, 1, cnt2);
					}

					break;

				case 3:
					if (SelectedNets[cnt] == 1)
					{
						cnt2 =
						    SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_FINDSTRING, (WPARAM) - 1,
						                          (LPARAM) (LPCTSTR) & str);
						res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_SETSEL, 1, cnt2);
					}

					break;
				}
			}
		}

		for (cnt = 0; cnt < count; cnt++)
		{
			res = SendDlgItemMessage(Dialog, IDD_LISTBOX_NETS, LB_GETSEL, cnt, 0);

			if (res > 0)
			{
				if (First == -1)
					First = cnt;
			}
		}

		if (First != -1)
			res = SendDlgItemMessage(Dialog, IDD_LISTBOX_NETS, LB_SETTOPINDEX, First, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			NetsSelected =
			    (int32) SendDlgItemMessage(Dialog, IDD_LISTBOX_NETS, LB_GETSELITEMS, Design.NrNets,
			                               (LPARAM) ((LPINT) NetInfo));

			if (NetsSelected > 0)
			{
				for (cnt = 0; cnt < NetsSelected; cnt++)
				{
					cnt2 = NetInfo[cnt];
					SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_GETTEXT, cnt2, (LPARAM) str);
					sscanf(str, "%s\t\t\t\t\t\t\t%i", str2, &res);
//              Net=&((*Nets)[res]);
					NetInfo[cnt] = res;
//              ok=1;
				}
			}

			EndDialog(Dialog, 1);
			return about;

		case IDD_DESELECTALL:
			for (cnt = 0; cnt < Design.NrNets; cnt++)
				SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_SETSEL, 0, (LPARAM) cnt);

			break;

		case IDD_SELECTALL:
			for (cnt = 0; cnt < Design.NrNets; cnt++)
				SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_SETSEL, 1, (LPARAM) cnt);

			break;

		case IDHELP:
			switch (NetDialogMode)
			{
			case 0:
				Help("highlight_unhighlight_nets.htm", 0);
				break;

			case 1:
				Help("hide_connections_nets.htm", 0);
				break;

			case 2:
				Help("disable_connections_nets.htm", 0);
				break;

			case 3:
				Help("unselect_traces_vias_nets.htm", 0);
				break;

			case 5:
				Help("delete_traces_vias_nets.htm", 0);
				break;
			}

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK UnconnectedNetsDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res;
	NetRecord *Net;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, "");

		SetWindowTextUTF8(Dialog, (LPSTR) SC(167, "Unconnected nets"));

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if (Net->Name[0] != 0)
			{
				if (Net->Dummy == 1)
				{
					res =
					    SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));
				}
			}
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			EndDialog(Dialog, 1);
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

void UnconnectedNetsDialog()
{
	int32 res;

	NetDialogMode = 0;
	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NETS), PCBWindow,
	                 (DLGPROC) UnconnectedNetsDialogBody);
}

void HiliteNetsDialog(void)
{
}

void HideConnectionsNetsDialog(void)
{
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnselectTracesViasNetDialog()
{
	int32 res, count;

	NetDialogMode = 3;
	AllocateSpecialMem(MEM_NETINFO, (Design.NrNets + 10) * sizeof(int32), (void **) &NetInfo);
	AllocateSpecialMem(MEM_NET_SELECTED, (Design.NrNets + 10) * sizeof(uint8), (void **) &SelectedNets);
	count = SelectNetsObjectsSelected(SelectedNets, Design.NrNets);

	if (count == 0)
	{
		DeallocateSpecialMem(MEM_NETINFO);
		DeallocateSpecialMem(MEM_NET_SELECTED);
		return;
	}

	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NETS), PCBWindow, (DLGPROC) NetsDialogBody);

	if (res == 1)
		UnselectTracesViasNet(NetsSelected, NetInfo, SelectedNets);

	DeallocateSpecialMem(MEM_NETINFO);
	DeallocateSpecialMem(MEM_NET_SELECTED);
}

void DisableConnectionsNetsDialog(void)
{
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 CALLBACK TextInputDialog1(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	double hulp;
	int32 res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(181, "Text height"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(182, "Line thickness"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(183, "Rotation"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");

		switch (DialogMode & 0x0f)
		{
		case 2:
			SetWindowTextUTF8(Dialog, SC(184, "Edit component value"));
			break;

		case 3:
			SetWindowTextUTF8(Dialog, SC(185, "Scale factor"));
			break;

		case 4:
			SetWindowTextUTF8(Dialog, SC(186, "Rotation (degrees)"));
			break;

		case 5:
			SetWindowTextUTF8(Dialog, SC(187, "Change start/end angle (degrees counter clockwise)"));
			break;

		case 6:
			SetWindowTextUTF8(Dialog, SC(1204, "Goto x,y"));
			break;

		case 7:
			SetWindowTextUTF8(Dialog, SC(1298, "Rotate each at any angle CCW"));
			break;
		}

		if ((DialogMode & 0x10) == 0)
		{
			TempUnits = Units;
			SetDialogValue(Dialog, IDC_EDIT1, WorkingObjectText->FontHeight);
			SetDialogValue(Dialog, IDC_EDIT3, WorkingObjectText->LineThickNess);
			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
		}

		SendDlgItemMessageOwn(Dialog, IDD_TEXTINPUT_EDIT1, EM_REPLACESEL, 0, (LPARAM) & (WorkingObjectText->Text));
		SetDialogFloatValue(Dialog, IDC_EDIT5, WorkingObjectText->Rotation, 1);
		strcpy(OldWorkingObjectText.Text, (WorkingObjectText->Text));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			if ((DialogMode & 0x10) == 0)
			{
				WorkingObjectText->FontHeight = (float) GetDialogValue(Dialog, IDC_EDIT1);
				WorkingObjectText->LineThickNess = (float) GetDialogValue(Dialog, IDC_EDIT3);
				TempUnits ^= 1;
				SetDialogValue(Dialog, IDC_EDIT1, WorkingObjectText->FontHeight);
				SetDialogValue(Dialog, IDC_EDIT3, WorkingObjectText->LineThickNess);
				SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
				SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);

				if (TempUnits == 0)
				{
					SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
					SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				}
				else
				{
					SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
					SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				}
			}

			break;

		case IDOK:
			res =
			    SendDlgItemMessageOwn(Dialog, IDD_TEXTINPUT_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1,
			                          (LPARAM) DialogTextLine);

			if (res > 0)
			{
				memset(&(WorkingObjectText->Text), 0, sizeof(WorkingObjectText->Text));
				strncpy(WorkingObjectText->Text, DialogTextLine, sizeof(WorkingObjectText->Text) - 1);
			}

			if ((DialogMode & 0x10) == 0)
			{
				WorkingObjectText->FontHeight = (float) GetDialogValue(Dialog, IDC_EDIT1);

				if (WorkingObjectText->FontHeight < 1000.0)
				{
					MessageBoxOwn(PCBWindow, SC(188, "Wrong value for text height"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				WorkingObjectText->LineThickNess = (float) GetDialogValue(Dialog, IDC_EDIT3);

				if (WorkingObjectText->LineThickNess < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(189, "Wrong value for line thickness"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				GetDialogFloatValue(Dialog, IDC_EDIT5, &hulp);
				WorkingObjectText->Rotation = (float) hulp;
			}
			else
				EndDialog(Dialog, 1);

			return about;

		case IDCANCEL:
			memset(DialogTextLine, 0, 200);
			EndDialog(Dialog, 2);
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

int32 CALLBACK TextInputDialog4(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, cnt2, res, NrLines, result, ok;
	uint16 *Lengte;
	double hulp;
	char FontName[64], str[128];
	CHOOSEFONT ChooseRec;
	LOGFONT FontRec;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(181, "Text height"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(182, "Line thickness"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(183, "Rotation"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(229, "Add Font"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");

		switch (DialogMode)
		{
		case 0:
			SetWindowTextUTF8(Dialog, SC(190, "Add text"));
			break;

		case 2:
			SetWindowTextUTF8(Dialog, SC(190, "Add text"));
			break;

		case 1:
			SetWindowTextUTF8(Dialog, SC(191, "Change text"));
			break;

		case 3:
			SetWindowTextUTF8(Dialog, SC(191, "Change text"));
			break;
		}

		TempUnits = Units;
		SetDialogValue(Dialog, IDC_EDIT1, WorkingObjectText->FontHeight);
		SetDialogValue(Dialog, IDC_EDIT3, WorkingObjectText->LineThickNess);
		SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
		SendDlgItemMessageOwn(Dialog, IDD_TEXTINPUT_EDIT1, EM_REPLACESEL, 0, (LPARAM) & (WorkingObjectText->Text));
		SetDialogFloatValue(Dialog, IDC_EDIT5, WorkingObjectText->Rotation, 1);
		OldValue3 = WorkingObjectText->Rotation;
		strcpy(OldWorkingObjectText.Text, (WorkingObjectText->Text));
		OldValue1 = WorkingObjectText->FontHeight;
		OldValue2 = WorkingObjectText->LineThickNess;
		OldValue4 = WorkingObjectText->FontNr;
		OldValue5 = WorkingObjectText->TextMode & 0x10;

		switch (DialogMode)
		{
		case 2:
		case 3:
			if (WorkingObjectText->TextMode & 0x10)
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(1165, "Line draw (default)"));

			if (Design.UsedFontStr[0][0] == 0)
			{
				strcpy(Design.UsedFontStr[0], "Arial");
				strcpy(Design.UsedFontStr[1], "Courier New");
				strcpy(Design.UsedFontStr[2], "Times New Roman");
#ifdef _DEBUG

//            strcpy(Design.UsedFontStr[3],"Arial Unicode MS");

#endif
			}

			for (cnt = 0; cnt < 16; cnt++)
			{
				if (Design.UsedFontStr[cnt][0] != 0)
					SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) Design.UsedFontStr[cnt]);
			}

			if (DialogMode == 2)
				SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, CurrentFontNr, 0);
			else
				SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, WorkingObjectText->FontNr, 0);

			break;
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			WorkingObjectText->FontHeight = (float) GetDialogValue(Dialog, IDC_EDIT1);
			WorkingObjectText->LineThickNess = (float) GetDialogValue(Dialog, IDC_EDIT3);
			TempUnits ^= 1;
			SetDialogValue(Dialog, IDC_EDIT1, WorkingObjectText->FontHeight);
			SetDialogValue(Dialog, IDC_EDIT3, WorkingObjectText->LineThickNess);
			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);

			if (TempUnits == 0)
			{
				SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			}
			else
			{
				SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			}

			break;

		case IDC_BUTTON1:
			memset(&ChooseRec, 0, sizeof(CHOOSEFONT));
			memset(&FontRec, 0, sizeof(LOGFONT));
			ChooseRec.lStructSize = sizeof(CHOOSEFONT);
			ChooseRec.hwndOwner = PCBWindow;
			ChooseRec.Flags = CF_BOTH | CF_FORCEFONTEXIST | CF_INITTOLOGFONTSTRUCT;
			ChooseRec.nFontType = SCREEN_FONTTYPE;
			ChooseRec.iPointSize = 1;
			ChooseRec.lpLogFont = &FontRec;
			ChooseRec.lpTemplateName = "FontDlg";
			ChooseRec.hInstance = PCBClass.hInstance;
			result = ChooseFont(&ChooseRec);

			if (result)
			{
				cnt2 = -1;

				for (cnt = 0; cnt < 16; cnt++)
				{
					if (Design.UsedFontStr[cnt][0] != 0)
					{
						if (strcmp(Design.UsedFontStr[cnt], FontRec.lfFaceName) == 0)
							break;
					}
					else
						cnt2 = cnt;
				}

				if (cnt2 != -1)
				{
					SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) FontRec.lfFaceName);

					for (cnt = 0; cnt < 16; cnt++)
					{
						if (Design.UsedFontStr[cnt][0] == 0)
							break;
					}

					if (cnt < 16)
						strcpy(Design.UsedFontStr[cnt], FontRec.lfFaceName);
				}

				SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) FontRec.lfFaceName);
				ok = 1;
				/*
				            commerror=CommDlgExtendedError();
				            OldFont=UserFont;
				            UserFont=CreateFontIndirect(&FontRec);
				            InitDeviceContext();
				            GetTextMetrics(WindowsDisplay,&TextInfo);
				            strcpy(CurrentFontName,FontRec.lfFaceName);
				            DoneDeviceContext();
				            if ((TextInfo.tmPitchAndFamily & TMPF_FIXED_PITCH)==0) {
				              CharY=TextInfo.tmHeight;
				              CharX=TextInfo.tmMaxCharWidth;
				              WindowChars=(ClientRect.right-ClientRect.left) / CharX;
				              WindowLines=(ClientRect.bottom-ClientRect.top-StatusLineThickness) / CharY;
				              memmove(&UsableMuisWindow,&ClientRect,sizeof(RECT));
				              UsableMuisWindow.bottom=WindowLines*CharY;
				              ClientToScreen(CrtWindow,(POINT FAR*)&UsableMuisWindow.left);
				              ClientToScreen(CrtWindow,(POINT FAR*)&UsableMuisWindow.right);
				              WindowKillFocus();
				              InvalidateRect(CrtWindow,NULL,1);
				              WindowSetFocus();
				              CheckInputMessages();
				            } else UserFont=OldFont;
				*/
			}

			break;

		case IDOK:
			memset(&(WorkingObjectText->Text), 0, sizeof(WorkingObjectText->Text));
			NrLines = -1;

			for (cnt = 255; cnt >= 0; cnt--)
			{
				Lengte = (uint16 *) & DialogTextLine;
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 10;

				if ((res =
				            SendDlgItemMessageOwn(Dialog, IDD_TEXTINPUT_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine)) > 0)
				{
					if (NrLines == -1)
						NrLines = cnt + 1;
				}
			}

			ok = 1;

			for (cnt = 0; cnt < NrLines; cnt++)
			{
				Lengte = (uint16 *) & DialogTextLine;
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING;
				res = SendDlgItemMessageOwn(Dialog, IDD_TEXTINPUT_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;

				if (strlen(WorkingObjectText->Text) + 2 + strlen(DialogTextLine) < sizeof(WorkingObjectText->Text) - 1)
				{
					strcat(WorkingObjectText->Text, DialogTextLine);
					strcat(WorkingObjectText->Text, "\r\n");
				}
			}

			WorkingObjectText->FontHeight = (float) GetDialogValue(Dialog, IDC_EDIT1);

			if (WorkingObjectText->FontHeight < 1000.0)
			{
				MessageBoxOwn(PCBWindow, SC(188, "Wrong value for text height"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			WorkingObjectText->LineThickNess = (float) GetDialogValue(Dialog, IDC_EDIT3);

			if (WorkingObjectText->LineThickNess < 100.0)
			{
				MessageBoxOwn(PCBWindow, SC(189, "Wrong value for line thickness"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			GetDialogFloatValue(Dialog, IDC_EDIT5, &hulp);
			WorkingObjectText->Rotation = (float) hulp;
			FontName[0] = 0;
			SendDlgItemMessage(Dialog, IDC_EDIT6, WM_GETTEXT, 0, (LPARAM) FontName);
			res = 0;

			switch (DialogMode)
			{
			case 2:
			case 3:
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

				if ((res != CB_ERR) && (res != 0))
				{
					res = SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_GETLBTEXT, res, (LPARAM) str);

					for (cnt = 0; cnt < 16; cnt++)
					{
						if (Design.UsedFontStr[cnt][0] != 0)
						{
							if (strcmp(Design.UsedFontStr[cnt], str) == 0)
								break;
						}
					}

					if (cnt < 16)
						res = cnt + 1;
				}

				break;
			}

			switch (DialogMode)
			{
			case 1:
				if ((OldValue1 != WorkingObjectText->FontHeight) || (OldValue3 != WorkingObjectText->Rotation)
				        || (OldValue2 != WorkingObjectText->LineThickNess)
				        || (strcmpUTF8(OldWorkingObjectText.Text, (WorkingObjectText->Text)) != 0))
					EndDialog(Dialog, 1);
				else
					EndDialog(Dialog, 2);

				break;

			case 2:
				WorkingObjectText->FontNr = (int16) res;
				EndDialog(Dialog, 1);
				break;

			case 3:
				if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0))
					WorkingObjectText->TextMode |= 0x10;
				else
					WorkingObjectText->TextMode &= ~0x10;

				WorkingObjectText->FontNr = (int16) res;

				if ((OldValue1 != WorkingObjectText->FontHeight) || (OldValue3 != WorkingObjectText->Rotation)
				        || (OldValue2 != WorkingObjectText->LineThickNess) || (OldValue4 != WorkingObjectText->FontNr)
				        || (OldValue5 != (WorkingObjectText->TextMode & 0x10))
				        || (strcmpUTF8(OldWorkingObjectText.Text, (WorkingObjectText->Text)) != 0))
					EndDialog(Dialog, 1);
				else
					EndDialog(Dialog, 2);

				break;

			default:
				EndDialog(Dialog, 1);
				break;
			}

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

int32 TextInputDialog(ObjectTextRecord2 * ObjectText, int32 Mode)
{
	int32 res;
#if 0
	int32 count, fp;
	HANDLE DialogResourceHandle;
	HGLOBAL DialogGlobal;
	uint8 *PreDialogData;
#endif
	res = 0;
	DialogMode = Mode;
	WorkingObjectText = ObjectText;

	if ((DialogMode & 0x10) == 0)
	{
		switch (DialogMode & 0x0f)
		{
		case 0:
			res =
			    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT4), PCBWindow,
			                 (DLGPROC) TextInputDialog4);
			break;

		case 1:
			res =
			    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT4), PCBWindow,
			                 (DLGPROC) TextInputDialog4);
			break;

		case 2:
			/*
			  FontData=LockResource(FontGlobal);
			*/
#if 0
			DialogResourceHandle = FindResource(NULL, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT5), RT_DIALOG);
			count = SizeofResource(NULL, DialogResourceHandle);
			DialogGlobal = LoadResource(NULL, DialogResourceHandle);
			PreDialogData = LockResource(DialogGlobal);
			fp = FileOpenWriteUTF8("e:\\new\\pcb\\dialogbox.bin");
			FileWrite(fp, PreDialogData, count, &res);
			FileClose(fp);
#endif
			res =
			    DialogBoxW(PCBClass.hInstance, (LPWSTR) MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT5), PCBWindow,
			               (DLGPROC) TextInputDialog4);
			break;

		case 3:
		case 4:
			res =
			    DialogBoxW(PCBClass.hInstance, (LPWSTR) MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT5), PCBWindow,
			               (DLGPROC) TextInputDialog4);
			break;
		}
	}
	else
	{
		res =
		    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT2), PCBWindow,
		                 (DLGPROC) TextInputDialog1);
	}

	return res;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK TextInputDialog3(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res;
	uint16 *Lengte;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, "");

		SetWindowTextUTF8(Dialog, LineInputDialogText);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) (LPSTR) MultiLineText);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			MultiLineText[0] = 0;

			for (cnt = 0; cnt < 256; cnt++)
			{
				Lengte = (uint16 *) & DialogTextLine;
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING;

				if ((res = SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine)) > 0)
				{
					DialogTextLine[res] = 0;
					strcat(MultiLineText, DialogTextLine);
					strcat(MultiLineText, "\r\n");
				}
			}

			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			MultiLineText[0] = 0;
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 TextInputDialog2(LPSTR Text, LPSTR DialogText, int32 Mode)
{
	int32 res;

	LineInputDialogText = DialogText;
	DialogMode = Mode;
	MultiLineText = Text;
	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT3), PCBWindow, (DLGPROC) TextInputDialog3);
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK ValueDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	double value1 = 0.0, value2;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(192, "Value"));

		switch (DialogMode)
		{
		case 0:
			SetWindowTextUTF8(Dialog, SC(193, "Change trace width"));
			break;

		case 1:
			SetWindowTextUTF8(Dialog, SC(194, "Change clearance width"));
			break;

		case 2:
			SetWindowTextUTF8(Dialog, SC(195, "Change text height"));
			break;

		case 3:
			SetWindowTextUTF8(Dialog, SC(196, "Change line width"));
			break;

		case 4:
			SetWindowTextUTF8(Dialog, SC(197, "Change diameter"));
			break;

		case 5:
			SetWindowTextUTF8(Dialog, SC(198, "Change width/height"));
			break;

		case 6:
			SetWindowTextUTF8(Dialog, SC(187, "Change start/end angle (degrees counter clockwise)"));
			break;
		}

		TempUnits = Units;
		SetDialogValue(Dialog, IDC_EDIT1, NewValue.Value);

		if ((DialogMode == 5) || (DialogMode == 6))
			SetDialogValue(Dialog, IDC_EDIT3, NewValue.Value2);

		SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
		value1 = NewValue.Value;
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			NewValue.Value = GetDialogValue(Dialog, IDC_EDIT1);
			NewValue.Value2 = GetDialogValue(Dialog, IDC_EDIT3);
			TempUnits ^= 1;
			SetDialogValue(Dialog, IDC_EDIT1, NewValue.Value);

			if ((DialogMode == 5) || (DialogMode == 6))
				SetDialogValue(Dialog, IDC_EDIT3, NewValue.Value2);

			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			break;

		case IDOK:
			value1 = GetDialogValue(Dialog, IDC_EDIT1);

			if ((value1 < NewValue.MinValue) || (value1 > NewValue.MaxValue))
			{
				MessageBoxOwn(PCBWindow, SC(199, "Wrong value"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			value2 = 0.0;

			if ((DialogMode == 5) || (DialogMode == 6))
			{
				value2 = GetDialogValue(Dialog, IDC_EDIT3);

				if ((value2 < NewValue.MinValue2) || (value2 > NewValue.MaxValue2))
				{
					MessageBoxOwn(PCBWindow, SC(199, "Wrong value"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}
			}

			if (DialogMode == 1)
			{
				if (value1 != NewValue.Value)
				{
					NewValue.Value = value1;
					EndDialog(Dialog, 1);
				}
				else
					EndDialog(Dialog, 2);
			}
			else
			{
				NewValue.Value = value1;
				NewValue.Value2 = value2;
				EndDialog(Dialog, 1);
			}

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

int32 ValueDialog(int32 Mode)
{
	int32 res;

	res = 0;
	DialogMode = Mode;

	switch (DialogMode)
	{
	case 0:
	case 1:
	case 2:
	case 3:
	case 4:
	case 10:
	case 11:
	case 20:
	case 21:
		res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_VALUE), PCBWindow, (DLGPROC) ValueDialog2);
		break;

	case 5:
	case 6:
		res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_VALUE2), PCBWindow, (DLGPROC) ValueDialog2);
		break;
	}

	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK RotationDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res;
	double Value1 = 0.0, Value2;
	char str[MAX_LENGTH_STRING];
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(183, "Rotation"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(200, "Rotation angle (degrees counter clockwise)"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(201, "Rotate around selection centre (On grid)"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(202, "Rotate around selection centre"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(203, "Rotate around user centre "));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(204, "Rotate around coordinate centre"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");

		SetDialogValue(Dialog, IDC_EDIT3, RotationCentreX);
		SetDialogValue(Dialog, IDC_EDIT4, RotationCentreY);

		sprintf(str, "%.2f", RotationValue);

		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str);
		SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, BST_CHECKED, 0);
		SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, BST_UNCHECKED, 0);
		SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, BST_UNCHECKED, 0);
		SendDlgItemMessageOwn(Dialog, IDC_RADIO4, BM_SETCHECK, BST_UNCHECKED, 0);

		TempUnits = Units;

		SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);

		SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT3, EM_SETREADONLY, 1, 0);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT4, EM_SETREADONLY, 1, 0);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_RADIO1:
		case IDC_RADIO2:
		case IDC_RADIO3:
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT3, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT4, EM_SETREADONLY, 1, 0);
			break;

		case IDC_RADIO4:
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT3, EM_SETREADONLY, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT4, EM_SETREADONLY, 0, 0);
			break;

		case IDD_UNITS:
			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0)) == 1)
			{
				Value1 = GetDialogValue(Dialog, IDC_EDIT3);
				Value2 = GetDialogValue(Dialog, IDC_EDIT4);
				TempUnits ^= 1;
				SetDialogValue(Dialog, IDC_EDIT3, Value1);
				SetDialogValue(Dialog, IDC_EDIT4, Value2);
				SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			}

			break;

		case IDOK:
			res = SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine);

			if (res > 0)
			{
				RotationValue = atof(DialogTextLine);

				if (RotationValue == 0.0)
					MessageBoxOwn(PCBWindow, SC(205, "Rotation is zero"), SC(118, "Warning"), MB_APPLMODAL | MB_OK);

				if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0)) == 1)
					EndDialog(Dialog, 1);

				if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0)) == 1)
					EndDialog(Dialog, 2);

				if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0)) == 1)
				{
					RotationCentreX = GetDialogValue(Dialog, IDC_EDIT3);
					RotationCentreY = GetDialogValue(Dialog, IDC_EDIT4);
					EndDialog(Dialog, 3);
				}
			}
			else
				MessageBoxOwn(PCBWindow, SC(206, "Wrong rotation value"), SC(24, "Error"), MB_APPLMODAL | MB_OK);

			return about;

		case IDCANCEL:
			memset(DialogTextLine, 0, 200);
			EndDialog(Dialog, 10);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 RotationDialog(double *RotationX, double *RotationY, double *Rotation, int32 Mode)
{
	int32 res;

	DialogMode = Mode;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ROTATION), PCBWindow, (DLGPROC) RotationDialog2);
	*RotationX = RotationCentreX;
	*RotationY = RotationCentreY;
	*Rotation = RotationValue;
	return res;
}

//***************************************************************************************************************************
//********************************* informace vybraných objektù - IDD_DIALOG_MESSAGE ****************************************
//***************************************************************************************************************************

int32 CALLBACK MessageDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, TabStops[10], BaseUnits, cnt, cnt2, cnt3, TabPos[10], MaxCharWidth, MaxStringLength[10],
	      MaxStringDialogUnits[10], MaxStringPixels[10], MaxTabPos[10], NrTabs;
	char str[MAX_LENGTH_STRING];
	LPSTR BufP, LineP;
	HFONT FontHandle;
	HDC DesktopHdc;
	TEXTMETRIC FontInfo;

	about = 1;

	/*
	#ifdef _DEBUG

	  if (Message==0x47) {
	    sprintf(str,"MessageDialog: Message 0x%04x, WPARAM = %d,%d LPARAM = %d,%d\n",
	            Message,LOWORD(WParam),HIWORD(WParam),
	            LOWORD(LParam),HIWORD(LParam));
	    OutputDebugStr(str);
	  }
	#endif
	*/

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(137, "Information on selected objects"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(1092, "Copy text to clipboard"));

		BaseUnits = GetDialogBaseUnits() & 0x0fff;

		/*
		pixelX = MulDiv(templateunitX, baseunitX, 4);
		pixelY = MulDiv(templateunitY, baseunitY, 8);

		templateunitX = MulDiv(pixelX, 4, baseunitX);
		templateunitY = MulDiv(pixelY, 8, baseunitY);
		*/

		FontHandle = (HFONT) SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT, WM_GETFONT, 0, 0);
		DesktopHdc = GetDC(NULL);
		SelectObject(DesktopHdc, FontHandle);
		GetTextMetrics(DesktopHdc, &FontInfo);
		MaxCharWidth = FontInfo.tmMaxCharWidth;
		ReleaseDC(NULL, DesktopHdc);

//      SendDlgItemMessageOwn(Dialog,IDD_MESSAGE_EDIT,WM_SETTEXT,0,(LPARAM)DialogTextLine);

		switch (DialogMode)
		{
		case 0:
			break;

		case 3:				// components
			cnt = 0;

			/*
			Comp        Layer           Geometry              Partnr    Value    OriginX  OriginY   Rotation
			*/

			memset(MaxTabPos, 0, sizeof(MaxTabPos));
			memset(TabStops, 0, sizeof(TabStops));
			memset(MaxStringLength, 0, sizeof(MaxStringLength));
			memset(MaxStringPixels, 0, sizeof(MaxStringPixels));
			memset(MaxStringDialogUnits, 0, sizeof(MaxStringDialogUnits));
			BufP = &MessageBuf[0];

			while (cnt < LParam)
			{
				LineP = BufP;
				cnt2 = 0;
				NrTabs = 0;

				while (LineP[cnt2] != '\r')
				{
					if (LineP[cnt2] == '\t')
					{
						if (NrTabs < 10)
							TabPos[NrTabs++] = cnt2;
					}

					cnt2++;
				}

				if (NrTabs < 10)
					TabPos[NrTabs++] = cnt2;

				if (NrTabs < 10)
					TabPos[NrTabs++] = 0;

				if (LineP[cnt2] == '\r')
					cnt2++;

				if (LineP[cnt2] == '\n')
					cnt2++;

				BufP += cnt2;
				MaxStringLength[0] = max(MaxStringLength[0], TabPos[0]);

				for (cnt3 = 1; cnt3 < 8; cnt3++)
				{
					if (TabPos[cnt3] > 0)
						MaxStringLength[cnt3] = max(MaxStringLength[cnt3], TabPos[cnt3] - TabPos[cnt3 - 1] - 1);
					else
						break;
				}

				cnt++;
			}

			ok = 1;

			for (cnt = 0; cnt < 7; cnt++)
			{
				MaxStringPixels[cnt] = (MaxStringLength[cnt] * (MaxCharWidth)) + 12;
//              MaxStringDialogUnits[cnt]=MulDiv(MaxStringPixels[cnt], BaseUnits, 4);
                MaxStringDialogUnits[cnt] = MulDiv(MaxStringPixels[cnt], 4, BaseUnits);

				if (cnt > 0)
					TabStops[cnt] = TabStops[cnt - 1] + MaxStringDialogUnits[cnt];
				else
					TabStops[cnt] = MaxStringDialogUnits[cnt];
			}

			/*
			          TabStops[0]=40;
			          TabStops[1]=70;
			          TabStops[2]=170;
			          TabStops[3]=270;
			          TabStops[4]=380;
			          TabStops[5]=420;
			          TabStops[6]=480;
			*/

			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT, EM_SETTABSTOPS, 7, (LPARAM) (LPINT) & TabStops);
			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT2, EM_SETTABSTOPS, 7, (LPARAM) (LPINT) & TabStops);
			sprintf(str, SC(1290, "%d objects selected"), LParam);
			SetDialogItemTextUTF8(Dialog, IDC_EDIT1, str);
			break;

		case 6:				// objects
			TabStops[0] = (75 * 13) / 20;
			TabStops[1] = (230 * 13) / 20;
			TabStops[2] = (360 * 13) / 20;
			TabStops[3] = (480 * 13) / 20;
			TabStops[4] = (570 * 13) / 20;
			TabStops[5] = (590 * 13) / 20;
			TabStops[6] = (720 * 13) / 20;
//          TabStops[4]=370;
//          TabStops[5]=430;
			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT, EM_SETTABSTOPS, 7, (LPARAM) (LPINT) & TabStops);
			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT2, EM_SETTABSTOPS, 7, (LPARAM) (LPINT) & TabStops);
			sprintf(str, SC(1290, "%d objects selected"), LParam);
			SetDialogItemTextUTF8(Dialog, IDC_EDIT1, str);
			break;

		case 7:				// Areafills
			TabStops[0] = (140 * 13) / 20;
			TabStops[1] = (200 * 13) / 20;
			TabStops[2] = (300 * 13) / 20;
			TabStops[3] = (440 * 13) / 20;
			TabStops[4] = (520 * 13) / 20;
			TabStops[5] = (620 * 13) / 20;
//          TabStops[4]=370;
//          TabStops[5]=430;
			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT, EM_SETTABSTOPS, 6, (LPARAM) (LPINT) & TabStops);
			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT2, EM_SETTABSTOPS, 6, (LPARAM) (LPINT) & TabStops);
			sprintf(str, SC(1290, "%d objects selected"), LParam);
			SetDialogItemTextUTF8(Dialog, IDC_EDIT1, str);
			break;

		case 8:				// Traces/vias
			TabStops[0] = (40 * 13) / 20;
			TabStops[1] = (90 * 13) / 20;
			TabStops[2] = (190 * 13) / 20;
			TabStops[3] = (300 * 13) / 20;
			TabStops[4] = (410 * 13) / 20;
			TabStops[5] = (480 * 13) / 20;
			TabStops[6] = (540 * 13) / 20;
			TabStops[7] = (570 * 13) / 20;
			TabStops[8] = (620 * 13) / 20;
			TabStops[9] = (630 * 13) / 20;
			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT, EM_SETTABSTOPS, 10, (LPARAM) (LPINT) & TabStops);
			SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT2, EM_SETTABSTOPS, 10, (LPARAM) (LPINT) & TabStops);
			sprintf(str, SC(1290, "%d objects selected"), LParam);
			SetDialogItemTextUTF8(Dialog, IDC_EDIT1, str);
			break;
		}

		SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT2, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		SendDlgItemBigMessageOwn(Dialog, IDD_MESSAGE_EDIT, WM_SETTEXT, 0, (LPARAM) (LPSTR) MessageBuf);
		SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT, EM_SETSEL, (WPARAM) - 1, 0);
		return about;

	case WM_MOVE:
//      SendDlgItemMessage(Dialog,IDD_MESSAGE_EDIT,EM_SETSEL,(WPARAM)-1,0);
		break;

	case WM_WINDOWPOSCHANGED:
		SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT, EM_SETSEL, (WPARAM) - 1, 0);
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDC_BUTTON1:
			CopyStrToClipboardOwn(PCBWindow, MessageBuf);
			break;
		}

		break;
	}

	about = 0;
	return about;
}


int32 MessageDialog(LPSTR InfoLine, int32 Mode, int32 ObjectsSelected)
{
	int32 res, ok;

	DialogMode = Mode;
	strcpy(DialogTextLine, InfoLine);
	res =
	    OwnDialogBoxParam(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_MESSAGE), PCBWindow, (DLGPROC) MessageDialog2,
	                      ObjectsSelected);
	ok = 1;
	return res;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK LineInputDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetWindowTextUTF8(Dialog, LineInputDialogText);

		TempUnits = Units;
		SendDlgItemMessageOwn(Dialog, IDD_LINEINPUT_EDIT1, WM_SETTEXT, 0, (LPARAM) & (WorkingObjectText->Text));

		switch (DialogMode)
		{
		case 0:
			SendDlgItemUnits(Dialog, IDD_LINEINPUT_EDIT2, TempUnits);
			break;

		case 1:
			SendDlgItemMessageOwn(Dialog, IDD_LINEINPUT_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) SC(208, "Degrees"));
			break;
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
//          res=(int32)SendDlgItemMessageOwn(Dialog,IDD_LISTBOX_NETS2,LB_GETCURSEL,0,0);
//          if (res!=LB_ERR) UnselectTracesViasNet(res);
//  Static_SetText sscanf
			memset(&(WorkingObjectText->Text), 0, sizeof(WorkingObjectText->Text));

			if ((res =
			            SendDlgItemMessageOwn(Dialog, IDD_LINEINPUT_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1,
			                                  (LPARAM) DialogTextLine)) > 0)
			{
				strncpy(WorkingObjectText->Text, DialogTextLine, sizeof(WorkingObjectText->Text) - 1);
				EndDialog(Dialog, 1);
			}
			else
				EndDialog(Dialog, 2);

			return about;

		case IDHELP:
			switch (DialogMode)
			{
			case 0:
				break;

			case 9:
//              WinHelp(GEOMWindow,"c:\\geom\\geom.hlp",HELP_CONTEXTPOPUP,"MOVE_ORIGIN);
				break;
			}

			break;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 LineInputDialog(ObjectTextRecord2 * ObjectText, LPSTR DialogText, int32 mode)
{
	int32 res, ok;

	LineInputDialogText = DialogText;
	WorkingObjectText = ObjectText;
	DialogMode = mode;
	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LINEINPUT), PCBWindow, (DLGPROC) LineInputDialog2);
	ok = 1;
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK CompRefDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetWindowTextUTF8(Dialog, LineInputDialogText);

		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & (WorkingObjectText->Text));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			memset(&(WorkingObjectText->Text), 0, sizeof(WorkingObjectText->Text));

			if ((res =
			            SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1,
			                                  (LPARAM) DialogTextLine)) > 0)
			{
				strncpy(WorkingObjectText->Text, DialogTextLine, sizeof(WorkingObjectText->Text) - 1);
				EndDialog(Dialog, 1);
			}
			else
				EndDialog(Dialog, 2);

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

int32 CompRefDialog(ObjectTextRecord2 * ObjectText, LPSTR DialogText)
{
	int32 res, ok;

	LineInputDialogText = DialogText;
	WorkingObjectText = ObjectText;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COMPREF), PCBWindow, (DLGPROC) CompRefDialog2);
	ok = 1;
	return res;
}

int32 SimpleLineInputDialog(ObjectTextRecord2 * ObjectText, LPSTR DialogText)
{
	int32 res, ok;

	LineInputDialogText = DialogText;
	WorkingObjectText = ObjectText;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COMPREF), PCBWindow, (DLGPROC) CompRefDialog2);
	ok = 1;
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK LineInputDialogLong2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetWindowTextUTF8(Dialog, LineInputDialogText);

		SendDlgItemBigMessageOwn(Dialog, IDD_LINEINPUT_EDIT1, WM_SETTEXT, 0, (LPARAM) WorkingTextLong);
		SendDlgItemUnits(Dialog, IDD_LINEINPUT_EDIT2, Units);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			*WorkingTextLong = 0;
			res = SendDlgItemBigMessageOwn(Dialog, IDD_LINEINPUT_EDIT1, WM_GETTEXT, 2047, (LPARAM) DialogTextLine);

			if (res > 0)
			{
				memmove(WorkingTextLong, &DialogTextLine, res + 1);
				EndDialog(Dialog, 1);
			}
			else
				EndDialog(Dialog, 2);

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

int32 LineInputDialogLong(LPSTR Text, LPSTR DialogText)
{
	int32 res, ok;

	LineInputDialogText = DialogText;
	WorkingTextLong = Text;
	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LINEINPUT), PCBWindow,
	                 (DLGPROC) LineInputDialogLong2);
	ok = 1;
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK AreaFillDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, Found;
	double value1, value2, value3, value4;
	NetRecord *Net;

	value1 = 0.0;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(218, "Areafills"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(209, "Net name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(210, "Thermal relief"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(211, "Clearance"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(212, "Areafill inside powerplane"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(213, "Clearance areafill inside powerplane"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(214, "Powerplane size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(215, "Powerplane"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, "PCB");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(216, "Thickness"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(217, "Add thermal reliefs"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(1158, "Add thermal reliefs to vias"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(41, "Vias"));

		TempUnits = Units;

		if (DialogMode == 0)
			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);

		if (DialogMode == 2)
			SendDlgItemUnits(Dialog, IDC_EDIT13, TempUnits);

		SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT5, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if (Net->Name[0] != 0)
				res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_ADDSTRING, 0, (LPARAM) ((LPSTR) Net->Name));
		}

		if (DialogMode == 0)
			SetDialogValue(Dialog, IDC_EDIT1, DialogAreaFill->SurroundThickNess);

		SetDialogValue(Dialog, IDC_EDIT3, DialogAreaFill->Clearance);
		SetDialogValue(Dialog, IDC_EDIT7, DialogAreaFill->ThermalReliefThickness);
		SetDialogValue(Dialog, IDC_EDIT8, DialogAreaFill->ThermalReliefDistance);

		if (DialogMode == 2)
			SetDialogValue(Dialog, IDC_EDIT9, DialogAreaFill->SurroundThickNess);

		if (DialogAreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		if (!(DialogAreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF))
			SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			Found = (int32) SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_GETCURSEL, 0, 0);

			if (Found != LB_ERR)
			{
				res = SendDlgItemMessageOwn(Dialog, IDD_LISTBOX_NETS, LB_GETTEXT, Found, (LPARAM) DialogTextLine);

				if (res > 0)
				{
					for (cnt = 0; cnt < Design.NrNets; cnt++)
					{
						Net = &((*Nets)[cnt]);

						if (Net->Name[0] != 0)
						{
							if (stricmpOwn(Net->Name, DialogTextLine) == 0)
								NetSelected = cnt;
						}
					}
				}
			}

			if (DialogMode == 0)
			{
				if ((value1 = GetDialogValue(Dialog, IDC_EDIT1)) < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(219, "Wrong value for clearance width in powerplane"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				DialogAreaFill->SurroundThickNess = (float) value1;
			}

			if ((value2 = GetDialogValue(Dialog, IDC_EDIT3)) < 100.0)
			{
				MessageBoxOwn(PCBWindow, SC(220, "Wrong value for clearance width"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			DialogAreaFill->Clearance = (float) value2;

			if (DialogMode == 2)
			{
				if ((value2 = GetDialogValue(Dialog, IDC_EDIT9)) < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(221, "Wrong value for surround thickness"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				DialogAreaFill->SurroundThickNess = (float) value2;
			}

			DialogAreaFill->Info &= ~AREAFILL_WITH_THERMAL_RELIEF;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0))
			{
				DialogAreaFill->Info |= AREAFILL_WITH_THERMAL_RELIEF;

				if ((value1 = GetDialogValue(Dialog, IDC_EDIT7)) < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(222, "Wrong value for thermal relief"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				if ((value2 = GetDialogValue(Dialog, IDC_EDIT8)) < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(222, "Wrong value for thermal relief"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				DialogAreaFill->ThermalReliefThickness = (float) value1;
				DialogAreaFill->ThermalReliefDistance = (float) value2;
			}

			DialogAreaFill->Info |= AREAFILL_WITH_NO_VIA_THERMAL_RELIEF;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0))
				DialogAreaFill->Info &= ~AREAFILL_WITH_NO_VIA_THERMAL_RELIEF;

			if (NetSelected != -1)
				EndDialog(Dialog, 1);
			else
				EndDialog(Dialog, 2);

			return about;

		case IDD_UNITS:
			if (DialogMode == 0)
			{
				if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0))
				{
					value1 = GetDialogValue(Dialog, IDC_EDIT1);
					DialogAreaFill->SurroundThickNess = (float) value1;
				}
			}

			if (DialogMode == 2)
				value1 = GetDialogValue(Dialog, IDC_EDIT9);

			value2 = GetDialogValue(Dialog, IDC_EDIT3);
			value3 = GetDialogValue(Dialog, IDC_EDIT7);
			value4 = GetDialogValue(Dialog, IDC_EDIT8);
			DialogAreaFill->Clearance = (float) value2;
			TempUnits ^= 1;

			if (DialogMode == 0)
				SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);

			if (DialogMode == 2)
				SendDlgItemUnits(Dialog, IDC_EDIT13, TempUnits);

			SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT5, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);

			if (DialogMode == 0)
			{
				if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0))
					SetDialogValue(Dialog, IDC_EDIT1, value1);
			}

			SetDialogValue(Dialog, IDC_EDIT3, value2);
			SetDialogValue(Dialog, IDC_EDIT7, value3);
			SetDialogValue(Dialog, IDC_EDIT8, value4);

			if (DialogMode == 2)
				SetDialogValue(Dialog, IDC_EDIT9, value1);

			break;

		case IDHELP:
			Help("areafills.htm", 0);
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

int32 AreaFillDialog(AreaFillRecord * AreaFill, int32 mode)
{
	int32 res = 0;

	NetRecord *Net;

	DialogAreaFill = AreaFill;
	NetSelected = -1;
	DialogMode = mode;

	switch (DialogMode)
	{
	case 0:
		res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_AREAFILL_INPOWER35), PCBWindow,
		                 (DLGPROC) AreaFillDialogBody);
		break;

	case 1:
		res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_AREAFILL35), PCBWindow,
		                 (DLGPROC) AreaFillDialogBody);
		break;

	case 2:
		res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_AREAFILL_POWER35), PCBWindow,
		                 (DLGPROC) AreaFillDialogBody);
		break;
	}

	if (res != 1)
		return -1;

	Net = &((*Nets)[NetSelected]);

	AreaFill->NetNr = (int16) NetSelected;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK AreaFillDialogBody2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	double value1, value2, value3, value4;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(1089, "Change areafill"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(1090, "Thermal relief"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(1091, "Add thermal reliefs"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(1158, "Add thermal reliefs to vias"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(41, "Vias"));

		TempUnits = Units;
		SendDlgItemUnits(Dialog, IDC_EDIT5, TempUnits);
		SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);

//      SetDialogValue(Dialog,IDC_EDIT3,DialogAreaFill->Clearance);

		SetDialogValue(Dialog, IDC_EDIT7, DialogAreaFill->ThermalReliefThickness);
		SetDialogValue(Dialog, IDC_EDIT8, DialogAreaFill->ThermalReliefDistance);

		if (DialogAreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		if (!(DialogAreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF))
			SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0))
			{
				DialogAreaFill->Info |= AREAFILL_WITH_THERMAL_RELIEF;

				if ((value1 = GetDialogValue(Dialog, IDC_EDIT7)) < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(223, "Wrong value for width"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				if ((value2 = GetDialogValue(Dialog, IDC_EDIT8)) < 100.0)
				{
					MessageBoxOwn(PCBWindow, SC(223, "Wrong value for width"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				DialogAreaFill->ThermalReliefThickness = (float) value1;
				DialogAreaFill->ThermalReliefDistance = (float) value2;
			}
			else
				DialogAreaFill->Info &= ~AREAFILL_WITH_THERMAL_RELIEF;

			DialogAreaFill->Info |= AREAFILL_WITH_NO_VIA_THERMAL_RELIEF;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0))
				DialogAreaFill->Info &= ~AREAFILL_WITH_NO_VIA_THERMAL_RELIEF;

			EndDialog(Dialog, 1);
			return about;

		case IDD_UNITS:
//          value2=GetDialogValue(Dialog,IDC_EDIT3);
			value3 = GetDialogValue(Dialog, IDC_EDIT7);
			value4 = GetDialogValue(Dialog, IDC_EDIT8);
//          DialogAreaFill->Clearance=value2;
			TempUnits ^= 1;
			SendDlgItemUnits(Dialog, IDC_EDIT5, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
//          SetDialogValue(Dialog,IDC_EDIT3,value2);
			SetDialogValue(Dialog, IDC_EDIT7, value3);
			SetDialogValue(Dialog, IDC_EDIT8, value4);
//          EndDialog(Dialog,1);
			return about;

		case IDHELP:
			Help("areafills.htm", 0);
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

int32 ChangeAreaFillDialog(AreaFillRecord * AreaFill)
{
	int32 res;

	DialogAreaFill = AreaFill;
	NetSelected = -1;
	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_AREAFILL_EDIT35), PCBWindow,
	                 (DLGPROC) AreaFillDialogBody2);
	return res;
}

//***********************************************************************************************
//************************** nové rozložení *****************************************************
//***********************************************************************************************

int32 CALLBACK NewDesignDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	double value, value1, value2, value3, value4, value5;
	int32 cnt, res, Found;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(130, "New layout")); //název dialogu okna
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(224, "PCB size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(225, "Width"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(226, "Height"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(227, "Origin"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(16, "Board outline keep out"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(38, "Design rules"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(170, "Trace width"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(211, "Clearance"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(230, "Silkscreen width"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(231, "Nr Layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(232, "Define vias"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(233, "Pad size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(234, "Drill diameter"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC14, SC(211, "Clearance"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC15, SC(235, "Anti power pad"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC16, SC(236, "Solder mask"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC17, SC(237, "Board outline width"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(238, "Top layer"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(239, "Bottom layer"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(240, "Top/bottom layer"));

		TempUnits = Units;
		
		SetDialogValue(Dialog, IDD_NEW_EDIT1, NewDesign.BoardOriginX);
		SetDialogValue(Dialog, IDD_NEW_EDIT2, NewDesign.BoardOriginY);
		SetDialogValue(Dialog, IDD_NEW_EDIT3, NewDesign.BoardHeight);
		SetDialogValue(Dialog, IDD_NEW_EDIT4, NewDesign.BoardWidth);
		SetDialogValue(Dialog, IDD_NEW_EDIT5, NewDesign.StandardTraceWidth);
		SetDialogValue(Dialog, IDD_NEW_EDIT6, NewDesign.StandardClearance);
		SetDialogValue(Dialog, IDD_NEW_EDIT7, NewDesign.SilkScreenWidth);
		SetDialogValue(Dialog, IDD_NEW_EDIT8, NewDesign.BoardOutlineKeepOut);
		SetDialogValue(Dialog, IDD_NEW_EDIT9, NewDesign.BoardOutlineWidth);
		SetDialogValue(Dialog, IDC_EDIT1, CurrentVia.ThickNess);
		SetDialogValue(Dialog, IDC_EDIT3, CurrentVia.DrillThickNess);
		SetDialogValue(Dialog, IDC_EDIT5, CurrentVia.Clearance);
		SetDialogValue(Dialog, IDC_EDIT7, CurrentVia.ThermalInner);
		SetDialogValue(Dialog, IDC_EDIT9, CurrentVia.SoldMask);
		res = SendDlgItemMessageOwn(Dialog, IDD_NEW_COMBO1, CB_RESETCONTENT, 0, 0);

		for (cnt = 1; cnt <= 16; cnt++)
		{
			sprintf(str, "%i", cnt);
			res = SendDlgItemMessageOwn(Dialog, IDD_NEW_COMBO1, CB_ADDSTRING, 0, (LPARAM) (str));
		}

		res = SendDlgItemMessageOwn(Dialog, IDD_NEW_COMBO1, CB_SELECTSTRING, 0, (LPARAM) (LPSTR) "2");
		SendDlgItemUnits(Dialog, IDD_NEW_UN1, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN2, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN3, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN4, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN5, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN6, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN7, TempUnits);
		SendDlgItemUnits(Dialog, IDD_NEW_UN8, TempUnits);
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

		case VIA_SOLDMASK_TOP:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
			break;

		case VIA_SOLDMASK_BOTTOM:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
			break;
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			NewDesign.BoardOriginX = (float) GetDialogValue(Dialog, IDD_NEW_EDIT1);
			NewDesign.BoardOriginY = (float) GetDialogValue(Dialog, IDD_NEW_EDIT2);
			value = GetDialogValue(Dialog, IDD_NEW_EDIT3);
			NewDesign.BoardHeight = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT4);
			NewDesign.BoardWidth = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT5);
			NewDesign.StandardTraceWidth = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT6);
			NewDesign.StandardClearance = (float) value;
			NewDesign.MaximumClearance = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT7);
			NewDesign.SilkScreenWidth = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT8);
			NewDesign.BoardOutlineKeepOut = (float) value;
			value = GetDialogValue(Dialog, IDD_NEW_EDIT9);
			NewDesign.BoardOutlineWidth = (float) value;

			value1 = GetDialogValue(Dialog, IDC_EDIT1);
			value2 = GetDialogValue(Dialog, IDC_EDIT3);
			value3 = GetDialogValue(Dialog, IDC_EDIT5);
			value4 = GetDialogValue(Dialog, IDC_EDIT7);
			value5 = GetDialogValue(Dialog, IDC_EDIT9);
			CurrentVia.ThickNess = (float) value1;
			CurrentVia.DrillThickNess = (float) value2;
			CurrentVia.Clearance = (float) value3;
			CurrentVia.ThermalInner = (float) value4;
			CurrentVia.SoldMask = (float) value5;

			TempUnits ^= 1;
			SetDialogValue(Dialog, IDD_NEW_EDIT1, NewDesign.BoardOriginX);
			SetDialogValue(Dialog, IDD_NEW_EDIT2, NewDesign.BoardOriginY);
			SetDialogValue(Dialog, IDD_NEW_EDIT3, NewDesign.BoardHeight);
			SetDialogValue(Dialog, IDD_NEW_EDIT4, NewDesign.BoardWidth);
			SetDialogValue(Dialog, IDD_NEW_EDIT5, NewDesign.StandardTraceWidth);
			SetDialogValue(Dialog, IDD_NEW_EDIT6, NewDesign.StandardClearance);
			SetDialogValue(Dialog, IDD_NEW_EDIT7, NewDesign.SilkScreenWidth);
			SetDialogValue(Dialog, IDD_NEW_EDIT8, NewDesign.BoardOutlineKeepOut);
			SetDialogValue(Dialog, IDD_NEW_EDIT9, NewDesign.BoardOutlineWidth);

			SetDialogValue(Dialog, IDC_EDIT1, CurrentVia.ThickNess);
			SetDialogValue(Dialog, IDC_EDIT3, CurrentVia.DrillThickNess);
			SetDialogValue(Dialog, IDC_EDIT5, CurrentVia.Clearance);
			SetDialogValue(Dialog, IDC_EDIT7, CurrentVia.ThermalInner);
			SetDialogValue(Dialog, IDC_EDIT9, CurrentVia.SoldMask);
			SendDlgItemUnits(Dialog, IDD_NEW_UN1, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN2, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN3, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN4, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN5, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN6, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN7, TempUnits);
			SendDlgItemUnits(Dialog, IDD_NEW_UN8, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT2, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT4, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT6, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT8, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);
			SendDlgItemUnits(Dialog, IDC_EDIT10, TempUnits);
			break;

		case IDOK:
			Found = (int32) SendDlgItemMessageOwn(Dialog, IDD_NEW_COMBO1, CB_GETCURSEL, 0, 0);

			if (Found != LB_ERR)
			{
				SendDlgItemMessageOwn(Dialog, IDD_NEW_COMBO1, CB_GETLBTEXT, Found, (LPARAM) DialogTextLine);
				res = sscanf(DialogTextLine, "%i", &NewDesign.NrBoardLayers);
			}

			NewDesign.BoardOriginX = (float) GetDialogValue(Dialog, IDD_NEW_EDIT1);
			NewDesign.BoardOriginY = (float) GetDialogValue(Dialog, IDD_NEW_EDIT2);

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT3)) < 10000.0)
			{
				MessageBoxOwn(PCBWindow, SC(242, "Wrong value for height"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.BoardHeight = (float) value;

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT4)) < 10000.0)
			{
				MessageBoxOwn(PCBWindow, SC(223, "Wrong value for width"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.BoardWidth = (float) value;

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

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT8)) < 0.0)
			{
				MessageBoxOwn(PCBWindow, SC(244, "Wrong value for board outline keep out"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.BoardOutlineKeepOut = (float) value;

			if ((value = GetDialogValue(Dialog, IDD_NEW_EDIT9)) < 0.0)
			{
				MessageBoxOwn(PCBWindow, SC(245, "Wrong value for board outline width"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			NewDesign.BoardOutlineWidth = (float) value;


// wsprintf
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

			NewDesign.DefVia1.ThickNess = (float) value1;
			NewDesign.DefVia1.DrillThickNess = (float) value2;
			NewDesign.DefVia1.Clearance = (float) value3;
			NewDesign.DefVia1.ThermalInner = (float) value4;
			NewDesign.DefVia1.SoldMask = (float) value5;
			NewDesign.DefVia1.ViaType = CurrentVia.ViaType;
			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("make_new_layout.htm", 0);
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

int32 NewDesignDialog()
{
	int32 res;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NEW), PCBWindow, (DLGPROC) NewDesignDialogBody);

	if (res != 1)
		return -1;

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LPSTR GetLayerString(int32 Layer)
{
	int32 cnt;

	for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
	{
		if (LayerCodes[cnt] == Layer)
			return GerberLayerStr[cnt];
	}

	return NULL;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SetPlotLayer(int32 Layer, int32 Set)
{
	int32 Layer2;

	/*
	#define SOLD_MASK_BOTTOM                        100
	#define SOLD_MASK_TOP                           101
	#define PASTE_MASK_BOTTOM                       200
	#define PASTE_MASK_TOP                          201
	#define INFO_LAYER                              1500
	#define BOARD_OUTLINE_LAYER                     1600
	#define SILKSCREEN_TOP_REFS                     2101
	#define SILKSCREEN_TOP_VALUES                   2201
	#define SILKSCREEN_BOTTOM_REFS                  2100
	#define SILKSCREEN_BOTTOM_VALUES                2200
	#define COMP_OUTLINE_LAYER                      4000
	#define DRILL_LAYER                             6000
	#define SPECIALS_LAYER                          7000
	#define INFO_LAYER2                             7500
	#define INFO_LAYER3                             8000
	#define INFO_LAYER4                             8500
	*/

	if (Layer >= 32)
		Layer2 = (Layer / 50) + (Layer & 1) + 100;
	else
		Layer2 = Layer;

	if (Set == 1)
	{	// Add this layer to the plottable layers
		PlotLayers[Layer2] = 1;
	}
	else
	{	// Remove this layer from the plottable layers
		PlotLayers[Layer2] = 0;
	}

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 IsLayerPlottable(int32 Layer)
{
	int32 Layer2;

	if (Layer >= 32)
		Layer2 = (Layer / 50) + (Layer & 1) + 100;
	else
		Layer2 = Layer;

	return PlotLayers[Layer2];
}

//*******************************************************************************************************************
//******************************* export gerberu/vrtání, penplot, tisk, bitmap, pdf *********************************
//*******************************************************************************************************************

void InitPlotLayers(int32 ReverseLayerNumbering, int32 mode)
{
	int32 cnt, count;
	char str[MAX_LENGTH_STRING];
	LPSTR LayerString;
	char DashString[MAX_LENGTH_STRING] = "------------------------------------------------------------------------------------------";

	if (NrPlotLayers == 0)
		memset((uint8 *) & PlotLayers, 0, sizeof(PlotLayers));


	/*
	#define SOLD_MASK_BOTTOM                        100
	#define SOLD_MASK_TOP                           101
	#define PASTE_MASK_BOTTOM                       200
	#define PASTE_MASK_TOP                          201
	#define INFO_LAYER                              1500
	#define BOARD_OUTLINE_LAYER                     1600
	#define SILKSCREEN_BOTTOM                       2000
	#define SILKSCREEN_TOP                          2001
	#define COMP_OUTLINE_LAYER                      4000
	#define DRILL_LAYER                             6000
	#define SPECIALS_LAYER                          7000
	#define INFO_LAYER2                             7500
	#define INFO_LAYER3                             8000
	#define INFO_LAYER4                             8500
	*/

	count = 0;
	GetLayerTextObjects(SILKSCREEN_TOP_REFS, GerberLayerStr[count], 1);
	LayerCodes[count++] = SILKSCREEN_TOP_REFS;
	GetLayerTextObjects(SILKSCREEN_TOP_VALUES, GerberLayerStr[count], 1);
	LayerCodes[count++] = SILKSCREEN_TOP_VALUES;
	GetLayerTextObjects(SILKSCREEN_TOP, GerberLayerStr[count], 1);
	LayerCodes[count++] = SILKSCREEN_TOP;
	GetLayerTextObjects(PASTE_MASK_TOP, GerberLayerStr[count], 1);
	LayerCodes[count++] = PASTE_MASK_TOP;
	GetLayerTextObjects(SOLD_MASK_TOP, GerberLayerStr[count], 1);
	LayerCodes[count++] = SOLD_MASK_TOP;
	sprintf(GerberLayerStr[count], DashString);
	LayerCodes[count++] = 10000;

	for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
	{
		if (!ReverseLayerNumbering)
			GetLayerText(Design.NrBoardLayers - 1 - cnt, str, 64 + 1);
		else
			GetLayerText(Design.NrBoardLayers - 1 - cnt, str, 64 + 16 + 1);

		sprintf(GerberLayerStr[count], "%s", str);
		LayerCodes[count++] = Design.NrBoardLayers - 1 - cnt;
	}

	sprintf(GerberLayerStr[count], DashString);
	LayerCodes[count] = 10000;

	if (Design.NrBoardLayers > 1)
	{
		sprintf(GerberLayerStr[count], SC(258, "Keepout\t\tTop")); //export gerberu/vrtání, penplot, tisk, bitmap, pdf
		LayerCodes[count++] = ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1;
	}

	if (Design.NrBoardLayers > 2)
	{
		for (cnt = 1; cnt < Design.NrBoardLayers - 1; cnt++)
		{
			if (!ReverseLayerNumbering)
				GetLayerText(Design.NrBoardLayers - 1 - cnt, str, 3);
			else
				GetLayerText(Design.NrBoardLayers - 1 - cnt, str, 16 + 3);

			sprintf(GerberLayerStr[count], SC(260, "Keepout\t%s"), str);
			LayerCodes[count++] = ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1 - cnt;
		}
	}

	sprintf(GerberLayerStr[count], SC(259, "Keepout\t\tBottom")); //export gerberu/vrtání, penplot, tisk, bitmap, pdf
	LayerCodes[count++] = ROUTING_KEEPOUT_LAYER;
	GetLayerTextObjects(SOLD_MASK_BOTTOM, GerberLayerStr[count], 1);
	LayerCodes[count++] = SOLD_MASK_BOTTOM;
	GetLayerTextObjects(PASTE_MASK_BOTTOM, GerberLayerStr[count], 1);
	LayerCodes[count++] = PASTE_MASK_BOTTOM;
	GetLayerTextObjects(SILKSCREEN_BOTTOM_REFS, GerberLayerStr[count], 1);
	LayerCodes[count++] = SILKSCREEN_BOTTOM_REFS;
	GetLayerTextObjects(SILKSCREEN_BOTTOM_VALUES, GerberLayerStr[count], 1);
	LayerCodes[count++] = SILKSCREEN_BOTTOM_VALUES;
	GetLayerTextObjects(SILKSCREEN_BOTTOM, GerberLayerStr[count], 1);
	LayerCodes[count++] = SILKSCREEN_BOTTOM;
	sprintf(GerberLayerStr[count], DashString);
	LayerCodes[count++] = 10000;
	GetLayerTextObjects(DRILL_LAYER, GerberLayerStr[count], 1);
	LayerCodes[count++] = DRILL_LAYER;
	sprintf(GerberLayerStr[count], DashString);
	LayerCodes[count++] = 10000;
	GetLayerTextObjects(BOARD_OUTLINE_LAYER, GerberLayerStr[count], 1);
	LayerCodes[count++] = BOARD_OUTLINE_LAYER;
	GetLayerTextObjects(COMP_OUTLINE_LAYER, GerberLayerStr[count], 1);
	LayerCodes[count++] = COMP_OUTLINE_LAYER;
	GetLayerTextObjects(COMP_OUTLINE_LAYER + 1, GerberLayerStr[count], 1);
	LayerCodes[count++] = COMP_OUTLINE_LAYER + 1;
	GetLayerTextObjects(INFO_LAYER, GerberLayerStr[count], 1);
	LayerCodes[count++] = INFO_LAYER;
	GetLayerTextObjects(INFO_LAYER2, GerberLayerStr[count], 1);
	LayerCodes[count++] = INFO_LAYER2;
	NrTempGerberLayers = Design.NrBoardLayers + 14;
	GetLayerTextObjects(INFO_LAYER3, GerberLayerStr[count], 1);
	LayerCodes[count++] = INFO_LAYER3;
	GetLayerTextObjects(INFO_LAYER4, GerberLayerStr[count], 1);
	LayerCodes[count++] = INFO_LAYER4;
	NrTempGerberLayers = count;

	if (NrPlotLayers == 0)
	{
		if (Design.NrBoardLayers > 1)
		{
			if ((LayerString = GetLayerString(PASTE_MASK_TOP)) != NULL)
				SetPlotLayer(PASTE_MASK_TOP, 1);

			if ((LayerString = GetLayerString(SOLD_MASK_TOP)) != NULL)
				SetPlotLayer(SOLD_MASK_TOP, 1);

			if ((LayerString = GetLayerString(SILKSCREEN_TOP_REFS)) != NULL)
				SetPlotLayer(SILKSCREEN_TOP_REFS, 1);

			if ((LayerString = GetLayerString(PASTE_MASK_BOTTOM)) != NULL)
				SetPlotLayer(PASTE_MASK_BOTTOM, 1);

			if ((LayerString = GetLayerString(SILKSCREEN_BOTTOM_REFS)) != NULL)
				SetPlotLayer(SILKSCREEN_BOTTOM_REFS, 1);
		}
		else
		{
			if ((LayerString = GetLayerString(PASTE_MASK_BOTTOM)) != NULL)
				SetPlotLayer(PASTE_MASK_BOTTOM, 1);

			if ((LayerString = GetLayerString(SILKSCREEN_BOTTOM_REFS)) != NULL)
				SetPlotLayer(SILKSCREEN_BOTTOM_REFS, 1);
		}

		if ((LayerString = GetLayerString(SOLD_MASK_BOTTOM)) != NULL)
			SetPlotLayer(SOLD_MASK_BOTTOM, 1);

		if ((LayerString = GetLayerString(DRILL_LAYER)) != NULL)
			SetPlotLayer(DRILL_LAYER, 1);

		if ((LayerString = GetLayerString(BOARD_OUTLINE_LAYER)) != NULL)
			SetPlotLayer(BOARD_OUTLINE_LAYER, 1);

		for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
		{
			if ((LayerString = GetLayerString(cnt)) != NULL)
				SetPlotLayer(Design.NrBoardLayers - 1 - cnt, 1);
		}

		NrPlotLayers = 1;
	}
}

//*******************************************************************************************************
//******************************* export gerberu/vrtání *************************************************
//*******************************************************************************************************

void SetgerberOutputMode(HWND Dialog)
{
	SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);

	if (GerberInfo.GerberNumberMode == 0)
	{
		if (Units == 0)
			GerberInfo.GerberNumberMode = 4;
		else
			GerberInfo.GerberNumberMode = 4 + 8;
	}

	if ((GerberInfo.GerberNumberMode & 8) == 0)
	{	// thou
		TempUnits = UNITS_INCH;
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "2  2");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "2  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "2  4");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "2  5");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "2  6");

		switch (GerberInfo.GerberNumberMode & 7)
		{
		case 2:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 0, 0);
			break;

		case 3:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
			break;

		case 4:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 2, 0);
			break;

		case 5:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 3, 0);
			break;

		case 6:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 4, 0);
			break;

		default:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 2, 0);
			break;
		}
	}
	else
	{
		TempUnits = UNITS_MM;
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "3  2");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "3  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "3  4");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "4  2");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "4  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "4  4");

		switch (GerberInfo.GerberNumberMode & 7)
		{
		case 2:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 0, 0);
			break;

		case 3:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
			break;

		case 4:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 2, 0);
			break;

		case 5:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 3, 0);
			break;

		case 6:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 4, 0);
			break;

		case 7:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 5, 0);
			break;

		default:
			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
			break;
		}
	}

	SetUnitText(Dialog, IDC_EDIT6, TempUnits);
}

//*******************************************************************************************************
//******************************* export gerberu/vrtání *************************************************
//*******************************************************************************************************

int32 CALLBACK GerberDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, res, res2, Layer, OldTempUnits;
	char str2[MAX_LENGTH_STRING];
	uint16 *Lengte;
	double value1 = 0.0, value2 = 0.0;
	LPSTR LayerString;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(718, "Export gerber/drilling"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(261, "Gerber output format (RS274X)"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(262, "Number format"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(263, "PCB testing"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(264, "Mirror"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(1155, "Material"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(216, "Thickness"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(1156, "Copper thickness"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(265, "Drill option"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(266, "Layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(349, "Options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, "PCB");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(172, "Extra"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(1288, "Areafill option for hatch fill"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC14, "PCB");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC15, SC(512, "Small pen")); //pøidán
		SetDialogItemTextUTF8(Dialog, IDC_STATIC16, SC(513, "Thick pen")); //pøidán
		
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(270, "Output neutral file"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(271, "Plot board outline"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(510, "Include drill tools in drill file (NCD)")); //pøidán
		SetDialogItemTextUTF8(Dialog, IDC_CHECK4, SC(272, "Mirror X"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(322, "Reverse layer numbering"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK6, SC(1160, "Output drill data as gerber file"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK7, SC(511, "On")); //pøidán
		SetDialogItemTextUTF8(Dialog, IDC_CHECK8, SC(0, "Create Gerbv project file"));
		
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(83, "Component references on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO5, SC(82, "Component values on silkscreen "));

		SetDialogItemTextUTF8(Dialog, IDD_CLEAR, SC(161, "Clear all"));
		SetDialogItemTextUTF8(Dialog, IDD_SETALL, SC(160, "Set all"));

		SetDialogItemTextUTF8(Dialog, IDD_UNITS, SC(480, "thou/mm/inch"));

		if (GerberInfo.PlotBoardOutline)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		if (GerberInfo.Invert)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_SETCHECK, 1, 0);

		if (GerberInfo.PlotMode & 1)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK7, BM_SETCHECK, 1, 0);

		if (GerberInfo.GerbvProject)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK8, BM_SETCHECK, 1, 0);

		GerberInfo.Xoffset = 0.0;
		GerberInfo.Yoffset = 0.0;
		strcpy(GerberInfo.TextLine[0], "$DesignName");
		strcpy(GerberInfo.TextLine[1], "$Layer");
		strcpy(GerberInfo.TextLine[2], "$Date");
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[0], GerberInfo.TextLine[1], GerberInfo.TextLine[2],
		        GerberInfo.TextLine[3]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str2);
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[4], GerberInfo.TextLine[5], GerberInfo.TextLine[6],
		        GerberInfo.TextLine[7]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) str2);
		SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, 0, 0);
		SetgerberOutputMode(Dialog);

		switch (TempUnits)
		{
		case UNITS_MM:
			SetDialogValue(Dialog, IDC_EDIT10, AreafillPenSize1);
			SetDialogValue(Dialog, IDC_EDIT11, AreafillPenSize2);
			SetUnitText(Dialog, IDC_EDIT8, TempUnits);
			SetUnitText(Dialog, IDC_EDIT9, TempUnits);
			break;

		case UNITS_INCH:
			TempUnits = UNITS_MILS;
			SetDialogValue(Dialog, IDC_EDIT10, AreafillPenSize1);
			SetDialogValue(Dialog, IDC_EDIT11, AreafillPenSize2);
			SetUnitText(Dialog, IDC_EDIT8, TempUnits);
			SetUnitText(Dialog, IDC_EDIT9, TempUnits);
			TempUnits = UNITS_INCH;
			break;
		}

		if (GerberInfo.OutputNeutral == 1)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 0, 0);
		if (GerberInfo.ReverseLayerNumbering)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 1, 0);

		SendDlgItemMessageOwn(Dialog, IDC_CHECK6, BM_SETCHECK, 0, 0);
		if (GerberInfo.DrillAsGerber)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK6, BM_SETCHECK, 1, 0);

		if (GerberInfo.GerberOutputMode == 0)
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);

		if (GerberInfo.DrillOutputOption == 1)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);

//      SendDlgItemMessageOwn(Dialog,IDC_RADIO2,BM_SETCHECK,1,0);
//      SendDlgItemMessageOwn(Dialog,IDC_EDIT2,WM_SETTEXT,0,(LPARAM)AperTureFile);

		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
		InitPlotLayers(GerberInfo.ReverseLayerNumbering, 0);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
		{
			Layer = LayerCodes[cnt];

			if ((Layer != 10000) && (IsLayerPlottable(Layer)))
			{
				if ((LayerString = GetLayerString(Layer)) != NULL)
				{
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
				}
			}
		}

		if (GerberInfo.PcbMaterial[0] == 0)
			strcpy(GerberInfo.PcbMaterial, "FR4");

		if (GerberInfo.PcbThickness[0] == 0)
			strcpy(GerberInfo.PcbThickness, "1.6 mm");

		if (GerberInfo.PcbCopperThickness[0] == 0)
			strcpy(GerberInfo.PcbCopperThickness, "0.035 mm");

		SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) GerberInfo.PcbMaterial);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) GerberInfo.PcbThickness);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) GerberInfo.PcbCopperThickness);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) GerberInfo.Extra1);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
			GerberInfo.GerberNumberMode &= ~7;
			GerberInfo.GerberNumberMode |= (res + 2);
			GerberInfo.OutputNeutral = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.OutputNeutral = 1;

			GerberInfo.DrillOutputOption = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.DrillOutputOption = 1;

			GerberInfo.Invert = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.Invert = 1;

			GerberInfo.PlotBoardOutline = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.PlotBoardOutline = 1;

			GerberInfo.PlotMode &= ~1;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK7, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.PlotMode |= 1;

			if (TempUnits == UNITS_INCH)
				TempUnits = UNITS_MILS;

			AreafillPenSize1 = GetDialogValue(Dialog, IDC_EDIT10);
			AreafillPenSize2 = GetDialogValue(Dialog, IDC_EDIT11);
			AreafillPenSize1 = max(AreafillPenSize1, 100.0);
			AreafillPenSize2 = max(AreafillPenSize2, 100.0);

			if (AreafillPenSize2 <= AreafillPenSize1)
				AreafillPenSize2 = AreafillPenSize1 * 5.0;

			GerberInfo.DrillAsGerber = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK6, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.DrillAsGerber = 1;

			GerberInfo.GerbvProject = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK8, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.GerbvProject = 1;

			NrGerberLayers = 0;

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];
				SetPlotLayer(Layer, 0);

				if (Layer != 10000)
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						res2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, res, 0);

						if (res2 != 0)
						{
							GerberLayers[NrGerberLayers++] = Layer;
							SetPlotLayer(Layer, 1);
						}
					}
				}
			}

			res = SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine);

			if (res > 0)
				strcpy(AperTureFile, DialogTextLine);

			Lengte = (uint16 *) & DialogTextLine;

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 1;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt], DialogTextLine);
			}

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 1;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT3, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt + 4], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt + 4], DialogTextLine);
			}

			GerberInfo.GerberOutputMode = 1;
			GerberInfo.ReverseLayerNumbering = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.ReverseLayerNumbering = 1;

			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_GETTEXT, 70, (LPARAM) GerberInfo.PcbMaterial);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_GETTEXT, 70, (LPARAM) GerberInfo.PcbThickness);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT5, WM_GETTEXT, 70, (LPARAM) GerberInfo.PcbCopperThickness);
			memset(GerberInfo.Extra1, 0, sizeof(GerberInfo.Extra1));
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, WM_GETTEXT, MAX_LENGTH_STRING * 3 - 1, (LPARAM) GerberInfo.Extra1);
			EndDialog(Dialog, 1);
			return about;

		case IDC_CHECK5:
			GerberInfo.ReverseLayerNumbering = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.ReverseLayerNumbering = 1;

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
			InitPlotLayers(GerberInfo.ReverseLayerNumbering, 0);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
				SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];

				if ((Layer != 10000) && (IsLayerPlottable(Layer)))
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					}
				}
			}

			break;

		case IDD_CLEAR:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 0, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 0);
				}
			}

//          SendDlgItemMessageOwn(Dialog,IDC_CHECK1,BM_SETCHECK,0,0);
			break;

		case IDD_SETALL:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 1);
				}
			}

//          SendDlgItemMessageOwn(Dialog,IDC_CHECK1,BM_SETCHECK,1,0);
			break;

		case IDD_UNITS:
			OldTempUnits = TempUnits;

			switch (TempUnits)
			{
			case UNITS_MM:
				value1 = GetDialogValue(Dialog, IDC_EDIT10);
				value2 = GetDialogValue(Dialog, IDC_EDIT11);
				TempUnits = UNITS_MILS;
				break;

			case UNITS_INCH:
				TempUnits = UNITS_MILS;
				value1 = GetDialogValue(Dialog, IDC_EDIT10);
				value2 = GetDialogValue(Dialog, IDC_EDIT11);
				TempUnits = UNITS_MM;
				break;
			}

			SetDialogValue(Dialog, IDC_EDIT10, value1);
			SetDialogValue(Dialog, IDC_EDIT11, value2);
			SetUnitText(Dialog, IDC_EDIT8, TempUnits);
			SetUnitText(Dialog, IDC_EDIT9, TempUnits);

			switch (OldTempUnits)
			{
			case UNITS_MM:
				GerberInfo.GerberNumberMode &= ~8;
				break;

			case UNITS_INCH:
				GerberInfo.GerberNumberMode |= 8;
				break;
			}

			SetgerberOutputMode(Dialog);
			break;

		case IDHELP:
			Help("plot_output_to_gerber_format.htm", 0);
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


int32 GerberDialog(int32 mode)
{
	int32 res;

	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_GERBER35), PCBWindow, (DLGPROC) GerberDialogBody);
	return res;
}

//*******************************************************************************************************
//************************************************ export penplotu ******************************************
//*******************************************************************************************************

int32 CALLBACK PlotDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt;
	float value1, value2;
	int32 res, ok, res2, Layer;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	uint16 *Lengte;
	LPSTR LayerString;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(276, "Pen sizes"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(277, "Pen 1"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(278, "Pen 2"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(279, "Plot options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(185, "Scale factor"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(280, "Pen speed (cm/sec)"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(281, "Offset"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(266, "Layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(185, "Scale factor"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, "PCB");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC30, SC(264, "Mirror"));

		SetDialogItemTextUTF8(Dialog, IDD_CLEAR, SC(161, "Clear all"));
		SetDialogItemTextUTF8(Dialog, IDD_SETALL, SC(160, "Set all"));

		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(272, "Mirror X"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(271, "Plot board outline"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(322, "Reverse layer numbering"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(83, "Component references on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO5, SC(82, "Component values on silkscreen "));

		if (DialogMode == 0)
			SetWindowTextUTF8(Dialog, SC(719, "Print plotfiles"));
		else
			SetWindowTextUTF8(Dialog, SC(283, "Export penplot"));

		TempUnits = 1;

		if (GerberInfo.Invert)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		if (GerberInfo.PlotBoardOutline)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		strcpy(GerberInfo.TextLine[0], "$DesignName");
		strcpy(GerberInfo.TextLine[1], "$Layer");
		strcpy(GerberInfo.TextLine[2], "$Date");
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[0], GerberInfo.TextLine[1], GerberInfo.TextLine[2],
		        GerberInfo.TextLine[3]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str2);
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[4], GerberInfo.TextLine[5], GerberInfo.TextLine[6],
		        GerberInfo.TextLine[7]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) str2);
		sprintf(str2, "%.4f", GerberInfo.ScaleFactor);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) str2);

		if (GerberInfo.PlotBoardOutline)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		if (DialogMode == 1)
		{
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) str2);
			sprintf(str, "%.4f", GerberInfo.PenSizes[0] / 100000);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) str);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) "mm");
			sprintf(str, "%.4f", GerberInfo.PenSizes[1] / 100000);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) str);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT8, WM_SETTEXT, 0, (LPARAM) "mm");
			sprintf(str, "%.1f", GerberInfo.PenSpeed);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) str);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT9, WM_SETTEXT, 0, (LPARAM) "0.0");
			SendDlgItemMessageOwn(Dialog, IDC_EDIT10, WM_SETTEXT, 0, (LPARAM) "0.0");
			SendDlgItemUnits(Dialog, IDC_EDIT12, TempUnits);
		}

		SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 0, 0);

		if (GerberInfo.ReverseLayerNumbering)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 1, 0);

		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
		InitPlotLayers(GerberInfo.ReverseLayerNumbering, 1);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
		{
			Layer = LayerCodes[cnt];

			if ((Layer != 10000) && (IsLayerPlottable(Layer)))
			{
				if ((LayerString = GetLayerString(Layer)) != NULL)
				{
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
				}
			}
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			GerberInfo.Invert = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.Invert = 1;

			GerberInfo.PlotBoardOutline = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.PlotBoardOutline = 1;

			if (DialogMode == 1)
			{
				value1 = (float) GetDialogValue(Dialog, IDC_EDIT9);

				if ((value1 < -10000e5) || (value1 > 10000e5))
				{
					MessageBoxOwn(PCBWindow, SC(284, "Wrong X offset"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				GerberInfo.Xoffset = value1;
				memset(DialogTextLine, 0, 200);
				SendDlgItemMessageOwn(Dialog, IDC_EDIT10, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine);
				ok = 1;
				value1 = (float) GetDialogValue(Dialog, IDC_EDIT10);

				if ((value1 < -10000e5) || (value1 > 10000e5))
				{
					MessageBoxOwn(PCBWindow, SC(285, "Wrong Y offset"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				GerberInfo.Yoffset = value1;
				memset(DialogTextLine, 0, 200);
				SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine);
				ok = 1;

				if ((sscanf(DialogTextLine, "%f", &value1) != 1) || (value1 < 0.01) || (value1 > 1000.0))
				{
					MessageBoxOwn(PCBWindow, SC(286, "Wrong pen speed"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				GerberInfo.PenSpeed = value1;
				memset(DialogTextLine, 0, 200);
				SendDlgItemMessageOwn(Dialog, IDC_EDIT5, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine);
				ok = 1;

				if ((sscanf(DialogTextLine, "%f", &value1) != 1) || (value1 < 0.01) || (value1 > 10.0))
				{
					MessageBoxOwn(PCBWindow, SC(287, "Wrong pen size"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				GerberInfo.PenSizes[0] = value1 * 100000;
				memset(DialogTextLine, 0, 200);
				SendDlgItemMessageOwn(Dialog, IDC_EDIT6, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine);
				ok = 1;
				value1 = 0.0;

				if ((sscanf(DialogTextLine, "%f", &value1) != 1) || (value1 != 0.0))
				{
					value1 *= 100000.0;
					value2 = (float) GerberInfo.PenSizes[0];
					GerberInfo.PenSizes[0] = min(value1, value2);
					GerberInfo.PenSizes[1] = max(value1, value2);
					GerberInfo.NrPlotPens = 2;
				}
			}

			memset(DialogTextLine, 0, 200);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine);
			ok = 1;

			if ((sscanf(DialogTextLine, "%f", &value1) != 1) || (value1 < 0.001) || (value1 > 1000.0))
			{
				MessageBoxOwn(PCBWindow, SC(288, "Wrong scaling factor"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			GerberInfo.ScaleFactor = value1;
			NrGerberLayers = 0;

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];
				SetPlotLayer(Layer, 0);

				if (Layer != 10000)
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						res2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, res, 0);

						if (res2 != 0)
						{
							GerberLayers[NrGerberLayers++] = Layer;
							SetPlotLayer(Layer, 1);
						}
					}
				}
			}

			Lengte = (uint16 *) & DialogTextLine;

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, 200);
				*Lengte = 180;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt], DialogTextLine);
			}

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, 200);
				*Lengte = 180;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT3, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt + 4], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt + 4], DialogTextLine);
			}

			EndDialog(Dialog, 1);
			return about;

		case IDD_UNITS:
			value1 = (float) GetDialogValue(Dialog, IDC_EDIT9);
			TempUnits ^= 1;
			SetDialogValue(Dialog, IDC_EDIT9, value1);
			TempUnits ^= 1;
			value1 = (float) GetDialogValue(Dialog, IDC_EDIT10);
			TempUnits ^= 1;
			SetDialogValue(Dialog, IDC_EDIT10, value1);
			SendDlgItemUnits(Dialog, IDC_EDIT12, TempUnits);
			break;

		case IDC_CHECK5:
			GerberInfo.ReverseLayerNumbering = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.ReverseLayerNumbering = 1;

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
			InitPlotLayers(GerberInfo.ReverseLayerNumbering, 1);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
				SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];

				if ((Layer != 10000) && (IsLayerPlottable(Layer)))
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					}
				}
			}

			break;

		case IDD_CLEAR:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 0, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 0);
				}
			}

			break;

		case IDD_SETALL:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 1);
				}
			}

			break;

		case IDHELP:
			if (DialogMode == 0)
				Help("plot_output_to_printer.htm", 0);
			else
				Help("penplot_output.htm", 0);

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

int32 PlotDialog(int32 mode)
{
	int32 res;

	/*
	#define SOLD_MASK_BOTTOM                        100
	#define SOLD_MASK_TOP                           101
	#define PASTE_MASK_BOTTOM                       200
	#define PASTE_MASK_TOP                          201
	#define INFO_LAYER                              1500
	#define SILKSCREEN_BOTTOM                       2000
	#define SILKSCREEN_TOP                          2001
	#define PLACEMENT_OUTLINE_LAYER                 3000
	#define COMP_OUTLINE_LAYER                      4000
	#define PCB_TOP                                 5000
	#define PCB_BOTTOM                              5500
	#define DRILL_LAYER                             6000

	*/
	res = 0;
	DialogMode = mode;

	if (mode == 0)
		res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PLOT35), PCBWindow, (DLGPROC) PlotDialogBody);
	else
	{
		res =
		    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PENPLOT20), PCBWindow,
		                 (DLGPROC) PlotDialogBody);
	}

	return res;
}

// *******************************************************************************************************
// ************************************ Exportovat soubory plotu do formátu bitmapy **********************
// *******************************************************************************************************

int32 CALLBACK ExportBitmapDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
#define    InRange20(x1,x2) ( (((x1>x2-1.0) && (x1<x2+1.0))) ? (1) : (0) )

	int32 about;
	int32 cnt, res, ok, res2, Found, Layer;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	uint16 *Lengte;
	double value1;
	LPSTR LayerString;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(295, "Export plotfiles to bitmap format"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, "PCB");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(279, "Plot options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(289, "Bitmap resolution"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(266, "Layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(290, "User"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(291, "Compression"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(83, "Component references on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO5, SC(82, "Component values on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO6, SC(1087, "Not compressed"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO7, SC(1088, "Compressed"));

		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(293, "Plot board outline on each layer"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(272, "Mirror X"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(322, "Reverse layer numbering"));

		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm/Âµm");

		SetDialogItemTextUTF8(Dialog, IDD_CLEAR, SC(161, "Clear all"));
		SetDialogItemTextUTF8(Dialog, IDD_SETALL, SC(160, "Set all"));

		TempUnits = 1;

		if (GerberInfo.PlotBoardOutline)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		strcpy(GerberInfo.TextLine[0], "$DesignName");
		strcpy(GerberInfo.TextLine[1], "$Layer");
		strcpy(GerberInfo.TextLine[2], "$Date");
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[0], GerberInfo.TextLine[1], GerberInfo.TextLine[2],
		        GerberInfo.TextLine[3]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str2);
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[4], GerberInfo.TextLine[5], GerberInfo.TextLine[6],
		        GerberInfo.TextLine[7]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) str2);
		sprintf(str2, "%.4f", GerberInfo.ScaleFactor);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) str2);


		if (GerberInfo.Invert)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		if (GerberInfo.PlotBoardOutline)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		if ((GerberInfo.BitmapExportSaveMode & 1) == 0)
			SendDlgItemMessageOwn(Dialog, IDC_RADIO7, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessageOwn(Dialog, IDC_RADIO6, BM_SETCHECK, 1, 0);

		TempUnits = Units;
		Found = -1;

		for (cnt = 0; cnt < NrBitmapExportResolutions; cnt++)
		{
			if (InRange20(GerberInfo.BitmapExportResolution, 2540000.0 / BitmapExportResolutions[cnt]))
				Found = cnt;

			sprintf(str, "%d dpi", BitmapExportResolutions[cnt]);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) str);
		}

		if (Found != -1)
		{
			SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_SETCURSEL, (WPARAM) Found, 0);
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, EM_SETREADONLY, 1, 0);
		}
		else
		{
			SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
			SendDlgItemUnits(Dialog, IDC_EDIT7, TempUnits);

			if (TempUnits == 0)
				SetDialogFloatValue(Dialog, IDC_EDIT2, (GerberInfo.BitmapExportResolution / 2540.0), 4);
			else
				SetDialogFloatValue(Dialog, IDC_EDIT2, (GerberInfo.BitmapExportResolution / 100000.0), 6);
		}

		SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 0, 0);

		if (GerberInfo.ReverseLayerNumbering)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 1, 0);

		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
		InitPlotLayers(GerberInfo.ReverseLayerNumbering, 1);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
		{
			Layer = LayerCodes[cnt];

			if ((Layer != 10000) && (IsLayerPlottable(Layer)))
			{
				if ((LayerString = GetLayerString(Layer)) != NULL)
				{
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
				}
			}
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.Invert = 1;

			GerberInfo.PlotBoardOutline = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.PlotBoardOutline = 1;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO6, BM_GETCHECK, 0, 0) == 1)
			{
				GerberInfo.BitmapExportSaveMode |= 1;	// Non compressed
			}
			else
			{
				GerberInfo.BitmapExportSaveMode &= ~1;	// Compressed
			}

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0) == 1)
			{
				GetDialogFloatValue(Dialog, IDC_EDIT2, &value1);

				switch (TempUnits)
				{
				case 0:
					value1 *= 2540.0;
					break;

				case 1:
					value1 *= 100000.0;
					break;

				case 2:
					value1 *= 100.0;
					break;
				}

				if ((value1 < 1.0) || (value1 > 100000.0))
				{
					MessageBoxOwn(PCBWindow, SC(297, "Wrong dot size"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				GerberInfo.BitmapExportResolution = value1;
			}
			else
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

				if (res == -1)
				{
					MessageBoxOwn(PCBWindow, SC(298, "A value should be selected"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					return about;
				}

				GerberInfo.BitmapExportResolution = (2540000.0 / BitmapExportResolutions[res]);
			}

			NrGerberLayers = 0;

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];
				SetPlotLayer(Layer, 0);

				if (Layer != 10000)
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						res2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, res, 0);

						if (res2 != 0)
						{
							GerberLayers[NrGerberLayers++] = Layer;
							SetPlotLayer(Layer, 1);
						}
					}
				}
			}

			Lengte = (uint16 *) & DialogTextLine;

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 1;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt], DialogTextLine);
			}

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 1;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT3, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt + 4], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt + 4], DialogTextLine);
			}

			EndDialog(Dialog, 1);
			return about;

		case IDC_CHECK5:
			GerberInfo.ReverseLayerNumbering = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.ReverseLayerNumbering = 1;

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
			InitPlotLayers(GerberInfo.ReverseLayerNumbering, 1);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
				SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];

				if ((Layer != 10000) && (IsLayerPlottable(Layer)))
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					}
				}
			}

			break;

		case IDC_COMBO1:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
			ok = 1;
			break;

		case IDC_RADIO2:
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
			ok = 1;
			break;

		case IDC_RADIO3:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT7, EM_SETREADONLY, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_SETCURSEL, (WPARAM) - 1, 0);
			value1 = GerberInfo.BitmapExportResolution;
			SendDlgItemUnits(Dialog, IDC_EDIT7, TempUnits);

			switch (TempUnits)
			{
			case 0:
				value1 /= 2540.0;
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 4);
				break;

			case 1:
				value1 /= 100000.0;
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 6);
				break;

			case 2:
				value1 /= 100.0;
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 3);
				break;
			}

			ok = 1;
			break;

		case IDD_UNITS:
			GetDialogFloatValue(Dialog, IDC_EDIT2, &value1);

			switch (TempUnits)
			{
			case 0:
				value1 *= 2540.0;
				break;

			case 1:
				value1 *= 100000.0;
				break;

			case 2:
				value1 *= 100.0;
				break;
			}

			TempUnits = (TempUnits + 1) % 3;
			SendDlgItemUnits(Dialog, IDC_EDIT7, TempUnits);

			switch (TempUnits)
			{
			case 0:
				value1 /= 2540.0;
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 4);
				break;

			case 1:
				value1 /= 100000.0;
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 6);
				break;

			case 2:
				value1 /= 100.0;
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 3);
				break;
			}

			break;

		case IDD_CLEAR:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 0, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 0);
				}
			}

			break;

		case IDD_SETALL:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 1);
				}
			}

			break;

		case IDHELP:
			Help("export_bmp.htm", 0);
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

int32 ExportBitmapDialog(int32 mode)
{
	int32 res;
	/*
	#define SOLD_MASK_BOTTOM                        100
	#define SOLD_MASK_TOP                           101
	#define PASTE_MASK_BOTTOM                       200
	#define PASTE_MASK_TOP                          201
	#define INFO_LAYER                              1500
	#define SILKSCREEN_BOTTOM                       2000
	#define SILKSCREEN_TOP                          2001
	#define PLACEMENT_OUTLINE_LAYER                 3000
	#define COMP_OUTLINE_LAYER                      4000
	#define PCB_TOP                                 5000
	#define PCB_BOTTOM                              5500
	#define DRILL_LAYER                             6000

	*/
	res = 0;
	DialogMode = mode;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_BITMAP_OUTPUT35), PCBWindow,
	                 (DLGPROC) ExportBitmapDialogBody);
	return res;
}

//*******************************************************************************************************
//********************************** IDD_DIALOG_PDF_OUTPUT35 ********************************************
//*******************************************************************************************************

int32 CALLBACK ExportPDFDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
#define    InRange20(x1,x2) ( (((x1>x2-1.0) && (x1<x2+1.0))) ? (1) : (0) )

	int32 about;
	int32 cnt, res, res2, Layer;
	double value1;
	char str2[MAX_LENGTH_STRING];
	LPSTR LayerString;
	uint16 *Lengte;

	value1 = 0.0;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, "PCB");
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(279, "Plot options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(289, "Bitmap resolution"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(266, "Layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(299, "Paper options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(300, "Orientation"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(301, "Scale"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(302, "Paper size"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(83, "Component references on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO5, SC(82, "Component values on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO6, "Auto");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO7, SC(304, "Portrait"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO8, SC(305, "Landscape"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO9, SC(306, "Scale 1x"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO10, SC(307, "Fit to page"));

		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(293, "Plot board outline on each layer"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(322, "Reverse layer numbering"));

		SetDialogItemTextUTF8(Dialog, IDD_CLEAR, SC(161, "Clear all"));
		SetDialogItemTextUTF8(Dialog, IDD_SETALL, SC(160, "Set all"));

		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A1");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A2");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A3");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A4");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A5");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B4");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B5");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B4_JIS");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B5_JIS");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "LEGAL");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "LETTER");

		for (cnt = 0; cnt < NrPageOutputIds; cnt++)
		{
			if (PageOutputIds[cnt] == PDFInfo.PaperSize)
				SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_SETCURSEL, (WPARAM) cnt, 0);
		}

		switch (PDFInfo.PaperOrientation)
		{
		case ORIENTATION_AUTO:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO6, BM_SETCHECK, 1, 0);
			break;

		case ORIENTATION_PORTRAIT:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO7, BM_SETCHECK, 1, 0);
			break;

		case ORIENTATION_LANDSCAPE:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO8, BM_SETCHECK, 1, 0);
			break;
		}

		if (PDFInfo.PaperFitToPage == 0)
			SendDlgItemMessageOwn(Dialog, IDC_RADIO9, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessageOwn(Dialog, IDC_RADIO10, BM_SETCHECK, 1, 0);

		SetWindowTextUTF8(Dialog, SC(308, "Export plot files to PDF"));

		if (GerberInfo.PlotBoardOutline)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		strcpy(GerberInfo.TextLine[0], "$DesignName");
		strcpy(GerberInfo.TextLine[1], "$Layer");
		strcpy(GerberInfo.TextLine[2], "$Date");
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[0], GerberInfo.TextLine[1], GerberInfo.TextLine[2],
		        GerberInfo.TextLine[3]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str2);
		sprintf(str2, "%s\r\n%s\r\n%s\r\n%s", GerberInfo.TextLine[4], GerberInfo.TextLine[5], GerberInfo.TextLine[6],
		        GerberInfo.TextLine[7]);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) str2);
		sprintf(str2, "%.4f", GerberInfo.ScaleFactor);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) str2);

		if (GerberInfo.PlotBoardOutline)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 0, 0);

		if (GerberInfo.ReverseLayerNumbering)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_SETCHECK, 1, 0);

		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
		InitPlotLayers(GerberInfo.ReverseLayerNumbering, 1);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

		for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
		{
			Layer = LayerCodes[cnt];

			if ((Layer != 10000) && (IsLayerPlottable(Layer)))
			{
				if ((LayerString = GetLayerString(Layer)) != NULL)
				{
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
				}
			}
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO6, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_AUTO;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO7, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_PORTRAIT;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO8, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_LANDSCAPE;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO9, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperFitToPage = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO10, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperFitToPage = 1;

			res = SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

			if (res != -1)
				PDFInfo.PaperSize = PageOutputIds[res];

			GerberInfo.PlotBoardOutline = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.PlotBoardOutline = 1;

			NrGerberLayers = 0;

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];
				SetPlotLayer(Layer, 0);

				if (Layer != 10000)
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						res2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, res, 0);

						if (res2 != 0)
						{
							GerberLayers[NrGerberLayers++] = Layer;
							SetPlotLayer(Layer, 1);
						}
					}
				}
			}

			Lengte = (uint16 *) & DialogTextLine;

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 1;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt], DialogTextLine);
			}

			for (cnt = 0; cnt < 4; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 1;
				res = SendDlgItemMessageOwn(Dialog, IDC_EDIT3, EM_GETLINE, cnt, (LPARAM) DialogTextLine);
				DialogTextLine[res] = 0;
				memset(&GerberInfo.TextLine[cnt + 4], 0, sizeof(GerberInfo.TextLine[cnt]));

				if (DialogTextLine[0] != 0)
					strcpy(GerberInfo.TextLine[cnt + 4], DialogTextLine);
			}

			EndDialog(Dialog, 1);
			return about;

		case IDC_CHECK5:
			GerberInfo.ReverseLayerNumbering = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.ReverseLayerNumbering = 1;

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
			InitPlotLayers(GerberInfo.ReverseLayerNumbering, 1);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
				SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) GerberLayerStr[cnt]);

			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				Layer = LayerCodes[cnt];

				if ((Layer != 10000) && (IsLayerPlottable(Layer)))
				{
					if ((LayerString = GetLayerString(Layer)) != NULL)
					{
						res =
						    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1,
						                          (LPARAM) LayerString);
						SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					}
				}
			}

			break;

		case IDD_CLEAR:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 0, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 0);
				}
			}

			break;

		case IDD_SETALL:
			for (cnt = 0; cnt < NrTempGerberLayers; cnt++)
			{
				if (LayerCodes[cnt] != 10000)
				{
					LayerString = GetLayerString(LayerCodes[cnt]);
					res =
					    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRINGEXACT, (UINT) - 1, (LPARAM) LayerString);
					SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
					SetPlotLayer(LayerCodes[cnt], 1);
				}
			}

			break;

		case IDHELP:
			Help("export_pdf.htm", 0);
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

int32 ExportPDFDialog(int32 mode)
{
	int32 res;
	/*
	#define SOLD_MASK_BOTTOM                        100
	#define SOLD_MASK_TOP                           101
	#define PASTE_MASK_BOTTOM                       200
	#define PASTE_MASK_TOP                          201
	#define INFO_LAYER                              1500
	#define SILKSCREEN_BOTTOM                       2000
	#define SILKSCREEN_TOP                          2001
	#define PLACEMENT_OUTLINE_LAYER                 3000
	#define COMP_OUTLINE_LAYER                      4000
	#define PCB_TOP                                 5000
	#define PCB_BOTTOM                              5500
	#define DRILL_LAYER                             6000

	*/
	res = 0;
	DialogMode = mode;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PDF_OUTPUT35), PCBWindow,
	                 (DLGPROC) ExportPDFDialogBody);
	return res;
}

//***************************************************************************************************************
//***************************************** O programu IDD_DIALOG_ABOUT *****************************************
//***************************************************************************************************************

int32 CALLBACK AboutDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(730, "About program"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(731, "Layout editor PCB Elegance"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, "Web PCB Elegance"); //pøidán

		sprintf(str, SC(732, "\r\n Build version %i.%i.%i  ( %s )"), VER_VERSION / 100, VER_VERSION % 100, VER_BUILD, VER_DATE_STR);

#ifdef GCC_COMP
		strcat(str, "\r\n\r\n Compiled with mingw (gcc 4.9.2)");
#endif
#ifdef VC2005
		strcat(str, "\r\n\r\n Compiled with Microsoft Visual Studio 2005");
#endif
#ifdef VC2010
		strcat(str, SC(733, "\r\n\r\n Compiled with Microsoft Visual Studio 2019"));
#endif
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) str);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			ShellExecute(0, 0, "http://www.pcbelegance.org", 0, 0, SW_SHOW); //pøidán
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 AboutDialog()
{
	int32 res;

	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ABOUT), PCBWindow, (DLGPROC) AboutDialogBody);
	return res;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK ComponentPlacement2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, FoundComp, cnt, cnt2, res, CompNr, MemSize, Layer, DigitString;
	uint16 *Lengte;
	uint8 *SelectedComponents;
	CompRecord *Comp, *NewComp, *NewComp2;
	ObjectRecord *Object;
	float x, y, rotation, MultFactor;
	char sel[MAX_LENGTH_STRING], RefName[MAX_LENGTH_STRING], PositionX[MAX_LENGTH_STRING], PositionY[MAX_LENGTH_STRING],
	     Rotation[MAX_LENGTH_STRING], LineCopy[MAX_LENGTH_STRING], UnitsStr[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(310, "Component placement"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
//          Component  U100  Top DIP24S   <partnr> GAL20V8     1000.0,10020.0 mil  0
			FoundComp = 0;
			Lengte = (uint16 *) & DialogTextLine;
			AllocateSpecialMem(MEM_NET_SELECTED, (Design.NrComps + 10) * sizeof(uint8), (void **) &SelectedComponents);
			memset(SelectedComponents, 0, Design.NrComps);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);

			for (cnt = 0; cnt < 5000; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 1;
				res = SendDlgItemMessageOwn(Dialog, IDD_MESSAGE_EDIT, EM_GETLINE, cnt, (LPARAM) DialogTextLine);

				if (res > MAX_LENGTH_STRING - 10)
					res = MAX_LENGTH_STRING - 10;

				DialogTextLine[res] = 0;
				strcpy(LineCopy, DialogTextLine);

				if ((DialogTextLine[0] != 0) && (DialogTextLine[0] != '#') && (DialogTextLine[0] != ';')
				        && (DialogTextLine[0] != '-'))
				{
//              GetString(DialogTextLine,sel);
					GetString(DialogTextLine, RefName);
					CompNr = FindCompNr(RefName, 0);

					if (CompNr != -1)
					{
						GetString(DialogTextLine, sel);
						Layer = 0;

//                if (stricmpOwn(sel,SC(311, "Top"))==0) Layer=0;
						if (stricmpOwn(sel, SC(312, "Bottom")) == 0)
							Layer = 1;

						strcpy(LineCopy, DialogTextLine);
						GetString(DialogTextLine, sel);	// geometry

						if (sel[0] == '"')
						{
							strcpy(DialogTextLine, LineCopy);
							GetQuoteString(DialogTextLine, sel);	// geometry
						}

						strcpy(LineCopy, DialogTextLine);
						GetString(DialogTextLine, sel);	// Part nr (optional)

						if (sel[0] == '"')
						{
							strcpy(DialogTextLine, LineCopy);
							GetQuoteString(DialogTextLine, sel);	// Part nr (optional)
						}

						strcpy(LineCopy, DialogTextLine);
						GetString(DialogTextLine, sel);	// value

						if (sel[0] == '"')
						{
							strcpy(DialogTextLine, LineCopy);
							GetQuoteString(DialogTextLine, sel);	// value
						}

						DigitString = 1;
						res = strlen(sel);

						for (cnt2 = 0; cnt2 < res; cnt2++)
						{
							if ((!isdigit(sel[cnt2])) && (sel[cnt2] != '.') && (sel[cnt2] != 'e') && (sel[cnt2] != 'E'))
								DigitString = 0;
						}

						if (DigitString)
							strcpy(PositionX, sel);
						else
							GetString(DialogTextLine, PositionX);

						GetString(DialogTextLine, PositionY);
						GetString(DialogTextLine, UnitsStr);
						GetString(DialogTextLine, Rotation);
						rotation = 0.0;
						x = 0.0;
						y = 0.0;
						MultFactor = 100000.0;

						if ((sscanf(PositionX, "%f", &x) == 1) && (sscanf(PositionY, "%f", &y) == 1)
						        && (sscanf(Rotation, "%f", &rotation) == 1))
						{
							FoundComp = 1;

							if (stricmpOwn(UnitsStr, "inch") == 0)
								MultFactor = 2540000.0;

							if ((stricmpOwn(UnitsStr, "thou") == 0) || (stricmpOwn(UnitsStr, "mil") == 0)
								|| (stricmpOwn(UnitsStr, "mils") == 0))
								MultFactor = 2540.0;

							x *= MultFactor;
							y *= MultFactor;
							Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
							MemSize = MemSizeComp(Comp);
							memmove(NewComp, Comp, MemSize);
							NewComp->CompOriginX = x;
							NewComp->CompOriginY = y;
							NewComp->CompMode &= ~0x000f;
							NewComp->Rotation = rotation;

							if (Layer == 1)
								NewComp->CompMode |= 8;

							if (AddComp(NewComp))
							{
								Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
								Comp->Info |= OBJECT_NOT_VISIBLE;
								Comp->DeleteNr = (int16) LastActionNr;
								NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);

								NrObjects = 0;
								ShapePinsToObject(NewComp2, 0.0, 0.0, 0, 0, 0, 0);

								for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
								{
									Object = &((*Objects)[cnt2]);

									if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
										SelectedComponents[Object->NetNr] = 1;
								}

							}
						}
					}
				}
			}

			if (FoundComp)
			{
				for (cnt = 0; cnt < Design.NrNets; cnt++)
				{
					if (SelectedComponents[cnt] == 1)
					{
						ReCalcConnectionsNet((int32) cnt, 0, 1);
						CheckForEscape();
					}
				}
			}

			DeallocateSpecialMem(MEM_NET_SELECTED);
			EndDialog(Dialog, 1);
			break;

		case IDHELP:
			Help("compPlacement.htm", 0);
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

int32 ComponentPlacementDialog(int32 Mode)
{
	int32 res, ok;

//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;
	DialogMode = Mode;
	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SETCOMP), PCBWindow, (DLGPROC) ComponentPlacement2);
	ok = 1;
	return res;
}

int32 Registration(void)
{
	return;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InitGeometryLibEntries(HWND Dialog, LPSTR LibraryName)
{
	LibRecord Lib;
	LibNameRecord LibName;
	int32 cnt, NrLibEntries;
	int32 Libfp, result;
	char str[MAX_LENGTH_STRING];

	strcpy(str, LibraryName);

	if ((Libfp = FileOpenReadOnlyUTF8(str)) <= 0)
		return 0;

	if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
		return 0;

	if (strcmp(Lib.Identification, LibraryCode1) == 0)
	{
		NrLibEntries = Lib.NrLibEntries;

		for (cnt = 0; cnt < (int32) min(NrLibEntries, 5000); cnt++)
		{
			if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
			{
				FileClose(Libfp);
				return 0;
			}

			SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LibName.Text));
		}
	}

	FileClose(Libfp);
	return 1;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 InitGeometriesEntries(HWND Dialog, LPSTR DirName)
{
	int32 res, lengte, count;
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	count = 0;
	DialogResult = 0;
	sprintf(str, "%s\\*.shp", DirName);

	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		lengte = strlen(str2);
		str2[lengte - 4] = 0;
		lengte -= 4;
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (str2));
		count++;
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	return count;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CALLBACK SelectGeomDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, res, hulp, TabStops[5], ok, LibMode, Found, Found2;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING * 5], InitStr[MAX_LENGTH_STRING],
	     ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;


	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(316, "Select geometry"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(314, "Geometry source"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(315, "Geometries"));

		if (GeometryDialogInitialX != -1)
		{
			GetWindowRect(Dialog, &DialogWindowRect);
			MoveWindow(Dialog, GeometryDialogInitialX, GeometryDialogInitialY,
			           DialogWindowRect.right - DialogWindowRect.left, DialogWindowRect.bottom - DialogWindowRect.top,
			           1);
		}

		TabStops[0] = 100;
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);

		sprintf(str, SC(317, "Local geometry directory\t%s\\pcb\\shapes"), DesignPath);
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		sprintf(str, SC(318, "Global geometry directory\t%s\\shapes"), ProjectPath);
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);

		for (cnt = 0; cnt < NrGeometryLibFiles; cnt++)
		{
			if (cnt < NrGeometryLibraries)
				sprintf(str, SC(319, "Own geometry library\t%s"), GeometryLibNames[cnt]);
			else
				sprintf(str, SC(320, "Global geometry library\t%s"), GeometryLibNames[cnt]);

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		}

		sprintf(str, "%s\\pcb\\shapes", DesignPath);
		res = InitGeometriesEntries(Dialog, str);
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, 0, 0);

		if (res == 0)
		{
			sprintf(str, "%s\\shapes", ProjectPath);
			res = InitGeometriesEntries(Dialog, str);
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETCURSEL, 1, 0);
		}

		ActiveViewGeometry = 0;
		PostMessage(Dialog, WM_COMMAND, (WPARAM) ((LBN_SELCHANGE << 16) + IDC_LIST1), (LPARAM) NULL);
		res = 1;
		CurrentGeometryLibraryNr = -1;
		CurrentGeometryNrInLibrary = -1;
		GeomViewActive = 0;
		return about;

	case WM_MOVE:
		GetWindowRect(Dialog, &DialogWindowRect);
		GeometryDialogInitialX = DialogWindowRect.left;
		GeometryDialogInitialY = DialogWindowRect.top;
		res = 1;
		break;

	case WM_SIZE:
		res = 1;
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			Found = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
			Found2 = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
			LibMode = 0;

			if (ClosingWindowMessage != 0)
				SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

			if ((Found != -1) && (Found2 != -1))
			{
				SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETTEXT, (WPARAM) Found2, (LPARAM) str);
				strcpy(TempGeometrieName, str);
				EndDialog(Dialog, 1);
			}

			return about;

		case IDCANCEL:
			if (ClosingWindowMessage != 0)
				SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
//          Help("Comp_geometry,0);
			break;

		case IDC_BUTTON1:
			Found = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
			Found2 = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
			LibMode = 0;

			if ((Found != -1) && (Found2 != -1))
			{
//               &&
//               (Found2!=CurrentGeometryNrInLibrary)) {
//              CurrentGeometryNrInLibrary=Found2;
				SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETTEXT, (WPARAM) Found2, (LPARAM) str);

				switch (Found)
				{
				case 0:
				case 1:
					switch (Found)
					{
					case 0:
						sprintf(str2, "%s\\pcb\\shapes\\%s.shp", DesignPath, str);
						break;

					case 1:
						sprintf(str2, "%s\\shapes\\%s.shp", ProjectPath, str);
						break;
					}

					break;

				default:
					if (Found - 2 < NrGeometryLibFiles)
					{
						Found -= 2;
						sprintf(str2, "%s", GeometryLibNames[Found]);
						LibMode = 1;
					}

					break;
				}

				if (LibMode == 0)
					strcpy(str, str2);
				else
					res = 1;

				InitStr[0] = 0;

				if (ProjectActive)
					strcpy(InitStr, "/o");

				if (FileExistsUTF8(GeometrieEditor) != 0)
					break;

				if (LibMode == 0)
				{
					if (FileExistsUTF8(str) == 0)
					{
						sprintf(ExeParams, "\"%s\" \"%s\" /S %i,%i,%i,%i %s /a /e \"%s\" /w %x", GeometrieEditor, str,
						        GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY, InitStr, ExePath,
						        (int32) PCBWindow);

						if (ClosingWindowMessage != 0)
						{
							SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

							StartupInfo.cb = sizeof(StartupInfo);
							StartupInfo.wShowWindow = SW_SHOW;
							CreateProcess(GeometrieEditor, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo,
							              &ProcessInfo);

//                  WinExec(str3,SW_SHOW);
							GeomViewActive = 1;
						}
					}
					else
					{
						sprintf(str3, SC(321, "Can not find geometry %s"), str);
						MessageBoxOwn(PCBWindow, str3, SC(24, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}
				}
				else
				{
					sprintf(ExeParams, "\"%s\" \"%s\" /l \"%s\" /S %i,%i,%i,%i %s /a /e \"%s\" /w %x", GeometrieEditor,
					        str, str2, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY, InitStr, ExePath,
					        (int32) PCBWindow);

					if (ClosingWindowMessage != 0)
					{
						SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

						StartupInfo.cb = sizeof(StartupInfo);
						StartupInfo.wShowWindow = SW_SHOW;
						CreateProcess(GeometrieEditor, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo,
						              &ProcessInfo);

//                WinExec(str3,SW_SHOW);
						GeomViewActive = 1;
					}
				}

				ok = 1;
			}

			break;

		case IDC_LIST1:
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				Found2 = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

				if ((Found2 != -1) && (CurrentGeometryLibraryNr != Found2))
				{
					CurrentGeometryLibraryNr = Found2;
					CurrentGeometryNrInLibrary = -1;

					switch (Found2)
					{
					case 0:
					case 1:
						switch (Found2)
						{
						case 0:
							sprintf(str2, "%s\\pcb\\shapes", DesignPath);
							break;

						case 1:
							sprintf(str2, "%s\\shapes", ProjectPath);
							break;
						}

						SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);
						InitGeometriesEntries(Dialog, str2);
						break;

					default:
						if (Found2 - 2 < NrGeometryLibFiles)
						{
							Found2 -= 2;
							strcpy(str2, GeometryLibNames[Found2]);
						}

						SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);
						InitGeometryLibEntries(Dialog, str2);
						break;
					}
				}
			}

			hulp = 1;
			break;

		case IDC_LIST2:
			if ((HIWORD(WParam) == LBN_SELCHANGE) && (GeomViewActive))
			{
				Found = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
				Found2 = (int32) SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
				LibMode = 0;

				if ((Found != -1) && (Found2 != -1))
				{
					//               &&
					//               (Found2!=CurrentGeometryNrInLibrary)) {
					//              CurrentGeometryNrInLibrary=Found2;
					SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETTEXT, (WPARAM) Found2, (LPARAM) str);

					switch (Found)
					{
					case 0:
					case 1:
						switch (Found)
						{
						case 0:
							sprintf(str2, "%s\\pcb\\shapes\\%s.shp", DesignPath, str);
							break;

						case 1:
							sprintf(str2, "%s\\shapes\\%s.shp", ProjectPath, str);
							break;
						}

						break;

					default:
						if (Found - 2 < NrGeometryLibFiles)
						{
							Found -= 2;
							sprintf(str2, "%s", GeometryLibNames[Found]);
							LibMode = 1;
						}

						break;
					}

					if (LibMode == 0)
						strcpy(str, str2);
					else
						res = 1;

					InitStr[0] = 0;

					if (ProjectActive)
						strcpy(InitStr, "/o");

					if (FileExistsUTF8(GeometrieEditor) != 0)
						break;

					if (LibMode == 0)
					{
						if (FileExistsUTF8(str) == 0)
						{
							sprintf(ExeParams, "\"%s\" \"%s\" /S %i,%i,%i,%i %s /a /e \"%s\" /w %x", GeometrieEditor,
							        str, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY, InitStr, ExePath,
							        (int32) PCBWindow);

							if (ClosingWindowMessage != 0)
							{
								SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

								StartupInfo.cb = sizeof(StartupInfo);
								StartupInfo.wShowWindow = SW_SHOW;
								CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo,
								              &ProcessInfo);

//                    WinExec(str3,SW_SHOW);
							}
						}
						else
						{
							sprintf(str3, SC(321, "Can not find geometry %s"), str);
							MessageBoxOwn(PCBWindow, str3, SC(24, "Error"), MB_APPLMODAL | MB_OK);
							return about;
						}
					}
					else
					{
						sprintf(ExeParams, "\"%s\" \"%s\" /l \"%s\" /S %i,%i,%i,%i %s /a /e \"%s\" /w %x",
						        GeometrieEditor, str, str2, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY,
						        InitStr, ExePath, (int32) PCBWindow);

						if (ClosingWindowMessage != 0)
						{
							SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

							StartupInfo.cb = sizeof(StartupInfo);
							StartupInfo.wShowWindow = SW_SHOW;
							CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
//                  WinExec(str3,SW_SHOW);
						}
					}

					ok = 1;
				}
			}

			break;
		}

		break;

	case WM_PARENTNOTIFY:
		hulp = 1;
		break;
	}

	about = 0;
	return about;
}

int32 SelectGeometrie(LPSTR GeometrieName)
{
	int32 res, ok, cnt;
	char str[MAX_LENGTH_STRING];
	char str2[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

//  sprintf(GeometrieEditor,"E:\\new\\geom\\Debug\\geom.exe");
	sprintf(GeometrieEditor, "%s\\geom.exe", ExePath);
	memset(&GeometryLibNames, 0, sizeof(GeometryLibNames));

	// Search in own libraries first
	NrGeometryLibFiles = 0;

	for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
	{
		if (NrGeometryLibFiles < 64)
			strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt]);
	}

	sprintf(str, "%s\\shplib\\*.slb", ProjectPath);

	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while ((res) && (NrGeometryLibFiles < 64))
	{
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		sprintf(GeometryLibNames[NrGeometryLibFiles++], "%s\\shplib\\%s", ProjectPath, str2);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_EDITGEOMETRY), PCBWindow,
	                 (DLGPROC) SelectGeomDialog2);

	if (ActiveViewGeometry)
	{
		ActiveViewGeometry = 0;
		ViewFull();
	}

	if (res == 1)
	{
		if (TempGeometrieName[0] != 0)
			strcpy(GeometrieName, TempGeometrieName);
	}

	ok = 1;
	return res;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK BackAnnotationDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, fp;
	char str[MAX_LENGTH_STRING], buf[16384];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(326, "Back annotation"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(323, "Current back annotations"));

		SetDialogItemTextUTF8(Dialog, IDAPPEND, SC(324, "Append to current back annotations"));
		SetDialogItemTextUTF8(Dialog, IDDELETE, SC(325, "Remove current back annotations"));

		sprintf(str, "%s\\pcb\\gatepin.ban", DesignPath);

		if (FileExistsUTF8(str) == 0)
		{
			memset(buf, 0, sizeof(buf));
			fp = FileOpenReadOnlyUTF8(str);
			FileRead(fp, &buf, 16383, &res);
			FileClose(fp);
			SendDlgItemBigMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) (LPSTR) buf);
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDDELETE:
			EndDialog(Dialog, 2);
			return about;

		case IDAPPEND:
			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 0);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 BackAnnotationDialog(int32 mode)
{
	int32 res;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_BACK_ANNOTATE), PCBWindow,
	                 (DLGPROC) BackAnnotationDialogBody);
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetCompString(CompRecord * Comp, LPSTR str)
{
	switch (CompSortMode)
	{
	case 0:
		if ((Comp->CompMode & 8) == 0)
			sprintf(str, SC(1264, "%s\tTop\t%s\t%s\t%s"), Comp->Name, Comp->Value, Comp->ShapeName, Comp->PartNr);
		else
			sprintf(str, SC(1265, "%s\tBottom\t%s\t%s\t%s"), Comp->Name, Comp->Value, Comp->ShapeName, Comp->PartNr);

		break;

	case 1:
		if ((Comp->CompMode & 8) == 0)
			sprintf(str, SC(1266, "%s\t%s\tTop\t%s\t%s"), Comp->ShapeName, Comp->Name, Comp->Value, Comp->PartNr);
		else
			sprintf(str, SC(1267, "%s\t%s\tBottom\t%s\t%s"), Comp->ShapeName, Comp->Name, Comp->Value, Comp->PartNr);

		break;

	case 2:
		if ((Comp->CompMode & 8) == 0)
			sprintf(str, SC(1266, "%s\t%s\tTop\t%s\t%s"), Comp->Value, Comp->Name, Comp->ShapeName, Comp->PartNr);
		else
			sprintf(str, SC(1267, "%s\t%s\tBottom\t%s\t%s"), Comp->Value, Comp->Name, Comp->ShapeName, Comp->PartNr);

		break;

	case 3:
		if ((Comp->CompMode & 8) == 0)
			sprintf(str, SC(1266, "%s\t%s\tTop\t%s\t%s"), Comp->PartNr, Comp->Name, Comp->Value, Comp->ShapeName);
		else
			sprintf(str, SC(1267, "%s\t%s\tBottom\t%s\t%s"), Comp->PartNr, Comp->Name, Comp->Value, Comp->ShapeName);

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ComponentSelectionFunction(HWND Dialog, int32 mode, int32 IndexNr)
{
	CompRecord *Comp;
	int32 cnt, res, res2, cnt2, TabStops[5];
	char str[MAX_LENGTH_STRING], LayerString[MAX_LENGTH_STRING], PartNr[MAX_LENGTH_STRING], Name[MAX_LENGTH_STRING],
	     ShapeName[MAX_LENGTH_STRING], Value[MAX_LENGTH_STRING];

	switch (mode)
	{
	case 0:
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
		SendDlgItemMessage(Dialog, IDC_LIST3, LB_RESETCONTENT, 0, 0);

		switch (CompSortMode)
		{
		case 0:
			strcpy(str, SC(1258, "Ref\tTop/Bottom\tValue\tGeometry\tPartnr"));
			TabStops[0] = 30;
			TabStops[1] = 80;
			TabStops[2] = 240;
			break;

		case 1:
			strcpy(str, SC(1259, "Geometry\tRef\tTop/Bottom\tValue\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 220;
			break;

		case 2:
			strcpy(str, SC(1260, "Value\tRef\tTop/Bottom\tGeometry\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 220;
			break;

		case 3:
			strcpy(str, SC(1261, "Partnr\tRef\tTop/Bottom\tValue\tGeometry"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 220;
			break;
		}

		TabStops[3] = 380;
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) str);
		NrUsedComps = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
			{
				if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					Comp->Info |= OBJECT_DONE;
				else
					Comp->Info &= ~OBJECT_DONE;
			}

			NrUsedComps++;
		}

		if (NrUsedComps > 100)
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_INITSTORAGE, NrUsedComps, NrUsedComps * 240);

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
			{
				GetCompString(Comp, str);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
			}
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE | COMPONENT_PROTECTED)) == OBJECT_DONE)
			{
				GetCompString(Comp, str);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRING, (WPARAM) - 1, (LPARAM) str);

				if (res != LB_ERR)
					res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
			}
		}

		break;

	case 1:
		for (cnt = 0; cnt < NrUsedComps; cnt++)
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, cnt);

		break;

	case 2:
		for (cnt = 0; cnt < NrUsedComps; cnt++)
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 0, cnt);

		break;

	case 3:
		for (cnt2 = 0; cnt2 < Design.NrComps; cnt2++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt2]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
				Comp->Info &= ~OBJECT_SELECTED;
		}

		for (cnt = 0; cnt < NrUsedComps; cnt++)
		{
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0);

			if (res > 0)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str);
				res2 = 0;
				Name[0] = 0;
				ShapeName[0] = 0;
				PartNr[0] = 0;
				Value[0] = 0;

				switch (CompSortMode)
				{
				case 0:
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Name, LayerString, Value, ShapeName, PartNr);
					break;

				case 1:
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", ShapeName, Name, LayerString, Value, PartNr);
					break;

				case 2:
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Value, Name, LayerString, ShapeName, PartNr);
					break;

				case 3:
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", PartNr, Name, LayerString, Value, ShapeName);

					if (res2 == 4)
						res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, LayerString, Value, ShapeName);

					break;
				}

				if (res2 >= 4)
				{
					for (cnt2 = 0; cnt2 < Design.NrComps; cnt2++)
					{
						Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt2]]);

						if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
						{
							if (stricmpUTF8(Comp->Name, Name) == 0)
								Comp->Info |= OBJECT_SELECTED;
						}
					}
				}
			}
		}

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
			{
				Comp->Info &= ~OBJECT_DONE;

				if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					cnt2++;
			}
		}

		if (cnt2 == 0)
			return -1;

		break;

	case 4:
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
				Comp->Info &= ~OBJECT_DONE;
		}

		for (cnt = 0; cnt < NrUsedComps; cnt++)
		{
			str[0] = 0;
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str);
			res2 = 0;
			Name[0] = 0;
			ShapeName[0] = 0;
			PartNr[0] = 0;
			Value[0] = 0;

			switch (OldCompSortMode)
			{
			case 0:
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Name, LayerString, Value, ShapeName, PartNr);
				break;

			case 1:
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", ShapeName, Name, LayerString, Value, PartNr);
				break;

			case 2:
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Value, Name, LayerString, ShapeName, PartNr);
				break;

			case 3:
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", PartNr, Name, LayerString, Value, ShapeName);

				if (res2 == 4)
					res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, LayerString, Value, ShapeName);

				break;
			}

			if (res2 >= 3)
			{
				for (cnt2 = 0; cnt2 < Design.NrComps; cnt2++)
				{
					Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt2]]);

					if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
					{
						if (stricmpUTF8(Comp->Name, Name) == 0)
						{
							res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0);

							if (res > 0)
								Comp->Info |= OBJECT_DONE;
						}
					}
				}
			}
		}

		SendDlgItemMessage(Dialog, IDC_LIST3, LB_RESETCONTENT, 0, 0);
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);

		switch (CompSortMode)
		{
		case 0:
			strcpy(str, SC(1258, "Ref\tTop/Bottom\tValue\tGeometry\tPartnr"));
			TabStops[0] = 30;
			TabStops[1] = 80;
			TabStops[2] = 240;
			break;

		case 1:
			strcpy(str, SC(1259, "Geometry\tRef\tTop/Bottom\tValue\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 220;
			break;

		case 2:
			strcpy(str, SC(1260, "Value\tRef\tTop/Bottom\tGeometry\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 220;
			break;

		case 3:
			strcpy(str, SC(1261, "Partnr\tRef\tTop/Bottom\tValue\tGeometry"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 220;
			break;
		}

		TabStops[3] = 380;
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) str);

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
			{
				GetCompString(Comp, str);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
			}
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED | OBJECT_DONE)) == OBJECT_DONE)
			{
				GetCompString(Comp, str);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRING, (WPARAM) - 1, (LPARAM) str);

				if (res != LB_ERR)
					res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
			}
		}

		break;

	case 5:
		if ((IndexNr < 0) || (IndexNr >= NrUsedComps))
			break;

		res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETTEXT, IndexNr, (LPARAM) str);
		res2 = 0;
		Name[0] = 0;
		ShapeName[0] = 0;
		PartNr[0] = 0;
		Value[0] = 0;

		switch (CompSortMode)
		{
		case 0:
			res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Name, LayerString, Value, ShapeName, PartNr);
			break;

		case 1:
			res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", ShapeName, Name, LayerString, Value, PartNr);
			break;

		case 2:
			res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Value, Name, LayerString, ShapeName, PartNr);
			break;

		case 3:
			res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", PartNr, Name, LayerString, Value, ShapeName);

			if (res2 == 4)
				res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, LayerString, Value, ShapeName);

			break;
		}

		if (res2 >= 4)
		{
			for (cnt2 = 0; cnt2 < Design.NrComps; cnt2++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt2]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_PROTECTED)) == 0)
				{
					if (stricmpUTF8(Comp->Name, Name) == 0)
					{
						strcpy(SearchForReferenceString, Comp->Name);
						PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_CENTER_ON_COMPONENT, (LPARAM) 4);
					}
				}
			}
		}

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK ComponentSelectionDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res, SelectedComponents[10];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(1257, "Sort by"));
		
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(1268, "Move components"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(1269, "Center screen on component"));

		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_DESELECTALL, SC(161, "Deselect all"));

		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETHORIZONTALEXTENT, 1200, 0);
		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(848, "Reference"));
		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(1262, "Geometry"));
		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(192, "Value"));
		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(1263, "Partnr"));
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETCURSEL, CompSortMode, 0);
		OldCompSortMode = CompSortMode;
		ComponentSelectionFunction(Dialog, 0, 0);
		SetWindowTextUTF8(Dialog, SC(1246, "Component selections"));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			ComponentSelectionFunction(Dialog, 3, 0);
			EndDialog(Dialog, 1);
			return about;

		case IDC_BUTTON1:
			if (ComponentSelectionFunction(Dialog, 3, 0) == 0)
				EndDialog(Dialog, 2);
			else
				EndDialog(Dialog, 1);

			return about;

		case IDC_BUTTON2:
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, 2, (LPARAM) & SelectedComponents);

			if (res == 1)
				ComponentSelectionFunction(Dialog, 5, SelectedComponents[0]);

			break;

		case IDC_LIST2:
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);

				if (res != LB_ERR)
				{
					CompSortMode = res;
					ComponentSelectionFunction(Dialog, 4, 0);
					OldCompSortMode = CompSortMode;
				}
			}

			break;

		case IDD_DESELECTALL:
			ComponentSelectionFunction(Dialog, 2, 0);
			break;

		case IDD_SELECTALL:
			ComponentSelectionFunction(Dialog, 1, 0);
			break;

		case IDHELP:
			Help("compSelection.htm", 0);
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

int32 ComponentSelectionDialog(int32 mode)
{
	int32 res;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COMP_SEL), PCBWindow,
	                 (DLGPROC) ComponentSelectionDialogBody);
	RePaint();

	if (res == 2)
		PostMessage(PCBWindow, WM_COMMAND, ID_MOVE_OBJECTS, 0);

	return res;
}

//*******************************************************************************************************************
//*************************************** Export pozice komponent  **************************************************
//*******************************************************************************************************************

int32 CALLBACK CompPositionOutputDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(814, "Units"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(1277, "Component value/part number option"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(1278, "SMD/THT components"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(1279, "Special options"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, "SMD");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, "SMD/THT");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(192, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(1280, "Part number"));

		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(1282, "Add extra column for not placed components"));

		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "thou");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "mm");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(817, "inch"));

		switch (CompPosOutput.Units & 3)
		{
		case 0:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, 0, 0);
			break;

		case 1:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, 1, 0);
			break;

		case 2:
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, 2, 0);
			break;
		}

		switch (CompPosOutput.SmdThroughHole)
		{
		case 0:				// Only SMD
			SendDlgItemMessage(Dialog, IDC_RADIO1, BM_SETCHECK, BST_CHECKED, 0);
			SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, BST_UNCHECKED, 0);
			break;

		case 1:				// SMD and THT
			SendDlgItemMessage(Dialog, IDC_RADIO1, BM_SETCHECK, BST_UNCHECKED, 0);
			SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, BST_CHECKED, 0);
			break;
		}

		if (CompPosOutput.ValuePartNr == 0)
		{
			SendDlgItemMessage(Dialog, IDC_RADIO4, BM_SETCHECK, BST_UNCHECKED, 0);
			SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, BST_CHECKED, 0);
		}
		else
		{
			SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, BST_UNCHECKED, 0);
			SendDlgItemMessage(Dialog, IDC_RADIO4, BM_SETCHECK, BST_CHECKED, 0);
		}

		if (CompPosOutput.NotPlaced)
			SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, BST_CHECKED, 0);
		else
			SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, BST_UNCHECKED, 0);

		SetWindowTextUTF8(Dialog, SC(496, "Export component positions"));

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			CompPosOutput.Units = 0;
			CompPosOutput.ValuePartNr = 0;
			CompPosOutput.NotPlaced = 0;
			CompPosOutput.SmdThroughHole = 0;

			if (SendDlgItemMessage(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0))
			{
			}

			if (SendDlgItemMessage(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0))
				CompPosOutput.SmdThroughHole = 1;

			if (SendDlgItemMessage(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0))
			{
			}

			if (SendDlgItemMessage(Dialog, IDC_RADIO4, BM_GETCHECK, 0, 0))
				CompPosOutput.ValuePartNr = 1;

			if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0))
				CompPosOutput.NotPlaced = 1;

			CompPosOutput.Units = SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0) & 3;
			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("export_component_positions.htm", 0);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 0);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

//****************************************************************************************************************
//****************************************************************************************************************
//****************************************************************************************************************
