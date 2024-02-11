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
#include "memory.h"
#include "commctrl.h"
#include "dialogs.h"
#include "stdlib.h"
#include "stdio.h"
#include "graphics.h"
#include "mainloop.h"
#include "select.h"
#include "resource.h"
#include "sch.h"
#include "io.h"
#include "files.h"
#include "files2.h"
#include "help.h"
#include "draw2.h"
#include "edit2.h"
#include "string.h"
#include "inscomp.h"
#include "insdel.h"
#include "utf8.h"
#include "calc2.h"
#include "richedit.h"
#include "ctype.h"
#include "../functionsc/version.h"

#define GeometryLibraryCode1        "Geometry library version 1.0"

typedef LibNameRecord LibNameArray[2000];

typedef struct
{
	int32 Info, Info1, Info2, Code1, Code2;
	DWORD niks2;
} DLGTEMPLATE2;

typedef struct
{
	DWORD niks1;
	DWORD dwExtendedStyle;
	DWORD style;
	short x;
	short y;
	short cx;
	short cy;
	int32 id;
} DLGITEMTEMPLATE2;

typedef struct
{
	int32 LibNr;
	int32 Mode;
	char SymbolName[24];
} SymbolEntryRecord;

typedef SymbolEntryRecord SymbolEntriesArray[4000];



LPSTR InitDialogTextLine, DialogWindowText, MessagePtr;

char *NetLabelText, GeometrieName[MAX_LENGTH_STRING], CodeString[16][8], PinString[16][128];
char DialogTextLine[MAX_LENGTH_STRING];
PinRecord AddedPins[200];
PowerPinRecord AddedPowerPin;
PinBusRecord AddedPinBus;
PinBusRecord AddedPinBus;
RedefinedPinBusRecord NewRedefinedPinBus;
ObjectTextRecord AddedPinText[200], SymbolText;
int32 OkToAddSymbol, ok;
int32 NrAddPins, hulp, DialogMode, NumberStart, NumberStep, NumberCount, CurrentSelection;
int32 SymbolDialogInitialX = -10000;
int32 SymbolDialogInitialY = -10000;
int32 GeometryDialogInitialX = -10000;
int32 GeometryDialogInitialY = -10000;
int32 ComponentDialogInitialX = -10000;
int32 ComponentDialogInitialY = -10000;
int32 RemoveTreeSelection;
double AddPinDialogDivX = 0.0;
double AddPinDialogDivY = -1.0;
InstanceRecord *WorkingInstance;
SymbolRecord *WorkingSymbol;
LPSTR TempGeometrieName;
int32 ActiveViewGeometry = 0;
HWND SymbolDialog = NULL;
HWND ComponentDialog = NULL;
RECT DialogWindowRect;

int32 NrGeometryLibFiles, CurrentGeometryLibraryNr, CurrentGeometryNrInLibrary;
int32 NrTryingSymbols;
int32 CompSortMode, OldCompSortMode, NrUsedComps;
char GeometryLibNames[64][MAX_LENGTH_STRING], SearchString2[MAX_LENGTH_STRING];
WCHAR SearchString2W[MAX_LENGTH_STRING];
extern int32 SelectedColorNr, InstCnt, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY;

extern ObjectNumbersRecord ObjectNumbers;
extern int32 ProjectActive, NrCompSelects;
extern ProjectInfoRecord *ProjectInfo;

STARTUPINFO StartupInfo;

int32 DialogStartNr, DialogStep, DialogAddY, LastPinConnectionType, LastPinType;
int32 TempUnits;

/*
WNDCLASS                    DialogClass = {0,
                                          (WNDPROC)DialogWinProc,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          "DIALOG"};
*/

int32 NetDialogMode = 0;
int32 NetsSelected, DialogResult;
char SelectedLibName[32], SelectedSymbolName[32];

ObjectTextRecord *WorkingObjectText, OldObjectText;
PinRecord *WorkingPin;
PowerPinRecord *WorkingPowerPin;
PinBusRecord *WorkingPinBus;
NetLabelRecord *WorkingNetLabel;

COLORREF GetNewColor(int32 mode, COLORREF InitialColor, HWND CurrentWindow);



int32 SearchGeometrie(void);

int32 SelectGeometrie(void);


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SetDialogItemText(HWND Dialog, int32 DlgItem, LPSTR Text)
{
	SendDlgItemMessageUTF8(Dialog, DlgItem, WM_SETTEXT, 0, (LPARAM) Text);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetDialogValue(HWND Dialog, int32 Control, double value)
{
	char str[MAX_LENGTH_STRING];

	if (TempUnits == 0)
		sprintf(str, "%.1f", value / 2540.0);
	else
		sprintf(str, "%.4f", value / 100000.0);

	SendDlgItemMessage(Dialog, Control, WM_SETTEXT, 0, (LPARAM) str);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetDialogFloatValue(HWND Dialog, int32 Control, double value, int32 Resolution)
{
	char str[MAX_LENGTH_STRING];

	switch (Resolution)
	{
	case 1:
		sprintf(str, "%.1f", value);
		break;

	case 2:
		sprintf(str, "%.2f", value);
		break;

	case 3:
		sprintf(str, "%.3f", value);
		break;

	case 4:
		sprintf(str, "%.4f", value);
		break;

	case 5:
		sprintf(str, "%.5f", value);
		break;

	case 6:
		sprintf(str, "%.6f", value);
		break;

	default:
		sprintf(str, "%.7f", value);
		break;
	}

	SendDlgItemMessage(Dialog, Control, WM_SETTEXT, 0, (LPARAM) str);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetDialogIntValue(HWND Dialog, int32 Control, int32 value)
{
	char str[MAX_LENGTH_STRING];

	sprintf(str, "%d", value);
	SendDlgItemMessage(Dialog, Control, WM_SETTEXT, 0, (LPARAM) str);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double GetDialogValue(HWND Dialog, int32 Control)
{
	char str[MAX_LENGTH_STRING];
	float value;

	str[0] = 0;
	value = (double) 0.0;
	SendDlgItemMessage(Dialog, Control, WM_GETTEXT, 80, (LPARAM) str);

	if (sscanf(str, "%f", &value) == 1)
	{
		if (TempUnits == 0)
			value *= (double) 2540.0;
		else
			value *= (double) 100000.0;
	}

	return value;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDialogIntValue(HWND Dialog, int32 Control, int32 * Value)
{
	char str[MAX_LENGTH_STRING];
	int32 NewValue;

	str[0] = 0;
	SendDlgItemMessage(Dialog, Control, WM_GETTEXT, 80, (LPARAM) str);

	if (sscanf(str, "%d", &NewValue) == 1)
	{
		*Value = NewValue;
		return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDialogFloatValue(HWND Dialog, int32 Control, double *Value)
{
	char str[MAX_LENGTH_STRING];
	float NewValue;

	str[0] = 0;
	SendDlgItemMessage(Dialog, Control, WM_GETTEXT, 80, (LPARAM) str);

	if (sscanf(str, "%f", &NewValue) == 1)
	{
		*Value = NewValue;
		return 1;
	}

	return 0;
}

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

int32 GetDialogTextLine(HWND Dialog, int32 Control, int32 LineNr, LPSTR TextLine, int32 MaxLength)
{
	uint16 *Lengte;
	int32 res;
	Lengte = (uint16 *) TextLine;
	memset(TextLine, 0, MaxLength);
	*Lengte = (uint16) (MaxLength - 1);
	res = SendDlgItemMessageUTF8(Dialog, Control, EM_GETLINE, LineNr, (LPARAM) TextLine);
	TextLine[res] = 0;
	return res;
}

//*************************************************************************************************************************
//************** IDD_DIALOG_TEXTINPUT * IDD_DIALOG_TEXTINPUT2 * IDD_DIALOG_TEXTINPUT3 * IDD_DIALOG_TEXTINPUT4 *************
//*************************************************************************************************************************

int32 CALLBACK TextInputDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, TextChanged, res;
	double value = 0.0;
	float x, y;
	char str[MAX_LENGTH_STRING];
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(22, "Not visible"));

		switch (DialogMode)
		{
		case 0:
			SetWindowTextUTF8(Dialog, SC(23, "Add text")); //IDD_DIALOG_TEXTINPUT4
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(35, "Resize text (1.0 = standard)"));
			SetDialogFloatValue(Dialog, IDC_EDIT2, WorkingObjectText->FontHeight, 2);
			strcpy(OldObjectText.Text, WorkingObjectText->Text);
			OldObjectText.FontHeight = WorkingObjectText->FontHeight;
			break;

		case 1:
			SetWindowTextUTF8(Dialog, SC(24, "Change text")); //IDD_DIALOG_TEXTINPUT3
			strcpy(OldObjectText.Text, WorkingObjectText->Text);
			break;

		case 2:
			SetWindowTextUTF8(Dialog, SC(25, "Change reference")); //IDD_DIALOG_TEXTINPUT

			if ((WorkingObjectText->Info & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

			break;

		case 3:
			if (!EditingSymbol)
				SetWindowTextUTF8(Dialog, SC(26, "Change value")); //IDD_DIALOG_TEXTINPUT
			else
				SetWindowTextUTF8(Dialog, SC(27, "Change symbol name")); //IDD_DIALOG_TEXTINPUT

			if ((WorkingObjectText->Info & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

			break;

		case 4:
			SetWindowTextUTF8(Dialog, SC(28, "Add external connection")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 5:
			SetWindowTextUTF8(Dialog, SC(29, "Change external connection")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 6:
			SetWindowTextUTF8(Dialog, SC(316, "Search for text")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 7:
//          SetWindowTextUTF8(Dialog,"Can not find exe path, type the exe path"); //IDD_DIALOG_TEXTINPUT2
			break;

		case 8:
			SetWindowTextUTF8(Dialog, SC(31, "Add symbol on shortcut")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 9:
			SetWindowTextUTF8(Dialog, SC(32, "Line width (0.1 = standard)")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 10:
			SetWindowTextUTF8(Dialog, SC(333, "Rotate at any angle")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 11:
			SetWindowTextUTF8(Dialog, SC(34, "Scale (1.0 = standard)")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 12:
			SetWindowTextUTF8(Dialog, SC(35, "Resize text (1.0 = standard)")); //IDD_DIALOG_TEXTINPUT2
			break;

		case 13:
			SetWindowTextUTF8(Dialog, SC(24, "Change text")); //IDD_DIALOG_TEXTINPUT4
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(35, "Resize text (1.0 = standard)"));
			SetDialogFloatValue(Dialog, IDC_EDIT2, WorkingObjectText->FontHeight, 2);
			strcpy(OldObjectText.Text, WorkingObjectText->Text);
			OldObjectText.FontHeight = WorkingObjectText->FontHeight;
			break;

		case 14:
			SetWindowTextUTF8(Dialog, SC(467, "Enter coordinates x,y (example 13,45)")); //IDD_DIALOG_TEXTINPUT2
			break;
		}

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & (WorkingObjectText->Text));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			TextChanged = 0;
			memset(WorkingObjectText->Text, 0, sizeof(WorkingObjectText->Text));
			res =
			    SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, sizeof(WorkingObjectText->Text) - 1,
			                           (LPARAM) WorkingObjectText->Text);

			switch (DialogMode)
			{
			case 2:
			case 3:
				if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 1, 0) == 1)
					WorkingObjectText->Info |= TEXT_NOT_VISIBLE;
				else
					WorkingObjectText->Info &= ~TEXT_NOT_VISIBLE;

				break;

			case 0:
			case 13:
				GetDialogFloatValue(Dialog, IDC_EDIT2, &value);
				WorkingObjectText->FontHeight = (float) value;
				break;

			case 14:
				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, 80, (LPARAM) str);

				if ((res > 0) && (sscanf(str, "%f,%f", &x, &y) == 2))
				{
					WorkingObjectText->X = x;
					WorkingObjectText->Y = y;
				}
				else
				{
					MessageBoxUTF8(SCHWindow, SC(469, "Wrong coordinates for goto (x,y)"), SC(38, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				break;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			switch (DialogMode)
			{
			case 0:
				Help("add_text.htm", 0);
				break;

			case 4:
			case 5:
				Help("add_globalconnection.htm", 0);
				break;

			case 6:
				Help("search_for_any_text.htm", 0);
				break;

			case 8:
				Help("add_symbol_on_short_cut.htm", 0);
				break;
			}

			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 TextInputDialog(ObjectTextRecord * ObjectText, int32 Mode)
{
	int32 res, ok;

//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;
	DialogMode = Mode;
	WorkingObjectText = ObjectText;

	switch (DialogMode)
	{
	case 0:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT4), SCHWindow,
		               (DLGPROC) TextInputDialog2);
		break;

	case 1:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT3), SCHWindow,
		               (DLGPROC) TextInputDialog2);
		break;

	case 2:
	case 3:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT), SCHWindow,
			           (DLGPROC) TextInputDialog2);
		break;

	case 13:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT4), SCHWindow,
		               (DLGPROC) TextInputDialog2);
		break;

	case 14:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT2), SCHWindow,
		               (DLGPROC) TextInputDialog2);
		break;

	default:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT2), SCHWindow,
		               (DLGPROC) TextInputDialog2);
		break;
	}

//  strcpy(TextLine,DialogTextLine);
	ok = 1;
	return res;
}

//************************************************************************************************************************
//********************************* IDD_DIALOG_NRPARTS *******************************************************************
//************************************************************************************************************************

int32 CALLBACK PartNrDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, hulp = 0;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(36, "Total number of parts"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));

		SetDialogIntValue(Dialog, IDC_EDIT1, DesignSymbol.NrPartsPerPackage);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			if (GetDialogIntValue(Dialog, IDC_EDIT1, &hulp) == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(37, "Wrong value in nr parts"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (DialogMode == 0)
			{
				if (hulp != DesignSymbol.NrPartsPerPackage)
				{
					DesignSymbol.NrPartsPerPackage = hulp;
					DataBaseChanged = 1;
					EndDialog(Dialog, 1);
				}
				else
					EndDialog(Dialog, 2);
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

int32 PartNrDialog(int32 Mode)
{
	int32 res, ok;

//  InitDialogTextLine=TextLine;
	DialogMode = Mode;
	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NRPARTS), SCHWindow, (DLGPROC) PartNrDialog2);
//  strcpy(TextLine,DialogTextLine);
	ok = 1;
	return res;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SetConnectionRadioButtons(HWND Dialog, int32 ConnectionType, int32 mode)
{
	SendDlgItemMessage(Dialog, IDC_RADIO1, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO4, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO5, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO6, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO7, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO8, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO9, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO10, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO11, BM_SETCHECK, 0, 0);
	SendDlgItemMessage(Dialog, IDC_RADIO12, BM_SETCHECK, 0, 0);

	if (mode == 2)
		return;
	else
		SendDlgItemMessage(Dialog, IDC_RADIO13, BM_SETCHECK, 0, 0);

	if (ConnectionType == POWER_CONNECTION)
	{
		SendDlgItemMessage(Dialog, IDC_RADIO13, BM_SETCHECK, 1, 0);
		return;
	}

	switch (ConnectionType & 0xff)
	{
	case CONNECTION_INPUT:
		SendDlgItemMessage(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
		break;

	case CONNECTION_OUTPUT:
		SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
		break;

	case CONNECTION_IO:
		SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
		break;

	case CONNECTION_TRISTATE:
		SendDlgItemMessage(Dialog, IDC_RADIO4, BM_SETCHECK, 1, 0);
		break;

	case CONNECTION_OC:
		SendDlgItemMessage(Dialog, IDC_RADIO5, BM_SETCHECK, 1, 0);
		break;

	case CONNECTION_PASSIVE:
		SendDlgItemMessage(Dialog, IDC_RADIO6, BM_SETCHECK, 1, 0);
		break;

	case CONNECTION_POWER:
		SendDlgItemMessage(Dialog, IDC_RADIO7, BM_SETCHECK, 1, 0);
		break;
	}

	if (mode == 0)
	{
		switch (ConnectionType >> 8)
		{
		case CONNECTION_UNDEFINED:
			SendDlgItemMessage(Dialog, IDC_RADIO8, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_TTL:
			SendDlgItemMessage(Dialog, IDC_RADIO9, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_LVTTL:
			SendDlgItemMessage(Dialog, IDC_RADIO10, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_CMOS:
			SendDlgItemMessage(Dialog, IDC_RADIO11, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_ANALOG:
			SendDlgItemMessage(Dialog, IDC_RADIO12, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_POWER:
			SendDlgItemMessage(Dialog, IDC_RADIO13, BM_SETCHECK, 1, 0);
			break;
		}
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetConnectionRadioButtons(HWND Dialog, int32 mode)
{
	int32 LastPinConnectionType, LastPinType;

	if (SendDlgItemMessage(Dialog, IDC_RADIO13, BM_GETCHECK, 0, 0) == 1)
		return ((CONNECTION_POWER * 256) + CONNECTION_POWER);

	LastPinConnectionType = CONNECTION_PASSIVE;
	LastPinType = CONNECTION_UNDEFINED;

	if (SendDlgItemMessage(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0) == 1)
		LastPinConnectionType = CONNECTION_INPUT;

	if (SendDlgItemMessage(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0) == 1)
		LastPinConnectionType = CONNECTION_OUTPUT;

	if (SendDlgItemMessage(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0) == 1)
		LastPinConnectionType = CONNECTION_IO;

	if (SendDlgItemMessage(Dialog, IDC_RADIO4, BM_GETCHECK, 0, 0) == 1)
		LastPinConnectionType = CONNECTION_TRISTATE;

	if (SendDlgItemMessage(Dialog, IDC_RADIO5, BM_GETCHECK, 0, 0) == 1)
		LastPinConnectionType = CONNECTION_OC;

	if (SendDlgItemMessage(Dialog, IDC_RADIO6, BM_GETCHECK, 0, 0) == 1)
		LastPinConnectionType = CONNECTION_PASSIVE;

	if (SendDlgItemMessage(Dialog, IDC_RADIO7, BM_GETCHECK, 0, 0) == 1)
		LastPinConnectionType = CONNECTION_POWER;

	if (mode == 0)
	{
		if (SendDlgItemMessage(Dialog, IDC_RADIO8, BM_GETCHECK, 0, 0) == 1)
			LastPinType = CONNECTION_UNDEFINED;

		if (SendDlgItemMessage(Dialog, IDC_RADIO9, BM_GETCHECK, 0, 0) == 1)
			LastPinType = CONNECTION_TTL;

		if (SendDlgItemMessage(Dialog, IDC_RADIO10, BM_GETCHECK, 0, 0) == 1)
			LastPinType = CONNECTION_LVTTL;

		if (SendDlgItemMessage(Dialog, IDC_RADIO11, BM_GETCHECK, 0, 0) == 1)
			LastPinType = CONNECTION_CMOS;

		if (SendDlgItemMessage(Dialog, IDC_RADIO12, BM_GETCHECK, 0, 0) == 1)
			LastPinType = CONNECTION_ANALOG;
	}

	return ((LastPinType * 256) + LastPinConnectionType);
}

//********************************************************************************************************************
//************ IDD_DIALOG_ADDPINS * IDD_DIALOG_ADDPINS2A * IDD_DIALOG_ADDPINS3 * IDD_DIALOG_ADDPINS4 *****************
//********************************************************************************************************************

int32 CALLBACK AddPinsDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, PinChanged, PowerPin, cnt = 0, cnt2, res, StartNumber, IncNumber, NrOfPins, AutoNumbering;
	double divx, divy, OX, OY;
	char sel[MAX_LENGTH_STRING], LabelName[MAX_LENGTH_STRING],
	     PinTextName[MAX_LENGTH_STRING], NewPinTextName[MAX_LENGTH_STRING], PinName[MAX_LENGTH_STRING],
	     NewPinName[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING], NumberString[100];
#ifdef _DEBUG
	char sel2[MAX_LENGTH_STRING];
	char *weg;
#endif

	about = 1;
	StartNumber = 0;
	IncNumber = 0;
	NrOfPins = 0;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(39, "Pin Name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(40, "Pin Text"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(41, "Label"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(42, "Offset +-X"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(43, "Offset +-Y"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(44, "Connection"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(45, "Type"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(176, "Gate/pin swap"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(440, "Auto numbering"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(112, "Start number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(113, "Increment"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(79, "Nr pins"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(121, "Powernet"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC15, SC(439, "Postion increment"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(46, "Input"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(47, "Output"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(48, "Input/Output"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(49, "Tristate"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO5, SC(50, "Open collector"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO6, SC(51, "Passive"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO7, SC(52, "Power"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO8, SC(53, "Undefined"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO9, "TTL");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO10, "LVTTL");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO11, "CMOS");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO12, SC(54, "Analog"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO13, SC(55, "Pin name is powernet"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(56, "Pin name not visible"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(309, "Auto name"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(310, "Auto text"));

		switch (DialogMode)
		{
		case 0:
			SetWindowTextUTF8(Dialog, SC(57, "Add pin")); //Pøidat pin
			break;

		case 1:
			SetWindowTextUTF8(Dialog, SC(58, "Change pin")); //Zmìnit pin
			break;

		case 2:
			SetWindowTextUTF8(Dialog, SC(59, "Add pin label")); //Pøidat oznaèení pinu
			break;

		case 3:
			SetWindowTextUTF8(Dialog, SC(60, "Change pin label")); //Zmìnit oznaèení pinu
			break;
		}

		if (!EditingSheetSymbol)
			SetConnectionRadioButtons(Dialog, WorkingPin->ConnectionType, 0);
		else
			SetConnectionRadioButtons(Dialog, WorkingPin->ConnectionType, 1);

		PowerPin = 0;

		if (WorkingPin->ConnectionType == POWER_CONNECTION)
			PowerPin = 1;

		switch (DialogMode)
		{
		case 0:
		case 2:
			SetDialogFloatValue(Dialog, IDC_EDIT4, AddPinDialogDivX, 2);
			SetDialogFloatValue(Dialog, IDC_EDIT5, AddPinDialogDivY, 2);
			break;

		case 1:
			if ((WorkingPin->NameInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) WorkingPin->Name);

			if (PowerPin)
				SendDlgItemMessage(Dialog, IDC_EDIT3, EM_SETREADONLY, 1, 0);
			else
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) WorkingPin->Label);

#ifdef _DEBUG
			res = 0;
			memmove(&res, &WorkingPin->SwapInfo, 2);
			ultoa((uint32) res, DialogTextLine, 2);
			res = strlen(DialogTextLine);
			strcpy(sel2, "0000000000000000000");
			sel2[16 - res] = 0;
			sprintf(sel, "%s%s", sel2, DialogTextLine);
			SendDlgItemMessage(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) sel);
#endif
			break;

		case 3:
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) WorkingPin->Label);
			break;
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_RADIO1:
			res = HIWORD(WParam);

			switch (HIWORD(WParam))
			{
			case LBN_SELCHANGE:
				break;
			}

			break;

		case IDC_RADIO13:
			res = HIWORD(WParam);

			switch (HIWORD(WParam))
			{
			case BN_CLICKED:
				if (SendDlgItemMessage(Dialog, IDC_RADIO13, BM_GETCHECK, 0, 0) == 1)
				{
					SendDlgItemMessage(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) "");
					SendDlgItemMessage(Dialog, IDC_EDIT3, EM_SETREADONLY, 1, 0);
					SendDlgItemMessage(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) "");
					SendDlgItemMessage(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
					SetConnectionRadioButtons(Dialog, 0, 2);
				}
				else
				{
					SendDlgItemMessage(Dialog, IDC_EDIT2, EM_SETREADONLY, 0, 0);
					SendDlgItemMessage(Dialog, IDC_EDIT3, EM_SETREADONLY, 0, 0);
					SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) WorkingPin->Label);

					if (DialogMode < 2)
						SetConnectionRadioButtons(Dialog, 0, 0);
					else
						SetConnectionRadioButtons(Dialog, 0, 1);
				}

				break;
			}

			break;

		case IDOK:
			PinChanged = 0;

			if (!EditingSheetSymbol)
				LastPinConnectionType = GetConnectionRadioButtons(Dialog, 0);
			else
				LastPinConnectionType = GetConnectionRadioButtons(Dialog, 1);

			PowerPin = 0;

			if (LastPinConnectionType == POWER_CONNECTION)
				PowerPin = 1;

// ********************************************************************************************
// ********************************************************************************************

			switch (DialogMode)
			{
			case 0:
				if (GetDialogFloatValue(Dialog, IDC_EDIT4, &AddPinDialogDivX) == 0)
				{
					MessageBoxUTF8(SCHWindow, SC(61, "Wrong value in offset +X"), SC(38, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				if (GetDialogFloatValue(Dialog, IDC_EDIT5, &AddPinDialogDivY) == 0)
				{
					MessageBoxUTF8(SCHWindow, SC(62, "Wrong value in offset +Y"), SC(38, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				if ((AddPinDialogDivX == 0.0) && (AddPinDialogDivY == 0.0))
					AddPinDialogDivY = -1.0;

				OX = 0.0;
				OY = 0.0;
				divx = AddPinDialogDivX;
				divy = AddPinDialogDivY;
				NrAddPins = 0;

				for (cnt = 0; cnt < 25; cnt++)
				{
					res = GetDialogTextLine(Dialog, IDC_EDIT1, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

					if (res > 0)
					{

						// ******************************************************************************************
						while ((res > 0) && (DialogTextLine[res - 1] == ' '))
						{
							DialogTextLine[res - 1] = 0;
							res--;
						}

						if (CheckPinName(DialogTextLine, PowerPin) == -1)
						{
							sprintf(sel, SC(63, "Pin name ( %s ) is not valid"), DialogTextLine);
							MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
							return about;
						}

						AddedPins[NrAddPins].X = (float) OX;
						AddedPins[NrAddPins].Y = (float) OY;
						AddedPins[NrAddPins].NameX = (float) (OX + 1.0);
						AddedPins[NrAddPins].NameY = (float) OY;
						AddedPins[NrAddPins].ConnectionType = (int16) LastPinConnectionType;
						memset(&AddedPinText[NrAddPins], 0, sizeof(ObjectTextRecord));
						memmove(&AddedPins[NrAddPins].Name, &DialogTextLine, sizeof(AddedPins[NrAddPins].Name) - 1);

						// ******************************************************************************************

						memset(&PinTextName, 0, MAX_LENGTH_STRING - 50);
						res = GetDialogTextLine(Dialog, IDC_EDIT2, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

						while ((res > 0) && (DialogTextLine[res - 1] == ' '))
						{
							DialogTextLine[res - 1] = 0;
							res--;
						}

						strcpy(PinTextName, DialogTextLine);

						// ******************************************************************************************

						memset(&LabelName, 0, MAX_LENGTH_STRING - 50);

						if (!PowerPin)
						{
							res = GetDialogTextLine(Dialog, IDC_EDIT3, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

							while ((res > 0) && (DialogTextLine[res - 1] == ' '))
							{
								DialogTextLine[res - 1] = 0;
								res--;
							}

							strcpy(LabelName, DialogTextLine);
						}

						// ******************************************************************************************

						if (LabelName[0] == 0)
						{
							if (!PowerPin)
							{
								if (PinTextName[0] == 0)
								{
									LabelName[0] = '_';
									strcat(LabelName, AddedPins[NrAddPins].Name);
								}
								else
								{
									PinTextName[sizeof(AddedPinText[0].Text) - 1] = 0;

									if (CheckLabelName(PinTextName) == -1)
									{
										sprintf(sel, SC(64, "Pin text ( %s ) is not valid for use as a label name"),
											    PinTextName);
										MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
										return about;
									}

									strcpy(AddedPinText[NrAddPins].Text, PinTextName);
									strcpy(LabelName, PinTextName);
								}
							}
							else
								strcpy(LabelName, AddedPins[NrAddPins].Name);
						}
						else
						{
							PinTextName[sizeof(AddedPinText[0].Text) - 1] = 0;
							strcpy(AddedPinText[NrAddPins].Text, PinTextName);

							if (CheckLabelName(LabelName) == -1)
							{
								sprintf(sel, SC(65, "Label name ( %s ) is not valid"), LabelName);
								MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
								return about;
							}
						}

						AddedPinText[NrAddPins].X = (float) (OX + 4.5);
						AddedPinText[NrAddPins].Y = (float) OY;
						AddedPinText[NrAddPins].FontHeight = (float) 1.0;
						AddedPinText[NrAddPins].TextMode = ALIGN_LEFT_CENTRE;
						AddedPinText[NrAddPins].Thickness = (float) 0.1;
						memset(&AddedPins[NrAddPins].Label, 0, sizeof(((PinRecord*)0)->Label));
						memmove(&AddedPins[NrAddPins].Label, &LabelName, sizeof(((PinRecord*)0)->Label) - 1);
						OX += divx;
						OY += divy;
						NrAddPins++;
					}
				}
				

				EndDialog(Dialog, 1);
				break;

// ********************************************************************************************
// ********************************************************************************************
			case 1:
				res = GetDialogTextLine(Dialog, IDC_EDIT1, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

				if (DialogTextLine[0] == 0)
					MessageBoxUTF8(SCHWindow, SC(66, "Pin should have a name"), SC(38, "Error"), MB_APPLMODAL | MB_OK);

				// ******************************************************************************************

				if (CheckPinName(DialogTextLine, PowerPin) == -1)
				{
					sprintf(sel, SC(63, "Pin name ( %s ) is not valid"), DialogTextLine);
					MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				memset(PinName, 0, MAX_LENGTH_STRING - 50);
				strcpy(PinName, DialogTextLine);

				// ******************************************************************************************

				memset(&LabelName, 0, MAX_LENGTH_STRING - 50);

				if (!PowerPin)
				{
					res = GetDialogTextLine(Dialog, IDC_EDIT3, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

					if (res > 0)
						strcpy(LabelName, DialogTextLine);
				}

				// ******************************************************************************************

				if (LabelName[0] == 0)
				{
					if (!PowerPin)
					{
						LabelName[0] = '_';
						strcat(LabelName, PinName);
					}
					else
						strcpy(LabelName, PinName);
				}
				else
				{
					if (CheckLabelName(LabelName) == -1)
					{
						MessageBoxUTF8(SCHWindow, SC(67, "Label name is not valid"), SC(38, "Error"),
						               MB_APPLMODAL | MB_OK);
						return about;
					}
				}

				// ******************************************************************************************

				if (WorkingPin->ConnectionType != LastPinConnectionType)
				{
					WorkingPin->ConnectionType = (int16) LastPinConnectionType;
					PinChanged = 1;
				}

				if (strcmpUTF8(WorkingPin->Name, PinName) != 0)
				{
					memset(WorkingPin->Name, 0, sizeof(WorkingPin->Name));
					memmove(WorkingPin->Name, &PinName, sizeof(WorkingPin->Name) - 1);
					PinChanged = 1;
				}

				if (strcmpUTF8(WorkingPin->Label, LabelName) != 0)
				{
					memset(WorkingPin->Label, 0, sizeof(WorkingPin->Label));
					memmove(WorkingPin->Label, &LabelName, sizeof(WorkingPin->Label) - 1);
					PinChanged = 1;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				{
					if ((WorkingPin->NameInfo & TEXT_NOT_VISIBLE) == 0)
					{
						WorkingPin->NameInfo |= TEXT_NOT_VISIBLE;
						PinChanged = 1;
					}
				}
				else
				{
					if ((WorkingPin->NameInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
					{
						WorkingPin->NameInfo &= ~TEXT_NOT_VISIBLE;
						PinChanged = 1;
					}
				}

#ifdef _DEBUG
				memset(DialogTextLine, 0, 200);

				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, (WPARAM) 17, (LPARAM) DialogTextLine)) > 0)
				{
					res = strtol(DialogTextLine, &weg, 2);

					if (res > 0)
					{
						WorkingPin->SwapInfo = (int16) res;
						PinChanged = 1;
					}
				}

#endif

				if (PinChanged)
					EndDialog(Dialog, 1);
				else
					EndDialog(Dialog, 2);

				break;

// ********************************************************************************************
// ********************************************************************************************
			case 2:
				if (GetDialogFloatValue(Dialog, IDC_EDIT4, &AddPinDialogDivX) == 0)
				{
					MessageBoxUTF8(SCHWindow, SC(61, "Wrong value in offset +X"), SC(38, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				if (GetDialogFloatValue(Dialog, IDC_EDIT5, &AddPinDialogDivY) == 0)
				{
					MessageBoxUTF8(SCHWindow, SC(62, "Wrong value in offset +Y"), SC(38, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				if ((AddPinDialogDivX == 0.0) && (AddPinDialogDivY == 0.0))
					AddPinDialogDivY = -1.0;

				OX = 0.0;
				OY = 0.0;
				divx = AddPinDialogDivX;
				divy = AddPinDialogDivY;
				NrAddPins = 0;

				for (cnt = 0; cnt < 25; cnt++)
				{
					res = GetDialogTextLine(Dialog, IDC_EDIT3, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

					if (res > 0)
					{

						// ******************************************************************************************

						while ((res > 0) && (DialogTextLine[res - 1] == ' '))
						{
							DialogTextLine[res - 1] = 0;
							res--;
						}

						if (CheckLabelName(DialogTextLine) == -1)
						{
							sprintf(sel, SC(65, "Label name ( %s ) is not valid"), DialogTextLine);
							MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
							return about;
						}

						AddedPins[NrAddPins].X = (float) OX;
						AddedPins[NrAddPins].Y = (float) OY;
						AddedPins[NrAddPins].NameX = (float) (OX + 2.5);
						AddedPins[NrAddPins].NameY = (float) (OY - 0.4);
						AddedPins[NrAddPins].ConnectionType = (int16) (LastPinConnectionType);
						memset(&AddedPinText[NrAddPins], 0, sizeof(ObjectTextRecord));
						memmove(&AddedPins[NrAddPins].Name, &DialogTextLine, sizeof(AddedPins[NrAddPins].Name) - 1);
						memmove(&AddedPins[NrAddPins].Label, &DialogTextLine, sizeof(AddedPins[NrAddPins].Label) - 1);

						// ******************************************************************************************

						memset(&AddedPins[NrAddPins].Label, 0, sizeof(((PinRecord*)0)->Label));
						memmove(&AddedPins[NrAddPins].Label, &DialogTextLine, sizeof(AddedPins[NrAddPins].Label) - 1);
						OX += divx;
						OY += divy;
						NrAddPins++;
					}
				}

				EndDialog(Dialog, 1);
				break;

// ********************************************************************************************
// ********************************************************************************************
			case 3:
				if ((res =
				            SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50,
				                                   (LPARAM) DialogTextLine)) == 0)
				{
					MessageBoxUTF8(SCHWindow, SC(68, "Label should have a name"), SC(38, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				// ******************************************************************************************

				if (CheckLabelName(DialogTextLine) == -1)
				{
					sprintf(sel, SC(65, "Label name ( %s ) is not valid"), DialogTextLine);
					MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				memset(PinName, 0, MAX_LENGTH_STRING - 50);
				strcpy(PinName, DialogTextLine);

				// ******************************************************************************************

				if (WorkingPin->ConnectionType != LastPinConnectionType)
				{
					WorkingPin->ConnectionType = (int16) LastPinConnectionType;
					PinChanged = 1;
				}

				if (strcmpUTF8(WorkingPin->Label, PinName) != 0)
				{
					memset(WorkingPin->Name, 0, sizeof(WorkingPin->Name));
//                memmove(WorkingPin->Name,&PinName,sizeof(WorkingPin->Name)-1);
					memset(WorkingPin->Label, 0, sizeof(WorkingPin->Label));
					memmove(WorkingPin->Label, &PinName, sizeof(WorkingPin->Label) - 1);
					PinChanged = 1;
				}

				if (PinChanged)
					EndDialog(Dialog, 1);
				else
					EndDialog(Dialog, 2);

				break;
			}

			return about;

		case IDC_BUTTON1:
			if (DialogMode != 0)
				return about;

			PinChanged = 0;

			if (!EditingSheetSymbol)
				LastPinConnectionType = GetConnectionRadioButtons(Dialog, 0);
			else
				LastPinConnectionType = GetConnectionRadioButtons(Dialog, 1);

			PowerPin = 0;

			if (LastPinConnectionType == POWER_CONNECTION)
				PowerPin = 1;

			AutoNumbering = 0;

			if ((SendDlgItemMessage(Dialog, IDC_EDIT6, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM)& NumberString)
						> 0) && (sscanf(NumberString, "%i", &StartNumber) == 1) && (StartNumber >= 0)
				&&
				(SendDlgItemMessage(Dialog, IDC_EDIT7, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM)& NumberString)
						 > 0) && (sscanf(NumberString, "%i", &IncNumber) == 1) && (IncNumber != 0)
				&&
				(SendDlgItemMessage(Dialog, IDC_EDIT8, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM)& NumberString)
						 > 0) && (sscanf(NumberString, "%i", &NrOfPins) == 1) && (NrOfPins > 0))
				AutoNumbering = 1;

			if (AutoNumbering)
			{
				char RepeatBuf[MAX_LENGTH_STRING * 25];

				if (PowerPin)
				{
					MessageBoxUTF8(SCHWindow, SC(480, "Powerpins and auto numbering can not be combined"),
						SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;	
				}

				memset(DialogTextLine, 0, MAX_LENGTH_STRING - 50);

				if ((res =
					SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50,
					(LPARAM)DialogTextLine)) > 0)
				{
					DialogTextLine[res] = 0;

					while ((res > 0) && ((DialogTextLine[res - 1] == ' ')))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}
				}

				if (DialogTextLine[0] != 0)
				{
					if (CheckPinName(DialogTextLine, PowerPin) == -1)
					{
						sprintf(sel, SC(63, "Pin name ( %s ) is not valid"), DialogTextLine);
						MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					strcpy(PinName, DialogTextLine);
				}
				else
					PinName[0] = 0;

				NrOfPins = min(NrOfPins, 25);
				cnt2 = StartNumber;
				memset(&NewPinName, 0, sizeof(NewPinName));
				RepeatBuf[0] = 0;

				for (cnt = 0; cnt < NrOfPins; cnt++)
				{
					sprintf(NewPinName, "%s%d\r\n", PinName, cnt2);
					strcat(RepeatBuf, NewPinName);

					NrAddPins++;
					cnt2 += IncNumber;

					if (cnt2 < 0)
						break;
				}

				RepeatBuf[strlen(RepeatBuf) - 2] = 0; //remove last '\r\n'
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM)RepeatBuf);

			}

			return about;

		case IDC_BUTTON2:
			if (DialogMode != 0)
				return about;

			PinChanged = 0;

			if (!EditingSheetSymbol)
				LastPinConnectionType = GetConnectionRadioButtons(Dialog, 0);
			else
				LastPinConnectionType = GetConnectionRadioButtons(Dialog, 1);

			PowerPin = 0;

			if (LastPinConnectionType == POWER_CONNECTION)
				PowerPin = 1;

			AutoNumbering = 0;

			if ((SendDlgItemMessage(Dialog, IDC_EDIT6, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM)& NumberString)
					> 0) && (sscanf(NumberString, "%i", &StartNumber) == 1) && (StartNumber >= 0)
				&&
				(SendDlgItemMessage(Dialog, IDC_EDIT7, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM)& NumberString)
				> 0) && (sscanf(NumberString, "%i", &IncNumber) == 1) && (IncNumber != 0)
				&&
				(SendDlgItemMessage(Dialog, IDC_EDIT8, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM)& NumberString)
						 > 0) && (sscanf(NumberString, "%i", &NrOfPins) == 1) && (NrOfPins > 0))
				AutoNumbering = 1;

			if (AutoNumbering)
			{
				char RepeatBuf[MAX_LENGTH_STRING * 25];

				if (PowerPin)
				{
					MessageBoxUTF8(SCHWindow, SC(480, "Powerpins and auto numbering can not be combined"),
						SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				memset(DialogTextLine, 0, MAX_LENGTH_STRING - 50);

				if ((res =
					SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 50,
					(LPARAM)DialogTextLine)) > 0)
				{
					DialogTextLine[res] = 0;

					while ((res > 0) && ((DialogTextLine[res - 1] == ' ')))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}
				}

				if (DialogTextLine[0] != 0)
				{
					if (CheckLabelName(DialogTextLine) == -1)
					{
						sprintf(sel, SC(64, "Pin text ( %s ) is not valid for use as a label name"), DialogTextLine);
						MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					strcpy(PinTextName, DialogTextLine);
				}
				else
					PinTextName[0] = 0;

				NrOfPins = min(NrOfPins, 25);
				cnt2 = StartNumber;
				memset(&NewPinTextName, 0, sizeof(NewPinTextName));
				RepeatBuf[0] = 0;

				for (cnt = 0; cnt < NrOfPins; cnt++)
				{
					sprintf(NewPinTextName, "%s%d\r\n", PinTextName, cnt2);
					strcat(RepeatBuf, NewPinTextName);

					NrAddPins++;
					cnt2 += IncNumber;

					if (cnt2 < 0)
						break;
				}

				RepeatBuf[strlen(RepeatBuf) - 2] = 0; //remove last '\r\n'
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM)RepeatBuf);

			}

			return about;

		case IDHELP:
			switch (DialogMode)
			{
			case 0:
				Help("add_pin.htm", 0);
				break;

			case 1:
				Help("edit_symbolpin.htm", 0);
				break;

			case 2:
				Help("add_sheetsymbolpin.htm", 0);
				break;

			case 3:
				Help("edit_sheetsymbolpin.htm", 0);
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

int32 AddPinsDialog(PinRecord * Pin, int32 Mode)
{
	int32 res, ok;

	res = 0;
//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;
	DialogMode = Mode;
	WorkingPin = Pin;

	switch (DialogMode)
	{
	case 0:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDPINS), SCHWindow, (DLGPROC) AddPinsDialog2);
		break;

	case 1:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDPINS2A), SCHWindow, (DLGPROC) AddPinsDialog2);
		break;

	case 2:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDPINS3), SCHWindow, (DLGPROC) AddPinsDialog2);
		break;

	case 3:
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDPINS4), SCHWindow, (DLGPROC) AddPinsDialog2);
		break;
	}

	ok = 1;
	return res;
}

//***********************************************************************************************************************
//****************************** IDD_DIALOG_ADDPOWERPINS ****************************************************************
//***********************************************************************************************************************

int32 CALLBACK AddPowerPinsDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, PowerPinChanged;
	int32 res, cnt2, cnt3;
	char DialogTextLine[MAX_LENGTH_STRING], DialogTextLine2[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(69, "Net name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(70, "Power pin(s)"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(71, "Power pin not visible in sheet"));

		if (DialogMode == 0)
			SetWindowTextUTF8(Dialog, SC(72, "Add power pin"));
		else
			SetWindowTextUTF8(Dialog, SC(73, "Change power pin"));

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) WorkingPowerPin->NetName);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) WorkingPowerPin->Text);

		if ((WorkingPowerPin->NameInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
			SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			PowerPinChanged = 0;
			res =
			    SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) DialogTextLine);
			DialogTextLine[res] = 0;

			if (DialogTextLine[0] == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(74, "Net must have a name"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (CheckNetName(DialogTextLine) == -1)
			{
				MessageBoxUTF8(SCHWindow, SC(75, "Net name is not valid"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (strcmpUTF8(WorkingPowerPin->NetName, DialogTextLine) != 0)
			{
				memset(WorkingPowerPin->NetName, 0, sizeof(WorkingPowerPin->NetName));
				memmove(WorkingPowerPin->NetName, &DialogTextLine, sizeof(WorkingPowerPin->NetName) - 1);
				PowerPinChanged = 1;
			}

			res =
			    SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) DialogTextLine);
			DialogTextLine[res] = 0;

			if (DialogTextLine[0] == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(76, "Power pin(s) must have a name"), SC(38, "Error"),
				               MB_APPLMODAL | MB_OK);
				return about;
			}

			while ((res > 0) && ((DialogTextLine[res - 1] == ' ') || (DialogTextLine[res - 1] == '\t')))
			{
				DialogTextLine[res - 1] = 0;
				res--;
			}

			while ((res > 0) && (DialogTextLine[res - 1] == ','))
			{
				DialogTextLine[res - 1] = 0;
				res--;
			}

			while ((res > 0) && ((DialogTextLine[res - 1] == ' ') || (DialogTextLine[res - 1] == '\t')))
			{
				DialogTextLine[res - 1] = 0;
				res--;
			}

			memcpy(DialogTextLine2, DialogTextLine, sizeof(DialogTextLine));
			cnt2 = 0;
			cnt3 = 0;

			while (cnt3 < res)
			{
				if ((DialogTextLine2[cnt3] != ' ') && (DialogTextLine2[cnt3] != '\t'))
				{
					DialogTextLine[cnt2] = DialogTextLine2[cnt3];
					cnt2++;
				}

				cnt3++;
			}

			DialogTextLine[cnt2] = 0;

			if (CheckPinNames(DialogTextLine) == -1)
			{
				MessageBoxUTF8(SCHWindow, SC(77, "Power pin(s) are not valid"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (strcmpUTF8(WorkingPowerPin->Text, DialogTextLine) != 0)
			{
				memset(WorkingPowerPin->Text, 0, sizeof(WorkingPowerPin->Text));
				memmove(WorkingPowerPin->Text, &DialogTextLine, sizeof(WorkingPowerPin->Text) - 1);
				PowerPinChanged = 1;
			}

			if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
			{
				if ((WorkingPowerPin->NameInfo & TEXT_NOT_VISIBLE) == 0)
				{
					WorkingPowerPin->NameInfo |= TEXT_NOT_VISIBLE;
					PowerPinChanged = 1;
				}
			}
			else
			{
				if ((WorkingPowerPin->NameInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				{
					WorkingPowerPin->NameInfo &= ~TEXT_NOT_VISIBLE;
					PowerPinChanged = 1;
				}
			}

			if (PowerPinChanged)
				EndDialog(Dialog, 1);
			else
				EndDialog(Dialog, 2);

			return about;

		case IDHELP:
			if (DialogMode == 0)
				Help("add_powerpin.htm", 0);
			else
				Help("edit_any_text.htm", 0);

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

int32 AddPowerPinsDialog(PowerPinRecord * PowerPin, int32 Mode)
{
	int32 res, ok;

//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;
	DialogMode = Mode;
	WorkingPowerPin = PowerPin;
	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDPOWERPINS), SCHWindow,
	              (DLGPROC) AddPowerPinsDialog2);
	ok = 1;
	return res;
}

//**********************************************************************************************************************
//******************************************* IDD_DIALOG_ADDPINBUS2 ****************************************************
//**********************************************************************************************************************

int32 CALLBACK AddPinBusDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, PinBusChanged;
	int32 hulp, TempPinConnectionType, TempPinType, cnt, cnt2, cnt3, TotalLength, res, StringLength, NrLines,
	      LastLength, PinCount;
	char PinStrings[8][MAX_LENGTH_STRING], sel[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING];
	PinBusRecord TempPinBus, *PtrPinBus;
#ifdef _DEBUG
	char sel2[MAX_LENGTH_STRING], *weg;
#endif
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(78, "Pin(s)"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(79, "Nr pins"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(80, "Pin text"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(41, "Label"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(44, "Connection"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(45, "Type"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(176, "Gate/pin swap"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(46, "Input"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(47, "Output"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(48, "Input/Output"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(49, "Tristate"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO5, SC(50, "Open collector"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO6, SC(51, "Passive"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO7, SC(52, "Power"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO8, SC(53, "Undefined"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO9, "TTL");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO10, "LVTTL");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO11, "CMOS");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO12, SC(54, "Analog"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO13, SC(52, "Power"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(56, "Pin name not visible"));

		if (DialogMode == 0)
			SetWindowTextUTF8(Dialog, SC(81, "Add pin bus"));
		else
			SetWindowTextUTF8(Dialog, SC(82, "Change pin bus"));

		switch (WorkingPinBus->ConnectionType & 0xff)
		{
		case CONNECTION_INPUT:
			SendDlgItemMessage(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_OUTPUT:
			SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_IO:
			SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_TRISTATE:
			SendDlgItemMessage(Dialog, IDC_RADIO4, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_OC:
			SendDlgItemMessage(Dialog, IDC_RADIO5, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_PASSIVE:
			SendDlgItemMessage(Dialog, IDC_RADIO6, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_POWER:
			SendDlgItemMessage(Dialog, IDC_RADIO7, BM_SETCHECK, 1, 0);
			break;
		}

		switch (WorkingPinBus->ConnectionType >> 8)
		{
		case CONNECTION_UNDEFINED:
			SendDlgItemMessage(Dialog, IDC_RADIO8, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_TTL:
			SendDlgItemMessage(Dialog, IDC_RADIO9, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_LVTTL:
			SendDlgItemMessage(Dialog, IDC_RADIO10, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_CMOS:
			SendDlgItemMessage(Dialog, IDC_RADIO11, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_ANALOG:
			SendDlgItemMessage(Dialog, IDC_RADIO12, BM_SETCHECK, 1, 0);
			break;

		case CONNECTION_POWER:
			SendDlgItemMessage(Dialog, IDC_RADIO13, BM_SETCHECK, 1, 0);
			break;
		}

		sprintf(DialogTextLine, "%i", WorkingPinBus->NrPins);
		SendDlgItemMessage(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		memset(&DialogTextLine, 0, sizeof(DialogTextLine));
		cnt2 = 0;
		WorkingPinBus->Text[sizeof(WorkingPinBus->Text) - 1] = 0;

		for (cnt = 0; cnt < (int32) strlen(WorkingPinBus->Text); cnt++)
		{
			if (WorkingPinBus->Text[cnt] == '\\')
			{
				DialogTextLine[cnt2] = '\r';
				DialogTextLine[cnt2 + 1] = '\n';

				if (cnt2 < sizeof(DialogTextLine) - 5)
					cnt2 += 2;
			}
			else
			{
				DialogTextLine[cnt2] = WorkingPinBus->Text[cnt];

				if (cnt2 < sizeof(DialogTextLine) - 5)
					cnt2++;
			}
		}

		if ((WorkingPinBus->NameInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
			SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) WorkingPinBus->Label);
#ifdef _DEBUG
		res = 0;
		memmove(&res, &WorkingPinBus->SwapInfo, 2);
		ultoa((uint32) res, DialogTextLine, 2);
		res = strlen(DialogTextLine);
		strcpy(sel2, "0000000000000000000");
		sel2[16 - res] = 0;
		sprintf(sel, "%s%s", sel2, DialogTextLine);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) sel);
#endif
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			PinBusChanged = 0;

			if (DialogMode == 0)
				PtrPinBus = WorkingPinBus;
			else
				PtrPinBus = &TempPinBus;

			memset(PtrPinBus, 0, sizeof(PinBusRecord));

			for (cnt = 0; cnt < 8; cnt++)
			{
				res = GetDialogTextLine(Dialog, IDC_EDIT1, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);
				PinStrings[cnt][0] = 0;

				if (DialogTextLine[0] != 0)
				{
					if (res > 127)
					{
						MessageBoxUTF8(SCHWindow, SC(83, "Nr characters in a line > 127"), SC(38, "Error"),
						               MB_APPLMODAL | MB_OK);
						return about;
					}

					while ((res > 0) && ((DialogTextLine[res - 1] == ' ') || (DialogTextLine[res - 1] == '\t')))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}

					while ((res > 0) && (DialogTextLine[res - 1] == ','))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}

					while ((res > 0) && ((DialogTextLine[res - 1] == ' ') || (DialogTextLine[res - 1] == '\t')))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}

					if (res > 0)
					{
//                memmove(&PinStrings[cnt],&DialogTextLine,res);
						cnt2 = 0;
						cnt3 = 0;

						while (cnt3 < res)
						{
							if ((DialogTextLine[cnt3] != ' ') && (DialogTextLine[res - 1] != '\t'))
							{
								PinStrings[cnt][cnt2] = DialogTextLine[cnt3];
								cnt2++;
							}

							cnt3++;
						}

						PinStrings[cnt][cnt2] = 0;
					}
				}
			}

			memset(&AddedPinText[0], 0, sizeof(ObjectTextRecord));

			if (DialogMode == 0)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING - 50);

				if ((res =
				            SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, MAX_LENGTH_STRING - 50,
				                                   (LPARAM) DialogTextLine)) != 0)
				{
					DialogTextLine[res] = 0;
					memmove(&AddedPinText[0].Text, &DialogTextLine, sizeof(AddedPinText[0].Text));
					AddedPinText[0].X = (float) 5.0;
					AddedPinText[0].Y = (float) 0.0;
					AddedPinText[0].FontHeight = (double) 1.0;
					AddedPinText[0].TextMode = ALIGN_LEFT_CENTRE;
					AddedPinText[NrAddPins].Thickness = (float) 0.1;
				}
			}

//          memset(&LabelName,0,sizeof(LabelName));
//          memset(&PinTextName,0,sizeof(PinText));
			memset(PtrPinBus->Label, 0, sizeof(PtrPinBus->Label));

			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, MAX_LENGTH_STRING - 50,
			                                   (LPARAM) DialogTextLine)) != 0)
			{
				DialogTextLine[res] = 0;

				while ((res > 0) && (DialogTextLine[res - 1] == ' '))
				{
					DialogTextLine[res - 1] = 0;
					res--;
				}

				memmove(PtrPinBus->Label, &DialogTextLine, sizeof(PtrPinBus->Label) - 1);

				if (CheckLabelName(PtrPinBus->Label) == -1)
				{
					MessageBoxUTF8(SCHWindow, SC(67, "Label name is not valid"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}
			}
			else
			{
				MessageBoxUTF8(SCHWindow, SC(84, "Label must have a name"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 50,
			                                   (LPARAM) DialogTextLine)) == 0)
				memset(DialogTextLine, 0, MAX_LENGTH_STRING - 50);
			else
				DialogTextLine[res] = 0;

			if ((sscanf(DialogTextLine, "%i", &hulp) == 1) && (hulp > 1))
			{
				if (hulp > 64)
				{
					MessageBoxUTF8(SCHWindow, SC(85, "Max nr pins is 64"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				PtrPinBus->NrPins = (int16) hulp;
			}
			else
			{
				MessageBoxUTF8(SCHWindow, SC(86, "Wrong value in nr pins >1"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			TempPinConnectionType = 0;

			if (SendDlgItemMessage(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0) == 1)
				TempPinConnectionType = CONNECTION_INPUT;

			if (SendDlgItemMessage(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0) == 1)
				TempPinConnectionType = CONNECTION_OUTPUT;

			if (SendDlgItemMessage(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0) == 1)
				TempPinConnectionType = CONNECTION_IO;

			if (SendDlgItemMessage(Dialog, IDC_RADIO4, BM_GETCHECK, 0, 0) == 1)
				TempPinConnectionType = CONNECTION_TRISTATE;

			if (SendDlgItemMessage(Dialog, IDC_RADIO5, BM_GETCHECK, 0, 0) == 1)
				TempPinConnectionType = CONNECTION_OC;

			if (SendDlgItemMessage(Dialog, IDC_RADIO6, BM_GETCHECK, 0, 0) == 1)
				TempPinConnectionType = CONNECTION_PASSIVE;

			if (SendDlgItemMessage(Dialog, IDC_RADIO7, BM_GETCHECK, 0, 0) == 1)
				TempPinConnectionType = CONNECTION_POWER;

			TempPinType = 0;

			if (SendDlgItemMessage(Dialog, IDC_RADIO8, BM_GETCHECK, 0, 0) == 1)
				TempPinType = CONNECTION_UNDEFINED;

			if (SendDlgItemMessage(Dialog, IDC_RADIO9, BM_GETCHECK, 0, 0) == 1)
				TempPinType = CONNECTION_TTL;

			if (SendDlgItemMessage(Dialog, IDC_RADIO10, BM_GETCHECK, 0, 0) == 1)
				TempPinType = CONNECTION_LVTTL;

			if (SendDlgItemMessage(Dialog, IDC_RADIO11, BM_GETCHECK, 0, 0) == 1)
				TempPinType = CONNECTION_CMOS;

			if (SendDlgItemMessage(Dialog, IDC_RADIO12, BM_GETCHECK, 0, 0) == 1)
				TempPinType = CONNECTION_ANALOG;

			if (SendDlgItemMessage(Dialog, IDC_RADIO13, BM_GETCHECK, 0, 0) == 1)
				TempPinType = CONNECTION_POWER;

			PtrPinBus->ConnectionType = (int16) (TempPinConnectionType + TempPinType * 256);
			NrLines = 0;
			PinCount = 0;
			LastLength = 1;

			for (cnt = 0; cnt < 8; cnt++)
			{
				TotalLength = strlen(PtrPinBus->Text);
				StringLength = strlen(PinStrings[cnt]);

				if (StringLength > 0)
				{
					if (LastLength == 0)
					{
						MessageBoxUTF8(SCHWindow, SC(87, "Nr characters in a line is 0"), SC(38, "Error"),
						               MB_APPLMODAL | MB_OK);
						return about;
					}

					if (CheckPinNames(PinStrings[cnt]) == -1)
					{
						sprintf(sel, SC(88, "Pins ( %s ) are not valid"), PinStrings[cnt]);
						MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					if ((res = GetNrPinNames(PinStrings[cnt])) > 0)
						PinCount += res;

					if (TotalLength + StringLength + 1 < sizeof(PtrPinBus->Text))
					{
						memmove(&(PtrPinBus->Text[TotalLength]), &PinStrings[cnt], (int32) StringLength);
						PtrPinBus->Text[TotalLength + StringLength] = '\\';
						NrLines++;
					}
					else
					{
						MessageBoxUTF8(SCHWindow, SC(89, "Nr characters in all lines > 371"), SC(38, "Error"),
						               MB_APPLMODAL | MB_OK);
						return about;
					}
				}
				else
				{
					if (NrLines == 0)
					{
						MessageBoxUTF8(SCHWindow, SC(87, "Nr characters in a line is 0"), SC(38, "Error"),
						               MB_APPLMODAL | MB_OK);
						return about;
					}
				}

				LastLength = StringLength;
			}

			if (PinCount != PtrPinBus->NrPins)
			{
				MessageBoxUTF8(SCHWindow, SC(90, "Wrong value in nr pins"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (PtrPinBus->Text[0] == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(91, "Pin bus should have pin names"), SC(38, "Error"),
				               MB_APPLMODAL | MB_OK);
				return about;
			}

			TotalLength = strlen(PtrPinBus->Text);
			PtrPinBus->Text[TotalLength - 1] = 0;
			TotalLength--;

			if (DialogMode == 0)
			{
				PtrPinBus->NameX = (float) 3.0;
				PtrPinBus->NameY = (float) (NrLines + 0.8);
				PtrPinBus->NameInfo = ALIGN_RIGHT_BOTTOM;
			}
			else
			{
				if ((strcmpUTF8(PtrPinBus->Text, WorkingPinBus->Text) != 0)
				        || (strcmpUTF8(PtrPinBus->Label, WorkingPinBus->Label) != 0)
				        || (PtrPinBus->NrPins != WorkingPinBus->NrPins)
				        || (TempPinConnectionType + TempPinType * 256 != WorkingPinBus->ConnectionType))
				{
					PinBusChanged = 1;
					memmove(WorkingPinBus->Text, PtrPinBus->Text, sizeof(PtrPinBus->Text));
					memmove(WorkingPinBus->Label, PtrPinBus->Label, sizeof(PtrPinBus->Label));
					WorkingPinBus->NrPins = PtrPinBus->NrPins;
					WorkingPinBus->ConnectionType = PtrPinBus->ConnectionType;
				}
			}

#ifdef _DEBUG
			memset(DialogTextLine, 0, 200);

			if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_GETTEXT, (WPARAM) 17, (LPARAM) DialogTextLine)) > 0)
			{
				res = strtol(DialogTextLine, &weg, 2);

				if (res > 0)
				{
					WorkingPinBus->SwapInfo = (int16) res;
					PinBusChanged = 1;
				}
			}

#endif

			if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
			{
				if ((WorkingPinBus->NameInfo & TEXT_NOT_VISIBLE) == 0)
				{
					WorkingPinBus->NameInfo |= TEXT_NOT_VISIBLE;
					PinBusChanged = 1;
				}
			}
			else
			{
				if ((WorkingPinBus->NameInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				{
					WorkingPinBus->NameInfo &= ~TEXT_NOT_VISIBLE;
					PinBusChanged = 1;
				}
			}

			if ((DialogMode == 0) || (PinBusChanged))
				EndDialog(Dialog, 1);
			else
				EndDialog(Dialog, 2);

			return about;

		case IDHELP:
			if (DialogMode == 0)
				Help("add_pinbus.htm", 0);
			else
				Help("edit_pinbus.htm", 0);

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

int32 AddPinBusDialog(PinBusRecord * PinBus, int32 Mode)
{
	int32 res, ok;

//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;

	DialogMode = Mode;
	WorkingPinBus = PinBus;

	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDPINBUS2), SCHWindow, (DLGPROC) AddPinBusDialog2);

//  strcpy(TextLine,&DialogTextLine);
	ok = 1;
	return res;
}

//**************************************************************************************************************************
//************************ IDD_DIALOG_SUBPINSTRINGS ************************************************************************
//**************************************************************************************************************************

int32 CALLBACK SubPinStringsDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, ok, ErrorLine, res, cnt, cnt2, cnt3, cnt4, count, count2, PinCount, NrSubPinDefsLines, NrPins;
	char sel[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING];
	PinRecord *Pin;
	SubPinDefsType *SubPinDef;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(94, "Package"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(92, "Pin name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(93, "Pins per part"));

		ErrorLine = 0;
		count = 0;
		NrPins = 0;

		if (MaxNrSubPinDefs < DesignSymbol.NrPartsPerPackage * DesignSymbol.NrPins)
			AllocateMemSubPinDefs(DesignSymbol.NrPartsPerPackage * DesignSymbol.NrPins, 0);

		memset(&DialogTextLine, 0, sizeof(DialogTextLine));

		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrPins++;
				SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) (LPSTR) Pin->Name);
				NrSubPinDefsLines = DesignSymbol.NrSubPinDefs / DesignSymbol.NrPartsPerPackage;

				for (cnt2 = 0; cnt2 < NrSubPinDefsLines; cnt2++)
				{
					if (strcmpUTF8(Pin->Name, SubPinDefsNames[cnt2].Name) == 0)
						break;
				}

				if (cnt2 < NrSubPinDefsLines)
				{
					for (cnt3 = 0; cnt3 < DesignSymbol.NrPartsPerPackage; cnt3++)
					{
						strcat(DialogTextLine, ((*SubPinDefs)[cnt2 * DesignSymbol.NrPartsPerPackage + cnt3]));

						if (cnt3 < DesignSymbol.NrPartsPerPackage - 1)
							strcat(DialogTextLine, ",");
					}
				}

				strcat(DialogTextLine, "\r\n");
			}
		}

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			count = 0;
			NrPins = 0;

			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == 0)
					NrPins++;
			}

			PinCount = 0;
			NrSubPinDefsLines = 0;
			memset(SubPinDefsNames, 0, sizeof(SubPinDefsNames));

			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					res = GetDialogTextLine(Dialog, IDC_EDIT1, PinCount, DialogTextLine, MAX_LENGTH_STRING - 50);

					if (DialogTextLine[0] == 0)
					{
						MessageBoxUTF8(SCHWindow, SC(95, "Empty line"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					if (CheckPinNames(DialogTextLine) == -1)
					{
						sprintf(sel, SC(88, "Pins ( %s ) are not valid"), DialogTextLine);
						MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					if ((count2 = GetNrPinNames(DialogTextLine)) != DesignSymbol.NrPartsPerPackage)
					{
						MessageBoxUTF8(SCHWindow, SC(96, "Pin count incorrect"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					while ((res > 0) && ((DialogTextLine[res - 1] == ' ') || (DialogTextLine[res - 1] == '\t')))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}

					while ((res > 0) && (DialogTextLine[res - 1] == ','))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}

					while ((res > 0) && ((DialogTextLine[res - 1] == ' ') || (DialogTextLine[res - 1] == '\t')))
					{
						DialogTextLine[res - 1] = 0;
						res--;
					}

					ok = 1;
					cnt2 = 0;
					cnt3 = 0;

					for (cnt4 = 0; cnt4 < DesignSymbol.NrPartsPerPackage; cnt4++)
					{
						while ((cnt2 < (int32) strlen(DialogTextLine)) && (DialogTextLine[cnt2] != ','))
							cnt2++;

						if (cnt2 == cnt3)
						{
							ok = 0;
							ErrorLine = NrPins + 1;
						}

						if ((ok) && (count < MaxNrSubPinDefs))
						{
							SubPinDef = &((*SubPinDefs)[count]);
							memset(SubPinDef, 0, sizeof(SubPinDefsType));
							memmove(SubPinDef, &DialogTextLine[cnt3],
							        (int32) min(sizeof(SubPinDefsType) - 1, cnt2 - cnt3));
							cnt2++;
							cnt3 = cnt2;
							count++;
							FileChanged = 1;
							DataBaseChanged = 1;
						}
					}

					strcpy(SubPinDefsNames[NrSubPinDefsLines].Name, Pin->Name);
					NrSubPinDefsLines++;
					PinCount++;
				}
			}

			DesignSymbol.NrSubPinDefs = (DesignSymbol.NrPartsPerPackage) * NrPins;
			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("edit_pinnumbers_package_parts.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 SubPinStringsDialog(int32 Mode)
{
	int32 res, ok;

//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;
	DialogMode = Mode;

	if (EditingSymbol)
	{
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SUBPINSTRINGS), SCHWindow,
		              (DLGPROC) SubPinStringsDialog2);
	}
	else
		res = 0;

	ok = 1;
	return res;
}

//*************************************************************************************************************************
//*********************** IDD_DIALOG_INSTANCEINFO *************************************************************************
//*************************************************************************************************************************

int32 CALLBACK InstanceInfoDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, InstanceChanged, cnt, res, SymbolNr, MemPos, NrPartsPerPackage, NrAttributes, LengthIdent, LengthValue,
	      pos, cnt2, cnt3, MaxLength, PropertyError, NrCompProperties;
	char sel[MAX_LENGTH_STRING], AttributeIdent[4096], AttributeValue[4096], DialogTextLine[4096],
	     DialogTextLine2[4096], DialogTextLine3[4096], DialogTextLine4[4096], PropertyID[MAX_LENGTH_STRING],
	     PropertyValue[MAX_LENGTH_STRING], PropertyBuf[4096], *InstanceAttrBuf;
	SymbolsPosRecord *SymbolPos;
	SymbolRecord *Symbol;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(107, "Component info"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDABORT, SC(97, "Abort"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(98, "Reference"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(99, "Symbol name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(100, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(101, "Part number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(102, "Geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(103, "Part description"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(104, "Package part nr"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(123, "Properties"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(124, "ID"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(125, "Values"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(444, "Names Power net (to be overwritten)"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(69, "Net name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(445, "New net name"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(105, "Placing option"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(22, "Not visible"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK4, SC(22, "Not visible"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(450, "No geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(106, "Select geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(199, "Search geometry"));

		memset(&GeometrieName, 0, sizeof(GeometrieName));

		if (DialogMode == 0)
		{
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) WorkingInstance->Reference);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) WorkingInstance->SymbolName);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) WorkingInstance->Value);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) WorkingInstance->PartNr);

			if ((WorkingInstance->SymbolInfo & NO_GEOMETRY) == 0)
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) WorkingInstance->Geometry);
			else
				SendDlgItemMessage(Dialog, IDC_CHECK5, BM_SETCHECK, 1, 0);

			SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) WorkingInstance->PartDescription);
			SymbolNr = -1;

			for (cnt = 0; cnt < Design.NrSymbols; cnt++)
			{
				SymbolPos = &((*SymbolsPos)[cnt]);

				if (stricmpUTF8(SymbolPos->SymbolName, WorkingInstance->SymbolName) == 0)
					SymbolNr = cnt;
			}

			if (SymbolNr == -1)
				return about;

			MemPos = (*SymbolsPos)[SymbolNr].Pos;
			Symbol = (SymbolRecord *) & (SymbolsMem[MemPos]);
			NrPartsPerPackage = min(100, Symbol->NrPartsPerPackage);

			for (cnt = 1; cnt < NrPartsPerPackage + 1; cnt++)
			{
				sprintf(sel, "%i [ %c ]", cnt, 64 + cnt);
				res = SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) sel);
			}

			res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, WorkingInstance->PackagePartNr - 1, 0);

			if ((WorkingInstance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				SendDlgItemMessage(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);

			if ((WorkingInstance->ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				SendDlgItemMessage(Dialog, IDC_CHECK4, BM_SETCHECK, 1, 0);

			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETSTYLE, BS_AUTOCHECKBOX, 0);

			if (WorkingInstance->PlacingOption != -1)
				SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

			DialogTextLine[0] = 0;
			DialogTextLine2[0] = 0;
			DialogTextLine3[0] = 0;
			DialogTextLine4[0] = 0;
			NrAttributes = 0;

			NrCompProperties = GetCompProperties(WorkingInstance, NULL, NULL, 0x40);

			for (cnt = 0; cnt < NrCompProperties; cnt++)
			{
				GetCompProperties(WorkingInstance, PropertyID, PropertyValue, 0x20 + cnt);

				if (PropertyID[0] != '~')
				{
					strcat(DialogTextLine, PropertyID);
					strcat(DialogTextLine, "\r\n");
					strcat(DialogTextLine2, PropertyValue);
					strcat(DialogTextLine2, "\r\n");
				}
				else
				{
					strcat(DialogTextLine3, (LPSTR) & PropertyID[1]);
					strcat(DialogTextLine3, "\r\n");
					strcat(DialogTextLine4, PropertyValue);
					strcat(DialogTextLine4, "\r\n");
				}
			}

			SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT8, WM_SETTEXT, 0, (LPARAM) DialogTextLine2);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT9, WM_SETTEXT, 0, (LPARAM) DialogTextLine3);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT10, WM_SETTEXT, 0, (LPARAM) DialogTextLine4);
		}
		else
		{
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT8, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT9, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT10, WM_SETTEXT, 0, (LPARAM) SC(487, "-- unchanged --"));
			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, BST_INDETERMINATE, 0);
			SendDlgItemMessage(Dialog, IDC_CHECK3, BM_SETCHECK, BST_INDETERMINATE, 0);
			SendDlgItemMessage(Dialog, IDC_CHECK4, BM_SETCHECK, BST_INDETERMINATE, 0);
			SendDlgItemMessage(Dialog, IDC_CHECK5, BM_SETCHECK, BST_INDETERMINATE, 0);
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			InstanceChanged = 0;
			res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
			sel[res] = 0;

			if (strcmpUTF8(sel, WorkingInstance->Reference) != 0)
			{
				memset(WorkingInstance->Reference, 0, sizeof(WorkingInstance->Reference));
				memcpy(WorkingInstance->Reference, &sel, min(sizeof(WorkingInstance->Reference) - 1, res));
				InstanceChanged = 1;
			}

			res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
			sel[res] = 0;

			if (strcmpUTF8(sel, WorkingInstance->Value) != 0)
			{
				memset(WorkingInstance->Value, 0, sizeof(WorkingInstance->Value));
				memcpy(WorkingInstance->Value, &sel, min(sizeof(WorkingInstance->Value) - 1, res));
				InstanceChanged = 1;
			}

			res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
			sel[res] = 0;

			if (strcmpUTF8(sel, WorkingInstance->PartNr) != 0)
			{
				memset(WorkingInstance->PartNr, 0, sizeof(WorkingInstance->PartNr));
				memcpy(WorkingInstance->PartNr, &sel, min(sizeof(WorkingInstance->PartNr) - 1, res));
				InstanceChanged = 1;
			}

			res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
			sel[res] = 0;

			if (stricmpUTF8(sel, WorkingInstance->Geometry) != 0)
			{
				memset(WorkingInstance->Geometry, 0, sizeof(WorkingInstance->Geometry));
				memcpy(WorkingInstance->Geometry, &sel, min(sizeof(WorkingInstance->Geometry) - 1, res));
				InstanceChanged = 1;
			}

			if (GeometrieName[0] != 0)
			{
				strcpy(WorkingInstance->Geometry, GeometrieName);
				InstanceChanged = 1;
			}

			res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
			sel[res] = 0;

			if (strcmpUTF8(sel, WorkingInstance->PartDescription) != 0)
			{
				memset(WorkingInstance->PartDescription, 0, sizeof(WorkingInstance->PartDescription));
				memcpy(WorkingInstance->PartDescription, &sel, min(sizeof(WorkingInstance->PartDescription) - 1, res));
				InstanceChanged = 1;
			}

			if (DialogMode == 0)
			{
				if (((res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0)) != CB_ERR)
				        && ((res + 1) != WorkingInstance->PackagePartNr))
				{
					WorkingInstance->PackagePartNr = (int16) (res + 1);
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0) == BST_CHECKED)
				        && ((WorkingInstance->RefInfo & TEXT_NOT_VISIBLE) == 0))
				{
					WorkingInstance->RefInfo |= TEXT_NOT_VISIBLE;
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0) == BST_UNCHECKED)
				        && ((WorkingInstance->RefInfo & TEXT_NOT_VISIBLE) != 0))
				{
					WorkingInstance->RefInfo &= ~TEXT_NOT_VISIBLE;
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK4, BM_GETCHECK, 0, 0) == BST_CHECKED)
				        && ((WorkingInstance->ValueInfo & TEXT_NOT_VISIBLE) == 0))
				{
					WorkingInstance->ValueInfo |= TEXT_NOT_VISIBLE;
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK4, BM_GETCHECK, 0, 0) == BST_UNCHECKED)
				        && ((WorkingInstance->ValueInfo & TEXT_NOT_VISIBLE) != 0))
				{
					WorkingInstance->ValueInfo &= ~TEXT_NOT_VISIBLE;
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED)
				        && (WorkingInstance->PlacingOption == -1))
				{
					WorkingInstance->PlacingOption = 0;
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_UNCHECKED)
				        && (WorkingInstance->PlacingOption != -1))
				{
					WorkingInstance->PlacingOption = -1;
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == BST_CHECKED)
				        && (WorkingInstance->SymbolInfo & NO_GEOMETRY) == 0)
				{
					WorkingInstance->SymbolInfo |= NO_GEOMETRY;
					InstanceChanged = 1;
				}

				if ((SendDlgItemMessage(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == BST_UNCHECKED)
				        && (WorkingInstance->SymbolInfo & NO_GEOMETRY))
				{
					WorkingInstance->SymbolInfo &= ~NO_GEOMETRY;
					InstanceChanged = 1;
				}
			}
			else
			{
				if (SendDlgItemMessage(Dialog, IDC_CHECK3, BM_GETCHECK, 1, 0) == BST_CHECKED)
				{
					WorkingInstance->RefInfo |= TEXT_NOT_VISIBLE;
					WorkingInstance->SymbolInfo |= CHANGED_REFERENCE;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK3, BM_GETCHECK, 1, 0) == BST_UNCHECKED)
				{
					WorkingInstance->RefInfo &= ~TEXT_NOT_VISIBLE;
					WorkingInstance->SymbolInfo |= CHANGED_REFERENCE;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK4, BM_GETCHECK, 1, 0) == BST_CHECKED)
				{
					WorkingInstance->ValueInfo |= TEXT_NOT_VISIBLE;
					WorkingInstance->SymbolInfo |= CHANGED_VALUE;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK4, BM_GETCHECK, 1, 0) == BST_UNCHECKED)
				{
					WorkingInstance->ValueInfo &= ~TEXT_NOT_VISIBLE;
					WorkingInstance->SymbolInfo |= CHANGED_VALUE;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 1, 0) == BST_CHECKED)
				{
					WorkingInstance->PlacingOption = 0;
					WorkingInstance->SymbolInfo |= CHANGED_PLACING_OPTION;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 1, 0) == BST_UNCHECKED)
				{
					WorkingInstance->PlacingOption = -1;
					WorkingInstance->SymbolInfo |= CHANGED_PLACING_OPTION;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == BST_CHECKED)
				{
					WorkingInstance->SymbolInfo |= NO_GEOMETRY;
					WorkingInstance->SymbolInfo |= CHANGED_GEOMETRY;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == BST_UNCHECKED)
				{
					WorkingInstance->SymbolInfo &= ~NO_GEOMETRY;
					WorkingInstance->SymbolInfo |= CHANGED_GEOMETRY;
				}
			}

			MaxLength = sizeof(WorkingInstance->Properties) - 2;
			memcpy(PropertyBuf, WorkingInstance->Properties, MaxLength + 2);
			InstanceAttrBuf = (LPSTR) WorkingInstance->Properties;
			memset(InstanceAttrBuf, 0, sizeof(WorkingInstance->Properties));
			cnt2 = 0;
			pos = 0;
			PropertyError = 0;

			for (cnt = 0; cnt < 40; cnt++)
			{
				memset(&AttributeIdent, 0, sizeof(AttributeIdent));
				memset(&AttributeValue, 0, sizeof(AttributeValue));
				res = GetDialogTextLine(Dialog, IDC_EDIT7, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

				if (DialogTextLine[0] != 0)
				{
					strncpy(AttributeIdent, DialogTextLine, 63);
					cnt3 = strlen(AttributeIdent);

					while ((cnt3 > 0) && (AttributeIdent[cnt3 - 1] == ' '))
						cnt3--;

					AttributeIdent[cnt3] = 0;
				}

				res = GetDialogTextLine(Dialog, IDC_EDIT8, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

				if (DialogTextLine[0] != 0)
				{
					strncpy(AttributeValue, DialogTextLine, 63);
					cnt3 = strlen(AttributeValue);

					while ((cnt3 > 0) && (AttributeValue[cnt3 - 1] == ' '))
						cnt3--;

					AttributeValue[cnt3] = 0;
				}

				LengthIdent = strlen(AttributeIdent);
				LengthValue = strlen(AttributeValue);

				if ((LengthIdent != 0) && (LengthValue != 0))
				{
					if (LengthIdent != 0)
					{
						if (LengthValue != 0)
						{
							if (pos + LengthIdent + LengthValue + 4 < MaxLength)
							{
								if (strcmp("MULTI_ASSY", AttributeIdent) != 0)
								{
									strcpy(InstanceAttrBuf, AttributeIdent);
									InstanceAttrBuf += LengthIdent + 1;
									strcpy(InstanceAttrBuf, AttributeValue);
									InstanceAttrBuf += LengthValue + 1;
									pos += LengthIdent + LengthValue + 2;
									cnt2++;
								}
							}
							else
							{
								PropertyError = 1;
								break;
							}
						}
						else
						{
							PropertyError = 2;
							break;
						}
					}
					else
					{
						PropertyError = 2;
						break;
					}
				}
			}

			for (cnt = 0; cnt < 20; cnt++)
			{
				memset(&AttributeIdent, 0, sizeof(AttributeIdent));
				memset(&AttributeValue, 0, sizeof(AttributeValue));
				memset(DialogTextLine, 0, MAX_LENGTH_STRING - 50);
				res = GetDialogTextLine(Dialog, IDC_EDIT9, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

				if (DialogTextLine[0] != 0)
				{
					AttributeIdent[0] = '~';
					strncpy((LPSTR) & AttributeIdent[1], DialogTextLine, 63);
					cnt3 = strlen(AttributeIdent);

					while ((cnt3 > 0) && (AttributeIdent[cnt3 - 1] == ' '))
						cnt3--;

					AttributeIdent[cnt3] = 0;
				}

				res = GetDialogTextLine(Dialog, IDC_EDIT10, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

				if (DialogTextLine[0] != 0)
				{
					strncpy(AttributeValue, DialogTextLine, 63);
					cnt3 = strlen(AttributeValue);

					while ((cnt3 > 0) && (AttributeValue[cnt3 - 1] == ' '))
						cnt3--;

					AttributeValue[cnt3] = 0;
				}

				LengthIdent = strlen(AttributeIdent);
				LengthValue = strlen(AttributeValue);

				if ((LengthIdent != 0) && (LengthValue != 0))
				{
					if (LengthIdent != 0)
					{
						if (LengthValue != 0)
						{
							if (pos + LengthIdent + LengthValue + 4 < MaxLength)
							{
								strcpy(InstanceAttrBuf, AttributeIdent);
								InstanceAttrBuf += LengthIdent + 1;
								strcpy(InstanceAttrBuf, AttributeValue);
								InstanceAttrBuf += LengthValue + 1;
								pos += LengthIdent + LengthValue + 2;
								cnt2++;
							}
							else
							{
								PropertyError = 1;
								break;
							}
						}
						else
						{
							PropertyError = 2;
							break;
						}
					}
					else
					{
						PropertyError = 2;
						break;
					}
				}
			}

			switch (PropertyError)
			{
			case 1:
				MessageBoxUTF8(SCHWindow, SC(478, "Out of property space"), SC(288, "Warning"), MB_APPLMODAL | MB_OK);
				break;

			case 2:
				MessageBoxUTF8(SCHWindow, SC(479, "Error in properties"), SC(288, "Warning"), MB_APPLMODAL | MB_OK);
				break;
			}

			if (PropertyError)
				break;

			if (memcmp(PropertyBuf, WorkingInstance->Properties, MaxLength + 2) != 0)
				InstanceChanged = 1;

			if ((InstanceChanged) || (DialogMode == 1))
				EndDialog(Dialog, 1);
			else
				EndDialog(Dialog, 2);

			return about;

		case IDC_BUTTON1:
			if ((WorkingInstance->SymbolInfo & NO_GEOMETRY) == 0)
			{
				if (SelectGeometrie() == 1)
				{
					SetFocus(Dialog);
					SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) GeometrieName);
				}
			}

			return about;

		case IDC_BUTTON2:
			if ((WorkingInstance->SymbolInfo & NO_GEOMETRY) == 0)
			{
				if (SearchGeometrie() == 1)
				{
					SetFocus(Dialog);
					SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) GeometrieName);
				}
			}

			return about;

		case IDC_CHECK5:
			if (SendDlgItemMessage(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == BST_CHECKED)
			{
				SendDlgItemMessage(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) "");
				WorkingInstance->SymbolInfo |= NO_GEOMETRY;
			}
			else
				WorkingInstance->SymbolInfo &= ~NO_GEOMETRY;

			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDABORT:
			EndDialog(Dialog, 3);
			return about;

		case IDHELP:
			Help("edit_symbol_parameters.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 InstanceInfoDialog(InstanceRecord * Instance, int32 NrInstances, int32 mode)
{
	int32 res;

	DialogMode = mode;
	WorkingInstance = Instance;
	res =
	    DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_INSTANCEINFO), SCHWindow,
	              (DLGPROC) InstanceInfoDialog2);
	return res;
}

//***************************************************************************************************************
//********************************** IDD_DIALOG_ADDLABEL ********************************************************
//***************************************************************************************************************

int32 CALLBACK AddNetlabelDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res;
	char sel[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));

		if (DialogMode == 0)
		{
			SetWindowTextUTF8(Dialog, SC(111, "Enter a name net label to wire/bus"));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & (WorkingNetLabel->Name));
		}
		else
		{
			SetWindowTextUTF8(Dialog, "Change label"); //asi nevyužita
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & (WorkingNetLabel->Name));
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res =
			    SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) DialogTextLine);
			DialogTextLine[res] = 0;

			if ((DialogTextLine[0] == 0) || (CheckNetName(DialogTextLine) == -1))
			{
				sprintf(sel, SC(110, "Net name %s is not valid"), DialogTextLine);
				MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

//          if (DialogMode==0) {
//            memset(&NetLabelText,0,sizeof(NetLabelText));
//            memmove(&NetLabelText,&DialogTextLine,sizeof(NetLabelText)-1);
			memset(&(WorkingNetLabel->Name), 0, sizeof(WorkingNetLabel->Name));
			memmove(&(WorkingNetLabel->Name), &DialogTextLine, sizeof(WorkingNetLabel->Name) - 1);
			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
//          memset(DialogTextLine,0,200);
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("add_netlabel.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 AddNetlabelDialog(NetLabelRecord * NetLabel, int32 Mode)
{
	int32 res, ok;

	DialogMode = Mode;
	WorkingNetLabel = NetLabel;
	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDLABEL), SCHWindow, (DLGPROC) AddNetlabelDialog2);
	ok = 1;
	return res;
}

//***********************************************************************************************************************
//************************************ IDD_DIALOG_ADDLABELS *************************************************************
//***********************************************************************************************************************

int32 CALLBACK AddNetlabelsDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res;
	char sel[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(114, "Add multiple labels"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(108, "Net labels"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(112, "Start number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(113, "Increment"));

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) NetLabelText);
		SetDialogIntValue(Dialog, IDC_EDIT2, DialogStartNr);
		SetDialogIntValue(Dialog, IDC_EDIT3, DialogStep);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res =
			    SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50,
			                           (LPARAM) & DialogTextLine);
			DialogTextLine[res] = 0;

			if ((DialogTextLine[0] == 0) || (CheckNetName(DialogTextLine) == -1))
			{
				sprintf(sel, SC(110, "Net name %s is not valid"), DialogTextLine);
				MessageBoxUTF8(SCHWindow, sel, SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			strcpy(NetLabelText, DialogTextLine);

			if (GetDialogIntValue(Dialog, IDC_EDIT2, &DialogStartNr) == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(115, "Wrong value in Number start"), SC(38, "Error"),
				               MB_APPLMODAL | MB_OK);
				return about;
			}

			if (GetDialogIntValue(Dialog, IDC_EDIT3, &DialogStep) == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(116, "Wrong value in Number step"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("add_incremental_netlabel.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 AddNetlabelsDialog(LPSTR TextLine, int32 * StartNr, int32 * Step)
{
	int32 res, ok;

	DialogStartNr = *StartNr;
	DialogStep = *Step;
	NetLabelText = TextLine;
	res =
	    DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDLABELS), SCHWindow, (DLGPROC) AddNetlabelsDialog2);

	if (res == 1)
	{
		*StartNr = DialogStartNr;
		*Step = DialogStep;
	}

	ok = 1;
	return res;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InitDialogs()
{
	int32 cnt;

	memset(&AddedPins, 0, sizeof(AddedPins));

	for (cnt = 0; cnt < 20; cnt++)
		AddedPins[cnt].ConnectionType = 0x101;

	AddedPinBus.ConnectionType = 0x101;

}

//**************************************************************************************************************************
//*********************** IDD_DIALOG_MESSAGE *******************************************************************************
//**************************************************************************************************************************

int32 CALLBACK MessageDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(20, "Message"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));

		SendDlgItemBigMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) MessagePtr);
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
			EndDialog(Dialog, 1);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 MessageDialog(LPSTR Message)
{
	int32 res, first;

	MessagePtr = Message;
	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_MESSAGE), SCHWindow, (DLGPROC) MessageDialog2);

	first = 1;
	return res;
}

//**************************************************************************************************************************
//************* IDD_DIALOG_SHEETSYMBOLINFO * IDD_DIALOG_SYMBOLINFO20 *******************************************************
//**************************************************************************************************************************

int32 CALLBACK SymbolInfoDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, SymbolChanged, cnt, res, ok, cnt2, cnt3;
	char sel[MAX_LENGTH_STRING], DialogTextLine[8192], AttributeIdent[MAX_LENGTH_STRING],
	     AttributeValue[MAX_LENGTH_STRING];
	InstanceRecord *Instance;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(128, "Symbol info"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(99, "Symbol name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(118, "Name interface"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(119, "Initial reference"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(120, "Description"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(443, "Sheet symbol name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(123, "Properties"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(124, "Property ID"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(125, "Values"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(126, "Symbol protected"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(22, "Not visible"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(22, "Not visible"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK4, SC(127, "Multiple symbols"));

		Instance = FindFirstInstance(0);

		if (!EditingSheetSymbol)
		{
			if (WorkingSymbol->InterfaceName[0] == 0)
				strcpy(WorkingSymbol->InterfaceName, Instance->SymbolName);

			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) Instance->Value);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) WorkingSymbol->InterfaceName);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) Instance->Reference);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) WorkingSymbol->Description);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) Instance->Geometry);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) Instance->PartNr);

			if ((WorkingSymbol->Info & OBJECT_PROTECTED) == OBJECT_PROTECTED)
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

			if ((WorkingSymbol->Info & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS)
				SendDlgItemMessage(Dialog, IDC_CHECK4, BM_SETCHECK, 1, 0);

			if ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

			if ((Instance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
				SendDlgItemMessage(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);

			DialogTextLine[0] = 0;

			if (Instance->Geometry[0] != 0)
			{
				strcat(DialogTextLine, "GEOMETRY");
				strcat(DialogTextLine, "\r\n");
			}

			if (Instance->PartNr[0] != 0)
			{
				strcat(DialogTextLine, "PARTNR");
				strcat(DialogTextLine, "\r\n");
			}

			for (cnt = 0; cnt < MaxNrSymbolAttributes; cnt++)
			{
				if ((SymbolAttributesIdent[cnt][0] != 0) && (SymbolAttributesValue[cnt][0] != 0))
				{
					strcat(DialogTextLine, SymbolAttributesIdent[cnt]);
					strcat(DialogTextLine, "\r\n");
				}
			}

			SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
			DialogTextLine[0] = 0;

			if (Instance->Geometry[0] != 0)
			{
				strcat(DialogTextLine, Instance->Geometry);
				strcat(DialogTextLine, "\r\n");
			}

			if (Instance->PartNr[0] != 0)
			{
				strcat(DialogTextLine, Instance->PartNr);
				strcat(DialogTextLine, "\r\n");
			}

			for (cnt = 0; cnt < MaxNrSymbolAttributes; cnt++)
			{
				if ((SymbolAttributesIdent[cnt][0] != 0) && (SymbolAttributesValue[cnt][0] != 0))
				{
					strcat(DialogTextLine, SymbolAttributesValue[cnt]);
					strcat(DialogTextLine, "\r\n");
				}
			}

			SendDlgItemMessageUTF8(Dialog, IDC_EDIT8, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		}
		else
		{
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) Instance->Value);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) WorkingSymbol->Description);
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			Instance = FindFirstInstance(0);
			SymbolChanged = 0;

			if (!EditingSheetSymbol)
			{
				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
				sel[res] = 0;

				if (stricmpUTF8(sel, Instance->Value) != 0)
				{
					memmove(Instance->Value, &sel, sizeof(Instance->Value) - 1);
					SymbolChanged = 1;
				}

				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
				sel[res] = 0;

				if (stricmpUTF8(sel, WorkingSymbol->InterfaceName) != 0)
				{
					memmove(WorkingSymbol->InterfaceName, &sel, sizeof(WorkingSymbol->InterfaceName) - 1);
					SymbolChanged = 1;
				}

				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
				sel[res] = 0;

				if (stricmpUTF8(sel, Instance->Reference) != 0)
				{
					memmove(Instance->Reference, &sel, sizeof(Instance->Reference) - 1);
					SymbolChanged = 1;
				}

				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
				sel[res] = 0;

				if (stricmpUTF8(sel, WorkingSymbol->Description) != 0)
				{
					memmove(WorkingSymbol->Description, &sel, sizeof(WorkingSymbol->Description) - 1);
					SymbolChanged = 1;
				}

				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
				sel[res] = 0;

				if (stricmpUTF8(sel, Instance->Geometry) != 0)
				{
					memmove(Instance->Geometry, &sel, sizeof(Instance->Geometry) - 1);
					SymbolChanged = 1;
				}

				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) sel);
				sel[res] = 0;

				if (stricmpUTF8(sel, Instance->PartNr) != 0)
				{
					memmove(Instance->PartNr, &sel, sizeof(Instance->PartNr) - 1);
					SymbolChanged = 1;
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				{
					if ((WorkingSymbol->Info & OBJECT_PROTECTED) == 0)
					{
						WorkingSymbol->Info |= OBJECT_PROTECTED;
						SymbolChanged = 1;
					}
				}
				else
				{
					if ((WorkingSymbol->Info & OBJECT_PROTECTED) == OBJECT_PROTECTED)
					{
						WorkingSymbol->Info &= ~OBJECT_PROTECTED;
						SymbolChanged = 1;
					}
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK4, BM_GETCHECK, 0, 0) == 1)
				{
					if ((WorkingSymbol->Info & MULTIPLE_SYMBOLS) == 0)
					{
						WorkingSymbol->Info |= MULTIPLE_SYMBOLS;
						SymbolChanged = 1;
					}
				}
				else
				{
					if ((WorkingSymbol->Info & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS)
					{
						WorkingSymbol->Info &= ~MULTIPLE_SYMBOLS;
						SymbolChanged = 1;
					}
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == 1)
				{
					if ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == 0)
					{
						Instance->ValueInfo |= TEXT_NOT_VISIBLE;
						SymbolChanged = 1;
					}
				}
				else
				{
					if ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
					{
						Instance->ValueInfo &= ~TEXT_NOT_VISIBLE;
						SymbolChanged = 1;
					}
				}

				if (SendDlgItemMessage(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0) == 1)
				{
					if ((Instance->RefInfo & TEXT_NOT_VISIBLE) == 0)
					{
						Instance->RefInfo |= TEXT_NOT_VISIBLE;
						SymbolChanged = 1;
					}
				}
				else
				{
					if ((Instance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
					{
						Instance->RefInfo &= ~TEXT_NOT_VISIBLE;
						SymbolChanged = 1;
					}
				}

				cnt2 = 0;
				memset(&SymbolAttributesIdent, 0, sizeof(SymbolAttributesIdent));
				memset(&SymbolAttributesValue, 0, sizeof(SymbolAttributesValue));

				for (cnt = 0; cnt < MaxNrSymbolAttributes; cnt++)
				{
					memset(&AttributeIdent, 0, sizeof(AttributeIdent));
					memset(&AttributeValue, 0, sizeof(AttributeValue));
					res = GetDialogTextLine(Dialog, IDC_EDIT7, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

					if (DialogTextLine[0] != 0)
					{
						strncpy(AttributeIdent, DialogTextLine, 63);
						cnt3 = strlen(AttributeIdent);

						while ((cnt3 > 0) && (AttributeIdent[cnt3 - 1] == ' '))
							cnt3--;

						AttributeIdent[cnt3] = 0;
					}

					res = GetDialogTextLine(Dialog, IDC_EDIT8, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

					if (DialogTextLine[0] != 0)
					{
						strncpy(AttributeValue, DialogTextLine, 63);
						cnt3 = strlen(AttributeValue);

						while ((cnt3 > 0) && (AttributeValue[cnt3 - 1] == ' '))
							cnt3--;

						AttributeValue[cnt3] = 0;
					}

					if ((AttributeIdent[0] != 0) && (AttributeValue[0] != 0))
					{
						if (strcmp(AttributeIdent, "GEOMETRY") == 0)
						{
							memset(Instance->Geometry, 0, sizeof(Instance->Geometry));
							strncpy(Instance->Geometry, AttributeValue, 31);
							SymbolChanged = 1;
						}
						else
						{
							if (strcmp(AttributeIdent, "PARTNR") == 0)
							{
								memset(Instance->PartNr, 0, sizeof(Instance->PartNr));
								strncpy(Instance->PartNr, AttributeValue, 31);
								SymbolChanged = 1;
							}
							else
							{
								if (cnt2 < MaxNrSymbolAttributes)
								{
									strcpy(SymbolAttributesIdent[cnt2], AttributeIdent);
									strcpy(SymbolAttributesValue[cnt2], AttributeValue);
									SymbolChanged = 1;
									cnt2++;
								}
							}
						}
					}
				}

				ok = 1;
			}
			else
			{
				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, MAX_LENGTH_STRING, (LPARAM) sel);
				sel[res] = 0;

				if (stricmpUTF8(sel, WorkingSymbol->Description) != 0)
				{
					memmove(WorkingSymbol->Description, &sel, sizeof(WorkingSymbol->Description) - 1);
					SymbolChanged = 1;
				}
			}

			if (SymbolChanged)
			{
				DataBaseChanged = 1;
				EndDialog(Dialog, 1);
			}
			else
				EndDialog(Dialog, 2);

			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDABORT:
			EndDialog(Dialog, 3);
			return about;

		case IDHELP:
			Help("edit_symbolnames.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 SymbolInfoDialog(SymbolRecord * Symbol)
{
	int32 res;
	WorkingSymbol = Symbol;

	if (!EditingSheetSymbol)
	{
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SYMBOLINFO20), SCHWindow,
		              (DLGPROC) SymbolInfoDialog2);
	}
	else
	{
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SHEETSYMBOLINFO), SCHWindow,
		              (DLGPROC) SymbolInfoDialog2);
	}

	ok = 1;
	return res;
}

//*************************************************************************************************************************
//********************************* IDD_DIALOG_NUMBERINC ******************************************************************
//*************************************************************************************************************************

int32 CALLBACK NumberInputDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, TextChanged;
	char DialogTextLine[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(130, "Add text numbers incremental"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(112, "Start number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(113, "Step"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(129, "Count"));

		SetDialogIntValue(Dialog, IDC_EDIT1, ObjectNumbers.Start);
		SetDialogIntValue(Dialog, IDC_EDIT2, ObjectNumbers.Step);
		SetDialogIntValue(Dialog, IDC_EDIT3, ObjectNumbers.Count);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			TextChanged = 0;

			if (GetDialogIntValue(Dialog, IDC_EDIT1, &ObjectNumbers.Start) == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(115, "Wrong value in Number start"), SC(38, "Error"),
				               MB_APPLMODAL | MB_OK);
				return about;
			}

			if (GetDialogIntValue(Dialog, IDC_EDIT2, &ObjectNumbers.Step) == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(116, "Wrong value in Number step"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (GetDialogIntValue(Dialog, IDC_EDIT3, &ObjectNumbers.Count) == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(117, "Wrong value in Count"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("other_objects.htm", 0);
			break;

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

int32 NumberInputDialog()
{
	int32 res, ok;

	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NUMBERINC), SCHWindow, (DLGPROC) NumberInputDialog2);

	ok = 1;
	return res;
}

//***************************************************************************************************************************
//************************************* IDD_DIALOG_WIRELABEL ****************************************************************
//***************************************************************************************************************************

int32 CALLBACK WireLabelDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, TextChanged, cnt, res;
	char DialogTextLine[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetWindowTextUTF8(Dialog, SC(200, "Add wire/labels"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(108, "Net labels"));
	
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			TextChanged = 0;

			for (cnt = 0; cnt < 64; cnt++)
			{
				res = GetDialogTextLine(Dialog, IDC_EDIT1, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);
				memmove(&WireLabel[cnt], &DialogTextLine, sizeof(WireLabel[cnt]) - 1);
			}

			NrWireLabels = 64;

			while ((NrWireLabels > 0) && (strlen(WireLabel[NrWireLabels - 1]) == 0))
				NrWireLabels--;

			if (NrWireLabels == 0)
				EndDialog(Dialog, 2);
			else
				EndDialog(Dialog, 1);

			return about;

		case IDHELP:
			Help("add_netlabel_wire.htm", 0);
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

int32 AddWireLabelsDialog()
{
	int32 res, ok;

	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_WIRELABEL), SCHWindow, (DLGPROC) WireLabelDialog2);

	ok = 1;
	return res;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 InitGeometryLibEntries(HWND Dialog, LPSTR LibraryName)
{
	LibRecord Lib;
	LibNameRecord LibName;
	int32 cnt, NrLibEntries;
	int32 Libfp, result;
	char str[MAX_LENGTH_STRING];

	strcpy(str, LibraryName);

	if ((Libfp = FileOpenReadOnlyUTF8(str)) == -1)
		return 0;

	if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
		return 0;

	if (strcmp(Lib.Identification, GeometryLibraryCode1) == 0)
	{
		NrLibEntries = Lib.NrLibEntries;

		for (cnt = 0; cnt < (int32) min(NrLibEntries, 5000); cnt++)
		{
			if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
			{
				FileClose(Libfp);
				return 0;
			}

			SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LibName.Text));
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
	char str[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

	count = 0;
	sprintf(str, "%s\\*.shp", DirName);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		UnicodeToUtf8(FileInfo.cFileName, str, MAX_LENGTH_STRING - 50);
		lengte = strlen(str);
		str[lengte - 4] = 0;
		strupr(str);
		SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) str);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
		count++;
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	return count;
}

//*************************************************************************************************************************
//******************************** IDD_DIALOG_EDITGEOMETRY ****************************************************************
//*************************************************************************************************************************

int32 CALLBACK SelectGeomDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, hulp, TabStops[5], ok, LibMode, Found, Found2;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING * 5], InitStr[MAX_LENGTH_STRING],
	     ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(106, "Select geometry"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(136, "Geometry source"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(102, "Geometries"));
		

		if (GeometryDialogInitialX != -10000)
		{
			GetWindowRect(Dialog, &DialogWindowRect);
			MoveWindow(Dialog, GeometryDialogInitialX, GeometryDialogInitialY,
			           DialogWindowRect.right - DialogWindowRect.left, DialogWindowRect.bottom - DialogWindowRect.top,
			           1);
		}

		TabStops[0] = 140;
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);

		sprintf(str, SC(138, "Local geometry directory\t%s\\pcb\\shapes"), DesignPath);
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		sprintf(str, SC(139, "Global geometry directory\t%s\\shapes"), ProjectPath);
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);

		for (cnt = 0; cnt < NrGeometryLibFiles; cnt++)
		{
			if (cnt < NrGeometryLibraries)
				sprintf(str, SC(140, "Own geometry library\t%s"), GeometryLibNames[cnt]);
			else
				sprintf(str, SC(141, "Global geometry library\t%s"), GeometryLibNames[cnt]);

			SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		}

		sprintf(str, "%s\\pcb\\shapes", DesignPath);
		res = InitGeometriesEntries(Dialog, str);
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, 0, 0);

		if (res == 0)
		{
			sprintf(str, "%s\\shapes", ProjectPath);
			res = InitGeometriesEntries(Dialog, str);
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, 1, 0);
		}

		ActiveViewGeometry = 0;
		PostMessage(Dialog, WM_COMMAND, (WPARAM) ((LBN_SELCHANGE << 16) + IDC_LIST1), (LPARAM) NULL);
		res = 1;
		CurrentGeometryLibraryNr = -1;
		CurrentGeometryNrInLibrary = -1;
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
			Found = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
			Found2 = SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
			LibMode = 0;

			if (ClosingWindowMessage != 0)
				SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

			if ((Found != -1) && (Found2 != -1))
			{
				SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_GETTEXT, (WPARAM) Found2, (LPARAM) str);
				strcpy(GeometrieName, str);
				EndDialog(Dialog, 1);
			}

			return about;

		case IDCANCEL:
			if (ClosingWindowMessage != 0)
				SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("comp_geometry.htm", 0);
			break;

		case IDC_LIST1:
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				Found2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

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

						SendDlgItemMessage(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);
						InitGeometriesEntries(Dialog, str2);
						break;

					default:
						if (Found2 - 2 < NrGeometryLibFiles)
						{
							Found2 -= 2;
							sprintf(str2, "%s", GeometryLibNames[Found2]);
						}

						SendDlgItemMessage(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);
						InitGeometryLibEntries(Dialog, str2);
						break;
					}
				}
			}

			hulp = 1;
			break;

		case IDC_LIST2:
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				Found = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
				Found2 = SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
				LibMode = 0;

				if ((Found != -1) && (Found2 != -1))
				{
//               &&
//               (Found2!=CurrentGeometryNrInLibrary)) {
//              CurrentGeometryNrInLibrary=Found2;
					SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_GETTEXT, (WPARAM) Found2, (LPARAM) str);

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
					{
						sprintf(str3, SC(143, "Can not find geometry editor %s"), GeometrieEditor);
						MessageBoxUTF8(SCHWindow, str3, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					if (LibMode == 0)
					{
						if (FileExistsUTF8(str) == 0)
						{
							if (ClosingWindowMessage != 0)
							{
								SendMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);
								sprintf(ExeParams, "\"%s\" \"%s\" /S %i,%i,%i,%i %s /a /w %x /u \"%s\"",
								        GeometrieEditor, str, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY,
								        InitStr, (uint32) SCHWindow, ProjectPath);
								StartupInfo.cb = sizeof(StartupInfo);
								StartupInfo.wShowWindow = SW_SHOW;
								CreateProcess(GeometrieEditor, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo,
								              &ProcessInfo);
							}
						}
						else
						{
							sprintf(str3, SC(142, "Can not find geometry %s"), str);
							MessageBoxUTF8(SCHWindow, str3, SC(38, "Error"), MB_APPLMODAL | MB_OK);
							return about;
						}
					}
					else
					{
						sprintf(ExeParams, "\"%s\" \"%s\" /l \"%s\" /S %i,%i,%i,%i %s /a /w %x /u \"%s\"",
						        GeometrieEditor, str, str2, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY,
						        InitStr, (uint32) SCHWindow, ProjectPath);
						StartupInfo.cb = sizeof(StartupInfo);
						StartupInfo.wShowWindow = SW_SHOW;

						if (ClosingWindowMessage != 0)
						{
							SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);
							CreateProcess(GeometrieEditor, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo,
							              &ProcessInfo);
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

int32 SelectGeometrie()
{
	int32 res, ok, cnt;
	char SearchFileName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

	NrGeometryLibFiles = 0;
	memset(&GeometryLibNames, 0, sizeof(GeometryLibNames));

// Search in own libraries first
	for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
	{
		if (NrGeometryLibFiles < 64)
			strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt]);
	}

// Search in default libraries

	sprintf(SearchFileName, "%s\\shplib\\*.slb", ProjectPath);
	FileSearchHandle = FindFirstFileUTF8(SearchFileName, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while ((res) && (NrGeometryLibFiles < 64))
	{
		UnicodeToUtf8(FileInfo.cFileName, str, MAX_LENGTH_STRING - 100);
		sprintf(GeometryLibNames[NrGeometryLibFiles++], "%s\\shplib\\%s", ProjectPath, str);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	res =
	    DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_EDITGEOMETRY), SCHWindow, (DLGPROC) SelectGeomDialog2);

	if (ActiveViewGeometry)
	{
		ActiveViewGeometry = 0;
		ViewFull(1);
	}

	ok = 1;
	return res;
}

//******************************************************************************************************************
//***************************************** O programu IDD_DIALOG_ABOUT ********************************************
//******************************************************************************************************************

int32 CALLBACK AboutDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char str[MAX_LENGTH_STRING];
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(406, "About program"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(133, "Schematic editor PCB Elegance"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, "Web PCB Elegance"); //pøidán

		sprintf(str, SC(134, "\r\n Build version %i.%i.%i  ( %s )"), VER_VERSION / 100, VER_VERSION % 100, VER_BUILD, VER_DATE_STR);

#ifdef GCC_COMP
		strcat(str, "\r\n\r\n Compiled with mingw (gcc 4.9.2)");
#endif
#ifdef VC2005
		strcat(str, "\r\n\r\n Compiled with Microsoft Visual Studio 2005");
#endif
#ifdef VC2010
		strcat(str, SC(135, "\r\n\r\n Compiled with Microsoft Visual Studio 2022"));
#endif
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) str);
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

	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ABOUT), SCHWindow, (DLGPROC) AboutDialogBody);
	return res;
}

//*********************************************************************************************************************
//************************** IDD_DIALOG_COLOR *************************************************************************
//*********************************************************************************************************************

int32 CALLBACK ColorDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, ok, Selection;
	COLORREF NewColor;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(146, "Change colors"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(400, "Select color"));
		SetDialogItemTextUTF8(Dialog, ID_CHANGE_COLOR, SC(145, "Change color"));

		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, BackGroundColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, WireColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, BusColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, BusConnectionColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, GlobalConnectionColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, JunctionColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, NetLabelColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, InstanceRefTextColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, InstanceValueTextColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolPinColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolPinBusColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolPinTextColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolPowerPinTextColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolPinBusTextColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolLineColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolRectColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolCircleColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolArcColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, SymbolTextColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ObjectLineColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ObjectRectColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ObjectCircleColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ObjectArcColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ObjectTextColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ButtonInfoColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, GridColorNr);
		SelectedColorNr = -1;
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
		case IDC_LIST1:
			res = HIWORD(WParam);
			/*
			          switch (HIWORD(WParam)) {
			            case LBN_SELCHANGE:
			              if ((res=SendDlgItemMessageUTF8(Dialog,IDC_LIST1,LB_GETCURSEL,0,0))!=CB_ERR) {
			                NewColor=GetNewColor(0,SCHColors[res],Dialog);
			                if (NewColor!=-1) {
			                  SCHColors[res]=NewColor;
			                  DeleteGraphicObjects();
			                  KillDrawObjects();
			                  CreateDrawObjects();
			                  memset(&TempDrawItem,0,sizeof(DRAWITEMSTRUCT));
			                  TempDrawItem.CtlType=ODT_LISTBOX;
			                  TempDrawItem.CtlID=IDC_LIST1;
			                  TempDrawItem.itemData=res;
			                  TempDrawItem.itemAction=ODA_DRAWENTIRE;
			                  DrawSpecialItem(&TempDrawItem);
			                }
			              }
			              break;
			          }
			*/
			break;

		case ID_CHANGE_COLOR:
			if ((Selection = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0)) != CB_ERR)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETITEMDATA, Selection, 0);
				NewColor = GetNewColor(0, SCHColors[res], Dialog);

				if (NewColor != -1)
				{
					SCHColors[res] = NewColor;
					DeleteGraphicObjects();
					CreateDrawObjects(0);
					RePaint();
					CheckInputMessages(0);
					CheckInputMessages(0);
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
					SendMessage(Dialog, WM_INITDIALOG, 0, 0);
					SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, Selection, 0);
				}
			}

			break;

		case IDHELP:
			Help("change_colors.htm", 0);
			break;

		case IDOK:
			EndDialog(Dialog, 1);
			return about;

//        case IDHELP:
//          Help("Plot_output_to_gerber_format,0);
//          return about;
		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 ColorDialog(int32 mode)
{
	int32 res;

	SelectedColorNr = -1;
	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COLOR), SCHWindow, (DLGPROC) ColorDialogBody);
	return res;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

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
	else
	{
	}

	return (COLORREF) - 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetInfoPinName(LPSTR PinName)
{
	int32 cnt, res;
	PinRecord *Pin;
	PinBusRecord *PinBus;

	res = 0;

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((Pin->Info & 0x10) == 0)
			{
				if (stricmpUTF8(Pin->Name, PinName) == 0)
				{
					res |= 256;
					Pin->Info |= 0x10;
					return res;
				}
			}
			else
			{
				if (stricmpUTF8(Pin->Name, PinName) == 0)
				{
					res |= 1024;
					return res;
				}
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((PinBus->Info & 0x10) == 0)
			{
				if (stricmpUTF8(PinBus->Label, PinName) == 0)
				{
					res |= 512 + PinBus->NrPins;
					PinBus->Info |= 0x10;
					return res;
				}
			}
			else
			{
				if (stricmpUTF8(PinBus->Label, PinName) == 0)
				{
					res |= 1024;
					return res;
				}
			}
		}
	}

	return res;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ConvertGatePinSwap(HWND SCHWindow)
{
	int32 cnt, cnt2, cnt3, cnt4, StrLength, ok, PinCnt, GroupCodes[16], GroupLines[16], res, BracePos1, BracePos2, pos1,
	      pos2, LineNr, PinInfo[16][64], NrPinsLine[16], PinGroupCnt, PinCodeNr;
	char str[MAX_LENGTH_STRING], PinNames[16][65][24];
	int32 Error, Changed;
	LPSTR PinName;
	int32 SwapInfo, TempLastActionNr;
	PinRecord *Pin, NewPin;
	PinBusRecord *PinBus, NewPinBus;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < 16; cnt++)
		GroupLines[cnt] = 0;

	for (cnt = 0; cnt < 16; cnt++)
	{
		GroupCodes[cnt] = 0;

		if (CodeString[cnt][0] != 0)
		{
			if (sscanf(CodeString[cnt], "%i", &GroupCodes[cnt]) != 1)
			{
				sprintf(str, SC(147, "Wrong paramater (%s) in line %i"), CodeString[cnt], cnt + 1);
				MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return -1;
			}

			if ((GroupCodes[cnt] < 1) || (GroupCodes[cnt] > 15))
			{
				sprintf(str, SC(148, "Code between 1 and 15 in line %i"), cnt + 1);
				MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return -1;
			}

			GroupLines[GroupCodes[cnt]]++;

			if (PinString[cnt][0] == 0)
			{
				sprintf(str, SC(149, "Pin field is empty in line %i"), cnt + 1);
				MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return -1;
			}
		}
	}

	for (cnt = 1; cnt < 16; cnt++)
	{
		if (GroupLines[cnt] > 15)
		{
			sprintf(str, SC(150, "Nr lines code %i is greater than 16"), cnt);
			MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
			return -1;
		}
	}

	for (cnt = 1; cnt < 16; cnt++)
	{
		if (GroupLines[cnt] > 0)
		{
			LineNr = 0;
			memset(&PinInfo, 0, sizeof(PinInfo));

			for (cnt2 = 0; cnt2 < 16; cnt2++)
				NrPinsLine[cnt2] = 0;

			for (cnt2 = 0; cnt2 < 16; cnt2++)
			{
				if (GroupCodes[cnt2] == cnt)
				{
					PinCodeNr = -1;
					PinGroupCnt = 0;

					if (LineNr == 16)
					{
						sprintf(str, SC(151, "Nr lines of one code limited to 16"));
						MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return -1;
					}

					StrLength = strlen(PinString[cnt2]);
					cnt3 = 0;
					PinCnt = 0;
					BracePos1 = -1;
					BracePos2 = -1;

					if (StrLength == 0)
						Error = 1;
					else
						Error = 0;

					while ((!Error) && (cnt3 < StrLength))
					{
						pos1 = cnt3;
						pos2 = cnt3;

						if (PinString[cnt2][cnt3] == '(')
						{
							BracePos1 = cnt3;
							cnt3++;
							pos1 = cnt3;

							if (cnt3 < StrLength)
							{
								if (PinString[cnt2][cnt3] == ')')
									Error = 1;
							}
							else
								Error = 1;
						}

						if (!Error)
						{
							while ((cnt3 < StrLength) && (PinString[cnt2][cnt3] != ',')
							        && (PinString[cnt2][cnt3] != ')'))
								cnt3++;

							pos2 = cnt3;

							if ((cnt3 == StrLength) && (BracePos1 != -1))
								Error = 1;

							if (PinString[cnt2][cnt3] == ')')
							{
								BracePos2 = cnt3;

								if (BracePos1 != -1)
								{
									cnt3++;

									if (cnt3 < StrLength)
									{
										if (PinString[cnt2][cnt3] != ',')
											Error = 1;
										else
											cnt3++;
									}
								}
								else
									Error = 1;
							}
							else
							{
								if (PinString[cnt2][cnt3] == ',')
									cnt3++;
							}

							if (!Error)
							{
								if (((pos2 - pos1) > 0) && (pos2 - pos1 < 24))
								{
									memmove(&PinNames[LineNr][PinCnt], &PinString[cnt2][pos1], pos2 - pos1);
									PinNames[LineNr][PinCnt][pos2 - pos1] = 0;

									for (cnt4 = 0; cnt4 < (int32) strlen(PinNames[LineNr][PinCnt]); cnt4++)
									{
										if (PinNames[LineNr][PinCnt][cnt4] == ' ')
											Error = 1;
									}

									if (!Error)
									{
										res = GetInfoPinName(PinNames[LineNr][PinCnt]);

										if (res == 0)
										{
											sprintf(str, SC(152, "Pin or Pinbus %s in line %i does not exist"),
											        PinNames[LineNr][PinCnt], cnt2 + 1);
											MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
											return -1;
										}
										else
										{
											if ((res & 1024) == 1024)
											{
												sprintf(str, SC(153, "Double pin or pinbus %s in line %i"),
												        PinNames[LineNr][PinCnt], cnt2 + 1);
												MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
												return -1;
											}
											else
											{
												PinInfo[LineNr][PinCnt] |= res;

												if (PinGroupCnt == 0)
													PinCodeNr++;

												PinInfo[LineNr][PinCnt] |= PinCodeNr << 12;

												if (BracePos1 != -1)
													PinInfo[LineNr][PinCnt] |= 2048;

												PinCnt++;

												if (PinCnt == 65)
												{
													sprintf(str, SC(154, "Nr pin names in one line is limited to 64"));
													MessageBoxUTF8(SCHWindow, str, SC(38, "Error"),
													               MB_APPLMODAL | MB_OK);
													return -1;
												}

												NrPinsLine[LineNr] = PinCnt;

												if (BracePos1 != -1)
													PinGroupCnt++;

												if (BracePos2 != -1)
												{
													BracePos1 = -1;
													BracePos2 = -1;
													PinGroupCnt = 0;
												}
											}
										}
									}
								}
								else
									Error = 1;
							}
						}
					}

					if (Error)
					{
						sprintf(str, SC(155, "Error in line %i"), cnt2 + 1);
						MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return -1;
					}

					LineNr++;
				}
			}

			ok = 1;

			if (LineNr > 1)
			{
				cnt3 = NrPinsLine[0];

				for (cnt2 = 1; cnt2 < LineNr; cnt2++)
				{
					if (NrPinsLine[cnt2] != cnt3)
					{
						sprintf(str, SC(156, "Nr pins in lines code %i do not match"), cnt);
						MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return -1;
					}
				}

				for (cnt2 = 1; cnt2 < LineNr; cnt2++)
				{
					for (cnt4 = 0; cnt4 < cnt3; cnt4++)
					{
						if (PinInfo[0][cnt4] != PinInfo[cnt2][cnt4])
						{
							sprintf(str, SC(157, "Not the same pins,pinbusses in lines code %i"), cnt);
							MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
							return -1;
						}
					}
				}
			}

			for (cnt2 = 0; cnt2 < LineNr; cnt2++)
			{
				PinCodeNr = -1;

				for (cnt4 = 0; cnt4 < NrPinsLine[cnt2]; cnt4++)
				{
					ok = 1;
					PinGroupCnt = PinInfo[cnt2][cnt4] >> 12;
					SwapInfo = (cnt << 12) + (cnt2 << 8) + (PinGroupCnt << 1);

					if ((PinInfo[cnt2][cnt4] & 2048) == 2048)
						SwapInfo++;

					PinName = PinNames[cnt2][cnt4];

					for (cnt3 = 0; cnt3 < DesignSymbol.NrPins; cnt3++)
					{
						Pin = &((*Pins)[cnt3]);

						if (((Pin->Info & OBJECT_NOT_VISIBLE) == 0) && (Pin->AddNr <= TempLastActionNr))
						{
							if (stricmpUTF8(Pin->Name, PinName) == 0)
							{
								memmove(&NewPin, Pin, sizeof(PinRecord));
								NewPin.SwapInfo = (int16) SwapInfo;
								NewPin.Info &= ~0x10;
								AddPin(&NewPin);
								Changed = 1;
							}
						}
					}

					for (cnt3 = 0; cnt3 < DesignSymbol.NrPinBusses; cnt3++)
					{
						PinBus = &((*PinBusses)[cnt3]);

						if (((PinBus->Info & OBJECT_NOT_VISIBLE) == 0) && (PinBus->AddNr <= TempLastActionNr))
						{
							if (stricmpUTF8(PinBus->Label, PinName) == 0)
							{
								memmove(&NewPinBus, PinBus, sizeof(PinBusRecord));
								NewPinBus.SwapInfo = (int16) SwapInfo;
								NewPinBus.Info &= ~0x10;
								AddPinBus(&NewPinBus);
								Changed = 1;
							}
						}
					}

				}
			}

			ok = 1;
		}
	}

	if (Changed)
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Pin->Info & 0x10) == 0x10)
				{
					Pin->DeleteNr = (int16) LastActionNr;
					Pin->Info |= OBJECT_NOT_VISIBLE;
				}
				else
				{
					if ((Pin->AddNr <= TempLastActionNr) && (Pin->SwapInfo != 0))
					{
						memmove(&NewPin, Pin, sizeof(PinRecord));
						NewPin.SwapInfo = 0;
						AddPin(&NewPin);
						Pin->DeleteNr = (int16) LastActionNr;
						Pin->Info |= OBJECT_NOT_VISIBLE;
					}
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((PinBus->Info & 0x10) == 0x10)
				{
					PinBus->DeleteNr = (int16) LastActionNr;
					PinBus->Info |= OBJECT_NOT_VISIBLE;
				}
				else
				{
					if ((PinBus->AddNr <= TempLastActionNr) && (PinBus->SwapInfo != 0))
					{
						memmove(&NewPinBus, PinBus, sizeof(PinBusRecord));
						NewPinBus.SwapInfo = 0;
						AddPinBus(&NewPinBus);
						PinBus->DeleteNr = (int16) LastActionNr;
						PinBus->Info |= OBJECT_NOT_VISIBLE;
					}
				}
			}
		}
	}

	return 0;
}

//**************************************************************************************************************************
//********************************* IDD_DIALOG_GATEPINSWAP *****************************************************************
//**************************************************************************************************************************

int32 CALLBACK GatePinSwapDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, NewPinGroup, LineIndexExists, FirstLine, FirstPinSwap, FirstPinPlaced, cnt, SwapInfo, TestSwapInfo,
	      res, Group, LineIndex, PinIndex, MaxLineIndex, MaxPinIndex, GroupCount;
	char str[MAX_LENGTH_STRING], GroupStr[MAX_LENGTH_STRING], PinStr[MAX_LENGTH_STRING],
	     DialogTextLine[MAX_LENGTH_STRING];
	PinRecord *Pin;
	PinBusRecord *PinBus;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(176, "Gate/pin swap"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(158, "Code"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(78, "Pins"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(160, "Line 1"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(161, "Line 2"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(162, "Line 3"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(163, "Line 4"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(164, "Line 5"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(165, "Line 6"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(166, "Line 7"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(167, "Line 8"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(168, "Line 9"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(169, "Line 10"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(170, "Line 11"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC14, SC(171, "Line 12"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC15, SC(172, "Line 13"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC16, SC(173, "Line 14"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC17, SC(174, "Line 15"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC18, SC(175, "Line 16"));

		GroupStr[0] = 0;
		PinStr[0] = 0;
		FirstLine = 0;

		for (Group = 1; Group < 16; Group++)
		{
			MaxLineIndex = 0;
			MaxPinIndex = 0;
			GroupCount = 0;
			NewPinGroup = 1;

			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if (((Pin->Info & OBJECT_NOT_VISIBLE) == 0) && ((Pin->SwapInfo >> 12) == Group))
				{
					SwapInfo = Pin->SwapInfo;
					MaxLineIndex = max(MaxLineIndex, (SwapInfo >> 8) & 0x0f);
					MaxPinIndex = max(MaxPinIndex, (SwapInfo >> 1) & 0x7f);
					GroupCount++;
				}
			}

			for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
			{
				PinBus = &((*PinBusses)[cnt]);

				if (((PinBus->Info & OBJECT_NOT_VISIBLE) == 0) && ((PinBus->SwapInfo >> 12) == Group))
				{
					SwapInfo = PinBus->SwapInfo;
					MaxLineIndex = max(MaxLineIndex, (SwapInfo >> 8) & 0x0f);
					MaxPinIndex = max(MaxPinIndex, (SwapInfo >> 1) & 0x7f);
					GroupCount++;
				}
			}

			MaxLineIndex++;
			MaxPinIndex++;

			if (GroupCount > 0)
			{
// ****************************************************************************************************
//          sprintf(str,"%i",Group);
//          strcat(GroupStr,str);
				for (LineIndex = 0; LineIndex < MaxLineIndex; LineIndex++)
				{
					LineIndexExists = 0;
					FirstPinPlaced = 0;

					for (PinIndex = 0; PinIndex < MaxPinIndex; PinIndex++)
					{
						TestSwapInfo = (Group << 12) + (LineIndex << 8) + (PinIndex << 1);
						FirstPinSwap = 0;

						for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
						{
							Pin = &((*Pins)[cnt]);

							if (((Pin->Info & OBJECT_NOT_VISIBLE) == 0) && ((Pin->SwapInfo & 0xfffe) == TestSwapInfo))
							{
								if (!LineIndexExists)
								{
									if (FirstLine)
									{
										strcat(GroupStr, "\r\n");
										strcat(PinStr, "\r\n");
									}

									FirstLine = 1;
									sprintf(str, "%i", Group);
									strcat(GroupStr, str);
								}

								LineIndexExists = 1;

								if ((Pin->SwapInfo & 1) == 1)
								{
									if (FirstPinPlaced)
										strcat(PinStr, ",");

									if (!FirstPinSwap)
										strcat(PinStr, "(");

									FirstPinSwap = 1;
								}
								else
								{
									if (FirstPinSwap)
										strcat(PinStr, ")");

									if (FirstPinPlaced)
										strcat(PinStr, ",");

									FirstPinSwap = 0;
								}

								strcat(PinStr, Pin->Name);
								FirstPinPlaced = 1;
							}
						}

						for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
						{
							PinBus = &((*PinBusses)[cnt]);

							if (((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
							        && ((PinBus->SwapInfo & 0xfffe) == TestSwapInfo))
							{
								if (!LineIndexExists)
								{
									if (FirstLine)
									{
										strcat(GroupStr, "\r\n");
										strcat(PinStr, "\r\n");
									}

									FirstLine = 1;
									sprintf(str, "%i", Group);
									strcat(GroupStr, str);
								}

								LineIndexExists = 1;

								if (FirstPinSwap)
									strcat(PinStr, ")");

								FirstPinSwap = 0;

								if (FirstPinPlaced)
									strcat(PinStr, ",");

								if ((PinBus->SwapInfo & 1) == 1)
								{
									strcat(PinStr, "(");
									strcat(PinStr, PinBus->Label);
									strcat(PinStr, ")");
								}
								else
									strcat(PinStr, PinBus->Label);

								FirstPinPlaced = 1;
							}
						}

						if (FirstPinSwap)
							strcat(PinStr, ")");
					}
				}

//          strcat(GroupStr,"\r\n");
//          strcat(PinStr,"\r\n");
// ****************************************************************************************************
			}
		}

		/*
		          DialogTextLine[count2  ]='\r';
		          DialogTextLine[count2+1]='\n';
		        }
		      }
		*/
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) GroupStr);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) PinStr);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			for (cnt = 0; cnt < 15; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				CodeString[cnt][0] = 0;
				res = GetDialogTextLine(Dialog, IDC_EDIT1, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

				if (res > 2)
				{
					sprintf(str, SC(177, "Maximum number of characters in code is 2"));
					MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				strcpy(CodeString[cnt], DialogTextLine);
				PinString[cnt][0] = 0;
				res = GetDialogTextLine(Dialog, IDC_EDIT2, cnt, DialogTextLine, MAX_LENGTH_STRING - 50);

				if (res > 127)
				{
					sprintf(str, SC(178, "Maximum number of characters in pin is 127"));
					MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				strcpy(PinString[cnt], DialogTextLine);
			}

			if ((res = ConvertGatePinSwap(SCHWindow)) != 0)
				return about;

			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
//          memset(DialogTextLine,0,200);
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("edit_gate_pin_swap.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 GatePinSwapDialog(int32 Mode)
{
	int32 cnt, res, ok;
	PinRecord *Pin;
	PinBusRecord *PinBus;

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
			Pin->Info &= ~0x10;
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			PinBus->Info &= ~0x10;
	}

	DialogMode = Mode;
	res =
	    DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_GATEPINSWAP), SCHWindow, (DLGPROC) GatePinSwapDialog2);

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
			Pin->Info &= ~0x10;
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			PinBus->Info &= ~0x10;
	}

	ok = 1;
	return res;
}

//*************************************************************************************************************************
//***************************** IDD_DIALOG_PINBUSREORDER ******************************************************************
//*************************************************************************************************************************

int32 CALLBACK PinBusReorderDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, Error;
	int32 cnt, res, hulp;
	char sel[MAX_LENGTH_STRING], sel2[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(182, "Change pin bus reorder"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(98, "Reference"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(179, "Pin bus label"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(79, "Nr pins"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(181, "Reorder"));

		sprintf(sel, "%i", ObjectNumbers.Start);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) NewRedefinedPinBus.Reference);
		sprintf(sel, "%i", ObjectNumbers.Step);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) NewRedefinedPinBus.Name);
		sprintf(sel, "%i", NewRedefinedPinBus.Info2);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) & sel);
		sel[0] = 0;

		for (cnt = 0; cnt < min(64, NewRedefinedPinBus.Info2); cnt++)
		{
			sprintf(sel2, "%i,", NewRedefinedPinBus.Order[cnt]);
			strcat(sel, sel2);
		}

		sel[strlen(sel) - 1] = 0;
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) & sel);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:

			/*
			          memset(DialogTextLine,0,200);
			          if (res=SendDlgItemMessageUTF8(Dialog,IDC_EDIT1,EM_GETLINE,31,(LPARAM)DialogTextLine)==0) {
			            MessageBoxUTF8(SCHWindow,"Reference is empty",SC(38,"Error"),MB_APPLMODAL|MB_OK);
			            return about;
			          }
			          strcpy(NewRedefinedPinBus.Reference,DialogTextLine);
			          memset(DialogTextLine,0,200);
			          if (res=SendDlgItemMessageUTF8(Dialog,IDC_EDIT2,EM_GETLINE,31,(LPARAM)DialogTextLine)==0) {
			            MessageBoxUTF8(SCHWindow,"Pinbus label is empty",SC(38,"Error"),MB_APPLMODAL|MB_OK);
			            return about;
			          }
			          strcpy(NewRedefinedPinBus.Name,DialogTextLine);
			          if ((sscanf(DialogTextLine,"%i",&hulp)!=1)
			             ||
			             (hulp<1)
			             ||
			             (hulp>64)) {
			            MessageBoxUTF8(SCHWindow,SC(90,"Wrong value in nr pins"),SC(38,"Error"),MB_APPLMODAL|MB_OK);
			            return about;
			          }
			          NewRedefinedPinBus.Info2=hulp;
			*/
			if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, 199, (LPARAM) DialogTextLine)) == 0)
			{
				MessageBoxUTF8(SCHWindow, SC(183, "Pinbus reorder is empty"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			DialogTextLine[res] = 0;

			Error = 0;

			for (cnt = 0; cnt < NewRedefinedPinBus.Info2; cnt++)
			{
				res = GetPinNameFromPinBus(DialogTextLine, sel, NewRedefinedPinBus.Info2, cnt);

				if (res < 0)
					Error = 1;

				if ((sscanf(sel, "%i", &hulp) != 1) || (hulp < 0) || (hulp >= NewRedefinedPinBus.Info2))
					Error = 1;

				if (!Error)
					NewRedefinedPinBus.Order[cnt] = (uint8) hulp;
			}

			if (Error)
			{
				MessageBoxUTF8(SCHWindow, SC(184, "Error in pinbus reorder"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("edit_pinbus_reorder.htm", 0);
			break;

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

int32 PinBusReorderDialog(RedefinedPinBusRecord * RedefinedPinBus, int32 mode)
{
	int32 res, ok;

	memmove(&NewRedefinedPinBus, RedefinedPinBus, sizeof(RedefinedPinBusRecord));

	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PINBUSREORDER), SCHWindow,
	              (DLGPROC) PinBusReorderDialog2);

	if (res == 1)
		memmove(RedefinedPinBus, &NewRedefinedPinBus, sizeof(RedefinedPinBusRecord));

	ok = 1;
	return res;
}

/*
  hwndHBar = CreateWindow ("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_HORZ,
    0,0,0,0, hwnd, (HMENU) 1, hInstance, NULL);
  hwndVBar = CreateWindow ("scrollbar", NULL, WS_CHILD | WS_VISIBLE | SBS_VERT,
    0,0,0,0, hwnd, (HMENU) 2, hInstance, NULL);
  hwndStatusBar = CreateStatusWindow (
     CCS_BOTTOM | WS_CHILD | WS_VISIBLE | SBARS_SIZEGRIP,
    "Ready", hwnd, 2);

*/

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetSymbolInfo(LPSTR FileName, LPSTR SymbolName, int32 Mode, LPSTR Info)
{
	int32 cnt, result, Designfp, Libfp, NrLibEntries;
	LibRecord Lib;
	int32 Found = 0;
	LibNameRecord LibName;
	SymbolRecord Symbol;

	memset(&Symbol, 0, sizeof(SymbolRecord));

	if (Mode == 0)
	{
		if ((Libfp = FileOpenReadOnlyUTF8(FileName)) == -1)
			return 0;

		if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
			return 0;

		if (strcmp(Lib.Identification, LibraryCode1) == 0)
		{
			NrLibEntries = Lib.NrLibEntries;
			cnt = 0;

			while ((!Found) && (cnt < NrLibEntries))
			{
				if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
					return 0;

				if (stricmpUTF8(LibName.Text, SymbolName) == 0)
				{
					FileSeek(Libfp, LibName.Pos);

					if ((FileRead(Libfp, &Symbol, sizeof(SymbolRecord), &result) != -1)
					        && ((strcmp(Symbol.SymbolIdent, SymbolCode1) == 0)
					            || (strcmp(Symbol.SymbolIdent, SymbolCode2) == 0)
					            || (strcmp(Symbol.SymbolIdent, SymbolCode3) == 0)))
					{
						strcpy(Info, Symbol.Description);
						return 1;
					}

					FileClose(Libfp);
					return 0;
				}

				cnt++;
			}
		}

		FileClose(Libfp);
		return 0;
	}

	if ((Designfp = FileOpenReadOnlyUTF8(FileName)) == -1)
		return 0;

	if ((FileRead(Designfp, &Symbol, sizeof(SymbolRecord), &result) != -1)
	        && ((strcmp(Symbol.SymbolIdent, SymbolCode1) == 0) || (strcmp(Symbol.SymbolIdent, SymbolCode2) == 0)
	            || (strcmp(Symbol.SymbolIdent, SymbolCode3) == 0)))
	{
		strcpy(Info, Symbol.Description);
		return 1;
	}

	FileClose(Designfp);
	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 InitSymbolEntries(HWND Dialog)
{
	LibRecord Lib;
	LibNameRecord LibName;
	int32 cnt, cnt2, NrLibEntries, count, Libfp, result, res, lengte;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING];
	WCHAR str4[MAX_LENGTH_STRING], SearchString2W[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;


	Utf8ToUnicode(SearchString2, SearchString2W, MAX_LENGTH_STRING - 100);

// ********************************************************************************************************
	count = 0;
	sprintf(str, "%s\\sym\\*.sym", DesignPath);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		wcscpy(str4, FileInfo.cFileName);
		lengte = wcslen(str4);
		str4[lengte - 4] = 0;
		lengte -= 4;
		wcsupr(str4);

		if (((SearchString2W[0] != '*')
		        && (wcsncmp(str4, SearchString2W, min((int32) wcslen(SearchString2W), lengte)) == 0))
		        || ((SearchString2W[0] == '*') && (wcsstr(str4, (WCHAR *) & SearchString2W[1]) != 0)))
		{
			UnicodeToUtf8(FileInfo.cFileName, str5, MAX_LENGTH_STRING - 100);
			UnicodeToUtf8(str4, str2, MAX_LENGTH_STRING - 100);
			sprintf(str3, "%s\t%s\\sym\\%s", str2, DesignPath, str5);
			SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str3);
			count++;
		}

		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

// ********************************************************************************************************
	sprintf(str, "%s\\sym\\*.sym", ProjectPath);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		wcscpy(str4, FileInfo.cFileName);
		lengte = wcslen(str4);
		str4[lengte - 4] = 0;
		lengte -= 4;
		wcsupr(str4);

		if (((SearchString2W[0] != '*')
		        && (wcsncmp(str4, SearchString2W, min((int32) wcslen(SearchString2W), lengte)) == 0))
		        || ((SearchString2W[0] == '*') && (wcsstr(str4, (WCHAR *) & SearchString2W[1]) != 0)))
		{
			UnicodeToUtf8(FileInfo.cFileName, str5, MAX_LENGTH_STRING - 100);
			UnicodeToUtf8(str4, str2, MAX_LENGTH_STRING - 100);
			sprintf(str3, "%s\t%s\\sym\\%s", str2, ExePath, str5);
			SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str3);
			count++;
		}

		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

// ********************************************************************************************************
	NrLibFiles = 0;
	memset(&LibNames, 0, sizeof(LibNames));

// Add own libraries first

	for (cnt = 0; cnt < NrSchematicSymbolLibraries; cnt++)
	{
		if (NrLibFiles < 128)
			strcpy(LibNames[NrLibFiles++], SchematicSymbolLibraries[cnt]);
	}

// Add global libraries

	sprintf(str, "%s\\lib\\*.lib", ProjectPath);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while ((res) && (NrLibFiles < 128))
	{
		UnicodeToUtf8(FileInfo.cFileName, str5, MAX_LENGTH_STRING - 100);
		sprintf(LibNames[NrLibFiles], "%s\\lib\\%s", ProjectPath, str5);
		NrLibFiles++;

		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	for (cnt2 = 0; cnt2 < NrLibFiles; cnt2++)
	{
		strcpy(str, LibNames[cnt2]);
#ifdef _DEBUG

		if (stricmp(str, "e:\\pcb_elegance35\\lib\\74ls.lib") == 0)
			res = 1;

#endif

		if ((Libfp = FileOpenReadOnlyUTF8(str)) == -1)
			return 0;

		if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
			return 0;

		if (strcmp(Lib.Identification, LibraryCode1) == 0)
		{
			NrLibEntries = Lib.NrLibEntries;

			for (cnt = 0; cnt < (int32) min(NrLibEntries, 5000); cnt++)
			{
				FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result);
				Utf8ToUnicode(LibName.Text, str4, MAX_LENGTH_STRING - 100);
				strcpy(str2, LibName.Text);
				wcsupr(str4);
				lengte = wcslen(str4);

				if (((SearchString2W[0] != '*')
				        && (wcsncmp(str4, SearchString2W, min((int32) wcslen(SearchString2W), lengte)) == 0))
				        || ((SearchString2W[0] == '*') && (wcsstr(str4, (WCHAR *) & SearchString2W[1]) != 0)))
				{
					strcat(str2, "\t");
					strcat(str2, str);
					SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str2);
					count++;
				}
			}
		}

		FileClose(Libfp);
	}

	return count;
}

//*********************************************************************************************************************
//************************ IDD_DIALOG_ADDSYMBOL2 **********************************************************************
//*********************************************************************************************************************

int32 CALLBACK AddSymbolOnShortCut2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, Found, count, TabStops[5];
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     InfoStr[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(189, "Symbol source"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(128, "Symbol info"));
		SetWindowTextUTF8(Dialog, SC(187, "Select symbol"));

		TabStops[0] = 100;
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);
		count = InitSymbolEntries(Dialog);
		sprintf(str, SC(188, "Select symbol [ %d symbols ]"), count);
		SetWindowTextUTF8(Dialog, str);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDCANCEL:
			EndDialog(Dialog, 2);
			RePaint();
			return about;

		case IDOK:
		case IDC_LIST1:
			Found = (int32)SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (Found != -1)
			{
				SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETTEXT, Found, (LPARAM) str);
				GetStringTab(str, str2);
				GetStringTab(str, str3);
				str4[0] = 0;
				GetExtensionFileName(str4, str3);

				if (str4[0] != 0)
				{
					if (stricmp(str4, "lib") == 0)
						GetSymbolInfo(str3, str2, 0, InfoStr);
					else
						GetSymbolInfo(str3, str2, 1, InfoStr);

					if (((LOWORD(WParam)) == IDC_LIST1) && (HIWORD(WParam) == LBN_SELCHANGE))
					{
						SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & InfoStr);
						break;
					}
				}

				if ((LOWORD(WParam)) == IDOK)
				{
					if (str4[0] != 0)
					{
						strcpy(NewSymbolName, str2);

						if (stricmp(str4, "lib") == 0)
						{
							strcpy(NewLibName, str3);
							NewSymbolDir[0] = 0;
						}
						else
						{
							strcpy(NewSymbolDir, str3);
							NewLibName[0] = 0;
						}

						EndDialog(Dialog, 1);
						return about;
					}
				}
			}

			break;

		case IDHELP:
			Help("add_symbol_on_short_cut.htm", 0);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 AddSymbolOnShortCut(int32 mode)
{
	int32 res;

	if (TextInputDialog(&SymbolText, 8) == 2)
		return -1;

	if (SymbolText.Text[0] == 0)
		return -1;

	if (strcmp(SymbolText.Text, "*") == 0)
		return -1;

	strcpy(SearchString2, SymbolText.Text);
	struprUTF8(SearchString2);
	res =
	    DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDSYMBOL2), SCHWindow,
	              (DLGPROC) AddSymbolOnShortCut2);

	if (res == 1)
		GetSymbol(NewSymbolName, NewSymbolDir, NewLibName, 0);

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 InitLibEntries(HWND Dialog, LPSTR LibraryName)
{
	LibRecord Lib;
	LibNameRecord LibName;
	int32 cnt, NrLibEntries;
	int32 Libfp, result;
	char str[MAX_LENGTH_STRING];

	strcpy(str, LibraryName);

	if ((Libfp = FileOpenReadOnlyUTF8(str)) == -1)
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

			SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) (LibName.Text));
		}
	}

	FileClose(Libfp);
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 InitSymbolsEntries(HWND Dialog, LPSTR DirName)
{
	int32 res, lengte;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

	sprintf(str, "%s\\*.sym ", DirName);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		lengte = strlen(str2);
		str2[lengte - 4] = 0;
		strupr(str2);
		SendDlgItemMessage(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) str2);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	return 1;
}

//************************************************************************************************************************
//*********************************** IDD_DIALOG_ADDSYMBOL ***************************************************************
//************************************************************************************************************************

int32 CALLBACK SelectSymbolDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, res, res2, hulp, TabStops[5], Found, Found2;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];


	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(189, "Symbol source"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(190, "Symbols"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(128, "Symbol info"));
		SetWindowTextUTF8(Dialog, SC(187, "Select symbol"));

		SystemBusyMode = 40;

		if (SymbolDialogInitialX != -10000)
		{
			GetWindowRect(Dialog, &DialogWindowRect);
			MoveWindow(Dialog, SymbolDialogInitialX, SymbolDialogInitialY,
			           DialogWindowRect.right - DialogWindowRect.left, DialogWindowRect.bottom - DialogWindowRect.top,
			           1);
		}

		TabStops[0] = 100;
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);

		sprintf(str, SC(191, "Local symbol directory\t%s\\sym"), DesignPath);
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		sprintf(str, SC(192, "Global symbol directory\t%s\\sym"), ProjectPath);
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);


		for (cnt = 0; cnt < NrLibFiles; cnt++)
		{
			if (cnt < NrSchematicSymbolLibraries)
				sprintf(str, SC(193, "Own library\t%s"), LibNames[cnt]);
			else
				sprintf(str, SC(194, "Global library\t%s"), LibNames[cnt]);

			SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
		}

		sprintf(str, "%s\\sym", DesignPath);
		InitSymbolsEntries(Dialog, str);
		CurrentSelection = 0;
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, 0, 0);
		res = 1;
		return about;

	case WM_MOVE:
		GetWindowRect(Dialog, &DialogWindowRect);
		SymbolDialogInitialX = DialogWindowRect.left;
		SymbolDialogInitialY = DialogWindowRect.top;
		res = 1;
		break;

	case WM_SIZE:
		res = 1;
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			return about;

		case IDCANCEL:

//          EndDialog(Dialog, 1);
			if (SymbolDialog != NULL)
				DestroyWindow(SymbolDialog);

			SymbolDialog = NULL;
			SystemBusyMode = 0;
			NrTryingSymbols = 0;
			NetDialogMode = 0;
			RePaint();

// InstCnt
			return about;

//        case ID_ADDSYMBOL_READLIB:
		case IDHELP:
			Help("add_symbol.htm", 0);
			break;

		case IDC_LIST1:
			res2 = HIWORD(WParam);

			if (res2 == 1)
			{
				Found2 = (int32)SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

				if ((Found2 != -1) && (CurrentSelection != Found2))
				{
					CurrentSelection = Found2;

					switch (Found2)
					{
					case 0:
					case 1:
						switch (Found2)
						{
						case 0:
							sprintf(str2, "%s\\sym", DesignPath);
							break;

						case 1:
							sprintf(str2, "%s\\sym", ProjectPath);
							break;
						}

						SendDlgItemMessage(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);
						InitSymbolsEntries(Dialog, str2);
//                  SendDlgItemMessage(Dialog,IDC_LIST2,LB_SETCURSEL,0,0);
						break;

					default:
						if (Found2 - 2 < NrLibFiles)
						{
							Found2 -= 2;
							strcpy(str2, LibNames[Found2]);
						}

						SendDlgItemMessage(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);
						InitLibEntries(Dialog, str2);
//                  SendDlgItemMessage(Dialog,IDC_LIST2,LB_SETCURSEL,0,0);
						break;
					}
				}
			}

			hulp = 1;
			break;

		case IDC_LIST2:
			res2 = HIWORD(WParam);

			if (res2 == 1)
			{
				Found = (int32)SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
				Found2 = (int32)SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);

				if ((Found != -1) && (Found2 != -1))
				{
					SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_GETTEXT, (WPARAM) Found2, (LPARAM) str);
					strcpy(NewSymbolName, str);
					str4[0] = 0;

					switch (Found)
					{
					case 0:
					case 1:
						switch (Found)
						{
						case 0:
							sprintf(str2, "%s\\sym\\%s.sym", DesignPath, str);
							break;

						case 1:
							sprintf(str2, "%s\\sym\\%s.sym", ProjectPath, str);
							break;
						}

						strcpy(NewSymbolDir, str2);
						NewLibName[0] = 0;

						if (GetSymbolInfo(str2, str, 1, str4) == 1)
							SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & str4);

						break;

					default:
						if (Found - 2 < NrLibFiles)
						{
							Found -= 2;
							strcpy(str2, LibNames[Found]);
						}

						NewSymbolDir[0] = 0;
						strcpy(NewLibName, str2);

						if (GetSymbolInfo(str2, str, 0, str4) == 1)
							SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & str4);

						break;
					}

					res = HIWORD(WParam);
					SetFocus(SCHWindow);
					NrTryingSymbols++;
					GetSymbol(NewSymbolName, NewSymbolDir, NewLibName, 0);
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

int32 SelectSymbolDialog()
{
	int32 res, cnt;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

	DialogResult = 0;

	NetDialogMode = 0;

	NrTryingSymbols = 0;
// findfirst

	NrLibFiles = 0;
	memset(&LibNames, 0, sizeof(LibNames));

// Add own symbol libraries first

	for (cnt = 0; cnt < NrSchematicSymbolLibraries; cnt++)
	{
		if (NrLibFiles < 128)
			strcpy(LibNames[NrLibFiles++], SchematicSymbolLibraries[cnt]);
	}

// Add global symbol libraries

	sprintf(str, "%s\\lib\\*.lib", ProjectPath);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while ((res) && (NrLibFiles < 128))
	{
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		sprintf(LibNames[NrLibFiles++], "%s\\lib\\%s", ProjectPath, str2);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	OkToAddSymbol = 0;
	NrTryingSymbols = 0;
	SymbolDialog =
	    CreateDialog(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDSYMBOL), SCHWindow,
	                 (DLGPROC) SelectSymbolDialog2);

	if (SymbolDialog != NULL)
	{
		NetDialogMode = 1;
		ShowWindow(SymbolDialog, SW_SHOW);
	}

// InstCnt SystemBusyMode
	return res;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddComponentTree(HWND Dialog, int32 TreeID)
{
	char FileStr[MAX_LENGTH_STRING], Str1[MAX_LENGTH_STRING], LineBuf[MAX_LENGTH_STRING], FirstChar;
	int32 fp, cnt, cnt2, cnt3, Code1, Num, Length, MenuPos2, MenuCode, res, ok;

	HTREEITEM Parent1, Parent2;

	sprintf(FileStr, "%s\\compmenu.txt", ProjectPath);

	if (FileExistsUTF8(FileStr) != 0)
	{
		if (ProjectPath[0] == 0)
		{
			MessageBoxUTF8(SCHWindow, FileStr, SC(438, "Can not find file compmenu.txt"), MB_APPLMODAL | MB_OK);
			return 0;
		}
		else
		{
			strcpy(FileStr, ProjectPath);
			AddBackSlash(FileStr);
			strcat(FileStr, "compmenu.txt");

			if (FileExistsUTF8(FileStr) != 0)
			{
				MessageBoxUTF8(SCHWindow, FileStr, SC(195, "Can not find file"), MB_APPLMODAL | MB_OK);
				return 0;
			}
		}
	}

	if ((fp = TextFileOpenUTF8(FileStr)) < 0)
	{
//    MessageBoxUTF8(SCHWindow,FileStr,SC(197,"Error in opening file"),MB_APPLMODAL|MB_OK);
		return 0;
	}

#ifdef _DEBUG
	cnt2 = sizeof(CompSelectRecord);
#endif
	AllocateSpecialMem(MEM_COMPSELECT, MaxCompSelect * sizeof(CompSelectRecord), (void *) &CompSelects);

	NrCompSelects = 0;
	Code1 = 0;
	MenuPos2 = -1;
	cnt3 = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (NrCompSelects < MaxCompSelect))
		{
			FirstChar = LineBuf[0];

			switch (FirstChar)
			{
			case '$':
				GetString((LPSTR) & LineBuf[1], Str1);

				if (sscanf(Str1, "%d", &Num) == 1)
				{
					GetQuoteString(LineBuf, Str1);

					if ((Num > 0) && (Num < 10) && (Str1[0] != 0))
					{
						if (Num == 9)
						{
							Code1 = Num;
							CompSelect = &((*CompSelects)[NrCompSelects]);
							CompSelect->Info = 10;
							NrCompSelects++;
						}

						Code1 = Num;
						CompSelect = &((*CompSelects)[NrCompSelects]);
						CompSelect->Info = 1;
						CompSelect->Code1 = Code1;
						CompSelect->Code2 = -1;
						MenuPos2 = -1;
						memmove(CompSelect->Name, &Str1, sizeof(CompSelect->Name) - 1);
						NrCompSelects++;
					}
				}

				break;

			case '#':
				if (Code1 > 0)
				{
					GetString((LPSTR) & LineBuf[1], Str1);

					if (sscanf(Str1, "%d", &Num) == 1)
					{
						GetQuoteString(LineBuf, Str1);

						if ((Num > 0) && (Num < 1000) && (Str1[0] != 0))
						{
							CompSelect = &((*CompSelects)[NrCompSelects]);
							CompSelect->Info = 2;
							CompSelect->Info2 = 0;
							CompSelect->Code1 = Code1;
							CompSelect->Code2 = Num;
							MenuPos2 = NrCompSelects;
							memmove(CompSelect->Name, &Str1, sizeof(CompSelect->Name) - 1);
							NrCompSelects++;
						}
					}
				}

				break;

			case '^':
				if (Code1 > 0)
				{
					GetString((LPSTR) & LineBuf[1], Str1);

					if (sscanf(Str1, "%d", &Num) == 1)
					{
						GetQuoteString(LineBuf, Str1);

						if ((Num > 0) && (Num < 1000) && (Str1[0] != 0))
						{
							CompSelect = &((*CompSelects)[NrCompSelects]);
							CompSelect->Info = 3;
							CompSelect->Code1 = Code1;
							CompSelect->Code2 = Num;
							memmove(CompSelect->Name, &Str1, sizeof(CompSelect->Name) - 1);

							if (MenuPos2 != -1)
							{
								CompSelect = &((*CompSelects)[MenuPos2]);
								CompSelect->Info2++;
							}

							NrCompSelects++;
						}
					}
				}

				break;
			}
		}

//      OutputDebugStr(LineBuf);
//      OutputDebugStr("\n");
	}

	/*
	  if (Length<-1) {
	    MessageBoxUTF8(SCHWindow,FileStr,SC(274,"Error in reading file"),MB_APPLMODAL|MB_OK);
	    return (HMENU)0;
	  }
	*/
	if (TextFileClose(fp) != 0)
	{
		MessageBoxUTF8(SCHWindow, FileStr, SC(196, "Error in closing file"), MB_APPLMODAL | MB_OK);
		return 0;
	}

	Parent1 = NULL;
	Parent2 = NULL;
	cnt2 = -1;
	res = 0;

	for (cnt = 0; cnt < NrCompSelects; cnt++)
	{
		CompSelect = &((*CompSelects)[cnt]);

		switch (CompSelect->Info)
		{
		case 1:
			cnt3 = -1;
			cnt2++;

			if (cnt2 < 20)
			{
				CompSelect->TreeItemInfo.item.mask = TVIF_TEXT | TVIF_PARAM;
				CompSelect->TreeItemInfo.item.pszText = CompSelect->Name;
				CompSelect->TreeItemInfo.item.cchTextMax = 50;
				CompSelect->TreeItemInfo.item.cChildren = 1;
				CompSelect->TreeItemInfo.item.lParam = 0;
				CompSelect->TreeItemInfo.hParent = NULL;
				CompSelect->TreeItemInfo.hInsertAfter = TVI_ROOT;
				CompSelect->TreeItem =
				    (HTREEITEM)SendDlgItemMessage(Dialog, TreeID, TVM_INSERTITEM, 0,
				                                   (LPARAM) & CompSelect->TreeItemInfo);
				Parent1 = CompSelect->TreeItem;
//          SendDlgItemMessageUTF8(Dialog,TreeID,TVM_SETINSERTMARK,0,(LPARAM)CompSelect->TreeItem);
				ok = 1;
			}

			break;

		case 2:
			if (CompSelect->Info2 > 0)
			{
				if (cnt3 < 30)
				{
					cnt3++;
					CompSelect->TreeItemInfo.item.mask = TVIF_TEXT | TVIF_PARAM;
					CompSelect->TreeItemInfo.item.pszText = CompSelect->Name;
					CompSelect->TreeItemInfo.item.cchTextMax = 50;
					CompSelect->TreeItemInfo.item.cChildren = 1;
					CompSelect->TreeItemInfo.item.lParam = 0;
					CompSelect->TreeItemInfo.hParent = Parent1;
					CompSelect->TreeItemInfo.hInsertAfter = TVI_LAST;
					CompSelect->TreeItem =
					    (HTREEITEM)SendDlgItemMessage(Dialog, TreeID, TVM_INSERTITEM, 0,
					                                   (LPARAM) & CompSelect->TreeItemInfo);
					Parent2 = CompSelect->TreeItem;
//            SendDlgItemMessageUTF8(Dialog,TreeID,TVM_SETINSERTMARK,0,(LPARAM)CompSelect->TreeItem);
				}
			}
			else
			{
				MenuCode = CompSelect->Code1 * 1000 + CompSelect->Code2;
				CompSelect->TreeItemInfo.item.mask = TVIF_TEXT | TVIF_PARAM;
				CompSelect->TreeItemInfo.item.pszText = CompSelect->Name;
				CompSelect->TreeItemInfo.item.cchTextMax = 50;
				CompSelect->TreeItemInfo.item.cChildren = 0;
				CompSelect->TreeItemInfo.item.lParam = MenuCode;
				CompSelect->TreeItemInfo.hParent = Parent1;
				CompSelect->TreeItemInfo.hInsertAfter = TVI_LAST;
				CompSelect->TreeItem =
				    (HTREEITEM)SendDlgItemMessage(Dialog, TreeID, TVM_INSERTITEM, 0,
				                                   (LPARAM) & CompSelect->TreeItemInfo);
//          SendDlgItemMessageUTF8(Dialog,TreeID,TVM_SETINSERTMARK,0,(LPARAM)CompSelect->TreeItem);
			}

			break;

		case 3:
			MenuCode = CompSelect->Code1 * 1000 + CompSelect->Code2;
			CompSelect->TreeItemInfo.item.mask = TVIF_TEXT | TVIF_PARAM;
			CompSelect->TreeItemInfo.item.pszText = CompSelect->Name;
			CompSelect->TreeItemInfo.item.cchTextMax = 50;
			CompSelect->TreeItemInfo.item.cChildren = 0;
			CompSelect->TreeItemInfo.item.lParam = MenuCode;
			CompSelect->TreeItemInfo.hParent = Parent2;
			CompSelect->TreeItemInfo.hInsertAfter = TVI_LAST;
			CompSelect->TreeItem =
			    (HTREEITEM)SendDlgItemMessage(Dialog, TreeID, TVM_INSERTITEM, 0, (LPARAM) & CompSelect->TreeItemInfo);
//        SendDlgItemMessageUTF8(Dialog,TreeID,TVM_SETINSERTMARK,0,(LPARAM)CompSelect->TreeItem);
			break;

		case 10:
			ok = 1;
			break;
		}
	}

//  GlobalUnlock(CompSelectGlobal);
//  GlobalFree(CompSelectGlobal);

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 FillComponentListbox(HWND Dialog, int32 ListBoxID, LPSTR SearchString, int32 Index, int32 mode)
{
	char Name[MAX_LENGTH_STRING], Code1Str[MAX_LENGTH_STRING], Code2Str[MAX_LENGTH_STRING], PartNr[MAX_LENGTH_STRING],
	     Value[MAX_LENGTH_STRING], Geometry[MAX_LENGTH_STRING], CompInfo[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING],
	     LineBuf[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING], TestString[MAX_LENGTH_STRING];
	int32 fp, Length, Code1, Code2, Comp1, Comp2, FoundComps1;

	SendDlgItemMessage(Dialog, ListBoxID, LB_RESETCONTENT, 0, 0);

	if ((SearchString) && (SearchString[0] != 0))
		struprUTF8(SearchString);

	Comp1 = Index / 1000;
	Comp2 = Index % 1000;

	memset(&PartNr, 0, sizeof(PartNr));
	memset(&Value, 0, sizeof(Value));
	memset(&Geometry, 0, sizeof(Geometry));
	memset(&CompInfo, 0, sizeof(CompInfo));

	FoundComps1 = 0;
	sprintf(FileStr, "%s\\comp.txt", ProjectPath);

	if ((fp = TextFileOpenUTF8(FileStr)) < 0)
	{
		MessageBoxUTF8(SCHWindow, FileStr, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 1)) >= 0)
	{
		LineBuf[Length] = 0;

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/'))
		{
			GetString(LineBuf, Name);
			GetString(LineBuf, Code1Str);
			GetString(LineBuf, Code2Str);
			Code1 = 0;
			Code2 = 0;

			if ((sscanf(Code1Str, "%d", &Code1) == 1) && (sscanf(Code2Str, "%d", &Code2) == 1) && (Code1 >= 1)
			        && (Code1 <= 9) && (Code2 >= 1) && (Code2 < 1000))
			{
				switch (mode)
				{
				case 0:		// Search for a code1, code2 type components
					if ((Code1 == Comp1) && (Code2 == Comp2))
					{
						GetQuoteString(LineBuf, PartNr);
						GetQuoteString(LineBuf, Value);
						GetString(LineBuf, Geometry);
						GetQuoteString(LineBuf, CompInfo);
						sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s\t%d,%d", Name, PartNr, Value, Geometry, CompInfo,
						        Code1, Code2);
						SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
						FoundComps1++;
					}

					break;

				case 1:		// Search for a value string
					GetQuoteString(LineBuf, PartNr);
					GetQuoteString(LineBuf, Value);
					GetString(LineBuf, Geometry);
					GetQuoteString(LineBuf, CompInfo);

					if (Value[0] != 0)
					{
						strcpy(TestString, Value);
						struprUTF8(TestString);

						if (strstr(TestString, SearchString))
						{
							sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s\t%d,%d", Name, PartNr, Value, Geometry,
							        CompInfo, Code1, Code2);
							SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
							FoundComps1++;
						}
					}

					break;

				case 2:		// Search for a partnr string
					GetQuoteString(LineBuf, PartNr);
					GetQuoteString(LineBuf, Value);
					GetString(LineBuf, Geometry);
					GetQuoteString(LineBuf, CompInfo);

					if (PartNr[0] != 0)
					{
						strcpy(TestString, PartNr);
						struprUTF8(TestString);

						if (strstr(TestString, SearchString))
						{
							sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s\t%d,%d", Name, PartNr, Value, Geometry,
							        CompInfo, Code1, Code2);
							SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
							FoundComps1++;
						}
					}

					break;

				case 3:		// Search for a description string
					GetQuoteString(LineBuf, PartNr);
					GetQuoteString(LineBuf, Value);
					GetString(LineBuf, Geometry);
					GetQuoteString(LineBuf, CompInfo);

					if (CompInfo[0] != 0)
					{
						strcpy(TestString, CompInfo);
						struprUTF8(TestString);

						if (strstr(TestString, SearchString))
						{
							sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s\t%d,%d", Name, PartNr, Value, Geometry,
							        CompInfo, Code1, Code2);
							SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
							FoundComps1++;
						}
					}

					break;

				case 4:		// Search for a geometry
					GetQuoteString(LineBuf, PartNr);
					GetQuoteString(LineBuf, Value);
					GetString(LineBuf, Geometry);
					GetQuoteString(LineBuf, CompInfo);

					if (Geometry[0] != 0)
					{
						strcpy(TestString, Geometry);
						struprUTF8(TestString);

						if (strstr(TestString, SearchString))
						{
							sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s\t%d,%d", Name, PartNr, Value, Geometry,
							        CompInfo, Code1, Code2);
							SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
							FoundComps1++;
						}
					}

					break;

				case 5:		// Search for a symbol
					GetQuoteString(LineBuf, PartNr);
					GetQuoteString(LineBuf, Value);
					GetString(LineBuf, Geometry);
					GetQuoteString(LineBuf, CompInfo);

					if (Name[0] != 0)
					{
						strcpy(TestString, Name);
						struprUTF8(TestString);

						if (strstr(TestString, SearchString))
						{
							sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s\t%d,%d", Name, PartNr, Value, Geometry,
							        CompInfo, Code1, Code2);
							SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
							FoundComps1++;
						}
					}

					break;
				}
			}
		}
	}

	if (TextFileClose(fp) != 0)
	{
		MessageBoxUTF8(SCHWindow, FileStr, SC(196, "Error in closing file"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	return 0;
}

//******************************************************************************************************************
//******************************** IDD_DIALOG_ADDCOMP **************************************************************
//******************************************************************************************************************

int32 CALLBACK SelectComponentDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, res, res2, ok, hulp, TabStops[5], code1, code2, pos, Selected, Length, dialogunitX, lengte;
	char Name[MAX_LENGTH_STRING], PartNr[MAX_LENGTH_STRING], CodeStr[MAX_LENGTH_STRING], Value[MAX_LENGTH_STRING],
	     Geometry[MAX_LENGTH_STRING], CompInfo[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING],
	     LineBuf[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], DialogTextLine[MAX_LENGTH_STRING],
	     FileName[MAX_LENGTH_STRING];
	NMTREEVIEW *NotifyInfo;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SystemBusyMode = 40;
		SetWindowTextUTF8(Dialog, SC(243, "Add database component"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(198, "Component groups"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(451, "Component"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(100, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(101, "Part number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(120, "Description"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(201, "Component search"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(102, "Geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(432, "Search"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(432, "Search"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(432, "Search"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON4, SC(432, "Search"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON5, SC(432, "Search"));

		if (ComponentDialogInitialX != -10000)
		{
			GetWindowRect(Dialog, &DialogWindowRect);
			MoveWindow(Dialog, ComponentDialogInitialX, ComponentDialogInitialY,
			           DialogWindowRect.right - DialogWindowRect.left, DialogWindowRect.bottom - DialogWindowRect.top,
			           1);
		}

		AddComponentTree(Dialog, IDC_TREE1);
		dialogunitX = LOWORD(GetDialogBaseUnits()) * 20;
		TabStops[0] = MulDiv(190, 107, dialogunitX);
		TabStops[1] = MulDiv(420, 107, dialogunitX);
		TabStops[2] = MulDiv(540, 107, dialogunitX);
		TabStops[3] = MulDiv(670, 107, dialogunitX);
		TabStops[4] = MulDiv(2000, 107, dialogunitX);
//      RefName,PartNr,Value,Geometry,CompInfo,
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 5, (LPARAM) (LPINT) & TabStops);
		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETTABSTOPS, 5, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETHORIZONTALEXTENT, 1200, 0);
		str[0] = 0;
		strcat(str, SC(99, "Symbol name"));
		strcat(str, "\t");
		strcat(str, SC(101, "Part number"));
		strcat(str, "\t");
		strcat(str, SC(100, "Value"));
		strcat(str, "\t");
		strcat(str, SC(102, "Geometry"));
		strcat(str, "\t");
		strcat(str, SC(103, "Part description"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) str);
		return about;

	case WM_MOVE:
		GetWindowRect(Dialog, &DialogWindowRect);
		ComponentDialogInitialX = DialogWindowRect.left;
		ComponentDialogInitialY = DialogWindowRect.top;
		res = 1;
		break;

	case WM_SIZE:
		res = 1;
		break;

	case WM_NOTIFY:
		ok = 1;

		switch (LOWORD(WParam))
		{
		case IDC_TREE1:
			NotifyInfo = (NMTREEVIEW *) LParam;

			if (NotifyInfo->hdr.code == TVN_SELCHANGED)
			{
				if (!RemoveTreeSelection)
				{
					Selected = NotifyInfo->itemNew.lParam;

					if (Selected > 0)
						FillComponentListbox(Dialog, IDC_LIST1, NULL, Selected, 0);
				}
				else
					RemoveTreeSelection = 0;

				ok = 1;
			}

			break;
		}

		res = 1;
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_BUTTON1:		// Value
			memset(&DialogTextLine, 0, sizeof(DialogTextLine));

			if (SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, 100, (LPARAM) & DialogTextLine) > 0)
				FillComponentListbox(Dialog, IDC_LIST1, DialogTextLine, 0, 1);

			return about;

		case IDC_BUTTON2:		// Part nr
			memset(&DialogTextLine, 0, sizeof(DialogTextLine));

			if (SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, 100, (LPARAM) & DialogTextLine) > 0)
				FillComponentListbox(Dialog, IDC_LIST1, DialogTextLine, 0, 2);

			return about;

		case IDC_BUTTON3:		// Description
			memset(&DialogTextLine, 0, sizeof(DialogTextLine));

			if (SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, 100, (LPARAM) & DialogTextLine) > 0)
				FillComponentListbox(Dialog, IDC_LIST1, DialogTextLine, 0, 3);

			return about;

		case IDC_BUTTON4:		// Geometry
			memset(&DialogTextLine, 0, sizeof(DialogTextLine));

			if (SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, 100, (LPARAM) & DialogTextLine) > 0)
				FillComponentListbox(Dialog, IDC_LIST1, DialogTextLine, 0, 4);

			return about;

		case IDC_BUTTON5:		// Symbol
			memset(&DialogTextLine, 0, sizeof(DialogTextLine));

			if (SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_GETTEXT, 100, (LPARAM) & DialogTextLine) > 0)
				FillComponentListbox(Dialog, IDC_LIST1, DialogTextLine, 0, 5);

			return about;

		case IDCANCEL:

//          EndDialog(Dialog, 1);
			if (ComponentDialog != NULL)
				DestroyWindow(ComponentDialog);

			ComponentDialog = NULL;
			SystemBusyMode = 0;
			NrTryingSymbols = 0;
			NetDialogMode = 0;
			RePaint();
// InstCnt
			return about;

//        case ID_ADDSYMBOL_READLIB:
		case IDHELP:
			Help("add_symbol.htm", 0);
			break;

		case IDC_TREE1:
			ok = 1;
			break;

		case IDC_LIST1:
			res2 = HIWORD(WParam);

			if (res2 == 1)
			{
				Selected = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
				DialogTextLine[0] = 0;

				if (Selected >= 0)
				{
					SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETTEXT, Selected, (LPARAM) DialogTextLine);
					strcpy(LineBuf, DialogTextLine);

					if (DialogTextLine[0] != 0)
					{
						Length = strlen(DialogTextLine);
						cnt = 0;
						GetStringTab(DialogTextLine, Name);
						GetStringTab(DialogTextLine, PartNr);
						GetStringTab(DialogTextLine, Value);
						GetStringTab(DialogTextLine, Geometry);
						GetStringTab(DialogTextLine, CompInfo);
						GetStringTab(DialogTextLine, CodeStr);

						if (sscanf(CodeStr, "%d,%d", &code1, &code2) == 2)
						{
							for (cnt = 0; cnt < NrCompSelects; cnt++)
							{
								CompSelect = &((*CompSelects)[cnt]);

								if ((CompSelect->Code1 == code1) && (CompSelect->Code2 == code2))
								{
									ok = 1;
									RemoveTreeSelection = 1;
									SendDlgItemMessage(Dialog, IDC_TREE1, TVM_SELECTITEM, TVGN_FIRSTVISIBLE,
									                   (LPARAM) CompSelect->TreeItem);
//                      SendDlgItemMessageUTF8(Dialog,IDC_TREE1,TVM_SELECTITEM,TVGN_CARET,
//                                         (LPARAM)CompSelect->TreeItem);
								}
							}
						}

						memset(&NewInstance, 0, sizeof(InstanceRecord));
						memmove(&NewInstance.Value, Value, sizeof(NewInstance.Value) - 1);
						memmove(&NewInstance.PartNr, PartNr, sizeof(NewInstance.PartNr) - 1);
						memmove(&NewInstance.Geometry, Geometry, sizeof(NewInstance.Geometry) - 1);
						memmove(&NewInstance.PartDescription, CompInfo, sizeof(NewInstance.PartDescription) - 1);

						if (SearchSymbol(Name, FileName, &pos, &lengte, 0) == 1)
						{
							OkToAddSymbol = 1;

							if ((pos == -1) || (pos == -2))
							{
								//                  GetSymbol(Name,FileName,"",1);
								strcpy(NewSymbolName, Name);
								strcpy(NewSymbolDir, FileName);
								strcpy(NewLibName, "");
							}
							else
							{
								strcpy(NewSymbolName, Name);
								strcpy(NewSymbolDir, "");
								strcpy(NewLibName, FileName);
								//                  GetSymbol(Name,"",FileName,1);
							}

							SetFocus(SCHWindow);
							NrTryingSymbols++;
							GetSymbol(NewSymbolName, NewSymbolDir, NewLibName, 1);
						}
						else
						{
							sprintf(FileStr, SC(203, "Symbol %s not found"), Name);
							MessageBoxUTF8(SCHWindow, FileStr, SC(38, "Error"), MB_APPLMODAL | MB_OK);
							break;
						}
					}
				}
			}

			hulp = 1;
			break;
		}

		break;

	case WM_PARENTNOTIFY:
		hulp = 1;
		break;

	default:
		ok = 1;
	}

	about = 0;
	return about;
}

int32 SelectComponentDialog(int32 mode)
{
	int32 res;


	res = 0;
	OkToAddSymbol = 0;
	NrTryingSymbols = 0;
	ComponentDialog =
	    CreateDialog(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADDCOMP), SCHWindow,
	                 (DLGPROC) SelectComponentDialog2);

	if (ComponentDialog != NULL)
	{
		NetDialogMode = 1;
		ShowWindow(ComponentDialog, SW_SHOW);
	}

	return res;
}

//***************************************************************************************************************************
//******************************** IDD_DIALOG_VIEW_SHEET * IDD_DIALOG_VIEW_SYMBOL *******************************************
//***************************************************************************************************************************

int32 CALLBACK EditSymbolSheetInfoDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res;
	char DialogTextLine[MAX_LENGTH_STRING];
	double value = 0.0;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(403, "View options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(229, "Grid"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(367, "Selection mode"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(392, "Repeat"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(481, "Mouse cursor Info display"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(482, "Sec"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(482, "Sec"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(483, "Activate after"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(484, "Visible for"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK4, SC(460, "Ruler"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(459, "On"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK6, SC(459, "On"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK12, SC(454, "Activate"));

		if (DialogMode == 0)
		{
			SetDialogItemTextUTF8(Dialog, IDC_CHECK7, SC(303, "Append properties to netlabels"));
			SetDialogItemTextUTF8(Dialog, IDC_CHECK8, SC(304, "Append properties to references"));
		}

		SendDlgItemMessageUTF8(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) SC(390, "Replacement"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) SC(391, "Appending"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "0.1");
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "0.2");
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "1.0");

		if (InRange(GridSize, 0.1))
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, 0, 0);

		if (InRange(GridSize, 0.2))
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, 1, 0);

		if (InRange(GridSize, 1.0))
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, 2, 0);

		if (ReplaceSelections)
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 0, 0);
		else
			SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETCURSEL, 1, 0);

		SendDlgItemMessage(Dialog, IDC_CHECK4, BM_SETCHECK, CrossHairVisible, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK5, BM_SETCHECK, GridVisible, 0);
		SendDlgItemMessage(Dialog, IDC_CHECK6, BM_SETCHECK, RepeatMode, 0);

		if (DialogMode == 0)
		{
			SendDlgItemMessage(Dialog, IDC_CHECK7, BM_SETCHECK, AppendPropertiesToNetlabel, 0);
			SendDlgItemMessage(Dialog, IDC_CHECK8, BM_SETCHECK, AppendPropertiesToReferences, 0);
		}

		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(461, "Wire/line"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(393, "Normal"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(394, "Extended"));

		if (WireSelectMode)
			SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 1, 0);
		else
			SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, 0, 0);

		if (DialogMode == 0)
		{
			SetWindowTextUTF8(Dialog, SC(453, "Sheet options"));
			SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(454, "Activate"));
			SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(462, "Disable one pinnet check"));
            SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(419, "Show netlabels"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(112, "Start number"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(456, "Sheet numbering"));

			if (Design.AnnotateStartNumber >= 1000000)
			{
				sprintf(DialogTextLine, "%d", Design.AnnotateStartNumber % 1000000);
				SendDlgItemMessage(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);
			}

			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, DisableOnePinNetCheck, 0);

			if ((Design.SheetInfo & 1) == 0)
				SendDlgItemMessage(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);
		}
		else
		{
			SetWindowTextUTF8(Dialog, SC(464, "Symbol options"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(224, "Line"));
		}

		SendDlgItemMessage(Dialog, IDC_CHECK12, BM_SETCHECK, PopupDisplayVisible, 0);
		SetDialogFloatValue(Dialog, IDC_EDIT9, (double) ButtonInfoTimeoutStart / 10, 1);
		SetDialogFloatValue(Dialog, IDC_EDIT10, (double) ButtonInfoTimeout / 10, 1);
		return about;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_CHECK1:
			break;

		case IDC_CHECK2:
			DisableOnePinNetCheck ^= 1;
			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, DisableOnePinNetCheck, 0);
			break;

		case IDC_CHECK3:
			break;

		case IDC_CHECK4:
			CrossHairVisible ^= 1;
			break;

		case IDC_CHECK5:
			GridVisible ^= 1;
			RePaint();
			break;

		case IDC_CHECK6:
			RepeatMode ^= 1;
			break;

		case IDC_CHECK7:
			AppendPropertiesToNetlabel ^= 1;
			break;

		case IDC_CHECK8:
			AppendPropertiesToReferences ^= 1;
			break;

		case IDC_LIST1:
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			switch (res)
			{
			case 0:
				DrawGridSize = GridSize = 0.1;
				break;

			case 1:
				DrawGridSize = GridSize = 0.2;
				break;

			case 2:
				DrawGridSize = GridSize = 1.0;
				break;
			}

			if (GridVisible)
				RePaint();

			break;

		case IDC_LIST2:
			res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);

			if (res == 0)
				WireSelectMode = 0;
			else
				WireSelectMode = 1;

			break;

		case IDC_LIST3:
			res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETCURSEL, 0, 0);

			if (res == 0)
				ReplaceSelections = 1;
			else
				ReplaceSelections = 0;

			break;

		case IDOK:
			memset(&DialogTextLine, 0, sizeof(DialogTextLine));

			if ((SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, 100, (LPARAM) & DialogTextLine) > 0)
			        && (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
			        && (sscanf(DialogTextLine, "%d", &res) == 1))
			{
				if (Design.AnnotateStartNumber != res + 1000000)
				{
					Design.AnnotateStartNumber = res + 1000000;
					FileChanged = 1;
				}
			}

			PopupDisplayVisible = SendDlgItemMessage(Dialog, IDC_CHECK12, BM_GETCHECK, 0, 0);
			GetDialogFloatValue(Dialog, IDC_EDIT9, &value);
			ButtonInfoTimeoutStart = (int32) (value * 10);
			GetDialogFloatValue(Dialog, IDC_EDIT10, &value);
			ButtonInfoTimeout = (int32) (value * 10);

			if (SendDlgItemMessage(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0) == 0)
			{
				if ((Design.SheetInfo & 1) == 0)
				{
					Design.SheetInfo |= 1;
					DataBaseChanged = 1;
				}
			}
			else
			{
				if ((Design.SheetInfo & 1) == 1)
				{
					Design.SheetInfo &= ~1;
					DataBaseChanged = 1;
				}
			}

			EndDialog(Dialog, 2);
			break;

		case IDCANCEL:
			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("view_options.htm", 0);
			break;
		}

		break;

	default:
		ok = 1;
	}

	about = 0;
	return about;
}

int32 EditSymbolSheetInfo(int32 mode)
{
	DialogMode = mode;

	if (mode == 0)
	{
		DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_VIEW_SHEET), SCHWindow,
		          (DLGPROC) EditSymbolSheetInfoDialog2);
	}
	else
	{
		DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_VIEW_SYMBOL), SCHWindow,
		          (DLGPROC) EditSymbolSheetInfoDialog2);
	}

	RePaint();
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 SearchGeometries(HWND Dialog, int32 ListBoxID, LPSTR SearchStr)
{
	LibRecord Lib;
	LibNameRecord LibName;
	int32 cnt, cnt2, NrLibEntries, Libfp, result, res, lengte;
	char str[MAX_LENGTH_STRING], SearchStr2[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING],
	     str4[MAX_LENGTH_STRING], SearchFileName[MAX_LENGTH_STRING];
	WCHAR str5[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING], SearchStrW[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

	NrGeometryLibFiles = 0;
	memset(&GeometryLibNames, 0, sizeof(GeometryLibNames));

// Search in own libraries first
	for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
	{
		if (NrGeometryLibFiles < 64)
			strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt]);
	}

// Search in default libraries

	sprintf(SearchFileName, "%s\\shplib\\*.slb", ProjectPath);
	FileSearchHandle = FindFirstFileUTF8(SearchFileName, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while ((res) && (NrGeometryLibFiles < 64))
	{
		UnicodeToUtf8(FileInfo.cFileName, str4, MAX_LENGTH_STRING - 100);
		sprintf(GeometryLibNames[NrGeometryLibFiles++], "%s\\shplib\\%s", ProjectPath, str4);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	Utf8ToUnicode(SearchStr, SearchStrW, MAX_LENGTH_STRING - 100);
	wcsupr(SearchStrW);
//  strcpy(SearchStr2,SearchStr);
//  strupr(SearchStr2);

	SendDlgItemMessage(Dialog, ListBoxID, LB_RESETCONTENT, 0, 0);
	sprintf(str, "%s\\pcb\\shapes\\*.shp", DesignPath);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		wcscpy(str5, FileInfo.cFileName);
		lengte = wcslen(str5);
		str5[lengte - 4] = 0;
		wcscpy(str6, str5);
		wcsupr(str6);

		if (wcsstr(str6, SearchStrW))
		{
			UnicodeToUtf8(FileInfo.cFileName, str4, MAX_LENGTH_STRING - 100);
			sprintf(str2, "%s\t%s\\pcb\\shapes\\%s.shp", str3, DesignPath, str4);
			SendDlgItemMessageUTF8(Dialog, ListBoxID, LB_ADDSTRING, 0, (LPARAM) str2);
		}

//    SendDlgItemMessageW(Dialog,IDC_LIST2,LB_ADDSTRING,0,(LPARAM)strW);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	sprintf(str, "%s\\shapes\\*.shp", ProjectPath);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		wcscpy(str5, FileInfo.cFileName);
		lengte = wcslen(str5);
		str5[lengte - 4] = 0;
		wcscpy(str6, str5);
		wcsupr(str6);

		if (wcsstr(str6, SearchStrW))
		{
			UnicodeToUtf8(FileInfo.cFileName, str4, MAX_LENGTH_STRING - 100);
			sprintf(str2, "%s\t%s\\shapes\\%s.shp", str3, ProjectPath, str4);
			SendDlgItemMessageUTF8(Dialog, ListBoxID, LB_ADDSTRING, 0, (LPARAM) str2);
		}

//    SendDlgItemMessageW(Dialog,IDC_LIST2,LB_ADDSTRING,0,(LPARAM)strW);
		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	for (cnt = 0; cnt < NrGeometryLibFiles; cnt++)
	{
		if ((Libfp = FileOpenReadOnlyUTF8(GeometryLibNames[cnt])) == -1)
			return 0;

		if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
			return 0;

		if (strcmp(Lib.Identification, GeometryLibraryCode1) == 0)
		{
			NrLibEntries = Lib.NrLibEntries;

			for (cnt2 = 0; cnt2 < (int32) min(NrLibEntries, 5000); cnt2++)
			{
				if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
				{
					FileClose(Libfp);
					return 0;
				}

				strcpy(str, LibName.Text);
				struprUTF8(str);
#ifdef _DEBUG

				if (stricmp(str, "SO20") == 0)
					ok = 1;

#endif

				if (strstr(str, SearchStr2))
				{
					sprintf(str2, "%s\t%s", LibName.Text, GeometryLibNames[cnt]);
					SendDlgItemMessageUTF8(Dialog, ListBoxID, LB_ADDSTRING, 0, (LPARAM) str2);
				}
			}
		}

		FileClose(Libfp);
	}

	return 0;
}

//*************************************************************************************************************************
//*************************** IDD_DIALOG_GEOM_SEARCH **********************************************************************
//*************************************************************************************************************************

int32 CALLBACK SearchGeomDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res, hulp, ok, LibMode, Found, TabStops[5];
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING * 5], InitStr[MAX_LENGTH_STRING],
	     *strp, ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(199, "Search geometry"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(137, "Enter geometry name (abbreviation)"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(106, "Select geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(432, "Search"));

		TabStops[0] = 70;

		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);

		if (GeometryDialogInitialX != -10000)
		{
			GetWindowRect(Dialog, &DialogWindowRect);
			MoveWindow(Dialog, GeometryDialogInitialX, GeometryDialogInitialY,
			           DialogWindowRect.right - DialogWindowRect.left, DialogWindowRect.bottom - DialogWindowRect.top, 1);
		}

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
			Found = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (ClosingWindowMessage != 0)
				SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

			if (Found != -1)
			{
				SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETTEXT, (WPARAM) Found, (LPARAM) GeometrieName);
				strp = strchr(GeometrieName, '\t');

				if (strp)
					*strp = 0;
			}

			EndDialog(Dialog, 1);

		case IDC_BUTTON1:
			if (SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, 80, (LPARAM) str) > 0)
				SearchGeometries(Dialog, IDC_LIST1, str);
			break;

		case IDCANCEL:
			if (ClosingWindowMessage != 0)
				SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);

			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("comp_geometry.htm", 0);
			break;

		case IDC_LIST1:
			if (HIWORD(WParam) == LBN_SELCHANGE)
			{
				Found = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
				LibMode = 0;

				if (Found != -1)
				{
					SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETTEXT, (WPARAM) Found, (LPARAM) str);
					strcpy(str2, strchr(str, '\t') + 1);
					strp = strchr(str, '\t');

					if (strp)
						*strp = 0;

					res = strlen(str2);

					if (stricmp(&str2[res - 3], "slb") == 0)
						LibMode = 1;

					InitStr[0] = 0;

					if (ProjectActive)
						strcpy(InitStr, "/o");

					if (FileExistsUTF8(GeometrieEditor) != 0)
					{
						sprintf(str3, SC(143, "Can not find geometry editor %s"), GeometrieEditor);
						MessageBoxUTF8(SCHWindow, str3, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}

					if (LibMode == 0)
					{
						if (FileExistsUTF8(str) == 0)
						{
							if (ClosingWindowMessage != 0)
							{
								SendMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);
								sprintf(ExeParams, "\"%s\" \"%s\" /S %i,%i,%i,%i %s /a /w %x /u \"%s\"",
								        GeometrieEditor, str2, GeomScreenWidth, GeomScreenHeight, GeomStartX,
								        GeomStartY, InitStr, (int32) SCHWindow, ProjectPath);
								StartupInfo.cb = sizeof(StartupInfo);
								StartupInfo.wShowWindow = SW_SHOW;
								CreateProcess(GeometrieEditor, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo,
								              &ProcessInfo);
							}
						}
						else
						{
							sprintf(str3, SC(142, "Can not find geometry %s"), str);
							MessageBoxUTF8(SCHWindow, str3, SC(38, "Error"), MB_APPLMODAL | MB_OK);
							return about;
						}
					}
					else
					{
						sprintf(ExeParams, "\"%s\" \"%s\" /l \"%s\" /S %i,%i,%i,%i %s /a /w %x /u \"%s\"",
						        GeometrieEditor, str, str2, GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY,
						        InitStr, (int32) SCHWindow, ProjectPath);
						StartupInfo.cb = sizeof(StartupInfo);
						StartupInfo.wShowWindow = SW_SHOW;

						if (ClosingWindowMessage != 0)
						{
							SendNotifyMessage(HWND_BROADCAST, ClosingWindowMessage, 0, 1);
							CreateProcess(GeometrieEditor, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo,
							              &ProcessInfo);
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

int32 SearchGeometrie()
{
	int32 res, ok;

	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_GEOM_SEARCH), SCHWindow, (DLGPROC) SearchGeomDialog2);

	if (ActiveViewGeometry)
	{
		ActiveViewGeometry = 0;
		ViewFull(1);
	}

	ok = 1;
	return res;
}

//***************************************************************************************************************************
//******************************* IDD_DIALOG_COMP_SEL ***********************************************************************
//***************************************************************************************************************************

int32 GetCompString(InstanceRecord * Instance, LPSTR str)
{
	switch (CompSortMode)
	{
	case 0:
		sprintf(str, "%s\t\t%s\t%s\t%s\t%s", Instance->Reference, Instance->SymbolName, Instance->Value,
		        Instance->Geometry, Instance->PartNr);
		break;

	case 1:
		sprintf(str, "%s\t%s\t\t%s\t%s\t%s", Instance->SymbolName, Instance->Reference, Instance->Value,
		        Instance->Geometry, Instance->PartNr);
		break;

	case 2:
		sprintf(str, "%s\t%s\t\t%s\t%s\t%s", Instance->Geometry, Instance->Reference, Instance->SymbolName,
		        Instance->Value, Instance->PartNr);
		break;

	case 3:
		sprintf(str, "%s\t%s\t\t%s\t%s\t%s", Instance->Value, Instance->Reference, Instance->SymbolName,
		        Instance->Geometry, Instance->PartNr);
		break;

	case 4:
		sprintf(str, "%s\t%s\t\t%s\t%s\t\t\t%s", Instance->PartNr, Instance->Reference, Instance->SymbolName, Instance->Value,
		        Instance->Geometry);
		break;
	}

	return 0;
}

//***************************************************************************************************************************
//******************************* IDD_DIALOG_COMP_SEL ***********************************************************************
//***************************************************************************************************************************

int32 ComponentSelectionFunction(HWND Dialog, int32 mode, int32 IndexNr)
{
	InstanceRecord *Instance, *FirstInstance;
	int32 cnt, res, res2, cnt2, TabStops[5];
	char str[MAX_LENGTH_STRING], PartNr[MAX_LENGTH_STRING], Name[MAX_LENGTH_STRING], ShapeName[MAX_LENGTH_STRING],
	     Value[MAX_LENGTH_STRING], SymbolName[MAX_LENGTH_STRING];

	switch (mode)
	{
	case 0:
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
		SendDlgItemMessage(Dialog, IDC_LIST3, LB_RESETCONTENT, 0, 0);

		switch (CompSortMode)
		{
		case 0:
			strcpy(str, SC(503, "Ref\tSymbol name\tValue\tGeometry\tPartnr"));
			TabStops[0] = 30;
			TabStops[1] = 100;
			TabStops[2] = 240;
			break;

		case 1:
			strcpy(str, SC(504, "Symbol name\tRef\tValue\tGeometry\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;

		case 2:
			strcpy(str, SC(505, "Geometry\tRef\tSymbol name\tValue\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;

		case 3:
			strcpy(str, SC(506, "Value\tRef\tSymbol name\tGeometry\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;

		case 4:
			strcpy(str, SC(507, "Partnr\tRef\tSymbol name\tValue\tGeometry"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;
		}

		TabStops[3] = 380;
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) str);
		NrUsedComps = 0;

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0) && (Instance->Reference[0] != 0)
			        && (strcmp(Instance->Reference, "?") != 0))
			{
				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					Instance->Info |= OBJECT_DONE;
				else
					Instance->Info &= ~OBJECT_DONE;
			}

			NrUsedComps++;
		}

		if (NrUsedComps > 100)
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_INITSTORAGE, NrUsedComps, NrUsedComps * 240);

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0) && (Instance->Reference[0] != 0)
			        && (strcmp(Instance->Reference, "?") != 0))
			{
				GetCompString(Instance, str);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
			}
		}

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE | OBJECT_PROTECTED)) == OBJECT_DONE)
			        && (Instance->Reference[0] != 0) && (strcmp(Instance->Reference, "?") != 0))
			{
				GetCompString(Instance, str);
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
		for (cnt2 = 0; cnt2 < Design.NrInstances; cnt2++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt2]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0) && (Instance->Reference[0] != 0)
			        && (strcmp(Instance->Reference, "?") != 0))
				Instance->Info &= ~(OBJECT_SELECTED | 7);
		}

		FirstInstance = NULL;

		for (cnt = 0; cnt < NrUsedComps; cnt++)
		{
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0);

			if (res > 0)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str);
				res2 = 0;
				Name[0] = 0;
				ShapeName[0] = 0;
				SymbolName[0] = 0;
				PartNr[0] = 0;
				Value[0] = 0;

				switch (CompSortMode)
				{
				case 0:
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Name, SymbolName, Value, ShapeName, PartNr);
					break;

				case 1:
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", SymbolName, Name, Value, ShapeName, PartNr);
					break;

				case 2:
					if (str[0] == '\t')
					{
						// No shapename
						res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, Value, PartNr);
					}
					else
						res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", ShapeName, Name, SymbolName, Value, PartNr);

					break;

				case 3:
					if (str[0] == '\t')
					{
						// No Value
						res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, ShapeName, PartNr);
					}
					else
						res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Value, Name, SymbolName, ShapeName, PartNr);

					break;

				case 4:
					if (str[0] == '\t')
					{
						// No partname
						res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, Value, ShapeName);
					}
					else
						res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", PartNr, Name, SymbolName, Value, ShapeName);

					break;
				}

				if (Name[0] != 0)
				{
					for (cnt2 = 0; cnt2 < Design.NrInstances; cnt2++)
					{
						Instance = (InstanceRecord *) & ((*Instances)[cnt2]);

						if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
						        && (Instance->Reference[0] != 0) && (strcmp(Instance->Reference, "?") != 0))
						{
							if (stricmp(Instance->Reference, Name) == 0)
							{
								if (!FirstInstance)
									FirstInstance = Instance;

								Instance->Info |= OBJECT_SELECTED | 7;
							}
						}
					}
				}
			}
		}

		cnt2++;

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Instance->Reference[0] != 0)
			        && (strcmp(Instance->Reference, "?") != 0))
			{
				Instance->Info &= ~OBJECT_DONE;

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_PROTECTED)) == OBJECT_SELECTED)
					cnt2++;
			}
		}

		if (FirstInstance)
		{
			strcpy(SearchCodeString, FirstInstance->Reference);
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_CENTER_ON_COMPONENT, (LPARAM) 1);
		}

		if (cnt2 == 0)
			return -1;

		break;

	case 4:
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0) && (Instance->Reference[0] != 0)
			        && (strcmp(Instance->Reference, "?") != 0))
				Instance->Info &= ~OBJECT_DONE;
		}

		for (cnt = 0; cnt < NrUsedComps; cnt++)
		{
			str[0] = 0;
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETTEXT, cnt, (LPARAM) str);

			if (res > 200)
				ok = 1;

//        res=SendDlgItemMessageOwn(Dialog,IDC_LIST1,LB_GETTEXT,cnt,(LPARAM)str);
			res2 = 0;
			Name[0] = 0;
			ShapeName[0] = 0;
			SymbolName[0] = 0;
			PartNr[0] = 0;
			Value[0] = 0;

			switch (OldCompSortMode)
			{
			case 0:
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Name, SymbolName, Value, ShapeName, PartNr);
				break;

			case 1:
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", SymbolName, Name, Value, ShapeName, PartNr);
				break;

			case 2:
				if (str[0] == '\t')
				{
					// No shapename
					res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, Value, PartNr);
				}
				else
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", ShapeName, Name, SymbolName, Value, PartNr);

				break;

			case 3:
				if (str[0] == '\t')
				{
					// No Value
					res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, ShapeName, PartNr);
				}
				else
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Value, Name, SymbolName, ShapeName, PartNr);

				break;

			case 4:
				if (str[0] == '\t')
				{
					// No partname
					res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, Value, ShapeName);
				}
				else
					res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", PartNr, Name, SymbolName, Value, ShapeName);

				break;
			}

			if (Name[0] != 0)
			{
				for (cnt2 = 0; cnt2 < Design.NrInstances; cnt2++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt2]);

					if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
					        && (Instance->Reference[0] != 0) && (strcmp(Instance->Reference, "?") != 0))
					{
						if (stricmp(Instance->Reference, Name) == 0)
						{
							res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSEL, cnt, 0);

							if (res > 0)
								Instance->Info |= OBJECT_DONE;
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
			strcpy(str, SC(503, "Ref\tSymbol name\tValue\tGeometry\tPartnr"));
			TabStops[0] = 30;
			TabStops[1] = 100;
			TabStops[2] = 240;
			break;

		case 1:
			strcpy(str, SC(504, "Symbol name\tRef\tValue\tGeometry\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;

		case 2:
			strcpy(str, SC(505, "Geometry\tRef\tSymbol name\tValue\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;

		case 3:
			strcpy(str, SC(506, "Value\tRef\tSymbol name\tGeometry\tPartnr"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;

		case 4:
			strcpy(str, SC(507, "Partnr\tRef\tSymbol name\tValue\tGeometry"));
			TabStops[0] = 140;
			TabStops[1] = 170;
			TabStops[2] = 260;
			break;
		}

		TabStops[3] = 380;
		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessage(Dialog, IDC_LIST3, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) str);

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0) && (Instance->Reference[0] != 0)
			        && (strcmp(Instance->Reference, "?") != 0))
			{
				GetCompString(Instance, str);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
			}
		}

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED | OBJECT_DONE)) == OBJECT_DONE)
			        && (Instance->Reference[0] != 0) && (strcmp(Instance->Reference, "?") != 0))
			{
				GetCompString(Instance, str);
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_FINDSTRING, (WPARAM) - 1, (LPARAM) str);

				if (res != LB_ERR)
					res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) res);
			}
		}

		break;

	case 5:
		res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_GETTEXT, IndexNr, (LPARAM) str);
		res2 = 0;
		Name[0] = 0;
		ShapeName[0] = 0;
		PartNr[0] = 0;
		Value[0] = 0;
		SymbolName[0] = 0;

		switch (CompSortMode)
		{
		case 0:
			res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Name, SymbolName, Value, ShapeName, PartNr);
			break;

		case 1:
			res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", SymbolName, Name, Value, ShapeName, PartNr);
			break;

		case 2:
			if (str[0] == '\t')
			{
				// No shapename
				res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, Value, PartNr);
			}
			else
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", ShapeName, Name, SymbolName, Value, PartNr);

			break;

		case 3:
			if (str[0] == '\t')
			{
				// No Value
				res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, ShapeName, PartNr);
			}
			else
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", Value, Name, SymbolName, ShapeName, PartNr);

			break;

		case 4:
			if (str[0] == '\t')
			{
				// No partname
				res2 = sscanf(str, "\t%s\t%s\t%s\t%s", Name, SymbolName, Value, ShapeName);
			}
			else
				res2 = sscanf(str, "%s\t%s\t%s\t%s\t%s", PartNr, Name, SymbolName, Value, ShapeName);

			break;
		}

		if (Name[0] != 0)
		{
			for (cnt2 = 0; cnt2 < Design.NrInstances; cnt2++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt2]);

				if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0) && (Instance->Reference[0] != 0)
				        && (strcmp(Instance->Reference, "?") != 0))
				{
					if (stricmp(Instance->Reference, Name) == 0)
					{
						strcpy(SearchCodeString, Instance->Reference);
						PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_CENTER_ON_COMPONENT, (LPARAM) 1);
					}
				}
			}
		}

		break;
	}

	return 0;
}

//************************************************************************************************************************
//******************************** IDD_DIALOG_COMP_SEL *******************************************************************
//************************************************************************************************************************

int32 CALLBACK ComponentSelectionDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res, SelectedComponents[10];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(491, "Component selections by list"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(492, "Sort by"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(493, "Move components"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(417, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(418, "Deselect all"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON4, SC(494, "Center screen on component"));

		SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETHORIZONTALEXTENT, 1200, 0);

		res = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(98, "Reference"));
		res = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(99, "Symbol name"));
		res = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(102, "Geometry"));
		res = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(100, "Value"));
		res = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) SC(101, "Part number"));

		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, CompSortMode, 0);
		OldCompSortMode = CompSortMode;
		ComponentSelectionFunction(Dialog, 0, 0);
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

		case IDC_BUTTON4:
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

		case IDC_BUTTON3:
			ComponentSelectionFunction(Dialog, 2, 0);
			break;

		case IDC_BUTTON2:
			ComponentSelectionFunction(Dialog, 1, 0);
			break;

		case IDHELP:
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
	    DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COMP_SEL), SCHWindow,
	              (DLGPROC) ComponentSelectionDialogBody);

	if (res == 2)
		PostMessage(SCHWindow, WM_COMMAND, ID_MOVE_OBJECTS, 0);
	else
		RePaint();

	return res;
}

//************************************************************************************************************************************
//********************************** IDD_DIALOG_REPLACE ******************************************************************************
//************************************************************************************************************************************

int32 CALLBACK ReplaceDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res, hiword;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		switch (DialogMode)
		{
		case 0:
		case 1:
			SetWindowTextUTF8(Dialog, SC(211, "Replace texts"));
			SetDialogItemTextUTF8(Dialog, IDOK, "OK");
			SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(312, "Find what"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(317, "Replace width"));
			SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(326, "Match case"));
			SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(346, "Whole words"));
			break;

		case 2:
		case 3:
			SetWindowTextUTF8(Dialog, SC(327, "Find nr text occurences"));
			break;

		case 4:
			break;
		}

		SendDlgItemMessage(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) SearchString);
		SendDlgItemMessage(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) ReplaceString);

		if ((SearchReplaceOptions & 0x01) == 0x01)
			SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		if ((SearchReplaceOptions & 0x02) == 0x02)
			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		if ((SearchReplaceOptions & 0x04) == 0x04)
			SendDlgItemMessage(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);

		/*
		      if ((SearchReplaceOptions & 0x08) == 0x08) {
		        SendDlgItemMessageUTF8(Dialog,IDC_CHECK4,BM_SETCHECK,1,0);
		      }
		      res=GetWindowRect(Dialog,&FindReplaceDialogRect);
		      res=GetWindowRect(Dialog,&FindReplaceDialogRectRelative);
		      FindReplaceDialogRectRelative.left-=RealWindow.left;
		      FindReplaceDialogRectRelative.top-=RealWindow.top;
		      FindReplaceDialogRectRelative.right-=RealWindow.right;
		      FindReplaceDialogRectRelative.bottom-=RealWindow.bottom;
		      SearchReplaceCount=0;
		      TotalReplaced=0;
		      WriteLineNr(TotalReplaced);
		      strcpy(OldSearchString,SearchString);
		      strcpy(OldReplaceString,ReplaceString);
		*/
		ok = 1;
		return about;

	case WM_MOVE:
		ok = 1;
		break;

	case WM_CHAR:
		ok = 1;
		break;

	case WM_SIZE:
		break;

	case WM_COMMAND:
		hiword = HIWORD(WParam);

		switch (LOWORD(WParam))
		{
		case IDOK:
			memset(DialogTextLine, 0, 200);
			res = SendDlgItemMessage(Dialog, IDC_EDIT1, WM_GETTEXT, 150, (LPARAM) DialogTextLine);

			if (res > 0)
				strcpy(SearchString, DialogTextLine);

			memset(DialogTextLine, 0, 200);
			ReplaceString[0] = 0;
			res = SendDlgItemMessage(Dialog, IDC_EDIT2, WM_GETTEXT, 150, (LPARAM) DialogTextLine);

			if (res > 0)
				strcpy(ReplaceString, DialogTextLine);

			SearchReplaceOptions = 0;

			if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 1, 0) == 1)
			{
				SearchReplaceOptions |= 1;	// Match case
			}

			if (SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 1, 0) == 1)
			{
				SearchReplaceOptions |= 2;	// Whole words
			}

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

int32 FindReplaceDialog(int32 Mode)
{
	DialogMode = Mode;
	return DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_REPLACE), SCHWindow, (DLGPROC) ReplaceDialog2);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
