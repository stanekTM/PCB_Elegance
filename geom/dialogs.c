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
#include "dialogs.h"
#include "stdlib.h"
#include "stdio.h"
#include "select.h"
#include "resource.h"
#include "geom.h"
#include "line2.h"
#include "calcdef.h"
#include "draw2.h"
#include "graphics.h"
#include "menus.h"
#include "help.h"
#include "draw2.h"
#include "files2.h"
#include "mainloop.h"
#include "ctype.h"
#include "../functionsc/version.h"
#include "utf8.h"

#define NrGeomRadioButtons  6
#define NrGeomInts          5
#define NrGeomFloats        22


int32 NetDialogMode, NetsSelected, DialogResult, DialogMode;
int32 ScaleOption = 0;;

char DialogTextLine[4096], OldPinStr[20];
LPSTR InitDialogTextLine, DialogWindowText, LineInputDialogText;
ObjectRecord *WorkingObject;
ObjectRecord2 *WorkingObject2;
int32 DCInUse2, Painting2;
double TempGridSize;

LPSTR MultiLineText, ViewText;

extern HDC OutputDisplay;
extern int32 SelectedColorNr;

extern double DefaultRuleSolderMask_TH, DefaultRulePasteMask_SMD, DefaultRuleSolderMask_SMD, DefaultRuleAntiPowerPad,
       DefaultRuleInnerPad, DefaultRulePad, DefaultRuleClearance;


int32 BGAGeomSet[20] = { IDD_GEOM_NRPINS_Y,
                         IDD_GEOM_PITCH,
                         IDD_GEOM_PAD,
                         IDD_GEOM_MASK,
                         IDD_GEOM_PAST,
                         IDD_GEOM_NRPINS_X,
                         IDD_GEOM_PIN1_SQUARE,
                         IDD_GEOM_PIN1_CIRCLE,
                         IDD_GEOM_CLEARANCE,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       };
int32 PGAGeomSet[20] = { IDD_GEOM_PITCH,
                         IDD_GEOM_PAD,
                         IDD_GEOM_MASK,
                         IDD_GEOM_INNER_PADSIZE,
                         IDD_GEOM_NRPINS_Y,
                         IDD_GEOM_DRILL,
                         IDD_GEOM_NRPINS_X,
                         IDD_GEOM_POW_PAD,
                         IDD_GEOM_PIN1_SQUARE,
                         IDD_GEOM_PIN1_CIRCLE,
                         IDD_GEOM_POW_PAD_BUT,
                         IDD_GEOM_INNER_PAD_BUT,
                         IDD_GEOM_CLEARANCE,
                         0, 0, 0, 0, 0, 0, 0
                       };

int32 SOICGeomSet[20] = { IDD_GEOM_PITCH_X,
                          IDD_GEOM_NRPINS,
                          IDD_GEOM_PAD_X,
                          IDD_GEOM_MASK_Y,
                          IDD_GEOM_PAD_Y,
                          IDD_GEOM_PAST_Y,
                          IDD_GEOM_MASK_X,
                          IDD_GEOM_PAST_X,
                          IDD_GEOM_PITCH_Y,
                          IDD_GEOM_CLEARANCE,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                        };
int32 DILGeomSet[20] = { IDD_GEOM_PITCH_X,
                         IDD_GEOM_PAD,
                         IDD_GEOM_MASK,
                         IDD_GEOM_PITCH_Y,
                         IDD_GEOM_INNER_PADSIZE,
                         IDD_GEOM_NRPINS,
                         IDD_GEOM_DRILL,
                         IDD_GEOM_POW_PAD,
                         IDD_GEOM_PIN1_SQUARE,
                         IDD_GEOM_PIN1_CIRCLE,
                         IDD_GEOM_POW_PAD_BUT,
                         IDD_GEOM_INNER_PAD_BUT,
                         IDD_GEOM_CLEARANCE,
                         0, 0, 0, 0, 0, 0, 0
                       };

int32 SILGeomSet[20] = { IDD_GEOM_START1,
                         IDD_GEOM_PAD,
                         IDD_GEOM_MASK,
                         IDD_GEOM_PITCH,
                         IDD_GEOM_INNER_PADSIZE,
                         IDD_GEOM_NRPINS,
                         IDD_GEOM_DRILL,
                         IDD_GEOM_START2,
                         IDD_GEOM_POW_PAD,
                         IDD_GEOM_INC,
                         IDD_GEOM_MASK_Y,
                         IDD_GEOM_PIN1_SQUARE,
                         IDD_GEOM_PIN1_CIRCLE,
                         IDD_GEOM_POW_PAD_BUT,
                         IDD_GEOM_INNER_PAD_BUT,
                         IDD_GEOM_CLEARANCE,
                         0, 0, 0, 0
                       };

int32 SIL_SMD_CIRCLEGeomSet[20] = { IDD_GEOM_NRPINS,
                                    IDD_GEOM_PAD,
                                    IDD_GEOM_MASK,
                                    IDD_GEOM_PAST,
                                    IDD_GEOM_START1,
                                    IDD_GEOM_START2,
                                    IDD_GEOM_PITCH,
                                    IDD_GEOM_INC,
                                    IDD_GEOM_TOP_LAYER,
                                    IDD_GEOM_BOTTOM_LAYER,
                                    IDD_GEOM_CLEARANCE,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0
                                  };

int32 SIL_SMD_RECTGeomSet[20] = { IDD_GEOM_NRPINS,
                                  IDD_GEOM_PAD_X,
                                  IDD_GEOM_MASK_Y,
                                  IDD_GEOM_PAD_Y,
                                  IDD_GEOM_PAST_Y,
                                  IDD_GEOM_MASK_X,
                                  IDD_GEOM_PAST_X,
                                  IDD_GEOM_START1,
                                  IDD_GEOM_START2,
                                  IDD_GEOM_PITCH,
                                  IDD_GEOM_INC,
                                  IDD_GEOM_TOP_LAYER,
                                  IDD_GEOM_BOTTOM_LAYER,
                                  IDD_GEOM_CLEARANCE,
                                  0, 0, 0, 0, 0, 0
                                };

int32 QUADGeomSet[20] = { IDD_GEOM_DIST_X,
                          IDD_GEOM_NRPINS_Y,
                          IDD_GEOM_PAD_X,
                          IDD_GEOM_PAD_Y,
                          IDD_GEOM_MASK_Y,
                          IDD_GEOM_DIST_Y,
                          IDD_GEOM_PAST_Y,
                          IDD_GEOM_MASK_X,
                          IDD_GEOM_PAST_X,
                          IDD_GEOM_NRPINS_X,
                          IDD_GEOM_PITCH,
                          IDD_GEOM_CLEARANCE,
                          IDD_GEOM_START1,
                          IDD_GEOM_START2,
                          0, 0, 0, 0, 0, 0
                        };

int32 TotalGeomFloatSet[30] = { IDD_GEOM_DIST_X,
                                IDD_GEOM_DIST_Y,
                                IDD_GEOM_DRILL,
                                IDD_GEOM_PAD,
                                IDD_GEOM_PAD_X,
                                IDD_GEOM_PAD_Y,
                                IDD_GEOM_PAST,
                                IDD_GEOM_PAST_X,
                                IDD_GEOM_PAST_Y,
                                IDD_GEOM_MASK,
                                IDD_GEOM_MASK_X,
                                IDD_GEOM_MASK_Y,
                                IDD_GEOM_PITCH,
                                IDD_GEOM_PITCH_X,
                                IDD_GEOM_PITCH_Y,
                                IDD_GEOM_POW_PAD,
                                IDD_GEOM_INNER_PADSIZE,
                                IDD_GEOM_CLEARANCE,
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                              };

int32 TotalGeomIntSet[10] = { IDD_GEOM_NRPINS,
                              IDD_GEOM_NRPINS_X,
                              IDD_GEOM_NRPINS_Y,
                              IDD_GEOM_START2,
                              IDD_GEOM_INC,
                              0, 0, 0, 0, 0
                            };

int32 TotalGeomRadioButtonSet[10] = { IDD_GEOM_POW_PAD_BUT,
                                      IDD_GEOM_INNER_PAD_BUT,
                                      IDD_GEOM_PIN1_SQUARE,
                                      IDD_GEOM_PIN1_CIRCLE,
                                      IDD_GEOM_TOP_LAYER,
                                      IDD_GEOM_BOTTOM_LAYER,
                                      0, 0, 0, 0
                                    };


// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

COLORREF GetNewColor(int32 mode, COLORREF InitialColor, HWND CurrentWindow);

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SetDialogValue(HWND Dialog, int32 Control, double value)
{
	char str[MAX_LENGTH_STRING];

	if (Units == 0)
		sprintf(str, "%.1f", value / 2540.0);
	else
		sprintf(str, "%.4f", value / 100000.0);

	SendDlgItemMessage(Dialog, Control, WM_SETTEXT, 0, (LPARAM) str);

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

double GetDialogValue(HWND Dialog, int32 Control)
{
	char str[MAX_LENGTH_STRING];
	float value;

	value = 0.0;
	SendDlgItemMessage(Dialog, Control, WM_GETTEXT, 80, (LPARAM) str);

	if (sscanf(str, "%f", &value) == 1)
	{
		if (Units == 0)
			value *= 2540.0;
		else
			value *= 100000.0;
	}

	return value;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

double GetDialogValue2(HWND Dialog, int32 DialogControl)
{
	char str[MAX_LENGTH_STRING];
	float FloatValue;
	int32 le, OwnUnits, res;

	FloatValue = 0.0;
	memset(&str, 0, sizeof(str));
	SendDlgItemMessage(Dialog, DialogControl, WM_GETTEXT, 80, (LPARAM) str);

	if ((res = sscanf(str, "%f", &FloatValue)) == 1)
	{
		_strlwr(str);
		le = strlen(str);
		OwnUnits = -1;

		if (le > 2)
		{
			if ((str[le - 1] == 'm') && (str[le - 2] == 'm'))
				OwnUnits = 1;

			if ((str[le - 1] == 'h') && (str[le - 2] == 't'))
				OwnUnits = 0;
		}

		if (le > 4)
		{
			if ((str[le - 1] == 's') && (str[le - 2] == 'l') && (str[le - 3] == 'i') && (str[le - 4] == 'm'))
			{
//          ParamsFloat[cnt]*=2540.0;
				OwnUnits = 0;
			}

			if ((str[le - 1] == 'h') && (str[le - 2] == 'c') && (str[le - 3] == 'n') && (str[le - 4] == 'i'))
			{
//          ParamsFloat[cnt]*=2540.0;
				OwnUnits = 2;
			}
		}

		if (OwnUnits != -1)
		{
			switch (OwnUnits)
			{
			case 0:
				FloatValue *= 2540.0;
				break;

			case 1:
				FloatValue *= 100000.0;
				break;

			case 2:
				FloatValue *= 2540000.0;
				break;
			}
		}
		else
		{
			if (Units == 0)
				FloatValue *= 2540.0;
			else
				FloatValue *= 100000.0;
		}
	}

	return FloatValue;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SetDialogItemText(HWND Dialog, int32 DlgItem, LPSTR Text)
{
	SendDlgItemMessageUTF8(Dialog, DlgItem, WM_SETTEXT, 0, (LPARAM) Text);
}

//*********************************************************************************************************************************
//******************* IDD_DIALOG_TEXTINPUT * IDD_DIALOG_TEXTINPUT2 * IDD_DIALOG_TEXTINPUT4 ****************************************
//*********************************************************************************************************************************

int32 CALLBACK TextInputDialogBody(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char sel[MAX_LENGTH_STRING];
	float value;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		switch (DialogMode & 0xf)
		{
		case 0:
			SetWindowTextUTF8(Dialog, SC(35, "Add text"));
			break;

		case 1:
			SetWindowTextUTF8(Dialog, SC(36, "Change text"));
			break;

		case 2:
			SetWindowTextUTF8(Dialog, SC(37, "Change geometry name"));
			break;

		case 3:
			SetWindowTextUTF8(Dialog, SC(38, "Scale factor"));
			break;

		case 4:
			SetWindowTextUTF8(Dialog, SC(39, "Rotation angle (positive is counter clock wise)"));
			break;

		case 5:
			SetWindowTextUTF8(Dialog, SC(40, "Nr layers (2..16)"));
			break;

		case 6:
			SetWindowTextUTF8(Dialog, SC(139, "Pin text"));
			break;

		case 7:
			SetWindowTextUTF8(Dialog, SC(105, "Geometry height"));
			break;

		case 8:
			SetWindowTextUTF8(Dialog, SC(128, "Goto x,y"));
			break;
		}

		if ((DialogMode & 0x10) == 0)
		{
			switch (Units)
			{
			case 0:
				sprintf(sel, "%.1f", WorkingObject->x2 / 2540.0);
				SendDlgItemMessage(Dialog, IDD_GEOM_UN1, LB_DELETESTRING, 0, 0);
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "thou");
				break;

			case 1:
				sprintf(sel, "%.4f", WorkingObject->x2 / 100000.0);
				SendDlgItemMessage(Dialog, IDD_GEOM_UN1, LB_DELETESTRING, 0, 0);
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "mm");
				break;
			}

			SendDlgItemMessage(Dialog, IDD_TEXTINPUT_EDIT2, WM_SETTEXT, 0, (LPARAM) & sel);
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDC_STATIC1, SC(44, "Text height"));
		}

		SendDlgItemMessage(Dialog, IDD_TEXTINPUT_EDIT1, WM_SETTEXT, 0, (LPARAM) & (WorkingObject->Text));

		if ((DialogMode & 0x100) == 0x100)
		{
			if ((ScaleOption & 1) == 0)
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);
			else
				SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 0, 0);
		}

		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:

//          res=(int32)SendDlgItemMessage(Dialog,ID_LISTBOX_NETS2,LB_GETCURSEL,0,0);
//          if (res!=LB_ERR) UnselectTracesViasNet(res);
//  Static_SetText sscanf

			if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0))
				ScaleOption &= ~1;
			else
				ScaleOption |= 1;

			if (SendDlgItemMessageUTF8
			        (Dialog, IDD_TEXTINPUT_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine) > 0)
			{
				memset(&(WorkingObject->Text), sizeof(WorkingObject->Text), 0);
				memmove(&(WorkingObject->Text), &DialogTextLine, sizeof(WorkingObject->Text) - 1);
			}

			if ((DialogMode & 0x10) == 0)
			{
				if (SendDlgItemMessageUTF8
				        (Dialog, IDD_TEXTINPUT_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine))
				{
					if (sscanf(DialogTextLine, "%f", &value) == 1)
					{
						WorkingObject->x2 = value;

						switch (Units)
						{
						case 0:
							WorkingObject->x2 *= 2540.0;
							break;

						case 1:
							WorkingObject->x2 *= 100000.0;
							break;
						}
					}

					if (WorkingObject->x2 < 1000.0)
						WorkingObject->x2 = (100 * 2540.0);
				}
			}

			EndDialog(Dialog, 1);
			return about;

		case ID_UNITS:
			if ((DialogMode & 0x10) == 0)
			{
				if (++Units == 2)
					Units = 0;

				switch (Units)
				{
				case 0:
					sprintf(sel, "%.1f", WorkingObject->x2 / 2540.0);
					SendDlgItemMessage(Dialog, IDD_GEOM_UN1, LB_DELETESTRING, 0, 0);
					SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "thou");
					break;

				case 1:
					sprintf(sel, "%.4f", WorkingObject->x2 / 100000.0);
					SendDlgItemMessage(Dialog, IDD_GEOM_UN1, LB_DELETESTRING, 0, 0);
					SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, LB_ADDSTRING, 0, (LPARAM) (LPSTR) "mm");
					break;
				}

				SendDlgItemMessageUTF8(Dialog, IDD_TEXTINPUT_EDIT2, WM_SETTEXT, 0, (LPARAM) & sel);
			}

			break;

		case IDCANCEL:
			memset(DialogTextLine, 0, 200);
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			switch (DialogMode & 0xf)
			{
			case 5:
				Help("nr_layers.htm", 0);
			}

			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 TextInputDialog(ObjectRecord * ObjectText, int32 Mode)
{
	int res, ok;

//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;
	DialogMode = Mode;
	WorkingObject = ObjectText;

	if ((Mode & 0x10) == 0)
	{
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT), GEOMWindow,
		              (DLGPROC) TextInputDialogBody);
	}
	else
	{
		if ((Mode & 0x100) == 0)
		{
			res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT2), GEOMWindow,
			              (DLGPROC) TextInputDialogBody);
		}
		else
		{
			res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT4), GEOMWindow,
			              (DLGPROC) TextInputDialogBody);
		}
	}

	ok = 1;
	return res;
}

//********************************************************************************************************************************
//******************** IDD_DIALOG_TEXTINPUT3 *************************************************************************************
//********************************************************************************************************************************

int32 CALLBACK TextInputDialog2Body(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, cnt;
	uint16 *Lengte;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		SetDialogItemText(Dialog, IDC_STATIC1, "");
		SetWindowTextUTF8(Dialog, LineInputDialogText);
		SendDlgItemBigMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) (LPSTR) MultiLineText);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			for (cnt = 0; cnt < 512; cnt++)
			{
				Lengte = (uint16 *) & DialogTextLine;
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 50;

				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) DialogTextLine)) > 0)
				{
					DialogTextLine[res] = 0;
					strcat(MultiLineText, DialogTextLine);
					strcat(MultiLineText, "\r");
				}
			}

			strcat(MultiLineText, "\0");
			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			strcat(MultiLineText, "\0");
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
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT3), GEOMWindow,
	              (DLGPROC) TextInputDialog2Body);
	return res;
}

//********************************************************************************************************************************
//*********************** IDD_DIALOG_MESSAGE *************************************************************************************
//********************************************************************************************************************************

int32 CALLBACK MessageDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 TabStops[10];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

//      SendDlgItemMessage(Dialog,IDD_MESSAGE_EDIT,WM_SETTEXT,0,(LPARAM)DialogTextLine);
		switch (DialogMode)
		{
		case 0:
		case 1:
			TabStops[0] = 60;
			TabStops[1] = 155;
			TabStops[2] = 195;
			TabStops[3] = 335;
			TabStops[4] = 445;
			TabStops[5] = 480;

			if (DialogMode == 0)
				SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT, EM_SETTABSTOPS, 6, (LPARAM) (LPINT) & TabStops);
			
			SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT2, EM_SETTABSTOPS, 6, (LPARAM) (LPINT) & TabStops);
			break;

		case 2:
		case 3:
			TabStops[0] = 335;
			TabStops[1] = 445;
			TabStops[2] = 480;
			SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT, EM_SETTABSTOPS, 3, (LPARAM) (LPINT) & TabStops);
			SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT2, EM_SETTABSTOPS, 3, (LPARAM) (LPINT) & TabStops);

			if (DialogMode == 2)
				SetWindowTextUTF8(Dialog, SC(48, "Error(s)"));
			else
				SetWindowTextUTF8(Dialog, SC(49, "Warning(s)"));

			break;
		}

		SendDlgItemMessageUTF8(Dialog, IDD_MESSAGE_EDIT2, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		SendDlgItemBigMessageUTF8(Dialog, IDD_MESSAGE_EDIT, WM_SETTEXT, 0, (LPARAM) (LPSTR) MessageBuf);

		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
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

int32 MessageDialog(LPSTR InfoLine, int32 Mode)
{
	int res, ok;

	DialogMode = Mode;
	strcpy(DialogTextLine, InfoLine);
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_MESSAGE), GEOMWindow, (DLGPROC) MessageDialog2);
	ok = 1;
	return res;
}

// **********************************************************************************************************************
// ******************************* IDD_DIALOG_ADD_TEXT_OBJECTS **********************************************************
// **********************************************************************************************************************

#ifdef _DEBUG

int32 CALLBACK AddTextObjectsDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, TabStops[10];
	uint16 *Lengte;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(4, "Message"));

		TabStops[0] = 110;
		TabStops[1] = 150;
		TabStops[2] = 200;
		TabStops[3] = 310;
		TabStops[4] = 370;
//      TabStops[5]=430;

		SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT, WM_SETTEXT, 0, (LPARAM) "");

		if (DialogMode == 0)
			SendDlgItemMessage(Dialog, IDD_MESSAGE_EDIT, EM_SETTABSTOPS, 5, (LPARAM) (LPINT) & TabStops);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			MessageBufPos = 0;
			Lengte = (uint16 *) & DialogTextLine;

			for (cnt = 0; cnt < 1024; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 50;

				if (SendDlgItemMessageUTF8(Dialog, IDD_MESSAGE_EDIT, EM_GETLINE, cnt, (LPARAM) DialogTextLine) == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);

				if (DialogTextLine[0] != 0)
				{
					sprintf(str, "%s\r\n", DialogTextLine);
					AddToMessageBuf(str);
				}
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

int32 AddTextObjectsDialog(int32 Mode)
{
	int res, ok;

	DialogMode = Mode;
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ADD_TEXT_OBJECTS), GEOMWindow,
	              (DLGPROC) AddTextObjectsDialog2);
	ok = 1;
	return res;
}

#endif

//************************************************************************************************************************************
//************************** IDD_DIALOG_LINEINPUT ************************************************************************************
//************************************************************************************************************************************

int32 CALLBACK LineInputDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, LineInputDialogText);
		SendDlgItemMessageUTF8(Dialog, IDD_LINEINPUT_EDIT1, WM_SETTEXT, 0, (LPARAM) & (WorkingObject2->Text));

		if (Units == 0)
			SendDlgItemMessageUTF8(Dialog, IDD_LINEINPUT_EDIT2, WM_SETTEXT, 0, (LPARAM) "thou");
		else
			SendDlgItemMessageUTF8(Dialog, IDD_LINEINPUT_EDIT2, WM_SETTEXT, 0, (LPARAM) "mm");

		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:

			if (SendDlgItemMessageUTF8
			        (Dialog, IDD_LINEINPUT_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine) > 0)
			{
				memset(&(WorkingObject2->Text), sizeof(WorkingObject2->Text), 0);
				memmove(&(WorkingObject2->Text), &DialogTextLine, sizeof(WorkingObject2->Text) - 1);
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

int32 LineInputDialog(ObjectRecord2 * ObjectText, LPSTR DialogText)
{
	int res, ok;

	LineInputDialogText = DialogText;
	WorkingObject2 = ObjectText;
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LINEINPUT), GEOMWindow, (DLGPROC) LineInputDialog2);
	ok = 1;
	return res;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 CheckDialogControl(int32 DialogMode, int32 DialogControl)
{
	int32 cnt;

	switch (DialogMode)
	{
	case IDD_DIALOG_BGA:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == BGAGeomSet[cnt])
				return 1;

		break;

	case IDD_DIALOG_DIL:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == DILGeomSet[cnt])
				return 1;

		break;

	case IDD_DIALOG_PGA:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == PGAGeomSet[cnt])
				return 1;

		break;

	case IDD_DIALOG_QUAD:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == QUADGeomSet[cnt])
				return 1;

		break;

	case IDD_DIALOG_SIL:
	case IDD_DIALOG_SIL2:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == SILGeomSet[cnt])
				return 1;

		break;

	case IDD_DIALOG_SIL_SMD_RECT:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == SIL_SMD_RECTGeomSet[cnt])
				return 1;

		break;

	case IDD_DIALOG_SIL_SMD_CIRCLE:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == SIL_SMD_CIRCLEGeomSet[cnt])
				return 1;

		break;

	case IDD_DIALOG_SOIC:
		for (cnt = 0; cnt < 20; cnt++)
			if (DialogControl == SOICGeomSet[cnt])
				return 1;

		break;
	}

	return 0;

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetDialogText(HWND Dialog, int32 DialogMode, int32 DialogControl)
{
	if (!CheckDialogControl(DialogMode, DialogControl))
		return 0;

	if (SendDlgItemMessageUTF8(Dialog, DialogControl, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) DialogTextLine))
		return 2;

	return 1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SetDialogText(HWND Dialog, int32 DialogMode, int32 DialogControl)
{
	if (!CheckDialogControl(DialogMode, DialogControl))
		return 0;

	if (SendDlgItemMessageUTF8(Dialog, DialogControl, WM_SETTEXT, (WPARAM) 0, (LPARAM) DialogTextLine))
		return 1;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SetDialogFloatValue(HWND Dialog, int32 DialogMode, int32 DialogControl, double value, int32 TempUnits)
{
	int32 res;

	switch (TempUnits)
	{
	case 0:					// thou
		value = value / 2540;
		sprintf(DialogTextLine, "%.1f", value);
		break;

	case 1:					// mm
		value = value / 100000;
		sprintf(DialogTextLine, "%.4f", value);
		break;
	}

	if (value == 0.0)
		strcpy(DialogTextLine, "0.0");
//	else
//	{

//	}

	if (!CheckDialogControl(DialogMode, DialogControl))
		return 0;

	if ((res = SendDlgItemMessageUTF8(Dialog, DialogControl, WM_SETTEXT, 0, (LPARAM) DialogTextLine)) != 0)
		return 1;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SetDialogIntValue(HWND Dialog, int32 DialogMode, int32 DialogControl, int32 value)
{
	int32 res;

	sprintf(DialogTextLine, "%i", value);

	if (!CheckDialogControl(DialogMode, DialogControl))
		return 0;

	if ((res = SendDlgItemMessageUTF8(Dialog, DialogControl, WM_SETTEXT, (WPARAM) 0, (LPARAM) DialogTextLine)) != 0)
		return 1;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SetDialogRadioButtonValue(HWND Dialog, int32 DialogMode, int32 DialogControl, int32 value)
{
	int32 res, num;

	if (!CheckDialogControl(DialogMode, DialogControl))
		return 0;

	if (value)
		num = 1;
	else
		num = 0;

	if ((res = SendDlgItemMessage(Dialog, DialogControl, BM_SETCHECK, num, 0)) != 0)
		return 1;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

GeomCreateRecord *GetGeomValueRecord(int32 DialogMode)
{
	switch (DialogMode)
	{
	case IDD_DIALOG_BGA:
		return &BGAGeom;
		break;

	case IDD_DIALOG_DIL:
		return &DILGeom;
		break;

	case IDD_DIALOG_PGA:
		return &PGAGeom;
		break;

	case IDD_DIALOG_QUAD:
		return &QUADGeom;
		break;

	case IDD_DIALOG_SIL:
	case IDD_DIALOG_SIL2:
		return &SILGeom;
		break;

	case IDD_DIALOG_SIL_SMD_RECT:
		return &SIL_SMD_RECTGeom;
		break;

	case IDD_DIALOG_SIL_SMD_CIRCLE:
		return &SIL_SMD_CIRCLEGeom;
		break;

	case IDD_DIALOG_SOIC:
		return &SOICGeom;
		break;
	}

	return NULL;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SetGetGeomIntValue(int32 DialogMode, int32 DialogControl, int32 value, int32 mode)
{
	GeomCreateRecord *TempGeom;

	TempGeom = GetGeomValueRecord(DialogMode);

	switch (DialogControl)
	{
	case IDD_GEOM_NRPINS:
		if (mode == 0)
			return TempGeom->NrPins;
		else
			TempGeom->NrPins = value;

		break;

	case IDD_GEOM_NRPINS_X:
		if (mode == 0)
			return TempGeom->NrPinsX;
		else
			TempGeom->NrPinsX = value;

		break;

	case IDD_GEOM_NRPINS_Y:
		if (mode == 0)
			return TempGeom->NrPinsY;
		else
			TempGeom->NrPinsY = value;

		break;

	case IDD_GEOM_START2:
		if (mode == 0)
			return TempGeom->Start2;
		else
			TempGeom->Start2 = value;

		break;

	case IDD_GEOM_INC:
		if (mode == 0)
			return TempGeom->PinInc;
		else
			TempGeom->PinInc = value;

		break;

	}

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SetGetGeomRadioButtonValue(int32 DialogMode, int32 DialogControl, int32 value, int32 mode)
{
	GeomCreateRecord *TempGeom;

	TempGeom = GetGeomValueRecord(DialogMode);

	switch (DialogControl)
	{
	case IDD_GEOM_PIN1_SQUARE:
		if (mode == 0)
			return TempGeom->Pin1Square;
		else
			TempGeom->Pin1Square = value;

		break;

	case IDD_GEOM_PIN1_CIRCLE:
		if (mode == 0)
			return !TempGeom->Pin1Square;
		else
			TempGeom->Pin1Square = !value;

		break;

	case IDD_GEOM_TOP_LAYER:
		if (mode == 0)
			return TempGeom->Layer;
		else
			TempGeom->Layer = value;

	case IDD_GEOM_BOTTOM_LAYER:
		if (mode == 0)
			return !TempGeom->Layer;
		else
			TempGeom->Layer = !value;

		break;
	}

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

double SetGetGeomFloatValue(int32 DialogMode, int32 DialogControl, double value, int32 mode)
{
	GeomCreateRecord *TempGeom;

	TempGeom = GetGeomValueRecord(DialogMode);

	switch (DialogControl)
	{
	case IDD_GEOM_DIST_X:
		if (mode == 0)
			return TempGeom->DistX;
		else
			TempGeom->DistX = value;

		break;

	case IDD_GEOM_DIST_Y:
		if (mode == 0)
			return TempGeom->DistY;
		else
			TempGeom->DistY = value;

		break;

	case IDD_GEOM_DRILL:
		if (mode == 0)
			return TempGeom->Drill;
		else
			TempGeom->Drill = value;

		break;

	case IDD_GEOM_PAD:
		if (mode == 0)
			return TempGeom->Pad;
		else
			TempGeom->Pad = value;

		break;

	case IDD_GEOM_PAD_X:
		if (mode == 0)
			return TempGeom->PadX;
		else
			TempGeom->PadX = value;

		break;

	case IDD_GEOM_PAD_Y:
		if (mode == 0)
			return TempGeom->PadY;
		else
			TempGeom->PadY = value;

		break;

	case IDD_GEOM_PAST:
		if (mode == 0)
			return TempGeom->Paste;
		else
			TempGeom->Paste = value;

		break;

	case IDD_GEOM_PAST_X:
		if (mode == 0)
			return TempGeom->PasteX;
		else
			TempGeom->PasteX = value;

		break;

	case IDD_GEOM_PAST_Y:
		if (mode == 0)
			return TempGeom->PasteY;
		else
			TempGeom->PasteY = value;

		break;

	case IDD_GEOM_MASK:
		if (mode == 0)
			return TempGeom->Mask;
		else
			TempGeom->Mask = value;

		break;

	case IDD_GEOM_MASK_X:
		if (mode == 0)
			return TempGeom->MaskX;
		else
			TempGeom->MaskX = value;

		break;

	case IDD_GEOM_MASK_Y:
		if (mode == 0)
			return TempGeom->MaskY;
		else
			TempGeom->MaskY = value;

		break;

	case IDD_GEOM_POW_PAD:
		if (mode == 0)
			return TempGeom->PowerPad;
		else
			TempGeom->PowerPad = value;

		break;

	case IDD_GEOM_INNER_PADSIZE:
		if (mode == 0)
			return TempGeom->InnerPad;
		else
			TempGeom->InnerPad = value;

		break;

	case IDD_GEOM_PITCH:
		if (mode == 0)
			return TempGeom->Pitch;
		else
			TempGeom->Pitch = value;

		break;

	case IDD_GEOM_PITCH_X:
		if (mode == 0)
			return TempGeom->PitchX;
		else
			TempGeom->PitchX = value;

		break;

	case IDD_GEOM_PITCH_Y:
		if (mode == 0)
			return TempGeom->PitchY;
		else
			TempGeom->PitchY = value;

		break;

	case IDD_GEOM_CLEARANCE:
		if (mode == 0)
			return TempGeom->Clearance;
		else
			TempGeom->Clearance = value;

		break;
	}

	return 0.0;


}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void ChangeDialogUnits(HWND Dialog, int32 DialogMode, int32 DialogControl, int32 OldUnits, int32 NewUnits)
{
	int32 res;
	float value;

	if ((res = GetDialogText(Dialog, DialogMode, DialogControl)) != 0)
	{
		if (res == 1)
			strcpy(DialogTextLine, "0");

		if ((res = sscanf(DialogTextLine, "%f", &value)) == 1)
		{
			switch (OldUnits)
			{
			case 0:
				value *= (float) 2540;

				switch (NewUnits)
				{
				case 0:		// thou -> mm
					value *= (float) 0.0254;
					break;
				}

				break;

			case 1:
				value *= (float) 100000;

				switch (NewUnits)
				{
				case 1:		//  mm -> thou
					value /= (float) 0.0254;
					break;
				}

				break;
			}

			SetDialogFloatValue(Dialog, DialogMode, DialogControl, value, NewUnits);
		}
	}

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SetUnitsCreateGeom(HWND Dialog, int32 DialogMode, int32 OldUnits, int32 NewUnits)
{
	char str[10];
	int32 count, cnt;
	GeomCreateRecord *TempGeom;

	TempGeom = GetGeomValueRecord(DialogMode);
	count = 0;

	switch (DialogMode)
	{
	case IDD_DIALOG_BGA:
		count = 5;
		break;

	case IDD_DIALOG_DIL:
		count = 10;
		break;

	case IDD_DIALOG_PGA:
		count = 9;
		break;

	case IDD_DIALOG_QUAD:
		count = 10;
		break;

	case IDD_DIALOG_SIL:
	case IDD_DIALOG_SIL2:
		count = 9;
		break;

	case IDD_DIALOG_SIL_SMD_RECT:
		count = 8;
		break;

	case IDD_DIALOG_SIL_SMD_CIRCLE:
		count = 5;
		break;

	case IDD_DIALOG_SOIC:
		count = 9;
		break;
	}

	switch (NewUnits)
	{
	case 0:
		strcpy(str, "thou");
		break;

	case 1:
		strcpy(str, "mm");
		break;
	}

	for (cnt = 0; cnt < count; cnt++)
	{
		switch (cnt)
		{
		case 0:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN1, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN1, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 1:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN2, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN2, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 2:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN3, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN3, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 3:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN4, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN4, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 4:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN5, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN5, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 5:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN6, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN6, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 6:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN7, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN7, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 7:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN8, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN8, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 8:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN9, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN9, LB_ADDSTRING, 0, (LPARAM) str);
			break;

		case 9:
			SendDlgItemMessage(Dialog, IDD_GEOM_UN10, LB_DELETESTRING, 0, 0);
			SendDlgItemMessage(Dialog, IDD_GEOM_UN10, LB_ADDSTRING, 0, (LPARAM) str);
			break;
		}
	}

	if ((OldUnits != -1) && (OldUnits != NewUnits))
	{
		for (cnt = 0; cnt < NrGeomFloats; cnt++)
			ChangeDialogUnits(Dialog, DialogMode, TotalGeomFloatSet[cnt], OldUnits, NewUnits);

		TempGeom->Units = NewUnits;
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void InitDialogValues(HWND Dialog, int32 DialogMode)
{
	int32 cnt, DialogControl;
	GeomCreateRecord *TempGeom;

	TempGeom = GetGeomValueRecord(DialogMode);


	switch (DialogMode)
	{
	case IDD_DIALOG_SIL:
		TempGeom->Pin1Square = 0;
		break;

	case IDD_DIALOG_SIL2:
		TempGeom->Pin1Square = 1;
		break;
	}

	for (cnt = 0; cnt < NrGeomFloats; cnt++)
	{
		DialogControl = TotalGeomFloatSet[cnt];

		if (CheckDialogControl(DialogMode, DialogControl))
		{
			SetDialogFloatValue(Dialog, DialogMode, DialogControl,
			                    SetGetGeomFloatValue(DialogMode, DialogControl, 0.0, 0), Units);
		}
	}

	for (cnt = 0; cnt < NrGeomInts; cnt++)
	{
		DialogControl = TotalGeomIntSet[cnt];

		if (CheckDialogControl(DialogMode, DialogControl))
			SetDialogIntValue(Dialog, DialogMode, DialogControl, SetGetGeomIntValue(DialogMode, DialogControl, 0, 0));
	}

	for (cnt = 0; cnt < NrGeomRadioButtons; cnt++)
	{
		DialogControl = TotalGeomRadioButtonSet[cnt];

		if (CheckDialogControl(DialogMode, DialogControl))
		{
			SetDialogRadioButtonValue(Dialog, DialogMode, DialogControl,
			                          SetGetGeomRadioButtonValue(DialogMode, DialogControl, 0, 0));
		}
	}

	if (CheckDialogControl(DialogMode, IDD_GEOM_START1))
	{
		strcpy(DialogTextLine, (LPSTR) TempGeom->Start1);
		SetDialogText(Dialog, DialogMode, IDD_GEOM_START1);
	}

	SetUnitsCreateGeom(Dialog, DialogMode, -1, Units);
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SetDialogRuleValues(HWND Dialog, int32 DialogMode)
{
	GeomCreateRecord *TempGeom;
	double value, NewValue;

	TempGeom = GetGeomValueRecord(DialogMode);

	/*
	double                       DefaultRuleSolderMask_TH=0.1e5;
	double                       DefaultRulePasteMask_SMD=0.0;
	double                       DefaultRuleSolderMask_SMD=0.1e5;
	double                       DefaultRuleAntiPowerPad=0.75e5;
	double                       DefaultRuleInnerPad=0.55e5;
	*/

	switch (DialogMode)
	{
	case IDD_DIALOG_BGA:
	case IDD_DIALOG_SIL_SMD_CIRCLE:
		value = GetDialogValue2(Dialog, IDD_GEOM_PAD);

		if (value != 0.0)
		{
			NewValue = value + DefaultRulePasteMask_SMD;
			SetDialogValue(Dialog, IDD_GEOM_PAST, NewValue);
			NewValue = value + DefaultRuleSolderMask_SMD;
			SetDialogValue(Dialog, IDD_GEOM_MASK, NewValue);
		}

		break;

	case IDD_DIALOG_DIL:
	case IDD_DIALOG_PGA:
	case IDD_DIALOG_SIL:
	case IDD_DIALOG_SIL2:
		value = GetDialogValue2(Dialog, IDD_GEOM_DRILL);

		if (value != 0.0)
		{
			NewValue = value + DefaultRulePad;
			SetDialogValue(Dialog, IDD_GEOM_PAD, NewValue);
			NewValue = value + DefaultRulePad + DefaultRuleSolderMask_SMD;
			SetDialogValue(Dialog, IDD_GEOM_MASK, NewValue);
			NewValue = value + DefaultRuleAntiPowerPad;
			SetDialogValue(Dialog, IDD_GEOM_POW_PAD, NewValue);
			NewValue = value + DefaultRuleInnerPad;
			SetDialogValue(Dialog, IDD_GEOM_INNER_PADSIZE, NewValue);
		}

		break;

	case IDD_DIALOG_QUAD:
	case IDD_DIALOG_SOIC:
	case IDD_DIALOG_SIL_SMD_RECT:
		value = GetDialogValue2(Dialog, IDD_GEOM_PAD_X);

		if (value != 0.0)
		{
			NewValue = value + DefaultRulePasteMask_SMD;
			SetDialogValue(Dialog, IDD_GEOM_PAST_X, NewValue);
			NewValue = value + DefaultRuleSolderMask_SMD;
			SetDialogValue(Dialog, IDD_GEOM_MASK_X, NewValue);
		}

		value = GetDialogValue2(Dialog, IDD_GEOM_PAD_Y);

		if (value != 0.0)
		{
			NewValue = value + DefaultRulePasteMask_SMD;
			SetDialogValue(Dialog, IDD_GEOM_PAST_Y, NewValue);
			NewValue = value + DefaultRuleSolderMask_SMD;
			SetDialogValue(Dialog, IDD_GEOM_MASK_Y, NewValue);
		}

		break;
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetDialogValues(HWND Dialog, int32 DialogMode, int32 TempUnits, int32 mode)
{
	int32 res, cnt, DialogControl, IntValue, le, OwnUnits;
	float FloatValue;
	int32 BoolValue;
	GeomCreateRecord *TempGeom;

	TempGeom = GetGeomValueRecord(DialogMode);

	for (cnt = 0; cnt < NrGeomFloats; cnt++)
	{
		DialogControl = TotalGeomFloatSet[cnt];

		if (CheckDialogControl(DialogMode, DialogControl))
		{
			if ((res = GetDialogText(Dialog, DialogMode, DialogControl)) != 0)
			{
				if (res == 1)
					strcpy(DialogTextLine, "0");

				if ((res = sscanf(DialogTextLine, "%f", &FloatValue)) == 1)
				{
					_strlwr(DialogTextLine);
					le = strlen(DialogTextLine);
					OwnUnits = -1;

					if (le > 2)
					{
						if ((DialogTextLine[le - 1] == 'm') && (DialogTextLine[le - 2] == 'm'))
							OwnUnits = 1;

						if ((DialogTextLine[le - 1] == 'h') && (DialogTextLine[le - 2] == 't'))
							OwnUnits = 0;
					}

					if (le > 4)
					{
						if ((DialogTextLine[le - 1] == 's') && (DialogTextLine[le - 2] == 'l')
						        && (DialogTextLine[le - 3] == 'i') && (DialogTextLine[le - 4] == 'm'))
						{
							//          ParamsFloat[cnt]*=2540.0;
							OwnUnits = 0;
						}

						if ((DialogTextLine[le - 1] == 'h') && (DialogTextLine[le - 2] == 'c')
						        && (DialogTextLine[le - 3] == 'n') && (DialogTextLine[le - 4] == 'i'))
						{
							//          ParamsFloat[cnt]*=2540.0;
							OwnUnits = 2;
						}
					}

					if (OwnUnits != -1)
					{
						switch (OwnUnits)
						{
						case 0:
							FloatValue *= 2540.0;
							break;

						case 1:
							FloatValue *= 100000.0;
							break;

						case 2:
							FloatValue *= 2540000.0;
							break;
						}
					}
					else
					{
						if (TempUnits == 0)
							FloatValue *= 2540.0;
						else
							FloatValue *= 100000.0;
					}

					SetGetGeomFloatValue(DialogMode, DialogControl, FloatValue, 1);
				}
			}
		}
	}

	for (cnt = 0; cnt < NrGeomInts; cnt++)
	{
		DialogControl = TotalGeomIntSet[cnt];

		if (CheckDialogControl(DialogMode, DialogControl))
		{
			if ((res = GetDialogText(Dialog, DialogMode, DialogControl)) != 0)
			{
				if (res == 1)
					strcpy(DialogTextLine, "0");

				if ((res = sscanf(DialogTextLine, "%i", &IntValue)) == 1)
					SetGetGeomIntValue(DialogMode, DialogControl, IntValue, 1);
			}
		}
	}

	for (cnt = 0; cnt < NrGeomRadioButtons; cnt++)
	{
		DialogControl = TotalGeomRadioButtonSet[cnt];

		if (CheckDialogControl(DialogMode, DialogControl))
		{
			res = SendDlgItemMessage(Dialog, DialogControl, BM_GETCHECK, 0, 0);
			BoolValue = 0;

			if (res == 1)
				BoolValue = 1;

			SetGetGeomRadioButtonValue(DialogMode, DialogControl, BoolValue, 1);
		}
	}

	if (CheckDialogControl(DialogMode, IDD_GEOM_START1))
	{
		if ((res = GetDialogText(Dialog, DialogMode, IDD_GEOM_START1)) == 2)
			strcpy((LPSTR) TempGeom->Start1, DialogTextLine);
		else
			TempGeom->Start1[0] = 0;
	}
}

//************************************************************************************************************************************
//* IDD_DIALOG_BGA * IDD_DIALOG_DIL * IDD_DIALOG_PGA * IDD_DIALOG_QUAD * IDD_DIALOG_SIL * IDD_DIALOG_SIL_SMD_CIRCLE ******************
//************************************************************************************************************************************
//* IDD_DIALOG_SIL_SMD_RECT * IDD_DIALOG_SOIC ****************************************************************************************
//************************************************************************************************************************************

int32 CALLBACK CreateGeomDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 OldUnits;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		switch (DialogMode)
		{
		case IDD_DIALOG_BGA:
			SetWindowTextUTF8(Dialog, SC(71, "Create BGA"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC4, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(57, "Pin A1"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_SQUARE, SC(59, "Square"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_CIRCLE, SC(60, "Circle"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(68, "Use default rules for solder paste/mask and clearance"));
			break;

		case IDD_DIALOG_DIL: 
			SetWindowTextUTF8(Dialog, SC(72, "Create DIP (DIL) THT"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(62, "Pin 1"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(63, "Distance"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(62, "Pin 1"));
			SetDialogItemText(Dialog, IDC_STATIC4, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(65, "Drilling diameter"));
			SetDialogItemText(Dialog, IDC_STATIC10, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDC_STATIC11, SC(16, "Anti power pad"));
			SetDialogItemText(Dialog, IDC_STATIC12, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC13, SC(17, "Inner pad"));
			SetDialogItemText(Dialog, IDC_STATIC14, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_SQUARE, SC(59, "Square"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_CIRCLE, SC(60, "Circle"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(67, "Use default rules for solder paste/mask, inner/anti power pad and clearance"));
			break;

		case IDD_DIALOG_PGA:
			SetWindowTextUTF8(Dialog, SC(69, "Create PGA"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC4, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(65, "Drilling diameter"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(16, "Anti power pad"));
			SetDialogItemText(Dialog, IDC_STATIC10, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC11, SC(17, "Inner pad"));
			SetDialogItemText(Dialog, IDC_STATIC12, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC13, SC(57, "Pin A1"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_SQUARE, SC(59, "Square"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_CIRCLE, SC(60, "Circle"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(67, "Use default rules for solder paste/mask, inner/anti power pad and clearance"));
			break;

		case IDD_DIALOG_QUAD:
			SetWindowTextUTF8(Dialog, SC(70, "Create QFP"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(52, "Starting pin nr"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(63, "Distance"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC4, SC(63, "Distance"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC10, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC11, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC12, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC13, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC14, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDC_STATIC15, SC(54, "Start pin name"));
			SetDialogItemText(Dialog, IDC_STATIC16, SC(61, "Pin name"));
			SetDialogItemText(Dialog, IDC_STATIC17, SC(77, "Pin names"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(68, "Use default rules for solder paste/mask and clearance"));
			break;

		case IDD_DIALOG_SIL:  
		case IDD_DIALOG_SIL2: 
			SetWindowTextUTF8(Dialog, SC(73, "Create SIP (SIL) THT square/circle"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(54, "Start pin name"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(50, "Pad type"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(77, "Pin names"));
			SetDialogItemText(Dialog, IDC_STATIC4, SC(52, "Starting pin nr"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(78, "Increment"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(80, "Pad size"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC10, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC11, SC(65, "Drilling diameter"));
			SetDialogItemText(Dialog, IDC_STATIC12, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDC_STATIC13, SC(16, "Anti power pad"));
			SetDialogItemText(Dialog, IDC_STATIC14, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC15, SC(17, "Inner pad"));
			SetDialogItemText(Dialog, IDC_STATIC16, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC17, SC(61, "Pin name"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_SQUARE, SC(59, "Square"));
			SetDialogItemText(Dialog, IDD_GEOM_PIN1_CIRCLE, SC(60, "Circle"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(67, "Use default rules for solder paste/mask, inner/anti power pad and clearance"));
			break;

		case IDD_DIALOG_SIL_SMD_RECT:
			SetWindowTextUTF8(Dialog, SC(75, "Create SIP (SIL) SMD Rectangle pad"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(54, "Start pin name"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC4, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(52, "Starting pin nr"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(78, "Increment"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC10, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC11, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC12, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC13, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDC_STATIC14, SC(79, "Layer"));
			SetDialogItemText(Dialog, IDC_STATIC15, SC(77, "Pin names"));
			SetDialogItemText(Dialog, IDC_STATIC16, SC(61, "Pin name"));
			SetDialogItemText(Dialog, IDD_GEOM_TOP_LAYER, SC(25, "Top"));
			SetDialogItemText(Dialog, IDD_GEOM_BOTTOM_LAYER, SC(26, "Bottom"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(68, "Use default rules for solder paste/mask and clearance"));
			break;

		case IDD_DIALOG_SIL_SMD_CIRCLE:
			SetWindowTextUTF8(Dialog, SC(76, "Create SIP (SIL) SMD Circle pad"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(54, "Start pin name"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(66, "Diameter pad"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC4, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(61, "Pin name"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(78, "Increment"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC10, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDC_STATIC11, SC(79, "Layer"));
			SetDialogItemText(Dialog, IDC_STATIC12, SC(77, "Pin names"));
			SetDialogItemText(Dialog, IDC_STATIC13, SC(52, "Starting pin nr"));
			SetDialogItemText(Dialog, IDD_GEOM_TOP_LAYER, SC(25, "Top"));
			SetDialogItemText(Dialog, IDD_GEOM_BOTTOM_LAYER, SC(26, "Bottom"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(68, "Use default rules for solder paste/mask and clearance"));
			break;

		case IDD_DIALOG_SOIC:
			SetWindowTextUTF8(Dialog, SC(74, "Create SOIC"));
			SetDialogItemText(Dialog, IDC_STATIC1, SC(62, "Pin 1"));
			SetDialogItemText(Dialog, IDC_STATIC2, SC(63, "Distance"));
			SetDialogItemText(Dialog, IDC_STATIC3, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC5, SC(53, "Pitch"));
			SetDialogItemText(Dialog, IDC_STATIC6, SC(64, "Nr pins"));
			SetDialogItemText(Dialog, IDC_STATIC7, SC(51, "Pad"));
			SetDialogItemText(Dialog, IDC_STATIC8, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC9, SC(56, "Solder paste"));
			SetDialogItemText(Dialog, IDC_STATIC10, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC11, SC(55, "Solder mask"));
			SetDialogItemText(Dialog, IDC_STATIC12, SC(58, "Clearance"));
			SetDialogItemText(Dialog, IDOK, "OK");
			SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
			SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
			SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
			SetDialogItemText(Dialog, IDD_GEOM_USE_DEFAULT_RULES,
			                  SC(68, "Use default rules for solder paste/mask and clearance"));
			break;
		}

		InitDialogValues(Dialog, DialogMode);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case ID_UNITS:
			OldUnits = Units;

			if (++Units == 2)
				Units = 0;

			SetUnitsCreateGeom(Dialog, DialogMode, OldUnits, Units);
			break;

		case IDHELP:
			switch (DialogMode)
			{
			case IDD_DIALOG_BGA:
				Help("new_BGA_geometry.htm", 0);
				break;

			case IDD_DIALOG_DIL:
				Help("new_DIP_geometry.htm", 0);
				break;

			case IDD_DIALOG_PGA:
				Help("new_PGA_geometry.htm", 0);
				break;

			case IDD_DIALOG_QUAD:
				Help("new_QUAD_flatpack_geometry.htm", 0);
				break;

			case IDD_DIALOG_SOIC:
				Help("new_SOIC_geometry.htm", 0);
				break;

			case IDD_DIALOG_SIL:
				Help("add_through_hole_pads.htm", 0);
				break;

			case IDD_DIALOG_SIL_SMD_RECT:
				Help("add_rectangle_SMD_pads.htm", 0);
				break;

			case IDD_DIALOG_SIL_SMD_CIRCLE:
				Help("add_circle_SMD_pads.htm", 0);
				break;
			}

			break;

		case IDD_GEOM_USE_DEFAULT_RULES:
			SetDialogRuleValues(Dialog, DialogMode);
			break;

		case IDOK:
			GetDialogValues(Dialog, DialogMode, Units, 0);
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

int32 CreateGeomDialog(int32 Mode)
{
	int res, ok;

	res = 0;
	DialogMode = Mode;

	switch (Mode)
	{
	case IDD_DIALOG_BGA:
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_BGA), GEOMWindow, (DLGPROC) CreateGeomDialog2);
		break;

	case IDD_DIALOG_DIL:
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_DIL), GEOMWindow, (DLGPROC) CreateGeomDialog2);
		break;

	case IDD_DIALOG_PGA:
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PGA), GEOMWindow, (DLGPROC) CreateGeomDialog2);
		break;

	case IDD_DIALOG_QUAD:
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_QUAD), GEOMWindow, (DLGPROC) CreateGeomDialog2);
		break;

	case IDD_DIALOG_SIL:
	case IDD_DIALOG_SIL2:
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SIL), GEOMWindow, (DLGPROC) CreateGeomDialog2);
		break;

	case IDD_DIALOG_SIL_SMD_RECT:
		res =
		    DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SIL_SMD_RECT), GEOMWindow,
		              (DLGPROC) CreateGeomDialog2);
		break;

	case IDD_DIALOG_SIL_SMD_CIRCLE:
		res =
		    DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SIL_SMD_CIRCLE), GEOMWindow,
		              (DLGPROC) CreateGeomDialog2);
		break;

	case IDD_DIALOG_SOIC:
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SOIC), GEOMWindow, (DLGPROC) CreateGeomDialog2);
		break;
	}

	ok = 1;
	return res;
}

//**********************************************************************************************************************************
//****************************** IDD_DIALOG_VIEW ***********************************************************************************
//**********************************************************************************************************************************

int32 CALLBACK ViewObjectsDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, Layer, NrLayersSelected, LayersSelected[32], cnt;
	char TextStr[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
		{
			GetLayerText(Layer, TextStr, 0);
			res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) TextStr);

			if (PadsVisible[Layer] != 0)
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, res);

			res = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) TextStr);

			if (RoutingKeepoutVisible[Layer] != 0)
				res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETSEL, 1, res);
		}

		SendDlgItemMessage(Dialog, IDD_VIEW_DRILLS, BM_SETCHECK, DrillVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_DRILL_UNPLATED, BM_SETCHECK, DrillUnplatedVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_MASK_BOTTOM, BM_SETCHECK, SoldMaskPadsBottomVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_MASK_TOP, BM_SETCHECK, SoldMaskPadsTopVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_BOTTOM, BM_SETCHECK, PastePadsBottomVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_TOP, BM_SETCHECK, PastePadsTopVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_PLACEM_OUTLINE, BM_SETCHECK, PlacementVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_COMP_OUTLINE, BM_SETCHECK, CompOutlineVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_TOP, BM_SETCHECK, SilkScreenTopVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_BOTTOM, BM_SETCHECK, SilkScreenBottomVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_POWERPADS, BM_SETCHECK, PowerPadsVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_PINNAMES, BM_SETCHECK, PinNamesVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_CLEARANCE, BM_SETCHECK, ClearanceVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_GEOMNAME, BM_SETCHECK, GeomNameVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_BOARD_OUTLINE, BM_SETCHECK, BoardOutlineVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_INFO1, BM_SETCHECK, Info1Visible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_INFO2, BM_SETCHECK, Info2Visible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_INFO3, BM_SETCHECK, Info3Visible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_INFO4, BM_SETCHECK, Info4Visible, 0);
		SendDlgItemMessage(Dialog, ID_VIEW_INSERTPOINT, BM_SETCHECK, ViewInsertionPoint, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_GRID, BM_SETCHECK, GridVisible, 0);
		SendDlgItemMessage(Dialog, IDD_VIEW_CROSSHAIR, BM_SETCHECK, CrossHairVisible, 0);

		if (NrPadLayers == 2)
			SendDlgItemMessage(Dialog, IDD_VIEW_INNERPADS, BM_SETCHECK, InnerPadsVisible, 0);

		SetWindowTextUTF8(Dialog, SC(94, "Select visible layers/objects"));
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		SetDialogItemText(Dialog, IDC_STATIC1, SC(142, "Top layer"));
		SetDialogItemText(Dialog, IDC_STATIC2, SC(143, "Bottom layer"));
		SetDialogItemText(Dialog, IDC_STATIC3, SC(106, "Other objects"));
		SetDialogItemText(Dialog, IDC_STATIC4, SC(107, "Pad/trace layers"));
		SetDialogItemText(Dialog, IDC_STATIC5, SC(108, "Routing keepout layers"));
		SetDialogItemText(Dialog, ID_UNSELECTALL, SC(109, "Unselect all"));
		SetDialogItemText(Dialog, ID_SELECTALL, SC(110, "Select all"));
		SetDialogItemText(Dialog, ID_DEFAULT, SC(111, "Default"));

		SetDialogItemText(Dialog, IDD_VIEW_DRILLS, SC(14, "Drill plated"));
		SetDialogItemText(Dialog, IDD_VIEW_DRILL_UNPLATED, SC(15, "Drill unplated"));
		SetDialogItemText(Dialog, IDD_VIEW_MASK_BOTTOM, SC(55, "Solder mask"));
		SetDialogItemText(Dialog, IDD_VIEW_MASK_TOP, SC(55, "Solder mask"));
		SetDialogItemText(Dialog, IDD_VIEW_PASTE_BOTTOM, SC(56, "Paste mask"));
		SetDialogItemText(Dialog, IDD_VIEW_PASTE_TOP, SC(56, "Paste mask"));
		SetDialogItemText(Dialog, IDD_VIEW_PLACEM_OUTLINE, SC(13, "Placement outline"));
		SetDialogItemText(Dialog, IDD_VIEW_COMP_OUTLINE, SC(11, "Component outline"));
		SetDialogItemText(Dialog, IDD_VIEW_SILKSCREEN_TOP, SC(95, "Silkscreen"));
		SetDialogItemText(Dialog, IDD_VIEW_SILKSCREEN_BOTTOM, SC(95, "Silkscreen"));
		SetDialogItemText(Dialog, IDD_VIEW_POWERPADS, SC(16, "Anti power pad"));
		SetDialogItemText(Dialog, IDD_VIEW_PINNAMES, SC(140, "Pin names"));
		SetDialogItemText(Dialog, IDD_VIEW_CLEARANCE, SC(58, "Clearance"));
		SetDialogItemText(Dialog, IDD_VIEW_GEOMNAME, SC(97, "Geometry name"));
		SetDialogItemText(Dialog, IDD_VIEW_BOARD_OUTLINE, SC(12, "Board outline"));
		SetDialogItemText(Dialog, IDD_VIEW_INFO1, "Info 1");
		SetDialogItemText(Dialog, IDD_VIEW_INFO2, "Info 2");
		SetDialogItemText(Dialog, IDD_VIEW_INFO3, "Info 3");
		SetDialogItemText(Dialog, IDD_VIEW_INFO4, "Info 4");
		SetDialogItemText(Dialog, ID_VIEW_INSERTPOINT, SC(102, "Insertion point"));
		SetDialogItemText(Dialog, IDD_VIEW_GRID, SC(103, "Grid"));
		SetDialogItemText(Dialog, IDD_VIEW_CROSSHAIR, SC(101, "Crosshair"));

		if (NrPadLayers == 2)
			SetDialogItemText(Dialog, IDD_VIEW_INNERPADS, SC(17, "Inner pad"));

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			NrLayersSelected =
			    (int32) SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, NrPadLayers,
			                               (LPARAM) ((LPINT) & LayersSelected));

			for (cnt = 0; cnt < NrPadLayers; cnt++)
				PadsVisible[cnt] = 0;

			for (cnt = 0; cnt < NrLayersSelected; cnt++)
			{
				Layer = NrPadLayers - LayersSelected[cnt] - 1;
				PadsVisible[Layer] = 1;
			}

			NrLayersSelected =
			    (int32) SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETSELITEMS, NrPadLayers,
			                               (LPARAM) ((LPINT) & LayersSelected));

			for (cnt = 0; cnt < NrPadLayers; cnt++)
				RoutingKeepoutVisible[cnt] = 0;

			for (cnt = 0; cnt < NrLayersSelected; cnt++)
			{
				Layer = NrPadLayers - LayersSelected[cnt] - 1;
				RoutingKeepoutVisible[Layer] = 1;
			}

			DrillVisible = SendDlgItemMessage(Dialog, IDD_VIEW_DRILLS, BM_GETCHECK, 0, 0);
			DrillUnplatedVisible = SendDlgItemMessage(Dialog, IDD_VIEW_DRILL_UNPLATED, BM_GETCHECK, 0, 0);
			SoldMaskPadsBottomVisible = SendDlgItemMessage(Dialog, IDD_VIEW_MASK_BOTTOM, BM_GETCHECK, 0, 0);
			SoldMaskPadsTopVisible = SendDlgItemMessage(Dialog, IDD_VIEW_MASK_TOP, BM_GETCHECK, 0, 0);
			PastePadsBottomVisible = SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_BOTTOM, BM_GETCHECK, 0, 0);
			PastePadsTopVisible = SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_TOP, BM_GETCHECK, 0, 0);
			PlacementVisible = SendDlgItemMessage(Dialog, IDD_VIEW_PLACEM_OUTLINE, BM_GETCHECK, 0, 0);
			CompOutlineVisible = SendDlgItemMessage(Dialog, IDD_VIEW_COMP_OUTLINE, BM_GETCHECK, 0, 0);
			SilkScreenTopVisible = SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_TOP, BM_GETCHECK, 0, 0);
			SilkScreenBottomVisible = SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_BOTTOM, BM_GETCHECK, 0, 0);
			PowerPadsVisible = SendDlgItemMessage(Dialog, IDD_VIEW_POWERPADS, BM_GETCHECK, 0, 0);
			PinNamesVisible = SendDlgItemMessage(Dialog, IDD_VIEW_PINNAMES, BM_GETCHECK, 0, 0);

			if (NrPadLayers == 2)
				InnerPadsVisible = SendDlgItemMessage(Dialog, IDD_VIEW_INNERPADS, BM_GETCHECK, 0, 0);

			ClearanceVisible = SendDlgItemMessage(Dialog, IDD_VIEW_CLEARANCE, BM_GETCHECK, 0, 0);
			GeomNameVisible = SendDlgItemMessage(Dialog, IDD_VIEW_GEOMNAME, BM_GETCHECK, 0, 0);
			BoardOutlineVisible = SendDlgItemMessage(Dialog, IDD_VIEW_BOARD_OUTLINE, BM_GETCHECK, 0, 0);
			Info1Visible = SendDlgItemMessage(Dialog, IDD_VIEW_INFO1, BM_GETCHECK, 0, 0);
			Info2Visible = SendDlgItemMessage(Dialog, IDD_VIEW_INFO2, BM_GETCHECK, 0, 0);
			Info3Visible = SendDlgItemMessage(Dialog, IDD_VIEW_INFO3, BM_GETCHECK, 0, 0);
			Info4Visible = SendDlgItemMessage(Dialog, IDD_VIEW_INFO4, BM_GETCHECK, 0, 0);
			ViewInsertionPoint = SendDlgItemMessage(Dialog, ID_VIEW_INSERTPOINT, BM_GETCHECK, 0, 0);
			GridVisible = SendDlgItemMessage(Dialog, IDD_VIEW_GRID, BM_GETCHECK, 0, 0);
			CrossHairVisible = SendDlgItemMessage(Dialog, IDD_VIEW_CROSSHAIR, BM_GETCHECK, 0, 0);

			for (cnt = 0; cnt < NrPadLayers; cnt++)
			{
				if (PadsVisible[cnt] == 0)
					ChangeSelections(ID_UNSELECT_PADS + cnt);

				if (RoutingKeepoutVisible[cnt] == 0)
					ChangeSelections(ID_UNSELECT_ROUT_KEEPOUT + cnt);
			}

			if (NrPadLayers == 2)
				
				if (!InnerPadsVisible)
					ChangeSelections(ID_UNSELECT_INNER_PADS);
			
			if (!DrillVisible)
				ChangeSelections(ID_UNSELECT_DRILL);

			if (!DrillUnplatedVisible)
				ChangeSelections(ID_UNSELECT_DRILL_UNPLATED);

			if (!SoldMaskPadsBottomVisible)
				ChangeSelections(SOLD_MASK_BOTTOM_LAYER);

			if (!SoldMaskPadsTopVisible)
				ChangeSelections(ID_UNSELECT_MASK_TOP);

			if (!PastePadsBottomVisible)
				ChangeSelections(ID_UNSELECT_PASTE_BOTTOM);

			if (!PastePadsTopVisible)
				ChangeSelections(ID_UNSELECT_PASTE_TOP);

			if (!PlacementVisible)
				ChangeSelections(ID_UNSELECT_PLACEM_OUTLINE);

			if (!CompOutlineVisible)
				ChangeSelections(ID_UNSELECT_COMP_OUTLINE);

			if (!SilkScreenTopVisible)
				ChangeSelections(ID_UNSELECT_SILK_TOP);

			if (!SilkScreenBottomVisible)
				ChangeSelections(ID_UNSELECT_SILK_BOTTOM);

			if (!BoardOutlineVisible)
				ChangeSelections(ID_UNSELECT_BOARD_OUTLINE);

			if (!Info1Visible)
				ChangeSelections(ID_UNSELECT_INFO1);

			if (!Info2Visible)
				ChangeSelections(ID_UNSELECT_INFO2);

			if (!Info3Visible)
				ChangeSelections(ID_UNSELECT_INFO3);

			if (!Info4Visible)
				ChangeSelections(ID_UNSELECT_INFO4);

			if (!PowerPadsVisible)
				ChangeSelections(ID_UNSELECT_ANTI_POWERPADS);

			EndDialog(Dialog, 1);
			return about;

		case ID_UNSELECTALL:
			for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
			{
				PadsVisible[Layer] = 0;
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 0, Layer);
				RoutingKeepoutVisible[Layer] = 0;
				res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETSEL, 0, Layer);
			}

			SendDlgItemMessage(Dialog, IDD_VIEW_DRILLS, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_DRILL_UNPLATED, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_MASK_BOTTOM, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_MASK_TOP, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_BOTTOM, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_TOP, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PLACEM_OUTLINE, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_COMP_OUTLINE, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_TOP, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_BOTTOM, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_POWERPADS, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PINNAMES, BM_SETCHECK, 0, 0);

			if (NrPadLayers == 2)
				SendDlgItemMessage(Dialog, IDD_VIEW_INNERPADS, BM_SETCHECK, 0, 0);

			SendDlgItemMessage(Dialog, IDD_VIEW_CLEARANCE, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_GEOMNAME, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_BOARD_OUTLINE, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO1, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO2, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO3, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO4, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, ID_VIEW_INSERTPOINT, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_GRID, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_CROSSHAIR, BM_SETCHECK, 0, 0);
			break;

		case ID_SELECTALL:
			for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
			{
				PadsVisible[Layer] = 1;
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, Layer);
				RoutingKeepoutVisible[Layer] = 1;
				res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETSEL, 1, Layer);
			}

			SendDlgItemMessage(Dialog, IDD_VIEW_DRILLS, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_DRILL_UNPLATED, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_MASK_BOTTOM, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_MASK_TOP, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_BOTTOM, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_TOP, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PLACEM_OUTLINE, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_COMP_OUTLINE, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_TOP, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_BOTTOM, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_POWERPADS, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PINNAMES, BM_SETCHECK, 1, 0);

			if (NrPadLayers == 2)
				SendDlgItemMessage(Dialog, IDD_VIEW_INNERPADS, BM_SETCHECK, 1, 0);

			SendDlgItemMessage(Dialog, IDD_VIEW_CLEARANCE, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_GEOMNAME, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_BOARD_OUTLINE, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO1, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO2, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO3, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO4, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, ID_VIEW_INSERTPOINT, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_GRID, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_CROSSHAIR, BM_SETCHECK, 1, 0);
			break;

		case ID_DEFAULT:
			for (Layer = NrPadLayers - 1; Layer >= 0; Layer--)
			{
				PadsVisible[Layer] = 1;
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETSEL, 1, Layer);
				RoutingKeepoutVisible[Layer] = 1;
				res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETSEL, 1, Layer);
			}

			SendDlgItemMessage(Dialog, IDD_VIEW_DRILLS, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_DRILL_UNPLATED, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_MASK_TOP, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_MASK_BOTTOM, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_TOP, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PASTE_BOTTOM, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PLACEM_OUTLINE, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_COMP_OUTLINE, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_TOP, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_SILKSCREEN_BOTTOM, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_POWERPADS, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_PINNAMES, BM_SETCHECK, 0, 0);

			if (NrPadLayers == 2)
				SendDlgItemMessage(Dialog, IDD_VIEW_INNERPADS, BM_SETCHECK, 0, 0);

			SendDlgItemMessage(Dialog, IDD_VIEW_CLEARANCE, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_GEOMNAME, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_BOARD_OUTLINE, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO1, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO2, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO3, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_INFO4, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, ID_VIEW_INSERTPOINT, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_GRID, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDD_VIEW_CROSSHAIR, BM_SETCHECK, 0, 0);
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

int32 ViewObjectsDialog(int32 Mode)
{
	int res, ok;

	DialogMode = Mode;
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_VIEW), GEOMWindow, (DLGPROC) ViewObjectsDialog2);
	ok = 1;
	return res;
}

//**********************************************************************************************************************************
//************************* IDD_DIALOG_PINASSIGN ***********************************************************************************
//**********************************************************************************************************************************

int32 CALLBACK AssignPinDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, cnt;
	PinInfoRecord *PinInfo;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		for (cnt = 0; cnt < NrPinObjects; cnt++)
		{
			PinInfo = &((*PinInfos)[cnt]);
			SendDlgItemMessageUTF8(Dialog, IDD_PINASSIGN_COMBO1, CB_ADDSTRING, 0, (LPARAM) (LPSTR) PinInfo->PinText);
		}

		SetWindowTextUTF8(Dialog, SC(41, "Assign/Remove pins to objects"));
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
		SetDialogItemText(Dialog, IDC_STATIC1, SC(112, "Used pins"));
		SetDialogItemText(Dialog, IDC_STATIC2, SC(113, "Enter pin designation"));

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res = SendDlgItemMessage(Dialog, IDD_PINASSIGN_COMBO1, CB_GETCURSEL, 0, 0);

			if (res == -1)
			{
				res =
				    SendDlgItemMessageUTF8(Dialog, IDD_PINASSIGN_NEWPIN, WM_GETTEXT, MAX_LENGTH_STRING - 1,
				                           (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			}
			else
			{
				res = SendDlgItemMessageUTF8(Dialog, IDD_PINASSIGN_COMBO1, CB_GETLBTEXT, res, (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("assign_objects_to_pin.htm", 0);
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

int32 AssignPinDialog(ObjectRecord * ObjectText, int32 Mode)
{
	int res, ok, cnt, num;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	DialogMode = Mode;

	if ((DialogMode == 0) || (OldPinStr[0] == 0))
	{
		memset(&ObjectText->Text, 0, sizeof(ObjectText->Text));
		res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PINASSIGN), GEOMWindow,
		              (DLGPROC) AssignPinDialog2);

		if (res == 1)
		{
			memmove(&ObjectText->Text, &DialogTextLine, 11);
			strcpy(OldPinStr, DialogTextLine);
			return res;
		}
	}
	else
	{
		cnt = strlen(OldPinStr) - 1;

		while ((cnt >= 0) && (isdigit(OldPinStr[cnt])))
			cnt--;

		cnt++;

		if (cnt < (int) strlen(OldPinStr))
		{
			strcpy(str2, OldPinStr);
			str2[cnt] = 0;
			sscanf((LPSTR) & OldPinStr[cnt], "%i", &num);
			sprintf(str, "%s%i", str2, num + 1);
		}
		else
			sprintf(str, "%s1", OldPinStr);

		strcpy(OldPinStr, str);
		strcpy(ObjectText->Text, str);
		return 1;
	}

	ok = 1;
	return res;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void FillValuesDialog(HWND Dialog, int32 Units)
{
	char str[MAX_LENGTH_STRING];
	int32 cnt;

	if (Units == 0)
	{
		SendDlgItemMessage(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		SendDlgItemMessage(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		SendDlgItemMessage(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		SendDlgItemMessage(Dialog, IDC_EDIT8, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		SendDlgItemMessage(Dialog, IDC_EDIT10, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		SendDlgItemMessage(Dialog, IDC_EDIT12, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
	}
	else
	{
		SendDlgItemMessage(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
		SendDlgItemMessage(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
		SendDlgItemMessage(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
		SendDlgItemMessage(Dialog, IDC_EDIT8, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
		SendDlgItemMessage(Dialog, IDC_EDIT10, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
		SendDlgItemMessage(Dialog, IDC_EDIT12, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
	}

	SendDlgItemMessage(Dialog, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(Dialog, IDC_COMBO2, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(Dialog, IDC_COMBO3, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(Dialog, IDC_COMBO4, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(Dialog, IDC_COMBO5, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(Dialog, IDC_COMBO6, CB_RESETCONTENT, 0, 0);

	for (cnt = 0; cnt < NrTraceWidths; cnt++)
	{
		if (Units == 0)
			sprintf(str, "%.1f thou", TraceWidths[cnt] / 2540.0);
		else
			sprintf(str, "%.4f mm", TraceWidths[cnt] / 100000.0);

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) str);
	}

	if (Units == 0)
		sprintf(str, "%.1f", TraceThickness / 2540.0);
	else
		sprintf(str, "%.4f", TraceThickness / 100000.0);

	SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str);


	for (cnt = 0; cnt < NrTraceClearances; cnt++)
	{
		if (Units == 0)
			sprintf(str, "%.1f thou", TraceClearances[cnt] / 2540.0);
		else
			sprintf(str, "%.4f mm", TraceClearances[cnt] / 100000.0);

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM) str);
	}

	if (Units == 0)
		sprintf(str, "%.1f", CurrentClearance / 2540.0);
	else
		sprintf(str, "%.4f", CurrentClearance / 100000.0);

	SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) str);


	for (cnt = 0; cnt < NrCompOutLines; cnt++)
	{
		if (Units == 0)
			sprintf(str, "%.1f thou", CompOutLines[cnt] / 2540.0);
		else
			sprintf(str, "%.4f mm", CompOutLines[cnt] / 100000.0);

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO3, CB_ADDSTRING, 0, (LPARAM) str);
	}

	if (Units == 0)
		sprintf(str, "%.1f", CurrentCompOutLine / 2540.0);
	else
		sprintf(str, "%.4f", CurrentCompOutLine / 100000.0);

	SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) str);

	for (cnt = 0; cnt < NrSilkscreenLines; cnt++)
	{
		if (Units == 0)
			sprintf(str, "%.1f thou", SilkscreenLines[cnt] / 2540.0);
		else
			sprintf(str, "%.4f mm", SilkscreenLines[cnt] / 100000.0);

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO4, CB_ADDSTRING, 0, (LPARAM) str);
	}

	if (Units == 0)
		sprintf(str, "%.1f", CurrentSilkscreenLine / 2540.0);
	else
		sprintf(str, "%.4f", CurrentSilkscreenLine / 100000.0);

	SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) str);

	for (cnt = 0; cnt < NrInfoLines; cnt++)
	{
		if (Units == 0)
			sprintf(str, "%.1f thou", InfoLines[cnt] / 2540.0);
		else
			sprintf(str, "%.4f mm", InfoLines[cnt] / 100000.0);

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO5, CB_ADDSTRING, 0, (LPARAM) str);
	}

	if (Units == 0)
		sprintf(str, "%.1f", CurrentInfoLine / 2540.0);
	else
		sprintf(str, "%.4f", CurrentInfoLine / 100000.0);

	SendDlgItemMessageUTF8(Dialog, IDC_EDIT9, WM_SETTEXT, 0, (LPARAM) str);

	for (cnt = 0; cnt < NrBoardOutLines; cnt++)
	{
		if (Units == 0)
			sprintf(str, "%.1f thou", BoardOutLines[cnt] / 2540.0);
		else
			sprintf(str, "%.4f mm", BoardOutLines[cnt] / 100000.0);

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO6, CB_ADDSTRING, 0, (LPARAM) str);
	}

	if (Units == 0)
		sprintf(str, "%.1f", CurrentBoardOutLine / 2540.0);
	else
		sprintf(str, "%.4f", CurrentBoardOutLine / 100000.0);

	SendDlgItemMessageUTF8(Dialog, IDC_EDIT11, WM_SETTEXT, 0, (LPARAM) str);

}

//**********************************************************************************************************************************
//******************** IDD_DIALOG_VALUES *******************************************************************************************
//**********************************************************************************************************************************

int32 CALLBACK AssignValuesDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res;
	float Value;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(117, "Rules of geometry"));
		FillValuesDialog(Dialog, Units);
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
		SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
		SetDialogItemText(Dialog, IDC_STATIC1, SC(118, "Trace width"));
		SetDialogItemText(Dialog, IDC_STATIC2, SC(119, "Clearance width"));
		SetDialogItemText(Dialog, IDC_STATIC3, SC(120, "Line width component outline"));
		SetDialogItemText(Dialog, IDC_STATIC4, SC(121, "Line width silkscreen"));
		SetDialogItemText(Dialog, IDC_STATIC5, SC(122, "Line width info layers"));
		SetDialogItemText(Dialog, IDC_STATIC6, SC(123, "Line width board outline"));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
// *******************************************************************************
			res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

			if (res == -1)
			{
				res =
				    SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1,
				                           (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);

				if (sscanf(DialogTextLine, "%f", &Value) == 1)
				{
					if (Units == 0)
						TraceThickness = Value * 2540.0;
					else
						TraceThickness = Value * 100000.0;
				}
			}
			else
				TraceThickness = TraceWidths[res];

// *******************************************************************************
			res = SendDlgItemMessage(Dialog, IDC_COMBO2, CB_GETCURSEL, 0, 0);

			if (res == -1)
			{
				res =
				    SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, MAX_LENGTH_STRING - 1,
				                           (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);

				if (sscanf(DialogTextLine, "%f", &Value) == 1)
				{
					if (Units == 0)
						CurrentClearance = Value * 2540.0;
					else
						CurrentClearance = Value * 100000.0;
				}
			}
			else
				CurrentClearance = TraceClearances[res];

// *******************************************************************************
			res = SendDlgItemMessage(Dialog, IDC_COMBO3, CB_GETCURSEL, 0, 0);

			if (res == -1)
			{
				res =
				    SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_GETTEXT, MAX_LENGTH_STRING - 1,
				                           (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);

				if (sscanf(DialogTextLine, "%f", &Value) == 1)
				{
					if (Units == 0)
						CurrentCompOutLine = Value * 2540.0;
					else
						CurrentCompOutLine = Value * 100000.0;
				}
			}
			else
				CurrentCompOutLine = CompOutLines[res];

// *******************************************************************************
			res = SendDlgItemMessage(Dialog, IDC_COMBO4, CB_GETCURSEL, 0, 0);

			if (res == -1)
			{
				res =
				    SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_GETTEXT, MAX_LENGTH_STRING - 1,
				                           (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);

				if (sscanf(DialogTextLine, "%f", &Value) == 1)
				{
					if (Units == 0)
						CurrentSilkscreenLine = Value * 2540.0;
					else
						CurrentSilkscreenLine = Value * 100000.0;
				}
			}
			else
				CurrentSilkscreenLine = SilkscreenLines[res];

// *******************************************************************************
			res = SendDlgItemMessage(Dialog, IDC_COMBO5, CB_GETCURSEL, 0, 0);

			if (res == -1)
			{
				res =
				    SendDlgItemMessageUTF8(Dialog, IDC_EDIT9, WM_GETTEXT, MAX_LENGTH_STRING - 1,
				                           (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);

				if (sscanf(DialogTextLine, "%f", &Value) == 1)
				{
					if (Units == 0)
						CurrentInfoLine = Value * 2540.0;
					else
						CurrentInfoLine = Value * 100000.0;
				}
			}
			else
				CurrentInfoLine = InfoLines[res];

// *******************************************************************************
			res = SendDlgItemMessage(Dialog, IDC_COMBO6, CB_GETCURSEL, 0, 0);

			if (res == -1)
			{
				res =
				    SendDlgItemMessageUTF8(Dialog, IDC_EDIT11, WM_GETTEXT, MAX_LENGTH_STRING - 1,
				                           (LPARAM) DialogTextLine);

				if (res == 0)
					memset(DialogTextLine, 0, MAX_LENGTH_STRING);

				if (sscanf(DialogTextLine, "%f", &Value) == 1)
				{
					if (Units == 0)
						CurrentBoardOutLine = Value * 2540.0;
					else
						CurrentBoardOutLine = Value * 100000.0;
				}
			}
			else
				CurrentBoardOutLine = BoardOutLines[res];

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("thickness_line_clearance.htm", 0);
			return about;

		case ID_UNITS:
			if (++Units == 2)
				Units = 0;

			FillValuesDialog(Dialog, Units);
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

int32 AssignValuesDialog(int32 Mode)
{
	int res, ok;

	DialogMode = Mode;
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_VALUES), GEOMWindow, (DLGPROC) AssignValuesDialog2);
	ok = 1;
	return res;
}

//****************************************************************************************************************************
//*************************** IDD_DIALOG_NEW *********************************************************************************
//****************************************************************************************************************************

int32 CALLBACK SelectNewGeomtrieDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(24, "Select new geometry"));
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM)SC(1, "New geometry"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "DIP (DIL)");
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "PGA");
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "SOIC");
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "QFP");
		SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) "BGA");
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (res != LB_ERR)
				EndDialog(Dialog, res);
			else
				EndDialog(Dialog, -1);

			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;

		case IDHELP:
			Help("make_new_geometry.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 SelectNewGeomtrieDialog(int32 Mode)
{
	int res;

	DialogMode = Mode;
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NEW), GEOMWindow, (DLGPROC) SelectNewGeomtrieDialog2);
	return res;
}

//**********************************************************************************************************************************
//**************** IDD_DIALOG_GRID *************************************************************************************************
//**********************************************************************************************************************************

int32 CALLBACK GridDialogBody(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, GridChanged;
	double value1, NewGridSize;
	int32 res, res2, cnt;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogValue(Dialog, IDC_EDIT1, TempGridSize);

		if (Units == 0)
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		else
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");

		for (cnt = 0; cnt < NrGridSizes; cnt++)
		{
			value1 = GridSizes[cnt];

			if (Units == 0)
				sprintf(str, "%.1f", value1 / 2540);
			else
				sprintf(str, "%.4f", value1 / 100000);

			res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);

			if (InRange(GridSize, value1))
				res2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, res, 0);
		}

		SetWindowTextUTF8(Dialog, SC(103, "Grid"));
		SetDialogItemText(Dialog, ID_CHANGE_COLOR, SC(131, "Change color"));
		SetDialogItemText(Dialog, IDC_STATIC1, SC(132, "User grid"));
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			GridChanged = 0;
			res2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
			TempGridSize = GetDialogValue(Dialog, IDC_EDIT1);
			NewGridSize = 25400.0;

			if (TempGridSize == 0)
			{
				if (res2 != LB_ERR)
				{
					NewGridSize = GridSizes[res2];
					GridChanged = 1;
				}
			}
			else
			{
				NewGridSize = TempGridSize;
				GridChanged = 1;
			}

			if (GridChanged)
			{
				if (GridVisible)
				{
					StartDrawingEditingWindow();
					DrawGrid();
					ExitDrawing();
					EndDrawingEditingWindow();
				}

				GridSize = NewGridSize;

				if (GridVisible)
				{
					StartDrawingEditingWindow();
					DrawGrid();
					ExitDrawing();
					EndDrawingEditingWindow();
				}
			}

			EndDialog(Dialog, 1);
			return about;

		case ID_UNITS:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
			TempGridSize = GetDialogValue(Dialog, IDC_EDIT1);
			Units ^= 1;

			for (cnt = 0; cnt < NrGridSizes; cnt++)
			{
				value1 = GridSizes[cnt];

				if (Units == 0)
					sprintf(str, "%.1f", value1 / 2540);
				else
					sprintf(str, "%.4f", value1 / 100000);

				res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);

				if (InRange(GridSize, value1))
					res2 = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, res, 0);
			}

			SetDialogValue(Dialog, IDC_EDIT1, TempGridSize);

			if (Units == 0)
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			else
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");

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

int32 GridDialog(int32 mode)
{
	int32 res;

	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_GRID), GEOMWindow, (DLGPROC) GridDialogBody);
	return res;
}

//***************************************************************************************************************
//***************************************** O programu IDD_DIALOG_ABOUT *****************************************
//***************************************************************************************************************

int32 CALLBACK AboutDialogBody(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(127, "About program"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(124, "Geometry editor PCB elegance"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, "Web PCB Elegance"); //pøidán

		sprintf(str, SC(125, "\r\n Build version %i.%i.%i  ( %s )"), VER_VERSION / 100, VER_VERSION % 100, VER_BUILD, VER_DATE_STR);

#ifdef GCC_COMP
		strcat(str, "\r\n\r\n Compiled with mingw (gcc 4.9.2)");
#endif
#ifdef VC2005
		strcat(str, "\r\n\r\n Compiled with Microsoft Visual Studio 2005");
#endif
#ifdef VC2010
		strcat(str, SC(126, "\r\n\r\n Compiled with Microsoft Visual Studio 2019"));
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

	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ABOUT), GEOMWindow, (DLGPROC) AboutDialogBody);
	return res;
}

//**********************************************************************************************************************************
//******************************* IDD_DIALOG_COLOR *********************************************************************************
//**********************************************************************************************************************************

int32 CALLBACK ColorDialogBody(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, ok, Selection;
	COLORREF NewColor;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, BackGroundColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeSilkScreenTopColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeSilkScreenBottomColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeCompOutlineColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePlacementOutLineColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePinsDrillColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePinsDrillUnplatedColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePinsTopColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePinsBottomColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePinsInnerColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePasteMaskTopColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePasteMaskBottomColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeSoldMaskTopColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeSoldMaskBottomColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeRoutingKeepoutTopColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeRoutingKeepoutBottomColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeRoutingKeepoutInnerColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeBoardOutlineColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeInfo1ColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeInfo2ColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeInfo3ColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeInfo4ColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapeGeomNameColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ShapePowerPadColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ClearanceColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, ButtonInfoColorNr);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, GridColorNr);
		SelectedColorNr = -1;
		SetWindowTextUTF8(Dialog, SC(134, "Select colors"));
		SetDialogItemText(Dialog, ID_CHANGE_COLOR, SC(131, "Change color"));
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDHELP, SC(47, "Help"));
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
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
			              if ((res=SendDlgItemMessage(Dialog,IDD_COLOR_LIST1,LB_GETCURSEL,0,0))!=CB_ERR) {
			                NewColor=GetNewColor(0,GEOMColors[res],Dialog);
			                if (NewColor!=-1) {
			                  GEOMColors[res]=NewColor;
			                  DeleteGraphicObjects();
			                  KillDrawObjects();
			                  CreateDrawObjects();
			                  memset(&TempDrawItem,0,sizeof(DRAWITEMSTRUCT));
			                  TempDrawItem.CtlType=ODT_LISTBOX;
			                  TempDrawItem.CtlID=IDD_COLOR_LIST1;
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
				NewColor = GetNewColor(0, GEOMColors[res], Dialog);

				if (NewColor != -1)
				{
					GEOMColors[res] = NewColor;
					DeleteGraphicObjects();
					CreateDrawObjects();
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
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COLOR), GEOMWindow, (DLGPROC) ColorDialogBody);
	return res;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

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

//*********************************************************************************************************************************
//*********************** IDD_DIALOG_PAD_RULES ************************************************************************************
//*********************************************************************************************************************************

int32 CALLBACK DeafultPadRulesDialogBody(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	double value1, value2, value3, value4, value5, value6;
	/*
	double                       DefaultRuleSolderMask_TH,
	                            DefaultRulePasteMask_SMD,DefaultRuleSolderMask_SMD,
	                            DefaultRuleAntiPowerPad,DefaultRuleInnerPad,
	                            DefaultRuleClearance;
	*/
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(429, "Default pad rules"));
		SetDialogItemText(Dialog, IDOK, "OK");
		SetDialogItemText(Dialog, IDCANCEL, SC(46, "Cancel"));
		SetDialogItemText(Dialog, ID_UNITS, "thou/mm");
		SetDialogItemText(Dialog, IDC_STATIC1, SC(430, "Paste mask SMD"));
		SetDialogItemText(Dialog, IDC_STATIC2, SC(431, "Solder mask SMD"));
		SetDialogItemText(Dialog, IDC_STATIC3, SC(432, "Solder mask (against pad)"));
		SetDialogItemText(Dialog, IDC_STATIC4, SC(433, "Anti power pad (against drill size)"));
		SetDialogItemText(Dialog, IDC_STATIC5, SC(434, "Inner pad (against drill size)"));
		SetDialogItemText(Dialog, IDC_STATIC6, SC(435, "Pad (against drill size)"));
		SetDialogItemText(Dialog, IDC_STATIC7, "SMD");
		SetDialogItemText(Dialog, IDC_STATIC8, "THT");

		SetDialogValue(Dialog, IDC_EDIT1, DefaultRulePasteMask_SMD);
		SetDialogValue(Dialog, IDC_EDIT2, DefaultRuleSolderMask_SMD);
		SetDialogValue(Dialog, IDC_EDIT3, DefaultRuleSolderMask_TH);
		SetDialogValue(Dialog, IDC_EDIT4, DefaultRuleAntiPowerPad);
		SetDialogValue(Dialog, IDC_EDIT5, DefaultRuleInnerPad);
		SetDialogValue(Dialog, IDC_EDIT6, DefaultRulePad);

//      SetDialogValue(Dialog,IDC_EDIT6,DefaultRuleClearance);
		if (Units == 0)
		{
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN5, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN6, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
//        SendDlgItemMessage(Dialog,IDD_GEOM_UN6,WM_SETTEXT,0,(LPARAM)(LPSTR)"thou");
		}
		else
		{
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN5, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN6, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
//        SendDlgItemMessage(Dialog,IDD_GEOM_UN6,WM_SETTEXT,0,(LPARAM)(LPSTR)"mm");
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case ID_UNITS:
			value1 = GetDialogValue(Dialog, IDC_EDIT1);
			value2 = GetDialogValue(Dialog, IDC_EDIT2);
			value3 = GetDialogValue(Dialog, IDC_EDIT3);
			value4 = GetDialogValue(Dialog, IDC_EDIT4);
			value5 = GetDialogValue(Dialog, IDC_EDIT5);
			value6 = GetDialogValue(Dialog, IDC_EDIT6);
//          value6=GetDialogValue(Dialog,IDC_EDIT6);
			Units ^= 1;
			SetDialogValue(Dialog, IDC_EDIT1, value1);
			SetDialogValue(Dialog, IDC_EDIT2, value2);
			SetDialogValue(Dialog, IDC_EDIT3, value3);
			SetDialogValue(Dialog, IDC_EDIT4, value4);
			SetDialogValue(Dialog, IDC_EDIT5, value5);
			SetDialogValue(Dialog, IDC_EDIT6, value6);

//          SetDialogValue(Dialog,IDC_EDIT6,value6);
			if (Units == 0)
			{
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN5, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN6, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
//            SendDlgItemMessage(Dialog,IDD_GEOM_UN6,WM_SETTEXT,0,(LPARAM)(LPSTR)"thou");
			}
			else
			{
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN1, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN5, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SendDlgItemMessageUTF8(Dialog, IDD_GEOM_UN6, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
//            SendDlgItemMessage(Dialog,IDD_GEOM_UN6,WM_SETTEXT,0,(LPARAM)(LPSTR)"mm");
			}

			break;

		case IDOK:
			DefaultRulePasteMask_SMD = GetDialogValue(Dialog, IDC_EDIT1);
			DefaultRuleSolderMask_SMD = GetDialogValue(Dialog, IDC_EDIT2);
			DefaultRuleSolderMask_TH = GetDialogValue(Dialog, IDC_EDIT3);
			DefaultRuleAntiPowerPad = GetDialogValue(Dialog, IDC_EDIT4);
			DefaultRuleInnerPad = GetDialogValue(Dialog, IDC_EDIT5);
			DefaultRulePad = GetDialogValue(Dialog, IDC_EDIT6);
//          DefaultRuleClearance=GetDialogValue(Dialog,IDC_EDIT6);
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

int32 EditPadRules()
{
	int32 res;

	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PAD_RULES), GEOMWindow,
	              (DLGPROC) DeafultPadRulesDialogBody);

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
