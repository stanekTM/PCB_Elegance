/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: dialogs.h
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



#ifndef _DIALOGS

#define _DIALOGS


#include "types.h"

void SetDialogItemText(HWND Dialog, int32 DlgItem, LPSTR Text);

void SetDialogValue(HWND Dialog, int32 Control, double value);

void SetDialogFloatValue(HWND Dialog, int32 Control, double value, int32 Resolution);

void SetDialogIntValue(HWND Dialog, int32 Control, int32 value);

double GetDialogValue(HWND Dialog, int32 Control);

int32 GetDialogIntValue(HWND Dialog, int32 Control, int32 * Value);

int32 GetDialogFloatValue(HWND Dialog, int32 Control, double *Value);

int32 TextInputDialog(ObjectTextRecord * ObjectText, int32 Mode);

int32 PartNrDialog(int32 Mode);

int32 AddPinsDialog(PinRecord * Pin, int32 Mode);

int32 AddPowerPinsDialog(PowerPinRecord * PowerPin, int32 Mode);

int32 AddPinBusDialog(PinBusRecord * PinBus, int32 Mode);

int32 SubPinStringsDialog(int32 Mode);

int32 SelectSymbolDialog(void);

int32 InstanceInfoDialog(InstanceRecord * Instance, int32 NrInstances, int32 mode);

int32 AddNetlabelDialog(NetLabelRecord * NetLabel, int32 Mode);

int32 AddNetlabelsDialog(LPSTR TextLine, int32 * StartNr, int32 * Step);

void InitDialogs(void);

int32 MessageDialog(LPSTR Message);

int32 SymbolInfoDialog(SymbolRecord * Symbol);

int32 NumberInputDialog(void);

int32 AddWireLabelsDialog(void);

int32 AnnotateDialog(void);

int32 AboutDialog(void);

int32 ColorDialog(int32 mode);

int32 GatePinSwapDialog(int32 Mode);

int32 PinBusReorderDialog(RedefinedPinBusRecord * RedefinedPinBus, int32 mode);

int32 Registration(void);

int32 AddSymbolOnShortCut(int32 mode);

int32 SelectComponentDialog(int32 mode);

int32 EditSheetInfo(int32 mode);

int32 EditSymbolSheetInfo(int32 mode);

int32 ComponentSelectionDialog(int32 mode);

int32 FindReplaceDialog(int32 Mode);

#endif
