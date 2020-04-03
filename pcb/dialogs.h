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

INT_PTR OwnDialogBox(HINSTANCE hInstance, LPCTSTR Resource, HWND Window, DLGPROC DialogBody);

INT_PTR OwnDialogBoxParam(HINSTANCE hInstance, LPCTSTR Resource, HWND Window, DLGPROC DialogBody, LPARAM LParam);

void SetDialogItemText(HWND Dialog, int32 DlgItem, LPSTR Text);

void SetDialogValue(HWND Dialog, int32 Control, double value);

void SetDialogValue2(HWND Dialog, int32 Control, double value);

void SetDialogIntValue(HWND Dialog, int32 Control, int32 value);

void SetDialogFloatValue(HWND Dialog, int32 Control, double value, int32 Resolution);

double GetDialogValue(HWND Dialog, int32 Control);

int32 GetDialogFloatValue(HWND Dialog, int32 Control, double *Value);

int32 GetDialogIntValue(HWND Dialog, int32 Control, int32 * Value);

int32 SetUnitText(HWND Dialog, int32 Control, int32 Units);

int32 SelectNetDialog(void);

void UnconnectedNetsDialog(void);

void HiliteNetsDialog(void);

void HideConnectionsNetsDialog(void);

void UnselectTracesViasNetDialog(void);

void DisableConnectionsNetsDialog(void);

void NetTypesDialog(int32 NetNr);

int32 ProtectComponentsDialog(void);

int32 TextInputDialog(ObjectTextRecord2 * ObjectText, int32 Mode);

int32 MessageDialog(LPSTR InfoLine, int32 Mode, int32 ObjectsSelected);

int32 CompRefDialog(ObjectTextRecord2 * ObjectText, LPSTR DialogText);

int32 SimpleLineInputDialog(ObjectTextRecord2 * ObjectText, LPSTR DialogText);

int32 LineInputDialog(ObjectTextRecord2 * ObjectText, LPSTR DialogText, int32 mode);

int32 LineInputDialogLong(LPSTR Text, LPSTR DialogText);

int32 AreaFillDialog(AreaFillRecord * AreaFill, int32 mode);

int32 NewDesignDialog(void);

int32 ValueDialog(int32 Mode);

int32 TextInputDialog2(LPSTR Text, LPSTR DialogText, int32 Mode);

int32 RotationDialog(double *RotationX, double *RotationY, double *Rotation, int32 Mode);

int32 ViewObjectsDialog(int32 Mode);

int32 SelectErrorDialog(void);

void DeleteTracesViasNetDialog(void);

int32 ChangeAreaFillDialog(AreaFillRecord * AreaFill);

int32 GerberDialog(int32 mode);

int32 ExportPDFDialog(int32 mode);

int32 PlotDialog(int32 mode);

int32 ExportBitmapDialog(int32 mode);

int32 GridDialog(int32 mode);

int32 AboutDialog(void);

int32 ComponentPlacementDialog(int32 Mode);

int32 Registration(void);

int32 InsertLayer(int32 mode);

int32 RemoveLayer(int32 mode);

int32 SwitchLayer(int32 mode);

int32 SelectLayer(int32 mode);

int32 SelectGeometrie(LPSTR GeometrieName);

int32 BackAnnotationDialog(int32 mode);

int32 ComponentSelectionDialog(int32 mode);

int32 CALLBACK CompPositionOutputDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam);

#endif
