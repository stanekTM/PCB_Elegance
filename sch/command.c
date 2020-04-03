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
#include "print.h"
#include "windows.h"
#include "string.h"
#include "stdlib.h"
#include "math.h"
#include "stdio.h"
#include "keyswin.h"
#include "toets.h"
#include "commdlg.h"
#include "sch.h"
#include "calc.h"
#include "calcdef.h"
#include "memory.h"
#include "menus.h"
#include "select.h"
#include "mainloop.h"
#include "files.h"
#include "files2.h"
#include "edit.h"
#include "edit2.h"
#include "ellipss.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "resource.h"
#include "command.h"
#include "insdel.h"
#include "dialogs.h"
#include "graphics.h"
#include "movecomp.h"
#include "change.h"
#include "inscomp.h"
#include "check.h"
#include "help.h"
#include "time.h"
#include "dxf.h"
#include "utf8.h"
#include "property.h"
#include "move2.h"

RECT GeomRect;

STARTUPINFO StartupInfo;

extern int32 DrawingObjects, EditMode, NetDialogMode;
extern int32 DrawingCommand;
extern int32 GeomScreenWidth, GeomScreenHeight, GeomStartX, GeomStartY;
extern int32 ProjectIndexNr;
extern ProjectInfoRecord *ProjectInfo;
extern int32 ChangeViewMode, ProjectActive;
extern char UsedFiles[16][MAX_LENGTH_STRING];
extern char IniFile[MAX_LENGTH_STRING];
extern char SelectedSymbolNameToEdit[MAX_LENGTH_STRING];
extern char PrinterName[MAX_LENGTH_STRING];
extern int32 NrUsedFiles, PrintOptions;
extern float GotoXY_X, GotoXY_Y;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


void SCHCommand(HWND hwnd, WPARAM WParam, int32 LParam)
{
	int32 cnt, cnt2, cnt3, cnt4, res, NrCopies, Comp1, Comp2, fp, lengte, InstNr, LoadResult, Pos, Length;
	char str[MAX_LENGTH_STRING * 5], FileName[MAX_LENGTH_STRING], LibName[MAX_LENGTH_STRING], EditFile2[MAX_LENGTH_STRING], *windir,
		ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;
	InstanceRecord *Instance, OldInstance;
	RedefinedPinBusRecord *RedefinedPinBus, NewRedefinedPinBus;
	ObjectTextRecord GotoXYObjectText;
	ObjectRecord *Object;
	DesignRecord NewDesign2;
	NetLabelRecord *NetLabel, *NetLabel1, *NetLabel2;
	PinRecord *Pin, *Pin1, *Pin2;
	struct tm *today;
	time_t ltime;
#ifdef _DEBUG
	int32 TotalMemSize, DeletedMemSize;
#endif

	switch (WParam)
	{
	case ID_ESCAPE:
	case ID_VIEW_ZOOMIN:
	case ID_VIEW_ZOOMOUT:
	case ID_VIEW_VIEWFULL:
	case ID_VIEW_PREVIOUS_VIEW:
	case ID_VIEW_PAN:
	case ID_VIEW_REPAINT:
	case ID_EDIT_ZERORELATIVECURSOR:
	case ID_HELP_ON_COMMAND:
		break;

	default:
		if ((SystemBusyMode != 0) || (NetDialogMode != 0))
			return;
	}

// ********************************************************************************************************
// ********************************************************************************************************

	if ((WParam >= 10000) && (WParam < 20000))
	{
		Comp1 = (WParam - 10000) / 1000;
		Comp2 = (WParam - 10000) % 1000;
		SelectCompDialog(Comp1, Comp2);
		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************

	if ((WParam >= ID_FILE_USED_FILES) && (WParam < ID_FILE_USED_FILES + 100))
	{
		if (SaveFile2(0) == 1)
		{
			strncpy(EditFile2, UsedFiles[WParam - ID_FILE_USED_FILES], MAX_LENGTH_STRING - 1);
			ChangeFile(EditFile2, 0);
		}

		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************

	if ((WParam >= ID_EDIT_PINBUSREORDER) && (WParam < ID_EDIT_PINBUSREORDER + 100))
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((Instance->Info & 4) == 4)
				{
					NrObjects = 0;
					InstanceToObject(Instance, 0.0, 0.0, 0);
					cnt4 = 0;

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if (Object->ObjectType == SYMBOL_PINBUS_TEXT)
						{
							if ((int) WParam - ID_EDIT_PINBUSREORDER == cnt4)
							{
								for (cnt3 = 0; cnt3 < Design.NrRedefinedPinBusses; cnt3++)
								{
									RedefinedPinBus = &((*RedefinedPinBusses)[cnt3]);

									if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
									{
										if ((stricmpUTF8(RedefinedPinBus->Reference, Instance->Reference) == 0)
										        && (stricmpUTF8(RedefinedPinBus->Name, Object->Text2) == 0))
										{
											memmove(&NewRedefinedPinBus, RedefinedPinBus,
											        sizeof(RedefinedPinBusRecord));

											if (PinBusReorderDialog(&NewRedefinedPinBus, 0) == 1)
											{
												AddRedefinedPinBus(&NewRedefinedPinBus);
												RedefinedPinBus->Info |= OBJECT_NOT_VISIBLE;
												break;
											}
										}
									}
								}
							}

							cnt4++;
						}
					}
				}
			}
		}

		RePaint();
		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************

	if ((WParam >= ID_ADD_PINBUSREORDER) && (WParam < ID_ADD_PINBUSREORDER + 100))
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((Instance->Info & 4) == 4)
				{
					NrObjects = 0;
					InstanceToObject(Instance, 0.0, 0.0, 0);
					cnt4 = 0;

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if (Object->ObjectType == SYMBOL_PINBUS_TEXT)
						{
							if ((int) WParam - ID_ADD_PINBUSREORDER == cnt4)
							{
								sprintf(str, "%s  %s", Instance->Reference, Object->Text2);
								memset(&NewRedefinedPinBus, 0, sizeof(RedefinedPinBusRecord));
								strcpy(NewRedefinedPinBus.Reference, Instance->Reference);
								strcpy(NewRedefinedPinBus.Name, Object->Text2);
								NewRedefinedPinBus.Info2 = (int16) Object->Info3;

								if (strchr(NewRedefinedPinBus.Reference, '?') == 0)
								{
									if (PinBusReorderDialog(&NewRedefinedPinBus, 0) == 1)
										AddRedefinedPinBus(&NewRedefinedPinBus);
								}
								else
								{
									MessageBoxUTF8(SCHWindow, SC(122, "Reference must be set before pinbus reorder"),
										SC(38, "Error"), MB_APPLMODAL | MB_OK);
								}
							}

							cnt4++;
						}
					}
				}
			}
		}

		RePaint();
		return;
	}

// ********************************************************************************************************
// ********************************************************************************************************

	NrCopies = 1;

	switch (WParam)
	{
	case ID_ESCAPE:
		SelectionEsc = 1;
		LastAction = 0;
		break;

// ********************************************************************************************************

	case ID_FILE_NEW_SHEET:
		if (SaveFile2(0) == 1)
		{
			EditFile[0] = 0;
			EditMode = 0;
			ChangeViewMode = 0;
			ChangeFile(0, 4);
		}

		break;

	case ID_FILE_NEW_SHEET2:
		if (SaveFile2(0) == 1)
		{
			sprintf(ExeFile, "%s\\sch.exe", ExePath);

			if (FileExistsUTF8(ExeFile) != 0)
				return;

			sprintf(ExeParams, "\"%s\" /a /t0 /e \"%s\" /p \"%s\" /u \"%s\"", ExeFile, ExePath, DesignPath,
			        ProjectPath);

			if (ProjectActive)
				strcat(str, " /o");

			StartupInfo.cb = sizeof(StartupInfo);
			StartupInfo.wShowWindow = SW_SHOW;
			CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		}

		break;

	case ID_FILE_NEW_SYMBOL:
		if (SaveFile2(0) == 1)
		{
			EditFile[0] = 0;
			EditMode = 1;
			ChangeViewMode = 0;
			ChangeFile(0, 4);
			ViewMode = 0;
		}

		break;

	case ID_FILE_NEW_SYMBOL2:
		if (SaveFile2(0) == 1)
		{
			sprintf(ExeFile, "%s\\sch.exe", ExePath);

			if (FileExistsUTF8(ExeFile) != 0)
				return;

			sprintf(ExeParams, "\"%s\" /a /t1 /e \"%s\" /p \"%s\" /u \"%s\"", ExeFile, ExePath, DesignPath,
			        ProjectPath);

			if (ProjectActive)
				strcat(str, " /o");

			StartupInfo.cb = sizeof(StartupInfo);
			StartupInfo.wShowWindow = SW_SHOW;
			CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		}

		break;

	case ID_FILE_NEW_SHEETSYMBOL:
		if (SaveFile2(0) == 1)
		{
			EditFile[0] = 0;
			EditMode = 2;
			ChangeViewMode = 0;
			ViewMode = 0;
			ChangeFile(0, 4);
		}

		break;

	case ID_FILE_NEW_SHEETSYMBOL2:
		if (SaveFile2(0) == 1)
		{
			sprintf(ExeFile, "%s\\sch.exe", ExePath);

			if (FileExistsUTF8(ExeFile) != 0)
				return;

			sprintf(ExeParams, "\"%s\" /a /t2 /e \"%s\" /p \"%s\" /u \"%s\"", ExeFile, ExePath, DesignPath,
			        ProjectPath);

			if (ProjectActive)
				strcat(str, " /o");

			StartupInfo.cb = sizeof(StartupInfo);
			StartupInfo.wShowWindow = SW_SHOW;
			CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		}

		break;

	case ID_FILE_OPEN:
		if (SaveFile2(0) == 1)
			LoadNewFile(0);

		break;

	case ID_FILE_OPEN2:
		if (SaveFile2(0) == 1)
			LoadNewFile(1);

		break;

	case ID_GLOBAL_FILE_SAVE:
	case ID_FILE_SAVE:
		if (SaveFile(0) != 0)
			MessageBoxUTF8(SCHWindow, SC(18, "Error in saving file"), EditFile, MB_APPLMODAL | MB_OK);

		break;

	case ID_FILE_SAVE_WITH_ERRORS:
		if (SaveFile(8) != 0)
			MessageBoxUTF8(SCHWindow, SC(18, "Error in saving file"), EditFile, MB_APPLMODAL | MB_OK);

		break;

	case ID_FILE_SAVEAS:
		strcpy(str, EditFile);

		if (SaveFile(1) != 0)
		{
			MessageBoxUTF8(SCHWindow, SC(18, "Error in saving file"), EditFile, MB_APPLMODAL | MB_OK);

			if (str[0] != 0)
				strcpy(EditFile, str);
		}

		break;

	case ID_FILE_SAVEAS_VERSION25:
		strcpy(str, EditFile);

		if (SaveFile(3) != 0)
		{
			MessageBoxUTF8(SCHWindow, SC(18, "Error in saving file"), EditFile, MB_APPLMODAL | MB_OK);

			if (str[0] != 0)
				strcpy(EditFile, str);
		}

		break;

	case ID_FILE_SAVEAS_VERSION30:
		strcpy(str, EditFile);

		if (SaveFile(5) != 0)
		{
			MessageBoxUTF8(SCHWindow, SC(18, "Error in saving file"), EditFile, MB_APPLMODAL | MB_OK);

			if (str[0] != 0)
				strcpy(EditFile, str);
		}

		break;

	case ID_FILE_RELOAD_SYMBOLS:
		ReloadSymbols(1);
		break;

	case ID_FILE_PRINT_DEFAULT:
		Print(PrintOptions, 0, PrinterName, 3);
		break;

	case ID_FILE_PRINT:
		Print(PrintOptions, 0, PrinterName, 0);
		break;

	case ID_FILE_PRINT_COLOR:
		Print(PrintOptions, 1, PrinterName, 0);
		break;

	case ID_EXPORT_DXF:
		ExportDXF(0);
		break;

	case ID_IMPORT_DXF:
		ImportDXF(0);
		break;

	case ID_FILE_EXPORT_BITMAPS:
		ExportToBitmap(1);
		break;

	case ID_FILE_EXPORT_PDF:
#ifdef _DEBUG
		ExportToPDF(PAPERSIZE_A3, ORIENTATION_AUTO, 0, 0, 0);
#else
		ExportToPDF(PAPERSIZE_A4, ORIENTATION_AUTO, 0, 0, 0);
#endif
		break;

	case ID_FILE_EXIT:
		ExitProgram();
		break;

	case ID_FILE_SHEETBACK:
		if (NestingLevel > 0)
		{
			if (SaveFile2(0) == 1)
			{
				sprintf(str, "%s\\%s", DesignPath, NestingFiles[NestingLevel - 1]);
				strcpy(EditFile2, str);
				NestingLevel--;
				ChangeFile(EditFile2, 0);
			}
		}

		break;

	case ID_FILE_EDIT_SHEET:
		GetNrSelections();

		if ((InstancesSelected > 0) && (GetFirstSymbolSheetSelected() >= 0) && (DesignPath[0] != 0)
		        && (GetCurrentEditFile(FileName) >= 0))
		{
			if (SaveFile2(0) == 1)
			{
				InstNr = GetFirstSymbolSheetSelected();

				if (InstNr >= 0)
				{
					Instance = (InstanceRecord *) & ((*Instances)[InstNr]);
					sprintf(str, "%s\\sch\\%s.sch", DesignPath, Instance->SymbolName);
					strcpy(EditFile2, str);

					if (FileExistsUTF8(EditFile2) != 0)
					{	// Schematic does not exist
						if ((fp = FileOpenWriteUTF8(EditFile2)) > 0)
						{
							memset(&NewDesign2, 0, sizeof(DesignRecord));
							memset(&NewDesign2.Identification, 0, sizeof(Design.Identification));
//                          strcpy(NewDesign2.Identification,SheetCode5);
							strcpy(NewDesign2.Identification, SheetCode4);
							time(&ltime);
							today = localtime(&ltime);
							NewDesign2.DesignDate.Year = (uint8) today->tm_year;
							NewDesign2.DesignDate.Month = (uint8) (today->tm_mon + 1);
							NewDesign2.DesignDate.Day = (uint8) today->tm_mday;
							NewDesign2.DesignDate.Hour = (uint8) today->tm_hour;
							NewDesign2.DesignDate.Minutes = (uint8) today->tm_min;
							FileWrite(fp, &NewDesign2, sizeof(DesignRecord), &res);
							FileClose(fp);
						}
					}

					if ((strlen(FileName) < sizeof(NestingFiles[0]) - 1) && (NestingLevel < 8))
					{
						strcpy(NestingFiles[NestingLevel], FileName);
						NestingLevel++;
					}

					ChangeFile(EditFile2, 0);
				}
			}
		}

		break;

	case ID_FILE_EDIT_SYMBOL:

		LoadResult = SearchSymbol(SelectedSymbolNameToEdit, LibName, &Pos, &Length, 0); //chyba Length
		
		if (LoadResult == 1)
		{
			if ((Pos != -1) && (Pos != -2))
			{
				sprintf(str, SC(19, "Do you want to copy the symbol from library %s to the local directory %s\\sym"),
					LibName, DesignPath);

				if (MessageBoxUTF8(SCHWindow, str, SC(20, "Message"), MB_APPLMODAL | MB_YESNOCANCEL) == IDYES)
				{
					sprintf(str, "%s\\sym\\%s.sym", DesignPath, SelectedSymbolNameToEdit);

					if (CopySymbolFromLibraryToFile(SelectedSymbolNameToEdit, str) == -1)
					{
						MessageBoxUTF8(SCHWindow, str, SC(21, "Error in copying symbolfile"), MB_APPLMODAL | MB_OK);
						break;
					}
					strcpy(SelectedSymbolNameToEdit, str);
				}
				else
					break;
			}
			else
				strcpy(SelectedSymbolNameToEdit, LibName);
		}
		else
			break;
		
		sprintf(ExeFile, "%s\\sch.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			return;

		if (Pos == -2)
		{	// editace souboru se symbolem z hlavního adresáøe sym
			sprintf(ExeParams, "\"%s\" \"%s\" /a /g /w %x /p \"%s\" /e \"%s\" /u \"%s\"", ExeFile,
			        SelectedSymbolNameToEdit, (int32) SCHWindow, DesignPath, ExePath, ProjectPath);
		}
		else
		{
			sprintf(ExeParams, "\"%s\" \"%s\" /a /w %x /p \"%s\\sym\" /e \"%s\" /u \"%s\"", ExeFile,
			        SelectedSymbolNameToEdit, (int32) SCHWindow, DesignPath, ExePath, ProjectPath);
		}

		if (ProjectActive)
//			strcat(str, " /o"); //chyba Length

		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		break;

	case ID_EDIT_LAYOUT_ON_REF:
		EditLayoutComponent(0);
		break;

	case ID_VIEW_CENTER_ON_COMPONENT:
		switch (LParam)
		{
		case 0:
			if (ProjectInfo)
			{
				UnselectAll = 1;
				SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
				LastAction = 0;
				UnselectAll = 0;
				strcpy(SearchCodeString, ProjectInfo->TempStr1);
				SearchReference = 1;
			}

			break;

		case 1:
			SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
			LastAction = 0;
			CenterScreenOnInstance(0);
			break;
		}

		break;

	case ID_VIEW_CENTER_ON_COMP_PARTNR:
		if (ProjectInfo)
		{
			UnselectAll = 1;
			SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
			LastAction = 0;
			UnselectAll = 0;
			strcpy(SearchCodeString, ProjectInfo->TempStr1);
			SearchPartnr = 1;
		}

		break;

// ********************************************************************************************************

	case ID_UNSELECT_ALL:
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		LastAction = 0;
		UnselectAll = 0;
		break;

	case ID_EDIT_UNDO:
		UndoObjects();
		break;

	case ID_EDIT_REDO:
		RedoObjects();
		break;

	case ID_EDIT_DELETE:
		DeleteSelectedObjects(0);
		break;

	case ID_REPLACE_TEXTS:
		ReplaceTexts(0);
		break;

	case ID_MOVE_OBJECTS:
//      AddInstanceObjectsToSelections(1);
		MoveSelectedObjects(0, 1);
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		LastAction = 1;
		break;

	case ID_DRAG_OBJECTS:
//      AddInstanceObjectsToSelections(2);
		MoveSelectedObjects(1, 1);
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;
		LastAction = 2;
		break;

	case ID_ROTATE_OBJECTS:
		PlaceRotatedFlippedComponents(1);
		break;

	case ID_ROTATE_OBJECTS2:
		RotateObjects(0);
		break;

	case ID_MIRRORX_OBJECTS:
		ExtendSelections(0);
		PlaceRotatedFlippedComponents(4);
		break;

	case ID_MIRRORY_OBJECTS:
		ExtendSelections(0);
		PlaceRotatedFlippedComponents(8);
		break;

	case ID_ALIGN_OBJECTS_LEFT:
		AlignTextObjects(0);
		break;

	case ID_ALIGN_OBJECTS_RIGHT:
		AlignTextObjects(1);
		break;

	case ID_CHANGE_LINE_WIDTH:
		ChangeLineWidthObjects(0);
		break;

	case ID_CHANGE_TEXT_HEIGHT:
		ChangeTextHeight(0);
		break;

	case ID_CHANGE_LINE_STYLE1:
		ChangeLineStyle(0);
		break;

	case ID_CHANGE_LINE_STYLE2:
		ChangeLineStyle(1);
		break;

	case ID_CHANGE_LINE_STYLE3:
		ChangeLineStyle(2);
		break;

	case ID_CHANGE_LINE_STYLE4:
		ChangeLineStyle(3);
		break;

	case ID_SCALE_OBJECTS:
		ScaleObjects(0);
		break;

	case ID_EDIT_TEXT:
		ChangeText(0);
		break;

	case ID_ADD_TEXT_OBJECTS_FROM_TEXTFILE:
		AddTextObjectsFromFile(0);
		break;

	case ID_COPY_OBJECTS:
		MoveSelectedObjects(2, 1);
		break;

	case ID_COPY_OBJECTS10:
		MoveSelectedObjects(2, 10);
		break;

	case ID_COPY_OBJECTS9:
		MoveSelectedObjects(2, 9);
		break;

	case ID_COPY_OBJECTS8:
		MoveSelectedObjects(2, 8);
		break;

	case ID_COPY_OBJECTS7:
		MoveSelectedObjects(2, 7);
		break;

	case ID_COPY_OBJECTS6:
		MoveSelectedObjects(2, 6);
		break;

	case ID_COPY_OBJECTS5:
		MoveSelectedObjects(2, 5);
		break;

	case ID_COPY_OBJECTS4:
		MoveSelectedObjects(2, 4);
		break;

	case ID_COPY_OBJECTS3:
		MoveSelectedObjects(2, 3);
		break;

	case ID_COPY_OBJECTS2:
		MoveSelectedObjects(2, 2);
		break;

	case ID_COPY_CLIP:
		CopyToClipBoard();
		break;

	case ID_INSERT_CLIP:
		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		LastAction = 0;
		UnselectAll = 0;
		res = CopyFromClipBoard();

		switch (res)
		{
		case 0:
//          OkToAddViewPos = 0;
//          InvalidateRect(SCHWindow,NULL,0);
//          PostMessage(SCHWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
			break;

		case -1:
			MessageBoxUTF8(SCHWindow, "Nothing in clipboard", "Info", MB_APPLMODAL | MB_OK);
			break;
		}

		break;

	case ID_EDITSYMBOLNAME:
		ChangeSelections(0, SELECTIONS_INST);

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
				Instance->Info |= OBJECT_SELECTED | 2;
		}

		ChangeText(1);
		break;

	case ID_EDITSYMBOLREF:
		ChangeSelections(0, SELECTIONS_INST);

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
				Instance->Info |= OBJECT_SELECTED | 1;
		}

		ChangeText(1);
		break;

	case ID_EDIT_SWAP_NETLABELS:
		if ((EditingSymbol) || (EditingSheetSymbol))
			break;

		NetLabel1 = NULL;
		NetLabel2 = NULL;

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (!NetLabel1)
					NetLabel1 = NetLabel;
				else
				{
					if (!NetLabel2)
						NetLabel2 = NetLabel;
				}
			}
		}

		if ((NetLabel1) && (NetLabel2))
		{
			memcpy(&NewNetLabel, NetLabel1, sizeof(NetLabelRecord));
			strcpy(NewNetLabel.Name, NetLabel2->Name);
			NewNetLabel.Info &= ~OBJECT_SELECTED;
			AddNetLabel(&NewNetLabel);
			memcpy(&NewNetLabel, NetLabel2, sizeof(NetLabelRecord));
			strcpy(NewNetLabel.Name, NetLabel1->Name);
			NewNetLabel.Info &= ~OBJECT_SELECTED;
			AddNetLabel(&NewNetLabel);
			NetLabel1->DeleteNr = (int16) LastActionNr;
			NetLabel2->DeleteNr = (int16) LastActionNr;
			NetLabel1->Info |= OBJECT_NOT_VISIBLE;
			NetLabel2->Info |= OBJECT_NOT_VISIBLE;
			RePaint();
		}

		break;

	case ID_EDIT_SWAP_PINS:
		if ((!EditingSymbol) || (EditingSheetSymbol))
			break;

		Pin1 = NULL;
		Pin2 = NULL;

		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (!Pin1)
					Pin1 = Pin;
				else
				{
					if (!Pin2)
						Pin2 = Pin;
				}
			}
		}

		if ((Pin1) && (Pin2))
		{
			memcpy(&NewPin, Pin1, sizeof(PinRecord));
			strcpy(NewPin.Name, Pin2->Name);
			NewPin.Info &= ~OBJECT_SELECTED;
			AddPin(&NewPin);
			memcpy(&NewPin, Pin2, sizeof(PinRecord));
			strcpy(NewPin.Name, Pin1->Name);
			NewPin.Info &= ~OBJECT_SELECTED;
			AddPin(&NewPin);
			Pin1->DeleteNr = (int16) LastActionNr;
			Pin2->DeleteNr = (int16) LastActionNr;
			Pin1->Info |= OBJECT_NOT_VISIBLE;
			Pin2->Info |= OBJECT_NOT_VISIBLE;
			RePaint();
		}

		break;

	case ID_EDIT_PARTS_NR:
		if (EditingSymbol)
			PartNrDialog(0);

		break;

	case ID_EDIT_PARTS_PINS:
		if (DesignSymbol.NrPartsPerPackage > 1)
			SubPinStringsDialog(0);

		break;

	case ID_EDIT_SYMBOLNAMES:
		Instance = FindFirstInstance(0);
		memmove(&OldInstance, Instance, sizeof(InstanceRecord));

		if (SymbolInfoDialog(&DesignSymbol) == 1)
			RePaint();

		break;

	case ID_VIEW_OPTIONS:
		if (!EditingSymbol)
			EditSymbolSheetInfo(0);
		else
			EditSymbolSheetInfo(1);

		break;

	case ID_EDIT_SYMBOLINFO:
		CheckPins(1);
		break;

	case ID_EDIT_EXPORTTEXT:
		ExportText(0);
		break;

	case ID_EDIT_CHECK:
	case ID_SCHEMATIC_CHECK:
		if (!EditingSymbol)
		{
			res = Check(1);

			if (res == 1)
				MessageBoxUTF8(SCHWindow, SC(433, "No error(s)"), SC(20, "Message"), MB_APPLMODAL | MB_OK);
		}
		else
		{
			if (!EditingSheetSymbol)
			{
				res = CheckPins(2);

				if (res == 1)
					MessageBoxUTF8(SCHWindow, SC(433, "No error(s)"), SC(20, "Message"), MB_APPLMODAL | MB_OK);
			}
		}

		break;

	case ID_EDIT_CLEAR_REFS:
		ClearRefs(0);
		break;

	case ID_EDIT_SEARCH_TEXT:
		SearchText(0);
		break;

	case ID_EDIT_SEARCH_NEXT_TEXT:
		SearchText(1);
		break;

	case ID_EDIT_UNPROTECT:
		UnprotectSymbols();
		break;

	case ID_EDIT_PROTECT:
		ProtectSymbols();
		break;

	case ID_EDIT_DELETE_PINBUSREORDER:
		for (cnt3 = 0; cnt3 < Design.NrRedefinedPinBusses; cnt3++)
		{
			RedefinedPinBus = &((*RedefinedPinBusses)[cnt3]);
			RedefinedPinBus->Info |= OBJECT_NOT_VISIBLE;
		}

		break;

	case ID_EDIT_GATEPINSWAP:
		if ((EditingSymbol) && (!EditingSheetSymbol))
			GatePinSwapDialog(0);

		break;

	case ID_EDIT_ZERORELATIVECURSOR:
		RelX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
		RelY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY - 1));
		DisplayCursorPosition();
		break;

	case ID_EDIT_PROPERTY:
		ModifyNetProperty(0, 0);
		break;

	case ID_EDIT_GOTOXY:
		memset(&GotoXYObjectText, 0, sizeof(GotoXYObjectText));

		if (TextInputDialog(&GotoXYObjectText, 14) == 1)
			CenterScreen(GotoXYObjectText.X, GotoXYObjectText.Y, 0);

		break;

	case ID_SHEET_GOTOXY:
		if (ProjectInfo)
		{
			memcpy(&GotoXYObjectText.X, &ProjectInfo->TempStr1[0], 4);
			memcpy(&GotoXYObjectText.Y, &ProjectInfo->TempStr1[4], 4);
			CenterScreen(GotoXYObjectText.X, GotoXYObjectText.Y, 0);
		}

		break;

	case ID_EDIT_MULTIPLE_INSTANCES:
		EditInstanceInfo(1);
		break;

	case ID_SELECT_COMPS_BY_LIST:
		ComponentSelectionDialog(0);
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

// ********************************************************************************************************

	case ID_ADDNETLABEL:
		if (!EditingSymbol)
			CommandAddNetLabels(0);

		break;

	case ID_ADDNETLABELS:
		if (!EditingSymbol)
		{
			GetNrSelections();

			if (WiresSelected > 1)
				CommandAddMultipleNetLabels(0);
		}

		break;

	case ID_ADDWIRELABEL:
		if (!EditingSymbol)
			AddWireLabel(0);

		break;

	case ID_ADDPIN:
		if (EditingSymbol)
			CommandAddPins(0);

		break;

	case ID_ADDPOWERPIN:
		if (EditingSymbol)
			CommandAddPowerPins(0);

		break;

	case ID_ADDPINBUS:
		if (EditingSymbol)
			CommandAddPinBus(0);

		break;

	case ID_ADDWIRE:
		if (!EditingSymbol)
		{
			if (LParam == 0)
				CommandAddWires(0);
			else
				CommandAddWires(4 + 0);
		}

		break;

	case ID_ADDWIRE2:
		if (!EditingSymbol)
			CommandAddWires(0);

		break;

	case ID_ADDBUS:
		if (!EditingSymbol)
		{
			if (LParam == 0)
				CommandAddWires(1);
			else
				CommandAddWires(4 + 1);
		}

		break;

	case ID_ADDBUS2:
		if (!EditingSymbol)
			CommandAddWires(1);

		break;

	case ID_ADDJUNCTION:
		if (!EditingSymbol)
		{
			if (LParam == 0)
				CommandAddJunction(0);
			else
				CommandAddJunction(1);
		}

		break;

	case ID_ADDONEPINNET:
		if (!EditingSymbol)
			CommandAddOnePinNet(0);

		break;

	case ID_ADDBUSCONNECTION:
		if (!EditingSymbol)
			CommandAddBusConnection(0);

		break;

	case ID_ADDGLOBALCONNECTION_I:
		if (!EditingSymbol)
			CommandAddGlobalConnection(0);

		break;

	case ID_ADDGLOBALCONNECTION_O:
		if (!EditingSymbol)
			CommandAddGlobalConnection(1);

		break;

	case ID_ADDGLOBALCONNECTION_IO:
		if (!EditingSymbol)
			CommandAddGlobalConnection(2);

		break;

	case ID_ADDSYMBOL_SHORTCUT:
		if (!EditingSymbol)
			AddSymbolOnShortCut(0);

		break;

	case ID_ADDSYMBOL:
		if (!EditingSymbol)
			SelectSymbolDialog();

		break;

	case ID_ADDCOMPONENT:
		if (!EditingSymbol)
			SelectComponentDialog(0);

		break;

	case ID_ADDLINE:
		if (LParam == 0)
			CommandAddWires(2);
		else
			CommandAddWires(4 + 2);

		break;

	case ID_ADD_ARROW1:
		CommandAddWires(0x100 + 8 + 2);
		break;

	case ID_ADD_ARROW2:
		CommandAddWires(0x200 + 8 + 2);
		break;

	case ID_ADD_ARROW3:
		CommandAddWires(0x300 + 8 + 2);
		break;

	case ID_ADD_DIMENSION:
		CommandAddWires(0x400 + 8 + 2);
		break;

	case ID_ADD_DIMENSION2:
		CommandAddWires(0x800 + 8 + 2);
		break;

	case ID_ADDCIRCLE:
		if (LParam == 0)
			CommandAddObjectCircle(15);
		else
			CommandAddObjectCircle(16 + 15);

		break;

	case ID_ADDCIRCLE_FILLED:
		CommandAddObjectCircle(32 + 15);
		break;

	case ID_ADDCIRCLE_3:
		CommandAddObjectCircle(3);
		break;

	case ID_ADDCIRCLE_6:
		CommandAddObjectCircle(6);
		break;

	case ID_ADDCIRCLE_C:
		CommandAddObjectCircle(12);
		break;

	case ID_ADDCIRCLE_9:
		CommandAddObjectCircle(9);
		break;

	case ID_ADDCIRCLE_1:
		CommandAddObjectCircle(1);
		break;

	case ID_ADDCIRCLE_2:
		CommandAddObjectCircle(2);
		break;

	case ID_ADDCIRCLE_4:
		CommandAddObjectCircle(4);
		break;

	case ID_ADDCIRCLE_8:
		CommandAddObjectCircle(8);
		break;

	case ID_ADDRECT:
		if (LParam == 0)
			CommandAddObjectRect(0);
		else
			CommandAddObjectRect(2);

		break;

	case ID_ADDRECT2:
		if (LParam == 0)
			CommandAddObjectRect(1);
		else
			CommandAddObjectRect(3);

		break;

	case ID_ADDRECT3:
		CommandAddObjectRect(4);
		break;

	case ID_ADDARC:
		CommandAddObjectArc(0);
		break;

	case ID_ADDTEXT:
		CommandAddObjectText(0);
		break;

	case ID_ADDTEXTNUM:
		CommandAddObjectTextNumbers(0);
		break;

// ********************************************************************************************************

	case ID_VIEW_REPAINT:
		RePaint();
		break;

	case ID_VIEW_ZOOMIN:
		ZoomIn(LParam);
		break;

	case ID_VIEW_ZOOMOUT:
		ZoomOut(LParam);
		break;

	case ID_VIEW_VIEWFULL:
		ViewFull(0);
		break;

	case ID_VIEW_PREVIOUS_VIEW:
		PreviousView();
		break;

	case ID_VIEW_PAN:
		ViewPan(LParam);
		break;

	case ID_VIEW_MEMINFO:
#ifdef _DEBUG
		MemInfo(&TotalMemSize, &DeletedMemSize);
		sprintf(str, "Total memory used is %d, Undo memory used is %d", TotalMemSize, DeletedMemSize);
		MessageBoxUTF8(SCHWindow, str, "Info", MB_APPLMODAL | MB_OK);
#endif
		break;

	case ID_VIEW_CHANGECOLORS:
		ColorDialog(0);
		break;

	case ID_VIEW_LOADDEFAULTCOLORS:
		DeleteGraphicObjects();
		LoadDefaultColors();
		CreateDrawObjects(0);
		RePaint();
		break;

	case ID_VIEW_LOADDEFAULTCOLORS2:
		DeleteGraphicObjects();
		LoadDefaultColors2();
		CreateDrawObjects(0);
		RePaint();
		break;

// ********************************************************************************************************

	case ID_UNSEL_INST:
		ChangeSelections(0, SELECTIONS_INST);
		break;

	case ID_UNSEL_INST_REF:
		ChangeSelections(0, SELECTIONS_INST_REF);
		break;

	case ID_UNSEL_INST_VALUE:
		ChangeSelections(0, SELECTIONS_INST_VALUE);
		break;

	case ID_UNSEL_WIRES:
		ChangeSelections(0, SELECTIONS_WIRES);
		break;

	case ID_UNSEL_BUSSES:
		ChangeSelections(0, SELECTIONS_BUSSES);
		break;

	case ID_UNSEL_JUNCTIONS:
		ChangeSelections(0, SELECTIONS_JUNCTIONS);
		break;

	case ID_UNSEL_BUSCONN:
		ChangeSelections(0, SELECTIONS_BUSCONN);
		break;

	case ID_UNSEL_GLOBALCONN:
		ChangeSelections(0, SELECTIONS_GLOBALCONN);
		break;

	case ID_UNSEL_GLOBALCONN_TEXT:
		ChangeSelections(0, SELECTIONS_GLOBALCONN_TEXT);
		break;

	case ID_UNSEL_NETLABELS:
		ChangeSelections(0, SELECTIONS_NETLABELS);
		break;

	case ID_UNSEL_LINES:
		ChangeSelections(0, SELECTIONS_LINES);
		break;

	case ID_UNSEL_RECTS:
		ChangeSelections(0, SELECTIONS_RECTS);
		break;

	case ID_UNSEL_CIRCLES:
		ChangeSelections(0, SELECTIONS_CIRCLES);
		break;

	case ID_UNSEL_ARCS:
		ChangeSelections(0, SELECTIONS_ARCS);
		break;

	case ID_UNSEL_TEXTS:
		ChangeSelections(0, SELECTIONS_TEXTS);
		break;

	case ID_UNSEL_PINS:
		ChangeSelections(0, SELECTIONS_PINS);
		break;

	case ID_UNSEL_POWERPINS:
		ChangeSelections(0, SELECTIONS_POWERPINS);
		break;

	case ID_UNSEL_PINBUSSES:
		ChangeSelections(0, SELECTIONS_PINBUSSES);
		break;

// ********************************************************************************************************

	case ID_SEL_ONLY_INST:
		ChangeSelections(1, SELECTIONS_INST);
		break;

	case ID_SEL_ONLY_INST_REF:
		ChangeSelections(1, SELECTIONS_INST_REF);
		break;

	case ID_SEL_ONLY_INST_VALUE:
		ChangeSelections(1, SELECTIONS_INST_VALUE);
		break;

	case ID_SEL_ONLY_WIRES:
		ChangeSelections(1, SELECTIONS_WIRES);
		break;

	case ID_SEL_ONLY_BUSSES:
		ChangeSelections(1, SELECTIONS_BUSSES);
		break;

	case ID_SEL_ONLY_JUNCTIONS:
		ChangeSelections(1, SELECTIONS_JUNCTIONS);
		break;

	case ID_SEL_ONLY_BUSCONN:
		ChangeSelections(1, SELECTIONS_BUSCONN);
		break;

	case ID_SEL_ONLY_GLOBALCONN:
		ChangeSelections(1, SELECTIONS_GLOBALCONN);
		break;

	case ID_SEL_ONLY_GLOBALCONN_TEXT:
		ChangeSelections(1, SELECTIONS_GLOBALCONN_TEXT);
		break;

	case ID_SEL_ONLY_NETLABELS:
		ChangeSelections(1, SELECTIONS_NETLABELS);
		break;

	case ID_SEL_ONLY_LINES:
		ChangeSelections(1, SELECTIONS_LINES);
		break;

	case ID_SEL_ONLY_RECTS:
		ChangeSelections(1, SELECTIONS_RECTS);
		break;

	case ID_SEL_ONLY_CIRCLES:
		ChangeSelections(1, SELECTIONS_CIRCLES);
		break;

	case ID_SEL_ONLY_ARCS:
		ChangeSelections(1, SELECTIONS_ARCS);
		break;

	case ID_SEL_ONLY_TEXTS:
		ChangeSelections(1, SELECTIONS_TEXTS);
		break;

	case ID_SEL_ONLY_PINS:
		ChangeSelections(1, SELECTIONS_PINS);
		break;

	case ID_SEL_ONLY_PINTEXTS:
		ChangeSelections(1, SELECTIONS_PINTEXTS);
		break;

	case ID_SEL_ONLY_POWERPINS:
		ChangeSelections(1, SELECTIONS_POWERPINS);
		break;

	case ID_SEL_ONLY_PINBUSSES:
		ChangeSelections(1, SELECTIONS_PINBUSSES);
		break;

// ********************************************************************************************************

	case ID_SEND_WINDOW_SIZE1:
		GeomRect.left = (int16) LOWORD(LParam);
		GeomRect.top = (int16) HIWORD(LParam);
		break;

	case ID_SEND_WINDOW_SIZE2:
		GeomRect.right = LOWORD(LParam);
		GeomRect.bottom = HIWORD(LParam);
		GeomScreenWidth = GeomRect.right - GeomRect.left;
		GeomScreenHeight = GeomRect.bottom - GeomRect.top;
		GeomStartX = GeomRect.left;
		GeomStartY = GeomRect.top;
		break;

// ********************************************************************************************************

	case ID_HELP_ON_COMMAND:
		HelpAsked = 1;
		break;

	case ID_HELP_TOPICS:
		Help("schematic_editor.htm", 0);
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
