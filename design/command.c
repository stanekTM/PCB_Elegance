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
#include "menus.h"
#include "stdio.h"
#include "commdlg.h"
#include "design.h"
#include "calc.h"
#include "memory.h"
#include "nets.h"
#include "help.h"
#include "direct.h"
#include "netlist.h"
#include "files.h"
#include "files2.h"
#include "resource.h"
#include "command.h"
#include "instance.h"
#include "mainloop.h"
#include "utf8.h"
#include "change.h"
#include "shlobj.h"
#include "shlwapi.h"
#include "winreg.h"


extern int32 DrawingObjects;
extern int32 DrawingCommand, MaxNrDesigns;
extern int32 UpdateLayout;

extern ProjectInfoRecord *ProjectInfo;

int32 LayoutEditorActive = 0;

STARTUPINFO StartupInfo;

int32 MaxNrRefsPerSheet = 100, ok;

char OldDesignFile[MAX_LENGTH_STRING];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckExeDir()
{
	WCHAR CurrentDir[MAX_LENGTH_STRING];

	GetCurrentDirectoryW(MAX_LENGTH_STRING - 50, CurrentDir);

	if (ExePath[0] == 0)
	{
		MessageBoxUTF8(DESIGNWindow, "", SC(3, "Error in finding EXE directory"), MB_APPLMODAL | MB_OK);
		return 0;
	}

	if (!SetCurrentDirectoryUTF8(ExePath))
	{
		MessageBoxUTF8(DESIGNWindow, ExePath, SC(4, "Error in EXE directory"), MB_APPLMODAL | MB_OK);
		return 0;
	}

	SetCurrentDirectoryW(CurrentDir);
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DESIGNCommand(HWND hwnd, WPARAM WParam, LPARAM LParam)
{
	int32 cnt, res, res2, SheetNr, Found, lengte, KeySize;
	PROCESS_INFORMATION ProcessInfo;
	char ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5], str[MAX_LENGTH_STRING * 5],
	     str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], *windir, WorkFile[MAX_LENGTH_STRING],
	     NewDesignFile[MAX_LENGTH_STRING];
	double x, y;
	float x2, y2;
	HWND WorkWindow;
	HKEY Key;

	if ((WParam >= ID_SHEET_OPEN_TOP_SHEET) && (WParam < ID_SHEET_OPEN_TOP_SHEET + 128))
	{
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			return;
		}

		SheetNr = (int32) WParam - ID_SHEET_OPEN_TOP_SHEET;
		GetDesignSheets();

		if ((SheetNr >= NrSheets) || (!CheckExeDir()))
			return;

		sprintf(WorkFile, "%s\\sch\\%s.sch", DesignPath, Sheets[SheetNr].SheetName);

		if (FileExistsUTF8(WorkFile) != 0)
		{
			MessageBox(DESIGNWindow, WorkFile, SC(5, "Error in finding file"), MB_APPLMODAL | MB_OK);
			return;
		}

		if ((WorkWindow = GetProjectWindow(WorkFile, 1, 0)))
		{
			ActivateProjectWindow(WorkWindow);
			return;
		}

		sprintf(ExeFile, "%s\\sch.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			return;

		sprintf(ExeParams, "\"%s\" \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExeFile, WorkFile, ExePath,
		        ProjectPath, DesignPath, DesignName);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		return;
	}

	if ((WParam >= ID_DESIGNS) && (WParam < ID_DESIGNS + TotalNrDesigns))
	{
		if (LastDesigns[WParam - ID_DESIGNS][0] != 0)
		{
			strcpy(OldDesignFile, DesignFile);
			strcpy(DesignFile, LastDesigns[WParam - ID_DESIGNS]);
			PostMessage(DESIGNWindow, WM_COMMAND, (WPARAM) ID_FILE_OPENDESIGN, (LPARAM) 1);
		}

		return;
	}

	switch (WParam)
	{
	case ID_SYMBOL_OPEN:
		sprintf(ExeFile, "%s\\sch.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		if (DesignActive)
		{
			if (CheckExeDir())
			{
				sprintf(ExeParams, "\"%s\" /a /t1 /o /e \"%s\" /p \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExeFile, ExePath,
				        DesignPath, ProjectPath, DesignPath, DesignName);
				StartupInfo.cb = sizeof(StartupInfo);
				StartupInfo.wShowWindow = SW_SHOW;
				CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
			}
		}
		else
		{
			sprintf(ExeParams, "\"%s\" /a /t1 /o /e \"%s\" /p \"%s\" /u \"%s\"", ExeFile, ExePath, DesignPath,
			        ProjectPath);
			StartupInfo.cb = sizeof(StartupInfo);
			StartupInfo.wShowWindow = SW_SHOW;
			CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		}

		break;

	case ID_ANNOTATE:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		GetDesignSheets();

		if ((res = AnnotationDialog(0, &MaxNrRefsPerSheet)) != -1)
		{
			SendMessageUTF8(EditWindow, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
			SaveOpenFiles(2);
			res2 = Annotate(res, MaxNrRefsPerSheet);
			strcpy(str, "\r\n");
			AddMessage(str);

			DeallocateMem();

			if (res2 == 0)
				ReloadSchematics(0);
		}
		else
		{
		}

		break;

	case ID_BACKANNOTATE:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		GetDesignSheets();
		SaveOpenFiles(2);

		if ((res = BackAnnotate(0)) != -1)
		{

		}
		else
		{
		}

		DeallocateMem();
		break;

	case ID_FILE_OPENDESIGN:
		if (LParam == 0)
			strcpy(OldDesignFile, DesignFile);

		if (DesignActive)
		{
			CloseOpenFiles(0);
			sprintf(str, SC(9, "Design %s closed\r\n"), OldDesignFile);
			AddMessage(str);
			AddMessage(SeparatorString);

			strcpy(NewDesignFile, DesignFile);
			strcpy(DesignFile, OldDesignFile);
			SaveDesignIniFile();
			strcpy(DesignFile, NewDesignFile);
			DesignActive = 0;
			UpdateLayout = 0;
			ProjectInfo->OtherInfos[0] = 0;
			SetWindowName(NULL, 0);
		}

		if (LParam == 0)
			DesignFile[0] = 0;

		if (LoadDesign() == 0)
		{
			LoadDesignIniFile();
			LoadSchematicIniFile(0);
			GetDesignSheets();
			SetWindowName(DesignFile, 0);
			DesignActive = 1;
			sprintf(str, SC(11, "Design opened\t\t%s\r\n"), DesignFile);
			AddMessage(str);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);

			Found = -1;

			for (cnt = 0; cnt < NrDesigns; cnt++)
			{
				if (stricmpUTF8(DesignFile, LastDesigns[cnt]) == 0)
					Found = cnt;
			}

			res = sizeof(LastDesigns[0]);

			if (Found == -1)
			{
				if (NrDesigns > 0)
					memmove(&(LastDesigns[1]), &(LastDesigns[0]), (min(TotalNrDesigns - 1, NrDesigns) * res));

				strncpy(LastDesigns[0], DesignFile, res - 1);
				LastDesigns[0][res - 1] = 0;

				if (NrDesigns < TotalNrDesigns)
					NrDesigns++;

				UpdateFileMenu(1);
			}
			else
			{
			}
		}

		break;

	case ID_FILE_NEWDESIGN:
		if (DesignActive)
		{
			CloseOpenFiles(0);
			SaveDesignIniFile();
		}

		strcpy(DesignPath, ProjectPath);

		if (NewProjectDialog(0) == 1)
			MakeNewProject(0);

		break;

	case ID_FILE_CLOSEDESIGN:
		if (DesignActive)
		{
			CloseOpenFiles(0);
			SaveDesignIniFile();
		}

		DesignActive = 0;
		SetWindowName(NULL, 0);
		sprintf(str, SC(9, "Design %s closed\r\n"), DesignFile);
		AddMessage(str);
		AddMessage(SeparatorString);

		break;

	case ID_FILE_PRINT_ALL_SHEETS:
		if (DesignActive)
		{
			SetWaitCursor();
			PrintAllSheets(0);
			SetNormalCursor();
		}

		break;

	case ID_FILE_PRINT_ALL_SHEETS_PDF:
		if (DesignActive)
		{
			SetWaitCursor();
			PrintAllSheets(1);
			SetNormalCursor();
		}

		break;

	case ID_EDIT_BILLOFMATERIALS:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		if ((res = CheckRefInstances(1)) == 0)
		{
			res = BOMDialog(0);

			if (res != -1)
			{
				SaveOpenFiles(2);
				ObjectTextBufPos = 0;
				BillOfMaterials(8 + res);
				strcpy(str, "\r\n");
				AddMessage(str);
			}
		}
		else
		{
			sprintf(str, SC(248, "Bill Of Materials could not be created\r\n"));
			AddMessage(str);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		}

		break;

	case ID_SHEET_OPEN_REF:
		if (LParam == 0)
			TextInputDialog(str3, 0);
		else
		{
			if (!ProjectInfo)
				break;

			strcpy(str3, ProjectInfo->TempStr1);
		}

		if (str3[0] == 0)
			break;

		SheetNr = GetSheetByReference(str3);

		if (SheetNr == -1)
		{
			sprintf(str2, SC(273, "Could not find reference %s"), str3);
			MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
			break;
		}

		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		if (!CheckExeDir())
			break;

		sprintf(str2, "%s\\sch\\%s.sch", DesignPath, Sheets[SheetNr].SheetName);

		if (FileExistsUTF8(str2) != 0)
		{
			MessageBoxUTF8(DESIGNWindow, str, SC(5, "Error in finding file"), MB_APPLMODAL | MB_OK);
			break;
		}

		if ((WorkWindow = GetProjectWindow(str2, 1, 0)))
		{
			if (LParam == 0)
				strcpy(ProjectInfo->TempStr1, str3);

			SendMessage(WorkWindow, WM_COMMAND, ID_VIEW_CENTER_ON_COMPONENT, 0);
			ActivateProjectWindow(WorkWindow);
			break;
		}

		if (LParam == 1)
			memset(&ProjectInfo->TempStr1, 0, sizeof(ProjectInfo->TempStr1));

		sprintf(ExeFile, "%s\\sch.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		sprintf(ExeParams, "\"%s\" \"%s\" /r \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExeFile, str2, str3,
		        ExePath, ProjectPath, DesignPath, DesignName);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		break;

	case ID_SHEET_OPEN_PARTNR:
		if (LParam == 0)
			TextInputDialog(str3, 1);
		else
		{
			if (!ProjectInfo)
				break;

			strcpy(str3, ProjectInfo->TempStr1);
		}

		if (str3[0] == 0)
			break;

		SheetNr = GetSheetByPartnr(str3);

		if (SheetNr == -1)
		{
			sprintf(str2, SC(274, "Could not find part nr %s"), str3);
			MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
			break;
		}

		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		if (!CheckExeDir())
			break;

		sprintf(str2, "%s\\sch\\%s.sch", DesignPath, Sheets[SheetNr].SheetName);

		if (FileExistsUTF8(str2) != 0)
		{
			MessageBoxUTF8(DESIGNWindow, str, SC(5, "Error in finding file"), MB_APPLMODAL | MB_OK);
			break;
		}

		if ((WorkWindow = GetProjectWindow(str2, 1, 0)))
		{
			if (LParam == 0)
				strcpy(ProjectInfo->TempStr1, str3);

			SendMessage(WorkWindow, WM_COMMAND, ID_VIEW_CENTER_ON_COMP_PARTNR, 2);
			ActivateProjectWindow(WorkWindow);
			break;
		}

		if (LParam == 1)
			memset(&ProjectInfo->TempStr1, 0, sizeof(ProjectInfo->TempStr1));

		sprintf(ExeFile, "%s\\sch.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		sprintf(ExeParams, "\"%s\" \"%s\" /r2 \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExeFile, str2, str3,
		        ExePath, ProjectPath, DesignPath, DesignName);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		break;

	case ID_LAYOUT_OPEN_REF:
		if (LParam == 0)
			TextInputDialog(str3, 0);
		else
		{
			if (!ProjectInfo)
				break;

			strcpy(str3, ProjectInfo->TempStr1);
		}

		if (str3[0] == 0)
			break;

		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		if (!CheckExeDir())
			break;

		if (LayoutFile[0] == 0)
		{
			MessageBoxUTF8(DESIGNWindow, "", SC(12, "No layout file defined"), MB_APPLMODAL | MB_OK);
			break;
		}

		sprintf(WorkFile, "%s\\pcb\\%s.pcb", DesignPath, LayoutFile);

		if (FileExistsUTF8(WorkFile) == 0)
		{
			if ((WorkWindow = GetProjectWindow(WorkFile, 4, 0)))
			{
				SendMessage(WorkWindow, WM_COMMAND, ID_VIEW_CENTER_ON_COMPONENT, 3);
				ActivateProjectWindow(WorkWindow);
				break;
			}
		}

		if (LParam == 1)
			memset(&ProjectInfo->TempStr1, 0, sizeof(ProjectInfo->TempStr1));

		SaveOpenFiles(8);
		sprintf(ExeFile, "%s\\pcb.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		sprintf(ExeParams, "\"%s\" \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\" /r \"%s\"", ExeFile, WorkFile,
		        ExePath, ProjectPath, DesignPath, DesignName, str3);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;

		if (CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo))
			LayoutEditorActive = 1;

		break;

	case ID_LAYOUT_OPEN_PARTNR:
		if (LParam == 0)
			TextInputDialog(str3, 1);
		else
		{
			if (!ProjectInfo)
				break;

			strcpy(str3, ProjectInfo->TempStr1);
		}

		if (str3[0] == 0)
			break;

		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		if (!CheckExeDir())
			break;

		if (LayoutFile[0] == 0)
		{
			MessageBoxUTF8(DESIGNWindow, "", SC(12, "No layout file defined"), MB_APPLMODAL | MB_OK);
			break;
		}

		sprintf(WorkFile, "%s\\pcb\\%s.pcb", DesignPath, LayoutFile);

		if (FileExistsUTF8(WorkFile) == 0)
		{
			if ((WorkWindow = GetProjectWindow(WorkFile, 4, 0)))
			{
				SendMessage(WorkWindow, WM_COMMAND, ID_VIEW_CENTER_ON_COMP_PARTNR, 3);
				ActivateProjectWindow(WorkWindow);
				break;
			}
		}

		if (LParam == 1)
			memset(&ProjectInfo->TempStr1, 0, sizeof(ProjectInfo->TempStr1));

		SaveOpenFiles(8);
		sprintf(ExeFile, "%s\\pcb.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		sprintf(ExeParams, "\"%s\" \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\" /r2 \"%s\"", ExeFile, WorkFile,
		        ExePath, ProjectPath, DesignPath, DesignName, str3);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;

		if (CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo))
			LayoutEditorActive = 1;

		break;

	case ID_EDIT_DESIGNSETTINGS:
		if (DesignActive)
			ProjectDialog(0);
		else
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

		break;

	case ID_EDIT_CONFIGUREPATHS:
		ConfigurePathsDialog(0);
		break;

	case ID_EDIT_CHECK:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		SetWaitCursor();
		SendMessageUTF8(EditWindow, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
		SaveOpenFiles(2);
		ObjectTextBufPos = 0;
		GetDesignSheets();
		x = -100000.0;
		SheetNr = CheckSheets(0, &x, &y);

		if (SheetNr == -2)
		{
			MessageBoxUTF8(DESIGNWindow, SC(275, "Error in loading sheets"), SC(19, "Error"), MB_APPLMODAL | MB_OK);
			break;
		}

		if (SheetNr >= 0)
		{
			strcpy(str, "\r\n");
			AddMessage(str);
			SetNormalCursor();
			sprintf(WorkFile, "%s\\sch\\%s.sch", DesignPath, Sheets[SheetNr].SheetName);
			sprintf(str2, SC(239, "Do you want to load the sheet %s"), WorkFile);

			if (MessageBoxUTF8(DESIGNWindow, str2, SC(8, "Message"), MB_APPLMODAL | MB_OKCANCEL) == IDOK)
			{
				if ((WorkWindow = GetProjectWindow(WorkFile, 1, 0)))
				{
					ActivateProjectWindow(WorkWindow);

					if ((x > -1000.0) && (x < 1000.0) && (y > -1000.0) && (y < 1000.0))
					{
						x2 = (float) x;
						y2 = (float) y;
						memcpy(&ProjectInfo->TempStr1[0], &x2, 4);
						memcpy(&ProjectInfo->TempStr1[4], &y2, 4);
						SendMessage(WorkWindow, WM_COMMAND, ID_SHEET_GOTOXY, 0);
					}
					else
						SendMessage(WorkWindow, WM_COMMAND, ID_SCHEMATIC_CHECK, 0);

					strcpy(str, "\r\n");
					AddMessage(str);
					SetNormalCursor();
					DeallocateMem();
					break;
				}
				else
				{
					sprintf(ExeFile, "%s\\sch.exe", ExePath);

					if (FileExistsUTF8(ExeFile) != 0)
						break;

					sprintf(ExeParams, "\"%s\\sch.exe\" \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExePath,
					        WorkFile, ExePath, ProjectPath, DesignPath, DesignName);

					if ((x > -1000.0) && (x < 1000.0) && (y > -1000.0) && (y < 1000.0))
					{
						sprintf(str2, " /y %.1f,%.1f", x, y);
						strcat(ExeParams, str2);
					}

					StartupInfo.cb = sizeof(StartupInfo);
					StartupInfo.wShowWindow = SW_SHOW;
					CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
				}
			}

			strcpy(str, "\r\n");
			AddMessage(str);
			SetNormalCursor();
			DeallocateMem();
			break;
		}

		if (CheckRefInstances(0) == 0)
		{
			sprintf(str, SC(165, "No errors found in check schematics\r\n"));
			AddMessage(str);

		}

		SetNormalCursor();
		DeallocateMem();
		break;

	case ID_BUILD_NETLIST:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		ObjectTextBufPos = 0;
		GetDesignSheets();
		SetWaitCursor();
		SendMessageUTF8(EditWindow, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");

		SaveOpenFiles(2);
		x = -100000.0;

		if ((SheetNr = CheckSheets(0, &x, &y)) != -1)
		{
			strcpy(str, "\r\n");
			AddMessage(str);

			SetNormalCursor();
			sprintf(str, SC(249, "Error in creating netlist\r\n"));
			AddMessage(str);


			sprintf(WorkFile, "%s\\sch\\%s.sch", DesignPath, Sheets[SheetNr].SheetName);
			sprintf(str2, SC(239, "Do you want to load the sheet %s"), WorkFile);

			if (MessageBoxUTF8(DESIGNWindow, str2, SC(8, "Message"), MB_APPLMODAL | MB_OKCANCEL) == IDOK)
			{
				cnt = 0;

				while ((cnt < 32)
				        && ((ProjectInfo->FileTypes[cnt] != 1)
				            || (stricmpUTF8(ProjectInfo->FileNames[cnt], WorkFile) != 0)))
					cnt++;

				if (cnt < 32)
				{
					ActivateProjectWindow(ProjectInfo->WindowHandles[cnt]);

					if ((x > -1000.0) && (x < 1000.0) && (y > -1000.0) && (y < 1000.0))
					{
						x2 = (float) x;
						y2 = (float) y;
						memcpy(&ProjectInfo->TempStr1[0], &x2, 4);
						memcpy(&ProjectInfo->TempStr1[4], &y2, 4);
						SendMessage(ProjectInfo->WindowHandles[cnt], WM_COMMAND, ID_SHEET_GOTOXY, 0);
					}
					else
						SendMessage(ProjectInfo->WindowHandles[cnt], WM_COMMAND, ID_SCHEMATIC_CHECK, 0);

					strcpy(str, "\r\n");
					AddMessage(str);
					SetNormalCursor();
					DeallocateMem();
					break;
				}
				else
				{
					sprintf(ExeFile, "%s\\sch.exe", ExePath);

					if (FileExistsUTF8(ExeFile) != 0)
						break;

					sprintf(ExeParams, "\"%s\\sch.exe\" \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExePath,
					        WorkFile, ExePath, ProjectPath, DesignPath, DesignName);

					if ((x > -1000.0) && (x < 1000.0) && (y > -1000.0) && (y < 1000.0))
					{
						sprintf(str2, " /y %.1f,%.1f", x, y);
						strcat(ExeParams, str2);
					}
					else
						strcat(ExeParams, " /c");

					StartupInfo.cb = sizeof(StartupInfo);
					StartupInfo.wShowWindow = SW_SHOW;
					CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
				}
			}

			strcpy(str, "\r\n");
			AddMessage(str);
			SetNormalCursor();
			DeallocateMem();
			break;
		}

		if ((res = CheckRefInstances(0)) == 0)
		{
			if ((SheetNr = BuildNets(0, &x, &y)) == 0)
				ProjectInfo->OtherInfos[0] = 1;
			else
			{
				if (SheetNr >= 1000)
				{
					SheetNr -= 1000;
					sprintf(WorkFile, "%s\\sch\\%s.sch", DesignPath, Sheets[SheetNr].SheetName);
					sprintf(str2, SC(239, "Do you want to load the sheet %s"), WorkFile);

					if (MessageBoxUTF8(DESIGNWindow, str2, SC(8, "Message"), MB_APPLMODAL | MB_OKCANCEL) == IDOK)
					{
						if ((WorkWindow = GetProjectWindow(WorkFile, 1, 0)))
						{
							ActivateProjectWindow(WorkWindow);

							if ((x > -1000.0) && (x < 1000.0) && (y > -1000.0) && (y < 1000.0))
							{
								x2 = (float) x;
								y2 = (float) y;
								memcpy(&ProjectInfo->TempStr1[0], &x2, 4);
								memcpy(&ProjectInfo->TempStr1[4], &y2, 4);
								SendMessage(WorkWindow, WM_COMMAND, ID_SHEET_GOTOXY, 0);
							}
							else
								SendMessage(WorkWindow, WM_COMMAND, ID_SCHEMATIC_CHECK, 0);

							strcpy(str, "\r\n");
							AddMessage(str);
							SetNormalCursor();
							DeallocateMem();
							break;
						}
						else
						{
							sprintf(ExeFile, "%s\\sch.exe", ExePath);

							if (FileExistsUTF8(ExeFile) != 0)
								break;

							sprintf(ExeParams, "\"%s\" \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExeFile,
							        WorkFile, ExePath, ProjectPath, DesignPath, DesignName);

							if ((x > -1000.0) && (x < 1000.0) && (y > -1000.0) && (y < 1000.0))
							{
								sprintf(str2, " /y %.1f,%.1f", x, y);
								strcat(ExeParams, str2);
							}
							else
								strcat(ExeParams, " /c");

							StartupInfo.cb = sizeof(StartupInfo);
							StartupInfo.wShowWindow = SW_SHOW;
							CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
						}
					}
				}
			}
		}
		else
		{
			sprintf(str, SC(249, "Error in creating netlist\r\n"));
			AddMessage(str);

		}

		strcpy(str, "\r\n");
		AddMessage(str);
		SetNormalCursor();
		DeallocateMem();
		break;

	case ID_FILE_EXIT:
		SendMessage(DESIGNWindow, WM_CLOSE, 0, 0);
		break;

	case ID_OPEN_GEOM_EDITOR:
		sprintf(ExeFile, "%s\\geom.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		if (DesignActive)
			sprintf(ExeParams, "\"%s\" /d \"%s\" /a /o /e \"%s\" /u \"%s\"", ExeFile, DesignPath, ExePath, ProjectPath);
		else
			sprintf(ExeParams, "\"%s\" /a /o /e \"%s\" /u \"%s\"", ExeFile, ExePath, ProjectPath);

		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		break;

	case ID_OPEN_LAYOUT_EDITOR:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		if (!CheckExeDir())
			break;

		if (LayoutFile[0] == 0)
		{
			MessageBoxUTF8(DESIGNWindow, "", SC(12, "No layout file defined"), MB_APPLMODAL | MB_OK);
			break;
		}

		sprintf(WorkFile, "%s\\pcb\\%s.pcb", DesignPath, LayoutFile);

		if (FileExistsUTF8(WorkFile) == 0)
		{
			cnt = 0;

			while ((cnt < 32)
			        && ((ProjectInfo->FileTypes[cnt] != 4) || (stricmpUTF8(ProjectInfo->FileNames[cnt], WorkFile) != 0)))
				cnt++;

			if (cnt < 32)
			{
				ActivateProjectWindow(ProjectInfo->WindowHandles[cnt]);
				break;
			}
		}

		SaveOpenFiles(8);
		sprintf(ExeFile, "%s\\pcb.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		sprintf(ExeParams, "\"%s\" \"%s\" /a /o /e \"%s\" /u \"%s\" /x \"%s\\%s.dsn\"", ExeFile, WorkFile, ExePath,
		        ProjectPath, DesignPath, DesignName);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;

		if (CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo))
			LayoutEditorActive = 1;

		break;

	case ID_LIBRARYMANAGER_SYMBOLS:
		sprintf(ExeFile, "%s\\libman.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		sprintf(ExeParams, "\"%s\" /e \"%s\" /u \"%s\"", ExeFile, ExePath, ProjectPath);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);

//      sprintf(FileName,"\"%s\\libman.exe\" /e \"%s\"",
//                       ExePath,ExePath);
//      WinExec(FileName,SW_SHOW);
		break;

	case ID_LIBRARYMANAGER_GEOMETRIES:
		sprintf(ExeFile, "%s\\libman.exe", ExePath);

		if (FileExistsUTF8(ExeFile) != 0)
			break;

		sprintf(ExeParams, "\"%s\" /d \"%s\" /g /e \"%s\" /u \"%s\"", ExeFile, DesignPath, ExePath, ProjectPath);
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);

//      sprintf(FileName,"\"%s\\libman.exe\" /d \"%s\" /g /e \"%s\"",
//                       ExePath,DesignPath,ExePath);
//      WinExec(FileName,SW_SHOW);
		break;

	case ID_EDIT_CREATEGATEPINSWAPINFO:
		GetDesignSheets();
		SaveOpenFiles(2);
		CreateGatePinSwapInfo(0);
		DeallocateMem();
		break;

	case ID_CHANGE_INSTANCES:
		if (DesignActive)
			ChangeInstances(0);
		else
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

		break;

	case ID_VIEW_GERBER_FILES:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

		if ((UseGerbv == 1) && (FileExistsUTF8(GerbvPath) == 0) && (strstr(GerbvPath, ".exe") != 0))
		{
			
			sprintf(str3, "%s\\pcb\\gerber\\GerbvProject.gvp", DesignPath);

			if (FileExistsUTF8(str3) != 0)
			{
				sprintf(str2, SC(281, "Gerbv project file not found!"));
				MessageBoxUTF8(NULL, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
				break;
			}

			sprintf(ExeFile, "%s", GerbvPath);
			sprintf(ExeParams, "\"%s\" -p \"%s\"", GerbvPath, str3);

		}
		else
		{
			sprintf(str3, "%s\\viewplot.ini", DesignPath);
			sprintf(str2, "%s\\pcb\\gerber\\GerberFiles.txt", DesignPath);
			sprintf(ExeFile, "%s\\viewplot.exe", ExePath);

			if (FileExistsUTF8(ExeFile) != 0)
				break;

			if (FileExistsUTF8(str2) == 0)
				sprintf(ExeParams, "\"%s\\viewplot.exe\" /x \"%s\" /b /y \"%s\"", ExePath, str2, str3);
			else
				sprintf(ExeParams, "\"%s\\viewplot.exe\" /b /y \"%s\"", ExePath, str3);
		}

		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		break;

	case ID_EDIT_VARS:
		if (!DesignActive)
		{
			MessageBoxUTF8(DESIGNWindow, SC(6, "No design active"), SC(8, "Message"), MB_APPLMODAL | MB_OK);

			break;
		}

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

	case ID_FILE_COPYSYMBOLSSHAPESLOCAL:
		SaveOpenFiles(2 + 8);
		GetDesignSheets();
		CopySymbolsToProject(0);
		CopyShapesToProject(0);
		break;

	case ID_FILE_ORCAD_CONV:
		OrcadConversion(0);
		break;

	case ID_FILE_ORCAD_CONV2:
		OrcadConversion(1);
		break;

	case ID_ORCAD_EXIT_MESSAGE:
		str[0] = 0;

		switch (LParam)
		{
		case 0:
			sprintf(str, SC(13, "ORCAD schematic(s) has been converted\r\n"));
			break;

		case 1:
			sprintf(str, SC(14, "ORCAD library has been converted\r\n"));
			break;

		default:
			sprintf(str, SC(15, "During conversion of the ORCAD file there was an error\r\n"));
			break;
		}

		if (str[0] != 0)
		{
			AddMessage(str);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		}

		ok = 1;
		break;

	case ID_HELP_TOPICS:
//		Help("design_manager.htm", 0);
		ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/", 0, 0, SW_SHOW);
		break;

	case ID_HELP_GETTING_STARTED:
//		Help("contents.htm", 0);
		ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/getting_started.html", 0, 0, SW_SHOW);
		break;

	case ID_HELP_ABOUT:
		AboutDialog();
// ProjectInfo
		break;

	case ID_MANUAL:
		/*           //odkaz na manual.pdf
		str2[0] = 0;
		str3[0] = 0;
		sprintf(str, ".pdf");

		if ((res = RegOpenKeyEx(HKEY_CLASSES_ROOT, str, 0, KEY_QUERY_VALUE, &Key)) == ERROR_SUCCESS)
		{
			KeySize = 200;
			RegQueryValueEx(Key, "", 0, NULL, (LPBYTE) & str2, (DWORD *) & KeySize);
			RegCloseKey(Key);
		}

		if (str2[0])
		{
			sprintf(str, "%s\\shell\\open\\command", str2);

			if ((res = RegOpenKeyEx(HKEY_CLASSES_ROOT, str, 0, KEY_QUERY_VALUE, &Key)) == ERROR_SUCCESS)
			{
				KeySize = 200;
				RegQueryValueEx(Key, "", 0, NULL, (LPBYTE) & str3, (DWORD *) & KeySize);
				RegCloseKey(Key);
			}
		}

		if (str3[0])
		{
			if (str3[0] == '\"')
				GetQuoteString(str3, str4);
			else
				GetString(str3, str4);

			if (str4[0])
			{
				sprintf(ExeParams, "\"%s\" \"%s\\manual.pdf\"", str4, ExePath);
				StartupInfo.cb = sizeof(StartupInfo);
				StartupInfo.wShowWindow = SW_SHOW;
				CreateProcess(str4, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
			}
		}
		*/

		ShellExecute(0, 0, "http://www.pcbelegance.org/support.html", 0, 0, SW_SHOW); //odkaz na web
		break;

#ifdef _DEBUG

	case ID_WRITE_SCHEMATICS:
		WriteSchematics(0);
		break;
#endif
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
