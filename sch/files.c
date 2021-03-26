/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: files.c
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
#include "stddef.h"
#include "calc.h"
#include "stdio.h"
#include "windows.h"
#include "memory.h"
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "direct.h"
#include "fcntl.h"
#include "time.h"
#include "errno.h"
#include "sys/stat.h"
#include "sch.h"
#include "files.h"
#include "files2.h"
#include "calcdef.h"
#include "menus.h"
#include "dialogs.h"
#include "resource.h"
#include "check.h"
#include "insdel.h"
#include "mainloop.h"
#include "graphics.h"
#include "math.h"
#include "utf8.h"
#include "owntime.h"

OPENFILENAME FileInputInfo;

int32 Designfp, ok, NrUsedFiles, ChangeViewMode;
char SymbolAttrBuf[4096];

extern HWND MasterWindow;
extern int32 EditMode, ProjectIndexNr;
extern char IniFile[MAX_LENGTH_STRING];
extern ProjectInfoRecord *ProjectInfo;
extern int32 ProjectActive;
extern int32 StartFromLibraryManager;

int32 LoadObjects5(void);

char UsedFiles[16][MAX_LENGTH_STRING];
char OldIniFile[MAX_LENGTH_STRING] = "";
char CopyStr[MAX_LENGTH_STRING];

STARTUPINFO StartupInfo;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void LoadIniFile(LPSTR FileName, int32 mode);

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SetWindowName(int32 mode)
{
	char str[MAX_LENGTH_STRING];
	int32 ok;

	if (!EditingSymbol)
	{
		if (EditFile[0] == 0)
			strcpy(str, SC(231, "New sheet"));
		else
			sprintf(str, SC(279, "Schematic file %s"), EditFile);
	}
	else
	{
		if (EditingSheetSymbol)
		{
			if (EditFile[0] == 0)
				strcpy(str, SC(254, "New sheet symbol"));
			else
				sprintf(str, SC(280, "Sheet symbol file %s"), EditFile);
		}
		else
		{
			if (EditFile[0] == 0)
				strcpy(str, SC(255, "New symbol"));
			else
				sprintf(str, SC(281, "Symbol file %s"), EditFile);
		}
	}

	if (ProjectIndexNr != -1)
	{
		strcpy(ProjectInfo->FileNames[ProjectIndexNr], EditFile);

		if (FileChanged)
			ProjectInfo->FileInfos[ProjectIndexNr] |= 1;
		else
			ProjectInfo->FileInfos[ProjectIndexNr] &= ~1;
	}
	else
		ok = 1;

	if (FileChanged)
		strcat(str, " *");

	if (SCHWindow != NULL)
		SetWindowTextUTF8(SCHWindow, str);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InsertUsedFile()
{
	int32 cnt;

	if (EditFile[0] == 0)
		return;

	cnt = 0;

	while ((cnt < NrUsedFiles) && (stricmpUTF8(EditFile, UsedFiles[cnt]) != 0))
		cnt++;

	if (cnt == NrUsedFiles)
	{
		if (NrUsedFiles < 16)
			strncpy(UsedFiles[NrUsedFiles++], EditFile, MAX_LENGTH_STRING - 1);
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopySymbolFromLibraryToFile(LPSTR SymbolName, LPSTR SymbolFileName)
{
#define BufSize 4096

	int Libfp, fp2, LoadResult, Pos, Length, result, cnt, res;
	uint8 Buf[BufSize];
	char LibName[MAX_LENGTH_STRING];

	LoadResult = SearchSymbol(SymbolName, LibName, &Pos, &Length, 0);

	if (LoadResult == 1)
	{
		LoadResult = 0;

		if ((Libfp = FileOpenReadOnlyUTF8(LibName)) > 0)
		{
			if ((Pos == -1) || (FileSeek(Libfp, Pos) != -1))
			{
				strlwrUTF8(SymbolFileName);

				if ((fp2 = CheckForWritingAndOpen(SymbolFileName, Length, SCHWindow)) > 0)
				{
					LoadResult = 1;
					cnt = Length;

					while ((LoadResult == 1) && (cnt > 0))
					{
						if (FileRead(Libfp, &Buf, min(BufSize, cnt), &result) != -1)
							FileWrite(fp2, &Buf, result, &res);
						else
							LoadResult = 0;

						cnt -= result;
					}

					FileClose(fp2);
				}
				else
					LoadResult = 0;
			}
			else
				LoadResult = 0;
		}

		FileClose(Libfp);
	}

	if (LoadResult == 0)
		return -1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ReloadSymbols(int32 mode)
{
	int32 cnt, cnt2, SymbolNr, Pos, Length, LoadResult, NrErrors, result, Libfp;
	InstanceRecord *Instance;
	SymbolRecord *Symbol;
	char SymbolName[MAX_LENGTH_STRING], LibName[MAX_LENGTH_STRING], Message[8192];
	SymbolsPosRecord *SymbolPos, *NewSymbolPos;
#ifdef _DEBUG
	char str[MAX_LENGTH_STRING];
	int32 Time1;
#endif

	Design.NrSymbols = 0;
	Design.SymbolsMem = 0;
	NrErrors = 0;
	sprintf(Message, SC(282, "The following symbols were not found/had errors :\r\n\r\n"));

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			SymbolNr = -1;

			for (cnt2 = 0; cnt2 < Design.NrSymbols; cnt2++)
			{
				SymbolPos = &((*SymbolsPos)[cnt2]);

				if (stricmpUTF8(SymbolPos->SymbolName, Instance->SymbolName) == 0)
					SymbolNr = cnt;
			}

			if (SymbolNr == -1)
			{
				strcpy(SymbolName, Instance->SymbolName);
#ifdef _DEBUG

				if (stricmp(SymbolName, "74f245") == 0)
					ok = 1;

				SetTimer2();
#endif
				LoadResult = SearchSymbol(SymbolName, LibName, &Pos, &Length, 0);
#ifdef _DEBUG
				Time1 = GetDifferenceTimer2inMilliSeconds();
				sprintf(str, "Search symbol %s time = %d msec\n", SymbolName, Time1);
				OutputDebugString(str);
#endif

				if (LoadResult == 1)
				{
					if (Length < 1024 * 1024)
					{
						if (Design.SymbolsMem + Length > MaxSymbolsMemSize)
						{
							if (AllocateMemSheetSymbols(Design.SymbolsMem + Length + 16384, 1) != 0)
								return 0;
						}

						if (Design.NrSymbols >= MaxNrSheetSymbols)
						{
							if (AllocateMemSheetSymbolsPos(MaxNrSheetSymbols + 32, 1) != 0)
								return 0;
						}

						Symbol = (SymbolRecord *) & SymbolsMem[Design.SymbolsMem];
						LoadResult = 0;
#ifdef _DEBUG
						SetTimer2();
#endif

						if ((Libfp = FileOpenReadOnlyUTF8(LibName)) > 0)
						{
							if ((Pos == -1) || (Pos == -2) || (FileSeek(Libfp, Pos) != -1))
							{
								LoadResult = 1;

								if (FileRead(Libfp, Symbol, (int)Length, &result) == -1)
									LoadResult = 0;

								if (result != Length)
									LoadResult = 0;
							}
							else
								LoadResult = 0;
						}

						FileClose(Libfp);
#ifdef _DEBUG
						Time1 = GetDifferenceTimer2inMilliSeconds();
						sprintf(str, "Load symbol %s time = %d msec\n", SymbolName, Time1);
						OutputDebugString(str);
#endif

						if (LoadResult == 0)
							memset(Symbol, 0, Length);

						if ((strcmp(Symbol->SymbolIdent, SymbolCode1) == 0)
						        || (strcmp(Symbol->SymbolIdent, SymbolCode2) == 0)
						        || (strcmp(Symbol->SymbolIdent, SymbolCode3) == 0))
						{
							NewSymbolPos = &((*SymbolsPos)[Design.NrSymbols]);
							memset(NewSymbolPos, 0, sizeof(SymbolsPosRecord));
							NewSymbolPos->Pos = Design.SymbolsMem;
							NewSymbolPos->Length = Length;
							memmove(NewSymbolPos->SymbolName, Symbol->Name, 32);
							NewSymbolPos->AddNr = 0;
							NewSymbolPos->DeleteNr = 0;
							Design.NrSymbols++;
							Design.SymbolsMem += Length;
						}
						else
						{
							if (strlen(Message) < 8192 - 100)
							{
								NrErrors++;
								strcat(Message, SC(283, "Wrong symbol integrity "));
								strcat(Message, SymbolName);
								strcat(Message, "\r\n");
							}
						}
					}
					else
					{
						NrErrors++;

						if (strlen(Message) < 8192 - 100)
						{
							strcat(Message, SC(284, "Memory size symbol "));
							strcat(Message, SymbolName);
							strcat(Message, SC(285, " to big\r\n"));
						}
					}
				}
				else
				{
					NrErrors++;
					strcat(Message, SymbolName);
					strcat(Message, "\r\n");
				}
			}
		}
	}

	if (mode == 1)
	{
		PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);
		CheckInputMessages(0);
		CheckInputMessages(0);
	}

	if (NrErrors > 0)
		MessageDialog(Message);

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetSymbolAttribute(SymbolRecord * Symbol, LPSTR AttributeIdent, LPSTR AttributeValue, int32 mode)
{
	int32 MemSize, NrAttributes, LengthIdent, LengthValue, pos, pos2;
	char *AttribBuf;

	NrAttributes = 0;

	if ((strcmp(Symbol->SymbolIdent, SymbolCode2) != 0) && (strcmp(Symbol->SymbolIdent, SymbolCode3) != 0))
		return -1;

	MemSize = sizeof(SymbolRecord);
	MemSize += Symbol->NrSubPinDefs * sizeof(SubPinDefsType);
	MemSize += Symbol->NrPins * sizeof(PinRecord);
	MemSize += Symbol->NrPowerPins * sizeof(PowerPinRecord);
	MemSize += Symbol->NrPinBusses * sizeof(PinBusRecord);
	MemSize += Symbol->NrObjectLines * sizeof(ObjectLineRecord);
	MemSize += Symbol->NrObjectRects * sizeof(ObjectRectRecord);
	MemSize += Symbol->NrObjectCircles * sizeof(ObjectCircleRecord);
	MemSize += Symbol->NrObjectArcs * sizeof(ObjectArcRecord);
	MemSize += Symbol->NrObjectTexts * sizeof(ObjectTextRecord);

	AttribBuf = (LPSTR) Symbol;
	AttribBuf += MemSize;
	pos = 0;

	while ((pos < sizeof(SymbolAttrBuf) - 2) && (NrAttributes < 40) && (AttribBuf[pos] != 0))
	{
		LengthIdent = strlen((LPSTR) & AttribBuf[pos]);

		if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(SymbolAttrBuf) - 1))
		{
			pos2 = pos + LengthIdent + 1;
			LengthValue = strlen((LPSTR) & AttribBuf[pos2]);

			if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(SymbolAttrBuf) - 1))
			{
				if (stricmpUTF8((LPSTR) & AttribBuf[pos], AttributeIdent) == 0)
				{
					strcpy(AttributeValue, (LPSTR) & AttribBuf[pos2]);
					return 0;
				}
			}

			pos += LengthIdent + LengthValue + 2;
		}
		else
			pos = sizeof(SymbolAttrBuf);
	}

	return -1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddSymbolAttribute(SymbolRecord * Symbol, LPSTR AttributeIdent, LPSTR AttributeValue, int32 mode)
{
	int32 cnt;
	char str[MAX_LENGTH_STRING];

	if (AttributeIdent[0] == 0)
		return -1;

	if (AttributeValue[0] == 0)
		return -1;

	for (cnt = 0; cnt < NrSymbolAttributes; cnt++)
	{
		if (stricmpUTF8(SymbolAttributesIdent[cnt], AttributeIdent) == 0)
		{
			strcpy(str, AttributeValue);
			str[63] = 0;
			strcpy(SymbolAttributesValue[cnt], str);
			return 0;
		}
	}

	for (cnt = 0; cnt < 40; cnt++)
	{
		if (SymbolAttributesIdent[cnt][0] == 0)
		{
			strcpy(str, AttributeIdent);
			str[63] = 0;
			strcpy(SymbolAttributesIdent[cnt], str);
			strcpy(str, AttributeValue);
			str[63] = 0;
			strcpy(SymbolAttributesValue[cnt], str);
			DataBaseChanged = 1;
			return 0;
		}
	}

	return -1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 RemoveSymbolAttribute(SymbolRecord * Symbol, LPSTR AttributeIdent, int32 mode)
{
	int32 cnt;

	if (AttributeIdent[0] == 0)
		return -1;

	for (cnt = 0; cnt < NrSymbolAttributes; cnt++)
	{
		if (stricmpUTF8(SymbolAttributesIdent[cnt], AttributeIdent) == 0)
		{
			SymbolAttributesIdent[cnt][0] = 0;
			DataBaseChanged = 1;
			return 0;
		}
	}

	return -1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 LoadDesign()
{
	int32 BytesToRead, SymbolNr, MemPos, result, Designfp, cnt, cnt2, Version25, Version30, Version35, pos,
	      DesignFileSize;
#ifdef _DEBUG
	int32 Time1, Time2;
	char str[MAX_LENGTH_STRING];
#endif
	SymbolsPosRecord *SymbolPos;
	SymbolRecord *Symbol;
	double x, y;
	InstanceRecord *Instance;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;

	OldObjectLineRecord OldObjectLine;
	OldObjectRectRecord OldObjectRect;
	OldObjectCircleRecord OldObjectCircle;
	OldObjectArcRecord OldObjectArc;
	OldObjectTextRecord OldObjectText;
#ifdef _DEBUG
	SetTimer1();
#endif

	ChangeViewMode = 1;

	DesignFileSize = FileSizeUTF8(EditFile);
	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
	memset(&Design, 0, sizeof(DesignRecord));

	if ((Designfp = FileOpenReadOnlyUTF8(EditFile)) < 0)
	{
		MessageBoxUTF8(SCHWindow, EditFile, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
	{
		MessageBoxUTF8(SCHWindow, EditFile, SC(274, "Error in reading file"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	Version25 = 0;
	Version30 = 0;
	Version35 = 0;

	if (strcmp(Design.Identification, SheetCode1) == 0)
		Version25 = 1;

	if ((strcmp(Design.Identification, SheetCode2) == 0) || (strcmp(Design.Identification, SheetCode3) == 0))
		Version30 = 1;

	if ((strcmp(Design.Identification, SheetCode4) == 0) || (strcmp(Design.Identification, SheetCode5) == 0))
	{
		Version30 = 1;
		Version35 = 1;
	}

	Design.NrWires = min(Design.NrWires, LimitMaxNrWires / 2);
	Design.NrBusses = min(Design.NrBusses, LimitMaxNrBusses / 2);
	Design.NrJunctions = min(Design.NrJunctions, LimitMaxNrJunctions / 2);
	Design.NrOnePinNets = min(Design.NrOnePinNets, LimitMaxNrOnePinNets / 2);
	Design.NrNetLabels = min(Design.NrNetLabels, LimitMaxNrNetLabels / 2);
	Design.NrBusConnections = min(Design.NrBusConnections, LimitMaxNrBusConnections / 2);
	Design.NrGlobalConnections = min(Design.NrGlobalConnections, LimitMaxNrGlobalConnections / 2);
	Design.NrObjectLines = min(Design.NrObjectLines, LimitMaxNrObjectLines / 2);
	Design.NrObjectRects = min(Design.NrObjectRects, LimitMaxNrObjectRects / 2);
	Design.NrObjectCircles = min(Design.NrObjectCircles, LimitMaxNrObjectCircles / 2);
	Design.NrObjectArcs = min(Design.NrObjectArcs, LimitMaxNrObjectArcs / 2);
	Design.NrObjectTexts = min(Design.NrObjectTexts, LimitMaxNrObjectTexts / 2);
	Design.NrInstances = min(Design.NrInstances, LimitMaxNrInstances / 2);
	Design.NrSymbols = min(Design.NrSymbols, LimitMaxNrSheetSymbols / 2);
	Design.NrRedefinedPinBusses = min(Design.NrRedefinedPinBusses, LimitMaxNrPinBusses / 2);

	if (AllocateMemWires(Design.NrWires * 2, 0) < 0)
		return -2;

	if (AllocateMemBusses(Design.NrBusses * 2, 0) < 0)
		return -2;

	if (AllocateMemJunctions(Design.NrJunctions * 2, 0) < 0)
		return -2;

	if (AllocateMemOnePinNets(Design.NrOnePinNets * 2, 0) < 0)
		return -2;

	if (AllocateMemNetLabels(Design.NrNetLabels * 2, 0) < 0)
		return -2;

	if (AllocateMemBusConnections(Design.NrBusConnections * 2, 0) < 0)
		return -2;

	if (AllocateMemGlobalConnections(Design.NrGlobalConnections * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectLines(Design.NrObjectLines * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectRects(Design.NrObjectRects * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectCircles(Design.NrObjectCircles * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectArcs(Design.NrObjectArcs * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectTexts(Design.NrObjectTexts * 2, 0) < 0)
		return -2;

	if (AllocateMemInstances(Design.NrInstances * 2, 0) < 0)
		return -2;

	if (AllocateMemSheetSymbolsPos(Design.NrSymbols * 2, 0) < 0)
		return -2;

	if (AllocateMemSheetSymbols(DefMaxSymbolsMemory, 0) < 0)
		return -2;

	if (AllocateMemRedefinedPinBusses(Design.NrRedefinedPinBusses, 0) < 0)
		return -2;

// ********************************************************************************************************
	pos = FileCurrentPointer(Designfp);
	BytesToRead = (int) Design.NrSymbols * sizeof(SymbolsPosRecord);
	FileRead(Designfp, SymbolsPos, BytesToRead, &result);
	pos = FileCurrentPointer(Designfp);

	if (pos == DesignFileSize)
	{
		if (Design.NrInstances > 0)
		{
			MessageBoxUTF8(SCHWindow, EditFile, SC(313, "File is corrupt"), MB_APPLMODAL | MB_OK);
			memset(&Design, 0, sizeof(DesignRecord));
			return -1;
		}
	}

// ********************************************************************************************************

	if ((strcmp(Design.Identification, SheetCode1) == 0) || (strcmp(Design.Identification, SheetCode2) == 0))
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			memset(Instance, 0, sizeof(InstanceRecord));
//      FileRead(Designfp,&OldInstance,sizeof(OldInstanceRecord),&result);
//      memcpy(Instance,&OldInstance,sizeof(OldInstanceRecord));
			FileRead(Designfp, Instance, offsetof(OldOldInstanceRecord, Geometry), &result);
			FileRead(Designfp, &Instance->Geometry,
			         sizeof(OldOldInstanceRecord) - offsetof(OldOldInstanceRecord, Geometry), &result);
		}
	}
	else
	{
		if ((strcmp(Design.Identification, SheetCode3) == 0) || (strcmp(Design.Identification, SheetCode4) == 0))
		{
//      BytesToRead=(int)Design.NrInstances*sizeof(OldInstanceRecord);
//      FileRead(Designfp,Instances,BytesToRead,&result);
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);
				memset(Instance, 0, sizeof(InstanceRecord));
				FileRead(Designfp, Instance, offsetof(OldInstanceRecord, Geometry), &result);
				FileRead(Designfp, &Instance->Geometry,
				         sizeof(OldInstanceRecord) - offsetof(OldInstanceRecord, Geometry), &result);
			}
		}
		else
		{
			BytesToRead = (int) Design.NrInstances * sizeof(InstanceRecord);
			FileRead(Designfp, Instances, BytesToRead, &result);
		}
	}

	pos = FileCurrentPointer(Designfp);
// ********************************************************************************************************
	BytesToRead = (int) Design.NrWires * sizeof(WireRecord);
	FileRead(Designfp, Wires, BytesToRead, &result);
// ********************************************************************************************************
	BytesToRead = (int) Design.NrBusses * sizeof(BusRecord);
	FileRead(Designfp, Busses, BytesToRead, &result);
// ********************************************************************************************************
	BytesToRead = (int) Design.NrJunctions * sizeof(JunctionRecord);
	FileRead(Designfp, Junctions, BytesToRead, &result);
// ********************************************************************************************************
	BytesToRead = (int) Design.NrNetLabels * sizeof(NetLabelRecord);
	FileRead(Designfp, NetLabels, BytesToRead, &result);
// ********************************************************************************************************
	BytesToRead = (int) Design.NrBusConnections * sizeof(BusConnectionRecord);
	FileRead(Designfp, BusConnections, BytesToRead, &result);
// ********************************************************************************************************
	BytesToRead = (int) Design.NrGlobalConnections * sizeof(GlobalConnectionRecord);
	FileRead(Designfp, GlobalConnections, BytesToRead, &result);

// ********************************************************************************************************
	if (Version30)
	{
		BytesToRead = (int) Design.NrObjectLines * sizeof(ObjectLineRecord);
		FileRead(Designfp, ObjectLines, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectRects * sizeof(ObjectRectRecord);
		FileRead(Designfp, ObjectRects, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectCircles * sizeof(ObjectCircleRecord);
		FileRead(Designfp, ObjectCircles, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectArcs * sizeof(ObjectArcRecord);
		FileRead(Designfp, ObjectArcs, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectTexts * sizeof(ObjectTextRecord);
		FileRead(Designfp, ObjectTexts, BytesToRead, &result);
	}
	else
	{
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			FileRead(Designfp, &OldObjectLine, sizeof(OldObjectLineRecord), &result);
			ObjectLine = &((*ObjectLines)[cnt]);
			ObjectLine->X1 = OldObjectLine.X1;
			ObjectLine->Y1 = OldObjectLine.Y1;
			ObjectLine->X2 = OldObjectLine.X2;
			ObjectLine->Y2 = OldObjectLine.Y2;
			ObjectLine->Info = OldObjectLine.Info;
			ObjectLine->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			FileRead(Designfp, &OldObjectRect, sizeof(OldObjectRectRecord), &result);
			ObjectRect = &((*ObjectRects)[cnt]);
			ObjectRect->CentreX = OldObjectRect.CentreX;
			ObjectRect->CentreY = OldObjectRect.CentreY;
			ObjectRect->Width = OldObjectRect.Width;
			ObjectRect->Height = OldObjectRect.Height;
			ObjectRect->Info = OldObjectRect.Info;
			ObjectRect->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
		{
			FileRead(Designfp, &OldObjectCircle, sizeof(OldObjectCircleRecord), &result);
			ObjectCircle = &((*ObjectCircles)[cnt]);
			ObjectCircle->CentreX = OldObjectCircle.CentreX;
			ObjectCircle->CentreY = OldObjectCircle.CentreY;
			ObjectCircle->Diam = OldObjectCircle.Diam;
			ObjectCircle->CircleMode = OldObjectCircle.CircleMode;
			ObjectCircle->Info = OldObjectCircle.Info;
			ObjectCircle->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			FileRead(Designfp, &OldObjectArc, sizeof(OldObjectArcRecord), &result);
			ObjectArc = &((*ObjectArcs)[cnt]);
			ObjectArc->CentreX = OldObjectArc.CentreX;
			ObjectArc->CentreY = OldObjectArc.CentreY;
			ObjectArc->Width = OldObjectArc.Width;
			ObjectArc->Height = OldObjectArc.Height;
			ObjectArc->StartDiffX = OldObjectArc.StartDiffX;
			ObjectArc->StartDiffY = OldObjectArc.StartDiffY;
			ObjectArc->EndDiffX = OldObjectArc.EndDiffX;
			ObjectArc->EndDiffY = OldObjectArc.EndDiffY;
			ObjectArc->Info = OldObjectArc.Info;
			ObjectArc->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			FileRead(Designfp, &OldObjectText, sizeof(OldObjectTextRecord), &result);
			ObjectText = &((*ObjectTexts)[cnt]);
			ObjectText->X = OldObjectText.X;
			ObjectText->Y = OldObjectText.Y;
			ObjectText->FontHeight = OldObjectText.FontHeight;
			ObjectText->TextMode = (int16) (OldObjectText.TextMode & 0x0f);
			ObjectText->Rotation = (float) ((OldObjectText.TextMode >> 8) * 90.0);
			memset(ObjectText->Text, 0, sizeof(ObjectText->Text));
			strcpy(ObjectText->Text, OldObjectText.Text);
			ObjectText->Thickness = (float) STANDARD_LINE_THICKNESS;
		}
	}

// ********************************************************************************************************
	BytesToRead = (int) Design.NrRedefinedPinBusses * sizeof(RedefinedPinBusRecord);
	FileRead(Designfp, RedefinedPinBusses, BytesToRead, &result);

// ********************************************************************************************************
	if (Version35)
	{
		BytesToRead = (int) Design.NrOnePinNets * sizeof(OnePinNetRecord);
		FileRead(Designfp, OnePinNets, BytesToRead, &result);
	}
	else
		Design.NrOnePinNets = 0;

// ********************************************************************************************************

	FileClose(Designfp);
#ifdef _DEBUG
	Time1 = GetDifferenceTimer1inMilliSeconds();
#endif
	ReloadSymbols(0);
#ifdef _DEBUG
	Time2 = GetDifferenceTimer1inMilliSeconds();
#endif
	InitLoadedObjects(0);


	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		SymbolNr = -1;

		for (cnt2 = 0; cnt2 < Design.NrSymbols; cnt2++)
		{
			SymbolPos = &((*SymbolsPos)[cnt2]);

			if (stricmpUTF8(SymbolPos->SymbolName, Instance->SymbolName) == 0)
				SymbolNr = cnt2;
		}

		if (SymbolNr != -1)
		{
			SymbolPos = &((*SymbolsPos)[SymbolNr]);
			MemPos = (*SymbolsPos)[SymbolNr].Pos;
			Symbol = (SymbolRecord *) & (SymbolsMem[MemPos]);

			if ((Symbol->Info & OBJECT_PROTECTED) == OBJECT_PROTECTED)
				Instance->Info |= OBJECT_PROTECTED;

			if ((Symbol->Info & 1) == 1)
				Instance->ValueInfo |= TEXT_NOT_VISIBLE;

			if ((Symbol->Info & 2) == 2)
				Instance->RefInfo |= TEXT_NOT_VISIBLE;

			if ((Symbol->Info & SHEET_SYMBOL) == SHEET_SYMBOL)
				Instance->Info |= SHEET_SYMBOL;

			if ((Symbol->Info & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS)
				Instance->Info |= MULTIPLE_SYMBOLS;

#ifdef _DEBUG

			if (stricmp(Instance->Reference, "CON100") == 0)
				ok = 1;

#endif

			if ((Instance->Geometry[0] == 0) && (stricmp(Symbol->SymbolIdent, SymbolCode3) == 0))
				GetSymbolAttribute(Symbol, "GEOMETRY", Instance->Geometry, 0);

			if ((Instance->PartNr[0] == 0) && (stricmp(Symbol->SymbolIdent, SymbolCode3) == 0))
				GetSymbolAttribute(Symbol, "PARTNR", Instance->PartNr, 0);
		}
	}

	RebuildJunctions(0, &x, &y);

	if (Design.DimensionHeight == 0.0)
		Design.DimensionHeight = DIMENSION_HEIGHT;

	if (Design.ArrowLength == 0.0)
		Design.ArrowLength = ARROW_LENGTH;

	DeleteGraphicObjects();
	CreateDrawObjects(0);
	LastActionNr = 1;
	MaxLastActionNr = 1;
	SetWindowName(0);
#ifdef _DEBUG
	sprintf(str, "Load %s time1 = %d msec, time2 = %d msec, time3 = %d msec\n", EditFile, Time1, Time2,
	        GetDifferenceTimer1inMilliSeconds());
	OutputDebugString(str);
#endif

	if (LoadObjects5() != 0)
	{
		ChangeViewMode = 0;
		ViewMode = 0;
	}
	else
	{
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 SearchSymbol(LPSTR SymbolName, LPSTR FileName, int32 * Pos, int32 * Length, int32 mode)
{
	LibRecord Lib;
	LibNameRecord *LibName, *LibNames;
	SymbolRecord Symbol;
	int32 cnt, NrLibEntries, Libfp, result, res, Symbolfp, Found;
	char FileStr[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

#ifdef _DEBUG

	if (stricmp(SymbolName, "c") == 0)
		ok = 1;

#endif
	sprintf(str, "%s\\sym\\%s.sym", DesignPath, SymbolName);

	if (FileExistsUTF8(str) == 0)
	{
		if ((Symbolfp = FileOpenReadOnlyUTF8(str)) == -1)
			return -1;

		if (FileRead(Symbolfp, &Symbol, sizeof(SymbolRecord), &result) == -1)
			return -1;

		if (FileClose(Symbolfp) == -1)
			return -1;

		strcpy(FileName, str);
		*Pos = -1;
		*Length = FileSizeUTF8(str);
		return 1;
	}

	sprintf(str, "%s\\sym\\%s.sym", ProjectPath, SymbolName);

	if (FileExistsUTF8(str) == 0)
	{
		if ((Symbolfp = FileOpenReadOnlyUTF8(str)) == -1)
			return -1;

		if (FileRead(Symbolfp, &Symbol, sizeof(SymbolRecord), &result) == -1)
			return -1;

		if (FileClose(Symbolfp) == -1)
			return -1;

		strcpy(FileName, str);
		*Pos = -2;
		*Length = FileSizeUTF8(str);
		return 1;
	}

// *******************************************************************************
// Search own libraries

	Found = 0;
	cnt = 0;

	while ((!Found) && (cnt < NrSchematicSymbolLibraries))
	{
		strcpy(FileStr, SchematicSymbolLibraries[cnt]);

		if ((Libfp = FileOpenReadOnlyUTF8(FileStr)) == -1)
			break;

		if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
			break;

		if (strcmp(Lib.Identification, LibraryCode1) == 0)
		{
			NrLibEntries = Lib.NrLibEntries;
			AllocateSpecialMem(MEM_LIBNAMES, min(NrLibEntries, 4096) * sizeof(LibNameRecord), (void *) &LibNames);

			if (FileRead(Libfp, LibNames, min(NrLibEntries, 4096) * sizeof(LibNameRecord), &result) == -1)
				break;

			LibName = LibNames;
			cnt = 0;

			while (cnt < NrLibEntries)
			{
				if (stricmpUTF8(LibName->Text, SymbolName) == 0)
				{
					Found = 1;
					*Pos = LibName->Pos;
					*Length = LibName->Length;
					strcpy(FileName, FileStr);
					break;
				}

				LibName++;
				cnt++;
			}
		}

		if (FileClose(Libfp) == -1)
			break;

		if (!Found)
			cnt++;
		else
			return 1;
	}

// *******************************************************************************
// Search global libraries
// findfirst


	sprintf(str, "%s\\lib\\*.lib", ProjectPath);
	FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while ((!Found) && (res))
	{
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		sprintf(FileStr, "%s\\lib\\%s", ProjectPath, str2);

		if ((Libfp = FileOpenReadOnlyUTF8(FileStr)) == -1)
			return -1;

		if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
			return -1;

		if (strcmp(Lib.Identification, LibraryCode1) == 0)
		{
			NrLibEntries = Lib.NrLibEntries;
			AllocateSpecialMem(MEM_LIBNAMES, min(NrLibEntries, 4096) * sizeof(LibNameRecord), (void *) &LibNames);

			if (FileRead(Libfp, LibNames, min(NrLibEntries, 4096) * sizeof(LibNameRecord), &result) == -1)
				break;

			LibName = LibNames;
			cnt = 0;

			while ((!Found) && (cnt < NrLibEntries))
			{
				if (stricmpUTF8(LibName->Text, SymbolName) == 0)
				{
					Found = 1;
					*Pos = LibName->Pos;
					*Length = LibName->Length;
					strcpy(FileName, FileStr);
				}

				LibName++;
				cnt++;
			}
		}

		if (FileClose(Libfp) == -1)
			break;

		if (!Found)
			res = FindNextFileW(FileSearchHandle, &FileInfo);
		else
			return 1;
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckWildCard(LPSTR SearchString, LPSTR Name)
{
	char SearchString2[MAX_LENGTH_STRING], SearchString2a[MAX_LENGTH_STRING], Name2[MAX_LENGTH_STRING];
	int32 cnt, cnt2, LengthSearchString, ok;
	LPSTR FoundStr;

	LengthSearchString = strlen(SearchString);

	if (LengthSearchString == 0)
		return 0;

	if (strcmp(SearchString, "*") == 0)
		return 1;


	strcpy(SearchString2, SearchString);
	cnt2 = LengthSearchString - 1;

	while ((cnt2 > 0) && (SearchString2[cnt2] == '*'))
		cnt2--;

	LengthSearchString = cnt2 + 1;
	SearchString2[LengthSearchString] = 0;
	strcpy(Name2, Name);
	struprUTF8(SearchString2);
	struprUTF8(Name2);

	strcpy(SearchString2a, SearchString2);
	cnt = 0;

	while ((cnt < LengthSearchString) && (SearchString2[cnt] == '?'))
		cnt++;

	cnt2 = LengthSearchString - 1;

	while ((cnt2 > 0) && (SearchString2[cnt2] == '?'))
		cnt2--;

	strncpy(SearchString2a, (LPSTR) & SearchString2a[cnt], cnt2 - cnt + 1);
	SearchString2a[cnt2 - cnt + 1] = 0;

	if ((cnt == 0) && (cnt2 == LengthSearchString - 1))
	{
// ********************************************************************************************************
		cnt = 0;

		while ((cnt < LengthSearchString) && (SearchString2[cnt] == '*'))
			cnt++;

		cnt2 = LengthSearchString - 1;
		strncpy(SearchString2a, (LPSTR) & SearchString2a[cnt], cnt2 - cnt + 1);
		SearchString2a[cnt2 - cnt + 1] = 0;

		if ((FoundStr = strstr(Name2, SearchString2a)) != NULL)
		{
			ok = 1;

			if ((cnt > 0) || (FoundStr == Name2))
				return 1;
		}
	}
	else
	{
// ********************************************************************************************************
		if (strlen(SearchString2a) == 0)
		{
			if (strlen(Name) == 1)
				return 1;

			return 0;
		}

		if ((FoundStr = strstr(Name2, SearchString2a)) != NULL)
		{
			if ((int32) FoundStr - (int32) Name2 == cnt)
				return 1;
		}
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GetSymbolNameFromFileName(LPSTR FileName, LPSTR SymbolName)
{
	int32 lengte, cnt, res;
	char str[MAX_LENGTH_STRING];

	SymbolName[0] = 0;
	lengte = strlen(FileName);
	memmove(&str, FileName, lengte + 1);

	if (lengte < 4)
		return;

	if (stricmp(&FileName[lengte - 3], "sym") != 0)
		return;

	cnt = lengte - 5;

	/*
	  while ((cnt>=0)
	        &&
	        (FileName[cnt]!='.')) cnt--;
	  if (cnt>=0) return;

	  cnt=lengte-5;
	*/
	while ((cnt >= 0) && (FileName[cnt] != '\\'))
		cnt--;

	res = lengte - 4;
	res -= (cnt + 1);
	memmove(SymbolName, &FileName[cnt + 1], res);
	SymbolName[res] = 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 LoadSymbol()
{
	int32 result, Designfp, cnt, pos, pos2, LengthIdent, LengthValue, BytesToRead, Version30;
	InstanceRecord *Instance;
	SymbolRecord *DesignS;
	char SymbolName[100];
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;

	OldObjectLineRecord OldObjectLine;
	OldObjectRectRecord OldObjectRect;
	OldObjectCircleRecord OldObjectCircle;
	OldObjectArcRecord OldObjectArc;
	OldObjectTextRecord OldObjectText;

	memset(&DesignSymbol, 0, sizeof(SymbolRecord));
	memset(&Design, 0, sizeof(DesignRecord));
	NrSymbolAttributes = 0;
	memset(&SymbolAttributesIdent, 0, sizeof(SymbolAttributesIdent));
	memset(&SymbolAttributesValue, 0, sizeof(SymbolAttributesValue));

	if ((Designfp = FileOpenReadOnlyUTF8(EditFile)) == -1)
		return -1;

	if (FileRead(Designfp, &DesignSymbol, sizeof(SymbolRecord), &result) == -1)
	{
		MessageBoxUTF8(SCHWindow, EditFile, SC(274, "Error in reading file"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	Version30 = 0;

	if (strcmp(DesignSymbol.SymbolIdent, SymbolCode3) == 0)
		Version30 = 1;

	memset(&SymbolName, 0, sizeof(SymbolName));
	GetSymbolNameFromFileName(EditFile, SymbolName);
	struprUTF8(SymbolName);
	memset(&DesignSymbol.Name, 0, sizeof(DesignSymbol.Name));
	strcpy(DesignSymbol.Name, SymbolName);

	if (DesignSymbol.InterfaceName[0] == 0)
		strcpy(DesignSymbol.InterfaceName, DesignSymbol.Name);

	DesignSymbol.NrPins = min(DesignSymbol.NrPins, LimitMaxNrPins / 2);
	DesignSymbol.NrPowerPins = min(DesignSymbol.NrPowerPins, LimitMaxNrPowerPins / 2);
	DesignSymbol.NrPinBusses = min(DesignSymbol.NrPinBusses, LimitMaxNrPinBusses / 2);
	DesignSymbol.NrObjectLines = min(DesignSymbol.NrObjectLines, LimitMaxNrObjectLines / 2);
	DesignSymbol.NrObjectRects = min(DesignSymbol.NrObjectRects, LimitMaxNrObjectRects / 2);
	DesignSymbol.NrObjectCircles = min(DesignSymbol.NrObjectCircles, LimitMaxNrObjectCircles / 2);
	DesignSymbol.NrObjectArcs = min(DesignSymbol.NrObjectArcs, LimitMaxNrObjectArcs / 2);
	DesignSymbol.NrObjectTexts = min(DesignSymbol.NrObjectTexts, LimitMaxNrObjectTexts / 2);
	DesignSymbol.NrSubPinDefs = min(DesignSymbol.NrSubPinDefs, LimitMaxNrSubPinDefs / 2);

	if (AllocateMemPins(DesignSymbol.NrPins, 0) < 0)
		return -2;

	if (AllocateMemPowerPins(DesignSymbol.NrPowerPins, 0) < 0)
		return -2;

	if (AllocateMemPinBusses(DesignSymbol.NrPinBusses, 0) < 0)
		return -2;

	if (AllocateMemSubPinDefs(DesignSymbol.NrSubPinDefs, 0) < 0)
		return -2;

	if (AllocateMemObjectLines(DesignSymbol.NrObjectLines * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectRects(DesignSymbol.NrObjectRects * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectCircles(DesignSymbol.NrObjectCircles * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectArcs(DesignSymbol.NrObjectArcs * 2, 0) < 0)
		return -2;

	if (AllocateMemObjectTexts(DesignSymbol.NrObjectTexts * 2, 0) < 0)
		return -2;

	if (AllocateMemInstances(16, 0) < 0)
		return -2;

	memset(&Design, 0, sizeof(DesignRecord));
	Instance = FindFirstInstance(0);
	memset(Instance, 0, sizeof(InstanceRecord));
	memmove(Instance->Reference, &DesignSymbol.InitialReference, sizeof(Instance->Reference) - 1);
	memmove(Instance->Value, &DesignSymbol.Name, sizeof(Instance->Value) - 1);
	Instance->PlacingOption = -1;
	Instance->RefOriginX = DesignSymbol.RefOriginX;
	Instance->RefOriginY = DesignSymbol.RefOriginY;
	Instance->ValueOriginX = DesignSymbol.ValueOriginX;
	Instance->ValueOriginY = DesignSymbol.ValueOriginY;

	if ((DesignSymbol.Info & 1) == 1)
		Instance->ValueInfo |= TEXT_NOT_VISIBLE;

	if ((DesignSymbol.Info & 2) == 2)
		Instance->RefInfo |= TEXT_NOT_VISIBLE;

	EditingSheetSymbol = 0;

	if ((DesignSymbol.Info & SHEET_SYMBOL) == SHEET_SYMBOL)
		EditingSheetSymbol = 1;

	Design.NrInstances = 1;
	Design.NrObjectLines = DesignSymbol.NrObjectLines;
	Design.NrObjectRects = DesignSymbol.NrObjectRects;
	Design.NrObjectCircles = DesignSymbol.NrObjectCircles;
	Design.NrObjectArcs = DesignSymbol.NrObjectArcs;
	Design.NrObjectTexts = DesignSymbol.NrObjectTexts;
	DesignS = &DesignSymbol;
	memset(SubPinDefsNames, 0, sizeof(SubPinDefsNames));

// ********************************************************************************************************
	if (DesignSymbol.NrSubPinDefs > 0)
	{
		BytesToRead = DesignSymbol.NrSubPinDefs * sizeof(SubPinDefsType);
		FileRead(Designfp, SubPinDefs, BytesToRead, &result);
	}

// ********************************************************************************************************
	BytesToRead = (int) DesignSymbol.NrPins * sizeof(PinRecord);
	FileRead(Designfp, Pins, BytesToRead, &result);

	if (DesignSymbol.NrSubPinDefs > 0)
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			strcpy(SubPinDefsNames[cnt].Name, (LPSTR) & ((*Pins)[cnt].Name));
			SubPinDefsNames[cnt].Index = cnt;
		}

		DesignSymbol.NrSubPinDefs = DesignSymbol.NrPins * DesignSymbol.NrPartsPerPackage;
	}

// ********************************************************************************************************
	BytesToRead = (int) DesignSymbol.NrPowerPins * sizeof(PowerPinRecord);
	FileRead(Designfp, PowerPins, BytesToRead, &result);
// ********************************************************************************************************
	BytesToRead = (int) DesignSymbol.NrPinBusses * sizeof(PinBusRecord);
	FileRead(Designfp, PinBusses, BytesToRead, &result);

// ********************************************************************************************************
	if (Version30)
	{
		BytesToRead = (int) Design.NrObjectLines * sizeof(ObjectLineRecord);
		FileRead(Designfp, ObjectLines, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectRects * sizeof(ObjectRectRecord);
		FileRead(Designfp, ObjectRects, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectCircles * sizeof(ObjectCircleRecord);
		FileRead(Designfp, ObjectCircles, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectArcs * sizeof(ObjectArcRecord);
		FileRead(Designfp, ObjectArcs, BytesToRead, &result);
		BytesToRead = (int) Design.NrObjectTexts * sizeof(ObjectTextRecord);
		FileRead(Designfp, ObjectTexts, BytesToRead, &result);
	}
	else
	{
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			FileRead(Designfp, &OldObjectLine, sizeof(OldObjectLineRecord), &result);
#ifdef _DEBUG

			if (cnt == 27)
				ok = 1;

#endif
			ObjectLine = &((*ObjectLines)[cnt]);
			ObjectLine->X1 = OldObjectLine.X1;
			ObjectLine->Y1 = OldObjectLine.Y1;
			ObjectLine->X2 = OldObjectLine.X2;
			ObjectLine->Y2 = OldObjectLine.Y2;
			ObjectLine->Info = OldObjectLine.Info;
			ObjectLine->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			FileRead(Designfp, &OldObjectRect, sizeof(OldObjectRectRecord), &result);
			ObjectRect = &((*ObjectRects)[cnt]);
			ObjectRect->CentreX = OldObjectRect.CentreX;
			ObjectRect->CentreY = OldObjectRect.CentreY;
			ObjectRect->Width = OldObjectRect.Width;
			ObjectRect->Height = OldObjectRect.Height;
			ObjectRect->Info = OldObjectRect.Info;
			ObjectRect->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
		{
			FileRead(Designfp, &OldObjectCircle, sizeof(OldObjectCircleRecord), &result);
			ObjectCircle = &((*ObjectCircles)[cnt]);
			ObjectCircle->CentreX = OldObjectCircle.CentreX;
			ObjectCircle->CentreY = OldObjectCircle.CentreY;
			ObjectCircle->Diam = OldObjectCircle.Diam;
			ObjectCircle->CircleMode = OldObjectCircle.CircleMode;
			ObjectCircle->Info = OldObjectCircle.Info;
			ObjectCircle->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			FileRead(Designfp, &OldObjectArc, sizeof(OldObjectArcRecord), &result);
			ObjectArc = &((*ObjectArcs)[cnt]);
			ObjectArc->CentreX = OldObjectArc.CentreX;
			ObjectArc->CentreY = OldObjectArc.CentreY;
			ObjectArc->Width = OldObjectArc.Width;
			ObjectArc->Height = OldObjectArc.Height;
			ObjectArc->StartDiffX = OldObjectArc.StartDiffX;
			ObjectArc->StartDiffY = OldObjectArc.StartDiffY;
			ObjectArc->EndDiffX = OldObjectArc.EndDiffX;
			ObjectArc->EndDiffY = OldObjectArc.EndDiffY;
			ObjectArc->Info = OldObjectArc.Info;
			ObjectArc->Thickness = (float) STANDARD_LINE_THICKNESS;
		}

		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			FileRead(Designfp, &OldObjectText, sizeof(OldObjectTextRecord), &result);
			ObjectText = &((*ObjectTexts)[cnt]);
			ObjectText->X = OldObjectText.X;
			ObjectText->Y = OldObjectText.Y;
			ObjectText->FontHeight = OldObjectText.FontHeight;
			ObjectText->TextMode = (int16) (OldObjectText.TextMode & 0x0f);
			ObjectText->Rotation = (float) ((OldObjectText.TextMode >> 8) * 90.0);
			memset(ObjectText->Text, 0, sizeof(ObjectText->Text));
			strcpy(ObjectText->Text, OldObjectText.Text);

			if (ObjectText->Thickness == 0.0)
				ObjectText->Thickness = (float) STANDARD_LINE_THICKNESS;
		}
	}

// ********************************************************************************************************
	if ((strcmp(DesignSymbol.SymbolIdent, SymbolCode2) == 0) || (strcmp(DesignSymbol.SymbolIdent, SymbolCode3) == 0))
	{
		memset(&SymbolAttrBuf, 0, sizeof(SymbolAttrBuf));
		FileRead(Designfp, &SymbolAttrBuf, sizeof(SymbolAttrBuf) - 2, &result);
		pos = 0;

		while ((pos < sizeof(SymbolAttrBuf) - 2) && (NrSymbolAttributes < 40) && (SymbolAttrBuf[pos] != 0))
		{
			LengthIdent = strlen((LPSTR) & SymbolAttrBuf[pos]);

			if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(SymbolAttrBuf) - 1))
			{
				pos2 = pos + LengthIdent + 1;
				LengthValue = strlen((LPSTR) & SymbolAttrBuf[pos2]);

				if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(SymbolAttrBuf) - 1))
				{
					if (stricmp((LPSTR) & SymbolAttrBuf[pos], "GEOMETRY") == 0)
						strcpy(Instance->Geometry, (LPSTR) & SymbolAttrBuf[pos2]);
					else
					{
						if (stricmp((LPSTR) & SymbolAttrBuf[pos], "PARTNR") == 0)
							strcpy(Instance->PartNr, (LPSTR) & SymbolAttrBuf[pos2]);
						else
						{
							strcpy(SymbolAttributesIdent[NrSymbolAttributes], (LPSTR) & SymbolAttrBuf[pos]);
							strcpy(SymbolAttributesValue[NrSymbolAttributes++], (LPSTR) & SymbolAttrBuf[pos2]);
						}
					}
				}

				pos += LengthIdent + LengthValue + 2;
			}
			else
				pos = sizeof(SymbolAttrBuf);
		}
	}

// ********************************************************************************************************

	FileClose(Designfp);
	InitLoadedObjects(0);
	SetWindowName(0);
	DeleteGraphicObjects();
	CreateDrawObjects(0);
	LastActionNr = 1;
	MaxLastActionNr = 1;

	if (Design.DimensionHeight == 0.0)
		Design.DimensionHeight = DIMENSION_HEIGHT;

	if (Design.ArrowLength == 0.0)
		Design.ArrowLength = ARROW_LENGTH;

	ViewMode = 0;
	ChangeViewMode = 0;
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 MakeBackup(LPSTR CurrentFile)
{
	char DirPart[MAX_LENGTH_STRING], FilePart[MAX_LENGTH_STRING], Extension[20], BackupFile1[MAX_LENGTH_STRING],
	     BackupFile2[MAX_LENGTH_STRING];

	if ((GetDirFromFileName(DirPart, CurrentFile) == 0) && (GetFilePartFromFileName(FilePart, CurrentFile) == 0))
	{
		GetExtensionFileName(Extension, CurrentFile);
		CutExtensionFileName(FilePart);
		sprintf(BackupFile1, "%s\\backup\\%s_4.%s", DirPart, FilePart, Extension);

		if (FileExistsUTF8(BackupFile1) == 0)
		{
			sprintf(BackupFile2, "%s\\backup\\%s_5.%s", DirPart, FilePart, Extension);
			CopyFileUTF8(BackupFile1, BackupFile2, 0);
		}

		sprintf(BackupFile1, "%s\\backup\\%s_3.%s", DirPart, FilePart, Extension);

		if (FileExistsUTF8(BackupFile1) == 0)
		{
			sprintf(BackupFile2, "%s\\backup\\%s_4.%s", DirPart, FilePart, Extension);
			CopyFileUTF8(BackupFile1, BackupFile2, 0);
		}

		sprintf(BackupFile1, "%s\\backup\\%s_2.%s", DirPart, FilePart, Extension);

		if (FileExistsUTF8(BackupFile1) == 0)
		{
			sprintf(BackupFile2, "%s\\backup\\%s_3.%s", DirPart, FilePart, Extension);
			CopyFileUTF8(BackupFile1, BackupFile2, 0);
		}

		sprintf(BackupFile1, "%s\\backup\\%s.%s", DirPart, FilePart, Extension);
		sprintf(BackupFile2, "%s\\backup\\%s_2.%s", DirPart, FilePart, Extension);
		CopyFileUTF8(BackupFile1, BackupFile2, 0);
	}
	else
		return -2;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckSymbolNameUsed(LPSTR SymbolName, int32 mode)
{
	int32 cnt;
	InstanceRecord *Instance;


	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if (stricmpUTF8(Instance->SymbolName, SymbolName) == 0)
				return 1;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 SaveFile(int32 mode)
{
	int32 result, cnt, cnt2, res, MemSize, SymbolCount, ObjectMirrorX, ObjectMirrorY, cnt3, cnt4, ok, TextRotation,
	      Alignment, Rotation, NrPins, Zero = 0, OnlyPowerPins = 1, LoadResult, NrSubPinDefsLines, Libfp =
	                  0, fp2, Pos, Length, res2;

	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	BusConnectionRecord *BusConnection;
	NetLabelRecord *NetLabel;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;

	InstanceRecord *Instance, *Instance2, CopyInstance;
	SymbolsPosRecord *SymbolPos;
	GlobalConnectionRecord *GlobalConnection;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;
	RedefinedPinBusRecord *RedefinedPinBus;
	DesignRecord NewDesign;
	SymbolRecord NewDesignSymbol;
	struct tm *today;
	time_t ltime;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], LibName[MAX_LENGTH_STRING], SymbolFile[MAX_LENGTH_STRING],
	     buf[1024];
	uint8 ZeroByte = 0;
	Instance = NULL;
	memset(&NewDesignSymbol, 0, sizeof(NewDesignSymbol));

	if (((mode & 1) == 0) && (!FileChanged))
		return 0;

	if (!EditingSymbol)
	{
		if ((mode & 6) != 0)
			res = Check(6);
		else
			res = Check((mode & 8) + 2);

		if (res == 0)
		{
			switch (mode & (16 + 8))
			{
			case 0:			// Normal save
				return 0;

			case 8:			// Save with errors
				break;

			case 16:			// Save before exit
				return -2;
			}
		}
	}
	else
	{
		if (!EditingSheetSymbol)
		{
			if (((mode & 1) == 0) && (EditingProtectedSymbol))
			{
				sprintf(str, SC(448, "Do you want to save/update the symbol into the global symbol directory %s\\sym"),
				        ProjectPath);

				switch (MessageBoxUTF8(SCHWindow, str, SC(20, "Message"), MB_APPLMODAL | MB_YESNOCANCEL))
				{
				case IDNO:
					sprintf(str, SC(449, "Do you want to save the symbol into the local symbol directory %s\\sym"),
					        DesignPath);

					if (MessageBoxUTF8(SCHWindow, str, SC(20, "Message"), MB_APPLMODAL | MB_YESNOCANCEL) != IDYES)
						return 0;

					GetFilePartFromFileName(str, EditFile);
					sprintf(EditFile, "%s\\sym\\%s", DesignPath, str);
					break;

				case IDCANCEL:
					return 0;
				}
			}

			res = CheckPins(0);

			if (res == 0)
				return 1;
		}
	}

	if (((mode & 1) == 1) || (EditFile[0] == 0))
	{
		res = SaveFileName(0);

		if (res == 0)
			return -1;

		if (res == 2)
			return 0;

		InsertUsedFile();
		AddMenuFiles();
	}

	strcpy(str, EditFile);

	if ((mode & 1) == 0)
		res = MakeBackup(EditFile);

// ******************************************************************************************
// ******************************************************************************************

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				for (cnt2 = cnt; cnt2 < Design.NrInstances; cnt2++)
				{
					Instance2 = (InstanceRecord *) & ((*Instances)[cnt2]);

					if ((cnt != cnt2) && ((Instance2->Info & OBJECT_NOT_VISIBLE) == 0))
					{
						if ((stricmpUTF8(Instance2->SymbolName, Instance->SymbolName) == 0)
						        && (InRange(Instance->OriginX, Instance2->OriginX))
						        && (InRange(Instance->OriginY, Instance2->OriginY)))
						{
							sprintf(str, SC(286, "Two or more symbols %s overlaps ( %.1f , %.1f )"), Instance->SymbolName,
							        Instance->OriginX, Instance->OriginY);
							MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
							return -1;
						}
					}
				}
			}
		}
	}

	if ((Designfp = CheckForWritingAndOpen(str, 512 * 1024, SCHWindow)) < 0)
		return -1;

	memmove(&NewDesign, &Design, sizeof(DesignRecord));

	if (!EditingSymbol)
	{

// ******************************************************************************************
// ******************************************************************************************

		FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
		SymbolCount = 0;

		for (cnt = 0; cnt < Design.NrSymbols; cnt++)
		{
			SymbolPos = (SymbolsPosRecord *) & ((*SymbolsPos)[cnt]);

			if ((CheckSymbolNameUsed(SymbolPos->SymbolName, 0)) && (SymbolPos->Length > 0))
			{
				FileWrite(Designfp, SymbolPos, sizeof(SymbolsPosRecord), &result);
				SymbolCount++;

				if ((SaveSymbolsLocally) && (DesignPath[0] != 0))
				{
					sprintf(SymbolFile, "%s\\sym\\%s.sym", DesignPath, SymbolPos->SymbolName);
					LoadResult = SearchSymbol(SymbolPos->SymbolName, LibName, &Pos, &Length, 0);

					if ((LoadResult == 1) && (stricmp(LibName, SymbolFile)) && (Length < 256 * 1024)
					        && ((Libfp = FileOpenReadOnlyUTF8(LibName)) > 0))
					{
						if ((Pos == -1) || (Pos == -2) || (FileSeek(Libfp, Pos) != -1))
						{
							fp2 = FileOpenWriteUTF8(SymbolFile);

							if (fp2 > 0)
							{
								while (Length > 0)
								{
									FileRead(Libfp, buf, min(1024, Length), &result);

									if (result > 0)
										FileWrite(fp2, buf, result, &res2);
									else
										break;

									Length -= result;
								}

								FileClose(fp2);
							}
						}

						FileClose(Libfp);
					}
				}
			}
		}

		NewDesign.NrSymbols = SymbolCount;

// ******************************************************************************************

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				memcpy(&CopyInstance, Instance, sizeof(InstanceRecord));

				ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
				ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);
				Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
				TextRotation = (((Instance->RefInfo >> 8) & 1) + Rotation) & 1;
				Alignment = Instance->RefInfo & 0x0f;

				switch (Rotation)
				{
				case 0:
					if (ObjectMirrorX == 1)
					{
						if (TextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					if (ObjectMirrorY == 1)
					{
						if (TextRotation == 0)
							Alignment = TextMirrorY[Alignment];
					}

					break;

				case 1:		//  90
					if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
					{
						if (TextRotation == 0)
							Alignment = TextMirrorX[Alignment];

						if (ObjectMirrorX == 1)
						{
							if (TextRotation == 1)
								Alignment = TextMirrorY[Alignment];
						}

						if (ObjectMirrorY == 1)
						{
							if (TextRotation == 0)
								Alignment = TextMirrorY[Alignment];
						}
					}
					else
					{
						if (TextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					break;
				}

				CopyInstance.RefInfo &= ~0x0f;
				CopyInstance.RefInfo |= Alignment;

				TextRotation = (((Instance->ValueInfo >> 8) & 1) + Rotation) & 1;
				Alignment = Instance->ValueInfo & 0x0f;

				switch (Rotation)
				{
				case 0:
					if (ObjectMirrorX == 1)
					{
						if (TextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					if (ObjectMirrorY == 1)
					{
						if (TextRotation == 0)
							Alignment = TextMirrorY[Alignment];
					}

					break;

				case 1:		//  90
					if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
					{
						if (TextRotation == 0)
							Alignment = TextMirrorX[Alignment];

						if (ObjectMirrorX == 1)
						{
							if (TextRotation == 1)
								Alignment = TextMirrorY[Alignment];
						}

						if (ObjectMirrorY == 1)
						{
							if (TextRotation == 0)
								Alignment = TextMirrorY[Alignment];
						}
					}
					else
					{
						if (TextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					break;
				}

				CopyInstance.ValueInfo &= ~0x0f;
				CopyInstance.ValueInfo |= Alignment;
				FileWrite(Designfp, &CopyInstance, sizeof(InstanceRecord), &result);
			}
			else
				NewDesign.NrInstances--;
		}

		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, Wire, sizeof(WireRecord), &result);
			else
				NewDesign.NrWires--;
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, Bus, sizeof(BusRecord), &result);
			else
				NewDesign.NrBusses--;
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, Junction, sizeof(JunctionRecord), &result);
			else
				NewDesign.NrJunctions--;
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, NetLabel, sizeof(NetLabelRecord), &result);
			else
				NewDesign.NrNetLabels--;
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, BusConnection, sizeof(BusConnectionRecord), &result);
			else
				NewDesign.NrBusConnections--;
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, GlobalConnection, sizeof(GlobalConnectionRecord), &result);
			else
				NewDesign.NrGlobalConnections--;
		}
	}
	else
	{

// ******************************************************************************************
// ******************************************************************************************

		Instance = FindFirstInstance(0);

		if ((EditingSheetSymbol) && (DesignSymbol.Name[0] == 0))
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, (LPARAM) NULL);

		strcpy(DesignSymbol.Name, Instance->Value);

		if (!EditingSheetSymbol)
			strcpy(DesignSymbol.InitialReference, Instance->Reference);

		if (DesignSymbol.InterfaceName[0] == 0)
			strcpy(DesignSymbol.InterfaceName, DesignSymbol.Name);

		memmove(&NewDesignSymbol, &DesignSymbol, sizeof(SymbolRecord));
		NewDesignSymbol.RefOriginX = Instance->RefOriginX;
		NewDesignSymbol.RefOriginY = Instance->RefOriginY;
		NewDesignSymbol.ValueOriginX = Instance->ValueOriginX;
		NewDesignSymbol.ValueOriginY = Instance->ValueOriginY;
		NewDesignSymbol.Info = 0;
		NewDesignSymbol.Info |= (DesignSymbol.Info & OBJECT_PROTECTED);
		NewDesignSymbol.Info |= (DesignSymbol.Info & MULTIPLE_SYMBOLS);

		if (EditingSheetSymbol)
			NewDesignSymbol.Info |= SHEET_SYMBOL;

		if ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
			NewDesignSymbol.Info |= 1;

		if ((Instance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
			NewDesignSymbol.Info |= 2;

		FileWrite(Designfp, &NewDesignSymbol, sizeof(SymbolRecord), &result);

		if (DesignSymbol.NrPartsPerPackage > 1)
		{
			NrSubPinDefsLines = DesignSymbol.NrSubPinDefs / DesignSymbol.NrPartsPerPackage;
			cnt2 = 0;
			cnt3 = 0;

			for (cnt = 0; cnt < DesignSymbol.NrSubPinDefs; cnt++)
			{
				if ((*SubPinDefs)[cnt][0] == 0)
					cnt3++;
			}

			NrPins = 0;

			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					for (cnt2 = 0; cnt2 < NrSubPinDefsLines; cnt2++)
					{
						if (strcmp(Pin->Name, SubPinDefsNames[cnt2].Name) == 0)
							break;
					}

					if (cnt2 == NrSubPinDefsLines)
						cnt3++;

					NrPins++;
				}
			}

			if (cnt3 > 0)
			{
				sprintf(str, SC(490, "There is a problem for the package pins"));
				MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
				SendMessage(SCHWindow, WM_COMMAND, ID_EDIT_PARTS_PINS, (LPARAM) 0);
			}

			cnt3 = 0;

			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					for (cnt2 = 0; cnt2 < NrSubPinDefsLines; cnt2++)
					{
						if (strcmp(Pin->Name, SubPinDefsNames[cnt2].Name) == 0)
							break;
					}

					if (cnt2 < NrSubPinDefsLines)
					{
						FileWrite(Designfp, &(*SubPinDefs)[cnt2 * DesignSymbol.NrPartsPerPackage],
						          DesignSymbol.NrPartsPerPackage * sizeof(SubPinDefsType), &result);
						cnt3++;
					}
				}
			}

			if (cnt3 < NrPins)
			{
				cnt4 = (NrPins - cnt3) * DesignSymbol.NrPartsPerPackage * sizeof(SubPinDefsType);

				for (cnt = 0; cnt < cnt4; cnt++)
					FileWrite(Designfp, &ZeroByte, 1, &result);
			}

			NewDesignSymbol.NrSubPinDefs = NrPins * DesignSymbol.NrPartsPerPackage;
		}
		else
			NewDesignSymbol.NrSubPinDefs = 0;

//    FileWrite(Designfp,SubPinDefs        ,(int)DesignSymbol.NrSubPinDefs*sizeof(SubPinDefsType),&result);

// ******************************************************************************************
		cnt2 = 0;

		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				cnt2++;
				FileWrite(Designfp, Pin, sizeof(PinRecord), &result);

				if (Pin->ConnectionType != POWER_CONNECTION)
					OnlyPowerPins = 0;
			}
			else
				NewDesignSymbol.NrPins--;
		}

		if (cnt2 == 0)
			OnlyPowerPins = 0;

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, PowerPin, sizeof(PowerPinRecord), &result);
			else
				NewDesignSymbol.NrPowerPins--;
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, PinBus, sizeof(PinBusRecord), &result);
			else
				NewDesignSymbol.NrPinBusses--;
		}
	}

// ******************************************************************************************
// ******************************************************************************************
	ok = 1;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0)
			FileWrite(Designfp, ObjectLine, sizeof(ObjectLineRecord), &result);
		else
			NewDesign.NrObjectLines--;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0)
			FileWrite(Designfp, ObjectRect, sizeof(ObjectRectRecord), &result);
		else
			NewDesign.NrObjectRects--;
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & OBJECT_NOT_VISIBLE) == 0)
			FileWrite(Designfp, ObjectCircle, sizeof(ObjectCircleRecord), &result);
		else
			NewDesign.NrObjectCircles--;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0)
			FileWrite(Designfp, ObjectArc, sizeof(ObjectArcRecord), &result);
		else
			NewDesign.NrObjectArcs--;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & OBJECT_NOT_VISIBLE) == 0) && (ObjectText->Text[0] != 0))
			FileWrite(Designfp, ObjectText, sizeof(ObjectTextRecord), &result);
		else
			NewDesign.NrObjectTexts--;
	}


	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrRedefinedPinBusses; cnt++)
		{
			RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);

			if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, RedefinedPinBus, sizeof(RedefinedPinBusRecord), &result);
			else
				NewDesign.NrRedefinedPinBusses--;
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if ((OnePinNet->Info & OBJECT_NOT_VISIBLE) == 0)
				FileWrite(Designfp, OnePinNet, sizeof(OnePinNetRecord), &result);
			else
				NewDesign.NrOnePinNets--;
		}
	}
	else
	{
		cnt2 = 0;

		if (Instance->Geometry[0] != 0)
		{
			strcpy(str2, "GEOMETRY");
			FileWrite(Designfp, str2, strlen(str2) + 1, &result);
			FileWrite(Designfp, Instance->Geometry, strlen(Instance->Geometry) + 1, &result);
			cnt2++;
		}

		if (Instance->PartNr[0] != 0)
		{
			strcpy(str2, "PARTNR");
			FileWrite(Designfp, str2, strlen(str2) + 1, &result);
			FileWrite(Designfp, Instance->PartNr, strlen(Instance->PartNr) + 1, &result);
			cnt2++;
		}

		if ((Instance->Reference[0] == 0) && (!OnlyPowerPins) && (!EditingSheetSymbol))
		{
			if (MessageBoxUTF8
			        (SCHWindow,
			         SC(287, "Symbol does not have a reference indication\n\nDo you want to add this reference ?"), SC(288,
			                 "Warning"),
			         MB_YESNO) == IDYES)
				PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_SYMBOLNAMES, (LPARAM) NULL);
		}

		for (cnt = 0; cnt < MaxNrSymbolAttributes - cnt2; cnt++)
		{
			if ((SymbolAttributesIdent[cnt][0] != 0) && (SymbolAttributesValue[cnt][0] != 0))
			{
				if ((strcmp(SymbolAttributesIdent[cnt], "GEOMETRY") != 0)
				        && (strcmp(SymbolAttributesIdent[cnt], "PARTNR") != 0))
				{
					FileWrite(Designfp, &SymbolAttributesIdent[cnt], strlen((LPSTR) & SymbolAttributesIdent[cnt]) + 1,
					          &result);
					FileWrite(Designfp, &SymbolAttributesValue[cnt], strlen((LPSTR) & SymbolAttributesValue[cnt]) + 1,
					          &result);
				}
			}
		}

		FileWrite(Designfp, &Zero, 4, &result);
	}

// ******************************************************************************************
// ******************************************************************************************

	FileSeek(Designfp, 0);

	if (!EditingSymbol)
	{
		memset(&NewDesign.Identification, 0, sizeof(Design.Identification));
		time(&ltime);
		today = localtime(&ltime);
		NewDesign.DesignDate.Year = (uint8) today->tm_year;
		NewDesign.DesignDate.Month = (uint8) (today->tm_mon + 1);
		NewDesign.DesignDate.Day = (uint8) today->tm_mday;
		NewDesign.DesignDate.Hour = (uint8) today->tm_hour;
		NewDesign.DesignDate.Minutes = (uint8) today->tm_min;
		strcpy(NewDesign.Identification, SheetCode1);
		strcpy(NewDesign.Identification, SheetCode5);
		FileWrite(Designfp, &NewDesign, sizeof(DesignRecord), &result);
	}
	else
	{
		NewDesignSymbol.NrObjectLines = NewDesign.NrObjectLines;
		NewDesignSymbol.NrObjectRects = NewDesign.NrObjectRects;
		NewDesignSymbol.NrObjectCircles = NewDesign.NrObjectCircles;
		NewDesignSymbol.NrObjectArcs = NewDesign.NrObjectArcs;
		NewDesignSymbol.NrObjectTexts = NewDesign.NrObjectTexts;

		MemSize = sizeof(SymbolRecord);
		MemSize += NewDesignSymbol.NrSubPinDefs * sizeof(SubPinDefsType);
		MemSize += NewDesignSymbol.NrPins * sizeof(PinRecord);
		MemSize += NewDesignSymbol.NrPowerPins * sizeof(PowerPinRecord);
		MemSize += NewDesignSymbol.NrPinBusses * sizeof(PinBusRecord);
		MemSize += NewDesignSymbol.NrObjectLines * sizeof(ObjectLineRecord);
		MemSize += NewDesignSymbol.NrObjectRects * sizeof(ObjectRectRecord);
		MemSize += NewDesignSymbol.NrObjectCircles * sizeof(ObjectCircleRecord);
		MemSize += NewDesignSymbol.NrObjectArcs * sizeof(ObjectArcRecord);
		MemSize += NewDesignSymbol.NrObjectTexts * sizeof(ObjectTextRecord);
		NewDesignSymbol.MemSize = MemSize;
		memset(&NewDesignSymbol.SymbolIdent, 0, sizeof(DesignSymbol.SymbolIdent));
		strcpy(NewDesignSymbol.SymbolIdent, SymbolCode3);
		FileWrite(Designfp, &NewDesignSymbol, sizeof(SymbolRecord), &result);
	}

	FileClose(Designfp);
	FileChanged = 0;
	SetWindowName(0);

	if (EditingSymbol)
	{
		if (MasterWindow != NULL)
			PostMessage(MasterWindow, WM_COMMAND, (WPARAM) ID_FILE_RELOAD_SYMBOLS, (LPARAM) NULL);

//    RePaint();
	}

	RePaint();

	return 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 SaveFile2(int32 mode)
{
	int32 res = 0;

	if (FileChanged)
	{
		res = MessageBoxUTF8(SCHWindow, SC(289, "Schematic/symbol has changed, save ?"), SC(20, "Message"), MB_APPLMODAL | MB_YESNOCANCEL);

		if (res == IDYES)
		{
			res = SaveFile(16);

			if (res == -1)
			{
				MessageBoxUTF8(SCHWindow, SC(18, "Error in saving file"), EditFile, MB_APPLMODAL | MB_OK);
				return 0;
			}

			if (res == -2)
				return 0;

			return 1;
		}
		else
		{
			if (res == IDCANCEL)
				return 0;
		}
	}

	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckFileName(LPSTR CheckFile, int32 FileSize, int32 Mode)
{
// Mode :
//
//  bit 0 (0 is load,1 is save)
//  bit 1 (0 is sheet,1 is symbol)
//
	char str2[MAX_LENGTH_STRING];
	WCHAR str2W[MAX_LENGTH_STRING], str3W[MAX_LENGTH_STRING], *FileNameW;
	char *FileName;
	int32 cnt;

	if (Mode == 0)
	{
		Utf8ToUnicode(CheckFile, str2W, MAX_LENGTH_STRING - 50);
		GetFullPathNameW((LPWSTR) str2W, MAX_LENGTH_STRING - 50, str3W, (LPWSTR *) & FileNameW);
		UnicodeToUtf8(str3W, str2, MAX_LENGTH_STRING - 50);
		FileName = strrchr(str2, '\\');

		if (!FileName)
			FileName = str2;

		cnt = strlen(str2) - 1;

		while ((cnt > 0) && (str2[cnt] != '\\'))
			cnt--;

		strncpy(EditPath, str2, cnt + 1);
		cnt = strlen(FileName);

		if (cnt > 4)
		{
			if (stricmp(&(FileName[cnt - 3]), "sch") == 0)
			{
				strcpy(EditFile, FileName);
				EditingSymbol = 0;
				return 0;
			}

			if (stricmp(&(FileName[cnt - 3]), "sym") == 0)
			{
				strcpy(EditFile, FileName);
				EditingSymbol = 1;
				return 0;
			}
		}

		strcpy(EditFile, FileName);
		strcat(EditFile, ".sch");
		EditingSymbol = 0;
		return 0;
	}

	return -1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InitFile(int32 mode)
{
	InstancesSelected = 0;
	WiresSelected = 0;
	BussesSelected = 0;
	BusConnectionsSelected = 0;
	JunctionsSelected = 0;
	OnePinNetsSelected = 0;
	NetLabelsSelected = 0;
	ObjectLinesSelected = 0;
	ObjectRectsSelected = 0;
	ObjectCirclesSelected = 0;
	ObjectArcsSelected = 0;
	ObjectTextsSelected = 0;
	PinsSelected = 0;
	PowerPinsSelected = 0;
	PinBussesSelected = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void InitLoadedObjects(int32 Mode)
{
	int32 cnt, ObjectMirrorX, ObjectMirrorY, TextRotation, Alignment, Rotation;

	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	BusConnectionRecord *BusConnection;
	NetLabelRecord *NetLabel;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	InstanceRecord *Instance;
	GlobalConnectionRecord *GlobalConnection;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;
	SymbolsPosRecord *SymbolPos;
	RedefinedPinBusRecord *RedefinedPinBus;
#ifdef _DEBUG
	int32 ok;
#endif

	for (cnt = 0; cnt < MaxNrSheetSymbols; cnt++)
	{
		SymbolPos = (SymbolsPosRecord *) & ((*SymbolsPos)[cnt]);
		SymbolPos->Info = 0;
		SymbolPos->AddNr = 0;
		SymbolPos->DeleteNr = 0;
	}

	for (cnt = 0; cnt < MaxNrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		Instance->Info = 0;
		Instance->AddNr = 0;
		Instance->DeleteNr = 0;

		if (!EditingSymbol)
		{
			ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
			ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);
			Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
			TextRotation = (((Instance->RefInfo >> 8) & 1) + Rotation) & 1;
			Alignment = Instance->RefInfo & 0x0f;

			switch (Rotation)
			{
			case 0:
				if (ObjectMirrorX == 1)
				{
					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				if (ObjectMirrorY == 1)
				{
					if (TextRotation == 0)
						Alignment = TextMirrorY[Alignment];
				}

				break;

			case 1:			//  90
				if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
				{
					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (ObjectMirrorX == 1)
					{
						if (TextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					if (ObjectMirrorY == 1)
					{
						if (TextRotation == 0)
							Alignment = TextMirrorY[Alignment];
					}
				}
				else
				{
					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				break;
			}

			Instance->RefInfo &= ~0x0f;
			Instance->RefInfo |= Alignment;

			TextRotation = (((Instance->ValueInfo >> 8) & 1) + Rotation) & 1;
			Alignment = Instance->ValueInfo & 0x0f;

			switch (Rotation)
			{
			case 0:
				if (ObjectMirrorX == 1)
				{
					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				if (ObjectMirrorY == 1)
				{
					if (TextRotation == 0)
						Alignment = TextMirrorY[Alignment];
				}

				break;

			case 1:			//  90
				if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
				{
					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (ObjectMirrorX == 1)
					{
						if (TextRotation == 1)
							Alignment = TextMirrorY[Alignment];
					}

					if (ObjectMirrorY == 1)
					{
						if (TextRotation == 0)
							Alignment = TextMirrorY[Alignment];
					}
				}
				else
				{
					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				break;
			}

			Instance->ValueInfo &= ~0x0f;
			Instance->ValueInfo |= Alignment;

			if ((fabs(Instance->OriginX) > 90000.0) || (fabs(Instance->OriginY) > 90000.0))
				Instance->Info = OBJECT_NOT_VISIBLE;
		}
	}

	for (cnt = 0; cnt < MaxNrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);
		Wire->Info = 0;
		Wire->AddNr = 0;
		Wire->DeleteNr = 0;

		if ((fabs(Wire->X1) > 90000.0) || (fabs(Wire->X2) > 90000.0) || (fabs(Wire->Y1) > 90000.0)
		        || (fabs(Wire->Y2) > 90000.0))
			Wire->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);
		Bus->Info = 0;
		Bus->AddNr = 0;
		Bus->DeleteNr = 0;

		if ((fabs(Bus->X1) > 90000.0) || (fabs(Bus->X2) > 90000.0) || (fabs(Bus->Y1) > 90000.0)
		        || (fabs(Bus->Y2) > 90000.0))
			Bus->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);
		Junction->Info = 0;
		Junction->AddNr = 0;
		Junction->DeleteNr = 0;

		if ((fabs(Junction->X) > 90000.0) || (fabs(Junction->Y) > 90000.0))
			Junction->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);
		OnePinNet->Info = 0;
		OnePinNet->AddNr = 0;
		OnePinNet->DeleteNr = 0;

		if ((fabs(OnePinNet->X) > 90000.0) || (fabs(OnePinNet->Y) > 90000.0))
			OnePinNet->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);
		BusConnection->Info = 0;
		BusConnection->AddNr = 0;
		BusConnection->DeleteNr = 0;

		if ((fabs(BusConnection->X) > 90000.0) || (fabs(BusConnection->Y) > 90000.0))
			BusConnection->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);
		GlobalConnection->Info = 0;
		GlobalConnection->AddNr = 0;
		GlobalConnection->DeleteNr = 0;

		if ((fabs(GlobalConnection->X) > 90000.0) || (fabs(GlobalConnection->Y) > 90000.0))
			GlobalConnection->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);
		NetLabel->Info = 0;
		NetLabel->AddNr = 0;
		NetLabel->DeleteNr = 0;

		if ((fabs(NetLabel->ConnectX) > 90000.0) || (fabs(NetLabel->ConnectY) > 90000.0))
			NetLabel->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
#ifdef _DEBUG

		if (cnt == 27)
			ok = 1;

#endif
		ObjectLine->Info = 0;
		ObjectLine->AddNr = 0;
		ObjectLine->DeleteNr = 0;

		if ((fabs(ObjectLine->X1) > 90000.0) || (fabs(ObjectLine->X2) > 90000.0) || (fabs(ObjectLine->Y1) > 90000.0)
		        || (fabs(ObjectLine->Y2) > 90000.0))
			ObjectLine->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);
		ObjectRect->Info = 0;
		ObjectRect->AddNr = 0;
		ObjectRect->DeleteNr = 0;

		if ((fabs(ObjectRect->CentreX) > 90000.0) || (fabs(ObjectRect->CentreY) > 90000.0))
			ObjectRect->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);
		ObjectCircle->Info = 0;
		ObjectCircle->AddNr = 0;
		ObjectCircle->DeleteNr = 0;

		if ((fabs(ObjectCircle->CentreX) > 90000.0) || (fabs(ObjectCircle->CentreY) > 90000.0))
			ObjectCircle->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ObjectArc->Info = 0;
		ObjectArc->AddNr = 0;
		ObjectArc->DeleteNr = 0;

		if ((fabs(ObjectArc->CentreX) > 90000.0) || (fabs(ObjectArc->CentreY) > 90000.0))
			ObjectArc->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);
		ObjectText->Info = 0;
		ObjectText->AddNr = 0;
		ObjectText->DeleteNr = 0;

		if (ObjectText->Thickness == 0.0)
			ObjectText->Thickness = (float) 0.1;

		if ((fabs(ObjectText->X) > 90000.0) || (fabs(ObjectText->Y) > 90000.0))
			ObjectText->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);
		Pin->Info = 0;
		Pin->AddNr = 0;
		Pin->DeleteNr = 0;

		if ((fabs(Pin->X) > 90000.0) || (fabs(Pin->Y) > 90000.0))
			Pin->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);
		PowerPin->Info = 0;
		PowerPin->AddNr = 0;
		PowerPin->DeleteNr = 0;

		if ((fabs(PowerPin->NameX) > 90000.0) || (fabs(PowerPin->NameY) > 90000.0))
			PowerPin->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);
		PinBus->Info = 0;
		PinBus->AddNr = 0;
		PinBus->DeleteNr = 0;

		if ((fabs(PinBus->X) > 90000.0) || (fabs(PinBus->Y) > 90000.0))
			PinBus->Info = OBJECT_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < MaxNrRedefinedPinBusses; cnt++)
	{
		RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);
		RedefinedPinBus->Info = 0;
		RedefinedPinBus->AddNr = 0;
		RedefinedPinBus->DeleteNr = 0;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetCurrentEditFile(LPSTR FileName)
{
	int32 lengte, mode, cnt, lengte2;
	char str[MAX_LENGTH_STRING];

	if (EditFile[0] == 0)
		return -1;

	lengte = strlen(EditFile);

	if (lengte < 5)
		return -1;

	mode = -1;

	if (stricmp((LPSTR) & EditFile[lengte - 3], "sym") == 0)
		mode = 0;

	if (stricmp((LPSTR) & EditFile[lengte - 3], "sch") == 0)
		mode = 1;

	if (mode == -1)
		return -1;

	cnt = lengte - 5;

	while ((cnt >= 0) && (EditFile[cnt] != '\\'))
		cnt--;

	if (cnt < 1)
		return -1;

	strcpy(str, EditFile);
	str[cnt] = 0;
	lengte2 = strlen(str);

	if (mode == 0)
	{
		if (stricmp((LPSTR) & str[lengte2 - 4], "\\sym") != 0)
			return -1;
	}
	else
	{
		if (stricmp((LPSTR) & str[lengte2 - 4], "\\sch") != 0)
			return -1;
	}

	strcpy(FileName, (LPSTR) & EditFile[cnt - 3]);
	return mode;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 LoadObjects5()
{
	char Object5File[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	int32 lengte, ok, lengte2, count, fp, result;

	strcpy(Object5File, EditFile);
	lengte = strlen(Object5File);
	Object5File[lengte - 3] = 'w';
	Object5File[lengte - 2] = 'i';
	Object5File[lengte - 1] = 'r';

	if (FileExistsUTF8(Object5File) != 0)
		return -1;

	if ((lengte2 = FileSizeUTF8(Object5File)) < 1)
		return -2;

	if ((lengte2 % sizeof(Object5Record)) != 0)
	{
		sprintf(str, SC(473, "Invalid wire file %s -> Run the netlist"), Object5File);
		MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
		return -3;
	}

	count = lengte2 / sizeof(Object5Record);

	if (AllocateMemObjects5(count) != 0)
		return -4;

	if ((fp = FileOpenReadOnlyUTF8(Object5File)) == -1)
		return -5;

	FileRead(fp, Objects5, lengte2, &result);
	FileClose(fp);
	NrObjects5 = count;
	ok = 1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckFile(LPSTR FileName, int32 mode)
{
	int32 result;
	char str[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING];
	DesignRecord Design;
	SymbolRecord DesignSymbol;

	if (FileName != NULL)
		strcpy(FileName2, FileName);
	else
		strcpy(FileName2, EditFile);

	if (FileExistsUTF8(FileName2) == 0)
	{
		if ((Designfp = FileOpenReadOnlyUTF8(FileName2)) == -1)
		{
			sprintf(str, SC(290, "Error in opening file %s"), FileName2);
			MessageBoxUTF8(SCHWindow, str, "", MB_OK);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		if ((mode & 1) == 0)
		{
			if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
			{
				sprintf(str, SC(291, "Error in reading file %s"), FileName2);
				MessageBoxUTF8(SCHWindow, str, "", MB_OK);
				FileClose(Designfp);

				if ((mode & 2) == 2)
					EditFile[0] = 0;

				return -1;
			}

			FileClose(Designfp);

			if ((strcmp(Design.Identification, SheetCode1) != 0) && (strcmp(Design.Identification, SheetCode2) != 0)
			        && (strcmp(Design.Identification, SheetCode3) != 0) && (strcmp(Design.Identification, SheetCode4) != 0)
			        && (strcmp(Design.Identification, SheetCode5) != 0))
			{
				sprintf(str, SC(292, "File %s is not a valid schematic file"), FileName2);
				MessageBoxUTF8(SCHWindow, str, "", MB_OK);

				if ((mode & 2) == 2)
					EditFile[0] = 0;

				return -1;
			}
		}
		else
		{
			if (FileRead(Designfp, &DesignSymbol, sizeof(SymbolRecord), &result) == -1)
			{
				sprintf(str, SC(291, "Error in reading file %s"), FileName2);
				MessageBoxUTF8(SCHWindow, str, "", MB_OK);
				FileClose(Designfp);

				if ((mode & 2) == 2)
					EditFile[0] = 0;

				return -1;
			}

			FileClose(Designfp);

			if ((strcmp(DesignSymbol.SymbolIdent, SymbolCode1) != 0)
			        && (strcmp(DesignSymbol.SymbolIdent, SymbolCode2) != 0)
			        && (strcmp(DesignSymbol.SymbolIdent, SymbolCode3) != 0))
			{
				sprintf(str, SC(293, "File %s is not a valid symbol file"), FileName2);
				MessageBoxUTF8(SCHWindow, str, "", MB_OK);

				if ((mode & 2) == 2)
					EditFile[0] = 0;

				return -1;
			}
		}
	}
	else
		return -2;

	strcpy(EditFile, FileName2);
	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ChangeFile(LPSTR FileName, int32 mode)
{
	/*
	  Mode:

	   bit 0 :
	   bit 1 : Do not prepare viewing of the file
	   bit 2 : Do not load ini file

	*/

	char str[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING];
	int32 lengte, res, CheckResult, EditingMode;

	EditingMode = -1;

	if (FileName != NULL)
		strcpy(FileName2, FileName);
	else
		strcpy(FileName2, EditFile);

	lengte = strlen(FileName2);

	if (lengte > 4)
	{
		if (stricmp((LPSTR) & FileName2[lengte - 3], "sym") == 0)
			EditingMode = 1;

		if (stricmp((LPSTR) & FileName2[lengte - 3], "sch") == 0)
			EditingMode = 0;
	}

	CheckResult = -1;

	if (EditingMode != -1)
	{
		CheckResult = CheckFile(FileName, (mode & 3) | EditingMode);

		if (CheckResult == -1)
		{
			RePaint();
			return -1;
		}
	}

	strcpy(OldIniFile, IniFile);

	if (EditFile[0] != 0)
	{
		EditingSheetSymbol = 0;

		if (EditingMode == 0)
			EditingSymbol = 0;
		else
			EditingSymbol = 1;

		IniFile[0] = 0;

		if (!EditingProtectedSymbol)
		{
			if (GetDirFromFileName(DesignPath, EditFile) == -1)
				strcpy(DesignPath, ExePath);
		}

//    MessageBoxUTF8(NULL,DesignPath,"DesignPath",MB_APPLMODAL+MB_OK);
		strcpy(str, DesignPath);
		strcat(str, "\\sch.ini");

		if (FileExistsUTF8(str) == 0)
			strcpy(IniFile, str);

		if ((lengte = strlen(DesignPath)) > 6)
		{
			if ((stricmp((LPSTR) & DesignPath[lengte - 4], "\\sch") == 0)
			        || (stricmp((LPSTR) & DesignPath[lengte - 4], "\\sym") == 0))
				DesignPath[lengte - 4] = 0;

			strcpy(str, DesignPath);
			strcat(str, "\\sch.ini");

			if (FileExistsUTF8(str) == 0)
				strcpy(IniFile, str);
		}
	}
	else
	{
		switch (EditMode)
		{
		case 0:
			EditingSymbol = 0;
			EditingSheetSymbol = 0;

			if (DesignPath[0] != 0)
			{
				strcpy(str, DesignPath);
				strcat(str, "\\sch.ini");

				if (FileExistsUTF8(str) == 0)
					strcpy(IniFile, str);

				if (ProjectActive)
				{
					strcpy(str, DesignPath);
					strcat(str, "\\sch");

					if (SetCurrentDirectoryUTF8(str))
						SetCurrentDirectoryUTF8(DesignPath);
				}
			}

			break;

		case 1:
		case 2:
			EditingSymbol = 1;

			if (EditMode == 1)
				EditingSheetSymbol = 0;
			else
				EditingSheetSymbol = 1;

			if (DesignPath[0] != 0)
			{
				strcpy(str, DesignPath);
				strcat(str, "\\sch.ini");

				if (FileExistsUTF8(str) == 0)
					strcpy(IniFile, str);

				if (ProjectActive)
				{
					strcpy(str, DesignPath);
					strcat(str, "\\sym");

					if (SetCurrentDirectoryUTF8(str))
						SetCurrentDirectoryUTF8(DesignPath);
				}
			}

			break;
		}
	}

	res = 0;

	if (((OldIniFile[0] == 0) || (stricmpUTF8(OldIniFile, IniFile) != 0)) && (IniFile != NULL) && (IniFile[0] != 0)
		&& ((mode & 4) == 0))
	{
		LoadIniFile(IniFile, 1);

		strcpy(str, ProjectPath);
		strcat(str, "\\sch.ini");

		if (FileExistsUTF8(str) == 0)
			LoadIniFile(str, 2);
	}
	else
	{
//    MessageBoxUTF8(SCHWindow,"No inifile","",MB_OK+MB_APPLMODAL);
	}


	DeAllocateMemDesign();
	res = 0;

	if (CheckResult == -2)
		res = 3;

	EditMode = 0;

	if (EditingSymbol)
	{
#ifdef _WIN64
		SetClassLong(SCHWindow, GCLP_HICON, (LONG)LoadIcon(SCHClass.hInstance, MAKEINTRESOURCE(ICON_SYMBOL)));
#else
		SetClassLong(SCHWindow, GCL_HICON, (LONG) LoadIcon(SCHClass.hInstance, MAKEINTRESOURCE(ICON_SYMBOL)));
#endif

		if (EditingSheetSymbol)
			NewSheetSymbol(res);
		else
			NewSymbol(res);

		FileChanged = 0;

		if (EditFile[0] != 0)
		{
			if ((mode & 2) == 0)
			{
				InsertUsedFile();

				if (CheckResult == 0)
				{
					if (LoadSymbol() != 0)
						MessageBoxUTF8(SCHWindow, EditFile, SC(294, "Error in symbol "), MB_APPLMODAL | MB_OK);
					else
					{
						DeleteGraphicObjects();
						CreateDrawObjects(0);
					}
				}
				else
				{
					DeleteGraphicObjects();
					CreateDrawObjects(0);
				}
			}
			else
				LoadSymbol();
		}
		else
		{
			DeleteGraphicObjects();
			CreateDrawObjects(0);
		}

		InitFile(0);

		if ((mode & 2) == 0)
			SetSymbolMenu();
	}
	else
	{
#ifdef _WIN64
		SetClassLong(SCHWindow, GCLP_HICON, (LONG)LoadIcon(SCHClass.hInstance, MAKEINTRESOURCE(ICON_SYMBOL)));
#else
		SetClassLong(SCHWindow, GCL_HICON, (LONG) LoadIcon(SCHClass.hInstance, MAKEINTRESOURCE(ICON_SHEET)));
#endif
		NewDesign(res);
		FileChanged = 0;

		if (EditFile[0] != 0)
		{
			if ((mode & 2) == 0)
			{
				InsertUsedFile();

				if (CheckResult == 0)
				{
					if (LoadDesign() != 0)
						MessageBoxUTF8(SCHWindow, EditFile, SC(295, "Error in sheet"), MB_APPLMODAL | MB_OK);
					else
					{
						DeleteGraphicObjects();
						CreateDrawObjects(0);
					}
				}
				else
				{
					DeleteGraphicObjects();
					CreateDrawObjects(0);
				}
			}
			else
				LoadDesign();
		}
		else
		{
			DeleteGraphicObjects();
			CreateDrawObjects(0);
		}

		InitFile(0);

		if ((mode & 2) == 0)
			SetSheetMenu();
	}

	if ((mode & 2) == 0)
	{
		AddMenuFiles();
		ViewWholeDesign(0);
		RePaint();

		CheckInputMessages(0);
		CheckInputMessages(0);
		SetWindowName(0);
	}
	else
		RePaint();

//  DrawCrossHair(1);
	DataBaseChanged = 0;
	LastActionNr = 1;
	MaxLastActionNr = 1;
	return 0;
}

//***************************************************************************************************************************
//******************************** otevøít symbol/list **********************************************************************
//***************************************************************************************************************************

int32 LoadNewFile(int32 mode)
{

	char str[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING], NewEditFile[MAX_LENGTH_STRING],
		EditFile2[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5];
//	    str4[MAX_LENGTH_STRING]; //viz dole

	PROCESS_INFORMATION ProcessInfo;
	int32 cnt, CommError, ok, DirPos, length;

	DrawCrossHair(2);
	DrawCrossHair(3);

	memset(&FileInputInfo, 0, sizeof(OPENFILENAME));

	FileInputInfo.lStructSize = sizeof(OPENFILENAME);
	FileInputInfo.hwndOwner = SCHWindow;
	str[0] = 0;
	FileInputInfo.lpstrFile = str;
	FileInputInfo.lpstrFileTitle = NULL;

	memset(str5, 0, sizeof(str5));
	strcpy(str5, SC(296, "Schematic/symbol files")); //nejde UTF8
	strcat(str5, " (*.sch/*.sym)");
	length = strlen(str5);
	strcpy(&str5[length + 1], "*.sch;*.sym");
	FileInputInfo.lpstrFilter = str5;


	if (!EditingSymbol)
	{
		sprintf(str3, "%s\\sch", DesignPath);

		if (chdir(str3) != 0)
			sprintf(str3, "%s", DesignPath);
	}
	else
	{
		sprintf(str3, "%s\\sym", DesignPath);

		if (chdir(str3) != 0)
			sprintf(str3, "%s", DesignPath);
	}

//	sprintf(str4, "Open %s",str3); //nejde UTF8, naèít?se systémov?
//	FileInputInfo.lpstrTitle = (LPSTR) & str4;

	FileInputInfo.lpstrInitialDir = str3;
	FileInputInfo.Flags = OFN_FILEMUSTEXIST | OFN_LONGNAMES | OFN_HIDEREADONLY;
	FileInputInfo.nMaxFile = 300;
	FileInputInfo.nMaxFileTitle = 32;

	if (GetOpenFileName(&FileInputInfo))
	{
		if (mode == 0)
		{
			DirPos = FileInputInfo.nFileOffset;
			memset(&EditPath, 0, sizeof(EditPath));
			memmove(&EditPath, (LPSTR) FileInputInfo.lpstrFile, min(180, (int) DirPos));
			memset(&EditFile2, 0, sizeof(EditPath));
			memmove(&EditFile2, FileInputInfo.lpstrFile, 180);
			ChangeFile(EditFile2, 0);
			return 0;
		}
		else
		{
			memset(&NewEditFile, 0, sizeof(EditPath));
			memmove(&NewEditFile, FileInputInfo.lpstrFile, 180);

			if (ProjectActive)
			{
				cnt = 0;

				while ((cnt < 32)
				        && ((ProjectInfo->FileTypes[cnt] != 1)
				            || (stricmp(ProjectInfo->FileNames[cnt], NewEditFile) != 0)))
					cnt++;

				if (cnt < 32)
				{
					if (ShowWindow(ProjectInfo->WindowHandles[cnt], SW_RESTORE))
						SetForegroundWindow(ProjectInfo->WindowHandles[cnt]);

					return 0;
				}
			}

			sprintf(ExeFile, "%s\\sch.exe", ExePath);

			if (FileExistsUTF8(ExeFile) != 0)
				return 0;

			sprintf(ExeParams, "\"%s\" \"%s\" /a /e \"%s\" /u \"%s\"", ExeFile, NewEditFile, ExePath, ProjectPath);

			if (ProjectActive)
				strcat(FileName, " /o");

			StartupInfo.cb = sizeof(StartupInfo);
			StartupInfo.wShowWindow = SW_SHOW;
			CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
		}
	}
	else
	{
		DrawCrossHair(0);
		CommError = CommDlgExtendedError();

		ok = 1;
		return 0;
	}

	return 0;
}

//**************************************************************************************************************************
//************************************ uložit symbol/schéma ****************************************************************
//**************************************************************************************************************************

int32 SaveFileName(int32 Mode)
{

	char str[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], DefExt[10];
//	str4[MAX_LENGTH_STRING]; //viz dole

	LPSTR Niks;
	int32 CommError, ok, length;
	InstanceRecord *Instance;

	memset(&FileInputInfo, 0, sizeof(OPENFILENAME));

	FileInputInfo.lStructSize = sizeof(OPENFILENAME);
	FileInputInfo.hwndOwner = SCHWindow;
	str[0] = 0;
	strcat(str, EditFile);

	if (EditingSymbol)
		strcpy(DefExt, "sym");
	else
		strcpy(DefExt, "sch");

	FileInputInfo.lpstrDefExt = DefExt;

	if (!EditingSymbol)
	{

		memset(str5, 0, sizeof(str5));
		strcpy(str5, SC(297, "Schematic files"));
		strcat(str5, " (*.sch)");
		length = strlen(str5);
		strcpy(&str5[length + 1], "*.sch");
		FileInputInfo.lpstrFilter = str5;

		sprintf(str3, "%s\\sch", DesignPath);

		if (chdir(str3) != 0)
			sprintf(str3, "%s", DesignPath);

//		sprintf(str4, "Save as %s", str3); //nejde UTF8, naèít?se systémov?
//		FileInputInfo.lpstrTitle = (LPSTR) & str4;

		strcpy(str, "*.sch");
		FileInputInfo.lpstrFile = str;
	}
	else
	{
		memset(str5, 0, sizeof(str5));
		strcpy(str5, SC(298, "Symbol files"));
		strcat(str5, " (*.sym)");
		length = strlen(str5);
		strcpy(&str5[length + 1], "*.sym");
		FileInputInfo.lpstrFilter = str5;

		sprintf(str3, "%s\\sym", DesignPath);

		if (chdir(str3) != 0)
			sprintf(str3, "%s", DesignPath);

//		sprintf(str4, "Save as %s", str3); //nejde UTF8, naèít?se systémov?
//		FileInputInfo.lpstrTitle = (LPSTR) & str4;

		strcpy(str, "*.sym");
		FileInputInfo.lpstrFile = str;
	}

	FileInputInfo.lpstrInitialDir = str3;
	FileInputInfo.Flags = OFN_LONGNAMES | OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST;
	FileInputInfo.nMaxFile = 300;
	FileInputInfo.nMaxFileTitle = 32;

	if (GetSaveFileName(&FileInputInfo))
	{
		if (GetFullPathName(str, 150, EditFile, &Niks) > 0)
		{
			if (EditingSymbol)
			{
				GetFilePartFromFileName(FileName, EditFile);
				CutExtensionFileName(FileName);
				struprUTF8(FileName);
				Instance = FindFirstInstance(0);
				memset(Instance->Value, 0, sizeof(Instance->Value));
				memmove(Instance->Value, &FileName, min(sizeof(Instance->Value) - 1, strlen(FileName)));
				memset(&DesignSymbol.InterfaceName, 0, sizeof(DesignSymbol.InterfaceName));
				memset(&DesignSymbol.Description, 0, sizeof(DesignSymbol.Description));
			}

			return 1;
		}
		else
			return 0;
	}
	else
	{
		CommError = CommDlgExtendedError();

		if (CommError == 0)
			return 2;

		ok = 1;
		return 0;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void LoadDesignIniFile()
{
	int32 fp, Length, ParamMode, Value;
	char LineBuf[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];
	WCHAR CurrentDir[MAX_LENGTH_STRING];

	DisableOnePinNetCheck = 0;
	GetCurrentDirectoryW(MAX_LENGTH_STRING - 50, CurrentDir);

	if (DesignFile[0] == 0)
		return;

	if ((fp = TextFileOpenUTF8(DesignFile)) < 0)
		return;

	ParamMode = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetSpecialString(LineBuf, str1, 0);
			GetSpecialString(LineBuf, str2, 0);

			if (str1[0] == '[')
			{
				ParamMode = 0;

				if (stricmp(str1, "[TopSheet]") == 0)
					ParamMode = 1;

				if (stricmp(str1, "[LayoutFile]") == 0)
					ParamMode = 2;

				if (stricmp(str1, "[Settings]") == 0)
					ParamMode = 3;

				if (stricmp(str1, "[GeometryLibraries]") == 0)
					ParamMode = 4;

				if (stricmp(str1, "[SchematicSymbolLibraries]") == 0)
					ParamMode = 5;
			}
			else
			{
				switch (ParamMode)
				{
				case 1:
//            strcpy(TopSheetName,str1);
					break;

				case 2:
//            strcpy(LayoutFile,str1);
					break;

				case 3:
					if (GetStringValue(str4, str1, str2))
					{
						if (stricmp(str1, "UpdateLayout") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
							{
//                  ProjectInfo->OtherInfos[0]=Value;
							}
						}

						if (stricmp(str1, "DisableOnePinNetCheck") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								DisableOnePinNetCheck = Value;
						}

						if (stricmp(str1, "SaveSymbolsLocally") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SaveSymbolsLocally = Value;
						}
					}

					break;

				case 4:		//  GeometryLibraries
					if (NrGeometryLibraries < 16)
					{
						if (FileExistsUTF8(str1) == 0)
							strcpy(GeometryLibraries[NrGeometryLibraries++], str1);
					}

					break;

				case 5:		//  SchematicSymbolLibraries
					if (NrSchematicSymbolLibraries < 16)
					{
						if (FileExistsUTF8(str1) == 0)
							strcpy(SchematicSymbolLibraries[NrSchematicSymbolLibraries++], str1);
					}

					break;
				}
			}
		}
	}

	TextFileClose(fp);
	SetCurrentDirectoryW(CurrentDir);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
