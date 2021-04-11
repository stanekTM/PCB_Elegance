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
#include "calc.h"
#include "stddef.h"
#include "stdio.h"
#include "windows.h"
#include "memory.h"
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "time.h"
#include "help.h"
#include "files2.h"
#include "nets.h"
#include "direct.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "design.h"
#include "files.h"
#include "resource.h"
#include "stddef.h"
#include "mainloop.h"
#include "utf8.h"
#include "instance.h"
#include "owntime.h"
#include "menus.h"
#include "../functionsc/version.h"

typedef struct
{
	int32 PaperSize;
	int32 PaperOrientation;
	int32 PaperFitToPage;
	int32 Color;
	int32 Invert;
} PDFInfoRecord;

char DialogTextLine[8192];
int32 Designfp, DialogMode, TempMaxNrRefsPerSheet, UpdateLayout, ok;
char DesignPath[MAX_LENGTH_STRING], OrcadDesignPath[MAX_LENGTH_STRING];
OrcadConversionRecord OrcadConversionInfo;

char PDFCreatorOrganisation[MAX_LENGTH_STRING], PDFCreatorName[MAX_LENGTH_STRING], PDFSubject[MAX_LENGTH_STRING],
     PDFTitle[MAX_LENGTH_STRING];

int32 NrPageOutputIds = 11;
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


PDFInfoRecord PDFInfo = { PAPERSIZE_A4, ORIENTATION_AUTO, 1, 0, 0 };

extern int32 WindowStartX, WindowStartY, MaxNrRefsPerSheet;
extern ProjectInfoRecord *ProjectInfo;
extern char ProjectPath[MAX_LENGTH_STRING];
extern HBITMAP BitmapUp, BitmapDown;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadDesignOnly(LPSTR FileName)
{
	int32 result;
	DesignRecord Design;

	if ((Designfp = FileOpenReadOnlyUTF8(FileName)) == -1)
		return -1;

	if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
	{
		FileClose(Designfp);
		return -1;
	}

	FileClose(Designfp);

	if (strcmp((LPSTR) & Design, SheetCode1) != 0)
		return -2;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadSheetInMemory(int32 SheetNr, int32 mode)
{
	int32 result, Designfp, cnt, cnt2, cnt3, cnt4, Libfp, MemPos, cnt5, LoadResult, ok, Pos, Length, TotalSymbolsMem,
	      FilePos, MemPos2, NrErrors, MemPosTemp, MaxMem, SymbolNr, NewSymbolNr, pos, pos2, LengthIdent, LengthValue,
	      Version25, Version30, Version35;
	char SymbolName[MAX_LENGTH_STRING], LibName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING];
	uint8 Properties[512], *PropertiesBuf;
	LPSTR InstanceAttrBuf;
	SymbolsPos2Record *SymbolPos2, *NewSymbolPos2;
	SymbolsPosRecord *SymbolPos;
	SymbolRecord *Symbol;
	InstanceRecord *Instance, *NewInstance;
	OldObjectLineRecord OldObjectLine;
	OldObjectRectRecord OldObjectRect;
	OldObjectCircleRecord OldObjectCircle;
	OldObjectArcRecord OldObjectArc;
	OldObjectTextRecord OldObjectText;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	ObjectRecord *Object;
	Object2Record *Object3, *Object4;
	int32 MemError, Unique, OnlyPowerPins;

	Symbol = NULL;
	MaxMem = Sheets[SheetNr].SheetMemSize + Sheets[SheetNr].SymbolMemSize;
	sprintf(FileName, "%s%s.sch", SheetDir, Sheets[SheetNr].SheetName);

	if ((mode & 1) == 0)
	{
		sprintf(str, SC(25, "Loading sheet\t\t%s\r\n"), FileName);
		AddMessage(str);
	}

	if ((Designfp = FileOpenReadOnlyUTF8(FileName)) == -1)
	{
		if ((mode & 1) == 0)
		{
			sprintf(str, SC(0, "Error in opening file %s\r\n"), FileName);
			AddMessage(str);
		}

		return -1;
	}

	if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
	{
		if ((mode & 1) == 0)
		{
			sprintf(str, SC(1, "Error in reading file %s\r\n"), FileName);
			AddMessage(str);
		}

		return -1;
	}

	if ((stricmp(Design.Identification, SheetCode1) != 0) && (stricmp(Design.Identification, SheetCode2) != 0)
	        && (stricmp(Design.Identification, SheetCode3) != 0) && (stricmp(Design.Identification, SheetCode4) != 0)
	        && (stricmp(Design.Identification, SheetCode5) != 0))
	{
		if ((mode & 1) == 0)
		{
			sprintf(str, SC(2, "%s is not a valid sheet file\r\n"), FileName);
			AddMessage(str);
		}

		FileClose(Designfp);
		return -1;
	}

	if ((mode & 2) == 2)
	{
		FileClose(Designfp);
		return 0;
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

	if (MaxMem > DesignBufMemSize)
	{
		if (AllocateMemDesignBuf(MaxMem) == -1)
			return -1;
	}

	FilePos = FileCurrentPointer(Designfp);
	MemPos = 0;
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);
	SymbolsPos = (SymbolsPosArray *) & DesignBuf[MemPos];
	FilePos = FileCurrentPointer(Designfp);
	MemPos += Design.NrSymbols * sizeof(SymbolsPosRecord);
	Instances = (InstancesArray *) & DesignBuf[MemPos];
	Sheets[SheetNr].InstancePos = MemPos + sizeof(DesignRecord);
	Sheets[SheetNr].InstancePos2 = MemPos + sizeof(DesignRecord);
	Sheets[SheetNr].InstancePos += offsetof(InstanceRecord, Reference);
	Sheets[SheetNr].InstancePos2 += offsetof(InstanceRecord, PackagePartNr);
	Sheets[SheetNr].NrInstances = Design.NrInstances;

	if ((strcmp(Design.Identification, SheetCode1) == 0) || (strcmp(Design.Identification, SheetCode2) == 0))
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			MemPos2 = MemPos;
			memset(&DesignBuf[MemPos2], 0, sizeof(InstanceRecord));
			FileRead(Designfp, &DesignBuf[MemPos2], offsetof(OldOldInstanceRecord, Geometry), &result);
			MemPos2 += offsetof(InstanceRecord, Geometry);
			FileRead(Designfp, &DesignBuf[MemPos2],
			         sizeof(OldOldInstanceRecord) - offsetof(OldOldInstanceRecord, Geometry), &result);
			MemPos += sizeof(InstanceRecord);
		}
	}
	else
	{
		if ((strcmp(Design.Identification, SheetCode3) == 0) || (strcmp(Design.Identification, SheetCode4) == 0))
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				MemPos2 = MemPos;
				memset(&DesignBuf[MemPos2], 0, sizeof(InstanceRecord));
				FileRead(Designfp, &DesignBuf[MemPos2], offsetof(OldInstanceRecord, Geometry), &result);
				MemPos2 += offsetof(InstanceRecord, Geometry);
				FileRead(Designfp, &DesignBuf[MemPos2],
				         sizeof(OldInstanceRecord) - offsetof(OldInstanceRecord, Geometry), &result);
				MemPos += sizeof(InstanceRecord);
			}
		}
		else
		{
			FileRead(Designfp, &DesignBuf[MemPos], Design.NrInstances * sizeof(InstanceRecord), &result);
			MemPos += Design.NrInstances * sizeof(InstanceRecord);
		}
	}

	FileRead(Designfp, &DesignBuf[MemPos], Design.NrWires * sizeof(WireRecord), &result);
	Wires = (WiresArray *) & DesignBuf[MemPos];
	MemPos += Design.NrWires * sizeof(WireRecord);
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrBusses * sizeof(BusRecord), &result);
	Busses = (BussesArray *) & DesignBuf[MemPos];
	MemPos += Design.NrBusses * sizeof(BusRecord);
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrJunctions * sizeof(JunctionRecord), &result);
	Junctions = (JunctionsArray *) & DesignBuf[MemPos];
	MemPos += Design.NrJunctions * sizeof(JunctionRecord);
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrNetLabels * sizeof(NetLabelRecord), &result);
	NetLabels = (NetLabelsArray *) & DesignBuf[MemPos];
	MemPos += Design.NrNetLabels * sizeof(NetLabelRecord);
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrBusConnections * sizeof(BusConnectionRecord), &result);
	BusConnections = (BusConnectionsArray *) & DesignBuf[MemPos];
	MemPos += Design.NrBusConnections * sizeof(BusConnectionRecord);
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrGlobalConnections * sizeof(GlobalConnectionRecord), &result);
	GlobalConnections = (GlobalConnectionsArray *) & DesignBuf[MemPos];
	MemPos += Design.NrGlobalConnections * sizeof(GlobalConnectionRecord);
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrObjectLines * sizeof(ObjectLineRecord), &result);

	if (Version30)
	{
		ObjectLines = (ObjectLinesArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectLines * sizeof(ObjectLineRecord);
		FileRead(Designfp, &DesignBuf[MemPos], Design.NrObjectRects * sizeof(ObjectRectRecord), &result);
		ObjectRects = (ObjectRectsArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectRects * sizeof(ObjectRectRecord);
		FileRead(Designfp, &DesignBuf[MemPos], Design.NrObjectCircles * sizeof(ObjectCircleRecord), &result);
		ObjectCircles = (ObjectCirclesArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectCircles * sizeof(ObjectCircleRecord);
		FileRead(Designfp, &DesignBuf[MemPos], Design.NrObjectArcs * sizeof(ObjectArcRecord), &result);
		ObjectArcs = (ObjectArcsArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectArcs * sizeof(ObjectArcRecord);
		FilePos = FileCurrentPointer(Designfp);
		FileRead(Designfp, &DesignBuf[MemPos], Design.NrObjectTexts * sizeof(ObjectTextRecord), &result);
		ObjectTexts = (ObjectTextsArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectTexts * sizeof(ObjectTextRecord);
	}
	else
	{
		ObjectLines = (ObjectLinesArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectLines * sizeof(ObjectLineRecord);

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			FileRead(Designfp, &OldObjectLine, sizeof(OldObjectLineRecord), &result);
			ObjectLine = &((*ObjectLines)[cnt]);
			ObjectLine->X1 = OldObjectLine.X1;
			ObjectLine->Y1 = OldObjectLine.Y1;
			ObjectLine->X2 = OldObjectLine.X2;
			ObjectLine->Y2 = OldObjectLine.Y2;
			ObjectLine->Info = OldObjectLine.Info;
			ObjectLine->Thickness = STANDARD_LINE_THICKNESS;
		}

		ObjectRects = (ObjectRectsArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectRects * sizeof(ObjectRectRecord);

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			FileRead(Designfp, &OldObjectRect, sizeof(OldObjectRectRecord), &result);
			ObjectRect = &((*ObjectRects)[cnt]);
			ObjectRect->CentreX = OldObjectRect.CentreX;
			ObjectRect->CentreY = OldObjectRect.CentreY;
			ObjectRect->Width = OldObjectRect.Width;
			ObjectRect->Height = OldObjectRect.Height;
			ObjectRect->Info = OldObjectRect.Info;
			ObjectRect->Thickness = STANDARD_LINE_THICKNESS;
		}

		ObjectCircles = (ObjectCirclesArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectCircles * sizeof(ObjectCircleRecord);

		for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
		{
			FileRead(Designfp, &OldObjectCircle, sizeof(OldObjectCircleRecord), &result);
			ObjectCircle = &((*ObjectCircles)[cnt]);
			ObjectCircle->CentreX = OldObjectCircle.CentreX;
			ObjectCircle->CentreY = OldObjectCircle.CentreY;
			ObjectCircle->Diam = OldObjectCircle.Diam;
			ObjectCircle->CircleMode = OldObjectCircle.CircleMode;
			ObjectCircle->Info = OldObjectCircle.Info;
			ObjectCircle->Thickness = STANDARD_LINE_THICKNESS;
		}

		ObjectArcs = (ObjectArcsArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectArcs * sizeof(ObjectArcRecord);

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
			ObjectArc->Thickness = STANDARD_LINE_THICKNESS;
		}

		ObjectTexts = (ObjectTextsArray *) & DesignBuf[MemPos];
		MemPos += Design.NrObjectTexts * sizeof(ObjectTextRecord);

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
			strncpy(ObjectText->Text, OldObjectText.Text, sizeof(ObjectText->Text) - 1);
			ObjectText->Thickness = STANDARD_LINE_THICKNESS;
		}
	}

	FileRead(Designfp, &DesignBuf[MemPos], Design.NrRedefinedPinBusses * sizeof(RedefinedPinBusRecord), &result);
	RedefinedPinBusses = (RedefinedPinBusArray *) & DesignBuf[MemPos];
	MemPos += Design.NrRedefinedPinBusses * sizeof(RedefinedPinBusRecord);
	FileRead(Designfp, &DesignBuf[MemPos], Design.NrOnePinNets * sizeof(OnePinNetRecord), &result);
	OnePinNets = (OnePinNetsArray *) & DesignBuf[MemPos];

	if (Version35)
		MemPos += Design.NrOnePinNets * sizeof(OnePinNetRecord);
	else
		Design.NrOnePinNets = 0;

	FileClose(Designfp);


	for (cnt = 0; cnt < Design.NrSymbols; cnt++)
	{
		SymbolPos = (SymbolsPosRecord *) & ((*SymbolsPos)[cnt]);
		ok = 1;
	}

	SymbolsPos2 = (SymbolsPos2Array *) & DesignBuf[MemPos];
	memset(&DesignBuf[MemPos], 0, 512 * sizeof(SymbolsPos2Record));
	MemPos += 512 * sizeof(SymbolsPos2Record);
	SymbolsMem = (uint8(*)[]) & DesignBuf[MemPos];
	MemPosTemp = MemPos;
	NrErrors = 0;
	Design.NrSymbols = 0;
	TotalSymbolsMem = 0;
	InitLoadedObjects(0);

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		strcpy(SymbolName, Instance->SymbolName);
		SymbolNr = -1;

		for (cnt3 = 0; cnt3 < Design.NrSymbols; cnt3++)
		{
			SymbolPos2 = &((*SymbolsPos2)[cnt3]);

			if (stricmpUTF8(SymbolPos2->SymbolName, Instance->SymbolName) == 0)
				SymbolNr = cnt3;
		}

		if (SymbolNr == -1)
		{
			SymbolPos2 = &((*SymbolsPos2)[Design.NrSymbols]);
			SymbolNr = Design.NrSymbols;
			LoadResult = SearchSymbol(Instance->SymbolName, LibName, &Pos, &Length, 0);

// 0 = Symbol from a library
// 1 = Symbol from global symbol directory
// 2 = Local symbol
// 3 = Local sheet symbol

			if (LoadResult >= 0)
			{
				strcpy(SymbolPos2->SymbolName, SymbolName);

				if ((Libfp = FileOpenReadOnlyUTF8(LibName)) >= 0)
				{
					if ((Pos == -1) || (FileSeek(Libfp, Pos) != -1))
					{
						Symbol = (SymbolRecord *) & ((*SymbolsMem)[TotalSymbolsMem]);

						if (FileRead(Libfp, Symbol, (int32) Length, &result) == -1)
							LoadResult = -1;

						if (result != Length)
							LoadResult = -1;
					}

					FileClose(Libfp);
					SymbolPos2->Pos = TotalSymbolsMem;
					SymbolPos2->Length = Length;
					strcpy(SymbolPos2->LibName, LibName);
					TotalSymbolsMem += Length;

					if (MemPosTemp + TotalSymbolsMem > DesignBufMemSize)
						ok = 1;

					SymbolNr = Design.NrSymbols;
					Instance->AddNr = (int16) SymbolNr;
					Design.NrSymbols++;
				}
				else
				{
					if ((mode & 1) == 0)
					{
						sprintf(str, SC(26, "Symbol %s not found\r\n"), SymbolName);
						AddMessage(str);
					}

					NrErrors++;
				}
			}
			else
			{
				if ((mode & 1) == 0)
				{
					sprintf(str, SC(26, "Symbol %s not found\r\n"), SymbolName);
					AddMessage(str);
				}

				NrErrors++;
			}
		}
		else
			Instance->AddNr = (int16) SymbolNr;

// *******************************************************************************************************

		if (SymbolNr >= 0)
		{
			MemPos = (*SymbolsPos2)[SymbolNr].Pos;
			Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);

			if ((Symbol->Info & OBJECT_PROTECTED) == OBJECT_PROTECTED)
				Instance->Info |= OBJECT_PROTECTED;

			if ((Symbol->Info & SHEET_SYMBOL) == SHEET_SYMBOL)
				Instance->Info |= SHEET_SYMBOL;

			if ((Symbol->Info & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS)
				Instance->Info |= MULTIPLE_SYMBOLS;
		}

// *******************************************************************************************************

		if (((mode & 4) == 4) && ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
		        && ((Instance->Info & (SHEET_SYMBOL)) == 0))
		{
			Instance->Error = 0;

			if ((Instance->Reference[0] != 0)
//         &&
//         (stricmp(Instance->SymbolName,"POWER_CONNECT")!=0)
			        && (stricmp(Instance->Reference, "?") != 0))
			{
#ifdef _DEBUG

				if (stricmp(Instance->Reference, "J300") == 0)
					ok = 1;

#endif
				MemError = 0;

				if ((NrObjects4 >= MaxNrObjects4) && (AllocateMemObjects4(MaxNrObjects4 + 1024) == -1))
					MemError = 1;

				if (!MemError)
				{
					Object4 = &((*Objects4)[NrObjects4]);
					memset(Object4, 0, sizeof(Object2Record));
					Object4->RefNum = ObjectTextToBuf(Instance->Reference);

					if ((Instance->SymbolInfo & NO_GEOMETRY) == 0)
						Object4->Text1 = ObjectTextToBuf(Instance->Geometry);
					else
						Object4->Text1 = -1;

					Object4->Text2 = ObjectTextToBuf(Instance->SymbolName);
					Object4->Text3 = ObjectTextToBuf(Instance->PartNr);
					Object4->Description = ObjectTextToBuf(Instance->PartDescription);
					InstanceAttrBuf = (LPSTR) Instance->Properties;
					pos = 0;

					while ((pos < sizeof(Instance->Properties) - 2) && (InstanceAttrBuf[pos] != 0))
					{
						LengthIdent = strlen((LPSTR) & InstanceAttrBuf[pos]);

						if ((LengthIdent > 0) && (LengthIdent < 64)
						        && (pos + LengthIdent < sizeof(Instance->Properties) - 1))
						{
							pos2 = pos + LengthIdent + 1;
							LengthValue = strlen((LPSTR) & InstanceAttrBuf[pos2]);

							if ((LengthValue > 0) && (LengthValue < 64)
							        && (pos2 + LengthValue < sizeof(Instance->Properties) - 1))
							{


							}

							pos += LengthIdent + LengthValue + 2;
						}
						else
							pos = sizeof(Instance->Properties);
					}

					if (pos > 0)
					{
						memset(Properties, 0, sizeof(Properties));
						memcpy(Properties, Instance->Properties, pos + 1);

						for (cnt2 = 0; cnt2 < pos; cnt2++)
						{
							if (Properties[cnt2] == 0)
								Properties[cnt2] = 0x80;
						}

						Object4->Properties = ObjectTextToBuf((char *) Properties);
						PropertiesBuf = (uint8 *) GetObjectText(Object4->Properties);

						for (cnt2 = 0; cnt2 < pos; cnt2++)
						{
							if (PropertiesBuf[cnt2] == 0x80)
								PropertiesBuf[cnt2] = 0;
						}
					}
					else
						Object4->Properties = -1;

#ifdef _DEBUG

					if (Object4->Text2 < 0)
						ok = 1;

#endif
					Object4->Value = ObjectTextToBuf(Instance->Value);
					Object4->Value = ObjectTextToBuf(Instance->Value);
					Object4->Info2 = Instance->Info;
					Object4->x1 = Instance->OriginX;
					Object4->y1 = Instance->OriginY;
					Object4->Info3 = Instance->PackagePartNr;
					Object4->PlacingOption = Instance->PlacingOption;
					NrObjects4++;
				}
			}

			NrObjects = 0;
			InstancePinsToObject(Instance, 1);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
				MemError = 0;
				Unique = 1;

				for (cnt5 = 0; cnt5 < NrObjects3; cnt5++)
				{
					Object3 = &((*Objects3)[cnt5]);

					if ((stricmpUTF8(GetObjectText(Object3->Text1), Object->Text1) == 0)
					        && (stricmpUTF8(GetObjectText(Object3->Text2), Object->Text2) == 0)
					        && (stricmpUTF8(GetObjectText(Object3->RefNum), Instance->Reference) == 0))
						Unique = 0;
				}

				if (Unique)
				{
					if ((NrObjects3 >= MaxNrObjects3) && (AllocateMemObjects3(MaxNrObjects3 + 128) == -1))
						MemError = 1;

					if (!MemError)
					{
						Object3 = &((*Objects3)[NrObjects3]);
						memset(Object3, 0, sizeof(Object2Record));
						Object3->ObjectType = SYMBOL_POWERPIN;
						Object3->Text1 = ObjectTextToBuf(Object->Text1);	// Power pin net
						Object3->Text2 = ObjectTextToBuf(Object->Text2);	// Power pin names
						Object3->RefNum = ObjectTextToBuf(Instance->Reference);	// Reference
						NrObjects3++;
					}
				}
			}
		}

// *******************************************************************************************************

		if (((mode & 8) == 8) && (SymbolNr >= 0) && (Instance->Info & (SHEET_SYMBOL | OBJECT_PROTECTED)) == 0)
		{
			NewSymbolNr = -1;

			for (cnt3 = 0; cnt3 < NrNewSymbols; cnt3++)
			{
				NewSymbolPos2 = &((*NewSymbolsPos2)[cnt3]);

				if (stricmpUTF8(NewSymbolPos2->SymbolName, Instance->SymbolName) == 0)
					NewSymbolNr = cnt3;
			}

			if (NewSymbolNr == -1)
			{
				SymbolPos2 = &((*SymbolsPos2)[SymbolNr]);
				Length = SymbolPos2->Length;

				if (NrNewSymbols + 1 >= MaxNrNewSymbols)
					AllocateMemNewSymbolsPos2(NrNewSymbols + 256);

				NewSymbolPos2 = &((*NewSymbolsPos2)[NrNewSymbols]);
				strcpy(NewSymbolPos2->SymbolName, SymbolName);

				if (NewSymbolsMemSize + Length > MaxNewSymbolsMemSize)
				{
					if (AllocateMemNewSymbols(NewSymbolsMemSize + Length + 65536) == -1)
						return -1;
				}

				NewSymbolPos2->Pos = NewSymbolsMemSize;
				NewSymbolPos2->Length = Length;
				strcpy(NewSymbolPos2->LibName, SymbolPos2->LibName);
				memmove(&((*NewSymbolsMem)[NewSymbolsMemSize]), Symbol, Length);
				NewSymbolNr = NrNewSymbols;
				NewSymbolsMemSize += Length;
				NrNewSymbols++;
			}

			NrObjects = 0;
			InstancePinsToObject(Instance, 0);

			if (((Symbol->Info & (SHEET_SYMBOL)) == 0) && (NrObjects > 0))
			{
				OnlyPowerPins = 1;

				for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
				{
					Object = &((*Objects)[cnt4]);

					switch (Object->ObjectType)
					{
					case SYMBOL_PIN:
						if (Object->Info2 != POWER_CONNECTION)
							OnlyPowerPins = 0;

						break;

					case SYMBOL_PINBUS:
						OnlyPowerPins = 0;
						break;
					}
				}

				if ((!OnlyPowerPins) && (Instance->Reference[0] != 0))
				{
					if (NrNewInstances >= MaxNrNewInstances)
					{
						if (AllocateMemNewInstances(MaxNrNewInstances + 256) != 0)
							return -1;
					}

					NewInstance = &((*NewInstances)[NrNewInstances]);
					memmove(NewInstance, Instance, sizeof(InstanceRecord));
					NewInstance->AddNr = (int16) NewSymbolNr;
					NewInstance->Info &= MULTIPLE_SYMBOLS;
					NrNewInstances++;
				}
			}
		}

// *******************************************************************************************************

	}

	if (NrErrors > 0)
	{
		ok = 1;
		return -1;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SearchSymbol(LPSTR SymbolName, LPSTR FileName, int32 * Pos, int32 * Length, int32 mode)
{
	int32 cnt, NrLibEntries, Libfp, Symbolfp, result, res, Found;
	LibRecord Lib;
	LibNameRecord *LibName, *LibNames;
	SymbolRecord Symbol;
	char FileStr[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	HANDLE FileSearchHandle;
	WIN32_FIND_DATAW FileInfo;

#ifdef _DEBUG

	if (stricmp(SymbolName, "74HC574") == 0)
		ok = 1;

#endif

	Found = 0;
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

		if ((mode & 1) == 0)
		{
			if ((Symbol.Info & SHEET_SYMBOL) == SHEET_SYMBOL)
				return 3;
			else
				return 2;
		}
		else
		{
			if ((Symbol.Info & SHEET_SYMBOL) == 0)
				return 2;
		}
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
		*Pos = -1;
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
			cnt++;
		else
			return 1;
	}

// *******************************************************************************
// Search global libraries
// findfirst

	if (LibraryPath[0] != 0)
	{
		sprintf(str, "%s\\*.lib", LibraryPath);
		FileSearchHandle = FindFirstFileUTF8(str, &FileInfo);
// findfirst
		res = 1;

		if (FileSearchHandle == INVALID_HANDLE_VALUE)
			res = 0;

		while ((!Found) && (res))
		{
			UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
			sprintf(FileStr, "%s\\%s", LibraryPath, str2);

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
			{
				if (FileSearchHandle != INVALID_HANDLE_VALUE)
					FindClose(FileSearchHandle);

				return 0;
			}
		}

		if (FileSearchHandle != INVALID_HANDLE_VALUE)
			FindClose(FileSearchHandle);
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

	while ((cnt >= 0) && (FileName[cnt] != '.'))
		cnt--;

	if (cnt >= 0)
		return;

	cnt = lengte - 5;

	while ((cnt >= 0) && (FileName[cnt] != '\\'))
		cnt--;

	res = lengte - 4;
	res -= (cnt + 1);
	memmove(SymbolName, &FileName[cnt + 1], res);
	SymbolName[res] = 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadDesign()
{
	int32 result;

	if (DesignFile[0] == 0)
	{
		if (DesignPath[0] == 0)
			strcpy(DesignPath, ProjectPath);

		result =
		    GetNewFileUTF8(DESIGNWindow, NULL, DesignFile, DesignPath, SC(27, "Design files"), NULL,
		                   SC(56, "Open design files"), "dsn", 0);

		if (result != 0)
			return -1;

		GetDirFromFileName(DesignPath, DesignFile);
		SetCurrentDirectoryUTF8(DesignPath);
	}
	else
	{
		GetDirFromFileName(DesignPath, DesignFile);

		if (DirectoryExistsUTF8(DesignPath) == 0)
			SetCurrentDirectoryUTF8(DesignPath);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitLoadedObjects(int32 Mode)
{
	int32 cnt;

	WireRecord *Wire;
	BusRecord *Bus;
	RedefinedPinBusRecord *RedefinedPinBus;
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
	SymbolsPosRecord *SymbolPos;

	for (cnt = 0; cnt < Design.NrSymbols; cnt++)
	{
		SymbolPos = (SymbolsPosRecord *) & ((*SymbolsPos)[cnt]);
		SymbolPos->Info = 0;
		SymbolPos->AddNr = 0;
		SymbolPos->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		Instance->Info = 0;
		Instance->AddNr = -1;
		Instance->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);
		Wire->Info = 0;
		Wire->AddNr = 0;
		Wire->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);
		Bus->Info = 0;
		Bus->AddNr = 0;
		Bus->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);
		Junction->Info = 0;
		Junction->AddNr = 0;
		Junction->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);
		OnePinNet->Info = 0;
		OnePinNet->AddNr = 0;
		OnePinNet->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);
		BusConnection->Info = 0;
		BusConnection->AddNr = 0;
		BusConnection->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);
		GlobalConnection->Info = 0;
		GlobalConnection->AddNr = 0;
		GlobalConnection->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);
		NetLabel->Info = 0;
		NetLabel->AddNr = 0;
		NetLabel->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
		ObjectLine->Info = 0;
		ObjectLine->AddNr = 0;
		ObjectLine->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);
		ObjectRect->Info = 0;
		ObjectRect->AddNr = 0;
		ObjectRect->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);
		ObjectCircle->Info = 0;
		ObjectCircle->AddNr = 0;
		ObjectCircle->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ObjectArc->Info = 0;
		ObjectArc->AddNr = 0;
		ObjectArc->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);
		ObjectText->Info = 0;
		ObjectText->AddNr = 0;
		ObjectText->DeleteNr = 0;
	}

	for (cnt = 0; cnt < Design.NrRedefinedPinBusses; cnt++)
	{
		RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);
		RedefinedPinBus->Info = 0;
		RedefinedPinBus->AddNr = 0;
		RedefinedPinBus->DeleteNr = 0;
	}

	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckExtensionFile(LPSTR FileName)
{
	int32 cnt;

	for (cnt = 0; cnt < (int32) strlen(FileName); cnt++)
	{
		if (FileName[cnt] == '.')
			return 0;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

		if (FileExistsUTF8(BackupFile1) == 0)
		{
			sprintf(BackupFile2, "%s\\backup\\%s_2.%s", DirPart, FilePart, Extension);
			CopyFileUTF8(BackupFile1, BackupFile2, 0);
		}

		sprintf(BackupFile1, "%s\\%s.%s", DirPart, FilePart, Extension);
		sprintf(BackupFile2, "%s\\backup\\%s.%s", DirPart, FilePart, Extension);
		CopyFileUTF8(BackupFile1, BackupFile2, 0);
	}
	else
		return -2;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void LoadSchematicIniFile(int32 mode)
{
	int32 fp, Length, ParamMode, Value;
	char LineBuf[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];
	WCHAR CurrentDirW[MAX_LENGTH_STRING];

	GetCurrentDirectoryW(MAX_LENGTH_STRING - 50, CurrentDirW);
	sprintf(str1, "%s\\sch.ini", DesignPath);

	if ((fp = TextFileOpenUTF8(str1)) < 0)
		return;

	ParamMode = 0;

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
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

				if (stricmp(str1, "[SymbolDirs]") == 0)
					ParamMode = 1;

				if (stricmp(str1, "[Printer]") == 0)
					ParamMode = 2;
			}
			else
			{
				switch (ParamMode)
				{
				case 1:
					if (mode == 0)
					{
						if (NrSymbolDirs < 8)
						{
							if (SetCurrentDirectoryUTF8(str1))
							{
								strcpy(SymbolDirs[NrSymbolDirs], str1);
								NrSymbolDirs++;
							}
							else
								MessageBoxUTF8(NULL, str1, SC(34, "Error in reading directory"), MB_APPLMODAL | MB_OK);
						}
					}

					break;

				case 2:
					if (mode == 1)
					{
						if (GetStringValue(str4, str1, str2))
						{
							if (stricmp(str1, "PrinterName") == 0)
							{
								if ((PrinterName[0] != 0) && (str2[0] != 0))
									strcpy(PrinterName, str2);
							}

							if (stricmp(str1, "PrinterOptions") == 0)
							{
								if (sscanf(str2, "%i", &Value) == 1)
								{
									if (PaperSize == -1)
										PaperSize = Value;
								}
							}
						}
					}

					break;
				}
			}
		}
	}

	TextFileClose(fp);
	SetCurrentDirectoryW(CurrentDirW);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckProjectPath(char* NewProjectPath)
{
	int32 fp, res, result;
	char str[MAX_LENGTH_STRING];

	if (DirectoryExists(NewProjectPath) != 0)
	{
		sprintf(str, SC(18, "Can not find project directory  %s\n\nCreate the directory ?"), NewProjectPath);
		res = MessageBoxUTF8(NULL, str, SC(19, "Error"), MB_APPLMODAL | MB_YESNOCANCEL);

		if (res == IDYES)
			CreateDirectoryUTF8(NewProjectPath);
		else
			return -1;
	}

	CutBackSlashDir(NewProjectPath);
	sprintf(str, "%s\\test.xxx", NewProjectPath);

	if (((fp = FileOpenWriteUTF8(str)) <= 0) || (FileWrite(fp, &fp, 4, &result) < 0))
	{
		sprintf(str, SC(20, "Can not create a test file  %s\\test.xxx\r\n\r\n"), NewProjectPath);
		strcat(str, SC(21, "Maybe the current user does not have the rights to create files\r\n"));
		strcat(str, SC(22, "in the PCB elegance main directory"));
		MessageBoxUTF8(NULL, str, SC(19, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	FileClose(fp);
	DeleteFileUTF8(str);
	return 0;
}

//***********************************************************************************************************
//***********************************************************************************************************

int32 CALLBACK TextInputDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about, TextChanged, res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		if (DialogMode == 0)
			SetWindowTextUTF8(Dialog, SC(271, "Insert component reference"));
		else
			SetWindowTextUTF8(Dialog, SC(272, "Insert component part nr"));

		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & DialogTextLine);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			TextChanged = 0;

			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1,
			                                   (LPARAM) DialogTextLine)) == 0)
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);

			EndDialog(Dialog, 1);	// wm_gettext
			return about;

		case IDCANCEL:
			memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 TextInputDialog(LPSTR TextLine, int32 Mode)
{
	int32 res;

	DialogMode = Mode;
	res =
	    DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_TEXTINPUT), DESIGNWindow,
	              (DLGPROC) TextInputDialog2);
	strcpy(TextLine, DialogTextLine);
	return res;
}

//*******************************************************************************************************
//*******************************************************************************************************

int32 CALLBACK NewProjectDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about, cnt, res;
	WORD *Lengte;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(66, "Create a new project"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(241, "Special settings"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(40, "Project name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(41, "Top sheet name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(42, "User symbol"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(43, "User geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(243, "Libraries"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(242, "Project"));
//		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(44, "Use part numbers"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(217, "Disable one pin net check"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(240, "Save symbols/geometries locally"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));

		DesignName[0] = 0;
		TopSheetName[0] = 0;

		if (DialogMode == 1)
		{
			strcpy(DesignPath, OrcadConversionInfo.OutputDirectory);
			GetFilePartFromFileName(DesignName, OrcadConversionInfo.OrcadFile);
			CutExtensionFileName(DesignName);
			GetFilePartFromFileName(TopSheetName, OrcadConversionInfo.OrcadFile);
			CutExtensionFileName(TopSheetName);
		}

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & DesignPath); 

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) & DesignName);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_SETTEXT, 0, (LPARAM) & TopSheetName);

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) "");
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) "");

		SendDlgItemMessage(Dialog, IDC_CHECK3, BM_SETCHECK, SaveSymbolsLocally, 0);

		if ((ForceDisableOnePinNetCheck) || (DisableOnePinNetCheck))
		{
			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, BST_CHECKED, 0);
			DisableOnePinNetCheck = 1;
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 50,
			                                   (LPARAM) DialogTextLine)) != 0)
			{
				strcpy(DesignName, DialogTextLine);

				if (DesignName[0] == 0)
				{
					MessageBoxUTF8(DESIGNWindow, "", SC(47, "A design name must be specified"), MB_APPLMODAL | MB_OK);
					return about;
				}

				sprintf(DesignPath, "%s\\%s", ProjectPath, DesignName);

				if (SetCurrentDirectoryUTF8(DesignPath))
				{
					if (MessageBoxUTF8
					        (DESIGNWindow, DesignPath, SC(46, "Directory exists, Ok to overwrite"),
					         MB_APPLMODAL | MB_OKCANCEL) == IDCANCEL)
						return about;
				}
			}

			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_GETTEXT, MAX_LENGTH_STRING - 50,
			                                   (LPARAM) DialogTextLine)) != 0)
			{
				strcpy(TopSheetName, DialogTextLine);

				if (TopSheetName[0] == 0)
				{
					MessageBoxUTF8(DESIGNWindow, "", SC(48, "A TopSheet name must be specified"), MB_APPLMODAL | MB_OK);
					return about;
				}
			}

			UsingPartNumbers = 0;

			if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				UsingPartNumbers = 1;

			if (!ForceDisableOnePinNetCheck)
				SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, DisableOnePinNetCheck, 0);
			else
			{
				SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);
				DisableOnePinNetCheck = 1;
			}

			NrSchematicSymbolLibraries = 0;
			Lengte = (uint16 *) & DialogTextLine;

			for (cnt = 0; cnt < 16; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 50;

				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, EM_GETLINE, cnt, (LPARAM) DialogTextLine)) != 0)
				{
					if (NrSchematicSymbolLibraries < 16)
					{
						memset(&SchematicSymbolLibraries[NrSchematicSymbolLibraries], 0,
						       sizeof(SchematicSymbolLibraries[0]));
						strncpy(SchematicSymbolLibraries[NrSchematicSymbolLibraries++], DialogTextLine,
						        min(res, MAX_LENGTH_STRING - 1));
					}
				}
			}

			NrGeometryLibraries = 0;

			for (cnt = 0; cnt < 16; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 50;

				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, EM_GETLINE, cnt, (LPARAM) DialogTextLine)) != 0)
				{
					if (NrGeometryLibraries < 16)
					{
						memset(&GeometryLibraries[NrGeometryLibraries], 0, sizeof(GeometryLibraries[0]));
						strncpy(GeometryLibraries[NrGeometryLibraries++], DialogTextLine,
						        min(res, MAX_LENGTH_STRING - 1));
					}
				}
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
//			Help("make_new_design.htm", 0);
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/make_new_design.html", 0, 0, SW_SHOW);
			break;

		case IDCANCEL:
			memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 NewProjectDialog(int32 Mode)
{
	int32 res;

	DialogMode = Mode;
	res =
	    DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_NEW_PROJECT), DESIGNWindow,
	              (DLGPROC) NewProjectDialog2);
	return res;
}

//*********************************************************************************************************
//*********************************************************************************************************

int32 CALLBACK ProjectDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about, cnt, res;
	WORD *Lengte;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(38, "Project settings"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(40, "Project name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(41, "Top sheet name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(243, "Libraries"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(42, "User symbol"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(43, "User geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(241, "Special settings"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(242, "Project"));
//		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(44, "Use part numbers")); 
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(217, "Disable one pin net check"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(240, "Save symbols/geometries locally"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & LayoutFile);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) & TopSheetName);
		SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, UsingPartNumbers, 0);

		if (!ForceDisableOnePinNetCheck)
			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, DisableOnePinNetCheck, 0);
		else
		{
			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);
			DisableOnePinNetCheck = 1;
		}

		SendDlgItemMessage(Dialog, IDC_CHECK3, BM_SETCHECK, SaveSymbolsLocally, 0);
		memset(&DialogTextLine, 0, sizeof(DialogTextLine));

		for (cnt = 0; cnt < NrSchematicSymbolLibraries; cnt++)
		{
			strcat(DialogTextLine, SchematicSymbolLibraries[cnt]);
			strcat(DialogTextLine, "\r\n");
		}

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		memset(&DialogTextLine, 0, sizeof(DialogTextLine));

		for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
		{
			strcat(DialogTextLine, GeometryLibraries[cnt]);
			strcat(DialogTextLine, "\r\n");
		}

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) DialogTextLine);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			LayoutFile[0] = 0;

			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50,
			                                   (LPARAM) DialogTextLine)) != 0)
				strcpy(LayoutFile, DialogTextLine);

			if (LayoutFile[0] == 0)
			{
				MessageBoxUTF8(DESIGNWindow, "", SC(49, "A Layout filename must be specified"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (!CheckExtensionFile(DialogTextLine))
			{
				MessageBoxUTF8(DESIGNWindow, "", SC(50, "Do not specify an extension"), MB_APPLMODAL | MB_OK);
				return about;
			}

			TopSheetName[0] = 0;

			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 50,
			                                   (LPARAM) DialogTextLine)) != 0)
				strcpy(TopSheetName, DialogTextLine);

			if (TopSheetName[0] == 0)
			{
				MessageBoxUTF8(DESIGNWindow, "", SC(48, "A TopSheet name must be specified"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if (!CheckExtensionFile(TopSheetName))
			{
				MessageBoxUTF8(DESIGNWindow, "", SC(50, "Do not specify an extension"), MB_APPLMODAL | MB_OK);
				return about;
			}

			UsingPartNumbers = 0;

			if (SendDlgItemMessageUTF8(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				UsingPartNumbers = 1;

			DisableOnePinNetCheck = 0;

			if (SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == 1)
				DisableOnePinNetCheck = 1;

			SaveSymbolsLocally = SendDlgItemMessage(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0);
			NrSchematicSymbolLibraries = 0;
			Lengte = (uint16 *) & DialogTextLine;

			for (cnt = 0; cnt < 16; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 50;

				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, EM_GETLINE, cnt, (LPARAM) DialogTextLine)) != 0)
				{
					if (NrSchematicSymbolLibraries < 16)
					{
						memset(&SchematicSymbolLibraries[NrSchematicSymbolLibraries], 0,
						       sizeof(SchematicSymbolLibraries[0]));
						strncpy(SchematicSymbolLibraries[NrSchematicSymbolLibraries++], DialogTextLine,
						        min(res, MAX_LENGTH_STRING - 1));
					}
				}
			}

			NrGeometryLibraries = 0;

			for (cnt = 0; cnt < 16; cnt++)
			{
				memset(DialogTextLine, 0, MAX_LENGTH_STRING);
				*Lengte = MAX_LENGTH_STRING - 50;

				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, EM_GETLINE, cnt, (LPARAM) DialogTextLine)) != 0)
				{
					if (NrGeometryLibraries < 16)
					{
						memset(&GeometryLibraries[NrGeometryLibraries], 0, sizeof(GeometryLibraries[0]));
						strncpy(GeometryLibraries[NrGeometryLibraries++], DialogTextLine,
						        min(res, MAX_LENGTH_STRING - 1));
					}
				}
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
//			Help("edit_design_settings.htm", 0);
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/edit_design_settings.html", 0, 0, SW_SHOW);
			break;

		case IDCANCEL:
			memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 ProjectDialog(int32 Mode)
{
	int32 res;

//  DialogMode=Mode;
	res = DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PROJECT), DESIGNWindow, (DLGPROC) ProjectDialog2);

	if (res == 1)
		SaveUserIniFile(1);

	return res;
}

//*******************************************************************************************************
//*************************** Konfigurovat cesty projektu IDD_DIALOG_PATHS ******************************
//*******************************************************************************************************

int32 CALLBACK ConfigurePathsDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about, res, PathOk, KeyInfo;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], NewProjectPath[MAX_LENGTH_STRING];
	HKEY Key;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(280, "Configure the project paths"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(282, "Project directory"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(283, "Project Path - This may require administrator privileges to set"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(163, "Gerber viewer"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(285, "Path"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(286, "Viewplot ( default )"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(287, "Gerbv"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(284, "Set"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(294, "Language path set"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(295, "English ( default )"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(296, "Other Language"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM)& ProjectPath);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM)& GerbvPath);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM)& LanguagePath);

		if (UseGerbv == 1)
			SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessage(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);

		if (UseLanguage == 1)
			SendDlgItemMessage(Dialog, IDC_RADIO4, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDC_BUTTON1:
			if ((res =
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50,
				(LPARAM)DialogTextLine)) != 0)
			{
				PathOk = 0;
				NewProjectPath[0] = 0;
				strcpy(NewProjectPath, DialogTextLine);

				if (CheckProjectPath(NewProjectPath) != 0)
					PathOk = -2;

				sprintf(str, "Software\\PCB Elegance");

				if ((res = RegCreateKeyEx(HKEY_LOCAL_MACHINE, str, 0, "", REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS,
					NULL, &Key, (LPDWORD)& KeyInfo)) != ERROR_SUCCESS)
					PathOk = -2;

				if ((res = RegSetValueEx(Key, "ProjectDir", 0, REG_EXPAND_SZ, (BYTE *)& NewProjectPath,
					strlen(NewProjectPath) + 1)) != ERROR_SUCCESS)
					PathOk = -2;

				if (RegCloseKey(Key) != ERROR_SUCCESS)
					PathOk = -2;

				if (PathOk == 0)
				{
					if (DesignActive)
					{
						CloseOpenFiles(0);
						SaveDesignIniFile();
						SetWindowName(NULL, 0);
						sprintf(str, SC(9, "Design %s closed\r\n"), DesignFile);
						AddMessage(str);
						AddMessage(SeparatorString);
					}

					DesignActive = 0;
					strcpy(ProjectPath, NewProjectPath);
					SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM)& ProjectPath);
					strcpy(DesignPath, ProjectPath);
					sprintf(UserIniFile, "%s\\user.ini", ProjectPath);
					NrDesigns = 0;
					LoadUserIniFile();
					UpdateFileMenu(1);
				}
			}
			return about;

		case IDOK:

			if (SendDlgItemMessage(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0) == 1)
				UseGerbv = 0;

			if (SendDlgItemMessage(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0) == 1)
			{
				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 50,
					(LPARAM)DialogTextLine)) != 0)
				{
					strcpy(str, DialogTextLine);
					if ((FileExistsUTF8(str) != -1) && (strstr(str, ".exe") != 0))
					{ // Gerbv found
						strcpy(GerbvPath, str);
						UseGerbv = 1;
					}
					else
					{
						sprintf(str2, SC(289, "Cannot find Gerbv at this location %s"), str);
						MessageBoxUTF8(NULL, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
						return about;
					}
				}
			}

			if (SendDlgItemMessage(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0) == 1)
				UseLanguage = 0;

			if (SendDlgItemMessage(Dialog, IDC_RADIO4, BM_GETCHECK, 0, 0) == 1)
			{
				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, MAX_LENGTH_STRING - 50,
					(LPARAM)DialogTextLine)) != 0)
				{
					strcpy(str, DialogTextLine);
					if (FileExistsUTF8(str) != -1)
					{ // Gerbv found
						strcpy(LanguagePath, str);
						UseLanguage = 1;
					}
				}

			}
			else
			{
				strcpy(LanguagePath, "");
				UseLanguage = 0;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
//			Help("..     .htm", 0); 
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/configure_paths.html", 0, 0, SW_SHOW);
			
			break;

		case IDCANCEL:
			memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 ConfigurePathsDialog(int32 Mode)
{
	int32 res;

	//  DialogMode=Mode;
	res = DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PATHS), DESIGNWindow, (DLGPROC)ConfigurePathsDialog2);

	if (res == 1)
		SaveUserIniFile(1);

	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void FillItemColumnsBOM(HWND Dialog, int32 ColumnListBox, int32 ListBox)
{
	int32 cnt, cnt2, TabStops[20];
	char str[MAX_LENGTH_STRING];


	for (cnt = 0; cnt < 20; cnt++)
		TabStops[cnt] = cnt * 50 + 50;

	str[0] = 0;
	SendDlgItemMessage(Dialog, ListBox, LB_SETTABSTOPS, 16, (LPARAM) (LPINT) & TabStops);
	SendDlgItemMessage(Dialog, ColumnListBox, LB_RESETCONTENT, 0, 0);

	for (cnt = 0; cnt < BOMInfo.NrColumnsUsed; cnt++)
	{
		cnt2 = BOMInfo.ColumnsUsed[cnt];

		if (BOMInfo.ColumnStr[cnt2][0] != 0)
		{
			SendDlgItemMessageUTF8(Dialog, ColumnListBox, LB_ADDSTRING, 0, (LPARAM) BOMInfo.ColumnStr[cnt2]);
			strcat(str, BOMInfo.ColumnStr[cnt2]);
		}
		else
			strcat(str, SC(51, "<NotUsed>"));

		if (cnt < 15)
			strcat(str, "\t");
	}

	SendDlgItemMessage(Dialog, ListBox, LB_RESETCONTENT, 0, 0);
	SendDlgItemMessageUTF8(Dialog, ListBox, LB_ADDSTRING, 0, (LPARAM) str);
	SendDlgItemMessageUTF8(Dialog, ListBox, LB_ADDSTRING, 0, (LPARAM) str);
	SendDlgItemMessageUTF8(Dialog, ListBox, LB_ADDSTRING, 0, (LPARAM) str);

}

//*******************************************************************************************************
//*******************************************************************************************************

int32 CALLBACK BOMDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about, cnt, cnt2, res;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(109, "Generate bill of materials"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(221, "Columns"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(222, "Columns exported file component.csv"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(224, "New column"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(223, "Column settings"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(114, "Sort"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(52, "Bill of materials"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(53, "List of components"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON4, SC(28, "<---- Add"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON5, SC(111, "Delete"));

		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETHORIZONTALEXTENT, 1400, 0);

		for (cnt = 0; cnt < MAX_NR_BOM_COLUMNS; cnt++)
		{
			if (BOMInfo.ColumnStr[cnt][0] != 0)
				SendDlgItemMessageUTF8(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) BOMInfo.ColumnStr[cnt]);
		}

		SendDlgItemMessageUTF8(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(31, "Value"));
		SendDlgItemMessageUTF8(Dialog, IDC_LIST4, LB_ADDSTRING, 0, (LPARAM) SC(33, "PartNr"));

		res = SendDlgItemMessage(Dialog, IDC_LIST4, LB_SETCURSEL, BOMInfo.BOMSortNr, 0);
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(245, "pos,value,#,geometrie"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON8, SC(246, "pos,value,#,geometrie,part nr"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON9, SC(247, "pos,value,#,geometrie,part nr,description"));
		SendDlgItemMessage(Dialog, IDC_BUTTON6, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) BitmapUp);
		SendDlgItemMessage(Dialog, IDC_BUTTON7, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) BitmapDown);
		cnt = 0;

		while (cnt < BOMInfo.NrColumnsUsed)
		{
			cnt2 = BOMInfo.ColumnsUsed[cnt];

			if (BOMInfo.ColumnStr[cnt2][0] == 0)
			{
				if (BOMInfo.NrColumnsUsed - cnt > 1)
				{
					memmove(&BOMInfo.ColumnsUsed[cnt], &BOMInfo.ColumnsUsed[cnt + 1],
					        (BOMInfo.NrColumnsUsed - cnt - 1) * 4);
				}

				BOMInfo.NrColumnsUsed--;
			}
			else
				cnt++;
		}

		FillItemColumnsBOM(Dialog, IDC_LIST1, IDC_LIST2);
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, BOMInfo.NrColumnsUsed - 1, 0);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDC_BUTTON1:
			EndDialog(Dialog, 0);
			return about;

		case IDC_BUTTON2:
		case IDC_BUTTON8:
		case IDC_BUTTON9:
			BOMInfo.BOMSortNr = SendDlgItemMessage(Dialog, IDC_LIST4, LB_GETCURSEL, 0, 0);

			switch (WParam)
			{
			case IDC_BUTTON2:
				EndDialog(Dialog, 1);
				return about;

			case IDC_BUTTON8:
				EndDialog(Dialog, 2);
				return about;

			case IDC_BUTTON9:
				EndDialog(Dialog, 3);
				return about;
			}

		case IDC_BUTTON4:		// Add
			if (BOMInfo.NrColumnsUsed < MAX_NR_BOM_COLUMNS)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST3, LB_GETCURSEL, 0, 0);

				if (res != LB_ERR)
				{
					SendDlgItemMessageUTF8(Dialog, IDC_LIST3, LB_GETTEXT, res, (LPARAM) str);

					for (cnt = 0; cnt < BOMInfo.NrColumnsUsed; cnt++)
					{
						cnt2 = BOMInfo.ColumnsUsed[cnt];

						if (strcmpUTF8(BOMInfo.ColumnStr[cnt2], str) == 0)
							break;
					}

					if (cnt == BOMInfo.NrColumnsUsed)
					{
						SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);
						BOMInfo.ColumnsUsed[cnt] = res;
						BOMInfo.NrColumnsUsed++;
					}

					FillItemColumnsBOM(Dialog, IDC_LIST1, IDC_LIST2);
					res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, cnt, 0);
				}
			}

			break;

		case IDC_BUTTON5:		// Delete
			if (BOMInfo.NrColumnsUsed > 0)
			{
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

				if (res != LB_ERR)
				{
					memmove(&BOMInfo.ColumnsUsed[res], &BOMInfo.ColumnsUsed[res + 1],
					        (BOMInfo.NrColumnsUsed - res - 1) * sizeof(int32));
					BOMInfo.NrColumnsUsed--;
					FillItemColumnsBOM(Dialog, IDC_LIST1, IDC_LIST2);
					res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, res, 0);
				}
			}

			break;

		case IDC_BUTTON6:		// Up
			res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if ((res != LB_ERR) && (res > 0))
			{
				cnt = BOMInfo.ColumnsUsed[res - 1];
				BOMInfo.ColumnsUsed[res - 1] = BOMInfo.ColumnsUsed[res];
				BOMInfo.ColumnsUsed[res] = cnt;
				FillItemColumnsBOM(Dialog, IDC_LIST1, IDC_LIST2);
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, res - 1, 0);
			}

			break;

		case IDC_BUTTON7:		// Down
			cnt = 0;

			while ((cnt < MAX_NR_BOM_COLUMNS) && (BOMInfo.ColumnStr[cnt][0] != 0))
				cnt++;

			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if ((res != LB_ERR) && (res < cnt - 1))
			{
				cnt = BOMInfo.ColumnsUsed[res + 1];
				BOMInfo.ColumnsUsed[res + 1] = BOMInfo.ColumnsUsed[res];
				BOMInfo.ColumnsUsed[res] = cnt;
				FillItemColumnsBOM(Dialog, IDC_LIST1, IDC_LIST2);
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, res + 1, 0);
			}

			break;

		case IDCANCEL:
			memset(DialogTextLine, 0, MAX_LENGTH_STRING);
			EndDialog(Dialog, -1);
			return about;

		case IDHELP:
//			Help("bill_of_materials.htm", 0);
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/bill_of_materials.html", 0, 0, SW_SHOW);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 BOMDialog(int32 Mode)
{
	int32 res;

	BillOfMaterials(-1);		// Get unique component properties from all sheets
	res = DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_BOM), DESIGNWindow, (DLGPROC) BOMDialog2);
	return res;
}

//************************************************************************************************************
//**************************** Generovat anotaci list IDD_DIALOG_ANNOTATION *********************************
//************************************************************************************************************

int32 CALLBACK AnnotationDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about, AnnotateLayout, res;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(143, "Generate annotation sheets"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(57, "Standard numbering"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(58, "Numbering per sheet"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(59, "Layout"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(60, "Restart annotation"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(61, "Appending annotation"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(60, "Restart annotation"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON4, SC(61, "Appending annotation"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(290, "Include annotation of the layout components"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(237, "Max number of references per sheet"));
		sprintf(str, "%d", TempMaxNrRefsPerSheet);
		SendDlgItemMessage(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		AnnotateLayout = 0;

		if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
			AnnotateLayout = 1;

		res = SendDlgItemMessage(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 50, (LPARAM) str);

		if (res > 0)
			TempMaxNrRefsPerSheet = atoi(str);

		switch (WParam)
		{
		case IDC_BUTTON1:
			if (!AnnotateLayout)
				EndDialog(Dialog, 0);
			else
				EndDialog(Dialog, 4);

			return about;

		case IDC_BUTTON2:
			if (!AnnotateLayout)
				EndDialog(Dialog, 1);
			else
				EndDialog(Dialog, 5);

			return about;

		case IDC_BUTTON3:
			if (!AnnotateLayout)
				EndDialog(Dialog, 2);
			else
				EndDialog(Dialog, 6);

			return about;

		case IDC_BUTTON4:
			if (!AnnotateLayout)
				EndDialog(Dialog, 3);
			else
				EndDialog(Dialog, 7);

			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;

		case IDHELP:
//			Help("annotation.htm", 0);
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/annotation.html", 0, 0, SW_SHOW);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

int32 AnnotationDialog(int32 Mode, int32 * MaxNrRefsPerSheet)
{
	int32 res;

//  DialogMode=Mode;
	TempMaxNrRefsPerSheet = *MaxNrRefsPerSheet;
	res =
	    DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ANNOTATION), DESIGNWindow,
	              (DLGPROC) AnnotationDialog2);

	if (res != -1)
		*MaxNrRefsPerSheet = TempMaxNrRefsPerSheet;

	return res;
}

//*******************************************************************************************************
//*********************************** O programu IDD_DIALOG_ABOUT ***************************************
//*******************************************************************************************************

int32 CALLBACK AboutDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(62, "About program"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(63, "Design manager PCB Elegance"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, "Web PCB Elegance"); 

		sprintf(str, SC(64, "\r\n Build version %i.%i.%i  ( %s )"), VER_VERSION / 100, VER_VERSION % 100, VER_BUILD, VER_DATE_STR);

#ifdef GCC_COMP
		strcat(str, "\r\n\r\n Compiled with mingw (gcc 4.9.2)");
#endif
#ifdef VC2005
		strcat(str, "\r\n\r\n Compiled with Microsoft Visual Studio 2005");
#endif
#ifdef VC2010
		strcat(str, SC(65, "\r\n\r\n Compiled with Microsoft Visual Studio 2019"));
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
			ShellExecute(0, 0, "http://www.pcbelegance.org", 0, 0, SW_SHOW); 
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

	res = DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ABOUT), DESIGNWindow, (DLGPROC) AboutDialogBody);
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadOrcadFileName(LPSTR OrcadFile, int32 mode)
{
	if (OrcadDesignPath[0] == 0)
		strcpy(OrcadDesignPath, DesignPath);

	if (mode == 0)
	{
		return GetNewFileUTF8(DESIGNWindow, NULL, OrcadFile, OrcadDesignPath, SC(67, "ORCAD file"),
		                      SC(211, "All files"), SC(167, "Open the ORCAD schema file"), "sch", 0);
	}
	else
	{
		return GetNewFileUTF8(DESIGNWindow, NULL, OrcadFile, OrcadDesignPath, SC(67, "ORCAD file"),
		                      SC(211, "All files"), SC(68, "Open the ORCAD library file"), "lib", 0);
	}
}

//*******************************************************************************************************
//*******************************************************************************************************

int32 CALLBACK OrcadConvertDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about, cnt, res, result;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		if (DialogMode == 0)
		{
			SetWindowTextUTF8(Dialog, SC(69, "Convert ORCAD schematic"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(70, "ORCAD top sheetname"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(71, "Copy to project directory"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(72, "Geometry part field nr"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(73, "Geometry conversion file"));
		}
		else
		{
			SetWindowTextUTF8(Dialog, SC(74, "Convert ORCAD library"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(75, "ORCAD library"));
			SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(76, "Destination directory"));
		}

		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) OrcadConversionInfo.OrcadFile);

		if (DialogMode == 0)
		{
			GetFilePartFromFileName(str, OrcadConversionInfo.OrcadFile);
			CutExtensionFileName(str);
			sprintf(OrcadConversionInfo.OutputDirectory, "%s\\%s", ProjectPath, str);
		}
		else
			sprintf(OrcadConversionInfo.OutputDirectory, "%s\\lib", ProjectPath);

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) OrcadConversionInfo.OutputDirectory);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0,
		                       (LPARAM) OrcadConversionInfo.GeometryConversionFileName);

		for (cnt = 0; cnt < 9; cnt++)
		{
			if (cnt == 0)
				SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(77, "Unused"));
			else
			{
				sprintf(str, "%d", cnt);
				SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) str);
			}
		}

		res = SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_SETCURSEL, OrcadConversionInfo.GeometryIndex, 0);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, (WPARAM) 180,
			                                   (LPARAM) OrcadConversionInfo.OrcadFile)) == 0)
			{
				MessageBoxUTF8(DESIGNWindow, "", SC(78, "The ORCAD conversion file must be specified"),
				               MB_APPLMODAL | MB_OK);
				return about;
			}

			if ((res =
			            SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, (WPARAM) 180,
			                                   (LPARAM) OrcadConversionInfo.OutputDirectory)) == 0)
			{
				MessageBoxUTF8(DESIGNWindow, "", SC(79, "The output directory must be specified"),
				               MB_APPLMODAL | MB_OK);
				return about;
			}

			if (DialogMode == 1)
			{
				if ((res =
				            SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, (WPARAM) 180,
				                                   (LPARAM) OrcadConversionInfo.GeometryConversionFileName)) == 0)
					OrcadConversionInfo.GeometryConversionFileName[0] = 0;

				OrcadConversionInfo.GeometryIndex = SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);
			}

			EndDialog(Dialog, 1);
			return about;

		case ID_READ_FILE:
			res = 0;
			result =
			    GetNewFileUTF8(DESIGNWindow, NULL, OrcadConversionInfo.GeometryConversionFileName, DesignPath,
			                   SC(161, "Geometry file"), SC(211, "All files"),
                               SC(80, "Geometry conversion schematic file"), "txt", 0);

			break;

		case IDHELP:
//			Help("conversion_ORCAD.htm", 0);
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/conversion_orcad.html", 0, 0, SW_SHOW);
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

int32 OrcadConversion(int32 mode)
{
	int32 res;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5];
	PROCESS_INFORMATION ProcessInfo;
	STARTUPINFO StartupInfo;

	if (DesignActive)
		strcpy(OrcadConversionInfo.OutputDirectory, DesignPath);

	if (mode == 0)
	{
		if (LoadOrcadFileName(OrcadConversionInfo.OrcadFile, 0) < 0)
			return -1;

		DialogMode = 0;
		res =
		    DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ORCAD_CONV), DESIGNWindow,
		              (DLGPROC) OrcadConvertDialog2);

		if (res == 2)
			return -1;
	}
	else
	{
		if (LoadOrcadFileName(OrcadConversionInfo.OrcadFile, 1) < 0)
			return -1;

		DialogMode = 1;
		res =
		    DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ORCAD_CONV2), DESIGNWindow,
		              (DLGPROC) OrcadConvertDialog2);

		if (res == 2)
			return -1;
	}

	sprintf(ExeFile, "%s\\OrcadConversion.exe", ExePath);

	if (FileExistsUTF8(ExeFile) != 0)
	{
		MessageBoxUTF8(DESIGNWindow, str, SC(5, "Error in finding file"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	if (DirectoryExists(OrcadConversionInfo.OutputDirectory) < 0)
	{
		if (mode == 1)
		{
			sprintf(str, SC(81, "Can not find directory  %s"), OrcadConversionInfo.OutputDirectory);
			MessageBoxUTF8(NULL, str, SC(19, "Error"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		if (DesignActive)
		{
			CloseOpenFiles(0);
			sprintf(str2, SC(9, "Design %s closed\r\n"), DesignFile);
			AddMessage(str);
			AddMessage(SeparatorString);

			SaveDesignIniFile();
			DesignActive = 0;
			UpdateLayout = 0;
			ProjectInfo->OtherInfos[0] = 0;
			SetWindowName(NULL, 0);
		}

		if (NewProjectDialog(1) == 1)
			MakeNewProject(0);
		else
			return -1;
	}

	if (mode == 0)
	{
		sprintf(ExeParams, "\"%s\" /s%s /d%s", ExeFile, OrcadConversionInfo.OrcadFile,
		        OrcadConversionInfo.OutputDirectory);

		if (OrcadConversionInfo.GeometryConversionFileName[0] != 0)
		{
			strcat(str2, " /c");
			strcat(str2, OrcadConversionInfo.GeometryConversionFileName);
		}

		if (OrcadConversionInfo.GeometryIndex != 0)
		{
			sprintf(str3, " /g%d", OrcadConversionInfo.GeometryIndex);
			strcat(str2, str3);
		}

		sprintf(str3, " /w%x /m%d", (int32) DESIGNWindow, ID_ORCAD_EXIT_MESSAGE);
		strcat(str2, str3);
		memset(&StartupInfo, 0, sizeof(StartupInfo));
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);

		if (DesignActive)
		{
			GetFilePartFromFileName(str4, OrcadConversionInfo.OrcadFile);
			CutExtensionFileName(str4);
			strcpy(TopSheetName, str4);
		}
	}
	else
	{
		sprintf(ExeParams, "\"%s\" /l%s /d%s  /w%x /m%d", ExeFile, OrcadConversionInfo.OrcadFile,
		        OrcadConversionInfo.OutputDirectory, (int32) DESIGNWindow, ID_ORCAD_EXIT_MESSAGE);
		memset(&StartupInfo, 0, sizeof(StartupInfo));
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
//    WinExec(str2,SW_SHOW);
	}

	return 0;
}

//*******************************************************************************************************
//********************************** IDD_DIALOG_PDF_OUTPUT **********************************************
//*******************************************************************************************************

int32 CALLBACK ExportPDFDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{

	int32 about, cnt, res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(218, "Print all the schematics to one PDF file"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(35, "Paper options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(266, "Orientation"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(277, "Scale"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(278, "Paper size"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO6, "Auto");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO7, SC(288, "Portrait"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO8, SC(291, "Landscape"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO9, SC(292, "Scale 1X"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO10, SC(293, "Fit to page"));

		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A1");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A2");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A3");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A4");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A5");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B4");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B5");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B4_JIS");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B5_JIS");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "LEGAL");
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "LETTER");

		for (cnt = 0; cnt < NrPageOutputIds; cnt++)
		{
			if (PageOutputIds[cnt] == PDFInfo.PaperSize)
				SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, (WPARAM) cnt, 0);
		}

		switch (PDFInfo.PaperOrientation)
		{
		case ORIENTATION_AUTO:
			SendDlgItemMessage(Dialog, IDC_RADIO6, BM_SETCHECK, 1, 0);
			break;

		case ORIENTATION_PORTRAIT:
			SendDlgItemMessage(Dialog, IDC_RADIO7, BM_SETCHECK, 1, 0);
			break;

		case ORIENTATION_LANDSCAPE:
			SendDlgItemMessage(Dialog, IDC_RADIO8, BM_SETCHECK, 1, 0);
			break;
		}

		if (PDFInfo.PaperFitToPage == 0)
			SendDlgItemMessage(Dialog, IDC_RADIO9, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessage(Dialog, IDC_RADIO10, BM_SETCHECK, 1, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			if (SendDlgItemMessage(Dialog, IDC_RADIO6, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_AUTO;

			if (SendDlgItemMessage(Dialog, IDC_RADIO7, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_PORTRAIT;

			if (SendDlgItemMessage(Dialog, IDC_RADIO8, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_LANDSCAPE;

			if (SendDlgItemMessage(Dialog, IDC_RADIO9, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperFitToPage = 0;

			if (SendDlgItemMessage(Dialog, IDC_RADIO10, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperFitToPage = 1;

			res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

			if (res != -1)
				PDFInfo.PaperSize = PageOutputIds[res];

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
//			Help("print_all_sheets_pdf", 0);
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/print_all_sheets_pdf.html", 0, 0, SW_SHOW);
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

//*******************************************************************************************************************
//************************************** tisk pdf IDD_DIALOG_PDF_OUTPUT *********************************************
//*******************************************************************************************************************

void PrintAllSheets(int32 mode)
{
	char SchematicFile[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING * 5], FileStr[MAX_LENGTH_STRING],
	     str2[MAX_LENGTH_STRING], buf[4096], PageTitles[128][MAX_LENGTH_STRING], ExeFile[MAX_LENGTH_STRING],
	     ExeParams[MAX_LENGTH_STRING * 5];
	int32 cnt, cnt2, cnt3, cnt4, pos, fp =
	    0, fp2, count, res, res2, test, test2, PageSizeUnitsX, PageSizeUnitsY, NrPDFObjectsRootDir, NrPDFObjects, len,
	    SheetPdfObjectStart[256];
	int32Array *PDFObjectPos;
	struct tm *today;
	time_t ltime;
	PROCESS_INFORMATION ProcessInfo;
	STARTUPINFO StartupInfo;

	NrPDFObjects = 0;
	GetDesignSheets();
	LoadSchematicIniFile(1);
	PageSizeUnitsX = 0;
	PageSizeUnitsY = 0;
	AllocateSpecialMem(MEM_PDF_OBJECTS, 64 * 1024 * sizeof(int32), (void *) &PDFObjectPos);

	if (mode == 1)
	{
		res =
		    DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PDF_OUTPUT), DESIGNWindow,
		              (DLGPROC) ExportPDFDialogBody);

		if (res == 2)
			return;

		sprintf(str, "%s\\sch\\%s.pdf", DesignPath, DesignName);

		if (FileExistsUTF8(str) == 0)
		{
			sprintf(str2, SC(112, "File %s exist overwrite ?"), str);

			if (MessageBoxOwn(DESIGNWindow, str2, SC(8, "Message"), MB_OKCANCEL | MB_APPLMODAL) != IDOK)
				return;
		}

		fp = FileOpenWriteUTF8(str);

		if (fp <= 0)
		{
			sprintf(str2, SC(1024, "Can not open file %s"), str);
			MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
			return;
		}

		// 0.35277777
		switch (PDFInfo.PaperSize)
		{
		case PAPERSIZE_A1:
			PageSizeUnitsX = 2381;
			PageSizeUnitsY = 1684;
			break;

		case PAPERSIZE_A2:
			PageSizeUnitsX = 1684;
			PageSizeUnitsY = 1191;
			break;

		case PAPERSIZE_A3:
			PageSizeUnitsX = 1191;
			PageSizeUnitsY = 842;
			break;

		case PAPERSIZE_A4:
			PageSizeUnitsX = 842;
			PageSizeUnitsY = 596;
			break;

		case PAPERSIZE_A5:
			PageSizeUnitsX = 596;
			PageSizeUnitsY = 421;
			break;

		case PAPERSIZE_B4:
			PageSizeUnitsX = 612;
			PageSizeUnitsY = 792;
			break;

		case PAPERSIZE_B5:
			PageSizeUnitsX = 612;
			PageSizeUnitsY = 792;
			break;

		case PAPERSIZE_B4_JIS:
			PageSizeUnitsX = 612;
			PageSizeUnitsY = 792;
			break;

		case PAPERSIZE_B5_JIS:
			PageSizeUnitsX = 612;
			PageSizeUnitsY = 792;
			break;

		case PAPERSIZE_LEGAL:
			PageSizeUnitsX = 1008;
			PageSizeUnitsY = 612;
			break;

		case PAPERSIZE_LETTER:
			PageSizeUnitsX = 792;
			PageSizeUnitsY = 612;
			break;
		}

		WriteToFile(fp, "%PDF-1.2\n");
		WriteToFile(fp, "\x25\xe2\xe3\xcf\xd3\n");
	}

	SheetPdfObjectStart[0] = 0;
	(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);

//  NrSheets=1;
	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(str, SC(219, "Printing sheet %s\r\n"), Sheets[cnt2].SheetName);
		AddMessage(str);

		sprintf(SchematicFile, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);
		sprintf(ExeFile, "%s\\sch.exe", ExePath);

		if (mode == 0)
		{
			sprintf(ExeParams, "\"%s\" /o /e \"%s\" /p \"%s\" /u \"%s\" /n \"%s,%d\" /x \"%s\\%s.dsn\" \"%s\"", ExeFile,
			        ExePath, DesignPath, ProjectPath, PrinterName, PaperSize, DesignPath, DesignName, SchematicFile);
		}
		else
		{
			sprintf(ExeParams,
			        "\"%s\" /o /e \"%s\" /p \"%s\" /u \"%s\" /n \"$PDF$,%d,%d,%d,%d\" /x \"%s\\%s.dsn\" \"%s\"",
			        ExeFile, ExePath, DesignPath, ProjectPath, PDFInfo.PaperSize, PDFInfo.PaperOrientation,
			        PDFInfo.PaperFitToPage, SheetPdfObjectStart[cnt2], DesignPath, DesignName, SchematicFile);
			GetFilePartFromFileName(PageTitles[cnt2], SchematicFile);
			CutExtensionFileName(PageTitles[cnt2]);
		}

		memset(&StartupInfo, 0, sizeof(StartupInfo));
		StartupInfo.cb = sizeof(StartupInfo);
		StartupInfo.wShowWindow = SW_SHOW;
		test = ProjectInfo->PrintingBusy;
		CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);

//    WinExec(str2,SW_SHOW);

//    CheckInputMessages();
//    Sleep(1000);
		test2 = ProjectInfo->PrintingBusy;
		count = 0;

		while ((!ProjectInfo->PrintingBusy) && (count < 400))
		{
//      CheckInputMessages();
			Sleep(10);
			count++;
		}

		count = 0;

		while ((ProjectInfo->PrintingBusy != 0) && (count < 1000))
		{
//      CheckInputMessages();
			Sleep(10);
			count++;
		}

		if ((count == 1000) || (ProjectInfo->PrintingError != 0))
			return;

		ProjectInfo->PrintingBusy = 0;

		if (mode == 0)
		{
			if (ProjectInfo != NULL)
			{
				if (ProjectInfo->PrinterName[0] != 0)
					strcpy(PrinterName, ProjectInfo->PrinterName);

				if (ProjectInfo->PaperSize != -1)
					PaperSize = ProjectInfo->PaperSize;
			}
		}
		else
		{
			strcpy(FileStr, SchematicFile);
			len = strlen(FileStr);
			FileStr[len - 4] = 0;
			strcat(FileStr, ".asc");

			if (FileExistsUTF8(FileStr) != 0)
			{
				FileClose(fp);
				return;
			}

			fp2 = FileOpenReadOnlyUTF8(FileStr);
			FileRead(fp2, &SheetPdfObjectStart[cnt2 + 1], 4, &res);
			NrPDFObjects = SheetPdfObjectStart[cnt2 + 1];
			cnt3 = SheetPdfObjectStart[cnt2 + 1] - SheetPdfObjectStart[cnt2];
			cnt4 = SheetPdfObjectStart[cnt2];
			pos = (*PDFObjectPos)[cnt4];
			FileRead(fp2, &((*PDFObjectPos)[cnt4]), (cnt3 + 1) * sizeof(int32), &res);

			for (cnt = 0; cnt < cnt3 + 1; cnt++)
				(*PDFObjectPos)[cnt4 + cnt] += pos;

			res = 1;

			while (res > 0)
			{
				FileRead(fp2, buf, 4096, &res);

				if (res > 0)
					FileWrite(fp, buf, res, &res2);
			}

			FileClose(fp2);
#ifndef _DEBUG
			DeleteFileUTF8(FileStr);
#endif
		}
	}

	if (mode == 0)
		return;

	NrPDFObjects = SheetPdfObjectStart[NrSheets];
	(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
	NrPDFObjectsRootDir = NrPDFObjects;
	sprintf(str, "%d 0 obj\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\n");
	WriteToFile(fp, "/Type /Pages\n");
	sprintf(str, "/Count %d\n", NrSheets);
	WriteToFile(fp, str);
	str[0] = 0;

	for (cnt = 0; cnt < NrSheets; cnt++)
	{
		if (cnt == 0)
			sprintf(str2, "/Kids [%d 0 R", NrPDFObjectsRootDir + cnt + 1);
		else
			sprintf(str2, " %d 0 R", NrPDFObjectsRootDir + cnt + 1);

		strcat(str, str2);
	}

	strcat(str, "]\n");
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\n");
	WriteToFile(fp, "endobj\n");

// ****************************************************************************************************
	cnt = 0;

	for (cnt3 = 0; cnt3 < NrSheets; cnt3++)
	{
		cnt++;
		(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
		sprintf(str, "%d 0 obj\n", NrPDFObjects);
		WriteToFile(fp, str);
		WriteToFile(fp, "<<\n");
		WriteToFile(fp, "/Type /Page\n");
		sprintf(str, "/Parent %d 0 R\n", NrPDFObjectsRootDir);
		WriteToFile(fp, str);
		sprintf(str, "/Resources << /Procset %d 0 R\n", NrPDFObjectsRootDir + NrSheets * 2 + 4);
		WriteToFile(fp, str);
		sprintf(str, "              /Font << /F1 %d 0 R >>\n", NrPDFObjectsRootDir + NrSheets * 2 + 5);
		WriteToFile(fp, str);
		WriteToFile(fp, "           >>\n");
		sprintf(str, "/MediaBox [0 0 %d %d]\n", PageSizeUnitsX, PageSizeUnitsY);
		WriteToFile(fp, str);
		sprintf(str, "/Contents %d 0 R\n", SheetPdfObjectStart[cnt3] + 1);
		WriteToFile(fp, str);

		str[0] = 0;
		cnt = 0;

		for (cnt4 = SheetPdfObjectStart[cnt3] + 1; cnt4 < SheetPdfObjectStart[cnt3 + 1]; cnt4++)
		{
			cnt++;

			if (cnt4 == SheetPdfObjectStart[cnt3] + 1)
				strcat(str, "/Annots [ ");

			if (strlen(str) + 7 > 80)
			{
				strcat(str, "\r\n");
				WriteToFile(fp, str);
				str[0] = 0;
			}

			sprintf(str2, "%d 0 R ", cnt4 + 1);
			strcat(str, str2);
			/*
			/Annots [ 5 0 R 6 0 R 7 0 R 8 0 R 9 0 R 10 0 R 11 0 R 12 0 R 13 0 R 14 0 R
			15 0 R 16 0 R 17 0 R 18 0 R 19 0 R 20 0 R 21 0 R 22 0 R 23 0 R 24 0 R
			25 0 R 26 0 R 27 0 R 28 0 R 29 0 R 30 0 R 31 0 R 32 0 R 33 0 R 34 0 R
			35 0 R 36 0 R 37 0 R 38 0 R 39 0 R 40 0 R ]
			*/
		}

		if (cnt > 0)
		{
			strcat(str, "]\r\n");
			WriteToFile(fp, str);
		}

		WriteToFile(fp, ">>\n");
		WriteToFile(fp, "endobj\n");
	}

// ****************************************************************************************************
	for (cnt = 0; cnt < NrSheets; cnt++)
	{
		(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
		sprintf(str, "%d 0 obj\n", NrPDFObjects);
		WriteToFile(fp, str);
		WriteToFile(fp, "<<\n");
		sprintf(str, "/Dest [%d 0 R /Fit]\n", NrPDFObjectsRootDir + cnt + 1);
		WriteToFile(fp, str);
		sprintf(str, "/Parent %d 0 R\n", NrPDFObjectsRootDir + NrSheets * 2 + 1);
		WriteToFile(fp, str);
		sprintf(str, "/Title (%s)\n", PageTitles[cnt]);
		WriteToFile(fp, str);

		if (cnt > 0)
		{
			sprintf(str, "/Prev %d 0 R\n", NrPDFObjectsRootDir + NrSheets + cnt);
			WriteToFile(fp, str);
		}

		if ((NrSheets > 1) && (cnt < NrSheets - 1))
		{
			sprintf(str, "/Next %d 0 R\n", NrPDFObjectsRootDir + NrSheets + cnt + 2);
			WriteToFile(fp, str);
		}

		WriteToFile(fp, ">>\n");
		WriteToFile(fp, "endobj\n");
	}

// ****************************************************************************************************

	(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\n");
	WriteToFile(fp, "/Type /Outlines\n");
	sprintf(str, "/First %d 0 R\n", NrPDFObjectsRootDir + NrSheets + 1);
	WriteToFile(fp, str);
	sprintf(str, "/Last %d 0 R\n", NrPDFObjectsRootDir + NrSheets * 2);
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\n");
	WriteToFile(fp, "endobj\n");

// ****************************************************************************************************
	(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\n");
	WriteToFile(fp, "/Type /Catalog\n");
	sprintf(str, "/Pages %d 0 R\n", NrPDFObjectsRootDir);
	WriteToFile(fp, str);
	sprintf(str, "/Outlines %d 0 R\n", NrPDFObjectsRootDir + NrSheets * 2 + 1);
	WriteToFile(fp, str);

	if (NrSheets > 1)
		WriteToFile(fp, "/PageMode /UseOutlines\n");

	WriteToFile(fp, "/PageLayout /SinglePage\n");
	WriteToFile(fp, ">>\n");
	WriteToFile(fp, "endobj\n");

// ****************************************************************************************************
	(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\n");
	WriteToFile(fp, "/CreationDate ");
	time(&ltime);
	today = localtime(&ltime);
	strftime(str, 100, "(D:%Y%m%d%H%M%S)\n", today);
	WriteToFile(fp, str);

	if (PDFCreatorOrganisation[0] != 0)
	{
		sprintf(str, "/Author (%s)\n", PDFCreatorOrganisation);
		WriteToFile(fp, str);
	}

	if (PDFCreatorName[0] != 0)
	{
		sprintf(str, "/Creator (%s)\n", PDFCreatorName);
		WriteToFile(fp, str);
	}

	if (PDFSubject[0] != 0)
	{
		sprintf(str, "/Subject (%s)\n", PDFSubject);
		WriteToFile(fp, str);
	}

	if (PDFTitle[0] != 0)
	{
		sprintf(str, "/Title (%s)\n", PDFTitle);
		WriteToFile(fp, str);
	}
	else
	{
		strcpy(str, DesignFile);
		CutExtensionFileName(str);
		cnt2 = 0;

		for (cnt = 0; cnt < (int32) strlen(str); cnt++)
		{
			if (str[cnt] != '\\')
				str2[cnt2++] = str[cnt];
			else
			{
				str2[cnt2++] = '\\';
				str2[cnt2++] = '\\';
			}
		}

		str2[cnt2] = 0;
		sprintf(PDFTitle, "Schematic files from design %s", str2);
		sprintf(str, "/Title (%s)\n", PDFTitle);
		WriteToFile(fp, str);
	}

	sprintf(str, "/Producer (PCB elegance %d.%d)\n", VER_VERSION / 100, VER_VERSION % 100);
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\n");
	WriteToFile(fp, "endobj\n");
// ****************************************************************************************************
	(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\n");
	WriteToFile(fp, "[ /PDF /Text ]\n");
	WriteToFile(fp, ">>\n");
	WriteToFile(fp, "endobj\n");
// ****************************************************************************************************
	(*PDFObjectPos)[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\n");
	WriteToFile(fp, "/Type /Font\n");
	WriteToFile(fp, "/Subtype /Type1\n");
	WriteToFile(fp, "/Name /F1\n");
	WriteToFile(fp, "/BaseFont /Courier\n");
	WriteToFile(fp, "/Encoding /MacRomanEncoding\n");
	WriteToFile(fp, ">>\n");
	WriteToFile(fp, "endobj\n");
// ****************************************************************************************************

	pos = FileCurrentPointer(fp);
	WriteToFile(fp, "xref\n");
	sprintf(str, "0 %d\n", NrPDFObjects + 1);
	WriteToFile(fp, str);
	WriteToFile(fp, "0000000000 65535 f \n");

	for (cnt = 0; cnt < NrPDFObjects; cnt++)
	{
		sprintf(str, "%010d 00000 n \n", (*PDFObjectPos)[cnt]);
		WriteToFile(fp, str);
	}

	WriteToFile(fp, "trailer\n");
	WriteToFile(fp, "<<\n");
	sprintf(str, "/Size %d\n", NrPDFObjects + 1);
	WriteToFile(fp, str);
	sprintf(str, "/Root %d 0 R\n", NrPDFObjectsRootDir + NrSheets * 2 + 2);
	WriteToFile(fp, str);
	sprintf(str, "/Info %d 0 R\n", NrPDFObjectsRootDir + NrSheets * 2 + 3);
	WriteToFile(fp, str);

	WriteToFile(fp, ">>\n");
	WriteToFile(fp, "startxref\n");
	sprintf(str, "%d\n", pos);
	WriteToFile(fp, str);
	WriteToFile(fp, "%%EOF\n");
	FileClose(fp);
	sprintf(str, SC(220, "Printing finished ( %s\\sch\\%s.pdf )\r\n"), DesignPath, DesignName);
	AddMessage(str);
//  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SaveDesignIniFile()
{
	int32 fp, cnt;
	char Line[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	if ((fp = FileOpenWriteUTF8(DesignFile)) <= 0)
	{
		MessageBoxUTF8(DESIGNWindow, DesignFile, SC(83, "Error in writing file"), MB_APPLMODAL | MB_OK);
		return;
	}

	WriteLn(fp, "");
	strcpy(Line, "[TopSheet]");
	WriteLn(fp, Line);
	WriteLn(fp, "");
	sprintf(Line, "\"%s\"", TopSheetName);
	WriteLn(fp, Line);
	WriteLn(fp, "");
	strcpy(Line, "[LayoutFile]");
	WriteLn(fp, Line);
	WriteLn(fp, "");
	sprintf(Line, "\"%s\"", LayoutFile);
	WriteLn(fp, Line);

	if (NrGeometryLibraries > 0)
	{
		WriteLn(fp, "");
		WriteLn(fp, "[GeometryLibraries]");
		WriteLn(fp, "");

		for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
		{
			sprintf(Line, "\"%s\"", GeometryLibraries[cnt]);
			WriteLn(fp, Line);
		}
	}

	if (NrSchematicSymbolLibraries > 0)
	{
		WriteLn(fp, "");
		WriteLn(fp, "[SchematicSymbolLibraries]");
		WriteLn(fp, "");

		for (cnt = 0; cnt < NrSchematicSymbolLibraries; cnt++)
		{
			sprintf(Line, "\"%s\"", SchematicSymbolLibraries[cnt]);
			WriteLn(fp, Line);
		}
	}

	WriteLn(fp, "");
	WriteLn(fp, "[Settings]");
	WriteLn(fp, "");
	sprintf(Line, "UpdateLayout=%i", ProjectInfo->OtherInfos[0]);
	WriteLn(fp, Line);
	sprintf(Line, "SaveSymbolsLocally=%i", SaveSymbolsLocally);
	WriteLn(fp, Line);
	sprintf(Line, "DisableOnePinNetCheck=%i", DisableOnePinNetCheck);
	WriteLn(fp, Line);

	if ((ExcludeInBOMID[0] != 0) && (ExcludeInBOMValue[0] != 0))
	{
		sprintf(Line, "PropertyExcludeInBOMID=%s", ExcludeInBOMID);
		WriteLn(fp, Line);
		sprintf(Line, "PropertyExcludeInBOMValue=%s", ExcludeInBOMValue);
		WriteLn(fp, Line);
	}

	if (PrinterName[0] != 0)
	{
		sprintf(Line, "PrinterName=\"%s\"", PrinterName);
		WriteLn(fp, Line);
	}

	if (PaperSize != -1)
	{
		sprintf(Line, "PrinterOptions=%i", PaperSize);
		WriteLn(fp, Line);
	}

	for (cnt = 0; cnt < BOMInfo.NrColumnsUsed; cnt++)
	{
		sprintf(Line, "BillOfMaterialColumnUsed%d=%d", cnt + 1, BOMInfo.ColumnsUsed[cnt]);
		WriteLn(fp, Line);
	}

	WriteLn(fp, "");
	FileClose(fp);

	if (WriteLnError != 0)
	{
		sprintf(str2, SC(85, "Could not write to file %s"), DesignFile);
		MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_OK);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void LoadDesignIniFile()
{
	int32 fp, Length, ParamMode, Value, cnt, cnt2, SaveIniFile, FoundColumns;
	char LineBuf[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];

	GetFilePartFromFileName(DesignName, DesignFile);
	CutExtensionFileName(DesignName);

	if ((fp = TextFileOpenUTF8(DesignFile)) < 0)
		return;

	memset(&BOMInfo.ColumnStr, 0, sizeof(BOMInfo.ColumnStr));
	BOMInfo.NrColumnsUsed = 0;
	FoundColumns = 0;
	ParamMode = 0;
	TopSheetName[0] = 0;
	LayoutFile[0] = 0;
	NrGeometryLibraries = 0;
	NrSchematicSymbolLibraries = 0;
	memset(&GeometryLibraries, 0, sizeof(GeometryLibraries));
	memset(&SchematicSymbolLibraries, 0, sizeof(SchematicSymbolLibraries));
	cnt2 = 0;

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
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
					strcpy(TopSheetName, str1);
					break;

				case 2:
//            sprintf(str2,"%s\\pcb\\%s",DesignPath,str1);
					strcpy(LayoutFile, str1);
					break;

				case 3:
					if (GetStringValue(str4, str1, str2))
					{
						if (stricmp(str1, "UpdateLayout") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								ProjectInfo->OtherInfos[0] = Value;
						}

						if (stricmp(str1, "SaveSymbolsLocally") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								SaveSymbolsLocally = Value;
						}

						if (stricmp(str1, "DisableOnePinNetCheck") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								DisableOnePinNetCheck = Value;
						}

						if (stricmp(str1, "MaxNrRefsPerSheet") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								MaxNrRefsPerSheet = Value;
						}

						if (stricmp(str1, "PrinterName") == 0)
							strcpy(PrinterName, str2);

						if (stricmp(str1, "PrinterOptions") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								PaperSize = Value;
						}

						for (cnt = 0; cnt < MAX_NR_BOM_COLUMNS; cnt++)
						{
							sprintf(str4, "BillOfMaterialColumnUsed%d", cnt + 1);

							if (stricmp(str1, str4) == 0)
							{
								if (sscanf(str2, "%i", &Value) == 1)
								{
									BOMInfo.ColumnsUsed[BOMInfo.NrColumnsUsed] = Value;
									BOMInfo.NrColumnsUsed++;
								}
							}
						}

						if (stricmp(str1, "PropertyExcludeInBOMID") == 0)
							strcpy(ExcludeInBOMID, str2);

						if (stricmp(str1, "PropertyExcludeInBOMValue") == 0)
							strcpy(ExcludeInBOMValue, str2);
					}

					break;

				case 4:
					if (NrGeometryLibraries < 16)
					{
						if (FileExistsUTF8(str1) == 0)
							strcpy(GeometryLibraries[NrGeometryLibraries++], str1);
					}

					break;

				case 5:
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
	SaveIniFile = 0;

	/*
	  if (Features & SAVESYMBOLSLOCALLY_FEATURE) {
	    SaveSymbolsLocally=1;
	    SaveIniFile=1;
	    DisableOnePinNetCheck=1;
	  }
	*/
	if (ForceDisableOnePinNetCheck)
	{
		DisableOnePinNetCheck = 1;
		SaveIniFile = 1;
	}

	if (SaveIniFile)
		SaveDesignIniFile();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SaveUserIniFile(int32 mode)
{
	int32 fp, cnt;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], Line[MAX_LENGTH_STRING];

	if (DesignActive)
		SaveDesignIniFile();

// ***********************************************************************************

	if ((fp = FileOpenWriteUTF8(UserIniFile)) <= 0)
	{
		MessageBoxUTF8(DESIGNWindow, UserIniFile, SC(83, "Error in writing file"), MB_APPLMODAL | MB_OK);
		return;
	}

	strcpy(Line, "[Settings]");
	WriteLn(fp, Line);
	WriteLn(fp, "");
	sprintf(Line, "WindowWidth=%i", WindowWidth);
	WriteLn(fp, Line);
	sprintf(Line, "WindowHeight=%i", WindowHeight);
	WriteLn(fp, Line);
	WriteLn(fp, "");
	sprintf(Line, "WindowStartX=%i", WindowStartX);
	WriteLn(fp, Line);
	sprintf(Line, "WindowStartY=%i", WindowStartY);
	WriteLn(fp, Line);
	sprintf(Line, "UseGerbv=%i", UseGerbv);
	WriteLn(fp, Line);
	sprintf(Line, "GerbvPath=\"%s\"", GerbvPath);
	WriteLn(fp, Line);
	sprintf(Line, "UseLanguage=%i", UseLanguage);
	WriteLn(fp, Line);
	sprintf(Line, "LanguagePath=\"%s\"", LanguagePath);
	WriteLn(fp, Line);
	sprintf(Line, "");
	WriteLn(fp, "");

	strcpy(Line, "[LastDesigns]");
	WriteLn(fp, Line);
	WriteLn(fp, "");

	for (cnt = 0; cnt < NrDesigns; cnt++)
	{
		if (stricmpUTF8(LastDesigns[cnt], DesignFile) == 0)
		{
			sprintf(str, "\"%s\"", LastDesigns[cnt]);
			WriteLn(fp, str);
		}
	}

	for (cnt = 0; cnt < NrDesigns; cnt++)
	{
		if (stricmpUTF8(LastDesigns[cnt], DesignFile) != 0)
		{
			sprintf(str, "\"%s\"", LastDesigns[cnt]);
			WriteLn(fp, str);
		}
	}

	WriteLn(fp, "");

	FileClose(fp);

	if (WriteLnError != 0)
	{
		sprintf(str2, SC(85, "Could not write to file %s"), FileName);
		MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void LoadUserIniFile()
{
	int32 fp, Length, ParamMode, Value, res;
	char LineBuf[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], CurrentDir[MAX_LENGTH_STRING],
	     str4[MAX_LENGTH_STRING];

	if ((fp = TextFileOpenUTF8(UserIniFile)) < 0)
		return;

	ParamMode = 0;

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
	{
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetSpecialString(LineBuf, str1, 0);
			GetSpecialString(LineBuf, str2, 0);

			if (str1[0] == '[')
			{
				ParamMode = 0;

//        if (stricmp(str1,"[ExeDirectory]")==0) ParamMode=1;
//        if (stricmp(str1,"[ProjectPath]")==0) ParamMode=2;
//        if (stricmp(str1,"[SymbolDirs]")==0) ParamMode=3;
//        if (stricmp(str1,"[GeometryLibraryPath]")==0) ParamMode=4;
				//      if (stricmp(str1,"[SchematicSymbolLibraryPath]")==0) ParamMode=5;
				if (stricmp(str1, "[LastDesigns]") == 0)
					ParamMode = 1;

				if (stricmp(str1, "[Settings]") == 0)
					ParamMode = 2;
			}
			else
			{
				switch (ParamMode)
				{
				case 1:
					if (NrDesigns < 16)
					{
						res = sizeof(LastDesigns[0]);

						if (FileExistsUTF8(str1) == 0)
						{
							strncpy(LastDesigns[NrDesigns], str1, res - 1);
							LastDesigns[NrDesigns][res - 1] = 0;
							NrDesigns++;
						}
					}

					break;

				case 2:
					if (GetStringValue(str4, str1, str2))
					{
						if (stricmp(str1, "WindowWidth") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowWidth = Value;
						}

						if (stricmp(str1, "WindowHeight") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowHeight = Value;
						}

						if (stricmp(str1, "WindowStartX") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowStartX = Value;
						}

						if (stricmp(str1, "WindowStartY") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								WindowStartY = Value;
						}

						if (stricmp(str1, "UseGerbv") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								UseGerbv = Value;
						}

						if (stricmp(str1, "GerbvPath") == 0)
						{
							if ((FileExistsUTF8(str2) != -1) && (strstr(str2, ".exe") != 0))
							{ // Gerbv found
								strcpy(GerbvPath, str2);
							}
						}

						if (stricmp(str1, "UseLanguage") == 0)
						{
							if (sscanf(str2, "%i", &Value) == 1)
								UseLanguage = Value;
						}

						if (stricmp(str1, "LanguagePath") == 0)
						{
							if (FileExistsUTF8(str2) != -1)
							{ // Gerbv found
								strcpy(LanguagePath, str2);
							}
							else
								strcpy(LanguagePath, "");
						}

					}

					break;

				case 3:
					break;

				case 4:
					break;

				case 5:
					break;

				case 6:
					break;
				}
			}
		}
	}

	TextFileClose(fp);
	SetCurrentDirectoryUTF8(CurrentDir);
	sprintf(LibraryPath, "%s\\lib", ProjectPath);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddMessage(LPSTR str)
{
	int32 res, start, eind;

	res = SendMessage(EditWindow, EM_GETSEL, (WPARAM) & start, (LPARAM) & eind);

	if (start > 200000)
	{
		SendMessage(EditWindow, EM_SETSEL, (WPARAM) 0, -1);
		SendMessage(EditWindow, WM_CLEAR, 0, 0);
	}

	SendMessage(EditWindow, EM_SETSEL, (WPARAM) - 1, 0);
	SendMessage(EditWindow, EM_SCROLLCARET, 0, 0);
	SendMessageUTF8(EditWindow, EM_REPLACESEL, 0, (LPARAM) str);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
