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
#include "stdio.h"
#include "windows.h"
#include "memory.h"
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "calc.h"
#include "calc4.h"
#include "calcdef.h"
#include "time.h"
#include "fcntl.h"
#include "direct.h"
#include "errno.h"
#include "sys/stat.h"
#include "pcb.h"
#include "insdel.h"
#include "files2.h"
#include "files.h"
#include "dialogs.h"
#include "menus.h"
#include "files2.h"
#include "graphics.h"
#include "mainloop.h"
#include "resource.h"
#include "date.h"
#include "owntime.h"
#include "calc3.h"

#define PCB_TOP                                 5000
#define PCB_BOTTOM                              5500


int32 Designfp;

int32 TempMaxNrHorTraces[32], TempMaxNrVerTraces[32], TempMaxNrDiag1Traces[32], TempMaxNrDiag2Traces[32],
      TempMaxNrNetsPos[32], TempMaxNrConnections, TempMaxNrConnectionsPos, TempMaxNrVias, TempMaxNrViasPos, TempMaxNrNets,
      TempMaxNrComps, TempMaxNrShapes, TempMaxNrAreaFills, TempMaxNrObjectLines, TempMaxNrObjectRects,
      TempMaxNrObjectCircles, TempMaxNrObjectArcs, TempMaxNrObjectTexts, TempMaxNrObjectTexts2, TempMaxCompsMemory,
      TempMaxShapesMemory, TempMaxAreaFillMemory, TempMaxNrObjectPolygons, TempMaxObjectPolygonMemory;

HGLOBAL DesignSaveMemGlobal;

uint8 *DesignSaveMem;
int32 MaxDesignSaveMem, ok;

OldObjectLineRecord OldObjectLine;
OldObjectRectRecord OldObjectRect;
OldObjectCircleRecord OldObjectCircle;
OldObjectArcRecord OldObjectArc;
OldObjectTextRecord2 OldObjectText2;
OldCompRecord OldComp;
OldNetRecord OldNet;

char CurrentPath[MAX_LENGTH_STRING];

extern char StartDir[MAX_LENGTH_STRING], IniFile[MAX_LENGTH_STRING];
extern int32 MakeNewDesign, ProjectActive, CompMemoryError;

extern int32 ProjectIndexNr, RedrawAbsPosStrCnt, SpecialDebugFile;
extern ProjectInfoRecord *ProjectInfo;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadGatePinSwapInfo(int32 mode);

void LoadIniFile(LPSTR FileName, int32 mode);


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 LoadShape(LPSTR ShapeName)
{
	int32 MemPos, cnt, cnt2, SizeFile, Pos, NrLibEntries, NrGeometryLibFiles, *ShapeTypeP, ShapeType, NrLines, Shapefp,
	      result, res, Libfp;
	int32 Found;
	ShapeLinesArray *ShapeLines;
	char str[MAX_LENGTH_STRING], GeometryLibNames[64][MAX_LENGTH_STRING], SearchFileName[MAX_LENGTH_STRING],
	     str2[MAX_LENGTH_STRING];
	ShapeInfoRecord *ShapeInfo;
	ShapeRecord *Shape;
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;
	LibRecord Lib;
	LibNameRecord LibName;

	SizeFile = 0;
#ifdef _DEBUG

	if (stricmpOwn(ShapeName, "k0805") == 0)
		ok = 1;

#endif
	Found = 0;
	cnt2 = 0;

	while ((cnt2 < Design.NrShapes) && (!Found))
	{
		ShapeInfo = &((*Shapes)[cnt2]);

		if (stricmpUTF8(ShapeInfo->ShapeName, ShapeName) == 0)
			Found = 1;
		else
			cnt2++;
	}

	Pos = -1;

	if (Found)
		return cnt2;

// TextBuf

	sprintf(str, "%s\\pcb\\shapes\\%s.shp", DesignPath, ShapeName);

	if (FileExistsUTF8(str) != 0)
	{
		sprintf(str, "%s\\shapes\\%s.shp", ProjectPath, ShapeName);

		if (FileExistsUTF8(str) != 0)
		{

// ************************************************************************************
// ************************************************************************************
// Check shape libraries

			// Search in own libraries first
			NrGeometryLibFiles = 0;

			for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
			{
				if (NrGeometryLibFiles < 64)
					strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt]);
			}

			sprintf(SearchFileName, "%s\\shplib\\*.slb", ProjectPath);
			FileSearchHandle = FindFirstFileUTF8(SearchFileName, &FileInfo);
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

			cnt2 = 0;

			while ((!Found) && (cnt2 < NrGeometryLibFiles))
			{
				if ((Libfp = FileOpenReadOnlyUTF8(GeometryLibNames[cnt2])) <= 0)
					return -1;

				if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
					return -1;

				if (strcmp(Lib.Identification, LibraryCode1) == 0)
				{
					NrLibEntries = Lib.NrLibEntries;
					cnt = 0;

					while ((!Found) && (cnt < NrLibEntries))
					{
						if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
							return -1;

						if (stricmpOwn(LibName.Text, ShapeName) == 0)
						{
							Found = 1;
							Pos = LibName.Pos;
							SizeFile = LibName.Length;
							strcpy(str, GeometryLibNames[cnt2]);
						}

						cnt++;
					}
				}

				if (FileClose(Libfp) == -1)
					return -1;

				cnt2++;
			}

			if (!Found)
				return -1;
		}
	}

	if (Pos == -1)
		SizeFile = FileSizeUTF8(str);

	if (Design.NrShapes + 2 >= MaxNrShapes)
	{
		if (AllocateMemShapes(Design.NrShapes + 32) != 0)
			return -1;
	}

	if (Design.ShapesMem + SizeFile >= MaxShapesMemory)
	{
		if (AllocateMemShapesMemory(Design.ShapesMem + SizeFile + 32768) != 0)
			return -1;
	}

	if (Pos == -1)
	{
		SizeFile = FileSizeUTF8(str);

		if ((Shapefp = FileOpenReadOnlyUTF8(str)) == -1)
			return -2;

		if (FileRead(Shapefp, &ShapesMem[Design.ShapesMem], SizeFile, &result) == -1)
			return -2;

		FileClose(Shapefp);
	}
	else
	{
		if ((Shapefp = FileOpenReadOnlyUTF8(str)) == -1)
			return -2;

		FileSeek(Shapefp, Pos);

		if (FileRead(Shapefp, &ShapesMem[Design.ShapesMem], SizeFile, &result) == -1)
			return -2;

		FileClose(Shapefp);
	}

	Shape = (ShapeRecord *) & ShapesMem[Design.ShapesMem];

	if (stricmp(Shape->Identification, ShapeCode) == 0)
	{	// Old shape definition
		Shape->NrLayers = 2;
		Shape->NrPolygons = 0;
		Shape->PolygonOffset = 0;
		Shape->Dummy2 = 0;
	}

#ifdef _DEBUG

	if (stricmpOwn(ShapeName, "k0805") == 0)
		ok = 1;

#endif
	strcpy(Shape->ShapeName, ShapeName);
	ShapeInfo = &((*Shapes)[Design.NrShapes]);
	strcpy(ShapeInfo->ShapeName, ShapeName);
	ShapeInfo->ShapePos = Design.ShapesMem;
	ShapeInfo++;
	ShapeInfo->ShapePos = Design.ShapesMem + SizeFile;

// *******************************************************************************************************
// *******************************************************************************************************

	if (stricmp(Shape->Identification, ShapeCode) == 0)
	{
		MemPos = Design.ShapesMem;
		MemPos += Shape->CompOutLineOffset;
		NrLines = min(16384, Shape->NrCompOutLines);

		while (NrLines > 0)
		{
			ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
			ShapeTypeP = (int32 *) ShapeLines;
			ShapeType = *ShapeTypeP;
			ShapeType &= 0x0000ff00;

			switch (ShapeType)
			{
			case OBJECT_LINE:	// line
				MemPos += 24;
				break;

			case OBJECT_RECT:	// rect
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_ARC:	// arc
				if ((*ShapeLines)[9] == 0.0)
					(*ShapeLines)[9] = 10.0;

				MemPos += 40;
				break;

			case OBJECT_TEXT:	// text
				MemPos += 24 + 64;
				break;
			}

			NrLines--;
		}

// *******************************************************************************************************
		MemPos = Design.ShapesMem;
		MemPos += Shape->SilkScreenOffset;
		NrLines = min(16384, Shape->NrSilkScreenOutLines);

		while (NrLines > 0)
		{
			ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
			ShapeTypeP = (int32 *) ShapeLines;
			ShapeType = *ShapeTypeP;
			ShapeType &= 0x0000ff00;

			switch (ShapeType)
			{
			case OBJECT_LINE:	// line
				MemPos += 24;
				break;

			case OBJECT_RECT:	// rect
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_ARC:	// arc
				if ((*ShapeLines)[9] == 0.0)
					(*ShapeLines)[9] = 10.0;

				MemPos += 40;
				break;

			case OBJECT_TEXT:	// text
				MemPos += 24 + 64;
				break;
			}

			NrLines--;
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	if (Shape->ShapeNameRotation == 1)
		Shape->ShapeNameRotation = 90;

	Design.ShapesMem += SizeFile;
	Design.NrShapes++;
	return (Design.NrShapes - 1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 LoadShapeDirectory(LPSTR ShapeName, LPSTR ShapeFileName, int32 * Pos, int32 * Length)
{
	int32 cnt, cnt2, SizeFile, NrLibEntries, NrGeometryLibFiles, result, res, Libfp;
	char str[MAX_LENGTH_STRING], GeometryLibNames[64][MAX_LENGTH_STRING], SearchFileName[MAX_LENGTH_STRING],
	     str2[MAX_LENGTH_STRING];
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;
	LibRecord Lib;
	LibNameRecord LibName;

	SizeFile = 0;
#ifdef _DEBUG

	if (stricmpOwn(ShapeName, "k0805") == 0)
		ok = 1;

#endif

	sprintf(str, "%s\\pcb\\shapes\\%s.shp", DesignPath, ShapeName);

	if (FileExistsUTF8(str) == 0)
	{
		*Pos = 0;
		*Length = FileSizeUTF8(str);
		strcpy(ShapeFileName, str);
		return 0;
	}

	sprintf(str, "%s\\shapes\\%s.shp", ProjectPath, ShapeName);

	if (FileExistsUTF8(str) == 0)
	{
		*Pos = 0;
		*Length = FileSizeUTF8(str);
		strcpy(ShapeFileName, str);
		return 0;
	}

// ************************************************************************************
// ************************************************************************************
// Check shape libraries

	// Search in own libraries first
	NrGeometryLibFiles = 0;

	for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
	{
		if (NrGeometryLibFiles < 64)
			strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt]);
	}

	sprintf(SearchFileName, "%s\\shplib\\*.slb", ProjectPath);

	FileSearchHandle = FindFirstFileUTF8(SearchFileName, &FileInfo);
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

	cnt2 = 0;

	while (cnt2 < NrGeometryLibFiles)
	{
		if ((Libfp = FileOpenReadOnlyUTF8(GeometryLibNames[cnt2])) <= 0)
			return -1;

		if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
			return -1;

		if (strcmp(Lib.Identification, LibraryCode1) == 0)
		{
			NrLibEntries = Lib.NrLibEntries;
			cnt = 0;

			while (cnt < NrLibEntries)
			{
				if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
					return -1;

				if (stricmpUTF8(LibName.Text, ShapeName) == 0)
				{
					*Pos = LibName.Pos;
					*Length = LibName.Length;
					FileClose(Libfp);
					strcpy(ShapeFileName, GeometryLibNames[cnt2]);
					return 0;
				}

				cnt++;
			}
		}

		FileClose(Libfp);
		cnt2++;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadShapes(int32 mode)
{
	int32 cnt, ok, MemPos, res;
	int32 First = 1;
	char str2[MAX_LENGTH_STRING];
	ShapeRecord *Shape;
	CompRecord *Comp;

	Design.NrShapes = 0;

	if (mode == 0)
		Design.ShapesMem = 0;

	MessageBufPos = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->ShapeNr = -1;

		if (Comp->Name[0] != 0)
		{
			res = LoadShape(Comp->ShapeName);

			if (res < 0)
			{
				if (First)
				{
					if (AddToMessageBuf(SC(378, "The following geometries were not found\r\n\r\n")) != 0)
						return 0;

					First = 0;
				}

				strcpy(str2, Comp->Name);
				strcat(str2, "\t");
				strcat(str2, Comp->ShapeName);

				if (AddToMessageBuf(str2) != 0)
					return 0;
			}
			else
			{
				Comp->ShapeNr = (int16) res;
				MemPos = (*Shapes)[res].ShapePos;
				Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
				Comp->NrPins = (int16) Shape->NrPins;
			}
		}
		else
			ok = 1;

	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ConvertComp(OldCompRecord * Comp)
{
	double Height, ShapeNameHeight;
	int32 TextRotation, Mirror, LengteCompName, LengteCompValue;

	ShapeNameHeight = 60 * 2540.0;

	TextRotation = ((Comp->CompMode & 3) + ((Comp->TextVisibility & 2) >> 1)) & 3;
	Mirror = ((Comp->CompMode & 8) >> 3);
	LengteCompName = strlen(Comp->Name);
	LengteCompValue = strlen(Comp->Value);

	if (Mirror == 0)
	{
		switch (Comp->CompMode & 3)
		{
		case 0:
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * 0.1 * DefFontSize);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x02;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (Height * -0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * 0.1 * DefFontSize);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * 0.1 * DefFontSize);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x20;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (Height * -0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * 0.1 * DefFontSize);
			}

			break;

		case 1:
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 3);
				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * 0.1 * DefFontSize);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (-LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginY += (float) (Height * -1.0 * DefFontSize);
				Comp->CompNameOriginX += (float) (Height * 0.2 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 3);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 3);
				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * 0.1 * DefFontSize);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (-LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginY += (float) (Height * -1.0 * DefFontSize);
				Comp->CompValueOriginX += (float) (Height * 0.2 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 3);
			}

			break;

		case 2:
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x04;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (-LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginX += (float) (Height * 0.2 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * -1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 2);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginY += (float) (-LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginY += (float) (Height * 0.2 * DefFontSize);
				Comp->CompNameOriginX += (float) (Height * 1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 2);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x40;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (-LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginX += (float) (Height * 0.2 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * -1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 2);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginY += (float) (-LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginY += (float) (Height * 0.2 * DefFontSize);
				Comp->CompValueOriginX += (float) (Height * 1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 2);
			}

			break;

		case 3:
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x04;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginY += (float) (-LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginX += (float) (Height * 1.0 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * 0.2 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 1);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x02;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginY += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 1);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x40;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginY += (float) (-LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginX += (float) (Height * 1.0 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * 0.2 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 1);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x20;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginY += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 1);
			}

			break;
		}
	}
	else
	{
		switch (Comp->CompMode & 3)
		{
		case 0:
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * 0.1 * DefFontSize);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x02;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * -0.1 * DefFontSize);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * 0.1 * DefFontSize);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x20;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * -0.1 * DefFontSize);
			}

			break;

		case 1:

//            Comp->CompMode^=2;
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * -0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 1);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (-LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginY += (float) (Height * -1.0 * DefFontSize);
				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 1);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * -0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 1);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (-LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginY += (float) (Height * -1.0 * DefFontSize);
				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 1);
			}

			break;

		case 2:
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x04;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (-LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * -1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 2);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x06;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginY += (float) (LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginX += (float) (Height * -1.0 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * -0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 2);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x40;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (-LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * -1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 2);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x20;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginY += (float) (LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginX += (float) (Height * -1.0 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * -0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 2);
			}

			break;

		case 3:

//            Comp->CompMode^=2;
			if ((Comp->TextVisibility & 2) == 0)
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x04;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginY += (float) (LengteCompName * Height * DefFontSize * 0.9);
				Comp->CompNameOriginY += (float) (Height * -0.1 * DefFontSize);
				Comp->CompNameOriginX += (float) (Height * -1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 3);
			}
			else
			{
				Comp->TextVisibility &= ~0x06;
				Comp->TextVisibility |= 0x02;
				Height = Comp->CompNameHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompNameOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompNameOriginY += (float) (Height * 0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompNameOriginX, &Comp->CompNameOriginY, 0.0, 0.0, 3);
			}

			if ((Comp->TextVisibility & 0x20) == 0)
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x40;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginY += (float) (LengteCompValue * Height * DefFontSize * 0.9);
				Comp->CompValueOriginY += (float) (Height * -0.1 * DefFontSize);
				Comp->CompValueOriginX += (float) (Height * -1.0 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 3);
			}
			else
			{
				Comp->TextVisibility &= ~0x60;
				Comp->TextVisibility |= 0x60;
				Height = Comp->CompValueHeight;

				if (Height == 0.0)
					Height = ShapeNameHeight;

				Comp->CompValueOriginX += (float) (Height * 0.1 * DefFontSize);
				Comp->CompValueOriginY += (float) (Height * 0.1 * DefFontSize);
				RotateFlipPoint(&Comp->CompValueOriginX, &Comp->CompValueOriginY, 0.0, 0.0, 3);
			}

			break;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadDesign()
{
	int32 hulp, result, Designfp, cnt, RotationInt, TextRotationRef, TextRotationValue, TextRotation, *CompPos,
	      CompsMemLength, PinsLength;
	NetRecord *Net;
	CompRecord *Comp, *NewComp;
	uint8 *CompP;
	double x3, y3, x4, y4;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc, *ObjectArc2, NewObjectArc;
	ObjectTextRecord *ObjectText;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;


	SetTimer0();

	DeAllocateMemDesign();

	if ((Designfp = FileOpenReadOnlyUTF8(EditFile)) == -1)
		return -3;

	if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
		return -3;

	ok = 1;

	if ((Design.BoardOutlineKeepOut < 0.0) || (Design.BoardOutlineKeepOut > (1000.0 * 2540.0)))
		Design.BoardOutlineKeepOut = (20 * 2540.0);

	if (Design.DimensionHeight == 0.0)
		Design.DimensionHeight = DIMENSION_HEIGHT;

	if (Design.ArrowLength == 0.0)
		Design.ArrowLength = ARROW_LENGTH;

	for (cnt = 0; cnt < 32; cnt++)
	{
		TempMaxNrHorTraces[cnt] = (Design.NrHorTraces[cnt] * 3) / 2;
		TempMaxNrVerTraces[cnt] = (Design.NrVerTraces[cnt] * 3) / 2;
		TempMaxNrDiag1Traces[cnt] = (Design.NrDiag1Traces[cnt] * 3) / 2;
		TempMaxNrDiag2Traces[cnt] = (Design.NrDiag2Traces[cnt] * 3) / 2;
	}

	TempMaxNrVias = max(Design.NrPins, (Design.NrVias * 3) / 2);
	TempMaxNrConnections = (Design.NrConnections * 3) / 2;
	TempMaxNrNets = (Design.NrNets * 3) / 2;
	TempMaxNrComps = max(100, (Design.NrComps * 3) / 2);
	TempMaxNrShapes = max(100, (Design.NrShapes * 3) / 2);
	TempMaxNrAreaFills = max(100, (Design.NrAreaFills * 3) / 2);
	TempMaxAreaFillMemory = max(32 * 1024, Design.AreaFillMem * 2);

	if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
		TempMaxCompsMemory = ((Design.CompsMem * 3) / 2) + 65536 + Design.NrComps * 1024;
	else
		TempMaxCompsMemory = ((Design.CompsMem * 3) / 2) + 65536;

	TempMaxNrObjectLines = max(DefMaxNrObjectLines, Design.NrObjectLines * 2);
	TempMaxNrObjectRects = max(DefMaxNrObjectCircles, Design.NrObjectRects * 2);
	TempMaxNrObjectCircles = max(DefMaxNrObjectRects, Design.NrObjectCircles * 2);
	TempMaxNrObjectArcs = max(DefMaxNrObjectArcs, Design.NrObjectArcs * 2);
	TempMaxNrObjectTexts = max(DefMaxNrObjectTexts, Design.NrObjectTexts * 2);
	TempMaxNrObjectTexts2 = max(DefMaxNrObjectTexts2, Design.NrObjectTexts2 * 2);

	TempMaxNrObjectPolygons = max(100, (Design.NrObjectPolygons * 3) / 2);
	TempMaxObjectPolygonMemory = max(32 * 1024, Design.ObjectPolygonMem * 2);

	for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
	{
		if (AllocateMemHorTraces(cnt, TempMaxNrHorTraces[cnt]) != 0)
			return -1;

		if (AllocateMemVerTraces(cnt, TempMaxNrVerTraces[cnt]) != 0)
			return -1;

		if (AllocateMemDiag1Traces(cnt, TempMaxNrDiag1Traces[cnt]) != 0)
			return -1;

		if (AllocateMemDiag2Traces(cnt, TempMaxNrDiag2Traces[cnt]) != 0)
			return -1;
	}

	if (AllocateMemComps(TempMaxNrComps) != 0)
		return -1;

	if (AllocateMemShapes(TempMaxNrShapes) != 0)
		return -1;

	if (AllocateMemVias(TempMaxNrVias) != 0)
		return -1;

	if (AllocateMemObjectLines(TempMaxNrObjectLines) != 0)
		return -1;

	if (AllocateMemObjectRects(TempMaxNrObjectRects) != 0)
		return -1;

	if (AllocateMemObjectCircles(TempMaxNrObjectCircles) != 0)
		return -1;

	if (AllocateMemObjectArcs(TempMaxNrObjectArcs) != 0)
		return -1;

	if (AllocateMemObjectTexts(max(TempMaxNrObjectTexts, TempMaxNrObjectTexts2)) != 0)
		return -1;

	if (AllocateMemObjectTexts2(max(TempMaxNrObjectTexts, TempMaxNrObjectTexts2)) != 0)
		return -1;

	if (AllocateMemObjectPolygons(TempMaxNrObjectPolygons) != 0)
		return -1;

	if (AllocateMemObjectPolygonMemory(TempMaxObjectPolygonMemory) != 0)
		return -1;

	if (AllocateMemAreaFills(TempMaxNrAreaFills) != 0)
		return -1;

	if (AllocateMemAreaFillMemory(TempMaxAreaFillMemory) != 0)
		return -1;

	if (AllocateMemCompsMemory(TempMaxCompsMemory) != 0)
		return -1;

	if (AllocateMemNets(TempMaxNrNets) != 0)
		return -1;

	if (AllocateMemNetConnections(1024) != 0)
		return -1;

	if (AllocateMemConnections(TempMaxNrConnections) != 0)
		return -1;

	if (FileRead(Designfp, Comps, sizeof(int32) * (int32) Design.NrComps, &result) == -1)
		return -3;

	ok = 1;

	if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
	{
		CompsMemLength = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			ok = 1;

			/*
			      if (0) {
			        if (FileRead(Designfp,&TestComp,sizeof(CompRecord),&result)==-1) return -3;
			      }
			*/
			if (FileRead(Designfp, &OldComp, sizeof(OldCompRecord), &result) == -1)
				return -3;

			if (strcmp(Design.Identification, PCBCode) == 0)
				ConvertComp(&OldComp);

			CompPos = &((*Comps)[cnt]);
			PinsLength = OldComp.NrPins * sizeof(CompPinRecord);
			*CompPos = CompsMemLength;
			CompP = &(CompsMem[CompsMemLength]);
			NewComp = (CompRecord *) CompP;
			memset((uint8 *) NewComp, 0, sizeof(CompRecord));
#ifdef _DEBUG

			if (stricmpOwn(OldComp.Name, "S1") == 0)
				ok = 1;

#endif
			NewComp->MemSize = OldComp.MemSize;
			NewComp->PinOffset = OldComp.PinOffset;
			NewComp->ShapeNr = OldComp.ShapeNr;
			NewComp->NrPins = OldComp.NrPins;
			NewComp->CompMode = OldComp.CompMode;
			NewComp->Info = OldComp.Info;
			NewComp->AddNr = OldComp.AddNr;
			NewComp->DeleteNr = OldComp.DeleteNr;
			NewComp->Info2 = OldComp.Info2;
			NewComp->Info3 = OldComp.Info3;
			NewComp->Info4 = OldComp.Info4;
			NewComp->Info5 = OldComp.Info5;
			NewComp->CompOriginX = OldComp.CompOriginX;
			NewComp->CompOriginY = OldComp.CompOriginY;
			NewComp->CompHeight = OldComp.CompHeight;
			NewComp->PlacementOriginX = OldComp.PlacementOriginX;
			NewComp->PlacementOriginY = OldComp.PlacementOriginY;
			NewComp->PlacementWidth = OldComp.PlacementWidth;
			NewComp->PlacementHeight = OldComp.PlacementHeight;
			NewComp->TextVisibility = OldComp.TextVisibility;
//      NewComp->TextRotation              = OldComp.TextRotation;
			NewComp->CompNameOriginX = OldComp.CompNameOriginX;
			NewComp->CompNameOriginY = OldComp.CompNameOriginY;
			NewComp->CompNameHeight = OldComp.CompNameHeight;
			NewComp->CompNamePenThickNess = OldComp.CompNamePenThickNess;
			NewComp->CompValueOriginX = OldComp.CompValueOriginX;
			NewComp->CompValueOriginY = OldComp.CompValueOriginY;
			NewComp->CompValueHeight = OldComp.CompValueHeight;
			NewComp->CompValuePenThickNess = OldComp.CompValuePenThickNess;
			NewComp->BoardPosMinX = OldComp.BoardPosMinX;
			NewComp->BoardPosMinY = OldComp.BoardPosMinY;
			NewComp->BoardPosMaxX = OldComp.BoardPosMaxX;
			NewComp->BoardPosMaxY = OldComp.BoardPosMaxY;
			NewComp->DummyX1 = OldComp.BoardPosDiagX1;
			NewComp->DummyY1 = OldComp.BoardPosDiagY1;
			NewComp->DummyX2 = OldComp.BoardPosDiagX2;
			NewComp->DummyY2 = OldComp.BoardPosDiagY2;
			NewComp->PinMaximumClearance = OldComp.PinMaximumClearance;
			memmove((uint8 *) & NewComp->Name, (uint8 *) & OldComp.Name, sizeof(OldComp.Name));
			memmove((uint8 *) & NewComp->Value, (uint8 *) & OldComp.Value, sizeof(OldComp.Value));
			memmove((uint8 *) & NewComp->PartNr, (uint8 *) & OldComp.PartNr, sizeof(OldComp.PartNr));
			memmove((uint8 *) & NewComp->PartDescription, (uint8 *) & OldComp.PartDescription,
			        sizeof(OldComp.PartDescription));
			memmove((uint8 *) & NewComp->ShapeName, (uint8 *) & OldComp.ShapeName, sizeof(OldComp.ShapeName));
			memmove((uint8 *) & NewComp->Sheet, (uint8 *) & OldComp.Sheet, sizeof(OldComp.Sheet));
			memmove((uint8 *) & NewComp->PCBGroup, (uint8 *) & OldComp.PCBGroup, sizeof(OldComp.PCBGroup));
			memmove((uint8 *) & NewComp->Options, (uint8 *) & OldComp.BestuckungOptie, sizeof(OldComp.BestuckungOptie));

			RotationInt = GetRotationFromComp(OldComp.CompMode);
			TextRotationRef = GetReferenceRotationFromComp(OldComp.TextVisibility);
			TextRotationValue = GetValueRotationFromComp(OldComp.TextVisibility);
			NewComp->Rotation = (float) (RotationInt * 45.0);
			NewComp->CompNameRotation = (float) (TextRotationRef * 45.0);
			NewComp->CompValueRotation = (float) (TextRotationValue * 45.0);
			NewComp->MemSize = 0;

			if (NewComp->Rotation > 359.9)
				NewComp->Rotation -= 360.0;

			if (NewComp->Rotation < 0.0)
				NewComp->Rotation += 360.0;

#ifdef _DEBUG

			if (stricmpOwn(NewComp->Name, "S1") == 0)
			{
				ok = 1;
//        NewComp->Rotation=10.0;
			}

#endif

//      memmove(CompP,&OldComp,sizeof(OldCompRecord));
			CompP += sizeof(CompRecord);
			CompsMemLength += sizeof(CompRecord);

			if (FileRead(Designfp, CompP, PinsLength, &result) == -1)
				return -3;

			CompsMemLength += PinsLength;
		}

		Design.CompsMem = CompsMemLength;
//  OldCompRecord          OldComp;
	}
	else
	{
		if (FileRead(Designfp, CompsMem, (int32) Design.CompsMem, &result) == -1)
			return -3;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "S1") == 0)
			{
				ok = 1;
//        Comp->Rotation=45.0;
//        Comp->CompMode|=8;
//        Comp->TextVisibility|=0x88;
//        Comp->CompMode&=~8;
//        Comp->TextVisibility&=~0x88;
			}

#endif
		}
	}

	if (LoadShapes(0) != 0)
		return -3;

	hulp = FileCurrentPointer(Designfp);

	if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
	{
		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			if (FileRead(Designfp, &OldNet, sizeof(OldNetRecord), &result) == -1)
				return -3;

			Net = &((*Nets)[cnt]);
			memmove(Net, &OldNet, sizeof(OldNetRecord));
			memset((uint8 *) & Net->Properties, 0, 256);
		}

	}
	else
	{
		if (FileRead(Designfp, Nets, sizeof(NetRecord) * (int32) Design.NrNets, &result) == -1)
			return -3;
	}

#ifdef _DEBUG

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (stricmpOwn(Net->Name, "12.288_MHz") == 0)
		{
			ok = 1;
		}
	}

#endif


	hulp = FileCurrentPointer(Designfp);
	Net = &((*Nets)[0]);

	for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
	{
		if (FileRead(Designfp, VerTraces[cnt], sizeof(TraceRecord) * (int32) Design.NrVerTraces[cnt], &result) == -1)
			return -3;

		if (FileRead(Designfp, HorTraces[cnt], sizeof(TraceRecord) * (int32) Design.NrHorTraces[cnt], &result) == -1)
			return -3;

		if (FileRead(Designfp, Diag1Traces[cnt], sizeof(TraceRecord) * (int32) Design.NrDiag1Traces[cnt], &result) ==
		        -1)
			return -3;

		if (FileRead(Designfp, Diag2Traces[cnt], sizeof(TraceRecord) * (int32) Design.NrDiag2Traces[cnt], &result) ==
		        -1)
			return -3;
	}

	hulp = FileCurrentPointer(Designfp);

	if (FileRead(Designfp, Vias, sizeof(ViaRecord) * (int32) Design.NrVias, &result) == -1)
		return -3;

	hulp = FileCurrentPointer(Designfp);

	if (FileRead(Designfp, Connections, sizeof(ConnectionsRecord) * (int32) Design.NrConnections, &result) == -1)
		return -3;

	hulp = FileCurrentPointer(Designfp);

	if (FileRead(Designfp, AreaFills, sizeof(int32) * (int32) Design.NrAreaFills, &result) == -1)
		return -3;

	if (FileRead(Designfp, AreaFillMem, (int32) Design.AreaFillMem, &result) == -1)
		return -3;

	hulp = FileCurrentPointer(Designfp);

	if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
	{
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			if (FileRead(Designfp, &OldObjectLine, sizeof(OldObjectLineRecord), &result) == -1)
				return -3;

			ObjectLine = &((*ObjectLines)[cnt]);
			ObjectLine->X1 = OldObjectLine.X1;
			ObjectLine->Y1 = OldObjectLine.Y1;
			ObjectLine->X2 = OldObjectLine.X2;
			ObjectLine->Y2 = OldObjectLine.Y2;
			ObjectLine->Info = OldObjectLine.Info;

			switch (OldObjectLine.Layer)
			{
			case PCB_BOTTOM:
				OldObjectLine.Layer = 0;
				break;

			case PCB_TOP:
				OldObjectLine.Layer = Design.NrBoardLayers - 1;
				break;
			}

			ObjectLine->Layer = OldObjectLine.Layer;
			ObjectLine->LineThickNess = OldObjectLine.LineThickNess;
			ObjectLine->NetNr = OldObjectLine.NetNr;

			if (strcmp(Design.Identification, PCBCode) == 0)
				ObjectLine->NetNr = -1;
			
			ObjectLine->Clearance = Design.StandardClearance;
			ObjectLine->LineMode = 0;
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			if (FileRead(Designfp, &OldObjectRect, sizeof(OldObjectRectRecord), &result) == -1)
				return -3;

			ObjectRect = &((*ObjectRects)[cnt]);
			memmove(ObjectRect, &OldObjectRect, sizeof(OldObjectRectRecord));

			switch (OldObjectRect.Layer)
			{
			case PCB_BOTTOM:
				ObjectRect->Layer = 0;
				break;

			case PCB_TOP:
				ObjectRect->Layer = Design.NrBoardLayers - 1;
				break;

			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
				ObjectRect->Info |= OBJECT_FILLED;
				ObjectRect->LineThickNess = 0.0;
				break;
			}

			if (strcmp(Design.Identification, PCBCode) == 0)
				ObjectRect->NetNr = -1;

			ObjectRect->Clearance = Design.StandardClearance;
			ObjectRect->Dummy = 0.0;
		}

		for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
		{
			if (FileRead(Designfp, &OldObjectCircle, sizeof(OldObjectCircleRecord), &result) == -1)
				return -3;

			ObjectCircle = &((*ObjectCircles)[cnt]);
			ObjectCircle->CentreX = OldObjectCircle.CentreX;
			ObjectCircle->CentreY = OldObjectCircle.CentreY;
			ObjectCircle->Diam = OldObjectCircle.Diam;
			ObjectCircle->CircleMode = OldObjectCircle.CircleMode;
			ObjectCircle->Info = OldObjectCircle.Info;
			ObjectCircle->LineThickNess = OldObjectCircle.LineThickNess;

			switch (OldObjectCircle.Layer)
			{
			case PCB_BOTTOM:
				OldObjectCircle.Layer = 0;
				break;

			case PCB_TOP:
				OldObjectCircle.Layer = Design.NrBoardLayers - 1;
				break;

			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
				ObjectCircle->Info |= OBJECT_FILLED;
				ObjectCircle->LineThickNess = 0.0;
				break;
			}

			ObjectCircle->Layer = OldObjectCircle.Layer;

			if (strcmp(Design.Identification, PCBCode) == 0)
				ObjectCircle->NetNr = -1;

			ObjectCircle->Clearance = Design.StandardClearance;
			ObjectCircle->Dummy = 0.0;
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			if (FileRead(Designfp, &OldObjectArc, sizeof(OldObjectArcRecord), &result) == -1)
				return -3;

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
			ObjectArc->NetNr = OldObjectArc.NetNr;
			ObjectArc->LineThickNess = OldObjectArc.LineThickNess;

			switch (OldObjectArc.Layer)
			{
			case PCB_BOTTOM:
				OldObjectArc.Layer = 0;
				break;

			case PCB_TOP:
				OldObjectArc.Layer = Design.NrBoardLayers - 1;
				break;

			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
				ObjectArc->Info |= OBJECT_FILLED;
				ObjectArc->LineThickNess = 0.0;
				break;
			}

			ObjectArc->Layer = OldObjectArc.Layer;

			if (strcmp(Design.Identification, PCBCode) == 0)
				ObjectArc->NetNr = -1;

			ObjectArc->Clearance = Design.StandardClearance;
			ObjectArc->Dummy = 0.0;
		}
	}
	else
	{
		if (FileRead(Designfp, ObjectLines, sizeof(ObjectLineRecord) * (int32) Design.NrObjectLines, &result) == -1)
			return -3;

		if (FileRead(Designfp, ObjectRects, sizeof(ObjectRectRecord) * (int32) Design.NrObjectRects, &result) == -1)
			return -3;

		if (FileRead(Designfp, ObjectArcs, sizeof(ObjectArcRecord) * (int32) Design.NrObjectArcs, &result) == -1)
			return -3;
	}

	if (FileRead(Designfp, ObjectTexts, sizeof(ObjectTextRecord) * (int32) Design.NrObjectTexts, &result) == -1)
		return -3;

	if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0)
	        || (strcmp(Design.Identification, PCBCode3) == 0))
	{
		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			if (FileRead(Designfp, &OldObjectText2, sizeof(OldObjectTextRecord2), &result) == -1)
				return -3;

			ObjectText2 = &((*ObjectTexts2)[cnt]);
			ObjectText2->X = OldObjectText2.X;
			ObjectText2->Y = OldObjectText2.Y;
			ObjectText2->FontHeight = OldObjectText2.FontHeight;
			ObjectText2->Info = OldObjectText2.Info;
			ObjectText2->Rotation = OldObjectText2.Rotation;
			ObjectText2->NetNr = OldObjectText2.NetNr;
			ObjectText2->LineThickNess = OldObjectText2.LineThickNess;

			if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
			{
				switch (OldObjectText2.Layer)
				{
				case PCB_BOTTOM:
					OldObjectText2.Layer = 0;
					break;

				case PCB_TOP:
					OldObjectText2.Layer = Design.NrBoardLayers - 1;
					break;
				}
			}

			ObjectText2->Layer = OldObjectText2.Layer;
			ObjectText2->TextMode = OldObjectText2.TextMode;
			memmove(ObjectText2->Text, OldObjectText2.Text, sizeof(OldObjectText2.Text));
		}
	}
	else
	{
		if (FileRead(Designfp, ObjectTexts2, sizeof(ObjectTextRecord2) * (int32) Design.NrObjectTexts2, &result) == -1)
			return -3;
	}

	if (FileRead(Designfp, ObjectPolygons, sizeof(int32) * (int32) Design.NrObjectPolygons, &result) == -1)
		return -3;

	if (FileRead(Designfp, ObjectPolygonMem, (int32) Design.ObjectPolygonMem, &result) == -1)
		return -3;

	if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
	{
		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			ObjectText = &((*ObjectTexts)[cnt]);
			ObjectText->NetNr = -1;
		}

		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);
			ObjectText2->NetNr = -1;
		}

		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
			ObjectPolygon->NetNr = -1;
		}
	}

	hulp = FileCurrentPointer(Designfp);
	FileClose(Designfp);

	if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
	{
		if (Design.NrObjectTexts > 0)
		{
			for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
			{
				ObjectText = &((*ObjectTexts)[cnt]);
				ObjectText2 = &((*ObjectTexts2)[cnt]);

				TextRotation = (ObjectText->TextMode >> 8) & 3;
				ObjectText2->Rotation = 0.0;

				switch (TextRotation)
				{
				case 0:		// 0
					break;

				case 1:		// 90
					ObjectText2->Rotation = (90.0);
					break;

				case 2:		// 180
					ObjectText2->Rotation = (180.0);
					break;

				case 3:		// 270
					ObjectText2->Rotation = (270.0);
					break;
				}

				ObjectText2->TextMode = (int16) (ObjectText->TextMode & 0xff);
				ObjectText2->Info = ObjectText->Info;
				ObjectText2->Layer = ObjectText->Layer;

				switch (ObjectText->Layer)
				{
				case PCB_BOTTOM:
					ObjectText2->Layer = 0;
					break;

				case PCB_TOP:
					ObjectText2->Layer = Design.NrBoardLayers - 1;
					break;
				}

				ObjectText2->X = ObjectText->X;
				ObjectText2->Y = ObjectText->Y;
				ObjectText2->FontHeight = ObjectText->FontHeight;
				ObjectText2->LineThickNess = ObjectText->LineThickNess;
				memmove(ObjectText2->Text, ObjectText->Text, sizeof(ObjectText->Text));
			}

			Design.NrObjectTexts2 = Design.NrObjectTexts;
			Design.NrObjectTexts = 0;
		}
	}

	memset(&NewObjectArc, 0, sizeof(NewObjectArc));

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);
		NewObjectArc.CentreX = ObjectCircle->CentreX;
		NewObjectArc.CentreY = ObjectCircle->CentreY;
		NewObjectArc.Width = ObjectCircle->Diam;
		NewObjectArc.Height = ObjectCircle->Diam;
		NewObjectArc.LineThickNess = ObjectCircle->LineThickNess;
		NewObjectArc.Layer = ObjectCircle->Layer;
		NewObjectArc.NetNr = -1;
		NewObjectArc.Info = 0;
		x3 = 0.0;
		y3 = 0.0;
		x4 = 0.0;
		y4 = 0.0;

		switch (ObjectCircle->CircleMode)
		{
		case 1:
			x3 = NewObjectArc.Width;
			y4 = NewObjectArc.Width;
			break;

		case 2:
			y3 = NewObjectArc.Width;
			x4 = -NewObjectArc.Width;
			break;

		case 3:
			y3 = -NewObjectArc.Width;
			y4 = NewObjectArc.Width;
			break;

		case 4:
			x3 = -NewObjectArc.Width;
			y4 = -NewObjectArc.Width;
			break;

		case 6:
			x3 = -NewObjectArc.Width;
			x4 = NewObjectArc.Width;
			break;

		case 8:
			y3 = -NewObjectArc.Width;
			x4 = NewObjectArc.Width;
			break;

		case 9:
			x3 = NewObjectArc.Width;
			x4 = -NewObjectArc.Width;
			break;

		case 12:
			y3 = NewObjectArc.Width;
			y4 = -NewObjectArc.Width;
			break;

		default:
			y3 = NewObjectArc.Width;
			y4 = NewObjectArc.Width;
			NewObjectArc.Info = (int16) (ObjectCircle->Info & OBJECT_FILLED);
			break;
		}

		switch (NewObjectArc.Layer)
		{
		case DRILL_LAYER:
		case DRILL_UNPLATED_LAYER:
		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
			if ((strcmp(Design.Identification, PCBCode) == 0) || (strcmp(Design.Identification, PCBCode2) == 0))
				NewObjectArc.Info = OBJECT_FILLED;

			break;
		}

		NewObjectArc.StartDiffX = (float) x3;
		NewObjectArc.StartDiffY = (float) y3;
		NewObjectArc.EndDiffX = (float) x4;
		NewObjectArc.EndDiffY = (float) y4;

		if (Design.NrObjectArcs >= MaxNrObjectArcs)
		{
			if (AllocateMemObjectArcs(MaxNrObjectArcs + 32) != 0)
			{
			}
		}

		ObjectArc2 = &((*ObjectArcs)[Design.NrObjectArcs]);
		memmove(ObjectArc2, &NewObjectArc, sizeof(ObjectArcRecord));
		Design.NrObjectArcs++;
		ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
		ObjectCircle->AddNr = 0;
		ObjectCircle->DeleteNr = 0;
	}

	Design.NrObjectCircles = 0;

	ok = 1;
	ResetAllObjects();
	LastActionNr = 1;
	MaxLastActionNr = 1;

	memmove(&CurrentVia, &Design.DefVia1, sizeof(ViaRecord));

	if ((CurrentDrawingLayer == -1) || (CurrentDrawingLayer >= Design.NrBoardLayers))
	{
		CurrentDrawingLayer = GetDrawingLayer(4);
	
	}

	MakeCheckMenu();
	FileChanged = 0;
	SetWindowName(0);

	DeleteGraphicObjects();
	CreateDrawObjects(0);

	AddPerformanceValue2(SC(379, "Layout loading"));

	WritePerformanceStrings();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemDesignSave(int32 DesignSaveMemSize)
{
	HGLOBAL NewMem;

	if (MaxDesignSaveMem == 0)
	{
		DesignSaveMemSize = max(DesignSaveMemSize, 65536);

		if ((DesignSaveMemGlobal = GlobalAlloc(GHND, DesignSaveMemSize)) == NULL)
			return -1;

		if ((DesignSaveMem = (uint8 *) GlobalLock(DesignSaveMemGlobal)) == NULL)
			return -1;

		MaxDesignSaveMem = DesignSaveMemSize;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(DesignSaveMemGlobal, DesignSaveMemSize, GHND)) == NULL)
			return -1;

		DesignSaveMemGlobal = NewMem;

		if ((DesignSaveMem = (uint8 *) GlobalLock(DesignSaveMemGlobal)) == NULL)
			return -1;

		MaxDesignSaveMem = DesignSaveMemSize;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteToBuf(void *MemToAdd, int32 BufPos, int32 MemSize)
{
	if (BufPos + MemSize > MaxDesignSaveMem)
	{
		if (AllocateMemDesignSave(max(BufPos + MemSize + 16384, MaxDesignSaveMem * 2)) != 0)
			return -1;
	}

	memmove(&DesignSaveMem[BufPos], MemToAdd, MemSize);
	return BufPos + MemSize;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeBackup(LPSTR CurrentFile)
{
	char DirPart[MAX_LENGTH_STRING], FilePart[MAX_LENGTH_STRING], BackupFile1[MAX_LENGTH_STRING],
	     BackupFile2[MAX_LENGTH_STRING];
	int32 CurrentFileTime, BackupFileTime;

	if ((GetDirFromFileName(DirPart, CurrentFile) == 0) && (GetFilePartFromFileName(FilePart, CurrentFile) == 0))
	{
		sprintf(BackupFile1, "%s\\backup\\%s", DirPart, FilePart);
		CutExtensionFileName(FilePart);
		sprintf(BackupFile2, "%s\\backup\\%s.1", DirPart, FilePart);
		CurrentFileTime = DaysAfter1900FromFileName(CurrentFile);

		if (CurrentFileTime != -1)
		{
			BackupFileTime = DaysAfter1900FromFileName(BackupFile1);

			if (BackupFileTime != -1)
			{
				if (BackupFileTime < CurrentFileTime)
					CopyFileUTF8(BackupFile1, BackupFile2, 0);
			}

			CopyFileUTF8(CurrentFile, BackupFile1, 0);
		}
		else
			return -1;
	}
	else
		return -2;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SaveFile(int32 mode)
{
	int32 cnt, cnt2, Layer, NewBufPos, TempPos, res, NrAreaFills, AreaFillMemSize, NewLayer, StartOfTracesPos, Libfp =
	    0, fp2, Pos, Length, res2, HorTracesCount[32], VerTracesCount[32], Diag1TracesCount[32], Diag2TracesCount[32],
	    ObjectLineCount, ObjectRectCount, ObjectCircleCount, ObjectArcCount, ObjectTextCount, ObjectText2Count,
	    ObjectPolygonCount, PolygonMemSize, ViasCount, ConnectionsCount, CompsCount, CompsMemSize, CompLength, BufPos =
	        0, fp, result, SpecialCompRotation;
	double value;
	ShapeInfoRecord *ShapeInfo;
	TraceRecord *Trace;
	ViaRecord *Via;
	CompRecord *Comp;
	NetRecord *Net;
	ConnectionsRecord *Connection;
	AreaFillRecord *AreaFill;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord SaveObjectText;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	DesignRecord NewDesign;
	char str[MAX_LENGTH_STRING], buf[1024], LibName[MAX_LENGTH_STRING];
	struct tm *today;
	time_t ltime;

	MaxDesignSaveMem = 0;
	ViasCount = 0;
	ConnectionsCount = 0;
	CompsCount = 0;
	CompsMemSize = 0;
	ObjectLineCount = 0;
	ObjectRectCount = 0;
	ObjectCircleCount = 0;
	ObjectArcCount = 0;
	ObjectTextCount = 0;
	ObjectText2Count = 0;
	SpecialCompRotation = 0;


	SetTimer0();

	if (((mode & 3) == 0) && (!FileChanged))
		return 0;

	if (((mode & 1) == 1) || (EditFile[0] == 0))
	{
		res = SaveFileName(0);

		if (res == -1)
			return 0;
	}

	memset(&SaveObjectText, 0, sizeof(SaveObjectText));

	value = (float) GetDifferenceTimer0inMicroSeconds() / 1000;

	BufPos = sizeof(DesignRecord);

	if ((mode & 1) == 0)
		res = MakeBackup(EditFile);


	if (SaveSymbolsLocally)
	{
		for (cnt2 = 0; cnt2 < Design.NrShapes; cnt2++)
		{
			ShapeInfo = &((*Shapes)[cnt2]);
			sprintf(str, "%s\\pcb\\shapes\\%s.shp", DesignPath, ShapeInfo->ShapeName);

			if ((LoadShapeDirectory(ShapeInfo->ShapeName, LibName, &Pos, &Length) >= 0) && (stricmpUTF8(LibName, str))
			        && (Length < 256 * 1024) && ((Libfp = FileOpenReadOnlyUTF8(LibName)) > 0))
			{
				FileSeek(Libfp, Pos);
				fp2 = FileOpenWriteUTF8(str);

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

				FileClose(Libfp);
			}
		}
	}

	ok = 1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			CompLength = MemSizeComp(Comp);
			CompsCount++;

			if ((NewBufPos = WriteToBuf(&CompsMemSize, BufPos, 4)) == -1)
			{
				return -1;
			}

			CompsMemSize += CompLength;
			BufPos = NewBufPos;
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Comp->TextVisibility &= ~(0x88);
			Comp->TextVisibility |= (Comp->CompMode & 8) * 0x11;
			CompLength = MemSizeComp(Comp);

			if ((NewBufPos = WriteToBuf(Comp, BufPos, CompLength)) == -1)
			{
				return -1;
			}

			BufPos = NewBufPos;
		}
	}

// *****************************************************************************************
// *****************************************************************************************
	ok = 1;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Count = 0;
		Net->Pos = 0;
	}

	if ((NewBufPos = WriteToBuf(Nets, BufPos, sizeof(NetRecord) * Design.NrNets)) == -1)
	{
		return -1;
	}

	BufPos = NewBufPos;
	TempPos = BufPos;
	StartOfTracesPos = BufPos;

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

	for (Layer = 0; Layer < Design.NrBoardLayers; Layer++)
	{
		VerTracesCount[Layer] = 0;
		HorTracesCount[Layer] = 0;
		Diag1TracesCount[Layer] = 0;
		Diag2TracesCount[Layer] = 0;

// *****************************************************************************************
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				VerTracesCount[Layer]++;

				if ((NewBufPos = WriteToBuf(Trace, BufPos, sizeof(TraceRecord))) == -1)
				{
					return -1;
				}

				BufPos = NewBufPos;
			}
		}

// *****************************************************************************************
		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				HorTracesCount[Layer]++;

				if ((NewBufPos = WriteToBuf(Trace, BufPos, sizeof(TraceRecord))) == -1)
				{
					return -1;
				}

				BufPos = NewBufPos;
			}
		}

// *****************************************************************************************
		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Diag1TracesCount[Layer]++;

				if ((NewBufPos = WriteToBuf(Trace, BufPos, sizeof(TraceRecord))) == -1)
				{
					return -1;
				}

				BufPos = NewBufPos;
			}
		}

// *****************************************************************************************
		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Diag2TracesCount[Layer]++;

				if ((NewBufPos = WriteToBuf(Trace, BufPos, sizeof(TraceRecord))) == -1)
				{
					return -1;
				}

				BufPos = NewBufPos;
			}
		}
	}

// *****************************************************************************************
// *****************************************************************************************

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ViasCount++;

			if ((NewBufPos = WriteToBuf(Via, BufPos, sizeof(ViaRecord))) == -1)
			{
				return -1;
			}

			BufPos = NewBufPos;
		}
	}

// *****************************************************************************************

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ConnectionsCount++;

			if ((NewBufPos = WriteToBuf(Connection, BufPos, sizeof(ConnectionsRecord))) == -1)
			{
				return -1;
			}

			BufPos = NewBufPos;
		}
	}

// *****************************************************************************************
// *****************************************************************************************
// AreaFills

	AreaFillMemSize = 0;
	NrAreaFills = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			NrAreaFills++;

			if ((NewBufPos = WriteToBuf(&AreaFillMemSize, BufPos, 4)) == -1)
			{
				return -1;
			}

			AreaFillMemSize += AreaFill->MemSize;
			BufPos = NewBufPos;
		}
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((NewBufPos = WriteToBuf(AreaFill, BufPos, AreaFill->MemSize)) == -1)
			{
				return -1;
			}

			BufPos = NewBufPos;
		}
	}


// *****************************************************************************************
// *****************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectLine->Layer;
			NewLayer = Layer;
			ObjectLineCount++;

			if ((NewBufPos = WriteToBuf(ObjectLine, BufPos, sizeof(ObjectLineRecord))) == -1)
			{
				return -1;
			}

			BufPos = NewBufPos;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectRect->Layer;
			NewLayer = Layer;
			ObjectRectCount++;

			if ((NewBufPos = WriteToBuf(ObjectRect, BufPos, sizeof(ObjectRectRecord))) == -1)
			{
				return -1;
			}

			BufPos = NewBufPos;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectArc->Layer;
			NewLayer = Layer;
			ObjectArcCount++;

			if ((NewBufPos = WriteToBuf(ObjectArc, BufPos, sizeof(ObjectArcRecord))) == -1)
			{
				sprintf(str, "Error in saving file (%d)", __LINE__);
				MessageBoxOwn(PCBWindow, str, "Message", MB_APPLMODAL | MB_OK);
				return -1;
			}

			BufPos = NewBufPos;
		}
	}

// *****************************************************************************************
// *****************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectText2->Text[0] != 0))
		{
			Layer = ObjectText2->Layer;
			NewLayer = Layer;
			memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			NewObjectText2.Layer = NewLayer;
			ObjectText2Count++;

			if ((NewBufPos = WriteToBuf(ObjectText2, BufPos, sizeof(ObjectTextRecord2))) == -1)
			{
				return -1;
			}

			BufPos = NewBufPos;
		}
	}

// *****************************************************************************************
// *****************************************************************************************
// ObjectPolygons
	PolygonMemSize = 0;
	ObjectPolygonCount = 0;

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectPolygon->NrVertices > 0))
		{
			Layer = ObjectPolygon->Layer;
			NewLayer = Layer;
			SetMinMaxObjectPolygon(ObjectPolygon, 0);
			ObjectPolygonCount++;

			if ((NewBufPos = WriteToBuf(&PolygonMemSize, BufPos, 4)) == -1)
			{
				return -1;
			}

			PolygonMemSize += MemSizeObjectPolygon(ObjectPolygon);
			BufPos = NewBufPos;
		}
	}

// *****************************************************************************************
// *****************************************************************************************

	memmove(&NewDesign, &Design, sizeof(DesignRecord));
	NewDesign.NrComps = CompsCount;

	for (Layer = 0; Layer < NewDesign.NrBoardLayers; Layer++)
	{
		NewDesign.NrVerTraces[Layer] = VerTracesCount[Layer];
		NewDesign.NrHorTraces[Layer] = HorTracesCount[Layer];
		NewDesign.NrDiag1Traces[Layer] = Diag1TracesCount[Layer];
		NewDesign.NrDiag2Traces[Layer] = Diag2TracesCount[Layer];
	}

	NewDesign.NrVias = ViasCount;
	NewDesign.NrConnections = ConnectionsCount;
	NewDesign.CompsMem = CompsMemSize;
	NewDesign.NrComps = CompsCount;
	NewDesign.NrAreaFills = NrAreaFills;
	NewDesign.AreaFillMem = AreaFillMemSize;
	NewDesign.NrObjectLines = ObjectLineCount;
	NewDesign.NrObjectRects = ObjectRectCount;
	NewDesign.NrObjectCircles = ObjectCircleCount;
	NewDesign.NrObjectArcs = ObjectArcCount;
	NewDesign.NrObjectTexts = ObjectTextCount;
	NewDesign.NrObjectTexts2 = ObjectText2Count;

	NewDesign.NrObjectPolygons = ObjectPolygonCount;
	NewDesign.ObjectPolygonMem = PolygonMemSize;
	memset(&NewDesign.Identification, 0, sizeof(NewDesign.Identification));
	time(&ltime);
	today = localtime(&ltime);
	NewDesign.DesignDate.Year = (uint8) today->tm_year;
	NewDesign.DesignDate.Month = (uint8) (today->tm_mon + 1);
	NewDesign.DesignDate.Day = (uint8) today->tm_mday;
	NewDesign.DesignDate.Hour = (uint8) today->tm_hour;
	NewDesign.DesignDate.Minutes = (uint8) today->tm_min;

	strcpy(NewDesign.Identification, PCBCode4);

	if ((res = WriteToBuf(&NewDesign, 0, sizeof(DesignRecord))) == -1)
	{
		return -1;
	}

	if ((fp = CheckForWritingAndOpen(EditFile, BufPos, PCBWindow)) < 0)
	{
		return -1;
	}


	FileWrite(fp, DesignSaveMem, BufPos, &result);
	FileClose(fp);

	if (DesignSaveMemGlobal != NULL)
	{
		GlobalUnlock(DesignSaveMemGlobal);
		GlobalFree(DesignSaveMemGlobal);
		DesignSaveMemGlobal = NULL;
	}

	MaxDesignSaveMem = 0;
	FileChanged = 0;
	SetWindowName(0);
	value = (float) GetDifferenceTimer0inMicroSeconds() / 1000;

	sprintf(str, "Layout saving in %.3f ms", value);
	
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SaveFile2(int32 mode)
{
	int32 res = 0;
	char str[MAX_LENGTH_STRING];

	if (FileChanged)
	{

		sprintf(str, SC(142, "The layout file has changed.\n\n%s\n\nDo you want to update it ?"), EditFile);
		
		res = MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNOCANCEL);

		if (res == IDYES)
		{
			res = SaveFile(0);

			if (res == -1)
				MessageBoxOwn(PCBWindow, EditFile, SC(147, "Error in saving file"), MB_APPLMODAL | MB_OK);

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DumpTracesConnectionsToFile()
{
#ifdef _DEBUG
	int32 fp, Layer, cnt, Written;

	TraceRecord *Trace;
	ViaRecord *Via;
	ConnectionsRecord *Connection;

	if ((fp = FileOpenUTF8("c:\\pcb\\tr.!!!")) == -1)
		return;

	FileWrite(fp, &Design, sizeof(DesignRecord), &Written);
	FileWrite(fp, &Design.NrConnections, sizeof(Design.NrConnections), &Written);

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			FileWrite(fp, Trace, sizeof(TraceRecord), &Written);
		}

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			FileWrite(fp, Trace, sizeof(TraceRecord), &Written);
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			FileWrite(fp, Trace, sizeof(TraceRecord), &Written);
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			FileWrite(fp, Trace, sizeof(TraceRecord), &Written);
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		FileWrite(fp, Via, sizeof(ViaRecord), &Written);
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		FileWrite(fp, Connection, sizeof(ConnectionsRecord), &Written);
	}

	FileClose(fp);
#endif
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetWindowName(int32 mode)
{
	char str[MAX_LENGTH_STRING];

	if (EditFile[0] != 0)
		sprintf(str, SC(383, "Layout %s"), EditFile);
	else
		sprintf(str, SC(130, "New layout"));

	if (ProjectIndexNr != -1)
	{
		strcpy(ProjectInfo->FileNames[ProjectIndexNr], EditFile);

		if (mode == 1)
			ProjectInfo->FileInfos[ProjectIndexNr] |= 1;
		else
			ProjectInfo->FileInfos[ProjectIndexNr] &= ~1;
	}

	if (mode == 1)
		strcat(str, " *");

	if (PCBWindow != NULL)
		SetWindowTextUTF8(PCBWindow, str);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitBoardValues(int32 mode)
{
	// mode 0 = change file
	// mode 1 = new design

	if (Units == 0)
	{
		NewDesign.BoardOriginX = (1000 * 2540.0);
		NewDesign.BoardOriginY = (1000 * 2540.0);
		NewDesign.BoardWidth = (4000 * 2540.0);
		NewDesign.BoardHeight = (2000 * 2540.0);
		NewDesign.StandardTraceWidth = (8 * 2540.0);
		NewDesign.StandardClearance = (8 * 2540.0);
		NewDesign.MaximumClearance = (8 * 2540.0);
		NewDesign.PowerPlaneBorder = (50 * 2540.0);
		NewDesign.SilkScreenWidth = (8 * 2540.0);
		NewDesign.BoardOutlineWidth = (8 * 2540.0);
		CurrentVia.ThickNess = (40 * 2540.0);
		CurrentVia.DrillThickNess = (0.4 * 10e4);
		CurrentVia.Clearance = (8 * 2540.0);
		CurrentVia.ThermalInner = (40 * 2540.0);
		CurrentVia.SoldMask = (43 * 2540.0);
		TraceWidthUser[0] = (8.0 * 2540.0);
		ClearanceWidthUser[0] = (8.0 * 2540.0);

		if (mode == 1)
		{
			GridSize = (5 * 2540.0);
			UserGridSize = (25 * 2540.0);
			TraceGridSize = (5 * 2540.0);
			AreafillGridSize = (5 * 2540.0);
			CompGridSize = (5 * 2540.0);
		}
	}
	else
	{
		NewDesign.BoardOriginX = (20 * 10e4);
		NewDesign.BoardOriginY = (20 * 10e4);
		NewDesign.BoardWidth = (100 * 10e4);
		NewDesign.BoardHeight = (50 * 10e4);
		NewDesign.StandardTraceWidth = (0.2 * 10e4);
		NewDesign.StandardClearance = (0.2 * 10e4);
		NewDesign.MaximumClearance = (0.2 * 10e4);
		NewDesign.PowerPlaneBorder = (1.0 * 10e4);
		NewDesign.SilkScreenWidth = (0.2 * 10e4);
		NewDesign.BoardOutlineWidth = (0.25 * 10e4);
		CurrentVia.ThickNess = (1.0 * 10e4);
		CurrentVia.DrillThickNess = (0.4 * 10e4);
		CurrentVia.Clearance = (0.2 * 10e4);
		CurrentVia.ThermalInner = (1.0 * 10e4);
		CurrentVia.SoldMask = (1.08 * 10e4);
		TraceWidthUser[0] = (0.2 * 10e4);
		ClearanceWidthUser[0] = (0.2 * 10e4);

		if (mode == 1)
		{
			GridSize = (0.2 * 10e4);
			UserGridSize = (1.0 * 10e4);
			TraceGridSize = (0.2 * 10e4);
			AreafillGridSize = (0.2 * 10e4);
			CompGridSize = (0.2 * 10e4);
		}
	}

	if (mode == 0)
	{
		Design.BoardOriginX = NewDesign.BoardOriginX;
		Design.BoardOriginY = NewDesign.BoardOriginY;
		Design.BoardWidth = NewDesign.BoardWidth;
		Design.BoardHeight = NewDesign.BoardHeight;
		Design.StandardTraceWidth = NewDesign.StandardTraceWidth;
		Design.StandardClearance = NewDesign.StandardClearance;
		Design.MaximumClearance = NewDesign.MaximumClearance;
		Design.PowerPlaneBorder = NewDesign.PowerPlaneBorder;
		Design.SilkScreenWidth = NewDesign.SilkScreenWidth;
		Design.BoardOutlineWidth = NewDesign.BoardOutlineWidth;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InitNewDesign(int32 mode)
{
	DesignRecord OldDesign;
	ObjectLineRecord NewObjectLine;

	memmove(&OldDesign, &Design, sizeof(DesignRecord));
	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	NewObjectLine.Layer = BOARD_OUTLINE_LAYER;
	NewObjectLine.LineThickNess = Design.SilkScreenWidth;

	if (InRange(NewObjectLine.LineThickNess, 0.0))
		NewObjectLine.LineThickNess = (8.0 * 2540);

	NewObjectLine.X1 = OldDesign.BoardOriginX;
	NewObjectLine.Y1 = OldDesign.BoardOriginY;
	NewObjectLine.X2 = OldDesign.BoardOriginX;
	NewObjectLine.Y2 = OldDesign.BoardOriginY + OldDesign.BoardHeight;
	AddObjectLine(&NewObjectLine);
	memmove(&NewObjectLine.X1, &NewObjectLine.X2, 2 * sizeof(NewObjectLine.X1));
	NewObjectLine.X2 = OldDesign.BoardOriginX + OldDesign.BoardWidth;
	NewObjectLine.Y2 = OldDesign.BoardOriginY + OldDesign.BoardHeight;
	AddObjectLine(&NewObjectLine);
	memmove(&NewObjectLine.X1, &NewObjectLine.X2, 2 * sizeof(NewObjectLine.X1));
	NewObjectLine.X2 = OldDesign.BoardOriginX + OldDesign.BoardWidth;
	NewObjectLine.Y2 = OldDesign.BoardOriginY;
	AddObjectLine(&NewObjectLine);
	memmove(&NewObjectLine.X1, &NewObjectLine.X2, 2 * sizeof(NewObjectLine.X1));
	NewObjectLine.X2 = OldDesign.BoardOriginX;
	NewObjectLine.Y2 = OldDesign.BoardOriginY;
	AddObjectLine(&NewObjectLine);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckFile(LPSTR FileName, int32 mode)
{
	int32 Designfp, result;
	DesignRecord Design;
	char str[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING];

	if ((mode & 2) == 0)
		strcpy(FileName2, FileName);
	else
		strcpy(FileName2, EditFile);

	if (FileExistsUTF8(FileName2) == 0)
	{
		if ((Designfp = FileOpenReadOnlyUTF8(FileName2)) == -1)
		{
			sprintf(str, SC(384, "Error in opening file %s"), FileName2);
			MessageBoxOwn(PCBWindow, str, "", MB_APPLMODAL | MB_OK);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
		{
			sprintf(str, SC(385, "Error in reading file %s"), FileName2);
			MessageBoxOwn(PCBWindow, str, "", MB_APPLMODAL | MB_OK);
			FileClose(Designfp);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		FileClose(Designfp);

		if ((stricmpOwn(Design.Identification, PCBCode) != 0) && (stricmpOwn(Design.Identification, PCBCode2) != 0)
		        && (stricmpOwn(Design.Identification, PCBCode3) != 0) && (stricmpOwn(Design.Identification, PCBCode4) != 0))
		{
			sprintf(str, SC(386, "File %s is not a valid layout file"), FileName2);
			MessageBoxOwn(PCBWindow, str, "", MB_APPLMODAL | MB_OK);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}
	}
	else
		return -2;

	strcpy(EditFile, FileName2);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeFile(LPSTR FileName, int32 mode)
{
	char str[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING], ProjectIniFile[MAX_LENGTH_STRING];
	int32 res, CheckResult;

	CheckResult = CheckFile(FileName, mode);

	if (CheckResult == -1)
		return;

	ProjectIniFile[0] = 0;

	if (EditFile[0] != 0)
	{
		IniFile[0] = 0;

		if (FoundDesignPath)
		{
			sprintf(FileName2, "%s\\pcb\\gerber", DesignPath);

			if ((!SetCurrentDirectoryUTF8(FileName2)) && (!CreateDirectoryUTF8(FileName2)))
			{
				MessageBoxOwn(PCBWindow, FileName2, SC(387, "Can not create directory"), MB_APPLMODAL | MB_OK);
				return;
			}

			sprintf(FileName2, "%s\\pcb\\shapes", DesignPath);

			if ((!SetCurrentDirectoryUTF8(FileName2)) && (!CreateDirectoryUTF8(FileName2)))
			{
				MessageBoxOwn(PCBWindow, FileName2, SC(387, "Can not create directory"), MB_APPLMODAL | MB_OK);
				return;
			}

			sprintf(FileName2, "%s\\pcb\\hpgl", DesignPath);

			if ((!SetCurrentDirectoryUTF8(FileName2)) && (!CreateDirectoryUTF8(FileName2)))
			{
				MessageBoxOwn(PCBWindow, FileName2, SC(387, "Can not create directory"), MB_APPLMODAL | MB_OK);
				return;
			}

			sprintf(FileName2, "%s\\pcb\\bmp", DesignPath);

			if ((!SetCurrentDirectoryUTF8(FileName2)) && (!CreateDirectoryUTF8(FileName2)))
			{
				MessageBoxOwn(PCBWindow, FileName2, SC(387, "Can not create directory"), MB_APPLMODAL | MB_OK);
				return;
			}

			sprintf(str, "%s\\pcb.ini", DesignPath);

			if (FileExistsUTF8(str) == 0)
				strcpy(IniFile, str);
		}

		if (FoundProjectPath)
		{
			sprintf(str, "%s\\pcb.ini", ProjectPath);

			if (FileExistsUTF8(str) == 0)
			{
				strcpy(ProjectIniFile, str);
				if (IniFile[0] == 0)
					strcpy(IniFile, str);
			}
		}

	}
	else
	{
		if (FoundProjectPath)
		{
			sprintf(str, "%s\\pcb.ini", ProjectPath);

			if (FileExistsUTF8(str) == 0)
			{
				strcpy(IniFile, str);
				strcpy(ProjectIniFile, str);
			}
		}
	}

	// load [Settings]
	if (IniFile[0] != 0)
		LoadIniFile(IniFile, 1);

	// load [Keys]
	if (ProjectIniFile[0] != 0)
		LoadIniFile(ProjectIniFile, 2);
	else
		LoadIniFile(IniFile, 2);

	DeAllocateMemDesign();

	res = 0;
	InitBoardValues(0);
//  InitNewDesign(0);

	if (EditFile[0] != 0)
	{
		if (CheckResult == 0)
		{
			if (LoadDesign() != 0)
			{
				MessageBoxOwn(PCBWindow, EditFile, SC(388, "Error in layout"), MB_APPLMODAL | MB_OK);
				CreateDrawObjects(0);
			}
		}
		else
		{
			MakeNewDesign = 1;
			CreateDrawObjects(0);
		}
	}
	else
		CreateDrawObjects(0);

	if ((mode & 2) == 0)
	{
		SetWindowName(0);
		ViewFull();
		CheckInputMessages(0);
		CheckInputMessages(0);

		if (MessageBufPos != 0)
		{
			MessageDialog(SC(24, "Error"), 0, 0);
			DeAllocateMemMessageBuf();
		}
	}
}


// *******************************************************************************************************
// ********************************** Otevøít soubor rozložení *******************************************
// *******************************************************************************************************

int32 LoadNewFile()
{
	char EditFile2[MAX_LENGTH_STRING] = "";
	int32 cnt;

	if (GetNewFileUTF8
	        (PCBWindow, NULL, EditFile2, ProjectPath, SC(389, "Layout file"), NULL, SC(404, "Open layout file"), "pcb", 0))
		return -1;

	if (ProjectActive)
	{
		cnt = 0;

		while ((cnt < 32)
		        && ((ProjectInfo->FileTypes[cnt] != 4) || (stricmpOwn(ProjectInfo->FileNames[cnt], EditFile2) != 0)))
			cnt++;

		if (cnt < 32)
		{
			if (ShowWindow(ProjectInfo->WindowHandles[cnt], SW_RESTORE))
				SetForegroundWindow(ProjectInfo->WindowHandles[cnt]);

			return 0;
		}
	}

	ChangeFile(EditFile2, 0);
	return 0;
}

//*******************************************************************************************************
//********************************** Uložit soubor rozložení ********************************************
//*******************************************************************************************************

int32 SaveFileName(int32 Mode)
{
	char str[MAX_LENGTH_STRING];

	if (EditFile[0] == 0)
		strcpy(EditFile, "noname.pcb");

	sprintf(str, "%s\\pcb", DesignPath);

	if (GetNewFileUTF8
	        (PCBWindow, NULL, EditFile, str, SC(389, "Layout file"), NULL, SC(140, "Save layout file"), "pcb", 1))
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SaveNewFile(LPSTR NewFile, int32 Mode)
{
	if (GetNewFileUTF8(PCBWindow, NULL, NewFile, ExportDir, SC(363, "DXF file"), NULL, SC(364, "Import DXF file"), "dxf", 1))
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void LoadDesignIniFile()
{
	int32 fp, Length, ParamMode, value;
	char LineBuf[512], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];

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

				if (stricmpOwn(str1, "[TopSheet]") == 0)
					ParamMode = 1;

				if (stricmpOwn(str1, "[LayoutFile]") == 0)
					ParamMode = 2;

				if (stricmpOwn(str1, "[Settings]") == 0)
					ParamMode = 3;

				if (stricmpOwn(str1, "[GeometryLibraries]") == 0)
					ParamMode = 4;

				if (stricmpOwn(str1, "[SchematicSymbolLibraries]") == 0)
					ParamMode = 5;
			}
			else
			{
				switch (ParamMode)
				{
				case 1:
					break;
				case 2:
					break;

				case 3:
					if (GetStringValue(str4, str1, str2))
					{
						if (stricmpOwn(str1, "SaveSymbolsLocally") == 0)
						{
							if (sscanf(str2, "%i", &value) == 1)
								SaveSymbolsLocally = value;
						}
					}

					break;

				case 4:		//  GeometryLibraryPath
					if (NrGeometryLibraries < 16)
					{
						if (FileExistsUTF8(str1) == 0)
							strcpy(GeometryLibraries[NrGeometryLibraries++], str1);
					}

					break;
				}
			}
		}
	}

	TextFileClose(fp);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void FilesMain()
{
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
