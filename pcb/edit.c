/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edit.c
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
#include "calcdef.h"
#include "pcb.h"
#include "nets.h"
#include "calc.h"
#include "calc4.h"
#include "edit.h"
#include "stdio.h"
#include "mainloop.h"
#include "files.h"
#include "files2.h"
#include "dialogs.h"
#include "insdel.h"
#include "resource.h"


#define BIT_ACTIVE(BitmapBuf,x) (((BitmapBuf[((x) >> 3)] & ((0x80 >> ((x) & 7)))) != 0) ? 1 : 0)


typedef struct
{
	int32 MemSize, ShapeNr, NrPins, CompMode, Info, AddNr, DeleteNr, Info2, Info3, Info4, Info5;
	double CompOriginX, CompOriginY, CompHeight, PlacementOriginX, PlacementOriginY, PlacementWidth, PlacementHeight;
	int32 TextVisibility;
	double Rotation, CompNameRotation, CompValueRotation, Dummy;
	double CompNameOriginX, CompNameOriginY, CompNameHeight, CompNamePenThickNess, CompValueOriginX, CompValueOriginY,
	       CompValueHeight, CompValuePenThickNess, BoardPosMinX, BoardPosMinY, BoardPosMaxX, BoardPosMaxY, DummyX1,
	       DummyY1, DummyX2, DummyY2, PinMaximumClearance;
	char Name[16], Value[32], ShapeName[32];
} CompImportPositionRecord;


char BitmapFileName[MAX_LENGTH_STRING];


extern uint32 ClipBoardLayoutTracesViasID, ClipBoardLayoutComponentsID;


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyTracesViasToClipBoard(int32 mode)
{
	typedef uint8 ByteArray[1000];
	int32 MemSize, cnt, *NrObjects, *TotalMemSize, Layer, TraceInfo, ViaInfo, count;
	char str[MAX_LENGTH_STRING];
	uint8 *BufP;
//  char   *Mem;
	ByteArray *Mem;
	ObjectRecord Object;
	TraceRecord *Trace;
	ViaRecord *Via;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;

	if (!OpenClipboard(PCBWindow))
		return -1;

	if (!EmptyClipboard())
		return -1;

	if (CurrentDrawingLayer == -1)
		return -1;

	Layer = CurrentDrawingLayer;
	count = 0;

	for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
	{
		Trace = &((*VerTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
			count++;
	}

	for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
	{
		Trace = &((*HorTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
			count++;
	}

	for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
	{
		Trace = &((*Diag1Traces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
			count++;
	}

	for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
	{
		Trace = &((*Diag2Traces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
			count++;
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
			count++;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (ObjectLine->NetNr >= 0)
		        && (ObjectLine->Layer == Layer))
			count++;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->NetNr >= 0) && (ObjectArc->Layer == Layer))
			count++;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (ObjectArc->NetNr >= 0)
		        && (ObjectArc->Layer == DRILL_LAYER))
			count++;
	}

	if (count == 0)
	{
		sprintf(str, SC(372, "There are no traces to copy from layer %d\r\n\r\n"), CurrentDrawingLayer);
		strcat(str, SC(373, "Maybe you should switch the layer"));
		MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	MemSize = count * sizeof(ObjectRecord) + 4096;

	if ((GlobalClipBoardMem = GlobalAlloc(GHND | GMEM_DDESHARE, MemSize)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -1;

	NrObjects = (int32 *) ClipBoardMem;
	(*NrObjects) = 0;
	BufP = ClipBoardMem + 4;
	TotalMemSize = (int32 *) BufP;
	BufP += 4;
	*TotalMemSize = 8;
	Mem = (ByteArray *) ClipBoardMem;

	NrObjects4 = 0;

	for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
	{
		Trace = &((*VerTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = Trace->X;
			Object.y1 = Trace->Y;
			Object.x2 = Trace->Length;
			Object.y2 = Trace->ThickNess;
			Object.ObjectType = TRACE_VER;
//      Object.Clearance=Trace->Clearance;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
	{
		Trace = &((*HorTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = Trace->X;
			Object.y1 = Trace->Y;
			Object.x2 = Trace->Length;
			Object.y2 = Trace->ThickNess;
			Object.ObjectType = TRACE_HOR;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
	{
		Trace = &((*Diag1Traces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = Trace->X;
			Object.y1 = Trace->Y;
			Object.x2 = Trace->Length;
			Object.y2 = Trace->ThickNess;
			Object.ObjectType = TRACE_DIAG1;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
	{
		Trace = &((*Diag2Traces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = Trace->X;
			Object.y1 = Trace->Y;
			Object.x2 = Trace->Length;
			Object.y2 = Trace->ThickNess;
			Object.ObjectType = TRACE_DIAG2;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = Via->X;
			Object.y1 = Via->Y;
			Object.x2 = Via->ThickNess;
			Object.y2 = Via->DrillThickNess;
			Object.ObjectType = PIN_SMD_ROUND;
			Object.Info = 2;
			Object.TraceNr = -1;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

// ********************************************************************************************************
// ********************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (ObjectLine->NetNr >= 0)
		        && (ObjectLine->Layer == Layer))
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = ObjectLine->X1;
			Object.y1 = ObjectLine->Y1;
			Object.x2 = ObjectLine->X2;
			Object.y2 = ObjectLine->Y2;
			Object.Thickness = ObjectLine->LineThickNess;
			Object.ObjectType = TRACE_ALL_ANGLE;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->NetNr >= 0) && (ObjectArc->Layer == Layer))
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = ObjectArc->CentreX;
			Object.y1 = ObjectArc->CentreY;
			Object.x2 = ObjectArc->Width;
			Object.y2 = ObjectArc->Height;
			Object.x3 = ObjectArc->StartDiffX;
			Object.y3 = ObjectArc->StartDiffY;
			Object.x4 = ObjectArc->EndDiffX;
			Object.y4 = ObjectArc->EndDiffY;
			Object.Thickness = ObjectArc->LineThickNess;
			Object.ObjectType = TRACE_ARC;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (ObjectArc->NetNr >= 0)
		        && (ObjectArc->Layer == DRILL_LAYER))
		{
			memset(&Object, 0, sizeof(ObjectRecord));
			Object.x1 = ObjectArc->CentreX;
			Object.y1 = ObjectArc->CentreY;
			Object.x2 = ObjectArc->Width;
			Object.ObjectType = DRILL;
			Object.NetNr = -1;
			memmove(BufP, &Object, sizeof(ObjectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRecord);
			(*TotalMemSize) += sizeof(ObjectRecord);
		}
	}

	GlobalUnlock(GlobalClipBoardMem);

	if (SetClipboardData(ClipBoardLayoutTracesViasID, GlobalClipBoardMem) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		GlobalFree(GlobalClipBoardMem);
		return -1;
	}

	CloseClipboard();
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyTracesViasFromClipBoard(int32 mode)
{
	typedef uint8 ByteArray[1000];

	int32 cnt, *NrObjects, TotalMemSize, Count, *hulp;
	uint8 *BufP, *NewClipBoardMem;
	HGLOBAL NewGlobalClipBoardMem;
	ObjectRecord *Object;

	ByteArray *Mem;

	if (!OpenClipboard(PCBWindow))
		return -2;

	if ((GlobalClipBoardMem = GetClipboardData(ClipBoardLayoutTracesViasID)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -2;

	hulp = (int32 *) (ClipBoardMem + 4);
	TotalMemSize = *hulp;

	if ((NewGlobalClipBoardMem = GlobalAlloc(GHND, TotalMemSize)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	if ((NewClipBoardMem = GlobalLock(NewGlobalClipBoardMem)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	memmove(NewClipBoardMem, ClipBoardMem, TotalMemSize);
	GlobalUnlock(GlobalClipBoardMem);
	CloseClipboard();

	NrObjects = (int32 *) NewClipBoardMem;
	Mem = (ByteArray *) NewClipBoardMem;
	BufP = NewClipBoardMem + 4;
	BufP += 4;
	Count = *NrObjects;
	cnt = 0;

	if (Count <= 0)
		return -1;

	if (AllocateMemObjects6(Count) == -1)
		return -1;

	NrObjects6 = 0;

	while (NrObjects6 < Count)
	{
		Object = &((*Objects6)[NrObjects6]);
		memmove(Object, BufP, sizeof(ObjectRecord));
		BufP += sizeof(ObjectRecord);
		NrObjects6++;
	}

	GlobalUnlock(NewGlobalClipBoardMem);
	GlobalFree(NewGlobalClipBoardMem);
	return NrObjects6;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ImportBitmap(int32 Layer, int32 mode)
{
	BmpHeaderRecord BmpHeader;
	int32 BitActive;
	ObjectTextRecord2 TypeObject;
	uint8 LineBytes[4096];
	char FileName[MAX_LENGTH_STRING];
	double StandardResolution, y, PadSize, CurrentX2, CurrentY2;
	float value;
	COLORREF PaletteColors[256];
	int32 MemSize, fp, result, cnt, cnt2, cnt3, cnt4, res, Count, Width, Height, BitPos2;

//  if ((fp=FileOpenReadOnlyUTF8("c:\\wincode\\omgeving4.bmp"))>0) {
//  if ((fp=FileOpenReadOnlyUTF8("c:\\viewplot\\blad1.bmp"))>0) {
	memset(&TypeObject, 0, sizeof(ObjectTextRecord2));
	PadSize = 25400.0;

	switch (Units)
	{
	case 0:
		sprintf(TypeObject.Text, "%.1f", PadSize / 2540.0);
		break;

	case 1:
		sprintf(TypeObject.Text, "%.4f", PadSize / 100000.0);
		break;
	}

	CheckInputMessages(0);

	if (BitmapFileName[0] == 0)
		strcpy(BitmapFileName, DesignPath);

	if (GetNewFileUTF8
	        (PCBWindow, NULL, BitmapFileName, ExportDir, SC(374, "Bitmap file"), NULL, SC(375, "Import bitmap files"), "bmp", 0))
		return -1;

//  if (LoadNewFile3(BitmapFileName,SC(374,"Bitmap file"),SC(375,"Import bitmap files"),"bmp",0)!=0) return -1;

	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	NewObjectLine.Layer = INFO_LAYER;
	NewObjectLine.Info = OBJECT_SELECTED | 3;
	NewObjectLine.NetNr = -1;
	NewObjectLine.LineThickNess = (float) (PadSize * 1.2);

	if ((fp = FileOpenReadOnlyUTF8(BitmapFileName)) > 0)
	{
		res = sizeof(BmpHeaderRecord);
		FileRead(fp, &BmpHeader, sizeof(BmpHeaderRecord), &result);

		if ((BmpHeader.Identifier != 0x4d42) || (BmpHeader.CompressionType != 0) || (BmpHeader.NrOfPlanes != 1)
		        || (BmpHeader.BitsPerPixel != 1))
		{
			MessageBoxOwn(PCBWindow, FileName, SC(376, "Wrong bitmap file"), MB_APPLMODAL | MB_OK);
			FileClose(fp);
			return -1;
		}

		if (BmpHeader.NrColors1 != 2)
		{
//      FileClose(fp);
//      return -2;
		}

		if (LineInputDialog(&TypeObject, SC(377, "Pixel size bitmap"), 0) == 1)
		{
			if ((sscanf(TypeObject.Text, "%f", &value)) == 1)
			{
				switch (Units)
				{
				case 0:
					PadSize = value * 2540.0;
					break;

				case 1:
					PadSize = value * 100000.0;
					break;
				}

				NewObjectLine.LineThickNess = (float) (PadSize * 1.2);
			}
		}

		CurrentX2 = 0.0;
		CurrentY2 = 0.0;
//    CurrentX2=AdjustToDrawGrid(PixelToRealOffX(MousePosX));
//    CurrentY2=AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY-MousePosY));
		FileRead(fp, &PaletteColors, sizeof(COLORREF) * max(2, BmpHeader.NrColors1), &result);

		if (BmpHeader.HResolutionInPixelsPerMeter == 11811)
			StandardResolution = (2540000.0 / 300);
		else
		{
			if (BmpHeader.HResolutionInPixelsPerMeter == 23622)
				StandardResolution = (2540000.0 / 600);
			else
				StandardResolution = (100000000 / BmpHeader.HResolutionInPixelsPerMeter);
		}

		Width = BmpHeader.Width;
		Height = BmpHeader.Height;
		cnt2 = (BmpHeader.FileSize - sizeof(BmpHeaderRecord)) / BmpHeader.Height;
		cnt3 = ((Width + 15) & ~15) / 8;
		MemSize = cnt2 * Height;
//    memset(BitmapMem,0xff,MemSize);
		res = FileCurrentPointer(fp);
		Count = 0;

		for (cnt = 0; cnt < Height; cnt++)
		{
//      FileRead(fp,&BitmapMem[cnt*cnt3],cnt2,&result);
			FileRead(fp, &LineBytes, cnt2, &result);
			y = PadSize * cnt;
			NewObjectLine.Y1 = (float) (CurrentY2 + y);
			NewObjectLine.Y2 = (float) (CurrentY2 + y);
			BitActive = !BIT_ACTIVE(LineBytes, 0);
			BitPos2 = -1;

			if (BitActive)
				BitPos2 = 0;

			for (cnt4 = 1; cnt4 < Width; cnt4++)
			{
				if (BitActive)
				{
					if (BIT_ACTIVE(LineBytes, cnt4))
					{
						// Add a number of black pixels
						NewObjectLine.X1 = (float) (CurrentX2 + PadSize * BitPos2);
						NewObjectLine.X2 = (float) (CurrentX2 + PadSize * (cnt4 - 1));
						AddObjectLine(&NewObjectLine);
						Count++;
						BitActive = 0;
						BitPos2 = -1;
					}
				}
				else
				{
					if (!BIT_ACTIVE(LineBytes, cnt4))
					{
						if (BitPos2 == -1)
						{
							BitPos2 = cnt4;
							BitActive = 1;
						}
					}
				}
			}

			if (BitActive)
			{
				NewObjectLine.X1 = (float) (CurrentX2 + PadSize * BitPos2);
				NewObjectLine.X2 = (float) (CurrentX2 + PadSize * (Width - 1));
				AddObjectLine(&NewObjectLine);
				Count++;
			}
		}

		FileClose(fp);
		PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_OBJECTS, (LPARAM) NULL);
		CheckInputMessages(0);
		RePaint();
//    PostMessage(GEOMWindow,WM_COMMAND,(WPARAM)ID_MOVE_OBJECTS,(LPARAM)NULL);
//    MoveSelectedObjects(0,0);
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyComponentsFromClipBoard(int32 mode)
{
	typedef uint8 ByteArray[1000];

	int32 cnt, *NrClipObjects, TotalMemSize, Count, *hulp, cnt2, TempLastActionNr, MemSize;
	HGLOBAL NewGlobalClipBoardMem;
	uint8 *BufP, *NewClipBoardMem, *SelectedNets;
	CompImportPositionRecord CompPosition;
	CompRecord *Comp, *NewComp;
	ObjectRecord *Object;

	ByteArray *Mem;

	if (!OpenClipboard(PCBWindow))
		return -2;

	if ((GlobalClipBoardMem = GetClipboardData(ClipBoardLayoutComponentsID)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -2;

	hulp = (int32 *) (ClipBoardMem + 4);
	TotalMemSize = *hulp;

	if ((NewGlobalClipBoardMem = GlobalAlloc(GHND, TotalMemSize)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	if ((NewClipBoardMem = GlobalLock(NewGlobalClipBoardMem)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	memmove(NewClipBoardMem, ClipBoardMem, TotalMemSize);
	GlobalUnlock(GlobalClipBoardMem);
	CloseClipboard();

	NrClipObjects = (int32 *) NewClipBoardMem;
	Mem = (ByteArray *) NewClipBoardMem;
	BufP = NewClipBoardMem + 4;
	BufP += 4;
	Count = *NrClipObjects;
	cnt = 0;

	if (Count <= 0)
		return -1;

	TempLastActionNr = LastActionNr - 1;
	AllocateSpecialMem(MEM_NET_SELECTED, (Design.NrNets + 10) * sizeof(uint8), (void **) &SelectedNets);
	memset(SelectedNets, 0, Design.NrNets);
	cnt2 = 0;

	while (cnt2 < Count)
	{
		memmove(&CompPosition, BufP, sizeof(CompImportPositionRecord));

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->AddNr <= TempLastActionNr)
			        && (stricmpOwn(Comp->Name, CompPosition.Name) == 0))
			{
				MemSize = MemSizeComp(Comp);
				AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
				memmove(NewComp, Comp, MemSize);
				NewComp->CompOriginX = (float) CompPosition.CompNameOriginX;
				NewComp->CompOriginY = (float) CompPosition.CompNameOriginY;
				NewComp->CompMode &= ~0x3;
				NewComp->CompMode &= ~8;
				NewComp->CompMode |= CompPosition.CompMode & 8;
				NewComp->Rotation = (float) CompPosition.Rotation;

				if (AddComp(NewComp))
				{
					NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
					Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
					Comp->Info &= ~OBJECT_SELECTED;
					NewComp->Info |= OBJECT_SELECTED;
					NewComp->Info4 = 0;
					Comp->Info |= OBJECT_NOT_VISIBLE;
					Comp->DeleteNr = (int16) LastActionNr;
					NrObjects = 0;
					ShapePinsToObject(NewComp, 0.0, 0.0, 0, 0, 0, 0);

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
							SelectedNets[Object->NetNr] = 1;
					}
				}
			}
		}

		BufP += sizeof(CompImportPositionRecord);
		cnt2++;
	}

	GlobalUnlock(NewGlobalClipBoardMem);
	GlobalFree(NewGlobalClipBoardMem);

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (SelectedNets[cnt] == 1)
		{
			ReCalcConnectionsNet((int32) cnt, 0, 1);
			CheckForEscape();
		}
	}

	DeallocateSpecialMem(MEM_NET_SELECTED);
	ViewFull();
	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyComponentsToClipBoard(int32 mode)
{
	typedef uint8 ByteArray[1000];
	int32 MemSize, cnt, *NrObjects, *TotalMemSize, count;
	uint8 *BufP;
	CompImportPositionRecord CompPosition;
	CompRecord *Comp;
//  char   *Mem;
	ByteArray *Mem;

	if (!OpenClipboard(PCBWindow))
		return -1;

	if (!EmptyClipboard())
		return -1;

	count = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
			count++;
	}

	if (count == 0)
		return -1;

	MemSize = count * sizeof(CompImportPositionRecord) + 4096;

	if ((GlobalClipBoardMem = GlobalAlloc(GHND | GMEM_DDESHARE, MemSize)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -1;

	memset(&CompPosition, 0, sizeof(CompImportPositionRecord));
	NrObjects = (int32 *) ClipBoardMem;
	(*NrObjects) = 0;
	BufP = ClipBoardMem + 4;
	TotalMemSize = (int32 *) BufP;
	BufP += 4;
	*TotalMemSize = 8;
	Mem = (ByteArray *) ClipBoardMem;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & ((OBJECT_NOT_VISIBLE | OBJECT_SELECTED))) == OBJECT_SELECTED)
		{
			CompPosition.CompMode = Comp->CompMode & 8;
			CompPosition.Rotation = Comp->Rotation;
			strcpy(CompPosition.Name, Comp->Name);
			CompPosition.CompOriginX = Comp->CompOriginX;
			CompPosition.CompOriginY = Comp->CompOriginY;
			memmove(BufP, &CompPosition, sizeof(CompImportPositionRecord));
			(*NrObjects)++;
			BufP += sizeof(CompImportPositionRecord);
			(*TotalMemSize) += sizeof(CompImportPositionRecord);
		}
	}

	GlobalUnlock(GlobalClipBoardMem);

	if (SetClipboardData(ClipBoardLayoutComponentsID, GlobalClipBoardMem) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		GlobalFree(GlobalClipBoardMem);
		return -1;
	}

	CloseClipboard();
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 PolygonToSourceCode(int32 mode)
{
#ifdef _DEBUG
	int32 cnt, cnt2, cnt3, count;
	ObjectPolygonRecord *ObjectPolygon;
	AreaFillRecord *AreaFill;
	char str[200];
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			count = ObjectPolygon->NrVertices;
			sprintf(str, "    memset(SubPolygon,0,sizeof(PolygonInitRecord));\r\n");
			OutputDebugString(str);
			sprintf(str, "    SubPolygon->NrVertices=%d;\r\n", count);
			OutputDebugString(str);

			for (cnt2 = 0; cnt2 < count; cnt2++)
			{
				sprintf(str, "    SubPolygon->Points[%d].x=%.3f;\r\n", cnt2, ObjectPolygon->Points[cnt2].x);
				OutputDebugString(str);
				sprintf(str, "    SubPolygon->Points[%d].y=%.3f;\r\n", cnt2, ObjectPolygon->Points[cnt2].y);
				OutputDebugString(str);
			}
		}
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			AreaPos = (uint8 *) AreaFill;
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

			if ((DrawPolygon->PolygonType & 2) == 2)
			{
				sprintf(str, "    memset(AreaFill,0,sizeof(AreaFillRecord));\r\n");
				OutputDebugString(str);
				sprintf(str, "    AreaFill->MemSize=sizeof(AreaFillRecord);\r\n");
				OutputDebugString(str);
				sprintf(str, "    AreaFill->NrPolygons=%d;\r\n", AreaFill->NrPolygons);
				OutputDebugString(str);
				sprintf(str, "    AreaFill->minx= 1000000000.0;\r\n");
				OutputDebugString(str);
				sprintf(str, "    AreaFill->miny= 1000000000.0;\r\n");
				OutputDebugString(str);
				sprintf(str, "    AreaFill->maxx=-1000000000.0;\r\n");
				OutputDebugString(str);
				sprintf(str, "    AreaFill->maxy=-1000000000.0;\r\n");
				OutputDebugString(str);
				sprintf(str, "    SubPolygon=(PolygonRecord *)((uint8 *)AreaFill+sizeof(AreaFillRecord));\r\n");
				OutputDebugString(str);
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					PolygonPos = (uint8 *) DrawPolygon;
					count = DrawPolygon->NrVertices;
					sprintf(str, "    SubPolygon->NrVertices=%d;\r\n", count);
					OutputDebugString(str);

					for (cnt3 = 0; cnt3 < count; cnt3++)
					{
						sprintf(str, "    SubPolygon->Points[%d].x=%.3f;\r\n", cnt3, DrawPolygon->Points[cnt3].x);
						OutputDebugString(str);
						sprintf(str, "    SubPolygon->Points[%d].y=%.3f;\r\n", cnt3, DrawPolygon->Points[cnt3].y);
						OutputDebugString(str);
					}

					sprintf(str, "    SetMinMaxPolygon(SubPolygon,0);\r\n");
					OutputDebugString(str);
					sprintf(str, "    AreaFill->MemSize+=MemSizePolygon(SubPolygon);\r\n");
					OutputDebugString(str);
					sprintf(str, "    AreaFill->minx=min(AreaFill->minx,SubPolygon->minx);\r\n");
					OutputDebugString(str);
					sprintf(str, "    AreaFill->miny=min(AreaFill->miny,SubPolygon->miny);\r\n");
					OutputDebugString(str);
					sprintf(str, "    AreaFill->maxx=max(AreaFill->maxx,SubPolygon->maxx);\r\n");
					OutputDebugString(str);
					sprintf(str, "    AreaFill->maxy=max(AreaFill->maxy,SubPolygon->maxy);\r\n");
					OutputDebugString(str);

					if (cnt2 < AreaFill->NrPolygons - 1)
					{
						sprintf(str,
						        "    SubPolygon=(PolygonRecord *)((uint8 *)SubPolygon+MemSizePolygon(SubPolygon));\r\n");
						OutputDebugString(str);
					}

					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;
				}
			}
		}
	}

#endif


	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
