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
#include "stdio.h"
#include "windows.h"
#include "memory.h"
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "direct.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "geom.h"
#include "files.h"
#include "files2.h"
#include "calcdef.h"
#include "calc.h"
#include "draw3.h"
#include "select.h"
#include "insdel.h"
#include "dialogs.h"
#include "mainloop.h"
#include "resource.h"
#include "graphics.h"
#include "movecomp.h"
#include "polygon.h"
#include "utf8.h"

//#define TEST


#define MaxObjectsPerPin                8192
#define MaxNrPins                       8192


#define BIT_ACTIVE(BitmapBuf,x) (((BitmapBuf[((x) >> 3)] & ((0x80 >> ((x) & 7)))) != 0) ? 1 : 0)



int32 Designfp, BufPos, NrObjectsBeforeSaving, SMDdevice, ok;
int32 DrillObjects[MaxNrPins];

ShapeLinesArray ShapeLines;

ShapePadRecord ShapePad;
PadRecord Pad;
Pad2Record Pad2;


char EditPath[MAX_LENGTH_STRING];

// *******************************************************************************************************
// *******************************************************************************************************

extern HWND MasterWindow;
extern char LibraryFile[MAX_LENGTH_STRING], StartDir[MAX_LENGTH_STRING], IniFile[MAX_LENGTH_STRING],
       ProjectPath[MAX_LENGTH_STRING];
extern int32 ProjectIndexNr, WidthScrollBar, PaintCount, HeightInfoBar, HeightScrollBar, CheckFilenameForSaving,
       ProjectActive, RepaintBecauseOfResize;
extern ProjectInfoRecord *ProjectInfo;


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

	if (stricmp(&FileName[lengte - 3], "shp") != 0)
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

void LoadObjects()
{
	int32 PinOffset, MemPos, cnt, NrPins, PinNr, NrLines, PolygonVertices, rot, *ShapeInfoP, ShapeInfo, NrPinShapes,
	      *PolygonPointer;
	double x1, y1, x2, y2, x3, y3, x4, y4;
	LPCSTR PinText;
	ShapeLinesArray *ShapeLines;
	ObjectPolygonRecord *ObjectPolygon;

	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	ObjectRecord NewObject;
	PinInfoRecord NewPinInfo;
	ObjectRecord *Object;
	char str[MAX_LENGTH_STRING], *TextP;

	memset(NewObjectPolygon, 0, sizeof(*NewObjectPolygon));
// EditFile
	NrObjects = 0;
	NrPinObjects = 0;
	MemPos = 0;
	PolygonVertices = 0;
	NrObjectPolygons = 0;
//  Shape=(ShapeRecord *)&(ShapesMem[MemPos]);
	memmove(&Shape, ShapesMem, sizeof(ShapeRecord));
	NrPadLayers = max(2, Shape.NrLayers);
	PinOffset = Shape.PinOffset;
	ShapeInfo = Shape.Info;
	NrPins = Shape.NrPins;
	MemPos = PinOffset;
	PinNr = 0;

	while (NrPins > 0)
	{
//    if (NrPins==3) MemPos=2840;
#ifdef _DEBUG
		if (PinNr == 213)
			ok = 1;

#endif
		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		NrPinShapes = ShapePad->NrPinShapes;
#ifdef _DEBUG

		if (PinNr >= 222)
		{
			if (NrPinShapes > 10)
				ok = 1;
		}

		if (stricmp(ShapePad->Name, "geom1230") == 0)
			ok = 1;

#endif
		MemPos += sizeof(ShapePadRecord);
		PinText = (LPCSTR) & (ShapePad->Name);

		while (NrPinShapes > 0)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

			if (MemPos == 226484)
				ok = 1;

#endif
			memset(&NewObject, 0, sizeof(ObjectRecord));
			NewObject.PinNr = (int16) PinNr;
			NewObject.Layer = Pad->Layer;
			NewObject.ObjectType = (int16) Pad->ShapeType;
			NewObject.x1 = Pad->X;
			NewObject.y1 = Pad->Y;
			NewObject.x2 = Pad->Width;
			NewObject.y2 = Pad->Height;
			NewObject.Clearance = Pad->Clearance;
#ifdef _DEBUG

			if ((InRange5(NewObject.x1, 39.3e5)) && (InRange5(NewObject.y1, 54.6e5)))
				ok = 1;

#endif

			switch (Pad->ShapeType)
			{
			case PIN_PUT_THROUGH_ROUND:
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.y2 = 0.0;
				NewObject.Layer = 0;
				AddObject(&NewObject);
				NewObject.Layer = NrPadLayers - 1;
				AddObject(&NewObject);
				NewObject.x2 = Pad->Height;
				NewObject.Layer = DRILL_LAYER;
				NewObject.y2 = 0.0;
				AddObject(&NewObject);

				if (NrPadLayers > 2)
				{
					for (cnt = 1; cnt < NrPadLayers - 1; cnt++)
					{
						NewObject.Layer = cnt;
						NewObject.ObjectType = OBJECT_CIRCLE;

						if (Pad->Special.Extra1 > 0.0)
							NewObject.x2 = Pad->Special.Extra1;
						else
							NewObject.x2 = Pad->Width;

						NewObject.y2 = 0.0;
						AddObject(&NewObject);
					}
				}
				else
				{
					NewObject.Layer = INNER_PAD_LAYER;
					NewObject.ObjectType = OBJECT_CIRCLE;

					if (Pad->Special.Extra1 > (float) 0.0)
						NewObject.x2 = Pad->Special.Extra1;
					else
						NewObject.x2 = Pad->Width;

					NewObject.y2 = (float) 0.0;
					NewObject.Clearance = (float) 0.0;
					AddObject(&NewObject);
				}

				if (Pad->Extra2 > 0.0)
				{
					NewObject.Layer = POWER_PAD_LAYER;
					NewObject.ObjectType = OBJECT_CIRCLE;
					NewObject.x2 = Pad->Extra2;
					NewObject.y2 = 0.0;
					NewObject.Clearance = 0.0;
					AddObject(&NewObject);
				}

				break;

			case PIN_PUT_THROUGH_SQUARE:
				NewObject.ObjectType = OBJECT_RECT;
				NewObject.y2 = Pad->Width;
				NewObject.Layer = 0;
				AddObject(&NewObject);
				NewObject.Layer = NrPadLayers - 1;
				AddObject(&NewObject);
				NewObject.x2 = Pad->Height;
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.Layer = DRILL_LAYER;
				NewObject.y2 = 0.0;
				AddObject(&NewObject);

				if (NrPadLayers > 2)
				{
					for (cnt = 1; cnt < NrPadLayers - 1; cnt++)
					{
						NewObject.Layer = cnt;
						NewObject.x2 = Pad->Width;
						AddObject(&NewObject);
					}
				}
				else
				{
					NewObject.Layer = INNER_PAD_LAYER;
					NewObject.ObjectType = OBJECT_CIRCLE;

					if (Pad->Special.Extra1 > 0.0)
						NewObject.x2 = Pad->Special.Extra1;
					else
						NewObject.x2 = Pad->Width;

					NewObject.y2 = 0.0;
					NewObject.Clearance = 0.0;
					AddObject(&NewObject);
				}

				if (Pad->Extra2 > 0.0)
				{
					NewObject.Layer = POWER_PAD_LAYER;
					NewObject.ObjectType = OBJECT_CIRCLE;
					NewObject.x2 = Pad->Extra2;
					NewObject.y2 = 0.0;
					NewObject.Clearance = 0.0;
					AddObject(&NewObject);
				}

				break;

			case DRILL:
				NewObject.Layer = DRILL_LAYER;
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.y2 = 0.0;
				AddObject(&NewObject);

				if (Pad->Special.Extra1 > 0.0)
				{
					NewObject.x2 = Pad->Special.Extra1;
					NewObject.Layer = INNER_PAD_LAYER;
					NewObject.y2 = (float) 0.0;
					NewObject.Clearance = 0.0;
					AddObject(&NewObject);
				}

				if (Pad->Extra2 > (float) 0.0)
				{
					NewObject.Layer = POWER_PAD_LAYER;
					NewObject.ObjectType = OBJECT_CIRCLE;
					NewObject.x2 = Pad->Extra2;
					NewObject.y2 = 0.0;
					NewObject.Clearance = 0.0;
					AddObject(&NewObject);
				}

				break;

			/*
			        case PIN_PUT_THROUGH_ROUND_INNER_PAD:
			          NewObject.Layer=INNER_PAD_LAYER;
			          NewObject.ObjectType=OBJECT_CIRCLE;
			          NewObject.y2=0.0;
			          AddObject(&NewObject);
			          break;
			*/
			case PIN_PUT_THROUGH_ROUND_POWER:
				NewObject.Layer = POWER_PAD_LAYER;
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.y2 = 0.0;
				AddObject(&NewObject);
				break;

			case DRILL_UNPLATED:
				NewObject.PinNr = -1;
				NewObject.Layer = DRILL_UNPLATED_LAYER;
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.y2 = 0.0;
				AddObject(&NewObject);
				break;

			case PIN_SMD_RECT:
				NewObject.ObjectType = OBJECT_RECT;
				AddObject(&NewObject);
				break;

			case PIN_SMD_ROUND:
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.y2 = 0.0;
				AddObject(&NewObject);
				break;

			case PIN_LINE_HOR:
				NewObject.ObjectType = OBJECT_LINE;
				NewObject.Thickness = Pad->Height;
				NewObject.x2 += NewObject.x1;
				NewObject.y2 = NewObject.y1;
				AddObject(&NewObject);
				break;

			case PIN_LINE_VER:
				NewObject.ObjectType = OBJECT_LINE;
				NewObject.Thickness = Pad->Height;
				NewObject.y2 = NewObject.y1 + NewObject.x2;
				NewObject.x2 = NewObject.x1;
				AddObject(&NewObject);
				break;

			case PIN_LINE_DIAG1:
				NewObject.ObjectType = OBJECT_LINE;
				NewObject.Thickness = Pad->Height;
				NewObject.y2 = NewObject.y1 - NewObject.x2;
				NewObject.x2 += NewObject.x1;
				AddObject(&NewObject);
				break;

			case PIN_LINE_DIAG2:
				NewObject.ObjectType = OBJECT_LINE;
				NewObject.Thickness = Pad->Height;
				NewObject.y2 = NewObject.y1 + NewObject.x2;
				NewObject.x2 += NewObject.x1;
				AddObject(&NewObject);
				break;

			case OBJECT_POLYGON:
			case PIN_SMD_POLYGON:
				ObjectPolygon = (ObjectPolygonRecord *) & (ShapesMem[Pad->Special.AddressOffset]);
				PolygonVertices = ObjectPolygon->NrVertices;
#ifdef _DEBUG

				if (Pad->Special.AddressOffset == 0)
				{
					cnt = Pad->Special.AddressOffset;
					ok = 1;
				}

				if (PolygonVertices > 100)
					ok = 1;

				if (ObjectPolygon->NrSubPolygons == 279)
					ok = 1;

#endif
				memcpy(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
				MoveObjectPolygon(NewObjectPolygon, Pad->X, Pad->Y, 0);
				NewObjectPolygon->Layer = Pad->Layer;
				NewObjectPolygon->PinNr = PinNr;
				NewObjectPolygon->Clearance = Pad->Clearance;
				NewObjectPolygon->Info = 0;
				AddObjectPolygon(NewObjectPolygon);
				break;

			case PIN_PUT_THROUGH_POLYGON:
				ObjectPolygon = (ObjectPolygonRecord *) & (ShapesMem[Pad->Special.AddressOffset]);
				memcpy(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
				MoveObjectPolygon(NewObjectPolygon, Pad->X, Pad->Y, 0);
				NewObjectPolygon->Layer = 0;
				NewObjectPolygon->PinNr = PinNr;
				NewObjectPolygon->Clearance = Pad->Clearance;
				NewObjectPolygon->Info = 0;
				AddObjectPolygon(NewObjectPolygon);
				NewObjectPolygon->Layer = NrPadLayers - 1;
				AddObjectPolygon(NewObjectPolygon);

				NewObject.x1 = Pad->X;
				NewObject.y1 = Pad->Y;
				NewObject.x2 = Pad->Height;
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.Layer = DRILL_LAYER;
				NewObject.y2 = (float) 0.0;
				AddObject(&NewObject);

				if (NrPadLayers > 2)
				{
					for (cnt = 1; cnt < NrPadLayers - 1; cnt++)
					{
						NewObject.Layer = cnt;
						NewObject.ObjectType = OBJECT_CIRCLE;
						AddObject(&NewObject);
					}
				}
				else
				{
					NewObject.Layer = INNER_PAD_LAYER;
					NewObject.ObjectType = OBJECT_CIRCLE;
					NewObject.Clearance = (float) 0.0;
					AddObject(&NewObject);
				}

				if (Pad->Extra2 > (float) 0.0)
				{
					NewObject.Layer = POWER_PAD_LAYER;
					NewObject.ObjectType = OBJECT_CIRCLE;
					NewObject.x2 = Pad->Extra2;
					NewObject.y2 = (float) 0.0;
					NewObject.Clearance = (float) 0.0;
					AddObject(&NewObject);
				}

				break;

			case PIN_LINE_ALL_ANGLE:
			case OBJECT_LINE:
				NewObject.ObjectType = OBJECT_LINE;
				NewObject.Thickness = Pad->Special.Extra1;
				AddObject(&NewObject);
				break;

			case PIN_ARC:		// arc
			case OBJECT_ARC:	// arc
				ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
				x1 = (*ShapeLines)[2];
				y1 = (*ShapeLines)[3];
				x2 = (*ShapeLines)[4];
				y2 = (*ShapeLines)[4];
//          y2=(*ShapeLines)[5];
				x3 = (*ShapeLines)[6];
				y3 = (*ShapeLines)[7];
				x4 = (*ShapeLines)[8];
				y4 = (*ShapeLines)[9];
				NewObject.x1 = (float) x1;
				NewObject.y1 = (float) y1;
				NewObject.x2 = (float) x2;
				NewObject.y2 = (float) y2;
				NewObject.x3 = (float) x3;
				NewObject.y3 = (float) y3;
				NewObject.x4 = (float) x4;
				NewObject.y4 = (float) y4;
				NewObject.ObjectType = OBJECT_ARC;
				NewObject.Thickness = (*ShapeLines)[10];
				NewObject.Clearance = (*ShapeLines)[11];
				AddObject(&NewObject);
				break;

			default:
				ok = 1;
				break;
			}

			switch (Pad->ShapeType)
			{
			case PIN_ARC:
			case OBJECT_ARC:
				MemPos += 48;
				break;

			default:
				MemPos += sizeof(PadRecord);
				break;
			}

			NrPinShapes--;
		}

		memset(&NewPinInfo, 0, sizeof(PinInfoRecord));
		memmove(&NewPinInfo.PinText, &ShapePad->Name, sizeof(NewPinInfo.PinText) - 1);
		AddPinInfo(&NewPinInfo);
		NrPins--;
		PinNr++;
	}

// **************************************************************************************

// PlacementOutlines

	MemPos = sizeof(ShapeRecord);
	NrLines = min(16384, Shape.NrPlacementOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeInfoP = (int32 *) ShapeLines;
		ShapeInfo = *ShapeInfoP;
		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		memset(&NewObject, 0, sizeof(ObjectRecord));
		NewObject.Layer = PLACEMENT_OUTLINE_LAYER;

		NewObject.ObjectType = (int16) ShapeInfo;
		NewObject.x1 = (float) x1;
		NewObject.y1 = (float) y1;
		NewObject.x2 = (float) x2;

		switch (ShapeInfo)
		{
		case OBJECT_LINE:		// line
			NewObject.y2 = (float) y2;
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			NewObject.y2 = (float) y2;
			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			NewObject.Info2 = (uint8) (y2 + 0.1);
			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			if (NewObject.y2 == 0)
				NewObject.y2 = NewObject.x2;

			if (NewObject.x2 == 0)
				NewObject.x2 = NewObject.y2;

			NewObject.x3 = (float) (*ShapeLines)[5];
			NewObject.y3 = (float) (*ShapeLines)[6];
			NewObject.x4 = (float) (*ShapeLines)[7];
			NewObject.y4 = (float) (*ShapeLines)[8];
			MemPos += 40;
			break;
			/*
			      case OBJECT_TEXT:    // text
			        NewObject.y2=0.0;
			        NewObject.Info2=(int16)y2;
			        memmove(NewObject.Text,&((*ShapeLines)[6]),sizeof(NewObject.Text));
			        MemPos+=24+64;
			        break;
			*/
		}

		NewObject.PinNr = -1;
		AddObject(&NewObject);
		NrLines--;
	}

// **************************************************************************************

	MemPos = Shape.CompOutLineOffset;
	NrLines = min(16384, Shape.NrCompOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeInfoP = (int32 *) ShapeLines;
		ShapeInfo = *ShapeInfoP;
		ShapeInfo &= 0x0000fff0;

		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		memset(&NewObject, 0, sizeof(ObjectRecord));
		NewObject.Layer = COMP_OUTLINE_LAYER;

		NewObject.ObjectType = (int16) ShapeInfo;
		NewObject.x1 = (float) x1;
		NewObject.y1 = (float) y1;
		NewObject.x2 = (float) x2;
		NewObject.PinNr = -1;

		switch (ShapeInfo)
		{
		case OBJECT_LINE:		// line
			NewObject.y2 = (float) y2;
			NewObject.Thickness = (*ShapeLines)[5];
			AddObject(&NewObject);
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			NewObject.y2 = (float) y2;
			NewObject.Thickness = (*ShapeLines)[5];
			AddObject(&NewObject);
			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			NewObject.Info2 = (uint8) (y2 + 0.1);
			NewObject.Thickness = (*ShapeLines)[5];

			if (NewObject.Thickness == 0.0)
				NewObject.Info2 = 0;
			else
			{
				if (NewObject.Info2 == 0)
					NewObject.Info2 = 15;
			}

			AddObject(&NewObject);
			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			NewObject.y2 = (float) y2;

			if (NewObject.y2 == 0)
				NewObject.y2 = NewObject.x2;

			if (NewObject.x2 == 0)
				NewObject.x2 = NewObject.y2;

			NewObject.x3 = (*ShapeLines)[5];
			NewObject.y3 = (*ShapeLines)[6];
			NewObject.x4 = (*ShapeLines)[7];
			NewObject.y4 = (*ShapeLines)[8];
			NewObject.Thickness = (*ShapeLines)[9];
			AddObject(&NewObject);
			MemPos += 40;
			break;

		case OBJECT_TEXT:		// line
			memcpy(&rot, &(*ShapeLines)[4], sizeof(float));

			if (y2 > 10.0)
			{
				// Floating point number
				NewObject.RotationAngle = y2 - 2000.0;
			}
			else
			{
				rot = (int32) (y2 + 0.1);
				rot = ((rot & 3) << 1) + ((rot & 4) >> 2);
				NewObject.RotationAngle = (float) rot *45.0;
			}

			NewObject.y2 = (float) 0.0;
			NewObject.Thickness = (*ShapeLines)[5];
			memmove(NewObject.Text, &((*ShapeLines)[6]), sizeof(NewObject.Text));
			AddObject(&NewObject);
			MemPos += 24 + 64;
			break;

		case OBJECT_POLYGON:
		case PIN_SMD_POLYGON:
			PolygonPointer = (int32 *) & (*ShapeLines)[3];
			ObjectPolygon = (ObjectPolygonRecord *) & (ShapesMem[*PolygonPointer]);
			PolygonVertices = ObjectPolygon->NrVertices;
			memmove(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
			MoveObjectPolygon(NewObjectPolygon, x1, y1, 0);
			NewObjectPolygon->Info = 0;
			NewObjectPolygon->Layer = COMP_OUTLINE_LAYER;
			AddObjectPolygon(NewObjectPolygon);
			MemPos += 16;
			break;

		default:
			ok = 1;
		}

		NrLines--;
	}


// **************************************************************************************

	MemPos = Shape.SilkScreenOffset;
	NrLines = min(16384, Shape.NrSilkScreenOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

		if (MemPos == 226484)
			ok = 1;

#endif
		ShapeInfoP = (int32 *) ShapeLines;
		ShapeInfo = *ShapeInfoP;
//    TextLength=ShapeInfo & 0xff;
		ShapeInfo &= 0x0000ff00;

		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		memset(&NewObject, 0, sizeof(ObjectRecord));
		NewObject.Layer = SILKSCREEN_TOP_LAYER;

		NewObject.ObjectType = (int16) ShapeInfo;
		NewObject.x1 = (float) x1;
		NewObject.y1 = (float) y1;
		NewObject.x2 = (float) x2;
		NewObject.PinNr = -1;

		switch (ShapeInfo)
		{
		case OBJECT_LINE:		// line
			NewObject.y2 = (float) y2;
			NewObject.Thickness = (*ShapeLines)[5];
			AddObject(&NewObject);
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			NewObject.y2 = (float) y2;
			NewObject.Thickness = (*ShapeLines)[5];
			AddObject(&NewObject);
			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			NewObject.Info2 = (uint8) (y2 + 0.1);
			NewObject.Thickness = (*ShapeLines)[5];

			if (NewObject.Thickness == 0.0)
				NewObject.Info2 = 0;
			else
			{
				if (NewObject.Info2 == 0)
					NewObject.Info2 = 15;
			}

			AddObject(&NewObject);
			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			if (NewObject.y2 == 0)
				NewObject.y2 = NewObject.x2;

			if (NewObject.x2 == 0)
				NewObject.x2 = NewObject.y2;

			NewObject.x3 = (*ShapeLines)[5];
			NewObject.y3 = (*ShapeLines)[6];
			NewObject.x4 = (*ShapeLines)[7];
			NewObject.y4 = (*ShapeLines)[8];
			NewObject.Thickness = (*ShapeLines)[9];
			AddObject(&NewObject);
			MemPos += 40;
			break;

		case OBJECT_TEXT:		// Text
			if (y2 > 10.0)
			{
				// Floating point number
				NewObject.RotationAngle = y2 - 2000.0;
			}
			else
			{
				rot = (int32) (y2 + 0.1);
				rot = ((rot & 3) << 1) + ((rot & 4) >> 2);
				NewObject.RotationAngle = (float) rot *45.0;
			}

			NewObject.y2 = (float) 0.0;
			NewObject.Thickness = (*ShapeLines)[5];
			memmove(&NewObject.Text, &((*ShapeLines)[6]), sizeof(NewObject.Text));
			AddObject(&NewObject);
			MemPos += 24 + 64;
			break;

		case OBJECT_POLYGON:
		case PIN_SMD_POLYGON:
			PolygonPointer = (int32 *) & (*ShapeLines)[3];
			ObjectPolygon = (ObjectPolygonRecord *) & (ShapesMem[*PolygonPointer]);
			PolygonVertices = ObjectPolygon->NrVertices;
			memmove(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
			MoveObjectPolygon(NewObjectPolygon, x1, y1, 0);
			NewObjectPolygon->Layer = SILKSCREEN_TOP_LAYER;
			NewObjectPolygon->Info = 0;
			AddObjectPolygon(NewObjectPolygon);
			MemPos += 16;
			break;
		}

		NrLines--;
	}


// **************************************************************************************


	MemPos = Shape.OtherObjectsOffset;
	NrPinShapes = min(65536, Shape.NrOtherObjects);
	cnt = 0;

	while (NrPinShapes > 0)
	{
		Pad = (PadRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

		if (MemPos >= 365404)
			ok = 1;

#endif
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		memset(&NewObject, 0, sizeof(ObjectRecord));
		NewObject.Layer = Pad->Layer;

#ifdef _DEBUG

		if (Pad->Layer == POWER_PAD_LAYER)
			ok = 1;

#endif
		/*
		    if (NewObject.Layer==1000) {
		      NewObject.Layer=ROUTING_KEEPOUT_LAYER;
		    }
		*/
		NewObject.ObjectType = (int16) Pad->ShapeType;
		NewObject.x1 = Pad->X;
		NewObject.y1 = Pad->Y;
		NewObject.x2 = Pad->Width;
		NewObject.y2 = Pad->Height;
		NewObject.PinNr = -1;

		switch (Pad->ShapeType)
		{
		case PIN_SMD_ROUND:
		case OBJECT_CIRCLE:
			NewObject.Info2 = (uint8) (NewObject.y2 + 0.1);
			NewObject.ObjectType = OBJECT_CIRCLE;
			NewObject.Thickness = Pad->Special.Thickness;

			if (NewObject.Thickness == 0.0)
				NewObject.Info2 = 0;
			else
			{
				if (NewObject.Info2 == 0)
					NewObject.Info2 = 15;
			}

			NewObject.y2 = 0.0;
			AddObject(&NewObject);
			break;

		case PIN_SMD_RECT:
		case OBJECT_RECT:
			NewObject.ObjectType = OBJECT_RECT;
			NewObject.Thickness = Pad->Special.Thickness;
			AddObject(&NewObject);
			break;

		case PIN_LINE_HOR:
			NewObject.ObjectType = OBJECT_LINE;
			NewObject.Thickness = Pad->Height;
			NewObject.x2 += NewObject.x1;
			NewObject.y2 = NewObject.y1;
			AddObject(&NewObject);
			break;

		case PIN_LINE_VER:
			NewObject.ObjectType = OBJECT_LINE;
			NewObject.Thickness = Pad->Height;
			NewObject.x2 = NewObject.x1;
			NewObject.y2 += NewObject.y1;
			AddObject(&NewObject);
			break;

		case PIN_LINE_DIAG1:
			NewObject.ObjectType = OBJECT_LINE;
			NewObject.Thickness = Pad->Height;
			NewObject.y2 = NewObject.y1 - NewObject.x2;
			NewObject.x2 += NewObject.x1;
			AddObject(&NewObject);
			break;

		case PIN_LINE_DIAG2:
			NewObject.ObjectType = OBJECT_LINE;
			NewObject.Thickness = Pad->Height;
			NewObject.y2 = NewObject.y1 + NewObject.x2;
			NewObject.x2 += NewObject.x1;
			AddObject(&NewObject);
			break;

		case PIN_ARC:
		case OBJECT_ARC:
#ifdef _DEBUG
			if ((NewObject.Layer >= ROUTING_KEEPOUT_LAYER) && (NewObject.Layer < ROUTING_KEEPOUT_LAYER + 32))
				ok = 1;

#endif
			x1 = (*ShapeLines)[2];
			y1 = (*ShapeLines)[3];
			x2 = (*ShapeLines)[4];
			y2 = (*ShapeLines)[5];
			x3 = (*ShapeLines)[6];
			y3 = (*ShapeLines)[7];
			x4 = (*ShapeLines)[8];
			y4 = (*ShapeLines)[9];

			switch (NewObject.Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
				y2 = x2;
				break;
			}

			NewObject.Thickness = (*ShapeLines)[10];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			NewObject.x1 = (float) x1;
			NewObject.y1 = (float) y1;
			NewObject.x2 = (float) x2;
			NewObject.y2 = (float) y2;
			NewObject.x3 = (float) x3;
			NewObject.y3 = (float) y3;
			NewObject.x4 = (float) x4;
			NewObject.y4 = (float) y4;
			NewObject.ObjectType = OBJECT_ARC;
			AddObject(&NewObject);
			break;

		case PIN_LINE_ALL_ANGLE:
		case OBJECT_LINE:
			NewObject.Thickness = Pad->Special.Thickness;
			AddObject(&NewObject);
			break;

		case OBJECT_TEXT:		// Text
			TextP = (LPSTR) & ((*ShapeLines)[7]);
#ifdef _DEBUG

			if (NewObject.Layer == SILKSCREEN_TOP_LAYER)
				ok = 1;

			if (strcmp(TextP, "   Info1_45") == 0)
				ok = 1;

#endif

			if (NewObject.y2 > 10.0)
			{
				// Floating point number
				NewObject.RotationAngle = NewObject.y2 - 2000.0;
			}
			else
			{
				rot = (int32) (NewObject.y2 + 0.1);
				rot = ((rot & 3) << 1) + ((rot & 4) >> 2);
				NewObject.RotationAngle = (float) rot *45.0;
			}

			NewObject.y2 = (float) 0.0;
#ifdef _DEBUG

			if (NewObject.Layer == SILKSCREEN_TOP_LAYER)
				ok = 1;

			if (strcmp(TextP, "   Info1_0") == 0)
			{
				if (NewObject.RotationAngle > 1000.0)
					ok = 1;
			}

#endif

			NewObject.Thickness = (*ShapeLines)[6];
			memcpy(&NewObject.Text, TextP, sizeof(NewObject.Text));
			AddObject(&NewObject);
			break;

		case OBJECT_POLYGON:
		case PIN_SMD_POLYGON:
#ifdef _DEBUG
			cnt = Pad->Special.AddressOffset;

			if (NewObject.Layer == PASTE_MASK_BOTTOM_LAYER)
				ok = 1;

#endif
			ObjectPolygon = (ObjectPolygonRecord *) & (ShapesMem[Pad->Special.AddressOffset]);
			PolygonVertices = ObjectPolygon->NrVertices;
			memmove(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
			MoveObjectPolygon(NewObjectPolygon, Pad->X, Pad->Y, 0);
			NewObjectPolygon->Layer = Pad->Layer;
			/*
			        if (NewObjectPolygon.Layer==1000) {
			          NewObjectPolygon.Layer=ROUTING_KEEPOUT_LAYER;
			        }
			*/
			NewObjectPolygon->PinNr = -1;
			NewObjectPolygon->Clearance = 0.0;
			NewObjectPolygon->Info = 0;
			AddObjectPolygon(NewObjectPolygon);
			break;
		}

		switch (Pad->ShapeType)
		{
		case PIN_ARC:
		case OBJECT_ARC:
			MemPos += 48;
			break;

		case OBJECT_TEXT:		// Text
			MemPos += 28 + 64;
			break;

		default:
			MemPos += sizeof(PadRecord);
			break;
		}

		NrPinShapes--;
		cnt++;
	}

	GetSymbolNameFromFileName(EditFile, str);

	if (stricmpUTF8(str, Shape.ShapeName) != 0)
	{
		memset(&Shape.ShapeName, 0, sizeof(Shape.ShapeName));
		strcpy(Shape.ShapeName, str);
		FileChanged = 1;
	}


	memset(&NewObject, 0, sizeof(ObjectRecord));
	NewObject.Layer = GEOM_NAME_LAYER;
	NewObject.ObjectType = OBJECT_TEXT;
	NewObject.x1 = Shape.ShapeNameOriginX;
	NewObject.y1 = Shape.ShapeNameOriginY;
	NewObject.x2 = Shape.ShapeNameHeight;
	NewObject.Info2 = 0;

	if (Shape.ShapeNameRotation == 1)
		NewObject.RotationAngle = 90.0;
	else
		NewObject.RotationAngle = Shape.ShapeNameRotation;

	NewObject.Thickness = (float) CurrentSilkscreenLine;
	strcpy(NewObject.Text, Shape.ShapeName);
	AddObject(&NewObject);

	FileChanged = 0;
	DataBaseChanged = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		Object->Info = 0;
		Object->AddNr = 0;
		Object->DeleteNr = 0;
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		ObjectPolygon->Info = 0;
		ObjectPolygon->AddNr = 0;
		ObjectPolygon->DeleteNr = 0;
		SetMinMaxObjectPolygon(ObjectPolygon, 0);
	}

	DeleteGraphicObjects();
	CreateDrawObjects();

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{	// ObjectPolygonMemorySize
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectPolygon->ShapePolygonPosition = 100000000;
#ifdef _DEBUG

			if (ObjectPolygon->ShapePolygonPosition == 231512)
				ok = 1;

#endif
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeShapeName(LPSTR Name)
{
	int32 cnt, ObjectType;
	ObjectRecord *Object;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectType = Object->ObjectType;

			if ((Object->Layer == GEOM_NAME_LAYER) && (ObjectType == OBJECT_TEXT))
			{
				strcpy(Object->Text, Name);
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeBackup(LPSTR CurrentFile)
{
	char DirPart[MAX_LENGTH_STRING], FilePart[MAX_LENGTH_STRING], BackupFile1[MAX_LENGTH_STRING];

	if ((GetDirFromFileName(DirPart, CurrentFile) == 0) && (GetFilePartFromFileName(FilePart, CurrentFile) == 0))
	{
		sprintf(BackupFile1, "%s\\backup\\%s", DirPart, FilePart);
		CopyFileUTF8(CurrentFile, BackupFile1, 0);
	}
	else
		return -2;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShowFileAndLine(LPSTR SourceFile, int32 LineNr)
{
	char MessageStr[MAX_LENGTH_STRING];

	sprintf(MessageStr, "%s :  L : %d", SourceFile, LineNr);
	MessageBoxUTF8(NULL, MessageStr, SC(4, "Message"), MB_APPLMODAL | MB_OK);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SaveFile(int32 mode)
{
	int32 fp, res, Written, cnt, SpecialPolygons;
	char str2[MAX_LENGTH_STRING];
	ObjectRecord *Object;
	ObjectPolygonRecord *ObjectPolygon;

	if (((mode & 4) == 0) && (!FileChanged))
		return 0;

	if (LibraryFile[0] != 0)
	{
		MessageBoxUTF8(GEOMWindow, SC(211, "Can not save a file in a library"), SC(48, "Error"), MB_APPLMODAL | MB_OK);
		return -2;
	}

	if ((CheckFilenameForSaving) || (mode & 4) || (EditFile[0] == 0))
	{
		if (CheckFilenameForSaving)
			res = SaveFileName(1);
		else
			res = SaveFileName(0);

		if (res == 0)
		{
			ShowFileAndLine(__FILE__, __LINE__);
			return -1;
		}

		if (res == 2)
		{
			ShowFileAndLine(__FILE__, __LINE__);
			return 0;
		}
	}

	if (AllocateMemShapes(65536) != 0)
	{
		ShowFileAndLine(__FILE__, __LINE__);
		return -1;
	}

	CheckFilenameForSaving = 0;

	if ((mode & 4) == 0)
		MakeBackup(EditFile);

// *******************************************************************************************************

	res = CheckPads(mode);

	switch (res)
	{
	case -1:
		ShowFileAndLine(__FILE__, __LINE__);
		return -2;

	case 0:
		if ((mode & 2) == 0)
		{
			NrObjects = NrObjectsBeforeSaving;
			return -2;
		}

		break;
	}

	if (CollectObjects() != 1)
	{
		NrObjects = NrObjectsBeforeSaving;
		ShowFileAndLine(__FILE__, __LINE__);
		return -1;
	}

	FileChanged = 0;

	if ((fp = CheckForWritingAndOpenUTF8(EditFile, BufPos, GEOMWindow)) < 1)
	{
		ShowFileAndLine(__FILE__, __LINE__);
		NrObjects = NrObjectsBeforeSaving;
		return -1;
	}

	Shape.MemSize = BufPos;
	GetSymbolNameFromFileName(EditFile, str2);
	str2[sizeof(Shape.ShapeName) - 1] = 0;

	if (stricmpUTF8(Shape.ShapeName, str2) != 0)
	{
		ChangeShapeName(str2);
		memset(&Shape.ShapeName, 0, sizeof(Shape.ShapeName));
		strcpy(Shape.ShapeName, str2);
	}

	cnt = sizeof(ShapeRecord);

	if (Shape.ShapeNameHeight == 0)
		Shape.ShapeNameHeight = (60 * 2540);

	Shape.Info = 0;

	if (SMDdevice == 1)
		Shape.Info |= SMD_DEVICE;

	memmove(ShapesMem, &Shape, sizeof(ShapeRecord));
	SpecialPolygons = 0;

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		        && (((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
		            || (ObjectPolygon->NrVertices > 500)))
			SpecialPolygons = 1;
	}

	if (SpecialPolygons == 0)
		strcpy((LPSTR) ShapesMem, ShapeCode2);
	else
		strcpy((LPSTR) ShapesMem, ShapeCode3);

	if (FileWrite(fp, ShapesMem, BufPos, &Written) == -1)
	{
		NrObjects = NrObjectsBeforeSaving;
		ShowFileAndLine(__FILE__, __LINE__);
		return -1;
	}

	FileClose(fp);

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		/*
		    if ((Object->Info & (OBJECT_NOT_VISIBLE|OBJECT_TO_BE_DELETED)) == OBJECT_TO_BE_DELETED) {
		      Object->Info|=OBJECT_NOT_VISIBLE;
		      Object->AddNr=0;
		      Object->DeleteNr=0;
		    }
		*/
		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
			Object->Info = 0;
	}

	DeAllocateMemShapes();

	NrObjects = NrObjectsBeforeSaving;
	DataBaseChanged = 0;

	if (MasterWindow != NULL)
		PostMessage(MasterWindow, WM_COMMAND, (WPARAM) ID_FILE_RELOADGEOMETRIES, (LPARAM) NULL);

	if (mode & 4)
		RePaint();

	SetWindowName(0);
	return 1;
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
		sprintf(str, SC(212, "File %s has changed, save ?"), EditFile);
		res = MessageBoxUTF8(GEOMWindow, str, SC(4, "Message"), MB_YESNOCANCEL);

		if (res == IDYES)
		{
			res = SaveFile(0);

			switch (res)
			{
			case -2:
				return 0;

			case -1:
				MessageBoxUTF8(GEOMWindow, SC(29, "Error in saving file"), SC(48, "Error"), MB_APPLMODAL | MB_OK);
				return 1;

			case 0:
				return 1;
			}
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

void SetWindowName(int32 mode)
{
	char str[MAX_LENGTH_STRING];

	if (EditFile[0] == 0)
		strcpy(str, SC(1, "New geometry"));
	else
	{
		if (LibraryFile[0] != 0)
			sprintf(str, SC(213, "Geometry %s [ library %s ]"), EditFile, LibraryFile);
		else
			sprintf(str, SC(214, "Geometry %s"), EditFile);
	}

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

	if (GEOMWindow != NULL)
	{
//    MessageBoxUTF8(GEOMWindow,str,"Window text",MB_APPLMODAL+MB_OK);
		SetWindowTextUTF8(GEOMWindow, str);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPads(int32 mode)
{
	int32 cnt, Layer, cnt2, cnt3, Layer2, PinNr, count, NrDrills, SaveError, DrillPinNr, ThrouHoleInfo, res,
	      NrInnerPadObjects, AntiPowerPadsLayerInfo, NrPadsInnerSelected, GeomNameObjectCnt, InnerPadMask, ObjectInfo,
	      ObjectType, Found, First, ObjectsAdded, Warning;
	uint32 ThrouHoleLayerInfo;
	ObjectRecord *Object, *Object2, *ObjectTopLayer, *ObjectBottomLayer, NewObject, *DrillObject, *FirstInnerPadObject,
	             *InnerPadObjects[32], *InnerPadObject, *PowerPadObject, *GeomNameObject, *ObjectPasteBottomLayer,
	             *ObjectPasteTopLayer, *DrillObject2, *ObjectSoldBottomLayer, *ObjectSoldTopLayer;
	double DrillX1, DrillY1, x1, y1, PadX1, PadY1;
	ObjectPolygonRecord *ObjectPolygon;

	PinInfoRecord *PinInfo;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], ValueStr[MAX_LENGTH_STRING],
	     TextStr[MAX_LENGTH_STRING];

	SaveError = 0;
	MessageBufPos = 0;
	First = 1;
	Warning = 0;
	GeomNameObjectCnt = -1;
	ObjectTopLayer = NULL;
	ObjectBottomLayer = NULL;
	GeomNameObject = NULL;
	FirstInnerPadObject = NULL;
	PowerPadObject = NULL;
	ObjectsAdded = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		Object->Info &= ~(OBJECT_SELECTED | OBJECT_SAVED | 15);
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		ObjectPolygon->Info &= ~(OBJECT_SELECTED | OBJECT_SAVED);
	}

// *******************************************************************************************************

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			for (cnt2 = cnt + 1; cnt2 < NrObjects; cnt2++)
			{
				Object2 = &((*Objects)[cnt2]);

				if ((cnt2 != cnt) && ((Object2->Info & (OBJECT_NOT_VISIBLE)) == 0))
				{
					if ((InRange(Object->x1, Object2->x1)) && (InRange(Object->y1, Object2->y1))
					        && (Object->ObjectType == Object2->ObjectType))
					{
						x1 = Object->x1;
						y1 = Object->y1;

						if (Units == 0)
						{
							x1 = x1 / 2540.0;
							y1 = y1 / 2540.0;
							sprintf(ValueStr, "%.1f,%.1f", x1, y1);
						}
						else
						{
							x1 = x1 / 100000.0;
							y1 = y1 / 100000.0;
							sprintf(ValueStr, "%.4f,%.4f", x1, y1);
						}

						switch (Object->ObjectType)
						{
						case OBJECT_RECT:
						case OBJECT_CIRCLE:
						case OBJECT_LINE:
							if ((Object->Layer == Object2->Layer)
							        && (((Object->Layer >= 0) && (Object->Layer < 32))
							            || ((Object->Layer >= ROUTING_KEEPOUT_LAYER)
							                && (Object->Layer < ROUTING_KEEPOUT_LAYER + 32))
							            || (Object->Layer == SOLD_MASK_BOTTOM_LAYER)
							            || (Object->Layer == SOLD_MASK_TOP_LAYER)
							            || (Object->Layer == PASTE_MASK_BOTTOM_LAYER)
							            || (Object->Layer == PASTE_MASK_TOP_LAYER) || (Object->Layer == DRILL_LAYER)
							            || (Object->Layer == DRILL_UNPLATED_LAYER) || (Object->Layer == INNER_PAD_LAYER)
							            || (Object->Layer == POWER_PAD_LAYER)))
							{
								switch (Object->ObjectType)
								{
								case OBJECT_RECT:
									if ((InRange(Object->x2, Object2->x2)) && (InRange(Object->y2, Object2->y2)))
										Object2->Info |= OBJECT_NOT_VISIBLE;
									else
									{
										if ((Object2->Info & 1) == 0)
										{
											sprintf(str, SC(215, "Two objects overlaps\t\t%s"), ValueStr);

											if (AddToMessageBuf(str) != 0)
												return -1;

											Object2->Info |= 1;
											SaveError = 1;
										}
									}

									break;

								case OBJECT_CIRCLE:
									if (InRange(Object->x2, Object2->x2))
										Object2->Info |= OBJECT_NOT_VISIBLE;
									else
									{
										if ((Object2->Info & 1) == 0)
										{
											sprintf(str, SC(215, "Two objects overlaps\t\t%s"), ValueStr);

											if (AddToMessageBuf(str) != 0)
												return -1;

											Object2->Info |= 1;
											SaveError = 1;
										}
									}

									break;

								case OBJECT_LINE:
									if ((InRange(Object->x2, Object2->x2)) && (InRange(Object->y2, Object2->y2)))
									{
										if (InRange(Object->Thickness, Object2->Thickness))
											Object2->Info |= OBJECT_NOT_VISIBLE;
										else
										{
											if ((Object2->Info & 1) == 0)
											{
												sprintf(str, SC(215, "Two objects overlaps\t\t%s"), ValueStr);

												if (AddToMessageBuf(str) != 0)
													return -1;

												Object2->Info |= 1;
												SaveError = 1;
											}
										}
									}

									break;
								}
							}

							break;
						}
					}
				}
			}
		}
	}

// *******************************************************************************************************

	NrDrillsSelected = 0;
	NrDrillsUnplatedSelected = 0;
	NrAntiPowerpadsSelected = 0;
	NrPadsInnerSelected = 0;
	NrSilkTopObjectsSelected = 0;
	NrSilkBottomObjectsSelected = 0;
	NrCompOutlinesSelected = 0;
	NrPlacemOutlinesSelected = 0;
	NrMaskTopObjectsSelected = 0;
	NrMaskBottomObjectsSelected = 0;
	NrPasteTopObjectsSelected = 0;
	NrPasteBottomObjectsSelected = 0;
	NrLinesSelected = 0;
	NrRectsSelected = 0;
	NrCirclesSelected = 0;
	NrArcsSelected = 0;
	NrTextsSelected = 0;
	NrPolygonsSelected = 0;
	GeomNameSelected = 0;
	NrBoardOutlinesSelected = 0;
	NrInfo1ObjectsSelected = 0;
	NrInfo2ObjectsSelected = 0;
	NrInfo3ObjectsSelected = 0;
	NrInfo4ObjectsSelected = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		Object->Info &= ~(OBJECT_SELECTED | 15);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = Object->Layer;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
				NrMaskBottomObjectsSelected++;
				break;

			case SOLD_MASK_TOP_LAYER:
				NrMaskTopObjectsSelected++;
				break;

			case PASTE_MASK_TOP_LAYER:
				NrMaskTopObjectsSelected++;
				break;

			case PASTE_MASK_BOTTOM_LAYER:
				NrPasteBottomObjectsSelected++;
				break;

			case SILKSCREEN_TOP_LAYER:
				NrSilkTopObjectsSelected++;
				break;

			case SILKSCREEN_BOTTOM_LAYER:
				NrSilkBottomObjectsSelected++;
				break;

			case PLACEMENT_OUTLINE_LAYER:
				NrPlacemOutlinesSelected++;
				break;

			case COMP_OUTLINE_LAYER:
				NrCompOutlinesSelected++;
				break;

			case BOARD_OUTLINE_LAYER:
				NrBoardOutlinesSelected++;
				break;

			case GEOM_NAME_LAYER:
				if (!GeomNameObject)
				{
					GeomNameObject = Object;
					GeomNameObjectCnt = cnt;
				}

				break;

			case POWER_PAD_LAYER:
				NrAntiPowerpadsSelected++;
				break;

			case INNER_PAD_LAYER:
				NrPadsInnerSelected++;
				break;

			case DRILL_LAYER:
				NrDrillsSelected++;
				break;

			case DRILL_UNPLATED_LAYER:
				NrDrillsUnplatedSelected++;
				break;

			case INFO_LAYER:
				NrInfo1ObjectsSelected++;
				break;

			case INFO_LAYER2:
				NrInfo2ObjectsSelected++;
				break;

			case INFO_LAYER3:
				NrInfo3ObjectsSelected++;
				break;

			case INFO_LAYER4:
				NrInfo4ObjectsSelected++;
				break;

			default:
				break;
			}
		}
	}

	NrObjectsBeforeSaving = NrObjects;

	if (NrCompOutlinesSelected == 0)
	{
		if (AddToMessageBuf(SC(216, "There were no component outlines defined")) != 0)
			return -1;

		if ((mode & 2) == 0)
			SaveError = 1;
	}

	if (NrPlacemOutlinesSelected == 0)
	{
		if (AddToMessageBuf(SC(217, "There were no placement outlines defined")) != 0)
			return -1;

		if ((mode & 2) == 0)
			SaveError = 1;
	}

	/*
	  if (MessageBufPos!=0) {
	    MessageDialog(SC(49,"Warning"),1);
	    InvalidateRect(GEOMWindow,NULL,0);
	    PostMessage(GEOMWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
	//    return 0;
	  }
	  MessageBufPos=0;
	*/
// *****************************************************************************************
// *****************************************************************************************


	for (cnt = 0; cnt < NrPinObjects; cnt++)
	{
		PinInfo = &((*PinInfos)[cnt]);
		PinInfo->NrObjects = 0;
	}

	First = 1;
	NrDrills = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			PinNr = Object->PinNr;

			if (Object->Layer == DRILL_LAYER)
			{
				if (NrDrills < MaxNrPins)
				{
					DrillObjects[NrDrills] = cnt;
					NrDrills++;
				}
			}
		}
	}

	for (cnt = 0; cnt < NrDrills; cnt++)
	{
		DrillObject = &((*Objects)[DrillObjects[cnt]]);

		if ((DrillObject->Info & OBJECT_SAVED) == 0)
		{
			DrillPinNr = DrillObject->PinNr;
			PinInfo = &((*PinInfos)[DrillPinNr]);
			cnt3 = 0;
			DrillX1 = DrillObject->x1;
			DrillY1 = DrillObject->y1;

			for (cnt2 = cnt + 1; cnt2 < NrDrills; cnt2++)
			{
				DrillObject2 = &((*Objects)[DrillObjects[cnt2]]);
#ifdef _DEBUG

				if (cnt2 == 5)
					ok = 1;

#endif

				if ((DrillObject2->Info & OBJECT_SAVED) == 0)
				{
					if (DrillPinNr == DrillObject2->PinNr)
					{
						if (cnt3 == 0)
						{
							GetUnitsValue(Units, DrillObject->x1, str2, 0);
							GetUnitsValue(Units, DrillObject->y1, str3, 0);
							sprintf(str, SC(276, "\r\nThe following pins use the same pinname (%s)"), PinInfo->PinText);

							if (AddToMessageBuf(str) != 0)
								return -1;

							sprintf(str, "%s\t%s", str2, str3);

							if (AddToMessageBuf(str) != 0)
								return -1;
						}

						GetUnitsValue(Units, DrillObject2->x1, str2, 0);
						GetUnitsValue(Units, DrillObject2->y1, str3, 0);
						sprintf(str, "%s\t%s", str2, str3);

						if (AddToMessageBuf(str) != 0)
							return -1;

						DrillObject2->Info |= OBJECT_SAVED;
						cnt3 = 1;
					}
				}
			}
		}
	}

	/*
	  if (MessageBufPos!=0) {
	    Warning=1;
	    MessageDialog(SC(49,"Warning"),1);
	    InvalidateRect(GEOMWindow,NULL,0);
	    PostMessage(GEOMWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
	  }
	  MessageBufPos=0;
	*/
	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		Object->Info &= ~(OBJECT_SELECTED | OBJECT_SAVED | 15);
	}

	for (cnt = 0; cnt < NrDrills; cnt++)
	{
		DrillObject = &((*Objects)[DrillObjects[cnt]]);
		ObjectInfo = DrillObject->Info;
		DrillPinNr = DrillObject->PinNr;
		DrillX1 = DrillObject->x1;
		DrillY1 = DrillObject->y1;

		FirstInnerPadObject = NULL;
		NrInnerPadObjects = 0;
		ThrouHoleInfo = 0;
		ThrouHoleLayerInfo = (uint32) - 1;
		AntiPowerPadsLayerInfo = 0;
		InnerPadMask = 0;

		for (cnt2 = 0; cnt2 < NrPadLayers; cnt2++)
		{
			ThrouHoleLayerInfo &= ~(1 << cnt2);

			if ((NrPadLayers > 2) && (cnt2 > 0) && (cnt2 < NrPadLayers - 1))
				InnerPadMask |= 1 << cnt2;
		}

		for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
		{
			Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

			if (cnt2 == 5)
				ok = 1;

			if ((InRange5(Object->x1, -22.2e5)) && (InRange5(Object->y1, -14.6e5)))
				ok = 1;

			if ((InRange5(DrillX1, 150.5e5)) && (InRange5(DrillY1, 108.1e5)))
			{
				ok = 1;

				if ((InRange5(Object->x1, 150.5e5)) && (InRange5(Object->y1, 108.1e5)))
				{
					ok = 1;

					if (Object->Layer == SOLD_MASK_TOP_LAYER)
						ok = 1;
				}
			}

#endif
			ObjectType = Object->ObjectType;

			if (((Object->Info & (OBJECT_NOT_VISIBLE)) == 0) && (InRange(DrillX1, Object->x1))
			        && (InRange(DrillY1, Object->y1)) && (ObjectType != OBJECT_LINE) && (ObjectType != OBJECT_ARC))
			{
				Layer2 = Object->Layer;

				switch (Layer2)
				{
				case POWER_PAD_LAYER:
					PowerPadObject = Object;
					ThrouHoleInfo |= 4;
					break;

				case INNER_PAD_LAYER:
					InnerPadObject = Object;
					FirstInnerPadObject = Object;
					ThrouHoleInfo |= 8;
					break;

				case SOLD_MASK_BOTTOM_LAYER:	// Solder mask bottom layer
					ThrouHoleInfo |= 16;
					break;

				case SOLD_MASK_TOP_LAYER:	// Solder mask top layer
					ThrouHoleInfo |= 32;
					break;

				default:
					if ((Object->PinNr == DrillPinNr) && (Layer2 < 32))
					{
						ThrouHoleInfo |= 64;	// Does have a pinname

						if (Layer2 == 0)
						{	// Bottom layer
							ObjectBottomLayer = Object;
							ThrouHoleInfo |= 2;
							ThrouHoleLayerInfo |= 1 << Layer2;
						}

						if (Layer2 == NrPadLayers - 1)
						{	// Top layer
							ObjectTopLayer = Object;
							ThrouHoleInfo |= 1;
							ThrouHoleLayerInfo |= 1 << Layer2;
						}

						if ((CheckIfInnerLayer(Layer2)) && (Object->ObjectType == OBJECT_CIRCLE))
						{
							if (FirstInnerPadObject != NULL)
							{
								if (InRange(Object->x2, FirstInnerPadObject->x2))
									ThrouHoleLayerInfo |= 1 << Layer2;
								else
								{
									ThrouHoleInfo |= 256;	// Different inner pads sizes
								}
							}
							else
							{
								FirstInnerPadObject = Object;
								ThrouHoleLayerInfo |= 1 << Layer2;
							}

							if (NrInnerPadObjects < 32)
								InnerPadObjects[NrInnerPadObjects++] = Object;

							ThrouHoleInfo |= 8;
						}
					}

					break;
				}
			}
		}

#ifdef _DEBUG

		if ((InRange5(DrillX1, -8.0e5)) && (InRange5(DrillY1, -2.4e5)))
			ok = 1;

#endif

		if ((ThrouHoleInfo & (64)) == 0)
			ThrouHoleLayerInfo = (uint32) - 1;

		if (((ThrouHoleInfo & (64 + 2 + 1)) == (64 + 2 + 1)) && (ThrouHoleLayerInfo == (uint32) - 1))
		{
			// Found all objects for a through hole pin
			memset(&NewObject, 0, sizeof(ObjectRecord));
			NewObject.Info = OBJECT_TO_BE_DELETED;
			Found = 0;

			if ((ObjectBottomLayer->ObjectType == OBJECT_CIRCLE) && (ObjectTopLayer->ObjectType == OBJECT_CIRCLE)
			        && (InRange(ObjectBottomLayer->x2, ObjectTopLayer->x2)))
			{
				Found = 1;
				NewObject.ObjectType = PIN_PUT_THROUGH_ROUND;
			}

			if ((ObjectBottomLayer->ObjectType == OBJECT_RECT) && (ObjectTopLayer->ObjectType == OBJECT_RECT)
			        && (InRange(ObjectTopLayer->x2, ObjectTopLayer->y2))
			        && (InRange(ObjectBottomLayer->x2, ObjectBottomLayer->y2))
			        && (InRange(ObjectTopLayer->x2, ObjectBottomLayer->x2)))
			{
				Found = 1;
				NewObject.ObjectType = PIN_PUT_THROUGH_SQUARE;
			}

			if (Found)
			{
				NewObject.x1 = (float) DrillX1;
				NewObject.y1 = (float) DrillY1;
				NewObject.x2 = ObjectBottomLayer->x2;
				NewObject.y2 = DrillObject->x2;
				NewObject.Clearance = ObjectBottomLayer->Clearance;
				NewObject.Layer = -1;

				if ((ThrouHoleInfo & 8) == 8)
				{	// Inner pad
					if (((ThrouHoleInfo & 4) == 4)	// Power pad
					        && (FirstInnerPadObject->x2 > PowerPadObject->x2))
						NewObject.x3 = PowerPadObject->x2;
					else
						NewObject.x3 = FirstInnerPadObject->x2;

					for (cnt3 = 0; cnt3 < NrInnerPadObjects; cnt3++)
					{
						if (InnerPadObjects[cnt3])
							InnerPadObjects[cnt3]->Info = OBJECT_SAVED;
					}

					if (FirstInnerPadObject)
						FirstInnerPadObject->Info = OBJECT_SAVED;
				}
				else
				{
					GetUnitsValue(Units, DrillX1, str2, 0);
					GetUnitsValue(Units, DrillY1, str3, 0);
					sprintf(str, SC(218, "No inner pad\t\t%s,%s"), str2, str3);

					if (AddToMessageBuf(str) != 0)
						return 0;

					if ((mode & 2) == 0)
						SaveError = 1;
				}

				if ((ThrouHoleInfo & 4) == 4)
				{	// Power pad
					if (((ThrouHoleInfo & 8) == 8)	// Inner pad
					        && (FirstInnerPadObject->x2 > PowerPadObject->x2))
						NewObject.y3 = FirstInnerPadObject->x2;
					else
						NewObject.y3 = PowerPadObject->x2;

					if (PowerPadObject)
						PowerPadObject->Info = OBJECT_SAVED;
				}
				else
				{
					GetUnitsValue(Units, DrillX1, str2, 0);
					GetUnitsValue(Units, DrillY1, str3, 0);
					sprintf(str, SC(221, "No anti power pad\t\t%s,%s"), str2, str3);

					if (AddToMessageBuf(str) != 0)
						return 0;

					if ((mode & 2) == 0)
						SaveError = 1;
				}

				NewObject.PinNr = DrillPinNr;
				DrillObject->Info = OBJECT_SAVED;
				GetUnitsValue(Units, DrillX1, str2, 0);
				GetUnitsValue(Units, DrillY1, str3, 0);

				if ((ThrouHoleInfo & (64 + 32)) == 64)
				{
					sprintf(str, SC(219, "No soldermask pad\tTOP\t%s,%s"), str2, str3);

					if (AddToMessageBuf(str) != 0)
						return -1;

					if ((mode & 2) == 0)
						SaveError = 1;
				}

				if ((ThrouHoleInfo & (64 + 16)) == 64)
				{
					GetUnitsValue(Units, DrillX1, str2, 0);
					GetUnitsValue(Units, DrillY1, str3, 0);
					sprintf(str, SC(220, "No soldermask pad\tBOTTOM\t%s,%s"), str2, str3);

					if (AddToMessageBuf(str) != 0)
						return -1;

					if ((mode & 2) == 0)
						SaveError = 1;
				}

				if (ObjectBottomLayer)
					ObjectBottomLayer->Info = OBJECT_SAVED;

				if (ObjectTopLayer)
					ObjectTopLayer->Info = OBJECT_SAVED;

				if ((mode & 1) == 0)
					AddObject(&NewObject);
			}
		}
		else
		{
			if (ThrouHoleLayerInfo != -1)
			{
				GetUnitsValue(Units, DrillX1, str2, 0);
				GetUnitsValue(Units, DrillY1, str3, 0);

				for (cnt2 = 1; cnt2 < NrPadLayers - 1; cnt2++)
				{
					if ((ThrouHoleLayerInfo & (1 << cnt2)) == 0)
					{
						if (ThrouHoleInfo & 256)
						{	// Different inner pads sizes
							sprintf(str, SC(223, "Different inner padsize\t%d\t%s,%s"), cnt2, str2, str3);

							if (AddToMessageBuf(str) != 0)
								return -1;
						}
						else
						{
							sprintf(str, SC(222, "Missing/wrong inner pad\t%d\t%s,%s"), cnt2, str2, str3);

							if (AddToMessageBuf(str) != 0)
								return -1;

							if ((mode & 2) == 0)
								SaveError = 1;
						}
					}
				}

				if ((ThrouHoleLayerInfo & (1 << (NrPadLayers - 1))) == 0)
				{
					sprintf(str, SC(245, "Missing pad\tTOP\t%s,%s"), str2, str3);

					if (AddToMessageBuf(str) != 0)
						return -1;

					if ((mode & 2) == 0)
						SaveError = 1;
				}

				if ((ThrouHoleLayerInfo & (1 << 0)) == 0)
				{
					sprintf(str, SC(275, "Missing pad\tBOTTOM\t%s,%s"), str2, str3);

					if (AddToMessageBuf(str) != 0)
						return -1;

					if ((mode & 2) == 0)
						SaveError = 1;
				}
			}
		}
	}

	if (MessageBufPos != 0)
	{
		if (Units == 0)
		{
			if (SaveError)
				MessageDialog("MESSAGE\tLAYER\tORIGIN (thou)", 2);
			else
				MessageDialog("MESSAGE\tLAYER\tORIGIN (thou)", 3);
		}
		else
		{
			if (SaveError)
				MessageDialog(SC(224, "MESSAGE\tLAYER\tORIGIN (mm)"), 2);
			else
				MessageDialog(SC(224, "MESSAGE\tLAYER\tORIGIN (mm)"), 3);
		}
	}

	if (SaveError)
		return 0;

// *****************************************************************************************
// *****************************************************************************************

	MessageBufPos = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
		{
			PinNr = Object->PinNr;
#ifdef _DEBUG

			if (PinNr == 0)
				ok = 1;

#endif

			if ((Object->Layer == 0) || (Object->Layer == NrPadLayers - 1) || (Object->Layer == DRILL_LAYER))
			{
				if ((Object->ObjectType == OBJECT_CIRCLE) || (Object->ObjectType == OBJECT_RECT))
				{
					PadX1 = Object->x1;
					PadY1 = Object->y1;
					GetUnitsValue(Units, PadX1, str2, 0);
					GetUnitsValue(Units, PadY1, str3, 0);

					if (PinNr >= 0)
					{
						ObjectSoldBottomLayer = NULL;
						ObjectSoldTopLayer = NULL;
						ObjectPasteBottomLayer = NULL;
						ObjectPasteTopLayer = NULL;

						for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
						{
							if (cnt2 != cnt)
							{
								Object2 = &((*Objects)[cnt2]);

								if (((Object2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
								        && (InRange(PadX1, Object2->x1)) && (InRange(PadY1, Object2->y1)))
								{
									switch (Object2->Layer)
									{
									case SOLD_MASK_BOTTOM_LAYER:
										ObjectSoldBottomLayer = Object2;
										break;

									case SOLD_MASK_TOP_LAYER:
										ObjectSoldTopLayer = Object2;
										break;

									case PASTE_MASK_TOP_LAYER:
										ObjectPasteTopLayer = Object2;
										break;

									case PASTE_MASK_BOTTOM_LAYER:
										ObjectPasteBottomLayer = Object2;
										break;
									}
								}
							}
						}

						if (Object->Layer == 0)
						{
							if (ObjectPasteBottomLayer == NULL)
							{
								for (cnt2 = 0; cnt2 < NrDrills; cnt2++)
								{
									DrillObject = &((*Objects)[DrillObjects[cnt2]]);

									if ((InRange(DrillObject->x1, Object->x1))
									        && (InRange(DrillObject->y1, Object->y1)))
										break;
								}

								if (cnt2 == NrDrills)
								{
									sprintf(str, SC(225, "No pastemask pad\tBOTTOM\t%s,%s"), str2, str3);

									if (AddToMessageBuf(str) != 0)
										return -1;

									Object->Info |= OBJECT_SELECTED | 15;
								}
							}

							if (ObjectSoldBottomLayer == NULL)
							{
								sprintf(str, SC(220, "No soldermask pad\tBOTTOM\t%s,%s"), str2, str3);

								if (AddToMessageBuf(str) != 0)
									return -1;

								Object->Info |= OBJECT_SELECTED | 15;
							}
						}

						if (Object->Layer == NrPadLayers - 1)
						{
							if (ObjectPasteTopLayer == NULL)
							{
								for (cnt2 = 0; cnt2 < NrDrills; cnt2++)
								{
									DrillObject = &((*Objects)[DrillObjects[cnt2]]);

									if ((InRange(DrillObject->x1, Object->x1))
									        && (InRange(DrillObject->y1, Object->y1)))
										break;
								}

								if (cnt2 == NrDrills)
								{
									sprintf(str, SC(226, "No pastemask pad\tTOP\t%s,%s"), str2, str3);

									if (AddToMessageBuf(str) != 0)
										return -1;

									Object->Info |= OBJECT_SELECTED | 15;
								}
							}

							if (ObjectSoldTopLayer == NULL)
							{
								sprintf(str, SC(227, "No soldermask pad\tTOP\t%s,%s"), str2, str3);

								if (AddToMessageBuf(str) != 0)
									return -1;

								Object->Info |= OBJECT_SELECTED | 15;
							}
						}
					}
					else
					{
						sprintf(str, SC(228, "No pinname defined\t\t%s,%s"), str2, str3);

						if (AddToMessageBuf(str) != 0)
							return -1;

						Object->Info |= OBJECT_SELECTED | 15;
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	NrMappableObjectPolygons = 0;
	MappableObjectPolygonStart = NrObjectPolygons;

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectPolygon->Layer;
			res = CheckMappableObjectPolygon(cnt, 0);	// Index polygons, find center point

//      PolygonUsage=1;
			if ((Layer >= 0) && (Layer < 32))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

				if (ObjectPolygon->PinNr == -1)
				{
					ObjectPolygon->Info |= OBJECT_SELECTED | OBJECT_SAVED;
					x1 = ObjectPolygon->Points[0].x;
					y1 = ObjectPolygon->Points[0].y;
					GetUnitsValue(Units, x1, str2, 0);
					GetUnitsValue(Units, y1, str3, 0);
					GetLayerText(Layer, TextStr, 0);
					sprintf(str, SC(229, "No pinname for polygon\t%s\t%s,%s"), TextStr, str2, str3);

					if (AddToMessageBuf(str) != 0)
						return -1;
				}
			}
		}
	}

// *****************************************************************************************
// *****************************************************************************************

	Found = 0;
	First = 1;
	count = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
		{
			x1 = Object->x1;
			y1 = Object->y1;
			GetUnitsValue(Units, x1, str2, 0);
			GetUnitsValue(Units, y1, str3, 0);

			switch (Object->Layer)
			{
			case POWER_PAD_LAYER:
				for (cnt2 = 0; cnt2 < NrDrills; cnt2++)
				{
					DrillObject = &((*Objects)[DrillObjects[cnt2]]);

					if ((InRange(DrillObject->x1, x1)) && (InRange(DrillObject->y1, y1)))
						break;
				}

				if (cnt2 == NrDrills)
				{
					if (NrPadLayers > 2)
					{
						sprintf(str, SC(230, "Unused anti power pad\t\t%s,%s"), str2, str3);
						Object->Info |= OBJECT_SELECTED | 15;
					}
					else
					{
						sprintf(str, SC(231, "Unused anti power pad (will not  be saved)\t\t%s,%s"), str2, str3);
						Object->Info |= OBJECT_SAVED;
					}

					if (AddToMessageBuf(str) != 0)
						return -1;
				}

				break;

			case INNER_PAD_LAYER:
				for (cnt2 = 0; cnt2 < NrDrills; cnt2++)
				{
					DrillObject = &((*Objects)[DrillObjects[cnt2]]);

					if ((InRange(DrillObject->x1, x1)) && (InRange(DrillObject->y1, y1)))
						break;
				}

				if ((cnt2 == NrDrills) && (NrPadLayers > 2))
				{
					sprintf(str, SC(232, "Unused inner pad\t%s\t%s,%s"), "", str2, str3);
					Object->Info |= OBJECT_SELECTED | 15;

					if (AddToMessageBuf(str) != 0)
						return -1;
				}

				break;
			}

			if ((NrPadLayers > 2) && (Object->Layer > 0) && (Object->Layer < NrPadLayers - 1)
			        && (Object->ObjectType == OBJECT_CIRCLE))
			{
				for (cnt2 = 0; cnt2 < NrDrills; cnt2++)
				{
					DrillObject = &((*Objects)[DrillObjects[cnt2]]);

					if ((InRange(DrillObject->x1, x1)) && (InRange(DrillObject->y1, y1)))
						break;
				}

				if (cnt2 == NrDrills)
				{
					GetLayerText(Object->Layer, TextStr, 0);
					sprintf(str, SC(232, "Unused inner pad\t%s\t%s,%s"), TextStr, str2, str3);

					if (AddToMessageBuf(str) != 0)
						return -1;
				}
			}
		}
	}

	if (NrSilkTopObjectsSelected + NrSilkBottomObjectsSelected == 0)
	{
		if (AddToMessageBuf(SC(233, "No silkscreen outlines")) != 0)
			return -1;
	}

	if (GeomNameObjectCnt != -1)
	{
		Object = &((*Objects)[GeomNameObjectCnt]);

		if (strcmp(Object->Text, "noname") == 0)
		{
			if (AddToMessageBuf(SC(234, "Default geometry name")) != 0)
				return -1;
		}
	}

	if (MessageBufPos != 0)
	{
		if (Units == 0)
			MessageDialog("MESSAGE\tLAYER\tORIGIN (thou)", 3);
		else
			MessageDialog(SC(224, "MESSAGE\tLAYER\tORIGIN (mm)"), 3);

		MessageBufPos = 0;
		InvalidateRect(GEOMWindow, NULL, 0);
		PostMessage(GEOMWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
	}
	else
	{
		if (((mode & 1) == 1) && (!Warning))
			MessageBox(GEOMWindow, SC(235, "No Error(s)/warning(s) found"), SC(4, "Message"), MB_OK);
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteToBuf(uint8 * MemToAdd, int32 BufPos, int32 MemSize)
{
	if (BufPos + MemSize > MaxShapesMem)
	{
		if (AllocateMemShapes(max(BufPos + MemSize + 16384, MaxShapesMem * 2)) != 0)
			return -1;
	}

	memmove(&ShapesMem[BufPos], MemToAdd, MemSize);
	return BufPos + MemSize;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CollectObjects()
{
	int32 ok, cnt, StartCnt, cnt2, cnt3, Layer2, NrPins, PolygonUsage, PolygonPos, LineSegments, SegmentCount,
	      ObjectType, Layer, NrPinShapesSaved, NrPinShapesBufPos, ObjectSize, NewBufPos, ObjectInfo, Found, LayerOk,
	      *ShapeInfoP, PinObjectMode[MaxObjectsPerPin], MemSizeObjectPolygons[256];
	double x1, y1, x2, y2;
	ObjectRecord *Object, LineObject, *PowerPadObject, *InnerPadObject;
	uint8 *PinObjects[MaxObjectsPerPin];
	PinInfoRecord *PinInfo;
	ObjectPolygonRecord *ObjectPolygon;
	double LineBuf[128];
	ShapeRecord OldShape;

	BufPos = sizeof(ShapeRecord);
	memcpy(&OldShape, &Shape, sizeof(ShapeRecord));
	MaxShapesMem = 0;

	Object = NULL;
	ObjectPolygon = NULL;
	ObjectType = 0;
	SMDdevice = 1;
	PolygonUsage = 0;
	ShapeInfoP = (int32 *) & ShapeLines;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if (((ObjectInfo & (OBJECT_NOT_VISIBLE)) == 0))
		{
			if ((Object->Layer >= 0) && (Object->Layer < NrPadLayers))
			{
				if (Object->Layer != NrPadLayers - 1)
					SMDdevice = 0;
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	Shape.NrPlacementOutLines = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = Object->Layer;

			if (Layer == PLACEMENT_OUTLINE_LAYER)
			{
				ObjectType = Object->ObjectType;
				*ShapeInfoP = (int32) ObjectType;
				ShapeLines[1] = (float) Object->x1;
				ShapeLines[2] = (float) Object->y1;
				ShapeLines[5] = 0.0;

				switch (ObjectType)
				{
				case OBJECT_LINE:
					if (Object->Info2 != 0)
					{
						x1 = Object->x1;
						y1 = Object->y1;
						x2 = Object->x2;
						y2 = Object->y2;
						LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, Object->Info2);
						SegmentCount = 0;

						for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
						{
							x1 = LineBuf[SegmentCount++];
							y1 = LineBuf[SegmentCount++];
							x2 = LineBuf[SegmentCount++];
							y2 = LineBuf[SegmentCount++];
							ShapeLines[1] = (float) x1;
							ShapeLines[2] = (float) y1;
							ShapeLines[3] = (float) x2;
							ShapeLines[4] = (float) y2;
							ObjectSize = 24;

							if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrPlacementOutLines++;
						}

						Object->Info |= OBJECT_SAVED;
					}
					else
					{
						ShapeLines[3] = (float) Object->x2;
						ShapeLines[4] = (float) Object->y2;
						ObjectSize = 24;

						if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrPlacementOutLines++;
						Object->Info |= OBJECT_SAVED;
					}

					break;

				case OBJECT_RECT:
					ShapeLines[3] = (float) Object->x2;
					ShapeLines[4] = (float) Object->y2;
					ObjectSize = 24;

					if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrPlacementOutLines++;
					Object->Info |= OBJECT_SAVED;
					break;

				case OBJECT_CIRCLE:
					if (Object->Info2 != 0)
					{
						ShapeLines[3] = (float) Object->x2;
						ShapeLines[4] = (float) Object->Info2;
						ObjectSize = 24;

						if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrPlacementOutLines++;
						Object->Info |= OBJECT_SAVED;
					}

					break;

				case OBJECT_ARC:
					if (Object->y2 == 0)
						Object->y2 = Object->x2;

					if (Object->x2 == 0)
						Object->x2 = Object->y2;

					ShapeLines[3] = (float) Object->x2;
					ShapeLines[4] = (float) Object->y2;
					ShapeLines[5] = (float) Object->x3;
					ShapeLines[6] = (float) Object->y3;
					ShapeLines[7] = (float) Object->x4;
					ShapeLines[8] = (float) Object->y4;
					ShapeLines[9] = 0.0;
					ObjectSize = 40;

					if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrPlacementOutLines++;
					Object->Info |= OBJECT_SAVED;
					break;
					/*
					          case OBJECT_TEXT:
					            ShapeLines[3]=Object->x2;
					            ShapeLines[4]=Object->Info2;
					            ShapeLines[5]=0.0;
					            memset(&ShapeLines[6],0,64);
					            memmove(&ShapeLines[6],Object->Text,min(strlen(Object->Text)-1,63));
					            ObjectSize=88;
					            if ((NewBufPos=WriteToBuf((uint8 *)&ShapeLines,BufPos,ObjectSize))==-1) return -1;
					            BufPos=NewBufPos;
					            Shape.NrPlacementOutLines++;
					            break;
					*/
				}

			}
		}
	}

	Shape.CompOutLineOffset = BufPos;

// **********************************************************************************
// **********************************************************************************

	Shape.NrCompOutLines = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = Object->Layer;

			if (Layer == COMP_OUTLINE_LAYER)
			{
				ObjectType = Object->ObjectType;
				*ShapeInfoP = (int32) ObjectType;
				ShapeLines[1] = (float) Object->x1;
				ShapeLines[2] = (float) Object->y1;
				ShapeLines[5] = (float) Object->Thickness;

				switch (ObjectType)
				{
				case OBJECT_LINE:
					if (Object->Info2 != 0)
					{
						x1 = Object->x1;
						y1 = Object->y1;
						x2 = Object->x2;
						y2 = Object->y2;
						LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, Object->Info2);
						SegmentCount = 0;

						for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
						{
							x1 = LineBuf[SegmentCount++];
							y1 = LineBuf[SegmentCount++];
							x2 = LineBuf[SegmentCount++];
							y2 = LineBuf[SegmentCount++];
							ShapeLines[1] = (float) x1;
							ShapeLines[2] = (float) y1;
							ShapeLines[3] = (float) x2;
							ShapeLines[4] = (float) y2;
							ObjectSize = 24;

							if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrCompOutLines++;
						}

						Object->Info |= OBJECT_SAVED;
					}
					else
					{
						ShapeLines[3] = (float) Object->x2;
						ShapeLines[4] = (float) Object->y2;
						ObjectSize = 24;

						if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrCompOutLines++;
						Object->Info |= OBJECT_SAVED;
					}

					break;

				case OBJECT_RECT:
					ShapeLines[3] = (float) Object->x2;
					ShapeLines[4] = (float) Object->y2;
					ObjectSize = 24;

					if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrCompOutLines++;
					Object->Info |= OBJECT_SAVED;
					break;

				case OBJECT_CIRCLE:
					ShapeLines[3] = (float) Object->x2;
					ShapeLines[4] = (float) Object->Info2;
					ObjectSize = 24;

					if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Object->Info |= OBJECT_SAVED;
					Shape.NrCompOutLines++;
					break;

				case OBJECT_ARC:
					ShapeLines[3] = (float) Object->x2;
					ShapeLines[4] = (float) Object->y2;
					ShapeLines[5] = (float) Object->x3;
					ShapeLines[6] = (float) Object->y3;
					ShapeLines[7] = (float) Object->x4;
					ShapeLines[8] = (float) Object->y4;
					ShapeLines[9] = (float) Object->Thickness;
					ObjectSize = 40;

					if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrCompOutLines++;
					Object->Info |= OBJECT_SAVED;
					break;

				case OBJECT_TEXT:
					ShapeLines[3] = (float) Object->x2;
					/*
					            Rotation=(int32)(Object->RotationAngle/45.0+0.1);
					            if (Rotation<0) Rotation+=8;
					            Rotation=(Rotation >> 1) + ((Rotation & 1) << 2);
					            ShapeLines[4]=(float)Rotation;
					*/
					ShapeLines[4] = (float) (Object->RotationAngle + 2000.0);
					memset(&ShapeLines[6], 0, 64);
					memmove(&ShapeLines[6], Object->Text, min(strlen(Object->Text), 63));
					ObjectSize = 88;

					if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrCompOutLines++;
					Object->Info |= OBJECT_SAVED;
					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons - NrMappableObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectPolygon->Layer;

			if (Layer == COMP_OUTLINE_LAYER)
			{
				*ShapeInfoP = OBJECT_POLYGON;
				ShapeLines[1] = (float) ObjectPolygon->OffsetX;
				ShapeLines[2] = (float) ObjectPolygon->OffsetY;
				ShapeLines[3] = 0.0;
				ObjectSize = 16;
#ifdef _DEBUG

				if (BufPos == 231504)
					ok = 1;

				if (cnt == 174)
					ok = 1;

#endif
				ObjectPolygon->ShapePolygonPosition = BufPos + 12;

				if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrCompOutLines++;
				ObjectPolygon->Info |= OBJECT_SAVED;
			}
		}
	}

	Shape.PinOffset = BufPos;

// **********************************************************************************
// **********************************************************************************

	Found = 1;
	StartCnt = 0;

	// Check copper objects
	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);
		Object->Test = 0;
#ifdef _DEBUG

		if (cnt2 == 4)
			ok = 1;

#endif
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
		{
			ObjectType = Object->ObjectType;
			Layer = Object->Layer;

			switch (ObjectType)
			{
			case OBJECT_RECT:
			case OBJECT_CIRCLE:
			case OBJECT_LINE:
			case OBJECT_ARC:
				if (((Layer >= 0) && (Layer < 32)) || (Layer == POWER_PAD_LAYER) || (Layer == INNER_PAD_LAYER)
				        || (Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
					Object->Test = 1;

				break;

			case PIN_PUT_THROUGH_ROUND:	// In CheckPads function added objects
			case PIN_PUT_THROUGH_SQUARE:
				Object->Test = 1;
				break;

			default:
				ok = 1;
				break;
				/*
				        case PIN_LINE_VER:
				        case PIN_LINE_HOR:
				        case PIN_LINE_DIAG1:
				        case PIN_LINE_DIAG2:
				          Object->Test=1;
				          break;
				*/
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons - NrMappableObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
		{
			Layer = ObjectPolygon->Layer;
			ObjectPolygon->BlockNr = 0;

			if ((Layer >= 0) && (Layer < 32))
				ObjectPolygon->BlockNr = 1;
		}
	}

	NrPins = 0;
	cnt = 0;

	while ((cnt < NrPinObjects) && (NrPins < MaxNrPins))
	{
		PinInfo = &((*PinInfos)[cnt]);
		memset(&ShapePad, 0, sizeof(ShapePadRecord));
		PowerPadObject = NULL;
		InnerPadObject = NULL;
		memmove(&ShapePad.Name, PinInfo->PinText, sizeof(ShapePad.Name) - 1);
#ifdef _DEBUG

		if (stricmp(ShapePad.Name, "5") == 0)
			ok = 1;

#endif

		for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
		{
			Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

			if (cnt2 == 4)
				ok = 1;

			if ((InRange5(Object->x1, -8.0e5)) && (InRange5(Object->y1, -2.4e5)))
			{
				if (stricmp(ShapePad.Name, "5") == 0)
					ok = 1;

				ok = 1;
			}

#endif
			ObjectInfo = Object->Info;

			if (((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0) && (Object->Test == 1)	// Copper object
			        && (Object->PinNr == cnt) && (ShapePad.NrPinShapes < MaxObjectsPerPin))
			{
				if ((Object->ObjectType == OBJECT_CIRCLE) && (Object->Layer == POWER_PAD_LAYER))
				{
					if (!PowerPadObject)
						PowerPadObject = Object;

					Object->Info |= OBJECT_SAVED;
				}
				else
				{
					if ((Object->ObjectType == OBJECT_CIRCLE) && (Object->Layer == INNER_PAD_LAYER))
					{
						if (!InnerPadObject)
							InnerPadObject = Object;

						Object->Info |= OBJECT_SAVED;
					}
					else
					{
						PinObjects[ShapePad.NrPinShapes] = (uint8 *) Object;
						PinObjectMode[ShapePad.NrPinShapes] = 0;	// Non polygon mode
						ShapePad.NrPinShapes++;
					}
				}
			}
		}

		for (cnt2 = 0; cnt2 < NrObjectPolygons - NrMappableObjectPolygons; cnt2++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt2]]);

			if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0) && (ObjectPolygon->BlockNr == 1)	// Copper object
			        && (ObjectPolygon->PinNr == cnt) && (ShapePad.NrPinShapes < MaxObjectsPerPin))
			{
				PinObjects[ShapePad.NrPinShapes] = (uint8 *) ObjectPolygon;
				PinObjectMode[ShapePad.NrPinShapes] = 32;	// Polygon mode
				ShapePad.NrPinShapes++;
			}
		}

// **********************************************************************************
// **********************************************************************************
		if (ShapePad.NrPinShapes > 0)
		{
			NrPins++;
			NrPinShapesSaved = 0;
			ObjectSize = sizeof(ShapePadRecord);
			NrPinShapesBufPos = BufPos + 10;

			if ((NewBufPos = WriteToBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
				return -1;

			BufPos = NewBufPos;

			// First save the put through pins
			for (cnt2 = 0; cnt2 < ShapePad.NrPinShapes; cnt2++)
			{
				Object = (ObjectRecord *) PinObjects[cnt2];
				ObjectType = Object->ObjectType;

				if ((PinObjectMode[cnt2] & 32) == 0)
				{	// Non polygons
					switch (ObjectType)
					{
					case PIN_PUT_THROUGH_SQUARE:
					case PIN_PUT_THROUGH_ROUND:
						memset(&Pad, 0, sizeof(PadRecord));
						Pad.ShapeType = ObjectType;
						Pad.X = (float) Object->x1;
						Pad.Y = (float) Object->y1;
						Pad.Width = (float) Object->x2;
						Pad.Height = (float) Object->y2;
						Pad.Clearance = (float) Object->Clearance;
						Pad.Special.Extra1 = (float) Object->x3;
						Pad.Extra2 = (float) Object->y3;
						Pad.Layer = -1;
						Object->Info |= OBJECT_SAVED;
						ObjectSize = sizeof(PadRecord);

						if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						NrPinShapesSaved++;
						PinObjectMode[cnt2] |= 31;
						break;

					}
				}
			}

// **********************************************************************************
			for (cnt3 = 0; cnt3 < NrPadLayers; cnt3++)
			{
				for (cnt2 = 0; cnt2 < ShapePad.NrPinShapes; cnt2++)
				{
#ifdef _DEBUG

					if (stricmp(ShapePad.Name, "bottom2") == 0)
						ok = 1;

#endif

					if ((PinObjectMode[cnt2] & 32) == 0)
					{	// Non polygon object
						Object = (ObjectRecord *) PinObjects[cnt2];
						ObjectType = Object->ObjectType;
						Layer2 = Object->Layer;

						if ((ObjectType == OBJECT_CIRCLE)
						        && ((Layer2 == DRILL_LAYER) || (Layer2 == DRILL_UNPLATED_LAYER)))
						{
							// Objects on drill or drill unplated layer will be saved later
							continue;
						}

						if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == OBJECT_SAVED))
							continue;
					}
					else
					{	// Polygon object
						ObjectPolygon = (ObjectPolygonRecord *) PinObjects[cnt2];

						if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == OBJECT_SAVED))
							continue;

						Layer2 = ObjectPolygon->Layer;
					}

					if ((Layer2 < 32) && (Layer2 >= NrPadLayers))
						Layer2 = NrPadLayers - 1;

					if (cnt3 != Layer2)
						continue;

					if ((PinObjectMode[cnt2] & 32) == 0)
					{	// Non polygons
						memset(&Pad, 0, sizeof(PadRecord));
						Pad.ShapeType = ObjectType;
						Pad.Layer = Layer2;
						Pad.X = (float) Object->x1;
						Pad.Y = (float) Object->y1;
						Pad.Width = (float) Object->x2;
						Pad.Height = (float) Object->y2;
						Pad.Clearance = (float) Object->Clearance;

						switch (ObjectType)
						{
						case OBJECT_CIRCLE:
							Pad.Height = (float) 0.0;
							Pad.ShapeType = PIN_SMD_ROUND;
							break;

						case OBJECT_ARC:
							break;

						case OBJECT_RECT:
							Pad.ShapeType = PIN_SMD_RECT;
							break;

						case OBJECT_LINE:
							if (GetDirection(Object->x1, Object->y1, Object->x2, Object->y2, &LineObject) == 0)
							{
								// Vertical/horizontal/diagonal line
								Pad.X = (float) LineObject.x1;
								Pad.Y = (float) LineObject.y1;
								Pad.Width = (float) LineObject.x2;
								Pad.Height = (float) Object->Thickness;
								Pad.ShapeType = LineObject.ObjectType;
							}
							else
							{
								// All angle line
								Pad.ShapeType = PIN_LINE_ALL_ANGLE;
								Pad.Special.Thickness = (float) Object->Thickness;
							}

							break;

						default:
							ok = 1;
							break;
						}

						if (ObjectType != OBJECT_ARC)
						{
							ObjectSize = sizeof(PadRecord);

							if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
						}
						else
						{
							Pad.ShapeType = PIN_ARC;
							Pad.Layer = Object->Layer;
							ShapeLines[2] = (float) Object->x1;
							ShapeLines[3] = (float) Object->y1;
							ShapeLines[4] = (float) Object->x2;
							ShapeLines[5] = (float) Object->y2;
							ShapeLines[6] = (float) Object->x3;
							ShapeLines[7] = (float) Object->y3;
							ShapeLines[8] = (float) Object->x4;
							ShapeLines[9] = (float) Object->y4;
							ShapeLines[10] = (float) Object->Thickness;
							ShapeLines[11] = (float) Object->Clearance;
							ObjectSize = 8;

							if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							ObjectSize = 40;

							if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines[2], BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
						}
					}
					else
					{	// Polygons
						ObjectPolygon = (ObjectPolygonRecord *) PinObjects[cnt2];
						memset(&Pad, 0, sizeof(PadRecord));
						Pad.ShapeType = PIN_SMD_POLYGON;
						Pad.Layer = Layer2;
						Pad.X = (float) ObjectPolygon->OffsetX;
						Pad.Y = (float) ObjectPolygon->OffsetY;
						Pad.Clearance = ObjectPolygon->Clearance;
						Pad.Special.AddressOffset = 0;
						// Fill in at which address the polygon pointer must be stored
#ifdef _DEBUG

						if (BufPos == 231504)
							ok = 1;

						if (cnt == 174)
							ok = 1;

#endif
						ObjectPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special);
						ObjectSize = sizeof(PadRecord);

						if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
					}

					if ((PinObjectMode[cnt2] & 32) == 0)
					{	// Non polygon object
						Object->Info |= OBJECT_SAVED;
					}
					else
					{	// Polygon object
						ObjectPolygon->Info |= OBJECT_SAVED;
					}

					NrPinShapesSaved++;
				}
			}

			// **********************************************************************************
			// Now save the remaining drill objects
			cnt3 = 0;

			for (cnt2 = 0; cnt2 < ShapePad.NrPinShapes; cnt2++)
			{
				if ((PinObjectMode[cnt2] & 32) == 0)
				{
					Object = (ObjectRecord *) PinObjects[cnt2];
					ObjectType = Object->ObjectType;

					if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0))
					{
						switch (Object->Layer)
						{
						case DRILL_LAYER:
						case DRILL_UNPLATED_LAYER:
							memset(&Pad, 0, sizeof(PadRecord));

							if (Object->Layer == DRILL_LAYER)
							{
								Pad.ShapeType = DRILL;

								if (PowerPadObject)
									Pad.Extra2 = (float) PowerPadObject->x2;

								if (InnerPadObject)
									Pad.Special.Extra1 = (float) InnerPadObject->x2;
							}
							else
								Pad.ShapeType = DRILL_UNPLATED;

							Pad.X = (float) Object->x1;
							Pad.Y = (float) Object->y1;
							Pad.Width = (float) Object->x2;
							Pad.Layer = -1;
							Pad.Clearance = (float) Object->Clearance;
							Pad.Height = 0.0;
							Object->Info |= OBJECT_SAVED;
							ObjectSize = sizeof(PadRecord);

							if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							NrPinShapesSaved++;
							break;

						default:
							cnt3++;
							ok = 1;
							break;
						}
					}
					else
						ok = 1;
				}
			}

			if (NrPinShapesSaved != ShapePad.NrPinShapes)
			{
				ShapePad.NrPinShapes = (int16) (ShapePad.NrPinShapes - cnt3);
				memcpy(&ShapesMem[NrPinShapesBufPos], &ShapePad.NrPinShapes, 4);
				ok = 1;
#ifdef _DEBUG
				MessageBoxUTF8(GEOMWindow, "Severe error", SC(48, "Error"), MB_APPLMODAL | MB_OK);
#endif
			}
		}

		cnt++;
	}

// **********************************************************************************

	memset(&ShapePad, 0, sizeof(ShapePadRecord));

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
		{
			if ((Object->Layer == DRILL_UNPLATED_LAYER) || (Object->Layer == POWER_PAD_LAYER))
				ShapePad.NrPinShapes++;
		}
	}

	if (ShapePad.NrPinShapes > 0)
	{
		NrPins++;
		strcpy(ShapePad.Name, "Unused");
		ObjectSize = sizeof(ShapePadRecord);

		if ((NewBufPos = WriteToBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);
			ObjectInfo = Object->Info;

			if ((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
			{
				if ((Object->Layer == DRILL_UNPLATED_LAYER) || (Object->Layer == POWER_PAD_LAYER))
				{
					memset(&Pad, 0, sizeof(PadRecord));
					Pad.ShapeType = DRILL_UNPLATED;
					Pad.Clearance = (float) Object->Clearance;

					if (Object->Layer == POWER_PAD_LAYER)
					{
						Pad.ShapeType = PIN_PUT_THROUGH_ROUND_POWER;
						Pad.Clearance = 0.0;
					}

					Pad.X = (float) Object->x1;
					Pad.Y = (float) Object->y1;
					Pad.Width = (float) Object->x2;
					Pad.Layer = -1;
					//      Pad.Layer=Object->Layer;
					ObjectSize = sizeof(PadRecord);

					if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Object->Info |= OBJECT_SAVED;
				}
			}
		}
	}

	Shape.NrPins = NrPins;

	Shape.SilkScreenOffset = BufPos;

// **********************************************************************************
// **********************************************************************************

	Shape.NrSilkScreenOutLines = 0;
	Shape.OtherObjectsOffset = BufPos;
	Shape.NrOtherObjects = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if (((ObjectInfo & (OBJECT_NOT_VISIBLE)) == 0))
		{
			ObjectType = Object->ObjectType;
			Layer = Object->Layer;
			LayerOk = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
			case SILKSCREEN_TOP_LAYER:
			case SILKSCREEN_BOTTOM_LAYER:
			case BOARD_OUTLINE_LAYER:
				LayerOk = 1;
				break;

			default:
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				switch (ObjectType)
				{
				case OBJECT_LINE:
					if (((Layer == INFO_LAYER) || (Layer == INFO_LAYER2) || (Layer == INFO_LAYER3)
					        || (Layer == INFO_LAYER4) || (Layer == SILKSCREEN_TOP_LAYER)
					        || (Layer == SILKSCREEN_BOTTOM_LAYER)) && (Object->Info2 != 0))
					{
						x1 = Object->x1;
						y1 = Object->y1;
						x2 = Object->x2;
						y2 = Object->y2;
						LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, Object->Info2);
						SegmentCount = 0;

						for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
						{
							x1 = LineBuf[SegmentCount++];
							y1 = LineBuf[SegmentCount++];
							x2 = LineBuf[SegmentCount++];
							y2 = LineBuf[SegmentCount++];
							Pad.X = (float) x1;
							Pad.Y = (float) y1;
							Pad.Width = (float) x2;
							Pad.Height = (float) y2;
							Pad.Layer = Object->Layer;
							Pad.ShapeType = ObjectType;
							Pad.Special.Thickness = (float) Object->Thickness;
							ObjectSize = sizeof(PadRecord);

							if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrOtherObjects++;
						}

						Object->Info |= OBJECT_SAVED;
					}
					else
					{
						memset(&Pad, 0, sizeof(PadRecord));
						Pad.X = (float) Object->x1;
						Pad.Y = (float) Object->y1;
						Pad.Width = (float) Object->x2;
						Pad.Height = (float) Object->y2;
						Pad.Layer = Object->Layer;
						Pad.ShapeType = ObjectType;
						Pad.Special.Thickness = (float) Object->Thickness;
						Object->Info |= OBJECT_SAVED;
						ObjectSize = sizeof(PadRecord);

						if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
					}

					break;

				case OBJECT_RECT:
					memset(&Pad, 0, sizeof(PadRecord));
					Pad.ShapeType = ObjectType;
					Pad.X = (float) Object->x1;
					Pad.Y = (float) Object->y1;
					Pad.Width = (float) Object->x2;
					Pad.Height = (float) Object->y2;
					Pad.Layer = Object->Layer;
					Pad.Special.Thickness = (float) Object->Thickness;
					Object->Info |= OBJECT_SAVED;
					ObjectSize = sizeof(PadRecord);

					if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrOtherObjects++;
					break;

				case OBJECT_CIRCLE:
#ifdef _DEBUG
					if ((InRange5(Object->x1, 108.0e5)) && (InRange5(Object->y1, 336.1e5)))
						ok = 1;

#endif
					memset(&Pad, 0, sizeof(PadRecord));
					Pad.ShapeType = ObjectType;
					Pad.X = (float) Object->x1;
					Pad.Y = (float) Object->y1;
					Pad.Width = (float) Object->x2;
					Pad.Height = (float) Object->Info2;
					Pad.Layer = Object->Layer;
					Pad.Special.Thickness = (float) Object->Thickness;
					Object->Info |= OBJECT_SAVED;
					ObjectSize = sizeof(PadRecord);

					if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrOtherObjects++;
					break;

				case OBJECT_ARC:
#ifdef _DEBUG
					if ((InRange5(Object->x1, 108.0e5)) && (InRange5(Object->y1, 336.1e5)))
						ok = 1;

#endif
					Pad.ShapeType = ObjectType;
					Pad.Layer = Object->Layer;
					Pad.X = (float) Object->x1;
					Pad.Y = (float) Object->y1;
					Pad.Width = (float) Object->x2;
					Pad.Height = (float) Object->y2;
					ShapeLines[2] = (float) Object->x1;
					ShapeLines[3] = (float) Object->y1;
					ShapeLines[4] = (float) Object->x2;
					ShapeLines[5] = (float) Object->y2;
					ShapeLines[6] = (float) Object->x3;
					ShapeLines[7] = (float) Object->y3;
					ShapeLines[8] = (float) Object->x4;
					ShapeLines[9] = (float) Object->y4;
					ShapeLines[10] = (float) Object->Thickness;
					ShapeLines[11] = 0.0;
					Object->Info |= OBJECT_SAVED;

					switch (Object->Layer)
					{
					case BOARD_OUTLINE_LAYER:
					case INFO_LAYER:
					case INFO_LAYER2:
					case INFO_LAYER3:
					case INFO_LAYER4:
					case SOLD_MASK_BOTTOM_LAYER:
					case SOLD_MASK_TOP_LAYER:
					case PASTE_MASK_BOTTOM_LAYER:
					case PASTE_MASK_TOP_LAYER:
					case SILKSCREEN_TOP_LAYER:
					case SILKSCREEN_BOTTOM_LAYER:
						ObjectSize = 8;

						if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						ObjectSize = 40;

						if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines[2], BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
						break;
					}

					break;

				case OBJECT_TEXT:
					switch (Layer)
					{
					case INFO_LAYER:
					case INFO_LAYER2:
					case INFO_LAYER3:
					case INFO_LAYER4:
					case SOLD_MASK_TOP_LAYER:
					case SOLD_MASK_BOTTOM_LAYER:
					case SILKSCREEN_TOP_LAYER:
					case SILKSCREEN_BOTTOM_LAYER:
#ifdef _DEBUG
						ok = 1;
#endif
						Pad.ShapeType = ObjectType;
						Pad.X = (float) Object->x1;
						Pad.Y = (float) Object->y1;
						Pad.Width = (float) Object->x2;
						Pad.Height = (float) Object->y2;
						Pad.Layer = Object->Layer;
						Pad.Height = (float) (Object->RotationAngle + 2000.0);
						Pad.Special.Thickness = (float) Object->Thickness;
						memset(&ShapeLines[7], 0, 64);
						memmove(&ShapeLines[7], Object->Text, min(strlen(Object->Text), 63));
						ObjectSize = 28;

						if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						ObjectSize = 64;

						if ((NewBufPos = WriteToBuf((uint8 *) & ShapeLines[7], BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
						Object->Info |= OBJECT_SAVED;
						break;
					}

					break;
				}
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons - NrMappableObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectPolygon->Layer;
			LayerOk = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
			case SILKSCREEN_TOP_LAYER:
			case SILKSCREEN_BOTTOM_LAYER:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
				LayerOk = 1;
				break;

			default:
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				memset(&Pad, 0, sizeof(PadRecord));
				Pad.ShapeType = OBJECT_POLYGON;
				Pad.X = (float) ObjectPolygon->OffsetX;
				Pad.Y = (float) ObjectPolygon->OffsetY;
				Pad.Layer = Layer;
				ObjectPolygon->Info |= OBJECT_SAVED;
				ObjectSize = sizeof(PadRecord);
				Pad.Special.AddressOffset = 0;
#ifdef _DEBUG

				if (BufPos == 231504)
					ok = 1;

				if (cnt == 174)
					ok = 1;

#endif
				ObjectPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special);

				if ((NewBufPos = WriteToBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}
		}
	}

#ifdef _DEBUG

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if (((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0))
			ok = 1;
	}

//  ObjectPolygon=(ObjectPolygonRecord *)&(ObjectPolygonMem[(*ObjectPolygons)[174]]);
	ok = 1;

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0))
			ok = 1;
	}

#endif

// **********************************************************************************
// **********************************************************************************
	Shape.PolygonOffset = BufPos;
	// Write the polygon shapes
	cnt2 = 0;
	MemSizeObjectPolygons[cnt2++] = 0;

	for (cnt = MappableObjectPolygonStart; cnt < MappableObjectPolygonStart + NrMappableObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		ObjectSize = MemSizeObjectPolygon(ObjectPolygon);
		MemSizeObjectPolygons[cnt2] = MemSizeObjectPolygons[cnt2 - 1] + ObjectSize;
		cnt2++;

		if ((NewBufPos = WriteToBuf((uint8 *) ObjectPolygon, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
	}

	Shape.NrPolygons = NrMappableObjectPolygons;

	// Fill in the address pointer to the polygon objects
	for (cnt = 0; cnt < NrObjectPolygons - NrMappableObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			cnt2 = ObjectPolygon->ShapePolygonPosition;
			PolygonPos = Shape.PolygonOffset + MemSizeObjectPolygons[ObjectPolygon->PolygonNr];
#ifdef _DEBUG

			if (cnt2 == 231516)
				ok = 1;

			if (cnt2 == 100000000)
				ok = 1;

#endif
//      CheckObjectPolygon=(ObjectPolygonRecord *)&(ObjectPolygonMem[(*ObjectPolygons)[cnt3]]);
			memmove(&ShapesMem[cnt2], &PolygonPos, 4);
		}
	}

// **********************************************************************************
// **********************************************************************************

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0)
		{
			if ((Object->Layer == GEOM_NAME_LAYER) && (Object->ObjectType == OBJECT_TEXT))
			{
				Object->Info |= OBJECT_SAVED;
				Shape.ShapeNameOriginX = (float) Object->x1;
				Shape.ShapeNameOriginY = (float) Object->y1;
				Shape.ShapeNameHeight = (float) Object->x2;
				Shape.ShapeNameRotation = (int32) (Object->RotationAngle + 0.1);
			}
		}
	}

// **********************************************************************************
// **********************************************************************************

	Shape.NrLayers = NrPadLayers;

#ifdef _DEBUG

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if (((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0))
			ok = 1;
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SAVED)) == 0))
			ok = 1;
	}

#endif
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckFile(LPSTR FileName, int32 mode)
{
	int32 Designfp, result, Pos, SizeFile, Libfp, cnt, NrLibEntries;
	int32 Found = 0;
	char str[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING];
	ShapeRecord Shape;
	LibRecord Lib;
	LibNameRecord LibName;

	Pos = 0;

	if ((mode & 2) == 0)
		strcpy(FileName2, FileName);
	else
		strcpy(FileName2, EditFile);

	if (LibraryFile[0] == 0)
	{
		if (FileExistsUTF8(FileName2) == 0)
		{
			if ((Designfp = FileOpenReadOnlyUTF8(FileName2)) == -1)
			{
				sprintf(str, SC(236, "Error in opening file %s"), FileName2);
				MessageBoxUTF8(GEOMWindow, str, "", MB_OK);

				if ((mode & 2) == 2)
					EditFile[0] = 0;

				return -1;
			}

			if (FileRead(Designfp, &Shape, sizeof(ShapeRecord), &result) == -1)
			{
				sprintf(str, SC(237, "Error in reading file %s"), FileName2);
				MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);
				FileClose(Designfp);

				if ((mode & 2) == 2)
					EditFile[0] = 0;

				return -1;
			}

			FileClose(Designfp);

			if ((stricmp(Shape.Identification, ShapeCode) != 0) && (stricmp(Shape.Identification, ShapeCode2) != 0)
			        && (stricmp(Shape.Identification, ShapeCode3) != 0))
			{
				sprintf(str, SC(238, "File %s is not a valid geometry file"), FileName2);
				MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);

				if ((mode & 2) == 2)
					EditFile[0] = 0;

				return -1;
			}
		}
		else
			return -2;

		strcpy(EditFile, FileName2);
	}
	else
	{
		GetFilePartFromFileName(str, EditFile);
		strcpy(FileName2, str);

		if ((Libfp = FileOpenReadOnlyUTF8(LibraryFile)) == -1)
		{
			sprintf(str, SC(239, "Error in opening library file %s"), LibraryFile);
			MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
		{
			sprintf(str, SC(240, "Error in reading library file %s"), LibraryFile);
			MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);
			FileClose(Libfp);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		if (strcmp(Lib.Identification, LibraryCode1) == 0)
		{
			NrLibEntries = Lib.NrLibEntries;
			cnt = 0;

			while ((!Found) && (cnt < NrLibEntries))
			{
				if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
					return 0;

				if (stricmpUTF8(LibName.Text, FileName2) == 0)
				{
					Found = 1;
					Pos = LibName.Pos;
					SizeFile = LibName.Length;
				}

				cnt++;
			}
		}
		else
		{
			sprintf(str, SC(241, "File %s is not a valid library file"), LibraryFile);
			MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		if (!Found)
		{
			FileClose(Libfp);
			sprintf(str, SC(242, "Geometry %s not found in library file %s"), FileName2, LibraryFile);
			MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		if (FileSeek(Libfp, Pos) == -1)
		{
			sprintf(str, SC(240, "Error in reading library file %s"), LibraryFile);
			MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);
			FileClose(Libfp);

			if ((mode & 2) == 2)
				EditFile[0] = 0;
		}

		if (FileRead(Libfp, &Shape, sizeof(ShapeRecord), &result) == -1)
		{
			sprintf(str, SC(240, "Error in reading library file %s"), LibraryFile);
			MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);
			FileClose(Libfp);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}

		if ((stricmp(Shape.Identification, ShapeCode) != 0) && (stricmp(Shape.Identification, ShapeCode2) != 0)
		        && (stricmp(Shape.Identification, ShapeCode3) != 0))
		{
			sprintf(str, SC(243, "Geometry %s in library %s is not a valid geometry file"), FileName2, LibraryFile);
			MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);

			if ((mode & 2) == 2)
				EditFile[0] = 0;

			return -1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeFile(LPSTR FileName, int32 mode)
{
	char str[MAX_LENGTH_STRING], ProjectIniFile[MAX_LENGTH_STRING];
	int32 res, CheckResult;

	CheckResult = CheckFile(FileName, mode);

	if (CheckResult == -1)
		return;

	if ((mode & 2) == 0)
		IniFile[0] = 0;

	ProjectIniFile[0] = 0;

	if ((FoundDesignPath) && (LibraryFile[0] == 0))
	{
		sprintf(str, "%s\\geom.ini", DesignPath);

		if (FileExistsUTF8(str) == 0)
		{
			if (IniFile[0] == 0)
				strcpy(IniFile, str);
		}
	}

	if (FoundProjectPath)
	{
		sprintf(str, "%s\\geom.ini", ProjectPath);

		if (FileExistsUTF8(str) == 0)
		{
			strcpy(ProjectIniFile, str);

			if (IniFile[0] == 0)
				strcpy(IniFile, str);
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

//  MessageBox(NULL,DesignPath,"ChangeFile2",MB_APPLMODAL+MB_OK);
	DeAllocateMemGeometrie();

	res = 0;

	if (EditFile[0] != 0)
	{
		if (CheckResult == 0)
		{
//      CheckInputMessages();
//      UpdateWindow(GEOMWindow);
			if (!LoadDesign())
				MessageBoxUTF8(GEOMWindow, SC(244, "Error in geometry"), EditFile, MB_APPLMODAL | MB_OK);
		}
	}

	str[0] = 0;

	if (CheckResult == -2)
	{
		if (EditFile[0] != 0)
			GetSymbolNameFromFileName(EditFile, str);

		InitNewShape(str, 0);
	}

	SetWindowName(0);

	if ((mode & 2) == 0)
	{
		ViewFull();
		CheckInputMessages(0);
		CheckInputMessages(0);
		/*
		    if (MessageBufPos!=0) {
		      MessageDialog(SC(48,"Error"),1);
		      DeAllocateMemMessageBuf();
		    }
		*/
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadDesign()
{
	int32 result, Designfp, cnt, LengteFile, Libfp, NrLibEntries, SizeFile, Pos;
	int32 Found = 0;
	char str[MAX_LENGTH_STRING];
	ShapeRecord *Shape;
	LibRecord Lib;
	LibNameRecord LibName;

//  MessageBox(NULL,"LoadDesign",SC(4, "Message"),MB_APPLMODAL|MB_OK);

	Pos = 0;
	SizeFile = 0;

	if (EditFile[0] == 0)
		return 0;

	if (LibraryFile[0] != 0)
	{
		GetFilePartFromFileName(str, EditFile);
		strcpy(EditFile, str);

		if ((Libfp = FileOpenReadOnlyUTF8(LibraryFile)) == -1)
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

				if (stricmpUTF8(LibName.Text, EditFile) == 0)
				{
					Found = 1;
					Pos = LibName.Pos;
					SizeFile = LibName.Length;
				}

				cnt++;
			}
		}

		if (!Found)
		{
			FileClose(Libfp);
			return 0;
		}

		AllocateMemShapes(SizeFile + 16384);
		FileSeek(Libfp, Pos);

		if (FileRead(Libfp, ShapesMem, SizeFile, &result) != 0)
			return 0;

		FileClose(Libfp);
	}
	else
	{
		LengteFile = FileSizeUTF8(EditFile);

		if ((Designfp = FileOpenReadOnlyUTF8(EditFile)) == 0)
			return 0;

		AllocateMemShapes(LengteFile + 16384);

		if (FileRead(Designfp, ShapesMem, MaxShapesMem, &result) != 0)
			return 0;

		FileClose(Designfp);
	}

	Shape = (ShapeRecord *) ShapesMem;

	if ((stricmp(Shape->Identification, ShapeCode) != 0) && (stricmp(Shape->Identification, ShapeCode2) != 0)
	        && (stricmp(Shape->Identification, ShapeCode3) != 0))
	{
		sprintf(str, SC(238, "File %s is not a valid geometry file"), EditFile);
		MessageBoxUTF8(GEOMWindow, str, SC(48, "Error"), MB_OK);
		return 0;
	}

	NrObjects = 0;
	LoadObjects();
	LastActionNr = 1;
	MaxLastActionNr = 1;
	FindMinMaxBoard(&BoardOX, &BoardOY, &BoardWidth, &BoardHeight, 0);
	SetWindowName(0);
	DeAllocateMemShapes();
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadBitmapFile(LPSTR EditFile)
{
	int32 result;

	result =
	    GetNewFileUTF8(GEOMWindow, NULL, EditFile, EditPath, SC(164, "Bitmap files"), NULL, SC(165, "Import bitmap"),
	                   "bmp", 0);

	if ((result == -1) || (result == 0))
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SaveFileName(int32 Mode)
{

	char str[MAX_LENGTH_STRING];
	int32 result;

	if (Mode == 0)
		str[0] = 0;
	else
		GetFilePartFromFileName(str, EditFile);

	if (EditPath[0] == 0)
	{
		if (FoundDesignPath)
			sprintf(EditPath, "%s\\pcb\\shapes", DesignPath);
		else
			sprintf(EditPath, "%s\\shapes", ProjectPath);
	}

	result = GetNewFileUTF8
	(GEOMWindow, NULL, EditFile, EditPath, SC(116, "Geometry files"), NULL, SC(114, "Save as geometry file"), "shp", 1); //uložit jako

	if (result < 0)
		return 0;

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitNewShape(LPSTR Name, int32 mode)
{
	DeAllocateMemGeometrie();
	memset(&Shape, 0, sizeof(ShapeRecord));
	strcpy((LPSTR) & Shape, ShapeCode2);
	NrObjects = 0;
//  NrPadLayers=2;
	NrPinObjects = 0;
	FileChanged = 0;
	DataBaseChanged = 0;

	if (Name[0] != 0)
	{
		memset(&NewObject, 0, sizeof(ObjectRecord));
		NewObject.Layer = GEOM_NAME_LAYER;
		NewObject.ObjectType = OBJECT_TEXT;
		Shape.ShapeNameOriginX = (500 * 2540);
		Shape.ShapeNameOriginY = (-1000 * 2540);
		NewObject.x1 = Shape.ShapeNameOriginX;
		NewObject.y1 = Shape.ShapeNameOriginY;
		NewObject.x2 = (60 * 2540.0);
		NewObject.Info2 = 0;
		NewObject.Thickness = (float) CurrentSilkscreenLine;
		strcpy(Shape.ShapeName, Name);
		strcpy(NewObject.Text, Shape.ShapeName);
		AddObject(&NewObject);
	}

	Factor = 0.0001;
	Xoffset = -45e5;
	Yoffset = -27e5;
//  if (mode==0) {
//    PostMessage(GEOMWindow,WM_COMMAND,(WPARAM)ID_POPUP_VIEWFULL,(LPARAM)NULL);
//  }
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ImportBitmapAsPin()
{
	BmpHeaderRecord BmpHeader;
	int32 BitActive, MemSize, fp, result, cnt, cnt2, cnt3, cnt4, res, Count, Width, Height, BitPos2;
	ObjectRecord2 TypeObject;
	uint8 LineBytes[4096];
	char FileName[MAX_LENGTH_STRING];
	double StandardResolution, y, PadSize, CurrentX2, CurrentY2;
	float value;
	COLORREF PaletteColors[256];

//  if ((fp=FileOpenReadOnlyUTF8("c:\\wincode\\omgeving4.bmp"))>0) {
//  if ((fp=FileOpenReadOnlyUTF8("c:\\viewplot\\blad1.bmp"))>0) {

	memset(&TypeObject, 0, sizeof(ObjectRecord2));
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
	FileName[0] = 0;

	if (LoadBitmapFile(FileName) != 0)
		return -1;

	memset(&NewObject, 0, sizeof(ObjectRecord));
	NewObject.ObjectType = OBJECT_LINE;
	NewObject.Layer = 0;
	NewObject.Info = OBJECT_SELECTED | 3;
	NewObject.PinNr = -1;
	NewObject.Thickness = (float) (PadSize * 1.2);

	if ((fp = FileOpenReadOnlyUTF8(FileName)) > 0)
	{
		res = sizeof(BmpHeaderRecord);
		FileRead(fp, &BmpHeader, sizeof(BmpHeaderRecord), &result);

		if ((BmpHeader.Identifier != 0x4d42) || (BmpHeader.CompressionType != 0) || (BmpHeader.NrOfPlanes != 1)
		        || (BmpHeader.BitsPerPixel != 1))
		{
			MessageBoxUTF8(GEOMWindow, FileName, SC(162, "Wrong bitmap file"), MB_APPLMODAL | MB_OK);
			FileClose(fp);
			return -1;
		}

		if (BmpHeader.NrColors1 != 2)
		{
//      FileClose(fp);
//      return -2;
		}

		if (LineInputDialog(&TypeObject, SC(163, "Pixel size bitmap")) == 1)
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

				NewObject.Thickness = (float) (PadSize * 1.2);
			}
		}

		CurrentX2 = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
		CurrentY2 = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
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
			NewObject.y1 = (float) (CurrentY2 + y);
			NewObject.y2 = (float) (CurrentY2 + y);
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
						NewObject.x1 = (float) (CurrentX2 + PadSize * BitPos2);
						NewObject.x2 = (float) (CurrentX2 + PadSize * (cnt4 - 1));
						AddObject(&NewObject);
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
				NewObject.x1 = (float) (CurrentX2 + PadSize * BitPos2);
				NewObject.x2 = (float) (CurrentX2 + PadSize * (Width - 1));
				AddObject(&NewObject);
				Count++;
			}
		}

		FileClose(fp);
		PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_MOVE_OBJECTS, (LPARAM) NULL);
//    MoveSelectedObjects(0,0);
	}

	return 0;
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
