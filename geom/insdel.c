/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: insdel.c
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
#include "string.h"
#include "insdel.h"
#include "calc.h"
#include "calcdef.h"
#include "stdio.h"
#include "resource.h"
#include "graphics.h"
#include "draw2.h"
#include "geom.h"
#include "select.h"
#include "files.h"
#include "toets.h"
#include "mainloop.h"
#include "polygon.h"
#include "utf8.h"


typedef int16 int16Array[];

char PinLetters[20] = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N',
                        'P', 'R', 'T', 'U', 'V', 'W', 'Y'
                      };
int32 ok;
GeomCreateRecord NewGeomCreate;
extern HDC OutputDisplay;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void DeleteObjectsPerAction(int32 FoundDeleteNr);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ZeroUnusedObjects(int32 mode)
{
	int32 cnt;
	ObjectRecord *Object;
	ObjectPolygonRecord *ObjectPolygon;

	if (MaxLastActionNr > LastActionNr)
	{
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (Object->AddNr >= LastActionNr)
				Object->AddNr = 0;

			if (Object->DeleteNr >= LastActionNr)
				Object->DeleteNr = 0;
		}

		for (cnt = 0; cnt < NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (ObjectPolygon->AddNr >= LastActionNr)
				ObjectPolygon->AddNr = 0;

			if (ObjectPolygon->DeleteNr >= LastActionNr)
				ObjectPolygon->DeleteNr = 0;
		}
	}

	MaxLastActionNr = LastActionNr;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void UndoObjects()
{
	int32 cnt;
	ObjectRecord *Object, PinObject;
	int32 Changed;
	ObjectPolygonRecord *ObjectPolygon;

	memset(&PinObject, 0, sizeof(PinObject));

	if (LastActionNr < 2)
		return;

	if (MaxLastActionNr < LastActionNr)
		MaxLastActionNr = LastActionNr;

	if (LastActionNr > 1)
		LastActionNr--;

	StartDrawingEditingWindow();

	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (Object->AddNr == LastActionNr)
		{
			if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Object->Info &= ~OBJECT_SELECTED;
				SetBackGroundActive(0);
				DrawObject(Object, 0.0, 0.0, 0);
				SetBackGroundActive(0);
				DrawPinTextObject(Object, 8);

				if (ClearanceVisible)
				{
					SetBackGroundActive(0);
					DrawObjectWithClearance(Object, 0.0, 0.0, 0);
				}

				Object->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->AddNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectPolygon->Info &= ~OBJECT_SELECTED;
				SetBackGroundActive(0);
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
				SetBackGroundActive(0);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
				DrawPinTextObject(&PinObject, 8);

				if (ClearanceVisible)
				{
					SetBackGroundActive(0);
//          DrawObjectWithClearance(Object,0.0,0.0,0);
				}

				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	BackGroundActive = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (Object->DeleteNr == LastActionNr)
		{
			if ((Object->Info & (OBJECT_NOT_VISIBLE)) == (OBJECT_NOT_VISIBLE))
			{
				Object->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
				DrawObject(Object, 0.0, 0.0, 0);
				DrawPinTextObject(Object, 8);

				if (ClearanceVisible)
					DrawObjectWithClearance(Object, 0.0, 0.0, 0);

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->DeleteNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == (OBJECT_NOT_VISIBLE))
			{
				ObjectPolygon->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
				DrawPinTextObject(&PinObject, 8);

				if (ClearanceVisible)
					DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, 0);

				Changed = 1;
			}
		}
	}

	if (Changed)
	{
		DataBaseChanged = 1;
		UndoRedoActive = 1;
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RedoObjects()
{
	int32 cnt;
	ObjectRecord *Object, PinObject;
	int32 Changed;
	ObjectPolygonRecord *ObjectPolygon;

	memset(&PinObject, 0, sizeof(PinObject));

	if ((LastActionNr > MaxLastActionNr) || (LastActionNr == 0))
		return;

	StartDrawingEditingWindow();

	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (Object->DeleteNr == LastActionNr)
		{
			if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				SetBackGroundActive(0);
				DrawObject(Object, 0.0, 0.0, 0);
				SetBackGroundActive(0);
				DrawPinTextObject(Object, 8);

				if (ClearanceVisible)
				{
					SetBackGroundActive(0);
					DrawObjectWithClearance(Object, 0.0, 0.0, 0);
				}

				Object->Info &= ~(OBJECT_SELECTED);
				Object->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->DeleteNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				SetBackGroundActive(0);
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
				SetBackGroundActive(0);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.PinNr = ObjectPolygon->PinNr;
				PinObject.Layer = ObjectPolygon->Layer;
				DrawPinTextObject(&PinObject, 8);

				if (ClearanceVisible)
				{
					SetBackGroundActive(0);
					DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, 0);
				}

				ObjectPolygon->Info &= ~(OBJECT_SELECTED);
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	BackGroundActive = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (Object->AddNr == LastActionNr)
		{
			if ((Object->Info & (OBJECT_NOT_VISIBLE)) == (OBJECT_NOT_VISIBLE))
			{
				Object->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
				DrawObject(Object, 0.0, 0.0, 0);
				DrawPinTextObject(Object, 8);

				if (ClearanceVisible)
					DrawObjectWithClearance(Object, 0.0, 0.0, 0);

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (ObjectPolygon->AddNr == LastActionNr)
		{
			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == (OBJECT_NOT_VISIBLE))
			{
				ObjectPolygon->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				DrawPinTextObject(&PinObject, 8);

				if (ClearanceVisible)
					DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, 0);

				Changed = 1;
			}
		}
	}

	if (MaxLastActionNr > LastActionNr)
		LastActionNr++;

	if (Changed)
	{
		DataBaseChanged = 1;
		UndoRedoActive = 1;
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteObjects()
{
	int32 cnt;
	ObjectRecord *Object, PinObject;
	ObjectPolygonRecord *ObjectPolygon;

	memset(&PinObject, 0, sizeof(PinObject));

	StartDrawingEditingWindow();

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->Layer != GEOM_NAME_LAYER))
		{
			Object->Info &= ~OBJECT_SELECTED;
			SetBackGroundActive(0);
			DrawObject(Object, 0.0, 0.0, 0);
			SetBackGroundActive(0);
			DrawPinTextObject(Object, 8);

			if (ClearanceVisible)
			{
				SetBackGroundActive(0);
				DrawObjectWithClearance(Object, 0.0, 0.0, 0);
			}

			ZeroUnusedObjects(0);
			Object->Info |= OBJECT_NOT_VISIBLE;
			Object->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->Layer != GEOM_NAME_LAYER))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			SetBackGroundActive(0);
			DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
			SetBackGroundActive(0);
			PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
			PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
			PinObject.PinNr = ObjectPolygon->PinNr;
			PinObject.Layer = ObjectPolygon->Layer;
			DrawPinTextObject(&PinObject, 8);

			if (ClearanceVisible)
			{
				SetBackGroundActive(0);
				DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, 0);
			}

			ZeroUnusedObjects(0);
			ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
			ObjectPolygon->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	BackGroundActive = 0;
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 AddObject(ObjectRecord * Object)
{
	ObjectRecord *Object3;
	ObjectPolygonRecord *ObjectPolygon;

	if (Object->ObjectType == OBJECT_TEXT)
	{
		if (Object->Text[0] == 0)
			return 0;
	}

#ifdef _DEBUG

	if (NrObjects == 6832)
		ok = 1;

#endif


	if (Object->ObjectType == OBJECT_POLYGON)
	{
		ObjectPolygon = (ObjectPolygonRecord *) Object->Address;
		ObjectPolygon->Info = (int16) Object->Info;
		return AddObjectPolygon(ObjectPolygon);
	}

	if (NrObjects >= MaxNrObjects)
	{
		if (AllocateMemObjects(MaxNrObjects + 128) != 0)
			return 0;
	}

	DataBaseChanged = 1;
	ZeroUnusedObjects(0);
	Object->AddNr = (int16) LastActionNr;
	Object->DeleteNr = 0;
	Object3 = &((*Objects)[NrObjects]);
	memmove(Object3, Object, sizeof(ObjectRecord));
	GetObjectSize(Object3, &VisibleMinX, &VisibleMinY, &VisibleMaxX, &VisibleMaxY);
	NrObjects++;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectPolygon2(int32 PolygonIndex)
{
	int32 ObjectPolygonLength;
	int32 *ObjectPolygonPos;
	uint8 *ObjectPolygonP;
	ObjectPolygonRecord *NewObjectPolygon2, *ObjectPolygon;

	ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[PolygonIndex]]);

	if (NrObjectPolygons >= MaxNrObjectPolygons)
	{
		if (AllocateMemObjectPolygons(MaxNrObjectPolygons + 128) != 0)
			return -1;
	}

	ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

	if (ObjectPolygonMemorySize + ObjectPolygonLength >= MaxObjectPolygonMemory)
	{
		if (AllocateMemObjectPolygonMemory(ObjectPolygonMemorySize + ObjectPolygonLength + 32768) != 0)
			return 0;

		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[PolygonIndex]]);
	}

	ObjectPolygonPos = &((*ObjectPolygons)[NrObjectPolygons]);
	ObjectPolygonP = &(ObjectPolygonMem[ObjectPolygonMemorySize]);
	memmove(ObjectPolygonP, ObjectPolygon, ObjectPolygonLength);
	NewObjectPolygon2 = (ObjectPolygonRecord *) ObjectPolygonP;
	*ObjectPolygonPos = ObjectPolygonMemorySize;

	ZeroUnusedObjects(0);
	NewObjectPolygon2->AddNr = (int16) LastActionNr;
	NewObjectPolygon2->DeleteNr = 0;
	SetMinMaxObjectPolygon(NewObjectPolygon2, 0);

	NrObjectPolygons++;
	ObjectPolygonMemorySize += ObjectPolygonLength;

	DataBaseChanged = 1;
	return 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectPolygon(ObjectPolygonRecord * ObjectPolygon)
{
	int32 ObjectPolygonLength, *ObjectPolygonPos;
	uint8 *ObjectPolygonP;
	ObjectPolygonRecord *NewObjectPolygon2;

#ifdef _DEBUG

	if (NrObjectPolygons == 42)
		ok = 1;

#endif

	if (NrObjectPolygons >= MaxNrObjectPolygons)
	{
		if (AllocateMemObjectPolygons(MaxNrObjectPolygons + 128) != 0)
			return -1;
	}

	ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

	if (ObjectPolygonMemorySize + ObjectPolygonLength >= MaxObjectPolygonMemory)
	{
		if (AllocateMemObjectPolygonMemory(ObjectPolygonMemorySize + ObjectPolygonLength + 32768) != 0)
			return 0;
	}

	ObjectPolygonPos = &((*ObjectPolygons)[NrObjectPolygons]);
	ObjectPolygonP = &(ObjectPolygonMem[ObjectPolygonMemorySize]);
	memmove(ObjectPolygonP, ObjectPolygon, ObjectPolygonLength);
	NewObjectPolygon2 = (ObjectPolygonRecord *) ObjectPolygonP;
	*ObjectPolygonPos = ObjectPolygonMemorySize;

	ZeroUnusedObjects(0);
	NewObjectPolygon2->AddNr = (int16) LastActionNr;
	NewObjectPolygon2->DeleteNr = 0;
	SetMinMaxObjectPolygon(NewObjectPolygon2, 0);

	NrObjectPolygons++;
	ObjectPolygonMemorySize += ObjectPolygonLength;

	DataBaseChanged = 1;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObject2(ObjectRecord * Object)
{
	ObjectRecord *Object3;

	if (NrObjects2 >= MaxNrObjects2)
	{
		if (AllocateMemObjects2(MaxNrObjects2 + 128) != 0)
			return 0;
	}

	ZeroUnusedObjects(0);
	Object->AddNr = (int16) LastActionNr;
	Object->DeleteNr = 0;
	Object3 = &((*Objects2)[NrObjects2]);
	memmove(Object3, Object, sizeof(ObjectRecord));
	NrObjects2++;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 AddPinInfo(PinInfoRecord * PinInfoObject)
{
	PinInfoRecord *NewPinInfo;

	if (NrPinObjects >= MaxNrPinObjects)
	{
		if (AllocateMemPinObjects(MaxNrPinObjects * 2) != 0)
			return 0;
	}

	NewPinInfo = &((*PinInfos)[NrPinObjects]);
	memmove(NewPinInfo, PinInfoObject, sizeof(PinInfoRecord));
	DataBaseChanged = 1;
	NrPinObjects++;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateBGA()
{

// Check parameters
	int32 cntx, cnty, let2, PinStrPos;
	char PinStr[12];
	double PinPosX, PinPosY, OffsetX, OffsetY;
	PinInfoRecord NewPinInfo;
	ObjectRecord BGAObject;

	if ((NewGeomCreate.NrPinsX <= 0) && (NewGeomCreate.NrPinsX > 100))
	{
		MessageBoxUTF8(GEOMWindow, SC(254, "Nr pins x direction <= 0 or >100"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if ((NewGeomCreate.NrPinsY <= 0) && (NewGeomCreate.NrPinsY > 100))
	{
		MessageBoxUTF8(GEOMWindow, SC(255, "Nr pins y direction <= 0 or >100"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(256, "Pitch < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pad < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(257, "Pad < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(259, "Pitch < pad size"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PasteX > NewGeomCreate.PadX)
		MessageBoxUTF8(GEOMWindow, SC(260, "Pad size solder paste > pad size"), SC(49, "Warning"), MB_OK);

	NrObjects = 0;
	NrPinObjects = 0;
	NrObjectPolygons = 0;
	InitNewShape("noname", 1);
	OffsetX = -((NewGeomCreate.NrPinsX - 1) * NewGeomCreate.Pitch) * 0.5;
	OffsetY = ((NewGeomCreate.NrPinsY - 1) * NewGeomCreate.Pitch) * 0.5;

	for (cntx = 0; cntx < NewGeomCreate.NrPinsX; cntx++)
	{
		let2 = (cntx / 20);

		for (cnty = 0; cnty < NewGeomCreate.NrPinsY; cnty++)
		{

// *******************************************************************************
// Add pad
// *******************************************************************************

			memset(&BGAObject, 0, sizeof(ObjectRecord));
			BGAObject.ObjectType = OBJECT_CIRCLE;
			PinPosX = NewGeomCreate.Pitch * (cntx);
			PinPosY = NewGeomCreate.Pitch * (cnty);
			BGAObject.Layer = NrPadLayers - 1;

			BGAObject.x1 = (float) (PinPosY + OffsetX);
			BGAObject.y1 = (float) (-PinPosX + OffsetY);
			BGAObject.x2 = (float) NewGeomCreate.Pad;
			BGAObject.y2 = 0.0;

			if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
			{
				BGAObject.ObjectType = OBJECT_RECT;
				BGAObject.y2 = (float) NewGeomCreate.Pad;
			}

			BGAObject.PinNr = NrPinObjects;
			BGAObject.Clearance = (float) NewGeomCreate.Clearance;
			AddObject(&BGAObject);

// *******************************************************************************
// Add paste mask
// *******************************************************************************

			BGAObject.Clearance = 0.0;
			BGAObject.PinNr = -1;

			if (NewGeomCreate.Paste > 0.0)
			{
				BGAObject.ObjectType = OBJECT_CIRCLE;
				BGAObject.Layer = PASTE_MASK_TOP_LAYER;
				BGAObject.x2 = (float) NewGeomCreate.Paste;
				BGAObject.y2 = 0.0;

				if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
				{
					BGAObject.ObjectType = OBJECT_RECT;
					BGAObject.y2 = (float) NewGeomCreate.Paste;
				}

				AddObject(&BGAObject);
			}

// *******************************************************************************
// Add solder mask
// *******************************************************************************

			if (NewGeomCreate.Mask > 0.0)
			{
				BGAObject.ObjectType = OBJECT_CIRCLE;
				BGAObject.Layer = SOLD_MASK_TOP_LAYER;
				BGAObject.x2 = (float) NewGeomCreate.Mask;
				BGAObject.y2 = 0.0;

				if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
				{
					BGAObject.ObjectType = OBJECT_RECT;
					BGAObject.y2 = (float) NewGeomCreate.Mask;
				}

				AddObject(&BGAObject);
			}

// *******************************************************************************
// *******************************************************************************

			memset(&PinStr, 0, 12);
			PinStr[0] = 0;
			PinStrPos = 0;

			if (let2 > 0)
			{
				PinStr[0] = (char) (64 + let2);
				PinStrPos = 1;
			}

			PinStr[PinStrPos] = PinLetters[cntx % 20];
			PinStrPos++;

			if (cnty > 8)
			{
				PinStr[PinStrPos] = (char) (48 + (cnty + 1) / 10);
				PinStrPos++;
			}

			PinStr[PinStrPos] = (char) (48 + ((cnty + 1) % 10));
			PinStrPos++;

			memmove(&NewPinInfo.PinText, &PinStr, 12);
			AddPinInfo(&NewPinInfo);
		}
	}

	ViewFull();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateDIL()
{
// Check parameters
	int32 cnt, cnt2;
	double PinPosX, PinPosY;
	PinInfoRecord NewPinInfo;
	ObjectRecord DILObject;

	if ((NewGeomCreate.NrPins <= 0) && (NewGeomCreate.NrPins > 1000))
	{
		MessageBoxUTF8(GEOMWindow, SC(261, "Nr pins y direction <= 0 or >1000"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PitchX < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(262, "Pitch x direction < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PitchY < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(263, "Pitch y direction < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pad < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(257, "Pad < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PitchX < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(264, "Pitch x direction < pad size"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PitchY < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(265, "Pitch y direction < pad size"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Drill > NewGeomCreate.Pad)
		MessageBoxUTF8(GEOMWindow, SC(266, "Pad size < drill size"), SC(49, "Warning"), MB_OK);

	NrObjects = 0;
	NrPinObjects = 0;
	NrObjectPolygons = 0;
	InitNewShape("noname", 1);

	for (cnt = 0; cnt < NewGeomCreate.NrPins * 2; cnt++)
	{
		memset(&NewPinInfo, 0, sizeof(PinInfoRecord));

// *******************************************************************************
// Add pad solder side
// *******************************************************************************

		memset(&DILObject, 0, sizeof(ObjectRecord));
		DILObject.ObjectType = OBJECT_CIRCLE;

		if (cnt < NewGeomCreate.NrPins)
		{
			PinPosX = 0.0;
			PinPosY = NewGeomCreate.PitchY * (-cnt);
		}
		else
		{
			PinPosX = NewGeomCreate.PitchX;
			PinPosY = NewGeomCreate.PitchY * (-(NewGeomCreate.NrPins * 2 - cnt - 1));
		}

		DILObject.Layer = 0;
		DILObject.x1 = (float) PinPosX;
		DILObject.y1 = (float) PinPosY;
		DILObject.x2 = (float) NewGeomCreate.Pad;

		if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
		{
			DILObject.ObjectType = OBJECT_RECT;
			DILObject.y2 = (float) NewGeomCreate.Pad;
		}

		DILObject.PinNr = NrPinObjects;
		DILObject.Clearance = (float) NewGeomCreate.Clearance;
		AddObject(&DILObject);

// *******************************************************************************
// Add pad component side
// *******************************************************************************

		DILObject.ObjectType = OBJECT_CIRCLE;
		DILObject.Layer = NrPadLayers - 1;
		DILObject.x2 = (float) NewGeomCreate.Pad;
		DILObject.y2 = 0.0;

		if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
		{
			DILObject.ObjectType = OBJECT_RECT;
			DILObject.y2 = (float) NewGeomCreate.Pad;
		}

		AddObject(&DILObject);


// *******************************************************************************
// Add drill
// *******************************************************************************

		DILObject.ObjectType = OBJECT_CIRCLE;
		DILObject.Layer = DRILL_LAYER;
		DILObject.x2 = (float) NewGeomCreate.Drill;
		DILObject.y2 = 0.0;
		AddObject(&DILObject);

// *******************************************************************************
// Add anti power pad
// *******************************************************************************

		if (NewGeomCreate.PowerPad > 0.0)
		{
			DILObject.Layer = POWER_PAD_LAYER;
			DILObject.x2 = (float) NewGeomCreate.PowerPad;
			AddObject(&DILObject);
		}

// *******************************************************************************
// Add inner pad
// *******************************************************************************

		if (NrPadLayers > 2)
		{
			for (cnt2 = 1; cnt2 < NrPadLayers - 1; cnt2++)
			{
				DILObject.Layer = cnt2;
				DILObject.x2 = (float) NewGeomCreate.InnerPad;
				AddObject(&DILObject);
			}
		}
		else
		{
			if (NewGeomCreate.InnerPad > 0.0)
			{
				DILObject.Layer = INNER_PAD_LAYER;
				DILObject.x2 = NewGeomCreate.InnerPad;
				AddObject(&DILObject);
			}
		}

// *******************************************************************************
// Add solder mask
// *******************************************************************************

		DILObject.Clearance = 0.0;
		DILObject.PinNr = -1;

		if (NewGeomCreate.Mask > 0.0)
		{
			DILObject.ObjectType = OBJECT_CIRCLE;
			DILObject.Layer = SOLD_MASK_BOTTOM_LAYER;
			DILObject.x2 = (float) NewGeomCreate.Mask;
			DILObject.y2 = 0.0;

			if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
			{
				DILObject.ObjectType = OBJECT_RECT;
				DILObject.y2 = (float) NewGeomCreate.Mask;
			}

			AddObject(&DILObject);
			DILObject.Layer = SOLD_MASK_TOP_LAYER;
			AddObject(&DILObject);
		}

// *******************************************************************************
// *******************************************************************************

		sprintf(NewPinInfo.PinText, "%i", NrPinObjects + 1);
		AddPinInfo(&NewPinInfo);

	}

	ViewFull();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateSOIC()
{

// Check parameters
	int32 cnt;
	double PinPosX, PinPosY;
	PinInfoRecord NewPinInfo;
	ObjectRecord SOICObject;

	if ((NewGeomCreate.NrPins <= 0) && (NewGeomCreate.NrPins > 1000))
	{
		MessageBoxUTF8(GEOMWindow, SC(261, "Nr pins y direction <= 0 or >1000"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PitchX < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(262, "Pitch x direction < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PitchY < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(263, "Pitch y direction < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PadX < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(267, "Pad x < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PadY < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(268, "Pad y < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PitchY < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(269, "Pitch y direction < pad y direction"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PasteX > NewGeomCreate.PadX)
		MessageBoxUTF8(GEOMWindow, SC(270, "Pad x solder paste > pad x"), SC(49, "Warning"), MB_OK);

	if (NewGeomCreate.PasteY > NewGeomCreate.PadY)
		MessageBoxUTF8(GEOMWindow, SC(271, "Pad y solder paste > pad y"), SC(49, "Warning"), MB_OK);

	NrObjects = 0;
	NrPinObjects = 0;
	NrObjectPolygons = 0;
	InitNewShape("noname", 1);

	for (cnt = 0; cnt < NewGeomCreate.NrPins * 2; cnt++)
	{

// *******************************************************************************
// Add pad
// *******************************************************************************

		memset(&SOICObject, 0, sizeof(ObjectRecord));
		SOICObject.ObjectType = OBJECT_RECT;

		if (cnt < NewGeomCreate.NrPins)
		{
			PinPosX = 0.0;
			PinPosY = NewGeomCreate.PitchY * (-cnt);
		}
		else
		{
			PinPosX = NewGeomCreate.PitchX;
			PinPosY = NewGeomCreate.PitchY * (-(NewGeomCreate.NrPins * 2 - cnt - 1));
		}

		PinPosX -= NewGeomCreate.PitchX * 0.5;
		PinPosY += NewGeomCreate.PitchY * ((NewGeomCreate.NrPins - 1) * 0.5);
		SOICObject.Layer = NrPadLayers - 1;
		SOICObject.x1 = (float) PinPosX;
		SOICObject.y1 = (float) PinPosY;
		SOICObject.x2 = (float) NewGeomCreate.PadX;
		SOICObject.y2 = (float) NewGeomCreate.PadY;
		SOICObject.PinNr = NrPinObjects;
		SOICObject.Clearance = (float) NewGeomCreate.Clearance;
		AddObject(&SOICObject);

// *******************************************************************************
// Add paste mask
// *******************************************************************************

		SOICObject.Clearance = 0.0;

		if ((NewGeomCreate.PasteX > 0.0) && (NewGeomCreate.PasteY > 0.0))
		{
			SOICObject.Layer = PASTE_MASK_TOP_LAYER;
			SOICObject.x2 = (float) NewGeomCreate.PasteX;
			SOICObject.y2 = (float) NewGeomCreate.PasteY;
			SOICObject.PinNr = -1;
			AddObject(&SOICObject);
		}

// *******************************************************************************
// Add solder mask
// *******************************************************************************

		if ((NewGeomCreate.MaskX > 0.0) && (NewGeomCreate.MaskY > 0.0))
		{
			SOICObject.Layer = SOLD_MASK_TOP_LAYER;
			SOICObject.x2 = (float) NewGeomCreate.MaskX;
			SOICObject.y2 = (float) NewGeomCreate.MaskY;
			AddObject(&SOICObject);
		}

// *******************************************************************************
// *******************************************************************************

		memset(&NewPinInfo, 0, sizeof(PinInfoRecord));
		sprintf(NewPinInfo.PinText, "%i", NrPinObjects + 1);
		AddPinInfo(&NewPinInfo);
	}

	ViewFull();
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateSIL()
{

// Check parameters
	int32 cnt, cnt2, cnt4, PinNr, PinCount, MaxPinCount, Found;
	double PinPosX, PinPosY;
	PinInfoRecord NewPinInfo, *PinInfo;
	ObjectRecord SILObject;
	char NewPinText[100];

	if ((NewGeomCreate.NrPins <= 0) && (NewGeomCreate.NrPins > 1000))
	{
		MessageBoxUTF8(GEOMWindow, SC(261, "Nr pins y direction <= 0 or >1000"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(263, "Pitch y direction < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pad < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(257, "Pad < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(265, "Pitch y direction < pad size"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Drill > NewGeomCreate.Pad)
		MessageBoxUTF8(GEOMWindow, SC(266, "Pad size < drill size"), SC(49, "Warning"), MB_OK);

	NrObjects2 = 0;
	PinCount = NewGeomCreate.Start2;
	MaxPinCount = NewGeomCreate.Start2 + (NewGeomCreate.NrPins - 1) * NewGeomCreate.PinInc;

	for (cnt = 0; cnt < NewGeomCreate.NrPins; cnt++)
	{
		memset(&SILObject, 0, sizeof(ObjectRecord));

		if (NewGeomCreate.Start1[0] != 0)
		{
			if (MaxPinCount > 99)
				NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 3] = 0;
			else
			{
				if (MaxPinCount > 9)
					NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 2] = 0;
				else
					NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 1] = 0;
			}

			sprintf(NewPinText, "%s%i", NewGeomCreate.Start1, PinCount);
		}
		else
			sprintf(NewPinText, "%i", PinCount);

		Found = -1;
		cnt4 = 0;

		while ((cnt4 < NrPinObjects) && (Found == -1))
		{
			PinInfo = &((*PinInfos)[cnt4]);

			if (stricmpUTF8((LPSTR) PinInfo->PinText, NewPinText) == 0)
				Found = cnt4;

			cnt4++;
		}

		if (Found == -1)
		{
			memset(&NewPinInfo, 0, sizeof(PinInfoRecord));
			memmove(&NewPinInfo.PinText, NewPinText, sizeof(NewPinInfo.PinText) - 1);
			Found = NrPinObjects;
			AddPinInfo(&NewPinInfo);
		}

		PinNr = Found;

// *******************************************************************************
// Add pad solder side
// *******************************************************************************

		PinPosX = 0.0;
		PinPosY = NewGeomCreate.Pitch * (-cnt);

		SILObject.ObjectType = OBJECT_CIRCLE;
		SILObject.Layer = 0;
		SILObject.x1 = (float) PinPosX;
		SILObject.y1 = (float) PinPosY;
		SILObject.x2 = (float) NewGeomCreate.Pad;
		SILObject.PinNr = PinNr;
		SILObject.Clearance = (float) NewGeomCreate.Clearance;

		if (NewGeomCreate.Pin1Square)
		{
			SILObject.ObjectType = OBJECT_RECT;
			SILObject.y2 = (float) NewGeomCreate.Pad;
		}

		AddObject2(&SILObject);

// *******************************************************************************
// Add pad component side
// *******************************************************************************

		SILObject.Layer = NrPadLayers - 1;
		SILObject.ObjectType = OBJECT_CIRCLE;

		if (NewGeomCreate.Pin1Square)
		{
			SILObject.ObjectType = OBJECT_RECT;
			SILObject.y2 = (float) NewGeomCreate.Pad;
		}

		AddObject2(&SILObject);

// *******************************************************************************
// Add drill
// *******************************************************************************

		SILObject.ObjectType = OBJECT_CIRCLE;
		SILObject.Layer = DRILL_LAYER;
		SILObject.x2 = (float) NewGeomCreate.Drill;
		AddObject2(&SILObject);

// *******************************************************************************
// Add anti power pad
// *******************************************************************************

		if (NewGeomCreate.PowerPad > 0.0)
		{
			SILObject.Layer = POWER_PAD_LAYER;
			SILObject.x2 = (float) NewGeomCreate.PowerPad;
			AddObject2(&SILObject);
		}

// *******************************************************************************
// Add inner pad
// *******************************************************************************

		if (NrPadLayers > 2)
		{
			SILObject.PinNr = PinNr;
			SILObject.x2 = (float) NewGeomCreate.InnerPad;

			for (cnt2 = 1; cnt2 < NrPadLayers - 1; cnt2++)
			{
				SILObject.Layer = cnt2;
				AddObject2(&SILObject);
			}
		}
		else
		{
			if (NewGeomCreate.InnerPad > 0.0)
			{
				SILObject.Layer = INNER_PAD_LAYER;
				SILObject.x2 = NewGeomCreate.InnerPad;
				SILObject.y2 = (float) 0.0;
				AddObject2(&SILObject);
			}
		}

// *******************************************************************************
// Add solder mask
// *******************************************************************************

		if (NewGeomCreate.Mask > 0.0)
		{
			SILObject.ObjectType = OBJECT_CIRCLE;
			SILObject.Layer = SOLD_MASK_TOP_LAYER;
			SILObject.x2 = (float) NewGeomCreate.Mask;

			if (NewGeomCreate.Pin1Square)
			{
				SILObject.ObjectType = OBJECT_RECT;
				SILObject.y2 = (float) NewGeomCreate.Mask;
			}

			SILObject.PinNr = -1;
			AddObject2(&SILObject);

			SILObject.Layer = SOLD_MASK_BOTTOM_LAYER;
			AddObject2(&SILObject);

		}

// *******************************************************************************
// *******************************************************************************

		PinCount += NewGeomCreate.PinInc;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateSIL_SMD_RECT()
{
// Check parameters
	int32 cnt, cnt4, Found, PinNr, PinCount, MaxPinCount;
	double PinPosX, PinPosY;
	PinInfoRecord NewPinInfo, *PinInfo;
	ObjectRecord SIL_SMD_RECTObject;
	char NewPinText[100];

	if ((NewGeomCreate.NrPins <= 0) && (NewGeomCreate.NrPins > 1000))
	{
		MessageBoxUTF8(GEOMWindow, SC(261, "Nr pins y direction <= 0 or >1000"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(263, "Pitch y direction < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PadX < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(267, "Pad x < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PadY < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(268, "Pad y < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(269, "Pitch y direction < pad y direction"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PasteX > NewGeomCreate.PadX)
		MessageBoxUTF8(GEOMWindow, SC(270, "Pad x solder paste > pad x"), SC(49, "Warning"), MB_OK);

	if (NewGeomCreate.PasteY > NewGeomCreate.PadY)
		MessageBoxUTF8(GEOMWindow, SC(271, "Pad y solder paste > pad y"), SC(49, "Warning"), MB_OK);

	NrObjects2 = 0;
	PinCount = NewGeomCreate.Start2;
	MaxPinCount = NewGeomCreate.Start2 + (NewGeomCreate.NrPins - 1) * NewGeomCreate.PinInc;

	for (cnt = 0; cnt < NewGeomCreate.NrPins; cnt++)
	{

		if (NewGeomCreate.Start1[0] != 0)
		{
			if (MaxPinCount > 99)
				NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 3] = 0;
			else
			{
				if (MaxPinCount > 9)
					NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 2] = 0;
				else
					NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 1] = 0;
			}

			sprintf(NewPinText, "%s%i", NewGeomCreate.Start1, PinCount);
		}
		else
			sprintf(NewPinText, "%i", PinCount);

		Found = -1;
		cnt4 = 0;

		while ((cnt4 < NrPinObjects) && (Found == -1))
		{
			PinInfo = &((*PinInfos)[cnt4]);

			if (stricmpUTF8((LPSTR) PinInfo->PinText, NewPinText) == 0)
				Found = cnt4;

			cnt4++;
		}

		if (Found == -1)
		{
			memset(&NewPinInfo, 0, sizeof(PinInfoRecord));
			memmove(&NewPinInfo.PinText, NewPinText, sizeof(NewPinInfo.PinText) - 1);
			Found = NrPinObjects;
			AddPinInfo(&NewPinInfo);
		}

		PinNr = Found;

// *******************************************************************************
// Add pad
// *******************************************************************************

		memset(&SIL_SMD_RECTObject, 0, sizeof(ObjectRecord));
		SIL_SMD_RECTObject.ObjectType = OBJECT_RECT;
		PinPosX = 0.0;
		PinPosY = NewGeomCreate.Pitch * (-cnt);
		SIL_SMD_RECTObject.Layer = NrPadLayers - 1;

		if (NewGeomCreate.Layer == 0)
			SIL_SMD_RECTObject.Layer = 0;

		SIL_SMD_RECTObject.x1 = (float) PinPosX;
		SIL_SMD_RECTObject.y1 = (float) PinPosY;
		SIL_SMD_RECTObject.x2 = (float) NewGeomCreate.PadX;
		SIL_SMD_RECTObject.y2 = (float) NewGeomCreate.PadY;
		SIL_SMD_RECTObject.PinNr = PinNr;
		SIL_SMD_RECTObject.Clearance = (float) NewGeomCreate.Clearance;
		AddObject2(&SIL_SMD_RECTObject);

// *******************************************************************************
// Add paste mask
// *******************************************************************************

		SIL_SMD_RECTObject.Clearance = 0.0;

		if ((NewGeomCreate.PasteX > 0.0) && (NewGeomCreate.PasteY > 0.0))
		{
			SIL_SMD_RECTObject.Layer = PASTE_MASK_TOP_LAYER;

			if (NewGeomCreate.Layer == 0)
				SIL_SMD_RECTObject.Layer = PASTE_MASK_BOTTOM_LAYER;

			SIL_SMD_RECTObject.x2 = (float) NewGeomCreate.PasteX;
			SIL_SMD_RECTObject.y2 = (float) NewGeomCreate.PasteY;
			SIL_SMD_RECTObject.PinNr = -1;
			AddObject2(&SIL_SMD_RECTObject);
		}

// *******************************************************************************
// Add solder mask
// *******************************************************************************

		if ((NewGeomCreate.MaskX > 0.0) && (NewGeomCreate.MaskY > 0.0))
		{
			SIL_SMD_RECTObject.Layer = SOLD_MASK_TOP_LAYER;

			if (NewGeomCreate.Layer == 0)
				SIL_SMD_RECTObject.Layer = SOLD_MASK_BOTTOM_LAYER;

			SIL_SMD_RECTObject.x2 = (float) NewGeomCreate.MaskX;
			SIL_SMD_RECTObject.y2 = (float) NewGeomCreate.MaskY;
			SIL_SMD_RECTObject.PinNr = -1;
			AddObject2(&SIL_SMD_RECTObject);
		}

// *******************************************************************************
// *******************************************************************************

		PinCount += NewGeomCreate.PinInc;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateSIL_SMD_CIRCLE()
{
// Check parameters
	int32 cnt, PinNr, PinCount, MaxPinCount, cnt4, Found;
	double PinPosX, PinPosY;
	PinInfoRecord NewPinInfo, *PinInfo;
	ObjectRecord SIL_SMD_CIRCLEObject;
	char NewPinText[100];

	if ((NewGeomCreate.NrPins <= 0) && (NewGeomCreate.NrPins > 1000))
	{
		MessageBoxUTF8(GEOMWindow, SC(261, "Nr pins y direction <= 0 or >1000"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(263, "Pitch y direction < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pad < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(257, "Pad < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(269, "Pitch y direction < pad y direction"), SC(48, "Error"), MB_OK);
		return -1;
	}

	NrObjects2 = 0;
	PinCount = NewGeomCreate.Start2;
	MaxPinCount = NewGeomCreate.Start2 + (NewGeomCreate.NrPins - 1) * NewGeomCreate.PinInc;

	for (cnt = 0; cnt < NewGeomCreate.NrPins; cnt++)
	{

		if (NewGeomCreate.Start1[0] != 0)
		{
			if (MaxPinCount > 99)
				NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 3] = 0;
			else
			{
				if (MaxPinCount > 9)
					NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 2] = 0;
				else
					NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 1] = 0;
			}

			sprintf(NewPinText, "%s%i", NewGeomCreate.Start1, PinCount);
		}
		else
			sprintf(NewPinText, "%i", PinCount);

		Found = -1;
		cnt4 = 0;

		while ((cnt4 < NrPinObjects) && (Found == -1))
		{
			PinInfo = &((*PinInfos)[cnt4]);

			if (stricmpUTF8((LPSTR) PinInfo->PinText, NewPinText) == 0)
				Found = cnt4;

			cnt4++;
		}

		if (Found == -1)
		{
			memset(&NewPinInfo, 0, sizeof(PinInfoRecord));
			memmove(&NewPinInfo.PinText, NewPinText, sizeof(NewPinInfo.PinText) - 1);
			Found = NrPinObjects;
			AddPinInfo(&NewPinInfo);
		}

		PinNr = Found;

// *******************************************************************************
// Add pad
// *******************************************************************************

		memset(&SIL_SMD_CIRCLEObject, 0, sizeof(ObjectRecord));
		SIL_SMD_CIRCLEObject.ObjectType = OBJECT_CIRCLE;
		PinPosX = 0.0;
		PinPosY = NewGeomCreate.Pitch * (-cnt);
		SIL_SMD_CIRCLEObject.Layer = NrPadLayers - 1;

		if (NewGeomCreate.Layer == 0)
			SIL_SMD_CIRCLEObject.Layer = 0;

		SIL_SMD_CIRCLEObject.x1 = (float) PinPosX;
		SIL_SMD_CIRCLEObject.y1 = (float) PinPosY;
		SIL_SMD_CIRCLEObject.x2 = (float) NewGeomCreate.Pad;
		SIL_SMD_CIRCLEObject.PinNr = PinNr;
		SIL_SMD_CIRCLEObject.Clearance = (float) NewGeomCreate.Clearance;
		AddObject2(&SIL_SMD_CIRCLEObject);

// *******************************************************************************
// Add paste mask
// *******************************************************************************

		SIL_SMD_CIRCLEObject.Clearance = 0.0;

		if (NewGeomCreate.Paste > 0.0)
		{
			SIL_SMD_CIRCLEObject.Layer = PASTE_MASK_TOP_LAYER;

			if (NewGeomCreate.Layer == 0)
				SIL_SMD_CIRCLEObject.Layer = PASTE_MASK_BOTTOM_LAYER;

			SIL_SMD_CIRCLEObject.x2 = (float) NewGeomCreate.Paste;
			SIL_SMD_CIRCLEObject.PinNr = -1;
			AddObject2(&SIL_SMD_CIRCLEObject);
		}

// *******************************************************************************
// Add solder mask
// *******************************************************************************

		if (NewGeomCreate.Mask > 0.0)
		{
			SIL_SMD_CIRCLEObject.Layer = SOLD_MASK_TOP_LAYER;

			if (NewGeomCreate.Layer == 0)
				SIL_SMD_CIRCLEObject.Layer = SOLD_MASK_BOTTOM_LAYER;

			SIL_SMD_CIRCLEObject.x2 = (float) NewGeomCreate.Mask;
			SIL_SMD_CIRCLEObject.PinNr = -1;
			AddObject2(&SIL_SMD_CIRCLEObject);
		}

// *******************************************************************************
// *******************************************************************************

		PinCount += NewGeomCreate.PinInc;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreatePGA()
{

// Check parameters
	int32 cntx, cnty, let2, PinStrPos, cnt;
	double PinPosX, PinPosY, OffsetX, OffsetY;
	char PinStr[12];
	PinInfoRecord NewPinInfo;
	ObjectRecord PGAObject;

	if ((NewGeomCreate.NrPinsX <= 0) && (NewGeomCreate.NrPinsX > 100))
	{
		MessageBoxUTF8(GEOMWindow, SC(254, "Nr pins x direction <= 0 or >100"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if ((NewGeomCreate.NrPinsY <= 0) && (NewGeomCreate.NrPinsY > 100))
	{
		MessageBoxUTF8(GEOMWindow, SC(255, "Nr pins y direction <= 0 or >100"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(256, "Pitch < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pad < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(257, "Pad < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < NewGeomCreate.Pad)
	{
		MessageBoxUTF8(GEOMWindow, SC(264, "Pitch x direction < pad size"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Drill > NewGeomCreate.Pad)
		MessageBoxUTF8(GEOMWindow, SC(266, "Pad size < drill size"), SC(49, "Warning"), MB_OK);

	NrObjects = 0;
	NrPinObjects = 0;
	NrObjectPolygons = 0;
	InitNewShape("noname", 1);

	OffsetX = -((NewGeomCreate.NrPinsX - 1) * NewGeomCreate.Pitch) * 0.5;
	OffsetY = ((NewGeomCreate.NrPinsY - 1) * NewGeomCreate.Pitch) * 0.5;

	for (cntx = 0; cntx < NewGeomCreate.NrPinsX; cntx++)
	{
		let2 = (cntx / 20);

		for (cnty = 0; cnty < NewGeomCreate.NrPinsY; cnty++)
		{

// *******************************************************************************
// Add pad solder side
// *******************************************************************************

			memset(&PGAObject, 0, sizeof(ObjectRecord));
			PGAObject.ObjectType = OBJECT_CIRCLE;
			PinPosX = NewGeomCreate.Pitch * (cntx);
			PinPosY = NewGeomCreate.Pitch * (cnty);
			PGAObject.Layer = 0;
			PGAObject.x1 = (float) (PinPosY + OffsetX);
			PGAObject.y1 = (float) (-PinPosX + OffsetY);
			PGAObject.x2 = (float) NewGeomCreate.Pad;
			PGAObject.y2 = 0.0;

			if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
			{
				PGAObject.ObjectType = OBJECT_RECT;
				PGAObject.y2 = (float) NewGeomCreate.Pad;
			}

			PGAObject.PinNr = NrPinObjects;
			PGAObject.Clearance = (float) NewGeomCreate.Clearance;
			AddObject(&PGAObject);

// *******************************************************************************
// Add pad component side
// *******************************************************************************

			PGAObject.ObjectType = OBJECT_CIRCLE;
			PGAObject.Layer = NrPadLayers - 1;
			PGAObject.x2 = (float) NewGeomCreate.Pad;
			PGAObject.y2 = 0.0;

			if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
			{
				PGAObject.ObjectType = OBJECT_RECT;
				PGAObject.y2 = (float) NewGeomCreate.Pad;
			}

			AddObject(&PGAObject);


			// *******************************************************************************
			// Add drill
			// *******************************************************************************

			PGAObject.ObjectType = OBJECT_CIRCLE;
			PGAObject.Layer = DRILL_LAYER;
			PGAObject.x2 = (float) NewGeomCreate.Drill;
			PGAObject.y2 = 0.0;
			PGAObject.Clearance = 0.0;
			AddObject(&PGAObject);

			// *******************************************************************************
			// Add anti power pad
			// *******************************************************************************

			if (NewGeomCreate.PowerPad > 0.0)
			{
				PGAObject.Layer = POWER_PAD_LAYER;
				PGAObject.x2 = (float) NewGeomCreate.PowerPad;
				PGAObject.y2 = 0.0;
				AddObject(&PGAObject);
			}

			// *******************************************************************************
			// Add inner pad
			// *******************************************************************************

			if (NrPadLayers > 2)
			{
				for (cnt = 1; cnt < NrPadLayers - 1; cnt++)
				{
					PGAObject.Layer = cnt;
					PGAObject.x2 = (float) NewGeomCreate.InnerPad;
					PGAObject.y2 = 0.0;
					AddObject(&PGAObject);
				}
			}
			else
			{
				PGAObject.Layer = INNER_PAD_LAYER;
				PGAObject.x2 = NewGeomCreate.InnerPad;
				PGAObject.y2 = (float) 0.0;
				AddObject(&PGAObject);
			}

			// *******************************************************************************
			// Add solder mask
			// *******************************************************************************

			if (NewGeomCreate.Mask > 0.0)
			{
				PGAObject.ObjectType = OBJECT_CIRCLE;
				PGAObject.Layer = SOLD_MASK_TOP_LAYER;
				PGAObject.x2 = (float) NewGeomCreate.Mask;
				PGAObject.y2 = 0.0;

				if ((NrPinObjects == 0) && (NewGeomCreate.Pin1Square))
				{
					PGAObject.ObjectType = OBJECT_RECT;
					PGAObject.y2 = (float) NewGeomCreate.Mask;
				}

				PGAObject.PinNr = -1;
				AddObject(&PGAObject);

				PGAObject.Layer = SOLD_MASK_BOTTOM_LAYER;
				AddObject(&PGAObject);
			}

			// *******************************************************************************
			// *******************************************************************************

			memset(&PinStr, 0, 12);
			PinStr[0] = 0;
			PinStrPos = 0;

			if (let2 > 0)
			{
				PinStr[0] = (char) (64 + let2);
				PinStrPos = 1;
			}

			PinStr[PinStrPos] = PinLetters[cntx % 20];
			PinStrPos++;

			if (cnty > 8)
			{
				PinStr[PinStrPos] = (char) (48 + (cnty + 1) / 10);
				PinStrPos++;
			}

			PinStr[PinStrPos] = (char) (48 + ((cnty + 1) % 10));
			PinStrPos++;

			memmove(&NewPinInfo.PinText, &PinStr, 12);

			AddPinInfo(&NewPinInfo);
		}

// *******************************************************************************
// *******************************************************************************

	}

	ViewFull();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateQUAD()
{
	int32 cnt2, row, PinCount, PinCount2, MaxPinCount;
	double PinPosX, PinPosY, OffX, OffY;
	PinInfoRecord NewPinInfo;
	ObjectRecord QUADObject;

	PinCount = 0;
	PinPosX = 0.0;
	PinPosY = 0.0;

	if ((NewGeomCreate.NrPinsX <= 0) && (NewGeomCreate.NrPinsX > 1000))
	{
		MessageBoxUTF8(GEOMWindow, SC(272, "Nr pins x direction <= 0 or >1000"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if ((NewGeomCreate.NrPinsY <= 0) && (NewGeomCreate.NrPinsY > 1000))
	{
		MessageBoxUTF8(GEOMWindow, SC(261, "Nr pins y direction <= 0 or >1000"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(256, "Pitch < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PadX < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(257, "Pad < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PadY < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(257, "Pad < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Clearance < 100.0)
	{
		MessageBoxUTF8(GEOMWindow, SC(258, "Clearance < 0.001 mm"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.Pitch < NewGeomCreate.PadY)
	{
		MessageBoxUTF8(GEOMWindow, SC(273, "Pitch < pad x"), SC(48, "Error"), MB_OK);
		return -1;
	}

	if (NewGeomCreate.PasteX > NewGeomCreate.PadX)
		MessageBoxUTF8(GEOMWindow, SC(270, "Pad x solder paste > pad x"), SC(49, "Warning"), MB_OK);

	if (NewGeomCreate.PasteY > NewGeomCreate.PadY)
		MessageBoxUTF8(GEOMWindow, SC(271, "Pad y solder paste > pad y"), SC(49, "Warning"), MB_OK);

	OffX = ((NewGeomCreate.DistX - ((NewGeomCreate.NrPinsX - 1) * NewGeomCreate.Pitch)) / 2);
	OffY = ((NewGeomCreate.DistY - ((NewGeomCreate.NrPinsY - 1) * NewGeomCreate.Pitch)) / 2);
	NrObjects = 0;
	NrPinObjects = 0;
	NrObjectPolygons = 0;
	MaxPinCount = NewGeomCreate.NrPinsY * 2 + NewGeomCreate.NrPinsX * 2;
	PinCount2 = NewGeomCreate.Start2;
	InitNewShape("noname", 1);

	for (row = 0; row < 4; row++)
	{
		switch (row)
		{
		case 0:
		case 2:
			PinCount = NewGeomCreate.NrPinsY;
			break;

		case 1:
		case 3:
			PinCount = NewGeomCreate.NrPinsX;
			break;
		}

		for (cnt2 = 0; cnt2 < PinCount; cnt2++)
		{

			// *******************************************************************************
			// Add pad
			// *******************************************************************************

			memset(&QUADObject, 0, sizeof(ObjectRecord));
			QUADObject.ObjectType = OBJECT_RECT;

			switch (row)
			{
			case 0:
				PinPosX = 0.0;
				PinPosY = NewGeomCreate.Pitch * (-cnt2);
				QUADObject.x2 = (float) NewGeomCreate.PadX;
				QUADObject.y2 = (float) NewGeomCreate.PadY;
				break;

			case 1:
				PinPosX = OffX + NewGeomCreate.Pitch * (cnt2);
				PinPosY = -((NewGeomCreate.NrPinsY - 1) * NewGeomCreate.Pitch + OffY);
				QUADObject.x2 = (float) NewGeomCreate.PadY;
				QUADObject.y2 = (float) NewGeomCreate.PadX;
				break;

			case 2:
				PinPosX = NewGeomCreate.DistX;
				PinPosY = NewGeomCreate.Pitch * (-(NewGeomCreate.NrPinsY - cnt2 - 1));
				QUADObject.x2 = (float) NewGeomCreate.PadX;
				QUADObject.y2 = (float) NewGeomCreate.PadY;
				break;

			case 3:
				PinPosX = OffX + NewGeomCreate.Pitch * (NewGeomCreate.NrPinsX - cnt2 - 1);
				PinPosY = OffY;
				QUADObject.x2 = (float) NewGeomCreate.PadY;
				QUADObject.y2 = (float) NewGeomCreate.PadX;
				break;
			}

			PinPosX -= NewGeomCreate.DistX * 0.5;
//      PinPosX-=NewGeomCreate.Pitch*(NewGeomCreate.NrPinsX-1)*0.5;
			PinPosY += NewGeomCreate.Pitch * ((NewGeomCreate.NrPinsY - 1) * 0.5);
			QUADObject.Layer = NrPadLayers - 1;
			QUADObject.x1 = (float) PinPosX;
			QUADObject.y1 = (float) PinPosY;
			QUADObject.PinNr = NrPinObjects;
			QUADObject.Clearance = (float) NewGeomCreate.Clearance;
			AddObject(&QUADObject);

// *******************************************************************************
// Add paste mask
// *******************************************************************************

			QUADObject.Clearance = 0.0;

			if ((NewGeomCreate.PasteX > 0.0) && (NewGeomCreate.PasteY > 0.0))
			{
				QUADObject.Layer = PASTE_MASK_TOP_LAYER;

				switch (row)
				{
				case 0:
				case 2:
					QUADObject.x2 = (float) NewGeomCreate.PasteX;
					QUADObject.y2 = (float) NewGeomCreate.PasteY;
					break;

				case 1:
				case 3:
					QUADObject.x2 = (float) NewGeomCreate.PasteY;
					QUADObject.y2 = (float) NewGeomCreate.PasteX;
					break;
				}

				QUADObject.PinNr = -1;
				AddObject(&QUADObject);
			}

// *******************************************************************************
// Add solder mask
// *******************************************************************************

			if ((NewGeomCreate.MaskX > 0.0) && (NewGeomCreate.MaskY > 0.0))
			{
				QUADObject.Layer = SOLD_MASK_TOP_LAYER;

				switch (row)
				{
				case 0:
				case 2:
					QUADObject.x2 = (float) NewGeomCreate.MaskX;
					QUADObject.y2 = (float) NewGeomCreate.MaskY;
					break;

				case 1:
				case 3:
					QUADObject.x2 = (float) NewGeomCreate.MaskY;
					QUADObject.y2 = (float) NewGeomCreate.MaskX;
					break;
				}

				QUADObject.PinNr = -1;
				AddObject(&QUADObject);
			}


// *******************************************************************************
// *******************************************************************************

			memset(&NewPinInfo, 0, sizeof(PinInfoRecord));

			if (NewGeomCreate.Start1[0] != 0)
			{
				if (MaxPinCount > 99)
					NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 3] = 0;
				else
				{
					if (MaxPinCount > 9)
						NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 2] = 0;
					else
						NewGeomCreate.Start1[sizeof(NewPinInfo.PinText) - 1] = 0;
				}

				sprintf(NewPinInfo.PinText, "%s%i", NewGeomCreate.Start1, PinCount2);
			}
			else
				sprintf(NewPinInfo.PinText, "%i", PinCount2);

			AddPinInfo(&NewPinInfo);
			PinCount2++;
		}

// *******************************************************************************
// *******************************************************************************

	}

	ViewFull();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InsertDialogObjects(int32 DialogMode)
{
	switch (DialogMode)
	{
	case IDD_DIALOG_BGA:
		memmove(&NewGeomCreate, &BGAGeom, sizeof(GeomCreateRecord));

		if (CreateBGA() != 0)
			return -1;

		break;

	case IDD_DIALOG_DIL:
		memmove(&NewGeomCreate, &DILGeom, sizeof(GeomCreateRecord));

		if (CreateDIL() != 0)
			return -1;

		break;

	case IDD_DIALOG_PGA:
		memmove(&NewGeomCreate, &PGAGeom, sizeof(GeomCreateRecord));

		if (CreatePGA() != 0)
			return -1;

		break;

	case IDD_DIALOG_QUAD:
		memmove(&NewGeomCreate, &QUADGeom, sizeof(GeomCreateRecord));

		if (CreateQUAD() != 0)
			return -1;

		break;

	case IDD_DIALOG_SIL:
	case IDD_DIALOG_SIL2:
		memmove(&NewGeomCreate, &SILGeom, sizeof(GeomCreateRecord));

		if (CreateSIL() != 0)
			return -1;

		break;

	case IDD_DIALOG_SIL_SMD_RECT:
		memmove(&NewGeomCreate, &SIL_SMD_RECTGeom, sizeof(GeomCreateRecord));

		if (CreateSIL_SMD_RECT() != 0)
			return -1;

		break;

	case IDD_DIALOG_SIL_SMD_CIRCLE:
		memmove(&NewGeomCreate, &SIL_SMD_CIRCLEGeom, sizeof(GeomCreateRecord));

		if (CreateSIL_SMD_CIRCLE() != 0)
			return -1;

		break;

	case IDD_DIALOG_SOIC:
		memmove(&NewGeomCreate, &SOICGeom, sizeof(GeomCreateRecord));

		if (CreateSOIC() != 0)
			return -1;

		break;
	}

	if (Units == 1)
		PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MM, (LPARAM) NULL);
	else
		PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_SETTINGS_UNITS_MILS, (LPARAM) NULL);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void InsertObjects2(double OffsetX, double OffsetY)
{
	int32 cnt;
	ObjectRecord *Object, NewObject;
	ObjectPolygonRecord *ObjectPolygon;

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);

		if (Object->ObjectType != OBJECT_POLYGON)
		{
			memmove(&NewObject, Object, sizeof(ObjectRecord));
			NewObject.x1 += (float) OffsetX;
			NewObject.y1 += (float) OffsetY;

			if (NewObject.ObjectType == OBJECT_LINE)
			{
				NewObject.x2 += (float) OffsetX;
				NewObject.y2 += (float) OffsetY;
			}

			AddObject(&NewObject);
		}
		else
		{
			ObjectPolygon = (ObjectPolygonRecord *) Object->Address;
			AddObjectPolygon(ObjectPolygon);
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[NrObjectPolygons - 1]]);
			MoveObjectPolygon(ObjectPolygon, OffsetX, OffsetY, 0);
		}
	}

	SetScrollPageSize();
	SetScrollPosition();

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void AddSILOnObjectsSelected(int32 mode)
{
	int32 cnt, Layer;
	int32 Found = 0;
	ObjectRecord *Object;

	if (mode == 0)
	{
		SILGeom.InsertPowerPad = 0;
		SILGeom.InsertInnerPad = 0;
		SILGeom.PinInc = 1;
		SILGeom.NrPins = 1;
		SILGeom.Pitch = (100 * 2540);
		SILGeom.Start2 = 1;

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				switch (Object->ObjectType)
				{
				case OBJECT_CIRCLE:
					Layer = Object->Layer;

					switch (Layer)
					{
					case SOLD_MASK_BOTTOM_LAYER:
					case SOLD_MASK_TOP_LAYER:
						SILGeom.Mask = Object->x2;
						break;

					case DRILL_LAYER:
						SILGeom.Drill = Object->x2;
						break;

					case POWER_PAD_LAYER:
						SILGeom.InsertPowerPad = 1;
						SILGeom.PowerPad = Object->x2;
						break;

					case INNER_PAD_LAYER:
						SILGeom.InsertInnerPad = 1;
						SILGeom.InnerPad = Object->x2;
						break;

					default:
						if ((Layer == 0) || (Layer == NrPadLayers - 1))
						{
							SILGeom.Pad = Object->x2;
							Found = 1;
						}

						if (CheckIfInnerLayer(Layer))
						{
							SILGeom.InsertInnerPad = 1;
							SILGeom.InnerPad = Object->x2;
						}

						break;
					}

					break;
				}
			}
		}

		if (Found)
			PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SIL, (LPARAM) NULL);

	}
	else
	{
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (Object->ObjectType == OBJECT_RECT)
					mode = 2;
			}
		}

		if (mode == 2)
		{
			SIL_SMD_RECTGeom.PinInc = 1;
			SIL_SMD_RECTGeom.NrPins = 1;
			SIL_SMD_RECTGeom.Pitch = (100 * 2540);
			SIL_SMD_RECTGeom.Start2 = 1;

			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					switch (Object->ObjectType)
					{
					case OBJECT_RECT:
						Layer = Object->Layer;

						switch (Layer)
						{
						case SOLD_MASK_BOTTOM_LAYER:
						case SOLD_MASK_TOP_LAYER:
							SIL_SMD_RECTGeom.MaskX = Object->x2;
							SIL_SMD_RECTGeom.MaskY = Object->y2;
							break;

						case PASTE_MASK_BOTTOM_LAYER:
						case PASTE_MASK_TOP_LAYER:
							SIL_SMD_RECTGeom.PasteX = Object->x2;
							SIL_SMD_RECTGeom.PasteY = Object->y2;
							break;

						default:
							if (Layer == 0)
							{
								SIL_SMD_RECTGeom.PadX = Object->x2;
								SIL_SMD_RECTGeom.PadY = Object->y2;
								SIL_SMD_RECTGeom.Layer = 0;
								Found = 1;
							}

							if (Layer == NrPadLayers - 1)
							{
								SIL_SMD_RECTGeom.PadX = Object->x2;
								SIL_SMD_RECTGeom.PadY = Object->y2;
								SIL_SMD_RECTGeom.Layer = NrPadLayers - 1;
								Found = 1;
							}

							break;
						}

						break;
					}
				}
			}

			if (Found)
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SIL_SMD_RECT, (LPARAM) NULL);
		}
		else
		{
			SIL_SMD_CIRCLEGeom.PinInc = 1;
			SIL_SMD_CIRCLEGeom.NrPins = 1;
			SIL_SMD_CIRCLEGeom.Pitch = (100 * 2540);
			SIL_SMD_CIRCLEGeom.Start2 = 1;

			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					switch (Object->ObjectType)
					{
					case OBJECT_CIRCLE:
						Layer = Object->Layer;

						switch (Layer)
						{
						case SOLD_MASK_BOTTOM_LAYER:
						case SOLD_MASK_TOP_LAYER:
							SIL_SMD_CIRCLEGeom.Mask = Object->x2;
							break;

						case PASTE_MASK_BOTTOM_LAYER:
						case PASTE_MASK_TOP_LAYER:
							SIL_SMD_CIRCLEGeom.Paste = Object->x2;
							break;

						default:
							if (Layer == 0)
							{
								SIL_SMD_CIRCLEGeom.Pad = Object->x2;
								SIL_SMD_CIRCLEGeom.Layer = 0;
								Found = 1;
							}

							if (Layer == NrPadLayers - 1)
							{
								SIL_SMD_CIRCLEGeom.Pad = Object->x2;
								SIL_SMD_CIRCLEGeom.Layer = NrPadLayers - 1;
								Found = 1;
							}

							break;
						}

						break;
					}
				}
			}

			if (Found)
				PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_CREATE_SIL_SMD_CIRCLE, (LPARAM) NULL);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
