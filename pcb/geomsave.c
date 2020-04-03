/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: geomsave.c
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
#include "stddef.h"
#include "memory.h"
#include "graphics.h"
#include "string.h"
#include "select4.h"
#include "select2.h"
#include "trace2.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "calc.h"
#include "files2.h"
#include "gerber.h"
#include "gerber2.h"
#include "gerber3.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "select3.h"
#include "polygon.h"
#include "mainloop.h"
#include "toets.h"
#include "rect.h"
#include "math.h"
#include "dialogs.h"
#include "time.h"
#include "plot.h"
#include "owntime.h"
#include "resource.h"
#include "dialogs.h"

typedef struct
{
	int32 PinType, StartNr, NrObjects;
	char PinText[12];
} PinInfoRecord;


ShapePadRecord ShapePad;
PadRecord Pad;
//Pad2Record      Pad2;

ShapeRecord Shape;
ShapeLinesArray ShapeLines;

typedef int32 GeomPolygonsArray[DefMaxNrObjectPolygons];

GeomPolygonsArray *GeomPolygons;
uint8 *GeomPolygonsMem;
int32 MaxGeomPolygons;
int32 NrGeomPolygons, MaxGeomPolygonsMem;
int32 GeomPolygonsMemSize, NrPins;
int32 NrPinShapesSaved, NewBufPos, BufPos;
float GeomSaveClearance;
int32 GeomSaveOptions = 8 + 4 + 1;
int32 SpecialPolygon;
char StartPinText[10];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteToShapeBuf(uint8 * MemToAdd, int32 BufPos, int32 MemSize)
{
	if (BufPos + MemSize + 64 * 1024 > MaxTempMemory)
	{
		if (AllocateMemTemp(BufPos + MemSize + 64 * 1024) != 0)
			return -1;
	}

	memcpy(&TempMem[BufPos], MemToAdd, MemSize);
	return BufPos + MemSize;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

GeomPolygonRecord *AddAreaFillAsGeomPolygon(AreaFillRecord * AreaFill, int32 * BufPos)
{
	int32 MemSize, count, NewMemSize, PointsMem, cnt2;
	double OffsetX, OffsetY;
	GeomPolygonRecord *NewGeomPolygon;
	GeomSubPolygonRecord *GeomSubPolygon;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos, *NewPointsPos, *PointsPos, *GeomSubPolygonPos;
#ifdef _DEBUG
	int32 ok;
#endif

	if (NrGeomPolygons >= MaxGeomPolygons)
	{
		if (AllocateMemTemp3((MaxGeomPolygons + 4096) * sizeof(int32)) != 0)
			return NULL;

		MaxGeomPolygons = MaxTempMemory3 / sizeof(int32);
		GeomPolygons = (GeomPolygonsArray *) TempMem3;
	}

	DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

	if ((AreaFill->NrPolygons > 1) || (DrawPolygon->NrVertices > 500))
		SpecialPolygon = 1;

	MemSize = AreaFill->MemSize + 16384;

	if (MaxGeomPolygonsMem + MemSize > MaxTempMemory2)
	{
		if (AllocateMemTemp2(MaxGeomPolygonsMem + MemSize) != 0)
			return NULL;

		MaxGeomPolygonsMem = MaxTempMemory2;
		GeomPolygonsMem = TempMem2;
	}

	OffsetX = -(*DrawPolygon).Points[0].x;
	OffsetY = -(*DrawPolygon).Points[0].y;
	NewGeomPolygon = (GeomPolygonRecord *) & GeomPolygonsMem[GeomPolygonsMemSize];
	memset(NewGeomPolygon, 0, sizeof(GeomPolygonInitRecord));
	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	count = DrawPolygon->NrVertices;
	NewGeomPolygon->NrVertices = count;
	NewGeomPolygon->minx = AreaFill->minx + OffsetX;
	NewGeomPolygon->miny = AreaFill->miny + OffsetY;
	NewGeomPolygon->maxx = AreaFill->maxx + OffsetX;
	NewGeomPolygon->maxy = AreaFill->maxy + OffsetY;
	PointsPos = (uint8 *) DrawPolygon + sizeof(PolygonInitRecord);
	NewPointsPos = (uint8 *) NewGeomPolygon + sizeof(GeomPolygonInitRecord);

	PointsMem = count * sizeof(PointRecord);
	GeomSubPolygonPos = NewPointsPos + PointsMem;
	memcpy(NewPointsPos, PointsPos, PointsMem);
	AdjustOffsetOnPoints((double *) NewPointsPos, OffsetX, OffsetY, count, 0);
	PolygonPos = (uint8 *) DrawPolygon;
#ifdef _DEBUG

	if (AreaFill->NrPolygons == 280)
		ok = 1;

#endif

	for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		if (cnt2 == 1)
			NewGeomPolygon->NrVerticesMainPolygon = NewGeomPolygon->NrVertices;

		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
		NewGeomPolygon->NrSubPolygons++;
		count = DrawPolygon->NrVertices;
		GeomSubPolygon = (GeomSubPolygonRecord *) GeomSubPolygonPos;
		memset(GeomSubPolygon, 0, sizeof(GeomSubPolygonInitRecord));
		GeomSubPolygon->NrVertices = count;
		GeomSubPolygon->Magic = SUB_POLYGON_MAGIC;
		GeomSubPolygon->minx = DrawPolygon->minx + OffsetX;
		GeomSubPolygon->miny = DrawPolygon->miny + OffsetY;
		GeomSubPolygon->maxx = DrawPolygon->maxx + OffsetX;
		GeomSubPolygon->maxy = DrawPolygon->maxy + OffsetY;
		PointsPos = (uint8 *) DrawPolygon + sizeof(PolygonInitRecord);
		NewPointsPos = (uint8 *) GeomSubPolygon + sizeof(GeomSubPolygonInitRecord);
		PointsMem = count * sizeof(PointRecord);
		memcpy(NewPointsPos, PointsPos, PointsMem);
		AdjustOffsetOnPoints((double *) NewPointsPos, OffsetX, OffsetY, count, 0);
		GeomSubPolygonPos = NewPointsPos + PointsMem;
		NewGeomPolygon->NrVertices += (PointsMem + sizeof(GeomSubPolygonInitRecord)) / sizeof(PointRecord);
	}

	NewMemSize = GeomSubPolygonPos - (uint8 *) NewGeomPolygon;
	(*GeomPolygons)[NrGeomPolygons] = GeomPolygonsMemSize;
	NewGeomPolygon->PolygonNr = NrGeomPolygons;
	NrGeomPolygons++;
	GeomPolygonsMemSize += NewMemSize;
	return NewGeomPolygon;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

GeomPolygonRecord *AddObjectPolygonAsGeomPolygon(ObjectPolygonRecord * ObjectPolygon, double *OffsetX, double *OffsetY)
{
	int32 MemSize, count, NewMemSize;
	GeomPolygonRecord *NewGeomPolygon;
	uint8 *NewPointsPos, *PointsPos;

	if (NrGeomPolygons >= MaxGeomPolygons)
	{
		if (AllocateMemTemp3((MaxGeomPolygons + 4096) * sizeof(int32)) != 0)
			return NULL;

		MaxGeomPolygons = MaxTempMemory3 / sizeof(int32);
		GeomPolygons = (GeomPolygonsArray *) TempMem3;
	}

	count = ObjectPolygon->NrVertices;

	if (count > 500)
		SpecialPolygon = 1;

	MemSize = sizeof(ObjectPolygonInitRecord) + count * sizeof(PointRecord) + 16384;

	if (MaxGeomPolygonsMem + MemSize > MaxTempMemory2)
	{
		if (AllocateMemTemp2(MaxGeomPolygonsMem + MemSize) != 0)
			return NULL;

		MaxGeomPolygonsMem = MaxTempMemory2;
		GeomPolygonsMem = TempMem2;
	}

	if (ObjectPolygon->NrVertices < 40)
	{
		*OffsetX = -(ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5;
		*OffsetY = -(ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5;
	}
	else
	{
		*OffsetX = -ObjectPolygon->Points[0].x;
		*OffsetY = -ObjectPolygon->Points[0].y;
	}

	NewGeomPolygon = (GeomPolygonRecord *) & GeomPolygonsMem[GeomPolygonsMemSize];
	memset(NewGeomPolygon, 0, sizeof(GeomPolygonInitRecord));
	NewGeomPolygon->NrVertices = count;
	NewGeomPolygon->minx = ObjectPolygon->minx + *OffsetX;
	NewGeomPolygon->miny = ObjectPolygon->miny + *OffsetY;
	NewGeomPolygon->maxx = ObjectPolygon->maxx + *OffsetX;
	NewGeomPolygon->maxy = ObjectPolygon->maxy + *OffsetY;
	NewMemSize = count * sizeof(PointRecord) + sizeof(GeomPolygonInitRecord);
	PointsPos = (uint8 *) ObjectPolygon + sizeof(ObjectPolygonInitRecord);
	NewPointsPos = (uint8 *) NewGeomPolygon + sizeof(GeomPolygonInitRecord);
	memcpy(NewPointsPos, PointsPos, count * sizeof(PointRecord));
	AdjustOffsetOnPoints((double *) NewPointsPos, *OffsetX, *OffsetY, count, 0);

	(*GeomPolygons)[NrGeomPolygons] = GeomPolygonsMemSize;
	NewGeomPolygon->PolygonNr = NrGeomPolygons;
	NrGeomPolygons++;
	GeomPolygonsMemSize += NewMemSize;
	return NewGeomPolygon;
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

GeomPolygonRecord *AddGeomPolygonAsGeomPolygon(GeomPolygonRecord * GeomPolygon, double x1, double y1, double Rotation,
        int32 Mirror, double *OffsetX, double *OffsetY)
{
	int32 MemSize, count, cnt, cnt2, NewMemSize, Rotation2;
	double x5, y5, TempValue, minx, miny, maxx, maxy;
	GeomPolygonRecord *NewGeomPolygon;
	GeomSubPolygonRecord *GeomSubPolygon;
	uint8 *GeomSubPolygonPos;

	Rotation2 = GetRotationFromFloat(Rotation);

	if (NrGeomPolygons >= MaxGeomPolygons)
	{
		if (AllocateMemTemp3((MaxGeomPolygons + 4096) * sizeof(int32)) != 0)
			return NULL;

		MaxGeomPolygons = MaxTempMemory3 / sizeof(int32);
		GeomPolygons = (GeomPolygonsArray *) TempMem3;
	}

	count = GeomPolygon->NrVertices;
	MemSize = sizeof(GeomPolygonInitRecord) + count * sizeof(PointRecord) + 16384;

	if (MaxGeomPolygonsMem + MemSize > MaxTempMemory2)
	{
		if (AllocateMemTemp2(MaxGeomPolygonsMem + MemSize) != 0)
			return NULL;

		MaxGeomPolygonsMem = MaxTempMemory2;
		GeomPolygonsMem = TempMem2;
	}

	NewGeomPolygon = (GeomPolygonRecord *) & GeomPolygonsMem[GeomPolygonsMemSize];

	NewMemSize = count * sizeof(PointRecord) + sizeof(GeomPolygonInitRecord);
	memcpy((uint8 *) NewGeomPolygon, (uint8 *) GeomPolygon, NewMemSize);

	minx = 1000000000.0;
	miny = 1000000000.0;
	maxx = -1000000000.0;
	maxy = -1000000000.0;

	if ((GeomPolygon->NrSubPolygons > 0) || (GeomPolygon->NrVertices > 500))
		SpecialPolygon = 1;

	if (NewGeomPolygon->NrSubPolygons > 0)
		count = NewGeomPolygon->NrVerticesMainPolygon;

	for (cnt = 0; cnt < count; cnt++)
	{
		x5 = NewGeomPolygon->Points[cnt].x;
		y5 = NewGeomPolygon->Points[cnt].y;

		if (Mirror == 1)
			x5 = -x5;

		switch (Rotation2)
		{
		case 0:
			break;

		case 1:
			TempValue = x5;
			x5 = -y5;
			y5 = TempValue;
			break;

		case 2:
			x5 = -x5;
			y5 = -y5;
			break;

		case 3:
			TempValue = x5;
			x5 = y5;
			y5 = -TempValue;
			break;

		default:
			RotatePointFromOtherPoint2(&x5, &y5, 0.0, 0.0, Rotation);
			break;
		}

		NewGeomPolygon->Points[cnt].x = x5 + x1;
		NewGeomPolygon->Points[cnt].y = y5 + y1;
		minx = min(minx, x5 + x1);
		miny = min(miny, y5 + y1);
		maxx = max(maxx, x5 + x1);
		maxy = max(maxy, y5 + y1);
	}

	if ((GeomPolygon->NrSubPolygons == 0) && (GeomPolygon->NrVertices < 40))
	{
		*OffsetX = -(minx + maxx) * 0.5;
		*OffsetY = -(miny + maxy) * 0.5;
	}
	else
	{
		*OffsetX = -NewGeomPolygon->Points[0].x;
		*OffsetY = -NewGeomPolygon->Points[0].y;
	}

	NewGeomPolygon->minx = minx + *OffsetX;
	NewGeomPolygon->miny = miny + *OffsetY;
	NewGeomPolygon->maxx = maxx + *OffsetX;
	NewGeomPolygon->maxy = maxy + *OffsetY;
	AdjustOffsetOnPoints((double *) &NewGeomPolygon->Points, *OffsetX, *OffsetY, count, 0);
	GeomSubPolygonPos = (uint8 *) NewGeomPolygon;
	GeomSubPolygonPos += sizeof(GeomPolygonInitRecord);
	GeomSubPolygonPos += count * sizeof(PointRecord);

	for (cnt2 = 0; cnt2 < NewGeomPolygon->NrSubPolygons; cnt2++)
	{
		GeomSubPolygon = (GeomSubPolygonRecord *) GeomSubPolygonPos;
		count = GeomSubPolygon->NrVertices;

		for (cnt = 0; cnt < count; cnt++)
		{
			x5 = GeomSubPolygon->Points[cnt].x;
			y5 = GeomSubPolygon->Points[cnt].y;

			if (Mirror == 1)
				x5 = -x5;

			switch (Rotation2)
			{
			case 0:
				break;

			case 1:
				TempValue = x5;
				x5 = -y5;
				y5 = TempValue;
				break;

			case 2:
				x5 = -x5;
				y5 = -y5;
				break;

			case 3:
				TempValue = x5;
				x5 = y5;
				y5 = -TempValue;
				break;

			default:
				RotatePointFromOtherPoint2(&x5, &y5, 0.0, 0.0, Rotation);
				break;
			}

			GeomSubPolygon->Points[cnt].x = x5 + x1 + *OffsetX;
			GeomSubPolygon->Points[cnt].y = y5 + y1 + *OffsetY;
		}

		GeomSubPolygonPos += sizeof(GeomSubPolygonInitRecord);
		GeomSubPolygonPos += count * sizeof(PointRecord);
	}

	(*GeomPolygons)[NrGeomPolygons] = GeomPolygonsMemSize;
	NewGeomPolygon->PolygonNr = NrGeomPolygons;
	NrGeomPolygons++;
	GeomPolygonsMemSize += NewMemSize;
	return NewGeomPolygon;
}




// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SavePinObject(ObjectRecord * Object)
{
	int32 ObjectType, ObjectSize, ok;
	double x2, y2, OffsetX, OffsetY;
	GeomPolygonRecord *GeomPolygon, *NewGeomPolygon, SpecialGeomPolygon;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon;


#ifdef _DEBUG

	if (NrPins == 231)
		ok = 1;

	if ((InRange9(Object->x1, 39.3e5)) && (InRange9(Object->y1, 54.6e5)))
		ok = 1;

#endif
	ObjectType = Object->ObjectType;

	switch (ObjectType)
	{
	case AREAFILL:
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object->TraceNr]]);
		DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
		memset(&Pad, 0, sizeof(PadRecord));
		Pad.X = (float) ((*DrawPolygon).Points[0].x);
		Pad.Y = (float) ((*DrawPolygon).Points[0].y);
		Pad.ShapeType = PIN_SMD_POLYGON;
		Pad.Layer = Object->Layer;
		Pad.Clearance = max(AreaFill->Clearance, GeomSaveClearance);
		// Fill in at which address the polygon pointer must be stored
		NewGeomPolygon = AddAreaFillAsGeomPolygon(AreaFill, 0);
		NewGeomPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special.AddressOffset);
		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case PIN_SMD_POLYGON:
		GeomPolygon = (GeomPolygonRecord *) Object->Address;
		memset(&Pad, 0, sizeof(PadRecord));
		Pad.ShapeType = PIN_SMD_POLYGON;

		if (Object->ObjectType2 == 0)
		{
#ifdef _DEBUG

			if (!GeomPolygon)
				ok = 1;

#endif
			NewGeomPolygon =
			    AddGeomPolygonAsGeomPolygon(GeomPolygon, Object->x1, Object->y1, Object->RotationAngle, Object->Mirror,
			                                &OffsetX, &OffsetY);
		}
		else
		{
#ifdef _DEBUG

			if (NrPins == 231)
				ok = 1;

			if ((InRange9(Object->x1, 360.7e5)) && (InRange9(Object->y1, 259.6e5)))
				ok = 1;

#endif
			memset(&SpecialGeomPolygon, 0, sizeof(SpecialGeomPolygon));
			SpecialGeomPolygon.NrVertices = 0;

			switch (Object->ObjectType2)
			{
			case OBJECT_RECT:
			case PIN_SMD_RECT:
				x2 = Object->x2;
				y2 = Object->y2;
				SpecialGeomPolygon.NrVertices = 4;
				SpecialGeomPolygon.Points[0].x = -x2 * 0.5;
				SpecialGeomPolygon.Points[0].y = -y2 * 0.5;
				SpecialGeomPolygon.Points[1].x = x2 * 0.5;
				SpecialGeomPolygon.Points[1].y = -y2 * 0.5;
				SpecialGeomPolygon.Points[2].x = x2 * 0.5;
				SpecialGeomPolygon.Points[2].y = y2 * 0.5;
				SpecialGeomPolygon.Points[3].x = -x2 * 0.5;
				SpecialGeomPolygon.Points[3].y = y2 * 0.5;
				break;

			default:
				ok = 1;
				break;
			}

			NewGeomPolygon =
			    AddGeomPolygonAsGeomPolygon(&SpecialGeomPolygon, Object->x1, Object->y1, Object->RotationAngle,
			                                Object->Mirror, &OffsetX, &OffsetY);
		}

		Pad.X = (float) (-OffsetX);
		Pad.Y = (float) (-OffsetY);
		Pad.Layer = Object->Layer;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);
		// Fill in at which address the polygon pointer must be stored
		NewGeomPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special.AddressOffset);
		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case PIN_PUT_THROUGH_POLYGON:
		GeomPolygon = (GeomPolygonRecord *) Object->Address;
		memset(&Pad, 0, sizeof(PadRecord));
#ifdef _DEBUG

		if (NrPins == 231)
			ok = 1;

		if ((InRange9(Object->x1, 39.3e5)) && (InRange9(Object->y1, 54.6e5)))
			ok = 1;

#endif

		if (Object->ObjectType2 == 0)
		{
			if (!GeomPolygon)
				ok = 1;

			NewGeomPolygon =
			    AddGeomPolygonAsGeomPolygon(GeomPolygon, Object->x1, Object->y1, Object->RotationAngle, Object->Mirror,
			                                &OffsetX, &OffsetY);
			Pad.X = (float) -OffsetX;
			Pad.Y = (float) -OffsetY;
		}
		else
		{
			memset(&SpecialGeomPolygon, 0, sizeof(SpecialGeomPolygon));
			SpecialGeomPolygon.NrVertices = 0;

			switch (Object->ObjectType2)
			{
			case OBJECT_RECT:
			case PIN_SMD_RECT:
			case PIN_PUT_THROUGH_SQUARE:
				x2 = Object->x2;
				y2 = Object->y2;

				if (Object->ObjectType2 == PIN_PUT_THROUGH_SQUARE)
					y2 = x2;

				SpecialGeomPolygon.NrVertices = 4;
				SpecialGeomPolygon.Points[0].x = -x2 * 0.5;
				SpecialGeomPolygon.Points[0].y = -y2 * 0.5;
				SpecialGeomPolygon.Points[1].x = x2 * 0.5;
				SpecialGeomPolygon.Points[1].y = -y2 * 0.5;
				SpecialGeomPolygon.Points[2].x = x2 * 0.5;
				SpecialGeomPolygon.Points[2].y = y2 * 0.5;
				SpecialGeomPolygon.Points[3].x = -x2 * 0.5;
				SpecialGeomPolygon.Points[3].y = y2 * 0.5;
				break;

			default:
				ok = 1;
				break;
			}

			NewGeomPolygon =
			    AddGeomPolygonAsGeomPolygon(&SpecialGeomPolygon, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
			                                &OffsetX, &OffsetY);
			Pad.X = (float) Object->x1;
			Pad.Y = (float) Object->y1;
		}

		Pad.Width = (float) Object->x2;
		Pad.Height = (float) Object->y2;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);

		if (Object->x3 != 0.0)
		{
			Pad.Special.Extra1 = (float) Object->x3;	// Inner pad
		}
		else
		{
			Pad.Special.Extra1 = Pad.Width;	// Inner pad
		}

		if (Object->x3 != 0.0)
		{
			Pad.Extra2 = (float) Object->y3;	// Anti power pad
		}
		else
		{
			Pad.Special.Extra1 = (float) (Pad.Width + Pad.Clearance * 2.0);	// Anti power pad
		}

		Pad.Layer = -1;
		Pad.ShapeType = PIN_PUT_THROUGH_POLYGON;
		Pad.Special.AddressOffset = 0;
		// Fill in at which address the polygon pointer must be stored
		NewGeomPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special.AddressOffset);
		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case PIN_SMD_ROUND:
	case PIN_SMD_RECT:
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
	case TRACE_VER:
	case TRACE_HOR:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
		memset(&Pad, 0, sizeof(PadRecord));

		switch (ObjectType)
		{
		case TRACE_VER:
			ObjectType = PIN_LINE_VER;
			break;

		case TRACE_HOR:
			ObjectType = PIN_LINE_HOR;
			break;

		case TRACE_DIAG1:
			ObjectType = PIN_LINE_DIAG1;
			break;

		case TRACE_DIAG2:
			ObjectType = PIN_LINE_DIAG2;
			break;
		}

		Pad.ShapeType = ObjectType;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);
		Pad.X = (float) Object->x1;
		Pad.Y = (float) Object->y1;
		Pad.Width = (float) Object->x2;
		Pad.Height = (float) Object->y2;
		Pad.Layer = Object->Layer;
		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
		memset(&Pad, 0, sizeof(PadRecord));
		Pad.ShapeType = PIN_LINE_ALL_ANGLE;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);
		Pad.X = (float) Object->x1;
		Pad.Y = (float) Object->y1;
		Pad.Width = (float) Object->x2;
		Pad.Height = (float) Object->y2;
		Pad.Special.Thickness = (float) Object->Thickness;
		Pad.Layer = Object->Layer;
		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case PIN_ARC:
	case TRACE_ARC:
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
		ShapeLines[11] = (float) max(Object->Clearance, GeomSaveClearance);
		ObjectSize = 8;

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		ObjectSize = 40;

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[2], BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case DRILL_UNPLATED:
		memset(&Pad, 0, sizeof(PadRecord));
		Pad.ShapeType = Object->ObjectType;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);
		Pad.X = (float) Object->x1;
		Pad.Y = (float) Object->y1;
		Pad.Width = (float) Object->x2;
		Pad.Layer = -1;

		if (Object->y3 != 0.0)
		{
			Pad.Extra2 = (float) Object->y3;	// Anti power pad
		}
		else
		{
			Pad.Extra2 = (float) (Pad.Width + Pad.Clearance * 2.0);	// Anti power pad
		}

		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case DRILL:
		memset(&Pad, 0, sizeof(PadRecord));
		Pad.ShapeType = Object->ObjectType;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);
		Pad.X = (float) Object->x1;
		Pad.Y = (float) Object->y1;
		Pad.Width = (float) Object->x2;

		if (Object->x3 != 0.0)
		{
			Pad.Special.Extra1 = (float) Object->x3;	// Inner pad
		}
		else
		{
			Pad.Special.Extra1 = Pad.Width;	// Inner pad
		}

		if (Object->y3 != 0.0)
		{
			Pad.Extra2 = (float) Object->y3;	// Anti power pad
		}
		else
		{
			Pad.Extra2 = (float) (Pad.Width + Pad.Clearance * 2.0);	// Anti power pad
		}

		Pad.Layer = -1;
		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case VIA_PUT_THROUGH_ROUND:
		memset(&Pad, 0, sizeof(PadRecord));
		Pad.ShapeType = PIN_PUT_THROUGH_ROUND;
		Pad.X = (float) Object->x1;
		Pad.Y = (float) Object->y1;
		Pad.Width = (float) Object->x2;
		Pad.Height = (float) Object->y2;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);
		Pad.Layer = -1;

		if (Object->x3 != 0.0)
		{
			Pad.Special.Extra1 = (float) Object->x3;	// Inner pad
		}
		else
		{
			Pad.Special.Extra1 = Pad.Width;	// Inner pad
		}

		if (Object->y3 != 0.0)
		{
			Pad.Extra2 = (float) Object->y3;	// Anti power pad
		}
		else
		{
			Pad.Extra2 = (float) (Pad.Width + Pad.Clearance * 2.0);	// Anti power pad
		}

		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	case PIN_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
		memset(&Pad, 0, sizeof(PadRecord));
		Pad.ShapeType = Object->ObjectType;
		Pad.X = (float) Object->x1;
		Pad.Y = (float) Object->y1;
		Pad.Width = (float) Object->x2;
		Pad.Height = (float) Object->y2;
		Pad.Clearance = (float) max(Object->Clearance, GeomSaveClearance);

		if (Object->x3 != 0.0)
		{
			Pad.Special.Extra1 = (float) Object->x3;	// Inner pad
		}
		else
		{
			Pad.Special.Extra1 = Pad.Width;	// Inner pad
		}

		if (Object->y3 != 0.0)
		{
			Pad.Extra2 = (float) Object->y3;	// Anti power pad
		}
		else
		{
			Pad.Extra2 = (float) (Pad.Width + Pad.Clearance * 2.0);	// Anti power pad
		}

		Pad.Layer = -1;
		ObjectSize = sizeof(PadRecord);

		if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
		NrPinShapesSaved++;
		Object->Info |= OBJECT_DONE;
		break;

	default:
		ok = 1;
		break;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK SaveGeomDialog2(HWND Dialog, WORD Message, WORD WParam, int32 LParam)
{
	int32 about;
	char str[MAX_LENGTH_STRING], *StrP;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetWindowTextUTF8(Dialog, SC(1131, "Save layout as a geoemtry"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(1126, "Copy component references on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(1127, "Copy component values on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(1128, "Convert board outline to component outline"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK4, SC(1129, "Soldermask on vias"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(1130, "Start text geometry pins"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		if (StartPinText[0] == 0)
		{
			GetFilePartFromFileName(StartPinText, EditFile);
			StrP = strchr(StartPinText, '.');

			if (StrP)
				*StrP = 0;
		}

		if (GeomSaveOptions & 1)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		if (GeomSaveOptions & 2)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

		if (GeomSaveOptions & 4)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_SETCHECK, 1, 0);

		if (GeomSaveOptions & 8)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_SETCHECK, 1, 0);

		SetDialogItemTextUTF8(Dialog, IDC_EDIT1, StartPinText);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			GeomSaveOptions = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0))
				GeomSaveOptions |= 1;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0))
				GeomSaveOptions |= 2;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0))
				GeomSaveOptions |= 4;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK4, BM_GETCHECK, 0, 0))
				GeomSaveOptions |= 8;

			memset(str, 0, 6);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_GETTEXT, 80, (LPARAM) str);
			str[5] = 0;

			if (strlen(str) > 0)
			{
				strcpy(StartPinText, str);
				EndDialog(Dialog, 1);
			}

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SaveLayoutAsGeometry(int32 mode)
{
	int32 NetNr, cnt, cnt2, cnt3, cnt4, cnt5, Layer, ObjectType, LineSegments, PolygonPos, res, SegmentCount,
	      ObjectSize, ObjectInfo, fp, Written, MaxCountX, NrLines, *Int32P, Mirror, Length, PinNr, FoundAreaFill, ViaType,
	      ShapePadBufPos, *MemSizeObjectPolygons, *ShapeInfoP, ok;
	ObjectRecord *Object, NewObject;
	CompRecord *Comp;
	GeomPolygonRecord *GeomPolygon, *NewGeomPolygon, SpecialGeomPolygon;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	PolygonRecord *Polygon, NewPolygon;
	char NewPinText[20], str2[200], str[1024], TextString[1024], *TextP;
	LPSTR StrP;
	AreaFillRecord *AreaFill;
	ViaRecord *Via;
	int32 LayerOk;
	double OffsetX, OffsetY, x1, y1, x2, y2, x3, y3, x4, y4, LineBuf[4096], minx, miny, maxx, maxy, Rotation2;
#ifdef _DEBUG
	NetRecord *Net;
#endif

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE)
			MessageBoxOwn(PCBWindow, SC(1025, "Powerplanes are not supported"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
	}

	if (GeomSaveClearance == 0.0)
		GeomSaveClearance = Design.StandardClearance;

	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SAVEGEOM), PCBWindow, (DLGPROC) SaveGeomDialog2);

	if (res != 1)
		return -1;

	SetWaitCursor();

	SpecialPolygon = 0;
	Polygon = (PolygonRecord *) TextString;

	if (AllocateMemTemp2(256 * 1024) != 0)
		return -1;

	MaxGeomPolygonsMem = MaxTempMemory2;
	GeomPolygonsMem = TempMem2;

	if (AllocateMemTemp3(4096 * sizeof(int32)) != 0)
		return -1;

	MaxGeomPolygons = 4096;
	GeomPolygons = (GeomPolygonsArray *) TempMem3;
	GeomPolygonsMemSize = 0;
	NrGeomPolygons = 0;
	BufPos = sizeof(ShapeRecord);
	memset(&Shape, 0, BufPos);
	ShapeInfoP = (int32 *) & ShapeLines;

	Shape.NrPlacementOutLines = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			NrObjects = 0;
			ShapePlacementOutLineToObject(Comp, 0.0, 0.0, 0.0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
				ObjectType = Object->ObjectType;
				*ShapeInfoP = (int32) ObjectType;
				ShapeLines[1] = (float) Object->x1;
				ShapeLines[2] = (float) Object->y1;
				ShapeLines[5] = (float) 0.0;

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

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrPlacementOutLines++;
						}
					}
					else
					{
						ShapeLines[3] = (float) Object->x2;
						ShapeLines[4] = (float) Object->y2;
						ObjectSize = 24;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrPlacementOutLines++;
					}

					break;

				case OBJECT_RECT:
					*ShapeInfoP = OBJECT_LINE;
					ObjectSize = 24;
					x1 = Object->x1;
					y1 = Object->y1;
					x2 = Object->x2;
					y2 = Object->y2;

					ShapeLines[1] = (float) (x1 - x2 * 0.5);
					ShapeLines[2] = (float) (y1 - y2 * 0.5);
					ShapeLines[3] = (float) (x1 + x2 * 0.5);
					ShapeLines[4] = (float) (y1 - y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					ShapeLines[1] = (float) (x1 + x2 * 0.5);
					ShapeLines[2] = (float) (y1 - y2 * 0.5);
					ShapeLines[3] = (float) (x1 + x2 * 0.5);
					ShapeLines[4] = (float) (y1 + y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					ShapeLines[1] = (float) (x1 + x2 * 0.5);
					ShapeLines[2] = (float) (y1 + y2 * 0.5);
					ShapeLines[3] = (float) (x1 - x2 * 0.5);
					ShapeLines[4] = (float) (y1 + y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					ShapeLines[1] = (float) (x1 - x2 * 0.5);
					ShapeLines[2] = (float) (y1 + y2 * 0.5);
					ShapeLines[3] = (float) (x1 - x2 * 0.5);
					ShapeLines[4] = (float) (y1 - y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrPlacementOutLines += 4;
					break;

				case OBJECT_CIRCLE:
					if (Object->Info2 != 0)
					{
						ShapeLines[3] = (float) Object->x2;
						ShapeLines[4] = (float) Object->Info2;
						ObjectSize = 24;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrPlacementOutLines++;
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
					ShapeLines[9] = (float) 0.0;
					ObjectSize = 40;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrPlacementOutLines++;
					break;

				case OBJECT_POLYGON:
					if ((Object->ObjectType2 != 0) && (Object->ObjectType2 == OBJECT_RECT))
					{
						memset(Polygon, 0, 512);
						x2 = Object->x2;
						y2 = Object->y2;
						Polygon->NrVertices = 4;
						Polygon->Points[0].x = -x2 * 0.5;
						Polygon->Points[0].y = -y2 * 0.5;
						Polygon->Points[1].x = x2 * 0.5;
						Polygon->Points[1].y = -y2 * 0.5;
						Polygon->Points[2].x = x2 * 0.5;
						Polygon->Points[2].y = y2 * 0.5;
						Polygon->Points[3].x = -x2 * 0.5;
						Polygon->Points[3].y = y2 * 0.5;

						if (Object->Mirror)
							MirrorPolygon(Polygon, 0);

						RotatePolygon(Polygon, 0.0, 0.0, Object->RotationAngle, 0);
						MovePolygon(Polygon, Object->x1, Object->y1, 0);
						*ShapeInfoP = OBJECT_LINE;
						ObjectSize = 24;
						ShapeLines[1] = (float) Polygon->Points[0].x;
						ShapeLines[2] = (float) Polygon->Points[0].y;
						ShapeLines[3] = (float) Polygon->Points[1].x;
						ShapeLines[4] = (float) Polygon->Points[1].y;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;

						ShapeLines[1] = (float) Polygon->Points[1].x;
						ShapeLines[2] = (float) Polygon->Points[1].y;
						ShapeLines[3] = (float) Polygon->Points[2].x;
						ShapeLines[4] = (float) Polygon->Points[2].y;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;

						ShapeLines[1] = (float) Polygon->Points[2].x;
						ShapeLines[2] = (float) Polygon->Points[2].y;
						ShapeLines[3] = (float) Polygon->Points[3].x;
						ShapeLines[4] = (float) Polygon->Points[3].y;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;

						ShapeLines[1] = (float) Polygon->Points[3].x;
						ShapeLines[2] = (float) Polygon->Points[3].y;
						ShapeLines[3] = (float) Polygon->Points[0].x;
						ShapeLines[4] = (float) Polygon->Points[0].y;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;

						Shape.NrPlacementOutLines += 4;
					}

					break;

				default:
					ok = 1;
					break;
				}
			}
		}
	}

	Shape.CompOutLineOffset = BufPos;

// **********************************************************************************
// **********************************************************************************

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			NrObjects = 0;
			ShapeCompOutLineToObject(Comp, 0.0, 0.0, 0.0);
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 3);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
				ObjectInfo = Object->Info;
				Layer = Object->Layer;

				if ((Layer == COMP_OUTLINE_LAYER_TOP) || (Layer == COMP_OUTLINE_LAYER_BOTTOM)
				        || ((GeomSaveOptions & 4) && (Layer == BOARD_OUTLINE_LAYER)))
				{
#ifdef _DEBUG

					if (Layer == BOARD_OUTLINE_LAYER)
						ok = 1;

#endif
					ObjectType = Object->ObjectType;
					*ShapeInfoP = (int32) ObjectType;
					ShapeLines[1] = (float) Object->x1;
					ShapeLines[2] = (float) Object->y1;
					ShapeLines[5] = (float) Object->Thickness;

					switch (ObjectType)
					{
					case OBJECT_LINE:
						ShapeLines[3] = (float) Object->x2;
						ShapeLines[4] = (float) Object->y2;
						ObjectSize = 24;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrCompOutLines++;
						break;

					case OBJECT_RECT:
						*ShapeInfoP = OBJECT_LINE;
						ObjectSize = 24;
						x1 = Object->x1;
						y1 = Object->y1;
						x2 = Object->x2;
						y2 = Object->y2;

						ShapeLines[1] = (float) (x1 - x2 * 0.5);
						ShapeLines[2] = (float) (y1 - y2 * 0.5);
						ShapeLines[3] = (float) (x1 + x2 * 0.5);
						ShapeLines[4] = (float) (y1 - y2 * 0.5);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;

						ShapeLines[1] = (float) (x1 + x2 * 0.5);
						ShapeLines[2] = (float) (y1 - y2 * 0.5);
						ShapeLines[3] = (float) (x1 + x2 * 0.5);
						ShapeLines[4] = (float) (y1 + y2 * 0.5);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;

						ShapeLines[1] = (float) (x1 + x2 * 0.5);
						ShapeLines[2] = (float) (y1 + y2 * 0.5);
						ShapeLines[3] = (float) (x1 - x2 * 0.5);
						ShapeLines[4] = (float) (y1 + y2 * 0.5);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;

						ShapeLines[1] = (float) (x1 - x2 * 0.5);
						ShapeLines[2] = (float) (y1 + y2 * 0.5);
						ShapeLines[3] = (float) (x1 - x2 * 0.5);
						ShapeLines[4] = (float) (y1 - y2 * 0.5);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrCompOutLines += 4;
						break;

					case OBJECT_CIRCLE:
						ShapeLines[3] = (float) Object->x2;
						ShapeLines[4] = (float) Object->Info2;
						ObjectSize = 24;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
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

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrCompOutLines++;
						break;

					case OBJECT_TEXT:
						ShapeLines[3] = (float) Object->x2;

						if (Object->Mirror == 0)
							ShapeLines[4] = (float) (LimitRotation(Object->RotationAngle) + 2000.0);
						else
							ShapeLines[4] = (float) (LimitRotation(Object->RotationAngle) + 4000.0);

						memset(&ShapeLines[6], 0, 64);
						memmove(&ShapeLines[6], (LPSTR) Object->TraceNr, min(strlen((LPSTR) Object->TraceNr), 63));
						ObjectSize = 88;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrCompOutLines++;
						break;

					case PIN_SMD_POLYGON:
					case OBJECT_POLYGON:
						if (Object->ObjectType2 == 0)
						{
							GeomPolygon = (GeomPolygonRecord *) Object->Address;
							NewGeomPolygon =
							    AddGeomPolygonAsGeomPolygon(GeomPolygon, Object->x1, Object->y1, Object->RotationAngle,
							                                Object->Mirror, &OffsetX, &OffsetY);
							*ShapeInfoP = OBJECT_POLYGON;
							ShapeLines[1] = (float) -OffsetX;
							ShapeLines[2] = (float) -OffsetY;
							ShapeLines[3] = (float) 0.0;
							ObjectSize = 16;
							NewGeomPolygon->ShapePolygonPosition = BufPos + 12;

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrCompOutLines++;
						}
						else
						{
							if (Object->ObjectType2 == OBJECT_RECT)
							{
								x1 = Object->x1;
								y1 = Object->y1;
								x2 = Object->x2;
								y2 = Object->y2;

								if (Object->Info & OBJECT_FILLED)
								{
									memset(&SpecialGeomPolygon, 0, sizeof(SpecialGeomPolygon));
									SpecialGeomPolygon.NrVertices = 4;
									SpecialGeomPolygon.Points[0].x = -x2 * 0.5;
									SpecialGeomPolygon.Points[0].y = -y2 * 0.5;
									SpecialGeomPolygon.Points[1].x = x2 * 0.5;
									SpecialGeomPolygon.Points[1].y = -y2 * 0.5;
									SpecialGeomPolygon.Points[2].x = x2 * 0.5;
									SpecialGeomPolygon.Points[2].y = y2 * 0.5;
									SpecialGeomPolygon.Points[3].x = -x2 * 0.5;
									SpecialGeomPolygon.Points[3].y = y2 * 0.5;
									NewGeomPolygon =
									    AddGeomPolygonAsGeomPolygon(&SpecialGeomPolygon, Object->x1, Object->y1,
									                                Object->RotationAngle, Object->Mirror, &OffsetX,
									                                &OffsetY);
									NewGeomPolygon->ShapePolygonPosition = BufPos + 12;
									*ShapeInfoP = OBJECT_POLYGON;
									ShapeLines[1] = (float) Object->x1;
									ShapeLines[2] = (float) Object->y1;
									ShapeLines[3] = 0.0;
									ObjectSize = 16;
									NewGeomPolygon->ShapePolygonPosition = BufPos + 12;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;
									Shape.NrCompOutLines++;
								}
								else
								{
									memset(&NewPolygon, 0, sizeof(PolygonRecord));
									NewPolygon.NrVertices = 4;
									NewPolygon.Points[0].x = -x2 * 0.5;
									NewPolygon.Points[0].y = -y2 * 0.5;
									NewPolygon.Points[1].x = x2 * 0.5;
									NewPolygon.Points[1].y = -y2 * 0.5;
									NewPolygon.Points[2].x = x2 * 0.5;
									NewPolygon.Points[2].y = y2 * 0.5;
									NewPolygon.Points[3].x = -x2 * 0.5;
									NewPolygon.Points[3].y = y2 * 0.5;

									if (Object->Mirror)
										MirrorPolygon(&NewPolygon, 0);

									RotatePolygon(&NewPolygon, 0.0, 0.0, Object->RotationAngle, 0);
									MovePolygon(&NewPolygon, Object->x1, Object->y1, 0);
									*ShapeInfoP = OBJECT_LINE;
									ObjectSize = 24;

									ShapeLines[1] = (float) NewPolygon.Points[0].x;
									ShapeLines[2] = (float) NewPolygon.Points[0].y;
									ShapeLines[3] = (float) NewPolygon.Points[1].x;
									ShapeLines[4] = (float) NewPolygon.Points[1].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;

									ShapeLines[1] = (float) NewPolygon.Points[1].x;
									ShapeLines[2] = (float) NewPolygon.Points[1].y;
									ShapeLines[3] = (float) NewPolygon.Points[2].x;
									ShapeLines[4] = (float) NewPolygon.Points[2].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;

									ShapeLines[1] = (float) NewPolygon.Points[2].x;
									ShapeLines[2] = (float) NewPolygon.Points[2].y;
									ShapeLines[3] = (float) NewPolygon.Points[3].x;
									ShapeLines[4] = (float) NewPolygon.Points[3].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;

									ShapeLines[1] = (float) NewPolygon.Points[3].x;
									ShapeLines[2] = (float) NewPolygon.Points[3].y;
									ShapeLines[3] = (float) NewPolygon.Points[0].x;
									ShapeLines[4] = (float) NewPolygon.Points[0].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;
									Shape.NrCompOutLines += 4;
								}
							}
							else
								ok = 1;
						}

						break;
					}
				}
			}
		}
	}

// ****************************************************************************
// ****************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			LayerOk = 0;
			Layer = ObjectLine->Layer;

			switch (Layer)
			{
			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) && (Layer == BOARD_OUTLINE_LAYER))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				x1 = ObjectLine->X1;
				y1 = ObjectLine->Y1;
				x2 = ObjectLine->X2;
				y2 = ObjectLine->Y2;

				*ShapeInfoP = OBJECT_LINE;
				ShapeLines[1] = (float) x1;
				ShapeLines[2] = (float) y1;
				ShapeLines[3] = (float) x2;
				ShapeLines[4] = (float) y2;
				ShapeLines[5] = ObjectLine->LineThickNess;
				ObjectSize = 24;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrCompOutLines++;
			}
		}
	}

// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectRect->Layer;
			LayerOk = 0;

			switch (Layer)
			{
			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) && (Layer == BOARD_OUTLINE_LAYER))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				memset(&Pad, 0, sizeof(PadRecord));
				*ShapeInfoP = OBJECT_RECT;
				ShapeLines[1] = ObjectRect->CentreX;
				ShapeLines[2] = ObjectRect->CentreY;
				ShapeLines[3] = ObjectRect->Width;
				ShapeLines[4] = ObjectRect->Height;
				ShapeLines[5] = ObjectRect->LineThickNess;
				ObjectSize = 24;

				if (ObjectRect->Info & OBJECT_FILLED)
				{
					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrCompOutLines++;
				}
				else
				{
					*ShapeInfoP = OBJECT_LINE;
					x1 = ObjectRect->CentreX;
					y1 = ObjectRect->CentreY;
					x2 = ObjectRect->Width;
					y2 = ObjectRect->Height;
					Pad.Layer = COMP_OUTLINE_LAYER;

					ShapeLines[1] = (float) (x1 - x2 * 0.5);
					ShapeLines[2] = (float) (y1 - y2 * 0.5);
					ShapeLines[3] = (float) (x1 + x2 * 0.5);
					ShapeLines[4] = (float) (y1 - y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					ShapeLines[1] = (float) (x1 + x2 * 0.5);
					ShapeLines[2] = (float) (y1 - y2 * 0.5);
					ShapeLines[3] = (float) (x1 + x2 * 0.5);
					ShapeLines[4] = (float) (y1 + y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					ShapeLines[1] = (float) (x1 + x2 * 0.5);
					ShapeLines[2] = (float) (y1 + y2 * 0.5);
					ShapeLines[3] = (float) (x1 - x2 * 0.5);
					ShapeLines[4] = (float) (y1 + y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					ShapeLines[1] = (float) (x1 - x2 * 0.5);
					ShapeLines[2] = (float) (y1 + y2 * 0.5);
					ShapeLines[3] = (float) (x1 - x2 * 0.5);
					ShapeLines[4] = (float) (y1 - y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrCompOutLines += 4;
				}
			}
		}
	}

// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectArc->Layer;
#ifdef _DEBUG

			if ((InRange9(ObjectArc->CentreX, 167.6e5)) && (InRange9(ObjectArc->CentreY, -4.9e5)))
				ok = 1;

#endif
			LayerOk = 0;

			switch (Layer)
			{
			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) && (Layer == BOARD_OUTLINE_LAYER))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				*ShapeInfoP = OBJECT_ARC;
				ShapeLines[1] = ObjectArc->CentreX;
				ShapeLines[2] = ObjectArc->CentreY;
				ShapeLines[3] = ObjectArc->Width;
				ShapeLines[4] = ObjectArc->Height;
				ShapeLines[5] = ObjectArc->StartDiffX;
				ShapeLines[6] = ObjectArc->StartDiffY;
				ShapeLines[7] = ObjectArc->EndDiffX;
				ShapeLines[8] = ObjectArc->EndDiffY;
				ShapeLines[9] = ObjectArc->LineThickNess;
				ObjectSize = 40;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrCompOutLines++;
			}
		}
	}

// **********************************************************************************

	Shape.PinOffset = BufPos;

	for (cnt5 = 0; cnt5 < 2; cnt5++)
	{
		for (NetNr = 0; NetNr < Design.NrNets; NetNr++)
		{
			GetObjectsNet(NetNr, MODE_OBJECTS2, 0);
			FoundAreaFill = 0;

			for (cnt = 0; cnt < NrObjects2; cnt++)
			{
				Object = &((*Objects2)[cnt]);

				if ((Object->ObjectType == AREAFILL) || (CheckObjectIsBigPolygon(Object)))
				{
					FoundAreaFill = 1;	// Nets with areafills/special polygons will be saved first
				}
			}

#ifdef _DEBUG
			Net = &((*Nets)[NetNr]);

			if (stricmpOwn(Net->Name, "BUF_L") == 0)
				ok = 1;

#endif

			if (((cnt5 == 0) && (FoundAreaFill)) || ((cnt5 == 1) && (!FoundAreaFill)))
			{
				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					Object = &((*Objects2)[cnt]);
					Object->Info &= ~OBJECT_DONE;
				}

				NrPinShapesSaved = 0;
				memset(&ShapePad, 0, sizeof(ShapePadRecord));
				memset(&NewPinText, 0, sizeof(NewPinText));
				strncpy(ShapePad.Name, StartPinText, 5);
				sprintf(str, "%d", NetNr + 1000);
				strcat(ShapePad.Name, str);
				ShapePadBufPos = BufPos;
				ObjectSize = sizeof(ShapePadRecord);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;

				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					Object = &((*Objects2)[cnt]);

					switch (Object->ObjectType)
					{
					case AREAFILL:
					case PIN_SMD_POLYGON:
					case PIN_PUT_THROUGH_POLYGON:
						SavePinObject(Object);
						break;
					}
				}

				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					Object = &((*Objects2)[cnt]);
					ObjectType = Object->ObjectType;

					switch (ObjectType)
					{
					case PIN_SMD_ROUND:
					case PIN_SMD_RECT:
					case PIN_LINE_HOR:
					case PIN_LINE_VER:
					case PIN_LINE_DIAG1:
					case PIN_LINE_DIAG2:
					case TRACE_VER:
					case TRACE_HOR:
					case TRACE_DIAG1:
					case TRACE_DIAG2:
					case PIN_LINE_ALL_ANGLE:
					case TRACE_ALL_ANGLE:
					case PIN_ARC:
					case TRACE_ARC:
						SavePinObject(Object);
						break;
					}
				}

				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					Object = &((*Objects2)[cnt]);

					switch (Object->ObjectType)
					{
					case DRILL_UNPLATED:
					case DRILL:
						SavePinObject(Object);
						break;
					}
				}

				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					Object = &((*Objects2)[cnt]);

					switch (Object->ObjectType)
					{
					case VIA_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_SQUARE:
						SavePinObject(Object);
						break;
					}
				}

#ifdef _DEBUG

				if (NrPinShapesSaved > 1000)
					ok = 1;

				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					Object = &((*Objects2)[cnt]);

					if ((Object->Info & OBJECT_DONE) == 0)
						ok = 1;
				}

#endif
				ShapePad.NrPinShapes = (int16) NrPinShapesSaved;
				memcpy(&TempMem[ShapePadBufPos], &ShapePad, sizeof(ShapePad));
			}
		}
	}

	NrPins = Design.NrNets;

	for (cnt2 = 0; cnt2 < Design.NrComps; cnt2++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt2]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "C12") == 0)
				ok = 1;

#endif
			NrObjects = 0;
			ShapePinsToObject(Comp, (float) 0.0, (float) 0.0, 0, 0, 0, 1);
			PinNr = -1;

			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);
				Object->Info &= ~(OBJECT_DONE | OBJECT_WARNING);
#ifdef _DEBUG

				if ((InRange9(Object->x1, 360.7e5)) && (InRange9(Object->y1, 259.6e5)))
					ok = 1;

#endif

				if ((Object->NetNr == 32767) || (Object->NetNr == -1))
				{
					if (Object->PinNr != PinNr)
					{
						Object->Info |= OBJECT_WARNING;
						PinNr = Object->PinNr;
					}
				}
			}

			ShapePadBufPos = 0;
			NrPinShapesSaved = -1;

			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if ((Object->NetNr == 32767) || (Object->NetNr == -1))
				{
#ifdef _DEBUG

					if ((InRange9(Object->x1, 360.7e5)) && (InRange9(Object->y1, 259.6e5)))
						ok = 1;

#endif

					if (Object->Info & OBJECT_WARNING)
					{
						if (NrPinShapesSaved > 0)
						{
#ifdef _DEBUG

							if (NrPins == 221)
								ok = 1;

#endif
							ShapePad.NrPinShapes = (int16) NrPinShapesSaved;
							memcpy(&TempMem[ShapePadBufPos], &ShapePad, sizeof(ShapePad));
						}
						else
						{
							if (NrPinShapesSaved == 0)
							{
								BufPos -= sizeof(ShapePadRecord);
								NrPins--;
							}
							else
								ok = 1;
						}

						NrPinShapesSaved = 0;
						memset(&ShapePad, 0, sizeof(ShapePadRecord));
						memset(&NewPinText, 0, sizeof(NewPinText));
						strncpy(ShapePad.Name, StartPinText, 5);
						sprintf(str, "%d", NrPins + 1000);
						strcat(ShapePad.Name, str);
						ShapePadBufPos = BufPos;
						ObjectSize = sizeof(ShapePadRecord);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						NrPins++;
					}

					SavePinObject(Object);
				}
			}

#ifdef _DEBUG

			if (NrPins == 222)
				ok = 1;

#endif

			if (NrPinShapesSaved > 0)
			{
#ifdef _DEBUG

				if (NrPins == 222)
					ok = 1;

#endif
				ShapePad.NrPinShapes = (int16) NrPinShapesSaved;
				memcpy(&TempMem[ShapePadBufPos], &ShapePad, sizeof(ShapePad));
			}
			else
			{
				if (NrPinShapesSaved == 0)
				{
					BufPos -= sizeof(ShapePadRecord);
					NrPins--;
				}
			}

			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if (((Object->Info & OBJECT_DONE) == 0) && ((Object->NetNr == 32767) || (Object->NetNr == -1)))
					ok = 1;
			}
		}
	}

// **********************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			LayerOk = 0;
			Layer = ObjectLine->Layer;

			if ((Layer >= 0) && (Layer < Design.NrBoardLayers) && (ObjectLine->NetNr == -1))
			{
				memset(&ShapePad, 0, sizeof(ShapePadRecord));
				memset(&NewPinText, 0, sizeof(NewPinText));
				strncpy(ShapePad.Name, StartPinText, 5);
				sprintf(str, "%d", NrPins + 1000);
				strcat(ShapePad.Name, str);
				ShapePadBufPos = BufPos;
				ShapePad.NrPinShapes = 1;
				ObjectSize = sizeof(ShapePadRecord);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				NrPins++;

				memset(&Pad, 0, sizeof(PadRecord));
				Pad.Layer = ObjectLine->Layer;
				Pad.ShapeType = PIN_LINE_ALL_ANGLE;
				Pad.Special.Thickness = ObjectLine->LineThickNess;
				Pad.Clearance = (float) max(ObjectLine->Clearance, GeomSaveClearance);
				ObjectSize = sizeof(PadRecord);
				Pad.X = ObjectLine->X1;
				Pad.Y = ObjectLine->Y1;
				Pad.Width = ObjectLine->X2;
				Pad.Height = ObjectLine->Y2;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
			}
		}
	}

// **********************************************************************************

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			LayerOk = 0;
			Layer = ObjectRect->Layer;

			if ((Layer >= 0) && (Layer < Design.NrBoardLayers) && (ObjectRect->NetNr == -1))
			{
				memset(&ShapePad, 0, sizeof(ShapePadRecord));
				memset(&NewPinText, 0, sizeof(NewPinText));
				strncpy(ShapePad.Name, StartPinText, 5);
				sprintf(str, "%d", NrPins + 1000);
				strcat(ShapePad.Name, str);
				ShapePadBufPos = BufPos;
				ObjectSize = sizeof(ShapePadRecord);

				if ((ObjectRect->Info & OBJECT_FILLED) == OBJECT_FILLED)
				{
					ShapePad.NrPinShapes = 1;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					NrPins++;

					memset(&Pad, 0, sizeof(PadRecord));
					ObjectSize = sizeof(PadRecord);
					Pad.Layer = ObjectRect->Layer;
					Pad.ShapeType = PIN_SMD_RECT;
					Pad.X = ObjectRect->CentreX;
					Pad.Y = ObjectRect->CentreY;
					Pad.Width = ObjectRect->Width;
					Pad.Height = ObjectRect->Height;
					Pad.Clearance = (float) max(ObjectRect->Clearance, GeomSaveClearance);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
				}
				else
				{
					ShapePad.NrPinShapes = 4;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					NrPins++;
					x1 = ObjectRect->CentreX;
					y1 = ObjectRect->CentreY;
					x2 = ObjectRect->Width;
					y2 = ObjectRect->Height;

					memset(&Pad, 0, sizeof(PadRecord));
					ObjectSize = sizeof(PadRecord);
					Pad.Layer = ObjectRect->Layer;
					Pad.Height = ObjectRect->LineThickNess;
					Pad.Clearance = (float) max(ObjectRect->Clearance, GeomSaveClearance);

					Pad.ShapeType = PIN_LINE_HOR;
					Pad.X = (float) (x1 - x2 * 0.5);
					Pad.Y = (float) (y1 - y2 * 0.5);
					Pad.Width = (float) x2;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					Pad.ShapeType = PIN_LINE_VER;
					Pad.X = (float) (x1 - x2 * 0.5);
					Pad.Y = (float) (y1 - y2 * 0.5);
					Pad.Width = (float) y2;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					Pad.ShapeType = PIN_LINE_HOR;
					Pad.X = (float) (x1 - x2 * 0.5);
					Pad.Y = (float) (y1 + y2 * 0.5);
					Pad.Width = (float) x2;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					Pad.ShapeType = PIN_LINE_VER;
					Pad.X = (float) (x1 + x2 * 0.5);
					Pad.Y = (float) (y1 - y2 * 0.5);
					Pad.Width = (float) y2;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
				}
			}
		}
	}

// **********************************************************************************

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectArc->Layer;
			LayerOk = 0;

			switch (Layer)
			{
//        case DRILL_LAYER:
			case DRILL_UNPLATED_LAYER:
				LayerOk = 1;
				break;

			default:
				if ((Layer >= 0) && (Layer < Design.NrBoardLayers) && (ObjectArc->NetNr == -1))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				memset(&ShapePad, 0, sizeof(ShapePadRecord));
				memset(&NewPinText, 0, sizeof(NewPinText));
				strncpy(ShapePad.Name, StartPinText, 5);
				sprintf(str, "%d", NrPins + 1000);
				strcat(ShapePad.Name, str);
				ShapePadBufPos = BufPos;
				ShapePad.NrPinShapes = 1;
				ObjectSize = sizeof(ShapePadRecord);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				NrPins++;
#ifdef _DEBUG

				if ((InRange9(ObjectArc->CentreX, 165.7e5)) && (InRange9(ObjectArc->CentreY, -0.3e5)))
					ok = 1;

#endif
				memset(&Pad, 0, sizeof(PadRecord));

				if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
				{
					Pad.X = ObjectArc->CentreX;
					Pad.Y = ObjectArc->CentreY;
					Pad.Width = ObjectArc->Width;
					Pad.Clearance = (float) GeomSaveClearance;
					Pad.Extra2 = (float) (Pad.Width + Pad.Clearance * 2.0);	// Anti power pad

					if (Layer == DRILL_LAYER)
					{
						Pad.ShapeType = DRILL;
						Layer = -1;
						Pad.Special.Extra1 = Pad.Width;	// Inner pad
					}
					else
					{
						if (Layer == DRILL_UNPLATED_LAYER)
						{
							Pad.ShapeType = DRILL_UNPLATED;
							Layer = -1;
						}
					}

					Pad.Layer = Layer;
					ObjectSize = sizeof(PadRecord);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;
				}
				else
				{
					Pad.Layer = Layer;

					if (ObjectArc->Info & OBJECT_FILLED)
					{
						Pad.ShapeType = PIN_SMD_ROUND;
						Pad.X = ObjectArc->CentreX;
						Pad.Y = ObjectArc->CentreY;
						Pad.Width = ObjectArc->Width;
						Pad.Layer = Layer;
						Pad.Clearance = (float) max(ObjectArc->Clearance, GeomSaveClearance);
						ObjectSize = sizeof(PadRecord);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;
					}
					else
					{
						Pad.ShapeType = PIN_ARC;
						ShapeLines[2] = ObjectArc->CentreX;
						ShapeLines[3] = ObjectArc->CentreY;
						ShapeLines[4] = ObjectArc->Width;
						ShapeLines[5] = ObjectArc->Height;
						ShapeLines[6] = ObjectArc->StartDiffX;
						ShapeLines[7] = ObjectArc->StartDiffY;
						ShapeLines[8] = ObjectArc->EndDiffX;
						ShapeLines[9] = ObjectArc->EndDiffY;
						ShapeLines[10] = ObjectArc->LineThickNess;
						ShapeLines[11] = (float) max(ObjectArc->Clearance, GeomSaveClearance);
						Pad.Special.Thickness = ObjectArc->LineThickNess;
						ObjectSize = 8;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						ObjectSize = 40;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[2], BufPos, ObjectSize)) == -1)
							return -1;
					}
				}

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}
		}
	}

// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(ObjectText2->Text, "info1_0") == 0)
				ok = 1;

#endif
			Layer = ObjectText2->Layer;

			if ((Layer >= 0) && (Layer < Design.NrBoardLayers) && (ObjectText2->NetNr == -1))
			{
				memset(&ShapePad, 0, sizeof(ShapePadRecord));
				memset(&NewPinText, 0, sizeof(NewPinText));
				strncpy(ShapePad.Name, StartPinText, 5);
				sprintf(str, "%d", NrPins + 1000);
				strcat(ShapePad.Name, str);
				ShapePadBufPos = BufPos;
				NrPinShapesSaved = 0;

				ObjectSize = sizeof(ShapePadRecord);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				NrPins++;
				memset(&Pad, 0, sizeof(PadRecord));
				Pad.Layer = Layer;
				Pad.ShapeType = OBJECT_TEXT;
				NrPinShapesSaved = 0;

				memset(&Pad, 0, sizeof(PadRecord));
				Pad.Layer = Layer;
				Pad.ShapeType = PIN_LINE_ALL_ANGLE;
				Pad.Special.Thickness = ObjectText2->LineThickNess;
				Pad.Clearance = GeomSaveClearance;
				Mirror = (ObjectText2->TextMode & 0x10) >> 4;
				Rotation2 = ObjectText2->Rotation;

				if (Mirror == 0)
					Pad.Height = (float) (ObjectText2->Rotation + 2000.0);
				else
					Pad.Height = (float) (ObjectText2->Rotation + 4000.0);

				ObjectSize = sizeof(PadRecord);

				if ((NrLines = ConvertObjectTextToStrings(ObjectText2->Text, 0, &MaxCountX, ObjectText2->Layer)) != 0)
				{
					x1 = ObjectText2->X;
					y1 = ObjectText2->Y;
					x2 = x1;
					y2 = y1;
					x3 = ObjectText2->FontHeight;

					for (cnt3 = 0; cnt3 < NrLines; cnt3++)
					{
						LineSegments =
						    TextStringToLineSegments2(x2, y2, x3, ObjectText2->Rotation, 0, Mirror, TextStrings2[cnt3],
						                              (double *) &LineBuf);

						SegmentCount = 0;

						for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
						{
							Pad.X = (float) LineBuf[SegmentCount++];
							Pad.Y = (float) LineBuf[SegmentCount++];
							Pad.Width = (float) LineBuf[SegmentCount++];
							Pad.Height = (float) LineBuf[SegmentCount++];

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							NrPinShapesSaved++;
						}

						ShapePad.NrPinShapes = (int16) NrPinShapesSaved;
						memcpy(&TempMem[ShapePadBufPos], &ShapePad, sizeof(ShapePad));

						if (Mirror == 0)
							x2 += sin(ANGLE_CONVERT(ObjectText2->Rotation)) * x3 * 1.1;
						else
							x2 -= sin(ANGLE_CONVERT(ObjectText2->Rotation)) * x3 * 1.1;

						y2 -= cos(ANGLE_CONVERT(ObjectText2->Rotation)) * x3 * 1.1;
					}
				}
			}
		}
	}

// **********************************************************************************

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectPolygon->Layer;

			if ((Layer >= 0) && (Layer < Design.NrBoardLayers) && (ObjectPolygon->NetNr == -1))
			{
				memset(&ShapePad, 0, sizeof(ShapePadRecord));
				memset(&NewPinText, 0, sizeof(NewPinText));
				strncpy(ShapePad.Name, StartPinText, 5);
				sprintf(str, "%d", NrPins + 1000);
				strcat(ShapePad.Name, str);
				ShapePadBufPos = BufPos;
				ShapePad.NrPinShapes = 1;
				ObjectSize = sizeof(ShapePadRecord);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				NrPins++;
				NewGeomPolygon = AddObjectPolygonAsGeomPolygon(ObjectPolygon, &OffsetX, &OffsetY);
				memset(&Pad, 0, sizeof(PadRecord));
				Pad.X = (float) -OffsetX;
				Pad.Y = (float) -OffsetY;
				Pad.ShapeType = PIN_SMD_POLYGON;
				Pad.Layer = Layer;
				Pad.Clearance = (float) GeomSaveClearance;
				ObjectSize = sizeof(PadRecord);
				NewGeomPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special.AddressOffset);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}
		}
	}


// **********************************************************************************
// **********************************************************************************

	memset(&NewObject, 0, sizeof(NewObject));

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr < 0))
		{
			ok = 1;
			NewObject.ObjectType = AREAFILL;
			NewObject.TraceNr = cnt;
			NewObject.Layer = AreaFill->Layer;
			memset(&ShapePad, 0, sizeof(ShapePadRecord));
			memset(&NewPinText, 0, sizeof(NewPinText));
			strncpy(ShapePad.Name, StartPinText, 5);
			sprintf(str, "%d", NrPins + 1000);
			strcat(ShapePad.Name, str);
			ShapePadBufPos = BufPos;
			ShapePad.NrPinShapes = 1;
			ObjectSize = sizeof(ShapePadRecord);

			if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapePad, BufPos, ObjectSize)) == -1)
				return -1;

			BufPos = NewBufPos;
			NrPins++;
			SavePinObject(&NewObject);
		}
	}

	Shape.NrPins = NrPins;

// **********************************************************************************
// **********************************************************************************

	Shape.SilkScreenOffset = BufPos;
	Shape.NrSilkScreenOutLines = 0;
	Shape.OtherObjectsOffset = BufPos;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, 0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

				if ((InRange9(Object->x1, 150.5e5)) && (InRange9(Object->y1, 108.1e5)))
				{
					ok = 1;

					if (Object->Layer == SOLD_MASK_TOP)
						ok = 1;
				}

#endif
				ObjectInfo = Object->Info;
				Layer = Object->Layer;

				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Layer ^= 1;
					break;
				}

				LayerOk = 0;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
				case INFO_LAYER:
				case INFO_LAYER2:
				case INFO_LAYER3:
				case INFO_LAYER4:
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					LayerOk = 1;
					break;

				case BOARD_OUTLINE_LAYER:
					if ((GeomSaveOptions & 4) == 0)
						LayerOk = 1;

					break;

				default:
					if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
						LayerOk = 1;

					break;
				}

				if (LayerOk)
				{
					ObjectType = Object->ObjectType;

					switch (ObjectType)
					{
					case OBJECT_LINE:
						Pad.X = (float) Object->x1;
						Pad.Y = (float) Object->y1;
						Pad.Width = (float) Object->x2;
						Pad.Height = (float) Object->y2;
						Pad.Layer = Layer;
						Pad.ShapeType = ObjectType;
						Pad.Special.Thickness = (float) Object->Thickness;
						ObjectSize = sizeof(PadRecord);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
						break;

					case OBJECT_RECT:
						memset(&Pad, 0, sizeof(PadRecord));
						Pad.ShapeType = ObjectType;
						x1 = Object->x1;
						y1 = Object->y1;
						x2 = Object->x2;
						y2 = Object->y2;
#ifdef _DEBUG

						if ((InRange9(x1, 87.4e5)) && (InRange9(y1, 281.2e5)))
							ok = 1;

#endif
						Pad.Layer = Layer;
						ObjectSize = sizeof(PadRecord);

						if (Object->Info & OBJECT_FILLED)
						{
							Pad.X = (float) x1;
							Pad.Y = (float) y1;
							Pad.Width = (float) x2;
							Pad.Height = (float) y2;

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrOtherObjects++;
						}
						else
						{
							Pad.ShapeType = OBJECT_LINE;
							Pad.Special.Thickness = (float) Object->Thickness;

							Pad.X = (float) (x1 - x2 * 0.5);
							Pad.Y = (float) (y1 - y2 * 0.5);
							Pad.Width = (float) (x1 + x2 * 0.5);
							Pad.Height = (float) (y1 - y2 * 0.5);

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;

							Pad.X = (float) (x1 + x2 * 0.5);
							Pad.Y = (float) (y1 - y2 * 0.5);
							Pad.Width = (float) (x1 + x2 * 0.5);
							Pad.Height = (float) (y1 + y2 * 0.5);

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;

							Pad.X = (float) (x1 + x2 * 0.5);
							Pad.Y = (float) (y1 + y2 * 0.5);
							Pad.Width = (float) (x1 - x2 * 0.5);
							Pad.Height = (float) (y1 + y2 * 0.5);

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;

							Pad.X = (float) (x1 - x2 * 0.5);
							Pad.Y = (float) (y1 + y2 * 0.5);
							Pad.Width = (float) (x1 - x2 * 0.5);
							Pad.Height = (float) (y1 - y2 * 0.5);

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrOtherObjects += 4;
						}

						break;

					case OBJECT_CIRCLE:
#ifdef _DEBUG
						if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
							ok = 1;

#endif
						memset(&Pad, 0, sizeof(PadRecord));
						Pad.ShapeType = ObjectType;
						Pad.X = (float) Object->x1;
						Pad.Y = (float) Object->y1;
						Pad.Width = (float) Object->x2;
						Pad.Height = (float) Object->Info2;
						Pad.Layer = Layer;

						if ((Object->Info & OBJECT_FILLED) == 0)
							Pad.Special.Thickness = (float) Object->Thickness;

						ObjectSize = sizeof(PadRecord);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
						break;

					case OBJECT_ARC:
						if (((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
						        || (Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
						{
							memset(&Pad, 0, sizeof(PadRecord));
							Pad.ShapeType = OBJECT_CIRCLE;
							Pad.X = (float) Object->x1;
							Pad.Y = (float) Object->y1;
							Pad.Width = (float) Object->x2;
							Pad.Layer = Layer;
							ObjectSize = sizeof(PadRecord);

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;
						}
						else
						{
							if (Object->Info & OBJECT_FILLED)
							{
								memset(&Pad, 0, sizeof(PadRecord));
								Pad.ShapeType = OBJECT_CIRCLE;
								Pad.X = (float) Object->x1;
								Pad.Y = (float) Object->y1;
								Pad.Width = (float) Object->x2;
								Pad.Layer = Layer;
								ObjectSize = sizeof(PadRecord);

								if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
									return -1;
							}
							else
							{
								Pad.ShapeType = ObjectType;
								Pad.Layer = Layer;
								ShapeLines[2] = (float) Object->x1;
								ShapeLines[3] = (float) Object->y1;
								ShapeLines[4] = (float) Object->x2;
								ShapeLines[5] = (float) Object->y2;
								ShapeLines[6] = (float) Object->x3;
								ShapeLines[7] = (float) Object->y3;
								ShapeLines[8] = (float) Object->x4;
								ShapeLines[9] = (float) Object->y4;
								ShapeLines[10] = (float) Object->Thickness;
								ShapeLines[11] = (float) 0.0;
								ObjectSize = 8;

								if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
									return -1;

								BufPos = NewBufPos;
								ObjectSize = 40;

								if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[2], BufPos, ObjectSize)) == -1)
									return -1;
							}
						}

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
						break;

					case OBJECT_TEXT:
						switch (Layer)
						{
						case INFO_LAYER:
						case INFO_LAYER2:
						case INFO_LAYER3:
						case INFO_LAYER4:
						case SILKSCREEN_TOP:
						case SILKSCREEN_BOTTOM:
#ifdef _DEBUG
							ok = 1;
#endif
							TextP = (LPSTR) Object->TraceNr;
							Pad.ShapeType = ObjectType;
							Pad.X = (float) Object->x1;
							Pad.Y = (float) Object->y1;
							Pad.Width = (float) Object->x2;
							Pad.Height = (float) Object->y2;
							Pad.Layer = Layer;

							if (Object->Mirror == 0)
								Pad.Height = (float) (LimitRotation(Object->RotationAngle) + 2000.0);
							else
								Pad.Height = (float) (LimitRotation(Object->RotationAngle) + 4000.0);

							Pad.Special.Thickness = (float) Object->Thickness;
							memset(&ShapeLines[7], 0, 64);
							memcpy(&ShapeLines[7], TextP, min(strlen(TextP), 63));
							ObjectSize = 28;

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							ObjectSize = 64;

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[7], BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrOtherObjects++;
							break;

						default:
							ok = 1;
							break;
						}

						break;

					case PIN_SMD_POLYGON:
					case OBJECT_POLYGON:
#ifdef _DEBUG
						if (Layer == BOARD_OUTLINE_LAYER)
							ok = 1;

#endif

						if (Object->ObjectType2 == 0)
						{
							GeomPolygon = (GeomPolygonRecord *) Object->Address;
							NewGeomPolygon =
							    AddGeomPolygonAsGeomPolygon(GeomPolygon, Object->x1, Object->y1, Object->RotationAngle,
							                                Object->Mirror, &OffsetX, &OffsetY);
							memset(&Pad, 0, sizeof(PadRecord));
							Pad.X = (float) -OffsetX;
							Pad.Y = (float) -OffsetY;
							Pad.ShapeType = OBJECT_POLYGON;
							Pad.Layer = Layer;

							if ((Object->Info & OBJECT_FILLED) == 0)
								Pad.Special.Thickness = (float) Object->Thickness;

							ObjectSize = sizeof(PadRecord);
							NewGeomPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special.AddressOffset);

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrOtherObjects++;
						}
						else
						{
							if (Object->ObjectType2 == OBJECT_RECT)
							{
								x1 = Object->x1;
								y1 = Object->y1;
								x2 = Object->x2;
								y2 = Object->y2;

								if (Object->Info & OBJECT_FILLED)
								{
									memset(&SpecialGeomPolygon, 0, sizeof(SpecialGeomPolygon));
									SpecialGeomPolygon.NrVertices = 4;
									SpecialGeomPolygon.Points[0].x = -x2 * 0.5;
									SpecialGeomPolygon.Points[0].y = -y2 * 0.5;
									SpecialGeomPolygon.Points[1].x = x2 * 0.5;
									SpecialGeomPolygon.Points[1].y = -y2 * 0.5;
									SpecialGeomPolygon.Points[2].x = x2 * 0.5;
									SpecialGeomPolygon.Points[2].y = y2 * 0.5;
									SpecialGeomPolygon.Points[3].x = -x2 * 0.5;
									SpecialGeomPolygon.Points[3].y = y2 * 0.5;
									NewGeomPolygon =
									    AddGeomPolygonAsGeomPolygon(&SpecialGeomPolygon, Object->x1, Object->y1,
									                                Object->RotationAngle, Object->Mirror, &OffsetX,
									                                &OffsetY);
									memset(&Pad, 0, sizeof(PadRecord));
									Pad.X = (float) Object->x1;
									Pad.Y = (float) Object->y1;
									Pad.ShapeType = OBJECT_POLYGON;
									Pad.Layer = Layer;
									ObjectSize = sizeof(PadRecord);
									NewGeomPolygon->ShapePolygonPosition =
									    BufPos + offsetof(PadRecord, Special.AddressOffset);

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;
									Shape.NrOtherObjects++;
								}
								else
								{
									memset(&NewPolygon, 0, sizeof(PolygonRecord));
									NewPolygon.NrVertices = 4;
									NewPolygon.Points[0].x = -x2 * 0.5;
									NewPolygon.Points[0].y = -y2 * 0.5;
									NewPolygon.Points[1].x = x2 * 0.5;
									NewPolygon.Points[1].y = -y2 * 0.5;
									NewPolygon.Points[2].x = x2 * 0.5;
									NewPolygon.Points[2].y = y2 * 0.5;
									NewPolygon.Points[3].x = -x2 * 0.5;
									NewPolygon.Points[3].y = y2 * 0.5;

									if (Object->Mirror)
										MirrorPolygon(&NewPolygon, 0);

									RotatePolygon(&NewPolygon, 0.0, 0.0, Object->RotationAngle, 0);
									MovePolygon(&NewPolygon, Object->x1, Object->y1, 0);
									ObjectSize = sizeof(PadRecord);
									Pad.ShapeType = OBJECT_LINE;
									Pad.Special.Thickness = (float) Object->Thickness;
									Pad.Layer = Layer;

									Pad.X = (float) NewPolygon.Points[0].x;
									Pad.Y = (float) NewPolygon.Points[0].y;
									Pad.Width = (float) NewPolygon.Points[1].x;
									Pad.Height = (float) NewPolygon.Points[1].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;

									Pad.X = (float) NewPolygon.Points[1].x;
									Pad.Y = (float) NewPolygon.Points[1].y;
									Pad.Width = (float) NewPolygon.Points[2].x;
									Pad.Height = (float) NewPolygon.Points[2].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;

									Pad.X = (float) NewPolygon.Points[2].x;
									Pad.Y = (float) NewPolygon.Points[2].y;
									Pad.Width = (float) NewPolygon.Points[3].x;
									Pad.Height = (float) NewPolygon.Points[3].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;

									Pad.X = (float) NewPolygon.Points[3].x;
									Pad.Y = (float) NewPolygon.Points[3].y;
									Pad.Width = (float) NewPolygon.Points[0].x;
									Pad.Height = (float) NewPolygon.Points[0].y;

									if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
										return -1;

									BufPos = NewBufPos;
									Shape.NrOtherObjects += 4;
								}
							}
							else
								ok = 1;
						}

						break;

					default:
						ok = 1;
						break;
					}
				}
			}

			if ((GeomSaveOptions & 1) && (MakeObjectFromCompRef(Comp, &NewObject, 0) == 0))
			{
				TextP = (LPSTR) NewObject.TraceNr;
				Pad.ShapeType = OBJECT_TEXT;
				Pad.X = (float) NewObject.x1;
				Pad.Y = (float) NewObject.y1;
				Pad.Width = (float) NewObject.x2;

				if (NewObject.Mirror == 0)
				{
					Pad.Height = (float) (LimitRotation(NewObject.RotationAngle) + 2000.0);
					Pad.Layer = SILKSCREEN_BOTTOM;
				}
				else
				{
					Pad.Height = (float) (LimitRotation(NewObject.RotationAngle) + 4000.0);
					Pad.Layer = SILKSCREEN_TOP;
				}

				Pad.Special.Thickness = (float) NewObject.Thickness;
				memset(&ShapeLines[7], 0, 64);
				memcpy(&ShapeLines[7], TextP, min(strlen(TextP), 63));
				ObjectSize = 28;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				ObjectSize = 64;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[7], BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}

			if ((GeomSaveOptions & 2) && (MakeObjectFromCompValue(Comp, &NewObject, 0) == 0))
			{
				TextP = (LPSTR) NewObject.TraceNr;
				Pad.ShapeType = OBJECT_TEXT;
				Pad.X = (float) NewObject.x1;
				Pad.Y = (float) NewObject.y1;
				Pad.Width = (float) NewObject.x2;

				if (NewObject.Mirror == 0)
				{
					Pad.Height = (float) (LimitRotation(NewObject.RotationAngle) + 2000.0);
					Pad.Layer = SILKSCREEN_BOTTOM;
				}
				else
				{
					Pad.Height = (float) (LimitRotation(NewObject.RotationAngle) + 4000.0);
					Pad.Layer = SILKSCREEN_TOP;
				}

				Pad.Special.Thickness = (float) NewObject.Thickness;
				memset(&ShapeLines[7], 0, 64);
				memcpy(&ShapeLines[7], TextP, min(strlen(TextP), 63));
				ObjectSize = 28;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				ObjectSize = 64;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[7], BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaType = Via->ViaType & 3;

		if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			memset(&Pad, 0, sizeof(PadRecord));
			Pad.ShapeType = OBJECT_CIRCLE;
			Pad.X = Via->X;
			Pad.Y = Via->Y;

			if (GeomSaveOptions & 8)
			{
				if (Via->SoldMask > 0.0)
					x2 = Via->SoldMask;
				else
					x2 = Via->ThickNess;

				Pad.Width = (float) x2;
				Pad.Layer = SOLD_MASK_BOTTOM;
				ObjectSize = sizeof(PadRecord);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
				Pad.Layer = SOLD_MASK_TOP;

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}
			else
			{
				if ((ViaType == 0) && (Via->SoldMask > 0.0))
				{
					Pad.Width = Via->SoldMask;
					Pad.Layer = SOLD_MASK_BOTTOM;
					ObjectSize = sizeof(PadRecord);

					if (ViaType & VIA_SOLDMASK_BOTTOM)
					{
						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
					}

					if (ViaType & VIA_SOLDMASK_TOP)
					{
						Pad.Layer = SOLD_MASK_TOP;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
					}
				}
			}
		}
	}

// ****************************************************************************
// ****************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			LayerOk = 0;
			Layer = ObjectLine->Layer;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
				LayerOk = 1;
				break;

			case SILKSCREEN_TOP:
			case SILKSCREEN_BOTTOM:
				Layer ^= 1;
				LayerOk = 1;
				break;

			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) == 0)
					LayerOk = 1;

				break;

			default:
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				x1 = ObjectLine->X1;
				y1 = ObjectLine->Y1;
				x2 = ObjectLine->X2;
				y2 = ObjectLine->Y2;

				memset(&Pad, 0, sizeof(PadRecord));
				Pad.Layer = Layer;
				Pad.ShapeType = OBJECT_LINE;
				Pad.Special.Thickness = ObjectLine->LineThickNess;
				ObjectSize = sizeof(PadRecord);

				if (ObjectLine->LineMode != 0)
				{
					LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, ObjectLine->LineMode);
					SegmentCount = 0;

					for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
					{
						x3 = LineBuf[SegmentCount++];
						y3 = LineBuf[SegmentCount++];
						x4 = LineBuf[SegmentCount++];
						y4 = LineBuf[SegmentCount++];
						Pad.X = (float) x3;
						Pad.Y = (float) y3;
						Pad.Width = (float) x4;
						Pad.Height = (float) y4;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						Shape.NrOtherObjects++;
					}
				}
				else
				{
					Pad.X = ObjectLine->X1;
					Pad.Y = ObjectLine->Y1;
					Pad.Width = ObjectLine->X2;
					Pad.Height = ObjectLine->Y2;

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrOtherObjects++;
				}
			}
		}
	}

// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectRect->Layer;
			LayerOk = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
				LayerOk = 1;
				break;

			case SILKSCREEN_TOP:
			case SILKSCREEN_BOTTOM:
				Layer ^= 1;
				LayerOk = 1;
				break;

			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) == 0)
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
				Pad.X = ObjectRect->CentreX;
				Pad.Y = ObjectRect->CentreY;
				Pad.Width = ObjectRect->Width;
				Pad.Height = ObjectRect->Height;
				Pad.Layer = Layer;
				Pad.ShapeType = OBJECT_RECT;
				ObjectSize = sizeof(PadRecord);

				if (ObjectRect->Info & OBJECT_FILLED)
				{
					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrOtherObjects++;
				}
				else
				{
					Pad.Special.Thickness = ObjectRect->LineThickNess;
					Pad.ShapeType = OBJECT_LINE;
					x1 = ObjectRect->CentreX;
					y1 = ObjectRect->CentreY;
					x2 = ObjectRect->Width;
					y2 = ObjectRect->Height;
					Pad.Layer = Layer;

					Pad.X = (float) (x1 - x2 * 0.5);
					Pad.Y = (float) (y1 - y2 * 0.5);
					Pad.Width = (float) (x1 + x2 * 0.5);
					Pad.Height = (float) (y1 - y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					Pad.X = (float) (x1 + x2 * 0.5);
					Pad.Y = (float) (y1 - y2 * 0.5);
					Pad.Width = (float) (x1 + x2 * 0.5);
					Pad.Height = (float) (y1 + y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					Pad.X = (float) (x1 + x2 * 0.5);
					Pad.Y = (float) (y1 + y2 * 0.5);
					Pad.Width = (float) (x1 - x2 * 0.5);
					Pad.Height = (float) (y1 + y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;

					Pad.X = (float) (x1 - x2 * 0.5);
					Pad.Y = (float) (y1 + y2 * 0.5);
					Pad.Width = (float) (x1 - x2 * 0.5);
					Pad.Height = (float) (y1 - y2 * 0.5);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;

					BufPos = NewBufPos;
					Shape.NrOtherObjects++;
				}
			}
		}
	}

// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectArc->Layer;
#ifdef _DEBUG

			if ((InRange9(ObjectArc->CentreX, 167.6e5)) && (InRange9(ObjectArc->CentreY, -4.9e5)))
				ok = 1;

#endif
			LayerOk = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
				LayerOk = 1;
				break;

			case SILKSCREEN_TOP:
			case SILKSCREEN_BOTTOM:
				Layer ^= 1;
				LayerOk = 1;
				break;

			case DRILL_LAYER:
			case DRILL_UNPLATED_LAYER:
				LayerOk = 1;
				break;

			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) == 0)
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

				if (((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32)) || (Layer == DRILL_LAYER)
				        || (Layer == DRILL_UNPLATED_LAYER))
				{
					memset(&Pad, 0, sizeof(PadRecord));
					Pad.ShapeType = OBJECT_CIRCLE;

					if (Layer == DRILL_LAYER)
					{
						Pad.ShapeType = DRILL;
						Layer = -1;
					}
					else
					{
						if (Layer == DRILL_UNPLATED_LAYER)
						{
							Pad.ShapeType = DRILL_UNPLATED;
							Layer = -1;
						}
					}

					Pad.X = ObjectArc->CentreX;
					Pad.Y = ObjectArc->CentreY;
					Pad.Width = ObjectArc->Width;
					Pad.Height = ObjectArc->Width;
					Pad.Layer = Layer;
					ObjectSize = sizeof(PadRecord);

					if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
						return -1;
				}
				else
				{
					if (ObjectArc->Info & OBJECT_FILLED)
					{
						Pad.ShapeType = OBJECT_CIRCLE;
						Pad.X = ObjectArc->CentreX;
						Pad.Y = ObjectArc->CentreY;
						Pad.Width = ObjectArc->Width;
						Pad.Layer = Layer;
						ObjectSize = sizeof(PadRecord);

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;
					}
					else
					{
						Pad.ShapeType = OBJECT_ARC;
						Pad.Layer = Layer;
						ShapeLines[2] = ObjectArc->CentreX;
						ShapeLines[3] = ObjectArc->CentreY;
						ShapeLines[4] = ObjectArc->Width;
						ShapeLines[5] = ObjectArc->Height;
						ShapeLines[6] = ObjectArc->StartDiffX;
						ShapeLines[7] = ObjectArc->StartDiffY;
						ShapeLines[8] = ObjectArc->EndDiffX;
						ShapeLines[9] = ObjectArc->EndDiffY;
						ShapeLines[10] = ObjectArc->LineThickNess;
						ShapeLines[11] = 0.0;

						if ((ObjectArc->Info & OBJECT_FILLED) == 0)
							Pad.Special.Thickness = ObjectArc->LineThickNess;

						ObjectSize = 8;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
							return -1;

						BufPos = NewBufPos;
						ObjectSize = 40;

						if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[2], BufPos, ObjectSize)) == -1)
							return -1;
					}
				}

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}
		}
	}

// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(ObjectText2->Text, "info1_0") == 0)
				ok = 1;

#endif
			Layer = ObjectText2->Layer;
			LayerOk = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
				LayerOk = 1;
				break;

			case SILKSCREEN_TOP:
			case SILKSCREEN_BOTTOM:
				Layer ^= 1;
				LayerOk = 1;
				break;

			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) == 0)
					LayerOk = 1;

				break;

			default:
				break;
			}

			if (LayerOk)
			{
				memset(&Pad, 0, sizeof(PadRecord));
				Pad.Layer = Layer;
				Pad.ShapeType = OBJECT_TEXT;
				Mirror = (ObjectText2->TextMode & 0x10) >> 4;
				memset(&str, 0, sizeof(str));
				strncpy(str, ObjectText2->Text, 512);
				Length = strlen(str);
				Pad.Width = ObjectText2->FontHeight;
				Pad.Special.Thickness = ObjectText2->LineThickNess;
				Rotation2 = ObjectText2->Rotation;

				if (Mirror == 0)
					Pad.Height = (float) (ObjectText2->Rotation + 2000.0);
				else
					Pad.Height = (float) (ObjectText2->Rotation + 4000.0);

				x1 = ObjectText2->X;
				y1 = ObjectText2->Y;
				x2 = ObjectText2->FontHeight;
				cnt3 = 0;
				cnt4 = cnt3;

				while (cnt3 < Length + 1)
				{
					if ((str[cnt3] == '\r') || ((cnt3 == Length) && (str[cnt3 - 1] != '\n')))
					{
						if (cnt3 - cnt4 > 0)
						{
							memset(TextString, 0, sizeof(TextString));
							strncpy(TextString, (LPSTR) & str[cnt4], min(127, cnt3 - cnt4));
							Pad.X = (float) x1;
							Pad.Y = (float) y1;
							memset(&ShapeLines[7], 0, 64);
							memcpy(&ShapeLines[7], TextString, min(strlen(TextString), 63));
							ObjectSize = 28;

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							ObjectSize = 64;

							if ((NewBufPos = WriteToShapeBuf((uint8 *) & ShapeLines[7], BufPos, ObjectSize)) == -1)
								return -1;

							BufPos = NewBufPos;
							Shape.NrOtherObjects++;
						}

						if (Mirror == 0)
							x1 += sin(ANGLE_CONVERT(Rotation2)) * x2;
						else
							x1 -= sin(ANGLE_CONVERT(Rotation2)) * x2;

						y1 -= cos(ANGLE_CONVERT(Rotation2)) * x2;
						cnt3 += 1;
						cnt4 = cnt3 + 1;
					}

					cnt3++;
				}

			}
		}
	}

// **********************************************************************************

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Layer = ObjectPolygon->Layer;
			LayerOk = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM:
			case SOLD_MASK_TOP:
			case PASTE_MASK_BOTTOM:
			case PASTE_MASK_TOP:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
				LayerOk = 1;
				break;

			case SILKSCREEN_TOP:
			case SILKSCREEN_BOTTOM:
				Layer ^= 1;
				LayerOk = 1;
				break;

			case BOARD_OUTLINE_LAYER:
				if ((GeomSaveOptions & 4) == 0)
					LayerOk = 1;

				break;

			default:
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				NewGeomPolygon = AddObjectPolygonAsGeomPolygon(ObjectPolygon, &OffsetX, &OffsetY);
				memset(&Pad, 0, sizeof(PadRecord));
				Pad.X = (float) -OffsetX;
				Pad.Y = (float) -OffsetY;
				Pad.ShapeType = OBJECT_POLYGON;
				Pad.Layer = Layer;
				ObjectSize = sizeof(PadRecord);
				NewGeomPolygon->ShapePolygonPosition = BufPos + offsetof(PadRecord, Special.AddressOffset);

				if ((NewBufPos = WriteToShapeBuf((uint8 *) & Pad, BufPos, ObjectSize)) == -1)
					return -1;

				BufPos = NewBufPos;
				Shape.NrOtherObjects++;
			}
		}
	}


// **********************************************************************************
// **********************************************************************************
	Shape.PolygonOffset = BufPos;
	// Write the polygon shapes
	cnt2 = MaxTempMemory3;

	if (AllocateMemTemp3(MaxTempMemory3 + (NrGeomPolygons + 16) * sizeof(int32)) != 0)
		return -1;

	MemSizeObjectPolygons = (int32 *) & TempMem3[cnt2];
	cnt2 = 0;

// MaxGeomPolygons; NrGeomPolygons,MaxGeomPolygonsMem; GeomPolygonsMemSize;

	MemSizeObjectPolygons[cnt2++] = 0;

	for (cnt = 0; cnt < NrGeomPolygons; cnt++)
	{
		GeomPolygon = (GeomPolygonRecord *) & (GeomPolygonsMem[(*GeomPolygons)[cnt]]);
		ObjectSize = sizeof(GeomPolygonInitRecord) + GeomPolygon->NrVertices * sizeof(PointRecord);
		MemSizeObjectPolygons[cnt2] = MemSizeObjectPolygons[cnt2 - 1] + ObjectSize;
		cnt2++;

		if ((NewBufPos = WriteToShapeBuf((uint8 *) GeomPolygon, BufPos, ObjectSize)) == -1)
			return -1;

		BufPos = NewBufPos;
	}

	Shape.NrPolygons = NrGeomPolygons;

	// Fill in the address pointer to the polygon objects
	for (cnt = 0; cnt < NrGeomPolygons; cnt++)
	{
		GeomPolygon = (GeomPolygonRecord *) & (GeomPolygonsMem[(*GeomPolygons)[cnt]]);
		cnt2 = GeomPolygon->ShapePolygonPosition;
#ifdef _DEBUG

		if (cnt2 == 185960)
			ok = 1;

#endif
		PolygonPos = Shape.PolygonOffset + MemSizeObjectPolygons[GeomPolygon->PolygonNr];
//      CheckObjectPolygon=(ObjectPolygonRecord *)&(ObjectPolygonMem[(*ObjectPolygons)[cnt3]]);
		Int32P = (int32 *) & TempMem[cnt2];
		memcpy(&TempMem[cnt2], &PolygonPos, 4);
	}

	FindMinMaxBoard(&minx, &miny, &maxx, &maxy, 1);
	Shape.ShapeNameHeight = (float) (60 * 2540.0);
	Shape.ShapeNameOriginX = (float) minx;
	Shape.ShapeNameOriginY = (float) (miny - Shape.ShapeNameHeight);
	Shape.ShapeNameRotation = 0;
	Shape.MemSize = BufPos;
	Shape.NrLayers = Design.NrBoardLayers;
	StrP = strrchr(EditFile, '\\');
	strcpy(str2, EditFile);
	StrP = strrchr(str2, '.');

	if (StrP)
		*StrP = 0;

	sprintf(str, "%s.shp", str2);
	str2[sizeof(Shape.ShapeName) - 1] = 0;
	strcpy(Shape.ShapeName, str2);

	if (SpecialPolygon == 0)
		strcpy((LPSTR) & Shape, ShapeCode2);
	else
		strcpy((LPSTR) & Shape, ShapeCode3);

	memcpy(TempMem, &Shape, sizeof(ShapeRecord));

	if ((fp = CheckForWritingAndOpen(str, BufPos, PCBWindow)) < 1)
	{
//    ShowFileAndLine(__FILE__,__LINE__);
		return -1;
	}

	if (FileWrite(fp, TempMem, BufPos, &Written) == -1)
	{
//    ShowFileAndLine(__FILE__,__LINE__);
		return -1;
	}

	FileClose(fp);

	DeAllocateMemTemp();
	DeAllocateMemTemp2();
	DeAllocateMemTemp3();
	SetNormalCursor();
	return 0;
}
