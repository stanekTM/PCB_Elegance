/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: polygon.c
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
#include "calcdef.h"
#include "commdlg.h"
#include "dialogs.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "files2.h"
#include "graphics.h"
#include "help.h"
#include "insdel.h"
#include "keyswin.h"
#include "line2.h"
#include "mainloop.h"
#include "math.h"
#include "memory.h"
#include "menus.h"
#include "polygon.h"
#include "rect.h"
#include "resource.h"
#include "select.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "time.h"
#include "toets.h"
#include "windows.h"
#include "geom.h"
#include "edit.h"
#include "utf8.h"


#define MaxNrCrosses           8

typedef struct
{
	int32 LineNrPolygonA, LineNrPolygonB, Test;
	double CrossX, CrossY;
} CrossPointRecord;

typedef struct
{
	PointRecord CrossPoints[4];
	int32 LineNrPolygonB1, LineNrPolygonA2;
	int32 LineNrPolygonB3, LineNrPolygonA4;
	int32 Count1, Count2, Count3, Count4, Mode;
} VerticeCopyRecord;

typedef struct
{
	int32 LineNrPolygonA, LineNrPolygonB, Test;
	double CrossX, CrossY;
} CrossPoint2Record;

typedef struct
{
	PointRecord CrossPoints[MaxNrCrosses];
	int32 LineNrPolygon[MaxNrCrosses], Count[MaxNrCrosses], Mode[MaxNrCrosses];
} VerticeCopy2Record;

typedef struct
{
	PointRecord CrossPoint;
	double Distance;
	int32 Pos, Test;
} DistanceOnSameLineRecord;

CrossPointRecord CrossPoints[MaxNrCrosses];

PolygonRecord *CurrentPolygon;

int32 ExtraDeletionPolygonPos[32], NrExtraDeletionPolygons, ExtraDeletionPolygonsMemSize, GlobalNrCrosses, StartCnt, ok,
      FinishPolygon, TestDraw, PolygonObjectType, ObjectNr, NrCrossPoints, PowerPlaneNetNr, StartPolygon;
VerticeCopyRecord VerticeCopy;
VerticeCopy2Record VerticeCopy2;
// Realloc GlobalAlloc

double DrawPolygonLineThickNess;
static double LineX1, LineY1, LineX2, LineY2, FirstX, FirstY;
static double CurrentX2, CurrentY2;
//uint8   PolygonBuf2[10000];
DistanceOnSameLineRecord DistanceOnSameLine[MaxNrCrosses];

double OldValue = 10000;
double OldValue2 = 0.1;




// *******************************************************************************************************
// *******************************************************************************************************

extern HDC OutputDisplay;
extern HGDIOBJ SpecialPen, SavePen, SaveBrush, EmptyBrush;
extern int32 AreafillDrawMode, SelectColorMode;
extern double LineCrossX, LineCrossY;

// *******************************************************************************************************
// *******************************************************************************************************

void CopyPolygonToPolygon(PolygonRecord * SrcPolygon, PolygonRecord * DestPolygon);

int32 MergePolygon(PolygonRecord * PolygonObject, int32 Mode);


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double GetNewRandomValue(int32 mode)
{
	double RandValue;

	while (((RandValue = ((rand() / RAND_MAX))) < 0.1) || (RandValue - OldValue2 < 0.0005)
	        || (RandValue - OldValue2 > 0.0015));

	OldValue2 = RandValue;
	return RandValue;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SetMinMaxObjectPolygon(ObjectPolygonRecord * ObjectPolygon, int32 mode)
{
	int32 count, cnt, cnt2;
	double x1, y1, minx, maxx, miny, maxy;
	int32 ObjectPolygonLength;
	uint8 *Buf;
	ObjectSubPolygonRecord *ObjectSubPolygon;

	count = ObjectPolygon->NrVertices;

	minx = 10000.0e5;
	miny = 10000.0e5;
	maxx = -10000.0e5;
	maxy = -10000.0e5;
	ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

	if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
		count = ObjectPolygon->NrVerticesMainPolygon;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*ObjectPolygon).Points[cnt].x;
		y1 = (*ObjectPolygon).Points[cnt].y;
		minx = min(minx, x1);
		miny = min(miny, y1);
		maxx = max(maxx, x1);
		maxy = max(maxy, y1);
	}

	ObjectPolygon->minx = minx;
	ObjectPolygon->miny = miny;
	ObjectPolygon->maxx = maxx;
	ObjectPolygon->maxy = maxy;

	if (ObjectPolygon->NrSubPolygons > 0)
	{
		Buf = (uint8 *) ObjectPolygon;
		Buf += sizeof(ObjectPolygonInitRecord);
		Buf += count * sizeof(PointRecord);

		for (cnt2 = 0; cnt2 < ObjectPolygon->NrSubPolygons; cnt2++)
		{
			if ((Buf - (uint8 *) ObjectPolygon) >= (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
				break;

			ObjectSubPolygon = (ObjectSubPolygonRecord *) Buf;

			if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
				break;

			count = ObjectSubPolygon->NrVertices;

			if (count >= 65536)
				break;

			minx = 10000.0e5;
			miny = 10000.0e5;
			maxx = -10000.0e5;
			maxy = -10000.0e5;

			for (cnt = 0; cnt < count; cnt++)
			{
				x1 = (*ObjectSubPolygon).Points[cnt].x;
				y1 = (*ObjectSubPolygon).Points[cnt].y;
				minx = min(minx, x1);
				miny = min(miny, y1);
				maxx = max(maxx, x1);
				maxy = max(maxy, y1);
			}

			ObjectSubPolygon->minx = minx;
			ObjectSubPolygon->miny = miny;
			ObjectSubPolygon->maxx = maxx;
			ObjectSubPolygon->maxy = maxy;
			Buf += sizeof(ObjectSubPolygonInitRecord);
			Buf += count * sizeof(PointRecord);
		}
	}


	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MemSizeObjectPolygon(ObjectPolygonRecord * ObjectPolygon)
{
	int32 count;
	count = sizeof(ObjectPolygonInitRecord);
	count += ObjectPolygon->NrVertices * sizeof(PointRecord);
	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MemSizePolygon(PolygonRecord * ObjectPolygon)
{
	int32 count;
	count = sizeof(PolygonInitRecord);
	count += ObjectPolygon->NrVertices * sizeof(PointRecord);
	return count;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OffsetX, double OffsetY, int32 mode)
{
	int32 count, cnt, cnt2, cnt3, ObjectPolygonLength;
	uint8 *Buf;
	ObjectSubPolygonRecord *ObjectSubPolygon;

	count = ObjectPolygon->NrVertices;
	ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

	if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
		count = ObjectPolygon->NrVerticesMainPolygon;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		(*ObjectPolygon).Points[cnt3].x += OffsetX;
		(*ObjectPolygon).Points[cnt3].y += OffsetY;
	}

	(*ObjectPolygon).minx += OffsetX;
	(*ObjectPolygon).miny += OffsetY;
	(*ObjectPolygon).maxx += OffsetX;
	(*ObjectPolygon).maxy += OffsetY;

	if (ObjectPolygon->NrSubPolygons > 0)
	{
		Buf = (uint8 *) ObjectPolygon;
		Buf += sizeof(ObjectPolygonInitRecord);
		Buf += count * sizeof(PointRecord);

		for (cnt2 = 0; cnt2 < ObjectPolygon->NrSubPolygons; cnt2++)
		{
			if ((Buf - (uint8 *) ObjectPolygon) >= (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
				break;

			ObjectSubPolygon = (ObjectSubPolygonRecord *) Buf;

			if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
				break;

			count = ObjectSubPolygon->NrVertices;

			if (count >= 65536)
				break;

			for (cnt = 0; cnt < count; cnt++)
			{
				(*ObjectSubPolygon).Points[cnt].x += OffsetX;
				(*ObjectSubPolygon).Points[cnt].y += OffsetY;
			}

			(*ObjectSubPolygon).minx += OffsetX;
			(*ObjectSubPolygon).miny += OffsetY;
			(*ObjectSubPolygon).maxx += OffsetX;
			(*ObjectSubPolygon).maxy += OffsetY;
			Buf += sizeof(ObjectSubPolygonInitRecord);
			Buf += count * sizeof(PointRecord);
		}
	}

}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PointInObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double px, double py)
{
	double x1, y1, x2, y2, fx, fy, *P, px2, py2;
	int32 cnt, res, crosses, NrVertices;

	x2 = 0.0;
	y2 = 0.0;
	crosses = 0;
	px2 = px - 0.0000015;
	py2 = py + 10000.0e5;
	NrVertices = ObjectPolygon->NrVertices;

	if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
		NrVertices = ObjectPolygon->NrVerticesMainPolygon;

	P = (double *) &(ObjectPolygon->Points);
	fx = *(P++);
	fy = *(P++);
	cnt = 0;

	while (cnt < NrVertices)
	{
		if (cnt > 0)
		{
			if (cnt < NrVertices - 1)
			{
				x1 = x2;
				y1 = y2;
				x2 = *(P++);
				y2 = *(P++);
			}
			else
			{
				x1 = x2;
				y1 = y2;
				x2 = fx;
				y2 = fy;
			}
		}
		else
		{
			x1 = fx;
			y1 = fy;
			x2 = *(P++);
			y2 = *(P++);
		}

		res = LineCrosses(px2, py, px2, py2, x1, y1, x2, y2);

		if (res == 1)
			crosses++;

		cnt++;
	}

	return crosses;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectPolygonInSearchArea(ObjectPolygonRecord * ObjectPolygon)
{
	int32 count;
	PolygonRecord SearchPolygon, *Polygon;

	memset(&SearchPolygon, 0, sizeof(SearchPolygon));
	memset(NewObjectPolygon, 0, sizeof(NewPolygon));

	SearchPolygon.NrVertices = 4;
	SearchPolygon.Points[0].x = SearchMinX;
	SearchPolygon.Points[0].y = SearchMinY;
	SearchPolygon.Points[1].x = SearchMinX;
	SearchPolygon.Points[1].y = SearchMaxY;
	SearchPolygon.Points[2].x = SearchMaxX;
	SearchPolygon.Points[2].y = SearchMaxY;
	SearchPolygon.Points[3].x = SearchMaxX;
	SearchPolygon.Points[3].y = SearchMinY;
	SearchPolygon.minx = SearchMinX;
	SearchPolygon.miny = SearchMinY;
	SearchPolygon.maxx = SearchMaxX;
	SearchPolygon.maxy = SearchMaxY;

	count = ObjectPolygon->NrVertices;

	if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
		count = ObjectPolygon->NrVerticesMainPolygon;

	AllocateMemTemp(count * sizeof(PointRecord) + 16384);
	Polygon = (PolygonRecord *) TempMem;
	memset(Polygon, 0, sizeof(PolygonInitRecord));
	Polygon->NrVertices = count;
	memcpy(Polygon->Points, ObjectPolygon->Points, Polygon->NrVertices * sizeof(PointRecord));
	Polygon->minx = ObjectPolygon->minx;
	Polygon->miny = ObjectPolygon->miny;
	Polygon->maxx = ObjectPolygon->maxx;
	Polygon->maxy = ObjectPolygon->maxy;

	if (CheckPolygonCompleetlyOutsidePolygon(Polygon, &SearchPolygon) != 1)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygonCompleetlyInsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon)
{
	int32 count, cnt, res, cnt2, count2;
	double *x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4, *FirstX, *FirstY, *FirstX2, *FirstY2;

	if ((PolygonObject->maxx + 100 < BigPolygon->minx) || (PolygonObject->minx - 100 > BigPolygon->maxx)
	        || (PolygonObject->maxy + 100 < BigPolygon->miny) || (PolygonObject->miny - 100 > BigPolygon->maxy))
		return -2;

	count = PolygonObject->NrVertices;
	count2 = BigPolygon->NrVertices;
	x1 = (double *) &((*PolygonObject).Points);
	y1 = x1 + 1;
	FirstX = x1;
	FirstY = y1;

	for (cnt = 0; cnt < count; cnt++)
	{
		res = PointInPolygon(BigPolygon, *x1, *y1);

		if ((res & 1) == 0)
		{
			return -2;			// Nr crosses is even, -> One or more points outside bigpolygon
//                   polygon is not inside the bigpolygon
		}

		x1 += 2;
		y1 += 2;
	}

// All polygon points are inside the big polygon
	x1 = FirstX;
	y1 = FirstY;

	for (cnt = 0; cnt < count; cnt++)
	{
		if (cnt < count - 1)
		{
			x2 = x1 + 2;
			y2 = y1 + 2;
		}
		else
		{
			x2 = FirstX;
			y2 = FirstY;
		}

		x3 = (double *) &((*BigPolygon).Points);
		y3 = x3 + 1;
		FirstX2 = x3;
		FirstY2 = y3;

		for (cnt2 = 0; cnt2 < count2; cnt2++)
		{
			if (cnt2 < count2 - 1)
			{
				x4 = x3 + 2;
				y4 = y3 + 2;
			}
			else
			{
				x4 = FirstX2;
				y4 = FirstY2;
			}

			res = LineCrosses(*x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4);

			if (res == 1)
			{
				return -1;		// Polygon lines are crossing
//                     polygon is not completly inside the bigpolygon
			}

			x3 += 2;
			y3 += 2;
		}

		x1 += 2;
		y1 += 2;
	}

// All polygon points are inside the big polygon, and there are no crossings in the
// polygons lines -> polygon is completly inside the bigpolygon
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygonCompleetlyOutsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon)
{
	int32 count, cnt, res, cnt2, count2;
	double *x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4, *FirstX, *FirstY, *FirstX2, *FirstY2;

	count = PolygonObject->NrVertices;
	count2 = BigPolygon->NrVertices;

	x1 = (double *) &((*BigPolygon).Points);
	y1 = x1 + 1;
	res = PointInPolygon(PolygonObject, *x1, *y1);

	if ((res & 1) == 1)
	{
		return -2;				// Nr crosses is odd, -> The first point bigpolygon
//                 is not outside the polygon
	}

	x1 = (double *) &((*PolygonObject).Points);
	y1 = x1 + 1;
	FirstX = x1;
	FirstY = y1;

	for (cnt = 0; cnt < count; cnt++)
	{
		res = PointInPolygon(BigPolygon, *x1, *y1);

		if ((res & 1) == 1)
		{
			return -2;			// Nr crosses is odd, -> One or more points inside polygon
//                   polygon is not outside the bigpolygon
		}

		x1 += 2;
		y1 += 2;
	}

	x1 = FirstX;
	y1 = FirstY;

	for (cnt = 0; cnt < count; cnt++)
	{
		if (cnt < count - 1)
		{
			x2 = x1 + 2;
			y2 = y1 + 2;
		}
		else
		{
			x2 = FirstX;
			y2 = FirstY;
		}

		x3 = (double *) &((*BigPolygon).Points);
		y3 = x3 + 1;
		FirstX2 = x3;
		FirstY2 = y3;

		for (cnt2 = 0; cnt2 < count2; cnt2++)
		{
			if (cnt2 < count2 - 1)
			{
				x4 = x3 + 2;
				y4 = y3 + 2;
			}
			else
			{
				x4 = FirstX2;
				y4 = FirstY2;
			}

			res = LineCrosses(*x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4);

			if (res == 1)
			{
				return -1;		// Polygon lines are crossing
//                     polygon is not completly outside the bigpolygon
			}

			x3 += 2;
			y3 += 2;
		}

		x1 += 2;
		y1 += 2;
	}

// All polygon points are outside the big polygon, and there are no crossings in the
// polygons lines -> polygon is completly outside the bigpolygon
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckNoCrossesInPolygon(PolygonRecord * Polygon)
{
	int32 cnt, cnt2, cnt3, count;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	count = Polygon->NrVertices;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*Polygon).Points[cnt].x;
		y1 = (*Polygon).Points[cnt].y;

		if (cnt < count - 1)
		{
			x2 = (*Polygon).Points[cnt + 1].x;
			y2 = (*Polygon).Points[cnt + 1].y;
		}
		else
		{
			x2 = (*Polygon).Points[0].x;
			y2 = (*Polygon).Points[0].y;
		}

		for (cnt2 = cnt + 2; cnt2 < cnt + count - 1; cnt2++)
		{
			cnt3 = cnt2;

			if (cnt3 >= count)
				cnt3 -= count;

			x3 = (*Polygon).Points[cnt3].x;
			y3 = (*Polygon).Points[cnt3].y;
			cnt3 = cnt2 + 1;

			if (cnt3 >= count)
				cnt3 -= count;

			x4 = (*Polygon).Points[cnt3].x;
			y4 = (*Polygon).Points[cnt3].y;

			if (LineCrosses(x1, y1, x2, y2, x3, y3, x4, y4) == 1)
				return 0;
		}
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygonCrossesOtherPolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon)
{
	int32 count, cnt, res, cnt2, count2;
	double *x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4, *FirstX, *FirstY, *FirstX2, *FirstY2;

	count = PolygonObject->NrVertices;
	count2 = BigPolygon->NrVertices;
	x1 = (double *) &((*PolygonObject).Points);
	y1 = x1 + 1;
	FirstX = x1;
	FirstY = y1;

	for (cnt = 0; cnt < count; cnt++)
	{
		if (cnt < count - 1)
		{
			x2 = x1 + 2;
			y2 = y1 + 2;
		}
		else
		{
			x2 = FirstX;
			y2 = FirstY;
		}

		x3 = (double *) &((*BigPolygon).Points);
		y3 = x3 + 1;
		FirstX2 = x3;
		FirstY2 = y3;

		for (cnt2 = 0; cnt2 < count2; cnt2++)
		{
			if (cnt2 < count2 - 1)
			{
				x4 = x3 + 2;
				y4 = y3 + 2;
			}
			else
			{
				x4 = FirstX2;
				y4 = FirstY2;
			}

			res = LineCrosses(*x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4);

			if (res == 1)
				return -1;

			x3 += 2;
			y3 += 2;
		}

		x1 += 2;
		y1 += 2;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckMappableObjectPolygon(int32 ObjectPolygonNr, int32 mode)
{
	int32 count, count2, cnt, cnt2, ok, PolygonMappable;
	double divx, divy, divx2, divy2, cx, cy;
	ObjectPolygonRecord *CheckObjectPolygon, *ObjectPolygon;

	/*
	NrMappableObjectPolygons,
	MappableObjectPolygonStart,
	*/
	divx = 0.0;
	divy = 0.0;
	ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[ObjectPolygonNr]]);
	count = ObjectPolygon->NrVertices;

	if (count < 3)
		return -1;

	if ((ObjectPolygon->NrSubPolygons == 0) && (count < 40))
	{
		PolygonMappable = 0;
		cnt = 0;

		while ((cnt < NrMappableObjectPolygons) && (!PolygonMappable))
		{
			CheckObjectPolygon =
			    (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt + MappableObjectPolygonStart]]);

			if (CheckObjectPolygon->NrVertices == count)
			{
				divx = CheckObjectPolygon->Points[0].x - ObjectPolygon->Points[0].x;
				divy = CheckObjectPolygon->Points[0].y - ObjectPolygon->Points[0].y;
				PolygonMappable = 1;
				count2 = ObjectPolygon->NrVertices;

				if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
					count2 = ObjectPolygon->NrVerticesMainPolygon;

				for (cnt2 = 1; cnt2 < count2; cnt2++)
				{
					divx2 = CheckObjectPolygon->Points[cnt2].x - ObjectPolygon->Points[cnt2].x;
					divy2 = CheckObjectPolygon->Points[cnt2].y - ObjectPolygon->Points[cnt2].y;

					if ((NotInRange2(divx, divx2)) || (NotInRange2(divy, divy2)))
						PolygonMappable = 0;
				}
			}

			if (PolygonMappable == 0)
				cnt++;
		}

		PolygonMappable = 0;

		if (PolygonMappable)
		{
			// Found a ObjectPolygon with the same shape
			ObjectPolygon->PolygonNr = cnt;
			ObjectPolygon->OffsetX = -divx;
			ObjectPolygon->OffsetY = -divy;
			return cnt;
		}
	}
	else
		ok = 1;

	// Did not found a ObjectPolygon with the same shape
	// Add this ObjectPolygon to the mappable ObjectPolygons

	if (!AddObjectPolygon2(ObjectPolygonNr))
		return -1;

	NrMappableObjectPolygons++;
	ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[ObjectPolygonNr]]);
	cnt = MappableObjectPolygonStart + NrMappableObjectPolygons - 1;
	CheckObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
	CheckObjectPolygon->AddNr = 0;
	CheckObjectPolygon->DeleteNr = 0;
	CheckObjectPolygon->Info = OBJECT_NOT_VISIBLE;
	ObjectPolygon->PolygonNr = NrMappableObjectPolygons - 1;

	if ((ObjectPolygon->NrSubPolygons == 0) && (count < 40))
	{
		cx = (ObjectPolygon->maxx + ObjectPolygon->minx) * 0.5;
		cy = (ObjectPolygon->maxy + ObjectPolygon->miny) * 0.5;
		divx = CheckObjectPolygon->Points[0].x - cx - ObjectPolygon->Points[0].x;
		divy = CheckObjectPolygon->Points[0].y - cy - ObjectPolygon->Points[0].y;
	}
	else
	{
		cx = CheckObjectPolygon->Points[0].x;
		cy = CheckObjectPolygon->Points[0].y;
		divx = -cx;
		divy = -cy;
	}

	MoveObjectPolygon(CheckObjectPolygon, -cx, -cy, 0);
	ObjectPolygon->OffsetX = -divx;
	ObjectPolygon->OffsetY = -divy;
	return NrMappableObjectPolygons - 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ViewVerticesObjectPolygon(int32 mode)
{
	int32 cnt, count;
	double x1a, y1a;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	ObjectPolygonRecord *ObjectPolygon, *FoundPolygon;

	FoundPolygon = NULL;

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (FoundPolygon == NULL)
				FoundPolygon = ObjectPolygon;
		}
	}

	if (FoundPolygon == NULL)
		return -1;

	count = FoundPolygon->NrVertices;

	if ((FoundPolygon->NrSubPolygons > 0) && (FoundPolygon->NrVerticesMainPolygon > 0))
		count = FoundPolygon->NrVerticesMainPolygon;

	sprintf(str3, "Polygon: %i  Vertices", count);
	str2[0] = 0;
	MessageBufPos = 0;

	for (cnt = 0; cnt < min(3000, count); cnt++)
	{
		x1a = FoundPolygon->Points[cnt].x;
		y1a = FoundPolygon->Points[cnt].y;

		if (Units == 0)
			sprintf(str, "%.1f,%.1f,", x1a / 2540.0, y1a / 2540.0);
		else
			sprintf(str, "%.4f,%.4f,", x1a / 100000.0, y1a / 100000.0);

		strcat(str2, str);

		if ((cnt % 5) == 4)
		{
			if (AddToMessageBuf(str2) != 0)
				return 0;

			str2[0] = 0;
		}
	}

	if (str2[0] != 0)
	{
		if (AddToMessageBuf(str2) != 0)
			return 0;
	}

	if (count > 1000)
	{
		if (AddToMessageBuf(".... (Only the first 3000 vertices are listed)\r\n") != 0)
			return 0;
	}


	MessageDialog(str3, 0);
	DeAllocateMemMessageBuf();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetBiggerSmallerPoints(double x1, double y1, double x2, double y2, double x3, double y3, double OldX, double OldY,
                             double *NewX1, double *NewY1, double *NewX2, double *NewY2, double *OldX2, double *OldY2,
                             double ThickNess, int32 mode)
{
	double a1, a2, b1, b2, b1a, b2a, AddNum, xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4, diva, suma, OldXA, OldYA, x2a, y2a,
	       x2b, y2b, x3a, y3a, temp, x1a, y1a, x1aa, y1aa;
	int32 Vert12, Vert23, c1, c2;
#ifdef _DEBUG
	char str[MAX_LENGTH_STRING];
#endif
//  ThickNess+=0.001;

	a1 = 0.0;
	a2 = 0.0;

#ifdef _DEBUG

	if (0)
	{
		sprintf(str, "  res=GetBiggerSmallerPoints(%.7f,%.7f,\n", x1, y1);
		OutputDebugString(str);
		sprintf(str, "                             %.7f,%.7f,\n", x2, y2);
		OutputDebugString(str);
		sprintf(str, "                             %.7f,%.7f,\n", x3, y3);
		OutputDebugString(str);
		sprintf(str, "                             %.7f,%.7f,\n", OldX, OldY);
		OutputDebugString(str);
		sprintf(str, "                             &x1a,&y1a,\n");
		OutputDebugString(str);
		sprintf(str, "                             &x2a,&y2a,\n");
		OutputDebugString(str);
		sprintf(str, "                             &OldX2,&OldY2,\n");
		OutputDebugString(str);
		sprintf(str, "                             %.7f,3);", ThickNess);
		OutputDebugString(str);
	}

#endif
	Vert12 = 0;
	Vert23 = 0;

	if (x1 != x2)
	{
		a1 = (y2 - y1) / (x2 - x1);

		if ((a1 < -10000) || (a1 > 10000))
			Vert12 = 1;
	}
	else
		Vert12 = 1;

	if (Vert12)
	{
		x1a = x1;
		x2a = x1;

		if (y1 > y2)
		{
			y1a = 10000.0e5;
			y2a = -10000.0e5;
		}
		else
		{
			y1a = -10000.0e5;
			y2a = 10000.0e5;
		}
	}
	else
	{
		if (x1 > x2)
		{
			x1a = 10000.0e5;
			x2a = -10000.0e5;
		}
		else
		{
			x1a = -10000.0e5;
			x2a = 10000.0e5;
		}

		y1a = (x1a - x1) * a1 + y1;
		y2a = (x2a - x1) * a1 + y1;
	}

	if (x2 != x3)
	{
		a2 = (y3 - y2) / (x3 - x2);

		if ((a2 < -10000) || (a2 > 10000))
			Vert23 = 1;
	}
	else
		Vert23 = 1;

	if (Vert23)
	{
		x3a = x3;
		x2b = x3;

		if (y3 > y2)
		{
			y3a = 10000.0e5;
			y2b = -10000.0e5;
		}
		else
		{
			y3a = -10000.0e5;
			y2b = 10000.0e5;
		}
	}
	else
	{
		if (x3 > x2)
		{
			x3a = 10000.0e5;
			x2b = -10000.0e5;
		}
		else
		{
			x3a = -10000.0e5;
			x2b = 10000.0e5;
		}

		y3a = (x3a - x2) * a2 + y2;
		y2b = (x2b - x2) * a2 + y2;
	}

	ThickNess *= 0.5;
	AddNum = 1000.0e5;

	if (Vert12)
	{
		OldXA = OldX;

		if (y2 > y1)
			OldYA = OldY - AddNum;
		else
			OldYA = OldY + AddNum;
	}
	else
	{
		if (fabs(a1) < 1)
		{
			if (x1 > x2)
				OldXA = OldX + AddNum;
			else
				OldXA = OldX - AddNum;

			OldYA = (OldXA - OldX) * a1 + OldY;
		}
		else
		{
			if (y1 > y2)
				OldYA = OldY + AddNum;
			else
				OldYA = OldY - AddNum;

			OldXA = (OldYA - OldY) / a1 + OldX;
		}

		/*
		    OldXA=OldX;
		    OldYA=OldY;
		*/
	}

	*OldX2 = OldXA;
	*OldY2 = OldYA;

	if (Vert12)
	{
		if (Vert23)
		{
// **************************************************************************************
// **************************************************************************************
			xx1 = x1 - ThickNess;
			xx2 = x1 + ThickNess;
			yy1 = y2;
			yy2 = y2;
			*NewX1 = xx1;
			*NewY1 = y2;
			*NewX2 = xx2;
			*NewY2 = y2;
		}
		else
		{
// **************************************************************************************
// **************************************************************************************
			b2 = ThickNess * sqrt(a2 * a2 + 1);
			xx1 = x2 - ThickNess;
			xx2 = x2 - ThickNess;
			xx3 = x2 + ThickNess;
			xx4 = x2 + ThickNess;
			yy1 = -ThickNess * a2 + y2 + b2;
			yy2 = -ThickNess * a2 + y2 - b2;
			yy3 = ThickNess * a2 + y2 + b2;
			yy4 = ThickNess * a2 + y2 - b2;
			*NewX1 = xx1;

			if (LineCrosses(x2, y2, x3a, y3a, xx1, yy1, xx2, yy2) == 1)
			{
				if (fabs(yy1 - OldYA) < fabs(yy2 - OldYA))
				{	// Shortest length y1 -> (yy1,yy2)
					*NewY1 = yy1;
				}
				else
					*NewY1 = yy2;
			}
			else
			{	// Largest length y1 -> (yy1,yy2)
				if (fabs(yy1 - OldYA) < fabs(yy2 - OldYA))
					*NewY1 = yy2;
				else
					*NewY1 = yy1;
			}

			*NewX2 = xx3;

			if (LineCrosses(x2, y2, x3a, y3a, xx3, yy3, xx4, yy4) == 1)
			{
				if (fabs(yy3 - OldYA) < fabs(yy4 - OldYA))
					*NewY2 = yy3;
				else
					*NewY2 = yy4;
			}
			else
			{	// Largest length y1 -> (yy1,yy2)
				if (fabs(yy3 - OldYA) < fabs(yy4 - OldYA))
					*NewY2 = yy4;
				else
					*NewY2 = yy3;
			}
		}
	}
	else
	{
		if (Vert23)
		{
// **************************************************************************************
// **************************************************************************************
			b1 = ThickNess * sqrt(a1 * a1 + 1);
			xx1 = x2 - ThickNess;
			xx2 = x2 + ThickNess;
			xx3 = x2 - ThickNess;
			xx4 = x2 + ThickNess;
			yy1 = -ThickNess * a1 + y2 + b1;
			yy2 = ThickNess * a1 + y2 + b1;
			yy3 = -ThickNess * a1 + y2 - b1;
			yy4 = ThickNess * a1 + y2 - b1;

			if (LineCrosses(x2, y2, x3a, y3a, xx1, yy1, xx2, yy2) == 1)
			{	// Shortest length y1 -> (yy1,yy2)
				if (fabs(xx1 - OldXA) < fabs(xx2 - OldXA))
				{
					*NewX1 = xx1;
					*NewY1 = yy1;
				}
				else
				{
					*NewX1 = xx2;
					*NewY1 = yy2;
				}
			}
			else
			{	// Largest length y1 -> (yy1,yy2)
				if (fabs(xx1 - OldXA) < fabs(xx2 - OldXA))
				{
					*NewX1 = xx2;
					*NewY1 = yy2;
				}
				else
				{
					*NewX1 = xx1;
					*NewY1 = yy1;
				}
			}

			if (LineCrosses(x2, y2, x3a, y3a, xx3, yy3, xx4, yy4) == 1)
			{	// Shortest length y1 -> (yy1,yy2)
				if (fabs(xx3 - OldXA) < fabs(xx4 - OldXA))
				{
					*NewX2 = xx3;
					*NewY2 = yy3;
				}
				else
				{
					*NewX2 = xx4;
					*NewY2 = yy4;
				}
			}
			else
			{	// Largest length y1 -> (yy1,yy2)
				if (fabs(xx3 - OldXA) < fabs(xx4 - OldXA))
				{
					*NewX2 = xx4;
					*NewY2 = yy4;
				}
				else
				{
					*NewX2 = xx3;
					*NewY2 = yy3;
				}
			}
		}
		else
		{
			if (InRangeRico(a1, a2))
			{
// **************************************************************************************
// **************************************************************************************
				if (y1 == y2)
				{
					xx1 = x2;
					xx2 = x2;
					yy1 = y2 + ThickNess;
					yy2 = y2 - ThickNess;
					*NewX1 = xx1;
					*NewY1 = yy1;
					*NewX2 = xx2;
					*NewY2 = yy2;
				}
				else
				{
// **************************************************************************************
// **************************************************************************************
					x1aa = ThickNess / (sqrt(1 + 1 / (a1 * a1)));
					y1aa = ThickNess / (sqrt(1 + a1 * a1));

					if (a1 < 0)
					{
						xx1 = x2 + x1aa;
						yy1 = y2 + y1aa;
						xx2 = x2 - x1aa;
						yy2 = y2 - y1aa;
					}
					else
					{
						xx1 = x2 + x1aa;
						yy1 = y2 - y1aa;
						xx2 = x2 - x1aa;
						yy2 = y2 + y1aa;
					}

					*NewX1 = xx1;
					*NewY1 = yy1;
					*NewX2 = xx2;
					*NewY2 = yy2;
				}
			}
			else
			{
// **************************************************************************************
// **************************************************************************************
				diva = 1 / (a2 - a1);
				suma = a1 + a2;
				temp = x2;
//        temp=(-x2*(a2-a1))*diva;
				b1 = ThickNess * sqrt(a1 * a1 + 1) * diva;
				b2 = ThickNess * sqrt(a2 * a2 + 1) * diva;
				b1a = ThickNess * sqrt(a1 * a1 + 1);
				b2a = ThickNess * sqrt(a2 * a2 + 1);
				xx1 = x2 + b2 + b1;
				xx2 = x2 - b2 + b1;
				xx3 = x2 + b2 - b1;
				xx4 = x2 - b2 - b1;

				yy1 = (xx1 - x2) * a1 + y2 + b1a;
				yy2 = (xx2 - x2) * a1 + y2 + b1a;
				yy3 = (xx3 - x2) * a1 + y2 - b1a;
				yy4 = (xx4 - x2) * a1 + y2 - b1a;

				if (LineCrosses(x2, y2, x3a, y3a, xx1, yy1, xx2, yy2) == 1)
				{
// Shortest length x1,y1 -> (xx1,yy1 or xx2,yy2)
					if (fabs(xx1 - OldXA) < fabs(xx2 - OldXA))
					{
						*NewX1 = xx1;
						*NewY1 = yy1;
					}
					else
					{
						*NewX1 = xx2;
						*NewY1 = yy2;
					}
				}
				else
				{
// Largest length x1,y1 -> (xx1,yy1 or xx2,yy2)
					if (fabs(xx1 - OldXA) > fabs(xx2 - OldXA))
					{
						*NewX1 = xx1;
						*NewY1 = yy1;
					}
					else
					{
						*NewX1 = xx2;
						*NewY1 = yy2;
					}
				}

				if (LineCrosses(x2, y2, x3a, y3a, xx3, yy3, xx4, yy4) == 1)
				{
// Shortest length x1,y1 -> (xx3,yy3 or xx4,yy4)
					if (fabs(xx3 - OldXA) > fabs(xx4 - OldXA))
					{
						*NewX2 = xx4;
						*NewY2 = yy4;
					}
					else
					{
						*NewX2 = xx3;
						*NewY2 = yy3;
					}
				}
				else
				{
// Largest length x1,y1 -> (xx3,yy3 or xx4,yy4)
					if (fabs(xx3 - OldXA) < fabs(xx4 - OldXA))
					{
						*NewX2 = xx4;
						*NewY2 = yy4;
					}
					else
					{
						*NewX2 = xx3;
						*NewY2 = yy3;
					}

				}

// **************************************************************************************
// **************************************************************************************
			}

		}
	}

	c1 = LineCrosses(x1a, y1a, x2a, y2a, OldX, OldY, *NewX1, *NewY1);

	if (c1 != 1)
		c1 = 0;

	c2 = LineCrosses(x1a, y1a, x2a, y2a, OldX, OldY, *NewX2, *NewY2);

	if (c2 != 1)
		c2 = 0;

	if ((mode & 2) == 2)
	{
#if 0
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
		DrawLine(MultX(x2), MultY(y2), MultX(x3), MultY(y3));
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
		ThickNess *= 2.0;
		ellips2(MultX(*NewX1), MultY(*NewY1), Mult(ThickNess), Mult(ThickNess), 255);
		ellips2(MultX(*NewX2), MultY(*NewY2), Mult(ThickNess), Mult(ThickNess), 255);
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
		ellips2(MultX(OldX), MultY(OldY), Mult(ThickNess), Mult(ThickNess), 255);
		sprintf(str, "1");
		TextOut(PCBDisplay, MultX(x1), MultY(y1), str, strlen(str));
		sprintf(str, "2");
		TextOut(PCBDisplay, MultX(x2), MultY(y2), str, strlen(str));
		sprintf(str, "3");
		TextOut(PCBDisplay, MultX(x3), MultY(y3), str, strlen(str));
		sprintf(str, "1a");
		TextOut(PCBDisplay, MultX(*NewX1), MultY(*NewY1), str, strlen(str));
		sprintf(str, "2a");
		TextOut(PCBDisplay, MultX(*NewX2), MultY(*NewY2), str, strlen(str));
#endif
	}

	return c1 * 2 + c2;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeBiggerSmallerObjectPolygon(ObjectPolygonRecord * Polygon, ObjectPolygonRecord * BiggerPolygon,
                                     double Thickness, int32 mode)
//  mode  bit 0 = 0 Bigger  polygon
//  mode  bit 0 = 1 Smaller polygon
{
#define    InRange13(x1,x2) ( (((x1>x2-0.001) && (x1<x2+0.001))) ? (1) : (0) )
#define NotInRange13(x1,x2) ( (((x1>x2-0.001) && (x1<x2+0.001))) ? (0) : (1) )
#define    InRange14(x1,x2) ( (((x1>x2-0.0001) && (x1<x2+0.0001))) ? (1) : (0) )
#define NotInRange14(x1,x2) ( (((x1>x2-0.0001) && (x1<x2+0.0001))) ? (0) : (1) )
#define    InRange15(x1,x2) ( (((x1>x2-10000.0) && (x1<x2+10000.0))) ? (1) : (0) )

	int32 cnt, cnt2, count, count2, CntMax, ok, Previous, Next, c1, c2, c1a, c2a, c3, c4, res;
	double x1, y1, x2, y2, x3, y3, maxy, OldX, OldY, OldX2, OldY2, NewX1, NewY1, NewX2, NewY2;
	int32 ChangeToNewX2;

	CntMax = 0;
	count = Polygon->NrVertices;

	if ((Polygon->NrSubPolygons > 0) && (Polygon->NrVerticesMainPolygon > 0))
		count = Polygon->NrVerticesMainPolygon;

	if (count <= 0)
		return -1;

	maxy = -10000.0e5;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*Polygon).Points[cnt].x;
		y1 = (*Polygon).Points[cnt].y;

		if (y1 > maxy)
		{
			CntMax = cnt;
			maxy = y1;
		}
	}

	Previous = CntMax - 1;

	if (Previous == -1)
		Previous += count;

	Next = CntMax + 1;

	if (Next == count)
		Next = 0;

	x1 = (*Polygon).Points[Previous].x;
	y1 = (*Polygon).Points[Previous].y;
	x2 = (*Polygon).Points[CntMax].x;
	y2 = (*Polygon).Points[CntMax].y;
	x3 = (*Polygon).Points[Next].x;
	y3 = (*Polygon).Points[Next].y;
	OldX = x2;
	OldY = 1e9;
	res =
	    GetBiggerSmallerPoints(x1, y1, x2, y2, x3, y3, OldX, OldY, &NewX1, &NewY1, &NewX2, &NewY2, &OldX2, &OldY2,
	                           Thickness, 0);

	c1a = (PointInObjectPolygon(Polygon, NewX1, NewY1)) & 1;
	c2a = (PointInObjectPolygon(Polygon, NewX2, NewY2)) & 1;

	if ((mode & 1) == 0)
	{	//  Make bigger polygon
		if (c1a + c2a != 1)
		{
			ok = 1;
			OldX = x2;
			OldY = 1e9;
			res =
			    GetBiggerSmallerPoints(x3, y3, x2, y2, x1, y1, OldX, OldY, &NewX1, &NewY1, &NewX2, &NewY2, &OldX2,
			                           &OldY2, Thickness, 0);

			c1a = (PointInObjectPolygon(Polygon, NewX1, NewY1)) & 1;
			c2a = (PointInObjectPolygon(Polygon, NewX2, NewY2)) & 1;

			if (c1a + c2a != 1)
				ok = 1;
		}

		if (c2a == 0)
		{
			NewX1 = NewX2;
			NewY1 = NewY2;
		}
	}
	else
	{	//  Make Smaller polygon
		if (c1a + c2a != 1)
		{
			ok = 1;
			OldX = x2;
			OldY = 1e9;
			res =
			    GetBiggerSmallerPoints(x3, y3, x2, y2, x1, y1, OldX, OldY, &NewX1, &NewY1, &NewX2, &NewY2, &OldX2,
			                           &OldY2, Thickness, 0);

			c1a = (PointInObjectPolygon(Polygon, NewX1, NewY1)) & 1;
			c2a = (PointInObjectPolygon(Polygon, NewX2, NewY2)) & 1;

			if (c1a + c2a != 1)
				ok = 1;
		}

		if (c2a == 1)
		{
			NewX1 = NewX2;
			NewY1 = NewY2;
		}
	}

	x1 = x2;
	y1 = y2;
	x2 = x3;
	y2 = y3;

//  InitDrawingObject(0,FIXED_COLOR_LAYER,1,GRAPHICS_WHITE+DRAW_WITH_PEN_AND_NOT_FILLED);
//  SetROP2(OutputDisplay,R2_COPYPEN);
//  ellips2(MultX(NewX1),MultY(NewY1),5,5,255);
//  while (!KeyPressed()) CheckInputMessages(0);

	cnt2 = 0;
	count2 = min(count, 18400);
	BiggerPolygon->NrVertices = count;
	BiggerPolygon->NrSubPolygons = 0;
	BiggerPolygon->NrVerticesMainPolygon = 0;
	cnt = 0;

	while (cnt < count2)
	{
		if (++Next == count)
			Next = 0;

		x3 = (*Polygon).Points[Next].x;
		y3 = (*Polygon).Points[Next].y;
		/*
		    if ((InRange(x2,x3))
		       &&
		       (InRange(y2,y3))) {
		      if (CountDirection==1) {
		        if (++Next==count) Next=0;
		      } else {
		        if (--Next==-1) Next+=count;
		      }
		      x3=(*Polygon).Points[Next].x;
		      y3=(*Polygon).Points[Next].y;
		      cnt++;
		    }
		*/
		OldX = NewX1;
		OldY = NewY1;

//    (*BiggerPolygon).Points[cnt2].x=NewX;
//    (*BiggerPolygon).Points[cnt2].y=NewY;
#ifdef _DEBUG

		if (cnt2 == 13)
			ok = 1;

		if ((InRange4(x1, 13347992.1143873450)) && (InRange4(y1, 9296956.7928441465)))
			ok = 1;

#endif
		res =
		    GetBiggerSmallerPoints(x1, y1, x2, y2, x3, y3, OldX, OldY, &NewX1, &NewY1, &NewX2, &NewY2, &OldX2, &OldY2,
		                           Thickness, 0);
		/*
		    Angle=CalcAngleLine(x1,y1,x2,y2,1);
		    Angle1=CalcAngleLine(OldX2,OldY2,NewX1,NewY1,1);
		    Angle2=CalcAngleLine(OldX2,OldY2,NewX2,NewY2,1);
		*/
		c1a = (PointInObjectPolygon(Polygon, NewX1, NewY1)) & 1;
		c2a = (PointInObjectPolygon(Polygon, NewX2, NewY2)) & 1;

		c1 = 0;
		c2 = 0;

		c3 = (res >> 1) & 1;
		c4 = res & 1;
		ChangeToNewX2 = 0;

		if ((mode & 1) == 1)
		{
// Smaller polygon
			if ((c1 == 0) && (c2 == 1))
				ChangeToNewX2 = 1;
			else
			{
				if (((c1 == 1) && (c2 == 1)) || ((c1 == 0) && (c2 == 0)))
				{
					if ((c3 == 1) && (c4 == 0))
					{
						if (!ChangeToNewX2)
							ok = 1;

						ChangeToNewX2 = 1;

						if ((c1a == 0) && (c2a == 1))
							ok = 1;
					}

					if ((c3 == 0) && (c4 == 1))
					{
						if ((c1a == 1) && (c2a == 0))
							ok = 1;
					}

					if (((c3 == 1) && (c4 == 1)) || ((c3 == 0) && (c4 == 0)))
						ok = 1;

					/*
					          if (InRange14(Angle,Angle2)) {
					            ChangeToNewX2=1;
					          } else {
					            if (NotInRange14(Angle,Angle1)) {
					              if (fabs(Angle-Angle1)>fabs(Angle-Angle2)) {
					                ChangeToNewX2=1;
					              }
					            }
					          }
					*/
				}
			}
		}
		else
		{
// Bigger polygon
			if ((c1 == 1) && (c2 == 0))
				ChangeToNewX2 = 1;
			else
			{
				if (((c1 == 1) && (c2 == 1)) || ((c1 == 0) && (c2 == 0)))
				{
					if ((c3 == 1) && (c4 == 0))
					{
						ChangeToNewX2 = 1;

						if ((c1a == 0) && (c2a == 1))
							ok = 1;
					}

					if ((c3 == 0) && (c4 == 1))
					{
						if ((c1a == 1) && (c2a == 0))
							ok = 1;
					}

					if (((c3 == 1) && (c4 == 1)) || ((c3 == 0) && (c4 == 0)))
						ok = 1;
				}
			}
		}

		if (ChangeToNewX2)
		{
			NewX1 = NewX2;
			NewY1 = NewY2;
		}

//    ellips2(Mult(NewX-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(NewY-Yoffset)-1,
//            Mult(Thickness),Mult(Thickness),255);
//    DrawLine(Mult(NewX-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(NewY+Thickness*0.5-Yoffset)-1,
//             Mult(NewX-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(NewY-Thickness*0.5-Yoffset)-1);
//    DrawLine(Mult(NewX+Thickness*0.5-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(NewY-Yoffset)-1,
//             Mult(NewX-Thickness*0.5-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(NewY-Yoffset)-1);

//    sprintf(str,"%i",cnt2);
//    TextOut(OutputDisplay,Mult(NewX-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(NewY-Yoffset)-1,
//            str,strlen(str));
		(*BiggerPolygon).Points[cnt2].x = NewX1;
		(*BiggerPolygon).Points[cnt2].y = NewY1;

//    if (cnt2>0) {
//      DrawLineGreen(OldX,OldY,NewX,NewY);
//      while (!KeyPressed()) CheckInputMessages();
//      ReadKeyFunction();
//    }

		cnt2++;
		x1 = x2;
		y1 = y2;
		x2 = x3;
		y2 = y3;
		cnt++;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MakePolygonFromPlotObject(ObjectRecord * Object, PolygonRecord * PolygonObject, double Clearance,
                               int32 CircleRoundings, int32 mode)
{
	int32 cnt, cnt2, Step, NrSegments, count;
	double x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, dikte, diktex, diktey, Angle, Angle1, Angle2, StartAngle,
	       Length1, Length2, Width, Height, Width2, Height2, sinx, sinx2, Length, AngleInc, AngleInc2, Thickness;

#ifdef STRIPPED
	return;
#endif

	Step = 1;
	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	Thickness = Object->Thickness;

	if ((mode & 32) == 0)
		CircleRoundings = 16;

	switch (Object->ObjectType)
	{
	case OBJECT_CIRCLE:
		dikte = x2 * 0.5 + Clearance;
		Angle = 0.0;
		AngleInc = 2 * PI / CircleRoundings;

		for (cnt = 0; cnt < CircleRoundings; cnt++)
		{
			x5 = cos(Angle) * dikte;
			y5 = sin(Angle) * dikte;
			PolygonObject->Points[cnt].x = x1 + x5;
			PolygonObject->Points[cnt].y = y1 + y5;
			Angle += AngleInc;
		}

		PolygonObject->NrVertices = CircleRoundings;
		PolygonObject->minx = x1 - dikte;
		PolygonObject->miny = y1 - dikte;
		PolygonObject->maxx = x1 + dikte;
		PolygonObject->maxy = y1 + dikte;
		break;

	case OBJECT_RECT:
		diktex = x2 / 2 + Clearance;
		diktey = y2 / 2 + Clearance;
		PolygonObject->Points[0].x = x1 + diktex;
		PolygonObject->Points[0].y = y1 + diktey;
		PolygonObject->Points[1].x = x1 - diktex;
		PolygonObject->Points[1].y = y1 + diktey;
		PolygonObject->Points[2].x = x1 - diktex;
		PolygonObject->Points[2].y = y1 - diktey;
		PolygonObject->Points[3].x = x1 + diktex;
		PolygonObject->Points[3].y = y1 - diktey;
		PolygonObject->minx = x1 - diktex;
		PolygonObject->miny = y1 - diktey;
		PolygonObject->maxx = x1 + diktex;
		PolygonObject->maxy = y1 + diktey;
		PolygonObject->NrVertices = 4;
		break;

	case OBJECT_LINE:
		dikte = Thickness * 0.5 + Clearance;
		x5 = x2 - x1;
		y5 = y2 - y1;

		if (NotInRange(x5, 0.0))
		{
			StartAngle = atan(y5 / x5);

			if (StartAngle == 0.0)
			{
				if (x5 < 0.0)
					StartAngle += PI;
			}
			else
			{
				if (StartAngle < 0.0)
					StartAngle += PI;
			}

			if (y2 < y1)
				StartAngle += PI;
		}
		else
		{
			if (y2 > y1)
				StartAngle = PI * 0.5;
			else
				StartAngle = PI * 1.5;
		}

		StartAngle += PI * 0.5;
		AngleInc = (2 * PI) / CircleRoundings;
		Angle = StartAngle;
		cnt2 = 0;

		for (cnt = 0; cnt < CircleRoundings + 1; cnt++)
		{
			x5 = cos(Angle) * dikte;
			y5 = sin(Angle) * dikte;

			if (cnt == (CircleRoundings / 2))
			{
				PolygonObject->Points[cnt2].x = x1 + x5;
				PolygonObject->Points[cnt2].y = y1 + y5;
				cnt2++;
			}

			if (cnt >= (CircleRoundings / 2))
			{
				PolygonObject->Points[cnt2].x = x2;
				PolygonObject->Points[cnt2].y = y2;
			}
			else
			{
				PolygonObject->Points[cnt2].x = x1;
				PolygonObject->Points[cnt2].y = y1;
			}

			PolygonObject->Points[cnt2].x += x5;
			PolygonObject->Points[cnt2].y += y5;
			cnt2++;
			Angle += AngleInc;

			if (Angle > (2 * PI))
				Angle -= 2 * PI;
		}

		PolygonObject->NrVertices = CircleRoundings + 2;
		PolygonObject->minx = min(x1, x2) - dikte;
		PolygonObject->miny = min(y1, y2) - dikte;
		PolygonObject->maxx = max(x1, x2) + dikte;
		PolygonObject->maxy = max(y1, y2) + dikte;
		break;

	case OBJECT_ARC:
		NrSegments = 32;
		count = 0;

		if (y2 == 0.0)
			y2 = x2;

		if ((x2 > (100 * 2540)) || (y2 > (100 * 2540)))
			NrSegments = 64;

		if ((x2 > (200 * 2540)) || (y2 > (200 * 2540)))
			NrSegments = 128;

		x3 = Object->x3;
		y3 = Object->y3;
		x4 = Object->x4;
		y4 = Object->y4;

		if ((x3 == x4) && (y3 == y4))
		{
			count = NrSegments;
			Angle1 = PI / 2;
			Angle2 = 0.0;
			AngleInc = (2 * PI) / count;
			AngleInc2 = 0.0;
		}
		else
		{
			ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &Length1);
			ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &Length2);

			if (Angle2 < Angle1)
				Angle2 += PI * 2;

			count = (int32) ((Angle2 - Angle1) / (PI * 2 / NrSegments));
			count = max(1, count);
			AngleInc = (Angle2 - Angle1) / count;
			AngleInc2 = (2 * PI) / CircleRoundings;
		}

		Width = (x2 + Thickness) * 0.5 + Clearance;
		Height = (x2 + Thickness) * 0.5 + Clearance;
//      Height=(y2+Thickness)*0.5+NewClearance;
		Width2 = Thickness * 0.5 + Clearance;
		Height2 = Thickness * 0.5 + Clearance;

		Angle = Angle1;
		cnt2 = 0;

		if ((x3 == x4) && (y3 == y4))
		{
			for (cnt = 0; cnt < count + 1; cnt++)
			{
				if (InRange(Width, Height))
				{
					x5 = x1 + Width * cos(Angle);
					y5 = y1 + Height * sin(Angle);
				}
				else
				{
					sinx = sin(Angle);
					sinx2 = sinx * sinx;
					Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
					x5 = x1 + Length * cos(Angle);
					y5 = y1 + Length * sin(Angle);
				}

				PolygonObject->Points[cnt2].x = x5;
				PolygonObject->Points[cnt2].y = y5;
				cnt2++;
				Angle += AngleInc;

				if (Angle > (2 * PI))
					Angle -= 2 * PI;
			}

			Angle -= AngleInc;
			Width = (x2 - Thickness) * 0.5 - Clearance;
			Height = (y2 - Thickness) * 0.5 - Clearance;

			for (cnt = 0; cnt < count + 1; cnt++)
			{
				if (InRange(Width, Height))
				{
					x5 = x1 + Width * cos(Angle);
					y5 = y1 + Height * sin(Angle);
				}
				else
				{
					sinx = sin(Angle);
					sinx2 = sinx * sinx;
					Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
					x5 = x1 + Length * cos(Angle);
					y5 = y1 + Length * sin(Angle);
				}

				PolygonObject->Points[cnt2].x = x5;
				PolygonObject->Points[cnt2].y = y5;
				cnt2++;
				Angle -= AngleInc;

				if (Angle < 0.0)
					Angle += 2 * PI;
			}

			PolygonObject->Points[0].x -= 1;
			PolygonObject->Points[cnt2 - 1].x -= 1;
			PolygonObject->NrVertices = cnt2;
			return;
		}

// **************************************************************************************

		for (cnt = 0; cnt < count; cnt++)
		{
			if (InRange(Width, Height))
			{
				x5 = x1 + Width * cos(Angle);
				y5 = y1 + Height * sin(Angle);
			}
			else
			{
				sinx = sin(Angle);
				sinx2 = sinx * sinx;
				Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
				x5 = x1 + Length * cos(Angle);
				y5 = y1 + Length * sin(Angle);
			}

			PolygonObject->Points[cnt2].x = x5;
			PolygonObject->Points[cnt2].y = y5;
			cnt2++;
			Angle += AngleInc;

			if (Angle > (2 * PI))
				Angle -= 2 * PI;
		}

		if (InRange(Width, Height))
		{
			x6 = x1 + (x2 * 0.5) * cos(Angle);
			y6 = y1 + (x2 * 0.5) * sin(Angle);
//        y6=y1+(y2*0.5)*sin(Angle);
		}
		else
		{
			sinx = sin(Angle);
			sinx2 = sinx * sinx;
			Length = (x2 * 0.5) * (x2 * 0.5) * sqrt(1 / (SQR((x2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
			Length = (x2 * 0.5) * (y2 * 0.5) * sqrt(1 / (SQR((y2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
			x6 = x1 + Length * cos(Angle);
			y6 = y1 + Length * sin(Angle);
		}

		Angle = Angle2;

		for (cnt = 0; cnt < CircleRoundings / 2; cnt++)
		{
			PolygonObject->Points[cnt2].x = x6 + cos(Angle) * Width2;
			PolygonObject->Points[cnt2].y = y6 + sin(Angle) * Height2;
			cnt2++;
			Angle += AngleInc2;

			if (Angle > (2 * PI))
				Angle -= 2 * PI;
		}

		Width = (x2 - Thickness) * 0.5 - Clearance;
		Height = (x2 - Thickness) * 0.5 - Clearance;
//      Height=(y2-Thickness)*0.5-NewClearance;
		Angle = Angle2;

		for (cnt = 0; cnt < count; cnt++)
		{
			if (InRange(Width, Height))
			{
				x5 = x1 + Width * cos(Angle);
				y5 = y1 + Height * sin(Angle);
			}
			else
			{
				sinx = sin(Angle);
				sinx2 = sinx * sinx;
				Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
				x5 = x1 + Length * cos(Angle);
				y5 = y1 + Length * sin(Angle);
			}

			PolygonObject->Points[cnt2].x = x5;
			PolygonObject->Points[cnt2].y = y5;
			cnt2++;
			Angle -= AngleInc;

			if (Angle < 0.0)
				Angle += 2 * PI;
		}

		if (InRange(Width, Height))
		{
			x6 = x1 + (x2 * 0.5) * cos(Angle);
			y6 = y1 + (x2 * 0.5) * sin(Angle);
//        y6=y1+(y2*0.5)*sin(Angle);
		}
		else
		{
			sinx = sin(Angle);
			sinx2 = sinx * sinx;
			Length = (x2 * 0.5) * (y2 * 0.5) * sqrt(1 / (SQR((y2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
			x6 = x1 + Length * cos(Angle);
			y6 = y1 + Length * sin(Angle);
		}

		Angle = Angle1 + PI;

		for (cnt = 0; cnt < CircleRoundings / 2; cnt++)
		{
			PolygonObject->Points[cnt2].x = x6 + cos(Angle) * Width2;
			PolygonObject->Points[cnt2].y = y6 + sin(Angle) * Height2;
			cnt2++;
			Angle += AngleInc2;

			if (Angle > (2 * PI))
				Angle -= 2 * PI;
		}

		PolygonObject->NrVertices = cnt2;
		SetMinMaxPolygon(PolygonObject, 0);
		break;

	default:
		PolygonObject->NrVertices = 0;
		break;
	}
}

// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// **************************************************************************************

void InsertVertice(int32 ResultStart, PolygonRecord * ResultPolygon, PointsArray * InsertPoints)
{
	int32 NrVerticesResultPolygon;

	NrVerticesResultPolygon = ResultPolygon->NrVertices;

	if (ResultStart < NrVerticesResultPolygon)
	{
		memmove(&((*ResultPolygon).Points[ResultStart + 1]), &((*ResultPolygon).Points[ResultStart]),
		        (NrVerticesResultPolygon - ResultStart) * sizeof(PointRecord));
	}

	memmove(&((*ResultPolygon).Points[ResultStart]), InsertPoints, sizeof(PointRecord));
	ResultPolygon->NrVertices++;
}

// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// **************************************************************************************

void AppendVertices(int32 InsertStart, int32 Count, int32 NrVerticesInsert, PointRecord * InsertPoints,
                    PointRecord * ResultPoints, int32 mode)
{
	int32 cnt;
	double *ResultP, *InsertP;

	ResultP = (double *) ResultPoints;
	InsertP = (double *) InsertPoints;
	InsertP += InsertStart * 2;

	if (mode == 0)
	{	// Oplopend
		cnt = 0;

		while (cnt < Count)
		{
			*(ResultP++) = *(InsertP++);
			*(ResultP++) = *(InsertP++);

//      if ((ResultStart++)==NrVerticesResult) {
//        ResultP-=NrVerticesResult*2;
//        ResultStart=0;
//      }
			if ((++InsertStart) == NrVerticesInsert)
			{
				InsertP -= NrVerticesInsert * 2;
				InsertStart = 0;
			}

			cnt++;
		}
	}
	else
	{	// Aflopend
		cnt = 0;

		while (cnt < Count)
		{
			*(ResultP++) = *(InsertP++);
			*(ResultP++) = *InsertP;
			InsertP -= 3;

//      if (ResultStart--==0) {
//        ResultP+=NrVerticesResult*2;
//        ResultStart=NrVerticesResult-1;
//      }
			if (InsertStart-- == 0)
			{
				InsertP += NrVerticesInsert * 2;
				InsertStart = NrVerticesInsert - 1;
			}

			cnt++;
		}
	}
}


// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// **************************************************************************************

int32 SetMinMaxPolygon(PolygonRecord * PolygonObject, int32 mode)
{
	int32 count, cnt;
	double x1, y1, minx, maxx, miny, maxy;

	count = PolygonObject->NrVertices;

	minx = 10000.0e5;
	miny = 10000.0e5;
	maxx = -10000.0e5;
	maxy = -10000.0e5;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*PolygonObject).Points[cnt].x;
		y1 = (*PolygonObject).Points[cnt].y;
		minx = min(minx, x1);
		miny = min(miny, y1);
		maxx = max(maxx, x1);
		maxy = max(maxy, y1);
	}

	PolygonObject->minx = minx;
	PolygonObject->miny = miny;
	PolygonObject->maxx = maxx;
	PolygonObject->maxy = maxy;

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PointInPolygon(PolygonRecord * Polygon, double px, double py)
{
	double x1, y1, x2, y2, fx, fy, *P, px2, py2;
	int32 cnt, res, crosses, NrVertices;

	crosses = 0;
	x2 = 0.0;
	y2 = 0.0;
	px2 = px - 0.0000015;
	py2 = py + 10000.0e5;
	NrVertices = Polygon->NrVertices;
	P = (double *) &(Polygon->Points);
	fx = *(P++);
	fy = *(P++);
	cnt = 0;

	while (cnt < NrVertices)
	{
		if (cnt > 0)
		{
			if (cnt < NrVertices - 1)
			{
				x1 = x2;
				y1 = y2;
				x2 = *(P++);
				y2 = *(P++);
			}
			else
			{
				x1 = x2;
				y1 = y2;
				x2 = fx;
				y2 = fy;
			}
		}
		else
		{
			x1 = fx;
			y1 = fy;
			x2 = *(P++);
			y2 = *(P++);
		}

		res = LineCrosses(px2, py, px2, py2, x1, y1, x2, y2);

		if (res == 1)
			crosses++;

		cnt++;
	}

	return crosses;
}

// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// **************************************************************************************

void CopyPolygonToPolygon(PolygonRecord * SrcPolygon, PolygonRecord * DestPolygon)
{
	int32 res;
	res = MemSizePolygon(SrcPolygon);

	if (res > 64000)
		res = 0;

	memmove(DestPolygon, SrcPolygon, MemSizePolygon(SrcPolygon));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindNextNrPolygonB(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MaxNrCrosses];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonB < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonB;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt < NrCrossPoints - 1)
			return Sort[cnt + 1];
		else
			return Sort[0];
	}
	else
		ok = 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindPreviousNrPolygonB(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MaxNrCrosses];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonB < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonB;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt > 0)
			return Sort[cnt - 1];
		else
			return Sort[NrCrossPoints - 1];
	}
	else
	{
		ok = 1;
		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindCrossIndexNrPolygonB(int32 Nr)
{
	int32 cnt;

	cnt = 0;

	while ((cnt < NrCrossPoints) && (CrossPoints[cnt].LineNrPolygonB != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
		return cnt;
	else
		return 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindNextNrPolygonA(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MaxNrCrosses];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonA < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonA;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt < NrCrossPoints - 1)
			return Sort[cnt + 1];
		else
			return Sort[0];
	}
	else
	{
		ok = 1;
		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindPreviousNrPolygonA(int32 Nr)
{
	int32 cnt, cnt2, ok, Sort[MaxNrCrosses];

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		cnt2 = cnt;

		while ((cnt2 > 0) && (CrossPoints[cnt].LineNrPolygonA < Sort[cnt2 - 1]))
			cnt2--;

		if (cnt2 < cnt)
			memmove(&Sort[cnt2 + 1], &Sort[cnt2], (cnt - cnt2) * sizeof(int32));

		Sort[cnt2] = CrossPoints[cnt].LineNrPolygonA;
	}

	cnt = 0;

	while ((cnt < NrCrossPoints) && (Sort[cnt] != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
	{
		if (cnt > 0)
			return Sort[cnt - 1];
		else
			return Sort[NrCrossPoints - 1];
	}
	else
	{
		ok = 1;
		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindCrossIndexNrPolygonA(int32 Nr)
{
	int32 cnt;

	cnt = 0;

	while ((cnt < NrCrossPoints) && (CrossPoints[cnt].LineNrPolygonA != Nr))
		cnt++;

	if (cnt < NrCrossPoints)
		return cnt;
	else
		return 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CollectCrossesPolygons2(PolygonRecord * PolygonObject, PolygonRecord * ResultPolygon, int32 mode)
{
	int32 cnt4, cnt3, cnt2, cnt, count, count2, res, NrCrosses, InsertPos, Pos, CountOnSameLine, Found, NrTries,
	      LineCnt2, LineCnt3, LineCnt2a, LineCnt3a, DisabledPos, NextPoint;
	double *x1, *y1, *x2, *y2, *FirstX, *FirstY, x3, y3, x4, y4, MinDist, DivX, DivY, cx, cy, prevx, prevy;
	PointRecord NewPoint;
	int32 Changed = 1;

	NrCrosses = 0;
	count = 0;
	Pos = 0;
	prevx = 0.0;
	prevy = 0.0;
	CountOnSameLine = 0;

	CopyPolygonToPolygon(ResultPolygon, WorkPolygon[0]);
	CopyPolygonToPolygon(PolygonObject, WorkPolygon[1]);


	NrTries = 0;

	while ((Changed) && (NrTries < 2))
	{
		Changed = 0;
		NrCrosses = 0;
		count2 = PolygonObject->NrVertices;

		for (cnt3 = 0; cnt3 < count2; cnt3++)
		{
			x3 = PolygonObject->Points[cnt3].x;
			y3 = PolygonObject->Points[cnt3].y;

			if (cnt3 < count2 - 1)
			{
				x4 = PolygonObject->Points[cnt3 + 1].x;
				y4 = PolygonObject->Points[cnt3 + 1].y;
			}
			else
			{
				x4 = PolygonObject->Points[0].x;
				y4 = PolygonObject->Points[0].y;
			}

			count = ResultPolygon->NrVertices;
			FirstX = (double *) &(ResultPolygon->Points);
			FirstY = FirstX + 1;
			x1 = FirstX;
			y1 = FirstY;

			for (cnt2 = 0; cnt2 < count; cnt2++)
			{
				if (cnt2 < count - 1)
				{
					x2 = x1 + 2;
					y2 = y1 + 2;
				}
				else
				{
					x2 = FirstX;
					y2 = FirstY;
				}

				res = LineCrosses(*x1, *y1, *x2, *y2, x3, y3, x4, y4);

				if (res == 1)
				{
					if (NrCrosses < MaxNrCrosses)
					{
						CrossPoints[NrCrosses].LineNrPolygonA = cnt2;
						CrossPoints[NrCrosses].LineNrPolygonB = cnt3;
						CrossPoints[NrCrosses].CrossX = LineCrossX;
						CrossPoints[NrCrosses].CrossY = LineCrossY;
						CrossPoints[NrCrosses].Test = 0;
					}

					NrCrosses++;
				}
				else
				{
					if (res == 0)
					{
						res = LineCrosses(*x1, *y1, *x2, *y2, x3, y3, x4, y4);
						return -1;
					}
				}

				x1 += 2;
				y1 += 2;
			}
		}

		if (NrCrosses > MaxNrCrosses)
			return NrCrosses;

		if ((NrCrosses & 1) == 1)
		{
			res = 1;

			for (cnt = 0; cnt < NrCrosses; cnt++)
			{
				LineCnt2 = CrossPoints[cnt].LineNrPolygonA;
				LineCnt2a = LineCnt2;

				if (LineCnt2a < count - 1)
					LineCnt2a++;
				else
					LineCnt2a = 0;

				LineCnt3 = CrossPoints[cnt].LineNrPolygonB;
				LineCnt3a = LineCnt3;

				if (LineCnt3a < count2 - 1)
					LineCnt3a++;
				else
					LineCnt3a = 0;
			}

			cnt = 0;

			while (cnt < NrCrosses - 1)
			{
				cnt2 = cnt + 1;

				while (cnt2 < NrCrosses)
				{
					if ((InRange6(CrossPoints[cnt].CrossX, CrossPoints[cnt2].CrossX))
					        && (InRange6(CrossPoints[cnt].CrossY, CrossPoints[cnt2].CrossY)))
					{
						res = 1;

						if (cnt2 < NrCrosses - 1)
						{
							memmove(&CrossPoints[cnt2], &CrossPoints[cnt2 + 1],
							        sizeof(CrossPointRecord) * (NrCrosses - cnt2 - 1));
						}

						NrCrosses--;
					}

					cnt2++;
				}

				cnt++;
			}
		}

		if ((NrCrosses & 1) == 1)
			return NrCrosses;

		DisabledPos = 10000000;
		cnt = 0;

		while (cnt < NrCrosses - 1)
		{

// **********************************************************************************************
// **********************************************************************************************

			cnt2 = cnt + 1;
			CountOnSameLine = 0;

			while (cnt2 < NrCrosses)
			{
				if (CrossPoints[cnt].LineNrPolygonA == CrossPoints[cnt2].LineNrPolygonA)
				{
					CountOnSameLine++;
					Pos = cnt2;
				}

				cnt2++;
			}

			if (CountOnSameLine == 1)
			{
				NewPoint.x = (CrossPoints[cnt].CrossX + CrossPoints[Pos].CrossX) / 2;
				NewPoint.y = (CrossPoints[cnt].CrossY + CrossPoints[Pos].CrossY) / 2;
				NextPoint = CrossPoints[cnt].LineNrPolygonA + 1;

//        if (NextPoint==ResultPolygon->NrVertices) NextPoint=0;
				if (NextPoint >= 10000000)
					res = 1;

				InsertVertice(NextPoint, ResultPolygon, (PointsArray *) & NewPoint);

				for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
				{
					if (CrossPoints[cnt3].LineNrPolygonA > CrossPoints[cnt].LineNrPolygonA)
						CrossPoints[cnt3].LineNrPolygonA++;
				}

				Changed = 1;
			}
			else
			{
				if (CountOnSameLine > 1)
				{
					InsertPos = CrossPoints[cnt].LineNrPolygonA;
					cx = ResultPolygon->Points[InsertPos].x;
					cy = ResultPolygon->Points[InsertPos].y;
					CountOnSameLine = 0;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt].CrossX;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt].CrossY;
					DivX = cx - CrossPoints[cnt].CrossX;
					DivY = cy - CrossPoints[cnt].CrossY;
					DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
					DistanceOnSameLine[CountOnSameLine].Pos = cnt;
					DistanceOnSameLine[CountOnSameLine].Test = 0;
					CountOnSameLine++;

					cnt2 = cnt + 1;

					while (cnt2 < NrCrosses)
					{
						if (CrossPoints[cnt].LineNrPolygonA == CrossPoints[cnt2].LineNrPolygonA)
						{
							DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt2].CrossX;
							DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt2].CrossY;
							DivX = cx - CrossPoints[cnt2].CrossX;
							DivY = cy - CrossPoints[cnt2].CrossY;
							DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
							DistanceOnSameLine[CountOnSameLine].Pos = cnt2;
							DistanceOnSameLine[CountOnSameLine].Test = 0;
							CountOnSameLine++;
						}

						cnt2++;
					}

					MinDist = 10000.0e5;
					Found = -1;
// Find first cross closest to start of line
					cnt4 = 0;

					while (cnt4 < CountOnSameLine)
					{
						if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
						{
							Found = cnt4;
							MinDist = DistanceOnSameLine[cnt4].Distance;
						}

						cnt4++;
					}

					if (Found != -1)
					{
						DistanceOnSameLine[Found].Test = 1;
						Pos = DistanceOnSameLine[Found].Pos;
						prevx = CrossPoints[Pos].CrossX;
						prevy = CrossPoints[Pos].CrossY;
						CrossPoints[Pos].LineNrPolygonA = DisabledPos++;
//            CrossPoints[Pos].LineNrPolygonA=10000000;
					}

// Insert point between next crosses and the previous cross
					cnt3 = 0;

					while (cnt3 < CountOnSameLine - 1)
					{
						MinDist = 10000.0e5;
						Found = -1;
						cnt4 = 0;

						while (cnt4 < CountOnSameLine)
						{
							if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
							{
								Found = cnt4;
								MinDist = DistanceOnSameLine[cnt4].Distance;
							}

							cnt4++;
						}

						if (Found != -1)
						{
							DistanceOnSameLine[Found].Test = 1;
							Pos = DistanceOnSameLine[Found].Pos;
							NewPoint.x = (prevx + CrossPoints[Pos].CrossX) / 2;
							NewPoint.y = (prevy + CrossPoints[Pos].CrossY) / 2;
							prevx = CrossPoints[Pos].CrossX;
							prevy = CrossPoints[Pos].CrossY;
							InsertVertice(InsertPos + cnt3 + 1, ResultPolygon, (PointsArray *) & NewPoint);
							CrossPoints[Pos].LineNrPolygonA = DisabledPos++;
							Changed = 1;
						}

						cnt3++;
					}

					for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
					{
						if ((CrossPoints[cnt3].LineNrPolygonA > InsertPos)
						        && (CrossPoints[cnt3].LineNrPolygonA < 10000000))
							CrossPoints[cnt3].LineNrPolygonA += CountOnSameLine - 1;
					}
				}
			}

// **********************************************************************************************
// **********************************************************************************************

			cnt2 = cnt + 1;
			CountOnSameLine = 0;

			while (cnt2 < NrCrosses)
			{
				if (CrossPoints[cnt].LineNrPolygonB == CrossPoints[cnt2].LineNrPolygonB)
				{
					CountOnSameLine++;
					Pos = cnt2;
				}

				cnt2++;
			}

			if (CountOnSameLine == 1)
			{
				NewPoint.x = (CrossPoints[cnt].CrossX + CrossPoints[Pos].CrossX) / 2;
				NewPoint.y = (CrossPoints[cnt].CrossY + CrossPoints[Pos].CrossY) / 2;
				NextPoint = CrossPoints[cnt].LineNrPolygonB + 1;

				if (NextPoint >= 10000000)
					res = 1;

				InsertVertice(NextPoint, PolygonObject, (PointsArray *) & NewPoint);

				for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
				{
					if (CrossPoints[cnt3].LineNrPolygonB > CrossPoints[cnt].LineNrPolygonB)
						CrossPoints[cnt3].LineNrPolygonB++;
				}

				Changed = 1;
			}
			else
			{
				if (CountOnSameLine > 1)
				{
					InsertPos = CrossPoints[cnt].LineNrPolygonB;
					cx = PolygonObject->Points[InsertPos].x;
					cy = PolygonObject->Points[InsertPos].y;
					CountOnSameLine = 0;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt].CrossX;
					DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt].CrossY;
					DivX = cx - CrossPoints[cnt].CrossX;
					DivY = cy - CrossPoints[cnt].CrossY;
					DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
					DistanceOnSameLine[CountOnSameLine].Pos = cnt;
					DistanceOnSameLine[CountOnSameLine].Test = 0;
					CountOnSameLine++;

					cnt2 = cnt + 1;

					while (cnt2 < NrCrosses)
					{
						if (CrossPoints[cnt].LineNrPolygonB == CrossPoints[cnt2].LineNrPolygonB)
						{
							DistanceOnSameLine[CountOnSameLine].CrossPoint.x = CrossPoints[cnt2].CrossX;
							DistanceOnSameLine[CountOnSameLine].CrossPoint.y = CrossPoints[cnt2].CrossY;
							DivX = cx - CrossPoints[cnt2].CrossX;
							DivY = cy - CrossPoints[cnt2].CrossY;
							DistanceOnSameLine[CountOnSameLine].Distance = sqrt(DivX * DivX + DivY * DivY);
							DistanceOnSameLine[CountOnSameLine].Pos = cnt2;
							DistanceOnSameLine[CountOnSameLine].Test = 0;
							CountOnSameLine++;
						}

						cnt2++;
					}

					MinDist = 10000.0e5;
					Found = -1;
// Find first cross closest to start of line
					cnt4 = 0;

					while (cnt4 < CountOnSameLine)
					{
						if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
						{
							Found = cnt4;
							MinDist = DistanceOnSameLine[cnt4].Distance;
						}

						cnt4++;
					}

					if (Found != -1)
					{
						DistanceOnSameLine[Found].Test = 1;
						Pos = DistanceOnSameLine[Found].Pos;
						prevx = CrossPoints[Pos].CrossX;
						prevy = CrossPoints[Pos].CrossY;
						CrossPoints[Pos].LineNrPolygonB = DisabledPos++;
//            CrossPoints[Pos].LineNrPolygonB=10000000;
					}

// Insert point between next crosses and the previous cross
					cnt3 = 0;

					while (cnt3 < CountOnSameLine - 1)
					{
						MinDist = 10000.0e5;
						Found = -1;
						cnt4 = 0;

						while (cnt4 < CountOnSameLine)
						{
							if ((DistanceOnSameLine[cnt4].Test == 0) && (DistanceOnSameLine[cnt4].Distance < MinDist))
							{
								Found = cnt4;
								MinDist = DistanceOnSameLine[cnt4].Distance;
							}

							cnt4++;
						}

						if (Found != -1)
						{
							DistanceOnSameLine[Found].Test = 1;
							Pos = DistanceOnSameLine[Found].Pos;
							NewPoint.x = (prevx + CrossPoints[Pos].CrossX) / 2;
							NewPoint.y = (prevy + CrossPoints[Pos].CrossY) / 2;
							prevx = CrossPoints[Pos].CrossX;
							prevy = CrossPoints[Pos].CrossY;
							InsertVertice(InsertPos + cnt3 + 1, PolygonObject, (PointsArray *) & NewPoint);
							CrossPoints[Pos].LineNrPolygonB = DisabledPos++;
							Changed = 1;
						}

						cnt3++;
					}

					for (cnt3 = cnt + 1; cnt3 < NrCrosses; cnt3++)
					{
						if ((CrossPoints[cnt3].LineNrPolygonB > InsertPos)
						        && (CrossPoints[cnt3].LineNrPolygonB < 10000000))
							CrossPoints[cnt3].LineNrPolygonB += CountOnSameLine - 1;
					}

#ifdef _DEBUG

//          DrawTestPolygon2(PolygonObject,0);
					if (ObjectNr == 37)
					{
						cnt3 = 0;
//            return 6;
					}

#endif
				}
			}

			cnt++;
		}

		NrTries++;

		if (CountOnSameLine > 2)
		{
//      DrawTestPolygon(PolygonObject,0);
//      return NrCrosses;
//      return 6;
		}

		if (mode == 1)
			return NrCrosses;
	}

	for (cnt = 0; cnt < NrCrosses; cnt++)
	{
		if ((CrossPoints[cnt].LineNrPolygonA > 10000) || (CrossPoints[cnt].LineNrPolygonA < -10000)
		        || (CrossPoints[cnt].LineNrPolygonB > 10000) || (CrossPoints[cnt].LineNrPolygonB < -10000))
		{
//      DrawTestPolygon3(PolygonObject,0);
//      DrawTestPolygon3(ResultPolygon,1);
			res = 1;
			return 100;
		}
	}

	return NrCrosses;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetPolygonInsertPointsForAppendDelete(PolygonRecord * CopyPolygon, PolygonRecord * AppendPolygon, int32 mode)
{
	int32 NrVerticesCopyPolygon, NrVerticesAppendPolygon, NextCrossNr, CurrentPos, cnt, NextNr, LineNrPolygonA,
	      LineNrPolygonB, PolygonCount, StartNr, TotalPolygons, NextPolygonALineNr, NextPolygonBLineNr, res, count;
	double CheckX, CheckY;

//          PointRecord CrossPoints[MaxNrCrosses];
//          int32 LineNrPolygon[MaxNrCrosses],Count[MaxNrCrosses],Mode[MaxNrCrosses];
//        } VerticeCopy2Record;

	PolygonCount = 0;
	CurrentPos = 0;
	StartNr = 0;
	TotalPolygons = 1;
	NrVerticesCopyPolygon = CopyPolygon->NrVertices;
	NrVerticesAppendPolygon = AppendPolygon->NrVertices;

	while (PolygonCount < NrCrossPoints)
	{

		VerticeCopy2.Mode[PolygonCount] = TotalPolygons;
		CrossPoints[CurrentPos].Test = 1;
		VerticeCopy2.CrossPoints[PolygonCount].x = CrossPoints[CurrentPos].CrossX;
		VerticeCopy2.CrossPoints[PolygonCount].y = CrossPoints[CurrentPos].CrossY;
		LineNrPolygonB = CrossPoints[CurrentPos].LineNrPolygonB;
		NextPolygonBLineNr = (LineNrPolygonB + 1) % NrVerticesAppendPolygon;
		CheckX = AppendPolygon->Points[NextPolygonBLineNr].x;
		CheckY = AppendPolygon->Points[NextPolygonBLineNr].y;
		res = PointInPolygon(CopyPolygon, CheckX, CheckY);

		if ((res & 1) == mode)
		{	// Point PolygonBLineNr+1 outside PolygonA (mode = 0) Append
			// Point PolygonBLineNr+1 inside  PolygonA (mode = 1) Delete
			NextNr = FindNextNrPolygonB(LineNrPolygonB);
			NextCrossNr = FindCrossIndexNrPolygonB(NextNr);

			if ((count = NextNr - LineNrPolygonB) < 0)
				count += NrVerticesAppendPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = NextPolygonBLineNr;
			VerticeCopy2.Count[PolygonCount] = count;
		}
		else
		{	// Point PolygonBLineNr+1 inside  PolygonA    (mode = 0) Append
			// Point PolygonBLineNr+1 outside PolygonA    (mode = 1) Delete
			NextNr = FindPreviousNrPolygonB(LineNrPolygonB);
			NextCrossNr = FindCrossIndexNrPolygonB(NextNr);

			if ((count = LineNrPolygonB - NextNr) < 0)
				count += NrVerticesAppendPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = LineNrPolygonB;
			VerticeCopy2.Count[PolygonCount] = -count;
		}

// *********************************************************************************************
// *********************************************************************************************

		PolygonCount++;
		CurrentPos = NextCrossNr;
		CrossPoints[CurrentPos].Test = 1;
		VerticeCopy2.Mode[PolygonCount] = TotalPolygons;
		VerticeCopy2.CrossPoints[PolygonCount].x = CrossPoints[CurrentPos].CrossX;
		VerticeCopy2.CrossPoints[PolygonCount].y = CrossPoints[CurrentPos].CrossY;
		LineNrPolygonA = CrossPoints[CurrentPos].LineNrPolygonA;
		NextPolygonALineNr = (LineNrPolygonA + 1) % NrVerticesCopyPolygon;
		CheckX = CopyPolygon->Points[NextPolygonALineNr].x;
		CheckY = CopyPolygon->Points[NextPolygonALineNr].y;
		res = PointInPolygon(AppendPolygon, CheckX, CheckY);

		if ((res & 1) == 0)
		{	// Point PolygonALineNr+1 outside PolygonB
			NextNr = FindNextNrPolygonA(LineNrPolygonA);
			NextCrossNr = FindCrossIndexNrPolygonA(NextNr);

			if ((count = NextNr - LineNrPolygonA) < 0)
				count += NrVerticesCopyPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = NextPolygonALineNr;
			VerticeCopy2.Count[PolygonCount] = count;
		}
		else
		{	// Point PolygonALineNr+1 inside  PolygonB
			NextNr = FindPreviousNrPolygonA(LineNrPolygonA);
			NextCrossNr = FindCrossIndexNrPolygonA(NextNr);

			if ((count = LineNrPolygonA - NextNr) < 0)
				count += NrVerticesCopyPolygon;

			VerticeCopy2.LineNrPolygon[PolygonCount] = LineNrPolygonA;
			VerticeCopy2.Count[PolygonCount] = -count;
		}

		PolygonCount++;


		if (StartNr == NextCrossNr)
		{
			if (PolygonCount < NrCrossPoints)
			{
// Find next unused cross nr
				TotalPolygons++;
				cnt = 0;

				while ((cnt < NrCrossPoints) && (CrossPoints[cnt].Test == 1))
					cnt++;

				if (cnt < NrCrossPoints)
				{
					StartNr = cnt;
					CurrentPos = cnt;
				}
			}
		}
		else
			CurrentPos = NextCrossNr;
	}

	for (cnt = 0; cnt < NrCrossPoints; cnt++)
	{
		if ((VerticeCopy2.Count[cnt] > 10000) || (VerticeCopy2.Count[cnt] < -10000))
			res = 1;
	}

	return TotalPolygons;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void IncludePolygonInPolygon2(PolygonRecord * StartPolygon, PolygonRecord * AppendDeletePolygon)
{
	int32 NrVerticesStartPolygon, NrVerticesAppendDeletePolygon, ResultP, NextCrossNr, WorkPolygonNr, cnt, count, res;
	PointRecord *StartPolygonPoints, *AppendDeletePolygonPoints, *ResultPolygonPoint, *CrossPoint;

	NrVerticesStartPolygon = StartPolygon->NrVertices;
	NrVerticesAppendDeletePolygon = AppendDeletePolygon->NrVertices;

	if ((NrVerticesStartPolygon == 0) || (NrVerticesAppendDeletePolygon == 0))
		res = 1;

	ResultP = 0;
	WorkPolygonNr = 0;
	StartPolygonPoints = (PointRecord *) & (StartPolygon->Points[0]);
	AppendDeletePolygonPoints = (PointRecord *) & (AppendDeletePolygon->Points[0]);

// ***********************************************************************************************
// ***********************************************************************************************

	NextCrossNr = 0;

	for (cnt = 0; cnt < 4; cnt++)
		WorkPolygon[cnt]->NrVertices = 0;

	ResultPolygon = (PolygonRecord *) WorkPolygon[WorkPolygonNr];
	ResultPolygonPoint = (PointRecord *) & (WorkPolygon[WorkPolygonNr]->Points[0]);

	for (cnt = 0; cnt < (NrCrossPoints / 2); cnt++)
	{

// Insert first cross point

		CrossPoint = (PointRecord *) & (VerticeCopy2.CrossPoints[NextCrossNr]);
		AppendVertices(0, 1, 1, CrossPoint, ResultPolygonPoint, 0);
		ResultP++;
		ResultPolygonPoint++;

		// Insert points polygonB (Append Delete)

		count = VerticeCopy2.Count[NextCrossNr];

		if (count > 0)
		{
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesAppendDeletePolygon,
			               AppendDeletePolygonPoints, ResultPolygonPoint, 0);
		}
		else
		{
			count = -count;
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesAppendDeletePolygon,
			               AppendDeletePolygonPoints, ResultPolygonPoint, 1);
		}

		ResultPolygonPoint += count;
		ResultP += count;

		// ***********************************************************************************************
		// ***********************************************************************************************

		// Insert second cross point

		NextCrossNr++;
		CrossPoint = (PointRecord *) & (VerticeCopy2.CrossPoints[NextCrossNr]);
		AppendVertices(0, 1, 1, CrossPoint, ResultPolygonPoint, 0);
		ResultP++;
		ResultPolygonPoint++;

		// Insert points polygonA ()

		count = VerticeCopy2.Count[NextCrossNr];

		if (count > 0)
		{
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesStartPolygon, StartPolygonPoints,
			               ResultPolygonPoint, 0);
		}
		else
		{
			count = -count;
			AppendVertices(VerticeCopy2.LineNrPolygon[NextCrossNr], count, NrVerticesStartPolygon, StartPolygonPoints,
			               ResultPolygonPoint, 1);
		}

		ResultPolygonPoint += count;
		ResultP += count;
		ResultPolygon->NrVertices = ResultP;
		NextCrossNr++;

		// ***********************************************************************************************
		// ***********************************************************************************************

		if ((NextCrossNr < NrCrossPoints) && (VerticeCopy2.Mode[NextCrossNr - 1] != VerticeCopy2.Mode[NextCrossNr]))
		{
			WorkPolygonNr++;
			ResultP = 0;
			ResultPolygon = (PolygonRecord *) WorkPolygon[WorkPolygonNr];
			ResultPolygonPoint = (PointRecord *) & (WorkPolygon[WorkPolygonNr]->Points[0]);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNewWorkPolygon(PolygonRecord * NewPolygonObject, int32 NrPolygons, int32 mode)
{
	int32 cnt, MaxNr, MaxCount, res;
	double Area, MaxArea;
	int32 FirstPolygonCheckOk;

	MaxCount = 0;
	MaxNr = 0;
	MaxArea = 0.0;

	for (cnt = 0; cnt < NrPolygons; cnt++)
	{
		SetMinMaxPolygon(WorkPolygon[cnt], 0);
		Area = (WorkPolygon[cnt]->maxx - WorkPolygon[cnt]->minx) * (WorkPolygon[cnt]->maxy - WorkPolygon[cnt]->miny);

		if (Area > MaxArea)
		{
			MaxArea = Area;
			MaxNr = cnt;
		}

		/*
		    if (WorkPolygon[cnt]->NrVertices>MaxCount) {
		      MaxCount=WorkPolygon[cnt]->NrVertices;
		      MaxNr=cnt;
		    }
		*/
	}

	if (mode == 0)
	{	// Check smaller polygon is inside a big polygon
		FirstPolygonCheckOk = 1;

		for (cnt = 0; cnt < NrPolygons; cnt++)
		{
			if (cnt != MaxNr)
			{
				if (CheckPolygonCompleetlyInsidePolygon(WorkPolygon[cnt], WorkPolygon[MaxNr]) == 0)
					FirstPolygonCheckOk = 0;
			}
		}

		if (FirstPolygonCheckOk)
		{	// The polygon with the max NrVertices encloses the other polygons
			CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);

			if (NrPolygons == 1)
				return 0;
			else
				return 1;
		}
		else
		{	// Find the polygon which encloses the others
			res = 1;

			switch (NrPolygons)
			{
			case 2:
				if (CheckPolygonCompleetlyInsidePolygon(WorkPolygon[MaxNr], WorkPolygon[MaxNr ^ 1]) == 0)
				{
					CopyPolygonToPolygon(WorkPolygon[MaxNr ^ 1], NewPolygonObject);
					return 1;
				}
				else
				{
					CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);
					return 1;
				}

				break;

			case 3:
			case 4:
				CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);
				return 1;
			}
		}
	}
	else
	{
		CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygonObject);

		if (NrPolygons == 1)
			return 0;
		else
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MergePolygon(PolygonRecord * PolygonObject, int32 Mode)
{
	int32 cnt, cnt2, cnt4, count, NrCrosses, TotalCount, NrPolygons, ok, VerticesCount, MemSizeAreaFill, Mem2, MaxNr;
	double Area, MaxArea;

	PolygonRecord *SubPolygon, *SpecialPolygon, *AreaFillPolygon, *TempPolygon;
	int32 Changed, AppendingPolygonChanged, SubPolygonInsidePolygonObject, InsideAppendingPolygon, OverlappedPolygon,
	      DeletionPolygonChanged, AppendingPolygonInsideDeletionPolygon;
	uint8 *PolygonPos, *AreaFillPolygonPos;

	GlobalNrCrosses = 0;

// *********************************************************************************
// Check if PolygonObject overlaps an existing polygon

#ifdef _DEBUG

	if (ObjectNr == 34)
		ok = 1;

#endif
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;
	OverlappedPolygon = 0;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
#ifdef _DEBUG

		if (SubPolygon->NrVertices > 10000)
			ok = 1;

#endif

		if ((ObjectsMinMaxOverlap3(PolygonObject, SubPolygon))
		        && (CheckPolygonCompleetlyOutsidePolygon(PolygonObject, SubPolygon) != 1))
			OverlappedPolygon = 1;

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

// *********************************************************************************
	if (!OverlappedPolygon)
	{
// Append polygon does not overlap any polygon -> just append polygon
		MemSizeAreaFill = TempAreaFill->MemSize + MemSizePolygon(PolygonObject);

		if (MemSizeAreaFill >= MaxAreaFillMemoryTemp)
		{
			if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill) != 0)
				return -105;

			NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
			TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
		}

		count = TempAreaFill->MemSize;
		memmove(NewAreaFill, TempAreaFill, count);
		SubPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + count);
		CopyPolygonToPolygon(PolygonObject, SubPolygon);
		SubPolygon->PolygonType |= 8;
#ifdef _DEBUG

		if (SubPolygon->NrVertices > 10000)
			ok = 1;

#endif
		NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
		NewAreaFill->NrPolygons++;
		return 2;
	}

// *********************************************************************************
// *********************************************************************************
// PolygonObject overlaps an existing polygon


	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		SubPolygon->Special.Test = 0;
		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	CopyPolygonToPolygon(PolygonObject, NewPolygon);
	CopyPolygonToPolygon(PolygonObject, BufferPolygon);
	NewPolygon->PolygonType = 0;

	NrExtraDeletionPolygons = 0;
	ExtraDeletionPolygonsMemSize = 0;
	ExtraDeletionPolygonPos[0] = 0;

// *********************************************************************************
// *********************************************************************************
// Check if a SubPolygon is inside PolygonObject

#ifdef _DEBUG

	if (ObjectNr == 2062)
		ok = 1;

#endif
	SubPolygonInsidePolygonObject = 0;
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if ((SubPolygon->PolygonType & 8) == 0)
		{	// Deletion polygon
			if (CheckPolygonCompleetlyInsidePolygon(SubPolygon, PolygonObject) == 1)
			{
				SubPolygon->Special.Test = 1;
				SubPolygonInsidePolygonObject = 1;
			}
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

// *********************************************************************************
// Find the resulting Appending polygon out of merging (adding) the PolygonObject
// with the other appending polygons from the areafill
// The resulting appending polygon will be NewPolygon
// *********************************************************************************

	Changed = 1;
	TotalCount = 0;
	AppendingPolygonChanged = 0;

	while ((Changed) && (TotalCount < 10000))
	{
		Changed = 0;
		SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) SubPolygon;

//    if (ObjectNr==10) {
//      res=1;
//      DrawTestPolygon(NewPolygon,0);  // red
//    }
		/*
		    if (ObjectNr==StartCnt+31) {
		      InvalidateRect(VIEWPLOTWindow,NULL,0);
		      PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
		      CheckInputMessages();
		    }
		*/
		for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
		{
			count = SubPolygon->NrVertices;

			if (((SubPolygon->PolygonType & 8) == 8)	// Appending polygons
			        && (SubPolygon->Special.Test == 0))
			{
				/*
				        if (ObjectNr==StartCnt+31) {
				          res=1;
				          DrawTestPolygon(SubPolygon,1);  // green
				          DrawTestPolygon(NewPolygon,2);  // yellow
				          while (!KeyPressed()) CheckInputMessages();
				          ReadKeyFunction();
				          InvalidateRect(VIEWPLOTWindow,NULL,0);
				          PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
				          CheckInputMessages();
				        }
				*/
				VerticesCount = SubPolygon->NrVertices + 256;

				if (VerticesCount >= MaxNrVerticesPolygon)
				{
					if (AllocateMemPolygons(VerticesCount) != 0)
						return -100;
				}

				if (!ObjectsMinMaxOverlap3(SubPolygon, NewPolygon))
					NrCrosses = 0;
				else
				{
					CopyPolygonToPolygon(SubPolygon, ExtraPolygon);
					NrCrosses = CollectCrossesPolygons2(ExtraPolygon, NewPolygon, 0);
				}

// ObjectNr
				if ((NrCrosses > 0) && (NrCrosses <= 8) && ((NrCrosses & 1) == 0))
				{
					NrCrossPoints = NrCrosses;

// *********************************************************************************
//  Get Insert points for appending polygon ExtraPolygon to NewPolygon
// *********************************************************************************
					NrPolygons = GetPolygonInsertPointsForAppendDelete(NewPolygon, ExtraPolygon, 0);

// NrPolygons is the resulting nr of polygons after appending

// *********************************************************************************
// ExtraPolygon (SubPolygon) is merged with the old NewPolygon into NewPolygon
// and will be deleted
// *********************************************************************************

					SubPolygon->Special.Test = 1;
					VerticesCount = NewPolygon->NrVertices + ExtraPolygon->NrVertices + 256;

					if (VerticesCount >= MaxNrVerticesPolygon)
					{
						if (AllocateMemPolygons(VerticesCount) != 0)
							return -101;
					}

					IncludePolygonInPolygon2(NewPolygon, ExtraPolygon);
// *********************************************************************************
// If there are more resulting polygons (NrPolygon>1) the largest polygon
// will be put in NewPolygon, the other polygons (Deletion polygons) will be copied to Buf

					MaxNr = 0;
					MaxArea = 0.0;

					for (cnt = 0; cnt < NrPolygons; cnt++)
					{
						SetMinMaxPolygon(WorkPolygon[cnt], 0);
						Area =
						    (WorkPolygon[cnt]->maxx - WorkPolygon[cnt]->minx) * (WorkPolygon[cnt]->maxy -
						            WorkPolygon[cnt]->miny);

						if (Area > MaxArea)
						{
							MaxArea = Area;
							MaxNr = cnt;
						}
					}

					CopyPolygonToPolygon(WorkPolygon[MaxNr], NewPolygon);

					if (NrPolygons > 1)
					{
						for (cnt4 = 0; cnt4 < NrPolygons; cnt4++)
						{
							if (cnt4 != MaxNr)
							{
								Mem2 = MemSizePolygon(WorkPolygon[cnt4]);

								if (ExtraDeletionPolygonsMemSize + Mem2 > MaxTempMemory)
								{
									if (AllocateMemTemp(ExtraDeletionPolygonsMemSize + Mem2 + 16384) != 0)
										return -1;
								}

								SpecialPolygon = (PolygonRecord *) & TempMem[ExtraDeletionPolygonsMemSize];
								CopyPolygonToPolygon(WorkPolygon[cnt4], SpecialPolygon);
								SetMinMaxPolygon(SpecialPolygon, 0);
								SpecialPolygon->PolygonType &= ~8;
								ExtraDeletionPolygonsMemSize += Mem2;
								NrExtraDeletionPolygons++;
								ExtraDeletionPolygonPos[NrExtraDeletionPolygons] = ExtraDeletionPolygonsMemSize;
							}
						}
					}

					if (Mode == 1)
						ok = 1;

//          if (ObjectNr==10) {
//            DrawTestPolygon(NewPolygon,2);
//            while (!KeyPressed()) CheckInputMessages();
//            ReadKeyFunction();
//          }
					Changed = 1;
					AppendingPolygonChanged = 1;
// ********************************************************************************************
// ********************************************************************************************
				}
				else
				{
					if (NrCrosses != 0)
					{
						GlobalNrCrosses = NrCrosses;

						if (NrCrosses == -1)
							return -10;

						if (NrCrosses == -2)
							return -11;

						return -20;
					}
				}

// ********************************************************************************************
// ********************************************************************************************
			}

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}

		TotalCount++;
	}

	if (!AppendingPolygonChanged)
		CopyPolygonToPolygon(PolygonObject, NewPolygon);

// *********************************************************************************
// End of finding the resulting Appending polygon
// *********************************************************************************

// *********************************************************************************
// If NewPolygon (resulting appending polygon) is inside an other appending polygon
// of the areafill then InsideAppendingPolygon = 1
//
	InsideAppendingPolygon = 0;
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	Mem2 = sizeof(AreaFillRecord);
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if (((SubPolygon->PolygonType & 8) == 8) && (SubPolygon->Special.Test == 0))
		{
			if (CheckPolygonCompleetlyInsidePolygon(NewPolygon, SubPolygon) == 1)
			{
				InsideAppendingPolygon = 1;
//        if (ObjectNr==523) {
//          DrawTestPolygon(PolygonObject,0);  // red
//          DrawTestPolygon(SubPolygon,1);  // green
//          while (!KeyPressed()) CheckInputMessages();
//          ReadKeyFunction();
//        }
			}
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		Mem2 += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
//
// Merge (cutting) resulting appending polygon NewPolygon with the deletion polygons
// NewPolygon (BufferPolygon) remains unchanged
//
// ********************************************************************************************

	if (ObjectNr == 649)
		ok = 1;

	DeletionPolygonChanged = 0;

	Changed = 1;
	TotalCount = 0;

	while ((Changed) && (TotalCount < 10000))
	{
		Changed = 0;
		SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) SubPolygon;

		for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
		{
			count = SubPolygon->NrVertices;
#ifdef _DEBUG

			if (count > 10000)
				ok = 1;

#endif

			if (((SubPolygon->PolygonType & 8) == 0)	// Deletion polygons
			        && (SubPolygon->Special.Test == 0))
			{
				/*
				   if ((ObjectNr==120)
				   &&
				   (cnt2==22)) {
				   res=1;
				   DrawTestPolygon(SubPolygon,1);  // green
				   DrawTestPolygon(NewPolygon,0);  // red
				   while (!KeyPressed()) CheckInputMessages();
				   ReadKeyFunction();
				   InvalidateRect(VIEWPLOTWindow,NULL,0);
				   PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
				   CheckInputMessages();
				   }
				 */
				VerticesCount = max(NewPolygon->NrVertices, SubPolygon->NrVertices) + 256;

				if (VerticesCount >= MaxNrVerticesPolygon)
				{
					if (AllocateMemPolygons(VerticesCount) != 0)
						return -100;
				}

				if (!ObjectsMinMaxOverlap3(SubPolygon, NewPolygon))
					NrCrosses = 0;
				else
				{
					/*
					   if (ObjectNr>=647) {
					   InvalidateRect(VIEWPLOTWindow,NULL,0);
					   PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
					   CheckInputMessages();
					   DrawTestPolygon(PolygonObject,2);  // yellow
					   DrawTestPolygon(SubPolygon,1);  // yellow
					   while (!KeyPressed()) CheckInputMessages();
					   ReadKeyFunction();
					   ok=1;
					   }
					 */
					CopyPolygonToPolygon(SubPolygon, ExtraPolygon);
					CopyPolygonToPolygon(NewPolygon, BufferPolygon);
					NrCrosses = CollectCrossesPolygons2(BufferPolygon, ExtraPolygon, 0);
				}

				if ((NrCrosses > 0) && (NrCrosses <= 8) && ((NrCrosses & 1) == 0))
				{
					NrCrossPoints = NrCrosses;
					DeletionPolygonChanged = 1;
					Changed = 1;

					// *********************************************************************************
					//  Get Insert points for appending polygon SubPolygon to NewPolygon (BufferPolygon)
					// *********************************************************************************
					NrPolygons = GetPolygonInsertPointsForAppendDelete(ExtraPolygon, BufferPolygon, 1);

					// NrPolygons is the resulting nr of polygons after appending

					// *********************************************************************************
					// SubPolygon is merged (cutting) with NewPolygon (BufferPolygon) and will be deleted
					// *********************************************************************************

					SubPolygon->Special.Test = 1;
					VerticesCount = NewPolygon->NrVertices + ExtraPolygon->NrVertices + 256;

					if (VerticesCount >= MaxNrVerticesPolygon)
					{
						if (AllocateMemPolygons(VerticesCount) != 0)
							return -101;
					}

					IncludePolygonInPolygon2(ExtraPolygon, BufferPolygon);

					// *********************************************************************************
					// The Polygons (Deletion polygons) will be copied to Buf

					for (cnt4 = 0; cnt4 < NrPolygons; cnt4++)
					{
						Mem2 = MemSizePolygon(WorkPolygon[cnt4]);

						if (ExtraDeletionPolygonsMemSize + Mem2 > MaxTempMemory)
						{
							if (AllocateMemTemp(ExtraDeletionPolygonsMemSize + Mem2 + 16384) != 0)
								return -1;
						}

						SpecialPolygon = (PolygonRecord *) & TempMem[ExtraDeletionPolygonsMemSize];
						CopyPolygonToPolygon(WorkPolygon[cnt4], SpecialPolygon);
						SetMinMaxPolygon(SpecialPolygon, 0);
						SpecialPolygon->PolygonType &= ~8;
						ExtraDeletionPolygonsMemSize += Mem2;
						NrExtraDeletionPolygons++;
						ExtraDeletionPolygonPos[NrExtraDeletionPolygons] = ExtraDeletionPolygonsMemSize;
						/*
						   if (ObjectNr>=627) {
						   InvalidateRect(VIEWPLOTWindow,NULL,0);
						   PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
						   CheckInputMessages();
						   DrawTestPolygon(WorkPolygon[cnt4],2);  // yellow
						   while (!KeyPressed()) CheckInputMessages();
						   ReadKeyFunction();
						   }
						 */
					}

					if (Mode == 1)
						ok = 1;

					//          if (ObjectNr==10) {
					//            DrawTestPolygon(NewPolygon,2);
					//            while (!KeyPressed()) CheckInputMessages();
					//            ReadKeyFunction();
					//          }
					// ********************************************************************************************
					// ********************************************************************************************
				}
				else
				{
					if (NrCrosses != 0)
					{
						GlobalNrCrosses = NrCrosses;

						if (NrCrosses == -1)
							return -10;

						if (NrCrosses == -2)
							return -11;

						return -20;
					}
				}

				// ********************************************************************************************
				// ********************************************************************************************
			}

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}

		TotalCount++;
	}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
//
// Merge (cutting) PolygonObject with the deletion polygons
//
//
// ********************************************************************************************


	Changed = 1;
	TotalCount = 0;

	while ((Changed) && (TotalCount < 10000))
	{
		Changed = 0;
		SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) SubPolygon;

		for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
		{
			count = SubPolygon->NrVertices;
#ifdef _DEBUG

			if (count > 10000)
				ok = 1;

#endif

			if (((SubPolygon->PolygonType & 8) == 0)	// Deletion polygons
			        && (SubPolygon->Special.Test == 0))
			{
				/*
				   if ((ObjectNr==120)
				   &&
				   (cnt2==22)) {
				   res=1;
				   DrawTestPolygon(SubPolygon,1);  // green
				   DrawTestPolygon(NewPolygon,0);  // red
				   while (!KeyPressed()) CheckInputMessages();
				   ReadKeyFunction();
				   InvalidateRect(VIEWPLOTWindow,NULL,0);
				   PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
				   CheckInputMessages();
				   }
				 */
				VerticesCount = max(PolygonObject->NrVertices, SubPolygon->NrVertices) + 256;

				if (VerticesCount >= MaxNrVerticesPolygon)
				{
					if (AllocateMemPolygons(VerticesCount) != 0)
						return -100;
				}

				if (!ObjectsMinMaxOverlap3(SubPolygon, PolygonObject))
					NrCrosses = 0;
				else
				{
					/*
					   if (ObjectNr>=647) {
					   InvalidateRect(VIEWPLOTWindow,NULL,0);
					   PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
					   CheckInputMessages();
					   DrawTestPolygon(PolygonObject,2);  // yellow
					   DrawTestPolygon(SubPolygon,1);  // yellow
					   while (!KeyPressed()) CheckInputMessages();
					   ReadKeyFunction();
					   ok=1;
					   }
					 */
					CopyPolygonToPolygon(SubPolygon, ExtraPolygon);
					CopyPolygonToPolygon(PolygonObject, BufferPolygon);
					NrCrosses = CollectCrossesPolygons2(BufferPolygon, ExtraPolygon, 0);
				}

				if ((NrCrosses > 0) && (NrCrosses <= 8) && ((NrCrosses & 1) == 0))
				{
					NrCrossPoints = NrCrosses;
					DeletionPolygonChanged = 1;
					Changed = 1;

					// *********************************************************************************
					//  Get Insert points for appending polygon SubPolygon to PolygonObject (BufferPolygon)
					// *********************************************************************************
					NrPolygons = GetPolygonInsertPointsForAppendDelete(ExtraPolygon, BufferPolygon, 1);

					// NrPolygons is the resulting nr of polygons after appending

					// *********************************************************************************
					// SubPolygon is merged (cutting) with PolygonObject (BufferPolygon) and will be deleted
					// *********************************************************************************

					SubPolygon->Special.Test = 1;
					VerticesCount = PolygonObject->NrVertices + ExtraPolygon->NrVertices + 256;

					if (VerticesCount >= MaxNrVerticesPolygon)
					{
						if (AllocateMemPolygons(VerticesCount) != 0)
							return -101;
					}

					IncludePolygonInPolygon2(ExtraPolygon, BufferPolygon);

					// *********************************************************************************
					// The Polygons (Deletion polygons) will be copied to Buf

					for (cnt4 = 0; cnt4 < NrPolygons; cnt4++)
					{
						Mem2 = MemSizePolygon(WorkPolygon[cnt4]);

						if (ExtraDeletionPolygonsMemSize + Mem2 > MaxTempMemory)
						{
							if (AllocateMemTemp(ExtraDeletionPolygonsMemSize + Mem2 + 16384) != 0)
								return -1;
						}

						SpecialPolygon = (PolygonRecord *) & TempMem[ExtraDeletionPolygonsMemSize];
						CopyPolygonToPolygon(WorkPolygon[cnt4], SpecialPolygon);
						SetMinMaxPolygon(SpecialPolygon, 0);
						SpecialPolygon->PolygonType &= ~8;
						ExtraDeletionPolygonsMemSize += Mem2;
						NrExtraDeletionPolygons++;
						ExtraDeletionPolygonPos[NrExtraDeletionPolygons] = ExtraDeletionPolygonsMemSize;
						/*
						   if (ObjectNr>=627) {
						   InvalidateRect(VIEWPLOTWindow,NULL,0);
						   PostMessage(VIEWPLOTWindow,WM_PAINT,(WPARAM)NULL,(LPARAM)NULL);
						   CheckInputMessages();
						   DrawTestPolygon(WorkPolygon[cnt4],2);  // yellow
						   while (!KeyPressed()) CheckInputMessages();
						   ReadKeyFunction();
						   }
						 */
					}

					if (Mode == 1)
						ok = 1;

					//          if (ObjectNr==10) {
					//            DrawTestPolygon(NewPolygon,2);
					//            while (!KeyPressed()) CheckInputMessages();
					//            ReadKeyFunction();
					//          }
					// ********************************************************************************************
					// ********************************************************************************************
				}
				else
				{
					if (NrCrosses != 0)
					{
						GlobalNrCrosses = NrCrosses;

						if (NrCrosses == -1)
							return -10;

						if (NrCrosses == -2)
							return -11;

						return -20;
					}
				}

				// ********************************************************************************************
				// ********************************************************************************************
			}

			PolygonPos += MemSizePolygon(SubPolygon);
			SubPolygon = (PolygonRecord *) PolygonPos;
		}

		TotalCount++;
	}

// *********************************************************************************
// If NewPolygon (resulting appending polygon) is inside an other appending polygon
// of the areafill then InsideAppendingPolygon = 1
//
	AppendingPolygonInsideDeletionPolygon = 0;
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	Mem2 = sizeof(AreaFillRecord);
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if (((SubPolygon->PolygonType & 8) == 0) && (SubPolygon->Special.Test == 0))
		{
			if (CheckPolygonCompleetlyInsidePolygon(NewPolygon, SubPolygon) == 1)
			{
				AppendingPolygonInsideDeletionPolygon = 1;
//        if (ObjectNr==523) {
//          DrawTestPolygon(PolygonObject,0);  // red
//          DrawTestPolygon(SubPolygon,1);  // green
//          while (!KeyPressed()) CheckInputMessages();
//          ReadKeyFunction();
//        }
			}
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		Mem2 += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

// *********************************************************************************
// If NewPolygon (Unchanged PolygonObject) is inside an other appending polygon
// and does not overlap other appending polygons
// and NewPolygon does not overlap a deletion polygon
// and NewPolygon not inside a deletion polygon
// and none of the deletion polygons is inside NewPolygon then
// PolygonObject will not be included

	if ((InsideAppendingPolygon) && (!SubPolygonInsidePolygonObject) && (!AppendingPolygonChanged)
	        && (!AppendingPolygonInsideDeletionPolygon) && (!DeletionPolygonChanged))
		return 3;


// *********************************************************************************
// NewPolygon (Unchanged PolygonObject) does not overlap appending or deletion polygons
// and none of the deletion polygons is inside NewPolygon then append NewPolygon

	if ((!SubPolygonInsidePolygonObject) && (!AppendingPolygonChanged) && (!DeletionPolygonChanged))
	{
		if ((!InsideAppendingPolygon) || (AppendingPolygonInsideDeletionPolygon))
		{
			MemSizeAreaFill = TempAreaFill->MemSize + MemSizePolygon(NewPolygon);

			if (MemSizeAreaFill >= MaxAreaFillMemoryTemp)
			{
				if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill) != 0)
					return -105;

				NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
				TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
			}

			count = TempAreaFill->MemSize;
			memmove(NewAreaFill, TempAreaFill, count);
			TempPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + count);
			CopyPolygonToPolygon(NewPolygon, TempPolygon);
			SetMinMaxPolygon(TempPolygon, 0);
			TempPolygon->PolygonType |= 8;
			NewAreaFill->MemSize += MemSizePolygon(TempPolygon);
			NewAreaFill->NrPolygons++;
			return 2;
		}
	}

//    if (ObjectNr==10) {
//      DrawTestPolygon(PolygonObject,0);  // red
//      DrawTestPolygon(SurroundPolygon,1);  // green
//      DrawTestPolygon(ResultPolygon,2);  // yellow
//      while (!KeyPressed()) CheckInputMessages();
//      ReadKeyFunction();
//    }

// ********************************************************************************************
// ********************************************************************************************
//
// Start building the new areafill out of not deleted SubPolygons and
// the copied Polygons in Buf
//
// When PolygonObject overlapped other appending polygons (AppendingPolygonChanged=1)
// the resulting polygon is NewPolygon and this NewPolygon did not overlap deletion polygons
// and NewPolygon is not inside a bigger appening polygon NewPolygon will be added as a
// appending polygon
//
// ********************************************************************************************

	MemSizeAreaFill = sizeof(AreaFillRecord);
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if (SubPolygon->Special.Test == 0)
			MemSizeAreaFill += MemSizePolygon(SubPolygon);

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	MemSizeAreaFill += ExtraDeletionPolygonsMemSize + 3172;
	MemSizeAreaFill += MemSizePolygon(NewPolygon);

	if (MemSizeAreaFill >= MaxAreaFillMemoryTemp)
	{
		if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill) != 0)
			return -105;

		NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
		TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
	}

// ********************************************************************************************
// ********************************************************************************************

	if (NewPolygon->NrVertices > 10000)
		ok = 1;

	memmove(NewAreaFill, TempAreaFill, sizeof(AreaFillRecord));
	NewAreaFill->MemSize = sizeof(AreaFillRecord);
	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygonPos = (uint8 *) AreaFillPolygon;
	NewAreaFill->NrPolygons = 0;


// Copy not deleted SubPolygons to NewAreaFill
	SubPolygon = (PolygonRecord *) ((uint8 *) TempAreaFill + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) SubPolygon;

	for (cnt2 = 0; cnt2 < TempAreaFill->NrPolygons; cnt2++)
	{
		if (SubPolygon->Special.Test == 0)
		{
			NewAreaFill->MemSize += MemSizePolygon(SubPolygon);
			NewAreaFill->NrPolygons++;
			CopyPolygonToPolygon(SubPolygon, AreaFillPolygon);
			AreaFillPolygon->PolygonType &= 8;
			AreaFillPolygonPos += MemSizePolygon(SubPolygon);
			AreaFillPolygon = (PolygonRecord *) AreaFillPolygonPos;
		}

		PolygonPos += MemSizePolygon(SubPolygon);
		SubPolygon = (PolygonRecord *) PolygonPos;
	}

	if (ObjectNr == 2)
		ok = 1;

// ********************************************************************************************
// Append polygons (deletion) stored in Buf

	for (cnt2 = 0; cnt2 < NrExtraDeletionPolygons; cnt2++)
	{
		SpecialPolygon = (PolygonRecord *) & TempMem[ExtraDeletionPolygonPos[cnt2]];
		/*
		    if (ObjectNr>645) {
		      DrawTestPolygon(SpecialPolygon,1);  // green
		      while (!KeyPressed()) CheckInputMessages();
		      ReadKeyFunction();
		    }
		*/
		CopyPolygonToPolygon(SpecialPolygon, AreaFillPolygon);
		AreaFillPolygon->PolygonType &= ~8;
		NewAreaFill->MemSize += MemSizePolygon(SpecialPolygon);
		NewAreaFill->NrPolygons++;
		AreaFillPolygonPos += MemSizePolygon(SpecialPolygon);
		AreaFillPolygon = (PolygonRecord *) AreaFillPolygonPos;
	}

// ********************************************************************************************
// Append NewPolyon
	if ((!InsideAppendingPolygon) || ((AppendingPolygonChanged) && (!DeletionPolygonChanged)))
	{
		CopyPolygonToPolygon(NewPolygon, AreaFillPolygon);
		AreaFillPolygon->PolygonType |= 8;

		if (NewPolygon->NrVertices > 0)
		{
			NewAreaFill->MemSize += MemSizePolygon(NewPolygon);
			NewAreaFill->NrPolygons++;
			AreaFillPolygonPos += MemSizePolygon(NewPolygon);
			AreaFillPolygon = (PolygonRecord *) AreaFillPolygonPos;
		}
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DeleteFromObjectPolygon(int32 mode)
{
	int32 ok, cnt, count, Found, Found2, res, MemSize, NewPolygonInside, SubPolygonSize, NewObjectPolygonLength;
	double x1, y1;
	PolygonRecord *AreaFillPolygon;
	PolygonRecord *PolygonObject, *DeletionPolygon;
	ObjectRecord *Object, PinObject;
	int32 FoundError;
	uint8 *Bufp1, *Bufp2;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectSubPolygonRecord *ObjectSubPolygon;


	MemSize = 128 * 1024;
	AllocateSpecialMem(MEM_POLYGON1, MemSize, (void **) &PolygonObject);
	AllocateSpecialMem(MEM_POLYGON2, MemSize, (void **) &DeletionPolygon);
//  AreaFill=(AreaFillRecord *)&(AreaFillMem[(*AreaFills)[AreaFillNr]]);
	FoundError = 0;
	Object = NULL;
	ObjectPolygon = NULL;
	Found = -1;
	Found2 = -1;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			Found = cnt;
	}

	if (Found == -1)
	{
		for (cnt = 0; cnt < NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				Found2 = cnt;
		}

		if (Found2 == -1)
			return 0;

		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[Found2]]);
		count = ObjectPolygon->NrVertices;

		if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
			count = ObjectPolygon->NrVerticesMainPolygon;

		if (count < (MemSize / 16) - 100)
			return 0;

		for (cnt = 0; cnt < count; cnt++)
		{
			x1 = ObjectPolygon->Points[cnt].x;
			y1 = ObjectPolygon->Points[cnt].y;
			(*PolygonObject).Points[cnt].x = x1;
			(*PolygonObject).Points[cnt].y = y1;
		}

		PolygonObject->NrVertices = count;
		SetMinMaxPolygon(PolygonObject, 0);
	}
	else
	{
		Object = &((*Objects)[Found]);
		MakePolygonFromPlotObject(Object, PolygonObject, 0.0, 32, 32);
	}


	CommandAddObjects(OBJECT_POLYGON, 0, SILKSCREEN_TOP_LAYER, 32);

	NewObjectPolygon->Info = 0;
	count = NewObjectPolygon->NrVertices;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = NewObjectPolygon->Points[cnt].x;
		y1 = NewObjectPolygon->Points[cnt].y;
		(*DeletionPolygon).Points[cnt].x = x1;
		(*DeletionPolygon).Points[cnt].y = y1;
	}

	DeletionPolygon->NrVertices = count;
	SetMinMaxPolygon(DeletionPolygon, 0);


	if (CheckPolygonCompleetlyOutsidePolygon(DeletionPolygon, PolygonObject) == 1)
		return 0;

	NewPolygonInside = 0;

	if (CheckPolygonCompleetlyInsidePolygon(DeletionPolygon, PolygonObject) == 1)
		NewPolygonInside = 1;

	if (AllocateMemAreaFillMemoryTemp(128 * 1024) != 0)
		return -1;

	if (AllocateMemPolygons(0) != 0)
		return -1;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	memset(NewAreaFill, 0, sizeof(*NewAreaFill));
	NewAreaFill->NrPolygons = 1;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	memset(AreaFillPolygon, 0, sizeof(PolygonRecord));
	CopyPolygonToPolygon(PolygonObject, AreaFillPolygon);
	AreaFillPolygon->PolygonType = 0;
	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);


	memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
	res = MergePolygon(DeletionPolygon, 0);

	if (res < 0)
		FoundError = 1;

	if (!FoundError)
	{
		if (!SelectionEsc)
		{
//      if (!AddAreaFill(0)) FoundError=1;
		}
		else
		{
			SelectionEsc = 0;
			FoundError = 1;
		}
	}
	else
		ok = 1;

	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));


	count = AreaFillPolygon->NrVertices;
	memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = AreaFillPolygon->Points[cnt].x;
		y1 = AreaFillPolygon->Points[cnt].y;
		NewObjectPolygon->Points[cnt].x = x1;
		NewObjectPolygon->Points[cnt].y = y1;
	}

	NewObjectPolygon->NrVertices = count;
	NewObjectPolygonLength = MemSizeObjectPolygon(NewObjectPolygon);

//  ObjectPolygon->NrSubPolygons=1;
//  ObjectPolygon->NrVerticesMainPolygon=ObjectPolygon->NrVertices-2;
	if (Found == -1)
	{
		if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
		{
			count = ObjectPolygon->NrVertices;
			Bufp1 = (uint8 *) ObjectPolygon + sizeof(ObjectPolygonInitRecord);
			Bufp1 += ObjectPolygon->NrVerticesMainPolygon * sizeof(PointRecord);
			Bufp2 = (uint8 *) NewObjectPolygon + NewObjectPolygonLength;
			memcpy(Bufp2, Bufp1, (count - ObjectPolygon->NrVerticesMainPolygon) * sizeof(PointRecord));
			NewObjectPolygon->NrVerticesMainPolygon = NewObjectPolygon->NrVertices;
			NewObjectPolygon->NrVertices += count - ObjectPolygon->NrVerticesMainPolygon;
			NewObjectPolygon->NrSubPolygons = ObjectPolygon->NrSubPolygons;
		}
	}
	else
	{
		/*
		    count=PolygonObject->NrVertices;
		    Bufp1=(uint8 *)PolygonObject+sizeof(PolygonInitRecord);
		    Bufp1+=PolygonObject->NrVertices*sizeof(PointRecord);
		    Bufp2=(uint8 *)NewObjectPolygon+NewObjectPolygonLength;
		    memcpy(Bufp2,Bufp1,count*sizeof(PointRecord));
		    NewObjectPolygon->NrVerticesMainPolygon=count;
		    NewObjectPolygon->NrVertices=count;
		    NewObjectPolygon->NrSubPolygons=0;
		*/
	}

	if (NewPolygonInside)
	{
		count = DeletionPolygon->NrVertices;
		Bufp2 = (uint8 *) NewObjectPolygon + MemSizeObjectPolygon(NewObjectPolygon);
		ObjectSubPolygon = (ObjectSubPolygonRecord *) Bufp2;
		SubPolygonSize = sizeof(ObjectSubPolygonInitRecord) + count * sizeof(PointRecord);
		ObjectSubPolygon->Magic = SUB_POLYGON_MAGIC;
		ObjectSubPolygon->NotUsed1 = 0;
		ObjectSubPolygon->NotUsed2 = 0;
		ObjectSubPolygon->NrVertices = count;

		for (cnt = 0; cnt < count; cnt++)
		{
			ObjectSubPolygon->Points[cnt].x = DeletionPolygon->Points[cnt].x;
			ObjectSubPolygon->Points[cnt].y = DeletionPolygon->Points[cnt].y;
		}

		if (ObjectPolygon->NrSubPolygons == 0)
		{
			NewObjectPolygon->NrSubPolygons++;
			NewObjectPolygon->NrVerticesMainPolygon = NewObjectPolygon->NrVertices;
		}

		NewObjectPolygon->NrVertices += SubPolygonSize / sizeof(PointRecord);
	}

	if (Found != -1)
	{
		NewObjectPolygon->Layer = Object->Layer;
		NewObjectPolygon->Clearance = (float) Object->Clearance;
		NewObjectPolygon->PinNr = Object->PinNr;
		PinObject.Layer = Object->Layer;
		PinObject.PinNr = Object->PinNr;
	}
	else
	{
		NewObjectPolygon->Layer = ObjectPolygon->Layer;
		NewObjectPolygon->Clearance = ObjectPolygon->Clearance;
		NewObjectPolygon->PinNr = ObjectPolygon->PinNr;
		PinObject.Layer = ObjectPolygon->Layer;
		PinObject.PinNr = ObjectPolygon->PinNr;
	}

	if (AddObjectPolygon(NewObjectPolygon))
	{
		StartDrawingEditingWindow();

		if (Found != -1)
		{
			Object = &((*Objects)[Found]);
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
			Object->DeleteNr = (int16) LastActionNr;
		}
		else
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[Found2]]);
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			SetBackGroundActive(0);
			DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
			SetBackGroundActive(0);
			PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
			PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
			DrawPinTextObject(&PinObject, 0);

			if (ClearanceVisible)
			{
				SetBackGroundActive(0);
				DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, 0);
			}

			ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
			ObjectPolygon->DeleteNr = (int16) LastActionNr;
		}

		BackGroundActive = 0;
		DrawObjectPolygon(NewObjectPolygon, (float) 0.0, (float) 0.0, 0);
		PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
		PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
		DrawPinTextObject(&PinObject, 0);

		if (ClearanceVisible)
			DrawPolygonObjectWithClearance(NewObjectPolygon, 0.0, 0.0, 0);

		ExitDrawing();
		EndDrawingEditingWindow();
	}

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();

	if (FoundError)
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MergeObjectsToPolygon(int32 mode)
{
	int32 cnt, cnt2, count, res, FoundLayer, FirstPinNr, MemSize, FoundError;
	double x1, y1, FilledArea, FilledAreaTemp, MaxClearance;
	PolygonRecord *AreaFillPolygon;
	PolygonRecord *PolygonObject, *MaxAreaPolygon, *DeletionPolygon;
	ObjectRecord *Object, PinObject;
	uint8 *AreafillPos;
	ObjectPolygonRecord *ObjectPolygon;

	MemSize = 128 * 1024;
	AllocateSpecialMem(MEM_POLYGON1, MemSize, (void **) &PolygonObject);
	AllocateSpecialMem(MEM_POLYGON2, MemSize, (void **) &DeletionPolygon);

//  AreaFill=(AreaFillRecord *)&(AreaFillMem[(*AreaFills)[AreaFillNr]]);
	FoundError = 0;
	MaxAreaPolygon = NULL;
	MaxClearance = 0.0;
	FoundLayer = -1;
	FirstPinNr = -1;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (FoundLayer == -1)
				FoundLayer = Object->Layer;
			else
			{
				if (Object->Layer != FoundLayer)
				{
					MessageBoxUTF8(GEOMWindow, "Selected objects should be on one layer only", SC(48, "Error"),
					               MB_APPLMODAL | MB_OK);
					return -1;
				}
			}

			if (Object->PinNr != -1)
			{
				if (FirstPinNr == -1)
					FirstPinNr = Object->PinNr;
				else
				{
					if (Object->PinNr != FirstPinNr)
					{
						MessageBoxUTF8(GEOMWindow, "Selected objects should have the same pinnumber", SC(48, "Error"),
						               MB_APPLMODAL | MB_OK);
						return -1;
					}
				}
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (FoundLayer == -1)
				FoundLayer = ObjectPolygon->Layer;
			else
			{
				if (ObjectPolygon->Layer != FoundLayer)
				{
					MessageBoxUTF8(GEOMWindow, "Selected objects should be on one layer only", SC(48, "Error"),
					               MB_APPLMODAL | MB_OK);
					return -1;
				}
			}

			if (ObjectPolygon->PinNr != -1)
			{
				if (FirstPinNr == -1)
					FirstPinNr = ObjectPolygon->PinNr;
				else
				{
					if (ObjectPolygon->PinNr != FirstPinNr)
					{
						MessageBoxUTF8(GEOMWindow, "Selected objects should have the same pinnumber", SC(48, "Error"),
						               MB_APPLMODAL | MB_OK);
						return -1;
					}
				}
			}
		}
	}

	switch (FoundLayer)
	{
	case PLACEMENT_OUTLINE_LAYER:
	case BOARD_OUTLINE_LAYER:
	case PIN_TEXT_LAYER:
	case GEOM_NAME_LAYER:
	case DRILL_LAYER:
	case DRILL_UNPLATED_LAYER:
	case POWER_PAD_LAYER:
		MessageBoxUTF8(GEOMWindow, "Can not merge objects one this layer", SC(48, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	if (AllocateMemAreaFillMemoryTemp(128 * 1024) != 0)
		return -1;

	if (AllocateMemPolygons(0) != 0)
		return -1;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	memset(NewAreaFill, 0, sizeof(*NewAreaFill));
	NewAreaFill->NrPolygons = 1;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	memset(AreaFillPolygon, 0, sizeof(PolygonRecord));
	AreaFillPolygon->PolygonType = 0;
	AreaFillPolygon->NrVertices = 4;
	AreaFillPolygon->Points[0].x = -10000.0e5;
	AreaFillPolygon->Points[0].y = -10000.0e5;
	AreaFillPolygon->Points[1].x = 10000.0e5;
	AreaFillPolygon->Points[1].y = -10000.0e5;
	AreaFillPolygon->Points[2].x = 10000.0e5;
	AreaFillPolygon->Points[2].y = 10000.0e5;
	AreaFillPolygon->Points[3].x = -10000.0e5;
	AreaFillPolygon->Points[3].y = 10000.0e9;
	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);


	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			MaxClearance = max(MaxClearance, Object->Clearance);
			MakePolygonFromPlotObject(Object, PolygonObject, 0.0, 32, 32);
			PolygonObject->PolygonType = 0;
			memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
			res = MergePolygon(PolygonObject, 0);

			if (res < 0)
				FoundError = 1;

			if (!FoundError)
			{
				if (!SelectionEsc)
				{
					//      if (!AddAreaFill(0)) FoundError=1;
				}
				else
				{
					SelectionEsc = 0;
					FoundError = 1;
				}
			}
			else
				ok = 1;
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			MaxClearance = max(MaxClearance, ObjectPolygon->Clearance);
			count = ObjectPolygon->NrVertices;

			if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
				count = ObjectPolygon->NrVerticesMainPolygon;

			if (count < (MemSize / 16) - 100)
			{
				for (cnt2 = 0; cnt2 < count; cnt2++)
				{
					x1 = ObjectPolygon->Points[cnt2].x;
					y1 = ObjectPolygon->Points[cnt2].y;
					(*PolygonObject).Points[cnt2].x = x1;
					(*PolygonObject).Points[cnt2].y = y1;
				}

				PolygonObject->NrVertices = count;
				SetMinMaxPolygon(PolygonObject, 0);
				PolygonObject->PolygonType = 0;
				memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
				res = MergePolygon(PolygonObject, 0);

				if (res < 0)
					FoundError = 1;

				if (!FoundError)
				{
					if (!SelectionEsc)
					{
						//      if (!AddAreaFill(0)) FoundError=1;
					}
					else
					{
						SelectionEsc = 0;
						FoundError = 1;
					}
				}
				else
					ok = 1;
			}
		}
	}

	if (NewAreaFill->NrPolygons != 2)
	{
		MessageBoxUTF8(GEOMWindow, "Selected objects should overlap each other", SC(48, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	FilledArea = 0.0;
	AreafillPos = ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	AreaFillPolygon = (PolygonRecord *) AreafillPos;

	for (cnt = 0; cnt < NewAreaFill->NrPolygons; cnt++)
	{
		AreafillPos += MemSizePolygon(AreaFillPolygon);
		AreaFillPolygon = (PolygonRecord *) AreafillPos;
		FilledAreaTemp =
		    (AreaFillPolygon->maxx - AreaFillPolygon->minx) * (AreaFillPolygon->maxy - AreaFillPolygon->miny);

		if (FilledAreaTemp > FilledArea)
		{
			FilledArea = FilledAreaTemp;
			MaxAreaPolygon = AreaFillPolygon;
		}
	}

	count = MaxAreaPolygon->NrVertices;
	memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = MaxAreaPolygon->Points[cnt].x;
		y1 = MaxAreaPolygon->Points[cnt].y;
		NewObjectPolygon->Points[cnt].x = x1;
		NewObjectPolygon->Points[cnt].y = y1;
	}

	NewObjectPolygon->NrVertices = count;

	NewObjectPolygon->Layer = FoundLayer;
	NewObjectPolygon->Clearance = (float) MaxClearance;
	NewObjectPolygon->PinNr = FirstPinNr;

	if (AddObjectPolygon(NewObjectPolygon))
	{
		StartDrawingEditingWindow();

		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				Object = &((*Objects)[cnt]);
				Object->Info = ~OBJECT_SELECTED;
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
				Object->DeleteNr = (int16) LastActionNr;
			}
		}

		for (cnt = 0; cnt < NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectPolygon->Info = ~OBJECT_SELECTED;
				SetBackGroundActive(0);
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
				SetBackGroundActive(0);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				DrawPinTextObject(&PinObject, 0);

				if (ClearanceVisible)
				{
					SetBackGroundActive(0);
					DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, 0);
				}

				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
			}
		}

		BackGroundActive = 0;
		DrawObjectPolygon(NewObjectPolygon, 0.0, 0.0, 0);
		PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
		PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
		DrawPinTextObject(&PinObject, 0);

		if (ClearanceVisible)
			DrawPolygonObjectWithClearance(NewObjectPolygon, 0.0, 0.0, 0);

		ExitDrawing();
		EndDrawingEditingWindow();
	}

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();

	if (FoundError)
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
