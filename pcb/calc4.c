/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc4.c
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
#include "calcdef.h"
#include "string.h"
#include "math.h"
#include "float.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "files2.h"
#include "toets.h"
#include "memory.h"
#include "mainloop.h"
#include "stdio.h"
#include "ellipss.h"
#include "line2.h"
#include "memory.h"
#include "pcb.h"
#include "draw.h"
#include "polygon.h"
#include "graphics.h"
#include "owntime.h"


#define  NotInRange4(x1,x2) ( (((x1>x2-1000.0) && (x1<x2+1000.0))) ? (0) : (1) )


#define  CircleDiameterBig     400000.0
#define  MAX_NR_CACHE_POLYGON_OBJECTS     64

typedef struct
{
	double X, Y, Rotation, Clearance;
	int32 Mirror, MemSize;
	uint32 Address;
	AreaFillRecord *AreaFill;
} CachePolygonObjectRecord;

typedef struct
{
	int32 NrLines, dummy;
	double x, y;
} LinePointRecord;

typedef LinePointRecord LinePointsArray[1000];

typedef int32 LinePointsNumbersArray[4096];

extern double LineCrossX, LineCrossY, OldValue2;

double PolygonPointAddValueX = 0.00105631372;
double PolygonPointAddValueY = 0.00156578542;
double PolygonPointMultValue = 1.00017201367;

extern HDC OutputDisplay;

//int32  NrCalcs1,NrCalcs2,NrCalcs3;

LinePointsNumbersArray *LinePointsNumbers;
int32 NrLinePoints, ok;

LinePointsArray *LinePoints;

uint8 GeomPolygonBuf[10240];

PolygonRecord *SpecialGeomPolygon = (PolygonRecord *) & GeomPolygonBuf;

int32 NrCachePolygonObjects, NrCachePolygonObjectsUpdate;
CachePolygonObjectRecord CachePolygonObjects[MAX_NR_CACHE_POLYGON_OBJECTS];


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

/*

Two crossing lines :

Line 1: x1,y1 - x2,y2
Line 2: x3,y3 - x4,y4

y=(x-x1)*a1+y1   a1=(y2-y1)/(x2-x1)

y=(x-x3)*a2+y3   a2=(y4-y3)/(x4-x3)

           x1*a1-y1-x3*a2+y3
CenterX = -------------------
                 a1-a2

*/

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LineCrossesNew(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                     double LineX4, double LineY4)
//  -1  Lines dont cross
//   0  Lines overlap
//   1  Lines cross
{
	double a, b, x, y;
	double minx12, miny12, maxx12, maxy12, minx34, miny34, maxx34, maxy34;

//  LineCrossX=-1000000.0;
//  LineCrossY=-1000000.0;

	minx12 = min(LineX1, LineX2);
	maxx34 = max(LineX3, LineX4);
	miny12 = min(LineY1, LineY2);
	maxy34 = max(LineY3, LineY4);
	maxx12 = max(LineX1, LineX2);
	minx34 = min(LineX3, LineX4);
	miny34 = min(LineY3, LineY4);
	maxy12 = max(LineY1, LineY2);

	/*
	   if ((InRange(LineY2,LineY1))
	   &&
	   (InRange(LineY4,LineY3))) return 0;   // hor

	   if ((InRange(LineX2,LineX1))
	   &&
	   (InRange(LineX4,LineX3))) return 0;   // ver

	   if ((InRange(LineX2-LineX1,LineY2-LineY1))
	   &&
	   (InRange(LineX4-LineX3,LineY4-LineY3))
	   &&
	   (InRange(-LineX3+LineY3,-LineX1+LineY1))) return 0;  // diag2

	   if ((InRange(LineX2-LineX1,LineY1-LineY2))
	   &&
	   (InRange(LineX4-LineX3,LineY3-LineY4))
	   &&
	   (InRange(LineX3+LineY3,LineX1+LineY1))) return 0;    // diag1
	 */

	if ((LineX2 > LineX1 - 0.0001) && (LineX2 < LineX1 + 0.0001))
	{
//  if (InRange7(LineX2,LineX1)) {
		if (InRange7(LineY3, LineY4))
		{
			LineCrossX = LineX1;
			LineCrossY = LineY3;
			return 1;
		}
		else
		{
			if (InRange7(LineX4, LineX3))
				return -1;

			b = (LineY4 - LineY3) / (LineX4 - LineX3);
			x = LineX1;
			y = (x - LineX3) * b + LineY3;

			if ((y > maxy12) || (y < miny12))
				return -1;

			if (fabs(b) > 1)
			{	// almost vertical line check y
				if ((y > maxy34) || (y < miny34))
					return -1;
			}
			else
			{	// almost vertical line check x
				if ((x > maxx34) || (x < minx34))
					return -1;
			}
		}
	}
	else
	{
		if (InRange7(LineX4, LineX3))
		{
			if (InRange7(LineY1, LineY2))
			{
				LineCrossX = LineX3;
				LineCrossY = LineY1;
				return 1;
			}
			else
			{
				a = (LineY2 - LineY1) / (LineX2 - LineX1);
				x = LineX3;
				y = (x - LineX1) * a + LineY1;

				if ((y > maxy34) || (y < miny34))
					return -1;

				if (fabs(a) > 1)
				{	// almost vertical line check y
					if ((y > maxy12) || (y < miny12))
						return -1;
				}
				else
				{	// almost vertical line check x
					if ((x > maxx12) || (x < minx12))
						return -1;
				}
			}
		}
		else
		{
			if ((LineX2 > LineX1 - 0.01) && (LineX2 < LineX1 + 0.01))
			{
				b = (LineY4 - LineY3) / (LineX4 - LineX3);
				x = LineX1;
				y = (x - LineX3) * b + LineY3;

				if ((y > maxy12) || (y < miny12))
					return -1;
			}
			else
			{
				a = (LineY2 - LineY1) / (LineX2 - LineX1);
				x = a * LineX1;
				x -= LineY1;
				b = (LineY4 - LineY3) / (LineX4 - LineX3);

				if (a == b)
					return -1;

				x += LineY3;
				x += -b * LineX3;
				x = x / (a - b);

				y = (x - LineX1) * a + LineY1;

				if (fabs(a) > 1)
				{	// almost vertical line check y
					if ((y > maxy12) || (y < miny12))
						return -1;

					if (fabs(b) > 1)
					{	// almost vertical line check y
						if ((y > maxy34) || (y < miny34))
							return -1;
					}
					else
					{	// almost vertical line check x
						if ((x > maxx34) || (x < minx34))
							return -1;
					}
				}
				else
				{	// almost vertical line check x
					if ((x > maxx12) || (x < minx12))
						return -1;

					if (fabs(b) > 1)
					{	// almost vertical line check y
						if ((y > maxy34) || (y < miny34))
							return -1;
					}
					else
					{	// almost vertical line check x
						if ((x > maxx34) || (x < minx34))
							return -1;
					}
				}
			}
		}
	}

	LineCrossX = x;
	LineCrossY = y;

	return 1;

	/*
	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	           (y2-y1)
	y = (x-x1)*------- + y1
	           (x2-x1)


	b = y1*(x2-x1) - x1*(y2-y1)


	2 lines crosses  (x1,y1) (x2,y2)   and  (x3,y3) (x4,y4)



	*/
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LineCrosses2(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                   double LineX4, double LineY4, double *CenterX, double *CenterY, int32 mode)
//   0  Lines do not cross
//   1  Lines cross
{
	double a1, a2, b1, b2, Angle1, Length1, Angle2, Length2;

	ConvNormalCoorToPolar(LineX1, LineY1, LineX2, LineY2, &Angle1, &Length1);
	ConvNormalCoorToPolar(LineX3, LineY3, LineX4, LineY4, &Angle2, &Length2);

	*CenterX = 0.0;
	*CenterY = 0.0;

	if ((InRangeSpecial(Angle1, Angle2, 0.001)) || (InRangeSpecial(Angle1, Angle2 + ANGLE_180, 0.001))
	        || (InRangeSpecial(Angle1 + ANGLE_180, Angle2, 0.001)))
		return 0;

	if (InRangeSpecial(LineX1, LineX2, 0.00001))
	{	// Vertical line
		a2 = (LineY4 - LineY3) / (LineX4 - LineX3);
		b2 = LineY4 - a2 * LineX4;
		*CenterX = LineX1;
		*CenterY = LineX1 * a2 + b2;
	}
	else
	{
		if (InRangeSpecial(LineX3, LineX4, 0.00001))
		{	// Vertical line
			a1 = (LineY2 - LineY1) / (LineX2 - LineX1);
			b1 = LineY2 - a1 * LineX2;
			*CenterX = LineX3;
			*CenterY = LineX3 * a1 + b1;
		}
		else
		{
			a1 = (LineY2 - LineY1) / (LineX2 - LineX1);
			a2 = (LineY4 - LineY3) / (LineX4 - LineX3);
			b1 = LineY2 - a1 * LineX2;
			b2 = LineY4 - a2 * LineX4;
			*CenterX = (b2 - b1) / (a1 - a2);
			*CenterY = *CenterX * a1 + b1;
		}
	}

	return 1;
	/*
	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	           (y2-y1)
	y = (x-x1)*------- + y1
	           (x2-x1)


	b = (y1*(x2-x1) - x1*(y2-y1)) / (x2-x1)


	2 lines crosses  (x1,y1) (x2,y2)   and  (x3,y3) (x4,y4)

	*/
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PointInPolygon(PolygonRecord * Polygon, double px, double py)
{
	double x1, y1, x2, y2, fx, fy, *P, px2, py2;
	int32 cnt, res, crosses, NrVertices;

	x2 = 0.0;
	y2 = 0.0;
	crosses = 0;
	px2 = px - 0.0000015;
	py2 = py + 2e9;
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


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PolygonInSearchArea(PolygonRecord * Polygon)
{
	PolygonRecord SearchPolygon;

	memset(&SearchPolygon, 0, sizeof(SearchPolygon));

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

	if (CheckPolygonCompleetlyInsidePolygon(Polygon, &SearchPolygon) == 1)
		return 1;

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectPolygonInSearchArea(ObjectPolygonRecord * ObjectPolygon)
{
	PolygonRecord SearchPolygon, NewPolygon;

	memset(&SearchPolygon, 0, sizeof(SearchPolygon));
	memset(&NewPolygon, 0, sizeof(NewPolygon));

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

	NewPolygon.NrVertices = ObjectPolygon->NrVertices;
	memmove(&NewPolygon.Points, ObjectPolygon->Points, NewPolygon.NrVertices * sizeof(PointRecord));
	NewPolygon.minx = ObjectPolygon->minx;
	NewPolygon.miny = ObjectPolygon->miny;
	NewPolygon.maxx = ObjectPolygon->maxx;
	NewPolygon.maxy = ObjectPolygon->maxy;

	if (CheckPolygonCompleetlyInsidePolygon(&NewPolygon, &SearchPolygon) == 1)
		return 1;

	return 0;
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
	py2 = py + 2e9;
	NrVertices = ObjectPolygon->NrVertices;
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

int32 RectangleInPolyLine(PolygonRecord * Polygon, double rxmin, double rymin, double rxmax, double rymax)
{
	double x1, y1, x2, y2, sx, sy, *px, xmin, ymin, xmax, ymax;
	int32 cnt, res, crosses, NrVertices;

	x2 = 0.0;
	y2 = 0.0;
	NrVertices = Polygon->NrVertices;
	crosses = 0;
	px = (double *) &(Polygon->Points);
	sx = *(px++);
	sy = *(px++);
	cnt = 0;

	while (cnt < NrVertices)
	{
		if (cnt > 0)
		{
			if (cnt < NrVertices - 1)
			{
				x1 = x2;
				y1 = y2;
				x2 = *(px++);
				y2 = *(px++);
			}
			else
			{
				x1 = x2;
				y1 = y2;
				x2 = sx;
				y2 = sy;
			}
		}
		else
		{
			x1 = sx;
			y1 = sy;
			x2 = *(px++);
			y2 = *(px++);
		}

		xmin = min(x1, x2);
		xmax = max(x1, x2);
		ymin = min(y1, y2);
		ymax = max(y1, y2);

		if ((xmin < rxmax) && (ymin < rymax) && (xmax > rxmin) && (ymax > rymin))
		{
			if ((res = LineCrosses(rxmin, rymax, rxmax, rymax, x1, y1, x2, y2)) != -1)
				crosses++;

			if ((res = LineCrosses(rxmin, rymin, rxmax, rymin, x1, y1, x2, y2)) != -1)
				crosses++;

			if ((res = LineCrosses(rxmin, rymin, rxmin, rymax, x1, y1, x2, y2)) != -1)
				crosses++;

			if ((res = LineCrosses(rxmax, rymin, rxmax, rymax, x1, y1, x2, y2)) != -1)
				crosses++;

			if (crosses > 0)
				return 0;
		}

		cnt++;
	}

	if ((rxmin < Polygon->minx) && (rymin < Polygon->miny) && (rxmax > Polygon->maxx) && (rymax > Polygon->maxy))
		return 2;
	else
		return 1;
}

// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// **************************************************************************************

void MakePolygonFromObject(ObjectRecord * Object, PolygonRecord * PolygonObject, double Clearance, double Pos,
                           int32 MaxPos, int32 mode)
/*

  mode :

  bit 0 = 0 : Usage clearance of maximum Object->Clearance and Clearance with a small addition
  bit 0 = 1 : Use Clearance from the parameters
  bit 1  : Search for OBJECT_SELECTED
  bit 2  : Add clearance for searching
  bit 4  : Do not add routing keepout

*/
{
	int32 count, res, cnt, cnt2, Step, CircleRoundings, NrSegments, Rotation, StartCnt, CircleRoundings2,
	      MemSizePolygonObject2, Mirror;
	ObjectRecord NewObject;
	AreaFillRecord *AreaFill;
	PolygonRecord *PolygonObject2;
	uint8 PolygonBuf2[16384], *AreaPos;
	GeomPolygonRecord *GeomPolygon;
	ObjectPolygonRecord *ObjectPolygon;
	PolygonRecord *DrawPolygon;
	double x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, dikte, dikte2, diktex, diktey, NewClearance, Angle,
	       StartAngle, AngleInc, AngleInc2, Thickness, Angle1, Angle2, Length1, Length2, Length, Width, Height, Width2,
	       Height2, TempValue, AngleMiddle, Angle4, Angle5, cx1, cy1, cx2, cy2, ExtraAngleFactor, Thickness2, sinx, sinx2,
	       CircleRoundingCompensation = 1.0;

	memset(&NewObject, 0, sizeof(NewObject));
	PolygonObject2 = (PolygonRecord *) & PolygonBuf2;
	GeomPolygon = NULL;
	CircleRoundings = 32;
//  CircleRoundings=8;
	Thickness = 0.0;
	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	PolygonObject->NrVertices = 0;
	MemSizePolygonObject2 = 0;

	if ((mode & 1) == 0)
		NewClearance = max(Clearance, Object->Clearance) + (Pos / MaxPos);
	else
		NewClearance = Clearance;

	if (PolygonPointAddValueX < 0.0045)
		PolygonPointAddValueX *= PolygonPointMultValue;
	else
		PolygonPointAddValueX /= PolygonPointMultValue;

	if (PolygonPointAddValueY < 0.0045)
		PolygonPointAddValueY *= PolygonPointMultValue;
	else
		PolygonPointAddValueY /= PolygonPointMultValue;

// **************************************************************************************
	switch (Object->ObjectType)
	{
	case PIN_SMD_ROUND:
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case DRILL_UNPLATED:
		Thickness = Object->x2;
		Thickness2 = Object->x2 + NewClearance * 2.0;

		if (mode & 2)
		{
			if (Thickness2 > CircleDiameterBig * 0.5)
			{
				if (Thickness2 > CircleDiameterBig)
				{
					CircleRoundings = 64;
					CircleRoundingCompensation = 1.002;
				}
				else
				{
					CircleRoundings = 32;
					CircleRoundingCompensation = 1.006;
				}
			}
			else
			{
				CircleRoundings = 16;
				CircleRoundingCompensation = 1.021;
			}
		}
		else
		{
			CircleRoundings = 16;

			if (Thickness2 > CircleDiameterBig * 0.5)
				CircleRoundings = 32;

			if (Thickness2 > CircleDiameterBig)
				CircleRoundings = 64;
		}

		break;

	case TRACE_HOR:
	case TRACE_VER:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
		Thickness2 = Object->y2 + NewClearance * 2.0;

		if (mode & 2)
		{
			if (Thickness2 > CircleDiameterBig * 0.5)
			{
				if (Thickness2 > CircleDiameterBig)
				{
					CircleRoundings = 64;
					CircleRoundingCompensation = 1.002;
				}
				else
				{
					CircleRoundings = 32;
					CircleRoundingCompensation = 1.006;
				}
			}
			else
			{
				CircleRoundings = 16;
				CircleRoundingCompensation = 1.021;
			}
		}
		else
		{
			CircleRoundings = 16;

			if (Thickness2 > CircleDiameterBig * 0.5)
				CircleRoundings = 32;

			if (Thickness2 > CircleDiameterBig)
				CircleRoundings = 64;
		}

		break;

	case OBJECT_LINE:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
		Thickness = Object->Thickness;
		Thickness2 = Object->Thickness + NewClearance * 2.0;

		if (mode & 2)
		{
			if (Thickness2 > CircleDiameterBig * 0.5)
			{
				if (Thickness2 > CircleDiameterBig)
				{
					CircleRoundings = 64;
					CircleRoundingCompensation = 1.002;
				}
				else
				{
					CircleRoundings = 32;
					CircleRoundingCompensation = 1.006;
				}
			}
			else
			{
				CircleRoundings = 16;
				CircleRoundingCompensation = 1.021;
			}
		}
		else
		{
			CircleRoundings = 16;

			if (Thickness2 > CircleDiameterBig * 0.5)
				CircleRoundings = 32;

			if (Thickness2 > CircleDiameterBig)
				CircleRoundings = 64;
		}

		break;

	case OBJECT_ARC:
	case TRACE_ARC:
	case PIN_ARC:
		Thickness = Object->Thickness;
		Thickness2 = Object->Thickness + NewClearance * 2.0;

		if (mode & 2)
		{
			if (Thickness2 > CircleDiameterBig * 0.5)
			{
				if (Thickness2 > CircleDiameterBig)
				{
					CircleRoundings = 64;
					CircleRoundingCompensation = 1.002;
				}
				else
				{
					CircleRoundings = 32;
					CircleRoundingCompensation = 1.006;
				}
			}
			else
			{
				CircleRoundings = 16;
				CircleRoundingCompensation = 1.021;
			}
		}
		else
		{
			CircleRoundings = 16;

			if (Thickness2 > CircleDiameterBig * 0.5)
				CircleRoundings = 32;

			if (Thickness2 > CircleDiameterBig)
				CircleRoundings = 64;
		}

		break;
	}

	Step = 128 / CircleRoundings;

// **************************************************************************************
	switch (Object->ObjectType)
	{
	case PIN_SMD_ROUND:
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case DRILL_UNPLATED:
	case OBJECT_CIRCLE:
		dikte = Thickness * 0.5 + NewClearance;

		if (mode & 2)
			dikte *= CircleRoundingCompensation;

		for (cnt = 0; cnt < CircleRoundings; cnt++)
		{
			x5 = CircleCos[cnt * Step] * dikte;
			y5 = CircleSin[cnt * Step] * dikte;
			PolygonObject->Points[cnt].x = x1 + x5 + PolygonPointAddValueX;
			PolygonObject->Points[cnt].y = y1 + y5 + PolygonPointAddValueY;
		}

		PolygonObject->NrVertices = CircleRoundings;
		PolygonObject->minx = x1 - dikte;
		PolygonObject->miny = y1 - dikte;
		PolygonObject->maxx = x1 + dikte;
		PolygonObject->maxy = y1 + dikte;
		break;

// **************************************************************************************
	case PIN_PUT_THROUGH_SQUARE:
		dikte2 = x2 / 2 + NewClearance;
		PolygonObject->Points[0].x = x1 + dikte2 + PolygonPointAddValueX;
		PolygonObject->Points[0].y = y1 + dikte2 + PolygonPointAddValueY;
		PolygonObject->Points[1].x = x1 - dikte2 + PolygonPointAddValueX;
		PolygonObject->Points[1].y = y1 + dikte2 + PolygonPointAddValueY;
		PolygonObject->Points[2].x = x1 - dikte2 + PolygonPointAddValueX;
		PolygonObject->Points[2].y = y1 - dikte2 + PolygonPointAddValueY;
		PolygonObject->Points[3].x = x1 + dikte2 + PolygonPointAddValueX;
		PolygonObject->Points[3].y = y1 - dikte2 + PolygonPointAddValueY;
		PolygonObject->NrVertices = 4;
		PolygonObject->minx = x1 - dikte2;
		PolygonObject->miny = y1 - dikte2;
		PolygonObject->maxx = x1 + dikte2;
		PolygonObject->maxy = y1 + dikte2;
		break;

// **************************************************************************************
	case PIN_SMD_RECT:
	case OBJECT_RECT:

		/*

		      diktex=x2/2+RandValue;
		      diktey=y2/2+RandValue;
		      if ((Thickness==0.0)
		         &&
		         (RandValue==0.0)) {
		        PolygonObject->Points[0].x=x1+diktex;
		        PolygonObject->Points[0].y=y1+diktey;
		        PolygonObject->Points[1].x=x1-diktex;
		        PolygonObject->Points[1].y=y1+diktey;
		        PolygonObject->Points[2].x=x1-diktex;
		        PolygonObject->Points[2].y=y1-diktey;
		        PolygonObject->Points[3].x=x1+diktex;
		        PolygonObject->Points[3].y=y1-diktey;
		        PolygonObject->NrVertices=4;
		        Thickness2=Thickness*0.5;
		      } else {
		        if (Thickness==0.0) {
		          Thickness=RandValue*2.0;
		          diktex=x2/2;
		          diktey=y2/2;
		          RandValue=0.0;
		        }
		        CircleRoundings=32;
		        Step=MaxNrCircleSegments/CircleRoundings;
		        cnt2=0;
		        SignX=1;
		        SignY=1;
		        x3=(x2-Thickness)*0.5+RandValue;
		        y3=(y2-Thickness)*0.5+RandValue;
		        Thickness2=Thickness*0.5;
		        for (cnt=0;cnt<CircleRoundings+1;cnt++) {
		          x5=CircleCos[cnt*Step]*Thickness2;
		          y5=CircleSin[cnt*Step]*Thickness2;
		          PolygonObject->Points[cnt2].x=x1+diktex*SignX+x5;
		          PolygonObject->Points[cnt2].y=y1+diktey*SignY+y5;
		          cnt2++;
		          if (((cnt % (CircleRoundings/Step)) == 0)
		             &&
		             (cnt>0)) {
		            switch (cnt / (CircleRoundings/Step)) {
		              case 1:
		                SignX=-1;
		                PolygonObject->Points[cnt2].x=x1+diktex*SignX+x5;
		                PolygonObject->Points[cnt2].y=y1+diktey*SignY+y5;
		                cnt2++;
		                break;
		              case 2:
		                SignY=-1;
		                PolygonObject->Points[cnt2].x=x1+diktex*SignX+x5;
		                PolygonObject->Points[cnt2].y=y1+diktey*SignY+y5;
		                cnt2++;
		                break;
		              case 3:
		                SignX=1;
		                PolygonObject->Points[cnt2].x=x1+diktex*SignX+x5;
		                PolygonObject->Points[cnt2].y=y1+diktey*SignY+y5;
		                cnt2++;
		                break;
		            }
		          }
		        }
		        PolygonObject->NrVertices=CircleRoundings+4;
		      }
		      PolygonObject->minx=x1-diktex-Thickness2;
		      PolygonObject->miny=y1-diktey-Thickness2;
		      PolygonObject->maxx=x1+diktex+Thickness2;
		      PolygonObject->maxy=y1+diktey+Thickness2;


		  */










		diktex = x2 / 2 + NewClearance;
		diktey = y2 / 2 + NewClearance;
		PolygonObject->Points[0].x = x1 + diktex + PolygonPointAddValueX;
		PolygonObject->Points[0].y = y1 + diktey + PolygonPointAddValueY;
		PolygonObject->Points[1].x = x1 - diktex + PolygonPointAddValueX;
		PolygonObject->Points[1].y = y1 + diktey + PolygonPointAddValueY;
		PolygonObject->Points[2].x = x1 - diktex + PolygonPointAddValueX;
		PolygonObject->Points[2].y = y1 - diktey + PolygonPointAddValueY;
		PolygonObject->Points[3].x = x1 + diktex + PolygonPointAddValueX;
		PolygonObject->Points[3].y = y1 - diktey + PolygonPointAddValueY;
		PolygonObject->minx = x1 - diktex;
		PolygonObject->miny = y1 - diktey;
		PolygonObject->maxx = x1 + diktex;
		PolygonObject->maxy = y1 + diktey;
		PolygonObject->NrVertices = 4;
		break;

// **************************************************************************************
	case TRACE_HOR:
	case PIN_LINE_HOR:
		dikte = y2 * 0.5 + NewClearance;

		if (mode & 2)
			dikte *= CircleRoundingCompensation;

		cnt2 = 0;

		for (cnt = CircleRoundings / 4; cnt < ((CircleRoundings * 5) / 4) + 1; cnt++)
		{
			x5 = CircleCos[cnt * Step] * dikte;
			y5 = CircleSin[cnt * Step] * dikte;

			if (cnt == ((CircleRoundings * 3) / 4))
			{
				PolygonObject->Points[cnt2].x = x1 + x5 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + y5 + PolygonPointAddValueY;
				cnt2++;
			}

			if (cnt >= ((CircleRoundings * 3) / 4))
				PolygonObject->Points[cnt2].x = x1 + x2 + PolygonPointAddValueX;
			else
				PolygonObject->Points[cnt2].x = x1 + PolygonPointAddValueX;

			PolygonObject->Points[cnt2].y = y1 + PolygonPointAddValueY;
			PolygonObject->Points[cnt2].x += x5;
			PolygonObject->Points[cnt2].y += y5;
			cnt2++;
		}

		PolygonObject->NrVertices = CircleRoundings + 2;
		PolygonObject->minx = x1 - dikte;
		PolygonObject->miny = y1 - dikte;
		PolygonObject->maxx = x1 + x2 + dikte;
		PolygonObject->maxy = y1 + dikte;
		break;

// **************************************************************************************
	case TRACE_VER:
	case PIN_LINE_VER:
		dikte = y2 * 0.5 + NewClearance;

		if (mode & 2)
			dikte *= CircleRoundingCompensation;

		cnt2 = 0;

		for (cnt = 0; cnt < CircleRoundings + 1; cnt++)
		{
			x5 = CircleCos[cnt * Step] * dikte;
			y5 = CircleSin[cnt * Step] * dikte;

			if (cnt == (CircleRoundings * 2) / 4)
			{
				PolygonObject->Points[cnt2].x = x1 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + x2 + PolygonPointAddValueY;
				PolygonObject->Points[cnt2].x += x5;
				PolygonObject->Points[cnt2].y += y5;
				cnt2++;
			}

			PolygonObject->Points[cnt2].x = x1 + PolygonPointAddValueX;

			if (cnt < ((CircleRoundings * 2) / 4))
				PolygonObject->Points[cnt2].y = y1 + x2 + PolygonPointAddValueY;
			else
				PolygonObject->Points[cnt2].y = y1 + PolygonPointAddValueY;

			PolygonObject->Points[cnt2].x += x5;
			PolygonObject->Points[cnt2].y += y5;
			cnt2++;
		}

		PolygonObject->NrVertices = CircleRoundings + 2;
		PolygonObject->minx = x1 - dikte;
		PolygonObject->miny = y1 - dikte;
		PolygonObject->maxx = x1 + dikte;
		PolygonObject->maxy = y1 + x2 + dikte;
		break;

// **************************************************************************************
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:
		dikte = y2 * 0.5 + NewClearance;

		if (mode & 2)
			dikte *= CircleRoundingCompensation;

		cnt2 = 0;

		for (cnt = CircleRoundings / 8; cnt < ((CircleRoundings * 9) / 8) + 1; cnt++)
		{
			x5 = CircleCos[cnt * Step] * dikte;
			y5 = CircleSin[cnt * Step] * dikte;

			if (cnt == ((CircleRoundings * 5) / 8))
			{
				PolygonObject->Points[cnt2].x = x1 + x5 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + y5 + PolygonPointAddValueY;
				cnt2++;
			}

			if (cnt >= ((CircleRoundings * 5) / 8))
			{
				PolygonObject->Points[cnt2].x = x1 + x2 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 - x2 + PolygonPointAddValueY;
			}
			else
			{
				PolygonObject->Points[cnt2].x = x1 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + PolygonPointAddValueY;
			}

			PolygonObject->Points[cnt2].x += x5;
			PolygonObject->Points[cnt2].y += y5;
			cnt2++;
		}

		PolygonObject->NrVertices = CircleRoundings + 2;
		PolygonObject->minx = x1 - dikte;
		PolygonObject->miny = y1 - x2 - dikte;
		PolygonObject->maxx = x1 + x2 + dikte;
		PolygonObject->maxy = y1 + dikte;
		break;

// **************************************************************************************
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:
		dikte = y2 * 0.5 + NewClearance;

		if (mode & 2)
			dikte *= CircleRoundingCompensation;

		cnt2 = 0;

		for (cnt = (3 * CircleRoundings) / 8; cnt < ((CircleRoundings * 11) / 8) + 1; cnt++)
		{
			x5 = CircleCos[cnt * Step] * dikte;
			y5 = CircleSin[cnt * Step] * dikte;

			if (cnt == ((CircleRoundings * 7) / 8))
			{
				PolygonObject->Points[cnt2].x = x1 + x5 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + y5 + PolygonPointAddValueY;
				cnt2++;
			}

			if (cnt >= ((CircleRoundings * 7) / 8))
			{
				PolygonObject->Points[cnt2].x = x1 + x2 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + x2 + PolygonPointAddValueY;
			}
			else
			{
				PolygonObject->Points[cnt2].x = x1 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + PolygonPointAddValueY;
			}

			PolygonObject->Points[cnt2].x += x5;
			PolygonObject->Points[cnt2].y += y5;
			cnt2++;
		}

		PolygonObject->NrVertices = CircleRoundings + 2;
		PolygonObject->minx = x1 - dikte;
		PolygonObject->miny = y1 - dikte;
		PolygonObject->maxx = x1 + x2 + dikte;
		PolygonObject->maxy = y1 + x2 + dikte;
		break;

// **************************************************************************************
	case OBJECT_LINE:
	case TRACE_ALL_ANGLE:
	case PIN_LINE_ALL_ANGLE:
//      CircleRoundings++;
		dikte = Thickness * 0.5 + NewClearance;

		if (mode & 2)
			dikte *= CircleRoundingCompensation;

		x5 = x2 - x1;
		y5 = y2 - y1;

		if (NotInRange(x5, 0.0))
		{
			StartAngle = atan(y5 / x5);

			if (StartAngle == 0.0)
			{
				if (x5 < 0.0)
					StartAngle += ANGLE_180;
			}
			else
			{
				if (StartAngle < 0.0)
					StartAngle += ANGLE_180;
			}

			if (y2 < y1)
				StartAngle += ANGLE_180;
		}
		else
		{
			if (y2 > y1)
				StartAngle = ANGLE_90;
			else
				StartAngle = ANGLE_270;
		}

		StartAngle += ANGLE_90;
		AngleInc = ANGLE_360 / CircleRoundings;
		Angle = StartAngle;
		cnt2 = 0;

		for (cnt = 0; cnt < CircleRoundings + 1; cnt++)
		{
			x5 = cos(Angle) * dikte;
			y5 = sin(Angle) * dikte;

			if (cnt == (CircleRoundings / 2))
			{
				PolygonObject->Points[cnt2].x = x1 + x5 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + y5 + PolygonPointAddValueY;
				cnt2++;
			}

			if (cnt >= (CircleRoundings / 2))
			{
				PolygonObject->Points[cnt2].x = x2 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y2 + PolygonPointAddValueY;
			}
			else
			{
				PolygonObject->Points[cnt2].x = x1 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y1 + PolygonPointAddValueY;
			}

			PolygonObject->Points[cnt2].x += x5;
			PolygonObject->Points[cnt2].y += y5;
			cnt2++;
			Angle += AngleInc;

			if (Angle > ANGLE_360)
				Angle -= ANGLE_360;
		}

		PolygonObject->NrVertices = CircleRoundings + 2;
		PolygonObject->minx = min(x1, x2) - dikte;
		PolygonObject->miny = min(y1, y2) - dikte;
		PolygonObject->maxx = max(x1, x2) + dikte;
		PolygonObject->maxy = max(y1, y2) + dikte;
		break;

// **************************************************************************************
	case OBJECT_ARC:
	case TRACE_ARC:
	case PIN_ARC:
		NrSegments = 64;
		count = 0;

		if (y2 == 0.0)
			y2 = x2;

		if ((x2 > (100 * 2540)) || (y2 > (100 * 2540)))
			NrSegments = 128;

		if ((x2 > (200 * 2540)) || (y2 > (200 * 2540)))
			NrSegments = 256;

#ifdef _DEBUG

		if (x2 > 10e5)
			ok = 1;

#endif
		x3 = Object->x3;
		y3 = Object->y3;
		x4 = Object->x4;
		y4 = Object->y4;

		if ((InRangeSpecial(x3, x4, 100.0)) && (InRangeSpecial(y3, y4, 100.0)))
		{
			count = NrSegments;
			Angle1 = PI / 2;
			Angle2 = 0.0;
			AngleInc = ANGLE_360 / (double) count;
			AngleInc2 = 0.0;

			Width2 = Thickness * 0.5 + NewClearance;
			Height2 = Thickness * 0.5 + NewClearance;

			if (mode & 2)
			{
				Width2 *= CircleRoundingCompensation;
				Height2 *= CircleRoundingCompensation;
			}

			Width = x2 * 0.5 + Width2;
			Height = y2 * 0.5 + Height2;

			Angle = Angle1;
			cnt2 = 0;

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
			Width = (x2 - Thickness) * 0.5 - NewClearance;
			Height = (y2 - Thickness) * 0.5 - NewClearance;

			if ((Width < 10) || (Height < 10))
			{
				// Circle pad
				PolygonObject->NrVertices = cnt2 - 1;
				PolygonObject->PolygonType = 0;
				SetMinMaxPolygon(PolygonObject, 0);
				return;
			}

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
			PolygonObject->PolygonType = 0;
			SetMinMaxPolygon(PolygonObject, 0);
			return;
		}

// **************************************************************************************
		Width = (x2 + Thickness) * 0.5 + NewClearance;
		Height = (y2 + Thickness) * 0.5 + NewClearance;
//      Height=(x2+Thickness)*0.5+RandValue;
		Width2 = Thickness * 0.5 + NewClearance;
		Height2 = Thickness * 0.5 + NewClearance;

		cnt2 = 0;

		ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &Length1);
		ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &Length2);

		if (Angle2 < Angle1)
			Angle2 += PI * 2;

		AngleMiddle = (Angle1 + Angle2) * 0.5;
		count = (int32) ((Angle2 - Angle1) / (PI * 2 / (double) NrSegments));
		count = max(4, count);
		AngleInc = (Angle2 - Angle1) / (double) count;

		if (Thickness > 2e5)
			CircleRoundings = 64;
		else
			CircleRoundings = 32;

		if (min(x2, y2) > Thickness * 1.1)
		{
			AngleInc2 = (2 * PI) / (double) CircleRoundings;
			CircleRoundings2 = CircleRoundings / 2;
			StartCnt = 0;

			if (InRange(Width, Height))
			{
				Angle4 = Angle1;
				Angle5 = Angle2;
				cx1 = x1 + (x2 * 0.5) * cos(Angle1);
				cy1 = y1 + (x2 * 0.5) * sin(Angle1);
				cx2 = x1 + (x2 * 0.5) * cos(Angle2);
				cy2 = y1 + (x2 * 0.5) * sin(Angle2);
			}
			else
			{
				sinx = sin(Angle1);
				sinx2 = sinx * sinx;
				Length = (x2 * 0.5) * (y2 * 0.5) * sqrt(1 / (SQR((y2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
				cx1 = x1 + Length * cos(Angle1);
				cy1 = y1 + Length * sin(Angle1);

				sinx = sin(Angle1 + 0.01);
				sinx2 = sinx * sinx;
				Length = (x2 * 0.5) * (y2 * 0.5) * sqrt(1 / (SQR((y2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
				x6 = x1 + Length * cos(Angle1 + 0.01);
				y6 = y1 + Length * sin(Angle1 + 0.01);

				ConvNormalCoorToPolar(cx1, cy1, x6, y6, &Angle4, &Length1);
				Angle4 -= ANGLE_90;

				sinx = sin(Angle2);
				sinx2 = sinx * sinx;
				Length = (x2 * 0.5) * (y2 * 0.5) * sqrt(1 / (SQR((y2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
				cx2 = x1 + Length * cos(Angle2);
				cy2 = y1 + Length * sin(Angle2);

				sinx = sin(Angle2 + 0.01);
				sinx2 = sinx * sinx;
				Length = (x2 * 0.5) * (y2 * 0.5) * sqrt(1 / (SQR((y2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
				x6 = x1 + Length * cos(Angle2 + 0.01);
				y6 = y1 + Length * sin(Angle2 + 0.01);
				ConvNormalCoorToPolar(cx2, cy2, x6, y6, &Angle5, &Length1);
				Angle5 -= ANGLE_90;
				
				ExtraAngleFactor = Width / Height;

				if (ExtraAngleFactor < 1.0)
					ExtraAngleFactor = 1 / ExtraAngleFactor;

				if (NrSegments == 64)
					StartCnt = (int32) (ExtraAngleFactor);
				else
				{
					if (NrSegments == 128)
						StartCnt = (int32) (ExtraAngleFactor);
					else
						StartCnt = (int32) (ExtraAngleFactor + 1.0);
				}

				if (StartCnt < 1)
					StartCnt = 1;
			}

			ok = 1;


			Angle = Angle1;

			for (cnt = 0; cnt < count; cnt++)
			{
//        for (cnt=0;cnt<count+StartCnt;cnt++) {
				if (cnt >= StartCnt)
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
				}

				Angle += AngleInc;

				if (Angle > (2 * PI))
					Angle -= 2 * PI;
			}

			Angle = Angle5;

//        Height=(x2-Thickness)*0.5-RandValue;

			for (cnt = 0; cnt <= CircleRoundings2; cnt++)
			{
				PolygonObject->Points[cnt2].x = cx2 + cos(Angle) * Width2;
				PolygonObject->Points[cnt2].y = cy2 + sin(Angle) * Height2;
				cnt2++;
				Angle += AngleInc2;

				if (Angle > (2 * PI))
					Angle -= 2 * PI;
			}

			Angle = Angle2;
			Width = (x2 - Thickness) * 0.5 - NewClearance;
			Height = (y2 - Thickness) * 0.5 - NewClearance;

			for (cnt = 0; cnt < count; cnt++)
			{
//        for (cnt=0;cnt<count+StartCnt;cnt++) {
				if (cnt >= StartCnt)
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
				}

				Angle -= AngleInc;

				if (Angle < 0.0)
					Angle += 2 * PI;
			}

			Angle = Angle4 + PI;

			for (cnt = 0; cnt <= CircleRoundings2; cnt++)
			{
				PolygonObject->Points[cnt2].x = cx1 + cos(Angle) * Width2;
				PolygonObject->Points[cnt2].y = cy1 + sin(Angle) * Height2;
				cnt2++;
				Angle += AngleInc2;

				if (Angle > (2 * PI))
					Angle -= 2 * PI;
			}






























		}
		else
		{
			/*
			        x5a=x1+cos(AngleMiddle+PI)*1e9;
			        y5a=y1+sin(AngleMiddle+PI)*1e9;
			        x4a=x1+cos(Angle2)*x2*0.5;
			        y4a=y1+sin(Angle2)*x2*0.5;
			        res=GetCrossPointsCircleCrossesLine(x1,y1,x5a,y5a,
			                                            x4a,y4a,Thickness,
			                                            &x5,&y5,&x6,&y6);
			        if (res==1) {
			          ConvNormalCoorToPolar(x4a,y4a,x5,y5,&Angle3,&Length3);
			          if (Angle3<Angle2) {
			            Angle3+=PI*2.0;
			          }
			          CircleRoundings2=(int32)((Angle3-Angle2)/(PI*2/(double)CircleRoundings));
			          AngleInc2=(Angle3-Angle2)/(double)CircleRoundings2;
			        } else {
			          AngleInc2=(2*PI)/(double)CircleRoundings;
			          CircleRoundings2=CircleRoundings/2;
			        }

			        Angle=Angle1;
			        for (cnt=0;cnt<count;cnt++) {
			          if (InRange(Width,Height)) {
			            x5=x1+Width*cos(Angle);
			            y5=y1+Height*sin(Angle);
			          } else {
			            sinx=sin(Angle);
			            sinx2=sinx*sinx;
			            Length=Width*Height*sqrt(1/(SQR(Height)*(1-sinx2)+SQR(Width)*sinx2));
			            x5=x1+Length*cos(Angle);
			            y5=y1+Length*sin(Angle);
			          }
			          PolygonObject->Points[cnt2].x=x5;
			          PolygonObject->Points[cnt2].y=y5;
			          cnt2++;
			          Angle+=AngleInc;
			          if (Angle>(2*PI)) Angle-=2*PI;
			        }
			        Angle=Angle2;
			        if (InRange(Width,Height)) {
			          x6=x1+(x2*0.5)*cos(Angle);
			          y6=y1+(x2*0.5)*sin(Angle);
			  //        y6=y1+(y2*0.5)*sin(Angle);
			        } else {
			          sinx=sin(Angle);
			          sinx2=sinx*sinx;
			          Length=(x2*0.5)*(x2*0.5)*sqrt(1/(SQR((x2*0.5))*(1-sinx2)+SQR((x2*0.5))*sinx2));
			          x6=x1+Length*cos(Angle);
			          Length=(x2*0.5)*(y2*0.5)*sqrt(1/(SQR((y2*0.5))*(1-sinx2)+SQR((x2*0.5))*sinx2));
			          y6=y1+Length*sin(Angle);
			        }

			        for (cnt=0;cnt<CircleRoundings2;cnt++) {
			          PolygonObject->Points[cnt2].x=x6+cos(Angle)*Width2;
			          PolygonObject->Points[cnt2].y=y6+sin(Angle)*Height2;
			          cnt2++;
			          Angle+=AngleInc2;
			          if (Angle>(2*PI)) Angle-=2*PI;
			        }
			        Angle=Angle1;
			        if (InRange(Width,Height)) {
			          x6=x1+(x2*0.5)*cos(Angle);
			          y6=y1+(x2*0.5)*sin(Angle);
			  //        y6=y1+(y2*0.5)*sin(Angle);
			        } else {
			          sinx=sin(Angle);
			          sinx2=sinx*sinx;
			          Length=(x2*0.5)*(x2*0.5)*sqrt(1/(SQR((x2*0.5))*(1-sinx2)+SQR((x2*0.5))*sinx2));
			          x6=x1+Length*cos(Angle);
			          Length=(x2*0.5)*(y2*0.5)*sqrt(1/(SQR((y2*0.5))*(1-sinx2)+SQR((x2*0.5))*sinx2));
			          y6=y1+Length*sin(Angle);
			        }
			//        Angle=(AngleMiddle+PI)*2.0-Angle;
			        Angle=Angle1-(double)CircleRoundings2*AngleInc2;
			        for (cnt=0;cnt<CircleRoundings2;cnt++) {
			          PolygonObject->Points[cnt2].x=x6+cos(Angle)*Width2;
			          PolygonObject->Points[cnt2].y=y6+sin(Angle)*Height2;
			          cnt2++;
			          Angle+=AngleInc2;
			          if (Angle>(2*PI)) Angle-=2*PI;
			        }
			*/
		}

		PolygonObject->NrVertices = cnt2;
		PolygonObject->PolygonType = 0;
		SetMinMaxPolygon(PolygonObject, 0);




#if 0
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
		Angle2 = 0.0;

		if ((InRange(x3, x4)) && (InRange(y3, y4)))
		{
			count = NrSegments;
			Angle1 = ANGLE_90;
			AngleInc = ANGLE_360 / count;
			AngleInc2 = 0.0;
		}
		else
		{
			ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &Length1);
			ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &Length2);

			if (Angle2 < Angle1)
				Angle2 += ANGLE_360;

			count = (int32) ((Angle2 - Angle1) / (ANGLE_360 / NrSegments));
			count = max(1, count);
			AngleInc = (Angle2 - Angle1) / count;
			AngleInc2 = ANGLE_360 / CircleRoundings;
		}

		Width2 = Thickness * 0.5 + NewClearance;
		Height2 = Thickness * 0.5 + NewClearance;

		if (mode & 2)
		{
			Width2 *= CircleRoundingCompensation;
			Height2 *= CircleRoundingCompensation;
		}

		Width = x2 * 0.5 + Width2;
		Height = y2 * 0.5 + Height2;

		Angle = Angle1;
		cnt2 = 0;

		if ((InRange(x3, x4)) && (InRange(y3, y4)))
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

				PolygonObject->Points[cnt2].x = x5 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y5 + PolygonPointAddValueY;
				cnt2++;
				Angle += AngleInc;

				if (Angle > ANGLE_360)
					Angle -= ANGLE_360;
			}

			Angle -= AngleInc;
			Width = (x2 - Thickness) * 0.5 - NewClearance;
			Height = (y2 - Thickness) * 0.5 - NewClearance;

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

				PolygonObject->Points[cnt2].x = x5 + PolygonPointAddValueX;
				PolygonObject->Points[cnt2].y = y5 + PolygonPointAddValueY;
				cnt2++;
				Angle -= AngleInc;

				if (Angle < 0.0)
					Angle += ANGLE_360;
			}

			PolygonObject->Points[0].x -= 1;
			PolygonObject->Points[cnt2 - 1].x -= 1;
			PolygonObject->NrVertices = cnt2;
			SetMinMaxPolygon(PolygonObject, 0);
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

			PolygonObject->Points[cnt2].x = x5 + PolygonPointAddValueX;
			PolygonObject->Points[cnt2].y = y5 + PolygonPointAddValueY;
			cnt2++;
			Angle += AngleInc;

			if (Angle > ANGLE_360)
				Angle -= ANGLE_360;
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
//        Length=(x2*0.5)*(y2*0.5)*sqrt(1/(SQR((y2*0.5))*(1-sinx2)+SQR((x2*0.5))*sinx2));
			x6 = x1 + Length * cos(Angle);
			y6 = y1 + Length * sin(Angle);
		}

		Angle = Angle2;

		for (cnt = 0; cnt < CircleRoundings / 2; cnt++)
		{
			PolygonObject->Points[cnt2].x = x6 + cos(Angle) * Width2 + PolygonPointAddValueX;
			PolygonObject->Points[cnt2].y = y6 + sin(Angle) * Height2 + PolygonPointAddValueY;
			cnt2++;
			Angle += AngleInc2;

			if (Angle > ANGLE_360)
				Angle -= ANGLE_360;
		}

		Width = (x2 - Thickness) * 0.5 - NewClearance;
		Height = (x2 - Thickness) * 0.5 - NewClearance;
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

			PolygonObject->Points[cnt2].x = x5 + PolygonPointAddValueX;
			PolygonObject->Points[cnt2].y = y5 + PolygonPointAddValueY;
			cnt2++;
			Angle -= AngleInc;

			if (Angle < 0.0)
				Angle += ANGLE_360;
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

		Angle = Angle1 + ANGLE_180;

		for (cnt = 0; cnt < CircleRoundings / 2; cnt++)
		{
			PolygonObject->Points[cnt2].x = x6 + cos(Angle) * Width2 + PolygonPointAddValueX;
			PolygonObject->Points[cnt2].y = y6 + sin(Angle) * Height2 + PolygonPointAddValueY;
			cnt2++;
			Angle += AngleInc2;

			if (Angle > ANGLE_360)
				Angle -= ANGLE_360;
		}

		PolygonObject->NrVertices = cnt2;
		SetMinMaxPolygon(PolygonObject, 0);
#endif
		break;

// **************************************************************************************
	case PIN_SMD_POLYGON:
	case OBJECT_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		Rotation = GetRotationFromFloat(Object->RotationAngle);
		Mirror = Object->Mirror;

		if (Object->ObjectType2 == 0)
		{
			if (Object->Address != 0)
			{
				GeomPolygon = (GeomPolygonRecord *) Object->Address;
				count = GeomPolygon->NrVertices;

				if ((GeomPolygon->NrSubPolygons > 0) && (GeomPolygon->NrVerticesMainPolygon > 0))
					count = GeomPolygon->NrVerticesMainPolygon;

				if (count > 400)
				{
					if (MemSizePolygonObject2 == 0)
					{
						MemSizePolygonObject2 = count * sizeof(PointRecord) + sizeof(PolygonInitRecord);
						PolygonObject2 =
						    (PolygonRecord *) malloc(count * sizeof(PointRecord) + sizeof(PolygonInitRecord));
					}
					else
					{
						if ((uint32) MemSizePolygonObject2 < count * sizeof(PointRecord) + sizeof(PolygonInitRecord))
						{
							MemSizePolygonObject2 = count * sizeof(PointRecord) + sizeof(PolygonInitRecord);
							PolygonObject2 = (PolygonRecord *) realloc(PolygonObject2, MemSizePolygonObject2);
						}
					}
				}

				PolygonObject->NrVertices = count;
				PolygonObject2->NrVertices = count;

				for (cnt = 0; cnt < count; cnt++)
				{
					x5 = GeomPolygon->Points[cnt].x;
					y5 = GeomPolygon->Points[cnt].y;

					if (Mirror == 1)
						x5 = -x5;

					switch (Rotation)
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
						RotatePointFromOtherPoint2(&x5, &y5, 0.0, 0.0, Object->RotationAngle);
						break;
					}

					if (NewClearance != 0.0)
					{
						PolygonObject2->Points[cnt].x = x5 + x1 + PolygonPointAddValueX;
						PolygonObject2->Points[cnt].y = y5 + y1 + PolygonPointAddValueY;
					}
					else
					{
						PolygonObject->Points[cnt].x = x5 + x1 + PolygonPointAddValueX;
						PolygonObject->Points[cnt].y = y5 + y1 + PolygonPointAddValueY;
					}
				}
			}
			else
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[Object->TraceNr]]);
				count = ObjectPolygon->NrVertices;

				if (count > 400)
				{
					if (MemSizePolygonObject2 == 0)
					{
						MemSizePolygonObject2 = count * sizeof(PointRecord) + sizeof(PolygonInitRecord);
						PolygonObject2 =
						    (PolygonRecord *) malloc(count * sizeof(PointRecord) + sizeof(PolygonInitRecord));
					}
					else
					{
						if ((uint32) MemSizePolygonObject2 < count * sizeof(PointRecord) + sizeof(PolygonInitRecord))
						{
							MemSizePolygonObject2 = count * sizeof(PointRecord) + sizeof(PolygonInitRecord);
							PolygonObject2 = (PolygonRecord *) realloc(PolygonObject2, MemSizePolygonObject2);
						}
					}
				}

				if (NewClearance != 0.0)
				{
					memset(PolygonObject2, 0, sizeof(PolygonInitRecord));
					PolygonObject2->NrVertices = count;
					memmove(PolygonObject2->Points, ObjectPolygon->Points, count * sizeof(PointRecord));
					PolygonObject2->minx = ObjectPolygon->minx;
					PolygonObject2->miny = ObjectPolygon->miny;
					PolygonObject2->maxx = ObjectPolygon->maxx;
					PolygonObject2->maxy = ObjectPolygon->maxy;
				}
				else
				{
					memset(PolygonObject, 0, sizeof(PolygonInitRecord));
					PolygonObject->NrVertices = count;
					memmove(PolygonObject->Points, ObjectPolygon->Points, count * sizeof(PointRecord));
					PolygonObject->minx = ObjectPolygon->minx;
					PolygonObject->miny = ObjectPolygon->miny;
					PolygonObject->maxx = ObjectPolygon->maxx;
					PolygonObject->maxy = ObjectPolygon->maxy;
				}
			}
		}
		else
		{
			NewObject.x1 = 0.0;
			NewObject.y1 = 0.0;
			NewObject.x2 = Object->x2;
			NewObject.y2 = Object->y2;

			if (Object->ObjectType2 == PIN_PUT_THROUGH_SQUARE)
				NewObject.y2 = Object->x2;

			NewObject.ObjectType = Object->ObjectType2;
#ifdef _DEBUG

			if (NewObject.ObjectType == PIN_SMD_RECT)
				ok = 1;

#endif
			MakePolygonFromObject(&NewObject, SpecialGeomPolygon, 0.0, 0.0, 1, 1);

			if (Mirror == 1)
				MirrorPolygon(SpecialGeomPolygon, 0);

			RotatePolygon(SpecialGeomPolygon, 0.0, 0.0, Object->RotationAngle, 0);
			MovePolygon(SpecialGeomPolygon, Object->x1, Object->y1, 0);

			if (NewClearance != 0.0)
				CopyPolygonToPolygon(SpecialGeomPolygon, PolygonObject2);
			else
				CopyPolygonToPolygon(SpecialGeomPolygon, PolygonObject);
		}

		if (NewClearance != 0.0)
		{
			res = MakeBiggerSmallerPolygon2(PolygonObject2, PolygonObject, NewClearance * 2.0, 0);
//        CopyPolygonToPolygon(PolygonObject2,PolygonObject);
		}

		if (MemSizePolygonObject2)
			free(PolygonObject2);

		SetMinMaxPolygon(PolygonObject, 0);
		break;

// **************************************************************************************
	case AREAFILL2:
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object->TraceNr]]);
		AreaPos = (uint8 *) AreaFill;
		DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
		PolygonObject->PolygonType = 0;
		PolygonObject->Clearance = 0.0;
		count = AreaFill->NrVerticesStartPolygon;

		if (count > 400)
		{
			if (MemSizePolygonObject2 == 0)
			{
				MemSizePolygonObject2 = count * sizeof(PointRecord) + sizeof(PolygonInitRecord);
				PolygonObject2 = (PolygonRecord *) malloc(count * sizeof(PointRecord) + sizeof(PolygonInitRecord));
			}
			else
			{
				if ((uint32) MemSizePolygonObject2 < count * sizeof(PointRecord) + sizeof(PolygonInitRecord))
				{
					MemSizePolygonObject2 = count * sizeof(PointRecord) + sizeof(PolygonInitRecord);
					PolygonObject2 = (PolygonRecord *) realloc(PolygonObject2, MemSizePolygonObject2);
				}
			}
		}

		PolygonObject2->NrVertices = count;

		for (cnt = 0; cnt < count; cnt++)
		{
			PolygonObject2->Points[cnt].x = AreaFill->StartPolygon[cnt].x;
			PolygonObject2->Points[cnt].y = AreaFill->StartPolygon[cnt].y;
		}

		res = MakeBiggerSmallerPolygon2(PolygonObject2, PolygonObject, NewClearance * 2.0, 0);

		if (MemSizePolygonObject2)
			free(PolygonObject2);

		SetMinMaxPolygon(PolygonObject, 0);
#ifdef _DEBUG

		if (DrawPolygon->NrVertices == 35)
			ok = 1;

#endif
		break;

// **************************************************************************************
	default:
		PolygonObject->NrVertices = 0;
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MemSizePolygon(PolygonRecord * Polygon)
{
	int32 count;
	count = sizeof(PolygonInitRecord);
	count += Polygon->NrVertices * sizeof(PointRecord);
	return count;
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

void CopyPolygonToPolygon(PolygonRecord * SrcPolygon, PolygonRecord * DestPolygon)
{
#ifdef _DEBUG
	int32 res, ok;
	res = MemSizePolygon(SrcPolygon);

	if (res > 64000)
		res = 0;

	if ((SrcPolygon->NrVertices < 3) || (SrcPolygon->NrVertices > 100000))
		ok = 1;

#endif

	memmove(DestPolygon, SrcPolygon, MemSizePolygon(SrcPolygon));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteVertices(int32 Start, int32 Count, PointsArray * Points, int32 NrVertices)
{
	int32 pos;

	if (Start + Count > NrVertices)
	{
		pos = Start + Count - NrVertices;
		memmove(&(*Points)[0], &(*Points)[pos], (NrVertices - Count) * sizeof(PointRecord));
	}
	else
	{
		if (Start + Count < NrVertices)
			memmove(&(*Points)[Start], &(*Points)[Start + Count], (NrVertices - Count) * sizeof(PointRecord));
	}

}

/*
void InsertVertices(int32 ResultStart,int32 InsertStart,int32 Count,
                    PointsArray *ResultPoints,PointsArray *InsertPoints,
                    int32 NrVerticesResult,int32 NrVerticesInsert)

{
  int32 Count1;

  if (ResultStart<NrVerticesResult-1) {
    memmove(&(*ResultPoints)[ResultStart+Count],&(*ResultPoints)[ResultStart],
            (NrVerticesResult-ResultStart-1)*sizeof(PointRecord));
  }
  if (InsertStart+Count>NrVerticesInsert) {
    Count1=NrVerticesInsert-InsertStart;
    memmove(&(*ResultPoints)[ResultStart],&(*InsertPoints)[InsertStart],
            Count1*sizeof(PointRecord));
    memmove(&(*ResultPoints)[ResultStart+Count1],&(*InsertPoints)[0],
            (Count-Count1)*sizeof(PointRecord));
  } else
    memmove(&(*ResultPoints)[ResultStart],&(*InsertPoints)[InsertStart],Count*sizeof(PointRecord));
}
*/

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygonInsidePolygon(PolygonRecord * PolygonObject, PolygonRecord * BigPolygon)
{
	int32 res;
	double CheckX, CheckY;

	CheckX = PolygonObject->Points[0].x;
	CheckY = PolygonObject->Points[0].y;
	res = PointInPolygon(BigPolygon, CheckX, CheckY);

	if ((res & 1) == 0)
		return 0;

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SetMinMaxPolygon(PolygonRecord * PolygonObject, int32 mode)
{
	int32 count, cnt;
	double x1, y1, minx, maxx, miny, maxy;

	count = PolygonObject->NrVertices;

	minx = 1e9;
	miny = 1e9;
	maxx = -1e9;
	maxy = -1e9;

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

int32 SetMinMaxObjectPolygon(ObjectPolygonRecord * ObjectPolygon, int32 mode)
{
	int32 count, cnt;
	double x1, y1, minx, maxx, miny, maxy;

	count = ObjectPolygon->NrVertices;

	minx = 1e9;
	miny = 1e9;
	maxx = -1e9;
	maxy = -1e9;

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

	return 1;
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

int32 CheckPolygonOverlapAreaFill(PolygonRecord * PolygonObject, AreaFillRecord * AreaFill)
{
	int32 count, cnt2, res;

	PolygonRecord *DrawPolygon, *FirstPolygon;
	uint8 *AreaPos, *PolygonPos;


	AreaPos = (uint8 *) AreaFill;
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	FirstPolygon = DrawPolygon;
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons - 1; cnt2++)
	{
		count = DrawPolygon->NrVertices;
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
		res = CheckPolygonCompleetlyInsidePolygon(PolygonObject, DrawPolygon);

		if (res == -1)
		{
			return 1;			// Polygon lines crosses
		}

		if (res == 1)
		{
			return 0;			// Polygon completly inside areafill deletion polygon
		}
	}

	res = CheckPolygonCompleetlyOutsidePolygon(PolygonObject, FirstPolygon);

	if (res == -1)
	{
		return 1;				// Polygon lines crosses
	}

	if (res == -2)
	{
		return 1;				// Polygon not completly outside areafill surround polygon
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckDeletionsAreaFill(int32 mode)
{
#ifdef _DEBUG
	int32 cnt2, cnt3, fp;
	uint8 *PolygonPos, *PolygonPos2, *CheckPolygonPos, PolygonInclude[4000];
	AreaFillRecord *AreaFill;
	PolygonRecord *Polygon, *CheckPolygon;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	sprintf(str, "%s\\pcb\\areafill2.txt", DesignPath);

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return 0;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[0]]);

	PolygonPos = (uint8 *) AreaFill;
	PolygonPos += sizeof(AreaFillRecord);
	PolygonPos2 = PolygonPos;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		Polygon = (PolygonRecord *) PolygonPos;
		PolygonPos += MemSizePolygon(Polygon);
		sprintf(str2, "Polygon %3i  %4i vertices  mempos %5i   %8.3f,%8.3f,%8.3f,%8.3f", cnt2, Polygon->NrVertices,
		        PolygonPos - PolygonPos2, Polygon->minx / 100000, Polygon->miny / 100000, Polygon->maxx / 100000,
		        Polygon->maxy / 100000);
		WriteLn(fp, str2);
	}

	PolygonPos = (uint8 *) AreaFill;
	PolygonPos += sizeof(AreaFillRecord);
	Polygon = (PolygonRecord *) PolygonPos;
	PolygonPos += MemSizePolygon(Polygon);

	for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		Polygon = (PolygonRecord *) PolygonPos;
		CheckPolygonPos = (uint8 *) AreaFill;
		CheckPolygonPos += sizeof(AreaFillRecord);
		CheckPolygon = (PolygonRecord *) CheckPolygonPos;
		CheckPolygonPos += MemSizePolygon(CheckPolygon);

		for (cnt3 = 1; cnt3 < AreaFill->NrPolygons; cnt3++)
		{
			CheckPolygon = (PolygonRecord *) CheckPolygonPos;

			if (cnt3 != cnt2)
			{
				if (CheckPolygonCompleetlyInsidePolygon(CheckPolygon, Polygon) == 1)
					PolygonInclude[cnt3] = 0;
			}

			CheckPolygonPos += MemSizePolygon(CheckPolygon);
		}

		PolygonPos += MemSizePolygon(Polygon);
	}

	FileClose(fp);
#endif
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygonInsideAreaFill(PolygonRecord * PolygonObject, AreaFillRecord * AreaFill, int32 mode)
{
	int32 cnt2, res;

	PolygonRecord *DrawPolygon, *FirstPolygon;
	uint8 *AreaPos, *PolygonPos;


	AreaPos = (uint8 *) AreaFill;
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	FirstPolygon = DrawPolygon;
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons - 1; cnt2++)
	{
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;

		if ((mode == 0) || ((DrawPolygon->PolygonType & 1) == 0))
		{
			res = CheckPolygonCrossesOtherPolygon(PolygonObject, DrawPolygon);

			if (res == -1)
				return 1;		// Polygon lines crosses
		}
	}

	res = CheckPolygonCrossesOtherPolygon(PolygonObject, FirstPolygon);

	if (res == -1)
		return 1;				// Polygon lines crosses

	return 0;
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

int32 CheckNoCrossesInObjectPolygon(ObjectPolygonRecord * ObjectPolygon)
{
	int32 cnt, cnt2, cnt3, count;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	count = ObjectPolygon->NrVertices;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = (*ObjectPolygon).Points[cnt].x;
		y1 = (*ObjectPolygon).Points[cnt].y;

		if (cnt < count - 1)
		{
			x2 = (*ObjectPolygon).Points[cnt + 1].x;
			y2 = (*ObjectPolygon).Points[cnt + 1].y;
		}
		else
		{
			x2 = (*ObjectPolygon).Points[0].x;
			y2 = (*ObjectPolygon).Points[0].y;
		}

		for (cnt2 = cnt + 2; cnt2 < cnt + count - 1; cnt2++)
		{
			cnt3 = cnt2;

			if (cnt3 >= count)
				cnt3 -= count;

			x3 = (*ObjectPolygon).Points[cnt3].x;
			y3 = (*ObjectPolygon).Points[cnt3].y;
			cnt3 = cnt2 + 1;

			if (cnt3 >= count)
				cnt3 -= count;

			x4 = (*ObjectPolygon).Points[cnt3].x;
			y4 = (*ObjectPolygon).Points[cnt3].y;

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

int32 GetLineDirection2(double x1, double y1, double x2, double y2, int32 mode)
{
	double divx, divy, divxabs, divyabs;
	divx = x1 - x2;
	divy = y1 - y2;
	divxabs = fabs(divx);
	divyabs = fabs(divy);

	if (InRange(divx, 0))
	{
		if (InRange(divy, 0))
			return -1;
		else
		{
			if (X1SmallerThenX2(divy, 0))
				return 0;
			else
				return 4;
		}
	}

	if (InRange(divy, 0))
	{
		if (X1SmallerThenX2(divx, 0))
			return 2;
		else
			return 6;
	}

	if ((InRange(divx, divy)) && (X1SmallerThenX2(divx, 0)))
		return 1;

	if ((InRange(divx, divy)) && (X1GreaterThenX2(divx, 0)))
		return 5;

	if ((InRange(divx, -divy)) && (X1SmallerThenX2(divx, 0)))
		return 3;

	if ((InRange(divx, -divy)) && (X1GreaterThenX2(divx, 0)))
		return 7;

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetLineDirection(double x1, double y1, double x2, double y2, int32 mode)
{
	double divx, divy, rico;
	divx = x2 - x1;
	divy = y2 - y1;

	if (x1 == x2)
	{
		if (y2 > y1)
			return 0;

		return 4;
	}

	rico = divy / divx;

	if ((fabs(divx) < 20) && (fabs(rico) > 99))
	{
		if (y2 > y1)
			return 0;

		return 4;
	}

	if ((fabs(divy) < 20) && (fabs(rico) < 0.011))
	{
		if (x2 > x1)
			return 2;

		return 6;
	}

	if (InRange7(rico, 1))
	{
		if (y2 > y1)
			return 1;

		return 5;
	}

	if (InRange7(rico, -1))
	{
		if (y2 > y1)
			return 7;

		return 3;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetStartPoint(PolygonRecord * Polygon)
{
	double *x1, *y1, *x2, *y2, *x3, *y3;
	int32 cnt;

	x1 = (double *) &((*Polygon).Points);
	y1 = x1 + 1;
	x2 = x1 + 2;
	y2 = x1 + 3;
	x3 = x1 + 4;
	y3 = x1 + 5;

	for (cnt = 0; cnt < Polygon->NrVertices; cnt++)
	{
		if (((NotInRange8(*x1, *x2)) || (NotInRange8(*x2, *x3)))
		        && ((NotInRange8(*y1, *y2)) || (NotInRange8(*y2, *y3))))
		{
			if ((cnt + 1) == Polygon->NrVertices)
				return 0;

			return cnt + 1;
		}

		x1 += 2;
		y1 += 2;
		x2 += 2;
		y2 += 2;
		x3 += 2;
		y3 += 2;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ReorganizePolygon(PolygonRecord * Polygon, PolygonRecord * ResultingPolygon, double TrimPointsDistance)
{
	int32 count, cnt, cnt2, cnt3, ok, AddPoint, MemSize;
	double *x1, *y1, *x3, *y3, LastX, LastY, *FirstX, *FirstY, px1, py1, px2, py2, xx1, yy1, xx2, yy2, xx3, yy3,
	       LinesAngle, Length12, Length23, Angle1, Angle2, Angle, TrimLength;
	PolygonRecord *TempPolygon;

	count = Polygon->NrVertices;
	MemSize = max(128 * 1024, count * sizeof(POINT) + 16384);
	AllocateSpecialMem(MEM_POLYGON, MemSize, (void **) &TempPolygon);

	ResultingPolygon->NrVertices = 0;
	TempPolygon->NrVertices = 0;

	if ((cnt = GetStartPoint(Polygon)) == -1)
		return 0;

	TempPolygon->NrVertices = count;
	FirstX = (double *) &((*Polygon).Points);
	FirstY = FirstX + 1;

	x1 = (double *) &((*Polygon).Points[cnt]);
	y1 = x1 + 1;
	LastX = *x1;
	LastY = *y1;
	x3 = (double *) &((*TempPolygon).Points);
	y3 = x3 + 1;
	*x3 = *x1;
	*y3 = *y1;
	x3 += 2;
	y3 += 2;
	x1 += 2;
	y1 += 2;

	if (++cnt == count)
		cnt = 0;

	cnt2 = 1;
	cnt3 = 1;

// ******************************************************************************
	while (cnt3 < count)
	{
		if ((NotInRange8(*x1, LastX)) || (NotInRange8(*y1, LastY)))
		{
			if (InRange8(*x1, LastX))
			{
				*x3 = LastX;
				*y3 = *y1;
				LastY = *y1;
			}
			else
			{
				if (InRange8(*y1, LastY))
				{
					*x3 = *x1;
					*y3 = LastY;
					LastX = *x1;
				}
				else
				{
					*x3 = *x1;
					*y3 = *y1;
					LastX = *x1;
					LastY = *y1;
				}
			}

			x3 += 2;
			y3 += 2;
		}
		else
			TempPolygon->NrVertices--;

		x1 += 2;
		y1 += 2;

		if (++cnt == count)
		{
			cnt = 0;
			x1 = FirstX;
			y1 = FirstY;
		}

		cnt3++;
	}

	cnt2 = 0;

	for (cnt = 0; cnt < TempPolygon->NrVertices; cnt++)
	{
		xx1 = (*TempPolygon).Points[cnt].x;
		yy1 = (*TempPolygon).Points[cnt].y;

		if (cnt < TempPolygon->NrVertices - 2)
		{
			xx2 = (*TempPolygon).Points[cnt + 1].x;
			yy2 = (*TempPolygon).Points[cnt + 1].y;
			xx3 = (*TempPolygon).Points[cnt + 2].x;
			yy3 = (*TempPolygon).Points[cnt + 2].y;
		}
		else
		{
			if (cnt == TempPolygon->NrVertices - 1)
			{
				xx2 = (*TempPolygon).Points[0].x;
				yy2 = (*TempPolygon).Points[0].y;
				xx3 = (*TempPolygon).Points[1].x;
				yy3 = (*TempPolygon).Points[1].y;
			}
			else
			{
				xx2 = (*TempPolygon).Points[cnt + 1].x;
				yy2 = (*TempPolygon).Points[cnt + 1].y;
				xx3 = (*TempPolygon).Points[0].x;
				yy3 = (*TempPolygon).Points[0].y;
			}
		}

#ifdef _DEBUG

		if ((InRangeSpecial(xx2, 75.234e5, 0.02e5)) && (InRangeSpecial(yy2, 86.497e5, 0.02e5)))
			ok = 1;

#endif
		ConvNormalCoorToPolar(xx2, yy2, xx1, yy1, &Angle1, &Length12);
		ConvNormalCoorToPolar(xx2, yy2, xx3, yy3, &Angle2, &Length23);
		LinesAngle = fabs(Angle1 - Angle2);

		if (LinesAngle > ANGLE_CONVERT(180.0))
			LinesAngle = ANGLE_CONVERT(360.0) - LinesAngle;

		Angle = LinesAngle * 180.0 / PI;
		AddPoint = 1;

		if (Angle < 46.0)
		{
			ok = 1;
			// Trim the sharp point, by inserting two points
			AddPoint = 0;
			TrimLength = fabs(TrimPointsDistance * tan(ANGLE_90 - LinesAngle * 0.5) * 0.6);

//            TrimLength=min(TrimLength,(min(Length12,Length23)*0.8));
			if ((TrimLength < Length12 * 0.8) && (TrimLength < Length23 * 0.8))
			{
				px1 = cos(Angle1) * TrimLength + xx2;
				py1 = sin(Angle1) * TrimLength + yy2;
				(*ResultingPolygon).Points[cnt2].x = px1;
				(*ResultingPolygon).Points[cnt2].y = py1;
				ResultingPolygon->NrVertices++;
				cnt2++;
				px2 = cos(Angle2) * TrimLength + xx2;
				py2 = sin(Angle2) * TrimLength + yy2;
				(*ResultingPolygon).Points[cnt2].x = px2;
				(*ResultingPolygon).Points[cnt2].y = py2;
				ResultingPolygon->NrVertices++;
				cnt2++;
			}
			else
			{
				if ((TrimLength > Length12 * 0.8) && (TrimLength > Length23 * 0.8))
				{
				}
				else
				{
					if (TrimLength < Length12 * 0.8)
					{
						px1 = cos(Angle1) * Length23 + xx2;
						py1 = sin(Angle1) * Length23 + yy2;
						(*ResultingPolygon).Points[cnt2].x = px1;
						(*ResultingPolygon).Points[cnt2].y = py1;
						ResultingPolygon->NrVertices++;
						cnt2++;
					}
					else
					{
						px2 = cos(Angle2) * Length12 + xx2;
						py2 = sin(Angle2) * Length12 + yy2;
						(*ResultingPolygon).Points[cnt2].x = px2;
						(*ResultingPolygon).Points[cnt2].y = py2;
						ResultingPolygon->NrVertices++;
						cnt2++;
					}
				}
			}
		}

		if (AddPoint)
		{
			(*ResultingPolygon).Points[cnt2].x = xx2;
			(*ResultingPolygon).Points[cnt2].y = yy2;
			ResultingPolygon->NrVertices++;
			cnt2++;
		}
	}

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
	int32 Vert12, Vert23;
	int32 c1, c2;
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
			y1a = 1000000000;
			y2a = -1000000000;
		}
		else
		{
			y1a = -1000000000;
			y2a = 1000000000;
		}
	}
	else
	{
		if (x1 > x2)
		{
			x1a = 1000000000;
			x2a = -1000000000;
		}
		else
		{
			x1a = -1000000000;
			x2a = 1000000000;
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
			y3a = 1000000000;
			y2b = -1000000000;
		}
		else
		{
			y3a = -1000000000;
			y2b = 1000000000;
		}
	}
	else
	{
		if (x3 > x2)
		{
			x3a = 1000000000;
			x2b = -1000000000;
		}
		else
		{
			x3a = -1000000000;
			x2b = 1000000000;
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
		{
			if (OldY > 0.9999e9)
				OldYA = -1e9;
			else
				OldYA = OldY - AddNum;
		}
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
			{
				if (OldY > 0.9999e9)
					OldYA = -1e9;
				else
					OldYA = OldY - AddNum;
			}

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
#ifdef _DEBUG
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
		DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
		DrawLine(MultX(x2), MultY(y2), MultX(x3), MultY(y3));
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_YELLOW + DRAW_WITH_PEN_AND_NOT_FILLED);
		ThickNess *= 2.0;
		ellips2(MultX(*NewX1), MultY(*NewY1), Mult(ThickNess), Mult(ThickNess), 255);
		ellips2(MultX(*NewX2), MultY(*NewY2), Mult(ThickNess), Mult(ThickNess), 255);
		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
		ellips2(MultX(OldX), MultY(OldY), Mult(ThickNess), Mult(ThickNess), 255);
		sprintf(str, "1");
		TextOutUTF8(OutputDisplay, MultX(x1), MultY(y1), str, strlen(str));
		sprintf(str, "2");
		TextOutUTF8(OutputDisplay, MultX(x2), MultY(y2), str, strlen(str));
		sprintf(str, "3");
		TextOutUTF8(OutputDisplay, MultX(x3), MultY(y3), str, strlen(str));
		sprintf(str, "1a");
		TextOutUTF8(OutputDisplay, MultX(*NewX1), MultY(*NewY1), str, strlen(str));
		sprintf(str, "2a");
		TextOutUTF8(OutputDisplay, MultX(*NewX2), MultY(*NewY2), str, strlen(str));
#endif
	}

	return c1 * 2 + c2;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeBiggerSmallerPolygon2(PolygonRecord * Polygon, PolygonRecord * BiggerPolygon, double Thickness, int32 mode)
//  mode  bit 0 = 0 Bigger  polygon
//  mode  bit 0 = 1 Smaller polygon
{
	int32 count;
	uint8 PolygonBuf[10240];
	PolygonRecord *CutPolygon;

	SetTimer0();
	BiggerPolygon->NrVertices = 0;
	CutPolygon = (PolygonRecord *) & PolygonBuf;

	count = Polygon->NrVertices;
	return MakeBiggerSmallerPolygon(Polygon, BiggerPolygon, Thickness, mode);

#if 0

	if (count > 32)
		return MakeBiggerSmallerPolygon(Polygon, BiggerPolygon, Thickness, mode);

//  Thickness*=0.5;
	memset((uint8 *) & Object, 0, sizeof(ObjectRecord));
	Object.ObjectType = OBJECT_LINE;
	Object.Thickness = Thickness;

//  mode^=1;
//  if (count>8) {
//    BiggerPolygon->NrVertices=0;
//    return 0;
//  }
	if (AllocateMemAreaFillMemoryTemp(128 * 1024) != 0)
		return -1;

	if (AllocateMemPolygons(0) != 0)
		return -1;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	NewAreaFill->Info &= ~(OBJECT_SELECTED);
	NewAreaFill->NrPolygons = 1;
	NewAreaFill->MemSize = sizeof(AreaFillRecord);

	AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));

	if ((mode & 1) == 0)
	{	// Bigger polygon
		memset(AreaFillPolygon, 0, sizeof(PolygonRecord));
		AreaFillPolygon->NrVertices = 4;
		AreaFillPolygon->Points[0].x = -1e9;
		AreaFillPolygon->Points[0].y = -1e9;
		AreaFillPolygon->Points[1].x = 1e9;
		AreaFillPolygon->Points[1].y = -1e9;
		AreaFillPolygon->Points[2].x = 1e9;
		AreaFillPolygon->Points[2].y = 1e9;
		AreaFillPolygon->Points[3].x = -1e9;
		AreaFillPolygon->Points[3].y = 1e9;
	}
	else
	{	// Smaller polygon
		CopyPolygonToPolygon(Polygon, AreaFillPolygon);
	}

	SetMinMaxPolygon(AreaFillPolygon, 0);
	AreaFillPolygon->PolygonType = 0;
	NewAreaFill->MemSize += MemSizePolygon(AreaFillPolygon);

	OldValue2 = 0.1234;
	count2 = 0;

	for (cnt = 0; cnt < count; cnt++)
	{
		x1 = Polygon->Points[cnt].x;
		y1 = Polygon->Points[cnt].y;

		if (cnt < count - 1)
		{
			x2 = Polygon->Points[cnt + 1].x;
			y2 = Polygon->Points[cnt + 1].y;
		}
		else
		{
			x2 = Polygon->Points[0].x;
			y2 = Polygon->Points[0].y;
		}

		Object.x1 = x1;
		Object.y1 = y1;
		Object.x2 = x2;
		Object.y2 = y2 + 0.1;
		CutPolygon->PolygonType = 0;
		RandValue = GetNewRandomValue(0);
		res = -1;
		TryCount = 0;

		while ((res < 0) && (TryCount < 10))
		{
			if ((mode & 1) == 0)
			{	// Bigger polygon
				MakePolygonFromObject(&Object, CutPolygon, 0.0, (-cnt - 1.0 - RandValue) - (TryCount * 133.3), 1, 0);
			}
			else
			{	// Smaller polygon
				MakePolygonFromObject(&Object, CutPolygon, 0.0, (cnt + 1.0 + RandValue) + (TryCount * 133.3), 1, 0);
			}

			memmove(AreaFillMemTemp2, NewAreaFill, NewAreaFill->MemSize);
			res = MergePolygon(CutPolygon, 0);
			count2++;
			TryCount++;

			if (res < 0)
				ok = 1;
		}

		if (res < 0)
		{
			ok = 1;
			return -1;
		}
	}

	cnt3 = GetDifferenceTimer0inMilliSeconds();

	if ((mode & 1) == 0)
	{	// Bigger polygon
		AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) AreaFillPolygon;
		PolygonPos += MemSizePolygon(AreaFillPolygon);
		AreaFillPolygon = (PolygonRecord *) PolygonPos;

		if (AreaFillPolygon->NrVertices < 512)
		{
			ok = 1;
			CopyPolygonToPolygon(AreaFillPolygon, BiggerPolygon);
		}
	}
	else
	{	// Smaller polygon
		AreaFillPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));

		if (NewAreaFill->NrPolygons > 0)
		{
			if (AreaFillPolygon->NrVertices < 512)
			{
				CopyPolygonToPolygon(AreaFillPolygon, BiggerPolygon);
				ok = 1;
			}
		}
	}

	DeAllocateMemPolygons();
	DeAllocateMemAreaFills();
	return 0;
#endif
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeBiggerSmallerPolygon(PolygonRecord * Polygon, PolygonRecord * BiggerPolygon, double Thickness, int32 mode)
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

#ifdef _DEBUG
	int32 fp, cnt3;
	char str[MAX_LENGTH_STRING];
#endif

	CntMax = 0;
	count = Polygon->NrVertices;

	if (count <= 0)
		return -1;

	maxy = -1000000000.0;

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
	                           0.01e5, 0);
//  res=GetBiggerSmallerPoints(x1,y1,x2,y2,x3,y3,OldX,OldY,
//                             &NewX1,&NewY1,&NewX2,&NewY2,&OldX2,&OldY2,Thickness,0);

	c1a = (PointInPolygon(Polygon, NewX1, NewY1)) & 1;
	c2a = (PointInPolygon(Polygon, NewX2, NewY2)) & 1;

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

			c1a = (PointInPolygon(Polygon, NewX1, NewY1)) & 1;
			c2a = (PointInPolygon(Polygon, NewX2, NewY2)) & 1;

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

			c1a = (PointInPolygon(Polygon, NewX1, NewY1)) & 1;
			c2a = (PointInPolygon(Polygon, NewX2, NewY2)) & 1;

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
		c1a = (PointInPolygon(Polygon, NewX1, NewY1)) & 1;
		c2a = (PointInPolygon(Polygon, NewX2, NewY2)) & 1;

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
		
		(*BiggerPolygon).Points[cnt2].x = NewX1;
		(*BiggerPolygon).Points[cnt2].y = NewY1;
		
		cnt2++;
		x1 = x2;
		y1 = y2;
		x2 = x3;
		y2 = y3;
		cnt++;
	}

#ifdef _DEBUG

	if ((mode & 2) == 2)
	{
		sprintf(str, "c:\\pcb\\polygon.txt");

		if ((fp = FileOpenWriteUTF8(str)) <= 0)
			return 0;

		WriteLn(fp, "-------------------------------------------------------------");
		sprintf(str, "PolygonType %i", Polygon->PolygonType);
		WriteLn(fp, str);
		count = Polygon->NrVertices;

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			x1 = (*Polygon).Points[cnt3].x / 100000.0;
			y1 = (*Polygon).Points[cnt3].y / 100000.0;
			sprintf(str, "Polygon points  %14.5f,%14.5f %i", x1, y1, cnt3);
			WriteLn(fp, str);
		}

		WriteLn(fp, "-------------------------------------------------------------");
		count = BiggerPolygon->NrVertices;

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			x1 = (*BiggerPolygon).Points[cnt3].x / 100000.0;
			y1 = (*BiggerPolygon).Points[cnt3].y / 100000.0;
			sprintf(str, "Polygon points  %14.5f,%14.5f %i", x1, y1, cnt3);
			WriteLn(fp, str);
		}

		FileClose(fp);
	}

#endif
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePolygon(PolygonRecord * Polygon, double CentreX, double CentreY, double Rotation, int32 mode)
{
	int32 count, cnt3;

	count = Polygon->NrVertices;

	for (cnt3 = 0; cnt3 < count; cnt3++)
		RotatePointFromOtherPoint2(&(*Polygon).Points[cnt3].x, &(*Polygon).Points[cnt3].y, CentreX, CentreY, Rotation);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MirrorPolygon(PolygonRecord * Polygon, int32 mode)
{
	int32 count, cnt3;

	count = Polygon->NrVertices;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		if ((mode & 1) == 0)
		{	// Mirror X
			(*Polygon).Points[cnt3].x *= -1;
		}
		else
		{	// Mirror Y
			(*Polygon).Points[cnt3].y *= -1;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPolygon(PolygonRecord * Polygon)
{
	int32 count, cnt3;
	double x1, y1;

	count = Polygon->NrVertices;

	if (count == 0)
		return 0;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		x1 = (*Polygon).Points[cnt3].x;
		y1 = (*Polygon).Points[cnt3].y;

		if ((!_finite(x1)) || (!_finite(y1)) || (x1 < -1e9) || (x1 > 1e9) || (y1 < -1e9) || (y1 > 1e9))
			return 0;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckAreaFillsConnected(AreaFillRecord * AreaFill, AreaFillRecord * AreaFill2)
{
	int32 res, cnt2, count;
	PolygonRecord *DrawPolygon, *DrawPolygon2;
	uint8 *AreaPos, *PolygonPos;
	uint8 *AreaPos2, *PolygonPos2;

	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

	AreaPos2 = (uint8 *) AreaFill2;
	count = sizeof(AreaFillRecord);
	DrawPolygon2 = (PolygonRecord *) (AreaPos2 + sizeof(AreaFillRecord));

	res = CheckPolygonCompleetlyOutsidePolygon(DrawPolygon, DrawPolygon2);

	if (res == 1)
		return 0;				// polygon outside

	if (res == -1)
		return 1;				// polygon lines crossing

	if (res == -2)
		return 1;				// polygon lines crossing

	PolygonPos2 = (uint8 *) DrawPolygon2;
	PolygonPos2 += MemSizePolygon(DrawPolygon2);
	DrawPolygon2 = (PolygonRecord *) PolygonPos2;

	for (cnt2 = 1; cnt2 < AreaFill2->NrPolygons; cnt2++)
	{
		if (CheckPolygonCompleetlyInsidePolygon(DrawPolygon, DrawPolygon2) == 1)
			return 0;

		PolygonPos2 += MemSizePolygon(DrawPolygon2);
		DrawPolygon2 = (PolygonRecord *) PolygonPos2;
	}

	DrawPolygon2 = (PolygonRecord *) (AreaPos2 + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;
	PolygonPos += MemSizePolygon(DrawPolygon);
	DrawPolygon = (PolygonRecord *) PolygonPos;

	for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		if (CheckPolygonCompleetlyInsidePolygon(DrawPolygon2, DrawPolygon) == 1)
			return 0;

		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	return 1;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetLinePointNr(double x, double y)
{
	int32 cnt;

	cnt = 0;

	while (cnt < NrLinePoints)
	{
		if ((InRangeSpecial(x, (*LinePoints)[cnt].x, 5000.0)) && (InRangeSpecial(y, (*LinePoints)[cnt].y, 5000.0)))
			return cnt;

		cnt++;
	}

	return -1;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void InsertLineObjectPoint(double x1, double y1)
{
	LinePointRecord *LinePoint;

	LinePoint = &((*LinePoints)[NrLinePoints]);
	memset(LinePoint, 0, sizeof(LinePointRecord));
	LinePoint->x = x1;
	LinePoint->y = y1;
	NrLinePoints++;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetLineObjectNrFromEndPoint(double x, double y, double *NewX, double *NewY, int32 mode)
{
	int32 cnt, Mask;
	double x1, y1, x2, y2;
	ObjectRecord *Object;

	if (mode == 0)
		Mask = 1;
	else
		Mask = 2;

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		if ((Object->Info & Mask) == 0)
		{
			if ((InRangeSpecial(x, x1, 5000.0)) && (InRangeSpecial(y, y1, 5000.0)))
			{
				*NewX = x2;
				*NewY = y2;
				return cnt;
			}

			if ((InRangeSpecial(x, x2, 5000.0)) && (InRangeSpecial(y, y2, 5000.0)))
			{
				*NewX = x1;
				*NewY = y1;
				return cnt;
			}
		}
	}

	return -1;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetBoardOutlineAreaFill(AreaFillRecord * BoardOutlineAreaFill, double KeepOut, int32 mode)
{

	int32 cnt, cnt2, cnt3, NrLinePointsNumbers, NrVertices, LineSegments, SegmentCount, NrPolygons,
	      MainPolygonStartObjectNr;

	ObjectRecord *Object, *Object2;
	CompRecord *Comp;
	PolygonRecord *Polygon, *BoardOutlinePolygon, *NewPolygon, *FirstPolygon = NULL;
	double MaxPolygonLength, CurrentPolygonLength, x1, y1, x2, y2, x3, y3, x4, y4, LineBuf[4096];
	LinePointRecord *LinePoint;
	uint8 *AreaFillPos;
	int32 Found, Found2, Stop, PolygonError;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;

	Object = NULL;
	memset(BoardOutlineAreaFill, 0, sizeof(AreaFillRecord));
	BoardOutlineAreaFill->minx = 1000000000.0;
	BoardOutlineAreaFill->miny = 1000000000.0;
	BoardOutlineAreaFill->maxx = -1000000000.0;
	BoardOutlineAreaFill->maxy = -1000000000.0;
	NrObjects2 = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0) && (ObjectLine->Layer == BOARD_OUTLINE_LAYER))
		{
			x1 = ObjectLine->X1;
			y1 = ObjectLine->Y1;
			x2 = ObjectLine->X2;
			y2 = ObjectLine->Y2;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object = &((*Objects2)[NrObjects2]);
			Object->x1 = x1;
			Object->y1 = y1;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->Thickness = ObjectLine->LineThickNess;
			Object->Info = 0;;
			NrObjects2++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0) && (ObjectRect->Layer == BOARD_OUTLINE_LAYER))
		{
			x1 = ObjectRect->CentreX;
			y1 = ObjectRect->CentreY;
			x2 = ObjectRect->Width * 0.5;
			y2 = ObjectRect->Height * 0.5;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object = &((*Objects2)[NrObjects2]);
			Object->x1 = x1 - x2;
			Object->y1 = y1 - y2;
			Object->x2 = x1 - x2;
			Object->y2 = y1 + y2;
			Object->Thickness = ObjectRect->LineThickNess;
			Object->Info = 0;;
			NrObjects2++;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object = &((*Objects2)[NrObjects2]);
			Object->x1 = x1 - x2;
			Object->y1 = y1 + y2;
			Object->x2 = x1 + x2;
			Object->y2 = y1 + y2;
			Object->Thickness = ObjectRect->LineThickNess;
			Object->Info = 0;;
			NrObjects2++;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object = &((*Objects2)[NrObjects2]);
			Object->x1 = x1 + x2;
			Object->y1 = y1 + y2;
			Object->x2 = x1 + x2;
			Object->y2 = y1 - y2;
			Object->Thickness = ObjectRect->LineThickNess;
			Object->Info = 0;;
			NrObjects2++;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object = &((*Objects2)[NrObjects2]);
			Object->x1 = x1 + x2;
			Object->y1 = y1 - y2;
			Object->x2 = x1 - x2;
			Object->y2 = y1 - y2;
			Object->Thickness = ObjectRect->LineThickNess;
			Object->Info = 0;;
			NrObjects2++;
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (((ObjectCircle->Info & OBJECT_NOT_VISIBLE) == 0)
	       &&
	       (ObjectCircle->Layer == BOARD_OUTLINE_LAYER)) {
	      x1=ObjectCircle->CentreX;
	      y1=ObjectCircle->CentreY;
	      x2=ObjectCircle->Diam;
	      CircleMode=(int32)(ObjectCircle->CircleMode+0.1);
	      LineSegments=CircleToLineSegments(x1,y1,x2,CircleMode,(double *)&LineBuf);
	      SegmentCount=0;
	      for (cnt2=0;cnt2<LineSegments;cnt2++) {
	        if (NrObjects2>=MaxNrObjects2-2) {
	          AllocateMemObjects2(MaxNrObjects2+128);
	        }
	        Object=&((*Objects2)[NrObjects2]);
	        Object->x1=LineBuf[SegmentCount];
	        SegmentCount++;
	        Object->y1=LineBuf[SegmentCount];
	        SegmentCount++;
	        Object->x2=LineBuf[SegmentCount];
	        SegmentCount++;
	        Object->y2=LineBuf[SegmentCount];
	        SegmentCount++;
	        Object->x3=ObjectCircle->LineThickNess;
	        Object->Info=0;;
	        NrObjects2++;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0) && (ObjectArc->Layer == BOARD_OUTLINE_LAYER))
		{
			x1 = ObjectArc->CentreX;
			y1 = ObjectArc->CentreY;
			x2 = ObjectArc->Width;
			y2 = ObjectArc->Height;
			x3 = ObjectArc->StartDiffX;
			y3 = ObjectArc->StartDiffY;
			x4 = ObjectArc->EndDiffX;
			y4 = ObjectArc->EndDiffY;
			LineSegments = ArcToLineSegments(x1, y1, x2, y2, x3, y3, x4, y4, (double *) &LineBuf, 1);

			SegmentCount = 0;

			for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
			{
				if (NrObjects2 >= MaxNrObjects2 - 2)
					AllocateMemObjects2(MaxNrObjects2 + 128);

				Object = &((*Objects2)[NrObjects2]);
				Object->x1 = LineBuf[SegmentCount];
				SegmentCount++;
				Object->y1 = LineBuf[SegmentCount];
				SegmentCount++;
				Object->x2 = LineBuf[SegmentCount];
				SegmentCount++;
				Object->y2 = LineBuf[SegmentCount];
				SegmentCount++;
				Object->Thickness = ObjectArc->LineThickNess;
				Object->Info = 0;
				NrObjects2++;
			}
		}
	}

	NrObjects = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 3);
	}

	for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
	{
		Object = &((*Objects)[cnt3]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;
		x3 = Object->x3;
		y3 = Object->y3;
		x4 = Object->x4;
		y4 = Object->y4;

		switch (Object->ObjectType)
		{
		case OBJECT_LINE:
			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object2 = &((*Objects2)[NrObjects2]);
			Object2->x1 = x1;
			Object2->y1 = y1;
			Object2->x2 = x2;
			Object2->y2 = y2;
			Object2->Thickness = Object->Thickness;
			Object2->Info = 0;
			NrObjects2++;
			break;

		case OBJECT_RECT:
			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object2 = &((*Objects2)[NrObjects2]);
			Object2->x1 = x1;
			Object2->y1 = y1;
			Object2->x2 = x1 + x2;
			Object2->y2 = y1;
			Object2->Thickness = Object->Thickness;
			Object2->Info = 0;
			NrObjects2++;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object2 = &((*Objects2)[NrObjects2]);
			Object2->x1 = x1 + x2;
			Object2->y1 = y1;
			Object2->x2 = x1 + x2;
			Object2->y2 = y1 + y2;
			Object2->Thickness = Object->Thickness;
			Object2->Info = 0;
			NrObjects2++;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object2 = &((*Objects2)[NrObjects2]);
			Object2->x1 = x1 + x2;
			Object2->y1 = y1 + y2;
			Object2->x2 = x1;
			Object2->y2 = y1 + y2;
			Object2->Thickness = Object->Thickness;
			Object2->Info = 0;
			NrObjects2++;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 128);

			Object2 = &((*Objects2)[NrObjects2]);
			Object2->x1 = x1;
			Object2->y1 = y1 + y2;
			Object2->x2 = x1;
			Object2->y2 = y1;
			Object2->Thickness = Object->Thickness;
			Object2->Info = 0;
			NrObjects2++;
			break;

		case OBJECT_ARC:
			LineSegments = ArcToLineSegments(x1, y1, x2, y2, x3, y3, x4, y4, (double *) &LineBuf, 1);

			SegmentCount = 0;

			for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
			{
				if (NrObjects2 >= MaxNrObjects2 - 2)
					AllocateMemObjects2(MaxNrObjects2 + 128);

				Object2 = &((*Objects2)[NrObjects2]);
				Object2->x1 = LineBuf[SegmentCount];
				SegmentCount++;
				Object2->y1 = LineBuf[SegmentCount];
				SegmentCount++;
				Object2->x2 = LineBuf[SegmentCount];
				SegmentCount++;
				Object2->y2 = LineBuf[SegmentCount];
				SegmentCount++;
				Object2->Thickness = Object->Thickness;
				Object2->Info = 0;
				NrObjects2++;
			}

			break;
		}
	}

// ************************************************************************************************
// ************************************************************************************************

	if (NrObjects2 == 0)
		return -2;

	PolygonError = 0;
	AllocateSpecialMem(MEM_PLOT_LINES_BUF, 128 * 1024, (void **) &LinePoints);
	AllocateSpecialMem(MEM_PLOT_POINTS_MAP, 128 * 1024, (void **) &LinePointsNumbers);
	AllocateSpecialMem(MEM_POLYGON_BIGGER, 128 * 1024, (void **) &NewPolygon);
	AllocateSpecialMem(MEM_POLYGON, 128 * 1024, (void **) &Polygon);
	memset(LinePoints, 0, sizeof(LinePointsArray));
//  LinePointsNumbers=
	NrLinePoints = 0;
	NrLinePointsNumbers = 0;
	MaxPolygonLength = 0.0;

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);

		if ((Object->Info & 1) == 0)
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;

			if ((cnt2 = GetLinePointNr(x1, y1)) == -1)
			{
				InsertLineObjectPoint(x1, y1);
				LinePoint = &((*LinePoints)[NrLinePoints - 1]);
			}
			else
				LinePoint = &((*LinePoints)[cnt2]);

			LinePoint->NrLines++;

			if ((cnt2 = GetLinePointNr(x2, y2)) == -1)
			{
				InsertLineObjectPoint(x2, y2);
				LinePoint = &((*LinePoints)[NrLinePoints - 1]);
			}
			else
				LinePoint = &((*LinePoints)[cnt2]);

			LinePoint->NrLines++;
		}
	}

	for (cnt = 0; cnt < NrLinePoints; cnt++)
	{
		LinePoint = &((*LinePoints)[cnt]);

		if (LinePoint->NrLines != 2)
		{
			ok = 1;
			return -1;
		}
	}


	Stop = 0;

	while (!Stop)
	{
		Found = 0;
		cnt = 0;

		while ((!Found) && (cnt < NrObjects2))
		{
			Object = &((*Objects2)[cnt]);

			if ((Object->Info & 1) == 0)
				Found = 1;
			else
				cnt++;
		}

		if (Found)
		{
			x1 = Object->x1;
			y1 = Object->y1;
			NrVertices = 0;
			Polygon->NrVertices = 0;
//      Polygon->Points[NrVertices].x=x1;
//      Polygon->Points[NrVertices].y=y1;
//      NrVertices++;
			x2 = Object->x2;
			y2 = Object->y2;
			Polygon->Points[NrVertices].x = x2;
			Polygon->Points[NrVertices].y = y2;
			NrVertices++;
			Object->Info |= 1;

			while ((NotInRange4(x1, x2)) || (NotInRange4(y1, y2)))
			{
				cnt2 = 0;
				Found2 = 0;

				while ((cnt2 < NrObjects2) && (!Found2))
				{
					Object = &((*Objects2)[cnt2]);

					if ((Object->Info & 1) == 0)
					{
						if ((InRange4(x2, Object->x1)) && (InRange4(y2, Object->y1)))
						{
							Found2 = 1;
							x2 = Object->x2;
							y2 = Object->y2;
							Polygon->Points[NrVertices].x = x2;
							Polygon->Points[NrVertices].y = y2;
							NrVertices++;
							Object->Info |= 1;
						}
						else
						{
							if ((InRange4(x2, Object->x2)) && (InRange4(y2, Object->y2)))
							{
								Found2 = 1;
								x2 = Object->x1;
								y2 = Object->y1;
								Polygon->Points[NrVertices].x = x2;
								Polygon->Points[NrVertices].y = y2;
								NrVertices++;
								Object->Info |= 1;
							}
						}

						if (!Found2)
							cnt2++;
					}
					else
						cnt2++;
				}

				if (!Found2)
					Stop = 1;
			}

			Polygon->NrVertices = NrVertices;
			CurrentPolygonLength = 0.0;

			for (cnt3 = 0; cnt3 < NrVertices; cnt3++)
			{
				x1 = Polygon->Points[cnt3].x;
				y1 = Polygon->Points[cnt3].y;

				if (cnt3 < NrVertices - 1)
				{
					x2 = Polygon->Points[cnt3].x;
					y2 = Polygon->Points[cnt3].y;
				}
				else
				{
					x2 = Polygon->Points[0].x;
					y2 = Polygon->Points[0].y;
				}

				CurrentPolygonLength += SQR(x2 - x1) + SQR(y2 - y1);

			}

			if (CurrentPolygonLength > MaxPolygonLength)
			{
				MaxPolygonLength = CurrentPolygonLength;
				MainPolygonStartObjectNr = cnt;
			}

//      DrawTestPolygon(Polygon,2);
		}
		else
			Stop = 1;
	}

// ************************************************************************************************
// ************************************************************************************************

	Object = &((*Objects2)[0]);
	NrPolygons = 0;
	MainPolygonStartObjectNr = 0;
	AreaFillPos = (uint8 *) BoardOutlineAreaFill;
	AreaFillPos += sizeof(AreaFillRecord);
	BoardOutlineAreaFill->MemSize = sizeof(AreaFillRecord);

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);
		Object->Info = 0;
	}

	Stop = 0;

	while (!Stop)
	{
		Found = 0;

		if (NrPolygons == 0)
		{
			cnt = MainPolygonStartObjectNr;
			Object = &((*Objects2)[cnt]);
			Found = 1;
		}
		else
		{
			cnt = 0;

			while ((!Found) && (cnt < NrObjects2))
			{
				Object = &((*Objects2)[cnt]);

				if ((Object->Info & 1) == 0)
					Found = 1;
				else
					cnt++;
			}
		}

		if (Found)
		{
			x1 = Object->x1;
			y1 = Object->y1;
			NrVertices = 0;
			Polygon->NrVertices = 0;
			x2 = Object->x2;
			y2 = Object->y2;
			Polygon->Points[NrVertices].x = x2;
			Polygon->Points[NrVertices].y = y2;
			NrVertices++;
			Object->Info |= 1;

			while ((NotInRange4(x1, x2)) || (NotInRange4(y1, y2)))
			{
				cnt2 = 0;
				Found2 = 0;

				while ((cnt2 < NrObjects2) && (!Found2))
				{
					Object = &((*Objects2)[cnt2]);

					if ((Object->Info & 1) == 0)
					{
						if ((InRange4(x2, Object->x1)) && (InRange4(y2, Object->y1)))
						{
							Found2 = 1;
							x2 = Object->x2;
							y2 = Object->y2;
							Polygon->Points[NrVertices].x = x2;
							Polygon->Points[NrVertices].y = y2;
							NrVertices++;
							Object->Info |= 1;
						}
						else
						{
							if ((InRange4(x2, Object->x2)) && (InRange4(y2, Object->y2)))
							{
								Found2 = 1;
								x2 = Object->x1;
								y2 = Object->y1;
								Polygon->Points[NrVertices].x = x2;
								Polygon->Points[NrVertices].y = y2;
								NrVertices++;
								Object->Info |= 1;
							}
						}

						if (!Found2)
							cnt2++;
					}
					else
						cnt2++;
				}

				if (!Found2)
					Stop = 1;
			}

			Polygon->NrVertices = NrVertices;
			Polygon->PolygonType = 0;
			SetMinMaxPolygon(Polygon, 0);
			BoardOutlinePolygon = (PolygonRecord *) AreaFillPos;

			if ((mode & 1) == 0)
			{
				CopyPolygonToPolygon(Polygon, BoardOutlinePolygon);
				AreaFillPos += MemSizePolygon(BoardOutlinePolygon);
				BoardOutlineAreaFill->MemSize += MemSizePolygon(BoardOutlinePolygon);
			}
			else
			{
				if (NrPolygons == 0)
					MakeBiggerSmallerPolygon(Polygon, NewPolygon, KeepOut * 2.0 - 100.0, 1);
				else
					MakeBiggerSmallerPolygon(Polygon, NewPolygon, KeepOut * 2.0 + 100.0, 0);

				SetMinMaxPolygon(NewPolygon, 0);
				CopyPolygonToPolygon(NewPolygon, BoardOutlinePolygon);
				BoardOutlineAreaFill->MemSize += MemSizePolygon(NewPolygon);
				AreaFillPos += MemSizePolygon(NewPolygon);
//        DrawTestPolygon(NewPolygon,2);
			}

			if (NrPolygons == 0)
				FirstPolygon = BoardOutlinePolygon;

			if (!CheckNoCrossesInPolygon(BoardOutlinePolygon))
				PolygonError = 1;

//int32 CheckPolygonInsideAreaFill(PolygonRecord *PolygonObject,AreaFillRecord *AreaFill,int32 mode);

			NrPolygons++;
			BoardOutlineAreaFill->NrPolygons = NrPolygons;
		}
		else
			Stop = 1;
	}

	if (!FirstPolygon)
		return -2;

	AreaFillPos = (uint8 *) BoardOutlineAreaFill;
	AreaFillPos += sizeof(AreaFillRecord);
	Polygon = (PolygonRecord *) AreaFillPos;
	AreaFillPos += MemSizePolygon(Polygon);

	for (cnt = 1; cnt < BoardOutlineAreaFill->NrPolygons; cnt++)
	{
		Polygon = (PolygonRecord *) AreaFillPos;

		if (CheckPolygonCompleetlyInsidePolygon(Polygon, FirstPolygon) != 1)
			PolygonError = 1;

		Polygon->PolygonType = 5;
		AreaFillPos += MemSizePolygon(Polygon);
	}

	if (PolygonError)
		return -2;

	if (FirstPolygon->NrVertices > 200)
		return -2;

	BoardOutlineAreaFill->minx = FirstPolygon->minx;
	BoardOutlineAreaFill->maxx = FirstPolygon->maxx;
	BoardOutlineAreaFill->miny = FirstPolygon->miny;
	BoardOutlineAreaFill->maxy = FirstPolygon->maxy;
	BoardOutlineAreaFill->NrVerticesStartPolygon = FirstPolygon->NrVertices;

	for (cnt = 0; cnt < FirstPolygon->NrVertices; cnt++)
	{
		BoardOutlineAreaFill->StartPolygon[cnt].x = (float) FirstPolygon->Points[cnt].x;
		BoardOutlineAreaFill->StartPolygon[cnt].y = (float) FirstPolygon->Points[cnt].y;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetArcEndPoints(ObjectRecord * Object, double *px1, double *py1, double *px2, double *py2, int32 mode)
{
	double x1, y1, x2, y2, x3, y3, x4, y4, Width, Height, Angle1, Angle2, Length1, Length2, sinx, sinx2, Length;

	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	x3 = Object->x3;
	y3 = Object->y3;
	x4 = Object->x4;
	y4 = Object->y4;
	Width = x2 * 0.5;

	if (y2 == 0.0)
		Height = x2 * 0.5;
	else
		Height = y2 * 0.5;

	ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &Length1);
	ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &Length2);

	if (InRange(Width, Height))
	{
		*px1 = x1 + Width * cos(Angle1);
		*py1 = y1 + Height * sin(Angle1);
		*px2 = x1 + Width * cos(Angle2);
		*py2 = y1 + Height * sin(Angle2);
	}
	else
	{
		sinx = sin(Angle1);
		sinx2 = sinx * sinx;
		Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
		*px1 = x1 + Length * cos(Angle1);
		*py1 = y1 + Length * sin(Angle1);
		sinx = sin(Angle2);
		sinx2 = sinx * sinx;
		Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
		*px2 = x1 + Length * cos(Angle2);
		*py2 = y1 + Length * sin(Angle2);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetArcEndPoints2(ObjectArcRecord * ObjectArc, double *px1, double *py1, double *px2, double *py2, int32 mode)
{
	double x1, y1, x2, y2, x3, y3, x4, y4, Width, Height, Angle1, Angle2, Length, sinx, sinx2, Length1, Length2;

	x1 = ObjectArc->CentreX;
	y1 = ObjectArc->CentreY;
	x2 = ObjectArc->Width;
	y2 = ObjectArc->Height;
	x3 = ObjectArc->StartDiffX;
	y3 = ObjectArc->StartDiffY;
	x4 = ObjectArc->EndDiffX;
	y4 = ObjectArc->EndDiffY;

	Width = x2 * 0.5;

	if (y2 == 0.0)
		Height = x2 * 0.5;
	else
		Height = y2 * 0.5;

	ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &Length1);
	ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &Length2);

	if (InRange(Width, Height))
	{
		*px1 = x1 + Width * cos(Angle1);
		*py1 = y1 + Height * sin(Angle1);
		*px2 = x1 + Width * cos(Angle2);
		*py2 = y1 + Height * sin(Angle2);
	}
	else
	{
		sinx = sin(Angle1);
		sinx2 = sinx * sinx;
		Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
		*px1 = x1 + Length * cos(Angle1);
		*py1 = y1 + Length * sin(Angle1);
		sinx = sin(Angle2);
		sinx2 = sinx * sinx;
		Length = Width * Height * sqrt(1 / (SQR(Height) * (1 - sinx2) + SQR(Width) * sinx2));
		*px2 = x1 + Length * cos(Angle2);
		*py2 = y1 + Length * sin(Angle2);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectIsBigPolygon(ObjectRecord * Object)
{
	int32 count;
	GeomPolygonRecord *GeomPolygon;

	switch (Object->ObjectType)
	{
	case OBJECT_POLYGON:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		if (Object->ObjectType2 != 0)
			return 0;

		GeomPolygon = (GeomPolygonRecord *) Object->Address;

		if (!GeomPolygon)
			return 0;

		count = GeomPolygon->NrVertices;

		if (count < 500)
		{
			if (GeomPolygon->NrSubPolygons == 0)
				return 0;

			if (GeomPolygon->NrVerticesMainPolygon == 0)
				return 0;
		}

		return 1;

	default:
		return 0;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectIsPolygonWithOpenSpots(ObjectRecord * Object)
{
	GeomPolygonRecord *GeomPolygon;

	switch (Object->ObjectType)
	{
	case OBJECT_POLYGON:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		if (Object->ObjectType2 != 0)
			return 0;

		GeomPolygon = (GeomPolygonRecord *) Object->Address;

		if (!GeomPolygon)
			return 0;

		if (GeomPolygon->NrSubPolygons == 0)
			return 0;

		if (GeomPolygon->NrVerticesMainPolygon == 0)
			return 0;

		return 1;

	default:
		return 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetAreaFillFromBigPolygonObject(ObjectRecord * Object, AreaFillRecord ** NewAreaFill, double Clearance,
                                      int32 mode)
{
	GeomPolygonRecord *GeomPolygon;
	GeomSubPolygonRecord *GeomSubPolygon = NULL;
	double x1, y1, x5, y5, TempValue;
	int32 Rotation, cnt, cnt2, cnt3, count, MemSize, res, MemSizeAreaFill;
	uint8 *PBuf, *PBuf2;
	AreaFillRecord *AreaFill;
	PolygonRecord *AreaFillPolygon, *NewPolygon, *Polygon;

	/*
	typedef struct {
	          float X,Y,Rotation,Clearance;
	          int32 Mirror,Address,MemSize;
	          AreaFillRecord *AreaFill;
	        } CachePolygonObjectRecord ;
	*/
	if (NrCachePolygonObjectsUpdate)
	{
		for (cnt3 = 0; cnt3 < NrCachePolygonObjects; cnt3++)
			free(CachePolygonObjects[cnt3].AreaFill);

		NrCachePolygonObjects = 0;
		NrCachePolygonObjectsUpdate = 0;
	}

	res = 1;

	for (cnt3 = 0; cnt3 < NrCachePolygonObjects; cnt3++)
	{
		if ((InRange(CachePolygonObjects[cnt3].X, Object->x1)) && (InRange(CachePolygonObjects[cnt3].Y, Object->y1))
		        && (InRange(CachePolygonObjects[cnt3].Clearance, Clearance))
		        && (InRange2(CachePolygonObjects[cnt3].Rotation, Object->RotationAngle))
		        && (CachePolygonObjects[cnt3].Mirror == Object->Mirror)
		        && (CachePolygonObjects[cnt3].Address == (uint32) Object->Address))
			break;
	}

	if (cnt3 < NrCachePolygonObjects)
	{
		*NewAreaFill = CachePolygonObjects[cnt3].AreaFill;
		return 0;
	}

	GeomPolygon = (GeomPolygonRecord *) Object->Address;
	count = GeomPolygon->NrVertices;
	MemSizeAreaFill = count * sizeof(PointRecord);
	MemSizeAreaFill += GeomPolygon->NrSubPolygons * sizeof(PolygonInitRecord) + 16384;

	if (NrCachePolygonObjects == MAX_NR_CACHE_POLYGON_OBJECTS)
	{
		for (cnt3 = 0; cnt3 < NrCachePolygonObjects; cnt3++)
			free(CachePolygonObjects[cnt3].AreaFill);

		cnt3 = 0;
		NrCachePolygonObjects = 0;
	}

	CachePolygonObjects[cnt3].AreaFill = malloc(MemSizeAreaFill);
	*NewAreaFill = CachePolygonObjects[cnt3].AreaFill;

	if (!CachePolygonObjects[cnt3].AreaFill)
		return -1;

	CachePolygonObjects[cnt3].X = Object->x1;
	CachePolygonObjects[cnt3].Y = Object->y1;
	CachePolygonObjects[cnt3].Clearance = Clearance;
	CachePolygonObjects[cnt3].Rotation = Object->RotationAngle;
	CachePolygonObjects[cnt3].Mirror = Object->Mirror;
	CachePolygonObjects[cnt3].Address = (uint32) Object->Address;
	NrCachePolygonObjects++;

	AreaFill = CachePolygonObjects[cnt3].AreaFill;
	memset(AreaFill, 0, sizeof(AreaFillRecord));
	AreaFill->MemSize = sizeof(AreaFillRecord);
	Rotation = GetRotationFromFloat(Object->RotationAngle);
	x1 = Object->x1;
	y1 = Object->y1;
	PBuf2 = (uint8 *) AreaFill;
	PBuf2 += sizeof(AreaFillRecord);
	AreaFillPolygon = (PolygonRecord *) PBuf2;
	AreaFill->NrPolygons = GeomPolygon->NrSubPolygons + 1;
	PBuf = (uint8 *) GeomPolygon;

	for (cnt2 = 0; cnt2 < GeomPolygon->NrSubPolygons + 1; cnt2++)
	{
		Polygon = (PolygonRecord *) PBuf2;
		memset(Polygon, 0, sizeof(PolygonRecord));

		if (cnt2 == 0)
		{
			if (GeomPolygon->NrSubPolygons == 0)
				count = GeomPolygon->NrVertices;
			else
				count = GeomPolygon->NrVerticesMainPolygon;
		}
		else
			count = GeomSubPolygon->NrVertices;

		Polygon->NrVertices = count;
		AreaFill->MemSize += sizeof(PolygonInitRecord) + count * sizeof(PointRecord);

		for (cnt = 0; cnt < count; cnt++)
		{
			if (cnt2 == 0)
			{
				x5 = GeomPolygon->Points[cnt].x;
				y5 = GeomPolygon->Points[cnt].y;
			}
			else
			{
				x5 = GeomSubPolygon->Points[cnt].x;
				y5 = GeomSubPolygon->Points[cnt].y;
			}

			if (Object->Mirror == 1)
				x5 = -x5;

			switch (Rotation)
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
				RotatePointFromOtherPoint2(&x5, &y5, 0.0, 0.0, Object->RotationAngle);
				break;
			}

			Polygon->Points[cnt].x = x5 + x1;
			Polygon->Points[cnt].y = y5 + y1;
		}

		if (Clearance > 0.0)
		{
			MemSize = MemSizePolygon(Polygon);
			AllocateSpecialMem(MEM_POLYGON_BIGGER, MemSize, (void **) &NewPolygon);

			if (cnt2 == 0)
			{
				res = MakeBiggerSmallerPolygon2(Polygon, NewPolygon, Clearance * 2.0, 0);	// Make bigger polygon
			}
			else
			{
				res = MakeBiggerSmallerPolygon2(Polygon, NewPolygon, Clearance * 2.0, 1);	// Make smaller polygon
			}

			CopyPolygonToPolygon(NewPolygon, Polygon);
		}

		SetMinMaxPolygon(Polygon, 0);

		if (cnt2 == 0)
			PBuf += sizeof(GeomPolygonInitRecord);
		else
			PBuf += sizeof(GeomSubPolygonInitRecord);

		PBuf += count * sizeof(PointRecord);
		PBuf2 += sizeof(PolygonInitRecord);
		PBuf2 += count * sizeof(PointRecord);
		GeomSubPolygon = (GeomSubPolygonRecord *) PBuf;
	}

	CachePolygonObjects[cnt3].MemSize = AreaFill->MemSize;
	SetMinMaxPolygon(AreaFillPolygon, 0);

	AreaFill->minx = AreaFillPolygon->minx;
	AreaFill->miny = AreaFillPolygon->miny;
	AreaFill->maxx = AreaFillPolygon->maxx;
	AreaFill->maxy = AreaFillPolygon->maxy;


	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetFirstPointPolygonObject(ObjectRecord * Object, double *x, double *y, int32 mode)
{
	GeomPolygonRecord *GeomPolygon;
	double x5, y5, TempValue;
	int32 Rotation;

	*x = 0;
	*y = 0;

	switch (Object->ObjectType)
	{
	case OBJECT_POLYGON:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		if (Object->ObjectType2 != 0)
			return 0;

		GeomPolygon = (GeomPolygonRecord *) Object->Address;

		if (!GeomPolygon)
			return 0;

		break;

	default:
		return 0;
	}

	x5 = GeomPolygon->Points[0].x;
	y5 = GeomPolygon->Points[0].y;
	Rotation = GetRotationFromFloat(Object->RotationAngle);

	if (Object->Mirror == 1)
		x5 = -x5;

	switch (Rotation)
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
		RotatePointFromOtherPoint2(&x5, &y5, 0.0, 0.0, Object->RotationAngle);
		break;
	}

	*x = x5 + Object->x1;
	*y = y5 + Object->y1;
	return 1;
}

// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// **************************************************************************************

int32 GetPolygonDirection(PolygonRecord * PolygonObject)
{
	int32 count, cnt, cnt2, cnt3;
	double *vx, *vy, x1, y1, x2, y2, x3, y3, maxx, Angle1, Angle2, Distance1, Distance2, Angle;

	maxx = -1e9;
	cnt2 = 0;
	count = PolygonObject->NrVertices;
	vx = (double *) &((*PolygonObject).Points);
	vy = vx + 1;

	for (cnt = 0; cnt < count; cnt++)
	{
		if (*vx > maxx)
		{
			maxx = *vx;
			cnt2 = cnt;
		}

		vx += 2;
		vy += 2;
	}

	x2 = PolygonObject->Points[cnt2].x;
	y2 = PolygonObject->Points[cnt2].y;
#ifdef _DEBUG

	if ((InRangeSpecial(x2, 176.377e5, 0.005e5)) && (InRangeSpecial(y2, 54.039e5, 0.005e5)))
		ok = 1;

#endif
	cnt3 = cnt2;

	if (cnt2 < count - 1)
		cnt2++;
	else
		cnt2 = 0;

	x3 = PolygonObject->Points[cnt2].x;
	y3 = PolygonObject->Points[cnt2].y;

	if (cnt3 == 0)
		cnt3 = count - 1;
	else
		cnt3--;

	x1 = PolygonObject->Points[cnt3].x;
	y1 = PolygonObject->Points[cnt3].y;

	ConvNormalCoorToPolar(x2, y2, x1, y1, &Angle1, &Distance1);
	ConvNormalCoorToPolar(x2, y2, x3, y3, &Angle2, &Distance2);
	Angle = Angle1 - Angle2;


	if (Angle > 0.0)
	{
		return 1;				// CCW
	}

	return 0;					// CW

}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
