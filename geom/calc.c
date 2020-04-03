/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc.c
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
#include "calc.h"
#include "calcdef.h"
#include "string.h"
#include "math.h"
#include "stdio.h"
#include "polygon.h"
#include "utf8.h"

extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern int32 DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY;
extern HWND GEOMWindow;

double EmptyCharacter[18] = { 0.0, 0.0, 0.4,
                              0.0, 0.0, 1.3,
                              0.0, 0.6, 1.3,
                              0.0, 0.6, 0.4,
                              0.0, 0.0, 0.4,
                              1.0, 0.0, 0.0
                            };


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ScanParameters(int32 NrParameters, LPSTR Str, int32 mode)
{
	int32 cnt, cnt2, pos[256], count, le, res, TempUnits;
	char SubStr[1000];
	int32 FoundUnits;

	ParametersRelative = 0;
	TempUnits = -1;
	count = 0;
	le = strlen(Str);

	if (le == 0)
		return -1;

	if (Str[0] == '@')
	{
		ParametersRelative = 1;
		memmove(&Str[0], &Str[1], le - 1);
		le--;

		if (le == 0)
			return -1;
	}
	else
	if (Str[0] == '#')
	{
		ParametersRelative = 0;
		memmove(&Str[0], &Str[1], le - 1);
		le--;

		if (le == 0)
			return -1;
	}

	pos[0] = -1;

	for (cnt = 0; cnt < le; cnt++)
	{
		if (Str[cnt] == ',')
		{
			pos[count + 1] = cnt;
			count++;
		}
	}

	pos[count + 1] = cnt;
	count++;

	if (count > 32)
		return -3;

	if ((NrParameters != -1) && (count != NrParameters))
		return -2;

	for (cnt = 0; cnt < count; cnt++)
	{
		memset(&SubStr, 0, sizeof(SubStr));
		memmove(&SubStr, &Str[pos[cnt] + 1], pos[cnt + 1] - pos[cnt] - 1);
		le = strlen(SubStr);
		FoundUnits = 0;
		cnt2 = le - 1;

		while ((cnt2 >= 0) && (SubStr[cnt2] == ' '))
		{
			SubStr[cnt2] = 0;
			cnt2--;
		}

		le = strlen(SubStr);
		cnt2 = 0;

		while ((cnt2 < le) && (SubStr[cnt2] == ' '))
			cnt2++;

		if (mode == 0)
		{
			res = sscanf((LPSTR) & SubStr[cnt2], "%f", &ParamsFloat[cnt]);

			if (res == 0)
				return -3;

			_strlwr(SubStr);
			le = strlen(SubStr);

			if (le > 2)
			{
				if ((SubStr[le - 1] == 'm') && (SubStr[le - 2] == 'm'))
				{
//          ParamsFloat[cnt]*=100000.0;
					TempUnits = 1;
				}
			}

			if (le > 4)
			{
				if ((SubStr[le - 1] == 's') && (SubStr[le - 2] == 'l') && (SubStr[le - 3] == 'i')
				        && (SubStr[le - 4] == 'm'))
				{
//          ParamsFloat[cnt]*=2540.0;
					TempUnits = 0;
				}
			}
		}
		else
		{
			res = sscanf(SubStr, "%i", &ParamsInt[cnt]);

			if (res == 0)
				return -3;
		}
	}

	if (mode == 0)
	{
		for (cnt = 0; cnt < count; cnt++)
		{
			if (TempUnits == -1)
			{
				switch (Units)
				{
				case 0:
					ParamsFloat[cnt] *= 2540.0;
					break;

				case 1:
					ParamsFloat[cnt] *= 100000.0;
					break;
				}
			}
			else
			{
				switch (TempUnits)
				{
				case 0:
					ParamsFloat[cnt] *= 2540.0;
					break;

				case 1:
					ParamsFloat[cnt] *= 100000.0;
					break;
				}
			}
		}
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetMinMaxText(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                   LPSTR Str)
{
	double TextSizeX, TextSizeY;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	TextSizeX = FontSize * 0.9 * DefFontSize * (strlen(Str));
	TextSizeY = FontSize * DefFontSize;

	x1 = 0.0;
	y1 = 0.0;
	x2 = 0.0;
	y2 = TextSizeY;
	x3 = TextSizeX;
	y3 = TextSizeY;
	x4 = TextSizeX;
	y4 = 0.0;
	RotatePoint2(&x1, &y1, Rotation);
	RotatePoint2(&x2, &y2, Rotation);
	RotatePoint2(&x3, &y3, Rotation);
	RotatePoint2(&x4, &y4, Rotation);

	if (Mirror == 1)
	{
		x1 = -x1;
		x2 = -x2;
		x3 = -x3;
		x4 = -x4;
	}

	TextMinX = (X + min(x1, min(x2, min(x3, x4))));
	TextMinY = (Y + min(y1, min(y2, min(y3, y4))));
	TextMaxX = (X + max(x1, max(x2, max(x3, x4))));
	TextMaxY = (Y + max(y1, max(y2, max(y3, y4))));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDirection(double x1, double y1, double x2, double y2, ObjectRecord * TraceObject)
{
	double divx, divy, divxabs, divyabs;
	divx = x2 - x1;
	divy = y2 - y1;
	divxabs = fabs(divx);
	divyabs = fabs(divy);
	TraceObject->ObjectType = 0;

	if (InRange(divy, 0))
	{
		TraceObject->ObjectType = PIN_LINE_HOR;

		if (divx > 0)
		{
			TraceObject->x1 = (float) x1;
			TraceObject->y1 = (float) y1;
		}
		else
		{
			TraceObject->x1 = (float) x2;
			TraceObject->y1 = (float) y2;
		}

		TraceObject->x2 = (float) divxabs;
	}
	else
	{
		if (InRange(divx, 0))
		{
			TraceObject->ObjectType = PIN_LINE_VER;

			if (divy > 0)
			{
				TraceObject->x1 = (float) x1;
				TraceObject->y1 = (float) y1;
			}
			else
			{
				TraceObject->x1 = (float) x2;
				TraceObject->y1 = (float) y2;
			}

			TraceObject->x2 = (float) divyabs;
		}
		else
		{
			if (InRange(divyabs, divxabs))
			{
				if (divx > 0)
				{
					TraceObject->x1 = (float) x1;
					TraceObject->y1 = (float) y1;

					if (divy > 0)
						TraceObject->ObjectType = PIN_LINE_DIAG2;
					else
						TraceObject->ObjectType = PIN_LINE_DIAG1;
				}
				else
				{
					TraceObject->x1 = (float) x2;
					TraceObject->y1 = (float) y2;

					if (divy < 0)
						TraceObject->ObjectType = PIN_LINE_DIAG2;
					else
						TraceObject->ObjectType = PIN_LINE_DIAG1;
				}

				TraceObject->x2 = (float) divxabs;
			}
			else
				return -1;
		}
	}

	return 0;
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestLine2(double LineX1, double LineY1, double LineX2, double LineY2, double Thickness)
{
	double LineXmin, LineXmax, LineYmin, LineYmax, DifX, DifY, xa, xb, ya, yb;
	double rico, rico2;
	PolygonRecord *PolygonPointObject, *PolygonObject;
	uint8 PolygonBuf1[8192];
	uint8 PolygonBuf2[8192];

	LineXmin = LineX1;
	LineYmin = LineY1;
	LineXmax = LineX2;
	LineYmax = LineY2;

	if (LineXmin > LineXmax)
	{
		LineXmin = LineX2;
		LineXmax = LineX1;
	}

	if (LineYmin > LineYmax)
	{
		LineYmin = LineY2;
		LineYmax = LineY1;
	}

	if ((X1SmallerThenX2(LineXmax, SearchMinX)) || (X1GreaterThenX2(LineXmin, SearchMaxX))
	        || (X1SmallerThenX2(LineYmax, SearchMinY)) || (X1GreaterThenX2(LineYmin, SearchMaxY)))
		return 0;


	if ((X1SmallerThenX2(LineXmax, SearchMaxX)) && (X1GreaterThenX2(LineXmin, SearchMinX))
	        && (X1SmallerThenX2(LineYmax, SearchMaxY)) && (X1GreaterThenX2(LineYmin, SearchMinY)))
		return 3;


	if ((X1SmallerThenX2(LineX1, SearchMaxX)) && (X1GreaterThenX2(LineX1, SearchMinX))
	        && (X1SmallerThenX2(LineY1, SearchMaxY)) && (X1GreaterThenX2(LineY1, SearchMinY)))
		return 1;

	if ((X1SmallerThenX2(LineX2, SearchMaxX)) && (X1GreaterThenX2(LineX2, SearchMinX))
	        && (X1SmallerThenX2(LineY2, SearchMaxY)) && (X1GreaterThenX2(LineY2, SearchMinY)))
		return 2;

	if ((InRange(LineX1, LineX2)) || (InRange(LineY1, LineY2)))
		return 3;

	DifX = LineX2 - LineX1;
	DifY = LineY2 - LineY1;
	rico = LineY2;
	rico = (rico - LineY1) / (LineX2 - LineX1);
	ya = ((SearchMinX - LineX1) * rico) + LineY1;
	yb = ((SearchMaxX - LineX1) * rico) + LineY1;

	if (((ya > SearchMinY) && (ya < SearchMaxY)) || ((yb > SearchMinY) && (yb < SearchMaxY)))
		return 3;

	rico2 = LineX2;
	rico2 = (rico2 - LineX1) / (LineY2 - LineY1);
	xa = ((SearchMinY - LineY1) * rico2) + LineX1;
	xb = ((SearchMaxY - LineY1) * rico2) + LineX1;

	if (((xa > SearchMinX) && (xa < SearchMaxX)) || ((xb > SearchMinX) && (xb < SearchMaxX)))
		return 3;


	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	memset(&NewObject, 0, sizeof(NewObject));
	NewObject.x1 = (float) LineX1;
	NewObject.y1 = (float) LineY1;
	NewObject.x2 = (float) LineX2;
	NewObject.y2 = (float) LineY2;
	NewObject.ObjectType = OBJECT_LINE;
	NewObject.Thickness = (float) max(Thickness, 0.1e5);

	MakePolygonFromPlotObject(&NewObject, PolygonObject, 0.0, 16, 32);

//  DrawFilledPolygon(PolygonObject,4);
	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 3;

	return 0;

	/*

	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	           (y2-y1)
	y = (x-x1)*------- + y1
	           (x2-x1)

	           (x2-x1)
	x = (y-y1)*------- + x1
	           (y2-y1)



	y = x-x1 + y1

	x = y-y1 + x1

	*/

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

void SetCursorFromRealPosition(double x, double y)
{
	int32 x1, y1;

	x1 = MultX(x);
	y1 = MultY(y);

	if ((x1 > DrawWindowMinX) && (x1 < DrawWindowMaxX) && (y1 > DrawWindowMinY) && (y1 < DrawWindowMaxY))
		SetCursorPos(x1 + ClientStartX, y1 + ClientStartY);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetArcEndPoints(ObjectRecord * Object, double *px1, double *py1, double *px2, double *py2, int32 mode)
{
	double x1, y1, x2, y2, x3, y3, x4, y4, Width, Height;
	double Angle1, Angle2, Length1, Length2;

	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	Width = x2 * 0.5;
	Height = y2 * 0.5;

	if (Object->ObjectType == OBJECT_CIRCLE)
	{
		switch (Object->Info2)
		{
		case 1:
			x3 = Width;
			y3 = 0.0;
			x4 = 0.0;
			y4 = Width;
			break;

		case 2:
			x3 = 0.0;
			y3 = -Width;
			x4 = Width;
			y4 = 0.0;
			break;

		case 3:
			x3 = 0.0;
			y3 = -Width;
			x4 = 0.0;
			y4 = Width;
			break;

		case 4:
			x3 = -Width;
			y3 = 0.0;
			x4 = 0.0;
			y4 = -Width;
			break;

		case 6:
			x3 = -Width;
			y3 = 0.0;
			x4 = Width;
			y4 = 0.0;
			break;

		case 8:
			x3 = 0.0;
			y3 = Width;
			x4 = -Width;
			y4 = 0.0;
			break;

		case 9:
			x3 = Width;
			y3 = 0.0;
			x4 = -Width;
			y4 = 0.0;
			break;

		case 12:
			x3 = 0.0;
			y3 = Width;
			x4 = 0.0;
			y4 = -Width;
			break;

		default:
			x3 = 0.0;
			y3 = Width;
			x4 = 0.0;
			y4 = Width;
			break;
		}

		*px1 = x1 + x3;
		*py1 = y1 + y3;
		*px2 = x1 + x4;
		*py2 = y1 + y4;
		return;
	}

	x3 = Object->x3;
	y3 = Object->y3;
	x4 = Object->x4;
	y4 = Object->y4;
	*px1 = 0.0;
	*py1 = 0.0;
	*px2 = 0.0;
	*py2 = 0.0;

	if ((x3 == x4) && (y3 == y4))
		return;

	ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &Length1);
	ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &Length2);

	*px1 = (x1 + Width * cos(Angle1));
	*py1 = (y1 + Height * sin(Angle1));
	*px2 = (x1 + Width * cos(Angle2));
	*py2 = (y1 + Height * sin(Angle2));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestArc2(ObjectRecord * Object)
{
	double ArcXmin, ArcXmax, ArcYmin, ArcYmax, ArcThickness2;
	double x1, y1, x2, y2;
	PolygonRecord *PolygonPointObject, *PolygonObject;
	uint8 PolygonBuf1[8192];
	uint8 PolygonBuf2[8192];
#ifdef _DEBUG
	int32 ok;
#endif

	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;

#ifdef _DEBUG

	if ((InRange5(x1, 19e5)) && (InRange5(y1, 19e5)))
		ok = 1;

#endif

	ArcThickness2 = Object->Thickness / 2;

	ArcXmin = x1 - x2 * 0.5 - ArcThickness2;
	ArcYmin = y1 - y2 * 0.5 - ArcThickness2;
	ArcXmax = x1 + x2 * 0.5 + ArcThickness2;
	ArcYmax = y1 + y2 * 0.5 + ArcThickness2;

	if ((ArcXmax < SearchMinX) || (ArcXmin > SearchMaxX) || (ArcYmax < SearchMinY) || (ArcYmin > SearchMaxY))
		return 0;


	if ((ArcXmax < SearchMaxX) && (ArcXmin > SearchMinX) && (ArcYmax < SearchMaxY) && (ArcYmin > SearchMinY))
		return 1;

	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	MakePolygonFromPlotObject(Object, PolygonObject, 0.0, 16, 32);

//  DrawFilledPolygon(PolygonObject,4);
	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestCircle2(ObjectRecord * Object)
{
	double ArcXmin, ArcXmax, ArcYmin, ArcYmax, ArcThickness2;
	double x1, y1, x2, y2, x3, y3, x4, y4;
	ObjectRecord NewObject;
	PolygonRecord *PolygonPointObject, *PolygonObject;
	uint8 PolygonBuf1[8192];
	uint8 PolygonBuf2[8192];
#ifdef _DEBUG
	int32 ok;
#endif


	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->x2;

#ifdef _DEBUG

	if ((InRange5(x1, 19e5)) && (InRange5(y1, 19e5)))
		ok = 1;

#endif

	ArcThickness2 = Object->Thickness / 2;

	ArcXmin = x1 - x2 * 0.5 - ArcThickness2;
	ArcYmin = y1 - y2 * 0.5 - ArcThickness2;
	ArcXmax = x1 + x2 * 0.5 + ArcThickness2;
	ArcYmax = y1 + y2 * 0.5 + ArcThickness2;

	if ((ArcXmax < SearchMinX) || (ArcXmin > SearchMaxX) || (ArcYmax < SearchMinY) || (ArcYmin > SearchMaxY))
		return 0;


	if ((ArcXmax < SearchMaxX) && (ArcXmin > SearchMinX) && (ArcYmax < SearchMaxY) && (ArcYmin > SearchMinY))
		return 1;

	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	memset(&NewObject, 0, sizeof(NewObject));
	NewObject.x1 = (float) x1;
	NewObject.y1 = (float) y1;
	NewObject.x2 = (float) x2;
	NewObject.y2 = (float) x2;

	switch (Object->Info2)
	{
	case 1:
		x3 = x2;
		y3 = 0.0;
		x4 = 0.0;
		y4 = x2;
		break;

	case 2:
		x3 = 0.0;
		y3 = -x2;
		x4 = x2;
		y4 = 0.0;
		break;

	case 3:
		x3 = 0.0;
		y3 = -x2;
		x4 = 0.0;
		y4 = x2;
		break;

	case 4:
		x3 = -x2;
		y3 = 0.0;
		x4 = 0.0;
		y4 = -x2;
		break;

	case 6:
		x3 = -x2;
		y3 = 0.0;
		x4 = x2;
		y4 = 0.0;
		break;

	case 8:
		x3 = 0.0;
		y3 = x2;
		x4 = -x2;
		y4 = 0.0;
		break;

	case 9:
		x3 = x2;
		y3 = 0.0;
		x4 = -x2;
		y4 = 0.0;
		break;

	case 12:
		x3 = 0.0;
		y3 = x2;
		x4 = 0.0;
		y4 = -x2;
		break;

	default:
		x3 = 0.0;
		y3 = x2;
		x4 = 0.0;
		y4 = x2;
		break;
	}

	NewObject.x3 = (float) x3;
	NewObject.y3 = (float) y3;
	NewObject.x4 = (float) x4;
	NewObject.y4 = (float) y4;
	NewObject.ObjectType = OBJECT_ARC;
	NewObject.Thickness = (float) max(Object->Thickness, 0.1e5);

	MakePolygonFromPlotObject(&NewObject, PolygonObject, 0.0, 16, 32);

//  DrawFilledPolygon(PolygonObject,4);
	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestText2(double x1, double y1, double x2, double Rotation, int32 Mirror, LPSTR Text)
{
	double tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4;
	PolygonRecord *PolygonPointObject, *PolygonObject;
	uint8 PolygonBuf1[1024];
	uint8 PolygonBuf2[1024];

	GetMinMaxText(x1, y1, x2, 0, Rotation, 0, Mirror, Text);

	if ((TextMaxX < SearchMinX) || (TextMinX > SearchMaxX) || (TextMaxY < SearchMinY) || (TextMinY > SearchMaxY))
		return 0;

	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	GetMinMaxText(x1, y1, x2, 0, 0.0, 0, Mirror, Text);
	tx1 = TextMinX;
	ty1 = TextMinY;
	tx2 = TextMaxX;
	ty2 = TextMinY;
	tx3 = TextMaxX;
	ty3 = TextMaxY;
	tx4 = TextMinX;
	ty4 = TextMaxY;
	/*
	if (Mirror == 1)
	{
		tx1 = -tx1;
		tx2 = -tx2;
		tx3 = -tx3;
		tx4 = -tx4;
	}
	*/
	RotatePointFromOtherPoint2(&tx1, &ty1, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx2, &ty2, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx3, &ty3, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx4, &ty4, x1, y1, Rotation);

	PolygonObject->NrVertices = 4;
	PolygonObject->Points[0].x = tx1;
	PolygonObject->Points[0].y = ty1;
	PolygonObject->Points[1].x = tx2;
	PolygonObject->Points[1].y = ty2;
	PolygonObject->Points[2].x = tx3;
	PolygonObject->Points[2].y = ty3;
	PolygonObject->Points[3].x = tx4;
	PolygonObject->Points[3].y = ty4;

	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 1;

	return 0;
}

// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// **************************************************************************************

void MakePolygonFromSpecialLine(double x1, double y1, double x2, double y2, ObjectPolygonRecord * ObjectPolygon,
                                double Thickness1, double Thickness2, int32 mode)
{
	double Length, Angle, x3, y3, x4, y4;


	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);
	x3 = Thickness1 * 0.5 * sin(Angle);
	y3 = Thickness1 * 0.5 * cos(Angle);
	x4 = Thickness2 * 0.5 * sin(Angle);
	y4 = Thickness2 * 0.5 * cos(Angle);
	ObjectPolygon->NrVertices = 4;
	ObjectPolygon->Points[0].x = x1 + x3;
	ObjectPolygon->Points[0].y = y1 - y3;
	ObjectPolygon->Points[1].x = x1 - x3;
	ObjectPolygon->Points[1].y = y1 + y3;
	ObjectPolygon->Points[2].x = x2 - x4;
	ObjectPolygon->Points[2].y = y2 + y4;
	ObjectPolygon->Points[3].x = x2 + x4;
	ObjectPolygon->Points[3].y = y2 - y4;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 TextStringToLineSegments(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror,
                               LPSTR str, double *LineSegments)
{
	char code, *str2;
	int32 cnt, cnt2, cnt3, cnt4, NrPolyLines, lengte, count, oldx, oldy, SegmentCount;
	double incX, incY, x1d, y1d, x1, y1, lengte2, NewLine, PointX[20], PointY[20];
	double *LineCodeP;
#ifdef _DEBUG
	int32 ok;
#endif

	lengte2 = strlen(str);

	LineCodeP = (double *) &EmptyCharacter;
	SegmentCount = 0;
	Size *= DefFontSize;
	incX = 0.9 * Size;
	incY = 0.0;
	RotatePoint2(&incX, &incY, Rotation);
	str2 = str;
#ifdef _DEBUG

	if (str[0] == ' ')
		ok = 1;

#endif
	lengte = (int32) strlen(str);

	for (cnt4 = 0; cnt4 < lengte; cnt4++)
	{
		code = (*str2);
//    code='+';
		oldx = -100000000;
		oldy = -100000000;
		count = 0;
		NrPolyLines = 0;

		if ((code > 32) && (code < 127))
		{
			code -= 33;
			NrPolyLines = (*Chars)[code].NrPolyLines;
			LineCodeP = (double *) &(*Chars)[code].Line;
		}
		else
		{
			if (code != 32)
			{
				NrPolyLines = 1;
				LineCodeP = (double *) &EmptyCharacter;
			}
		}

		cnt2 = 0;

		for (cnt = 0; cnt < NrPolyLines; cnt++)
		{
			count = 0;

			do
			{
				x1d = LineCodeP[cnt2 + 1];
				y1d = LineCodeP[cnt2 + 2];
				y1d -= 0.4;
				x1 = x1d * Size;
				y1 = y1d * Size;

				RotatePoint2(&x1, &y1, Rotation);

				if (Mirror == 1)
					x1 = -x1;

				PointX[count] = x1 + x;
				PointY[count] = y1 + y;
				count++;
				cnt2 += 3;
				NewLine = (LineCodeP[cnt2]);
			}
			while (InRange2(NewLine, 0.0));

			for (cnt3 = 0; cnt3 < count - 1; cnt3++)
			{
				LineSegments[SegmentCount++] = PointX[cnt3];
				LineSegments[SegmentCount++] = PointY[cnt3];
				LineSegments[SegmentCount++] = PointX[cnt3 + 1];
				LineSegments[SegmentCount++] = PointY[cnt3 + 1];
			}
		}

		if (Mirror == 0)
		{
			x += incX;
			y += incY;
		}
		else
		{
			x -= incX;
			y += incY;
		}

		str2++;
	}

	return SegmentCount / 4;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectToLineSegments(ObjectRecord * Object, double *LineSegments, int32 mode)
{
	int32 NrSegments, cnt2, count, SegmentCount;
	double x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, hoek1, hoek2, hoek, hoek_inc, Length, sinx, sinx2, lengte1, lengte2;
	NrSegments = 32;
	SegmentCount = 0;

	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	x3 = Object->x3;
	y3 = Object->y3;
	x4 = Object->x4;
	y4 = Object->y4;

	if (Object->ObjectType == OBJECT_RECT)
	{
		x2 *= 0.5;
		y2 *= 0.5;
		LineSegments[SegmentCount++] = (x1 - x2);
		LineSegments[SegmentCount++] = (y1 - y2);
		LineSegments[SegmentCount++] = (x1 + x2);
		LineSegments[SegmentCount++] = (y1 - y2);
		LineSegments[SegmentCount++] = (x1 + x2);
		LineSegments[SegmentCount++] = (y1 + y2);
		LineSegments[SegmentCount++] = (x1 - x2);
		LineSegments[SegmentCount++] = (y1 + y2);
		LineSegments[SegmentCount++] = (x1 - x2);
		LineSegments[SegmentCount++] = (y1 - y2);
		return 4;
	}

	if (Object->ObjectType == OBJECT_CIRCLE)
	{
		y2 = x2;

		switch (Object->Info2 & 15)
		{
		case 1:
			x3 = x2;
			y3 = 0.0;
			x4 = 0.0;
			y4 = x2;
			break;

		case 2:
			x3 = 0.0;
			y3 = -x2;
			x4 = x2;
			y4 = 0.0;
			break;

		case 3:
			x3 = 0.0;
			y3 = -x2;
			x4 = 0.0;
			y4 = x2;
			break;

		case 4:
			x3 = -x2;
			y3 = 0.0;
			x4 = 0.0;
			y4 = -x2;
			break;

		case 6:
			x3 = -x2;
			y3 = 0.0;
			x4 = x2;
			y4 = 0.0;
			break;

		case 8:
			x3 = 0.0;
			y3 = x2;
			x4 = -x2;
			y4 = 0.0;
			break;

		case 9:
			x3 = x2;
			y3 = 0.0;
			x4 = -x2;
			y4 = 0.0;
			break;

		case 12:
			x3 = 0.0;
			y3 = x2;
			x4 = 0.0;
			y4 = -x2;
			break;

		default:
			x3 = 0.0;
			y3 = x2;
			x4 = 0.0;
			y4 = x2;
			break;
		}
	}

	if (mode == 0)
	{
		if (max(x2, y2) > (400 * 2540))
			NrSegments = 64;

		if (max(x2, y2) > (1000 * 2540))
			NrSegments = 128;
	}
	else
	{
		if (max(x2, y2) > (100 * 2540))
			NrSegments = 64;

		if (max(x2, y2) > (250 * 2540))
			NrSegments = 128;

		if (max(x2, y2) > (500 * 2540))
			NrSegments = 256;
	}

	if ((InRange(x3, x4)) && (InRange(y3, y4)))
	{
		hoek1 = ANGLE_90;
		hoek2 = hoek1 + ANGLE_360;
	}
	else
	{
		x3 += x1;
		y3 += y1;
		x4 += x1;
		y4 += y1;
		ConvNormalCoorToPolar(x1, y1, x3, y3, &hoek1, &lengte1);
		ConvNormalCoorToPolar(x1, y1, x4, y4, &hoek2, &lengte2);

		if (hoek2 < hoek1)
			hoek2 += ANGLE_360;
	}

	hoek = hoek1;
	count = (int32) ((hoek2 - hoek1) / (ANGLE_360 / NrSegments));
	count = max(1, count);
	hoek_inc = ((hoek2 - hoek1) / count);

	for (cnt2 = 0; cnt2 < count + 1; cnt2++)
	{
		sinx = sin(hoek);
		sinx2 = sinx * sinx;
		Length = (x2 * 0.5) * (y2 * 0.5) * sqrt(1 / (SQR((y2 * 0.5)) * (1 - sinx2) + SQR((x2 * 0.5)) * sinx2));
		x5 = Length * cos(hoek);
		y5 = Length * sin(hoek);
		LineSegments[SegmentCount++] = (x1 + x5);
		LineSegments[SegmentCount++] = (y1 + y5);
		hoek += hoek_inc;
	}

	return (SegmentCount - 2) / 2;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CalcDistanceBetweenObjects(int32 mode)
{
	ObjectPolygonRecord *ObjectPolygon, *ObjectPolygon1, *ObjectPolygon2, *ObjectPolygon3;
	ObjectRecord *Object, *Object1, *Object2, *Object3, NewObject1, NewObject2, TestObject1, TestObject2;
	PolygonRecord *PolygonObject1, *PolygonObject2;
	double MinDistance, MinCenterDistance, dist, Thickness1, Thickness2;
	double LinePoints1[2048], LinePoints2[2048];
	double x1, y1, x2, y2, x3, y3, x4, y4;
	int32 cnt, cnt2, NrPoints1, NrPoints2, Swap, count, count2;
	char str[MAX_LENGTH_STRING];

	PolygonObject1 = (PolygonRecord *) & LinePoints1[0];
	PolygonObject2 = (PolygonRecord *) & LinePoints2[0];
	Object1 = NULL;
	Object2 = NULL;
	ObjectPolygon1 = NULL;
	ObjectPolygon2 = NULL;
	memset(&NewObject1, 0, sizeof(NewObject1));
	memset(&NewObject2, 0, sizeof(NewObject2));
	memset(&TestObject1, 0, sizeof(TestObject1));
	memset(&TestObject2, 0, sizeof(TestObject2));
	NewObject1.ObjectType = OBJECT_POLYGON;
	NewObject2.ObjectType = OBJECT_POLYGON;
	TestObject1.ObjectType = OBJECT_LINE;
	TestObject2.ObjectType = OBJECT_LINE;
	count = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			count++;

			if (Object1 == NULL)
				Object1 = Object;
			else
			{
				if (Object2 == NULL)
					Object2 = Object;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			count++;

			if (Object1 == NULL)
			{
				ObjectPolygon1 = ObjectPolygon;
				Object1 = &NewObject1;
			}
			else
			{
				if (Object2 == NULL)
				{
					ObjectPolygon2 = ObjectPolygon;
					Object2 = &NewObject2;
				}
			}
		}
	}

	if (count != 2)
	{
		MessageBoxUTF8(GEOMWindow, SC(0, "Two objects should be selected"), SC(48, "Error"), MB_APPLMODAL | MB_OK);
		return 0;
	}

	if ((Object1->ObjectType == OBJECT_TEXT) || (Object1->ObjectType == OBJECT_TEXT))
		return 0;

	/*
	  if ((Object1->Layer==PLACEMENT_OUTLINE_LAYER)
	     ||
	     (Object1->Layer==PLACEMENT_OUTLINE_LAYER)) {
	    return 0;
	  }
	*/
	Swap = 0;

	switch (Object1->ObjectType)
	{
	case OBJECT_LINE:
		switch (Object2->ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_RECT:
		case OBJECT_POLYGON:
			break;
		}

		break;

	case OBJECT_CIRCLE:
	case OBJECT_ARC:
		switch (Object2->ObjectType)
		{
		case OBJECT_LINE:
			Swap = 1;
			break;

		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_RECT:
		case OBJECT_POLYGON:
			break;
		}

		break;

	case OBJECT_RECT:
		switch (Object2->ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
			Swap = 1;
			break;

		case OBJECT_RECT:
		case OBJECT_POLYGON:
			break;
		}

		break;

	case OBJECT_POLYGON:
		switch (Object2->ObjectType)
		{
		case OBJECT_LINE:
		case OBJECT_CIRCLE:
		case OBJECT_ARC:
		case OBJECT_RECT:
			Swap = 1;
			break;

		case OBJECT_POLYGON:
			break;
		}

		break;
	}

	if (Swap)
	{
		Object3 = Object1;
		Object1 = Object2;
		Object2 = Object3;
		ObjectPolygon3 = ObjectPolygon1;
		ObjectPolygon1 = ObjectPolygon2;
		ObjectPolygon2 = ObjectPolygon3;
	}

	x1 = Object1->x1;
	y1 = Object1->y1;
	x2 = Object1->x2;
	y2 = Object1->y2;
	x3 = Object2->x1;
	y3 = Object2->y1;
	x4 = Object2->x2;
	y4 = Object2->y2;

	if (Object1->ObjectType == OBJECT_POLYGON)
	{
		x1 = ((ObjectPolygon1->maxx + ObjectPolygon1->minx) * 0.5);
		y1 = ((ObjectPolygon1->maxy + ObjectPolygon1->miny) * 0.5);
	}

	if (Object2->ObjectType == OBJECT_POLYGON)
	{
		x3 = ((ObjectPolygon2->maxx + ObjectPolygon2->minx) * 0.5);
		y3 = ((ObjectPolygon2->maxy + ObjectPolygon2->miny) * 0.5);
	}

	Thickness1 = Object1->Thickness * 0.5;
	Thickness2 = Object2->Thickness * 0.5;
	MinDistance = 0.0;
	MinCenterDistance = 0.0;

// *******************************************************************************************************
// *******************************************************************************************************
	switch (Object1->ObjectType)
	{
	case OBJECT_LINE:
		switch (Object2->ObjectType)
		{
		case OBJECT_LINE:
			MinCenterDistance = MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0);
			MinDistance = MinCenterDistance - Thickness1 - Thickness2;
			break;

		case OBJECT_CIRCLE:
		case OBJECT_ARC:
			MinCenterDistance = MinDistancePointToLine(x1, y1, x2, y2, x3, y3, 0);

			if (Object1->Thickness == 0.0)
				MinDistance = MinCenterDistance - Thickness1 - Object2->x2 * 0.5;
			else
			{
				NrPoints2 = ObjectToLineSegments(Object2, (double *) &LinePoints2, 1);
				dist = 1e9;

				for (cnt2 = 0; cnt2 < NrPoints2; cnt2++)
				{
					x3 = LinePoints2[cnt2 * 2];
					y3 = LinePoints2[cnt2 * 2 + 1];
					x4 = LinePoints2[cnt2 * 2 + 2];
					y4 = LinePoints2[cnt2 * 2 + 3];
					dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
				}

				MinDistance = dist - Thickness1 - Thickness2;
			}

			break;

		case OBJECT_RECT:
			MinCenterDistance = MinDistancePointToLine(x1, y1, x2, y2, x3, y3, 0);
			NrPoints2 = ObjectToLineSegments(Object2, (double *) &LinePoints2, 1);
			dist = 1e9;

			for (cnt2 = 0; cnt2 < NrPoints2; cnt2++)
			{
				x3 = LinePoints2[cnt2 * 2];
				y3 = LinePoints2[cnt2 * 2 + 1];
				x4 = LinePoints2[cnt2 * 2 + 2];
				y4 = LinePoints2[cnt2 * 2 + 3];
				dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
			}

			if (Object1->Thickness == 0.0)
				MinDistance = dist - Thickness1;
			else
				MinDistance = dist - Thickness1 - Thickness2;

			break;

		case OBJECT_POLYGON:
			MinCenterDistance = MinDistancePointToLine(x1, y1, x2, y2, x3, y3, 0);
			dist = 1e9;
			count = ObjectPolygon2->NrVertices;

			if ((ObjectPolygon2->NrSubPolygons > 0) && (ObjectPolygon2->NrVerticesMainPolygon > 0))
				count = ObjectPolygon2->NrVerticesMainPolygon;

			for (cnt2 = 0; cnt2 < count; cnt2++)
			{
				x3 = ObjectPolygon2->Points[cnt2].x;
				y3 = ObjectPolygon2->Points[cnt2].y;

				if (cnt2 < ObjectPolygon2->NrVertices - 1)
				{
					x4 = ObjectPolygon2->Points[cnt2 + 1].x;
					y4 = ObjectPolygon2->Points[cnt2 + 1].y;
				}
				else
				{
					x4 = ObjectPolygon2->Points[0].x;
					y4 = ObjectPolygon2->Points[0].y;
				}

				dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
			}

			MinDistance = dist - Thickness1;
			break;
		}

		break;

// *******************************************************************************************************
// *******************************************************************************************************
	case OBJECT_CIRCLE:
	case OBJECT_ARC:
		if (Object1->Thickness == 0.0)
		{
			switch (Object2->ObjectType)
			{
			case OBJECT_CIRCLE:
			case OBJECT_ARC:
				MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);

				if (Object2->Thickness == 0.0)
					MinDistance = MinCenterDistance - Object2->x2 * 0.5 - Object1->x2 * 0.5;
				else
				{
					NrPoints2 = ObjectToLineSegments(Object2, (double *) &LinePoints2, 1);
					dist = 1e9;

					for (cnt2 = 0; cnt2 < NrPoints2; cnt2++)
					{
						x3 = LinePoints2[cnt2 * 2];
						y3 = LinePoints2[cnt2 * 2 + 1];
						x4 = LinePoints2[cnt2 * 2 + 2];
						y4 = LinePoints2[cnt2 * 2 + 3];
						dist = min(dist, MinDistancePointToLine(x3, y3, x4, y4, x1, y1, 0));
					}

					MinDistance = dist - Object1->x2 * 0.5 - Thickness2;
				}

				break;

			case OBJECT_RECT:
				MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);
				NrPoints2 = ObjectToLineSegments(Object2, (double *) &LinePoints2, 1);
				dist = 1e9;

				for (cnt2 = 0; cnt2 < NrPoints2; cnt2++)
				{
					x3 = LinePoints2[cnt2 * 2];
					y3 = LinePoints2[cnt2 * 2 + 1];
					x4 = LinePoints2[cnt2 * 2 + 2];
					y4 = LinePoints2[cnt2 * 2 + 3];
					dist = min(dist, MinDistancePointToLine(x3, y3, x4, y4, x1, y1, 0));
				}

				if (Object2->Thickness == 0.0)
					MinDistance = dist - Object1->x2 * 0.5;
				else
					MinDistance = dist - Thickness2 - Object1->x2 * 0.5;

				break;

			case OBJECT_POLYGON:
				MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);
				dist = 1e9;
				count = ObjectPolygon2->NrVertices;

				if ((ObjectPolygon2->NrSubPolygons > 0) && (ObjectPolygon2->NrVerticesMainPolygon > 0))
					count = ObjectPolygon2->NrVerticesMainPolygon;

				for (cnt2 = 0; cnt2 < count; cnt2++)
				{
					x3 = ObjectPolygon2->Points[cnt2].x;
					y3 = ObjectPolygon2->Points[cnt2].y;

					if (cnt2 < count - 1)
					{
						x4 = ObjectPolygon2->Points[cnt2 + 1].x;
						y4 = ObjectPolygon2->Points[cnt2 + 1].y;
					}
					else
					{
						x4 = ObjectPolygon2->Points[0].x;
						y4 = ObjectPolygon2->Points[0].y;
					}

					dist = min(dist, MinDistancePointToLine(x3, y3, x4, y4, x1, y1, 0));
				}

				MinDistance = dist - Object1->x2 * 0.5;
				break;
			}
		}
		else
		{
// *******************************************************************************************************
			NrPoints1 = ObjectToLineSegments(Object1, (double *) &LinePoints1, 1);

			switch (Object2->ObjectType)
			{
			case OBJECT_CIRCLE:
			case OBJECT_ARC:
				MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);

				if (Object2->Thickness == 0.0)
				{
					dist = 1e9;

					for (cnt = 0; cnt < NrPoints1; cnt++)
					{
						x1 = LinePoints1[cnt * 2];
						y1 = LinePoints1[cnt * 2 + 1];
						x2 = LinePoints1[cnt * 2 + 2];
						y2 = LinePoints1[cnt * 2 + 3];
						dist = min(dist, MinDistancePointToLine(x1, y1, x2, y2, x3, y3, 0));
					}

					MinDistance = dist - Thickness1 - Object2->x2 * 0.5;
				}
				else
				{
					NrPoints2 = ObjectToLineSegments(Object2, (double *) &LinePoints2, 1);
					dist = 1e9;

					for (cnt = 0; cnt < NrPoints1; cnt++)
					{
						x1 = LinePoints1[cnt * 2];
						y1 = LinePoints1[cnt * 2 + 1];
						x2 = LinePoints1[cnt * 2 + 2];
						y2 = LinePoints1[cnt * 2 + 3];

						for (cnt2 = 0; cnt2 < NrPoints2; cnt2++)
						{
							x3 = LinePoints2[cnt2 * 2];
							y3 = LinePoints2[cnt2 * 2 + 1];
							x4 = LinePoints2[cnt2 * 2 + 2];
							y4 = LinePoints2[cnt2 * 2 + 3];
							dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
						}
					}

					MinDistance = dist - Thickness1 - Thickness2;
				}

				break;

			case OBJECT_RECT:
				MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);
				NrPoints2 = ObjectToLineSegments(Object2, (double *) &LinePoints2, 1);
				dist = 1e9;

				for (cnt = 0; cnt < NrPoints1; cnt++)
				{
					x1 = LinePoints1[cnt * 2];
					y1 = LinePoints1[cnt * 2 + 1];
					x2 = LinePoints1[cnt * 2 + 2];
					y2 = LinePoints1[cnt * 2 + 3];

					for (cnt2 = 0; cnt2 < NrPoints2; cnt2++)
					{
						x3 = LinePoints2[cnt2 * 2];
						y3 = LinePoints2[cnt2 * 2 + 1];
						x4 = LinePoints2[cnt2 * 2 + 2];
						y4 = LinePoints2[cnt2 * 2 + 3];
						dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
					}
				}

				MinDistance = dist - Thickness1;

				if (Object2->Thickness != 0.0)
					MinDistance -= Thickness2;

				break;

			case OBJECT_POLYGON:
				MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);
				dist = 1e9;

				for (cnt = 0; cnt < NrPoints1; cnt++)
				{
					x1 = LinePoints1[cnt * 2];
					y1 = LinePoints1[cnt * 2 + 1];
					x2 = LinePoints1[cnt * 2 + 2];
					y2 = LinePoints1[cnt * 2 + 3];
					count = ObjectPolygon2->NrVertices;

					if ((ObjectPolygon2->NrSubPolygons > 0) && (ObjectPolygon2->NrVerticesMainPolygon > 0))
						count = ObjectPolygon2->NrVerticesMainPolygon;

					for (cnt2 = 0; cnt2 < count; cnt2++)
					{
						x3 = ObjectPolygon2->Points[cnt2].x;
						y3 = ObjectPolygon2->Points[cnt2].y;

						if (cnt2 < count - 1)
						{
							x4 = ObjectPolygon2->Points[cnt2 + 1].x;
							y4 = ObjectPolygon2->Points[cnt2 + 1].y;
						}
						else
						{
							x4 = ObjectPolygon2->Points[0].x;
							y4 = ObjectPolygon2->Points[0].y;
						}

						dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
					}
				}

				MinDistance = dist - Thickness1;
				break;
			}
		}

		break;

// *******************************************************************************************************
// *******************************************************************************************************
	case OBJECT_RECT:
		NrPoints1 = ObjectToLineSegments(Object1, (double *) &LinePoints1, 1);

		switch (Object2->ObjectType)
		{
		case OBJECT_RECT:
			MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);
			NrPoints2 = ObjectToLineSegments(Object2, (double *) &LinePoints2, 1);
			dist = 1e9;

			for (cnt = 0; cnt < NrPoints1; cnt++)
			{
				x1 = LinePoints1[cnt * 2];
				y1 = LinePoints1[cnt * 2 + 1];
				x2 = LinePoints1[cnt * 2 + 2];
				y2 = LinePoints1[cnt * 2 + 3];

				for (cnt2 = 0; cnt2 < NrPoints2; cnt2++)
				{
					x3 = LinePoints2[cnt2 * 2];
					y3 = LinePoints2[cnt2 * 2 + 1];
					x4 = LinePoints2[cnt2 * 2 + 2];
					y4 = LinePoints2[cnt2 * 2 + 3];
					dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
				}
			}

			if (Object2->Thickness == 0.0)
				MinDistance = dist;
			else
				MinDistance = dist - Thickness2;

			if (Object1->Thickness != 0.0)
				MinDistance -= Thickness1;

			break;

		case OBJECT_POLYGON:
			MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);
			dist = 1e9;

			for (cnt = 0; cnt < NrPoints1; cnt++)
			{
				x1 = LinePoints1[cnt * 2];
				y1 = LinePoints1[cnt * 2 + 1];
				x2 = LinePoints1[cnt * 2 + 2];
				y2 = LinePoints1[cnt * 2 + 3];
				count = ObjectPolygon2->NrVertices;

				if ((ObjectPolygon2->NrSubPolygons > 0) && (ObjectPolygon2->NrVerticesMainPolygon > 0))
					count = ObjectPolygon2->NrVerticesMainPolygon;

				for (cnt2 = 0; cnt2 < count; cnt2++)
				{
					x3 = ObjectPolygon2->Points[cnt2].x;
					y3 = ObjectPolygon2->Points[cnt2].y;

					if (cnt2 < count - 1)
					{
						x4 = ObjectPolygon2->Points[cnt2 + 1].x;
						y4 = ObjectPolygon2->Points[cnt2 + 1].y;
					}
					else
					{
						x4 = ObjectPolygon2->Points[0].x;
						y4 = ObjectPolygon2->Points[0].y;
					}

					dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
				}
			}

			MinDistance = dist;

			if (Object1->Thickness != 0.0)
				MinDistance -= Thickness1;

			break;
		}

		break;

// *******************************************************************************************************
// *******************************************************************************************************
	case OBJECT_POLYGON:
		switch (Object2->ObjectType)
		{
		case OBJECT_POLYGON:
			MinCenterDistance = CalcLengthLine(x3, y3, x1, y1);
			count = ObjectPolygon1->NrVertices;

			if ((ObjectPolygon1->NrSubPolygons > 0) && (ObjectPolygon1->NrVerticesMainPolygon > 0))
				count = ObjectPolygon1->NrVerticesMainPolygon;

			count2 = ObjectPolygon2->NrVertices;

			if ((ObjectPolygon2->NrSubPolygons > 0) && (ObjectPolygon2->NrVerticesMainPolygon > 0))
				count2 = ObjectPolygon2->NrVerticesMainPolygon;

			PolygonObject1->NrVertices = count;
			PolygonObject2->NrVertices = count2;

			for (cnt = 0; cnt < count; cnt++)
			{
				PolygonObject1->Points[cnt].x = ObjectPolygon1->Points[cnt].x;
				PolygonObject1->Points[cnt].y = ObjectPolygon1->Points[cnt].y;
			}

			for (cnt = 0; cnt < count2; cnt++)
			{
				PolygonObject2->Points[cnt].x = ObjectPolygon2->Points[cnt].x;
				PolygonObject2->Points[cnt].y = ObjectPolygon2->Points[cnt].y;
			}

			if (CheckPolygonCompleetlyOutsidePolygon(PolygonObject1, PolygonObject2) == 1)
			{
				dist = 1e9;

				for (cnt = 0; cnt < count; cnt++)
				{
					x1 = ObjectPolygon1->Points[cnt].x;
					y1 = ObjectPolygon1->Points[cnt].y;

					if (cnt < count - 1)
					{
						x2 = ObjectPolygon1->Points[cnt + 1].x;
						y2 = ObjectPolygon1->Points[cnt + 1].y;
					}
					else
					{
						x2 = ObjectPolygon1->Points[0].x;
						y2 = ObjectPolygon1->Points[0].y;
					}

					for (cnt2 = 0; cnt2 < count2; cnt2++)
					{
						x3 = ObjectPolygon2->Points[cnt2].x;
						y3 = ObjectPolygon2->Points[cnt2].y;

						if (cnt2 < count2 - 1)
						{
							x4 = ObjectPolygon2->Points[cnt2 + 1].x;
							y4 = ObjectPolygon2->Points[cnt2 + 1].y;
						}
						else
						{
							x4 = ObjectPolygon2->Points[0].x;
							y4 = ObjectPolygon2->Points[0].y;
						}

						dist = min(dist, MinDistanceLineToLine(x1, y1, x2, y2, x3, y3, x4, y4, 0));
					}
				}

				MinDistance = dist;
			}
			else
				MinDistance = 0.0;

			break;
		}

		break;
	}

	if (MinDistance < 0.0)
		MinDistance = 0.0;

	if (Units == 0)
	{
		sprintf(str, "Minimum distance is \t%.1f thou\r\nMinimum center distance is \t%.1f thou",
		        MinDistance / 2540.0, MinCenterDistance / 2540.0);
	}
	else
	{
		sprintf(str, SC(3, "Minimum distance is \t%.4f mm\r\nMinimum center distance is \t%.4f mm"),
		        MinDistance / 100000.0, MinCenterDistance / 100000.0);
	}

	MessageBoxUTF8(GEOMWindow, str, SC(4, "Message"), MB_APPLMODAL | MB_OK);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetArcAngle(ObjectRecord * Object, double *Angle1, double *Angle2)
{
	double Length, x2, x3, y3, x4, y4;

	*Angle1 = 0.0;
	*Angle2 = 0.0;

	if ((Object->ObjectType != OBJECT_ARC) && (Object->ObjectType != OBJECT_CIRCLE))
		return -1;

	if (Object->ObjectType == OBJECT_ARC)
	{
		ConvertPointToPolar(Object->x3, Object->y3, &Length, Angle1);
		ConvertPointToPolar(Object->x4, Object->y4, &Length, Angle2);
	}
	else
	{
		x2 = Object->x2;
		x3 = 0.0;
		y3 = 0.0;
		x4 = 0.0;
		y4 = 0.0;

		switch (Object->Info2 & 15)
		{
		case 1:
			x3 = x2;
			y4 = x2;
			break;

		case 2:
			y3 = x2;
			x4 = -x2;
			break;

		case 3:
			y3 = -x2;
			y4 = x2;
			break;

		case 4:
			x3 = -x2;
			y4 = -x2;
			break;

		case 6:
			x3 = -x2;
			x4 = x2;
			break;

		case 8:
			y3 = -x2;
			x4 = x2;
			break;

		case 9:
			x3 = x2;
			x4 = -x2;
			break;

		case 12:
			y3 = x2;
			y4 = -x2;
			break;

		default:
			y3 = x2;
			y4 = x2;
			break;
		}

		ConvertPointToPolar(x3, y3, &Length, Angle1);
		ConvertPointToPolar(x4, y4, &Length, Angle2);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DimensionToLineSegments(double x1, double y1, double x2, double y2, double *LineSegments, int32 mode)
{
	double Angle, Length, Length2;
	double x1old, y1old, x2old, y2old, x3, y3, x4, y4;
	char str[200];
	int32 SegmentCount = 0;

	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

	if (Length > 0.0)
	{
		if ((mode & 12) == 0)
		{
			LineSegments[SegmentCount++] = x1;
			LineSegments[SegmentCount++] = y1;
			LineSegments[SegmentCount++] = x2;
			LineSegments[SegmentCount++] = y2;

			if ((mode & 1) == 1)
			{
				x3 = x1 + cos(Angle) * ArrowLength;
				y3 = y1 + sin(Angle) * ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x1, y1, 30.0);
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x1, y1, -30.0);
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}

			if ((mode & 2) == 2)
			{
				x3 = x2 - cos(Angle) * ArrowLength;
				y3 = y2 - sin(Angle) * ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2, y2, 30.0);
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2, y2, -30.0);
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}
		}
		else
		{
			if ((mode & 8) == 0)
			{
				GetUnitsValue(Units, Length, str, 6);
				GetMinMaxText(0.0, 0.0, DimensionHeight, 0, 0.0, 0, 0, str);

				if (Length < (TextMaxX - TextMinX) + DimensionHeight * 2.0)
				{
					x1old = x1;
					y1old = y1;
					x2old = x2;
					y2old = y2;
					x1 -= cos(Angle) * ArrowLength;
					y1 -= sin(Angle) * ArrowLength;
					Length2 = Length + (TextMaxX - TextMinX) + DimensionHeight * 2.0;
					x2 = x1old + (cos(Angle) * Length2);
					y2 = y1old + (sin(Angle) * Length2);
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;


					x3 = x1old - (cos(Angle) * ArrowLength);
					y3 = y1old - (sin(Angle) * ArrowLength);
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1old, y1old, 30.0);
					LineSegments[SegmentCount++] = x1old;
					LineSegments[SegmentCount++] = y1old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1old, y1old, -30.0);
					LineSegments[SegmentCount++] = x1old;
					LineSegments[SegmentCount++] = y1old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;

					x3 = x2old + (cos(Angle) * ArrowLength);
					y3 = y2old + (sin(Angle) * ArrowLength);
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, 30.0);
					LineSegments[SegmentCount++] = x2old;
					LineSegments[SegmentCount++] = y2old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, -30.0);
					LineSegments[SegmentCount++] = x2old;
					LineSegments[SegmentCount++] = y2old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
				}
				else
				{
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					x3 = x1 + (cos(Angle) * ArrowLength);
					y3 = y1 + (sin(Angle) * ArrowLength);
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1, y1, 30.0);
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1, y1, -30.0);
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x3 = x2 - (cos(Angle) * ArrowLength);
					y3 = y2 - (sin(Angle) * ArrowLength);
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2, y2, 30.0);
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2, y2, -30.0);
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
				}
			}
			else
			{
				GetUnitsValue(Units, Length, str, 6);
				GetMinMaxText(0.0, 0.0, DimensionHeight, 0, 0.0, 0, 0, str);
				x2old = x2;
				y2old = y2;
				Length2 = Length + (TextMaxX - TextMinX) + DimensionHeight * 2.0;
				x2 = x1 + (cos(Angle) * Length2);
				y2 = y1 + (sin(Angle) * Length2);
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				x3 = x2old + cos(Angle) * ArrowLength;
				y3 = y2old + sin(Angle) * ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, 30.0);
				LineSegments[SegmentCount++] = x2old;
				LineSegments[SegmentCount++] = y2old;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, -30.0);
				LineSegments[SegmentCount++] = x2old;
				LineSegments[SegmentCount++] = y2old;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}
		}
	}

	return SegmentCount / 4;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDimensionTextFromLine(double x1, double y1, double x2, double y2, ObjectRecord * ObjectText2, int32 mode)
{
	double Angle, Length, Angle2;
	int32 Quadrant;
	double cx, cy, tx, ty, x3, y3, dx, dy, TextAngle;
	char str[200];


	x3 = 0.0;
	y3 = 0.0;
	ObjectText2->Text[0] = 0;
	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

	if (((mode & 12) != 0) && (Length > 0.0))
	{
		Quadrant = (int32) ((Angle + ANGLE_CONVERT(22.5)) / ANGLE_CONVERT(45.0));
		GetUnitsValue(Units, Length, str, 6);
		GetMinMaxText(0.0, 0.0, ObjectText2->x2, 0, 0.0, 0, 0, str);
		dx = TextMaxX - TextMinX;
		dy = TextMaxY - TextMinY;
		TextAngle = Angle;

		if ((mode & 8) == 0)
		{
			if ((mode & 4) == 4)
			{
				if (Length < (TextMaxX - TextMinX) + ObjectText2->x2 * 2.0)
				{
					cx = x1 + (cos(Angle) * (Length + dx * 0.5 + ObjectText2->x2 * 2.0));
					cy = y1 + (sin(Angle) * (Length + dx * 0.5 + ObjectText2->x2 * 2.0));
					Angle2 = atan(ObjectText2->x2 * 0.6 / max(Length + dx * 0.5, 1000000.0)) * 180 / PI;
				}
				else
				{
					cx = (x1 + x2) * 0.5;
					cy = (y1 + y2) * 0.5;
					Angle2 = atan(ObjectText2->x2 * 0.7 / (Length * 0.5)) * 180 / PI;
				}

				tx = cx;
				ty = cy;

				switch (Quadrant)
				{
				case 8:
				case 0:
				case 1:
				case 2:
				case 7:
					RotatePointFromOtherPoint2(&tx, &ty, x1, y1, Angle2);
					x3 = tx - dx * 0.5;
					y3 = ty - dy * 0.5;
					RotatePointFromOtherPoint2(&x3, &y3, tx, ty, (Angle * 180 / PI));
					break;

				case 3:
				case 4:
				case 5:
				case 6:
					RotatePointFromOtherPoint2(&tx, &ty, x1, y1, -Angle2);
					x3 = tx + dx * 0.5;
					y3 = ty + dy * 0.5;
					RotatePointFromOtherPoint2(&x3, &y3, tx, ty, (Angle * 180 / PI));
					TextAngle += ANGLE_CONVERT(180.0);
					break;
				}
			}
		}
		else
		{
			Angle2 = atan(ObjectText2->x2 * 0.6 / max(Length + dx * 0.5, 1000000.0)) * 180.0 / PI;
			tx = x1 + (cos(Angle) * (Length + dx * 0.5 + ObjectText2->x2 * 2.0));
			ty = y1 + (sin(Angle) * (Length + dx * 0.5 + ObjectText2->x2 * 2.0));

			switch (Quadrant)
			{
			case 8:
			case 0:
			case 1:
			case 2:
			case 7:
				RotatePointFromOtherPoint2(&tx, &ty, x1, y1, Angle2);
				x3 = tx - dx * 0.5;
				y3 = ty - dy * 0.5;
				RotatePointFromOtherPoint2(&x3, &y3, tx, ty, (Angle * 180 / PI));
				break;

			case 3:
			case 4:
			case 5:
			case 6:
				RotatePointFromOtherPoint2(&tx, &ty, x1, y1, -Angle2);
				x3 = tx + dx * 0.5;
				y3 = ty + dy * 0.5;
				RotatePointFromOtherPoint2(&x3, &y3, tx, ty, (Angle * 180 / PI));
				TextAngle += ANGLE_CONVERT(180.0);
				break;
			}
		}

		ObjectText2->x1 = (float) x3;
		ObjectText2->y1 = (float) y3;
		strcpy(ObjectText2->Text, str);
		ObjectText2->RotationAngle = (float) (TextAngle * (180 / PI));
		return 0;
	}

	return -1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
