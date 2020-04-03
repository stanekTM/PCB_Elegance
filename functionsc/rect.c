/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: rect.c
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



#include  "owntypes.h"
#include  "rect.h"
#include  "math.h"
#include  "windows.h"


int32 ok;

extern int32 DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY, Printing;
extern HDC OutputDisplay;
extern COLORREF LineColor;

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

void RectangleXor(HDC Display, int32 x1, int32 y1, int32 x2, int32 y2)
{
	Rectangle(Display, x1, y1, x2, y2);
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

void RectangleXor2(HDC Display, int32 x, int32 y, int32 width, int32 height)
{
	int32 x1, y1, x2, y2;

	width = (width | 1) >> 1;
	height = (height | 1) >> 1;
	x1 = x - width;
	y1 = y - height;
	x2 = x + width + 1;
	y2 = y + height + 1;

	Rectangle(Display, x1, y1, x2, y2);
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

void rect2(int32 x, int32 y, int32 width, int32 height)
{
	int32 x1, y1, x2, y2;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	x1 = x;
	y1 = y;
	x2 = x + width;
	y2 = y + height;

	if ((x2 <= DrawWindowMinX - 1000) || (x1 >= DrawWindowMaxX + 1000) || (y2 <= DrawWindowMinY - 1000)
	        || (y1 >= DrawWindowMaxY + 1000))
		return;

	if (x1 < DrawWindowMinX - 1000)
		x1 = DrawWindowMinX - 1000;

	if (y1 < DrawWindowMinY - 1000)
		y1 = DrawWindowMinY - 1000;

	if (x2 > DrawWindowMaxX + 1000)
		x2 = DrawWindowMaxX + 1000;

	if (y2 > DrawWindowMaxY + 1000)
		y2 = DrawWindowMaxY + 1000;

	if (x2 - x1 <= 1)
	{
		MoveToEx(OutputDisplay, x1, y1, NULL);
		LineTo(OutputDisplay, x1, y2);
	}
	else if (y2 - y1 <= 1)
	{
		MoveToEx(OutputDisplay, x1, y1, NULL);
		LineTo(OutputDisplay, x2, y1);
	}
	else
		Rectangle(OutputDisplay, x1, y1, x2, y2);
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

void LineFunction(int32 x, int32 y, int32 mode)
{
	if (mode == 0)
		MoveToEx(OutputDisplay, x, y, NULL);
	else
		LineTo(OutputDisplay, x, y);
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

void rect3(int32 x, int32 y, int32 width, int32 height)
{
	int32 x1, y1, x2, y2;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif
	width = (width | 1) >> 1;
	height = (height | 1) >> 1;
	x1 = x - width;
	y1 = y - height;
	x2 = x + width + 1;
	y2 = y + height + 1;

	if ((x2 <= DrawWindowMinX - 1000) || (x1 >= DrawWindowMaxX + 1000) || (y2 <= DrawWindowMinY - 1000)
	        || (y1 >= DrawWindowMaxY + 1000))
		return;

	if (x1 < DrawWindowMinX - 1000)
		x1 = DrawWindowMinX - 1000;

	if (y1 < DrawWindowMinY - 1000)
		y1 = DrawWindowMinY - 1000;

	if (x2 > DrawWindowMaxX + 1000)
		x2 = DrawWindowMaxX + 1000;

	if (y2 > DrawWindowMaxY + 1000)
		y2 = DrawWindowMaxY + 1000;

	if (abs(x1 - x2) == 1)
	{
		LineFunction(x1, y1, 0);
		LineFunction(x1, y2, 1);
	}
	else
	{
		if (abs(y1 - y2) == 1)
		{
			LineFunction(x1, y1, 0);
			LineFunction(x2, y1, 1);
		}
		else
			Rectangle(OutputDisplay, x1, y1, x2, y2);
	}
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

void rect4(int32 x, int32 y, int32 width, int32 height)
{
	int32 x1, y1, x2, y2;
	POINT Points[4];
#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	width = (width | 1) >> 1;
	height = (height | 1) >> 1;
	x1 = x - width;
	y1 = y - height;
	x2 = x + width;
	y2 = y + height;

	if ((x2 <= DrawWindowMinX - 1000) || (x1 >= DrawWindowMaxX + 1000) || (y2 <= DrawWindowMinY - 1000)
	        || (y1 >= DrawWindowMaxY + 1000))
		return;

	if (x1 < DrawWindowMinX - 1000)
		x1 = DrawWindowMinX - 1000;

	if (y1 < DrawWindowMinY - 1000)
		y1 = DrawWindowMinY - 1000;

	if (x2 > DrawWindowMaxX + 1000)
		x2 = DrawWindowMaxX + 1000;

	if (y2 > DrawWindowMaxY + 1000)
		y2 = DrawWindowMaxY + 1000;

	Points[0].x = x1;
	Points[0].y = y1;
	Points[1].x = x1;
	Points[1].y = y2;
	Points[2].x = x2;
	Points[2].y = y2;
	Points[3].x = x2;
	Points[3].y = y1;
	Polygon(OutputDisplay, (CONST POINT *) & Points, 4);
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
