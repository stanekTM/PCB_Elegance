/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: ellipss.c
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



#include  "ellipss.h"
#include  "windows.h"
#include  "math.h"
#include  "line2.h"
#include  "owntypes.h"

int32 ok;



extern COLORREF LineColor;

extern int32 Printing, OperatingSystem, DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY;
extern HDC OutputDisplay;

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void ellips2(int32 xm, int32 ym, int32 a, int32 b, int32 EllipssType)
/*
              /-------------\
             /       |       \
            /        |        \
           /\    128 |   1    /\
          /   \      |      /   \
         /      \    |    /      \
        /    64   \  |  /    2    \
       /            \|/            \
       -----------------------------
       \            /|\            /
        \    32   /  |  \    4    /
         \      /    |    \      /
          \   /      |      \   /
           \/    16  |   8    \/
            \        |        /
             \       |       /
              \-------------/

*/
{
	int32 x3, y3, x4, y4;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	if ((xm + a < DrawWindowMinX - 100) || (ym + b < DrawWindowMinY - 100) || (xm - a > DrawWindowMaxX + 100)
	        || (ym - b > DrawWindowMaxY + 100) || (a > 200000) || (b > 200000) || (a <= 0) || (b <= 0))
		return;

	if ((a == b) && (EllipssType == 255) && (a < 3))
	{
		SetPixel(OutputDisplay, xm, ym, LineColor);
		return;
	}

	a = ((a - 1) | 1) >> 1;
	b = ((b - 1) | 1) >> 1;
	x3 = xm - a;
	x4 = xm + a + 1;
	y3 = ym - b;
	y4 = ym + b + 1;

	switch (EllipssType)
	{
	case 0xff:
		Ellipse(OutputDisplay, x3, y3, x4, y4);
		break;

	case 0x0f:
		Arc(OutputDisplay, x3, y3, x4, y4, xm, y4, xm, y3);
		break;

	case 0x3c:
		Arc(OutputDisplay, x3, y3, x4, y4, x3, ym, x4, ym);
		break;

	case 0xf0:
		Arc(OutputDisplay, x3, y3, x4, y4, xm, y3, xm, y4);
		break;

	case 0xc3:
		Arc(OutputDisplay, x3, y3, x4, y4, x4, ym, x3, ym);
		break;

	case 0x03:
		Arc(OutputDisplay, x3, y3, x4, y4, x4, ym, xm, y3);
		break;

	case 0x0c:
		Arc(OutputDisplay, x3, y3, x4, y4, xm, y4, x4, ym);
		break;

	case 0x30:
		Arc(OutputDisplay, x3, y3, x4, y4, x3, ym, xm, y4);
		break;

	case 0xc0:
		Arc(OutputDisplay, x3, y3, x4, y4, xm, y3, x3, ym);
		break;

	case 0x1e:
		Arc(OutputDisplay, x3, y3, x4, y4, x3, y4, x4, y3);
		break;

	case 0x78:
		Arc(OutputDisplay, x3, y3, x4, y4, x3, y3, x4, y4);
		break;

	case 0xe1:
		Arc(OutputDisplay, x3, y3, x4, y4, x4, y3, x3, y4);
		break;

	case 0x87:
		Arc(OutputDisplay, x3, y3, x4, y4, x4, y4, x3, y3);
		break;
	}

	return;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SpecialArc(int32 xm, int32 ym, int32 a, int32 b, int32 x1, int32 y1, int32 x2, int32 y2)
{
#ifdef _DEBUG
	int32 ok;

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	if ((xm + a < DrawWindowMinX - 100) || (ym + b < DrawWindowMinY - 100) || (xm - a > DrawWindowMaxX + 100)
	        || (ym - b > DrawWindowMaxY + 100) || (a <= 0) || (b <= 0))
		return;

	if (a < 3)
	{
		SetPixel(OutputDisplay, xm, ym, LineColor);
		return;
	}

	a = ((a - 1) | 1) >> 1;
	b = ((b - 1) | 1) >> 1;
	Arc(OutputDisplay, xm - a, ym - b, xm + a + 1, ym + b + 1, x1, y1, x2, y2);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
