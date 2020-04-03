/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: line2.c
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
#include  "line2.h"
#include  "math.h"
#include  "rect.h"
#include  "windows.h"
#include  "ellipss.h"


const double EllipssPos2[65] = { 0.0,
                                 6.02634847446823E-0004,
                                 2.41344736827180E-0003,
                                 5.44119135783011E-0003,
                                 9.70055653526363E-0003,
                                 1.52123204933219E-0002,
                                 2.20035651989768E-0002,
                                 3.01079619387721E-0002,
                                 3.95661298965800E-0002,
                                 5.04260749907126E-0002,
                                 6.27437172258806E-0002,
                                 7.65835166847687E-0002,
                                 9.20192104555731E-0002,
                                 1.09134675340317E-0001,
                                 1.28024934205407E-0001,
                                 1.48797327433430E-0001,
                                 1.71572875253810E-0001,
                                 1.96487861945043E-0001,
                                 2.23695679233926E-0001,
                                 2.53368973947236E-0001,
                                 2.85702154455406E-0001,
                                 3.20914322142442E-0001,
                                 3.59252708629945E-0001,
                                 4.00996717537665E-0001,
                                 4.46462692171690E-0001,
                                 4.96009558996357E-0001,
                                 5.50045532785609E-0001,
                                 6.09036115246545E-0001,
                                 6.73513677715992E-0001,
                                 7.44088994364519E-0001,
                                 8.21465190789023E-0001,
                                 9.06454701582762E-0001,
                                 1.00000000000000E+0000,
                                 1.10319908789033E+0000,
                                 1.21733703535203E+0000,
                                 1.34392526643139E+0000,
                                 1.48475084187033E+0000,
                                 1.64193875365698E+0000,
                                 1.81803130903668E+0000,
                                 2.01609017782527E+0000,
                                 2.23982880884355E+0000,
                                 2.49378599939804E+0000,
                                 2.78355590919168E+0000,
                                 3.11609651237734E+0000,
                                 3.50014861423135E+0000,
                                 3.94681315719520E+0000,
                                 4.47035903163004E+0000,
                                 5.08937290121105E+0000,
                                 5.82842712474619E+0000,
                                 6.72055081397470E+0000,
                                 7.81097843327591E+0000,
                                 9.16299056080641E+0000,
                                 1.08672960249186E+0001,
                                 1.30576401200819E+0001,
                                 1.59378507396995E+0001,
                                 1.98310100515295E+0001,
                                 2.52741423690882E+0001,
                                 3.32138057711649E+0001,
                                 4.54471805344755E+0001,
                                 6.57361906383047E+0001,
                                 1.03086868919818E+0002,
                                 1.83783280946544E+0002,
                                 4.14345062231903E+0002,
                                 1.65937964629276E+0003,
                                 2.66491007064718E+0032
                               };


int32 NrPoints, ok;

POINT EllipssPoints[300];

extern HDC OutputDisplay;

extern COLORREF LineColor;
extern HPEN EmptyPen;
extern int32 Printing, ReverseY, OperatingSystem, DrawWindowMinX, DrawWindowMaxX, DrawWindowMinY, DrawWindowMaxY;

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void GetPoint(int32 a, int32 NrEllipssPoints, int32 pos, int32 * x1, int32 * y1)
{

	int32 pos2, pos3 = 0;
	double a2, a2_2, cx, cy;

	a2 = a;
	a2_2 = a2 * a2;
	pos2 = pos;

	if (pos2 < 0)
		pos2 += NrEllipssPoints;

	if (pos2 >= NrEllipssPoints)
		pos2 -= NrEllipssPoints;

	switch (NrEllipssPoints)
	{
	case 256:
		pos3 = pos2 & 0x3f;
		break;

	case 128:
		pos3 = (pos2 * 2) & 0x3f;
		break;

	case 64:
		pos3 = (pos2 * 4) & 0x3f;
		break;

	case 32:
		pos3 = (pos2 * 8) & 0x3f;
		break;

	case 16:
		pos3 = (pos2 * 16) & 0x3f;
		break;

	case 8:
		pos3 = (pos2 * 32) & 0x3f;
		break;

	case 4:
		pos3 = (pos2 * 64) & 0x3f;
		break;
	}

	if (pos2 >= (NrEllipssPoints * 3) / 4)
	{
		cx = a2_2 * sqrt(1 / (a2_2 + a2_2 * EllipssPos2[64 - pos3]));
		cy = sqrt((a2_2 * (1 - (cx * cx / (a2_2)))));
		*x1 = (int32) (cx);
		*y1 = (int32) (-cy);
		return;
	}

	if (pos2 >= (NrEllipssPoints * 2) / 4)
	{
		cx = a2_2 * sqrt(1 / (a2_2 + a2_2 * EllipssPos2[pos3]));
		cy = sqrt((a2_2 * (1 - (cx * cx / (a2_2)))));
		*x1 = (int32) (-cx);
		*y1 = (int32) (-cy);
		return;
	}

	if (pos2 >= (NrEllipssPoints * 1) / 4)
	{
		cx = a2_2 * sqrt(1 / (a2_2 + a2_2 * EllipssPos2[64 - pos3]));
		cy = sqrt((a2_2 * (1 - (cx * cx / (a2_2)))));
		*x1 = (int32) (-cx);
		*y1 = (int32) (cy);
		return;
	}

	cx = a2_2 * sqrt(1 / (a2_2 + a2_2 * EllipssPos2[pos3]));
	cy = sqrt((a2_2 * (1 - (cx * cx / (a2_2)))));
	*x1 = (int32) (cx);
	*y1 = (int32) (cy);
	return;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 GetCircleLinePoints(int32 xm, int32 ym, int32 xm2, int32 ym2, int32 a, int32 mode, POINT * LinePoints)
{
	int32 x1, y1, x2, y2, cnt, NrPoints, start;

	a = ((a - 1) | 1) >> 1;

	NrPoints = 256;

	if (a < 256)
		NrPoints = 128;

	if (a < 128)
		NrPoints = 64;

	if (a < 64)
		NrPoints = 32;

	if (a < 32)
		NrPoints = 16;

	switch (mode)
	{
	case 0:					// hor line
		start = (NrPoints * 2) / 8;

		for (cnt = start; cnt <= start + NrPoints; cnt++)
		{
			GetPoint(a, NrPoints, cnt, &x1, &y1);

			if (cnt == (start + NrPoints / 2))
			{
				x2 = x1;
				y2 = y1;
				x2 += xm;
				y2 += ym;
				LinePoints->x = x2;
				LinePoints->y = y2;
				LinePoints++;
			}

			if (cnt < (start + NrPoints / 2))
			{
				x1 += xm;
				y1 += ym;
			}
			else
			{
				x1 += xm + xm2;
				y1 += ym;
			}

			LinePoints->x = x1;
			LinePoints->y = y1;
			LinePoints++;
		}

		break;

	case 1:					// ver line
		start = (NrPoints * 4) / 8;

		for (cnt = start; cnt <= start + NrPoints; cnt++)
		{
			GetPoint(a, NrPoints, cnt, &x1, &y1);

			if (cnt == NrPoints)
			{
				x2 = x1;
				y2 = y1;
				x2 += xm;
				y2 += ym;
				LinePoints->x = x2;
				LinePoints->y = y2;
				LinePoints++;
			}

			if (cnt < NrPoints)
			{
				x1 += xm;
				y1 += ym;
			}
			else
			{
				x1 += xm;
				y1 += ym + xm2;
			}

			LinePoints->x = x1;
			LinePoints->y = y1;
			LinePoints++;
		}

		break;

	case 2:					// diag1 line
		start = (NrPoints * 3) / 8;

		for (cnt = start; cnt <= start + NrPoints; cnt++)
		{
			GetPoint(a, NrPoints, cnt, &x1, &y1);

			if (cnt == (start + NrPoints / 2))
			{
				x2 = x1;
				y2 = y1;
				x2 += xm;
				y2 += ym;
				LinePoints->x = x2;
				LinePoints->y = y2;
				LinePoints++;
			}

			if (cnt < (start + NrPoints / 2))
			{
				x1 += xm;
				y1 += ym;
			}
			else
			{
				x1 += xm + xm2;
				y1 += ym + xm2;
			}

			LinePoints->x = x1;
			LinePoints->y = y1;
			LinePoints++;
		}

		break;

	case 3:					// diag2 line
		start = (NrPoints * 1) / 8;

		for (cnt = start; cnt <= start + NrPoints; cnt++)
		{
			GetPoint(a, NrPoints, cnt, &x1, &y1);

			if (cnt == (start + NrPoints / 2))
			{
				x2 = x1;
				y2 = y1;
				x2 += xm;
				y2 += ym;
				LinePoints->x = x2;
				LinePoints->y = y2;
				LinePoints++;
			}

			if (cnt < (start + NrPoints / 2))
			{
				x1 += xm;
				y1 += ym;
			}
			else
			{
				x1 += xm + xm2;
				y1 += ym - xm2;
			}

			LinePoints->x = x1;
			LinePoints->y = y1;
			LinePoints++;
		}

		break;

	case 4:					// all angle line
		break;
	}

	return NrPoints;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawLine(int32 x1, int32 y1, int32 x2, int32 y2)
{
	int32 minx2, miny2, maxx2, maxy2;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	if (x1 > x2)
	{
		minx2 = x2;
		maxx2 = x1;
	}
	else
	{
		minx2 = x1;
		maxx2 = x2;
	}

	if (y1 > y2)
	{
		miny2 = y2;
		maxy2 = y1;
	}
	else
	{
		miny2 = y1;
		maxy2 = y2;
	}

	if ((maxx2 < DrawWindowMinX - 1000) || (minx2 >= DrawWindowMaxX + 1000) || (maxy2 < DrawWindowMinY - 1000)
	        || (miny2 >= DrawWindowMaxY + 1000))
		return;

	MoveToEx(OutputDisplay, x1, y1, NULL);
	LineTo(OutputDisplay, x2, y2);
	SetPixel(OutputDisplay, x2, y2, LineColor);
	return;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void DrawHorLine(int32 x1, int32 y1, int32 x2, int32 Height, int32 mode)
/*
            ____________________________________
           /                                    \
      y1  / .                                  . \
          \                                      /
           \------------------------------------/

            x1                                 x2

*/
{
	int32 hulp, OldHeight;
	HGDIOBJ SavePen = NULL;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	OldHeight = Height;

	if (Height < 1)
		Height = 1;

	Height = (Height - 1) >> 1;

	if (ReverseY)
		y1 = DrawWindowMaxY - y1 - 1;

	if (x1 > x2)
	{
		hulp = x2;
		x2 = x1;
		x1 = hulp;
	}

	if ((x1 - Height >= DrawWindowMaxX) || (x2 + Height < DrawWindowMinX) || (y1 - Height >= DrawWindowMaxY)
	        || (y1 + Height < DrawWindowMinY))
		return;

	if (x1 + Height < DrawWindowMinX - 1)
		x1 = DrawWindowMinX - Height - 1;

	if (x2 - Height >= DrawWindowMaxX + 1)
		x2 = DrawWindowMaxX + Height + 1;

	switch (Height)
	{
	case 0:
		MoveToEx(OutputDisplay, x1, y1, NULL);
		LineTo(OutputDisplay, (x2 + 1), y1);
		break;

	case 1:
		MoveToEx(OutputDisplay, x1, y1 - 1, NULL);
		LineTo(OutputDisplay, x2 + 1, y1 - 1);
		MoveToEx(OutputDisplay, x1 - 1, y1, NULL);
		LineTo(OutputDisplay, x2 + 2, y1);
		MoveToEx(OutputDisplay, x1, y1 + 1, NULL);
		LineTo(OutputDisplay, x2 + 1, y1 + 1);
		break;

	case 2:
		MoveToEx(OutputDisplay, x1, y1 - 2, NULL);
		LineTo(OutputDisplay, x2 + 1, y1 - 2);
		MoveToEx(OutputDisplay, x1 - 1, y1 - 1, NULL);
		LineTo(OutputDisplay, x2 + 2, y1 - 1);
		MoveToEx(OutputDisplay, x1 - 1, y1, NULL);
		LineTo(OutputDisplay, x2 + 2, y1);
		MoveToEx(OutputDisplay, x1 - 1, y1 + 1, NULL);
		LineTo(OutputDisplay, x2 + 2, y1 + 1);
		MoveToEx(OutputDisplay, x1, y1 + 2, NULL);
		LineTo(OutputDisplay, x2 + 1, y1 + 2);
		break;

	case 3:
		MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
		LineTo(OutputDisplay, x1 - 2, y1 + 2);
		MoveToEx(OutputDisplay, x1 - 1, y1 - 2, NULL);
		LineTo(OutputDisplay, x1 - 1, y1 + 3);

		if (mode == 0)
			Rectangle(OutputDisplay, x1, y1 - 3, x2 + 1, y1 + 4);
		else
			RectangleXor(OutputDisplay, x1, y1 - 3, x2 + 1, y1 + 4);

		MoveToEx(OutputDisplay, x2 + 1, y1 - 2, NULL);
		LineTo(OutputDisplay, x2 + 1, y1 + 3);
		MoveToEx(OutputDisplay, x2 + 2, y1 - 1, NULL);
		LineTo(OutputDisplay, x2 + 2, y1 + 2);
		break;

	case 4:
		MoveToEx(OutputDisplay, x1 - 4, y1 - 2, NULL);
		LineTo(OutputDisplay, x1 - 4, y1 + 3);
		MoveToEx(OutputDisplay, x1 - 3, y1 - 3, NULL);
		LineTo(OutputDisplay, x1 - 3, y1 + 4);

		if (mode == 0)
			Rectangle(OutputDisplay, x1 - 2, y1 - 4, x2 + 3, y1 + 5);
		else
			RectangleXor(OutputDisplay, x1 - 2, y1 - 4, x2 + 3, y1 + 5);

		MoveToEx(OutputDisplay, x2 + 3, y1 - 3, NULL);
		LineTo(OutputDisplay, x2 + 3, y1 + 4);
		MoveToEx(OutputDisplay, x2 + 4, y1 - 2, NULL);
		LineTo(OutputDisplay, x2 + 4, y1 + 3);
		break;

	default:
		if (Height < 10000)
		{
			if ((mode & 1) == 0)
			{
				if (x2 > x1 + 1)
					Rectangle(OutputDisplay, (x1), (y1 - Height), (x2 + 1), (y1 + Height + 1));

				Ellipse(OutputDisplay, (x1 - Height), (y1 - Height), (x1 + Height + 1), (y1 + Height + 1));
				Ellipse(OutputDisplay, (x2 - Height), (y1 - Height), (x2 + Height + 1), (y1 + Height + 1));
			}
			else
			{
				NrPoints = GetCircleLinePoints(x1, y1, x2 - x1, 0, OldHeight, 0, (POINT *) & EllipssPoints);

				if ((mode & 2) == 0)
					SavePen = SelectObject(OutputDisplay, EmptyPen);

				Polygon(OutputDisplay, EllipssPoints, NrPoints + 2);

				if ((mode & 2) == 0)
					SelectObject(OutputDisplay, SavePen);
			}
		}

		break;
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void DrawVerLine(int32 x1, int32 y1, int32 y2, int32 Height, int32 mode)
/*      _
       / \
      /   \
     |  .  |  y2
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |     |
     |  .  |  y1
      \   /
       \_/

        x1
*/
{
	int32 hulp, OldHeight;
	HGDIOBJ SavePen = NULL;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif
	OldHeight = Height;

	if (Height < 1)
		Height = 1;

	Height = (Height - 1) >> 1;

	if (ReverseY)
	{
		y1 = DrawWindowMaxY - y1 - 1;
		y2 = DrawWindowMaxY - y2 - 1;
	}

	if (y1 > y2)
	{
		hulp = y2;
		y2 = y1;
		y1 = hulp;
	}

	if ((x1 - Height >= DrawWindowMaxX) || (x1 + Height < DrawWindowMinX) || (y1 - Height >= DrawWindowMaxY)
	        || (y2 + Height < DrawWindowMinY))
		return;

	if (y1 + Height < DrawWindowMinY - 1)
		y1 = DrawWindowMinY - Height - 1;

	if (y2 - Height >= DrawWindowMaxY + 1)
		y2 = DrawWindowMaxY + Height + 1;

	switch (Height)
	{
	case 0:
		MoveToEx(OutputDisplay, x1, y1, NULL);
		LineTo(OutputDisplay, x1, (y2 + 1));
		break;

	case 1:
		MoveToEx(OutputDisplay, x1 - 1, y1, NULL);
		LineTo(OutputDisplay, x1 - 1, y2 + 1);
		MoveToEx(OutputDisplay, x1, y1 - 1, NULL);
		LineTo(OutputDisplay, x1, y2 + 2);
		MoveToEx(OutputDisplay, x1 + 1, y1, NULL);
		LineTo(OutputDisplay, x1 + 1, y2 + 1);
		break;

	case 2:
		MoveToEx(OutputDisplay, x1 - 2, y1, NULL);
		LineTo(OutputDisplay, x1 - 2, y2 + 1);
		MoveToEx(OutputDisplay, x1 - 1, y1 - 1, NULL);
		LineTo(OutputDisplay, x1 - 1, y2 + 2);
		MoveToEx(OutputDisplay, x1, y1 - 1, NULL);
		LineTo(OutputDisplay, x1, y2 + 2);
		MoveToEx(OutputDisplay, x1 + 1, y1 - 1, NULL);
		LineTo(OutputDisplay, x1 + 1, y2 + 2);
		MoveToEx(OutputDisplay, x1 + 2, y1, NULL);
		LineTo(OutputDisplay, x1 + 2, y2 + 1);
		break;

	case 3:
		MoveToEx(OutputDisplay, x1 - 1, y1 - 2, NULL);
		LineTo(OutputDisplay, x1 + 2, y1 - 2);
		MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
		LineTo(OutputDisplay, x1 + 3, y1 - 1);

		if (mode == 0)
			Rectangle(OutputDisplay, x1 - 3, y1, x1 + 4, y2 + 1);
		else
			RectangleXor(OutputDisplay, x1 - 3, y1, x1 + 4, y2 + 1);

		MoveToEx(OutputDisplay, x1 - 2, y2 + 1, NULL);
		LineTo(OutputDisplay, x1 + 3, y2 + 1);
		MoveToEx(OutputDisplay, x1 - 1, y2 + 2, NULL);
		LineTo(OutputDisplay, x1 + 2, y2 + 2);
		break;

	case 4:
		MoveToEx(OutputDisplay, x1 - 2, y1 - 4, NULL);
		LineTo(OutputDisplay, x1 + 3, y1 - 4);
		MoveToEx(OutputDisplay, x1 - 3, y1 - 3, NULL);
		LineTo(OutputDisplay, x1 + 4, y1 - 3);

		if (mode == 0)
			Rectangle(OutputDisplay, x1 - 4, y1 - 2, x1 + 5, y2 + 3);
		else
			RectangleXor(OutputDisplay, x1 - 4, y1 - 2, x1 + 5, y2 + 3);

		MoveToEx(OutputDisplay, x1 - 3, y2 + 3, NULL);
		LineTo(OutputDisplay, x1 + 4, y2 + 3);
		MoveToEx(OutputDisplay, x1 - 2, y2 + 4, NULL);
		LineTo(OutputDisplay, x1 + 3, y2 + 4);
		break;

	default:
		if (Height < 10000)
		{
			if ((mode & 1) == 0)
			{
				if (y2 > y1 + 1)
					Rectangle(OutputDisplay, (x1 - Height), (y1 + 1), (x1 + Height + 1), (y2));

				Ellipse(OutputDisplay, (x1 - Height), (y1 - Height), (x1 + Height + 1), (y1 + Height + 1));
				Ellipse(OutputDisplay, (x1 - Height), (y2 - Height), (x1 + Height + 1), (y2 + Height + 1));
			}
			else
			{
				NrPoints = GetCircleLinePoints(x1, y1, y2 - y1, 0, OldHeight, 1, (POINT *) & EllipssPoints);

				if ((mode & 2) == 0)
					SavePen = SelectObject(OutputDisplay, EmptyPen);

				Polygon(OutputDisplay, EllipssPoints, NrPoints + 2);

				if ((mode & 2) == 0)
					SelectObject(OutputDisplay, SavePen);
			}
		}

		break;
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void DrawDiag1Line(int32 x1, int32 y1, int32 x2, int32 Height, int32 mode)
/*            1
             /\
           /   \
         / x1   \
     4 /         \
       \          \
        \          \
         \          \
          \          \
           \          \
            \         /  2
             \      /
              \   / x2
               \/
               3
*/
{
	int32 hulp, Height2, Height2a, y2, x1a, y1a, x2a, y2a, x3a, y3a, x4a, y4a, x10, x20, x1b, y1b, x2b, y2b, x3b, y3b,
	      x4b, y4b, OldHeight, ok;
	double h2;
	POINT LinePoints[10];
	HGDIOBJ SavePen = NULL;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	if (Height < 1)
		Height = 1;

	OldHeight = (Height - 1) | 1;
	Height = (Height - 1) >> 1;

	if (x1 > x2)
	{
		hulp = x2;
		x2 = x1;
		x1 = hulp;
	}

	y2 = y1 + x1 - x2;

	if (ReverseY)
	{
		y1 = DrawWindowMaxY - 1 - y1;
		y2 = DrawWindowMaxY - 1 - y2;

		if ((x1 - Height >= DrawWindowMaxX) || (x2 + Height < DrawWindowMinX) || (y1 - Height >= DrawWindowMaxY)
		        || (y2 + Height < DrawWindowMinY))
			return;

		if (x1 + Height < DrawWindowMinX)
		{
			y1 = DrawWindowMinX - Height + y1 - x1;
			x1 = DrawWindowMinX - Height;

			if (y1 - Height >= DrawWindowMaxY)
			{
				x1 = Height + DrawWindowMaxY - y1 + x1;
				y1 = Height + DrawWindowMaxY;
			}
		}

		if (y1 - Height >= DrawWindowMaxY)
		{
			x1 = Height + DrawWindowMaxY - y1 + x1;
			y1 = Height + DrawWindowMaxY;
		}

		if (x2 - Height >= DrawWindowMaxX)
		{
			y2 = DrawWindowMaxX + Height + y2 - x2;
			x2 = DrawWindowMaxX + Height;

			if (y2 + Height < DrawWindowMinY)
			{
				x2 = DrawWindowMinY - Height - y2 + x2;
				y2 = DrawWindowMinY - Height;
			}
		}

		if (y2 + Height < DrawWindowMinY)
		{
			x2 = DrawWindowMinY - Height - y2 + x2;
			y2 = DrawWindowMinY - Height;
		}
	}
	else
	{
		if ((x1 - Height >= DrawWindowMaxX) || (x2 + Height < DrawWindowMinX) || (y2 - Height >= DrawWindowMaxY)
		        || (y1 + Height < DrawWindowMinY))
			return;

		if (x1 + Height < DrawWindowMinX)
		{
			y1 = -(DrawWindowMinX - Height) + y1 + x1;
			x1 = DrawWindowMinX - Height;

			if (y1 - Height >= DrawWindowMaxY)
			{
				x1 = -(Height + DrawWindowMaxY) + y1 + x1;
				y1 = Height + DrawWindowMaxY;
			}
		}

		if (y1 - Height >= DrawWindowMaxY)
		{
			x1 = -(Height + DrawWindowMaxY) + y1 + x1;
			y1 = Height + DrawWindowMaxY;
		}

		if (x2 - Height >= DrawWindowMaxX)
		{
			y2 = -(DrawWindowMaxX + Height) + y2 + x2;
			x2 = DrawWindowMaxX + Height;

			if (y2 + Height < DrawWindowMinY)
			{
				x2 = -(DrawWindowMinY - Height) + y2 + x2;
				y2 = DrawWindowMinY - Height;
			}
		}

		if (y2 + Height < DrawWindowMinY)
		{
			x2 = -DrawWindowMinY + Height + y2 + x2;
			y2 = DrawWindowMinY - Height;
		}
	}

	/*
	  x1&=0xffff;
	  y1&=0xffff;
	  x2&=0xffff;
	  y2&=0xffff;
	*/
	switch (Height)
	{
	case 0:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 1);
		}
		else
		{
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 1);
		}

		break;

	case 1:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 + 1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2);
			MoveToEx(OutputDisplay, x1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 1);
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 1);
			MoveToEx(OutputDisplay, x1 - 1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 2);
			MoveToEx(OutputDisplay, x1 - 1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2, y2 + 2);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 + 1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2);
			MoveToEx(OutputDisplay, x1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 1);
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 1);
			MoveToEx(OutputDisplay, x1 - 1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 2);
			MoveToEx(OutputDisplay, x1 - 1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2, y2 - 2);
		}

		break;

	case 2:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 + 1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2);
			MoveToEx(OutputDisplay, x1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 1);
			MoveToEx(OutputDisplay, x1 - 1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 2);
			MoveToEx(OutputDisplay, x1 - 1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 2);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 1, NULL);
			LineTo(OutputDisplay, x2, y2 + 3);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 + 1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2);
			MoveToEx(OutputDisplay, x1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 1);
			MoveToEx(OutputDisplay, x1 - 1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 2);
			MoveToEx(OutputDisplay, x1 - 1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 2);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
			LineTo(OutputDisplay, x2, y2 - 3);
		}

		break;

	case 3:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 + 1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2);
			MoveToEx(OutputDisplay, x1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 1);
			MoveToEx(OutputDisplay, x1 - 1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 2);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 3);

			MoveToEx(OutputDisplay, x1 - 3, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 4);
			MoveToEx(OutputDisplay, x1 - 3, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 + 1, NULL);
			LineTo(OutputDisplay, x2, y2 + 4);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 + 1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2);
			MoveToEx(OutputDisplay, x1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 1);
			MoveToEx(OutputDisplay, x1 - 1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 2);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 3);

			MoveToEx(OutputDisplay, x1 - 3, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 4);
			MoveToEx(OutputDisplay, x1 - 3, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 - 1, NULL);
			LineTo(OutputDisplay, x2, y2 - 4);
		}

		break;

	case 4:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 + 2, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2 - 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2);
			MoveToEx(OutputDisplay, x1, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2 + 1);
			MoveToEx(OutputDisplay, x1 - 1, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2 + 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 2);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 3, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 4);

			MoveToEx(OutputDisplay, x1 - 4, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 5);
			MoveToEx(OutputDisplay, x1 - 4, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 5);
			MoveToEx(OutputDisplay, x1 - 4, y1 + 1, NULL);
			LineTo(OutputDisplay, x2, y2 + 5);
			MoveToEx(OutputDisplay, x1 - 4, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 - 1, y2 + 5);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 + 2, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2 + 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2);
			MoveToEx(OutputDisplay, x1, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2 - 1);
			MoveToEx(OutputDisplay, x1 - 1, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 5, y2 - 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 2);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 3, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 4);

			MoveToEx(OutputDisplay, x1 - 4, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 5);
			MoveToEx(OutputDisplay, x1 - 4, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 5);
			MoveToEx(OutputDisplay, x1 - 4, y1 - 1, NULL);
			LineTo(OutputDisplay, x2, y2 - 5);
			MoveToEx(OutputDisplay, x1 - 4, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 - 1, y2 - 5);
		}

		break;

	default:
		if (Height < 10000)
		{
			if ((mode & 1) == 0)
			{
				switch (Height)
				{
				case 5:
					Height2 = 4;
					Height2a = 4;
					//            if (mode==1) Height2a=3;
					break;

				case 6:
					Height2 = 4;
					Height2a = 4;
					break;

				case 7:
					Height2 = 5;
					Height2a = 5;
					break;

				case 8:
					Height2 = 6;
					Height2a = 5;
					break;

				case 9:
					Height2 = 7;
					Height2a = 6;
					OldHeight = 20;
					break;

				default:
					h2 = Height;
					h2 = h2 * 0.7071067812;
					Height2 = (int32) (h2 + 0.75);
					Height2a = (int32) (h2 + 0.25);

					if (Height == 11)
						Height2a--;

					break;
				}

				ellips2(x1, y1, OldHeight, OldHeight, 255);
				ellips2(x2, y2, OldHeight, OldHeight, 255);

				if (ReverseY)
				{
					x1b = x1 + Height2;
					y1b = y1 - Height2;
					x2b = x2 + Height2;
					y2b = y2 - Height2;
					x3b = x2 - Height2;
					y3b = y2 + Height2;
					x4b = x1 - Height2;
					y4b = y1 + Height2;
				}
				else
				{
					x1b = x1 + Height2;
					y1b = y1 + Height2;
					x2b = x2 + Height2;
					y2b = y2 + Height2;
					x3b = x2 - Height2;
					y3b = y2 - Height2;
					x4b = x1 - Height2;
					y4b = y1 - Height2;
				}

				x10 = DrawWindowMaxY - y1b + x1b;
				x20 = DrawWindowMinY - y3b + x3b;

				if ((x10 < DrawWindowMinX) || (x20 >= DrawWindowMaxX))
				{
					ok = 1;
					return;
				}

				x10 = -DrawWindowMaxY + y1b + x1b;
				x20 = -DrawWindowMinY + y3b + x3b;

				if ((x10 >= DrawWindowMaxX) || (x20 < DrawWindowMinX))
				{
					ok = 1;
					return;
				}

				if (Height2 > 4000)
				{
					x1a = (DrawWindowMaxX >> 1);
					y1a = -(DrawWindowMaxX >> 1);
					x2a = DrawWindowMaxX + (DrawWindowMaxY >> 1);
					y2a = (DrawWindowMaxY >> 1);
					x3a = (DrawWindowMaxX >> 1);
					y3a = DrawWindowMaxY + (DrawWindowMaxX >> 1);
					x4a = -(DrawWindowMaxY >> 1);
					y4a = (DrawWindowMaxY >> 1);
					x10 = DrawWindowMinY + x1b - y1b;
					x20 = DrawWindowMaxY + x4b - y4b;

					if (x10 < DrawWindowMaxX)
					{
						x1a = x1a - ((DrawWindowMaxX - x10) >> 1);
						y1a = y1a + ((DrawWindowMaxX - x10) >> 1);
						x2a = x2a - ((DrawWindowMaxX - x10) >> 1);
						y2a = y2a + ((DrawWindowMaxX - x10) >> 1);
					}

					if (x20 >= DrawWindowMinX)
					{
						x3a = x3a + ((x20 - DrawWindowMinX) >> 1);
						y3a = y3a - ((x20 - DrawWindowMinX) >> 1);
						x4a = x4a + ((x20 - DrawWindowMinX) >> 1);
						y4a = y4a - ((x20 - DrawWindowMinX) >> 1);
					}

					LinePoints[0].x = x1a;
					LinePoints[0].y = y1a;
					LinePoints[1].x = x2a;
					LinePoints[1].y = y2a;
					LinePoints[2].x = x3a;
					LinePoints[2].y = y3a;
					LinePoints[3].x = x4a;
					LinePoints[3].y = y4a;
				}
				else
				{
					LinePoints[0].x = x1b;
					LinePoints[0].y = y1b;
					LinePoints[1].x = x2b;
					LinePoints[1].y = y2b;
					LinePoints[2].x = x3b;
					LinePoints[2].y = y3b;
					LinePoints[3].x = x4b;
					LinePoints[3].y = y4b;
				}

				Polygon(OutputDisplay, LinePoints, 4);
			}
			else
			{
				NrPoints = GetCircleLinePoints(x1, y1, x2 - x1, 0, OldHeight, 2, (POINT *) & EllipssPoints);

				if ((mode & 2) == 0)
					SavePen = SelectObject(OutputDisplay, EmptyPen);

				Polygon(OutputDisplay, EllipssPoints, NrPoints + 2);

				if ((mode & 2) == 0)
					SelectObject(OutputDisplay, SavePen);
			}
		}

		break;
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void DrawDiag2Line(int32 x1, int32 y1, int32 x2, int32 Height, int32 mode)
/*                 1

                 / \
               /    \
             /       *  x2
           /          \
         /             \
       /              /  2
 4   /              /
   /              /
   \            /
    \         /
     *      /
  x1  \   /
       \/
         3
*/
{
	int32 hulp, Height2, Height2a, y2, x1a, y1a, x2a, y2a, x3a, y3a, x4a, y4a, x20, x10, x1b, y1b, x2b, y2b, x3b, y3b,
	      x4b, y4b, OldHeight, ok;
	double h2;
	POINT LinePoints[10];
	HGDIOBJ SavePen = NULL;

#ifdef _DEBUG

	if (OutputDisplay == NULL)
		ok = 1;

#endif

	if (Height < 1)
		Height = 1;

	OldHeight = (Height - 1) | 1;
	Height = (Height - 1) >> 1;

	if (x1 > x2)
	{
		hulp = x2;
		x2 = x1;
		x1 = hulp;
	}

	y2 = y1 + x2 - x1;

	if (ReverseY)
	{
		y1 = DrawWindowMaxY - y1 - 1;
		y2 = DrawWindowMaxY - y2 - 1;
	}

	if (ReverseY)
	{
		if ((x1 - Height >= DrawWindowMaxX) || (x2 + Height < DrawWindowMinX) || (y2 - Height >= DrawWindowMaxY)
		        || (y1 + Height < DrawWindowMinY))
			return;

		if (x1 + Height < DrawWindowMinX)
		{
			y1 = -(DrawWindowMinX - Height) + y1 + x1;
			x1 = DrawWindowMinX - Height;

			if (y1 - Height >= DrawWindowMaxY)
			{
				x1 = -(Height + DrawWindowMaxY) + y1 + x1;
				y1 = Height + DrawWindowMaxY;
			}
		}

		if (y1 - Height >= DrawWindowMaxY)
		{
			x1 = -(Height + DrawWindowMaxY) + y1 + x1;
			y1 = Height + DrawWindowMaxY;
		}

		if (x2 - Height >= DrawWindowMaxX)
		{
			y2 = -(DrawWindowMaxX + Height) + y2 + x2;
			x2 = DrawWindowMaxX + Height;

			if (y2 + Height < DrawWindowMinY)
			{
				x2 = -(DrawWindowMinY - Height) + y2 + x2;
				y2 = DrawWindowMinY - Height;
			}
		}

		if (y2 + Height < DrawWindowMinY)
		{
			x2 = -DrawWindowMinY + Height + y2 + x2;
			y2 = DrawWindowMinY - Height;
		}
	}
	else
	{
		if ((x1 - Height >= DrawWindowMaxX) || (x2 + Height < DrawWindowMinX) || (y1 - Height >= DrawWindowMaxY)
		        || (y2 + Height < DrawWindowMinY))
			return;

		if (x1 + Height < DrawWindowMinX)
		{
			y1 = DrawWindowMinX - Height + y1 - x1;
			x1 = DrawWindowMinX - Height;

			if (y1 - Height >= DrawWindowMaxY)
			{
				x1 = Height + DrawWindowMaxY - y1 + x1;
				y1 = Height + DrawWindowMaxY;
			}
		}

		if (y1 - Height >= DrawWindowMaxY)
		{
			x1 = Height + DrawWindowMaxY - y1 + x1;
			y1 = Height + DrawWindowMaxY;
		}

		if (x2 - Height >= DrawWindowMaxX)
		{
			y2 = DrawWindowMaxX + Height + y2 - x2;
			x2 = DrawWindowMaxX + Height;

			if (y2 + Height < DrawWindowMinY)
			{
				x2 = DrawWindowMinY - Height - y2 + x2;
				y2 = DrawWindowMinY - Height;
			}
		}

		if (y2 + Height < DrawWindowMinY)
		{
			x2 = DrawWindowMinY - Height - y2 + x2;
			y2 = DrawWindowMinY - Height;
		}
	}

	/*
	  x1&=0xffff;
	  y1&=0xffff;
	  x2&=0xffff;
	  y2&=0xffff;
	*/
	switch (Height)
	{
	case 0:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 1);
		}
		else
		{
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 1);
		}

		break;

	case 1:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 - 1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2, y2 - 2);
			MoveToEx(OutputDisplay, x1 - 1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 2);
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 1);
			MoveToEx(OutputDisplay, x1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 - 1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2, y2 + 2);
			MoveToEx(OutputDisplay, x1 - 1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 2);
			MoveToEx(OutputDisplay, x1, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 1);
			MoveToEx(OutputDisplay, x1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2);
		}

		break;

	case 2:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
			LineTo(OutputDisplay, x2, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 3);

			MoveToEx(OutputDisplay, x1 - 1, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 2);
			MoveToEx(OutputDisplay, x1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 - 2, y1 + 1, NULL);
			LineTo(OutputDisplay, x2, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 3);

			MoveToEx(OutputDisplay, x1 - 1, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 2);
			MoveToEx(OutputDisplay, x1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2);
		}

		break;

	case 3:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 - 3, y1 - 1, NULL);
			LineTo(OutputDisplay, x2, y2 - 4);
			MoveToEx(OutputDisplay, x1 - 3, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 - 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 4);

			MoveToEx(OutputDisplay, x1 - 2, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 1, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 2);
			MoveToEx(OutputDisplay, x1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 - 3, y1 + 1, NULL);
			LineTo(OutputDisplay, x2, y2 + 4);
			MoveToEx(OutputDisplay, x1 - 3, y1, NULL);
			LineTo(OutputDisplay, x2 + 1, y2 + 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 4);

			MoveToEx(OutputDisplay, x1 - 2, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 1, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 2);
			MoveToEx(OutputDisplay, x1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2);
		}

		break;

	case 4:
		if (ReverseY)
		{
			MoveToEx(OutputDisplay, x1 - 4, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 - 2 + 1, y2 - 4 - 1);
			MoveToEx(OutputDisplay, x1 - 4, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 - 1 + 1, y2 - 4 - 1);
			MoveToEx(OutputDisplay, x1 - 4, y1, NULL);
			LineTo(OutputDisplay, x2 + 0 + 1, y2 - 4 - 1);
			MoveToEx(OutputDisplay, x1 - 4, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 1 + 1, y2 - 4 - 1);

			MoveToEx(OutputDisplay, x1 - 3, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 - 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 4);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 3);
			MoveToEx(OutputDisplay, x1 - 1, y1 + 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 - 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 - 1 - 1);
			MoveToEx(OutputDisplay, x1, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 + 0 - 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 + 1 - 1);
			MoveToEx(OutputDisplay, x1 + 2, y1 + 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 + 2 - 1);
		}
		else
		{
			MoveToEx(OutputDisplay, x1 - 4, y1 + 2, NULL);
			LineTo(OutputDisplay, x2 - 2 + 1, y2 + 4 + 1);
			MoveToEx(OutputDisplay, x1 - 4, y1 + 1, NULL);
			LineTo(OutputDisplay, x2 - 1 + 1, y2 + 4 + 1);
			MoveToEx(OutputDisplay, x1 - 4, y1, NULL);
			LineTo(OutputDisplay, x2 + 0 + 1, y2 + 4 + 1);
			MoveToEx(OutputDisplay, x1 - 4, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 1 + 1, y2 + 4 + 1);

			MoveToEx(OutputDisplay, x1 - 3, y1 - 1, NULL);
			LineTo(OutputDisplay, x2 + 2, y2 + 4);
			MoveToEx(OutputDisplay, x1 - 3, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 4);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 2, NULL);
			LineTo(OutputDisplay, x2 + 3, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 2, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 3);
			MoveToEx(OutputDisplay, x1 - 1, y1 - 3, NULL);
			LineTo(OutputDisplay, x2 + 4, y2 + 2);

			MoveToEx(OutputDisplay, x1 - 1, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 + 1 + 1);
			MoveToEx(OutputDisplay, x1, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 - 0 + 1);
			MoveToEx(OutputDisplay, x1 + 1, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 - 1 + 1);
			MoveToEx(OutputDisplay, x1 + 2, y1 - 4, NULL);
			LineTo(OutputDisplay, x2 + 4 + 1, y2 - 2 + 1);
		}

		break;

	default:
		if (Height < 10000)
		{
			if ((mode & 1) == 0)
			{
				switch (Height)
				{
				case 5:
					Height2 = 4;
					Height2a = 4;
					break;

				case 6:
					Height2 = 4;
					Height2a = 4;
					break;

				case 7:
					Height2 = 5;
					Height2a = 5;
					break;

				case 8:
					Height2 = 6;
					Height2a = 5;
					break;

				case 9:
					Height2 = 7;
					Height2a = 6;
					OldHeight = 20;
					break;

				default:
					h2 = Height;
					h2 = h2 * 0.7071067812;
					Height2 = (int32) (h2 + 0.75);
					Height2a = (int32) (h2 + 0.25);

					if (Height == 11)
						Height2a--;

					break;
				}

				ellips2(x1, y1, OldHeight, OldHeight, 255);
				ellips2(x2, y2, OldHeight, OldHeight, 255);

				if (ReverseY)
				{
					x1b = x2 - Height2;
					y1b = y2 - Height2;
					x2b = x2 + Height2;
					y2b = y2 + Height2;
					x3b = x1 + Height2;
					y3b = y1 + Height2;
					x4b = x1 - Height2;
					y4b = y1 - Height2;
				}
				else
				{
					x1b = x2 - Height2;
					y1b = y2 + Height2;
					x2b = x2 + Height2;
					y2b = y2 - Height2;
					x3b = x1 + Height2;
					y3b = y1 - Height2;
					x4b = x1 - Height2;
					y4b = y1 + Height2;
				}

				x10 = -DrawWindowMaxY + y4b + x4b;
				x20 = -DrawWindowMinY + y2b + x2b;

				if ((x10 >= DrawWindowMaxX) || (x20 < DrawWindowMinX))
				{
					ok = 1;
					return;
				}

				x10 = DrawWindowMinY - y4b + x4b;
				x20 = DrawWindowMaxY - y2b + x2b;

				if ((x10 >= DrawWindowMaxX) || (x20 < DrawWindowMinX))
				{
					ok = 1;
					return;
				}

				if (Height2 > 2000)
				{
					x1a = (DrawWindowMaxX >> 1);
					y1a = -(DrawWindowMaxX >> 1);
					x2a = DrawWindowMaxX + (DrawWindowMaxY >> 1);
					y2a = (DrawWindowMaxY >> 1);
					x3a = (DrawWindowMaxX >> 1);
					y3a = DrawWindowMaxY + (DrawWindowMaxX >> 1);
					x4a = -(DrawWindowMaxY >> 1);
					y4a = (DrawWindowMaxY >> 1);
					x10 = -DrawWindowMinY + x1b + y1b;
					x20 = -DrawWindowMaxY + x2b + y2b;

					if (x10 >= DrawWindowMinX)
					{
						x1a = x1a + ((x10 - DrawWindowMinX) >> 1);
						y1a = y1a + ((x10 - DrawWindowMinX) >> 1);
						x4a = x4a + ((x10 - DrawWindowMinX) >> 1);
						y4a = y4a + ((x10 - DrawWindowMinX) >> 1);
					}

					if (x20 < DrawWindowMaxX)
					{
						x2a = x2a - ((DrawWindowMaxX - x20) >> 1);
						y2a = y2a - ((DrawWindowMaxX - x20) >> 1);
						x3a = x3a - ((DrawWindowMaxX - x20) >> 1);
						y3a = y3a - ((DrawWindowMaxX - x20) >> 1);
					}

					x10 = DrawWindowMinY - y2b + x2b;
					x20 = DrawWindowMaxY - y4b + x4b;

					if (x20 >= DrawWindowMinX)
					{
						x3a = x3a + ((x20 - DrawWindowMinX) >> 1);
						y3a = y3a - ((x20 - DrawWindowMinX) >> 1);
						x4a = x4a + ((x20 - DrawWindowMinX) >> 1);
						y4a = y4a - ((x20 - DrawWindowMinX) >> 1);
					}

					if (x10 < DrawWindowMaxX)
					{
						x1a = x1a - ((DrawWindowMaxX - x10) >> 1);
						y1a = y1a + ((DrawWindowMaxX - x10) >> 1);
						x2a = x2a - ((DrawWindowMaxX - x10) >> 1);
						y2a = y2a + ((DrawWindowMaxX - x10) >> 1);
					}

					LinePoints[0].x = x1a;
					LinePoints[0].y = y1a;
					LinePoints[1].x = x2a;
					LinePoints[1].y = y2a;
					LinePoints[2].x = x3a;
					LinePoints[2].y = y3a;
					LinePoints[3].x = x4a;
					LinePoints[3].y = y4a;
				}
				else
				{
					LinePoints[0].x = x1b;
					LinePoints[0].y = y1b;
					LinePoints[1].x = x2b;
					LinePoints[1].y = y2b;
					LinePoints[2].x = x3b;
					LinePoints[2].y = y3b;
					LinePoints[3].x = x4b;
					LinePoints[3].y = y4b;
				}

				Polygon(OutputDisplay, LinePoints, 4);
			}
			else
			{
				NrPoints = GetCircleLinePoints(x1, y1, x2 - x1, 0, OldHeight, 3, (POINT *) & EllipssPoints);

				if ((mode & 2) == 0)
					SavePen = SelectObject(OutputDisplay, EmptyPen);

				Polygon(OutputDisplay, EllipssPoints, NrPoints + 2);

				if ((mode & 2) == 0)
					SelectObject(OutputDisplay, SavePen);
			}
		}

		break;
	}
}
