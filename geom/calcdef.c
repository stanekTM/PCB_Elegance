/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calcdef.c
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
#include "files2.h"
#include "memory.h"
#include "stdio.h"

extern double Factor, Xoffset, Yoffset, GridSize;
extern int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY;

double LineCrossX, LineCrossY;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double PixelToReal(int32 X)
{
	double hulp;

	hulp = X;
	return (hulp / Factor);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double PixelToRealOffX(int32 X)
{
	double hulp;

	hulp = (X - DrawWindowMinX);
	return ((hulp / Factor) + Xoffset);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double PixelToRealOffY(int32 Y)
{
	double hulp;

	hulp = Y;
	return ((hulp / Factor) + Yoffset);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AdjustMouseToGrid(int32 MouseX, int32 MouseY, double *x2, double *y2)
{
	double hulp3;
	double niks, f;

	hulp3 = (PixelToRealOffX(MouseX)) / GridSize;

	if (hulp3 >= 0.0)
		hulp3 += 0.5;
	else
		hulp3 -= 0.5;

	niks = modf(hulp3, &f);
	hulp3 = (f * GridSize);
	*x2 = hulp3;

	hulp3 = (PixelToRealOffY(DrawWindowMaxY - MouseY - 1)) / GridSize;

	if (hulp3 >= 0.0)
		hulp3 += 0.5;
	else
		hulp3 -= 0.5;

	niks = modf(hulp3, &f);
	hulp3 = (f * GridSize);
	*y2 = hulp3;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


double AdjustToDrawGrid(double x)
{
	double hulp;
	double niks, f;

	hulp = x / GridSize;

	if (hulp >= 0.0)
		hulp += 0.5;
	else
		hulp -= 0.5;

	niks = modf(hulp, &f);
	return (f * GridSize);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePointFromOtherPoint(float *x, float *y, double OX, double OY, double Rotation)
{
	double dx, dy, length;
	double r1, hoek;

	if (InRange2(Rotation, 0.0))
		return;


	dx = *x - OX;
	dy = *y - OY;
	length = sqrt(SQR(dx) + SQR(dy));

	if (InRange2(dx, 0.0))
	{	// Vertical line
		if (dy > 0.0)
		{	// Vertical line to top
			hoek = ANGLE_CONVERT(90.0);
		}
		else
		{	// Vertical line to bottom
			hoek = ANGLE_CONVERT(270.0);
		}
	}
	else
	{
		r1 = dy / dx;
		hoek = atan(r1);

		if (r1 > 0.0)
		{
			if (dx < 0.0)
				hoek += ANGLE_CONVERT(180.0);
		}
		else
		{
			if (dx > 0.0)
				hoek += ANGLE_CONVERT(360.0);
			else
				hoek += ANGLE_CONVERT(180.0);
		}
	}

	hoek += ANGLE_CONVERT(Rotation);

	if (hoek > ANGLE_CONVERT(360.0))
		hoek -= ANGLE_CONVERT(360.0);

	if (InRange2(hoek, 0.0))
	{
		*x = (float) length;
		*y = 0.0;
	}
	else if (InRange2(hoek, ANGLE_CONVERT(90.0)))
	{
		*x = 0.0;
		*y = (float) length;
	}
	else if (InRange2(hoek, ANGLE_CONVERT(180.0)))
	{
		*x = (float) -length;
		*y = 0.0;
	}
	else if (InRange2(hoek, ANGLE_CONVERT(270.0)))
	{
		*x = 0.0;
		*y = (float) -length;
	}
	else
	{
		*x = (float) (cos(hoek) * length);
		*y = (float) (sin(hoek) * length);
	}

	*x += (float) OX;
	*y += (float) OY;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePointFromOtherPoint2(double *x, double *y, double OX, double OY, double Rotation)
{
	double dx, dy, r1, length, hoek;

	if (InRange2(Rotation, 0.0))
		return;


	dx = *x - OX;
	dy = *y - OY;
	length = sqrt(SQR(dx) + SQR(dy));

	if (InRange2(dx, 0.0))
	{	// Vertical line
		if (dy > 0.0)
		{	// Vertical line to top
			hoek = ANGLE_90;
		}
		else
		{	// Vertical line to bottom
			hoek = ANGLE_270;
		}
	}
	else
	{
		r1 = dy / dx;
		hoek = atan(r1);

		if (r1 > 0.0)
		{
			if (dx < 0.0)
				hoek += ANGLE_180;
		}
		else
		{
			if (dx > 0.0)
				hoek += ANGLE_360;
			else
				hoek += ANGLE_180;
		}
	}

	hoek += ANGLE_CONVERT(Rotation);

	if (hoek > ANGLE_360)
		hoek -= ANGLE_360;

	if (InRange2(hoek, 0.0))
	{
		*x = length;
		*y = 0.0;
	}
	else if (InRange2(hoek, ANGLE_90))
	{
		*x = 0.0;
		*y = length;
	}
	else if (InRange2(hoek, ANGLE_180))
	{
		*x = -length;
		*y = 0.0;
	}
	else if (InRange2(hoek, ANGLE_270))
	{
		*x = 0.0;
		*y = -length;
	}
	else
	{
		*x = cos(hoek) * length;
		*y = sin(hoek) * length;
	}

	*x += OX;
	*y += OY;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotateFlipPoint(float *x, float *y, double CX, double CY, int32 Mode)
{
	double hx, hy;

	switch (Mode)
	{
	case 1:
//90  :   x= CX+CY-y
//        y=-CX+CY+x
		hx = CX + CY - *y;
		hy = -CX + CY + *x;
		*x = (float) hx;
		*y = (float) hy;
		break;

	case 2:
//180 :   x=2*CX-x
//        y=2*CY-y
		hx = 2 * CX - *x;
		hy = 2 * CY - *y;
		*x = (float) hx;
		*y = (float) hy;
		break;

//270 :   x= CX-CY+y
//        y= CX+CY-x
	case 3:
		hx = CX - CY + *y;
		hy = CX + CY - *x;
		*x = (float) hx;
		*y = (float) hy;
		break;

	case 4:
//   flip x
		hx = 2 * CX - *x;
		*x = (float) hx;
		break;

	case 8:
//   flip y
		hy = 2 * CY - *y;
		*y = (float) hy;
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotateFlipPoint2(double *x, double *y, double OX, double OY, int32 Mode)
{
	double hx, hy;

	switch (Mode)
	{
	case 1:
		hx = OX + OY - *y;
		hy = -OX + OY + *x;
		*x = hx;
		*y = hy;
		break;

	case 2:
		hx = 2 * OX - *x;
		hy = 2 * OY - *y;
		*x = hx;
		*y = hy;
		break;

	case 3:
		hx = OX - OY + *y;
		hy = OX + OY - *x;
		*x = hx;
		*y = hy;
		break;

	case 4:
//   flip x
		hx = 2 * OX - *x;
		*x = hx;
		break;

	case 8:
//   flip y
		hy = 2 * OY - *y;
		*y = hy;
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 InRangeRico(double rico1, double rico2)
{

	if (rico1 == rico2)
		return 1;

	if ((rico1 == 0) && (fabs(rico2) < 0.0001))
		return 1;

	if ((rico2 == 0) && (fabs(rico1) < 0.0001))
		return 1;

	if ((rico1 < 0) && (rico1 < rico2 * 0.99) && (rico1 > rico2 * 1.01))
		return 1;

	if ((rico1 > 0) && (rico1 > rico2 * 0.99) && (rico1 < rico2 * 1.01))
		return 1;

	if ((fabs(rico1) < 0.0001) && (fabs(rico2) < 0.0001))
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double GetAngleBetweenLines(double x1, double y1, double x2, double y2, double x3, double y3)
{
	double Angle1, Angle2, Distance1, Distance2, Angle;

	ConvNormalCoorToPolar(x2, y2, x1, y1, &Angle1, &Distance1);
	ConvNormalCoorToPolar(x2, y2, x3, y3, &Angle2, &Distance2);
	Angle = fabs(Angle1 - Angle2);

	if (Angle > ANGLE_CONVERT(180.0))
		Angle -= ANGLE_CONVERT(180.0);

	return Angle;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ConvertPointToPolar(double x, double y, double *Distance, double *Angle)
{
	double r1, length, hoek;

	length = sqrt(SQR(x) + SQR(y));
	*Distance = length;

	if (InRange2(x, 0.0))
	{	// Vertical line
		if (y > 0.0)
		{	// Vertical line to top
			hoek = ANGLE_CONVERT(90.0);
		}
		else
		{	// Vertical line to bottom
			hoek = ANGLE_CONVERT(270.0);
		}
	}
	else
	{
		r1 = y / x;
		hoek = atan(r1);

		if (r1 > 0.0)
		{
			if (x < 0.0)
				hoek += ANGLE_CONVERT(180.0);
		}
		else
		{
			if (x > 0.0)
				hoek += ANGLE_CONVERT(360.0);
			else
				hoek += ANGLE_CONVERT(180.0);
		}
	}

	if (hoek > ANGLE_CONVERT(360.0) - 0.001)
		hoek -= ANGLE_CONVERT(360.0);

	*Angle = (hoek * 180.0 / PI);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ConvNormalCoorToPolar(double x1, double y1, double x2, double y2, double *Angle, double *Length)
{
	double dx, dy, r1, hoek;

	dx = x2 - x1;
	dy = y2 - y1;

	if ((dx == 0.0) && (dy == 0.0))
	{
		*Length = 0.0;
		*Angle = 0.0;
		return;
	}

	*Length = sqrt(SQR(dx) + SQR(dy));

	if (InRangeSpecial(dx, 0.0, 0.000001))
	{	// Vertical line
		if (dy > 0.0)
		{	// Vertical line to top
			hoek = PI * 0.5;
		}
		else
		{	// Vertical line to bottom
			hoek = PI * 1.5;
		}
	}
	else
	{
		r1 = dy / dx;
		hoek = atan(r1);

		if (r1 >= 0.0)
		{
			if (dx < 0.0)
				hoek += PI;
		}
		else
		{
			if (dx > 0.0)
				hoek += 2 * PI;
			else
			{
				if (dx < 0.0)
					hoek += PI;
			}
		}
	}

	*Angle = hoek;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

double UnitConvert(double Value, int32 Units)
{
	switch (Units)
	{
	case 0:
		return Value * 2540.0;

	case 1:
		return Value * 100000.0;

	case 2:
		return Value * 2540000.0;

	default:
		return Value;
	}
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

double ConvertUnits(double Value, int32 Units)
{
	switch (Units)
	{
	case 0:
		return Value / 2540.0;

	case 1:
		return Value / 100000.0;

	case 2:
		return Value / 2540000.0;

	default:
		return Value;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePoint(float *x, float *y, double Rotation)
{
	double hx, hy, dx, dy, r1, length, hoek;

	if (InRange2(Rotation, 0.0))
		return;
	else if (InRange2(Rotation, 90.0))
	{
		hx = -*y;
		hy = *x;
		*x = (float) hx;
		*y = (float) hy;
		return;
	}
	else if (InRange2(Rotation, 180.0))
	{
		*x = -*x;
		*y = -*y;
		return;
	}
	else if (InRange2(Rotation, 270.0))
	{
		hx = *y;
		hy = -*x;
		*x = (float) hx;
		*y = (float) hy;
		return;
	}
	else
	{
		dx = *x;
		dy = *y;

		if (InRange2(dx, 0.0))
		{
			if (dy > 0.0)
				hoek = ANGLE_CONVERT(90.0);
			else
				hoek = ANGLE_CONVERT(270.0);
		}
		else
		{
			r1 = dy / dx;
			hoek = atan(r1);

			if (r1 > 0.0)
			{
				if (dx < 0.0)
					hoek += ANGLE_CONVERT(180.0);
			}
			else
			{
				if (dx > 0.0)
					hoek += ANGLE_CONVERT(360.0);
				else
					hoek += ANGLE_CONVERT(180.0);
			}
		}

		hoek += ANGLE_CONVERT(Rotation);
		length = sqrt(SQR(dx) + SQR(dy));

		if (InRange2(hoek, 0.0))
		{
			*x = (float) length;
			*y = 0.0;
		}
		else if (InRange2(hoek, ANGLE_CONVERT(90.0)))
		{
			*x = 0.0;
			*y = (float) length;
		}
		else if (InRange2(hoek, ANGLE_CONVERT(180.0)))
		{
			*x = (float) -length;
			*y = 0.0;
		}
		else if (InRange2(hoek, ANGLE_CONVERT(270.0)))
		{
			*x = 0.0;
			*y = (float) -length;
		}
		else
		{
			*x = (float) (cos(hoek) * length);
			*y = (float) (sin(hoek) * length);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePoint2(double *x, double *y, double Rotation)
{
	double hx, hy, dx, dy, r1, length, hoek;

	if (InRange2(Rotation, 0.0))
		return;
	else if (InRange2(Rotation, ANGLE_90))
	{
		hx = -*y;
		hy = *x;
		*x = hx;
		*y = hy;
		return;
	}
	else if (InRange2(Rotation, ANGLE_180))
	{
		*x = -*x;
		*y = -*y;
		return;
	}
	else if (InRange2(Rotation, ANGLE_270))
	{
		hx = *y;
		hy = -*x;
		*x = hx;
		*y = hy;
		return;
	}
	else
	{
		dx = *x;
		dy = *y;

		if (InRange2(dx, 0.0))
		{
			if (dy > 0.0)
				hoek = ANGLE_90;
			else
				hoek = ANGLE_270;
		}
		else
		{
			r1 = dy / dx;
			hoek = atan(r1);

			if (r1 > 0.0)
			{
				if (dx < 0.0)
					hoek += ANGLE_180;
			}
			else
			{
				if (dx > 0.0)
					hoek += ANGLE_360;
				else
					hoek += ANGLE_180;
			}
		}

		hoek += ANGLE_CONVERT(Rotation);
		length = sqrt(SQR(dx) + SQR(dy));

		if (InRange2(hoek, 0.0))
		{
			*x = length;
			*y = 0.0;
		}
		else if (InRange2(hoek, ANGLE_90))
		{
			*x = 0.0;
			*y = length;
		}
		else if (InRange2(hoek, ANGLE_180))
		{
			*x = -length;
			*y = 0.0;
		}
		else if (InRange2(hoek, ANGLE_270))
		{
			*x = 0.0;
			*y = -length;
		}
		else
		{
			*x = (cos(hoek) * length);
			*y = (sin(hoek) * length);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 LineCrosses(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                  double LineX4, double LineY4)
//  -1  Lines dont cross
//   0  Lines overlap
//   1  Lines cross
{

	double a, b, x, y, minx12, miny12, maxx12, maxy12, minx34, miny34, maxx34, maxy34;

//  LineCrossX=-1000000.0;
//  LineCrossY=-1000000.0;

	minx12 = min(LineX1, LineX2);
	maxx34 = max(LineX3, LineX4);

	if (minx12 > maxx34)
		return -1;

	miny12 = min(LineY1, LineY2);
	maxy34 = max(LineY3, LineY4);

	if (miny12 > maxy34)
		return -1;

	maxx12 = max(LineX1, LineX2);
	minx34 = min(LineX3, LineX4);

	if (maxx12 < minx34)
		return -1;

	miny34 = min(LineY3, LineY4);
	maxy12 = max(LineY1, LineY2);

	if (maxy12 < miny34)
		return -1;

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
	  if ((X1GreaterThenX2a(x,maxx12))
	     ||
	     (X1SmallerThenX2a(x,minx12))
	     ||
	     (X1GreaterThenX2a(y,maxy12))
	     ||
	     (X1SmallerThenX2a(y,miny12))
	     ||
	     (X1GreaterThenX2a(x,maxx34))
	     ||
	     (X1SmallerThenX2a(x,minx34))
	     ||
	     (X1GreaterThenX2a(y,maxy34))
	     ||
	     (X1SmallerThenX2a(y,miny34))) return -1;

	  if ((x>maxx12)
	     ||
	     (x<minx12)
	     ||
	     (y>maxy12)
	     ||
	     (y<miny12)
	     ||
	     (x>maxx34)
	     ||
	     (x<minx34)
	     ||
	     (y>maxy34)
	     ||
	     (y<miny34)) return -1;
	*/

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

int32 GetCrossPointLineWithPoint(double x1, double y1, double x2, double y2, double px, double py, double *cx,
                                 double *cy, int32 mode)
{
	double rico1, b1, b2, divx, divy, tx, ty;
	/*

	  Line:             y1=rico1*x1+b1
	  Line from point:  py=rico2*px+b2  ( py=-px/rico1+b2 )

	       (rico1*(b2-b1)
	  x  = ---------------
	        rico1*rico1+1

	        rico1*rico1*b2 + b1
	  y  = --------------------
	          rico1*rico1 + 1


	  Cross point: x


	  -----------------x----------------------  line
	                   .
	                   .
	                   .
	                   .
	                   .
	                   . (Line from point to cross point)
	                   .
	                   .
	                   .
	                   .
	                   .
	                   o  (Point)


	*/


	divx = x2 - x1;
	divy = y2 - y1;

	if (InRange(divx, 2.0))
	{	// Vertical line
		*cx = x1;
		*cy = py;
		return 0;
	}

	if (InRange(divy, 2.0))
	{	// Horizontal line
		*cx = px;
		*cy = y1;
		return 0;
	}

	rico1 = divy / divx;
	b1 = y1 - rico1 * x1;
	b2 = py + px / rico1;
	tx = (rico1 * (b2 - b1)) / (rico1 * rico1 + 1);
	ty = (rico1 * rico1 * b2 + b1) / (rico1 * rico1 + 1);
	*cx = tx;
	*cy = ty;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PointWithinLine(double x1, double y1, double x2, double y2, double px, double py, int32 mode)
{
	double divx, divy;

	divx = x2 - x1;
	divy = y2 - y1;

	if (fabs(divx) > fabs(divy))
	{
		if ((px > min(x1, x2)) && (px < max(x1, x2)))
			return 1;
	}
	else
	{
		if ((py > min(y1, y2)) && (py < max(y1, y2)))
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double MinDistancePointToLine(double x1, double y1, double x2, double y2, double px, double py, int32 mode)
{
	double dist, cx, cy, divx, divy, Length1, Length2;

	divx = x2 - x1;
	divy = y2 - y1;
	GetCrossPointLineWithPoint(x1, y1, x2, y2, px, py, &cx, &cy, 0);

	if (fabs(divx) > fabs(divy))
	{	// Most horizontal
		if ((cx > min(x1, x2)) && (cx < max(x1, x2)))
			dist = CalcLengthLine(px, py, cx, cy);
		else
		{
			Length1 = CalcLengthLine2(x1, y1, cx, cy);
			Length2 = CalcLengthLine2(x2, y2, cx, cy);

			if (Length1 < Length2)
			{	// Distance from point to x1,y1
				dist = CalcLengthLine(x1, y1, px, py);
			}
			else
			{	// Distance from point to x2,y2
				dist = CalcLengthLine(x2, y2, px, py);
			}
		}
	}
	else
	{	// Most vertical
		if ((cy > min(y1, y2)) && (cy < max(y1, y2)))
			dist = CalcLengthLine(px, py, cx, cy);
		else
		{
			Length1 = CalcLengthLine2(x1, y1, cx, cy);
			Length2 = CalcLengthLine2(x2, y2, cx, cy);

			if (Length1 < Length2)
			{	// Distance from point to x1,y1
				dist = CalcLengthLine(x1, y1, px, py);
			}
			else
			{	// Distance from point to x2,y2
				dist = CalcLengthLine(x2, y2, px, py);
			}
		}
	}

	return dist;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double MinDistanceLineToLine(double x11, double y11, double x12, double y12, double x21, double y21, double x22,
                             double y22, int32 mode)
{
	double dist;

	dist = 1e9;

	if (LineCrosses(x11, y11, x12, y12, x21, y21, x22, y22) != 1)
	{	// No crosses
		dist = min(dist, MinDistancePointToLine(x11, y11, x12, y12, x21, y21, 0));
		dist = min(dist, MinDistancePointToLine(x11, y11, x12, y12, x22, y22, 0));
		dist = min(dist, MinDistancePointToLine(x21, y21, x22, y22, x11, y11, 0));
		dist = min(dist, MinDistancePointToLine(x21, y21, x22, y22, x12, y12, 0));
	}
	else
		dist = 0.0;

	return dist;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetLayerText(int32 Layer, LPSTR TextStr, int32 mode)
{
	int32 Layer2;

	if ((mode & 2) == 0)
		Layer2 = Layer;
	else
		Layer2 = NrPadLayers - Layer - 1;

	switch (Layer)
	{
	case SOLD_MASK_BOTTOM_LAYER:
		strcpy(TextStr, SC(6, "Solder mask bottom"));
		break;

	case SOLD_MASK_TOP_LAYER:
		strcpy(TextStr, SC(5, "Solder mask top"));
		break;

	case PASTE_MASK_BOTTOM_LAYER:
		strcpy(TextStr, SC(8, "Paste mask bottom"));
		break;

	case PASTE_MASK_TOP_LAYER:
		strcpy(TextStr, SC(7, "Paste mask top"));
		break;

	case SILKSCREEN_TOP_LAYER:
		strcpy(TextStr, SC(9, "Silkscreen top"));
		break;

	case SILKSCREEN_BOTTOM_LAYER:
		strcpy(TextStr, SC(10, "Silkscreen bottom"));
		break;

	case COMP_OUTLINE_LAYER:
		strcpy(TextStr, SC(11, "Component outline"));
		break;

	case BOARD_OUTLINE_LAYER:
		strcpy(TextStr, SC(12, "Board outline"));
		break;

	case INFO_LAYER:
		strcpy(TextStr, "Info 1");
		break;

	case INFO_LAYER2:
		strcpy(TextStr, "Info 2");
		break;

	case INFO_LAYER3:
		strcpy(TextStr, "Info 3");
		break;

	case INFO_LAYER4:
		strcpy(TextStr, "Info 4");
		break;

	case PLACEMENT_OUTLINE_LAYER:
		strcpy(TextStr, SC(13, "Placement outline"));
		break;

	case DRILL_LAYER:
		strcpy(TextStr, SC(14, "Drill plated"));
		break;

	case DRILL_UNPLATED_LAYER:
		strcpy(TextStr, SC(15, "Drill unplated"));
		break;

	case POWER_PAD_LAYER:
		strcpy(TextStr, SC(16, "Anti power pad"));
		break;

	case INNER_PAD_LAYER:
		strcpy(TextStr, SC(17, "Inner pad"));
		break;

	default:
		if (Layer < 32)
		{
			sprintf(TextStr, SC(22, "Layer %i"), Layer2);

			if (Layer == 0)
				strcat(TextStr, SC(21, " (Bottom)"));

			if (NrPadLayers > 1)
			{
				if (Layer == NrPadLayers - 1)
					strcat(TextStr, SC(20, " (Top)"));
			}
		}
		else
		{
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			{
				sprintf(TextStr, SC(23, "Routing keepout layer %i"), Layer2 - ROUTING_KEEPOUT_LAYER);

				if (Layer - ROUTING_KEEPOUT_LAYER == 0)
					strcat(TextStr, SC(21, " (Bottom)"));

				if (NrPadLayers > 1)
				{
					if (Layer - ROUTING_KEEPOUT_LAYER == NrPadLayers - 1)
						strcat(TextStr, SC(20, " (Top)"));
				}
			}
		}

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckIfInnerLayer(int32 Layer)
{
	if (NrPadLayers > 2)
	{
		if ((Layer > 0) && (Layer < NrPadLayers - 1))
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ArcToLineSegments(double x1, double y1, double Width, double Height, double x2a, double y2a, double x2b,
                        double y2b, double *LineSegments)
{
	int32 NrSegments, cnt2, count, SegmentCount;
	double x3, y3, x4, y4, x5, y5, hoek1, hoek2, hoek, hoek_inc, Length, sinx, sinx2, lengte1, lengte2;
#ifdef _DEBUG
	double hoek1a, hoek2a;
#endif

	NrSegments = 32;
	SegmentCount = 0;

	if (max(Width, Height) > (400 * 2540))
		NrSegments = 64;

	if (max(Width, Height) > (1000 * 2540))
		NrSegments = 128;

	if ((InRange(x2a, x2b)) && (InRange(y2a, y2b)))
	{
		hoek1 = ANGLE_180;
		hoek2 = hoek1 + ANGLE_360;
	}
	else
	{
		x2a += x1;
		y2a += y1;
		x2b += x1;
		y2b += y1;
		ConvNormalCoorToPolar(x1, y1, x2a, y2a, &hoek1, &lengte1);
		ConvNormalCoorToPolar(x1, y1, x2b, y2b, &hoek2, &lengte2);

		if (hoek2 < hoek1)
			hoek2 += ANGLE_360;
	}

	/*
	  hoek1+=ANGLE_CONVERT(Rotation);
	  hoek2+=ANGLE_CONVERT(Rotation);
	  if (hoek1>ANGLE_CONVERT(360.0)) hoek1-=ANGLE_CONVERT(360.0);
	  if (hoek2>ANGLE_CONVERT(360.0)) hoek2-=ANGLE_CONVERT(360.0);
	*/
	hoek = hoek1;
	count = (int32) ((hoek2 - hoek1) / (ANGLE_360 / NrSegments));
	count = max(1, count);
	hoek_inc = ((hoek2 - hoek1 + 0.01) / count);
#ifdef _DEBUG
	hoek1a = hoek1 * 180 / PI;
	hoek2a = hoek2 * 180 / PI;
#endif

	sinx = sin(hoek);
	sinx2 = sinx * sinx;
	Length =
	    (Width * 0.5) * (Height * 0.5) * sqrt(1 / (SQR((Height * 0.5)) * (1 - sinx2) + SQR((Width * 0.5)) * sinx2));
	x5 = Length * cos(hoek);
	y5 = Length * sin(hoek);
//  x5=Width*0.5*(cos(hoek));
//  y5=Height*0.5*(sin(hoek));

	x4 = x1 + x5;
	y4 = y1 + y5;

	for (cnt2 = 0; cnt2 < count; cnt2++)
	{
		hoek += hoek_inc;
		x3 = x4;
		y3 = y4;
		sinx = sin(hoek);
		sinx2 = sinx * sinx;
		Length =
		    (Width * 0.5) * (Height * 0.5) * sqrt(1 / (SQR((Height * 0.5)) * (1 - sinx2) + SQR((Width * 0.5)) * sinx2));
		x5 = Length * cos(hoek);
		y5 = Length * sin(hoek);
//    x5=Width*0.5*(cos(hoek));
//    y5=Height*0.5*(sin(hoek));
		x4 = x1 + x5;
		y4 = y1 + y5;
		LineSegments[SegmentCount++] = x3;
		LineSegments[SegmentCount++] = y3;
		LineSegments[SegmentCount++] = x4;
		LineSegments[SegmentCount++] = y4;
	}

	return SegmentCount / 4;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetUnitsValue(int32 Units, double value, LPSTR str, int32 mode)
{
	double NewValue;
	NewValue = ConvertUnits(value, Units);
	NewValue *= 1.000000001;

	switch (Units)
	{
	case UNITS_MILS:
		if ((mode & 4) == 0)
		{
			if ((mode & 1) == 0)
				sprintf(str, "%.2f", NewValue);
			else
				sprintf(str, "%.4f", NewValue);

			StripAppendingZeros(str, 0);

			if ((mode & 2) == 2)
				strcat(str, " thou");
		}
		else
		{
			NewValue = ConvertUnits(value, UNITS_INCH);
			NewValue *= 1.000000001;

			if ((mode & 1) == 0)
				sprintf(str, "%.5f", NewValue);
			else
				sprintf(str, "%.8f", NewValue);

			if ((mode & 2) == 2)
				strcat(str, SC(27, " inch"));
		}

		break;

	case UNITS_MM:
		if ((mode & 1) == 0)
			sprintf(str, "%.4f", NewValue);
		else
			sprintf(str, "%.7f", NewValue);

		StripAppendingZeros(str, 0);

		if ((mode & 2) == 2)
			strcat(str, " mm");

		break;

	case UNITS_INCH:
		if ((mode & 1) == 0)
			sprintf(str, "%.5f", NewValue);
		else
			sprintf(str, "%.8f", NewValue);

		if ((mode & 2) == 2)
			strcat(str, SC(27, " inch"));

		break;

	case UNITS_HPGL:
		if ((mode & 1) == 0)
			sprintf(str, "%.2f", NewValue);
		else
			sprintf(str, "%.5f", NewValue);

		break;

	case UNITS_0_01MM:
		if ((mode & 1) == 0)
			sprintf(str, "%.2f", NewValue);
		else
			sprintf(str, "%.5f", NewValue);

		break;

	case UNITS_0_1MILS:
		if ((mode & 1) == 0)
			sprintf(str, "%.1f", NewValue);
		else
			sprintf(str, "%.4f", NewValue);

		break;

	case UNITS_0_1MM:
		if ((mode & 1) == 0)
			sprintf(str, "%.3f", NewValue);
		else
			sprintf(str, "%.6f", NewValue);

		break;

	case UNITS_MICRON:
		if ((mode & 1) == 0)
			sprintf(str, "%.1f", NewValue);
		else
			sprintf(str, "%.4f", NewValue);

		if ((mode & 2) == 2)
			strcat(str, SC(28, " µm"));

		break;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
