/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calcdiag.c
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


#include "calcdiag.h"
#include "string.h"
#include "math.h"

extern double SearchMinX, SearchMinY, SearchMaxX, SearchMaxY, SearchX1, SearchY1, SearchX2, SearchY2;

/*

Lijn doorsnijd cirkel :

                 2           2    2
cirkel : (x - xm)  + (y - ym)  = r

lijn   :  y = ax + b

        2    2           2
(y - ym)  = r  - (x - xm)

         2    2           2
(ax+b-ym)  = r  - (x - xm)

b2 = b-ym

       2    2    2            2
(ax+b2)  = r  - x  + 2xxm - xm

 2 2             2    2    2            2
a x  + 2axb2 + b2  = r  - x  + 2xxm - xm

 2  2                      2    2     2
x (a +1) + x(2ab2-2xm) + (b2 - r  + xm ) = 0

     2
A = a  + 1

B = 2ab2-2xm
B = 2a(b-ym)-2xm
B = 2ab - 2(xm + aym)

     2    2     2
C = b2 - r  + xm

            2    2     2
C = (b - ym)  - r  + xm

      2
als (B  >= 4AC) twee punten doorsnijding

-----------------------------------------------------------------
        /
      /
    /
  /
/

y = ax  + b
a = 1
b = -x1 + y1

A = 2
B = 2(-x1 + y1) - 2(xm + ym)

                   2    2     2
C = (-x1 + y1 - ym)  - r  + xm

-----------------------------------------------------------------
\
  \
    \
      \
        \

y = ax  + b
a = -1
b = x1 + y1

A = 2
B = -2(x1 + y1) - 2(xm - ym)

                  2    2     2
C = (x1 + y1 - ym)  - r  + xm

*/


int32 DiagTestLine(double x1, double y1, double x2, double y2)
{
	/*
	  double  Xmin,Xmax,Ymin,Ymax,DifX,DifY,xa,xb,ya,yb;
	  double rico,rico2;

	  return 0;



	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	           (y2-y1)
	y = (x-x1)*------- + y1
	           (x2-x1)

	           (x2-x1)
	x = (y-y1)*------- + x1
	           (y2-y1)


	                   \
	y = -x + x1 + y1     \
	x = -y + x1 + y1       \
	y = -x + b               \
	b = x1 + y1                \



	                           /
	y =  x  - x1 + y1        /
	x =  y  + x1 - y1      /
	y =  x  + b          /
	b = -x1 + y1       /


	*/
	return 0;
}

int32 DiagTestCircleOutline(double CircleX, double CircleY, double CircleThickness, double mode)
{

	/*
	  double  CircleXmin,CircleXmax,CircleYmin,CircleYmax,xa,ya;
	  double r,rxmin,rymin,rxmax,rymax;

	  CircleThickness=CircleThickness/2;
	  CircleXmin=CircleX;
	  CircleXmax=CircleX;
	  CircleYmin=CircleY;
	  CircleYmax=CircleY;

	  if ((mode & 0xf0) > 0) CircleXmin=CircleX-CircleThickness;
	  if ((mode & 0x3c) > 0) CircleYmin=CircleY-CircleThickness;
	  if ((mode & 0x0f) > 0) CircleXmax=CircleX+CircleThickness;
	  if ((mode & 0xc3) > 0) CircleYmax=CircleY+CircleThickness;

	  if ((CircleXmax<SearchMinX)
	     ||
	     (CircleXmin>SearchMaxX)
	     ||
	     (CircleYmax<SearchMinY)
	     ||
	     (CircleYmin>SearchMaxY)) return 0;
	*/

	return 0;

}



int32 DiagTestCircle(double CircleX, double CircleY, double CircleThickness)
{

	/*            1
	             /\
	           /   \
	         /      \
	     4 /         \
	       \          \
	        \          \
	         \          \
	          \          \
	           \          \
	            \         /  2
	             \      /
	              \   /
	               \/
	               3
	*/


	double xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4, CircleR, CircleMinXD, CircleMinYD, CircleMaxXD, CircleMaxYD,
	       CircleXmin, CircleXmax, CircleYmin, CircleYmax;
	double hulpx, hulpy, hulp;

	CircleThickness = CircleThickness / 2;

	CircleXmin = CircleX - CircleThickness;
	CircleYmin = CircleY - CircleThickness;
	CircleXmax = CircleX + CircleThickness;
	CircleYmax = CircleY + CircleThickness;

	if ((CircleXmax < SearchMinX) || (CircleXmin > SearchMaxX) || (CircleYmax < SearchMinY)
	        || (CircleYmin > SearchMaxY))
		return 0;

	hulp = CircleThickness;
	hulp = hulp * 0.707106781186548;
	CircleR = hulp;

	CircleMinXD = CircleX - CircleR;
	CircleMinYD = CircleY - CircleR;
	CircleMaxXD = CircleX + CircleR;
	CircleMaxYD = CircleY + CircleR;

	xx4 = SearchX1;
	yy4 = SearchY1;

	xx2 = SearchX2;
	yy2 = SearchY2;

	if (CircleMaxXD + CircleMaxYD < xx4 + yy4)
		return 0;

	if (CircleMinXD + CircleMinYD > xx2 + yy2)
		return 0;

	if (-CircleMaxXD + CircleMinYD > -xx4 + yy4)
		return 0;

	if (-CircleMinXD + CircleMaxYD < -xx2 + yy2)
		return 0;

	if ((-CircleMinXD + CircleMaxYD <= -xx4 + yy4) && (-CircleMaxXD + CircleMinYD >= -xx2 + yy2)
	        && (CircleMinXD + CircleMinYD <= xx2 + yy2) && (CircleMaxXD + CircleMaxYD >= xx4 + yy4))
		return 1;

	xx1 = (SearchX1 - SearchY1 + SearchX2 + SearchY2) / 2;
	yy1 = (-SearchX1 + SearchY1 + SearchX2 + SearchY2) / 2;

	xx3 = (SearchX1 + SearchY1 + SearchX2 - SearchY2) / 2;
	yy3 = (SearchX1 + SearchY1 - SearchX2 + SearchY2) / 2;

	if ((CircleX + CircleY > xx2 + yy2) && (-CircleX + CircleY > -xx4 + yy4))
	{
		hulpx = CircleX - xx1;
		hulpy = CircleY - yy1;
		hulp = sqrt(hulpx * hulpx + hulpy * hulpy);

		if (hulp > CircleThickness)
			return 0;
	}

	if ((-CircleX + CircleY < -xx2 + yy2) && (CircleX + CircleY < xx4 + yy4))
	{
		hulpx = CircleX - xx3;
		hulpy = CircleY - yy3;
		hulp = sqrt(hulpx * hulpx + hulpy * hulpy);

		if (hulp > CircleThickness)
			return 0;
	}

	if ((CircleX + CircleY < xx4 + yy4) && (-CircleX + CircleY > -xx4 + yy4))
	{
		hulpx = CircleX - xx4;
		hulpy = CircleY - yy4;
		hulp = sqrt(hulpx * hulpx + hulpy * hulpy);

		if (hulp > CircleThickness)
			return 0;
	}

	if ((-CircleX + CircleY < -xx2 + yy2) && (CircleX + CircleY > xx2 + yy2))
	{
		hulpx = CircleX - xx2;
		hulpy = CircleY - yy2;
		hulp = sqrt(hulpx * hulpx + hulpy * hulpy);

		if (hulp > CircleThickness)
			return 0;
	}

	return 1;

}

int32 DiagTestRect(double RectXmin, double RectYmin, double RectXmax, double RectYmax)
{

	if ((RectXmax < SearchMinX) || (RectXmin > SearchMaxX) || (RectYmax < SearchMinY) || (RectYmin > SearchMaxY))
		return 0;

	if (RectXmax + RectYmax < SearchX1 + SearchY1)
		return 0;

	if (RectXmin + RectYmin > SearchX2 + SearchY2)
		return 0;

	if (-RectXmax + RectYmin > -SearchX1 + SearchY1)
		return 0;

	if (-RectXmin + RectYmax < -SearchX2 + SearchY2)
		return 0;

	return 1;

}

int32 DiagTestRect2(double RectX, double RectY, double RectWidth, double RectHeight)
{
	double RectXmin, RectXmax, RectYmin, RectYmax;

	RectWidth = RectWidth / 2;
	RectHeight = RectHeight / 2;
	RectXmin = RectX - RectWidth;
	RectYmin = RectY - RectHeight;
	RectXmax = RectX + RectWidth;
	RectYmax = RectY + RectHeight;

	if ((RectXmax < SearchMinX) || (RectXmin > SearchMaxX) || (RectYmax < SearchMinY) || (RectYmin > SearchMaxY))
		return 0;

	if (RectXmax + RectYmax < SearchX1 + SearchY1)
		return 0;

	if (RectXmin + RectYmin > SearchX2 + SearchY2)
		return 0;

	if (-RectXmax + RectYmin > -SearchX1 + SearchY1)
		return 0;

	if (-RectXmin + RectYmax < -SearchX2 + SearchY2)
		return 0;

	return 1;

}

int32 DiagTestRectOutline(double x1, double y1, double x2, double y2)
{

//  double  Xmin,Xmax,Ymin,Ymax;


	return 0;

}


int32 DiagTestDiag1(double Diag1X1, double Diag1Y1, double Diag1LengthX, double Diag1Width)
{

	/*            1
	             /\
	    5      /   \
	         / x1   \
	     4 /         \
	       \          \
	        \          \
	         \          \
	          \          \
	           \          \
	            \         /  2
	             \      /
	              \   / x2   6
	               \/
	               3
	*/


	double Diag1Xmin, Diag1Xmax, Diag1Ymin, Diag1Ymax, Diag1X2, Diag1Y2, xx2, yy2, xx4, yy4, r;
	double hulp;


	hulp = Diag1Width;
	hulp = hulp * 0.707106781186548 * 0.5;
	r = hulp;

	Diag1X2 = Diag1X1 + Diag1LengthX;
	Diag1Y2 = Diag1Y1 - Diag1LengthX;

	Diag1Xmin = Diag1X1 - r;
	Diag1Ymin = Diag1Y2 - r;
	Diag1Xmax = Diag1X2 + r;
	Diag1Ymax = Diag1Y1 + r;

	if ((Diag1Xmax < SearchMinX) || (Diag1Xmin > SearchMaxX) || (Diag1Ymax < SearchMinY) || (Diag1Ymin > SearchMaxY))
		return 0;

	xx2 = Diag1X2 + r;
	yy2 = Diag1Y2 + r;
	xx4 = Diag1X1 - r;
	yy4 = Diag1Y1 - r;

	if (SearchX2 + SearchY2 < xx4 + yy4)
		return 0;

	if (SearchX1 + SearchY1 > xx2 + yy2)
		return 0;

	if (-SearchX2 + SearchY2 > -xx4 + yy4)
		return 0;

	if (-SearchX1 + SearchY1 < -xx2 + yy2)
		return 0;

	return 1;

}

int32 DiagTestDiag2(double Diag2X1, double Diag2Y1, double Diag2LengthX, double Diag2Width)
{

	/*                     1

	                     / \
	                   /    \
	                 /       *  x2
	               /          \     6
	             /             \
	           /              /  2
	     4   /              /
	       /              /
	   5   \            /
	        \         /
	         *      /
	      x1  \   /
	           \/
	             3
	*/

	double Diag2Xmin, Diag2Xmax, Diag2Ymin, Diag2Ymax, Diag2X2, Diag2Y2, xx2, yy2, xx4, yy4, r;
	double hulp;

	hulp = Diag2Width;
	hulp = hulp * 0.707106781186548 * 0.5;
	r = hulp;

	Diag2X2 = Diag2X1 + Diag2LengthX;
	Diag2Y2 = Diag2Y1 + Diag2LengthX;

	Diag2Xmin = Diag2X1 - r;
	Diag2Ymin = Diag2Y1 - r;
	Diag2Xmax = Diag2X2 + r;
	Diag2Ymax = Diag2Y2 + r;

	if ((Diag2Xmax < SearchMinX) || (Diag2Xmin > SearchMaxX) || (Diag2Ymax < SearchMinY) || (Diag2Ymin > SearchMaxY))
		return 0;


	xx2 = Diag2X2 + r;
	yy2 = Diag2Y2 - r;
	xx4 = Diag2X1 - r;
	yy4 = Diag2Y1 + r;

	if (-SearchX2 + SearchY2 > -xx4 + yy4)
		return 0;

	if (-SearchX1 + SearchY1 < -xx2 + yy2)
		return 0;

	if (SearchX2 + SearchY2 < xx4 + yy4)
		return 0;

	if (SearchX1 + SearchY1 > xx2 + yy2)
		return 0;

	return 1;
}
