/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calcrect.c
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


#include "owntypes.h"
#include "calcrect.h"
#include "string.h"
#include "math.h"
#include "stdlib.h"

#define  SQR(x) ((x)*(x))


extern float SearchMinX, SearchMinY, SearchMaxX, SearchMaxY;


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestLine(double LineX1, double LineY1, double LineX2, double LineY2)
{
	double RectXmin, RectXmax, RectYmin, RectYmax, DifX, DifY, xa, xb, ya, yb;
	double rico, rico2;

	RectXmin = LineX1;
	RectYmin = LineY1;
	RectXmax = LineX2;
	RectYmax = LineY2;

	if (RectXmin > RectXmax)
	{
		RectXmin = LineX2;
		RectXmax = LineX1;
	}

	if (RectYmin > RectYmax)
	{
		RectYmin = LineY2;
		RectYmax = LineY1;
	}

	if ((RectXmax < SearchMinX) || (RectXmin > SearchMaxX) || (RectYmax < SearchMinY) || (RectYmin > SearchMaxY))
		return 0;


	if ((RectXmax < SearchMaxX) && (RectXmin > SearchMinX) && (RectYmax < SearchMaxY) && (RectYmin > SearchMinY))
		return 1;

	if (RectXmin == RectXmax)
	{
		if ((RectXmin > SearchMinX) && (RectXmin < SearchMaxX))
		{
			if ((RectYmax > SearchMinY) || (RectYmin < SearchMaxY))
				return 1;
		}

		return 0;
	}

	if (RectYmin == RectYmax)
	{
		if ((RectYmin > SearchMinY) && (RectYmin < SearchMaxY))
		{
			if ((RectXmax > SearchMinX) || (RectXmin < SearchMaxX))
				return 1;
		}

		return 0;

	}

	DifX = LineX2 - LineX1;
	DifY = LineY2 - LineY1;

	if (DifX == DifY)
	{
		ya = SearchMinX - LineX1 + LineY1;
		xa = SearchMinY - LineY1 + LineX1;
		yb = SearchMaxX - LineX1 + LineY1;
		xb = SearchMaxY - LineY1 + LineX1;

		if (((ya > SearchMinY) && (ya < SearchMaxY)) || ((yb > SearchMinY) && (yb < SearchMaxY))
		        || ((xa > SearchMinX) && (xa < SearchMaxX)) || ((xb > SearchMinX) && (xb < SearchMaxX)))
			return 1;

		return 0;
	}

	if (DifX == -DifY)
	{
		ya = -SearchMinX + LineX1 + LineY1;
		xa = -SearchMinY + LineY1 + LineX1;
		yb = -SearchMaxX + LineX1 + LineY1;
		xb = -SearchMaxY + LineY1 + LineX1;

		if (((ya > SearchMinY) && (ya < SearchMaxY)) || ((yb > SearchMinY) && (yb < SearchMaxY))
		        || ((xa > SearchMinX) && (xa < SearchMaxX)) || ((xb > SearchMinX) && (xb < SearchMaxX)))
			return 1;

		return 0;

	}

	rico = LineY2;
	rico = (rico - LineY1) / (LineX2 - LineX1);
	ya = ((SearchMinX - LineX1) * rico) + LineY1;
	yb = ((SearchMaxX - LineX1) * rico) + LineY1;

	if (((ya > SearchMinY) && (ya < SearchMaxY)) || ((yb > SearchMinY) && (yb < SearchMaxY)))
		return 1;

	rico2 = LineX2;
	rico2 = (rico2 - LineX1) / (LineY2 - LineY1);
	xa = ((SearchMinY - LineY1) * rico2) + LineX1;
	xb = ((SearchMaxY - LineY1) * rico2) + LineX1;

	if (((xa > SearchMinX) && (xa < SearchMaxX)) || ((xb > SearchMinX) && (xb < SearchMaxX)))
		return 1;

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

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestCircleOutline(double CircleX, double CircleY, double CircleThickness, int32 CircleMode)
{
	double CircleXmin, CircleXmax, CircleYmin, CircleYmax, xa, ya;
	double r, rxmin, rymin, rxmax, rymax;

	CircleThickness = CircleThickness / 2;
	CircleXmin = CircleX;
	CircleXmax = CircleX;
	CircleYmin = CircleY;
	CircleYmax = CircleY;

	if ((CircleMode & 0xf0) > 0)
		CircleXmin = CircleX - CircleThickness;

	if ((CircleMode & 0x3c) > 0)
		CircleYmin = CircleY - CircleThickness;

	if ((CircleMode & 0x0f) > 0)
		CircleXmax = CircleX + CircleThickness;

	if ((CircleMode & 0xc3) > 0)
		CircleYmax = CircleY + CircleThickness;

	if ((CircleXmax < SearchMinX) || (CircleXmin > SearchMaxX) || (CircleYmax < SearchMinY)
	        || (CircleYmin > SearchMaxY))
		return 0;

	if ((CircleXmax < SearchMaxX) && (CircleXmin > SearchMinX) && (CircleYmax < SearchMaxY)
	        && (CircleYmin > SearchMinY))
		return 1;

	r = CircleThickness;
	r = r * r;
	rxmin = SearchMinX - CircleX;
	rxmin = r - rxmin * rxmin;
	rxmax = SearchMaxX - CircleX;
	rxmax = r - rxmax * rxmax;

	if (rxmin >= 0)
	{
		ya = sqrt(rxmin);

		if (((CircleY - ya > SearchMinY) && (CircleY - ya < SearchMaxY))
		        || ((CircleY + ya > SearchMinY) && (CircleY + ya < SearchMaxY)))
			return 1;
	}

	if (rxmax >= 0)
	{
		ya = sqrt(rxmax);

		if (((CircleY - ya > SearchMinY) && (CircleY - ya < SearchMaxY))
		        || ((CircleY + ya > SearchMinY) && (CircleY + ya < SearchMaxY)))
			return 1;
	}

	rymin = SearchMinY - CircleY;
	rymin = r - rymin * rymin;
	rymax = SearchMaxY - CircleY;
	rymax = r - rymax * rymax;

	if (rymin >= 0)
	{
		xa = sqrt(rymin);

		if (((CircleX - xa > SearchMinX) && (CircleX - xa < SearchMaxX))
		        || ((CircleX + xa > SearchMinX) && (CircleX + xa < SearchMaxX)))
			return 1;
	}

	if (rymax >= 0)
	{
		xa = sqrt(rymax);

		if (((CircleX - xa > SearchMinX) && (CircleX - xa < SearchMaxX))
		        || ((CircleX + xa > SearchMinX) && (CircleX + xa < SearchMaxX)))
			return 1;
	}

	return 0;

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestCircle(double CircleX, double CircleY, double CircleThickness, int32 CircleMode)
{
	double CircleXmin, CircleXmax, CircleYmin, CircleYmax, CircleThickness2;
	double r, rxmin, rymin, rxmax, rymax;

	CircleThickness2 = CircleThickness / 2;
	CircleXmin = CircleX;
	CircleYmin = CircleY;
	CircleXmax = CircleX;
	CircleYmax = CircleY;

	if ((CircleMode & 0xf0) > 0)
		CircleXmin = CircleX - CircleThickness2;

	if ((CircleMode & 0x3c) > 0)
		CircleYmin = CircleY - CircleThickness2;

	if ((CircleMode & 0x0f) > 0)
		CircleXmax = CircleX + CircleThickness2;

	if ((CircleMode & 0xc3) > 0)
		CircleYmax = CircleY + CircleThickness2;

	if ((CircleXmax < SearchMinX) || (CircleXmin > SearchMaxX) || (CircleYmax < SearchMinY)
	        || (CircleYmin > SearchMaxY))
		return 0;

	if ((CircleXmax <= SearchMaxX) && (CircleXmin >= SearchMinX) && (CircleYmax <= SearchMaxY)
	        && (CircleYmin >= SearchMinY))
		return 1;

	r = CircleThickness2;
	r = r * r;

	rxmin = SearchMinX - CircleX;
	rxmin = rxmin * rxmin;
	rxmax = SearchMaxX - CircleX;
	rxmax = rxmax * rxmax;
	rymin = SearchMinY - CircleY;
	rymin = rymin * rymin;
	rymax = SearchMaxY - CircleY;
	rymax = rymax * rymax;

	if (rxmin + rymin < r)
		return 1;

	if (rxmin + rymax < r)
		return 1;

	if (rxmax + rymin < r)
		return 1;

	if (rxmax + rymax < r)
		return 1;

	return RectTestCircleOutline(CircleX, CircleY, CircleThickness, (uint8) CircleMode);

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetMinMaxArc(double ArcX, double ArcY, double ArcWidth, double ArcHeight, double StartDiffX, double StartDiffY,
                  double EndDiffX, double EndDiffY, double *ArcXmin, double *ArcYmin, double *ArcXmax, double *ArcYmax)
{
	double ArcThickness2;
	double StartAngle, StartX, StartY, EndX, EndY, EndAngle, DiffAngle, rico;
	int32 StartPos, ok;

	ArcThickness2 = ArcWidth / 2;

	if (StartDiffX == 0.0)
	{
		StartX = StartDiffX;
		StartY = ArcThickness2;

		if (StartDiffY < 0)
			StartY *= -1;
	}
	else
	{
		rico = StartDiffY / StartDiffX;
		StartX = ArcThickness2 * sqrt(1 / (1 + SQR(rico)));

		if (StartDiffX < 0)
			StartX = -StartX;

		StartY = rico * StartX;
	}

	if (EndDiffX == 0.0)
	{
		EndX = EndDiffX;
		EndY = ArcThickness2;

		if (EndDiffY < 0)
			EndY *= -1;
	}
	else
	{
		rico = EndDiffY / EndDiffX;
		EndX = ArcThickness2 * sqrt(1 / (1 + SQR(rico)));

		if (EndDiffX < 0)
			EndX = -EndX;

		EndY = rico * EndX;
	}

	if (StartX != 0.0)
	{
		StartAngle = atan(StartY / StartX) * 180 / PI;

		if (StartAngle < 0.0)
			StartAngle += 180.0;

		if (StartY < 0)
			StartAngle += 180.0;
	}
	else
	{
		if (StartY > 0)
			StartAngle = 90.0;
		else
			StartAngle = 270.0;
	}

	if (EndX != 0.0)
	{
		EndAngle = atan(EndY / EndX) * 180 / PI;

		if (EndAngle < 0.0)
			EndAngle += 180.0;

		if (EndY < 0)
			EndAngle += 180.0;
	}
	else
	{
		if (EndY > 0)
			EndAngle = 90.0;
		else
			EndAngle = 270.0;
	}

	DiffAngle = EndAngle - StartAngle;

	if (DiffAngle < 0)
		DiffAngle += 360.0;

	*ArcXmin = ArcX + StartX;
	*ArcXmin = min(*ArcXmin, ArcX + EndX);
	*ArcXmax = ArcX + StartX;
	*ArcXmax = max(*ArcXmax, ArcX + EndX);
	*ArcYmin = ArcY + StartY;
	*ArcYmin = min(*ArcYmin, ArcY + EndY);
	*ArcYmax = ArcY + StartY;
	*ArcYmax = max(*ArcYmax, ArcY + EndY);
	ok = 1;

	if (DiffAngle < 91)
	{
		StartPos = (int32) (StartAngle / 45);
		StartPos *= 45;

		switch (StartPos)
		{
		case 0:
			break;

		case 45:
			*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);
			break;

		case 90:
			break;

		case 135:
			*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);
			break;

		case 180:
			break;

		case 225:
			*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);
			break;

		case 270:
			break;

		case 315:
			*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);
			break;
		}

		return;
	}

	if (DiffAngle > 269)
	{
		StartPos = (int32) (EndAngle / 45);
		StartPos *= 45;

		switch (StartPos)
		{
		case 0:
			break;

		case 45:
			*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);
			break;

		case 90:
			break;

		case 135:
			*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);
			break;

		case 180:
			break;

		case 225:
			*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);
			break;

		case 270:
			break;

		case 315:
			*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);
			break;
		}

		return;
	}

	if ((DiffAngle > 90) && (DiffAngle < 181))
	{
		StartPos = (int32) (StartAngle / 45);
		StartPos *= 45;

		switch (StartPos)
		{
		case 0:
			*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);
			break;

		case 45:
			*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);

			if (DiffAngle > 135)
				*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);

			break;

		case 90:
			*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);
			break;

		case 135:
			*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);

			if (DiffAngle > 135)
				*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);

			break;

		case 180:
			*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);
			break;

		case 225:
			*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);

			if (DiffAngle > 135)
				*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);

			break;

		case 270:
			*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);
			break;

		case 315:
			*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);

			if (DiffAngle > 135)
				*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);

			break;
		}

		return;
	}
	else
	{
		StartPos = (int32) (EndAngle / 45);
		StartPos *= 45;

		switch (StartPos)
		{
		case 0:
			*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);
			break;

		case 45:
			*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);

			if (DiffAngle > 135)
				*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);

			break;

		case 90:
			*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);
			break;

		case 135:
			*ArcXmin = min(*ArcXmin, ArcX - ArcThickness2);

			if (DiffAngle > 135)
				*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);

			break;

		case 180:
			*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);
			break;

		case 225:
			*ArcYmin = min(*ArcYmin, ArcY - ArcThickness2);

			if (DiffAngle > 135)
				*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);

			break;

		case 270:
			*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);
			break;

		case 315:
			*ArcXmax = max(*ArcXmax, ArcX + ArcThickness2);

			if (DiffAngle > 135)
				*ArcYmax = max(*ArcYmax, ArcY + ArcThickness2);

			break;
		}
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestArc(double ArcX, double ArcY, double ArcWidth, double ArcHeight, double StartDiffX, double StartDiffY,
                  double EndDiffX, double EndDiffY)
{
	double ArcXmin, ArcXmax, ArcYmin, ArcYmax, ArcThickness2;
	double StartAngle, StartX, StartY, EndX, EndY, Diam, EndAngle, rico, SearchCentreX, SearchCentreY, SearchCircleDiam;
	int32 StartPos, EndPos;

	ArcThickness2 = ArcWidth / 2;

	ArcXmin = ArcX - ArcWidth * 0.5;
	ArcYmin = ArcY - ArcHeight * 0.5;
	ArcXmax = ArcX + ArcWidth * 0.5;
	ArcYmax = ArcY + ArcHeight * 0.5;


	if ((ArcXmax < SearchMinX) || (ArcXmin > SearchMaxX) || (ArcYmax < SearchMinY) || (ArcYmin > SearchMaxY))
		return 0;

	if ((ArcXmax < SearchMaxX) && (ArcXmin > SearchMinX) && (ArcYmax < SearchMaxY) && (ArcYmin > SearchMinY))
		return 1;

	SearchCentreX = (SearchMinX + SearchMaxX) * 0.5;
	SearchCentreY = (SearchMinY + SearchMaxY) * 0.5;
	SearchCircleDiam = sqrt(SQR((SearchMaxX - SearchMinX) * 0.5) + SQR((SearchMaxY - SearchMinY) * 0.5));
	Diam = sqrt(SQR(ArcX - SearchCentreX) + SQR(ArcY - SearchCentreY));

	if (Diam - SearchCircleDiam > max(ArcWidth, ArcHeight) * 0.5)
		return 0;

	if (StartDiffX == 0.0)
	{
		if (StartDiffY < 0.0)
			StartAngle = 270.0;
		else
			StartAngle = 90.0;
	}
	else
	{
		rico = StartDiffY / StartDiffX;
		StartAngle = atan(rico) * 180 / PI;

		if (rico > 0.0)
		{
			if (StartDiffX < 0.0)
				StartAngle += 180.0;
		}
		else
		{
			if (StartDiffX > 0.0)
				StartAngle += 360.0;
			else
				StartAngle += 180.0;
		}
	}

	if (EndDiffX == 0.0)
	{
		if (EndDiffY < 0.0)
			EndAngle = 270.0;
		else
			EndAngle = 90.0;
	}
	else
	{
		rico = EndDiffY / EndDiffX;
		EndAngle = atan(rico) * 180 / PI;

		if (rico > 0.0)
		{
			if (EndDiffX < 0.0)
				EndAngle += 180.0;
		}
		else
		{
			if (EndDiffX > 0.0)
				EndAngle += 360.0;
			else
				EndAngle += 180.0;
		}
	}

	StartX = ArcX + StartDiffX;
	StartY = ArcY + StartDiffY;
	EndX = ArcX + EndDiffX;
	EndY = ArcY + EndDiffY;

	StartPos = (int32) (StartAngle / 90);

	if (StartPos == 4)
		StartPos = 0;

	EndPos = (int32) (EndAngle / 90);

	if (EndPos == 4)
		EndPos = 0;

	switch (StartPos)
	{
	case 0:
		switch (EndPos)
		{
		case 0:
			ArcXmin = EndX;
			ArcXmax = StartX;
			ArcYmax = EndY;
			ArcYmin = StartY;
			break;

		case 1:
			ArcXmin = EndX;
			ArcXmax = StartX;
//          ArcYmax=EndY;
			ArcYmin = min(EndY, StartY);
			break;

		case 2:
//          ArcXmin=EndX;
			ArcXmax = StartX;
//          ArcYmax=EndY;
			ArcYmin = EndY;
			break;

		case 3:
//          ArcXmin=EndX;
			ArcXmax = max(StartX, EndX);
//          ArcYmax=EndY;
//          ArcYmin=StartY;
			break;
		}

		break;

	case 1:
		switch (EndPos)
		{
		case 1:
			ArcXmin = EndX;
			ArcXmax = StartX;
			ArcYmax = StartY;
			ArcYmin = EndY;
			break;

		case 2:
//          ArcXmin=EndX;
			ArcXmax = max(StartX, EndX);
			ArcYmax = StartY;
			ArcYmin = EndY;
			break;

		case 3:
//          ArcXmin=EndX;
			ArcXmax = EndX;
			ArcYmax = StartY;
//          ArcYmin=EndY;
			break;

		case 0:
//          ArcXmin=EndX;
//          ArcXmax=max(StartX,EndX);
			ArcYmax = max(StartY, EndY);
//          ArcYmin=StartY;
			break;
		}

		break;

	case 2:
		switch (EndPos)
		{
		case 2:
			ArcXmin = StartX;
			ArcXmax = EndX;
			ArcYmax = StartY;
			ArcYmin = EndY;
			break;

		case 3:
			ArcXmin = StartX;
			ArcXmax = EndX;
			ArcYmax = max(EndY, StartY);
//          ArcYmin=min(EndY,StartY);
			break;

		case 0:
			ArcXmin = StartX;
//          ArcXmax=StartX;
			ArcYmax = EndY;
//          ArcYmin=EndY;
			break;

		case 1:
//          ArcXmin=EndX;
			ArcXmin = min(StartX, EndX);
//          ArcYmax=EndY;
//          ArcYmin=StartY;
			break;
		}

		break;

	case 3:
		switch (EndPos)
		{
		case 3:
			ArcXmin = StartX;
			ArcXmax = EndX;
			ArcYmax = EndY;
			ArcYmin = StartY;
			break;

		case 0:
			ArcXmin = min(EndX, StartX);
//          ArcXmax=StartX;
			ArcYmax = EndY;
			ArcYmin = StartY;
			break;

		case 1:
			ArcXmin = EndX;
//          ArcXmax=StartX;
//          ArcYmax=EndY;
			ArcYmin = StartY;
			break;

		case 2:
//          ArcXmin=EndX;
			ArcYmin = min(StartY, EndY);
//          ArcYmax=EndY;
//          ArcYmin=StartY;
			break;
		}

		break;
	}

	if ((ArcXmax < SearchMinX) || (ArcXmin > SearchMaxX) || (ArcYmax < SearchMinY) || (ArcYmin > SearchMaxY))
		return 0;

	return 1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestRect(double RectXmin, double RectYmin, double RectXmax, double RectYmax)
{

	if ((RectXmax < SearchMinX) || (RectXmin > SearchMaxX) || (RectYmax < SearchMinY) || (RectYmin > SearchMaxY))
		return 0;

	return 1;

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestRect2(double RectX, double RectY, double RectWidth, double RectHeight)
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

	return 1;

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestRectOutline(double RectX, double RectY, double RectWidth, double RectHeight)
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

	if ((RectXmax < SearchMaxX) && (RectXmin > SearchMinX) && (RectYmax < SearchMaxY) && (RectYmin > SearchMinY))
		return 1;

	if ((RectXmin > SearchMinX) && (RectXmin < SearchMaxX))
	{
		if ((RectYmax > SearchMinY) || (RectYmin < SearchMaxY))
			return 1;
	}

	if ((RectXmax > SearchMinX) && (RectXmax < SearchMaxX))
	{
		if ((RectYmax > SearchMinY) || (RectYmin < SearchMaxY))
			return 1;
	}

	if ((RectYmin > SearchMinY) && (RectYmin < SearchMaxY))
	{
		if ((RectXmax > SearchMinX) || (RectXmin < SearchMaxX))
			return 1;
	}

	if ((RectYmax > SearchMinY) && (RectYmax < SearchMaxY))
	{
		if ((RectXmax > SearchMinX) || (RectXmin < SearchMaxX))
			return 1;
	}

	return 0;

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestDiag1(double Diag1X1, double Diag1Y1, double Diag1LengthX, double Diag1Width)
{

	/*            1               +---------------------+
	             /\               |                     |
	           /   \              |                     |
	         / x1   \             |                     |
	     4 /         \            |                     |
	       \          \           |                     |
	        \          \          +---------------------+
	         \          \
	          \          \
	           \          \
	            \         /  2
	             \      /
	              \   / x2
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

	if ((Diag1Xmax < SearchMaxX) && (Diag1Xmin > SearchMinX) && (Diag1Ymax < SearchMaxY) && (Diag1Ymin > SearchMinY))
		return 1;

	xx4 = Diag1X1 - r;
	yy4 = Diag1Y1 - r;
	xx2 = Diag1X2 + r;
	yy2 = Diag1Y2 + r;

	if (SearchMaxX + SearchMaxY < xx4 + yy4)
		return 0;

	if (SearchMinX + SearchMinY > xx2 + yy2)
		return 0;

	if (-SearchMinX + SearchMaxY < -xx2 + yy2)
		return 0;

	if (-SearchMaxX + SearchMinY > -xx4 + yy4)
		return 0;

	return 1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 RectTestDiag2(double Diag2X1, double Diag2Y1, double Diag2LengthX, double Diag2Width)
{

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

	double Diag2Xmin, Diag2Xmax, Diag2Ymin, Diag2Ymax, Diag2X2, Diag2Y2, xx2, yy2, xx4, yy4, r;
	double hulp;

	hulp = Diag2Width;
	hulp = hulp * PI * 0.25;
	r = hulp;

	Diag2X2 = Diag2X1 + Diag2LengthX;
	Diag2Y2 = Diag2Y1 + Diag2LengthX;

	Diag2Xmin = Diag2X1 - r;
	Diag2Ymin = Diag2Y1 - r;
	Diag2Xmax = Diag2X2 + r;
	Diag2Ymax = Diag2Y2 + r;

	if ((Diag2Xmax < SearchMinX) || (Diag2Xmin > SearchMaxX) || (Diag2Ymax < SearchMinY) || (Diag2Ymin > SearchMaxY))
		return 0;

	if ((Diag2Xmax < SearchMaxX) && (Diag2Xmin > SearchMinX) && (Diag2Ymax < SearchMaxY) && (Diag2Ymin > SearchMinY))
		return 1;

	xx4 = Diag2X1 - r;
	yy4 = Diag2Y1 + r;
	xx2 = Diag2X2 + r;
	yy2 = Diag2Y2 - r;

	if (-SearchMaxX + SearchMinY > -xx4 + yy4)
		return 0;

	if (-SearchMinX + SearchMaxY < -xx2 + yy2)
		return 0;

	if (SearchMaxX + SearchMaxY < xx4 + yy4)
		return 0;

	if (SearchMinX + SearchMinY > xx2 + yy2)
		return 0;

	return 1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
