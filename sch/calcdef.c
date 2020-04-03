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
#include "uservar.h"
#include "memory.h"
#include "time.h"
#include "ctype.h"


char ConvertedTextString[MAX_LENGTH_STRING];

extern double Factor, Xoffset, Yoffset, DrawGridSize;
extern int32 DrawWindowMinX, DrawWindowMinY, DrawWindowMaxX, DrawWindowMaxY;


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


double PixelToReal(int32 X)
{
	double hulp;

	hulp = (double) X;
	return (hulp / Factor);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

double PixelToRealOffX(int32 X)
{
	double hulp;

	hulp = (double) (X - DrawWindowMinX);
	return ((hulp / Factor) + Xoffset);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

double PixelToRealOffY(int32 Y)
{
	double hulp;

	hulp = (double) Y;
	return ((hulp / Factor) + Yoffset);
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

double AdjustToDrawGrid(double x)
{
	double hulp, niks, f;

	hulp = x / DrawGridSize;

	if (hulp >= 0.0)
		hulp += 0.5;
	else
		hulp -= 0.5;

	niks = modf(hulp, &f);
	return (f * DrawGridSize);
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

		if (r1 >= 0.0)
		{
			if (dx < 0.0)
				hoek += ANGLE_180;
		}
		else
		{
			if (dx > 0.0)
				hoek += ANGLE_360;
			else
			{
				if (dx < 0.0)
					hoek += ANGLE_180;
			}
		}
	}

	*Angle = hoek;
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

	if (max(Width, Height) > (double) (4.0))
		NrSegments = 64;

	if (max(Width, Height) > (double) (10.0))
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
		LineSegments[SegmentCount++] = (double) x3;
		LineSegments[SegmentCount++] = (double) y3;
		LineSegments[SegmentCount++] = (double) x4;
		LineSegments[SegmentCount++] = (double) y4;
	}

	return SegmentCount / 4;
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

void RotatePoint2(double *x, double *y, double Rotation)
{
	double hx, hy, dx, dy, r1, length, hoek;

	if (InRangeSpecial(Rotation, 0.0, 0.001))
		return;
	else if (InRangeSpecial(Rotation, ANGLE_90, 0.001))
	{
		hx = -*y;
		hy = *x;
		*x = hx;
		*y = hy;
		return;
	}
	else if (InRangeSpecial(Rotation, ANGLE_180, 0.001))
	{
		*x = -*x;
		*y = -*y;
		return;
	}
	else if (InRangeSpecial(Rotation, ANGLE_270, 0.001))
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

		if (InRangeSpecial(dx, 0.0, 0.001))
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

		if (InRangeSpecial(hoek, 0.0, 0.001))
		{
			*x = length;
			*y = 0.0;
		}
		else if (InRangeSpecial(hoek, ANGLE_90, 0.001))
		{
			*x = 0.0;
			*y = length;
		}
		else if (InRangeSpecial(hoek, ANGLE_180, 0.001))
		{
			*x = -length;
			*y = 0.0;
		}
		else if (InRangeSpecial(hoek, ANGLE_270, 0.001))
		{
			*x = 0.0;
			*y = -length;
		}
		else
		{
			*x = cos(hoek) * length;
			*y = sin(hoek) * length;
		}
	}
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
		return (double) (Value * 2540.0);

	case 1:
		return (double) (Value * 100000.0);

	case 2:
		return (double) (Value * 2540000.0);

	default:
		return Value;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePointFromOtherPoint(double *x, double *y, double OX, double OY, double Rotation)
{
	double dx, dy, r1, length, hoek;

	if (InRangeSpecial(Rotation, (double) 0.0, 0.001))
		return;


	dx = *x - OX;
	dy = *y - OY;
	length = sqrt(SQR(dx) + SQR(dy));

	if (InRangeSpecial(dx, 0.0, 0.001))
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

	if (InRangeSpecial(hoek, 0.0, 0.001))
	{
		*x = (double) length;
		*y = (double) 0.0;
	}
	else if (InRangeSpecial(hoek, ANGLE_90, 0.001))
	{
		*x = (double) 0.0;
		*y = (double) length;
	}
	else if (InRangeSpecial(hoek, ANGLE_180, 0.001))
	{
		*x = (double) -length;
		*y = (double) 0.0;
	}
	else if (InRangeSpecial(hoek, ANGLE_270, 0.001))
	{
		*x = (double) 0.0;
		*y = (double) -length;
	}
	else
	{
		*x = (double) (cos(hoek) * length);
		*y = (double) (sin(hoek) * length);
	}

	*x += OX;
	*y += OY;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePointFromOtherPoint2(float *x, float *y, double OX, double OY, double Rotation)
{
	double dx, dy, r1, length, hoek;

	if (InRangeSpecial(Rotation, (double) 0.0, 0.001))
		return;


	dx = *x - OX;
	dy = *y - OY;
	length = sqrt(SQR(dx) + SQR(dy));

	if (InRangeSpecial(dx, 0.0, 0.001))
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

	if (InRangeSpecial(hoek, 0.0, 0.001))
	{
		*x = (float) length;
		*y = (float) 0.0;
	}
	else if (InRangeSpecial(hoek, ANGLE_90, 0.001))
	{
		*x = (float) 0.0;
		*y = (float) length;
	}
	else if (InRangeSpecial(hoek, ANGLE_180, 0.001))
	{
		*x = (float) -length;
		*y = (float) 0.0;
	}
	else if (InRangeSpecial(hoek, ANGLE_270, 0.001))
	{
		*x = (float) 0.0;
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

double AdjustToGrid(double x, double Grid)
{
	double hulp;
	double niks, f;

	hulp = x / Grid;

	if (hulp >= (double) 0.0)
		hulp += (double) 0.5;
	else
		hulp -= (double) 0.5;

	niks = modf(hulp, &f);
	return (double) (f * Grid);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ConvertTextString(LPSTR TextStr, LPSTR NewTextStr)
{
	struct tm FileTime, *today;
	time_t ltime;
	char NewStr[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	int32 cnt, Length;

	Length = strlen(TextStr);

	if ((Length < 2) || (TextStr[0] != '$'))
	{
//    strcpy(NewTextStr,TextStr);
		return -1;
	}

	strcpy(NewStr, TextStr);
	cnt = 2;

	while (__iscsym(NewStr[cnt]))
		cnt++;

	NewStr[cnt] = 0;

	if (stricmp(NewStr, "$DATE") == 0)
	{
		/*
		tm_sec   Seconds after minute (0 – 59)
		tm_min   Minutes after hour (0 – 59)
		tm_hour   Hours after midnight (0 – 23)
		tm_mday   Day of month (1 – 31)
		tm_mon   Month (0 – 11; January = 0)
		tm_year   Year (current year minus 1900)
		tm_wday   Day of week (0 – 6; Sunday = 0)
		tm_yday   Day of year (0 – 365; January 1 = 0)
		*/
		if (Design.DesignDate.Year != 0)
		{
			FileTime.tm_year = Design.DesignDate.Year;
			FileTime.tm_mon = Design.DesignDate.Month - 1;
			FileTime.tm_mday = Design.DesignDate.Day;
			FileTime.tm_hour = Design.DesignDate.Hour;
			FileTime.tm_min = Design.DesignDate.Minutes;
			FileTime.tm_sec = 0;
			strftime(ConvertedTextString, 100, "%B %d, %Y    %X", &FileTime);
//          strftime(TextStr,100,"%c",&FileTime);
//          strftime(TextStr,100,"%x  %X",&FileTime);
		}
		else
		{
			time(&ltime);
			today = localtime(&ltime);
			strftime(ConvertedTextString, 100, "%B %d, %Y    %X", today);
		}

		strcpy(NewTextStr, ConvertedTextString);
		return 0;
	}

	if ((stricmp(NewStr, "$DESIGNNAME") == 0) && (DesignFile[0] != 0))
	{
		GetFilePartFromFileName(str2, DesignFile);
		CutExtensionFileName(str2);
		strcpy(NewTextStr, str2);
		return 0;
	}

	if ((stricmp(NewStr, "$FILENAME") == 0) && (!EditingSymbol))
	{
		GetFilePartFromFileName(str2, EditFile);
		CutExtensionFileName(str2);
		strcpy(NewTextStr, str2);
		return 0;
	}

	if (GetUserVar(NewStr, ConvertedTextString, 0) == 1)
	{
		strcpy(NewTextStr, ConvertedTextString);
		return 0;
	}

	strcpy(NewTextStr, NewStr);
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
