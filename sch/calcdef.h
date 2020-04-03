/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calcdef.h
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


#ifndef _CALCDEF

#define _CALCDEF

#include "owntypes.h"

#define PI                    3.14159265358979262
#define ANGLE_90              (PI*0.5)
#define ANGLE_180             (PI*1.0)
#define ANGLE_270             (PI*1.5)
#define ANGLE_360             (PI*2.0)
#define SQRT05                0.70710678118654752
#define VALUE_1_MIN_SQRT05    0.29289321881345248
#define VALUE_SQRT2_MIN_1     0.41421356237309504
#define SQRT2                 1.41421356237309504
#define SQR(x)                ((x)*(x))

#define ARROW_LENGTH          1.0

#define ANGLE_CONVERT(Rotation) ((Rotation*PI/180.0))

#define Mult(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr)+0.5)) : ((int32)(Factor*(Nr)-0.5)) )
#define MultX(Nr) (Mult(Nr-Xoffset)+DrawWindowMinX)
#define MultY(Nr) (DrawWindowMaxY-Mult(Nr-Yoffset)-1)

#define  InRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (1) : (0) )
#define  NotInRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (0) : (1) )

#define CalcLengthLine(x1,y1,x2,y2) (sqrt(SQR((x2)-(x1))+SQR((y2)-(y1))))

#define AdjustTo100Mil(Nr) ( (((Nr))>(0)) ? (((Nr+50)/100)*100) : (((Nr-50)/100)*100) )

double PixelToReal(int32 X);

double PixelToRealOffX(int32 X);

double PixelToRealOffY(int32 Y);

double AdjustToDrawGrid(double x);

int32 ArcToLineSegments(double x1, double y1, double Width, double Height, double x2a, double y2a, double x2b,
                        double y2b, double *LineSegments);

double GetAngleBetweenLines(double x1, double y1, double x2, double y2, double x3, double y3);

void RotatePoint2(double *x, double *y, double Rotation);

double UnitConvert(double Value, int32 Units);

void ConvNormalCoorToPolar(double x1, double y1, double x2, double y2, double *Angle, double *Length);

void RotatePointFromOtherPoint(double *x, double *y, double OX, double OY, double Rotation);

void RotatePointFromOtherPoint2(float *x, float *y, double OX, double OY, double Rotation);

double AdjustToGrid(double x, double Grid);

int32 ConvertTextString(LPSTR TextStr, LPSTR NewTextStr);

#endif
