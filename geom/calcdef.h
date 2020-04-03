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

#define Mult(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr)+0.5)) : ((int32)(Factor*(Nr)-0.5)) )
#define Mult2(Nr) ((int32)(Factor*(Nr)))
#define MultX(Nr) (Mult((Nr)-Xoffset)+DrawWindowMinX)
#define MultY(Nr) (DrawWindowMaxY-Mult((Nr)-Yoffset)-1)
#define MultY2(Nr) (Mult((Nr)-Yoffset)-1)
#define NotInRange(x1,x2) ( ((((x1)>(x2)-5.0) && ((x1)<(x2)+5.0))) ? (0) : (1) )
#define    InRange(x1,x2) ( ((((x1)>(x2)-5.0) && ((x1)<(x2)+5.0))) ? (1) : (0) )
#define    InRange2(x1,x2) ( ((((x1)>(x2)-0.01) && ((x1)<(x2)+0.01))) ? (1) : (0) )
#define NotInRange2(x1,x2) ( ((((x1)>(x2)-0.01) && ((x1)<(x2)+0.01))) ? (0) : (1) )
#define    InRange3(x1,x2) ( ((((x1)>(x2)-0.001) && ((x1)<(x2)+0.001))) ? (1) : (0) )
#define    InRange4(x1,x2) ( ((((x1)>(x2)-1000.0) && ((x1)<(x2)+1000.0))) ? (1) : (0) )
#define NotInRange4(x1,x2) ( ((((x1)>(x2)-1000.0) && ((x1)<(x2)+1000.0))) ? (0) : (1) )
#define    InRange5(x1,x2) ( ((((x1)>(x2)-20000.0) && ((x1)<(x2)+20000.0))) ? (1) : (0) )
#define    InRange6(x1,x2) ( ((((x1)>(x2)-0.2) && ((x1)<(x2)+0.2))) ? (1) : (0) )
#define    InRange7(x1,x2) ( ((((x1)>(x2)-0.000001) && ((x1)<(x2)+0.000001))) ? (1) : (0) )
#define X1SmallerThenX2(x1,x2) ( ((x1)<(x2)-5.0) ? (1) : (0) )
#define X1GreaterThenX2(x1,x2) ( ((x1)>(x2)+5.0) ? (1) : (0) )

#define InRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (1) : (0) )
#define NotInRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (0) : (1) )

#define CalcLengthLine(x1,y1,x2,y2) (sqrt(SQR((x2)-(x1))+SQR((y2)-(y1))))
#define CalcLengthLine2(x1,y1,x2,y2) (SQR((x2)-(x1))+SQR((y2)-(y1)))

#define ObjectsMinMaxOverlap(Object1,Object2)    \
    (  ((Object1->maxx>Object2->minx)            \
       &&                                        \
       (Object1->minx<Object2->maxx)             \
       &&                                        \
       (Object1->maxy>Object2->miny)             \
       &&                                        \
       (Object1->miny<Object2->maxy)) ? (1) : (0) )

#define ObjectsMinMaxOverlap2(Object1,Object2)   \
    (  ((Object1->maxx+10.0>Object2->minx)       \
       &&                                        \
       (Object1->minx-10.0<Object2->maxx)        \
       &&                                        \
       (Object1->maxy+10.0>Object2->miny)        \
       &&                                        \
       (Object1->miny-10.0<Object2->maxy)) ? (1) : (0) )

#define ObjectsMinMaxOverlap3(Object1,Object2)   \
    (  ((Object1->maxx+100.0>Object2->minx)      \
       &&                                        \
       (Object1->minx-100.0<Object2->maxx)       \
       &&                                        \
       (Object1->maxy+100.0>Object2->miny)       \
       &&                                        \
       (Object1->miny-100.0<Object2->maxy)) ? (1) : (0) )

#define PI          3.14159265358979262
#define ANGLE_90    (PI*0.5)
#define ANGLE_180   (PI*1.0)
#define ANGLE_270   (PI*1.5)
#define ANGLE_360   (PI*2.0)
#define SQR(x)      ((x)*(x))

#define UNITS_MILS            0
#define UNITS_MM              1
#define UNITS_INCH            2
#define UNITS_0_01MM          3
#define UNITS_HPGL            4
#define UNITS_0_1MILS         5
#define UNITS_0_1MM           6
#define UNITS_MICRON          7

#define ANGLE_CONVERT(Rotation) ((Rotation*PI/180.0))

double PixelToReal(int32 X);

double PixelToRealOffX(int32 X);

double PixelToRealOffY(int32 Y);

void AdjustMouseToGrid(int32 MouseX, int32 MouseY, double *x2, double *y2);

double AdjustToDrawGrid(double x);

void RotatePointFromOtherPoint(float *x, float *y, double OX, double OY, double Rotation);

void RotatePointFromOtherPoint2(double *x, double *y, double OX, double OY, double Rotation);

void RotateFlipPoint(float *x, float *y, double CX, double CY, int32 Mode);

void RotatePoint(float *x, float *y, double Rotation);

void RotatePoint2(double *x, double *y, double Rotation);

void RotateFlipPoint2(double *x, double *y, double OX, double OY, int32 Mode);

int32 InRangeRico(double rico1, double rico2);

double GetAngleBetweenLines(double x1, double y1, double x2, double y2, double x3, double y3);

void ConvertPointToPolar(double x, double y, double *Distance, double *Angle);

void ConvNormalCoorToPolar(double x1, double y1, double x2, double y2, double *Angle, double *Length);

double UnitConvert(double Value, int32 Units);

double ConvertUnits(double Value, int32 Units);

int32 LineCrosses(double LineX1, double LineY1, double LineX2, double LineY2, double LineX3, double LineY3,
                  double LineX4, double LineY4);

int32 GetCrossPointLineWithPoint(double x1, double y1, double x2, double y2, double px, double py, double *cx,
                                 double *cy, int32 mode);

int32 PointWithinLine(double x1, double y1, double x2, double y2, double px, double py, int32 mode);

double MinDistancePointToLine(double x1, double y1, double x2, double y2, double px, double py, int32 mode);

double MinDistanceLineToLine(double x11, double y11, double x12, double y12, double x21, double y21, double x22,
                             double y22, int32 mode);

int32 GetLayerText(int32 Layer, LPSTR TextStr, int32 mode);

int32 CheckIfInnerLayer(int32 Layer);

int32 ArcToLineSegments(double x1, double y1, double Width, double Height, double x2a, double y2a, double x2b,
                        double y2b, double *LineSegments);

int32 GetUnitsValue(int32 Units, double value, LPSTR str, int32 mode);

#endif
