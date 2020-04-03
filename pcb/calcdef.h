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

#include "windows.h"
#include "owntypes.h"

#define PI                    3.14159265358979262
#define ANGLE_45              (PI*0.25)
#define ANGLE_90              (PI*0.5)
#define ANGLE_135             (PI*0.75)
#define ANGLE_180             (PI*1.0)
#define ANGLE_225             (PI*1.25)
#define ANGLE_270             (PI*1.5)
#define ANGLE_315             (PI*1.75)
#define ANGLE_360             (PI*2.0)
#define SQRT05                0.70710678118654752
#define VALUE_1_MIN_SQRT05    0.29289321881345248
#define VALUE_SQRT2_MIN_1     0.41421356237309504
#define SQRT2                 1.41421356237309504
#define SQR(x)                ((x)*(x))

#define UNITS_MILS            0
#define UNITS_MM              1
#define UNITS_INCH            2
#define UNITS_0_01MM          3
#define UNITS_HPGL            4
#define UNITS_0_1MILS         5
#define UNITS_0_1MM           6
#define UNITS_MICRON          7


#define Mult(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr)+0.5)) : ((int32)(Factor*(Nr)-0.5)) )
#define MultX(Nr) (Mult((Nr)-Xoffset)+DrawWindowMinX)
#define MultY(Nr) (DrawWindowMaxY-Mult((Nr)-Yoffset)-1)
#define MultY2(Nr) (Mult((Nr)-Yoffset)-1)
#define MultY3(Nr) (Mult((Nr)-Yoffset))
#define NotInRange(x1,x2)   ( ((((x1)>(x2)-10.0)     && ((x1)<(x2)+10.0)))     ? (0) : (1) )
#define    InRange(x1,x2)   ( ((((x1)>(x2)-10.0)     && ((x1)<(x2)+10.0)))     ? (1)  : (0) )
#define    InRange2(x1,x2)  ( ((((x1)>(x2)-0.01)     && ((x1)<(x2)+0.01)))     ? (1)  : (0) )
#define NotInRange2(x1,x2)  ( ((((x1)>(x2)-0.01)     && ((x1)<(x2)+0.01)))     ? (0) : (1) )
#define    InRange3(x1,x2)  ( ((((x1)>(x2)-50.0)     && ((x1)<(x2)+50.0)))     ? (1)  : (0) )
#define    InRange4(x1,x2)  ( ((((x1)>(x2)-1000.0)   && ((x1)<(x2)+1000.0)))   ? (1)  : (0) )
#define    InRange6(x1,x2)  ( ((((x1)>(x2)-0.2)      && ((x1)<(x2)+0.2)))      ? (1)  : (0) )
#define    InRange7(x1,x2)  ( ((((x1)>(x2)-0.00001) && ((x1)<(x2)+0.00001)))   ? (1)  : (0) )
#define    InRange8(x1,x2)  ( ((((x1)>(x2)-25.0)     && ((x1)<(x2)+25.0)))     ? (1)  : (0) )
#define NotInRange8(x1,x2)  ( ((((x1)>(x2)-25.0)     && ((x1)<(x2)+25.0)))     ? (0) : (1) )
#define    InRange9(x1,x2)  ( ((((x1)>(x2)-20000.0)  && ((x1)<(x2)+20000.0)))  ? (1)  : (0) )
#define    InRange10(x1,x2) ( ((((x1)>(x2)-100000.0) && ((x1)<(x2)+100000.0))) ? (1)  : (0) )
#define    InRange11(x1,x2) ( (((x1>x2-0.001) && (x1<x2+0.001))) ? (1) : (0) )

#define  InRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (1) : (0) )
#define  NotInRangeSpecial(x1,x2,Resolution) ( ((((x1)>(x2)-(Resolution)) && ((x1)<(x2)+(Resolution)))) ? (0) : (1) )


#define RoundValue(x,Rounding) ( ((x)>(0.0)) ? (((x)+(Rounding)*0.5)) : (((x)-(Rounding)*0.5)) )

#define CalcLengthLine(x1,y1,x2,y2) (sqrt(SQR((x2)-(x1))+SQR((y2)-(y1))))
#define CalcLengthLine2(x1,y1,x2,y2) (SQR((x2)-(x1))+SQR((y2)-(y1)))

#define X1SmallerThenX2(x1,x2) ( ((x1)<(x2)-10.0) ? (1) : (0) )
#define X1GreaterThenX2(x1,x2) ( ((x1)>(x2)+10.0) ? (1) : (0) )

#define ANGLE_CONVERT(Rotation) (((Rotation)*PI/180.0))

#define SetRotationToComp(Rotation)            ( ((Rotation & 6   ) >> 1) + ((Rotation & 1    ) << 2 ) )
#define GetRotationFromComp(Rotation)          ( ((Rotation & 3   ) << 1) + ((Rotation & 4    ) >> 2 ) )
#define GetReferenceRotationFromComp(Rotation) ( ( Rotation & 6   )       + ((Rotation & 0x400) >> 10) )
#define SetReferenceRotationToComp(Rotation)   ( ( Rotation & 6   )       + ((Rotation & 1    ) << 10) )
#define GetValueRotationFromComp(Rotation)     ( ((Rotation & 0x60) >> 4) + ((Rotation & 0x800) >> 11) )
#define SetValueRotationToComp(Rotation)       ( ((Rotation & 6   ) << 4) + ((Rotation & 1    ) << 11) )

extern double CircleCos[257];
extern double CircleSin[257];


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double LimitRotation(double rotation);

double PixelToReal(int32 X);

double PixelToRealOffX(int32 X);

double PixelToRealOffY(int32 Y);

void AdjustMouseToGrid(int32 MouseX, int32 MouseY, double *x2, double *y2);

double AdjustToGrid(double x, double Grid);

double AdjustToDrawGrid(double x);

double AdjustToDrawGridDiv2(double x);

void XchangeMem(uint8 * buf1, uint8 * buf2, int32 Length);

double UnitsConvert(int32 Units, double value);

double ConvertToUnits2(int32 Units, double value, int32 mode);

double ConvertToUnits(int32 Units, double value);

int32 GetUnitsValue(int32 Units, double value, LPSTR str, int32 mode);

int32 GetUnitsValue2(int32 Units, double value, LPSTR str, int32 mode);

int32 GetUnitText(int32 Units, LPSTR str, int32 mode);

int32 SetNextUnits(int32 Units, int32 mode);

int32 ScanParameters(int32 NrParameters, LPSTR Str, int32 mode);

void RotateFlipPoint(float *x, float *y, double OX, double OY, int32 Mode);

void RotateFlipPoint2(double *x, double *y, double OX, double OY, int32 Mode);

void ConvertPointToPolar(double x, double y, double *Distance, double *Angle);

void ConvNormalCoorToPolar(double x1, double y1, double x2, double y2, double *Angle, double *Length);

void RotatePoint(float *x, float *y, double Rotation);

void RotatePoint2(double *x, double *y, double Rotation);

void RotatePointFromOtherPoint2(double *x, double *y, double OX, double OY, double Rotation);

void RotatePointFromOtherPoint(float *x, float *y, double OX, double OY, double Rotation);

int32 RectTestLine2(double LineX1, double LineY1, double LineX2, double LineY2);

int32 RectTestLine3(double LineX1, double LineY1, double LineX2, double LineY2, double Thickness);

int32 GetRotationFromFloat(double Rotation);

void SevereProgramError(LPSTR SourceFile, int32 LineNr);

int32 CircleTestCircleObjects(double CircleX1, double CircleY1, double CircleThickness1, double CircleX2,
                              double CircleY2, double CircleThickness2);

int32 RectTestCircleObjects(double RectX, double RectY, double RectWidth, double RectHeight, double CircleX,
                            double CircleY, double CircleThickness);

int32 CircleTestDiag1Objects(double CircleX, double CircleY, double CircleThickness, double Diag1X1, double Diag1Y1,
                             double Diag1LengthX, double Diag1Width);

int32 CircleTestDiag2Objects(double CircleX, double CircleY, double CircleThickness, double Diag2X1, double Diag2Y1,
                             double Diag2LengthX, double Diag2Width);

int32 RectTestDiag1Objects(double RectX, double RectY, double RectWidth, double RectHeight, double Diag1X1,
                           double Diag1Y1, double Diag1LengthX, double Diag1Width);

int32 RectTestDiag2Objects(double RectX, double RectY, double RectWidth, double RectHeight, double Diag2X1,
                           double Diag2Y1, double Diag2LengthX, double Diag2Width);

int32 Diag1TestDiag1Objects(double Diag1X11, double Diag1Y11, double Diag1LengthX1, double Diag1Width1, double Diag1X21,
                            double Diag1Y21, double Diag1LengthX2, double Diag1Width2);

int32 Diag1TestDiag2Objects(double Diag1X11, double Diag1Y11, double Diag1LengthX1, double Diag1Width1, double Diag2X21,
                            double Diag2Y21, double Diag2LengthX2, double Diag2Width2);

int32 Diag2TestDiag2Objects(double Diag2X11, double Diag2Y11, double Diag2LengthX1, double Diag2Width1, double Diag2X21,
                            double Diag2Y21, double Diag2LengthX2, double Diag2Width2);

int32 InRangeRico(double rico1, double rico2);

double GetAngleBetweenLines(double x1, double y1, double x2, double y2, double x3, double y3);

void GetString2a(LPSTR Str, LPSTR Result);

void GetString2b(LPSTR Str, LPSTR Delimiters, LPSTR Result);

int32 CompressSrcDestLayer(int32 SrcLayer, int32 DestLayer);

int32 DecompressSrcDestLayer(int32 CodedLayer, int32 * SrcLayer, int32 * DestLayer);

void SendDlgItemUnits(HWND Dialog, int32 Control, int32 Units);

int32 CheckIfInnerLayer(int32 Layer);

int32 CheckIfTopLayer(int32 Layer);

int32 CheckIfBottomLayer(int32 Layer);

int32 CheckIfCopperLayer(int32 Layer);

int32 CheckIfDrillLayer(int32 Layer);

int32 CheckIfLayerHasObjectWithClearances(int32 Layer);

int32 CheckGeometryLayer(int32 * Layer, int32 NrGeomLayers, int32 Mirror);

ObjectRecord *GetNewObject4(void);

ObjectRecord *GetNewObject5(int32 Layer);

int32 IsRoutingKeepoutLayer(int32 Layer);

uint8 SwapBitsByte(uint8 code);

void SwapBitsLine(uint8 * LineBytes, int32 ByteCount);

void GetRotationAndMirrorFromShapeText(double ShapeRotation, double *Rotation, int32 * Mirror);

int32 IsPolygonVisible(PolygonRecord * DrawPolygon, int32 mode);

int32 IsAreaFillVisible(AreaFillRecord * AreaFill, int32 mode);

int32 AdjustOffsetOnPoints(double *Points, double OffsetX, double OffsetY, int32 count, int32 mode);

int32 ConvertTextString(LPSTR TextStr, LPSTR NewTextStr, int32 Layer);

uint32 mktime2(uint32 year, uint32 mon, uint32 day, uint32 hour, uint32 min, uint32 sec);

int32 GetTimeString(int32 mode, LPSTR TimeString);

#endif
