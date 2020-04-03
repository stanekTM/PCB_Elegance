/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: pixel.c
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
#include "memory.h"
#include "string.h"
#include "edit.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "toets.h"
#include "ellipss.h"
#include "nets.h"
#include "calc.h"
#include "calc4.h"
#include "calcdef.h"
#include "math.h"
#include "graphics.h"
#include "calcrect.h"
#include "mainloop.h"
#include "insdel.h"
#include "select.h"
#include "dialogs.h"
#include "stdio.h"
#include "resource.h"
#include "help.h"
#include "files.h"
#include "files2.h"
#include "gerber2.h"
#include "polygon.h"
#include "rules.h"
#include "InputGerber.h"
#include "pixel.h"
#include "rules.h"
#include "dxf.h"
#include "pcb.h"


#define BitMapXY(x,y) ((BitmapBuf1[(y)*128+((x) >> 3)] & ((0x80 >> ((x) & 7)))))
#define BitMap2XY(x,y) ((BitmapBuf2[(y)*NrBytesBitmapLine+((x) >> 3)] & ((0x80 >> ((x) & 7)))))
#define BitMap3XY(x,y) ((BitmapBuf1[(y)*NrBytesBitmapLine+((x) >> 3)] & ((0x80 >> ((x) & 7)))))

#define SetBitMap2PointXY(mainx,mainy,x,y) (BitmapBuf2[((y)+1022*mainy)*NrBytesBitmapLine+(((x)+1022*mainx) >> 3)]|=((0x80 >> (((x)+1022*mainx) & 7))))
#define SetBitMap3PointXY(x,y) (BitmapBuf2[(y)*NrBytesBitmapLine+((x) >> 3)]|=((0x80 >> ((x) & 7))))
#define ClearBitMap2PointXY(x,y) (BitmapBuf2[(y)*NrBytesBitmapLine+((x) >> 3)]&=(~(0x80 >> ((x) & 7))))

#define MaxPoints       4096

typedef int32 Int32Array[8192];
typedef uint8 BYTEArray[8192];

typedef struct
{
	float x, y;
	int32 NetNr, Info, Index, Count;
} PointObjectRecord;

typedef struct
{
	int32 Width, Height, Layer, DrillLayer, Info, NrLineBytes, BitmapScaled;
	double Rotation, Scale;
	int32 Unused[12];
	float Xoffset, Yoffset, StandardResolution, HResolution, VResolution, Other;
	float CenterX, CenterY;
	char FileName[200];
} SpecialBitmapRecord;

typedef PointObjectRecord PointObjectArray[DefMaxNrObjects];

uint8 bits1[8] = { 0xff, 0x7f, 0x3f, 0x1f, 0x0f, 0x07, 0x03, 0x01 };
uint8 bits2[8] = { 0x00, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe };
uint8 bits3[8] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };
uint8 bits4[8] = { 0x00, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe };


ObjectRecord NewObject;
ObjectLineRecord NewObjectLine;

int32 BitMapNetNr, NrTempPoints, NrPointObjects, PixelsX, PixelsY;

int32 TempUnits, NrBytesBitmapLine, MaxNrBitmapLines;

float BitMapMinX, BitMapMinY, StartX;
float BitMapMaxX, BitMapMaxY, StartY;

float CheckPenSize, StandardResolution, HResolution, VResolution;
int32 Printing;


uint8 *BitmapBuf1, *BitmapBuf2, *BitmapMem;

SpecialBitmapRecord SpecialBitmap;
ObjectPolygonRecord *CharPolygon;

extern HDC OutputDisplay;
extern HWND SelectLayerWindow;
extern int32 NrWarnings;
extern uint8 *TempMem3;

int32 AllocateMemTemp3(int32 MemSize);


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WriteBitmapBuf2ToFile(LPSTR FileStr)
{
	BmpHeaderRecord BmpHeader;
	int fp, res, result;
	uint8 Zeros[8] = { 0, 0, 0, 0, 0xff, 0xff, 0xff, 0 };

	if ((fp = FileOpenWriteUTF8(FileStr)) > 0)
	{
		memset(&BmpHeader, 0, sizeof(BmpHeader));
		res = sizeof(BmpHeader);
		BmpHeader.Identifier = 19778;
		BmpHeader.StartOfDataOffset = 62;
		BmpHeader.BitmapHeaderSize = 40;
		BmpHeader.Width = NrBytesBitmapLine * 8;
		BmpHeader.Height = MaxNrBitmapLines;
		BmpHeader.NrOfPlanes = 1;
		BmpHeader.BitsPerPixel = 1;
		BmpHeader.BitmapDataSize = NrBytesBitmapLine * MaxNrBitmapLines;
		BmpHeader.FileSize = sizeof(BmpHeader) + 8 + NrBytesBitmapLine * MaxNrBitmapLines;
		BmpHeader.NrColors1 = 2;
		BmpHeader.NrImportedColors = 2;
		BmpHeader.HResolutionInPixelsPerMeter = 11811;
		BmpHeader.VResolutionInPixelsPerMeter = 11811;

		FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
		FileWrite(fp, &Zeros, 8, &result);
		FileWrite(fp, BitmapBuf2, NrBytesBitmapLine * MaxNrBitmapLines, &result);
		FileClose(fp);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BitMapPoints(int32 mainx, int32 mainy, int32 mode)
{
	int32 cntx, cnty, PreviousPoint, CurrentPoint, NextPoint;

//  memset(&Buf[128*1024],0,128*1024);
#ifdef _DEBUG

	if (mode == 1)
	{
		for (cnty = 1; cnty < 1023; cnty++)
		{
			for (cntx = 1; cntx < 1023; cntx++)
			{
				if (BitMapXY(cntx, cnty) != 0)
					SetBitMap2PointXY(mainx, mainy, cntx, cnty);
			}
		}
	}

#endif


	for (cnty = 1; cnty < 1023; cnty++)
	{
		PreviousPoint = 0;
		CurrentPoint = 0;
		NextPoint = 0;

		if (BitMapXY(0, cnty) != 0)
			PreviousPoint = 1;

		if (BitMapXY(1, cnty) != 0)
			CurrentPoint = 1;

		if (BitMapXY(2, cnty) != 0)
			NextPoint = 1;

		for (cntx = 1; cntx < 1023; cntx++)
		{
			if (CurrentPoint == 0)
			{
				if (PreviousPoint == 1)
					SetBitMap2PointXY(mainx, mainy, cntx, cnty);
				else
				{
					if (NextPoint == 1)
						SetBitMap2PointXY(mainx, mainy, cntx, cnty);
				}
			}

			PreviousPoint = CurrentPoint;
			CurrentPoint = NextPoint;
			NextPoint = 0;

			if (cntx < 1022)
			{
				if (BitMapXY(cntx + 2, cnty) != 0)
					NextPoint = 1;
			}
		}
	}

// *******************************************************************************************************

	for (cntx = 1; cntx < 1023; cntx++)
	{
		PreviousPoint = 0;
		CurrentPoint = 0;
		NextPoint = 0;

		if (BitMapXY(cntx, 0) != 0)
			PreviousPoint = 1;

		if (BitMapXY(cntx, 1) != 0)
			CurrentPoint = 1;

		if (BitMapXY(cntx, 2) != 0)
			NextPoint = 1;

		for (cnty = 1; cnty < 1023; cnty++)
		{
			if (CurrentPoint == 0)
			{
				if (PreviousPoint == 1)
					SetBitMap2PointXY(mainx, mainy, cntx, cnty);
				else
				{
					if (NextPoint == 1)
						SetBitMap2PointXY(mainx, mainy, cntx, cnty);
				}
			}

			PreviousPoint = CurrentPoint;
			CurrentPoint = NextPoint;
			NextPoint = 0;

			if (cnty < 1022)
			{
				if (BitMapXY(cntx, cnty + 2) != 0)
					NextPoint = 1;
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BitMapPoints2(int32 mode)
{
	int32 cntx, cnty, PreviousPoint, CurrentPoint, NextPoint;

//  memset(&Buf[128*1024],0,128*1024);

	for (cnty = 1; cnty < 1023; cnty++)
	{
		PreviousPoint = 0;
		CurrentPoint = 0;
		NextPoint = 0;

		if (BitMapXY(0, cnty) != 0)
			PreviousPoint = 1;

		if (BitMapXY(1, cnty) != 0)
			CurrentPoint = 1;

		if (BitMapXY(2, cnty) != 0)
			NextPoint = 1;

		for (cntx = 1; cntx < 1023; cntx++)
		{
			if (CurrentPoint == 0)
			{
				if (PreviousPoint == 1)
					SetBitMap3PointXY(cntx, cnty);
				else
				{
					if (NextPoint == 1)
						SetBitMap3PointXY(cntx, cnty);
				}
			}

			PreviousPoint = CurrentPoint;
			CurrentPoint = NextPoint;
			NextPoint = 0;

			if (cntx < 1022)
			{
				if (BitMapXY(cntx + 2, cnty) != 0)
					NextPoint = 1;
			}
		}
	}

// *******************************************************************************************************

	for (cntx = 1; cntx < 1023; cntx++)
	{
		PreviousPoint = 0;
		CurrentPoint = 0;
		NextPoint = 0;

		if (BitMapXY(cntx, 0) != 0)
			PreviousPoint = 1;

		if (BitMapXY(cntx, 1) != 0)
			CurrentPoint = 1;

		if (BitMapXY(cntx, 2) != 0)
			NextPoint = 1;

		for (cnty = 1; cnty < 1023; cnty++)
		{
			if (CurrentPoint == 0)
			{
				if (PreviousPoint == 1)
					SetBitMap3PointXY(cntx, cnty);
				else
				{
					if (NextPoint == 1)
						SetBitMap3PointXY(cntx, cnty);
				}
			}

			PreviousPoint = CurrentPoint;
			CurrentPoint = NextPoint;
			NextPoint = 0;

			if (cnty < 1022)
			{
				if (BitMapXY(cntx, cnty + 2) != 0)
					NextPoint = 1;
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

__inline int32 CheckBitMap2XY(int32 x, int32 y)
{
	if (((x) < 0) || ((y) < 0) || ((x) >= NrBytesBitmapLine * 8) || ((y) >= MaxNrBitmapLines))
		return 0;

	if ((BitmapBuf2[(y) * NrBytesBitmapLine + ((x) >> 3)] & (0x80 >> ((x) & 7))) != 0)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckBitMap2XYa(int32 x, int32 y)
{
	if (((x) < 0) || ((y) < 0) || ((x) >= NrBytesBitmapLine * 8) || ((y) >= MaxNrBitmapLines))
		return 0;

	if ((BitmapBuf2[(y) * NrBytesBitmapLine + ((x) >> 3)] & (0x80 >> ((x) & 7))) != 0)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetPixelNeighbours(int32 x, int32 y)
{
	return (CheckBitMap2XYa(x, y + 1) + CheckBitMap2XYa(x + 1, y + 1) + CheckBitMap2XYa(x + 1, y) +
	        CheckBitMap2XYa(x + 1, y - 1) + CheckBitMap2XYa(x, y - 1) + CheckBitMap2XYa(x - 1,
	                y - 1) + CheckBitMap2XYa(x - 1,
	                        y) +
	        CheckBitMap2XYa(x - 1, y + 1));

}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BitmapPointsToLines3(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt3, cntx, cnty, x, y, x1, y1, x2, y2, res, ok, StartPointX, StartPointY, NrPointsFound, NrTries,
	      PosX, PreviousPointX, PreviousPointY, PPreviousPointX, PPreviousPointY, PixelNeighbours, NextDirection,
	      FirstObjectNr, count, NotClosedLines, Direction, NextDirectionPoint[8], fp;
	int32 StartSequence[8] = { 2, 0, 1, 7, 4, 3, 5, 6 };
	int32 Sequences[64] = { 0, 1, 7, 2, 6, 3, 5, -1,
	                        1, 0, 2, 3, 7, 4, 6, -1,
	                        2, 1, 3, 0, 4, 5, 7, -1,
	                        3, 2, 4, 1, 5, 0, 6, -1,
	                        4, 3, 5, 2, 6, 1, 7, -1,
	                        5, 4, 6, 3, 7, 2, 0, -1,
	                        6, 5, 7, 0, 4, 1, 3, -1,
	                        7, 0, 6, 1, 5, 2, 4, -1
	                      };
	int32 DirectionIncX[8] = { 0, 1, 1, 1, 0, -1, -1, -1 };
	int32 DirectionIncY[8] = { 1, 1, 0, -1, -1, -1, 0, 1 };
	int32 PixelNeighboursX[128];
	int32 PixelNeighboursY[128];
	int32 NrPixelNeighbours, PixelNeighboursAroundPixel[8];

	Int32Array *FoundPointsX, *FoundPointsY;
//  Int32Array *FoundPointsX,*FoundPointsY,*FoundPointsX2,*FoundPointsY2;
	BYTEArray *PointInfo;
	ObjectPolygonRecord *Polygon;
	char str[200];
	double dx, dy, dx2, dy2, minx, maxx, miny, maxy;
	int32 Stop, Stop2, FoundBit, ClosedLine, EndPointReached;

	FoundPointsX = (Int32Array *) & Buf[0];
	FoundPointsY = (Int32Array *) & Buf[8192 * sizeof(int32)];
	PointInfo = (BYTEArray *) & Buf[16384 * sizeof(int32)];
	Polygon = (ObjectPolygonRecord *) & Buf[24576 * sizeof(int32)];
	Polygon->Layer = INFO_LAYER;

// *******************************************************************************************************

	cnt = 0;
	cntx = 0;
	cnt3 = 0;
	PreviousPointX = 0;
	PreviousPointY = 0;
	cnt2 = MaxNrBitmapLines * NrBytesBitmapLine * 8;
	NrPixelNeighbours = 0;

	for (y = 1; y < MaxNrBitmapLines - 1; y++)
	{
		PosX = y * NrBytesBitmapLine;

		for (cntx = 0; cntx < NrBytesBitmapLine; cntx++)
		{
			if (BitmapBuf2[PosX + cntx] != 0)
			{
				cnt3++;

				for (x2 = 0; x2 < 8; x2++)
				{
					x = cntx * 8 + x2;

					if ((x > 0) && (x < NrBytesBitmapLine * 8 - 1) && (CheckBitMap2XY(x, y)))
					{
						cnt++;
						PixelNeighbours =
						    CheckBitMap2XY(x, y + 1) + CheckBitMap2XY(x + 1, y + 1) + CheckBitMap2XY(x + 1,
						            y) +
						    CheckBitMap2XY(x + 1, y - 1) + CheckBitMap2XY(x, y - 1) + CheckBitMap2XY(x - 1,
						            y - 1) +
						    CheckBitMap2XY(x - 1, y) + CheckBitMap2XY(x - 1, y + 1);

						if (PixelNeighbours < 2)
						{
							ClearBitMap2PointXY(x, y);

							if (NrPixelNeighbours < 128)
							{
								PixelNeighboursX[NrPixelNeighbours] = x;
								PixelNeighboursY[NrPixelNeighbours] = y;
								NrPixelNeighbours++;
							}

							res = 1;
						}
					}
				}
			}
		}
	}

	res = 1;

//  Beep(1000,100);
	for (cnt = 0; cnt < NrPixelNeighbours; cnt++)
	{
		Stop = 0;
		x1 = max(PixelNeighboursX[cnt] - 50, 1);
		x2 = min(PixelNeighboursX[cnt] + 50, NrBytesBitmapLine * 8 - 1);
		y1 = max(PixelNeighboursY[cnt] - 50, 1);
		y2 = min(PixelNeighboursY[cnt] + 50, MaxNrBitmapLines - 1);
		NrTries = 0;

		while ((!Stop) && (NrTries < 100))
		{
			cnt2 = 0;

			for (y = y1; y < y2; y++)
			{
				for (x = x1; x < x2; x++)
				{
					if (CheckBitMap2XY(x, y))
					{
						PixelNeighbours = GetPixelNeighbours(x, y);

						if (PixelNeighbours < 2)
						{
							ClearBitMap2PointXY(x, y);
							cnt2++;
						}
					}
				}
			}

			if (cnt2 == 0)
				Stop = 1;

			NrTries++;
		}

		if (NrTries == 100)
			res = 1;

		if (NrTries > 3)
			res = 1;
	}

//  Beep(4000,100);

#ifdef _DEBUG

	if (1)
		WriteBitmapBuf2ToFile("e:\\pcb_elegance\\roundings2.bmp");

#endif


// *******************************************************************************************************


//  DeAllocateMemBitmapPolygons();
	memset(Polygon, 0, sizeof(ObjectPolygonRecord));
	BitMapNetNr = 0;
	Stop = 0;
	NotClosedLines = 0;
	cnty = 1;
	NrTries = 0;

	while (!Stop)
	{
		FoundBit = 0;

		while ((cnty < MaxNrBitmapLines) && (!FoundBit))
		{
			cntx = 0;

			while ((cntx < NrBytesBitmapLine) && (!FoundBit))
			{
				if (BitmapBuf2[cnty * NrBytesBitmapLine + cntx] != 0)
					FoundBit = 1;
				else
					cntx++;
			}

			if (!FoundBit)
				cnty++;
		}

		if (FoundBit)
		{
			cntx *= 8;

			x = 0;

			while ((BitMap2XY(cntx + x, cnty) == 0) && (x < 8))
				x++;

			cntx += x;
			/*
			      px=(float)cntx*StandardResolution;
			      py=(float)cnty*StandardResolution;
			#ifdef _DEBUG
			      if ((InRangeInt2(cntx,1199))
			         &&
			         (InRangeInt2(cnty,1238))) {
			        res=1;
			      }
			      if (NrTries==1) {
			        res=1;
			      }
			      if ((InRange10(px,(float)68.95e5))
			         &&
			         (InRange10(py,(float)64.0e5))) {
			        res=1;
			      }
			#endif
			*/
			StartPointX = cntx;
			StartPointY = cnty;
			x = cntx;
			y = cnty;
			ok = 1;
			ClosedLine = 0;
			Direction = -1;
			NrPointsFound = 0;

			(*FoundPointsX)[NrPointsFound] = x;
			(*FoundPointsY)[NrPointsFound] = y;
			NrPointsFound++;
			EndPointReached = 0;

			while (!EndPointReached)
			{
#ifdef _DEBUG

				if ((x == 1705) && (y == 868))
					res = 1;

#endif
				NextDirectionPoint[0] = CheckBitMap2XY(x, y + 1);
				NextDirectionPoint[1] = CheckBitMap2XY(x + 1, y + 1);
				NextDirectionPoint[2] = CheckBitMap2XY(x + 1, y);
				NextDirectionPoint[3] = CheckBitMap2XY(x + 1, y - 1);
				NextDirectionPoint[4] = CheckBitMap2XY(x, y - 1);
				NextDirectionPoint[5] = CheckBitMap2XY(x - 1, y - 1);
				NextDirectionPoint[6] = CheckBitMap2XY(x - 1, y);
				NextDirectionPoint[7] = CheckBitMap2XY(x - 1, y + 1);

				if (0)
					ClearBitMap2PointXY(x, y);

				PixelNeighboursAroundPixel[0] = GetPixelNeighbours(x, y + 1);
				PixelNeighboursAroundPixel[1] = GetPixelNeighbours(x + 1, y + 1);
				PixelNeighboursAroundPixel[2] = GetPixelNeighbours(x + 1, y);
				PixelNeighboursAroundPixel[3] = GetPixelNeighbours(x + 1, y - 1);
				PixelNeighboursAroundPixel[4] = GetPixelNeighbours(x, y - 1);
				PixelNeighboursAroundPixel[5] = GetPixelNeighbours(x - 1, y - 1);
				PixelNeighboursAroundPixel[6] = GetPixelNeighbours(x - 1, y);
				PixelNeighboursAroundPixel[7] = GetPixelNeighbours(x - 1, y + 1);

				if (0)
					SetBitMap3PointXY(x, y);


				if (Direction == -1)
				{
					cnt = 0;

					while ((cnt < 8) && (NextDirectionPoint[StartSequence[cnt]] == 0))
						cnt++;

					if (0)
					{
						while ((cnt < 8) && (NextDirectionPoint[StartSequence[cnt]] == 0)
						        && (PixelNeighboursAroundPixel[StartSequence[cnt]] == 0))
							cnt++;
					}

					if (cnt < 8)
						Direction = StartSequence[cnt];
					else
						EndPointReached = 1;

					NextDirection = Direction;
				}
				else
				{
					if (NextDirectionPoint[Direction] == 0)
					{
						cnt = 0;
						Stop2 = 0;

						while ((cnt < 7) && (!Stop2))
						{
							if (NextDirectionPoint[Sequences[Direction * 8 + cnt]] != 0)
							{
								if (PixelNeighboursAroundPixel[Sequences[Direction * 8 + cnt]] != 0)
									Stop2 = 1;
								else
									cnt++;
							}
							else
								cnt++;
						}

						if (cnt < 7)
							NextDirection = Sequences[Direction * 8 + cnt];
						else
						{
							cnt = 0;

							while ((cnt < 7) && (NextDirectionPoint[Sequences[Direction * 8 + cnt]] == 0))
								cnt++;

							if (cnt < 7)
								NextDirection = Sequences[Direction * 8 + cnt];
							else
								NextDirection = -1;
						}
					}
					else
						NextDirection = Direction;
				}

				if (!EndPointReached)
				{
					if (NextDirection != -1)
					{
						PPreviousPointX = PreviousPointX;
						PPreviousPointY = PreviousPointY;
						PreviousPointX = x;
						PreviousPointY = y;
						x += DirectionIncX[NextDirection];
						y += DirectionIncY[NextDirection];

						/*
						#ifdef _DEBUG
						            if ((x==20)
						               &&
						               (y==30)) {
						              ok=1;
						            }
						            dx2=(double)StartX+(double)x*StandardResolution;
						            dy2=(double)StartY+(double)y*StandardResolution;
						            if ((InRange(dx2,(float)30.8e5))
						               &&
						               (InRange(dy2,(float)58.125e5))) {
						              res=1;
						            }
						#endif
						*/
						if (NextDirection == Direction)
						{
							(*FoundPointsX)[NrPointsFound] = x;
							(*FoundPointsY)[NrPointsFound] = y;
						}

						if ((StartPointX == x) && (StartPointY == y))
						{
//              if (NextDirection!=Direction) {
							NrPointsFound++;
//              }
							ClosedLine = 1;

							if (NextDirection != Direction)
							{
								(*FoundPointsX)[NrPointsFound] = x;
								(*FoundPointsY)[NrPointsFound] = y;
								NrPointsFound++;
							}

							EndPointReached = 1;
						}

						/*
						#ifdef _DEBUG
						            if ((InRangeInt2(x,1630))
						               &&
						               (InRangeInt2(y,1512))) {
						              px=(float)x*StandardResolution;
						              py=(float)y*StandardResolution;
						              res=1;
						            }
						#endif
						*/
						ClearBitMap2PointXY(x, y);
					}

					if ((!EndPointReached) && (NextDirection != Direction))
					{
						NrPointsFound++;
						(*FoundPointsX)[NrPointsFound] = x;
						(*FoundPointsY)[NrPointsFound] = y;

						if (NextDirection == -1)
						{
							EndPointReached = 1;
							res = 1;
						}
						else
						{
							if (NrPointsFound == MaxPoints - 2)
								EndPointReached = 1;
						}

#ifdef _DEBUG

						if (NrPointsFound > NrTempPoints)
							NrTempPoints = NrPointsFound;

						/*
						            if (NrPointsFound==5) {
						              EndPointReached=1;
						              NrPointsFound--;
						            }
						*/
#endif
					}

					Direction = NextDirection;
				}
				else
				{
					/*
					#ifdef _DEBUG
					          if ((InRangeInt2(x,1630))
					             &&
					             (InRangeInt2(y,1512))) {
					            px=(float)x*StandardResolution;
					            py=(float)y*StandardResolution;
					            res=1;
					          }
					#endif
					*/
					ClearBitMap2PointXY(x, y);
				}
			}

			count = 0;
			FirstObjectNr = NrPointObjects;
#ifdef _DEBUG

			if (NrPointObjects == 567)
				ok = 1;

#endif

			if (!ClosedLine)
			{
				NotClosedLines++;
				(*FoundPointsX)[NrPointsFound] = -10000000;
				(*FoundPointsY)[NrPointsFound] = -10000000;
			}
			else
			{
				(*FoundPointsX)[NrPointsFound] = (*FoundPointsX)[0];
				(*FoundPointsY)[NrPointsFound] = (*FoundPointsY)[0];
			}

			/*
			#ifdef _DEBUG
			      dx2=(double)(*FoundPointsX)[0]*StandardResolution;
			      dy2=(double)(*FoundPointsY)[0]*StandardResolution;
			      if ((InRange(dx2,(float)45.625e5))
			         &&
			         (InRange(dy2,(float)30.5e5))) {
			        res=1;
			//        Stop=1;
			      }
			#endif
			*/
// *******************************************************************************************************

#ifdef _DEBUG

			if (0)
			{
				if ((fp = FileOpenWriteUTF8("e:\\viewplot\\lines.txt")) > 0)
				{
					for (cnt = 0; cnt < NrPointsFound; cnt++)
					{
						x1 = (*FoundPointsX)[cnt];
						y1 = (*FoundPointsY)[cnt];
//            sprintf(str,"Point %3i %2i %4i,%4i",cnt,(*PointInfo)[cnt],x1,y1);
						sprintf(str, "Point %3i %2i %4i,%4i", cnt, (*PointInfo)[cnt], x1 - 795, y1 - 134);
						WriteLn(fp, str);
					}

					FileClose(fp);
				}
			}

#endif

			ok = 1;
// *******************************************************************************************************
			dx = (double) (*FoundPointsX)[0] * StandardResolution;
			dy = (double) (*FoundPointsY)[0] * StandardResolution;
//      dx+=SpecialBitmap.Xoffset;
//      dy+=SpecialBitmap.Yoffset;
			Polygon->Points[0].x = dx;
			Polygon->Points[0].y = dy;
			minx = dx;
			miny = dy;
			maxx = dx;
			maxy = dy;

			Polygon->NrVertices = 1;
			cnt2 = 1;

			for (cnt = 1; cnt < NrPointsFound; cnt++)
			{
				if ((*FoundPointsX)[cnt] != 1000000)
				{
					dx2 = (double) (*FoundPointsX)[cnt] * StandardResolution;
					dy2 = (double) (*FoundPointsY)[cnt] * StandardResolution;
//          dx2+=SpecialBitmap.Xoffset;
//          dy2+=SpecialBitmap.Yoffset;
					minx = min(minx, dx2);
					miny = min(miny, dy2);
					maxx = max(maxx, dx2);
					maxy = max(maxy, dy2);
					Polygon->Points[cnt2].x = dx2;
					Polygon->Points[cnt2].y = dy2;
					dx = dx2;
					dy = dy2;
					Polygon->NrVertices++;
					cnt2++;
				}
			}

			if (ClosedLine)
			{
				BitMapNetNr++;
				Polygon->minx = minx;
				Polygon->miny = miny;
				Polygon->maxx = maxx;
				Polygon->maxy = maxy;
				Polygon->Layer = INFO_LAYER;

				if (BitMapNetNr > 1)
					Polygon->Layer = INFO_LAYER2;

				AddObjectPolygon(Polygon);
//        AddBitmapPolygon(Polygon,1);
			}
			else
				ok = 1;
		}
		else
			Stop = 1;

		NrTries++;
	}

//  if (NrBitmapPolygons==MaxNrBitmapPolygons) {
//  if (BitmapPolygonsMemory+MemSize>=MaxBitmapPolygonsMemory) {

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WriteToBitmapFile(LPSTR FileName, uint8 * Data, int32 PixelsX, int32 PixelsY)
{
	int fp, result;
	BmpHeaderRecord BmpHeader;
//  uint8 TwoColors[8]={255,255,255,0,0,0,0,0};
	uint8 TwoColors[8] = { 0, 0, 0, 0, 255, 255, 255, 0 };

	fp = FileOpenWriteUTF8(FileName);
	memset(&BmpHeader, 0, sizeof(BmpHeader));

	BmpHeader.Identifier = 0x4d42;
	BmpHeader.StartOfDataOffset = 62;
	BmpHeader.BitmapHeaderSize = 40;
	BmpHeader.Width = PixelsX;
	BmpHeader.Height = PixelsY;
	BmpHeader.NrOfPlanes = 1;
	BmpHeader.BitsPerPixel = 1;
	BmpHeader.BitmapDataSize = (PixelsX / 8) * PixelsY;
	BmpHeader.FileSize = sizeof(BmpHeader) + 8 + BmpHeader.BitmapDataSize;
	BmpHeader.NrColors1 = 2;
	BmpHeader.NrImportedColors = 2;
	BmpHeader.HResolutionInPixelsPerMeter = 11811;
	BmpHeader.VResolutionInPixelsPerMeter = 11811;


	FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
	FileWrite(fp, &TwoColors, 8, &result);

	FileWrite(fp, Data, BmpHeader.BitmapDataSize, &result);

	FileClose(fp);

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************




int32 ConvertBitmapToLines()
{

//BitmapBuf2

//  BitmapPointsToLines(HResolution,VResolution,PenSize,Layer,0);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BitMapPoints3(int32 NrBytesBitmapLine, int32 MaxNrBitmapLines, int32 mode)
{
	int32 cntx, cnty, PreviousPoint, CurrentPoint, NextPoint;

	for (cnty = 1; cnty < MaxNrBitmapLines - 2; cnty++)
	{
		PreviousPoint = 0;
		CurrentPoint = 0;
		NextPoint = 0;

		if (BitMap3XY(0, cnty) != 0)
			PreviousPoint = 1;

		if (BitMap3XY(1, cnty) != 0)
			CurrentPoint = 1;

		if (BitMap3XY(2, cnty) != 0)
			NextPoint = 1;

		for (cntx = 1; cntx < NrBytesBitmapLine * 8 - 2; cntx++)
		{
			if (CurrentPoint == 0)
			{
				if (PreviousPoint == 1)
					SetBitMap3PointXY(cntx, cnty);
				else
				{
					if (NextPoint == 1)
						SetBitMap3PointXY(cntx, cnty);
				}
			}

			PreviousPoint = CurrentPoint;
			CurrentPoint = NextPoint;
			NextPoint = 0;

			if (BitMap3XY(cntx + 2, cnty) != 0)
				NextPoint = 1;
		}
	}

// *******************************************************************************************************

	for (cntx = 1; cntx < NrBytesBitmapLine * 8 - 1; cntx++)
	{
		PreviousPoint = 0;
		CurrentPoint = 0;
		NextPoint = 0;

		if (BitMap3XY(cntx, 0) != 0)
			PreviousPoint = 1;

		if (BitMap3XY(cntx, 1) != 0)
			CurrentPoint = 1;

		if (BitMap3XY(cntx, 2) != 0)
			NextPoint = 1;

		for (cnty = 2; cnty < MaxNrBitmapLines - 2; cnty++)
		{
			if (CurrentPoint == 0)
			{
				if (PreviousPoint == 1)
					SetBitMap3PointXY(cntx, cnty);
				else
				{
					if (NextPoint == 1)
						SetBitMap3PointXY(cntx, cnty);
				}
			}

			PreviousPoint = CurrentPoint;
			CurrentPoint = NextPoint;
			NextPoint = 0;

			if (BitMap3XY(cntx, cnty + 2) != 0)
				NextPoint = 1;
		}
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 GetMillingPointsBitmap(int32 Layer, int32 mode)
{
	int32 cnt4;

	NrBytesBitmapLine = SpecialBitmap.NrLineBytes;
	MaxNrBitmapLines = SpecialBitmap.Height;
	DeAllocateMemTemp();
	DeAllocateMemTemp2();
	AllocateMemTemp(NrBytesBitmapLine * MaxNrBitmapLines);
	memset(TempMem, 0xff, MaxTempMemory);
	AllocateMemTemp2(NrBytesBitmapLine * MaxNrBitmapLines);
	BitmapBuf1 = TempMem;
	BitmapBuf2 = TempMem2;
	memmove(BitmapBuf1, BitmapMem, SpecialBitmap.Height * NrBytesBitmapLine);

	for (cnt4 = 0; cnt4 < NrBytesBitmapLine * MaxNrBitmapLines; cnt4++)
		BitmapBuf1[cnt4] ^= 0xff;

	memset(TempMem2, 0, MaxTempMemory2);
	BitMapPoints3(NrBytesBitmapLine, MaxNrBitmapLines, 0);
#ifdef _DEBUG

	if (1)
		WriteBitmapBuf2ToFile("e:\\temp\\roundings.bmp");

#endif
	BitmapPointsToLines3(Layer, 0);
	DeAllocateMemTemp();
	DeAllocateMemTemp2();
	return 0;
}

/****************************************************************************
 *  FUNCTION   : IntFromFixed
 *  RETURNS    : int value approximating the FIXED value.
 ****************************************************************************/
static int32 IntFromFixed(FIXED f)
{
	if (f.fract >= 0x8000)
		return (f.value + 1);
	else
		return (f.value);
}

/****************************************************************************
 *  FUNCTION   : fxDiv2
 *  RETURNS    : (val1 + val2)/2 for FIXED values
 ****************************************************************************/
static FIXED fxDiv2(FIXED fxVal1, FIXED fxVal2)
{
	long l;

	l = (*((int32 *) & (fxVal1)) + *((int32 *) & (fxVal2))) / 2;
	return (*(FIXED *) & l);
}

/****************************************************************************
 *
 *  FUNCTION   : QSpline2Polyline
 *
 *  PURPOSE    : Fake spline cracker.  All it really does is take
 *               the A, B, and C points and pretend they are a polyline,
 *               creating a very unsmooth curve.
 *
 *               In real life, the spline would be digitized.
 *
 *               The calculated points are stored in the given array.
 *
 *  RETURNS    : Number of points added to lpptPolygon.
 *
 *  BONUS INFO : Here is a description of an algorithm to implement
 *               this functionality.
 *
 *** To break up a parabola defined by three points (A,B,C) into straight
 *** line vectors given a maximium error. The maximum error is
 *** 1/resolution * 1/ERRDIV.
 ***
 ***
 ***         B *-_
 ***          /   `-_
 ***         /       `-_
 ***        /           `-_
 ***       /               `-_
 ***      /                   `* C
 ***   A *
 ***
 *** PARAMETERS:
 ***
 *** Ax, Ay contains the x and y coordinates for point A.
 *** Bx, By contains the x and y coordinates for point B.
 *** Cx, Cy contains the x and y coordinates for point C.
 *** hX, hY are handles to the areas where the straight line vectors are going to be put.
 *** count is pointer to a count of how much data has been put into *hX, and *hY.
 ***
 *** F (t) = (1-t)^2 * A + 2 * t * (1-t) * B + t * t * C, t = 0... 1 =>
 *** F (t) = t * t * (A - 2B + C) + t * (2B - 2A) + A  =>
 *** F (t) = alfa * t * t + beta * t + A
 *** Now say that s goes from 0...N, => t = s/N
 *** set: G (s) = N * N * F (s/N)
 *** G (s) = s * s * (A - 2B + C) + s * N * 2 * (B - A) + N * N * A
 *** => G (0) = N * N * A
 *** => G (1) = (A - 2B + C) + N * 2 * (B - A) + G (0)
 *** => G (2) = 4 * (A - 2B + C) + N * 4 * (B - A) + G (0) =
 ***           3 * (A - 2B + C) + 2 * N * (B - A) + G (1)
 ***
 *** D (G (0)) = G (1) - G (0) = (A - 2B + C) + 2 * N * (B - A)
 *** D (G (1)) = G (2) - G (1) = 3 * (A - 2B + C) + 2 * N * (B - A)
 *** DD (G)   = D (G (1)) - D (G (0)) = 2 * (A - 2B + C)
 *** Also, error = DD (G) / 8 .
 *** Also, a subdivided DD = old DD/4.
 ***
 ****************************************************************************/

int QSpline2Polyline(LPPOINT lpptPolygon, LPPOINTFX lppfSpline)
{
	// Skip over A point.  It is already in the polygon.
	lppfSpline++;

	// Store the B point.
	lpptPolygon->x = IntFromFixed(lppfSpline->x);
	lpptPolygon->y = IntFromFixed(lppfSpline->y);
	lppfSpline++;
	lpptPolygon++;

	// Store the C point.
	lpptPolygon->x = IntFromFixed(lppfSpline->x);
	lpptPolygon->y = IntFromFixed(lppfSpline->y);

	return (2);					// Two points added to polygon.
}


int QSpline2PolylineNew(LPPOINT lpptBuffer, LPPOINTFX lpqsPoints, int inGY, unsigned int *count, int nAscent);


/****************************************************************************
 *  FUNCTION   : DrawT2Outline
 *
 *  PURPOSE    : Decode the GGO_NATIVE outline, create a polypolygon from it,
 *               and draw it using PolyPolygon.  Color and relative
 *               positioning provided by caller.
 *
 *               Polygon is not actually returned as would be more common
 *               in real usage.  Also, an arbitrary size for the polygon
 *               is specified instead of actually expanding on a need-to-
 *               grow basis.
 *
 *               Error conditions are not handled.
 *
 *  RETURNS    : none.
 ****************************************************************************/
void DrawT2Outline(HDC hDC, LPTTPOLYGONHEADER lpHeader, DWORD size)
{
//  TTPOLYGONHEADER ;
//  TTPOLYCURVE ;
	LPTTPOLYGONHEADER lpStart;
	LPTTPOLYCURVE lpCurve;
	POINT pt[2000];
	int32 count[50];
	int32 cTotal = 0;			// Total number of points in polypolygon.
	int32 cInCurve;				// Number of points in current curve.
	int32 cCurves = 0;			// Number of curves in polypolygon.
	int cInSpline;				// Number of points in digitized spline curve.
	int32 iFirstCurve;			// Index to start point of first curve.
	int32 i = 0, cnt;
	int32 ok, i2;
	POINTFX spline[3];
	lpStart = lpHeader;

	while ((DWORD) lpHeader < (DWORD) (((LPSTR) lpStart) + size))
	{
		if (lpHeader->dwType == TT_POLYGON_TYPE)
		{
			cInCurve = 0;

			// Get to first curve.
			lpCurve = (LPTTPOLYCURVE) (lpHeader + 1);
			iFirstCurve = cTotal;

			while ((DWORD) lpCurve < (DWORD) (((LPSTR) lpHeader) + lpHeader->cb))
			{
				if (lpCurve->wType == TT_PRIM_LINE)
				{
					for (i = 0; i < lpCurve->cpfx; i++)
					{
						pt[cTotal].x = IntFromFixed(lpCurve->apfx[i].x);
						pt[cTotal].y = IntFromFixed(lpCurve->apfx[i].y);
						cTotal++;
						cInCurve++;
					}
				}
				else if (lpCurve->wType == TT_PRIM_QSPLINE)
				{
					// **********************************************
					// Format assumption:
					//   The bytes immediately preceding a POLYCURVE
					//   structure contain a valid POINTFX.
					//
					//   If this is first curve, this points to the
					//      pfxStart of the POLYGONHEADER.
					//   Otherwise, this points to the last point of
					//      the previous POLYCURVE.
					//
					//   In either case, this is representative of the
					//      previous curve's last point.
					// **********************************************
					spline[0] = *(LPPOINTFX) ((LPSTR) lpCurve - sizeof(POINTFX));

					for (i = 0; i < lpCurve->cpfx;)
					{
						// The B point.
						spline[1] = lpCurve->apfx[i++];

						// Calculate the C point.
						if (i == (lpCurve->cpfx - 1))
							spline[2] = lpCurve->apfx[i++];
						else
						{
							// C is midpoint between B and next point.
							spline[2].x = fxDiv2(lpCurve->apfx[i - 1].x, lpCurve->apfx[i].x);
							spline[2].y = fxDiv2(lpCurve->apfx[i - 1].y, lpCurve->apfx[i].y);
						}

						cInSpline = 0;
						QSpline2PolylineNew((LPPOINT) & (pt[cTotal]), spline, -2, &cInSpline, 0);
						cTotal += (uint16) cInSpline;
						cInCurve += (uint16) cInSpline;
//              cInSpline = QSpline2Polyline((LPPOINT)&(pt[cTotal]), spline);
//              cTotal += cInSpline;
//              cInCurve += cInSpline;

						// New A point for next slice of spline.
						spline[0] = spline[2];
					}
				}
				else
					ok = 1;

				// Move on to next curve.
				lpCurve = (LPTTPOLYCURVE) & (lpCurve->apfx[i]);
			}

			// Add points to close curve.
			// Depending on the specific font and glyph being used, these
			// may not always be needed, but it never hurts.
			pt[cTotal].x = lpHeader->pfxStart.x.value;
			pt[cTotal].y = lpHeader->pfxStart.y.value;
			cInCurve++;
			cTotal++;
			/*
			      pt[cTotal].x = pt[iFirstCurve].x;
			      pt[cTotal].y = pt[iFirstCurve].y;
			      cInCurve++;
			      cTotal++;
			*/
			count[cCurves++] = cInCurve;

			// Move on to next polygon.
			lpHeader = (LPTTPOLYGONHEADER) (((LPSTR) lpHeader) + lpHeader->cb);
		}
		else
			ok = 1;
	}

#if 1
	ok = 1;
	cnt = 0;

	for (i = 0; i < cCurves; i++)
	{
		CharPolygon->NrVertices = count[i];

		for (i2 = 0; i2 < count[i]; i2++)
		{
			CharPolygon->Points[i2].x = pt[cnt + i2].x * 1000.0;
			CharPolygon->Points[i2].y = pt[cnt + i2].y * 1000.0;
		}

		switch (i)
		{
		case 0:
		default:
			CharPolygon->Layer = INFO_LAYER;
			break;

		case 1:
			CharPolygon->Layer = INFO_LAYER2;
			break;

		case 2:
			CharPolygon->Layer = INFO_LAYER3;
			break;

		case 3:
			CharPolygon->Layer = INFO_LAYER4;
			break;
		}

		AddObjectPolygon(CharPolygon);

		if (i == 0)
		{
//      ObjectPolygon=(ObjectPolygonRecord *)&(ObjectPolygonMem[(*ObjectPolygons)[Design.NrObjectPolygons-1]]);
		}
		else
		{
		}

		cnt += count[i];
	}

	RePaint();
#else
	ok = 1;

	// flip coordinates to get glyph right side up (Windows coordinates)
	for (i = 0; i < cTotal; i++)
	{
		pt[i].y = 800 - pt[i].y;
		pt[i].x += 100;
	}

	cnt = 0;

	for (i = 0; i < cCurves; i++)
	{
		Polygon(hDC, &pt[cnt], count[i]);
		cnt += count[i];
	}

#endif
//  Polygon(hDC,pt,53);
//  Polygon(hDC,&pt[53],4);
//  res=PolyPolygon(hDC, pt, count, cCurves);
	/*
	  MoveToEx(hDC,pt[0].x,pt[0].y,NULL);
	  for (i = 1; i < cTotal; i++) {
	    LineTo(hDC,pt[i].x,pt[i].y);
	  }
	*/
	ok = 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void GetFontPolygon(int32 mode)
{
	int32 count, ok;
	HFONT PinTextFont;
	uint8 InfoMem[16384];
	int32 res;
	WCHAR TextChar;
	GLYPHMETRICS GlyphMetrics;
	MAT2 TransFormation;


	HPEN EmptyPen = CreatePen(PS_NULL, 0, 0);
	/*
	  PinTextFont=CreateFont(1200,0,0,0,0,0,0,0,ANSI_CHARSET,
	                         OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
	                         FIXED_PITCH,"Courier New");
	  PinTextFont=CreateFont(1200,0,0,0,0,0,0,0,ANSI_CHARSET,
	                         OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY,
	                         DEFAULT_PITCH,"Arial");
	*/
	PinTextFont =
	    CreateFont(1200, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
	               DEFAULT_PITCH, "Arial Unicode MS");
	AllocateMemTemp3(128 * 1024);
	CharPolygon = (ObjectPolygonRecord *) TempMem3;
	CharPolygon->Layer = INFO_LAYER;
//  memset(&GlyphMetrics,0,sizeof(GlyphMetrics));
	memset(&TransFormation, 0, sizeof(TransFormation));
	TransFormation.eM11.value = 1;
	TransFormation.eM22.value = 1;
	/*
	  GlyphMetrics.gmBlackBoxX=10;
	  GlyphMetrics.gmBlackBoxY=10;
	  GlyphMetrics.gmCellIncX=10;
	  GlyphMetrics.gmCellIncY=10;
	*/
// DrawWindowMinX,(int32)DrawWindowMinY,(int32)DrawWindowMaxX,(int32)DrawWindowMaxY
	StartDrawingEditingWindow();
	SelectObject(OutputDisplay, PinTextFont);
	SelectObject(OutputDisplay, GetStockObject(WHITE_BRUSH));
	SelectObject(OutputDisplay, EmptyPen);
//  SelectObject(OutputDisplay,GetStockObject(WHITE_PEN));
	SetROP2(OutputDisplay, R2_XORPEN);
//  SetTextColor(OutputDisplay,RGB(255,255,255)); // White
//  TextChar='o';
	TextChar = 32332;
	count = GetGlyphOutlineW(OutputDisplay, TextChar, GGO_NATIVE, &GlyphMetrics, 0, NULL, &TransFormation);
	res = GetGlyphOutlineW(OutputDisplay, TextChar, GGO_NATIVE, &GlyphMetrics, count, &InfoMem, &TransFormation);

	/*
	  Rect.left=0;
	  Rect.top=0;
	  Rect.right=100;
	  Rect.bottom=100;
	  FillRect(OutputDisplay,&Rect,GetStockObject(WHITE_BRUSH));
	*/
//  Rectangle(OutputDisplay,0,0,600,600);
	DrawT2Outline(OutputDisplay, (LPTTPOLYGONHEADER) & InfoMem, count);
	DeleteObject(EmptyPen);
	ok = 1;

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void GetFontPolygon2(int32 mode)
{
	int32 PixelsX, PixelsY, ok;
	char str[MAX_LENGTH_STRING];
	HBITMAP ViewBitmap;
	HFONT PinTextFont, SimpleTextFont;
	BITMAPINFO *BitmapInfo;
	uint8 BitmapInfoMem[1024];
	BITMAPV4HEADER *BitmapHeader;
	int32 res;
	RECT Rect;
	SIZE TextSize;
	HDC BitmapDisplay;

	memset(&BitmapInfoMem, 0, sizeof(BitmapInfoMem));
	BitmapInfo = (BITMAPINFO *) BitmapInfoMem;
	BitmapHeader = (BITMAPV4HEADER *) & BitmapInfo->bmiHeader;
	BitmapHeader->bV4Size = sizeof(BITMAPV4HEADER);
	BitmapHeader->bV4Planes = 1;
	BitmapHeader->bV4BitCount = 1;
	BitmapHeader->bV4V4Compression = BI_RGB;

	BitmapDisplay = CreateCompatibleDC(0);

	PixelsX = GetDeviceCaps(BitmapDisplay, HORZRES);
	PixelsY = GetDeviceCaps(BitmapDisplay, VERTRES);
	BitmapHeader->bV4Width = PixelsX;
	BitmapHeader->bV4Height = PixelsY;
	SetBkMode(BitmapDisplay, TRANSPARENT);
	SetROP2(BitmapDisplay, R2_COPYPEN);

	SpecialBitmap.Width = PixelsX;
	SpecialBitmap.Height = PixelsY;
	NrBytesBitmapLine = PixelsX / 8;
	MaxNrBitmapLines = PixelsY;

	PinTextFont =
	    CreateFont(PixelsY, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
	               FIXED_PITCH, "Courier New");
	SimpleTextFont =
	    CreateFont(-12, 0, 0, 0, FW_DONTCARE, 0, 0, 0, OEM_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
	               DEFAULT_QUALITY, DEFAULT_PITCH, "SansSerif");

	AllocateMemTemp(NrBytesBitmapLine * MaxNrBitmapLines);

	ViewBitmap = CreateCompatibleBitmap(BitmapDisplay, PixelsX, PixelsY);
	SelectObject(BitmapDisplay, ViewBitmap);


	Rect.left = 0;
	Rect.top = 0;
	Rect.right = PixelsX;
	Rect.bottom = PixelsY;
	FillRect(BitmapDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	StandardResolution = (float) (100000.0 / PixelsY);
	SelectObject(BitmapDisplay, PinTextFont);
//  strcpy(str,"A");
	strcpy(str, "o");
	SetTextColor(BitmapDisplay, RGB(0, 0, 0));	// Black
	GetTextExtentPoint32(BitmapDisplay, str, 1, &TextSize);
	TextOut(BitmapDisplay, 0, 0, str, 1);
	AllocateMemTemp2(NrBytesBitmapLine * MaxNrBitmapLines);
	res = GetDIBits(BitmapDisplay, ViewBitmap, 0, PixelsY, TempMem, BitmapInfo, DIB_RGB_COLORS);
	BitmapBuf1 = TempMem;
	BitmapBuf2 = TempMem2;
	WriteToBitmapFile("e:\\new\\pcb\\test.bmp", BitmapBuf1, PixelsX, PixelsY);
//  for (cnt4=0;cnt4<NrBytesBitmapLine*MaxNrBitmapLines;cnt4++) BitmapBuf1[cnt4]^=0xff;
	memset(TempMem2, 0, MaxTempMemory2);
	BitMapPoints3(NrBytesBitmapLine, MaxNrBitmapLines, 0);
	WriteToBitmapFile("e:\\new\\pcb\\test2.bmp", BitmapBuf2, PixelsX, PixelsY);
#ifdef _DEBUG

	if (1)
	{
//    WriteBitmapBuf2ToFile("e:\\temp\\roundings.bmp");
	}

#endif
	BitmapPointsToLines3(0, 0);


	DeleteObject(ViewBitmap);
	DeleteDC(BitmapDisplay);


	DeAllocateMemTemp();
	ok = 1;

	return;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
