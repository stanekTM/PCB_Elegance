/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw.c
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
#include "graphics.h"
#include "windows.h"
#include "memory.h"
#include "draw.h"
#include "draw2.h"
#include "calcdef.h"
#include "line2.h"
#include "rect.h"
#include "geom.h"
#include "math.h"
#include "calc.h"
#include "ellipss.h"
#include "string.h"
#include "toets.h"
#include "mainloop.h"

double ViewMinX, ViewMinY, ViewMaxX, ViewMaxY;

extern int32 Printing;
extern COLORREF LineColor;
extern HGDIOBJ SpecialPen, SavePen, SaveBrush, EmptyBrush;
extern HDC OutputDisplay;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawInsertionPoint(int32 mode)
{
	if (mode == 0)
	{
		InitDrawingColorYellow2();
		SetROP2(OutputDisplay, R2_COPYPEN);
		DrawLine(-1, MultY(Shape.InsertionY), 4000, MultY(Shape.InsertionY));
		DrawLine(MultX(Shape.InsertionX), -10, MultX(Shape.InsertionX), 4000);
	}
	else
	{
		StartDrawingEditingWindow();
		InitDrawingColorYellow2();
		SetROP2(OutputDisplay, R2_XORPEN);
		DrawLine(-1, MultY(Shape.InsertionY), 4000, MultY(Shape.InsertionY));
		DrawLine(MultX(Shape.InsertionX), -10, MultX(Shape.InsertionX), 4000);
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawStrWithRotation(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror, char *str)
{
	char code, *str2;
	int32 cnt, cnt2, cnt4, NrPolyLines, lengte, count, oldx, oldy, hulp, res, xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4,
	      hx, hy;
	double x1d, y1d, lengte2, NewLine;
	double x1, y1, tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4, incX, incY;
	POINT LinePoints[20];
#ifdef _DEBUG
	int32 ok;
#endif

	GetMinMaxText(x, y, Size, 0, Rotation, Alignment, Mirror, str);

#ifdef _DEBUG

	if (InRange(Rotation, 135.0))
		ok = 1;

	if (stricmp(str, "NO DIVING") == 0)
		ok = 1;

#endif


	hulp = Mult(Size);

	if ((hulp < 8) && (ReverseY))
	{
		if ((InRange2(Rotation, 0.0)) || (InRange2(Rotation, 90.0)) || (InRange2(Rotation, 180.0))
		        || (InRange2(Rotation, 270.0)))
		{
			xx1 = MultX((TextMaxX + TextMinX) * 0.5);

			if (ReverseY)
				yy1 = MultY((TextMaxY + TextMinY) * 0.5);
			else
				yy1 = Mult(TextMinY - Yoffset);

			xx2 = Mult(TextMaxX - TextMinX) - 1;
			yy2 = Mult(TextMaxY - TextMinY) - 1;
			rect3(xx1, yy1, xx2, yy2);
			/*
			      if ((yy2<=2)
			         ||
			         (xx2<=2)) rect3(xx1,yy1,xx2-2,yy2-2);
			*/
		}
		else
		{
			GetMinMaxText(x, y, Size, 0, 0.0, Alignment, Mirror, str);
			tx1 = TextMinX;
			ty1 = TextMinY;
			tx2 = TextMaxX;
			ty2 = TextMinY;
			tx3 = TextMaxX;
			ty3 = TextMaxY;
			tx4 = TextMinX;
			ty4 = TextMaxY;

			if (Mirror == 1)
				Rotation = -Rotation;

			RotatePointFromOtherPoint2(&tx1, &ty1, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx2, &ty2, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx3, &ty3, x, y, Rotation);
			RotatePointFromOtherPoint2(&tx4, &ty4, x, y, Rotation);
			xx1 = MultX(tx1);

			if (ReverseY)
				yy1 = MultY(ty1);
			else
				yy1 = Mult(ty1 - Yoffset);

			xx2 = MultX(tx2);

			if (ReverseY)
				yy2 = MultY(ty2);
			else
				yy2 = Mult(ty2 - Yoffset);

			xx3 = MultX(tx3);

			if (ReverseY)
				yy3 = MultY(ty3);
			else
				yy3 = Mult(ty3 - Yoffset);

			xx4 = MultX(tx4);

			if (ReverseY)
				yy4 = MultY(ty4);
			else
				yy4 = Mult(ty4 - Yoffset);

			LinePoints[0].x = xx1;
			LinePoints[0].y = yy1;
			LinePoints[1].x = xx2;
			LinePoints[1].y = yy2;
			LinePoints[2].x = xx3;
			LinePoints[2].y = yy3;
			LinePoints[3].x = xx4;
			LinePoints[3].y = yy4;
			Polygon(OutputDisplay, LinePoints, 4);
		}

		return;
	}

	/*
	  LineSegments=TextStringToLineSegments(x,y,Size,Rotation,0,Mirror,str,(double *)&LineBuf);
	  SegmentCount=0;
	  for (cnt2=0;cnt2<LineSegments;cnt2++) {
	    xx1=MultX(LineBuf[SegmentCount]);
	    SegmentCount++;
	    yy1=MultY(LineBuf[SegmentCount]);
	    SegmentCount++;
	    xx2=MultX(LineBuf[SegmentCount]);
	    SegmentCount++;
	    yy2=MultY(LineBuf[SegmentCount]);
	    SegmentCount++;
	    MoveToEx(OutputDisplay,xx1,yy1,NULL);
	    LineTo(OutputDisplay,xx2,yy2);
	    SetPixel(OutputDisplay,xx2,yy2,LineColor);
	  }


	*/
	lengte2 = strlen(str);

	Size *= DefFontSize;
	incX = 0.9 * Size;
	incY = 0.0;
	RotatePoint2(&incX, &incY, Rotation);
	str2 = str;
	lengte = (int32) strlen(str);

	for (cnt4 = 0; cnt4 < lengte; cnt4++)
	{
		code = (*str2);
//    code='+';
		oldx = -100000000;
		oldy = -100000000;
		count = 0;

		if ((code >= 32) && (code < 127))
		{
			if (code > 32)
			{
				code -= 33;
				NrPolyLines = (*Chars)[code].NrPolyLines;
				cnt2 = 0;

				for (cnt = 0; cnt < NrPolyLines; cnt++)
				{
					count = 0;

					do
					{
						x1d = ((*Chars)[code].Line[cnt2 + 1]);
						y1d = ((*Chars)[code].Line[cnt2 + 2]);
						y1d -= 0.4;
						x1 = x1d * Size;
						y1 = y1d * Size;
						RotatePoint2(&x1, &y1, Rotation);

						if (Mirror == 1)
							x1 = -x1;

//            RotateFlipPoint(&x1,&y1,0.0,0.0,Rotation);
						hx = (Mult(x1 + x - Xoffset) + DrawWindowMinX);

						if (ReverseY)
							hy = (DrawWindowMaxY - Mult(y1 + y - Yoffset) - 1);
						else
							hy = (Mult(y1 + y - Yoffset));

						LinePoints[count].x = (int) hx;
						LinePoints[count].y = (int) hy;
						count++;
						cnt2 += 3;
						NewLine = ((*Chars)[code].Line[cnt2]);
					}
					while (InRange2(NewLine, 0.0));

					res = Polyline(OutputDisplay, (POINT *) & LinePoints, (int) count);
					SetPixel(OutputDisplay, (int) LinePoints[count - 1].x, (int) LinePoints[count - 1].y, LineColor);
				}
			}

			if (Mirror == 0)
			{
				x += incX;
				y += incY;
			}
			else
			{
				x -= incX;
				y += incY;
			}
		}

		str2++;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawFilledPolygon(PolygonRecord * FilledPolygon, int32 mode)
{
#define BUF_SIZE   16384
	typedef POINT PointsArray[200];

	int32 OkToDraw = 1;
	int32 cnt, x, y, count;
	PointsArray *Points;
	uint8 PolygonBuf[BUF_SIZE];

	Points = (PointsArray *) & PolygonBuf;

	count = FilledPolygon->NrVertices;

	if (count > BUF_SIZE / 8)
		return;

	if ((mode & 8) == 8)
		StartDrawingEditingWindow();

	/*
	  switch (mode & 7) {
	    case 4:
	      InitDrawingColorYellow2();
	      break;
	    case 5:
	      InitDrawingColorRed2();
	      break;
	    case 6:
	      InitDrawingColorBlue2();
	      break;
	    case 7:
	      InitDrawingColorGreen2();
	      break;
	  }
	*/
	for (cnt = 0; cnt < count; cnt++)
	{
		x = MultX((*FilledPolygon).Points[cnt].x);
		y = MultY((*FilledPolygon).Points[cnt].y);
		/*
		    if ((x<-32768)
		       ||
		       (x>32767)
		       ||
		       (y<-32768)
		       ||
		       (y>32767)) {
		      if (OperatingSystem!=VER_PLATFORM_WIN32_NT) {
		        OkToDraw=0;
		      }
		    }
		*/
		(*Points)[cnt].x = x;
		(*Points)[cnt].y = y;
	}

	if (OkToDraw)
		Polygon(OutputDisplay, (POINT *) Points, count);

	if ((mode & 8) == 8)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
