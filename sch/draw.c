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
#include "calc.h"
#include "line2.h"
#include "rect.h"
#include "sch.h"
#include "math.h"
#include "ellipss.h"
#include "string.h"
#include "toets.h"
#include "mainloop.h"
#include "commdlg.h"
#include "time.h"
#include "stdio.h"
#include "property.h"


double ViewMinX, ViewMinY, ViewMaxX, ViewMaxY;
extern COLORREF LineColor;
extern HFONT PrinterFont, PrinterFont_90;
extern HFONT PrinterFont2, PrinterFont2_90;
extern HFONT PrinterFont3, PrinterFont3_90;
extern int32 Font90;
extern int32 SelectColorMode;

int32 ok;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawWire(WireRecord * Wire, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, y2, Xmax, Xmin, Ymax, Ymin;
	int32 Info;
	int32 x1a, y1a, x2a, y2a;

	x1 = Wire->X1;
	y1 = Wire->Y1;
	x2 = Wire->X2;
	y2 = Wire->Y2;
	Info = Wire->Info;

	switch (Info & 3)
	{
	case 0:
	case 3:
		x1 += OX;
		y1 += OY;
		x2 += OX;
		y2 += OY;
		break;

	case 1:
		x1 += OX;
		y1 += OY;
		break;

	case 2:
		x2 += OX;
		y2 += OY;
		break;
	}

	if (x1 < x2)
	{
		Xmin = x1;
		Xmax = x2;
	}
	else
	{
		Xmin = x2;
		Xmax = x1;
	}

	if (y1 < y2)
	{
		Ymin = y1;
		Ymax = y2;
	}
	else
	{
		Ymin = y2;
		Ymax = y1;
	}

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1a = Mult(x1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;
		else
			y1a = Mult(y1 - Yoffset);

		x2a = Mult(x2 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y2a = DrawWindowMaxY - Mult(y2 - Yoffset) - 1;
		else
			y2a = Mult(y2 - Yoffset);

		InitDrawingWires(Mult(STANDARD_WIRE_THICKNESS));

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if ((x1 == x2) && (y1 == y2))
		{
//      ellips2(x1a,y1a,Mult(1.0),Mult(1.0),255);
		}
		else
			DrawLine(x1a, y1a, x2a, y2a);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawWires(int32 Mode)
{
	WireRecord *Wire;
	int32 cnt;

	for (cnt = 0; cnt < min(MaxNrWires, Design.NrWires); cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawWire(Wire, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawBus(BusRecord * Bus, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, y2, Xmax, Xmin, Ymax, Ymin;
	int32 Info;
	int32 x1a, y1a, x2a, y2a;

	x1 = Bus->X1;
	y1 = Bus->Y1;
	x2 = Bus->X2;
	y2 = Bus->Y2;
	Info = Bus->Info;

	switch (Info & 3)
	{
	case 0:
	case 3:
		x1 += OX;
		y1 += OY;
		x2 += OX;
		y2 += OY;
		break;

	case 1:
		x1 += OX;
		y1 += OY;
		break;

	case 2:
		x2 += OX;
		y2 += OY;
		break;
	}

	if (x1 < x2)
	{
		Xmin = x1;
		Xmax = x2;
	}
	else
	{
		Xmin = x2;
		Xmax = x1;
	}

	if (y1 < y2)
	{
		Ymin = y1;
		Ymax = y2;
	}
	else
	{
		Ymin = y2;
		Ymax = y1;
	}

	Xmin -= 0.5;
	Ymin -= 0.5;
	Xmax += 0.5;
	Ymax += 0.5;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		InitDrawingBusses(Mult(STANDARD_BUS_THICKNESS));
		x1a = Mult(x1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;
		else
			y1a = Mult(y1 - Yoffset);

		x2a = Mult(x2 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y2a = DrawWindowMaxY - Mult(y2 - Yoffset) - 1;
		else
			y2a = Mult(y2 - Yoffset);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if ((x1 == x2) && (y1 == y2))
		{
//      ellips2(x1a,y1a,Mult(1.0),Mult(1.0),255);
		}
		else
			DrawLine(x1a, y1a, x2a, y2a);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawBusses(int32 Mode)
{
	BusRecord *Bus;
	int32 cnt;

	for (cnt = 0; cnt < min(MaxNrBusses, Design.NrBusses); cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if ((Bus->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawBus(Bus, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawJunction(JunctionRecord * Junction, double OX, double OY, int32 Mode)
{
	double xx1, yy1, yy2, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, ThickNess, Info;

	xx1 = Junction->X + OX;
	yy1 = Junction->Y + OY;
//  dikte=Junction->ThickNess;
//  Drill=Junction->DrillThickNess;
	yy2 = 0.25;
	Xmin = xx1 - yy2;
	Ymin = yy1 - yy2;
	Xmax = xx1 + yy2;
	Ymax = yy1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		Info = Junction->Info;
		x1 = Mult(xx1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1 = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
		else
			y1 = Mult(yy1 - Yoffset);

		ThickNess = Mult(0.5);
		InitDrawingJunctions();

		if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if (!ReverseY)
			ellips2(x1, y1, ThickNess, ThickNess, 255);
		else
			ellips2(x1, y1, ThickNess, ThickNess, 255);

		if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawJunctions(int32 Mode)
{
	int32 cnt;
	JunctionRecord *Junction;

//  InitDrawingJunctions();
	if (!Printing)
		if (!Printing)
			SetROP2(OutputDisplay, R2_COPYPEN);

	for (cnt = 0; cnt < min(MaxNrJunctions, Design.NrJunctions); cnt++)
	{
		Junction = &((*Junctions)[cnt]);
#ifdef _DEBUG

		if (cnt == 25)
			ok = 1;

#endif

		if ((Junction->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawJunction(Junction, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawOnePinNet(OnePinNetRecord * OnePinNet, double OX, double OY, int32 Mode)
{
#define ONE_PIN_NET_SIZE   0.3
	double xx1, yy1, yy2, Xmax, Xmin, Ymax, Ymin;
	int32 x1, y1, x2, y2, Info;

	xx1 = OnePinNet->X + OX;
	yy1 = OnePinNet->Y + OY;
//  dikte=OnePinNet->ThickNess;
//  Drill=OnePinNet->DrillThickNess;
	yy2 = ONE_PIN_NET_SIZE;
	Xmin = xx1 - yy2;
	Ymin = yy1 - yy2;
	Xmax = xx1 + yy2;
	Ymax = yy1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		Info = OnePinNet->Info;
		InitDrawingOnePinNets(Mult(STANDARD_LINE_THICKNESS));

		if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		x1 = MultX(xx1 - ONE_PIN_NET_SIZE);

		if (ReverseY)
			y1 = MultY(yy1 - ONE_PIN_NET_SIZE);
		else
			y1 = Mult(yy1 - Yoffset - ONE_PIN_NET_SIZE);

		x2 = MultX(xx1 + ONE_PIN_NET_SIZE);

		if (ReverseY)
			y2 = MultY(yy1 + ONE_PIN_NET_SIZE);
		else
			y2 = Mult(yy1 - Yoffset + ONE_PIN_NET_SIZE);

		DrawLine(x1, y1, x2, y2);
		DrawLine(x1, y2, x2, y1);

		if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawOnePinNets(int32 Mode)
{
	int32 cnt;
	OnePinNetRecord *OnePinNet;

//  InitDrawingOnePinNets();
	if (!Printing)
		if (!Printing)
			SetROP2(OutputDisplay, R2_COPYPEN);

	for (cnt = 0; cnt < min(MaxNrOnePinNets, Design.NrOnePinNets); cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if ((OnePinNet->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawOnePinNet(OnePinNet, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawBusConnection(BusConnectionRecord * BusConnection, double OX, double OY, int32 Mode)
{
	double x, y, Xmax, Xmin, Ymax, Ymin, TextX, TextY;
	int32 d1, d2, d4, xx, yy, BusConnectionType, Info, TextAlignment;

	x = BusConnection->X + OX;
	y = BusConnection->Y + OY;
	TextX = BusConnection->TextX;
	TextY = BusConnection->TextY;
	TextAlignment = (BusConnection->Alignment & 0x0f);
	BusConnectionType = (BusConnection->Alignment >> 14) & 3;
	Info = BusConnection->Info;
//  switch (BusConnectionType) {
//    case 0:                            /*  \    */
//                                       /*  /    */
//      Xmin=x;
//      Xmax=x-BusSizeX;
//      break;
//    case 1:  /* rotate 90                  /\   */
//                                       /*       */
//      Xmin=x;
//      Xmax=x+BusSizeX;
//      break;
//    case 2:  /* rotate 180                 /    */
//                                       /*  \    */
//      Xmin=x;
//      Xmax=x+BusSizeX;
//      break;
//    case 3:  /* rotate 270                      */
//                                       /*  \/   */
//      Xmin=x;
//      Xmax=x+BusSizeX;
//      break;
//  }
	Xmin = x - 2.0;
	Xmax = x + 2.0;
	Ymin = y - 2.0;
	Ymax = y + 2.0;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		InitDrawingBusConnections(0);

		if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);

		xx = MultX(x);

		if (ReverseY)
			yy = MultY(y);
		else
			yy = Mult(y - Yoffset);

		switch (BusConnectionType)
		{
		case 0:				/*  \    */
			/*  /    */
			d1 = Mult(BusSizeX * 0.5);
			d2 = Mult(BusSizeX);
			d4 = Mult(BusSizeY) | 1;
			rect2(xx - d2, yy - (d4 / 2), d1 | 1, d4);
			InitDrawingBusConnections(Mult(STANDARD_LINE_THICKNESS));
			DrawLine(xx - d1 - 1, yy, xx, yy);
			break;

		case 1:				/* rotate 90   */
			/*  /\         */
			/*             */
			d1 = Mult(BusSizeX * 0.5);
			d2 = Mult(BusSizeX);
			d4 = Mult(BusSizeY) | 1;
			rect2(xx - (d4 / 2), yy + d1, d4, d1 | 1);
			InitDrawingBusConnections(Mult(STANDARD_LINE_THICKNESS));
			DrawLine(xx, yy + d1 - 1, xx, yy);
			break;

		case 2:				/* rotate 180  */
			/*  /          */
			/*  \          */
			d1 = Mult(BusSizeX * 0.5);
			d2 = Mult(BusSizeX);
			d4 = Mult(BusSizeY) | 1;
			xx = MultX(x);

			if (ReverseY)
				yy = MultY(y);
			else
				yy = Mult(y - Yoffset);

			rect2(xx + d1, yy - (d4 / 2), d1 | 1, d4);
			InitDrawingBusConnections(Mult(STANDARD_LINE_THICKNESS));
			DrawLine(xx + d1, yy, xx, yy);
			break;

		case 3:				/* rotate 270  */
			/*             */
			/*  \/         */
			d1 = Mult(BusSizeX * 0.5);
			d2 = Mult(BusSizeX);
			d4 = Mult(BusSizeY) | 1;
			rect2(xx - (d4 / 2), yy - d2, d4, d1 | 1);
			InitDrawingBusConnections(Mult(STANDARD_LINE_THICKNESS));
			DrawLine(xx, yy - d1, xx, yy);
			break;
		}

		if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawBusConnections(int32 Mode)
{
	BusConnectionRecord *BusConnection;
	int32 cnt;

	for (cnt = 0; cnt < min(MaxNrBusConnections, Design.NrBusConnections); cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawBusConnection(BusConnection, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawGlobalConnection(GlobalConnectionRecord * GlobalConnection, double OX, double OY, int32 Mode)
{
	double xx1, yy1, yy2, yy2a, Xmax, Xmin, Ymax, Ymin;
	int32 hulp, hulp2, hulpx, hulpy, Info, ConnectionType;
	POINT LinePoints[20];
	int32 TempBackGroundActive;

	Xmax = 0.0;
	Xmin = 0.0;
	Ymax = 0.0;
	Ymin = 0.0;

	TempBackGroundActive = BackGroundActive;
	Info = GlobalConnection->Info;

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)))
	{
		xx1 = GlobalConnection->X + OX;
		yy1 = GlobalConnection->Y + OY;
		ConnectionType = GlobalConnection->ConnectionType;

		switch (ConnectionType >> 1)
		{
		case 0:				// input
		case 1:				// output
			yy2 = 0.4;
			Ymin = yy1 - yy2;
			Ymax = yy1 + yy2;

			if ((ConnectionType & 1) == 0)
			{
				Xmin = xx1 - yy2 * 2.0;
				Xmax = xx1;
			}
			else
			{
				Xmin = xx1;
				Xmax = xx1 + yy2 * 2.0;
			}

			break;

		case 2:				// input/output
			yy2 = 0.4;
			yy2a = 1.8;
			Ymin = yy1 - yy2;
			Ymax = yy1 + yy2;

			if ((ConnectionType & 1) == 0)
			{
				Xmin = xx1 - yy2a;
				Xmax = xx1;
			}
			else
			{
				Xmin = xx1;
				Xmax = xx1 + yy2a;
			}

			break;
		}

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			InitDrawingGlobalConnections(0);

			if (((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)) && (Mode == 0))
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);

			if (ReverseY)
			{
				switch (ConnectionType >> 1)
				{
				case 0:		// input
					hulp = Mult(0.4);
					hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;
					hulpy = Mult(yy1 - Yoffset);
					LinePoints[0].x = 0;
					LinePoints[0].y = (int) (DrawWindowMaxY - hulpy - 1);
					LinePoints[1].x = (int) (-2 * hulp);
					LinePoints[1].y = (int) (DrawWindowMaxY - hulpy - hulp - 1);
					LinePoints[2].x = (int) (-2 * hulp);
					LinePoints[2].y = (int) (DrawWindowMaxY - hulpy + hulp - 1);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);
					break;

				case 1:		// output
					hulp = Mult(0.4);
					hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;
					hulpy = Mult(yy1 - Yoffset);
					LinePoints[0].x = 0;
					LinePoints[0].y = (int) (DrawWindowMaxY - hulpy - hulp - 1);
					LinePoints[1].x = 0;
					LinePoints[1].y = (int) (DrawWindowMaxY - hulpy + hulp - 1);
					LinePoints[2].x = (int) (-2 * hulp);
					LinePoints[2].y = (int) (DrawWindowMaxY - hulpy - 1);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);
					break;

				case 2:		// input/output
					hulp = Mult(0.4);
					hulp2 = Mult(2.0);
					hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;
					hulpy = Mult(yy1 - Yoffset);
					LinePoints[0].x = 0;
					LinePoints[0].y = (int) (DrawWindowMaxY - hulpy - 1);
					LinePoints[1].x = (int) (-2 * hulp);
					LinePoints[1].y = (int) (DrawWindowMaxY - hulpy - hulp - 1);
					LinePoints[2].x = (int) (-2 * hulp);
					LinePoints[2].y = (int) (DrawWindowMaxY - hulpy + hulp - 1);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);

					LinePoints[0].x = (int) (-hulp2);
					LinePoints[0].y = (int) (DrawWindowMaxY - hulpy - hulp - 1);
					LinePoints[1].x = (int) (-hulp2);
					LinePoints[1].y = (int) (DrawWindowMaxY - hulpy + hulp - 1);
					LinePoints[2].x = (int) (-hulp2 - hulp * 2);
					LinePoints[2].y = (int) (DrawWindowMaxY - hulpy - 1);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
						LinePoints[3].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);

					LinePoints[0].x = (int) (-hulp2);
					LinePoints[0].y = (int) (DrawWindowMaxY - hulpy - 1);
					LinePoints[1].x = (int) (-2 * hulp);
					LinePoints[1].y = (int) (DrawWindowMaxY - hulpy - 1);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					InitDrawingGlobalConnections(Mult(STANDARD_LINE_THICKNESS));
					Polyline(OutputDisplay, (POINT *) & LinePoints, 2);
					break;
				}
			}
			else
			{
				switch (ConnectionType >> 1)
				{
				case 0:		// input
					hulp = Mult(0.4);
					hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;
					hulpy = Mult(yy1 - Yoffset);
					LinePoints[0].x = 0;
					LinePoints[0].y = (int) (hulpy);
					LinePoints[1].x = (int) (-2 * hulp);
					LinePoints[1].y = (int) (hulpy - hulp);
					LinePoints[2].x = (int) (-2 * hulp);
					LinePoints[2].y = (int) (hulpy + hulp);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);
					break;

				case 1:		// output
					hulp = Mult(0.4);
					hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;
					hulpy = Mult(yy1 - Yoffset);
					LinePoints[0].x = 0;
					LinePoints[0].y = (int) (hulpy - hulp);
					LinePoints[1].x = 0;
					LinePoints[1].y = (int) (hulpy + hulp);
					LinePoints[2].x = (int) (-2 * hulp);
					LinePoints[2].y = (int) (hulpy);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);
					break;

				case 2:		// input/output
					hulp = Mult(0.4);
					hulp2 = Mult(1.0);
					hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;
					hulpy = Mult(yy1 - Yoffset);
					LinePoints[0].x = 0;
					LinePoints[0].y = (int) (hulpy);
					LinePoints[1].x = (int) (-2 * hulp);
					LinePoints[1].y = (int) (hulpy - hulp);
					LinePoints[2].x = (int) (-2 * hulp);
					LinePoints[2].y = (int) (hulpy + hulp);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);

					LinePoints[0].x = (int) (-hulp2);
					LinePoints[0].y = (int) (hulpy - hulp);
					LinePoints[1].x = (int) (-hulp2);
					LinePoints[1].y = (int) (hulpy + hulp);
					LinePoints[2].x = (int) (-hulp2 - hulp * 2);
					LinePoints[2].y = (int) (hulpy);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
						LinePoints[2].x *= -1;
						LinePoints[3].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					LinePoints[2].x += (int) (hulpx);
					LinePoints[3].x += (int) (hulpx);
					Polygon(OutputDisplay, (POINT *) & LinePoints, 3);

					LinePoints[0].x = (int) (-hulp2);
					LinePoints[0].y = (int) (hulpy);
					LinePoints[1].x = (int) (-2 * hulp);
					LinePoints[1].y = (int) (hulpy);

					if ((ConnectionType & 1) == 1)
					{
						LinePoints[0].x *= -1;
						LinePoints[1].x *= -1;
					}

					LinePoints[0].x += (int) (hulpx);
					LinePoints[1].x += (int) (hulpx);
					InitDrawingGlobalConnections(Mult(STANDARD_LINE_THICKNESS));
					Polyline(OutputDisplay, (POINT *) & LinePoints, 2);
					break;
				}
			}

			if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}

	BackGroundActive = TempBackGroundActive;

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2)))
	{
		xx1 = GlobalConnection->NameX + OX;
		yy1 = GlobalConnection->NameY + OY;
		GetMinMaxText(xx1, yy1, 1.0, 0, 0, GlobalConnection->NameInfo & 15, GlobalConnection->Text);

		if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
		{

			InitDrawingGlobalConnections(Mult(STANDARD_LINE_THICKNESS));

			if (((Info & OBJECT_SELECTED) == OBJECT_SELECTED) && (Mode == 0))
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);

			DrawStr(xx1, yy1, 1.0, 0, GlobalConnection->NameInfo & 15, GlobalConnection->Text);

			if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
			{
				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
			}
		}
	}

}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void DrawGlobalConnections(int32 Mode)
{
	GlobalConnectionRecord *GlobalConnection;
	int32 cnt;

	for (cnt = 0; cnt < min(MaxNrGlobalConnections, Design.NrGlobalConnections); cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if ((GlobalConnection->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawGlobalConnection(GlobalConnection, 0.0, 0.0, 0);
	}

	ExitDrawing();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawNetLabel(NetLabelRecord * NetLabel, double OX, double OY, int32 Mode)
{
	double xx1, yy1, xx2, yy2;
	int32 Info, TextAlignment, TextRotation, x1a, y1a, x2a, y2a, NrProperties;
	char str[MAX_LENGTH_STRING];

#ifdef _DEBUG

	if (stricmp(NetLabel->Name, "12.288_MHz") == 0)
		ok = 1;

#endif
	Info = NetLabel->Info;
	xx1 = NetLabel->ConnectX;
	yy1 = NetLabel->ConnectY;

	if ((Info & 1) == 1)
	{	// ConnectX and TextX
		xx1 += OX;
		yy1 += OY;
		xx2 = xx1 + NetLabel->TextX;
		yy2 = yy1 + NetLabel->TextY;
	}
	else
	{	// TextX
		xx2 = xx1 + NetLabel->TextX + OX;
		yy2 = yy1 + NetLabel->TextY + OY;
	}

	TextRotation = (NetLabel->Alignment >> 8) & 0x01;
	TextAlignment = (NetLabel->Alignment & 0x0f);
	NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1);

	if ((NrProperties > 0) && (AppendPropertiesToNetlabel))
		sprintf(str, "%s (...)", NetLabel->Name);
	else
		strcpy(str, NetLabel->Name);

	GetMinMaxText(xx2, yy2, 1.0, 0, TextRotation, TextAlignment, str);

	if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
	{
		x1a = Mult(xx1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1a = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
		else
			y1a = Mult(yy1 - Yoffset);

		x2a = Mult(xx2 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y2a = DrawWindowMaxY - Mult(yy2 - Yoffset) - 1;
		else
			y2a = Mult(yy2 - Yoffset);

		InitDrawingNetLabels(Mult(STANDARD_LINE_THICKNESS));

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if (Printing)
			yy2 += 0.1;

		DrawStr(xx2, yy2, 1.0, TextRotation, TextAlignment, str);

		if ((Mode & 4) == 4)
			DrawLine(x1a, y1a, x2a, y2a);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawNetLabels(int32 Mode)
{
	int32 cnt;
	NetLabelRecord *NetLabel;

	for (cnt = 0; cnt < min(MaxNrNetLabels, Design.NrNetLabels); cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((NetLabel->Info & OBJECT_SELECTED) != 0)
			{
				if (!Printing)
					if (!Printing)
						SetROP2(OutputDisplay, SelectColorMode);
			}

			DrawNetLabel(NetLabel, 0.0, 0.0, 0);

			if ((NetLabel->Info & OBJECT_SELECTED) != 0)
			{
				if (!Printing)
					if (!Printing)
						SetROP2(OutputDisplay, R2_COPYPEN);
			}
		}
	}

	ExitDrawing();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectLines(int32 Mode)
{
	int32 cnt;
	ObjectLineRecord *ObjectLine;

	for (cnt = 0; cnt < min(MaxNrObjectLines, Design.NrObjectLines); cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawObjectLine(ObjectLine, 0.0, 0.0, 0);
	}

	ExitDrawing();


}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectLine(ObjectLineRecord * ObjectLine, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, y2, Xmax, Xmin, Ymax, Ymin, LineBuf[256], Length, Angle;
	int32 Info, x1a, y1a, x2a, y2a, ok, SegmentCount, cnt2, LineSegments;

	Info = ObjectLine->Info;
	x1 = ObjectLine->X1;
	y1 = ObjectLine->Y1;
	x2 = ObjectLine->X2;
	y2 = ObjectLine->Y2;
#ifdef _DEBUG

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
		ok = 1;

#endif

	switch (Info & 3)
	{
	case 0:
	case 3:
		x1 += OX;
		y1 += OY;
		x2 += OX;
		y2 += OY;
		break;

	case 1:
		x1 += OX;
		y1 += OY;
		break;

	case 2:
		x2 += OX;
		y2 += OY;
		break;
	}

	if (x1 < x2)
	{
		Xmin = x1;
		Xmax = x2;
	}
	else
	{
		Xmin = x2;
		Xmax = x1;
	}

	if (y1 < y2)
	{
		Ymin = y1;
		Ymax = y2;
	}
	else
	{
		Ymin = y2;
		Ymax = y1;
	}

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1a = MultX(x1);
		y1a = MultY(y1);
		x2a = MultX(x2);
		y2a = MultY(y2);
		InitDrawingObjectLines(Mult(ObjectLine->Thickness));

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if ((x1 == x2) && (y1 == y2))
		{
//      ellips2(Mult(x1-Xoffset),y1a,Mult(1.0),Mult(1.0),255);
		}
		else
		{
			if (ObjectLine->LineMode != 0)
			{
				ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

				if (Length > 0.0)
				{
					LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, ObjectLine->LineMode);
					SegmentCount = 0;

					for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
					{
						x1a = MultX(LineBuf[SegmentCount]);
						SegmentCount++;
						y1a = MultY(LineBuf[SegmentCount]);
						SegmentCount++;
						x2a = MultX(LineBuf[SegmentCount]);
						SegmentCount++;
						y2a = MultY(LineBuf[SegmentCount]);
						SegmentCount++;
						DrawLine(x1a, y1a, x2a, y2a);
					}
				}
			}
			else
				DrawLine(x1a, y1a, x2a, y2a);
		}

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
	else
		ok = 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectRects(int32 Mode)
{
	int32 cnt;
	ObjectRectRecord *ObjectRect;

	for (cnt = 0; cnt < min(MaxNrObjectRects, Design.NrObjectRects); cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectRect(ObjectRectRecord * ObjectRect, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, y2, xx2, yy2, Xmax, Xmin, Ymax, Ymin;
	int32 Info, x1a, y1a;

	x1 = ObjectRect->CentreX + OX;
	y1 = ObjectRect->CentreY + OY;
	x2 = ObjectRect->Width;
	y2 = ObjectRect->Height;
	xx2 = x2 / 2;
	yy2 = y2 / 2;
	Xmin = x1 - xx2;
	Ymin = y1 - yy2;
	Xmax = x1 + xx2;
	Ymax = y1 + yy2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1a = Mult(x1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;
		else
			y1a = Mult(y1 - Yoffset);

		Info = ObjectRect->Info;

		if (ObjectRect->Thickness == 0.0)
			InitDrawingObjectRects(0, 1);
		else
			InitDrawingObjectRects(Mult(ObjectRect->Thickness), 0);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		rect3(x1a, y1a, Mult(x2), Mult(y2));

		if (ReverseY)
		{
			if (Mode == 3)
			{
				DrawLine(x1a - 3, y1a - 3, x1a + 3, y1a + 3);
				DrawLine(x1a - 3, y1a + 3, x1a + 3, y1a - 3);
			}
		}

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void DrawObjectCircles(int32 Mode)
{
	int32 cnt;
	ObjectCircleRecord *ObjectCircle;

	for (cnt = 0; cnt < min(MaxNrObjectCircles, Design.NrObjectCircles); cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawObjectCircle(ObjectCircle, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectCircle(ObjectCircleRecord * ObjectCircle, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, xx2, Xmax, Xmin, Ymax, Ymin;
	int32 Info, CircleMode, x1a, y1a;

	x1 = ObjectCircle->CentreX + OX;
	y1 = ObjectCircle->CentreY + OY;
	x2 = ObjectCircle->Diam;

	if (!ReverseY)
		CircleMode = CircleConv[CircleMirrorY[ObjectCircle->CircleMode]];
	else
		CircleMode = CircleConv[ObjectCircle->CircleMode];

//  y2=ObjectCircle->Height;
	xx2 = x2 / 2;
//  yy2=y2/2;
	Xmin = x1 - xx2;
	Ymin = y1 - xx2;
	Xmax = x1 + xx2;
	Ymax = y1 + xx2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1a = Mult(x1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;
		else
			y1a = Mult(y1 - Yoffset);

		Info = ObjectCircle->Info;

		if (ObjectCircle->Thickness == 0.0)
		{
			InitDrawingObjectCircles(0, 1);
			CircleMode = 255;
		}
		else
			InitDrawingObjectCircles(Mult(ObjectCircle->Thickness), 0);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		ellips2(x1a, y1a, Mult(x2), Mult(x2), (uint8) CircleMode);

		if (ReverseY)
		{
			if (Mode == 3)
			{
				DrawLine(x1a - 3, y1a - 3, x1a + 3, y1a + 3);
				DrawLine(x1a - 3, y1a + 3, x1a + 3, y1a - 3);
			}
		}

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void DrawObjectArcs(int32 Mode)
{
	int32 cnt;
	ObjectArcRecord *ObjectArc;

	for (cnt = 0; cnt < min(MaxNrObjectArcs, Design.NrObjectArcs); cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectArc(ObjectArcRecord * ObjectArc, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, y2, x3, y3, x4, y4, xx2, Xmax, Xmin, Ymax, Ymin;
	int32 Info, x1a, y1a;

	x1 = ObjectArc->CentreX + OX;
	y1 = ObjectArc->CentreY + OY;
	x2 = ObjectArc->Width;
	y2 = ObjectArc->Height;
	xx2 = x2 * 0.5;
//  yy2=y2/2;
	Xmin = x1 - xx2;
	Ymin = y1 - xx2;
	Xmax = x1 + xx2;
	Ymax = y1 + xx2;

	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		x1a = Mult(x1 - Xoffset) + DrawWindowMinX;

		if (ReverseY)
			y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;
		else
			y1a = Mult(y1 - Yoffset);

		Info = ObjectArc->Info;
		x3 = ObjectArc->StartDiffX;
		y3 = ObjectArc->StartDiffY;
		x4 = ObjectArc->EndDiffX;
		y4 = ObjectArc->EndDiffY;
		InitDrawingObjectArcs(Mult(ObjectArc->Thickness));

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		if (ReverseY)
		{
			SpecialArc(x1a, y1a, Mult(x2), Mult(x2), Mult(x1 + x3 - Xoffset) + DrawWindowMinX,
			           DrawWindowMaxY - Mult(y1 + y3 - Yoffset) - 1, Mult(x1 + x4 - Xoffset) + DrawWindowMinX,
			           DrawWindowMaxY - Mult(y1 + y4 - Yoffset) - 1);

			if (Mode == 3)
			{
				DrawLine(x1a - 3, y1a - 3, x1a + 3, y1a + 3);
				DrawLine(x1a - 3, y1a + 3, x1a + 3, y1a - 3);
			}
		}
		else
			SpecialArc(x1a, y1a, Mult(x2), Mult(x2), Mult(x1 + x3 - Xoffset) + DrawWindowMinX,
			           Mult(y1 + y3 - Yoffset) - 1, Mult(x1 + x4 - Xoffset) + DrawWindowMinX,
			           Mult(y1 + y4 - Yoffset) - 1);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectTexts(int32 Mode)
{
	int32 cnt;
	ObjectTextRecord *ObjectText;

	for (cnt = 0; cnt < min(MaxNrObjectTexts, Design.NrObjectTexts); cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawObjectText(ObjectText, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectText(ObjectTextRecord * ObjectText, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, y2, x3, x4, y4, Xmax, Xmin, Ymax, Ymin, Rotation;
	int32 cnt, cnt2, cnt3, Info, Length, TextAlignment, NrLines;
	char TextStr[MAX_LENGTH_STRING];
	char TextStrings[64][MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 ok;
#endif

	if ((Length = strlen(ObjectText->Text)) == 0)
		return;

#ifdef _DEBUG

	if (Length > 120)
		ok = 1;

	if (stricmp(ObjectText->Text, "A7/P6.7") == 0)
		ok = 1;

	if (stricmp(ObjectText->Text, "$NR_SHEETS") == 0)
		ok = 1;

#endif

	if (ConvertTextString(ObjectText->Text, TextStr) == -1)
		strcpy(TextStr, ObjectText->Text);

	x1 = ObjectText->X;
	y1 = ObjectText->Y;
	x2 = x1 + OX;
	y2 = y1 + OY;
	x4 = x2;
	y4 = y2;
	x3 = ObjectText->FontHeight;
	Rotation = ObjectText->Rotation;
	TextAlignment = ObjectText->TextMode & 0x0f;
#ifdef _DEBUG

	if (Rotation != 0.0)
		ok = 1;

#endif

	Xmin = 1e9;
	Xmax = -1e9;
	Ymin = 1e9;
	Ymax = -1e9;
	Length = strlen(TextStr);
	NrLines = 0;
	memset(TextStrings, 0, sizeof(TextStrings));
	cnt3 = 0;
	cnt2 = cnt3;

	while (cnt3 < Length + 1)
	{
		if ((TextStr[cnt3] == '\r') || ((cnt3 == Length) && (TextStr[cnt3 - 1] != '\n')))
		{
			if (NrLines < 64)
			{
				if (cnt3 - cnt2 > 0)
				{
					strncpy((LPSTR) & TextStrings[NrLines], (LPSTR) & TextStr[cnt2], min(127, cnt3 - cnt2));

					if ((Rotation == 0.0) || (InRangeSpecial(Rotation, 90.0, 0.01)))
					{
						if (Rotation == 0.0)
							GetMinMaxText(x4, y4, x3, 0, 0, TextAlignment, (LPSTR) & TextStrings[NrLines]);
						else
							GetMinMaxText(x4, y4, x3, 0, 1, TextAlignment, (LPSTR) & TextStrings[NrLines]);
					}
					else
						GetMinMaxText2(x4, y4, x3, 0, Rotation, TextAlignment, 0, (LPSTR) & TextStrings[NrLines]);

					Xmin = min(Xmin, TextMinX);
					Ymin = min(Ymin, TextMinY);
					Xmax = max(Xmax, TextMaxX);
					Ymax = max(Ymax, TextMaxY);
				}

				NrLines++;
				x4 += sin(ANGLE_CONVERT(Rotation)) * x3;
				y4 -= cos(ANGLE_CONVERT(Rotation)) * x3;
			}

			cnt3 += 1;
			cnt2 = cnt3 + 1;
		}

		cnt3++;
	}


	if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
	{
		Info = ObjectText->Info;
		InitDrawingObjectTexts(Mult(ObjectText->Thickness));

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		for (cnt = 0; cnt < NrLines; cnt++)
		{
			if ((Rotation == 0.0) || (InRangeSpecial(Rotation, 90.0, 0.01)))
			{
				if (Rotation == 0.0)
					DrawStr(x2, y2, x3, 0, TextAlignment, TextStrings[cnt]);
				else
					DrawStr(x2, y2, x3, 1, TextAlignment, TextStrings[cnt]);
			}
			else
				DrawStrWithRotation(x2, y2, x3, Rotation, 0, 0, TextStrings[cnt]);

			x2 += sin(ANGLE_CONVERT(Rotation)) * x3;
			y2 -= cos(ANGLE_CONVERT(Rotation)) * x3;
		}

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPin(PinRecord * Pin, double OX, double OY, int32 Mode)
{
	double xx1, yy1, yy2, Xmax, Xmin, Ymax, Ymin;
	int32 hulp, hulpx, hulpy, Info;
	POINT LinePoints[20];
	int32 TempBackGroundActive;

	TempBackGroundActive = BackGroundActive;

	xx1 = Pin->X + OX;
	yy1 = Pin->Y + OY;
	Info = Pin->Info;

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)))
	{
		yy2 = 0.25;
		Xmin = xx1 - yy2;
		Ymin = yy1 - yy2;
		Xmax = xx1 + yy2;
		Ymax = yy1 + yy2;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			hulp = Mult(0.25);
			hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;

			if (ReverseY)
				hulpy = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
			else
				hulpy = Mult(yy1 - Yoffset);

			LinePoints[0].x = hulpx - hulp;
			LinePoints[0].y = hulpy;
			LinePoints[1].x = hulpx;
			LinePoints[1].y = hulpy - hulp;
			LinePoints[2].x = hulpx + hulp;
			LinePoints[2].y = hulpy;
			LinePoints[3].x = hulpx;
			LinePoints[3].y = hulpy + hulp;
			LinePoints[4].x = hulpx - hulp;
			LinePoints[4].y = hulpy;
			InitDrawingSymbolPins(0);

			if (((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)) && (Mode == 0))
			{
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);
			}

			Polygon(OutputDisplay, (POINT *) & LinePoints, 5);

			if (((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)) && (Mode == 0))
			{
				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
			}
		}
	}

//
	BackGroundActive = TempBackGroundActive;

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2)))
	{
		xx1 = Pin->NameX + OX;
		yy1 = Pin->NameY + OY;
		GetMinMaxText(xx1, yy1, 1.0, 0, (Pin->NameInfo >> 8) & 1, Pin->NameInfo & 15, Pin->Name);

		if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
		{
			InitDrawingSymbolPinTexts(Mult(STANDARD_LINE_THICKNESS));

			if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
			{
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);
			}

			if (!EditingSheetSymbol)
				DrawStr(xx1, yy1, 1.0, (Pin->NameInfo >> 8) & 1, Pin->NameInfo & 15, Pin->Name);
			else
				DrawStr(xx1, yy1, 1.0, (Pin->NameInfo >> 8) & 1, Pin->NameInfo & 15, Pin->Label);

			if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
			{
				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
			}
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPins(int32 Mode)
{
	int32 cnt;
	PinRecord *Pin;

	for (cnt = 0; cnt < min(MaxNrPins, DesignSymbol.NrPins); cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawPin(Pin, 0.0, 0.0, 0);
	}

	ExitDrawing();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPowerPin(PowerPinRecord * PowerPin, double OX, double OY, int32 Mode)
{
	double x1, y1, x2;
	int32 Info, TextRotation, FontNr, Alignment;
	char str[MAX_LENGTH_STRING];

	x1 = PowerPin->NameX + OX;
	y1 = PowerPin->NameY + OY;
	x2 = 1.0;
	TextRotation = (PowerPin->NameInfo >> 8) & 1;
	Alignment = PowerPin->NameInfo & 0x0f;
	FontNr = 0;
	strcpy(str, PowerPin->NetName);
	strcat(str, " : ");
	strcat(str, PowerPin->Text);
	GetMinMaxText(x1, y1, x2, FontNr, TextRotation, 0, str);

//  TextMaxX=10000.0;
//  TextMinX=-10000.0;
//  TextMaxY=10000.0;
//  TextMinY=-10000.0;

	if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
	{
		Info = PowerPin->Info;
		InitDrawingSymbolPowerPinTexts(Mult(STANDARD_LINE_THICKNESS));

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		DrawStr(x1, y1, x2, TextRotation, Alignment, str);

		if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPowerPins(int32 Mode)
{
	int32 cnt;
	PowerPinRecord *PowerPin;

	for (cnt = 0; cnt < min(MaxNrPowerPins, DesignSymbol.NrPowerPins); cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if ((PowerPin->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawPowerPin(PowerPin, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPinBus(PinBusRecord * PinBus, double OX, double OY, int32 Mode)
{
	double x1, y1, x2, xx1, yy1, yy2, xx3, yy3, Xmax, Xmin, Ymax, Ymin;
	int32 Info, FontNr, NrLines, lengte, TextAlignment, TextRotation, cnt, hulp, hulpx, hulpy, AddMode, LinePos[16];
	char PinBusStrings[16][64];
	LPSTR LabelText;
	POINT LinePoints[20];
	int32 TempBackGroundActive;
#ifdef _DEBUG
	int32 ok;
#endif

	TempBackGroundActive = BackGroundActive;

	xx1 = PinBus->X + OX;
	yy1 = PinBus->Y + OY;
	Info = PinBus->Info;

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)))
	{
		yy2 = 0.25;
		Xmin = xx1 - yy2;
		Ymin = yy1 - yy2;
		Xmax = xx1 + yy2;
		Ymax = yy1 + yy2;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			hulp = Mult(0.25);
			hulpx = Mult(xx1 - Xoffset) + DrawWindowMinX;

			if (ReverseY)
				hulpy = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
			else
				hulpy = Mult(yy1 - Yoffset);

			LinePoints[0].x = hulpx - hulp;
			LinePoints[0].y = hulpy;
			LinePoints[1].x = hulpx;
			LinePoints[1].y = hulpy - hulp;
			LinePoints[2].x = hulpx + hulp;
			LinePoints[2].y = hulpy;
			LinePoints[3].x = hulpx;
			LinePoints[3].y = hulpy + hulp;
			LinePoints[4].x = hulpx - hulp;
			LinePoints[4].y = hulpy;
			InitDrawingSymbolPinBusses();

			if (((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)) && (Mode == 0))
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);

			Polygon(OutputDisplay, (POINT *) & LinePoints, 5);

			if (((Info & (OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1)) && (Mode == 0))
				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}

	BackGroundActive = TempBackGroundActive;

	if ((Mode != 2) || ((Info & (OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2)))
	{

		x1 = PinBus->NameX + OX;
		y1 = PinBus->NameY + OY;
		x2 = 1.0;
		TextRotation = (PinBus->NameInfo >> 8) & 1;
		TextAlignment = PinBus->NameInfo & 0x0f;
		AddMode = 0;

		if (TextRotation)
			AddMode += 4;

		if ((TextAlignment == 6) || (TextAlignment == 8))
			AddMode += 1;		// Mirror X

		if ((TextAlignment == 2) || (TextAlignment == 8))
			AddMode += 2;		// Mirror Y

		LabelText = (PinBus->Label);

#ifdef _DEBUG

		if (stricmp(LabelText, "d[0:63]") == 0)
			ok = 1;

#endif


		FontNr = 0;
		lengte = strlen(PinBus->Text);
		cnt = 0;
		NrLines = 0;
		memset(LinePos, 0, sizeof(LinePos));
		memset(PinBusStrings, 0, sizeof(PinBusStrings));
		LinePos[0] = 0;

		while (cnt < lengte)
		{
			if (PinBus->Text[cnt] == '\\')
			{
				memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], cnt - LinePos[NrLines]);
				NrLines++;
				LinePos[NrLines] = cnt + 1;
			}

			cnt++;
		}

		if (PinBus->Text[lengte - 1] != '\\')
		{
			memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], cnt - LinePos[NrLines]);
			LinePos[NrLines] = cnt;
			NrLines++;
		}

//  strcat(str," : ");
//  strcat(str,PinBus->Text);

		xx3 = x1;
		yy3 = y1;
		Xmax = -10000000.0;
		Xmin = 10000000.0;
		Ymax = -10000000.0;
		Ymin = 10000000.0;

		switch (AddMode)
		{
		case 2:				// Rotation = 0 , MirrorY = 1 , MirrorX = 0
		case 3:				// Rotation = 0 , MirrorY = 1 , MirrorX = 1
			yy3 += (NrLines - 1);
			break;

		case 6:				// Rotation = 1 , MirrorY = 1 , MirrorX = 0
		case 7:				// Rotation = 1 , MirrorY = 1 , MirrorX = 1
			xx3 -= (NrLines - 1);
			break;
		}

		for (cnt = 0; cnt < NrLines; cnt++)
		{
			GetMinMaxText(xx3, yy3, x2, 0, TextRotation, TextAlignment, PinBusStrings[cnt]);
			Xmin = min(Xmin, TextMinX);
			Ymin = min(Ymin, TextMinY);
			Xmax = max(Xmax, TextMaxX);
			Ymax = max(Ymax, TextMaxY);

			switch (AddMode)
			{
			case 0:			// Rotation = 0 , MirrorY = 0 , MirrorX = 0
			case 1:			// Rotation = 0 , MirrorY = 0 , MirrorX = 1
			case 2:			// Rotation = 0 , MirrorY = 1 , MirrorX = 0
			case 3:			// Rotation = 0 , MirrorY = 1 , MirrorX = 1
				yy3 -= 1.0;
				break;

			case 4:			// Rotation = 1 , MirrorY = 0 , MirrorX = 0
			case 5:			// Rotation = 1 , MirrorY = 0 , MirrorX = 1
			case 6:			// Rotation = 1 , MirrorY = 1 , MirrorX = 0
			case 7:			// Rotation = 1 , MirrorY = 1 , MirrorX = 1
				xx3 += 1.0;
				break;
			}
		}

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			InitDrawingSymbolPinBusTexts(Mult(STANDARD_LINE_THICKNESS));

			if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
			{
				if (!Printing)
					SetROP2(OutputDisplay, SelectColorMode);
			}

			xx3 = x1;
			yy3 = y1;

			switch (AddMode)
			{
			case 2:			// Rotation = 0 , MirrorY = 1 , MirrorX = 0
			case 3:			// Rotation = 0 , MirrorY = 1 , MirrorX = 1
				yy3 += (NrLines - 1);
				break;

			case 6:			// Rotation = 1 , MirrorY = 1 , MirrorX = 0
			case 7:			// Rotation = 1 , MirrorY = 1 , MirrorX = 1
				xx3 -= (NrLines - 1);
				break;
			}

			for (cnt = 0; cnt < NrLines; cnt++)
			{
				DrawStr(xx3, yy3, x2, TextRotation, TextAlignment, PinBusStrings[cnt]);

				switch (AddMode)
				{
				case 0:		// Rotation = 0 , MirrorY = 0 , MirrorX = 0
				case 1:		// Rotation = 0 , MirrorY = 0 , MirrorX = 1
				case 2:		// Rotation = 0 , MirrorY = 1 , MirrorX = 0
				case 3:		// Rotation = 0 , MirrorY = 1 , MirrorX = 1
					yy3 -= 1.0;
					break;

				case 4:		// Rotation = 1 , MirrorY = 0 , MirrorX = 0
				case 5:		// Rotation = 1 , MirrorY = 0 , MirrorX = 1
				case 6:		// Rotation = 1 , MirrorY = 1 , MirrorX = 0
				case 7:		// Rotation = 1 , MirrorY = 1 , MirrorX = 1
					xx3 += 1.0;
					break;
				}
			}

			if ((Mode == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
			{
				if (!Printing)
					SetROP2(OutputDisplay, R2_COPYPEN);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPinBusses(int32 Mode)
{
	int32 cnt;
	PinBusRecord *PinBus;

	for (cnt = 0; cnt < min(MaxNrPinBusses, DesignSymbol.NrPinBusses); cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawPinBus(PinBus, 0.0, 0.0, 0);
	}

	ExitDrawing();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawStr(double x, double y, double Size, int32 Rotation, int32 Alignment, char *str)
// Polygon
{
	char code, *str2;
	int32 ok, cnt, cnt2, cnt4, NrPolyLines, lengte, count, oldx, oldy, Found, hulp, xx1, yy1, xx2, yy2, hx, hy;
	double x1d, y1d, x1, y1, lengte2, NewLine;

	POINT LinePoints[20];
	GetMinMaxText(x, y, Size, 0, Rotation, Alignment, str);

	hulp = Mult(Size);

#ifdef _DEBUG

	if (stricmp(str, "R502 (...)") == 0)
		ok = 1;

#endif

	if ((hulp < 4) && (!Printing))
	{
		xx1 = Mult((TextMaxX + TextMinX) * 0.5 - Xoffset) + DrawWindowMinX;

		if (!Printing)
			yy1 = DrawWindowMaxY - Mult((TextMaxY + TextMinY) * 0.5 - Yoffset) - 1;
		else
			yy1 = Mult(TextMinY - Yoffset);

		xx2 = Mult(TextMaxX - TextMinX) - 1;
		yy2 = Mult(TextMaxY - TextMinY) - 1;
//    yy2=max(2,Mult(TextMaxY-TextMinY)-1);
		rect3(xx1, yy1, xx2, yy2);

		if ((yy2 <= 2) || (xx2 <= 2))
			rect3(xx1, yy1, xx2 - 2, yy2 - 2);

//    rect2(Mult(TextMinX-Xoffset),DrawWindowMaxY-Mult(TextMinY-Yoffset)-1,
//          Mult(TextMaxX-TextMinX),Mult(TextMinY-TextMaxY));
		return;
	}

	lengte2 = strlen(str);

	switch (Rotation)
	{
	case 0:
		if (ReverseY)
			y -= Size * 0.3 * DefFontSize;
		else
			y -= Size * 0.2 * DefFontSize;

		switch (Alignment)
		{
		case ALIGN_LEFT_BOTTOM:
			x += Size * 0.1 * DefFontSize;
			break;

		case ALIGN_RIGHT_BOTTOM:
			if (!Printing)
			{
				if (ReverseY)
					x -= lengte2 * Size * DefFontSize * CharWidthFactor;
				else
				{
				}
			}

			break;

		case ALIGN_LEFT_CENTRE:
			if (ReverseY)
				x += Size * 0.1 * DefFontSize;

			y -= Size * 0.45 * DefFontSize;
			break;

		case ALIGN_RIGHT_CENTRE:
			y -= Size * 0.45 * DefFontSize;

			if (!Printing)
			{
				if (ReverseY)
					x -= lengte2 * Size * DefFontSize * CharWidthFactor;
				else
				{
				}
			}

			break;

		case ALIGN_LEFT_TOP:
			x += Size * 0.1 * DefFontSize;
			y -= Size * 1.15 * DefFontSize;
			break;

		case ALIGN_RIGHT_TOP:
			y -= Size * 1.15 * DefFontSize;

			if (!Printing)
			{
				if (ReverseY)
					x -= lengte2 * Size * DefFontSize * CharWidthFactor;
				else
				{
				}
			}

			break;
		}

		break;

	case 1:
		x += Size * 0.3 * DefFontSize;

		switch (Alignment)
		{
		case ALIGN_LEFT_BOTTOM:
			y += Size * 0.1 * DefFontSize;
			break;

		case ALIGN_RIGHT_BOTTOM:
			if (!Printing)
			{
				if (ReverseY)
					y -= lengte2 * Size * DefFontSize * CharWidthFactor;
				else
				{
				}
			}

			break;

		case ALIGN_LEFT_CENTRE:
			y += Size * 0.1 * DefFontSize;
			x += Size * 0.45 * DefFontSize;
			break;

		case ALIGN_RIGHT_CENTRE:
			x += Size * 0.45 * DefFontSize;

			if (!Printing)
			{
				if (ReverseY)
					y -= lengte2 * Size * DefFontSize * CharWidthFactor;
				else
				{
				}
			}

			break;

		case ALIGN_LEFT_TOP:
			y += Size * 0.1 * DefFontSize;
			x += Size * 1.15 * DefFontSize;
			break;

		case ALIGN_RIGHT_TOP:
			if (!Printing)
			{
				if (ReverseY)
					y -= lengte2 * Size * DefFontSize * CharWidthFactor;
				else
				{
				}
			}

			x += Size * 1.15 * DefFontSize;
			break;
		}

		break;
	}

	if (Printing)
	{
		if ((InRangeSpecial(Size, 1.0, 0.2)) || (InRangeSpecial(Size, 2.0, 0.2)) || (InRangeSpecial(Size, 3.0, 0.2)))
		{
#ifdef _DEBUG

			if (stricmp(str, "SHLD") == 0)
				ok = 1;

#endif
			Found = 0;

			if (Rotation == 1)
			{
				if ((InRangeSpecial(Size, 1.0, 0.2)))
				{
					SelectObject(OutputDisplay, PrinterFont_90);
					Found = 1;
				}

				if ((InRangeSpecial(Size, 2.0, 0.2)))
				{
					SelectObject(OutputDisplay, PrinterFont2_90);
					Found = 1;
				}

				if ((InRangeSpecial(Size, 3.0, 0.2)))
				{
					SelectObject(OutputDisplay, PrinterFont3_90);
					Found = 1;
				}
			}

			if (Rotation == 0)
			{
				if ((InRangeSpecial(Size, 1.0, 0.2)))
				{
					SelectObject(OutputDisplay, PrinterFont);
					Found = 1;
				}

				if ((InRangeSpecial(Size, 2.0, 0.2)))
				{
					SelectObject(OutputDisplay, PrinterFont2);
					Found = 1;
				}

				if ((InRangeSpecial(Size, 3.0, 0.2)))
				{
					SelectObject(OutputDisplay, PrinterFont3);
					Found = 1;
				}
			}

#ifdef _DEBUG

			if (Found == 0)
				ok = 1;

#endif
			xx1 = Mult(x - Xoffset);

			//    yy1=Mult(y-Yoffset);
			if (ReverseY)
				yy1 = DrawWindowMaxY - Mult(y - Yoffset) - 1;
			else
				yy1 = Mult(y - Yoffset);

			switch (Alignment)
			{
			case ALIGN_LEFT_BOTTOM:
			case ALIGN_LEFT_CENTRE:
			case ALIGN_LEFT_TOP:
				SetTextAlign(OutputDisplay, TA_LEFT | TA_BOTTOM);
				break;

			case ALIGN_RIGHT_BOTTOM:
			case ALIGN_RIGHT_CENTRE:
			case ALIGN_RIGHT_TOP:
				SetTextAlign(OutputDisplay, TA_RIGHT | TA_BOTTOM);
				break;
			}

			TextOutUTF8(OutputDisplay, xx1, yy1, str, strlen(str));
			return;
		}

		ok = 1;
	}

	Size *= DefFontSize;
	str2 = str;
	lengte = (int32) strlen(str);

	for (cnt4 = 0; cnt4 < lengte; cnt4++)
	{
		code = (*str2);
//    code='"';
		oldx = -100000000;
		oldy = -100000000;
		count = 0;

		if ((code >= 32) && (code < 127))
		{
			if (code > 32)
			{
//        NrLines=0;
				code -= 33;
				NrPolyLines = (*Chars)[(int32) code].NrPolyLines;
				cnt2 = 0;

				for (cnt = 0; cnt < NrPolyLines; cnt++)
				{
					count = 0;

					do
					{
						x1d = ((*Chars)[(int32) code].Line[cnt2 + 1]);
						y1d = ((*Chars)[(int32) code].Line[cnt2 + 2]);
						x1 = x1d * Size;
						y1 = y1d * Size;

						if ((Rotation != 0) && (Rotation != 1))
							Rotation = 0;

						switch (Rotation)
						{
						case 0:
							hx = (Mult(x1 + x - Xoffset) + DrawWindowMinX);

							if (ReverseY)
								hy = (DrawWindowMaxY - Mult(y1 + y - Yoffset) - 1);
							else
								hy = (Mult(y1 + y - Yoffset));

							LinePoints[count].x = (int32) hx;
							LinePoints[count].y = (int32) hy;
							break;

						case 1:
							hx = (Mult(-y1 + x - Xoffset) + DrawWindowMinX);

							if (ReverseY)
								hy = (DrawWindowMaxY - Mult(x1 + y - Yoffset) - 1);
							else
								hy = (Mult(x1 + y - Yoffset));

							LinePoints[count].x = (int32) hx;
							LinePoints[count].y = (int32) hy;
							break;
						}

						count++;
						cnt2 += 3;
						NewLine = ((*Chars)[(int32) code].Line[cnt2]);
					}
					while (InRange(NewLine, 0.0));

					if (ReverseY)
					{
						if (count >= 20)
							ok = 1;

						Polyline(OutputDisplay, (POINT *) & LinePoints, (int32) count);
						SetPixel(OutputDisplay, (int32) LinePoints[count - 1].x, (int32) LinePoints[count - 1].y,
						         LineColor);
					}
					else
					{
						Polyline(OutputDisplay, (POINT *) & LinePoints, (int) count);
						SetPixel(OutputDisplay, (int32) LinePoints[count - 1].x, (int32) LinePoints[count - 1].y,
						         LineColor);
					}

				}

				/*
				        for (cnt=0;cnt<NrLines;cnt++) {
				          x1d=((*Chars)[code].PolyLine[cnt].x1/1000000.0);
				          y1d=((*Chars)[code].PolyLine[cnt].y1/1000000.0);
				          x2d=((*Chars)[code].PolyLine[cnt].x2/1000000.0);
				          y2d=((*Chars)[code].PolyLine[cnt].y2/1000000.0);
				          x1=x1d*Size;
				          y1=y1d*Size;
				          x2=x2d*Size;
				          y2=y2d*Size;
				          switch (Rotation) {
				            case 0:
				              DrawLine(Mult(x1+x-Xoffset),DrawWindowMaxY-(Mult(y1+y-Yoffset))-1,
				                       Mult(x2+x-Xoffset),DrawWindowMaxY-(Mult(y2+y-Yoffset))-1);
				              break;
				            case 1:
				              DrawLine(Mult(-y1+x-Xoffset),DrawWindowMaxY-(Mult(x1+y-Yoffset))-1,
				                       Mult(-y2+x-Xoffset),DrawWindowMaxY-(Mult(x2+y-Yoffset))-1);
				              break;
				            case 2:
				              DrawLine(Mult(-x1+x-Xoffset),DrawWindowMaxY-(Mult(-y1+y-Yoffset))-1,
				                       Mult(-x2+x-Xoffset),DrawWindowMaxY-(Mult(-y2+y-Yoffset))-1);
				              break;
				            case 3:
				              DrawLine(Mult(y1+x-Xoffset),DrawWindowMaxY-(Mult(-x1+y-Yoffset))-1,
				                       Mult(y2+x-Xoffset),DrawWindowMaxY-(Mult(-x2+y-Yoffset))-1);
				              break;
				          }
				        }
				*/
			}

			switch (Rotation)
			{
			case 0:
				x1d = CharWidthFactor;
				x += x1d * Size;
				break;

			case 1:
				x1d = CharWidthFactor;
				y += x1d * Size;
				break;

			case 2:
				x1d = CharWidthFactor;
				x -= x1d * Size;
				break;

			case 3:
				x1d = CharWidthFactor;
				y -= x1d * Size;
				break;
			}
		}

		str2++;
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
	double x1d, y1d, NewLine, x1, y1, tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4, incX, incY;
#ifdef _DEBUG
	int32 ok;
#endif

	POINT LinePoints[20];

	GetMinMaxText2(x, y, Size, 0, Rotation, Alignment, Mirror, str);

#ifdef _DEBUG

	if (InRange(Rotation, 135.0))
		ok = 1;

#endif


	hulp = Mult(Size);

	if ((hulp < 4) && (ReverseY))
	{
		if ((InRangeSpecial(Rotation, 0.0, 0.001)) || (InRangeSpecial(Rotation, 90.0, 0.001))
		        || (InRangeSpecial(Rotation, 180.0, 0.001)) || (InRangeSpecial(Rotation, 270.0, 0.001)))
		{
			xx1 = Mult((TextMaxX + TextMinX) * 0.5 - Xoffset) + DrawWindowMinX;

			if (ReverseY)
				yy1 = DrawWindowMaxY - Mult((TextMaxY + TextMinY) * 0.5 - Yoffset) - 1;
			else
				yy1 = Mult(TextMinY - Yoffset);

			xx2 = Mult(TextMaxX - TextMinX) - 1;
			yy2 = Mult(TextMaxY - TextMinY) - 1;
			rect3(xx1, yy1, xx2, yy2);

			if ((yy2 <= 2) || (xx2 <= 2))
				rect3(xx1, yy1, xx2 - 2, yy2 - 2);
		}
		else
		{
			GetMinMaxText2(x, y, Size, 0, 0.0, Alignment, Mirror, str);
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

			RotatePointFromOtherPoint(&tx1, &ty1, x, y, Rotation);
			RotatePointFromOtherPoint(&tx2, &ty2, x, y, Rotation);
			RotatePointFromOtherPoint(&tx3, &ty3, x, y, Rotation);
			RotatePointFromOtherPoint(&tx4, &ty4, x, y, Rotation);
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
				NrPolyLines = (*Chars)[(int32) code].NrPolyLines;
				cnt2 = 0;

				for (cnt = 0; cnt < NrPolyLines; cnt++)
				{
					count = 0;

					do
					{
						x1d = ((*Chars)[(int32) code].Line[cnt2 + 1]);
						y1d = ((*Chars)[(int32) code].Line[cnt2 + 2]);
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

						LinePoints[count].x = (int32) hx;
						LinePoints[count].y = (int32) hy;
						count++;
						cnt2 += 3;
						NewLine = ((*Chars)[(int32) code].Line[cnt2]);
					}
					while (InRangeSpecial(NewLine, 0.0, 0.001));

					res = Polyline(OutputDisplay, (POINT *) & LinePoints, (int32) count);
					SetPixel(OutputDisplay, (int32) LinePoints[count - 1].x, (int32) LinePoints[count - 1].y,
					         LineColor);
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
