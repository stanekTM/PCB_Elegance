/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: nets2.c
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
#include "nets.h"
#include "draw.h"
#include "draw2.h"
#include "line2.h"
#include "math.h"
#include "pcb.h"
#include "calc.h"
#include "calcdef.h"
#include "calc2.h"
#include "calc4.h"
#include "graphics.h"
#include "string.h"
#include "mainloop.h"
#include "insdel.h"


TracePointsArray *TracePoints;

int32 ObjectsNetHorTraces, ObjectsNetVerTraces, ObjectsNetDiag1Traces, ObjectsNetDiag2Traces, ObjectsNetVias,
      ObjectsNetPins, NrTracePoints, ObjectsNetConnecions;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************



int32 GetTracePointNr(double x, double y)
{
	int32 cnt;

	cnt = 0;

	while (cnt < NrTracePoints)
	{
		if ((InRange4(x, (*TracePoints)[cnt].x)) && (InRange4(y, (*TracePoints)[cnt].y)))
			return cnt;

		cnt++;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InsertObjectPoint(double x1, double y1)
{
	TracePointRecord *TracePoint;

	TracePoint = &((*TracePoints)[NrTracePoints]);
	memset(TracePoint, 0, sizeof(TracePointRecord));
	TracePoint->x = x1;
	TracePoint->y = y1;
	NrTracePoints++;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 GetTraceObjectNrFromEndPoint(double x, double y, double *NewX, double *NewY, int32 mode)
{
	int32 cnt, Mask;
	double x1, y1, x2, y2;
	ObjectRecord *Object;

	if (mode == 0)
		Mask = 1;
	else
		Mask = 2;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		if ((Object->Info & Mask) == 0)
		{
			switch (Object->ObjectType)
			{
			case TRACE_HOR:
			case PIN_LINE_HOR:
				if ((InRange3(x, x1)) && (InRange3(y, y1)))
				{
					*NewX = x1 + x2;
					*NewY = y1;
					return cnt;
				}

				if ((InRange3(x, x1 + x2)) && (InRange3(y, y1)))
				{
					*NewX = x1;
					*NewY = y1;
					return cnt;
				}

				break;

			case TRACE_VER:
			case PIN_LINE_VER:
				if ((InRange3(x, x1)) && (InRange3(y, y1)))
				{
					*NewX = x1;
					*NewY = y1 + x2;
					return cnt;
				}

				if ((InRange3(x, x1)) && (InRange3(y, y1 + x2)))
				{
					*NewX = x1;
					*NewY = y1;
					return cnt;
				}

				break;

			case TRACE_DIAG1:
			case PIN_LINE_DIAG1:
				if ((InRange3(x, x1)) && (InRange3(y, y1)))
				{
					*NewX = x1 + x2;
					*NewY = y1 - x2;
					return cnt;
				}

				if ((InRange3(x, x1 + x2)) && (InRange3(y, y1 - x2)))
				{
					*NewX = x1;
					*NewY = y1;
					return cnt;
				}

				break;

			case TRACE_DIAG2:
			case PIN_LINE_DIAG2:
				if ((InRange3(x, x1)) && (InRange3(y, y1)))
				{
					*NewX = x1 + x2;
					*NewY = y1 + x2;
					return cnt;
				}

				if ((InRange3(x, x1 + x2)) && (InRange3(y, y1 + x2)))
				{
					*NewX = x1;
					*NewY = y1;
					return cnt;
				}

				break;
			}
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double GetTraceLengthNet(int32 mode)
{
	int32 cnt, cnt2, NrSegments, LineSegments, SegmentCount, LastLayer, ObjectNr3, ok;

	ObjectRecord *Object;
	double NewX, NewY, TraceLength, x1, y1, x2, y2;
	TracePointRecord *TracePoint;

	int32 Found, Stop;
	double LineBuf[4096];

	AllocateSpecialMem(MEM_POINTS, 256 * 1024, (void **) &TracePoints);
	NrTracePoints = 0;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		Object->Info = 0;
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		switch (Object->ObjectType)
		{
		case PIN_PUT_THROUGH_ROUND:
		case VIA_PUT_THROUGH_ROUND:
		case PIN_SMD_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
		case PIN_SMD_RECT:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
			{
				InsertObjectPoint(x1, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				TracePoint->NrThruHolePads++;
				break;

			case VIA_PUT_THROUGH_ROUND:
				TracePoint->NrVias++;
				break;

			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_SMD_POLYGON:
				TracePoint->NrSmdPads++;
				break;
			}

			break;

		case TRACE_HOR:
		case PIN_LINE_HOR:
			if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
			{
				InsertObjectPoint(x1, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;

			if ((cnt2 = GetTracePointNr(x1 + x2, y1)) == -1)
			{
				InsertObjectPoint(x1 + x2, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;
			break;

		case TRACE_VER:
		case PIN_LINE_VER:
			if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
			{
				InsertObjectPoint(x1, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;

			if ((cnt2 = GetTracePointNr(x1, y1 + x2)) == -1)
			{
				InsertObjectPoint(x1, y1 + x2);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;
			break;

		case TRACE_DIAG1:
		case PIN_LINE_DIAG1:
			if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
			{
				InsertObjectPoint(x1, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;

			if ((cnt2 = GetTracePointNr(x1 + x2, y1 - x2)) == -1)
			{
				InsertObjectPoint(x1 + x2, y1 - x2);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;
			break;

		case TRACE_DIAG2:
		case PIN_LINE_DIAG2:
			if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
			{
				InsertObjectPoint(x1, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;

			if ((cnt2 = GetTracePointNr(x1 + x2, y1 + x2)) == -1)
			{
				InsertObjectPoint(x1 + x2, y1 + x2);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;
			break;

		case TRACE_ALL_ANGLE:
		case PIN_LINE_ALL_ANGLE:
			if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
			{
				InsertObjectPoint(x1, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;

			if ((cnt2 = GetTracePointNr(x2, y2)) == -1)
			{
				InsertObjectPoint(x2, y2);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;
			break;

		case TRACE_ARC:
		case PIN_ARC:
			GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);

			if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
			{
				InsertObjectPoint(x1, y1);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;

			if ((cnt2 = GetTracePointNr(x2, y2)) == -1)
			{
				InsertObjectPoint(x2, y2);
				TracePoint = &((*TracePoints)[NrTracePoints - 1]);
			}
			else
				TracePoint = &((*TracePoints)[cnt2]);

			TracePoint->NrTraces++;
			break;
		}
	}

// Find first point which is connected to only one trace
	Found = 0;
	TraceLength = 0.0;
	cnt = 0;

	while ((!Found) && (cnt < NrTracePoints))
	{
		TracePoint = &((*TracePoints)[cnt]);

		if (TracePoint->NrTraces == 1)
			Found = 1;
		else
			cnt++;
	}

	if (!Found)
		return -10;

	NrSegments = 0;
	Stop = 0;
	LastLayer = -1;

	while (!Stop)
	{
		TracePoint = &((*TracePoints)[cnt]);
		ObjectNr3 = GetTraceObjectNrFromEndPoint(TracePoint->x, TracePoint->y, &NewX, &NewY, 0);

		if (ObjectNr3 != -1)
		{
			Object = &((*Objects3)[ObjectNr3]);
			Object->Info |= 1;

			if ((LastLayer == -1) || (TracePoint->NrVias == 1) || (TracePoint->NrThruHolePads == 1)
			        || (LastLayer == Object->Layer))
			{
				switch (Object->ObjectType)
				{
				case TRACE_HOR:
				case PIN_LINE_HOR:
				case TRACE_VER:
				case PIN_LINE_VER:
					TraceLength += Object->x2;
					break;

				case TRACE_DIAG1:
				case PIN_LINE_DIAG1:
				case TRACE_DIAG2:
				case PIN_LINE_DIAG2:
					TraceLength += (Object->x2 * sqrt(2.0));
					break;

				case TRACE_ARC:
				case PIN_ARC:
					LineSegments =
					    ArcToLineSegments(Object->x1, Object->y1, Object->x2, Object->y2, Object->x3, Object->y3,
					                      Object->x4, Object->y4, (double *) &LineBuf, 0);
					SegmentCount = 0;

					for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
					{
						x1 = LineBuf[SegmentCount];
						SegmentCount++;
						y1 = LineBuf[SegmentCount];
						SegmentCount++;
						x2 = LineBuf[SegmentCount];
						SegmentCount++;
						y2 = LineBuf[SegmentCount];
						SegmentCount++;
						TraceLength += CalcLengthLine(x1, y1, x2, y2);
					}

					break;

				case TRACE_ALL_ANGLE:
				case PIN_LINE_ALL_ANGLE:
					TraceLength += CalcLengthLine(Object->x1, Object->y1, Object->x2, Object->y2);
					break;
				}

				LastLayer = Object->Layer;
				cnt = GetTracePointNr(NewX, NewY);

				if (cnt == -1)
					Stop = 1;

				NrSegments++;
			}
			else
				Stop = 1;
		}
		else
			Stop = 1;
	}

	ok = 1;
	return TraceLength;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
