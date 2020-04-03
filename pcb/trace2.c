/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: trace2.c
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
#include "stdio.h"
#include "memory.h"
#include "graphics.h"
#include "string.h"
#include "select4.h"
#include "select2.h"
#include "trace2.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "math.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "polygon.h"
#include "mainloop.h"
#include "toets.h"


int32 ok, TraceCheck = 1;

extern int32 ObjectKeepOut, ObjectKeepOut2, TracesInserted, TraceDrawingMode;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectsAreCovered(ObjectRecord * TraceObject, ObjectRecord * DrawObject)
/*  ObjectsAreCovered :

0 = Object are not covered (DrawObject will not change)
1 = Object are     covered (DrawObject will change)
    TraceObject should be deleted
2 = Object are     covered (DrawObject will change to nothing)
    TraceObject should be deleted

*/
{
	double TraceX1, TraceY1, TraceX2, DrawX1, DrawY1, DrawX2;
	int32 CountX1, CountX2;
//  ObjectRecord TraceObject,PointObject,PointObject2;

//  nx=AdjustToDrawGridDiv2(x);
//  ny=AdjustToDrawGridDiv2(y);
	if (TraceObject->Layer != DrawObject->Layer)
		return 0;

	if (TraceObject->ObjectType != DrawObject->ObjectType)
		return 0;

	if (NotInRange(TraceObject->y2, DrawObject->y2))
		return 0;

	DrawX1 = DrawObject->x1;
	DrawY1 = DrawObject->y1;
	DrawX2 = DrawObject->x2;
	TraceX1 = TraceObject->x1;
	TraceY1 = TraceObject->y1;
	TraceX2 = TraceObject->x2;

	switch (TraceObject->ObjectType)
	{
	case TRACE_HOR:
		if (NotInRange(TraceY1, DrawY1))
			return 0;

		CountX1 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1, TraceY1, 0);
		CountX2 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1 + TraceX2, TraceY1, 0);

		if (InRange(TraceX1, DrawX1))
		{
			if (NotInRange(TraceX2, DrawX2))
			{
				if ((CountX1 >= 1) && (CountX2 > 1))
				{
					if (DrawX2 > TraceX2)
						DrawObject->x1 = TraceX1 + TraceX2;
					else
						DrawObject->x1 = DrawX1 + DrawX2;

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
			{
//          DrawObject->x2=0;
				return 2;
			}

			return 1;
		}

		if (InRange(TraceX1 + TraceX2, DrawX1))
		{
			if ((CountX1 > 1) && (CountX2 >= 1))
			{
				DrawObject->x1 = TraceX1;
				DrawObject->x2 = TraceX2 + DrawX2;
			}

			return 1;
		}

		if (InRange(TraceX1 + TraceX2, DrawX1 + DrawX2))
		{
			if (NotInRange(TraceX1, DrawX1))
			{
				if ((CountX1 > 1) && (CountX2 >= 1))
				{
					if (DrawX1 > TraceX1)
						DrawObject->x1 = TraceX1;

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
			{
//          DrawObject->x2=0;
				return 2;
			}

			return 1;
		}

		if (InRange(TraceX1, DrawX1 + DrawX2))
		{
			if ((CountX1 >= 1) && (CountX2 > 1))
				DrawObject->x2 = TraceX2 + DrawX2;

			return 1;
		}

		break;

	case TRACE_VER:
		if (NotInRange(TraceX1, DrawX1))
			return 0;

		CountX1 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1, TraceY1, 0);
		CountX2 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1, TraceY1 + TraceX2, 0);

		if (InRange(TraceY1, DrawY1))
		{
			if (NotInRange(TraceX2, DrawX2))
			{
				if ((CountX1 >= 1) && (CountX2 > 1))
				{
					if (DrawX2 > TraceX2)
						DrawObject->y1 = TraceY1 + TraceX2;
					else
						DrawObject->y1 = DrawY1 + DrawX2;

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
			{
//          DrawObject->x2=0;
				return 2;
			}

			return 1;
		}

		if (InRange(TraceY1 + TraceX2, DrawY1))
		{
			DrawObject->y1 = TraceY1;
			DrawObject->x2 = TraceX2 + DrawX2;
			return 1;
		}

		if (InRange(TraceY1 + TraceX2, DrawY1 + DrawX2))
		{
			if (NotInRange(TraceY1, DrawY1))
			{
				if ((CountX1 > 1) && (CountX2 >= 1))
				{
					if (DrawY1 > TraceY1)
						DrawObject->y1 = TraceY1;

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
			{
//          DrawObject->x2=0;
				return 2;
			}

			return 1;
		}

		if (InRange(TraceY1, DrawY1 + DrawX2))
		{
			DrawObject->x2 = TraceX2 + DrawX2;
			return 1;
		}

		if (DrawY1 > TraceY1 + TraceX2)
			return 0;

		if (DrawY1 + DrawX2 < TraceY1)
			return 0;

		switch (DrawObject->Info2)
		{
		case 0:
			DrawObject->x2 = TraceY1 + TraceX2 - DrawY1;
			break;

		case 4:
			DrawObject->y1 = TraceY1;
			DrawObject->x2 = DrawY1 + DrawX2 - TraceY1;
			break;
		}

		return 3;

	case TRACE_DIAG1:
		if (NotInRange(TraceX1 + TraceY1, DrawX1 + DrawY1))
			return 0;

		CountX1 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1, TraceY1, 0);
		CountX2 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1 + TraceX2, TraceY1 - TraceX2, 0);

		if (InRange(TraceX1, DrawX1))
		{
			if (NotInRange(TraceX2, DrawX2))
			{
				if ((CountX1 >= 1) && (CountX2 > 1))
				{
					if (DrawX2 > TraceX2)
					{
						DrawObject->x1 = TraceX1 + TraceX2;
						DrawObject->y1 = TraceY1 - TraceX2;
					}
					else
					{
						DrawObject->x1 = DrawX1 + DrawX2;
						DrawObject->y1 = DrawY1 - DrawX2;
					}

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
				return 2;

			return 1;
		}

		if (InRange(TraceX1 + TraceX2, DrawX1))
		{
			if ((CountX1 > 1) && (CountX2 >= 1))
			{
				DrawObject->x1 = TraceX1;
				DrawObject->y1 = TraceY1;
				DrawObject->x2 = TraceX2 + DrawX2;
			}

			return 1;
		}

		if (InRange(TraceX1 + TraceX2, DrawX1 + DrawX2))
		{
			if (NotInRange(TraceX1, DrawX1))
			{
				if ((CountX1 > 1) && (CountX2 >= 1))
				{
					if (DrawX1 > TraceX1)
					{
						DrawObject->x1 = TraceX1;
						DrawObject->y1 = TraceY1;
					}

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
			{
//          DrawObject->x2=0;
				return 2;
			}

			return 1;
		}

		if (InRange(TraceX1, DrawX1 + DrawX2))
		{
			if ((CountX1 >= 1) && (CountX2 > 1))
				DrawObject->x2 = TraceX2 + DrawX2;

			return 1;
		}

		break;

	case TRACE_DIAG2:
		if (NotInRange(-TraceX1 + TraceY1, -DrawX1 + DrawY1))
			return 0;

		CountX1 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1, TraceY1, 0);
		CountX2 = CountNrObjectsEndPoint(TraceObject->Layer, TraceX1 + TraceX2, TraceY1 + TraceX2, 0);

		if (InRange(TraceX1, DrawX1))
		{
			if (NotInRange(TraceX2, DrawX2))
			{
				if ((CountX1 >= 1) && (CountX2 > 1))
				{
					if (DrawX2 > TraceX2)
					{
						DrawObject->x1 = TraceX1 + TraceX2;
						DrawObject->y1 = TraceY1 + TraceX2;
					}
					else
					{
						DrawObject->x1 = DrawX1 + DrawX2;
						DrawObject->y1 = DrawY1 + DrawX2;
					}

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
			{
//          DrawObject->x2=0;
				return 2;
			}

			return 1;
		}

		if (InRange(TraceX1 + TraceX2, DrawX1))
		{
			if ((CountX1 > 1) && (CountX2 >= 1))
			{
				DrawObject->x1 = TraceX1;
				DrawObject->y1 = TraceY1;
				DrawObject->x2 = TraceX2 + DrawX2;
			}

			return 1;
		}

		if (InRange(TraceX1 + TraceX2, DrawX1 + DrawX2))
		{
			if (NotInRange(TraceX1, DrawX1))
			{
				if ((CountX1 > 1) && (CountX2 >= 1))
				{
					if (DrawX1 > TraceX1)
					{
						DrawObject->x1 = TraceX1;
						DrawObject->y1 = TraceY1;
					}

					DrawObject->x2 = fabs(TraceX2 - DrawX2);
				}
			}
			else
			{
//          DrawObject->x2=0;
				return 2;
			}

			return 1;
		}

		if (InRange(TraceX1, DrawX1 + DrawX2))
		{
			if ((CountX1 >= 1) && (CountX2 > 1))
				DrawObject->x2 = TraceX2 + DrawX2;

			return 1;
		}

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ExtendNewTrace(ObjectRecord * DrawTrace)
{

	/*

	ExtendNewTrace checks DrawTrace overlapped an existing trace.

	If this is true the new trace will be a combination of the DrawTrace
	and the overlapped trace

	The return value will be the index Objects3 of the overlapped trace,

	If there is no overlapped trace the return value will be -1

	*/

	int32 ObjectsCovered, cnt;
	ObjectRecord *ConnectObject;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		ConnectObject = &((*Objects3)[cnt]);

		if ((DrawTrace->ObjectType == ConnectObject->ObjectType) && (ConnectObject->Info2 == 0))
		{
			switch (ConnectObject->ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
				if ((ObjectsCovered = ObjectsAreCovered(ConnectObject, DrawTrace)) > 0)
					return cnt;
			}
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CalcDoubleTrace()
{
	double MinDivXY, DivX, DivY, DivXabs, DivYabs;

	DrawTrace2.Clearance = DrawTrace1.Clearance;
	DrawTrace2.Layer = DrawTrace1.Layer;
	DrawTrace2.y2 = DrawTrace1.y2;
	DrawTrace2.NetNr = DrawTrace1.NetNr;
	DivX = StartFirstDrawingTraceX - DrawingConnectionX2;
	DivY = StartFirstDrawingTraceY - DrawingConnectionY2;
	DivXabs = fabs(DivX);
	DivYabs = fabs(DivY);
	MinDivXY = min(DivXabs, DivYabs);

	switch (DrawTrace1.Info2)
	{
	case 0:
		if ((DivY < 0) && (DivYabs > DivXabs))
		{
			DrawTrace1.x1 = StartFirstDrawingTraceX;
			DrawTrace1.y1 = StartFirstDrawingTraceY;
			DrawTrace1.x2 = DivYabs - DivXabs;

			if (InRange(DivX, 0))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivX < 0)
				{
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = StartFirstDrawingTraceX;
					DrawTrace2.y1 = DrawingConnectionY2 - DivXabs;
					DrawTrace2.x2 = DivXabs;
					DrawTrace2.Info2 = 1;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = DrawingConnectionY2;
					DrawTrace2.x2 = DivXabs;
					DrawTrace2.Info2 = 7;
				}
			}
		}
		else
			return 0;

		break;

	case 1:
		if ((DivY < 0) && (DivX < 0))
		{
			DrawTrace1.x1 = StartFirstDrawingTraceX;
			DrawTrace1.y1 = StartFirstDrawingTraceY;
			DrawTrace1.x2 = MinDivXY;

			if (InRange(DivX, DivY))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivXabs < DivYabs)
				{
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = StartFirstDrawingTraceX + MinDivXY;
					DrawTrace2.y1 = StartFirstDrawingTraceY + MinDivXY;
					DrawTrace2.x2 = DivYabs - MinDivXY;
					DrawTrace2.Info2 = 0;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = StartFirstDrawingTraceX + MinDivXY;
					DrawTrace2.y1 = StartFirstDrawingTraceY + MinDivXY;
					DrawTrace2.x2 = DivXabs - MinDivXY;
					DrawTrace2.Info2 = 2;
				}
			}
		}
		else
			return 0;

		break;

	case 2:
		if ((DivX < 0) && (DivXabs > DivYabs))
		{
			DrawTrace1.x1 = StartFirstDrawingTraceX;
			DrawTrace1.y1 = StartFirstDrawingTraceY;
			DrawTrace1.x2 = DivXabs - MinDivXY;

			if (InRange(DivY, 0))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivY < 0)
				{
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = DrawingConnectionX2 - MinDivXY;
					DrawTrace2.y1 = StartFirstDrawingTraceY;
					DrawTrace2.x2 = MinDivXY;
					DrawTrace2.Info2 = 1;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = DrawingConnectionX2 - MinDivXY;
					DrawTrace2.y1 = StartFirstDrawingTraceY;
					DrawTrace2.x2 = MinDivXY;
					DrawTrace2.Info2 = 3;
				}
			}
		}
		else
			return 0;

		break;

	case 3:
		if ((DivY > 0) && (DivX < 0))
		{
			DrawTrace1.x1 = StartFirstDrawingTraceX;
			DrawTrace1.y1 = StartFirstDrawingTraceY;
			DrawTrace1.x2 = MinDivXY;

			if (InRange(-DivX, DivY))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivXabs < DivYabs)
				{
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = DrawingConnectionY2;
					DrawTrace2.x2 = DivYabs - MinDivXY;
					DrawTrace2.Info2 = 4;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = StartFirstDrawingTraceX + MinDivXY;
					DrawTrace2.y1 = StartFirstDrawingTraceY - MinDivXY;
					DrawTrace2.x2 = DivXabs - MinDivXY;
					DrawTrace2.Info2 = 2;
				}
			}
		}
		else
			return 0;

		break;

	case 4:
		if ((DivY > 0) && (DivYabs > DivXabs))
		{
			DrawTrace1.x1 = StartFirstDrawingTraceX;
			DrawTrace1.y1 = DrawingConnectionY2 + MinDivXY;
			DrawTrace1.x2 = DivYabs - MinDivXY;

			if (InRange(DivX, 0))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivX < 0)
				{
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = StartFirstDrawingTraceX;
					DrawTrace2.y1 = DrawingConnectionY2 + MinDivXY;
					DrawTrace2.x2 = DivXabs;
					DrawTrace2.Info2 = 3;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = StartFirstDrawingTraceX - DivXabs;
					DrawTrace2.y1 = DrawingConnectionY2;
					DrawTrace2.x2 = DivXabs;
					DrawTrace2.Info2 = 5;
				}
			}
		}
		else
			return 0;

		break;

	case 5:
		if ((DivY > 0) && (DivX > 0))
		{
			DrawTrace1.x1 = StartFirstDrawingTraceX - MinDivXY;
			DrawTrace1.y1 = StartFirstDrawingTraceY - MinDivXY;
			DrawTrace1.x2 = MinDivXY;

			if (InRange(DivX, DivY))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivXabs < DivYabs)
				{
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = DrawingConnectionY2;
					DrawTrace2.x2 = DivYabs - MinDivXY;
					DrawTrace2.Info2 = 4;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = DrawingConnectionY2;
					DrawTrace2.x2 = DivXabs - MinDivXY;
					DrawTrace2.Info2 = 6;
				}
			}
		}
		else
			return 0;

		break;

	case 6:
		if ((DivX > 0) && (DivXabs > DivYabs))
		{
			DrawTrace1.x1 = DrawingConnectionX2 + MinDivXY;
			DrawTrace1.y1 = StartFirstDrawingTraceY;
			DrawTrace1.x2 = DivXabs - MinDivXY;

			if (InRange(DivY, 0))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivY < 0)
				{
					DrawTrace2.ObjectType = TRACE_DIAG1;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = DrawingConnectionY2;
					DrawTrace2.x2 = MinDivXY;
					DrawTrace2.Info2 = 7;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_DIAG2;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = DrawingConnectionY2;
					DrawTrace2.x2 = MinDivXY;
					DrawTrace2.Info2 = 5;
				}
			}
		}
		else
			return 0;

		break;

	case 7:
		if ((DivY < 0) && (DivX > 0))
		{
			DrawTrace1.x1 = StartFirstDrawingTraceX - MinDivXY;
			DrawTrace1.y1 = StartFirstDrawingTraceY + MinDivXY;
			DrawTrace1.x2 = MinDivXY;

			if (InRange(-DivX, DivY))
				DrawTrace2.ObjectType = 0;
			else
			{
				if (DivXabs < DivYabs)
				{
					DrawTrace2.ObjectType = TRACE_VER;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = StartFirstDrawingTraceY + MinDivXY;
					DrawTrace2.x2 = DivYabs - MinDivXY;
					DrawTrace2.Info2 = 0;
				}
				else
				{
					DrawTrace2.ObjectType = TRACE_HOR;
					DrawTrace2.x1 = DrawingConnectionX2;
					DrawTrace2.y1 = StartFirstDrawingTraceY + MinDivXY;
					DrawTrace2.x2 = DivXabs - MinDivXY;
					DrawTrace2.Info2 = 6;
				}
			}
		}
		else
			return 0;

		break;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckIfTraceFitAndAdjust(ObjectRecord * DrawTrace, int32 mode)
{
	double NewX, NewY;

	FillPositionObject(DrawTrace);
#if 0

	if (AltPressed)
		return 1;

#endif

	if ((TraceDrawingMode == 2) || (TraceDrawingMode == 3))
	{
		if ((DrawTrace->x2 > 0.0) && (TraceCheck) && (CheckObjectOverlapped(DrawTrace, 0)))
			return 2;

		return 0;
	}

	DrawTrace->Test = 0;

	if ((DrawTrace->x2 > 0.0) && (TraceCheck)
//     &&
//     ((CheckSpecialVersion(0)==0) || (!CtrlPressed))
	        && (CheckObjectOverlapped(DrawTrace, 0)))
	{
		NewX = CurrentDrawX1;
		NewY = CurrentDrawY1;

		if (mode == 0)
		{
			if (!FindLimitTrace
			        (StartFirstDrawingTraceX, StartFirstDrawingTraceY, EndFirstDrawingTraceX, EndFirstDrawingTraceY,
			         DrawTrace->Thickness, DrawTrace->Clearance, &NewX, &NewY, DrawTrace->NetNr, CurrentDrawingLayer))
				return -1;
		}
		else
		{
			if (!FindLimitTrace
			        (StartSecondDrawingTraceX, StartSecondDrawingTraceY, EndSecondDrawingTraceX, EndSecondDrawingTraceY,
			         DrawTrace->Thickness, DrawTrace->Clearance, &NewX, &NewY, DrawTrace->NetNr, CurrentDrawingLayer))
				return -1;
		}

		if (((mode == 0)
		        && ((NotInRange(NewX, StartFirstDrawingTraceX)) || (NotInRange(NewY, StartFirstDrawingTraceY))))
		        || ((mode == 1)
		            && ((NotInRange(NewX, StartSecondDrawingTraceX)) || (NotInRange(NewY, StartSecondDrawingTraceY)))))
		{
			switch (DrawTrace->Info2)
			{
			case 0:			// up
				DrawTrace->x2 = NewY - DrawTrace->y1;
				DrawTrace->Test = 1;
				break;

			case 1:
				DrawTrace->x2 = NewX - DrawTrace->x1;
				DrawTrace->Test = 1;
				break;

			case 2:
				DrawTrace->x2 = NewX - DrawTrace->x1;
				DrawTrace->Test = 1;
				break;

			case 3:
				DrawTrace->x2 = NewX - DrawTrace->x1;
				DrawTrace->Test = 1;
				break;

			case 4:
				DrawTrace->x2 = DrawTrace->x2 + (DrawTrace->y1 - NewY);
				DrawTrace->x1 = NewX;
				DrawTrace->y1 = NewY;
				DrawTrace->Test = 1;
				break;

			case 5:
				DrawTrace->x2 = DrawTrace->x2 + (DrawTrace->x1 - NewX);
				DrawTrace->x1 = NewX;
				DrawTrace->y1 = NewY;
				DrawTrace->Test = 1;
				break;

			case 6:
				DrawTrace->x2 = DrawTrace->x2 + (DrawTrace->x1 - NewX);
				DrawTrace->x1 = NewX;
				DrawTrace->y1 = NewY;
				DrawTrace->Test = 1;
				break;

			case 7:
				DrawTrace->x2 = DrawTrace->x2 + (DrawTrace->x1 - NewX);
				DrawTrace->x1 = NewX;
				DrawTrace->y1 = NewY;
				DrawTrace->Test = 1;
				break;

			case -1:			// All angle line
				DrawTrace->x2 = NewX;
				DrawTrace->y2 = NewY;
				DrawTrace->Test = 1;
				break;
			}

			if (DrawTrace->Test == 1)
			{
				if (mode == 0)
				{
					EndFirstDrawingTraceX = NewX;
					EndFirstDrawingTraceY = NewY;
				}
				else
				{
					EndSecondDrawingTraceX = NewX;
					EndSecondDrawingTraceY = NewY;
				}
			}

			FillPositionObject(DrawTrace);

			if ((DrawTrace->x2 > 0.0) && (CheckObjectOverlapped(DrawTrace, 0)))
				return 2;

			return 1;
		}
		else
			return -1;
	}

	return 0;
}

// *******************************************************************************
// *******************************************************************************
// *******************************************************************************
// *******************************************************************************

int32 CheckIfViaAtPoint(double x, double y)
{
	int32 cnt;
	double x1, y1;
	ObjectRecord *Object;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;

		if ((Object->ObjectType == VIA_PUT_THROUGH_ROUND) && (InRange(x1, x)) && (InRange(y1, y)))
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteAndUnDisplayObjectTrace(int32 ObjectNr, int32 RePaint)
{
	ObjectRecord *ConnectObject = NULL;
	TraceRecord *Trace;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;
#ifdef _DEBUG
	int32 ok;
#endif

	if (ObjectNr == -1)
		return;

	if ((ObjectNr >= 0) && (ObjectNr < NrObjects3))
	{
		ConnectObject = &((*Objects3)[ObjectNr]);

		if (!RePaint)
		{
			StartDrawingEditingWindow();
			DrawCode = DrawLayerCode[CurrentDrawingLayer];
			SetBackGroundActive(0);
			DrawObject(ConnectObject, 0);

			if (OkToDrawClearances)
			{
				SetBackGroundActive(0);
				InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
				DrawObject(ConnectObject, 0x40 + 8);
			}
		}
	}

	if (CurrentDrawingLayer != -1)
	{
#ifdef _DEBUG

		if (!ConnectObject)
			ok = 1;

#endif

		if (ConnectObject)
		{
			ZeroUnusedObjects(0);

			switch (ConnectObject->ObjectType)
			{
			case TRACE_HOR:
				Trace = &((*HorTraces[CurrentDrawingLayer])[ConnectObject->TraceNr]);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				ConnectObject->Info2 = 1;
				break;

			case TRACE_VER:
				Trace = &((*VerTraces[CurrentDrawingLayer])[ConnectObject->TraceNr]);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				ConnectObject->Info2 = 1;
				break;

			case TRACE_DIAG1:
				Trace = &((*Diag1Traces[CurrentDrawingLayer])[ConnectObject->TraceNr]);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				ConnectObject->Info2 = 1;
				break;

			case TRACE_DIAG2:
				Trace = &((*Diag2Traces[CurrentDrawingLayer])[ConnectObject->TraceNr]);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
				ConnectObject->Info2 = 1;
				break;

			case TRACE_ALL_ANGLE:
				ObjectLine = &((*ObjectLines)[ConnectObject->TraceNr]);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				ObjectLine->DeleteNr = (int16) LastActionNr;
				ConnectObject->Info2 = 1;
				break;

			case TRACE_ARC:
				ObjectArc = &((*ObjectArcs)[ConnectObject->TraceNr]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				ConnectObject->Info2 = 1;
				break;
			}
		}
	}

	if (!RePaint)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InsertTraces(int32 mode, int32 EndPointObjectNr)
{
	int32 cnt, ok, ConnectionObjectNr, CurrentTraceNr, OkToRePaint, res, count1, count2, TempLastActionNr;
	double x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, Angle1, Angle2;
	int32 RepaintNet, TraceDeleted, OkToAddSecondTrace, ObjectInAreaFillInserted;
	NetRecord *Net;

// InsertTraces is 1 if an endpoint is reached
// If DrawTrace1.x2=0 Trace backwards

	ViaRecord *Via;
	ObjectRecord *ConnectObject, RebuildTraceObject, CopyDrawTrace1, *EndPointObject;

	OkToRePaint = 0;
	RepaintNet = 0;
	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;
	x5 = 0.0;
	y5 = 0.0;
	ObjectInAreaFillInserted = 0;
	EndPointObject = NULL;
	ObjectKeepOut = 0;
	ObjectKeepOut2 = 0;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		ConnectObject = &((*Objects3)[cnt]);
		ConnectObject->Info2 = 0;
	}

	if (EndPointObjectNr != -1)
	{	// Endpoint reached
		ok = 1;
		EndPointObject = &((*Objects3)[EndPointObjectNr]);
	}

#ifdef _DEBUG

	if (TracesInserted == 1)
		ok = 1;

	if (InRange(DrawTrace1.x2, 0.0))
		ok = 1;

#endif
	TempLastActionNr = (int16) LastActionNr;
	Net = &((*Nets)[CurrentDrawingNetNr]);
	OkToAddSecondTrace = 1;

// *******************************************************************************
	if (DrawTrace1.x2 > 0)
	{
		if ((res = CheckIfTraceFitAndAdjust(&DrawTrace1, 0)) == -1)
			return 1;

		if (((TraceDrawingMode == 2) || (TraceDrawingMode == 3)) && (res == 2))
		{	// Trace arc does not fit
			return 1;
		}

		if ((res > 0)			// res>0 first trace will be cut -> do not add second trace
		        || (DrawTrace2.Info2 == -1)	// DrawTrace2 does not exist
		        || (InRange(DrawTrace2.x2, 0.0)))
			OkToAddSecondTrace = 0;
	}

// *******************************************************************************
	if (DrawTrace1.x2 > 0)
	{
		memmove(&CopyDrawTrace1, &DrawTrace1, sizeof(ObjectRecord));

		if (TraceDrawingMode == 0)
			res = ExtendNewTrace(&CopyDrawTrace1);
		else
			res = -1;

		if ((CurrentWorkingTrace.Info2 & 2) == 0)
		{
			ok = 1;

// If the DrawTrace overlaps the underlying CurrentWorkingTrace -> delete trace
			if ((CurrentWorkingTrace.ObjectType == DrawTrace1.ObjectType)
			        && (InRange(CurrentWorkingTrace.x1, DrawTrace1.x1)) && (InRange(CurrentWorkingTrace.y1, DrawTrace1.y1))
			        && (InRange(CurrentWorkingTrace.x2, DrawTrace1.x2)))
			{
				DrawTrace1.x2 = 0.0;
				EndPointObjectNr = -1;
				OkToAddSecondTrace = 0;
				EndPreviousTraceX = EndFirstDrawingTraceX;
				EndPreviousTraceY = EndFirstDrawingTraceY;

				if (res != -1)
				{
					ZeroUnusedObjects(0);

					if (!OkToRePaintAfterTraceDrawing)
						DeleteAndUnDisplayObjectTrace(res, 0);
					else
						DeleteAndUnDisplayObjectTrace(res, 1);
				}
			}
			else
			{
				if (res != -1)
				{
					ZeroUnusedObjects(0);

					if (!OkToRePaintAfterTraceDrawing)
						DeleteAndUnDisplayObjectTrace(res, 0);
					else
						DeleteAndUnDisplayObjectTrace(res, 1);

					res = ExtendNewTrace(&CopyDrawTrace1);

					if (res != -1)
					{
						ok = 1;
						ZeroUnusedObjects(0);

						if (!OkToRePaintAfterTraceDrawing)
							DeleteAndUnDisplayObjectTrace(res, 0);
						else
							DeleteAndUnDisplayObjectTrace(res, 1);
					}

					memmove(&DrawTrace1, &CopyDrawTrace1, sizeof(ObjectRecord));
				}
			}
		}
		else
		{
			if (OkToAddSecondTrace)
			{
				if (res != -1)
				{
					ZeroUnusedObjects(0);

					if (!OkToRePaintAfterTraceDrawing)
						DeleteAndUnDisplayObjectTrace(res, 0);
					else
						DeleteAndUnDisplayObjectTrace(res, 1);

					if ((CurrentWorkingTrace.TraceNr != -1) && (res != CurrentWorkingTrace.TraceNr))
						memmove(&DrawTrace1, &CopyDrawTrace1, sizeof(ObjectRecord));
				}
			}
			else
				res = -1;

			if ((CurrentWorkingTrace.TraceNr != -1) && ((res == -1) || (res != CurrentWorkingTrace.TraceNr)))
			{
				ZeroUnusedObjects(0);

				if (!OkToRePaintAfterTraceDrawing)
					DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 0);
				else
					DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 1);
			}
		}
	}

// *****************************************************************************
// *****************************************************************************
// Add new trace1

	if (DrawTrace1.x2 > 0)
	{
		DrawTrace1.Info = 0;

		if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
			DrawTrace1.Info = OBJECT_HIGHLITED;

		DrawTrace1.Layer = CurrentDrawingLayer;

		if (CheckObjectInsideAreaFillPowerPlane(&DrawTrace1) == 0)
		{
			if (!AddTrace(&DrawTrace1))
				return 0;

			TracesInserted++;

			switch (TraceDrawingMode)
			{
			case 0:
				PreviousDirection =
				    GetNewDirection(StartFirstDrawingTraceX, StartFirstDrawingTraceY, EndFirstDrawingTraceX,
				                    EndFirstDrawingTraceY, 0);
				break;

			case 1:
				PreviousDirection =
				    GetNewDirection(StartFirstDrawingTraceX, StartFirstDrawingTraceY, EndFirstDrawingTraceX,
				                    EndFirstDrawingTraceY, 0);
				break;

			case 2:
			case 3:
				PreviousDirection = DrawTrace1.Info2;
				break;
			}

			if ((RecalcAreafillAfterInsert) && (TraceCheck))
			{
				if (!AltPressed)
				{
					res = InsertObjectInAreaFill(&DrawTrace1, DrawTrace1.Layer, DrawTrace1.NetNr, 2);

					if (res == 1)
						ObjectInAreaFillInserted = 1;
				}
			}

			// Check if trace on the endpoint needs to be cut or removed
			if (EndPointObjectNr != -1)
			{
				ok = 1;
				GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
				x3 = EndPointObject->x1;
				y3 = EndPointObject->y1;

				switch (EndPointObject->ObjectType)
				{
				case TRACE_HOR:
					x4 = EndPointObject->x1 + EndPointObject->x2;
					y4 = EndPointObject->y1;
					break;

				case TRACE_VER:
					x4 = EndPointObject->x1;
					y4 = EndPointObject->y1 + EndPointObject->x2;
					break;

				case TRACE_DIAG1:
					x4 = EndPointObject->x1 + EndPointObject->x2;
					y4 = EndPointObject->y1 - EndPointObject->x2;
					break;

				case TRACE_DIAG2:
					x4 = EndPointObject->x1 + EndPointObject->x2;
					y4 = EndPointObject->y1 + EndPointObject->x2;
					break;

				case TRACE_ALL_ANGLE:
					x4 = EndPointObject->x2;
					y4 = EndPointObject->y2;
					break;
//          case TRACE_ARC:
//            GetArcEndPoints(EndPointObject,&x3,&y3,&x4,&y4,0);
//            break;
				}

				if (ObjectIsTrace(EndPointObject))
				{
					count1 = CountNrObjectsEndPoint(DrawTrace2.Layer, x3, y3, 0);
					count2 = CountNrObjectsEndPoint(DrawTrace2.Layer, x4, y4, 0);
					memmove(&RebuildTraceObject, EndPointObject, sizeof(RebuildTraceObject));

					if ((InRange(EndSecondDrawingTraceX, x3)) && (InRange(EndSecondDrawingTraceY, y3)))
						ok = 1;
					else
					{
						if ((InRange(EndSecondDrawingTraceX, x4)) && (InRange(EndSecondDrawingTraceY, y4)))
							ok = 1;
						else
						{
							if ((count1 == 1) && (count2 == 2))
							{
								RebuildTraceObject.x1 = EndSecondDrawingTraceX;
								RebuildTraceObject.y1 = EndSecondDrawingTraceY;

								switch (EndPointObject->ObjectType)
								{
								case TRACE_HOR:
									x5 = EndSecondDrawingTraceX - EndPointObject->x1;
									RebuildTraceObject.x2 = EndPointObject->x2 - x5;
									break;

								case TRACE_VER:
									x5 = EndSecondDrawingTraceY - EndPointObject->y1;
									RebuildTraceObject.x2 = EndPointObject->x2 - x5;
									break;

								case TRACE_DIAG1:
									x5 = EndSecondDrawingTraceX - EndPointObject->x1;
									RebuildTraceObject.x2 = EndPointObject->x2 - x5;
									break;

								case TRACE_DIAG2:
									x5 = EndSecondDrawingTraceX - EndPointObject->x1;
									RebuildTraceObject.x2 = EndPointObject->x2 - x5;
									break;

								case TRACE_ALL_ANGLE:
									break;

								case TRACE_ARC:
									break;
								}

								ZeroUnusedObjects(0);

								if (!CheckObjectOverlapped(&RebuildTraceObject, 0))
								{
									if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
										DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 0);
									else
										DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 1);

									if (!AddTrace(&RebuildTraceObject))
										return 0;
								}

								ok = 1;
							}
							else
							{
								if ((count1 == 2) && (count2 == 1))
								{
									if (EndPointObject->ObjectType != TRACE_VER)
									{
										x5 = EndSecondDrawingTraceX - EndPointObject->x1;
										RebuildTraceObject.x2 = x5;
									}
									else
									{
										x5 = EndSecondDrawingTraceY - EndPointObject->y1;
										RebuildTraceObject.x2 = x5;
									}

									ZeroUnusedObjects(0);

									if (!CheckObjectOverlapped(&RebuildTraceObject, 0))
									{
										if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
											DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 0);
										else
											DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 1);

										if (!AddTrace(&RebuildTraceObject))
											return 0;
									}

									ok = 1;
								}
							}

							ok = 1;
						}
					}
				}
			}

			GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
			RepaintNet = 1;
			memmove(&CurrentWorkingTrace, &DrawTrace1, sizeof(ObjectRecord));
			CurrentWorkingTrace.TraceNr = FindObjectNrFromNetObjects(&DrawTrace1);
			CurrentWorkingTrace.Info2 = 0;
			CurrentDrawX1 = EndFirstDrawingTraceX;
			CurrentDrawY1 = EndFirstDrawingTraceY;
			EndPreviousTraceX = CurrentDrawX1;
			EndPreviousTraceY = CurrentDrawY1;
//      EndPreviousTraceX=StartFirstDrawingTraceX;
//      EndPreviousTraceY=StartFirstDrawingTraceY;
		}
		else
		{
			OkToAddSecondTrace = 0;
			MessageBoxOwn(PCBWindow, SC(1063, "Can not draw on powerplane"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
			Beep(1000, 200);
		}

// *****************************************************************************
// Add new trace2
		if ((OkToAddSecondTrace) && (DrawTwoTryingTraces))
		{
			if (CheckIfTraceFitAndAdjust(&DrawTrace2, 1) != -1)
			{
				DrawTrace2.Info = 0;

				switch (TraceDrawingMode)
				{
				case 0:
					PreviousDirection =
					    GetNewDirection(StartSecondDrawingTraceX, StartSecondDrawingTraceY, EndSecondDrawingTraceX,
					                    EndSecondDrawingTraceY, 0);
					break;

				case 1:
					break;

				case 2:
				case 3:
					PreviousDirection = DrawTrace2.Info2;
					break;
				}

				if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
					DrawTrace2.Info = OBJECT_HIGHLITED;

				DrawTrace2.Layer = CurrentDrawingLayer;

				if (CheckObjectInsideAreaFillPowerPlane(&DrawTrace2) == 0)
				{
					if ((res = ExtendNewTrace(&DrawTrace2)) != -1)
					{
						ZeroUnusedObjects(0);

						if (!OkToRePaintAfterTraceDrawing)
							DeleteAndUnDisplayObjectTrace(res, 0);
						else
							DeleteAndUnDisplayObjectTrace(res, 1);
					}

					if (!AddTrace(&DrawTrace2))
						return 0;

					TracesInserted++;

					if ((RecalcAreafillAfterInsert) && (TraceCheck))
					{
						if (!AltPressed)
						{
							res = InsertObjectInAreaFill(&DrawTrace2, DrawTrace2.Layer, DrawTrace2.NetNr, 2);

							if (res == 1)
								ObjectInAreaFillInserted = 1;
						}
					}

					// Check if trace on the endpoint needs to be cut or removed
					if (EndPointObjectNr != -1)
					{
						ok = 1;
						GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
						x3 = EndPointObject->x1;
						y3 = EndPointObject->y1;

						switch (EndPointObject->ObjectType)
						{
						case TRACE_HOR:
							x4 = EndPointObject->x1 + EndPointObject->x2;
							y4 = EndPointObject->y1;
							break;

						case TRACE_VER:
							x4 = EndPointObject->x1;
							y4 = EndPointObject->y1 + EndPointObject->x2;
							break;

						case TRACE_DIAG1:
							x4 = EndPointObject->x1 + EndPointObject->x2;
							y4 = EndPointObject->y1 - EndPointObject->x2;
							break;

						case TRACE_DIAG2:
							x4 = EndPointObject->x1 + EndPointObject->x2;
							y4 = EndPointObject->y1 + EndPointObject->x2;
							break;

						case TRACE_ALL_ANGLE:
							x4 = EndPointObject->x2;
							y4 = EndPointObject->y2;
							break;
//              case TRACE_ARC:
//                break;
						}

						if (ObjectIsTrace(EndPointObject))
						{
							count1 = CountNrObjectsEndPoint(DrawTrace2.Layer, x3, y3, 0);
							count2 = CountNrObjectsEndPoint(DrawTrace2.Layer, x4, y4, 0);
							memmove(&RebuildTraceObject, EndPointObject, sizeof(RebuildTraceObject));

							if ((InRange(EndSecondDrawingTraceX, x3)) && (InRange(EndSecondDrawingTraceY, y3)))
								ok = 1;
							else
							{
								if ((InRange(EndSecondDrawingTraceX, x4)) && (InRange(EndSecondDrawingTraceY, y4)))
									ok = 1;
								else
								{
									if ((count1 == 1) && (count2 == 2))
									{
										RebuildTraceObject.x1 = EndSecondDrawingTraceX;
										RebuildTraceObject.y1 = EndSecondDrawingTraceY;

										switch (EndPointObject->ObjectType)
										{
										case TRACE_HOR:
											x5 = EndSecondDrawingTraceX - EndPointObject->x1;
											RebuildTraceObject.x2 = EndPointObject->x2 - x5;
											break;

										case TRACE_VER:
											x5 = EndSecondDrawingTraceY - EndPointObject->y1;
											RebuildTraceObject.x2 = EndPointObject->x2 - x5;
											break;

										case TRACE_DIAG1:
											x5 = EndSecondDrawingTraceX - EndPointObject->x1;
											RebuildTraceObject.x2 = EndPointObject->x2 - x5;
											break;

										case TRACE_DIAG2:
											x5 = EndSecondDrawingTraceX - EndPointObject->x1;
											RebuildTraceObject.x2 = EndPointObject->x2 - x5;
											break;

										case TRACE_ALL_ANGLE:
											break;

										case TRACE_ARC:
											break;
										}

										ZeroUnusedObjects(0);

										if (!CheckObjectOverlapped(&RebuildTraceObject, 0))
										{
											if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
												DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 0);
											else
												DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 1);

											if (!AddTrace(&RebuildTraceObject))
												return 0;
										}

										ok = 1;
									}
									else
									{
										if ((count1 == 2) && (count2 == 1))
										{
											if (EndPointObject->ObjectType != TRACE_VER)
											{
												x5 = EndSecondDrawingTraceX - EndPointObject->x1;
												RebuildTraceObject.x2 = x5;
											}
											else
											{
												x5 = EndSecondDrawingTraceY - EndPointObject->y1;
												RebuildTraceObject.x2 = x5;
											}

											ZeroUnusedObjects(0);

											if (!CheckObjectOverlapped(&RebuildTraceObject, 0))
											{
												if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
													DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 0);
												else
													DeleteAndUnDisplayObjectTrace(EndPointObjectNr, 1);

												if (!AddTrace(&RebuildTraceObject))
													return 0;
											}

											ok = 1;
										}
									}

									ok = 1;
								}
							}
						}
					}

					RepaintNet = 1;
					GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
					memmove(&CurrentWorkingTrace, &DrawTrace2, sizeof(ObjectRecord));
					CurrentWorkingTrace.TraceNr = FindObjectNrFromNetObjects(&DrawTrace2);
					CurrentWorkingTrace.Info2 = 0;
					CurrentDrawX1 = EndSecondDrawingTraceX;
					CurrentDrawY1 = EndSecondDrawingTraceY;
					EndPreviousTraceX = EndSecondDrawingTraceX;
					EndPreviousTraceY = EndSecondDrawingTraceY;
//          EndPreviousTraceX=StartSecondDrawingTraceX;
//          EndPreviousTraceY=StartSecondDrawingTraceY;
//          EndPreviousTraceX=CurrentDrawX1;
//          EndPreviousTraceY=CurrentDrawY1;
				}
				else
				{
					MessageBoxOwn(PCBWindow, SC(1063, "Can not draw on powerplane"), SC(24, "Error"),
					              MB_APPLMODAL | MB_OK);
					Beep(1000, 200);
				}
			}
			else
				EndPointObjectNr = -1;
		}

//    if (EndPointObjectNr!=-1) {
//    }
		if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
			ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
		else
		{
			ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 1);
			RePaint();
		}

		if (RepaintNet)
		{
			if (ObjectInAreaFillInserted)
				CheckInputMessages(0);

			LastActionNr++;

			if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
			{
				StartDrawingEditingWindow();
				ReDisplayNet(0);
				ExitDrawing();
				EndDrawingEditingWindow();
			}
		}

		if (EndPointObjectNr != -1)
			return 0;

		return 1;
	}

// *****************************************************************************
// *****************************************************************************
// Find the next via/trace connected to the deleted trace at the mouse cursor
// If thers is a via and trace then the via is favorit

#ifdef _DEBUG

	if (InRange(DrawTrace1.x2, 0.0))
		ok = 1;

#endif
	TraceDeleted = 0;
	CurrentTraceNr = -1;

	if (CurrentWorkingTrace.ObjectType != 0)
	{
		if (CurrentWorkingTrace.Info2 == 3)
		{
			CurrentTraceNr = CurrentWorkingTrace.TraceNr;
			ZeroUnusedObjects(0);

			if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
				DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 0);
			else
				DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 1);

			TraceDeleted = 1;
		}
	}

	GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
	ConnectionObjectNr = GetObjectNrFromEndPoint(DrawTrace1.Layer, EndPreviousTraceX, EndPreviousTraceY, 1);

	if (ConnectionObjectNr == -1)
		ConnectionObjectNr = GetObjectNrFromEndPoint(-1, EndPreviousTraceX, EndPreviousTraceY, 1);
	else
	{
		ConnectObject = &((*Objects3)[ConnectionObjectNr]);

		switch (ConnectObject->ObjectType)
		{
		case PIN_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			ConnectionObjectNr = GetObjectNrFromEndPoint(-1, EndPreviousTraceX, EndPreviousTraceY, 2);
			break;
		}
	}

	if (ConnectionObjectNr == -1)
	{	// If nothing connected to the deleted trace exit
		ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
		LastActionNr++;
		return 0;
	}

	ConnectObject = &((*Objects3)[ConnectionObjectNr]);

	if (ConnectObject->ObjectType == VIA_PUT_THROUGH_ROUND)
	{
// Delete the found via
// NrObjects3
		ZeroUnusedObjects(0);
		Via = &((*Vias)[ConnectObject->TraceNr]);
		Via->Info |= OBJECT_NOT_VISIBLE;
		Via->DeleteNr = (int16) LastActionNr;
		DataBaseChanged = 1;
		/*
		    if ((!OkToRePaintAfterTraceDrawing)
		       &&
		       (!ObjectInAreaFillInserted)) {
		      StartDrawingEditingWindow();
		      SetBackGroundActive(0);
		      DrawObject(ConnectObject,0);
		      if (OkToDrawClearances) {
		        SetBackGroundActive(0);
		        InitDrawingObject(0,CLEARANCE_LAYER,0,DRAW_WITH_PEN_AND_NOT_FILLED);
		        DrawObject(ConnectObject,0x40+8);
		      }
		      ReCalcConnectionsNet(CurrentDrawingNetNr,0,0);
		      SetROP2(OutputDisplay,R2_COPYPEN);
		      GetObjectsNet(CurrentDrawingNetNr,MODE_OBJECTS3,0);
		      ReDisplayNet(0);
		      ExitDrawing();
		      EndDrawingEditingWindow();
		    } else {
		*/
		ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 1);
		ConnectionObjectNr = GetObjectNrFromEndPoint(-1, EndPreviousTraceX, EndPreviousTraceY, 0);

		if (ConnectionObjectNr != -1)
		{
// Trace from a different layer connected to the via
			ConnectObject = &((*Objects3)[ConnectionObjectNr]);
#ifdef _DEBUG

			if (!ObjectIsTrace(ConnectObject))
				ok = 1;

#endif
			CurrentDrawingLayer = ConnectObject->Layer;

			if ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10)
			{
				TracesInserted++;
				LastActionNr++;

				if (OkToRePaintAfterTraceDrawing)
					RePaint();

				return 0;
			}
		}

		OkToRePaint = 1;
	}
	else
	{
		if (ConnectObject->Layer != -1)
			CurrentDrawingLayer = ConnectObject->Layer;

		if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
			ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
		else
			ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 1);

		if ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10)
		{
			TracesInserted++;
			LastActionNr++;

			if ((OkToRePaintAfterTraceDrawing) || (ObjectInAreaFillInserted))
				RePaint();

			return 0;
		}
	}

	if ((ConnectionObjectNr == -1)
	        || ((!ObjectIsTrace(ConnectObject)) && (ConnectObject->ObjectType != TRACE_ALL_ANGLE)
	            && (ConnectObject->ObjectType != TRACE_ARC)))
	{
		TracesInserted++;
		LastActionNr++;

		if (OkToRePaintAfterTraceDrawing)
			RePaint();

		return 0;
	}

// Find the starting point of the connected trace

	memmove(&CurrentWorkingTrace, ConnectObject, sizeof(ObjectRecord));
	CurrentTraceWidth = CurrentWorkingTrace.y2;
	CurrentClearance = CurrentWorkingTrace.Clearance;
	CurrentWorkingTrace.TraceNr = ConnectionObjectNr;
#ifdef _DEBUG

	if (CurrentWorkingTrace.TraceNr == 0)
		ok = 1;

#endif
	x1 = 0.0;
	y1 = 0.0;
	x2 = 0.0;
	y2 = 0.0;
	Angle1 = 0.0;
	Angle2 = 0.0;

	if (ConnectObject->ObjectType == TRACE_ARC)
		GetArcEndPoints(ConnectObject, &x1, &y1, &x2, &y2, 0);

	CurrentWorkingTrace.Info2 = 3;
	TraceDrawingMode = 0;

	switch (ConnectObject->ObjectType)
	{
	case TRACE_HOR:
		x1 = ConnectObject->x1;
		y1 = ConnectObject->y1;
		x2 = ConnectObject->x1 + ConnectObject->x2;
		y2 = ConnectObject->y1;
		break;

	case TRACE_VER:
		x1 = ConnectObject->x1;
		y1 = ConnectObject->y1;
		x2 = ConnectObject->x1;
		y2 = ConnectObject->y1 + ConnectObject->x2;
		break;

	case TRACE_DIAG1:
		x1 = ConnectObject->x1;
		y1 = ConnectObject->y1;
		x2 = ConnectObject->x1 + ConnectObject->x2;
		y2 = ConnectObject->y1 - ConnectObject->x2;
		break;

	case TRACE_DIAG2:
		x1 = ConnectObject->x1;
		y1 = ConnectObject->y1;
		x2 = ConnectObject->x1 + ConnectObject->x2;
		y2 = ConnectObject->y1 + ConnectObject->x2;
		break;

	case TRACE_ALL_ANGLE:
		x1 = ConnectObject->x1;
		y1 = ConnectObject->y1;
		x2 = ConnectObject->x2;
		y2 = ConnectObject->y2;
		TraceDrawingMode = 1;
		CurrentTraceWidth = CurrentWorkingTrace.Thickness;
		break;

	case TRACE_ARC:
		TraceDrawingMode = 2;
		CurrentTraceWidth = CurrentWorkingTrace.Thickness;
		GetArcAngle(ConnectObject, &Angle1, &Angle2);
		Angle1 -= ANGLE_90;
		Angle2 += ANGLE_90;
		PreviousDirection = (40 - (int32) (Angle2 / 11.25 + 0.1)) % 32;
		PreviousDirection = (40 - (int32) (Angle1 / 11.25 + 0.1)) % 32;
		break;
	}

	if ((InRange(x1, EndPreviousTraceX)) && (InRange(y1, EndPreviousTraceY)))
	{
		StartPreviousTraceX = x1;
		StartPreviousTraceY = y1;
		EndPreviousTraceX = x2;
		EndPreviousTraceY = y2;
		PreviousDirection =
		    GetNewDirection(StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX, EndPreviousTraceY, 0);

		if (ConnectObject->ObjectType == TRACE_ARC)
			PreviousDirection = (40 - (int32) (Angle1 / 11.25 + 0.1)) % 32;
	}
	else
	{
		StartPreviousTraceX = x2;
		StartPreviousTraceY = y2;
		EndPreviousTraceX = x1;
		EndPreviousTraceY = y1;
		PreviousDirection =
		    GetNewDirection(StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX, EndPreviousTraceY, 0);

		if (ConnectObject->ObjectType == TRACE_ARC)
			PreviousDirection = (40 - (int32) (Angle2 / 11.25 + 0.1)) % 32;
	}

//  StartSecondDrawingTraceX=EndPreviousTraceX;
//  StartSecondDrawingTraceY=EndPreviousTraceY;
	CurrentDrawX1 = EndPreviousTraceX;
	CurrentDrawY1 = EndPreviousTraceY;

	if (CurrentWorkingTrace.TraceNr != -1)
	{
		if (InRange(DrawTrace1.x2, 0.0))
		{
		}
	}


// Remove trace when backwards drawing
	if ((!TraceDeleted) && (CurrentTraceNr != -1) && (InRange(DrawTrace1.x2, 0.0)))
	{
		ZeroUnusedObjects(0);

		if ((!OkToRePaintAfterTraceDrawing) && (!ObjectInAreaFillInserted))
		{
			DeleteAndUnDisplayObjectTrace(CurrentTraceNr, 0);
			GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
			ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 0);
		}
		else
		{
			DeleteAndUnDisplayObjectTrace(CurrentTraceNr, 1);
			GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
			ReCalcConnectionsNet(CurrentDrawingNetNr, 0, 1);
		}

		CurrentWorkingTrace.ObjectType = 0;
	}

	TracesInserted++;
	LastActionNr++;

	if ((OkToRePaint) || (OkToRePaintAfterTraceDrawing) || (ObjectInAreaFillInserted))
		RePaint();

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InsertTwoTracesOnGuide(int32 mode)
{
	int32 res;
	int32 res2, DoubleTraceRes, IncludeTrace1, IncludeTrace2;
	int32 OkToDrawTraces = 0;
	int32 ObjectInAreaFillInserted = 0;
	NetRecord *Net;


	res2 = 1;
	ObjectKeepOut = 0;
	ObjectKeepOut2 = 0;

	if ((DrawTrace1.ObjectType == 0) || (DrawTrace1.x2 == 0))
		return 1;

	if ((ConnectionObject2.Layer != -1) && (DrawTrace1.Layer != ConnectionObject2.Layer))
		return 1;

	if ((DoubleTraceRes = CalcDoubleTrace()) == 0)
		return 1;

	Net = &((*Nets)[CurrentDrawingNetNr]);
	FillPositionObject(&DrawTrace1);
	FillPositionObject(&DrawTrace2);

	IncludeTrace1 = 0;
	IncludeTrace2 = 0;

	if ((DrawTrace1.x2 > 0.0) && (!CheckObjectOverlapped(&DrawTrace1, 0)))
	{
		if (DrawTrace2.ObjectType != 0)
		{
			if ((DrawTrace2.x2 > 0.0) && (!CheckObjectOverlapped(&DrawTrace2, 0)))
			{
				IncludeTrace1 = 1;
				IncludeTrace2 = 1;
			}
			else
				IncludeTrace1 = 1;
		}
		else
			IncludeTrace1 = 1;
	}

	res2 = 1;

	if (IncludeTrace1)
	{
		if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
			DrawTrace1.Info = OBJECT_HIGHLITED;

		if (CheckObjectInsideAreaFillPowerPlane(&DrawTrace1) == 0)
		{
			ZeroUnusedObjects(0);
			/*
			      StartDrawingEditingWindow();
			      SetROP2(OutputDisplay,R2_XORPEN);
			      InitDrawingConnections();
			      Connection=&((*Connections)[CurrentGuideNr]);
			      DrawConnection(Connection);
			      ExitDrawing();
			      EndDrawingEditingWindow();
			      Connection->Info|=OBJECT_NOT_VISIBLE;
			      Connection->DeleteNr=(int16)LastActionNr;
			      DataBaseChanged=1;
			*/
			DrawTrace1.Info = 0;

			if ((res = ExtendNewTrace(&DrawTrace1)) != -1)
			{
				if (!OkToRePaintAfterTraceDrawing)
					DeleteAndUnDisplayObjectTrace(res, 0);
				else
					DeleteAndUnDisplayObjectTrace(res, 1);
			}

			if (!AddTrace(&DrawTrace1))
				return 0;

			TracesInserted++;

			if (RecalcAreafillAfterInsert)
			{
				if (!OkToRePaintAfterTraceDrawing)
					res = InsertObjectInAreaFill(&DrawTrace1, DrawTrace1.Layer, DrawTrace1.NetNr, 0);
				else
					res = InsertObjectInAreaFill(&DrawTrace1, DrawTrace1.Layer, DrawTrace1.NetNr, 2);

				if (res == 1)
					ObjectInAreaFillInserted = 1;
			}

			OkToDrawTraces = 1;
			memmove(&LastAddedObject, &DrawTrace1, sizeof(ObjectRecord));
			res2 = 0;
		}
		else
		{
			IncludeTrace2 = 0;
			strcpy(InfoStr, SC(1063, "Can not draw on powerplane"));
			RedrawInfoStr(1);
			Beep(1000, 200);
			res2 = 1;
		}
	}

	if (IncludeTrace2)
	{
		DrawTrace2.Info = 0;

		if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
			DrawTrace2.Info = OBJECT_HIGHLITED;

		if (CheckObjectInsideAreaFillPowerPlane(&DrawTrace1) == 0)
		{
			if ((res = ExtendNewTrace(&DrawTrace2)) != -1)
			{
				ZeroUnusedObjects(0);

				if (!OkToRePaintAfterTraceDrawing)
					DeleteAndUnDisplayObjectTrace(res, 0);
				else
					DeleteAndUnDisplayObjectTrace(res, 1);
			}

			if (!AddTrace(&DrawTrace2))
				return 0;

			TracesInserted++;

			if (RecalcAreafillAfterInsert)
			{
				if (!OkToRePaintAfterTraceDrawing)
					res = InsertObjectInAreaFill(&DrawTrace2, DrawTrace2.Layer, DrawTrace2.NetNr, 0);
				else
					res = InsertObjectInAreaFill(&DrawTrace2, DrawTrace2.Layer, DrawTrace2.NetNr, 2);

				if (res == 1)
					ObjectInAreaFillInserted = 1;
			}

			OkToDrawTraces = 1;
			res2 = 0;
		}
		else
		{
			strcpy(InfoStr, SC(1063, "Can not draw on powerplane"));
			RedrawInfoStr(1);
			Beep(1000, 200);
			res2 = 1;
		}
	}

	if ((IncludeTrace1) || (IncludeTrace2))
		LastActionNr++;

	if (OkToDrawTraces)
	{
		if ((!OkToRePaintAfterTraceDrawing) || (!ObjectInAreaFillInserted))
		{
			StartDrawingEditingWindow();
			SetROP2(OutputDisplay, R2_COPYPEN);
			GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
			ReDisplayNet(0);
			ExitDrawing();
			EndDrawingEditingWindow();
		}
		else
			RePaint();
	}

	return res2;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InsertVia()
{
	ObjectRecord ViaObject, *ConnectObject, *Object3, CheckObject;
	int32 cnt, ObjectCoveredNr, hulp, FoundNr, OldLayer, ok;
	ViaRecord NewVia, *NetVia;
	int32 res;
	NetRecord *Net;

	Net = &((*Nets)[CurrentDrawingNetNr]);

	ObjectKeepOut = 0;
	ObjectKeepOut2 = 0;
	res = 1;
	ViaObject.ObjectType = VIA_PUT_THROUGH_ROUND;
	ViaObject.x1 = CurrentDrawX1;
	ViaObject.y1 = CurrentDrawY1;
//  ViaObject.x1=StartSecondDrawingTraceX;
//  ViaObject.y1=StartSecondDrawingTraceY;
	NetVia = &CurrentVia;
	ViaObject.x2 = CurrentVia.ThickNess;
	ViaObject.y2 = CurrentVia.DrillThickNess;
	ViaObject.Clearance = CurrentVia.Clearance;

	if ((Net->ViaNr > 0) && (Net->ViaNr <= 10))
	{
		NetVia = &Design.DefVia1;
		NetVia += Net->ViaNr - 1;
		ViaObject.x2 = NetVia->ThickNess;
		ViaObject.y2 = NetVia->DrillThickNess;
		ViaObject.Clearance = NetVia->Clearance;
	}

	ViaObject.NetNr = CurrentDrawingNetNr;
	ViaObject.Info = 0;
	ViaObject.Layer = -1;
	FillPositionObject(&ViaObject);

	if (CheckObjectOverlapped(&ViaObject, 0))
		return -1;

//    DrawObjects2WithColor(Black,0);
//    DrawCrossObjects3(0,Black,0);
//  NrObjects2  NrObjects4

//    for (cnt=0;cnt<10000000;cnt++) ;

	FoundNr = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object3 = &((*Objects3)[cnt]);

		switch (Object3->ObjectType)
		{
		case PIN_PUT_THROUGH_ROUND:
		case VIA_PUT_THROUGH_ROUND:
		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
		case PIN_PUT_THROUGH_SQUARE:
			memmove(&CheckObject, &ViaObject, sizeof(ObjectRecord));
			CheckObject.x2 += max(CheckObject.Clearance, Object3->Clearance) * 2;

			if (ObjectsConnected(&CheckObject, Object3))
			{
				ok = 1;

				if (((InRange(Object3->x1, ViaObject.x1)) && (InRange(Object3->y1, ViaObject.y1))))
					FoundNr = cnt;
			}

			break;

		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			break;
		}
	}

	if (FoundNr != -1)
		return -2;

	/*
	  if (CurrentWorkingTrace.TraceNr!=-1) {
	    DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr);
	  }
	*/
	ObjectCoveredNr = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		ConnectObject = &((*Objects3)[cnt]);

		if (ConnectObject->ObjectType == VIA_PUT_THROUGH_ROUND)
		{
			hulp = 0;

			if ((InRange(ViaObject.x1, ConnectObject->x1)) && (InRange(ViaObject.y1, ConnectObject->y1))
			        && (ObjectCoveredNr == -1))
			{
				EndPreviousTraceX = ViaObject.x1;
				EndPreviousTraceY = ViaObject.y1;
				return 2;
			}
		}
	}

	res = 1;
	OldLayer = CurrentDrawingLayer;

	if (ObjectCoveredNr == -1)
	{
		ViaObject.Layer = -1;

		if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
			ViaObject.Info |= OBJECT_HIGHLITED;

		memmove(&NewVia, NetVia, sizeof(ViaRecord));
		NewVia.X = (float) ViaObject.x1;
		NewVia.Y = (float) ViaObject.y1;
		NewVia.NetNr = (int16) ViaObject.NetNr;
		NewVia.Info = 0;

		if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
			NewVia.Info = OBJECT_HIGHLITED;

		if (!AddVia(&NewVia))
			res = 0;

		TracesInserted++;
		memmove(&LastAddedObject, &ViaObject, sizeof(ObjectRecord));
		LastActionNr++;

		if (RecalcAreafillAfterInsert)
		{
			if (!OkToRePaintAfterTraceDrawing)
				InsertObjectInAreaFill(&ViaObject, -1, ViaObject.NetNr, 0);
			else
				InsertObjectInAreaFill(&ViaObject, -1, ViaObject.NetNr, 2);
		}

		EndPreviousTraceX = ViaObject.x1;
		EndPreviousTraceY = ViaObject.y1;

		GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
		/*
		    if (!OkToRePaintAfterTraceDrawing) {
		      StartDrawingEditingWindow();
		      ReDisplayNet(0);
		      ExitDrawing();
		      EndDrawingEditingWindow();
		    }
		*/
		DataBaseChanged = 1;
		CurrentWorkingTrace.Info2 = 0;
		CurrentWorkingTrace.ObjectType = VIA_PUT_THROUGH_ROUND;

//    memmove(&ConnectionObject1,&ViaObject,sizeof(ObjectRecord));
		if (LastUsedDrawingLayers[0] != -1)
		{
			if (LastUsedDrawingLayers[0] != CurrentDrawingLayer)
			{
				hulp = CurrentDrawingLayer;
				CurrentDrawingLayer = LastUsedDrawingLayers[0];
				CurrentDrawingLayer = GetDrawingLayer(0);	// Check CurrentDrawingLayer, change if necessary
				LastUsedDrawingLayers[0] = hulp;
			}
			else
				CurrentDrawingLayer = GetDrawingLayer(2);
		}
		else
		{
			LastUsedDrawingLayers[0] = CurrentDrawingLayer;
			CurrentDrawingLayer = GetDrawingLayer(2);	// Get new CurrentDrawingLayer
		}

//    if (OkToRePaintAfterTraceDrawing) {
		RePaint();
//    }
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SwapNetsTraces()
{
	int32 cnt, cnt2, Net1, Net2, Layer;
	TraceRecord *Trace = NULL;
	ViaRecord *Via, NewVia;
	ObjectRecord *ObjectNet1, *ObjectNet2, *TestObject, TraceObject;

	Net1 = -1;
	Net2 = -1;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (Net1 == -1)
					Net1 = Trace->NetNr;
				else if (Net2 == -1)
					Net2 = Trace->NetNr;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (Net1 == -1)
					Net1 = Trace->NetNr;
				else if (Net2 == -1)
					Net2 = Trace->NetNr;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (Net1 == -1)
					Net1 = Trace->NetNr;
				else if (Net2 == -1)
					Net2 = Trace->NetNr;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_SELECTED | OBJECT_NOT_VISIBLE)) == OBJECT_SELECTED)
			{
				if (Net1 == -1)
					Net1 = Trace->NetNr;
				else if (Net2 == -1)
					Net2 = Trace->NetNr;
			}
		}
	}

	if ((Net1 != -1) && (Net2 != -1) && (Net1 != Net2))
	{
		DataBaseChanged = 1;
		GetObjectsNet((int32) Net1, MODE_OBJECTS2, 0);
		GetObjectsNet((int32) Net2, MODE_OBJECTS3, 0);

		for (cnt = 0; cnt < NrObjects2; cnt++)
		{
			ObjectNet1 = &((*Objects2)[cnt]);
			ObjectNet1->Info = 0;
		}

		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			ObjectNet2 = &((*Objects3)[cnt]);
			ObjectNet2->Info = 0;
		}

		for (cnt = 0; cnt < NrObjects2; cnt++)
		{
			ObjectNet1 = &((*Objects2)[cnt]);

			switch (ObjectNet1->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_SMD_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_SMD_RECT:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
				for (cnt2 = 0; cnt2 < NrObjects2; cnt2++)
				{
					TestObject = &((*Objects2)[cnt2]);

					if ((cnt != cnt2) && (TestObject->Info == 0) && (ObjectIsTrace(TestObject)))
					{
						if (ObjectsConnected(TestObject, ObjectNet1))
							TestObject->Info = OBJECT_NOT_VISIBLE;
					}
				}
			}
		}

		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			ObjectNet2 = &((*Objects3)[cnt]);

			switch (ObjectNet2->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_SMD_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_SMD_RECT:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
				for (cnt2 = 0; cnt2 < NrObjects3; cnt2++)
				{
					TestObject = &((*Objects3)[cnt2]);

					if ((cnt != cnt2) && (TestObject->Info == 0) && (ObjectIsTrace(TestObject)))
					{
						if (ObjectsConnected(TestObject, ObjectNet2))
							TestObject->Info = OBJECT_NOT_VISIBLE;
					}
				}
			}
		}

		for (cnt = 0; cnt < NrObjects2; cnt++)
		{
			ObjectNet1 = &((*Objects2)[cnt]);

			if ((ObjectIsTrace(ObjectNet1)))
			{
				memmove(&TraceObject, ObjectNet1, sizeof(TraceObject));
				TraceObject.NetNr = (int16) Net2;

				switch (ObjectNet1->ObjectType)
				{
				case TRACE_VER:
					Trace = &((*VerTraces[ObjectNet1->Layer])[ObjectNet1->TraceNr]);
					break;

				case TRACE_HOR:
					Trace = &((*HorTraces[ObjectNet1->Layer])[ObjectNet1->TraceNr]);
					break;

				case TRACE_DIAG1:
					Trace = &((*Diag1Traces[ObjectNet1->Layer])[ObjectNet1->TraceNr]);
					break;

				case TRACE_DIAG2:
					Trace = &((*Diag2Traces[ObjectNet1->Layer])[ObjectNet1->TraceNr]);
					break;
				}

				if (AddTrace(&TraceObject))
				{
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16)LastActionNr;
				}
			}
			else
			{
				Via = &((*Vias)[ObjectNet1->TraceNr]);

				if (ObjectNet1->ObjectType == VIA_PUT_THROUGH_ROUND)
				{
					memmove(&NewVia, Via, sizeof(NewVia));
					NewVia.NetNr = (int16)Net2;
					AddVia(&NewVia);
					Via = &((*Vias)[ObjectNet1->TraceNr]);
					Via->Info |= OBJECT_NOT_VISIBLE;
					Via->DeleteNr = (int16) LastActionNr;
				}
			}
		}

		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			ObjectNet2 = &((*Objects3)[cnt]);

			if ((ObjectIsTrace(ObjectNet2)))
			{
				memmove(&TraceObject, ObjectNet2, sizeof(TraceObject));
				TraceObject.NetNr = (int16)Net1;

				switch (ObjectNet2->ObjectType)
				{
				case TRACE_VER:
					Trace = &((*VerTraces[ObjectNet2->Layer])[ObjectNet2->TraceNr]);
					break;

				case TRACE_HOR:
					Trace = &((*HorTraces[ObjectNet2->Layer])[ObjectNet2->TraceNr]);
					break;

				case TRACE_DIAG1:
					Trace = &((*Diag1Traces[ObjectNet2->Layer])[ObjectNet2->TraceNr]);
					break;

				case TRACE_DIAG2:
					Trace = &((*Diag2Traces[ObjectNet2->Layer])[ObjectNet2->TraceNr]);
					break;
				}

				if (AddTrace(&TraceObject))
				{
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16)LastActionNr;
				}
			}
			else
			{
				Via = &((*Vias)[ObjectNet2->TraceNr]);

				if (ObjectNet2->ObjectType == VIA_PUT_THROUGH_ROUND)
				{
					memmove(&NewVia, Via, sizeof(NewVia));
					NewVia.NetNr = (int16)Net1;
					AddVia(&NewVia);
					Via = &((*Vias)[ObjectNet2->TraceNr]);
					Via->Info |= OBJECT_NOT_VISIBLE;
					Via->DeleteNr = (int16) LastActionNr;
				}
			}
		}

		ReCalcConnectionsNet((int32) Net1, 0, 1);
		ReCalcConnectionsNet((int32) Net2, 0, 1);
		RePaint();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SwitchToNearestDrawingLayer(int32 mode)
{
	int32 ConnectionObjectNr, NewLayer, cnt, OldCurrentDrawingLayer, ok;
	ObjectRecord *ConnectObject;
	int32 Stop = 0;

	/*
	  if ((SelectionMode!=ROUTING_MODE)
	     &&
	     (SelectionMode!=MOVE_ONE_TRACE_MODE)
	     &&
	     (SelectionMode!=MOVING_TRACES_VIAS_MODE)) {
	    return 0;
	  }
	*/
	OldCurrentDrawingLayer = CurrentDrawingLayer;

	if (SystemBusyMode == 1)
	{
		ConnectionObjectNr = GetObjectNrFromEndPoint(-1, CurrentDrawX1, CurrentDrawY1, 1);
		OkToSwitchDrawingLayers = 0;

		if ((ConnectionObjectNr != -1) && (Design.NrBoardLayers > 1))
		{
			ConnectObject = &((*Objects3)[ConnectionObjectNr]);

			switch (ConnectObject->ObjectType)
			{
			case DRILL:
			case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
				OkToSwitchDrawingLayers = 1;
				break;
			}
		}
	}
	else
		OkToSwitchDrawingLayers = 1;

	if ((!OkToSwitchDrawingLayers) || (Design.NrBoardLayers == 0))
		return -1;

	if (CurrentDrawingLayer == -1)
	{
		CurrentDrawingLayer = GetDrawingLayer(0);

		if (CurrentDrawingLayer != -1)
			RePaint();

		return 0;
	}

	if (Design.NrBoardLayers == 2)
	{
		if (!ViewSingleLayer)
		{
			if (DrawLayerCode[CurrentDrawingLayer ^ 1] < 0x10)
				CurrentDrawingLayer ^= 1;
		}
		else
		{
			CurrentDrawingLayer ^= 1;

			if (CurrentDrawingLayer == 0)
			{
				OkToDrawBottomPads = 1;
				OkToDrawSilkScreenBottom = 1;
				DrawBottomComponents = 1;
				OkToDrawTopPads = 0;
				OkToDrawSilkScreenTop = 0;
				DrawTopComponents = 0;
				OkToDrawInnerPads = 1;
				OkToDrawRoutingKeepoutTop = 1;
				OkToDrawRoutingKeepoutBottom = 1;
				OkToDrawRoutingKeepoutInner = 1;
			}
			else
			{
				OkToDrawBottomPads = 0;
				OkToDrawSilkScreenBottom = 0;
				DrawBottomComponents = 0;
				OkToDrawTopPads = 1;
				OkToDrawSilkScreenTop = 1;
				DrawTopComponents = 1;
				OkToDrawInnerPads = 1;
				OkToDrawRoutingKeepoutTop = 1;
				OkToDrawRoutingKeepoutBottom = 1;
				OkToDrawRoutingKeepoutInner = 1;
			}
		}
	}
	else
	{
		if (!ViewSingleLayer)
		{
			cnt = 0;

			while ((cnt < Design.NrBoardLayers) && (!Stop))
			{
				NewLayer = CurrentDrawingLayer + cnt + 1;

				if (NewLayer >= Design.NrBoardLayers)
					NewLayer -= Design.NrBoardLayers;

				if ((DrawLayerCode[NewLayer] < 0x10) && (!IsLayerPowerPlane(NewLayer)))
				{
					CurrentDrawingLayer = NewLayer;
					Stop = 1;
				}

				cnt++;
			}
		}
		else
		{
			for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
				DrawLayerCode[cnt] |= 0x10;

			CurrentDrawingLayer += 1;

			if (CurrentDrawingLayer == Design.NrBoardLayers)
				CurrentDrawingLayer = 0;

			DrawLayerCode[CurrentDrawingLayer] &= ~0x10;

			if (DrawLayerCode[0] & 0x10)
			{
				OkToDrawBottomPads = 0;
				OkToDrawSilkScreenBottom = 0;
				DrawBottomComponents = 0;
				OkToDrawRoutingKeepoutBottom = 0;
			}
			else
			{
				OkToDrawBottomPads = 1;
//        OkToDrawSilkScreenBottom=1;
				DrawBottomComponents = 1;
				OkToDrawRoutingKeepoutBottom = 1;
			}

			if (DrawLayerCode[Design.NrBoardLayers - 1] & 0x10)
			{
				OkToDrawTopPads = 0;
				OkToDrawSilkScreenTop = 0;
				DrawTopComponents = 0;
				OkToDrawRoutingKeepoutTop = 0;
			}
			else
			{
				OkToDrawTopPads = 1;
//        OkToDrawSilkScreenTop=1;
				DrawTopComponents = 1;
				OkToDrawRoutingKeepoutTop = 1;
			}

			if (Design.NrBoardLayers > 2)
			{
				if ((DrawLayerCode[Design.NrBoardLayers - 1] & 0x10) && (DrawLayerCode[0] & 0x10))
				{
					OkToDrawInnerPads = 1;
					OkToDrawRoutingKeepoutInner = 1;
//          OkToDrawSilkScreenBottom=1;
					DrawBottomComponents = 1;
					DrawTopComponents = 1;
//          OkToDrawSilkScreenTop=1;
				}
				else
				{
					OkToDrawInnerPads = 0;
					OkToDrawRoutingKeepoutInner = 0;
				}
			}
		}
	}

	if ((OldCurrentDrawingLayer != -1) && (OldCurrentDrawingLayer != CurrentDrawingLayer))
	{
		if (SystemBusyMode == 1)
		{
			NewLayer = CurrentDrawingLayer;
			CurrentDrawingLayer = OldCurrentDrawingLayer;

			if (CurrentWorkingTrace.Info2 != 0)
			{
				if (!OkToRePaintAfterTraceDrawing)
					DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 0);
				else
					DeleteAndUnDisplayObjectTrace(CurrentWorkingTrace.TraceNr, 1);
			}

			CurrentDrawingLayer = NewLayer;
			ok = 1;
			CurrentWorkingTrace.TraceNr = -1;
			CurrentWorkingTrace.Info2 = 2;
			GetObjectsNet(CurrentDrawingNetNr, MODE_OBJECTS3, 0);
		}

		if (ViewSingleLayer)
		{
			for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
			{
				DrawLayerCode[cnt] |= 0x10;
				DrawLayerCode[CurrentDrawingLayer] &= ~0x10;
			}
		}

		RePaint();
		CheckInputMessages(0);
	}
	else
	{
		/*
		    if ((CurrentDrawingLayer!=-1)
		       &&
		       (DrawLayerCode[CurrentDrawingLayer]<0x10)
		       &&
		       (!IsLayerPowerPlane(CurrentDrawingLayer))) {
		      CurrentDrawingLayer=NewLayer;
		*/
	}

	SetInfoStr(0);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
