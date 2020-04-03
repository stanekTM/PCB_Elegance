/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc2.c
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
#include "calc2.h"
#include "draw.h"
#include "math.h"
#include "pcb.h"
#include "calc.h"
#include "calc4.h"
#include "calcrect.h"
#include "calcdef.h"
#include "string.h"
#include "polygon.h"


typedef struct
{
	double x11, y11, x12, y12, x21, x22, y21, y22, x11a, y11a, x21a, y21a, lengte1, lengte1a, lengte2, lengte2a;
	int32 ObjectType1, ObjectType2;
} CalculateParamsRecord;

CalculateParamsRecord CalculateParams;

int32 ok;

// *******************************************************************************************************

int32 CircleCheck(CalculateParamsRecord * CalcP);
int32 RectangleCheck(CalculateParamsRecord * CalcP);
int32 SquareCheck(CalculateParamsRecord * CalcP);
int32 TraceHorCheck(CalculateParamsRecord * CalcP);
int32 TraceVerCheck(CalculateParamsRecord * CalcP);
int32 TraceDiag1Check(CalculateParamsRecord * CalcP);
int32 TraceDiag2Check(CalculateParamsRecord * CalcP);
int32 ConnectionCheck(CalculateParamsRecord * CalcP);

double CalculateDistanceToRoundSquareRectangle(CalculateParamsRecord * CalcP);
double CalculateDistanceToTraceHor(CalculateParamsRecord * CalcP);
double CalculateDistanceToTraceVer(CalculateParamsRecord * CalcP);
double CalculateDistanceToTraceDiag1(CalculateParamsRecord * CalcP);
double CalculateDistanceToTraceDiag2(CalculateParamsRecord * CalcP);
double CalculateDistanceToPinLineHor(CalculateParamsRecord * CalcP);
double CalculateDistanceToPinLineVer(CalculateParamsRecord * CalcP);
double CalculateDistanceToPinLineDiag1(CalculateParamsRecord * CalcP);
double CalculateDistanceToPinLineDiag2(CalculateParamsRecord * CalcP);
double CalculateDistanceToLinesArcs(CalculateParamsRecord * CalcP);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 ObjectsConnected(ObjectRecord * Object1a, ObjectRecord * Object2a)
{
	int32 ObjectType1, ObjectType2, ok, AreaFillObject1, AreaFillObject2;
	ObjectRecord Object1, Object2;
	int32 Object1OnInnerLayer = 0;
	int32 Object2OnInnerLayer = 0;
	double x11, y11, x12, y12, x21, x22, y21, y22, x11a, y11a, x21a, y21a, px, py, lengte1, lengte1a, lengte2, lengte2a;
	AreaFillRecord *AreaFill, *AreaFill2;
	PolygonRecord *PolygonObject1, *PolygonObject2;
	uint8 PolygonBuf1[10240], PolygonBuf2[10240];


	PolygonObject1 = (PolygonRecord *) & PolygonBuf1;
	PolygonObject2 = (PolygonRecord *) & PolygonBuf2;

	if ((Object1a->Layer != -1) && (Object2a->Layer != -1) && (Object2a->Layer != Object1a->Layer))
		return 0;


	memmove(&Object1, Object1a, sizeof(ObjectRecord));
	memmove(&Object2, Object2a, sizeof(ObjectRecord));
	Object1.ObjectType &= 0xfffe;
	Object2.ObjectType &= 0xfffe;


	x11 = Object1.x1;
	y11 = Object1.y1;
	x12 = Object1.x2;
	y12 = Object1.y2;
	x11a = x11;
	y11a = y11;
	lengte1 = x12;
	lengte1a = x12 * 0.5;

	x21 = Object2.x1;
	y21 = Object2.y1;
	x22 = Object2.x2;
	y22 = Object2.y2;
	x21a = x21;
	y21a = y21;
	lengte2 = x22;
	lengte2a = x22 * 0.5;

	ObjectType1 = Object1.ObjectType;
	ObjectType2 = Object2.ObjectType;

	switch (ObjectType1)
	{
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
	case PIN_BIG_POLYGON:
		ObjectType1 = OBJECT_POLYGON;
		break;

	case PIN_ARC:
	case OBJECT_ARC:
		ObjectType1 = TRACE_ARC;
		break;

	case PIN_LINE_ALL_ANGLE:
	case OBJECT_LINE:
		ObjectType1 = TRACE_ALL_ANGLE;
		break;
	}

	switch (ObjectType2)
	{
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
	case PIN_BIG_POLYGON:
		ObjectType2 = OBJECT_POLYGON;
		break;

	case PIN_ARC:
	case OBJECT_ARC:
		ObjectType2 = TRACE_ARC;
		break;

	case PIN_LINE_ALL_ANGLE:
	case OBJECT_LINE:
		ObjectType2 = TRACE_ALL_ANGLE;
		break;
	}


	if ((Object1.Layer > 0) && (Object1.Layer < Design.NrBoardLayers - 1))
		Object1OnInnerLayer = 1;

	if ((Object2.Layer > 0) && (Object2.Layer < Design.NrBoardLayers - 1))
		Object2OnInnerLayer = 1;

	if (Object1OnInnerLayer)
	{
		if (!Object2OnInnerLayer)
		{
			switch (Object2.ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				Object1.ObjectType = PIN_SMD_ROUND;

				if (Object2.x3 != 0.0)
					Object2.x2 = Object2.x3;

				Object2.Layer = Object1.Layer;
				break;
			}
		}
	}
	else
	{
		if (Object2OnInnerLayer)
		{
			switch (Object1.ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				Object1.ObjectType = PIN_SMD_ROUND;

				if (Object1.x3 != 0.0)
					Object1.x2 = Object1.x3;

				Object1.Layer = Object2.Layer;
				break;
			}
		}
	}

	/*
	  if ((ObjectType1==DRILL)
	     ||
	     (ObjectType1==DRILL_UNPLATED)) {
	    Object1.x2+=Design.StandardClearance*2.0;
	  }
	  if ((ObjectType2==DRILL)
	     ||
	     (ObjectType2==DRILL_UNPLATED)) {
	    Object2.x2+=Design.StandardClearance*2.0;
	  }
	*/
#ifdef _DEBUG

	if ((ObjectType2 == AREAFILL) && (ObjectType1 == PIN_SMD_ROUND))
	{
		if ((InRange9(Object1.x1, 40.6e5)) && (InRange9(Object1.y1, 31.3e5)))
			ok = 1;

		if (Object1.x1 > 30.6e5)
			ok = 1;

		ok = 1;
	}

	if ((InRange9(Object1.x1, 41.5e5)) && (InRange9(Object1.y1, 67.8e5)))
	{
		ok = 1;

		if (ObjectType2 == AREAFILL)
			ok = 1;
	}

	if ((InRange9(Object2.x1, 41.5e5)) && (InRange9(Object2.y1, 67.8e5)))
	{
		ok = 1;

		if (ObjectType1 == AREAFILL)
			ok = 1;
	}

#endif

	AreaFillObject1 = 0;

	if (CheckObjectIsBigPolygon(&Object1))
		AreaFillObject1 = 1;

	AreaFillObject2 = 0;

	if (CheckObjectIsBigPolygon(&Object2))
		AreaFillObject2 = 1;


	/***************************************************************************************/
	/***************************************************************************************/

	if ((AreaFillObject1) || (AreaFillObject2))
	{
		if ((ObjectType1 == CONNECTION) || (ObjectType2 == CONNECTION))
		{
			if (ObjectType2 == CONNECTION)
			{
				if (GetFirstPointPolygonObject(&Object1, &px, &py, 0))
				{
					if ((InRange(px, x21)) && (InRange(py, y21)))
						return 1;

					if ((InRange(px, x22)) && (InRange(py, y22)))
						return 1;
				}
				else
				{
					if ((InRange(x11, x21)) && (InRange(y11, y21)))
						return 1;

					if ((InRange(x11, x22)) && (InRange(y11, y22)))
						return 1;
				}

				return 0;
			}
			else
			{
				if (GetFirstPointPolygonObject(&Object2, &px, &py, 0))
				{
					if ((InRange(x11, px)) && (InRange(y11, py)))
						return 1;

					if ((InRange(x12, px)) && (InRange(y12, py)))
						return 1;
				}
				else
				{
					if ((InRange(x11, x21)) && (InRange(y11, y21)))
						return 1;

					if ((InRange(x12, x21)) && (InRange(y12, y21)))
						return 1;
				}

				return 0;
			}
		}

		if (AreaFillObject1)
		{
			if ((Object1.Info & OBJECT_WITH_CLEARANCE) == 0)
				GetAreaFillFromBigPolygonObject(&Object1, &AreaFill, 0.0, 0);
			else
			{
				GetAreaFillFromBigPolygonObject(&Object1, &AreaFill, 0.0, 0);
//        GetAreaFillFromBigPolygonObject(&Object1,&AreaFill,Object1.Clearance-(float)10.0,0);
			}

			if (AreaFillObject2)
			{
				if ((Object2.Info & OBJECT_WITH_CLEARANCE) == 0)
					GetAreaFillFromBigPolygonObject(&Object2, &AreaFill2, 0.0, 0);
				else
				{
					GetAreaFillFromBigPolygonObject(&Object2, &AreaFill2, 0.0, 0);
//          GetAreaFillFromBigPolygonObject(&Object2,&AreaFill2,Object1.Clearance-(float)10.0,0);
				}

				return CheckAreaFillsConnected(AreaFill2, AreaFill);
			}

			if ((Object2.Info & OBJECT_WITH_CLEARANCE) == 0)
				MakePolygonFromObject(&Object2, PolygonObject2, 0.0, 0.0, 1, 0);
			else
			{
				MakePolygonFromObject(&Object2, PolygonObject2, 0.0, 0.0, 1, 0);
//        MakePolygonFromObject(&Object2,PolygonObject2,Object2.Clearance-(float)10.0,0.0,1,0);
			}

			if (CheckPolygonOverlapAreaFill(PolygonObject2, AreaFill) == 1)
				return 1;
			else
				return 0;
		}
		else
		{
			if ((Object2.Info & OBJECT_WITH_CLEARANCE) == 0)
				GetAreaFillFromBigPolygonObject(&Object2, &AreaFill2, 0.0, 0);
			else
				GetAreaFillFromBigPolygonObject(&Object2, &AreaFill2, Object2.Clearance - (float) 10.0, 0);

			if ((Object2.Info & OBJECT_WITH_CLEARANCE) == 0)
				MakePolygonFromObject(&Object1, PolygonObject1, 0.0, 0.0, 1, 0);
			else
			{
				MakePolygonFromObject(&Object1, PolygonObject1, 0.0, 0.0, 1, 0);
//        MakePolygonFromObject(&Object1,PolygonObject1,Object1.Clearance-(float)10.0,0.0,1,0);
			}

			if (CheckPolygonOverlapAreaFill(PolygonObject1, AreaFill2) == 1)
				return 1;
			else
				return 0;
		}
	}

	/***************************************************************************************/
	/***************************************************************************************/

	if ((ObjectType1 == AREAFILL) || (ObjectType2 == AREAFILL))
	{
		if ((ObjectType1 == CONNECTION) || (ObjectType2 == CONNECTION))
		{
			if (ObjectType2 == CONNECTION)
			{
				if ((InRange(x11, x21)) && (InRange(y11, y21)))
					return 1;

				if ((InRange(x11, x22)) && (InRange(y11, y22)))
					return 1;

				return 0;
			}
			else
			{
				if ((InRange(x11, x21)) && (InRange(y11, y21)))
					return 1;

				if ((InRange(x12, x21)) && (InRange(y12, y21)))
					return 1;

				return 0;
			}
		}

		if (ObjectType1 == AREAFILL)
		{
			if (Object1a->ObjectType2 == 0)
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object1.TraceNr]]);
			else
				AreaFill = (AreaFillRecord *) Object1.TraceNr;

			if (ObjectType2 == AREAFILL)
			{
				ok = 1;

				if (Object2a->ObjectType2 == 0)
					AreaFill2 = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object2.TraceNr]]);
				else
					AreaFill2 = (AreaFillRecord *) Object2.TraceNr;

//        return 0;
				return CheckAreaFillsConnected(AreaFill2, AreaFill);
			}

			MakePolygonFromObject(&Object2, PolygonObject2, 0.0, 0.0, 1, 0);
//      MakePolygonFromObject(&Object2,PolygonObject2,AreaFill->Clearance-(float)10.0,0.0,1,0);
		}
		else
		{
			if (Object2a->ObjectType2 == 0)
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object2.TraceNr]]);
			else
				AreaFill = (AreaFillRecord *) Object1.TraceNr;

			MakePolygonFromObject(&Object1, PolygonObject2, 0.0, 0.0, 1, 0);
//      MakePolygonFromObject(&Object1,PolygonObject2,AreaFill->Clearance-(float)10.0,0.0,1,0);
		}

		if (CheckPolygonOverlapAreaFill(PolygonObject2, AreaFill) == 1)
			return 1;
		else
			return 0;
	}

	/***************************************************************************************/
	/***************************************************************************************/

	if ((ObjectType1 == TRACE_ALL_ANGLE) || (ObjectType2 == TRACE_ALL_ANGLE) || (ObjectType1 == TRACE_ARC)
	        || (ObjectType2 == TRACE_ARC))
	{
		if (ObjectType1 == TRACE_ARC)
			GetArcEndPoints(&Object1, &x11, &y11, &x12, &y12, 0);

		if (ObjectType2 == TRACE_ARC)
			GetArcEndPoints(&Object2, &x21, &y21, &x22, &y22, 0);

		if (ObjectType1 == CONNECTION)
		{
			if ((InRange(x11, x21)) && (InRange(y11, y21)))
				return 1;

			if ((InRange(x11, x22)) && (InRange(y11, y22)))
				return 1;

			if ((InRange(x12, x21)) && (InRange(y12, y21)))
				return 1;

			if ((InRange(x12, x22)) && (InRange(y12, y22)))
				return 1;

			return 0;
		}

		if (ObjectType2 == CONNECTION)
		{
			if ((InRange(x11, x21)) && (InRange(y11, y21)))
				return 1;

			if ((InRange(x11, x22)) && (InRange(y11, y22)))
				return 1;

			if ((InRange(x12, x21)) && (InRange(y12, y21)))
				return 1;

			if ((InRange(x12, x22)) && (InRange(y12, y22)))
				return 1;

			return 0;
		}

		MakePolygonFromObject(&Object1, PolygonObject1, 0.0, 0.0, 1, 1);
		MakePolygonFromObject(&Object2, PolygonObject2, 0.0, 0.0, 1, 1);

		if ((PolygonObject1->maxx > PolygonObject2->minx) && (PolygonObject1->minx < PolygonObject2->maxx)
		        && (PolygonObject1->maxy > PolygonObject2->miny) && (PolygonObject1->miny < PolygonObject2->maxy))
		{
			if (CheckPolygonCompleetlyOutsidePolygon(PolygonObject1, PolygonObject2) != 1)
				return 1;
		}

		return 0;
	}

	/***************************************************************************************/
	/***************************************************************************************/

	if ((ObjectType1 == OBJECT_POLYGON) || (ObjectType2 == OBJECT_POLYGON))
	{
		if ((ObjectType1 == CONNECTION) || (ObjectType2 == CONNECTION))
		{
			if (ObjectType2 == CONNECTION)
			{
				if ((InRange(x11, x21)) && (InRange(y11, y21)))
					return 1;

				if ((InRange(x11, x22)) && (InRange(y11, y22)))
					return 1;

				return 0;
			}
			else
			{
				if ((InRange(x11, x21)) && (InRange(y11, y21)))
					return 1;

				if ((InRange(x12, x21)) && (InRange(y12, y21)))
					return 1;

				return 0;
			}
		}

		if (ObjectType1 != OBJECT_POLYGON)
			MakePolygonFromObject(&Object1, PolygonObject1, 0.0, 0.0, 1, 1);
		else
		{
			if ((Object1.Info & OBJECT_WITH_CLEARANCE) == 0)
				MakePolygonFromObject(&Object1, PolygonObject1, 0.0, 0.0, 1, 1);
			else
			{
				MakePolygonFromObject(&Object1, PolygonObject1, 0.0, 0.0, 1, 1);
//        MakePolygonFromObject(&Object1,PolygonObject1,Object1.Clearance,0.0,1,1);
			}
		}

		if (ObjectType2 != OBJECT_POLYGON)
			MakePolygonFromObject(&Object2, PolygonObject2, 0.0, 0.0, 1, 1);
		else
		{
			if ((Object2.Info & OBJECT_WITH_CLEARANCE) == 0)
				MakePolygonFromObject(&Object2, PolygonObject2, 0.0, 0.0, 1, 1);
			else
			{
				MakePolygonFromObject(&Object2, PolygonObject2, 0.0, 0.0, 1, 1);
//        MakePolygonFromObject(&Object2,PolygonObject2,Object2.Clearance,0.0,1,1);
			}
		}

		if ((PolygonObject1->maxx > PolygonObject2->minx) && (PolygonObject1->minx < PolygonObject2->maxx)
		        && (PolygonObject1->maxy > PolygonObject2->miny) && (PolygonObject1->miny < PolygonObject2->maxy))
		{
			if (CheckPolygonCompleetlyOutsidePolygon(PolygonObject1, PolygonObject2) != 1)
				return 1;
		}

		return 0;
	}

	CalculateParams.x11 = x11;
	CalculateParams.x12 = x12;
	CalculateParams.x21 = x21;
	CalculateParams.x22 = x22;
	CalculateParams.y11 = y11;
	CalculateParams.y12 = y12;
	CalculateParams.y21 = y21;
	CalculateParams.y22 = y22;
	CalculateParams.x11a = x11a;
	CalculateParams.x21a = x21a;
	CalculateParams.y11a = y11a;
	CalculateParams.y21a = y21a;
	CalculateParams.lengte1 = lengte1;
	CalculateParams.lengte1a = lengte1a;
	CalculateParams.lengte2 = lengte2;
	CalculateParams.lengte2a = lengte2a;
	CalculateParams.ObjectType1 = ObjectType1;
	CalculateParams.ObjectType2 = ObjectType2;

	/***************************************************************************************/
	/***************************************************************************************/

	switch (ObjectType1)
	{
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:
		return CircleCheck(&CalculateParams);

	/***************************************************************************************/
	case PIN_SMD_RECT:
		return RectangleCheck(&CalculateParams);

	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:
		return SquareCheck(&CalculateParams);

	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:
		return TraceHorCheck(&CalculateParams);

	/***************************************************************************************/
	case TRACE_VER:
	case PIN_LINE_VER:
		return TraceVerCheck(&CalculateParams);

	/***************************************************************************************/
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:
		return TraceDiag1Check(&CalculateParams);

	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:
		return TraceDiag2Check(&CalculateParams);

	/***************************************************************************************/
	case CONNECTION:
		return ConnectionCheck(&CalculateParams);
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double ObjectsDistance(ObjectRecord * Object1, ObjectRecord * Object2)
{
	int32 ObjectType1, ObjectType2;
	double x11, y11, x12, y12, x21, x22, y21, y22, lengte1, lengte2, lengte1a, lengte2a;

	x11 = Object1->x1;
	y11 = Object1->y1;
	x12 = Object1->x2;
	y12 = Object1->y2;
	lengte1 = x12;
	lengte1a = x12 * 0.5;

	x21 = Object2->x1;
	y21 = Object2->y1;
	x22 = Object2->x2;
	y22 = Object2->y2;
	lengte2 = x22;
	lengte2a = x22 * 0.5;

	ObjectType1 = Object1->ObjectType;
	ObjectType2 = Object2->ObjectType;

	if ((ObjectType1 == PIN_ARC) || (ObjectType1 == TRACE_ARC))
		GetArcEndPoints(Object1, &x11, &y11, &x12, &y12, 0);

	if ((ObjectType2 == PIN_ARC) || (ObjectType2 == TRACE_ARC))
		GetArcEndPoints(Object2, &x21, &y21, &x22, &y22, 0);


	CalculateParams.x11 = x11;
	CalculateParams.x12 = x12;
	CalculateParams.x21 = x21;
	CalculateParams.x22 = x22;
	CalculateParams.y11 = y11;
	CalculateParams.y12 = y12;
	CalculateParams.y21 = y21;
	CalculateParams.y22 = y22;
	CalculateParams.lengte1 = lengte1;
	CalculateParams.lengte1a = lengte1a;
	CalculateParams.lengte2 = lengte2;
	CalculateParams.lengte2a = lengte2a;
	CalculateParams.ObjectType1 = ObjectType1;
	CalculateParams.ObjectType2 = ObjectType2;

	/***************************************************************************************/
	/***************************************************************************************/

	switch (ObjectType1)
	{
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_SMD_RECT:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		return CalculateDistanceToRoundSquareRectangle(&CalculateParams);

	/***************************************************************************************/
	case TRACE_HOR:
		return CalculateDistanceToTraceHor(&CalculateParams);

	/***************************************************************************************/
	case PIN_LINE_HOR:
		return CalculateDistanceToPinLineHor(&CalculateParams);

	/***************************************************************************************/
	case TRACE_VER:
		return CalculateDistanceToTraceVer(&CalculateParams);

	/***************************************************************************************/
	case PIN_LINE_VER:
		return CalculateDistanceToPinLineVer(&CalculateParams);

	/***************************************************************************************/
	case TRACE_DIAG1:
		return CalculateDistanceToTraceDiag1(&CalculateParams);

	/***************************************************************************************/
	case PIN_LINE_DIAG1:
		return CalculateDistanceToPinLineDiag1(&CalculateParams);

	/***************************************************************************************/
	case TRACE_DIAG2:
		return CalculateDistanceToTraceDiag2(&CalculateParams);

	/***************************************************************************************/
	case PIN_LINE_DIAG2:
		return CalculateDistanceToPinLineDiag2(&CalculateParams);

	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		return CalculateDistanceToLinesArcs(&CalculateParams);
		/***************************************************************************************/
	}

	SevereProgramError(__FILE__, ObjectType1 * 10000 + __LINE__);
	return 0.0;
}

// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 CircleCheck(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		return CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->x22);
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_SMD_RECT:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		return RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11,
		                             CalcP->x12);
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		return RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11,
		                             CalcP->x12);
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:

// hor line left circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// hor line right circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22))
			return 1;

// hor line
		if (RectTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
	case PIN_LINE_VER:

// ver line bottom circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// ver line top circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// ver line
		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->lengte2, CalcP->x11, CalcP->y11,
		         CalcP->x12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:

// diag1 line left top circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag1 line right bottom circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

// diag1 line
		if (CircleTestDiag1Objects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:

// diag2 line left bottom circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag2 line right top circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// diag2 line
		if (CircleTestDiag2Objects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 RectangleCheck(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:
		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if ((CalcP->x11 - CalcP->x12 * 0.5 > CalcP->x21 + CalcP->x22 * 0.5)
		        || (CalcP->x11 + CalcP->x12 * 0.5 < CalcP->x21 - CalcP->x22 * 0.5)
		        || (CalcP->y11 - CalcP->y12 * 0.5 > CalcP->y21 + CalcP->x22 * 0.5)
		        || (CalcP->y11 + CalcP->y12 * 0.5 < CalcP->y21 - CalcP->x22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_SMD_RECT:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if ((CalcP->x11 - CalcP->x12 * 0.5 > CalcP->x21 + CalcP->x22 * 0.5)
		        || (CalcP->x11 + CalcP->x12 * 0.5 < CalcP->x21 - CalcP->x22 * 0.5)
		        || (CalcP->y11 - CalcP->y12 * 0.5 > CalcP->y21 + CalcP->y22 * 0.5)
		        || (CalcP->y11 + CalcP->y12 * 0.5 < CalcP->y21 - CalcP->y22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:

// hor line left circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// hor line right circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22))
			return 1;

// hor line
		if ((CalcP->x11 - (CalcP->x12 * 0.5) > CalcP->x21 + CalcP->lengte2)
		        || (CalcP->x11 + (CalcP->x12 * 0.5) < CalcP->x21)
		        || (CalcP->y11 - (CalcP->y12 * 0.5) > CalcP->y21 + (CalcP->y22 * 0.5))
		        || (CalcP->y11 + (CalcP->y12 * 0.5) < CalcP->y21 - (CalcP->y22 * 0.5)))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
	case PIN_LINE_VER:

// ver line bottom circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// ver line top circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// ver line
		if ((CalcP->x11 - CalcP->x12 * 0.5 > CalcP->x21 + CalcP->y22 * 0.5)
		        || (CalcP->x11 + CalcP->x12 * 0.5 < CalcP->x21 - CalcP->y22 * 0.5)
		        || (CalcP->y11 - CalcP->y12 * 0.5 > CalcP->y21 + CalcP->lengte2)
		        || (CalcP->y11 + CalcP->y12 * 0.5 < CalcP->y21))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:

// diag1 line left top circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag1 line right bottom circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2,
		         CalcP->y22))
			return 1;

// diag1 line
		if (RectTestDiag1Objects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:

// diag2 line left bottom circle
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag2 line right top circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2,
		         CalcP->y22))
			return 1;

// diag2 line
		if (RectTestDiag2Objects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 SquareCheck(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if ((CalcP->x11 - CalcP->x12 * 0.5 > CalcP->x21 + CalcP->x22 * 0.5)
		        || (CalcP->x11 + CalcP->x12 * 0.5 < CalcP->x21 - CalcP->x22 * 0.5)
		        || (CalcP->y11 - CalcP->x12 * 0.5 > CalcP->y21 + CalcP->x22 * 0.5)
		        || (CalcP->y11 + CalcP->x12 * 0.5 < CalcP->y21 - CalcP->x22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_SMD_RECT:
		if ((InRange3(CalcP->x11, CalcP->x21)) && (InRange3(CalcP->y11, CalcP->y21)))
			return 1;

		if ((CalcP->x11 - CalcP->x12 * 0.5 > CalcP->x21 + CalcP->x22 * 0.5)
		        || (CalcP->x11 + CalcP->x12 * 0.5 < CalcP->x21 - CalcP->x22 * 0.5)
		        || (CalcP->y11 - CalcP->x12 * 0.5 > CalcP->y21 + CalcP->y22 * 0.5)
		        || (CalcP->y11 + CalcP->x12 * 0.5 < CalcP->y21 - CalcP->y22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:

// hor line left circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// hor line right circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22))
			return 1;

// hor line
		if ((CalcP->x11 - CalcP->x12 * 0.5 > CalcP->x21 + CalcP->lengte2)
		        || (CalcP->x11 + CalcP->x12 * 0.5 < CalcP->x21)
		        || (CalcP->y11 - CalcP->x12 * 0.5 > CalcP->y21 + CalcP->y22 * 0.5)
		        || (CalcP->y11 + CalcP->x12 * 0.5 < CalcP->y21 - CalcP->y22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
	case PIN_LINE_VER:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// ver line
		if ((CalcP->x11 - CalcP->x12 * 0.5 > CalcP->x21 + CalcP->y22 * 0.5)
		        || (CalcP->x11 + CalcP->x12 * 0.5 < CalcP->x21 - CalcP->y22 * 0.5)
		        || (CalcP->y11 - CalcP->x12 * 0.5 > CalcP->y21 + CalcP->lengte2)
		        || (CalcP->y11 + CalcP->x12 * 0.5 < CalcP->y21))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag1 line right bottom circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2,
		         CalcP->y22))
			return 1;

// diag1 line
		if (RectTestDiag1Objects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag2 line right top circle
		if ((InRange3(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange3(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2,
		         CalcP->y22))
			return 1;

// diag2 line
		if (RectTestDiag2Objects
		        (CalcP->x11, CalcP->y11, CalcP->x12, CalcP->x12, CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 TraceHorCheck(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:

// hor line left circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// hor line right circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// hor line
		if (RectTestCircleObjects
		        (CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_SMD_RECT:

// hor line left circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// hor line right circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12))
			return 1;

// hor line
		if ((CalcP->x11 > CalcP->x21 + (CalcP->x22 * 0.5))
		        || (CalcP->x11 + CalcP->lengte1 < CalcP->x21 - (CalcP->x22 * 0.5))
		        || (CalcP->y11 - (CalcP->y12 * 0.5) > CalcP->y21 + (CalcP->y22 * 0.5))
		        || (CalcP->y11 + (CalcP->y12 * 0.5) < CalcP->y21 - (CalcP->y22 * 0.5)))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:

// hor line left circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// hor line right circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12))
			return 1;

// hor line
		if ((CalcP->x11 > CalcP->x21 + (CalcP->x22 * 0.5))
		        || (CalcP->x11 + CalcP->lengte1 < CalcP->x21 - CalcP->x22 * 0.5)
		        || (CalcP->y11 - (CalcP->y12 * 0.5) > CalcP->y21 + (CalcP->x22 * 0.5))
		        || (CalcP->y11 + (CalcP->y12 * 0.5) < CalcP->y21 - (CalcP->x22 * 0.5)))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1a, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1a, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:

// hor line left circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// hor line right circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line
		                          CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22))
			return 1;

// hor line
		if ((CalcP->x11 > CalcP->x21 + CalcP->lengte2) || (CalcP->x11 + CalcP->lengte1 < CalcP->x21)
		        || (CalcP->y11 - CalcP->y12 * 0.5 > CalcP->y21 + CalcP->y22 * 0.5)
		        || (CalcP->y11 + CalcP->y12 * 0.5 < CalcP->y21 - CalcP->y22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_VER:
	case PIN_LINE_VER:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                          CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// ver line
		if (RectTestCircleObjects(CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22,	// hor line left circle
		                          CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22,	// hor line right circle
		                          CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12))
			return 1;

		if ((CalcP->x11 > CalcP->x21 + CalcP->y22 * 0.5)
		        || (CalcP->x11 + CalcP->lengte1 < CalcP->x21 - CalcP->y22 * 0.5)
		        || (CalcP->y11 - CalcP->y12 * 0.5 > CalcP->y21 + CalcP->lengte2)
		        || (CalcP->y11 + CalcP->y12 * 0.5 < CalcP->y21))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag1 line right bottom circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                          CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

// diag1 line
		if (CircleTestDiag1Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (RectTestDiag1Objects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                         CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag2 line right top circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                          CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// diag2 line
		if (CircleTestDiag2Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// hor line left circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->y12,	// hor line right circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (RectTestDiag2Objects(CalcP->x11 + CalcP->lengte1 * 0.5, CalcP->y11, CalcP->x12, CalcP->y12,	// hor line rect
		                         CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 TraceVerCheck(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// ver line
		if (RectTestCircleObjects
		        (CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->lengte1, CalcP->x21, CalcP->y21,
		         CalcP->x22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_SMD_RECT:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;

// ver line
		if ((CalcP->x11 - CalcP->y12 * 0.5 > CalcP->x21 + CalcP->x22 * 0.5)
		        || (CalcP->x11 + CalcP->y12 * 0.5 < CalcP->x21 - CalcP->x22 * 0.5)
		        || (CalcP->y11 > CalcP->y21 + CalcP->y22 * 0.5)
		        || (CalcP->y11 + CalcP->lengte1 < CalcP->y21 - CalcP->y22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;

// ver line
		if ((CalcP->x11 - CalcP->y12 * 0.5 > CalcP->x21 + CalcP->x22 * 0.5)
		        || (CalcP->x11 + CalcP->y12 * 0.5 < CalcP->x21 - CalcP->x22 * 0.5)
		        || (CalcP->y11 > CalcP->y21 + CalcP->x22 * 0.5)
		        || (CalcP->y11 + CalcP->lengte1 < CalcP->y21 - CalcP->x22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1a, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11 + CalcP->lengte1a, CalcP->y22)))
			return 1;

		/*
		      if (CalcP->ObjectType1==PIN_LINE_VER) {
		        ok=1;
		      }
		*/
		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->y22,	// hor line left circle
		                            CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22,	// hor line right circle
		                            CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

		if (RectTestCircleObjects(CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22,	// hor line
		                          CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->y22,	// hor line left circle
		                            CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22,	// hor line right circle
		                            CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;

		if (RectTestCircleObjects(CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22,	// hor line
		                          CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;

// ver line
		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// hor line left circle
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// hor line right circle
		                          CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22))
			return 1;

		if ((CalcP->x11 - CalcP->y12 * 0.5 > CalcP->x21 + CalcP->lengte2)
		        || (CalcP->x11 + CalcP->y12 * 0.5 < CalcP->x21) || (CalcP->y11 > CalcP->y21 + CalcP->y22 * 0.5)
		        || (CalcP->y11 + CalcP->lengte1 < CalcP->y21 - CalcP->y22 * 0.5))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
	case PIN_LINE_VER:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                            CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                            CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                          CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// ver line
		if ((CalcP->x11 - CalcP->y12 * 0.5 > CalcP->x21 + CalcP->y22 * 0.5)
		        || (CalcP->x11 + CalcP->y12 * 0.5 < CalcP->x21 - CalcP->y22 * 0.5)
		        || (CalcP->y11 > CalcP->y21 + CalcP->lengte2) || (CalcP->y11 + CalcP->lengte1 < CalcP->y21))
			return 0;

		return 1;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag1 line right bottom circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                          CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

// diag1 line
		if (CircleTestDiag1Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (RectTestDiag1Objects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                         CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                          CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

// diag2 line right top circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if (RectTestCircleObjects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                          CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

// diag2 line
		if (CircleTestDiag2Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// ver line bottom circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// ver line top circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (RectTestDiag2Objects(CalcP->x11, CalcP->y11 + CalcP->lengte1 * 0.5, CalcP->y12, CalcP->x12,	// ver line
		                         CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 TraceDiag1Check(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// diag1 line right bottom circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// diag1 line
		if (CircleTestDiag1Objects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_SMD_RECT:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// diag1 line right bottom circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1,
		         CalcP->y12))
			return 1;

// diag1 line
		if (RectTestDiag1Objects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// diag1 line right bottom circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1,
		         CalcP->y12))
			return 1;

// diag1 line
		if (RectTestDiag1Objects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1a, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1a, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x22)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1a, CalcP->x22)) && (InRange(CalcP->y11 - CalcP->lengte1a, CalcP->y22)))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:

// hor line left circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag1 line left top circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12))
			return 1;			// diag1 line right bottom circle

		if (CircleTestDiag1Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag1 line

// hor line right circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag1 line left top circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 - CalcP->lengte1, CalcP->y12))
			return 1;			// diag1 line right bottom circle

		if (CircleTestDiag1Objects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag1 line

// hor line
		if (RectTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag1 line left top circle

		if (RectTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 - CalcP->lengte1, CalcP->y12))
			return 1;			// diag1 line right bottom circle

		if (RectTestDiag1Objects
		        (CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12,
		         CalcP->y12))
			return 1;			// diag1 line

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
	case PIN_LINE_VER:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag1 line left top circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12))
			return 1;			// diag1 line right bottom circle

		if (CircleTestDiag1Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag1 line

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag1 line left top circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21))
		        && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 - CalcP->lengte1, CalcP->y12))
			return 1;			// diag1 line right bottom circle

		if (CircleTestDiag1Objects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag1 line

// ver line

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag1 line left top circle

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 - CalcP->lengte1, CalcP->y12))
			return 1;			// diag1 line right bottom circle

		if (RectTestDiag1Objects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->x12,
		         CalcP->y12))
			return 1;			// diag1 line

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag1 line left circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12,	// diag1 line right circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag1 line

// diag1 line right bottom circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag1 line left circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12,	// diag1 line right circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12,
		         CalcP->y12))
			return 1;			// diag1 line

// diag1 line
		if (CircleTestDiag1Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag1 line left circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12,	// diag1 line right circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (Diag1TestDiag1Objects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12,	// diag1 line
		                          CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag1 line left top circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12,	// diag1 line right bottom circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag1 line

// diag2 line right top circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag1 line left top circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 - CalcP->lengte1, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12,	// diag1 line right bottom circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12,
		         CalcP->y12))
			return 1;			// diag1 line

// diag2 line
		if (CircleTestDiag2Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag1 line left top circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->y12,	// diag1 line right bottom circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (Diag1TestDiag2Objects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12,	// diag1 line
		                          CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;

		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 TraceDiag2Check(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_SMD_ROUND:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// diag2 line right top circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12, CalcP->x21, CalcP->y21, CalcP->x22))
			return 1;

// diag2 line
		if (CircleTestDiag2Objects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_SMD_RECT:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// diag2 line right top circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1,
		         CalcP->y12))
			return 1;

// diag2 line
		if (RectTestDiag2Objects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_SQUARE:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;

// diag2 line right top circle
		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1,
		         CalcP->y12))
			return 1;

// diag2 line
		if (RectTestDiag2Objects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1a, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1a, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x22)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1a, CalcP->x22)) && (InRange(CalcP->y11 + CalcP->lengte1a, CalcP->y22)))
			return 1;

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
	case PIN_LINE_HOR:

// hor line left circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag2 line left bottom circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;			// diag2 line right top circle

		if (CircleTestDiag2Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag2 line

// hor line right circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag2 line left bottom circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;			// diag2 line right top circle

		if (CircleTestDiag2Objects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag2 line

// hor line
		if (RectTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag2 line left bottom circle

		if (RectTestCircleObjects
		        (CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;			// diag2 line right top circle

		if (RectTestDiag2Objects
		        (CalcP->x21 + CalcP->lengte2 * 0.5, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12,
		         CalcP->y12))
			return 1;			// diag2 line

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
	case PIN_LINE_VER:

// ver line bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag2 line left bottom circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;			// diag2 line right top circle

		if (CircleTestDiag2Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag2 line

// ver line top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag2 line left bottom circle

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21))
		        && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;			// diag2 line right top circle

		if (CircleTestDiag2Objects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag2 line

// ver line
		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->y12))
			return 1;			// diag2 line left bottom circle

		if (RectTestCircleObjects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22, CalcP->x11 + CalcP->lengte1,
		         CalcP->y11 + CalcP->lengte1, CalcP->y12))
			return 1;			// diag2 line right top circle

		if (RectTestDiag2Objects
		        (CalcP->x21, CalcP->y21 + CalcP->lengte2 * 0.5, CalcP->y22, CalcP->x22, CalcP->x11, CalcP->y11, CalcP->x12,
		         CalcP->y12))
			return 1;			// diag2 line

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:

// diag1 line left top circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag2 line left bottom circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// diag2 line right top circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag2 line

// diag1 line right bottom circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag2 line left bottom circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// diag2 line right top circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects
		        (CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12,
		         CalcP->y12))
			return 1;			// diag2 line

// diag1 line
		if (CircleTestDiag1Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag2 line left bottom circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag1Objects(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// diag2 line right top circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (Diag1TestDiag2Objects
		        (CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag2 line

		return 0;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:

// diag2 line left bottom circle
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag2 line left bottom circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21)) && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// diag2 line right top circle
		                            CalcP->x21, CalcP->y21, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects(CalcP->x21, CalcP->y21, CalcP->y22, CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;			// diag2 line

// diag2 line right top circle
		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag2 line left bottom circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if ((InRange(CalcP->x11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2))
		        && (InRange(CalcP->y11 + CalcP->lengte1, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if (CircleTestCircleObjects(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// diag2 line right top circle
		                            CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects(CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2, CalcP->y22,	// diag2 line
		                           CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12))
			return 1;

// diag2 line
		if (CircleTestDiag2Objects(CalcP->x11, CalcP->y11, CalcP->y12,	// diag2 line left bottom circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (CircleTestDiag2Objects(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->y12,	// diag2 line right top circle
		                           CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		if (Diag2TestDiag2Objects(CalcP->x11, CalcP->y11, CalcP->x12, CalcP->y12,	// diag2 line
		                          CalcP->x21, CalcP->y21, CalcP->x22, CalcP->y22))
			return 1;

		return 0;
		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

int32 ConnectionCheck(CalculateParamsRecord * CalcP)
{
	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
		if ((InRange(CalcP->x11, CalcP->x21a)) && (InRange(CalcP->y11, CalcP->y21a)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21a)) && (InRange(CalcP->y12, CalcP->y21a)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x22)) && (InRange(CalcP->y11, CalcP->y22)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x22)) && (InRange(CalcP->y12, CalcP->y22)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2a)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2a)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21 + CalcP->lengte2)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2a)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21 + CalcP->lengte2a)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y12, CalcP->y21 - CalcP->lengte2)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2a)) && (InRange(CalcP->y11, CalcP->y21 - CalcP->lengte2a)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y12, CalcP->y21 - CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2a)) && (InRange(CalcP->y12, CalcP->y21 - CalcP->lengte2a)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y12, CalcP->y21 + CalcP->lengte2)))
			return 1;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
		if ((InRange(CalcP->x11, CalcP->x21)) && (InRange(CalcP->y11, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x11, CalcP->x21 + CalcP->lengte2a)) && (InRange(CalcP->y11, CalcP->y21 + CalcP->lengte2a)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21)) && (InRange(CalcP->y12, CalcP->y21)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2)) && (InRange(CalcP->y12, CalcP->y21 + CalcP->lengte2)))
			return 1;

		if ((InRange(CalcP->x12, CalcP->x21 + CalcP->lengte2a)) && (InRange(CalcP->y12, CalcP->y21 + CalcP->lengte2a)))
			return 1;

		break;
	}

	return 0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToRoundSquareRectangle(CalculateParamsRecord * CalcP)
{
	double Length11, Length12;

	ConnectionX1 = CalcP->x11;
	ConnectionY1 = CalcP->y11;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_SMD_RECT:
	case DRILL:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;
		return CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x22, CalcP->y22);

		if (Length11 < Length12)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}
		else
		{
			ConnectionX2 = CalcP->x22;
			ConnectionY2 = CalcP->y22;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:

// hor line left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line right circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);

		if (Length11 < Length12)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}
		else
		{
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;
		return CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21);

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:

// ver line bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// ver line top circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);

		if (Length11 < Length12)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}
		else
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2a);

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:

// diag1 line left top circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag1 line right bottom circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);

		if (Length11 < Length12)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}
		else
		{
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 - CalcP->lengte2;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;
		return CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 - CalcP->lengte2a);

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:

// diag2 line left bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag2 line right top circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2);

		if (Length11 < Length12)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}
		else
		{
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 + CalcP->lengte2a);
	}

	return 0.0;
}

// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToTraceHor(CalculateParamsRecord * CalcP)
{
	double Length11, Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_SMD_RECT:
	case DRILL:
	case PIN_SMD_POLYGON:
	case PIN_SMD_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
	case PIN_PUT_THROUGH_POLYGON:
	case AREAFILL:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;

// hor line left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);

// hor line right circle
		Length12 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21);

		if (Length11 < Length12)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length11;
		}
		else
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x22, CalcP->y22);
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x22, CalcP->y22);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x22;
			ConnectionY2 = CalcP->y22;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
// hor line1 left circle,hor line2 left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 left circle,hor line2 right circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);
// hor line1 right circle,hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 right circle,hor line2 right circle
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);
		ConnectionY1 = CalcP->y11;
		ConnectionY2 = CalcP->y21;

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionX2 = CalcP->x21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		return Length22;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
// hor line1 right circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
// hor line1 right circle
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_VER:
// hor line1 left circle,ver line2 left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 left circle,ver line2 right circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);
// hor line1 right circle,ver line2 left circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 right circle,ver line2 right circle
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x21;

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case PIN_LINE_VER:
// hor line1 right circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2a);
// hor line1 right circle
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2a);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_DIAG1:
// hor line1 left circle,diag1 line left top circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 right circle,diag1 line left top circle
		Length12 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 left circle,diag1 line right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);
// hor line1 right circle,diag1 line right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 - CalcP->lengte2;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;

		return Length22;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
// hor line1 left circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 - CalcP->lengte2a);
// hor line1 right circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
// hor line1 left circle,diag2 line left bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 right circle,diag2 line left bottom circle
		Length12 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line1 left circle,diag2 line right top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2);
// hor line1 right circle,diag2 line right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
// hor line1 left circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 + CalcP->lengte2a);
// hor line1 right circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;
	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToTraceVer(CalculateParamsRecord * CalcP)
{
	double Length11, Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;

// ver line bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);

// ver line top circle
		Length12 = CalcLengthLine(CalcP->x21, CalcP->y21, CalcP->x11, CalcP->y11 + CalcP->lengte1);

		if (Length11 < Length12)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length11;
		}
		else
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x22, CalcP->y22);
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x22, CalcP->y22);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x22;
			ConnectionY2 = CalcP->y22;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
// ver line1 bottom circle,hor line2 left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// ver line1 bottom circle,hor line2 right circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);
// ver line1 top circle,hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// ver line1 top circle,hor line2 right circle
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2, CalcP->y21);
		ConnectionX1 = CalcP->x11;
		ConnectionY2 = CalcP->y21;

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			return Length21;
		}

		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
// ver line1 top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
// ver line1 top circle
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_VER:
// ver line1 bottom circle,ver line2 left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// ver line1 bottom circle,ver line2 right circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);
// ver line1 top circle,ver line2 left circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// ver line1 top circle,ver line2 right circle
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11;
		ConnectionX2 = CalcP->x21;

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionY1 = CalcP->y11;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionY1 = CalcP->y11;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
// ver line1 top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// ver line1 top circle
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21 + CalcP->lengte2a);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_DIAG1:

// ver line1 bottom circle,diag1 line left top circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// ver line1 bottom circle,diag1 line left top circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// ver line1 top circle,diag1 line right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);
// ver line1 top circle,diag1 line right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 - CalcP->lengte2;
			return Length21;
		}

		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
// ver line1 top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);
// ver line1 top circle
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
// ver line1 bottom circle,diag2 line left bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// ver line1 bottom circle,diag2 line left bottom circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// ver line1 top circle,diag2 line right top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2);
// ver line1 top circle,diag2 line right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length21;
		}

		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
// ver line1 top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 + CalcP->lengte2a);
// ver line1 top circle
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;
	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToTraceDiag1(CalculateParamsRecord * CalcP)
{
	double Length11, Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;

// diag1 line1 left top circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);

// diag1 line1 right bottom circle
		Length12 = CalcLengthLine(CalcP->x21, CalcP->y21, CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1);

		if (Length11 < Length12)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length11;
		}
		else
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 - CalcP->lengte1;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x22, CalcP->y22);
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x22, CalcP->y22);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x22;
			ConnectionY2 = CalcP->y22;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 - CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:

// diag1 line1 left top circle,hor line2 left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag1 line1 left top circle,hor line2 right circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);
// diag1 line1 right bottom circle,hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag1 line1 right bottom circle,hor line2 right circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 - CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
// diag1 line1 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
// diag1 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:

// diag1 line1 left top circle,ver line2 bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag1 line1 left top circle,ver line2 bottom circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);
// diag1 line1 right bottom circle,ver line2 top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag1 line1 right bottom circle,ver line2 top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 - CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
// diag1 line1 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2a);
// diag1 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2a);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:

// diag1 line1 left top circle,diag1 line2 left top circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag1 line1 left top circle,diag1 line2 left top circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);
// diag1 line1 right bottom circle,diag1 line2 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag1 line1 right bottom circle,diag1 line2 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 - CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 - CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:

// diag1 line1 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 - CalcP->lengte2a);
// diag1 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:

// diag1 line1 left top circle,diag2 line2 left bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag1 line1 left top circle,diag2 line2 left bottom circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2);
// diag1 line1 right bottom circle,diag2 line2 right top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag1 line1 right bottom circle,diag2 line2 right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 - CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
// diag1 line1 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag1 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 - CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToTraceDiag2(CalculateParamsRecord * CalcP)
{
	double Length11, Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;

// diag2 line1 left top circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);

// diag2 line1 right bottom circle
		Length12 = CalcLengthLine(CalcP->x21, CalcP->y21, CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1);

		if (Length11 < Length12)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length11;
		}
		else
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x22, CalcP->y22);
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x22, CalcP->y22);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x22;
			ConnectionY2 = CalcP->y22;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:

// diag2 line1 left top circle,hor line2 left circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag2 line1 left top circle,hor line2 right circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);
// diag2 line1 right bottom circle,hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag2 line1 right bottom circle,hor line2 right circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
// diag2 line1 right bottom circle
		Length21 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21);
// diag2 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:

// diag2 line1 left top circle,ver line2 bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag2 line1 left top circle,ver line2 bottom circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);
// diag2 line1 right bottom circle,ver line2 top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag2 line1 right bottom circle,ver line2 top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
// diag2 line1 right bottom circle
		Length21 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2a);
// diag2 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2a);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:

// diag2 line1 left top circle,diag1 line2 left top circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag2 line1 left top circle,diag1 line2 left top circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);
// diag2 line1 right bottom circle,diag1 line2 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag2 line1 right bottom circle,diag1 line2 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 - CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
// diag2 line1 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 - CalcP->lengte2a);
// diag2 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:

// diag2 line1 left top circle,diag2 line2 left bottom circle
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
// diag2 line1 left top circle,diag2 line2 left bottom circle
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2);
// diag2 line1 right bottom circle,diag2 line2 right top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21, CalcP->y21);
// diag2 line1 right bottom circle,diag2 line2 right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x11 + CalcP->lengte1;
			ConnectionY1 = CalcP->y11 + CalcP->lengte1;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:

// diag2 line1 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 + CalcP->lengte2a);
// diag2 line1 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1, CalcP->y11 + CalcP->lengte1, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length21 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length21;
		}

		return Length22;
	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToPinLineHor(CalculateParamsRecord * CalcP)
{
	double Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;
		Length12 = CalcLengthLine(CalcP->x21, CalcP->y21, CalcP->x11 + CalcP->lengte1a, CalcP->y11);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;
		return Length12;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x22, CalcP->y22);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;

		if (Length21 < Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
// hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21, CalcP->y21);
// hor line2 right circle
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
// ver line2 top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21, CalcP->y21);
// ver line2 top circle
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
// diag1 line2 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21, CalcP->y21);
// diag1 line2 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
// diag2 line2 right top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21, CalcP->y21);
// diag2 line2 right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;
	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToPinLineVer(CalculateParamsRecord * CalcP)
{
	double Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;
		Length12 = CalcLengthLine(CalcP->x21, CalcP->y21, CalcP->x11, CalcP->y11 + CalcP->lengte1a);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		return Length12;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x22, CalcP->y22);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 < Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:

// hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// hor line2 right circle
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2, CalcP->y21);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
// ver line2 top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// ver line2 top circle
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
// diag1 line2 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// diag1 line2 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
// diag2 line2 right top circle
		Length21 = CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// diag2 line2 right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
		ConnectionX1 = CalcP->x11;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;
	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToPinLineDiag1(CalculateParamsRecord * CalcP)
{
	double Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;
		Length12 = CalcLengthLine(CalcP->x21, CalcP->y21, CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;
		return Length12;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x22, CalcP->y22);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:

// hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21, CalcP->y21);
// hor line2 right circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
// ver line2 top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21, CalcP->y21);
// ver line2 top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
// diag1 line2 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21, CalcP->y21);
// diag1 line2 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
// diag2 line2 right top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21, CalcP->y21);
// diag2 line2 right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 - CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 - CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;
	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToPinLineDiag2(CalculateParamsRecord * CalcP)
{
	double Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_POLYGON:
	case PIN_SMD_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;
		Length12 = CalcLengthLine(CalcP->x21, CalcP->y21, CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		return Length12;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x22, CalcP->y22);

		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
// hor line2 left circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// hor line2 right circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_HOR:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_VER:
// ver line2 top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// ver line2 top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_VER:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG1:
// diag1 line2 right bottom circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// diag1 line2 right bottom circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 - CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG1:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 - CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
// diag2 line2 right top circle
		Length21 = CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21, CalcP->y21);
// diag2 line2 right top circle
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2,
		                   CalcP->y21 + CalcP->lengte2);
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;

		if (Length21 <= Length22)
		{
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_LINE_DIAG2:
		ConnectionX1 = CalcP->x11 + CalcP->lengte1a;
		ConnectionY1 = CalcP->y11 + CalcP->lengte1a;
		Length22 =
		    CalcLengthLine(CalcP->x11 + CalcP->lengte1a, CalcP->y11 + CalcP->lengte1a, CalcP->x21 + CalcP->lengte2a,
		                   CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;
		return Length22;

	}

	return 0.0;
}


// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************
// *****************************************************************************************************************

double CalculateDistanceToLinesArcs(CalculateParamsRecord * CalcP)
{
	double Length11, Length12, Length21, Length22;

	switch (CalcP->ObjectType2)
	{
	/***************************************************************************************/
	/***************************************************************************************/
	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case PIN_SMD_RECT:
	case PIN_SMD_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_SQUARE:
	case AREAFILL:
	case PIN_PUT_THROUGH_POLYGON:
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21;
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21);

		if (Length11 < Length12)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length11;
		}
		else
		{
			ConnectionX1 = CalcP->x12;
			ConnectionY1 = CalcP->y12;
			return Length12;
		}

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case CONNECTION:
	case PIN_ARC:
	case TRACE_ARC:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
	case OBJECT_LINE:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x22, CalcP->y22);
		Length21 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x22, CalcP->y22);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x22;
			ConnectionY2 = CalcP->y22;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x12;
			ConnectionY1 = CalcP->y12;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		ConnectionX2 = CalcP->x22;
		ConnectionY2 = CalcP->y22;
		return Length22;

		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_HOR:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21);
		Length21 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2, CalcP->y21);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x12;
			ConnectionY1 = CalcP->y12;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case PIN_LINE_HOR:
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2a, CalcP->y21);

		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21;

		if (Length12 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length12;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_VER:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2);
		Length21 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x12;
			ConnectionY1 = CalcP->y12;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case PIN_LINE_VER:
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21 + CalcP->lengte2a);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21 + CalcP->lengte2a);

		ConnectionX2 = CalcP->x21;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length12 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length12;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case TRACE_DIAG1:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);
		Length21 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2, CalcP->y21 - CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 - CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x12;
			ConnectionY1 = CalcP->y12;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case PIN_LINE_DIAG1:
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 - CalcP->lengte2a);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2a, CalcP->y21 - CalcP->lengte2a);

		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 - CalcP->lengte2a;

		if (Length12 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length12;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		return Length22;
		break;

	/***************************************************************************************/
	/***************************************************************************************/
	case TRACE_DIAG2:
		Length11 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21, CalcP->y21);
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2);
		Length21 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21, CalcP->y21);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2, CalcP->y21 + CalcP->lengte2);

		if ((Length11 <= Length12) && (Length11 <= Length21) && (Length11 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length11;
		}

		if ((Length12 <= Length11) && (Length12 <= Length21) && (Length12 <= Length22))
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			ConnectionX2 = CalcP->x21 + CalcP->lengte2;
			ConnectionY2 = CalcP->y21 + CalcP->lengte2;
			return Length12;
		}

		if ((Length21 <= Length11) && (Length21 <= Length12) && (Length21 <= Length22))
		{
			ConnectionX1 = CalcP->x12;
			ConnectionY1 = CalcP->y12;
			ConnectionX2 = CalcP->x21;
			ConnectionY2 = CalcP->y21;
			return Length21;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		ConnectionX2 = CalcP->x21 + CalcP->lengte2;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2;
		return Length22;

		break;

	/***************************************************************************************/
	/***************************************************************************************/

	case PIN_LINE_DIAG2:
		Length12 = CalcLengthLine(CalcP->x11, CalcP->y11, CalcP->x21 + CalcP->lengte2a, CalcP->y21 + CalcP->lengte2a);
		Length22 = CalcLengthLine(CalcP->x12, CalcP->y12, CalcP->x21 + CalcP->lengte2a, CalcP->y21 + CalcP->lengte2a);
		ConnectionX2 = CalcP->x21 + CalcP->lengte2a;
		ConnectionY2 = CalcP->y21 + CalcP->lengte2a;

		if (Length12 <= Length22)
		{
			ConnectionX1 = CalcP->x11;
			ConnectionY1 = CalcP->y11;
			return Length12;
		}

		ConnectionX1 = CalcP->x12;
		ConnectionY1 = CalcP->y12;
		return Length22;
	}

	return 0.0;
}
