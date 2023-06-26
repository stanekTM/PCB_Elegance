/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: rules.c
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
#include "toets.h"
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
#include "move3.h"
#include "pcb.h"
#include "rules.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "math.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "select3.h"
#include "polygon.h"
#include "mainloop.h"
#include "resource.h"
#include "owntime.h"


extern HDC OutputDisplay;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AddClearanceToObject(ObjectRecord * Object, double Clearance)
{
	Clearance -= 10.0;

	switch (Object->ObjectType)
	{
	case TRACE_VER:
	case PIN_LINE_VER:
	case TRACE_HOR:
	case PIN_LINE_HOR:
	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:
	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:
		Object->y2 += Clearance * 2.0;
		break;

	case PIN_PUT_THROUGH_ROUND:
	case PIN_SMD_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
	case VIA_PUT_THROUGH_ROUND:
	case DRILL:
	case DRILL_UNPLATED:
		Object->x2 += Clearance * 2.0;
		break;

	case OBJECT_LINE:
		Object->x3 += Clearance * 2.0;
		break;

	case PIN_SMD_RECT:
		Object->x2 += Clearance * 2.0;
		Object->y2 += Clearance * 2.0;
		break;

	case OBJECT_POLYGON:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		Object->Info |= OBJECT_WITH_CLEARANCE;
		Object->Clearance = Clearance - 0.01e5;
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ViewDesignRuleError(int32 mode)
{
	ObjectRecord *Object, *Object2;
	double minx, maxx, miny, maxy;

	if (CurrentErrorNr >= 0)
	{
		Object = &((*ErrorObjects)[CurrentErrorNr * 2]);
		Object2 = &((*ErrorObjects)[CurrentErrorNr * 2 + 1]);
		Object->minx = 10000.0e5;
		Object->miny = 10000.0e5;
		Object->maxx = -10000.0e5;
		Object->maxy = -10000.0e5;
		Object2->minx = 10000.0e5;
		Object2->miny = 10000.0e5;
		Object2->maxx = -10000.0e5;
		Object2->maxy = -10000.0e5;
		FillPositionObject(Object);

		if ((Object2->ObjectType != AREAFILL) && ((Object2->ObjectType != OBJECT_LINE) || ((Object2->Info2 & 2) == 2)))
		{
			FillPositionObject(Object2);
			minx = Object2->minx;
			miny = Object2->miny;
			maxx = Object2->maxx;
			maxy = Object2->maxy;
			/*
			      minx=min(Object->minx,Object2->minx);
			      miny=min(Object->miny,Object2->miny);
			      maxx=max(Object->maxx,Object2->maxx);
			      maxy=max(Object->maxy,Object2->maxy);
			*/
		}
		else
		{
			if (Object2->ObjectType == AREAFILL)
			{
				minx = Object2->x1;
				miny = Object2->y1;
				maxx = Object2->x1;
				maxy = Object2->y1;
			}
			else
			{
				minx = Object2->minx;
				miny = Object2->miny;
				maxx = Object2->maxx;
				maxy = Object2->maxy;
			}
		}

		if ((Object2->ObjectType == AREAFILL) && (Object->ObjectType != AREAFILL))
		{
			minx = Object->x1;
			miny = Object->y1;
			maxx = Object->x1;
			maxy = Object->y1;
		}

		if (minx < 10000.0e5)
			CenterScreenOnPoint((minx + maxx) / 2, (miny + maxy) / 2, 0);

		InvalidateRect(PCBWindow, NULL, 0);
		PostMessage(PCBWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
		UpdateWindow(PCBWindow);

		if (minx < 10000.0e5)
		{
			DrawLineYellow((minx + maxx) / 2, -10000.0e5, (minx + maxx) / 2, 10000.0e5);
			DrawLineYellow(-10000.0e5, (miny + maxy) / 2, 10000.0e5, (miny + maxy) / 2);
		}
	}
	else
		ViewFull();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 ErrorExist(ObjectRecord * Object1, ObjectRecord * Object2, int32 mode)
{
	int32 ok, cnt;
	ObjectRecord *ErrorObject1, *ErrorObject2;

	for (cnt = 0; cnt < NrErrorObjects / 2; cnt++)
	{
		ErrorObject1 = &((*ErrorObjects)[cnt * 2]);
		ErrorObject2 = &((*ErrorObjects)[cnt * 2 + 1]);

		if ((ErrorObject1->ObjectType == Object1->ObjectType) && (ErrorObject2->ObjectType == Object2->ObjectType))
		{
			if ((InRange(ErrorObject1->x1, Object1->x1)) && (InRange(ErrorObject1->y1, Object1->y1))
			        && (InRange(ErrorObject2->x1, Object2->x1)) && (InRange(ErrorObject2->y1, Object2->y1)))
			{
				ok = 1;
				return 1;
			}
		}

		if ((ErrorObject2->ObjectType == Object1->ObjectType) && (ErrorObject1->ObjectType == Object2->ObjectType))
		{
			if ((InRange(ErrorObject2->x1, Object1->x1)) && (InRange(ErrorObject2->y1, Object1->y1))
			        && (InRange(ErrorObject1->x1, Object2->x1)) && (InRange(ErrorObject1->y1, Object2->y1)))
			{
				ok = 1;
				return 1;
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


int ObjectsCompare(const void *arg1, const void *arg2)
{
	if (((ObjectRecord *) arg1)->Info2 > ((ObjectRecord *) arg2)->Info2)
		return 1;

	if (((ObjectRecord *) arg1)->Info2 == ((ObjectRecord *) arg2)->Info2)
		return 0;

	return -1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckDesignRules(int32 TestLayer, int32 mode)
{
	int32 ok, cnt, cnt2, count, Found, res, Layer, TotalFound, Divisions, ObjectNr, StartLayer, EndLayer, CountNr,
	      count2, cnt3, res2, MemSize, NrErrors, cntx, cnty, NetNr;
	double xx1, yy1, xx2, yy2, x1, y1, x11, y11, x22, y22, BoardDivX, BoardDivY, Clearance, MaxClearance, Length,
	       NewLength, BoardStartX, BoardStartY, NewClearance, DesignBoardWidth, DesignBoardHeight, DesignBoardOriginX,
	       DesignBoardOriginY, ObjectMinX, ObjectMinY, ObjectMaxX, ObjectMaxY, ThickNess;
	PolygonRecord *AreaFillPolygon, *PolygonObject, *BiggerPolygon, *AreaFillPolygon2, *ChangedPolygon,
	              *BoardOutLinePolygon;
	ObjectRecord *Object, *Object1, *Object2, NewObject, NewObject2, *ErrorObject, *Object5;
	int32 FoundError, PowerPlaneLayer, IncludeError, SurroundingError, Stop, FoundAreafillError, OtherErrors,
	      CheckBoardOutline;
	uint8 *AreaPos, *PolygonPos, *ErrorNrs;
	AreaFillRecord *AreaFill, *AreaFill2, *BoardOutlineAreaFill;
	NetRecord *Net;

	uint8 PolygonBuf2[10240];
	char str[MAX_LENGTH_STRING], CopyInfoStr[MAX_LENGTH_STRING];

//  if (AllocateMemAreaFillMemoryTemp(0)!=0) {
//    return 0;
//  }

// ************************************************************************************************
// ************************************************************************************************

	SelectionEsc = 0;
	AllocateSpecialMem(MEM_BOARDOUTLINE_AREAFILL, 128 * 1024, (void **) &BoardOutlineAreaFill);

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	CheckBoardOutline = 0;

	if (NotInRange(Design.BoardOutlineKeepOut, 0.0))
	{
		res = GetBoardOutlineAreaFill(BoardOutlineAreaFill, Design.BoardOutlineKeepOut, 1);

		switch (res)
		{
		case 0:
			CheckBoardOutline = 1;
			BoardOutLinePolygon = (PolygonRecord *) ((uint8 *) BoardOutlineAreaFill + sizeof(AreaFillRecord));
			break;

		case -1:
			break;

		case -2:
			break;
		}
	}

// ************************************************************************************************
// ************************************************************************************************

	res = FindMinMaxBoard(&DesignBoardOriginX, &DesignBoardOriginY, &DesignBoardWidth, &DesignBoardHeight, 1);
	DesignBoardWidth -= DesignBoardOriginX;
	DesignBoardHeight -= DesignBoardOriginY;

	AllocateSpecialMem(MEM_POLYGON_BIGGER, 128 * 1024, (void **) &ChangedPolygon);

	BoardStartX = DesignBoardOriginX;
	BoardStartY = DesignBoardOriginY;
	strcpy(CopyInfoStr, InfoStr);
	Divisions = max(2, Design.NrNets / 200);

	if (Design.NrNets < 200)
		Divisions = 1;

	Divisions = 6;

	NrErrors = 0;
	NrErrorObjects = 0;
	TotalFound = 0;
	BoardDivX = DesignBoardWidth / Divisions;
	BoardDivY = DesignBoardHeight / Divisions;

//  RecalcBoardSize(0);

//  SearchMinX=NewAreaFill->minx;
//  SearchMinY=NewAreaFill->miny;
//  SearchMaxX=NewAreaFill->maxx;
//  SearchMaxY=NewAreaFill->maxy;

	if (TestLayer == -1)
	{
		StartLayer = 0;
		EndLayer = Design.NrBoardLayers;
	}
	else
	{
		StartLayer = TestLayer;
		EndLayer = TestLayer + 1;
	}


	for (Layer = StartLayer; Layer < EndLayer; Layer++)
	{
		PowerPlaneLayer = IsLayerPowerPlane(Layer);

		if (!PowerPlaneLayer)
		{
			SearchMinX = -10000.0e5;
			SearchMinY = -10000.0e5;
			SearchMaxX = 10000.0e5;
			SearchMaxY = 10000.0e5;
			CountNr = 0;
#ifdef _DEBUG

			if (Layer == 1)
				ok = 1;

			SetTimer0();
			ResetPerformanceStrings();
			TotalFound = CopyCopperObjectsFromRectWindowToObjects4(Layer, 4 + 1);
			AddPerformanceValue2("Loading design rule objects");
			WritePerformanceStrings();
#endif


			for (cntx = 0; cntx < Divisions; cntx++)
			{
				for (cnty = 0; cnty < Divisions; cnty++)
				{
					ok = 1;
#ifdef _DEBUG

					if ((cntx == 1) && (cnty == 0))
					{
						if (Layer == 3)
							ok = 1;

						ok = 1;
					}

#endif
					SearchMinX = BoardStartX + (cntx) * BoardDivX - (1 * 2540);
					SearchMinY = BoardStartY + (cnty) * BoardDivY - (1 * 2540);
					SearchMaxX = BoardStartX + (cntx + 1) * BoardDivX + (1 * 2540);
					SearchMaxY = BoardStartY + (cnty + 1) * BoardDivY + (1 * 2540);

					Found = CopyCopperObjectsFromRectWindowToObjects4(Layer, 32 + 4);
					MaxClearance = Design.StandardClearance;

					for (cnt = 0; cnt < NrObjects4; cnt++)
					{
						Object = &((*Objects4)[cnt]);
						Object->Info2 = 1;

						if ((CheckObjectIsBigPolygon(Object)) || (Object->ObjectType == AREAFILL))
							Object->Info2 = 0;
						else
							FillPositionObject(Object);
					}

//          qsort(Objects4,NrObjects4,sizeof(ObjectRecord),ObjectsCompare);
					ok = 1;

// ************************************************************************************************
// Check board outline against other objects
					for (cnt = 0; cnt < NrObjects4; cnt++)
					{
						Object = &((*Objects4)[cnt]);
#ifdef _DEBUG

						if (cnt == 577)
							ok = 1;

						if (Object->ObjectType == PIN_SMD_POLYGON)
							ok = 1;

						if (Object->NetNr == -2)
							ok = 1;

#endif

						if (Object->ObjectType != AREAFILL)
						{
							if (CheckBoardOutline)
							{
								if (CheckObjectIsBigPolygon(Object))
								{
									GetAreaFillFromBigPolygonObject(Object, &AreaFill2, 0.0, 0);
									PolygonObject = (PolygonRecord *) ((uint8 *) AreaFill2 + sizeof(AreaFillRecord));
								}
								else
									MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

								NewObject.ObjectType = OBJECT_LINE;
								NewObject.x1 = 0.0;
								NewObject.y1 = 0.0;

								//              DrawTestPolygon(PolygonObject,1);
								if ((CheckPolygonInsideAreaFill(PolygonObject, BoardOutlineAreaFill, 0) == 1)
								        || (CheckPolygonOverlapAreaFill(PolygonObject, BoardOutlineAreaFill) == 0))
								{
									if (!ErrorExist(Object, &NewObject, 0))
									{
										NrErrors++;

										if (NrErrorObjects + 2 >= MaxNrErrorObjects)
										{
											if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
												return -4;
										}

										ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
										ErrorObject->ObjectType = OBJECT_LINE;
										ErrorObject->x1 = 0.0;
										ErrorObject->y1 = 0.0;
										ErrorObject->Test = 0;
										ErrorObject->Info2 = 0;
										NrErrorObjects++;
										ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
										memmove(ErrorObject, Object, sizeof(ObjectRecord));
										ErrorObject->Test = 0;
										ErrorObject->Info2 = 0;
										NrErrorObjects++;
									}
								}
							}
						}

						MaxClearance = max(MaxClearance, Object->Clearance);
					}



// ************************************************************************************************
					/*
					          if ((Design.NrBoardLayers>2)
					             &&
					             (Layer>0)
					             &&
					             (Layer<Design.NrBoardLayers-1)) {
					            for (cnt=PinObjectsPos;cnt<NrObjects4;cnt++) {
					              Object=&((*Objects4)[cnt]);
					              switch (Object->ObjectType) {
					                case PIN_PUT_THROUGH_ROUND:
					                case PIN_PUT_THROUGH_SQUARE:
					                case PIN_PUT_THROUGH_POLYGON:
					                  Object->ObjectType=PIN_SMD_ROUND;
					                  if (NotInRange(Object->x3,0.0)) {
					// Innerpad exists
					                    Object->x2=Object->x3;
					                  }
					                  Object->Layer=Layer;
					                  break;
					              }
					            }
					          }
					*/
// ************************************************************************************************
// Check objects (No areafills/big polygons) against other objects (No areafills/big polygons)
					for (cnt = 0; cnt < NrObjects4; cnt++)
					{
						Object = &((*Objects4)[cnt]);

						if (Object->Info2 == 0)
							continue;

#ifdef _DEBUG

						if ((InRange9(Object->x1, 109.6e5)) && (InRange9(Object->y1, 90.2e5)))
							ok = 1;

						if ((InRange9(Object->x1, 172.7e5)) && (InRange9(Object->y1, 59.2e5)))
							ok = 1;

						// Layer
						if (Object->NetNr == -1)
							ok = 1;

						if (Object->NetNr == -1)
							ok = 1;

						if (cnt == 2)
							ok = 1;

						if (Object->ObjectType == OBJECT_LINE)
							ok = 1;

#endif
						memcpy(&NewObject, Object, sizeof(ObjectRecord));
						memcpy(&NewObject2, Object, sizeof(ObjectRecord));
						NetNr = Object->NetNr;
#ifdef _DEBUG

						if (1)
						{
#else

						if ((CountNr % 100) == 0)
						{
#endif
							sprintf(InfoStr, SC(1026, "Layer %i  %i (%i)"), Layer, CountNr, TotalFound);
							RedrawInfoStr(1);
							CheckForEscape();

							if (SelectionEsc)
							{
								NrErrorObjects = 0;
								strcpy(InfoStr, CopyInfoStr);
								RedrawInfoStr(1);
								SelectionEsc = 0;
								return 0;
							}
						}

						CountNr++;

						if (NewObject.NetNr != -2)
						{	// Non routing keepout objects
							FillPositionObjectWithClearance(&NewObject, MaxClearance);
						}

						ObjectMinX = NewObject.minx;
						ObjectMinY = NewObject.miny;
						ObjectMaxX = NewObject.maxx;
						ObjectMaxY = NewObject.maxy;

						if (NewObject.NetNr != -2)
						{	// Non routing keepout objects
							AddClearanceToObject(&NewObject, max(Object->Clearance, Design.StandardClearance));
						}

						for (cnt2 = 0; cnt2 < NrObjects4; cnt2++)
						{
							if (cnt2 != cnt)
							{
								Object2 = &((*Objects4)[cnt2]);

								if (Object2->NetNr == -2)
								{	// Routing keepout
									switch (Object->ObjectType)
									{
									case DRILL:
									case DRILL_UNPLATED:
									case PIN_LINE_ALL_ANGLE:
									case PIN_ARC:
									case PIN_LINE_VER:
									case PIN_LINE_HOR:
									case PIN_LINE_DIAG1:
									case PIN_LINE_DIAG2:
									case PIN_SMD_ROUND:
									case PIN_SMD_RECT:
									case PIN_PUT_THROUGH_ROUND:
									case PIN_PUT_THROUGH_SQUARE:
										ok = 1;
										continue;

									case PIN_PUT_THROUGH_POLYGON:
										ok = 1;
										continue;

									case PIN_SMD_POLYGON:
										ok = 1;
										continue;
									}
								}

								switch (Object2->ObjectType)
								{
								case DRILL:
								case DRILL_UNPLATED:
								case PIN_LINE_ALL_ANGLE:
								case PIN_ARC:
								case PIN_LINE_VER:
								case PIN_LINE_HOR:
								case PIN_LINE_DIAG1:
								case PIN_LINE_DIAG2:
								case PIN_SMD_ROUND:
								case PIN_SMD_RECT:
								case PIN_PUT_THROUGH_ROUND:
								case PIN_PUT_THROUGH_SQUARE:
									if (Object->NetNr == -2)
									{	// Routing keepout
										ok = 1;
										continue;
									}

								case PIN_PUT_THROUGH_POLYGON:
									if (Object->NetNr == -2)
									{	// Routing keepout
										ok = 1;
										continue;
									}

								case PIN_SMD_POLYGON:
									if (Object->NetNr == -2)
									{	// Routing keepout
										ok = 1;
										continue;
									}
								}

#ifdef _DEBUG

								if ((InRange9(Object2->x1, 110.5e5)) && (InRange9(Object2->y1, 91.57e5)))
								{
									ok = 1;

									if (Object->ObjectType == OBJECT_LINE)
										ok = 1;
								}

								if ((InRange9(Object2->x1, 180.3e5)) && (InRange9(Object2->y1, 58.4e5)))
									ok = 1;

								if (cnt == 2)
									ok = 1;

								if ((cnt == 4) && (cnt2 == 10))
									ok = 1;

								if (Object2->NetNr == -1)
									ok = 1;

								if (Object2->NetNr == -2)
									ok = 1;

#endif

								if ((Object2->NetNr != NetNr) && (Object2->Info2 == 1) && (Object2->minx < ObjectMaxX)
								        && (Object2->miny < ObjectMaxY) && (Object2->maxx > ObjectMinX)
								        && (Object2->maxy > ObjectMinY))
								{
									FoundError = 0;
									NewClearance = 0.0;

									if (Object2->Clearance > NewObject.Clearance + 10.0)
									{
										memcpy(&NewObject2, Object, sizeof(ObjectRecord));

										if (NewObject2.NetNr != -2)
										{	// Non routing keepout objects
											NewClearance = max(Object2->Clearance, Design.StandardClearance);
										}

										/*
										                    if (CheckObjectIsPolygonWithOpenSpots(&NewObject2,&Dummy)) {
										                      NewClearance-=1000.0;
										                    }
										*/
										if (NewObject2.NetNr != -2)
										{	// Non routing keepout objects
											AddClearanceToObject(&NewObject2, NewClearance);
										}

										if (ObjectsConnected(&NewObject2, Object2))
											FoundError = 1;
									}
									else
									{
										if (ObjectsConnected(&NewObject, Object2))
											FoundError = 1;
									}

									if (FoundError)
									{
										if (!ErrorExist(Object, Object2, 0))
										{
											NrErrors++;

											if (NrErrorObjects + 2 >= MaxNrErrorObjects)
											{
												if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
													return -4;
											}

											ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
											memmove(ErrorObject, Object, sizeof(ObjectRecord));

											if (Object->NetNr == -2)
											{	// Routing keepout layer
												ErrorObject->Layer += ROUTING_KEEPOUT_LAYER;
											}

											ErrorObject->Test = 0;
											ErrorObject->Info2 = 0;
											NrErrorObjects++;
											ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
											memmove(ErrorObject, Object2, sizeof(ObjectRecord));

											if (Object2->NetNr == -2)
											{	// Routing keepout layer
												ErrorObject->Layer += ROUTING_KEEPOUT_LAYER;
											}

											ErrorObject->Test = 0;
											ErrorObject->Info2 = 0;
											NrErrorObjects++;
										}
									}
								}
							}
						}
					}
				}

// ************************************************************************************************

			}

			sprintf(InfoStr, SC(1026, "Layer %i  %i ( %i )"), Layer, CountNr, TotalFound);
			RedrawInfoStr(1);

			if (MaxNrObjects4 > 4096)
				DeAllocateMemObjects4();
		}

// ************************************************************************************************
// ************************************************************************************************
// Check big pin polygons against other objects (No areafills/big polygons)

		SearchMinX = -10000.0e5;
		SearchMinY = -10000.0e5;
		SearchMaxX = 10000.0e5;
		SearchMaxY = 10000.0e5;

		NrObjects5 = CopyCompBigPolygonObjectsFromRectWindowToObjects4(Layer, 4);
		AllocateMemObjects5(NrObjects5);
		memcpy(Objects5, Objects4, NrObjects5 * sizeof(ObjectRecord));

		for (cnt = 0; cnt < NrObjects5; cnt++)
		{
			Object5 = &((*Objects5)[cnt]);
			FillPositionObject(Object5);
			SearchMinX = Object5->minx - 2.0e5;
			SearchMinY = Object5->miny - 2.0e5;
			SearchMaxX = Object5->maxx + 2.0e5;
			SearchMaxY = Object5->maxy + 2.0e5;

			if (CheckObjectIsBigPolygon(Object5))
			{
				GetAreaFillFromBigPolygonObject(Object5, &AreaFill, 0.0, 0);
				Found = CopyCopperObjectsFromRectWindowToObjects4(Layer, 4);
				AreaFill->Clearance = (float) Object5->Clearance;
				MaxClearance = max(Object5->Clearance, Design.StandardClearance);
				NetNr = Object5->NetNr;

				for (cnt = 0; cnt < NrObjects4; cnt++)
				{
					Object = &((*Objects4)[cnt]);
					MaxClearance = max(MaxClearance, Object->Clearance);
					Object->Info2 = 1;

					if ((CheckObjectIsBigPolygon(Object)) || (Object->ObjectType == AREAFILL))
						Object->Info2 = 0;
					else
						FillPositionObject(Object);
				}


				FoundError = 0;
				cnt2 = 0;

				while ((!FoundError) && (!SelectionEsc) && (cnt2 < Found))
				{
					ObjectNr = cnt2;
					Object = &((*Objects4)[cnt2]);

#ifdef _DEBUG

					if (cnt2 == 749)
						ok = 1;

#endif
					IncludeError = 0;
					SurroundingError = 0;

					if (!PowerPlaneLayer)
					{
						if (Object->Info2 == 1)
						{
							Clearance = max(AreaFill->Clearance, Object->Clearance) - 0.01e5;
							MakePolygonFromObject(Object, PolygonObject, Clearance, 0.00001, 1, 1);
#ifdef _DEBUG

							if ((InRange9(Object->x1, 102.4e5)) && (InRange9(Object->y1, 61.5e5)))
							{
								ok = 1;
							}

#endif

							if (Object->NetNr != NetNr)
							{
								if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
								{
									IncludeError = 1;
#ifdef _DEBUG

									if (0)
										PolygonToObjectLines(PolygonObject, INFO_LAYER4, 0, 0, 0);

									if (0)
										AreafillToObjectLines(NewAreaFill, INFO_LAYER3, 0, 0, 0);

#endif
								}
							}
							else
							{
								res = CheckPolygonInsideAreaFill(PolygonObject, AreaFill, 1);

								if ((res == 1) && (!ObjectIsTrace(Object)))
								{
#ifdef _DEBUG
									/*
									                  DrawTestPolygon(PolygonObject,1);  // green
									                  while (!KeyPressed()) CheckInputMessages(0);
									                  ReadKeyFunction();
									*/
#endif

									if (NetNr >= 0)
									{
										IncludeError = 1;
										SurroundingError = 1;
									}
								}
							}
						}
					}
					else
					{
						if ((Object->Info2 == 1) && (Object->NetNr == NetNr))
						{
#ifdef _DEBUG

							if ((InRange9(Object->x1, 171.8e5)) && (InRange9(Object->y1, 94.0e5)))
								ok = 1;

#endif
							Clearance = max(AreaFill->Clearance, Object->Clearance) - 0.01e5;
							Object->Clearance -= 50.0;
							Object->Clearance -= 50.0;
							MakePolygonFromObject(Object, PolygonObject, AreaFill->Clearance - 50.0, 0.00001, 1, 0);
//              DrawTestPolygon(PolygonObject,1);
							res = CheckPolygonInsideAreaFill(PolygonObject, AreaFill, 1);

							if (res == 1)
							{
								IncludeError = 1;
								SurroundingError = 1;
							}
						}
					}

					if (IncludeError)
					{
						NrErrors++;

						if (NrErrorObjects >= MaxNrErrorObjects)
						{
							if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
								return -4;
						}

						ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
						memmove(ErrorObject, Object, sizeof(ObjectRecord));

						if (SurroundingError)
							ErrorObject->Info2 = 1;
						else
							ErrorObject->Info2 = 0;

						NrErrorObjects++;
						memset(&NewObject, 0, sizeof(ObjectRecord));
						NewObject.ObjectType = AREAFILL;
						NewObject.TraceNr = cnt;
						NewObject.NetNr = NetNr;
						NewObject.Info = AreaFill->Info;
						AreaPos = (uint8 *) AreaFill;
						AreaFillPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
						NewObject.x1 = (*AreaFillPolygon).Points[0].x;
						NewObject.y1 = (*AreaFillPolygon).Points[0].y;
						NewObject.Layer = AreaFill->Layer;
						ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
						memmove(ErrorObject, &NewObject, sizeof(ObjectRecord));
						ErrorObject->Info2 = 0;
						NrErrorObjects++;
						ok = 1;
					}

//          CheckForEscape();
					cnt2++;
				}
			}
		}


// **************************************************************************************
// **************************************************************************************
// Check areafills against other areafills

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
			{
				NetNr = AreaFill->NetNr;

				if (NetNr >= 0)
					Net = &((*Nets)[NetNr]);
				else
					Net = &EmptyNet;

				AreaFillPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
				Clearance = max(AreaFill->Clearance, Design.StandardClearance);

				for (cnt2 = 0; cnt2 < Design.NrAreaFills; cnt2++)
				{
					AreaFill2 = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt2]]);

					if (((AreaFill2->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0) && (AreaFill2->Layer == Layer)
					        && (AreaFill2->NetNr != NetNr))
					{
						MaxClearance = max(Clearance, AreaFill2->Clearance);
						MemSize = MemSizePolygon(AreaFillPolygon) + 8192;
						AllocateSpecialMem(MEM_POLYGON_BIGGER, MemSize, (void **) &BiggerPolygon);
						MakeBiggerSmallerPolygon(AreaFillPolygon, BiggerPolygon, MaxClearance * 1.99, 0);
						SetMinMaxPolygon(BiggerPolygon, 0);
						res = CheckPolygonOverlapAreaFill(BiggerPolygon, AreaFill2);

						if (res == 1)
						{
							NrErrors++;

							if (NrErrorObjects >= MaxNrErrorObjects)
							{
								if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
									return -4;
							}

							NewObject.ObjectType = AREAFILL;
							NewObject.TraceNr = cnt;
							NewObject.NetNr = NetNr;
							NewObject.Info = AreaFill->Info;
							AreaPos = (uint8 *) AreaFill;
							AreaFillPolygon2 = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
							NewObject.x1 = (*AreaFillPolygon2).Points[0].x;
							NewObject.y1 = (*AreaFillPolygon2).Points[0].y;
							NewObject.Layer = AreaFill->Layer;
							ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
							memmove(ErrorObject, &NewObject, sizeof(ObjectRecord));
							ErrorObject->Info2 = 0;
							NrErrorObjects++;
							NewObject.ObjectType = AREAFILL;
							NewObject.TraceNr = cnt2;
							NewObject.NetNr = AreaFill2->NetNr;
							NewObject.Info = AreaFill->Info;
							AreaPos = (uint8 *) AreaFill2;
							AreaFillPolygon2 = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
							NewObject.x1 = (*AreaFillPolygon2).Points[0].x;
							NewObject.y1 = (*AreaFillPolygon2).Points[0].y;
							NewObject.Layer = AreaFill2->Layer;
							ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
							memmove(ErrorObject, &NewObject, sizeof(ObjectRecord));
							ErrorObject->Info2 = 0;
							NrErrorObjects++;
						}
					}
				}
			}
		}

// **************************************************************************************
// **************************************************************************************
// Check areafills against other objects

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
			{
				NetNr = AreaFill->NetNr;

				if (NetNr >= 0)
					Net = &((*Nets)[NetNr]);
				else
					Net = &EmptyNet;

#ifdef _DEBUG

				if (stricmpOwn(Net->Name, "VCORE"))
					ok = 1;

#endif
				SearchMinX = 10000.0e5;
				SearchMinY = 10000.0e5;
				SearchMaxX = -10000.0e5;
				SearchMaxY = -10000.0e5;

				AreaFillPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				for (cnt2 = 0; cnt2 < AreaFillPolygon->NrVertices; cnt2++)
				{
					x1 = (*AreaFillPolygon).Points[cnt2].x;
					y1 = (*AreaFillPolygon).Points[cnt2].y;
					SearchMinX = min(SearchMinX, x1 - (AreaFill->Clearance + 0.1e5));
					SearchMinY = min(SearchMinY, y1 - (AreaFill->Clearance + 0.1e5));
					SearchMaxX = max(SearchMaxX, x1 + (AreaFill->Clearance + 0.1e5));
					SearchMaxY = max(SearchMaxY, y1 + (AreaFill->Clearance + 0.1e5));
				}

				sprintf(InfoStr, SC(1027, "Areafill layer %i net %s"), Layer, Net->Name);
				RedrawInfoStr(1);

				Found = CopyCopperObjectsFromRectWindowToObjects4(Layer, 4);

				FoundError = 0;
				cnt2 = 0;

				while ((!FoundError) && (!SelectionEsc) && (cnt2 < Found))
				{
					ObjectNr = cnt2;
					Object = &((*Objects4)[cnt2]);

					if ((cnt2 % 100) == 0)
					{
						sprintf(InfoStr, SC(1028, "Areafill layer %i net %s  %i ( %i )"), Layer, Net->Name, cnt2, Found);
						RedrawInfoStr(1);
						CheckForEscape();

						if (SelectionEsc)
						{
							NrErrorObjects = 0;
							strcpy(InfoStr, CopyInfoStr);
							RedrawInfoStr(1);
							SelectionEsc = 0;
							return 0;
						}
					}

#ifdef _DEBUG

					if (cnt2 == 749)
						ok = 1;

#endif
					IncludeError = 0;
					SurroundingError = 0;

					if (!PowerPlaneLayer)
					{
						if (Object->ObjectType != 0)
						{
							if (CheckObjectIsBigPolygon(Object))
							{
								if (Object->NetNr != NetNr)
								{
									Clearance = max(AreaFill->Clearance, Object->Clearance) - 0.01e5;
									GetAreaFillFromBigPolygonObject(Object, &AreaFill2, Clearance, 0);

									if (CheckAreaFillsConnected(AreaFill2, AreaFill))
										IncludeError = 1;
								}
								else
								{
								}
							}
							else
							{
								Clearance = max(AreaFill->Clearance, Object->Clearance) - 0.01e5;

//              DrawTestPolygon(PolygonObject,1);
								if (Object->NetNr != NetNr)
								{
									MakePolygonFromObject(Object, PolygonObject, Clearance, 0.00001, 1, 1);
#ifdef _DEBUG

									if ((InRange9(Object->x1, 102.4e5)) && (InRange9(Object->y1, 61.5e5)))
									{
										/*
										                    DrawTestPolygon(PolygonObject,1);  // green
										                    while (!KeyPressed()) CheckInputMessages(0);
										                    ReadKeyFunction();
										*/
										ok = 1;
									}

#endif

									if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
									{
										IncludeError = 1;
#ifdef _DEBUG

										if (0)
											PolygonToObjectLines(PolygonObject, INFO_LAYER4, 0, 0, 0);

										if (0)
											AreafillToObjectLines(NewAreaFill, INFO_LAYER3, 0, 0, 0);

#endif
									}
								}
								else
								{
									MakePolygonFromObject(Object, PolygonObject, 0.0, 0.00001, 1, 1);
									res = CheckPolygonInsideAreaFill(PolygonObject, AreaFill, 1);

									if ((res == 1) && (!ObjectIsTrace(Object)))
									{
#ifdef _DEBUG
										/*
										                    DrawTestPolygon(PolygonObject,1);  // green
										                    while (!KeyPressed()) CheckInputMessages(0);
										                    ReadKeyFunction();
										*/
#endif

										if (NetNr >= 0)
										{
											IncludeError = 1;
											SurroundingError = 1;
										}
									}
								}
							}
						}
					}
					else
					{
						if ((Object->ObjectType != 0) && (Object->NetNr == NetNr))
						{
#ifdef _DEBUG

							if ((InRange9(Object->x1, 171.8e5)) && (InRange9(Object->y1, 94.0e5)))
								ok = 1;

#endif
							Clearance = max(AreaFill->Clearance, Object->Clearance) - 0.01e5;
							Object->Clearance -= 50.0;

							if (CheckObjectIsBigPolygon(Object))
							{
								GetAreaFillFromBigPolygonObject(Object, &AreaFill2, Clearance, 0);

								if (CheckAreaFillsConnected(AreaFill2, AreaFill))
								{
									IncludeError = 1;
									SurroundingError = 1;
								}
							}
							else
							{
								Object->Clearance -= 50.0;
								MakePolygonFromObject(Object, PolygonObject, AreaFill->Clearance - 50.0, 0.00001, 1, 0);
//              DrawTestPolygon(PolygonObject,1);
								res = CheckPolygonInsideAreaFill(PolygonObject, AreaFill, 1);

								if (res == 1)
								{
									IncludeError = 1;
									SurroundingError = 1;
								}
							}
						}
					}

					if (IncludeError)
					{
						NrErrors++;

						if (NrErrorObjects >= MaxNrErrorObjects)
						{
							if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
								return -4;
						}

						ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
						memmove(ErrorObject, Object, sizeof(ObjectRecord));

						if (SurroundingError)
							ErrorObject->Info2 = 1;
						else
							ErrorObject->Info2 = 0;

						NrErrorObjects++;
						memset(&NewObject, 0, sizeof(ObjectRecord));
						NewObject.ObjectType = AREAFILL;
						NewObject.TraceNr = cnt;
						NewObject.NetNr = NetNr;
						NewObject.Info = AreaFill->Info;
						AreaPos = (uint8 *) AreaFill;
						AreaFillPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
						NewObject.x1 = (*AreaFillPolygon).Points[0].x;
						NewObject.y1 = (*AreaFillPolygon).Points[0].y;
						NewObject.Layer = AreaFill->Layer;
						ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
						memmove(ErrorObject, &NewObject, sizeof(ObjectRecord));
						ErrorObject->Info2 = 0;
						NrErrorObjects++;
						ok = 1;
					}
					cnt2++;
				}

				if (MaxNrObjects4 > 4096)
					DeAllocateMemObjects4();

				sprintf(InfoStr, SC(1028, "Areafill layer %i net %s  %i ( %i )"), Layer, Net->Name, cnt2, Found);
				RedrawInfoStr(1);
			}
		}

// **************************************************************************************
// **************************************************************************************
// Check areafills against board outline

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
			{
				NetNr = AreaFill->NetNr;
				PowerPlaneLayer = IsLayerPowerPlane(Layer);

				if (NetNr >= 0)
					Net = &((*Nets)[NetNr]);
				else
					Net = &EmptyNet;

				AreaFillPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				if (CheckBoardOutline)
				{
					if ((CheckPolygonInsideAreaFill(AreaFillPolygon, BoardOutlineAreaFill, 0) == 1)
					        || (CheckPolygonOverlapAreaFill(AreaFillPolygon, BoardOutlineAreaFill) == 0))
					{
						NrErrors++;

						if (NrErrorObjects + 2 >= MaxNrErrorObjects)
						{
							if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
								return -4;
						}

						ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
						ErrorObject->ObjectType = OBJECT_LINE;
						ErrorObject->Test = 0;
						ErrorObject->Info2 = 0;
						NrErrorObjects++;
						memset(&NewObject, 0, sizeof(ObjectRecord));
						NewObject.ObjectType = AREAFILL;
						NewObject.TraceNr = cnt;
						NewObject.Layer = AreaFill->Layer;
						NewObject.x1 = AreaFillPolygon->Points[0].x;
						NewObject.y1 = AreaFillPolygon->Points[0].y;
						ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
						memmove(ErrorObject, &NewObject, sizeof(ObjectRecord));
						ErrorObject->Test = 0;
						ErrorObject->Info2 = 0;
						NrErrorObjects++;

					}
				}

// **************************************************************************************
// Check areafill for thin lines
				if ((!PowerPlaneLayer) && (!DoNotShowAreafillThinLinesError))
				{
					AreaFillPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
					PolygonPos = (uint8 *) AreaFillPolygon;
					ThickNess = min(Design.StandardClearance, Design.StandardTraceWidth);

					for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
					{
						count = AreaFillPolygon->NrVertices;
#ifdef _DEBUG

						if (count == 17)
							ok = 1;

#endif

						if (cnt2 == 0)
						{
							MakeBiggerSmallerPolygon(AreaFillPolygon, ChangedPolygon, ThickNess * 1.01, 1);	// smaller
						}
						else
						{
							MakeBiggerSmallerPolygon(AreaFillPolygon, ChangedPolygon, ThickNess * 1.01, 0);	// Bigger
						}

#ifdef _DEBUG
//            DrawTestPolygon4(AreaFillPolygon,ThickNess,0);
//            DrawTestPolygon4(ChangedPolygon,ThickNess,0);
#endif
						count2 = ChangedPolygon->NrVertices;

						for (cnt3 = 0; cnt3 < count2; cnt3++)
						{
							x11 = (*ChangedPolygon).Points[cnt3].x;
							y11 = (*ChangedPolygon).Points[cnt3].y;

							if (cnt3 < count2 - 1)
							{
								x22 = (*ChangedPolygon).Points[cnt3 + 1].x;
								y22 = (*ChangedPolygon).Points[cnt3 + 1].y;
							}
							else
							{
								x22 = (*ChangedPolygon).Points[0].x;
								y22 = (*ChangedPolygon).Points[0].y;
							}

							if ((res =
							            CheckLineCrossesWithAreaFill(x11, y11, x22, y22, AreaFill, &xx1, &yy1, &xx2, &yy2,
							                    ThickNess * 0.99, 0)) != -1)
							{
								if (res == 0)
								{
									xx1 = x11;
									yy1 = y11;
									xx2 = x22;
									yy2 = y22;
								}
								else
								{
									Length = CalcLengthLine(x11, y11, x22, y22);
									NewLength = CalcLengthLine(xx1, yy1, xx2, yy2);

									if ((Length > 2e5) && (NewLength < 1e5))
									{
										NrErrors++;
#ifdef _DEBUG

										if (0)
										{
											PolygonToObjectLines(AreaFillPolygon, INFO_LAYER3, ChangedPolygon,
											                     INFO_LAYER4, 0);
											PolygonVerticesToMessage(AreaFillPolygon, ChangedPolygon);

											if (cnt2 == 0)
											{
												MakeBiggerSmallerPolygon(AreaFillPolygon, ChangedPolygon, ThickNess * 1.01, 1);	// smaller
											}
											else
											{
												MakeBiggerSmallerPolygon(AreaFillPolygon, ChangedPolygon, ThickNess * 1.01, 0);	// Bigger
											}
										}

#endif

										if (NrErrorObjects + 2 >= MaxNrErrorObjects)
										{
											if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
												return -4;
										}

										memset(&NewObject, 0, sizeof(ObjectRecord));
										NewObject.ObjectType = AREAFILL;
										NewObject.TraceNr = cnt;
										NewObject.Layer = AreaFill->Layer;
										ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
										memmove(ErrorObject, &NewObject, sizeof(ObjectRecord));
										ErrorObject->Test = 0;
										ErrorObject->Info2 = 0;
										NrErrorObjects++;

										ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
										memset(ErrorObject, 0, sizeof(ObjectRecord));
										ErrorObject->ObjectType = OBJECT_LINE;
										ErrorObject->x1 = x11;
										ErrorObject->y1 = y11;
										ErrorObject->x2 = x22;
										ErrorObject->y2 = y22;
										ErrorObject->Thickness = ThickNess;
										ErrorObject->Test = 0;
										ErrorObject->Info2 = 2;
										NrErrorObjects++;
									}
								}
							}

							if (res != 0)
							{
								if ((res2 =
								            CheckLineCrossesWithAreaFill(x22, y22, x11, y11, AreaFill, &xx1, &yy1, &xx2, &yy2,
								                    ThickNess * 0.99, 0)) != -1)
								{
									if (res2 == 0)
									{
										xx1 = x11;
										yy1 = y11;
										xx2 = x22;
										yy2 = y22;
									}
									else
									{
										Length = CalcLengthLine(x11, y11, x22, y22);
										NewLength = CalcLengthLine(xx1, yy1, xx2, yy2);

										if ((Length > 2e5) && (NewLength < 1e5))
										{
											NrErrors++;
#ifdef _DEBUG

											if (0)
											{
												PolygonToObjectLines(AreaFillPolygon, INFO_LAYER, ChangedPolygon,
												                     INFO_LAYER2, 0);
											}

#endif

											if (NrErrorObjects + 2 >= MaxNrErrorObjects)
											{
												if (AllocateMemErrorObjects(MaxNrErrorObjects + 128) != 0)
													return -4;
											}

											memset(&NewObject, 0, sizeof(ObjectRecord));
											NewObject.ObjectType = AREAFILL;
											NewObject.TraceNr = cnt;
											NewObject.Layer = AreaFill->Layer;
											ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
											memmove(ErrorObject, &NewObject, sizeof(ObjectRecord));
											ErrorObject->Test = 0;
											ErrorObject->Info2 = 0;
											NrErrorObjects++;

											ErrorObject = &((*ErrorObjects)[NrErrorObjects]);
											memset(ErrorObject, 0, sizeof(ObjectRecord));
											ErrorObject->ObjectType = OBJECT_LINE;
											ErrorObject->x1 = x11;
											ErrorObject->y1 = y11;
											ErrorObject->x2 = x22;
											ErrorObject->y2 = y22;
											ErrorObject->Thickness = ThickNess;
											ErrorObject->Test = 0;
											ErrorObject->Info2 = 2;
											NrErrorObjects++;
										}
									}
								}
							}
						}

						PolygonPos += MemSizePolygon(AreaFillPolygon);
						AreaFillPolygon = (PolygonRecord *) PolygonPos;
					}
				}
			}
		}

// **************************************************************************************
	}

	strcpy(InfoStr, CopyInfoStr);
	RedrawInfoStr(1);

	AllocateSpecialMem(MEM_POINTS, 256 * 1024, (void **) &ErrorNrs);
	OkToDrawErrors = 1;
	CurrentErrorNr = -1;
	OtherErrors = 0;
	FoundAreafillError = 0;

	if (NrErrors > 0)
	{
		if (NrErrorObjects / 2 < 256 * 1024)
		{
			memset(ErrorNrs, 0, sizeof(256 * 1024));

			for (cnt = 0; cnt < NrErrorObjects / 2; cnt++)
			{
				Object1 = &((*ErrorObjects)[cnt * 2]);
				Object2 = &((*ErrorObjects)[cnt * 2 + 1]);

				if (((Object1->ObjectType == AREAFILL) && (Object2->Info2 == 1))
				        || ((Object2->ObjectType == AREAFILL) && (Object1->Info2 == 1)))
				{
					ErrorNrs[cnt] = 1;
					FoundAreafillError = 1;
				}
				else
					OtherErrors = 1;
			}

			if (FoundAreafillError)
			{
				cnt2 = 0;
				cnt = NrErrorObjects / 2 - 1;

				if (cnt > 0)
				{
					while (cnt > cnt2)
					{
						if (ErrorNrs[cnt] == 0)
						{
							Stop = 0;

							while ((cnt2 < cnt) && (!Stop))
							{
								if (ErrorNrs[cnt2] == 1)
									Stop = 1;
								else
									cnt2++;
							}

							if (cnt2 < cnt)
							{
								XchangeMem((uint8 *) & ((*ErrorObjects)[cnt2 * 2]),
								           (uint8 *) & ((*ErrorObjects)[cnt * 2]), 2 * sizeof(ObjectRecord));
								ErrorNrs[cnt2] = 0;
								ErrorNrs[cnt] = 1;
								cnt2++;
							}
						}

						cnt--;
					}
				}
			}
		}

//    RePaint();
		CheckInputMessages(0);
		DeAllocateMemObjects4();

		if (FoundAreafillError)
		{
			if (OtherErrors)
			{
				sprintf(str, SC(1029, "%i error(s)/warning(s) are found"), NrErrors);
				MessageBoxOwn(PCBWindow, str, SC(1030, "Error/warning"), MB_APPLMODAL | MB_OK);
			}
			else
			{
				sprintf(str, SC(1031, "%i warning(s) are found"), NrErrors);
				MessageBoxOwn(PCBWindow, str, SC(118, "Warning"), MB_APPLMODAL | MB_OK);
			}
		}
		else
		{
			sprintf(str, SC(1032, "%i error(s) are found"), NrErrors);
			MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		}

		ViewDesignRuleError(0);
	}
	else
		MessageBoxOwn(PCBWindow, SC(1033, "No design rule errors"), SC(1, "Message"), MB_APPLMODAL | MB_OK);

	DeAllocateMemTemp();
	return 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
