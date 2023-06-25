/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select3.c
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
#include "select.h"
#include "select3.h"
#include "calcdef.h"
#include "graphics.h"
#include "nets.h"
#include "calcrect.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "calcdiag.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "line2.h"
#include "pcb.h"
#include "math.h"
#include "string.h"
#include "mainloop.h"
#include "trace3.h"
#include "dialogs.h"
#include "stdio.h"
#include "import.h"



int32 FirstSchematicSelect, ok;

extern uint32 TimerObject, ClipID2;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNetObjectsFromObjects4(int32 NetNr)
{
	int32 cnt;
	ObjectRecord *Object, *ObjectNet;

	NrObjects3 = 0;

	for (cnt = 0; cnt < NrObjects4; cnt++)
	{
		Object = &((*Objects4)[cnt]);

		if (Object->NetNr == NetNr)
		{
			ObjectNet = &((*Objects3)[NrObjects3]);
			memmove(ObjectNet, Object, sizeof(ObjectRecord));
			NrObjects3++;
		}
	}

	return NrObjects3;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyTracesFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
/*

  mode :

  bit 0  : Only count the objects
  bit 1  : Search for OBJECT_SELECTED
  bit 2  : Add clearance for searching

*/
{
	int32 cnt, NrFound, start, end, Layer, TraceInfo, CompareMask, CompareValue;
	double x1, y1, x2, y2, dikte, lengte;

	TraceRecord *Trace;
	ObjectRecord *Object4;

	if ((mode & 1) == 0)
	{
		if (MaxNrObjects4 < 1024)
			AllocateMemObjects4(1024);
	}

	NrFound = 0;
	start = 0;
	end = 32;

	if ((SelectedLayer != -1) && (SelectedLayer >= 0) && (SelectedLayer < 32))
	{
		start = SelectedLayer;
		end = SelectedLayer + 1;
	}

	CompareValue = 0;

	if ((mode & 2) == 0)
		CompareMask = OBJECT_NOT_VISIBLE;
	else
	{
		CompareMask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
//    CompareValue=OBJECT_SELECTED;
	}

	for (Layer = start; Layer < end; Layer++)
	{

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
#ifdef _DEBUG

			if (Layer == 1)
			{
				if (cnt == 24)
					ok = 1;

				ok = 1;
			}

#endif
			TraceInfo = Trace->Info;

			if ((TraceInfo & CompareMask) == CompareValue)
			{
				x1 = Trace->X;
				x2 = x1;
				y1 = Trace->Y;
				lengte = Trace->Length;
				y2 = y1 + lengte;
				dikte = Trace->ThickNess;

				if ((mode & 4) == 4)
					dikte += Trace->Clearance * 2;

				if ((RectTestRect2(x1, y1 + (lengte / 2), dikte, lengte)) || (RectTestCircle(x1, y1, dikte, 255))
				        || (RectTestCircle(x2, y2, dikte, 255)))
				{
					if ((mode & 1) == 0)
					{
						if (NrObjects4 < MaxNrObjects4)
						{
							Object4 = &((*Objects4)[NrObjects4]);
							Object4->x1 = Trace->X;
							Object4->y1 = Trace->Y;
							Object4->x2 = lengte;
							Object4->y2 = Trace->ThickNess;
							Object4->Clearance = Trace->Clearance;
							Object4->ObjectType = TRACE_VER;
							Object4->TraceNr = cnt;
							Object4->Layer = Layer;
							Object4->NetNr = Trace->NetNr;

							if (NrObjects4 >= MaxNrObjects4 - 1)
							{
								if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
									NrObjects4++;
							}
							else
								NrObjects4++;
						}
					}

					NrFound++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & CompareMask) == CompareValue)
			{
				x1 = Trace->X;
				y1 = Trace->Y;
				lengte = Trace->Length;
				x2 = x1 + lengte;
				y2 = y1;
				dikte = Trace->ThickNess;

				if ((mode & 4) == 4)
					dikte += Trace->Clearance * 2;

				if ((RectTestRect2(x1 + (lengte / 2), y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
				        || (RectTestCircle(x2, y2, dikte, 255)))
				{
					if ((mode & 1) == 0)
					{
						if (NrObjects4 < MaxNrObjects4)
						{
							Object4 = &((*Objects4)[NrObjects4]);
							Object4->x1 = Trace->X;
							Object4->y1 = Trace->Y;
							Object4->x2 = lengte;
							Object4->y2 = Trace->ThickNess;
							Object4->Clearance = Trace->Clearance;
							Object4->ObjectType = TRACE_HOR;
							Object4->TraceNr = cnt;
							Object4->Layer = Layer;
							Object4->NetNr = Trace->NetNr;

							if (NrObjects4 >= MaxNrObjects4 - 1)
							{
								if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
									NrObjects4++;
							}
							else
								NrObjects4++;
						}
					}

					NrFound++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & CompareMask) == CompareValue)
			{
				x1 = Trace->X;
				y1 = Trace->Y;
				lengte = Trace->Length;
				x2 = x1 + lengte;
				y2 = y1 - lengte;
				dikte = Trace->ThickNess;

				if ((mode & 4) == 4)
					dikte += Trace->Clearance * 2;

				if ((RectTestDiag1(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
				        || (RectTestCircle(x2, y2, dikte, 255)))
				{
					if ((mode & 1) == 0)
					{
						if (NrObjects4 < MaxNrObjects4)
						{
							Object4 = &((*Objects4)[NrObjects4]);
							Object4->x1 = Trace->X;
							Object4->y1 = Trace->Y;
							Object4->x2 = lengte;
							Object4->y2 = Trace->ThickNess;
							Object4->Clearance = Trace->Clearance;
							Object4->ObjectType = TRACE_DIAG1;
							Object4->TraceNr = cnt;
							Object4->Layer = Layer;
							Object4->NetNr = Trace->NetNr;

							if (NrObjects4 >= MaxNrObjects4 - 1)
							{
								if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
									NrObjects4++;
							}
							else
								NrObjects4++;
						}
					}

					NrFound++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & CompareMask) == CompareValue)
			{
				x1 = Trace->X;
				y1 = Trace->Y;
				lengte = Trace->Length;
				x2 = x1 + lengte;
				y2 = y1 + lengte;
				dikte = Trace->ThickNess;

				if ((mode & 4) == 4)
					dikte += Trace->Clearance * 2;

				if ((RectTestDiag2(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
				        || (RectTestCircle(x2, y2, dikte, 255)))
				{
					if ((mode & 1) == 0)
					{
						if (NrObjects4 < MaxNrObjects4)
						{
							Object4 = &((*Objects4)[NrObjects4]);
							Object4->x1 = Trace->X;
							Object4->y1 = Trace->Y;
							Object4->x2 = lengte;
							Object4->y2 = Trace->ThickNess;
							Object4->Clearance = Trace->Clearance;
							Object4->ObjectType = TRACE_DIAG2;
							Object4->TraceNr = cnt;
							Object4->Layer = Layer;
							Object4->NetNr = Trace->NetNr;

							if (NrObjects4 >= MaxNrObjects4 - 1)
							{
								if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
									NrObjects4++;
							}
							else
								NrObjects4++;
						}
					}

					NrFound++;
				}
			}
		}
	}

	return NrFound;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyViasFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	/*

	  mode :

	  bit 0  : Only count the objects
	  bit 1  : Search for OBJECT_SELECTED
	  bit 2  : Add clearance for searching

	*/

	int32 cnt, NrFound, ViaInfo, CompareMask, CompareValue;
	double x1, y1, ThickNess, Drill;
	ViaRecord *Via;
	ObjectRecord *Object4;
	int32 InnerLayer;

	InnerLayer = CheckIfInnerLayer(SelectedLayer);

	if ((mode & 1) == 0)
	{
		if (MaxNrObjects4 < 1024)
			AllocateMemObjects4(1024);
	}

	NrFound = 0;
	CompareValue = 0;

	if ((mode & 2) == 0)
		CompareMask = OBJECT_NOT_VISIBLE;
	else
	{
		CompareMask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
//    CompareValue=OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & CompareMask) == CompareValue)
		{
			x1 = Via->X;
			y1 = Via->Y;
			ThickNess = Via->ThickNess;

			if (InnerLayer)
			{
				if (Via->ThermalInner != 0)
					ThickNess = Via->ThermalInner;
			}

			if ((mode & 4) == 4)
				ThickNess += Via->Clearance * 2;

			Drill = Via->DrillThickNess;

			if (RectTestCircle(x1, y1, ThickNess, 255))
			{
				if ((mode & 1) == 0)
				{
					if (NrObjects4 < MaxNrObjects4)
					{
						Object4 = &((*Objects4)[NrObjects4]);
						Object4->x1 = Via->X;
						Object4->y1 = Via->Y;
						Object4->x2 = Via->ThickNess;
						Object4->y2 = Drill;
						Object4->Clearance = Via->Clearance;
						Object4->NetNr = Via->NetNr;
						Object4->ObjectType = VIA_PUT_THROUGH_ROUND;
						Object4->TraceNr = cnt;
						Object4->Layer = -1;

						if (NrObjects4 >= MaxNrObjects4 - 1)
						{
							if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
								NrObjects4++;
						}
						else
							NrObjects4++;
					}
				}

				NrFound++;
			}
		}
	}

	return NrFound;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyAreafillsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	/*

	  mode :

	  bit 0  : Only count the objects
	  bit 1  : Search for OBJECT_SELECTED
	  bit 2  : Add clearance for searching

	*/

	int32 cnt, NrFound, count, CompareMask, CompareValue, InnerLayer;
	AreaFillRecord *AreaFill;
	ObjectRecord *Object4;
	uint8 *AreaPos;
	PolygonRecord *DrawPolygon;

	InnerLayer = CheckIfInnerLayer(SelectedLayer);

	if ((mode & 1) == 0)
	{
		if (MaxNrObjects4 < 1024)
			AllocateMemObjects4(1024);
	}

	NrFound = 0;
	CompareValue = 0;

	if ((mode & 2) == 0)
		CompareMask = OBJECT_NOT_VISIBLE;
	else
		CompareMask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (CompareMask)) == CompareValue)
		        && ((SelectedLayer == -1) || (AreaFill->Layer == SelectedLayer)))
		{
			AreaPos = (uint8 *) AreaFill;
			count = sizeof(AreaFillRecord);
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

			if ((SearchMaxX > AreaFill->minx - 100000.0) && (SearchMinX < AreaFill->maxx + 100000.0)
			        && (SearchMaxY > AreaFill->miny - 100000.0) && (SearchMinY < AreaFill->maxy + 100000.0))
			{
				if ((mode & 1) == 0)
				{
					if (NrObjects4 < MaxNrObjects4)
					{
						Object4 = &((*Objects4)[NrObjects4]);
						Object4->x1 = 0.0;
						Object4->y1 = 0.0;
						Object4->x2 = 0.0;
						Object4->y2 = 0.0;
						Object4->Clearance = AreaFill->Clearance;
						Object4->NetNr = AreaFill->NetNr;
						Object4->ObjectType = AREAFILL2;
						Object4->TraceNr = cnt;
						Object4->Layer = AreaFill->Layer;
						Object4->Test = AreaFill->NrVerticesStartPolygon;

						if (NrObjects4 >= MaxNrObjects4 - 1)
						{
							if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
								NrObjects4++;
						}
						else
							NrObjects4++;
					}
				}

				NrFound++;
			}
		}
	}

	return NrFound;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyConnectionsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	int32 cnt, NrFound, ConnectionInfo, TestResult, CompareMask, CompareValue;
	double x1, y1, x2, y2;

	ConnectionsRecord *Connection;
	ObjectRecord *Object4;

	if ((mode & 1) == 0)
	{
		if (MaxNrObjects4 < 1024)
			AllocateMemObjects4(1024);
	}

	CompareValue = 0;

	if ((mode & 2) == 0)
		CompareMask = OBJECT_NOT_VISIBLE;
	else
	{
		CompareMask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
//    CompareValue=OBJECT_SELECTED;
//    CompareMask=OBJECT_NOT_VISIBLE;
	}

	NrFound = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & CompareMask) == CompareValue)
		{
			x1 = Connection->x1;
			y1 = Connection->y1;
			x2 = Connection->x2;
			y2 = Connection->y2;

			if ((TestResult = RectTestLine2(x1, y1, x2, y2)) != 0)
			{
				if ((mode & 1) == 0)
				{
					if (NrObjects4 < MaxNrObjects4)
					{
						Object4 = &((*Objects4)[NrObjects4]);
						Object4->x1 = x1;
						Object4->y1 = y1;
						Object4->x2 = x2;
						Object4->y2 = y2;
						Object4->CompNr = Connection->Comp1Nr;
						Object4->CompNr2 = Connection->Comp2Nr;
						Object4->NetNr = Connection->NetNr;
						Object4->ObjectType = CONNECTION;
						Object4->Layer = Connection->Layer;
//            Object4->Info=Connection->Info;
						Object4->TraceNr = cnt;

						if (NrObjects4 >= MaxNrObjects4 - 1)
						{
							if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
								NrObjects4++;
						}
						else
							NrObjects4++;
					}
				}

				NrFound++;
			}
		}
	}

	return NrFound;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyCompObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	/*

	  mode :

	  bit 0  : Only count the objects
	  bit 1  : Search for OBJECT_SELECTED
	  bit 2  : Add clearance for searching
	  bit 4  : Do not add routing keepout

	*/
	int32 cnt, cnt2, NrFound, CompareMask, CompareValue, TestMode, TestRectangle, InnerLayer;
	double x1, y1, x2, y2;
	ObjectRecord *Object, *Object4;
	CompRecord *Comp;

	InnerLayer = CheckIfInnerLayer(SelectedLayer);

	if ((mode & 1) == 0)
	{
		if (MaxNrObjects4 < 1024)
			AllocateMemObjects4(1024);
	}

	TestMode = 0;

	if ((mode & 4) == 4)
		TestMode = 1;

	NrFound = 0;
	CompareValue = 0;

	if ((mode & 2) == 0)
		CompareMask = OBJECT_NOT_VISIBLE;
	else
	{
		CompareMask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
//    CompareValue=OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & CompareMask) == CompareValue)
		{
			x1 = Comp->BoardPosMinX;
			y1 = Comp->BoardPosMinY;
			x2 = Comp->BoardPosMaxX;
			y2 = Comp->BoardPosMaxY;
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "H1") == 0)
			{
				ok = 1;

				if (SelectedLayer == 3)
					ok = 1;
			}

#endif

			if ((SearchMaxX > x1) && (SearchMinX < x2) && (SearchMaxY > y1) && (SearchMinY < y2))
			{
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "C11") == 0)
				{
					ok = 1;

					if (SelectedLayer == 3)
						ok = 1;
				}

#endif
// *******************************************************************************************************
// *******************************************************************************************************
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);	// Inner pads in Object->x3

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
					Object->CompNr = cnt;
//          FillPositionObject(Object);
#ifdef _DEBUG

					if ((InRange9(Object->x1, 180.3e5)) && (InRange9(Object->y1, 58.4e5)))
						ok = 1;

#endif

					if ((SelectedLayer == -1) || (Object->Layer == -1) || (Object->Layer == SelectedLayer))
					{
						TestRectangle = 1;

						if ((mode & 32) == 32)
						{
							if (CheckObjectIsBigPolygon(Object))
								TestRectangle = 0;
						}

						if ((TestRectangle) && (RectTestObject(Object, TestMode)))
						{
							if ((mode & 1) == 0)
							{
								if (NrObjects4 < MaxNrObjects4)
								{
									Object4 = &((*Objects4)[NrObjects4]);

									if (InnerLayer)
									{
										switch (Object->ObjectType)
										{
										case PIN_PUT_THROUGH_ROUND:
										case PIN_PUT_THROUGH_SQUARE:
										case PIN_PUT_THROUGH_POLYGON:
											Object->ObjectType = PIN_PUT_THROUGH_ROUND;

											if (Object->x3 != 0.0)
												Object->x2 = Object->x3;

											break;
										}
									}

									memmove(Object4, Object, sizeof(ObjectRecord));

									if (NrObjects4 >= MaxNrObjects4 - 1)
									{
										if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
											NrObjects4++;
									}
									else
										NrObjects4++;
								}
							}

							NrFound++;
						}
					}
				}

// *******************************************************************************************************
// *******************************************************************************************************
				if ((mode & 16) == 0)
				{
					NrObjects = 0;
					ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 4);	// Routing keepout

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
						Object->Layer -= ROUTING_KEEPOUT_LAYER;
						Object->NetNr = -2;
						Object->CompNr = cnt;

						switch (Object->ObjectType)
						{
						case OBJECT_ARC:
							Object->ObjectType = PIN_SMD_ROUND;
							break;

						case OBJECT_RECT:
							Object->ObjectType = PIN_SMD_RECT;
							break;
						}

//            FillPositionObject(Object);

#ifdef _DEBUG

						if ((InRange9(Object->x1, 180.3e5)) && (InRange9(Object->y1, 58.4e5)))
							ok = 1;

#endif

						if ((SelectedLayer == -1) || (Object->Layer == SelectedLayer))
						{
							if (RectTestObject(Object, 0))
							{
								if ((mode & 1) == 0)
								{
									if (NrObjects4 < MaxNrObjects4)
									{
										Object4 = &((*Objects4)[NrObjects4]);
										memmove(Object4, Object, sizeof(ObjectRecord));

										if (NrObjects4 >= MaxNrObjects4 - 1)
										{
											if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
												NrObjects4++;
										}
										else
											NrObjects4++;
									}
								}

								NrFound++;
							}
						}
					}
				}
			}
		}
	}

	return NrFound;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 CopyCompBigPolygonObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	/*

	  mode :

	  bit 0  : Only count the objects
	  bit 1  : Search for OBJECT_SELECTED
	  bit 2  : Add clearance for searching
	  bit 4  : Do not add routing keepout

	*/
	int32 cnt, cnt2, NrFound, CompareMask, CompareValue, InnerLayer;
	double x1, y1, x2, y2;
	ObjectRecord *Object, *Object4;
	CompRecord *Comp;

	InnerLayer = CheckIfInnerLayer(SelectedLayer);

	if ((mode & 1) == 0)
	{
		if (MaxNrObjects4 < 1024)
			AllocateMemObjects4(1024);
	}

	NrFound = 0;
	CompareValue = 0;

	if ((mode & 2) == 0)
		CompareMask = OBJECT_NOT_VISIBLE;
	else
	{
		CompareMask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
//    CompareValue=OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & CompareMask) == CompareValue)
		{
			x1 = Comp->BoardPosMinX;
			y1 = Comp->BoardPosMinY;
			x2 = Comp->BoardPosMaxX;
			y2 = Comp->BoardPosMaxY;
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "H1") == 0)
			{
				int32 ok = 1;

				if (SelectedLayer == 3)
					ok = 1;
			}

#endif

			if ((SearchMaxX > x1) && (SearchMinX < x2) && (SearchMaxY > y1) && (SearchMinY < y2))
			{
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "C11") == 0)
				{
					ok = 1;

					if (SelectedLayer == 3)
						ok = 1;
				}

#endif
// *******************************************************************************************************
// *******************************************************************************************************
				NrObjects = 0;
				ShapePinsToObject(Comp, (float) 0.0, (float) 0.0, 0, 0, 0, 1);	// Inner pads in Object->x3

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
//          FillPositionObject(Object);
#ifdef _DEBUG

					if ((InRange9(Object->x1, 180.3e5)) && (InRange9(Object->y1, 58.4e5)))
						ok = 1;

#endif

					if ((SelectedLayer == -1) || (Object->Layer == -1) || (Object->Layer == SelectedLayer))
					{
						if (CheckObjectIsBigPolygon(Object))
						{
							if ((((mode & 4) == 0) && (RectTestObject(Object, 0)))
							        || (((mode & 4) == 4) && (RectTestObject(Object, 1))))
							{
								if ((mode & 1) == 0)
								{
									if (NrObjects4 < MaxNrObjects4)
									{
										Object4 = &((*Objects4)[NrObjects4]);
										memmove(Object4, Object, sizeof(ObjectRecord));

										if (NrObjects4 >= MaxNrObjects4 - 1)
										{
											if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
												NrObjects4++;
										}
										else
											NrObjects4++;
									}
								}

								NrFound++;
							}
						}
					}
				}
			}
		}
	}

	return NrFound;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyOtherObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	/*

	  mode :

	  bit 0  : Only count the objects
	  bit 1  : Search for OBJECT_SELECTED
	  bit 2  : Add clearance for searching
	  bit 4  : Do not add routing keepout

	*/
	int32 cnt, cnt2, cnt4, CompareMask, NrFound, CompareValue, LineSegments, SegmentCount, MaxCountX, FontNr, Mirror,
	      Layer2, NrLines;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectRecord *Object4;
	ObjectPolygonRecord *ObjectPolygon;
	double x1, y1, x2, y2, x22, y22, x3, y3, x4, y4, Rotation, Xmax, Xmin, Ymax, Ymin, LineBuf[4096];


	if ((mode & 1) == 0)
	{
		if (MaxNrObjects4 < 1024)
			AllocateMemObjects4(1024);
	}


	NrFound = 0;
	Layer2 = SelectedLayer;
	CompareValue = 0;

	if ((mode & 2) == 0)
		CompareMask = OBJECT_NOT_VISIBLE;
	else
	{
		CompareMask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
//    CompareValue=OBJECT_SELECTED;
	}

	/*
	  switch (SelectedLayer) {
	    case DRILL_LAYER:
	    case DRILL_UNPLATED_LAYER:
	      Layer2=-1;
	      break;
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & CompareMask) == CompareValue) && (CheckIfCopperLayer(ObjectLine->Layer)))
		{
			if ((SelectedLayer == -1) || (ObjectLine->Layer == SelectedLayer))
			{
				x1 = ObjectLine->X1;
				y1 = ObjectLine->Y1;
				x2 = ObjectLine->X2;
				y2 = ObjectLine->Y2;
				x22 = ObjectLine->LineThickNess * 0.5;

				if ((mode & 4) == 4)
				{
					if (ObjectLine->Clearance != 0)
						x22 += ObjectLine->Clearance;
					else
						x22 += Design.StandardClearance;
				}

				if ((min(x1, x2) - x22 <= SearchMaxX) && (max(x1, x2) + x22 >= SearchMinX)
				        && (min(y1, y2) - x22 <= SearchMaxY) && (max(y1, y2) + x22 >= SearchMinY))
				{
					if ((mode & 1) == 0)
					{
						if (NrObjects4 < MaxNrObjects4)
						{
							Object4 = &((*Objects4)[NrObjects4]);
							Object4->x1 = x1;
							Object4->y1 = y1;
							Object4->x2 = x2;
							Object4->y2 = y2;
							Object4->Thickness = ObjectLine->LineThickNess;
							Object4->Clearance = ObjectLine->Clearance;
							Object4->NetNr = ObjectLine->NetNr;
							Object4->ObjectType = TRACE_ALL_ANGLE;
							Object4->ObjectType2 = 0;
							Object4->TraceNr = cnt;
							Object4->Layer = ObjectLine->Layer;

							if (NrObjects4 >= MaxNrObjects4 - 1)
							{
								if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
									NrObjects4++;
							}
							else
								NrObjects4++;
						}
					}

					NrFound++;
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & CompareMask) == CompareValue) && (CheckIfCopperLayer(ObjectRect->Layer)))
		{
			if ((SelectedLayer == -1) || (ObjectRect->Layer == SelectedLayer)
			        || (ObjectRect->Layer == SelectedLayer + ROUTING_KEEPOUT_LAYER))
			{
				if (((mode & 16) == 0) || ((ObjectRect->Layer >= 0) && (ObjectRect->Layer < 32)))
				{
					x1 = ObjectRect->CentreX;
					y1 = ObjectRect->CentreY;
					x2 = ObjectRect->Width;
					y2 = ObjectRect->Height;

					if ((ObjectRect->Info & OBJECT_FILLED) == 0)
					{
						x22 = (x2 + ObjectRect->LineThickNess) * 0.5;
						y22 = (y2 + ObjectRect->LineThickNess) * 0.5;

						if ((mode & 4) == 4)
						{
							if (ObjectRect->Clearance != 0)
							{
								x22 += ObjectRect->Clearance;
								y22 += ObjectRect->Clearance;
							}
							else
							{
								x22 += Design.StandardClearance;
								y22 += Design.StandardClearance;
							}
						}

						x2 *= 0.5;
						y2 *= 0.5;

						if ((x1 - x22 <= SearchMaxX) && (x1 + x22 >= SearchMinX) && (y1 - y22 <= SearchMaxY)
						        && (y1 + y22 >= SearchMinY))
						{
							if ((mode & 1) == 0)
							{
								if (NrObjects4 < MaxNrObjects4)
								{
									Object4 = &((*Objects4)[NrObjects4]);
									Object4->x1 = x1 - x2;
									Object4->y1 = y1 - y2;
									Object4->x2 = x1 + x2;
									Object4->y2 = y1 - y2;
									Object4->Thickness = ObjectRect->LineThickNess;
									Object4->Clearance = ObjectRect->Clearance;
									Object4->NetNr = ObjectRect->NetNr;
									Object4->ObjectType = TRACE_ALL_ANGLE;
									Object4->ObjectType2 = 0;
									Object4->TraceNr = cnt;

									if (ObjectRect->Layer < 32)
										Object4->Layer = ObjectRect->Layer;
									else
									{
										Object4->Layer = ObjectRect->Layer - ROUTING_KEEPOUT_LAYER;
										Object4->NetNr = -2;
									}

									if (NrObjects4 >= MaxNrObjects4 - 1)
									{
										if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
											NrObjects4++;
									}
									else
										NrObjects4++;
								}

								if (NrObjects4 < MaxNrObjects4)
								{
									Object4 = &((*Objects4)[NrObjects4]);
									Object4->x1 = x1 + x2;
									Object4->y1 = y1 - y2;
									Object4->x2 = x1 + x2;
									Object4->y2 = y1 + y2;
									Object4->Thickness = ObjectRect->LineThickNess;
									Object4->Clearance = ObjectRect->Clearance;
									Object4->NetNr = ObjectRect->NetNr;
									Object4->ObjectType = TRACE_ALL_ANGLE;
									Object4->ObjectType2 = 0;
									Object4->TraceNr = cnt;

									if (ObjectRect->Layer < 32)
										Object4->Layer = ObjectRect->Layer;
									else
									{
										Object4->Layer = ObjectRect->Layer - ROUTING_KEEPOUT_LAYER;
										Object4->NetNr = -2;
									}

									if (NrObjects4 >= MaxNrObjects4 - 1)
									{
										if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
											NrObjects4++;
									}
									else
										NrObjects4++;
								}

								if (NrObjects4 < MaxNrObjects4)
								{
									Object4 = &((*Objects4)[NrObjects4]);
									Object4->x1 = x1 + x2;
									Object4->y1 = y1 + y2;
									Object4->x2 = x1 - x2;
									Object4->y2 = y1 + y2;
									Object4->Thickness = ObjectRect->LineThickNess;
									Object4->Clearance = ObjectRect->Clearance;
									Object4->ObjectType = TRACE_ALL_ANGLE;
									Object4->ObjectType2 = 0;
									Object4->TraceNr = cnt;

									if (ObjectRect->Layer < 32)
									{
										Object4->Layer = ObjectRect->Layer;
										Object4->NetNr = ObjectRect->NetNr;
									}
									else
									{
										Object4->Layer = ObjectRect->Layer - ROUTING_KEEPOUT_LAYER;
										Object4->NetNr = -2;
									}

									if (NrObjects4 >= MaxNrObjects4 - 1)
									{
										if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
											NrObjects4++;
									}
									else
										NrObjects4++;
								}

								if (NrObjects4 < MaxNrObjects4)
								{
									Object4 = &((*Objects4)[NrObjects4]);
									Object4->x1 = x1 - x2;
									Object4->y1 = y1 + y2;
									Object4->x2 = x1 - x2;
									Object4->y2 = y1 - y2;
									Object4->Thickness = ObjectRect->LineThickNess;
									Object4->Clearance = ObjectRect->Clearance;
									Object4->NetNr = ObjectRect->NetNr;
									Object4->ObjectType = TRACE_ALL_ANGLE;
									Object4->ObjectType2 = 0;
									Object4->TraceNr = cnt;

									if (ObjectRect->Layer < 32)
										Object4->Layer = ObjectRect->Layer;
									else
									{
										Object4->Layer = ObjectRect->Layer - ROUTING_KEEPOUT_LAYER;
										Object4->NetNr = -2;
									}

									if (NrObjects4 >= MaxNrObjects4 - 1)
									{
										if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
											NrObjects4++;
									}
									else
										NrObjects4++;
								}
							}

							NrFound += 4;
						}
					}
					else
					{
						// *******************************************************************************************************
						x22 = x2 * 0.5;
						y22 = y2 * 0.5;

						if ((mode & 4) == 4)
						{
							if (ObjectRect->Clearance != 0)
							{
								x22 += ObjectRect->Clearance;
								y22 += ObjectRect->Clearance;
							}
							else
							{
								x22 += Design.StandardClearance;
								y22 += Design.StandardClearance;
							}
						}

						if ((x1 - x22 <= SearchMaxX) && (x1 + x22 >= SearchMinX) && (y1 - y22 <= SearchMaxY)
						        && (y1 + y22 >= SearchMinY))
						{
							if ((mode & 1) == 0)
							{
								if (NrObjects4 < MaxNrObjects4)
								{
									Object4 = &((*Objects4)[NrObjects4]);
									Object4->x1 = x1;
									Object4->y1 = y1;
									Object4->x2 = x2;
									Object4->y2 = y2;
									Object4->Clearance = ObjectRect->Clearance;
									Object4->NetNr = ObjectRect->NetNr;
									Object4->ObjectType = PIN_SMD_RECT;
									Object4->ObjectType2 = 0;
									Object4->TraceNr = cnt;

									if (ObjectRect->Layer < 32)
										Object4->Layer = ObjectRect->Layer;
									else
									{
										Object4->Layer = ObjectRect->Layer - ROUTING_KEEPOUT_LAYER;
										Object4->NetNr = -2;
									}

									if (NrObjects4 >= MaxNrObjects4 - 1)
									{
										if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
											NrObjects4++;
									}
									else
										NrObjects4++;
								}
							}

							NrFound += 1;
						}
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & CompareMask) == CompareValue)
		{
			if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
			{
				x1 = ObjectArc->CentreX;
				y1 = ObjectArc->CentreY;
				x2 = ObjectArc->Width;
				y2 = ObjectArc->Height;
				x3 = ObjectArc->StartDiffX;
				y3 = ObjectArc->StartDiffY;
				x4 = ObjectArc->EndDiffX;
				y4 = ObjectArc->EndDiffY;
				x22 = x2 * 0.5;
				y22 = y2 * 0.5;

				if ((mode & 4) == 4)
				{
					if (ObjectArc->Clearance != 0)
						x22 += ObjectArc->Clearance;
					else
						x22 += Design.StandardClearance;
				}

				if ((x1 - x22 <= SearchMaxX) && (x1 + x22 >= SearchMinX) && (y1 - x22 <= SearchMaxY)
				        && (y1 + x22 >= SearchMinY))
				{
					if ((mode & 1) == 0)
					{
						if (NrObjects4 < MaxNrObjects4)
						{
							Object4 = &((*Objects4)[NrObjects4]);
							Object4->x1 = x1;
							Object4->y1 = y1;
							Object4->x2 = x2;
							Object4->y2 = 0.0;
							Object4->x3 = 0.0;
							Object4->y3 = 0.0;
							Object4->x4 = 0.0;
							Object4->y4 = 0.0;
							Object4->Thickness = 0.0;
							Object4->Clearance = ObjectArc->Clearance;
							Object4->NetNr = ObjectArc->NetNr;

							if (ObjectArc->Layer == DRILL_LAYER)
							{
								Object4->ObjectType = DRILL;
								Object4->Layer = -1;
							}
							else
							{
								Object4->ObjectType = DRILL_UNPLATED;
								Object4->Layer = SelectedLayer;
								Object4->NetNr = -1;
							}

							Object4->TraceNr = cnt;

							if (NrObjects4 >= MaxNrObjects4 - 1)
							{
								if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
									NrObjects4++;
							}
							else
								NrObjects4++;
						}
					}

					NrFound++;
				}
			}
			else
			{
				if (CheckIfCopperLayer(ObjectArc->Layer))
				{
					if ((SelectedLayer == -1) || (ObjectArc->Layer == SelectedLayer)
					        || (ObjectArc->Layer == SelectedLayer + ROUTING_KEEPOUT_LAYER))
					{
						if (((mode & 16) == 0) || ((ObjectArc->Layer >= 0) && (ObjectArc->Layer < 32)))
						{
							x1 = ObjectArc->CentreX;
							y1 = ObjectArc->CentreY;
							x2 = ObjectArc->Width;
							y2 = ObjectArc->Height;
							x3 = ObjectArc->StartDiffX;
							y3 = ObjectArc->StartDiffY;
							x4 = ObjectArc->EndDiffX;
							y4 = ObjectArc->EndDiffY;
							x22 = x2 * 0.5;
							y22 = y2 * 0.5;

							if ((ObjectArc->Info & OBJECT_FILLED) != 0)
							{
								if ((mode & 4) == 4)
								{
									if (ObjectArc->Clearance != 0)
										x22 += ObjectArc->Clearance;
									else
										x22 += Design.StandardClearance;
								}

								if ((x1 - x22 <= SearchMaxX) && (x1 + x22 >= SearchMinX) && (y1 - x22 <= SearchMaxY)
								        && (y1 + x22 >= SearchMinY))
								{
									if ((mode & 1) == 0)
									{
										if (NrObjects4 < MaxNrObjects4)
										{
											Object4 = &((*Objects4)[NrObjects4]);
											Object4->x1 = x1;
											Object4->y1 = y1;
											Object4->x2 = x2;
											Object4->y2 = x2;
											Object4->x3 = 0.0;
											Object4->y3 = 0.0;
											Object4->x4 = 0.0;
											Object4->y4 = 0.0;
											Object4->Thickness = 0.0;
											Object4->Clearance = ObjectArc->Clearance;
											Object4->NetNr = ObjectArc->NetNr;
											Object4->ObjectType = PIN_SMD_ROUND;
											Object4->TraceNr = cnt;

											if (ObjectArc->Layer < 32)
												Object4->Layer = ObjectArc->Layer;
											else
											{
												Object4->Layer = ObjectArc->Layer - ROUTING_KEEPOUT_LAYER;
												Object4->NetNr = -2;
											}

											if (NrObjects4 >= MaxNrObjects4 - 1)
											{
												if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
													NrObjects4++;
											}
											else
												NrObjects4++;
										}
									}

									NrFound++;
								}
							}
							else
							{
								x22 += ObjectArc->LineThickNess;
								y22 += ObjectArc->LineThickNess;
								x22 *= 0.5;
								y22 *= 0.5;

								if ((mode & 4) == 4)
								{
									if (ObjectArc->Clearance != 0)
									{
										x22 += ObjectArc->Clearance;
										y22 += ObjectArc->Clearance;
									}
									else
									{
										x22 += Design.StandardClearance;
										y22 += Design.StandardClearance;
									}
								}

								if ((x1 - x22 <= SearchMaxX) && (x1 + x22 >= SearchMinX) && (y1 - y22 <= SearchMaxY)
								        && (y1 + y22 >= SearchMinY))
								{
									if ((mode & 1) == 0)
									{
										if (NrObjects4 < MaxNrObjects4)
										{
											Object4 = &((*Objects4)[NrObjects4]);
											Object4->x1 = x1;
											Object4->y1 = y1;
											Object4->x2 = x2;
											Object4->y2 = y2;
											Object4->x3 = x3;
											Object4->y3 = y3;
											Object4->x4 = x4;
											Object4->y4 = y4;
											Object4->Thickness = ObjectArc->LineThickNess;
											Object4->Clearance = ObjectArc->Clearance;
											Object4->NetNr = ObjectArc->NetNr;
											Object4->ObjectType = TRACE_ARC;
											Object4->TraceNr = cnt;
											Object4->Layer = Layer2;

											if (NrObjects4 >= MaxNrObjects4 - 1)
											{
												if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
													NrObjects4++;
											}
											else
												NrObjects4++;
										}
									}

									NrFound++;
								}
							}
						}
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & CompareMask) == CompareValue) && (CheckIfCopperLayer(ObjectText2->Layer)))
		{
			if ((SelectedLayer == -1) || (ObjectText2->Layer == SelectedLayer))
			{
				x1 = ObjectText2->X;
				y1 = ObjectText2->Y;
				x2 = ObjectText2->FontHeight;
				Rotation = ObjectText2->Rotation;
				FontNr = ObjectText2->FontNr;
				Mirror = (ObjectText2->TextMode & 0x10) >> 4;

				if ((NrLines =
				            ConvertObjectTextToStrings(ObjectText2->Text, FontNr, &MaxCountX, ObjectText2->Layer)) > 0)
				{
// *******************************************************************************************************
					if (FontNr == 0)
					{
						for (cnt2 = 0; cnt2 < NrLines; cnt2++)
						{
							LineSegments =
							    TextStringToLineSegments2(x1, y1, x2, Rotation, 0, Mirror, TextStrings2[NrLines],
							                              (double *) &LineBuf);

							if ((mode & 1) == 0)
							{
								SegmentCount = 0;

								for (cnt4 = 0; cnt4 < LineSegments; cnt4++)
								{
									if (NrObjects4 < MaxNrObjects4)
									{
										Object4 = &((*Objects4)[NrObjects4]);
										Object4->x1 = LineBuf[SegmentCount];
										SegmentCount++;
										Object4->y1 = LineBuf[SegmentCount];
										SegmentCount++;
										Object4->x2 = LineBuf[SegmentCount];
										SegmentCount++;
										Object4->y2 = LineBuf[SegmentCount];
										SegmentCount++;
										Object4->Thickness = ObjectText2->LineThickNess;
										Object4->Clearance = Design.StandardClearance * 1.05;
										Object4->NetNr = -1;
										Object4->ObjectType = OBJECT_LINE;
										Object4->TraceNr = cnt;
										Object4->Layer = ObjectText2->Layer;

										if (NrObjects4 >= MaxNrObjects4 - 1)
										{
											if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
												NrObjects4++;
										}
										else
											NrObjects4++;
									}
								}
							}

							NrFound += LineSegments;

							if (Mirror == 0)
								x1 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
							else
								x1 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;

							y1 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
						}

// *******************************************************************************************************
					}
					else
					{
						x4 = x1;
						y4 = y1;
						Xmin = 1e9;
						Xmax = -1e9;
						Ymin = 1e9;
						Ymax = -1e9;

						for (cnt2 = 0; cnt2 < NrLines; cnt2++)
						{
							if (FontNr == 0)
								GetMinMaxText2(x4, y4, x2, FontNr, Rotation, 0, Mirror, TextStrings2[cnt2]);
							else
								GetMinMaxText2b(x4, y4, x2, FontNr, Rotation, 0, Mirror, TextStrings[cnt2]);

							Xmin = min(Xmin, TextMinX);
							Ymin = min(Ymin, TextMinY);
							Xmax = max(Xmax, TextMaxX);
							Ymax = max(Ymax, TextMaxY);

							if (FontNr == 0)
							{
								if (Mirror == 0)
									x4 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
								else
									x4 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;

								y4 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
							}
							else
							{
								if (Mirror == 0)
									x4 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
								else
									x4 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;

								y4 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
							}
						}

						if ((Xmax >= SearchMinX) && (Xmin <= SearchMaxX) && (Ymax >= SearchMinY)
						        && (Ymin <= SearchMaxY))
						{
							if ((InRangeSpecial(Rotation, 0.0, 0.1)) || (InRangeSpecial(Rotation, 90.0, 0.1))
							        || (InRangeSpecial(Rotation, 180.0, 0.1)) || (InRangeSpecial(Rotation, 270.0, 0.1)))
							{
								if ((mode & 1) == 0)
								{
									if (NrObjects4 < MaxNrObjects4)
									{
										Object4 = &((*Objects4)[NrObjects4]);
										Object4->x1 = (Xmin + Xmax) * 0.5;
										Object4->y1 = (Ymin + Ymax) * 0.5;
										Object4->x2 = Xmax - Xmin;
										Object4->y2 = Ymax - Ymin;
										Object4->x3 = 0.0;
										Object4->y3 = 0.0;
										Object4->x4 = 0.0;
										Object4->y4 = 0.0;
										Object4->Thickness = 0.0;
										Object4->Clearance = 0.0;
										Object4->NetNr = ObjectText2->NetNr;
										Object4->ObjectType = PIN_SMD_RECT;
										Object4->ObjectType2 = OBJECT_TEXT2;
										Object4->TraceNr = cnt;
										Object4->Layer = Layer2;

										if (NrObjects4 >= MaxNrObjects4 - 1)
										{
											if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
												NrObjects4++;
										}
										else
											NrObjects4++;
									}
								}

								NrFound++;
							}
							else
							{
								for (cnt2 = 0; cnt2 < NrLines; cnt2++)
								{
									if ((mode & 1) == 0)
									{
										if (NrObjects4 < MaxNrObjects4)
										{
											if (FontNr == 0)
												GetMinMaxText2(x1, y1, x2, 0, 0.0, 0, Mirror, TextStrings2[cnt2]);
											else
												GetMinMaxText2b(x1, y1, x2, FontNr, 0.0, 0, Mirror, TextStrings[cnt2]);

#ifdef _DEBUG

											if (FontNr != 0)
												ok = 1;

#endif
											Object4 = &((*Objects4)[NrObjects4]);
											y2 = TextMaxY - TextMinY;
											x3 = TextMinX + y2 * 0.5;
											y3 = (TextMaxY + TextMinY) * 0.5;
											x4 = TextMaxX - y2 * 0.5;
											y4 = (TextMaxY + TextMinY) * 0.5;

											if (Mirror == 0)
												RotatePointFromOtherPoint2(&x3, &y3, x1, y1, Rotation);
											else
												RotatePointFromOtherPoint2(&x3, &y3, x1, y1, -Rotation);

											if (Mirror == 0)
												RotatePointFromOtherPoint2(&x4, &y4, x1, y1, Rotation);
											else
												RotatePointFromOtherPoint2(&x4, &y4, x1, y1, -Rotation);

											Object4->x1 = x3;
											Object4->y1 = y3;
											Object4->x2 = x4;
											Object4->y2 = y4;
											Object4->x3 = 0.0;
											Object4->y3 = 0.0;
											Object4->x4 = 0.0;
											Object4->y4 = 0.0;
											Object4->Thickness = ObjectText2->LineThickNess;
											Object4->Clearance = 0.0;
											Object4->NetNr = ObjectText2->NetNr;
											Object4->ObjectType = TRACE_ALL_ANGLE;
											Object4->ObjectType2 = OBJECT_TEXT2;
											Object4->TraceNr = cnt;
											Object4->Layer = Layer2;

											if (NrObjects4 >= MaxNrObjects4 - 1)
											{
												if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
													NrObjects4++;
											}
											else
												NrObjects4++;
										}
									}

									NrFound++;

									if (FontNr == 0)
									{
										if (Mirror == 0)
											x1 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
										else
											x1 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;

										y1 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
									}
									else
									{
										if (Mirror == 0)
											x1 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
										else
											x1 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;

										y1 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
									}
								}
							}
						}
					}
				}
			}
		}
	}


// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & CompareMask) == CompareValue) && (CheckIfCopperLayer(ObjectPolygon->Layer)))
		{
			if ((SelectedLayer == -1) || (ObjectPolygon->Layer == SelectedLayer)
			        || (ObjectPolygon->Layer == SelectedLayer + ROUTING_KEEPOUT_LAYER))
			{
				if (((mode & 16) == 0) || ((ObjectPolygon->Layer >= 0) && (ObjectPolygon->Layer < 32)))
				{
					if ((ObjectPolygon->maxx >= SearchMinX) && (ObjectPolygon->minx <= SearchMaxX)
					        && (ObjectPolygon->maxy >= SearchMinY) && (ObjectPolygon->miny <= SearchMaxY))
					{
						if ((mode & 1) == 0)
						{
							Object4 = &((*Objects4)[NrObjects4]);
							Object4->NetNr = ObjectPolygon->NetNr;

							if (ObjectPolygon->Layer < 32)
								Object4->Layer = ObjectPolygon->Layer;
							else
							{
								Object4->Layer = ObjectPolygon->Layer - ROUTING_KEEPOUT_LAYER;
								Object4->NetNr = -2;
							}

							Object4->ObjectType = OBJECT_POLYGON;
							Object4->TraceNr = cnt;
							Object4->ObjectType2 = 0;
							Object4->Address = 0;
							Object4->Thickness = 0.0;
							Object4->Clearance = 0.0;

							if (NrObjects4 >= MaxNrObjects4 - 1)
							{
								if (AllocateMemObjects4(MaxNrObjects4 + 1024) == 0)
									NrObjects4++;
							}
							else
								NrObjects4++;
						}

						NrFound += 1;
					}
				}
			}
		}
	}

	return NrFound;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyCopperObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	/*

	  mode :

	  bit 1  : Search for OBJECT_SELECTED
	  bit 2  : Add clearance for searching
	  bit 4  : Do not add routing keepout
	  bit 5  : Do not add big polygons

	*/
	int32 Found;

	if (SelectedLayer >= 32)
		return 0;

	// Copy objects in search window to Objects4

	NrObjects4 = 0;
	Found = 0;
	Found += CopyTracesFromRectWindowToObjects4(SelectedLayer, mode);
	Found += CopyViasFromRectWindowToObjects4(SelectedLayer, mode);
	Found += CopyCompObjectsFromRectWindowToObjects4(SelectedLayer, mode);
	Found += CopyOtherObjectsFromRectWindowToObjects4(SelectedLayer, mode);

	return Found;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyAllObjectsFromRectWindowToObjects4(int32 SelectedLayer, int32 mode)
{
	/*

	  mode :

	  bit 1  : Search for OBJECT_SELECTED
	  bit 2  : Add clearance for searching

	*/
	int32 Found;

	if (SelectedLayer > 32)
		return 0;

	NrObjects4 = 0;
	Found = 0;
	Found += CopyTracesFromRectWindowToObjects4(1, SelectedLayer);
	Found += CopyViasFromRectWindowToObjects4(8 + 1 + mode, SelectedLayer);	// With inner pads
	Found += CopyCompObjectsFromRectWindowToObjects4(SelectedLayer, 8 + 1 + mode);

	if (SelectedLayer == -1)
	{
		Found += CopyOtherObjectsFromRectWindowToObjects4(1 + mode, 0);

		if (Design.NrBoardLayers > 1)
			Found += CopyOtherObjectsFromRectWindowToObjects4(1, Design.NrBoardLayers - 1);

		if (CheckIfInnerLayer(SelectedLayer))
		{
			Found += CopyOtherObjectsFromRectWindowToObjects4(1 + mode, SelectedLayer);
			Found += CopyOtherObjectsFromRectWindowToObjects4(1, ROUTING_KEEPOUT_LAYER + SelectedLayer);
		}
	}
	else
	{
		if (SelectedLayer == 0)
		{
			Found += CopyOtherObjectsFromRectWindowToObjects4(1 + mode, 0);
			Found += CopyOtherObjectsFromRectWindowToObjects4(1, ROUTING_KEEPOUT_LAYER);
		}

		if (SelectedLayer == Design.NrBoardLayers - 1)
		{
			Found += CopyOtherObjectsFromRectWindowToObjects4(1 + mode, SelectedLayer);
			Found += CopyOtherObjectsFromRectWindowToObjects4(1, ROUTING_KEEPOUT_LAYER + SelectedLayer);
		}

		if (CheckIfInnerLayer(SelectedLayer))
		{
			Found += CopyOtherObjectsFromRectWindowToObjects4(1 + mode, SelectedLayer);
			Found += CopyOtherObjectsFromRectWindowToObjects4(1, ROUTING_KEEPOUT_LAYER + SelectedLayer);
		}
	}

	Found += CopyOtherObjectsFromRectWindowToObjects4(1 + mode, DRILL_LAYER);
	Found += CopyOtherObjectsFromRectWindowToObjects4(1 + mode, DRILL_UNPLATED_LAYER);

	if (Found > 0)
	{
		AllocateMemObjects4(Found);
		// Copy objects in search window to Objects4
		CopyTracesFromRectWindowToObjects4(0, SelectedLayer);
		CopyViasFromRectWindowToObjects4(8 + mode, SelectedLayer);	// With inner pads
		CopyCompObjectsFromRectWindowToObjects4(SelectedLayer, 8 + mode);	// With inner pads

		if (SelectedLayer == -1)
		{
			CopyOtherObjectsFromRectWindowToObjects4(mode, 0);

			if (Design.NrBoardLayers > 1)
				CopyOtherObjectsFromRectWindowToObjects4(mode, Design.NrBoardLayers - 1);
		}
		else
		{
			if (SelectedLayer == 0)
				CopyOtherObjectsFromRectWindowToObjects4(mode, 0);

			if (Design.NrBoardLayers > 1)
			{
				if (SelectedLayer == Design.NrBoardLayers - 1)
					CopyOtherObjectsFromRectWindowToObjects4(mode, Design.NrBoardLayers - 1);
			}
		}

		CopyOtherObjectsFromRectWindowToObjects4(mode, DRILL_LAYER);
		CopyOtherObjectsFromRectWindowToObjects4(mode, DRILL_UNPLATED_LAYER);
	}

	return Found;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnhiliteTraces()
{
	int32 cnt, TraceInfo, Layer;
	TraceRecord *Trace;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && ((TraceInfo & OBJECT_HIGHLITED) != 0))
			{
				Trace->Info = (int16) (TraceInfo ^ OBJECT_HIGHLITED);
				DataBaseChanged = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && ((TraceInfo & OBJECT_HIGHLITED) != 0))
			{
				Trace->Info = (int16) (TraceInfo ^ OBJECT_HIGHLITED);
				DataBaseChanged = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && ((TraceInfo & OBJECT_HIGHLITED) != 0))
			{
				Trace->Info = (int16) (TraceInfo ^ OBJECT_HIGHLITED);
				DataBaseChanged = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && ((TraceInfo & OBJECT_HIGHLITED) != 0))
			{
				Trace->Info = (int16) (TraceInfo ^ OBJECT_HIGHLITED);
				DataBaseChanged = 1;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnhiliteVias()
{
	int32 cnt, ViaInfo;
	ViaRecord *Via;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if (((ViaInfo & OBJECT_NOT_VISIBLE) == 0) && ((ViaInfo & OBJECT_HIGHLITED) != 0))
		{
			Via->Info = (int16) (ViaInfo ^ OBJECT_HIGHLITED);
			DataBaseChanged = 1;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnhiliteComps()
{
	int32 cnt, CompInfo;
	CompRecord *Comp;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((CompInfo & OBJECT_NOT_VISIBLE) == 0) && ((CompInfo & OBJECT_HIGHLITED) != 0))
			Comp->Info = (int16) (CompInfo ^ OBJECT_HIGHLITED);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnhiliteNets()
{
	int32 cnt;
	NetRecord *Net;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (Net->Name[0] != 0)
		{
			if ((Net->Info & OBJECT_HIGHLITED) != 0)
			{
				Net->Info ^= OBJECT_HIGHLITED;
				DataBaseChanged = 1;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnhiliteConnections()
{
	int32 cnt, ConnectionInfo;
	ConnectionsRecord *Connection;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if (((ConnectionInfo & OBJECT_NOT_VISIBLE) == 0) && ((ConnectionInfo & OBJECT_HIGHLITED) != 0))
			Connection->Info = (int16) (ConnectionInfo ^ OBJECT_HIGHLITED);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeNetsHilite(int32 NetNr, int32 Hilite)
{
	int32 cnt, cnt2, TraceInfo, Layer, ConnectionInfo, ViaInfo;
	TraceRecord *Trace;
	ViaRecord *Via;
	ConnectionsRecord *Connection;

	StartDrawingEditingWindow(BM_DoubleBuffer);

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr == NetNr))
			{
				if (Hilite)
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == 0)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
							DrawVerTrace(Trace);

						DataBaseChanged = 1;
					}
				}
				else
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == OBJECT_HIGHLITED)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);
						DrawVerTrace(Trace);
						DataBaseChanged = 1;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr == NetNr))
			{
				if (Hilite)
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == 0)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
							DrawHorTrace(Trace);

						DataBaseChanged = 1;
					}
				}
				else
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == OBJECT_HIGHLITED)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
							DrawHorTrace(Trace);

						DataBaseChanged = 1;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr == NetNr))
			{
				if (Hilite)
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == 0)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
							DrawDiag1Trace(Trace);

						DataBaseChanged = 1;
					}
				}
				else
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == OBJECT_HIGHLITED)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
							DrawDiag1Trace(Trace);

						DataBaseChanged = 1;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr == NetNr))
			{
				if (Hilite)
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == 0)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
							DrawDiag2Trace(Trace);

						DataBaseChanged = 1;
					}
				}
				else
				{
					if ((TraceInfo & (OBJECT_HIGHLITED)) == OBJECT_HIGHLITED)
					{
						Trace->Info ^= (int16) (OBJECT_HIGHLITED);

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
							DrawDiag2Trace(Trace);

						DataBaseChanged = 1;
					}
				}
			}

		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if (((ViaInfo & OBJECT_NOT_VISIBLE) == 0) && (Via->NetNr == NetNr))
		{
			if (Hilite)
			{
				if ((ViaInfo & (OBJECT_HIGHLITED)) == 0)
				{
					Via->Info ^= (int16) (OBJECT_HIGHLITED);

					if (OkToDrawVias)
					{
						DrawVia(Via);
						DrawViaDrill(Via);
					}

					DataBaseChanged = 1;
				}
			}
			else
			{
				if ((ViaInfo & (OBJECT_HIGHLITED)) == OBJECT_HIGHLITED)
				{
					Via->Info ^= (int16) (OBJECT_HIGHLITED);

					if (OkToDrawVias)
					{
						DrawVia(Via);
						DrawViaDrill(Via);
					}

					DataBaseChanged = 1;
				}
			}
		}
	}

	SetROP2(OutputDisplay, R2_XORPEN);
	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if (((ConnectionInfo & OBJECT_NOT_VISIBLE) == 0) && (Connection->NetNr == NetNr))
		{
			if (Hilite)
			{
				if ((ConnectionInfo & (OBJECT_HIGHLITED)) == 0)
				{
					Connection->Info ^= (int16) (OBJECT_HIGHLITED);

					if (OkToDrawConnections)
						DrawConnection(Connection);

					DataBaseChanged = 1;
				}
			}
			else
			{
				if ((ConnectionInfo & (OBJECT_HIGHLITED)) == OBJECT_HIGHLITED)
				{
					Connection->Info ^= (int16) (OBJECT_HIGHLITED);
					DrawConnection2(Connection);

					if ((Connection->Info & (CONNECTIONS_NOT_VISIBLE)) == 0)
					{
						if (OkToDrawConnections)
							DrawConnection(Connection);

						DataBaseChanged = 1;
					}
				}
			}
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow(BM_DoubleBuffer);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void HiliteVisibleConnections(int32 mode)
{
	int32 cnt, Mask;
	ConnectionsRecord *Connection;
	NetRecord *Net;

	if (mode == 0)
	{
		Mask = OBJECT_NOT_VISIBLE | OBJECT_HIGHLITED | CONNECTIONS_DISABLED | CONNECTIONS_NOT_VISIBLE;

		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);

			if ((Connection->Info & (Mask)) == 0)
			{
				if ((Connection->NetNr >= 0) && (Connection->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Connection->NetNr]);
					Net->Info |= OBJECT_HIGHLITED;
				}

				Connection->Info |= OBJECT_HIGHLITED;
				DataBaseChanged = 1;
			}
		}
	}
	else
	{
		Mask = OBJECT_NOT_VISIBLE | OBJECT_HIGHLITED | CONNECTIONS_DISABLED | CONNECTIONS_NOT_VISIBLE;

		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);

			if ((Connection->Info & (Mask)) == OBJECT_HIGHLITED)
			{
				if ((Connection->NetNr >= 0) && (Connection->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Connection->NetNr]);
					Net->Info &= ~OBJECT_HIGHLITED;
				}

				Connection->Info &= ~OBJECT_HIGHLITED;
				DataBaseChanged = 1;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ViewAllConnections()
{
	int32 ConnectionInfo, cnt;
	ConnectionsRecord *Connection;
	NetRecord *Net;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & (OBJECT_NOT_VISIBLE | CONNECTIONS_DISABLED)) == 0)
			Connection->Info &= ~(OBJECT_HIGHLITED | CONNECTIONS_NOT_VISIBLE);
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (Net->Name[0] != 0)
			Net->Info &= ~(OBJECT_HIGHLITED | CONNECTIONS_NOT_VISIBLE);
	}

	DataBaseChanged = 1;
	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnViewAllConnections()
{
	int32 ConnectionInfo, cnt;
	ConnectionsRecord *Connection;
	NetRecord *Net;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & (OBJECT_NOT_VISIBLE | CONNECTIONS_DISABLED)) == 0)
			Connection->Info |= CONNECTIONS_NOT_VISIBLE;
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (Net->Name[0] != 0)
			Net->Info |= CONNECTIONS_NOT_VISIBLE;
	}

	DataBaseChanged = 1;
	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopySelectionsToClipBoard()
{
	int32 *TotalMemSize;
	HGLOBAL NewGlobalClipBoardMem;
	uint8 *NewClipBoardMem;

	if (!OpenClipboard(PCBWindow))
		return -1;

	if (!EmptyClipboard())
		return -1;

	if ((NewGlobalClipBoardMem = GlobalAlloc(GHND | GMEM_DDESHARE, ClipBoardMemPos + 4)) == NULL)
		return -1;

	if ((NewClipBoardMem = GlobalLock(NewGlobalClipBoardMem)) == NULL)
		return -1;

	TotalMemSize = (int32 *) NewClipBoardMem;
	*TotalMemSize = ClipBoardMemPos;
	memmove(&NewClipBoardMem[4], ClipBoardMem, ClipBoardMemPos);

	GlobalUnlock(NewGlobalClipBoardMem);

	if (SetClipboardData(ClipID2, NewGlobalClipBoardMem) == NULL)
	{
		GlobalUnlock(NewGlobalClipBoardMem);
		GlobalFree(NewGlobalClipBoardMem);
		return -1;
	}

	CloseClipboard();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopySelectionsFromClipBoard()
{
	int32 TotalMemSize, *hulp;
	HGLOBAL NewGlobalClipBoardMem;
	uint8 *NewClipBoardMem;

	if (!IsClipboardFormatAvailable(ClipID2))
		return -1;

	if (!OpenClipboard(PCBWindow))
		return -1;

	if ((NewGlobalClipBoardMem = GetClipboardData(ClipID2)) == NULL)
		return -1;

	if ((NewClipBoardMem = GlobalLock(NewGlobalClipBoardMem)) == NULL)
		return -1;

	hulp = (int32 *) (NewClipBoardMem);
	TotalMemSize = *hulp;

	if (ClipBoardMemSize <= TotalMemSize)
	{
		if (AllocateMemClipBoard(TotalMemSize + 64) != 0)
		{
			GlobalUnlock(NewGlobalClipBoardMem);
			CloseClipboard();
			return -1;
		}
	}

	if (TotalMemSize >= ClipBoardMemSize)
	{
		if (AllocateMemClipBoard(TotalMemSize + 4096) != 0)
			return -1;
	}

	memmove(ClipBoardMem, &NewClipBoardMem[4], TotalMemSize);
	ClipBoardMemPos = TotalMemSize;
	GlobalUnlock(NewGlobalClipBoardMem);
	EmptyClipboard();
	CloseClipboard();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeVisibiltyConnectionsComponentsVisible(CompRecord * Comp, int32 mode, int32 Visible)
{
	int32 cnt2, cnt3, PinsMemPos, ModeXor, PinNr, ConnectionFound;
	double cx1, cy1, cx2, cy2, ObjectX, ObjectY, CompX, CompY;
	ObjectRecord *Object, *Object4;
	ConnectionsRecord *Connection;
	CompPinRecord *CompPin;

	SetROP2(OutputDisplay, R2_XORPEN);

	SearchMinX = Comp->BoardPosMinX;
	SearchMinY = Comp->BoardPosMinY;
	SearchMaxX = Comp->BoardPosMaxX;
	SearchMaxY = Comp->BoardPosMaxY;
	NrObjects4 = CopyConnectionsFromRectWindowToObjects4(0, -1);

	PinsMemPos = (uint8 *) Comp - &(CompsMem[0]) + sizeof(CompRecord);
	NrObjects = 0;
	ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
	Object = &((*Objects)[0]);
	CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
	PinNr = Object->PinNr;
	CompX = Comp->CompOriginX;
	CompY = Comp->CompOriginY;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);

		if (Object->PinNr != PinNr)
		{
			PinNr = Object->PinNr;
			PinsMemPos += sizeof(CompPinRecord);
			CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
		}

		if (CompPin->NetNr != 32767)
		{
			ObjectX = Object->x1;
			ObjectY = Object->y1;
			ConnectionFound = 0;

			for (cnt3 = 0; cnt3 < NrObjects4; cnt3++)
			{
				Object4 = &((*Objects4)[cnt3]);
				cx1 = Object4->x1;
				cy1 = Object4->y1;
				cx2 = Object4->x2;
				cy2 = Object4->y2;
				ModeXor = 0;

				if ((InRange(cx1, ObjectX)) && (InRange(cy1, ObjectY)))
					ModeXor |= 1;

				if ((InRange(cx2, ObjectX)) && (InRange(cy2, ObjectY)))
					ModeXor |= 2;

				if (ModeXor > 0)
				{
					Connection = &((*Connections)[Object4->TraceNr]);

					if (Visible)
					{
						if ((Connection->Info & CONNECTIONS_NOT_VISIBLE) == CONNECTIONS_NOT_VISIBLE)
						{
							Connection->Info &= ~CONNECTIONS_NOT_VISIBLE;
							DrawConnection(Connection);
						}
					}
					else
					{
						if ((Connection->Info & CONNECTIONS_NOT_VISIBLE) == 0)
						{
							DrawConnection(Connection);
							Connection->Info |= CONNECTIONS_NOT_VISIBLE;
						}
					}
				}
			}
		}
	}

	SetROP2(OutputDisplay, R2_COPYPEN);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 HideSelectedConnections()
{
	int32 cnt, cnt2, ConnectionInfo;
	ConnectionsRecord *Connection;

	StartDrawingEditingWindow(BM_DoubleBuffer);
	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | CONNECTIONS_NOT_VISIBLE)) == OBJECT_SELECTED)
		{
			DrawConnection2(Connection);
			Connection->Info ^= (CONNECTIONS_NOT_VISIBLE);
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow(BM_DoubleBuffer);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeNetsVisibility(int32 NetNr, int32 Visibility)
{
	int32 cnt, cnt2, ConnectionInfo;
	ConnectionsRecord *Connection;

	StartDrawingEditingWindow(BM_DoubleBuffer);
//  SetROP2(OutputDisplay,R2_COPYPEN);
	SetROP2(OutputDisplay, R2_XORPEN);
	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if (((ConnectionInfo & OBJECT_NOT_VISIBLE) == 0) && (Connection->NetNr == NetNr))
		{
			if (!Visibility)
			{
				if ((ConnectionInfo & (CONNECTIONS_NOT_VISIBLE)) == 0)
				{
					DrawConnection(Connection);
					Connection->Info ^= (CONNECTIONS_NOT_VISIBLE);
				}
			}
			else
			{
				if ((ConnectionInfo & (CONNECTIONS_NOT_VISIBLE)) == CONNECTIONS_NOT_VISIBLE)
				{
					Connection->Info ^= (CONNECTIONS_NOT_VISIBLE);
					DrawConnection(Connection);
				}
			}
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow(BM_DoubleBuffer);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckClipBoard()
{
	int32 BufPos, cnt, *Command, NetNr;
	char *CompRef, *NetName;
	NetRecord *Net;
	CompRecord *Comp;

	if (ActiveSchematicSelect)
	{
		if (FirstSchematicSelect == 0)
		{
			UnViewAllConnections();
			FirstSchematicSelect = 1;
			UnselectAll = 1;
			SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
			UnselectAll = 0;
		}

		if (CopySelectionsFromClipBoard() == 0)
		{
#ifdef _DEBUG
//      Beep(2000,500);
#endif
			AllocateMemObjects4(2048);
			BufPos = 0;

			while (BufPos < ClipBoardMemPos)
			{
				Command = (int32 *) & (ClipBoardMem[BufPos]);

				switch (*Command)
				{
				case UNSELECT_ALL:
					UnselectAll = 1;
					SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
					UnselectAll = 0;
					UnViewAllConnections();

					for (cnt = 0; cnt < Design.NrNets; cnt++)
					{
						Net = &((*Nets)[cnt]);
						Net->Info &= ~CONNECTIONS_NOT_VISIBLE;
//              Net->Info&=~OBJECT_HIGHLITED;
					}

//            ConnectionsNetsNonVisible(0);
//            HiliteNetsFromDialog(0,0);
					BufPos += 4;
					break;

				case UNSELECT_NETNR:
					NetName = (LPSTR) & (ClipBoardMem[BufPos + 4]);

					if (((NetNr = GetNetNr(NetName)) != -1) && (NetNr >= 0) && (NetNr < Design.NrNets))
					{
						Net = &((*Nets)[NetNr]);
						Net->Info &= ~CONNECTIONS_NOT_VISIBLE;
//              Net->Info&=~OBJECT_HIGHLITED;
//              ChangeNetsHilite((int32)*NetNr,0);
						ChangeNetsVisibility((int32) NetNr, 0);
					}

					BufPos += 64;
					break;

				case SELECT_NETNR:
					NetName = (LPSTR) & (ClipBoardMem[BufPos + 4]);

					if (((NetNr = GetNetNr(NetName)) != -1) && (NetNr >= 0) && (NetNr < Design.NrNets))
					{
						Net = &((*Nets)[NetNr]);
						Net->Info &= ~CONNECTIONS_NOT_VISIBLE;
//              Net->Info&=~OBJECT_HIGHLITED;
//              ChangeNetsHilite((int32)*NetNr,0);
						ChangeNetsVisibility((int32) NetNr, 1);
					}

					BufPos += 64;
					break;

				case UNSELECT_REF:
					CompRef = (LPSTR) & (ClipBoardMem[BufPos + 4]);
					BufPos += 16;
					StartDrawingEditingWindow(BM_DoubleBuffer);

					for (cnt = 0; cnt < Design.NrComps; cnt++)
					{
						Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

						if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
						        && (stricmpOwn(CompRef, Comp->Name) == 0))
						{
							Comp->Info &= ~(0x10);
							Comp->Info &= ~OBJECT_SELECTED;
							SetBackGroundActive(0);
							Comp->TextVisibility &= ~(1 + 16);
							DrawComp2(Comp, 0.0, 0.0, 0, 0x201);
							DrawComp2(Comp, 0.0, 0.0, 0, 0200);
							ChangeVisibiltyConnectionsComponentsVisible(Comp, 0, 0);
						}
					}

					ExitDrawing();
					EndDrawingEditingWindow(BM_DoubleBuffer);
					break;

				case SELECT_REF:
					CompRef = (LPSTR) & (ClipBoardMem[BufPos + 4]);
					BufPos += 16;
					StartDrawingEditingWindow(BM_DoubleBuffer);

					for (cnt = 0; cnt < Design.NrComps; cnt++)
					{
						Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

						if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | COMPONENT_PROTECTED)) == 0)
						        && (stricmpOwn(CompRef, Comp->Name) == 0))
						{
							Comp->Info &= ~(0x10);
							Comp->Info |= (OBJECT_SELECTED);
							DrawComp(Comp, 0.0, 0.0, 0, 0x200);
							ChangeVisibiltyConnectionsComponentsVisible(Comp, 0, 1);
						}
					}

					ExitDrawing();
					EndDrawingEditingWindow(BM_DoubleBuffer);
					break;
				}
			}

			if (MaxNrObjects4 > 4096)
				DeAllocateMemObjects4();
		}
	}
	else
		FirstSchematicSelect = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ComponentProtection(int32 NetsSelected, int32 * NetInfo, uint8 * SelectedComponents)
{
	int32 cnt, cnt2, ok, index;
	CompRecord *Comp;

	memset(SelectedComponents, 0, Design.NrComps);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "C1") == 0)
				ok = 1;

#endif

			if ((Comp->Info & (COMPONENT_PROTECTED)) == COMPONENT_PROTECTED)
			{
				SelectedComponents[cnt] = 1;
				ok = 1;
			}
		}
	}

	cnt2 = 0;

	for (cnt = 0; cnt < NetsSelected; cnt++)
	{
		index = NetInfo[cnt];
		SelectedComponents[index] |= 2;
		cnt2++;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

		if (stricmpOwn(Comp->Name, "C1") == 0)
			ok = 1;

#endif

		switch (SelectedComponents[cnt])
		{
		case 0:
			break;

		case 1:
			Comp->Info &= ~COMPONENT_PROTECTED;
			DataBaseChanged = 1;
			break;

		case 2:
			Comp->Info |= COMPONENT_PROTECTED;
			Comp->Info &= ~OBJECT_SELECTED;
			Comp->TextVisibility &= ~(1 + 16);
			DataBaseChanged = 1;
			break;

		case 3:
			break;
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DisableConnectionsNets(int32 NrNetsSelected, int32 mode, int32 * NetInfo, uint8 * SelectedNets)
{
	int32 cnt, cnt2, index;
	ConnectionsRecord *Connection;
	NetRecord *Net;

	memset(SelectedNets, 0, Design.NrNets);

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (Net->Name[0] != 0)
		{
			if ((Net->Info & CONNECTIONS_DISABLED) == CONNECTIONS_DISABLED)
				SelectedNets[cnt] = 1;
		}
	}

	for (cnt = 0; cnt < NrNetsSelected; cnt++)
	{
		index = NetInfo[cnt];
		SelectedNets[index] |= 2;
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		switch (SelectedNets[cnt])
		{
		case 0:
			break;

		case 1:
			Net->Info &= ~CONNECTIONS_DISABLED;
			DataBaseChanged = 1;
			DeleteAndUndisplayConnectionsNet(cnt, 1);
			sprintf(InfoStr, SC(1062, "Calculating guides for net %s"), Net->Name);
			RedrawInfoStr(1);
			CheckNet(cnt, 0);
			InsertConnections(cnt, 0);

			for (cnt2 = 0; cnt2 < Design.NrConnections; cnt2++)
			{
				Connection = &((*Connections)[cnt2]);

				if (((Connection->Info & OBJECT_NOT_VISIBLE) == 0) && (Connection->NetNr == cnt))
				{
					Connection->AddNr = (int16) LastActionNr;

					if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
						Connection->Info |= OBJECT_HIGHLITED;
				}
			}

			break;

		case 2:
			DeleteAndUndisplayConnectionsNet(cnt, 1);
			DataBaseChanged = 1;
			Net->Info |= CONNECTIONS_DISABLED;
			break;

		case 3:
			break;
		}
	}

	RePaint();
	InfoStr[0] = 0;
	RedrawInfoStr(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void UnselectTracesViasNet(int32 NrNetsSelected, int32 * NetInfo, uint8 * SelectedNets)
{
	int32 cnt, TraceInfo, Layer, index, ViaInfo;
	TraceRecord *Trace;
	ViaRecord *Via;

//  memset(SelectedNets,0,Design.NrNets);
	for (cnt = 0; cnt < NrNetsSelected; cnt++)
	{
		index = NetInfo[cnt];
		SelectedNets[index] |= 2;
	}

	CheckInputMessages(0);

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Trace->NetNr >= 0)
			        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr] == 1))
				Trace->Info = (int16) (TraceInfo ^ OBJECT_SELECTED);
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Trace->NetNr >= 0)
			        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr] == 1))
				Trace->Info = (int16) (TraceInfo ^ OBJECT_SELECTED);
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Trace->NetNr >= 0)
			        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr] == 1))
				Trace->Info = (int16) (TraceInfo ^ OBJECT_SELECTED);
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Trace->NetNr >= 0)
			        && (Trace->NetNr < Design.NrNets) && (SelectedNets[Trace->NetNr] == 1))
				Trace->Info = (int16) (TraceInfo ^ OBJECT_SELECTED);

		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if (((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Via->NetNr >= 0)
		        && (Via->NetNr < Design.NrNets) && (SelectedNets[Via->NetNr] == 1))
			Via->Info = (int16) (ViaInfo ^ OBJECT_SELECTED);
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteNets(int32 NetsSelected, int32 * NetInfo)
{
	int32 cnt, index;

	for (cnt = 0; cnt < NetsSelected; cnt++)
	{
		index = NetInfo[cnt];
		DeleteNet((int32) index);
		ReCalcConnectionsNet(index, 0, 1);
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
