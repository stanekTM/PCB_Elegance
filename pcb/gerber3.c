/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: gerber3.c
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
#include "calcdef.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "plot.h"
#include "polygon.h"



extern double CurrentPlotPenSize;
extern int32 PlotDrawingOk, PenPlotMode;
extern int32 ChangeSilkScreenPen, ChangeSilkScreenPenAsk;
extern HWND PCBWindow;
extern int32 GerberPrepareErrors;

int32 ok;

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 CheckPenPlotSize(double *SpecialLineThickNess)
{
	char str[MAX_LENGTH_STRING];

	if (!ChangeSilkScreenPenAsk)
	{
		if (InRange(*SpecialLineThickNess, CurrentPlotPenSize))
			return 0;

		strcpy(str, SC(424, "There are lines, rectangles, circles, arcs, texts on the layers\n\n"));
		strcat(str, SC(425, "Silkscreen\nBoard outline\nInfo layers\n\n"));
		strcat(str, SC(426, "with a different diameter than plotter pen 1.\n\n"));
		strcat(str, SC(427, "Do you want to use plotter pen 1 for these objects ?"));

		if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDYES)
			ChangeSilkScreenPen = 1;

		ChangeSilkScreenPenAsk = 1;
	}

	if (ChangeSilkScreenPen)
		*SpecialLineThickNess = CurrentPlotPenSize;

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareTraces(int32 Layer, int32 mode)
{
	int32 cnt, TraceInfo, cnt2, mode2;
	TraceRecord *Trace;
	ObjectRecord *Object4;
#ifdef _DEBUG
	int32 ok;
#endif

	if (mode == 0)
		mode2 = 3;
	else
		mode2 = 1;

	Object4 = NULL;
	cnt2 = NrObjects4;

	for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
	{
		Trace = &((*VerTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object4 = GetNewObject4()) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object4, TRACE_VER, Layer, cnt, mode2);
			Object4->Info2 = CheckTraceAperTure(Trace->ThickNess);
			NrObjects4++;
		}
	}

	for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
	{
		Trace = &((*HorTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object4 = GetNewObject4()) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object4, TRACE_HOR, Layer, cnt, mode2);
			Object4->Info2 = CheckTraceAperTure(Trace->ThickNess);
			NrObjects4++;
		}
	}

	for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
	{
		Trace = &((*Diag1Traces[Layer])[cnt]);
#ifdef _DEBUG

		if (InRange4(Trace->ThickNess, 3.048e5))
			ok = 1;

#endif
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object4 = GetNewObject4()) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object4, TRACE_DIAG1, Layer, cnt, mode2);
			Object4->Info2 = CheckTraceAperTure(Trace->ThickNess);
			NrObjects4++;
		}
	}

	for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
	{
		Trace = &((*Diag2Traces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object4 = GetNewObject4()) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object4, TRACE_DIAG2, Layer, cnt, mode2);
			Object4->Info2 = CheckTraceAperTure(Trace->ThickNess);
			NrObjects4++;
		}
	}

	return NrObjects4 - cnt2;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareVias(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt3, ViaInfo, Found;
	ViaRecord *Via;
	ObjectRecord *Object4;
	int32 PowerPlaneLayer;
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf2[10240];
	AreaFillRecord *AreaFill;

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	cnt3 = NrObjects4;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	if ((mode & 1) == 0)
	{
		if ((Layer == 0) || (Layer == Design.NrBoardLayers - 1))
		{
			for (cnt = 0; cnt < Design.NrVias; cnt++)
			{
				Via = &((*Vias)[cnt]);
				ViaInfo = Via->Info;

				if ((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					CreateViaObjectFromVia(Via, Object4, 1);
					Object4->Info = 2;
					Object4->Test = 0;
					Object4->TraceNr = -1;
					Object4->Info2 = CheckRoundPadAperTure(Via->ThickNess);
					NrObjects4++;
				}
			}
		}
		else
		{
			// ****************************************************************************
			// ****************************************************************************
			for (cnt = 0; cnt < Design.NrVias; cnt++)
			{
				Via = &((*Vias)[cnt]);
				ViaInfo = Via->Info;

				if ((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Found = -1;

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					CreateViaObjectFromVia(Via, Object4, 1);
					Object4->y2 = 0.0;
					Object4->x3 = Via->ThermalInner;
					Object4->TraceNr = -1;
					Object4->Info = 0;
					Object4->Test = 0;

					if (PowerPlaneLayer)
					{
						MakePolygonFromObject(Object4, PolygonObject, 0.0, 0.00001, 1, 0);

						for (cnt2 = 0; cnt2 < Design.NrAreaFills; cnt2++)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt2]]);

							if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
							        && (AreaFill->maxx >= Object4->minx) && (AreaFill->minx <= Object4->maxx)
							        && (AreaFill->maxy >= Object4->miny) && (AreaFill->miny <= Object4->maxy))
							{
								if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
									Found = cnt2;
							}
						}
					}

					if (Found != -1)
					{
						AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
						Object4->TraceNr = Found;

						if (AreaFill->NetNr == Via->NetNr)
						{
							if ((Via->DrillThickNess > MinDrillForThermalRelief)
							        && ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) == AREAFILL_WITH_THERMAL_RELIEF)
							        && ((AreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF) == 0)
							        && (NotInRange(AreaFill->ThermalReliefDistance, 0.0))
							        && (NotInRange(AreaFill->ThermalReliefThickness, 0.0)))
							{
								// Add thermal relief if diameter greater MinDrillForThermalRelief
								Object4->Info2 =
								    CheckThermalReliefAperTure(Via->DrillThickNess,
								                               Via->DrillThickNess +
								                               AreaFill->ThermalReliefThickness * 2.0,
								                               AreaFill->ThermalReliefDistance);
								//                  Object4->Info2=CheckRoundPadAperTure(AreaFill->ThermalReliefThickness);
								Object4->Info |= 4;
								Object4->x2 = Via->DrillThickNess + AreaFill->ThermalReliefThickness * 2.0;
								FillPositionObject(Object4);
								NrObjects4++;
							}
						}
						else
						{
							// Add anti power pad
							if (NotInRange(Object4->x3, 0.0))
								Object4->x2 = Object4->x3;
							else
								Object4->x2 += max(Via->Clearance, Design.StandardClearance) * 2;

							Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
							NrObjects4++;
						}
					}
					else
					{
						// Add normal pad
						Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
						NrObjects4++;
					}
				}
			}
		}
	}
	else
	{
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// Via drill holes
		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (NrObjects4 + 1 > MaxNrObjects4)
				{
					if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
						return -1;
				}

				Object4 = &((*Objects4)[NrObjects4]);
				Object4->x1 = Via->X;
				Object4->y1 = Via->Y;
				Object4->x2 = Via->DrillThickNess;
				Object4->ObjectType = PIN_SMD_ROUND;
				Object4->Test = 0;
				Object4->Info2 = CheckRoundPadAperTure(Via->DrillThickNess);
				NrObjects4++;
				FillPositionObject(Object4);
				Object4->Info = 2;
			}
		}
	}

	return NrObjects4 - cnt3;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareComponentPins(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, Found;
	double x2a, y2a;
	CompRecord *Comp;
	ObjectRecord *Object, *Object4, *Object2;
	int32 PowerPlaneLayer;
	AreaFillRecord *AreaFill;
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf2[10240];
	ObjectArcRecord *ObjectArc;
#ifdef _DEBUG
	int32 ok;
#endif

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	Object4 = NULL;
	cnt3 = NrObjects4;
	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);

	if ((mode & 1) == 0)
	{
		if ((Layer == 0) || (Layer == Design.NrBoardLayers - 1))
		{
			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					NrObjects = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "Z101") == 0)
						ok = 1;

#endif

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if (GerberInfo.Invert)
						{
							switch (Object->ObjectType)
							{
							case OBJECT_POLYGON:
							case PIN_SMD_POLYGON:
							case PIN_PUT_THROUGH_POLYGON:
								Object->Mirror ^= 1;
								Object->RotationAngle = -Object->RotationAngle;

								if (Object->RotationAngle < 0.0)
									Object->RotationAngle += 360.0;

								break;
							}
						}
					}

					// ****************************************************************************

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

						if (stricmpOwn(Comp->Name, "Z100") == 0)
						{
							ok = 1;

							if (Layer == 1)
							{
								ok = 1;

								if (cnt2 == 11)
									ok = 1;
							}
						}

#endif

						if ((Object->Layer == -1) || (Object->Layer == Layer))
						{
							if ((Object4 = GetNewObject4()) == NULL)
								return -1;

							memmove(Object4, Object, sizeof(ObjectRecord));
							Object4->Test = 0;
							Object4->Info &= OBJECT_FILLED;

							switch (Object->ObjectType)
							{
							case DRILL:
								//              case DRILL_UNPLATED:
								Object4->Info2 = CheckRoundPadAperTure(Object->x2);
								NrObjects4++;
								FillPositionObject(Object4);
								Object4->Info = 2;
								break;

							case PIN_PUT_THROUGH_ROUND:
								Object4->Info2 = CheckRoundPadAperTure(Object->x2);
								NrObjects4++;
								FillPositionObject(Object4);
								Object4->Info = 2;
								break;

							case PIN_SMD_ROUND:
								Object4->Info2 = CheckRoundPadAperTure(Object->x2);
								NrObjects4++;
								FillPositionObject(Object4);
								break;

							case PIN_PUT_THROUGH_SQUARE:
								Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->x2);
								NrObjects4++;
								FillPositionObject(Object4);
								Object4->Info = 2;
								break;

							case PIN_SMD_RECT:
								Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->y2);
								NrObjects4++;
								FillPositionObject(Object4);
								break;

							case PIN_LINE_HOR:
							case PIN_LINE_VER:
							case PIN_LINE_DIAG1:
							case PIN_LINE_DIAG2:
								Object4->Info2 = CheckTraceAperTure(Object->y2);
								NrObjects4++;
								FillPositionObject(Object4);
								break;

							case PIN_LINE_ALL_ANGLE:
							case OBJECT_LINE:
								Object4->Info2 = CheckTraceAperTure(Object->Thickness);
								NrObjects4++;
								FillPositionObject(Object4);
								break;

							case PIN_ARC:
							case OBJECT_ARC:
								Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
								FillPositionObject(Object4);
								NrObjects4++;
								break;

							case OBJECT_POLYGON:
							case PIN_SMD_POLYGON:
								if (Object->ObjectType2 != 0)
								{
									Object4->ObjectType = PIN_SMD_POLYGON;
									Object4->Info2 =
									    CheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
									                         Object->RotationAngle, Object->Mirror, 0);
								}
								else
								{
									Object4->ObjectType = PIN_SMD_POLYGON;
									Object4->Info2 =
									    CheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
									                         Object->Address);
								}

								FillPositionObject(Object4);
								NrObjects4++;
								break;

							case PIN_PUT_THROUGH_POLYGON:
								Object4->ObjectType = PIN_PUT_THROUGH_POLYGON;

								if (Object->ObjectType2 != 0)
								{
									Object4->Info2 =
									    CheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->x2,
									                         Object->RotationAngle, Object->Mirror, 0);
								}
								else
								{
									Object4->Info2 =
									    CheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
									                         Object->Address);
								}

								FillPositionObject(Object4);
								NrObjects4++;
								Object4->Info = 2;
								break;
							}
						}
					}
				}
			}

			// ****************************************************************************
			// ****************************************************************************
		}
		else
		{
			// Inner layers
			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					NrObjects = 0;
					NrObjects2 = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);	// With anti power pads -> Objects
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 1, 0, 1);	// With inner pads      -> Objects2
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "Z100") == 0)
						ok = 1;

#endif

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
						Object2 = &((*Objects2)[cnt2]);
#ifdef _DEBUG

						if (stricmpOwn(Comp->Name, "J103") == 0)
						{
							ok = 1;

							if (Layer == 0)
							{
								ok = 1;

								if (cnt2 == 12)
									ok = 1;
							}
						}

						if ((InRange9(Object->x1, 180.5e5)) && (InRange9(Object->y1, 94.0e5)))
							ok = 1;

#endif

						switch (Object->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:
						case PIN_PUT_THROUGH_SQUARE:
						case DRILL:
						case PIN_PUT_THROUGH_POLYGON:
							Found = -1;

							if ((Object4 = GetNewObject4()) == NULL)
								return -1;

							Object4->x1 = Object->x1;
							Object4->y1 = Object->y1;
							x2a = Object->x2;
							y2a = Object->y2;

							if (Object->ObjectType == DRILL)
							{
								//                      Object->y2=Object->x2;
								Object->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
								Object->y2 = Object->x2;
							}

							Object4->x2 = Object->x2;
							Object4->y2 = 0.0;
							Object4->ObjectType = PIN_SMD_ROUND;
							Object4->Info = 0;
							Object4->Test = 0;
							Object4->TraceNr = -1;
							Object4->NetNr = Object->NetNr;
							FillPositionObject(Object4);

							if (PowerPlaneLayer)
							{
								MakePolygonFromObject(Object4, PolygonObject, 0.0, 0.00001, 1, 0);

								for (cnt4 = 0; cnt4 < Design.NrAreaFills; cnt4++)
								{
									AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt4]]);

									if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
									        && (AreaFill->maxx >= Object4->minx) && (AreaFill->minx <= Object4->maxx)
									        && (AreaFill->maxy >= Object4->miny) && (AreaFill->miny <= Object4->maxy))
									{
										if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
											Found = cnt4;
									}
								}
							}

							// Object inside a areafill (powerplane)
							if (Found != -1)
							{
								AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
								Object4->TraceNr = Found;

								if (AreaFill->NetNr == Object->NetNr)
								{
									// Add thermal relief if diameter greater MinDrillForThermalRelief
									if ((Object->y2 > MinDrillForThermalRelief)
									        && ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) ==
									            AREAFILL_WITH_THERMAL_RELIEF)
									        && (NotInRange(AreaFill->ThermalReliefDistance, 0.0))
									        && (NotInRange(AreaFill->ThermalReliefThickness, 0.0)))
									{
										//                        Object4->x2=Object->x2;
										//                        Object4->x3=AreaFill->ThermalReliefDistance;
										Object4->Info2 =
										    CheckThermalReliefAperTure(Object->y2,
										                               Object->y2 +
										                               AreaFill->ThermalReliefThickness * 2.0,
										                               AreaFill->ThermalReliefDistance);
										//                        Object4->Info2=CheckRoundPadAperTure(AreaFill->ThermalReliefThickness);
										Object4->Info |= 4;	// Add Component pin as a thermal relief
										Object4->x2 = Object->y2 + AreaFill->ThermalReliefThickness * 2.0;
										FillPositionObject(Object4);
										NrObjects4++;
									}
								}
								else
								{
									// Add anti power pad
									if (Object->ObjectType != DRILL)
									{
										if (InRange(Object->x3, 0.0))
											Object4->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
										else
											Object4->x2 = Object->x3;
									}
									else
									{
										if (InRange(y2a, 0.0))
											Object4->x2 = x2a;
										else
										{
											Object4->x2 = y2a;	// anti power pad
										}
									}

									Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
									NrObjects4++;
								}
							}
							else
							{
								// Check inner pad
								if (Object->ObjectType != DRILL)
								{
									if (NotInRange(Object2->x3, 0.0))
										Object4->x2 = Object2->x3;
								}
								else
									Object4->x2 = x2a;

								Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
								NrObjects4++;
							}

							break;

						// ****************************************************************************
						case DRILL_UNPLATED:
							if (PowerPlaneLayer)
							{
								// Add anti power pad
								if ((Object4 = GetNewObject4()) == NULL)
									return -1;

								Object4->x1 = Object->x1;
								Object4->y1 = Object->y1;
								Object4->x2 = Object->x2;

								if (NotInRange(Object->y2, 0.0))
									Object4->x2 = Object->y2;
								else
								{
									// Create pad
									Object4->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
								}

								Object4->y2 = 0.0;
								Object4->ObjectType = PIN_SMD_ROUND;
								Object4->Info = 0;
								Object4->NetNr = Object->NetNr;
								Object4->Test = 0;
								Object4->TraceNr = -1;
								FillPositionObject(Object4);
								Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
								NrObjects4++;
							}

							break;

						// ****************************************************************************
						default:
							if (Object->Layer == Layer)
							{
								if ((Object4 = GetNewObject4()) == NULL)
									return -1;

								memmove(Object4, Object, sizeof(ObjectRecord));
								Object4->Test = 0;
								Object4->Info = 0;

								switch (Object->ObjectType)
								{
								case PIN_SMD_ROUND:
									Object4->Info2 = CheckRoundPadAperTure(Object->x2);
									NrObjects4++;
									FillPositionObject(Object4);
									break;

								case PIN_SMD_RECT:
									Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->y2);
									NrObjects4++;
									FillPositionObject(Object4);
									break;

								case PIN_LINE_HOR:
								case PIN_LINE_VER:
								case PIN_LINE_DIAG1:
								case PIN_LINE_DIAG2:
									Object4->Info2 = CheckTraceAperTure(Object->y2);
									NrObjects4++;
									FillPositionObject(Object4);
									break;

								case PIN_LINE_ALL_ANGLE:
								case OBJECT_LINE:
									Object4->Info2 = CheckTraceAperTure(Object->Thickness);
									NrObjects4++;
									FillPositionObject(Object4);
									break;
								}
							}

							break;
						}
					}
				}
			}

			if (PowerPlaneLayer)
			{
				// Drill objects in a powerplane
				for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
				{
					ObjectArc = &((*ObjectArcs)[cnt]);

					if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
					        && ((ObjectArc->Layer == DRILL_UNPLATED_LAYER) || (ObjectArc->Layer == DRILL_LAYER)))
					{
						if ((Object4 = GetNewObject4()) == NULL)
							return -1;

						Object4->x1 = ObjectArc->CentreX;
						Object4->y1 = ObjectArc->CentreY;
						Object4->x2 = ObjectArc->Width + Design.StandardClearance * 2;
						Object4->y2 = 0.0;
						Object4->ObjectType = PIN_SMD_ROUND;
						Object4->Info = 0;
						Object4->Test = 0;
						Object4->TraceNr = -1;
						Object4->NetNr = -1;
						FillPositionObject(Object4);
						Found = -1;
						MakePolygonFromObject(Object4, PolygonObject, 0.0, 0.00001, 1, 0);

						for (cnt4 = 0; cnt4 < Design.NrAreaFills; cnt4++)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt4]]);

							if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
							        && (AreaFill->maxx >= Object4->minx) && (AreaFill->minx <= Object4->maxx)
							        && (AreaFill->maxy >= Object4->miny) && (AreaFill->miny <= Object4->maxy))
							{
								if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
									Found = cnt4;
							}
						}

						// Object inside a areafill (powerplane)
						if (Found != -1)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
							Object4->TraceNr = Found;
							Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
							NrObjects4++;
						}
					}
				}
			}
		}
	}
	else
	{
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// Component drill holes

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					switch (Object->ObjectType)
					{
					case DRILL_UNPLATED:
					case DRILL:
					case PIN_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_SQUARE:
					case PIN_PUT_THROUGH_POLYGON:
						if (NrObjects4 + 1 > MaxNrObjects4)
						{
							if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
								return -1;
						}

						if ((Object->ObjectType == DRILL_UNPLATED) || (Object->ObjectType == DRILL))
							Object->y2 = Object->x2;

						Object4 = &((*Objects4)[NrObjects4]);
						memmove(Object4, Object, sizeof(ObjectRecord));
						Object4->ObjectType = PIN_SMD_ROUND;
						Object4->Test = 0;
						Object4->x2 = Object->y2;
						Object4->Info2 = CheckRoundPadAperTure(Object->y2);
						NrObjects4++;
						FillPositionObject(Object4);
						Object4->Info = 2;
						break;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (ObjectArc->Layer == DRILL_LAYER)
				{
					if (NrObjects4 + 1 > MaxNrObjects4)
					{
						if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
							return -1;
					}

					Object4 = &((*Objects4)[NrObjects4]);
					Object4->x1 = ObjectArc->CentreX;
					Object4->y1 = ObjectArc->CentreY;
					Object4->x2 = ObjectArc->Width;
					Object4->ObjectType = PIN_SMD_ROUND;
					Object4->Test = 0;
					Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
					NrObjects4++;
					FillPositionObject(Object4);
					Object4->Info = 2;
				}

				if (ObjectArc->Layer == DRILL_UNPLATED_LAYER)
				{
					if (NrObjects4 + 1 > MaxNrObjects4)
					{
						if (AllocateMemObjects4(MaxNrObjects4 + 128) == -1)
							return -1;
					}

					Object4 = &((*Objects4)[NrObjects4]);
					Object4->x1 = ObjectArc->CentreX;
					Object4->y1 = ObjectArc->CentreY;
					Object4->x2 = ObjectArc->Width;
					Object4->ObjectType = PIN_SMD_ROUND;
					Object4->Test = 0;
					Object4->Info2 = CheckRoundPadAperTure(Object4->x2);
					NrObjects4++;
					FillPositionObject(Object4);
					Object4->Info = 2;
				}
			}
		}
	}

	return NrObjects4 - cnt3;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareComponentPinsViasSolderPasteObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, ViaInfo, ViaType;
	CompRecord *Comp;
	ViaRecord *Via;
	ObjectRecord *Object, *Object4;
	int32 PowerPlaneLayer;
#ifdef _DEBUG
	int32 ok;
#endif

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	switch (Layer)
	{
	case SOLD_MASK_BOTTOM:
	case SOLD_MASK_TOP:
		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);
			ViaInfo = Via->Info;
			ViaType = Via->ViaType & 3;

			if (Layer == SOLD_MASK_BOTTOM)
				ViaType &= ~VIA_SOLDMASK_BOTTOM;

			if (Layer == SOLD_MASK_TOP)
				ViaType &= ~VIA_SOLDMASK_TOP;

			if (((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0) && (ViaType == 0) && (NotInRange(Via->SoldMask, 0.0)))
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				Object4->x1 = Via->X;
				Object4->y1 = Via->Y;
				Object4->x2 = Via->SoldMask;
				Object4->ObjectType = PIN_SMD_ROUND;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->TraceNr = -1;
				Object4->Clearance = 0.0;
				FillPositionObject(Object4);
				Object4->Info2 = CheckRoundPadAperTure(Via->SoldMask);
				NrObjects4++;
			}
		}

		break;
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 1);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					if (GerberInfo.Invert)
					{
						switch (Object->ObjectType)
						{
						case OBJECT_POLYGON:
						case PIN_SMD_POLYGON:
						case PIN_PUT_THROUGH_POLYGON:
							Object->Mirror ^= 1;
							Object->RotationAngle = -Object->RotationAngle;

							if (Object->RotationAngle < 0.0)
								Object->RotationAngle += 360.0;

							break;
						}
					}

					memmove(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					Object4->TraceNr = -1;

					switch (Object->ObjectType)
					{
					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						Object4->Info2 = CheckTraceAperTure(Object->Thickness);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
						else
						{
							Object4->Info2 = CheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
						else
						{
							Object4->Info2 = CheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
						else
						{
							Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_RECT;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_HOR:
					case TRACE_HOR:
						Object4->Info2 = CheckTraceAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_HOR;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_VER:
					case TRACE_VER:
						Object4->Info2 = CheckTraceAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_VER;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_DIAG1:
					case TRACE_DIAG1:
						Object4->Info2 = CheckTraceAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_DIAG1;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_DIAG2:
					case TRACE_DIAG2:
						Object4->Info2 = CheckTraceAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_DIAG2;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						Object4->ObjectType = PIN_SMD_POLYGON;

						if (Object->ObjectType2 != 0)
						{
							Object4->Info2 =
							    CheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2, Object->RotationAngle,
							                         Object->Mirror, 0);
						}
						else
						{
							Object4->Info2 =
							    CheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                         Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareComponentInfoObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2;
	double SpecialLineThickNess;
	LPSTR TextP;
	CompRecord *Comp;
	ObjectRecord *Object, *Object4;
	int32 PowerPlaneLayer;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 2);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if (GerberInfo.Invert)
					{
						switch (Object->ObjectType)
						{
						case OBJECT_POLYGON:
						case PIN_SMD_POLYGON:
							if ((Object->ObjectType2 == 0) || ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED))
							{
								Object->Mirror ^= 1;
								Object->RotationAngle = -Object->RotationAngle;

								if (Object->RotationAngle < 0.0)
									Object->RotationAngle += 360.0;
							}

							break;

						case PIN_PUT_THROUGH_POLYGON:
							Object->Mirror ^= 1;
							Object->RotationAngle = -Object->RotationAngle;

							if (Object->RotationAngle < 0.0)
								Object->RotationAngle += 360.0;

							break;
						}
					}

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memmove(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
							Object4->Info2 = CheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
#ifdef _DEBUG

							if (InRange4(Object->x2, 300 * 2540.0))
							{
								ok = 1;

								if (Layer == INFO_LAYER3)
									ok = 1;
							}

#endif
							Object4->Info2 = CheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
#ifdef _DEBUG

							if ((InRange4(Object->x2, 170 * 2540.0)) && (InRange4(Object->y2, 75 * 2540.0)))
							{
								ok = 1;

								if (Layer == INFO_LAYER3)
									ok = 1;
							}

#endif
							Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_RECT;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    CheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                         Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    CheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                         Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_TEXT:
					case OBJECT_TEXT2:
						TextP = (LPSTR) Object4->TraceNr;
#ifdef _DEBUG

						if (stricmpOwn(TextP, "info1_0") == 0)
							ok = 1;

#endif

						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Test = Object->Info2;
						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************


int32 GerberPrepareComponentOutlineObjects(int32 Layer, int32 CompOutlineLayers, int32 mode)
{
	int32 cnt, cnt2;
	double SpecialLineThickNess;
	CompRecord *Comp;
	LPSTR TextP;
	ObjectRecord *Object, *Object4;
	int32 PowerPlaneLayer;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			/*
			      CompOutlineOnBottom=0;
			      if (CompOutlineLayers==2) {
			        if (GetComponentPinLayer(Comp)==0) {
			#ifdef _DEBUG
			          if (Layer==COMP_OUTLINE_LAYER+1) {
			            ok=1;
			          }
			#endif
			          CompOutlineOnBottom=1;
			        }
			      }
			*/
			NrObjects = 0;
			ShapeCompOutLineToObject(Comp, 0.0, 0.0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if (GerberInfo.Invert)
					{
						switch (Object->ObjectType)
						{
						case OBJECT_POLYGON:
						case PIN_SMD_POLYGON:
							if ((Object->ObjectType2 == 0) || ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED))
							{
								Object->Mirror ^= 1;
								Object->RotationAngle = -Object->RotationAngle;

								if (Object->RotationAngle < 0.0)
									Object->RotationAngle += 360.0;
							}

							break;

						case PIN_PUT_THROUGH_POLYGON:
							Object->Mirror ^= 1;
							Object->RotationAngle = -Object->RotationAngle;

							if (Object->RotationAngle < 0.0)
								Object->RotationAngle += 360.0;

							break;
						}
					}

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memmove(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
							Object4->Info2 = CheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
							Object4->Info2 = CheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
							Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_RECT;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    CheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                         Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    CheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                         Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_TEXT:
					case OBJECT_TEXT2:
						TextP = (LPSTR) Object4->TraceNr;
#ifdef _DEBUG

						if (stricmpOwn(TextP, "info1_0") == 0)
							ok = 1;

#endif

						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Test = Object->Info2;
						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareComponentBoardOutlineObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, PowerPlaneLayer;
	double SpecialLineThickNess;
	CompRecord *Comp;
	ObjectRecord *Object, *Object4;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 3);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memmove(Object4, Object, sizeof(ObjectRecord));
					Object4->Info = 0;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case OBJECT_LINE:
						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareComponentRoutingKeepoutObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2;
	double SpecialLineThickNess;
	CompRecord *Comp;
	ObjectRecord *Object, *Object4;
	int32 PowerPlaneLayer;
#ifdef _DEBUG
	int32 ok;
#endif

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 4);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if (GerberInfo.Invert)
					{
						switch (Object->ObjectType)
						{
						case OBJECT_POLYGON:
						case PIN_SMD_POLYGON:
							if ((Object->ObjectType2 == 0) || ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED))
							{
								Object->Mirror ^= 1;
								Object->RotationAngle = -Object->RotationAngle;

								if (Object->RotationAngle < 0.0)
									Object->RotationAngle += 360.0;
							}

							break;

						case PIN_PUT_THROUGH_POLYGON:
							Object->Mirror ^= 1;
							Object->RotationAngle = -Object->RotationAngle;

							if (Object->RotationAngle < 0.0)
								Object->RotationAngle += 360.0;

							break;
						}
					}

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memmove(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case OBJECT_ARC:
						Object4->Info2 = CheckRoundPadAperTure(Object->x2);
						Object4->ObjectType = PIN_SMD_ROUND;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->y2);
						Object4->ObjectType = PIN_SMD_RECT;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    CheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                         Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    CheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                         Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareComponentSilkScreenObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, ShapeNr, MemPos, Mirror, Layer2 = 0, PowerPlaneLayer, OkToAddObject;
	double SpecialLineThickNess;
	LPSTR TextP;
	CompRecord *Comp;
	ShapeRecord *Shape;
	ObjectRecord *Object, *Object4, NewObject;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	switch (Layer)
	{
	case SILKSCREEN_BOTTOM:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_REFS:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_REFS:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_VALUES:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_VALUES:
		Layer2 = SILKSCREEN_TOP;
		break;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			NrObjects = 0;
			ShapeCompSilkScreenToObject(Comp, 0.0, 0.0, 0);
			ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, -1, 5);
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "Z100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer2)
				{
					if (GerberInfo.Invert)
					{
						switch (Object->ObjectType)
						{
						case OBJECT_POLYGON:
						case PIN_SMD_POLYGON:
							if ((Object->ObjectType2 == 0) || ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED))
							{
								Object->Mirror ^= 1;
								Object->RotationAngle = -Object->RotationAngle;

								if (Object->RotationAngle < 0.0)
									Object->RotationAngle += 360.0;
							}

							break;

						case PIN_PUT_THROUGH_POLYGON:
							Object->Mirror ^= 1;
							Object->RotationAngle = -Object->RotationAngle;

							if (Object->RotationAngle < 0.0)
								Object->RotationAngle += 360.0;

							break;
						}
					}

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memmove(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Clearance = 0.0;
					Object4->Test = 0;
					SpecialLineThickNess = Object->Thickness;

					switch (Object->ObjectType)
					{
					case OBJECT_LINE:
						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
							Object4->Thickness = SpecialLineThickNess;
						}
						else
							Object4->Info2 = CheckRectPadAperTure(Object->x2, Object->y2);

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							SpecialLineThickNess = Object4->Thickness;

							if (PenPlotMode)
								CheckPenPlotSize(&SpecialLineThickNess);

							Object4->Thickness = SpecialLineThickNess;
							Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
							Object4->Info2 = CheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_TEXT:
					case OBJECT_TEXT2:
						TextP = (LPSTR) Object4->TraceNr;
#ifdef _DEBUG

						if (stricmpOwn(TextP, "info1_0") == 0)
							ok = 1;

#endif

						if (PenPlotMode)
							CheckPenPlotSize(&SpecialLineThickNess);

						Object4->Test = Object->Info2;
						Object4->Info2 = CheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    CheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                         Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = CheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    CheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                         Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}
				}
			}

// ****************************************************************************
			ShapeNr = Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "z101") == 0)
			{
				ok = 1;

				if (Layer == SILKSCREEN_BOTTOM_REFS)
					ok = 1;
			}

			if (Layer == SILKSCREEN_TOP_VALUES)
				ok = 1;

#endif
			memset(&NewObject, 0, sizeof(NewObject));
			OkToAddObject = 1;
			Mirror = ((Comp->CompMode & 8) >> 3);

			switch (Layer)
			{
			case SILKSCREEN_BOTTOM_REFS:
				if (((Comp->TextVisibility & 0x100) != 0)	// Reference
				        || (!Mirror) || ((Comp->TextVisibility & 0x8) == 0)
				        || (MakeObjectFromCompRef(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;

			case SILKSCREEN_TOP_REFS:
				if (((Comp->TextVisibility & 0x100) != 0)	// Reference
				        || (Mirror) || (Comp->TextVisibility & 0x8) || (MakeObjectFromCompRef(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;

			case SILKSCREEN_BOTTOM_VALUES:
				if (((Comp->TextVisibility & 0x200) != 0)	// value
				        || (!Mirror) || ((Comp->TextVisibility & 0x80) == 0)
				        || (MakeObjectFromCompValue(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;

			case SILKSCREEN_TOP_VALUES:
				if (((Comp->TextVisibility & 0x200) != 0)	// value
				        || (Mirror) || (Comp->TextVisibility & 0x80) || (MakeObjectFromCompValue(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;
			}

			if (OkToAddObject)
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				memmove(Object4, &NewObject, sizeof(ObjectRecord));
				SpecialLineThickNess = Object4->Thickness;

				if (PenPlotMode)
					CheckPenPlotSize(&SpecialLineThickNess);

				Object4->Info2 = CheckTraceAperTure(SpecialLineThickNess);
				Object4->Thickness = SpecialLineThickNess;
				Object4->Info = 0;
				FillPositionObject(Object4);
				NrObjects4++;
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberPrepareSpecialObjects(int32 Layer, int32 mode)
{
	int32 cnt, Layer2;
	double SpecialLineThickNess;
	ObjectRecord *Object4;
	int32 PowerPlaneLayer;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
#ifdef _DEBUG
	int32 ok;
#endif

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	switch (Layer)
	{
	case SILKSCREEN_BOTTOM:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_REFS:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_REFS:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_VALUES:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_VALUES:
		Layer2 = SILKSCREEN_TOP;
		break;

	default:
		Layer2 = Layer;
		break;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((ObjectLine->Layer == Layer2)
			        || ((ObjectLine->Layer == BOARD_OUTLINE_LAYER) && (GerberInfo.PlotBoardOutline)))
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				Object4->x1 = ObjectLine->X1;
				Object4->y1 = ObjectLine->Y1;
				Object4->x2 = ObjectLine->X2;
				Object4->y2 = ObjectLine->Y2;
				Object4->Info = 0;
				Object4->Clearance = 0.0;
				Object4->Layer = ObjectLine->Layer;
				Object4->ObjectType = OBJECT_LINE;
				Object4->Thickness = ObjectLine->LineThickNess;
				SpecialLineThickNess = ObjectLine->LineThickNess;

				if (PenPlotMode)
					CheckPenPlotSize(&SpecialLineThickNess);

				Object4->Info2 = CheckTraceAperTure(SpecialLineThickNess);
				Object4->Test = ObjectLine->LineMode;
				Object4->Thickness = SpecialLineThickNess;
				FillPositionObject(Object4);
				NrObjects4++;
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((ObjectRect->Layer == Layer2)
			        || ((ObjectRect->Layer == BOARD_OUTLINE_LAYER) && ((ObjectRect->Info & OBJECT_FILLED) == 0)
			            && (GerberInfo.PlotBoardOutline)))
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				Object4->x1 = ObjectRect->CentreX;
				Object4->y1 = ObjectRect->CentreY;
				Object4->x2 = ObjectRect->Width;
				Object4->y2 = ObjectRect->Height;
				Object4->Thickness = ObjectRect->LineThickNess;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->Clearance = 0.0;
				Object4->Layer = ObjectRect->Layer;
				SpecialLineThickNess = ObjectRect->LineThickNess;
#ifdef _DEBUG

				if ((InRange9(Object4->x1, -88.7e5)) && (InRange9(Object4->y1, 85.7e5)))
					ok = 1;

#endif

				if ((ObjectRect->Info & OBJECT_FILLED) == 0)
				{
					Object4->ObjectType = OBJECT_RECT;

					if (PenPlotMode)
						CheckPenPlotSize(&SpecialLineThickNess);

					Object4->Info2 = CheckTraceAperTure(SpecialLineThickNess);
					Object4->Thickness = SpecialLineThickNess;
				}
				else
				{
					Object4->Info = OBJECT_FILLED;
					Object4->ObjectType = PIN_SMD_RECT;
#ifdef _DEBUG

					if ((InRange4(ObjectRect->Width, 170 * 2540.0)) && (InRange4(ObjectRect->Height, 75 * 2540.0)))
					{
						ok = 1;

						if (Layer == INFO_LAYER3)
							ok = 1;
					}

#endif
					Object4->Info2 = CheckRectPadAperTure(ObjectRect->Width, ObjectRect->Height);
				}

				FillPositionObject(Object4);
				NrObjects4++;
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((ObjectArc->Layer == Layer2)
			        || ((ObjectArc->Layer == BOARD_OUTLINE_LAYER) && ((ObjectArc->Info & OBJECT_FILLED) == 0)
			            && (GerberInfo.PlotBoardOutline)))
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				Object4->x1 = ObjectArc->CentreX;
				Object4->y1 = ObjectArc->CentreY;
				Object4->x2 = ObjectArc->Width;
				Object4->y2 = ObjectArc->Height;
				Object4->x3 = ObjectArc->StartDiffX;
				Object4->y3 = ObjectArc->StartDiffY;
				Object4->x4 = ObjectArc->EndDiffX;
				Object4->y4 = ObjectArc->EndDiffY;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->TraceNr = -1;
				Object4->Layer = ObjectArc->Layer;
				Object4->Clearance = 0.0;
				Object4->ObjectType = OBJECT_ARC;
				Object4->Thickness = ObjectArc->LineThickNess;
				SpecialLineThickNess = ObjectArc->LineThickNess;

				if ((ObjectArc->Info & OBJECT_FILLED) == 0)
				{
					if (PenPlotMode)
						CheckPenPlotSize(&SpecialLineThickNess);

					Object4->Info2 = CheckTraceAperTure(SpecialLineThickNess);
					Object4->Thickness = SpecialLineThickNess;
				}
				else
				{
					Object4->ObjectType = PIN_SMD_ROUND;
					Object4->Info2 = CheckRoundPadAperTure(ObjectArc->Width);
					Object4->Info = OBJECT_FILLED;
				}

				FillPositionObject(Object4);
				NrObjects4++;
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(ObjectText2->Text, "info1_0") == 0)
				ok = 1;

#endif

			if (ObjectText2->Layer == Layer2)
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				Object4->x1 = ObjectText2->X;
				Object4->y1 = ObjectText2->Y;
				Object4->x2 = ObjectText2->FontHeight;
				Object4->y2 = 0.0;
				Object4->Thickness = ObjectText2->LineThickNess;
				Object4->Info = 0;
				Object4->Layer = ObjectText2->Layer;
				Object4->Test = 0;
				Object4->ObjectType = OBJECT_TEXT;
				Object4->RotationAngle = ObjectText2->Rotation;
				Object4->Mirror = (ObjectText2->TextMode & 0x10) >> 4;
				Object4->Test = ObjectText2->TextMode + (ObjectText2->FontNr << 16);
				Object4->TraceNr = (int32) & (ObjectText2->Text);
				SpecialLineThickNess = ObjectText2->LineThickNess;

				if (PenPlotMode)
					CheckPenPlotSize(&SpecialLineThickNess);

				Object4->Info2 = CheckTraceAperTure(SpecialLineThickNess);
				Object4->Thickness = SpecialLineThickNess;
				FillPositionObject(Object4);
				NrObjects4++;
			}
		}
	}

	return 0;
}


// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
