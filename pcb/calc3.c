/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc3.c
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
#include "trace2.h"
#include "line2.h"
#include "files2.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "calc.h"
#include "math.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "polygon.h"
#include "move3.h"



int32 NrCalcs1, NrCalcs2, NrCalcs3, ok;


extern int Plotfp, OkToDrawPolygon;
extern PolygonRecord PolygonBuf;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectOverlapped(ObjectRecord * Object, int32 mode)
{
	int32 cnt, cnt2, Layer, StartLayer, EndLayer, ObjectNetNr, ok, ObjectLayer;
	double ObjectClearance, TestClearance, xx1, yy1, xx2, yy2, xx2a, yy2a, Thickness;
	ObjectRecord CheckObject, *CheckObject2;
	TraceRecord *Trace;
	ViaRecord *Via;
	CompRecord *Comp;
	NetRecord *Net;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectPolygonRecord *ObjectPolygon;
	AreaFillRecord *AreaFill;
	uint8 PolygonBuf2[10240];
	PolygonRecord *PolygonObject;

	memset(&OverlapObject, 0, sizeof(ObjectRecord));
	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	NrFoundObjects1 = 0;
	NrFoundObjects2 = 0;

	ObjectLayer = Object->Layer;
	StartLayer = ObjectLayer;
	EndLayer = ObjectLayer + 1;

	if (ObjectLayer == -1)
	{
		StartLayer = 0;
		EndLayer = 31;
	}

	ObjectClearance = Object->Clearance;
	ObjectNetNr = Object->NetNr;

	if (ObjectNetNr >= 0)
		Net = &((*Nets)[ObjectNetNr]);

// ****************************************************************************************
// ****************************************************************************************
// ****************************************************************************************
// ****************************************************************************************

	for (Layer = StartLayer; Layer < EndLayer; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				TestClearance = max(ObjectClearance, Trace->Clearance) * 2 - 10.0;
				xx1 = Trace->X;
				yy2a = Trace->ThickNess * 0.5 + TestClearance;

				if ((xx1 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx))
				{
					CheckObject.x1 = Trace->X;
					CheckObject.y1 = Trace->Y;
					CheckObject.x2 = Trace->Length;
					CheckObject.y2 = Trace->ThickNess + TestClearance;
					CheckObject.ObjectType = TRACE_VER;
					CheckObject.Layer = Layer;

					if (ObjectsConnected(Object, &CheckObject))
					{
						if (Trace->NetNr != ObjectNetNr)
						{
							memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
							OverlapObject.TraceNr = cnt;
							return 1;
						}
						else
						{
							if (mode == 0)
							{
								if (NrFoundObjects1 < MaxFoundObjects)
								{
									memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
									FoundObjects1[NrFoundObjects1].TraceNr = cnt;
									NrFoundObjects1++;
								}
							}
							else
							{
								if (NrFoundObjects2 < MaxFoundObjects)
								{
									memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
									FoundObjects2[NrFoundObjects2].TraceNr = cnt;
									NrFoundObjects2++;
								}
							}
						}
					}
				}
			}
		}

// ****************************************************************************************

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				TestClearance = max(ObjectClearance, Trace->Clearance) * 2 - 10.0;
				yy1 = Trace->Y;
				yy2a = Trace->ThickNess * 0.5 + TestClearance;
#ifdef _DEBUG

				if ((Trace->X > 30.4e5) && (Trace->X < 30.5e5) && (Trace->Y > 57.1e5) && (Trace->Y < 57.2e5))
					ok = 1;

#endif

				if ((yy1 + yy2a >= Object->miny) && (yy1 - yy2a <= Object->maxy))
				{
					CheckObject.x1 = Trace->X;
					CheckObject.y1 = Trace->Y;
					CheckObject.x2 = Trace->Length;
					CheckObject.y2 = Trace->ThickNess + TestClearance;
					CheckObject.ObjectType = TRACE_HOR;
					CheckObject.Layer = Layer;

//            if (cnt==9) {
//              ok=1;
//            }
					if (ObjectsConnected(Object, &CheckObject))
					{
						if (Trace->NetNr != ObjectNetNr)
						{
							memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
							OverlapObject.TraceNr = cnt;
							return 1;
						}
						else
						{
							if (mode == 0)
							{
								if (NrFoundObjects1 < MaxFoundObjects)
								{
									memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
									FoundObjects1[NrFoundObjects1].TraceNr = cnt;
									NrFoundObjects1++;
								}
							}
							else
							{
								if (NrFoundObjects2 < MaxFoundObjects)
								{
									memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
									FoundObjects2[NrFoundObjects2].TraceNr = cnt;
									NrFoundObjects2++;
								}
							}
						}
					}
				}
			}
		}

// ***************************************************************************************

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				TestClearance = max(ObjectClearance, Trace->Clearance) * 2 - 10.0;
				xx1 = Trace->X;
				xx2 = xx1 + Trace->Length;
				yy2a = Trace->ThickNess * 0.5 + TestClearance;

				if ((xx2 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx))
				{
					CheckObject.x1 = Trace->X;
					CheckObject.y1 = Trace->Y;
					CheckObject.x2 = Trace->Length;
					CheckObject.y2 = Trace->ThickNess + TestClearance;
					CheckObject.ObjectType = TRACE_DIAG1;
					CheckObject.Layer = Layer;

					if (ObjectsConnected(Object, &CheckObject))
					{
						if (Trace->NetNr != ObjectNetNr)
						{
							memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
							OverlapObject.TraceNr = cnt;
							return 1;
						}
						else
						{
							if (mode == 0)
							{
								if (NrFoundObjects1 < MaxFoundObjects)
								{
									memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
									FoundObjects1[NrFoundObjects1].TraceNr = cnt;
									NrFoundObjects1++;
								}
							}
							else
							{
								if (NrFoundObjects2 < MaxFoundObjects)
								{
									memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
									FoundObjects2[NrFoundObjects2].TraceNr = cnt;
									NrFoundObjects2++;
								}
							}
						}
					}
				}
			}
		}

// ***************************************************************************************

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				TestClearance = max(ObjectClearance, Trace->Clearance) * 2 - 10.0;
				xx1 = Trace->X;
				xx2 = xx1 + Trace->Length;
				yy2a = Trace->ThickNess * 0.5 + TestClearance;

				if ((xx2 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx))
				{
					CheckObject.x1 = Trace->X;
					CheckObject.y1 = Trace->Y;
					CheckObject.x2 = Trace->Length;
					CheckObject.y2 = Trace->ThickNess + TestClearance;
					CheckObject.ObjectType = TRACE_DIAG2;
					CheckObject.Layer = Layer;

					if ((Layer == 3) && (cnt == 2637))
						ok = 1;

					if (ObjectsConnected(Object, &CheckObject))
					{
						if (Trace->NetNr != ObjectNetNr)
						{
							memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
							OverlapObject.TraceNr = cnt;
							return 1;
						}
						else
						{
							if (mode == 0)
							{
								if (NrFoundObjects1 < MaxFoundObjects)
								{
									memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
									FoundObjects1[NrFoundObjects1].TraceNr = cnt;
									NrFoundObjects1++;
								}
							}
							else
							{
								if (NrFoundObjects2 < MaxFoundObjects)
								{
									memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
									FoundObjects2[NrFoundObjects2].TraceNr = cnt;
									NrFoundObjects2++;
								}
							}
						}
					}
				}
			}
		}
	}

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			TestClearance = max(ObjectClearance, Via->Clearance) * 2 - 10.0;
			xx1 = Via->X;
			yy1 = Via->Y;
			yy2a = Via->ThickNess * 0.5 + TestClearance;

			if ((xx1 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx) && (yy1 + yy2a >= Object->miny)
			        && (yy1 - yy2a <= Object->maxy))
			{
				CheckObject.x1 = Via->X;
				CheckObject.y1 = Via->Y;
				CheckObject.x2 = Via->ThickNess + TestClearance;
				CheckObject.ObjectType = VIA_PUT_THROUGH_ROUND;
				CheckObject.Layer = ObjectLayer;

				if (ObjectsConnected(Object, &CheckObject))
				{
					if (Via->NetNr != ObjectNetNr)
					{
						memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
						OverlapObject.TraceNr = cnt;
						return 1;
					}
					else
					{
						if (mode == 0)
						{
							if (NrFoundObjects1 < MaxFoundObjects)
							{
								memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
								FoundObjects1[NrFoundObjects1].TraceNr = cnt;
								NrFoundObjects1++;
							}
						}
						else
						{
							if (NrFoundObjects2 < MaxFoundObjects)
							{
								memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
								FoundObjects2[NrFoundObjects2].TraceNr = cnt;
								NrFoundObjects2++;
							}
						}
					}
				}
			}
		}
	}

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			TestClearance = max(ObjectClearance, Comp->PinMaximumClearance);
			FillPositionObjectWithClearance(Object, TestClearance);

			if ((Object->maxx >= Comp->BoardPosMinX - 0.1e5) && (Object->minx <= Comp->BoardPosMaxX + 0.1e5)
			        && (Object->maxy >= Comp->BoardPosMinY - 0.1e5) && (Object->miny <= Comp->BoardPosMaxY + 0.1e5))
			{

				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
				ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 4);	// Keepout

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					CheckObject2 = &((*Objects)[cnt2]);
					TestClearance = max(ObjectClearance, CheckObject2->Clearance) * 2 - 10.0;

					if ((CheckObject2->Layer >= ROUTING_KEEPOUT_LAYER)
					        && (CheckObject2->Layer < ROUTING_KEEPOUT_LAYER + 32))
					{
						CheckObject2->Layer -= ROUTING_KEEPOUT_LAYER;
						CheckObject2->NetNr = -2;

						switch (CheckObject2->ObjectType)
						{
						case OBJECT_ARC:
							if (CheckObject2->Thickness == 0.0)
								CheckObject2->ObjectType = PIN_SMD_ROUND;

							break;

						case OBJECT_RECT:
							if (CheckObject2->Thickness == 0.0)
								CheckObject2->ObjectType = PIN_SMD_RECT;

							break;
						}
					}

					switch (CheckObject2->ObjectType)
					{
					case PIN_PUT_THROUGH_ROUND:
					case VIA_PUT_THROUGH_ROUND:
					case PIN_SMD_ROUND:
					case DRILL_UNPLATED:
					case DRILL:
					case PIN_PUT_THROUGH_SQUARE:
						CheckObject2->x2 += TestClearance;
						break;

					case PIN_SMD_RECT:
						CheckObject2->x2 += TestClearance;
						CheckObject2->y2 += TestClearance;
						break;

					case PIN_LINE_HOR:
					case PIN_LINE_VER:
					case PIN_LINE_DIAG1:
					case PIN_LINE_DIAG2:
						CheckObject2->y2 += TestClearance;
						break;

					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						CheckObject2->Thickness += TestClearance;
						break;

					case OBJECT_ARC:
					case PIN_ARC:
						CheckObject2->Thickness += TestClearance;
						break;

					case PIN_SMD_POLYGON:
					case OBJECT_POLYGON:
					case PIN_PUT_THROUGH_POLYGON:
						CheckObject2->x3 = CheckObject2->Clearance;
						CheckObject2->Info |= OBJECT_WITH_CLEARANCE;
						CheckObject2->Clearance = TestClearance;
						CheckObject2->ObjectType = OBJECT_POLYGON;
						break;

					default:
						ok = 1;
						break;
					}

//          if ((cnt==21)
//             &&
//             (cnt2==0)) {
//            ok=1;
//          }
					if (ObjectsConnected(Object, CheckObject2))
					{
						switch (CheckObject2->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:
						case VIA_PUT_THROUGH_ROUND:
						case PIN_SMD_ROUND:
						case DRILL_UNPLATED:
						case DRILL:
						case PIN_PUT_THROUGH_SQUARE:
							CheckObject2->x2 -= TestClearance;
							break;

						case PIN_SMD_RECT:
							CheckObject2->x2 -= TestClearance;
							CheckObject2->y2 -= TestClearance;
							break;

						case PIN_LINE_HOR:
						case PIN_LINE_VER:
						case PIN_LINE_DIAG1:
						case PIN_LINE_DIAG2:
							CheckObject2->y2 -= TestClearance;
							break;

						case PIN_LINE_ALL_ANGLE:
						case PIN_ARC:
						case OBJECT_ARC:
						case OBJECT_LINE:
							CheckObject2->Thickness -= TestClearance;
							break;

						case PIN_SMD_POLYGON:
						case PIN_PUT_THROUGH_POLYGON:
						case OBJECT_POLYGON:
							CheckObject2->Clearance = CheckObject2->x3;
							CheckObject2->x3 = 0.0;
							CheckObject2->Info &= ~OBJECT_WITH_CLEARANCE;
							break;

						default:
							ok = 1;
							break;
						}

						ok = 1;

						if (CheckObject2->NetNr != ObjectNetNr)
						{
							memmove(&OverlapObject, CheckObject2, sizeof(ObjectRecord));
							return 1;
						}
						else
						{
							if (mode == 0)
							{
								if (NrFoundObjects1 < MaxFoundObjects)
								{
									memmove(&FoundObjects1[NrFoundObjects1], CheckObject2, sizeof(ObjectRecord));
									FoundObjects1[NrFoundObjects1].TraceNr = 0;
									NrFoundObjects1++;
								}
							}
							else
							{
								if (NrFoundObjects2 < MaxFoundObjects)
								{
									memmove(&FoundObjects2[NrFoundObjects2], CheckObject2, sizeof(ObjectRecord));
									FoundObjects2[NrFoundObjects2].TraceNr = 0;
									NrFoundObjects2++;
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
	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->Layer < 32))
		{
			TestClearance = max(ObjectClearance, ObjectLine->Clearance) * 2 - 10.0;
			xx1 = ObjectLine->X1;
			yy1 = ObjectLine->Y1;
			xx2 = ObjectLine->X2;
			yy2 = ObjectLine->Y2;
			yy2a = (ObjectLine->LineThickNess) * 0.5 + TestClearance;

			if ((max(xx1, xx2) + yy2a >= Object->minx) && (min(xx1, xx2) - yy2a <= Object->maxx)
			        && (max(yy1, yy2) + yy2a >= Object->miny) && (min(yy1, yy2) - yy2a <= Object->maxy))
			{
				CheckObject.x1 = ObjectLine->X1;
				CheckObject.y1 = ObjectLine->Y1;
				CheckObject.x2 = ObjectLine->X2;
				CheckObject.y2 = ObjectLine->Y2;
				CheckObject.Thickness = ObjectLine->LineThickNess;
				CheckObject.Thickness += TestClearance;
				CheckObject.ObjectType = TRACE_ALL_ANGLE;
				CheckObject.Layer = ObjectLayer;

				if (ObjectsConnected(Object, &CheckObject))
				{
					if (ObjectLine->NetNr != ObjectNetNr)
					{
						memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
						OverlapObject.TraceNr = cnt;
						return 1;
					}
					else
					{
						if (mode == 0)
						{
							if (NrFoundObjects1 < MaxFoundObjects)
							{
								memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
								FoundObjects1[NrFoundObjects1].TraceNr = cnt;
								NrFoundObjects1++;
							}
						}
						else
						{
							if (NrFoundObjects2 < MaxFoundObjects)
							{
								memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
								FoundObjects2[NrFoundObjects2].TraceNr = cnt;
								NrFoundObjects2++;
							}
						}
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		        && ((ObjectRect->Layer < 32)
		            || ((ObjectRect->Layer >= ROUTING_KEEPOUT_LAYER) && (ObjectRect->Layer < ROUTING_KEEPOUT_LAYER + 32))))
		{
			Thickness = ObjectRect->LineThickNess;
			TestClearance = max(ObjectClearance, ObjectRect->Clearance) * 2 - 10.0;
			xx1 = ObjectRect->CentreX;
			yy1 = ObjectRect->CentreY;
			xx2a = (ObjectRect->Width + Thickness) * 0.5 + TestClearance;
			yy2a = (ObjectRect->Height + Thickness) * 0.5 + TestClearance;

			if ((xx1 + xx2a >= Object->minx) && (xx1 - xx2a <= Object->maxx) && (yy1 + yy2a >= Object->miny)
			        && (yy1 - yy2a <= Object->maxy))
			{
				CheckObject.x1 = ObjectRect->CentreX;
				CheckObject.y1 = ObjectRect->CentreY;
				CheckObject.x2 = ObjectRect->Width;
				CheckObject.y2 = ObjectRect->Height;
				CheckObject.Thickness = Thickness;
				CheckObject.ObjectType = PIN_SMD_RECT;
				CheckObject.Layer = ObjectRect->Layer;

				if ((ObjectRect->Layer >= ROUTING_KEEPOUT_LAYER) && (ObjectRect->Layer < ROUTING_KEEPOUT_LAYER + 32))
				{
					CheckObject.Layer = ObjectRect->Layer - ROUTING_KEEPOUT_LAYER;
					CheckObject.x2 += TestClearance;
				}
				else
				{
					if ((ObjectRect->Info & OBJECT_FILLED) == OBJECT_FILLED)
						CheckObject.x2 += TestClearance;
					else
						CheckObject.Thickness += TestClearance;
				}

				if (ObjectsConnected(Object, &CheckObject))
				{
					if (ObjectRect->NetNr != ObjectNetNr)
					{
						memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
						OverlapObject.TraceNr = cnt;
						return 1;
					}
					else
					{
						if (mode == 0)
						{
							if (NrFoundObjects1 < MaxFoundObjects)
							{
								memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
								FoundObjects1[NrFoundObjects1].TraceNr = cnt;
								NrFoundObjects1++;
							}
						}
						else
						{
							if (NrFoundObjects2 < MaxFoundObjects)
							{
								memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
								FoundObjects2[NrFoundObjects2].TraceNr = cnt;
								NrFoundObjects2++;
							}
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

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		        && ((ObjectArc->Layer < 32)
		            || ((ObjectArc->Layer >= ROUTING_KEEPOUT_LAYER) && (ObjectArc->Layer < ROUTING_KEEPOUT_LAYER + 32))
		            || (ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER)))
		{
			Thickness = ObjectArc->LineThickNess;

			if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
				Thickness = 0.0;

			TestClearance = max(ObjectClearance, ObjectArc->Clearance) * 2 - 10.0;
			xx1 = ObjectArc->CentreX;
			yy1 = ObjectArc->CentreY;
			yy2a = (ObjectArc->Width + Thickness) * 0.5 + TestClearance;

			if ((xx1 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx) && (yy1 + yy2a >= Object->miny)
			        && (yy1 - yy2a <= Object->maxy))
			{
				CheckObject.x1 = ObjectArc->CentreX;
				CheckObject.y1 = ObjectArc->CentreY;
				CheckObject.x2 = ObjectArc->Width;
				CheckObject.y2 = ObjectArc->Height;
				CheckObject.x3 = ObjectArc->StartDiffX;
				CheckObject.y3 = ObjectArc->StartDiffY;
				CheckObject.x4 = ObjectArc->EndDiffX;
				CheckObject.y4 = ObjectArc->EndDiffY;
#ifdef _DEBUG

				if ((InRange9(CheckObject.x1, 182.9e5)) && (InRange9(CheckObject.y1, -6.6e5)))
					ok = 1;

#endif
				CheckObject.Thickness = Thickness;
				CheckObject.ObjectType = TRACE_ARC;

				if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
				{
					CheckObject.Layer = -1;
					CheckObject.ObjectType = DRILL;
					CheckObject.x2 += TestClearance;
				}
				else
				{
					if ((ObjectArc->Layer >= ROUTING_KEEPOUT_LAYER) && (ObjectArc->Layer < ROUTING_KEEPOUT_LAYER + 32))
					{
						CheckObject.Layer = ObjectArc->Layer - ROUTING_KEEPOUT_LAYER;
						CheckObject.NetNr = -2;
					}
					else
					{
						if (ObjectArc->Layer < 32)
							CheckObject.Layer = ObjectArc->Layer;
					}

					if ((ObjectArc->Info & OBJECT_FILLED) == OBJECT_FILLED)
					{
						CheckObject.ObjectType = PIN_SMD_ROUND;
						CheckObject.x2 += TestClearance;
					}
					else
						CheckObject.Thickness += TestClearance;
				}

				if (ObjectsConnected(Object, &CheckObject))
				{
					if (ObjectArc->NetNr != ObjectNetNr)
					{
						memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
						OverlapObject.TraceNr = cnt;
						return 1;
					}
					else
					{
						if (mode == 0)
						{
							if (NrFoundObjects1 < MaxFoundObjects)
							{
								memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
								FoundObjects1[NrFoundObjects1].TraceNr = cnt;
								NrFoundObjects1++;
							}
						}
						else
						{
							if (NrFoundObjects2 < MaxFoundObjects)
							{
								memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
								FoundObjects2[NrFoundObjects2].TraceNr = cnt;
								NrFoundObjects2++;
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

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		        && ((ObjectPolygon->Layer < 32)
		            || ((ObjectPolygon->Layer >= ROUTING_KEEPOUT_LAYER)
		                && (ObjectPolygon->Layer < ROUTING_KEEPOUT_LAYER + 32))))
		{
			TestClearance = ObjectClearance * 2 - 10.0;

			if ((ObjectPolygon->maxx + TestClearance >= Object->minx)
			        && (ObjectPolygon->minx - TestClearance <= Object->maxx)
			        && (ObjectPolygon->maxy + TestClearance >= Object->miny)
			        && (ObjectPolygon->miny - TestClearance <= Object->maxy))
			{
				if (ObjectPolygon->Layer < 32)
					CheckObject.Layer = ObjectPolygon->Layer;
				else
					CheckObject.Layer = ObjectPolygon->Layer - ROUTING_KEEPOUT_LAYER;

				CheckObject.ObjectType = OBJECT_POLYGON;
				CheckObject.TraceNr = cnt;
				CheckObject.ObjectType2 = 0;
				CheckObject.Address = 0;
				CheckObject.Thickness = 0.0;
				CheckObject.Info = OBJECT_WITH_CLEARANCE;
				CheckObject.Clearance = ObjectClearance;

				if (ObjectsConnected(Object, &CheckObject))
				{
					if (ObjectPolygon->NetNr != ObjectNetNr)
					{
						memmove(&OverlapObject, &CheckObject, sizeof(ObjectRecord));
						OverlapObject.TraceNr = cnt;
						return 1;
					}
					else
					{
						if (mode == 0)
						{
							if (NrFoundObjects1 < MaxFoundObjects)
							{
								memmove(&FoundObjects1[NrFoundObjects1], &CheckObject, sizeof(ObjectRecord));
								FoundObjects1[NrFoundObjects1].TraceNr = cnt;
								NrFoundObjects1++;
							}
						}
						else
						{
							if (NrFoundObjects2 < MaxFoundObjects)
							{
								memmove(&FoundObjects2[NrFoundObjects2], &CheckObject, sizeof(ObjectRecord));
								FoundObjects2[NrFoundObjects2].TraceNr = cnt;
								NrFoundObjects2++;
							}
						}
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	if (mode == 1)
	{
		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
			{
				if ((AreaFill->NetNr != ObjectNetNr) && ((Object->Layer == -1) || (AreaFill->Layer == Object->Layer)))
				{
					MakePolygonFromObject(Object, PolygonObject, AreaFill->Clearance, 0.0, 1, 0);

					if ((PolygonObject->minx < AreaFill->maxx) && (PolygonObject->miny < AreaFill->maxy)
					        && (PolygonObject->maxx > AreaFill->minx) && (PolygonObject->maxy > AreaFill->miny))
					{
						if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
							return 1;
					}
				}
			}
		}
	}


//  hulp=NrObjects2;
//  for (cnt=0;cnt<NrObjects2;cnt++) {
//    Object=&((*Objects2)[cnt]);
//    FillPositionObject(Object);
//    Object->Info=-1;
//  }

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectOverlappedFromObjects(ObjectRecord * Object, int32 mode)
{
	int32 cnt, NrCheckObjects, ObjectLayer, ObjectNetNr, ok;
	double ObjectClearance, TestClearance, xx1, yy1, xx2, yy2, xx2a, yy2a, CheckObjectX2, CheckObjectY2;
	ObjectRecord CheckObject, CheckObject2, *ObjectToCheck;

	ObjectToCheck = NULL;
	ObjectLayer = Object->Layer;
	ObjectClearance = Object->Clearance;
	ObjectNetNr = Object->NetNr;
	memmove(&CheckObject, Object, sizeof(ObjectRecord));
	CheckObject.ObjectType &= ~1;
	CheckObjectX2 = CheckObject.x2;
	CheckObjectY2 = CheckObject.y2;

	switch (mode & 7)
	{
	case 4:
		NrCheckObjects = NrObjects4;
		break;

	case 5:
		NrCheckObjects = NrObjects5;
		break;

	default:
		return -1;
	}

	for (cnt = 0; cnt < NrCheckObjects; cnt++)
	{
		ok = 1;

		switch (mode & 7)
		{
		case 4:
			ObjectToCheck = &((*Objects4)[cnt]);
			break;

		case 5:
			ObjectToCheck = &((*Objects5)[cnt]);
			break;
		}

		if ((ObjectToCheck->NetNr != Object->NetNr)
		        && ((ObjectToCheck->Layer == -1) || (ObjectLayer == -1) || (ObjectLayer == ObjectToCheck->Layer)))
		{
			switch (CheckObject.ObjectType)
			{
			case PIN_SMD_POLYGON:
				TestClearance = max(ObjectClearance, ObjectToCheck->Clearance) - 10.0;
				break;

			default:
				TestClearance = max(ObjectClearance, ObjectToCheck->Clearance) * 2 - 10.0;
				break;
			}

#ifdef _DEBUG

			if ((InRange9(ObjectToCheck->x1, 158.75e5)) && (InRange9(ObjectToCheck->y1, 101.66e5)))
				ok = 1;

#endif
			CheckObject.x2 = CheckObjectX2;
			CheckObject.y2 = CheckObjectY2;
			CheckObject.Thickness = Object->Thickness;

			switch (ObjectToCheck->ObjectType)
			{
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_SMD_RECT:
				break;

			default:
				switch (CheckObject.ObjectType)
				{
				case PIN_PUT_THROUGH_ROUND:
				case PIN_SMD_ROUND:
				case DRILL:
				case DRILL_UNPLATED:
				case VIA_PUT_THROUGH_ROUND:
					CheckObject.x2 += TestClearance;
					break;

				case PIN_PUT_THROUGH_SQUARE:
					CheckObject.x2 += TestClearance;
					break;

				case PIN_SMD_RECT:
					CheckObject.x2 += TestClearance;
					CheckObject.y2 += TestClearance;
					break;

				case TRACE_VER:
				case PIN_LINE_VER:
				case TRACE_HOR:
				case PIN_LINE_HOR:
				case TRACE_DIAG1:
				case PIN_LINE_DIAG1:
				case TRACE_DIAG2:
				case PIN_LINE_DIAG2:
					CheckObject.y2 += TestClearance;
					break;

				case PIN_LINE_ALL_ANGLE:
				case TRACE_ALL_ANGLE:
				case OBJECT_LINE:
					CheckObject.Thickness += TestClearance;
					break;

				case PIN_ARC:
				case TRACE_ARC:
				case OBJECT_ARC:
					CheckObject.Thickness += TestClearance;
					break;

				case PIN_PUT_THROUGH_POLYGON:
				case PIN_SMD_POLYGON:
				case OBJECT_POLYGON:
					CheckObject.x3 = CheckObject.Clearance;
					CheckObject.Info |= OBJECT_WITH_CLEARANCE;
					CheckObject.Clearance = TestClearance;
					break;

				default:
					ok = 1;
					break;
				}

				break;
			}

// *******************************************************************************************************
			switch (ObjectToCheck->ObjectType & ~1)
			{
			case TRACE_VER:
			case PIN_LINE_VER:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				xx2 = ObjectToCheck->x2;
				yy2a = ObjectToCheck->y2 * 0.5 + TestClearance;

				if ((xx1 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx) && (yy1 + xx2 + yy2a >= Object->miny)
				        && (yy1 - yy2a <= Object->maxy))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			case TRACE_HOR:
			case PIN_LINE_HOR:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				xx2 = ObjectToCheck->x2;
				yy2a = ObjectToCheck->y2 * 0.5 + TestClearance;

				if ((yy1 + yy2a >= Object->miny) && (yy1 - yy2a <= Object->maxy) && (xx1 + xx2 + yy2a >= Object->minx)
				        && (xx1 - yy2a <= Object->maxx))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			case TRACE_DIAG1:
			case PIN_LINE_DIAG1:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				xx2 = ObjectToCheck->x2;
				yy2a = ObjectToCheck->y2 * 0.5 + TestClearance;

				if ((xx1 + xx2 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx) && (yy1 + yy2a >= Object->miny)
				        && (yy1 - xx2 - yy2a <= Object->maxy))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			case TRACE_DIAG2:
			case PIN_LINE_DIAG2:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				xx2 = ObjectToCheck->x2;
				yy2a = ObjectToCheck->y2 * 0.5 + TestClearance;

				if ((xx1 + xx2 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx)
				        && (yy1 + xx2 + yy2a >= Object->miny) && (yy1 - yy2a <= Object->maxy))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			case PIN_LINE_ALL_ANGLE:
			case TRACE_ALL_ANGLE:
			case OBJECT_LINE:
				xx1 = min(ObjectToCheck->x1, ObjectToCheck->x2);
				yy1 = min(ObjectToCheck->y1, ObjectToCheck->y2);
				xx2 = max(ObjectToCheck->x1, ObjectToCheck->x2);
				yy2 = max(ObjectToCheck->y1, ObjectToCheck->y2);
				yy2a = ObjectToCheck->Thickness * 0.5 + TestClearance;

				if ((xx2 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx) && (yy2 + yy2a >= Object->miny)
				        && (yy1 - yy2a <= Object->maxy))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			case PIN_ARC:
			case TRACE_ARC:
			case OBJECT_ARC:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				xx2 = ObjectToCheck->x2 * 0.5;
				yy2 = ObjectToCheck->x2 * 0.5;
				yy2a = ObjectToCheck->Thickness * 0.5 + TestClearance;

				if ((xx1 + xx2 + yy2a >= Object->minx) && (xx1 - xx2 - yy2a <= Object->maxx)
				        && (yy1 + yy2 + yy2a >= Object->miny) && (yy1 - yy2 - yy2a <= Object->maxy))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			case PIN_PUT_THROUGH_ROUND:
			case PIN_SMD_ROUND:
			case DRILL:
			case DRILL_UNPLATED:
			case VIA_PUT_THROUGH_ROUND:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				yy2a = ObjectToCheck->x2 * 0.5 + TestClearance;

				if ((xx1 + yy2a >= Object->minx) && (xx1 - yy2a <= Object->maxx) && (yy1 + yy2a >= Object->miny)
				        && (yy1 - yy2a <= Object->maxy))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			case PIN_PUT_THROUGH_SQUARE:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				xx2a = ObjectToCheck->x2 * 0.5 + TestClearance;
				yy2a = ObjectToCheck->x2 * 0.5 + TestClearance;

				if ((xx1 + xx2a >= Object->minx) && (xx1 - xx2a <= Object->maxx) && (yy1 + yy2a >= Object->miny)
				        && (yy1 - yy2a <= Object->maxy))
				{
					memmove(&CheckObject2, ObjectToCheck, sizeof(ObjectRecord));
					CheckObject2.x2 += TestClearance;

					if (ObjectsConnected(&CheckObject2, &CheckObject))
						return cnt;
				}

				break;

			case PIN_SMD_RECT:
				xx1 = ObjectToCheck->x1;
				yy1 = ObjectToCheck->y1;
				xx2a = ObjectToCheck->x2 * 0.5 + TestClearance;
				yy2a = ObjectToCheck->y2 * 0.5 + TestClearance;

				if ((xx1 + xx2a >= Object->minx) && (xx1 - xx2a <= Object->maxx) && (yy1 + yy2a >= Object->miny)
				        && (yy1 - yy2a <= Object->maxy))
				{
					memmove(&CheckObject2, ObjectToCheck, sizeof(ObjectRecord));
					CheckObject2.x2 += TestClearance;
					CheckObject2.y2 += TestClearance;

					if (ObjectsConnected(&CheckObject2, &CheckObject))
						return cnt;
				}

				break;

			case PIN_PUT_THROUGH_POLYGON:
			case PIN_SMD_POLYGON:
			case OBJECT_POLYGON:
				if ((ObjectToCheck->maxx + TestClearance >= Object->minx)
				        && (ObjectToCheck->minx - TestClearance <= Object->maxx)
				        && (ObjectToCheck->maxy + TestClearance >= Object->miny)
				        && (ObjectToCheck->miny - TestClearance <= Object->maxy))
				{
					if (ObjectsConnected(ObjectToCheck, &CheckObject))
						return cnt;
				}

				break;

			default:
				ok = 1;
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

int32 ObjectIsTrace(ObjectRecord * Object)
{
	switch (Object->ObjectType)
	{
	case TRACE_HOR:
	case TRACE_VER:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
	case TRACE_ALL_ANGLE:
		return 1;

	default:
		return 0;
	}
}

int32 PointConnectedToCenterObject(double x, double y, ObjectRecord* Object)
{
	return;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 IsLayerPowerPlane(int32 Layer)
{
	int32 cnt, Found;
	AreaFillRecord *AreaFill;

	Found = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE) && (AreaFill->Layer == Layer))
			Found = cnt;
	}

	if (Found != -1)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetLayerText(int32 Layer, LPSTR TextStr, int32 mode)
{
	int32 Found, cnt, Layer2;
	AreaFillRecord *AreaFill;
	NetRecord *Net;

	if ((mode & 16) == 0)
		Layer2 = Layer;
	else
		Layer2 = Design.NrBoardLayers - Layer - 1;

	if (mode & 64)
		Layer2++;

	if (Layer == 0)
	{
		switch (mode & 15)
		{
		case 0:
			sprintf(TextStr, SC(3, "Layer %i (Bottom)"), Layer2); //export odb++, nazev v gerber bez diakritiky
			break;

		case 1:
			sprintf(TextStr, SC(4, "Layer %i\t\tBottom"), Layer2); //export gerber/vrtani, penplot, tisk, bitmap, pdf
			break;

		case 2:
			sprintf(TextStr, SC(5, "3Layer %i\tBottom"), Layer2);
			break;

		case 3:
			sprintf(TextStr, SC(6, "4(Bottom)"));
			break;

		case 4:
			sprintf(TextStr, SC(7, "Bottom %d"), Layer2); //info mysi
			break;

		case 5:
		case 6:
			sprintf(TextStr, SC(8, "Bottom layer %d"), Layer2); //barvy, kontext
			break;

		case 7:
			break;

		case 8:
			sprintf(TextStr, "Bottom_%d", Layer2); //export dxf, nazev souboru.ger
			break;
		}

		return 0;
	}

	if (Design.NrBoardLayers > 1)
	{
		if (Layer == Design.NrBoardLayers - 1)
		{
			switch (mode & 15)
			{
			case 0:
				sprintf(TextStr, SC(10, "Layer %i (Top)"), Layer2); //export odb++, nazev v gerber bez diakritiky
				break;

			case 1:
				sprintf(TextStr, SC(11, "Layer %i\t\tTop"), Layer2); //export gerber/vrtani, penplot, tisk, bitmap, pdf
				break;

			case 2:
				sprintf(TextStr, SC(12, "3Layer %i\tTop"), Layer2);
				break;

			case 3:
				sprintf(TextStr, SC(13, "4(Top)"));
				break;

			case 4:
				sprintf(TextStr, SC(14, "Top %d"), Layer2); //info mysi
				break;

			case 5:
			case 6:
				sprintf(TextStr, SC(15, "Top layer %d"), Layer2); //barvy, kontext
				break;

			case 7:
				break;

			case 8:
				sprintf(TextStr, "Top_%d", Layer2); //export dxf, nazev souboru.ger
				break;
			}

			return 0;
		}
	}
	else
		return 0;

	Found = -1;

	if ((Design.NrBoardLayers > 2) && (Layer > 0) && (Layer < Design.NrBoardLayers - 1))
	{
		if ((mode & 32) == 0)
		{
			for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

				if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE) && (AreaFill->Layer == Layer))
					Found = cnt;
			}
		}

		if (Found != -1)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);

			if (AreaFill->NetNr >= 0)
				Net = &((*Nets)[AreaFill->NetNr]);
			else
				Net = &EmptyNet;

			switch (mode & 15)
			{
			case 0:
				sprintf(TextStr, SC(17, "Layer %i ( Powerplane %s )"), Layer2, Net->Name); //vrstvy, kontext
				break;

			case 1:
				sprintf(TextStr, SC(18, "Layer %i\t\tInner\tPowerplane %s"), Layer2, Net->Name); //export gerber/vrtani, penplot, tisk, bitmap, pdf
				break;

			case 2:
				sprintf(TextStr, SC(19, "3Layer %i\tPowerplane %s"), Layer2, Net->Name);
				break;

			case 3:
				sprintf(TextStr, SC(20, "\tInner %i\tPowerplane %s"), Layer2, Net->Name); //export gerber/vrtani, penplot, tisk, bitmap, pdf
				break;

			case 4:
				sprintf(TextStr, SC(21, "Inner %i"), Layer2); //leve info dole
				break;

			case 5:
				sprintf(TextStr, SC(22, "Powerplane ( %s ) layer %i"), Net->Name, Layer2); //barvy, kontext
				break;

			case 6:
				sprintf(TextStr, SC(23, "Inner layer %i ( Powerplane %s )"), Layer2, Net->Name); //layers.txt
				break;

			case 8:
				sprintf(TextStr, "Inner_%i", Layer2); //export dxf, nazev souboru.ger
				break;
			}
		}
		else
		{
			switch (mode & 15)
			{
			case 0:
			case 1:
			case 2:
				sprintf(TextStr, SC(25, "Layer %i"), Layer2); //export odb++
				break;

			case 3:
				sprintf(TextStr, SC(26, "2( Inner layer %i )"), Layer2);
				break;

			case 4:
				sprintf(TextStr, SC(21, "3Inner %i"), Layer2);
				break;

			case 5:
			case 6:
				sprintf(TextStr, SC(27, "4Inner layer %i"), Layer2);
				break;

			case 7:
				break;

			case 8:
				sprintf(TextStr, "Inner_%i", Layer2); //export dxf, nazev souboru.ger
				break;
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetLayerTextObjects(int32 LayerObjectNr, LPSTR LayerText, int32 mode)
{
	int32 cnt, Layer, NewLayer;

#ifdef _DEBUG
	
	int32 ok;

	if (LayerObjectNr == ShapePinsTopObjectNr)
		ok = 1;
#endif
	
	Layer = LayerObjectNr;

	if ((mode & 0x100) == 0x100)
	{
		for (cnt = 0; cnt < NrLayerObjects; cnt++)
		{
			if (LayerObjectCodes[cnt].ColorNr == LayerObjectNr)
				Layer = LayerObjectCodes[cnt].Layer;
		}
	}

	LayerText[0] = 0;

	switch (Layer)
	{
	case SILKSCREEN_TOP:
		switch (mode & 0x0F) //0x0F
		{
		case 0:
			strcpy(LayerText, SC(28, "Silkscreen top")); //export odb++, název v gerber bez diakritiky
			break;
		
		case 3:
			strcpy(LayerText, SC(122, "Silkscreen top")); //layers.txt
			break;

		case 1:
			strcpy(LayerText, SC(1036, "Silkscreen\tTop")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "SilkscreenTop"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(122, "Silkscreen top")); //barvy, kontext
			break;

		case 2:
			strcpy(LayerText, SC(122, "6Silkscreen top"));
			break;
		}

		break;

	case SILKSCREEN_TOP_REFS:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(30, "Silkscreen refs top")); //export odb++, název v gerber bez diakritiky
			break;
		
		case 3:
			strcpy(LayerText, SC(30, "2Silkscreen refs top"));
			break;

		case 1:
			strcpy(LayerText, SC(1038, "Silkscreen\tTop\tRefs")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "SilkscreenRefsTop"); //název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(30, "5Silkscreen refs top"));
			break;

		case 2:
			strcpy(LayerText, SC(30, "6Silkscreen refs top"));
			break;
		}

		break;

	case SILKSCREEN_TOP_VALUES:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(32, "Silkscreen values top")); //export odb++, název v gerber bez diakritiky
			break;
		
		case 3:
			strcpy(LayerText, SC(32, "2Silkscreen values top"));
			break;

		case 1:
			strcpy(LayerText, SC(1039, "Silkscreen\tTop\tValues")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "SilkscreenValuesTop"); //název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(32, "5Silkscreen values top"));
			break;

		case 2:
			strcpy(LayerText, SC(32, "6Silkscreen values top"));
			break;
		}

		break;

	case SILKSCREEN_BOTTOM:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(29, "Silkscreen bottom")); //export odb++, název v gerber bez diakritiky
			break;
		
		case 3:
			strcpy(LayerText, SC(123, "Silkscreen bottom")); //layers.txt
			break;

		case 1:
			strcpy(LayerText, SC(1037, "Silkscreen\tBottom")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "SilkscreenBottom"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(123, "Silkscreen bottom")); //barvy, kontext
			break;

		case 2:
			strcpy(LayerText, SC(123, "6Silkscreen bottom"));
			break;
		}

		break;

	case SILKSCREEN_BOTTOM_REFS:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(31, "Silkscreen refs bottom")); //export odb++, název v gerber bez diakritiky
			break;
		
		case 3:
			strcpy(LayerText, SC(31, "2Silkscreen refs bottom"));
			break;

		case 1:
			strcpy(LayerText, SC(1040, "Silkscreen\tBottom\tRefs")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "SilkscreenRefsBottom"); //název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(31, "5Silkscreen refs bottom"));
			break;

		case 2:
			strcpy(LayerText, SC(31, "6Silkscreen refs bottom"));
			break;
		}

		break;

	case SILKSCREEN_BOTTOM_VALUES:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(33, "Silkscreen values bottom")); //export odb++, název v gerber bez diakritiky
			break;
		
		case 3:
			strcpy(LayerText, SC(33, "2Silkscreen values bottom"));
			break;

		case 1:
			strcpy(LayerText, SC(1041, "Silkscreen\tBottom\tValues")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "SilkscreenValuesBottom"); //název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(33, "5Silkscreen values bottom"));
			break;

		case 2:
			strcpy(LayerText, SC(33, "6Silkscreen values bottom"));
			break;
		}

		break;

	case INFO_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, "Info 1"); //export odb++, název v gerber bez diakritiky
			break;
		
		case 3:
			strcpy(LayerText, "Info 1"); //layers.txt
			break;

		case 1:
			strcpy(LayerText, "Info 1"); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "Info_1"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, "Info 1"); //barvy, kontext
			break;
		
		case 2:
			strcpy(LayerText, "6Info 1");
			break;
		}

		break;

	case INFO_LAYER2:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, "Info 2"); //export odb++, název v gerber bez diakritiky
			break;

		case 3:
			strcpy(LayerText, "Info 2"); //layers.txt
			break;

		case 1:
			strcpy(LayerText, "Info 2"); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "Info_2"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, "Info 2"); //barvy, kontext
			break;

		case 2:
			strcpy(LayerText, "6Info 2");
			break;
		}

		break;

	case INFO_LAYER3:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, "Info 3"); //export odb++, název v gerber bez diakritiky
			break;

		case 3:
			strcpy(LayerText, "Info 3"); //layers.txt
			break;

		case 1:
			strcpy(LayerText, "Info 3"); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "Info_3"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, "Info 3"); //barvy, kontext
			break;

		case 2:
			strcpy(LayerText, "6Info 3");
			break;
		}

		break;

	case INFO_LAYER4:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, "Info 4"); //export odb++, název v gerber bez diakritiky
			break;

		case 3:
			strcpy(LayerText, "Info 4"); //layers.txt
			break;

		case 1:
			strcpy(LayerText, "Info 4"); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 5:
			strcpy(LayerText, "Info_4"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, "Info 4"); //barvy, kontext
			break;

		case 2:
			strcpy(LayerText, "6Info 4");
			break;
		}

		break;

	case PLACEMENT_OUTLINE_TOP:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(34, "Placement outline top")); //export odb++
			break;
		
		case 3:
			strcpy(LayerText, SC(34, "2Placement outline top"));
			break;

		case 1:
			strcpy(LayerText, SC(34, "3Placement outline top"));
			break;

		case 5:
			strcpy(LayerText, "PlacementOutlineTop"); //export dxf
			break;

		case 4:
			strcpy(LayerText, SC(34, "Placement outline top")); //barvy
			break;

		case 2:
			strcpy(LayerText, SC(34, "6Placement outline top"));
			break;
		}

		break;

	case PLACEMENT_OUTLINE_BOTTOM:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(35, "Placement outline bottom")); //export odb++
			break;

		case 3:
			strcpy(LayerText, SC(35, "2Placement outline bottom"));
			break;

		case 1:
			strcpy(LayerText, SC(35, "3Placement outline bottom"));
			break;

		case 5:
			strcpy(LayerText, "PlacementOutlineBottom"); //export dxf
			break;

		case 4:
			strcpy(LayerText, SC(35, "Placement outline bottom")); //barvy
			break;

		case 2:
			strcpy(LayerText, SC(35, "6Placement outline bottom"));
			break;
		}

		break;

	case COMP_OUTLINE_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(48, "Component outline top")); //export odb++, název v gerber bez diakritiky
			break;

		case 1:
			strcpy(LayerText, SC(50, "Component outline\tTop")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 3:
			strcpy(LayerText, SC(124, "Component outline top")); //layers.txt
			break;

		case 2:
			strcpy(LayerText, SC(124, "4Component outline top"));
			break;

		case 4:
			strcpy(LayerText, SC(124, "Component outline top")); //barvy
			break;

		case 5:
			strcpy(LayerText, "ComponentOutlineTop"); //export dxf, název souboru.ger
			break;
		}

		break;

	case COMP_OUTLINE_LAYER + 1:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(49, "Component outline bottom")); //export odb++, název v gerber bez diakritiky
			break;

		case 1:
			strcpy(LayerText, SC(51, "Component outline\tBottom")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 3:
			strcpy(LayerText, SC(125, "Component outline bottom")); //layers.txt
			break;

		case 2:
			strcpy(LayerText, SC(125, "4Component outline bottom"));
			break;

		case 4:
			strcpy(LayerText, SC(125, "Component outline bottom")); //barvy
			break;

		case 5:
			strcpy(LayerText, "ComponentOutlineBottom"); //export dxf, název souboru.ger
			break;
		}

		break;

	case COMP_REF_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(46, "1Component references"));
			break;

		case 1:
			strcpy(LayerText, SC(46, "2Component references"));
			break;

		case 3:
			strcpy(LayerText, SC(46, "3Component references"));
			break;

		case 4:
			strcpy(LayerText, SC(46, "Component references")); //barvy
			break;

		case 2:
			strcpy(LayerText, SC(46, "5Component references"));
			break;
		}

		break;

	case COMP_VALUE_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(47, "1Component values"));
			break;

		case 1:
			strcpy(LayerText, SC(47, "2Component values"));
			break;

		case 3:
			strcpy(LayerText, SC(47, "3Component values"));
			break;

		case 4:
			strcpy(LayerText, SC(47, "Component values")); //barvy
			break;

		case 2:
			strcpy(LayerText, SC(47, "5Component values"));
			break;
		}

		break;

	case BOARD_OUTLINE_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(43, "Board outline")); //název v prohlížeèi gerber bez diakritiky
			break;

		case 1:
			strcpy(LayerText, SC(43, "Board outline")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 3:
			strcpy(LayerText, SC(43, "Board outline")); //layers.txt
			break;

		case 2:
			strcpy(LayerText, SC(43, "4Board outline"));
			break;

		case 5:
			strcpy(LayerText, "BoardOutline"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(43, "Board outline")); //barvy, kontext
			break;
		}

		break;

	case BOARD_OUTLINE_KEEPOUT_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(16, "1Board outline keep out"));
			break;

		case 1:
			strcpy(LayerText, SC(16, "2Board outline keep out"));
			break;

		case 3:
			strcpy(LayerText, SC(16, "3Board outline keep out"));
			break;

		case 2:
			strcpy(LayerText, SC(16, "4Board outline keep out"));
			break;

		case 5:
			strcpy(LayerText, "BoardOutlineKeepOut"); //název gerber
			break;

		case 4:
			strcpy(LayerText, SC(16, "Board outline keep out")); //barvy
			break;
		}

		break;

	case SOLD_MASK_BOTTOM:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(53, "Solder mask bottom")); //export odb++, název v gerber bez diakritiky
			break;

		case 3:
			strcpy(LayerText, SC(127, "Solder mask bottom")); //layers.txt
			break;

		case 1:
			strcpy(LayerText, SC(57, "Solder mask\tBottom")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 2:
			strcpy(LayerText, SC(127, "4Solder mask bottom"));
			break;

		case 5:
			strcpy(LayerText, "SolderMaskBottom"); //export dxf ,název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(127, "Solder mask bottom")); //barvy, kontext
			break;
		}

		break;

	case SOLD_MASK_TOP:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(52, "Solder mask top")); //export odb++, název v gerber bez diakritiky
			break;

		case 3:
			strcpy(LayerText, SC(126, "Solder mask top")); //layers.txt
			break;

		case 1:
			strcpy(LayerText, SC(56, "Solder mask\tTop")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 2:
			strcpy(LayerText, SC(126, "4Solder mask top"));
			break;

		case 5:
			strcpy(LayerText, "SolderMaskTop"); //export dxf ,název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(126, "Solder mask top")); //barvy, kontext
			break;
		}

		break;

	case PASTE_MASK_BOTTOM:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(55, "Paste mask bottom")); //export odb++, název v gerber bez diakritiky
			break;

		case 3:
			strcpy(LayerText, SC(129, "Paste mask bottom")); //layers.txt
			break;

		case 1:
			strcpy(LayerText, SC(59, "Paste mask\tBottom")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 2:
			strcpy(LayerText, SC(129, "4Paste mask bottom"));
			break;

		case 5:
			strcpy(LayerText, "PasteMaskBottom"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(129, "Paste mask bottom")); //barvy, kontext
			break;
		}

		break;

	case PASTE_MASK_TOP:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(54, "Paste mask top")); //export odb++, název v gerber bez diakritiky
			break;

		case 3:
			strcpy(LayerText, SC(128, "Paste mask top")); //layers.txt
			break;

		case 1:
			strcpy(LayerText, SC(58, "Paste mask\tTop")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 2:
			strcpy(LayerText, SC(128, "4Paste mask top"));
			break;

		case 5:
			strcpy(LayerText, "PasteMaskTop"); //export dxf, název souboru.ger
			break;

		case 4:
			strcpy(LayerText, SC(128, "Paste mask top")); //barvy, kontext
			break;
		}

		break;

	case ASSEMBLY_BOTTOM_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(1312, "1Assembly bottom"));
			break;

		case 3:
			strcpy(LayerText, SC(1312, "2Assembly bottom"));
			break;

		case 1:
			strcpy(LayerText, SC(1313, "3Assembly\tBottom"));
			break;

		case 2:
			strcpy(LayerText, SC(1312, "4Assembly bottom"));
			break;

		case 5:
			strcpy(LayerText, "AssemblyBottom"); //název gerber
			break;

		case 4:
			strcpy(LayerText, SC(1315, "6Assembly bottom"));
			break;
		}

		break;

	case ASSEMBLY_TOP_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(1316, "1Assembly top"));
			break;

		case 3:
			strcpy(LayerText, SC(1316, "2Assembly top"));
			break;

		case 1:
			strcpy(LayerText, SC(1317, "3Assembly\tTop"));
			break;

		case 2:
			strcpy(LayerText, SC(1316, "4Assembly top"));
			break;

		case 5:
			strcpy(LayerText, "AssemblyTop"); //název gerber
			break;

		case 4:
			strcpy(LayerText, SC(1319, "6Assembly top"));
			break;
		}

		break;

	case BOTTOM_COMP_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(2, "Components bottom")); //export odb++
			break;

		case 3:
			strcpy(LayerText, SC(2, "2Components bottom"));
			break;

		case 1:
			strcpy(LayerText, SC(1334, "3Components\tBottom"));
			break;

		case 2:
			strcpy(LayerText, SC(2, "4Components bottom"));
			break;

		case 5:
			strcpy(LayerText, "ComponentsBottom"); //název gerber
			break;

		case 4:
			strcpy(LayerText, SC(1336, "6Components bottom"));
			break;
		}

		break;

	case TOP_COMP_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(9, "Components top")); //export odb++
			break;

		case 3:
			strcpy(LayerText, SC(9, "2Components top"));
			break;

		case 1:
			strcpy(LayerText, SC(1338, "3Components\tTop"));
			break;

		case 2:
			strcpy(LayerText, SC(9, "4Components top"));
			break;

		case 5:
			strcpy(LayerText, "ComponentsTop"); //název gerber
			break;

		case 4:
			strcpy(LayerText, SC(1340, "6Components top"));
			break;
		}

		break;

	case DRILL_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(61, "Drills plated")); //export odb++
			break;

		case 1:
			strcpy(LayerText, SC(61, "Drills plated")); //export gerber/vrtání, penplot, tisk, bitmap, pdf
			break;

		case 2:
			strcpy(LayerText, SC(61, "3Drills plated"));
			break;

		case 3:
			strcpy(LayerText, SC(61, "4Drills plated"));
			break;

		case 4:
			strcpy(LayerText, SC(61, "Drills plated")); //barvy, kontext
			break;

		case 5:
			strcpy(LayerText, "DrillsPlated"); //export dxf, název souboru.ger
			break;
		}

		break;

	case DRILL_UNPLATED_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(62, "1Drills unplated"));
			break;

		case 1:
			strcpy(LayerText, SC(62, "2Drills unplated"));
			break;

		case 2:
			strcpy(LayerText, SC(62, "3Drills unplated"));
			break;

		case 3:
			strcpy(LayerText, SC(62, "4Drills unplated"));
			break;

		case 4:
			strcpy(LayerText, SC(62, "Drills unplated")); //barvy, kontext
			break;

		case 5:
			strcpy(LayerText, "DrillsUnplated"); //export dxf
			break;
		}

		break;

	case VIA_DRILL_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(63, "1Drill vias"));
			break;

		case 1:
			strcpy(LayerText, SC(63, "2Drill vias"));
			break;

		case 2:
			strcpy(LayerText, SC(63, "3Drill vias"));
			break;

		case 3:
			strcpy(LayerText, SC(63, "4Drill vias"));
			break;
		
		case 4:
			strcpy(LayerText, SC(63, "Drill vias")); //barvy
			break;
		}

		break;

	case SHAPE_PINS_BOTTOM:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(65, "1Component pins bottom"));
			break;
		
		case 1:
			strcpy(LayerText, SC(65, "2Component pins bottom"));
			break;

		case 2:
			strcpy(LayerText, SC(65, "3Component pins bottom"));
			break;

		case 3:
			strcpy(LayerText, SC(65, "4Component pins bottom"));
			break;

		case 4:
			strcpy(LayerText, SC(65, "Component pins bottom")); //barvy
			break;
		}

		break;

	case SHAPE_PINS_TOP:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(64, "1Component pins top"));
			break;

		case 1:
			strcpy(LayerText, SC(64, "2Component pins top"));
			break;

		case 2:
			strcpy(LayerText, SC(64, "3Component pins top"));
			break;

		case 3:
			strcpy(LayerText, SC(64, "4Component pins top"));
			break;

		case 4:
			strcpy(LayerText, SC(64, "Component pins top")); //barvy
			break;
		}

		break;

	case SHAPE_PINS_INNER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(66, "1Component pins inner"));
			break;

		case 1:
			strcpy(LayerText, SC(66, "2Component pins inner"));
			break;

		case 2:
			strcpy(LayerText, SC(66, "3Component pins inner"));
			break;

		case 3:
			strcpy(LayerText, SC(66, "4Component pins inner"));
			break;

		case 4:
			strcpy(LayerText, SC(66, "Component pins inner")); //barvy
			break;
		}

		break;

	case VIA_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(41, "1Vias"));
			break;

		case 1:
			strcpy(LayerText, SC(41, "2Vias"));
			break;

		case 2:
			strcpy(LayerText, SC(41, "3Vias"));
			break;

		case 3:
			strcpy(LayerText, SC(41, "4Vias"));
			break;

		case 4:
			strcpy(LayerText, SC(41, "Vias")); //barvy
			break;
		}

		break;

	case NET_PINS_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
			strcpy(LayerText, SC(67, "Net component pins")); //barvy
			break;
		}

		break;

	case NET_PINS_LAYER2:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
			strcpy(LayerText, SC(68, "Net component pins powerplane")); //barvy
			break;
		}

		break;

	case CONNECTIONS_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
			strcpy(LayerText, SC(69, "Guides ( Connections )")); //barvy
			break;
		}

		break;

	case GRID_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
			strcpy(LayerText, SC(70, "Grid")); //barvy
			break;
		}

		break;

	case SWAP_GATE_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 3:
		case 4:
			strcpy(LayerText, SC(71, "Swappable gate pins")); //barvy
			break;

		case 2:
			break;
		}

		break;

	case SWAP_PINS_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 3:
		case 4:
			strcpy(LayerText, SC(72, "Swappable pins")); //barvy
			break;

		case 2:
			break;
		}

		break;

	case BACKGROUND_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
			strcpy(LayerText, SC(73, "Background")); //barvy
			break;
		}

		break;

	case CLEARANCE_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
			strcpy(LayerText, SC(74, "Clearances"));
			break;

		case 2:
			break;

		case 3:
		case 4:
			strcpy(LayerText, SC(74, "Clearances")); //barvy
			break;
		}

		break;

	case ERROR_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
			strcpy(LayerText, SC(24, "Error"));
			break;

		case 3:
		case 4:
			strcpy(LayerText, SC(75, "Design rule errors")); //barvy
			break;
		}

		break;

	case WARNING_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
			strcpy(LayerText, SC(118, "Warning"));
			break;

		case 3:
		case 4:
			strcpy(LayerText, SC(117, "Design rule warnings"));
			break;
		}

		break;

	case BUTTON_INFO_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
			strcpy(LayerText, SC(76, "Button info"));
			break;

		case 2:
			break;

		case 3:
		case 4:
			strcpy(LayerText, SC(76, "Button info")); //barvy
			break;
		}

		break;

	case CROSS_HAIR_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
			strcpy(LayerText, SC(78, "Cross hair"));
			break;

		case 2:
			break;

		case 3:
		case 4:
			strcpy(LayerText, SC(78, "Cross hair")); //barvy
			break;
		}

		break;

	case POLYLINE_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
		case 2:
		case 3:
			strcpy(LayerText, SC(116, "Polyline"));
			break;
		}

		break;

	case UNCONNECTED_PADS_BOTTOM_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
			strcpy(LayerText, SC(79, "Unconnected pads bottom"));
			break;

		case 2:
			break;

		case 3:
			strcpy(LayerText, SC(79, "Unconnected pads bottom"));
			break;

		case 4:
			strcpy(LayerText, SC(79, "Unconnected pads bottom")); //barvy
			break;
		}

		break;

	case UNCONNECTED_PADS_TOP_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
			strcpy(LayerText, SC(80, "Unconnected pads top"));
			break;

		case 2:
			break;

		case 3:
			strcpy(LayerText, SC(80, "Unconnected pads top"));
			break;

		case 4:
			strcpy(LayerText, SC(80, "Unconnected pads top")); //barvy
			break;
		}

		break;

	case UNCONNECTED_PADS_INNER_LAYER:
		switch (mode & 0x0F)
		{
		case 0:
		case 1:
			strcpy(LayerText, SC(81, "Unconnected pads inner"));
			break;

		case 2:
			break;

		case 3:
			strcpy(LayerText, SC(81, "Unconnected pads inner"));
			break;

		case 4:
			strcpy(LayerText, SC(81, "Unconnected pads inner")); //barvy
			break;
		}

		break;

	case ROUTING_KEEPOUT_INNER:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(60, "Keepout inner")); //export odb++
			break;

		case 1:
			strcpy(LayerText, SC(60, "2Keepout inner"));
			break;

		case 2:
			strcpy(LayerText, SC(60, "3Keepout inner"));
			break;

		case 3:
			strcpy(LayerText, SC(60, "4Keepout inner"));
			break;

		case 4:
			strcpy(LayerText, SC(60, "Keepout inner")); //barvy
			break;
		}

		break;

	case ROUTING_KEEPOUT_TOP:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(44, "Keepout top")); //export odb++
			break;

		case 1:
			strcpy(LayerText, SC(44, "2Keepout top"));
			break;

		case 2:
			strcpy(LayerText, SC(44, "3Keepout top"));
			break;

		case 3:
			strcpy(LayerText, SC(44, "4Keepout top"));
			break;

		case 4:
			strcpy(LayerText, SC(44, "Keepout top")); //barvy
			break;
		}

		break;

	case ROUTING_KEEPOUT_BOTTOM:
		switch (mode & 0x0F)
		{
		case 0:
			strcpy(LayerText, SC(45, "1Keepout bottom"));
		
		case 1:
			strcpy(LayerText, SC(45, "Keepout bottom")); //export odb++
			break;

		case 2:
			strcpy(LayerText, SC(45, "3Keepout bottom"));
			break;

		case 3:
			strcpy(LayerText, SC(45, "4Keepout bottom"));
			break;

		case 4:
			strcpy(LayerText, SC(45, "Keepout bottom")); //barvy
			break;
		}

		break;

	default:
		switch (mode & 0x0F)
		{
		case 0:
			if (Layer < 32)
			{
				if ((mode & 0x10) == 0)
					GetLayerText(Layer, LayerText, (mode & 64) + 32);
				else
					GetLayerText(Layer, LayerText, (mode & 64) + 32 + 16);
			}
			else
			{
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
				{
					if ((mode & 0x10) == 0)
						NewLayer = Layer - ROUTING_KEEPOUT_LAYER;
					else
						NewLayer = Design.NrBoardLayers - (Layer - ROUTING_KEEPOUT_LAYER) - 1;

					if (Layer == ROUTING_KEEPOUT_LAYER)
						sprintf(LayerText, SC(110, "Keepout bottom %d"), NewLayer); //nazev v prohlizeci gerber bez diakritiky

					if (Design.NrBoardLayers > 1)
					{
						if (Layer == ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1)
							sprintf(LayerText, SC(111, "Keepout top %d"), NewLayer); //nazev v prohlizeci gerber bez diakritiky
					}

					if (CheckIfInnerLayer(Layer - ROUTING_KEEPOUT_LAYER))
						sprintf(LayerText, SC(112, "Keepout inner %d"), NewLayer); //nazev v prohlizeci gerber bez diakritiky
				}
			}

			break;

		case 1:
			if (Layer < 32)
				GetLayerText(Layer, LayerText, 2);
			else
			{
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
				{
					if ((mode & 0x10) == 0)
						NewLayer = Layer - ROUTING_KEEPOUT_LAYER - 1;
					else
						NewLayer = Design.NrBoardLayers - (Layer - ROUTING_KEEPOUT_LAYER) - 1;

					if (Layer == ROUTING_KEEPOUT_LAYER)
						sprintf(LayerText, SC(113, "1Keepout\tBottom %d"), Layer - ROUTING_KEEPOUT_LAYER - 1);

					if (Design.NrBoardLayers > 1)
					{
						if (Layer == ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1)
							sprintf(LayerText, SC(114, "2Keepout\tTop %d"), Layer - ROUTING_KEEPOUT_LAYER - 1);
					}

					if (CheckIfInnerLayer(Layer - ROUTING_KEEPOUT_LAYER))
						sprintf(LayerText, SC(115, "3Keepout\tInner %d"), Layer - ROUTING_KEEPOUT_LAYER - 1);
				}
			}

			break;

		case 2:
			break;

		case 5:
			if (Layer < 32)
			{
				if ((mode & 0x10) == 0)
					GetLayerText(Layer, LayerText, (mode & 64) + 8);
				else
					GetLayerText(Layer, LayerText, (mode & 64) + 16 + 8);
			}
			else
			{
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
				{
					if ((mode & 0x10) == 0)
						NewLayer = Layer - ROUTING_KEEPOUT_LAYER;
					else
						NewLayer = Design.NrBoardLayers - (Layer - ROUTING_KEEPOUT_LAYER) - 1;

					if (Layer == ROUTING_KEEPOUT_LAYER)
						sprintf(LayerText, "KeepoutBottom_%d", NewLayer); //název souboru.ger

					if (Design.NrBoardLayers > 1)
					{
						if (Layer == ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1)
							sprintf(LayerText, "KeepoutTop_%d", NewLayer); //název souboru.ger
					}

					if (CheckIfInnerLayer(Layer - ROUTING_KEEPOUT_LAYER))
					{
						if ((mode & 0x10) == 0)
							NewLayer = Layer - ROUTING_KEEPOUT_LAYER;
						else
							NewLayer = Design.NrBoardLayers - (Layer - ROUTING_KEEPOUT_LAYER) - 1;

						sprintf(LayerText, "KeepoutInner_%d", NewLayer); //název souboru.ger
					}
				}
			}

			break;

		case 3:
			if (Layer < 32)
			{
				if ((mode & 0x10) == 0)
					NewLayer = Layer;
				else
					NewLayer = Design.NrBoardLayers - Layer - 1;

				if (Layer == 0)
					strcpy(LayerText, SC(119, "1Traces bottom layer"));
				else
				{
					if (Layer == Design.NrBoardLayers - 1)
						strcpy(LayerText, SC(120, "2Traces top layer"));
					else
						sprintf(LayerText, SC(121, "3Traces inner layer %d"), Layer);
				}
			}
			else
			{
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
				{
					if ((mode & 0x10) == 0)
						NewLayer = Layer - ROUTING_KEEPOUT_LAYER;
					else
						NewLayer = Design.NrBoardLayers - (Layer - ROUTING_KEEPOUT_LAYER) - 1;

					if (Layer == ROUTING_KEEPOUT_LAYER)
						sprintf(LayerText, SC(110, "1Keepout bottom %d"), NewLayer);

					if (Design.NrBoardLayers > 1)
					{
						if (Layer == ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1)
							sprintf(LayerText, SC(111, "2Keepout top %d"), NewLayer);
					}

					if (CheckIfInnerLayer(Layer - ROUTING_KEEPOUT_LAYER))
						sprintf(LayerText, SC(112, "3Keepout inner %d"), NewLayer);
				}
			}

			break;

		case 4:
			if (Layer < 32)
			{
				if ((mode & 0x10) == 0)
					GetLayerText(Layer, LayerText, 5);
				else
					GetLayerText(Layer, LayerText, 16 + 5);
			}
			else
			{
				if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
				{
					if ((mode & 0x10) == 0)
						NewLayer = Layer - ROUTING_KEEPOUT_LAYER;
					else
						NewLayer = Design.NrBoardLayers - (Layer - ROUTING_KEEPOUT_LAYER) - 1;

					if (Layer == ROUTING_KEEPOUT_LAYER)
						strcpy(LayerText, SC(45, "Keepout bottom")); //kontext, layers.txt

					if (Design.NrBoardLayers > 1)
					{
						if (Layer == ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1)
							strcpy(LayerText, SC(44, "Keepout top")); //kontext, layers.txt
					}

					if (CheckIfInnerLayer(Layer - ROUTING_KEEPOUT_LAYER))
						sprintf(LayerText, SC(109, "Keepout inner %d"), NewLayer); //kontext, layers.txt
				}
			}

			break;
		}

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetPowerPlaneByLayer(int32 Layer)
{
	int32 cnt, Found;
	AreaFillRecord *AreaFill;

	Found = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE) && (AreaFill->Layer == Layer))
			Found = cnt;
	}

	return Found;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckLineCrossesCircle(double x1, double y1, double x2, double y2, double cx, double cy, double ThickNess)
{
	double R, R2, a1, dx, dy, one_diva1, divx1, divy1, divx2, divy2;
	int32 Vert12;


	a1 = 0.0;
	R = ThickNess * 0.5;

	if (min(x1, x2) > cx + R)
		return 0;

	if (max(x1, x2) < cx - R)
		return 0;

	if (min(y1, y2) > cy + R)
		return 0;

	if (max(y1, y2) < cy - R)
		return 0;

	R2 = R * R;

	divx1 = (cx - x1) * (cx - x1);
	divy1 = (cy - y1) * (cy - y1);
	divx2 = (cx - x2) * (cx - x2);
	divy2 = (cy - y2) * (cy - y2);

	if ((divx1 + divy1 < R2) || (divx2 + divy2 < R2))
		return 1;

	Vert12 = 0;

	if (x1 != x2)
	{
		a1 = (y2 - y1) / (x2 - x1);

		if ((a1 < -100000) || (a1 > 100000))
			Vert12 = 1;
	}
	else
		Vert12 = 1;

	if (Vert12)
	{
		if (divx1 > R2)
			return 0;

		if ((cy > min(y1, y2)) && (cy < max(y1, y2)))
			return 1;

		return 0;
	}
	else
	{
		if (y1 == y2)
		{
			if (divy1 > R2)
				return 0;

			if ((cx > min(x1, x2)) && (cx < max(x1, x2)))
				return 1;

			return 0;
		}
		else
		{
			if (fabs(a1) < 0.001)
			{
				dx = cx;
				dy = y1;
			}
			else
			{
				one_diva1 = -1 / a1;
				dx = (cx * one_diva1 - cy - x1 * a1 + y1) / (one_diva1 - a1);
				dy = (dx - cx) * one_diva1 + cy;
			}

			if ((dx > min(x1, x2)) && (dx < max(x1, x2)) && (((cx - dx) * (cx - dx) + (cy - dy) * (cy - dy)) < R2))
				return 1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 CheckFillingLine(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4,
                       double *NewX, double *NewY, double ThickNess)
{
	double xx, yy, OldX, OldY, IncX, IncY;
	int32 cnt, NrTries;
	int32 Stop = 0;
	/*

	  x1,y1 - x2,y2  Thick line

	  x3,y3 - x4,y4  Line to check on


	*/

//  NrCalcs2++;

	if (CheckLineCrossesCircle(x3, y3, x4, y4, x1, y1, ThickNess) == 1)
		return -1;

	NrCalcs3++;

	*NewX = 1000000000;
	*NewY = 1000000000;

	if (fabs(y1 - y2) < fabs(x1 - x2))
		NrTries = 4 * ((int32) (((fabs(x1 - x2)) / ThickNess)));
	else
		NrTries = 4 * ((int32) (((fabs(y1 - y2)) / ThickNess)));

	if (NrTries == 0)
		NrTries++;

	IncX = (x2 - x1) / NrTries;
	IncY = (y2 - y1) / NrTries;
	cnt = 0;
//  NrTries--;
	xx = x1;
	yy = y1;
	OldX = xx;
	OldY = yy;

	while ((cnt < NrTries + 1) && (!Stop))
	{
#ifdef _DEBUG

		if (0)
		{
			StartDrawingEditingWindow();
			InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_WHITE_PEN_AND_NOT_FILLED);
			SetROP2(OutputDisplay, R2_COPYPEN);
			ellips2(Mult(xx - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult(yy - Yoffset) - 1, Mult(ThickNess),
			        Mult(ThickNess), 255);
			ExitDrawing();
			EndDrawingEditingWindow();
		}

#endif

		if (CheckLineCrossesCircle(x3, y3, x4, y4, xx, yy, ThickNess) == 1)
		{
			*NewX = OldX;
			*NewY = OldY;
			Stop = 1;
		}
		else
		{
			OldX = xx;
			OldY = yy;
			xx += IncX;
			yy += IncY;
		}

		cnt++;
	}

	if (Stop)
	{
		NrTries = 10;
		IncX /= 5;
		IncY /= 5;
		cnt = 0;
		xx = OldX;
		yy = OldY;

		while (cnt < NrTries)
		{
			if (CheckLineCrossesCircle(x3, y3, x4, y4, xx, yy, ThickNess) == 1)
			{
				*NewX = OldX;
				*NewY = OldY;
				return 1;
			}
			else
			{
				OldX = xx;
				OldY = yy;
				xx += IncX;
				yy += IncY;
			}

			cnt++;
		}

		*NewX = OldX;
		*NewY = OldY;
		return 1;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckLineCrossesWithAreaFill(double x1, double y1, double x2, double y2, AreaFillRecord * AreaFill, double *NewX1,
                                   double *NewY1, double *NewX2, double *NewY2, double Thickness, int32 mode)
{
	int32 cnt, cnt2, res, count;
	double *x3, *y3, *x4, *y4, StartX, StartY, FoundX, FoundY, Length, NewLength, NewX, NewY, R, minx12, maxx12, miny12,
	       maxy12, *FirstX3, *FirstY3;
	int32 LineStartError, Found;
	PolygonRecord *Polygon;
	uint8 *AreaPos, *PolygonPos;
#ifdef _DEBUG
	char str[MAX_LENGTH_STRING];
	int32 ok;
#endif

	StartX = x1;
	StartY = y1;

	R = Thickness * 0.5001;
	maxx12 = max(x1, x2) + R;
	minx12 = min(x1, x2) - R;
	maxy12 = max(y1, y2) + R;
	miny12 = min(y1, y2) - R;

	LineStartError = 0;
	Found = 0;
	FoundX = 1000000000;
	FoundY = 1000000000;
	*NewX1 = 1000000000;
	*NewY1 = 1000000000;
	*NewX2 = 1000000000;
	*NewY2 = 1000000000;
	Length = (StartX - FoundX) * (StartX - FoundX) + (StartY - FoundY) * (StartY - FoundY);

	AreaPos = (uint8 *) AreaFill;
	Polygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

	if ((Polygon->maxx < minx12) || (Polygon->maxy < miny12) || (Polygon->minx > maxx12) || (Polygon->miny > maxy12))
		LineStartError = 1;

	PolygonPos = (uint8 *) Polygon;
	cnt = 0;

	while ((!LineStartError) && (cnt < AreaFill->NrPolygons))
	{
		count = Polygon->NrVertices;
//    x4=(*Polygon).Points[0].x;
//    y4=(*Polygon).Points[0].y;
		NrCalcs2++;


		if ((Polygon->maxx > minx12) && (Polygon->maxy > miny12) && (Polygon->minx < maxx12)
		        && (Polygon->miny < maxy12))
		{

			if (cnt == 0)
			{
				if ((PointInPolygon(Polygon, x1, y1) & 1) == 0)
					LineStartError = 1;
			}
			else
			{
				if ((PointInPolygon(Polygon, x1, y1) & 1) == 1)
					LineStartError = 1;
			}

#ifdef _DEBUG

			if (OkToDrawPolygon)
				DrawTestPolygon3(Polygon, 0);

#endif

			FirstX3 = &(*Polygon).Points[0].x;
			FirstY3 = &(*Polygon).Points[0].y;
			x3 = FirstX3;
			y3 = FirstY3;
			x4 = x3 + 2;
			y4 = y3 + 2;
			cnt2 = 0;

			while ((!LineStartError) && (cnt2 < count))
			{
				if (cnt2 == count - 1)
				{
					x4 = FirstX3;
					y4 = FirstY3;
				}

#ifdef _DEBUG

				if ((OkToDrawPolygon) && (cnt == 288) && (cnt2 == 78))
					ok = 1;

#endif
				
				NrCalcs1++;

				if ((max(*x3, *x4) > minx12) && (max(*y3, *y4) > miny12) && (min(*x3, *x4) < maxx12)
				        && (min(*y3, *y4) < maxy12))
				{

					res = CheckFillingLine(x1, y1, x2, y2, *x3, *y3, *x4, *y4, &NewX, &NewY, Thickness);

					if (res == -1)
						LineStartError = 1;

					if (res == 1)
					{
						NewLength = (NewX - StartX) * (NewX - StartX) + (NewY - StartY) * (NewY - StartY);

						if (NewLength < Length)
						{
							Length = NewLength;
							FoundX = NewX;
							FoundY = NewY;

							if ((NewX >= 999900000) || (NewX <= -999900000) || (NewY >= 999900000)
							        || (NewY <= -999900000))
							{
#ifdef _DEBUG
								sprintf(str, "Found oneindig");
								WriteLn(Plotfp, str);
#endif
								return -1;
							}

							//          InitDrawingColorYellow();
							//          ellips2(Mult(NewX-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(NewY-Yoffset)-1,
							//                  Mult(Thickness),Mult(Thickness),255);
							Found = 1;

							//          DrawLine(Mult(x3-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(y3-Yoffset)-1,
							//                   Mult(x4-Xoffset)+DrawWindowMinX,DrawWindowMaxY-Mult(y4-Yoffset)-1);
						}
					}
				}

				cnt2++;
				x3 = x4;
				y3 = y4;
				x4 += 2;
				y4 += 2;
			}
		}

		PolygonPos += MemSizePolygon(Polygon);
		Polygon = (PolygonRecord *) PolygonPos;
		cnt++;
	}

	if (!LineStartError)
	{
		if (!Found)
			return 0;

		*NewX1 = StartX;
		*NewY1 = StartY;
		*NewX2 = FoundX;
		*NewY2 = FoundY;
		return 1;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPointInAreaFill(double x, double y, AreaFillRecord * AreaFill)
{
	int32 cnt, count, NrCrosses;
	PolygonRecord *Polygon;
	uint8 *AreaPos, *PolygonPos;

	NrCrosses = 0;
	AreaPos = (uint8 *) AreaFill;
	Polygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) Polygon;

	for (cnt = 0; cnt < AreaFill->NrPolygons; cnt++)
	{
		count = Polygon->NrVertices;
		NrCrosses += PointInPolygon(Polygon, x, y);
		PolygonPos += MemSizePolygon(Polygon);
		Polygon = (PolygonRecord *) PolygonPos;
	}

	if ((NrCrosses & 1) == 1)
		return 1;

	return 0;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 CopyThermalReliefInPolygon(ObjectRecord * Object, PolygonRecord * Polygon, double ThickNess, double Pos,
                                 int32 MaxPos, int32 mode)
{
	int32 NrSegments, cnt, VertixCount;
	double x1, y1, x2, y2, x3, y3, x4, y4, CircleR, StartAngle, EndAngle, IncAngle, Angle, VertexR, Fact, ThickNess2,
	       PointX1[35], PointY1[35], PointX2[35], PointY2[35];

//  AperTure=&((*AperTures)[CurrentAperTureNr]);

	x4 = 0.0;
	y4 = 0.0;
	Polygon->NrVertices = 0;

	switch (Object->ObjectType)
	{
//    case DRILL:
//    case DRILL_UNPLATED:
	case PIN_PUT_THROUGH_ROUND:
	case PIN_SMD_ROUND:
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
	case PIN_PUT_THROUGH_SQUARE:
	case PIN_SMD_RECT:
	case VIA_PUT_THROUGH_ROUND:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		break;

	default:
		return -1;
	}

	Fact = Pos / MaxPos;
	ThickNess2 = ThickNess + Fact;
	VertixCount = 0;

// ****************************************************************************
// ****************************************************************************

	if ((Object->ObjectType == PIN_PUT_THROUGH_SQUARE) || (Object->ObjectType == PIN_SMD_RECT))
	{
		if (Object->ObjectType == PIN_PUT_THROUGH_SQUARE)
			Object->y2 = Object->x2;

		Polygon->NrVertices = 6;

		x1 = -Object->x2 * 0.5;
		y1 = Object->y2 * 0.5;
		x3 = Object->x3 * 0.5 + Fact;
		(*Polygon).Points[0].x = x1;
		(*Polygon).Points[0].y = x3;
		(*Polygon).Points[1].x = x1 - ThickNess2;
		(*Polygon).Points[1].y = x3;
		(*Polygon).Points[2].x = x1 - ThickNess2;
		(*Polygon).Points[2].y = y1 + ThickNess2;
		(*Polygon).Points[3].x = -x3;
		(*Polygon).Points[3].y = y1 + ThickNess2;
		(*Polygon).Points[4].x = -x3;
		(*Polygon).Points[4].y = y1;
		(*Polygon).Points[5].x = x1;
		(*Polygon).Points[5].y = y1;

		switch (mode)
		{
		case 0:
			break;

		case 1:
			MirrorPolygon(Polygon, 0);
			break;

		case 2:
			MirrorPolygon(Polygon, 0);
			MirrorPolygon(Polygon, 1);
			break;

		case 3:
			MirrorPolygon(Polygon, 1);
			break;
		}

		MovePolygon(Polygon, Object->x1, Object->y1, 0);
		return 0;
	}

// ****************************************************************************
// ****************************************************************************
	switch (Object->ObjectType)
	{
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
		NrSegments = 4;

		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		switch (Object->ObjectType)
		{
		case PIN_LINE_DIAG1:
		case PIN_LINE_DIAG2:
			x2 *= SQRT2;
			break;
		}

		if (x2 > (200000))
			NrSegments = 8;

		if (x2 > (600000))
			NrSegments = 16;

		CircleR = y2 * 0.5;
		VertexR = CircleR;
		StartAngle = asin((Object->x3 * 0.5 + Fact) / VertexR);

		EndAngle = ANGLE_90 - StartAngle;
		IncAngle = (EndAngle - StartAngle) / NrSegments;
		Angle = StartAngle;

		for (cnt = 0; cnt < NrSegments + 1; cnt++)
		{
			PointX1[cnt] = (cos(Angle) * VertexR);
			PointY1[cnt] = (sin(Angle) * VertexR);
			Angle += IncAngle;
		}


		VertexR = CircleR + ThickNess2;
		StartAngle = asin((Object->x3 * 0.5 + Fact) / VertexR);
		EndAngle = ANGLE_90 - StartAngle;
		IncAngle = (EndAngle - StartAngle) / NrSegments;
		Angle = StartAngle;

		for (cnt = 0; cnt < NrSegments + 1; cnt++)
		{
			PointX2[NrSegments - cnt] = (cos(Angle) * VertexR);
			PointY2[NrSegments - cnt] = (sin(Angle) * VertexR);
			Angle += IncAngle;
		}

		for (cnt = 0; cnt < NrSegments + 1; cnt++)
		{
			switch (mode)
			{
			case 0:
				x4 = x1 + PointX1[cnt];
				y4 = y1 + PointY1[cnt];
				break;

			case 1:
				x4 = x1 - PointX1[cnt];
				y4 = y1 + PointY1[cnt];
				break;

			case 2:
				x4 = x1 - PointX1[cnt];
				y4 = y1 - PointY1[cnt];
				break;

			case 3:
				x4 = x1 + PointX1[cnt];
				y4 = y1 - PointY1[cnt];
				break;
			}

			if ((mode == 0) || (mode == 3))
				x4 += x2;

			(*Polygon).Points[VertixCount].x = x4;
			(*Polygon).Points[VertixCount].y = y4;
			VertixCount++;
		}

		if ((mode == 0) || (mode == 3))
			x4 -= x2 * 0.5;
		else
			x4 += x2 * 0.5;

		(*Polygon).Points[VertixCount].x = x4;
		(*Polygon).Points[VertixCount].y = y4;
		VertixCount++;

		for (cnt = 0; cnt < NrSegments + 1; cnt++)
		{
			x3 = x4;
			y3 = y4;

			switch (mode)
			{
			case 0:
				x4 = x1 + PointX2[cnt];
				y4 = y1 + PointY2[cnt];
				break;

			case 1:
				x4 = x1 - PointX2[cnt];
				y4 = y1 + PointY2[cnt];
				break;

			case 2:
				x4 = x1 - PointX2[cnt];
				y4 = y1 - PointY2[cnt];
				break;

			case 3:
				x4 = x1 + PointX2[cnt];
				y4 = y1 - PointY2[cnt];
				break;
			}

			if ((mode == 0) || (mode == 3))
			{
				if (cnt == 0)
				{
					x4 += x2 * 0.5;
					(*Polygon).Points[VertixCount].x = x4;
					(*Polygon).Points[VertixCount].y = y4;
					VertixCount++;
					x4 -= x2 * 0.5;
				}

				x4 += x2;
				(*Polygon).Points[VertixCount].x = x4;
				(*Polygon).Points[VertixCount].y = y4;
				VertixCount++;
			}
			else
			{
				if (cnt == 0)
				{
					x4 += x2 * 0.5;
					(*Polygon).Points[VertixCount].x = x4;
					(*Polygon).Points[VertixCount].y = y4;
					VertixCount++;
					x4 -= x2 * 0.5;
				}

				(*Polygon).Points[VertixCount].x = x4;
				(*Polygon).Points[VertixCount].y = y4;
				VertixCount++;
			}
		}

		Polygon->NrVertices = VertixCount;

		switch (Object->ObjectType)
		{
		case PIN_LINE_VER:
			RotatePolygon(Polygon, x1, y1, 90.0, 0);
			break;

		case PIN_LINE_DIAG1:
			RotatePolygon(Polygon, x1, y1, -45.0, 0);
			break;

		case PIN_LINE_DIAG2:
			RotatePolygon(Polygon, x1, y1, 45.0, 0);
			break;
		}

		return 0;
		break;

// ****************************************************************************
	case PIN_PUT_THROUGH_POLYGON:
		Polygon->NrVertices = 6;

		if (Object->ObjectType2 == PIN_PUT_THROUGH_SQUARE)
			Object->y2 = Object->x2;

		x1 = -Object->x2 * 0.5;
		y1 = Object->y2 * 0.5;
		x3 = Object->x3 * 0.5 + Fact;
		(*Polygon).Points[0].x = x1;
		(*Polygon).Points[0].y = x3;
		(*Polygon).Points[1].x = x1 - ThickNess2;
		(*Polygon).Points[1].y = x3;
		(*Polygon).Points[2].x = x1 - ThickNess2;
		(*Polygon).Points[2].y = y1 + ThickNess2;
		(*Polygon).Points[3].x = -x3;
		(*Polygon).Points[3].y = y1 + ThickNess2;
		(*Polygon).Points[4].x = -x3;
		(*Polygon).Points[4].y = y1;
		(*Polygon).Points[5].x = x1;
		(*Polygon).Points[5].y = y1;

		switch (mode)
		{
		case 0:
			break;

		case 1:
			MirrorPolygon(Polygon, 0);
			break;

		case 2:
			MirrorPolygon(Polygon, 0);
			MirrorPolygon(Polygon, 1);
			break;

		case 3:
			MirrorPolygon(Polygon, 1);
			break;
		}

		RotatePolygon(Polygon, 0.0, 0.0, Object->RotationAngle, 0);
		MovePolygon(Polygon, Object->x1, Object->y1, 0);
		return 0;

	case PIN_SMD_POLYGON:
		if (Object->ObjectType2 == PIN_SMD_RECT)
		{
			Polygon->NrVertices = 6;
			x1 = -Object->x2 * 0.5;
			y1 = Object->y2 * 0.5;
			x3 = Object->x3 * 0.5 + Fact;
			(*Polygon).Points[0].x = x1;
			(*Polygon).Points[0].y = x3;
			(*Polygon).Points[1].x = x1 - ThickNess2;
			(*Polygon).Points[1].y = x3;
			(*Polygon).Points[2].x = x1 - ThickNess2;
			(*Polygon).Points[2].y = y1 + ThickNess2;
			(*Polygon).Points[3].x = -x3;
			(*Polygon).Points[3].y = y1 + ThickNess2;
			(*Polygon).Points[4].x = -x3;
			(*Polygon).Points[4].y = y1;
			(*Polygon).Points[5].x = x1;
			(*Polygon).Points[5].y = y1;

			switch (mode)
			{
			case 0:
				break;

			case 1:
				MirrorPolygon(Polygon, 0);
				break;

			case 2:
				MirrorPolygon(Polygon, 0);
				MirrorPolygon(Polygon, 1);
				break;

			case 3:
				MirrorPolygon(Polygon, 1);
				break;
			}

			RotatePolygon(Polygon, 0.0, 0.0, Object->RotationAngle, 0);
			MovePolygon(Polygon, Object->x1, Object->y1, 0);
			return 0;
		}

		break;
	}

// ****************************************************************************
// ****************************************************************************

	NrSegments = 4;

	if (Object->x2 > (200000))
		NrSegments = 8;

	if (Object->x2 > (600000))
		NrSegments = 16;

	x1 = Object->x1;
	y1 = Object->y1;
	CircleR = Object->x2 * 0.5;
	VertexR = CircleR;
	StartAngle = asin((Object->x3 * 0.5 + Fact) / VertexR);

	EndAngle = ANGLE_90 - StartAngle;
	IncAngle = (EndAngle - StartAngle) / NrSegments;
	Angle = StartAngle;

	for (cnt = 0; cnt < NrSegments + 1; cnt++)
	{
		PointX1[cnt] = (cos(Angle) * VertexR);
		PointY1[cnt] = (sin(Angle) * VertexR);
		Angle += IncAngle;
	}


	VertexR = CircleR + ThickNess2;
	StartAngle = asin((Object->x3 * 0.5 + Fact) / VertexR);
	EndAngle = ANGLE_90 - StartAngle;
	IncAngle = (EndAngle - StartAngle) / NrSegments;
	Angle = StartAngle;

	for (cnt = 0; cnt < NrSegments + 1; cnt++)
	{
		PointX2[NrSegments - cnt] = (cos(Angle) * VertexR);
		PointY2[NrSegments - cnt] = (sin(Angle) * VertexR);
		Angle += IncAngle;
	}

	for (cnt = 0; cnt < NrSegments + 1; cnt++)
	{
		switch (mode)
		{
		case 0:
			x4 = x1 + PointX1[cnt];
			y4 = y1 + PointY1[cnt];
			break;

		case 1:
			x4 = x1 - PointX1[cnt];
			y4 = y1 + PointY1[cnt];
			break;

		case 2:
			x4 = x1 - PointX1[cnt];
			y4 = y1 - PointY1[cnt];
			break;

		case 3:
			x4 = x1 + PointX1[cnt];
			y4 = y1 - PointY1[cnt];
			break;
		}

		(*Polygon).Points[VertixCount].x = x4;
		(*Polygon).Points[VertixCount].y = y4;
		VertixCount++;
	}

	for (cnt = 0; cnt < NrSegments + 1; cnt++)
	{
		x3 = x4;
		y3 = y4;

		switch (mode)
		{
		case 0:
			x4 = x1 + PointX2[cnt];
			y4 = y1 + PointY2[cnt];
			break;

		case 1:
			x4 = x1 - PointX2[cnt];
			y4 = y1 + PointY2[cnt];
			break;

		case 2:
			x4 = x1 - PointX2[cnt];
			y4 = y1 - PointY2[cnt];
			break;

		case 3:
			x4 = x1 + PointX2[cnt];
			y4 = y1 - PointY2[cnt];
			break;
		}

		(*Polygon).Points[VertixCount].x = x4;
		(*Polygon).Points[VertixCount].y = y4;
		VertixCount++;
	}

	Polygon->NrVertices = VertixCount;
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ComplexAreaFill(AreaFillRecord * AreaFill)
{
	int32 cnt2, count;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;
	count = 0;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		count += DrawPolygon->NrVertices;
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	if (count > 1000)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestObject(ObjectRecord * Object, int32 mode)
{
	double x1, y1, x2, y2;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;
	AreaFillRecord *AreaFill;
	int32 ok;

	PolygonObject = (PolygonRecord *) & PolygonBuf;
	x1 = Object->x1;
	y1 = Object->y1;

	switch (Object->ObjectType)
	{
	case PIN_PUT_THROUGH_ROUND:
	case PIN_SMD_ROUND:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
		if (mode == 0)
			x2 = Object->x2;
		else
			x2 = Object->x2 + Object->Clearance * 2;

		return RectTestCircle(x1, y1, x2, 255);

	case PIN_PUT_THROUGH_SQUARE:
		if (mode == 0)
			x2 = Object->x2;
		else
			x2 = Object->x2 + Object->Clearance * 2;

		return RectTestRect2(x1, y1, x2, x2);

	case PIN_SMD_RECT:
		if (mode == 0)
		{
			x2 = Object->x2;
			y2 = Object->y2;
		}
		else
		{
			x2 = Object->x2 + Object->Clearance * 2;
			y2 = Object->y2 + Object->Clearance * 2;
		}

		return RectTestRect2(x1, y1, x2, y2);

	case PIN_LINE_VER:
		x2 = Object->x2;

		if (mode == 0)
			y2 = Object->y2;
		else
			y2 = Object->y2 + Object->Clearance * 2;

		return ((RectTestRect2(x1, y1 + (x2 / 2), y2, x2)) || (RectTestCircle(x1, y1, y2, 255))
		        || (RectTestCircle(x1, y1 + x2, y2, 255)));

	case PIN_LINE_HOR:
		x2 = Object->x2;

		if (mode == 0)
			y2 = Object->y2;
		else
			y2 = Object->y2 + Object->Clearance * 2;

		return ((RectTestRect2(x1 + x2 / 2, y1, x2, y2)) || (RectTestCircle(x1, y1, y2, 255))
		        || (RectTestCircle(x1 + x2, y1, y2, 255)));

	case PIN_LINE_DIAG1:
		x2 = Object->x2;

		if (mode == 0)
			y2 = Object->y2;
		else
			y2 = Object->y2 + Object->Clearance * 2;

		return ((RectTestDiag1(x1, y1, x2, y2)) || (RectTestCircle(x1, y1, y2, 255))
		        || (RectTestCircle(x1 + x2, y1 - x2, y2, 255)));

	case PIN_LINE_DIAG2:
		x2 = Object->x2;

		if (mode == 0)
			y2 = Object->y2;
		else
			y2 = Object->y2 + Object->Clearance * 2;

		return ((RectTestDiag2(x1, y1, x2, y2)) || (RectTestCircle(x1, y1, y2, 255))
		        || (RectTestCircle(x1 + x2, y1 + x2, y2, 255)));

	case OBJECT_LINE:
	case TRACE_ALL_ANGLE:
	case PIN_LINE_ALL_ANGLE:
	case PIN_ARC:
	case TRACE_ARC:
	case OBJECT_ARC:
		if (mode == 0)
			MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
		else
			MakePolygonFromObject(Object, PolygonObject, Object->Clearance, 0.0, 1, 1);

		if ((SearchMaxX > PolygonObject->minx) && (SearchMinX < PolygonObject->maxx)
		        && (SearchMaxY > PolygonObject->miny) && (SearchMinY < PolygonObject->maxy))
			return 1;

		break;

	case OBJECT_POLYGON:
	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		if (CheckObjectIsBigPolygon(Object))
		{
			if (mode == 0)
				GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
			else
				GetAreaFillFromBigPolygonObject(Object, &AreaFill, Object->Clearance, 0);

			if ((SearchMaxX > AreaFill->minx) && (SearchMinX < AreaFill->maxx) && (SearchMaxY > AreaFill->miny)
			        && (SearchMinY < AreaFill->maxy))
				return 1;
		}
		else
		{
			if (mode == 0)
				MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
			else
				MakePolygonFromObject(Object, PolygonObject, Object->Clearance, 0.0, 1, 1);

			if ((SearchMaxX > PolygonObject->minx) && (SearchMinX < PolygonObject->maxx)
			        && (SearchMaxY > PolygonObject->miny) && (SearchMinY < PolygonObject->maxy))
				return 1;
		}

		break;

	default:
		ok = 1;
		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void FillPositionObject(ObjectRecord * Object)
{
	int32 SegmentCount, LineSegments, cnt2, TextAlignment, Mirror, ok, FontNr, MaxCountX, NrLines;
	double x1, y1, x2, y2, x3, y3, x4, y4, x2a, y2a, Rotation, LineBuf[128], Xmax, Xmin, Ymax, Ymin;
	ObjectPolygonRecord *ObjectPolygon;
	PolygonRecord *TestPolygon;
	uint8 PolygonBuf[10240];
	AreaFillRecord *AreaFill;

	TestPolygon = (PolygonRecord *) & PolygonBuf;

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
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	case THERMAL_RELIEF_ROUND:
		y2a = x2 * 0.5;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + y2a;
		Object->maxy = y1 + y2a;
		break;

	case PIN_SMD_RECT:
		x2a = x2 * 0.5;
		y2a = y2 * 0.5;
		Object->minx = x1 - x2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2a;
		Object->maxy = y1 + y2a;
		break;

	case CONNECTION:
		Object->minx = min(x1, x2) - 10000.0;
		Object->miny = min(y1, y2) - 10000.0;
		Object->maxx = max(x1, x2) + 10000.0;
		Object->maxy = max(y1, y2) + 10000.0;
		break;

	case TRACE_HOR:
	case PIN_LINE_HOR:
		y2a = y2 * 0.5;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2 + y2a;
		Object->maxy = y1 + y2a;
		break;

	case TRACE_VER:
	case PIN_LINE_VER:
		y2a = y2 * 0.5;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + y2a;
		Object->maxy = y1 + x2 + y2a;
		break;

	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:
		y2a = y2 * 0.5;
		Object->minx = x1 - y2a;
		Object->miny = y1 - x2 - y2a;
		Object->maxx = x1 + x2 + y2a;
		Object->maxy = y1 + y2a;
		break;

	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:
		y2a = y2 * 0.5;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2 + y2a;
		Object->maxy = y1 + x2 + y2a;
		break;

	case OBJECT_LINE:
		x2a = Object->Thickness * 0.5;

		if (Object->Test == 0)
		{
			Object->minx = min(x1, x2) - x2a;
			Object->miny = min(y1, y2) - x2a;
			Object->maxx = max(x1, x2) + x2a;
			Object->maxy = max(y1, y2) + x2a;
		}
		else
		{
			Object->minx = 1e9;
			Object->miny = 1e9;
			Object->maxx = -1e9;
			Object->maxy = -1e9;
			LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, Object->Test);
			SegmentCount = 0;

			for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
			{
				x3 = LineBuf[SegmentCount++];
				y3 = LineBuf[SegmentCount++];
				x4 = LineBuf[SegmentCount++];
				y4 = LineBuf[SegmentCount++];
				Object->minx = min(Object->minx, min(x3, x4) - x2a);
				Object->miny = min(Object->miny, min(y3, y4) - x2a);
				Object->maxx = max(Object->maxx, max(x3, x4) + x2a);
				Object->maxy = max(Object->maxy, max(y3, y4) + x2a);
			}
		}

		break;

	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
		x2a = Object->Thickness * 0.5;
		Object->minx = min(x1, x2) - x2a;
		Object->miny = min(y1, y2) - x2a;
		Object->maxx = max(x1, x2) + x2a;
		Object->maxy = max(y1, y2) + x2a;
		break;

	case OBJECT_RECT:
		if ((Object->Info & OBJECT_FILLED) == 0)
		{
			x2a = (x2 + Object->Thickness) * 0.5;
			y2a = (y2 + Object->Thickness) * 0.5;
		}
		else
		{
			x2a = x2 * 0.5;
			y2a = y2 * 0.5;
		}

		Object->minx = x1 - x2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2a;
		Object->maxy = y1 + y2a;
		break;

	case OBJECT_CIRCLE:
		if ((Object->Info & OBJECT_FILLED) == 0)
			x2a = (x2 + Object->Thickness) * 0.5;
		else
			x2a = x2 * 0.5;

		Object->minx = x1 - x2a;
		Object->miny = y1 - x2a;
		Object->maxx = x1 + x2a;
		Object->maxy = y1 + x2a;
		break;

	case OBJECT_ARC:
	case PIN_ARC:
	case TRACE_ARC:
		x2a = (x2 + Object->Thickness) * 0.5;
		y2a = (y2 + Object->Thickness) * 0.5;
		Object->minx = x1 - x2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2a;
		Object->maxy = y1 + y2a;
		break;

	case OBJECT_TEXT:
	case OBJECT_TEXT2:
		x2a = Object->Thickness * 0.5;
		Rotation = Object->RotationAngle;
		TextAlignment = 0;
		Mirror = Object->Mirror;
		FontNr = (Object->Test >> 16) & 0xff;

		NrLines = ConvertObjectTextToStrings((LPSTR) Object->TraceNr, FontNr, &MaxCountX, Object->Layer);
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

		if (FontNr > 0)
			x2a = 0.0;

		Object->minx = Xmin - x2a;
		Object->miny = Ymin - x2a;
		Object->maxx = Xmax + x2a;
		Object->maxy = Ymax + x2a;
		break;

	case OBJECT_POLYGON:
		if ((Object->ObjectType2 != 0) || (Object->Address != 0))
		{
			if (CheckObjectIsBigPolygon(Object))
			{
				GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
				Object->minx = AreaFill->minx;
				Object->miny = AreaFill->miny;
				Object->maxx = AreaFill->maxx;
				Object->maxy = AreaFill->maxy;
			}
			else
			{
				MakePolygonFromObject(Object, TestPolygon, 0.0, 0.0, 0, 1);
				Object->minx = TestPolygon->minx;
				Object->miny = TestPolygon->miny;
				Object->maxx = TestPolygon->maxx;
				Object->maxy = TestPolygon->maxy;
			}
		}
		else
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[Object->TraceNr]]);
			SetMinMaxObjectPolygon(ObjectPolygon, 0);
			Object->minx = ObjectPolygon->minx;
			Object->miny = ObjectPolygon->miny;
			Object->maxx = ObjectPolygon->maxx;
			Object->maxy = ObjectPolygon->maxy;
		}

		break;

	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		if (CheckObjectIsBigPolygon(Object))
		{
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
			Object->minx = AreaFill->minx;
			Object->miny = AreaFill->miny;
			Object->maxx = AreaFill->maxx;
			Object->maxy = AreaFill->maxy;
		}
		else
		{
			MakePolygonFromObject(Object, TestPolygon, 0.0, 0.0, 0, 1);
			Object->minx = TestPolygon->minx;
			Object->miny = TestPolygon->miny;
			Object->maxx = TestPolygon->maxx;
			Object->maxy = TestPolygon->maxy;
		}

		break;

	case AREAFILL:
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object->TraceNr]]);
		Object->minx = AreaFill->minx;
		Object->miny = AreaFill->miny;
		Object->maxx = AreaFill->maxx;
		Object->maxy = AreaFill->maxy;
		ok = 1;
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void FillPositionObjectWithClearance(ObjectRecord * Object, double Clearance)
{
	double x1, y1, x2, x2a, y2, y2a;
	PolygonRecord *TestPolygon;
	uint8 PolygonBuf[10240];
	AreaFillRecord *AreaFill;

	TestPolygon = (PolygonRecord *) & PolygonBuf;

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
	case DRILL:
	case DRILL_UNPLATED:
	case OBJECT_CIRCLE:
		y2a = x2 * 0.5 + Clearance;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + y2a;
		Object->maxy = y1 + y2a;
		break;

	case PIN_SMD_RECT:
	case OBJECT_RECT:
		x2a = x2 * 0.5 + Clearance;
		y2a = y2 * 0.5 + Clearance;
		Object->minx = x1 - x2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2a;
		Object->maxy = y1 + y2a;
		break;

	case CONNECTION:
		Object->minx = min(x1, x2) - 10000.0;
		Object->miny = min(y1, y2) - 10000.0;
		Object->maxx = max(x1, x2) + 10000.0;
		Object->maxy = max(y1, y2) + 10000.0;
		break;

	case TRACE_HOR:
	case PIN_LINE_HOR:
		y2a = y2 * 0.5 + Clearance;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2 + y2a;
		Object->maxy = y1 + y2a;
		break;

	case TRACE_VER:
	case PIN_LINE_VER:
		y2a = y2 * 0.5 + Clearance;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + y2a;
		Object->maxy = y1 + x2 + y2a;
		break;

	case TRACE_DIAG1:
	case PIN_LINE_DIAG1:
		y2a = y2 * 0.5 + Clearance;
		Object->minx = x1 - y2a;
		Object->miny = y1 - x2 - y2a;
		Object->maxx = x1 + x2 + y2a;
		Object->maxy = y1 + y2a;
		break;

	case TRACE_DIAG2:
	case PIN_LINE_DIAG2:
		y2a = y2 * 0.5 + Clearance;
		Object->minx = x1 - y2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2 + y2a;
		Object->maxy = y1 + x2 + y2a;
		break;

	case OBJECT_LINE:
	case PIN_LINE_ALL_ANGLE:
	case TRACE_ALL_ANGLE:
		y2a = Object->Thickness * 0.5 + Clearance;
		Object->minx = min(x1, x2) - y2a;
		Object->miny = min(y1, y2) - y2a;
		Object->maxx = max(x1, x2) + y2a;
		Object->maxy = max(y1, y2) + y2a;
		break;

	case PIN_ARC:
	case OBJECT_ARC:
	case TRACE_ARC:
		x2a = (x2 + Object->Thickness) * 0.5 + Clearance;
		y2a = (y2 + Object->Thickness) * 0.5 + Clearance;
		Object->minx = x1 - x2a;
		Object->miny = y1 - y2a;
		Object->maxx = x1 + x2a;
		Object->maxy = y1 + y2a;
		break;

	case PIN_SMD_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		if (CheckObjectIsBigPolygon(Object))
		{
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, Clearance, 0);
			Object->minx = AreaFill->minx;
			Object->miny = AreaFill->miny;
			Object->maxx = AreaFill->maxx;
			Object->maxy = AreaFill->maxy;
		}
		else
		{
			MakePolygonFromObject(Object, TestPolygon, Clearance, 0.0, 0, 1);
			Object->minx = TestPolygon->minx;
			Object->miny = TestPolygon->miny;
			Object->maxx = TestPolygon->maxx;
			Object->maxy = TestPolygon->maxy;
		}

		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MakePolygonFromSpecialLine(double x1, double y1, double x2, double y2, ObjectPolygonRecord * ObjectPolygon,
                                double Thickness1, double Thickness2, int32 mode)
{
	double Length, Angle, x3, y3, x4, y4;


	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);
	x3 = Thickness1 * 0.5 * sin(Angle);
	y3 = Thickness1 * 0.5 * cos(Angle);
	x4 = Thickness2 * 0.5 * sin(Angle);
	y4 = Thickness2 * 0.5 * cos(Angle);
	ObjectPolygon->NrVertices = 4;
	ObjectPolygon->Points[0].x = x1 + x3;
	ObjectPolygon->Points[0].y = y1 - y3;
	ObjectPolygon->Points[1].x = x1 - x3;
	ObjectPolygon->Points[1].y = y1 + y3;
	ObjectPolygon->Points[2].x = x2 - x4;
	ObjectPolygon->Points[2].y = y2 + y4;
	ObjectPolygon->Points[3].x = x2 + x4;
	ObjectPolygon->Points[3].y = y2 - y4;

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 MakeObjectFromCompRef(CompRecord * Comp, ObjectRecord * Object, int32 Mode)
{
	double x2, y2, h2, NewRotation, TextRotation;
	int32 lengte, MemPos, ShapeNr, Mirror;
	ShapeRecord *Shape;

	if ((Comp->TextVisibility & 0x100) != 0)
		return -1;

//  CompMirror=((Comp->CompMode & 8) >> 3);
	Mirror = (Comp->TextVisibility & 8) >> 3;

//  Mirror=Mirror ^ CompMirror;
	ShapeNr = Comp->ShapeNr;

	if (ShapeNr == -1)
		return -1;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	lengte = strlen(Comp->Name);
	h2 = Comp->CompNameHeight;

	if (h2 == 0)
		h2 = Shape->ShapeNameHeight;

	x2 = Comp->CompNameOriginX;
	y2 = Comp->CompNameOriginY;

	NewRotation = Comp->Rotation;
	TextRotation = Comp->CompNameRotation;

	if (Mirror == 0)
		TextRotation += NewRotation;
	else
	{
		x2 = -x2;
		TextRotation -= NewRotation;
	}

	RotatePoint2(&x2, &y2, NewRotation);

	x2 += Comp->CompOriginX;
	y2 += Comp->CompOriginY;
	Object->ObjectType = OBJECT_TEXT;
	Object->x1 = x2;
	Object->y1 = y2;
	Object->x2 = h2;
	Object->Test = 0;
	Object->RotationAngle = LimitRotation(TextRotation);
	Object->Mirror = Mirror;
	Object->Thickness = Comp->CompNamePenThickNess;
	Object->TraceNr = (int32) & (Comp->Name);
	Object->Layer = COMP_REF_LAYER;
	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 MakeObjectFromCompValue(CompRecord * Comp, ObjectRecord * Object, int32 Mode)
{
	double x2, y2, h2, NewRotation, TextRotation;
	int32 lengte, MemPos, ShapeNr, Mirror;
	ShapeRecord *Shape;

	if ((Comp->TextVisibility & 0x200) != 0)
		return -1;

//  CompMirror=((Comp->CompMode & 8) >> 3);
	Mirror = (Comp->TextVisibility & 8) >> 3;

//  Mirror=Mirror ^ CompMirror;
	ShapeNr = Comp->ShapeNr;

	if (ShapeNr == -1)
		return -1;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	lengte = strlen(Comp->Value);
	h2 = Comp->CompValueHeight;

	if (h2 == 0)
		h2 = Shape->ShapeNameHeight;

	x2 = Comp->CompValueOriginX;
	y2 = Comp->CompValueOriginY;

	NewRotation = Comp->Rotation;
	TextRotation = Comp->CompValueRotation;

	if ((Mirror = (Comp->TextVisibility & 0x80) >> 7) == 0)
		TextRotation += NewRotation;
	else
	{
		x2 = -x2;
		TextRotation -= NewRotation;
	}

	RotatePoint2(&x2, &y2, NewRotation);

	x2 += Comp->CompOriginX;
	y2 += Comp->CompOriginY;
	Object->ObjectType = OBJECT_TEXT;
	Object->x1 = x2;
	Object->y1 = y2;
	Object->x2 = h2;
	Object->Test = 0;
	Object->RotationAngle = LimitRotation(TextRotation);
	Object->Mirror = Mirror;
	Object->Thickness = Comp->CompValuePenThickNess;
	Object->TraceNr = (int32) & (Comp->Value);
	Object->Layer = COMP_VALUE_LAYER;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetArcAngle(ObjectRecord * Object, double *Angle1, double *Angle2)
{
	double Length;

	*Angle1 = 0.0;
	*Angle2 = 0.0;

	if ((Object->ObjectType != OBJECT_ARC) && (Object->ObjectType != PIN_ARC) && (Object->ObjectType != TRACE_ARC))
		return -1;

	ConvertPointToPolar(Object->x3, Object->y3, &Length, Angle1);
	ConvertPointToPolar(Object->x4, Object->y4, &Length, Angle2);
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetCoordinatesObjectTrace(ObjectRecord * Object, double *x1, double *y1, double *x2, double *y2)
{
	*x1 = 0.0;
	*y1 = 0.0;
	*x2 = 0.0;
	*y2 = 0.0;

	switch (Object->ObjectType)
	{
	case TRACE_HOR:
	case TRACE_VER:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
		*x1 = Object->x1;
		*y1 = Object->y1;

		switch (Object->ObjectType)
		{
		case TRACE_HOR:
		case PIN_LINE_HOR:
			*x2 = *x1 + Object->x2;
			*y2 = *y1;
			break;

		case TRACE_VER:
		case PIN_LINE_VER:
			*x2 = *x1;
			*y2 = *y1 + Object->x2;
			break;

		case TRACE_DIAG1:
		case PIN_LINE_DIAG1:
			*x2 = *x1 + Object->x2;
			*y2 = *y1 - Object->x2;
			break;

		case TRACE_DIAG2:
		case PIN_LINE_DIAG2:
			*x2 = *x1 + Object->x2;
			*y2 = *y1 + Object->x2;
			break;
		}

		break;
	}

	return 0;
}


// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 AdjustOffsetForSnapOnGerberObject(double CursorX, double CursorY, double GridX, double GridY, double divx,
                                        double divy, double *ShiftOffsetX, double *ShiftOffsetY, int32 mode)
{
	int32 cnt, cnt2, SelectionMask1, SelectionMask2, count, ObjectCount;
	double x1, y1, x2, y2, px1, py1, px2, py2, AddX, AddY, Length1, Length2, TempLength;
	ObjectRecord *Object, NewObject;
	PolygonRecord *PolygonObject;
	AreaFillRecord *AreaFill;
	uint8 PolygonBuf1[10240];

	px1 = 0.0;
	py1 = 0.0;
	px2 = 0.0;
	py2 = 0.0;

	if ((mode & 1) == 0)
	{
//    SelectionMask1=OBJECT_NOT_VISIBLE|OBJECT_SELECTED;
//    SelectionMask2=OBJECT_SELECTED;
		SelectionMask1 = OBJECT_NOT_VISIBLE;
		SelectionMask2 = 0;
	}
	else
	{
		divx = 0.0;
		divy = 0.0;
		SelectionMask1 = OBJECT_NOT_VISIBLE;
		SelectionMask2 = 0;
	}

	memset(&NewObject, 0, sizeof(NewObject));

	PolygonObject = (PolygonRecord *) & PolygonBuf1;

	if ((mode & 4) == 0)
		ObjectCount = NrObjects6;
	else
		ObjectCount = NrObjects4;

	for (cnt = 0; cnt < ObjectCount; cnt++)
	{
		if ((mode & 4) == 0)
			Object = &((*Objects6)[cnt]);
		else
			Object = &((*Objects4)[cnt]);

		if ((Object->Info & SelectionMask1) == SelectionMask2)
		{
			switch (Object->ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
				memmove(&NewObject, Object, sizeof(NewObject));
				NewObject.x1 += divx;
				NewObject.y1 += divy;
				GetCoordinatesObjectTrace(&NewObject, &x1, &y1, &x2, &y2);
				NewObject.Thickness = max(NewObject.Thickness, 1e5);
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					Length1 = CalcLengthLine(x1, y1, CursorX, CursorY);
					Length2 = CalcLengthLine(x2, y2, CursorX, CursorY);

					if (Length1 < Length2)
					{
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = x1 - GridX;
								AddY = y1 - GridY;
							}
							else
							{
								AddX = x1 - CursorX;
								AddY = y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - x1;
							AddY = GridY - y1;
						}
					}
					else
					{
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = x2 - GridX;
								AddY = y2 - GridY;
							}
							else
							{
								AddX = x2 - CursorX;
								AddY = y2 - CursorY;
							}
						}
						else
						{
							AddX = GridX - x2;
							AddY = GridY - y2;
						}
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
					return 1;
				}

				break;

			case TRACE_ALL_ANGLE:
			case OBJECT_LINE:
			case PIN_LINE_ALL_ANGLE:
				memmove(&NewObject, Object, sizeof(NewObject));
				NewObject.x1 += divx;
				NewObject.y1 += divy;
				NewObject.x2 += divx;
				NewObject.y2 += divy;
				NewObject.Thickness = max(NewObject.Thickness, 1e5);
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					Length1 = CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
					Length2 = CalcLengthLine(NewObject.x2, NewObject.y2, CursorX, CursorY);

					if (Length1 < Length2)
					{
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x1 - GridX;
								AddY = NewObject.y1 - GridY;
							}
							else
							{
								AddX = NewObject.x1 - CursorX;
								AddY = NewObject.y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x1;
							AddY = GridY - NewObject.y1;
						}
					}
					else
					{
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x2 - GridX;
								AddY = NewObject.y2 - GridY;
							}
							else
							{
								AddX = NewObject.x2 - CursorX;
								AddY = NewObject.y2 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x2;
							AddY = GridY - NewObject.y2;
						}
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
					return 1;
				}

				break;

			case TRACE_ARC:
			case OBJECT_ARC:
			case PIN_ARC:
				/*
				          if (Object->Thickness==0.0) { // Filled
				            px1=Object->x1;
				            py1=Object->y1;
				            NewObject.x2=Object->x2;
				          } else {
				*/
				NewObject.x2 = max(Object->Thickness, 1e5);

				NewObject.ObjectType = PIN_SMD_ROUND;
				NewObject.x1 = Object->x1 + divx;
				NewObject.y1 = Object->y1 + divy;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
					return 1;
				}

				GetArcEndPoints(Object, &px1, &py1, &px2, &py2, 0);
				NewObject.ObjectType = PIN_SMD_ROUND;
				NewObject.x1 = px1 + divx;
				NewObject.y1 = py1 + divy;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
					return 1;
				}

				NewObject.x1 = px2 + divx;
				NewObject.y1 = py2 + divy;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
					return 1;
				}

				break;

			case PIN_SMD_ROUND:
			case PIN_PUT_THROUGH_ROUND:
			case VIA_PUT_THROUGH_ROUND:
			case DRILL:
			case DRILL_UNPLATED:
				NewObject.ObjectType = PIN_SMD_ROUND;
				NewObject.x1 = Object->x1 + divx;
				NewObject.y1 = Object->y1 + divy;
				NewObject.x2 = Object->x2;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
				}

				break;

			case OBJECT_CIRCLE:
				if (((Object->Thickness == 0.0) && (Object->Info2 == 0)) || (Object->Info2 == 15))
					NewObject.x2 = Object->x2;
				else
				{
					GetArcEndPoints(Object, &px1, &py1, &px2, &py2, 0);
					NewObject.x2 = max(Object->Thickness, 1e5);
				}

				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.x1 = Object->x1 + divx;
				NewObject.y1 = Object->y1 + divy;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
				}

				if (!(((Object->Thickness == 0.0) && (Object->Info2 == 0)) || (Object->Info2 == 15)))
				{
					NewObject.x1 = px1 + divx;
					NewObject.y1 = py1 + divy;
					MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

					if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
					{	// Point within object snap it
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x1 - GridX;
								AddY = NewObject.y1 - GridY;
							}
							else
							{
								AddX = NewObject.x1 - CursorX;
								AddY = NewObject.y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x1;
							AddY = GridY - NewObject.y1;
						}

						*ShiftOffsetX += AddX;
						*ShiftOffsetY += AddY;
						return 1;
					}

					NewObject.x1 = px2 + divx;
					NewObject.y1 = py2 + divy;
					MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

					if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
					{	// Point within object snap it
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x1 - GridX;
								AddY = NewObject.y1 - GridY;
							}
							else
							{
								AddX = NewObject.x1 - CursorX;
								AddY = NewObject.y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x1;
							AddY = GridY - NewObject.y1;
						}

						*ShiftOffsetX += AddX;
						*ShiftOffsetY += AddY;
						return 1;
					}
				}

				break;

			case OBJECT_RECT:
			case PIN_SMD_RECT:
				memmove(&NewObject, Object, sizeof(ObjectRecord));
				NewObject.x1 += divx;
				NewObject.y1 += divy;
				MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
					return 1;
				}

				break;

			case OBJECT_POLYGON:
			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
				memmove(&NewObject, Object, sizeof(ObjectRecord));
				NewObject.x1 += divx;
				NewObject.y1 += divy;

				if (CheckObjectIsBigPolygon(Object))
				{
					GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
					PolygonObject = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
				}
				else
					MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

				count = PolygonObject->NrVertices;

				for (cnt2 = 0; cnt2 < count; cnt2++)
				{
					x1 = PolygonObject->Points[cnt2].x;
					y1 = PolygonObject->Points[cnt2].y;
					NewObject.x1 = x1 + divx;
					NewObject.y1 = y1 + divy;

					if (CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY) < 0.25e5)
					{
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x1 - GridX;
								AddY = NewObject.y1 - GridY;
							}
							else
							{
								AddX = NewObject.x1 - CursorX;
								AddY = NewObject.y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x1;
							AddY = GridY - NewObject.y1;
						}

						*ShiftOffsetX += AddX;
						*ShiftOffsetY += AddY;
						return 1;
					}
				}

				x1 = ((PolygonObject->minx + PolygonObject->maxx) * 0.5);
				y1 = ((PolygonObject->miny + PolygonObject->maxy) * 0.5);
				NewObject.x1 = x1 + divx;
				NewObject.y1 = y1 + divy;
				TempLength = CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);

				if (TempLength < 1e5)
				{
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					*ShiftOffsetX += AddX;
					*ShiftOffsetY += AddY;
					return 1;
				}

				break;
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckIfCopperObjectArc(ObjectArcRecord * ObjectArc, int32 mode)
{
	if ((ObjectArc->Layer < 32) && ((ObjectArc->Info & OBJECT_FILLED) == 0))
		return 1;

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateTraceObjectFromSpecialLine(ObjectRecord * TraceObject, double StartX, double StartY, double Length,
                                       double Angle, double TraceWidth, int32 mode)
{
	double x1, y1, x2, y2, TraceLength;

	x1 = StartX;
	y1 = StartY;
	x2 = StartX + cos(Angle) * Length;
	y2 = StartY + sin(Angle) * Length;

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
		TraceObject->ObjectType = -1;
	else
	{
		if (InRange3(x1, x2))
		{	// Ver trace
			TraceObject->ObjectType = TRACE_VER;
			TraceLength = fabs(y1 - y2);
			y1 = min(y1, y2);
			TraceObject->x1 = x1;
			TraceObject->y1 = y1;
			TraceObject->x2 = TraceLength;
			TraceObject->y2 = TraceWidth;
		}
		else
		{
			if (InRange3(y1, y2))
			{	// Hor trace
				TraceObject->ObjectType = TRACE_HOR;
				TraceLength = fabs(x1 - x2);
				x1 = min(x1, x2);
				TraceObject->x1 = x1;
				TraceObject->y1 = y1;
				TraceObject->x2 = TraceLength;
				TraceObject->y2 = TraceWidth;
			}
			else
			{
				if (InRange3(x1 - x2, y1 - y2))
				{	// diag2 trace
					TraceObject->ObjectType = TRACE_DIAG2;
					TraceLength = fabs(x1 - x2);

					if (x2 < x1)
					{
						x1 = x2;
						y1 = y2;
					}

					TraceObject->x1 = x1;
					TraceObject->y1 = y1;
					TraceObject->x2 = TraceLength;
					TraceObject->y2 = TraceWidth;
				}
				else
				{
					if (InRange3(x1 - x2, y2 - y1))
					{	// diag1 trace
						TraceObject->ObjectType = TRACE_DIAG1;
						TraceLength = fabs(x1 - x2);

						if (x2 < x1)
						{
							x1 = x2;
							y1 = y2;
						}

						TraceObject->x1 = x1;
						TraceObject->y1 = y1;
						TraceObject->x2 = TraceLength;
						TraceObject->y2 = TraceWidth;
					}
					else
					{
						TraceObject->ObjectType = TRACE_ALL_ANGLE;
						TraceObject->x1 = x1;
						TraceObject->y1 = y1;
						TraceObject->x2 = x2;
						TraceObject->y2 = y2;
						TraceObject->Thickness = TraceWidth;
					}
				}
			}
		}
	}

	FillPositionObject(TraceObject);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CreateTraceObjectFromTrace(TraceRecord * Trace, ObjectRecord * TraceObject, int32 ObjectType, int32 Layer,
                                int32 TraceNr, int32 mode)
{
	switch (ObjectType)
	{
	case TRACE_VER:
		TraceObject->x1 = Trace->X;
		TraceObject->y1 = Trace->Y;
		TraceObject->Clearance = Trace->Clearance;

		if ((mode & 2) == 0)
		{
			TraceObject->ObjectType = TRACE_VER;
			TraceObject->x2 = Trace->Length;
			TraceObject->y2 = Trace->ThickNess;
			TraceObject->Thickness = 0.0;
		}
		else
		{
			TraceObject->ObjectType = OBJECT_LINE;
			TraceObject->x2 = Trace->X;
			TraceObject->y2 = Trace->Y + Trace->Length;
			TraceObject->Thickness = Trace->ThickNess;
		}

		TraceObject->Info = Trace->Info & OBJECT_SELECTED;

		if ((mode & 4) == 0)
			TraceObject->Layer = Layer;

		TraceObject->TraceNr = TraceNr;
		TraceObject->NetNr = Trace->NetNr;
		break;

	case TRACE_HOR:
		TraceObject->x1 = Trace->X;
		TraceObject->y1 = Trace->Y;
		TraceObject->Clearance = Trace->Clearance;

		if ((mode & 2) == 0)
		{
			TraceObject->ObjectType = TRACE_HOR;
			TraceObject->x2 = Trace->Length;
			TraceObject->y2 = Trace->ThickNess;
			TraceObject->Thickness = 0.0;
		}
		else
		{
			TraceObject->ObjectType = OBJECT_LINE;
			TraceObject->x2 = Trace->X + Trace->Length;
			TraceObject->y2 = Trace->Y;
			TraceObject->Thickness = Trace->ThickNess;
		}

		TraceObject->Info = Trace->Info & OBJECT_SELECTED;

		if ((mode & 4) == 0)
			TraceObject->Layer = Layer;

		TraceObject->TraceNr = TraceNr;
		TraceObject->NetNr = Trace->NetNr;
		break;

	case TRACE_DIAG1:
		TraceObject->x1 = Trace->X;
		TraceObject->y1 = Trace->Y;
		TraceObject->Clearance = Trace->Clearance;

		if ((mode & 2) == 0)
		{
			TraceObject->ObjectType = TRACE_DIAG1;
			TraceObject->x2 = Trace->Length;
			TraceObject->y2 = Trace->ThickNess;
			TraceObject->Thickness = 0.0;
		}
		else
		{
			TraceObject->ObjectType = OBJECT_LINE;
			TraceObject->x2 = Trace->X + Trace->Length;
			TraceObject->y2 = Trace->Y - Trace->Length;
			TraceObject->Thickness = Trace->ThickNess;
		}

		TraceObject->Info = Trace->Info & OBJECT_SELECTED;

		if ((mode & 4) == 0)
			TraceObject->Layer = Layer;

		TraceObject->TraceNr = TraceNr;
		TraceObject->NetNr = Trace->NetNr;
		break;

	case TRACE_DIAG2:
		TraceObject->x1 = Trace->X;
		TraceObject->y1 = Trace->Y;
		TraceObject->Clearance = Trace->Clearance;

		if ((mode & 2) == 0)
		{
			TraceObject->ObjectType = TRACE_DIAG2;
			TraceObject->x2 = Trace->Length;
			TraceObject->y2 = Trace->ThickNess;
			TraceObject->Thickness = 0.0;
		}
		else
		{
			TraceObject->ObjectType = OBJECT_LINE;
			TraceObject->x2 = Trace->X + Trace->Length;
			TraceObject->y2 = Trace->Y + Trace->Length;
			TraceObject->Thickness = Trace->ThickNess;
		}

		TraceObject->Info = Trace->Info & OBJECT_SELECTED;

		if ((mode & 4) == 0)
			TraceObject->Layer = Layer;

		TraceObject->TraceNr = TraceNr;
		TraceObject->NetNr = Trace->NetNr;
		break;
	}

	if ((mode & 1) == 1)
		FillPositionObject(TraceObject);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateViaObjectFromVia(ViaRecord * Via, ObjectRecord * ViaObject, int32 mode)
{
	ViaObject->x1 = Via->X;
	ViaObject->y1 = Via->Y;
	ViaObject->x2 = Via->ThickNess;
	ViaObject->y2 = Via->DrillThickNess;
	ViaObject->Clearance = Via->Clearance;
	ViaObject->ObjectType = VIA_PUT_THROUGH_ROUND;
	ViaObject->Info = Via->Info & OBJECT_SELECTED;

	if ((mode & 2) == 0)
		ViaObject->Layer = -1;

	ViaObject->TraceNr = 0;
	ViaObject->NetNr = Via->NetNr;

	if ((mode & 1) == 1)
		FillPositionObject(ViaObject);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateArcObjectFromArc(ObjectArcRecord * ObjectArc, ObjectRecord * ArcObject, int32 mode)
{
	if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
	{
		ArcObject->x1 = ObjectArc->CentreX;
		ArcObject->y1 = ObjectArc->CentreY;
		ArcObject->x2 = ObjectArc->Width;
		ArcObject->y2 = 0.0;
		ArcObject->x3 = 0.0;
		ArcObject->y3 = 0.0;
		ArcObject->x4 = 0.0;
		ArcObject->y4 = 0.0;
		ArcObject->Info = ArcObject->Info & OBJECT_SELECTED;

		if ((mode & 2) == 0)
			ArcObject->Layer = -1;

		if (ObjectArc->Layer == DRILL_LAYER)
			ArcObject->ObjectType = DRILL;
		else
			ArcObject->ObjectType = DRILL_UNPLATED;

		ArcObject->NetNr = ObjectArc->NetNr;
		ArcObject->Clearance = ObjectArc->Clearance;
		ArcObject->Thickness = ObjectArc->LineThickNess;

		if ((mode & 1) == 1)
			FillPositionObject(ArcObject);
	}

	if (ObjectArc->Layer < 32)
	{
		ArcObject->x1 = ObjectArc->CentreX;
		ArcObject->y1 = ObjectArc->CentreY;
		ArcObject->x2 = ObjectArc->Width;
		ArcObject->y2 = ObjectArc->Height;
		ArcObject->x3 = ObjectArc->StartDiffX;
		ArcObject->y3 = ObjectArc->StartDiffY;
		ArcObject->x4 = ObjectArc->EndDiffX;
		ArcObject->y4 = ObjectArc->EndDiffY;
		ArcObject->Info = ArcObject->Info & OBJECT_SELECTED;

		if ((mode & 2) == 0)
			ArcObject->Layer = ObjectArc->Layer;

		ArcObject->ObjectType = TRACE_ARC;
		ArcObject->NetNr = ObjectArc->NetNr;
		ArcObject->Clearance = ObjectArc->Clearance;
		ArcObject->Thickness = ObjectArc->LineThickNess;

		if ((mode & 1) == 1)
			FillPositionObject(ArcObject);

		return 0;
	}

	ArcObject->x1 = ObjectArc->CentreX;
	ArcObject->y1 = ObjectArc->CentreY;
	ArcObject->x2 = ObjectArc->Width;
	ArcObject->y2 = ObjectArc->Height;
	ArcObject->x3 = ObjectArc->StartDiffX;
	ArcObject->y3 = ObjectArc->StartDiffY;
	ArcObject->x4 = ObjectArc->EndDiffX;
	ArcObject->y4 = ObjectArc->EndDiffY;
	ArcObject->Info = ArcObject->Info & OBJECT_SELECTED;
	ArcObject->Layer = ObjectArc->Layer;
	ArcObject->ObjectType = OBJECT_ARC;
	ArcObject->Clearance = ObjectArc->Clearance;
	ArcObject->Thickness = ObjectArc->LineThickNess;

	if ((mode & 1) == 1)
		FillPositionObject(ArcObject);

	return 0;
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 AdjustOffsetForSnapOnObject(ObjectRecord * Object, double CursorX, double CursorY, double GridX, double GridY,
                                  double divx, double divy, double *ShiftOffsetX, double *ShiftOffsetY, int32 mode)
{
	PolygonRecord *PolygonObject;
	ObjectRecord NewObject;
	AreaFillRecord *AreaFill;
	int32 count, cnt2;
	double Length1, Length2, AddX, AddY, px1, py1, px2, py2, TempLength, x1, y1;
	uint8 PolygonBuf1[10240];
#ifdef _DEBUG
	int32 ok;
#endif

	PolygonObject = (PolygonRecord *) & PolygonBuf1;

	switch (Object->ObjectType)
	{
	case TRACE_ALL_ANGLE:
	case PIN_LINE_ALL_ANGLE:
	case OBJECT_LINE:
		Object->x1 += divx;
		Object->y1 += divy;
		Object->x2 += divx;
		Object->y2 += divy;
		Object->Thickness = max(Object->Thickness, 0.2e5);
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
		{	// Point within object snap it
			Length1 = CalcLengthLine(Object->x1, Object->y1, CursorX, CursorY);
			Length2 = CalcLengthLine(Object->x2, Object->y2, CursorX, CursorY);

			if (Length1 < Length2)
			{
				if ((mode & 2) == 0)
				{
					if ((mode & 1) == 0)
					{
						AddX = Object->x1 - GridX;
						AddY = Object->y1 - GridY;
					}
					else
					{
						AddX = Object->x1 - CursorX;
						AddY = Object->y1 - CursorY;
					}
				}
				else
				{
					AddX = GridX - Object->x1;
					AddY = GridY - Object->y1;
				}
			}
			else
			{
				if ((mode & 2) == 0)
				{
					if ((mode & 1) == 0)
					{
						AddX = Object->x2 - GridX;
						AddY = Object->y2 - GridY;
					}
					else
					{
						AddX = Object->x2 - CursorX;
						AddY = Object->y2 - CursorY;
					}
				}
				else
				{
					AddX = GridX - Object->x2;
					AddY = GridY - Object->y2;
				}
			}

			*ShiftOffsetX += AddX;
			*ShiftOffsetY += AddY;
			return 1;
		}

		break;

	case PIN_SMD_ROUND:
	case DRILL:
	case DRILL_UNPLATED:
	case PIN_PUT_THROUGH_ROUND:
#ifdef _DEBUG
		if ((InRange9(Object->x1, -7e5)) && (InRange9(Object->y1, -5.7e5)))
			ok = 1;

#endif
		Object->ObjectType = PIN_SMD_ROUND;
		Object->x1 = Object->x1 + divx;
		Object->y1 = Object->y1 + divy;
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
		{	// Point within object snap it
			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
				{
					AddX = Object->x1 - GridX;
					AddY = Object->y1 - GridY;
				}
				else
				{
					AddX = Object->x1 - CursorX;
					AddY = Object->y1 - CursorY;
				}
			}
			else
			{
				AddX = GridX - Object->x1;
				AddY = GridY - Object->y1;
			}

			*ShiftOffsetX += AddX;
			*ShiftOffsetY += AddY;
		}

		break;

	case PIN_SMD_RECT:
	case PIN_PUT_THROUGH_SQUARE:
		Object->x1 += divx;
		Object->y1 += divy;
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
		{	// Point within object snap it
			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
				{
					AddX = Object->x1 - GridX;
					AddY = Object->y1 - GridY;
				}
				else
				{
					AddX = Object->x1 - CursorX;
					AddY = Object->y1 - CursorY;
				}
			}
			else
			{
				AddX = GridX - Object->x1;
				AddY = GridY - Object->y1;
			}

			*ShiftOffsetX += AddX;
			*ShiftOffsetY += AddY;
			return 1;
		}

		break;

	case PIN_ARC:
	case OBJECT_ARC:
	case TRACE_ARC:
		Object->x2 = max(Object->Thickness, 1e5);
		Object->ObjectType = PIN_SMD_ROUND;
		Object->x1 = Object->x1 + divx;
		Object->y1 = Object->y1 + divy;
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
		{	// Point within object snap it
			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
				{
					AddX = Object->x1 - GridX;
					AddY = Object->y1 - GridY;
				}
				else
				{
					AddX = Object->x1 - CursorX;
					AddY = Object->y1 - CursorY;
				}
			}
			else
			{
				AddX = GridX - Object->x1;
				AddY = GridY - Object->y1;
			}

			*ShiftOffsetX += AddX;
			*ShiftOffsetY += AddY;
			return 1;
		}

		GetArcEndPoints(Object, &px1, &py1, &px2, &py2, 0);
		Object->ObjectType = PIN_SMD_ROUND;
		Object->x1 = px1 + divx;
		Object->y1 = py1 + divy;
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
		{	// Point within object snap it
			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
				{
					AddX = Object->x1 - GridX;
					AddY = Object->y1 - GridY;
				}
				else
				{
					AddX = Object->x1 - CursorX;
					AddY = Object->y1 - CursorY;
				}
			}
			else
			{
				AddX = GridX - Object->x1;
				AddY = GridY - Object->y1;
			}

			*ShiftOffsetX += AddX;
			*ShiftOffsetY += AddY;
			return 1;
		}

		Object->x1 = px2 + divx;
		Object->y1 = py2 + divy;
		MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
		{	// Point within object snap it
			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
				{
					AddX = Object->x1 - GridX;
					AddY = Object->y1 - GridY;
				}
				else
				{
					AddX = Object->x1 - CursorX;
					AddY = Object->y1 - CursorY;
				}
			}
			else
			{
				AddX = GridX - Object->x1;
				AddY = GridY - Object->y1;
			}

			*ShiftOffsetX += AddX;
			*ShiftOffsetY += AddY;
			return 1;
		}

		break;

	case PIN_SMD_POLYGON:
	case OBJECT_POLYGON:
	case PIN_PUT_THROUGH_POLYGON:
		if (CheckObjectIsBigPolygon(Object))
		{
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
			PolygonObject = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
		}
		else
			MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

		count = PolygonObject->NrVertices;

		for (cnt2 = 0; cnt2 < count; cnt2++)
		{
			x1 = PolygonObject->Points[cnt2].x;
			y1 = PolygonObject->Points[cnt2].y;
			NewObject.x1 = x1 + divx;
			NewObject.y1 = y1 + divy;

			if (CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY) < 0.25e5)
			{
				if ((mode & 2) == 0)
				{
					if ((mode & 1) == 0)
					{
						AddX = NewObject.x1 - GridX;
						AddY = NewObject.y1 - GridY;
					}
					else
					{
						AddX = NewObject.x1 - CursorX;
						AddY = NewObject.y1 - CursorY;
					}
				}
				else
				{
					AddX = GridX - NewObject.x1;
					AddY = GridY - NewObject.y1;
				}

				*ShiftOffsetX += AddX;
				*ShiftOffsetY += AddY;
				return 1;
			}
		}

		x1 = ((PolygonObject->minx + PolygonObject->maxx) * 0.5);
		y1 = ((PolygonObject->miny + PolygonObject->maxy) * 0.5);
		NewObject.x1 = x1 + divx;
		NewObject.y1 = y1 + divy;
		TempLength = CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);

		if (TempLength < 1e5)
		{
			if ((mode & 2) == 0)
			{
				if ((mode & 1) == 0)
				{
					AddX = NewObject.x1 - GridX;
					AddY = NewObject.y1 - GridY;
				}
				else
				{
					AddX = NewObject.x1 - CursorX;
					AddY = NewObject.y1 - CursorY;
				}
			}
			else
			{
				AddX = GridX - NewObject.x1;
				AddY = GridY - NewObject.y1;
			}

			*ShiftOffsetX += AddX;
			*ShiftOffsetY += AddY;
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

int32 GetDrawingLayer(int32 mode)
{
	int32 cnt, cnt2;

	if (CurrentDrawingLayer == -1)
		CurrentDrawingLayer = 0;

	if (CurrentDrawingLayer >= Design.NrBoardLayers)
		CurrentDrawingLayer = 0;

	if ((mode & 2) == 2)
	{
		// Get new CurrentDrawingLayer
		cnt = CurrentDrawingLayer;

		if ((Design.NrBoardLayers > 2) && (cnt == 0))
			cnt = Design.NrBoardLayers - 1;
		else
		{
			cnt++;

			if (cnt == Design.NrBoardLayers)
				cnt = 0;
		}
	}
	else
	{
		// Check CurrentDrawingLayer, change if necessary
		cnt = CurrentDrawingLayer;
	}

	cnt2 = 0;

	while (cnt2 < Design.NrBoardLayers)
	{
		if (DrawLayerCode[cnt] < 0x10)
		{
			if (cnt == 0)
			{
				if ((mode & 1) == 0)
					return cnt;

				if (OkToDrawBottomPads)
					return cnt;
			}
			else
			{
				if (cnt == Design.NrBoardLayers - 1)
				{
					if ((mode & 1) == 0)
						return cnt;

					if (OkToDrawTopPads)
						return cnt;
				}
				else
				{
					if ((mode & 1) == 0)
						return cnt;

					if (OkToDrawInnerPads)
						return cnt;
				}
			}
		}

		if ((Design.NrBoardLayers > 2) && (cnt == 0))
		{
			if ((mode & 4) == 0)
				cnt = Design.NrBoardLayers - 1;
			else
			{
				cnt++;

				if (cnt == Design.NrBoardLayers)
					cnt = 0;
			}
		}
		else
		{
			cnt++;

			if (cnt == Design.NrBoardLayers)
				cnt = 0;
		}

		cnt2++;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetCompProperties(CompRecord * Comp, LPSTR PropertyID, LPSTR PropertyValue, int32 mode)
{
	int32 cnt = 0, pos, pos2, LengthValue, LengthIdent;
	LPSTR CompPropertiesBuf;

	CompPropertiesBuf = (LPSTR) Comp->Properties;
	pos = 0;

	while ((pos < sizeof(Comp->Properties) - 2) && (CompPropertiesBuf[pos] != 0))
	{
		LengthIdent = strlen((LPSTR) & CompPropertiesBuf[pos]);

		if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(Comp->Properties) - 1))
		{
			pos2 = pos + LengthIdent + 1;
			LengthValue = strlen((LPSTR) & CompPropertiesBuf[pos2]);

			if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(Comp->Properties) - 1))
			{
				switch (mode & 0x60)
				{
				case 0:
					if (strcmpUTF8(PropertyID, &CompPropertiesBuf[pos]) == 0)
					{
						strcpy(PropertyValue, (LPSTR) & CompPropertiesBuf[pos2]);
						return 0;
					}

					break;

				case 0x20:
					if ((mode & 0x1f) == cnt)
					{
						strcpy(PropertyID, (LPSTR) & CompPropertiesBuf[pos]);
						strcpy(PropertyValue, (LPSTR) & CompPropertiesBuf[pos2]);
						return 0;
					}

					break;
				}

				if (cnt < 0x20)
					cnt++;
			}

			pos += LengthIdent + LengthValue + 2;
		}
		else
			break;
	}

	switch (mode & 0x60)
	{
	case 0:
	case 0x20:
		return -1;

	case 0x40:
		return cnt;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNetProperties(NetRecord * Net, LPSTR PropertyID, LPSTR PropertyValue, int32 mode)
{
	int32 cnt = 0, pos, pos2, LengthValue, LengthIdent;
	LPSTR NetPropertiesBuf;

	NetPropertiesBuf = (LPSTR) Net->Properties;
	pos = 0;

	while ((pos < sizeof(Net->Properties) - 2) && (NetPropertiesBuf[pos] != 0))
	{
		LengthIdent = strlen((LPSTR) & NetPropertiesBuf[pos]);

		if ((LengthIdent > 0) && (LengthIdent < 64) && (pos + LengthIdent < sizeof(Net->Properties) - 1))
		{
			pos2 = pos + LengthIdent + 1;
			LengthValue = strlen((LPSTR) & NetPropertiesBuf[pos2]);

			if ((LengthValue > 0) && (LengthValue < 64) && (pos2 + LengthValue < sizeof(Net->Properties) - 1))
			{
				switch (mode & 0x60)
				{
				case 0:
					if (strcmpUTF8(PropertyID, &NetPropertiesBuf[pos]) == 0)
					{
						strcpy(PropertyValue, (LPSTR) & NetPropertiesBuf[pos2]);
						return 0;
					}

					break;

				case 0x20:
					if ((mode & 0x1f) == cnt)
					{
						strcpy(PropertyID, (LPSTR) & NetPropertiesBuf[pos]);
						strcpy(PropertyValue, (LPSTR) & NetPropertiesBuf[pos2]);
						return 0;
					}

					break;
				}

				if (cnt < 0x20)
					cnt++;
			}

			pos += LengthIdent + LengthValue + 2;
		}
		else
			break;
	}

	switch (mode & 0x60)
	{
	case 0:
	case 0x20:
		return -1;

	case 0x40:
		return cnt;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
