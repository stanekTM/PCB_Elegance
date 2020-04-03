/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select2.c
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
#include "select2.h"
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


ObjectRecord FoundObject;


char OldInfoStr[MAX_LENGTH_STRING];
int32 ObjectKeepOut, ObjectKeepOut2;
int32 OldDrawingLayer = -1;


extern int32 TimerValue, TraceDrawingMode;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectObjectOnPointer(double x11, double y11, double x22, double y22, int32 mode)
{
	int32 cnt, cnt2, FoundGuideNr, FoundAllLayerNr, FoundAllLayerTraceType, FoundCurrentLayerTraceType, Layer,
	      ObjectLayer, FoundCurrentLayerTraceNr, FoundAllLayerTraceNr, FoundViaNr, NetNr, TraceInfo, ObjectType;
	double ConnectionX1, ConnectionY1, ConnectionX2, ConnectionY2, SearchMinX, SearchMinY, SearchMaxX, SearchMaxY,
	       MinLengthCurrentLayerPin, MinLengthAllLayerPin, MinLengthVia, MinLengthCurrentLayerTrace,
	       MinLengthAllLayerTrace, PointX, PointY, HulpLength, x1, y1, x2, y2, y2a, MinLengthGuide;
	ObjectRecord *Object, FoundAllLayerPinObject, TraceObject, FoundCurrentLayerPinObject;
	ConnectionsRecord *Connection;
	CompRecord *Comp;
	ViaRecord *Via;
	TraceRecord *Trace = NULL;
	uint8 PolygonBuf2[10240];
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;
	PolygonRecord *PolygonObject;

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	Layer = 0;
	PointX = (x11 + x22) / 2;
	PointY = (y11 + y22) / 2;

//  SearchMinX=PixelToRealOffX(min(x1,x2));
//  SearchMinY=PixelToRealOffY(DrawWindowMaxY-max(y1,y2)-1);
//  SearchMaxX=PixelToRealOffX(max(x1,x2));
//  SearchMaxY=PixelToRealOffY(DrawWindowMaxY-min(y1,y2)-1);

	FoundAllLayerTraceType = -1;
	FoundCurrentLayerTraceType = -1;
	FoundGuideNr = -1;
	FoundAllLayerTraceNr = -1;
	FoundCurrentLayerTraceNr = -1;
	FoundViaNr = -1;
	FoundAllLayerNr = -1;

	MinLengthGuide = 1e9;
	MinLengthCurrentLayerTrace = 1e9;
	MinLengthAllLayerTrace = 1e9;
	MinLengthAllLayerPin = 1e9;
	MinLengthCurrentLayerPin = 1e9;
	MinLengthVia = 1e9;
	SearchMinX = ViewMinX;
	SearchMaxX = ViewMaxX;
	SearchMinY = ViewMinY;
	SearchMaxY = ViewMaxY;
	ObjectLayer = -1;
	ObjectType = 0;
	memset(&FoundAllLayerPinObject, 0, sizeof(ObjectRecord));
	memset(&FoundCurrentLayerPinObject, 0, sizeof(ObjectRecord));

	if ((mode == 0) && (OkToDrawConnections))
	{
		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);
			NetNr = Connection->NetNr;

			if ((Connection->Info & (CONNECTIONS_DISABLED | OBJECT_NOT_VISIBLE)) == 0)
			{
				if ((Connection->Info & (OBJECT_HIGHLITED | CONNECTIONS_NOT_VISIBLE)) != CONNECTIONS_NOT_VISIBLE)
				{
					ConnectionX1 = Connection->x1;
					ConnectionY1 = Connection->y1;
					ConnectionX2 = Connection->x2;
					ConnectionY2 = Connection->y2;
					HulpLength =
					    DistancePointToConnection(PointX, PointY, ConnectionX1, ConnectionY1, ConnectionX2,
					                              ConnectionY2);

					if (HulpLength < MinLengthGuide)
					{
						MinLengthGuide = HulpLength;
						FoundGuideNr = cnt;
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		if ((TraceLayersSelectable[Layer] == -1) && (DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
		{
			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					x2 = x1;
					y2 = y1 + Trace->Length;
					y2a = Trace->ThickNess * 0.5;

					if ((x2 + y2a >= SearchMinX) && (x1 - y2a <= SearchMaxX) && (y2 + y2a >= SearchMinY)
					        && (y1 - y2a <= SearchMaxY))
					{
						HulpLength = DistancePointToTrace(PointX, PointY, x1, y1, x2, y2) + 100.0;
						CreateTraceObjectFromTrace(Trace, &TraceObject, TRACE_VER, Layer, cnt, 0);
						MakePolygonFromObject(&TraceObject, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = (float) 100.0;

						if (HulpLength < MinLengthAllLayerTrace)
						{
							MinLengthAllLayerTrace = HulpLength;
							FoundAllLayerTraceType = TRACE_VER;
							FoundAllLayerNr = Layer;
							FoundAllLayerTraceNr = cnt;
						}

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Layer)
						        && (HulpLength < MinLengthCurrentLayerTrace))
						{
							MinLengthCurrentLayerTrace = HulpLength;
							FoundCurrentLayerTraceType = TRACE_VER;
							FoundCurrentLayerTraceNr = cnt;
						}
					}
				}
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					x2 = x1 + Trace->Length;
					y2 = y1;
					y2a = Trace->ThickNess * 0.5;

					if ((x2 + y2a >= SearchMinX) && (x1 - y2a <= SearchMaxX) && (y2 + y2a >= SearchMinY)
					        && (y1 - y2a <= SearchMaxY))
					{
						HulpLength = DistancePointToTrace(PointX, PointY, x1, y1, x2, y2) + 100.0;
						CreateTraceObjectFromTrace(Trace, &TraceObject, TRACE_HOR, Layer, cnt, 0);
						MakePolygonFromObject(&TraceObject, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 100.0;

						if (HulpLength < MinLengthAllLayerTrace)
						{
							MinLengthAllLayerTrace = HulpLength;
							FoundAllLayerTraceType = TRACE_HOR;
							FoundAllLayerNr = Layer;
							FoundAllLayerTraceNr = cnt;
						}

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Layer)
						        && (HulpLength < MinLengthCurrentLayerTrace))
						{
							MinLengthCurrentLayerTrace = HulpLength;
							FoundCurrentLayerTraceType = TRACE_HOR;
							FoundCurrentLayerTraceNr = cnt;
						}
					}
				}
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					x2 = x1 + Trace->Length;
					y2 = y1 - Trace->Length;
					y2a = Trace->ThickNess * 0.5;

					if ((x2 + y2a >= SearchMinX) && (x1 - y2a <= SearchMaxX) && (y1 + y2a >= SearchMinY)
					        && (y2 - y2a <= SearchMaxY))
					{
						HulpLength = DistancePointToTrace(PointX, PointY, x1, y1, x2, y2) + 100.0;
						CreateTraceObjectFromTrace(Trace, &TraceObject, TRACE_DIAG1, Layer, cnt, 0);
						MakePolygonFromObject(&TraceObject, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 100.0;

						if (HulpLength < MinLengthAllLayerTrace)
						{
							MinLengthAllLayerTrace = HulpLength;
							FoundAllLayerTraceType = TRACE_DIAG1;
							FoundAllLayerNr = Layer;
							FoundAllLayerTraceNr = cnt;
						}

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Layer)
						        && (HulpLength < MinLengthCurrentLayerTrace))
						{
							MinLengthCurrentLayerTrace = HulpLength;
							FoundCurrentLayerTraceType = TRACE_DIAG1;
							FoundCurrentLayerTraceNr = cnt;
						}
					}
				}
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);
				TraceInfo = Trace->Info;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					x2 = x1 + Trace->Length;
					y2 = y1 + Trace->Length;
					y2a = Trace->ThickNess * 0.5;

					if ((x2 + y2a >= SearchMinX) && (x1 - y2a <= SearchMaxX) && (y2 + y2a >= SearchMinY)
					        && (y1 - y2a <= SearchMaxY))
					{
						HulpLength = DistancePointToTrace(PointX, PointY, x1, y1, x2, y2) + 100.0;
						CreateTraceObjectFromTrace(Trace, &TraceObject, TRACE_DIAG2, Layer, cnt, 0);
						MakePolygonFromObject(&TraceObject, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 100.0;

						if (HulpLength < MinLengthAllLayerTrace)
						{
							MinLengthAllLayerTrace = HulpLength;
							FoundAllLayerTraceNr = cnt;
							FoundAllLayerTraceType = TRACE_DIAG2;
							FoundAllLayerNr = Layer;
						}

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Layer)
						        && (HulpLength < MinLengthCurrentLayerTrace))
						{
							MinLengthCurrentLayerTrace = HulpLength;
							FoundCurrentLayerTraceType = TRACE_DIAG2;
							FoundCurrentLayerTraceNr = cnt;
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
			x1 = ObjectLine->X1;
			y1 = ObjectLine->Y1;
			x2 = ObjectLine->X2;
			y2 = ObjectLine->Y2;
			y2a = ObjectLine->LineThickNess * 0.5;

			if ((max(x1, x2) + y2a >= SearchMinX) && (min(x1, x2) - y2a <= SearchMaxX)
			        && (max(y1, y2) + y2a >= SearchMinY) && (min(y1, y2) - y2a <= SearchMaxY))
			{
				HulpLength = DistancePointToTrace(PointX, PointY, x1, y1, x2, y2) + 100.0;

				if (HulpLength < MinLengthAllLayerTrace)
				{
					MinLengthAllLayerTrace = HulpLength;
					FoundAllLayerTraceNr = cnt;
					FoundAllLayerTraceType = TRACE_ALL_ANGLE;
					FoundAllLayerNr = ObjectLine->Layer;
				}

				if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Layer)
				        && (HulpLength < MinLengthCurrentLayerTrace))
				{
					MinLengthCurrentLayerTrace = HulpLength;
					FoundCurrentLayerTraceType = TRACE_ALL_ANGLE;
					FoundCurrentLayerTraceNr = cnt;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0) && (ObjectArc->Layer < 32))
		{
			x1 = ObjectArc->CentreX;
			y1 = ObjectArc->CentreY;
			y2a = (ObjectArc->Width + ObjectArc->LineThickNess) * 0.5;

			if ((x1 + y2a >= SearchMinX) && (x1 - y2a <= SearchMaxX) && (y1 + y2a >= SearchMinY)
			        && (y1 - y2a <= SearchMaxY))
			{
				HulpLength =
				    DistancePointToArc(PointX, PointY, ObjectArc->CentreX, ObjectArc->CentreY, ObjectArc->Width,
				                       ObjectArc->Height, ObjectArc->StartDiffX, ObjectArc->StartDiffY,
				                       ObjectArc->EndDiffX, ObjectArc->EndDiffY);

				if (HulpLength < MinLengthAllLayerTrace)
				{
					MinLengthAllLayerTrace = HulpLength;
					FoundAllLayerTraceNr = cnt;
					FoundAllLayerTraceType = TRACE_ARC;
					FoundAllLayerNr = ObjectArc->Layer;
				}

				if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Layer)
				        && (HulpLength < MinLengthCurrentLayerTrace))
				{
					MinLengthCurrentLayerTrace = HulpLength;
					FoundCurrentLayerTraceType = TRACE_ARC;
					FoundCurrentLayerTraceNr = cnt;
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Via->X;
			y1 = Via->Y;
			y2a = Via->ThickNess * 0.5;

			if ((x1 + y2a >= SearchMinX) && (x1 - y2a <= SearchMaxX) && (y1 + y2a >= SearchMinY)
			        && (y1 - y2a <= SearchMaxY))
			{
				HulpLength = (float) sqrt((PointX - x1) * (PointX - x1) + (PointY - y1) * (PointY - y1));

//        if (HulpLength<y2a) HulpLength=20000.0;
				if (HulpLength < y2a)
					HulpLength = 50.0;

				if (HulpLength < MinLengthVia)
				{
					MinLengthVia = HulpLength;
					FoundViaNr = cnt;
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Comp->BoardPosMinX;
			y1 = Comp->BoardPosMinY;
			x2 = Comp->BoardPosMaxX;
			y2 = Comp->BoardPosMaxY;

			if ((SearchMaxX > x1) && (SearchMinX < x2) && (SearchMaxY > y1) && (SearchMinY < y2))
			{
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
					Object->Info &= ~OBJECT_DONE;
				}

// *******************************************************************************************************
				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					switch (Object->ObjectType)
					{
					case DRILL:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);

						if (HulpLength < Object->x2 * 0.5)
							HulpLength = 200.0;

						if (HulpLength < MinLengthAllLayerPin)
						{
							MinLengthAllLayerPin = HulpLength;
							memmove(&FoundAllLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_PUT_THROUGH_ROUND:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);

//                if (HulpLength<Object->x2*0.5) HulpLength=20000.0;
						if (HulpLength < Object->x2 * 0.5)
							HulpLength = 200.0;

						if (HulpLength < MinLengthAllLayerPin)
						{
							MinLengthAllLayerPin = HulpLength;
							memmove(&FoundAllLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_PUT_THROUGH_SQUARE:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						HulpLength = CalcLengthLine(x1, y1, PointX, PointY);
						/*
						              if ((fabs(x1a-x1)<Object->x2*0.5)
						                 &&
						                 (fabs(y1a-y1)<Object->x2*0.5)) {
						                HulpLength=20000.0;
						              }
						*/
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if (HulpLength < MinLengthAllLayerPin)
						{
							MinLengthAllLayerPin = HulpLength;
							memmove(&FoundAllLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_PUT_THROUGH_POLYGON:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if (HulpLength < MinLengthAllLayerPin)
						{
							MinLengthAllLayerPin = HulpLength;
							memmove(&FoundAllLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_SMD_ROUND:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);
//                if (HulpLength<Object->x2*0.5) HulpLength=20000.0;
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						DrawCode = DrawLayerCode[Object->Layer];

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS)
						        && (HulpLength < MinLengthAllLayerPin))
						{
							MinLengthAllLayerPin = HulpLength;
							memmove(&FoundAllLayerPinObject, Object, sizeof(ObjectRecord));
						}

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_SMD_RECT:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);
						/*
						              if ((fabs(PointX-x1)<Object->x2*0.5)
						                 &&
						                 (fabs(PointY-y1)<Object->y2*0.5)) {
						                HulpLength=(float)20000.0;
						              }
						*/
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						DrawCode = DrawLayerCode[Object->Layer];

						if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS)
						        && (HulpLength < MinLengthAllLayerPin))
						{
							MinLengthAllLayerPin = HulpLength;
							memmove(&FoundAllLayerPinObject, Object, sizeof(ObjectRecord));
						}

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_SMD_POLYGON:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						HulpLength = (float) sqrt((PointX - x1) * (PointX - x1) + (PointY - y1) * (PointY - y1));
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_LINE_HOR:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1 + Object->x2 * 0.5;
						y1 = Object->y1;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);
						/*
						              if ((fabs(PointX-x1)<Object->x2*0.5)
						                 &&
						                 (fabs(PointY-y1)<Object->y2*0.5)) {
						                HulpLength=(float)20000.0;
						              }
						*/
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_LINE_VER:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1 + Object->x2 * 0.5;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);
						/*
						              if ((fabs(PointX-x1)<Object->y2*0.5)
						                 &&
						                 (fabs(PointY-y1)<Object->x2*0.5)) {
						                HulpLength=(float)20000.0;
						              }
						*/
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_LINE_DIAG1:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1 + Object->x2 * 0.5;
						y1 = Object->y1 - Object->x2 * 0.5;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);
						/*
						              if ((fabs(PointX-x1)<Object->x2*0.5)
						                 &&
						                 (fabs(PointY-y1)<Object->y2*0.5)) {
						                HulpLength=(float)20000.0;
						              }
						*/
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_LINE_DIAG2:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1 + Object->x2 * 0.5;
						y1 = Object->y1 + Object->x2 * 0.5;
						HulpLength = (float) CalcLengthLine(x1, y1, PointX, PointY);
						/*
						              if ((fabs(PointX-x1)<Object->x2*0.5)
						                 &&
						                 (fabs(PointY-y1)<Object->y2*0.5)) {
						                HulpLength=(float)20000.0;
						              }
						*/
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_ARC:
						Object->Info |= OBJECT_DONE;
						GetArcEndPoints(&FoundObject, &x1, &y1, &x2, &y2, 0);
						HulpLength = DistancePointToTrace(PointX, PointY, x1, y1, x2, y2);
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;

					case PIN_LINE_ALL_ANGLE:
						Object->Info |= OBJECT_DONE;
						x1 = Object->x1;
						y1 = Object->y1;
						x2 = Object->x2;
						y2 = Object->y2;
						HulpLength = DistancePointToTrace(PointX, PointY, x1, y1, x2, y2);
						MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

						if ((PointInPolygon(PolygonObject, PointX, PointY) & 1) == 1)
							HulpLength = 200.0;

						if ((CurrentDrawingLayer != -1) && (CurrentDrawingLayer == Object->Layer)
						        && (HulpLength < MinLengthCurrentLayerPin))
						{
							MinLengthCurrentLayerPin = HulpLength;
							memmove(&FoundCurrentLayerPinObject, Object, sizeof(ObjectRecord));
						}

						break;
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	/*
	  MinLengthCurrentLayerPin+=100.0;
	  MinLengthVia+=300.0;
	  MinLengthCurrentLayerTrace+=20000.0;
	*/
	MinLengthAllLayerPin += 200.0;
	MinLengthAllLayerTrace += 200.0;

	if ((MinLengthGuide > 3e5) && (MinLengthCurrentLayerPin > 3e5) && (MinLengthAllLayerPin > 3e5)
	        && (MinLengthCurrentLayerTrace > 3e5) && (MinLengthAllLayerTrace > 3e5) && (MinLengthVia > 3e5))
		return;

	if (mode == 0)
	{	// Only draw from pins/vias

		if ((MinLengthGuide < MinLengthCurrentLayerPin) && (MinLengthGuide < MinLengthAllLayerPin)
		        && (MinLengthGuide < MinLengthCurrentLayerTrace) && (MinLengthGuide < MinLengthAllLayerTrace)
		        && (MinLengthGuide < MinLengthVia))
		{
			FoundObject.TraceNr = FoundGuideNr;
			FoundObject.Layer = -1;
			FoundObject.ObjectType = CONNECTION;
			DrawTraceFromGuide(PointX, PointY);
			SetInfoStr(0);
			return;
		}

		if ((MinLengthCurrentLayerPin < MinLengthAllLayerPin) && (MinLengthCurrentLayerPin < MinLengthCurrentLayerTrace)
		        && (MinLengthCurrentLayerPin < MinLengthAllLayerTrace) && (MinLengthCurrentLayerPin < MinLengthVia)
		        && (MinLengthCurrentLayerPin < MinLengthGuide))
		{
			memmove(&FoundObject, &FoundCurrentLayerPinObject, sizeof(ObjectRecord));
			DrawTraceFromPinOrVia(PointX, PointY);
			SetInfoStr(0);
			return;
		}

		if ((MinLengthAllLayerPin < MinLengthCurrentLayerPin) && (MinLengthAllLayerPin < MinLengthCurrentLayerTrace)
		        && (MinLengthAllLayerPin < MinLengthAllLayerTrace) && (MinLengthAllLayerPin < MinLengthVia)
		        && (MinLengthAllLayerPin < MinLengthGuide))
		{
			memmove(&FoundObject, &FoundAllLayerPinObject, sizeof(ObjectRecord));
			DrawTraceFromPinOrVia(PointX, PointY);
			SetInfoStr(0);
			return;
		}

		if ((MinLengthVia < MinLengthCurrentLayerPin) && (MinLengthVia < MinLengthAllLayerPin)
		        && (MinLengthVia < MinLengthCurrentLayerTrace) && (MinLengthVia < MinLengthAllLayerTrace)
		        && (MinLengthVia < MinLengthGuide))
		{
			FoundObject.TraceNr = FoundViaNr;
			FoundObject.Layer = -1;
			FoundObject.ObjectType = VIA_PUT_THROUGH_ROUND;
			DrawTraceFromPinOrVia(PointX, PointY);
			SetInfoStr(0);
			return;
		}
	}

//  MinLengthAllLayerTrace=max(.3e5,MinLengthAllLayerTrace);

	if ((MinLengthCurrentLayerTrace < MinLengthVia) && (MinLengthCurrentLayerTrace < MinLengthAllLayerPin)
	        && (MinLengthCurrentLayerTrace < MinLengthCurrentLayerPin)
	        && (MinLengthCurrentLayerTrace < MinLengthAllLayerTrace) && (MinLengthCurrentLayerTrace < MinLengthGuide))
	{
		FoundObject.Layer = CurrentDrawingLayer;
		FoundObject.TraceNr = FoundCurrentLayerTraceNr;
		FoundObject.ObjectType = FoundCurrentLayerTraceType;
		DrawTraceFromTrace(PointX, PointY);
		SetInfoStr(0);
		return;
	}

	if ((MinLengthAllLayerTrace < MinLengthVia) && (MinLengthAllLayerTrace < MinLengthAllLayerPin)
	        && (MinLengthAllLayerTrace < MinLengthCurrentLayerTrace) && (MinLengthAllLayerTrace < MinLengthCurrentLayerPin)
	        && (MinLengthAllLayerTrace < MinLengthGuide))
	{
		FoundObject.Layer = FoundAllLayerNr;
		FoundObject.TraceNr = FoundAllLayerTraceNr;
		FoundObject.ObjectType = FoundAllLayerTraceType;
		DrawTraceFromTrace(PointX, PointY);
		SetInfoStr(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTraceFromGuide(double PointX, double PointY)
{
	int32 cnt, cnt2, ConnectionObjectNr1, ConnectionObjectNr2, ConnectionObjectNr1a, ConnectionObjectNr2a, NetNr, Layer,
	      Layer1, Layer2;
	double ConnectionX1, ConnectionY1, ConnectionX2, ConnectionY2, hulp1, hulp2, divx, x1, y1, x2, y2, Angle1, Angle2;
	ConnectionsRecord *Connection;
	NetRecord *Net;
	ObjectRecord *Object1, *Object2, *Object1a, *Object2a, *Object3;

	int32 FoundLayer = 0;
	int32 FoundTrace = 0;

	Layer = 0;
	OldDrawingLayer = CurrentDrawingLayer;
	strcpy(OldInfoStr, InfoStr);

	EndPreviousTraceX = 1e9;
	EndPreviousTraceY = 1e9;
	StartPreviousTraceX = 1e9;
	StartPreviousTraceY = 1e9;

	CurrentGuideNr = FoundObject.TraceNr;
	DrawTraceUsingGuide = 1;
	Connection = &((*Connections)[FoundObject.TraceNr]);
	ConnectionX1 = Connection->x1;
	ConnectionY1 = Connection->y1;
	ConnectionX2 = Connection->x2;
	ConnectionY2 = Connection->y2;

	NetNr = Connection->NetNr;
	GetObjectsNet(NetNr, MODE_OBJECTS3, 0);
	Net = &((*Nets)[NetNr]);
// NrObjects3
	CurrentDrawingNetNr = NetNr;

	PreviousDirection = -1;
//        DrawObjects2Magenta(0);
	ConnectionObjectNr1 = GetObjectNrFromEndPoint(Connection->Layer, ConnectionX1, ConnectionY1, 1);
	ConnectionObjectNr2 = GetObjectNrFromEndPoint(Connection->Layer, ConnectionX2, ConnectionY2, 1);
	ConnectionObjectNr1a = GetObjectNrFromEndPoint(-1, ConnectionX1, ConnectionY1, 4 + 1);
	ConnectionObjectNr2a = GetObjectNrFromEndPoint(-1, ConnectionX2, ConnectionY2, 4 + 1);

	if (((ConnectionObjectNr1 == -1) || (ConnectionObjectNr1 >= NrObjects3))
	        && ((ConnectionObjectNr2 == -1) || (ConnectionObjectNr2 >= NrObjects3)))
		return;

	hulp1 = sqrt((PointX - ConnectionX1) * (PointX - ConnectionX1) + (PointY - ConnectionY1) * (PointY - ConnectionY1));
	hulp2 = sqrt((PointX - ConnectionX2) * (PointX - ConnectionX2) + (PointY - ConnectionY2) * (PointY - ConnectionY2));

	divx = fabs(PixelToRealOffX(0) - PixelToRealOffX(1));

	NullObject.ObjectType = 0;
	NullObject.Layer = 0;

	if (ConnectionObjectNr1 == -1)
	{
		Object1 = &NullObject;
		Object1->Test = -1;
	}
	else
	{
		Object1 = &((*Objects3)[ConnectionObjectNr1]);
		Object1->Test = ConnectionObjectNr1;
	}

	if (ConnectionObjectNr2 == -1)
	{
		Object2 = &NullObject;
		Object2->Test = -1;
	}
	else
	{
		Object2 = &((*Objects3)[ConnectionObjectNr2]);
		Object2->Test = ConnectionObjectNr2;
	}

// **************************************************************************************************************
// **************************************************************************************************************

	if (hulp1 < hulp2)
	{
		switch (Object1->ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			CurrentTraceWidth = Object1->y2;
			CurrentClearance = Object1->Clearance;
			FoundTrace = 1;
			break;

		case TRACE_ARC:
		case TRACE_ALL_ANGLE:
			CurrentTraceWidth = Object1->Thickness;
			CurrentClearance = Object1->Clearance;
			FoundTrace = 1;
			break;

		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
		case DRILL:
		case PIN_PUT_THROUGH_ROUND:
		case VIA_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
		case PIN_ARC:
		case PIN_LINE_ALL_ANGLE:
			if (ConnectionObjectNr1a != -1)
			{
				Object1a = &((*Objects3)[ConnectionObjectNr1a]);

				switch (Object1a->ObjectType)
				{
				case TRACE_HOR:
				case TRACE_VER:
				case TRACE_DIAG1:
				case TRACE_DIAG2:
					CurrentTraceWidth = Object1a->y2;
					CurrentClearance = Object1a->Clearance;
					FoundTrace = 1;
					break;

				case TRACE_ARC:
				case TRACE_ALL_ANGLE:
					CurrentTraceWidth = Object1a->Thickness;
					CurrentClearance = Object1a->Clearance;
					FoundTrace = 1;
					break;
				}
			}

			break;
		}
	}
	else
	{
		switch (Object2->ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			CurrentTraceWidth = Object2->y2;
			CurrentClearance = Object2->Clearance;
			FoundTrace = 1;
			break;

		case TRACE_ARC:
		case TRACE_ALL_ANGLE:
			CurrentTraceWidth = Object2->Thickness;
			CurrentClearance = Object2->Clearance;
			FoundTrace = 1;
			break;

		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
		case DRILL:
		case PIN_PUT_THROUGH_ROUND:
		case VIA_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
		case PIN_ARC:
		case PIN_LINE_ALL_ANGLE:
			if (ConnectionObjectNr2a != -1)
			{
				Object2a = &((*Objects3)[ConnectionObjectNr2a]);

				switch (Object2a->ObjectType)
				{
				case TRACE_HOR:
				case TRACE_VER:
				case TRACE_DIAG1:
				case TRACE_DIAG2:
					CurrentTraceWidth = Object2a->y2;
					CurrentClearance = Object2a->Clearance;
					FoundTrace = 1;
					break;

				case TRACE_ARC:
				case TRACE_ALL_ANGLE:
					CurrentTraceWidth = Object2a->Thickness;
					CurrentClearance = Object2a->Clearance;
					FoundTrace = 1;
					break;
				}
			}

			break;
		}
	}

	if ((!FoundTrace) || (CurrentTraceWidth == 0) || (CurrentClearance == 0))
	{
		CurrentTraceWidth = Net->TraceWidth;
		CurrentClearance = Net->TraceClearance;
	}


	Layer1 = Object1->Layer;
	Layer2 = Object2->Layer;

	if ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10)
	{
		if (Design.NrBoardLayers == 2)
		{
			CurrentDrawingLayer ^= 1;

			if ((DrawLayerCode[CurrentDrawingLayer] & 0x10) == 0x10)
				return;
		}
		else
		{
			cnt = 0;

			while ((cnt < Design.NrBoardLayers) && (DrawLayerCode[cnt] >= 0x10))
				cnt++;

			if (cnt == Design.NrBoardLayers)
				return;

			CurrentDrawingLayer = cnt;
		}
	}

	if (CurrentDrawingLayer == -1)
	{
		if ((Layer1 == -1) && (Layer2 == -1))
		{
			Layer = 0;

			if (DrawLayerCode[Layer] < 0x10)
				FoundLayer = 1;
		}

		if ((!FoundLayer) && (Layer1 == -1))
		{
			Layer = Layer2;

			if (DrawLayerCode[Layer] < 0x10)
				FoundLayer = 1;
		}

		if ((!FoundLayer) && (Layer2 == -1))
		{
			Layer = Layer1;

			if (DrawLayerCode[Layer] < 0x10)
				FoundLayer = 1;
		}

		if (!FoundLayer)
		{
			switch (Object1->ObjectType)
			{
			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
			case PIN_SMD_POLYGON:
			case PIN_ARC:
			case PIN_LINE_ALL_ANGLE:
				if ((Object2->ObjectType == TRACE_HOR) || (Object2->ObjectType == TRACE_VER)
				        || (Object2->ObjectType == TRACE_DIAG1) || (Object2->ObjectType == TRACE_DIAG2)
				        || (Object2->ObjectType == TRACE_ALL_ANGLE) || (Object2->ObjectType == TRACE_ARC))
				{
					Layer = Layer1;

					if (DrawLayerCode[Layer] < 0x10)
						FoundLayer = 1;
				}
			}
		}

		if (!FoundLayer)
		{
			switch (Object2->ObjectType)
			{
			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
			case PIN_SMD_POLYGON:
			case PIN_ARC:
			case PIN_LINE_ALL_ANGLE:
				if ((Object1->ObjectType == TRACE_HOR) || (Object1->ObjectType == TRACE_VER)
				        || (Object1->ObjectType == TRACE_DIAG1) || (Object1->ObjectType == TRACE_DIAG2)
				        || (Object1->ObjectType == TRACE_ALL_ANGLE) || (Object1->ObjectType == TRACE_ARC))
				{
					Layer = Layer2;

					if (DrawLayerCode[Layer] < 0x10)
						FoundLayer = 1;
				}
			}
		}

		if (!FoundLayer)
		{
			if (hulp1 < hulp2)
				Layer = Layer1;
			else
				Layer = Layer2;
		}
	}
	else
	{
// **************************************************************************************************************
// **************************************************************************************************************
		if ((Layer1 == -1) && (Layer2 == -1))
		{
			Layer = CurrentDrawingLayer;

			if (DrawLayerCode[Layer] < 0x10)
				FoundLayer = 1;
		}

		if ((!FoundLayer) && (Layer1 == -1))
		{
			if (hulp1 < hulp2)
				Layer = CurrentDrawingLayer;
			else
				Layer = Layer2;

			if (DrawLayerCode[Layer] < 0x10)
				FoundLayer = 1;
		}

		if ((!FoundLayer) && (Layer2 == -1))
		{
			if (hulp2 < hulp1)
				Layer = CurrentDrawingLayer;
			else
				Layer = Layer1;

			if (DrawLayerCode[Layer] < 0x10)
				FoundLayer = 1;
		}

		if (!FoundLayer)
		{
			if (hulp1 < hulp2)
				Layer = Layer1;
			else
				Layer = Layer2;
		}
	}

// **************************************************************************************************************
// **************************************************************************************************************

	if (Layer == -1)
	{
		if ((Layer1 == -1) || (Layer2 == -1))
		{
			cnt = 1;

			while ((cnt < Design.NrBoardLayers) && (DrawLayerCode[cnt] >= 0x10))
				cnt++;

			if (cnt == Design.NrBoardLayers)
				return;

			Layer = cnt;
		}

	}

	CurrentClearance = CurrentClearance;
	CurrentDrawingLayer = Layer;

	if ((DrawLayerCode[CurrentDrawingLayer] >= 0) && (DrawLayerCode[CurrentDrawingLayer] < MAX_ACTIVE_DRAWING_LAYERS))
	{
		CurrentWorkingTrace.ObjectType = 0;

		if (hulp1 < hulp2)
		{
			switch (Object1->ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
				CurrentTraceWidth = Object1->y2;
				CurrentClearance = Object1->Clearance;
				FoundTrace = 1;
				break;
			}

			if ((!FoundTrace) || (CurrentTraceWidth == 0) || (CurrentClearance == 0))
			{
				CurrentTraceWidth = Net->TraceWidth;
				CurrentClearance = Net->TraceClearance;
			}

			CurrentDrawX1 = ConnectionX1;
			CurrentDrawY1 = ConnectionY1;
			CurrentDrawX2 = ConnectionX1;
			CurrentDrawY2 = ConnectionY1;
			DrawingConnectionX1 = ConnectionX1;
			DrawingConnectionY1 = ConnectionY1;
			DrawingConnectionX2 = ConnectionX2;
			DrawingConnectionY2 = ConnectionY2;
			memmove(&ConnectionObject1, Object1, sizeof(ObjectRecord));
			memmove(&ConnectionObject2, Object2, sizeof(ObjectRecord));
			memmove(&CurrentWorkingTrace, Object1, sizeof(ObjectRecord));
			CurrentWorkingTrace.TraceNr = ConnectionObjectNr1;
			ObjectKeepOut = ConnectionObjectNr1;
		}
		else
		{
			CurrentDrawX1 = ConnectionX2;
			CurrentDrawY1 = ConnectionY2;
			CurrentDrawX2 = ConnectionX2;
			CurrentDrawY2 = ConnectionY2;
			DrawingConnectionX1 = ConnectionX2;
			DrawingConnectionY1 = ConnectionY2;
			DrawingConnectionX2 = ConnectionX1;
			DrawingConnectionY2 = ConnectionY1;
			memmove(&ConnectionObject1, Object2, sizeof(ObjectRecord));
			memmove(&ConnectionObject2, Object1, sizeof(ObjectRecord));
			memmove(&CurrentWorkingTrace, Object2, sizeof(ObjectRecord));
			CurrentWorkingTrace.TraceNr = ConnectionObjectNr2;
			ObjectKeepOut = ConnectionObjectNr2;
		}

		ObjectKeepOut2 = GetObjectNrFromEndPoint(CurrentDrawingLayer, DrawingConnectionX1, DrawingConnectionY1, 3);


		cnt2 = 0;

		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object3 = &((*Objects3)[cnt]);

			if (Object3->ObjectType == CONNECTION)
				cnt2++;
		}

		if (Units == 0)
		{
			sprintf(InfoStr, "%s  [ %d ]   ( %.1f , %.1f thou )", Net->Name, cnt2, CurrentTraceWidth / 2540.0,
			        CurrentClearance / 2540.0);
		}
		else
		{
			sprintf(InfoStr, "7%s  [ %d ]   ( %.4f , %.4f mm )", Net->Name, cnt2, CurrentTraceWidth / 100000.0,
			        CurrentClearance / 100000.0);
		}

		RedrawInfoStr(1);
		CurrentWorkingTrace.Info2 = 0;

		switch (ConnectionObject1.ObjectType)
		{
		case TRACE_HOR:
			if ((InRange(CurrentDrawX1, ConnectionObject1.x1)) && (InRange(CurrentDrawY1, ConnectionObject1.y1)))
			{
				EndPreviousTraceX = ConnectionObject1.x1;
				EndPreviousTraceY = ConnectionObject1.y1;
				StartPreviousTraceX = ConnectionObject1.x1 + ConnectionObject1.x2;
				StartPreviousTraceY = ConnectionObject1.y1;
			}
			else
			{
				EndPreviousTraceX = ConnectionObject1.x1 + ConnectionObject1.x2;
				EndPreviousTraceY = ConnectionObject1.y1;
				StartPreviousTraceX = ConnectionObject1.x1;
				StartPreviousTraceY = ConnectionObject1.y1;
			}

			PreviousDirection =
			    GetNewDirection(StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX, EndPreviousTraceY, 0);
			break;

		case TRACE_VER:
			if ((InRange(CurrentDrawX1, ConnectionObject1.x1)) && (InRange(CurrentDrawY1, ConnectionObject1.y1)))
			{
				EndPreviousTraceX = ConnectionObject1.x1;
				EndPreviousTraceY = ConnectionObject1.y1;
				StartPreviousTraceX = ConnectionObject1.x1;
				StartPreviousTraceY = ConnectionObject1.y1 + ConnectionObject1.x2;
			}
			else
			{
				EndPreviousTraceX = ConnectionObject1.x1;
				EndPreviousTraceY = ConnectionObject1.y1 + ConnectionObject1.x2;
				StartPreviousTraceX = ConnectionObject1.x1;
				StartPreviousTraceY = ConnectionObject1.y1;
			}

			PreviousDirection =
			    GetNewDirection(StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX, EndPreviousTraceY, 0);
			break;

		case TRACE_DIAG1:
			if ((InRange(CurrentDrawX1, ConnectionObject1.x1)) && (InRange(CurrentDrawY1, ConnectionObject1.y1)))
			{
				EndPreviousTraceX = ConnectionObject1.x1;
				EndPreviousTraceY = ConnectionObject1.y1;
				StartPreviousTraceX = ConnectionObject1.x1 + ConnectionObject1.x2;
				StartPreviousTraceY = ConnectionObject1.y1 - ConnectionObject1.x2;
			}
			else
			{
				EndPreviousTraceX = ConnectionObject1.x1 + ConnectionObject1.x2;
				EndPreviousTraceY = ConnectionObject1.y1 - ConnectionObject1.x2;
				StartPreviousTraceX = ConnectionObject1.x1;
				StartPreviousTraceY = ConnectionObject1.y1;
			}

			PreviousDirection =
			    GetNewDirection(StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX, EndPreviousTraceY, 0);
			break;

		case TRACE_DIAG2:
			if ((InRange(CurrentDrawX1, ConnectionObject1.x1)) && (InRange(CurrentDrawY1, ConnectionObject1.y1)))
			{
				EndPreviousTraceX = ConnectionObject1.x1;
				EndPreviousTraceY = ConnectionObject1.y1;
				StartPreviousTraceX = ConnectionObject1.x1 + ConnectionObject1.x2;
				StartPreviousTraceY = ConnectionObject1.y1 + ConnectionObject1.x2;
			}
			else
			{
				EndPreviousTraceX = ConnectionObject1.x1 + ConnectionObject1.x2;
				EndPreviousTraceY = ConnectionObject1.y1 + ConnectionObject1.x2;
				StartPreviousTraceX = ConnectionObject1.x1;
				StartPreviousTraceY = ConnectionObject1.y1;
			}

			PreviousDirection =
			    GetNewDirection(StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX, EndPreviousTraceY, 0);
			break;

		case TRACE_ALL_ANGLE:
			if ((InRange(CurrentDrawX1, ConnectionObject1.x1)) && (InRange(CurrentDrawY1, ConnectionObject1.y1)))
			{
				EndPreviousTraceX = ConnectionObject1.x1;
				EndPreviousTraceY = ConnectionObject1.y1;
				StartPreviousTraceX = ConnectionObject1.x2;
				StartPreviousTraceY = ConnectionObject1.y2;
			}
			else
			{
				EndPreviousTraceX = ConnectionObject1.x2;
				EndPreviousTraceY = ConnectionObject1.y2;
				StartPreviousTraceX = ConnectionObject1.x1;
				StartPreviousTraceY = ConnectionObject1.y1;
			}

			PreviousDirection =
			    GetNewDirection(StartPreviousTraceX, StartPreviousTraceY, EndPreviousTraceX, EndPreviousTraceY, 0);
			break;

		case TRACE_ARC:
			GetArcEndPoints(&ConnectionObject1, &x1, &y1, &x2, &y2, 0);
			GetArcAngle(&ConnectionObject1, &Angle1, &Angle2);

			if ((InRange(CurrentDrawX1, x1)) && (InRange(CurrentDrawY1, y1)))
			{
				EndPreviousTraceX = x1;
				EndPreviousTraceY = y1;
				StartPreviousTraceX = x2;
				StartPreviousTraceY = y2;
				Angle1 -= ANGLE_90 * 180 / PI;
				PreviousDirection = (40 - (int32) (Angle1 / 11.25 + 0.1)) % 32;
			}
			else
			{
				EndPreviousTraceX = x2;
				EndPreviousTraceY = y2;
				StartPreviousTraceX = x1;
				StartPreviousTraceY = y1;
				Angle2 += ANGLE_90 * 180 / PI;
				PreviousDirection = (40 - (int32) (Angle2 / 11.25 + 0.1)) % 32;
			}

			break;
		}

		CurrentGuideNr = FoundObject.TraceNr;

		if (OldDrawingLayer != CurrentDrawingLayer)
		{
			OldDrawingLayer = CurrentDrawingLayer;
			RePaint();
			CheckInputMessages(0);
		}

		TraceDrawing(0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTraceFromTrace(double PointX, double PointY)
{
	int32 cnt, NetNr, TraceObjectNr1, TraceObjectNr2, TraceObjectNr;
	double TraceX1, TraceY1, TraceX2, TraceY2, hulp1, hulp2, Angle1, Angle2;
	TraceRecord *Trace = NULL;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;
	NetRecord *Net;
	ObjectRecord *Object, *Object1, *Object2;

	int32 OkToDrawExtraTrace = 0;

	PreviousDirection = -1;
	ObjectLine = NULL;
	ObjectArc = NULL;
	NetNr = -1;
	EndPreviousTraceX = 1e9;
	EndPreviousTraceY = 1e9;
	StartPreviousTraceX = 1e9;
	StartPreviousTraceY = 1e9;

	TraceX1 = 0.0;
	TraceX2 = 0.0;
	TraceY1 = 0.0;
	TraceY2 = 0.0;
	ConnectionObject1.Test = -1;
	ConnectionObject2.Test = -1;

	if ((FoundObject.Layer < 0) || (FoundObject.Layer >= Design.NrBoardLayers))
		return;

	DrawTraceUsingGuide = 0;

	switch (FoundObject.ObjectType)
	{
	case TRACE_HOR:
		Trace = &((*HorTraces[FoundObject.Layer])[FoundObject.TraceNr]);
		TraceX1 = Trace->X;
		TraceY1 = Trace->Y;
		TraceX2 = TraceX1 + Trace->Length;
		TraceY2 = TraceY1;
		NetNr = Trace->NetNr;
		break;

	case TRACE_VER:
		Trace = &((*VerTraces[FoundObject.Layer])[FoundObject.TraceNr]);
		TraceX1 = Trace->X;
		TraceY1 = Trace->Y;
		TraceX2 = TraceX1;
		TraceY2 = TraceY1 + Trace->Length;
		NetNr = Trace->NetNr;
		break;

	case TRACE_DIAG1:
		Trace = &((*Diag1Traces[FoundObject.Layer])[FoundObject.TraceNr]);
		TraceX1 = Trace->X;
		TraceY1 = Trace->Y;
		TraceX2 = TraceX1 + Trace->Length;
		TraceY2 = TraceY1 - Trace->Length;
		NetNr = Trace->NetNr;
		break;

	case TRACE_DIAG2:
		Trace = &((*Diag2Traces[FoundObject.Layer])[FoundObject.TraceNr]);
		TraceX1 = Trace->X;
		TraceY1 = Trace->Y;
		TraceX2 = TraceX1 + Trace->Length;
		TraceY2 = TraceY1 + Trace->Length;
		NetNr = Trace->NetNr;
		break;

	case TRACE_ALL_ANGLE:
		ObjectLine = &((*ObjectLines)[FoundObject.TraceNr]);
		TraceX1 = ObjectLine->X1;
		TraceY1 = ObjectLine->Y1;
		TraceX2 = ObjectLine->X2;
		TraceY2 = ObjectLine->Y2;
		NetNr = ObjectLine->NetNr;
		break;

	case TRACE_ARC:
		ObjectArc = &((*ObjectArcs)[FoundObject.TraceNr]);
		GetArcEndPoints2(ObjectArc, &TraceX1, &TraceY1, &TraceX2, &TraceY2, 0);
		NetNr = ObjectArc->NetNr;
		break;
	}

	if (NetNr == -1)
		return;

	GetObjectsNet(NetNr, MODE_OBJECTS3, 0);
	TraceObjectNr = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		if ((Object->ObjectType == FoundObject.ObjectType) && (Object->Layer == FoundObject.Layer)
		        && (Object->TraceNr == FoundObject.TraceNr))
			TraceObjectNr = cnt;
	}


	if (TraceObjectNr == -1)
		return;


	Net = &((*Nets)[NetNr]);
	Object = &((*Objects3)[TraceObjectNr]);
	Object->ObjectType = 0;
	CurrentDrawingNetNr = NetNr;
	strcpy(OldInfoStr, InfoStr);

	TraceObjectNr1 = GetObjectNrFromEndPoint(Object->Layer, TraceX1, TraceY1, 0);
	TraceObjectNr2 = GetObjectNrFromEndPoint(Object->Layer, TraceX2, TraceY2, 0);
	Object->ObjectType = FoundObject.ObjectType;

	hulp1 = sqrt((PointX - TraceX1) * (PointX - TraceX1) + (PointY - TraceY1) * (PointY - TraceY1));
	hulp2 = sqrt((PointX - TraceX2) * (PointX - TraceX2) + (PointY - TraceY2) * (PointY - TraceY2));

	NullObject.ObjectType = 0;
	NullObject.Layer = 0;
	Angle1 = 0.0;
	Angle2 = 0.0;

	if (TraceObjectNr1 == -1)
		Object1 = &NullObject;
	else
		Object1 = &((*Objects3)[TraceObjectNr1]);

	if (TraceObjectNr2 == -1)
		Object2 = &NullObject;
	else
		Object2 = &((*Objects3)[TraceObjectNr2]);

	switch (FoundObject.ObjectType)
	{
	case TRACE_HOR:
	case TRACE_VER:
	case TRACE_DIAG1:
	case TRACE_DIAG2:
		CurrentTraceWidth = Trace->ThickNess;
		CurrentClearance = Trace->Clearance;
		break;

	case TRACE_ALL_ANGLE:
		CurrentTraceWidth = ObjectLine->LineThickNess;
		CurrentClearance = ObjectLine->Clearance;
		break;

	case TRACE_ARC:
		CurrentTraceWidth = ObjectArc->LineThickNess;
		CurrentClearance = ObjectArc->Clearance;
		break;
	}

	if (CurrentTraceWidth == 0)
	{
		switch (Object1->ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			CurrentTraceWidth = Object1->y2;
			break;

		case TRACE_ALL_ANGLE:
		case TRACE_ARC:
			CurrentTraceWidth = Object1->Thickness;
			break;
		}
	}

	if (CurrentTraceWidth == 0)
	{
		switch (Object1->ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			CurrentTraceWidth = Object2->y2;
			break;

		case TRACE_ALL_ANGLE:
		case TRACE_ARC:
			CurrentTraceWidth = Object2->Thickness;
			break;
		}
	}

	if (CurrentClearance == 0)
	{
		switch (Object2->ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
		case TRACE_ALL_ANGLE:
		case TRACE_ARC:
			CurrentClearance = Object1->Clearance;
			break;
		}
	}

	if (CurrentClearance == 0)
	{
		switch (Object2->ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
		case TRACE_ALL_ANGLE:
		case TRACE_ARC:
			CurrentClearance = Object2->Clearance;
			break;
		}
	}

	if (CurrentTraceWidth == 0)
		CurrentTraceWidth = Net->TraceWidth;

	if (CurrentClearance == 0)
		CurrentClearance = Net->TraceClearance;

	//**************************** spodní pøeklad, trasovat ***********************************************************
	if (Units == 0)
	{
		sprintf(InfoStr, "net  %s  ( trace width %.1f , clearance %.1f thou )", Net->Name, CurrentTraceWidth / 2540.0,
		        CurrentClearance / 2540.0);
	}
	else
	{
		sprintf(InfoStr, SC(340, "net  %s  ( trace width %.4f , clearance %.4f mm )"), Net->Name, CurrentTraceWidth / 100000.0,
		        CurrentClearance / 100000.0);
	}

	RedrawInfoStr(1);

	CurrentDrawingLayer = FoundObject.Layer;

	if (!AddExtraTraceMode)
	{
		switch (FoundObject.ObjectType)
		{
		case TRACE_HOR:
		case TRACE_VER:
		case TRACE_DIAG1:
		case TRACE_DIAG2:
			CurrentWorkingTrace.x1 = Trace->X;
			CurrentWorkingTrace.y1 = Trace->Y;
			CurrentWorkingTrace.x2 = Trace->Length;
			CurrentWorkingTrace.y2 = Trace->ThickNess;
			CurrentWorkingTrace.ObjectType = FoundObject.ObjectType;
			CurrentWorkingTrace.Layer = CurrentDrawingLayer;
			CurrentWorkingTrace.Clearance = Trace->Clearance;
			CurrentWorkingTrace.Info = Trace->Info;
			CurrentWorkingTrace.Info2 = 3;
			CurrentWorkingTrace.NetNr = NetNr;
			CurrentWorkingTrace.TraceNr = TraceObjectNr;
//        TraceDrawingMode=0;
			break;

		case TRACE_ALL_ANGLE:
			if (TraceDrawingMode == 0)
			{
				CurrentWorkingTrace.x1 = ObjectLine->X1;
				CurrentWorkingTrace.y1 = ObjectLine->Y1;
				CurrentWorkingTrace.x2 = 0.0;
				CurrentWorkingTrace.y2 = ObjectLine->LineThickNess;
				CurrentWorkingTrace.Layer = CurrentDrawingLayer;
				CurrentWorkingTrace.Clearance = ObjectLine->Clearance;
				CurrentWorkingTrace.Info = ObjectLine->Info;
				CurrentWorkingTrace.Info2 = 3;
				CurrentWorkingTrace.NetNr = NetNr;
				CurrentWorkingTrace.TraceNr = TraceObjectNr;
			}
			else
			{
				ObjectLine = &((*ObjectLines)[FoundObject.TraceNr]);
				CurrentWorkingTrace.x1 = ObjectLine->X1;
				CurrentWorkingTrace.y1 = ObjectLine->Y1;
				CurrentWorkingTrace.x2 = ObjectLine->X2;
				CurrentWorkingTrace.y2 = ObjectLine->Y2;
				CurrentWorkingTrace.Thickness = ObjectLine->LineThickNess;
				CurrentWorkingTrace.ObjectType = TRACE_ALL_ANGLE;
				CurrentWorkingTrace.Layer = CurrentDrawingLayer;
				CurrentWorkingTrace.Clearance = ObjectLine->Clearance;
				CurrentWorkingTrace.Info = ObjectLine->Info;
				CurrentWorkingTrace.Info2 = 3;
				CurrentWorkingTrace.NetNr = NetNr;
				CurrentWorkingTrace.TraceNr = TraceObjectNr;
				TraceDrawingMode = 1;
			}

			break;

		case TRACE_ARC:
			ObjectArc = &((*ObjectArcs)[FoundObject.TraceNr]);

			/*
			        CurrentWorkingTrace.x1=TraceX1;
			        CurrentWorkingTrace.y1=TraceY1;
			        CurrentWorkingTrace.x2=TraceX2;
			        CurrentWorkingTrace.y2=TraceY2;
			*/
			CurrentWorkingTrace.x1 = ObjectArc->CentreX;
			CurrentWorkingTrace.y1 = ObjectArc->CentreY;
			CurrentWorkingTrace.x2 = ObjectArc->Width;
			CurrentWorkingTrace.y2 = ObjectArc->Height;
			CurrentWorkingTrace.x3 = ObjectArc->StartDiffX;
			CurrentWorkingTrace.y3 = ObjectArc->StartDiffY;
			CurrentWorkingTrace.x4 = ObjectArc->EndDiffX;
			CurrentWorkingTrace.y4 = ObjectArc->EndDiffY;
			CurrentWorkingTrace.Thickness = ObjectArc->LineThickNess;
			CurrentWorkingTrace.ObjectType = TRACE_ARC;
			CurrentWorkingTrace.Layer = CurrentDrawingLayer;
			CurrentWorkingTrace.Clearance = ObjectArc->Clearance;
			CurrentWorkingTrace.Info = ObjectArc->Info;
			CurrentWorkingTrace.Info2 = 3;
			CurrentWorkingTrace.NetNr = NetNr;
			CurrentWorkingTrace.TraceNr = TraceObjectNr;

			GetArcAngle(&CurrentWorkingTrace, &Angle1, &Angle2);

			TraceDrawingMode = 2;
			break;
		}

		if ((DrawLayerCode[CurrentDrawingLayer] >= 0)
		        && (DrawLayerCode[CurrentDrawingLayer] < MAX_ACTIVE_DRAWING_LAYERS))
		{
			if (hulp1 > hulp2)
			{
				CurrentDrawX1 = TraceX1;
				CurrentDrawY1 = TraceY1;
				CurrentDrawX2 = TraceX1;
				CurrentDrawY2 = TraceY1;
				EndPreviousTraceX = TraceX1;
				EndPreviousTraceY = TraceY1;
				memmove(&TraceObject1, Object1, sizeof(ObjectRecord));
				ObjectKeepOut2 = GetObjectNrFromEndPoint(CurrentDrawingLayer, TraceX2, TraceY2, 3);

				if (FoundObject.ObjectType == TRACE_ARC)
				{
//          Angle1-=ANGLE_180*180/PI;
//          PreviousDirection=(40-(int32)(Angle1/11.25+0.1)) % 32;
					PreviousDirection = ((8 - ((int32) (Angle1 / 45.0 + 0.1) % 8)) * 4) % 32;
				}
				else
					PreviousDirection = GetNewDirection(TraceX1, TraceY1, TraceX2, TraceY2, 0);
			}
			else
			{
				CurrentDrawX1 = TraceX2;
				CurrentDrawY1 = TraceY2;
				CurrentDrawX2 = TraceX2;
				CurrentDrawY2 = TraceY2;
				EndPreviousTraceX = TraceX2;
				EndPreviousTraceY = TraceY2;
				memmove(&TraceObject1, Object2, sizeof(ObjectRecord));
				ObjectKeepOut2 = GetObjectNrFromEndPoint(CurrentDrawingLayer, TraceX1, TraceY1, 3);

				if (FoundObject.ObjectType == TRACE_ARC)
				{
					Angle2 -= ANGLE_180 * 180 / PI;
//          PreviousDirection=(40-(int32)(Angle2/11.25+0.1)) % 32;
					PreviousDirection = ((8 - ((int32) (Angle2 / 45.0 + 0.1)) % 8) * 4) % 32;
				}
				else
					PreviousDirection = GetNewDirection(TraceX2, TraceY2, TraceX1, TraceY1, 0);
			}

			CurrentGuideNr = -1;
			ObjectKeepOut = CurrentWorkingTrace.TraceNr;

			if (OldDrawingLayer != CurrentDrawingLayer)
			{
				OldDrawingLayer = CurrentDrawingLayer;
				RePaint();
				CheckInputMessages(0);
			}
			else
				DrawCrossHair(1);

			TraceDrawing(1);
		}
	}
	else
	{
		AddExtraTraceMode = 0;
		CurrentWorkingTrace.ObjectType = 0;
		TraceObject1.ObjectType = 0;
// OkToDrawExtraTrace
		PointX = AdjustToDrawGrid(PointX);
		PointY = AdjustToDrawGrid(PointY);

		switch (FoundObject.ObjectType)
		{
		case TRACE_HOR:
			if ((PointX >= TraceX1) && (PointX <= TraceX2))
			{
				OkToDrawExtraTrace = 1;
				CurrentDrawX1 = PointX;
				CurrentDrawX2 = PointX;
				CurrentDrawY1 = TraceY1;
				CurrentDrawY2 = TraceY1;
			}

			break;

		case TRACE_VER:
			if ((PointY >= TraceY1) && (PointY <= TraceY2))
			{
				OkToDrawExtraTrace = 1;
				CurrentDrawX1 = TraceX1;
				CurrentDrawX2 = TraceX1;
				CurrentDrawY1 = PointY;
				CurrentDrawY2 = PointY;
			}

			break;

		case TRACE_DIAG1:
			if ((PointX >= TraceX1) && (PointX <= TraceX2))
			{
				OkToDrawExtraTrace = 1;
				CurrentDrawX1 = PointX;
				CurrentDrawX2 = PointX;
				CurrentDrawY1 = -PointX + TraceX1 + TraceY1;
				CurrentDrawY2 = CurrentDrawY1;
			}

			break;

		case TRACE_DIAG2:
			if ((PointX >= TraceX1) && (PointX <= TraceX2))
			{
				OkToDrawExtraTrace = 1;
				CurrentDrawX1 = PointX;
				CurrentDrawX2 = PointX;
				CurrentDrawY1 = PointX - TraceX1 + TraceY1;
				CurrentDrawY2 = CurrentDrawY1;
			}

			break;
		}

		if (OkToDrawExtraTrace)
		{
			CurrentGuideNr = -1;
			ObjectKeepOut = CurrentWorkingTrace.TraceNr;
			ObjectKeepOut2 = -1;

			if (OldDrawingLayer != CurrentDrawingLayer)
			{
				OldDrawingLayer = CurrentDrawingLayer;
				RePaint();
				CheckInputMessages(0);
			}

			TraceDrawing(1);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTraceFromPinOrVia(double PointX, double PointY)
{
	int32 NetNr, NewLayer;
	double x1, y1, x2, y2, Angle1, Angle2, x2a;
	ViaRecord *Via;
	NetRecord *Net;
	ObjectRecord *Object;

	EndPreviousTraceX = 1e9;
	EndPreviousTraceY = 1e9;
	StartPreviousTraceX = 1e9;
	StartPreviousTraceY = 1e9;

	PreviousDirection = -1;
	DrawTraceUsingGuide = 0;

	if (FoundObject.ObjectType == VIA_PUT_THROUGH_ROUND)
	{
		Via = &((*Vias)[FoundObject.TraceNr]);
		NetNr = Via->NetNr;
		CurrentDrawX1 = Via->X;
		CurrentDrawY1 = Via->Y;
		CurrentDrawX2 = Via->X;
		CurrentDrawY2 = Via->Y;
	}
	else
	{
		NetNr = FoundObject.NetNr;
		CurrentDrawX1 = FoundObject.x1;
		CurrentDrawY1 = FoundObject.y1;
		CurrentDrawX2 = FoundObject.x1;
		CurrentDrawY2 = FoundObject.y1;
		x2a = FoundObject.x2 * 0.5;

		switch (FoundObject.ObjectType)
		{
		case PIN_LINE_HOR:
			CurrentDrawX1 += x2a;
			CurrentDrawX2 += x2a;
			break;

		case PIN_LINE_VER:
			CurrentDrawY1 += x2a;
			CurrentDrawY2 += x2a;
			break;

		case PIN_LINE_DIAG1:
			CurrentDrawX1 += x2a;
			CurrentDrawY1 -= x2a;
			CurrentDrawX2 += x2a;
			CurrentDrawY2 -= x2a;
			break;

		case PIN_LINE_DIAG2:
			CurrentDrawX1 += x2a;
			CurrentDrawY1 += x2a;
			CurrentDrawX2 += x2a;
			CurrentDrawY2 += x2a;
			break;

		case PIN_ARC:
			GetArcEndPoints(&FoundObject, &x1, &y1, &x2, &y2, 0);
			GetArcAngle(&FoundObject, &Angle1, &Angle2);

			if (CalcLengthLine(PointX, PointY, x1, y1) < CalcLengthLine(PointX, PointY, x2, y2))
			{
				CurrentDrawX1 = x1;
				CurrentDrawY1 = y1;
				CurrentDrawX2 = x1;
				CurrentDrawY2 = y1;
				Angle1 -= ANGLE_90;
				PreviousDirection = (40 - (int32) (Angle1 / 11.25 + 0.1)) % 32;
			}
			else
			{
				CurrentDrawX1 = x2;
				CurrentDrawY1 = y2;
				CurrentDrawX2 = x2;
				CurrentDrawY2 = y2;
				Angle2 += ANGLE_90;
				PreviousDirection = (40 - (int32) (Angle2 / 11.25 + 0.1)) % 32;
			}

			break;

		case PIN_LINE_ALL_ANGLE:
			if (CalcLengthLine(PointX, PointY, FoundObject.x1, FoundObject.y1) <
			        CalcLengthLine(PointX, PointY, FoundObject.x2, FoundObject.y2))
			{
				CurrentDrawX1 = FoundObject.x1;
				CurrentDrawY1 = FoundObject.y1;
				CurrentDrawX2 = FoundObject.x1;
				CurrentDrawY2 = FoundObject.y1;
				PreviousDirection = GetNewDirection(FoundObject.x2, FoundObject.y2, FoundObject.x1, FoundObject.y1, 0);
			}
			else
			{
				CurrentDrawX1 = FoundObject.x2;
				CurrentDrawY1 = FoundObject.y2;
				CurrentDrawX2 = FoundObject.x2;
				CurrentDrawY2 = FoundObject.y2;
				PreviousDirection = GetNewDirection(FoundObject.x1, FoundObject.y1, FoundObject.x2, FoundObject.y2, 0);
			}

			break;
		}
	}

//  CurrentDrawingLayer=0;
	if (CurrentDrawingLayer == -1)
	{
		CurrentDrawingLayer = FoundObject.Layer;

		if (CurrentDrawingLayer == -1)
		{
			CurrentDrawingLayer = 0;

			if ((NewLayer = GetDrawingLayer(1)) == -1)
				return;

			CurrentDrawingLayer = NewLayer;
		}
	}
	else
	{
		if (FoundObject.Layer != -1)
			CurrentDrawingLayer = FoundObject.Layer;

		if ((NewLayer = GetDrawingLayer(1)) == -1)
			return;

		CurrentDrawingLayer = NewLayer;
	}


	strcpy(OldInfoStr, InfoStr);

	if ((NetNr < 0) || (NetNr >= Design.NrNets))
	{
		Beep(1000, 200);
		MessageBoxOwn(PCBWindow, SC(1061, "Pin is not connected"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
		return;
	}

	GetObjectsNet(NetNr, MODE_OBJECTS3, 0);
	
	if (NrObjects3 == 1)
	{
		Object = &((*Objects3)[0]);

		switch (Object->ObjectType)
		{
		case PIN_PUT_THROUGH_ROUND:
		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
		case PIN_PUT_THROUGH_SQUARE:
		case PIN_SMD_POLYGON:
		case PIN_PUT_THROUGH_POLYGON:
			
			break;
		}
	}

	Net = &((*Nets)[NetNr]);
	CurrentDrawingNetNr = NetNr;

	if (NotInRange(Net->TraceWidth, 0.0))
		CurrentTraceWidth = Net->TraceWidth;

	if (NotInRange(Net->TraceClearance, 0.0))
		CurrentClearance = Net->TraceClearance;

	if ((CurrentTraceWidth == 0) || (CurrentClearance == 0))
	{
		CurrentTraceWidth = Design.StandardTraceWidth;
		CurrentClearance = Design.StandardClearance;
	}
	//****************************** spodní pøeklad, trasovat ***************************************************************
	if (Units == 0)
	{
		sprintf(InfoStr, "net  %s  ( trace width %.1f , clearance %.1f mm )", Net->Name, CurrentTraceWidth / 2540.0,
		        CurrentClearance / 2540.0);
	}
	else
	{
		sprintf(InfoStr, SC(340, "net  %s  ( trace width %.4f , clearance %.4f mm )"), Net->Name, CurrentTraceWidth / 100000.0,
		        CurrentClearance / 100000.0);
	}

	RedrawInfoStr(1);

	memset(&CurrentWorkingTrace, 0, sizeof(ObjectRecord));
	CurrentWorkingTrace.TraceNr = -1;
	memset(&TraceObject1, 0, sizeof(ObjectRecord));

	if ((DrawLayerCode[CurrentDrawingLayer] >= 0) && (DrawLayerCode[CurrentDrawingLayer] < MAX_ACTIVE_DRAWING_LAYERS))
	{
		CurrentGuideNr = -1;
		ObjectKeepOut = GetObjectNrFromEndPoint(CurrentDrawingLayer, CurrentDrawX1, CurrentDrawY1, 1);
		ObjectKeepOut2 = GetObjectNrFromEndPoint(CurrentDrawingLayer, CurrentDrawX1, CurrentDrawY1, 3);

		if (OldDrawingLayer != CurrentDrawingLayer)
		{
			OldDrawingLayer = CurrentDrawingLayer;
			RePaint();
			CheckInputMessages(0);
		}

		TraceDrawing(2);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindEndPointFromNetObjects(double StartX, double StartY, double EndX, double EndY, double ThickNess,
                                 int32 Direction, int32 Layer, double *NewX, double *NewY)
{
	int32 cnt, ok, FoundNr;
	double x1, y1, x2, y2, r, CenterX, CenterY;
	ObjectRecord *Object, CheckObject;


	ThickNess = min(ThickNess, (12 * 2540.0));
	memset(&CheckObject, 0, sizeof(ObjectRecord));
	CheckObject.ObjectType = PIN_SMD_ROUND;
	CheckObject.x1 = EndX;
	CheckObject.y1 = EndY;
	CheckObject.x2 = ThickNess;
	CheckObject.Layer = Layer;

	FoundNr = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		if ((cnt != ObjectKeepOut) && (cnt != ObjectKeepOut2))
		{
			Object = &((*Objects3)[cnt]);
#ifdef _DEBUG

			if ((InRange9(Object->x1, 49.9e5)) && (InRange9(Object->y1, 29.6e5)))
				ok = 1;

#endif

			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case VIA_PUT_THROUGH_ROUND:
			case DRILL:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				if (ObjectsConnected(&CheckObject, Object))
				{
					FoundNr = cnt;
					*NewX = Object->x1;
					*NewY = Object->y1;
					return FoundNr;
				}
			}
		}
	}

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		if ((cnt != ObjectKeepOut) && (cnt != ObjectKeepOut2))
		{
			Object = &((*Objects3)[cnt]);
#ifdef _DEBUG

			if ((InRange9(Object->x1, 49.9e5)) && (InRange9(Object->y1, 29.6e5)))
				ok = 1;

#endif

			switch (Object->ObjectType)
			{
			case PIN_SMD_RECT:
			case PIN_SMD_ROUND:
			case PIN_SMD_POLYGON:
				if (ObjectsConnected(&CheckObject, Object))
				{
					FoundNr = cnt;
					*NewX = Object->x1;
					*NewY = Object->y1;
					return FoundNr;
				}

				break;

			case PIN_ARC:
				if (ObjectsConnected(&CheckObject, Object))
				{
					GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);
					FoundNr = cnt;

					if (CalcLengthLine(EndX, EndY, x1, y1) < CalcLengthLine(EndX, EndY, x2, y2))
					{
						*NewX = x1;
						*NewY = y1;
					}
					else
					{
						*NewX = x2;
						*NewY = y2;
					}

					return FoundNr;
				}

				break;

			case PIN_LINE_ALL_ANGLE:
				if (ObjectsConnected(&CheckObject, Object))
				{
					FoundNr = cnt;

					if (CalcLengthLine(EndX, EndY, Object->x1, Object->y1) <
					        CalcLengthLine(EndX, EndY, Object->x2, Object->y2))
					{
						*NewX = Object->x1;
						*NewY = Object->y1;
					}
					else
					{
						*NewX = Object->x2;
						*NewY = Object->y2;
					}

					return FoundNr;
				}

				break;
			}
		}
	}

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		if ((cnt != ObjectKeepOut) && (cnt != ObjectKeepOut2))
		{
			Object = &((*Objects3)[cnt]);

			switch (Object->ObjectType)
			{
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
				if (ObjectsConnected(&CheckObject, Object))
				{
					FoundNr = cnt;

					switch (Object->ObjectType)
					{
					case PIN_LINE_HOR:
						*NewX = Object->x1 + Object->x2 * 0.5;
						*NewY = Object->y1;
						break;

					case PIN_LINE_VER:
						*NewX = Object->x1;
						*NewY = Object->y1 + Object->x2 * 0.5;
						break;

					case PIN_LINE_DIAG1:
						*NewX = Object->x1 + Object->x2 * 0.5;
						*NewY = Object->y1 - Object->x2 * 0.5;
						break;

					case PIN_LINE_DIAG2:
						*NewX = Object->x1 + Object->x2 * 0.5;
						*NewY = Object->y1 + Object->x2 * 0.5;
						break;
					}

					return FoundNr;
				}
			}
		}
	}

	ok = 1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		if ((cnt != ObjectKeepOut) && (cnt != ObjectKeepOut2))
		{
			Object = &((*Objects3)[cnt]);

			switch (Object->ObjectType)
			{
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
				Object->y2 = min(Object->y2, (12 * 2540.0));

				if (ObjectsConnected(&CheckObject, Object))
				{
//            FoundNr=cnt;
					switch (Object->ObjectType)
					{
					case TRACE_HOR:
						r = Object->y2 * 0.5;

						if ((EndX > Object->x1 - r) && (EndX < Object->x1 + r) && (EndY > Object->y1 - r)
						        && (EndY < Object->y1 + r))
						{
							*NewX = Object->x1;
							*NewY = Object->y1;
							return cnt;
						}

						if ((EndX > Object->x1 + Object->x2 - r) && (EndX < Object->x1 + Object->x2 + r)
						        && (EndY > Object->y1 - r) && (EndY < Object->y1 + r))
						{
							*NewX = Object->x1 + Object->x2;
							*NewY = Object->y1;
							return cnt;
						}

						*NewY = Object->y1;

						switch (Direction)
						{
						case 0:
						case 4:
							*NewX = EndX;

							if (*NewX < Object->x1)
								*NewX = Object->x1;

							if (*NewX > Object->x1 + Object->x2)
								*NewX = Object->x1 + Object->x2;

							return cnt;
							break;

						case 1:
						case 5:
							*NewX = *NewY + EndX - EndY;

							if (*NewX < Object->x1)
								*NewX = Object->x1;

							if (*NewX > Object->x1 + Object->x2)
								*NewX = Object->x1 + Object->x2;

							return cnt;
							break;

						case 3:
						case 7:
							*NewX = -(*NewY) + EndX + EndY;

							if (*NewX < Object->x1)
								*NewX = Object->x1;

							if (*NewX > Object->x1 + Object->x2)
								*NewX = Object->x1 + Object->x2;

							return cnt;
							break;

						case 2:
							*NewX = Object->x1;
							return cnt;
							break;

						case 6:
							*NewX = Object->x1 + Object->x2;
							return cnt;
							break;

						default:
							if (LineCrosses2
							        (StartX, StartY, EndX, EndY, Object->x1, Object->y1, Object->x1 + Object->x2,
							         Object->y1, &CenterX, &CenterY, 0) == 1)
							{
								*NewX = CenterX;
								*NewY = CenterY;
								return cnt;
							}

							ok = 1;
							break;
						}

						break;

					case TRACE_DIAG1:
						r = Object->y2 * 0.5;

						if ((EndX > Object->x1 - r) && (EndX < Object->x1 + r) && (EndY > Object->y1 - r)
						        && (EndY < Object->y1 + r))
						{
							*NewX = Object->x1;
							*NewY = Object->y1;
							return cnt;
						}

						if ((EndX > Object->x1 + Object->x2 - r) && (EndX < Object->x1 + Object->x2 + r)
						        && (EndY > Object->y1 - Object->x2 - r) && (EndY < Object->y1 - Object->x2 + r))
						{
							*NewX = Object->x1 + Object->x2;
							*NewY = Object->y1 - Object->x2;
							return cnt;
						}

						switch (Direction)
						{
						case 1:
						case 5:
							*NewY = (Object->x1 + Object->y1 - EndX + EndY) * 0.5;

							if (*NewY < Object->y1 - Object->x2)
								*NewY = Object->y1 - Object->x2;

							if (*NewY > Object->y1)
								*NewY = Object->y1;

							*NewX = -(*NewY) + Object->x1 + Object->y1;
							return cnt;
							break;

						case 2:
						case 6:
							*NewY = EndY;

							if (*NewY < Object->y1 - Object->x2)
								*NewY = Object->y1 - Object->x2;

							if (*NewY > Object->y1)
								*NewY = Object->y1;

							*NewX = -(*NewY) + Object->x1 + Object->y1;
							return cnt;
							break;

						case 0:
						case 4:
							*NewX = EndX;

							if (*NewX < Object->x1)
								*NewX = Object->x1;

							if (*NewX > Object->x1 + Object->x2)
								*NewX = Object->x1 + Object->x2;

							*NewY = -(*NewX) + Object->x1 + Object->y1;
							return cnt;
							break;

						case 3:
							*NewX = Object->x1;
							*NewY = Object->y1;
							return cnt;
							break;

						case 7:
							*NewX = Object->x1 + Object->x2;
							*NewY = Object->y1 - Object->x2;
							return cnt;
							break;

						default:
							if (LineCrosses2
							        (StartX, StartY, EndX, EndY, Object->x1, Object->y1, Object->x1 + Object->x2,
							         Object->y1 - Object->x2, &CenterX, &CenterY, 0) == 1)
							{
								*NewX = CenterX;
								*NewY = CenterY;
								return cnt;
							}

							ok = 1;
							break;
						}

						break;

					case TRACE_VER:
						r = Object->y2 * 0.5;

						if ((EndX > Object->x1 - r) && (EndX < Object->x1 + r) && (EndY > Object->y1 - r)
						        && (EndY < Object->y1 + r))
						{
							*NewX = Object->x1;
							*NewY = Object->y1;
							return cnt;
						}

						if ((EndX > Object->x1 - r) && (EndX < Object->x1 + r) && (EndY > Object->y1 + Object->x2 - r)
						        && (EndY < Object->y1 + Object->x2 + r))
						{
							*NewX = Object->x1;
							*NewY = Object->y1 + Object->x2;
							return cnt;
						}

						*NewX = Object->x1;

						switch (Direction)
						{
						case 2:
						case 6:
							*NewY = EndY;

							if (*NewY < Object->y1)
								*NewY = Object->y1;

							if (*NewY > Object->y1 + Object->x2)
								*NewY = Object->y1 + Object->x2;

							return cnt;
							break;

						case 3:
						case 7:
							*NewY = -(*NewX) + EndX + EndY;

							if (*NewY < Object->y1)
								*NewY = Object->y1;

							if (*NewY > Object->y1 + Object->x2)
								*NewY = Object->y1 + Object->x2;

							return cnt;
							break;

						case 1:
						case 5:
							*NewY = *NewX - EndX + EndY;

							if (*NewY < Object->y1)
								*NewY = Object->y1;

							if (*NewY > Object->y1 + Object->x2)
								*NewY = Object->y1 + Object->x2;

							return cnt;
							break;

						case 0:
							*NewY = Object->y1;
							return cnt;
							break;

						case 4:
							*NewY = Object->y1 + Object->x2;
							return cnt;
							break;

						default:
							if (LineCrosses2
							        (StartX, StartY, EndX, EndY, Object->x1, Object->y1, Object->x1,
							         Object->y1 + Object->x2, &CenterX, &CenterY, 0) == 1)
							{
								*NewX = CenterX;
								*NewY = CenterY;
								return cnt;
							}

							ok = 1;
							break;
						}

						break;

					case TRACE_DIAG2:
						r = Object->y2 * 0.5;

						if ((EndX > Object->x1 - r) && (EndX < Object->x1 + r) && (EndY > Object->y1 - r)
						        && (EndY < Object->y1 + r))
						{
							*NewX = Object->x1;
							*NewY = Object->y1;
							return cnt;
						}

						if ((EndX > Object->x1 + Object->x2 - r) && (EndX < Object->x1 + Object->x2 + r)
						        && (EndY > Object->y1 + Object->x2 - r) && (EndY < Object->y1 + Object->x2 + r))
						{
							*NewX = Object->x1 + Object->x2;
							*NewY = Object->y1 + Object->x2;
							return cnt;
						}

						switch (Direction)
						{
						case 3:
						case 7:
							*NewY = (-Object->x1 + Object->y1 + EndX + EndY) * 0.5;

							if (*NewY < Object->y1)
								*NewY = Object->y1;

							if (*NewY > Object->y1 + Object->x2)
								*NewY = Object->y1 + Object->x2;

							*NewX = *NewY + Object->x1 - Object->y1;
							return cnt;
							break;

						case 2:
						case 6:
							*NewY = EndY;

							if (*NewY < Object->y1)
								*NewY = Object->y1;

							if (*NewY > Object->y1 + Object->x2)
								*NewY = Object->y1 + Object->x2;

							*NewX = *NewY + Object->x1 - Object->y1;
							return cnt;
							break;

						case 0:
						case 4:
							*NewX = EndX;

							if (*NewX < Object->x1)
								*NewX = Object->x1;

							if (*NewX > Object->x1 + Object->x2)
								*NewX = Object->x1 + Object->x2;

							*NewY = *NewX - Object->x1 + Object->y1;
							return cnt;
							break;

						case 1:
							*NewX = Object->x1;
							*NewY = Object->y1;
							return cnt;
							break;

						case 5:
							*NewX = Object->x1 + Object->x2;
							*NewY = Object->y1 + Object->x2;
							return cnt;
							break;

						default:
							if (LineCrosses2
							        (StartX, StartY, EndX, EndY, Object->x1, Object->y1, Object->x1 + Object->x2,
							         Object->y1 + Object->x2, &CenterX, &CenterY, 0) == 1)
							{
								*NewX = CenterX;
								*NewY = CenterY;
								return cnt;
							}

							ok = 1;
							break;
						}

						break;
					}
				}

			case TRACE_ARC:
				if (ObjectsConnected(&CheckObject, Object))
					return cnt;

				break;

			case TRACE_ALL_ANGLE:
				if (ObjectsConnected(&CheckObject, Object))
				{
					if (LineCrosses2
					        (StartX, StartY, EndX, EndY, Object->x1, Object->y1, Object->x1 + Object->x2,
					         Object->y1 + Object->x2, &CenterX, &CenterY, 0) == 1)
					{
						*NewX = CenterX;
						*NewY = CenterY;
						return cnt;
					}
				}

				break;
			}
		}
	}

	return -1;
}


/*
                   \
y = -x + x1 + y1     \
x = -y + x1 + y1       \
y = -x + b               \
b = x1 + y1                \



                           /
y =  x  - x1 + y1        /
x =  y  + x1 - y1      /
y =  x  + b          /
b = -x1 + y1       /

*/

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindObjectNrFromNetObjects(ObjectRecord * SearchObject)
{

	int32 cnt;
	ObjectRecord *Object;


	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);

		if ((SearchObject->ObjectType == Object->ObjectType) && (SearchObject->Layer == Object->Layer)
		        && (InRange(SearchObject->x1, Object->x1)) && (InRange(SearchObject->y1, Object->y1))
		        && (InRange(SearchObject->x2, Object->x2)))
			return cnt;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SelectTraceUnderCursor(int32 mode)
{
	int32 cnt, Layer, ObjectChanged;
	double x1, y1, x2, y2, dikte, lengte;

	TraceRecord *Trace;

	ObjectChanged = 0;

	SearchMinX = PixelToRealOffX(MousePosX - 2);
	SearchMinY = PixelToRealOffY(DrawWindowMaxY - (MousePosY + 2) - 1);
	SearchMaxX = PixelToRealOffX(MousePosX + 2);
	SearchMaxY = PixelToRealOffY(DrawWindowMaxY - (MousePosY - 2) - 1);

	for (Layer = 0; Layer < 32; Layer++)
	{
		if (Layer == CurrentDrawingLayer)
		{
			DrawCode = DrawLayerCode[Layer];

			if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
			{
				for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
				{
					Trace = &((*VerTraces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							x2 = x1;
							y1 = Trace->Y;
							lengte = Trace->Length;
							y2 = y1 + lengte;
							dikte = Trace->ThickNess;

							if ((RectTestRect2(x1, y1 + (lengte / 2), dikte, lengte))
							        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
								}
							}
						}
					}
				}

				for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
				{
					Trace = &((*HorTraces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							y1 = Trace->Y;
							lengte = Trace->Length;
							x2 = x1 + lengte;
							y2 = y1;
							dikte = Trace->ThickNess;

							if ((RectTestRect2(x1 + (lengte / 2), y1, lengte, dikte))
							        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
								}
							}
						}
					}
				}

				for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
				{
					Trace = &((*Diag1Traces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							y1 = Trace->Y;
							lengte = Trace->Length;
							x2 = x1 + lengte;
							y2 = y1 - lengte;
							dikte = Trace->ThickNess;

							if ((RectTestDiag1(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
							        || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
								}
							}
						}
					}
				}

				for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
				{
					Trace = &((*Diag2Traces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							y1 = Trace->Y;
							lengte = Trace->Length;
							x2 = x1 + lengte;
							y2 = y1 + lengte;
							dikte = Trace->ThickNess;

							if ((RectTestDiag2(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
							        || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
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
	if (ObjectChanged)
		return 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		if (Layer != CurrentDrawingLayer)
		{
			DrawCode = DrawLayerCode[Layer];

			if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
			{
				for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
				{
					Trace = &((*VerTraces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							x2 = x1;
							y1 = Trace->Y;
							lengte = Trace->Length;
							y2 = y1 + lengte;
							dikte = Trace->ThickNess;

							if ((RectTestRect2(x1, y1 + (lengte / 2), dikte, lengte))
							        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
								}
							}
						}
					}
				}

				for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
				{
					Trace = &((*HorTraces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							y1 = Trace->Y;
							lengte = Trace->Length;
							x2 = x1 + lengte;
							y2 = y1;
							dikte = Trace->ThickNess;

							if ((RectTestRect2(x1 + (lengte / 2), y1, lengte, dikte))
							        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
								}
							}
						}
					}
				}

				for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
				{
					Trace = &((*Diag1Traces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							y1 = Trace->Y;
							lengte = Trace->Length;
							x2 = x1 + lengte;
							y2 = y1 - lengte;
							dikte = Trace->ThickNess;

							if ((RectTestDiag1(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
							        || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
								}
							}
						}
					}
				}

				for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
				{
					Trace = &((*Diag2Traces[Layer])[cnt]);

					if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if (Trace->NetNr != CurrentDrawingNetNr)
						{
							x1 = Trace->X;
							y1 = Trace->Y;
							lengte = Trace->Length;
							x2 = x1 + lengte;
							y2 = y1 + lengte;
							dikte = Trace->ThickNess;

							if ((RectTestDiag2(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
							        || (RectTestCircle(x2, y2, dikte, 255)))
							{
								if (!ObjectChanged)
								{
									Trace->Info |= OBJECT_SELECTED;
									ObjectChanged = 1;
								}
							}
						}
					}
				}
			}
		}
	}

	if (ObjectChanged)
		return 0;

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
