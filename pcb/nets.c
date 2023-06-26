/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: nets.c
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
#include "calcdef.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "line2.h"
#include "math.h"
#include "pcb.h"
#include "files.h"
#include "calc.h"
#include "calcdef.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "graphics.h"
#include "string.h"
#include "mainloop.h"
#include "insdel.h"
#include "dialogs.h"
#include "stdio.h"
#include "toets.h"
#include "select2.h"
#include "files2.h"


typedef struct
{
	double x1, y1, x2, y2, MinLength;
	int32 ObjectNr1, ObjectNr2;
} MinConnectionsRecord;

#define   MaxNrNetSegments   6000

MinConnectionsRecord MinConnections[10];
MinConnectionsRecord MinConnectionsLayer[10];

int32 NetSegmentIndex[MaxNrNetSegments], SegmentObjects[MaxNrNetSegments], NrNetConnections, ConnectionIndex,
      ObjectCount, CheckCount, BigNetInfoCheck, DrawOn, ok;

double ShortestConnectionLength = 0.0;
int32 ShortestConnectionIndex = -1;

extern int32 DebugMode;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetObjectsNet2(int32 NetNr, int32 ObjectArrayNr, int32 Mode)
{
	int32 pos, cnt;
	ObjectRecord *Object4, *ObjectNet;
	NetRecord *Net;
#ifdef _DEBUG
	int32 ok;
#endif

	Net = &((*Nets)[NetNr]);
#ifdef _DEBUG

	if (stricmpOwn(Net->Name, "N$1008") == 0)
		ok = 1;

#endif
	pos = Net->Pos;

	if (ObjectArrayNr == 2)
		NrObjects2 = 0;
	else
		NrObjects3 = 0;

	for (cnt = 0; cnt < Net->Count; cnt++)
	{
		Object4 = &((*Objects4)[pos + cnt]);

		if (ObjectArrayNr == 2)
		{
			ObjectNet = &((*Objects2)[NrObjects2]);
			memcpy(ObjectNet, Object4, sizeof(ObjectRecord));

			if (NrObjects2 >= MaxNrObjects2 - 1)
			{
				if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
					NrObjects2++;
			}
			else
				NrObjects2++;
		}
		else
		{
			ObjectNet = &((*Objects3)[NrObjects3]);
			memcpy(ObjectNet, Object4, sizeof(ObjectRecord));

			if (NrObjects3 >= MaxNrObjects3 - 1)
			{
				if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
					NrObjects3++;
			}
			else
				NrObjects3++;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetObjectsNet(int32 NetNr, int32 ObjectArrayNr, int32 Mode)
{
	int32 Layer, ok, cnt, cnt2;
	double CurrentX, CurrentY;

	TraceRecord *Trace;
	ViaRecord *Via;
	NetRecord *Net;
	CompRecord *Comp;
	ConnectionsRecord *Connection;
	ObjectRecord *Object, *ObjectNet;
	AreaFillRecord *AreaFill;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;
	uint8 *AreaPos;
	PolygonRecord *DrawPolygon;

	Net = &((*Nets)[NetNr]);
#ifdef _DEBUG

	if (stricmpOwn(Net->Name, "N$1008") == 0)
		ok = 1;

#endif

	switch (ObjectArrayNr)
	{
	case 2:
		NrObjects2 = 0;

		for (Layer = 0; Layer < 32; Layer++)
		{
			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects2)[NrObjects2]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_VER;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects2 >= MaxNrObjects2 - 1)
						{
							if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
								NrObjects2++;
						}
						else
							NrObjects2++;
					}
				}
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects2)[NrObjects2]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_HOR;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects2 >= MaxNrObjects2 - 1)
						{
							if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
								NrObjects2++;
						}
						else
							NrObjects2++;
					}
				}
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects2)[NrObjects2]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_DIAG1;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects2 >= MaxNrObjects2 - 1)
						{
							if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
								NrObjects2++;
						}
						else
							NrObjects2++;
					}
				}
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects2)[NrObjects2]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_DIAG2;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects2 >= MaxNrObjects2 - 1)
						{
							if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
								NrObjects2++;
						}
						else
							NrObjects2++;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if (Via->NetNr == NetNr)
				{
					ObjectNet = &((*Objects2)[NrObjects2]);
					ObjectNet->x1 = Via->X;
					ObjectNet->y1 = Via->Y;
					ObjectNet->x2 = Via->ThickNess;
					ObjectNet->y2 = Via->DrillThickNess;
					ObjectNet->Clearance = Via->Clearance;
					ObjectNet->ObjectType = VIA_PUT_THROUGH_ROUND;
					ObjectNet->Info = (Via->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
					ObjectNet->CompNr = -1;
					ObjectNet->TraceNr = cnt;
					ObjectNet->Layer = -1;
					ObjectNet->NetNr = NetNr;

					if (NrObjects2 >= MaxNrObjects2 - 1)
					{
						if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
							NrObjects2++;
					}
					else
						NrObjects2++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);

			if ((Connection->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if (Connection->NetNr == NetNr)
				{
					if (Mode == 0)
					{
						ObjectNet = &((*Objects2)[NrObjects2]);
						ObjectNet->x1 = Connection->x1;
						ObjectNet->y1 = Connection->y1;
						ObjectNet->x2 = Connection->x2;
						ObjectNet->y2 = Connection->y2;
						ObjectNet->ObjectType = CONNECTION;
						ObjectNet->Info = (Connection->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->TraceNr = cnt;
						ObjectNet->Layer = Connection->Layer;
						ObjectNet->NetNr = NetNr;

						if (NrObjects2 >= MaxNrObjects2 - 1)
						{
							if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
								NrObjects2++;
						}
						else
							NrObjects2++;
					}
					else
						Connection->Extra1 = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				NrObjects = 0;
//          if (cnt==530) {
//            ok=1;
//          }
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "U100") == 0)
					ok = 1;

#endif

				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (Object->NetNr == NetNr)
					{
#ifdef _DEBUG

						if (Object->ObjectType == 2480)
							ok = 1;

#endif
						ObjectNet = &((*Objects2)[NrObjects2]);
#ifdef _DEBUG

						if (Object->ObjectType == PIN_LINE_ALL_ANGLE)
							ok = 1;

#endif
						memcpy(ObjectNet, Object, sizeof(ObjectRecord));

						if (NrObjects2 >= MaxNrObjects2 - 1)
						{
							if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
								NrObjects2++;
						}
						else
							NrObjects2++;
					}
				}

				ok = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr == NetNr))
			{
				ObjectNet = &((*Objects2)[NrObjects2]);
				ObjectNet->ObjectType = AREAFILL;
				ObjectNet->ObjectType2 = 0;
				AreaPos = (uint8 *) AreaFill;
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
				ObjectNet->x1 = (*DrawPolygon).Points[0].x;
				ObjectNet->y1 = (*DrawPolygon).Points[0].y;
				ObjectNet->Info = AreaFill->Info;
				ObjectNet->TraceNr = cnt;
				ObjectNet->Layer = AreaFill->Layer;
				ObjectNet->NetNr = NetNr;

				if (NrObjects2 >= MaxNrObjects2 - 1)
				{
					if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
						NrObjects2++;
				}
				else
					NrObjects2++;
			}
		}


		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->Layer < 32)
			        && (ObjectLine->NetNr == NetNr))
			{
				ObjectNet = &((*Objects2)[NrObjects2]);
				ObjectNet->x1 = ObjectLine->X1;
				ObjectNet->y1 = ObjectLine->Y1;
				ObjectNet->x2 = ObjectLine->X2;
				ObjectNet->y2 = ObjectLine->Y2;
				ObjectNet->Info = 0;
				ObjectNet->Test = 0;
				ObjectNet->Clearance = ObjectLine->Clearance;
				ObjectNet->TraceNr = cnt;
				ObjectNet->NetNr = NetNr;
				ObjectNet->Layer = ObjectLine->Layer;
				ObjectNet->ObjectType = TRACE_ALL_ANGLE;
				ObjectNet->Thickness = ObjectLine->LineThickNess;

				if (NrObjects2 >= MaxNrObjects2 - 1)
				{
					if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
						NrObjects2++;
				}
				else
					NrObjects2++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0) && (ObjectArc->Layer < 32)
			        && (ObjectArc->NetNr == NetNr))
			{
				ObjectNet = &((*Objects2)[NrObjects2]);
				ObjectNet->x1 = ObjectArc->CentreX;
				ObjectNet->y1 = ObjectArc->CentreY;
				ObjectNet->x2 = ObjectArc->Width;
				ObjectNet->y2 = ObjectArc->Height;
				ObjectNet->x3 = ObjectArc->StartDiffX;
				ObjectNet->y3 = ObjectArc->StartDiffY;
				ObjectNet->x4 = ObjectArc->EndDiffX;
				ObjectNet->y4 = ObjectArc->EndDiffY;
				ObjectNet->Info = 0;
				ObjectNet->Test = 0;
				ObjectNet->Layer = ObjectArc->Layer;
				ObjectNet->ObjectType = TRACE_ARC;
				ObjectNet->TraceNr = cnt;
				ObjectNet->NetNr = NetNr;
				ObjectNet->Clearance = ObjectArc->Clearance;
				ObjectNet->Thickness = ObjectArc->LineThickNess;

				if (NrObjects2 >= MaxNrObjects2 - 1)
				{
					if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
						NrObjects2++;
				}
				else
					NrObjects2++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectArc->Layer == DRILL_LAYER)
			        && (ObjectArc->NetNr == NetNr))
			{
				ObjectNet = &((*Objects2)[NrObjects2]);
				ObjectNet->x1 = ObjectArc->CentreX;
				ObjectNet->y1 = ObjectArc->CentreY;
				ObjectNet->x2 = ObjectArc->Width;
				ObjectNet->y2 = ObjectArc->Width;
				ObjectNet->Clearance = ObjectArc->Clearance;
				ObjectNet->ObjectType = VIA_PUT_THROUGH_ROUND;
				ObjectNet->Info = 0;
				ObjectNet->CompNr = -1;
				ObjectNet->TraceNr = cnt;
				ObjectNet->Layer = -1;
				ObjectNet->NetNr = NetNr;

				if (NrObjects2 >= MaxNrObjects2 - 1)
				{
					if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
						NrObjects2++;
				}
				else
					NrObjects2++;
			}
		}

		CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
		CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

		//  hulp=NrObjects2;
		for (cnt = 0; cnt < NrObjects2; cnt++)
		{
			Object = &((*Objects2)[cnt]);
			Object->Info &= ~OBJECT_DONE;
			FillPositionObject(Object);
#ifdef _DEBUG

			if (Object->minx == 0.0)
				ok = 1;

#endif
			Object->Test = -1;
//        SearchX=(float)15443200;
//        SearchY=(float)19075400;
//        SearchY=(float)18821400;
//        if ((InRange3(Object->x1,SearchX))
//           &&
//           (InRange3(Object->y1,SearchY))) {
//          ok=1;
//        }
		}

#ifdef _DEBUG

		if (0)
		{
			for (cnt2 = 0; cnt2 < NrObjects2; cnt2++)
			{
				Object = &((*Objects2)[cnt2]);
				FillPositionObject(Object);

				if ((Object->minx < CurrentX) && (Object->maxx > CurrentX) && (Object->miny < CurrentY)
				        && (Object->maxy > CurrentY))
					PrintObjectInText(Object);
			}
		}

#endif
		break;

// *******************************************************************************************************
// *******************************************************************************************************
	case 3:
		NrObjects3 = 0;

		for (Layer = 0; Layer < 32; Layer++)
		{
			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects3)[NrObjects3]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_VER;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects3 >= MaxNrObjects3 - 1)
						{
							if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
								NrObjects3++;
						}
						else
							NrObjects3++;
					}
				}
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects3)[NrObjects3]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_HOR;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects3 >= MaxNrObjects3 - 1)
						{
							if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
								NrObjects3++;
						}
						else
							NrObjects3++;
					}
				}
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects3)[NrObjects3]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_DIAG1;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects3 >= MaxNrObjects3 - 1)
						{
							if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
								NrObjects3++;
						}
						else
							NrObjects3++;
					}
				}
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);

				if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (Trace->NetNr == NetNr)
					{
						ObjectNet = &((*Objects3)[NrObjects3]);
						ObjectNet->x1 = Trace->X;
						ObjectNet->y1 = Trace->Y;
						ObjectNet->x2 = Trace->Length;
						ObjectNet->y2 = Trace->ThickNess;
						ObjectNet->Clearance = Trace->Clearance;
						ObjectNet->ObjectType = TRACE_DIAG2;
						ObjectNet->Info = (Trace->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->Layer = Layer;
						ObjectNet->CompNr = -1;
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects3 >= MaxNrObjects3 - 1)
						{
							if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
								NrObjects3++;
						}
						else
							NrObjects3++;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if (Via->NetNr == NetNr)
				{
					ObjectNet = &((*Objects3)[NrObjects3]);
					ObjectNet->x1 = Via->X;
					ObjectNet->y1 = Via->Y;
					ObjectNet->x2 = Via->ThickNess;
					ObjectNet->y2 = Via->DrillThickNess;
					ObjectNet->Clearance = Via->Clearance;
					ObjectNet->ObjectType = VIA_PUT_THROUGH_ROUND;
					ObjectNet->Info = (Via->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
					ObjectNet->CompNr = -1;
					ObjectNet->TraceNr = cnt;
					ObjectNet->Layer = -1;
					ObjectNet->NetNr = NetNr;

					if (NrObjects3 >= MaxNrObjects3 - 1)
					{
						if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
							NrObjects3++;
					}
					else
						NrObjects3++;
				}
			}
		}


		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);

			if ((Connection->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if (Connection->NetNr == NetNr)
				{
					if (Mode == 0)
					{
						ObjectNet = &((*Objects3)[NrObjects3]);
						ObjectNet->x1 = Connection->x1;
						ObjectNet->y1 = Connection->y1;
						ObjectNet->x2 = Connection->x2;
						ObjectNet->y2 = Connection->y2;
						ObjectNet->ObjectType = CONNECTION;
						ObjectNet->Info = (Connection->Info & ~OBJECT_HIGHLITED) | (Net->Info & OBJECT_HIGHLITED);
						ObjectNet->TraceNr = cnt;
						ObjectNet->NetNr = NetNr;

						if (NrObjects3 >= MaxNrObjects3 - 1)
						{
							if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
								NrObjects3++;
						}
						else
							NrObjects3++;
					}
					else
						Connection->Extra1 = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				NrObjects = 0;
//          if (cnt==530) {
//            ok=1;
//          }
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (Object->NetNr == NetNr)
					{
						ObjectNet = &((*Objects3)[NrObjects3]);
						memcpy(ObjectNet, Object, sizeof(ObjectRecord));

						if (NrObjects3 >= MaxNrObjects3 - 1)
						{
							if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
								NrObjects3++;
						}
						else
							NrObjects3++;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr == NetNr))
			{
				ObjectNet = &((*Objects3)[NrObjects3]);
				ObjectNet->ObjectType = AREAFILL;
				ObjectNet->ObjectType2 = 0;
				AreaPos = (uint8 *) AreaFill;
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
				ObjectNet->x1 = (*DrawPolygon).Points[0].x;
				ObjectNet->y1 = (*DrawPolygon).Points[0].y;
				ObjectNet->Info = AreaFill->Info;
				ObjectNet->TraceNr = cnt;
				ObjectNet->Layer = AreaFill->Layer;
				ObjectNet->NetNr = NetNr;

				if (NrObjects3 >= MaxNrObjects3 - 1)
				{
					if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
						NrObjects3++;
				}
				else
					NrObjects3++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->Layer < 32)
			        && (ObjectLine->NetNr == NetNr))
			{
				ObjectNet = &((*Objects3)[NrObjects3]);
				ObjectNet->x1 = ObjectLine->X1;
				ObjectNet->y1 = ObjectLine->Y1;
				ObjectNet->x2 = ObjectLine->X2;
				ObjectNet->y2 = ObjectLine->Y2;
				ObjectNet->Info = 0;
				ObjectNet->Test = 0;
				ObjectNet->Clearance = ObjectLine->Clearance;
				ObjectNet->TraceNr = cnt;
				ObjectNet->NetNr = NetNr;
				ObjectNet->Layer = ObjectLine->Layer;
				ObjectNet->ObjectType = TRACE_ALL_ANGLE;
				ObjectNet->Thickness = ObjectLine->LineThickNess;

				if (NrObjects3 >= MaxNrObjects3 - 1)
				{
					if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
						NrObjects3++;
				}
				else
					NrObjects3++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0) && (ObjectArc->Layer < 32)
			        && (ObjectArc->NetNr == NetNr))
			{
				ObjectNet = &((*Objects3)[NrObjects3]);
				ObjectNet->x1 = ObjectArc->CentreX;
				ObjectNet->y1 = ObjectArc->CentreY;
				ObjectNet->x2 = ObjectArc->Width;
				ObjectNet->y2 = ObjectArc->Height;
				ObjectNet->x3 = ObjectArc->StartDiffX;
				ObjectNet->y3 = ObjectArc->StartDiffY;
				ObjectNet->x4 = ObjectArc->EndDiffX;
				ObjectNet->y4 = ObjectArc->EndDiffY;
				ObjectNet->Info = 0;
				ObjectNet->Test = 0;
				ObjectNet->Layer = ObjectArc->Layer;
				ObjectNet->ObjectType = TRACE_ARC;
				ObjectNet->TraceNr = cnt;
				ObjectNet->NetNr = NetNr;
				ObjectNet->Clearance = ObjectArc->Clearance;
				ObjectNet->Thickness = ObjectArc->LineThickNess;

				if (NrObjects3 >= MaxNrObjects3 - 1)
				{
					if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
						NrObjects3++;
				}
				else
					NrObjects3++;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectArc->Layer == DRILL_LAYER)
			        && (ObjectArc->NetNr == NetNr))
			{
				ObjectNet = &((*Objects3)[NrObjects3]);
				ObjectNet->x1 = ObjectArc->CentreX;
				ObjectNet->y1 = ObjectArc->CentreY;
				ObjectNet->x2 = ObjectArc->Width;
				ObjectNet->y2 = ObjectArc->Width;
				ObjectNet->Clearance = ObjectArc->Clearance;
				ObjectNet->ObjectType = VIA_PUT_THROUGH_ROUND;
				ObjectNet->Info = 0;
				ObjectNet->CompNr = -1;
				ObjectNet->TraceNr = cnt;
				ObjectNet->Layer = -1;
				ObjectNet->NetNr = NetNr;

				if (NrObjects3 >= MaxNrObjects3 - 1)
				{
					if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
						NrObjects3++;
				}
				else
					NrObjects3++;
			}
		}

		//  hulp=NrObjects3;
		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object = &((*Objects3)[cnt]);
			Object->Info &= ~OBJECT_DONE;
			FillPositionObject(Object);
			Object->Test = -1;
#ifdef _DEBUG

			if (Object->minx == 0.0)
				ok = 1;

#endif
//        SearchX=(float)15443200;
//        SearchY=(float)18821350;
//        if ((InRange3(Object->x1,SearchX))
//           &&
//           (InRange3(Object->y1,SearchY))) {
//          ok=1;
//        }
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AddPowerPlaneObjects3(int32 PowerNetNr, int32 mode)
{
	int32 cnt, cnt2;

	ViaRecord *Via;
	CompRecord *Comp;
	ObjectRecord *Object, *ObjectNet;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if (Via->NetNr == PowerNetNr)
			{
				ObjectNet = &((*Objects3)[NrObjects3]);
				ObjectNet->x1 = Via->X;
				ObjectNet->y1 = Via->Y;
				ObjectNet->x2 = Via->ThickNess;
				ObjectNet->Clearance = Via->Clearance;
				ObjectNet->ObjectType = VIA_PUT_THROUGH_ROUND + 1;
				ObjectNet->Info = Via->Info;
				ObjectNet->CompNr = -1;
				ObjectNet->TraceNr = cnt;
				ObjectNet->Layer = -1;
				ObjectNet->NetNr = PowerNetNr;

				if (NrObjects3 >= MaxNrObjects3 - 1)
				{
					if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
						NrObjects3++;
				}
				else
					NrObjects3++;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			NrObjects = 0;
//          if (cnt==530) {
//            ok=1;
//          }
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->NetNr == PowerNetNr)
				{
					ObjectNet = &((*Objects3)[NrObjects3]);
					memcpy(ObjectNet, Object, sizeof(ObjectRecord));
					ObjectNet->ObjectType += 1;

					if (NrObjects3 >= MaxNrObjects3 - 1)
					{
						if (AllocateMemObjects3(MaxNrObjects3 + 1024) == 0)
							NrObjects3++;
					}
					else
						NrObjects3++;
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CountNrObjectsEndPoint(int32 Layer, double x, double y, int32 mode)
{
	int32 cnt, Count;
	double x1, y1, x2, x2a, y2;
	ObjectRecord *Object;
	int32 OkToAdd;

	Count = 0;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		x2a = x2 * 0.5;
		y2 = Object->y2;
		OkToAdd = 0;

		if (Object->ObjectType != CONNECTION)
		{
			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				break;

			case TRACE_HOR:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
					OkToAdd = 1;

				break;

			case PIN_LINE_HOR:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2a)) && (InRange(y, y1)))
					return cnt;

				break;

			case TRACE_VER:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
					OkToAdd = 1;

				break;

			case PIN_LINE_VER:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
					OkToAdd = 1;

				if ((InRange(x, x1)) && (InRange(y, y1 + x2a)))
					OkToAdd = 1;

				break;

			case TRACE_DIAG1:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
					OkToAdd = 1;

				break;

			case PIN_LINE_DIAG1:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2a)) && (InRange(y, y1 - x2a)))
					OkToAdd = 1;

				break;

			case TRACE_DIAG2:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
					OkToAdd = 1;

				break;

			case PIN_LINE_DIAG2:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
					OkToAdd = 1;

				if ((InRange(x, x1 + x2a)) && (InRange(y, y1 + x2a)))
					OkToAdd = 1;

				break;

			case TRACE_ALL_ANGLE:
			case PIN_LINE_ALL_ANGLE:
				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x2)) && (InRange(y, y2)))
					OkToAdd = 1;

				break;

			case TRACE_ARC:
			case PIN_ARC:
				GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);

				if ((InRange(x, x1)) && (InRange(y, y1)))
					OkToAdd = 1;

				if ((InRange(x, x2)) && (InRange(y, y2)))
					OkToAdd = 1;

				break;

			}

			if (OkToAdd)
			{
				if ((Layer == -1) || (Object->Layer == -1) || (Object->Layer == Layer))
					Count++;
			}
		}
	}

	return Count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetObjectNrFromEndPoint(int32 Layer, double x, double y, int32 mode)
{
	int32 cnt, FirstCnt, PutThroughCnt;
	double x1, y1, x2, x2a, y2;
	ObjectRecord *Object, Object2;

	Object2.ObjectType = PIN_SMD_ROUND;
	Object2.Layer = Layer;
	Object2.x1 = x;
	Object2.y1 = y;
	Object2.x2 = 50.0;
	Object2.y2 = 50.0;
	Object2.Clearance = 1.0;

	if ((mode & 4) == 4)
	{
		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object = &((*Objects3)[cnt]);
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			x2a = x2 * 0.5;
			y2 = Object->y2;

			if ((Object->ObjectType != CONNECTION) && (Object->Layer != -1))
			{
				switch (Object->ObjectType)
				{
				case TRACE_HOR:
					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
						return cnt;

					break;

				case PIN_LINE_HOR:
					if ((InRange(x, x1 + x2a)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
						return cnt;

					break;

				case TRACE_VER:
					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
						return cnt;

					break;

				case PIN_LINE_VER:
					if ((InRange(x, x1)) && (InRange(y, y1 + x2a)))
						return cnt;

					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
						return cnt;

					break;

				case TRACE_DIAG1:
					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
						return cnt;

					break;

				case PIN_LINE_DIAG1:
					if ((InRange(x, x1 + x2a)) && (InRange(y, y1 - x2a)))
						return cnt;

					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
						return cnt;

					break;

				case TRACE_DIAG2:
					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
						return cnt;

					break;

				case PIN_LINE_DIAG2:
					if ((InRange(x, x1 + x2a)) && (InRange(y, y1 + x2a)))
						return cnt;

					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
						return cnt;

					break;

				case TRACE_ALL_ANGLE:
				case PIN_LINE_ALL_ANGLE:
					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x2)) && (InRange(y, x2)))
						return cnt;

					break;

				case PIN_ARC:
				case TRACE_ARC:
					GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);

					if ((InRange(x, x1)) && (InRange(y, y1)))
						return cnt;

					if ((InRange(x, x2)) && (InRange(y, x2)))
						return cnt;

					break;
				}
			}
		}

		return -1;
	}

	FirstCnt = -1;
	PutThroughCnt = -1;

	if ((mode & 2) == 0)
	{
		if (Layer != -1)
		{
			for (cnt = 0; cnt < NrObjects3; cnt++)
			{
				Object = &((*Objects3)[cnt]);
				x1 = Object->x1;
				y1 = Object->y1;
				x2 = Object->x2;
				x2a = x2 * 0.5;
				y2 = Object->y2;

				if ((Object->ObjectType != CONNECTION) && (Object->Layer != -1) && (Object->Layer == Layer))
				{
					switch (Object->ObjectType)
					{
					case TRACE_HOR:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
							return cnt;

						break;

					case PIN_LINE_HOR:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1 + x2a)) && (InRange(y, y1)))
							return cnt;

						break;

					case TRACE_VER:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
							return cnt;

						break;

					case PIN_LINE_VER:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
							return cnt;

						if ((InRange(x, x1)) && (InRange(y, y1 + x2a)))
							return cnt;

						break;

					case TRACE_DIAG1:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
							return cnt;

						break;

					case PIN_LINE_DIAG1:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
							return cnt;

						if ((InRange(x, x1 + x2a)) && (InRange(y, y1 - x2a)))
							return cnt;

						break;

					case TRACE_DIAG2:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
							return cnt;

						break;

					case PIN_LINE_DIAG2:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
							return cnt;

						if ((InRange(x, x1 + x2a)) && (InRange(y, y1 + x2a)))
							return cnt;

						break;

					case TRACE_ALL_ANGLE:
					case PIN_LINE_ALL_ANGLE:
						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x2)) && (InRange(y, x2)))
							return cnt;

						break;

					case PIN_ARC:
					case TRACE_ARC:
						GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);

						if ((InRange(x, x1)) && (InRange(y, y1)))
							return cnt;

						if ((InRange(x, x2)) && (InRange(y, x2)))
							return cnt;

						break;
					}
				}
			}
		}
	}

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		x2a = x2 * 0.5;
		y2 = Object->y2;

		if (Layer == -1)
			Object2.Layer = Object->Layer;

		if (Object->ObjectType != CONNECTION)
		{
			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case DRILL:
			case PIN_PUT_THROUGH_POLYGON:
				if (((mode & 1) == 1) && (InRange(x1, x)) && (InRange(y1, y)))
				{
					if (PutThroughCnt == -1)
						PutThroughCnt = cnt;
				}

				break;

			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_SMD_POLYGON:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case TRACE_HOR:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case PIN_LINE_HOR:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2a)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case TRACE_VER:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case PIN_LINE_VER:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1)) && (InRange(y, y1 + x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1)) && (InRange(y, y1 + x2a)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case TRACE_DIAG1:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case PIN_LINE_DIAG1:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 - x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2a)) && (InRange(y, y1 - x2a)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case TRACE_DIAG2:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case PIN_LINE_DIAG2:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2)) && (InRange(y, y1 + x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x1 + x2a)) && (InRange(y, y1 + x2a)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case TRACE_ALL_ANGLE:
			case PIN_LINE_ALL_ANGLE:
				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x2)) && (InRange(y, x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;

			case PIN_ARC:
			case TRACE_ARC:
				GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);

				if ((InRange(x, x1)) && (InRange(y, y1)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				if ((InRange(x, x2)) && (InRange(y, x2)))
				{
					if (FirstCnt == -1)
						FirstCnt = cnt;
				}

				break;
			}
		}
	}

	if (PutThroughCnt != -1)
		return PutThroughCnt;

	if ((mode & 8) == 0)
	{
		if (FirstCnt != -1)
			return FirstCnt;
	}

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		if (Layer == -1)
			Object2.Layer = Object->Layer;

		if ((Object->ObjectType != CONNECTION) && (ObjectsConnected(Object, &Object2)))
		{
			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				if (PutThroughCnt == -1)
					PutThroughCnt = cnt;

				break;

			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_SMD_POLYGON:
			case TRACE_HOR:
			case PIN_LINE_HOR:
			case TRACE_VER:
			case PIN_LINE_VER:
			case TRACE_DIAG1:
			case PIN_LINE_DIAG1:
			case TRACE_DIAG2:
			case PIN_LINE_DIAG2:
			case TRACE_ALL_ANGLE:
			case PIN_LINE_ALL_ANGLE:
			case PIN_ARC:
			case TRACE_ARC:
				if (FirstCnt == -1)
					FirstCnt = cnt;

				break;
			}
		}
	}

	if (PutThroughCnt != -1)
		return PutThroughCnt;

	return FirstCnt;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetObjectNrFromEndPoint2(int32 Layer, double x, double y)
{
	int32 cnt, FirstCnt, PutThroughCnt;
	double x1, y1, x2, y2;
	ObjectRecord *Object, Object2;

	Object2.ObjectType = PIN_SMD_ROUND;
	Object2.Layer = Layer;
	Object2.x1 = x;
	Object2.y1 = y;
	Object2.x2 = 50.0;
	Object2.y2 = 50.0;
	Object2.Clearance = 1.0;

	FirstCnt = -1;
	PutThroughCnt = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		/*
		    if ((x1>234.1e5)
		       &&
		       (x1<234.2e5)
		       &&
		       (y1>36.3e5)
		       &&
		       (y1<36.5e5)) {
		      ok=1;
		    }
		*/
		x2 = Object->x2;
		y2 = Object->y2;

		if (Layer == -1)
			Object2.Layer = Object->Layer;

		if (Object->ObjectType != CONNECTION)
		{
			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:

//        case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_SMD_POLYGON:
			case PIN_PUT_THROUGH_POLYGON:
				if ((InRange3(Object->x1, Object2.x1)) && (InRange3(Object->y1, Object2.y1)))
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

int32 GetViaObjectNrFromEndPoint(double x, double y)
{
	int32 cnt, FirstCnt, PutThroughCnt, Layer;
	double x1, y1, x2, y2;
	ObjectRecord *Object, Object2;

	Layer = -1;
	Object2.ObjectType = PIN_SMD_ROUND;
	Object2.Layer = Layer;
	Object2.x1 = x;
	Object2.y1 = y;
	Object2.x2 = 50.0;
	Object2.y2 = 50.0;
	Object2.Clearance = 1.0;

	FirstCnt = -1;
	PutThroughCnt = -1;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object = &((*Objects3)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		if (Layer == -1)
			Object2.Layer = Object->Layer;

		if ((Object->ObjectType != CONNECTION) && (ObjectsConnected(Object, &Object2)))
		{
			switch (Object->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				if (PutThroughCnt == -1)
					PutThroughCnt = cnt;

				break;

			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_SMD_POLYGON:
			case TRACE_HOR:
			case PIN_LINE_HOR:
			case TRACE_VER:
			case PIN_LINE_VER:
			case TRACE_DIAG1:
			case PIN_LINE_DIAG1:
			case TRACE_DIAG2:
			case PIN_LINE_DIAG2:
			case TRACE_ALL_ANGLE:
			case PIN_LINE_ALL_ANGLE:
			case PIN_ARC:
			case TRACE_ARC:
				if (FirstCnt == -1)
					FirstCnt = cnt;

				break;
			}
		}
	}

	if (PutThroughCnt != -1)
		return PutThroughCnt;

	return FirstCnt;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckConnection(int32 Layer, double ConnectionX1, double ConnectionY1, double ConnectionX2, double ConnectionY2)
{
	int32 cnt;
	double x1, y1, x2, y2;
	int32 Found;

	Found = 0;
	cnt = 0;

	while ((cnt < NrNetConnections) && (!Found))
	{
		x1 = (*NetConnections)[cnt].x1;
		y1 = (*NetConnections)[cnt].y1;
		x2 = (*NetConnections)[cnt].x2;
		y2 = (*NetConnections)[cnt].y2;

		if ((*NetConnections)[cnt].Layer == Layer)
		{
			if (((InRange(x1, ConnectionX1)) && (InRange(y1, ConnectionY1)) && (InRange(x2, ConnectionX2))
			        && (InRange(y2, ConnectionY2))) || ((InRange(x1, ConnectionX2)) && (InRange(y1, ConnectionY2))
			                && (InRange(x2, ConnectionX1)) && (InRange(y2, ConnectionY1))))
				Found = 1;
		}

		cnt++;
	}

	return Found;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SearchUnconnectedObject(int32 * ObjectNr)
{
	int32 cnt2;
	ObjectRecord *Object2;
#ifdef _DEBUG
	int32 ok;
#endif

	for (cnt2 = 0; cnt2 < NrObjects2; cnt2++)
	{
		Object2 = &((*Objects2)[cnt2]);
#ifdef _DEBUG

		if ((InRange9(Object2->x1, 41.5e5)) && (InRange9(Object2->y1, 67.8e5)))
			ok = 1;

#endif

		if ((Object2->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
		{
			*ObjectNr = cnt2;
			return 1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetObjectsConnectedToObject(int32 ObjectNr)
{
	ObjectRecord *CheckObject, *Object;
	int32 cnt, res;
#ifdef _DEBUG
	int32 ok;
#endif

#ifdef _DEBUG

	if ((ObjectNr < 0) || (ObjectNr >= NrObjects2))
		ok = 1;

#endif
	CheckObject = &((*Objects2)[ObjectNr]);

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);

		if ((cnt != ObjectNr) && ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0))
		{
			CheckCount++;
#ifdef _DEBUG

			if ((InRange9(Object->x1, 61.9e5)) && (InRange9(Object->y1, 16.66e5)))
			{
				ok = 1;

				if (CheckObject->ObjectType == AREAFILL)
					ok = 1;
			}

			if ((Object->minx == 0.0) || (CheckObject->minx == 0.0))
				ok = 1;

#endif

			if ((Object->minx < CheckObject->maxx) && (Object->miny < CheckObject->maxy)
			        && (Object->maxx > CheckObject->minx) && (Object->maxy > CheckObject->miny))
			{
#ifdef _DEBUG

				if ((InRange9(Object->x1, 61.9e5)) && (InRange9(Object->y1, 16.66e5)))
				{
					ok = 1;

					if (CheckObject->ObjectType == AREAFILL)
						ok = 1;
				}

#endif
				res = ObjectsConnected(CheckObject, Object);
#ifdef _DEBUG

				if ((InRangeSpecial(Object->x1, 2.54e5, 0.02e5)) && (InRangeSpecial(Object->y1, 11.05e5, 0.02e5)))
					ok = 1;

				if ((InRangeSpecial(Object->x2, 2.54e5, 0.02e5)) && (InRangeSpecial(Object->y2, 11.05e5, 0.02e5)))
					ok = 1;

				if ((InRangeSpecial(CheckObject->x1, 2.54e5, 0.02e5))
				        && (InRangeSpecial(CheckObject->y1, 11.05e5, 0.02e5)))
					ok = 1;

				if ((InRangeSpecial(CheckObject->x2, 2.54e5, 0.02e5))
				        && (InRangeSpecial(CheckObject->y2, 11.05e5, 0.02e5)))
					ok = 1;

#endif

				if ((res) && (ObjectCount < MaxNrNetSegments))
				{
					SegmentObjects[ObjectCount++] = cnt;
					Object->Info |= OBJECT_DONE;
#ifdef _DEBUG

					if ((InRange9(Object->x1, 5.1e5)) && (InRange9(Object->y1, 108.2e5)))
						ok = 1;

					if (DrawOn)
					{
						if (Object->ObjectType == AREAFILL)
							ok = 1;

						StartDrawingEditingWindow();
						InitDrawingObject(0, ERROR_LAYER, -1, NORMAL_FILLED_AND_PEN1);
						DrawObject(Object, 64);
						ExitDrawing();
						EndDrawingEditingWindow();
//            while (!KeyPressed()) CheckInputMessages(0);
//            ReadKeyFunction();
//          }
					}

#endif
				}
			}
		}
	}

//  NrSegmentObjects

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckNet(int32 NetNr, int32 mode)
{
	double ObjectMinLength, ObjectMin2Length, ObjectLength, ShortestObjectLength, FoundObjectX, FoundObjectY,
	       ShortestObjectLengthLayer, CentreX, CentreY;
	int32 ok, SegmentCount, hulp, LayerGuide, FoundObjectNr, OldConnectionCount, NrMinConnections, ConnectionObject1,
	      ConnectionObject2, NrNetSegments, Collections, NrMinConnectionsLayer, cnt, cnt2, cnt3, cnt4;

	int32 ConnectedAll, InsertConnection;

	NetRecord *Net;
	ConnectionsRecord *Connection;
	ObjectRecord *Object, *ObjectNet, *CheckObject, CentreObject, *Object_save1 = NULL;
	CompRecord *Comp;
	char str2[MAX_LENGTH_STRING];
#ifdef _DEBUG
	ObjectRecord *ObjectsPos[100];
	int32 res2;
#endif

	FoundObjectX = 0.0;
	FoundObjectY = 0.0;
	FoundObjectNr = 0;

	Net = &((*Nets)[NetNr]);

	if (Net->Name[0] == 0)
		return -3;

	if (((mode & 1) == 0) && ((Net->Info & CONNECTIONS_DISABLED) == CONNECTIONS_DISABLED))
	{
		ok = 1;
		sprintf(InfoStr, SC(872, "%s disabled"), Net->Name);
		RedrawInfoStr(1);
		return -1;
	}

#ifdef _DEBUG

	if (stricmpOwn(Net->Name, "IOW#") == 0)
		res2 = 1;

	if (stricmp(Net->Name, "N$1023") == 0)
		res2 = 1;

#endif
	
	if ((mode & 4) == 0)
	{
		GetObjectsNet(NetNr, 2, 1);
	}
	
	else
		GetObjectsNet2(NetNr, 2, 1);

	OldConnectionCount = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if (Connection->NetNr == NetNr)
				OldConnectionCount++;
		}
	}

#ifdef _DEBUG

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);

		if ((InRangeSpecial(Object->x1, 2.54e5, 0.05e5)) && (InRangeSpecial(Object->y1, 11.05e5, 0.05e5)))
			ok = 1;
	}

	ok = 1;
#endif

	ConnectedAll = 0;
	ConnectionIndex = NrObjects2;

	if (((mode & 2) == 0) && (NrObjects2 > 500) && (BigNetInfoCheck == 0))
	{
		BigNetInfoCheck++;
//   OutputDisplay
	}

	hulp = NrObjects2;
	MaxCollections = 200;

#ifdef _DEBUG

	for (cnt = 0; cnt < 100; cnt++)
		ObjectsPos[cnt] = &((*Objects2)[cnt]);

#endif

	DrawOn = 0;
#ifdef _DEBUG

	if (stricmp(Net->Name, "N$1023") == 0)
	{
		res2 = 1;
	}

#endif

	Collections = 0;

	while (!ConnectedAll)
	{
		for (cnt = 0; cnt < NrObjects2; cnt++)
		{
			Object = &((*Objects2)[cnt]);
			Object->Info &= ~OBJECT_DONE;
		}

		NrNetSegments = 0;
		ObjectCount = 0;
		NetSegmentIndex[0] = 0;

		while (SearchUnconnectedObject(&FoundObjectNr))
		{
			cnt = ObjectCount;
			Object = &((*Objects2)[FoundObjectNr]);
			Object->Info |= OBJECT_DONE;
#ifdef _DEBUG

			if (DrawOn)
			{
				StartDrawingEditingWindow();
				InitDrawingObject(0, ERROR_LAYER, -1, NORMAL_FILLED_AND_PEN1);
				DrawObject(Object, 64);
				ExitDrawing();
				EndDrawingEditingWindow();

				while (!KeyPressed())
					CheckInputMessages(0);

				ReadKeyFunction();
			}

#endif
			SegmentObjects[ObjectCount++] = FoundObjectNr;

			while (cnt < ObjectCount)
			{
				cnt2 = SegmentObjects[cnt];
				GetObjectsConnectedToObject(cnt2);
				cnt++;
			}

			NrNetSegments++;
			NetSegmentIndex[NrNetSegments] = ObjectCount;
			ok = 1;
		}

		ok = 1;
		NetSegmentIndex[NrNetSegments] = ObjectCount;
//  NrObjects2
//  for (cnt=0;cnt<100;cnt++) ObjectsPos[cnt]=&((*Objects2)[cnt]);

		ok = 1;
		NrNetConnections = 0;

		if (NrNetSegments > 1)
			ok = 1;

		if (NrNetSegments > 1)
		{
// *******************************************************************************************************
// *******************************************************************************************************
			if (NrObjects2 < 500)
			{
				NrMinConnections = 1;
				SegmentCount = NrNetSegments;

				if (SegmentCount == 2)
					SegmentCount--;

				for (cnt = 0; cnt < SegmentCount; cnt++)
				{
					ObjectMinLength = 1000000000.0;
					ObjectMin2Length = 1000000000.0;
					ShortestObjectLength = 1000000000.0;
					ShortestObjectLengthLayer = 1000000000.0;
					NrMinConnections = 0;
					NrMinConnectionsLayer = 0;

					for (cnt2 = NetSegmentIndex[cnt]; cnt2 < NetSegmentIndex[cnt + 1]; cnt2++)
					{
						CheckObject = &((*Objects2)[SegmentObjects[cnt2]]);
#ifdef _DEBUG

						if ((InRangeSpecial(CheckObject->x1, 61.9e5, 0.2e5))
						        && (InRangeSpecial(CheckObject->x1, 16.66e5, 0.2e5)))
							ok = 1;

#endif

						for (cnt3 = 0; cnt3 < NrNetSegments; cnt3++)
						{
							if (cnt3 != cnt)
							{
								for (cnt4 = NetSegmentIndex[cnt3]; cnt4 < NetSegmentIndex[cnt3 + 1]; cnt4++)
								{
									Object = &((*Objects2)[SegmentObjects[cnt4]]);
									ObjectLength = ObjectsDistance(CheckObject, Object);
#ifdef _DEBUG

									if ((InRangeSpecial(ConnectionX1, 2.54e5, 0.02e5))
									        && (InRangeSpecial(ConnectionY1, 11.05e5, 0.02e5)))
										ok = 1;

									if ((InRangeSpecial(ConnectionX2, 2.54e5, 0.02e5))
									        && (InRangeSpecial(ConnectionY2, 11.05e5, 0.02e5)))
										ok = 1;

#endif

									/*

									Modication (7-11-2003):

									When commented out all net connections will start from an areafill point

									                  if ((CheckObject->ObjectType==AREAFILL)
									                     ||
									                     (Object->ObjectType==AREAFILL)) {
									                    ObjectLength=-1000000000.0;
									                  }
									*/
									if ((ObjectLength < ShortestObjectLength)
									        && ((NotInRange(ConnectionX1, ConnectionX2))
									            || (NotInRange(ConnectionY1, ConnectionY2))))
									{
										ShortestObjectLength = ObjectLength;
#ifdef _DEBUG

										if ((InRange9(ConnectionX1, 150.6e5)) && (InRange9(ConnectionY1, 66.6e5)))
											ok = 1;

										if ((InRange9(ConnectionX2, 150.6e5)) && (InRange9(ConnectionY2, 66.6e5)))
											ok = 1;

										if ((InRangeSpecial(ConnectionX1, 2.54e5, 0.02e5))
										        && (InRangeSpecial(ConnectionY1, 11.05e5, 0.02e5)))
											ok = 1;

										if ((InRangeSpecial(ConnectionX2, 2.54e5, 0.02e5))
										        && (InRangeSpecial(ConnectionY2, 11.05e5, 0.02e5)))
											ok = 1;

										if ((InRangeSpecial(Object->x1, 2.54e5, 0.02e5))
										        && (InRangeSpecial(Object->x1, 11.05e5, 0.02e5)))
											ok = 1;

#endif
										MinConnections[0].MinLength = ObjectLength;
										MinConnections[0].x1 = ConnectionX1;
										MinConnections[0].y1 = ConnectionY1;
										MinConnections[0].x2 = ConnectionX2;
										MinConnections[0].y2 = ConnectionY2;
										MinConnections[0].ObjectNr1 = SegmentObjects[cnt2];
										MinConnections[0].ObjectNr2 = SegmentObjects[cnt4];
										NrMinConnections = 1;
									}

									/*
									                  if ((ObjectLength<ShortestObjectLengthLayer)
									                     &&
									                     ((NotInRange(ConnectionX1,ConnectionX2)) || (NotInRange(ConnectionY1,ConnectionY2)))) {
									                    if ((CheckObject->Layer==-1)
									                       ||
									                       (Object->Layer==-1)
									                       ||
									                       (CheckObject->Layer==Object->Layer)) {
									                      ShortestObjectLengthLayer=ObjectLength;
									                      MinConnectionsLayer[0].MinLength=ObjectLength;
									                      MinConnectionsLayer[0].x1=ConnectionX1;
									                      MinConnectionsLayer[0].y1=ConnectionY1;
									                      MinConnectionsLayer[0].x2=ConnectionX2;
									                      MinConnectionsLayer[0].y2=ConnectionY2;
									                      MinConnectionsLayer[0].ObjectNr1=SegmentObjects[cnt2];
									                      MinConnectionsLayer[0].ObjectNr2=SegmentObjects[cnt4];
									                      NrMinConnectionsLayer=1;
									                    }
									                  }
									*/
								}
							}
						}
					}

					if (NrMinConnectionsLayer == 1)
						memmove(&MinConnections[0], &MinConnectionsLayer[0], sizeof(MinConnectionsRecord));

					InsertConnection = 1;

					if (NrMinConnections == 0)
						InsertConnection = 0;

					if (InsertConnection)
					{
						//        SelectedMinConnection=0;
						if (NrNetConnections + 1 >= MaxNrNetConnections)
						{
							if (AllocateMemNetConnections(NrNetConnections + 128) != 0)
								return -3;
						}

						(*NetConnections)[NrNetConnections].x1 = (float) MinConnections[0].x1;
						(*NetConnections)[NrNetConnections].y1 = (float) MinConnections[0].y1;
						(*NetConnections)[NrNetConnections].x2 = (float) MinConnections[0].x2;
						(*NetConnections)[NrNetConnections].y2 = (float) MinConnections[0].y2;

						ConnectionObject1 = MinConnections[0].ObjectNr1;
						ConnectionObject2 = MinConnections[0].ObjectNr2;
						Object = &((*Objects2)[ConnectionObject1]);
						LayerGuide = -1;

						if (Object->Layer != -1)
							LayerGuide = Object->Layer;

						Object->Test++;
						(*NetConnections)[NrNetConnections].Comp1Nr = -1;
						Comp = NULL;

						if (Object->CompNr != -1)
						{
							(*NetConnections)[NrNetConnections].Comp1Nr = (int16) Object->CompNr;
							Comp = (CompRecord *) & (CompsMem[(*Comps)[Object->CompNr]]);
						}

						Object_save1 = Object;
						Object = &((*Objects2)[ConnectionObject2]);

						if (Object->Layer != -1)
						{
							if (LayerGuide == -1)
								LayerGuide = Object->Layer;
							else if (Object->Layer != LayerGuide)
								LayerGuide = -1;
						}
						else
						{
							if (LayerGuide == -1)
								LayerGuide = Design.NrBoardLayers;
						}

#ifdef _DEBUG

						if (Collections >= MaxCollections - 10)
							res2 = 1;

#endif
						Object->Test++;
						Comp = NULL;
						(*NetConnections)[NrNetConnections].Comp2Nr = -1;

						if (Object->CompNr != -1)
						{
							(*NetConnections)[NrNetConnections].Comp2Nr = (int16) Object->CompNr;
//              Comp=(CompRecord *)&(CompsMem[(*Comps)[Object->CompNr]]);
						}

//            ConnectionsInObjects[ConnectionObject1]++;
//            ConnectionsInObjects[ConnectionObject2]++;

						(*NetConnections)[NrNetConnections].Layer = (int16) LayerGuide;

#ifdef _DEBUG

						if ((InRangeSpecial(MinConnections[0].x1, 2.54e5, 0.05e5))
						        && (InRangeSpecial(MinConnections[0].y1, 11.05e5, 0.05e5)))
							ok = 1;

						if ((InRangeSpecial(MinConnections[0].x2, 2.54e5, 0.05e5))
						        && (InRangeSpecial(MinConnections[0].y2, 11.05e5, 0.05e5)))
							ok = 1;

#endif

						if (!CheckConnection
						        (LayerGuide, MinConnections[0].x1, MinConnections[0].y1, MinConnections[0].x2,
						         MinConnections[0].y2))
							NrNetConnections++;
					}
				}

				ok = 1;

				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					ObjectNet = &((*Objects2)[cnt]);
					ObjectNet->Test = -1;
				}

				for (cnt = 0; cnt < NrNetConnections; cnt++)
				{
//          if (NetNr==75) {
//            cnt2=0;
//          }
					ObjectNet = &((*Objects2)[NrObjects2]);
#ifdef _DEBUG

					if (NrObjects2 == 18)
						res2 = 1;

#endif
					ObjectNet->x1 = (*NetConnections)[cnt].x1;
					ObjectNet->y1 = (*NetConnections)[cnt].y1;
					ObjectNet->x2 = (*NetConnections)[cnt].x2;
					ObjectNet->y2 = (*NetConnections)[cnt].y2;
					ObjectNet->Clearance = 0.0;
					ObjectNet->ObjectType = CONNECTION;
					ObjectNet->PinNr = 0;
					ObjectNet->CompNr = (*NetConnections)[cnt].Comp1Nr;
					ObjectNet->CompNr2 = (*NetConnections)[cnt].Comp2Nr;
					ObjectNet->NetNr = NetNr;
					ObjectNet->Test = -1;
					ObjectNet->Layer = (*NetConnections)[cnt].Layer;
					FillPositionObject(ObjectNet);
#ifdef _DEBUG

					if ((InRangeSpecial(ObjectNet->x1, 2.54e5, 0.05e5))
					        && (InRangeSpecial(ObjectNet->y1, 11.05e5, 0.05e5)))
						ok = 1;

					if ((InRangeSpecial(ObjectNet->x2, 2.54e5, 0.05e5))
					        && (InRangeSpecial(ObjectNet->y2, 11.05e5, 0.05e5)))
						ok = 1;

					if (Collections >= MaxCollections - 10)
						res2 = 1;

#endif

					NrObjects2++;

				}

// *******************************************************************************************************
// *******************************************************************************************************
			}
			else
			{
				CentreX = 0.0;
				CentreY = 0.0;

				for (cnt = 0; cnt < NrObjects2; cnt++)
				{
					ObjectNet = &((*Objects2)[cnt]);
					CentreX = CentreX + ObjectNet->minx + ObjectNet->maxx;
					CentreY = CentreY + ObjectNet->miny + ObjectNet->maxy;
				}

				CentreX = CentreX / (NrObjects2) / 2;
				CentreY = Design.BoardOriginY - (4000 * 2540);
//        CentreY=CentreY/(NrObjects2)/2;
				CentreObject.x1 = CentreX;
				CentreObject.y1 = CentreY;
				CentreObject.ObjectType = VIA_PUT_THROUGH_ROUND;
				CentreObject.x2 = 100.0;

				for (cnt = 0; cnt < NrNetSegments; cnt++)
				{
					ObjectMinLength = 1000000000.0;
					ObjectMin2Length = 1000000000.0;
					ShortestObjectLength = 1000000000.0;
					NrMinConnections = 0;

					for (cnt2 = NetSegmentIndex[cnt]; cnt2 < NetSegmentIndex[cnt + 1]; cnt2++)
					{
						CheckObject = &((*Objects2)[SegmentObjects[cnt2]]);
						ObjectLength = ObjectsDistance(CheckObject, &CentreObject);

						if ((CheckObject->ObjectType == AREAFILL) || (CentreObject.ObjectType == AREAFILL))
							ObjectLength = -1000000000.0;

						if (ObjectLength < ShortestObjectLength)
						{
							ShortestObjectLength = ObjectLength;
							FoundObjectNr = cnt2;
							FoundObjectX = ConnectionX1;
							FoundObjectY = ConnectionY1;
						}
					}

					CheckObject = &((*Objects2)[SegmentObjects[FoundObjectNr]]);
					ObjectNet = &((*Objects2)[NrObjects2]);
#ifdef _DEBUG

					if (NrObjects2 == 18)
						res2 = 1;

#endif
					ObjectNet->x1 = FoundObjectX;
					ObjectNet->y1 = FoundObjectY;
					ObjectNet->x2 = CentreX;
					ObjectNet->y2 = CentreY;
					ObjectNet->Clearance = 0.0;
					ObjectNet->ObjectType = CONNECTION;
					ObjectNet->PinNr = 0;
					ObjectNet->CompNr = CheckObject->CompNr;
					ObjectNet->NetNr = NetNr;
					ObjectNet->Test = -1;
					ObjectNet->Layer = -1;
					FillPositionObject(ObjectNet);

					if (NrObjects2 == MaxNrObjects2 - 1)
					{
						if (AllocateMemObjects2(MaxNrObjects2 + 1024) == 0)
							NrObjects2++;
					}
					else
						NrObjects2++;

#ifdef _DEBUG

					if (Collections >= MaxCollections - 10)
						res2 = 1;

#endif
				}

				NrNetConnections = 0;
			}

// *******************************************************************************************************
// *******************************************************************************************************
		}

//  ObjectsPos

		if (NrNetConnections == 0)
			ConnectedAll = 1;

		Collections++;

		if (Collections == MaxCollections - 1)
			ok = 1;

		if (Collections == MaxCollections)
		{
			ConnectedAll = 1;
			sprintf(str2, SC(873, "Error in connectivity ( Net %s )"), Net->Name);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		}
	}

	NrNetConnections = NrObjects2 - ConnectionIndex;

#ifdef _DEBUG

	if (0)
		SearchUnconnectedObject(&FoundObjectNr);

#endif

	if (NrNetConnections == 0)
	{
		if (OldConnectionCount > 0)
			return 1;

		return 0;
	}

	if (OldConnectionCount > 0)
		return 3;

	return 2;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 InsertConnections(int32 NetNr, int32 mode)
{
	int32 cnt, count;
	ObjectRecord *Object;
	ConnectionsRecord NewConnection;
	NetRecord *Net;

	Net = &((*Nets)[NetNr]);

	if (Net->Name[0] == 0)
		return 0;

	count = NrObjects2 - ConnectionIndex;

	if ((Net->Info & CONNECTIONS_DISABLED) == 0)
	{
		for (cnt = ConnectionIndex; cnt < NrObjects2; cnt++)
		{
			Object = &((*Objects2)[cnt]);
			NewConnection.x1 = (float) Object->x1;
			NewConnection.y1 = (float) Object->y1;
			NewConnection.x2 = (float) Object->x2;
			NewConnection.y2 = (float) Object->y2;
			NewConnection.Comp1Nr = (int16) Object->CompNr;
			NewConnection.Comp2Nr = (int16) Object->CompNr2;
			NewConnection.NetNr = (int16) NetNr;
			NewConnection.Layer = (int16) Object->Layer;
			NewConnection.Info = 0;
			AddConnection(&NewConnection);
		}
	}

	ConnectionIndex = 0;
	NrObjects2 = 0;
	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteAndUndisplayConnectionsNet(int32 NetNr, int32 mode)
{
	int32 cnt;
	ConnectionsRecord *Connection;
	NetRecord *Net;

	Net = &((*Nets)[NetNr]);

	if (Net->Name[0] == 0)
		return;

	if (mode == 0)
	{
		StartDrawingEditingWindow();
		InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if (((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Connection->NetNr == NetNr))
		{
			if (mode == 0)
			{
				if (((Net->Info & (CONNECTIONS_DISABLED)) == 0)
				        && ((Net->Info & (CONNECTIONS_NOT_VISIBLE | OBJECT_HIGHLITED)) != CONNECTIONS_NOT_VISIBLE))
					DrawConnection2(Connection);
			}

			ZeroUnusedObjects(0);
			Connection->Info |= OBJECT_NOT_VISIBLE;
			Connection->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

	if (mode == 0)
	{
		ExitDrawing();
		EndDrawingEditingWindow();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ReCalcConnectionsNet(int32 NetNr, int32 mode, int32 RePaint)
{
	int32 cnt, count, res;
	ConnectionsRecord *Connection;
	NetRecord *Net;

	if ((NetNr < 0) || (NetNr >= Design.NrNets))
		return;

	Net = &((*Nets)[NetNr]);

	if (Net->Name[0] == 0)
		return;

	if ((Net->Info & (CONNECTIONS_DISABLED)) != 0)
	{
		if (mode == 1)
		{
			MessageBoxOwn(PCBWindow, SC(874, "Net connectivity check is disabled"), SC(1, "Message"),
			              MB_APPLMODAL | MB_OK);
		}

		return;
	}

	if (mode == 1)
		mode = 2;

	res = CheckNet(NetNr, (int32) mode);

	if (res < 0)
		return;

	if ((res == 1) || (res == 3))
	{
		if (!RePaint)
			DeleteAndUndisplayConnectionsNet(NetNr, 0);
		else
			DeleteAndUndisplayConnectionsNet(NetNr, 1);
	}

	if ((res == 2) || (res == 3))
	{
		count = InsertConnections(NetNr, 1);

		if (!RePaint)
		{
			StartDrawingEditingWindow();
			SetROP2(OutputDisplay, R2_XORPEN);
			InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
		}

		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);

			if (((Connection->Info & OBJECT_NOT_VISIBLE) == 0) && (Connection->NetNr == NetNr))
			{
				if ((Net->Info & (OBJECT_HIGHLITED)) == OBJECT_HIGHLITED)
					Connection->Info |= OBJECT_HIGHLITED;

				if (!RePaint)
					DrawConnection(Connection);
			}
		}

		if (!RePaint)
		{
			ExitDrawing();
			EndDrawingEditingWindow();
		}

		if (mode == 2)
			MessageBoxOwn(PCBWindow, SC(875, "Connectivity error(s)"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
	}
	else
	{
		if (mode == 2)
			MessageBoxOwn(PCBWindow, SC(876, "Connectivity ok"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteNet(int32 NetNr)
{
	int32 Layer, cnt;
	TraceRecord *Trace;
	ViaRecord *Via;
	ConnectionsRecord *Connection;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->NetNr == NetNr) && (Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->NetNr == NetNr) && (Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->NetNr == NetNr) && (Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->NetNr == NetNr) && (Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				ZeroUnusedObjects(0);
				Trace->Info |= OBJECT_NOT_VISIBLE;
				Trace->DeleteNr = (int16) LastActionNr;
			}
		}

	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->NetNr == NetNr) && (Via->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			ZeroUnusedObjects(0);
			Via->Info |= OBJECT_NOT_VISIBLE;
			Via->DeleteNr = (int16) LastActionNr;
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if (((Connection->Info & OBJECT_NOT_VISIBLE) == 0) && (Connection->NetNr == NetNr))
		{
			ZeroUnusedObjects(0);
			Connection->Info |= OBJECT_NOT_VISIBLE;
			Connection->DeleteNr = (int16) LastActionNr;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->Layer < 32)
		        && (ObjectLine->NetNr == NetNr))
		{
			ZeroUnusedObjects(0);
			ObjectLine->Info |= OBJECT_NOT_VISIBLE;
			ObjectLine->DeleteNr = (int16) LastActionNr;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0)
		        && ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)) && (ObjectArc->NetNr == NetNr))
		{
			ZeroUnusedObjects(0);
			ObjectArc->Info |= OBJECT_NOT_VISIBLE;
			ObjectArc->DeleteNr = (int16) LastActionNr;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteTracesViasNetSelectedTrace()
{
	int32 NetNr;

	NetNr = GetNetNrSelectedTrace();

	if ((NetNr >= 0) && (NetNr < Design.NrNets))
	{
		DeleteNet((int32) NetNr);
		ReCalcConnectionsNet((int32) NetNr, 0, 1);
		RePaint();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNetNrSelectedTrace()
{
	int32 Layer, cnt;
	TraceRecord *Trace;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				return Trace->NetNr;
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				return Trace->NetNr;
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				return Trace->NetNr;
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				return Trace->NetNr;
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeleteTracesVias()
{
	int32 Layer, cnt;
	TraceRecord *Trace;
	ViaRecord *Via;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			Trace->Info |= OBJECT_NOT_VISIBLE;
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			Trace->Info |= OBJECT_NOT_VISIBLE;
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			Trace->Info |= OBJECT_NOT_VISIBLE;
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			Trace->Info |= OBJECT_NOT_VISIBLE;
		}

	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		Via->Info |= OBJECT_NOT_VISIBLE;
	}

	InvalidateRect(PCBWindow, NULL, 0);
	PostMessage(PCBWindow, WM_PAINT, (WPARAM) NULL, (LPARAM) NULL);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrObjectsAllNets()
{
	int32 Layer, NrNetObjects, cnt, cnt2, Count;

	TraceRecord *Trace;
	ViaRecord *Via;
	CompRecord *Comp;
	ConnectionsRecord *Connection;
	ObjectRecord *Object;
	AreaFillRecord *AreaFill;
	NetRecord *Net;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Count = 0;
		Net->Pos = 0;
	}

	NrNetObjects = 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					NrNetObjects++;
					Net = &((*Nets)[Trace->NetNr]);
					Net->Count++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					NrNetObjects++;
					Net = &((*Nets)[Trace->NetNr]);
					Net->Count++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					NrNetObjects++;
					Net = &((*Nets)[Trace->NetNr]);
					Net->Count++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					NrNetObjects++;
					Net = &((*Nets)[Trace->NetNr]);
					Net->Count++;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
			{
				NrNetObjects++;
				Net = &((*Nets)[Via->NetNr]);
				Net->Count++;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((Connection->NetNr >= 0) && (Connection->NetNr < Design.NrNets))
			{
				NrNetObjects++;
				Net = &((*Nets)[Connection->NetNr]);
				Net->Count++;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			NrObjects = 0;
//          if (cnt==530) {
//            ok=1;
//          }
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
				{
					NrNetObjects++;
					Net = &((*Nets)[Object->NetNr]);
					Net->Count++;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr >= 0)
		        && (AreaFill->NetNr < Design.NrNets))
		{
			NrNetObjects++;
			Net = &((*Nets)[AreaFill->NetNr]);
			Net->Count++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->Layer < 32) && (ObjectLine->NetNr >= 0)
		        && (ObjectLine->NetNr < Design.NrNets))
		{
			NrNetObjects++;
			Net = &((*Nets)[ObjectLine->NetNr]);
			Net->Count++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0)
		        && ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)) && (ObjectArc->NetNr >= 0)
		        && (ObjectArc->NetNr < Design.NrNets))
		{
			NrNetObjects++;
			Net = &((*Nets)[ObjectArc->NetNr]);
			Net->Count++;
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	return NrNetObjects;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyObjectsAllNetsToObjects4(int32 mode)
{
	int32 Layer, cnt, cnt2, Count;

	TraceRecord *Trace;
	ViaRecord *Via;
	CompRecord *Comp;
	ConnectionsRecord *Connection;
	ObjectRecord *Object, *ObjectNet, NewObject;
	AreaFillRecord *AreaFill;
	uint8 *AreaPos;
	PolygonRecord *DrawPolygon;
	NetRecord *Net;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;

	memset(&NewObject, 0, sizeof(NewObject));

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Trace->NetNr]);
					ObjectNet = &((*Objects4)[Net->Pos]);
					ObjectNet->x1 = Trace->X;
					ObjectNet->y1 = Trace->Y;
					ObjectNet->x2 = Trace->Length;
					ObjectNet->y2 = Trace->ThickNess;
					ObjectNet->Clearance = Trace->Clearance;
					ObjectNet->ObjectType = TRACE_VER;
					ObjectNet->Info = Trace->Info;
					ObjectNet->Layer = Layer;
					ObjectNet->CompNr = -1;
					ObjectNet->TraceNr = cnt;
					ObjectNet->NetNr = Trace->NetNr;
					FillPositionObject(ObjectNet);
					Net->Pos++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Trace->NetNr]);
					ObjectNet = &((*Objects4)[Net->Pos]);
					ObjectNet->x1 = Trace->X;
					ObjectNet->y1 = Trace->Y;
					ObjectNet->x2 = Trace->Length;
					ObjectNet->y2 = Trace->ThickNess;
					ObjectNet->Clearance = Trace->Clearance;
					ObjectNet->ObjectType = TRACE_HOR;
					ObjectNet->Info = Trace->Info;
					ObjectNet->Layer = Layer;
					ObjectNet->CompNr = -1;
					ObjectNet->TraceNr = cnt;
					ObjectNet->NetNr = Trace->NetNr;
					FillPositionObject(ObjectNet);
					Net->Pos++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Trace->NetNr]);
					ObjectNet = &((*Objects4)[Net->Pos]);
					ObjectNet->x1 = Trace->X;
					ObjectNet->y1 = Trace->Y;
					ObjectNet->x2 = Trace->Length;
					ObjectNet->y2 = Trace->ThickNess;
					ObjectNet->Clearance = Trace->Clearance;
					ObjectNet->ObjectType = TRACE_DIAG1;
					ObjectNet->Info = Trace->Info;
					ObjectNet->Layer = Layer;
					ObjectNet->CompNr = -1;
					ObjectNet->TraceNr = cnt;
					ObjectNet->NetNr = Trace->NetNr;
					FillPositionObject(ObjectNet);
					Net->Pos++;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Trace->NetNr]);
					ObjectNet = &((*Objects4)[Net->Pos]);
					ObjectNet->x1 = Trace->X;
					ObjectNet->y1 = Trace->Y;
					ObjectNet->x2 = Trace->Length;
					ObjectNet->y2 = Trace->ThickNess;
					ObjectNet->Clearance = Trace->Clearance;
					ObjectNet->ObjectType = TRACE_DIAG2;
					ObjectNet->Info = Trace->Info;
					ObjectNet->Layer = Layer;
					ObjectNet->CompNr = -1;
					ObjectNet->TraceNr = cnt;
					ObjectNet->NetNr = Trace->NetNr;
					FillPositionObject(ObjectNet);
					Net->Pos++;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Via->NetNr]);
				ObjectNet = &((*Objects4)[Net->Pos]);
				ObjectNet->x1 = Via->X;
				ObjectNet->y1 = Via->Y;
				ObjectNet->x2 = Via->ThickNess;
				ObjectNet->Clearance = Via->Clearance;
				ObjectNet->ObjectType = VIA_PUT_THROUGH_ROUND;
				ObjectNet->Info = Via->Info;
				ObjectNet->CompNr = -1;
				ObjectNet->TraceNr = cnt;
				ObjectNet->Layer = -1;
				ObjectNet->NetNr = Via->NetNr;
				FillPositionObject(ObjectNet);
				Net->Pos++;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((Connection->NetNr >= 0) && (Connection->NetNr < Design.NrNets))
			{
				if (mode == 0)
				{
					Net = &((*Nets)[Connection->NetNr]);
					ObjectNet = &((*Objects4)[Net->Pos]);
					ObjectNet->x1 = Connection->x1;
					ObjectNet->y1 = Connection->y1;
					ObjectNet->x2 = Connection->x2;
					ObjectNet->y2 = Connection->y2;
					ObjectNet->ObjectType = CONNECTION;
					ObjectNet->Info = Connection->Info;
					ObjectNet->TraceNr = cnt;
					ObjectNet->Layer = Connection->Layer;
					ObjectNet->NetNr = Connection->NetNr;
					Net->Pos++;
					FillPositionObject(ObjectNet);
				}
				else
					Connection->Extra1 = 0;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			NrObjects = 0;
//          if (cnt==530) {
//            ok=1;
//          }
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
				{
					Net = &((*Nets)[Object->NetNr]);
					ObjectNet = &((*Objects4)[Net->Pos]);
					memcpy(ObjectNet, Object, sizeof(ObjectRecord));
					FillPositionObject(ObjectNet);
					Net->Pos++;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr >= 0)
		        && (AreaFill->NetNr < Design.NrNets))
		{
			if (AreaFill->NetNr >= 0)
				Net = &((*Nets)[AreaFill->NetNr]);
			else
				Net = &EmptyNet;

			ObjectNet = &((*Objects4)[Net->Pos]);
			ObjectNet->ObjectType = AREAFILL;
			AreaPos = (uint8 *) AreaFill;
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
			ObjectNet->x1 = (*DrawPolygon).Points[0].x;
			ObjectNet->y1 = (*DrawPolygon).Points[0].y;
			ObjectNet->Info = AreaFill->Info;
			ObjectNet->TraceNr = cnt;
			ObjectNet->Layer = AreaFill->Layer;
			ObjectNet->NetNr = AreaFill->NetNr;
			FillPositionObject(ObjectNet);
			Net->Pos++;
		}
	}


	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->Layer < 32) && (ObjectLine->NetNr >= 0)
		        && (ObjectLine->NetNr < Design.NrNets))
		{
			NewObject.x1 = ObjectLine->X1;
			NewObject.y1 = ObjectLine->Y1;
			NewObject.x2 = ObjectLine->X2;
			NewObject.y2 = ObjectLine->Y2;
			NewObject.Info = 0;
			NewObject.Test = 0;
			NewObject.Clearance = ObjectLine->Clearance;
			NewObject.TraceNr = cnt;
			NewObject.NetNr = ObjectLine->NetNr;
			NewObject.Layer = ObjectLine->Layer;
			NewObject.ObjectType = TRACE_ALL_ANGLE;
			NewObject.Thickness = ObjectLine->LineThickNess;
			Net = &((*Nets)[ObjectLine->NetNr]);
			ObjectNet = &((*Objects4)[Net->Pos]);
			memcpy(ObjectNet, &NewObject, sizeof(ObjectRecord));
			FillPositionObject(ObjectNet);
			Net->Pos++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0) && (ObjectArc->Layer < 32)
		        && (ObjectArc->NetNr >= 0) && (ObjectArc->NetNr < Design.NrNets))
		{
			NewObject.x1 = ObjectArc->CentreX;
			NewObject.y1 = ObjectArc->CentreY;
			NewObject.x2 = ObjectArc->Width;
			NewObject.y2 = ObjectArc->Height;
			NewObject.x3 = ObjectArc->StartDiffX;
			NewObject.y3 = ObjectArc->StartDiffY;
			NewObject.x4 = ObjectArc->EndDiffX;
			NewObject.y4 = ObjectArc->EndDiffY;
			NewObject.Info = 0;
			NewObject.Test = 0;
			NewObject.Layer = ObjectArc->Layer;
			NewObject.ObjectType = TRACE_ARC;
			NewObject.TraceNr = cnt;
			NewObject.NetNr = ObjectArc->NetNr;
			NewObject.Clearance = ObjectArc->Clearance;
			NewObject.Thickness = ObjectArc->LineThickNess;
			Net = &((*Nets)[ObjectArc->NetNr]);
			ObjectNet = &((*Objects4)[Net->Pos]);
			memcpy(ObjectNet, &NewObject, sizeof(ObjectRecord));
			FillPositionObject(ObjectNet);
			Net->Pos++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectArc->Layer == DRILL_LAYER)
		        && (ObjectArc->NetNr >= 0) && (ObjectArc->NetNr < Design.NrNets))
		{
			NewObject.x1 = ObjectArc->CentreX;
			NewObject.y1 = ObjectArc->CentreY;
			NewObject.x2 = ObjectArc->Width;
			NewObject.y2 = ObjectArc->Width;
			NewObject.Info = 0;
			NewObject.Test = 0;
			NewObject.Layer = -1;
			NewObject.ObjectType = VIA_PUT_THROUGH_ROUND;
			NewObject.TraceNr = cnt;
			NewObject.NetNr = ObjectArc->NetNr;
			NewObject.Clearance = ObjectArc->Clearance;
			NewObject.Thickness = 0.0;
			Net = &((*Nets)[ObjectArc->NetNr]);
			ObjectNet = &((*Objects4)[Net->Pos]);
			memcpy(ObjectNet, &NewObject, sizeof(ObjectRecord));
			FillPositionObject(ObjectNet);
			Net->Pos++;
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CheckConnectivity(int32 mode)
{
	NetRecord *Net;
	int32 res, cnt, count, NrNetObjects;
	int32 NetError = 0;
	ConnectionsRecord *Connection;
	char Filename[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 ok;
#endif

	strcpy(Filename, EditFile);
	CutExtensionFileName(Filename);
	strcat(Filename, ".nnn");
	DeleteFileUTF8(Filename);

	if (Design.NrComps > 0)
	{
		Design.NrConnections = 0;
		NrNetObjects = GetNrObjectsAllNets();
		AllocateMemObjects4(NrNetObjects);
		CopyObjectsAllNetsToObjects4(1);

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if (Net->Name[0] != 0)
			{
				Net->Dummy = 0;
				strcpy(InfoStr, Net->Name);
				RedrawInfoStr(1);

#ifdef _DEBUG

				if (stricmpOwn(Net->Name, "N$1008") == 0)
					ok = 1;

#endif
				res = CheckNet((int32) cnt, 4 + 2 + 1);
				CheckForEscape();

				if ((count = InsertConnections((int32) cnt, 0)) > 0)
				{
					Net->Dummy = 1;
					NetError = 1;
				}
			}
		}

		if (MaxNrObjects4 > 4096)
			DeAllocateMemObjects4();


		if (mode == 0)
		{
			for (cnt = 0; cnt < Design.NrConnections; cnt++)
			{
				Connection = &((*Connections)[cnt]);
				Net = &((*Nets)[Connection->NetNr]);

				if (((Connection->Info & OBJECT_NOT_VISIBLE) == 0) && ((Net->Info & CONNECTIONS_DISABLED) == 0)
				        && ((Net->Info & CONNECTIONS_NOT_VISIBLE) == 0))
					Connection->Info |= OBJECT_HIGHLITED;

				if (Net->Info & CONNECTIONS_NOT_VISIBLE)
					Connection->Info |= CONNECTIONS_NOT_VISIBLE;
				else
					Connection->Info &= ~CONNECTIONS_NOT_VISIBLE;
			}

			if (!NetError)
				MessageBoxOwn(PCBWindow, SC(876, "Connectivity ok"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
			else
			{
				ViewFull();
				UnconnectedNetsDialog();
			}
		}
		else
			ViewFull();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ShowFirstUnroutedNet(int32 mode)
{
	NetRecord *Net;
	int32 res, cnt, GuideMode, NewShortestConnectionIndex, NetNr;
	double minx, maxx, miny, maxy, x, y, x3, y3, MinLength, Length, MaxLength;
	ConnectionsRecord *Connection;
	ObjectRecord *Object, Object1, Object2, FirstObject;

	if (mode == 1)
	{
		ShortestConnectionLength = 0.0;
		ShortestConnectionIndex = -1;
		return 0;
	}

	memset(&FirstObject, 0, sizeof(FirstObject));

	if (!OkToDrawConnections)
	{
		Beep(1000, 200);
		MessageBoxOwn(PCBWindow, SC(877, "Guides are invisible"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	minx = 1000000000.0;
	miny = 1000000000.0;
	maxx = -1000000000.0;
	maxy = -1000000000.0;
	FirstObject.ObjectType = 0;
	NewShortestConnectionIndex = -1;
	MaxLength = ShortestConnectionLength;
	MinLength = 1e9;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		NetNr = Connection->NetNr;
		Net = &((*Nets)[NetNr]);

		if (((Connection->Info & (OBJECT_NOT_VISIBLE | CONNECTIONS_NOT_VISIBLE)) == 0)
		        && ((Net->Info & (CONNECTIONS_NOT_VISIBLE | CONNECTIONS_DISABLED)) == 0))
		{
			Length = sqrt(SQR(Connection->x1 - Connection->x2) + SQR(Connection->y1 - Connection->y2));

			if ((Length < MinLength) && (Length > MaxLength))
			{
				MinLength = Length;
				FirstObject.ObjectType = CONNECTION;
				FirstObject.x1 = Connection->x1;
				FirstObject.y1 = Connection->y1;
				FirstObject.x2 = Connection->x2;
				FirstObject.y2 = Connection->y2;
				FirstObject.NetNr = NetNr;
				NewShortestConnectionIndex = cnt;
			}
		}
	}

	if (FirstObject.ObjectType == 0)
	{
		if (ShortestConnectionIndex != -1)
		{
			// Start again from the beginning
			ShortestConnectionLength = 0.0;
			ShortestConnectionIndex = -1;
			MaxLength = ShortestConnectionLength;
			NewShortestConnectionIndex = -1;
			MinLength = 1e9;

			for (cnt = 0; cnt < Design.NrConnections; cnt++)
			{
				Connection = &((*Connections)[cnt]);
				NetNr = Connection->NetNr;
				Net = &((*Nets)[NetNr]);

				if (((Connection->Info & (OBJECT_NOT_VISIBLE | CONNECTIONS_NOT_VISIBLE)) == 0)
				        && ((Net->Info & (CONNECTIONS_NOT_VISIBLE | CONNECTIONS_DISABLED)) == 0))
				{
					Length = sqrt(SQR(Connection->x1 - Connection->x2) + SQR(Connection->y1 - Connection->y2));

					if ((Length < MinLength) && (Length > MaxLength))
					{
						MinLength = Length;
						FirstObject.ObjectType = CONNECTION;
						FirstObject.x1 = Connection->x1;
						FirstObject.y1 = Connection->y1;
						FirstObject.x2 = Connection->x2;
						FirstObject.y2 = Connection->y2;
						FirstObject.NetNr = NetNr;
						NewShortestConnectionIndex = cnt;
					}
				}
			}
		}
	}

	if (FirstObject.ObjectType == 0)
	{
		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);
			NetNr = Connection->NetNr;
			Net = &((*Nets)[NetNr]);

			if (((Connection->Info & OBJECT_NOT_VISIBLE) == 0) && ((Net->Info & CONNECTIONS_DISABLED) == 0))
			{
				Length = sqrt(SQR(Connection->x1 - Connection->x2) + SQR(Connection->y1 - Connection->y2));

				if ((Length < MinLength) && (Length > MaxLength))
				{
					MinLength = Length;
					FirstObject.ObjectType = CONNECTION;
					FirstObject.x1 = Connection->x1;
					FirstObject.y1 = Connection->y1;
					FirstObject.x2 = Connection->x2;
					FirstObject.y2 = Connection->y2;
					FirstObject.NetNr = NetNr;
				}
			}
		}

		if (FirstObject.ObjectType != 0)
		{
			Beep(1000, 200);
			MessageBoxOwn(PCBWindow, SC(878, "Some connections are hided"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
			return -1;
		}
	}

	if (FirstObject.ObjectType == 0)
	{
		Beep(1000, 200);
		MessageBoxOwn(PCBWindow, SC(879, "Routing complete"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	ShortestConnectionIndex = NewShortestConnectionIndex;
	ShortestConnectionLength = MinLength + 100.0;
	x = FirstObject.x1;
	y = FirstObject.y1;
	GuideMode = 0;

	if ((x < Design.BoardOriginX) || (x > Design.BoardOriginX + Design.BoardWidth) || (y < Design.BoardOriginY)
	        || (y > Design.BoardOriginY + Design.BoardHeight))
		GuideMode = 2;

	x = FirstObject.x2;
	y = FirstObject.y2;

	if ((x < Design.BoardOriginX) || (x > Design.BoardOriginX + Design.BoardWidth) || (y < Design.BoardOriginY)
	        || (y > Design.BoardOriginY + Design.BoardHeight))
		GuideMode = 1;

	Object1.ObjectType = 0;
	Object2.ObjectType = 0;

	if (GuideMode == 0)
	{
		NetNr = FirstObject.NetNr;
		Net = &((*Nets)[NetNr]);
		GetObjectsNet(NetNr, 3, 0);
		res = GetObjectNrFromEndPoint(-1, FirstObject.x1, FirstObject.y1, 3);

		if (res != -1)
		{
			Object = &((*Objects3)[res]);
			memmove(&Object1, Object, sizeof(ObjectRecord));
		}

		res = GetObjectNrFromEndPoint(-1, FirstObject.x2, FirstObject.y2, 3);

		if (res != -1)
		{
			Object = &((*Objects3)[res]);
			memmove(&Object2, Object, sizeof(ObjectRecord));
		}

		switch (Object1.ObjectType)
		{
		case PIN_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
		case PIN_PUT_THROUGH_POLYGON:
			GuideMode = 1;
			break;

		case VIA_PUT_THROUGH_ROUND:
			switch (Object2.ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				GuideMode = 2;
				break;

			default:
				GuideMode = 1;
				break;
			}

			break;

		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
		case PIN_SMD_POLYGON:
			switch (Object2.ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_POLYGON:
				GuideMode = 2;
				break;

			default:
				GuideMode = 1;
				break;
			}

			break;

		default:
			switch (Object2.ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case VIA_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_POLYGON:
				GuideMode = 2;
				break;

			default:
				GuideMode = 1;
				break;
			}

			break;
		}
	}

	/*
	      case PIN_PUT_THROUGH_ROUND:
	      case VIA_PUT_THROUGH_ROUND:
	      case PIN_PUT_THROUGH_SQUARE:
	      case PIN_SMD_ROUND:
	      case PIN_SMD_RECT:
	      case TRACE_HOR:
	      case PIN_LINE_HOR:
	      case TRACE_VER:
	      case PIN_LINE_VER:
	      case TRACE_DIAG1:
	      case PIN_LINE_DIAG1:
	      case TRACE_DIAG2:
	      case PIN_LINE_DIAG2:
	*/
	Factor = 0.0005;

	if (GuideMode == 2)
	{
		x3 = FirstObject.x2 + (FirstObject.x1 - FirstObject.x2) * 0.25;
		y3 = FirstObject.y2 + (FirstObject.y1 - FirstObject.y2) * 0.25;
		CenterScreen(FirstObject.x2, FirstObject.y2);
		CheckInputMessages(0);
		CheckInputMessages(0);
	}
	else
	{
		x3 = FirstObject.x1 + (FirstObject.x2 - FirstObject.x1) * 0.25;
		y3 = FirstObject.y1 + (FirstObject.y2 - FirstObject.y1) * 0.25;
		CenterScreen(FirstObject.x1, FirstObject.y1);
		CheckInputMessages(0);
		CheckInputMessages(0);
	}

	SelectObjectOnPointer(x3, y3, x3, y3, 0);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ConnectivityTest()
{
	ObjectRecord NewObject1, NewObject2;
	double CurrentX, CurrentY;
	int32 ok, res;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

#if 0

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			NrObjects = 0;

#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
				ok = 1;

#endif

			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 1);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
				FillPositionObject(Object);

				if ((Object->minx < CurrentX) && (Object->maxx > CurrentX) && (Object->miny < CurrentY)
				        && (Object->maxy > CurrentY))
					PrintObjectInText(Object);
			}
		}
	}

#endif

	NewObject1.ObjectType = 768;
	NewObject1.Test = -1;
	NewObject1.Layer = 0;
	NewObject1.x1 = 19887976.000000;
	NewObject1.y1 = 1725145.000000;
	NewObject1.x2 = 241592.000000;
	NewObject1.y2 = 20320.000000;
	NewObject1.x3 = 0.000000;
	NewObject1.y3 = 0.000000;
	NewObject1.x4 = 0.000000;
	NewObject1.y4 = 0.000000;
	NewObject1.Clearance = 20320.000000;
	NewObject1.minx = 19877816.000000;
	NewObject1.miny = 1473393.000000;
	NewObject1.maxx = 20139728.000000;
	NewObject1.maxy = 1735305.000000;
	NewObject1.Thickness = 0.000000;
	NewObject1.Address = 0;
	NewObject1.ObjectType2 = 0;
	NewObject1.RotationAngle = 0.000000;
	NewObject1.Mirror = 0;
	NewObject1.PinCount = 0;
	NewObject1.TraceNr = 127;
	NewObject1.Info = 3;
	NewObject1.Info2 = 0;
	NewObject1.NetNr = 46;
	NewObject1.CompNr = -1;
	NewObject1.PinNr = 0;
	NewObject1.CompNr2 = 0;

	NewObject2.ObjectType = 3520;
	NewObject2.Test = -1;
	NewObject2.Layer = 0;
	NewObject2.x1 = 20129568.000000;
	NewObject2.y1 = 1483553.000000;
	NewObject2.x2 = 19764700.000000;
	NewObject2.y2 = 1395871.750000;
	NewObject2.x3 = 142240.000000;
	NewObject2.y3 = 114947.281250;
	NewObject2.x4 = -114947.281250;
	NewObject2.y4 = 114947.281250;
	NewObject2.Clearance = 20320.000000;
	NewObject2.minx = 19757080.000000;
	NewObject2.miny = 1388251.750000;
	NewObject2.maxx = 20137188.000000;
	NewObject2.maxy = 1491173.000000;
	NewObject2.Thickness = 15240.000000;
	NewObject2.Address = 0;
	NewObject2.ObjectType2 = 0;
	NewObject2.RotationAngle = 45.000000;
	NewObject2.Mirror = 0;
	NewObject2.PinCount = 0;
	NewObject2.TraceNr = 32606780;
	NewObject2.Info = 0;
	NewObject2.Info2 = 0;
	NewObject2.NetNr = 46;
	NewObject2.CompNr = 0;
	NewObject2.PinNr = 12;
	NewObject2.CompNr2 = 0;

	res = ObjectsConnected(&NewObject1, &NewObject2);
	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeNetType(int32 NetNr, double TraceWidth, double Clearance, int32 mode)
{
	int32 Layer, cnt;
	NetRecord *Net;
	ViaRecord *Via;
	AreaFillRecord *AreaFill;
	TraceRecord *Trace;

	if ((NetNr < 0) || (NetNr >= Design.NrNets))
		return -1;

	Net = &((*Nets)[NetNr]);

	Net->TraceWidth = (float) TraceWidth;
	Net->TraceClearance = (float) Clearance;

	if (mode == 1)
		return 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr))
			{
				Trace->ThickNess = (float) TraceWidth;
				Trace->Clearance = (float) Clearance;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr))
			{
				Trace->ThickNess = (float) TraceWidth;
				Trace->Clearance = (float) Clearance;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr))
			{
				Trace->ThickNess = (float) TraceWidth;
				Trace->Clearance = (float) Clearance;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->NetNr == NetNr))
			{
				Trace->ThickNess = (float) TraceWidth;
				Trace->Clearance = (float) Clearance;
			}
		}
	}

// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if (((Via->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Via->NetNr == NetNr))
			Via->Clearance = (float) Clearance;
	}

// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->NetNr == NetNr))
			AreaFill->Clearance = (float) Clearance;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeDesignRulesPcb(int32 mode)
{
	char str[200];
	int32 Layer, cnt;
	NetRecord *Net;
	ViaRecord *Via;
	AreaFillRecord *AreaFill;
	TraceRecord *Trace;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;

	if ((NotInRange(Design.StandardClearance, NewDesign.StandardClearance))
	        || (NotInRange(Design.StandardTraceWidth, NewDesign.StandardTraceWidth))
	        || (NotInRange(Design.SilkScreenWidth, NewDesign.SilkScreenWidth))
	        || (NotInRange(Design.BoardOutlineWidth, NewDesign.BoardOutlineWidth))
	        || (NotInRange(Design.BoardOutlineKeepOut, NewDesign.BoardOutlineKeepOut)))
	{
// *******************************************************************************************************
// *******************************************************************************************************
		if (NotInRange(Design.StandardClearance, NewDesign.StandardClearance))
		{
			if (Units == 0)
			{
				sprintf(str, "Do you want to change the clearance of all the traces/vias/areafills and nets to %.1f thou\r\n\r\n",
				        NewDesign.StandardClearance / 2540.0);
			}
			else
			{
				sprintf(str,
				        SC(781,
				           "Do you want to change the clearance of all the traces/vias/areafills and nets to %.4f mm \r\n\r\n"),
				        NewDesign.StandardClearance / 100000.0);
			}

			strcat(str, SC(179, "This operation can not be undone"));
			Design.StandardClearance = NewDesign.StandardClearance;
			Design.MaximumClearance = NewDesign.StandardClearance;

			if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDYES)
			{
				ok = 1;
				DataBaseChanged = 1;

				for (cnt = 0; cnt < Design.NrNets; cnt++)
				{
					Net = &((*Nets)[cnt]);
					Net->TraceClearance = NewDesign.StandardClearance;
				}

				for (Layer = 0; Layer < 32; Layer++)
				{
					for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
					{
						Trace = &((*VerTraces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->Clearance = NewDesign.StandardClearance;
					}

					for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
					{
						Trace = &((*HorTraces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->Clearance = NewDesign.StandardClearance;
					}

					for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
					{
						Trace = &((*Diag1Traces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->Clearance = NewDesign.StandardClearance;
					}

					for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
					{
						Trace = &((*Diag2Traces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->Clearance = NewDesign.StandardClearance;
					}
				}

				for (cnt = 0; cnt < Design.NrVias; cnt++)
				{
					Via = &((*Vias)[cnt]);

					if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
						Via->Clearance = NewDesign.StandardClearance;
				}

				for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

					if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
						AreaFill->Clearance = NewDesign.StandardClearance;
				}

				for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
				{
					ObjectArc = &((*ObjectArcs)[cnt]);

					if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((ObjectArc->Layer == 0) || (ObjectArc->Layer == Design.NrBoardLayers - 1))
							ObjectArc->Clearance = NewDesign.StandardClearance;
					}
				}

				for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
				{
					ObjectLine = &((*ObjectLines)[cnt]);

					if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((ObjectLine->Layer == 0) || (ObjectLine->Layer == Design.NrBoardLayers - 1))
							ObjectLine->Clearance = NewDesign.StandardClearance;
					}
				}
			}
		}

// *******************************************************************************************************
// *******************************************************************************************************
		Design.BoardWidth = NewDesign.BoardWidth;
		Design.BoardHeight = NewDesign.BoardHeight;

// *******************************************************************************************************
// *******************************************************************************************************
		if (NotInRange(Design.SilkScreenWidth, NewDesign.SilkScreenWidth))
		{
			if (Units == 0)
			{
				sprintf(str, "Do you want to change the width of all silkscreen lines to %.1f thou\r\n\r\n",
				        NewDesign.SilkScreenWidth / 2540.0);
			}
			else
			{
				sprintf(str, SC(782, "Do you want to change the width of all silkscreen lines to %.4f mm\r\n\r\n"),
				        NewDesign.SilkScreenWidth / 100000.0);
			}

			strcat(str, SC(179, "This operation can not be undone"));
			Design.SilkScreenWidth = NewDesign.SilkScreenWidth;
			DataBaseChanged = 1;

			if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDYES)
			{
				for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
				{
					ObjectLine = &((*ObjectLines)[cnt]);

					if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
					        && ((ObjectLine->Layer == SILKSCREEN_BOTTOM) || (ObjectLine->Layer == SILKSCREEN_TOP)))
						ObjectLine->LineThickNess = NewDesign.SilkScreenWidth;
				}

				for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
				{
					ObjectRect = &((*ObjectRects)[cnt]);

					if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0)
					        && ((ObjectRect->Layer == SILKSCREEN_BOTTOM) || (ObjectRect->Layer == SILKSCREEN_TOP)))
						ObjectRect->LineThickNess = NewDesign.SilkScreenWidth;
				}

				for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
				{
					ObjectArc = &((*ObjectArcs)[cnt]);

					if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0)
					        && ((ObjectArc->Layer == SILKSCREEN_BOTTOM) || (ObjectArc->Layer == SILKSCREEN_TOP)))
						ObjectArc->LineThickNess = NewDesign.SilkScreenWidth;
				}

				for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
				{
					ObjectText2 = &((*ObjectTexts2)[cnt]);

					if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
					        && ((ObjectText2->Layer == SILKSCREEN_BOTTOM) || (ObjectText2->Layer == SILKSCREEN_TOP)))
						ObjectText2->LineThickNess = NewDesign.SilkScreenWidth;
				}
			}
		}

// *******************************************************************************************************
// *******************************************************************************************************
		if (NotInRange(Design.BoardOutlineWidth, NewDesign.BoardOutlineWidth))
		{
			if (Units == 0)
			{
				sprintf(str, "Do you want to change the width of all board outline lines to %.1f thou\r\n\r\n",
				        NewDesign.BoardOutlineWidth / 2540.0);
			}
			else
			{
				sprintf(str, SC(783, "Do you want to change the width of all board outline lines to %.4f mm\r\n\r\n"),
				        NewDesign.BoardOutlineWidth / 100000.0);
			}

			strcat(str, SC(179, "This operation can not be undone"));
			Design.BoardOutlineWidth = NewDesign.BoardOutlineWidth;
			DataBaseChanged = 1;

			if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDYES)
			{

				for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
				{
					ObjectLine = &((*ObjectLines)[cnt]);

					if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->Layer == BOARD_OUTLINE_LAYER))
						ObjectLine->LineThickNess = NewDesign.BoardOutlineWidth;
				}

				for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
				{
					ObjectRect = &((*ObjectRects)[cnt]);

					if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0)
					        && (ObjectRect->Layer == BOARD_OUTLINE_LAYER))
						ObjectRect->LineThickNess = NewDesign.BoardOutlineWidth;
				}

				for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
				{
					ObjectArc = &((*ObjectArcs)[cnt]);

					if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0)
					        && (ObjectArc->Layer == BOARD_OUTLINE_LAYER))
						ObjectArc->LineThickNess = NewDesign.BoardOutlineWidth;
				}
			}
		}

// *******************************************************************************************************
// *******************************************************************************************************
		if (NotInRange(Design.BoardOutlineKeepOut, NewDesign.BoardOutlineKeepOut))
		{
			Design.BoardOutlineKeepOut = NewDesign.BoardOutlineKeepOut;
			DataBaseChanged = 1;
		}

// *******************************************************************************************************
// *******************************************************************************************************
		if (NotInRange(Design.StandardTraceWidth, NewDesign.StandardTraceWidth))
		{
			if (Units == 0)
			{
				sprintf(str, "Do you want to change the width of all the traces and nets to %.1f thou\r\n\r\n",
				        NewDesign.StandardTraceWidth / 2540.0);
			}
			else
			{
				sprintf(str, SC(784, "Do you want to change the width of all the traces and nets to %.4f mm\r\n\r\n"),
				        NewDesign.StandardTraceWidth / 100000.0);
			}

			strcat(str, SC(179, "This operation can not be undone"));
			DataBaseChanged = 1;
			Design.StandardTraceWidth = NewDesign.StandardTraceWidth;

			if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDYES)
			{
				ok = 1;

				for (cnt = 0; cnt < Design.NrNets; cnt++)
				{
					Net = &((*Nets)[cnt]);
					Net->TraceWidth = NewDesign.StandardTraceWidth;
				}

				for (Layer = 0; Layer < 32; Layer++)
				{
					for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
					{
						Trace = &((*VerTraces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->ThickNess = NewDesign.StandardTraceWidth;
					}

					for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
					{
						Trace = &((*HorTraces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->ThickNess = NewDesign.StandardTraceWidth;
					}

					for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
					{
						Trace = &((*Diag1Traces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->ThickNess = NewDesign.StandardTraceWidth;
					}

					for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
					{
						Trace = &((*Diag2Traces[Layer])[cnt]);

						if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
							Trace->ThickNess = NewDesign.StandardTraceWidth;
					}
				}
			}
		}

		RePaint();
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************