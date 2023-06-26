/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: select.c
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
#include "select.h"
#include "calcdef.h"
#include "graphics.h"
#include "toets.h"
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
#include "rect.h"
#include "string.h"
#include "mainloop.h"
#include "trace3.h"
#include "dialogs.h"
#include "gateswap.h"
#include "move3.h"

int32 SortLayer, ok;
double Length;

extern HDC OutputDisplay;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectObjectsFromWindow(double x1, double y1, double x2, double y2, int32 mode)
{
	int32 hulpx, hulpy;

	if (OkToDrawCrossHair == 1)
		DrawCrossHair(1);

	StartDrawingEditingWindow(BM_DoubleBuffer);


	if ((mode & 1) == 1)
	{
		x1 = PixelToRealOffX(MousePosX);
		y1 = PixelToRealOffY(DrawWindowMaxY - MousePosY);
		x2 = x1;
		y2 = y1;
	}

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
	{
		hulpx = MultX(x1);
		hulpy = MultY(y1);
		SearchMinX = PixelToRealOffX(hulpx - 2);
		SearchMinY = PixelToRealOffY(DrawWindowMaxY - (hulpy + 2) - 1);
		SearchMaxX = PixelToRealOffX(hulpx + 2);
		SearchMaxY = PixelToRealOffY(DrawWindowMaxY - (hulpy - 2) - 1);
		mode = 0;
	}
	else
	{
		SearchMinX = min(x1, x2);
		SearchMinY = min(y1, y2);
		SearchMaxX = max(x1, x2);
		SearchMaxY = max(y1, y2);
	}

//  SetROP2(OutputDisplay,R2_COPYPEN);

	switch (SelectionMode)
	{
	case DRAG_TRACES_VIAS_COMPS_MODE:
		SelectCompsFromRectWindow(0);
		SelectTracesFromRectWindow();

		if (OkToDrawVias)
			SelectViasFromRectWindow();

		SelectConnectionsFromRectWindow();

		if (OkToDrawAreaFills)
			SelectAreaFillsFromRectWindow(0);

		SelectObjectLinesFromRectWindow(1);
		SelectObjectArcsFromRectWindow(1);

		if (UnselectAll)
		{
			strcpy(InfoStr, SC(269, "Drag traces/vias/components")); //spodni pravy preklad
			RedrawInfoStr(0);
		}

		break;

	case ROUTING_MODE:			// Routing
		SelectTracesFromRectWindow();
//      if (UnselectAll) {
//        strcpy(InfoStr, SC(138, "Routing traces")); //spodni pravy preklad
//        RedrawInfoStr(0);
//      }
		break;

	case MOVE_ONE_TRACE_MODE:	// Moving one trace
		SelectTracesFromRectWindow();
//      if (UnselectAll) {
//        strcpy(InfoStr, SC(135, "Drag traces")); //spodni pravy preklad
//        RedrawInfoStr(0);
//      }
		break;

	case MOVE_COMPONENTS_MODE:
		SelectCompsFromRectWindow((mode & 2) >> 1);
		LastAction = MOVE_COMPONENTS_MODE;
		break;

	case MOVE_COMPONENT_REFERENCES_MODE:
		if (OkToDrawCompReference)
		{
			SelectReferencesFromRectWindow(0);
			LastAction = MOVE_COMPONENT_REFERENCES_MODE;

			if (UnselectAll)
			{
				strcpy(InfoStr, SC(241, "Modify component references")); //spodni pravy preklad
				RedrawInfoStr(0);
			}
		}

		break;

	case MOVE_COMPONENT_VALUES_MODE:
		if (OkToDrawCompValue)
		{
			SelectCompValuesFromRectWindow(0);
			LastAction = MOVE_COMPONENT_VALUES_MODE;

			if (UnselectAll)
			{
				strcpy(InfoStr, SC(268, "Modify component values")); //spodni pravy preklad
				RedrawInfoStr(0);
			}
		}

		break;

	case OBJECTS_MODE:
		SelectObjectLinesFromRectWindow(0);
		SelectObjectRectsFromRectWindow(0);
		/*
		      SelectObjectCirclesFromRectWindow(0);
		*/
		SelectObjectArcsFromRectWindow(0);
		SelectObjectTexts2FromRectWindow(0);
		SelectObjectPolygonsFromRectWindow(0);
		LastAction = OBJECTS_MODE;
		break;

	case AREAFILLS_MODE:
		if (OkToDrawAreaFills)
			SelectAreaFillsFromRectWindow(0);

		if (UnselectAll)
		{
			strcpy(InfoStr, SC(176, "Areafills/powerplanes")); //spodni pravy preklad
			RedrawInfoStr(0);
		}

		break;

	case MOVING_TRACES_VIAS_MODE:
		SelectTracesFromRectWindow();
		SelectConnectionsFromRectWindow();

		if (OkToDrawVias)
			SelectViasFromRectWindow();

		SelectObjectLinesFromRectWindow(1);
		SelectObjectArcsFromRectWindow(1);
		break;

	case GATE_PINSWAP_MODE:
		if (!UnselectAll)
			SelectGatePinSwap(0);

		break;
	}

	ExitDrawing();
	EndDrawingEditingWindow(BM_DoubleBuffer);

	if (OkToDrawCrossHair == 1)
		DrawCrossHair(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectCompsFromRectWindow(int32 mode)
{
	int32 cnt, Mask, CompInfo, OldInfo, NrComponentsSelected, SelectionOk, Selected;
	double x1a, x2a, y1a, y2a;
	CompRecord *Comp, *FirstComps[10], *FirstProtectedComp;
	int32 ObjectChanged;
	ConnectionsRecord *Connection;
	char str[MAX_LENGTH_STRING];

//  strcpy(InfoStr,"              ");
//  RedrawInfoStr(0);

	NrComponentsSelected = 0;

	if (UnselectAll)
	{
		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);

			if ((Connection->Info & OBJECT_NOT_VISIBLE) == 0)
				Connection->Info &= ~(OBJECT_SELECTED | 0x003f);
		}
	}

	Mask = 0;

	if ((SelectionMode == MOVE_COMPONENTS_MODE) || (SelectionMode == DRAG_TRACES_VIAS_COMPS_MODE))
		Mask = COMPONENT_PROTECTED;

	memset(FirstComps, 0, sizeof(FirstComps));
	FirstProtectedComp = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;
		ObjectChanged = 0;
		OldInfo = CompInfo;
		SelectionOk = 1;

		if (CompInfo & OBJECT_NOT_VISIBLE)
			SelectionOk = 0;
		else
		{
			if ((Comp->CompMode & 8) == 0)
			{
				// Top components
				if (!DrawTopComponents)
					SelectionOk = 0;

				if (ViewSingleLayer)
				{
					if (CheckIfInnerLayer(CurrentDrawingLayer))
						SelectionOk = 0;
				}
			}
			else
			{
				// Bottom components
				if (!DrawBottomComponents)
					SelectionOk = 0;

				if (ViewSingleLayer)
				{
					if (CheckIfInnerLayer(CurrentDrawingLayer))
						SelectionOk = 0;
				}
			}
		}

		if (SelectionOk)
		{
			if (UnselectAll)
			{
				if ((CompInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					CompsSelected--;
					CompInfo &= ~(OBJECT_SELECTED | 0x10);
					ObjectChanged = 1;
				}
			}
			else
			{
#ifdef _DEBUG

				if (stricmp(Comp->Name, "T4") == 0)
					ok = 1;

#endif

				if ((CompInfo & (Mask)) == 0)
				{
					/*
					          x1a=Comp->BoardPosMinX;
					          y1a=Comp->BoardPosMinY;
					          x2a=Comp->BoardPosMaxX;
					          y2a=Comp->BoardPosMaxY;
					*/
					x1a = Comp->PlacementOriginX - Comp->PlacementWidth * 0.5;
					y1a = Comp->PlacementOriginY - Comp->PlacementHeight * 0.5;
					x2a = Comp->PlacementOriginX + Comp->PlacementWidth * 0.5;
					y2a = Comp->PlacementOriginY + Comp->PlacementHeight * 0.5;

					if ((SearchMaxX > x1a) && (SearchMinX < x2a) && (SearchMaxY > y1a) && (SearchMinY < y2a))
					{
						if (!ReplaceSelections)
						{
							if ((CompInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							{
								if (ShiftPressed)
								{
									CompInfo &= ~OBJECT_SELECTED;
									CompInfo &= ~(0x10);
									ObjectChanged = 1;
								}
							}
							else
							{
								CompInfo |= (OBJECT_SELECTED);
								NrComponentsSelected++;
								CompInfo &= ~(0x10);
								ObjectChanged = 1;
							}
						}
						else
						{
							if (ShiftPressed)
							{
								if ((CompInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								{
									if ((NrComponentsSelected == 1) || (mode == 0))
									{
										CompInfo ^= OBJECT_SELECTED;
										NrComponentsSelected--;
										CompInfo &= ~(0x10);
										ObjectChanged = 1;
									}
								}
								else
								{
									if ((NrComponentsSelected == 0) || (mode == 0))
									{
										CompInfo ^= OBJECT_SELECTED;
										NrComponentsSelected++;
										CompInfo &= ~(0x10);
										ObjectChanged = 1;
									}
								}
							}
							else
							{
								if ((NrComponentsSelected == 0) || (mode == 0))
								{
									CompInfo |= OBJECT_SELECTED;
									NrComponentsSelected++;
									CompInfo &= ~(0x10);
									ObjectChanged = 1;
								}
							}
						}
					}
				}
			}

			if (ObjectChanged)
			{
				Comp->Info = (int16) CompInfo;

				if ((OldInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					if ((Comp->Info & OBJECT_SELECTED) == 0)
					{
						Comp->TextVisibility &= ~(1 + 16);
						SetBackGroundActive(0);
						DrawComp2(Comp, 0.0, 0.0, 0, 0x201);
					}
				}
				else
					Comp->TextVisibility |= (1 + 16);

				DrawComp2(Comp, 0.0, 0.0, 0, 0x200);
			}

			if ((!UnselectAll) && ((CompInfo & (Mask)) == Mask))
			{
				if ((CompInfo & OBJECT_SELECTED) == 0)
				{
					x1a = Comp->BoardPosMinX;
					y1a = Comp->BoardPosMinY;
					x2a = Comp->BoardPosMaxX;
					y2a = Comp->BoardPosMaxY;

					if ((SearchMaxX > x1a) && (SearchMinX < x2a) && (SearchMaxY > y1a) && (SearchMinY < y2a))
					{
						if (!FirstProtectedComp)
							FirstProtectedComp = Comp;
					}
				}
			}
		}
	}

	Selected = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->Info & OBJECT_SELECTED)
		{
			if (Selected < 10)
				FirstComps[Selected] = Comp;

			Selected++;
		}
	}

	ok = 1;

	if ((FirstProtectedComp) && (Selected == 0))
	{
		sprintf(InfoStr, SC(336, "component %s ( value %s ) protected [ %d selected ]"), FirstProtectedComp->Name,
		        FirstProtectedComp->Value, Selected);
		RedrawInfoStr(0);
	}
	else
	{
		if (FirstComps[0])
		{
			if (Selected == 1)
			{
				sprintf(InfoStr, SC(337, "component %s ( value %s ) [ %d selected ]"), FirstComps[0]->Name, FirstComps[0]->Value,
				        Selected);
			}
			else
			{
				InfoStr[0] = 0;

				for (cnt = 0; cnt < min(10, Selected); cnt++)
				{
					if (cnt == 0)
						sprintf(str, SC(338, "components %s"), FirstComps[cnt]->Name);
					else
						sprintf(str, " , %s", FirstComps[cnt]->Name);

					strcat(InfoStr, str);
				}

				if (Selected > 10)
					strcat(InfoStr, " ........");

				sprintf(str, SC(338, "components %s"), FirstComps[cnt]->Name);
				strcat(InfoStr, str);
			}

			RedrawInfoStr(0);
		}
		else
		{
			strcpy(InfoStr, SC(269, "Move/rotate/change components"));
			RedrawInfoStr(0);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrCompsSelected()
{
	int32 cnt, count;
	CompRecord *Comp;

	count = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			count++;
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectTracesFromRectWindow()
{
	int32 cnt, TraceInfo, Layer;
	double x1, y1, x2, y2, dikte, lengte;
	int32 ObjectChanged;

	TraceRecord *Trace;

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
		{
			InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);
				TraceInfo = Trace->Info;
				ObjectChanged = 0;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					x2 = x1;
					y1 = Trace->Y;
					lengte = Trace->Length;
					y2 = y1 + lengte;
					dikte = Trace->ThickNess;

					if (UnselectAll)
					{
						if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							TraceInfo &= ~(OBJECT_SELECTED | 0x000f);
							ObjectChanged = 1;
						}
					}
					else
					{
						if ((RectTestRect2(x1, y1 + (lengte / 2), dikte, lengte))
						        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
						{
							if (!ReplaceSelections)
							{
								if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								{
									if (ShiftPressed)
										TraceInfo &= ~OBJECT_SELECTED;
								}
								else
									TraceInfo |= (OBJECT_SELECTED);
							}
							else
							{
								if (ShiftPressed)
									TraceInfo ^= OBJECT_SELECTED;
								else
									TraceInfo |= OBJECT_SELECTED;
							}

							TraceInfo &= ~0x000f;

							if ((SearchMaxX > x1) && (SearchMinX < x1) && (SearchMaxY > y1) && (SearchMinY < y1))
								TraceInfo |= 0x0001;

							if ((SearchMaxX > x2) && (SearchMinX < x2) && (SearchMaxY > y2) && (SearchMinY < y2))
								TraceInfo |= 0x0002;

							if ((TraceInfo & (OBJECT_SELECTED | 3)) == OBJECT_SELECTED)
								TraceInfo |= 3;

							ObjectChanged = 1;
						}
					}
				}

				if (ObjectChanged)
				{
					Trace->Info = (int16) TraceInfo;

					if ((SelectionMode != MOVE_ONE_TRACE_MODE) || (RepeatMode == 0)
					        || ((TraceInfo & OBJECT_SELECTED) == 0))
						DrawVerTrace(Trace);
				}
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);
				TraceInfo = Trace->Info;
				ObjectChanged = 0;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					lengte = Trace->Length;
					x2 = x1 + lengte;
					y2 = y1;
					dikte = Trace->ThickNess;

					if (UnselectAll)
					{
						if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							TraceInfo &= ~(OBJECT_SELECTED | 0x000f);
							ObjectChanged = 1;
						}
					}
					else
					{
						if ((RectTestRect2(x1 + (lengte / 2), y1, lengte, dikte))
						        || (RectTestCircle(x1, y1, dikte, 255)) || (RectTestCircle(x2, y2, dikte, 255)))
						{
							if (!ReplaceSelections)
							{
								if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								{
									if (ShiftPressed)
										TraceInfo &= ~OBJECT_SELECTED;
								}
								else
									TraceInfo |= (OBJECT_SELECTED);
							}
							else
							{
								if (ShiftPressed)
									TraceInfo ^= OBJECT_SELECTED;
								else
									TraceInfo |= OBJECT_SELECTED;
							}

							TraceInfo &= ~0x000f;

							if ((SearchMaxX > x1) && (SearchMinX < x1) && (SearchMaxY > y1) && (SearchMinY < y1))
								TraceInfo |= 0x0001;

							if ((SearchMaxX > x2) && (SearchMinX < x2) && (SearchMaxY > y2) && (SearchMinY < y2))
								TraceInfo |= 0x0002;

							if ((TraceInfo & (OBJECT_SELECTED | 3)) == OBJECT_SELECTED)
								TraceInfo |= 3;

							ObjectChanged = 1;
						}
					}
				}

				if (ObjectChanged)
				{
					Trace->Info = (int16) TraceInfo;

					if ((SelectionMode != MOVE_ONE_TRACE_MODE) || (RepeatMode == 0)
					        || ((TraceInfo & OBJECT_SELECTED) == 0))
						DrawHorTrace(Trace);
				}
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);
				TraceInfo = Trace->Info;
				ObjectChanged = 0;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					lengte = Trace->Length;
					x2 = x1 + lengte;
					y2 = y1 - lengte;
					dikte = Trace->ThickNess;

					if (UnselectAll)
					{
						if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							TraceInfo &= ~(OBJECT_SELECTED | 0x000f);
							ObjectChanged = 1;
						}
					}
					else
					{
						if ((RectTestDiag1(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
						        || (RectTestCircle(x2, y2, dikte, 255)))
						{
							if (!ReplaceSelections)
							{
								if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								{
									if (ShiftPressed)
										TraceInfo &= ~OBJECT_SELECTED;
								}
								else
									TraceInfo |= (OBJECT_SELECTED);
							}
							else
							{
								if (ShiftPressed)
									TraceInfo ^= OBJECT_SELECTED;
								else
									TraceInfo |= OBJECT_SELECTED;
							}

							TraceInfo &= ~0x000f;

							if ((SearchMaxX > x1) && (SearchMinX < x1) && (SearchMaxY > y1) && (SearchMinY < y1))
								TraceInfo |= 0x0001;

							if ((SearchMaxX > x2) && (SearchMinX < x2) && (SearchMaxY > y2) && (SearchMinY < y2))
								TraceInfo |= 0x0002;

							if ((TraceInfo & (OBJECT_SELECTED | 3)) == OBJECT_SELECTED)
								TraceInfo |= 3;

							ObjectChanged = 1;
						}
					}
				}

				if (ObjectChanged)
				{
					Trace->Info = (int16) TraceInfo;

					if ((SelectionMode != MOVE_ONE_TRACE_MODE) || (RepeatMode == 0)
					        || ((TraceInfo & OBJECT_SELECTED) == 0))
						DrawDiag1Trace(Trace);
				}
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);
				TraceInfo = Trace->Info;
				ObjectChanged = 0;

				if ((TraceInfo & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Trace->X;
					y1 = Trace->Y;
					lengte = Trace->Length;
					x2 = x1 + lengte;
					y2 = y1 + lengte;
					dikte = Trace->ThickNess;

					if (UnselectAll)
					{
						if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							TraceInfo &= ~(OBJECT_SELECTED | 0x000f);
							ObjectChanged = 1;
						}
					}
					else
					{
						if ((RectTestDiag2(x1, y1, lengte, dikte)) || (RectTestCircle(x1, y1, dikte, 255))
						        || (RectTestCircle(x2, y2, dikte, 255)))
						{
							if (!ReplaceSelections)
							{
								if ((TraceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								{
									if (ShiftPressed)
										TraceInfo &= ~OBJECT_SELECTED;
								}
								else
									TraceInfo |= (OBJECT_SELECTED);
							}
							else
							{
								if (ShiftPressed)
									TraceInfo ^= OBJECT_SELECTED;
								else
									TraceInfo |= OBJECT_SELECTED;
							}

							TraceInfo &= ~0x000f;

							if ((SearchMaxX > x1) && (SearchMinX < x1) && (SearchMaxY > y1) && (SearchMinY < y1))
								TraceInfo |= 0x0001;

							if ((SearchMaxX > x2) && (SearchMinX < x2) && (SearchMaxY > y2) && (SearchMinY < y2))
								TraceInfo |= 0x0002;

							if ((TraceInfo & (OBJECT_SELECTED | 3)) == OBJECT_SELECTED)
								TraceInfo |= 3;

							ObjectChanged = 1;
						}
					}
				}

				if (ObjectChanged)
				{
					Trace->Info = (int16) TraceInfo;

					if ((SelectionMode != MOVE_ONE_TRACE_MODE) || (RepeatMode == 0)
					        || ((TraceInfo & OBJECT_SELECTED) == 0))
						DrawDiag2Trace(Trace);
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrTracesSelected()
{
	int32 cnt, count, Layer;

	TraceRecord *Trace;

	count = 0;

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
		{
			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					count++;
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					count++;
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					count++;
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					count++;
			}
		}
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrViasSelected()
{
	int32 cnt, count, ViaInfo;
	ViaRecord *Via;

	count = 0;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			count++;
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectViasFromRectWindow()
{
	int32 cnt, ViaInfo;
	double x1, y1, ThickNess, Drill;
	int32 ObjectChanged;
	ViaRecord *Via;

	if (CurrentObjectCode != GraphicsObjectCodes[ViaPinsObjectNr])
		InitDrawingObject(VIA_PUT_THROUGH_ROUND, 0, 0, NORMAL_FILLED_AND_PEN1);

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;
		ObjectChanged = 0;

		if ((ViaInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Via->X;
			y1 = Via->Y;
			ThickNess = Via->ThickNess;
			Drill = Via->DrillThickNess;

			if (UnselectAll)
			{
				if ((ViaInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					ViaInfo &= ~(OBJECT_SELECTED | 1);
					ObjectChanged = 1;
				}
			}
			else
			{
				if (RectTestCircle(x1, y1, ThickNess, 255))
				{
					if (!ReplaceSelections)
					{
						if ((ViaInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							if (ShiftPressed)
								ViaInfo &= ~OBJECT_SELECTED;
						}
						else
							ViaInfo |= (OBJECT_SELECTED);
					}
					else
					{
						if (ShiftPressed)
							ViaInfo ^= OBJECT_SELECTED;
						else
							ViaInfo |= OBJECT_SELECTED;
					}

//          ViaInfo&=~0x0001;
					ObjectChanged = 1;
				}
			}
		}

		if (ObjectChanged)
		{
			Via->Info = (int16) ViaInfo;
			DrawVia(Via);
			DrawViaDrill(Via);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrConnectionsSelected()
{
	int32 cnt, count, ConnectionInfo;
	ConnectionsRecord *Connection;

	count = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			count++;
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectConnectionsFromRectWindow()
{
	int32 cnt, ConnectionInfo, TestResult;
	double x1, y1, x2, y2;
	int32 ObjectChanged;

	ConnectionsRecord *Connection;

	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Connection->x1;
			y1 = Connection->y1;
			x2 = Connection->x2;
			y2 = Connection->y2;
			ObjectChanged = 0;

			if (UnselectAll)
			{
				if ((ConnectionInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					ObjectChanged = 1;
					ConnectionInfo &= ~(OBJECT_SELECTED | 0x003f);
//          ConnectionInfo&=~(OBJECT_SELECTED);
				}
			}
			else
			{
				if ((TestResult = RectTestLine2(x1, y1, x2, y2)) != 0)
				{
					if ((ConnectionInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						ConnectionInfo &= ~(OBJECT_SELECTED | 0x003f);
						ConnectionInfo |= TestResult;
//            ConnectionsSelected--;
						ObjectChanged = 1;
					}
					else
					{
//            if (TestResult!=3) {
						ConnectionInfo &= ~0x003f;
						ConnectionInfo |= OBJECT_SELECTED;
						ConnectionInfo |= TestResult;
						ConnectionsSelected++;
						ObjectChanged = 1;
//            }
					}
				}
			}

			if (ObjectChanged)
			{
				Connection->Info = (int16) ConnectionInfo;
				DrawConnection(Connection);
			}
		}
	}

	SetROP2(OutputDisplay, R2_COPYPEN);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectObjectLinesFromRectWindow(int32 Mode)
{
	int32 cnt, ObjectLineInfo, TestResult, Layer;
	double x1, y1, x2, y2;
	int32 ObjectChanged;

	ObjectLineRecord *ObjectLine;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
		Layer = ObjectLine->Layer;
		DrawCode = -1;

		if (Layer < 32)
			DrawCode = DrawLayerCode[Layer];

		ObjectLineInfo = ObjectLine->Info;

		if ((ObjectLineInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (Layer == BOARD_OUTLINE_LAYER)
			{
				ok = 1;

				if (InRangeSpecial(ObjectLine->Y1, 106.6e5, 0.1e5))
					ok = 1;
			}

#endif

			if (((Mode == 0) && (OkToDrawObjectsLayer(Layer) == 0))
			        || ((Mode == 1) && (DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS)))
			{
				x1 = ObjectLine->X1;
				y1 = ObjectLine->Y1;
				x2 = ObjectLine->X2;
				y2 = ObjectLine->Y2;
				ObjectChanged = 0;

				if (UnselectAll)
				{
					if ((ObjectLineInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						ObjectLineInfo &= ~(OBJECT_SELECTED | 3);
						ObjectChanged = 1;
					}
				}
				else
				{
					if ((TestResult = RectTestLine2(x1, y1, x2, y2)) != 0)
					{
						if (!ReplaceSelections)
						{
							if ((ObjectLineInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							{
								if (ShiftPressed)
									ObjectLineInfo &= ~OBJECT_SELECTED;
							}
							else
							{
								ObjectLineInfo &= ~3;
								ObjectLineInfo |= (OBJECT_SELECTED | TestResult);
							}
						}
						else
						{
							if (ShiftPressed)
							{
								if ((ObjectLineInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
									ObjectLineInfo &= ~(OBJECT_SELECTED | 3);
								else
								{
									ObjectLineInfo &= ~3;
									ObjectLineInfo |= (OBJECT_SELECTED | TestResult);
								}
							}
							else
							{
								ObjectLineInfo &= ~3;
								ObjectLineInfo |= (OBJECT_SELECTED | TestResult);
							}
						}

						ObjectChanged = 1;
					}
				}

				if (ObjectChanged)
				{
					ObjectLine->Info = (int16) ObjectLineInfo;
					DrawObjectLine(ObjectLine, 0.0, 0.0, 0);
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectObjectRectsFromRectWindow(int32 Mode)
{
	int32 cnt, ObjectRectInfo;
	double x1, y1, x2, y2;
	int32 ObjectChanged;

	ObjectRectRecord *ObjectRect;

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);
		ObjectRectInfo = ObjectRect->Info;

		if (((ObjectRectInfo & (OBJECT_NOT_VISIBLE)) == 0) && (OkToDrawObjectsLayer(ObjectRect->Layer) == 0))
		{
			x1 = ObjectRect->CentreX;
			y1 = ObjectRect->CentreY;
			x2 = ObjectRect->Width;
			y2 = ObjectRect->Height;
			ObjectChanged = 0;

			if (UnselectAll)
			{
				if ((ObjectRectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					ObjectRectInfo &= ~OBJECT_SELECTED;
					ObjectChanged = 1;
				}
			}
			else
			{
				if (RectTestRect2(x1, y1, x2, y2))
				{
					if (!ReplaceSelections)
					{
						if ((ObjectRectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							if (ShiftPressed)
								ObjectRectInfo &= ~OBJECT_SELECTED;
						}
						else
							ObjectRectInfo |= OBJECT_SELECTED;
					}
					else
					{
						if (ShiftPressed)
						{
							if ((ObjectRectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								ObjectRectInfo &= ~(OBJECT_SELECTED);
							else
								ObjectRectInfo |= OBJECT_SELECTED;
						}
						else
							ObjectRectInfo |= OBJECT_SELECTED;
					}

					ObjectChanged = 1;
				}
			}

			if (ObjectChanged)
			{
				ObjectRect->Info = (int16) ObjectRectInfo;
				DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
			}
		}
	}

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
/*
void SelectObjectCirclesFromRectWindow(int32 Mode)

{
  int32  cnt;
  double  x1,y1,x2,y2,dikte,lengte;
  int32  ObjectCircleInfo,ok,Layer,CircleMode;
  int32   Check,ObjectChanged;

  ObjectCircleRecord  *ObjectCircle;

  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
    ObjectCircle=&((*ObjectCircles)[cnt]);
    ObjectCircleInfo=ObjectCircle->Info;
    if (((ObjectCircleInfo & (OBJECT_NOT_VISIBLE)) == 0)
       &&
       (OkToDrawObjectsLayer(ObjectCircle->Layer)==0)) {
      x1=ObjectCircle->CentreX;
      y1=ObjectCircle->CentreY;
      x2=ObjectCircle->Diam;
      CircleMode=CircleConv[(int32)ObjectCircle->CircleMode];
      ObjectChanged=0;
      if (UnselectAll) {
        if ((ObjectCircleInfo & OBJECT_SELECTED) == OBJECT_SELECTED) {
          ObjectCircleInfo&=~OBJECT_SELECTED;
          ObjectChanged=1;
        }
      } else {
        if (RectTestCircle(x1,y1,x2,CircleMode)) {
          if (!ReplaceSelections) {
            if ((ObjectCircleInfo & OBJECT_SELECTED) == OBJECT_SELECTED) {
              if (ShiftPressed) {
                ObjectCircleInfo&=~OBJECT_SELECTED;
              }
            } else {
              ObjectCircleInfo|=OBJECT_SELECTED;
            }
          } else {
            if (ShiftPressed) {
              if ((ObjectCircleInfo & OBJECT_SELECTED) == OBJECT_SELECTED) {
                ObjectCircleInfo&=~(OBJECT_SELECTED);
              } else {
                ObjectCircleInfo|=OBJECT_SELECTED;
              }
            } else {
              ObjectCircleInfo|=OBJECT_SELECTED;
            }
          }
          ObjectChanged=1;
        }
      }
      if (ObjectChanged) {
        ObjectCircle->Info=ObjectCircleInfo;
        DrawObjectCircle(ObjectCircle,0.0,0.0,0);
      }
    }
  }
}
*/
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectObjectArcsFromRectWindow(int32 Mode)
{
	int32 cnt, ObjectArcInfo, Layer;
	double x1, y1, x2, y2, x3, y3, x4, y4;
	int32 OkToCheck, ObjectChanged, ActiveCircle;
#ifdef _DEBUG
	int32 ok;
#endif

	ObjectArcRecord *ObjectArc;

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ObjectArcInfo = ObjectArc->Info;
		ActiveCircle = 0;

		if ((ObjectArcInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			OkToCheck = 0;
			Layer = ObjectArc->Layer;

			if (Mode == 0)
			{
				if (OkToDrawObjectsLayer(ObjectArc->Layer) == 0)
					OkToCheck = 1;
			}
			else
			{
				if (Layer < 32)
				{
					DrawCode = DrawLayerCode[Layer];

					if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
						OkToCheck = 1;
				}
				else
				{
					if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
					{
						if (DrawDrillMode > 0)
							OkToCheck = 1;
					}
				}
			}

			if (OkToCheck)
			{
				x1 = ObjectArc->CentreX;
				y1 = ObjectArc->CentreY;
				x2 = ObjectArc->Width;
				y2 = ObjectArc->Height;
				x3 = ObjectArc->StartDiffX;
				y3 = ObjectArc->StartDiffY;
				x4 = ObjectArc->EndDiffX;
				y4 = ObjectArc->EndDiffY;
#ifdef _DEBUG

				if (!UnselectAll)
				{
					if ((InRange9(x1, 185.0e5)) && (InRange9(y1, -6.28e5)))
						ok = 1;
				}

#endif

				if (((ObjectArc->Info & OBJECT_FILLED) == OBJECT_FILLED) || (ObjectArc->Layer == DRILL_LAYER)
				        || (ObjectArc->Layer == DRILL_UNPLATED_LAYER) || ((InRange(x3, x4)) && (InRange(y3, y4))
				                && (InRange(x2, y2))))
					ActiveCircle = 1;

				ObjectChanged = 0;

				if (UnselectAll)
				{
					if ((ObjectArcInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						ObjectArcInfo &= ~OBJECT_SELECTED;
						ObjectChanged = 1;
					}
				}
				else
				{
					if (((!ActiveCircle) && (RectTestArc2(x1, y1, x2, y2, x3, y3, x4, y4, ObjectArc->LineThickNess)))
					        || ((ActiveCircle) && (RectTestCircle(x1, y1, x2, 255))))
					{
						if (!ReplaceSelections)
						{
							if ((ObjectArcInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							{
								if (ShiftPressed)
									ObjectArcInfo &= ~OBJECT_SELECTED;
							}
							else
								ObjectArcInfo |= OBJECT_SELECTED;
						}
						else
						{
							if (ShiftPressed)
							{
								if ((ObjectArcInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
									ObjectArcInfo &= ~(OBJECT_SELECTED);
								else
									ObjectArcInfo |= OBJECT_SELECTED;
							}
							else
								ObjectArcInfo |= OBJECT_SELECTED;
						}

						ObjectChanged = 1;
					}
				}

				if (ObjectChanged)
				{
					ObjectArc->Info = (int16) ObjectArcInfo;
					DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
				}
			}
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectObjectTexts2FromRectWindow(int32 Mode)
{
	int32 cnt, cnt3, NrLines, ObjectTextInfo, TextMode, TextAlignment, MaxCountX, FontNr, Mirror;
	double x1, y1, x2, x4, y4, Rotation;
	int32 ObjectChanged, ObjectToBeSelected;
#ifdef _DEBUG
	int32 ok;
#endif

	ObjectTextRecord2 *ObjectText2;

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);
		ObjectTextInfo = ObjectText2->Info;

		if (((ObjectTextInfo & (OBJECT_NOT_VISIBLE)) == 0) && (OkToDrawObjectsLayer(ObjectText2->Layer) == 0))
		{
			x1 = ObjectText2->X;
			y1 = ObjectText2->Y;
			x2 = ObjectText2->FontHeight;
			TextMode = ObjectText2->TextMode;
			FontNr = ObjectText2->FontNr;
			Rotation = ObjectText2->Rotation;
			TextAlignment = (TextMode & 0x0f);
			Mirror = (ObjectText2->TextMode & 0x10) >> 4;
			ObjectChanged = 0;

			if (UnselectAll)
			{
				if ((ObjectTextInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					ObjectTextInfo &= ~OBJECT_SELECTED;
					ObjectChanged = 1;
				}
			}
			else
			{
				ObjectToBeSelected = 0;

				if (strlen(ObjectText2->Text) > 0)
				{
					NrLines = ConvertObjectTextToStrings(ObjectText2->Text, FontNr, &MaxCountX, ObjectText2->Layer);
#ifdef _DEBUG
					Length = strlen(ObjectText2->Text);

					if (Length > 40)
						ok = 1;

					if (stricmp(ObjectText2->Text, "Multiline text line1\r\nline2\r\nLast line 3\r\n") == 0)
						ok = 1;

#endif
					x4 = x1;
					y4 = y1;

					for (cnt3 = 0; cnt3 < NrLines; cnt3++)
					{
						if (FontNr == 0)
						{
							if (RectTestText2(x4, y4, x2, Rotation, Mirror, TextStrings2[cnt3]))
								ObjectToBeSelected = 1;

							if (Mirror == 0)
								x4 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
							else
								x4 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;

							y4 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
						}
						else
						{
							if (RectTestText2b(x4, y4, x2, Rotation, Mirror, FontNr, TextStrings[cnt3]))
								ObjectToBeSelected = 1;

							if (Mirror == 0)
								x4 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
							else
								x4 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;

							y4 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
						}
					}
				}

				if (ObjectToBeSelected)
				{
					if (!ReplaceSelections)
					{
						if ((ObjectTextInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							if (ShiftPressed)
								ObjectTextInfo &= ~OBJECT_SELECTED;
						}
						else
							ObjectTextInfo |= OBJECT_SELECTED;
					}
					else
					{
						if (ShiftPressed)
						{
							if ((ObjectTextInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								ObjectTextInfo &= ~(OBJECT_SELECTED);
							else
								ObjectTextInfo |= OBJECT_SELECTED;
						}
						else
							ObjectTextInfo |= OBJECT_SELECTED;
					}

					ObjectChanged = 1;
				}
			}

			if (ObjectChanged)
			{
				ObjectText2->Info = (int16) ObjectTextInfo;
				DrawObjectText2(ObjectText2, 0.0, 0.0, 0.0, 0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void SelectObjectPolygonsFromRectWindow(int32 Mode)
{
	int32 cnt, ObjectPolygonInfo;
	int32 ObjectChanged;
	double cx, cy;

	ObjectPolygonRecord *ObjectPolygon;

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		ObjectPolygonInfo = ObjectPolygon->Info;

		if (((ObjectPolygonInfo & (OBJECT_NOT_VISIBLE)) == 0) && (OkToDrawObjectsLayer(ObjectPolygon->Layer) == 0))
		{
			ObjectChanged = 0;

			if (UnselectAll)
			{
				if ((ObjectPolygonInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					ObjectPolygonInfo &= ~OBJECT_SELECTED;
					ObjectChanged = 1;
				}
			}
			else
			{
				cx = (SearchMinX + SearchMaxX) * 0.5;
				cy = (SearchMinY + SearchMaxY) * 0.5;

				if (((PointInObjectPolygon(ObjectPolygon, cx, cy) & 1) == 1)
				        || (ObjectPolygonInSearchArea(ObjectPolygon) == 1))
				{
					if (!ReplaceSelections)
					{
						if ((ObjectPolygonInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							if (ShiftPressed)
								ObjectPolygonInfo &= ~OBJECT_SELECTED;
						}
						else
							ObjectPolygonInfo |= OBJECT_SELECTED;
					}
					else
					{
						if (ShiftPressed)
						{
							if ((ObjectPolygonInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
								ObjectPolygonInfo &= ~(OBJECT_SELECTED);
							else
								ObjectPolygonInfo |= OBJECT_SELECTED;
						}
						else
							ObjectPolygonInfo |= OBJECT_SELECTED;
					}

					ObjectChanged = 1;
				}
			}

			if (ObjectChanged)
			{
				ObjectPolygon->Info = (int16) ObjectPolygonInfo;
				DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void SelectReferencesFromRectWindow(int32 Mode)
{
	int32 cnt, MemPos, lengte, OldTextVisibility, OldInfo, Mirror, CompInfo, ShapeNr, TextVisibility;
	double x2, y2, h2, RotationAngle, RotationAngle2, TextRotation;
	int32 ObjectChanged;
	ShapeRecord *Shape;
	CompRecord *Comp;
#ifdef _DEBUG
	int32 res;
#endif

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

		if (stricmpOwn(Comp->Name, "C122") == 0)
			res = 1;

#endif
		CompInfo = Comp->Info;
		ObjectChanged = 0;
		TextVisibility = Comp->TextVisibility;
		OldTextVisibility = TextVisibility;
		OldInfo = CompInfo;
		Mirror = ((Comp->CompMode & 8) >> 3);

		if (((Mirror == 0) && (!OkToDrawSilkScreenTop)) || ((Mirror == 1) && (!OkToDrawSilkScreenBottom)))
			continue;

		if (((CompInfo & OBJECT_NOT_VISIBLE) == 0) && ((Comp->TextVisibility & 0x100) == 0))
		{
//       &&
//       ((TextVisibility & 4) == 0)) {
			if (UnselectAll)
			{
				if (((TextVisibility & 1) == 1) || ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED))
				{
					TextVisibility &= ~1;
					ObjectChanged = 1;
				}
			}
			else
			{
				ShapeNr = Comp->ShapeNr;
				MemPos = (*Shapes)[ShapeNr].ShapePos;
				Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
				lengte = strlen(Comp->Name);
				h2 = Comp->CompNameHeight;

				if (h2 == 0)
					h2 = Shape->ShapeNameHeight;

				x2 = Comp->CompNameOriginX;
				y2 = Comp->CompNameOriginY;

				TextRotation = Comp->CompNameRotation;
				RotationAngle = Comp->Rotation;
				RotationAngle2 = TextRotation;

				Mirror = (Comp->TextVisibility & 8) >> 3;

				if (Mirror == 0)
					RotationAngle2 += RotationAngle;
				else
				{
					x2 = -x2;
					RotationAngle2 -= RotationAngle;
				}

				if (RotationAngle2 > 360)
					RotationAngle2 -= 360;

				if (RotationAngle2 < 0)
					RotationAngle2 += 360;

				RotatePoint2(&x2, &y2, RotationAngle);

				x2 = x2 + Comp->CompOriginX;
				y2 = y2 + Comp->CompOriginY;

				if (RectTestText2(x2, y2, h2, RotationAngle2, Mirror, Comp->Name))
				{
					if (!ReplaceSelections)
					{
						if ((TextVisibility & 1) == 1)
						{
							if (ShiftPressed)
								TextVisibility &= ~1;
						}
						else
							TextVisibility |= 1;
					}
					else
					{
						if (ShiftPressed)
						{
							if ((TextVisibility & 1) == 1)
								TextVisibility &= ~1;
							else
								TextVisibility |= 1;
						}
						else
							TextVisibility |= 1;
					}

					ObjectChanged = 1;
				}
			}

			if (ObjectChanged)
			{
				Comp->TextVisibility = TextVisibility;

				if ((OldTextVisibility & 1) == 1)
				{
					if ((TextVisibility & 1) == 0)
					{
						SetBackGroundActive(0);
						Comp->TextVisibility &= ~(1 + 0x10);
						Comp->Info &= ~OBJECT_SELECTED;
						DrawComp2(Comp, 0.0, 0.0, 0, 0x205);
					}
				}
				else
				{
					Comp->TextVisibility |= (1 + 0x10);
					Comp->Info |= OBJECT_SELECTED;
				}

				DrawComp2(Comp, 0.0, 0.0, 0, 0x204);
//        return;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrReferencesSelected()
{
	int32 cnt, count;

	CompRecord *Comp;

	count = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && ((Comp->TextVisibility & 1) == 1))
			count++;
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectCompValuesFromRectWindow(int32 Mode)
{
	int32 cnt, MemPos, lengte, OldTextVisibility, OldInfo, Mirror, CompInfo, ShapeNr, TextVisibility;
	double x2, y2, h2, NewRotation, TextRotation;
	int32 ObjectChanged;
	ShapeRecord *Shape;
	CompRecord *Comp;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;
		ObjectChanged = 0;
		TextVisibility = Comp->TextVisibility;
		OldTextVisibility = TextVisibility;
		OldInfo = CompInfo;
		Mirror = ((Comp->CompMode & 8) >> 3);

		if (((Mirror == 0) && (!OkToDrawSilkScreenTop)) || ((Mirror == 1) && (!OkToDrawSilkScreenBottom)))
			continue;

		if (((CompInfo & OBJECT_NOT_VISIBLE) == 0) && ((Comp->TextVisibility & 0x200) == 0))
		{
			if (UnselectAll)
			{
				if (((TextVisibility & 0x10) == 0x10) || ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED))
				{
					TextVisibility &= ~0x10;
					ObjectChanged = 1;
				}
			}
			else
			{
				ShapeNr = Comp->ShapeNr;
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
				x2 = x2 + Comp->CompOriginX;
				y2 = y2 + Comp->CompOriginY;

				if (RectTestText2(x2, y2, h2, TextRotation, Mirror, Comp->Value))
				{
					if (!ReplaceSelections)
					{
						if ((TextVisibility & 0x10) == 0x10)
						{
							if (ShiftPressed)
								TextVisibility &= ~0x10;
						}
						else
							TextVisibility |= 0x10;
					}
					else
					{
						if (ShiftPressed)
						{
							if ((TextVisibility & 0x10) == 0x10)
								TextVisibility &= ~0x10;
							else
								TextVisibility |= 0x10;
						}
						else
							TextVisibility |= 0x10;
					}

					ObjectChanged = 1;
				}
			}

			if (ObjectChanged)
			{
				Comp->TextVisibility = TextVisibility;

				if ((OldTextVisibility & 0x10) == 0x10)
				{
					if ((TextVisibility & 0x10) == 0)
					{
						SetBackGroundActive(0);
						Comp->TextVisibility &= ~(1 + 16);
						Comp->Info &= ~OBJECT_SELECTED;
						DrawComp2(Comp, 0.0, 0.0, 0, 0x205);
					}
				}
				else
				{
					Comp->TextVisibility |= (1 + 16);
					Comp->Info |= OBJECT_SELECTED;
				}

				DrawComp2(Comp, 0.0, 0.0, 0, 0x204);
//        return;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrCompValuesSelected()
{
	int32 cnt, count;

	CompRecord *Comp;

	count = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && ((Comp->TextVisibility & 0x10) == 0x10))
			count++;
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SelectAreaFillsFromRectWindow(int32 Mode)
{
	AreaFillRecord *AreaFill;
	int32 ObjectChanged;
	int32 cnt, cnt2, FoundSubPolygon;
	double cx, cy;
	NetRecord *Net;
	char LayerText[MAX_LENGTH_STRING];
	PolygonRecord *DrawPolygon, *FirstPolygon;
	uint8 *AreaPos, *PolygonPos;
#ifdef _DEBUG
	int32 ok;
#endif

// DrawPolygon->PolygonType  =  2   ->  polygon selected

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectChanged = 0;

			if (UnselectAll)
			{
				AreaPos = (uint8 *) AreaFill;
				DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

				for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
				{
					PolygonPos = (uint8 *) DrawPolygon;

					if ((DrawPolygon->PolygonType & 2) == 2)
					{
						DrawPolygon->PolygonType &= ~2;
						ObjectChanged = 1;
					}

					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;
				}
			}
			else
			{
				DrawCode = DrawLayerCode[AreaFill->Layer];

				if ((DrawCode >= 0) && (DrawCode < MAX_ACTIVE_DRAWING_LAYERS))
				{
					cx = (SearchMinX + SearchMaxX) * 0.5;
					cy = (SearchMinY + SearchMaxY) * 0.5;
					AreaPos = (uint8 *) AreaFill;
					DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
#ifdef _DEBUG

					if (cnt == 2)
						ok = 1;

#endif

					if (Mode == 0)
					{
						FirstPolygon = NULL;
						FoundSubPolygon = 0;

						for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
						{
							PolygonPos = (uint8 *) DrawPolygon;
#ifdef _DEBUG

							if (cnt == 2)
							{
								if ((DrawPolygon->PolygonType & 2) == 2)
								{	// User deletion
									ok = 1;
								}

								if (cnt2 == 1)
									ok = 1;
							}

#endif

							if (cnt2 == 0)
							{
								if (RectTestPolygon(DrawPolygon))
									FirstPolygon = DrawPolygon;
							}
							else
							{
// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion
								if ((PointInPolygon(DrawPolygon, cx, cy) & 1) == 1)
								{
									FoundSubPolygon = 1;

//#else
#ifdef _DEBUG

									if ((DrawPolygon->PolygonType & 8) == 8)
										ok = 1;

#endif

									if (((DrawPolygon->PolygonType & 1) == 1) || ((DrawPolygon->PolygonType & 8) == 8))
									{
										if (!ReplaceSelections)
										{
											if ((DrawPolygon->PolygonType & 2) == 2)
											{
												if (ShiftPressed)
												{
													DrawPolygon->PolygonType &= ~2;
													ObjectChanged = 1;
												}
											}
											else
											{
												DrawPolygon->PolygonType |= 2;
												ObjectChanged = 1;
											}
										}
										else
										{
											if (ShiftPressed)
											{
												DrawPolygon->PolygonType ^= 2;
												ObjectChanged = 1;
											}
											else
											{
												DrawPolygon->PolygonType |= 2;
												ObjectChanged = 1;
											}
										}
									}
								}
							}

							PolygonPos += MemSizePolygon(DrawPolygon);
							DrawPolygon = (PolygonRecord *) PolygonPos;
						}

						if ((FoundSubPolygon == 0) && (FirstPolygon != NULL))
						{
							DrawPolygon = FirstPolygon;

							if (!ReplaceSelections)
							{
								if ((DrawPolygon->PolygonType & 2) == 2)
								{
									if (ShiftPressed)
									{
										DrawPolygon->PolygonType &= ~2;
										ObjectChanged = 1;
									}
								}
								else
								{
									DrawPolygon->PolygonType |= 2;
									ObjectChanged = 1;
								}
							}
							else
							{
								if (ShiftPressed)
								{
									DrawPolygon->PolygonType ^= 2;
									ObjectChanged = 1;
								}
								else
								{
									DrawPolygon->PolygonType |= 2;
									ObjectChanged = 1;
#ifdef _DEBUG

									if (0)
										AreafillToObjectLines(AreaFill, INFO_LAYER4, 0, 0, 1);

#endif
								}
							}
						}
					}
					else
					{
						if ((AreaFill->maxx >= SearchMinX) && (AreaFill->minx <= SearchMaxX)
						        && (AreaFill->maxy >= SearchMinY) && (AreaFill->miny <= SearchMaxY))
						{
							if (!ReplaceSelections)
							{
								if ((DrawPolygon->PolygonType & 2) == 2)
								{
									if (ShiftPressed)
									{
										DrawPolygon->PolygonType &= ~2;
										ObjectChanged = 1;
									}
								}
								else
								{
									DrawPolygon->PolygonType |= 2;
									ObjectChanged = 1;
								}
							}
							else
							{
								if (ShiftPressed)
								{
									DrawPolygon->PolygonType ^= 2;
									ObjectChanged = 1;
								}
								else
								{
									DrawPolygon->PolygonType |= 2;
									ObjectChanged = 1;
								}
							}
						}
					}
				}
			}

			if (ObjectChanged)
			{
				DrawAreaFill(AreaFill, 2);
				GetLayerText(AreaFill->Layer, LayerText, 0);

				if (AreaFill->NetNr >= 0)
					Net = &((*Nets)[AreaFill->NetNr]);
				else
					Net = &EmptyNet;

				sprintf(InfoStr, SC(341, "AreaFill %s Net %s"), LayerText, Net->Name);
				RedrawInfoStr(0);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrAreaFillsSelected()
{
	AreaFillRecord *AreaFill;
	int32 cnt, cnt2, count;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

	count = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			AreaPos = (uint8 *) AreaFill;
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));

			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				PolygonPos = (uint8 *) DrawPolygon;

				if ((DrawPolygon->PolygonType & 2) == 2)
					count++;

				PolygonPos += MemSizePolygon(DrawPolygon);
				DrawPolygon = (PolygonRecord *) PolygonPos;
			}
		}
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrPowerPlanes()
{
	AreaFillRecord *AreaFill;
	int32 cnt, count;

	count = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == POWERPLANE)
			count++;
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNrObjectSelections(int32 Layer, int32 mode)
{
	int32 cnt, count;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectLinesSelected = 0;
	ObjectRectsSelected = 0;
	ObjectRectsFilledSelected = 0;
	ObjectCirclesSelected = 0;
	ObjectCirclesFilledSelected = 0;
	ObjectArcsSelected = 0;
	ObjectTextsSelected = 0;
	ObjectPolygonsSelected = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((mode != 100) || (ObjectLine->Layer == Layer)))
			ObjectLinesSelected++;
	}


	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((mode != 100) || (ObjectRect->Layer == Layer)))
		{
			ObjectRectsSelected++;

			if ((ObjectRect->Info & (OBJECT_FILLED)) == OBJECT_FILLED)
				ObjectRectsFilledSelected++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((mode != 100) || (ObjectArc->Layer == Layer)))
		{
			if (CheckObjectArcIsCircle(ObjectArc, 0))
			{
				ObjectCirclesSelected++;

				if ((ObjectArc->Info & (OBJECT_FILLED)) == OBJECT_FILLED)
					ObjectCirclesFilledSelected++;
			}
			else
				ObjectArcsSelected++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((mode != 100) || (ObjectText->Layer == Layer)))
			ObjectTextsSelected++;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((mode != 100) || (ObjectText2->Layer == Layer)))
			ObjectTextsSelected++;
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((mode != 100) || (ObjectPolygon->Layer == Layer)))
			ObjectPolygonsSelected++;
	}

	switch (mode)
	{
	case -1:
	case 100:
		return (ObjectLinesSelected + ObjectRectsSelected + ObjectCirclesSelected + ObjectArcsSelected +
		        ObjectTextsSelected + ObjectPolygonsSelected);

	case 0:
		return ObjectLinesSelected;

	case 1:
		return ObjectRectsSelected;

	case 2:
		return ObjectCirclesSelected;

	case 3:
		return ObjectArcsSelected;

	case 4:
		return ObjectTextsSelected;

	case 5:
		return ObjectPolygonsSelected;

	case 11:
		return ObjectRectsFilledSelected;

	case 12:
		return ObjectCirclesFilledSelected;

	case 20:
		count = ObjectLinesSelected;

		if (ObjectRectsSelected > ObjectRectsFilledSelected)
			count += ObjectRectsSelected - ObjectRectsFilledSelected;

		if (ObjectCirclesSelected > ObjectCirclesFilledSelected)
			count += ObjectCirclesSelected - ObjectCirclesFilledSelected;

		if (ObjectArcsSelected > ObjectCirclesSelected)
			count += ObjectArcsSelected - ObjectCirclesSelected;

		return count;

	case 30:
		count = 0;

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectLine->NetNr >= 0) && (CheckIfLayerHasObjectWithClearances(ObjectLine->Layer)))
				count++;
		}

		/*
		      for (cnt=0;cnt<Design.NrObjectRects;cnt++) {
		        ObjectRect=&((*ObjectRects)[cnt]);
		        if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
		           &&
		           (CheckIfLayerHasObjectWithClearances(ObjectRect->Layer))) {
		          count++;
		        }
		      }
		*/
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        /*
			                   &&
			                   (ObjectArc->NetNr>=0)
			        */
			        && (CheckIfLayerHasObjectWithClearances(ObjectArc->Layer)))
				count++;
		}

		/*
		      for (cnt=0;cnt<Design.NrObjectPolygons;cnt++) {
		        ObjectPolygon=(ObjectPolygonRecord *)&(ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		        if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
		           &&
		           (CheckIfLayerHasObjectWithClearances(ObjectPolygon->Layer))) {
		        }
		      }
		*/
		return count;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SelectOnlyObjectOnLayer(int32 Layer, int32 mode)
{
	int32 cnt;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->Layer != Layer))
			ObjectLine->Info &= ~OBJECT_SELECTED;
	}


	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->Layer != Layer))
			ObjectRect->Info &= ~OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->Layer != Layer))
			ObjectArc->Info &= ~OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->Layer != Layer))
			ObjectText2->Info &= ~OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->Layer != Layer))
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
	}

	RePaint();
	return 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 UnselectObjectOnLayer(int32 Layer, int32 mode)
{
	int32 cnt;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->Layer == Layer))
			ObjectLine->Info &= ~OBJECT_SELECTED;
	}


	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->Layer == Layer))
			ObjectRect->Info &= ~OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->Layer == Layer))
			ObjectArc->Info &= ~OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->Layer == Layer))
			ObjectText2->Info &= ~OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->Layer == Layer))
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
	}

	RePaint();
	return 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 IndexSelectedObjects(int32 CopyMode, int32 * SelectedObjects, int32 MaxSelectedObjects, int32 mode)
{
	int32 cnt, NrFound, Layer, SelectedIndex, TraceInfo, ViaInfo, ConnectionInfo, *IndexP;
	TraceRecord *Trace;
	ViaRecord *Via;
	ConnectionsRecord *Connection;


	SelectedIndex = 0;
	NrFound = 0;
	IndexP = SelectedObjects;

	for (Layer = 0; Layer < 32; Layer++)
	{
		NrIndexesVerTraces[Layer] = 0;
		IndexVerTraces[Layer] = IndexP;

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (CopyMode == 0)
				{
					if (SelectedIndex < MaxSelectedObjects)
					{
						NrIndexesVerTraces[Layer]++;
						*IndexP = cnt;
						IndexP += 1;
						SelectedIndex++;
					}
				}

				NrFound++;
			}
		}

		NrIndexesHorTraces[Layer] = 0;
		IndexHorTraces[Layer] = IndexP;

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (CopyMode == 0)
				{
					if (SelectedIndex < MaxSelectedObjects)
					{
						NrIndexesHorTraces[Layer]++;
						*IndexP = cnt;
						IndexP += 1;
						SelectedIndex++;
					}
				}

				NrFound++;
			}
		}

		NrIndexesDiag1Traces[Layer] = 0;
		IndexDiag1Traces[Layer] = IndexP;

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (CopyMode == 0)
				{
					if (SelectedIndex < MaxSelectedObjects)
					{
						NrIndexesDiag1Traces[Layer]++;
						*IndexP = cnt;
						IndexP += 1;
						SelectedIndex++;
					}
				}

				NrFound++;
			}
		}

		NrIndexesDiag2Traces[Layer] = 0;
		IndexDiag2Traces[Layer] = IndexP;

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (CopyMode == 0)
				{
					if (SelectedIndex < MaxSelectedObjects)
					{
						NrIndexesDiag2Traces[Layer]++;
						*IndexP = cnt;
						IndexP += 1;
						SelectedIndex++;
					}
				}

				NrFound++;
			}
		}
	}


	NrIndexesVias = 0;
	IndexVias = IndexP;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (CopyMode == 0)
			{
				if (SelectedIndex < MaxSelectedObjects)
				{
					NrIndexesVias++;
					*IndexP = cnt;
					IndexP += 1;
					SelectedIndex++;
				}
			}

			NrFound++;
		}
	}

	if ((mode & 2) == 2)
	{
		NrIndexesConnections = 0;
		IndexConnections = IndexP;

		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);
			ConnectionInfo = Connection->Info;

			if ((ConnectionInfo & (CONNECTIONS_DISABLED | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((ConnectionInfo & (OBJECT_HIGHLITED | OBJECT_NOT_VISIBLE)) != OBJECT_NOT_VISIBLE)
				{
					if (CopyMode == 0)
					{
						if (SelectedIndex < MaxSelectedObjects)
						{
							NrIndexesConnections++;
							*IndexP = cnt;
							IndexP += 1;
							SelectedIndex++;

							if ((mode & 3) == 3)
								Connection->Info &= ~OBJECT_SELECTED;
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

int32 CopySelectedTracesToObjects4(int32 mode, int32 SelectedLayer)
{
	int32 cnt, NrFound, start, end, Layer, TraceInfo;

	TraceRecord *Trace;
	ObjectRecord *Object;

	NrFound = 0;
	start = 0;
	end = 32;

	if ((SelectedLayer != -1) && (SelectedLayer >= 0) && (SelectedLayer < 32))
	{
		start = SelectedLayer;
		end = SelectedLayer + 1;
	}

	for (Layer = start; Layer < end; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (mode == 0)
				{
					if (NrObjects4 < MaxNrObjects4)
					{
						Object = &((*Objects4)[NrObjects4]);
						Object->x1 = Trace->X;
						Object->y1 = Trace->Y;
						Object->x2 = Object->x1;
						Object->y2 = Object->y1 + Trace->Length;
						Object->ObjectType = CONNECTION;
						NrObjects4++;
					}
				}

				NrFound++;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (mode == 0)
				{
					if (NrObjects4 < MaxNrObjects4)
					{
						Object = &((*Objects4)[NrObjects4]);
						Object->x1 = Trace->X;
						Object->y1 = Trace->Y;
						Object->x2 = Object->x1 + Trace->Length;
						Object->y2 = Object->y1;
						Object->ObjectType = CONNECTION;
						NrObjects4++;
					}
				}

				NrFound++;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (mode == 0)
				{
					if (NrObjects4 < MaxNrObjects4)
					{
						Object = &((*Objects4)[NrObjects4]);
						Object->x1 = Trace->X;
						Object->y1 = Trace->Y;
						Object->x2 = Object->x1 + Trace->Length;
						Object->y2 = Object->y1 - Trace->Length;
						Object->ObjectType = CONNECTION;
						NrObjects4++;
					}
				}

				NrFound++;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (mode == 0)
				{
					if (NrObjects4 < MaxNrObjects4)
					{
						Object = &((*Objects4)[NrObjects4]);
						Object->x1 = Trace->X;
						Object->y1 = Trace->Y;
						Object->x2 = Object->x1 + Trace->Length;
						Object->y2 = Object->y1 + Trace->Length;
						Object->ObjectType = CONNECTION;
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

int32 CopySelectedViasToObjects4(int32 mode, int32 SelectedLayer)
{
	int32 cnt, NrFound, ViaInfo;
	ViaRecord *Via;
	ObjectRecord *Object;

	NrFound = 0;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (mode == 0)
			{
				if (NrObjects4 < MaxNrObjects4)
				{
					Object = &((*Objects4)[NrObjects4]);
					Object->x1 = Via->X;
					Object->y1 = Via->Y;
//          Object->x2=Object->x1+Trace->Length;
					Object->ObjectType = VIA_PUT_THROUGH_ROUND;
					NrObjects4++;
				}
			}

			NrFound++;
		}
	}

	return NrFound;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopySelectedConnectionsToObjects4(int32 mode, int32 SelectedLayer)
{
	int32 cnt, NrFound, ConnectionInfo;

	ConnectionsRecord *Connection;
	ObjectRecord *Object;

	NrFound = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		ConnectionInfo = Connection->Info;

		if ((ConnectionInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (mode == 0)
			{
				if (NrObjects4 < MaxNrObjects4)
				{
					Object = &((*Objects4)[NrObjects4]);
					Object->x1 = Connection->x1;
					Object->y1 = Connection->y1;
					Object->x2 = Connection->x2;
					Object->y2 = Connection->y2;
					Object->ObjectType = CONNECTION;
					Object->NetNr = Connection->NetNr;
					Object->Layer = Connection->Layer;
					Object->TraceNr = cnt;
					NrObjects4++;
				}
			}

			NrFound++;
		}
	}

	return NrFound;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetMaxRectSelectedObjects(int32 mode, double *MinX, double *MinY, double *MaxX, double *MaxY)
{
	int32 cnt, cnt2, TextMode, Mirror, TextAlignment, NrLines, FontNr, MaxCountX;
	double x1, y1, x2, x4, y4, Rotation, minx, maxx, miny, maxy;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	minx = 1000000000.0;
	miny = 1000000000.0;
	maxx = -1000000000.0;
	maxy = -1000000000.0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(ObjectLine->X1, minx);
			maxx = max(ObjectLine->X1, maxx);
			minx = min(ObjectLine->X2, minx);
			maxx = max(ObjectLine->X2, maxx);
			miny = min(ObjectLine->Y1, miny);
			maxy = max(ObjectLine->Y1, maxy);
			miny = min(ObjectLine->Y2, miny);
			maxy = max(ObjectLine->Y2, maxy);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(ObjectRect->CentreX - ObjectRect->Width * 0.5, minx);
			maxx = max(ObjectRect->CentreX + ObjectRect->Width * 0.5, maxx);
			miny = min(ObjectRect->CentreY - ObjectRect->Height * 0.5, miny);
			maxy = max(ObjectRect->CentreY + ObjectRect->Height * 0.5, maxy);
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      minx=min(ObjectCircle->CentreX-ObjectCircle->Diam*0.5,minx);
	      maxx=max(ObjectCircle->CentreX+ObjectCircle->Diam*0.5,maxx);
	      miny=min(ObjectCircle->CentreY-ObjectCircle->Diam*0.5,miny);
	      maxy=max(ObjectCircle->CentreY+ObjectCircle->Diam*0.5,maxy);
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(ObjectArc->CentreX - ObjectArc->Width * 0.5, minx);
			maxx = max(ObjectArc->CentreX + ObjectArc->Width * 0.5, maxx);
			miny = min(ObjectArc->CentreY - ObjectArc->Height * 0.5, miny);
			maxy = max(ObjectArc->CentreY + ObjectArc->Height * 0.5, maxy);
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if ((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      x1=ObjectText->X;
	      y1=ObjectText->Y;
	      x2=ObjectText->FontHeight;
	      TextMode=ObjectText->TextMode;
	      TextRotation=(TextMode >> 8) & 3;
	      TextAlignment=(TextMode & 0x0f);
	      Mirror=(ObjectText->TextMode & 0x10) >> 4;
	      GetMinMaxText(x1,y1,x2,0,TextRotation,TextAlignment,Mirror,ObjectText->Text);
	      minx=min(minx,TextMinX);
	      maxx=max(maxx,TextMaxX);
	      miny=min(miny,TextMinY);
	      maxy=max(maxy,TextMaxY);
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			x1 = ObjectText2->X;
			y1 = ObjectText2->Y;
			x2 = ObjectText2->FontHeight;
			TextMode = ObjectText2->TextMode;
			Rotation = ObjectText2->Rotation;
			FontNr = ObjectText2->FontNr;
			TextAlignment = (TextMode & 0x0f);
			Mirror = (ObjectText2->TextMode & 0x10) >> 4;
			NrLines = ConvertObjectTextToStrings(ObjectText2->Text, FontNr, &MaxCountX, ObjectText2->Layer);
			x4 = x1;
			y4 = y1;

			for (cnt2 = 0; cnt2 < NrLines; cnt2++)
			{
				if (FontNr == 0)
					GetMinMaxText2(x4, y4, x2, FontNr, Rotation, 0, Mirror, TextStrings2[cnt2]);
				else
					GetMinMaxText2b(x4, y4, x2, FontNr, Rotation, 0, Mirror, TextStrings[cnt2]);

				minx = min(minx, TextMinX);
				maxx = max(maxx, TextMaxX);
				miny = min(miny, TextMinY);
				maxy = max(maxy, TextMaxY);

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
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SetMinMaxObjectPolygon(ObjectPolygon, 0);
			minx = min(minx, ObjectPolygon->minx);
			maxx = max(maxx, ObjectPolygon->maxx);
			miny = min(miny, ObjectPolygon->miny);
			maxy = max(maxy, ObjectPolygon->maxy);
		}
	}

	*MinX = minx;
	*MaxX = maxx;
	*MinY = miny;
	*MaxY = maxy;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PrintValue(double value, LPSTR ValueStr, int32 mode)
{
	if (Units == 0)
	{
		sprintf(ValueStr, "%.1f", value / 2540.0); //trasy/pruchodky

		if (mode == 1)
			strcat(ValueStr, " thou"); //trasy/pruchodky
	}
	else
	{
		sprintf(ValueStr, "%.4f", value / 100000.0); //trasy/pruchodky

		if (mode == 1)
			strcat(ValueStr, "  mm"); //trasy/pruchodky
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PrintTrace(TraceRecord * Trace, LPSTR ValueStr, int32 TraceType, int32 Layer, int32 CountIndex)
{
	char str2[MAX_LENGTH_STRING];
	NetRecord *Net;
	int32 NetNr;

	if (Layer == 0)
		sprintf(ValueStr, SC(1357, "Trace\tBottom\t"), Layer); //trasy/pruchodky
	else
	{
		if (Design.NrBoardLayers > 1)
		{
			if (Layer == Design.NrBoardLayers - 1)
				sprintf(ValueStr, SC(1358, "Trace\tTop\t"), Layer); //trasy/pruchodky
			else
				sprintf(ValueStr, SC(1359, "Trace\tLayer %i\t"), Layer); //trasy/pruchodky
		}
	}

	NetNr = Trace->NetNr;
	Net = &((*Nets)[NetNr]);
	strcat(ValueStr, Net->Name);
	strcat(ValueStr, "\t");
	PrintValue(Trace->X, str2, 0);
	strcat(ValueStr, str2);
	strcat(ValueStr, "  ");
	PrintValue(Trace->Y, str2, 0);
	strcat(ValueStr, str2);
	strcat(ValueStr, "\t");

	if (TraceType == TRACE_VER)
		PrintValue(Trace->X, str2, 0);
	else
		PrintValue(Trace->X + Trace->Length, str2, 0);

	strcat(ValueStr, str2);
	strcat(ValueStr, "  ");

	if ((TraceType == TRACE_VER) || (TraceType == TRACE_DIAG2))
		PrintValue(Trace->Y + Trace->Length, str2, 0);

	if (TraceType == TRACE_HOR)
		PrintValue(Trace->Y, str2, 0);

	if (TraceType == TRACE_DIAG1)
		PrintValue(Trace->Y - Trace->Length, str2, 0); // pozice x,y

	strcat(ValueStr, str2);
	strcat(ValueStr, SC(1041, "\tTrace width\t"));
	PrintValue(Trace->ThickNess, str2, 0);
	strcat(ValueStr, str2);
	strcat(ValueStr, SC(1042, "\t\t\tClearance\t"));
	PrintValue(Trace->Clearance, str2, 1);
	strcat(ValueStr, str2);
#ifdef _DEBUG
	sprintf(str2, "\t%d", CountIndex);
	strcat(ValueStr, str2);
#endif
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ObjectsInfo(ObjectRecord * Object)
{
	int32 cnt, rot, Layer, NetNr, lengte;
	double x1, y1, x2, y2, dikte, Rotation, Dist1, Dist2, Clearance, Angle1, Angle2, Angle, Length;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], Val2[MAX_LENGTH_STRING], Val4a[MAX_LENGTH_STRING],
	     Val5[MAX_LENGTH_STRING], Val6[MAX_LENGTH_STRING], Val6a[MAX_LENGTH_STRING], Val7[MAX_LENGTH_STRING],
	     Val7a[MAX_LENGTH_STRING], Val8[MAX_LENGTH_STRING], LayerText[MAX_LENGTH_STRING], ObjectText[MAX_LENGTH_STRING],
	     ObjectNrStr[MAX_LENGTH_STRING];
	NetRecord *Net;

	ObjectNrStr[0] = 0;
	Angle1 = 0.0;
	Angle2 = 0.0;
	Layer = Object->Layer;
	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	Rotation = y2;
	rot = Object->Info2;
	rot *= 90;
	dikte = Object->Thickness;
	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

	if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
		dikte = 0.0;

	ObjectNrStr[0] = 0;
	NetNr = Object->NetNr;
	Clearance = max(Object->Clearance, Design.StandardClearance);

	if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER) || ((Layer >= 0) && (Layer < Design.NrBoardLayers)))
	{
		if ((NetNr >= 0) && (NetNr < Design.NrNets - 1))
		{
			Net = &((*Nets)[NetNr]);
			strcpy(ObjectNrStr, Net->Name);
		}
	}

	if (Units == 0)
	{
		x1 = x1 / 2540.0;
		y1 = y1 / 2540.0;
		x2 = x2 / 2540.0;
		y2 = y2 / 2540.0;
		dikte = dikte / 2540.0;

		if (((Object->Info & OBJECT_FILLED) == 0) || (Object->Layer == DRILL_LAYER))
		{
			if (Object->Layer == DRILL_LAYER)
				sprintf(Val2, "%.1f  %.1f\t%.1f (%.1f)\t", x1, y1, x2, Clearance / 2540.0);
			else
				sprintf(Val2, "%.1f  %.1f\t%.1f\t%.1f", x1, y1, x2, dikte);
		}
		else
			sprintf(Val2, "%.1f  %.1f\t%.1f\t\t\t", x1, y1, x2);

		sprintf(Val4a, "%.1f  %.1f\t%.1f\t%.1f\t%i", x1, y1, x2, dikte, rot);
		sprintf(Val5, "%.1f  %.1f\t%.1f\t%.1f\t%.1f", x1, y1, x2, dikte, Rotation);

		if (Object->ObjectType == OBJECT_ARC)
		{
			ConvertPointToPolar(Object->x3, Object->y3, &Dist1, &Angle1);
			ConvertPointToPolar(Object->x4, Object->y4, &Dist2, &Angle2);
		}

		sprintf(Val6, "%.1f  %.1f\t%.1f  %.1f\t%.1f\t%.1f  %.1f", x1, y1, x2, y2, dikte, Angle1, Angle2);
		sprintf(Val6a, "%.1f  %.1f\t%.1f  %.1f\t%.1f (%.1f)\t%.1f  %.1f", x1, y1, x2, y2, dikte, Clearance / 2540.0,
		        Angle1, Angle2);
		sprintf(Val7, SC(1043, "%.1f  %.1f\t%.1f  %.1f\t%.1f\tLength  %.1f, Angle %.1f"), x1, y1, x2, y2, dikte,
		        Length / 2540.0, Angle * 180.0 / PI);
		sprintf(Val7a, SC(1104, "%.1f  %.1f\t%.1f  %.1f\t%.1f (%.1f)\tLength  %.1f, Angle %.1f"), x1, y1, x2, y2, dikte,
		        Clearance / 2540.0, Length / 2540.0, Angle * 180.0 / PI);

		if ((Object->Info & OBJECT_FILLED) == 0)
			sprintf(Val8, "%.1f  %.1f\t%.1f  %.1f\t%.1f", x1, y1, x2, y2, dikte);
		else
			sprintf(Val8, "%.1f  %.1f\t%.1f  %.1f\t\t\t", x1, y1, x2, y2);
	}
	else
	{
		x1 = x1 / 100000.0;
		y1 = y1 / 100000.0;
		x2 = x2 / 100000.0;
		y2 = y2 / 100000.0;
		dikte = dikte / 100000.0;

		if (((Object->Info & OBJECT_FILLED) == 0) || (Object->Layer == DRILL_LAYER))
		{
			if (Object->Layer == DRILL_LAYER)
				sprintf(Val2, "%.4f  %.4f\t%.4f (%.4f)\t", x1, y1, x2, Clearance / 100000.0);
			else
				sprintf(Val2, "%.4f  %.4f\t%.4f\t%.4f", x1, y1, x2, dikte);
		}
		else
			sprintf(Val2, "%.4f  %.4f\t%.4f\t\t\t", x1, y1, x2);

		sprintf(Val4a, "%.4f  %.4f\t%.4f\t%.4f\t%i", x1, y1, x2, dikte, rot);
		sprintf(Val5, "%.4f  %.4f\t%.4f\t%.4f\t%.1f", x1, y1, x2, dikte, Rotation);

		if (Object->ObjectType == OBJECT_ARC)
		{
			ConvertPointToPolar(Object->x3, Object->y3, &Dist1, &Angle1);
			ConvertPointToPolar(Object->x4, Object->y4, &Dist2, &Angle2);
		}

		sprintf(Val6, "%.4f  %.4f\t%.4f  %.4f\t%.4f\t%.1f  %.1f", x1, y1, x2, y2, dikte, Angle1, Angle2);
		sprintf(Val6a, "%.4f  %.4f\t%.4f  %.4f\t%.4f (%.4f)\t%.1f  %.1f", x1, y1, x2, y2, dikte, Clearance / 100000.0,
		        Angle1, Angle2);
		sprintf(Val7, SC(1044, "%.4f  %.4f\t%.4f  %.4f\t%.4f\tLength  %.4f, Angle %.1f"), x1, y1, x2, y2, dikte,
		        Length / 100000.0, Angle * 180.0 / PI);
		sprintf(Val7a, SC(1105, "%.4f  %.4f\t%.4f  %.4f\t%.4f (%.4f)\tLength  %.4f, Angle %.1f"), x1, y1, x2, y2, dikte,
		        Clearance / 100000.0, Length / 100000.0, Angle * 180.0 / PI);

		if ((Object->Info & OBJECT_FILLED) == 0)
			sprintf(Val8, "%.4f  %.4f\t%.4f  %.4f\t%.4f", x1, y1, x2, y2, dikte);
		else
			sprintf(Val8, "%.4f  %.4f\t%.4f  %.4f\t\t\t", x1, y1, x2, y2);
	}

	GetLayerTextObjects(Object->Layer, LayerText, 0);

	switch (Object->ObjectType)
	{
	case OBJECT_LINE:
		strcpy(ObjectText, SC(462, "Line"));

		if (!CheckIfLayerHasObjectWithClearances(Layer))
			sprintf(str, "%s\t%s\t%s\t%s", ObjectText, LayerText, Val7, ObjectNrStr);
		else
			sprintf(str, "%s\t%s\t%s\t%s", ObjectText, LayerText, Val7a, ObjectNrStr);

		if (AddToMessageBuf(str) != 0)
			return;

		break;

	case OBJECT_RECT:
		if ((Object->Info & OBJECT_FILLED) == 0)
			strcpy(ObjectText, SC(583, "Rect"));
		else
			strcpy(ObjectText, SC(585, "Rect pad"));

		sprintf(str, "%s\t%s\t%s\t%s", ObjectText, LayerText, Val8, ObjectNrStr);

		if (AddToMessageBuf(str) != 0)
			return;

		break;

	case OBJECT_CIRCLE:
		if ((Object->Info & OBJECT_FILLED) == 0)
			strcpy(ObjectText, SC(1045, "Circle"));
		else
			strcpy(ObjectText, SC(586, "Circle pad"));

		sprintf(str, "%s\t%s\t%s\t%s", ObjectText, LayerText, Val2, ObjectNrStr);

		if (AddToMessageBuf(str) != 0)
			return;

		break;

	case OBJECT_ARC:
		if ((Object->Layer == DRILL_LAYER) || (Object->Layer == DRILL_UNPLATED_LAYER))
		{
			strcpy(ObjectText, SC(1045, "Circle"));
			sprintf(str, "%s\t%s\t%s    \t%s", ObjectText, LayerText, Val2, ObjectNrStr);
		}
		else
		{
			if ((Object->Info & OBJECT_FILLED) == 0)
			{
				if ((InRange(Object->x3, Object->x4)) && (InRange(Object->y3, Object->y4))
				        && (InRange(Object->x2, Object->y2)))
				{
					strcpy(ObjectText, SC(1045, "Circle"));
					sprintf(str, "%s\t%s\t%s    \t%s", ObjectText, LayerText, Val2, ObjectNrStr);
				}
				else
				{
					strcpy(ObjectText, SC(588, "Arc"));

					if (!CheckIfLayerHasObjectWithClearances(Layer))
						sprintf(str, "%s\t%s\t%s    \t%s", ObjectText, LayerText, Val6, ObjectNrStr);
					else
						sprintf(str, "%s\t%s\t%s    \t%s", ObjectText, LayerText, Val6a, ObjectNrStr);
				}
			}
			else
			{
				strcpy(ObjectText, SC(586, "Circle pad"));
				sprintf(str, "%s\t%s\t%s    \t%s", ObjectText, LayerText, Val2, ObjectNrStr);
			}
		}

		if (AddToMessageBuf(str) != 0)
			return;

		break;

	/*
	    case OBJECT_TEXT:
	      strcpy(ObjectText,"TEXT");
	      sprintf(str,"%s\t%s\t%s\t%s\t%s",ObjectText,LayerText,Val4a,(LPSTR)Object->TraceNr,ObjectNrStr);
	      if (AddToMessageBuf(str)!=0) return;
	      break;
	*/
	case OBJECT_TEXT:
	case OBJECT_TEXT2:
		strcpy(ObjectText, SC(589, "Text"));
		strcpy(str2, (LPSTR) Object->TraceNr);
		lengte = strlen(str2);

		for (cnt = 0; cnt < lengte; cnt++)
		{
			if ((str2[cnt] == '\r') || (str2[cnt] == '\n'))
				str2[cnt] = '\x01';
		}

		sprintf(str, "%s\t%s\t%s\t%s\t%s", ObjectText, LayerText, Val5, str2, ObjectNrStr);

		if (AddToMessageBuf(str) != 0)
			return;

		break;

	case OBJECT_POLYGON:
		strcpy(ObjectText, SC(1046, "Polygon"));
		sprintf(str, SC(1047, "%s\t%s\t\t\t\t%d vertices\t\t%s"), ObjectText, LayerText, Object->TraceNr, ObjectNrStr);

		if (AddToMessageBuf(str) != 0)
			return;

		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetInfoSelectedObjects(int32 mode)
{
	int32 cnt, cnt2, Layer, MemPos, NetNr, TraceInfo, ViaInfo, CompInfo, ShapeNr, ObjectsSelected, UserCutOuts,
	      UserInternalCutOuts, ThermalReliefCutOuts;
	double Rotation;
	TraceRecord *Trace;
	CompRecord *Comp;
	ViaRecord *Via;
	NetRecord *Net;
	AreaFillRecord *AreaFill;
	ObjectRecord SpecialObject;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	PolygonRecord *FirstPolygon;
	ShapeRecord *Shape;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING],
	     str6[MAX_LENGTH_STRING], str7[MAX_LENGTH_STRING], str8[MAX_LENGTH_STRING], str9[MAX_LENGTH_STRING];
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;
#ifdef _DEBUG
	char str3[MAX_LENGTH_STRING];
#endif

	if (SelectionMode == DRAG_TRACES_VIAS_COMPS_MODE)
		return;

	MessageBufPos = 0;
	ObjectsSelected = 0;
	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ShapeNr = (int32) Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			strcpy(str5, Shape->ShapeName);
			struprUTF8(str5);
			strcpy(str7, Comp->Name);
			struprUTF8(str7);
			strcpy(str4, Comp->Value);
			struprUTF8(str4);
			strcpy(str6, Comp->PartNr);
			struprUTF8(str6);
			PrintValue(Comp->CompOriginX, str8, 0);
			PrintValue(Comp->CompOriginY, str9, 1);
			Rotation = Comp->Rotation;

			if ((Comp->CompMode & 8) == 0)
			{
				sprintf(str, SC(1048, "%s\tTop\t%s\t%s\t%s\t%s\t%s\t%.1f"), str7, str5, str6, str4, str8, str9,
				        Rotation);
			}
			else
			{
				sprintf(str, SC(1049, "%s\tBottom\t%s\t%s\t%s\t%s\t%s\t%.1f"), str7, str5, str6, str4, str8, str9,
				        Rotation);
			}

			if (AddToMessageBuf(str) != 0)
				return;

//      sprintf(str,"\t\t%s,%s\t%i",
//      if (AddToMessageBuf(str)!=0) return;
			cnt2++;
			ObjectsSelected++;
		}
	}

	memset(&str, '-', 190);
	str[190] = 0;
//  if (AddToMessageBuf(str)!=0) return;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectsSelected++;
				PrintTrace(Trace, str, TRACE_VER, Layer, cnt);

				if (AddToMessageBuf(str) != 0)
					return;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectsSelected++;
				PrintTrace(Trace, str, TRACE_HOR, Layer, cnt);

				if (AddToMessageBuf(str) != 0)
					return;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectsSelected++;
				PrintTrace(Trace, str, TRACE_DIAG1, Layer, cnt);

				if (AddToMessageBuf(str) != 0)
					return;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if ((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectsSelected++;
				PrintTrace(Trace, str, TRACE_DIAG2, Layer, cnt);

				if (AddToMessageBuf(str) != 0)
					return;
			}
		}
	}

	memset(&str, '-', 190);
	str[190] = 0;
//  if (AddToMessageBuf(str)!=0) return;

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			sprintf(str, SC(1050, "Via\t\t"));
			NetNr = Via->NetNr;
			Net = &((*Nets)[NetNr]);
			strcat(str, Net->Name);
			strcat(str, "\t");
			PrintValue(Via->X, str2, 0);
			strcat(str, str2);
			strcat(str, "  ");
			PrintValue(Via->Y, str2, 0);
			strcat(str, str2);
			strcat(str, SC(1051, "\t\tPadsize\t"));
			PrintValue(Via->ThickNess, str2, 0);
			strcat(str, str2);
			strcat(str, SC(1052, "\tDrill\t"));
			PrintValue(Via->DrillThickNess, str2, 0);
			strcat(str, str2);
			strcat(str, SC(1053, "\tClearance\t"));
			PrintValue(Via->Clearance, str2, 1);
			strcat(str, str2);

			if (AddToMessageBuf(str) != 0)
				return;

			ObjectsSelected++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SpecialObject.x1 = ObjectLine->X1;
			SpecialObject.y1 = ObjectLine->Y1;
			SpecialObject.x2 = ObjectLine->X2;
			SpecialObject.y2 = ObjectLine->Y2;
			SpecialObject.Thickness = ObjectLine->LineThickNess;
			SpecialObject.Layer = ObjectLine->Layer;
			SpecialObject.NetNr = ObjectLine->NetNr;
			SpecialObject.Clearance = ObjectLine->Clearance;
			SpecialObject.ObjectType = OBJECT_LINE;
			ObjectsInfo(&SpecialObject);
			ObjectsSelected++;
		}
	}


	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SpecialObject.x1 = ObjectRect->CentreX;
			SpecialObject.y1 = ObjectRect->CentreY;
			SpecialObject.x2 = ObjectRect->Width;
			SpecialObject.y2 = ObjectRect->Height;
			SpecialObject.Thickness = ObjectRect->LineThickNess;
			SpecialObject.Info = ObjectRect->Info;
			SpecialObject.Layer = ObjectRect->Layer;
			SpecialObject.NetNr = ObjectRect->NetNr;
			SpecialObject.ObjectType = OBJECT_RECT;
			ObjectsInfo(&SpecialObject);
			ObjectsSelected++;
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      SpecialObject.x1=ObjectCircle->CentreX;
	      SpecialObject.y1=ObjectCircle->CentreY;
	      SpecialObject.x2=ObjectCircle->Diam;
	      SpecialObject.x3=ObjectCircle->LineThickNess;
	      SpecialObject.Layer=ObjectCircle->Layer;
	      SpecialObject.NetNr=ObjectCircle->NetNr;
	      SpecialObject.Info=ObjectCircle->Info;
	      SpecialObject.ObjectType=OBJECT_CIRCLE;
	      ObjectsInfo(&SpecialObject);
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SpecialObject.x1 = ObjectArc->CentreX;
			SpecialObject.y1 = ObjectArc->CentreY;
			SpecialObject.x2 = ObjectArc->Width;
			SpecialObject.y2 = ObjectArc->Height;
			SpecialObject.Thickness = ObjectArc->LineThickNess;
			SpecialObject.Layer = ObjectArc->Layer;
			SpecialObject.NetNr = ObjectArc->NetNr;
			SpecialObject.ObjectType = OBJECT_ARC;
			SpecialObject.Clearance = ObjectArc->Clearance;
			SpecialObject.Info = ObjectArc->Info;
			SpecialObject.x3 = ObjectArc->StartDiffX;
			SpecialObject.y3 = ObjectArc->StartDiffY;
			SpecialObject.x4 = ObjectArc->EndDiffX;
			SpecialObject.y4 = ObjectArc->EndDiffY;
			ObjectsInfo(&SpecialObject);
			ObjectsSelected++;
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if ((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      SpecialObject.x1=ObjectText->X;
	      SpecialObject.y1=ObjectText->Y;
	      SpecialObject.x2=ObjectText->FontHeight;
	      SpecialObject.Info2=(ObjectText->TextMode >> 8) & 0x03;
	      SpecialObject.NetNr=ObjectText->NetNr;
	      if (((ObjectText->TextMode & 0x10) == 0x10)
	         &&
	         (SpecialObject.Info2>=1)) {
	        SpecialObject.Info2^=2;
	      }
	      SpecialObject.x3=ObjectText->LineThickNess;
	      SpecialObject.Layer=ObjectText->Layer;
	      SpecialObject.TraceNr=(int32)ObjectText->Text;
	      SpecialObject.ObjectType=OBJECT_TEXT;
	      ObjectsInfo(&SpecialObject);
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SpecialObject.x1 = ObjectText2->X;
			SpecialObject.y1 = ObjectText2->Y;
			SpecialObject.x2 = ObjectText2->FontHeight;
			SpecialObject.y2 = ObjectText2->Rotation;
			SpecialObject.Info2 = (ObjectText2->TextMode >> 8) & 0x03;
			SpecialObject.NetNr = ObjectText2->NetNr;
			SpecialObject.Thickness = ObjectText2->LineThickNess;
			SpecialObject.Layer = ObjectText2->Layer;
			SpecialObject.TraceNr = (intptr_t) ObjectText2->Text;
			SpecialObject.ObjectType = OBJECT_TEXT;
			ObjectsInfo(&SpecialObject);
			ObjectsSelected++;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SpecialObject.x1 = 0.0;
			SpecialObject.y1 = 0.0;
			SpecialObject.x2 = 0.0;
			SpecialObject.y2 = 0.0;
			SpecialObject.x3 = 0.0;
			SpecialObject.Info2 = 0;
			SpecialObject.NetNr = ObjectPolygon->NetNr;
			SpecialObject.Layer = ObjectPolygon->Layer;
			SpecialObject.TraceNr = ObjectPolygon->NrVertices;
			SpecialObject.ObjectType = OBJECT_POLYGON;
			ObjectsInfo(&SpecialObject);
			ObjectsSelected++;
		}
	}

// PolygonType bit 0 = 0  -> polygon deletion because of copper
// PolygonType bit 0 = 1  -> polygon deletion because of user deletion/thermal relief
// PolygonType bit 2 = 0  -> polygon deletion because of thermal relief
// PolygonType bit 2 = 1  -> polygon deletion because of user deletion
	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			GetLayerText(AreaFill->Layer, str2, 4);

			if ((AreaFill->Info & (POWERPLANE)) == 0)
			{
				FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				if (AreaFill->NetNr >= 0)
					Net = &((*Nets)[AreaFill->NetNr]);
				else
					Net = &EmptyNet;

				UserCutOuts = 0;
				UserInternalCutOuts = 0;
				ThermalReliefCutOuts = 0;

				if ((FirstPolygon->PolygonType & 2) == 2)
				{
					ObjectsSelected++;
					sprintf(str, SC(1012, "Areafill\t%s\t%s\t%i"), str2, Net->Name, FirstPolygon->NrVertices);
#ifdef _DEBUG
					AreaPos = (uint8 *) AreaFill;
					DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
					PolygonPos = (uint8 *) DrawPolygon;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;

					for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
					{
						if ((DrawPolygon->PolygonType & 8) == 8)
							UserCutOuts++;

						if ((DrawPolygon->PolygonType & 5) == 5)
							UserInternalCutOuts++;

						if ((DrawPolygon->PolygonType & 5) == 1)
							ThermalReliefCutOuts++;

						PolygonPos += MemSizePolygon(DrawPolygon);
						DrawPolygon = (PolygonRecord *) PolygonPos;
					}


					sprintf(str3, "\t%i\t%i [ User,internal,thermal %d,%d,%d ]  %d", AreaFill->MemSize,
					        AreaFill->NrPolygons - 1, UserCutOuts, UserInternalCutOuts, ThermalReliefCutOuts, cnt);
					strcat(str, str3);


#endif

					if (AddToMessageBuf(str) != 0)
						return;
				}
				else
				{
					AreaPos = (uint8 *) AreaFill;
					DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
					PolygonPos = (uint8 *) DrawPolygon;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;

					for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
					{
						if ((DrawPolygon->PolygonType & 2) == 2)
						{
							sprintf(str, SC(1013, "Areafill cut out\t%s\t%s\t%i"), str2, Net->Name,
							        DrawPolygon->NrVertices);

							if ((DrawPolygon->PolygonType & 5) == 1)
								strcat(str, SC(1014, " (Thermal relief)"));

							if ((DrawPolygon->PolygonType & 5) == 5)
								strcat(str, SC(1054, " (User encapsulated cutout)"));

							if ((DrawPolygon->PolygonType & 8) == 8)
								strcat(str, SC(1055, " (User cutout)"));

							if (AddToMessageBuf(str) != 0)
								return;
						}

						PolygonPos += MemSizePolygon(DrawPolygon);
						DrawPolygon = (PolygonRecord *) PolygonPos;
					}
				}
			}
			else
			{
				if (AreaFill->NetNr >= 0)
					Net = &((*Nets)[AreaFill->NetNr]);
				else
					Net = &EmptyNet;

				FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

				if ((FirstPolygon->PolygonType & 2) == 2)
				{
					sprintf(str, SC(1056, "Powerplane\t%s\t%s\t%i"), str2, Net->Name, FirstPolygon->NrVertices);

					if (AddToMessageBuf(str) != 0)
						return;
				}
				else
				{

					AreaPos = (uint8 *) AreaFill;
					DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
					PolygonPos = (uint8 *) DrawPolygon;
					PolygonPos += MemSizePolygon(DrawPolygon);
					DrawPolygon = (PolygonRecord *) PolygonPos;

					for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
					{
						if ((DrawPolygon->PolygonType & 2) == 2)
						{
							GetLayerText(AreaFill->Layer, str2, 0);
							sprintf(str, SC(1057, "Powerplane cut out\t%s\t%s\t%i"), str2, Net->Name,
							        DrawPolygon->NrVertices);

							if (AddToMessageBuf(str) != 0)
								return;
						}

						PolygonPos += MemSizePolygon(DrawPolygon);
						DrawPolygon = (PolygonRecord *) PolygonPos;
					}

				}
			}
		}
	}

	if (MessageBufPos != 0)
	{
		switch (SelectionMode)
		{
		case MOVE_COMPONENTS_MODE:	// Components
		case MOVE_COMPONENT_REFERENCES_MODE:	// Component references
		case MOVE_COMPONENT_VALUES_MODE:	// Component values
			MessageDialog(SC(1093, "Comp\tLayer\tGeometry\tPartnr\tValue\tOrigin\t\tRotation"), 3, ObjectsSelected);
			break;

		case OBJECTS_MODE:		// Other objects
//        MessageDialog(SC(617,"Info"),6);
			MessageDialog(SC(1058, "Object\tLayer\tOrigin\tSize\tThick\tExtra"), 6, ObjectsSelected);
			break;

		case AREAFILLS_MODE:	// areafills
#ifdef _DEBUG
			MessageDialog("Object\tLayer\tNet\tNr vertices\tMemsize\tNr cut outs [ types ]", 7, ObjectsSelected);
#else
			MessageDialog(SC(1096, "Object\tLayer\tNet\tNr vertices\tCut out type"), 7, 0);
#endif
			break;

		case MOVING_TRACES_VIAS_MODE:	// traces/vias
			MessageDialog(SC(1059, "Object\tLayer\tNet\tOrigin\t\tSize\t\t\t\tClearance"), 8, ObjectsSelected);
			break;
		}

		DeAllocateMemMessageBuf();
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
