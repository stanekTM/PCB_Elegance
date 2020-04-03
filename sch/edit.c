/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edit.c
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
#include "string.h"
#include "edit.h"
#include "edit2.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "sch.h"
#include "calc.h"
#include "files.h"
#include "toets.h"
#include "calcdef.h"
#include "math.h"
#include "calc2.h"
#include "graphics.h"
#include "calcrect.h"
#include "mainloop.h"
#include "insdel.h"
#include "select.h"
#include "dialogs.h"
#include "stdio.h"
#include "resource.h"
#include "help.h"


double BetweenPointX, BetweenPointY, MouseX, MouseY, lastx1, lasty1, lastx2, lasty2;

int32 OldDir, ArcMode, LineOldX1, LineOldX2, ok;
int32 LineHeight = 7;
WireRecord FirstWire, SecondWire, *FoundWire;
BusRecord FirstBus, SecondBus, *FoundBus;
int32 FirstWireOk, SecondWireOk;
int32 CopyObjectsFromClipBoard = 0;
char ObjectTextLine[MAX_LENGTH_STRING];
int32 AddLabelsStartNr = 0;
int32 AddLabelsStep = 1;
int32 AddLabelsAddY = -1;
int32 FoundWireNr, FoundBusNr;
int32 WireChanged, DisplayObjectOnEscape;

DesignRecord BackupDesign;
SymbolRecord BackupDesignSymbol;


char AddLabelTextLine[100] = "";
char AddLabelsTextLine[100] = "";

extern uint32 ClipBoardSchematicID;
extern uint32 ClipBoardSelectionsSchematicID;


ObjectNumbersRecord ObjectNumbers = { 0.0, 0.0, 1, 1, 1 };

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddNetLabel(int32 Mode);

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawTryingWire(double CurrentX, double CurrentY, int32 Mode)
/*
      0

      |
      |
      |
3 ----+---- 1
      |
      |
      |

      2

*/
{
	double divx, divy, x1, y1, x2, y2, x3, y3;
	int32 ok;

	if ((Mode & 4) == 0)
	{
		DrawCrossHair(0);
		return;
	}

	StartDrawingEditingWindow();

	if ((Mode & 0x20000) == 0)
	{
		if ((FoundWireNr != -1) || (FoundBusNr != -1))
		{
			if ((FoundWire == NULL) || (FoundBus == NULL))
			{
				if (FoundWireNr != -1)
				{
					FoundWire = &((*Wires)[FoundWireNr]);
					x1 = FoundWire->X1;
					y1 = FoundWire->Y1;
					x2 = FoundWire->X2;
					y2 = FoundWire->Y2;
				}
				else
				{
					FoundBus = &((*Busses)[FoundBusNr]);
					x1 = FoundBus->X1;
					y1 = FoundBus->Y1;
					x2 = FoundBus->X2;
					y2 = FoundBus->Y2;
				}

				if (CalcLengthLine(MouseX, MouseY, x1, y1) < CalcLengthLine(MouseX, MouseY, x2, y2))
				{
					CurrentX2 = x2;
					CurrentY2 = y2;
				}
				else
				{
					CurrentX2 = x1;
					CurrentY2 = y1;
				}
			}

			if ((Mode & 0x10000) == 0x10000)
				SetBackGroundActive(0);

			if (FoundWireNr != -1)
				DrawWire(FoundWire, 0.0, 0.0, 1);
			else
				DrawBus(FoundBus, 0.0, 0.0, 1);

			BackGroundActive = 0;
			ok = 1;
		}
	}

	SetROP2(OutputDisplay, R2_XORPEN);

	switch (Mode & 3)
	{
	case 0:
		InitDrawingWires(Mult(STANDARD_WIRE_THICKNESS));
		break;

	case 1:
		InitDrawingBusses(Mult(STANDARD_BUS_THICKNESS));
		break;

	case 2:
		InitDrawingObjectLines(Mult(NewObjectLine.Thickness));
		break;
	}

	if ((Mode & 3) < 2)
	{
		divx = CurrentX - CurrentX2;
		divy = CurrentY - CurrentY2;

		x1 = CurrentX2;
		y1 = CurrentY2;


		switch (OldDir)
		{
		case 0:
			if (X1SmallerThenX2(divy, 0))
			{
				if (X1SmallerThenX2(divx, 0))
					OldDir = 3;
				else if (X1GreaterThenX2(divx, 0))
					OldDir = 1;
				else
					OldDir = 2;
			}

			break;

		case 2:
			if (X1GreaterThenX2(divy, 0))
			{
				if (X1SmallerThenX2(divx, 0))
					OldDir = 3;
				else if (X1GreaterThenX2(divx, 0))
					OldDir = 1;
				else
					OldDir = 0;
			}

			break;

		case 1:
			if (X1SmallerThenX2(divx, 0))
			{
				if (X1SmallerThenX2(divy, 0))
					OldDir = 2;
				else if (X1GreaterThenX2(divy, 0))
					OldDir = 0;
				else if (X1SmallerThenX2(divy, 0))
					OldDir = 3;
			}

			break;

		case 3:
			if (X1GreaterThenX2(divx, 0))
			{
				if (X1SmallerThenX2(divy, 0))
					OldDir = 2;
				else if (X1GreaterThenX2(divy, 0))
					OldDir = 0;
				else if (X1SmallerThenX2(divy, 0))
					OldDir = 1;
			}

			break;

		case -1:
			if ((NotInRange(divx, 0)) || (NotInRange(divy, 0)))
			{
				if (X1GreaterThenX2(divx, 0))
				{
					if (X1GreaterThenX2(divy, 0))
					{
						if (X1GreaterThenX2(divx, divy))
							OldDir = 1;
						else
							OldDir = 0;
					}
					else
					{
						if (X1SmallerThenX2(divy, 0))
						{
							if (X1GreaterThenX2(divx, divy))
								OldDir = 1;
							else
								OldDir = 2;
						}
						else
						{
							if (X1GreaterThenX2(divx, 0))
								OldDir = 1;
							else
								OldDir = 3;
						}
					}
				}
				else
				{
					if (X1SmallerThenX2(divx, 0))
					{
						if (X1GreaterThenX2(divy, 0))
						{
							if (X1GreaterThenX2(divx, divy))
								OldDir = 3;
							else
								OldDir = 0;
						}
						else
						{
							if (X1SmallerThenX2(divy, 0))
							{
								if (X1GreaterThenX2(divx, divy))
									OldDir = 3;
								else
									OldDir = 2;
							}
							else
							{
								if (X1GreaterThenX2(divx, 0))
									OldDir = 1;
								else
									OldDir = 3;
							}
						}
					}
					else
					{
						if (X1GreaterThenX2(divy, 0))
							OldDir = 0;
						else
							OldDir = 2;
					}
				}
			}
		}

		if ((InRange(divx, 0)) && (InRange(divy, 0)))
			OldDir = -1;

		x3 = CurrentX;
		y3 = CurrentY;
		SecondWire.X2 = (float) CurrentX;
		SecondWire.Y2 = (float) CurrentY;
		FirstWire.X1 = (float) CurrentX2;
		FirstWire.Y1 = (float) CurrentY2;
		SecondBus.X2 = (float) CurrentX;
		SecondBus.Y2 = (float) CurrentY;
		FirstBus.X1 = (float) CurrentX2;
		FirstBus.Y1 = (float) CurrentY2;
		FirstWireOk = 0;
		SecondWireOk = 0;

		switch (OldDir)
		{
		case 0:
		case 2:
			x2 = x1;
			y2 = CurrentY;
			FirstWire.X2 = (float) CurrentX2;
			FirstWire.Y2 = (float) CurrentY;
			SecondWire.X1 = (float) CurrentX2;
			SecondWire.Y1 = (float) CurrentY;
			FirstBus.X2 = (float) CurrentX2;
			FirstBus.Y2 = (float) CurrentY;
			SecondBus.X1 = (float) CurrentX2;
			SecondBus.Y1 = (float) CurrentY;

			if ((NotInRange(FirstWire.X1, FirstWire.X2)) || (NotInRange(FirstWire.Y1, FirstWire.Y2)))
			{
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
				FirstWireOk = 1;
			}


			if ((NotInRange(x2, x3)) || (NotInRange(y2, y3)))
			{
				DrawLine(MultX(x2), MultY(y2), MultX(x3), MultY(y3));
				BetweenPointX = x2;
				BetweenPointY = y2;
				SecondWireOk = 1;
			}

			break;

		case 1:
		case 3:
			x2 = CurrentX;
			y2 = CurrentY2;
			FirstWire.X2 = (float) CurrentX;
			FirstWire.Y2 = (float) CurrentY2;
			SecondWire.X1 = (float) CurrentX;
			SecondWire.Y1 = (float) CurrentY2;
			FirstBus.X2 = (float) CurrentX;
			FirstBus.Y2 = (float) CurrentY2;
			SecondBus.X1 = (float) CurrentX;
			SecondBus.Y1 = (float) CurrentY2;

			if ((NotInRange(FirstWire.X1, FirstWire.X2)) || (NotInRange(FirstWire.Y1, FirstWire.Y2)))
			{
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));
				FirstWireOk = 1;
			}

			if ((x2 != x3) || (y2 != y3))
			{
				DrawLine(MultX(x2), MultY(y2), MultX(x3), MultY(y3));
				BetweenPointX = x2;
				BetweenPointY = y2;
				SecondWireOk = 1;
			}

			break;
		}
	}
	else
	{
		NewObjectLine.X1 = (float) CurrentX2;
		NewObjectLine.Y1 = (float) CurrentY2;
		NewObjectLine.X2 = (float) CurrentX;
		NewObjectLine.Y2 = (float) CurrentY;
		DrawObjectLine(&NewObjectLine, 0.0, 0.0, 0);

		if (GetDimensionTextFromLine(CurrentX2, CurrentY2, CurrentX, CurrentY, &NewObjectText, NewObjectLine.LineMode)
		        == 0)
			DrawObjectText(&NewObjectText, 0.0, 0.0, 0);
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingWireBus(double CurrentX, double CurrentY, int32 Count, int32 Mode)
{
	int32 WireChanged, cnt, ok, EndPointReached, PointBetweenLines;
	double x1, y1, x2, y2, Distance, MinDistance;
	WireRecord *Wire;
	BusRecord *Bus;

	MinDistance = 1e9;

	if ((Mode & 4) == 4)
	{
		if (FoundWireNr != -1)
		{
			ZeroUnusedObjects(0);
			Wire = &((*Wires)[FoundWireNr]);
			Wire->Info |= OBJECT_NOT_VISIBLE;
			Wire->DeleteNr = (int16) LastActionNr;
		}

		if (FoundBusNr != -1)
		{
			ZeroUnusedObjects(0);
			Bus = &((*Busses)[FoundBusNr]);
			Bus->Info |= OBJECT_NOT_VISIBLE;
			Bus->DeleteNr = (int16) LastActionNr;
		}

		FoundWireNr = -1;
		FoundBusNr = -1;
		EndPointReached = WireBusEndPointReached(CurrentX, CurrentY, Mode & 3);

		if (FirstWireOk)
		{
			WireChanged = 0;

			if (!SecondWireOk)
				PointBetweenLines = TestLineConnectedToCircle(lastx1, lasty1, lastx2, lasty2, CurrentX, CurrentY, 0.04);
			else
			{
				PointBetweenLines =
				    TestLineConnectedToCircle(lastx1, lasty1, lastx2, lasty2, BetweenPointX, BetweenPointY, 0.04);
			}

			if ((Mode & 3) == 0)
			{
// ********************************************************************************
				if ((Count > 0) && (PointBetweenLines))
				{
					if (((InRange(lastx2, FirstWire.X1)) && (InRange(lasty2, FirstWire.Y1)))
					        || ((InRange(lastx2, FirstWire.X2)) && (InRange(lasty2, FirstWire.Y2))))
					{
						FirstWire.X1 = (float) lastx1;
						FirstWire.Y1 = (float) lasty1;
					}
					else
					{
						FirstWire.X1 = (float) lastx2;
						FirstWire.Y1 = (float) lasty2;
					}

					if (!SecondWireOk)
					{
						FirstWire.X2 = (float) CurrentX;
						FirstWire.Y2 = (float) CurrentY;
					}
					else
					{
						FirstWire.X2 = (float) BetweenPointX;
						FirstWire.Y2 = (float) BetweenPointY;
					}

					WireChanged = 1;
					EndPointReached = 0;
				}

				if (WireChanged)
					CommandAddTryingWire(&FirstWire, 4);
				else
					CommandAddTryingWire(&FirstWire, 0);

				lastx1 = FirstWire.X1;
				lasty1 = FirstWire.Y1;
				lastx2 = FirstWire.X2;
				lasty2 = FirstWire.Y2;
			}
			else
			{
				if ((Count > 0) && (PointBetweenLines))
				{
					if (((InRange(lastx2, FirstBus.X1)) && (InRange(lasty2, FirstBus.Y1)))
					        || ((InRange(lastx2, FirstBus.X2)) && (InRange(lasty2, FirstBus.Y2))))
					{
						FirstBus.X1 = (float) lastx1;
						FirstBus.Y1 = (float) lasty1;
					}
					else
					{
						FirstBus.X1 = (float) lastx2;
						FirstBus.Y1 = (float) lasty2;
					}

					if (!SecondWireOk)
					{
						FirstBus.X2 = (float) CurrentX;
						FirstBus.Y2 = (float) CurrentY;
					}
					else
					{
						FirstBus.X2 = (float) BetweenPointX;
						FirstBus.Y2 = (float) BetweenPointY;
					}

					WireChanged = 1;
					EndPointReached = 0;
				}

				if (WireChanged)
					CommandAddTryingBus(&FirstBus, 4);
				else
					CommandAddTryingBus(&FirstBus, 0);

				lastx1 = FirstBus.X1;
				lasty1 = FirstBus.Y1;
				lastx2 = FirstBus.X2;
				lasty2 = FirstBus.Y2;
			}
		}

// ********************************************************************************
		if (SecondWireOk)
		{
			WireChanged = 0;
			PointBetweenLines = TestLineConnectedToCircle(lastx1, lasty1, lastx2, lasty2, CurrentX, CurrentY, 0.04);

			if ((Mode & 3) == 0)
			{
				if ((Count > 0) && (PointBetweenLines))
				{
					if (((InRange(lastx2, FirstWire.X1)) && (InRange(lasty2, FirstWire.Y1)))
					        || ((InRange(lastx2, FirstWire.X2)) && (InRange(lasty2, FirstWire.Y2))))
					{
						FirstWire.X1 = (float) lastx1;
						FirstWire.Y1 = (float) lasty1;
						FirstWire.X2 = (float) CurrentX;
						FirstWire.Y2 = (float) CurrentY;
						WireChanged = 1;
						EndPointReached = 0;
					}
					else
					{
						FirstWire.X1 = (float) lastx2;
						FirstWire.Y1 = (float) lasty2;
						FirstWire.X2 = (float) CurrentX;
						FirstWire.Y2 = (float) CurrentY;
						WireChanged = 1;
						EndPointReached = 0;
					}
				}

				if (WireChanged)
					CommandAddTryingWire(&SecondWire, 4);
				else
					CommandAddTryingWire(&SecondWire, 0);

				lastx1 = SecondWire.X1;
				lasty1 = SecondWire.Y1;
				lastx2 = SecondWire.X2;
				lasty2 = SecondWire.Y2;
			}
			else
			{
				if ((Count > 0) && (PointBetweenLines))
				{
					if (((InRange(lastx2, FirstBus.X1)) && (InRange(lasty2, FirstBus.Y1)))
					        || ((InRange(lastx2, FirstBus.X2)) && (InRange(lasty2, FirstBus.Y2))))
					{
						FirstBus.X1 = (float) lastx1;
						FirstBus.Y1 = (float) lasty1;
						FirstBus.X2 = (float) CurrentX;
						FirstBus.Y2 = (float) CurrentY;
						WireChanged = 1;
						EndPointReached = 0;
					}
					else
					{
						FirstBus.X1 = (float) lastx2;
						FirstBus.Y1 = (float) lasty2;
						FirstBus.X2 = (float) CurrentX;
						FirstBus.Y2 = (float) CurrentY;
						WireChanged = 1;
						EndPointReached = 0;
					}
				}

				if (WireChanged)
					CommandAddTryingBus(&SecondBus, 4);
				else
					CommandAddTryingBus(&SecondBus, 0);

				lastx1 = SecondBus.X1;
				lasty1 = SecondBus.Y1;
				lastx2 = SecondBus.X2;
				lasty2 = SecondBus.Y2;
			}
		}

		if ((Mode & 7) == 4)
		{
			if (FirstWireOk)
			{
				PlaceJunctionOnEndPoint(FirstWire.X1, FirstWire.Y1, 0);
				PlaceJunctionOnEndPoint(FirstWire.X2, FirstWire.Y2, 0);
			}

			if (SecondWireOk)
			{
				PlaceJunctionOnEndPoint(SecondWire.X1, SecondWire.Y1, 0);
				PlaceJunctionOnEndPoint(SecondWire.X2, SecondWire.Y2, 0);
			}

			PlaceJunctionOnEndPoint(CurrentX, CurrentY, 0);
			PlaceJunctionOnEndPoint(CurrentX2, CurrentY2, 0);
		}

		if (EndPointReached)
		{
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}
	}
	else
	{
// ********************************************************************************
// ********************************************************************************
		MouseX = PixelToRealOffX(MousePosX);
		MouseY = PixelToRealOffY(DrawWindowMaxY - MousePosY);

		switch (Mode & 3)
		{
		case 0:				// Wires
			for (cnt = 0; cnt < Design.NrWires; cnt++)
			{
				Wire = &((*Wires)[cnt]);

				if ((Wire->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Wire->X1;
					y1 = Wire->Y1;
					x2 = Wire->X2;
					y2 = Wire->Y2;
					Distance = DistancePointToLine(MouseX, MouseY, x1, y1, x2, y2);

					if (Distance < MinDistance)
					{
						MinDistance = Distance;
						FoundWireNr = cnt;
					}
				}
			}

			if (MinDistance > 0.3)
				FoundWireNr = -1;

			break;

		case 1:				// Busses
			for (cnt = 0; cnt < Design.NrBusses; cnt++)
			{
				Bus = &((*Busses)[cnt]);

				if ((Bus->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Bus->X1;
					y1 = Bus->Y1;
					x2 = Bus->X2;
					y2 = Bus->Y2;
					Distance = DistancePointToLine(MouseX, MouseY, x1, y1, x2, y2);

					if (Distance < MinDistance)
					{
						MinDistance = Distance;
						FoundBusNr = cnt;
					}
				}
			}

			if (MinDistance > 0.3)
				FoundBusNr = -1;

			break;
		}

		ok = 1;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingObjectLine()
{
#ifdef _DEBUG

	if ((InRange(NewObjectLine.X1, NewObjectLine.X2)) && (InRange(NewObjectLine.Y1, NewObjectLine.Y2)))
		ok = 1;

#endif
	NewObjectLine.Info = 0;
	AddObjectLine(&NewObjectLine);

	if ((NewObjectLine.LineMode != 0) && (NewObjectText.Text[0] != 0))
		AddObjectText(&NewObjectText);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddWires(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY, x1, x2, y2;
	int32 Count, Mode2;
	int32 DisplayObjectOnEscape = 1;
	HMENU PopUpMenu;
	DrawXorFunctionRecord DrawXorFunction;

	PopUpMenu = CreatePopupMenu();
	AppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(265, "Escape"));


	lastx1 = 0.0;
	lasty1 = 0.0;
	lastx2 = 0.0;
	lasty2 = 0.0;
	SelectionEsc = 0;
	CurrentDrawMode = 1;
	SystemBusyMode = 1;

	memset(&FirstWire, 0, sizeof(WireRecord));
	memset(&SecondWire, 0, sizeof(WireRecord));
	memset(&FirstBus, 0, sizeof(BusRecord));
	memset(&SecondBus, 0, sizeof(BusRecord));
	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	NewObjectLine.Thickness = (float) STANDARD_LINE_THICKNESS;
	NewObjectText.FontHeight = (float) Design.DimensionHeight;
	NewObjectText.Thickness = (float) STANDARD_LINE_THICKNESS;
	MouseX = PixelToRealOffX(MousePosX);
	MouseY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
	CurrentX = AdjustToDrawGrid(MouseX);
	CurrentY = AdjustToDrawGrid(MouseY);

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;
	FoundWire = NULL;
	FoundBus = NULL;
	OldX = CurrentX;
	OldY = CurrentY;
	Count = 0;
	FoundWireNr = -1;
	FoundBusNr = -1;
	lastx1 = 100000000;

	if ((Mode & 4) == 0)
	{
		switch (Mode & 3)
		{
		case 0:
			sprintf(InfoStr, SC(266, "Drawing wires"));
			RedrawInfoStr(1);
			break;

		case 1:
			sprintf(InfoStr, SC(267, "Drawing busses"));
			RedrawInfoStr(1);
			break;

		case 2:
			if ((Mode & 0xf00) == 0)
				sprintf(InfoStr, SC(268, "Drawing lines"));
			else
			{
				if ((Mode & 0xf00) <= 0x300)
				{
					NewObjectLine.LineMode = (int16) ((Mode >> 8) & 3);
					sprintf(InfoStr, SC(269, "Drawing arrows"));
				}
				else
				{
					NewObjectLine.LineMode = (int16) ((Mode >> 8) & 15);
					sprintf(InfoStr, SC(270, "Drawing dimensions"));
				}
			}

			RedrawInfoStr(1);
			break;
		}
	}

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
	Mode2 = Mode | 0x10000;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingWire;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode2;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingWire(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				x1 = sqrt((CurrentX - CurrentX2) * (CurrentX - CurrentX2) +
				          (CurrentY - CurrentY2) * (CurrentY - CurrentY2));
				x2 = fabs(CurrentX - CurrentX2);
				y2 = fabs(CurrentY - CurrentY2);

				if ((Mode & 4) == 4)
				{
					sprintf(InfoStr, SC(271, "%.1f , %.1f  Length %.2f"), x2, y2, x1);
					RedrawInfoStr(1);
				}

				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingWire(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingWire(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingWire(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingWire(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingWire(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingWire(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingWire(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
		}

		if (CheckLeftButton())
		{
			if ((FoundWireNr != -1) || (FoundBusNr != -1))
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x20000);
			else
				DrawTryingWire(CurrentX, CurrentY, Mode);

			if ((Mode & 3) < 2)
				CommandAddTryingWireBus(CurrentX, CurrentY, Count, Mode);
			else
			{
				if ((NotInRange(NewObjectLine.X1, NewObjectLine.X2))
				        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
					CommandAddTryingObjectLine();

				if ((Mode & 4) == 4)
				{
					if (((Mode >> 3) & 3) != 0)
					{
						SelectionEsc = 1;
						DisplayObjectOnEscape = 0;
					}
				}
			}

			RePaint();
			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;
			OldX = CurrentX;
			OldY = CurrentY;
			OldDir = -1;
			CheckInputMessages(0);
			Count++;

			if ((Mode & 4) == 4)
				LastActionNr++;

			Mode |= 4;
			Mode2 = Mode | 0x10000;

			if (!SelectionEsc)
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40,
			               0, SCHWindow, NULL);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingWire(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				switch (Mode & 3)
				{
				case 0:
					Help("add_wire.htm", 0);
					break;

				case 1:
					Help("add_bus.htm", 0);
					break;

				case 2:
					Help("other_objects.htm", 0);
					break;
				}

				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingWire(CurrentX, CurrentY, Mode | 0x10000);
		}
	}

	if (DisplayObjectOnEscape)
		DrawTryingWire(CurrentX, CurrentY, Mode);

	UnClipMouseCursor();

	if (Count > 0)
		LastActionNr--;

	SystemBusyMode = 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingJunction(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	NewJunction.X = (float) CurrentX;
	NewJunction.Y = (float) CurrentY;
	DrawJunction(&NewJunction, 0.0, 0.0, 0);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingJunction()
{
	NewJunction.Info = 0;
	AddJunction(&NewJunction);
	RePaint();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddJunction(int32 Mode)
{
	int32 Count;
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;


	memset(&NewJunction, 0, sizeof(JunctionRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if (Mode == 1)
	{
		NewJunction.X = (float) CurrentX;
		NewJunction.Y = (float) CurrentY;
		CommandAddTryingJunction();
		return;
	}


	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	Mode = 0xfff;
	Count = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingJunction(CurrentX, CurrentY, Mode);
	SystemBusyMode = 2;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingJunction;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingJunction(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingJunction(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingJunction(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingJunction(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingJunction(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingJunction(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingJunction(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingJunction(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingJunction(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingJunction(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingJunction(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingJunction(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingJunction(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingJunction(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingJunction(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingJunction(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingJunction(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingJunction(CurrentX, CurrentY, Mode);
			CommandAddTryingJunction();

//      GetCursorPos(&MouseP);
//      SetCursorPos(MouseP.x,MouseP.y+(int16)Mult(1.0));
//      CurrentY-=1.0;
//      OldY=CurrentY;
			if ((Mode & 0xfff) != 0xfff)
				Mode++;

			Count++;
			LastActionNr++;
			DrawTryingJunction(CurrentX, CurrentY, Mode);
			CheckInputMessages(0);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingJunction(CurrentX, CurrentY, Mode);
			Mode ^= 0x10000;
			DrawTryingJunction(CurrentX, CurrentY, Mode);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingJunction(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help(0, 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingJunction(OldX, OldY, Mode);
		}
	}

	if (DisplayObjectOnEscape)
		DrawTryingJunction(CurrentX, CurrentY, Mode);

	UnClipMouseCursor();

	if (Count > 0)
		LastActionNr--;

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingOnePinNet(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	NewOnePinNet.X = (float) CurrentX;
	NewOnePinNet.Y = (float) CurrentY;
	DrawOnePinNet(&NewOnePinNet, 0.0, 0.0, 0);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingOnePinNet()
{
	NewOnePinNet.Info = 0;
	AddOnePinNet(&NewOnePinNet);
	RePaint();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddOnePinNet(int32 Mode)
{
	int32 Count;
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;


	memset(&NewOnePinNet, 0, sizeof(OnePinNetRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if (Mode == 1)
	{
		NewOnePinNet.X = (float) CurrentX;
		NewOnePinNet.Y = (float) CurrentY;
		CommandAddTryingOnePinNet();
		return;
	}


	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	Mode = 0xfff;
	Count = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
	SystemBusyMode = 2;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingOnePinNet;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingOnePinNet(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingOnePinNet(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingOnePinNet(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingOnePinNet(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingOnePinNet(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingOnePinNet(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingOnePinNet(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingOnePinNet(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			CommandAddTryingOnePinNet();

//      GetCursorPos(&MouseP);
//      SetCursorPos(MouseP.x,MouseP.y+(int16)Mult(1.0));
//      CurrentY-=1.0;
//      OldY=CurrentY;
			if ((Mode & 0xfff) != 0xfff)
				Mode++;

			Count++;
			LastActionNr++;
			DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			CheckInputMessages(0);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
			Mode ^= 0x10000;
			DrawTryingOnePinNet(CurrentX, CurrentY, Mode);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingOnePinNet(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help(0, 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingOnePinNet(OldX, OldY, Mode);
		}
	}

	if (DisplayObjectOnEscape)
		DrawTryingOnePinNet(CurrentX, CurrentY, Mode);

	UnClipMouseCursor();

	if (Count > 0)
		LastActionNr--;

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingBusConnection(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	NewBusConnection.X = (float) CurrentX;
	NewBusConnection.Y = (float) CurrentY;
	NewBusConnection.Alignment = (int16) (Mode << 14);
	DrawBusConnection(&NewBusConnection, 0.0, 0.0, 0);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingBusConnection(BusConnectionRecord * BusConnection)
{
	BusConnection->Info = 0;
	AddBusConnection(BusConnection);
	RePaint();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddBusConnection(int32 Mode)
{
	int32 Count;
	double OldX, OldY, CurrentX, CurrentY;
	POINT MouseP;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	memset(&NewBusConnection, 0, sizeof(BusConnectionRecord));
	NewBusConnection.TextX = 1.0;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	Mode = 0;
	Count = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingBusConnection(CurrentX, CurrentY, Mode);
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingBusConnection;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	SystemBusyMode = 3;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingBusConnection(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingBusConnection(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingBusConnection(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingBusConnection(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingBusConnection(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingBusConnection(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingBusConnection(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingBusConnection(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			CommandAddTryingBusConnection(&NewBusConnection);

			if ((Mode & 1) == 0)
			{
				CurrentY -= 1.0;
				GetCursorPos(&MouseP);
				SetCursorPos(MouseP.x, MouseP.y + (int16) Mult(1.0));
			}
			else
			{
				CurrentX -= 1.0;
				GetCursorPos(&MouseP);
				SetCursorPos(MouseP.x + (int16) Mult(1.0), MouseP.y);
			}

			OldX = CurrentX;
			OldY = CurrentY;
			Count++;
			LastActionNr++;
			DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			CheckInputMessages(0);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingBusConnection(CurrentX, CurrentY, Mode);
			Mode += 1;
			Mode &= 3;
			DrawTryingBusConnection(CurrentX, CurrentY, Mode);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingBusConnection(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("add_busconnection.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingBusConnection(CurrentX, CurrentY, Mode);
		}
	}

	if (DisplayObjectOnEscape)
		DrawTryingBusConnection(CurrentX, CurrentY, Mode);

	UnClipMouseCursor();

	if (Count > 0)
		LastActionNr--;

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingNetLabel(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	NewNetLabel.TextX = (float) (CurrentX - NewNetLabel.ConnectX);
	NewNetLabel.TextY = (float) (CurrentY - NewNetLabel.ConnectY);

	if (Mode == 1)
	{
		switch (NewNetLabel.Alignment)
		{
		case 0:
		case 6:
			NewNetLabel.TextY += (float) 0.3;
			break;

		case 0x100:
		case 0x106:
			NewNetLabel.TextX -= (float) 0.3;
			break;
		}
	}

	DrawNetLabel(&NewNetLabel, 0.0, 0.0, 4);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingNetLabel(NetLabelRecord * NetLabel)
{
	NetLabel->Info = 0;
	AddNetLabel(NetLabel);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddNetLabels(int32 Mode)
{
	int32 cnt, NrWiresSelected, NrBussesSelected, res, WiresSelected[256], BussesSelected[256];
	BusRecord *Bus;
	WireRecord *Wire;
	char TextLine[MAX_LENGTH_STRING];

	GetNrSelections();

	if ((InstancesSelected + BusConnectionsSelected + JunctionsSelected + NetLabelsSelected + ObjectLinesSelected +
	        ObjectRectsSelected + ObjectCirclesSelected + ObjectArcsSelected + ObjectTextsSelected +
	        GlobalConnectionsSelected) != 0)
	{
// Only wires and busses may selected
		return;
	}

	NrWiresSelected = 0;

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (NrWiresSelected < 256)
			{
				WiresSelected[NrWiresSelected] = cnt;
				Wire->Info &= ~OBJECT_SELECTED;
				NrWiresSelected++;
			}
		}
	}

	NrBussesSelected = 0;

//  InitDrawingBusses();
	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (NrBussesSelected < 256)
			{
				BussesSelected[NrBussesSelected] = cnt;
				Bus->Info &= ~OBJECT_SELECTED;
				NrBussesSelected++;
			}
		}
	}

	for (cnt = 0; cnt < NrWiresSelected; cnt++)
	{
		Wire = &((*Wires)[WiresSelected[cnt]]);
		Wire->Info |= OBJECT_SELECTED;
		CheckInputMessages(0);

		if ((res = AddNetlabelDialog(&NewNetLabel, 0)) == 1)
		{
			memmove(&NewWire, Wire, sizeof(WireRecord));
			NewNetLabel.ConnectX = Wire->X1;
			NewNetLabel.ConnectY = Wire->Y1;
			CommandAddNetLabel(0);

		}

		Wire = &((*Wires)[WiresSelected[cnt]]);
		Wire->Info &= ~OBJECT_SELECTED;
	}

	for (cnt = 0; cnt < NrBussesSelected; cnt++)
	{
		Bus = &((*Busses)[BussesSelected[cnt]]);
		Bus->Info |= OBJECT_SELECTED;
		DrawBus(Bus, 0.0, 0.0, 0);
		ExitDrawing();
		EndDrawingEditingWindow();
		CheckInputMessages(0);
		memset(&TextLine, 0, MAX_LENGTH_STRING);

		if ((res = AddNetlabelDialog(&NewNetLabel, 0)) == 1)
		{
			memmove(&NewBus, Bus, sizeof(BusRecord));
			NewNetLabel.ConnectX = Bus->X1;
			NewNetLabel.ConnectY = Bus->Y1;
			NewNetLabel.LabelType = 1;
			CommandAddNetLabel(1);
		}

		Bus = &((*Busses)[BussesSelected[cnt]]);
		Bus->Info &= ~OBJECT_SELECTED;
	}

	RePaint();

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddMultipleNetLabels(int32 Mode)
{
	int32 cnt, cnt2, NrWiresSelected, res, Found, Length, HighestCnt, LowestCnt, ok, count, count2, WiresSelected[256];
	double Highest, Lowest;
	char FirstText[100], LastText[100], HulpLine[100];
	int32 Check;
	WireRecord *Wire;

	GetNrSelections();

	if ((InstancesSelected + BusConnectionsSelected + BussesSelected + JunctionsSelected + NetLabelsSelected +
	        ObjectLinesSelected + ObjectRectsSelected + ObjectCirclesSelected + ObjectArcsSelected + ObjectTextsSelected +
	        GlobalConnectionsSelected) != 0)
	{
// Only wires may selected
		return;
	}

	HighestCnt = -1;
	Highest = -100000000.0;
	LowestCnt = -1;
	Lowest = 100000000.0;
	Check = 1;
	NrWiresSelected = 0;
	count2 = 0;

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (NrWiresSelected < 256)
			{
				WiresSelected[NrWiresSelected] = cnt;
//        Wire->Info&=~OBJECT_SELECTED;
//        DrawWire(Wire,0.0,0.0,0);
				NrWiresSelected++;

				if (NotInRange(Wire->Y1, Wire->Y2))
					Check = 0;

				if (Wire->Y1 > Highest)
				{
					Highest = Wire->Y1;
					HighestCnt = cnt;
				}

				if (Wire->Y2 < Lowest)
				{
					Lowest = Wire->Y1;
					LowestCnt = cnt;
				}
			}
		}
	}

//  ExitDrawing();
//  EndDrawingEditingWindow();

	if (!Check)
	{
		ok = 1;
		return;
	}

	AddLabelsStartNr = 0;
	AddLabelsStep = 1;
	AddLabelsAddY = -1;

//  strcpy(AddLabelsTextLine,"hallo");
	if ((res = AddNetlabelsDialog(AddLabelsTextLine, &AddLabelsStartNr, &AddLabelsStep)) == 1)
	{
		Length = strlen(AddLabelsTextLine);
		Found = Length;

		for (cnt = 0; cnt < Length; cnt++)
		{
			if (AddLabelsTextLine[cnt] == '#')
				Found = cnt;
		}

		memset(&FirstText, 0, 100);
		memset(&LastText, 0, 100);

		if (Found > 0)
			memmove(&FirstText, &AddLabelsTextLine, (int) Found);

		if (Found < Length - 1)
			memmove(&LastText, &AddLabelsTextLine[Found + 1], (int) (Length - Found - 1));

		if ((AddLabelsAddY >= -16) && (AddLabelsAddY <= 16) && (AddLabelsStep >= -1024) && (AddLabelsStep <= 1024))
		{
			if (AddLabelsAddY > 0)
			{
				count = AddLabelsStartNr;

				for (cnt = 0; cnt < NrWiresSelected; cnt++)
				{
					Lowest = 100000000.0;
					LowestCnt = -1;

					for (cnt2 = 0; cnt2 < NrWiresSelected; cnt2++)
					{
						if (WiresSelected[cnt2] > -1)
						{
							Wire = &((*Wires)[WiresSelected[cnt2]]);

							if (Wire->Y2 < Lowest)
							{
								Lowest = Wire->Y1;
								LowestCnt = cnt2;
							}
						}
					}

					if (LowestCnt > -1)
					{
						Wire = &((*Wires)[WiresSelected[LowestCnt]]);
						WiresSelected[LowestCnt] = -1;
						memset(&NewNetLabel, 0, sizeof(NetLabelRecord));
						memset(&HulpLine, 0, 100);
						sprintf(HulpLine, "%s%i%s", FirstText, count, LastText);
						memmove(&NewNetLabel.Name, &HulpLine, 35);
						NewNetLabel.ConnectX = Wire->X1;
						NewNetLabel.ConnectY = Wire->Y1;
						NewNetLabel.TextX = (float) 2.0;
						NewNetLabel.TextY = 0.0;
						count += AddLabelsStep;
						NewNetLabel.LabelType = 0;
						CommandAddTryingNetLabel(&NewNetLabel);
						count2++;
						LastActionNr++;
					}
				}

			}
			else
			{
				count = AddLabelsStartNr;

				for (cnt = 0; cnt < NrWiresSelected; cnt++)
				{
					HighestCnt = -1;
					Highest = -100000000.0;

					for (cnt2 = 0; cnt2 < NrWiresSelected; cnt2++)
					{
						if (WiresSelected[cnt2] > -1)
						{
							Wire = &((*Wires)[WiresSelected[cnt2]]);

							if (Wire->Y2 > Highest)
							{
								Highest = Wire->Y1;
								HighestCnt = cnt2;
							}
						}
					}

					if (HighestCnt > -1)
					{
						Wire = &((*Wires)[WiresSelected[HighestCnt]]);
						WiresSelected[HighestCnt] = -1;
						memset(&NewNetLabel, 0, sizeof(NetLabelRecord));
						memset(&HulpLine, 0, 100);
						sprintf(HulpLine, "%s%i%s", FirstText, count, LastText);
						memmove(&NewNetLabel.Name, &HulpLine, 35);
						NewNetLabel.ConnectX = Wire->X1;
						NewNetLabel.ConnectY = Wire->Y1;
						NewNetLabel.TextX = 2.0;
						NewNetLabel.TextY = 0.0;
						count += AddLabelsStep;
						NewNetLabel.LabelType = 0;
						CommandAddTryingNetLabel(&NewNetLabel);
						count2++;
						LastActionNr++;
					}
				}
			}

			for (cnt = 0; cnt < Design.NrWires; cnt++)
			{
				Wire = &((*Wires)[cnt]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					if (NrWiresSelected < 256)
					{
						WiresSelected[NrWiresSelected] = cnt;
						Wire->Info &= ~OBJECT_SELECTED;
						NrWiresSelected++;
					}
				}
			}
		}
	}

	if (count2 > 0)
		LastActionNr--;

	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddNetLabel(int32 Mode)
{
	int32 Mode2;
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	Mode2 = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingNetLabel(CurrentX, CurrentY, Mode);

	SystemBusyMode = 4;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingNetLabel;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingNetLabel(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingNetLabel(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingNetLabel(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingNetLabel(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingNetLabel(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingNetLabel(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingNetLabel(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingNetLabel(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			CommandAddTryingNetLabel(&NewNetLabel);
			CheckInputMessages(0);
			DisplayObjectOnEscape = 0;
			SelectionEsc = 1;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			Mode2 = (Mode2 + 1) & 3;

			switch (Mode2)
			{
			case 0:
				if (Mode == 0)
				{
					NewNetLabel.ConnectX = NewWire.X1;
					NewNetLabel.ConnectY = NewWire.Y1;
				}
				else
				{
					NewNetLabel.ConnectX = NewBus.X1;
					NewNetLabel.ConnectY = NewBus.Y1;
				}

				NewNetLabel.Alignment = 0;
				break;

			case 1:
				if (Mode == 0)
				{
					NewNetLabel.ConnectX = NewWire.X2;
					NewNetLabel.ConnectY = NewWire.Y2;
				}
				else
				{
					NewNetLabel.ConnectX = NewBus.X2;
					NewNetLabel.ConnectY = NewBus.Y2;
				}

				NewNetLabel.Alignment = 6;
				break;

			case 2:
				if (Mode == 0)
				{
					NewNetLabel.ConnectX = NewWire.X1;
					NewNetLabel.ConnectY = NewWire.Y1;
				}
				else
				{
					NewNetLabel.ConnectX = NewBus.X1;
					NewNetLabel.ConnectY = NewBus.Y1;
				}

				NewNetLabel.Alignment = 0x100;
				break;

			case 3:
				if (Mode == 0)
				{
					NewNetLabel.ConnectX = NewWire.X2;
					NewNetLabel.ConnectY = NewWire.Y2;
				}
				else
				{
					NewNetLabel.ConnectX = NewBus.X2;
					NewNetLabel.ConnectY = NewBus.Y2;
				}

				NewNetLabel.Alignment = 0x106;
				break;
			}

			DrawTryingNetLabel(CurrentX, CurrentY, Mode);
			//    RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingNetLabel(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("add_netlabel.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			UnClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingNetLabel(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingNetLabel(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingObjectRect(double CurrentX, double CurrentY, int32 Mode)
{
	if ((Mode & 2) == 0)
		return;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	NewObjectRect.CentreX = (float) ((CurrentX2 + CurrentX) * 0.5);
	NewObjectRect.CentreY = (float) ((CurrentY2 + CurrentY) * 0.5);
	NewObjectRect.Width = (float) fabs(CurrentX - CurrentX2);
	NewObjectRect.Height = (float) fabs(CurrentY - CurrentY2);
	DrawObjectRect(&NewObjectRect, 0.0, 0.0, 0);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingObjectRect(ObjectRectRecord * ObjectRect, int32 Mode)
{
	if ((Mode & 1) == 0)
	{
		ObjectRect->Info = 0;
		AddObjectRect(ObjectRect);
	}
	else
	{
		memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
		NewObjectLine.Thickness = NewObjectRect.Thickness;
		NewObjectLine.Info = 0;
		NewObjectLine.X1 = (float) (ObjectRect->CentreX - (ObjectRect->Width * 0.5));
		NewObjectLine.Y1 = (float) (ObjectRect->CentreY - (ObjectRect->Height * 0.5));
		NewObjectLine.X2 = (float) (ObjectRect->CentreX - (ObjectRect->Width * 0.5));
		NewObjectLine.Y2 = (float) (ObjectRect->CentreY + (ObjectRect->Height * 0.5));
		AddObjectLine(&NewObjectLine);
		NewObjectLine.X1 = (float) (ObjectRect->CentreX - (ObjectRect->Width * 0.5));
		NewObjectLine.Y1 = (float) (ObjectRect->CentreY + (ObjectRect->Height * 0.5));
		NewObjectLine.X2 = (float) (ObjectRect->CentreX + (ObjectRect->Width * 0.5));
		NewObjectLine.Y2 = (float) (ObjectRect->CentreY + (ObjectRect->Height * 0.5));
		AddObjectLine(&NewObjectLine);
		NewObjectLine.X1 = (float) (ObjectRect->CentreX + (ObjectRect->Width * 0.5));
		NewObjectLine.Y1 = (float) (ObjectRect->CentreY + (ObjectRect->Height * 0.5));
		NewObjectLine.X2 = (float) (ObjectRect->CentreX + (ObjectRect->Width * 0.5));
		NewObjectLine.Y2 = (float) (ObjectRect->CentreY - (ObjectRect->Height * 0.5));
		AddObjectLine(&NewObjectLine);
		NewObjectLine.X1 = (float) (ObjectRect->CentreX + (ObjectRect->Width * 0.5));
		NewObjectLine.Y1 = (float) (ObjectRect->CentreY - (ObjectRect->Height * 0.5));
		NewObjectLine.X2 = (float) (ObjectRect->CentreX - (ObjectRect->Width * 0.5));
		NewObjectLine.Y2 = (float) (ObjectRect->CentreY - (ObjectRect->Height * 0.5));
		AddObjectLine(&NewObjectLine);
	}

	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddObjectRect(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY, x1, y1;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	memset(&NewObjectRect, 0, sizeof(ObjectRectRecord));

	if (Mode & 4)
		NewObjectRect.Thickness = 0.0;
	else
		NewObjectRect.Thickness = (float) STANDARD_LINE_THICKNESS;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;

//  while (LeftButtonPressed) CheckInputMessages(0) ;
	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingObjectRect(CurrentX, CurrentY, Mode);
	SystemBusyMode = 5;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectRect;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObjectRect(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				x1 = fabs(CurrentX - CurrentX2);
				y1 = fabs(CurrentY - CurrentY2);
				sprintf(InfoStr, "%.1f , %.1f", x1, y1);
				RedrawInfoStr(1);
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectRect(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingObjectRect(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectRect(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingObjectRect(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectRect(CurrentX, CurrentY, Mode);

			if ((Mode & 2) == 2)
			{
				if ((NotInRange(NewObjectRect.Width, 0.0)) || (NotInRange(NewObjectRect.Height, 0.0)))
					CommandAddTryingObjectRect(&NewObjectRect, Mode);

				SelectionEsc = 1;
				DisplayObjectOnEscape = 0;
			}

			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;
			OldX = CurrentX;
			OldY = CurrentY;
			OldDir = -1;
			CheckInputMessages(0);
			Mode += 2;

			if (DisplayObjectOnEscape)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("other_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();
	InfoStr[0] = 0;
	RedrawInfoStr(1);

	if (DisplayObjectOnEscape)
		DrawTryingObjectRect(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingObjectCircle(double CurrentX, double CurrentY, int32 Mode)
{
	if (Mode == 0)
		return;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	switch (Mode)
	{
	case 1:
		NewObjectCircle.CentreX = (float) CurrentX2;
		NewObjectCircle.CentreY = (float) CurrentY2;
		NewObjectCircle.Diam = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
		break;

	case 2:
		NewObjectCircle.CentreX = (float) CurrentX;
		NewObjectCircle.CentreY = (float) CurrentY;
		break;
	}

	DrawObjectCircle(&NewObjectCircle, 0.0, 0.0, 0);
	DrawLine(Mult(NewObjectCircle.CentreX - Xoffset) + DrawWindowMinX - 3,
	         DrawWindowMaxY - Mult(NewObjectCircle.CentreY - Yoffset) - 1 - 3,
	         Mult(NewObjectCircle.CentreX - Xoffset) + DrawWindowMinX + 3,
	         DrawWindowMaxY - Mult(NewObjectCircle.CentreY - Yoffset) - 1 + 3);
	DrawLine(Mult(NewObjectCircle.CentreX - Xoffset) + DrawWindowMinX - 3,
	         DrawWindowMaxY - Mult(NewObjectCircle.CentreY - Yoffset) - 1 + 3,
	         Mult(NewObjectCircle.CentreX - Xoffset) + DrawWindowMinX + 3,
	         DrawWindowMaxY - Mult(NewObjectCircle.CentreY - Yoffset) - 1 - 3);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingObjectCircle(ObjectCircleRecord * ObjectCircle)
{
	ObjectCircle->Info = 0;
	AddObjectCircle(ObjectCircle);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddObjectCircle(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY, x1;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	memset(&NewObjectCircle, 0, sizeof(ObjectCircleRecord));

	if (Mode & 32)
		NewObjectCircle.Thickness = 0.0;
	else
		NewObjectCircle.Thickness = (float) STANDARD_LINE_THICKNESS;

	NewObjectCircle.CircleMode = (int16) (Mode & 15);

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	Mode = ((Mode >> 4) & 1);

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
	SystemBusyMode = 6;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectCircle;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObjectCircle(OldX, OldY, Mode);
				x1 = max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0;
				sprintf(InfoStr, "%.1f", x1);
				RedrawInfoStr(1);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectCircle(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingObjectCircle(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectCircle(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingObjectCircle(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectCircle(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectCircle(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectCircle(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectCircle(CurrentX, CurrentY, Mode);

			if (Mode == 1)
			{
				if (NotInRange(NewObjectCircle.Diam, 0.0))
					CommandAddTryingObjectCircle(&NewObjectCircle);

				SelectionEsc = 1;
				DisplayObjectOnEscape = 0;
			}

			Mode++;
			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;

			OldX = CurrentX;
			OldY = CurrentY;
			OldDir = -1;
			CheckInputMessages(0);

			if (DisplayObjectOnEscape)
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectCircle(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("other_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingObjectCircle(CurrentX, CurrentY, Mode);
		}
	}

	InfoStr[0] = 0;
	RedrawInfoStr(1);
	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingObjectCircle(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingObjectArc(double CurrentX, double CurrentY, int32 Mode)
{
	if (Mode == 0)
		return;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	switch (Mode)
	{
	case 1:
		NewObjectArc.CentreX = (float) CurrentX2;
		NewObjectArc.CentreY = (float) CurrentY2;
		NewObjectArc.Width = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
		NewObjectArc.Height = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
		NewObjectArc.StartDiffX = (float) (CurrentX - CurrentX2);
		NewObjectArc.StartDiffY = (float) (CurrentY - CurrentY2);
		NewObjectArc.EndDiffX = (float) (CurrentX - CurrentX2);
		NewObjectArc.EndDiffY = (float) (CurrentY - CurrentY2);
		break;

	case 2:
		NewObjectArc.StartDiffX = (float) (CurrentX - NewObjectArc.CentreX);
		NewObjectArc.StartDiffY = (float) (CurrentY - NewObjectArc.CentreY);
		NewObjectArc.EndDiffX = (float) (CurrentX - NewObjectArc.CentreX);
		NewObjectArc.EndDiffY = (float) (CurrentY - NewObjectArc.CentreY);
		break;

	case 3:
		NewObjectArc.EndDiffX = (float) (CurrentX - NewObjectArc.CentreX);
		NewObjectArc.EndDiffY = (float) (CurrentY - NewObjectArc.CentreY);
		break;

	case 4:
		NewObjectArc.CentreX = (float) CurrentX;
		NewObjectArc.CentreY = (float) CurrentY;
		break;
	}

	DrawObjectArc(&NewObjectArc, 0.0, 0.0, 1);

	switch (Mode)
	{
	case 1:
		DrawLine(Mult(NewObjectArc.CentreX - Xoffset) + DrawWindowMinX - 3,
		         DrawWindowMaxY - Mult(NewObjectArc.CentreY - Yoffset) - 1 - 3,
		         Mult(NewObjectArc.CentreX - Xoffset) + DrawWindowMinX + 3,
		         DrawWindowMaxY - Mult(NewObjectArc.CentreY - Yoffset) - 1 + 3);
		DrawLine(Mult(NewObjectArc.CentreX - Xoffset) + DrawWindowMinX - 3,
		         DrawWindowMaxY - Mult(NewObjectArc.CentreY - Yoffset) - 1 + 3,
		         Mult(NewObjectArc.CentreX - Xoffset) + DrawWindowMinX + 3,
		         DrawWindowMaxY - Mult(NewObjectArc.CentreY - Yoffset) - 1 - 3);
		break;

	case 2:
	case 3:
		DrawLine(Mult(NewObjectArc.CentreX - Xoffset) + DrawWindowMinX,
		         DrawWindowMaxY - Mult(NewObjectArc.CentreY - Yoffset) - 1, Mult(CurrentX - Xoffset) + DrawWindowMinX,
		         DrawWindowMaxY - Mult(CurrentY - Yoffset) - 1);
		break;
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingObjectArc(ObjectArcRecord * ObjectArc)
{
	ObjectArc->Info = 0;
	AddObjectArc(ObjectArc);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddObjectArc(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY, x1;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	memset(&NewObjectArc, 0, sizeof(ObjectArcRecord));
	NewObjectArc.Thickness = (float) STANDARD_LINE_THICKNESS;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;

	ArcMode = 0;
	Mode = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingObjectArc(CurrentX, CurrentY, Mode);
	SystemBusyMode = 7;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectArc;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObjectArc(OldX, OldY, Mode);
				x1 = max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0;
				sprintf(InfoStr, "%.1f", x1);
				RedrawInfoStr(1);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectArc(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingObjectArc(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectArc(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingObjectArc(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectArc(CurrentX, CurrentY, Mode);

			if (Mode == 3)
			{
				if (NotInRange(NewObjectArc.Width, 0.0))
					CommandAddTryingObjectArc(&NewObjectArc);

				SelectionEsc = 1;
				DisplayObjectOnEscape = 0;
			}

			Mode++;

			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;

			OldX = CurrentX;
			OldY = CurrentY;
			CheckInputMessages(0);

			if (DisplayObjectOnEscape)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("other_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}
	}

	InfoStr[0] = 0;
	RedrawInfoStr(1);
	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingObjectArc(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingObjectText(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	NewObjectText.X = (float) CurrentX;
	NewObjectText.Y = (float) CurrentY;

	if (Mode == 0)
		NewObjectText.TextMode = 0;
	else
		NewObjectText.TextMode = 1;

	DrawObjectText(&NewObjectText, 0.0, 0.0, 0);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingObjectText(ObjectTextRecord * ObjectText)
{
	ObjectText->Info = 0;
	AddObjectText(ObjectText);
	RePaint();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddObjectText(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	memset(&NewObjectText, 0, sizeof(ObjectTextRecord));
	NewObjectText.FontHeight = (float) 1.0;
	NewObjectText.Thickness = (float) STANDARD_LINE_THICKNESS;

	if (TextInputDialog(&NewObjectText, 0) == 2)
		return;

	if (strlen(NewObjectText.Text) == 0)
		return;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	Mode = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingObjectText(CurrentX, CurrentY, Mode);
	SystemBusyMode = 8;

	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectText;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObjectText(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectText(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingObjectText(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectText(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingObjectText(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObjectText(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectText(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectText(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectText(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectText(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{

			DrawTryingObjectText(CurrentX, CurrentY, Mode);

			CommandAddTryingObjectText(&NewObjectText);

			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingObjectText(CurrentX, CurrentY, Mode);
			Mode = (Mode + 1) & 1;
//      RightButtonPressed=0;
			CheckInputMessages(0);
			DrawTryingObjectText(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectText(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("other_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingObjectText(OldX, OldY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingObjectText(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingObjectNumbers(double CurrentX, double CurrentY, int32 Mode)
{
	int32 cnt;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	ObjectNumbers.x1 = CurrentX;
	ObjectNumbers.y1 = CurrentY;

	for (cnt = 0; cnt < ObjectNumbers.Count; cnt++)
	{
		memset(&NewObjectText, 0, sizeof(ObjectTextRecord));
		sprintf(NewObjectText.Text, "%i", ObjectNumbers.Start + ObjectNumbers.Step * cnt);
		NewObjectText.X = (float) CurrentX;
		NewObjectText.Y = (float) (CurrentY - cnt);
		NewObjectText.FontHeight = (float) 1.0;
		NewObjectText.Thickness = (float) STANDARD_LINE_THICKNESS;

		if (Mode == 1)
			NewObjectText.TextMode = ALIGN_RIGHT_BOTTOM;

		DrawObjectText(&NewObjectText, 0.0, 0.0, 0);
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline()

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingObjectNumbers(int32 Mode)
{
	int32 cnt;

	for (cnt = 0; cnt < ObjectNumbers.Count; cnt++)
	{
		memset(&NewObjectText, 0, sizeof(ObjectTextRecord));
		sprintf(NewObjectText.Text, "%i", ObjectNumbers.Start + ObjectNumbers.Step * cnt);
		NewObjectText.X = (float) ObjectNumbers.x1;
		NewObjectText.Y = (float) (ObjectNumbers.y1 - cnt);
		NewObjectText.FontHeight = (float) 1.0;
		NewObjectText.Thickness = (float) STANDARD_LINE_THICKNESS;

		if (Mode == 1)
			NewObjectText.TextMode = ALIGN_RIGHT_BOTTOM;

		AddObjectText(&NewObjectText);
	}

	RePaint();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddObjectTextNumbers(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;

	if (NumberInputDialog() == 2)
		return;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	Mode = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
	SystemBusyMode = 9;

	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectNumbers;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObjectNumbers(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectNumbers(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingObjectNumbers(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingObjectNumbers(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingObjectNumbers(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectNumbers(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectNumbers(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectNumbers(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			CommandAddTryingObjectNumbers(Mode);
			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
			Mode = (Mode + 1) & 1;
//      RightButtonPressed=0;
			CheckInputMessages(0);
			DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectNumbers(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("other_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingObjectNumbers(OldX, OldY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingObjectNumbers(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyToClipBoard()
{
	typedef uint8 ByteArray[1000];
	int32 MemSize = 256 * 1024;
	int32 cnt, *NrObjects, *TotalMemSize;
	int16 *BufP2;
	uint8 *BufP;
//  char   *Mem;
	ByteArray *Mem;

	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	BusConnectionRecord *BusConnection;
	NetLabelRecord *NetLabel;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	InstanceRecord *Instance;
	GlobalConnectionRecord *GlobalConnection;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;


	if (!OpenClipboard(SCHWindow))
		return -1;

	if (!EmptyClipboard())
		return -1;

	if ((GlobalClipBoardMem = GlobalAlloc(GHND | GMEM_DDESHARE, MemSize)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -1;

	NrObjects = (int32 *) ClipBoardMem;
	BufP = ClipBoardMem + 4;
	TotalMemSize = (int32 *) BufP;
	BufP += 4;
	*TotalMemSize = 8;
	Mem = (ByteArray *) ClipBoardMem;

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = INSTANCE;
				memmove(BufP, Instance, sizeof(InstanceRecord));
				(*NrObjects)++;
				BufP += sizeof(InstanceRecord);
				(*TotalMemSize) += 2 + sizeof(InstanceRecord);
			}
		}

		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = WIRE;
				memmove(BufP, Wire, sizeof(WireRecord));
				(*NrObjects)++;
				BufP += sizeof(WireRecord);
				(*TotalMemSize) += 2 + sizeof(WireRecord);
			}
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = BUS;
				memmove(BufP, Bus, sizeof(BusRecord));
				(*NrObjects)++;
				BufP += sizeof(BusRecord);
				(*TotalMemSize) += 2 + sizeof(BusRecord);
			}
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = NET_LABEL;
				memmove(BufP, NetLabel, sizeof(NetLabelRecord));
				(*NrObjects)++;
				BufP += sizeof(NetLabelRecord);
				(*TotalMemSize) += 2 + sizeof(NetLabelRecord);
			}
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = BUS_CONNECTION;
				memmove(BufP, BusConnection, sizeof(BusConnectionRecord));
				(*NrObjects)++;
				BufP += sizeof(BusConnectionRecord);
				(*TotalMemSize) += 2 + sizeof(BusConnectionRecord);
			}
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = JUNCTION;
				memmove(BufP, Junction, sizeof(JunctionRecord));
				(*NrObjects)++;
				BufP += sizeof(JunctionRecord);
				(*TotalMemSize) += 2 + sizeof(JunctionRecord);
			}
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = ONE_PIN_NET;
				memmove(BufP, OnePinNet, sizeof(OnePinNetRecord));
				(*NrObjects)++;
				BufP += sizeof(OnePinNetRecord);
				(*TotalMemSize) += 2 + sizeof(OnePinNetRecord);
			}
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = GLOBAL_CONNECTION;
				memmove(BufP, GlobalConnection, sizeof(GlobalConnectionRecord));
				(*NrObjects)++;
				BufP += sizeof(GlobalConnectionRecord);
				(*TotalMemSize) += 2 + sizeof(GlobalConnectionRecord);
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = SYMBOL_PIN;
				memmove(BufP, Pin, sizeof(PinRecord));
				(*NrObjects)++;
				BufP += sizeof(PinRecord);
				(*TotalMemSize) += 2 + sizeof(PinRecord);
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = SYMBOL_POWERPIN;
				memmove(BufP, PowerPin, sizeof(PowerPinRecord));
				(*NrObjects)++;
				BufP += sizeof(PowerPinRecord);
				(*TotalMemSize) += 2 + sizeof(PowerPinRecord);
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				BufP2 = (int16 *) BufP;
				BufP += 2;
				*BufP2 = SYMBOL_PINBUS;
				memmove(BufP, PinBus, sizeof(PinBusRecord));
				(*NrObjects)++;
				BufP += sizeof(PinBusRecord);
				(*TotalMemSize) += 2 + sizeof(PinBusRecord);
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			BufP2 = (int16 *) BufP;
			BufP += 2;
			*BufP2 = INFO_LINE;
			memmove(BufP, ObjectLine, sizeof(ObjectLineRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectLineRecord);
			(*TotalMemSize) += 2 + sizeof(ObjectLineRecord);
		}
	}


	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			BufP2 = (int16 *) BufP;
			BufP += 2;
			*BufP2 = INFO_RECT;
			memmove(BufP, ObjectRect, sizeof(ObjectRectRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectRectRecord);
			(*TotalMemSize) += 2 + sizeof(ObjectRectRecord);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			BufP2 = (int16 *) BufP;
			BufP += 2;
			*BufP2 = INFO_CIRCLE;
			memmove(BufP, ObjectCircle, sizeof(ObjectCircleRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectCircleRecord);
			(*TotalMemSize) += 2 + sizeof(ObjectCircleRecord);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			BufP2 = (int16 *) BufP;
			BufP += 2;
			*BufP2 = INFO_ARC;
			memmove(BufP, ObjectArc, sizeof(ObjectArcRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectArcRecord);
			(*TotalMemSize) += 2 + sizeof(ObjectArcRecord);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			BufP2 = (int16 *) BufP;
			BufP += 2;
			*BufP2 = INFO_TEXT;
			memmove(BufP, ObjectText, sizeof(ObjectTextRecord));
			(*NrObjects)++;
			BufP += sizeof(ObjectTextRecord);
			(*TotalMemSize) += 2 + sizeof(ObjectTextRecord);
		}
	}



	GlobalUnlock(GlobalClipBoardMem);

	if (SetClipboardData(ClipBoardSchematicID, GlobalClipBoardMem) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		GlobalFree(GlobalClipBoardMem);
		return -1;
	}

	CloseClipboard();
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyFromClipBoard()
{

	typedef uint8 ByteArray[1000];

	int32 cnt, *NrObjects, TotalMemSize, Count, *hulp, pos, lengte;
	int16 *BufP2, ObjectType;
	uint8 *BufP;
	int32 ObjectsAdded = 0;
	HGLOBAL NewGlobalClipBoardMem;
	uint8 *NewClipBoardMem;

	ByteArray *Mem;

	WireRecord NewWire;
	BusRecord NewBus;
	JunctionRecord NewJunction;
	OnePinNetRecord NewOnePinNet;
	BusConnectionRecord NewBusConnection;
	NetLabelRecord NewNetLabel;
	ObjectLineRecord NewObjectLine;
	ObjectRectRecord NewObjectRect;
	ObjectCircleRecord NewObjectCircle;
	ObjectArcRecord NewObjectArc;
	ObjectTextRecord NewObjectText;
	InstanceRecord NewInstance;
	GlobalConnectionRecord NewGlobalConnection;
	PinRecord NewPin;
	PowerPinRecord NewPowerPin;
	PinBusRecord NewPinBus;


	char FileName[MAX_LENGTH_STRING], NewSymbolDir[MAX_LENGTH_STRING], NewLibName[MAX_LENGTH_STRING];

	memmove(&BackupDesign, &Design, sizeof(DesignRecord));
	memmove(&BackupDesignSymbol, &DesignSymbol, sizeof(SymbolRecord));

	if (!OpenClipboard(SCHWindow))
		return -2;

	if ((GlobalClipBoardMem = GetClipboardData(ClipBoardSchematicID)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -2;

	hulp = (int32 *) (ClipBoardMem + 4);
	TotalMemSize = *hulp;
//  EscapeInsertSymbol=0;

	if ((NewGlobalClipBoardMem = GlobalAlloc(GHND, TotalMemSize)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	if ((NewClipBoardMem = GlobalLock(NewGlobalClipBoardMem)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	memmove(NewClipBoardMem, ClipBoardMem, TotalMemSize);
	GlobalUnlock(GlobalClipBoardMem);
	CloseClipboard();

	NrObjects = (int32 *) NewClipBoardMem;
	Mem = (ByteArray *) NewClipBoardMem;
	BufP = NewClipBoardMem + 4;
	BufP += 4;
	BufP2 = (int16 *) BufP;
	Count = *NrObjects;
	cnt = 0;

	while (cnt < Count)
	{
		BufP2 = (int16 *) BufP;
		ObjectType = *BufP2;
		BufP += 2;

		switch (ObjectType)
		{
		case INSTANCE:
			if (!EditingSymbol)
			{
				memmove(&NewInstance, BufP, sizeof(InstanceRecord));
				NewInstance.Info = OBJECT_SELECTED | 7;
				BufP += sizeof(InstanceRecord);

				if (SearchSymbol(NewInstance.SymbolName, FileName, &pos, &lengte, 0) == 1)
				{
					if ((pos == -1) || (pos == -2))
					{
						strcpy(NewSymbolDir, FileName);
						strcpy(NewLibName, "");
					}
					else
					{
						strcpy(NewSymbolDir, "");
						strcpy(NewLibName, FileName);
					}

					GetSymbol(NewInstance.SymbolName, NewSymbolDir, NewLibName, 3);
					NewInstance.OriginX += 100000.0;
					NewInstance.OriginY += 100000.0;
					AddInstance(&NewInstance);
					ObjectsAdded = 1;
				}
			}

			break;

		case WIRE:
			if (!EditingSymbol)
			{
				memmove(&NewWire, BufP, sizeof(WireRecord));
				NewWire.Info = OBJECT_SELECTED | 3;
				BufP += sizeof(WireRecord);
				NewWire.X1 += 100000.0;
				NewWire.Y1 += 100000.0;
				NewWire.X2 += 100000.0;
				NewWire.Y2 += 100000.0;
				CommandAddTryingWire(&NewWire, 3);
				ObjectsAdded = 1;
			}

			break;

		case BUS:
			if (!EditingSymbol)
			{
				memmove(&NewBus, BufP, sizeof(BusRecord));
				NewBus.Info = OBJECT_SELECTED | 3;
				BufP += sizeof(BusRecord);
				NewBus.X1 += 100000.0;
				NewBus.Y1 += 100000.0;
				NewBus.X2 += 100000.0;
				NewBus.Y2 += 100000.0;
				CommandAddTryingBus(&NewBus, 3);
				ObjectsAdded = 1;
			}

			break;

		case BUS_CONNECTION:
			if (!EditingSymbol)
			{
				memmove(&NewBusConnection, BufP, sizeof(BusConnectionRecord));
				NewBusConnection.Info = OBJECT_SELECTED;
				BufP += sizeof(BusConnectionRecord);
				NewBusConnection.X += 100000.0;
				NewBusConnection.Y += 100000.0;
				AddBusConnection(&NewBusConnection);
				ObjectsAdded = 1;
			}

			break;

		case JUNCTION:
			if (!EditingSymbol)
			{
				memmove(&NewJunction, BufP, sizeof(JunctionRecord));
				NewJunction.Info = OBJECT_SELECTED;
				BufP += sizeof(JunctionRecord);
				NewJunction.X += 100000.0;
				NewJunction.Y += 100000.0;
				AddJunction(&NewJunction);
				ObjectsAdded = 1;
			}

			break;

		case ONE_PIN_NET:
			if (!EditingSymbol)
			{
				memmove(&NewOnePinNet, BufP, sizeof(OnePinNetRecord));
				NewOnePinNet.Info = OBJECT_SELECTED;
				BufP += sizeof(OnePinNetRecord);
				NewOnePinNet.X += 100000.0;
				NewOnePinNet.Y += 100000.0;
				AddOnePinNet(&NewOnePinNet);
				ObjectsAdded = 1;
			}

			break;

		case GLOBAL_CONNECTION:
			if (!EditingSymbol)
			{
				memmove(&NewGlobalConnection, BufP, sizeof(GlobalConnectionRecord));
				NewGlobalConnection.Info = OBJECT_SELECTED | 3;
				BufP += sizeof(GlobalConnectionRecord);
				NewGlobalConnection.X += 100000.0;
				NewGlobalConnection.Y += 100000.0;
				NewGlobalConnection.NameX += 100000.0;
				NewGlobalConnection.NameY += 100000.0;
				AddGlobalConnection(&NewGlobalConnection);
				ObjectsAdded = 1;
			}

			break;

		case NET_LABEL:
			if (!EditingSymbol)
			{
				memmove(&NewNetLabel, BufP, sizeof(NetLabelRecord));
				NewNetLabel.Info = OBJECT_SELECTED | 3;
				BufP += sizeof(NetLabelRecord);
				NewNetLabel.ConnectX += 100000.0;
				NewNetLabel.ConnectY += 100000.0;
				AddNetLabel(&NewNetLabel);
				ObjectsAdded = 1;
			}

			break;

		case SYMBOL_PIN:
			if (EditingSymbol)
			{
				memmove(&NewPin, BufP, sizeof(PinRecord));
				NewPin.Info = OBJECT_SELECTED | 3;
				BufP += sizeof(PinRecord);
				NewPin.X += 100000.0;
				NewPin.Y += 100000.0;
				AddPin(&NewPin);
				ObjectsAdded = 1;
			}

			break;

		case SYMBOL_POWERPIN:
			if (EditingSymbol)
			{
				memmove(&NewPowerPin, BufP, sizeof(PowerPinRecord));
				NewPowerPin.Info = OBJECT_SELECTED | 3;
				BufP += sizeof(PowerPinRecord);
				NewPowerPin.NameX += 100000.0;
				NewPowerPin.NameY += 100000.0;
				AddPowerPin(&NewPowerPin);
				ObjectsAdded = 1;
			}

			break;

		case SYMBOL_PINBUS:
			if (EditingSymbol)
			{
				memmove(&NewPinBus, BufP, sizeof(PinBusRecord));
				NewPinBus.Info = OBJECT_SELECTED | 3;
				BufP += sizeof(PinBusRecord);
				NewPinBus.X += 100000.0;
				NewPinBus.Y += 100000.0;
				AddPinBus(&NewPinBus);
				ObjectsAdded = 1;
			}

			break;

		case INFO_LINE:
			memmove(&NewObjectLine, BufP, sizeof(ObjectLineRecord));
			NewObjectLine.Info = OBJECT_SELECTED;
			BufP += sizeof(ObjectLineRecord);
			NewObjectLine.X1 += 100000.0;
			NewObjectLine.Y1 += 100000.0;
			NewObjectLine.X2 += 100000.0;
			NewObjectLine.Y2 += 100000.0;
			AddObjectLine(&NewObjectLine);
			ObjectsAdded = 1;
			break;

		case INFO_RECT:
			memmove(&NewObjectRect, BufP, sizeof(ObjectRectRecord));
			NewObjectRect.Info = OBJECT_SELECTED;
			BufP += sizeof(ObjectRectRecord);
			NewObjectRect.CentreX += 100000.0;
			NewObjectRect.CentreY += 100000.0;
			AddObjectRect(&NewObjectRect);
			ObjectsAdded = 1;
			break;

		case INFO_CIRCLE:
			memmove(&NewObjectCircle, BufP, sizeof(ObjectCircleRecord));
			NewObjectCircle.Info = OBJECT_SELECTED;
			BufP += sizeof(ObjectCircleRecord);
			NewObjectCircle.CentreX += 100000.0;
			NewObjectCircle.CentreY += 100000.0;
			AddObjectCircle(&NewObjectCircle);
			ObjectsAdded = 1;
			break;

		case INFO_ARC:
			memmove(&NewObjectArc, BufP, sizeof(ObjectArcRecord));
			NewObjectArc.Info = OBJECT_SELECTED;
			BufP += sizeof(ObjectArcRecord);
			NewObjectArc.CentreX += 100000.0;
			NewObjectArc.CentreY += 100000.0;
			AddObjectArc(&NewObjectArc);
			ObjectsAdded = 1;
			break;

		case INFO_TEXT:
			memmove(&NewObjectText, BufP, sizeof(ObjectTextRecord));
			NewObjectText.Info = OBJECT_SELECTED;
			BufP += sizeof(ObjectTextRecord);
			NewObjectText.X += 100000.0;
			NewObjectText.Y += 100000.0;
			AddObjectText(&NewObjectText);
			ObjectsAdded = 1;
			break;
		}

		cnt++;
	}

	GlobalUnlock(NewGlobalClipBoardMem);
	GlobalFree(NewGlobalClipBoardMem);

	if (ObjectsAdded)
	{
//    ZeroUnusedObjects(1);
		CopyObjectsFromClipBoard = 1;
		PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_MOVE_OBJECTS, (LPARAM) NULL);
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopySelectionsToClipBoard()
{
	int32 *TotalMemSize;
	HGLOBAL NewGlobalClipBoardMem;
	uint8 *NewClipBoardMem;

	if (!OpenClipboard(SCHWindow))
		return -1;

//  if (!EmptyClipboard()) return -1;
	if ((NewGlobalClipBoardMem = GlobalAlloc(GHND | GMEM_DDESHARE, ClipBoardMemPos + 16)) == NULL)
		return -1;

	if ((NewClipBoardMem = GlobalLock(NewGlobalClipBoardMem)) == NULL)
		return -1;

	TotalMemSize = (int32 *) NewClipBoardMem;
	*TotalMemSize = ClipBoardMemPos;
	memmove(&NewClipBoardMem[4], ClipBoardMem, ClipBoardMemPos);

	GlobalUnlock(NewGlobalClipBoardMem);

	if (SetClipboardData(ClipBoardSelectionsSchematicID, NewGlobalClipBoardMem) == NULL)
	{
		GlobalUnlock(NewGlobalClipBoardMem);
		GlobalFree(NewGlobalClipBoardMem);
		return -1;
	}
	else
	{
//#ifdef _DEBUG
//#endif
	}

	CloseClipboard();
	return 0;
}



// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 WireBusEndPointReached(double CurrentX, double CurrentY, int32 mode)
{
	int32 cnt, cnt2;
	double x1a, x2a, y1a, y2a, x1, y1, x2, y2;
	int32 ObjectChanged, Found;
	InstanceRecord *Instance;
	WireRecord *Wire;
	BusRecord *Bus;
	ObjectRecord *Object;
	GlobalConnectionRecord *GlobalConnection;
	BusConnectionRecord *BusConnection;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		ObjectChanged = 0;

		if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			x1a = Instance->BoardPosMinX - 0.5;
			y1a = Instance->BoardPosMinY - 0.5;
			x2a = Instance->BoardPosMaxX + 0.5;
			y2a = Instance->BoardPosMaxY + 0.5;

			if ((CurrentX > x1a) && (CurrentX < x2a) && (CurrentY > y1a) && (CurrentY < y2a))
			{
				NrObjects = 0;
				InstanceToObject(Instance, 0.0, 0.0, 1);
				Found = 0;

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if ((InRange(Object->x1, CurrentX)) && (InRange(Object->y1, CurrentY)))
						return 1;
				}
			}
		}
	}

	if (mode == 0)
	{
		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Wire->X1;
				y1 = Wire->Y1;
				x2 = Wire->X2;
				y2 = Wire->Y2;

				if (((InRange(x1, CurrentX)) && (InRange(y1, CurrentY)))
				        || ((InRange(x2, CurrentX)) && (InRange(y2, CurrentY))))
					return 1;

				if (TestLineConnectedToCircle(x1, y1, x2, y2, CurrentX, CurrentY, 0.04))
					return 1;
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Bus->X1;
				y1 = Bus->Y1;
				x2 = Bus->X2;
				y2 = Bus->Y2;

				if (((InRange(x1, CurrentX)) && (InRange(y1, CurrentY)))
				        || ((InRange(x2, CurrentX)) && (InRange(y2, CurrentY))))
					return 1;

				if (TestLineConnectedToCircle(x1, y1, x2, y2, CurrentX, CurrentY, 0.04))
					return 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = GlobalConnection->X;
			y1 = GlobalConnection->Y;

			if ((InRange(x1, CurrentX)) && (InRange(y1, CurrentY)))
				return 1;
		}
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = BusConnection->X;
			y1 = BusConnection->Y;

			if ((InRange(x1, CurrentX)) && (InRange(y1, CurrentY)))
				return 1;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
