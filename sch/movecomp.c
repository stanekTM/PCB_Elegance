/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: movecomp.c
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
#include "calc.h"
#include "menus.h"
#include "sch.h"
#include "calcdef.h"
#include "graphics.h"
#include "toets.h"
#include "mainloop.h"
#include "draw3.h"
#include "draw2.h"
#include "draw.h"
#include "select.h"
#include "line2.h"
#include "insdel.h"
#include "movecomp.h"
#include "move2.h"
#include "resource.h"
#include "help.h"
#include "stdio.h"
#include "dialogs.h"
#include "property.h"
#include "ctype.h"

int32 ConnectionsComponentsDrawing, ok;
double SelectedMinX, SelectedMinY, SelectedMaxX, SelectedMaxY;

extern int32 CopyObjectsFromClipBoard;
extern DesignRecord BackupDesign;
extern SymbolRecord BackupDesignSymbol;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GetMinMaxSelectedObjects(void);

void PlaceMovedComponents(double CurrentX, double CurrentY, int32 Mode, int32 count);

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetCenterSelectedText(double *CenterX, double *CenterY)
{
	int32 cnt;

	NetLabelRecord *NetLabel;
	GlobalConnectionRecord *GlobalConnection;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;

	ObjectTextRecord *ObjectText;
	InstanceRecord *Instance;

	GetNrSelections();

	if (!EditingSymbol)
	{
		if (WiresSelected + BussesSelected + InstancesSelected + BusConnectionsSelected + JunctionsSelected +
		        OnePinNetsSelected + ObjectLinesSelected + ObjectRectsSelected + ObjectCirclesSelected +
		        ObjectArcsSelected + GlobalConnectionsSelected == 0)
		{
			if ((InstanceRefsSelected == 1) && (InstanceValuesSelected == 0) && (NetLabelsSelected == 0)
			        && (ObjectTextsSelected == 0) && (GlobalConnectionTextsSelected == 0))
			{
				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt]);

					if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1))
					{
						*CenterX = Instance->OriginX + Instance->RefOriginX;
						*CenterY = Instance->OriginY + Instance->RefOriginY;
						return 1;
					}
				}

			}

			if ((InstanceRefsSelected == 0) && (InstanceValuesSelected == 1) && (NetLabelsSelected == 0)
			        && (ObjectTextsSelected == 0) && (GlobalConnectionTextsSelected == 0))
			{
				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt]);

					if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2))
					{
						*CenterX = Instance->OriginX + Instance->ValueOriginX;
						*CenterY = Instance->OriginY + Instance->ValueOriginY;
						return 1;
					}
				}
			}

			if ((InstanceRefsSelected == 0) && (InstanceValuesSelected == 0) && (NetLabelsSelected == 1)
			        && (ObjectTextsSelected == 0) && (GlobalConnectionTextsSelected == 0))
			{
				for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
				{
					NetLabel = &((*NetLabels)[cnt]);

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						*CenterX = NetLabel->ConnectX + NetLabel->TextX;
						*CenterY = NetLabel->ConnectY + NetLabel->TextY;
						return 1;
					}
				}
			}

			if ((InstanceRefsSelected == 0) && (InstanceValuesSelected == 0) && (NetLabelsSelected == 0)
			        && (ObjectTextsSelected == 1) && (GlobalConnectionTextsSelected == 0))
			{
				for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
				{
					ObjectText = &((*ObjectTexts)[cnt]);

					if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						*CenterX = ObjectText->X;
						*CenterY = ObjectText->Y;
						return 2;
					}
				}
			}

			if ((InstanceRefsSelected == 0) && (InstanceValuesSelected == 0) && (NetLabelsSelected == 0)
			        && (ObjectTextsSelected == 0) && (GlobalConnectionTextsSelected == 1))
			{
				for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
				{
					GlobalConnection = &((*GlobalConnections)[cnt]);

					if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2))
					{
						*CenterX = GlobalConnection->NameX;
						*CenterY = GlobalConnection->NameY;
						return 1;
					}
				}
			}
		}
	}
	else
	{
		if (ObjectLinesSelected + ObjectRectsSelected + ObjectCirclesSelected + ObjectArcsSelected + PinBussesSelected +
		        PinsSelected == 0)
		{
			if ((PinBussesSelected == 0) && (PowerPinsSelected == 0) && (PinsSelected == 1)
			        && (ObjectTextsSelected == 0))
			{
				for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
				{
					Pin = &((*Pins)[cnt]);

					if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2))
					{
						*CenterX = Pin->NameX;
						*CenterY = Pin->NameY;
						return 2;
					}
				}
			}

			if ((PinBussesSelected == 0) && (PowerPinsSelected == 1) && (PinsSelected == 0)
			        && (ObjectTextsSelected == 0))
			{
				for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
				{
					PowerPin = &((*PowerPins)[cnt]);

					if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						*CenterX = PowerPin->NameX;
						*CenterY = PowerPin->NameY;
						return 2;
					}
				}
			}

			if ((PinBussesSelected == 1) && (PowerPinsSelected == 0) && (PinsSelected == 0)
			        && (ObjectTextsSelected == 0))
			{
				for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
				{
					PinBus = &((*PinBusses)[cnt]);

					if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2))
					{
						*CenterX = PinBus->NameX;
						*CenterY = PinBus->NameY;
						return 2;
					}
				}
			}

			if ((PinBussesSelected == 0) && (PowerPinsSelected == 0) && (PinsSelected == 0)
			        && (ObjectTextsSelected == 1))
			{
				for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
				{
					ObjectText = &((*ObjectTexts)[cnt]);

					if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						*CenterX = ObjectText->X;
						*CenterY = ObjectText->Y;
						return 2;
					}
				}
			}
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void MoveSelectedObjects(int32 Mode, int32 Count)
/*
Mode = 0  -> Move objects
Mode = 1  -> Drag objects
Mode = 2  -> Copy objects

*/
{
	int32 cnt, count, CompPlaced, FirstShift, MoveText;
	double OldX, OldY, CurrentX, CurrentY, hx, hy, divx, divy, ShiftX, ShiftY, CurrentX2, CurrentY2, x1, y1, Mode2 = 1;
	HMENU PopUpMenu;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;

	if ((MousePosX == 10000) || (MousePosY == 10000))
	{
		CurrentX = AdjustToDrawGrid((ViewMinX + ViewMaxX) * 0.5);
		CurrentY = AdjustToDrawGrid((ViewMinY + ViewMaxY) * 0.5);
		SetNewCursor(&CurrentX, &CurrentY);
	}

	GetNrSelections();

	PopUpMenu = CreatePopupMenu();
	AppendMenu(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(265, "Escape"));
	count = 0;
	ConnectionsComponentsDrawing = 1;
	SelectionEsc = 0;
	CompPlaced = 0;

	ExtendSelections(Mode);

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;
	CentreSelectedX = CurrentX;
	CentreSelectedY = CurrentY;
	DrawXorFunction.Function1 = (FUNCP1) DrawSelectedObjects;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode2;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode2;
	ZoomInOutProcessed = 0;

	if (CopyObjectsFromClipBoard)
	{
		GetMinMaxSelectedObjects();

		hx = (SelectedMinX + SelectedMaxX) / 2;
		hy = (SelectedMinY + SelectedMaxY) / 2;
		CentreSelectedX = AdjustToDrawGrid(hx);
		CentreSelectedY = AdjustToDrawGrid(hy);
	}

	MoveText = 0;

	OldX = CurrentX;
	OldY = CurrentY;
	SystemBusyMode = 20;

	ClipMouseCursor();
	DrawSelectedObjects(CurrentX, CurrentY, 1);
	FirstShift = 1;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawSelectedObjects(OldX, OldY, 1);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawSelectedObjects(CurrentX, CurrentY, 1);
				else
				{
//          if (FirstShift) {
//            ShiftX=CurrentX;
//            ShiftY=CurrentY;
//            FirstShift=0;
//            DrawSelectedObjects(CurrentX,CurrentY,1);
//          }
				}

				x1 = CurrentX - CentreSelectedX;
				y1 = CurrentY - CentreSelectedY;
				sprintf(InfoStr, SC(447, "Move %.1f , %.1f mm"), x1, y1); //pøesun info pozice dole
				RedrawInfoStr(1);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{
//        DrawSelectedObjects(OldX,OldY,1);
				CentreSelectedX -= ShiftX - CurrentX;
				CentreSelectedY -= ShiftY - CurrentY;
				FirstShift = 1;
//        DrawSelectedObjects(OldX,OldY,1);
			}
		}
		else
		{
			if (FirstShift)
			{
				ShiftX = CurrentX;
				ShiftY = CurrentY;
				FirstShift = 0;
//            DrawSelectedObjects(CurrentX,CurrentY,1);
			}
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawSelectedObjects(CurrentX, CurrentY, 1);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawSelectedObjects(OldX, OldY, 1);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawSelectedObjects(CurrentX, CurrentY, 1);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawSelectedObjects(OldX, OldY, 1);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawSelectedObjects(CurrentX, CurrentY, 1);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawSelectedObjects(OldX, OldY, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawSelectedObjects(CurrentX, CurrentY, 1);
		}

		if (CheckLeftButton())
		{

			CompPlaced = 1;
			divx = CurrentX - CentreSelectedX;
			divy = CurrentY - CentreSelectedY;
			DrawSelectedObjects(CurrentX, CurrentY, 1);

			for (cnt = 0; cnt < Count; cnt++)
			{
				PlaceMovedComponents(CurrentX, CurrentY, Mode, Count - cnt);
				CurrentX += divx;
				CurrentY += divy;

				//update selection rectangle
				SearchMinX += divx;
				SearchMaxX += divx;
				SearchMinY += divy;
				SearchMaxY += divy;
				count++;
			}

			if (CopyObjectsFromClipBoard)
			{
				LastActionNr--;
				ZeroUnusedObjects(1);
				LastActionNr++;
			}

			CheckInputMessages(0);
			SelectionEsc = 1;

		}

//    if (LeftButtonDoublePressed) {
//
//    }
		if (CheckRightButton(&DrawXorFunction) == 1)
		{
			TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40,
			               0, SCHWindow, NULL);
			RightButtonPressed = 0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawSelectedObjects(OldX, OldY, 1);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				switch (Mode)
				{
				case 0:
					Help("move_objects.htm", 0);
					break;

				case 1:
					Help("drag_objects.htm", 0);
					break;

				case 2:
					Help("copy_objects.htm", 0);
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
			FirstShift = 1;
			ClipMouseCursor();
			DrawSelectedObjects(CurrentX, CurrentY, 1);
		}
	}

	InfoStr[0] = 0;
	RedrawInfoStr(1);
	UnClipMouseCursor();

	if (!CompPlaced)
	{
		DrawSelectedObjects(CurrentX, CurrentY, 1);

		if (CopyObjectsFromClipBoard)
		{
			memmove(&Design, &BackupDesign, sizeof(DesignRecord));
			memmove(&DesignSymbol, &BackupDesignSymbol, sizeof(SymbolRecord));
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, (LPARAM) NULL);
			ok = 1;
		}
	}

	if (MoveText)
	{
		DrawNewGrid();
		GridSize = 1.0;
		DrawNewGrid();
	}

	ConnectionsComponentsDrawing = 0;
	CopyObjectsFromClipBoard = 0;
	SystemBusyMode = 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawSelectedObjects(double CurrentX, double CurrentY, int32 Mode)
{

//  ObjectRecord *Object,*Object2;
	int32 cnt;
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


	StartDrawingEditingWindow();
	CurrentObjectCode = 0;
	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			DrawInstance(Instance, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
	}

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawWire(Wire, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawBus(Bus, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawJunction(Junction, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawOnePinNet(OnePinNet, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawBusConnection(BusConnection, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawGlobalConnection(GlobalConnection, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawNetLabel(NetLabel, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2 + 4);
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawPin(Pin, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawPowerPin(PowerPin, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				DrawPinBus(PinBus, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
		}


	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			DrawObjectLine(ObjectLine, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			DrawObjectRect(ObjectRect, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 3);
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			DrawObjectCircle(ObjectCircle, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 3);
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			DrawObjectArc(ObjectArc, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 3);
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			DrawObjectText(ObjectText, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 2);
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GetMinMaxSelectedObjects()
{
//  ObjectRecord *Object,*Object2;
	double x1, y1, x2;
	int32 cnt, cnt2, lengte, NrLines, Count, LinePos[16], TextRotation, TextAlignment, NrProperties, BusConnectionType;
	char PinBusStrings[16][64];
	char str[MAX_LENGTH_STRING];

	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	InstanceRecord *Instance;

	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;

	SelectedMinX = 100000000.0;
	SelectedMinY = 100000000.0;
	SelectedMaxX = -100000000.0;
	SelectedMaxY = -100000000.0;

	Count = Design.NrInstances;

	if (EditingSymbol)
		Count = 1;

	for (cnt = 0; cnt < Count; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (((Instance->Info & (OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((EditingSymbol) || ((!EditingSymbol) && ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0))))
		{
			if ((!EditingSymbol) && ((Instance->Info & 4) == 4))
			{
				SelectedMinX = min(SelectedMinX, Instance->BoardPosMinX);
				SelectedMaxX = max(SelectedMaxX, Instance->BoardPosMaxX);
				SelectedMinY = min(SelectedMinY, Instance->BoardPosMinY);
				SelectedMaxY = max(SelectedMaxY, Instance->BoardPosMaxY);
			}

			if (((Instance->Info & 1) == 1) && (GetMinMaxInstanceReferenceText(Instance) == 0))
			{
				SelectedMinX = min(SelectedMinX, TextMinX);
				SelectedMaxX = max(SelectedMaxX, TextMaxX);
				SelectedMinY = min(SelectedMinY, TextMinY);
				SelectedMaxY = max(SelectedMaxY, TextMaxY);
			}

			if (((Instance->Info & 2) == 2) && (GetMinMaxInstanceValueText(Instance)))
			{
				SelectedMinX = min(SelectedMinX, TextMinX);
				SelectedMaxX = max(SelectedMaxX, TextMaxX);
				SelectedMinY = min(SelectedMinY, TextMinY);
				SelectedMaxY = max(SelectedMaxY, TextMaxY);
			}
		}
	}

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				SelectedMinX = min(SelectedMinX, Wire->X1);
				SelectedMaxX = max(SelectedMaxX, Wire->X1);
				SelectedMinX = min(SelectedMinX, Wire->X2);
				SelectedMaxX = max(SelectedMaxX, Wire->X2);
				SelectedMinY = min(SelectedMinY, Wire->Y1);
				SelectedMaxY = max(SelectedMaxY, Wire->Y1);
				SelectedMinY = min(SelectedMinY, Wire->Y2);
				SelectedMaxY = max(SelectedMaxY, Wire->Y2);
			}
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				SelectedMinX = min(SelectedMinX, Bus->X1);
				SelectedMaxX = max(SelectedMaxX, Bus->X1);
				SelectedMinX = min(SelectedMinX, Bus->X2);
				SelectedMaxX = max(SelectedMaxX, Bus->X2);
				SelectedMinY = min(SelectedMinY, Bus->Y1);
				SelectedMaxY = max(SelectedMaxY, Bus->Y1);
				SelectedMinY = min(SelectedMinY, Bus->Y2);
				SelectedMaxY = max(SelectedMaxY, Bus->Y2);
			}
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				SelectedMinX = min(SelectedMinX, Junction->X);
				SelectedMaxX = max(SelectedMaxX, Junction->X);
				SelectedMinY = min(SelectedMinY, Junction->Y);
				SelectedMaxY = max(SelectedMaxY, Junction->Y);
			}
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				SelectedMinX = min(SelectedMinX, OnePinNet->X);
				SelectedMaxX = max(SelectedMaxX, OnePinNet->X);
				SelectedMinY = min(SelectedMinY, OnePinNet->Y);
				SelectedMaxY = max(SelectedMaxY, OnePinNet->Y);
			}
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				TextAlignment = (BusConnection->Alignment & 0x0f);
				BusConnectionType = (BusConnection->Alignment >> 14) & 3;

				switch (BusConnectionType)
				{
				case 0:
					SelectedMinX = min(SelectedMinX, BusConnection->X - BusSizeX);
					SelectedMaxX = max(SelectedMaxX, BusConnection->X);
					SelectedMinY = min(SelectedMinY, BusConnection->Y - BusSizeY * 0.5);
					SelectedMaxY = max(SelectedMaxY, BusConnection->Y + BusSizeY * 0.5);
					break;

				case 1:
					SelectedMinX = min(SelectedMinX, BusConnection->X - BusSizeY * 0.5);
					SelectedMaxX = max(SelectedMaxX, BusConnection->X + BusSizeY * 0.5);
					SelectedMinY = min(SelectedMinY, BusConnection->Y - BusSizeX);
					SelectedMaxY = max(SelectedMaxY, BusConnection->Y);
					break;

				case 2:
					SelectedMinX = min(SelectedMinX, BusConnection->X);
					SelectedMaxX = max(SelectedMaxX, BusConnection->X + BusSizeX);
					SelectedMinY = min(SelectedMinY, BusConnection->Y - BusSizeY * 0.5);
					SelectedMaxY = max(SelectedMaxY, BusConnection->Y + BusSizeY * 0.5);
					break;

				case 3:
					SelectedMinX = min(SelectedMinX, BusConnection->X - BusSizeY * 0.5);
					SelectedMaxX = max(SelectedMaxX, BusConnection->X + BusSizeY * 0.5);
					SelectedMinY = min(SelectedMinY, BusConnection->Y);
					SelectedMaxY = max(SelectedMaxY, BusConnection->Y + BusSizeX);
					break;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				SelectedMinX = min(SelectedMinX, GlobalConnection->X - 2.0);
				SelectedMaxX = max(SelectedMaxX, GlobalConnection->X + 2.0);
				SelectedMinY = min(SelectedMinY, GlobalConnection->Y - 1.0);
				SelectedMaxY = max(SelectedMaxY, GlobalConnection->Y + 1.0);
				TextRotation = (GlobalConnection->NameInfo >> 8) & 1;
				GetMinMaxText(GlobalConnection->NameX, GlobalConnection->NameY, 1.0, 0, TextRotation,
				              GlobalConnection->NameInfo & 15, (LPSTR) GlobalConnection->Text);
				SelectedMinX = min(SelectedMinX, TextMinX);
				SelectedMaxX = max(SelectedMaxX, TextMaxX);
				SelectedMinY = min(SelectedMinY, TextMinY);
				SelectedMaxY = max(SelectedMaxY, TextMaxY);
			}
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				TextAlignment = (NetLabel->Alignment & 0x0f);
				TextRotation = (NetLabel->Alignment >> 8) & 0x01;
				NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1);

				if (NrProperties > 0)
					sprintf(str, "%s (...)", NetLabel->Name);
				else
					strcpy(str, NetLabel->Name);

				GetMinMaxText(NetLabel->ConnectX + NetLabel->TextX, NetLabel->ConnectY + NetLabel->TextY, 1.0, 0,
				              TextRotation, TextAlignment, str);
				SelectedMinX = min(SelectedMinX, TextMinX);
				SelectedMaxX = max(SelectedMaxX, TextMaxX);
				SelectedMinY = min(SelectedMinY, TextMinY);
				SelectedMaxY = max(SelectedMaxY, TextMaxY);
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
				SelectedMinX = min(SelectedMinX, Pin->X - 0.3);
				SelectedMaxX = max(SelectedMaxX, Pin->X + 0.3);
				SelectedMinY = min(SelectedMinY, Pin->Y - 0.3);
				SelectedMaxY = max(SelectedMaxY, Pin->Y + 0.3);
				TextRotation = (Pin->NameInfo >> 8) & 1;
				GetMinMaxText(Pin->NameX, Pin->NameY, 1.0, 0, TextRotation, Pin->NameInfo & 15, (LPSTR) Pin->Name);
				SelectedMinX = min(SelectedMinX, TextMinX);
				SelectedMaxX = max(SelectedMaxX, TextMaxX);
				SelectedMinY = min(SelectedMinY, TextMinY);
				SelectedMaxY = max(SelectedMaxY, TextMaxY);
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				strcpy(str, PowerPin->NetName);
				strcat(str, " : ");
				strcat(str, PowerPin->Text);
				TextRotation = (PowerPin->NameInfo >> 8) & 1;
				GetMinMaxText(PowerPin->NameX, PowerPin->NameY, 1.0, 0, TextRotation, 0, str);
				SelectedMinX = min(SelectedMinX, TextMinX);
				SelectedMaxX = max(SelectedMaxX, TextMaxX);
				SelectedMinY = min(SelectedMinY, TextMinY);
				SelectedMaxY = max(SelectedMaxY, TextMaxY);
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				SelectedMinX = min(SelectedMinX, PinBus->X - 0.3);
				SelectedMaxX = max(SelectedMaxX, PinBus->X + 0.3);
				SelectedMinY = min(SelectedMinY, PinBus->Y - 0.3);
				SelectedMaxY = max(SelectedMaxY, PinBus->Y + 0.3);

				lengte = strlen(PinBus->Text);
				cnt2 = 0;
				NrLines = 0;
				memset(LinePos, 0, sizeof(LinePos));
				memset(PinBusStrings, 0, sizeof(PinBusStrings));
				LinePos[0] = 0;

				while (cnt2 < lengte)
				{
					if (PinBus->Text[cnt2] == '\\')
					{
						memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], cnt2 - LinePos[NrLines]);

						if (NrLines < 7)
							NrLines++;

						LinePos[NrLines] = cnt2 + 1;
					}

					cnt2++;
				}

				if (PinBus->Text[lengte - 1] != '\\')
				{
					memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], cnt2 - LinePos[NrLines]);
					LinePos[NrLines] = cnt2;
					NrLines++;
				}

				TextRotation = (PinBus->NameInfo >> 8) & 1;
				TextAlignment = PinBus->NameInfo & 0x0f;

				x1 = PinBus->NameX;
				y1 = PinBus->NameY;
				x2 = 1.0;

				for (cnt2 = 0; cnt2 < NrLines; cnt2++)
				{
					if (TextRotation == 0)
						GetMinMaxText(x1, y1 - cnt2, x2, 0, TextRotation, TextAlignment, PinBusStrings[cnt2]);
					else
						GetMinMaxText(x1 + cnt2, y1, x2, 0, TextRotation, TextAlignment, PinBusStrings[cnt2]);

					SelectedMinX = min(SelectedMinX, TextMinX);
					SelectedMinY = min(SelectedMinY, TextMinY);
					SelectedMaxX = max(SelectedMaxX, TextMaxX);
					SelectedMaxY = max(SelectedMaxY, TextMaxY);
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = min(SelectedMinX, ObjectLine->X1);
			SelectedMaxX = max(SelectedMaxX, ObjectLine->X1);
			SelectedMinX = min(SelectedMinX, ObjectLine->X2);
			SelectedMaxX = max(SelectedMaxX, ObjectLine->X2);
			SelectedMinY = min(SelectedMinY, ObjectLine->Y1);
			SelectedMaxY = max(SelectedMaxY, ObjectLine->Y1);
			SelectedMinY = min(SelectedMinY, ObjectLine->Y2);
			SelectedMaxY = max(SelectedMaxY, ObjectLine->Y2);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = min(SelectedMinX, ObjectRect->CentreX - ObjectRect->Width / 2);
			SelectedMaxX = max(SelectedMaxX, ObjectRect->CentreX + ObjectRect->Width / 2);
			SelectedMinY = min(SelectedMinY, ObjectRect->CentreY - ObjectRect->Height / 2);
			SelectedMaxY = max(SelectedMaxY, ObjectRect->CentreY + ObjectRect->Height / 2);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = min(SelectedMinX, ObjectCircle->CentreX - ObjectCircle->Diam / 2);
			SelectedMaxX = max(SelectedMaxX, ObjectCircle->CentreX + ObjectCircle->Diam / 2);
			SelectedMinY = min(SelectedMinY, ObjectCircle->CentreY - ObjectCircle->Diam / 2);
			SelectedMaxY = max(SelectedMaxY, ObjectCircle->CentreY + ObjectCircle->Diam / 2);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = min(SelectedMinX, ObjectArc->CentreX - ObjectArc->Width / 2);
			SelectedMaxX = max(SelectedMaxX, ObjectArc->CentreX + ObjectArc->Width / 2);
			SelectedMinY = min(SelectedMinY, ObjectArc->CentreY - ObjectArc->Height / 2);
			SelectedMaxY = max(SelectedMaxY, ObjectArc->CentreY + ObjectArc->Height / 2);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			TextAlignment = ObjectText->TextMode & 0x0f;
			TextRotation = (ObjectText->TextMode >> 8) & 0x01;
//      FontNr=ObjectText->FontNr;
			GetMinMaxText(ObjectText->X, ObjectText->Y, ObjectText->FontHeight, 0, TextRotation, TextAlignment,
			              ObjectText->Text);
			SelectedMinX = min(SelectedMinX, TextMinX);
			SelectedMaxX = max(SelectedMaxX, TextMaxX);
			SelectedMinY = min(SelectedMinY, TextMinY);
			SelectedMaxY = max(SelectedMaxY, TextMaxY);
		}
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void PlaceMovedComponents(double CurrentX, double CurrentY, int32 Mode, int32 count)
{
	int32 cnt, TempLastActionNr, Rotation, Changed;
	double DifX, DifY, rx, ry;
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

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	if ((!EditingSymbol) || (Mode < 2))
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Instance->AddNr <= TempLastActionNr))
			{
				memcpy(&NewInstance, Instance, sizeof(InstanceRecord));

				if ((Mode < 2) || (count > 1))
					NewInstance.Info &= ~(OBJECT_SELECTED | 7);

				DifX = CurrentX - CentreSelectedX;
				DifY = CurrentY - CentreSelectedY;

				if ((Instance->Info & 4) == 4)
				{
					NewInstance.OriginX += (float) DifX;
					NewInstance.OriginY += (float) DifY;
				}
				else
				{
					if ((Instance->Info & 1) == 1)
					{
						Rotation = Instance->SymbolInfo & OBJECT_ROTATE90;
						rx = NewInstance.RefOriginX;
						ry = NewInstance.RefOriginY;
						RotateFlipPoint(&rx, &ry, 0.0, 0.0, Rotation);

						if (Instance->SymbolInfo & OBJECT_MIRRORX)
						{	// Mirror X
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 4);
						}

						if (Instance->SymbolInfo & OBJECT_MIRRORY)
						{	// Mirror Y
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 8);
						}

						rx += DifX;
						ry += DifY;

						if (Instance->SymbolInfo & OBJECT_MIRRORY)
						{	// Mirror Y
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 8);
						}

						if (Instance->SymbolInfo & OBJECT_MIRRORX)
						{	// Mirror X
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 4);
						}

						RotateFlipPoint(&rx, &ry, 0.0, 0.0, (4 - Rotation) & 3);
						NewInstance.RefOriginX = (float) rx;
						NewInstance.RefOriginY = (float) ry;
					}

					if ((Instance->Info & 2) == 2)
					{
						Rotation = Instance->SymbolInfo & OBJECT_ROTATE90;
						rx = NewInstance.ValueOriginX;
						ry = NewInstance.ValueOriginY;
						RotateFlipPoint(&rx, &ry, 0.0, 0.0, Rotation);

						if (Instance->SymbolInfo & OBJECT_MIRRORX)
						{	// Mirror X
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 4);
						}

						if (Instance->SymbolInfo & OBJECT_MIRRORY)
						{	// Mirror Y
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 8);
						}

						rx += DifX;
						ry += DifY;

						if (Instance->SymbolInfo & OBJECT_MIRRORY)
						{	// Mirror Y
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 8);
						}

						if (Instance->SymbolInfo & OBJECT_MIRRORX)
						{	// Mirror X
							RotateFlipPoint(&rx, &ry, 0.0, 0.0, 4);
						}

						RotateFlipPoint(&rx, &ry, 0.0, 0.0, (4 - Rotation) & 3);
						NewInstance.ValueOriginX = (float) rx;
						NewInstance.ValueOriginY = (float) ry;
					}
				}

				if ((!EditingSymbol) || (Mode < 2))
				{
					if (AddInstance(&NewInstance))
					{
						Instance = (InstanceRecord *) & ((*Instances)[cnt]);

						if (Mode < 2)
						{
							Instance->Info |= OBJECT_NOT_VISIBLE;
							Instance->DeleteNr = (int16) LastActionNr;
						}
						else
						{
							if (count < 2)
								Instance->Info &= ~(OBJECT_SELECTED | 7);
						}

						Changed = 1;
					}
				}
			}
		}
	}

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if (((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Wire->AddNr <= TempLastActionNr))
			{
				memmove(&NewWire, Wire, sizeof(WireRecord));

				if ((Mode < 2) || (count > 1))
					NewWire.Info &= ~(OBJECT_SELECTED | 7);

				if ((Wire->Info & 1) == 1)
				{
					NewWire.X1 += (float) (CurrentX - CentreSelectedX);
					NewWire.Y1 += (float) (CurrentY - CentreSelectedY);
				}

				if ((Wire->Info & 2) == 2)
				{
					NewWire.X2 += (float) (CurrentX - CentreSelectedX);
					NewWire.Y2 += (float) (CurrentY - CentreSelectedY);
				}

				if (((InRange(NewWire.X1, NewWire.X2)) && (InRange(NewWire.Y1, NewWire.Y2)))
				        || (CommandAddTryingWire(&NewWire, 1)))
				{
					Changed = 1;
					Wire = &((*Wires)[cnt]);

					if (Mode < 2)
					{
						ZeroUnusedObjects(0);
						Wire->Info |= OBJECT_NOT_VISIBLE;
						Wire->DeleteNr = (int16) LastActionNr;
						DataBaseChanged = 1;
					}
					else
					{
						if (count < 2)
							Wire->Info &= ~(OBJECT_SELECTED | 7);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if (((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Bus->AddNr <= TempLastActionNr))
			{
				memmove(&NewBus, Bus, sizeof(BusRecord));

				if ((Mode < 2) || (count > 1))
					NewBus.Info &= ~(OBJECT_SELECTED | 7);

				if ((Bus->Info & 1) == 1)
				{
					NewBus.X1 += (float) (CurrentX - CentreSelectedX);
					NewBus.Y1 += (float) (CurrentY - CentreSelectedY);
				}

				if ((Bus->Info & 2) == 2)
				{
					NewBus.X2 += (float) (CurrentX - CentreSelectedX);
					NewBus.Y2 += (float) (CurrentY - CentreSelectedY);
				}

				if (((InRange(NewBus.X1, NewBus.X2)) && (InRange(NewBus.Y1, NewBus.Y2)))
				        || (CommandAddTryingBus(&NewBus, 1)))
				{
					Changed = 1;
					Bus = &((*Busses)[cnt]);

					if (Mode < 2)
					{
						ZeroUnusedObjects(0);
						Bus->Info |= OBJECT_NOT_VISIBLE;
						Bus->DeleteNr = (int16) LastActionNr;
						DataBaseChanged = 1;
					}
					else
					{
						if (count < 2)
							Bus->Info &= ~(OBJECT_SELECTED | 7);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if (((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Junction->AddNr <= TempLastActionNr))
			{
				memmove(&NewJunction, Junction, sizeof(JunctionRecord));

				if ((Mode < 2) || (count > 1))
					NewJunction.Info &= ~(OBJECT_SELECTED | 7);

				NewJunction.X += (float) (CurrentX - CentreSelectedX);
				NewJunction.Y += (float) (CurrentY - CentreSelectedY);

				if (AddJunction(&NewJunction))
				{
					Junction = &((*Junctions)[cnt]);

					if (Mode < 2)
					{
						Junction->Info |= OBJECT_NOT_VISIBLE;
						Junction->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							Junction->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if (((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (OnePinNet->AddNr <= TempLastActionNr))
			{
				memmove(&NewOnePinNet, OnePinNet, sizeof(OnePinNetRecord));

				if ((Mode < 2) || (count > 1))
					NewOnePinNet.Info &= ~(OBJECT_SELECTED | 7);

				NewOnePinNet.X += (float) (CurrentX - CentreSelectedX);
				NewOnePinNet.Y += (float) (CurrentY - CentreSelectedY);

				if (AddOnePinNet(&NewOnePinNet))
				{
					OnePinNet = &((*OnePinNets)[cnt]);

					if (Mode < 2)
					{
						OnePinNet->Info |= OBJECT_NOT_VISIBLE;
						OnePinNet->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							OnePinNet->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if (((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (BusConnection->AddNr <= TempLastActionNr))
			{
				memmove(&NewBusConnection, BusConnection, sizeof(BusConnectionRecord));

				if ((Mode < 2) || (count > 1))
					NewBusConnection.Info &= ~(OBJECT_SELECTED | 7);

				NewBusConnection.X += (float) (CurrentX - CentreSelectedX);
				NewBusConnection.Y += (float) (CurrentY - CentreSelectedY);

				if (AddBusConnection(&NewBusConnection))
				{
					BusConnection = &((*BusConnections)[cnt]);

					if (Mode < 2)
					{
						BusConnection->Info |= OBJECT_NOT_VISIBLE;
						BusConnection->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							BusConnection->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if (((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (GlobalConnection->AddNr <= TempLastActionNr))
			{
				memmove(&NewGlobalConnection, GlobalConnection, sizeof(GlobalConnectionRecord));

				if ((Mode < 2) || (count > 1))
					NewGlobalConnection.Info &= ~(OBJECT_SELECTED | 7);

				if ((GlobalConnection->Info & 1) == 1)
				{
					NewGlobalConnection.X += (float) (CurrentX - CentreSelectedX);
					NewGlobalConnection.Y += (float) (CurrentY - CentreSelectedY);
				}

				if ((GlobalConnection->Info & 2) == 2)
				{
					NewGlobalConnection.NameX += (float) (CurrentX - CentreSelectedX);
					NewGlobalConnection.NameY += (float) (CurrentY - CentreSelectedY);
				}

				if (AddGlobalConnection(&NewGlobalConnection))
				{
					GlobalConnection = &((*GlobalConnections)[cnt]);

					if (Mode < 2)
					{
						GlobalConnection->Info |= OBJECT_NOT_VISIBLE;
						GlobalConnection->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							GlobalConnection->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if (((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (NetLabel->AddNr <= TempLastActionNr))
			{
				memmove(&NewNetLabel, NetLabel, sizeof(NetLabelRecord));

				if ((Mode < 2) || (count > 1))
					NewNetLabel.Info &= ~(OBJECT_SELECTED | 7);

				if ((NetLabel->Info & 1) == 1)
				{
					NewNetLabel.ConnectX += (float) (CurrentX - CentreSelectedX);
					NewNetLabel.ConnectY += (float) (CurrentY - CentreSelectedY);
				}
				else
				{
					NewNetLabel.TextX += (float) (CurrentX - CentreSelectedX);
					NewNetLabel.TextY += (float) (CurrentY - CentreSelectedY);
				}

				if (AddNetLabel(&NewNetLabel))
				{
					NetLabel = &((*NetLabels)[cnt]);

					if (Mode < 2)
					{
						NetLabel->Info |= OBJECT_NOT_VISIBLE;
						NetLabel->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							NetLabel->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if (((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Pin->AddNr <= TempLastActionNr))
			{
				memmove(&NewPin, Pin, sizeof(PinRecord));

				if ((Mode < 2) || (count > 1))
					NewPin.Info &= ~(OBJECT_SELECTED | 7);

				if ((Pin->Info & 1) == 1)
				{
					NewPin.X += (float) (CurrentX - CentreSelectedX);
					NewPin.Y += (float) (CurrentY - CentreSelectedY);
				}

				if ((Pin->Info & 2) == 2)
				{
					NewPin.NameX += (float) (CurrentX - CentreSelectedX);
					NewPin.NameY += (float) (CurrentY - CentreSelectedY);
				}

				if (AddPin(&NewPin))
				{
					Pin = &((*Pins)[cnt]);

					if (Mode < 2)
					{
						Pin->Info |= OBJECT_NOT_VISIBLE;
						Pin->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							Pin->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if (((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (PowerPin->AddNr <= TempLastActionNr))
			{
				memmove(&NewPowerPin, PowerPin, sizeof(PowerPinRecord));

				if ((Mode < 2) || (count > 1))
					NewPowerPin.Info &= ~(OBJECT_SELECTED | 7);

				NewPowerPin.NameX += (float) (CurrentX - CentreSelectedX);
				NewPowerPin.NameY += (float) (CurrentY - CentreSelectedY);

				if (AddPowerPin(&NewPowerPin))
				{
					PowerPin = &((*PowerPins)[cnt]);

					if (Mode < 2)
					{
						PowerPin->Info |= OBJECT_NOT_VISIBLE;
						PowerPin->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							PowerPin->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if (((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (PinBus->AddNr <= TempLastActionNr))
			{
				memmove(&NewPinBus, PinBus, sizeof(PinBusRecord));

				if ((Mode < 2) || (count > 1))
					NewPinBus.Info &= ~(OBJECT_SELECTED | 7);

				if ((PinBus->Info & 1) == 1)
				{
					NewPinBus.X += (float) (CurrentX - CentreSelectedX);
					NewPinBus.Y += (float) (CurrentY - CentreSelectedY);
				}

				if ((PinBus->Info & 2) == 2)
				{
					NewPinBus.NameX += (float) (CurrentX - CentreSelectedX);
					NewPinBus.NameY += (float) (CurrentY - CentreSelectedY);
				}

				if (AddPinBus(&NewPinBus))
				{
					PinBus = &((*PinBusses)[cnt]);

					if (Mode < 2)
					{
						PinBus->Info |= OBJECT_NOT_VISIBLE;
						PinBus->DeleteNr = (int16) LastActionNr;
					}
					else
					{
						if (count < 2)
							PinBus->Info &= ~(OBJECT_SELECTED | 7);
					}

					Changed = 1;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));

			if ((Mode < 2) || (count > 1))
				NewObjectLine.Info &= ~(OBJECT_SELECTED | 7);

			if ((ObjectLine->Info & 1) == 1)
			{
				NewObjectLine.X1 += (float) (CurrentX - CentreSelectedX);
				NewObjectLine.Y1 += (float) (CurrentY - CentreSelectedY);
			}

			if ((ObjectLine->Info & 2) == 2)
			{
				NewObjectLine.X2 += (float) (CurrentX - CentreSelectedX);
				NewObjectLine.Y2 += (float) (CurrentY - CentreSelectedY);
			}

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);

				if (Mode < 2)
				{
					ObjectLine->Info |= OBJECT_NOT_VISIBLE;
					ObjectLine->DeleteNr = (int16) LastActionNr;
				}
				else
				{
					if (count < 2)
						ObjectLine->Info &= ~(OBJECT_SELECTED | 7);
				}

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));

			if ((Mode < 2) || (count > 1))
				NewObjectRect.Info &= ~(OBJECT_SELECTED | 7);

			NewObjectRect.CentreX += (float) (CurrentX - CentreSelectedX);
			NewObjectRect.CentreY += (float) (CurrentY - CentreSelectedY);

			if (AddObjectRect(&NewObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);

				if (Mode < 2)
				{
					ObjectRect->Info |= OBJECT_NOT_VISIBLE;
					ObjectRect->DeleteNr = (int16) LastActionNr;
				}
				else
				{
					if (count < 2)
						ObjectRect->Info &= ~(OBJECT_SELECTED | 7);
				}

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectCircle->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectCircle, ObjectCircle, sizeof(ObjectCircleRecord));

			if ((Mode < 2) || (count > 1))
				NewObjectCircle.Info &= ~(OBJECT_SELECTED | 7);

			NewObjectCircle.CentreX += (float) (CurrentX - CentreSelectedX);
			NewObjectCircle.CentreY += (float) (CurrentY - CentreSelectedY);

			if (AddObjectCircle(&NewObjectCircle))
			{
				ObjectCircle = &((*ObjectCircles)[cnt]);

				if (Mode < 2)
				{
					ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
					ObjectCircle->DeleteNr = (int16) LastActionNr;
				}
				else
				{
					if (count < 2)
						ObjectCircle->Info &= ~(OBJECT_SELECTED | 7);
				}

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));

			if ((Mode < 2) || (count > 1))
				NewObjectArc.Info &= ~(OBJECT_SELECTED | 7);

			NewObjectArc.CentreX += (float) (CurrentX - CentreSelectedX);
			NewObjectArc.CentreY += (float) (CurrentY - CentreSelectedY);

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if (Mode < 2)
				{
					ObjectArc->Info |= OBJECT_NOT_VISIBLE;
					ObjectArc->DeleteNr = (int16) LastActionNr;
				}
				else
				{
					if (count < 2)
						ObjectArc->Info &= ~(OBJECT_SELECTED | 7);
				}

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectText, ObjectText, sizeof(ObjectTextRecord));

			if ((Mode < 2) || (count > 1))
				NewObjectText.Info &= ~(OBJECT_SELECTED | 7);

			NewObjectText.X += (float) (CurrentX - CentreSelectedX);
			NewObjectText.Y += (float) (CurrentY - CentreSelectedY);

			if (AddObjectText(&NewObjectText))
			{
				ObjectText = &((*ObjectTexts)[cnt]);

				if (Mode < 2)
				{
					ObjectText->Info |= OBJECT_NOT_VISIBLE;
					ObjectText->DeleteNr = (int16) LastActionNr;
				}
				else
				{
					if (count < 2)
						ObjectText->Info &= ~(OBJECT_SELECTED | 7);
				}

				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void FlipTextAlignment(float *x, float *y, int16 *RefInfo, int16 SymbolInfo, int32 length, int32 Mode)
{
	// Mode:
	// 4 = flip x
	// 8 = flip y

	//int32 ObjectMirrorX = ((SymbolInfo & OBJECT_MIRRORX) >> 4);
	//int32 ObjectMirrorY = ((SymbolInfo & OBJECT_MIRRORY) >> 5);

	int32 Rotation = (SymbolInfo) & OBJECT_ROTATE90;
	int32 TextRotation = (((*RefInfo >> 8) & 1) + Rotation) & 1;
	int32 Alignment = *RefInfo & 0x0f;

//	char str[200]; //zakázáno

	if ( ((TextRotation == 0) && (Mode == 4) && (Rotation == 0))
		|| ((TextRotation == 1) && (Mode == 8) && (Rotation == 1)) )
	{
		switch (Alignment)
		{
		case ALIGN_LEFT_TOP:
		case ALIGN_LEFT_CENTRE:
		case ALIGN_LEFT_BOTTOM:
			*x += length * DefFontSize * (float)CharWidthFactor;
			*RefInfo &= ~0x0f;
			*RefInfo |= TextMirrorX[Alignment];
			break;

		case ALIGN_RIGHT_TOP:
		case ALIGN_RIGHT_CENTRE:
		case ALIGN_RIGHT_BOTTOM:
			*x -= length * DefFontSize * (float)CharWidthFactor;
			*RefInfo &= ~0x0f;
			*RefInfo |= TextMirrorX[Alignment];
			break;
		}
	}

	if ( ((TextRotation == 1) && (Mode == 8) && (Rotation == 0))
		|| ((TextRotation == 0) && (Mode == 4) && (Rotation == 1)) )
	{
		switch (Alignment)
		{
		case ALIGN_LEFT_TOP:
		case ALIGN_LEFT_CENTRE:
		case ALIGN_LEFT_BOTTOM:
			*y += length * DefFontSize * (float)CharWidthFactor;
			*RefInfo &= ~0x0f;
			*RefInfo |= TextMirrorX[Alignment];
			break;

		case ALIGN_RIGHT_TOP:
		case ALIGN_RIGHT_CENTRE:
		case ALIGN_RIGHT_BOTTOM:
			*y -= length * DefFontSize * (float)CharWidthFactor;
			*RefInfo &= ~0x0f;
			*RefInfo |= TextMirrorX[Alignment];
			break;
		}
	}

}
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RotateFlipPoint(double *x, double *y, double CX, double CY, int32 Mode)
{
	double hx, hy;

	switch (Mode)
	{
	case 1:
//90  :   x= CX+CY-y
//        y=-CX+CY+x
		hx = CX + CY - *y;
		hy = -CX + CY + *x;
		*x = hx;
		*y = hy;
		break;

	case 2:
//180 :   x=2*CX-x
//        y=2*CY-y
		hx = 2 * CX - *x;
		hy = 2 * CY - *y;
		*x = hx;
		*y = hy;
		break;

//270 :   x= CX-CY+y
//        y= CX+CY-x
	case 3:
		hx = CX - CY + *y;
		hy = CX + CY - *x;
		*x = hx;
		*y = hy;
		break;

	case 4:
//   flip x
		hx = 2 * CX - *x;
		*x = hx;
		break;

	case 8:
//   flip y
		hy = 2 * CY - *y;
		*y = hy;
		break;
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RotateFlipPoint2(float *x, float *y, double CX, double CY, int32 Mode)
{
	double hx, hy;

	switch (Mode)
	{
	case 1:
//90  :   x= CX+CY-y
//        y=-CX+CY+x
		hx = CX + CY - *y;
		hy = -CX + CY + *x;
		*x = (float) hx;
		*y = (float) hy;
		break;

	case 2:
//180 :   x=2*CX-x
//        y=2*CY-y
		hx = 2 * CX - *x;
		hy = 2 * CY - *y;
		*x = (float) hx;
		*y = (float) hy;
		break;

//270 :   x= CX-CY+y
//        y= CX+CY-x
	case 3:
		hx = CX - CY + *y;
		hy = CX + CY - *x;
		*x = (float) hx;
		*y = (float) hy;
		break;

	case 4:
//   flip x
		hx = 2 * CX - *x;
		*x = (float) hx;
		break;

	case 8:
//   flip y
		hy = 2 * CY - *y;
		*y = (float) hy;
		break;
	}



}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void PlaceRotatedFlippedComponents(int32 Mode)
{

	/*

	rotate around px,py


	0   :   x=x
	        y=y

	90  :   x= px+py-y
	        y=-px+py+x

	180 :   x=2*px-x
	        y=2*py-y

	270 :   x= px-py+y
	        y= px+py-x



	*/

//  ObjectRecord *Object,*Object2;
	int32 cnt, TempLastActionNr, PlacementMode, Rotation, CircleMode, TextRotation, Alignment, PinRot;
	double CentreX, CentreY, hx, hy;
	int32 Changed, ChangePositionNetlabel = 0, ChangePositionGlobalConnection = 0;
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

	GetNrSelections();

	if ((InstancesSelected + BusConnectionsSelected + WiresSelected + BussesSelected + JunctionsSelected +
	        GlobalConnectionsSelected) != 0)
		ChangePositionNetlabel = 1;

	if ((InstancesSelected + BusConnectionsSelected + WiresSelected + BussesSelected + JunctionsSelected +
	        NetLabelsSelected) != 0)
		ChangePositionGlobalConnection = 1;

	TextRotation = 0;

	CentreX = AdjustToDrawGrid((SearchMinX + SearchMaxX) / 2);
	CentreY = AdjustToDrawGrid((SearchMinY + SearchMaxY) / 2);

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Instance->AddNr <= TempLastActionNr))
		{
			memmove(&NewInstance, Instance, sizeof(InstanceRecord));
			PlacementMode = 0;

			if (Instance->SymbolInfo & OBJECT_ROTATE90)
				PlacementMode |= 4;

			if (Instance->SymbolInfo & OBJECT_MIRRORX)
				PlacementMode |= 1;

			if (Instance->SymbolInfo & OBJECT_MIRRORY)
				PlacementMode |= 2;

			hx = CentreX - CentreSelectedX;
			hy = CentreY - CentreSelectedY;

			if ((NewInstance.Info & 4) == 4)
			{
				RotateFlipPoint2(&NewInstance.OriginX, &NewInstance.OriginY, CentreX, CentreY, Mode);

				switch (Mode)
				{
				case 1:
					switch (PlacementMode)
					{
					case 0:	// Rotation = 0 , MirrorY = 0 , MirrorX = 0
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90;
						break;

					case 1:	// Rotation = 0 , MirrorY = 0 , MirrorX = 1
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90 + OBJECT_MIRRORX + OBJECT_MIRRORY;
						break;

					case 2:	// Rotation = 0 , MirrorY = 1 , MirrorX = 0
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90 + OBJECT_MIRRORX + OBJECT_MIRRORY;
						break;

					case 3:	// Rotation = 0 , MirrorY = 1 , MirrorX = 1
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90;
						break;

					case 4:	// Rotation = 1 , MirrorY = 0 , MirrorX = 0
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90 + OBJECT_MIRRORX + OBJECT_MIRRORY;
						break;

					case 5:	// Rotation = 1 , MirrorY = 0 , MirrorX = 1
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90;
						break;

					case 6:	// Rotation = 1 , MirrorY = 1 , MirrorX = 0
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90;
						break;

					case 7:	// Rotation = 1 , MirrorY = 1 , MirrorX = 1
						NewInstance.SymbolInfo ^= OBJECT_ROTATE90 + OBJECT_MIRRORX + OBJECT_MIRRORY;
						break;
					}

					break;

				case 4:
					NewInstance.SymbolInfo ^= OBJECT_MIRRORX;
					break;

				case 8:
					NewInstance.SymbolInfo ^= OBJECT_MIRRORY;
					break;
				}
			}

			if ((NewInstance.Info & (4 + 1)) == 1)
			{
				Alignment = NewInstance.RefInfo & 0x0f;

				switch (Mode)
				{
				case 1:
					NewInstance.RefInfo ^= (1 << 8);
					break;

				case 4:
					FlipTextAlignment(&NewInstance.RefOriginX, &NewInstance.RefOriginY,	&NewInstance.RefInfo,
										NewInstance.SymbolInfo, strlen(NewInstance.Reference), Mode);
					break;

				case 8:
					FlipTextAlignment(&NewInstance.RefOriginX, &NewInstance.RefOriginY, &NewInstance.RefInfo,
										NewInstance.SymbolInfo, strlen(NewInstance.Reference), Mode);
					break;
				}
			}

			if ((NewInstance.Info & (4 + 2)) == 2)
			{
				Alignment = NewInstance.ValueInfo & 0x0f;

				switch (Mode)
				{
				case 1:
					NewInstance.ValueInfo ^= (1 << 8);
					break;

				case 4:
					FlipTextAlignment(&NewInstance.ValueOriginX, &NewInstance.ValueOriginY, &NewInstance.ValueInfo,
										NewInstance.SymbolInfo, strlen(NewInstance.Value), Mode);
					break;

				case 8:
					FlipTextAlignment(&NewInstance.ValueOriginX, &NewInstance.ValueOriginY, &NewInstance.ValueInfo,
										NewInstance.SymbolInfo, strlen(NewInstance.Value), Mode);
					break;
				}
			}

//      NewInstance.Info=OBJECT_SELECTED|7;
			if (EditingSymbol)
				NewInstance.SymbolInfo &= ~(OBJECT_ROTATE90 + OBJECT_MIRRORX + OBJECT_MIRRORY);

			if (AddInstance(&NewInstance))
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);
				Instance->Info |= OBJECT_NOT_VISIBLE;
				Instance->Info &= ~(OBJECT_SELECTED | 7);
				Instance->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if (((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Wire->AddNr <= TempLastActionNr))
			{
				memmove(&NewWire, Wire, sizeof(WireRecord));

				if ((Wire->Info & 3) != 0)
				{
					RotateFlipPoint2(&NewWire.X1, &NewWire.Y1, CentreX, CentreY, Mode);
					RotateFlipPoint2(&NewWire.X2, &NewWire.Y2, CentreX, CentreY, Mode);
				}

//        NewWire.Info=0;
//        NewWire.Info=OBJECT_SELECTED|3;
				if (CommandAddTryingWire(&NewWire, 1))
				{
					Wire = &((*Wires)[cnt]);
					Wire->Info |= OBJECT_NOT_VISIBLE;
					Wire->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if (((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Bus->AddNr <= TempLastActionNr))
			{
				memmove(&NewBus, Bus, sizeof(BusRecord));

				if ((Bus->Info & 3) != 0)
				{
					RotateFlipPoint2(&NewBus.X1, &NewBus.Y1, CentreX, CentreY, Mode);
					RotateFlipPoint2(&NewBus.X2, &NewBus.Y2, CentreX, CentreY, Mode);
				}

//        NewBus.Info=OBJECT_SELECTED|3;
				if (CommandAddTryingBus(&NewBus, 1))
				{
					Bus = &((*Busses)[cnt]);
					Bus->Info |= OBJECT_NOT_VISIBLE;
					Bus->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if (((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Junction->AddNr <= TempLastActionNr))
			{
				memmove(&NewJunction, Junction, sizeof(JunctionRecord));
				RotateFlipPoint2(&NewJunction.X, &NewJunction.Y, CentreX, CentreY, Mode);
				NewJunction.Info = OBJECT_SELECTED;

				if (AddJunction(&NewJunction))
				{
					Junction = &((*Junctions)[cnt]);
					Junction->Info |= OBJECT_NOT_VISIBLE;
					Junction->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if (((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (OnePinNet->AddNr <= TempLastActionNr))
			{
				memmove(&NewOnePinNet, OnePinNet, sizeof(OnePinNetRecord));
				RotateFlipPoint2(&NewOnePinNet.X, &NewOnePinNet.Y, CentreX, CentreY, Mode);
				NewOnePinNet.Info = OBJECT_SELECTED;

				if (AddOnePinNet(&NewOnePinNet))
				{
					OnePinNet = &((*OnePinNets)[cnt]);
					OnePinNet->Info |= OBJECT_NOT_VISIBLE;
					OnePinNet->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if (((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (BusConnection->AddNr <= TempLastActionNr))
			{
				memmove(&NewBusConnection, BusConnection, sizeof(BusConnectionRecord));

				switch (Mode)
				{
				case 1:
					NewBusConnection.Alignment = (int16) (NewBusConnection.Alignment >> 14);
					NewBusConnection.Alignment += 1;
					NewBusConnection.Alignment = (int16) ((NewBusConnection.Alignment & 3) << 14);
					break;

				case 4:
					if ((NewBusConnection.Alignment & 0x4000) == 0)
					{
						NewBusConnection.Alignment ^= 0x8000;
						NewBusConnection.Alignment += 1;
						NewBusConnection.Alignment = (int16) ((NewBusConnection.Alignment & 3) << 14);
					}

					break;

				case 8:
					if ((NewBusConnection.Alignment & 0x4000) == 0x4000)
					{
						NewBusConnection.Alignment ^= 0x8000;
						NewBusConnection.Alignment += 1;
						NewBusConnection.Alignment = (int16) ((NewBusConnection.Alignment & 3) << 14);
					}

					break;
				}

				RotateFlipPoint2(&NewBusConnection.X, &NewBusConnection.Y, CentreX, CentreY, Mode);
				NewBusConnection.Info = OBJECT_SELECTED;

				if (AddBusConnection(&NewBusConnection))
				{
					BusConnection = &((*BusConnections)[cnt]);
					BusConnection->Info |= OBJECT_NOT_VISIBLE;
					BusConnection->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if (((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (NetLabel->AddNr <= TempLastActionNr))
			{
				memmove(&NewNetLabel, NetLabel, sizeof(NetLabelRecord));
				TextRotation = (NetLabel->Alignment >> 8) & 1;
				Rotation = TextRotation;
				Alignment = NetLabel->Alignment & 0x0f;

				if (ChangePositionNetlabel)
					RotateFlipPoint2(&NewNetLabel.ConnectX, &NewNetLabel.ConnectY, CentreX, CentreY, Mode);

				switch (Mode)
				{
				case 1:
					Rotation ^= 1;

					if (ChangePositionNetlabel)
						RotateFlipPoint2(&NewNetLabel.TextX, &NewNetLabel.TextY, 0.0, 0.0, Mode);

					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

//            if (TextRotation==1) Alignment=TextMirrorY[Alignment];
					break;

				case 4:
					NewNetLabel.TextX = -NewNetLabel.TextX;

					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

//            if (TextRotation==1) Alignment=TextMirrorY[Alignment];
					break;

				case 8:
					NewNetLabel.TextY = -NewNetLabel.TextY;

//            if (TextRotation==0) Alignment=TextMirrorY[Alignment];
					if (TextRotation == 1)
						Alignment = TextMirrorX[Alignment];

					break;
				}

				NewNetLabel.Alignment = (int16) (Alignment + (Rotation << 8));
				NewNetLabel.Info = OBJECT_SELECTED;

				if (AddNetLabel(&NewNetLabel))
				{
					NetLabel = &((*NetLabels)[cnt]);
					NetLabel->Info |= OBJECT_NOT_VISIBLE;
					NetLabel->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if (((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (GlobalConnection->AddNr <= TempLastActionNr))
			{
				memmove(&NewGlobalConnection, GlobalConnection, sizeof(GlobalConnectionRecord));

				if (Mode == 4)
					NewGlobalConnection.ConnectionType ^= 1;

				if (ChangePositionGlobalConnection)
				{
					RotateFlipPoint2(&NewGlobalConnection.X, &NewGlobalConnection.Y, CentreX, CentreY, Mode);
					RotateFlipPoint2(&NewGlobalConnection.NameX, &NewGlobalConnection.NameY, CentreX, CentreY, Mode);
				}

				Alignment = NewGlobalConnection.NameInfo & 15;

				switch (Mode)
				{
				case 1:
					break;

				case 4:
					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

//            if (TextRotation==1) Alignment=TextMirrorY[Alignment];
					break;

				case 8:

//            if (TextRotation==0) Alignment=TextMirrorY[Alignment];
					if (TextRotation == 1)
						Alignment = TextMirrorX[Alignment];

					break;
				}

				NewGlobalConnection.NameInfo = (int16) Alignment;

				NewGlobalConnection.Info = OBJECT_SELECTED | 3;

				if (AddGlobalConnection(&NewGlobalConnection))
				{
					GlobalConnection = &((*GlobalConnections)[cnt]);
					GlobalConnection->Info |= OBJECT_NOT_VISIBLE;
					GlobalConnection->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if (((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Pin->AddNr <= TempLastActionNr))
			{
				memmove(&NewPin, Pin, sizeof(PinRecord));
				RotateFlipPoint2(&NewPin.X, &NewPin.Y, CentreX, CentreY, Mode);
				RotateFlipPoint2(&NewPin.NameX, &NewPin.NameY, CentreX, CentreY, Mode);
				PinRot = (NewPin.NameInfo >> 8) & 1;
				TextRotation = PinRot;
				Alignment = NewPin.NameInfo & 15;

				switch (Mode)
				{
				case 1:
					TextRotation = (PinRot + 1) & 1;

					if (TextRotation == 0)
					{
						Alignment = TextMirrorX[Alignment];
						Alignment = TextMirrorY[Alignment];
					}

					break;

				case 4:
					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];

					break;

				case 8:
					if (TextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorX[Alignment];

					break;
				}

				NewPin.NameInfo = (int16) (Alignment + (TextRotation << 8));
				NewPin.Info = OBJECT_SELECTED | 3;

				if (AddPin(&NewPin))
				{
					Pin = &((*Pins)[cnt]);
					Pin->Info |= OBJECT_NOT_VISIBLE;
					Pin->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if (((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (PowerPin->AddNr <= TempLastActionNr))
			{
				memmove(&NewPowerPin, PowerPin, sizeof(PowerPinRecord));
				RotateFlipPoint2(&NewPowerPin.NameX, &NewPowerPin.NameY, CentreX, CentreY, Mode);
				PinRot = (NewPowerPin.NameInfo >> 8) & 1;
				TextRotation = PinRot;
				Alignment = NewPowerPin.NameInfo & 15;

				switch (Mode)
				{
				case 1:
					TextRotation = (PinRot + 1) & 1;

					if (TextRotation == 0)
					{
						Alignment = TextMirrorX[Alignment];
						Alignment = TextMirrorY[Alignment];
					}

					break;

				case 4:
					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];

					break;

				case 8:
					if (TextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorX[Alignment];

					break;
				}

				NewPowerPin.NameInfo = (int16) (Alignment + (TextRotation << 8));
				NewPowerPin.Info = OBJECT_SELECTED;

				if (AddPowerPin(&NewPowerPin))
				{
					PowerPin = &((*PowerPins)[cnt]);
					PowerPin->Info |= OBJECT_NOT_VISIBLE;
					PowerPin->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if (((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (PinBus->AddNr <= TempLastActionNr))
			{
				memmove(&NewPinBus, PinBus, sizeof(PinBusRecord));
				RotateFlipPoint2(&NewPinBus.X, &NewPinBus.Y, CentreX, CentreY, Mode);
				RotateFlipPoint2(&NewPinBus.NameX, &NewPinBus.NameY, CentreX, CentreY, Mode);
				PinRot = (NewPinBus.NameInfo >> 8) & 1;
				TextRotation = PinRot;
				Alignment = NewPinBus.NameInfo & 15;

				switch (Mode)
				{
				case 1:
					TextRotation = (PinRot + 1) & 1;

					if (TextRotation == 0)
					{
						Alignment = TextMirrorX[Alignment];
						Alignment = TextMirrorY[Alignment];
					}

					break;

				case 4:
					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];

					break;

				case 8:
					if (TextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorX[Alignment];

					break;
				}

				NewPinBus.NameInfo = (int16) (Alignment + (TextRotation << 8));
				NewPinBus.Info = OBJECT_SELECTED | 3;

				if (AddPinBus(&NewPinBus))
				{
					PinBus = &((*PinBusses)[cnt]);
					PinBus->Info |= OBJECT_NOT_VISIBLE;
					PinBus->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));

			if ((ObjectLine->Info & 3) != 0)
			{
				RotateFlipPoint2(&NewObjectLine.X1, &NewObjectLine.Y1, CentreX, CentreY, Mode);
				RotateFlipPoint2(&NewObjectLine.X2, &NewObjectLine.Y2, CentreX, CentreY, Mode);
			}

			NewObjectLine.Info = OBJECT_SELECTED | 3;

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				ObjectLine->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			RotateFlipPoint2(&NewObjectRect.CentreX, &NewObjectRect.CentreY, CentreX, CentreY, Mode);

			if (Mode == 1)
			{
				hx = NewObjectRect.Width;
				NewObjectRect.Width = NewObjectRect.Height;
				NewObjectRect.Height = (float) hx;
			}

			NewObjectRect.Info = OBJECT_SELECTED;

			if (AddObjectRect(&NewObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectCircle->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectCircle, ObjectCircle, sizeof(ObjectCircleRecord));
			RotateFlipPoint2(&NewObjectCircle.CentreX, &NewObjectCircle.CentreY, CentreX, CentreY, Mode);
			CircleMode = NewObjectCircle.CircleMode;

			switch (Mode)
			{
			case 1:
				NewObjectCircle.CircleMode = CircleRotate90[CircleMode];
				break;

			case 2:
				NewObjectCircle.CircleMode = CircleRotate180[CircleMode];
				break;

			case 3:
				NewObjectCircle.CircleMode = CircleRotate270[CircleMode];
				break;

			case 4:
				NewObjectCircle.CircleMode = CircleMirrorX[CircleMode];
				break;

			case 8:
				NewObjectCircle.CircleMode = CircleMirrorY[CircleMode];
				break;
			}

			NewObjectCircle.Info = OBJECT_SELECTED;

			if (AddObjectCircle(&NewObjectCircle))
			{
				ObjectCircle = &((*ObjectCircles)[cnt]);
				ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
				ObjectCircle->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			RotateFlipPoint2(&NewObjectArc.CentreX, &NewObjectArc.CentreY, CentreX, CentreY, Mode);

			switch (Mode)
			{
			case 1:
				hx = NewObjectArc.StartDiffX;
				NewObjectArc.StartDiffX = -NewObjectArc.StartDiffY;
				NewObjectArc.StartDiffY = (float) hx;
				hx = NewObjectArc.EndDiffX;
				NewObjectArc.EndDiffX = -NewObjectArc.EndDiffY;
				NewObjectArc.EndDiffY = (float) hx;
				break;

			case 2:
				NewObjectArc.StartDiffX *= (float) -1.0;
				NewObjectArc.StartDiffY *= (float) -1.0;
				NewObjectArc.EndDiffX *= (float) -1.0;
				NewObjectArc.EndDiffY *= (float) -1.0;
				break;

			case 3:
				hx = NewObjectArc.StartDiffX;
				NewObjectArc.StartDiffX = NewObjectArc.StartDiffY;
				NewObjectArc.StartDiffY = (float) -hx;
				hx = NewObjectArc.EndDiffX;
				NewObjectArc.EndDiffX = NewObjectArc.EndDiffY;
				NewObjectArc.EndDiffY = (float) -hx;
				break;

			case 4:
				NewObjectArc.StartDiffX *= (float) -1.0;
				NewObjectArc.EndDiffX *= (float) -1.0;
				hx = NewObjectArc.StartDiffX;
				NewObjectArc.StartDiffX = NewObjectArc.EndDiffX;
				NewObjectArc.EndDiffX = (float) hx;
				hy = NewObjectArc.StartDiffY;
				NewObjectArc.StartDiffY = NewObjectArc.EndDiffY;
				NewObjectArc.EndDiffY = (float) hy;
				break;

			case 8:
				NewObjectArc.StartDiffY *= (float) -1.0;
				NewObjectArc.EndDiffY *= (float) -1.0;
				hx = NewObjectArc.StartDiffX;
				NewObjectArc.StartDiffX = NewObjectArc.EndDiffX;
				NewObjectArc.EndDiffX = (float) hx;
				hy = NewObjectArc.StartDiffY;
				NewObjectArc.StartDiffY = (float) NewObjectArc.EndDiffY;
				NewObjectArc.EndDiffY = (float) hy;
				break;
			}

			NewObjectArc.Info = OBJECT_SELECTED;

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectText, ObjectText, sizeof(ObjectTextRecord));
			RotateFlipPoint2(&NewObjectText.X, &NewObjectText.Y, CentreX, CentreY, Mode);
			Alignment = NewObjectText.TextMode & 0x0f;
//      TextRotation=(NewObjectText.TextMode >> 8) & 1;
			TextRotation = (int32) (NewObjectText.Rotation / 90.0) & 1;

			switch (Mode)
			{
			case 1:
				if (!EditingSymbol)
				{
					NewObjectText.Rotation += 90.0;

					if (NewObjectText.Rotation >= 360.0)
						NewObjectText.Rotation -= 360.0;
				}
				else
				{
					TextRotation = (TextRotation + 1) & 1;

					if (TextRotation == 0)
					{
						Alignment = TextMirrorX[Alignment];
						Alignment = TextMirrorY[Alignment];
					}
				}

				break;

			case 4:
				if (EditingSymbol)
				{
					if (TextRotation == 0)
						Alignment = TextMirrorX[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorY[Alignment];
				}

				break;

			case 8:
				if (EditingSymbol)
				{
					if (TextRotation == 0)
						Alignment = TextMirrorY[Alignment];

					if (TextRotation == 1)
						Alignment = TextMirrorX[Alignment];
				}

				break;
			}

			if (EditingSymbol)
				NewObjectText.Rotation = (float) (TextRotation * 90.0);

			NewObjectText.TextMode = (int16) Alignment;
			NewObjectText.Info = OBJECT_SELECTED;

			if (AddObjectText(&NewObjectText))
			{
				ObjectText = &((*ObjectTexts)[cnt]);
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				ObjectText->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ClearRefs(int32 mode)
{
	int32 cnt, cnt2, TempLastActionNr, lengte, Changed = 0, Selected = 0, Mask, Value;
	double CurrentX = 0.0, CurrentY = 0.0;
	InstanceRecord *Instance;

	TempLastActionNr = LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		lengte = strlen(Instance->Reference);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			Selected++;
	}

	if (Selected > 0)
	{
		Mask = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
		Value = OBJECT_SELECTED;
	}
	else
	{
		Mask = OBJECT_NOT_VISIBLE;
		Value = 0;
	}

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		lengte = strlen(Instance->Reference);

		if (((Instance->Info & (Mask)) == Value) && (Instance->AddNr <= TempLastActionNr) && (lengte > 1)
		        && (Instance->Reference[lengte - 1] != '?'))
		{
			Instance->Info &= ~OBJECT_SELECTED;
			memmove(&NewInstance, Instance, sizeof(InstanceRecord));
			cnt2 = lengte - 1;

			while ((cnt2 > 0) && (isdigit(NewInstance.Reference[cnt2])))
			{
				NewInstance.Reference[cnt2] = 0;
				cnt2--;
			}

			NewInstance.Reference[cnt2 + 1] = '?';
			NewInstance.Reference[cnt2 + 2] = 0;

			if (AddInstance(&NewInstance))
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);
				Instance->Info |= OBJECT_NOT_VISIBLE;
				Instance->Info &= ~(OBJECT_SELECTED | 7);
				Instance->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
	{
		UnselectAll = 1;
		SelectObjectsFromWindow(CurrentX2, CurrentY2, CurrentX, CurrentY, 0);
		UnselectAll = 0;
		RePaint();
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ScaleObjects(int32 mode)
{

//  ObjectRecord *Object,*Object2;
	int32 cnt, TempLastActionNr, TextAlignment;
	double x1, y1, x2, minx, miny, maxx, maxy, CentreX, CentreY, Rotation;
	float ScaleFactor;
	int32 Changed;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText, ChangedText1;

	ScaleFactor = 1.0;
	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	memset(&ChangedText1, 0, sizeof(ChangedText1));
	sprintf(ChangedText1.Text, "%.4f", ScaleFactor);

	if (TextInputDialog(&ChangedText1, 11) == 2)
		return;

	if ((sscanf((LPSTR) & ChangedText1.Text, "%f", &ScaleFactor)) != 1)
		return;

	minx = 1e6;
	miny = 1e6;
	maxx = -1e6;
	maxy = -1e6;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, min(ObjectLine->X1, ObjectLine->X2));
			miny = min(miny, min(ObjectLine->Y1, ObjectLine->Y2));
			maxx = max(maxx, max(ObjectLine->X1, ObjectLine->X2));
			maxy = max(maxy, max(ObjectLine->Y1, ObjectLine->Y2));
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, ObjectRect->CentreX - ObjectRect->Width * 0.5);
			miny = min(miny, ObjectRect->CentreY - ObjectRect->Height * 0.5);
			maxx = max(maxx, ObjectRect->CentreX + ObjectRect->Width * 0.5);
			maxy = max(maxy, ObjectRect->CentreY + ObjectRect->Height * 0.5);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, ObjectCircle->CentreX - ObjectCircle->Diam * 0.5);
			miny = min(miny, ObjectCircle->CentreY - ObjectCircle->Diam * 0.5);
			maxx = max(maxx, ObjectCircle->CentreX + ObjectCircle->Diam * 0.5);
			maxy = max(maxy, ObjectCircle->CentreY + ObjectCircle->Diam * 0.5);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, ObjectArc->CentreX - ObjectArc->Width * 0.5);
			miny = min(miny, ObjectArc->CentreY - ObjectArc->Height * 0.5);
			maxx = max(maxx, ObjectArc->CentreX + ObjectArc->Width * 0.5);
			maxy = max(maxy, ObjectArc->CentreY + ObjectArc->Height * 0.5);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			x1 = ObjectText->X;
			y1 = ObjectText->Y;
			x2 = ObjectText->FontHeight;
			Rotation = ObjectText->Rotation;
			TextAlignment = ObjectText->TextMode & 0x0f;

			if ((Rotation == 0.0) || (InRangeSpecial(Rotation, 90.0, 0.01)))
			{
				if (Rotation == 0.0)
					GetMinMaxText(x1, y1, x2, 0, 0, TextAlignment, ObjectText->Text);
				else
					GetMinMaxText(x1, y1, x2, 0, 1, TextAlignment, ObjectText->Text);
			}
			else
				GetMinMaxText2(x1, y1, x2, 0, Rotation, TextAlignment, 0, ObjectText->Text);


			minx = min(minx, TextMinX);
			miny = min(miny, TextMinY);
			maxx = max(maxx, TextMaxX);
			maxy = max(maxy, TextMaxY);
		}
	}

	CentreX = (minx + maxx) * 0.5;
	CentreY = (miny + maxy) * 0.5;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			NewObjectLine.X1 = (float) ((ObjectLine->X1 - CentreX) * ScaleFactor + CentreX);
			NewObjectLine.Y1 = (float) ((ObjectLine->Y1 - CentreY) * ScaleFactor + CentreY);
			NewObjectLine.X2 = (float) ((ObjectLine->X2 - CentreX) * ScaleFactor + CentreX);
			NewObjectLine.Y2 = (float) ((ObjectLine->Y2 - CentreY) * ScaleFactor + CentreY);
			NewObjectLine.Info = 0;

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				ObjectLine->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			NewObjectRect.CentreX = (float) ((ObjectRect->CentreX - CentreX) * ScaleFactor + CentreX);
			NewObjectRect.CentreY = (float) ((ObjectRect->CentreY - CentreY) * ScaleFactor + CentreY);
			NewObjectRect.Width *= (float) ScaleFactor;
			NewObjectRect.Height *= (float) ScaleFactor;

			if (AddObjectRect(&NewObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectCircle->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectCircle, ObjectCircle, sizeof(ObjectCircleRecord));
			NewObjectCircle.CentreX = (float) ((ObjectCircle->CentreX - CentreX) * ScaleFactor + CentreX);
			NewObjectCircle.CentreY = (float) ((ObjectCircle->CentreY - CentreY) * ScaleFactor + CentreY);
			NewObjectCircle.Diam *= (float) ScaleFactor;
			NewObjectCircle.Info = 0;

			if (AddObjectCircle(&NewObjectCircle))
			{
				ObjectCircle = &((*ObjectCircles)[cnt]);
				ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
				ObjectCircle->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			NewObjectArc.CentreX = (float) ((ObjectArc->CentreX - CentreX) * ScaleFactor + CentreX);
			NewObjectArc.CentreY = (float) ((ObjectArc->CentreY - CentreY) * ScaleFactor + CentreY);
			NewObjectArc.Width *= (float) ScaleFactor;
			NewObjectArc.Height *= (float) ScaleFactor;
			NewObjectArc.Info = 0;

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectText, ObjectText, sizeof(ObjectTextRecord));
			NewObjectText.X = (float) ((ObjectText->X - CentreX) * ScaleFactor + CentreX);
			NewObjectText.Y = (float) ((ObjectText->Y - CentreY) * ScaleFactor + CentreY);
			NewObjectText.FontHeight *= (float) ScaleFactor;
			NewObjectText.Info = 0;

			if (AddObjectText(&NewObjectText))
			{
				ObjectText = &((*ObjectTexts)[cnt]);
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				ObjectText->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void AlignTextObjects(int32 mode)
{
	/*
	  mode:

	  0 = Align left
	  1 = Align right
	*/
//  ObjectRecord *Object,*Object2;
	int32 cnt, TempLastActionNr, RotationInt, count;
	double x1, lengte2;
	int32 Changed, Found;
	NetLabelRecord *NetLabel;
	ObjectTextRecord *ObjectText;
	PinRecord *Pin;
	PinBusRecord *PinBus;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	if (!EditingSymbol)
	{
		RotationInt = -1;
		Found = 1;
		count = 0;
		x1 = 0.0;

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (((NetLabel->Alignment >> 8) & 0x01) == 1)
					Found = 0;

				count++;
				lengte2 = strlen(NetLabel->Name);

				if (mode == 0)
				{
					x1 += NetLabel->ConnectX + NetLabel->TextX;

					if (NetLabel->Alignment == ALIGN_RIGHT_BOTTOM)
						x1 += lengte2 * DefFontSize * CharWidthFactor;
				}
				else
				{
					x1 += NetLabel->ConnectX + NetLabel->TextX;

					if (NetLabel->Alignment == ALIGN_LEFT_BOTTOM)
						x1 += lengte2 * DefFontSize * CharWidthFactor;
				}
			}
		}

		if (Found)
		{
			x1 = AdjustToGrid(x1 / count, GridSize);

			for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
			{
				NetLabel = &((*NetLabels)[cnt]);

				if (((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (NetLabel->AddNr <= TempLastActionNr))
				{
					memmove(&NewNetLabel, NetLabel, sizeof(NetLabelRecord));
					NewNetLabel.TextX = (float) (x1 - NetLabel->ConnectX);
					NewNetLabel.Alignment &= ~0x0f;

					if (mode == 1)
						NewNetLabel.Alignment = ALIGN_RIGHT_BOTTOM;

					NewNetLabel.Info = 0;

					if (AddNetLabel(&NewNetLabel))
					{
						NetLabel = &((*NetLabels)[cnt]);
						NetLabel->Info |= OBJECT_NOT_VISIBLE;
						NetLabel->DeleteNr = (int16) LastActionNr;
					}
				}
			}
		}
	}
	else
	{
		Found = 1;
		count = 0;
		x1 = 0.0;

		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (((Pin->NameInfo >> 8) & 0x01) == 1)
					Found = 0;

				count++;

				if (!EditingSheetSymbol)
					lengte2 = strlen(Pin->Name);
				else
					lengte2 = strlen(Pin->Label);

				x1 += Pin->NameX;

				if (mode == 0)
				{
					if ((Pin->NameInfo & 0x0f) == ALIGN_RIGHT_BOTTOM)
						x1 += lengte2 * DefFontSize * CharWidthFactor;
				}
				else
				{
					if ((Pin->NameInfo & 0x0f) == ALIGN_LEFT_BOTTOM)
						x1 += lengte2 * DefFontSize * CharWidthFactor;
				}
			}
		}

		if (Found)
		{
			x1 = AdjustToGrid(x1 / count, GridSize);

			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if (((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (((Pin->NameInfo >> 8) & 0x01) == 1) && (Pin->AddNr <= TempLastActionNr))
				{
					memmove(&NewPin, Pin, sizeof(PinRecord));
					NewPin.NameInfo &= ~0x0f;

					if (mode == 1)
						NewPin.NameInfo |= ALIGN_RIGHT_BOTTOM;

					NewPin.NameX = (float) x1;
					NewPin.Info = 0;

					if (AddPin(&NewPin))
					{
						Pin = &((*Pins)[cnt]);
						Pin->Info |= OBJECT_NOT_VISIBLE;
						Pin->DeleteNr = (int16) LastActionNr;
						Changed = 1;
					}
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if (((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (((PinBus->NameInfo >> 8) & 0x01) == 1) && (PinBus->AddNr <= TempLastActionNr))
			{
				memmove(&NewPinBus, PinBus, sizeof(PinBusRecord));
				NewPinBus.NameInfo &= ~0x0f;

				if (mode == 1)
					NewPinBus.NameInfo |= ALIGN_RIGHT_BOTTOM;

				NewPinBus.Info = 0;

				if (AddPinBus(&NewPinBus))
				{
					PinBus = &((*PinBusses)[cnt]);
					PinBus->Info |= OBJECT_NOT_VISIBLE;
					PinBus->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}
	}

	Found = 1;
	count = 0;
	x1 = 0.0;

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (ObjectText->Rotation != 0.0)
				Found = 0;

			x1 += ObjectText->X;
			lengte2 = strlen(ObjectText->Text);

			if (mode == 0)
			{
				if ((ObjectText->TextMode & 0x0f) == ALIGN_RIGHT_BOTTOM)
					x1 += lengte2 * ObjectText->FontHeight * DefFontSize * CharWidthFactor;
			}
			else
			{
				if ((ObjectText->TextMode & 0x0f) == ALIGN_LEFT_BOTTOM)
					x1 += lengte2 * ObjectText->FontHeight * DefFontSize * CharWidthFactor;
			}

			count++;
		}
	}

	if (Found)
	{
		x1 = AdjustToGrid(x1 / count, GridSize);

		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			ObjectText = &((*ObjectTexts)[cnt]);

			if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectText->Rotation == 0.0) && (ObjectText->AddNr <= TempLastActionNr))
			{
				memmove(&NewObjectText, ObjectText, sizeof(ObjectTextRecord));
				NewObjectText.X = (float) x1;
				NewObjectText.TextMode &= ~0x0f;

				if (mode == 1)
					NewObjectText.TextMode |= ALIGN_RIGHT_BOTTOM;

				NewObjectText.Info = 0;

				if (AddObjectText(&NewObjectText))
				{
					ObjectText = &((*ObjectTexts)[cnt]);
					ObjectText->Info |= OBJECT_NOT_VISIBLE;
					ObjectText->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}
	}

	UnselectAll = 1;
	SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
	LastAction = 0;
	UnselectAll = 0;

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ChangeLineWidthObjects(int32 mode)
{

//  ObjectRecord *Object,*Object2;
	int32 cnt, TempLastActionNr;
	float NewThickness;
	int32 Changed;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText, ChangedText1;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;
	NewThickness = (float) STANDARD_LINE_THICKNESS;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NewThickness = ObjectLine->Thickness;
			Changed = 1;
			break;
		}
	}

	if (!Changed)
	{
		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				NewThickness = ObjectRect->Thickness;
				Changed = 1;
				break;
			}
		}
	}

	if (!Changed)
	{
		for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
		{
			ObjectCircle = &((*ObjectCircles)[cnt]);

			if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				NewThickness = ObjectCircle->Thickness;
				Changed = 1;
				break;
			}
		}
	}

	if (!Changed)
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				NewThickness = ObjectArc->Thickness;
				Changed = 1;
				break;
			}
		}
	}

	if (!Changed)
	{
		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			ObjectText = &((*ObjectTexts)[cnt]);

			if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				NewThickness = ObjectText->Thickness;
				Changed = 1;
				break;
			}
		}
	}

	Changed = 0;
	memset(&ChangedText1, 0, sizeof(ChangedText1));
	sprintf(ChangedText1.Text, "%.4f", NewThickness);

	if (TextInputDialog(&ChangedText1, 9) == 2)
		return;

	if ((sscanf((LPSTR) & ChangedText1.Text, "%f", &NewThickness)) != 1)
		return;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			NewObjectLine.Thickness = (float) NewThickness;
			NewObjectLine.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				ObjectLine->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			NewObjectRect.Thickness = (float) NewThickness;
			NewObjectRect.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectRect(&NewObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectCircle->AddNr <= TempLastActionNr))
		{
			NewObjectCircle.Thickness = (float) NewThickness;
			NewObjectCircle.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectCircle(&NewObjectCircle))
			{
				ObjectCircle = &((*ObjectCircles)[cnt]);
				ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
				ObjectCircle->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			NewObjectArc.Thickness = (float) NewThickness;
			NewObjectArc.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectText, ObjectText, sizeof(ObjectTextRecord));
			NewObjectText.Thickness = (float) NewThickness;
			NewObjectText.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectText(&NewObjectText))
			{
				ObjectText = &((*ObjectTexts)[cnt]);
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				ObjectText->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RotateObjects(int32 mode)
{
	int32 cnt, TempLastActionNr, TextAlignment;
	double x1, y1, x2, x3, y3, x4, y4, CentreX, CentreY, minx, miny, maxx, maxy;
	float Rotation;
	int32 Changed;
	ObjectLineRecord *ObjectLine, NewObjectLine2, NewObjectLine3, NewObjectLine4;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText, ChangedText1;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;
	Rotation = 0.0;
	CentreX = 0.0;
	CentreY = 0.0;

	memset(&ChangedText1, 0, sizeof(ChangedText1));

	if (TextInputDialog(&ChangedText1, 10) == 2)
		return;

	if ((sscanf((LPSTR) & ChangedText1.Text, "%f", &Rotation)) != 1)
		return;

	minx = 1e6;
	miny = 1e6;
	maxx = -1e6;
	maxy = -1e6;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, min(ObjectLine->X1, ObjectLine->X2));
			miny = min(miny, min(ObjectLine->Y1, ObjectLine->Y2));
			maxx = max(maxx, max(ObjectLine->X1, ObjectLine->X2));
			maxy = max(maxy, max(ObjectLine->Y1, ObjectLine->Y2));
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, ObjectRect->CentreX - ObjectRect->Width * 0.5);
			miny = min(miny, ObjectRect->CentreY - ObjectRect->Height * 0.5);
			maxx = max(maxx, ObjectRect->CentreX + ObjectRect->Width * 0.5);
			maxy = max(maxy, ObjectRect->CentreY + ObjectRect->Height * 0.5);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, ObjectCircle->CentreX - ObjectCircle->Diam * 0.5);
			miny = min(miny, ObjectCircle->CentreY - ObjectCircle->Diam * 0.5);
			maxx = max(maxx, ObjectCircle->CentreX + ObjectCircle->Diam * 0.5);
			maxy = max(maxy, ObjectCircle->CentreY + ObjectCircle->Diam * 0.5);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			minx = min(minx, ObjectArc->CentreX - ObjectArc->Width * 0.5);
			miny = min(miny, ObjectArc->CentreY - ObjectArc->Height * 0.5);
			maxx = max(maxx, ObjectArc->CentreX + ObjectArc->Width * 0.5);
			maxy = max(maxy, ObjectArc->CentreY + ObjectArc->Height * 0.5);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{

			x1 = ObjectText->X;
			y1 = ObjectText->Y;
			x2 = ObjectText->FontHeight;
			Rotation = ObjectText->Rotation;
			TextAlignment = ObjectText->TextMode & 0x0f;

			if ((Rotation == 0.0) || (InRangeSpecial(Rotation, 90.0, 0.01)))
			{
				if (Rotation == 0.0)
					GetMinMaxText(x1, y1, x2, 0, 0, TextAlignment, ObjectText->Text);
				else
					GetMinMaxText(x1, y1, x2, 0, 1, TextAlignment, ObjectText->Text);
			}
			else
				GetMinMaxText2(x1, y1, x2, 0, Rotation, TextAlignment, 0, ObjectText->Text);

			minx = min(minx, TextMinX);
			miny = min(miny, TextMinY);
			maxx = max(maxx, TextMaxX);
			maxy = max(maxy, TextMaxY);
		}
	}

	CentreX = (minx + maxx) * 0.5;
	CentreY = (miny + maxy) * 0.5;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			RotatePointFromOtherPoint2(&NewObjectLine.X1, &NewObjectLine.Y1, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine.X2, &NewObjectLine.Y2, CentreX, CentreY, Rotation);
			NewObjectLine.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				ObjectLine->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->AddNr <= TempLastActionNr))
		{
			memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
			NewObjectLine.Thickness = ObjectRect->Thickness;
			NewObjectLine.LineMode = ObjectRect->LineMode;
			memmove(&NewObjectLine2, &NewObjectLine, sizeof(ObjectLineRecord));
			memmove(&NewObjectLine3, &NewObjectLine, sizeof(ObjectLineRecord));
			memmove(&NewObjectLine4, &NewObjectLine, sizeof(ObjectLineRecord));
			NewObjectLine.X1 = (float) (ObjectRect->CentreX - ObjectRect->Width * 0.5);
			NewObjectLine.Y1 = (float) (ObjectRect->CentreY - ObjectRect->Height * 0.5);
			NewObjectLine.X2 = (float) (ObjectRect->CentreX - ObjectRect->Width * 0.5);
			NewObjectLine.Y2 = (float) (ObjectRect->CentreY + ObjectRect->Height * 0.5);

			NewObjectLine2.X1 = (float) (ObjectRect->CentreX - ObjectRect->Width * 0.5);
			NewObjectLine2.Y1 = (float) (ObjectRect->CentreY + ObjectRect->Height * 0.5);
			NewObjectLine2.X2 = (float) (ObjectRect->CentreX + ObjectRect->Width * 0.5);
			NewObjectLine2.Y2 = (float) (ObjectRect->CentreY + ObjectRect->Height * 0.5);

			NewObjectLine3.X1 = (float) (ObjectRect->CentreX + ObjectRect->Width * 0.5);
			NewObjectLine3.Y1 = (float) (ObjectRect->CentreY + ObjectRect->Height * 0.5);
			NewObjectLine3.X2 = (float) (ObjectRect->CentreX + ObjectRect->Width * 0.5);
			NewObjectLine3.Y2 = (float) (ObjectRect->CentreY - ObjectRect->Height * 0.5);

			NewObjectLine4.X1 = (float) (ObjectRect->CentreX + ObjectRect->Width * 0.5);
			NewObjectLine4.Y1 = (float) (ObjectRect->CentreY - ObjectRect->Height * 0.5);
			NewObjectLine4.X2 = (float) (ObjectRect->CentreX - ObjectRect->Width * 0.5);
			NewObjectLine4.Y2 = (float) (ObjectRect->CentreY - ObjectRect->Height * 0.5);
			RotatePointFromOtherPoint2(&NewObjectLine.X1, &NewObjectLine.Y1, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine.X2, &NewObjectLine.Y2, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine2.X1, &NewObjectLine2.Y1, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine2.X2, &NewObjectLine2.Y2, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine3.X1, &NewObjectLine3.Y1, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine3.X2, &NewObjectLine3.Y2, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine4.X1, &NewObjectLine4.Y1, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectLine4.X2, &NewObjectLine4.Y2, CentreX, CentreY, Rotation);

			if ((AddObjectLine(&NewObjectLine)) && (AddObjectLine(&NewObjectLine2)) && (AddObjectLine(&NewObjectLine3))
			        && (AddObjectLine(&NewObjectLine4)))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectCircle->AddNr <= TempLastActionNr))
		{
			memset(&NewObjectArc, 0, sizeof(ObjectArcRecord));
			NewObjectArc.LineMode = ObjectCircle->LineMode;
			NewObjectArc.Width = ObjectCircle->Diam;
			NewObjectArc.Height = ObjectCircle->Diam;
			NewObjectArc.Thickness = ObjectCircle->Thickness;
			x3 = 0.0;
			y3 = 0.0;
			x4 = 0.0;
			y4 = 0.0;

			switch (ObjectCircle->CircleMode)
			{
			case 1:
				x3 = NewObjectArc.Width;
				y4 = NewObjectArc.Width;
				break;

			case 2:
				y3 = NewObjectArc.Width;
				x4 = -NewObjectArc.Width;
				break;

			case 3:
				y3 = -NewObjectArc.Width;
				y4 = NewObjectArc.Width;
				break;

			case 4:
				x3 = -NewObjectArc.Width;
				y4 = -NewObjectArc.Width;
				break;

			case 6:
				x3 = -NewObjectArc.Width;
				x4 = NewObjectArc.Width;
				break;

			case 8:
				y3 = -NewObjectArc.Width;
				x4 = NewObjectArc.Width;
				break;

			case 9:
				x3 = NewObjectArc.Width;
				x4 = -NewObjectArc.Width;
				break;

			case 12:
				y3 = NewObjectArc.Width;
				y4 = -NewObjectArc.Width;
				break;

			default:
				y3 = NewObjectArc.Width;
				y4 = NewObjectArc.Width;
				break;
			}

			NewObjectArc.StartDiffX = (float) x3;
			NewObjectArc.StartDiffX = (float) y3;
			NewObjectArc.EndDiffX = (float) x4;
			NewObjectArc.EndDiffY = (float) y4;
			RotatePointFromOtherPoint2(&NewObjectArc.CentreX, &NewObjectArc.CentreY, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectArc.StartDiffX, &NewObjectArc.StartDiffY, 0.0, 0.0, Rotation);
			RotatePointFromOtherPoint2(&NewObjectArc.EndDiffX, &NewObjectArc.EndDiffY, 0.0, 0.0, Rotation);

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectCircle = &((*ObjectCircles)[cnt]);
				ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
				ObjectCircle->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			RotatePointFromOtherPoint2(&NewObjectArc.CentreX, &NewObjectArc.CentreY, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint2(&NewObjectArc.StartDiffX, &NewObjectArc.StartDiffY, 0.0, 0.0, Rotation);
			RotatePointFromOtherPoint2(&NewObjectArc.EndDiffX, &NewObjectArc.EndDiffY, 0.0, 0.0, Rotation);
			NewObjectArc.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectText, ObjectText, sizeof(ObjectTextRecord));
			RotatePointFromOtherPoint2(&NewObjectText.X, &NewObjectText.Y, CentreX, CentreY, Rotation);
			NewObjectText.Info &= ~(OBJECT_SELECTED | 3);
			NewObjectText.Rotation += (float) Rotation;

			if (AddObjectText(&NewObjectText))
			{
				ObjectText = &((*ObjectTexts)[cnt]);
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				ObjectText->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ChangeTextHeight(int32 mode)
{
	int32 cnt, TempLastActionNr;
	float NewHeight;
	int32 Changed;
	ObjectTextRecord *ObjectText, ChangedText1;

	NewHeight = 1.0;

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NewHeight = ObjectText->FontHeight;
			break;
		}
	}

	memset(&ChangedText1, 0, sizeof(ChangedText1));
	sprintf(ChangedText1.Text, "%.4f", NewHeight);

	if (TextInputDialog(&ChangedText1, 12) == 2)
		return;

	if ((sscanf((LPSTR) & ChangedText1.Text, "%f", &NewHeight)) != 1)
		return;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectText, ObjectText, sizeof(ObjectTextRecord));
			NewObjectText.FontHeight = (float) NewHeight;
			NewObjectText.Info = 0;

			if (AddObjectText(&NewObjectText))
			{
				ObjectText = &((*ObjectTexts)[cnt]);
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				ObjectText->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ChangeLineStyle(int32 mode)
{
	int32 cnt, TempLastActionNr;
	int32 Changed;
	ObjectLineRecord *ObjectLine;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			NewObjectLine.LineMode = (int16) mode;
			NewObjectLine.Info &= ~(OBJECT_SELECTED | 3);

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				ObjectLine->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
