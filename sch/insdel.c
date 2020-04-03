/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: insdel.c
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
#include "insdel.h"
#include "calc.h"
#include "calcdef.h"
#include "calcrect.h"
#include "graphics.h"
#include "draw.h"
#include "draw2.h"
#include "sch.h"
#include "files.h"
#include "toets.h"
#include "mainloop.h"
#include "utf8.h"
#include "math.h"
#include "stdio.h"


typedef struct
{
	double X, Y, Distance;
	int32 Pos;
} LineJunctionRecord;

LineJunctionRecord LineJunctions[128];

int32 FirstCheckJunctions = 1, ok;
JunctionRecord ExtraJunction;

// ********************************************************************************************************
// ********************************************************************************************************
// ***************************************************************7*****************************************
// ********************************************************************************************************

int32 JunctionCheckWire(int32 WireCnt, int32 mode);

// ********************************************************************************************************
// ********************************************************************************************************
// ***************************************************************7*****************************************
// ********************************************************************************************************

InstanceRecord *FindFirstInstance(int32 mode)
{
	int32 cnt;
	InstanceRecord *Instance;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
			return Instance;
	}

	return (InstanceRecord *) & ((*Instances)[0]);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ZeroUnusedObjects(int32 mode)
{
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

	if (((mode == 0) && (MaxLastActionNr > LastActionNr)) || (mode == 1))
	{
		if (!EditingSymbol)
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if (mode == 0)
				{
					if (Instance->AddNr >= LastActionNr)
						Instance->AddNr = 0;

					if (Instance->DeleteNr >= LastActionNr)
						Instance->DeleteNr = 0;
				}
				else
				{
					if (Instance->AddNr == LastActionNr)
					{
						Instance->AddNr = 0;
						Instance->DeleteNr = 0;
					}

					if (Instance->DeleteNr == LastActionNr)
					{
						Instance->AddNr = 0;
						Instance->DeleteNr = 0;
					}
				}
			}
		}


		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if (mode == 0)
			{
				if (Wire->AddNr >= LastActionNr)
					Wire->AddNr = 0;

				if (Wire->DeleteNr >= LastActionNr)
					Wire->DeleteNr = 0;
			}
			else
			{
				if (Wire->AddNr == LastActionNr)
				{
					Wire->AddNr = 0;
					Wire->DeleteNr = 0;
				}

				if (Wire->DeleteNr == LastActionNr)
				{
					Wire->DeleteNr = 0;
					Wire->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if (mode == 0)
			{
				if (Bus->AddNr >= LastActionNr)
					Bus->AddNr = 0;

				if (Bus->DeleteNr >= LastActionNr)
					Bus->DeleteNr = 0;
			}
			else
			{
				if (Bus->AddNr == LastActionNr)
				{
					Bus->AddNr = 0;
					Bus->DeleteNr = 0;
				}

				if (Bus->DeleteNr == LastActionNr)
				{
					Bus->AddNr = 0;
					Bus->DeleteNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if (mode == 0)
			{
				if (Junction->AddNr >= LastActionNr)
					Junction->AddNr = 0;

				if (Junction->DeleteNr >= LastActionNr)
					Junction->DeleteNr = 0;
			}
			else
			{
				if (Junction->AddNr == LastActionNr)
				{
					Junction->AddNr = 0;
					Junction->DeleteNr = 0;
				}

				if (Junction->DeleteNr == LastActionNr)
				{
					Junction->AddNr = 0;
					Junction->DeleteNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if (mode == 0)
			{
				if (OnePinNet->AddNr >= LastActionNr)
					OnePinNet->AddNr = 0;

				if (OnePinNet->DeleteNr >= LastActionNr)
					OnePinNet->DeleteNr = 0;
			}
			else
			{
				if (OnePinNet->AddNr == LastActionNr)
				{
					OnePinNet->AddNr = 0;
					OnePinNet->DeleteNr = 0;
				}

				if (OnePinNet->DeleteNr == LastActionNr)
				{
					OnePinNet->AddNr = 0;
					OnePinNet->DeleteNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if (mode == 0)
			{
				if (BusConnection->AddNr >= LastActionNr)
					BusConnection->AddNr = 0;

				if (BusConnection->DeleteNr >= LastActionNr)
					BusConnection->DeleteNr = 0;
			}
			else
			{
				if (BusConnection->AddNr == LastActionNr)
				{
					BusConnection->DeleteNr = 0;
					BusConnection->AddNr = 0;
				}

				if (BusConnection->DeleteNr == LastActionNr)
				{
					BusConnection->DeleteNr = 0;
					BusConnection->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if (mode == 0)
			{
				if (GlobalConnection->AddNr >= LastActionNr)
					GlobalConnection->AddNr = 0;

				if (GlobalConnection->DeleteNr >= LastActionNr)
					GlobalConnection->DeleteNr = 0;
			}
			else
			{
				if (GlobalConnection->AddNr == LastActionNr)
				{
					GlobalConnection->DeleteNr = 0;
					GlobalConnection->AddNr = 0;
				}

				if (GlobalConnection->DeleteNr == LastActionNr)
				{
					GlobalConnection->AddNr = 0;
					GlobalConnection->DeleteNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if (mode == 0)
			{
				if (NetLabel->AddNr >= LastActionNr)
					NetLabel->AddNr = 0;

				if (NetLabel->DeleteNr >= LastActionNr)
					NetLabel->DeleteNr = 0;
			}
			else
			{
				if (NetLabel->AddNr == LastActionNr)
				{
					NetLabel->DeleteNr = 0;
					NetLabel->AddNr = 0;
				}

				if (NetLabel->DeleteNr == LastActionNr)
				{
					NetLabel->DeleteNr = 0;
					NetLabel->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (mode == 0)
			{
				if (ObjectLine->AddNr >= LastActionNr)
					ObjectLine->AddNr = 0;

				if (ObjectLine->DeleteNr >= LastActionNr)
					ObjectLine->DeleteNr = 0;
			}
			else
			{
				if (ObjectLine->AddNr == LastActionNr)
				{
					ObjectLine->AddNr = 0;
					ObjectLine->DeleteNr = 0;
				}

				if (ObjectLine->DeleteNr == LastActionNr)
				{
					ObjectLine->AddNr = 0;
					ObjectLine->DeleteNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if (mode == 0)
			{
				if (ObjectRect->AddNr >= LastActionNr)
					ObjectRect->AddNr = 0;

				if (ObjectRect->DeleteNr >= LastActionNr)
					ObjectRect->DeleteNr = 0;
			}
			else
			{
				if (ObjectRect->AddNr == LastActionNr)
				{
					ObjectRect->DeleteNr = 0;
					ObjectRect->AddNr = 0;
				}

				if (ObjectRect->DeleteNr == LastActionNr)
				{
					ObjectRect->DeleteNr = 0;
					ObjectRect->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
		{
			ObjectCircle = &((*ObjectCircles)[cnt]);

			if (mode == 0)
			{
				if (ObjectCircle->AddNr >= LastActionNr)
					ObjectCircle->AddNr = 0;

				if (ObjectCircle->DeleteNr >= LastActionNr)
					ObjectCircle->DeleteNr = 0;
			}
			else
			{
				if (ObjectCircle->AddNr == LastActionNr)
				{
					ObjectCircle->DeleteNr = 0;
					ObjectCircle->AddNr = 0;
				}

				if (ObjectCircle->DeleteNr == LastActionNr)
				{
					ObjectCircle->DeleteNr = 0;
					ObjectCircle->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (mode == 0)
			{
				if (ObjectArc->AddNr >= LastActionNr)
					ObjectArc->AddNr = 0;

				if (ObjectArc->DeleteNr >= LastActionNr)
					ObjectArc->DeleteNr = 0;
			}
			else
			{
				if (ObjectArc->AddNr == LastActionNr)
				{
					ObjectArc->DeleteNr = 0;
					ObjectArc->AddNr = 0;
				}

				if (ObjectArc->DeleteNr == LastActionNr)
				{
					ObjectArc->DeleteNr = 0;
					ObjectArc->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			ObjectText = &((*ObjectTexts)[cnt]);

			if (mode == 0)
			{
				if (ObjectText->AddNr >= LastActionNr)
					ObjectText->AddNr = 0;

				if (ObjectText->DeleteNr >= LastActionNr)
					ObjectText->DeleteNr = 0;
			}
			else
			{
				if (ObjectText->AddNr == LastActionNr)
				{
					ObjectText->DeleteNr = 0;
					ObjectText->AddNr = 0;
				}

				if (ObjectText->DeleteNr == LastActionNr)
				{
					ObjectText->DeleteNr = 0;
					ObjectText->AddNr = 0;
				}
			}
		}



		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if (mode == 0)
			{
				if (Pin->AddNr >= LastActionNr)
					Pin->AddNr = 0;

				if (Pin->DeleteNr >= LastActionNr)
					Pin->DeleteNr = 0;
			}
			else
			{
				if (Pin->AddNr == LastActionNr)
				{
					Pin->DeleteNr = 0;
					Pin->AddNr = 0;
				}

				if (Pin->DeleteNr == LastActionNr)
				{
					Pin->DeleteNr = 0;
					Pin->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if (mode == 0)
			{
				if (PowerPin->AddNr >= LastActionNr)
					PowerPin->AddNr = 0;

				if (PowerPin->DeleteNr >= LastActionNr)
					PowerPin->DeleteNr = 0;
			}
			else
			{
				if (PowerPin->AddNr == LastActionNr)
				{
					PowerPin->AddNr = 0;
					PowerPin->DeleteNr = 0;
				}

				if (PowerPin->DeleteNr == LastActionNr)
				{
					PowerPin->AddNr = 0;
					PowerPin->DeleteNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if (mode == 0)
			{
				if (PinBus->AddNr >= LastActionNr)
					PinBus->AddNr = 0;

				if (PinBus->DeleteNr >= LastActionNr)
					PinBus->DeleteNr = 0;
			}
			else
			{
				if (PinBus->AddNr == LastActionNr)
				{
					PinBus->DeleteNr = 0;
					PinBus->AddNr = 0;
				}

				if (PinBus->DeleteNr == LastActionNr)
				{
					PinBus->DeleteNr = 0;
					PinBus->AddNr = 0;
				}
			}
		}
	}

	if (mode == 0)
		MaxLastActionNr = LastActionNr;

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeleteSelectedObjects(int32 Mode)
{
	int32 cnt, cnt2, cnt3, Changed, points1, points2;
	double x1, y1, x2, y2;
	ObjectRecord *Object;

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
//  SubSheetRecord *SubSheet;
	GlobalConnectionRecord *GlobalConnection;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;
	RedefinedPinBusRecord *RedefinedPinBus;

	Changed = 0;

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ZeroUnusedObjects(0);
				Instance->Info |= OBJECT_NOT_VISIBLE;
				Instance->DeleteNr = (int16) LastActionNr;
				DataBaseChanged = 1;
				Changed = 1;

				for (cnt2 = 0; cnt2 < Design.NrRedefinedPinBusses; cnt2++)
				{
					RedefinedPinBus = &((*RedefinedPinBusses)[cnt2]);

					if (((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
					        && (stricmpUTF8(RedefinedPinBus->Reference, Instance->Reference) == 0))
					{
						NrObjects = 0;
						InstanceToObject(Instance, 0.0, 0.0, 0);

						for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
						{
							Object = &((*Objects)[cnt3]);

							if (Object->ObjectType == SYMBOL_PINBUS_TEXT)
							{
								if (stricmpUTF8(RedefinedPinBus->Reference, Object->Text2) == 0)
								{
									RedefinedPinBus->Info |= OBJECT_NOT_VISIBLE;
									RedefinedPinBus->DeleteNr = (int16) LastActionNr;
								}
							}
						}
					}
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SetBackGroundActive(0);
			DrawWire(Wire, (double) 0.0, (double) 0.0, 1);
			ZeroUnusedObjects(0);
			Wire->Info |= OBJECT_NOT_VISIBLE;
			Wire->DeleteNr = (int16) LastActionNr;
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;

			for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
			{
				NetLabel = &((*NetLabels)[cnt2]);

				if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, NetLabel->ConnectX)) && (InRange(y1, NetLabel->ConnectY)))
					        || ((InRange(x2, NetLabel->ConnectX)) && (InRange(y2, NetLabel->ConnectY))))
					{
						Changed = 1;
						NetLabel->Info |= OBJECT_NOT_VISIBLE;
						NetLabel->DeleteNr = (int16) LastActionNr;
					}
				}
			}

			points1 = GetNrCrossingFromWirePoint(x1, y1);
			points2 = GetNrCrossingFromWirePoint(x2, y2);

			if (points1 < 3)
			{
				for (cnt2 = 0; cnt2 < Design.NrJunctions; cnt2++)
				{
					Junction = &((*Junctions)[cnt2]);

					if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((InRange(Junction->X, x1)) && (InRange(Junction->Y, y1)))
						{
							Junction->Info |= OBJECT_NOT_VISIBLE;
							Junction->DeleteNr = (int16) LastActionNr;
							DataBaseChanged = 1;
							Changed = 1;
						}
					}
				}
			}

			if (points2 < 3)
			{
				for (cnt2 = 0; cnt2 < Design.NrJunctions; cnt2++)
				{
					Junction = &((*Junctions)[cnt2]);

					if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((InRange(Junction->X, x2)) && (InRange(Junction->Y, y2)))
						{
							Junction->Info |= OBJECT_NOT_VISIBLE;
							Junction->DeleteNr = (int16) LastActionNr;
							DataBaseChanged = 1;
							Changed = 1;
						}
					}
				}
			}

			Changed = 1;
			DataBaseChanged = 1;
		}
	}

//  InitDrawingBusses();
	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SetBackGroundActive(0);
			DrawBus(Bus, (double) 0.0, (double) 0.0, 1);
			ZeroUnusedObjects(0);
			Bus->Info |= OBJECT_NOT_VISIBLE;
			Bus->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			x1 = Bus->X1;
			y1 = Bus->Y1;
			x2 = Bus->X2;
			y2 = Bus->Y2;

			for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
			{
				NetLabel = &((*NetLabels)[cnt2]);

				if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, NetLabel->ConnectX)) && (InRange(y1, NetLabel->ConnectY)))
					        || ((InRange(x2, NetLabel->ConnectX)) && (InRange(y2, NetLabel->ConnectY))))
					{
						NetLabel->Info |= OBJECT_NOT_VISIBLE;
						NetLabel->DeleteNr = (int16) LastActionNr;
					}
				}
			}

			Changed = 1;
			DataBaseChanged = 1;
		}
	}

//  InitDrawingWhitePen();
//  InitDrawingWhiteBrush();
	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			Junction->Info |= OBJECT_NOT_VISIBLE;
			Junction->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			OnePinNet->Info |= OBJECT_NOT_VISIBLE;
			OnePinNet->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			BusConnection->Info |= OBJECT_NOT_VISIBLE;
			BusConnection->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			GlobalConnection->Info |= OBJECT_NOT_VISIBLE;
			GlobalConnection->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

//  InitDrawingUnfilledBrush();

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			NetLabel->Info |= OBJECT_NOT_VISIBLE;
			NetLabel->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectLine->Info |= OBJECT_NOT_VISIBLE;
			ObjectLine->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectRect->Info |= OBJECT_NOT_VISIBLE;
			ObjectRect->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
			ObjectCircle->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectArc->Info |= OBJECT_NOT_VISIBLE;
			ObjectArc->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			ObjectText->Info |= OBJECT_NOT_VISIBLE;
			ObjectText->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			Pin->Info |= OBJECT_NOT_VISIBLE;
			Pin->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			PowerPin->Info |= OBJECT_NOT_VISIBLE;
			PowerPin->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ZeroUnusedObjects(0);
			PinBus->Info |= OBJECT_NOT_VISIBLE;
			PinBus->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
			Changed = 1;
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void UndoObjects()
{
	int32 cnt, Changed;

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
	RedefinedPinBusRecord *RedefinedPinBus;

	if (LastActionNr < 2)
		return;

	if (MaxLastActionNr < LastActionNr)
		MaxLastActionNr = LastActionNr;

	if (LastActionNr > 1)
		LastActionNr--;

	Changed = 0;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (Instance->AddNr == LastActionNr)
		{
			if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Instance->Info &= ~(OBJECT_SELECTED | 15);
				Instance->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (Instance->DeleteNr == LastActionNr)
		{
			if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Instance->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				DrawInstance(Instance, (double) 0.0, (double) 0.0, 0);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if (Wire->AddNr == LastActionNr)
		{
			if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Wire->Info &= ~(OBJECT_SELECTED | 15);
				Wire->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if (Wire->DeleteNr == LastActionNr)
		{
			if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Wire->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if (Bus->AddNr == LastActionNr)
		{
			if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Bus->Info &= ~(OBJECT_SELECTED | 15);
				Bus->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if (Bus->DeleteNr == LastActionNr)
		{
			if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Bus->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if (Junction->AddNr == LastActionNr)
		{
			if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Junction->Info &= ~(OBJECT_SELECTED | 15);
				Junction->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if (Junction->DeleteNr == LastActionNr)
		{
			if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Junction->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if (OnePinNet->AddNr == LastActionNr)
		{
			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				OnePinNet->Info &= ~(OBJECT_SELECTED | 15);
				OnePinNet->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if (OnePinNet->DeleteNr == LastActionNr)
		{
			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				OnePinNet->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if (NetLabel->AddNr == LastActionNr)
		{
			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NetLabel->Info &= ~(OBJECT_SELECTED | 15);
				NetLabel->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if (NetLabel->DeleteNr == LastActionNr)
		{
			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				NetLabel->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if (BusConnection->AddNr == LastActionNr)
		{
			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				BusConnection->Info &= ~(OBJECT_SELECTED | 15);
				BusConnection->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if (BusConnection->DeleteNr == LastActionNr)
		{
			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				BusConnection->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->AddNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectLine->Info &= ~(OBJECT_SELECTED | 15);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->DeleteNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectLine->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->AddNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectRect->Info &= ~(OBJECT_SELECTED | 15);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->DeleteNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectRect->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (ObjectCircle->AddNr == LastActionNr)
		{
			if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectCircle->Info &= ~(OBJECT_SELECTED | 15);
				ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (ObjectCircle->DeleteNr == LastActionNr)
		{
			if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectCircle->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->AddNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectArc->Info &= ~(OBJECT_SELECTED | 15);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->DeleteNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectArc->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (ObjectText->AddNr == LastActionNr)
		{
			if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectText->Info &= ~(OBJECT_SELECTED | 15);
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (ObjectText->DeleteNr == LastActionNr)
		{
			if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectText->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if (GlobalConnection->AddNr == LastActionNr)
		{
			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				GlobalConnection->Info &= ~(OBJECT_SELECTED | 15);
				GlobalConnection->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if (GlobalConnection->DeleteNr == LastActionNr)
		{
			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				GlobalConnection->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if (Pin->AddNr == LastActionNr)
		{
			if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Pin->Info &= ~(OBJECT_SELECTED | 15);
				Pin->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if (Pin->DeleteNr == LastActionNr)
		{
			if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Pin->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if (PowerPin->AddNr == LastActionNr)
		{
			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				PowerPin->Info &= ~(OBJECT_SELECTED | 3);
				PowerPin->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if (PowerPin->DeleteNr == LastActionNr)
		{
			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				PowerPin->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if (PinBus->AddNr == LastActionNr)
		{
			if ((PinBus->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				PinBus->Info &= ~(OBJECT_SELECTED | 15);
				PinBus->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if (PinBus->DeleteNr == LastActionNr)
		{
			if ((PinBus->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				PinBus->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrRedefinedPinBusses; cnt++)
	{
		RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);

		if (RedefinedPinBus->AddNr == LastActionNr)
		{
			if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				RedefinedPinBus->Info &= ~(OBJECT_SELECTED);
				RedefinedPinBus->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrRedefinedPinBusses; cnt++)
	{
		RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);

		if (RedefinedPinBus->DeleteNr == LastActionNr)
		{
			if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				RedefinedPinBus->Info &= ~(OBJECT_NOT_VISIBLE);
				Changed = 1;
			}
		}
	}

	if (Changed)
	{
		DataBaseChanged = 1;
		UndoRedoActive = 1;
		RePaint();
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RedoObjects()
{
	int32 cnt, Changed;

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
	RedefinedPinBusRecord *RedefinedPinBus;

	if ((LastActionNr > MaxLastActionNr) || (LastActionNr == 0))
		return;

	Changed = 0;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (Instance->DeleteNr == LastActionNr)
		{
			if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Instance->Info &= ~(OBJECT_SELECTED | 15);
				Instance->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	BackGroundActive = 0;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (Instance->AddNr == LastActionNr)
		{
			if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Instance->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if (Wire->DeleteNr == LastActionNr)
		{
			if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Wire->Info &= ~(OBJECT_SELECTED | 15);;
				Wire->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if (Wire->AddNr == LastActionNr)
		{
			if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Wire->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if (Bus->DeleteNr == LastActionNr)
		{
			if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Bus->Info &= ~(OBJECT_SELECTED | 15);;
				Bus->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if (Bus->AddNr == LastActionNr)
		{
			if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Bus->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if (Junction->DeleteNr == LastActionNr)
		{
			if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Junction->Info &= ~(OBJECT_SELECTED | 15);;
				Junction->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if (Junction->AddNr == LastActionNr)
		{
			if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Junction->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if (OnePinNet->DeleteNr == LastActionNr)
		{
			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				OnePinNet->Info &= ~(OBJECT_SELECTED | 15);;
				OnePinNet->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if (OnePinNet->AddNr == LastActionNr)
		{
			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				OnePinNet->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if (NetLabel->DeleteNr == LastActionNr)
		{
			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NetLabel->Info &= ~(OBJECT_SELECTED | 15);;
				NetLabel->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if (NetLabel->AddNr == LastActionNr)
		{
			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				NetLabel->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if (BusConnection->DeleteNr == LastActionNr)
		{
			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				BusConnection->Info &= ~(OBJECT_SELECTED | 15);;
				BusConnection->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if (BusConnection->AddNr == LastActionNr)
		{
			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				BusConnection->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->DeleteNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectLine->Info &= ~(OBJECT_SELECTED | 15);;
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (ObjectLine->AddNr == LastActionNr)
		{
			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectLine->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->DeleteNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectRect->Info &= ~(OBJECT_SELECTED | 15);;
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (ObjectRect->AddNr == LastActionNr)
		{
			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectRect->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (ObjectCircle->DeleteNr == LastActionNr)
		{
			if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectCircle->Info &= ~(OBJECT_SELECTED | 15);;
				ObjectCircle->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if (ObjectCircle->AddNr == LastActionNr)
		{
			if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectCircle->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->DeleteNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectArc->Info &= ~(OBJECT_SELECTED | 15);;
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (ObjectArc->AddNr == LastActionNr)
		{
			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectArc->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (ObjectText->DeleteNr == LastActionNr)
		{
			if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ObjectText->Info &= ~(OBJECT_SELECTED | 15);;
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (ObjectText->AddNr == LastActionNr)
		{
			if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				ObjectText->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if (GlobalConnection->DeleteNr == LastActionNr)
		{
			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				GlobalConnection->Info &= ~(OBJECT_SELECTED | 15);;
				GlobalConnection->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if (GlobalConnection->AddNr == LastActionNr)
		{
			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				GlobalConnection->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if (Pin->DeleteNr == LastActionNr)
		{
			if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Pin->Info &= ~(OBJECT_SELECTED | 15);;
				Pin->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if (Pin->AddNr == LastActionNr)
		{
			if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				Pin->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if (PowerPin->DeleteNr == LastActionNr)
		{
			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				PowerPin->Info &= ~(OBJECT_SELECTED | 15);;
				PowerPin->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if (PowerPin->AddNr == LastActionNr)
		{
			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				PowerPin->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if (PinBus->DeleteNr == LastActionNr)
		{
			if ((PinBus->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				PinBus->Info &= ~(OBJECT_SELECTED | 15);;
				PinBus->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if (PinBus->AddNr == LastActionNr)
		{
			if ((PinBus->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				PinBus->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 15);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrRedefinedPinBusses; cnt++)
	{
		RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);

		if (RedefinedPinBus->DeleteNr == LastActionNr)
		{
			if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				RedefinedPinBus->Info &= ~(OBJECT_SELECTED);
				RedefinedPinBus->Info |= OBJECT_NOT_VISIBLE;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrRedefinedPinBusses; cnt++)
	{
		RedefinedPinBus = &((*RedefinedPinBusses)[cnt]);

		if (RedefinedPinBus->AddNr == LastActionNr)
		{
			if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				RedefinedPinBus->Info &= ~(OBJECT_NOT_VISIBLE | OBJECT_SELECTED);
				Changed = 1;
			}
		}
	}

	if (MaxLastActionNr > LastActionNr)
		LastActionNr++;

	if (Changed)
	{
		DataBaseChanged = 1;
		UndoRedoActive = 1;
		RePaint();
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void MemInfo(int32 * TotalMemSize, int32 * DeletedMemSize)
{
	int32 cnt, TotalMem, DeletedMem;
	int32 InstancesDeleted = 0;
	int32 WiresDeleted = 0;
	int32 BussesDeleted = 0;
	int32 BusConnectionsDeleted = 0;
	int32 JunctionsDeleted = 0;
	int32 NetLabelsDeleted = 0;
	int32 ObjectLinesDeleted = 0;
	int32 ObjectRectsDeleted = 0;
	int32 ObjectCirclesDeleted = 0;
	int32 ObjectArcsDeleted = 0;
	int32 ObjectTextsDeleted = 0;
	int32 PinsDeleted = 0;
	int32 PowerPinsDeleted = 0;
	int32 PinBussesDeleted = 0;
	int32 GlobalConnectionsDeleted = 0;
	int32 SymbolsPosDeleted = 0;
	int32 SymbolsMemDeleted = 0;

	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
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
	SymbolsPosRecord *SymbolPos;

	for (cnt = 0; cnt < Design.NrSymbols; cnt++)
	{
		SymbolPos = (SymbolsPosRecord *) & ((*SymbolsPos)[cnt]);

		if ((SymbolPos->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
		{
			SymbolsPosDeleted++;
			SymbolsMemDeleted += SymbolPos->Length;
		}
	}

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			InstancesDeleted++;
	}

	if (!EditingSymbol)
	{

		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				WiresDeleted++;
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				BussesDeleted++;
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				JunctionsDeleted++;
		}

		for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				NetLabelsDeleted++;
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				BusConnectionsDeleted++;
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				GlobalConnectionsDeleted++;
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				PinsDeleted++;
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				PowerPinsDeleted++;
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
				PinBussesDeleted++;
		}

	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			ObjectLinesDeleted++;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			ObjectRectsDeleted++;
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			ObjectCirclesDeleted++;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			ObjectArcsDeleted++;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			ObjectTextsDeleted++;
	}

	TotalMem = 0;
	TotalMem += Design.NrInstances * sizeof(InstanceRecord);

	if (!EditingSymbol)
	{
		TotalMem += Design.NrWires * sizeof(WireRecord);
		TotalMem += Design.NrBusses * sizeof(BusRecord);
		TotalMem += Design.NrBusConnections * sizeof(BusConnectionRecord);
		TotalMem += Design.NrJunctions * sizeof(JunctionRecord);
		TotalMem += Design.NrNetLabels * sizeof(NetLabelRecord);
		TotalMem += Design.NrGlobalConnections * sizeof(GlobalConnectionRecord);
		TotalMem += Design.NrSymbols * sizeof(SymbolsPosRecord);
		TotalMem += Design.SymbolsMem;
	}
	else
	{
		TotalMem += DesignSymbol.NrPins * sizeof(PinRecord);
		TotalMem += DesignSymbol.NrPowerPins * sizeof(PowerPinRecord);
		TotalMem += DesignSymbol.NrPinBusses * sizeof(PinBusRecord);
	}

	TotalMem += Design.NrObjectLines * sizeof(ObjectLineRecord);
	TotalMem += Design.NrObjectRects * sizeof(ObjectRectRecord);
	TotalMem += Design.NrObjectCircles * sizeof(ObjectCircleRecord);
	TotalMem += Design.NrObjectArcs * sizeof(ObjectArcRecord);
	TotalMem += Design.NrObjectTexts * sizeof(ObjectTextRecord);

	DeletedMem = 0;
	DeletedMem += InstancesDeleted * sizeof(InstanceRecord);

	if (!EditingSymbol)
	{
		DeletedMem += WiresDeleted * sizeof(WireRecord);
		DeletedMem += BussesDeleted * sizeof(BusRecord);
		DeletedMem += BusConnectionsDeleted * sizeof(BusConnectionRecord);
		DeletedMem += JunctionsDeleted * sizeof(JunctionRecord);
		DeletedMem += NetLabelsDeleted * sizeof(NetLabelRecord);
		DeletedMem += GlobalConnectionsDeleted * sizeof(GlobalConnectionRecord);
		DeletedMem += SymbolsPosDeleted * sizeof(SymbolsPosRecord);
		DeletedMem += SymbolsMemDeleted;
	}
	else
	{
		DeletedMem += PinsDeleted * sizeof(PinRecord);
		DeletedMem += PowerPinsDeleted * sizeof(PowerPinRecord);
		DeletedMem += PinBussesDeleted * sizeof(PinBusRecord);
	}

	DeletedMem += ObjectLinesDeleted * sizeof(ObjectLineRecord);
	DeletedMem += ObjectRectsDeleted * sizeof(ObjectRectRecord);
	DeletedMem += ObjectCirclesDeleted * sizeof(ObjectCircleRecord);
	DeletedMem += ObjectArcsDeleted * sizeof(ObjectArcRecord);
	DeletedMem += ObjectTextsDeleted * sizeof(ObjectTextRecord);

	*TotalMemSize = TotalMem;
	*DeletedMemSize = DeletedMem;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddInstance(InstanceRecord * Instance)
{
	InstanceRecord *NewInstance;

//  if (!MakeRoomInstances(1)) return 0;
	if (Design.NrInstances >= MaxNrInstances)
	{
		if (AllocateMemInstances(MaxNrInstances + 64, 1) != 0)
			return 0;
	}

	NewInstance = &((*Instances)[Design.NrInstances]);
	memmove(NewInstance, Instance, sizeof(InstanceRecord));
	NewInstance->AddNr = (int16) LastActionNr;
	NewInstance->DeleteNr = 0;
	SetBoardPosInstance((int16) Design.NrInstances);
	ZeroUnusedObjects(0);
	Design.NrInstances++;
	DataBaseChanged = 1;
	return 1;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddWire(WireRecord * Wire)
{
	WireRecord *NewWire;

	if ((InRange(Wire->X1, Wire->X2)) && (InRange(Wire->Y1, Wire->Y2)))
		return 0;

	if (Design.NrWires >= MaxNrWires)
	{
		if (AllocateMemWires(MaxNrWires + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomWires(1)) return 0;
	NewWire = &((*Wires)[Design.NrWires]);
	memmove(NewWire, Wire, sizeof(WireRecord));
//  NewWire->Info=OBJECT_ADDED;
	NewWire->AddNr = (int16) LastActionNr;
	NewWire->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrWires++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CommandAddTryingBus(BusRecord * Bus, int32 Mode)
{
	int32 cnt, found, FoundNetLabelNr1, FoundNetLabelNr2, FoundNetLabelNr3, FoundNetLabelNr4;
	BusRecord *Bus2;
	double x1, y1, x2, y2, x3, y3, x4, y4, tx, ty;
	NetLabelRecord *NetLabel, NewNetLabel;
	int32 Changed = 0;

	Bus2 = NULL;
	NetLabel = NULL;

	if (InRange(Bus->Y1, Bus->Y2))
	{
		if (Bus->X1 > Bus->X2)
		{
			x1 = Bus->X1;
			Bus->X1 = Bus->X2;
			Bus->X2 = (float) x1;
			y1 = Bus->Y1;
			Bus->Y1 = Bus->Y2;
			Bus->Y2 = (float) y1;
		}
	}

	x1 = Bus->X1;
	y1 = Bus->Y1;
	x2 = Bus->X2;
	y2 = Bus->Y2;

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
		return 0;

	found = -1;

	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus2 = &((*Busses)[cnt]);

		if ((Bus2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
		{
			if (LinesOverlap(Bus2->X1, Bus2->Y1, Bus2->X2, Bus2->Y2, x1, y1, x2, y2))
			{
				found = cnt;
				cnt = Design.NrBusses;
			}
		}
	}

	if (found != -1)
	{
		Bus2 = &((*Busses)[found]);

		if ((Mode & 4) == 0)
		{
			if ((InRange(Bus2->X1, x1)) && (InRange(Bus2->X2, x2)))
			{
				y3 = min(Bus2->Y1, y1);
				y3 = min(Bus2->Y2, y3);
				y3 = min(y1, y3);
				y3 = min(y2, y3);
				y4 = max(Bus2->Y1, y1);
				y4 = max(Bus2->Y2, y4);
				y4 = max(y1, y4);
				y4 = max(y2, y4);
				Bus->Y1 = (float) y3;
				Bus->Y2 = (float) y4;
			}
			else
			{
				if ((InRange(Bus2->Y1, y1)) && (InRange(Bus2->Y2, y2)))
				{
					x3 = min(Bus2->X1, x1);
					x3 = min(Bus2->X2, x3);
					x3 = min(x1, x3);
					x3 = min(x2, x3);
					x4 = max(Bus2->X1, x1);
					x4 = max(Bus2->X2, x4);
					x4 = max(x1, x4);
					x4 = max(x2, x4);
					Bus->X1 = (float) x3;
					Bus->X2 = (float) x4;
				}
			}
		}
	}

	if (AddBus(Bus))
	{
		if ((Mode & 2) == 0)
		{
			if (found != -1)
			{
				Bus2 = &((*Busses)[found]);
				Bus2->Info |= OBJECT_NOT_VISIBLE;
				Bus2->DeleteNr = (int16) LastActionNr;
				FoundNetLabelNr1 = GetNetLabelIndexFromEndPointLine(x1, y1);
				FoundNetLabelNr2 = GetNetLabelIndexFromEndPointLine(x2, y2);
				FoundNetLabelNr3 = GetNetLabelIndexFromEndPointLine(Bus2->X1, Bus2->Y1);
				FoundNetLabelNr4 = GetNetLabelIndexFromEndPointLine(Bus2->X2, Bus2->Y2);

				if ((FoundNetLabelNr1 != -1) || (FoundNetLabelNr2 != -1) || (FoundNetLabelNr3 != -1)
				        || (FoundNetLabelNr4 != -1))
				{
					if (FoundNetLabelNr1 != -1)
						NetLabel = &((*NetLabels)[FoundNetLabelNr1]);

					if (FoundNetLabelNr2 != -1)
						NetLabel = &((*NetLabels)[FoundNetLabelNr2]);

					if (FoundNetLabelNr3 != -1)
						NetLabel = &((*NetLabels)[FoundNetLabelNr3]);

					if (FoundNetLabelNr4 != -1)
						NetLabel = &((*NetLabels)[FoundNetLabelNr4]);

					memmove(&NewNetLabel, NetLabel, sizeof(NetLabelRecord));
					tx = NetLabel->ConnectX + NetLabel->TextX;
					ty = NetLabel->ConnectY + NetLabel->TextY;

					if (InRangeSpecial(Bus->X1, Bus->X2, 0.01))
					{
						if (Bus->Y1 < Bus->Y2)
						{
							NewNetLabel.ConnectX = Bus->X1;
							NewNetLabel.ConnectY = Bus->Y1;
						}
						else
						{
							NewNetLabel.ConnectX = Bus->X2;
							NewNetLabel.ConnectY = Bus->Y2;
						}
					}
					else
					{
						if (Bus->X1 < Bus->X2)
						{
							NewNetLabel.ConnectX = Bus->X1;
							NewNetLabel.ConnectY = Bus->Y1;
						}
						else
						{
							NewNetLabel.ConnectX = Bus->X2;
							NewNetLabel.ConnectY = Bus->Y2;
						}
					}

					NewNetLabel.TextX = (float) (tx - NewNetLabel.ConnectX);
					NewNetLabel.TextY = (float) (ty - NewNetLabel.ConnectY);

					if (AddNetLabel(&NewNetLabel))
					{
						if (FoundNetLabelNr1 != -1)
							NetLabel = &((*NetLabels)[FoundNetLabelNr1]);

						if (FoundNetLabelNr2 != -1)
							NetLabel = &((*NetLabels)[FoundNetLabelNr2]);

						if (FoundNetLabelNr3 != -1)
							NetLabel = &((*NetLabels)[FoundNetLabelNr3]);

						if (FoundNetLabelNr4 != -1)
							NetLabel = &((*NetLabels)[FoundNetLabelNr4]);

						NetLabel->Info |= OBJECT_NOT_VISIBLE;
						NetLabel->DeleteNr = (int16) LastActionNr;
					}
				}
			}
		}

		Changed = 1;
	}

	return Changed;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddBus(BusRecord * Bus)
{
	BusRecord *NewBus;

	if ((InRange(Bus->X1, Bus->X2)) && (InRange(Bus->Y1, Bus->Y2)))
		return 0;

	if (Design.NrBusses >= MaxNrBusses)
	{
		if (AllocateMemBusses(MaxNrBusses + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomBusses(1)) return 0;
	NewBus = &((*Busses)[Design.NrBusses]);
	memmove(NewBus, Bus, sizeof(BusRecord));
//  NewBus->Info=OBJECT_ADDED;
	NewBus->AddNr = (int16) LastActionNr;
	NewBus->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrBusses++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddJunction(JunctionRecord * Junction)
{
	JunctionRecord *NewJunction;

	if (Design.NrJunctions >= MaxNrJunctions)
	{
		if (AllocateMemJunctions(MaxNrJunctions + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomJunctions(1)) return 0;
	NewJunction = &((*Junctions)[Design.NrJunctions]);
	memmove(NewJunction, Junction, sizeof(JunctionRecord));
//  NewJunction->Info=OBJECT_ADDED;
	NewJunction->AddNr = (int16) LastActionNr;
	NewJunction->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrJunctions++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddOnePinNet(OnePinNetRecord * OnePinNet)
{
	OnePinNetRecord *NewOnePinNet;

	if (Design.NrOnePinNets >= MaxNrOnePinNets)
	{
		if (AllocateMemOnePinNets(MaxNrOnePinNets + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomOnePinNets(1)) return 0;
	NewOnePinNet = &((*OnePinNets)[Design.NrOnePinNets]);
	memmove(NewOnePinNet, OnePinNet, sizeof(OnePinNetRecord));
//  NewOnePinNet->Info=OBJECT_ADDED;
	NewOnePinNet->AddNr = (int16) LastActionNr;
	NewOnePinNet->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrOnePinNets++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddNetLabel(NetLabelRecord * NetLabel)
{
	NetLabelRecord *NewNetLabel;

	if (NetLabel->Name[0] == 0)
		return 1;

	if (Design.NrNetLabels >= MaxNrNetLabels)
	{
		if (AllocateMemNetLabels(MaxNrNetLabels + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomNetLabels(1)) return 0;
	NewNetLabel = &((*NetLabels)[Design.NrNetLabels]);
	memmove(NewNetLabel, NetLabel, sizeof(NetLabelRecord));
//  NewNetLabel->Info=OBJECT_ADDED;
	NewNetLabel->AddNr = (int16) LastActionNr;
	NewNetLabel->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrNetLabels++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddBusConnection(BusConnectionRecord * BusConnection)
{
	BusConnectionRecord *NewBusConnection;

	if (Design.NrBusConnections >= MaxNrBusConnections)
	{
		if (AllocateMemBusConnections(MaxNrBusConnections + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomBusConnections(1)) return 0;
	NewBusConnection = &((*BusConnections)[Design.NrBusConnections]);
	memmove(NewBusConnection, BusConnection, sizeof(BusConnectionRecord));
//  NewBusConnection->Info=OBJECT_ADDED;
	NewBusConnection->AddNr = (int16) LastActionNr;
	NewBusConnection->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrBusConnections++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddObjectLine(ObjectLineRecord * ObjectLine)
{
	ObjectLineRecord *NewObjectLine;

	if (Design.NrObjectLines >= MaxNrObjectLines)
	{
		if (AllocateMemObjectLines(MaxNrObjectLines + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomObjectLines(1)) return 0;
	NewObjectLine = &((*ObjectLines)[Design.NrObjectLines]);
	memmove(NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
//  NewObjectLine->Info=OBJECT_ADDED;
	NewObjectLine->AddNr = (int16) LastActionNr;
	NewObjectLine->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectLines++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddObjectRect(ObjectRectRecord * ObjectRect)
{
	ObjectRectRecord *NewObjectRect;

	if (Design.NrObjectRects >= MaxNrObjectRects)
	{
		if (AllocateMemObjectRects(MaxNrObjectRects + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomObjectRects(1)) return 0;
	NewObjectRect = &((*ObjectRects)[Design.NrObjectRects]);
	memmove(NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
//  NewObjectRect->Info=OBJECT_ADDED;
	NewObjectRect->AddNr = (int16) LastActionNr;
	NewObjectRect->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectRects++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddObjectCircle(ObjectCircleRecord * ObjectCircle)
{
	ObjectCircleRecord *NewObjectCircle;

	if (Design.NrObjectCircles >= MaxNrObjectCircles)
	{
		if (AllocateMemObjectCircles(MaxNrObjectCircles + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomObjectCircles(1)) return 0;
	NewObjectCircle = &((*ObjectCircles)[Design.NrObjectCircles]);
	memmove(NewObjectCircle, ObjectCircle, sizeof(ObjectCircleRecord));
//  NewObjectCircle->Info=OBJECT_ADDED;
	NewObjectCircle->AddNr = (int16) LastActionNr;
	NewObjectCircle->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectCircles++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddObjectArc(ObjectArcRecord * ObjectArc)
{
	ObjectArcRecord *NewObjectArc;

	if (Design.NrObjectArcs >= MaxNrObjectArcs)
	{
		if (AllocateMemObjectArcs(MaxNrObjectArcs + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomObjectArcs(1)) return 0;
	NewObjectArc = &((*ObjectArcs)[Design.NrObjectArcs]);
	memmove(NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
//  NewObjectArc->Info=OBJECT_ADDED;
	NewObjectArc->AddNr = (int16) LastActionNr;
	NewObjectArc->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectArcs++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddObjectText(ObjectTextRecord * ObjectText)
{
	ObjectTextRecord *NewObjectText;

	if (strlen(ObjectText->Text) == 0)
		return 0;

	if (Design.NrObjectTexts >= MaxNrObjectTexts)
	{
		if (AllocateMemObjectTexts(MaxNrObjectTexts + 16, 1) != 0)
			return 0;
	}

//  if (!MakeRoomObjectTexts(1)) return 0;
	NewObjectText = &((*ObjectTexts)[Design.NrObjectTexts]);
	memmove(NewObjectText, ObjectText, sizeof(ObjectTextRecord));
//  NewObjectText->Info=OBJECT_ADDED;
	NewObjectText->AddNr = (int16) LastActionNr;
	NewObjectText->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrObjectTexts++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddObject(ObjectRecord * Object)
{
	ObjectRecord *NewObject;

	if (NrObjects >= MaxNrObjects)
	{
		if (AllocateMemObjects(MaxNrObjects + 128, 1) != 0)
			return 0;
	}

	NewObject = &((*Objects)[NrObjects]);
	memmove(NewObject, Object, sizeof(ObjectRecord));
	NrObjects++;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddGlobalConnection(GlobalConnectionRecord * GlobalConnection)
{
	GlobalConnectionRecord *NewGlobalConnection;

	if (GlobalConnection->Text[0] == 0)
		return 1;

	if (Design.NrGlobalConnections >= MaxNrGlobalConnections)
	{
		if (AllocateMemGlobalConnections(MaxNrGlobalConnections + 64, 1) != 0)
			return 0;
	}

//  if (!MakeRoomGlobalConnections(1)) return 0;
	NewGlobalConnection = &((*GlobalConnections)[Design.NrGlobalConnections]);
	memmove(NewGlobalConnection, GlobalConnection, sizeof(GlobalConnectionRecord));
//  NewGlobalConnection->Info=OBJECT_ADDED;
	NewGlobalConnection->AddNr = (int16) LastActionNr;
	NewGlobalConnection->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrGlobalConnections++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddPin(PinRecord * Pin)
{
	PinRecord *NewPin;
	int32 hulp;

	hulp = MaxNrPins;

	if (DesignSymbol.NrPins >= MaxNrPins)
	{
		if (AllocateMemPins(MaxNrPins + 16, 1) != 0)
			return 0;
	}

//  if (!MakeRoomPins(1)) return 0;
	NewPin = &((*Pins)[DesignSymbol.NrPins]);
	memmove(NewPin, Pin, sizeof(PinRecord));
//  NewPin->Info=OBJECT_ADDED;
	NewPin->AddNr = (int16) LastActionNr;
	NewPin->DeleteNr = 0;
	ZeroUnusedObjects(0);
	DesignSymbol.NrPins++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddPowerPin(PowerPinRecord * PowerPin)
{
	PowerPinRecord *NewPowerPin;

	if (DesignSymbol.NrPowerPins >= MaxNrPowerPins)
	{
		if (AllocateMemPowerPins(MaxNrPowerPins + 16, 1) != 0)
			return 0;
	}

//  if (!MakeRoomPowerPins(1)) return 0;
	NewPowerPin = &((*PowerPins)[DesignSymbol.NrPowerPins]);
	memmove(NewPowerPin, PowerPin, sizeof(PowerPinRecord));
//  NewPowerPin->Info=OBJECT_ADDED;
	NewPowerPin->AddNr = (int16) LastActionNr;
	NewPowerPin->DeleteNr = 0;
	ZeroUnusedObjects(0);
	DesignSymbol.NrPowerPins++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddPinBus(PinBusRecord * PinBus)
{
	PinBusRecord *NewPinBus;

	if (DesignSymbol.NrPinBusses >= MaxNrPinBusses)
	{
		if (AllocateMemPinBusses(MaxNrPinBusses + 8, 1) != 0)
			return 0;
	}

//  if (!MakeRoomPinBusses(1)) return 0;
	NewPinBus = &((*PinBusses)[DesignSymbol.NrPinBusses]);
	memmove(NewPinBus, PinBus, sizeof(PinBusRecord));
//  NewPinBus->Info=OBJECT_ADDED;
	NewPinBus->AddNr = (int16) LastActionNr;
	NewPinBus->DeleteNr = 0;
	ZeroUnusedObjects(0);
	DesignSymbol.NrPinBusses++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddRedefinedPinBus(RedefinedPinBusRecord * RedefinedPinBus)
{
	RedefinedPinBusRecord *NewRedefinedPinBus;

	if (Design.NrRedefinedPinBusses >= MaxNrRedefinedPinBusses)
	{
		if (AllocateMemPinBusses(MaxNrRedefinedPinBusses + 8, 1) != 0)
			return 0;
	}

//  if (!MakeRoomPinBusses(1)) return 0;
	NewRedefinedPinBus = &((*RedefinedPinBusses)[Design.NrRedefinedPinBusses]);
	memmove(NewRedefinedPinBus, RedefinedPinBus, sizeof(RedefinedPinBusRecord));
//  NewPinBus->Info=OBJECT_ADDED;
	NewRedefinedPinBus->AddNr = (int16) LastActionNr;
	NewRedefinedPinBus->DeleteNr = 0;
	ZeroUnusedObjects(0);
	Design.NrRedefinedPinBusses++;
	DataBaseChanged = 1;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddSymbol(SymbolRecord * Symbol)
{
	SymbolRecord *NewSymbol;
	SymbolsPosRecord *NewSymbolPos;

	if (Design.SymbolsMem + Symbol->MemSize > MaxSymbolsMemSize)
	{
		if (AllocateMemSheetSymbols(MaxSymbolsMemSize + Symbol->MemSize + 16384, 1) != 0)
			return 0;
	}

	if (Design.NrSymbols >= MaxNrSheetSymbols)
	{
		if (AllocateMemSheetSymbolsPos(MaxNrSheetSymbols + 32, 1) != 0)
			return 0;
	}

	NewSymbol = (SymbolRecord *) & SymbolsMem[Design.SymbolsMem];
	memmove(NewSymbol, Symbol, (int) Symbol->MemSize);
	NewSymbolPos = &((*SymbolsPos)[Design.NrSymbols]);
	memset(NewSymbolPos, 0, sizeof(SymbolsPosRecord));
	NewSymbolPos->Pos = Design.SymbolsMem;
	NewSymbolPos->Length = Symbol->MemSize;
	memmove(NewSymbolPos->SymbolName, Symbol->Name, 32);
	NewSymbolPos->AddNr = (int16) LastActionNr;
	NewSymbolPos->DeleteNr = 0;
	Design.NrSymbols++;
	Design.SymbolsMem += Symbol->MemSize;
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void RemoveDeletedObjects()
{

	int32 cnt, NrWires, NrBusses, NrJunctions, NrOnePinNets, NrNetLabels, NrBusConnections, NrGlobalConnections, NrPins,
	      NrPowerPins, NrPinBusses, NrObjectLines, NrObjectRects, NrObjectCircles, NrObjectArcs, NrObjectTexts,
	      NrInstances;
	WireRecord *Wire, *Wire2;
	BusRecord *Bus, *Bus2;
	JunctionRecord *Junction, *Junction2;
	OnePinNetRecord *OnePinNet, *OnePinNet2;
	NetLabelRecord *NetLabel, *NetLabel2;
	BusConnectionRecord *BusConnection, *BusConnection2;
	GlobalConnectionRecord *GlobalConnection, *GlobalConnection2;
	PinRecord *Pin, *Pin2;
	PowerPinRecord *PowerPin, *PowerPin2;
	PinBusRecord *PinBus, *PinBus2;

	ObjectLineRecord *ObjectLine, *ObjectLine2;
	ObjectRectRecord *ObjectRect, *ObjectRect2;
	ObjectCircleRecord *ObjectCircle, *ObjectCircle2;
	ObjectArcRecord *ObjectArc, *ObjectArc2;
	ObjectTextRecord *ObjectText, *ObjectText2;
	InstanceRecord *Instance, *Instance2;

	NrInstances = Design.NrInstances;
	cnt = 0;

	while (cnt < NrInstances)
	{
		Instance = &((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NrInstances - 1)
			{
				Instance2 = &((*Instances)[NrInstances - 1]);
				memmove(Instance, Instance2, sizeof(InstanceRecord));
			}

			NrInstances--;
		}
		else
			cnt++;
	}

	Design.NrInstances = NrInstances;

	if (!EditingSymbol)
	{
		NrWires = Design.NrWires;
		cnt = 0;

		while (cnt < NrWires)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrWires - 1)
				{
					Wire2 = &((*Wires)[NrWires - 1]);
					memmove(Wire, Wire2, sizeof(WireRecord));
				}

				NrWires--;
			}
			else
				cnt++;
		}

		Design.NrWires = NrWires;

		NrBusses = Design.NrBusses;
		cnt = 0;

		while (cnt < NrBusses)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrBusses - 1)
				{
					Bus2 = &((*Busses)[NrBusses - 1]);
					memmove(Bus, Bus2, sizeof(BusRecord));
				}

				NrBusses--;
			}
			else
				cnt++;
		}

		Design.NrBusses = NrBusses;


		NrJunctions = Design.NrJunctions;
		cnt = 0;

		while (cnt < NrJunctions)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrJunctions - 1)
				{
					Junction2 = &((*Junctions)[NrJunctions - 1]);
					memmove(Junction, Junction2, sizeof(JunctionRecord));
				}

				NrJunctions--;
			}
			else
				cnt++;
		}

		Design.NrJunctions = NrJunctions;

		NrOnePinNets = Design.NrOnePinNets;
		cnt = 0;

		while (cnt < NrOnePinNets)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if ((OnePinNet->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrOnePinNets - 1)
				{
					OnePinNet2 = &((*OnePinNets)[NrOnePinNets - 1]);
					memmove(OnePinNet, OnePinNet2, sizeof(OnePinNetRecord));
				}

				NrOnePinNets--;
			}
			else
				cnt++;
		}

		Design.NrOnePinNets = NrOnePinNets;

		NrNetLabels = Design.NrNetLabels;
		cnt = 0;

		while (cnt < NrNetLabels)
		{
			NetLabel = &((*NetLabels)[cnt]);

			if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrNetLabels - 1)
				{
					NetLabel2 = &((*NetLabels)[NrNetLabels - 1]);
					memmove(NetLabel, NetLabel2, sizeof(NetLabelRecord));
				}

				NrNetLabels--;
			}
			else
				cnt++;
		}

		Design.NrNetLabels = NrNetLabels;

		NrBusConnections = Design.NrBusConnections;
		cnt = 0;

		while (cnt < NrBusConnections)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrBusConnections - 1)
				{
					BusConnection2 = &((*BusConnections)[NrBusConnections - 1]);
					memmove(BusConnection, BusConnection2, sizeof(BusConnectionRecord));
				}

				NrBusConnections--;
			}
			else
				cnt++;
		}

		Design.NrBusConnections = NrBusConnections;

		NrGlobalConnections = Design.NrGlobalConnections;
		cnt = 0;

		while (cnt < NrGlobalConnections)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrGlobalConnections - 1)
				{
					GlobalConnection2 = &((*GlobalConnections)[NrGlobalConnections - 1]);
					memmove(GlobalConnection, GlobalConnection2, sizeof(GlobalConnectionRecord));
				}

				NrGlobalConnections--;
			}
			else
				cnt++;
		}

		Design.NrGlobalConnections = NrGlobalConnections;
	}
	else
	{
		NrPins = DesignSymbol.NrPins;
		cnt = 0;

		while (cnt < DesignSymbol.NrPins)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrPins - 1)
				{
					Pin2 = &((*Pins)[NrPins - 1]);
					memmove(Pin, Pin2, sizeof(PinRecord));
				}

				NrPins--;
			}
			else
				cnt++;
		}

		DesignSymbol.NrPins = NrPins;

		NrPowerPins = DesignSymbol.NrPowerPins;
		cnt = 0;

		while (cnt < DesignSymbol.NrPowerPins)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrPowerPins - 1)
				{
					PowerPin2 = &((*PowerPins)[NrPowerPins - 1]);
					memmove(PowerPin, PowerPin2, sizeof(PowerPinRecord));
				}

				NrPowerPins--;
			}
			else
				cnt++;
		}

		DesignSymbol.NrPowerPins = NrPowerPins;

		NrPinBusses = DesignSymbol.NrPinBusses;
		cnt = 0;

		while (cnt < DesignSymbol.NrPinBusses)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & (OBJECT_NOT_VISIBLE)) == OBJECT_NOT_VISIBLE)
			{
				if (cnt < NrPinBusses - 1)
				{
					PinBus2 = &((*PinBusses)[NrPinBusses - 1]);
					memmove(PinBus, PinBus2, sizeof(PinBusRecord));
				}

				NrPinBusses--;
			}
			else
				cnt++;
		}

		DesignSymbol.NrPinBusses = NrPinBusses;
	}

	NrObjectLines = Design.NrObjectLines;
	cnt = 0;

	while (cnt < NrObjectLines)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NrObjectLines - 1)
			{
				ObjectLine2 = &((*ObjectLines)[NrObjectLines - 1]);
				memmove(ObjectLine, ObjectLine2, sizeof(ObjectLineRecord));
			}

			NrObjectLines--;
		}
		else
			cnt++;
	}

	Design.NrObjectLines = NrObjectLines;

	NrObjectRects = Design.NrObjectRects;
	cnt = 0;

	while (cnt < NrObjectRects)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NrObjectRects - 1)
			{
				ObjectRect2 = &((*ObjectRects)[NrObjectRects - 1]);
				memmove(ObjectRect, ObjectRect2, sizeof(ObjectRectRecord));
			}

			NrObjectRects--;
		}
		else
			cnt++;
	}

	Design.NrObjectRects = NrObjectRects;


	NrObjectCircles = Design.NrObjectCircles;
	cnt = 0;

	while (cnt < NrObjectCircles)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NrObjectCircles - 1)
			{
				ObjectCircle2 = &((*ObjectCircles)[NrObjectCircles - 1]);
				memmove(ObjectCircle, ObjectCircle2, sizeof(ObjectCircleRecord));
			}

			NrObjectCircles--;
		}
		else
			cnt++;
	}

	Design.NrObjectCircles = NrObjectCircles;


	NrObjectArcs = Design.NrObjectArcs;
	cnt = 0;

	while (cnt < NrObjectArcs)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NrObjectArcs - 1)
			{
				ObjectArc2 = &((*ObjectArcs)[NrObjectArcs - 1]);
				memmove(ObjectArc, ObjectArc2, sizeof(ObjectArcRecord));
			}

			NrObjectArcs--;
		}
		else
			cnt++;
	}

	Design.NrObjectArcs = NrObjectArcs;


	NrObjectTexts = Design.NrObjectTexts;
	cnt = 0;

	while (cnt < NrObjectTexts)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & OBJECT_NOT_VISIBLE) == OBJECT_NOT_VISIBLE)
		{
			if (cnt < NrObjectTexts - 1)
			{
				ObjectText2 = &((*ObjectTexts)[NrObjectTexts - 1]);
				memmove(ObjectText, ObjectText2, sizeof(ObjectTextRecord));
			}

			NrObjectTexts--;
		}
		else
			cnt++;
	}

	Design.NrObjectTexts = NrObjectTexts;
	InitLoadedObjects(0);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckIfCoordinateMoreThenDouble(double x1, double y1, int32 mode)
{
	InstanceRecord *Instance;
	int32 cnt, cnt2;
	ObjectRecord *Object;

	if (mode == 0)
	{
		if (FirstCheckJunctions)
		{
			FirstCheckJunctions = 0;
			NrObjects = 0;

			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
					InstanceToObject(Instance, 0.0, 0.0, 1);
			}
		}

		for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
		{
			Object = &((*Objects)[cnt2]);

			if (Object->ObjectType == SYMBOL_PIN)
			{
				if ((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
					return 1;
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				NrObjects = 0;
				InstanceToObject(Instance, 0.0, 0.0, 1);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (Object->ObjectType == SYMBOL_PIN)
					{
						if ((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
							return 1;
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

int32 CheckJunction(double x1, double y1, int32 mode)
{
	int32 cnt, cnt2, Found;
	JunctionRecord *Junction, NewJunction;
	InstanceRecord *Instance;
	ObjectRecord *Object;

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((InRange(Junction->X, x1)) && (InRange(Junction->Y, y1)))
				return 0;
		}
	}

#ifdef _DEBUG

	if ((InRange(x1, 123.0)) && (InRange(y1, 55.0)))
		ok = 1;

#endif

	if (mode == 0)
		LastActionNr = 0;

	Found = -1;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			NrObjects = 0;
			InstanceToObject(Instance, 0.0, 0.0, 1);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Object->ObjectType == SYMBOL_PIN) || (Object->ObjectType == SYMBOL_PINBUS))
				{
					if ((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
						Found = 100000;
				}
			}
		}
	}


	if (Found == -1)
	{
		memset(&NewJunction, 0, sizeof(JunctionRecord));

		switch (mode)
		{
		case 0:
			NewJunction.X = (float) x1;
			NewJunction.Y = (float) y1;
			NewJunction.Info = OBJECT_SELECTED;
			AddJunction(&NewJunction);
			break;

		case 1:
			if ((ExtraJunction.Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				ExtraJunction.X = (float) x1;
				ExtraJunction.Y = (float) y1;
				ExtraJunction.Info = OBJECT_NOT_VISIBLE;
			}

			break;

		case 2:
			NewJunction.X = (float) x1;
			NewJunction.Y = (float) y1;
			AddJunction(&NewJunction);
			break;
		}
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckJunctionToBePlaced(double x, double y, int32 mode)
{
	WireRecord *Wire;
	int32 cnt, cnt2, Found1, Found2, Found3, Found1a, Found2a, Found3a, FoundPin, FoundJunction;
	double x1, y1, x2, y2;
	JunctionRecord *Junction;
	InstanceRecord *Instance;
	ObjectRecord *Object;
#ifdef _DEBUG
	int32 ok;
#endif

	Found1 = -1;
	Found2 = -1;
	Found3 = -1;
	Found1a = -1;
	Found2a = -1;
	Found3a = -1;
	FoundPin = 0;

	FoundJunction = -1;

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((InRange(Junction->X, x)) && (InRange(Junction->Y, y)))
				FoundJunction = cnt;
		}
	}

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);
#ifdef _DEBUG

		if (cnt == Design.NrWires - 1)
			ok = 1;

#endif

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;

			if (((InRange(x1, x)) && (InRange(y1, y))) || ((InRange(x2, x)) && (InRange(y2, y))))
			{
				if (Found1 == -1)
					Found1 = cnt;
				else
				{
					if (Found2 == -1)
						Found2 = cnt;
					else
					{
						if (Found3 == -1)
							Found3 = cnt;
					}
				}
			}
			else
			{
				if (TestLineConnectedToCircle(x1, y1, x2, y2, x, y, (double) 0.04))
				{
					if (Found1a == -1)
						Found1a = cnt;
				}
			}
		}
	}

#ifdef _DEBUG

	if ((InRange(x, 123.0)) && (InRange(y, 55.0)))
		ok = 1;

#endif

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if ((x > Instance->BoardPosMinX - 0.1) && (x < Instance->BoardPosMaxX + 0.1)
			        && (y > Instance->BoardPosMinY - 0.1) && (y < Instance->BoardPosMaxY + 0.1))
			{
				NrObjects = 0;
				InstanceToObject(Instance, 0.0, 0.0, 1);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if ((Object->ObjectType == SYMBOL_PIN) || (Object->ObjectType == SYMBOL_PINBUS))
					{
						if ((InRange(Object->x1, x)) && (InRange(Object->y1, y)))
							FoundPin = 1;
					}
				}
			}
		}
	}

#ifdef _DEBUG

	if ((InRange(x, 123.0)) && (InRange(y, 55.0)))
		ok = 1;

#endif
#ifdef _DEBUG
	ok = 1;
#endif

	if ((Found1 != -1) && (Found2 != -1) && (Found3 != -1) && (FoundJunction == -1))
		return 1;

	if ((Found1 != -1) && (Found2 != -1) && (Found3 == -1) && (FoundPin == 1) && (FoundJunction == -1))
		return 1;

	if ((Found1 != -1) && (Found2 == -1) && (Found1a != -1) && (FoundPin == 1) && (FoundJunction == -1))
		return 1;

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CommandAddTryingWire(WireRecord * Wire, int32 Mode)
{
	double x1, y1, x2, y2;
	int32 Changed = 0;

	x1 = Wire->X1;
	y1 = Wire->Y1;
	x2 = Wire->X2;
	y2 = Wire->Y2;

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
		return 0;

	if (InRange(Wire->Y1, Wire->Y2))
	{
		if (Wire->X1 > Wire->X2)
		{
			x1 = Wire->X1;
			Wire->X1 = Wire->X2;
			Wire->X2 = (float) x1;
			y1 = Wire->Y1;
			Wire->Y1 = Wire->Y2;
			Wire->Y2 = (float) y1;
		}
	}

	if (AddWire(Wire))
	{
		Changed = 1;
//      RebuildJunctions(1);
	}

	return Changed;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 PlaceJunctionOnEndPoint(double x, double y, int32 mode)
{
	WireRecord *Wire;
	int32 cnt, Found1, Found2, Found3, Found1a, Found2a, Found3a;
	double x1, y1, x2, y2;
#ifdef _DEBUG
	int32 ok;
#endif

	Found1 = -1;
	Found2 = -1;
	Found3 = -1;
	Found1a = -1;
	Found2a = -1;
	Found3a = -1;

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);
#ifdef _DEBUG

		if (cnt == Design.NrWires - 1)
			ok = 1;

#endif

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;

			if (((InRange(x1, x)) && (InRange(y1, y))) || ((InRange(x2, x)) && (InRange(y2, y))))
			{
				if (Found1 == -1)
					Found1 = cnt;
				else
				{
					if (Found2 == -1)
						Found2 = cnt;
					else
					{
						if (Found3 == -1)
							Found3 = cnt;
					}
				}
			}
			else
			{
				if (TestLineConnectedToCircle(x1, y1, x2, y2, x, y, (double) 0.04))
				{
					if (Found1a == -1)
						Found1a = cnt;
				}
			}
		}
	}

#ifdef _DEBUG

	if ((InRange(x, 106.0)) && (InRange(y, 31.0)))
		ok = 1;

#endif
#ifdef _DEBUG
	ok = 1;
#endif

	if ((Found1 != -1) && (Found2 != -1) && (Found3 != -1))
	{
		CheckJunction(x, y, 2);
		return 0;
	}

	if ((Found1 != -1) && (Found2 != -1) && (Found3 == -1) && (Found1a != -1))
	{
		CheckJunction(x, y, 2);
		JunctionCheckWire(Found1a, 0);
		return 0;
	}

	if ((Found1 != -1) && (Found2 == -1) && (Found1a != -1))
	{
		CheckJunction(x, y, 2);
		JunctionCheckWire(Found1a, 0);
		return 0;
	}



	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************


int32 QsortCompare(const char *arg1, const char *arg2)
{
	if ((*(LineJunctionRecord *) arg1).Distance > (*(LineJunctionRecord *) arg2).Distance)
		return 1;

	if ((*(LineJunctionRecord *) arg1).Distance == (*(LineJunctionRecord *) arg2).Distance)
		return 0;

	return -1;
}

int32 JunctionCheckWire(int32 WireCnt, int32 mode)
{
	WireRecord *Wire, NewWire, *Wire2;
	JunctionRecord *Junction;
	int32 cnt2, NrLineJunctions, cnt3, Vertical, FoundNetLabelNr1, FoundNetLabelNr2;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	Wire = &((*Wires)[WireCnt]);
	x1 = Wire->X1;
	y1 = Wire->Y1;
	x2 = Wire->X2;
	y2 = Wire->Y2;

	if (((InRange(x1, x2)) || (InRange(y1, y2))) && (CalcLengthLine(x1, y1, x2, y2) > 0.0))
	{
		Vertical = 0;

		if (InRange(x1, x2))
			Vertical = 1;

		memset(LineJunctions, 0, sizeof(LineJunctions));
		NrLineJunctions = 0;

		for (cnt2 = 0; cnt2 < Design.NrJunctions; cnt2++)
		{
			Junction = &((*Junctions)[cnt2]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				x3 = Junction->X;
				y3 = Junction->Y;

				if (TestLineConnectedToCircle(x1, y1, x2, y2, x3, y3, 0.04))
				{
					if (!(((InRange(x1, x3)) && (InRange(y1, y3))) || ((InRange(x2, x3)) && (InRange(y2, y3)))))
					{
						// Junction not placed on one of the wire endpoints
						if (NrLineJunctions < 128)
						{
							LineJunctions[NrLineJunctions].Distance = CalcLengthLine(x1, y1, x3, y3);
							LineJunctions[NrLineJunctions].X = x3;
							LineJunctions[NrLineJunctions].Y = y3;
							NrLineJunctions++;
						}
					}
				}
			}
		}

		if (NrLineJunctions > 0)
		{
			if (NrLineJunctions > 1)
				qsort(&LineJunctions, NrLineJunctions, sizeof(LineJunctionRecord), QsortCompare);

			LineJunctions[NrLineJunctions].X = x2;
			LineJunctions[NrLineJunctions].Y = y2;
			NrLineJunctions++;
			FoundNetLabelNr1 = GetNetLabelIndexFromEndPointLine(x1, y1);
			FoundNetLabelNr2 = GetNetLabelIndexFromEndPointLine(x2, y2);

			if (FoundNetLabelNr1 != -1)
			{
			}
			else
			{
			}

			memcpy(&NewWire, Wire, sizeof(WireRecord));
			x4 = x1;
			y4 = y1;

			for (cnt3 = 0; cnt3 < NrLineJunctions; cnt3++)
			{
				if (Vertical)
				{
					// vertical
					NewWire.Y1 = (float) y4;
					NewWire.Y2 = (float) LineJunctions[cnt3].Y;
					AddWire(&NewWire);
					y4 = NewWire.Y2;
				}
				else
				{
					// horizontal
					NewWire.X1 = (float) x4;
					NewWire.X2 = (float) LineJunctions[cnt3].X;
					AddWire(&NewWire);
					x4 = NewWire.X2;
				}

				if (mode == 1)
				{
					Wire2 = &((*Wires)[Design.NrWires - 1]);
					Wire2->AddNr = 0;
				}
			}

			Wire = &((*Wires)[WireCnt]);
			Wire->Info |= OBJECT_NOT_VISIBLE;

			if (mode == 0)
				Wire->DeleteNr = (int16) LastActionNr;
		}
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 RebuildJunctions(int32 mode, double *FailedJunctionX, double *FailedJunctionY)
{
	WireRecord *Wire, *Wire2;
	JunctionRecord *Junction, *Junction2;
	int32 cnt, cnt2;
	double x1, y1, x2, y2, x3, y3, x4, y4;
	char str[MAX_LENGTH_STRING];

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			for (cnt2 = 0; cnt2 < Design.NrJunctions; cnt2++)
			{
				if (cnt != cnt2)
				{
					Junction2 = &((*Junctions)[cnt2]);

					if ((Junction2->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((InRange(Junction->X, Junction2->X)) && (InRange(Junction->Y, Junction2->Y)))
							Junction2->Info |= OBJECT_NOT_VISIBLE;
					}
				}
			}
		}
	}

	memset(&ExtraJunction, 0, sizeof(JunctionRecord));

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;

			/*
			      x1=40.0;
			      y1=30.0;
			      x2=40.0;
			      y2=-10.0;
			*/
			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				if (cnt2 != cnt)
				{
					Wire2 = &((*Wires)[cnt2]);

					if ((Wire2->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						x3 = Wire2->X1;
						y3 = Wire2->Y1;
						x4 = Wire2->X2;
						y4 = Wire2->Y2;

						/*
						            x3=10.0;
						            y3=10.0;
						            x4=40.0;
						            y4=10.0;
						*/
						if ((max(x3, x4) > min(x1, x2) - 0.01) && (max(y3, y4) > min(y1, y2) - 0.01)
						        && (min(x3, x4) < max(x1, x2) + 0.01) && (min(y3, y4) < max(y1, y2) + 0.01))
						{
							if (mode == 1)
							{
								if (CheckJunctionToBePlaced(x1, y1, 0))
								{
									*FailedJunctionX = x1;
									*FailedJunctionY = y1;
									return 1;
								}
							}
							else
							{
								if (((NotInRange(x1, x3)) || (NotInRange(y1, y3)))
								        && ((NotInRange(x1, x4)) || (NotInRange(y1, y4))) && ((NotInRange(x2, x3))
								                || (NotInRange(y2, y3)))
								        && ((NotInRange(x2, x4)) || (NotInRange(y2, y4))))
								{
									if (TestLineConnectedToCircle(x1, y1, x2, y2, x3, y3, 0.04))
										CheckJunction(x3, y3, mode);
									else
									{
										if (TestLineConnectedToCircle(x1, y1, x2, y2, x4, y4, 0.04))
											CheckJunction(x4, y4, mode);
									}
								}
								else
								{
									if ((InRange(x1, x3)) && (InRange(y1, y3)))
									{
										if (CheckIfCoordinateMoreThenDouble(x1, y1, mode) == 1)
											CheckJunction(x1, y1, mode);
									}

									if ((InRange(x1, x4)) && (InRange(y1, y4)))
									{
										if (CheckIfCoordinateMoreThenDouble(x1, y1, mode) == 1)
											CheckJunction(x1, y1, mode);
									}

									if ((InRange(x2, x3)) && (InRange(y2, y3)))
									{
										if (CheckIfCoordinateMoreThenDouble(x2, y2, mode) == 1)
											CheckJunction(x2, y2, mode);
									}

									if ((InRange(x2, x4)) && (InRange(y2, y4)))
									{
										if (CheckIfCoordinateMoreThenDouble(x2, y2, mode) == 1)
											CheckJunction(x2, y2, mode);
									}
								}
							}
						}
					}
				}
			}
		}
	}

	/*
	  if ((mode==1)
	     &&
	     ((ExtraJunction.Info & (OBJECT_NOT_VISIBLE)) != 0)) {
	    *FailedJunctionX=ExtraJunction.X;
	    *FailedJunctionY=ExtraJunction.Y;
	    return 1;
	  }
	*/
	if (mode == 0)
	{
		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				JunctionCheckWire(cnt, 1);
		}

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				cnt2++;
		}

		if (cnt2 > 0)
		{
			sprintf(str, SC(311, "There are %d extra junctions added (selected)"), cnt2);
			MessageBoxUTF8(SCHWindow, str, SC(288, "Warning"), MB_APPLMODAL | MB_OK);
		}
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
