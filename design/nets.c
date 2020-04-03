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
#include "math.h"
#include "stdio.h"
#include "check.h"
#include "calc.h"
#include "files.h"
#include "string.h"
#include "utf8.h"

extern double LineCrossX, LineCrossY;

extern int32 UnNamedGlobalNetNr;

int32 WriteNetInfo(void);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ObjectTextToBuf(LPSTR ObjectText)
{
	int32 lengte, hulp;
	char *TextPos;

	if (ObjectText[0] == 0)
		lengte = 0;
	else
		lengte = strlen(ObjectText);

	if (lengte + 1 + ObjectTextBufPos > ObjectTextBufMemSize)
	{
		if (AllocateMemObjectTextBuf(ObjectTextBufMemSize + 65536) == -1)
			return 0;
	}

	hulp = ObjectTextBufPos;
	TextPos = ObjectTextBuf + ObjectTextBufPos;
	memmove(TextPos, ObjectText, lengte + 1);
	ObjectTextBufPos += lengte + 1;
	return hulp;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void FillInParametersObject2(Object2Record * Object2)
{
	int32 cnt, cnt3, cnt4, ObjectNr, res, LineNr;
	int32 ChangedPinBus;
	BusRecord *Bus;
	WireRecord *Wire;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;
	InstanceRecord *Instance;
	ObjectRecord *PinObject, *PinBusObject;
	char NewPinBusStr[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	RedefinedPinBusRecord *RedefinedPinBus;
#ifdef _DEBUG
	int32 ok;
#endif

	cnt = 0;
	ObjectNr = Object2->ObjectNr;
	Object2->Info4 = -1;

	switch (Object2->ObjectType)
	{
	case WIRE:
		Wire = &((*Wires)[ObjectNr]);
		Object2->x1 = Wire->X1;
		Object2->y1 = Wire->Y1;
		Object2->x2 = Wire->X2;
		Object2->y2 = Wire->Y2;
		break;

	case BUS:
		Bus = &((*Busses)[ObjectNr]);
		Object2->x1 = Bus->X1;
		Object2->y1 = Bus->Y1;
		Object2->x2 = Bus->X2;
		Object2->y2 = Bus->Y2;
		break;

	case BUS_CONNECTION:
		BusConnection = &((*BusConnections)[ObjectNr]);
		Object2->x1 = BusConnection->X;
		Object2->y1 = BusConnection->Y;
		Object2->Info2 = (int16) ((BusConnection->Alignment >> 14) & 3);
		break;

	case GLOBAL_CONNECTION:
		GlobalConnection = &((*GlobalConnections)[ObjectNr]);
		Object2->x1 = GlobalConnection->X;
		Object2->y1 = GlobalConnection->Y;
		Object2->Info2 = GlobalConnection->ConnectionType;
		Object2->Text1 = ObjectTextToBuf(GlobalConnection->Text);
		break;

	case NET_LABEL:
		if (ObjectNr != -1)
		{
			NetLabel = &((*NetLabels)[ObjectNr]);
			Object2->x1 = NetLabel->ConnectX;
			Object2->y1 = NetLabel->ConnectY;
			Object2->x2 = NetLabel->TextX;
			Object2->y2 = NetLabel->TextY;
			Object2->Text1 = ObjectTextToBuf(NetLabel->Name);
		}

		break;

	case SYMBOL_PIN:
		PinObject = &((*Objects)[ObjectNr]);
#ifdef _DEBUG

		if (cnt == 195)
			ok = 1;

		if (PinObject->Info3 == 1)
		{	// sheet symbol pin
			ok = 1;
		}

		if (PinObject->Info3 == 1)
		{	// sheet symbol pin
//        s1=GetObjectText(PinObject->Text1);
//        s2=GetObjectText(PinObject->Text2);
			ok = 1;
		}

#endif
		Object2->x1 = PinObject->x1;
		Object2->y1 = PinObject->y1;
		Object2->Info2 = PinObject->Info2;	// ConnectionType
		Instance = PinObject->Instance;

		if (PinObject->Info3 == 0)
		{	// symbol pin
			Object2->Text1 = ObjectTextToBuf(PinObject->Text1);	// Pin name
#if 0

			if (stricmp(Instance->SymbolName, "POWER_CONNECT") == 0)
			{
				Object2->Text2 = ObjectTextToBuf("POWER_CONNECT");	// power connect special symbol
			}
			else
			{
#endif
				Object2->Text2 = ObjectTextToBuf(PinObject->Text2);	// Label name
			}
			else
			{	// sheet symbol pin
				Object2->Text1 = ObjectTextToBuf(PinObject->Text1);	// Sheet name
				Object2->Text2 = ObjectTextToBuf(PinObject->Text2);	// Label name
			}

			if (Object2->Info2 == POWER_CONNECTION)
			{
				Object2->Text2 = ObjectTextToBuf(PinObject->Text1);	// Pin name
			}

			Object2->RefNum = ObjectTextToBuf(Instance->Reference);
			Object2->Info3 = PinObject->Info3;	// 0 = pin,1 = sheet pin
			Object2->Info5 = -1;
//        if (Object2->Info3==1) {
//          for (cnt2=0;cnt2<NrSheets;cnt2++) {
//            if (stricmp(PinObject->Text1,Sheets[cnt2].SheetName)==0) Object2->Info5=cnt2;
//          }
//        }
			break;

		case SYMBOL_PINBUS:
			PinBusObject = &((*Objects)[ObjectNr]);
			Object2->x1 = PinBusObject->x1;
			Object2->y1 = PinBusObject->y1;
			Object2->Info2 = PinBusObject->Info2;	// ConnectionType
			Object2->Text2 = ObjectTextToBuf(PinBusObject->Text2);	// Label name
			Instance = PinBusObject->Instance;
			Object2->RefNum = ObjectTextToBuf(Instance->Reference);
			Object2->Info3 = PinBusObject->Info3;	// Nr pins
			ChangedPinBus = 0;

			for (cnt3 = 0; cnt3 < Design.NrRedefinedPinBusses; cnt3++)
			{
				RedefinedPinBus = &((*RedefinedPinBusses)[cnt3]);

				if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if ((stricmpUTF8(RedefinedPinBus->Reference, Instance->Reference) == 0)
					        && (stricmpUTF8(RedefinedPinBus->Name, PinBusObject->Text2) == 0))
					{
						LineNr = 0;
						NewPinBusStr[0] = 0;

						for (cnt4 = 0; cnt4 < PinBusObject->Info3; cnt4++)
						{
							res =
							    GetPinNameFromPinBus(PinBusObject->Text1, str2, PinBusObject->Info3,
							                         RedefinedPinBus->Order[cnt4]);

							if (res > LineNr)
								strcat(NewPinBusStr, "\\");
							else if (cnt4 > 0)
								strcat(NewPinBusStr, ",");

							strcat(NewPinBusStr, str2);
							LineNr = res;
							ChangedPinBus = 1;
						}
					}
				}
			}

			if (ChangedPinBus)
			{
				Object2->Text1 = ObjectTextToBuf(NewPinBusStr);	// Reordered pin names
			}
			else
			{
				Object2->Text1 = ObjectTextToBuf(PinBusObject->Text1);	// Pin names
			}

			break;
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	void FillInParametersObjects2(int32 Start)
	{
		int32 cnt;
		Object2Record *Object2;

		for (cnt = Start; cnt < NrObjects2; cnt++)
		{
			Object2 = &((*Objects2)[cnt]);
			FillInParametersObject2(Object2);
		}
	}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 WireToObject(WireRecord * Wire, int32 ObjectNr)
	{
		Object2Record *Object2;
#ifdef _DEBUG
		int32 ok;
#endif

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

#ifdef _DEBUG

		if (((InRange(152.0, Wire->X1)) && (InRange(95.0, Wire->Y1)))
		        && ((InRange(152.0, Wire->X2)) && (InRange(78.0, Wire->Y2))))
			ok = 1;

#endif
		Object2 = &((*Objects2)[NrObjects2]);
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = WIRE;
		Object2->ObjectNr = ObjectNr;
		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 BusToObject(BusRecord * Bus, int32 ObjectNr)
	{
		Object2Record *Object2;

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

		Object2 = &((*Objects2)[NrObjects2]);
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = BUS;
		Object2->ObjectNr = ObjectNr;
		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 BusConnectionToObject(BusConnectionRecord * BusConnection, int32 ObjectNr, int32 mode)
	{
		Object2Record *Object2;

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

		Object2 = &((*Objects2)[NrObjects2]);
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = BUS_CONNECTION;
		Object2->ObjectNr = ObjectNr;

		if (mode == 1)
			Object2->Info3 = 1;

		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 GlobalConnectionToObject(GlobalConnectionRecord * GlobalConnection, int32 ObjectNr)
	{
		Object2Record *Object2;

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

		Object2 = &((*Objects2)[NrObjects2]);
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = GLOBAL_CONNECTION;
		Object2->ObjectNr = ObjectNr;
		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 NetLabelToObject(NetLabelRecord * NetLabel, int32 ObjectNr)
	{
		Object2Record *Object2;
#ifdef _DEBUG
		int32 ok;
#endif

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

		Object2 = &((*Objects2)[NrObjects2]);
#ifdef _DEBUG

		if (NrObjects2 == 1188)
			ok = 1;

#endif
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = NET_LABEL;
		Object2->ObjectNr = ObjectNr;
		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 OnePinNetToObject(OnePinNetRecord * OnePinNet, int32 ObjectNr)
	{
		Object2Record *Object2;
#ifdef _DEBUG
		int32 ok;
#endif

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

		Object2 = &((*Objects2)[NrObjects2]);
#ifdef _DEBUG

		if (NrObjects2 == 1188)
			ok = 1;

#endif
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = ONE_PIN_NET;
		Object2->ObjectNr = ObjectNr;
		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 PinToObject(ObjectRecord * PinObject, int32 ObjectNr)
	{
		Object2Record *Object2;
#ifdef _DEBUG
		int32 ok;
#endif

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

#ifdef _DEBUG

		if ((InRange(PinObject->x1, 125.0)) && (InRange(PinObject->y1, 55.0)))
			ok = 1;

		if (NrObjects2 == 195)
			ok = 1;

#endif
		Object2 = &((*Objects2)[NrObjects2]);
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = PinObject->ObjectType;
		Object2->ObjectNr = ObjectNr;
		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 PowerPinToObject(ObjectRecord * PowerPinObject, int32 ObjectNr)
	{
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 PinBusToObject(ObjectRecord * PinBusObject, int32 ObjectNr)
	{
		Object2Record *Object2;

		if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
			return 0;

		Object2 = &((*Objects2)[NrObjects2]);
		memset(Object2, 0, sizeof(Object2Record));
		Object2->ObjectType = PinBusObject->ObjectType;
		Object2->ObjectNr = ObjectNr;
		NrObjects2++;
		return 1;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 CollectObjectsConnectedToInstancePin(ObjectRecord * PinObject)
	{
		int32 cnt, cnt2, cnt3, cnt4, start, stop, FoundJunction;
		double x1, y1;

		WireRecord *Wire;
		BusRecord *Bus;
		ObjectRecord *Object;
		JunctionRecord *Junction = NULL;
		OnePinNetRecord *OnePinNet;

		x1 = PinObject->x1;
		y1 = PinObject->y1;

		for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
		{
			Wire = &((*Wires)[cnt3]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, Wire->X1)) && (InRange(y1, Wire->Y1)))
				        || ((InRange(x1, Wire->X2)) && (InRange(y1, Wire->Y2))))
				{
					if (!WireToObject(Wire, cnt3))
						return -1;
				}
				else
				{
					if (TestLineConnectedToCircle(Wire->X1, Wire->Y1, Wire->X2, Wire->Y2, x1, y1, 0.04))
					{
						FoundJunction = 0;

						if (stricmp(Design.Identification, SheetCode1) != 0)
						{
							for (cnt4 = 0; cnt4 < Design.NrJunctions; cnt4++)
							{
								Junction = &((*Junctions)[cnt4]);

								if (((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0) && (InRange(Junction->X, x1))
								        && (InRange(Junction->Y, y1)))
									FoundJunction = 1;
							}
						}
						else
							FoundJunction = 2;

						if (FoundJunction)
						{
							if (FoundJunction == 1)
								Junction->Info |= OBJECT_DONE;

							if (!WireToObject(Wire, cnt3))
								return -1;
						}
					}
				}
			}
		}

		if (PinObject->Info3 == 1)
		{
			for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
			{
				Bus = &((*Busses)[cnt3]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
				{
					if (((InRange(x1, Bus->X1)) && (InRange(y1, Bus->Y1)))
					        || ((InRange(x1, Bus->X2)) && (InRange(y1, Bus->Y2))))
					{
						if (!BusToObject(Bus, cnt3))
							return -1;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			start = (*ObjectsPosition1)[cnt];
			stop = (*ObjectsPosition1)[cnt + 1];

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Object != PinObject) && (Object->ObjectType == SYMBOL_PIN) && ((Object->Info & OBJECT_DONE) == 0)
				        && (InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
				{
					if (!PinToObject(Object, cnt2))
						return -1;

					Object->Info |= OBJECT_DONE;
				}
			}
		}

		for (cnt4 = 0; cnt4 < Design.NrOnePinNets; cnt4++)
		{
			OnePinNet = &((*OnePinNets)[cnt4]);

			if (((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == 0) && (InRange(OnePinNet->X, x1))
			        && (InRange(OnePinNet->Y, y1)))
			{
				OnePinNet->Info |= OBJECT_DONE;

				if (!OnePinNetToObject(OnePinNet, cnt4))
					return -1;
			}
		}

		return 0;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 CollectObjectsConnectedToInstancePinBus(ObjectRecord * PinBusObject)
	{
		int32 cnt3;
		double x1, y1;

		BusRecord *Bus;

		x1 = PinBusObject->x1;
		y1 = PinBusObject->y1;

		for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
		{
			Bus = &((*Busses)[cnt3]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, Bus->X1)) && (InRange(y1, Bus->Y1)))
				        || ((InRange(x1, Bus->X2)) && (InRange(y1, Bus->Y2))))
				{
					if (!BusToObject(Bus, cnt3))
						return -1;
				}
			}
		}

		return 0;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 CollectObjectsConnectedToWire(WireRecord * TestWire)
	{
		int32 cnt, cnt2, cnt3, cnt4, start, stop, ok, CheckJunction, Connected;
		double x1, y1, x2, y2, x3, y3, x4, y4, x5, y5;

		WireRecord *Wire;
		BusConnectionRecord *BusConnection;
		GlobalConnectionRecord *GlobalConnection;
		JunctionRecord *Junction;
		NetLabelRecord *NetLabel;
		ObjectRecord *Object;
		x1 = TestWire->X1;
		y1 = TestWire->Y1;
		x2 = TestWire->X2;
		y2 = TestWire->Y2;

		for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
		{
			Wire = &((*Wires)[cnt3]);
			x3 = Wire->X1;
			y3 = Wire->Y1;
			x4 = Wire->X2;
			y4 = Wire->Y2;

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, x3)) && (InRange(y1, y3))) || ((InRange(x1, x4)) && (InRange(y1, y4)))
				        || ((InRange(x2, x3)) && (InRange(y2, y3))) || ((InRange(x2, x4)) && (InRange(y2, y4))))
				{
					if (!WireToObject(Wire, cnt3))
						return -1;
				}
				else
				{
					CheckJunction = 0;
					x5 = 0.0;
					y5 = 0.0;

					if (TestLineConnectedToCircle(x1, y1, x2, y2, x3, y3, 0.04))
					{
						x5 = x3;
						y5 = y3;
						CheckJunction = 1;
					}

					if (TestLineConnectedToCircle(x1, y1, x2, y2, x4, y4, 0.04))
					{
						x5 = x4;
						y5 = y4;
						CheckJunction = 1;
					}

					if (TestLineConnectedToCircle(x3, y3, x4, y4, x1, y1, 0.04))
					{
						x5 = x1;
						y5 = y1;
						CheckJunction = 1;
					}

					if (TestLineConnectedToCircle(x3, y3, x4, y4, x2, y2, 0.04))
					{
						x5 = x2;
						y5 = y2;
						CheckJunction = 1;
					}

					if (CheckJunction)
					{
						if (stricmp(Design.Identification, SheetCode1) != 0)
						{
							for (cnt4 = 0; cnt4 < Design.NrJunctions; cnt4++)
							{
								Junction = &((*Junctions)[cnt4]);

								if (((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0) && (InRange(Junction->X, x5))
								        && (InRange(Junction->Y, y5)))
								{
									Junction->Info |= OBJECT_DONE;

									if (!WireToObject(Wire, cnt3))
										return -1;

									ok = 1;
								}
							}
						}
						else
						{
							if (!WireToObject(Wire, cnt3))
								return -1;
						}
					}
					else
					{
						if (LineCrosses(x1, y1, x2, y2, x3, y3, x4, y4) == 1)
						{
							ok = 1;

							for (cnt4 = 0; cnt4 < Design.NrJunctions; cnt4++)
							{
								Junction = &((*Junctions)[cnt4]);

								if (((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0) && (InRange(Junction->X, LineCrossX))
								        && (InRange(Junction->Y, LineCrossY)))
								{
									Junction->Info |= OBJECT_DONE;

									if (!WireToObject(Wire, cnt3))
										return -1;

									ok = 1;
								}
							}
						}
					}
				}
			}
		}

		for (cnt3 = 0; cnt3 < Design.NrGlobalConnections; cnt3++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt3]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, GlobalConnection->X)) && (InRange(y1, GlobalConnection->Y)))
				        || ((InRange(x2, GlobalConnection->X)) && (InRange(y2, GlobalConnection->Y))))
				{
					if (!GlobalConnectionToObject(GlobalConnection, cnt3))
						return -1;

					GlobalConnection->Info |= OBJECT_DONE;
				}
			}
		}

		for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
		{
			BusConnection = &((*BusConnections)[cnt3]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, BusConnection->X)) && (InRange(y1, BusConnection->Y)))
				        || ((InRange(x2, BusConnection->X)) && (InRange(y2, BusConnection->Y))))
				{
					if (!BusConnectionToObject(BusConnection, cnt3, 0))
						return -1;

					BusConnection->Info |= OBJECT_DONE;
				}
			}
		}

		for (cnt3 = 0; cnt3 < Design.NrNetLabels; cnt3++)
		{
			NetLabel = &((*NetLabels)[cnt3]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, NetLabel->ConnectX)) && (InRange(y1, NetLabel->ConnectY)))
				        || ((InRange(x2, NetLabel->ConnectX)) && (InRange(y2, NetLabel->ConnectY))))
				{
					if (!NetLabelToObject(NetLabel, cnt3))
						return -1;

					NetLabel->Info |= OBJECT_DONE;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			start = (*ObjectsPosition1)[cnt];
			stop = (*ObjectsPosition1)[cnt + 1];

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
				Connected = 0;

				if ((Object->ObjectType == SYMBOL_PIN) && ((Object->Info & OBJECT_DONE) == 0))
				{
					if (((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
					        || ((InRange(Object->x1, x2)) && (InRange(Object->y1, y2))))
						Connected = 1;
					else
					{
						if (TestLineConnectedToCircle(x1, y1, x2, y2, Object->x1, Object->y1, 0.04))
						{
							if (stricmp(Design.Identification, SheetCode1) != 0)
							{
								for (cnt4 = 0; cnt4 < Design.NrJunctions; cnt4++)
								{
									Junction = &((*Junctions)[cnt4]);

									if (((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
									        && (InRange(Junction->X, Object->x1)) && (InRange(Junction->Y, Object->y1)))
										Connected = 1;
								}
							}
							else
								Connected = 1;
						}
					}

					if (Connected)
					{
						if (!PinToObject(Object, cnt2))
							return -1;

						Object->Info |= OBJECT_DONE;
					}
				}
			}
		}

		return 0;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 CollectObjectsConnectedToBus(BusRecord * TestBus)
	{
		int32 cnt, cnt2, cnt3, start, stop;
		double x1, y1, x2, y2, x3, y3, x4, y4;

		BusRecord *Bus;
		BusConnectionRecord *BusConnection;
		GlobalConnectionRecord *GlobalConnection;
		NetLabelRecord *NetLabel;
		InstanceRecord *Instance;
		ObjectRecord *Object;

		x1 = TestBus->X1;
		y1 = TestBus->Y1;
		x2 = TestBus->X2;
		y2 = TestBus->Y2;

		for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
		{
			Bus = &((*Busses)[cnt3]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, Bus->X1)) && (InRange(y1, Bus->Y1)))
				        || ((InRange(x1, Bus->X2)) && (InRange(y1, Bus->Y2))) || ((InRange(x2, Bus->X1))
				                && (InRange(y2, Bus->Y1)))
				        || ((InRange(x2, Bus->X2)) && (InRange(y2, Bus->Y2)))
				        || (TestLinesConnected(x1, y1, x2, y2, Bus->X1, Bus->Y1, Bus->X2, Bus->Y2, 0)))
				{
					if (!BusToObject(Bus, cnt3))
						return -1;
				}
			}
		}

		for (cnt3 = 0; cnt3 < Design.NrGlobalConnections; cnt3++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt3]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, GlobalConnection->X)) && (InRange(y1, GlobalConnection->Y)))
				        || ((InRange(x2, GlobalConnection->X)) && (InRange(y2, GlobalConnection->Y))))
				{
					if (!GlobalConnectionToObject(GlobalConnection, cnt3))
						return -1;

					GlobalConnection->Info |= OBJECT_DONE;
				}
			}
		}

		for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
		{
			BusConnection = &((*BusConnections)[cnt3]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, BusConnection->X)) && (InRange(y1, BusConnection->Y)))
				        || ((InRange(x2, BusConnection->X)) && (InRange(y2, BusConnection->Y))))
				{
					if (!BusConnectionToObject(BusConnection, cnt3, 0))
						return -1;

					BusConnection->Info |= OBJECT_DONE;
				}
			}

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE2)) == 0)
			{
				x3 = BusConnection->X;
				y3 = BusConnection->Y;
				x4 = x3;
				y4 = y3;

				switch ((BusConnection->Alignment >> 14) & 3)
				{
				case 0:
					x4 = x3 - 2.0;
					break;

				case 1:
					y4 = y3 - 2.0;
					break;

				case 2:
					x4 = x3 + 2.0;
					break;

				case 3:
					y4 = y3 + 2.0;
					break;
				}

				if (TestLineConnectedToCircle(x1, y1, x2, y2, x4, y4, 0.04))
				{
					if (!BusConnectionToObject(BusConnection, cnt3, 1))
						return -1;

					BusConnection->Info |= OBJECT_DONE2;
				}
			}
		}

		for (cnt3 = 0; cnt3 < Design.NrNetLabels; cnt3++)
		{
			NetLabel = &((*NetLabels)[cnt3]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				if (((InRange(x1, NetLabel->ConnectX)) && (InRange(y1, NetLabel->ConnectY)))
				        || ((InRange(x2, NetLabel->ConnectX)) && (InRange(y2, NetLabel->ConnectY))))
				{
					if (!NetLabelToObject(NetLabel, cnt3))
						return -1;

					NetLabel->Info |= OBJECT_DONE;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			start = (*ObjectsPosition1)[cnt];
			stop = (*ObjectsPosition1)[cnt + 1];

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Instance->Info & SHEET_SYMBOL) == SHEET_SYMBOL)
				{
					if (((Object->Info & OBJECT_DONE) == 0)
					        && (((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
					            || ((InRange(Object->x1, x2)) && (InRange(Object->y1, y2)))))
					{
						if (!PinToObject(Object, cnt2))
							return -1;

						Object->Info |= OBJECT_DONE;
					}
				}
				else
				{
					if ((Object->ObjectType == SYMBOL_PINBUS) && ((Object->Info & OBJECT_DONE) == 0)
					        && (((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
					            || ((InRange(Object->x1, x2)) && (InRange(Object->y1, y2)))))
					{
						if (!PinBusToObject(Object, cnt2))
							return -1;

						Object->Info |= OBJECT_DONE;
					}
				}
			}
		}

		return 0;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 SearchUnconnectedObject(int16 * ObjectType, int32 * ObjectNr)
	{
		int32 cnt2, cnt3;

		ObjectRecord *Object;
		WireRecord *Wire;
		BusRecord *Bus;

		for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
		{
			Wire = &((*Wires)[cnt3]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				*ObjectType = WIRE;
				*ObjectNr = cnt3;
				return 1;
			}
		}

		for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
		{
			Bus = &((*Busses)[cnt3]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				*ObjectType = BUS;
				*ObjectNr = cnt3;
				return 1;
			}
		}

		for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
		{
			Object = &((*Objects)[cnt2]);

			if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_DONE)) == 0)
			{
				*ObjectType = PIN_OBJECT;
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

	int32 nets1(int32 SheetNr, double *x, double *y)
	{
		int32 cnt, cnt2, ObjectNr, StartObjectNr, Object2Count, StartObjectsSheet, ok, NrObjectNetLabels,
		      NrObjectSheetPins, NrObjectPins;
		char str2[200];
		LPSTR FirstSheetSymbolPinName;
		int16 ObjectType;
		InstanceRecord *Instance;
		ObjectRecord *Object;
		Object2Record *Object2;
		WireRecord *Wire;
		BusRecord *Bus;

		ClearErrors(0);

		NrObjects = 0;

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
#ifdef _DEBUG

				if (stricmp(Instance->Reference, "K4") == 0)
					ok = 1;

				if (stricmp(Instance->SymbolName, "GND") == 0)
					ok = 1;

#endif
				InstancePinsToObject(Instance, 0);
			}

			if ((NrInt32Objects1 + 1 >= MaxNrInt32Objects1)
			        && (AllocateMemObjectsPosition1(MaxNrInt32Objects1 + 128) != 0))
				return -1;

			(*ObjectsPosition1)[cnt + 1] = NrObjects;
			NrInt32Objects1++;
		}

		ok = 1;

		for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
		{
			Object = &((*Objects)[cnt2]);
			Object->Info = 0;
		}

		StartObjectsSheet = NrObjects2;

		while (SearchUnconnectedObject(&ObjectType, &ObjectNr))
		{
			StartObjectNr = NrObjects2;
#ifdef _DEBUG

			if (TotalNrNets == 264)
				ok = 1;

			if (StartObjectNr == 810)
				ok = 1;

#endif

			switch (ObjectType)
			{
			case WIRE:
				Wire = &((*Wires)[ObjectNr]);

				if (!WireToObject(Wire, ObjectNr))
					return -1;

				Wire->Info |= OBJECT_DONE;

				if (CollectObjectsConnectedToWire(Wire) == -1)
					return -1;

				break;

			case BUS:
				Bus = &((*Busses)[ObjectNr]);

				if (!BusToObject(Bus, ObjectNr))
					return -1;

				Bus->Info |= OBJECT_DONE;

				if (CollectObjectsConnectedToBus(Bus) == -1)
					return -1;

				break;

			case PIN_OBJECT:
				Object = &((*Objects)[ObjectNr]);

				switch (Object->ObjectType)
				{
				case SYMBOL_PIN:
					if (!PinToObject(Object, ObjectNr))
						return -1;

					Object->Info |= OBJECT_DONE;

					if (CollectObjectsConnectedToInstancePin(Object) == -1)
						return -1;

					break;

				case SYMBOL_PINBUS:
					if (!PinBusToObject(Object, ObjectNr))
						return -1;

					Object->Info |= OBJECT_DONE;

					if (CollectObjectsConnectedToInstancePinBus(Object) == -1)
						return -1;

					break;
				}

				break;
			}

			cnt2 = StartObjectNr + 1;

			while (cnt2 < NrObjects2)
			{
				Object2 = &((*Objects2)[cnt2]);
				ObjectNr = Object2->ObjectNr;

				switch (Object2->ObjectType)
				{
				case WIRE:
					Wire = &((*Wires)[ObjectNr]);
					Wire->Info |= OBJECT_DONE;

					if (CollectObjectsConnectedToWire(Wire) == -1)
						return -1;

					break;

				case BUS:
					Bus = &((*Busses)[ObjectNr]);
					Bus->Info |= OBJECT_DONE;

					if (CollectObjectsConnectedToBus(Bus) == -1)
						return -1;

					break;

				case SYMBOL_PIN:
					Object = &((*Objects)[ObjectNr]);
					Object->Info |= OBJECT_DONE;

					if (CollectObjectsConnectedToInstancePin(Object) == -1)
						return -1;

					break;

				case SYMBOL_PINBUS:
					Object = &((*Objects)[ObjectNr]);
					Object->Info |= OBJECT_DONE;

					if (CollectObjectsConnectedToInstancePinBus(Object) == -1)
						return -1;

					break;
				}

				cnt2++;
			}

			Object2Count = NrObjects2 - StartObjectNr;

// *******************************************************************************************************

			NrObjectNetLabels = 0;
			NrObjectSheetPins = 0;
			NrObjectPins = 0;
			FirstSheetSymbolPinName = 0;

			for (cnt2 = StartObjectNr; cnt2 < NrObjects2; cnt2++)
			{
				Object2 = &((*Objects2)[cnt2]);

				switch (Object2->ObjectType)
				{
				case SYMBOL_PIN:
					FillInParametersObject2(Object2);

					if (Object2->Info3 == 0)
						NrObjectPins++;
					else
					{
						NrObjectSheetPins++;

						if (FirstSheetSymbolPinName == 0)
						{
							FirstSheetSymbolPinName = GetObjectText(Object2->Text2);
							ok = 1;
						}
					}

					break;

				case NET_LABEL:
					NrObjectNetLabels++;
					break;
				}
			}

			if ((NrObjectSheetPins > 0) && (NrObjectNetLabels == 0))
			{

				if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
					return 0;

				Object2 = &((*Objects2)[NrObjects2]);
#ifdef _DEBUG

				if (NrObjects2 == 1188)
					ok = 1;

#endif
				sprintf(str2, "N$%i", UnNamedGlobalNetNr);
				UnNamedGlobalNetNr++;
				memset(Object2, 0, sizeof(Object2Record));
				Object2->ObjectType = NET_LABEL;
				Object2->ObjectNr = -1;
//      Object2->Text1=ObjectTextToBuf(FirstSheetSymbolPinName);
				Object2->Text1 = ObjectTextToBuf(str2);
				NrObjects2++;
			}

// *******************************************************************************************************


			if (TotalNrNets + 1 >= NetInfoSize)
			{
				if (AllocateMemNetInfo(TotalNrNets + 128) != 0)
					return -1;
			}

			(*NetInfos)[TotalNrNets].Info = 0;
			(*NetInfos)[TotalNrNets + 1].Pos = NrObjects2;
			TotalNrNets++;
		}

		FillInParametersObjects2(StartObjectsSheet);
		Sheets[SheetNr].NrNets = TotalNrNets - Sheets[SheetNr].NetPos;
		Sheets[SheetNr + 1].NetPos = TotalNrNets;
		*x = 100000.0;
		*y = 100000.0;

		if (CheckNets(SheetNr, 0, x, y) == -1)
		{
#ifdef _DEBUG

			if (WriteNetInfo() == -1)
				return -1;

#endif
			return -2;
		}

		return 0;
	}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

	int32 CheckNets(int32 SheetNr, int32 mode, double *x, double *y)
	{
		int32 cnt, cnt2, cnt3, cnt4, res, start, stop, NrObjectPins, NrObjectSheetPins, NrObjectPinBusses,
		      NrObjectWires, NrObjectBusses, NrObjectBusConnections1, NrObjectBusConnections2, ok, NrPinsPinBus, NrNets,
		      NrObjectGlobalConnections, NrPowerPins, NrObjectNetLabels, start2, stop2, ObjectNr, GlobalConnectionType,
		      GlobalConnX1, GlobalConnY1, BusConnX1, BusConnY1, BusConnX2, BusConnY2, PinBusX1, PinBusY1, FirstPinX,
		      FirstPinY, FirstSheetPinX, FirstSheetPinY, PinType, NrObjectOnePinNets, GlobalConnectionError, TotalError,
		      PowerPinConn, NetLabelTextX, NetLabelTextY, PowerNameText, BusX1, BusY1, BusX2, BusY2;
		Object2Record *Object2, *Object2a;
		LPSTR GlobalConnectionText, NetLabelText, FirstSheetSymbolPinName, SymbolName;
		char str[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING];
#ifdef _DEBUG
		LPSTR T1, T2;
#endif

		BusX1 = 0;
		BusY1 = 0;
		BusX2 = 0;
		BusY2 = 0;
		BusConnX1 = 0;
		BusConnY1 = 0;
		BusConnX2 = 0;
		BusConnY2 = 0;
		GlobalConnX1 = 0;
		GlobalConnY1 = 0;
		NrPinsPinBus = 0;
		NetLabelTextX = 0;
		NetLabelTextY = 0;
		NetLabelText = NULL;
		TotalError = 0;
		NrNets = Sheets[SheetNr].NrNets;

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[SheetNr].SheetName);

		for (cnt = Sheets[SheetNr].NetPos; cnt < Sheets[SheetNr].NetPos + NrNets; cnt++)
		{
#ifdef _DEBUG

			if (cnt == 264)
				ok = 1;

#endif
			(*NetInfos)[cnt].Info = 0;
			start = (*NetInfos)[cnt].Pos;
			stop = (*NetInfos)[cnt + 1].Pos;
			NrObjectPins = 0;
			NrObjectSheetPins = 0;
			NrObjectPinBusses = 0;
			NrObjectWires = 0;
			NrObjectBusses = 0;
			NrObjectOnePinNets = 0;
			NrObjectBusConnections1 = 0;
			NrObjectBusConnections2 = 0;
			NrObjectGlobalConnections = 0;
			FirstSheetSymbolPinName = 0;
			NrObjectNetLabels = 0;
			GlobalConnectionType = 0;
			NrPowerPins = 0;
			PinType = -1;
			PowerPinConn = 0;
			(*NetInfos)[cnt].BusConnectionObjectNr = -1;
			(*NetInfos)[cnt].NetLabelObjectNr = -1;
			(*NetInfos)[cnt].PinBusObjectNr = -1;
			(*NetInfos)[cnt].SheetNr = SheetNr;
			(*NetInfos)[cnt].GlobalConnObjectNr = -1;
			(*NetInfos)[cnt].PinBusCount = 0;
			(*NetInfos)[cnt].NrPinsPinBus = 0;
			(*NetInfos)[cnt].Presence = 0;

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
			PowerNameText = -1;

			if (cnt == 156)
				ok = 1;

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				Object2 = &((*Objects2)[cnt2]);
				Object2->SheetNr = (int16) SheetNr;
				Object2->NetNr = (int16) cnt;

				switch (Object2->ObjectType)
				{
				case SYMBOL_PIN:
					FirstPinX = (int32) Object2->x1;
					FirstPinY = (int32) Object2->y1;

					if (Object2->Info3 == 0)
						NrObjectPins++;
					else
					{
						NrObjectSheetPins++;

						if (FirstSheetSymbolPinName == 0)
						{
							FirstSheetSymbolPinName = GetObjectText(Object2->Text1);
							ok = 1;
						}

						FirstSheetPinX = (int32) Object2->x1;
						FirstSheetPinY = (int32) Object2->y1;
					}

					SymbolName = GetObjectText(Object2->Text2);

					if (Object2->Info2 == POWER_CONNECTION)
					{
						PowerPinConn = 1;

						if (PowerNameText == -1)
						{
							PowerNameText = Object2->Text2;
#ifdef _DEBUG
							T2 = GetObjectText(PowerNameText);
#endif
							NrPowerPins++;
						}
						else
						{
#ifdef _DEBUG
							T1 = GetObjectText(Object2->Text2);
#endif

							if (stricmpUTF8(GetObjectText(Object2->Text2), GetObjectText(PowerNameText)) != 0)
								NrPowerPins++;
						}
					}

					break;

				case ONE_PIN_NET:
					NrObjectOnePinNets++;
					break;

				case WIRE:
					NrObjectWires++;
					break;

				case BUS:
					NrObjectBusses++;
					BusX1 = (int32) Object2->x1;
					BusY1 = (int32) Object2->y1;
					BusX2 = (int32) Object2->x2;
					BusY2 = (int32) Object2->y2;
					break;

				case NET_LABEL:
#ifdef _DEBUG
					if (cnt == 264)
						ok = 1;

#endif
					NrObjectNetLabels++;

					if (NrObjectNetLabels == 1)
					{
						NetLabelText = GetObjectText(Object2->Text1);
						NetLabelTextX = (int32) (Object2->x1 + Object2->x2);
						NetLabelTextY = (int32) (Object2->y1 + Object2->y2);
						(*NetInfos)[cnt].NetLabelObjectNr = cnt2;
					}

					if ((NrObjectNetLabels > 1) && (stricmpUTF8(NetLabelText, GetObjectText(Object2->Text1)) != 0))
					{
						sprintf(str, SC(200, "%s: For the same net different labels (%s,%s)\r\n"), FileName,
						        NetLabelText, GetObjectText(Object2->Text1));
						AddMessage(str);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
						*x = Object2->x1 + Object2->x2;
						*y = Object2->y1 + Object2->y2;
						TotalError = 1;
					}

					break;

				case SYMBOL_PINBUS:
					NrObjectPinBusses++;

					if (NrObjectPinBusses == 1)
					{
						PinBusX1 = (int32) Object2->x1;
						PinBusY1 = (int32) Object2->y1;
						NrPinsPinBus = Object2->Info3;
						(*NetInfos)[cnt].PinBusObjectNr = cnt2;
						(*NetInfos)[cnt].NrPinsPinBus = Object2->Info3;
					}

					if (NrObjectPinBusses > 1)
					{
						if (NrPinsPinBus != Object2->Info3)
						{
							sprintf(str, SC(201, "%s: Nr pins pin busses are not equal (%i,%i)\r\n"), FileName,
							        (int32) Object2->x1, (int32) Object2->y1);
							AddMessage(str);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
							*x = Object2->x1;
							*y = Object2->y1;
							TotalError = 1;
						}
					}

					break;

				case BUS_CONNECTION:
					(*NetInfos)[cnt].BusConnectionObjectNr = cnt2;

					if (Object2->Info3 == 0)
					{
						NrObjectBusConnections1++;

						if (NrObjectBusConnections1 == 1)
						{
							BusConnX1 = (int32) (Object2->x1);
							BusConnY1 = (int32) (Object2->y1);
						}

						ObjectNr = Object2->ObjectNr;

						for (cnt3 = Sheets[SheetNr].NetPos; cnt3 < Sheets[SheetNr].NetPos + NrNets; cnt3++)
						{
							start2 = (*NetInfos)[cnt3].Pos;
							stop2 = (*NetInfos)[cnt3 + 1].Pos;

							for (cnt4 = start2; cnt4 < stop2; cnt4++)
							{
								Object2a = &((*Objects2)[cnt4]);

								if (Object2a->ObjectType == BUS_CONNECTION)
								{
									if ((Object2a->Info3 == 1) && (Object2a->ObjectNr == ObjectNr))
										Object2->Info4 = cnt3;
								}
							}
						}

						if (NrObjectBusConnections1 > 1)
						{
							sprintf(str, SC(202, "%s: Only one bus connection (%i,%i) allowed\r\n"), FileName,
							        (int32) Object2->x1, (int32) Object2->y1);
							AddMessage(str);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
							*x = Object2->x1;
							*y = Object2->y1;
							TotalError = 1;
						}
					}
					else
					{
						NrObjectBusConnections2++;

						if (NrObjectBusConnections2 == 1)
						{
							BusConnX2 = (int32) (Object2->x1);
							BusConnY2 = (int32) (Object2->y1);
						}
					}

					break;

				case GLOBAL_CONNECTION:
					NrObjectGlobalConnections++;

					if (NrObjectGlobalConnections == 1)
					{
						GlobalConnX1 = (int32) (Object2->x1);
						GlobalConnY1 = (int32) (Object2->y1);
						(*NetInfos)[cnt].GlobalConnObjectNr = cnt2;
					}

					if (NrObjectGlobalConnections == 2)
					{
						GlobalConnectionError = 1;
						sprintf(str, SC(203, "%s: Only one external connection %s allowed\r\n"), FileName,
						        GetObjectText(Object2->Text1));
						AddMessage(str);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
						TotalError = 1;
						*x = Object2->x1;
						*y = Object2->y1;
						GlobalConnectionText = GetObjectText(Object2->Text1);
					}

					break;
				}
			}

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

#ifdef _DEBUG

			if (cnt == 273)
				ok = 1;

#endif

			ok = 1;

			if ((NrObjectBusConnections1 > 0) && (NrObjectBusses > 0) && (NrObjectPinBusses == 0))
			{
				sprintf(str, SC(204, "%s: Only a pinbus is allowed to connect to a busconnection (%i,%i)\r\n"),
				        FileName, BusConnX1, BusConnY1);
				TotalError = 1;
				AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				*x = BusConnX1;
				*y = BusConnY1;
			}

			if ((NrObjectBusses > 0) && (NrObjectBusConnections1 == 0) && (NrObjectNetLabels == 0))
			{
				sprintf(str, SC(205, "%s: No label connected to a bus (%i,%i - %i,%i)\r\n"), FileName, BusX1, BusY1,
				        BusX2, BusY2);
				*x = BusX1;
				*y = BusY1;
				AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				TotalError = 1;
			}

			if ((NrObjectBusConnections1 > 0) && (NrObjectNetLabels == 0))
			{
				sprintf(str, SC(206, "%s: No label connected to bus connection (%i,%i)\r\n"), FileName, BusConnX1,
				        BusConnY1);
				TotalError = 1;
				AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				*x = BusConnX1;
				*y = BusConnY1;
			}

			/*
			    if ((NrObjectGlobalConnections>0)
			       &&
			       (NrObjectNetLabels==0)) {
			      sprintf(str,"No label connected to external connection (%i,%i)\r\n",
			                          GlobalConnX1,GlobalConnY1);
			      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
			      TotalError=1;
			    }
			*/

			if ((NrObjectGlobalConnections > 0) && (NrObjectBusConnections1 > 0))
			{
				sprintf(str, SC(207, "%s: Bus and external connections can not be combined (%i,%i)\r\n"), FileName,
				        GlobalConnX1, GlobalConnY1);
				AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				*x = GlobalConnX1;
				*y = GlobalConnY1;
				TotalError = 1;
			}

			if ((PowerPinConn) && (NrObjectGlobalConnections > 0))
			{
				sprintf(str, SC(208, "%s: Power and external connections can not be combined (%i,%i)\r\n"), FileName,
				        GlobalConnX1, GlobalConnY1);
				AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				*x = GlobalConnX1;
				*y = GlobalConnY1;
				TotalError = 1;
			}

			if ((PowerPinConn) && (NrObjectNetLabels == 1))
			{
				sprintf(str, SC(209, "Warning: Power connection (%s) connected to label (%s) (%i,%i)\r\n"),
				        GetObjectText(PowerNameText), NetLabelText, NetLabelTextX, NetLabelTextY);
				AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				TotalError = 0;
			}

			/*
			    if ((NrObjectSheetPins>0)
			       &&
			       (NrObjectNetLabels==0)
			       &&
			       (NrObjectBusConnections1>0)) {
			      sprintf(str,"No label connected to a sheet symbol pin (%i,%i)\r\n",
			                  FirstSheetPinX,FirstSheetPinY);
			      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
			      TotalError=1;
			    }
			*/
			if ((NrObjectPinBusses > 0) && (NrObjectBusses > 0))
			{
				res = GetNrLabelsBusName(NetLabelText);

				if ((res < 1) || ((res != NrPinsPinBus) && (res != 0)))
				{
					sprintf(str, SC(210, "%s: Nr pins (%i) pin bus(ses) and label (%s) are not equal\r\n"), FileName,
					        NrPinsPinBus, NetLabelText);
					AddMessage(str);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
					TotalError = 1;
				}
			}

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
			if (NrObjectPins > 0)
				(*NetInfos)[cnt].Info |= 0x00000100;

			if (NrObjectPins > 1)
				(*NetInfos)[cnt].Info |= 0x00000200;

			if (NrObjectPinBusses > 0)
				(*NetInfos)[cnt].Info |= 0x00000400;

			if (NrObjectPinBusses > 1)
				(*NetInfos)[cnt].Info |= 0x00000800;

			if (NrObjectGlobalConnections > 0)
				(*NetInfos)[cnt].Info |= 0x00001000;

			if (NrObjectBusConnections1 > 0)
				(*NetInfos)[cnt].Info |= 0x00002000;

			if (NrObjectBusConnections2 > 0)
				(*NetInfos)[cnt].Info |= 0x00004000;

			if (NrObjectNetLabels > 0)
				(*NetInfos)[cnt].Info |= 0x00008000;

			if (NrObjectWires > 0)
				(*NetInfos)[cnt].Info |= 0x00010000;

			if (NrObjectBusses > 0)
				(*NetInfos)[cnt].Info |= 0x00020000;

			if (NrObjectSheetPins > 0)
				(*NetInfos)[cnt].Info |= 0x00040000;

			if (NrObjectSheetPins > 1)
				(*NetInfos)[cnt].Info |= 0x00080000;

			if (NrObjectOnePinNets > 0)
				(*NetInfos)[cnt].Info |= 0x00400000;

			if (PowerPinConn)
			{
				(*NetInfos)[cnt].Info |= 0x00100000;

				if (NrPowerPins > 1)
					(*NetInfos)[cnt].Info |= 0x00200000;
			}

			cnt2 = 0;
		}

// **********************************************************************************
		if (TotalError)
			return -1;

		return 0;
	}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
