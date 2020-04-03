/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: check.c
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
#include "check.h"
#include "math.h"
#include "calc.h"
#include "calcdef.h"
#include "string.h"
#include "resource.h"
#include "stdio.h"
#include "dialogs.h"
#include "mainloop.h"
#include "insdel.h"
#include "sch.h"
#include "graphics.h"
#include "draw.h"
#include "utf8.h"
#include "owntime.h"


#define WireERROR                  4000
#define BusERROR                   4001
#define BusConnectionERROR         4002
#define ExternalConnectionERROR    4003
#define InstanceERROR              4004
#define NetLabelERROR              4005
#define JunctionERROR              4006
#define OnePinNetERROR             4007


LPSTR MessagePtr2;
int32 DialogMode, ok;

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckWiresAndBusses()
{
	int32 cnt, cnt2;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	WireRecord *Wire;
	BusRecord *Bus;

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;

			for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
			{
				Bus = &((*Busses)[cnt2]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x3 = Bus->X1;
					y3 = Bus->Y1;
					x4 = Bus->X2;
					y4 = Bus->Y2;

					if (TestLinesConnected(x1, y1, x2, y2, x3, y3, x4, y4, 0))
					{
						Bus->Info |= OBJECT_ERROR;
						Wire->Info |= OBJECT_ERROR;
						return 1;
					}
				}
			}
		}
	}

	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckForOverlappingWires()
{
	int32 cnt, cnt2, Found;
	double x1, y1, x2, y2;

	WireRecord *Wire, *Wire2;

	for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
	{
		Wire = &((*Wires)[cnt2]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;
			Found = -1;

			for (cnt = 0; cnt < Design.NrWires; cnt++)
			{
				Wire2 = &((*Wires)[cnt]);

				if ((cnt2 != cnt) && ((Wire2->Info & (OBJECT_NOT_VISIBLE)) == 0))
				{
					if (LinesOverlap(Wire2->X1, Wire2->Y1, Wire2->X2, Wire2->Y2, x1, y1, x2, y2))
					{
						if (((InRange(x1, Wire2->X1)) && (InRange(y1, Wire2->Y1)))
						        || ((InRange(x1, Wire2->X2)) && (InRange(y1, Wire2->Y2))) || ((InRange(x2, Wire2->X1))
						                && (InRange(y2, Wire2->Y1)))
						        || ((InRange(x2, Wire2->X2)) && (InRange(y2, Wire2->Y2))))
						{
						}
						else
						{
							Wire->Info |= OBJECT_ERROR;
							Wire2->Info |= OBJECT_ERROR;
							return 1;
						}
					}
				}
			}
		}
	}

	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckForLoseWires(int32 mode)
{
	int32 cnt, cnt2, cnt3, FoundWire, FoundPin, FoundNetLabel, FoundJunction, TestMode, FoundGlobalConnection,
	      FoundBusConnection, count;
	double x1, y1, x3, y3;
	InstanceRecord *Instance;
	ObjectRecord *Object;
	BusConnectionRecord *BusConnection;
	WireRecord *Wire, *Wire2, *TestWire;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;
	JunctionRecord *Junction;

	if (mode == 0)
		count = 0;
	else
		count = -1;

	for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
	{
		Wire = &((*Wires)[cnt2]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			TestWire = NULL;

			for (TestMode = 0; TestMode < 2; TestMode++)
			{
				if (TestMode == 0)
				{
					x1 = Wire->X1;
					y1 = Wire->Y1;
				}
				else
				{
					x1 = Wire->X2;
					y1 = Wire->Y2;
				}

				FoundWire = -1;
				FoundPin = -1;
				FoundGlobalConnection = -1;
				FoundBusConnection = -1;

				for (cnt = 0; cnt < Design.NrWires; cnt++)
				{
					Wire2 = &((*Wires)[cnt]);

					if ((cnt2 != cnt) && ((Wire2->Info & (OBJECT_NOT_VISIBLE)) == 0))
					{
						if (((InRange(x1, Wire2->X1)) && (InRange(y1, Wire2->Y1)))
						        || ((InRange(x1, Wire2->X2)) && (InRange(y1, Wire2->Y2))))
							FoundWire = cnt;
						else
						{
							if (TestLineConnectedToCircle
							        (Wire2->X1, Wire2->Y1, Wire2->X2, Wire2->Y2, x1, y1, (float) 0.3))
							{
								FoundJunction = -1;

								for (cnt3 = 0; cnt3 < Design.NrJunctions; cnt3++)
								{
									Junction = &((*Junctions)[cnt3]);

									if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
									{
										if ((InRange(x1, Junction->X)) && (InRange(y1, Junction->Y)))
										{
											FoundJunction = cnt3;
											break;
										}
									}
								}

								if (FoundJunction != -1)
									FoundWire = cnt;
							}
						}
					}
				}


				for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
				{
					BusConnection = &((*BusConnections)[cnt]);

					if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						x3 = BusConnection->X;
						y3 = BusConnection->Y;

						if ((InRange(x1, x3)) && (InRange(y1, y3)))
							FoundBusConnection = cnt;
					}
				}

				for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
				{
					GlobalConnection = &((*GlobalConnections)[cnt]);

					if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						x3 = GlobalConnection->X;
						y3 = GlobalConnection->Y;

						if ((InRange(x1, x3)) && (InRange(y1, y3)))
							FoundGlobalConnection = cnt;
					}
				}

				for (cnt3 = 0; cnt3 < Design.NrInstances; cnt3++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt3]);

					if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
#ifdef _DEBUG

						if (stricmp(Instance->Reference, "t10") == 0)
						{
							ok = 1;

							if ((InRange(x1, 116.0)) && (InRange(y1, 75.0)))
								ok = 1;
						}

#endif

						if ((x1 > Instance->BoardPosMinX - 0.5) && (x1 < Instance->BoardPosMaxX + 0.5)
						        && (y1 > Instance->BoardPosMinY - 0.5) && (y1 < Instance->BoardPosMaxY + 0.5))
						{
							NrObjects = 0;
							InstanceToObject(Instance, 0.0, 0.0, 1);

							for (cnt = 0; cnt < NrObjects; cnt++)
							{
								Object = &((*Objects)[cnt]);

								if ((InRange(x1, Object->x1)) && (InRange(y1, Object->y1)))
									FoundPin = cnt3;
							}
						}
					}
				}

				if ((FoundWire == -1) && (FoundPin == -1) && (FoundGlobalConnection == -1)
				        && (FoundBusConnection == -1))
					TestWire = Wire;
			}

			if (TestWire)
			{
				FoundNetLabel = -1;

				for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
				{
					NetLabel = &((*NetLabels)[cnt]);

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						x1 = NetLabel->ConnectX;
						y1 = NetLabel->ConnectY;

						if (((InRange(TestWire->X1, x1)) && (InRange(TestWire->Y1, y1)))
						        || ((InRange(TestWire->X2, x1)) && (InRange(TestWire->Y2, y1))))
							FoundNetLabel = cnt;
					}
				}

				if (FoundNetLabel == -1)
				{
					if (mode == 1)
					{
						Wire->Info |= OBJECT_ERROR;
						return cnt2;
					}
					else
					{
						Wire->Info |= OBJECT_ERROR | OBJECT_SELECTED;
//            Wire->Info|=OBJECT_SELECTED;
					}

					count++;
				}
			}
		}
	}

	return count;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckForOverlappingBusses()
{
	int32 cnt, cnt2, Found;
	double x1, y1, x2, y2;

	BusRecord *Bus, *Bus2;

	for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
	{
		Bus = &((*Busses)[cnt2]);

		if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Bus->X1;
			y1 = Bus->Y1;
			x2 = Bus->X2;
			y2 = Bus->Y2;
			Found = -1;

			for (cnt = 0; cnt < Design.NrBusses; cnt++)
			{
				Bus2 = &((*Busses)[cnt]);

				if ((cnt2 != cnt) && ((Bus2->Info & (OBJECT_NOT_VISIBLE)) == 0))
				{
					if (LinesOverlap(Bus2->X1, Bus2->Y1, Bus2->X2, Bus2->Y2, x1, y1, x2, y2))
					{
						if (((InRange(x1, Bus2->X1)) && (InRange(y1, Bus2->Y1)))
						        || ((InRange(x1, Bus2->X2)) && (InRange(y1, Bus2->Y2))) || ((InRange(x2, Bus2->X1))
						                && (InRange(y2, Bus2->Y1)))
						        || ((InRange(x2, Bus2->X2)) && (InRange(y2, Bus2->Y2))))
						{
						}
						else
						{
							Bus->Info |= OBJECT_ERROR;
							Bus2->Info |= OBJECT_ERROR;
							return 1;
						}
					}
				}
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckBusConnections()
{
	int32 cnt, cnt2, count, count2;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	WireRecord *Wire;
	BusConnectionRecord *BusConnection;

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;
			count2 = 0;
			x1 = BusConnection->X;
			y1 = BusConnection->Y;
			x2 = x1;
			y2 = y1;

			switch ((BusConnection->Alignment >> 14) & 3)
			{
			case 0:
				x2 = x1 - (double) 2.0;
				break;

			case 1:
				y2 = y1 - (double) 2.0;
				break;

			case 2:
				x2 = x1 + (double) 2.0;
				break;

			case 3:
				y2 = y1 + (double) 2.0;
				break;
			}

			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				Wire = &((*Wires)[cnt2]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x3 = Wire->X1;
					y3 = Wire->Y1;
					x4 = Wire->X2;
					y4 = Wire->Y2;

					if (TestLineConnectedToCircle(x3, y3, x4, y4, x2, y2, (double) 0.3))
					{
						count2++;
						Wire->Info |= OBJECT_ERROR;
						return 1;
					}
				}
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckDoubleBusConnections()
{
	int32 cnt, cnt2;

	BusConnectionRecord *BusConnection;
	BusConnectionRecord *BusConnection2;

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			for (cnt2 = cnt + 1; cnt2 < Design.NrBusConnections; cnt2++)
			{
				BusConnection2 = &((*BusConnections)[cnt2]);

				if ((BusConnection2->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if ((InRange(BusConnection->X, BusConnection2->X)) && (InRange(BusConnection->Y, BusConnection2->Y))
					        && (BusConnection->Alignment == BusConnection2->Alignment))
					{
						BusConnection2->Info |= OBJECT_ERROR;
						return 1;
					}
				}
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


int32 CheckUnusedBusConnections()
{
	int32 cnt, cnt2, count, count2;
	double x1, y1, x2, y2;

	WireRecord *Wire;
	BusConnectionRecord *BusConnection;
	BusRecord *Bus;

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;
			count2 = 0;
			x1 = BusConnection->X;
			y1 = BusConnection->Y;
			x2 = x1;
			y2 = y1;

			switch ((BusConnection->Alignment >> 14) & 3)
			{
			case 0:
				x2 = x1 - (double) 2.0;
				break;

			case 1:
				y2 = y1 - (double) 2.0;
				break;

			case 2:
				x2 = x1 + (double) 2.0;
				break;

			case 3:
				y2 = y1 + (double) 2.0;
				break;
			}

			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				Wire = &((*Wires)[cnt2]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, Wire->X1)) && (InRange(y1, Wire->Y1)))
					        || ((InRange(x1, Wire->X2)) && (InRange(y1, Wire->Y2))))
						count2++;
				}
			}

			for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
			{
				Bus = &((*Busses)[cnt2]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, Bus->X1)) && (InRange(y1, Bus->Y1)))
					        || ((InRange(x1, Bus->X2)) && (InRange(y1, Bus->Y2))))
						count2++;
				}
			}

			for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
			{
				Bus = &((*Busses)[cnt2]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (TestLineConnectedToCircle(Bus->X1, Bus->Y1, Bus->X2, Bus->Y2, x2, y2, (double) 0.3))
						count2++;
				}
			}

			if (count2 < 2)
			{
				BusConnection->Info |= OBJECT_ERROR;
				return 1;
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckUnusedJunctions()
{
	int32 cnt, cnt2, cnt3, count, count2, count3;
	double x1, y1;

	WireRecord *Wire;
	JunctionRecord *Junction;
	BusRecord *Bus;
	InstanceRecord *Instance;
	ObjectRecord *Object;

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;
			count2 = 0;
			count3 = 0;
			x1 = Junction->X;
			y1 = Junction->Y;

			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				Wire = &((*Wires)[cnt2]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, Wire->X1)) && (InRange(y1, Wire->Y1)))
					        || ((InRange(x1, Wire->X2)) && (InRange(y1, Wire->Y2))))
					{
//             &&
//             (TestLineConnectedToCircle(Wire->X1,Wire->Y1,Wire->X2,Wire->Y2,x1,y1,(double)0.3))) {
						count++;
					}
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrInstances; cnt3++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt3]);

				if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if ((x1 > Instance->BoardPosMinX - 0.1) && (x1 < Instance->BoardPosMaxX + 0.1)
					        && (y1 > Instance->BoardPosMinY - 0.1) && (y1 < Instance->BoardPosMaxY + 0.1))
					{
						NrObjects = 0;
						InstanceToObject(Instance, 0.0, 0.0, 1);

						for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
						{
							Object = &((*Objects)[cnt2]);

							if ((Object->ObjectType == SYMBOL_PIN) || (Object->ObjectType == SYMBOL_PINBUS))
							{
								if ((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
									count3++;
							}
						}
					}
				}
			}

			for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
			{
				Bus = &((*Busses)[cnt2]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (TestLineConnectedToCircle(Bus->X1, Bus->Y1, Bus->X2, Bus->Y2, x1, y1, (double) 0.3))
						count2++;
				}
			}

			if ((count < 3) && (count3 == 0))
			{
				Junction->Info |= OBJECT_ERROR;
				return 1;
			}

			if (count2 > 0)
			{
				Junction->Info |= OBJECT_ERROR;
				return 1;
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckUnusedOnePinNets()
{
	int32 cnt, cnt2, cnt3, count;
	InstanceRecord *Instance;
	double x1, y1;
	ObjectRecord *Object;
	OnePinNetRecord *OnePinNet;

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;
			x1 = OnePinNet->X;
			y1 = OnePinNet->Y;

			for (cnt3 = 0; cnt3 < Design.NrInstances; cnt3++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt3]);
#ifdef _DEBUG

				if (stricmp(Instance->Reference, "r93") == 0)
				{
					ok = 1;

					if ((InRange(x1, 70.0)) && (InRange(y1, 37.0)))
						ok = 1;
				}

#endif

				if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if ((x1 > Instance->BoardPosMinX - 0.5) && (x1 < Instance->BoardPosMaxX + 0.5)
					        && (y1 > Instance->BoardPosMinY - 0.5) && (y1 < Instance->BoardPosMaxY + 0.5))
					{
						NrObjects = 0;
						InstanceToObject(Instance, 0.0, 0.0, 1);

						for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
						{
							Object = &((*Objects)[cnt2]);

							if ((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
								count = 1;
						}
					}
				}
			}

			if (count == 0)
			{
				OnePinNet->Info |= OBJECT_ERROR;
				return 1;
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckJunctions(double *x1, double *y1)
{
	int32 Error = 0;

	if (RebuildJunctions(1, x1, y1))
		Error = 1;

//  RePaint();
	return Error;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckBusJunctions()
{
	int32 cnt, cnt2, count, count2;
	double x1, y1;

	JunctionRecord *Junction;
	BusRecord *Bus;

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;
			count2 = 0;
			x1 = Junction->X;
			y1 = Junction->Y;

			for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
			{
				Bus = &((*Busses)[cnt2]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (TestLineConnectedToCircle(Bus->X1, Bus->Y1, Bus->X2, Bus->Y2, x1, y1, (double) 0.3))
						count2++;
				}
			}

			if (count2 > 0)
			{
				Junction->Info |= OBJECT_ERROR;
				return 1;
			}
		}
	}

	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckGlobalConnections()
{
	int32 cnt, cnt3, count, count2;
	double x1, y1, x3, y3, x4, y4;

	GlobalConnectionRecord *GlobalConnection;
	BusConnectionRecord *BusConnection;

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;
			count2 = 0;
			x1 = GlobalConnection->X;
			y1 = GlobalConnection->Y;

			for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
			{
				BusConnection = &((*BusConnections)[cnt3]);

				if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
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

					if (((InRange(x1, x3)) && (InRange(y1, y3))) || ((InRange(x1, x4)) && (InRange(y1, y4))))
					{
						count++;
						BusConnection->Info |= OBJECT_ERROR;
						return 1;
					}
				}
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckUniqueGlobalConnections()
{
	int32 cnt, cnt2, count;

	GlobalConnectionRecord *GlobalConnection, *GlobalConnection2;

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;

			for (cnt2 = 0; cnt2 < Design.NrGlobalConnections; cnt2++)
			{
				if (cnt2 != cnt)
				{
					GlobalConnection2 = &((*GlobalConnections)[cnt2]);

					if ((GlobalConnection2->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if (stricmpUTF8(GlobalConnection->Text, GlobalConnection2->Text) == 0)
						{
							count++;
							GlobalConnection->Info |= OBJECT_ERROR;
							GlobalConnection2->Info |= OBJECT_ERROR;
							return 1;
						}
					}
				}
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckUnusedGlobalConnections()
{
	int32 cnt, cnt2, count;
	double x1, y1;

	WireRecord *Wire;
	GlobalConnectionRecord *GlobalConnection;
	BusRecord *Bus;

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);

		if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = 0;
			x1 = GlobalConnection->X;
			y1 = GlobalConnection->Y;

			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				Wire = &((*Wires)[cnt2]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, Wire->X1)) && (InRange(y1, Wire->Y1)))
					        || ((InRange(x1, Wire->X2)) && (InRange(y1, Wire->Y2))))
						count++;
				}
			}

			for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
			{
				Bus = &((*Busses)[cnt2]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, Bus->X1)) && (InRange(y1, Bus->Y1)))
					        || ((InRange(x1, Bus->X2)) && (InRange(y1, Bus->Y2))))
						count++;
				}
			}

			if (count == 0)
			{
				GlobalConnection->Info |= OBJECT_ERROR;
				return 1;
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckNetLabels()
{
	int32 cnt, cnt3;
	double x1, y1;

	BusRecord *Bus;
	WireRecord *Wire;
	NetLabelRecord *NetLabel;
	int32 Found;


	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = NetLabel->ConnectX;
			y1 = NetLabel->ConnectY;
			Found = 0;

			for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
			{
				Bus = &((*Busses)[cnt3]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, Bus->X1)) && (InRange(y1, Bus->Y1)))
					        || ((InRange(x1, Bus->X2)) && (InRange(y1, Bus->Y2))))
						Found = 1;
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
			{
				Wire = &((*Wires)[cnt3]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(x1, Wire->X1)) && (InRange(y1, Wire->Y1)))
					        || ((InRange(x1, Wire->X2)) && (InRange(y1, Wire->Y2))))
						Found = 1;
				}
			}

			if (!Found)
			{
				NetLabel->Info |= OBJECT_ERROR;
				return 1;
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckInstances(int32 Mode, double *cx, double *cy)
{
	int32 cnt, cnt2, cnt3, cnt4, NrInstanceObjects;
	double x1, y1, x2, y2, InstanceMinX, InstanceMinY, InstanceMaxX, InstanceMaxY;

	InstanceRecord *Instance, *Instance2;
	ObjectRecord *Object, *Object2, FoundObject;
	ObjectArray *InstanceObjects;
	WireRecord *Wire, *FoundWire;
	BusRecord *Bus, *FoundBus;
	BusConnectionRecord *BusConnection, *FoundBusConnection;
	GlobalConnectionRecord *GlobalConnection, *FoundGlobalConnection;
	OnePinNetRecord *OnePinNet, *FoundOnePinNet;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		NrObjects = 0;
		InstanceToObject(Instance, 0.0, 0.0, 1);
		NrInstanceObjects = NrObjects;
		AllocateSpecialMem(MEM_OBJECTS2, NrObjects * sizeof(ObjectRecord), (void *) &InstanceObjects);
		memcpy(InstanceObjects, Objects, NrObjects * sizeof(ObjectRecord));

		if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			InstanceMinX = Instance->BoardPosMinX - 0.5;
			InstanceMaxX = Instance->BoardPosMaxX + 0.5;
			InstanceMinY = Instance->BoardPosMinY - 0.5;
			InstanceMaxY = Instance->BoardPosMaxY + 0.5;
// ***************************************************************************************

			for (cnt2 = 0; cnt2 < NrInstanceObjects; cnt2++)
			{
				Object = &((*InstanceObjects)[cnt2]);
				*cx = Object->x1;
				*cy = Object->y1;
				FoundWire = NULL;
				FoundBus = NULL;
				FoundGlobalConnection = NULL;
				FoundBusConnection = NULL;
				FoundOnePinNet = NULL;
				memset(&FoundObject, 0, sizeof(ObjectRecord));

				for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
				{
					Wire = &((*Wires)[cnt3]);

					if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if (((InRange(Object->x1, Wire->X1)) && (InRange(Object->y1, Wire->Y1)))
						        || ((InRange(Object->x1, Wire->X2)) && (InRange(Object->y1, Wire->Y2))))
							FoundWire = Wire;
					}
				}

				for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
				{
					Bus = &((*Busses)[cnt3]);

					if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if (((InRange(Object->x1, Bus->X1)) && (InRange(Object->y1, Bus->Y1)))
						        || ((InRange(Object->x1, Bus->X2)) && (InRange(Object->y1, Bus->Y2))))
							FoundBus = Bus;
					}
				}

				for (cnt3 = 0; cnt3 < Design.NrGlobalConnections; cnt3++)
				{
					GlobalConnection = &((*GlobalConnections)[cnt3]);

					if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						x1 = GlobalConnection->X;
						y1 = GlobalConnection->Y;

						if ((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
							FoundGlobalConnection = GlobalConnection;
					}
				}

				for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
				{
					BusConnection = &((*BusConnections)[cnt3]);

					if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						x1 = BusConnection->X;
						y1 = BusConnection->Y;
						x2 = x1;
						y2 = y1;

						switch ((BusConnection->Alignment >> 14) & 3)
						{
						case 0:
							x2 = x1 - 2.0;
							break;

						case 1:
							y2 = y1 - 2.0;
							break;

						case 2:
							x2 = x1 + 2.0;
							break;

						case 3:
							y2 = y1 + 2.0;
							break;
						}

						if (((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
						        || ((InRange(Object->x1, x2)) && (InRange(Object->y1, y2))))
							FoundBusConnection = BusConnection;
					}
				}

				for (cnt3 = 0; cnt3 < Design.NrOnePinNets; cnt3++)
				{
					OnePinNet = &((*OnePinNets)[cnt3]);

					if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						x1 = OnePinNet->X;
						y1 = OnePinNet->Y;

						if ((InRange(Object->x1, x1)) && (InRange(Object->y1, y1)))
							FoundOnePinNet = OnePinNet;
					}
				}

				for (cnt3 = 0; cnt3 < Design.NrInstances; cnt3++)
				{
					if (cnt3 != cnt)
					{
						Instance2 = (InstanceRecord *) & ((*Instances)[cnt3]);

						if (((Instance2->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Instance2->BoardPosMinX < InstanceMaxX)
						        && (Instance2->BoardPosMinY < InstanceMaxY) && (Instance2->BoardPosMaxX > InstanceMinX)
						        && (Instance2->BoardPosMaxY > InstanceMinY))
						{
							NrObjects = 0;
							InstanceToObject(Instance2, 0.0, 0.0, 1);

							for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
							{
								Object2 = &((*Objects)[cnt4]);

								if ((InRange(Object->x1, Object2->x1)) && (InRange(Object->y1, Object2->y1)))
									memcpy(&FoundObject, Object2, sizeof(ObjectRecord));
							}
						}
					}
				}

				if (FoundWire)
				{
					if (Object->ObjectType == SYMBOL_PINBUS)
					{
						FoundWire->Info |= OBJECT_ERROR;
						Instance->Info |= OBJECT_ERROR;
						Instance->Error = 1;
						return 1;
					}
				}

				if (FoundBus)
				{
					if (Object->ObjectType == SYMBOL_PIN)
					{
						if ((Instance->Info & SHEET_SYMBOL) == 0)
						{
							FoundBus->Info |= OBJECT_ERROR;
							Instance->Info |= OBJECT_ERROR;
							Instance->Error = 3;
							return 1;
						}
					}
				}

				if (FoundGlobalConnection)
				{
					FoundGlobalConnection->Info |= OBJECT_ERROR;
					Instance->Info |= OBJECT_ERROR;
					Instance->Error = 6;
					return 1;
				}

				if (FoundBusConnection)
				{
					FoundBusConnection->Info |= OBJECT_ERROR;
					Instance->Info |= OBJECT_ERROR;
					Instance->Error = 5;
					return 1;
				}

				if ((!FoundWire) && (!FoundBus) && (FoundObject.ObjectType == 0))
				{
					if ((ActiveOnePinNetError) && (!DisableOnePinNetCheck))
					{
						if (!FoundOnePinNet)
						{
							Instance->Info |= OBJECT_ERROR;
							Instance->Error = 7;
							return 2;
						}
					}
				}
			}
		}
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void ClearErrors(int32 mode)
{
	int32 cnt, cnt3, ClearMask;

	InstanceRecord *Instance;
	WireRecord *Wire;
	BusRecord *Bus;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	NetLabelRecord *NetLabel;

	if (mode == 0)
		ClearMask = OBJECT_ERROR | OBJECT_DONE;
	else
		ClearMask = OBJECT_ERROR | OBJECT_DONE | OBJECT_SELECTED | 0x000f;

	for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
	{
		Wire = &((*Wires)[cnt3]);

		if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
			Wire->Info &= ~(ClearMask);
	}

	for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
	{
		Bus = &((*Busses)[cnt3]);

		if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
			Bus->Info &= ~(ClearMask);
	}

	for (cnt3 = 0; cnt3 < Design.NrJunctions; cnt3++)
	{
		Junction = &((*Junctions)[cnt3]);

		if ((Junction->Info & (OBJECT_NOT_VISIBLE)) == 0)
			Junction->Info &= ~(ClearMask);
	}

	for (cnt3 = 0; cnt3 < Design.NrOnePinNets; cnt3++)
	{
		OnePinNet = &((*OnePinNets)[cnt3]);

		if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE)) == 0)
			OnePinNet->Info &= ~(ClearMask);
	}

	for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
	{
		BusConnection = &((*BusConnections)[cnt3]);

		if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			BusConnection->Info &= ~(ClearMask);
	}

	for (cnt3 = 0; cnt3 < Design.NrGlobalConnections; cnt3++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt3]);

		if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			GlobalConnection->Info &= ~(ClearMask);
	}

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
			Instance->Info &= ~(ClearMask);
	}

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
			NetLabel->Info &= ~(ClearMask);
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 ChangeErrorObjectToSelected(int32 mode)
{
	int32 cnt3, ClearMask;

	InstanceRecord *Instance;
	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;

	if (mode == 0)
		ClearMask = OBJECT_ERROR | OBJECT_DONE;
	else
		ClearMask = OBJECT_ERROR | OBJECT_DONE | OBJECT_SELECTED | 0x000f;

	switch (mode)
	{
	case WireERROR:
		for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
		{
			Wire = &((*Wires)[cnt3]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				Wire->Info |= OBJECT_SELECTED | 3;
				return cnt3;
			}
		}

		break;

	case BusERROR:
		for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
		{
			Bus = &((*Busses)[cnt3]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				Bus->Info |= OBJECT_SELECTED | 3;
				return cnt3;
			}
		}

		break;

	case JunctionERROR:
		for (cnt3 = 0; cnt3 < Design.NrJunctions; cnt3++)
		{
			Junction = &((*Junctions)[cnt3]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				Junction->Info |= OBJECT_SELECTED | 3;
				return cnt3;
			}
		}

		break;

	case OnePinNetERROR:
		for (cnt3 = 0; cnt3 < Design.NrOnePinNets; cnt3++)
		{
			OnePinNet = &((*OnePinNets)[cnt3]);

			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				OnePinNet->Info |= OBJECT_SELECTED | 3;
				return cnt3;
			}
		}

		break;

	case BusConnectionERROR:
		for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
		{
			BusConnection = &((*BusConnections)[cnt3]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				BusConnection->Info |= OBJECT_SELECTED;
				return cnt3;
			}
		}

		break;

	case ExternalConnectionERROR:
		for (cnt3 = 0; cnt3 < Design.NrGlobalConnections; cnt3++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt3]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				GlobalConnection->Info |= OBJECT_SELECTED | 3;
				return cnt3;
			}
		}

		break;

	case InstanceERROR:
		for (cnt3 = 0; cnt3 < Design.NrInstances; cnt3++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt3]);

			if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				Instance->Info |= OBJECT_SELECTED | 7;
				return cnt3;
			}
		}

		break;

	case NetLabelERROR:
		for (cnt3 = 0; cnt3 < Design.NrNetLabels; cnt3++)
		{
			NetLabel = &((*NetLabels)[cnt3]);

			if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_ERROR)) == OBJECT_ERROR)
			{
				NetLabel->Info |= OBJECT_SELECTED | 3;
				return cnt3;
			}
		}

		break;
	}

	return -1;
}

//**********************************************************************************************************************
//********************** IDD_DIALOG_MESSAGE2 ***************************************************************************
//**********************************************************************************************************************

int32 CALLBACK CheckMessageDialog2(HWND Dialog, uint16 Message, uint16 WParam, int32 LParam)
{
	int32 about;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));

		if (DialogMode == 0)
			SetWindowTextUTF8(Dialog, SC(4, "Error/message"));
		else
			SetWindowTextUTF8(Dialog, SC(258, "Warning/message"));

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, EM_REPLACESEL, 0, (LPARAM) MessagePtr2);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDOK:
			EndDialog(Dialog, IDOK);
			return about;

		case IDSKIP:
			EndDialog(Dialog, IDSKIP);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, IDCANCEL);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 CheckMessageDialog(LPSTR Message, int32 mode)
{
	int res, first;

	MessagePtr2 = Message;
	DialogMode = mode;
	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_MESSAGE2), SCHWindow, (DLGPROC) CheckMessageDialog2);

	first = 1;
	return res;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 Check(int32 mode)
{
	int32 res, cnt, result, ErrorMode;
	char str[200];
	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;
	double x1, y1;

#ifdef _DEBUG
	SetTimer0();
#endif
	ErrorMode = 0;

	if (mode & 8)
		ErrorMode = 1;

	res = IDSKIP;
	ClearErrors(1);

	if (CheckWiresAndBusses())
	{
		cnt = ChangeErrorObjectToSelected(WireERROR);

		if (cnt != -1)
		{
			Wire = &((*Wires)[cnt]);
			CenterScreen(Wire->X1, Wire->Y1, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(5, "Wire(s) directly connected to bus(ses)"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckWiresAndBusses %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckForOverlappingWires())
	{
		cnt = ChangeErrorObjectToSelected(WireERROR);

		if (cnt != -1)
		{
			Wire = &((*Wires)[cnt]);
			CenterScreen(Wire->X1, Wire->Y1, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(434, "Two or more wires overlap"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckForOverlappingWires %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckForOverlappingBusses())
	{
		cnt = ChangeErrorObjectToSelected(BusERROR);

		if (cnt != -1)
		{
			Bus = &((*Busses)[cnt]);
			CenterScreen(Bus->X1, Bus->Y1, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(435, "Two or more busses overlap"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckForOverlappingBusses %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckBusConnections())
	{
		cnt = ChangeErrorObjectToSelected(BusConnectionERROR);

		if (cnt != -1)
		{
			BusConnection = &((*BusConnections)[cnt]);
			CenterScreen(BusConnection->X, BusConnection->Y, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(6, "Wire/Busconnection"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckBusConnections %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckDoubleBusConnections())
	{
		cnt = ChangeErrorObjectToSelected(BusConnectionERROR);

		if (cnt != -1)
		{
			BusConnection = &((*BusConnections)[cnt]);
			CenterScreen(BusConnection->X, BusConnection->Y, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(7, "Double busconnection"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckDoubleBusConnections %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckBusJunctions())
	{
		cnt = ChangeErrorObjectToSelected(JunctionERROR);

		if (cnt != -1)
		{
			Junction = &((*Junctions)[cnt]);
			CenterScreen(Junction->X, Junction->Y, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(8, "Junction(s) placed on busses"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckBusJunctions %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckJunctions(&x1, &y1))
	{
		CenterScreen(x1, y1, 0);
		CheckInputMessages(200);
		DrawCircleWhite(x1, y1, 20, 1);
		CheckMessageDialog(SC(436, "No junction placed"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckJunctions %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckUnusedOnePinNets())
	{
		cnt = ChangeErrorObjectToSelected(OnePinNetERROR);

		if (cnt != -1)
		{
			OnePinNet = &((*OnePinNets)[cnt]);
			CenterScreen(OnePinNet->X, OnePinNet->Y, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(472, "Unused one pin net marking(s)"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckUnusedOnePinNets %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckUnusedGlobalConnections())
	{
		cnt = ChangeErrorObjectToSelected(ExternalConnectionERROR);

		if (cnt != -1)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);
			CenterScreen(GlobalConnection->X, GlobalConnection->Y, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(12, "Unused external connection(s)"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckUnusedGlobalConnections %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckUnusedBusConnections())
	{
		cnt = ChangeErrorObjectToSelected(BusConnectionERROR);

		if (cnt != -1)
		{
			BusConnection = &((*BusConnections)[cnt]);
			CenterScreen(BusConnection->X, BusConnection->Y, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(13, "Unused busconnection(s)"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckUnusedBusConnections %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckGlobalConnections())
	{
		cnt = ChangeErrorObjectToSelected(BusConnectionERROR);

		if (cnt != -1)
		{
			BusConnection = &((*BusConnections)[cnt]);
			CenterScreen(BusConnection->X, BusConnection->Y, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(10, "Busconnection directly connected to external connection"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckGlobalConnections %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckNetLabels())
	{
		cnt = ChangeErrorObjectToSelected(NetLabelERROR);

		if (cnt != -1)
		{
			NetLabel = &((*NetLabels)[cnt]);
			CenterScreen(NetLabel->ConnectX + NetLabel->TextX, NetLabel->ConnectY + NetLabel->TextY, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(11, "Netlabel(s) unconnected"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckNetLabels %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckUniqueGlobalConnections())
	{
		cnt = ChangeErrorObjectToSelected(ExternalConnectionERROR);

		if (cnt != -1)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);
			CenterScreen(GlobalConnection->X, GlobalConnection->Y, 0);
			CheckInputMessages(200);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(437, "Every external connection must be unique"), ErrorMode);
		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckUniqueGlobalConnections %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if ((result = CheckInstances(0, &x1, &y1)))
	{
		cnt = ChangeErrorObjectToSelected(InstanceERROR);

		if ((cnt != -1) && (SetBoardPosInstance((int16) cnt) == 1))
		{
			/*
			      Instance=(InstanceRecord *)&((*Instances)[cnt]);
			      CenterScreen((Instance->BoardPosMinX+Instance->BoardPosMaxX)/2,
			                   (Instance->BoardPosMinY+Instance->BoardPosMaxY)/2,0);
			*/
			CenterScreen(x1, y1, 0);
			CheckInputMessages(200);
			DrawCircleWhite(x1, y1, 20, 1);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		switch (result)
		{
		case 1:
			sprintf(str, SC(476, "Component(s) not connected properly ( %.1f , %.1f )"), x1, y1);
			CheckMessageDialog(str, ErrorMode);
			break;

		case 2:
			sprintf(str, SC(477, "Component with unconnected pin(s) ( %.1f , %.1f )"), x1, y1);
			CheckMessageDialog(str, ErrorMode);
			break;
		}

		return 0;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckInstances %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if (CheckUnusedJunctions())
	{
		cnt = ChangeErrorObjectToSelected(JunctionERROR);

		if (cnt != -1)
		{
			Junction = &((*Junctions)[cnt]);
			CenterScreen(Junction->X, Junction->Y, 0);
//      CheckInputMessages(200);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

		CheckMessageDialog(SC(9, "Unused junction(s)"), 1);
		return 2;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckUnusedJunctions %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	if ((res = CheckForLoseWires(0)))
	{
//    Wire=&((*Wires)[res]);
//    CenterScreen(Wire->X1,Wire->Y1,0);
		cnt = ChangeErrorObjectToSelected(WireERROR);

		if (cnt != -1)
		{
			Wire = &((*Wires)[cnt]);
			CenterScreen(Wire->X1, Wire->Y1, 0);
		}
		else
			PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);

//    RePaint();
		sprintf(str, SC(241, "There are %d wire(s) with one end of the wire unconnected"), res);
		CheckMessageDialog(str, 1);
		return 2;
	}

#ifdef _DEBUG
	res = GetDifferenceTimer0inMilliSeconds();
	sprintf(str, "Time CheckForLoseWires %d msec\r\n", res);
	OutputDebugString(str);
	SetTimer0();
#endif

	return 1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckPins(int32 mode)
{
	int32 cnt, cnt2, count, lengte, NrLines, NrErrors, LinePos[16], Errors[MAX_LENGTH_STRING];
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;
	char PinStrings[1000][12];
	char str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	char PinBusStrings[16][64], Message[4096];


	count = 0;
	memset(&PinStrings, 0, sizeof(PinStrings));

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if (count < 1000)
				memmove(&PinStrings[count], Pin->Name, sizeof(Pin->Name) - 1);

			count++;
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if ((PowerPin->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			memset(&str2, 0, MAX_LENGTH_STRING);
			strcpy(str2, PowerPin->Text);
			str2[299] = 0;

			while (str2[0] != 0)
			{
				memset(&str3, 0, MAX_LENGTH_STRING);
				GetStringKomma(str2, str3);

				if (count < 1000)
					memmove(&PinStrings[count], &str3, 11);

				count++;
			}
		}
	}

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
		{
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

			for (cnt2 = 0; cnt2 < NrLines; cnt2++)
			{
				memset(&str2, 0, MAX_LENGTH_STRING);
				strcpy(str2, PinBusStrings[cnt2]);
				str2[299] = 0;

				while (str2[0] != 0)
				{
					memset(&str3, 0, MAX_LENGTH_STRING);
					GetStringKomma(str2, str3);

					if (count < 1000)
						memmove(&PinStrings[count], &str3, 11);

					count++;
				}
			}

		}
	}

	count = min(1000, count);
	NrErrors = 0;

	for (cnt = 0; cnt < count - 1; cnt++)
	{
		memset(&str2, 0, MAX_LENGTH_STRING);
		strcpy(str2, PinStrings[cnt]);

		for (cnt2 = cnt + 1; cnt2 < count; cnt2++)
		{
			if (stricmpUTF8(str2, PinStrings[cnt2]) == 0)
			{
				if (NrErrors < 200)
					Errors[NrErrors] = cnt;

				NrErrors++;
			}
		}
	}

	NrErrors = min(200, NrErrors);

	Message[0] = 0;

	if ((mode & 1) == 0)
	{
		if (NrErrors > 0)
		{
			strcat(Message, SC(14, "Double pin names"));
			strcat(Message, "\r\n");
			strcat(Message, "\r\n");

			for (cnt = 0; cnt < NrErrors; cnt++)
			{
				if (strlen(Message) < 4096 - 20)
				{
					strcat(Message, PinStrings[Errors[cnt]]);
					strcat(Message, "\r\n");
				}

			}

			if (mode == 0)
			{
				strcat(Message, "\r\n");
				strcat(Message, SC(15, "Symbol will not be saved"));
			}

			MessageDialog(Message);
			return 0;
		}
	}
	else
	{
		strcat(Message, SC(16, "Symbol info :\r\n"));
		strcat(Message, "\r\n");
		sprintf(str2, SC(17, "Nr pins : %i\r\n"), count);
		strcat(Message, str2);

		if (NrErrors > 0)
		{
			strcat(Message, "\r\n");
			strcat(Message, SC(14, "Double pin names"));
			strcat(Message, "\r\n");
			strcat(Message, "\r\n");

			for (cnt = 0; cnt < NrErrors; cnt++)
			{
				if (strlen(Message) < 4096 - 20)
				{
					strcat(Message, PinStrings[Errors[cnt]]);
					strcat(Message, "\r\n");
				}

			}
		}

		MessageDialog(Message);
		return 0;
	}

	return 1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
