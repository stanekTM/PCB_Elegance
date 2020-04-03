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
#include "stdio.h"
#include "check.h"
#include "math.h"
#include "calc.h"
#include "string.h"
#include "resource.h"
#include "files.h"
#include "utf8.h"

extern UINT ClipID3;
int32 ok;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckWiresAndBusses(int32 SheetMode)
{
	int32 cnt, cnt2;
	double x1, y1, x2, y2, x3, y3, x4, y4;
	int32 Error = 0;

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
						Error = 1;
					}
				}
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckBusConnections(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt2, count, count2;
	double x1, y1, x2, y2, x3, y3, x4, y4;
	int32 Error = 0;

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

			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				Wire = &((*Wires)[cnt2]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x3 = Wire->X1;
					y3 = Wire->Y1;
					x4 = Wire->X2;
					y4 = Wire->Y2;

					if (TestLineConnectedToCircle(x3, y3, x4, y4, x2, y2, 0.3))
					{
						count2++;
						Wire->Info |= OBJECT_ERROR;
						*cx = Wire->X1;
						*cy = Wire->Y1;
						return 1;
					}
				}
			}

			if (count2 > 0)
			{
				BusConnection->Info |= OBJECT_ERROR;
				*cx = BusConnection->X;
				*cy = BusConnection->Y;
				return 1;
			}
		}
	}

	return Error;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckForLoseWires(int32 mode, LPSTR Filename)
{
	int32 cnt, cnt2, cnt3, FoundWire, FoundPin, FoundNetLabel, FoundJunction, TestMode, FoundGlobalConnection,
	      FoundBusConnection;
	double x1, y1, x3, y3;
	InstanceRecord *Instance;
	ObjectRecord *Object;
	BusConnectionRecord *BusConnection;
	WireRecord *Wire, *Wire2, *TestWire;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;
	JunctionRecord *Junction;
	char str[200];

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

								if ((FoundJunction != -1) || (stricmp(Design.Identification, SheetCode1) == 0))
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
							InstancePinsToObject(Instance, 0);

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
					sprintf(str,
					        SC(238, "Warning: One end of the wire is unconnected in sheet %s (%.1f,%.1f - %.1f,%.1f)\r\n"),
						Filename, Wire->X1, Wire->Y1, Wire->X2, Wire->Y2);
					AddMessage(str);
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

int32 CheckForOverlappingWires(int32 SheetMode)
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

int32 CheckForOverlappingBusses(int32 SheetMode)
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckUnusedBusConnections(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt2, count, count2;
	double x1, y1, x2, y2;
	int32 Error = 0;

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
					if (TestLineConnectedToCircle(Bus->X1, Bus->Y1, Bus->X2, Bus->Y2, x2, y2, 0.3))
						count2++;
				}
			}

			if (count2 < 2)
			{
				BusConnection->Info |= OBJECT_ERROR;
				*cx = BusConnection->X;
				*cy = BusConnection->Y;
				return 1;
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckUnusedJunctions(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt2, count, count2;
	double x1, y1;
	int32 Error = 0;

	WireRecord *Wire;
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

			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				Wire = &((*Wires)[cnt2]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (TestLineConnectedToCircle(Wire->X1, Wire->Y1, Wire->X2, Wire->Y2, x1, y1, 0.3))
						count++;
				}
			}

			for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
			{
				Bus = &((*Busses)[cnt2]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (TestLineConnectedToCircle(Bus->X1, Bus->Y1, Bus->X2, Bus->Y2, x1, y1, 0.3))
						count2++;
				}
			}

			if ((count == 0) || (count2 > 0))
			{
				Junction->Info |= OBJECT_ERROR;
				*cx = Junction->X;
				*cy = Junction->Y;
				return 1;
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckUnusedOnePinNets(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt2, cnt3, count;
	double x1, y1;
	int32 Error = 0;
	InstanceRecord *Instance;
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

				if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if ((Instance->Info & (SHEET_SYMBOL)) == 0)
					{
						NrObjects = 0;
						InstancePinsToObject(Instance, 0);

						for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
						{
							Object = &((*Objects)[cnt2]);

							if ((InRange(x1, Object->x1)) && (InRange(y1, Object->y1)))
								count = 1;
						}
					}
				}
			}

			if (count == 0)
			{
				OnePinNet->Info |= OBJECT_ERROR;
				Error = 1;
				*cx = x1, *cy = y1;
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckGlobalConnections(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt3, count, count2;
	double x1, y1, x3, y3, x4, y4;
	int32 Error = 0;

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
						*cx = BusConnection->X;
						*cy = BusConnection->Y;
						return 1;
					}
				}
			}

			if (count > 0)
			{
				GlobalConnection->Info |= OBJECT_ERROR;
				*cx = GlobalConnection->X;
				*cy = GlobalConnection->Y;
				return 1;
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckUnusedGlobalConnections(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt2, count;
	double x1, y1;
	int32 Error = 0;

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
				*cx = GlobalConnection->X;
				*cy = GlobalConnection->Y;
				return 1;
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckUniqueGlobalConnections(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt2, count;
	int32 Error = 0;

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
							*cx = GlobalConnection->X;
							*cy = GlobalConnection->Y;
							return 1;
						}
					}
				}
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckNetLabels(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt3;
	double x1, y1;

	BusRecord *Bus;
	WireRecord *Wire;
	NetLabelRecord *NetLabel;
	int32 Error = 0;
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
				*cx = NetLabel->TextX;
				*cy = NetLabel->TextY;
				return 1;
			}
		}
	}

	return Error;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckInstances(int32 SheetMode, double *cx, double *cy)
{
	int32 cnt, cnt2, cnt3, cnt4, NrInstanceObjects, MemPos, SymbolNr;
	double x1, y1, x2, y2;

	InstanceRecord *Instance, *Instance2;
	ObjectRecord *Object, *Object2, FoundObject;
	ObjectArray *InstanceObjects;
	WireRecord *Wire, *FoundWire;
	BusRecord *Bus, *FoundBus;
	OnePinNetRecord *OnePinNet, *FoundOnePinNet;
	BusConnectionRecord *BusConnection, *FoundBusConnection;
	GlobalConnectionRecord *GlobalConnection, *FoundGlobalConnection;
	SymbolRecord *Symbol;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		NrObjects = 0;
		SymbolNr = Instance->AddNr;

		if (SymbolNr == -1)
			continue;

		MemPos = (*SymbolsPos2)[SymbolNr].Pos;
		Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);
		InstancePinsToObject(Instance, 0);
		NrInstanceObjects = NrObjects;
		AllocateSpecialMem(MEM_OBJECTS2, NrObjects * sizeof(ObjectRecord), (void *) &InstanceObjects);
		memcpy(InstanceObjects, Objects, NrObjects * sizeof(ObjectRecord));

		if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			/*
			      InstanceMinX=Instance->BoardPosMinX-0.5;
			      InstanceMaxX=Instance->BoardPosMaxX+0.5;
			      InstanceMinY=Instance->BoardPosMinY-0.5;
			      InstanceMaxY=Instance->BoardPosMaxY+0.5;
			*/
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

						if (((Instance2->Info & (OBJECT_NOT_VISIBLE)) == 0) && (1))
						{
							/*
							               (Instance2->BoardPosMinX<InstanceMaxX)
							               &&
							               (Instance2->BoardPosMinY<InstanceMaxY)
							               &&
							               (Instance2->BoardPosMaxX>InstanceMinX)
							               &&
							               (Instance2->BoardPosMaxY>InstanceMinY)) {
							*/
							NrObjects = 0;
							InstancePinsToObject(Instance2, 0);

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

				if ((Symbol->Info & SHEET_SYMBOL) == 0)
				{
					if ((FoundBus) && (Object->ObjectType == SYMBOL_PIN))
					{
						FoundBus->Info |= OBJECT_ERROR;
						Instance->Info |= OBJECT_ERROR;
						Instance->Error = 3;
						return 1;
					}

					if (FoundGlobalConnection)
					{
						FoundGlobalConnection->Info |= OBJECT_ERROR;
						Instance->Info |= OBJECT_ERROR;
						Instance->Error = 6;
						return 1;
					}
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
					if (!DisableOnePinNetCheck)
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ClearErrors(int32 mode)
{
	int32 cnt, cnt3, ClearMask;

	InstanceRecord *Instance;
	WireRecord *Wire;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	BusRecord *Bus;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;

	if (mode == 0)
		ClearMask = OBJECT_ERROR | OBJECT_DONE;
	else
		ClearMask = OBJECT_ERROR | OBJECT_DONE | OBJECT_CHANGED_TO_SELECTED | 0x000f;

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

	for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
	{
		BusConnection = &((*BusConnections)[cnt3]);

		if ((BusConnection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			BusConnection->Info &= ~(ClearMask);
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckSheetForErrors(LPSTR Filename, int32 SheetMode, double *cx, double *cy)

{
	char str[MAX_LENGTH_STRING];
	int32 res, start, eind;

	res = SendMessage(EditWindow, EM_GETSEL, (WPARAM) & start, (LPARAM) & eind);

	ClearErrors(1);

	if (CheckWiresAndBusses(SheetMode))
	{
		sprintf(str, SC(226, "Wire/bus error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	CheckForLoseWires(SheetMode, Filename);

	if (CheckForOverlappingWires(SheetMode))
	{
		sprintf(str, SC(227, "Overlapping wires in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckForOverlappingBusses(SheetMode))
	{
		sprintf(str, SC(228, "Overlapping busses in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckBusConnections(SheetMode, cx, cy))
	{
		sprintf(str, SC(229, "Busconnection error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckGlobalConnections(SheetMode, cx, cy))
	{
		sprintf(str, SC(230, "Global connection error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckUnusedJunctions(SheetMode, cx, cy))
	{
		sprintf(str, SC(231, "Unused junction error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckUnusedOnePinNets(SheetMode, cx, cy))
	{
		sprintf(str, SC(232, "Unused one pin net error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckNetLabels(SheetMode, cx, cy))
	{
		sprintf(str, SC(233, "Netlabel error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckInstances(SheetMode, cx, cy))
	{
//		sprintf(str, SC(234, "Pin error ( %.1f , %.1f ) in sheet %s\r\n"), cx, cy, Filename); //pùvodní

		sprintf(str, SC(234, "Unused pin component in sheet %s\r\n"), Filename); //nový

		AddMessage(str);

		return 1;
	}

	if (CheckUnusedGlobalConnections(SheetMode, cx, cy))
	{
		sprintf(str, SC(235, "Unused Global connection error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	if (CheckUnusedBusConnections(SheetMode, cx, cy))
	{
		sprintf(str, SC(236, "Unused busconnection error(s) in sheet %s\r\n"), Filename);
		AddMessage(str);

		return 1;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
