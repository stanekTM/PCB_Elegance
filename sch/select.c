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
#include "memory.h"
#include "select.h"
#include "calcdef.h"
#include "calc2.h"
#include "graphics.h"
#include "calcrect.h"
#include "calc.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "line2.h"
#include "sch.h"
#include "stdio.h"
#include "math.h"
#include "edit.h"
#include "string.h"
#include "files2.h"
#include "mainloop.h"
#include "dialogs.h"
#include "resource.h"
#include "utf8.h"
#include "property.h"


#define  UNSELECT_ALL       20000
#define  UNSELECT_NETNR     20001
#define  SELECT_NETNR       20002
#define  UNSELECT_REF       20003
#define  SELECT_REF         20004

int32 DisplayInfoCursorX = -1, DisplayInfoCursorY = -1, ok;

char SearchTextStr[MAX_LENGTH_STRING], OldSearchTextStr[MAX_LENGTH_STRING];
int32 SearchTextCount;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetNetNrSelectedWiresObjects5(WireRecord * Wire, int32 mode);

int32 GetNetNrSelectedBussesObjects5(BusRecord * Bus, int32 mode);

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyToClipBoardMem(int32 Command, int32 NetNr, LPSTR Name)
{
	uint8 Buf[128], *MemPos;
	int32 BufSize, *Bufp;

	memset(&Buf, 0, sizeof(Buf));
	Bufp = (int32 *) & Buf;
	*Bufp++ = Command;
	BufSize = 0;

	switch (Command)
	{
//    case UNSELECT_ALL:
//      BufSize=2;
//      break;
	case UNSELECT_NETNR:
	case SELECT_NETNR:
		memmove(Bufp, Name, min(59, strlen(Name)));
		BufSize = 64;
		break;

	case UNSELECT_REF:
	case SELECT_REF:
		memmove(Bufp, Name, min(11, strlen(Name)));
		BufSize = 16;
		break;
	}

	if (BufSize > 0)
	{
		if (ClipBoardMemPos + BufSize + 128 >= ClipBoardMemSize)
		{
			if (AllocateMemClipBoard(ClipBoardMemPos + BufSize + 16384) != 0)
				return -1;
		}

		MemPos = &(ClipBoardMem[ClipBoardMemPos]);
		memmove(MemPos, &Buf, BufSize);
		ClipBoardMemPos += BufSize;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectObjectsFromWindow(double x1, double y1, double x2, double y2, int32 mode)
{
	int32 hulpx, hulpy, ok;

	StartDrawingEditingWindow();

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
	{
		hulpx = MultX(x1);
		hulpy = MultY(y1);
		SearchMinX = PixelToRealOffX(hulpx - 5);
		SearchMinY = PixelToRealOffY(DrawWindowMaxY - (hulpy + 5) - 1);
		SearchMaxX = PixelToRealOffX(hulpx + 5);
		SearchMaxY = PixelToRealOffY(DrawWindowMaxY - (hulpy - 5) - 1);
		mode = 0;
	}
	else
	{
		SearchMinX = min(x1, x2);
		SearchMinY = min(y1, y2);
		SearchMaxX = max(x1, x2);
		SearchMaxY = max(y1, y2);
	}

	SelectInstancesFromRectWindow(0);

	if (!EditingSymbol)
	{
		SelectWiresFromRectWindow(0);
		SelectBussesFromRectWindow(0);
		SelectJunctionsFromRectWindow(0);
		SelectOnePinNetsFromRectWindow(0);
		SelectBusConnectionsFromRectWindow(0);
		SelectGlobalConnectionsFromRectWindow(0);

		if ((Design.SheetInfo & 1) == 0)
			SelectNetLabelsFromRectWindow(0);
	}
	else
	{
		SelectPinBussesFromRectWindow(0);
		SelectPinsFromRectWindow(0);
		SelectPowerPinsFromRectWindow(0);
	}

	SelectObjectLinesFromRectWindow(0);
	SelectObjectRectsFromRectWindow(0);
	SelectObjectCirclesFromRectWindow(0);
	SelectObjectArcsFromRectWindow(0);
	SelectObjectTextsFromRectWindow(0);


	ExitDrawing();
	EndDrawingEditingWindow();

	if ((ViewMode == 1) && (mode == 0) && (!EditingSymbol) && (ClipBoardMemPos != 0))
	{
//    memmove(&Buf,ClipBoardMem,400);
//    memset(&Buf2,0,sizeof(Buf2));
//    memmove(&Buf2,ClipBoardMem,ClipBoardMemPos);
		CopySelectionsToClipBoard();
		ok = 1;
	}
}

//****************************************************************************************************************************************
//****************************** pravý spodní pøeklad vybraných symbolù ******************************************************************
//****************************************************************************************************************************************

void SelectInstancesFromRectWindow(int32 Mode)
{
	double x1a, x2a, y1a, y2a;
	int32 cnt, cnt2, InstanceInfo, ObjectMirrorX, ObjectMirrorY, Rotation, ObjectChanged, Found, Selected;
	InstanceRecord *Instance;
	ObjectRecord *Object;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		InstanceInfo = Instance->Info;
		ObjectChanged = 0;

		if ((InstanceInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			x1a = Instance->BoardPosMinX;
			y1a = Instance->BoardPosMinY;
			x2a = Instance->BoardPosMaxX;
			y2a = Instance->BoardPosMaxY;

			if (UnselectAll)
			{
				if ((InstanceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					InstanceInfo ^= OBJECT_SELECTED;
					InstanceInfo &= ~7;
					ObjectChanged = 1;

					if (ViewMode == 1)
						CopyToClipBoardMem(UNSELECT_REF, 0, Instance->Reference);
				}
			}
			else
			{
				if ((SearchMaxX > x1a) && (SearchMinX < x2a) && (SearchMaxY > y1a) && (SearchMinY < y2a))
				{
					NrObjects = 0;
					InstanceToObject(Instance, 0.0, 0.0, 0);
					FillPositionObjects();
					Found = 0;
					cnt2 = 0;

					while ((!Found) && (cnt2 < NrObjects))
					{
						Object = &((*Objects)[cnt2]);

						if ((Object->maxx >= SearchMinX) && (Object->minx <= SearchMaxX) && (Object->maxy >= SearchMinY)
						        && (Object->miny <= SearchMaxY))
						{
							Found = 1;

							if ((InstanceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							{
								if ((InstanceInfo & 4) == 4)
								{
									InstanceInfo &= ~(OBJECT_SELECTED | 7);

									if (ViewMode == 1)
										CopyToClipBoardMem(UNSELECT_REF, 0, Instance->Reference);
								}
								else
								{
									InstanceInfo |= 7;

									if (ViewMode == 1)
										CopyToClipBoardMem(SELECT_REF, 0, Instance->Reference);
								}
							}
							else
							{
								InstanceInfo |= 7;
								InstanceInfo ^= OBJECT_SELECTED;

								if (ViewMode == 1)
									CopyToClipBoardMem(SELECT_REF, 0, Instance->Reference);
							}

							ObjectChanged = 1;
						}

						cnt2++;
					}
				}

				Rotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
				ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
				ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);

				if (!ObjectChanged)
				{

// *****************************************************************************************
					if (GetMinMaxInstanceReferenceText(Instance) == 0)
					{
						if ((TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
						        && (TextMinY <= SearchMaxY))
						{
							if ((InstanceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							{
								if ((InstanceInfo & 4) == 4)
									InstanceInfo &= ~(OBJECT_SELECTED | 7);
								else
								{
									InstanceInfo ^= 1;

									if ((InstanceInfo & 7) == 0)
										InstanceInfo ^= OBJECT_SELECTED;
								}
							}
							else
								InstanceInfo |= OBJECT_SELECTED | 1;

							ObjectChanged = 1;
						}
					}

					// *****************************************************************************************

					if (GetMinMaxInstanceValueText(Instance) == 0)
					{
						if ((TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
						        && (TextMinY <= SearchMaxY))
						{
							if ((InstanceInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							{
								if ((InstanceInfo & 4) == 4)
									InstanceInfo &= ~(OBJECT_SELECTED | 7);
								else
								{
									InstanceInfo ^= 2;

									if ((InstanceInfo & 7) == 0)
										InstanceInfo ^= OBJECT_SELECTED;
								}
							}
							else
								InstanceInfo |= OBJECT_SELECTED | 2;

							ObjectChanged = 1;
						}
					}
				}
			}
		}

		if (ObjectChanged)
		{
			Instance->Info = (int16) InstanceInfo;
			DrawInstance(Instance, 0.0, 0.0, 0);
		}
	}

	Selected = 0;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (Instance->Info & OBJECT_SELECTED)
				Selected++;
		}
	}

	if (Selected > 0)
		sprintf(InfoStr, SC(420, "%d symbols selected"), Selected);
	else
		InfoStr[0] = 0;

	if (!OutputDisplay)
		RedrawInfoStr(1);
	else
		RedrawInfoStr(0);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectWiresFromRectWindow(int32 Mode)
{
	int32 cnt, WireInfo, TestResult;
	double x1, y1, x2, y2;
	int32 Check, Changed;

	WireRecord *Wire;

	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);
		WireInfo = Wire->Info;

		if ((WireInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;
			Check = 0;
			Changed = 0;

			if (UnselectAll)
			{
				if ((WireInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					WireInfo &= ~(OBJECT_SELECTED | 3);
					Changed = 1;

					if (ViewMode == 1)
						GetNetNrSelectedWiresObjects5(Wire, UNSELECT_NETNR);
				}
			}
			else
			{
				if ((TestResult = RectTestLine2(x1, y1, x2, y2)) != 0)
				{
					if ((WireInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						WireInfo = (WireInfo ^ OBJECT_SELECTED) & ~3;
						Changed = 1;

						if (ViewMode == 1)
							GetNetNrSelectedWiresObjects5(Wire, UNSELECT_NETNR);
					}
					else
					{
						if ((WireSelectMode == 0) && (TestResult == 3))
						{
							if (((SearchMinX < min(x1, x2)) && (SearchMaxX > max(x1, x2)))
							        && ((SearchMinY < min(y1, y2)) && (SearchMaxY > max(y1, y2))))
							{
								if (ViewMode == 1)
									GetNetNrSelectedWiresObjects5(Wire, SELECT_NETNR);

								WireInfo = ((WireInfo ^ OBJECT_SELECTED) & ~3) | TestResult;
								Changed = 1;
							}
						}
						else
						{
							if (ViewMode == 1)
								GetNetNrSelectedWiresObjects5(Wire, SELECT_NETNR);

							WireInfo = ((WireInfo ^ OBJECT_SELECTED) & ~3) | TestResult;
							Changed = 1;
						}
					}
				}
			}

			if (Changed)
			{
				Wire->Info = (int16) WireInfo;
				DrawWire(Wire, 0.0, 0.0, 0);
				/*
				        for (cnt2=0;cnt2<Design.NrNetLabels;cnt2++) {
				          NetLabel=&((*NetLabels)[cnt2]);
				          if ((NetLabel->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == 0) {
				            x3=NetLabel->ConnectX;
				            y3=NetLabel->ConnectY;
				            x4=x3+NetLabel->TextX;
				            y4=y3+NetLabel->TextY;
				            if (((InRange(x1,x3))
				               &&
				               (InRange(y1,y3)))
				               ||
				               ((InRange(x2,x3))
				               &&
				               (InRange(y2,y3)))) {
				              ok=1;
				              NetLabel->Info|=OBJECT_SELECTED|3;
				              DrawNetLabel(NetLabel,0.0,0.0,0);
				            }
				          }
				        }
				*/
			}
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectBussesFromRectWindow(int32 Mode)
{
	double x1, y1, x2, y2;
	int32 cnt, BusInfo, TestResult, Changed, Check;

	BusRecord *Bus;

	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);
		BusInfo = Bus->Info;

		if ((BusInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Bus->X1;
			y1 = Bus->Y1;
			x2 = Bus->X2;
			y2 = Bus->Y2;
			Check = 0;
			Changed = 0;

			if (UnselectAll)
			{
				if ((BusInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					BusInfo &= ~(OBJECT_SELECTED | 3);

					if (ViewMode == 1)
						GetNetNrSelectedBussesObjects5(Bus, UNSELECT_NETNR);

					Changed = 1;
				}
			}
			else
			{
				if ((TestResult = RectTestLine2(x1, y1, x2, y2)) != 0)
				{
					if ((BusInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						BusInfo = (BusInfo ^ OBJECT_SELECTED) & ~3;
						Changed = 1;

						if (ViewMode == 1)
							GetNetNrSelectedBussesObjects5(Bus, UNSELECT_NETNR);
					}
					else
					{
						if ((WireSelectMode == 0) && (TestResult == 3))
						{
							if (((SearchMinX < min(x1, x2)) && (SearchMaxX > max(x1, x2)))
							        && ((SearchMinY < min(y1, y2)) && (SearchMaxY > max(y1, y2))))
							{
								if (ViewMode == 1)
									GetNetNrSelectedBussesObjects5(Bus, SELECT_NETNR);

								BusInfo = ((BusInfo ^ OBJECT_SELECTED) & ~3) | TestResult;
								Changed = 1;
							}
						}
						else
						{
							if (ViewMode == 1)
								GetNetNrSelectedBussesObjects5(Bus, SELECT_NETNR);

							BusInfo = ((BusInfo ^ OBJECT_SELECTED) & ~3) | TestResult;
							Changed = 1;
						}
					}
				}
			}

			if (Changed)
			{
				Bus->Info = (int16) BusInfo;
				DrawBus(Bus, 0.0, 0.0, 0);
				/*
				        for (cnt2=0;cnt2<Design.NrNetLabels;cnt2++) {
				          NetLabel=&((*NetLabels)[cnt2]);
				          if ((NetLabel->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == 0) {
				            x3=NetLabel->ConnectX;
				            y3=NetLabel->ConnectY;
				            x4=x3+NetLabel->TextX;
				            y4=y3+NetLabel->TextY;
				            if ((InRange(x1,x3))
				               &&
				               (InRange(y1,y3))) {
				              ok=1;
				              NetLabel->Info|=OBJECT_SELECTED|3;
				              DrawNetLabel(NetLabel,0.0,0.0,0);
				            }
				          }
				        }
				*/
			}
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectJunctionsFromRectWindow(int32 Mode)
{
	int32 cnt;
	double x1, y1, ThickNess;
	int32 JunctionInfo, ok;
	JunctionRecord *Junction;

	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);
		JunctionInfo = Junction->Info;

		if ((JunctionInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Junction->X;
			y1 = Junction->Y;
			ThickNess = 0.5;

//      ThickNess=Junction->ThickNess;
			if (((UnselectAll) && ((JunctionInfo & OBJECT_SELECTED) == OBJECT_SELECTED))
			        || ((!UnselectAll) && (RectTestCircle(x1, y1, ThickNess, 0xff))))
			{
				JunctionInfo ^= OBJECT_SELECTED;
				Junction->Info = (int16) JunctionInfo;
				DrawJunction(Junction, 0.0, 0.0, 0);
			}
		}
	}

	ok = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectOnePinNetsFromRectWindow(int32 Mode)
{
	int32 cnt;
	double x1, y1, ThickNess;
	int32 OnePinNetInfo, ok;
	OnePinNetRecord *OnePinNet;

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);
		OnePinNetInfo = OnePinNet->Info;

		if ((OnePinNetInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = OnePinNet->X;
			y1 = OnePinNet->Y;
			ThickNess = 0.5;

//      ThickNess=OnePinNet->ThickNess;
			if (((UnselectAll) && ((OnePinNetInfo & OBJECT_SELECTED) == OBJECT_SELECTED))
			        || ((!UnselectAll) && (RectTestCircle(x1, y1, ThickNess, 0xff))))
			{
				OnePinNetInfo ^= OBJECT_SELECTED;
				OnePinNet->Info = (int16) OnePinNetInfo;
				DrawOnePinNet(OnePinNet, 0.0, 0.0, 0);
			}
		}
	}

	ok = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectNetLabelsFromRectWindow(int32 Mode)
{
	int32 cnt, NrProperties, NetLabelInfo, ok, TextAlignment, TextRotation;
	double x1, y1, x2, y2;
	NetLabelRecord *NetLabel;
	char str[MAX_LENGTH_STRING];

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);
		NetLabelInfo = NetLabel->Info;

		if ((NetLabelInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = NetLabel->ConnectX;
			y1 = NetLabel->ConnectY;
			x2 = x1 + NetLabel->TextX;
			y2 = y1 + NetLabel->TextY;
			TextAlignment = (NetLabel->Alignment & 0x0f);
			TextRotation = (NetLabel->Alignment >> 8) & 0x01;
			NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1);

			if (NrProperties > 0)
				sprintf(str, "%s (...)", NetLabel->Name);
			else
				strcpy(str, NetLabel->Name);

			GetMinMaxText(x2, y2, 1.0, 0, TextRotation, TextAlignment, str);

//      ThickNess=2.0;
//      ThickNess=NetLabel->ThickNess;
			if (UnselectAll)
			{
				if ((NetLabelInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					NetLabelInfo &= ~(OBJECT_SELECTED | 3);
					NetLabel->Info = (int16) NetLabelInfo;
					DrawNetLabel(NetLabel, 0.0, 0.0, 0);
				}
			}
			else
			{
				NetLabelInfo &= ~3;

				if (((TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
				        && (TextMinY <= SearchMaxY)) || ((x1 >= SearchMinX) && (x1 <= SearchMaxX) && (y1 >= SearchMinY)
				                && (y1 <= SearchMaxY)))
				{
					if ((NetLabelInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						NetLabelInfo &= ~(OBJECT_SELECTED | 3);
					else
					{
						if ((TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
						        && (TextMinY <= SearchMaxY))
							NetLabelInfo |= OBJECT_SELECTED | 2;

						if ((x1 >= SearchMinX) && (x1 <= SearchMaxX) && (y1 >= SearchMinY) && (y1 <= SearchMaxY))
							NetLabelInfo |= OBJECT_SELECTED | 1;

						NetLabel->Info = (int16) NetLabelInfo;
					}

					DrawNetLabel(NetLabel, 0.0, 0.0, 0);
				}
			}
		}
	}

	ok = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectBusConnectionsFromRectWindow(int32 Mode)
{
	double x1, y1;
	int32 cnt, BusConnectionInfo, ok, BusConnectionType;
	BusConnectionRecord *BusConnection;
	int32 Changed;

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);
		BusConnectionInfo = BusConnection->Info;

		if ((BusConnectionInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			BusConnectionType = (BusConnection->Alignment >> 14) & 3;
			x1 = BusConnection->X;
			y1 = BusConnection->Y;

			Changed = 0;

			if (UnselectAll)
			{
				if ((BusConnectionInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					Changed = 1;
					BusConnectionInfo &= ~OBJECT_SELECTED;
				}
			}
			else
			{
				switch (BusConnectionType)
				{
				case 0:
					if (RectTestRect(x1 - BusSizeX, y1 - BusSizeY * 0.5, x1, y1 + BusSizeY * 0.5))
					{
						BusConnectionInfo ^= OBJECT_SELECTED;
						Changed = 1;
					}

					break;

				case 1:
					if (RectTestRect(x1 - BusSizeY * 0.5, y1 - BusSizeX, x1 + BusSizeY * 0.5, y1))
					{
						BusConnectionInfo ^= OBJECT_SELECTED;
						Changed = 1;
					}

					break;

				case 2:
					if (RectTestRect(x1, y1 - BusSizeY * 0.5, x1 + BusSizeX, y1 + BusSizeY * 0.5))
					{
						BusConnectionInfo ^= OBJECT_SELECTED;
						Changed = 1;
					}

					break;

				case 3:
					if (RectTestRect(x1 - BusSizeY * 0.5, y1, x1 + BusSizeY * 0.5, y1 + BusSizeX))
					{
						BusConnectionInfo ^= OBJECT_SELECTED;
						Changed = 1;
					}

					break;
				}
			}

			if (Changed)
			{
				BusConnection->Info = (int16) BusConnectionInfo;
				DrawBusConnection(BusConnection, 0.0, 0.0, 0);
			}
		}
	}

	ok = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectGlobalConnectionsFromRectWindow(int32 Mode)
{
	double x1a, y1a, x1, y1, yy2, yy2a, Xmax, Xmin, Ymax, Ymin;
	int32 cnt, GlobalConnectionInfo, ConnectionType;
	int32 Changed;

	GlobalConnectionRecord *GlobalConnection;

	Xmin = 0.0;
	Ymin = 0.0;
	Xmax = 0.0;
	Ymax = 0.0;

	for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
	{
		GlobalConnection = &((*GlobalConnections)[cnt]);
		GlobalConnectionInfo = GlobalConnection->Info;

		if ((GlobalConnectionInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			ConnectionType = GlobalConnection->ConnectionType;
			x1 = GlobalConnection->X;
			y1 = GlobalConnection->Y;
			x1a = GlobalConnection->NameX;
			y1a = GlobalConnection->NameY;
			GetMinMaxText(x1a, y1a, 1.0, 0, (int32) ((GlobalConnection->NameInfo >> 8) & 1),
			              (int32) (GlobalConnection->NameInfo & 15), (LPSTR) GlobalConnection->Text);
			Changed = 0;

			switch (ConnectionType >> 1)
			{
			case 0:			// input
			case 1:			// output
				yy2 = 0.4;
				Ymin = y1 - yy2;
				Ymax = y1 + yy2;

				if ((ConnectionType & 1) == 0)
				{
					Xmin = x1 - yy2 * 2;
					Xmax = x1;
				}
				else
				{
					Xmin = x1;
					Xmax = x1 + yy2 * 2;
				}

				break;

			case 2:			// output
				yy2 = 0.4;
				yy2a = 2.8;
				Ymin = y1 - yy2;
				Ymax = y1 + yy2;

				if ((ConnectionType & 1) == 0)
				{
					Xmin = x1 - yy2a;
					Xmax = x1;
				}
				else
				{
					Xmin = x1;
					Xmax = x1 + yy2a;
				}

				break;
			}

			if (UnselectAll)
			{
				if ((GlobalConnectionInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					Changed = 1;
					GlobalConnectionInfo &= ~(OBJECT_SELECTED | 3);
				}
			}
			else
			{
				if (RectTestRect(Xmin, Ymin, Xmax, Ymax))
				{
					if ((GlobalConnectionInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						GlobalConnectionInfo &= ~(OBJECT_SELECTED | 3);
					else
						GlobalConnectionInfo |= (OBJECT_SELECTED | 3);

					Changed = 1;
				}
				else
				{
					if ((TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
					        && (TextMinY <= SearchMaxY))
					{
						if ((GlobalConnectionInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							GlobalConnectionInfo ^= 2;

							if ((GlobalConnectionInfo & 3) == 0)
								GlobalConnectionInfo &= ~(OBJECT_SELECTED | 3);
						}
						else
							GlobalConnectionInfo |= (OBJECT_SELECTED | 2);

						Changed = 1;
					}
				}
			}

			if (Changed)
			{
				GlobalConnection->Info = (int16) GlobalConnectionInfo;
				DrawGlobalConnection(GlobalConnection, 0.0, 0.0, 0);
			}
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectObjectLinesFromRectWindow(int32 Mode)
{
	double x1, y1, x2, y2;
	int32 cnt, ObjectLineInfo, TestResult;
	int32 Check, Changed;

	ObjectLineRecord *ObjectLine;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
		ObjectLineInfo = ObjectLine->Info;

		if ((ObjectLineInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			x1 = ObjectLine->X1;
			y1 = ObjectLine->Y1;
			x2 = ObjectLine->X2;
			y2 = ObjectLine->Y2;
			Check = 0;
			Changed = 0;

			if (UnselectAll)
			{
				if ((ObjectLineInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					ObjectLineInfo &= ~(OBJECT_SELECTED | 3);
					Changed = 1;
				}
			}
			else
			{
				if ((TestResult = RectTestLine2(x1, y1, x2, y2)) != 0)
				{
					if ((ObjectLineInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						ObjectLineInfo = (ObjectLineInfo ^ OBJECT_SELECTED) & ~3;
						Changed = 1;
					}
					else
					{
						if ((WireSelectMode == 0) && (TestResult == 3))
						{
							if (((SearchMinX < min(x1, x2)) && (SearchMaxX > max(x1, x2)))
							        && ((SearchMinY < min(y1, y2)) && (SearchMaxY > max(y1, y2))))
							{
								ObjectLineInfo = ((ObjectLineInfo ^ OBJECT_SELECTED) & ~3) | TestResult;
								Changed = 1;
							}
						}
						else
						{
							ObjectLineInfo = ((ObjectLineInfo ^ OBJECT_SELECTED) & ~3) | TestResult;
							Changed = 1;
						}
					}
				}
			}

			if (Changed)
			{
				ObjectLine->Info = (int16) ObjectLineInfo;
				DrawObjectLine(ObjectLine, 0.0, 0.0, 0);
			}
		}
	}
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectObjectRectsFromRectWindow(int32 Mode)
{
	double x1, y1, x2, y2;
	int32 cnt, ObjectRectInfo;
	int32 Check;

	ObjectRectRecord *ObjectRect;

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);
		ObjectRectInfo = ObjectRect->Info;

		if ((ObjectRectInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			x1 = ObjectRect->CentreX;
			y1 = ObjectRect->CentreY;
			x2 = ObjectRect->Width;
			y2 = ObjectRect->Height;
			Check = 0;

			if (((UnselectAll) && ((ObjectRectInfo & OBJECT_SELECTED) == OBJECT_SELECTED))
			        || ((!UnselectAll) && (RectTestRect2(x1, y1, x2, y2))))
			{
				ObjectRectInfo ^= OBJECT_SELECTED;
				ObjectRect->Info = (int16) ObjectRectInfo;
				DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
			}
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectObjectCirclesFromRectWindow(int32 Mode)
{
	double x1, y1, x2;
	int32 cnt, ObjectCircleInfo, CircleMode;
	int32 Check;

	ObjectCircleRecord *ObjectCircle;

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);
		ObjectCircleInfo = ObjectCircle->Info;

		if ((ObjectCircleInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			x1 = ObjectCircle->CentreX;
			y1 = ObjectCircle->CentreY;
			x2 = ObjectCircle->Diam;
			CircleMode = CircleConv[(int32) ObjectCircle->CircleMode];
			Check = 0;

			if (((UnselectAll) && ((ObjectCircleInfo & OBJECT_SELECTED) == OBJECT_SELECTED))
			        || ((!UnselectAll) && (RectTestCircle(x1, y1, x2, CircleMode))))
			{
				ObjectCircleInfo ^= OBJECT_SELECTED;
				ObjectCircle->Info = (int16) ObjectCircleInfo;
				DrawObjectCircle(ObjectCircle, 0.0, 0.0, 0);
			}
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************



void SelectObjectArcsFromRectWindow(int32 Mode)
{
	double x1, y1, x2, y2, x3, y3, x4, y4;
	int32 cnt, ObjectArcInfo;
	int32 Check;

	ObjectArcRecord *ObjectArc;

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ObjectArcInfo = ObjectArc->Info;

		if ((ObjectArcInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			x1 = ObjectArc->CentreX;
			y1 = ObjectArc->CentreY;
			x2 = ObjectArc->Width;
			y2 = ObjectArc->Height;
			x3 = ObjectArc->StartDiffX;
			y3 = ObjectArc->StartDiffY;
			x4 = ObjectArc->EndDiffX;
			y4 = ObjectArc->EndDiffY;
			Check = 0;

			if (((UnselectAll) && ((ObjectArcInfo & OBJECT_SELECTED) == OBJECT_SELECTED))
			        || ((!UnselectAll) && (RectTestArc(x1, y1, x2, y2, x3, y3, x4, y4))))
			{
				ObjectArcInfo ^= OBJECT_SELECTED;
				ObjectArc->Info = (int16) ObjectArcInfo;
				DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
			}
		}
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectObjectTextsFromRectWindow(int32 Mode)
{
	double x1, y1, x2, Rotation;
	int32 cnt, ObjectTextInfo, TextMode, TextAlignment, cnt2, cnt3, Length;
	int32 ObjectToBeSelected, ObjectChanged;
	char TextString[MAX_LENGTH_STRING];
	double x4, y4;
	ObjectTextRecord *ObjectText;

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);
		ObjectTextInfo = ObjectText->Info;
		ObjectChanged = 0;

		if ((ObjectTextInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			if (UnselectAll)
			{
				if ((ObjectTextInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					ObjectText->Info &= ~OBJECT_SELECTED;
					ObjectChanged = 1;
				}
			}
			else
			{
				ObjectToBeSelected = 0;

				if ((Length = strlen(ObjectText->Text)) > 0)
				{
					x1 = ObjectText->X;
					y1 = ObjectText->Y;
					x2 = ObjectText->FontHeight;
					TextMode = ObjectText->TextMode;
					Rotation = ObjectText->Rotation;
					TextAlignment = (TextMode & 0x0f);

					x4 = x1;
					y4 = y1;
					cnt3 = 0;
					cnt2 = cnt3;

					while (cnt3 < Length + 1)
					{
						if ((ObjectText->Text[cnt3] == '\r')
						        || ((cnt3 == Length) && (ObjectText->Text[cnt3 - 1] != '\n')))
						{
							if (cnt3 - cnt2 > 0)
							{
								memset(TextString, 0, sizeof(TextString));
								strncpy(TextString, (LPSTR) & ObjectText->Text[cnt2], min(127, cnt3 - cnt2));

								if ((Rotation == 0.0) || (InRangeSpecial(Rotation, 90.0, 0.01)))
								{
									if (Rotation == 0.0)
										GetMinMaxText(x4, y4, x2, 0, 0, TextAlignment, TextString);
									else
										GetMinMaxText(x4, y4, x2, 0, 1, TextAlignment, TextString);
								}
								else
									GetMinMaxText2(x4, y4, x2, 0, Rotation, TextAlignment, 0, TextString);

								if ((TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
								        && (TextMinY <= SearchMaxY))
									ObjectToBeSelected = 1;
							}

							x4 += sin(ANGLE_CONVERT(Rotation)) * x2;
							y4 -= cos(ANGLE_CONVERT(Rotation)) * x2;
							cnt3 += 1;
							cnt2 = cnt3 + 1;
						}

						cnt3++;
					}
				}

				if (ObjectToBeSelected)
				{
					ObjectTextInfo ^= OBJECT_SELECTED;
					ObjectText->Info = (int16) ObjectTextInfo;
					ObjectChanged = 1;
				}
			}

			if (ObjectChanged)
				DrawObjectText(ObjectText, 0.0, 0.0, 0);
		}
	}

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectPinsFromRectWindow(int32 Mode)
{
	double x1, y1, x1a, y1a;
	int32 cnt, PinInfo, ok;
	PinRecord *Pin;
	int32 Changed;

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);
		PinInfo = Pin->Info;

		if ((PinInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Pin->X;
			x1a = Pin->NameX;
			y1 = Pin->Y;
			y1a = Pin->NameY;

//      TextAlignment=Pin->Alignment;
//      TextRotation=0;
//      TextAlignment=(Pin->Alignment & 0x0f);
			if (!EditingSheetSymbol)
				GetMinMaxText(x1a, y1a, 1.0, 0, (Pin->NameInfo >> 8) & 1, Pin->NameInfo & 15, (LPSTR) Pin->Name);
			else
				GetMinMaxText(x1a, y1a, 1.0, 0, (Pin->NameInfo >> 8) & 1, Pin->NameInfo & 15, (LPSTR) Pin->Label);

			Changed = 0;

			if (UnselectAll)
			{
				if ((PinInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					Changed = 1;
					PinInfo &= ~(OBJECT_SELECTED | 3);
				}
			}
			else
			{
				if (RectTestRect(x1 - 0.25, y1 - 0.25, x1 + 0.25, y1 + 0.25))
				{
					if ((PinInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						PinInfo &= ~(OBJECT_SELECTED | 3);
					else
						PinInfo |= (OBJECT_SELECTED | 3);

					Changed = 1;
				}
				else
				{
					if ((TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
					        && (TextMinY <= SearchMaxY))
					{
						if ((PinInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							PinInfo ^= 2;

							if ((PinInfo & 3) == 0)
								PinInfo &= ~(OBJECT_SELECTED | 3);
						}
						else
							PinInfo |= (OBJECT_SELECTED | 2);

						Changed = 1;
					}
				}
			}

			if (Changed)
			{
				Pin->Info = (int16) PinInfo;
				DrawPin(Pin, 0.0, 0.0, 0);
			}
		}
	}

	ok = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectPowerPinsFromRectWindow(int32 Mode)
{
	double x1, y1, x2;
	int32 cnt, PowerPinInfo, TextRotation, Alignment;
	int32 Check;
	char str[MAX_LENGTH_STRING];

	PowerPinRecord *PowerPin;

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);
		PowerPinInfo = PowerPin->Info;

		if ((PowerPinInfo & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			x1 = PowerPin->NameX;
			y1 = PowerPin->NameY;
			x2 = 1.0;
			strcpy(str, PowerPin->NetName);
			strcat(str, " : ");
			strcat(str, PowerPin->Text);
			TextRotation = (PowerPin->NameInfo >> 8) & 1;
			Alignment = PowerPin->NameInfo & 0x0f;
			GetMinMaxText(x1, y1, x2, 0, TextRotation, Alignment, str);
			Check = 0;

			if (((UnselectAll) && ((PowerPinInfo & OBJECT_SELECTED) == OBJECT_SELECTED))
			        || ((!UnselectAll) && (TextMaxX >= SearchMinX) && (TextMinX <= SearchMaxX) && (TextMaxY >= SearchMinY)
			            && (TextMinY <= SearchMaxY)))
			{
				PowerPinInfo ^= OBJECT_SELECTED;
				PowerPin->Info = (int16) PowerPinInfo;
				DrawPowerPin(PowerPin, 0.0, 0.0, 0);
			}
		}
	}

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SelectPinBussesFromRectWindow(int32 Mode)
{
	int32 cnt, cnt2, NrLines, lengte, AddMode, PinBusInfo, ok, TextAlignment, TextRotation;
	double x1, y1, x2, x1a, y1a, xx3, yy3, Xmax, Xmin, Ymax, Ymin;
	PinBusRecord *PinBus;
	int32 Changed;
	LPSTR LabelText;
	char PinBusStrings[16][64];
	int32 LinePos[16];

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);
		PinBusInfo = PinBus->Info;

		if ((PinBusInfo & OBJECT_NOT_VISIBLE) == 0)
		{

			x1 = PinBus->X;
			x1a = PinBus->NameX;
			y1 = PinBus->Y;
			y1a = PinBus->NameY;
			x2 = 1.0;
			LabelText = (PinBus->Label);

#ifdef _DEBUG

			if (stricmp(LabelText, "DQ[56:63]") == 0)
				ok = 1;

#endif

			cnt2 = 0;
			NrLines = 0;
			lengte = strlen(PinBus->Text);
			memset(LinePos, 0, sizeof(LinePos));
			memset(PinBusStrings, 0, sizeof(PinBusStrings));
			LinePos[0] = 0;

			while (cnt2 < lengte)
			{
				if (PinBus->Text[cnt2] == '\\')
				{
					memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], (int) (cnt2 - LinePos[NrLines]));
					NrLines++;
					LinePos[NrLines] = cnt2 + 1;
				}

				cnt2++;
			}

			if (PinBus->Text[lengte - 1] != '\\')
			{
				memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], (int) (cnt2 - LinePos[NrLines]));
				LinePos[NrLines] = cnt2;
				NrLines++;
			}

			xx3 = x1a;
			yy3 = y1a;
			TextRotation = (PinBus->NameInfo >> 8) & 1;
			TextAlignment = PinBus->NameInfo & 0x0f;
			AddMode = 0;

			if (TextRotation)
				AddMode += 4;

			if ((TextAlignment == 6) || (TextAlignment == 8))
				AddMode += 1;	// Mirror X

			if ((TextAlignment == 2) || (TextAlignment == 8))
				AddMode += 2;	// Mirror Y


			Xmax = -10000000.0;
			Xmin = 10000000.0;
			Ymax = -10000000.0;
			Ymin = 10000000.0;

			switch (AddMode)
			{
			case 2:			// Rotation = 0 , MirrorY = 1 , MirrorX = 0
			case 3:			// Rotation = 0 , MirrorY = 1 , MirrorX = 1
				yy3 += (NrLines - 1);
				break;

			case 6:			// Rotation = 1 , MirrorY = 1 , MirrorX = 0
			case 7:			// Rotation = 1 , MirrorY = 1 , MirrorX = 1
				xx3 -= (NrLines - 1);
				break;
			}

			for (cnt2 = 0; cnt2 < NrLines; cnt2++)
			{
				GetMinMaxText(xx3, yy3, x2, 0, TextRotation, TextAlignment, PinBusStrings[cnt2]);
				Xmin = min(Xmin, TextMinX);
				Ymin = min(Ymin, TextMinY);
				Xmax = max(Xmax, TextMaxX);
				Ymax = max(Ymax, TextMaxY);

				switch (AddMode)
				{
				case 0:		// Rotation = 0 , MirrorY = 0 , MirrorX = 0
				case 1:		// Rotation = 0 , MirrorY = 0 , MirrorX = 1
				case 2:		// Rotation = 0 , MirrorY = 1 , MirrorX = 0
				case 3:		// Rotation = 0 , MirrorY = 1 , MirrorX = 1
					yy3 -= 1.0;
					break;

				case 4:		// Rotation = 1 , MirrorY = 0 , MirrorX = 0
				case 5:		// Rotation = 1 , MirrorY = 0 , MirrorX = 1
				case 6:		// Rotation = 1 , MirrorY = 1 , MirrorX = 0
				case 7:		// Rotation = 1 , MirrorY = 1 , MirrorX = 1
					xx3 += 1.0;
					break;
				}
			}

			Changed = 0;

			if (UnselectAll)
			{
				if ((PinBusInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
				{
					Changed = 1;
					PinBusInfo &= ~(OBJECT_SELECTED | 3);
				}
			}
			else
			{
				if (RectTestRect(x1 - 0.25, y1 - 0.25, x1 + 0.25, y1 + 0.25))
				{
					if ((PinBusInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						PinBusInfo &= ~(OBJECT_SELECTED | 3);
					else
						PinBusInfo |= (OBJECT_SELECTED | 3);

					Changed = 1;
				}
				else
				{
					if ((Xmax >= SearchMinX) && (Xmin <= SearchMaxX) && (Ymax >= SearchMinY) && (Ymin <= SearchMaxY))
					{
						if ((PinBusInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
						{
							PinBusInfo ^= 2;

							if ((PinBusInfo & 3) == 0)
								PinBusInfo &= ~(OBJECT_SELECTED | 3);
						}
						else
							PinBusInfo |= (OBJECT_SELECTED | 2);

						Changed = 1;
					}
				}
			}

			if (Changed)
			{
				PinBus->Info = (int16) PinBusInfo;
				DrawPinBus(PinBus, 0.0, 0.0, 0);
			}
		}
	}

	ok = 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void GetNrSelections()
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

	InstancesSelected = 0;
	InstanceRefsSelected = 0;
	InstanceValuesSelected = 0;
	WiresSelected = 0;
	BussesSelected = 0;
	BusConnectionsSelected = 0;
	JunctionsSelected = 0;
	OnePinNetsSelected = 0;
	NetLabelsSelected = 0;
	ObjectLinesSelected = 0;
	ObjectRectsSelected = 0;
	ObjectCirclesSelected = 0;
	ObjectArcsSelected = 0;
	ObjectTextsSelected = 0;
	PinsSelected = 0;
	PowerPinsSelected = 0;
	PinBussesSelected = 0;
	GlobalConnectionsSelected = 0;
	GlobalConnectionTextsSelected = 0;

//  SubSheetsSelected[CurrentSheetNr]=0;
	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if ((Instance->Info & 4) == 4)
			{
				InstancesSelected++;
				InstanceRefsSelected++;
				InstanceValuesSelected++;
			}
			else
			{
				if ((Instance->Info & 1) == 1)
					InstanceRefsSelected++;

				if ((Instance->Info & 2) == 2)
					InstanceValuesSelected++;
			}
		}
	}

	if (!EditingSymbol)
	{

		for (cnt = 0; cnt < Design.NrWires; cnt++)
		{
			Wire = &((*Wires)[cnt]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				WiresSelected++;
		}

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				BussesSelected++;
		}

		for (cnt = 0; cnt < Design.NrJunctions; cnt++)
		{
			Junction = &((*Junctions)[cnt]);

			if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				JunctionsSelected++;
		}

		for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
		{
			OnePinNet = &((*OnePinNets)[cnt]);

			if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				OnePinNetsSelected++;
		}

		if ((Design.SheetInfo & 1) == 0)
		{
			for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
			{
				NetLabel = &((*NetLabels)[cnt]);

				if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					NetLabelsSelected++;
			}
		}

		for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
		{
			BusConnection = &((*BusConnections)[cnt]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				BusConnectionsSelected++;
		}

		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((GlobalConnection->Info & 1) == 1)
					GlobalConnectionsSelected++;

				if ((GlobalConnection->Info & 2) == 2)
					GlobalConnectionTextsSelected++;
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				PinsSelected++;
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);

			if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				PowerPinsSelected++;
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);

			if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				PinBussesSelected++;
		}

	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectLinesSelected++;
	}


	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectRectsSelected++;
	}

	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectCirclesSelected++;
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectArcsSelected++;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			ObjectTextsSelected++;
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ExtendSelections(int32 Mode)
{
	int32 cnt, cnt2, cnt3, CalcObjects, Found = 0;
	double CompX, CompY, InstanceMinX, InstanceMaxX, InstanceMinY, InstanceMaxY, x1, y1, x2, y2, x3, y3;

	/*

	  Mode = 0 (Move objects)

	      Select netlabels connected to selected wires/busses
	?     Select bus connections connected to selected wires/busses
	?     Select global connections connected to selected wires/busses
	      Select object lines to fully
	      Select busses to fully
	      Select wires to fully
	      Select instances to fully

	  Mode = 1 (Drag objects)

	      Select wires/busses connected to instances
	      Select wires/busses connected to fully selected wires/busses
	      Select wires/busses on one side connected to selected netlabels
	      Select wires/busses on one side connected to selected global connections
	      Select wires/busses on one side connected to selected bus connections
	      Select netlabels connected to partially selected wires/busses
	      Select bus connections connected to selected wires/busses
	      Select global connections connected to selected wires/busses

	      Select object lines connected to fully selected object lines

	  Mode = 2 (Copy objects)

	      Select netlabels connected to selected wires/busses
	?     Select bus connections connected to selected wires/busses
	?     Select global connections connected to selected wires/busses
	      Select wires/busses connected to selected netlabels
	?     Select wires/busses connected to selected global connections
	?     Select wires/busses connected to selected bus connections
	      Select object lines to fully
	      Select busses to fully
	      Select wires to fully
	      Select instances to fully

	*/

	InstanceRecord *Instance;
	ObjectRecord *Object;
	ObjectLineRecord *ObjectLine, *ObjectLine2;
	WireRecord *Wire, *Wire2;
	BusRecord *Bus, *Bus2;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	NetLabelRecord *NetLabel;
	int32 Change;

	StartDrawingEditingWindow();

	if ((Mode == 1) && (!EditingSymbol))
	{

// ***************************************************************************************
// ***************************************************************************************
// Select wires/busses connected to instances

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			CalcObjects = 0;

			if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if ((Instance->Info & 4) == 0)
				{
					Instance->Info &= ~(OBJECT_SELECTED | 7);
					Found = 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			CalcObjects = 0;

			if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				InstanceMinX = Instance->BoardPosMinX - 0.2;
				InstanceMaxX = Instance->BoardPosMaxX + 0.2;
				InstanceMinY = Instance->BoardPosMinY - 0.2;
				InstanceMaxY = Instance->BoardPosMaxY + 0.2;
#ifdef _DEBUG

				if (stricmp(Instance->Reference, "t25") == 0)
					ok = 1;

#endif

				for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
				{
					Wire = &((*Wires)[cnt3]);

					if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if ((min(Wire->X1, Wire->X2) < InstanceMaxX) && (max(Wire->X1, Wire->X2) > InstanceMinX)
						        && (min(Wire->Y1, Wire->Y2) < InstanceMaxY) && (max(Wire->Y1, Wire->Y2) > InstanceMinY))
						{
							if (CalcObjects == 0)
							{
								NrObjects = 0;
								InstanceToObject(Instance, 0.0, 0.0, 1);
								CalcObjects = 1;
							}

							cnt2 = 0;

							while (cnt2 < NrObjects)
							{
								Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

								if (stricmp(Instance->Reference, "t25") == 0)
								{
									ok = 1;

									if ((InRange(Object->x1, 74.0)) && (InRange(Object->y1, 38.0)))
										ok = 1;
								}

#endif

								if ((InRange(Object->x1, Wire->X1)) && (InRange(Object->y1, Wire->Y1)))
								{
									Wire->Info &= ~3;
									Wire->Info |= OBJECT_SELECTED | 1;
									cnt2 = NrObjects;
									DrawWire(Wire, 0.0, 0.0, 0);
								}
								else
								{
									if ((InRange(Object->x1, Wire->X2)) && (InRange(Object->y1, Wire->Y2)))
									{
										Wire->Info &= ~3;
										Wire->Info |= OBJECT_SELECTED | 2;
										cnt2 = NrObjects;
										DrawWire(Wire, 0.0, 0.0, 0);
									}
								}

								cnt2++;
							}
						}
					}
				}

				for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
				{
					Bus = &((*Busses)[cnt3]);

					if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if ((min(Bus->X1, Bus->X2) < InstanceMaxX) && (max(Bus->X1, Bus->X2) > InstanceMinX)
						        && (min(Bus->Y1, Bus->Y2) < InstanceMaxY) && (max(Bus->Y1, Bus->Y2) > InstanceMinY))
						{
							if (CalcObjects == 0)
							{
								NrObjects = 0;
								InstanceToObject(Instance, 0.0, 0.0, 1);
								CalcObjects = 1;
							}

							cnt2 = 0;

							while (cnt2 < NrObjects)
							{
								Object = &((*Objects)[cnt2]);

								if ((InRange(Object->x1, Bus->X1)) && (InRange(Object->y1, Bus->Y1)))
								{
									Bus->Info &= ~3;
									Bus->Info |= OBJECT_SELECTED | 1;
									cnt2 = NrObjects;
									DrawBus(Bus, 0.0, 0.0, 0);
								}
								else
								{
									if ((InRange(Object->x1, Bus->X2)) && (InRange(Object->y1, Bus->Y2)))
									{
										Bus->Info &= ~3;
										Bus->Info |= OBJECT_SELECTED | 2;
										cnt2 = NrObjects;
										DrawBus(Bus, 0.0, 0.0, 0);
									}
								}

								cnt2++;
							}
						}
					}
				}
			}
		}

// ***************************************************************************************
// ***************************************************************************************
// Select wires connected to fully selected wires

		for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
		{
			Wire = &((*Wires)[cnt3]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 3)) == (OBJECT_SELECTED | 3))
			{
				for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
				{
					if (cnt2 != cnt3)
					{
						Wire2 = &((*Wires)[cnt2]);

						if ((Wire2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
						{
							if ((InRange(Wire->X1, Wire2->X1)) && (InRange(Wire->Y1, Wire2->Y1)))
							{
								Wire2->Info &= ~3;
								Wire2->Info |= OBJECT_SELECTED | 1;
								DrawWire(Wire2, 0.0, 0.0, 0);
							}
							else
							{
								if ((InRange(Wire->X1, Wire2->X2)) && (InRange(Wire->Y1, Wire2->Y2)))
								{
									Wire2->Info &= ~3;
									Wire2->Info |= OBJECT_SELECTED | 2;
									DrawWire(Wire2, 0.0, 0.0, 0);
								}
							}
						}
					}
				}

				for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
				{
					if (cnt2 != cnt3)
					{
						Wire2 = &((*Wires)[cnt2]);

						if ((Wire2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
						{
							if ((InRange(Wire->X2, Wire2->X1)) && (InRange(Wire->Y2, Wire2->Y1)))
							{
								Wire2->Info &= ~3;
								Wire2->Info |= OBJECT_SELECTED | 1;
								DrawWire(Wire2, 0.0, 0.0, 0);
							}
							else
							{
								if ((InRange(Wire->X2, Wire2->X2)) && (InRange(Wire->Y2, Wire2->Y2)))
								{
									Wire2->Info &= ~3;
									Wire2->Info |= OBJECT_SELECTED | 2;
									DrawWire(Wire2, 0.0, 0.0, 0);
								}
							}
						}
					}
				}
			}
		}

// ***************************************************************************************
// ***************************************************************************************
// Select busses connected to fully selected busses

		for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
		{
			Bus = &((*Busses)[cnt3]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 3)) == (OBJECT_SELECTED | 3))
			{
				for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
				{
					if (cnt2 != cnt3)
					{
						Bus2 = &((*Busses)[cnt2]);

						if ((Bus2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
						{
							if ((InRange(Bus->X1, Bus2->X1)) && (InRange(Bus->Y1, Bus2->Y1)))
							{
								Bus2->Info &= ~3;
								Bus2->Info |= OBJECT_SELECTED | 1;
								DrawBus(Bus2, 0.0, 0.0, 0);
							}
							else
							{
								if ((InRange(Bus->X1, Bus2->X2)) && (InRange(Bus->Y1, Bus2->Y2)))
								{
									Bus2->Info &= ~3;
									Bus2->Info |= OBJECT_SELECTED | 2;
									DrawBus(Bus2, 0.0, 0.0, 0);
								}
							}
						}
					}
				}

				for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
				{
					if (cnt2 != cnt3)
					{
						Bus2 = &((*Busses)[cnt2]);

						if ((Bus2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
						{
							if ((InRange(Bus->X2, Bus2->X1)) && (InRange(Bus->Y2, Bus2->Y1)))
							{
								Bus2->Info &= ~3;
								Bus2->Info |= OBJECT_SELECTED | 1;
								DrawBus(Bus2, 0.0, 0.0, 0);
							}
							else
							{
								if ((InRange(Bus->X2, Bus2->X2)) && (InRange(Bus->Y2, Bus2->Y2)))
								{
									Bus2->Info &= ~3;
									Bus2->Info |= OBJECT_SELECTED | 2;
									DrawBus(Bus2, 0.0, 0.0, 0);
								}
							}
						}
					}
				}
			}
		}

// ***************************************************************************************
// ***************************************************************************************
// Select bus connections connected to fully selected busses

		for (cnt = 0; cnt < Design.NrBusses; cnt++)
		{
			Bus = &((*Busses)[cnt]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 3)) == (OBJECT_SELECTED | 3))
			{
				for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
				{
					BusConnection = &((*BusConnections)[cnt3]);

					if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
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

						if (InRange(Bus->X1, Bus->X2))
						{	// Vertical
							if (InRange(Bus->X1, x2))
							{
								BusConnection->Info |= OBJECT_SELECTED | 3;
								DrawBusConnection(BusConnection, 0.0, 0.0, 0);
							}
						}

						if (InRange(Bus->Y1, Bus->Y2))
						{	// Horizontal
							if (InRange(Bus->Y1, y2))
							{
								BusConnection->Info |= OBJECT_SELECTED | 3;
								DrawBusConnection(BusConnection, 0.0, 0.0, 0);
							}
						}
					}
				}
			}
		}

// ***************************************************************************************
// ***************************************************************************************
// Select wires/busses on one side connected to selected bus connections

		for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
		{
			BusConnection = &((*BusConnections)[cnt3]);

			if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
			{
				CompX = BusConnection->X;
				CompY = BusConnection->Y;

				for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
				{
					Wire = &((*Wires)[cnt2]);

					if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if ((InRange(CompX, Wire->X1)) && (InRange(CompY, Wire->Y1)))
						{
							Wire->Info &= ~3;
							Wire->Info |= OBJECT_SELECTED | 1;
							DrawWire(Wire, 0.0, 0.0, 0);
						}
						else
						{
							if ((InRange(CompX, Wire->X2)) && (InRange(CompY, Wire->Y2)))
							{
								Wire->Info &= ~3;
								Wire->Info |= OBJECT_SELECTED | 2;
								DrawWire(Wire, 0.0, 0.0, 0);
							}
						}
					}
				}

				for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
				{
					Bus = &((*Busses)[cnt2]);

					if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if ((InRange(CompX, Bus->X1)) && (InRange(CompY, Bus->Y1)))
						{
							Bus->Info &= ~3;
							Bus->Info |= OBJECT_SELECTED | 1;
							DrawBus(Bus, 0.0, 0.0, 0);
						}
						else
						{
							if ((InRange(CompX, Bus->X2)) && (InRange(CompY, Bus->Y2)))
							{
								Bus->Info &= ~3;
								Bus->Info |= OBJECT_SELECTED | 2;
								DrawBus(Bus, 0.0, 0.0, 0);
							}
						}
					}
				}
			}
		}

// ***************************************************************************************
// ***************************************************************************************
// Select wires/busses on one side connected to global connections

		for (cnt3 = 0; cnt3 < Design.NrGlobalConnections; cnt3++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt3]);

			if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				CompX = GlobalConnection->X;
				CompY = GlobalConnection->Y;

				for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
				{
					Wire = &((*Wires)[cnt2]);

					if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if ((InRange(CompX, Wire->X1)) && (InRange(CompY, Wire->Y1)))
						{
							Wire->Info &= ~3;
							Wire->Info |= OBJECT_SELECTED | 1;
							DrawWire(Wire, 0.0, 0.0, 0);
						}
						else
						{
							if ((InRange(CompX, Wire->X2)) && (InRange(CompY, Wire->Y2)))
							{
								Wire->Info &= ~3;
								Wire->Info |= OBJECT_SELECTED | 2;
								DrawWire(Wire, 0.0, 0.0, 0);
							}
						}
					}
				}

				for (cnt2 = 0; cnt2 < Design.NrBusses; cnt2++)
				{
					Bus = &((*Busses)[cnt2]);

					if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
					{
						if ((InRange(CompX, Bus->X1)) && (InRange(CompY, Bus->Y1)))
						{
							Bus->Info &= ~3;
							Bus->Info |= OBJECT_SELECTED | 1;
							DrawBus(Bus, 0.0, 0.0, 0);
						}
						else
						{
							if ((InRange(CompX, Bus->X2)) && (InRange(CompY, Bus->Y2)))
							{
								Bus->Info &= ~3;
								Bus->Info |= OBJECT_SELECTED | 2;
								DrawBus(Bus, 0.0, 0.0, 0);
							}
						}
					}
				}
			}
		}


// ***************************************************************************************
// ***************************************************************************************
// Select wires/busses on one side connected to selected netlabels

		for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
		{
			Wire = &((*Wires)[cnt3]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
			{
				for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
				{
					NetLabel = &((*NetLabels)[cnt2]);
					Change = 0;

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((Wire->Info & 3) == 3)
						{
							if (((InRange(Wire->X1, NetLabel->ConnectX)) && (InRange(Wire->Y1, NetLabel->ConnectY)))
							        || ((InRange(Wire->X2, NetLabel->ConnectX)) && (InRange(Wire->Y2, NetLabel->ConnectY))))
								Change = 1;
						}

						if (((Wire->Info & 3) == 1) && (InRange(Wire->X1, NetLabel->ConnectX))
						        && (InRange(Wire->Y1, NetLabel->ConnectY)))
							Change = 1;

						if (((Wire->Info & 3) == 2) && (InRange(Wire->X2, NetLabel->ConnectX))
						        && (InRange(Wire->Y2, NetLabel->ConnectY)))
							Change = 1;
					}

					if (Change)
					{
						NetLabel->Info |= OBJECT_SELECTED | 3;
						DrawNetLabel(NetLabel, 0.0, 0.0, 0);
					}
				}
			}
		}

// ***************************************************************************************
// ***************************************************************************************
// Select netlabels connected to partially selected wires

		for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
		{
			Wire = &((*Wires)[cnt3]);

			if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
			{
				for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
				{
					NetLabel = &((*NetLabels)[cnt2]);
					Change = 0;

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((Wire->Info & 3) == 3)
						{
							if (((InRange(Wire->X1, NetLabel->ConnectX)) && (InRange(Wire->Y1, NetLabel->ConnectY)))
							        || ((InRange(Wire->X2, NetLabel->ConnectX)) && (InRange(Wire->Y2, NetLabel->ConnectY))))
								Change = 1;
						}

						if (((Wire->Info & 3) == 1) && (InRange(Wire->X1, NetLabel->ConnectX))
						        && (InRange(Wire->Y1, NetLabel->ConnectY)))
							Change = 1;

						if (((Wire->Info & 3) == 2) && (InRange(Wire->X2, NetLabel->ConnectX))
						        && (InRange(Wire->Y2, NetLabel->ConnectY)))
							Change = 1;
					}

					if (Change)
					{
						NetLabel->Info |= OBJECT_SELECTED | 3;
						DrawNetLabel(NetLabel, 0.0, 0.0, 0);
					}
				}
			}
		}

// ***************************************************************************************
// ***************************************************************************************
// Select netlabels connected to partially selected busses

		for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
		{
			Bus = &((*Busses)[cnt3]);

			if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
			{
				for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
				{
					NetLabel = &((*NetLabels)[cnt2]);
					Change = 0;

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((Bus->Info & 3) == 3)
						{
							if (((InRange(Bus->X1, NetLabel->ConnectX)) && (InRange(Bus->Y1, NetLabel->ConnectY)))
							        || ((InRange(Bus->X2, NetLabel->ConnectX)) && (InRange(Bus->Y2, NetLabel->ConnectY))))
								Change = 1;
						}

						if (((Bus->Info & 3) == 1) && (InRange(Bus->X1, NetLabel->ConnectX))
						        && (InRange(Bus->Y1, NetLabel->ConnectY)))
							Change = 1;

						if (((Bus->Info & 3) == 2) && (InRange(Bus->X2, NetLabel->ConnectX))
						        && (InRange(Bus->Y2, NetLabel->ConnectY)))
							Change = 1;
					}

					if (Change)
					{
						NetLabel->Info |= OBJECT_SELECTED | 3;
						DrawNetLabel(NetLabel, 0.0, 0.0, 0);
					}
				}
			}
		}
	}


// ***************************************************************************************
// ***************************************************************************************
// Select netlabels connected to selected wires

	if (Mode != 1)
	{
		if (!EditingSymbol)
		{
			for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
			{
				Wire = &((*Wires)[cnt3]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
					{
						NetLabel = &((*NetLabels)[cnt2]);

						if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
						{
							if (((InRange(Wire->X1, NetLabel->ConnectX)) && (InRange(Wire->Y1, NetLabel->ConnectY)))
							        || ((InRange(Wire->X2, NetLabel->ConnectX)) && (InRange(Wire->Y2, NetLabel->ConnectY))))
							{
								NetLabel->Info |= OBJECT_SELECTED | 3;
								DrawNetLabel(NetLabel, 0.0, 0.0, 0);
							}
						}
					}
				}
			}

// ***************************************************************************************
// ***************************************************************************************
// Select netlabels connected to selected busses

			for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
			{
				Bus = &((*Busses)[cnt3]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
				{
					for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
					{
						NetLabel = &((*NetLabels)[cnt2]);
						Change = 0;

						if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
						{
							if ((Bus->Info & 3) == 3)
							{
								if (((InRange(Bus->X1, NetLabel->ConnectX)) && (InRange(Bus->Y1, NetLabel->ConnectY)))
								        || ((InRange(Bus->X2, NetLabel->ConnectX))
								            && (InRange(Bus->Y2, NetLabel->ConnectY))))
									Change = 1;
							}

							if (((Bus->Info & 3) == 1) && (InRange(Bus->X1, NetLabel->ConnectX))
							        && (InRange(Bus->Y1, NetLabel->ConnectY)))
								Change = 1;

							if (((Bus->Info & 3) == 2) && (InRange(Bus->X2, NetLabel->ConnectX))
							        && (InRange(Bus->Y2, NetLabel->ConnectY)))
								Change = 1;
						}

						if (Change)
						{
							NetLabel->Info |= OBJECT_SELECTED | 3;
							DrawNetLabel(NetLabel, 0.0, 0.0, 0);
						}
					}
				}
			}

// ***************************************************************************************
// ***************************************************************************************
// Select wires/busses connected to selected netlabels

			if (Mode == 2)
			{
				for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
				{
					NetLabel = &((*NetLabels)[cnt2]);
					Change = 0;

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						x3 = NetLabel->ConnectX;
						y3 = NetLabel->ConnectY;

						for (cnt = 0; cnt < Design.NrWires; cnt++)
						{
							Wire = &((*Wires)[cnt]);

							if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
							{
								if (((InRange(Wire->X1, x3)) && (InRange(Wire->Y1, y3)))
								        || ((InRange(Wire->X2, x3)) && (InRange(Wire->Y2, y3))))
								{
									Wire->Info |= OBJECT_SELECTED | 3;
									DrawWire(Wire, 0.0, 0.0, 0);
									Change = 1;
								}
							}
						}

						for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
						{
							Bus = &((*Busses)[cnt3]);

							if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
							{
								if (((InRange(Bus->X1, x3)) && (InRange(Bus->Y1, y3)))
								        || ((InRange(Bus->X2, x3)) && (InRange(Bus->Y2, y3))))
								{
									Bus->Info |= OBJECT_SELECTED | 3;
									DrawBus(Bus, 0.0, 0.0, 0);
									Change = 1;
								}
							}
						}

						if (Change)
						{
							NetLabel->Info |= OBJECT_SELECTED | 3;
							DrawNetLabel(NetLabel, 0.0, 0.0, 0);
						}
					}
				}
			}
		}
	}

// ***************************************************************************************
// ***************************************************************************************
// Select object lines connected to fully selected object lines

	if (Mode == 1)
	{
		for (cnt3 = 0; cnt3 < Design.NrObjectLines; cnt3++)
		{
			ObjectLine = &((*ObjectLines)[cnt3]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 3)) == (OBJECT_SELECTED | 3))
			{
				// ***************************************************************************************
				for (cnt2 = 0; cnt2 < Design.NrObjectLines; cnt2++)
				{
					if (cnt2 != cnt3)
					{
						ObjectLine2 = &((*ObjectLines)[cnt2]);

						if ((ObjectLine2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
						{
							if ((InRange(ObjectLine->X1, ObjectLine2->X1))
							        && (InRange(ObjectLine->Y1, ObjectLine2->Y1)))
							{
								ObjectLine2->Info &= ~3;
								ObjectLine2->Info |= OBJECT_SELECTED | 1;
								DrawObjectLine(ObjectLine2, 0.0, 0.0, 0);
							}
							else
							{
								if ((InRange(ObjectLine->X1, ObjectLine2->X2))
								        && (InRange(ObjectLine->Y1, ObjectLine2->Y2)))
								{
									ObjectLine2->Info &= ~3;
									ObjectLine2->Info |= OBJECT_SELECTED | 2;
									DrawObjectLine(ObjectLine2, 0.0, 0.0, 0);
								}
							}
						}
					}
				}

				for (cnt2 = 0; cnt2 < Design.NrObjectLines; cnt2++)
				{
					if (cnt2 != cnt3)
					{
						ObjectLine2 = &((*ObjectLines)[cnt2]);

						if ((ObjectLine2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0)
						{
							if ((InRange(ObjectLine->X2, ObjectLine2->X1))
							        && (InRange(ObjectLine->Y2, ObjectLine2->Y1)))
							{
								ObjectLine2->Info &= ~3;
								ObjectLine2->Info |= OBJECT_SELECTED | 1;
								DrawObjectLine(ObjectLine2, 0.0, 0.0, 0);
							}
							else
							{
								if ((InRange(ObjectLine->X2, ObjectLine2->X2))
								        && (InRange(ObjectLine->Y2, ObjectLine2->Y2)))
								{
									ObjectLine2->Info &= ~3;
									ObjectLine2->Info |= OBJECT_SELECTED | 2;
									DrawObjectLine(ObjectLine2, 0.0, 0.0, 0);
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
// Select object lines to fully

	if (Mode != 1)
	{
		for (cnt2 = 0; cnt2 < Design.NrObjectLines; cnt2++)
		{
			ObjectLine2 = &((*ObjectLines)[cnt2]);

			if ((ObjectLine2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectLine2->Info |= 3;
				DrawObjectLine(ObjectLine2, 0.0, 0.0, 0);
			}
		}
	}

// ***************************************************************************************
// ***************************************************************************************
// Select busses to fully

	if (Mode != 1)
	{
		if (!EditingSymbol)
		{
			for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
			{
				Bus = &((*Busses)[cnt3]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
				{
					Bus->Info |= 3;
					DrawBus(Bus, 0.0, 0.0, 0);
				}
			}
		}
	}

// ***************************************************************************************
// ***************************************************************************************
// Select wires to fully

	if (Mode != 1)
	{
		if (!EditingSymbol)
		{
			for (cnt2 = 0; cnt2 < Design.NrWires; cnt2++)
			{
				Wire = &((*Wires)[cnt2]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Wire->Info |= 3;
					DrawWire(Wire, 0.0, 0.0, 0);
				}
			}
		}
	}

// ***************************************************************************************
// ***************************************************************************************
// Select instances to fully (only move)

	if (Mode == 2)
	{
		if (!EditingSymbol)
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Instance->Info |= 7;
					Found = 1;
				}
			}
		}
	}

// ***************************************************************************************
// ***************************************************************************************

	ExitDrawing();
	EndDrawingEditingWindow();

	if (Found)
		RePaint();

	return Found;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ChangeSelections(int32 Mode, int32 Selected)
// Mode 0 -> unselect
// Mode 1 -> select only
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

	if (!EditingSymbol)
	{
		if (((Mode == 0) && (Selected == SELECTIONS_WIRES)) || ((Mode == 1) && (Selected != SELECTIONS_WIRES)))
		{
			for (cnt = 0; cnt < Design.NrWires; cnt++)
			{
				Wire = &((*Wires)[cnt]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Wire->Info &= ~OBJECT_SELECTED;

					if (ViewMode == 1)
						GetNetNrSelectedWiresObjects5(Wire, UNSELECT_NETNR);

					DrawWire(Wire, 0.0, 0.0, 0);
				}
			}
		}

		if (((Mode == 0) && (Selected == SELECTIONS_BUSSES)) || ((Mode == 1) && (Selected != SELECTIONS_BUSSES)))
		{
			for (cnt = 0; cnt < Design.NrBusses; cnt++)
			{
				Bus = &((*Busses)[cnt]);

				if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Bus->Info &= ~OBJECT_SELECTED;
					DrawBus(Bus, 0.0, 0.0, 0);

					if (ViewMode == 1)
						GetNetNrSelectedBussesObjects5(Bus, UNSELECT_NETNR);
				}
			}
		}

		if (((Mode == 0) && (Selected == SELECTIONS_JUNCTIONS)) || ((Mode == 1) && (Selected != SELECTIONS_JUNCTIONS)))
		{
			for (cnt = 0; cnt < Design.NrJunctions; cnt++)
			{
				Junction = &((*Junctions)[cnt]);

				if ((Junction->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Junction->Info &= ~OBJECT_SELECTED;
					DrawJunction(Junction, 0.0, 0.0, 0);
				}
			}
		}

		if (((Mode == 0) && (Selected == SELECTIONS_ONEPINNETS))
		        || ((Mode == 1) && (Selected != SELECTIONS_ONEPINNETS)))
		{
			for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
			{
				OnePinNet = &((*OnePinNets)[cnt]);

				if ((OnePinNet->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					OnePinNet->Info &= ~OBJECT_SELECTED;
					DrawOnePinNet(OnePinNet, 0.0, 0.0, 0);
				}
			}
		}

		if (((Mode == 0) && (Selected == SELECTIONS_BUSCONN)) || ((Mode == 1) && (Selected != SELECTIONS_BUSCONN)))
		{
			for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
			{
				BusConnection = &((*BusConnections)[cnt]);

				if ((BusConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					BusConnection->Info &= ~OBJECT_SELECTED;
					DrawBusConnection(BusConnection, 0.0, 0.0, 0);
				}
			}
		}

		if (Mode == 0)
		{
			if (Selected == SELECTIONS_GLOBALCONN)
			{
				for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
				{
					GlobalConnection = &((*GlobalConnections)[cnt]);

					if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						GlobalConnection->Info &= ~OBJECT_SELECTED;
						DrawGlobalConnection(GlobalConnection, 0.0, 0.0, 0);
					}
				}
			}
		}

		if (Mode == 1)
		{
			if ((Selected != SELECTIONS_GLOBALCONN) && (Selected != SELECTIONS_GLOBALCONN_TEXT))
			{
				for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
				{
					GlobalConnection = &((*GlobalConnections)[cnt]);

					if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						GlobalConnection->Info &= ~(OBJECT_SELECTED | 3);
						DrawGlobalConnection(GlobalConnection, 0.0, 0.0, 0);
					}
				}
			}

			if ((Selected != SELECTIONS_GLOBALCONN) && (Selected == SELECTIONS_GLOBALCONN_TEXT))
			{

				for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
				{
					GlobalConnection = &((*GlobalConnections)[cnt]);

					if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						GlobalConnection->Info &= ~1;
						DrawGlobalConnection(GlobalConnection, 0.0, 0.0, 0);
					}
				}
			}
		}

		if ((Design.SheetInfo & 1) == 0)
		{
			if (((Mode == 0) && (Selected == SELECTIONS_NETLABELS))
			        || ((Mode == 1) && (Selected != SELECTIONS_NETLABELS)))
			{
				for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
				{
					NetLabel = &((*NetLabels)[cnt]);

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						NetLabel->Info &= ~OBJECT_SELECTED;
						DrawNetLabel(NetLabel, 0.0, 0.0, 0);
					}
				}
			}
		}
	}
	else
	{

		if (((Mode == 0) && (Selected == SELECTIONS_POWERPINS)) || ((Mode == 1) && (Selected != SELECTIONS_POWERPINS)))
		{
			for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
			{
				PowerPin = &((*PowerPins)[cnt]);

				if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					PowerPin->Info &= ~(OBJECT_SELECTED | 3);
					DrawPowerPin(PowerPin, 0.0, 0.0, 0);
				}
			}
		}

		if (Mode == 0)
		{
			if (Selected == SELECTIONS_PINS)
			{
				for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
				{
					Pin = &((*Pins)[cnt]);

					if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						Pin->Info &= ~(OBJECT_SELECTED | 3);
						DrawPin(Pin, 0.0, 0.0, 0);
					}
				}
			}

			if (Selected == SELECTIONS_PINBUSSES)
			{
				for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
				{
					PinBus = &((*PinBusses)[cnt]);

					if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						PinBus->Info &= ~(OBJECT_SELECTED | 3);
						DrawPinBus(PinBus, 0.0, 0.0, 0);
					}
				}
			}
		}

		if (Mode == 1)
		{
			if ((Selected != SELECTIONS_PINS) && (Selected != SELECTIONS_PINTEXTS))
			{
				for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
				{
					Pin = &((*Pins)[cnt]);

					if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						Pin->Info &= ~(OBJECT_SELECTED | 3);
						DrawPin(Pin, 0.0, 0.0, 0);
					}
				}
			}

			if ((Selected != SELECTIONS_PINS) && (Selected == SELECTIONS_PINTEXTS))
			{
				for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
				{
					Pin = &((*Pins)[cnt]);

					if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
					{
						Pin->Info &= ~1;
						DrawPin(Pin, 0.0, 0.0, 0);
					}
				}
			}

			if ((Selected != SELECTIONS_PINBUSSES) && (Selected != SELECTIONS_PINTEXTS))
			{
				for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
				{
					PinBus = &((*PinBusses)[cnt]);

					if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						PinBus->Info &= ~(OBJECT_SELECTED | 3);
						DrawPinBus(PinBus, 0.0, 0.0, 0);
					}
				}
			}

			if ((Selected != SELECTIONS_PINBUSSES) && (Selected == SELECTIONS_PINTEXTS))
			{
				for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
				{
					PinBus = &((*PinBusses)[cnt]);

					if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						PinBus->Info &= ~1;
						DrawPinBus(PinBus, 0.0, 0.0, 0);
					}
				}
			}
		}
	}

	if (Mode == 0)
	{
		if (Selected == SELECTIONS_INST)
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Instance->Info &= ~(OBJECT_SELECTED | 7);
					DrawInstance(Instance, 0.0, 0.0, 0);

					if (ViewMode == 1)
						CopyToClipBoardMem(UNSELECT_REF, 0, Instance->Reference);
				}
			}
		}

		if (Selected == SELECTIONS_INST_REF)
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1))
				{
					Instance->Info &= ~1;

					if ((Instance->Info & 3) == 0)
						Instance->Info &= ~(OBJECT_SELECTED | 7);

					DrawInstance(Instance, 0.0, 0.0, 0);
				}
			}
		}

		if (Selected == SELECTIONS_INST_VALUE)
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2))
				{
					Instance->Info &= ~2;

					if ((Instance->Info & 3) == 0)
						Instance->Info &= ~(OBJECT_SELECTED | 7);

					DrawInstance(Instance, 0.0, 0.0, 0);
				}
			}
		}
	}

	if (Mode == 1)
	{
		if ((Selected != SELECTIONS_INST_REF) && (Selected != SELECTIONS_INST_VALUE) && (Selected != SELECTIONS_INST))
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Instance->Info &= ~(OBJECT_SELECTED | 7);
					DrawInstance(Instance, 0.0, 0.0, 0);

					if (ViewMode == 1)
						CopyToClipBoardMem(UNSELECT_REF, 0, Instance->Reference);
				}
			}
		}

		if ((Selected != SELECTIONS_INST_REF) && (Selected != SELECTIONS_INST))
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 1)) == (OBJECT_SELECTED | 1))
				{
					Instance->Info &= ~(4 + 1);

					if ((Instance->Info & 3) == 0)
						Instance->Info &= ~(OBJECT_SELECTED | 7);

					DrawInstance(Instance, 0.0, 0.0, 0);
				}
			}
		}

		if ((Selected != SELECTIONS_INST_VALUE) && (Selected != SELECTIONS_INST))
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | 2)) == (OBJECT_SELECTED | 2))
				{
					Instance->Info &= ~(4 + 2);

					if ((Instance->Info & 3) == 0)
						Instance->Info &= ~(OBJECT_SELECTED | 7);

					DrawInstance(Instance, 0.0, 0.0, 0);
				}
			}
		}
	}


	if (((Mode == 0) && (Selected == SELECTIONS_LINES)) || ((Mode == 1) && (Selected != SELECTIONS_LINES)))
	{
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectLine->Info &= ~OBJECT_SELECTED;
				DrawObjectLine(ObjectLine, 0.0, 0.0, 0);
			}
		}
	}

	if (((Mode == 0) && (Selected == SELECTIONS_RECTS)) || ((Mode == 1) && (Selected != SELECTIONS_RECTS)))
	{
		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectRect->Info &= ~OBJECT_SELECTED;
				DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
			}
		}
	}

	if (((Mode == 0) && (Selected == SELECTIONS_CIRCLES)) || ((Mode == 1) && (Selected != SELECTIONS_CIRCLES)))
	{
		for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
		{
			ObjectCircle = &((*ObjectCircles)[cnt]);

			if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectCircle->Info &= ~OBJECT_SELECTED;
				DrawObjectCircle(ObjectCircle, 0.0, 0.0, 0);
			}
		}
	}

	if (((Mode == 0) && (Selected == SELECTIONS_ARCS)) || ((Mode == 1) && (Selected != SELECTIONS_ARCS)))
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectArc->Info &= ~OBJECT_SELECTED;
				DrawObjectArc(ObjectArc, 0.0, 0.0, 0);
			}
		}
	}

	if (((Mode == 0) && (Selected == SELECTIONS_TEXTS)) || ((Mode == 1) && (Selected != SELECTIONS_TEXTS)))
	{
		for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
		{
			ObjectText = &((*ObjectTexts)[cnt]);

			if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectText->Info &= ~OBJECT_SELECTED;
				DrawObjectText(ObjectText, 0.0, 0.0, 0);
			}
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow();

	if ((ViewMode == 1) && (!EditingSymbol) && (ClipBoardMemPos != 0))
	{
//    memmove(&Buf,ClipBoardMem,400);
//    memset(&Buf2,0,sizeof(Buf2));
//    memmove(&Buf2,ClipBoardMem,ClipBoardMemPos);
		CopySelectionsToClipBoard();
	}
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ExportText(int32 mode)
{

	int32 cnt, cnt2, count, lengte, NrLines;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;
	ObjectTextRecord *ObjectText;

	char str2[MAX_LENGTH_STRING];
	char PinBusStrings[16][MAX_LENGTH_STRING], Message[16384];
	int32 LinePos[16];


	count = 0;
	memset(&Message, 0, sizeof(Message));

	for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
	{
		Pin = &((*Pins)[cnt]);

		if ((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			sprintf(str2, "%s\t%s\r\n", Pin->Name, Pin->Label);

			if (strlen(Message) < 16000)
				strcat(Message, str2);
		}
	}

	strcat(Message, "\r\n");

	for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
	{
		PowerPin = &((*PowerPins)[cnt]);

		if ((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			sprintf(str2, "%s\t%s\r\n", PowerPin->NetName, PowerPin->Text);

			if (strlen(Message) < 16000)
				strcat(Message, str2);
		}
	}

	strcat(Message, "\r\n");

	for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
	{
		PinBus = &((*PinBusses)[cnt]);

		if ((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
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

				if (strlen(Message) < 16000)
					strcat(Message, PinBusStrings[cnt2]);

				strcat(Message, "\r\n");
			}

		}
	}

	strcat(Message, "\r\n");

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (strlen(Message) < 16000)
				strcat(Message, ObjectText->Text);

			strcat(Message, "\r\n");
		}
	}

	MessageDialog(Message);

}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetFirstSymbolSheetSelected()
{
	int32 cnt, InstanceInfo;
	InstanceRecord *Instance;

	cnt = 0;

	while (cnt < Design.NrInstances)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);
		InstanceInfo = Instance->Info;

		if ((InstanceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | SHEET_SYMBOL)) == (OBJECT_SELECTED | SHEET_SYMBOL))
			return cnt;

		cnt++;
	}

	return -1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetNetNameByNr(int32 NetNr, LPSTR NetName)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	int32 fp, Length, cnt;

	sprintf(str, "%s\\net.nr", DesignPath);

	if ((fp = TextFileOpenUTF8(str)) < 0)
		return -1;

	while ((Length = ReadLn(fp, str2)) >= 0)
	{
		str2[Length] = 0;

		if (Length > 1)
		{
			if ((sscanf(str2, "%i %s", &cnt, str3) == 2) && (cnt == NetNr))
			{
				TextFileClose(fp);
				strcpy(NetName, str3);
				return 0;
			}
		}
	}

	TextFileClose(fp);
	return -2;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetNetNrSelectedWiresObjects5(WireRecord * Wire, int32 mode)
{
	int32 cnt, cnt2, Object5Pos, res;
	Object5Record *Object5;
	char NetName[128];

	Object5Pos = -1;
	cnt = 0;

	while ((cnt < NrObjects5) && (Object5Pos == -1))
	{
		Object5 = &((*Objects5)[cnt]);

		if (Object5->ObjectType == WIRE)
		{
			if ((InRange(Object5->x1, Wire->X1)) && (InRange(Object5->y1, Wire->Y1)) && (InRange(Object5->x2, Wire->X2))
			        && (InRange(Object5->y2, Wire->Y2)))
				Object5Pos = Object5->FirstObjectNr;
		}

		cnt++;
	}

	if (Object5Pos == -1)
		return -1;

	for (cnt2 = Object5Pos; cnt2 < NrObjects5; cnt2++)
	{
		Object5 = &((*Objects5)[cnt2]);

		if ((Object5->FirstObjectNr == Object5Pos) && (Object5->ObjectType == WIRE))
		{
			if ((res = GetNetNameByNr(Object5->NetNr, NetName)) == 0)
				CopyToClipBoardMem(mode, 0, NetName);
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetNetNrSelectedBussesObjects5(BusRecord * Bus, int32 mode)
{
	int32 cnt, cnt2, Object5Pos, res;
	Object5Record *Object5;
	char NetName[128];
	Object5Record *Object5a[1024];

	for (cnt = 0; cnt < min(1024, NrObjects5); cnt++)
		Object5a[cnt] = Object5 = &((*Objects5)[cnt]);

	Object5Pos = -1;
	cnt = 0;

	while ((cnt < NrObjects5) && (Object5Pos == -1))
	{
		Object5 = &((*Objects5)[cnt]);

		if (Object5->ObjectType == BUS)
		{
			if ((InRange(Object5->x1, Bus->X1)) && (InRange(Object5->y1, Bus->Y1)) && (InRange(Object5->x2, Bus->X2))
			        && (InRange(Object5->y2, Bus->Y2)))
				Object5Pos = Object5->FirstObjectNr;
		}

		cnt++;
	}

	if (Object5Pos == -1)
		return -1;

	for (cnt2 = Object5Pos; cnt2 < NrObjects5; cnt2++)
	{
		Object5 = &((*Objects5)[cnt2]);

		if ((Object5->FirstObjectNr == Object5Pos) && (Object5->ObjectType == BUS_INFO))
		{
			if ((res = GetNetNameByNr(Object5->NetNr, NetName)) == 0)
				CopyToClipBoardMem(mode, 0, NetName);
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void SearchText(int32 mode)
{
	char PinName[30];
	int32 cnt, cnt2, res, Found, Changed, SkipSearchCount = 0;

	NetLabelRecord *NetLabel;
	ObjectTextRecord *ObjectText, NewObjectText;
	InstanceRecord *Instance;
	GlobalConnectionRecord *GlobalConnection;
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;

	if (SearchTextStr[0] == 0)
		mode = 0;

	if (mode == 0)
	{
		strcpy(NewObjectText.Text, SearchTextStr);

		if (TextInputDialog(&NewObjectText, 6) == 2)
			return;

		if (strlen(NewObjectText.Text) == 0)
			return;

		SearchTextCount = 1;
	}

	if (mode == 1)
		SearchTextCount++;

	UnselectAll = 1;
	ClipBoardMemPos = 0;
	SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
	LastAction = 0;
	UnselectAll = 0;

	if (mode == 0)
	{
		strcpy(SearchTextStr, NewObjectText.Text);
		strcpy(SearchCodeString, SearchTextStr);
	}

	if (CenterScreenOnInstance(2) == 0)
		return;

again:
	Found = -1;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = &((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
		{
			if ((EditingSymbol) || ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == 0))
			{
				if (stricmpUTF8(Instance->Value, SearchTextStr) == 0)
				{
					SkipSearchCount++;

					if (SkipSearchCount == SearchTextCount)
					{
						Instance->Info |= OBJECT_SELECTED | 2;

						if (GetMinMaxInstanceValueText(Instance) == 0)
						{
							CenterScreen((TextMinX + TextMaxX) / 2, (TextMinY + TextMaxY) / 2, 0);
							return;
						}
					}
				}
			}
		}
	}

	if (!EditingSymbol)
	{
		for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
		{
			GlobalConnection = &((*GlobalConnections)[cnt]);

			if ((GlobalConnection->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if (stricmpUTF8(GlobalConnection->Text, SearchTextStr) == 0)
				{
					SkipSearchCount++;

					if (SkipSearchCount == SearchTextCount)
					{
						GlobalConnection->Info |= OBJECT_SELECTED | 3;
						CenterScreen(GlobalConnection->NameX, GlobalConnection->NameY, 0);
						return;
					}
				}
			}
		}

		if ((Design.SheetInfo & 1) == 0)
		{
			for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
			{
				NetLabel = &((*NetLabels)[cnt]);

				if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if (stricmpUTF8(NetLabel->Name, SearchTextStr) == 0)
					{
						SkipSearchCount++;

						if (SkipSearchCount == SearchTextCount)
						{
							NetLabel->Info |= OBJECT_SELECTED | 3;
							CenterScreen(NetLabel->ConnectX + NetLabel->TextX, NetLabel->ConnectY + NetLabel->TextY, 0);
							return;
						}
					}
				}
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
		{
			Pin = &((*Pins)[cnt]);

			if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if (stricmpUTF8(Pin->Name, SearchTextStr) == 0)
				{
					SkipSearchCount++;

					if (SkipSearchCount == SearchTextCount)
					{
						Pin->Info |= OBJECT_SELECTED | 3;
						CenterScreen(Pin->X, Pin->Y, 0);
						return;
					}
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
		{
			PinBus = &((*PinBusses)[cnt]);
			Changed = 0;

			if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				for (cnt2 = 0; cnt2 < PinBus->NrPins; cnt2++)
				{
					if ((res = GetPinNameFromPinBus(PinBus->Text, PinName, PinBus->NrPins, cnt2)) == 0)
					{
						if (stricmpUTF8(PinName, SearchTextStr) == 0)
						{
							SkipSearchCount++;

							if (SkipSearchCount == SearchTextCount)
							{
								PinBus->Info |= OBJECT_SELECTED | 3;
								CenterScreen(PinBus->X, PinBus->Y, 0);
								return;
							}
						}
					}
				}
			}
		}

		for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
		{
			PowerPin = &((*PowerPins)[cnt]);
			Changed = 0;

			if ((PowerPin->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if (stricmpUTF8(PowerPin->NetName, SearchTextStr) == 0)
					Changed = 1;

				if ((res = GetPinNameFromPinBus(PowerPin->Text, PinName, 100, 0)) == 0)
				{
					for (cnt2 = 0; cnt2 < 100; cnt2++)
					{
						if ((res = GetPinNameFromPinBus(PowerPin->Text, PinName, 100, cnt2)) == 0)
						{
							if (stricmpUTF8(PinName, SearchTextStr) == 0)
								Changed = 1;
						}
					}
				}

				if (Changed)
				{
					SkipSearchCount++;

					if (SkipSearchCount == SearchTextCount)
					{
						PowerPin->Info |= OBJECT_SELECTED;
						CenterScreen(PowerPin->NameX, PowerPin->NameY, 0);
						return;
					}
				}
			}
		}
	}


	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			if (stricmpUTF8(ObjectText->Text, SearchTextStr) == 0)
			{
				SkipSearchCount++;

				if (SkipSearchCount == SearchTextCount)
				{
					ObjectText->Info |= OBJECT_SELECTED;
					CenterScreen(ObjectText->X, ObjectText->Y, 0);
					return;
				}
			}
		}
	}

	if (mode == 1)
	{
		SearchTextCount = 1;

		if (SkipSearchCount > 0)
		{
			SkipSearchCount = 0;
			goto again;
		}
	}

	ok = 1;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetInfoStr(LPSTR InfoStr, int32 mode)
{
	double x, y, x1, y1, x2, y2;
	NetLabelRecord *NetLabel;
	InstanceRecord *Instance;
	int32 cnt, cnt2, cnt3, TextAlignment, TextRotation, NrProperties, NrCompProperties;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], PropertyID[MAX_LENGTH_STRING], PropertyValue[MAX_LENGTH_STRING];
//	char str[2048]

	if ((MousePosX == 10000) || (mode == 1))
		return 2;

	x = PixelToRealOffX(MousePosX);
	y = PixelToRealOffY(DrawWindowMaxY - MousePosY);

	if (DisplayInfoCursorX != -1)
	{
		if ((abs(DisplayInfoCursorX - MousePosX) > 5) || (abs(DisplayInfoCursorY - MousePosY) > 5))
		{
			DisplayInfoCursorX = -1;
			DisplayInfoCursorY = -1;
			return 2;
		}

		return 3;
	}

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((x > Instance->BoardPosMinX) && (x < Instance->BoardPosMaxX) && (y > Instance->BoardPosMinY)
			        && (y < Instance->BoardPosMaxY))
			{
				if (((GetMinMaxInstanceReferenceText(Instance) == 0) && (TextMaxX > x) && (TextMinX < x)
				        && (TextMaxY > y) && (TextMinY < y)) || ((GetMinMaxInstanceValueText(Instance) == 0)
				                && (TextMaxX > x) && (TextMinX < x) && (TextMaxY > y)
				                && (TextMinY < y)))
				{

					if (!EditingSymbol)
					{
						sprintf(str, SC(451, "Component"));
					    strcat(str, "\r\n\f\r\n");
					}
					else
					{
						if (!EditingSheetSymbol)
						{
							sprintf(str, SC(186, "Symbol"));
							strcat(str, "\r\n\f\r\n");
						}
						else
						{
							sprintf(str, SC(455, "Sheet symbol"));
							strcat(str, "\r\n\f\r\n");
						}
					}

					if (!EditingSheetSymbol)
					{
						sprintf(str2, SC(495, "reference : %s\r\n"), Instance->Reference);
						strcat(str, str2);
						sprintf(str2, SC(496, "value : %s\r\n"), Instance->Value);
						strcat(str, str2);
					}

					if (!EditingSymbol)
					{
						sprintf(str2, SC(497, "symbol : %s\r\n"), Instance->SymbolName);
						strcat(str, str2);
					}

					if (!EditingSheetSymbol)
					{
						sprintf(str2, SC(498, "geometry : %s\r\n"), Instance->Geometry);
						strcat(str, str2);
					}

					if (!EditingSymbol)
					{
						sprintf(str2, SC(499, "part nr : %s\r\n"), Instance->PartNr);
						strcat(str, str2);
					}

					sprintf(str2, SC(500, "part description : %s\r\n"), Instance->PartDescription);
					strcat(str, str2);

					if (!EditingSymbol)
					{
						NrCompProperties = GetCompProperties(Instance, NULL, NULL, 0x40);

						if (NrCompProperties > 0)
						{
							for (cnt3 = 0; cnt3 < NrCompProperties; cnt3++)
							{
								GetCompProperties(Instance, PropertyID, PropertyValue, 0x20 + cnt3);

								if (PropertyID[0] != '~')
								{
									strcat(str, PropertyID);
									strcat(str, " : ");
									strcat(str, PropertyValue);
									strcat(str, "\r\n");
								}
								else
								{
									sprintf(str2, SC(501, "net %s -> %s\r\n"), (LPSTR) & PropertyID[1], PropertyValue);
									strcat(str, str2);
								}
							}
						}
					}

					strcpy(InfoStr, str);
					DisplayInfoCursorX = MousePosX;
					DisplayInfoCursorY = MousePosY;
					return 1;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
	{
		NetLabel = &((*NetLabels)[cnt]);

		if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = NetLabel->ConnectX;
			y1 = NetLabel->ConnectY;
			x2 = x1 + NetLabel->TextX;
			y2 = y1 + NetLabel->TextY;
			TextAlignment = (NetLabel->Alignment & 0x0f);
			TextRotation = (NetLabel->Alignment >> 8) & 0x01;
			GetMinMaxText(x2, y2, 1.0, 0, TextRotation, TextAlignment, NetLabel->Name);

			if ((TextMaxX > x) && (TextMinX < x) && (TextMaxY > y) && (TextMinY < y))
			{
				NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1);

				if (NrProperties > 0)
				{
					sprintf(str, SC(502, "Netname %s\r\n\f\r\nProperties\r\n\f\r\n"), NetLabel->Name);

					for (cnt2 = 0; cnt2 < NrProperties; cnt2++)
					{
						GetProperty(NetLabel->Name, PropertyID, PropertyValue, cnt2);
						strcat(str, PropertyID);
						strcat(str, " : ");
						strcat(str, PropertyValue);
						strcat(str, "\r\n");
					}

					strcpy(InfoStr, str);
					DisplayInfoCursorX = MousePosX;
					DisplayInfoCursorY = MousePosY;
					return 1;
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
