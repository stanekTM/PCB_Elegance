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
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "menus.h"
#include "pcb.h"
#include "stdio.h"
#include "dialogs.h"
#include "math.h"
#include "rect.h"
#include "time.h"
#include "io.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "calcdef.h"
#include "graphics.h"
#include "toets.h"
#include "mainloop.h"
#include "draw3.h"
#include "draw2.h"
#include "draw.h"
#include "move3.h"
#include "select3.h"
#include "line2.h"
#include "trace2.h"
#include "movecomp.h"
#include "select.h"
#include "ellipss.h"
#include "insdel.h"
#include "help.h"
#include "nets.h"
#include "files.h"
#include "files2.h"
#include "polygon.h"
#include "direct.h"
#include "resource.h"
#include "dialogs.h"
#include "ctype.h"


typedef struct
{
	double XO, YO, NewX, NewY, Width, Height, DivX2, DivY2;
	int32 CompNr, DivX, DivY, ShapeNr, Rotation, NewCompNr;
} CompPlaceRecord;


typedef struct
{
	char ComponentImportFileName[MAX_LENGTH_STRING];
	int32 SkipLines;
	int32 TotalLines;
	int32 MaxLineNr;
	int32 CompRefColumn;
	int32 CompValueColumn;
	int32 PositionXColumn;
	int32 PositionYColumn;
	int32 RotationColumn;
	int32 LayerColumn;
	int32 Units;
	int32 Info;
	double UnitsValue;
	int32 RotationMode;
} ImportComponentInfoRecord;


typedef CompPlaceRecord CompPlaceRecordArray[1000];

typedef int32 NetCountArray[1024];

typedef char ImportComponentTextRecord[1600][160];


ImportComponentInfoRecord ImportComponentInfo;

double CentreComponentsX, CentreComponentsY, CentreComponentsX2, CentreComponentsY2, MovingCompMinX, MovingCompMinY,
       MovingCompMaxX, MovingCompMaxY;

ImportComponentTextRecord *ImportComponentText;


int32 MaxPixels = 256;
int32 MaxPix = 0;
int32 AlignCompMode = 0;
int32 NrCompsSelected = 0;
char TempCompStr[MAX_LENGTH_STRING];
char ImportComponentFileName[MAX_LENGTH_STRING];

STARTUPINFO StartupInfo;

int32 *NetCount, ok;

extern HDC OutputDisplay;
extern int32 ProjectActive;
extern int32 ProjectIndexNr;
extern int32 ProjectIndexNr, CrossHairMode;
extern ProjectInfoRecord *ProjectInfo;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawMoveableConnections(double CurrentX, double CurrentY, double Rotation, int32 mode);

void MoveConnectionsComponents(int32 mode);

void CollectComponentsForMoving(int32 mode);

void CollectComponentsForMoving2(int32 mode);

int32 CheckFloatingConnections(void);

int32 AdjustOffsetForSnap(double CursorX, double CursorY, double GridX, double GridY, double divx, double divy,
                          double *ShiftOffsetX, double *ShiftOffsetY, int32 mode);

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void MoveComponents(int32 mode)
{
	int32 *SelectedObjects, Found, cnt, NrCompsSelected;
	CompRecord *Comp;

// mode  =  1  -> Move only components with shortest connections
// mode  =  2  -> Move components,traces/vias  (unselect connections)
// mode  =  3  -> Move components/traces/vias

	Found = 0;

	if ((mode & 2) == 0)
		CollectComponentsForMoving2(0);
	else
		CollectComponentsForMoving(0);

	NrCompsSelected = 0;
	MovingCompMinX = 1e9;
	MovingCompMinY = 1e9;
	MovingCompMaxX = -1e9;
	MovingCompMaxY = -1e9;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NrCompsSelected++;
			MovingCompMinX = min(MovingCompMinX, Comp->BoardPosMinX);
			MovingCompMinY = min(MovingCompMinY, Comp->BoardPosMinY);
			MovingCompMaxX = max(MovingCompMaxX, Comp->BoardPosMaxX);
			MovingCompMaxY = max(MovingCompMaxY, Comp->BoardPosMaxY);
		}
	}

//  DrawSelectedObjectsBlack(1);
	if ((mode & 2) == 2)
	{
		Found = IndexSelectedObjects(1, NULL, 0, 2);
		AllocateSpecialMem(MEM_OBJECT_SELECTED, Found * sizeof(int32), (void **) &SelectedObjects);
		IndexSelectedObjects(0, SelectedObjects, Found, mode);
	}
	else
	{
		memset(&NrIndexesVerTraces, 0, sizeof(NrIndexesVerTraces));
		memset(&NrIndexesHorTraces, 0, sizeof(NrIndexesHorTraces));
		memset(&NrIndexesDiag1Traces, 0, sizeof(NrIndexesDiag1Traces));
		memset(&NrIndexesDiag2Traces, 0, sizeof(NrIndexesDiag2Traces));
		NrIndexesVias = 0;
	}

	MoveConnectionsComponents(mode);

	if ((mode & 2) == 2)
	{
		if (Found > 4096)
			DeallocateSpecialMem(MEM_OBJECT_SELECTED);
	}

	DeAllocateMemObjects4();

	if ((mode & 2) == 0)
	{
		if (MaxNrObjects5 > 4096)
			DeAllocateMemObjects5();
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CollectComponentsForMoving(int32 mode)
{
	int32 cnt, cnt2, cnt3, PinsMemPos, ModeXor, CompInfo, PinNr, ConnectionFound;
	double cx1, cy1, cx2, cy2, cx2a, ObjectX, ObjectY, ObjectX2, ObjectY2, ObjectX2a, CompX, CompY;
	CompRecord *Comp;
	ObjectRecord *Object, *Object4;
	ConnectionsRecord *Connection;
	CompPinRecord *CompPin;

	AllocateMemObjects4(8192);

	NrObjects2 = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{

			SearchMinX = Comp->BoardPosMinX;
			SearchMinY = Comp->BoardPosMinY;
			SearchMaxX = Comp->BoardPosMaxX;
			SearchMaxY = Comp->BoardPosMaxY;
			NrObjects4 = 0;
			CopyConnectionsFromRectWindowToObjects4(-1, 0);

			PinsMemPos = (uint8 *) Comp - &(CompsMem[0]) + sizeof(CompRecord);
			NrObjects = 0;
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
			Object = &((*Objects)[0]);
			CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
			PinNr = Object->PinNr;
			CompX = Comp->CompOriginX;
			CompY = Comp->CompOriginY;

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->PinNr != PinNr)
				{
					PinNr = Object->PinNr;
					PinsMemPos += sizeof(CompPinRecord);
					CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				}

				if ((CompPin->NetNr != 32767) && (CompPin->NetNr != -1))
				{
					ObjectX = Object->x1;
					ObjectY = Object->y1;
					ObjectX2 = Object->x2;
					ObjectY2 = Object->y2;
					ObjectX2a = ObjectX2 * 0.5;
					ConnectionFound = 0;

					for (cnt3 = 0; cnt3 < NrObjects4; cnt3++)
					{
						Object4 = &((*Objects4)[cnt3]);
						cx1 = Object4->x1;
						cy1 = Object4->y1;
						cx2 = Object4->x2;
						cx2a = cx2 * 0.5;
						cy2 = Object4->y2;

						if (Object->NetNr == Object4->NetNr)
						{
							ModeXor = 0;

							if ((InRange(cx1, ObjectX)) && (InRange(cy1, ObjectY)))
							{
								ModeXor |= 1;
								ConnectionFound = 1;
							}

							if ((InRange(cx2, ObjectX)) && (InRange(cy2, ObjectY)))
							{
								ModeXor |= 2;
								ConnectionFound = 1;
							}

							switch (Object->ObjectType)
							{
							case PIN_LINE_HOR:
								if ((InRange(cx1, ObjectX + ObjectX2)) && (InRange(cy1, ObjectY)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx1, ObjectX + ObjectX2a)) && (InRange(cy1, ObjectY)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX + ObjectX2)) && (InRange(cy2, ObjectY)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX + ObjectX2a)) && (InRange(cy2, ObjectY)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								break;

							case PIN_LINE_VER:
								if ((InRange(cx1, ObjectX)) && (InRange(cy1, ObjectY + ObjectX2)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx1, ObjectX)) && (InRange(cy1, ObjectY + ObjectX2a)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX)) && (InRange(cy2, ObjectY + ObjectX2)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX)) && (InRange(cy2, ObjectY + ObjectX2a)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								break;

							case PIN_LINE_DIAG1:
								if ((InRange(cx1, ObjectX + ObjectX2)) && (InRange(cy1, ObjectY - ObjectX2)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx1, ObjectX + ObjectX2a)) && (InRange(cy1, ObjectY - ObjectX2a)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX + ObjectX2)) && (InRange(cy2, ObjectY - ObjectX2)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX + ObjectX2a)) && (InRange(cy2, ObjectY - ObjectX2a)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								break;

							case PIN_LINE_DIAG2:
								if ((InRange(cx1, ObjectX + ObjectX2)) && (InRange(cy1, ObjectY + ObjectX2)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx1, ObjectX + ObjectX2a)) && (InRange(cy1, ObjectY + ObjectX2a)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX + ObjectX2)) && (InRange(cy2, ObjectY + ObjectX2)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX + ObjectX2a)) && (InRange(cy2, ObjectY + ObjectX2a)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								break;

							case PIN_LINE_ALL_ANGLE:
								if ((InRange(cx1, ObjectX)) && (InRange(cy1, ObjectY)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx1, ObjectX2)) && (InRange(cy1, ObjectY2)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX)) && (InRange(cy2, ObjectY)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX2)) && (InRange(cy2, ObjectY2)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								break;

							case PIN_ARC:
								GetArcEndPoints(Object, &ObjectX, &ObjectY, &ObjectX2, &ObjectY2, 0);

								if ((InRange(cx1, ObjectX)) && (InRange(cy1, ObjectY)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx1, ObjectX2)) && (InRange(cy1, ObjectY2)))
								{
									ModeXor |= 1;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX)) && (InRange(cy2, ObjectY)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								if ((InRange(cx2, ObjectX2)) && (InRange(cy2, ObjectY2)))
								{
									ModeXor |= 2;
									ConnectionFound = 1;
								}

								break;
							}

							if (ModeXor > 0)
							{
								Connection = &((*Connections)[Object4->TraceNr]);
								Connection->Info |= OBJECT_SELECTED | ModeXor;
							}
						}
					}
				}
			}
		}
	}

    //ExitDrawing();
    //EndDrawingEditingWindow(0);

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	if (mode == 1)
	{
		for (cnt = 0; cnt < Design.NrConnections; cnt++)
		{
			Connection = &((*Connections)[cnt]);

			if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
				Connection->Info &= ~OBJECT_SELECTED;
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************

	NrCompsSelected = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			NrCompsSelected++;
	}

	/*
	  if (NrCompsSelected==1) {
	    AllocateSpecialMem(MEM_POINTS,128*1024,(void **)&SelectedNets);
	    memset(SelectedNets,0,Design.NrComps);
	    for (cnt=0;cnt<Design.NrComps;cnt++) {
	      Comp=(CompRecord *)&(CompsMem[(*Comps)[cnt]]);
	      CompInfo=Comp->Info;
	      if ((CompInfo & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	        NrObjects=0;
	        ShapePinsToObject(Comp,0.0,0.0,0,0,0,0);
	        for (cnt2=0;cnt2<NrObjects;cnt2++) {
	          Object=&((*Objects)[cnt2]);
	          if ((Object->NetNr>=0)
	             &&
	             (Object->NetNr<Design.NrNets)) {
	            SelectedNets[Object->NetNr]=1;
	          }
	        }
	      }
	    }
	  }
	*/
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void CollectComponentsForMoving2(int32 mode)
{
	int32 cnt, cnt2, ok, count, count2, CompInfo;
	double x2a, ObjectX, ObjectY, px, py, ObjectX2, ObjectY2;
	CompRecord *Comp, *Comp2;
	ObjectRecord *Object, *Object4, *Object5;
	uint8 *SelectedNets;

	AllocateMemObjects4(1024);
	AllocateMemObjects5(512);
	SetWaitCursor();
	NrObjects4 = 0;
	NrObjects5 = 0;
	NrCompsSelected = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			NrCompsSelected++;
	}

	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &SelectedNets);
	AllocateSpecialMem(MEM_NETINFO, 128 * 1024, (void **) &NetCount);
	memset(SelectedNets, 0, Design.NrNets);
	memset(NetCount, 0, 128 * 1024);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NrObjects = 0;
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
			px = 1e9;
			py = 1e9;

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets)
				        && ((NotInRange(Object->x1, px)) || (NotInRange(Object->y1, py))))
				{
					SelectedNets[Object->NetNr] = 1;

					if (NrObjects5 >= MaxNrObjects5)
						AllocateMemObjects5(NrObjects5 + 256);

					Object5 = &((*Objects5)[NrObjects5]);
					memmove(Object5, Object, sizeof(ObjectRecord));
					x2a = Object5->x2 * 0.5;

					switch (Object->ObjectType)
					{
					case PIN_LINE_HOR:
						Object5->x1 += x2a;
						break;

					case PIN_LINE_VER:
						Object5->y1 += x2a;
						break;

					case PIN_LINE_DIAG1:
						Object5->x1 += x2a;
						Object5->y1 -= x2a;
						break;

					case PIN_LINE_DIAG2:
						Object5->x1 += x2a;
						Object5->y1 += x2a;
						break;

					case PIN_ARC:
						GetArcEndPoints(Object, &ObjectX, &ObjectY, &ObjectX2, &ObjectY2, 0);
						Object5->x1 = ObjectX;
						Object5->y1 = ObjectY;
						break;
					}

					NrObjects5++;
				}

				px = Object->x1;
				py = Object->y1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (SelectedNets[cnt] == 1)
		{
			GetObjectsNet((int32) cnt, MODE_OBJECTS3, 1);

			for (cnt2 = 0; cnt2 < NrObjects3; cnt2++)
			{
				Object = &((*Objects3)[cnt2]);

				switch (Object->ObjectType)
				{
				case AREAFILL:
					break;

				case VIA_PUT_THROUGH_ROUND:
				case TRACE_HOR:
				case TRACE_VER:
				case TRACE_DIAG1:
				case TRACE_DIAG2:
				case TRACE_ALL_ANGLE:
				case TRACE_ARC:
					if (NrObjects4 >= MaxNrObjects4)
						AllocateMemObjects4(NrObjects4 + 256);

					Object4 = &((*Objects4)[NrObjects4]);
					memmove(Object4, Object, sizeof(ObjectRecord));
					NrObjects4++;
					NetCount[cnt]++;
					break;

				default:
					Comp2 = (CompRecord *) Object->TraceNr;

					if ((Comp2 != NULL) && ((Comp2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0))
					{
						if (NrObjects4 >= MaxNrObjects4)
							AllocateMemObjects4(NrObjects4 + 256);

						Object4 = &((*Objects4)[NrObjects4]);
						memmove(Object4, Object, sizeof(ObjectRecord));
						NrObjects4++;
						NetCount[cnt]++;
					}

					break;
				}
			}
		}
	}

	count = NetCount[0];
	NetCount[0] = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		count2 = NetCount[cnt + 1];
		NetCount[cnt + 1] = NetCount[cnt] + count;
		count = count2;
	}

	SetNormalCursor();
	ok = 1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void MoveConnectionsComponents(int32 mode)
{
// mode  =  1  -> Move only components with shortest connections
// mode  =  2  -> Move components,traces/vias  (unselect connections)
// mode  =  3  -> Move components/traces/vias

	int32 ok, cnt, NrParams, mode2, CompCount, res, FirstShift, ObjectsPlaced, FirstComp = 1;
	double OldX, OldY, CurrentX, CurrentY, ShiftX, ShiftY, CentreX, CentreY, x1, y1, x2, y2, CompRotationForMoving,
		   CursorX, CursorY, ViewFactor, ViewDivX, ViewDivY, divx, divy, NewMode1, NewMode2;
	CompRecord *Comp;
	ViaRecord *Via;
	DrawXorFunctionRecord DrawXorFunction;
	ObjectTextRecord2 TypeObject;
#ifdef _DEBUG
	char str[200], str2[200];
#endif
	ShiftX = 0.0;
	ShiftY = 0.0;
	RelX = CurrentDrawX1;
	RelY = CurrentDrawY1;
	CompCount = 0;
	SelectionEsc = 0;
	ObjectsPlaced = 0;
	/*
	  Rect.left=(int32)DrawWindowMinX+ClientStartX;
	  Rect.top=(int32)DrawWindowMinY+ClientStartY;
	  Rect.right=(int32)DrawWindowMaxX+ClientStartX;
	  Rect.bottom=(int32)DrawWindowMaxY+ClientStartY;
	*/
//  RelX=PixelToRealOffX(MousePosX);
//  RelY=PixelToRealOffY(DrawWindowMaxY-MousePosY-1);
	strcpy(TempCompStr, InfoStr);
//  CollectComponentsForMoving(1);

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if ((CompSnapMode & 1) == 1)
	{
		CentreComponentsX = CurrentX;
		CentreComponentsY = CurrentY;
//    RelX=PixelToRealOffX(MousePosX);
//    RelY=PixelToRealOffY(DrawWindowMaxY-MousePosY-1);
	}
	else
	{
//    CentreComponentsX=0.0;
//    CentreComponentsY=0.0;
		CentreComponentsX = CurrentX;
		CentreComponentsY = CurrentY;
		RelX = CurrentX;
		RelY = CurrentY;
	}

	ShiftOffsetX = 0.0;
	ShiftOffsetY = 0.0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			CompCount++;

			if (FirstComp)
			{
				if ((CompSnapMode & 1) == 1)
				{
					ShiftOffsetX = -(CurrentX - Comp->CompOriginX);
					ShiftOffsetY = -(CurrentY - Comp->CompOriginY);
					CentreComponentsX = Comp->CompOriginX;
					CentreComponentsY = Comp->CompOriginY;
				}

				FirstComp = 0;
			}
		}
	}

	if (FirstComp)
	{
		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if ((Via->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (FirstComp)
				{
					if ((CompSnapMode & 1) == 1)
					{
						ShiftOffsetX = -(CurrentX - Via->X);
						ShiftOffsetY = -(CurrentY - Via->Y);
						CentreComponentsX = Via->X;
						CentreComponentsY = Via->Y;
					}

					FirstComp = 0;
				}
			}
		}
	}

	if ((MoveCompAutoZoom) && (CompCount > 0))
	{
		ViewDivX = ViewMaxX - ViewMinX;
		ViewDivY = ViewMaxY - ViewMinY;
		ViewFactor = 0.1;

		if ((MovingCompMinX < ViewMinX - ViewDivX * ViewFactor) || (MovingCompMaxX > ViewMaxX + ViewDivX * ViewFactor)
		        || (MovingCompMinY < ViewMinY - ViewDivY * ViewFactor)
		        || (MovingCompMaxY > ViewMaxY + ViewDivY * ViewFactor))
		{
			CenterScreenOnWindow(MovingCompMinX, MovingCompMinY, MovingCompMaxX, MovingCompMaxY);
			CheckInputMessages(0);
			CheckInputMessages(0);
		}
	}

	if ((CompSnapMode & 1) == 1)
	{
		RelX = CurrentX + ShiftOffsetX;
		RelY = CurrentY + ShiftOffsetY;
	}

	DisplayCursorPosition();
	CompRotationForMoving = 0.0;
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	if ((mode & (4 + 8)) != 0)
	{
		if ((mode & 4) == 4)
		{	// Rotate 90
			CompRotationForMoving = 90.0;
		}

		if ((mode & 8) == 8)
		{	// Rotate at any angle
			res = RotationDialog(&CentreX, &CentreY, &CompRotationForMoving, 0);

			switch (res)
			{
			case 1:
				CentreComponentsX = AdjustToDrawGrid(CentreComponentsX);
				CentreComponentsY = AdjustToDrawGrid(CentreComponentsY);
				break;

			case 2:
				CentreComponentsX = CentreX;
				CentreComponentsY = CentreY;
				break;

			case 3:
				if (CommandSelectPoint(&CentreComponentsX, &CentreComponentsY, 0.0, 0.0, 0.0, 0.0, 0) == 0)
					return;

				break;

			case 4:
				break;

			default:
				return;
			}
		}
		
		CurrentX = 0.0;
		CurrentY = 0.0;
		CurrentX2 = 0.0;
		CurrentY2 = 0.0;
		PlaceMovedObjects(0.0, 0.0, CompRotationForMoving, 0);
		return;
	}

	mode2 = 1;

	if (CompCount < 10)
	{
//    mode2=1;
	}

	if ((mode & 3) == 1)
		mode2 |= 2;

	NewMode1 = mode2 | BM_MultiStart;
	NewMode2 = mode2 | BM_MultiEnd;

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	FirstShift = 1;				//  CrossHairX CrossHairY
	CrossHairMode = 1;
	DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
	ClipMouseCursor();
	SystemBusyMode = 2;
	DrawXorFunction.Function8 = (FUNCP8) DrawMoveableConnections;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &CompRotationForMoving;
	DrawXorFunction.Param1[3] = &NewMode1;
	DrawXorFunction.Mode = 7;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &CompRotationForMoving;
	DrawXorFunction.Param2[3] = &NewMode2;
	ZoomInOutProcessed = 0;

	//************************************ dolní zobrazení pozice pøesunu *********************************************

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_MultiStart);
				
				if (Units == 0)
				{
					x1 = (CurrentX - ShiftOffsetX - CurrentX2) / 2540.0;
					y1 = (CurrentY - ShiftOffsetY - CurrentY2) / 2540.0;
					sprintf(InfoStr, "%s  move x,y %.2f , %.2f thou", TempCompStr, x1, y1);
				}
				
				else
				{
					x1 = (CurrentX - ShiftOffsetX - CurrentX2) / 100000.0;
					y1 = (CurrentY - ShiftOffsetY - CurrentY2) / 100000.0;
					sprintf(InfoStr, SC(342, "%s  move x,y %.4f , %.4f mm"), TempCompStr, x1, y1);
				}

				//RedrawInfoStr(1);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_MultiEnd);
			
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | 16 | BM_MultiStart);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_MultiEnd);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | 16 | BM_MultiStart);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_MultiEnd);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | 16 | BM_MultiStart);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_MultiEnd);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | 16 | BM_MultiStart);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_MultiEnd);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{
				DrawMoveableConnections(ShiftX, ShiftY, CompRotationForMoving, mode2 | BM_MultiStart);
				ShiftOffsetX -= ShiftX - CurrentX;
				ShiftOffsetY -= ShiftY - CurrentY;
				FirstShift = 1;
				CursorX = PixelToRealOffX(MousePosX);
				CursorY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
				divx = CurrentX - CurrentX2 - ShiftOffsetX;
				divy = CurrentY - CurrentY2 - ShiftOffsetY;

				if ((CompRotationForMoving == 0) && ((SnapMode & 1) == 1))
				{
					AdjustOffsetForSnap(CursorX, CursorY, CurrentX, CurrentY, divx, divy, &ShiftOffsetX, &ShiftOffsetY,
					                    0);
				}

				if ((CompSnapMode & 1) == 1)
				{
					RelX = CurrentX2 + ShiftOffsetX;
					RelY = CurrentY2 + ShiftOffsetY;
				}

				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_MultiEnd);
				DisplayCursorPosition();
			}
		}
		else
		{
			if (FirstShift)
			{
				ShiftX = CurrentX;
				ShiftY = CurrentY;
				FirstShift = 0;
			}
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			else
				ObjectsPlaced = 1;
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			ZoomWindow();

			while (CtrlPressed)
				CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			else
				ObjectsPlaced = 1;
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			PanWindow();

			while (CtrlPressed)
				CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			else
				ObjectsPlaced = 1;
		}

		if (CheckLeftButton())
		{
			DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			UnClipMouseCursor();
			
			PlaceMovedObjects(CurrentX - ShiftOffsetX, CurrentY - ShiftOffsetY, CompRotationForMoving, 0);
			CheckInputMessages(0);
			SelectionEsc = 1;
			ObjectsPlaced = 1;
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
#ifdef _DEBUG
			GetTimeString(1, str2);
			sprintf(str, "%s | RightButtonPressed\n", str2);
			OutputDebugString(str);
#endif
			
			if (SelectionMode != MOVING_TRACES_VIAS_MODE)
			{
				DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_MultiStart);
			
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;

				if (AltPressed)
					CompRotationForMoving += 45.0;
				else
					CompRotationForMoving += 90.0;

				if (CompRotationForMoving >= 360.0)
					CompRotationForMoving -= 360.0;

				RightButtonPressed = 0;
				CheckInputMessages(0);
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_MultiEnd);
			}
			else
			{
				RightButtonPressed = 0;
				CheckInputMessages(0);
			}
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawMoveableConnections(OldX, OldY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(TypeObject));

				if (LineInputDialog(&TypeObject, SC(835, "Move objects to ( x,y ) or rotation angle"), 0) == 1)
				{
					NrParams = ScanParameters(-1, TypeObject.Text, 0);

					if (NrParams == 1)
						CompRotationForMoving = ConvertToUnits(Units, ParamsFloat[0]);

					if (NrParams == 2)
					{
						x1 = ParamsFloat[0];
						y1 = ParamsFloat[1];

						if (!ParametersRelative)
						{
							x1 -= ShiftOffsetX;
							y1 -= ShiftOffsetY;
						}
						else
						{
							x1 += CurrentX2;
							y1 += CurrentY2;
						}

						x2 = x1 - CurrentX2;
						y2 = y1 - CurrentY2;
						PlaceMovedObjects(x1, y1, CompRotationForMoving, 0);
						SelectionEsc = 1;
					}
				}
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				Help("move_components.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			FirstShift = 1;

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_DoubleBuffer);
			else
				ObjectsPlaced = 1;

			ok = 1;
			ClipMouseCursor();
		}
	}

// MaxPix
	strcpy(InfoStr, TempCompStr);
	RedrawInfoStr(1);

	if (!ObjectsPlaced)
		DrawMoveableConnections(CurrentX, CurrentY, CompRotationForMoving, mode2 | BM_DoubleBuffer);

	CrossHairMode = 0;
	DrawCrossHair(16 + 2);
	UnClipMouseCursor();
	CheckFloatingConnections();
	SystemBusyMode = 0;
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 AdjustOffsetForSnap(double CursorX, double CursorY, double GridX, double GridY, double divx, double divy,
                          double *ShiftOffsetX, double *ShiftOffsetY, int32 mode)
{
	int32 cnt, cnt2, Layer, SelectionMask1, SelectionMask2, res;
	double px1, py1, px2, py2;
	ObjectRecord NewObject, *Object;
	TraceRecord *Trace;
	ViaRecord *Via;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectPolygonRecord *ObjectPolygon;
	CompRecord *Comp;

	px1 = 0.0;
	py1 = 0.0;
	px2 = 0.0;
	py2 = 0.0;

	if ((mode & 3) == 0)
	{
		SelectionMask1 = OBJECT_NOT_VISIBLE | OBJECT_SELECTED;
		SelectionMask2 = OBJECT_SELECTED;
	}
	else
	{
		divx = 0.0;
		divy = 0.0;
		SelectionMask1 = OBJECT_NOT_VISIBLE;
		SelectionMask2 = 0;
	}

	memset(&NewObject, 0, sizeof(NewObject));

	if ((mode & 4) == 0)
	{
		mode &= 3;

		if (CurrentDrawingLayer != -1)
		{
			for (cnt = 0; cnt < Design.NrVerTraces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*VerTraces[CurrentDrawingLayer])[cnt]);

				if ((Trace->Info & SelectionMask1) == SelectionMask2)
				{
					CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_VER, CurrentDrawingLayer, cnt, 2);
					res =
					    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
					                                ShiftOffsetX, ShiftOffsetY, mode);

					if (res == 1)
						return 1;
				}
			}

			for (cnt = 0; cnt < Design.NrHorTraces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*HorTraces[CurrentDrawingLayer])[cnt]);

				if ((Trace->Info & SelectionMask1) == SelectionMask2)
				{
					CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_HOR, CurrentDrawingLayer, cnt, 2);
					res =
					    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
					                                ShiftOffsetX, ShiftOffsetY, mode);

					if (res == 1)
						return 1;
				}
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*Diag1Traces[CurrentDrawingLayer])[cnt]);

				if ((Trace->Info & SelectionMask1) == SelectionMask2)
				{
					CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_DIAG1, CurrentDrawingLayer, cnt, 2);
					res =
					    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
					                                ShiftOffsetX, ShiftOffsetY, mode);

					if (res == 1)
						return 1;
				}
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[CurrentDrawingLayer]; cnt++)
			{
				Trace = &((*Diag2Traces[CurrentDrawingLayer])[cnt]);

				if ((Trace->Info & SelectionMask1) == SelectionMask2)
				{
					CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_DIAG2, CurrentDrawingLayer, cnt, 2);
					res =
					    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
					                                ShiftOffsetX, ShiftOffsetY, mode);

					if (res == 1)
						return 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if ((Via->Info & SelectionMask1) == SelectionMask2)
			{
				CreateViaObjectFromVia(Via, &NewObject, 0);
				res =
				    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy, ShiftOffsetX,
				                                ShiftOffsetY, mode);
			}
		}

		for (Layer = 0; Layer < 32; Layer++)
		{
			if (Layer != CurrentDrawingLayer)
			{
				for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
				{
					Trace = &((*VerTraces[Layer])[cnt]);

					if ((Trace->Info & SelectionMask1) == SelectionMask2)
					{
						CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_VER, Layer, cnt, 2);
						res =
						    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
						                                ShiftOffsetX, ShiftOffsetY, mode);
					}
				}

				for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
				{
					Trace = &((*HorTraces[Layer])[cnt]);

					if ((Trace->Info & SelectionMask1) == SelectionMask2)
					{
						CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_HOR, Layer, cnt, 2);
						res =
						    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
						                                ShiftOffsetX, ShiftOffsetY, mode);

						if (res == 1)
							return 1;
					}
				}

				for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
				{
					Trace = &((*Diag1Traces[Layer])[cnt]);

					if ((Trace->Info & SelectionMask1) == SelectionMask2)
					{
						CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_DIAG1, Layer, cnt, 2);
						res =
						    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
						                                ShiftOffsetX, ShiftOffsetY, mode);

						if (res == 1)
							return 1;
					}
				}

				for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
				{
					Trace = &((*Diag2Traces[Layer])[cnt]);

					if ((Trace->Info & SelectionMask1) == SelectionMask2)
					{
						CreateTraceObjectFromTrace(Trace, &NewObject, TRACE_DIAG2, Layer, cnt, 2);
						res =
						    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
						                                ShiftOffsetX, ShiftOffsetY, mode);

						if (res == 1)
							return 1;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & SelectionMask1) == SelectionMask2)
			{
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
					memmove(&NewObject, Object, sizeof(ObjectRecord));

					switch (Object->ObjectType)
					{
					case PIN_LINE_HOR:
						NewObject.ObjectType = OBJECT_LINE;
						NewObject.x2 = Object->x1 + Object->x2;
						NewObject.y2 = Object->y1;
						NewObject.Thickness = Object->y2;
						break;

					case PIN_LINE_VER:
						NewObject.ObjectType = OBJECT_LINE;
						NewObject.x2 = Object->x1;
						NewObject.y2 = Object->y1 + Object->x2;
						NewObject.Thickness = Object->y2;
						break;

					case PIN_LINE_DIAG1:
						NewObject.ObjectType = OBJECT_LINE;
						NewObject.x2 = Object->x1 + Object->x2;
						NewObject.y2 = Object->y1 - Object->x2;
						NewObject.Thickness = Object->y2;
						break;

					case PIN_LINE_DIAG2:
						NewObject.ObjectType = OBJECT_LINE;
						NewObject.x2 = Object->x1 + Object->x2;
						NewObject.y2 = Object->y1 + Object->x2;
						NewObject.Thickness = Object->y2;
						break;

					case PIN_ARC:
					case PIN_LINE_ALL_ANGLE:
					case PIN_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_SQUARE:
					case PIN_PUT_THROUGH_POLYGON:
						break;
					}

					res =
					    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy,
					                                ShiftOffsetX, ShiftOffsetY, mode);

					if (res == 1)
						return 1;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & SelectionMask1) == SelectionMask2) && (ObjectLine->Layer < 32))
			{
				NewObject.x1 = ObjectLine->X1;
				NewObject.y1 = ObjectLine->Y1;
				NewObject.x2 = ObjectLine->X2;
				NewObject.y2 = ObjectLine->Y2;
				NewObject.Info = 0;
				NewObject.Test = 0;
				NewObject.Clearance = ObjectLine->Clearance;
				NewObject.NetNr = ObjectLine->NetNr;
				NewObject.Layer = ObjectLine->Layer;
				NewObject.ObjectType = OBJECT_LINE;
				NewObject.Thickness = ObjectLine->LineThickNess;
				res =
				    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy, ShiftOffsetX,
				                                ShiftOffsetY, mode);

				if (res == 1)
					return 1;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & SelectionMask1) == SelectionMask2)
			        && ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)
			            || (ObjectArc->Layer == DRILL_UNPLATED_LAYER)))
			{
				CreateArcObjectFromArc(ObjectArc, &NewObject, 0);
				res =
				    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy, ShiftOffsetX,
				                                ShiftOffsetY, mode);

				if (res == 1)
					return 1;
			}
		}
	}
	else
	{
		mode &= 3;

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & SelectionMask1) == SelectionMask2)
			{
				NewObject.x1 = ObjectLine->X1;
				NewObject.y1 = ObjectLine->Y1;
				NewObject.x2 = ObjectLine->X2;
				NewObject.y2 = ObjectLine->Y2;
				NewObject.Info = 0;
				NewObject.Test = 0;
				NewObject.Clearance = ObjectLine->Clearance;
				NewObject.NetNr = ObjectLine->NetNr;
				NewObject.Layer = ObjectLine->Layer;
				NewObject.ObjectType = OBJECT_LINE;
				NewObject.Thickness = ObjectLine->LineThickNess;
				res =
				    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy, ShiftOffsetX,
				                                ShiftOffsetY, mode);

				if (res == 1)
					return 1;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if ((ObjectRect->Info & (SelectionMask1)) == SelectionMask2)
			{
				NewObject.x1 = ObjectRect->CentreX;
				NewObject.y1 = ObjectRect->CentreY;
				NewObject.x2 = ObjectRect->Width;
				NewObject.y2 = ObjectRect->Height;
				NewObject.Info = 0;
				NewObject.Test = 0;
				NewObject.Clearance = ObjectRect->Clearance;
				NewObject.NetNr = ObjectRect->NetNr;
				NewObject.Layer = ObjectRect->Layer;
				NewObject.ObjectType = PIN_SMD_RECT;
				NewObject.Thickness = ObjectRect->LineThickNess;
				res =
				    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy, ShiftOffsetX,
				                                ShiftOffsetY, mode);

				if (res == 1)
					return 1;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & SelectionMask1) == SelectionMask2)
			{
				CreateArcObjectFromArc(ObjectArc, &NewObject, 0);
				res =
				    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy, ShiftOffsetX,
				                                ShiftOffsetY, mode);

				if (res == 1)
					return 1;
			}
		}

		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & SelectionMask1) == SelectionMask2)
			{
				NewObject.ObjectType = OBJECT_POLYGON;
				NewObject.ObjectType2 = 0;
				NewObject.TraceNr = cnt;
				res =
				    AdjustOffsetForSnapOnObject(&NewObject, CursorX, CursorY, GridX, GridY, divx, divy, ShiftOffsetX,
				                                ShiftOffsetY, mode);

				if (res == 1)
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

void DrawMoveableConnections(double CurrentX, double CurrentY, double Rotation, int32 mode)
{
	int32 cnt, cnt2, Layer, i, Start, End, FoundCnt, TraceInfo, ConnectionInfo, CompInfo, NetNr, *IndexP, x1a, y1a, x2a,
	      y2a, NrCompsSelected;
	double x1, y1, x2, y2, x3, y3, x4, y4, dikte, dikte2, Length, MinLength, Xmax, Xmin, Ymax, Ymin, NewRotation;
	TraceRecord *Trace;
	ViaRecord *Via;
	ConnectionsRecord *Connection;
	CompRecord *Comp;
	ObjectRecord *Object5, *Object4;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;
	int32 BufferMode;

	BufferMode = mode;
	mode &= ~BM_Mask;

	NrCompsSelected = 0;
	StartDrawingEditingWindow(BufferMode);
	SetROP2(OutputDisplay, R2_XORPEN);
	InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
	NewRotation = Rotation;

	for (Layer = 0; Layer < 32; Layer++)
	{
// ****************************************************************************************************
		IndexP = IndexVerTraces[Layer];

		for (cnt = 0; cnt < NrIndexesVerTraces[Layer]; cnt++)
		{
			i = *IndexP;
			Trace = &((*VerTraces[Layer])[*IndexP]);
			TraceInfo = Trace->Info;
			x1 = Trace->X;
			y1 = Trace->Y;
			x2 = x1;
			y2 = y1 + Trace->Length;

			if ((TraceInfo & 1) == 1)
			{
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += CurrentX - CurrentX2 - ShiftOffsetX;
				y1 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if ((TraceInfo & 2) == 2)
			{
				RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
				x2 += CurrentX - CurrentX2 - ShiftOffsetX;
				y2 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if (x1 < x2)
			{
				Xmin = x1;
				Xmax = x2;
			}
			else
			{
				Xmin = x2;
				Xmax = x1;
			}

			if (y1 < y2)
			{
				Ymin = y1;
				Ymax = y2;
			}
			else
			{
				Ymin = y2;
				Ymax = y1;
			}

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);

				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
				}
				else
					DrawLine(x1a, y1a, x2a, y1a);
			}

			IndexP += 1;
		}

// ****************************************************************************************************
		IndexP = IndexHorTraces[Layer];

		for (cnt = 0; cnt < NrIndexesHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[*IndexP]);
			TraceInfo = Trace->Info;
			x1 = Trace->X;
			y1 = Trace->Y;
			x2 = x1 + Trace->Length;
			y2 = y1;

			if ((TraceInfo & 1) == 1)
			{
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += CurrentX - CurrentX2 - ShiftOffsetX;
				y1 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if ((TraceInfo & 2) == 2)
			{
				RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
				x2 += CurrentX - CurrentX2 - ShiftOffsetX;
				y2 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if (x1 < x2)
			{
				Xmin = x1;
				Xmax = x2;
			}
			else
			{
				Xmin = x2;
				Xmax = x1;
			}

			if (y1 < y2)
			{
				Ymin = y1;
				Ymax = y2;
			}
			else
			{
				Ymin = y2;
				Ymax = y1;
			}

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);

				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
				}
				else
					DrawLine(x1a, y1a, x2a, y1a);
			}

			IndexP += 1;
		}

// ****************************************************************************************************

		IndexP = IndexDiag1Traces[Layer];

		for (cnt = 0; cnt < NrIndexesDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[*IndexP]);
			TraceInfo = Trace->Info;
			x1 = Trace->X;
			y1 = Trace->Y;
			x2 = x1 + Trace->Length;
			y2 = y1 - Trace->Length;

			if ((TraceInfo & 1) == 1)
			{
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += CurrentX - CurrentX2 - ShiftOffsetX;
				y1 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if ((TraceInfo & 2) == 2)
			{
				RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
				x2 += CurrentX - CurrentX2 - ShiftOffsetX;
				y2 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if (x1 < x2)
			{
				Xmin = x1;
				Xmax = x2;
			}
			else
			{
				Xmin = x2;
				Xmax = x1;
			}

			if (y1 < y2)
			{
				Ymin = y1;
				Ymax = y2;
			}
			else
			{
				Ymin = y2;
				Ymax = y1;
			}

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);

				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
				}
				else
					DrawLine(x1a, y1a, x2a, y1a);
			}

			IndexP += 1;
		}

// ****************************************************************************************************

		IndexP = IndexDiag2Traces[Layer];

		for (cnt = 0; cnt < NrIndexesDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[*IndexP]);
			TraceInfo = Trace->Info;
			x1 = Trace->X;
			y1 = Trace->Y;
			x2 = x1 + Trace->Length;
			y2 = y1 + Trace->Length;

			if ((TraceInfo & 1) == 1)
			{
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += CurrentX - CurrentX2 - ShiftOffsetX;
				y1 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if ((TraceInfo & 2) == 2)
			{
				RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
				x2 += CurrentX - CurrentX2 - ShiftOffsetX;
				y2 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if (x1 < x2)
			{
				Xmin = x1;
				Xmax = x2;
			}
			else
			{
				Xmin = x2;
				Xmax = x1;
			}

			if (y1 < y2)
			{
				Ymin = y1;
				Ymax = y2;
			}
			else
			{
				Ymin = y2;
				Ymax = y1;
			}

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);

				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
				}
				else
					DrawLine(x1a, y1a, x2a, y1a);
			}

			IndexP += 1;
		}
	}

// ****************************************************************************************************
	IndexP = IndexVias;
	dikte = 50000.0;

	for (cnt = 0; cnt < NrIndexesVias; cnt++)
	{
		Via = &((*Vias)[*IndexP]);
		x1 = Via->X;
		y1 = Via->Y;
		RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
		x1 += CurrentX - CurrentX2 - ShiftOffsetX;
		y1 += CurrentY - CurrentY2 - ShiftOffsetY;

		dikte = Via->ThickNess;
		dikte2 = Via->ThickNess + Via->Clearance * 2;
		Xmin = x1 - dikte;
		Ymin = y1 - dikte;
		Xmax = x1 + dikte;
		Ymax = y1 + dikte;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			ellips2(MultX(x1), MultY(y1), Mult(dikte), Mult(dikte), 255);
			ellips2(MultX(x1), MultY(y1), Mult(dikte2), Mult(dikte2), 255);
		}

		IndexP += 1;
	}

// ****************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			x1 = ObjectLine->X1;
			y1 = ObjectLine->Y1;
			x2 = ObjectLine->X2;
			y2 = ObjectLine->Y2;

			if ((ObjectLine->Info & 1) == 1)
			{
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += CurrentX - CurrentX2 - ShiftOffsetX;
				y1 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if ((ObjectLine->Info & 2) == 2)
			{
				RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
				x2 += CurrentX - CurrentX2 - ShiftOffsetX;
				y2 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if (x1 < x2)
			{
				Xmin = x1;
				Xmax = x2;
			}
			else
			{
				Xmin = x2;
				Xmax = x1;
			}

			if (y1 < y2)
			{
				Ymin = y1;
				Ymax = y2;
			}
			else
			{
				Ymin = y2;
				Ymax = y1;
			}

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);

				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
				}
				else
					DrawLine(x1a, y1a, x2a, y1a);
			}
		}
	}

// ****************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			x1 = ObjectArc->CentreX;
			y1 = ObjectArc->CentreY;
			x2 = ObjectArc->Width;
			y2 = ObjectArc->Height;
			Xmin = x1 - x2 * 0.5;
			Xmax = x1 + x2 * 0.5;
			Ymin = y1 - x2 * 0.5;
			Ymax = y1 + x2 * 0.5;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += CurrentX - CurrentX2 - ShiftOffsetX;
				y1 += CurrentY - CurrentY2 - ShiftOffsetY;

				if ((ObjectArc->Layer == DRILL_UNPLATED_LAYER) || (ObjectArc->Layer == DRILL_LAYER))
					ellips2(MultX(x1), MultY(y1), Mult(x2), Mult(x2), 255);
				else
				{
					x3 = ObjectArc->StartDiffX;
					y3 = ObjectArc->StartDiffY;
					x4 = ObjectArc->EndDiffX;
					y4 = ObjectArc->EndDiffY;
					RotatePointFromOtherPoint2(&x3, &y4, 0.0, 0.0, Rotation);
					RotatePointFromOtherPoint2(&x4, &y4, 0.0, 0.0, Rotation);
					SpecialArc(MultX(x1), MultY(y1), Mult(x2) + 1, Mult(y2) + 1, MultX(x1 + x3), MultY(y1 + y3),
					           MultX(x1 + x4), MultY(y1 + y4));
				}
			}
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************
	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			x1 = Comp->CompOriginX;
			y1 = Comp->CompOriginY;
			RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
			x1 += CurrentX - CurrentX2 - ShiftOffsetX;
			y1 += CurrentY - CurrentY2 - ShiftOffsetY;

			if ((mode & 1) == 1)
				DrawComp(Comp, x1 - Comp->CompOriginX, y1 - Comp->CompOriginY, Rotation, 0x202);
			else
				DrawPlacementOutlineComp(Comp, x1 - Comp->CompOriginX, y1 - Comp->CompOriginY, Rotation, 2);
		}
	}

	DrawSelectedAreafills(CurrentX - ShiftOffsetX, CurrentY - ShiftOffsetY, CurrentX2, CurrentY2, CentreComponentsX,
	                      CentreComponentsY, Rotation, 2);
	InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);

	if ((mode & 2) == 0)
	{
		IndexP = IndexConnections;

		for (cnt = 0; cnt < NrIndexesConnections; cnt++)
		{
			Connection = &((*Connections)[*IndexP]);
			ConnectionInfo = Connection->Info;
			x1 = Connection->x1;
			y1 = Connection->y1;
			x2 = Connection->x2;
			y2 = Connection->y2;

			if ((ConnectionInfo & 1) == 1)
			{
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += CurrentX - CurrentX2 - ShiftOffsetX;
				y1 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if ((ConnectionInfo & 2) == 2)
			{
				RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
				x2 += CurrentX - CurrentX2 - ShiftOffsetX;
				y2 += CurrentY - CurrentY2 - ShiftOffsetY;
			}

			if (x1 < x2)
			{
				Xmin = x1;
				Xmax = x2;
			}
			else
			{
				Xmin = x2;
				Xmax = x1;
			}

			if (y1 < y2)
			{
				Ymin = y1;
				Ymax = y2;
			}
			else
			{
				Ymin = y2;
				Ymax = y1;
			}

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				if ((ConnectionInfo & OBJECT_HIGHLITED) != 0)
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ConnectionsHilitedObjectNr])
						InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_HILITED_PEN_AND_NOT_FILLED);
				}
				else
				{
					if (CurrentObjectCode != GraphicsObjectCodes[ConnectionsObjectNr])
						InitDrawingObject(0, CONNECTIONS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
				}

				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);

				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
					//        MaxPix+=max(abs(x1a-x2a),abs(y1a-y2a));
				}
				else
					DrawLine(x1a, y1a, x2a, y1a);
			}

			IndexP += 1;
		}
	}
	else
	{
// ****************************************************************************************************
// ****************************************************************************************************
		for (cnt = 0; cnt < NrObjects5; cnt++)
		{
			Object5 = &((*Objects5)[cnt]);
			x1 = Object5->x1;
			y1 = Object5->y1;
			RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
			x1 += CurrentX - CurrentX2 - ShiftOffsetX;
			y1 += CurrentY - CurrentY2 - ShiftOffsetY;
			NetNr = Object5->NetNr;
			Start = NetCount[NetNr];
			End = NetCount[NetNr + 1];
			FoundCnt = -1;
			MinLength = 1e18;

			for (cnt2 = Start; cnt2 < End; cnt2++)
			{
				Object4 = &((*Objects4)[cnt2]);
				x2 = Object4->x1;
				y2 = Object4->y1;
				Length = SQR(x2 - x1) + SQR(y2 - y1);

				if (Length < MinLength)
				{
					MinLength = Length;
					FoundCnt = cnt2;
				}
			}

			if (FoundCnt != -1)
			{
				Object4 = &((*Objects4)[FoundCnt]);
				x2 = Object4->x1;
				y2 = Object4->y1;
				x1a = MultX(x1);
				y1a = MultY(y1);
				x2a = MultX(x2);
				y2a = MultY(y2);

				if ((min(x1a, x2a) > -32000) && (max(x1a, x2a) < 32000) && (min(y1a, y2a) > -32000)
				        && (max(y1a, y2a) < 32000))
				{
					MoveToEx(OutputDisplay, x1a, y1a, NULL);
					LineTo(OutputDisplay, x2a, y2a);
				}
				else
					DrawLine(x1a, y1a, x2a, y1a);
			}
		}
	}

	if ((mode & 16) == 0)
		DrawCrossHair(16 + 8);
	else
		DrawCrossHair(16 + 8 + 2);

	ExitDrawing();
	EndDrawingEditingWindow(BufferMode);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 MoveTrace(TraceRecord * Trace, double x1, double y1, double x2, double y2, int32 Layer, int32 OldTraceType,
                int32 CheckAreaFills, uint8 * SelectedNetsForMove)
{
	int32 InsertConnection, OkToRepaint = 0, ok, res;
	double TraceLength, dx, dy;
	NetRecord *Net;
	ObjectRecord TraceObject;

	InsertConnection = 0;
	TraceLength = Trace->Length;
	dx = x1 - x2;
	dy = y1 - y2;
	TraceObject.ObjectType = 0;

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
		TraceObject.ObjectType = -1;
	else
	{
		if (InRangeSpecial(x1, x2, 0.1))
		{	// Ver trace
			TraceObject.ObjectType = TRACE_VER;
			TraceLength = fabs(y1 - y2);
			y1 = min(y1, y2);
		}
		else
		{
			if (InRangeSpecial(y1, y2, 0.1))
			{	// Hor trace
				TraceObject.ObjectType = TRACE_HOR;
				TraceLength = fabs(x1 - x2);
				x1 = min(x1, x2);
			}
			else
			{
				if (InRangeSpecial(x1 - x2, y1 - y2, 0.1))
				{	// diag2 trace
					TraceObject.ObjectType = TRACE_DIAG2;
					TraceLength = fabs(x1 - x2);

					if (x2 < x1)
					{
						x1 = x2;
						y1 = y2;
					}
				}
				else
				{
					if (InRangeSpecial(x1 - x2, y2 - y1, 0.1))
					{	// diag1 trace
						TraceObject.ObjectType = TRACE_DIAG1;
						TraceLength = fabs(x1 - x2);

						if (x2 < x1)
						{
							x1 = x2;
							y1 = y2;
						}
					}
					else
						ok = 1;
				}
			}
		}
	}

// Delete old trace
	Trace->Info = 0;
	ZeroUnusedObjects(0);
	Trace->Info |= OBJECT_NOT_VISIBLE;
	Trace->DeleteNr = (int16) LastActionNr;
	DataBaseChanged = 1;

	if (TraceObject.ObjectType != 0)
	{
		if (TraceObject.ObjectType != -1)
		{
			TraceObject.x1 = x1;
			TraceObject.y1 = y1;
			TraceObject.x2 = TraceLength;
			TraceObject.y2 = Trace->ThickNess;
			TraceObject.Clearance = Trace->Clearance;
			TraceObject.Layer = Layer;
			TraceObject.NetNr = Trace->NetNr;
			FillPositionObject(&TraceObject);

			if (CheckObjectOverlappedFromObjects(&TraceObject, 5) == -1)
			{
				// Moved trace can be placed
				TraceObject.Info = 0;
				Net = &((*Nets)[TraceObject.NetNr]);

				if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
					TraceObject.Info |= OBJECT_HIGHLITED;

				if (AddTrace(&TraceObject))
				{
					SelectedNetsForMove[TraceObject.NetNr] = 1;

					if (CheckAreaFills)
					{
						if (RecalcAreafillAfterInsert)
						{
							res = InsertObjectInAreaFill(&TraceObject, TraceObject.Layer, TraceObject.NetNr, 2);

							if ((res == 1) && (OkToDrawAreaFills))
								OkToRepaint = 1;
						}
					}
				}
			}
		}
	}
	else
	{
		SelectedNetsForMove[Trace->NetNr] = 1;
		memset(&NewObjectLine, 0, sizeof(NewObjectLine));
		NewObjectLine.Layer = Layer;
		NewObjectLine.Clearance = Trace->Clearance;
		NewObjectLine.NetNr = Trace->NetNr;
		NewObjectLine.LineThickNess = Trace->ThickNess;
		NewObjectLine.X1 = (float) x1;
		NewObjectLine.Y1 = (float) y1;
		NewObjectLine.X2 = (float) x2;
		NewObjectLine.Y2 = (float) y2;
		TraceObject.x1 = x1;
		TraceObject.y1 = y1;
		TraceObject.x2 = x2;
		TraceObject.y2 = y2;
		TraceObject.Thickness = Trace->ThickNess;
		TraceObject.Clearance = Trace->Clearance;
		TraceObject.Layer = Layer;
		TraceObject.NetNr = Trace->NetNr;
		FillPositionObject(&TraceObject);

		if (AddObjectLine(&NewObjectLine))
		{
			if (CheckAreaFills)
			{
				if (RecalcAreafillAfterInsert)
					res = InsertObjectInAreaFill(&TraceObject, -1, NewObjectLine.NetNr, 2);
			}
		}
	}

	DataBaseChanged = 1;
	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 MoveComponentsOneGridPosition(int32 mode)
{
	double OffsetX = 0.0, OffsetY = 0.0;

	CurrentX2 = 0.0;
	CurrentY2 = 0.0;

	switch (mode)
	{
	case 0:
		OffsetX = 0.0;
		OffsetY = GridSize;
		break;

	case 1:
		OffsetX = 0.0;
		OffsetY = -GridSize;
		break;

	case 2:
		OffsetX = -GridSize;
		OffsetY = 0.0;
		break;

	case 3:
		OffsetX = GridSize;
		OffsetY = 0.0;
		break;
	}

	PlaceMovedObjects(OffsetX, OffsetY, 0.0, 0);
	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void PlaceMovedObjects(double CurrentX, double CurrentY, double Rotation, int32 mode)
{
	int32 cnt, cnt2, *IndexP, Layer, MemSize, FoundComps, Object5OverlapNr, Count, res, mirror, FirstCompNr, TraceInfo,
	      ViaInfo, CompInfo, CompCnt1, CompCnt2, TempLastActionNr, CompRot, CompOverlap, CheckAreaFills = 0, OkToRepaint;
	float Rotation3;
	double divx, divy, x1, y1, x2, y2, x3, y3, x4, y4, dikte, rx, ry, vx, vy, xx1, yy1, xx2, yy2a, fminx, fmaxx, fminy,
	       fmaxy;
	ConnectionsRecord NewConnection;
	CompRecord *Comp, *NewComp, *NewComp2, *Comp1, *Comp2;
	TraceRecord *Trace;
	ObjectRecord *Object, CheckObject, ViaObject, *Object5;
	ViaRecord *Via, NewVia;
	NetRecord *Net;
	RECT CompRect;
	AreaFillRecord *AreaFill;
	char str2[MAX_LENGTH_STRING];
	uint8 *SelectedNetsForMove;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 ChangedObjectText2;

	AllocateSpecialMem(MEM_NET_SELECTED, Design.NrNets + 10, (void **) &SelectedNetsForMove);
	FirstCompNr = 0;
	memset(SelectedNetsForMove, 0, Design.NrNets);
	divx = 0.0;
	divy = 0.0;
	FoundComps = 0;
	CompCnt1 = -1;
	CompCnt2 = -1;

	switch (mode)
	{
	case 0:
		divx = CurrentX - CurrentX2;	// LastActionNr
		divy = CurrentY - CurrentY2;
		break;

	case 1:
		break;

	case 2:
		break;

	case 3:
		memset(&ChangedObjectText2, 0, sizeof(ObjectTextRecord2));
		strcpy(ChangedObjectText2.Text, "0.0");

		if (TextInputDialog(&ChangedObjectText2, 0x17) != 1)
			return;

		Rotation3 = 0.0;
		sscanf(ChangedObjectText2.Text, "%f", &Rotation3);
		Rotation = Rotation3;
		break;
	}

	SearchMinX = 1e9;
	SearchMinY = 1e9;
	SearchMaxX = -1e9;
	SearchMaxY = -1e9;

// **************************************************************************************
// **************************************************************************************
// **************************************************************************************
// Find minimum occupying rectangle moving components
// **************************************************************************************

	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
	NewComp2 = (CompRecord *) ((uint8 *) NewComp + 64 * 1024);

	switch (mode)
	{
	case 0:
		FoundComps = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
			CompInfo = Comp->Info;

			if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				FoundComps++;
				FirstCompNr = cnt;
				x1 = Comp->CompOriginX;
				y1 = Comp->CompOriginY;
// **************************************************************************************
// Recalc new position component -> NewComp
// **************************************************************************************
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				MemSize = MemSizeComp(Comp);
				memcpy(NewComp, Comp, MemSize);

//      x3=AdjustToDrawGrid(x3);
//      y3=AdjustToDrawGrid(y3);
				NewComp->CompOriginX = (float) (x1 + divx);
				NewComp->CompOriginY = (float) (y1 + divy);
				NewComp->Rotation += (float) Rotation;

				if (NewComp->Rotation > 359.9)
					NewComp->Rotation -= 360.0;

				if (NewComp->Rotation < 0.0)
					NewComp->Rotation += 360.0;

				SetBoardPosComp(NewComp, 0);
				SearchMinX = min(SearchMinX, NewComp->BoardPosMinX);
				SearchMinY = min(SearchMinY, NewComp->BoardPosMinY);
				SearchMaxX = max(SearchMaxX, NewComp->BoardPosMaxX);
				SearchMaxY = max(SearchMaxY, NewComp->BoardPosMaxY);
			}
		}

		break;

	case 1:
		Comp1 = NULL;
		Comp2 = NULL;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
			CompInfo = Comp->Info;

			if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				if (!Comp1)
				{
					Comp1 = Comp;
					CompCnt1 = cnt;
				}
				else
				{
					if (!Comp2)
					{
						Comp2 = Comp;
						CompCnt2 = cnt;
					}
				}
			}
		}

		if ((!Comp1) || (!Comp2))
			return;

		MemSize = MemSizeComp(Comp1);
		memcpy(NewComp, Comp1, MemSize);
		MemSize = MemSizeComp(Comp2);
		memcpy(NewComp2, Comp2, MemSize);
		NewComp->CompOriginX = (float) (Comp2->CompOriginX);
		NewComp->CompOriginY = (float) (Comp2->CompOriginY);
		NewComp2->CompOriginX = (float) (Comp1->CompOriginX);
		NewComp2->CompOriginY = (float) (Comp1->CompOriginY);
		SetBoardPosComp(NewComp, 0);
		SetBoardPosComp(NewComp2, 0);
		SearchMinX = min(SearchMinX, NewComp->BoardPosMinX);
		SearchMinY = min(SearchMinY, NewComp->BoardPosMinY);
		SearchMaxX = max(SearchMaxX, NewComp->BoardPosMaxX);
		SearchMaxY = max(SearchMaxY, NewComp->BoardPosMaxY);
		SearchMinX = min(SearchMinX, NewComp2->BoardPosMinX);
		SearchMinY = min(SearchMinY, NewComp2->BoardPosMinY);
		SearchMaxX = max(SearchMaxX, NewComp2->BoardPosMaxX);
		SearchMaxY = max(SearchMaxY, NewComp2->BoardPosMaxY);
		FoundComps = 2;
		break;

	case 2:
	case 3:
		FoundComps = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
			CompInfo = Comp->Info;

			if ((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				FoundComps++;
				FirstCompNr = cnt;
// **************************************************************************************
// Recalc new position component -> NewComp
// **************************************************************************************
				MemSize = MemSizeComp(Comp);
				memcpy(NewComp, Comp, MemSize);
				NewComp->Rotation += (float) Rotation;

				if (NewComp->Rotation > 359.9)
					NewComp->Rotation -= 360.0;

				if (NewComp->Rotation < 0.0)
					NewComp->Rotation += 360.0;

				SetBoardPosComp(NewComp, 0);
				SearchMinX = min(SearchMinX, NewComp->BoardPosMinX);
				SearchMinY = min(SearchMinY, NewComp->BoardPosMinY);
				SearchMaxX = max(SearchMaxX, NewComp->BoardPosMaxX);
				SearchMaxY = max(SearchMaxY, NewComp->BoardPosMaxY);
			}
		}

		break;
	}

// **************************************************************************************
// **************************************************************************************

	if (mode == 0)
	{
		DrawCrossHair(16 + 2);

		for (Layer = 0; Layer < 32; Layer++)
		{
			for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					OkToRepaint = 1;
					xx1 = Trace->X;
					yy1 = Trace->Y;
					xx2 = Trace->Length;
					yy2a = Trace->ThickNess * 0.5 + Trace->Clearance;
					x1 = xx1;
					x2 = xx1;
					y1 = yy1;
					y2 = yy1 + xx2;

					if ((Trace->Info & 1) == 1)
					{
						RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
						x1 += divx;
						y1 += divy;
					}

					if ((Trace->Info & 2) == 2)
					{
						RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
						x2 += divx;
						y2 += divy;
					}

					SearchMinX = min(SearchMinX, min(x1, x2) - yy2a);
					SearchMaxX = max(SearchMaxX, max(x1, x2) + yy2a);
					SearchMinY = min(SearchMinY, min(y1, y2) - yy2a);
					SearchMaxY = max(SearchMaxY, max(y1, y2) + yy2a);
				}
			}

			for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					OkToRepaint = 1;
					xx1 = Trace->X;
					yy1 = Trace->Y;
					xx2 = Trace->Length;
					yy2a = Trace->ThickNess * 0.5 + Trace->Clearance;
					x1 = xx1;
					x2 = xx1 + xx2;
					y1 = yy1;
					y2 = yy1;

					if ((Trace->Info & 1) == 1)
					{
						RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
						x1 += divx;
						y1 += divy;
					}

					if ((Trace->Info & 2) == 2)
					{
						RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
						x2 += divx;
						y2 += divy;
					}

					SearchMinX = min(SearchMinX, min(x1, x2) - yy2a);
					SearchMaxX = max(SearchMaxX, max(x1, x2) + yy2a);
					SearchMinY = min(SearchMinY, min(y1, y2) - yy2a);
					SearchMaxY = max(SearchMaxY, max(y1, y2) + yy2a);
				}
			}

			for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					OkToRepaint = 1;
					xx1 = Trace->X;
					yy1 = Trace->Y;
					xx2 = Trace->Length;
					yy2a = Trace->ThickNess * 0.5 + Trace->Clearance;
					x1 = xx1;
					x2 = xx1 + xx2;
					y1 = yy1;
					y2 = yy1 - xx2;

					if ((Trace->Info & 1) == 1)
					{
						RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
						x1 += divx;
						y1 += divy;
					}

					if ((Trace->Info & 2) == 2)
					{
						RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
						x2 += divx;
						y2 += divy;
					}

					SearchMinX = min(SearchMinX, min(x1, x2) - yy2a);
					SearchMaxX = max(SearchMaxX, max(x1, x2) + yy2a);
					SearchMinY = min(SearchMinY, min(y1, y2) - yy2a);
					SearchMaxY = max(SearchMaxY, max(y1, y2) + yy2a);
				}
			}

			for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					OkToRepaint = 1;
					xx1 = Trace->X;
					yy1 = Trace->Y;
					xx2 = Trace->Length;
					yy2a = Trace->ThickNess * 0.5 + Trace->Clearance;
					x1 = xx1;
					x2 = xx1 + xx2;
					y1 = yy1;
					y2 = yy1 + xx2;

					if ((Trace->Info & 1) == 1)
					{
						RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
						x1 += divx;
						y1 += divy;
					}

					if ((Trace->Info & 2) == 2)
					{
						RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
						x2 += divx;
						y2 += divy;
					}

					SearchMinX = min(SearchMinX, min(x1, x2) - yy2a);
					SearchMaxX = max(SearchMaxX, max(x1, x2) + yy2a);
					SearchMinY = min(SearchMinY, min(y1, y2) - yy2a);
					SearchMaxY = max(SearchMaxY, max(y1, y2) + yy2a);
				}
			}
		}

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);
			ViaInfo = Via->Info;

			if ((ViaInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				OkToRepaint = 1;
				xx1 = Via->X;
				yy1 = Via->Y;
				RotatePointFromOtherPoint2(&xx1, &yy1, CentreComponentsX, CentreComponentsY, Rotation);
				xx1 += divx;
				yy1 += divy;
				yy2a = Via->ThickNess * 0.5 + Via->Clearance;
				SearchMinX = min(SearchMinX, xx1 - yy2a);
				SearchMaxX = max(SearchMaxX, xx1 + yy2a);
				SearchMinY = min(SearchMinY, yy1 - yy2a);
				SearchMaxY = max(SearchMaxY, yy1 + yy2a);
			}
		}

		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectLine->Layer < 32))
			{
				OkToRepaint = 1;
				yy2a = ObjectLine->LineThickNess * 0.5 + ObjectLine->Clearance;
				x1 = ObjectLine->X1;
				y1 = ObjectLine->Y1;
				x2 = ObjectLine->X2;
				y2 = ObjectLine->Y2;

				if ((ObjectLine->Info & 1) == 1)
				{
					RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
					x1 += divx;
					y1 += divy;
				}

				if ((ObjectLine->Info & 2) == 2)
				{
					RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
					x2 += divx;
					y2 += divy;
				}

				SearchMinX = min(SearchMinX, min(x1, x2) - yy2a);
				SearchMaxX = max(SearchMaxX, max(x1, x2) + yy2a);
				SearchMinY = min(SearchMinY, min(y1, y2) - yy2a);
				SearchMaxY = max(SearchMaxY, max(y1, y2) + yy2a);
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_FILLED)) == 0)
			        && ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)
			            || (ObjectArc->Layer == DRILL_UNPLATED_LAYER)))
			{
				OkToRepaint = 1;

				if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
				{
					xx1 = ObjectArc->CentreX;
					yy1 = ObjectArc->CentreY;
					RotatePointFromOtherPoint2(&xx1, &yy1, CentreComponentsX, CentreComponentsY, Rotation);
					xx1 += divx;
					yy1 += divy;
					yy2a = ObjectArc->Width * 0.5 + ObjectArc->Clearance;
					SearchMinX = min(SearchMinX, xx1 - yy2a);
					SearchMaxX = max(SearchMaxX, xx1 + yy2a);
					SearchMinY = min(SearchMinY, yy1 - yy2a);
					SearchMaxY = max(SearchMaxY, yy1 + yy2a);
				}
				else
				{
					xx1 = ObjectArc->CentreX;
					yy1 = ObjectArc->CentreY;
					RotatePointFromOtherPoint2(&xx1, &yy1, CentreComponentsX, CentreComponentsY, Rotation);
					xx1 += divx;
					yy1 += divy;
					yy2a = (ObjectArc->Width + ObjectArc->LineThickNess) * 0.5 + ObjectArc->Clearance;
					SearchMinX = min(SearchMinX, xx1 - yy2a);
					SearchMaxX = max(SearchMaxX, xx1 + yy2a);
					SearchMinY = min(SearchMinY, yy1 - yy2a);
					SearchMaxY = max(SearchMaxY, yy1 + yy2a);
				}
			}
		}
		
		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if ((SearchMaxX > AreaFill->minx) && (SearchMinX < AreaFill->maxx) && (SearchMaxY > AreaFill->miny)
				        && (SearchMinY < AreaFill->maxy))
				{
					CheckAreaFills = 1;
					OkToRepaint = 1;
				}
			}
		}

// **************************************************************************************
// **************************************************************************************
// Count objects in selection rectangle
// **************************************************************************************
// **************************************************************************************

		NrObjects4 = 0;
		CopyTracesFromRectWindowToObjects4(-1, 2);
		CopyViasFromRectWindowToObjects4(-1, 2);
		CopyCompObjectsFromRectWindowToObjects4(-1, 2);
		CopyOtherObjectsFromRectWindowToObjects4(-1, 16 + 2);	// All angle traces/arcs/drills/no routing keepouts

		if (AllocateMemObjects5(NrObjects4 + 1) != 0)
			return;

		memmove(Objects5, Objects4, NrObjects4 * sizeof(ObjectRecord));
		NrObjects5 = NrObjects4;

		/*
		  InitDrawingInfoObjects(Mult(1.0));
		  x1=(SearchMinX+SearchMaxX)*0.5;
		  y1=(SearchMinY+SearchMaxY)*0.5;
		  x2=SearchMaxX-SearchMinX;
		  y2=SearchMaxY-SearchMinY;
		  rect3(Mult(x1-Xoffset),DrawWindowMaxY-Mult(y1-Yoffset)-1,Mult(x2),Mult(y2));
		*/
	}

	SetWaitCursor();

	if (!OkToRePaintAfterCompMove)
	{
		OkToRepaint = 0;
		StartDrawingEditingWindow(0);
	}
	else
		OkToRepaint = 1;

	CompOverlap = 0;

// **************************************************************************************
// **************************************************************************************
// Components (selected) to be moved
// Every component will be moved to its new position
// **************************************************************************************
// **************************************************************************************

	TempLastActionNr = (int16) LastActionNr - 1;

	switch (mode)
	{
	case 0:
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
			CompInfo = Comp->Info;

			if (((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Comp->AddNr <= TempLastActionNr))
			{
				ok = 1;
				x1 = Comp->CompOriginX;
				y1 = Comp->CompOriginY;
				CompRot = GetRotationFromComp(Comp->CompMode);
				mirror = (Comp->TextVisibility & 8) >> 3;
				rx = Comp->CompNameOriginX;
				ry = Comp->CompNameOriginY;
				vx = Comp->CompValueOriginX;
				vy = Comp->CompValueOriginY;
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += divx;
				y1 += divy;
				MemSize = MemSizeComp(Comp);
				memmove(NewComp, Comp, MemSize);
				NewComp->CompOriginX = (float) x1;
				NewComp->CompOriginY = (float) y1;
				NewComp->CompNameOriginX = (float) rx;
				NewComp->CompNameOriginY = (float) ry;
				NewComp->CompValueOriginX = (float) vx;
				NewComp->CompValueOriginY = (float) vy;
				NewComp->Rotation += (float) Rotation;

				if (NewComp->Rotation > 359.9)
					NewComp->Rotation -= 360.0;

				if (NewComp->Rotation < 0.0)
					NewComp->Rotation += 360.0;

				if (AddComp(NewComp))
				{
					NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
					Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
					NrObjects = 0;
					ShapePinsToObject(NewComp2, 0.0, 0.0, 0, 0, 0, 0);

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

						if (Object->ObjectType == OBJECT_TEXT)
							ok = 1;

#endif

						if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
							SelectedNetsForMove[Object->NetNr] = 1;

						memmove(&CheckObject, Object, sizeof(ObjectRecord));
						FillPositionObject(&CheckObject);

						if (RecalcAreafillAfterInsert)
						{
							strcpy(str2, InfoStr);
							sprintf(InfoStr, SC(836, "Inserting objects in areafill"));
							RedrawInfoStr(0);
							res = InsertObjectInAreaFill(&CheckObject, CheckObject.Layer, CheckObject.NetNr, 2);

							if ((res == 1) && (OkToDrawAreaFills))
								OkToRepaint = 1;

							strcpy(InfoStr, str2);
							RedrawInfoStr(0);
						}

						Object5OverlapNr = 0;
						Count = 0;

						while ((Object5OverlapNr != -1) && (Count < 1000))
						{
							if ((Object5OverlapNr = CheckObjectOverlappedFromObjects(&CheckObject, 5)) != -1)
							{
								Object5 = &((*Objects5)[Object5OverlapNr]);

								if (Object5->CompNr != cnt)
								{
									switch (Object5->ObjectType)
									{
									case PIN_PUT_THROUGH_ROUND:
									case PIN_SMD_ROUND:
									case PIN_PUT_THROUGH_SQUARE:
									case DRILL:
									case DRILL_UNPLATED:
									case PIN_SMD_RECT:
									case PIN_LINE_VER:
									case PIN_LINE_HOR:
									case PIN_LINE_DIAG1:
									case PIN_LINE_DIAG2:
									case PIN_SMD_POLYGON:
									case PIN_PUT_THROUGH_POLYGON:
										CompOverlap = 1;
										break;
									}

									Object5->ObjectType |= 1;
								}

								Count++;
							}

							ok = 1;
						}
					}

					Comp->Info &= ~OBJECT_SELECTED;
					NewComp2->Info &= ~OBJECT_SELECTED;
					Comp->TextVisibility &= ~(1 + 16);
					NewComp2->TextVisibility &= ~(1 + 16);

					if (!OkToRepaint)
					{
						SetBackGroundActive(0);
						DrawComp(Comp, 0.0, 0.0, 0, 0x201);

						if (OkToDrawClearances)
						{
							InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
							DrawPinsComp(Comp, 0.0, 0.0, 0, 0x240 + 8);
						}

						DrawComp(NewComp2, 0.0, 0.0, 0, 0x200);

						if (OkToDrawClearances)
						{
							InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
							DrawPinsComp(NewComp2, 0.0, 0.0, 0, 0x240 + 8);
						}
					}

					//        Comp->Info&=~OBJECT_SELECTED;
					Comp->Info |= OBJECT_NOT_VISIBLE;
					Comp->DeleteNr = (int16) LastActionNr;
					DataBaseChanged = 1;
				}
				else
					ok = 1;

// **************************************************************************************
// Remove traces/vias under pads moved components
// **************************************************************************************

				memset(&NewConnection, 0, sizeof(ConnectionsRecord));

				for (cnt2 = 0; cnt2 < NrObjects5; cnt2++)
				{
					Object5 = &((*Objects5)[cnt2]);
					DrawCode = DrawLayerCode[Object5->Layer];

					if ((Object5->ObjectType & 1) == 1)
					{
						switch (Object5->ObjectType & 0xfffe)
						{
						case TRACE_VER:
							Trace = &((*VerTraces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_HOR:
							Trace = &((*HorTraces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_DIAG1:
							Trace = &((*Diag1Traces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_DIAG2:
							Trace = &((*Diag2Traces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case VIA_PUT_THROUGH_ROUND:
							Via = &((*Vias)[Object5->TraceNr]);
							Via->Info |= OBJECT_NOT_VISIBLE;
							Via->DeleteNr = (int16) LastActionNr;

							if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
								SelectedNetsForMove[Via->NetNr] = 1;

							DataBaseChanged = 1;
							OkToRepaint = 1;
							break;

						case TRACE_ALL_ANGLE:
							ObjectLine = &((*ObjectLines)[Object5->TraceNr]);
							ObjectLine->Info |= OBJECT_NOT_VISIBLE;
							ObjectLine->DeleteNr = (int16) LastActionNr;

							if ((ObjectLine->NetNr >= 0) && (ObjectLine->NetNr < Design.NrNets))
								SelectedNetsForMove[ObjectLine->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_ARC:
							ObjectArc = &((*ObjectArcs)[Object5->TraceNr]);

							if ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)
							        || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
							{
								ObjectArc->Info |= OBJECT_NOT_VISIBLE;
								ObjectArc->DeleteNr = (int16) LastActionNr;
								DataBaseChanged = 1;

								if ((ObjectArc->NetNr >= 0) && (ObjectArc->NetNr < Design.NrNets))
									SelectedNetsForMove[ObjectArc->NetNr] = 1;

								if (CheckIfCopperObjectArc(ObjectArc, 0))
									OkToRepaint = 1;
							}

							break;
						}
					}
				}
			}
		}

		break;

	case 1:
		for (cnt = 0; cnt < 2; cnt++)
		{
			ok = 1;
			Comp1 = (CompRecord *) & (CompsMem[(*Comps)[CompCnt1]]);
			Comp2 = (CompRecord *) & (CompsMem[(*Comps)[CompCnt2]]);

			if (cnt == 0)
			{
				Comp = Comp1;
				MemSize = MemSizeComp(Comp1);
				memmove(NewComp, Comp1, MemSize);
				NewComp->CompOriginX = (float) (Comp2->CompOriginX);
				NewComp->CompOriginY = (float) (Comp2->CompOriginY);
			}
			else
			{
				Comp = Comp2;
				MemSize = MemSizeComp(Comp2);
				memmove(NewComp, Comp2, MemSize);
				NewComp->CompOriginX = (float) (Comp1->CompOriginX);
				NewComp->CompOriginY = (float) (Comp1->CompOriginY);
			}

			if (AddComp(NewComp))
			{
				NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);

				if (cnt == 0)
					Comp = (CompRecord *) & (CompsMem[(*Comps)[CompCnt1]]);
				else
					Comp = (CompRecord *) & (CompsMem[(*Comps)[CompCnt2]]);

				NrObjects = 0;
				ShapePinsToObject(NewComp2, 0.0, 0.0, 0, 0, 0, 0);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

					if (Object->ObjectType == OBJECT_TEXT)
						ok = 1;

#endif

					if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
						SelectedNetsForMove[Object->NetNr] = 1;

					memmove(&CheckObject, Object, sizeof(ObjectRecord));
					FillPositionObject(&CheckObject);

					if (RecalcAreafillAfterInsert)
					{
						strcpy(str2, InfoStr);
						sprintf(InfoStr, SC(836, "Inserting objects in areafill"));
						RedrawInfoStr(0);
						res = InsertObjectInAreaFill(&CheckObject, CheckObject.Layer, CheckObject.NetNr, 2);

						if ((res == 1) && (OkToDrawAreaFills))
							OkToRepaint = 1;

						strcpy(InfoStr, str2);
						RedrawInfoStr(0);
					}

					Object5OverlapNr = 0;
					Count = 0;

					while ((Object5OverlapNr != -1) && (Count < 1000))
					{
						if ((Object5OverlapNr = CheckObjectOverlappedFromObjects(&CheckObject, 5)) != -1)
						{
							Object5 = &((*Objects5)[Object5OverlapNr]);

							if (Object5->CompNr != cnt)
							{
								switch (Object5->ObjectType)
								{
								case PIN_PUT_THROUGH_ROUND:
								case PIN_SMD_ROUND:
								case PIN_PUT_THROUGH_SQUARE:
								case DRILL:
								case DRILL_UNPLATED:
								case PIN_SMD_RECT:
								case PIN_LINE_VER:
								case PIN_LINE_HOR:
								case PIN_LINE_DIAG1:
								case PIN_LINE_DIAG2:
								case PIN_SMD_POLYGON:
								case PIN_PUT_THROUGH_POLYGON:
									CompOverlap = 1;
									break;
								}

								Object5->ObjectType |= 1;
							}

							Count++;
						}

						ok = 1;
					}
				}

				Comp->Info &= ~OBJECT_SELECTED;
				NewComp2->Info &= ~OBJECT_SELECTED;
				Comp->TextVisibility &= ~(1 + 16);
				NewComp2->TextVisibility &= ~(1 + 16);

				if (!OkToRepaint)
				{
					SetBackGroundActive(0);
					DrawComp(Comp, 0.0, 0.0, 0, 0x201);

					if (OkToDrawClearances)
					{
						InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
						DrawPinsComp(Comp, 0.0, 0.0, 0, 0x240 + 8);
					}

					DrawComp(NewComp2, 0.0, 0.0, 0, 0x200);

					if (OkToDrawClearances)
					{
						InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
						DrawPinsComp(NewComp2, 0.0, 0.0, 0, 0x240 + 8);
					}
				}

				//        Comp->Info&=~OBJECT_SELECTED;
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
				DataBaseChanged = 1;
			}
			else
				ok = 1;

// **************************************************************************************
// Remove traces/vias under pads moved components
// **************************************************************************************
			memset(&NewConnection, 0, sizeof(ConnectionsRecord));

			for (cnt2 = 0; cnt2 < NrObjects5; cnt2++)
			{
				Object5 = &((*Objects5)[cnt2]);
				DrawCode = DrawLayerCode[Object5->Layer];

				if ((Object5->ObjectType & 1) == 1)
				{
					switch (Object5->ObjectType & 0xfffe)
					{
					case TRACE_VER:
						Trace = &((*VerTraces[Object5->Layer])[Object5->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						Trace->DeleteNr = (int16) LastActionNr;

						if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
							SelectedNetsForMove[Trace->NetNr] = 1;

						OkToRepaint = 1;
						break;

					case TRACE_HOR:
						Trace = &((*HorTraces[Object5->Layer])[Object5->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						Trace->DeleteNr = (int16) LastActionNr;

						if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
							SelectedNetsForMove[Trace->NetNr] = 1;

						OkToRepaint = 1;
						break;

					case TRACE_DIAG1:
						Trace = &((*Diag1Traces[Object5->Layer])[Object5->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						Trace->DeleteNr = (int16) LastActionNr;

						if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
							SelectedNetsForMove[Trace->NetNr] = 1;

						OkToRepaint = 1;
						break;

					case TRACE_DIAG2:
						Trace = &((*Diag2Traces[Object5->Layer])[Object5->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						Trace->DeleteNr = (int16) LastActionNr;

						if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
							SelectedNetsForMove[Trace->NetNr] = 1;

						OkToRepaint = 1;
						break;

					case VIA_PUT_THROUGH_ROUND:
						Via = &((*Vias)[Object5->TraceNr]);
						Via->Info |= OBJECT_NOT_VISIBLE;
						Via->DeleteNr = (int16) LastActionNr;

						if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
							SelectedNetsForMove[Via->NetNr] = 1;

						DataBaseChanged = 1;
						OkToRepaint = 1;
						break;

					case TRACE_ALL_ANGLE:
						ObjectLine = &((*ObjectLines)[Object5->TraceNr]);
						ObjectLine->Info |= OBJECT_NOT_VISIBLE;
						ObjectLine->DeleteNr = (int16) LastActionNr;

						if ((ObjectLine->NetNr >= 0) && (ObjectLine->NetNr < Design.NrNets))
							SelectedNetsForMove[ObjectLine->NetNr] = 1;

						OkToRepaint = 1;
						break;

					case TRACE_ARC:
						ObjectArc = &((*ObjectArcs)[Object5->TraceNr]);

						if ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)
						        || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
						{
							ObjectArc->Info |= OBJECT_NOT_VISIBLE;
							ObjectArc->DeleteNr = (int16) LastActionNr;
							DataBaseChanged = 1;

							if ((ObjectArc->NetNr >= 0) && (ObjectArc->NetNr < Design.NrNets))
								SelectedNetsForMove[ObjectArc->NetNr] = 1;

							if (CheckIfCopperObjectArc(ObjectArc, 0))
								OkToRepaint = 1;
						}

						break;
					}
				}
			}
		}

		break;

	case 2:
	case 3:
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
			CompInfo = Comp->Info;

			if (((CompInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Comp->AddNr <= TempLastActionNr))
			{
				ok = 1;
				MemSize = MemSizeComp(Comp);
				memmove(NewComp, Comp, MemSize);
				NewComp->Rotation += (float) Rotation;

				if (NewComp->Rotation > 359.9)
					NewComp->Rotation -= 360.0;

				if (NewComp->Rotation < 0.0)
					NewComp->Rotation += 360.0;

				if (AddComp(NewComp))
				{
					NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
					Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
					NrObjects = 0;
					ShapePinsToObject(NewComp2, 0.0, 0.0, 0, 0, 0, 0);

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

						if (Object->ObjectType == OBJECT_TEXT)
							ok = 1;

#endif

						if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
							SelectedNetsForMove[Object->NetNr] = 1;

						memmove(&CheckObject, Object, sizeof(ObjectRecord));
						FillPositionObject(&CheckObject);

						if (RecalcAreafillAfterInsert)
						{
							strcpy(str2, InfoStr);
							sprintf(InfoStr, SC(836, "Inserting objects in areafill"));
							RedrawInfoStr(0);
							res = InsertObjectInAreaFill(&CheckObject, CheckObject.Layer, CheckObject.NetNr, 2);

							if ((res == 1) && (OkToDrawAreaFills))
								OkToRepaint = 1;

							strcpy(InfoStr, str2);
							RedrawInfoStr(0);
						}

						Object5OverlapNr = 0;
						Count = 0;

						while ((Object5OverlapNr != -1) && (Count < 1000))
						{
							if ((Object5OverlapNr = CheckObjectOverlappedFromObjects(&CheckObject, 5)) != -1)
							{
								Object5 = &((*Objects5)[Object5OverlapNr]);

								if (Object5->CompNr != cnt)
								{
									switch (Object5->ObjectType)
									{
									case PIN_PUT_THROUGH_ROUND:
									case PIN_SMD_ROUND:
									case PIN_PUT_THROUGH_SQUARE:
									case DRILL:
									case DRILL_UNPLATED:
									case PIN_SMD_RECT:
									case PIN_LINE_VER:
									case PIN_LINE_HOR:
									case PIN_LINE_DIAG1:
									case PIN_LINE_DIAG2:
									case PIN_SMD_POLYGON:
									case PIN_PUT_THROUGH_POLYGON:
										CompOverlap = 1;
										break;
									}

									Object5->ObjectType |= 1;
								}

								Count++;
							}

							ok = 1;
						}
					}

					Comp->Info &= ~OBJECT_SELECTED;
					NewComp2->Info &= ~OBJECT_SELECTED;
					Comp->TextVisibility &= ~(1 + 16);
					NewComp2->TextVisibility &= ~(1 + 16);

					if (!OkToRepaint)
					{
						SetBackGroundActive(0);
						DrawComp(Comp, 0.0, 0.0, 0, 0x201);

						if (OkToDrawClearances)
						{
							InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
							DrawPinsComp(Comp, 0.0, 0.0, 0, 0x240 + 8);
						}

						DrawComp(NewComp2, 0.0, 0.0, 0, 0x200);

						if (OkToDrawClearances)
						{
							InitDrawingObject(0, CLEARANCE_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
							DrawPinsComp(NewComp2, 0.0, 0.0, 0, 0x240 + 8);
						}
					}

					//        Comp->Info&=~OBJECT_SELECTED;
					Comp->Info |= OBJECT_NOT_VISIBLE;
					Comp->DeleteNr = (int16) LastActionNr;
					DataBaseChanged = 1;
				}
				else
					ok = 1;

// **************************************************************************************
// Remove traces/vias under pads moved components
// **************************************************************************************

				memset(&NewConnection, 0, sizeof(ConnectionsRecord));

				for (cnt2 = 0; cnt2 < NrObjects5; cnt2++)
				{
					Object5 = &((*Objects5)[cnt2]);
					DrawCode = DrawLayerCode[Object5->Layer];

					if ((Object5->ObjectType & 1) == 1)
					{
						switch (Object5->ObjectType & 0xfffe)
						{
						case TRACE_VER:
							Trace = &((*VerTraces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_HOR:
							Trace = &((*HorTraces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_DIAG1:
							Trace = &((*Diag1Traces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_DIAG2:
							Trace = &((*Diag2Traces[Object5->Layer])[Object5->TraceNr]);
							Trace->Info |= OBJECT_NOT_VISIBLE;
							Trace->DeleteNr = (int16) LastActionNr;

							if ((Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
								SelectedNetsForMove[Trace->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case VIA_PUT_THROUGH_ROUND:
							Via = &((*Vias)[Object5->TraceNr]);
							Via->Info |= OBJECT_NOT_VISIBLE;
							Via->DeleteNr = (int16) LastActionNr;

							if ((Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
								SelectedNetsForMove[Via->NetNr] = 1;

							DataBaseChanged = 1;
							OkToRepaint = 1;
							break;

						case TRACE_ALL_ANGLE:
							ObjectLine = &((*ObjectLines)[Object5->TraceNr]);
							ObjectLine->Info |= OBJECT_NOT_VISIBLE;
							ObjectLine->DeleteNr = (int16) LastActionNr;

							if ((ObjectLine->NetNr >= 0) && (ObjectLine->NetNr < Design.NrNets))
								SelectedNetsForMove[ObjectLine->NetNr] = 1;

							OkToRepaint = 1;
							break;

						case TRACE_ARC:
							ObjectArc = &((*ObjectArcs)[Object5->TraceNr]);

							if ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)
							        || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
							{
								ObjectArc->Info |= OBJECT_NOT_VISIBLE;
								ObjectArc->DeleteNr = (int16) LastActionNr;
								DataBaseChanged = 1;

								if ((ObjectArc->NetNr >= 0) && (ObjectArc->NetNr < Design.NrNets))
									SelectedNetsForMove[ObjectArc->NetNr] = 1;

								if (CheckIfCopperObjectArc(ObjectArc, 0))
									OkToRepaint = 1;
							}

							break;
						}
					}
				}
			}
		}

		break;
	}

// **************************************************************************************
// **************************************************************************************
// Drag traces/vias
// **************************************************************************************
// **************************************************************************************

	if ((divx != 0.0) || (divy != 0.))
	{
		for (Layer = 0; Layer < 32; Layer++)
		{
			IndexP = IndexVerTraces[Layer];

			for (cnt = 0; cnt < NrIndexesVerTraces[Layer]; cnt++)
			{
				Trace = &((*VerTraces[Layer])[*IndexP]);
				TraceInfo = Trace->Info;
				x1 = Trace->X;
				y1 = Trace->Y;
				x2 = x1;
				y2 = y1 + Trace->Length;

				if ((TraceInfo & 1) == 1)
				{
					RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
					x1 += divx;
					y1 += divy;
				}

				if ((TraceInfo & 2) == 2)
				{
					RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
					x2 += divx;
					y2 += divy;
				}

				MoveTrace(Trace, x1, y1, x2, y2, Layer, TRACE_VER, CheckAreaFills, SelectedNetsForMove);
				IndexP += 1;
				OkToRepaint = 1;
			}

			IndexP = IndexHorTraces[Layer];

			for (cnt = 0; cnt < NrIndexesHorTraces[Layer]; cnt++)
			{
				Trace = &((*HorTraces[Layer])[*IndexP]);
				TraceInfo = Trace->Info;
				x1 = Trace->X;
				y1 = Trace->Y;
				x2 = x1 + Trace->Length;
				y2 = y1;

				if ((TraceInfo & 1) == 1)
				{
					RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
					x1 += divx;
					y1 += divy;
				}

				if ((TraceInfo & 2) == 2)
				{
					RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
					x2 += divx;
					y2 += divy;
				}

				MoveTrace(Trace, x1, y1, x2, y2, Layer, TRACE_HOR, CheckAreaFills, SelectedNetsForMove);
				IndexP += 1;
				OkToRepaint = 1;
			}

			IndexP = IndexDiag1Traces[Layer];

			for (cnt = 0; cnt < NrIndexesDiag1Traces[Layer]; cnt++)
			{
				Trace = &((*Diag1Traces[Layer])[*IndexP]);
				TraceInfo = Trace->Info;
				x1 = Trace->X;
				y1 = Trace->Y;
				x2 = x1 + Trace->Length;
				y2 = y1 - Trace->Length;

				if ((TraceInfo & 1) == 1)
				{
					RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
					x1 += divx;
					y1 += divy;
				}

				if ((TraceInfo & 2) == 2)
				{
					RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
					x2 += divx;
					y2 += divy;
				}

				MoveTrace(Trace, x1, y1, x2, y2, Layer, TRACE_DIAG1, CheckAreaFills, SelectedNetsForMove);
				IndexP += 1;
				OkToRepaint = 1;
			}

			IndexP = IndexDiag2Traces[Layer];

			for (cnt = 0; cnt < NrIndexesDiag2Traces[Layer]; cnt++)
			{
				Trace = &((*Diag2Traces[Layer])[*IndexP]);
				TraceInfo = Trace->Info;
				x1 = Trace->X;
				y1 = Trace->Y;
				x2 = x1 + Trace->Length;
				y2 = y1 + Trace->Length;

				if ((TraceInfo & 1) == 1)
				{
					RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
					x1 += divx;
					y1 += divy;
				}

				if ((TraceInfo & 2) == 2)
				{
					RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
					x2 += divx;
					y2 += divy;
				}

				MoveTrace(Trace, x1, y1, x2, y2, Layer, TRACE_DIAG2, CheckAreaFills, SelectedNetsForMove);
				IndexP += 1;
				OkToRepaint = 1;
			}
		}

		IndexP = IndexVias;
		dikte = 50000.0;

		for (cnt = 0; cnt < NrIndexesVias; cnt++)
		{
			Via = &((*Vias)[*IndexP]);
			ViaInfo = Via->Info;
			x1 = Via->X;
			y1 = Via->Y;
			RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
			x1 += divx;
			y1 += divy;

			Via->Info = 0;
			Via->Info &= ~OBJECT_SELECTED;
			ViaObject.ObjectType = VIA_PUT_THROUGH_ROUND;
			ViaObject.x1 = x1;
			ViaObject.y1 = y1;
			ViaObject.x2 = Via->ThickNess;
			ViaObject.Clearance = Via->Clearance;
			ViaObject.NetNr = Via->NetNr;
			ViaObject.Layer = -1;
			NewVia.Info = 0;
			FillPositionObject(&ViaObject);
			SelectedNetsForMove[Via->NetNr] = 1;

			if ((res = CheckObjectOverlappedFromObjects(&ViaObject, 5)) == -1)
			{
				memmove(&NewVia, Via, sizeof(ViaRecord));
				NewVia.X = (float) x1;
				NewVia.Y = (float) y1;
				NewVia.AddNr = (int16) LastActionNr;
				Net = &((*Nets)[Via->NetNr]);

				if ((Net->Info & OBJECT_HIGHLITED) == OBJECT_HIGHLITED)
					NewVia.Info |= OBJECT_HIGHLITED;

				AddVia(&NewVia);

				if (CheckAreaFills)
				{
					if (RecalcAreafillAfterInsert)
					{
						res = InsertObjectInAreaFill(&ViaObject, -1, ViaObject.NetNr, 2);

						if ((res == 1) && (OkToDrawAreaFills))
							OkToRepaint = 1;
					}
				}
			}
			else
				Object5 = &((*Objects5)[res]);

			DataBaseChanged = 1;
			ZeroUnusedObjects(0);
			Via = &((*Vias)[*IndexP]);
			Via->Info |= OBJECT_NOT_VISIBLE;
			Via->DeleteNr = (int16) LastActionNr;
			IndexP += 1;
			OkToRepaint = 1;
		}

// **************************************************************************************
// **************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectLine->Info &= ~OBJECT_SELECTED;
				x1 = ObjectLine->X1;
				y1 = ObjectLine->Y1;
				x2 = ObjectLine->X2;
				y2 = ObjectLine->Y2;

				if ((ObjectLine->Info & 1) == 1)
				{
					RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
					x1 += divx;
					y1 += divy;
				}

				if ((ObjectLine->Info & 2) == 2)
				{
					RotatePointFromOtherPoint2(&x2, &y2, CentreComponentsX, CentreComponentsY, Rotation);
					x2 += divx;
					y2 += divy;
				}

				memset(&CheckObject, 0, sizeof(ObjectRecord));
				CheckObject.x1 = x1;
				CheckObject.y1 = y1;
				CheckObject.x2 = x2;
				CheckObject.y2 = y2;
				CheckObject.ObjectType = TRACE_ALL_ANGLE;
				CheckObject.Layer = ObjectLine->Layer;
				CheckObject.Clearance = ObjectLine->Clearance;
				CheckObject.Thickness = ObjectLine->LineThickNess;

				FillPositionObject(&CheckObject);

				if ((res = CheckObjectOverlappedFromObjects(&CheckObject, 5)) == -1)
				{
					memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
					NewObjectLine.X1 = (float) x1;
					NewObjectLine.Y1 = (float) y1;
					NewObjectLine.X2 = (float) x2;
					NewObjectLine.Y2 = (float) y2;

					if (AddObjectLine(&NewObjectLine))
					{
						if ((NewObjectLine.NetNr >= 0) && (NewObjectLine.NetNr < Design.NrNets))
						{
							SelectedNetsForMove[NewObjectLine.NetNr] = 1;

							if (CheckAreaFills)
							{
								if (RecalcAreafillAfterInsert)
									res = InsertObjectInAreaFill(&CheckObject, -1, NewObjectLine.NetNr, 2);
							}
						}

						ObjectLine = &((*ObjectLines)[cnt]);
						ObjectLine->Info |= OBJECT_NOT_VISIBLE;
						ObjectLine->DeleteNr = (int16) LastActionNr;
					}
				}

				OkToRepaint = 1;
			}
		}

// **************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				ObjectArc->Info &= ~OBJECT_SELECTED;
				x1 = ObjectArc->CentreX;
				y1 = ObjectArc->CentreY;
				x2 = ObjectArc->Width;
				y2 = ObjectArc->Height;
				RotatePointFromOtherPoint2(&x1, &y1, CentreComponentsX, CentreComponentsY, Rotation);
				x1 += divx;
				y1 += divy;
				memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));

				if ((ObjectArc->Layer == DRILL_UNPLATED_LAYER) || (ObjectArc->Layer == DRILL_LAYER))
				{
					memset(&CheckObject, 0, sizeof(ObjectRecord));
					CheckObject.x1 = x1;
					CheckObject.y1 = y1;
					CheckObject.x2 = x2;
					CheckObject.ObjectType = DRILL;
					CheckObject.Layer = -1;
					CheckObject.Clearance = ObjectArc->Clearance;
					FillPositionObject(&CheckObject);

					if ((res = CheckObjectOverlappedFromObjects(&CheckObject, 5)) == -1)
					{
						memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
						NewObjectArc.CentreX = (float) x1;
						NewObjectArc.CentreY = (float) y1;

						if (AddObjectArc(&NewObjectArc))
						{
							if ((NewObjectArc.NetNr >= 0) && (NewObjectArc.NetNr < Design.NrNets))
							{
								SelectedNetsForMove[NewObjectArc.NetNr] = 1;

								if (CheckAreaFills)
								{
									if (RecalcAreafillAfterInsert)
										res = InsertObjectInAreaFill(&CheckObject, -1, NewObjectArc.NetNr, 2);
								}
							}

							ObjectArc = &((*ObjectArcs)[cnt]);
							ObjectArc->Info |= OBJECT_NOT_VISIBLE;
							ObjectArc->DeleteNr = (int16) LastActionNr;
						}
					}
				}
				else
				{
					x3 = ObjectArc->StartDiffX;
					y3 = ObjectArc->StartDiffY;
					x4 = ObjectArc->EndDiffX;
					y4 = ObjectArc->EndDiffY;
					RotatePointFromOtherPoint2(&x3, &y3, 0.0, 0.0, Rotation);
					RotatePointFromOtherPoint2(&x4, &y4, 0.0, 0.0, Rotation);

					CheckObject.x1 = x1;
					CheckObject.y1 = y1;
					CheckObject.x2 = x2;
					CheckObject.y2 = y2;
					CheckObject.x3 = x3;
					CheckObject.y3 = y3;
					CheckObject.x4 = x4;
					CheckObject.y4 = y4;
					CheckObject.Thickness = ObjectArc->LineThickNess;
					CheckObject.ObjectType = TRACE_ARC;
					CheckObject.Layer = Layer;
					CheckObject.Clearance = ObjectArc->Clearance;
					FillPositionObject(&CheckObject);

					if ((res = CheckObjectOverlappedFromObjects(&CheckObject, 5)) == -1)
					{
						memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
						NewObjectArc.CentreX = (float) x1;
						NewObjectArc.CentreY = (float) y1;
						NewObjectArc.StartDiffX = (float) x3;
						NewObjectArc.StartDiffY = (float) y3;
						NewObjectArc.EndDiffX = (float) x4;
						NewObjectArc.EndDiffY = (float) y4;

						if (AddObjectArc(&NewObjectArc))
						{
							if ((NewObjectArc.NetNr >= 0) && (NewObjectArc.NetNr < Design.NrNets))
							{
								SelectedNetsForMove[NewObjectArc.NetNr] = 1;

								if (CheckAreaFills)
								{
									if (RecalcAreafillAfterInsert)
									{
										res = InsertObjectInAreaFill(&CheckObject, -1, NewObjectArc.NetNr, 2);

										if ((res == 1) && (OkToDrawAreaFills))
											OkToRepaint = 1;
									}
								}
							}

							ObjectArc = &((*ObjectArcs)[cnt]);
							ObjectArc->Info |= OBJECT_NOT_VISIBLE;
							ObjectArc->DeleteNr = (int16) LastActionNr;
						}
					}
				}

				OkToRepaint = 1;
			}
		}


// **************************************************************************************
// **************************************************************************************
// Drag areafills
// **************************************************************************************
// **************************************************************************************
		if (PlaceMovedAreafills
		        (CurrentX, CurrentY, CurrentX2, CurrentY2, CentreComponentsX, CentreComponentsY, Rotation,
		         SelectedNetsForMove, OkToRepaint))
			OkToRepaint = 1;
	}

// NrConnections

	ok = 1;

	if ((!OkToRepaint) && (OutputDisplay))
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}

	if (MaxNrObjects5 > 4096)
		DeAllocateMemObjects5();

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

	SetScrollPageSize();
	SetScrollPosition();
	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (SelectedNetsForMove[cnt] == 1)
		{
			if (!OkToRepaint)
				ReCalcConnectionsNet(cnt, 0, 0);
			else
				ReCalcConnectionsNet(cnt, 0, 1);

			CheckForEscape();
			SetWaitCursor();
			cnt2++;
		}
	}

	if (FoundComps == 1)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[FirstCompNr]]);
		fminx = Comp->BoardPosMinX;
		fmaxx = Comp->BoardPosMaxX;
		fminy = Comp->BoardPosMinY;
		fmaxy = Comp->BoardPosMaxY;

		if (OkToDrawCompValue)
		{
			GetTextMinMaxCompValue(Comp);
			fminx = min(fminx, TextMinX);
			fmaxx = max(fmaxx, TextMaxX);
			fminy = min(fminy, TextMinY);
			fmaxy = max(fmaxy, TextMaxY);
		}

		if (OkToDrawCompReference)
		{
			GetTextMinMaxCompReference(Comp);
			fminx = min(fminx, TextMinX);
			fmaxx = max(fmaxx, TextMaxX);
			fminy = min(fminy, TextMinY);
			fmaxy = max(fmaxy, TextMaxY);
		}

		CompRect.left = max(DrawWindowMinX, MultX(fminx) - 2);
		CompRect.right = min(DrawWindowMaxX, MultX(fmaxx) + 2);
		CompRect.top = max(DrawWindowMinY, MultY(fmaxy) - 2);
		CompRect.bottom = min(DrawWindowMaxY, MultY(fminy) + 2);
		InvalidateRect(PCBWindow, &CompRect, 0);
		ok = 1;
	}

	if (CompOverlap)
	{
		Beep(1000, 200);
		MessageBoxOwn(PCBWindow, SC(837, "Component pins overlap"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
	}

	RePaint();
	SetNormalCursor();
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


void FillAreaPosition(int32 SizeX, int32 SizeY, int32 CheckX, int32 CheckY, uint8 * AreaBuf, int32 mode)
{
	int32 StartX, StartY, BufPos1, BufPos2, y;

	StartX = CheckX - SizeX / 2;
	StartY = CheckY - SizeY / 2;

	if (StartX < 0)
	{
		SizeX += StartX;
		StartX = 0;
	}

	if (StartX + SizeX >= MaxPixels)
		SizeX = MaxPixels - StartX;

	if (SizeX <= 0)
		return;

	BufPos1 = StartY * MaxPixels;

	for (y = StartY; y < StartY + SizeY; y++)
	{
		if ((y >= 0) && (y < MaxPixels))
		{
			BufPos2 = BufPos1 + StartX;
			memset(&AreaBuf[BufPos2], 1, SizeX);
		}

		BufPos1 += MaxPixels;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckAreaPosition(int32 SizeX, int32 SizeY, int32 CheckX, int32 CheckY, uint8 * AreaBuf, int32 mode)
{
	int32 StartX, StartY, BufPos1, BufPos2, CountX, y;
	uint8 *mempos;

//  sprintf(str,"%i,%i\n",CheckX,CheckY);
//  OutputDebugString(str);

	StartX = CheckX - SizeX / 2;
	StartY = CheckY - SizeY / 2;

	if (StartX < 0)
	{
		SizeX += StartX;
		StartX = 0;
	}

	if (StartX + SizeX >= MaxPixels)
		SizeX = MaxPixels - StartX;

	BufPos1 = StartY * MaxPixels;

	for (y = StartY; y < StartY + SizeY; y++)
	{
		if ((y >= 0) && (y < MaxPixels))
		{
			BufPos2 = BufPos1 + StartX;
			mempos = &AreaBuf[BufPos2];
			CountX = 0;

			while ((CountX < SizeX) && (*mempos == 0))
			{
				CountX++;
				mempos++;
			}

			if (CountX < SizeX)
				return -1;
		}

		BufPos1 += MaxPixels;
	}

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 FindAreaPosition(int32 SizeX, int32 SizeY, int32 * FoundX, int32 * FoundY, uint8 * AreaBuf, int32 mode)
{
	int32 NrColumns, NrLines, CountX, CountY, cntx, cnty, ok, CentreX, CentreY, NrRings, Found = 0;

	NrColumns = (MaxPixels - SizeX) / 2;
	NrLines = (MaxPixels - SizeY) / 2;
	NrRings = min(NrColumns, NrLines);
	CountX = 2;
	CountY = 2;
	CentreX = MaxPixels / 2;
	CentreY = MaxPixels / 2;

//  NrRings=2;


	if (CheckAreaPosition(SizeX, SizeY, CentreX, CentreY, AreaBuf, 0) == 0)
	{
		Found = 1;
		*FoundX = CentreX;
		*FoundY = CentreY;
	}

	if (!Found)
	{
		CentreX = MaxPixels / 2;
		CentreY = MaxPixels / 2 - 1;

		if (CheckAreaPosition(SizeX, SizeY, CentreX, CentreY, AreaBuf, 0) == 0)
		{
			Found = 1;
			*FoundX = CentreX;
			*FoundY = CentreY;
		}
	}

	if (!Found)
	{
		CentreX = MaxPixels / 2 - 1;
		CentreY = MaxPixels / 2 - 1;

		if (CheckAreaPosition(SizeX, SizeY, CentreX, CentreY, AreaBuf, 0) == 0)
		{
			Found = 1;
			*FoundX = CentreX;
			*FoundY = CentreY;
		}
	}

	if (!Found)
	{
		CentreX = MaxPixels / 2 - 1;
		CentreY = MaxPixels / 2;

		if (CheckAreaPosition(SizeX, SizeY, CentreX, CentreY, AreaBuf, 0) == 0)
		{
			Found = 1;
			*FoundX = CentreX;
			*FoundY = CentreY;
		}
	}


	if (!Found)
	{
		CentreX = MaxPixels / 2;
		CentreY = MaxPixels / 2;

		while ((!Found) && (NrRings > 0))
		{
			CentreX += 1;
			CentreY += 1;
// **************************************************************************************
// Check right pixels
			cnty = CentreY;

			while ((cnty >= MaxPixels - 1 - CentreY) && (!Found))
			{
				if (CheckAreaPosition(SizeX, SizeY, CentreX, cnty, AreaBuf, 0) == 0)
				{
					Found = 1;
					*FoundX = CentreX;
					*FoundY = cnty;
				}
				else
					cnty--;

				ok = 1;
			}

// **************************************************************************************
// Check bottom pixels
			if (!Found)
			{
				cntx = CentreX - 1;

				while ((cntx > MaxPixels - 1 - CentreX) && (!Found))
				{
					if (CheckAreaPosition(SizeX, SizeY, cntx, MaxPixels - 1 - CentreY, AreaBuf, 0) == 0)
					{
						Found = 1;
						*FoundX = cntx;
						*FoundY = MaxPixels - 1 - CentreY;
					}
					else
						cntx--;

					ok = 1;
				}
			}

// **************************************************************************************
// Check left pixels
			if (!Found)
			{
				cnty = MaxPixels - 1 - CentreY;

				while ((cnty <= CentreY) && (!Found))
				{
					if (CheckAreaPosition(SizeX, SizeY, MaxPixels - 1 - CentreX, cnty, AreaBuf, 0) == 0)
					{
						Found = 1;
						*FoundX = MaxPixels - 1 - CentreX;
						*FoundY = cnty;
					}
					else
						cnty++;

					ok = 1;
				}
			}

// **************************************************************************************
// Check top pixels
			if (!Found)
			{
				cntx = MaxPixels - CentreX;

				while ((cntx < CentreX) && (!Found))
				{
					if (CheckAreaPosition(SizeX, SizeY, cntx, CentreY, AreaBuf, 0) == 0)
					{
						Found = 1;
						*FoundX = cntx;
						*FoundY = CentreY;
					}
					else
						cntx++;

					ok = 1;
				}
			}

			NrRings--;
// **************************************************************************************
		}
	}

	if (Found)
		return 1;

	return 0;
}

//#pragma optimize( "", off )

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 RegroupComponents(int32 mode)
{
	int32 cnt, cnt2, PinsMemPos, CompCount, ShapeNr, NewX, NewY, cnt3, ModeXor, CompNr1, CompNr2, NrShapeAreas,
	      ShapeCompNrs[512], PinNr, ConnectionChanged;
	uint8 ShapeAreaPresence[512], *AreaBuf;
	double ShapeArea[512], Area, MaxArea, UnitSize, PlaceX, PlaceY, ObjectX, ObjectY;
#ifdef _DEBUG
	int32 ok;
#endif

	CompPinRecord *CompPin;
	ObjectRecord *Object, *Object4;
	CompRecord *Comp, *NewComp, *NewComp2;
	CompPlaceRecord *CompPlace;
	CompPlaceRecordArray *CompPlaces;
	CompPlaceRecord *CompPlacePos[100];
	ConnectionsRecord *Connection, NewConnection;

	MaxArea = 0.0;
	AllocateSpecialMem(MEM_NET_SELECTED, 256 * 1024, (void **) &AreaBuf);
	memset(AreaBuf, 0, 256 * 1024);
	memset(&ShapeArea, 0, sizeof(ShapeArea));
	memset(&ShapeCompNrs, 0xff, sizeof(ShapeCompNrs));
	memset(&ShapeAreaPresence, 0, sizeof(ShapeAreaPresence));
	NrShapeAreas = 0;
	CompCount = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Comp->ShapeNr >= 0))
		{
			Area = Comp->PlacementWidth * Comp->PlacementHeight;
			MaxArea += Area;
			CompCount++;

			if (ShapeAreaPresence[Comp->ShapeNr] == 0)
			{
				cnt2 = 0;

				while ((cnt2 < NrShapeAreas) && (Area > ShapeArea[cnt2]))
					cnt2++;

#ifdef _DEBUG

				if (cnt2 == 12)
					ok = 1;

				if (Area < 1e6)
					ok = 1;

#endif

				if (cnt2 == NrShapeAreas)
				{
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					NrShapeAreas++;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
				}
				else
				{
					memmove(&ShapeArea[cnt2 + 1], &ShapeArea[cnt2], (NrShapeAreas - cnt2) * sizeof(double));
					memmove(&ShapeCompNrs[cnt2 + 1], &ShapeCompNrs[cnt2], (NrShapeAreas - cnt2) * sizeof(int32));
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
					NrShapeAreas++;
				}
			}
		}

		if (NrShapeAreas == 500)
		{
			ok = 1;
			break;
		}
	}

	MaxArea *= 4.0;

	Area = MaxArea / (MaxPixels * MaxPixels);
	UnitSize = max(50000.0, sqrt(Area));

	AllocateMemTemp(CompCount * sizeof(CompPlaceRecord));
	CompPlaces = (CompPlaceRecordArray *) TempMem;
	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->Info4 = 0;

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			CompPlace = &((*CompPlaces)[cnt2]);
			CompPlace->CompNr = cnt;
			CompPlace->DivX = (int32) (Comp->PlacementWidth / UnitSize) + 3;
			CompPlace->DivY = (int32) (Comp->PlacementHeight / UnitSize) + 3;
			CompPlace->XO = Comp->CompOriginX - Comp->PlacementOriginX;
			CompPlace->YO = Comp->CompOriginY - Comp->PlacementOriginY;
			CompPlace->ShapeNr = Comp->ShapeNr;
			CompPlace->Rotation = ((int32) Comp->CompMode) & 3;
			CompPlace->Width = Comp->PlacementWidth;
			CompPlace->Height = Comp->PlacementHeight;
			Comp->Info4 = 1;
			cnt2++;
		}
	}

	for (cnt = 0; cnt < 100; cnt++)
		CompPlacePos[cnt] = &((*CompPlaces)[cnt2]);

	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);

	for (cnt = NrShapeAreas - 1; cnt >= 0; cnt--)
	{
		ShapeNr = ShapeCompNrs[cnt];

		for (cnt2 = 0; cnt2 < CompCount; cnt2++)
		{
			CompPlace = &((*CompPlaces)[cnt2]);

			if (CompPlace->ShapeNr == ShapeNr)
			{
				if (FindAreaPosition(CompPlace->DivX, CompPlace->DivY, &NewX, &NewY, AreaBuf, 0) == 1)
				{
					FillAreaPosition(CompPlace->DivX, CompPlace->DivY, NewX, NewY, AreaBuf, 0);
					PlaceX = (NewX - MaxPixels / 2);
					PlaceX = PlaceX * UnitSize;
					CompPlace->NewX = PlaceX + CompPlace->XO;

					PlaceY = (NewY - MaxPixels / 2);
					PlaceY = PlaceY * UnitSize;
					CompPlace->NewY = PlaceY + CompPlace->YO;
					Comp = (CompRecord *) & (CompsMem[(*Comps)[CompPlace->CompNr]]);
					memcpy(NewComp, Comp, sizeof(CompRecord) + (int32) Comp->NrPins * sizeof(CompPinRecord));
					NewComp->CompOriginX = (float) AdjustToGrid(CompPlace->NewX, CompGridSize);
					NewComp->CompOriginY = (float) AdjustToGrid(CompPlace->NewY, CompGridSize);
					Comp->DummyX1 = Comp->CompOriginX - NewComp->CompOriginX;
					Comp->DummyY1 = Comp->CompOriginY - NewComp->CompOriginY;
#ifdef _DEBUG

					if (stricmp(Comp->Name, "R26") == 0)
						ok = 1;

#endif

					if (AddComp(NewComp))
					{
						Comp = (CompRecord *) & (CompsMem[(*Comps)[CompPlace->CompNr]]);
						Comp->Info |= OBJECT_NOT_VISIBLE;
						Comp->DeleteNr = (int16) LastActionNr;
						NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
						NewComp2->Info4 = 0;
					}
				}
			}
		}

	}

// **************************************************************************************
// **************************************************************************************

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			Connection->Info &= ~0x000c;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->Info4 == 1)
		{
			SearchMinX = Comp->BoardPosMinX;
			SearchMinY = Comp->BoardPosMinY;
			SearchMaxX = Comp->BoardPosMaxX;
			SearchMaxY = Comp->BoardPosMaxY;
			NrObjects4 = 0;
			CopyConnectionsFromRectWindowToObjects4(-1, 0);

			PinsMemPos = (uint8 *) Comp - &(CompsMem[0]) + sizeof(CompRecord);
			NrObjects = 0;
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
			Object = &((*Objects)[0]);
			PinNr = Object->PinNr;
			CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->PinNr != PinNr)
				{
					PinNr = Object->PinNr;
					PinsMemPos += sizeof(CompPinRecord);
					CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				}

				if (CompPin->NetNr != 32767)
				{
					ObjectX = Object->x1;
					ObjectY = Object->y1;

					for (cnt3 = 0; cnt3 < NrObjects4; cnt3++)
					{
						Object4 = &((*Objects4)[cnt3]);
						Connection = &((*Connections)[Object4->TraceNr]);
						ModeXor = 0;

						if ((InRange(Object4->x1, ObjectX)) && (InRange(Object4->y1, ObjectY)))
						{
							ModeXor |= 1;
							Connection->Comp1Nr = (int16) cnt;
						}

						if ((InRange(Object4->x2, ObjectX)) && (InRange(Object4->y2, ObjectY)))
						{
							ModeXor |= 2;
							Connection->Comp2Nr = (int16) cnt;
						}

						if (ModeXor > 0)
							Connection->Info |= ModeXor << 2;
					}
				}
			}
		}
	}

	if (MaxNrObjects4 > 4096)
		DeAllocateMemObjects4();

// **************************************************************************************

	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if (((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0) && ((Connection->Info & 0x000c) != 0))
		{
			CompNr1 = Connection->Comp1Nr;
			CompNr2 = Connection->Comp2Nr;
			ConnectionChanged = 0;
			memmove(&NewConnection, Connection, sizeof(ConnectionsRecord));

// **************************************************************************************
// Comp1Nr
			if (((Connection->Info & 0x0004) == 4) && (CompNr1 >= 0) && (CompNr1 < Design.NrComps))
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr1]]);
				ConnectionChanged = 1;
				NewConnection.x1 -= Comp->DummyX1;
				NewConnection.y1 -= Comp->DummyY1;
			}

// **************************************************************************************
// Comp2Nr
			if (((Connection->Info & 0x0008) == 8) && (CompNr2 >= 0) && (CompNr2 < Design.NrComps))
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr2]]);
				ConnectionChanged = 1;
				NewConnection.x2 -= Comp->DummyX1;
				NewConnection.y2 -= Comp->DummyY1;
			}

// **************************************************************************************
			if (ConnectionChanged)
			{
				AddConnection(&NewConnection);
				Connection = &((*Connections)[cnt]);
				Connection->Info |= OBJECT_NOT_VISIBLE;
				Connection->DeleteNr = (int16) LastActionNr;
			}
		}
	}

// **************************************************************************************
// **************************************************************************************

	DeAllocateMemTemp();
	DeallocateSpecialMem(MEM_NET_SELECTED);
	ViewFull();
	return 0;
}

//#pragma optimize( "", on )

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void SwitchLayerComp(int32 Layer)
{
	int32 cnt, cnt2, PinsMemPos, CompCount, cnt3, ModeXor, CompNr1, CompNr2, MemSize, PinNr, ConnectionChanged;
	double ObjectX, ObjectY;

	CompPinRecord *CompPin;
	ObjectRecord *Object, *Object4;
	CompRecord *Comp, *NewComp, *NewComp2;
	CompPlaceRecord *CompPlace;
	CompPlaceRecordArray *CompPlaces;
	ConnectionsRecord *Connection, NewConnection;

	CompCount = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			CompCount++;
	}

	AllocateMemTemp(CompCount * sizeof(CompPlaceRecord));
	CompPlaces = (CompPlaceRecordArray *) TempMem;
	CompCount = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->Info4 = 0;

		/*
		    ID_MOVE_COMP_TO_BOTTOM: Layer = 0
		    ID_MOVE_COMP_TO_TOP:    Layer = 1
		*/
		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (((Comp->Info & (OBJECT_SELECTED)) == OBJECT_SELECTED) && ((((Comp->CompMode & 8) >> 3) ^ Layer) == 0))
			{
				CompPlace = &((*CompPlaces)[CompCount]);
				CompPlace->CompNr = cnt;
				CompPlace->XO = Comp->CompOriginX;
				CompPlace->YO = Comp->CompOriginY;
				Comp->Info4 = 1;
				CompCount++;
			}
		}
	}

	if (CompCount == 0)
	{
		DeAllocateMemTemp();
		return;
	}

	for (cnt2 = 0; cnt2 < CompCount; cnt2++)
	{
		CompPlace = &((*CompPlaces)[cnt2]);
		Comp = (CompRecord *) & (CompsMem[(*Comps)[CompPlace->CompNr]]);

		MemSize = MemSizeComp(Comp);
		AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
		Comp->Info &= ~OBJECT_SELECTED;
		memmove(NewComp, Comp, MemSize);
		NewComp->CompMode ^= 8;
		NewComp->TextVisibility &= ~(0x88);
//    NewComp->TextVisibility&=~(1+16);
//    NewComp->Info&=~OBJECT_SELECTED;
		NewComp->TextVisibility |= (NewComp->CompMode & 8) * 0x11;

//    NewComp->Info&=~OBJECT_SELECTED;
		if (AddComp(NewComp))
		{
			NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
			Comp = (CompRecord *) & (CompsMem[(*Comps)[CompPlace->CompNr]]);
//        Comp->BoardPosDiagX1=Comp->CompOriginX;
//        Comp->BoardPosDiagY1=Comp->CompOriginY;
			NewComp2->Info4 = 0;
			Comp->Info |= OBJECT_NOT_VISIBLE;
			Comp->DeleteNr = (int16) LastActionNr;
		}
	}

// **************************************************************************************
// **************************************************************************************

	AllocateMemObjects4(2048);

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
			Connection->Info &= ~0x000c;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (Comp->Info4 == 1)
		{
			SearchMinX = Comp->BoardPosMinX;
			SearchMinY = Comp->BoardPosMinY;
			SearchMaxX = Comp->BoardPosMaxX;
			SearchMaxY = Comp->BoardPosMaxY;
			NrObjects4 = 0;
			NrObjects4 = CopyConnectionsFromRectWindowToObjects4(0, -1);

			PinsMemPos = (uint8 *) Comp - &(CompsMem[0]) + sizeof(CompRecord);
			NrObjects = 0;
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);
			Object = &((*Objects)[0]);
			PinNr = Object->PinNr;
			CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->PinNr != PinNr)
				{
					PinNr = Object->PinNr;
					PinsMemPos += sizeof(CompPinRecord);
					CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				}

				if (CompPin->NetNr != 32767)
				{
					ObjectX = Object->x1;
					ObjectY = Object->y1;

					for (cnt3 = 0; cnt3 < NrObjects4; cnt3++)
					{
						Object4 = &((*Objects4)[cnt3]);
						Connection = &((*Connections)[Object4->TraceNr]);
						ModeXor = 0;

						if ((InRange(Object4->x1, ObjectX)) && (InRange(Object4->y1, ObjectY)))
						{
							ModeXor |= 1;
							Connection->Comp1Nr = (int16) cnt;
						}

						if ((InRange(Object4->x2, ObjectX)) && (InRange(Object4->y2, ObjectY)))
						{
							ModeXor |= 2;
							Connection->Comp2Nr = (int16) cnt;
						}

						if (ModeXor > 0)
							Connection->Info |= ModeXor << 2;
					}
				}
			}
		}
	}

	DeAllocateMemObjects4();

// **************************************************************************************

	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);

		if (((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0) && ((Connection->Info & 0x000c) != 0))
		{
			CompNr1 = Connection->Comp1Nr;
			CompNr2 = Connection->Comp2Nr;
			ConnectionChanged = 0;
			memmove(&NewConnection, Connection, sizeof(ConnectionsRecord));

// **************************************************************************************
// Comp1Nr
			if (((Connection->Info & 0x0004) == 4) && (CompNr1 >= 0) && (CompNr1 < Design.NrComps))
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr1]]);
				ConnectionChanged = 1;
				NewConnection.x1 = Comp->CompOriginX * 2 - NewConnection.x1;
//        NewConnection.y1=Comp->CompOriginY*2-NewConnection.y1;
			}

// **************************************************************************************
// Comp2Nr
			if (((Connection->Info & 0x0008) == 8) && (CompNr2 >= 0) && (CompNr2 < Design.NrComps))
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr2]]);
				ConnectionChanged = 1;
				NewConnection.x2 = Comp->CompOriginX * 2 - NewConnection.x2;
//        NewConnection.y2=Comp->CompOriginY*2-NewConnection.y2;
			}

// **************************************************************************************
			if (ConnectionChanged)
			{
				AddConnection(&NewConnection);
				Connection = &((*Connections)[cnt]);
				Connection->Info |= OBJECT_NOT_VISIBLE;
				Connection->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	DeAllocateMemTemp();
	RePaint();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void UnselectComp(int32 mode)
{
	int32 cnt;
	CompRecord *Comp;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (((mode == 0) && (Comp->CompMode & 8)) || ((mode == 1) && ((Comp->CompMode & 8) == 0)))
				Comp->Info &= ~OBJECT_SELECTED;
		}
	}

	RePaint();
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 CheckFloatingConnections()
{
#ifdef _DEBUG

	int32 cnt, cnt2, cnt3, ok, ConnectionCount, CompInfo, ConnectionX1, ConnectionX2;
	double x1, x2, y1, y2, ObjectX, ObjectY, xx, yy, ObjectX2;
	CompRecord *Comp;
	ObjectRecord *Object;
	ConnectionsRecord *Connection;

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		if (cnt == Design.NrConnections - 2)
			ok = 1;

		Connection = &((*Connections)[cnt]);

		if ((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = Connection->x1;
			y1 = Connection->y1;
			x2 = Connection->x2;
			y2 = Connection->y2;
			xx = (143.5e5);
			yy = (115.9e5);

			if (((x1 > xx) && (x1 < xx + 10000)) && ((y1 > yy) && (y1 < yy + 10000)) || ((x2 > xx) && (x2 < xx + 10000))
			        && ((y2 > yy) && (y2 < yy + 10000)))
				ok = 1;

			ConnectionCount = 0;
			ConnectionX1 = 0;
			ConnectionX2 = 0;

			for (cnt2 = 0; cnt2 < Design.NrComps; cnt2++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt2]]);
				CompInfo = Comp->Info;
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "T113") == 0)
					ok = 1;

#endif

				if ((CompInfo & (OBJECT_NOT_VISIBLE)) == 0)
				{
					SearchMinX = Comp->BoardPosMinX;
					SearchMinY = Comp->BoardPosMinY;
					SearchMaxX = Comp->BoardPosMaxX;
					SearchMaxY = Comp->BoardPosMaxY;

					if (RectTestLine2(x1, y1, x2, y2) != 0)
					{
						NrObjects = 0;
						ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

						for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
						{
							Object = &((*Objects)[cnt3]);
							ObjectX = Object->x1;
							ObjectY = Object->y1;
							ObjectX2 = Object->x2;

							if ((InRange(x1, ObjectX)) && (InRange(y1, ObjectY)))
							{
								ConnectionX1 = 1;
								ConnectionCount++;
							}

							if ((InRange(x2, ObjectX)) && (InRange(y2, ObjectY)))
							{
								ConnectionX2 = 1;
								ConnectionCount++;
							}

							switch (Object->ObjectType)
							{
							case PIN_LINE_HOR:
								if ((InRange(x1, ObjectX + ObjectX2)) && (InRange(y1, ObjectY)))
								{
									ConnectionX1 = 1;
									ConnectionCount++;
								}

								if ((InRange(x2, ObjectX + ObjectX2)) && (InRange(y2, ObjectY)))
								{
									ConnectionCount++;
									ConnectionX2 = 1;
								}

								break;

							case PIN_LINE_VER:
								if ((InRange(x1, ObjectX)) && (InRange(y1, ObjectY + ObjectX2)))
								{
									ConnectionX1 = 1;
									ConnectionCount++;
								}

								if ((InRange(x2, ObjectX)) && (InRange(y2, ObjectY + ObjectX2)))
								{
									ConnectionCount++;
									ConnectionX2 = 1;
								}

								break;

							case PIN_LINE_DIAG1:
								if ((InRange(x1, ObjectX + ObjectX2)) && (InRange(y1, ObjectY - ObjectX2)))
								{
									ConnectionX1 = 1;
									ConnectionCount++;
								}

								if ((InRange(x2, ObjectX + ObjectX2)) && (InRange(y2, ObjectY - ObjectX2)))
								{
									ConnectionX2 = 1;
									ConnectionCount++;
								}

								break;

							case PIN_LINE_DIAG2:
								if ((InRange(x1, ObjectX + ObjectX2)) && (InRange(y1, ObjectY + ObjectX2)))
								{
									ConnectionX1 = 1;
									ConnectionCount++;
								}

								if ((InRange(x2, ObjectX + ObjectX2)) && (InRange(y2, ObjectY + ObjectX2)))
								{
									ConnectionCount++;
									ConnectionX2 = 1;
								}

								break;
							}
						}
					}
				}
			}

//      if (ConnectionCount<2) {
			if ((!ConnectionX1) || (!ConnectionX2))
				ok = 1;
		}
	}

#endif
	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 EditShapeComponent(int32 mode)
{
#define BufSize 4096

	int32 cnt, cnt2, count, ok, Pos, SizeFile, res, Found, Libfp, fp2, LoadResult, NrLibEntries, result,
	      NrGeometryLibFiles;
	CompRecord *Comp;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING * 5], GeometryLibNames[64][160],
	     ExeFile[MAX_LENGTH_STRING], ExeParams[MAX_LENGTH_STRING * 5], SearchFileName[MAX_LENGTH_STRING],
	     str3[MAX_LENGTH_STRING];
	LibRecord Lib;
	LibNameRecord LibName;
	uint8 ReadBuf[BufSize];
	PROCESS_INFORMATION ProcessInfo;
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;

	count = 0;
	Pos = 0;
	SizeFile = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			sprintf(str, "%s\\pcb\\shapes\\%s.shp", DesignPath, Comp->ShapeName);

			if (FileExistsUTF8(str) != 0)
			{
				sprintf(str, "%s\\shapes\\%s.shp", ProjectPath, Comp->ShapeName);

				if (FileExistsUTF8(str) != 0)
				{

					// Search in own libraries first
					NrGeometryLibFiles = 0;

					for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
					{
						if (NrGeometryLibFiles < 64)
							strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt]);
					}

					if (ProjectPath[0] != 0)
					{
						sprintf(SearchFileName, "%s\\shplib\\*.slb", ProjectPath);
						FileSearchHandle = FindFirstFileUTF8(SearchFileName, &FileInfo);
						res = 1;

						if (FileSearchHandle == INVALID_HANDLE_VALUE)
							res = 0;

						while ((res) && (NrGeometryLibFiles < 64))
						{
							UnicodeToUtf8(FileInfo.cFileName, str3, MAX_LENGTH_STRING - 100);
							sprintf(GeometryLibNames[NrGeometryLibFiles++], "%s\\shplib\\%s", ProjectPath, str3);
							res = FindNextFileW(FileSearchHandle, &FileInfo);
						}

						if (FileSearchHandle != INVALID_HANDLE_VALUE)
							FindClose(FileSearchHandle);
					}

					cnt2 = 0;
					Found = -1;

					while ((Found == -1) && (cnt2 < NrGeometryLibFiles))
					{
						if ((Libfp = FileOpenReadOnlyUTF8(GeometryLibNames[cnt2])) <= 0)
							return -1;

						if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
							return -1;

						if (strcmp(Lib.Identification, LibraryCode1) == 0)
						{
							NrLibEntries = Lib.NrLibEntries;
							cnt = 0;

							while ((Found == -1) && (cnt < NrLibEntries))
							{
								if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
									return -1;

								if (stricmpUTF8(LibName.Text, Comp->ShapeName) == 0)
								{
									Found = 1;
									Pos = LibName.Pos;
									SizeFile = LibName.Length;
									strcpy(FileName, GeometryLibNames[cnt2]);
									Found = cnt2;
								}

								cnt++;
							}
						}

						if (FileClose(Libfp) == -1)
							return -1;

						cnt2++;
					}

					if (Found == -1)
						return -1;	// open

					sprintf(str,
					        SC(787,
					           "Do you want to copy geometry %s from library %s to the local directory %s\\pcb\\shapes"),
					        Comp->ShapeName, FileName, DesignPath);

					if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNOCANCEL) == IDYES)
					{
						sprintf(str, "%s\\pcb\\shapes", DesignPath);

						if ((!SetCurrentDirectoryUTF8(str)) && (!CreateDirectoryUTF8(str)))
						{
							MessageBoxOwn(PCBWindow, str, SC(387, "Can not create directory"), MB_APPLMODAL | MB_OK);
							return -1;
						}

						sprintf(str, "%s\\pcb\\shapes\\%s.shp", DesignPath, Comp->ShapeName);

						if ((Libfp = FileOpenReadOnlyUTF8(FileName)) <= 0)
							return -1;

						if ((fp2 = CheckForWritingAndOpenUTF8(str, SizeFile, PCBWindow)) > 0)
						{
							FileSeek(Libfp, Pos);
							LoadResult = 1;
							cnt = SizeFile;

							while ((LoadResult == 1) && (cnt > 0))
							{
								if (FileRead(Libfp, &ReadBuf, min(BufSize, cnt), &result) != -1)
									FileWrite(fp2, &ReadBuf, result, &res);
								else
									LoadResult = 0;

								cnt -= result;
							}

							FileClose(fp2);
						}
						else
							return -1;

						FileClose(Libfp);
					}
					else
						return -1;
				}
			}

			ok = 1;
			sprintf(ExeFile, "%s\\geom.exe", ExePath);

			if (FileExistsUTF8(ExeFile) != 0)
				return 0;

			if (ProjectActive)
			{
				cnt = 0;

				while ((cnt < 32)
				        && ((ProjectInfo->FileTypes[cnt] != 3) || (stricmpUTF8(ProjectInfo->FileNames[cnt], str) != 0)))
					cnt++;

				if (cnt < 32)
				{
					if (ShowWindow(ProjectInfo->WindowHandles[cnt], SW_RESTORE))
						SetForegroundWindow(ProjectInfo->WindowHandles[cnt]);

					return 0;
				}
			}

			sprintf(ExeParams, "\"%s\\geom.exe\" \"%s\" /w %x /a /e \"%s\"", ExePath, str, (int32) PCBWindow, ExePath);

			if (ProjectActive)
				strcat(FileName, " /o");

			if (FoundDesignPath)
			{
				sprintf(str2, " /d \"%s\"", DesignPath);
				strcat(ExeParams, str2);
			}

			StartupInfo.cb = sizeof(StartupInfo);
			StartupInfo.wShowWindow = SW_SHOW;
			CreateProcess(ExeFile, ExeParams, NULL, NULL, 1, 0, NULL, NULL, &StartupInfo, &ProcessInfo);
//        MessageBoxOwn(PCBWindow,"",FileName,MB_APPLMODAL|MB_OK);
//        WinExec(FileName,SW_SHOW);
			return 0;
		}
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 InitComponentImportText(HWND Dialog)
{
	int32 cnt, cnt2;
	char LineBuf[MAX_LENGTH_STRING], str[11][MAX_LENGTH_STRING], NewStr[MAX_LENGTH_STRING];

	SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
	SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);

	for (cnt = 0; cnt < ImportComponentInfo.TotalLines; cnt++)
	{
		strcpy(LineBuf, (LPSTR) & (*ImportComponentText)[cnt]);

		if (cnt < ImportComponentInfo.SkipLines)
		{
			if (cnt > ImportComponentInfo.SkipLines - 4)
			{
				sprintf(NewStr, "%i\t%s", cnt + 1, LineBuf);
				SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) NewStr);
			}
		}
		else
		{
			memset(&str, 0, sizeof(str));

			for (cnt2 = 0; cnt2 < 11; cnt2++)
			{
				if (LineBuf[0] != 0)
				{
					GetString2a(LineBuf, str[cnt2]);
					str[cnt2][15] = 0;
				}
			}

			sprintf(NewStr, "%i", cnt + 1);

			for (cnt2 = 0; cnt2 < 11; cnt2++)
			{
				strcat(NewStr, "\t");
				strcat(NewStr, str[cnt2]);
			}

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) NewStr);
		}
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 SetComponentImportColumns(HWND Dialog)
{
	char str[MAX_LENGTH_STRING];

	sprintf(str, "%i", ImportComponentInfo.SkipLines);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	sprintf(str, "%i", ImportComponentInfo.CompRefColumn + 1);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	if (ImportComponentInfo.CompValueColumn >= 0)
		sprintf(str, "%i", ImportComponentInfo.CompValueColumn + 1);
	else
		sprintf(str, "-");

	SendDlgItemMessageOwn(Dialog, IDC_COMBO6, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	sprintf(str, "%i", ImportComponentInfo.PositionXColumn + 1);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO7, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);
	sprintf(str, "%i", ImportComponentInfo.PositionYColumn + 1);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO8, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);
	sprintf(str, "%i", ImportComponentInfo.RotationColumn + 1);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO9, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	if (ImportComponentInfo.LayerColumn >= 0)
		sprintf(str, "%i", ImportComponentInfo.LayerColumn + 1);
	else
		sprintf(str, "-");

	SendDlgItemMessageOwn(Dialog, IDC_COMBO10, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);
	return 0;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 InitComponentImportColumn(HWND Dialog)
{
	int32 cnt;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_RESETCONTENT, 0, 0);
	sprintf(str, SC(462, "Line"));

	for (cnt = 1; cnt < 12; cnt++)
	{
		if (ImportComponentInfo.CompRefColumn == cnt - 1)
			strcat(str, SC(839, "\tReference"));
		else
		{
			if (ImportComponentInfo.CompValueColumn == cnt - 1)
				strcat(str, SC(840, "\tValue"));
			else
			{
				if (ImportComponentInfo.PositionXColumn == cnt - 1)
					strcat(str, SC(841, "\tPosition X"));
				else
				{
					if (ImportComponentInfo.PositionYColumn == cnt - 1)
						strcat(str, SC(842, "\tPosition Y"));
					else
					{
						if (ImportComponentInfo.RotationColumn == cnt - 1)
							strcat(str, SC(843, "\tRotation"));
						else
						{
							if (ImportComponentInfo.LayerColumn == cnt - 1)
								strcat(str, SC(844, "\tLayer"));
							else
							{
								sprintf(str2, "\t%i", cnt);
								strcat(str, str2);
							}
						}
					}
				}
			}
		}
	}

	SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) str);
	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CALLBACK ComponentImportDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, ok, res, res2, hulp, TabStops[10];
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
// ************************************************************************************************
		SetWindowTextUTF8(Dialog, SC(461, "Edit component data"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(846, "Component file"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(469, "Info lines"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(470, "Columns"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(847, "Component"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(469, "Info lines"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(472, "Skip first"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(473, "lines"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(470, "Columns"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(848, "Reference"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(192, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(849, "Position X,Y"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC16, SC(183, "Rotation"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC17, SC(850, "Top/bottom"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(814, "Units"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC14, SC(814, "Units"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC15, SC(851, "Rotation mode"));

		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, SC(488, "thou/mm/inch/0.01mm"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(852, "Component: Reference/Value/X/Y/Rotation/Layer"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(853, "Component: Reference/X/Y/Rotation/Layer"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(1197, "Component: Reference/Layer/geometry/Value/X/Y/Rotation"));
// ************************************************************************************************

		ImportComponentInfo.CompRefColumn = 0;
		ImportComponentInfo.LayerColumn = 1;
		ImportComponentInfo.CompValueColumn = 2;
		ImportComponentInfo.PositionXColumn = 3;
		ImportComponentInfo.PositionYColumn = 4;
		ImportComponentInfo.RotationColumn = 5;
		TabStops[0] = 30;
		TabStops[1] = 70;
		TabStops[2] = 150;
		TabStops[3] = 230;
		TabStops[4] = 270;
		TabStops[5] = 310;
		TabStops[6] = 350;
		TabStops[7] = 390;
		TabStops[8] = 430;
		TabStops[9] = 470;
		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETTABSTOPS, 10, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_SETTABSTOPS, 10, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);
		InitComponentImportText(Dialog);

		for (cnt = 0; cnt < ImportComponentInfo.TotalLines; cnt++)
		{
			sprintf(str, "%i", cnt);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM) str);
		}

		for (cnt = 0; cnt < 10; cnt++)
		{
			sprintf(str, "%i", cnt);
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) str);
			SendDlgItemMessage(Dialog, IDC_COMBO7, CB_ADDSTRING, 0, (LPARAM) str);
			SendDlgItemMessage(Dialog, IDC_COMBO8, CB_ADDSTRING, 0, (LPARAM) str);
			SendDlgItemMessage(Dialog, IDC_COMBO9, CB_ADDSTRING, 0, (LPARAM) str);
		}

		for (cnt = 0; cnt < 11; cnt++)
		{
			if (cnt == 0)
				sprintf(str, "-");
			else
				sprintf(str, "%i", cnt);

			SendDlgItemMessage(Dialog, IDC_COMBO6, CB_ADDSTRING, 0, (LPARAM) str);
			SendDlgItemMessage(Dialog, IDC_COMBO10, CB_ADDSTRING, 0, (LPARAM) str);
		}

		SetComponentImportColumns(Dialog);

		SendDlgItemMessageOwn(Dialog, IDC_COMBO11, CB_ADDSTRING, 0, (LPARAM) SC(854, "Rotation degrees  CCW"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO11, CB_ADDSTRING, 0, (LPARAM) SC(855, "Rotation degrees  CW"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO11, CB_ADDSTRING, 0, (LPARAM) SC(856, "Rotation quadrant CCW"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO11, CB_ADDSTRING, 0, (LPARAM) SC(857, "Rotation quadrant CW"));
		SendDlgItemMessageOwn(Dialog, IDC_COMBO11, CB_SELECTSTRING, (WPARAM) - 1,
		                      (LPARAM) SC(854, "Rotation degrees  CCW"));

		InitComponentImportColumn(Dialog);
		ImportComponentInfo.Units = Units;
		SetUnitText(Dialog, IDC_EDIT2, ImportComponentInfo.Units);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0,
		                      (LPARAM) (LPSTR) ImportComponentInfo.ComponentImportFileName);
		return about;

	case WM_MOVE:
		break;

	case WM_SIZE:
		res = 1;
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			ImportComponentInfo.Units = SetNextUnits(ImportComponentInfo.Units, 4);	// thou/mm/inch/0.01mm
			SetUnitText(Dialog, IDC_EDIT2, ImportComponentInfo.Units);
			break;

		case IDC_BUTTON1:
			ImportComponentInfo.CompRefColumn = 0;
			ImportComponentInfo.CompValueColumn = 1;
			ImportComponentInfo.PositionXColumn = 2;
			ImportComponentInfo.PositionYColumn = 3;
			ImportComponentInfo.RotationColumn = 4;
			ImportComponentInfo.LayerColumn = 5;
			SetComponentImportColumns(Dialog);
			InitComponentImportColumn(Dialog);
			break;

		case IDC_BUTTON2:
			ImportComponentInfo.CompRefColumn = 0;
			ImportComponentInfo.CompValueColumn = -1;
			ImportComponentInfo.PositionXColumn = 1;
			ImportComponentInfo.PositionYColumn = 2;
			ImportComponentInfo.RotationColumn = 3;
			ImportComponentInfo.LayerColumn = 4;
			SetComponentImportColumns(Dialog);
			InitComponentImportColumn(Dialog);
			break;

		case IDC_BUTTON3:
			ImportComponentInfo.CompRefColumn = 0;
			ImportComponentInfo.LayerColumn = 1;
			ImportComponentInfo.CompValueColumn = 3;
			ImportComponentInfo.PositionXColumn = 4;
			ImportComponentInfo.PositionYColumn = 5;
			ImportComponentInfo.RotationColumn = 7;
			SetComponentImportColumns(Dialog);
			InitComponentImportColumn(Dialog);
			break;

		case IDOK:
			memset(&str, 0, sizeof(str));

			if (SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) str) > 0)
			{
				if (!isdigit(str[0]))
					sprintf(str2, "1 %s", str);
				else
					strcpy(str2, str);

				if (ScanParameters(1, str2, 0) == 1)
					ImportComponentInfo.UnitsValue = ParamsFloat[0];
				else
					ImportComponentInfo.UnitsValue = UnitsConvert(ImportComponentInfo.Units, 1.0);
			}
			else
				ImportComponentInfo.UnitsValue = UnitsConvert(ImportComponentInfo.Units, 1.0);

			ImportComponentInfo.RotationMode = SendDlgItemMessageOwn(Dialog, IDC_COMBO11, CB_GETCURSEL, 0, 0);
			EndDialog(Dialog, 1);
			break;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("import_component_positions.htm", 0);
			break;

		case IDC_COMBO2:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					ImportComponentInfo.SkipLines = res;
					SetComponentImportColumns(Dialog);
					InitComponentImportText(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO1:		// Reference
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

				if (res > 0)
				{
					ImportComponentInfo.CompRefColumn = res - 1;
					SetComponentImportColumns(Dialog);
					InitComponentImportColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO6:		// Value
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO6, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					if (res == 0)
						ImportComponentInfo.CompValueColumn = -1;
					else
						ImportComponentInfo.CompValueColumn = res - 1;

					SetComponentImportColumns(Dialog);
					InitComponentImportColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO7:		// X
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO7, CB_GETCURSEL, 0, 0);

				if (res > 0)
				{
					ImportComponentInfo.PositionXColumn = res - 1;
					SetComponentImportColumns(Dialog);
					InitComponentImportColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO8:		// Y
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO8, CB_GETCURSEL, 0, 0);

				if (res > 0)
				{
					ImportComponentInfo.PositionYColumn = res - 1;
					SetComponentImportColumns(Dialog);
					InitComponentImportColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO9:		// Rotation
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO9, CB_GETCURSEL, 0, 0);

				if (res > 0)
				{
					ImportComponentInfo.RotationColumn = res - 1;
					SetComponentImportColumns(Dialog);
					InitComponentImportColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO10:		// Top/Bottom
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO10, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					if (res == 0)
						ImportComponentInfo.LayerColumn = -1;
					else
						ImportComponentInfo.LayerColumn = res - 1;

					SetComponentImportColumns(Dialog);
					InitComponentImportColumn(Dialog);
				}

				ok = 1;
			}

			break;
		}

		break;

	case WM_PARENTNOTIFY:
		hulp = 1;
		break;
	}

	about = 0;
	return about;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 ImportComponentPositions(int32 mode)
{
	int32 cnt, cnt2, Length, res, TempLastActionNr, MemSize, fp = 0, Mirror, LineNr, ClipboardSize =
	            0, CommaCount, TabCount, count, ShapeNr, MemPos;
	char Delimiters[10], LineBuf[512], LineBufCopy[512], XName[MAX_LENGTH_STRING], YName[MAX_LENGTH_STRING];
	float CompX, CompY, Rotation, ix, iy;
	uint8 *SelectedNets;
	CompRecord *Comp, *NewComp;
	ObjectRecord *Object;
	ShapeRecord *Shape;
	HGLOBAL GlobalClipBoardMem = NULL;
	char *ClipBoardMem = NULL, *strp = NULL, *newstrp;

	char str[512], RefName[MAX_LENGTH_STRING], LayerName[MAX_LENGTH_STRING], ValueName[MAX_LENGTH_STRING],
	     ImportStrings[16][MAX_LENGTH_STRING], RotationName[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 ok;
#endif

	memset(&LineBufCopy, 0, sizeof(LineBufCopy));

	if (mode == 0)
	{
		if (ImportComponentFileName[0] == 0)
			strcpy(ImportComponentFileName, DesignPath);

		if (GetNewFileUTF8
			//********************************************************************************************************
			//*********************************************** Importovat pozice komponent ****************************
			//********************************************************************************************************
		        
			(PCBWindow, NULL, ImportComponentFileName, ExportDir, SC(390, "All files"), NULL,
		         SC(275, "Import component positions from file"), "*", 0))
			return -1;


		//  if (LoadNewFile3(ImportComponentFileName,SC(275,"Import component positions from file"),SC(390,"All files"),0,0)!=0)
		if (FileExistsUTF8(ImportComponentFileName) != 0)
			return -1;

		AllocateMemTemp(max(256 * 1024, FileSize(ImportComponentFileName) * 5));	// MaxTempMemory

		if ((fp = TextFileOpenUTF8(ImportComponentFileName)) < 0)
		{
			DeAllocateMemTemp();
			return -1;
		}
	}
	else
	{
		if (!OpenClipboard(PCBWindow))
			return -2;

		if (!IsClipboardFormatAvailable(CF_TEXT))
			return -2;

		if ((GlobalClipBoardMem = GetClipboardData(CF_TEXT)) == NULL)
			return -1;

		ClipboardSize = GlobalSize(GlobalClipBoardMem);

		if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
			return -2;

		AllocateMemTemp(max(256 * 1024, ClipboardSize * 5));	// MaxTempMemory
		strp = ClipBoardMem;
	}

	ImportComponentText = (ImportComponentTextRecord *) TempMem;
	ImportComponentInfo.TotalLines = 0;
	ImportComponentInfo.SkipLines = 0;

//  strcpy(ImportComponentFileName,"c:\\pcb_elegance\\cdio\\pcb\\pos_mils.txt");

	CommaCount = 0;
	TabCount = 0;
	Delimiters[0] = 0;
	strcpy(ImportComponentInfo.ComponentImportFileName, ImportComponentFileName);

	while (1)
	{
		if (mode == 0)
		{
			Length = ReadLn(fp, LineBuf);

			if (Length < 0)
			{
				TextFileClose(fp);
				break;
			}
		}
		else
		{
			if (strp < ClipBoardMem + ClipboardSize)
			{
				newstrp = strchr(strp, '\n');

				if (!newstrp)
					newstrp = ClipBoardMem + ClipboardSize;

				Length = newstrp - strp;

				if (Length > 0)
					Length--;

				if (Length > 159)
					Length = 159;

				if (Length > 0)
					strncpy(LineBuf, strp, 159);

				LineBuf[Length] = 0;

				if ((Length > 0) && (LineBuf[Length - 1] == '\r'))
					LineBuf[--Length] = 0;

				strp = newstrp + 1;
			}
			else
				break;
		}

		if (Length > 0)
		{
			LineBuf[159] = 0;
			strcpy((LPSTR) & ((*ImportComponentText)[ImportComponentInfo.TotalLines]), LineBuf);
			ImportComponentInfo.TotalLines++;

			if ((LineBuf[0] == '#') || (LineBuf[0] == ';'))
				ImportComponentInfo.SkipLines++;
			else
			{
				for (cnt2 = 0; cnt2 < (int32) strlen(LineBuf); cnt2++)
				{
					if (LineBuf[cnt2] == ',')
						CommaCount++;

					if (LineBuf[cnt2] == '\t')
						TabCount++;
				}
			}
		}
	}

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COMPONENT_IMPORT), PCBWindow,
	                 (DLGPROC) ComponentImportDialog2);

	if (res == 2)
	{
		DeAllocateMemTemp();

		if (mode == 1)
		{
			GlobalUnlock(GlobalClipBoardMem);
			CloseClipboard();
		}

		return -1;
	}

//  return -2;

	strcpy(Delimiters, " ");

	if (CommaCount > ImportComponentInfo.TotalLines)
		strcat(Delimiters, ",");
	else
	{
		if (TabCount > ImportComponentInfo.TotalLines)
			strcat(Delimiters, "\t");
	}

	TempLastActionNr = (int16) LastActionNr - 1;
	count = 0;
	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &SelectedNets);
	memset(SelectedNets, 0, Design.NrNets);
	LineNr = 1;

	if (mode == 0)
	{
		if ((fp = TextFileOpenUTF8(ImportComponentFileName)) < 0)
		{
			DeAllocateMemTemp();
			return -1;
		}
	}
	else
		strp = ClipBoardMem;

	while (1)
	{
		if (mode == 0)
		{
			Length = ReadLn(fp, LineBuf);

			if (Length < 0)
				break;
		}
		else
		{
			if (strp < ClipBoardMem + ClipboardSize)
			{
				newstrp = strchr(strp, '\n');

				if (!newstrp)
					newstrp = ClipBoardMem + ClipboardSize;

				Length = newstrp - strp;

				if (Length > 0)
					Length--;

				if (Length > 159)
					Length = 159;

				if (Length > 0)
					strncpy(LineBuf, strp, 159);

				LineBuf[Length] = 0;

				if ((Length > 0) && (LineBuf[Length - 1] == '\r'))
					LineBuf[--Length] = 0;

				strp = newstrp + 1;
			}
			else
				break;
		}

		if ((Length > 0) && (LineNr > ImportComponentInfo.SkipLines))
		{
			memset(&ImportStrings, 0, sizeof(ImportStrings));
			strncpy(LineBufCopy, LineBuf, 200);

			for (cnt2 = 0; cnt2 < 11; cnt2++)
			{
				if (LineBuf[0] != 0)
					GetString2b(LineBuf, Delimiters, ImportStrings[cnt2]);
			}

			RefName[0] = 0;
			strcpy(RefName, ImportStrings[ImportComponentInfo.CompRefColumn]);
#ifdef _DEBUG

			if (stricmpOwn(RefName, "U200") == 0)
				ok = 1;

#endif
			ValueName[0] = 0;

			if (ImportComponentInfo.CompValueColumn != -1)
			{
				strcpy(ValueName, ImportStrings[ImportComponentInfo.CompValueColumn]);
//        cnt2=ImportComponentInfo.CompValueColumn;
			}

			XName[0] = 0;
			strcpy(XName, ImportStrings[ImportComponentInfo.PositionXColumn]);
			YName[0] = 0;
			strcpy(YName, ImportStrings[ImportComponentInfo.PositionYColumn]);
			strcpy(RotationName, ImportStrings[ImportComponentInfo.RotationColumn]);
			LayerName[0] = 0;

			if (ImportComponentInfo.LayerColumn != -1)
				strcpy(LayerName, ImportStrings[ImportComponentInfo.LayerColumn]);

			Rotation = 0.0;
			Mirror = 0;
			CompX = 1000000000.0;
			CompY = 1000000000.0;
			sscanf(XName, "%f", &CompX);
			sscanf(YName, "%f", &CompY);
			sscanf(RotationName, "%f", &Rotation);

			switch (ImportComponentInfo.RotationMode)
			{
			case 0:
				break;

			case 1:
				Rotation = 360 - Rotation;
				break;

			case 2:
				Rotation *= 90.0;
				break;

			case 3:
				Rotation *= 90.0;
				Rotation = 360 - Rotation;
				break;
			}

			if (Rotation >= 360.0)
				Rotation -= 360.0;

			if (Rotation < 0.0)
				Rotation += 360.0;

			if ((Rotation >= 360.0) || (Rotation < 0.0))
				Rotation = 0.0;

			CompX *= (float) ImportComponentInfo.UnitsValue;
			CompY *= (float) ImportComponentInfo.UnitsValue;

			if (LayerName[0] != 0)
			{
				if (stricmpOwn(LayerName, "bottom") == 0)
					Mirror = 1;

				if (stricmpOwn(LayerName, "unten") == 0)
					Mirror = 1;

				if (stricmpOwn(LayerName, "bot") == 0)
					Mirror = 1;

				if (stricmpOwn(LayerName, "0") == 0)
					Mirror = 1;
			}

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->AddNr <= TempLastActionNr)
				        && (stricmpOwn(Comp->Name, RefName) == 0))
				{

					ShapeNr = (int32) Comp->ShapeNr;
					MemPos = (*Shapes)[ShapeNr].ShapePos;
					Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "U220") == 0)
					{
//            Rotation=115.67;
						ok = 1;
					}

#endif


					ix = Shape->InsertionX;
					iy = Shape->InsertionY;
					RotatePoint(&ix, &iy, Rotation);

					if (Mirror == 1)
						ix = -ix;

					CompX -= ix;
					CompY -= iy;
//          ix+=Comp->CompOriginX;
//          iy+=Comp->CompOriginY;



					count++;
					MemSize = MemSizeComp(Comp);
					AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
					memmove(NewComp, Comp, MemSize);
#ifdef _DEBUG

					if (stricmpOwn(RefName, "U200") == 0)
						ok = 1;

#endif
					NewComp->CompOriginX = (float) CompX;
					NewComp->CompOriginY = (float) CompY;
					NewComp->CompMode &= ~0x3;
					NewComp->CompMode &= ~8;

					if (Mirror != 0)
						NewComp->CompMode |= 8;

					NewComp->Rotation = (float) Rotation;

					if (AddComp(NewComp))
					{
						NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
						Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
						SetBackGroundActive(0);
						Comp->Info &= ~OBJECT_SELECTED;
						NewComp->Info |= OBJECT_SELECTED;
						NewComp->Info4 = 0;
						Comp->Info |= OBJECT_NOT_VISIBLE;
						Comp->DeleteNr = (int16) LastActionNr;
						NrObjects = 0;
						ShapePinsToObject(NewComp, 0.0, 0.0, 0, 0, 0, 0);

						for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
						{
							Object = &((*Objects)[cnt2]);

							if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
								SelectedNets[Object->NetNr] = 1;
						}
					}
				}
			}
		}

		LineNr++;
	}

	if (mode == 0)
		TextFileClose(fp);
	else
	{
		if (GlobalClipBoardMem)
			GlobalUnlock(GlobalClipBoardMem);

		CloseClipboard();
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (SelectedNets[cnt] == 1)
		{
			ReCalcConnectionsNet((int32) cnt, 0, 1);
			CheckForEscape();
		}
	}

	ViewFull();
	DeAllocateMemTemp();

	sprintf(str, SC(1108, "%d components have been replaced"), count);
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK AlignCompDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, res, mode;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(860, "Align left"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(861, "Align right"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(862, "Align top"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO4, SC(863, "Align bottom"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO5, SC(864, "Evenly spaced vertical"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO6, SC(865, "Evenly spaced horizontal"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO7, SC(866, "Align vertical"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO8, SC(867, "Align horizontal"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(868, "Alignment on component pins"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(869, "Alignment on component center"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(870, "Component evenly placed (Component center)"));

		switch (AlignCompMode)
		{
		case 0:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
			break;

		case 1:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
			break;

		case 2:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
			break;

		case 3:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO4, BM_SETCHECK, 1, 0);
			break;

		case 4:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO5, BM_SETCHECK, 1, 0);
			break;

		case 5:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO6, BM_SETCHECK, 1, 0);
			break;

		case 6:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO7, BM_SETCHECK, 1, 0);
			break;

		case 7:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO8, BM_SETCHECK, 1, 0);
			break;
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			mode = -1;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0)) == 1)
				mode = 0;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0)) == 1)
				mode = 1;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0)) == 1)
				mode = 2;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO4, BM_GETCHECK, 0, 0)) == 1)
				mode = 3;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO5, BM_GETCHECK, 0, 0)) == 1)
				mode = 4;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO6, BM_GETCHECK, 0, 0)) == 1)
				mode = 5;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO7, BM_GETCHECK, 0, 0)) == 1)
				mode = 6;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO8, BM_GETCHECK, 0, 0)) == 1)
				mode = 7;

			EndDialog(Dialog, mode);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 AlignComponents(int32 mode)
{
	/*
	  mode:

	    0: Align left
	    1: Align right
	    2: Align top
	    3: Align bottom

	    4: Evenly space vertical
	    5: Evenly space horizontal

	    6: Align vertical comp center
	    7: Align horizontal comp center
	*/

	int32 cnt, cnt2, cnt3, ok, MemSize, TempLastActionNr, count, Found, CompIndexSmallest, CompIndexBiggest,
	      CompIndexMinX, CompIndexMaxX, CompIndexMinY, CompIndexMaxY;
	CompRecord *Comp, *NewComp, *NewComp2;
	ObjectRecord *Object;
	uint8 *SelectedNets;
	double CompMinCenterX, CompMaxCenterX, CompMinCenterY, CompMaxCenterY, CompMinX, CompMaxX, CompMinY, CompMaxY,
	       Spacing, StartX, StartY, CompWidth, CompHeight, CompDivX, CompDivY, MinDistance, CompMaxWidth, CompMaxHeight,
	       CompMinWidth, CompMinHeight, CenterX, CenterY, x1, y1;

	mode =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COMP_ALIGN), PCBWindow, (DLGPROC) AlignCompDialog2);

	if (mode < 0)
		return -1;

	AlignCompMode = mode;

	AllocateSpecialMem(MEM_NET_SELECTED, 128 * 1024, (void **) &SelectedNets);
	memset(SelectedNets, 0, Design.NrNets);

	CompMaxWidth = 0.0;
	CompMaxHeight = 0.0;
	CompMinWidth = 1e9;
	CompMinHeight = 1e9;
	CompMinX = 1e9;
	CompMaxX = -1e9;
	CompMinY = 1e9;
	CompMaxY = -1e9;
	CompMinCenterX = 1e9;
	CompMaxCenterX = -1e9;
	CompMinCenterY = 1e9;
	CompMaxCenterY = -1e9;
	count = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SetBoardPosComp(Comp, 7);	// Get min,max pins comp -> Search min,max
			CompWidth = SearchMaxX - SearchMinX;
			CompHeight = SearchMaxY - SearchMinY;
			CenterX = (SearchMaxX + SearchMinX) * 0.5;
			CenterY = (SearchMaxY + SearchMinY) * 0.5;
			Comp->DummyX1 = (float) CenterX;
			Comp->DummyY1 = (float) CenterY;
			Comp->DummyX2 = (float) CompWidth;
			Comp->DummyY2 = (float) CompHeight;

			if (CenterX < CompMinCenterX)
			{
				CompMinCenterX = CenterX;
				CompIndexMinX = cnt;
			}

			if (CenterX > CompMaxCenterX)
			{
				CompMaxCenterX = CenterX;
				CompIndexMaxX = cnt;
			}

			if (CenterY < CompMinCenterY)
			{
				CompMinCenterY = CenterY;
				CompIndexMinY = cnt;
			}

			if (CenterY > CompMaxCenterY)
			{
				CompMaxCenterY = CenterY;
				CompIndexMaxY = cnt;
			}

			switch (AlignCompMode)
			{
			case 0:			// Align left
				if (SearchMinX < CompMinX)
				{
					CompMinX = SearchMinX;
					CompIndexMinX = cnt;
				}

				if (SearchMinX > CompMaxX)
				{
					CompMaxX = SearchMinX;
					CompIndexMaxX = cnt;
				}

				break;

			case 1:			// Align right
				if (SearchMaxX > CompMaxX)
				{
					CompMaxX = SearchMaxX;
					CompIndexMaxX = cnt;
				}

				if (SearchMaxX < CompMinX)
				{
					CompMinX = SearchMaxX;
					CompIndexMinX = cnt;
				}

				break;

			case 2:			// Align top
				if (SearchMaxY > CompMaxY)
				{
					CompMaxY = SearchMaxY;
					CompIndexMaxY = cnt;
				}

				if (SearchMaxY < CompMinY)
				{
					CompMinY = SearchMaxY;
					CompIndexMinY = cnt;
				}

				break;

			case 3:			// Align bottom
				if (SearchMinY < CompMinY)
				{
					CompMinY = SearchMinY;
					CompIndexMinY = cnt;
				}

				if (SearchMinY > CompMaxY)
				{
					CompMaxY = SearchMinY;
					CompIndexMaxY = cnt;
				}

				break;

			case 4:			// Evenly spaced vertical
				if (CompHeight < CompMinHeight)
				{
					CompMinHeight = CompHeight;
					CompIndexSmallest = cnt;
				}

				if (CompHeight > CompMaxHeight)
				{
					CompMaxHeight = CompHeight;
					CompIndexBiggest = cnt;
				}

				break;

			case 5:			// Evenly spaced horizontal
				if (CompWidth < CompMinWidth)
				{
					CompMinWidth = CompWidth;
					CompIndexSmallest = cnt;
				}

				if (CompWidth > CompMaxWidth)
				{
					CompMaxWidth = CompWidth;
					CompIndexBiggest = cnt;
				}

				break;

			case 6:			// Align vertical comp center
			case 7:			// Align horizontal comp center
				break;
			}

			count++;
		}
	}

	if (count == 0)
		return -1;

// ****************************************************************************************************
// ****************************************************************************************************

	x1 = 0.0;
	y1 = 0.0;
	CompDivX = 0.0;
	CompDivY = 0.0;
	Spacing = 0.0;
	StartX = 0.0;
	StartY = 0.0;

	switch (AlignCompMode)
	{
	case 0:					// Align left
	case 1:					// Align right
		x1 = (CompMaxX + CompMinX) * 0.5;
		break;

	case 2:					// Align top
	case 3:					// Align bottom
		y1 = (CompMaxY + CompMinY) * 0.5;
		break;

	case 4:					// Evenly spaced vertical
		CompDivY = CompMaxCenterY - CompMinCenterY;
		Spacing = CompDivY / (count - 1);
		StartY = CompMinCenterY;
		break;

	case 5:					// Evenly spaced horizontal
		CompDivX = CompMaxCenterX - CompMinCenterX;
		Spacing = CompDivX / (count - 1);
		StartX = CompMinCenterX;
		break;

	case 6:					// Align vertical comp center
		x1 = (CompMaxCenterX + CompMinCenterX) * 0.5;
		break;

	case 7:					// Align horizontal comp center
		y1 = (CompMaxCenterY + CompMinCenterY) * 0.5;
		break;
	}

	TempLastActionNr = (int16) LastActionNr - 1;
	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);

	switch (AlignCompMode)
	{
	case 0:					// Align left
	case 1:					// Align right
	case 2:					// Align top
	case 3:					// Align bottom
	case 6:					// Align vertical comp center
	case 7:					// Align horizontal comp center
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Comp->AddNr <= TempLastActionNr))
			{
				MemSize = MemSizeComp(Comp);
				memmove(NewComp, Comp, MemSize);
				SetBoardPosComp(Comp, 7);	// Get min,max pins comp -> Search min,max
				CompWidth = SearchMaxX - SearchMinX;
				CompHeight = SearchMaxY - SearchMinY;
				CenterX = (SearchMaxX + SearchMinX) * 0.5;
				CenterY = (SearchMaxY + SearchMinY) * 0.5;

				switch (AlignCompMode)
				{
				case 0:		// Align left
					NewComp->CompOriginX -= (float) (SearchMinX - x1);
					break;

				case 1:		// Align right
					NewComp->CompOriginX -= (float) (SearchMaxX - x1);
					break;

				case 2:		// Align top
					NewComp->CompOriginY -= (float) (SearchMaxY - y1);
					break;

				case 3:		// Align bottom
					NewComp->CompOriginY -= (float) (SearchMinY - y1);
					break;

				case 6:		// Align vertical comp center
					NewComp->CompOriginX -= (float) (CenterX - x1);
					break;

				case 7:		// Align horizontal comp center
					NewComp->CompOriginY -= (float) (CenterY - y1);
					break;
				}

				if (AddComp(NewComp))
				{
					NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
					Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
					Comp->Info &= ~OBJECT_SELECTED;
					NewComp2->Info4 = 0;
					NewComp2->Info &= ~OBJECT_SELECTED;
					NewComp2->TextVisibility &= ~(1 + 16);
					Comp->TextVisibility &= ~(1 + 16);
					Comp->Info |= OBJECT_NOT_VISIBLE;
					Comp->DeleteNr = (int16) LastActionNr;
					NrObjects = 0;
					ShapePinsToObject(NewComp2, 0.0, 0.0, 0, 0, 0, 0);

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
							SelectedNets[Object->NetNr] = 1;
					}
				}
			}
		}

		break;

	case 4:					// Evenly spaced vertical
	case 5:					// Evenly spaced horizontal
		if (count < 3)
		{
			MessageBoxOwn(PCBWindow, SC(871, "More than two components are needed"), SC(24, "Error"),
			              MB_APPLMODAL | MB_OK);
			return -1;
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Comp->AddNr <= TempLastActionNr))
				Comp->Info4 = 0;
		}

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			Found = -1;
			MinDistance = 1e9;

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED) && (Comp->Info4 == 0)
				        && (Comp->AddNr <= TempLastActionNr))
				{
					CenterX = Comp->DummyX1;
					CenterY = Comp->DummyY1;
					CompWidth = Comp->DummyX2;
					CompHeight = Comp->DummyY2;

					switch (AlignCompMode)
					{
					case 4:	// Evenly spaced vertical
						if (CenterY - StartY < MinDistance)
						{
							MinDistance = CenterY - StartY;
							Found = cnt;
						}

						break;

					case 5:	// Evenly spaced horizontal
						if (CenterX - StartX < MinDistance)
						{
							MinDistance = CenterX - StartX;
							Found = cnt;
						}

						break;
					}
				}
			}

			if (Found != -1)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[Found]]);
				Comp->Info4 = 1;
				MemSize = MemSizeComp(Comp);
				memmove(NewComp, Comp, MemSize);

				switch (AlignCompMode)
				{
				case 4:		// Evenly spaced vertical
					NewComp->CompOriginY = (float) StartY;
					break;

				case 5:		// Evenly spaced horizontal
					NewComp->CompOriginX = (float) StartX;
					break;
				}

				if (AddComp(NewComp))
				{
					NewComp2 = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
					Comp = (CompRecord *) & (CompsMem[(*Comps)[Found]]);
					Comp->Info &= ~OBJECT_SELECTED;
					Comp->TextVisibility &= ~(1 + 16);
					Comp->Info |= OBJECT_NOT_VISIBLE;
					Comp->DeleteNr = (int16) LastActionNr;
					NewComp2->Info4 = 0;
					NewComp2->Info &= ~OBJECT_SELECTED;
					NewComp2->TextVisibility &= ~(1 + 16);
					NewComp2->DummyX1 = 0.0;
					NewComp2->DummyY1 = 0.0;
					NewComp2->DummyX2 = 0.0;
					NewComp2->DummyY2 = 0.0;
					NrObjects = 0;
					ShapePinsToObject(NewComp2, 0.0, 0.0, 0, 0, 0, 0);

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
							SelectedNets[Object->NetNr] = 1;
					}
				}
			}

			StartX += Spacing;
			StartY += Spacing;
		}

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Comp->AddNr <= TempLastActionNr))
			{
				Comp->Info4 = 0;
				Comp->DummyX1 = 0.0;
				Comp->DummyY1 = 0.0;
				Comp->DummyX2 = 0.0;
				Comp->DummyY2 = 0.0;
			}
		}

		break;
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (SelectedNets[cnt] == 1)
		{
			ReCalcConnectionsNet((int32) cnt, 0, 1);
			CheckForEscape();
		}
	}

	DeallocateSpecialMem(MEM_NET_SELECTED);
	RePaint();
	ok = 1;

	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
