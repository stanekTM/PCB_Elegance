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
#include "calcrect.h"
#include "menus.h"
#include "geom.h"
#include "stdio.h"
#include "math.h"
#include "dialogs.h"
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
#include "resource.h"
#include "files.h"
#include "help.h"
#include "polygon.h"
#include "utf8.h"

typedef struct
{
	int32 NrLines, dummy;
	double x, y;
} LinePointRecord;

typedef LinePointRecord LinePointsArray[1000];

typedef int32 LinePointsNumbersArray[4096];

// double  CentreSelectedX;
double CentreSelectedX, CentreSelectedY, CurrentX2, CurrentY2, ShiftOffsetX, ShiftOffsetY, SelectedMinX, SelectedMinY,
       SelectedMaxX, SelectedMaxY;


int32 ConnectionsComponentsDrawing, RotateIndividual, CompRotation, NrLinePoints, ok;

LinePointsArray *LinePoints;
LinePointsNumbersArray *LinePointsNumbers;

// *******************************************************************************************************
// *******************************************************************************************************

extern HDC OutputDisplay;
extern int32 NrGraphicsObjects, ScaleOption;
extern int32 CheckFilenameForSaving;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceMovedObjects(double CurrentX, double CurrentY, int32 Mode);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveSelectedObjectsToZero(int32 Mode, int32 Count)
{
	double x1, y1;

	GetMinMaxSelectedObjects();
	x1 = (SelectedMinX + SelectedMaxX) / 2;
	y1 = (SelectedMinY + SelectedMaxY) / 2;
	CentreSelectedX = 0.0;
	CentreSelectedY = 0.0;
	CurrentX2 = 0.0;
	CurrentY2 = 0.0;
	PlaceMovedObjects(CenterMoveX - x1, CenterMoveY - y1, 0);

//  NewObject.x1+=CurrentX-CentreSelectedX;
//  NewObject.y1+=CurrentY-CentreSelectedY;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveSpecial()
{
	double CurrentX, CurrentY;
	int32 cnt, res, FirstSelected = 1, OkToCopy;
	ObjectRecord *Object;

	CurrentX = PixelToRealOffX(MousePosX);
	CurrentY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
	SearchMinX = CurrentX;
	SearchMinY = CurrentY;
	SearchMaxX = CurrentX;
	SearchMaxY = CurrentY;
	CentreSelectedX = 0.0;
	CentreSelectedY = 0.0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (((Object->ObjectType == OBJECT_RECT) || (Object->ObjectType == OBJECT_CIRCLE)))
			{
				if (FirstSelected)
				{
					CentreSelectedX = Object->x1;
					CentreSelectedY = Object->y1;
					FirstSelected = 0;
				}
				else
				{
					if ((NotInRange(Object->x1, CentreSelectedX)) || (NotInRange(Object->y1, CentreSelectedY)))
						return;
				}
			}
			else
			{
				if (FirstSelected)
				{
					CentreSelectedX = Object->x1;
					CentreSelectedY = Object->y1;
				}
			}
		}
	}

	res = GetNrSelectObjects();

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		OkToCopy = 0;

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == 0) && (ObjectLayerVisible(Object->Layer)))
		{
			switch (Object->ObjectType)
			{
			case OBJECT_CIRCLE:
				if (RectTestCircle(Object->x1, Object->y1, Object->x2, 255))
					OkToCopy = 1;

				break;

			case OBJECT_RECT:
				if (RectTestRect2(Object->x1, Object->y1, Object->x2, Object->y2))
					OkToCopy = 1;

				break;
			}

			if (OkToCopy)
			{
				CurrentX2 = 0.0;
				CurrentY2 = 0.0;
				PlaceMovedObjects(Object->x1 - CentreSelectedX, Object->y1 - CentreSelectedY, 0);
				return;
			}
		}
	}

}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 GetCoordinateFirstObject(double CurrentX, double CurrentY, double *x, double *y)
{
	int32 cnt, cnt2, count2;
	double x1, y1, x2, y2, MinLength, Length;
	ObjectPolygonRecord *ObjectPolygon;

	ObjectRecord *Object;

	MinLength = 10000.0e5;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
				Length = CalcLengthLine(CurrentX, CurrentY, Object->x1, Object->y1);

				if (Length < MinLength)
				{
					*x = Object->x1;
					*y = Object->y1;
					MinLength = Length;
				}

				Length = CalcLengthLine(CurrentX, CurrentY, Object->x2, Object->y2);

				if (Length < MinLength)
				{
					*x = Object->x2;
					*y = Object->y2;
					MinLength = Length;
				}

				break;

			case OBJECT_CIRCLE:
				if (((Object->Thickness == 0.0) && (Object->Info2 == 0)) || (Object->Info2 == 15))
				{
					Length = CalcLengthLine(CurrentX, CurrentY, Object->x1, Object->y1);

					if (Length < MinLength)
					{
						*x = Object->x1;
						*y = Object->y1;
						MinLength = Length;
					}
				}
				else
				{
					GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);
					Length = CalcLengthLine(CurrentX, CurrentY, x1, y1);

					if (Length < MinLength)
					{
						*x = x1;
						*y = y1;
						MinLength = Length;
					}

					Length = CalcLengthLine(CurrentX, CurrentY, x2, y2);

					if (Length < MinLength)
					{
						*x = x2;
						*y = y2;
						MinLength = Length;
					}

					x1 = Object->x1;
					y1 = Object->y1;
					Length = CalcLengthLine(CurrentX, CurrentY, x1, y1);

					if (Length < MinLength)
					{
						*x = x1;
						*y = y1;
						MinLength = Length;
					}
				}

				break;

			case OBJECT_RECT:
			case OBJECT_TEXT:
				Length = CalcLengthLine(CurrentX, CurrentY, Object->x1, Object->y1);

				if (Length < MinLength)
				{
					*x = Object->x1;
					*y = Object->y1;
					MinLength = Length;
				}

				break;

			case OBJECT_ARC:
				GetArcEndPoints(Object, &x1, &y1, &x2, &y2, 0);
				Length = CalcLengthLine(CurrentX, CurrentY, x1, y1);

				if (Length < MinLength)
				{
					*x = x1;
					*y = y1;
					MinLength = Length;
				}

				Length = CalcLengthLine(CurrentX, CurrentY, x2, y2);

				if (Length < MinLength)
				{
					*x = x2;
					*y = y2;
					MinLength = Length;
				}

				x1 = Object->x1;
				y1 = Object->y1;
				Length = CalcLengthLine(CurrentX, CurrentY, x1, y1);

				if (Length < MinLength)
				{
					*x = x1;
					*y = y1;
					MinLength = Length;
				}

				break;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			count2 = ObjectPolygon->NrVertices;

			if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
				count2 = ObjectPolygon->NrVerticesMainPolygon;

			for (cnt2 = 0; cnt2 < count2; cnt2++)
			{
				x1 = ObjectPolygon->Points[cnt2].x;
				y1 = ObjectPolygon->Points[cnt2].x;
				Length = CalcLengthLine(x1, y1, CurrentX, CurrentY);

				if (Length < 0.5e5)
				{
					*x = x1;
					*y = y1;
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


void MoveSelectedObjects(int32 Mode, int32 Count)
{
	int32 cnt, NrParams, res, val, Mode2, CompPlaced, FirstShift;
	double OldX, OldY, CurrentX, CurrentY, hx, hy, divx, divy, ShiftX, ShiftY, x1, y1, CursorX, CursorY, divx2, divy2;
	HMENU PopUpMenu;
	ObjectRecord2 TypeObject;
	ObjectRecord *Object;
	DrawXorFunctionRecord DrawXorFunction;

//  GetNrSelections();

//  PopUpMenu=CreatePopupMenu();
//  AppendMenu(PopUpMenu,MF_ENABLED|MF_STRING,ID_POPUP_ESCAPE,SC(154,"Escape"));

	ShiftX = 0.0;
	ShiftY = 0.0;

	if (Count == 0)
		Count = 1;

	ConnectionsComponentsDrawing = 1;
	SelectionActive = 1;
	SelectionEsc = 0;
	CompPlaced = 0;
	/*
	  Rect.left=(int16)DrawWindowMinX+ClientStartX;
	  Rect.top=(int16)DrawWindowMinY+ClientStartY;
	  Rect.right=(int16)DrawWindowMaxX+ClientStartX;
	  Rect.bottom=(int16)DrawWindowMaxY+ClientStartY;
	*/
	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	if (Mode == 1)
	{
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Object->ObjectType == OBJECT_LINE))
				Object->Info |= 3;
		}
	}

	/*
	  ShiftOffsetX=0.0;
	  ShiftOffsetY=0.0;
	  CentreSelectedX=0.0;
	  CentreSelectedY=0.0;
	  for (cnt=0;cnt<NrObjects;cnt++) {
	    Object=&((*Objects)[cnt]);
	    if ((Object->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      if (((Object->ObjectType==OBJECT_RECT)
	         ||
	         (Object->ObjectType==PIN_PUT_THROUGH_ROUND_POWER)
	         ||
	         (Object->ObjectType==PIN_PUT_THROUGH_ROUND_INNER_PAD)
	         ||
	         (Object->ObjectType==DRILL)
	         ||
	         (Object->ObjectType==DRILL_UNPLATED)
	         ||
	         (Object->ObjectType==OBJECT_CIRCLE))) {
	        if (FirstSelected) {
	          CentreSelectedX=Object->x1;
	          CentreSelectedY=Object->y1;
	          FirstSelected=0;
	        } else {
	          if (ObjectsInPoint) {
	            if ((NotInRange(Object->x1,CentreSelectedX))
	               ||
	               (NotInRange(Object->y1,CentreSelectedY))) {
	              ObjectsInPoint=0;
	            }
	          }
	        }
	      } else {
	        if (FirstSelected) {
	          CentreSelectedX=Object->x1;
	          CentreSelectedY=Object->y1;
	        }
	      }
	    }
	  }

	  if ((!FirstSelected)
	     &&
	     (ObjectsInPoint)) {
	    ShiftOffsetX=CentreSelectedX-CurrentX;
	    ShiftOffsetY=CentreSelectedY-CurrentY;
	  }
	*/

	CentreSelectedX = 0.0;
	CentreSelectedY = 0.0;
	RelX = CurrentX;
	RelY = CurrentY;
	hx = CurrentX;
	hy = CurrentY;

	if ((SnapMode & 1) == 1)
	{
		res = GetCoordinateFirstObject(CurrentX, CurrentY, &hx, &hy);	// Snap on a round/rect object selected
	}

	ShiftOffsetX = hx - CurrentX;
	ShiftOffsetY = hy - CurrentY;

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

//  AdjustOffsetForSnap(CurrentX,CurrentY,CurrentX2,CurrentY2,&ShiftOffsetX,&ShiftOffsetY,0);

	RelX = ShiftOffsetX + CurrentX;
	RelY = ShiftOffsetY + CurrentY;
	val = (int32) PixelToReal(10);
	DrawLineWhite(RelX, RelY + val, RelX, RelY - val, 0);
	DrawLineWhite(RelX + val, RelY, RelX - val, RelY, 0);

	OldX = CurrentX;
	OldY = CurrentY;

	SystemBusyMode = 10;
	Mode2 = 1;
	DrawXorFunction.Function1 = (FUNCP1) DrawSelectedObjects;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode2;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode2;
	ZoomInOutProcessed = 0;

	DrawSelectedObjects(CurrentX, CurrentY, 1);
	FirstShift = 1;
//  while (!LeftButtonPressed) CheckInputMessages(0) ;
	ClipMouseCursor();

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

				if (Units == 0)
				{
					x1 = (CurrentX - CurrentX2 - ShiftOffsetX) / 2540.0;
					y1 = (CurrentY - CurrentY2 - ShiftOffsetY) / 2540.0;
					sprintf(InfoStr, "%.2f,%.2f", x1, y1);
				}
				else
				{
					x1 = (CurrentX - CurrentX2 - ShiftOffsetX) / 100000.0;
					y1 = (CurrentY - CurrentY2 - ShiftOffsetY) / 100000.0;
					sprintf(InfoStr, "%.4f,%.4f", x1, y1);
				}

				RedrawInfoStr(1);

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
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollRight(ScrollSize);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollDown(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollLeft(ScrollSize);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawSelectedObjects(OldX, OldY, 1);
				ScrollUp(ScrollSize);
//        NrGraphicsObjects
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
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
				DrawSelectedObjects(ShiftX, ShiftY, 1);
//        DrawSelectedObjects(CurrentX,CurrentY,1);
				ShiftOffsetX -= ShiftX - CurrentX;
				ShiftOffsetY -= ShiftY - CurrentY;
				FirstShift = 1;

				CursorX = PixelToRealOffX(MousePosX);
				CursorY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
				divx = CurrentX - CurrentX2 - ShiftOffsetX;
				divy = CurrentY - CurrentY2 - ShiftOffsetY;

				if ((SnapMode & 1) == 1)
				{
					AdjustOffsetForSnap(CursorX, CursorY, CurrentX, CurrentY, divx, divy, &ShiftOffsetX, &ShiftOffsetY,
					                    0);
				}

				DrawSelectedObjects(CurrentX, CurrentY, 1);
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
			DrawSelectedObjects(CurrentX, CurrentY, 1);
			DisplayCursorPosition();
			ZoomInOutProcessed = 0;
		}

//    if (CheckRightButton(&DrawXorFunction)==1) {
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
			else
				CompPlaced = 1;
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawSelectedObjects(OldX, OldY, 1);
			ZoomWindow();

			while (CtrlPressed)
				CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			else
				CompPlaced = 1;
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawSelectedObjects(OldX, OldY, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedObjects(CurrentX, CurrentY, 1);
			else
				CompPlaced = 1;
		}

		if (CheckLeftButton())
		{
			CompPlaced = 1;
			divx = CurrentX - CentreSelectedX;
			divy = CurrentY - CentreSelectedY;
			DrawSelectedObjects(CurrentX, CurrentY, 1);

			if (((SnapMode & 1) == 1) && (Count == 1))
			{
				CursorX = PixelToRealOffX(MousePosX);
				CursorY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
				AdjustOffsetForSnap(CursorX, CursorY, CurrentX, CurrentY, 0.0, 0.0, &ShiftOffsetX, &ShiftOffsetY, 3);
			}

			divx = CurrentX - CurrentX2 - ShiftOffsetX;
			divy = CurrentY - CurrentY2 - ShiftOffsetY;
			divx2 = divx;
			divy2 = divy;

			for (cnt = 0; cnt < Count; cnt++)
			{
				PlaceMovedObjects(divx2, divy2, (int32) Mode);
				divx2 += divx;
				divy2 += divy;
			}

//      (int16)LastActionNr--;
			//CheckInputMessages(0);
			SelectionEsc = 1;

		}

//    if (LeftButtonDoublePressed) {
//
//    }

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			PopUpMenu = CreatePopupMenu();

			if ((SnapMode & 1) == 0)
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_SNAP_MODE_ON, SC(149, "Snap on"));
			else
				AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_SNAP_MODE_OFF, SC(150, "Snap off"));

			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(154, "Escape"));
			TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40,
			               0, GEOMWindow, NULL);
			DestroyMenu(PopUpMenu);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawSelectedObjects(OldX, OldY, 1);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectRecord));

				if (((Mode == 0) && (LineInputDialog(&TypeObject, SC(395, "Move objects to")) == 1))
				        || ((Mode != 0) && (LineInputDialog(&TypeObject, SC(396, "Copy objects to")) == 1)))
				{
					if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 0)
					{
						if (NrParams == 2)
						{
							NewObject.x1 = (float) ParamsFloat[0];
							NewObject.y1 = (float) ParamsFloat[1];

							if (!ParametersRelative)
							{
								divx = CurrentX2 + ShiftOffsetX;
								divy = CurrentY2 + ShiftOffsetY;
								PlaceMovedObjects(NewObject.x1 - divx, NewObject.y1 - divy, (int32) Mode);
//                PlaceMovedObjects(NewObject.x1-CentreSelectedX+CurrentX2,NewObject.y1-CentreSelectedY+CurrentY2,(int16)Mode);
							}
							else
							{
								for (cnt = 0; cnt < Count; cnt++)
								{
									PlaceMovedObjects(NewObject.x1, NewObject.y1, (int32) Mode);
									NewObject.x1 += (float) ParamsFloat[0];
									NewObject.y1 += (float) ParamsFloat[1];
								}
							}

							SelectionEsc = 1;
						}
					}
				}

				SpacePressed = 0;
			}

			if (HelpAsked)
			{
				switch (Mode)
				{
				case 0:
					Help("move_objects.htm", 0);
					break;

				default:
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
			DrawSelectedObjects(CurrentX, CurrentY, 1);
			ClipMouseCursor();
		}
	}

	SelectionActive = 0;
	UnClipMouseCursor();
	SystemBusyMode = 0;
	RePaint();
	ConnectionsComponentsDrawing = 0;
	InfoStr[0] = 0;
	RedrawInfoStr(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void DrawSelectedObjects(double CurrentX, double CurrentY, int32 Mode)
{
	int32 cnt;
	double x1, y1;
	ObjectRecord *Object;
	ObjectPolygonRecord *ObjectPolygon;

	StartDrawingEditingWindow();

	CurrentObjectCode = 0;

//  MovingObjects=Mode;

	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			DrawObject(Object, CurrentX - CurrentX2 - ShiftOffsetX, CurrentY - CurrentY2 - ShiftOffsetY, 4);

			if (ClearanceVisible)
			{
				DrawObjectWithClearance(Object, CurrentX - CurrentX2 - ShiftOffsetX,
				                        CurrentY - CurrentY2 - ShiftOffsetY, 4);
			}

			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
			case OBJECT_TEXT:
//        case PIN_LINE_HOR:
//        case PIN_LINE_VER:
//        case PIN_LINE_DIAG1:
//        case PIN_LINE_DIAG2:
				break;

			default:
				InitDrawingColorWhite(0);
				x1 = Object->x1 + CurrentX - CurrentX2 - ShiftOffsetX;
				y1 = Object->y1 + CurrentY - CurrentY2 - ShiftOffsetY;
				DrawLine(MultX(x1) - 3, MultY(y1) - 3, MultX(x1) + 3, MultY(y1) + 3);
				DrawLine(MultX(x1) - 3, MultY(y1) + 3, MultX(x1) + 3, MultY(y1) - 3);
				break;
			}
		}
	}

//  MovingObjects=0;
	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			DrawObjectPolygon(ObjectPolygon, CurrentX - CurrentX2 - ShiftOffsetX, CurrentY - CurrentY2 - ShiftOffsetY,
			                  4);
		}
	}

	DrawCrossHair(16 + 8);
	/*
	  RelX=CurrentX;
	  RelY=CurrentY;
	  val=10;
	  DrawLine(MultX(RelX),MultY(RelY)+val,MultX(RelX),MultY(RelY)-val);
	  DrawLine(MultX(RelX)+val,MultY(RelY),MultX(RelX)-val,MultY(RelY));
	*/
	ExitDrawing();

	EndDrawingEditingWindow();

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceMovedObjects(double divx, double divy, int32 Mode)
{
	/*
	  mode = 0   -> move
	  mode = 1   -> copy

	*/

	int32 cnt, cnt2, TempLastActionNr, ObjectType, Changed;
	ObjectRecord NewObject, *Object, PinObject;
	ObjectPolygonRecord *ObjectPolygon;

	TempLastActionNr = LastActionNr - 1;

	Changed = 0;

	for (cnt2 = 0; cnt2 < 2; cnt2++)
	{
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);
			ObjectType = Object->ObjectType;

			if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Object->AddNr <= TempLastActionNr) && ((Mode == 0) || (Object->Layer != GEOM_NAME_LAYER)))
			{
				if (((cnt2 == 0) && (Object->Layer != DRILL_LAYER) && (Object->Layer != DRILL_UNPLATED_LAYER))
				        || ((cnt2 == 1) && ((Object->Layer == DRILL_LAYER) || (Object->Layer == DRILL_UNPLATED_LAYER))))
				{
					if (Mode == 0)
						Object->Info &= ~OBJECT_SELECTED;

					memmove(&NewObject, Object, sizeof(ObjectRecord));

					if (Object->ObjectType != OBJECT_LINE)
					{
						NewObject.x1 += (float) divx;
						NewObject.y1 += (float) divy;
					}
					else
					{
						if ((Object->Info & 1) == 1)
						{
							NewObject.x1 += (float) divx;
							NewObject.y1 += (float) divy;
						}

						if ((Object->Info & 2) == 2)
						{
							NewObject.x2 += (float) divx;
							NewObject.y2 += (float) divy;
						}
					}

					NewObject.DeleteNr = 0;

					if (Mode == 1)
						NewObject.Info &= ~OBJECT_SELECTED;

					if (AddObject(&NewObject))
					{
						if (Mode == 0)
						{
							Object = &((*Objects)[cnt]);
							Object->Info |= OBJECT_NOT_VISIBLE;
							Object->DeleteNr = (int16) LastActionNr;
						}
					}

					Changed = 1;
				}
			}
		}
	}

	memset(&PinObject, 0, sizeof(PinObject));
	memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			if (Mode == 0)
				ObjectPolygon->Info &= ~OBJECT_SELECTED;

			memmove(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
			MoveObjectPolygon(NewObjectPolygon, divx, divy, 0);
			NewObjectPolygon->DeleteNr = 0;

			if (Mode == 1)
				NewObjectPolygon->Info &= ~OBJECT_SELECTED;

			if (AddObjectPolygon(NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

				if (Mode == 0)
				{
					PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
					PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
					PinObject.Layer = ObjectPolygon->Layer;
					PinObject.PinNr = ObjectPolygon->PinNr;
					ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
					ObjectPolygon->DeleteNr = (int16) LastActionNr;
				}

				PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
			}

			Changed = 1;
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

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
	int32 cnt, cnt2, cnt3, TempLastActionNr, ModeCopy, count, CircleMode, Mirror, ObjectPolygonLength, Changed;
	double x1, y1, x2, y2, x3, y3, x4, y4, CentreX, CentreY, hx, Rotation;
	float RotationAngle;
	ObjectSubPolygonRecord *ObjectSubPolygon;
	ObjectRecord NewObject, *Object, PinObject, ChangedTextObject;
	ObjectPolygonRecord *ObjectPolygon;
	uint8 *Buf;

	x1 = 0.0;
	y1 = 0.0;
	RepeatMode = 0;
	RotationAngle = 0.0;

	if ((Mode & 32) == 32)
	{
		memset(&ChangedTextObject, 0, sizeof(ChangedTextObject));
		strcpy(ChangedTextObject.Text, "0.0");

		if (TextInputDialog(&ChangedTextObject, 0x14) != 1)
			return;

		sscanf(ChangedTextObject.Text, "%f", &RotationAngle);

		if (RotationAngle == 0.0)
			return;

		if ((RotationAngle < -360.0) || (RotationAngle > 360.0))
			return;

		if (InRange3(RotationAngle, 90.0))
			Mode = 1;

		if (InRange3(RotationAngle, 180.0))
			Mode = 2;

		if (InRange3(RotationAngle, 270.0))
			Mode = 3;
	}


	ModeCopy = Mode;

	CurrentObjectCode = 0;
	CentreX = AdjustToDrawGrid((SearchMinX + SearchMaxX) / 2);
	CentreY = AdjustToDrawGrid((SearchMinY + SearchMaxY) / 2);

	if ((Mode & 16) == 16)
	{
		Mode &= 0x0f;
		CentreX = 0.0;
		CentreY = 0.0;
	}

	TempLastActionNr = LastActionNr - 1;

	count = 0;
	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			count++;
			x1 = Object->x1;
			y1 = Object->y1;

			if (Object->ObjectType == OBJECT_LINE)
				Object->Info |= 3;
		}
	}

	if (count == 1)
	{
		CentreX = x1;
		CentreY = y1;
	}

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->AddNr <= TempLastActionNr))
		{
			Object->Info &= ~OBJECT_SELECTED;
			memmove(&NewObject, Object, sizeof(ObjectRecord));

			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
				if ((ModeCopy & 32) == 0)
				{
					RotateFlipPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, Mode);
					RotateFlipPoint2(&NewObject.x2, &NewObject.y2, CentreX, CentreY, Mode);
				}
				else
				{
					RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);
					RotatePointFromOtherPoint2(&NewObject.x2, &NewObject.y2, CentreX, CentreY, RotationAngle);
				}

				NewObject.Info = (int16) (OBJECT_SELECTED + (Object->Info & 3));
				NewObject.DeleteNr = 0;

				if (AddObject(&NewObject))
				{
					Object = &((*Objects)[cnt]);
					Object->Info |= OBJECT_NOT_VISIBLE;
					Object->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
				break;

			case OBJECT_RECT:
				if ((ModeCopy & 32) == 0)
				{
					RotateFlipPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, Mode);

					if ((Mode == 1) || (Mode == 3))
					{
						hx = NewObject.x2;
						NewObject.x2 = NewObject.y2;
						NewObject.y2 = (float) hx;
					}

					NewObject.Info = (int16) (OBJECT_SELECTED + (Object->Info & 3));
					NewObject.DeleteNr = 0;

					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[cnt]);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;
					}
				}
				else
				{
					if (Object->Thickness == 0.0)
					{	// Filled object
						NewObjectPolygon->Points[0].x = Object->x1 - Object->x2 * 0.5;
						NewObjectPolygon->Points[0].y = Object->y1 - Object->y2 * 0.5;
						NewObjectPolygon->Points[1].x = Object->x1 + Object->x2 * 0.5;
						NewObjectPolygon->Points[1].y = Object->y1 - Object->y2 * 0.5;
						NewObjectPolygon->Points[2].x = Object->x1 + Object->x2 * 0.5;
						NewObjectPolygon->Points[2].y = Object->y1 + Object->y2 * 0.5;
						NewObjectPolygon->Points[3].x = Object->x1 - Object->x2 * 0.5;
						NewObjectPolygon->Points[3].y = Object->y1 + Object->y2 * 0.5;
						NewObjectPolygon->NrVertices = 4;

						for (cnt2 = 0; cnt2 < NewObjectPolygon->NrVertices; cnt2++)
						{
							RotatePointFromOtherPoint2(&NewObjectPolygon->Points[cnt2].x,
							                           &NewObjectPolygon->Points[cnt2].y, CentreX, CentreY,
							                           RotationAngle);
						}

						NewObjectPolygon->DeleteNr = 0;
						NewObjectPolygon->Info = OBJECT_SELECTED;
						NewObjectPolygon->Layer = Object->Layer;
						NewObjectPolygon->PinNr = Object->PinNr;

						if (AddObjectPolygon(NewObjectPolygon))
						{
							DrawObjectPolygon(NewObjectPolygon, (float) 0.0, (float) 0.0, 0);
							PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
							PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
							PinObject.Layer = Object->Layer;
							PinObject.PinNr = Object->PinNr;
						}
					}
					else
					{
						NewObject.ObjectType = OBJECT_LINE;
						NewObject.Info = OBJECT_SELECTED + 3;
						NewObject.DeleteNr = 0;
						x1 = (Object->x1 - Object->x2 * 0.5);
						y1 = (Object->y1 - Object->y2 * 0.5);
						x2 = (Object->x1 - Object->x2 * 0.5);
						y2 = (Object->y1 + Object->y2 * 0.5);
						NewObject.x1 = (float) x1;
						NewObject.y1 = (float) y1;
						NewObject.x2 = (float) x2;
						NewObject.y2 = (float) y2;
						RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);
						RotatePointFromOtherPoint2(&NewObject.x2, &NewObject.y2, CentreX, CentreY, RotationAngle);

						if (AddObject(&NewObject))
						{
						}

						x1 = x2;
						y1 = y2;
						x2 = (Object->x1 + Object->x2 * 0.5);
						y2 = (Object->y1 + Object->y2 * 0.5);
						NewObject.x1 = (float) x1;
						NewObject.y1 = (float) y1;
						NewObject.x2 = (float) x2;
						NewObject.y2 = (float) y2;
						RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);
						RotatePointFromOtherPoint2(&NewObject.x2, &NewObject.y2, CentreX, CentreY, RotationAngle);

						if (AddObject(&NewObject))
						{
						}

						x1 = x2;
						y1 = y2;
						x2 = (Object->x1 + Object->x2 * 0.5);
						y2 = (Object->y1 - Object->y2 * 0.5);
						NewObject.x1 = (float) x1;
						NewObject.y1 = (float) y1;
						NewObject.x2 = (float) x2;
						NewObject.y2 = (float) y2;
						RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);
						RotatePointFromOtherPoint2(&NewObject.x2, &NewObject.y2, CentreX, CentreY, RotationAngle);

						if (AddObject(&NewObject))
						{
						}

						x1 = x2;
						y1 = y2;
						x2 = (Object->x1 - Object->x2 * 0.5);
						y2 = (Object->y1 - Object->y2 * 0.5);
						NewObject.x1 = (float) x1;
						NewObject.y1 = (float) y1;
						NewObject.x2 = (float) x2;
						NewObject.y2 = (float) y2;
						RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);
						RotatePointFromOtherPoint2(&NewObject.x2, &NewObject.y2, CentreX, CentreY, RotationAngle);

						if (AddObject(&NewObject))
						{
						}
					}

					Object = &((*Objects)[cnt]);
					Object->Info |= OBJECT_NOT_VISIBLE;
					Object->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
				break;

			case OBJECT_CIRCLE:
				CircleMode = NewObject.Info2;

				if ((ModeCopy & 32) == 0)
				{
					RotateFlipPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, Mode);

					if (Object->Info2 != 0.0)
					{
						switch (Mode)
						{
						case 1:
							NewObject.Info2 = CircleRotate90[CircleMode];
							break;

						case 2:
							NewObject.Info2 = CircleRotate180[CircleMode];
							break;

						case 3:
							NewObject.Info2 = CircleRotate270[CircleMode];
							break;

						case 4:
							NewObject.Info2 = CircleMirrorX[CircleMode];
							break;

						case 8:
							NewObject.Info2 = CircleMirrorY[CircleMode];
							break;
						}
					}
				}
				else
				{
					x2 = Object->x2;
					RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);

					if ((Object->Thickness != 0.0) || (Object->Info2 != 0.0))
					{
						GetArcEndPoints(Object, &x3, &y3, &x4, &y4, 0);
						NewObject.ObjectType = OBJECT_ARC;
						NewObject.y2 = (float) x2;
						NewObject.x3 = (float) x3;
						NewObject.y3 = (float) y3;
						NewObject.x4 = (float) x4;
						NewObject.y4 = (float) y4;
						NewObject.Info2 = 0;
						RotatePointFromOtherPoint2(&NewObject.x3, &NewObject.y3, CentreX, CentreY, RotationAngle);
						RotatePointFromOtherPoint2(&NewObject.x4, &NewObject.y4, CentreX, CentreY, RotationAngle);
					}
				}

				NewObject.Info = (int16) (OBJECT_SELECTED + (Object->Info & 3));
				NewObject.DeleteNr = 0;

				if (AddObject(&NewObject))
				{
					Object = &((*Objects)[cnt]);
					Object->Info |= OBJECT_NOT_VISIBLE;
					Object->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
				break;

			/*
			        case DRILL:
			        case DRILL_UNPLATED:
			        case PIN_PUT_THROUGH_ROUND_POWER:
			        case PIN_PUT_THROUGH_ROUND_INNER_PAD:
			          if ((ModeCopy & 32) == 0) {
			            RotateFlipPoint(&NewObject.x1,&NewObject.y1,CentreX,CentreY,Mode);
			          } else {
			            RotatePointFromOtherPoint(&NewObject.x1,&NewObject.y1,CentreX,CentreY,RotationAngle);
			          }
			          NewObject.Info=OBJECT_SELECTED+(Object->Info & 3);
			          NewObject.DeleteNr=0;
			          if (AddObject(&NewObject)) {
			            Object=&((*Objects)[cnt]);
			            DrawObject(&NewObject,0.0,0.0,0);
			            DrawPinTextObject(&NewObject,0);
			            if (ClearanceVisible) {
			              DrawObjectWithClearance(&NewObject,0.0,0.0,0);
			            }
			            Object->Info|=OBJECT_NOT_VISIBLE;
			            Object->DeleteNr=(int16)LastActionNr;
			          }
			          Changed=1;
			          break;
			*/
			case OBJECT_ARC:
				if ((ModeCopy & 32) == 0)
				{
					RotateFlipPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, Mode);

					switch (Mode)
					{
					case 1:
						hx = NewObject.x3;
						NewObject.x3 = -NewObject.y3;
						NewObject.y3 = (float) hx;
						hx = NewObject.x4;
						NewObject.x4 = -NewObject.y4;
						NewObject.y4 = (float) hx;
						break;

					case 2:
						NewObject.x3 *= (float) -1.0;
						NewObject.y3 *= (float) -1.0;
						NewObject.x4 *= (float) -1.0;
						NewObject.y4 *= (float) -1.0;
						break;

					case 3:
						hx = NewObject.x3;
						NewObject.x3 = NewObject.y3;
						NewObject.y3 = (float) -hx;
						hx = NewObject.x4;
						NewObject.x4 = NewObject.y4;
						NewObject.y4 = (float) -hx;
						break;

					case 4:
						hx = NewObject.y4;
						NewObject.y4 = NewObject.y3;
						NewObject.y3 = hx;
						hx = NewObject.x4;
						NewObject.x4 = NewObject.x3;
						NewObject.x3 = hx;
						NewObject.x3 *= (float) -1.0;
						NewObject.x4 *= (float) -1.0;
						break;

					case 8:
						hx = NewObject.y4;
						NewObject.y4 = NewObject.y3;
						NewObject.y3 = hx;
						hx = NewObject.x4;
						NewObject.x4 = NewObject.x3;
						NewObject.x3 = hx;
						NewObject.y3 *= (float)-1.0;
						NewObject.y4 *= (float)-1.0;
						break;
					}
				}
				else
				{
					RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);

					if ((Object->Thickness != 0.0) || (Object->Layer == PLACEMENT_OUTLINE_LAYER))
					{
						RotatePointFromOtherPoint2(&NewObject.x3, &NewObject.y3, 0.0, 0.0, RotationAngle);
						RotatePointFromOtherPoint2(&NewObject.x4, &NewObject.y4, 0.0, 0.0, RotationAngle);
					}
				}

				NewObject.Info = (int16) (OBJECT_SELECTED + (Object->Info & 3));
				NewObject.DeleteNr = 0;

				if (AddObject(&NewObject))
				{
					Object = &((*Objects)[cnt]);
					Object->Info |= OBJECT_NOT_VISIBLE;
					Object->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
				break;

			case OBJECT_TEXT:
				Rotation = NewObject.RotationAngle;
				Mirror = 0;

				if (Rotation > 1000.0)
				{
					Rotation -= 2000.0;
					Mirror = 1;
				}

				if ((ModeCopy & 32) == 0)
				{
					switch (Mode)
					{
					case 1:
						Rotation += 90.0;
						break;

					case 2:
						Rotation += 180.0;
						break;

					case 3:
						Rotation += 270.0;
						break;

					case 4:  // Flip X
						Mirror ^= 1;
						break;

					case 8:  // Flip Y
						Mirror ^= 1;
						Rotation += 180.0;
						break;

					case 9:	// Mirror = 0
						Mirror = 0;
						break;

					case 10:	// Mirror = 1
						Mirror = 1;
						break;

					case 11:	// Swap mirror
						Mirror ^= 1;
						break;
					}

					if (Rotation >= 360.0)
						Rotation -= 360.0;

					RotateFlipPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, Mode);
				}
				else
				{
					Rotation += RotationAngle;
					if (Rotation >= 360.0)
						Rotation -= 360.0;

					if (Rotation <= -360.0)
						Rotation += 360.0;

					RotatePointFromOtherPoint2(&NewObject.x1, &NewObject.y1, CentreX, CentreY, RotationAngle);
				}

				NewObject.Info = OBJECT_SELECTED + (Object->Info & 3);
				NewObject.DeleteNr = 0;

				if (Mirror == 0)
					NewObject.RotationAngle = Rotation;
				else
					NewObject.RotationAngle = Rotation + 2000.0;

				if (AddObject(&NewObject))
				{
					Object = &((*Objects)[cnt]);
					Object->Info |= OBJECT_NOT_VISIBLE;
					Object->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
				break;
			}
		}
	}

	memset(&PinObject, 0, sizeof(PinObject));
	memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			memmove(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));

			count = ObjectPolygon->NrVertices;
			ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

			if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
				count = ObjectPolygon->NrVerticesMainPolygon;

			for (cnt2 = 0; cnt2 < count; cnt2++)
			{
				if ((ModeCopy & 32) == 0)
				{
					RotateFlipPoint2(&NewObjectPolygon->Points[cnt2].x, &NewObjectPolygon->Points[cnt2].y, CentreX,
					                 CentreY, Mode);
				}
				else
				{
					RotatePointFromOtherPoint2(&NewObjectPolygon->Points[cnt2].x, &NewObjectPolygon->Points[cnt2].y,
					                           CentreX, CentreY, RotationAngle);
				}
			}

			if (NewObjectPolygon->NrSubPolygons > 0)
			{
				Buf = (uint8 *) NewObjectPolygon;
				Buf += sizeof(ObjectPolygonInitRecord);
				Buf += count * sizeof(PointRecord);

				for (cnt2 = 0; cnt2 < NewObjectPolygon->NrSubPolygons; cnt2++)
				{
					if ((Buf - (uint8 *) NewObjectPolygon) >=
					        (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
						break;

					ObjectSubPolygon = (ObjectSubPolygonRecord *) Buf;

					if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
						break;

					count = ObjectSubPolygon->NrVertices;

					if (count >= 65536)
						break;

					for (cnt3 = 0; cnt3 < count; cnt3++)
					{
						if ((ModeCopy & 32) == 0)
						{
							RotateFlipPoint2(&ObjectSubPolygon->Points[cnt3].x, &ObjectSubPolygon->Points[cnt3].y,
							                 CentreX, CentreY, Mode);
						}
						else
						{
							RotatePointFromOtherPoint2(&ObjectSubPolygon->Points[cnt3].x,
							                           &ObjectSubPolygon->Points[cnt3].y, CentreX, CentreY,
							                           RotationAngle);
						}
					}

					Buf += sizeof(ObjectSubPolygonInitRecord);
					Buf += count * sizeof(PointRecord);
				}
			}

			NewObjectPolygon->DeleteNr = 0;
			NewObjectPolygon->Info = OBJECT_SELECTED;

			if (AddObjectPolygon(NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
				CurrentObjectCode = 0;
				PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
			}

			Changed = 1;
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveAllObjects(double x, double y)
{
	int32 cnt, ObjectInfo;

	ObjectRecord *Object;
	ObjectPolygonRecord *ObjectPolygon;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Object->x1 -= (float) x;
			Object->y1 -= (float) y;

			if (Object->ObjectType == OBJECT_LINE)
			{
				Object->x2 -= (float) x;
				Object->y2 -= (float) y;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
			MoveObjectPolygon(ObjectPolygon, -x, -y, 0);
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 AssignObjectsToPin(ObjectRecord * ObjectText)
{
	int32 cnt, Layer, Found, ObjectType;
	ObjectRecord *Object, PinObject;
	ObjectPolygonRecord *ObjectPolygon;
	PinInfoRecord *PinInfo, NewPinInfo;

	Found = -1;
	cnt = 0;

	while ((cnt < NrPinObjects) && (Found == -1))
	{
		PinInfo = &((*PinInfos)[cnt]);

		if (stricmpUTF8((LPSTR) PinInfo->PinText, (LPSTR) ObjectText->Text) == 0)
			Found = cnt;

		cnt++;
	}

	if (Found == -1)
	{
		memset(&NewPinInfo, 0, sizeof(PinInfoRecord));
		memmove(&NewPinInfo.PinText, ObjectText->Text, sizeof(NewPinInfo.PinText) - 1);
		Found = NrPinObjects;
		AddPinInfo(&NewPinInfo);
	}

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			memmove(&NewObject, Object, sizeof(ObjectRecord));
			NewObject.Info &= ~OBJECT_SELECTED;
		}
	}

	BackGroundActive = 0;


	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectType = Object->ObjectType;

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->Layer != DRILL_LAYER) && (Object->Layer != DRILL_UNPLATED_LAYER))
		{
			memmove(&NewObject, Object, sizeof(ObjectRecord));
			NewObject.Info &= ~OBJECT_SELECTED;
		}
	}



	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectType = Object->ObjectType;

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((Object->Layer == DRILL_LAYER) || (Object->Layer == DRILL_UNPLATED_LAYER)))
		{
			memmove(&NewObject, Object, sizeof(ObjectRecord));
			NewObject.Info &= ~OBJECT_SELECTED;
		}
	}

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Layer = Object->Layer;
			ObjectType = Object->ObjectType;

			switch (Layer)
			{
			case POWER_PAD_LAYER:
			case DRILL_LAYER:
			case INNER_PAD_LAYER:
				Object->PinNr = Found;
				Object->Info &= ~OBJECT_SELECTED;
				DataBaseChanged = 1;
				break;

			default:
				if (Layer < 32)
				{
					Object->PinNr = Found;
					Object->Info &= ~OBJECT_SELECTED;
					DataBaseChanged = 1;
				}

				break;
			}

			Object->Info &= ~OBJECT_SELECTED;
		}
	}

	memset(&PinObject, 0, sizeof(PinObject));

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			Layer = ObjectPolygon->Layer;

			if (Layer < 32)
			{
				ObjectPolygon->PinNr = Found;
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
				DataBaseChanged = 1;
			}
		}
	}

	RePaint();
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeLineWidth(double value)
{
	int32 cnt, TempLastActionNr, Layer, ObjectType, Changed;
	ObjectRecord NewObject, *Object;

	TempLastActionNr = LastActionNr - 1;

	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->Layer != PLACEMENT_OUTLINE_LAYER) && (Object->AddNr <= TempLastActionNr))
		{
			Layer = Object->Layer;
			ObjectType = Object->ObjectType;
			Object->Info &= ~OBJECT_SELECTED;
			memmove(&NewObject, Object, sizeof(ObjectRecord));
			NewObject.Thickness = (float) (max(1.0, value));
			NewObject.DeleteNr = 0;

			if (AddObject(&NewObject))
			{
				Object = &((*Objects)[cnt]);
				Object->Info |= OBJECT_NOT_VISIBLE;
				Object->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeTraceWidth(double value)
{
	int32 cnt, TempLastActionNr, Layer, ObjectType, Changed, LayerOk;
	ObjectRecord NewObject, *Object;


	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->AddNr <= TempLastActionNr))
		{
			Layer = Object->Layer;
			LayerOk = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
				LayerOk = 1;
				break;

			default:
				if (Layer < 32)
					LayerOk = 1;

				break;
			}

			if (LayerOk)
			{
				ObjectType = Object->ObjectType;

				switch (ObjectType)
				{
				case OBJECT_LINE:
					Object->Info &= ~OBJECT_SELECTED;
					memmove(&NewObject, Object, sizeof(ObjectRecord));
					NewObject.y2 = (float) value;
					NewObject.DeleteNr = 0;

					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[cnt]);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;
					}

					Changed = 1;
					break;
				}
			}
		}
	}

	RePaint();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeClearance(double value, int32 mode)
{
	int32 cnt, TempLastActionNr, Layer, SelectionMask, ObjectType, Changed, OkToChange;
	ObjectRecord NewObject, *Object, PinObject;
	ObjectPolygonRecord *ObjectPolygon;

	if (mode == 0)
		SelectionMask = OBJECT_SELECTED;
	else
		SelectionMask = 0;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | SelectionMask)) == SelectionMask)
		        && (Object->AddNr <= TempLastActionNr))
		{
			Layer = Object->Layer;
			OkToChange = 0;
			ObjectType = Object->ObjectType;

			switch (Layer)
			{
			case DRILL_LAYER:
			case DRILL_UNPLATED_LAYER:
				switch (ObjectType)
				{
				case OBJECT_CIRCLE:
					OkToChange = 1;
					break;
				}

				break;

			default:
				if (Layer < 32)
				{
					switch (ObjectType)
					{
					case OBJECT_LINE:
					case OBJECT_RECT:
					case OBJECT_ARC:
					case OBJECT_CIRCLE:
						OkToChange = 1;
						break;
					}
				}

				break;
			}

			if (OkToChange)
			{
				Object->Info &= ~OBJECT_SELECTED;
				memmove(&NewObject, Object, sizeof(ObjectRecord));
				NewObject.Clearance = (float) value;
				NewObject.DeleteNr = 0;

				if (AddObject(&NewObject))
				{
					Object = &((*Objects)[cnt]);
					Object->Info |= OBJECT_NOT_VISIBLE;
					Object->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
			}
		}
	}

// *******************************************************************************************************
	memset(&PinObject, 0, sizeof(PinObject));
	memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | SelectionMask)) == SelectionMask)
		        && (ObjectPolygon->AddNr <= TempLastActionNr) && (ObjectPolygon->Layer < 32))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			memmove(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
			NewObjectPolygon->Clearance = (float) value;
			NewObjectPolygon->DeleteNr = 0;

			if (AddObjectPolygon(NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
				PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void Changetext()
{

	int32 cnt, TempLastActionNr, Layer, TextMode, ObjectType, Changed;
	ObjectRecord NewObject, *Object;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;
	GetNrSelectObjects();

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->AddNr <= TempLastActionNr))
		{
			Layer = Object->Layer;
			ObjectType = Object->ObjectType;

			if (ObjectType == OBJECT_TEXT)
			{
				memmove(&NewObject, Object, sizeof(ObjectRecord));
				TextMode = 1;

				if (Object->Layer == GEOM_NAME_LAYER)
					TextMode = 2;

//        if (NrTextsSelected>1) TextMode+=0x10;
				if (TextInputDialog(&NewObject, TextMode) == 1)
				{
					Object->Info &= ~OBJECT_SELECTED;
					NewObject.Info &= ~OBJECT_SELECTED;
					NewObject.DeleteNr = 0;

					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[cnt]);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;

						if (Object->Layer == GEOM_NAME_LAYER)
						{
							sprintf(EditFile, "%s\\%s.shp", DesignPath, NewObject.Text);
							CheckFilenameForSaving = 1;
						}
					}
				}
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangetextHeight()
{
	int32 cnt, TempLastActionNr, Layer, ObjectType, Changed;
	float value;
	ObjectRecord NewObject, *Object;
	ObjectRecord2 TypeObject;

	value = (100 * 2540);
	memset(&TypeObject, 0, sizeof(ObjectRecord));

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->ObjectType == OBJECT_TEXT) && (TypeObject.Text[0] == 0))
			GetUnitsValue(Units, Object->x2, TypeObject.Text, 0);
	}

	if (LineInputDialog(&TypeObject, SC(309, "Change text height")) != 1)
		return;

	if ((sscanf(TypeObject.Text, "%f", &value)) != 1)
		return;

	switch (Units)
	{
	case 0:
		value = value * (float) 2540.0;
		break;

	case 1:
		value = value * (float) 100000.0;
		break;
	}


	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->AddNr <= TempLastActionNr))
		{
			Layer = Object->Layer;
			ObjectType = Object->ObjectType;

			if (ObjectType == OBJECT_TEXT)
			{
				memmove(&NewObject, Object, sizeof(ObjectRecord));
				Object->Info &= ~OBJECT_SELECTED;
				NewObject.Info &= ~OBJECT_SELECTED;
				NewObject.DeleteNr = 0;
				NewObject.x2 = (float) value;

				if (AddObject(&NewObject))
				{
					Object = &((*Objects)[cnt]);
					Object->DeleteNr = (int16) LastActionNr;
					Object->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeParamsObjects(int32 mode)
{
	int32 cnt, TempLastActionNr, Layer, NrParams, ObjectType, Changed;
	double Width, Height, Angle1, Angle2;
	ObjectRecord NewObject, *Object;
	ObjectRecord2 TypeObject;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	Width = 0.0;
	Angle1 = 0.0;
	Angle2 = 0.0;
	Height = 20 * 2540.0;
	memset(&TypeObject, 0, sizeof(ObjectRecord));

	switch (mode)
	{
	case 0:
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Object->ObjectType == OBJECT_CIRCLE) && (TypeObject.Text[0] == 0))
				GetUnitsValue(Units, Object->x2, TypeObject.Text, 0);
		}

		if (LineInputDialog(&TypeObject, SC(397, "Change circle diameter")) != 1)
			return -1;

		if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) != 1)
			return -1;

		Width = ParamsFloat[0];

		if (Width < 10)
			return -1;

		break;

	case 1:
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Object->ObjectType == OBJECT_RECT) && (TypeObject.Text[0] == 0))
			{
				GetUnitsValue(Units, Object->x2, str, 0);
				GetUnitsValue(Units, Object->y2, str2, 0);
				sprintf(TypeObject.Text, "%s,%s", str, str2);
			}
		}

		if (LineInputDialog(&TypeObject, SC(398, "Change rectangle width,height")) != 1)
			return -1;

		if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) != 2)
			return -1;

		Width = ParamsFloat[0];
		Height = ParamsFloat[1];

		if ((Width < 10) || (Height < 10))
			return -1;

		break;

	case 2:
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Object->ObjectType == OBJECT_ARC) && (TypeObject.Text[0] == 0))
				GetUnitsValue(Units, Object->x2, TypeObject.Text, 0);
		}

		if (LineInputDialog(&TypeObject, SC(399, "Change arc diameter")) != 1)
			return -1;

		if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) != 1)
			return -1;

		Width = ParamsFloat[0];

		if (Width < 10)
			return -1;

		break;

	case 3:
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Object->ObjectType == OBJECT_ARC) && (TypeObject.Text[0] == 0))
			{
				GetArcAngle(Object, &Angle1, &Angle2);
				GetUnitsValue(Units, Object->x2, str, 0);
				GetUnitsValue(Units, Object->y2, str2, 0);
				sprintf(TypeObject.Text, "%.2f,%.2f", Angle1 * 180 / PI, Angle2 * 180 / PI);
			}
		}

		if (LineInputDialog(&TypeObject, SC(400, "Change angles arc (Degrees counter clock wise)")) != 1)
			return -1;

		if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) != 2)
			return -1;

		Angle1 = ParamsFloat[0];
		Angle2 = ParamsFloat[1];
		break;
	}


	TempLastActionNr = LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->AddNr <= TempLastActionNr))
		{
			Layer = Object->Layer;
			ObjectType = Object->ObjectType;

			switch (mode)
			{
			case 0:
				if (ObjectType == OBJECT_CIRCLE)
				{
					memmove(&NewObject, Object, sizeof(ObjectRecord));
					Object->Info &= ~OBJECT_SELECTED;
					NewObject.Info &= ~OBJECT_SELECTED;
					NewObject.DeleteNr = 0;
					NewObject.x2 = (float) Width;

					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[cnt]);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;
					}
				}

				break;

			case 1:
				if (ObjectType == OBJECT_RECT)
				{
					memmove(&NewObject, Object, sizeof(ObjectRecord));
					Object->Info &= ~OBJECT_SELECTED;
					NewObject.Info &= ~OBJECT_SELECTED;
					NewObject.DeleteNr = 0;
					NewObject.x2 = (float) Width;
					NewObject.y2 = (float) Height;

					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[cnt]);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;
					}
				}

				break;

			case 2:
				if (ObjectType == OBJECT_ARC)
				{
					memmove(&NewObject, Object, sizeof(ObjectRecord));
					Object->Info &= ~OBJECT_SELECTED;
					NewObject.Info &= ~OBJECT_SELECTED;
					NewObject.DeleteNr = 0;
					NewObject.x2 = (float) Width;
					NewObject.y2 = (float) Width;

					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[cnt]);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;
					}
				}

				break;

			case 3:
				if (ObjectType == OBJECT_ARC)
				{
					memmove(&NewObject, Object, sizeof(ObjectRecord));
					Object->Info &= ~OBJECT_SELECTED;
					NewObject.Info &= ~OBJECT_SELECTED;
					NewObject.DeleteNr = 0;
					NewObject.x3 = NewObject.x2 * (float) cos(Angle1 * PI / 180);
					NewObject.y3 = NewObject.x2 * (float) sin(Angle1 * PI / 180);
					NewObject.x4 = NewObject.x2 * (float) cos(Angle2 * PI / 180);
					NewObject.y4 = NewObject.x2 * (float) sin(Angle2 * PI / 180);

					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[cnt]);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;
					}
				}

				break;
			}

		}
	}

	RePaint();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CopyObjectsToOtherLayer(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, TempLastActionNr, Layer2, CopyMode, count, Changed, OkToCopy;
	ObjectRecord NewObject, *Object, PinObject, NewObjectLine;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	ObjectPolygonRecord *ObjectPolygon;

	Object = NULL;

	Layer2 = Layer;

	switch (Layer)
	{
	case DRILL_LAYER:
	case DRILL_UNPLATED_LAYER:
	case POWER_PAD_LAYER:
		Layer2 = -1;
		break;
	}

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if ((Object->Layer == Layer2) && (Object->Layer != -1))
			{
				GetLayerText(Layer, str, 0);
				sprintf(str2, SC(401, "Can not copy selected objects on the same layer [ %s ]"), str);
				MessageBoxUTF8(GEOMWindow, str2, SC(48, "Error"), MB_APPLMODAL | MB_OK);
				return;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (ObjectPolygon->Layer == Layer2)
			{
				GetLayerText(Layer, str, 0);
				sprintf(str2, SC(401, "Can not copy selected objects on the same layer [ %s ]"), str);
				MessageBoxUTF8(GEOMWindow, str2, SC(48, "Error"), MB_APPLMODAL | MB_OK);
				return;
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	TempLastActionNr = LastActionNr - 1;

	Changed = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->AddNr <= TempLastActionNr))
		{
			memmove(&NewObject, Object, sizeof(ObjectRecord));

			if (mode == 0)
			{	// Copy object
//        NewObject.x1+=1e5;
//        NewObject.y1+=1e5;
				switch (Object->ObjectType)
				{
				case OBJECT_LINE:
//            NewObject.x2+=1e5;
//            NewObject.y2+=1e5;
					break;
				}
			}

			NewObject.Layer = Layer;
			Object->Info &= ~OBJECT_SELECTED;
			OkToCopy = 1;

#ifdef _DEBUG

			if (Object->ObjectType == OBJECT_CIRCLE)
				ok = 1;

#endif

			switch (Layer)
			{
			case DRILL_LAYER:
			case DRILL_UNPLATED_LAYER:
				if (Object->Clearance == 0.0)
					NewObject.Clearance = (float) CurrentClearance;

				break;

			case PLACEMENT_OUTLINE_LAYER:
				NewObject.Thickness = 0.0;
				break;

			case SILKSCREEN_TOP_LAYER:
			case SILKSCREEN_BOTTOM_LAYER:
				if (Object->ObjectType == OBJECT_CIRCLE)
				{
					if ((Object->Thickness > 0.0) && (Object->Thickness < 2.0))
						NewObject.Thickness = (float) CurrentSilkscreenLine;
				}

				if ((Object->ObjectType == OBJECT_ARC) || (Object->ObjectType == OBJECT_LINE)
				        || (Object->ObjectType == OBJECT_TEXT))
				{
					if (Object->Thickness < 2.0)
						NewObject.Thickness = (float) CurrentSilkscreenLine;
				}

				break;

			case COMP_OUTLINE_LAYER:
				if (Object->ObjectType == OBJECT_CIRCLE)
				{
					if ((Object->Thickness > 0.0) && (Object->Thickness < 2.0))
						NewObject.Thickness = (float) CurrentSilkscreenLine;
				}

				if ((Object->ObjectType == OBJECT_ARC) || (Object->ObjectType == OBJECT_LINE)
				        || (Object->ObjectType == OBJECT_TEXT))
				{
					if (Object->Thickness < 2.0)
						NewObject.Thickness = (float) CurrentCompOutLine;
				}

				break;

			case BOARD_OUTLINE_LAYER:
				if (Object->Thickness < 2.0)
					NewObject.Thickness = (float) CurrentBoardOutLine;

				break;

			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
				if (Object->ObjectType == OBJECT_CIRCLE)
				{
					if ((Object->Thickness > 0.0) && (Object->Thickness < 2.0))
						NewObject.Thickness = (float) CurrentSilkscreenLine;
				}

				if ((Object->ObjectType == OBJECT_ARC) || (Object->ObjectType == OBJECT_LINE)
				        || (Object->ObjectType == OBJECT_TEXT))
				{
					if (Object->Thickness == 0.0)
						NewObject.Thickness = (float) CurrentInfoLine;
				}

				break;

			case GEOM_NAME_LAYER:
				if (Object->ObjectType != OBJECT_TEXT)
					OkToCopy = 0;

				break;

			default:
				if (Layer < 32)
				{
					if (Object->ObjectType == OBJECT_LINE)
					{
						if (Object->Thickness < 2.0)
							NewObject.Thickness = (float) TraceThickness;
					}

					if (Object->Clearance < 2.0)
						NewObject.Clearance = (float) CurrentClearance;
				}
				else
				{
					if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					{
						switch (Object->ObjectType)
						{
						case OBJECT_ARC:
						case OBJECT_LINE:
						case OBJECT_TEXT:
							OkToCopy = 0;
							break;

						case OBJECT_CIRCLE:
							if ((Object->Info2 > 0) && (Object->Info2 < 15))
								OkToCopy = 0;

							break;
						}
					}
				}

				break;
			}

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
				if (Object->ObjectType == OBJECT_CIRCLE)
				{
					NewObject.Info2 = 0;
					NewObject.Thickness = 0.0;
				}

				if (Object->ObjectType == OBJECT_RECT)
					NewObject.Thickness = 0.0;

				break;

			case PLACEMENT_OUTLINE_LAYER:
				if (Object->ObjectType == OBJECT_CIRCLE)
				{
					if (Object->Info2 == 0)
						NewObject.Info2 = 15;

					NewObject.Thickness = 0.0;
				}

				break;

			case DRILL_LAYER:
			case DRILL_UNPLATED_LAYER:
			case POWER_PAD_LAYER:
			case INNER_PAD_LAYER:
				switch (Object->ObjectType)
				{
				case OBJECT_ARC:
				case OBJECT_LINE:
				case OBJECT_TEXT:
				case OBJECT_RECT:
					OkToCopy = 0;
					break;
				}

				break;

			default:
				if (Layer < 32)
				{
					if (Object->ObjectType == OBJECT_CIRCLE)
					{
						NewObject.Info2 = 0;
						NewObject.Thickness = 0.0;
					}

					if (Object->ObjectType == OBJECT_RECT)
						NewObject.Thickness = 0.0;
				}
				else
				{
					if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					{
						if (Object->ObjectType == OBJECT_CIRCLE)
						{
							NewObject.Info2 = 0;
							NewObject.Thickness = 0.0;
						}

						if (Object->ObjectType == OBJECT_RECT)
							NewObject.Thickness = 0.0;
					}
				}

				break;
			}

			switch (Object->ObjectType)
			{
			case OBJECT_TEXT:
				switch (Layer)
				{
//            case SOLD_MASK_BOTTOM_LAYER:
//            case SOLD_MASK_TOP_LAYER:
				case PASTE_MASK_BOTTOM_LAYER:
				case PASTE_MASK_TOP_LAYER:
				case DRILL_LAYER:
				case DRILL_UNPLATED_LAYER:
				case POWER_PAD_LAYER:
				case INNER_PAD_LAYER:
				case PLACEMENT_OUTLINE_LAYER:
					OkToCopy = 0;
					break;

				default:
					if (Layer < 32)
						OkToCopy = 0;
					else
					{
						if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
							OkToCopy = 0;
					}

					break;
				}

				break;

			case OBJECT_CIRCLE:
				if ((Object->Thickness < 2.0) && (Layer != PLACEMENT_OUTLINE_LAYER))
				{
//            NewObject.Info2=0;
				}

				break;
			}

			if (mode == 1)
			{	// Move object
				NewObject.Info &= ~(OBJECT_SELECTED | 3);
			}

			if (OkToCopy)
			{
				NewObject.DeleteNr = 0;

				if (AddObject(&NewObject))
				{
					Object = &((*Objects)[cnt]);

					if (mode == 1)
					{	// Move object
						Object->DeleteNr = (int16) LastActionNr;
						Object->Info |= OBJECT_NOT_VISIBLE;
					}
				}
			}
		}
	}

// *******************************************************************************************************

	memset(&PinObject, 0, sizeof(PinObject));
	memset(NewObjectPolygon, 0, sizeof(*NewObjectPolygon));

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			OkToCopy = 0;

			switch (Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
			case SILKSCREEN_TOP_LAYER:
			case SILKSCREEN_BOTTOM_LAYER:
			case COMP_OUTLINE_LAYER:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
			case PLACEMENT_OUTLINE_LAYER:
			case BOARD_OUTLINE_LAYER:
				OkToCopy = 1;
				break;

			case GEOM_NAME_LAYER:
				break;

			default:
				if (Layer < 32)
					OkToCopy = 1;
				else
				{
					if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
						OkToCopy = 1;
				}

				break;
			}

			if (OkToCopy)
			{
				CopyMode = 0;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM_LAYER:
				case SOLD_MASK_TOP_LAYER:
				case PASTE_MASK_BOTTOM_LAYER:
				case PASTE_MASK_TOP_LAYER:
				case SILKSCREEN_TOP_LAYER:
				case SILKSCREEN_BOTTOM_LAYER:
				case COMP_OUTLINE_LAYER:
				case INFO_LAYER:
				case INFO_LAYER2:
				case INFO_LAYER3:
				case INFO_LAYER4:
					CopyMode = 1;
					break;

				case PLACEMENT_OUTLINE_LAYER:
				case BOARD_OUTLINE_LAYER:
					CopyMode = 2;
					break;

				default:
					if (Layer < 32)
					{
					}
					else
					{
						if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
							CopyMode = 1;
					}

					break;
				}

				switch (CopyMode)
				{
				case 1:
					memmove(NewObjectPolygon, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
					ObjectPolygon->Info &= ~OBJECT_SELECTED;
					NewObjectPolygon->Layer = Layer;
					NewObject.DeleteNr = 0;
					NewObjectPolygon->DeleteNr = 0;

					if (mode == 0)
					{	// Copy object
						MoveObjectPolygon(NewObjectPolygon, 1e5, 1e5, 0);
					}
					else
						NewObjectPolygon->Info &= ~OBJECT_SELECTED;

					if (AddObjectPolygon(NewObjectPolygon))
					{
						ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

						if (mode == 1)
						{	// Move object
							PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
							PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
							PinObject.Layer = ObjectPolygon->Layer;
							PinObject.PinNr = ObjectPolygon->PinNr;
							ObjectPolygon->DeleteNr = (int16) LastActionNr;
							ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
						}

						PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
						PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
						PinObject.Layer = ObjectPolygon->Layer;
						PinObject.PinNr = ObjectPolygon->PinNr;
					}

					break;

				case 2:
					ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
					ObjectPolygon->Info &= ~OBJECT_SELECTED;

					if (mode == 1)
					{	// Move object
						PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
						PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
						PinObject.Layer = ObjectPolygon->Layer;
						PinObject.PinNr = ObjectPolygon->PinNr;
						ZeroUnusedObjects(0);
						ObjectPolygon->DeleteNr = (int16) LastActionNr;
						ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
					}

					memset(&NewObjectLine, 0, sizeof(ObjectRecord));
					NewObjectLine.ObjectType = OBJECT_LINE;
					NewObjectLine.Layer = Layer;
					NewObjectLine.PinNr = -1;

					if (mode == 0)
					{	// Copy object
						NewObjectLine.Info = OBJECT_SELECTED | 3;
					}

					switch (Layer)
					{
					case PLACEMENT_OUTLINE_LAYER:
						break;

					case BOARD_OUTLINE_LAYER:
						NewObjectLine.Thickness = (float) CurrentBoardOutLine;
						break;
					}

					count = ObjectPolygon->NrVertices;

					if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
						count = ObjectPolygon->NrVerticesMainPolygon;

					for (cnt2 = 0; cnt2 < count; cnt2++)
					{
						NewObjectLine.x1 = (float) ObjectPolygon->Points[cnt2].x;
						NewObjectLine.y1 = (float) ObjectPolygon->Points[cnt2].y;

						if (cnt2 < count - 1)
						{
							NewObjectLine.x2 = (float) ObjectPolygon->Points[cnt2 + 1].x;
							NewObjectLine.y2 = (float) ObjectPolygon->Points[cnt2 + 1].y;
						}
						else
						{
							NewObjectLine.x2 = (float) ObjectPolygon->Points[0].x;
							NewObjectLine.y2 = (float) ObjectPolygon->Points[0].y;
						}

						if (mode == 0)
						{	// Copy object
							NewObjectLine.x1 += 1e5;
							NewObjectLine.y1 += 1e5;
							NewObjectLine.x2 += 1e5;
							NewObjectLine.y2 += 1e5;
						}

						AddObject(&NewObjectLine);
					}

					break;
				}
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void CopyOnMultipleCoordinates()
{
	ObjectRecord *Object;
	int32 cnt, NrParams, FirstSelected = 1;
	ObjectRecord2 TypeObject;
	double x, y;

	CurrentX2 = 0.0;
	CurrentY2 = 0.0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && ((Object->ObjectType == OBJECT_RECT) || (Object->ObjectType == DRILL)
		            || (Object->ObjectType == DRILL_UNPLATED) || (Object->Layer == OBJECT_CIRCLE)) && (FirstSelected))
		{
			CurrentX2 = Object->x1;
			CurrentY2 = Object->y1;
			FirstSelected = 0;
		}
	}

	memset(&TypeObject, 0, sizeof(ObjectRecord2));

	if (LineInputDialog(&TypeObject, SC(402, "Copy objects to x1,y1,x2,y2,x3,y3,x4,y4, .... )")) == 1)
	{
		if (((NrParams = ScanParameters(-1, TypeObject.Text, 0)) > 1) && ((NrParams & 1) == 0))
		{
			for (cnt = 0; cnt < NrParams / 2; cnt++)
			{
				x = ParamsFloat[cnt * 2];
				y = ParamsFloat[cnt * 2 + 1];
				PlaceMovedObjects(x, y, 1);
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ScaleSelectedObjects(int32 mode)
{
	int32 count, cnt, cnt2, cnt3, TempLastActionNr, ObjectPolygonLength, Changed;
	double DifX, DifY, x1, y1, minx, miny;
	float Scale;
	uint8 *Buf;
	ObjectRecord *Object, NewObject, ChangedTextObject, PinObject;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectSubPolygonRecord *ObjectSubPolygon;

	memset(&NewObject, 0, sizeof(NewObject));
	memset(&ChangedTextObject, 0, sizeof(ChangedTextObject));
	strcpy(ChangedTextObject.Text, "1.00000");

	if (TextInputDialog(&ChangedTextObject, 0x113) != 1)
		return;

	sscanf(ChangedTextObject.Text, "%f", &Scale);

	if ((Scale < 0.0001) || (Scale > 10000.0))
		return;

	TempLastActionNr = LastActionNr - 1;
	Changed = 0;


	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->AddNr <= TempLastActionNr))
		{
			Object->Info &= ~OBJECT_SELECTED;
			memmove(&NewObject, Object, sizeof(ObjectRecord));

			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
			case OBJECT_RECT:
				if ((ScaleOption & 1) == 0)
				{
					if (Object->ObjectType == OBJECT_LINE)
					{
						DifX = Object->x2 - Object->x1;
						DifY = Object->y2 - Object->y1;
						NewObject.x2 = NewObject.x1 + (float) (Scale * DifX);
						NewObject.y2 = NewObject.y1 + (float) (Scale * DifY);
					}
					else
					{
						NewObject.x2 *= (float) Scale;
						NewObject.y2 *= (float) Scale;
					}
				}
				else
				{
					NewObject.x1 *= (float) Scale;
					NewObject.y1 *= (float) Scale;
					NewObject.x2 *= (float) Scale;
					NewObject.y2 *= (float) Scale;
				}

				NewObject.Thickness *= (float) Scale;
				break;

			case OBJECT_CIRCLE:
			case OBJECT_TEXT:
				if ((ScaleOption & 1) == 1)
				{
					NewObject.x1 *= (float) Scale;
					NewObject.y1 *= (float) Scale;
				}

				NewObject.x2 *= (float) Scale;
				NewObject.Thickness *= (float) Scale;
				break;

			case OBJECT_ARC:
				if ((ScaleOption & 1) == 1)
				{
					NewObject.x1 *= (float) Scale;
					NewObject.y1 *= (float) Scale;
				}

				NewObject.x2 *= (float) Scale;
				NewObject.y2 *= (float) Scale;
				NewObject.x3 *= (float) Scale;
				NewObject.y3 *= (float) Scale;
				NewObject.x4 *= (float) Scale;
				NewObject.y4 *= (float) Scale;
				NewObject.Thickness *= (float) Scale;
				break;
			}

			NewObject.Info = (int16) (OBJECT_SELECTED + (Object->Info & 3));
			NewObject.DeleteNr = 0;

			if (AddObject(&NewObject))
			{
				Object = &((*Objects)[cnt]);
				Object->Info |= OBJECT_NOT_VISIBLE;
				Object->DeleteNr = (int16) LastActionNr;
			}

			Changed = 1;
		}
	}

	memset(&PinObject, 0, sizeof(PinObject));
	memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);
			memmove(NewObjectPolygon, ObjectPolygon, ObjectPolygonLength);
			count = ObjectPolygon->NrVertices;

			if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
				count = ObjectPolygon->NrVerticesMainPolygon;

			if ((ScaleOption & 1) == 0)
			{
				minx = ObjectPolygon->minx;
				miny = ObjectPolygon->miny;

				for (cnt2 = 0; cnt2 < count; cnt2++)
				{
					x1 = NewObjectPolygon->Points[cnt2].x;
					y1 = NewObjectPolygon->Points[cnt2].y;
					NewObjectPolygon->Points[cnt2].x = (x1 - minx) * Scale + minx;
					NewObjectPolygon->Points[cnt2].y = (y1 - miny) * Scale + miny;
				}

				if (ObjectPolygon->NrSubPolygons > 0)
				{
					Buf = (uint8 *) NewObjectPolygon;
					Buf += sizeof(ObjectPolygonInitRecord);
					Buf += count * sizeof(PointRecord);

					for (cnt2 = 0; cnt2 < NewObjectPolygon->NrSubPolygons; cnt2++)
					{
						if ((Buf - (uint8 *) NewObjectPolygon) >=
						        (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
							break;

						ObjectSubPolygon = (ObjectSubPolygonRecord *) Buf;

						if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
							break;

						count = ObjectSubPolygon->NrVertices;

						if (count >= 65536)
							break;

						for (cnt3 = 0; cnt3 < count; cnt3++)
						{
							x1 = ObjectSubPolygon->Points[cnt3].x;
							y1 = ObjectSubPolygon->Points[cnt3].y;
							ObjectSubPolygon->Points[cnt3].x = (x1 - minx) * Scale + minx;
							ObjectSubPolygon->Points[cnt3].y = (y1 - miny) * Scale + miny;
						}

						Buf += sizeof(ObjectSubPolygonInitRecord);
						Buf += count * sizeof(PointRecord);
					}
				}
			}
			else
			{
				for (cnt2 = 0; cnt2 < count; cnt2++)
				{
					NewObjectPolygon->Points[cnt2].x *= Scale;
					NewObjectPolygon->Points[cnt2].y *= Scale;
				}

				if (ObjectPolygon->NrSubPolygons > 0)
				{
					Buf = (uint8 *) ObjectPolygon;
					Buf += sizeof(ObjectPolygonInitRecord);
					Buf += count * sizeof(PointRecord);

					for (cnt2 = 0; cnt2 < ObjectPolygon->NrSubPolygons; cnt2++)
					{
						if ((Buf - (uint8 *) ObjectPolygon) >=
						        (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
							break;

						ObjectSubPolygon = (ObjectSubPolygonRecord *) Buf;

						if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
							break;

						count = ObjectSubPolygon->NrVertices;

						if (count >= 65536)
							break;

						for (cnt3 = 0; cnt3 < count; cnt3++)
						{
							ObjectSubPolygon->Points[cnt3].x *= Scale;
							ObjectSubPolygon->Points[cnt3].y *= Scale;
						}

						Buf += sizeof(ObjectSubPolygonInitRecord);
						Buf += count * sizeof(PointRecord);
					}
				}
			}

			NewObjectPolygon->DeleteNr = 0;

			if (AddObjectPolygon(NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
				PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
			}

			Changed = 1;
		}
	}

	RePaint();
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 AdjustOffsetForSnap(double CursorX, double CursorY, double GridX, double GridY, double divx, double divy,
                          double *ShiftOffsetX, double *ShiftOffsetY, int32 mode)
{
	int32 cnt, cnt2, SelectionMask1, SelectionMask2, NrFoundObjects, count;
	double x1, y1, px1, py1, px2, py2, AddX, AddY, Length1, Length2, TempLength, FoundObjectsAddX[256],
	       FoundObjectsAddY[256], Length, FoundObjectsLength[256];
	ObjectRecord *Object, NewObject;
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf1[8192];
	ObjectPolygonRecord *ObjectPolygon;

	px1 = 0.0;
	py1 = 0.0;
	px2 = 0.0;
	py2 = 0.0;

	if (mode == 0)
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

	PolygonObject = (PolygonRecord *) & PolygonBuf1;
	NrFoundObjects = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & SelectionMask1) == SelectionMask2)
		{
			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
				memmove(&NewObject, Object, sizeof(NewObject));
				NewObject.x1 += (float) divx;
				NewObject.y1 += (float) divy;
				NewObject.x2 += (float) divx;
				NewObject.y2 += (float) divy;
				NewObject.Thickness = (float) max(NewObject.Thickness, 1e5);
				MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					Length1 = CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
					Length2 = CalcLengthLine(NewObject.x2, NewObject.y2, CursorX, CursorY);

					if (Length1 < Length2)
					{
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x1 - GridX;
								AddY = NewObject.y1 - GridY;
							}
							else
							{
								AddX = NewObject.x1 - CursorX;
								AddY = NewObject.y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x1;
							AddY = GridY - NewObject.y1;
						}

						if (NrFoundObjects < 256)
						{
							FoundObjectsAddX[NrFoundObjects] = AddX;
							FoundObjectsAddY[NrFoundObjects] = AddY;
							FoundObjectsLength[NrFoundObjects] = Length1;
							NrFoundObjects++;
						}
					}
					else
					{
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x2 - GridX;
								AddY = NewObject.y2 - GridY;
							}
							else
							{
								AddX = NewObject.x2 - CursorX;
								AddY = NewObject.y2 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x2;
							AddY = GridY - NewObject.y2;
						}

						if (NrFoundObjects < 256)
						{
							FoundObjectsAddX[NrFoundObjects] = AddX;
							FoundObjectsAddY[NrFoundObjects] = AddY;
							FoundObjectsLength[NrFoundObjects] = Length2;
							NrFoundObjects++;
						}
					}

//            *ShiftOffsetX+=AddX;
//            *ShiftOffsetY+=AddY;
//            return 1;
				}

				break;

			/*

			          NewObject.ObjectType=OBJECT_CIRCLE;
			          NewObject.x1=Object->x1+divx;
			          NewObject.y1=Object->y1+divy;
			          NewObject.x2=Object->Thickness;
			          MakePolygonFromPlotObject(&NewObject,PolygonObject,0,16,32);
			          if ((PointInPolygon(PolygonObject,CurrentX,CurrentY) & 1) == 1) { // Point within object snap it
			            if (mode==0) {
			              *ShiftOffsetX+=NewObject.x1-CurrentX;
			              *ShiftOffsetY+=NewObject.y1-CurrentY;
			            } else {
			              *ShiftOffsetX-=NewObject.x1-CurrentX;
			              *ShiftOffsetY-=NewObject.y1-CurrentY;
			            }
			            return 1;
			          }
			          NewObject.x1=Object->x2+divx;
			          NewObject.y1=Object->y2+divy;
			          MakePolygonFromPlotObject(&NewObject,PolygonObject,0,16,32);
			          if ((PointInPolygon(PolygonObject,CurrentX,CurrentY) & 1) == 1) { // Point within object snap it
			            if (mode==0) {
			              *ShiftOffsetX+=NewObject.x1-CurrentX;
			              *ShiftOffsetY+=NewObject.y1-CurrentY;
			            } else {
			              *ShiftOffsetX-=NewObject.x1-CurrentX;
			              *ShiftOffsetY-=NewObject.y1-CurrentY;
			            }
			//                Beep(2000,100);
			            return 1;
			          }
			          break;
			*/
			case OBJECT_ARC:
				/*
				          if (Object->Thickness==0.0) { // Filled
				            px1=Object->x1;
				            py1=Object->y1;
				            NewObject.x2=Object->x2;
				          } else {
				*/
				NewObject.x2 = (float) max(Object->Thickness, 1e5);

				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.x1 = Object->x1 + (float) divx;
				NewObject.y1 = Object->y1 + (float) divy;
				MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					if (NrFoundObjects < 256)
					{
						FoundObjectsAddX[NrFoundObjects] = AddX;
						FoundObjectsAddY[NrFoundObjects] = AddY;
						FoundObjectsLength[NrFoundObjects] =
						    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
						NrFoundObjects++;
					}

//            *ShiftOffsetX+=AddX;
//            *ShiftOffsetY+=AddY;
//            return 1;
				}

				GetArcEndPoints(Object, &px1, &py1, &px2, &py2, 0);
				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.x1 = (float) (px1 + divx);
				NewObject.y1 = (float) (py1 + divy);
				MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					if (NrFoundObjects < 256)
					{
						FoundObjectsAddX[NrFoundObjects] = AddX;
						FoundObjectsAddY[NrFoundObjects] = AddY;
						FoundObjectsLength[NrFoundObjects] =
						    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
						NrFoundObjects++;
					}

//            *ShiftOffsetX+=AddX;
//            *ShiftOffsetY+=AddY;
//            return 1;
				}

				NewObject.x1 = (float) (px2 + divx);
				NewObject.y1 = (float) (py2 + divy);
				MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					if (NrFoundObjects < 256)
					{
						FoundObjectsAddX[NrFoundObjects] = AddX;
						FoundObjectsAddY[NrFoundObjects] = AddY;
						FoundObjectsLength[NrFoundObjects] =
						    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
						NrFoundObjects++;
					}

					//          *ShiftOffsetX+=AddX;
					//          *ShiftOffsetY+=AddY;
					//          return 1;
				}

				break;

			case OBJECT_CIRCLE:
#ifdef _DEBUG
				if ((InRange5(Object->x1, -7e5)) && (InRange5(Object->y1, -5.7e5)))
					ok = 1;

#endif

				if (((Object->Thickness == 0.0) && (Object->Info2 == 0)) || (Object->Info2 == 15))
					NewObject.x2 = Object->x2;
				else
				{
					GetArcEndPoints(Object, &px1, &py1, &px2, &py2, 0);
					NewObject.x2 = (float) max(Object->Thickness, 1e5);
				}

				NewObject.ObjectType = OBJECT_CIRCLE;
				NewObject.x1 = Object->x1 + (float) divx;
				NewObject.y1 = Object->y1 + (float) divy;
				MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					if (NrFoundObjects < 256)
					{
						FoundObjectsAddX[NrFoundObjects] = AddX;
						FoundObjectsAddY[NrFoundObjects] = AddY;
						FoundObjectsLength[NrFoundObjects] =
						    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
						NrFoundObjects++;
					}

//            *ShiftOffsetX+=AddX;
//            *ShiftOffsetY+=AddY;
				}

				if (!(((Object->Thickness == 0.0) && (Object->Info2 == 0)) || (Object->Info2 == 15)))
				{
					NewObject.x1 = (float) (px1 + divx);
					NewObject.y1 = (float) (py1 + divy);
					MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

					if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
					{	// Point within object snap it
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x1 - GridX;
								AddY = NewObject.y1 - GridY;
							}
							else
							{
								AddX = NewObject.x1 - CursorX;
								AddY = NewObject.y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x1;
							AddY = GridY - NewObject.y1;
						}

						if (NrFoundObjects < 256)
						{
							FoundObjectsAddX[NrFoundObjects] = AddX;
							FoundObjectsAddY[NrFoundObjects] = AddY;
							FoundObjectsLength[NrFoundObjects] =
							    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
							NrFoundObjects++;
						}

//              *ShiftOffsetX+=AddX;
//              *ShiftOffsetY+=AddY;
						return 1;
					}

					NewObject.x1 = (float) (px2 + divx);
					NewObject.y1 = (float) (py2 + divy);
					MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

					if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
					{	// Point within object snap it
						if ((mode & 2) == 0)
						{
							if ((mode & 1) == 0)
							{
								AddX = NewObject.x1 - GridX;
								AddY = NewObject.y1 - GridY;
							}
							else
							{
								AddX = NewObject.x1 - CursorX;
								AddY = NewObject.y1 - CursorY;
							}
						}
						else
						{
							AddX = GridX - NewObject.x1;
							AddY = GridY - NewObject.y1;
						}

						if (NrFoundObjects < 256)
						{
							FoundObjectsAddX[NrFoundObjects] = AddX;
							FoundObjectsAddY[NrFoundObjects] = AddY;
							FoundObjectsLength[NrFoundObjects] =
							    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
							NrFoundObjects++;
						}

//              *ShiftOffsetX+=AddX;
//              *ShiftOffsetY+=AddY;
//              return 1;
					}
				}

				break;

			case OBJECT_RECT:
				memmove(&NewObject, Object, sizeof(ObjectRecord));
				NewObject.x1 += (float) divx;
				NewObject.y1 += (float) divy;
				MakePolygonFromPlotObject(&NewObject, PolygonObject, 0, 16, 32);

				if ((PointInPolygon(PolygonObject, CursorX, CursorY) & 1) == 1)
				{	// Point within object snap it
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					if (NrFoundObjects < 256)
					{
						FoundObjectsAddX[NrFoundObjects] = AddX;
						FoundObjectsAddY[NrFoundObjects] = AddY;
						FoundObjectsLength[NrFoundObjects] =
						    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
						NrFoundObjects++;
					}

//            *ShiftOffsetX+=AddX;
//            *ShiftOffsetY+=AddY;
//            return 1;
				}

				break;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & SelectionMask1) == SelectionMask2)
		{
			count = ObjectPolygon->NrVertices;

			if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
				count = ObjectPolygon->NrVerticesMainPolygon;

			for (cnt2 = 0; cnt2 < count; cnt2++)
			{
				x1 = ObjectPolygon->Points[cnt2].x;
				y1 = ObjectPolygon->Points[cnt2].y;
				NewObject.x1 = (float) (x1 + divx);
				NewObject.y1 = (float) (y1 + divy);

				if (CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY) < 0.25e5)
				{
					if ((mode & 2) == 0)
					{
						if ((mode & 1) == 0)
						{
							AddX = NewObject.x1 - GridX;
							AddY = NewObject.y1 - GridY;
						}
						else
						{
							AddX = NewObject.x1 - CursorX;
							AddY = NewObject.y1 - CursorY;
						}
					}
					else
					{
						AddX = GridX - NewObject.x1;
						AddY = GridY - NewObject.y1;
					}

					if (NrFoundObjects < 256)
					{
						FoundObjectsAddX[NrFoundObjects] = AddX;
						FoundObjectsAddY[NrFoundObjects] = AddY;
						FoundObjectsLength[NrFoundObjects] =
						    CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
						NrFoundObjects++;
					}

//          *ShiftOffsetX+=AddX;
//          *ShiftOffsetY+=AddY;
//          return 1;
				}
			}

			x1 = ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
			y1 = ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
			NewObject.x1 = (float) (x1 + divx);
			NewObject.y1 = (float) (y1 + divy);
			TempLength = CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);

			if (TempLength < 1e5)
			{
				if ((mode & 2) == 0)
				{
					if ((mode & 1) == 0)
					{
						AddX = NewObject.x1 - GridX;
						AddY = NewObject.y1 - GridY;
					}
					else
					{
						AddX = NewObject.x1 - CursorX;
						AddY = NewObject.y1 - CursorY;
					}
				}
				else
				{
					AddX = GridX - NewObject.x1;
					AddY = GridY - NewObject.y1;
				}

				if (NrFoundObjects < 256)
				{
					FoundObjectsAddX[NrFoundObjects] = AddX;
					FoundObjectsAddY[NrFoundObjects] = AddY;
					FoundObjectsLength[NrFoundObjects] = CalcLengthLine(NewObject.x1, NewObject.y1, CursorX, CursorY);
					NrFoundObjects++;
				}

//        *ShiftOffsetX+=AddX;
//        *ShiftOffsetY+=AddY;
//        return 1;
			}
		}
	}

	cnt2 = -1;
	Length = 10000.0e5;

	for (cnt = 0; cnt < NrFoundObjects; cnt++)
	{
		if (FoundObjectsLength[cnt] < Length)
		{
			Length = FoundObjectsLength[cnt];
			cnt2 = cnt;
		}
	}

	if (cnt2 != -1)
	{
		*ShiftOffsetX += FoundObjectsAddX[cnt2];
		*ShiftOffsetY += FoundObjectsAddY[cnt2];
		return 1;
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetLinePointNr(double x, double y)
{
	int32 cnt;

	cnt = 0;

	while (cnt < NrLinePoints)
	{
		if ((InRange4(x, (*LinePoints)[cnt].x)) && (InRange4(y, (*LinePoints)[cnt].y)))
			return cnt;

		cnt++;
	}

	return -1;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void InsertLineObjectPoint(double x1, double y1)
{
	LinePointRecord *LinePoint;

	LinePoint = &((*LinePoints)[NrLinePoints]);
	memset(LinePoint, 0, sizeof(LinePointRecord));
	LinePoint->x = x1;
	LinePoint->y = y1;
	NrLinePoints++;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetLineObjectNrFromEndPoint(double x, double y, double *NewX, double *NewY, int32 mode)
{
	int32 cnt, Mask;
	double x1, y1, x2, y2;
	ObjectRecord *Object;

	if (mode == 0)
		Mask = 1;
	else
		Mask = 2;

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		if ((Object->Info & Mask) == 0)
		{
			if ((InRange3(x, x1)) && (InRange3(y, y1)))
			{
				*NewX = x2;
				*NewY = y2;
				return cnt;
			}

			if ((InRange3(x, x2)) && (InRange3(y, y2)))
			{
				*NewX = x1;
				*NewY = y1;
				return cnt;
			}
		}
	}

	return -1;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetPolygonFromLines(int32 mode)
{

	int32 cnt, cnt2, NrLinePointsNumbers, NrVertices, Layer, PinNr, Found, Found2, PolygonError;

	ObjectRecord *Object, *Object2, PinObject;
	double MaxPolygonLength;
	LinePointRecord *LinePoint;
	double x1, y1, x2, y2;

	memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));
	Object = NULL;
	Object2 = NULL;
	NrObjects2 = 0;
	Layer = -1;
	PinNr = -1;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (Object->ObjectType == OBJECT_LINE)
			{
				if (Layer == -1)
					Layer = Object->Layer;

				if ((Object->PinNr != -1) && (PinNr == -1))
					PinNr = Object->PinNr;
			}
		}
	}

	switch (Layer)
	{
	case PLACEMENT_OUTLINE_LAYER:
	case COMP_OUTLINE_LAYER:
	case GEOM_NAME_LAYER:
	case BOARD_OUTLINE_LAYER:
		MessageBoxUTF8(GEOMWindow, SC(403, "Can not create a polygon on this layer"), SC(48, "Error"),
		               MB_APPLMODAL | MB_OK);
		return -1;
		break;
	}

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->ObjectType == OBJECT_LINE) && (Object->Layer == Layer))
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;

			if (NrObjects2 >= MaxNrObjects2 - 2)
				AllocateMemObjects2(MaxNrObjects2 + 1024);

			Object2 = &((*Objects2)[NrObjects2]);
			Object2->x1 = (float) x1;
			Object2->y1 = (float) y1;
			Object2->x2 = (float) x2;
			Object2->y2 = (float) y2;
			Object2->x3 = Object->Thickness;
			Object2->Info = 0;
			NrObjects2++;
		}
	}


// ************************************************************************************************
// ************************************************************************************************

	if (NrObjects2 == 0)
		return -2;

	PolygonError = 0;
	LinePoints = (LinePointsArray *) & Buf;
	LinePointsNumbers = (LinePointsNumbersArray *) & Buf[128 * 1024];
	memset(LinePoints, 0, sizeof(LinePointsArray));
//  LinePointsNumbers=
	NrLinePoints = 0;
	NrLinePointsNumbers = 0;
	MaxPolygonLength = 0.0;

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object2 = &((*Objects2)[cnt]);

		if ((Object2->Info & 1) == 0)
		{
			x1 = Object2->x1;
			y1 = Object2->y1;
			x2 = Object2->x2;
			y2 = Object2->y2;

			if ((cnt2 = GetLinePointNr(x1, y1)) == -1)
			{
				InsertLineObjectPoint(x1, y1);
				LinePoint = &((*LinePoints)[NrLinePoints - 1]);
			}
			else
				LinePoint = &((*LinePoints)[cnt2]);

			LinePoint->NrLines++;

			if ((cnt2 = GetLinePointNr(x2, y2)) == -1)
			{
				InsertLineObjectPoint(x2, y2);
				LinePoint = &((*LinePoints)[NrLinePoints - 1]);
			}
			else
				LinePoint = &((*LinePoints)[cnt2]);

			LinePoint->NrLines++;
		}
	}

	for (cnt = 0; cnt < NrLinePoints; cnt++)
	{
		LinePoint = &((*LinePoints)[cnt]);

		if (LinePoint->NrLines != 2)
		{
			ok = 1;
			MessageBoxUTF8(GEOMWindow, SC(405, "Did not found a closed polygon"), SC(48, "Error"),
			               MB_APPLMODAL | MB_OK);
			return -1;
		}
	}


	Found = 0;
	cnt = 0;

	while ((!Found) && (cnt < NrObjects2))
	{
		Object2 = &((*Objects2)[cnt]);

		if ((Object2->Info & 1) == 0)
			Found = 1;
		else
			cnt++;
	}

	if (Found)
	{
		x1 = Object2->x1;
		y1 = Object2->y1;
		NrVertices = 0;
		NewObjectPolygon->NrVertices = 0;
//      Polygon->Points[NrVertices].x=x1;
//      Polygon->Points[NrVertices].y=y1;
//      NrVertices++;
		x2 = Object2->x2;
		y2 = Object2->y2;
		NewObjectPolygon->Points[NrVertices].x = x2;
		NewObjectPolygon->Points[NrVertices].y = y2;
		NrVertices++;
		Object2->Info |= 1;

		while ((NotInRange4(x1, x2)) || (NotInRange4(y1, y2)))
		{
			cnt2 = 0;
			Found2 = 0;

			while ((cnt2 < NrObjects2) && (!Found2))
			{
				Object2 = &((*Objects2)[cnt2]);

				if ((Object2->Info & 1) == 0)
				{
					if ((InRange4(x2, Object2->x1)) && (InRange4(y2, Object2->y1)))
					{
						Found2 = 1;
						x2 = Object2->x2;
						y2 = Object2->y2;

						if (NrVertices < 512)
						{
							NewObjectPolygon->Points[NrVertices].x = x2;
							NewObjectPolygon->Points[NrVertices].y = y2;
							NrVertices++;
						}

						Object2->Info |= 1;
					}
					else
					{
						if ((InRange4(x2, Object2->x2)) && (InRange4(y2, Object2->y2)))
						{
							Found2 = 1;
							x2 = Object2->x1;
							y2 = Object2->y1;

							if (NrVertices < 512)
							{
								NewObjectPolygon->Points[NrVertices].x = x2;
								NewObjectPolygon->Points[NrVertices].y = y2;
								NrVertices++;
							}

							Object2->Info |= 1;
						}
					}

					if (!Found2)
						cnt2++;
				}
				else
					cnt2++;
			}
		}

		NewObjectPolygon->NrVertices = NrVertices;
		NewObjectPolygon->Layer = Layer;
		NewObjectPolygon->PinNr = PinNr;
	}

	if (!CheckNoCrossesInObjectPolygon(NewObjectPolygon))
		MessageBoxUTF8(GEOMWindow, SC(406, "Polygon contains line crossings"), SC(48, "Error"), MB_APPLMODAL | MB_OK);

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Object->ObjectType == OBJECT_LINE) && (Object->Layer == Layer))
		{
			ZeroUnusedObjects(0);
			Object->Info &= ~OBJECT_SELECTED;
			Object->Info |= OBJECT_NOT_VISIBLE;
			Object->DeleteNr = (int16) LastActionNr;
		}
	}

	if (AddObjectPolygon(NewObjectPolygon))
	{
		PinObject.x1 = (float) ((NewObjectPolygon->minx + NewObjectPolygon->maxx) * 0.5);
		PinObject.y1 = (float) ((NewObjectPolygon->miny + NewObjectPolygon->maxy) * 0.5);
		PinObject.Layer = NewObjectPolygon->Layer;
		PinObject.PinNr = NewObjectPolygon->PinNr;
	}

	RePaint();
	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 UnselectFirstObject(int32 mode)
{
	ObjectRecord *Object;
	int32 cnt;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Object->Info &= ~OBJECT_SELECTED;
			StartDrawingEditingWindow();
			DrawObject(Object, 0.0, 0.0, 0);
			ExitDrawing();
			EndDrawingEditingWindow();
			return 1;
		}
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
