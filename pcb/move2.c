/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: move2.c
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
#include "math.h"
#include "memory.h"
#include "string.h"
#include "calc.h"
#include "menus.h"
#include "pcb.h"
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
#include "move2.h"
#include "calc4.h"
#include "resource.h"
#include "dialogs.h"
#include "help.h"
#include "stdio.h"
#include "files.h"
#include "files2.h"
#include "polygon.h"
#include "movecomp.h"

double CentreSelectedX, CentreSelectedY;

double SelectedMinX, SelectedMinY, SelectedMaxX, SelectedMaxY;

int32 AreafillNrToMove, ok;

extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern HDC OutputDisplay;
extern ProjectInfoRecord *ProjectInfo;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceMovedSpecialObjects(double CurrentX, double CurrentY, int32 Mode);

int32 GetMinMaxSelectedSpecialObjects(void);

void DrawSelectedSpecialObjects(double CurrentX, double CurrentY, int32 Mode);

void PlaceMovedRefs(double CurrentX, double CurrentY, double Rotation);

void PlaceMovedCompValues(double CurrentX, double CurrentY, double Rotation);

void GetMinMaxSelectedComponents(void);

void DrawSelectedRefs(double CurrentX, double CurrentY, double Rotation);

void DrawSelectedCompValues(double CurrentX, double CurrentY, double Rotation);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveSelectedSpecialObjects(int32 Mode, int32 Count)
{
	int32 cnt, Layer, NrLinesSelected, NrRectanglesSelected, NrCirclesSelected, NrArcsSelected, NrTextsSelected,
	      NewMode;
	double OldX, OldY, CurrentX, CurrentY, divx, divy, ShiftX, ShiftY, x1, y1, divx2, divy2, CursorX, CursorY;
	ObjectLineRecord *ObjectLine;
	ObjectTextRecord2 TypeObject;
	int32 CompPlaced, FirstShift;
	HMENU PopUpMenu = NULL;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;


	NrLinesSelected = GetNrObjectSelections(0, 0);
	NrRectanglesSelected = GetNrObjectSelections(0, 1);
	NrCirclesSelected = GetNrObjectSelections(0, 2);
	NrArcsSelected = GetNrObjectSelections(0, 3);
	NrTextsSelected = GetNrObjectSelections(0, 4);

	SelectionEsc = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Layer = ObjectLine->Layer;

			if ((Layer == SOLD_MASK_BOTTOM) || (Layer == SOLD_MASK_TOP) || (Layer == PASTE_MASK_BOTTOM)
			        || (Layer == PASTE_MASK_TOP))
				ObjectLine->Info |= 3;
		}
	}

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	GetMinMaxSelectedSpecialObjects();
	ShiftOffsetX = 0.0;
	ShiftOffsetY = 0.0;

	if ((NrRectanglesSelected != 0) || (NrCirclesSelected != 0) || (NrArcsSelected != 0) || (NrTextsSelected != 0))
	{
		ShiftOffsetX = -(CurrentX - SelectedMinX);
		ShiftOffsetY = -(CurrentY - SelectedMinY);
//    ShiftOffsetX=SelectedMinX;
//    ShiftOffsetY=SelectedMinY;
	}

	OldX = CurrentX;
	OldY = CurrentY;

	ClipMouseCursor();
	DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
	FirstShift = 1;
	CompPlaced = 0;
	SystemBusyMode = 3;
	NewMode = 1;
	DrawXorFunction.Function1 = (FUNCP1) DrawSelectedSpecialObjects;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &NewMode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &NewMode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawSelectedSpecialObjects(OldX, OldY, 1);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawSelectedSpecialObjects(OldX, OldY, 1);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawSelectedSpecialObjects(OldX, OldY, 1);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawSelectedSpecialObjects(OldX, OldY, 1);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawSelectedSpecialObjects(OldX, OldY, 1);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{
				DrawSelectedSpecialObjects(ShiftX, ShiftY, 1);
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
					                    4);
				}

				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
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
			DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			ZoomInOutProcessed = 0;
		}

//    if (CheckRightButton(&DrawXorFunction)==1) {
		if (!Focused)
		{
			DrawSelectedSpecialObjects(OldX, OldY, 1);
			CompPlaced = 1;
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawSelectedSpecialObjects(OldX, OldY, 1);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			else
				CompPlaced = 1;
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawSelectedSpecialObjects(OldX, OldY, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
			else
				CompPlaced = 1;
		}

		if (CheckLeftButton())
		{
			CompPlaced = 1;
			DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);

			for (cnt = 0; cnt < Count; cnt++)
			{
				if ((Count == 1) && ((SnapMode & 1) == 1))
				{
					CursorX = PixelToRealOffX(MousePosX);
					CursorY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
					AdjustOffsetForSnap(CursorX, CursorY, CurrentX, CurrentY, 0.0, 0.0, &ShiftOffsetX, &ShiftOffsetY,
					                    7);
				}

				divx2 = CurrentX - ShiftOffsetX;
				divy2 = CurrentY - ShiftOffsetY;
				divx = CurrentX - CurrentX2 - ShiftOffsetX;
				divy = CurrentY - CurrentY2 - ShiftOffsetY;
				PlaceMovedSpecialObjects(divx, divy, (int32) Mode);
				CurrentX += divx2;
				CurrentY += divy2;
			}

			CheckInputMessages(0);
			SelectionEsc = 1;
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			PopUpMenu = CreatePopupMenu();

			if ((SnapMode & 1) == 0)
				AppendMenuOwn(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_SNAP_MODE_ON, SC(746, "Snap on"));
			else
				AppendMenuOwn(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_SNAP_MODE_OFF, SC(802, "Snap off"));

			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(493, "Escape"));
			TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40,
			               0, PCBWindow, NULL);
			DestroyMenu(PopUpMenu);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawSelectedSpecialObjects(OldX, OldY, 1);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectTextRecord2));

				if ((((Mode & 2) == 0) && (LineInputDialog(&TypeObject, SC(494, "Move objects to"), 0) == 1))
				        || (((Mode & 2) == 2) && (LineInputDialog(&TypeObject, SC(596, "Copy objects to"), 0) == 1)))
				{
					if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 0)
					{
						if (NrParams == 2)
						{
							x1 = ParamsFloat[0];
							y1 = ParamsFloat[1];

							if (!ParametersRelative)
							{
//                PlaceMovedSpecialObjects(x1+ShiftOffsetX,y1+ShiftOffsetY,(int32)Mode);
								divx = CurrentX2 + ShiftOffsetX;
								divy = CurrentY2 + ShiftOffsetY;
								PlaceMovedSpecialObjects(x1 - divx, y1 - divy, (int32) Mode);
//  x1   -ShiftOffsetX
							}
							else
							{
								PlaceMovedSpecialObjects(x1, y1, (int32) Mode);
// x1+ShiftOffsetX  -ShiftOffsetX
							}

							SelectionEsc = 1;
							CompPlaced = 1;
						}
					}
				}

				SpacePressed = 0;
			}

			if (HelpAsked)
			{
				Help("special_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			FirstShift = 1;

			if (SelectionEsc)
				CompPlaced = 1;
			else
				DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);
		}
	}

	DrawCrossHair(2);

	if (!CompPlaced)
		DrawSelectedSpecialObjects(CurrentX, CurrentY, 1);

	UnClipMouseCursor();
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawSelectedSpecialObjects(double CurrentX, double CurrentY, int32 Mode)
{
	int32 cnt;
	double x1, y1;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectTextRecord2 *ObjectText2;

	StartDrawingEditingWindow();
	CurrentObjectCode = -1;

	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			DrawObjectLine(ObjectLine, CurrentX - CurrentX2 - ShiftOffsetX, CurrentY - CurrentY2 - ShiftOffsetY, 1);
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			DrawObjectRect(ObjectRect, CurrentX - CurrentX2 - ShiftOffsetX, CurrentY - CurrentY2 - ShiftOffsetY, 1);
			InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
			x1 = ObjectRect->CentreX + CurrentX - CurrentX2 - ShiftOffsetX;
			y1 = ObjectRect->CentreY + CurrentY - CurrentY2 - ShiftOffsetY;
			DrawLine(MultX(x1) - 3, MultY(y1) - 3, MultX(x1) + 3, MultY(y1) + 3);
			DrawLine(MultX(x1) - 3, MultY(y1) + 3, MultX(x1) + 3, MultY(y1) - 3);
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      DrawObjectCircle(ObjectCircle,CurrentX-ShiftOffsetX,CurrentY-ShiftOffsetY,1);
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			DrawObjectArc(ObjectArc, CurrentX - CurrentX2 - ShiftOffsetX, CurrentY - CurrentY2 - ShiftOffsetY, 1);
			InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
			x1 = ObjectArc->CentreX + CurrentX - CurrentX2 - ShiftOffsetX;
			y1 = ObjectArc->CentreY + CurrentY - CurrentY2 - ShiftOffsetY;
			DrawLine(MultX(x1) - 3, MultY(y1) - 3, MultX(x1) + 3, MultY(y1) + 3);
			DrawLine(MultX(x1) - 3, MultY(y1) + 3, MultX(x1) + 3, MultY(y1) - 3);
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if ((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      DrawObjectText(ObjectText,CurrentX-ShiftOffsetX,CurrentY-ShiftOffsetY,1);
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			DrawObjectPolygon(ObjectPolygon, CurrentX - CurrentX2 - ShiftOffsetX, CurrentY - CurrentY2 - ShiftOffsetY,
			                  1);
			InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
			x1 = (ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5 + CurrentX - CurrentX2 - ShiftOffsetX;
			y1 = (ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5 + CurrentY - CurrentY2 - ShiftOffsetY;
			DrawLine(MultX(x1) - 3, MultY(y1) - 3, MultX(x1) + 3, MultY(y1) + 3);
			DrawLine(MultX(x1) - 3, MultY(y1) + 3, MultX(x1) + 3, MultY(y1) - 3);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			DrawObjectText2(ObjectText2, CurrentX - CurrentX2 - ShiftOffsetX, CurrentY - CurrentY2 - ShiftOffsetY, 0.0,
			                1);
		}
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetMinMaxSelectedSpecialObjects()
{
	int32 cnt;

	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;

	SelectedMinX = 1000000000.0;
	SelectedMinY = 1000000000.0;
	SelectedMaxX = -1000000000.0;
	SelectedMaxY = -1000000000.0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = ObjectLine->X1;
			SelectedMinY = ObjectLine->Y1;
			return 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = ObjectRect->CentreX;
			SelectedMinY = ObjectRect->CentreY;
			return 1;
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED) {
	      SelectedMinX=ObjectCircle->CentreX;
	      SelectedMinY=ObjectCircle->CentreY;
	      return 1;
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = ObjectArc->CentreX;
			SelectedMinY = ObjectArc->CentreY;
			return 1;
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			SelectedMinX = ObjectText2->X;
			SelectedMinY = ObjectText2->Y;
			return 1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceMovedSpecialObjects(double divx, double divy, int32 Mode)
{
	int32 cnt, TempLastActionNr, PolygonSize;
	int32 Changed;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectTextRecord2 *ObjectText2;

	ObjectRecord NewObject;

	memset(&NewObject, 0, sizeof(NewObject));

	DrawCrossHair(2);
	TempLastActionNr = (int16) LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			ObjectLine->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));

			if ((ObjectLine->Info & 1) == 1)
			{
				NewObjectLine.X1 += (float) divx;
				NewObjectLine.Y1 += (float) divy;
			}

			if ((ObjectLine->Info & 2) == 2)
			{
				NewObjectLine.X2 += (float) divx;
				NewObjectLine.Y2 += (float) divy;
			}

			NewObjectLine.Info = 0;

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);

				if (Mode < 2)
				{
				}
				else
					ObjectLine->Info |= OBJECT_SELECTED;

				if (Mode < 2)
				{
					ObjectLine->Info |= OBJECT_NOT_VISIBLE;
					ObjectLine->DeleteNr = (int16) LastActionNr;
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
			ObjectRect->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			NewObjectRect.CentreX += (float) divx;
			NewObjectRect.CentreY += (float) divy;
			NewObjectRect.Info &= OBJECT_FILLED;

			if (AddObjectRect(&NewObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);

				if (Mode < 2)
				{
				}
				else
					ObjectRect->Info |= OBJECT_SELECTED;

				if (Mode < 2)
				{
					ObjectRect->Info |= OBJECT_NOT_VISIBLE;
					ObjectRect->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectCircle->AddNr<=TempLastActionNr)) {
	      ObjectCircle->Info&=~OBJECT_SELECTED;
	      memmove(&NewObjectCircle,ObjectCircle,sizeof(ObjectCircleRecord));
	      NewObjectCircle.CentreX+=divx;
	      NewObjectCircle.CentreY+=divy;
	      NewObjectCircle.Info&=OBJECT_FILLED;
	      if (AddObjectCircle(&NewObjectCircle)) {
	        ObjectCircle=&((*ObjectCircles)[cnt]);
	        if (Mode<2) {
	          SetBackGroundActive(0);
	          DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	        } else {
	          ObjectCircle->Info|=OBJECT_SELECTED;
	        }
	        DrawObjectCircle(&NewObjectCircle,0.0,0.0,0);
	        if (Mode<2) {
	          ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	          ObjectCircle->DeleteNr=(int16)LastActionNr;
	        }
	        Changed=1;
	        if ((NewObjectCircle.Layer==DRILL_UNPLATED_LAYER)
	           ||
	           (NewObjectCircle.Layer==DRILL_LAYER)) {
	          if (NewObjectCircle.Layer==DRILL_UNPLATED_LAYER) {
	            NewObject.ObjectType=DRILL_UNPLATED;
	          } else {
	            NewObject.ObjectType=DRILL;
	          }
	          NewObject.x1=NewObjectCircle.CentreX;
	          NewObject.y1=NewObjectCircle.CentreY;
	          NewObject.x2=NewObjectCircle.Diam;
	          NewObject.Layer=-1;
	          if (InsertObjectInAreaFill(&NewObject,-1,-1,2)==1) {
	            if (OkToDrawAreaFills) {
	              RePaint();
	            }
	          }
	        }
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			ObjectArc->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			NewObjectArc.CentreX += (float) divx;
			NewObjectArc.CentreY += (float) divy;
			NewObjectArc.Info &= OBJECT_FILLED;

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if (Mode < 2)
				{
				}
				else
					ObjectArc->Info |= OBJECT_SELECTED;

				if (Mode < 2)
				{
					ObjectArc->Info |= OBJECT_NOT_VISIBLE;
					ObjectArc->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectText->AddNr<=TempLastActionNr)) {
	      ObjectText->Info&=~OBJECT_SELECTED;
	      memmove(&NewObjectText,ObjectText,sizeof(ObjectTextRecord));
	      NewObjectText.X+=divx;
	      NewObjectText.Y+=divy;
	      NewObjectText.Info=0;
	      if (AddObjectText(&NewObjectText)) {
	        ObjectText=&((*ObjectTexts)[cnt]);
	        if (Mode<2) {
	          SetBackGroundActive(0);
	          DrawObjectText(ObjectText,0.0,0.0,0);
	        } else {
	          ObjectText->Info|=OBJECT_SELECTED;
	        }
	        DrawObjectText(&NewObjectText,0.0,0.0,0);
	        if (Mode<2) {
	          ObjectText->Info|=OBJECT_NOT_VISIBLE;
	          ObjectText->DeleteNr=(int16)LastActionNr;
	        }
	        Changed=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->AddNr <= TempLastActionNr))
		{
			ObjectText2->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			NewObjectText2.X += (float) divx;
			NewObjectText2.Y += (float) divy;
			NewObjectText2.Info = 0;

			if (AddObjectText2(&NewObjectText2))
			{
				ObjectText2 = &((*ObjectTexts2)[cnt]);

				if (Mode < 2)
				{
				}
				else
					ObjectText2->Info |= OBJECT_SELECTED;

				if (Mode < 2)
				{
					ObjectText2->Info |= OBJECT_NOT_VISIBLE;
					ObjectText2->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			PolygonSize = MemSizeObjectPolygon(ObjectPolygon);
			memmove(&NewObjectPolygon, ObjectPolygon, PolygonSize);
			MoveObjectPolygon(&NewObjectPolygon, divx, divy, 0);

			if (AddObjectPolygon(&NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

				if (Mode < 2)
				{
				}
				else
					ObjectPolygon->Info |= OBJECT_SELECTED;

				if (Mode < 2)
				{
					ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
					ObjectPolygon->DeleteNr = (int16) LastActionNr;
				}

				Changed = 1;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceRotatedSpecialObjects(int32 Mode)
{
	int32 cnt, TempLastActionNr, PolygonSize, res;
	double hx, CentreX, CentreY, Rotation;
	int32 Changed;
	ObjectLineRecord *ObjectLine, NewObjectLine, NewObjectLine2, NewObjectLine3, NewObjectLine4;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	ObjectRecord NewObject;
#ifdef _DEBUG
	int32 ok;
#endif

	res = RotationDialog(&CentreX, &CentreY, &Rotation, 0);

	switch (res)
	{
	case 1:
		CentreX = AdjustToDrawGrid((SearchMinX + SearchMaxX) / 2);
		CentreY = AdjustToDrawGrid((SearchMinY + SearchMaxY) / 2);
		break;

	case 2:
		CentreX = (SearchMinX + SearchMaxX) / 2;
		CentreY = (SearchMinY + SearchMaxY) / 2;
		break;

	case 3:
		if (CommandSelectPoint(&CentreX, &CentreY, 0.0, 0.0, 0.0, 0.0, 0) == 0)
			return;

		break;

	case 4:
		break;

	default:
		return;
	}

	memset(&NewObject, 0, sizeof(NewObject));

	TempLastActionNr = (int16) LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			ObjectLine->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));

			if (Mode == 1)
				Rotation = 90.0;

			RotatePointFromOtherPoint(&NewObjectLine.X1, &NewObjectLine.Y1, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint(&NewObjectLine.X2, &NewObjectLine.Y2, CentreX, CentreY, Rotation);

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
			ObjectRect->Info &= ~OBJECT_SELECTED;
#ifdef _DEBUG

			if ((InRange9(ObjectRect->CentreX, -38.4e5)) && (InRange9(ObjectRect->CentreY, 287.1e5)))
				ok = 1;

#endif

			if (Mode == 1)
			{
				memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
				RotateFlipPoint(&NewObjectRect.CentreX, &NewObjectRect.CentreY, CentreX, CentreY, 1);
				hx = NewObjectRect.Width;
				NewObjectRect.Width = NewObjectRect.Height;
				NewObjectRect.Height = (float) hx;
				NewObjectRect.Info &= OBJECT_FILLED;

				if (AddObjectRect(&NewObjectRect))
				{
					ObjectRect = &((*ObjectRects)[cnt]);
					ObjectRect->Info |= OBJECT_NOT_VISIBLE;
					ObjectRect->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
			else
			{
				if ((ObjectRect->Info & OBJECT_FILLED) == 0)
				{
					memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
					NewObjectLine.Layer = ObjectRect->Layer;
					NewObjectLine.LineThickNess = ObjectRect->LineThickNess;
					memmove(&NewObjectLine2, &NewObjectLine, sizeof(ObjectLineRecord));
					memmove(&NewObjectLine3, &NewObjectLine, sizeof(ObjectLineRecord));
					memmove(&NewObjectLine4, &NewObjectLine, sizeof(ObjectLineRecord));
					NewObjectLine.X1 = ObjectRect->CentreX - ObjectRect->Width * (float) 0.5;
					NewObjectLine.Y1 = ObjectRect->CentreY - ObjectRect->Height * (float) 0.5;
					NewObjectLine.X2 = ObjectRect->CentreX - ObjectRect->Width * (float) 0.5;
					NewObjectLine.Y2 = ObjectRect->CentreY + ObjectRect->Height * (float) 0.5;

					NewObjectLine2.X1 = ObjectRect->CentreX - ObjectRect->Width * (float) 0.5;
					NewObjectLine2.Y1 = ObjectRect->CentreY + ObjectRect->Height * (float) 0.5;
					NewObjectLine2.X2 = ObjectRect->CentreX + ObjectRect->Width * (float) 0.5;
					NewObjectLine2.Y2 = ObjectRect->CentreY + ObjectRect->Height * (float) 0.5;

					NewObjectLine3.X1 = ObjectRect->CentreX + ObjectRect->Width * (float) 0.5;
					NewObjectLine3.Y1 = ObjectRect->CentreY + ObjectRect->Height * (float) 0.5;
					NewObjectLine3.X2 = ObjectRect->CentreX + ObjectRect->Width * (float) 0.5;
					NewObjectLine3.Y2 = ObjectRect->CentreY - ObjectRect->Height * (float) 0.5;

					NewObjectLine4.X1 = ObjectRect->CentreX + ObjectRect->Width * (float) 0.5;
					NewObjectLine4.Y1 = ObjectRect->CentreY - ObjectRect->Height * (float) 0.5;
					NewObjectLine4.X2 = ObjectRect->CentreX - ObjectRect->Width * (float) 0.5;
					NewObjectLine4.Y2 = ObjectRect->CentreY - ObjectRect->Height * (float) 0.5;
					RotatePointFromOtherPoint(&NewObjectLine.X1, &NewObjectLine.Y1, CentreX, CentreY, Rotation);
					RotatePointFromOtherPoint(&NewObjectLine.X2, &NewObjectLine.Y2, CentreX, CentreY, Rotation);
					RotatePointFromOtherPoint(&NewObjectLine2.X1, &NewObjectLine2.Y1, CentreX, CentreY, Rotation);
					RotatePointFromOtherPoint(&NewObjectLine2.X2, &NewObjectLine2.Y2, CentreX, CentreY, Rotation);
					RotatePointFromOtherPoint(&NewObjectLine3.X1, &NewObjectLine3.Y1, CentreX, CentreY, Rotation);
					RotatePointFromOtherPoint(&NewObjectLine3.X2, &NewObjectLine3.Y2, CentreX, CentreY, Rotation);
					RotatePointFromOtherPoint(&NewObjectLine4.X1, &NewObjectLine4.Y1, CentreX, CentreY, Rotation);
					RotatePointFromOtherPoint(&NewObjectLine4.X2, &NewObjectLine4.Y2, CentreX, CentreY, Rotation);

					if ((AddObjectLine(&NewObjectLine)) && (AddObjectLine(&NewObjectLine2))
					        && (AddObjectLine(&NewObjectLine3)) && (AddObjectLine(&NewObjectLine4)))
					{
						ObjectRect = &((*ObjectRects)[cnt]);
						ObjectRect->Info |= OBJECT_NOT_VISIBLE;
						ObjectRect->DeleteNr = (int16) LastActionNr;
						Changed = 1;
					}
				}
				else
				{
					memset(&NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));
					NewObjectPolygon.NrVertices = 4;
					NewObjectPolygon.Layer = ObjectRect->Layer;
					NewObjectPolygon.Points[0].x = ObjectRect->CentreX - ObjectRect->Width * 0.5;
					NewObjectPolygon.Points[0].y = ObjectRect->CentreY - ObjectRect->Height * 0.5;
					NewObjectPolygon.Points[1].x = ObjectRect->CentreX - ObjectRect->Width * 0.5;
					NewObjectPolygon.Points[1].y = ObjectRect->CentreY + ObjectRect->Height * 0.5;
					NewObjectPolygon.Points[2].x = ObjectRect->CentreX + ObjectRect->Width * 0.5;
					NewObjectPolygon.Points[2].y = ObjectRect->CentreY + ObjectRect->Height * 0.5;
					NewObjectPolygon.Points[3].x = ObjectRect->CentreX + ObjectRect->Width * 0.5;
					NewObjectPolygon.Points[3].y = ObjectRect->CentreY - ObjectRect->Height * 0.5;

					RotateObjectPolygon(&NewObjectPolygon, CentreX, CentreY, Rotation, 0);
					SetMinMaxObjectPolygon(&NewObjectPolygon, 0);

					if (AddObjectPolygon(&NewObjectPolygon))
					{
						ObjectRect = &((*ObjectRects)[cnt]);
						ObjectRect->Info |= OBJECT_NOT_VISIBLE;
						ObjectRect->DeleteNr = (int16) LastActionNr;
						Changed = 1;
					}
				}
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectCircle->AddNr<=TempLastActionNr)) {
	      ObjectCircle->Info&=~OBJECT_SELECTED;
	      if ((Mode==1)
	         ||
	         (ObjectCircle->CircleMode == 15)
	         ||
	         (((ObjectCircle->Info & OBJECT_FILLED) != 0)
	         &&
	         ((NewObjectCircle.Layer==DRILL_UNPLATED_LAYER)
	         ||
	         (NewObjectCircle.Layer==DRILL_LAYER)))) {
	        memmove(&NewObjectCircle,ObjectCircle,sizeof(ObjectCircleRecord));
	        if (Mode==1) {
	          RotateFlipPoint(&NewObjectCircle.CentreX,&NewObjectCircle.CentreY,CentreX,CentreY,1);
	          NewObjectCircle.CircleMode=CircleRotate90[ObjectCircle->CircleMode];
	        } else {
	          RotatePointFromOtherPoint2(&NewObjectCircle.CentreX,&NewObjectCircle.CentreY,CentreX,CentreY,Rotation);
	        }

	        NewObjectCircle.Info&=OBJECT_FILLED;
	        if (AddObjectCircle(&NewObjectCircle)) {
	          ObjectCircle=&((*ObjectCircles)[cnt]);
	          SetBackGroundActive(0);
	          DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	          DrawObjectCircle(&NewObjectCircle,0.0,0.0,0);
	          ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	          ObjectCircle->DeleteNr=(int16)LastActionNr;
	          Changed=1;
	          if ((NewObjectCircle.Layer==DRILL_UNPLATED_LAYER)
	             ||
	             (NewObjectCircle.Layer==DRILL_LAYER)) {
	            if (NewObjectCircle.Layer==DRILL_UNPLATED_LAYER) {
	              NewObject.ObjectType=DRILL_UNPLATED;
	            } else {
	              NewObject.ObjectType=DRILL;
	            }
	            NewObject.x1=NewObjectCircle.CentreX;
	            NewObject.y1=NewObjectCircle.CentreY;
	            NewObject.x2=NewObjectCircle.Diam;
	            NewObject.Layer=-1;
	            if (InsertObjectInAreaFill(&NewObject,-1,-1,2)==1) {
	              if (OkToDrawAreaFills) {
	                RePaint();
	              }
	            }
	          }
	        }
	      } else {
	        memset(&NewObjectArc,0,sizeof(ObjectArcRecord));
	        NewObjectArc.Layer=ObjectCircle->Layer;
	        NewObjectArc.CentreX=ObjectCircle->CentreX;
	        NewObjectArc.CentreY=ObjectCircle->CentreY;
	        NewObjectArc.Width=ObjectCircle->Diam;
	        NewObjectArc.Height=ObjectCircle->Diam;
	        NewObjectArc.LineThickNess=ObjectCircle->LineThickNess;
	        switch (ObjectCircle->CircleMode) {
	          case 1:
	            NewObjectArc.StartDiffX=ObjectCircle->Diam;
	            NewObjectArc.StartDiffY=0.0;
	            NewObjectArc.EndDiffX=0.0;
	            NewObjectArc.EndDiffY=ObjectCircle->Diam;
	            break;
	          case 2:
	            NewObjectArc.StartDiffX=0.0;
	            NewObjectArc.StartDiffY=-ObjectCircle->Diam;
	            NewObjectArc.EndDiffX=ObjectCircle->Diam;
	            NewObjectArc.EndDiffY=0.0;
	            break;
	          case 3:
	            NewObjectArc.StartDiffX=0.0;
	            NewObjectArc.StartDiffY=-ObjectCircle->Diam;
	            NewObjectArc.EndDiffX=0.0;
	            NewObjectArc.EndDiffY=ObjectCircle->Diam;
	            break;
	          case 4:
	            NewObjectArc.StartDiffX=-ObjectCircle->Diam;
	            NewObjectArc.StartDiffY=0.0;
	            NewObjectArc.EndDiffX=0.0;
	            NewObjectArc.EndDiffY=-ObjectCircle->Diam;
	            break;
	          case 6:
	            NewObjectArc.StartDiffX=-ObjectCircle->Diam;
	            NewObjectArc.StartDiffY=0.0;
	            NewObjectArc.EndDiffX=ObjectCircle->Diam;
	            NewObjectArc.EndDiffY=0.0;
	            break;
	          case 8:
	            NewObjectArc.StartDiffX=0.0;
	            NewObjectArc.StartDiffY=ObjectCircle->Diam;
	            NewObjectArc.EndDiffX=-ObjectCircle->Diam;
	            NewObjectArc.EndDiffY=0.0;
	            break;
	          case 9:
	            NewObjectArc.StartDiffX=ObjectCircle->Diam;
	            NewObjectArc.StartDiffY=0.0;
	            NewObjectArc.EndDiffX=-ObjectCircle->Diam;
	            NewObjectArc.EndDiffY=0.0;
	            break;
	          case 12:
	            NewObjectArc.StartDiffX=0.0;
	            NewObjectArc.StartDiffY=ObjectCircle->Diam;
	            NewObjectArc.EndDiffX=0.0;
	            NewObjectArc.EndDiffY=-ObjectCircle->Diam;
	            break;
	        }
	        RotatePointFromOtherPoint2(&NewObjectArc.CentreX,&NewObjectArc.CentreY,CentreX,CentreY,Rotation);
	        RotatePointFromOtherPoint2(&NewObjectArc.StartDiffX,&NewObjectArc.StartDiffY,0.0,0.0,Rotation);
	        RotatePointFromOtherPoint2(&NewObjectArc.EndDiffX,&NewObjectArc.EndDiffY,0.0,0.0,Rotation);
	        if (AddObjectArc(&NewObjectArc)) {
	          SetBackGroundActive(0);
	          DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	          ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	          ObjectCircle->DeleteNr=(int16)LastActionNr;

	          DrawObjectArc(&NewObjectArc,0.0,0.0,0);
	          Changed=1;
	        }
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			ObjectArc->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));

			if (Mode == 1)
				Rotation = 90.0;

			RotatePointFromOtherPoint(&NewObjectArc.CentreX, &NewObjectArc.CentreY, CentreX, CentreY, Rotation);
			RotatePointFromOtherPoint(&NewObjectArc.StartDiffX, &NewObjectArc.StartDiffY, 0.0, 0.0, Rotation);
			RotatePointFromOtherPoint(&NewObjectArc.EndDiffX, &NewObjectArc.EndDiffY, 0.0, 0.0, Rotation);

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectText->AddNr<=TempLastActionNr)) {
	      ObjectText->Info&=~OBJECT_SELECTED;
	      memmove(&NewObjectText,ObjectText,sizeof(ObjectTextRecord));
	      RotateFlipPoint(&NewObjectText.X,&NewObjectText.Y,CentreX,CentreY,1);
	      Rotation2=(ObjectText->TextMode >> 8) & 3;
	      Rotation2=(Rotation2+1) & 3;
	      NewObjectText.TextMode=(NewObjectText.TextMode & ~0x300)+(Rotation2 << 8);
	      NewObjectText.Info=0;
	      if (AddObjectText(&NewObjectText)) {
	        ObjectText=&((*ObjectTexts)[cnt]);
	        SetBackGroundActive(0);
	        DrawObjectText(ObjectText,0.0,0.0,0);
	        DrawObjectText(&NewObjectText,0.0,0.0,0);
	        ObjectText->Info|=OBJECT_NOT_VISIBLE;
	        ObjectText->DeleteNr=(int16)LastActionNr;
	        Changed=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->AddNr <= TempLastActionNr))
		{
			ObjectText2->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));

			if (Mode == 1)
				Rotation = ANGLE_CONVERT(90.0);

			RotatePointFromOtherPoint(&NewObjectText2.X, &NewObjectText2.Y, CentreX, CentreY, Rotation);
			NewObjectText2.Rotation += (float) Rotation;

			if (NewObjectText2.Rotation >= 360.0)
				NewObjectText2.Rotation -= 360.0;

//      NewObjectText2.Rotation+=ANGLE_CONVERT(Rotation);
			NewObjectText2.Info = 0;

			if (AddObjectText2(&NewObjectText2))
			{
				ObjectText2 = &((*ObjectTexts2)[cnt]);
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
				ObjectText2->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			PolygonSize = MemSizeObjectPolygon(ObjectPolygon);
			memmove(&NewObjectPolygon, ObjectPolygon, PolygonSize);

			if (Mode == 1)
				Rotation = ANGLE_CONVERT(90.0);

			RotateObjectPolygon(&NewObjectPolygon, CentreX, CentreY, Rotation, 0);

			if (AddObjectPolygon(&NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ScaleSelectedSpecialObjects(int32 mode)
{
	int32 cnt, cnt3, count, TempLastActionNr, PolygonSize;
	float Scale;
	int32 Changed;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 ChangedObjectText2;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectRecord NewObject;


	memset(&ChangedObjectText2, 0, sizeof(ObjectTextRecord2));
	memset(&NewObject, 0, sizeof(NewObject));
	strcpy(ChangedObjectText2.Text, "1.00000");

	if (TextInputDialog(&ChangedObjectText2, 0x10 + 3) != 1)
		return;

	sscanf(ChangedObjectText2.Text, "%f", &Scale);

	if ((Scale < 0.0001) || (Scale > 10000.0))
		return;

	CheckInputMessages(200);
	TempLastActionNr = (int16) LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			ObjectLine->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			NewObjectLine.X1 *= (float) Scale;
			NewObjectLine.Y1 *= (float) Scale;
			NewObjectLine.X2 *= (float) Scale;
			NewObjectLine.Y2 *= (float) Scale;
			NewObjectLine.LineThickNess *= (float) Scale;
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
			ObjectRect->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			NewObjectRect.CentreX *= (float) Scale;
			NewObjectRect.CentreY *= (float) Scale;
			NewObjectRect.Width *= (float) Scale;
			NewObjectRect.Height *= (float) Scale;
			NewObjectRect.Info &= OBJECT_FILLED;

			if ((NewObjectRect.Info & OBJECT_FILLED) == 0)
				NewObjectRect.LineThickNess *= (float) Scale;

			if (AddObjectRect(&NewObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectCircle->AddNr<=TempLastActionNr)) {
	      ObjectCircle->Info&=~OBJECT_SELECTED;
	      memmove(&NewObjectCircle,ObjectCircle,sizeof(ObjectCircleRecord));
	      NewObjectCircle.CentreX*=Scale;
	      NewObjectCircle.CentreY*=Scale;
	      NewObjectCircle.Diam*=Scale;

	      NewObjectCircle.Info&=OBJECT_FILLED;
	      if (AddObjectCircle(&NewObjectCircle)) {
	        ObjectCircle=&((*ObjectCircles)[cnt]);
	        SetBackGroundActive(0);
	        DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	        DrawObjectCircle(&NewObjectCircle,0.0,0.0,0);
	        ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	        ObjectCircle->DeleteNr=(int16)LastActionNr;
	        Changed=1;
	        if ((NewObjectCircle.Layer==DRILL_UNPLATED_LAYER)
	           ||
	           (NewObjectCircle.Layer==DRILL_LAYER)) {
	          if (NewObjectCircle.Layer==DRILL_UNPLATED_LAYER) {
	            NewObject.ObjectType=DRILL_UNPLATED;
	          } else {
	            NewObject.ObjectType=DRILL;
	          }
	          NewObject.x1=NewObjectCircle.CentreX;
	          NewObject.y1=NewObjectCircle.CentreY;
	          NewObject.x2=NewObjectCircle.Diam;
	          NewObject.Layer=-1;
	          if (InsertObjectInAreaFill(&NewObject,-1,-1,2)==1) {
	            if (OkToDrawAreaFills) {
	              RePaint();
	            }
	          }
	        }
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			ObjectArc->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			NewObjectArc.CentreX *= (float) Scale;
			NewObjectArc.CentreY *= (float) Scale;
			NewObjectArc.Width *= (float) Scale;
			NewObjectArc.Height *= (float) Scale;
			NewObjectArc.StartDiffX *= (float) Scale;
			NewObjectArc.StartDiffY *= (float) Scale;
			NewObjectArc.EndDiffX *= (float) Scale;
			NewObjectArc.EndDiffY *= (float) Scale;
			NewObjectArc.Info &= OBJECT_FILLED;

			if ((NewObjectArc.Info & OBJECT_FILLED) == 0)
				NewObjectArc.LineThickNess *= (float) Scale;

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectText->AddNr<=TempLastActionNr)) {
	      ObjectText->Info&=~OBJECT_SELECTED;
	      memmove(&NewObjectText,ObjectText,sizeof(ObjectTextRecord));
	      NewObjectText.X*=Scale;
	      NewObjectText.Y*=Scale;
	      NewObjectText.FontHeight*=Scale;
	      if (AddObjectText(&NewObjectText)) {
	        ObjectText=&((*ObjectTexts)[cnt]);
	        SetBackGroundActive(0);
	        DrawObjectText(ObjectText,0.0,0.0,0);
	        DrawObjectText(&NewObjectText,0.0,0.0,0);
	        ObjectText->Info|=OBJECT_NOT_VISIBLE;
	        ObjectText->DeleteNr=(int16)LastActionNr;
	        Changed=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->AddNr <= TempLastActionNr))
		{
			ObjectText2->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			NewObjectText2.X *= (float) Scale;
			NewObjectText2.Y *= (float) Scale;
			NewObjectText2.FontHeight *= (float) Scale;
			NewObjectText2.LineThickNess *= (float) Scale;

			if (AddObjectText2(&NewObjectText2))
			{
				ObjectText2 = &((*ObjectTexts2)[cnt]);
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
				ObjectText2->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			PolygonSize = MemSizeObjectPolygon(ObjectPolygon);
			memmove(&NewObjectPolygon, ObjectPolygon, PolygonSize);
			count = NewObjectPolygon.NrVertices;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				NewObjectPolygon.Points[cnt3].x *= (float) Scale;
				NewObjectPolygon.Points[cnt3].y *= (float) Scale;
			}

			if (AddObjectPolygon(&NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveSelectedRefs(int32 Mode, int32 Count)
{
	int32 CompPlaced, FirstShift;
	double OldX, OldY, CurrentX, CurrentY, hx, hy, divx, divy, ShiftX, ShiftY, x1, y1, CurrentX2, CurrentY2, Rotation;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;


	if (GetNrReferencesSelected() == 0)
		return;

	SelectionEsc = 0;

//  GetMinMaxSelectedComponents();
	hx = (SearchMinX + SearchMaxX) / 2;
	hy = (SearchMinY + SearchMaxY) / 2;
	CentreSelectedX = AdjustToDrawGrid(hx);
	CentreSelectedY = AdjustToDrawGrid(hy);

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	Rotation = 0.0;
	ClipMouseCursor();
	DrawSelectedRefs(CurrentX, CurrentY, Rotation);
	FirstShift = 1;
	SystemBusyMode = 4;
	DrawXorFunction.Function2 = (FUNCP2) DrawSelectedRefs;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Rotation;
	DrawXorFunction.Mode = 1;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Rotation;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			DrawCrossHair(0);

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawSelectedRefs(OldX, OldY, Rotation);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawSelectedRefs(CurrentX, CurrentY, Rotation);
				
				x1 = (CurrentX - CurrentX2) / 100000.0;
				y1 = (CurrentY - CurrentY2) / 100000.0;
				sprintf(InfoStr, "%.4f , %.4f", x1, y1);
				
				RedrawInfoStr(1);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawSelectedRefs(OldX, OldY, Rotation);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawSelectedRefs(OldX, OldY, Rotation);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawSelectedRefs(OldX, OldY, Rotation);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawSelectedRefs(OldX, OldY, Rotation);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{
				CentreSelectedX -= ShiftX - CurrentX;
				CentreSelectedY -= ShiftY - CurrentY;
				FirstShift = 1;
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
			DrawSelectedRefs(CurrentX, CurrentY, Rotation);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawSelectedRefs(OldX, OldY, Rotation);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawSelectedRefs(OldX, OldY, Rotation);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawSelectedRefs(OldX, OldY, Rotation);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
		}

		if (CheckLeftButton())
		{
			CompPlaced = 1;
			divx = CurrentX - CentreSelectedX;
			divy = CurrentY - CentreSelectedY;
			DrawSelectedRefs(CurrentX, CurrentY, Rotation);
			PlaceMovedRefs(CurrentX, CurrentY, Rotation);
			CheckInputMessages(0);
			SelectionEsc = 1;
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawSelectedRefs(OldX, OldY, Rotation);
//      TrackPopupMenu(PopUpMenu,TPM_RIGHTBUTTON,
//                     RealWindow.left+MousePosX+5,RealWindow.top+MousePosY+40,0,PCBWindow,NULL);
			RightButtonPressed = 0;
			CheckInputMessages(0);

			if (AltPressed)
				Rotation += 45.0;
			else
				Rotation += 90.0;

			if (Rotation >= 360.0)
				Rotation -= 360.0;

			DrawSelectedRefs(OldX, OldY, Rotation);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawSelectedRefs(OldX, OldY, Rotation);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (HelpAsked)
			{
				Help("moving_component_references.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			FirstShift = 1;

			if (!SelectionEsc)
				DrawSelectedRefs(CurrentX, CurrentY, Rotation);
		}
	}

	UnClipMouseCursor();
	DrawCrossHair(2);
	UnselectAll = 1;
	SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
	UnselectAll = 0;
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawSelectedRefs(double CurrentX, double CurrentY, double Rotation)
{
	int32 cnt, CompInfo;
	CompRecord *Comp;
	int32 ObjectChanged;

	StartDrawingEditingWindow();

	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;
		ObjectChanged = 0;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && ((Comp->TextVisibility & (1)) == 1))
			DrawReferenceComp(Comp, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, Rotation, 8 + 1);
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MoveSelectedCompValues(int32 Mode, int32 Count)
{
	int32 CompPlaced, FirstShift;
	double OldX, OldY, CurrentX, CurrentY, hx, hy, divx, divy, ShiftX, ShiftY, Rotation, CurrentX2, CurrentY2;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;

	if (GetNrCompValuesSelected() == 0)
		return;

	SelectionEsc = 0;

//  GetMinMaxSelectedComponents();
	hx = (SearchMinX + SearchMaxX) / 2;
	hy = (SearchMinY + SearchMaxY) / 2;
	CentreSelectedX = AdjustToDrawGrid(hx);
	CentreSelectedY = AdjustToDrawGrid(hy);

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	Rotation = 0.0;
	ClipMouseCursor();
	DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
	FirstShift = 1;
	SystemBusyMode = 5;
	DrawXorFunction.Function2 = (FUNCP2) DrawSelectedCompValues;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Rotation;
	DrawXorFunction.Mode = 1;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Rotation;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			DrawCrossHair(0);

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawSelectedCompValues(OldX, OldY, Rotation);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawSelectedCompValues(OldX, OldY, Rotation);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawSelectedCompValues(OldX, OldY, Rotation);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawSelectedCompValues(OldX, OldY, Rotation);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawSelectedCompValues(OldX, OldY, Rotation);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{
				CentreSelectedX -= ShiftX - CurrentX;
				CentreSelectedY -= ShiftY - CurrentY;
				FirstShift = 1;
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
			DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawSelectedCompValues(OldX, OldY, Rotation);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawSelectedCompValues(OldX, OldY, Rotation);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawSelectedCompValues(OldX, OldY, Rotation);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
		}

		if (CheckLeftButton())
		{
			CompPlaced = 1;
			divx = CurrentX - CentreSelectedX;
			divy = CurrentY - CentreSelectedY;
			DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
			PlaceMovedCompValues(CurrentX, CurrentY, Rotation);
			CheckInputMessages(0);
			SelectionEsc = 1;
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawSelectedCompValues(OldX, OldY, Rotation);
//      TrackPopupMenu(PopUpMenu,TPM_RIGHTBUTTON,
//                     RealWindow.left+MousePosX+5,RealWindow.top+MousePosY+40,0,PCBWindow,NULL);
			RightButtonPressed = 0;
			CheckInputMessages(0);

			if (AltPressed)
				Rotation += 45.0;
			else
				Rotation += 90.0;

			if (Rotation >= 360.0)
				Rotation -= 360.0;

			DrawSelectedCompValues(OldX, OldY, Rotation);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawSelectedCompValues(OldX, OldY, Rotation);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if (HelpAsked)
			{
				Help("moving_component_values.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			OldX = CurrentX;
			OldY = CurrentY;
			FirstShift = 1;

			if (!SelectionEsc)
				DrawSelectedCompValues(CurrentX, CurrentY, Rotation);
		}
	}

	UnClipMouseCursor();
	DrawCrossHair(2);
	UnselectAll = 1;
	SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
	UnselectAll = 0;
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawSelectedCompValues(double CurrentX, double CurrentY, double Rotation)
{
	int32 cnt, CompInfo;
	CompRecord *Comp;
	int32 ObjectChanged;

	StartDrawingEditingWindow();

	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;
		ObjectChanged = 0;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && ((Comp->TextVisibility & (0x10)) == 0x10))
			DrawValueComp(Comp, CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, Rotation, 8 + 1);
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetMinMaxSelectedComponents()
{
	int32 cnt;
	CompRecord *Comp;

	StartDrawingEditingWindow();

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && ((Comp->TextVisibility & 1) == 1))
		{
			SearchMinX = Comp->BoardPosMinX;
			SearchMinY = Comp->BoardPosMinY;
			SearchMaxX = Comp->BoardPosMaxX;
			SearchMaxY = Comp->BoardPosMaxY;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceMovedRefs(double CurrentX, double CurrentY, double Rotation)
{
	int32 cnt, TempLastActionNr, MemSize, Mirror, CompInfo;
	double DifX, DifY, rx, ry;
	CompRecord *Comp, *NewComp;

	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->AddNr <= TempLastActionNr)
		        && ((Comp->TextVisibility & (1)) == 1))
		{
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
			memmove(NewComp, Comp, MemSize);

			DifX = CurrentX - CentreSelectedX;
			DifY = CurrentY - CentreSelectedY;
			Mirror = ((Comp->TextVisibility & 8) >> 3);

//     TextVisibility
//
//     0x001   selected
//     0x002   rotated
//     0x004   rotated
//     0x008   mirrored
//     0x100   visible
//     0x400   rotated (45)


			rx = NewComp->CompNameOriginX;
			ry = NewComp->CompNameOriginY;

			if (Mirror == 1)
				rx = -rx;

			RotatePoint2(&rx, &ry, Comp->Rotation);
			rx += DifX;
			ry += DifY;
			RotatePoint2(&rx, &ry, 360.0 - Comp->Rotation);

			if (Mirror == 0)
				NewComp->CompNameRotation += (float) Rotation;
			else
			{
				NewComp->CompNameRotation += (float) Rotation;
				rx = -rx;
			}

			if (NewComp->CompNameRotation < 0.0)
				NewComp->CompNameRotation += 360.0;

			if (NewComp->CompNameRotation > 360.0)
				NewComp->CompNameRotation -= 360.0;

			NewComp->CompNameOriginX = (float) rx;
			NewComp->CompNameOriginY = (float) ry;
			NewComp->TextVisibility &= ~(0x400 + 0x10 + 0x4 + 0x2 + 1);
			Comp->TextVisibility &= ~(1 + 0x10);
			NewComp->Info &= ~OBJECT_SELECTED;
			Comp->Info &= ~OBJECT_SELECTED;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceMovedCompValues(double CurrentX, double CurrentY, double Rotation)
{
	int32 cnt, TempLastActionNr, MemSize, Mirror, CompInfo;
	double DifX, DifY, rx, ry;
	CompRecord *Comp, *NewComp;

	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->AddNr <= TempLastActionNr)
		        && ((Comp->TextVisibility & (0x10)) == 0x10))
		{
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
			memmove(NewComp, Comp, MemSize);

			DifX = CurrentX - CentreSelectedX;
			DifY = CurrentY - CentreSelectedY;
			Mirror = ((Comp->TextVisibility & 0x80) >> 7);

//     TextVisibility
//
//     0x010   selected
//     0x020   rotated      Not used anymore
//     0x040   rotated      Not used anymore
//     0x080   mirrored
//     0x200   visible
//     0x800   rotated (45) Not used anymore


			rx = NewComp->CompValueOriginX;
			ry = NewComp->CompValueOriginY;

			if (Mirror == 1)
				rx = -rx;

			RotatePointFromOtherPoint2(&rx, &ry, 0.0, 0.0, Comp->Rotation);
			rx += DifX;
			ry += DifY;
			RotatePointFromOtherPoint2(&rx, &ry, 0.0, 0.0, 360.0 - Comp->Rotation);

			if (Mirror == 0)
				NewComp->CompValueRotation += (float) Rotation;
			else
			{
				NewComp->CompValueRotation += (float) Rotation;
				rx = -rx;
			}

			if (NewComp->CompValueRotation > 360)
				NewComp->CompValueRotation -= 360;

			if (NewComp->CompValueRotation < 0)
				NewComp->CompValueRotation += 360;

			NewComp->CompValueOriginX = (float) rx;
			NewComp->CompValueOriginY = (float) ry;
			NewComp->TextVisibility &= ~(0x800 + 0x40 + 0x20 + 0x10 + 1);
			Comp->TextVisibility &= ~(1 + 0x10);
			NewComp->Info &= ~OBJECT_SELECTED;
			Comp->Info &= ~OBJECT_SELECTED;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceRotatedComponentReferences(double Rotation, int32 mode)
{
	int32 cnt, TempLastActionNr, MemSize, Mirror, CompInfo;
	float Rotation2;
	CompRecord *Comp, *NewComp;
	ObjectTextRecord2 ChangedObjectText2;

	TempLastActionNr = (int16) LastActionNr - 1;
	Rotation2 = 0.0;

	switch (mode)
	{
	case 2:
	case 3:
		memset(&ChangedObjectText2, 0, sizeof(ObjectTextRecord));
		strcpy(ChangedObjectText2.Text, "0.0");

		if (TextInputDialog(&ChangedObjectText2, 0x10 + 4) != 1)
			return;

		sscanf(ChangedObjectText2.Text, "%f", &Rotation2);

		if ((Rotation2 < -360.0) || (Rotation2 > 360.0))
			return;

		break;
	}

	CheckInputMessages(200);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->AddNr <= TempLastActionNr)
		        && ((Comp->TextVisibility & (1)) == 1))
		{
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
			memmove(NewComp, Comp, MemSize);
			Mirror = ((Comp->TextVisibility & 8) >> 3);

//     TextVisibility
//
//     0x001   selected
//     0x002   rotated
//     0x004   rotated
//     0x008   mirrored
//     0x100   visible
//     0x400   rotated (45)

			switch (mode)
			{
			case 0:
				NewComp->CompNameRotation += (float) Rotation;
				break;

			case 1:
				if (Mirror == 0)
					NewComp->CompNameRotation = (float) (Rotation - Comp->Rotation);
				else
					NewComp->CompNameRotation = (float) (Rotation + Comp->Rotation);

				break;

			case 2:
				NewComp->CompNameRotation += (float) Rotation2;
				break;

			case 3:
				if (Mirror == 0)
					NewComp->CompNameRotation = (float) (Rotation2 - Comp->Rotation);
				else
					NewComp->CompNameRotation = (float) (Rotation2 + Comp->Rotation);

				break;
			}

			if (NewComp->CompNameRotation < 0.0)
				NewComp->CompNameRotation += 360.0;

			if (NewComp->CompNameRotation > 360.0)
				NewComp->CompNameRotation -= 360.0;

			NewComp->TextVisibility &= ~(0x400 + 0x10 + 0x4 + 0x2 + 1);
			Comp->TextVisibility &= ~(1 + 0x10);
			NewComp->Info &= ~OBJECT_SELECTED;
			Comp->Info &= ~OBJECT_SELECTED;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceRotatedComponentValues(double Rotation, int32 mode)
{
	int32 cnt, TempLastActionNr, MemSize, Mirror, CompInfo;
	float Rotation2;
	CompRecord *Comp, *NewComp;
	ObjectTextRecord2 ChangedObjectText2;

	TempLastActionNr = (int16) LastActionNr - 1;
	Rotation2 = 0.0;

	switch (mode)
	{
	case 2:
	case 3:
		memset(&ChangedObjectText2, 0, sizeof(ObjectTextRecord));
		strcpy(ChangedObjectText2.Text, "0.0");

		if (TextInputDialog(&ChangedObjectText2, 0x10 + 4) != 1)
			return;

		sscanf(ChangedObjectText2.Text, "%f", &Rotation2);

		if ((Rotation2 < -360.0) || (Rotation2 > 360.0))
			return;

		break;
	}

	CheckInputMessages(200);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->AddNr <= TempLastActionNr)
		        && ((Comp->TextVisibility & (0x10)) == 0x10))
		{
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
			memmove(NewComp, Comp, MemSize);

//     TextVisibility
//
//     0x010   selected
//     0x020   rotated      Not used anymore
//     0x040   rotated      Not used anymore
//     0x080   mirrored
//     0x200   visible
//     0x800   rotated (45) Not used anymore


			Mirror = ((Comp->TextVisibility & 0x80) >> 7);

			switch (mode)
			{
			case 0:
				NewComp->CompValueRotation += (float) Rotation;
				break;

			case 1:
				if (Mirror == 0)
					NewComp->CompValueRotation = (float) (Rotation - Comp->Rotation);
				else
					NewComp->CompValueRotation = (float) (Rotation + Comp->Rotation);

				break;

			case 2:
				NewComp->CompValueRotation += (float) Rotation2;
				break;

			case 3:
				if (Mirror == 0)
					NewComp->CompValueRotation = (float) (Rotation2 - Comp->Rotation);
				else
					NewComp->CompValueRotation = (float) (Rotation2 + Comp->Rotation);

				break;
			}

			if (NewComp->CompValueRotation < 0.0)
				NewComp->CompValueRotation += 360.0;

			if (NewComp->CompValueRotation > 360.0)
				NewComp->CompValueRotation -= 360.0;

			NewComp->TextVisibility &= ~(0x800 + 0x40 + 0x20 + 0x10 + 1);
			Comp->TextVisibility &= ~(1 + 0x10);
			NewComp->Info &= ~OBJECT_SELECTED;
			Comp->Info &= ~OBJECT_SELECTED;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeVisibilityCompRefsValues(int32 Mode)
{
	int32 cnt, TempLastActionNr, MemSize, CompInfo, Mask, CompareValue;
	CompRecord *Comp, *NewComp;

	Mask = 0;
	CompareValue = 0;

	TempLastActionNr = (int16) LastActionNr - 1;

	switch (Mode)
	{
	case 0:					// Hide component refs
		Mask = 0x100;
		CompareValue = 0;
		break;

	case 1:					// Visible component refs
		Mask = 0x100;
		CompareValue = 0x100;
		break;

	case 2:					// Hide component values
		Mask = 0x200;
		CompareValue = 0;
		break;

	case 3:					// Visible component values
		Mask = 0x200;
		CompareValue = 0x200;
		break;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Comp->AddNr <= TempLastActionNr) && ((Comp->TextVisibility & Mask) == CompareValue))
		{
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
			/*
			      if ((Mode & 2) == 0) {
			        Comp->TextVisibility&=~(OBJECT_SELECTED|1);
			      } else {
			        Comp->TextVisibility&=~(OBJECT_SELECTED|0x10);
			      }
			*/
			Comp->TextVisibility &= ~(OBJECT_SELECTED | 0x11);
			Comp->Info &= ~OBJECT_SELECTED;
			memmove(NewComp, Comp, MemSize);
			NewComp->TextVisibility ^= Mask;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeMirrorCompRefsValues(int32 Mode)
{
	int32 cnt, TempLastActionNr, MemSize, CompInfo, Mask, Mask1, CompareValue;
	CompRecord *Comp, *NewComp;

	Mask = 0;
	Mask1 = 0;
	CompareValue = 0;

	TempLastActionNr = (int16) LastActionNr - 1;

	switch (Mode)
	{
	case 0:					// Component refs normal
		Mask = 8;
		Mask1 = 8;
		CompareValue = 8;
		break;

	case 1:					// Component refs mirrored
		Mask = 8;
		Mask1 = 8;
		CompareValue = 0;
		break;

	case 2:					// Component values normal
		Mask = 0x80;
		Mask1 = 0x80;
		CompareValue = 0x80;
		break;

	case 3:					// Component values mirrored
		Mask = 0x80;
		Mask1 = 0x80;
		CompareValue = 0;
		break;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Comp->AddNr <= TempLastActionNr) && ((Comp->TextVisibility & Mask) == CompareValue))
		{
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);

			if ((Mode & 2) == 0)
			{
				Comp->Info &= ~OBJECT_SELECTED;
				Comp->TextVisibility &= ~1;
			}
			else
			{
				Comp->Info &= ~OBJECT_SELECTED;
				Comp->TextVisibility &= ~0x10;
			}

			memmove(NewComp, Comp, MemSize);
			NewComp->TextVisibility ^= Mask1;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeCompValue()
{
	int32 cnt, TempLastActionNr, MemSize, CompInfo;
	CompRecord *Comp, *NewComp;
	ObjectTextRecord2 ChangedObjectText2;

	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		MemSize = MemSizeComp(Comp);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Comp->AddNr <= TempLastActionNr))
		{
			memset(&ChangedObjectText2, 0, sizeof(ObjectTextRecord));
			memmove(&ChangedObjectText2.Text, Comp->Value, strlen(Comp->Value));

			if (TextInputDialog(&ChangedObjectText2, 0x10 + 2) == 1)
			{
				AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
				Comp->Info &= ~OBJECT_SELECTED;
				Comp->TextVisibility &= ~(1 + 16);
				memmove(NewComp, Comp, MemSize);
				memset(&NewComp->Value, 0, sizeof(Comp->Value));
				memmove(&NewComp->Value, &ChangedObjectText2.Text, max(strlen(Comp->Value), sizeof(Comp->Value) - 1));

				if (AddComp(NewComp))
				{
					NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
					Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
					Comp->Info |= OBJECT_NOT_VISIBLE;
					Comp->DeleteNr = (int16) LastActionNr;
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

void ChangeText(int32 Mode)
{
	int32 cnt, TempLastActionNr;
	int32 TextChanged, Found;
	ObjectTextRecord2 ChangedObjectText2, *ObjectText2;

	Found = 0;

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			Found = 1;
	}

	if (!Found)
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 1);

	TextChanged = 0;
	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->AddNr <= TempLastActionNr))
		{
			memmove(&ChangedObjectText2, ObjectText2, sizeof(ObjectTextRecord2));

			if (TextInputDialog(&ChangedObjectText2, 3) == 1)
			{
				ChangedObjectText2.Info &= ~OBJECT_SELECTED;

				if (AddObjectText2(&ChangedObjectText2))
				{
					ObjectText2 = &((*ObjectTexts2)[cnt]);
					ObjectText2->Info |= OBJECT_NOT_VISIBLE;
					ObjectText2->DeleteNr = (int16) LastActionNr;
					TextChanged = 1;
					RePaint();
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeTextHeight(int32 Mode)
{
	int32 cnt, TempLastActionNr;
	int32 TextChanged;
	ObjectTextRecord *ObjectText;
	ObjectTextRecord2 *ObjectText2, ChangedObjectText2;

	NewValue.MinValue = (1 * 2540);
	NewValue.MaxValue = (10000 * 2540);

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			NewValue.Value = ObjectText->FontHeight;
	}

	if (ValueDialog(2) != 1)
		return;

	TextChanged = 0;
	TempLastActionNr = (int16) LastActionNr - 1;

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (NotInRange(ObjectText->FontHeight,NewValue.Value))
	       &&
	       (ObjectText->AddNr<=TempLastActionNr)) {
	      memmove(&ChangedObjectText,ObjectText,sizeof(ObjectTextRecord));
	      ChangedObjectText.Info&=~OBJECT_SELECTED;
	      ChangedObjectText.FontHeight=NewValue.Value;
	      if (AddObjectText(&ChangedObjectText)) {
	        ObjectText=&((*ObjectTexts)[cnt]);
	        SetBackGroundActive(0);
	        DrawObjectText(ObjectText,0.0,0.0,1);
	        ObjectText->Info|=OBJECT_NOT_VISIBLE;
	        ObjectText->DeleteNr=(int16)LastActionNr;
	        DrawObjectText(&ChangedObjectText,0.0,0.0,0);
	        TextChanged=1;
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (NotInRange(ObjectText2->FontHeight, NewValue.Value)) && (ObjectText2->AddNr <= TempLastActionNr))
		{
			memmove(&ChangedObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			ChangedObjectText2.Info &= ~OBJECT_SELECTED;
			ChangedObjectText2.FontHeight = (float) NewValue.Value;

			if (AddObjectText2(&ChangedObjectText2))
			{
				ObjectText2 = &((*ObjectTexts2)[cnt]);
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
				ObjectText2->DeleteNr = (int16) LastActionNr;
				TextChanged = 1;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeTextHeightCompRefsValues(int32 Mode)
{
	int32 cnt, TempLastActionNr, MemSize, CompInfo, Mask, CompareValue;
	double CurrentHeight;
	CompRecord *Comp, *NewComp;

	NewValue.Value = 0.0;
	NewValue.MinValue = (1 * 2540);
	NewValue.MaxValue = (10000 * 2540);

	switch (Mode)
	{
	case 0:					// Component refs
		Mask = 4;
		CompareValue = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				NewValue.Value = Comp->CompNameHeight;
		}

		if (ValueDialog(2) != 1)
			return;

		break;

	case 2:					// Component values
		Mask = 0x40;
		CompareValue = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				NewValue.Value = Comp->CompValueHeight;
		}

		if (ValueDialog(2) != 1)
			return;

		break;
	}

	TempLastActionNr = (int16) LastActionNr - 1;
	StartDrawingEditingWindow();

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((Mode & 2) == 0)
			CurrentHeight = Comp->CompNameHeight;
		else
			CurrentHeight = Comp->CompValueHeight;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Comp->AddNr <= TempLastActionNr) && (NotInRange(CurrentHeight, NewValue.Value)))
		{
//       &&
//       ((Comp->TextVisibility & Mask) == CompareValue)) {
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);

			if ((Mode & 2) == 0)
				Comp->TextVisibility &= ~(OBJECT_SELECTED | 1);
			else
				Comp->TextVisibility &= ~(OBJECT_SELECTED | 0x10);

			Comp->Info &= ~OBJECT_SELECTED;
			memmove(NewComp, Comp, MemSize);

			if ((Mode & 2) == 0)
				NewComp->CompNameHeight = (float) NewValue.Value;
			else
				NewComp->CompValueHeight = (float) NewValue.Value;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				SetBackGroundActive(0);
				DrawComp2(Comp, 0.0, 0.0, 0, 1);
				DrawComp2(NewComp, 0.0, 0.0, 0, 0);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeLineWidth(int32 Mode)
{
	int32 cnt, TempLastActionNr;

	ObjectLineRecord *ObjectLine, ChangedObjectLine;
	ObjectRectRecord *ObjectRect, ChangedObjectRect;
	ObjectArcRecord *ObjectArc, ChangedObjectArc;
	ObjectTextRecord2 *ObjectText2, ChangedObjectText2;

	NewValue.Value = 0.0;
	NewValue.MinValue = 0.0;
	NewValue.MaxValue = (10000 * 2540);


	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			NewValue.Value = ObjectLine->LineThickNess;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
			NewValue.Value = ObjectRect->LineThickNess;
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED|OBJECT_FILLED)) == OBJECT_SELECTED) {
	      NewValue.Value=ObjectCircle->LineThickNess;
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
			NewValue.Value = ObjectArc->LineThickNess;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			NewValue.Value = ObjectText2->LineThickNess;
	}

	if (ValueDialog(3) != 1)
		return;

	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (NotInRange(ObjectLine->LineThickNess, NewValue.Value)) && (ObjectLine->AddNr <= TempLastActionNr))
		{
			ObjectLine->Info &= ~OBJECT_SELECTED;
			memmove(&ChangedObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			ChangedObjectLine.LineThickNess = (float) NewValue.Value;

			if (AddObjectLine(&ChangedObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
				ObjectLine->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
		        && (NotInRange(ObjectRect->LineThickNess, NewValue.Value)) && (ObjectRect->AddNr <= TempLastActionNr))
		{
			ObjectRect->Info &= ~OBJECT_SELECTED;
			memmove(&ChangedObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			ChangedObjectRect.LineThickNess = (float) NewValue.Value;

			if (AddObjectRect(&ChangedObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED|OBJECT_FILLED)) == OBJECT_SELECTED)
	       &&
	       (NotInRange(ObjectCircle->LineThickNess,NewValue.Value))
	       &&
	       (ObjectCircle->AddNr<=TempLastActionNr)) {
	      ObjectCircle->Info&=~OBJECT_SELECTED;
	      memmove(&ChangedObjectCircle,ObjectCircle,sizeof(ObjectCircleRecord));
	      ChangedObjectCircle.LineThickNess=NewValue.Value;
	      if (AddObjectCircle(&ChangedObjectCircle)) {
	        ObjectCircle=&((*ObjectCircles)[cnt]);
	        SetBackGroundActive(0);
	        DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	        ObjectCircle->Info|=OBJECT_NOT_VISIBLE;
	        ObjectCircle->DeleteNr=(int16)LastActionNr;
	        DrawObjectCircle(&ChangedObjectCircle,0.0,0.0,0);
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
		        && (NotInRange(ObjectArc->LineThickNess, NewValue.Value)) && (ObjectArc->AddNr <= TempLastActionNr))
		{
			ObjectArc->Info &= ~OBJECT_SELECTED;
			memmove(&ChangedObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			ChangedObjectArc.LineThickNess = (float) NewValue.Value;

			if (AddObjectArc(&ChangedObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (NotInRange(ObjectText->LineThickNess,NewValue.Value))
	       &&
	       (ObjectText->AddNr<=TempLastActionNr)) {
	      ObjectText->Info&=~OBJECT_SELECTED;
	      memmove(&ChangedObjectText,ObjectText,sizeof(ObjectTextRecord));
	      ChangedObjectText.LineThickNess=NewValue.Value;
	      if (AddObjectText(&ChangedObjectText)) {
	        ObjectText=&((*ObjectTexts)[cnt]);
	        SetBackGroundActive(0);
	        DrawObjectText(ObjectText,0.0,0.0,0);
	        ObjectText->Info|=OBJECT_NOT_VISIBLE;
	        ObjectText->DeleteNr=(int16)LastActionNr;
	        DrawObjectText(&ChangedObjectText,0.0,0.0,0);
	      }
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->AddNr <= TempLastActionNr))
		{
			ObjectText2->Info &= ~OBJECT_SELECTED;
			memmove(&ChangedObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			ChangedObjectText2.LineThickNess = (float) NewValue.Value;

			if (AddObjectText2(&ChangedObjectText2))
			{
				ObjectText2 = &((*ObjectTexts2)[cnt]);
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
				ObjectText2->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeCircleDiameter(int32 mode)
{
	int32 cnt, TempLastActionNr;

	ObjectArcRecord *ObjectArc, ChangedObjectArc;


	NewValue.Value = 0.0;
	NewValue.MinValue = (1 * 2540);
	NewValue.MaxValue = (10000 * 2540);

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (CheckObjectArcIsCircle(ObjectArc, 0))
				NewValue.Value = ObjectArc->Width;
		}
	}

	if (ValueDialog(4) != 1)
		return;

	TempLastActionNr = LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (CheckObjectArcIsCircle(ObjectArc, 0)) && (NotInRange(ObjectArc->Width, NewValue.Value))
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			ObjectArc->Info &= ~OBJECT_SELECTED;
			memmove(&ChangedObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			ChangedObjectArc.Width = (float) NewValue.Value;
			ChangedObjectArc.Height = (float) NewValue.Value;

			if (AddObjectArc(&ChangedObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeRectangle(int32 mode)
{
	int32 cnt, TempLastActionNr;

	ObjectRectRecord *ObjectRect, ChangedObjectRect;

	NewValue.Value = 0.0;
	NewValue.MinValue = (1 * 2540);
	NewValue.MaxValue = (10000 * 2540);
	NewValue.Value2 = 0.0;
	NewValue.MinValue2 = (1 * 2540);
	NewValue.MaxValue2 = (10000 * 2540);

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NewValue.Value = ObjectRect->Width;
			NewValue.Value2 = ObjectRect->Height;
		}
	}

	if (ValueDialog(5) != 1)
		return;

	TempLastActionNr = LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->AddNr <= TempLastActionNr) && ((NotInRange(ObjectRect->Width, NewValue.Value))
		                || (NotInRange(ObjectRect->Height, NewValue.Value2))))
		{
			ObjectRect->Info &= ~OBJECT_SELECTED;
			memmove(&ChangedObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			ChangedObjectRect.Width = (float) NewValue.Value;
			ChangedObjectRect.Height = (float) NewValue.Value2;

			if (AddObjectRect(&ChangedObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangeArc(int32 mode)
{
	int32 cnt, TempLastActionNr;
	double Length, Angle1, Angle2;
	float value1 = 0.0, value2 = 0.0;

	ObjectArcRecord *ObjectArc, ChangedObjectArc;
	ObjectTextRecord2 ChangedObjectText2;

	NewValue.Value = 0.0;
	NewValue.MinValue = (1 * 2540);
	NewValue.MaxValue = (10000 * 2540);
	NewValue.Value2 = 0.0;
	NewValue.MinValue2 = (1 * 2540);
	NewValue.MaxValue2 = (10000 * 2540);
	Angle1 = 0.0;
	Angle2 = 0.0;

	if (mode == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
			{
				NewValue.Value = ObjectArc->Width;
				NewValue.Value2 = ObjectArc->Height;
			}
		}

		if (ValueDialog(5) != 1)
			return;
	}
	else
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
			{
				ConvertPointToPolar(ObjectArc->StartDiffX, ObjectArc->StartDiffY, &Length, &Angle1);
				ConvertPointToPolar(ObjectArc->EndDiffX, ObjectArc->EndDiffY, &Length, &Angle2);
				NewValue.Value = Angle1;
				NewValue.Value2 = Angle2;
			}
		}

		memset(&ChangedObjectText2, 0, sizeof(ObjectTextRecord));
		sprintf(ChangedObjectText2.Text, "%.2f,%.2f", Angle1, Angle2);

		if (TextInputDialog(&ChangedObjectText2, 0x10 + 5) != 1)
			return;

		if (sscanf(ChangedObjectText2.Text, "%f,%f", &value1, &value2) != 2)
			return;

		NewValue.Value = value1;
		NewValue.Value2 = value2;
	}

	TempLastActionNr = (int16) LastActionNr - 1;

	if (mode == 0)
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
			        && (ObjectArc->AddNr <= TempLastActionNr) && ((NotInRange(ObjectArc->Width, NewValue.Value))
			                || (NotInRange(ObjectArc->Height, NewValue.Value2))))
			{
				ObjectArc->Info &= ~OBJECT_SELECTED;
				memmove(&ChangedObjectArc, ObjectArc, sizeof(ObjectArcRecord));
				ChangedObjectArc.Width = (float) NewValue.Value;
				ChangedObjectArc.Height = (float) NewValue.Value2;

				if (AddObjectArc(&ChangedObjectArc))
				{
					ObjectArc = &((*ObjectArcs)[cnt]);
					ObjectArc->Info |= OBJECT_NOT_VISIBLE;
					ObjectArc->DeleteNr = (int16) LastActionNr;
				}
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED | OBJECT_FILLED)) == OBJECT_SELECTED)
			        && (ObjectArc->AddNr <= TempLastActionNr))
			{
				ConvertPointToPolar(ObjectArc->StartDiffX, ObjectArc->StartDiffY, &Length, &Angle1);
				ConvertPointToPolar(ObjectArc->EndDiffX, ObjectArc->EndDiffY, &Length, &Angle2);

				if (((NotInRange(Angle1, NewValue.Value)) || (NotInRange(Angle2, NewValue.Value2))))
				{
					ObjectArc->Info &= ~OBJECT_SELECTED;
					memmove(&ChangedObjectArc, ObjectArc, sizeof(ObjectArcRecord));
					ChangedObjectArc.StartDiffX = (float) (cos(ANGLE_CONVERT(NewValue.Value)) * ChangedObjectArc.Width);
					ChangedObjectArc.StartDiffY = (float) (sin(ANGLE_CONVERT(NewValue.Value)) * ChangedObjectArc.Width);
					ChangedObjectArc.EndDiffX = (float) (cos(ANGLE_CONVERT(NewValue.Value2)) * ChangedObjectArc.Height);
					ChangedObjectArc.EndDiffY = (float) (sin(ANGLE_CONVERT(NewValue.Value2)) * ChangedObjectArc.Height);

					if (AddObjectArc(&ChangedObjectArc))
					{
						ObjectArc = &((*ObjectArcs)[cnt]);
						ObjectArc->Info |= OBJECT_NOT_VISIBLE;
						ObjectArc->DeleteNr = (int16) LastActionNr;
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

void ChangeLineWidthCompRefsValues(int32 Mode)
{
	int32 cnt, TempLastActionNr, MemSize, CompInfo, Mask, CompareValue;
	double CurrentHeight;
	CompRecord *Comp, *NewComp;

	NewValue.Value = 0.0;
	NewValue.MinValue = (1 * 2540);
	NewValue.MaxValue = (10000 * 2540);

	switch (Mode)
	{
	case 0:					// Component refs
		Mask = 4;
		CompareValue = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				NewValue.Value = Comp->CompNamePenThickNess;
		}

		if (ValueDialog(3) != 1)
			return;

		break;

	case 2:					// Component values
		Mask = 0x40;
		CompareValue = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				NewValue.Value = Comp->CompValuePenThickNess;
		}

		if (ValueDialog(3) != 1)
			return;

		break;
	}

	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((Mode & 2) == 0)
			CurrentHeight = Comp->CompNamePenThickNess;
		else
			CurrentHeight = Comp->CompValuePenThickNess;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Comp->AddNr <= TempLastActionNr) && (NotInRange(CurrentHeight, NewValue.Value)))
		{
//       &&
//       ((Comp->TextVisibility & Mask) == CompareValue)) {
			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);

			if ((Mode & 2) == 0)
				Comp->TextVisibility &= ~(OBJECT_SELECTED | 1);
			else
				Comp->TextVisibility &= ~(OBJECT_SELECTED | 0x10);

			Comp->Info &= ~OBJECT_SELECTED;
			memmove(NewComp, Comp, MemSize);

			if ((Mode & 2) == 0)
				NewComp->CompNamePenThickNess = (float) NewValue.Value;
			else
				NewComp->CompValuePenThickNess = (float) NewValue.Value;

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AssignSelectedSpecialObjectsToNet(int32 mode)
{

	int32 cnt, TempLastActionNr, NetNr;
	int32 Changed;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	ObjectRecord NewObject;

	NetNr = -1;

	if (mode == 0)
	{
		NetNr = SelectNetDialog();

		if (NetNr == -1)
			return -1;
	}

	TempLastActionNr = (int16) LastActionNr - 1;
	Changed = 0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr) && (ObjectLine->Layer < 32))
		{
			ObjectLine->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));

			if (mode == 1)
				NewObjectLine.NetNr = -1;
			else
				NewObjectLine.NetNr = (int16) NetNr;

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
		        && (ObjectRect->AddNr <= TempLastActionNr) && (ObjectRect->Layer < 32))
		{
			ObjectRect->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));

			if (mode == 1)
				NewObjectRect.NetNr = -1;
			else
				NewObjectRect.NetNr = (int16) NetNr;

			NewObjectRect.Info &= OBJECT_FILLED;

			if (AddObjectRect(&NewObjectRect))
			{
				ObjectRect = &((*ObjectRects)[cnt]);
				ObjectRect->Info |= OBJECT_NOT_VISIBLE;
				ObjectRect->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr) && ((ObjectArc->Layer < 32) || (ObjectArc->Layer == DRILL_LAYER)))
		{
			ObjectArc->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			NewObjectArc.Info &= OBJECT_FILLED;
			NewObjectArc.Info = 0;

			if (mode == 1)
				NewObjectArc.NetNr = -1;
			else
				NewObjectArc.NetNr = (int16) NetNr;

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;

				if ((NewObjectArc.Layer == DRILL_UNPLATED_LAYER) || (NewObjectArc.Layer == DRILL_LAYER))
				{
					if (NewObjectArc.Layer == DRILL_UNPLATED_LAYER)
						NewObject.ObjectType = DRILL_UNPLATED;
					else
						NewObject.ObjectType = DRILL;

					NewObject.x1 = NewObjectArc.CentreX;
					NewObject.y1 = NewObjectArc.CentreY;
					NewObject.x2 = NewObjectArc.Width;
					NewObject.y2 = 0.0;
					NewObject.Layer = -1;

					if (RecalcAreafillAfterInsert)
					{
						if (InsertObjectInAreaFill(&NewObject, -1, -1, 2) == 1)
						{
							if (OkToDrawAreaFills)
								RePaint();
						}
					}
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr) && (ObjectPolygon->Layer < 32))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectPolygon, ObjectPolygon, sizeof(ObjectPolygonRecord));

			if (mode == 1)
				NewObjectPolygon.NetNr = -1;
			else
				NewObjectPolygon.NetNr = (int16) NetNr;

			if (AddObjectPolygon(&NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (mode == 1)
	{
		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (ObjectText2->AddNr <= TempLastActionNr) && (ObjectText2->Layer < 32))
			{
				ObjectText2->Info &= ~OBJECT_SELECTED;
				memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
				NewObjectText2.NetNr = -1;
				NewObjectText2.Info &= OBJECT_FILLED;

				if (AddObjectText2(&NewObjectText2))
				{
					ObjectText2 = &((*ObjectTexts2)[cnt]);
					ObjectText2->Info |= OBJECT_NOT_VISIBLE;
					ObjectText2->DeleteNr = (int16) LastActionNr;
					Changed = 1;
				}
			}
		}
	}

	RePaint();
	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 EditSchematicComponent(int32 mode)
{
	int32 cnt;
	CompRecord *Comp;
	HWND DesignWindow;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if ((ProjectInfo) && (DesignWindow = GetDesignManagersWindow(0)))
			{
				memset(&ProjectInfo->TempStr1, 0, sizeof(ProjectInfo->TempStr1));
				strncpy(ProjectInfo->TempStr1, Comp->Name, sizeof(ProjectInfo->TempStr1) - 1);
				SendMessage(DesignWindow, WM_COMMAND, ID_SHEET_OPEN_REF, 1);
			}

			return 0;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SearchForComponents(LPSTR SearchString, int32 mode)
{
	int32 cnt;
	CompRecord *Comp;
	ObjectTextRecord2 ReferenceText;

	if (mode & 32)
	{
		memset(&ReferenceText, 0, sizeof(ObjectTextRecord));

		switch (mode & 15)
		{
		case 0:
			if (mode & 16)
			{
				if (CompRefDialog(&ReferenceText, SC(1248, "Select components on partnr")) != 1)
					return -1;
			}
			else
			{
				if (CompRefDialog(&ReferenceText, SC(1249, "Deselect components on partnr")) != 1)
					return -1;
			}

			SearchString = (LPSTR) & ReferenceText.Text;
			break;

		case 1:
			if (mode & 16)
			{
				if (CompRefDialog(&ReferenceText, SC(1250, "Select components on geometry")) != 1)
					return -1;
			}
			else
			{
				if (CompRefDialog(&ReferenceText, SC(1251, "Deselect components on geometry")) != 1)
					return -1;
			}

			SearchString = (LPSTR) & ReferenceText.Text;
			break;

		case 2:
			if (mode & 16)
			{
				if (CompRefDialog(&ReferenceText, SC(1252, "Select components on value")) != 1)
					return -1;
			}
			else
			{
				if (CompRefDialog(&ReferenceText, SC(1253, "Deselect components on value")) != 1)
					return -1;
			}

			SearchString = (LPSTR) & ReferenceText.Text;
			break;
		}
	}
	else
	{
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			{
				switch (mode & 15)
				{
				case 0:
					SearchString = (LPSTR) & Comp->PartNr;
					break;

				case 1:
					SearchString = (LPSTR) & Comp->ShapeName;
					break;

				case 2:
					SearchString = (LPSTR) & Comp->Value;
					break;
				}
			}
		}
	}

	if (!SearchString)
	{
		ok = 1;
		return 0;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			switch (mode & 15)
			{
			case 0:
				if (stricmpUTF8(Comp->PartNr, SearchString) == 0)
				{
					if (mode & 16)
						Comp->Info |= OBJECT_SELECTED;
					else
						Comp->Info &= ~OBJECT_SELECTED;
				}

				break;

			case 1:
				if (stricmpUTF8(Comp->ShapeName, SearchString) == 0)
				{
					if (mode & 16)
						Comp->Info |= OBJECT_SELECTED;
					else
						Comp->Info &= ~OBJECT_SELECTED;
				}

				break;

			case 2:
				if (stricmpUTF8(Comp->Value, SearchString) == 0)
				{
					if (mode & 16)
						Comp->Info |= OBJECT_SELECTED;
					else
						Comp->Info &= ~OBJECT_SELECTED;
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

int32 ChangeNetAreaFills(int32 mode)
{
	int32 NetNr, cnt;
	AreaFillRecord *AreaFill;
	PolygonRecord *FirstPolygon;

	NetNr = SelectNetDialog();

	if (NetNr == -1)
		return -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0))
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((FirstPolygon->PolygonType & 2) == 2)
			{
				if (AllocateMemAreaFillMemoryTemp(AreaFill->MemSize + 3172) != 0)
					return -1;

				NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
				TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
				memmove(NewAreaFill, AreaFill, AreaFill->MemSize);
				NewAreaFill->NetNr = (int16) NetNr;

				if (AddAreaFill(0))
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
					AreaFill->Info |= OBJECT_NOT_VISIBLE;
					AreaFill->DeleteNr = (int16) LastActionNr;
				}
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

int32 ChangeNetTracesVias(int32 mode)
{
	int32 cnt, NetNr, Layer, TempLastActionNr, TraceInfo;
	ViaRecord *Via, NewVia;
	TraceRecord *Trace;
	ObjectRecord TraceObject;

	memset(&TraceObject, 0, sizeof(TraceObject));
	NetNr = SelectNetDialog();

	if (NetNr == -1)
		return -1;

	TempLastActionNr = (int16) LastActionNr - 1;

	for (Layer = 0; Layer < 32; Layer++)
	{
		DrawCode = DrawLayerCode[Layer];

		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				TraceObject.Layer = Layer;
				TraceObject.x1 = Trace->X;
				TraceObject.y1 = Trace->Y;
				TraceObject.x2 = Trace->Length;
				TraceObject.y2 = Trace->ThickNess;
				TraceObject.NetNr = NetNr;
				TraceObject.Clearance = Trace->Clearance;
				TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
				TraceObject.ObjectType = TRACE_VER;

				if (AddTrace(&TraceObject))
				{
					Trace = &((*VerTraces[Layer])[cnt]);
					Trace->Info &= ~OBJECT_SELECTED;
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16) LastActionNr;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				TraceObject.Layer = Layer;
				TraceObject.x1 = Trace->X;
				TraceObject.y1 = Trace->Y;
				TraceObject.x2 = Trace->Length;
				TraceObject.y2 = Trace->ThickNess;
				TraceObject.NetNr = NetNr;
				TraceObject.Clearance = Trace->Clearance;
				TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
				TraceObject.ObjectType = TRACE_HOR;

				if (AddTrace(&TraceObject))
				{
					Trace = &((*HorTraces[Layer])[cnt]);
					Trace->Info &= ~OBJECT_SELECTED;
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16) LastActionNr;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				TraceObject.Layer = Layer;
				TraceObject.x1 = Trace->X;
				TraceObject.y1 = Trace->Y;
				TraceObject.x2 = Trace->Length;
				TraceObject.y2 = Trace->ThickNess;
				TraceObject.NetNr = NetNr;
				TraceObject.Clearance = Trace->Clearance;
				TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
				TraceObject.TraceNr = cnt;
				TraceObject.ObjectType = TRACE_DIAG1;

				if (AddTrace(&TraceObject))
				{
					Trace = &((*Diag1Traces[Layer])[cnt]);
					Trace->Info &= ~OBJECT_SELECTED;
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16) LastActionNr;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			TraceInfo = Trace->Info;

			if (((TraceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Trace->AddNr <= TempLastActionNr))
			{
				TraceObject.Layer = Layer;
				TraceObject.x1 = Trace->X;
				TraceObject.y1 = Trace->Y;
				TraceObject.x2 = Trace->Length;
				TraceObject.y2 = Trace->ThickNess;
				TraceObject.NetNr = NetNr;
				TraceObject.Clearance = Trace->Clearance;
				TraceObject.Info = Trace->Info & OBJECT_HIGHLITED;
				TraceObject.TraceNr = cnt;
				TraceObject.ObjectType = TRACE_DIAG2;

				if (AddTrace(&TraceObject))
				{
					Trace = &((*Diag2Traces[Layer])[cnt]);
					Trace->Info &= ~OBJECT_SELECTED;
					Trace->Info |= OBJECT_NOT_VISIBLE;
					Trace->DeleteNr = (int16) LastActionNr;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if (((Via->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Via->AddNr <= TempLastActionNr) && (Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
		{
			memcpy(&NewVia, Via, sizeof(ViaRecord));
			NewVia.Info &= ~OBJECT_SELECTED;
			NewVia.NetNr = (int16) NetNr;
			AddVia(&NewVia);
			Via = &((*Vias)[cnt]);
			Via->Info |= OBJECT_NOT_VISIBLE;
			Via->DeleteNr = (int16) LastActionNr;
		}
	}

	RePaint();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
