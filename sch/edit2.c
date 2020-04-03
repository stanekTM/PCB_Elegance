/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edit2.c
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
#include "resource.h"
#include "graphics.h"
#include "string.h"
#include "toets.h"
#include "mainloop.h"
#include "edit.h"
#include "edit2.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "sch.h"
#include "calc.h"
#include "math.h"
#include "calc2.h"
#include "calcdef.h"
#include "calcrect.h"
#include "insdel.h"
#include "files2.h"
#include "files.h"
#include "dialogs.h"
#include "insdel.h"
#include "help.h"
#include "utf8.h"


#define  IsWordChar(c)   ( ( ((c>='a') && (c<='z')) || ((c>='A') && (c<='Z')) || \
                             ((c>='0') && (c<='9')) || (c=='_')) ? (1) : (0) )

int32 EscapeInsertSymbol, InstCnt;
PinRecord NewPins[200];
ObjectTextRecord NewPinText[200];
char TextObjectsFile[MAX_LENGTH_STRING];

extern PinRecord AddedPins[200];
extern PowerPinRecord AddedPowerPin;
extern PinBusRecord AddedPinBus;
extern ObjectTextRecord AddedPinText[200];
extern int32 NrAddPins, NrTryingSymbols;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawTryingAddPins(double CurrentX, double CurrentY, int32 Mode)
{
	int32 cnt;
	double hulp;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	memmove(&NewPins, &AddedPins, (int) NrAddPins * sizeof(PinRecord));
	memmove(&NewPinText, &AddedPinText, (int) NrAddPins * sizeof(ObjectTextRecord));

	switch (Mode)
	{
	case 1:
		for (cnt = 0; cnt < NrAddPins; cnt++)
		{
			NewPins[cnt].X *= -1.0;
			NewPins[cnt].NameX *= -1.0;
			NewPins[cnt].NameInfo = TextMirrorX[NewPins[cnt].NameInfo & 0x0f];
			NewPinText[cnt].X *= -1.0;
			NewPinText[cnt].TextMode = TextMirrorX[NewPinText[cnt].TextMode & 0x0f];
		}

		break;

	case 3:
		for (cnt = 0; cnt < NrAddPins; cnt++)
		{
			hulp = NewPins[cnt].X;
			NewPins[cnt].X = -NewPins[cnt].Y;
			NewPins[cnt].Y = (float) -hulp;
			hulp = NewPins[cnt].NameX;
			NewPins[cnt].NameX = -NewPins[cnt].NameY;
			NewPins[cnt].NameY = (int16) - hulp;
			NewPins[cnt].NameInfo = (int16) ((TextMirrorX[NewPins[cnt].NameInfo & 0x0f]) + (1 << 8));
			hulp = NewPinText[cnt].X;
			NewPinText[cnt].X = -NewPinText[cnt].Y;
			NewPinText[cnt].Y = (float) -hulp;
			NewPinText[cnt].TextMode = (int16) ((TextMirrorX[NewPinText[cnt].TextMode & 0x0f]) + (1 << 8));
			NewPinText[cnt].Rotation = 90.0;
//        NewPinText[cnt].TextMode=TextMirrorX[NewPinText[cnt].TextMode & 0x0f]+(NewPinText[cnt].TextMode & 0xfff0);
		}

		break;

	case 2:
		for (cnt = 0; cnt < NrAddPins; cnt++)
		{
			hulp = NewPins[cnt].X;
			NewPins[cnt].X = -NewPins[cnt].Y;
			NewPins[cnt].Y = (float) hulp;
			hulp = NewPins[cnt].NameX;
			NewPins[cnt].NameX = -NewPins[cnt].NameY;
			NewPins[cnt].NameY = (float) hulp;
			NewPins[cnt].NameInfo = (int16) (NewPins[cnt].NameInfo + (1 << 8));
//        NewPins[cnt].NameInfo=(TextMirrorX[NewPins[cnt].NameInfo & 0x0f])+(1 << 8);
			hulp = NewPinText[cnt].X;
			NewPinText[cnt].X = -NewPinText[cnt].Y;
			NewPinText[cnt].Y = (float) hulp;
			NewPinText[cnt].TextMode = (int16) (NewPinText[cnt].TextMode + (1 << 8));
			NewPinText[cnt].Rotation = 90.0;
//        NewPinText[cnt].TextMode=(TextMirrorX[NewPinText[cnt].TextMode & 0x0f])+(1 << 8);
		}

		break;
	}


	for (cnt = 0; cnt < NrAddPins; cnt++)
	{
		NewPins[cnt].X += (float) CurrentX;
		NewPins[cnt].Y += (float) CurrentY;
		NewPins[cnt].NameX += (float) CurrentX;
		NewPins[cnt].NameY += (float) CurrentY;

		DrawPin(&NewPins[cnt], 0.0, 0.0, 0);

		NewPinText[cnt].X += (float) CurrentX;
		NewPinText[cnt].Y += (float) CurrentY;

		if (NewPinText[cnt].Text[0] != 0)
			DrawObjectText(&NewPinText[cnt], 0.0, 0.0, 0);
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingPins()
{
	int32 cnt;

	for (cnt = 0; cnt < NrAddPins; cnt++)
	{
		NewPins[cnt].Info = 0;
		AddPin(&NewPins[cnt]);
	}

	for (cnt = 0; cnt < NrAddPins; cnt++)
	{
		NewPinText[cnt].Info = 0;

		if (NewPinText[cnt].Text[0] != 0)
			AddObjectText(&NewPinText[cnt]);
	}

	DrawCrossHair(2);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddPins(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

//  memset(&NewPinText,0,sizeof(NewPinText));
//  NewObjectText.FontHeight=1.0;

	if (!EditingSheetSymbol)
	{
		if (AddPinsDialog(&AddedPins[0], 0) == 2)
			return;
	}
	else
	{
		if (AddPinsDialog(&AddedPins[0], 2) == 2)
			return;
	}

	if (NrAddPins == 0)
		return;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	Mode = 0;

	CheckInputMessages(0);
//  while (LeftButtonPressed) CheckInputMessages(0) ;
	ClipMouseCursor();
	DrawTryingAddPins(CurrentX, CurrentY, Mode);

	SystemBusyMode = 10;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingAddPins;
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
				DrawTryingAddPins(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingAddPins(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingAddPins(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingAddPins(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingAddPins(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPins(CurrentX, CurrentY, Mode);
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
			DrawTryingAddPins(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingAddPins(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingAddPins(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingAddPins(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingAddPins(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingAddPins(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingAddPins(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingAddPins(CurrentX, CurrentY, Mode);
			CommandAddTryingPins();
			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingAddPins(CurrentX, CurrentY, Mode);
			Mode = (Mode + 1) & 3;
//      RightButtonPressed=0;
			CheckInputMessages(0);
			DrawTryingAddPins(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingAddPins(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				if (!EditingSheetSymbol)
					Help("add_pin.htm", 0);
				else
					Help("add_sheetsymbolpin.htm", 0);

				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
			}

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			ClipMouseCursor();

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingAddPins(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingAddPins(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingAddPowerPins(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	memmove(&NewPowerPin, &AddedPowerPin, sizeof(PowerPinRecord));

	if (Mode == 1)
		NewPowerPin.NameInfo = (int16) ((NewPowerPin.NameInfo & 0x0f) + (1 << 8));

	NewPowerPin.NameX += (float) CurrentX;
	NewPowerPin.NameY += (float) CurrentY;
	DrawPowerPin(&NewPowerPin, 0.0, 0.0, 0);
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

void CommandAddTryingPowerPins()
{
	NewPowerPin.Info = 0;
	AddPowerPin(&NewPowerPin);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddPowerPins(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	if (EditingSheetSymbol)
	{
		MessageBoxUTF8(SCHWindow, SC(272, "Can not add powerpins on a sheetsymbol"), SC(38, "Error"), MB_OK);
		return;
	}

	if (AddPowerPinsDialog(&AddedPowerPin, 0) == 2)
		return;

//  TextInputDialog("Add Text",&ObjectTextLine);
//  if (strlen(&ObjectTextLine)==0) return;
//  memmove(&NewObjectText.Text,&ObjectTextLine,31);
//  NewObjectText.Text[31]=0;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	Mode = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);

	SystemBusyMode = 11;

	DrawXorFunction.Function1 = (FUNCP1) DrawTryingAddPowerPins;
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
				DrawTryingAddPowerPins(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingAddPowerPins(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingAddPowerPins(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingAddPowerPins(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + 40 + ClientStartY);
				DrawTryingAddPowerPins(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
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
			DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingAddPowerPins(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingAddPowerPins(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingAddPowerPins(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
			CommandAddTryingPowerPins();
			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
			Mode = (Mode + 1) & 1;
//      RightButtonPressed=0;
			CheckInputMessages(0);
			DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingAddPowerPins(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("add_powerpin.htm", 0);
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
				DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingAddPowerPins(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawTryingAddPinBus(double CurrentX, double CurrentY, int32 Mode)
{
	double hulp;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	memmove(&NewPinBus, &AddedPinBus, sizeof(PinBusRecord));

	memmove(&NewObjectText, &AddedPinText[0], sizeof(ObjectTextRecord));

	switch (Mode)
	{
	case 1:
		NewPinBus.X *= -1.0;
		NewPinBus.NameX *= (float) -1.0;
		NewPinBus.NameInfo = TextMirrorX[NewPinBus.NameInfo & 0x0f];
		NewObjectText.X *= (float) -1.0;
		NewObjectText.TextMode = TextMirrorX[NewObjectText.TextMode & 0x0f];
		break;

	case 2:
		hulp = NewPinBus.X;
		NewPinBus.X = NewPinBus.Y;
		NewPinBus.Y = (float) -hulp;
		hulp = NewPinBus.NameX;
		NewPinBus.NameX = -NewPinBus.NameY;
		NewPinBus.NameY = (float) -hulp;
		NewPinBus.NameInfo = (int16) ((TextMirrorX[NewPinBus.NameInfo & 0x0f]) + (1 << 8));
		hulp = NewObjectText.X;
		NewObjectText.X = NewObjectText.Y;
		NewObjectText.Y = (float) -hulp;
		NewObjectText.TextMode = (int16) ((TextMirrorX[NewObjectText.TextMode & 0x0f]) + (1 << 8));
		break;

	case 3:
		hulp = NewPinBus.X;
		NewPinBus.X = -NewPinBus.Y;
		NewPinBus.Y = (float) hulp;
		hulp = NewPinBus.NameX;
		NewPinBus.NameX = -NewPinBus.NameY;
		NewPinBus.NameY = (float) hulp;
		NewPinBus.NameInfo = (int16) (NewPinBus.NameInfo + (1 << 8));
//        NewPinBus.NameInfo=(TextMirrorX[NewPinBus.NameInfo & 0x0f])+(1 << 8);
		hulp = NewObjectText.X;
		NewObjectText.X = -NewObjectText.Y;
		NewObjectText.Y = (float) hulp;
		NewObjectText.TextMode = (int16) (NewObjectText.TextMode + (1 << 8));
//        NewObjectText.TextMode=(TextMirrorX[NewObjectText.TextMode & 0x0f])+(1 << 8);
		break;
	}

	NewPinBus.X += (float) CurrentX;
	NewPinBus.Y += (float) CurrentY;
	NewPinBus.NameX += (float) CurrentX;
	NewPinBus.NameY += (float) CurrentY;
	DrawPinBus(&NewPinBus, 0.0, 0.0, 0);

	NewObjectText.X += (float) CurrentX;
	NewObjectText.Y += (float) CurrentY;

	DrawObjectText(&NewObjectText, 0.0, 0.0, 0);
	DrawCrossHair(8);

	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingPinBus()
{
	NewPinBus.Info = 0;
	AddPinBus(&NewPinBus);

	NewObjectText.Info = 0;
	AddObjectText(&NewObjectText);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddPinBus(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

//  sprintf(&AddedPinBus.Text,"123\\45");
//  sprintf(&AddedPinBus.Label,"hallo");
	if (EditingSheetSymbol)
	{
		MessageBoxUTF8(SCHWindow, SC(273, "Can not add a pinbus on a sheetsymbol"), SC(38, "Error"), MB_OK);
		return;
	}

	if (AddPinBusDialog(&AddedPinBus, 0) == 2)
		return;

//  TextInputDialog("Add Text",&ObjectTextLine);
//  if (strlen(&ObjectTextLine)==0) return;
//  memmove(&NewObjectText.Text,&ObjectTextLine,31);
//  NewObjectText.Text[31]=0;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	Mode = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingAddPinBus(CurrentX, CurrentY, Mode);

	SystemBusyMode = 12;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingAddPinBus;
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
				DrawTryingAddPinBus(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingAddPinBus(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingAddPinBus(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingAddPinBus(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingAddPinBus(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
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
			DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingAddPinBus(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingAddPinBus(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingAddPinBus(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
			CommandAddTryingPinBus();
			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
			Mode = (Mode + 1) & 3;
//      RightButtonPressed=0;
			CheckInputMessages(0);
			DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingAddPinBus(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("add_pinbus.htm", 0);
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
				DrawTryingAddPinBus(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingAddPinBus(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

#ifndef GCC_COMP

#pragma auto_inline(off)

#endif

void DrawTryingInstance(double CurrentX, double CurrentY, int32 Mode)
{
	int32 Mode2;

	Mode2 = Mode;
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	switch (Mode)
	{
	case 0:
		NewInstance.SymbolInfo = 0;	// Normal
		break;

	case 1:
		NewInstance.SymbolInfo = OBJECT_MIRRORX;	// Mirror X
		break;

	case 2:
		NewInstance.SymbolInfo = OBJECT_ROTATE90;	// Rotate 90.0
		break;

	case 3:
		NewInstance.SymbolInfo = OBJECT_MIRRORX + OBJECT_ROTATE90;	// Mirror X and rotate 90.0
		break;

	case 4:
		NewInstance.SymbolInfo = OBJECT_MIRRORY;	// Mirror Y
		break;

	case 5:
		NewInstance.SymbolInfo = OBJECT_MIRRORX + OBJECT_MIRRORY;	// Mirror Y and Mirror X
		break;
	}

	NewInstance.OriginX = (float) (CurrentX - CentreSelectedX);
	NewInstance.OriginY = (float) (CurrentY - CentreSelectedY);
	DrawInstance(&NewInstance, 0.0, 0.0, 1);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

#ifndef GCC_COMP

#pragma auto_inline(on)

#endif

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingInstance()
{
	AddInstance(&NewInstance);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddInstance(int32 Mode)
{
	int32 ok;
	double OldX, OldY, CurrentX, CurrentY, ShiftX, ShiftY;
	int32 FirstShift;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

//  memset(&NewInstance,0,sizeof(InstanceRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CentreSelectedX = 0.0;
	CentreSelectedY = 0.0;
//  CentreSelectedX=CurrentX;
//  CentreSelectedY=CurrentY;

	ShiftX = 0.0;
	ShiftY = 0.0;
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	Mode = 0;


	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingInstance(CurrentX, CurrentY, Mode);
	FirstShift = 1;


	SystemBusyMode = 13;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingInstance;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	ZoomInOutProcessed = 0;

	while ((!SelectionEsc) && (!EscapeInsertSymbol))
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawTryingInstance(OldX, OldY, Mode);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawTryingInstance(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingInstance(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingInstance(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingInstance(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingInstance(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingInstance(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingInstance(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + 40 + ClientStartY);
				DrawTryingInstance(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingInstance(CurrentX, CurrentY, Mode);
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
			DrawTryingInstance(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			if (NrTryingSymbols > 0)
				ok = 1;

			DrawTryingInstance(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingInstance(CurrentX, CurrentY, Mode);
			else
			{
				if (NrTryingSymbols > 0)
				{
					EscapeInsertSymbol = 1;
					DisplayObjectOnEscape = 0;
//          Beep(1000,500);
				}
			}
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingInstance(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingInstance(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingInstance(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingInstance(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingInstance(CurrentX, CurrentY, Mode);
			CommandAddTryingInstance();
			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingInstance(OldX, OldY, Mode);
			Mode = (Mode + 1) & 7;

			if (Mode == 6)
				Mode = 0;

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingInstance(CurrentX, CurrentY, Mode);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingInstance(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("dd_symbol.htm", 0);
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

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
				DrawTryingInstance(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();
//  if (DisplayObjectOnEscape) DrawTryingInstance(CurrentX,CurrentY,Mode);
	EscapeInsertSymbol = 0;
	SystemBusyMode = 0;
//  RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetSymbol(LPSTR SymbolName, LPSTR SymbolDir, LPSTR LibName, int32 Mode)
{
	int32 res, res2, result, Libfp, Symbolfp, cnt, NrLibEntries, SymbolPosition, SymbolLength, pos, SymbolNr, MemPos;
	LibRecord Lib;
	LibNameRecord LibEntry;
	SymbolRecord *LoadSymbol, NewSymbol;
	char str[MAX_LENGTH_STRING], SymbolFile[MAX_LENGTH_STRING], TestSymbolName[MAX_LENGTH_STRING];
	HGLOBAL SymbolGlobal;
	SymbolsPosRecord *SymbolPos;
//	int32 res2;

	SymbolLength = 1000000;
	res = 0;
	SymbolGlobal = NULL;
	LoadSymbol = NULL;
	SymbolNr = -1;
	SymbolPosition = -1;
	strcpy(TestSymbolName, SymbolName);

	for (cnt = 0; cnt < Design.NrSymbols; cnt++)
	{
		SymbolPos = &((*SymbolsPos)[cnt]);

		if (stricmp(SymbolPos->SymbolName, TestSymbolName) == 0)
			SymbolNr = cnt;
	}

	if (SymbolNr != -1)
	{
		MemPos = (*SymbolsPos)[SymbolNr].Pos;
		LoadSymbol = (SymbolRecord *) & (SymbolsMem[MemPos]);
	}
	else
	{

// *******************************************************************************
// If symbol not available in memory search for the symbol
// *******************************************************************************

		if (SymbolDir[0] == 0)
		{

// *******************************************************************************
// Insert symbol from a library
// *******************************************************************************

			if ((Libfp = FileOpenReadOnlyUTF8(LibName)) < 0)
			{
				MessageBoxUTF8(SCHWindow, LibName, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
				return 0;
			}

			if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
			{
				MessageBoxUTF8(SCHWindow, LibName, SC(274, "Error in reading file"), MB_APPLMODAL | MB_OK);
				FileClose(Libfp);
				return 0;
			}

			if (strcmp(Lib.Identification, LibraryCode1) != 0)
			{
				MessageBoxUTF8(SCHWindow, LibName, SC(275, "File is not a library"), MB_APPLMODAL | MB_OK);
				FileClose(Libfp);
				return 0;
			}

			NrLibEntries = Lib.NrLibEntries;
			cnt = 0;

			while ((cnt < NrLibEntries) && ((res = FileRead(Libfp, &LibEntry, sizeof(LibNameRecord), &result)) == 0))
			{
				if (stricmpUTF8(SymbolName, LibEntry.Text) == 0)
				{
					SymbolPosition = LibEntry.Pos;
					SymbolLength = LibEntry.Length;
				}

				cnt++;
			}

			if (res == -1)
			{
				MessageBoxUTF8(SCHWindow, LibName, SC(274, "Error in reading file"), MB_APPLMODAL | MB_OK);
				FileClose(Libfp);
				return 0;
			}

			if ((SymbolPosition != -1) && (SymbolLength < 1000000))
			{
				if (FileSeek(Libfp, SymbolPosition) >= 0)
				{
					pos = FileCurrentPointer(Libfp);

					if (((SymbolGlobal = GlobalAlloc(GHND, SymbolLength)) != NULL)
					        && ((LoadSymbol = (SymbolRecord *) GlobalLock(SymbolGlobal)) != NULL))
					{
						if (((res = FileRead(Libfp, LoadSymbol, (int) SymbolLength, &result)) == -1)
						        || (LoadSymbol->MemSize > SymbolLength))
							LoadSymbol = NULL;
					}
				}
				else
				{
					MessageBoxUTF8(SCHWindow, LibName, SC(274, "Error in reading file"), MB_APPLMODAL | MB_OK);
					FileClose(Libfp);
					return 0;
				}
			}

			FileClose(Libfp);

// *******************************************************************************
// Insert symbol from a symbol file
// *******************************************************************************

		}
		else
		{
			strcpy(SymbolFile, SymbolDir);

//      AddBackSlash(SymbolFile);
//      strcat(SymbolFile,SymbolName);
//      strcat(SymbolFile,".sym");
			if ((Symbolfp = FileOpenReadOnlyUTF8(SymbolFile)) < 0)
			{
				MessageBoxUTF8(SCHWindow, SymbolFile, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
				return 0;
			}

			if (FileRead(Symbolfp, &NewSymbol, sizeof(SymbolRecord), &result) == -1)
			{
				MessageBoxUTF8(SCHWindow, SymbolFile, SC(274, "Error in reading file"), MB_APPLMODAL | MB_OK);
				FileClose(Symbolfp);
				return 0;
			}

			SymbolLength = NewSymbol.MemSize;

			if ((strcmp(NewSymbol.SymbolIdent, SymbolCode1) != 0) && (strcmp(NewSymbol.SymbolIdent, SymbolCode2) != 0)
			        && (strcmp(NewSymbol.SymbolIdent, SymbolCode3) != 0))
			{
				strcpy(str, SC(276, "File "));
				strcat(str, SymbolFile);
				strcat(str, SC(277, " is not a symbol file"));
				MessageBoxUTF8(SCHWindow, str, "", MB_APPLMODAL | MB_OK);
				FileClose(Symbolfp);
				return 0;
			}

			if (NewSymbol.MemSize > 1000000)
			{
				strcpy(str, SC(278, "Symbol size is >1000000"));
				MessageBoxUTF8(SCHWindow, str, "", MB_APPLMODAL | MB_OK);
				FileClose(Symbolfp);
				return 0;
			}

			strcpy(TestSymbolName, NewSymbol.Name);

			if (((SymbolGlobal = GlobalAlloc(GHND, SymbolLength)) != NULL)
			        && ((LoadSymbol = (SymbolRecord *) GlobalLock(SymbolGlobal)) != NULL))
			{
				if ((FileSeek(Symbolfp, 0) == -1)
				        || (FileRead(Symbolfp, LoadSymbol, (int) SymbolLength, &result) == -1))
				{
					MessageBoxUTF8(SCHWindow, SymbolFile, SC(274, "Error in reading file"), MB_APPLMODAL | MB_OK);
					LoadSymbol = NULL;
				}
			}

			FileClose(Symbolfp);
		}

// *******************************************************************************
// Insert symbol from a library/symbol file
// *******************************************************************************

		if (LoadSymbol == NULL)
		{
			if (SymbolGlobal != NULL)
			{
				GlobalUnlock(SymbolGlobal);
				GlobalFree(SymbolGlobal);
			}

			return 0;
		}

		memset(LoadSymbol->Name, 0, sizeof(LoadSymbol->Name));
		strcpy(LoadSymbol->Name, SymbolName);
		res2 = AddSymbol(LoadSymbol);
		MemPos = (*SymbolsPos)[Design.NrSymbols - 1].Pos;
		LoadSymbol = (SymbolRecord *) & (SymbolsMem[MemPos]);

		if (SymbolGlobal != NULL)
		{
			GlobalUnlock(SymbolGlobal);
			GlobalFree(SymbolGlobal);
		}

		if (!res2)
			return 0;
	}

// *******************************************************************************
// Insert instance
// *******************************************************************************


	if (LoadSymbol == NULL)
	{
		if (SymbolGlobal != NULL)
		{
			GlobalUnlock(SymbolGlobal);
			GlobalFree(SymbolGlobal);
		}

		return 0;
	}

	if ((Mode & 1) == 0)
	{
		memset(&NewInstance, 0, sizeof(InstanceRecord));

		if (SymbolDir[0] == 0)
			memmove(NewInstance.Value, SymbolName, 31);
		else
			memmove(NewInstance.Value, LoadSymbol->Name, 31);
	}

	if ((Mode & 2) == 2)
		return 1;

	memmove(NewInstance.SymbolName, LoadSymbol->Name, 31);
	memmove(NewInstance.Reference, LoadSymbol->InitialReference, 7);
	SearchMinX = NewInstance.OriginX;
	SearchMinY = NewInstance.OriginY;
	SearchMaxX = NewInstance.OriginX;
	SearchMaxY = NewInstance.OriginY;

	NewInstance.RefOriginX = LoadSymbol->RefOriginX;
	NewInstance.RefOriginY = LoadSymbol->RefOriginY;
	NewInstance.ValueOriginX = LoadSymbol->ValueOriginX;
	NewInstance.ValueOriginY = LoadSymbol->ValueOriginY;
	NewInstance.Info = 0;

	if ((LoadSymbol->Info & 1) == 1)
		NewInstance.ValueInfo |= TEXT_NOT_VISIBLE;

	if ((LoadSymbol->Info & 2) == 2)
		NewInstance.RefInfo |= TEXT_NOT_VISIBLE;

	if (LoadSymbol->NrPartsPerPackage > 1)
		NewInstance.PackagePartNr = 1;

	if ((LoadSymbol->Info & OBJECT_PROTECTED) == OBJECT_PROTECTED)
		NewInstance.Info |= OBJECT_PROTECTED;

	if ((LoadSymbol->Info & SHEET_SYMBOL) == SHEET_SYMBOL)
		NewInstance.Info |= SHEET_SYMBOL;

	NewInstance.NrPins = (int16) LoadSymbol->NrPins;
	NewInstance.PlacingOption = -1;
	InstCnt++;
	CommandAddInstance(0);
	return 1;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 EditInstanceInfo(int32 mode)
{
	int32 cnt, cnt2, res, InstanceInfo, TempLastActionNr, NrInstances =
	    0, NrCompProperties, NrOldCompProperties, Changed =
	        0, Changed2, pos, ChangedCompNetProperties, ChangedCompProperties;
	double x1a, x2a, y1a, y2a;
	InstanceRecord *Instance, *FirstInstance = NULL, ChangedInstance;
	char str2[MAX_LENGTH_STRING], PropertyID[MAX_LENGTH_STRING], PropertyValue[MAX_LENGTH_STRING];

	cnt = 0;
	TempLastActionNr = LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NrInstances++;

			if (!FirstInstance)
				FirstInstance = Instance;
		}
	}

	if ((mode == 0) || (NrInstances == 1))
	{
		cnt = 0;

		while (cnt < Design.NrInstances)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			InstanceInfo = Instance->Info;

			if (((InstanceInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
			        && (Instance->AddNr <= TempLastActionNr))
			{
				if ((InstanceInfo & SHEET_SYMBOL) == 0)
				{
					x1a = Instance->BoardPosMinX;
					y1a = Instance->BoardPosMinY;
					x2a = Instance->BoardPosMaxX;
					y2a = Instance->BoardPosMaxY;
					//      Instance->Info&=~OBJECT_SELECTED;
					memmove(&NewInstance, Instance, sizeof(InstanceRecord));
					//      DrawInstance((int16)cnt,0.0,0.0,0);
					res = InstanceInfoDialog(&NewInstance, NrInstances, 0);
					CheckInputMessages(0);

					switch (res)
					{
					case 1:
						if (AddInstance(&NewInstance))
						{
							Instance = (InstanceRecord *) & ((*Instances)[cnt]);
							Instance->Info |= OBJECT_NOT_VISIBLE;
							Instance->Info &= ~(OBJECT_SELECTED | 7);
							Instance->DeleteNr = (int16) LastActionNr;
							Instance = (InstanceRecord *) & ((*Instances)[Design.NrInstances - 1]);
							Instance->Info &= ~(OBJECT_SELECTED | 7);
							Changed = 1;
						}

						break;

					case 3:
						cnt = Design.NrInstances;
						break;
					}
				}
				else
				{
					PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_FILE_EDIT_SHEET, (LPARAM) NULL);
					return 0;
				}
			}

			cnt++;
		}
	}
	else
	{
// ********************************************************************************************************
// ********************************************************************************************************
		if (NrInstances > 1)
		{
//      memmove(&ChangedInstance,FirstInstance,sizeof(InstanceRecord));
			memset(&ChangedInstance, 0, sizeof(InstanceRecord));
			res = InstanceInfoDialog(&ChangedInstance, NrInstances, 1);
			strcpy(str2, SC(487, "-- unchanged --"));
			NrCompProperties = GetCompProperties(&ChangedInstance, NULL, NULL, 0x40);
			ChangedCompProperties = 1;
			ChangedCompNetProperties = 1;

			for (cnt = 0; cnt < NrCompProperties; cnt++)
			{
				GetCompProperties(&ChangedInstance, PropertyID, PropertyValue, 0x20 + cnt);

				if (strcmp(PropertyID, "MULTI_ASSY") != 0)
				{
					if (PropertyID[0] != '~')
					{
						if ((strcmpUTF8(str2, PropertyID) == 0) && (strcmpUTF8(str2, PropertyValue) == 0))
							ChangedCompProperties = 0;
					}
					else
					{
						if ((strcmpUTF8(str2, &PropertyID[1]) == 0) && (strcmpUTF8(str2, PropertyValue) == 0))
							ChangedCompNetProperties = 0;
					}
				}
			}

			CheckInputMessages(0);

			if (res == 1)
			{
				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt]);

					if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED | SHEET_SYMBOL | OBJECT_SELECTED)) ==
					        OBJECT_SELECTED) && (Instance->AddNr <= TempLastActionNr))
					{
						Changed2 = 0;
						memcpy(&NewInstance, Instance, sizeof(InstanceRecord));

						if (strncmp(ChangedInstance.Reference, str2, 7) != 0)
						{
							memcpy(NewInstance.Reference, ChangedInstance.Reference, sizeof(ChangedInstance.Reference));
							Changed2 = 1;
						}

						if (strcmpUTF8(ChangedInstance.Geometry, str2) != 0)
						{
							memcpy(NewInstance.Geometry, ChangedInstance.Geometry, sizeof(ChangedInstance.Geometry));
							Changed2 = 1;
						}

						if (strcmpUTF8(ChangedInstance.PartDescription, str2) != 0)
						{
							memcpy(NewInstance.PartDescription, ChangedInstance.PartDescription,
							       sizeof(ChangedInstance.PartDescription));
							Changed2 = 1;
						}

						pos = 0;

						if ((ChangedCompProperties) || (ChangedCompNetProperties))
						{
							memset(&NewInstance.Properties, 0, sizeof(ChangedInstance.Properties));
							NrOldCompProperties = GetCompProperties(Instance, NULL, NULL, 0x40);

							if ((ChangedCompProperties) && (!ChangedCompNetProperties))
							{
								for (cnt2 = 0; cnt2 < NrOldCompProperties; cnt2++)
								{
									GetCompProperties(Instance, PropertyID, PropertyValue, 0x20 + cnt2);

									if (PropertyID[0] == '~')
									{	// Copy old comp net properties
										if (pos + strlen(PropertyID) + strlen(PropertyValue) + 3 <
										        sizeof(ChangedInstance.Properties))
										{
											memcpy(&NewInstance.Properties[pos], PropertyID, strlen(PropertyID));
											pos += strlen(PropertyID) + 1;
											memcpy(&NewInstance.Properties[pos], PropertyValue, strlen(PropertyValue));
											pos += strlen(PropertyValue) + 1;
										}
									}
								}

								for (cnt2 = 0; cnt2 < NrCompProperties; cnt2++)
								{
									GetCompProperties(&ChangedInstance, PropertyID, PropertyValue, 0x20 + cnt2);

									if (PropertyID[0] != '~')
									{	// Copy new comp properties
										if (pos + strlen(PropertyID) + strlen(PropertyValue) + 3 <
										        sizeof(ChangedInstance.Properties))
										{
											memcpy(&NewInstance.Properties[pos], PropertyID, strlen(PropertyID));
											pos += strlen(PropertyID) + 1;
											memcpy(&NewInstance.Properties[pos], PropertyValue, strlen(PropertyValue));
											pos += strlen(PropertyValue) + 1;
										}
									}
								}
							}

							if ((!ChangedCompProperties) && (ChangedCompNetProperties))
							{
								for (cnt2 = 0; cnt2 < NrOldCompProperties; cnt2++)
								{
									GetCompProperties(Instance, PropertyID, PropertyValue, 0x20 + cnt2);

									if ((PropertyID[0] != '~')	// Copy old comp properties
									        && ((ChangedInstance.PlacingOption != -1)
									            || (strcmp("MULTI_ASSY", PropertyID) != 0)))
									{
										if (pos + strlen(PropertyID) + strlen(PropertyValue) + 3 <
										        sizeof(ChangedInstance.Properties))
										{
											memcpy(&NewInstance.Properties[pos], PropertyID, strlen(PropertyID));
											pos += strlen(PropertyID) + 1;
											memcpy(&NewInstance.Properties[pos], PropertyValue, strlen(PropertyValue));
											pos += strlen(PropertyValue) + 1;
										}
									}
								}

								for (cnt2 = 0; cnt2 < NrCompProperties; cnt2++)
								{
									GetCompProperties(&ChangedInstance, PropertyID, PropertyValue, 0x20 + cnt2);

									if (PropertyID[0] == '~')
									{	// Copy new comp net properties
										if (pos + strlen(PropertyID) + strlen(PropertyValue) + 3 <
										        sizeof(ChangedInstance.Properties))
										{
											memcpy(&NewInstance.Properties[pos], PropertyID, strlen(PropertyID));
											pos += strlen(PropertyID) + 1;
											memcpy(&NewInstance.Properties[pos], PropertyValue, strlen(PropertyValue));
											pos += strlen(PropertyValue) + 1;
										}
									}
								}
							}

							if ((ChangedCompProperties) && (ChangedCompNetProperties))
							{
								memcpy(&NewInstance.Properties, &ChangedInstance.Properties,
								       sizeof(ChangedInstance.Properties));
							}

							Changed2 = 1;
						}

						if (strcmpUTF8(ChangedInstance.PartNr, str2) != 0)
						{
							memcpy(NewInstance.PartNr, ChangedInstance.PartNr, sizeof(ChangedInstance.PartNr));
							Changed2 = 1;
						}

						if (strcmpUTF8(ChangedInstance.Value, str2) != 0)
						{
							memcpy(NewInstance.Value, ChangedInstance.Value, sizeof(ChangedInstance.Value));
							Changed2 = 1;
						}

						if (ChangedInstance.SymbolInfo & CHANGED_GEOMETRY)
						{
							if ((ChangedInstance.SymbolInfo & NO_GEOMETRY) == 0)
								NewInstance.SymbolInfo &= ~NO_GEOMETRY;
							else
								NewInstance.SymbolInfo |= NO_GEOMETRY;

							Changed2 = 1;
						}

						if (ChangedInstance.SymbolInfo & CHANGED_REFERENCE)
						{
							if ((ChangedInstance.RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
								NewInstance.RefInfo |= TEXT_NOT_VISIBLE;
							else
								NewInstance.RefInfo &= ~TEXT_NOT_VISIBLE;

							Changed2 = 1;
						}

						if (ChangedInstance.SymbolInfo & CHANGED_VALUE)
						{
							if ((ChangedInstance.ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
								NewInstance.ValueInfo |= TEXT_NOT_VISIBLE;
							else
								NewInstance.ValueInfo &= ~TEXT_NOT_VISIBLE;

							Changed2 = 1;
						}

						if (ChangedInstance.SymbolInfo & CHANGED_PLACING_OPTION)
						{
							NewInstance.PlacingOption = ChangedInstance.PlacingOption;
							Changed2 = 1;
						}

						if ((Changed2) && (AddInstance(&NewInstance)))
						{
							Instance = (InstanceRecord *) & ((*Instances)[cnt]);
							Instance->Info |= OBJECT_NOT_VISIBLE;
							Instance->Info &= ~(OBJECT_SELECTED | 7);
							Instance->DeleteNr = (int16) LastActionNr;
							Instance = (InstanceRecord *) & ((*Instances)[Design.NrInstances - 1]);
							Instance->Info &= ~(OBJECT_SELECTED | 7);
							Changed = 1;
						}
					}
				}
			}
		}
	}

	if (Changed)
		RePaint();

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawTryingGlobalConnection(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	NewGlobalConnection.X = (float) CurrentX;
	NewGlobalConnection.Y = (float) CurrentY;
	NewGlobalConnection.NameY = (float) (CurrentY - 0.4);

	if (Mode == 0)
	{
		NewGlobalConnection.ConnectionType &= ~1;
		NewGlobalConnection.NameX = (float) (CurrentX - 1.5);

		if (NewGlobalConnection.ConnectionType == 4)
			NewGlobalConnection.NameX -= (float) 1.5;

		NewGlobalConnection.NameInfo = ALIGN_RIGHT_BOTTOM;
	}
	else
	{
		NewGlobalConnection.ConnectionType |= 1;
		NewGlobalConnection.NameX = (float) (CurrentX + 1.5);

		if (NewGlobalConnection.ConnectionType == 5)
			NewGlobalConnection.NameX += (float) 1.5;

		NewGlobalConnection.NameInfo = 0;
	}

	DrawGlobalConnection(&NewGlobalConnection, 0.0, 0.0, 0);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingGlobalConnection(GlobalConnectionRecord * GlobalConnection)
{
	GlobalConnection->Info = 0;
	AddGlobalConnection(GlobalConnection);
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddGlobalConnection(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;

	memset(&NewGlobalConnection, 0, sizeof(GlobalConnectionRecord));
	NewGlobalConnection.ConnectionType = (int16) (Mode << 1);

	memset(&NewObjectText, 0, sizeof(ObjectTextRecord));
	NewObjectText.FontHeight = (float) 1.0;
	NewObjectText.Thickness = (float) STANDARD_LINE_THICKNESS;

	if (TextInputDialog(&NewObjectText, 4) == 2)
		return;

	if (strlen(NewObjectText.Text) == 0)
		return;

	NewObjectText.Text[31] = 0;

	strcpy(NewGlobalConnection.Text, NewObjectText.Text);

	Mode = 0;
	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);

	DrawXorFunction.Function1 = (FUNCP1) DrawTryingGlobalConnection;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode;
	DrawXorFunction.Mode = 0;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode;
	SystemBusyMode = 14;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingGlobalConnection(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingGlobalConnection(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingGlobalConnection(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingGlobalConnection(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingGlobalConnection(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
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
			DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingGlobalConnection(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingGlobalConnection(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingGlobalConnection(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
			CommandAddTryingGlobalConnection(&NewGlobalConnection);
			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
			Mode ^= 1;
			DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
//      RightButtonPressed=0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingGlobalConnection(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("add_globalconnection.htm", 0);
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
				DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingGlobalConnection(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DrawTryingWireLabels(double CurrentX, double CurrentY, int32 Mode)
{
	WireRecord NewWire;
	NetLabelRecord NewNetLabel;
	int32 cnt;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < NrWireLabels; cnt++)
	{
		memset(&NewWire, 0, sizeof(WireRecord));
		memset(&NewNetLabel, 0, sizeof(NetLabelRecord));
		NewWire.X1 = (float) CurrentX;
		NewWire.Y1 = (float) (CurrentY - cnt);
		NewWire.X2 = (float) (CurrentX + 12.0);
		NewWire.Y2 = (float) (CurrentY - cnt);
		NewNetLabel.ConnectX = NewWire.X1;
		NewNetLabel.ConnectY = NewWire.Y1;
		NewNetLabel.TextX = 3.0;
		NewNetLabel.TextY = 0.0;
		memmove(&NewNetLabel.Name, &WireLabel[cnt], sizeof(WireLabel[cnt]) - 1);

		if (WireLabel[cnt][0] != 0)
		{
			DrawWire(&NewWire, 0.0, 0.0, 0);
			DrawNetLabel(&NewNetLabel, 0.0, 0.0, 0);
		}
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void CommandAddTryingWireLabels(double CurrentX, double CurrentY)
{
	WireRecord NewWire;
	NetLabelRecord NewNetLabel;
	int32 cnt;


	for (cnt = 0; cnt < NrWireLabels; cnt++)
	{
		memset(&NewWire, 0, sizeof(WireRecord));
		memset(&NewNetLabel, 0, sizeof(NetLabelRecord));
		NewWire.X1 = (float) CurrentX;
		NewWire.Y1 = (float) (CurrentY - cnt);
		NewWire.X2 = (float) (CurrentX + 12.0);
		NewWire.Y2 = (float) (CurrentY - cnt);
		NewNetLabel.ConnectX = NewWire.X1;
		NewNetLabel.ConnectY = NewWire.Y1;
		NewNetLabel.TextX = 3.0;
		NewNetLabel.TextY = 0.0;
		memmove(&NewNetLabel.Name, &WireLabel[cnt], sizeof(WireLabel[cnt]) - 1);

		if (WireLabel[cnt][0] != 0)
		{
			if (CommandAddTryingWire(&NewWire, 1))
				AddNetLabel(&NewNetLabel);
		}
	}

	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void AddWireLabel(int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	int32 DisplayObjectOnEscape = 1;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	CurrentDrawMode = 1;


	if (AddWireLabelsDialog() == 2)
		return;

//  TextInputDialog("Add Text",&ObjectTextLine);
//  if (strlen(&ObjectTextLine)==0) return;
//  memmove(&NewObjectText.Text,&ObjectTextLine,31);
//  NewObjectText.Text[31]=0;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	Mode = 0;

	CheckInputMessages(0);
	ClipMouseCursor();
	DrawTryingWireLabels(CurrentX, CurrentY, Mode);
	SystemBusyMode = 15;

	DrawXorFunction.Function1 = (FUNCP1) DrawTryingWireLabels;
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
				DrawTryingWireLabels(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingWireLabels(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawTryingWireLabels(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawTryingWireLabels(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ClientStartY + ScrollSizeDrawing);
				DrawTryingWireLabels(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
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
			DrawTryingWireLabels(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingWireLabels(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingWireLabels(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingWireLabels(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingWireLabels(CurrentX, CurrentY, Mode);
			CommandAddTryingWireLabels(CurrentX, CurrentY);
			CheckInputMessages(0);
			SelectionEsc = 1;
			DisplayObjectOnEscape = 0;
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingWireLabels(CurrentX, CurrentY, Mode);
//      Mode=(Mode + 1) & 1;
			RightButtonPressed = 0;
//      CheckInputMessages(0);
			DrawTryingWireLabels(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingWireLabels(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (HelpAsked)
			{
				Help("add_netlabel_wire.htm", 0);
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
				DrawTryingWireLabels(CurrentX, CurrentY, Mode);
		}
	}

	UnClipMouseCursor();

	if (DisplayObjectOnEscape)
		DrawTryingWireLabels(CurrentX, CurrentY, Mode);

	SystemBusyMode = 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddTextObjectsFromFile(int32 mode)
{
	char str[200], LineBuf[512];
	int32 cnt, fp, Length, Length2, LineNr;
	double SizeX, CurrentX, CurrentY;


	if (GetNewFileUTF8(SCHWindow, NULL, TextObjectsFile, DesignPath, SC(365, "text file"), NULL, SC(253, "Import text from file"), "*", 0) != 0)
		return -1;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX((DrawWindowMaxX - DrawWindowMinX) / 2));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY((DrawWindowMaxY - DrawWindowMinY) / 2));

//  strcpy(TextObjectsFile,"c:\\pcb_elegance\\mach15324\\insert.txt");

	if ((fp = TextFileOpenUTF8(TextObjectsFile)) < 0)
	{
//    MessageBoxUTF8(SCHWindow,FileStr,SC(197,"Error in opening file"),MB_APPLMODAL|MB_OK);
		return 0;
	}

	memset(&NewObjectText, 0, sizeof(NewObjectText));
	NewObjectText.FontHeight = (float) 1.0;
	NewObjectText.Thickness = (float) STANDARD_LINE_THICKNESS;
	NewObjectText.Info = OBJECT_SELECTED | 15;
	NewObjectText.X = (float) CurrentX;
	NewObjectText.Y = (float) CurrentY;
	LineNr = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineNr++;
		LineBuf[Length] = 0;

		if (Length == 0)
		{
			NewObjectText.X = (float) CurrentX;
			NewObjectText.Y -= 1.0;
			continue;
		}

		cnt = 0;

		while ((LineBuf[0] != 0) && (cnt < 128))
		{
			GetString(LineBuf, str);

			if ((str[0] == 0) || (str[0] == ' '))
				break;

			Length2 = min(sizeof(NewObjectText.Text) - 1, strlen(str));
			memset(&NewObjectText.Text, 0, sizeof(NewObjectText.Text));
			strncpy(NewObjectText.Text, str, Length2);
			AddObjectText(&NewObjectText);
			SizeX = ((double) Length2 + 1) * DefFontSize * 0.8 * NewObjectText.FontHeight;
			NewObjectText.X += (float) SizeX;
			cnt++;
		}

		NewObjectText.X = (float) CurrentX;
		NewObjectText.Y -= 1.0;
	}

	TextFileClose(fp);
	RePaint();
	CheckInputMessages(0);
	PostMessage(SCHWindow, WM_COMMAND, ID_MOVE_OBJECTS, 0);
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 SearchReplaceString(LPSTR Src, LPSTR Dest)
{
	int32 cnt2, Length, Found, pos, diff, LengthSearch;
	char SearchString2[512] = "", str2[512], *strp, *strp2, *SearchStringP, *SrcP;

	/*
	SearchReplaceOptions|=1; // Match case
	SearchReplaceOptions|=2; // Whole words
	SearchString
	ReplaceString
	*/

	if ((SearchReplaceOptions & 1) == 0)
	{
		// Case insensitive
		strcpy(SearchString2, SearchString);
		struprUTF8(SearchString2);
		strcpy(str2, Src);
		struprUTF8(str2);
		SearchStringP = SearchString2;
		SrcP = str2;
	}
	else
	{
		// Match case
		SearchStringP = SearchString;
		SrcP = Src;
	}

	Length = strlen(Src);
	LengthSearch = strlen(SearchString);
	cnt2 = 0;
	Found = 0;

	while (!Found)
	{
		if (!(strp = strstr(&SrcP[cnt2], SearchStringP)))
			return 0;

		if (SearchReplaceOptions & 2)
		{
			// Check for whole words
			strp2 = strp + LengthSearch;

			if ((strp == SrcP) || (!IsWordChar(*strp)))
			{
				// The left of the word search is ok
				// Check the word right
				if (!IsWordChar(*strp2))
				{
					// Searchstring is on a word boundary
				}
				else
				{
					// Searchstring found but not on a word boundary
					// Try again with searching on the next position
					cnt2 = strp - SrcP + 1;
					continue;
				}
			}
			else
			{
				// Searchstring found but not on a word boundary
				// Try again with searching on the next position
				cnt2 = strp - SrcP + 1;
				continue;
			}
		}

		// Found the searchstring -> replace it
		pos = strp - SrcP;
		strcpy(Dest, SrcP);
		diff = strlen(ReplaceString) - strlen(SearchString);

		if (diff > 0)
		{
			// replace string is larger
			memmove(&Dest[pos + diff], &Dest[pos], Length - pos + 2);
		}
		else
		{
			if (diff < 0)
			{
				// replace string is smaller
				memmove(&Dest[pos], &Dest[pos - diff], Length - pos + 2);
			}
		}

		memcpy(&Dest[pos], ReplaceString, strlen(ReplaceString));
		return 1;
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ReplaceTexts(int32 mode)
{
	ObjectTextRecord *ObjectText;
	int32 cnt, count, Length, TempLastActionNr, diff;
	char str[512];

	if (FindReplaceDialog(0) != 1)
		return -1;

	count = 0;

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectText->Info & OBJECT_SELECTED)
				count++;
		}
	}

	if (count == 0)
		mode = 0;
	else
		mode = 1;

	diff = strlen(ReplaceString) - strlen(SearchString);
	TempLastActionNr = LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectText->AddNr <= TempLastActionNr))
		{
			if ((mode == 0) || (ObjectText->Info & OBJECT_SELECTED))
			{
				if (!SearchReplaceString(ObjectText->Text, str))
					continue;

				Length = strlen(ObjectText->Text);
				memcpy(&NewObjectText, ObjectText, sizeof(NewObjectText));
				memset(&NewObjectText.Text, 0, sizeof(NewObjectText.Text));
				strncpy(NewObjectText.Text, str, min(sizeof(NewObjectText.Text) - 1, Length + diff));
				AddObjectText(&NewObjectText);
				ObjectText = &((*ObjectTexts)[cnt]);
				ObjectText->Info &= ~OBJECT_SELECTED;
				ObjectText->Info |= OBJECT_NOT_VISIBLE;
				ObjectText->DeleteNr = (int16) LastActionNr;
			}
		}
	}

	RePaint();
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
