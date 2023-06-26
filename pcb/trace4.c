/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: trace4.c
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
#include "trace4.h"
#include "line2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "menus.h"
#include "calc.h"
#include "toets.h"
#include "calcdef.h"
#include "math.h"
#include "calc2.h"
#include "graphics.h"
#include "calcrect.h"
#include "mainloop.h"
#include "insdel.h"
#include "select.h"
#include "dialogs.h"
#include "stdio.h"
#include "resource.h"
#include "help.h"
#include "polygon.h"

extern HDC OutputDisplay;

//static double  CurrentX2,CurrentY2;

static int32 OldDir, ArcMode, LineOldX1, LineOldX2, ThickNess;;
static char ObjectTextLine[MAX_LENGTH_STRING], CopyInfoStr[MAX_LENGTH_STRING];


extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern int32 CrossHairMode, CrossHairType;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitInfoStr(double CurrentX, double CurrentY, int32 mode)
{
	char str[MAX_LENGTH_STRING];
	double x3, y3, x, y, Angle, Length;

	x = fabs(CurrentX - CurrentX2);
	y = fabs(CurrentY - CurrentY2);

	switch (mode)
	{
	case 0:
		mode = 100;
		ConvNormalCoorToPolar(CurrentX2, CurrentY2, CurrentX, CurrentY, &Angle, &Length);
		Length = ConvertToUnits(Units, Length);
		Angle *= 180 / PI;

		switch (Units)
		{
		case 0:
			sprintf(str, SC(1064, "Length %.1f thou   Angle %.1f"), Length, Angle);
			break;

		case 1:
			sprintf(str, SC(1065, "Length %.4f mm   Angle %.1f"), Length, Angle);
			break;
		}

		break;

	case 1:
		mode = 100;
		x3 = ConvertToUnits(Units, x);
		y3 = ConvertToUnits(Units, y);

		switch (Units)
		{
		case 0:
			sprintf(str, "%.1f , %.1f", x3, y3);
			break;

		case 1:
			sprintf(str, "%.4f , %.4f", x3, y3);
			break;
		}

		break;

	case 2:
	case 3:
		mode = 100;
		ConvNormalCoorToPolar(NewObjectArc.CentreX, NewObjectArc.CentreY, CurrentX, CurrentY, &Angle, &Length);
		sprintf(str, SC(1066, "Angle %.1f"), Angle * 180 / PI);
		break;

	case 11:
		mode = 100;
		x3 = ConvertToUnits(Units, NewObjectArc.Width);

		switch (Units)
		{
		case 0:
			sprintf(str, "%.1f", x3);
			break;

		case 1:
			sprintf(str, "%.4f", x3);
			break;
		}

		break;
	}

	if (mode == 100)
	{
		strcpy(InfoStr, str);
		RedrawInfoStr(1);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingObjectLine(double CurrentX, double CurrentY, int32 Mode)
/*
      0

      |
      |
      |
3 ----+---- 1
      |
      |
      |

      2

*/
{
	double x1, y1;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	if (Mode == 0)
	{
		InitDrawingObject(0, CROSS_HAIR_LAYER, 1, DRAW_WITH_DASH_PEN_AND_NO_BRUSH);
		DrawLine(MultX(CurrentX), 100000, MultX(CurrentX), -100000);
		DrawLine(100000, MultY(CurrentY), -100000, MultY(CurrentY));

		ExitDrawing();
		EndDrawingEditingWindow();
		return;
	}

//  if (!SilkScreenDraw) InitDrawingInfoObjects(ThickNess);
//  else InitDrawingSilkScreen(ThickNess);

	x1 = 0.0;
	y1 = 0.0;

	NewObjectLine.X1 = (float) CurrentX2;	// Start
	NewObjectLine.Y1 = (float) CurrentY2;
	NewObjectLine.X2 = (float) CurrentX;	// End (Cursor position)
	NewObjectLine.Y2 = (float) CurrentY;

	DrawObjectLine(&NewObjectLine, 0.0, 0.0, 0);

//  DrawLine(Mult(CurrentX-Xoffset),DrawWindowMaxY-Mult(CurrentY-Yoffset)-1,
//           Mult(CurrentX2-Xoffset),DrawWindowMaxY-Mult(CurrentY2-Yoffset)-1);
	if (GetDimensionTextFromLine(CurrentX2, CurrentY2, CurrentX, CurrentY, &NewObjectText2, NewObjectLine.LineMode) ==
	        0)
		DrawObjectText2(&NewObjectText2, 0.0, 0.0, 0.0, 0);

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddTryingObjectLine()
{
	int32 ok;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_COPYPEN);
	DrawCrossHair(8 + 2);

	if ((InRange(NewObjectLine.X1, NewObjectLine.X2)) && (InRange(NewObjectLine.Y1, NewObjectLine.Y2)))
		ok = 1;


	if (AddObjectLine(&NewObjectLine))
		DrawObjectLine(&NewObjectLine, 0.0, 0.0, 0);

	if ((NewObjectLine.LineMode != 0) && (NewObjectText2.Text[0] != 0))
	{
		if (AddObjectText2(&NewObjectText2))
			DrawObjectText2(&NewObjectText2, 0.0, 0.0, 0.0, 0);
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ConvertPoint2(double CurrentX, double CurrentY, double *NewX, double *NewY)
{
	double divx, divy;

	*NewX = CurrentX;
	*NewY = CurrentY;
	divx = CurrentX - CurrentX2;
	divy = CurrentY - CurrentY2;

	if (divx > 0)
	{
		if (divy > 0)
		{
			if (divx > divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (divx * 3 < divy)
					*NewX = CurrentX2;
				else
				{
					if (divx > divy)
						*NewY = CurrentY2 + divx;
					else
						*NewX = CurrentX2 + divy;
				}
			}
		}
		else
		{
			if (divx > -divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (divx * 3 < -divy)
					*NewX = CurrentX2;
				else
				{
					if (divx > -divy)
						*NewY = CurrentY2 - divx;
					else
						*NewX = CurrentX2 - divy;
				}
			}
		}
	}
	else
	{
		if (divy > 0)
		{
			if (-divx > divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (-divx * 3 < divy)
					*NewX = CurrentX2;
				else
				{
					if (-divx > divy)
						*NewY = CurrentY2 - divx;
					else
						*NewX = CurrentX2 - divy;
				}
			}
		}
		else
		{
			if (-divx > -divy * 3)
				*NewY = CurrentY2;
			else
			{
				if (-divx * 3 < -divy)
					*NewX = CurrentX2;
				else
				{
					if (-divx < -divy)
						*NewY = CurrentY2 + divx;
					else
						*NewX = CurrentX2 + divy;
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddObjectLines(double LineThickNess, int32 Layer, int32 Mode)
{
	int32 cnt, OldMode;
	double OldX, OldY, CurrentX, CurrentY, NewX, NewY;
	HMENU PopUpMenu;
	ObjectTextRecord2 TypeObject;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
//  CurrentDrawMode=1;
	strcpy(InfoStr, CopyInfoStr);
	OldMode = Mode;
	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	NewObjectLine.NetNr = -1;
	memset(&NewObjectText2, 0, sizeof(ObjectTextRecord2));

	if ((Layer == SOLD_MASK_BOTTOM) || (Layer == SOLD_MASK_TOP) || (Layer == PASTE_MASK_BOTTOM)
	        || (Layer == PASTE_MASK_TOP))
		LinesAllDirection = 0;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	if (!LinesAllDirection)
	{
		ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
		CurrentX = NewX;
		CurrentY = NewY;
	}

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;

	NewObjectLine.LineThickNess = (float) LineThickNess;
	NewObjectLine.Layer = Layer;
	NewObjectLine.LineMode = Mode;

	NewObjectText2.FontHeight = Design.DimensionHeight;
	NewObjectText2.LineThickNess = (float) LineThickNess;
	NewObjectText2.Layer = Layer;
	NewObjectText2.NetNr = -1;

	ClipMouseCursor();

	CrossHairMode = 1;
	DrawTryingObjectLine(CurrentX, CurrentY, Mode);
	SystemBusyMode = 10;
	Mode = 0;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectLine;
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

			if ((Mode != 0) && (!LinesAllDirection))
			{
				ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObjectLine(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawTryingObjectLine(OldX, OldY, Mode);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if (!LinesAllDirection)
				{
					ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawTryingObjectLine(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if (!LinesAllDirection)
				{
					ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawTryingObjectLine(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if (!LinesAllDirection)
				{
					ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawTryingObjectLine(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if (!LinesAllDirection)
				{
					ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
			}

			if (Mode > 0)
				InitInfoStr(CurrentX, CurrentY, 0);

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if (!LinesAllDirection)
			{
				ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObjectLine(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectLine(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectLine(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if (!LinesAllDirection)
			{
				ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectLine(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if (!LinesAllDirection)
			{
				ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectLine(CurrentX, CurrentY, Mode);

			if (Mode > 0)
			{
				if ((NotInRange(NewObjectLine.X1, NewObjectLine.X2))
				        || (NotInRange(NewObjectLine.Y1, NewObjectLine.Y2)))
					CommandAddTryingObjectLine();

				CurrentX2 = CurrentX;
				CurrentY2 = CurrentY;
			}
			else
			{
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				CurrentX2 = CurrentX;
				CurrentY2 = CurrentY;

				if (!LinesAllDirection)
				{
					ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
			}

			Mode++;
			RelX = CurrentX;
			RelY = CurrentY;
			CheckInputMessages(0);

			if ((NewObjectLine.LineMode != 0) && (Mode > 1))
				SelectionEsc = 1;

			if (!SelectionEsc)
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingObjectLine(OldX, OldY, Mode);
			PopUpMenu = CreatePopupMenu();
			AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(493, "Escape"));

			if ((Layer != SOLD_MASK_BOTTOM) && (Layer != SOLD_MASK_TOP) && (Layer != PASTE_MASK_BOTTOM)
			        && (Layer != PASTE_MASK_TOP))
			{
				if (LinesAllDirection)
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_45_DIR,
					              SC(991, "Draw with 45/90 degrees directions"));
				}
				else
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_ALL_DIR,
					              SC(992, "Draw in all directions"));
				}
			}

			TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
			               RealWindow.top + MousePosY + 40, 0, PCBWindow, NULL);
			DestroyMenu(PopUpMenu);
			RightButtonPressed = 0;
			CheckInputMessages(0);

			if (!SelectionEsc)
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectLine(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectTextRecord));

				if (Mode == 0)
				{
					if (LineInputDialog(&TypeObject, SC(1067, "Add line (x1,y1,x2,y2,x3,y3,x4,y4, .... )"), 0) == 1)
					{
						if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 0)
						{
							if (NrParams == 2)
							{
								if (!ParametersRelative)
								{
									NewObjectLine.X1 = (float) ParamsFloat[0];
									NewObjectLine.Y1 = (float) ParamsFloat[1];
								}
								else
								{
									NewObjectLine.X1 = (float) (RelX + ParamsFloat[0]);
									NewObjectLine.Y1 = (float) (RelY + ParamsFloat[1]);
								}

								CurrentX2 = NewObjectLine.X1;
								CurrentY2 = NewObjectLine.Y1;
								Mode = 1;
							}

							if ((NrParams > 2) && ((NrParams & 1) == 0))
							{
								for (cnt = 0; cnt < NrParams / 2 - 1; cnt++)
								{
									if (!ParametersRelative)
									{
										NewObjectLine.X1 = (float) ParamsFloat[cnt * 2];
										NewObjectLine.Y1 = (float) ParamsFloat[cnt * 2 + 1];
										NewObjectLine.X2 = (float) ParamsFloat[cnt * 2 + 2];
										NewObjectLine.Y2 = (float) ParamsFloat[cnt * 2 + 3];
									}
									else
									{
										NewObjectLine.X1 = (float) (RelX + ParamsFloat[cnt * 2]);
										NewObjectLine.Y1 = (float) (RelY + ParamsFloat[cnt * 2 + 1]);
										NewObjectLine.X2 = (float) (RelX + ParamsFloat[cnt * 2 + 2]);
										NewObjectLine.Y2 = (float) (RelY + ParamsFloat[cnt * 2 + 3]);
									}

									CommandAddTryingObjectLine();
								}

								CurrentX2 = NewObjectLine.X2;
								CurrentY2 = NewObjectLine.Y2;
								Mode = 1;
							}
						}
					}
				}
				else
				{
					if (LineInputDialog(&TypeObject, SC(1068, "Add line to (x2,y2)"), 0) == 1)
					{
						if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) == 2)
						{
							if (!ParametersRelative)
							{
								NewObjectLine.X2 = (float) ParamsFloat[0];
								NewObjectLine.Y2 = (float) ParamsFloat[1];
							}
							else
							{
								NewObjectLine.X2 = (float) (CurrentX2 + ParamsFloat[0]);
								NewObjectLine.Y2 = (float) (CurrentY2 + ParamsFloat[1]);
							}

							CurrentX2 = NewObjectLine.X2;
							CurrentY2 = NewObjectLine.Y2;
							CommandAddTryingObjectLine();
						}
					}
				}
			}

			SpacePressed = 0;

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if (HelpAsked)
			{
				Help("special_objects.htm", 0);
				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			if (!LinesAllDirection)
			{
				ConvertPoint2(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectLine(CurrentX, CurrentY, Mode);

			ClipMouseCursor();
		}
	}

	UnClipMouseCursor();
	CrossHairMode = 0;
	DrawCrossHair(2);
	strcpy(InfoStr, CopyInfoStr);
	RedrawInfoStr(1);
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingObjectRect(double CurrentX, double CurrentY, int32 Mode)
{
	switch (Mode)
	{
	case 0:
		StartDrawingEditingWindow();
		SetROP2(OutputDisplay, R2_XORPEN);
		InitDrawingObject(0, CROSS_HAIR_LAYER, 1, DRAW_WITH_DASH_PEN_AND_NO_BRUSH);
		DrawLine(MultX(CurrentX), 100000, MultX(CurrentX), -100000);
		DrawLine(100000, MultY(CurrentY), -100000, MultY(CurrentY));
		ExitDrawing();
		EndDrawingEditingWindow();
		return;

	case 1:
		NewObjectRect.CentreX = (float) ((CurrentX2 + CurrentX) * 0.5);
		NewObjectRect.CentreY = (float) ((CurrentY2 + CurrentY) * 0.5);
		NewObjectRect.Width = (float) fabs(CurrentX - CurrentX2);
		NewObjectRect.Height = (float) fabs(CurrentY - CurrentY2);
//        NewObjectRect.CentreX=CurrentX2;
//        NewObjectRect.CentreY=CurrentY2;
//        NewObjectRect.Width=(fabs(CurrentX-CurrentX2)*2);
//        NewObjectRect.Height=(fabs(CurrentY-CurrentY2)*2);
		break;

	case 2:
		NewObjectRect.CentreX = (float) CurrentX;
		NewObjectRect.CentreY = (float) CurrentY;
		break;
	}

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
	DrawLine(Mult(NewObjectRect.CentreX - Xoffset) + DrawWindowMinX - 3,
	         DrawWindowMaxY - Mult(NewObjectRect.CentreY - Yoffset) - 1 - 3,
	         Mult(NewObjectRect.CentreX - Xoffset) + DrawWindowMinX + 3,
	         DrawWindowMaxY - Mult(NewObjectRect.CentreY - Yoffset) - 1 + 3);
	DrawLine(Mult(NewObjectRect.CentreX - Xoffset) + DrawWindowMinX - 3,
	         DrawWindowMaxY - Mult(NewObjectRect.CentreY - Yoffset) - 1 + 3,
	         Mult(NewObjectRect.CentreX - Xoffset) + DrawWindowMinX + 3,
	         DrawWindowMaxY - Mult(NewObjectRect.CentreY - Yoffset) - 1 - 3);

	if (NewObjectRect.LineThickNess == 0.0)
		DrawObjectRect(&NewObjectRect, 0.0, 0.0, 1);
	else
		DrawObjectRect(&NewObjectRect, 0.0, 0.0, 0);

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddTryingObjectRect(ObjectRectRecord * ObjectRect, int32 Mode)
{
	StartDrawingEditingWindow();
	DrawCrossHair(8 + 2);
	SetROP2(OutputDisplay, R2_COPYPEN);

	if (Mode == 1)
	{
		memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
		NewObjectLine.Layer = NewObjectRect.Layer;
		NewObjectLine.NetNr = -1;
		NewObjectLine.LineThickNess = NewObjectRect.LineThickNess;
		NewObjectLine.X1 = NewObjectRect.CentreX - (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y1 = NewObjectRect.CentreY - (NewObjectRect.Height * (float) 0.5);
		NewObjectLine.X2 = NewObjectRect.CentreX - (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y2 = NewObjectRect.CentreY + (NewObjectRect.Height * (float) 0.5);

		if (AddObjectLine(&NewObjectLine))
			DrawObjectLine(&NewObjectLine, 0.0, 0.0, 0);

		NewObjectLine.X1 = NewObjectRect.CentreX - (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y1 = NewObjectRect.CentreY + (NewObjectRect.Height * (float) 0.5);
		NewObjectLine.X2 = NewObjectRect.CentreX + (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y2 = NewObjectRect.CentreY + (NewObjectRect.Height * (float) 0.5);

		if (AddObjectLine(&NewObjectLine))
			DrawObjectLine(&NewObjectLine, 0.0, 0.0, 0);

		NewObjectLine.X1 = NewObjectRect.CentreX + (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y1 = NewObjectRect.CentreY + (NewObjectRect.Height * (float) 0.5);
		NewObjectLine.X2 = NewObjectRect.CentreX + (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y2 = NewObjectRect.CentreY - (NewObjectRect.Height * (float) 0.5);

		if (AddObjectLine(&NewObjectLine))
			DrawObjectLine(&NewObjectLine, 0.0, 0.0, 0);

		NewObjectLine.X1 = NewObjectRect.CentreX + (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y1 = NewObjectRect.CentreY - (NewObjectRect.Height * (float) 0.5);
		NewObjectLine.X2 = NewObjectRect.CentreX - (NewObjectRect.Width * (float) 0.5);
		NewObjectLine.Y2 = NewObjectRect.CentreY - (NewObjectRect.Height * (float) 0.5);

		if (AddObjectLine(&NewObjectLine))
			DrawObjectLine(&NewObjectLine, 0.0, 0.0, 0);
	}
	else
	{
		if (AddObjectRect(ObjectRect))
			DrawObjectRect(ObjectRect, 0.0, 0.0, 0);
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddObjectRects(double LineThickNess, int32 Layer, int32 Mode)
{
	int32 RectMode;
	double OldX, OldY, CurrentX, CurrentY;
	ObjectTextRecord2 TypeObject;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
//  CurrentDrawMode=1;
	strcpy(InfoStr, CopyInfoStr);

	memset(&NewObjectRect, 0, sizeof(ObjectRectRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	RelX = CurrentX;
	RelY = CurrentY;
	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;

	NewObjectRect.LineThickNess = (float) LineThickNess;
	NewObjectRect.Layer = Layer;

	if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		NewObjectRect.NetNr = -2;
	else
		NewObjectRect.NetNr = -1;

	if (Mode == 2)
	{
		NewObjectRect.Info = OBJECT_FILLED;
		NewObjectRect.LineThickNess = 0.0;
		Mode = 0;
	}

	RectMode = Mode;
	Mode = 0;
	ClipMouseCursor();
	DrawTryingObjectRect(CurrentX, CurrentY, Mode);
	SystemBusyMode = 11;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectRect;
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
				DrawTryingObjectRect(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawTryingObjectRect(OldX, OldY, Mode);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawTryingObjectRect(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawTryingObjectRect(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawTryingObjectRect(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			}

			if (Mode > 0)
				InitInfoStr(CurrentX, CurrentY, 1);

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
			DrawTryingObjectRect(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton() == 1)
		{
			DrawTryingObjectRect(CurrentX, CurrentY, Mode);

			if (Mode > 0)
			{
				if ((NotInRange(NewObjectRect.Width, 0.0)) || (NotInRange(NewObjectRect.Height, 0.0)))
				{
					CommandAddTryingObjectRect(&NewObjectRect, RectMode);
					SelectionEsc = 1;
				}
			}

			OldX = CurrentX;
			OldY = CurrentY;
			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;
			CheckInputMessages(0);
			Mode++;
			RelX = CurrentX;
			RelY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectRect(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectTextRecord));

				switch (Mode)
				{
				case 0:
					if (LineInputDialog(&TypeObject, SC(1069, "Add rectangle (width,height,[CenterX,CenterY])"), 0) ==
					        1)
					{
						if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 0)
						{
							if (NrParams == 2)
							{
								NewObjectRect.Width = (float) ParamsFloat[0];
								NewObjectRect.Height = (float) ParamsFloat[1];
								Mode = 2;
							}

							if (NrParams == 4)
							{
								NewObjectRect.Width = (float) ParamsFloat[0];
								NewObjectRect.Height = (float) ParamsFloat[1];

								if (!ParametersRelative)
								{
									NewObjectRect.CentreX = (float) ParamsFloat[2];
									NewObjectRect.CentreY = (float) ParamsFloat[3];
								}
								else
								{
									NewObjectRect.CentreX = (float) (RelX + ParamsFloat[2]);
									NewObjectRect.CentreY = (float) (RelY + ParamsFloat[3]);
								}

								if ((NotInRange(NewObjectRect.Width, 0.0)) && (NotInRange(NewObjectRect.Height, 0.0)))
								{
									CommandAddTryingObjectRect(&NewObjectRect, RectMode);
									SelectionEsc = 1;
								}
							}
						}
						else
						{
						}
					}

					break;

				case 1:
					if (LineInputDialog(&TypeObject, SC(1070, "Add rectangle (width,height)"), 0) == 1)
					{
						if (((NrParams = ScanParameters(-1, TypeObject.Text, 0)) == 2)
						        && (NotInRange(NewObjectRect.Width, 0.0)) && (NotInRange(NewObjectRect.Height, 0.0)))
						{
							NewObjectRect.Width = (float) fabs(ParamsFloat[0]);
							NewObjectRect.Height = (float) fabs(ParamsFloat[1]);
							NewObjectRect.CentreX = (float) (CurrentX2 + ParamsFloat[0] * 0.5);
							NewObjectRect.CentreY = (float) (CurrentY2 + ParamsFloat[1] * 0.5);
							CommandAddTryingObjectRect(&NewObjectRect, RectMode);
							SelectionEsc = 1;
						}
					}

					break;

				case 2:
					if (LineInputDialog(&TypeObject, SC(1071, "Add rectangle (CenterX,CenterY)"), 0) == 1)
					{
						if (((NrParams = ScanParameters(-1, TypeObject.Text, 0)) == 2)
						        && (NotInRange(NewObjectRect.Width, 0.0)) && (NotInRange(NewObjectRect.Height, 0.0)))
						{
							if (!ParametersRelative)
							{
								NewObjectRect.CentreX = (float) ParamsFloat[0];
								NewObjectRect.CentreY = (float) ParamsFloat[1];
							}
							else
							{
								NewObjectRect.CentreX = (float) (RelX + ParamsFloat[0]);
								NewObjectRect.CentreY = (float) (RelY + ParamsFloat[1]);
							}

							CommandAddTryingObjectRect(&NewObjectRect, RectMode);
							SelectionEsc = 1;
						}
					}
				}
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				Help("special_objects.htm", 0);
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

			if (!SelectionEsc)
				DrawTryingObjectRect(CurrentX, CurrentY, Mode);

			ClipMouseCursor();
		}
	}

	UnClipMouseCursor();
	strcpy(InfoStr, CopyInfoStr);
	RedrawInfoStr(1);
	SystemBusyMode = 0;
	CrossHairMode = 0;
	DrawCrossHair(2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingObjectArc(double CurrentX, double CurrentY, int32 Mode)
{
	int32 ok, OkToDraw;

	if (Mode == 0)
	{
		StartDrawingEditingWindow();
		SetROP2(OutputDisplay, R2_XORPEN);
		DrawCrossHair(8);
		ExitDrawing();
		EndDrawingEditingWindow();
		return;
	}

	OkToDraw = 0;
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	switch (Mode)
	{
	case 1:
		NewObjectArc.CentreX = (float) CurrentX2;
		NewObjectArc.CentreY = (float) CurrentY2;

		if (((NewObjectArc.Info & OBJECT_FILLED) == 0) && (NewObjectArc.Layer != SOLD_MASK_BOTTOM)
		        && (NewObjectArc.Layer != SOLD_MASK_TOP) && (NewObjectArc.Layer != PASTE_MASK_BOTTOM)
		        && (NewObjectArc.Layer != PASTE_MASK_TOP) && (NewObjectArc.Layer >= 32))
		{
			NewObjectArc.Width = (float) (fabs(CurrentX - CurrentX2) * 2.0);
			NewObjectArc.Height = (float) (fabs(CurrentY - CurrentY2) * 2.0);
		}
		else
		{
			NewObjectArc.Width = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
			NewObjectArc.Height = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
		}

		NewObjectArc.StartDiffX = (float) (CurrentX - CurrentX2);
		NewObjectArc.StartDiffY = (float) (CurrentY - CurrentY2);
		NewObjectArc.EndDiffX = (float) (CurrentX - CurrentX2);
		NewObjectArc.EndDiffY = (float) (CurrentY - CurrentY2);
		OkToDraw = 1;
		break;

	case 2:
		NewObjectArc.StartDiffX = (float) (CurrentX - NewObjectArc.CentreX);
		NewObjectArc.StartDiffY = (float) (CurrentY - NewObjectArc.CentreY);
		NewObjectArc.EndDiffX = (float) (CurrentX - NewObjectArc.CentreX);
		NewObjectArc.EndDiffY = (float) (CurrentY - NewObjectArc.CentreY);
		OkToDraw = 1;
		break;

	case 3:
		NewObjectArc.EndDiffX = (float) (CurrentX - NewObjectArc.CentreX);
		NewObjectArc.EndDiffY = (float) (CurrentY - NewObjectArc.CentreY);
		OkToDraw = 1;
		break;

	case 4:
		NewObjectArc.CentreX = (float) CurrentX;
		NewObjectArc.CentreY = (float) CurrentY;
		OkToDraw = 1;
		break;

	case 5:
		NewObjectArc.CentreX = (float) CurrentX;
		NewObjectArc.CentreY = (float) CurrentY;
		OkToDraw = 1;
		break;

	case 10:
		NewObjectArc.CentreX = (float) CurrentX;
		NewObjectArc.CentreY = (float) CurrentY;
		break;

	case 11:
		NewObjectArc.CentreX = (float) CurrentX2;
		NewObjectArc.CentreY = (float) CurrentY2;
		NewObjectArc.Width = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
		NewObjectArc.Height = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
		OkToDraw = 1;
		break;

	case 12:
		NewObjectArc.CentreX = (float) CurrentX;
		NewObjectArc.CentreY = (float) CurrentY;
		OkToDraw = 1;
		break;

	default:
		ok = 1;
		break;
	}

	if (OkToDraw)
		DrawObjectArc(&NewObjectArc, 0.0, 0.0, 1);

	/*
	      if (NewObjectCircle.LineThickNess==0.0) {
	        DrawObjectCircle(&NewObjectCircle,0.0,0.0,1);
	      } else {
	        DrawObjectCircle(&NewObjectCircle,0.0,0.0,0);
	      }
	*/
	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);

	switch (Mode)
	{
	case 1:
	case 4:
	case 5:
	case 10:
	case 11:
	case 12:
		DrawLine(MultX(NewObjectArc.CentreX) - 3, MultY(NewObjectArc.CentreY) - 3, MultX(NewObjectArc.CentreX) + 3,
		         MultY(NewObjectArc.CentreY) + 3);
		DrawLine(MultX(NewObjectArc.CentreX) - 3, MultY(NewObjectArc.CentreY) + 3, MultX(NewObjectArc.CentreX) + 3,
		         MultY(NewObjectArc.CentreY) - 3);
		break;

	case 2:
	case 3:
		DrawLine(MultX(NewObjectArc.CentreX), MultY(NewObjectArc.CentreY), MultX(CurrentX), MultY(CurrentY));
		break;
	}

	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddTryingObjectArc(ObjectArcRecord * ObjectArc)
{
	StartDrawingEditingWindow();
	DrawCrossHair(2);
	SetROP2(OutputDisplay, R2_COPYPEN);

	if (AddObjectArc(ObjectArc))
		DrawObjectArc(ObjectArc, 0.0, 0.0, 0);

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddObjectArcs(double LineThickNess, int32 Layer, int32 CircleMode, int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	ObjectTextRecord2 TypeObject;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	strcpy(InfoStr, CopyInfoStr);


	memset(&NewObjectArc, 0, sizeof(ObjectArcRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	RelX = CurrentX;
	RelY = CurrentY;
	NewObjectArc.LineThickNess = (float) LineThickNess;
	NewObjectArc.Layer = Layer;

	if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		NewObjectArc.NetNr = -2;
	else
		NewObjectArc.NetNr = -1;

	Mode = 0;

	switch (CircleMode)
	{
	case 1:
		NewObjectArc.StartDiffX = 100e5;
		NewObjectArc.StartDiffY = 0e5;
		NewObjectArc.EndDiffX = 0e5;
		NewObjectArc.EndDiffY = 100e5;
		Mode = 10;
		break;

	case 2:
		NewObjectArc.StartDiffX = 0e5;
		NewObjectArc.StartDiffY = -100e5;
		NewObjectArc.EndDiffX = 100e5;
		NewObjectArc.EndDiffY = 0e5;
		Mode = 10;
		break;

	case 3:
		NewObjectArc.StartDiffX = 0e5;
		NewObjectArc.StartDiffY = -100e5;
		NewObjectArc.EndDiffX = 0e5;
		NewObjectArc.EndDiffY = 100e5;
		Mode = 10;
		break;

	case 4:
		NewObjectArc.StartDiffX = -100e5;
		NewObjectArc.StartDiffY = 0e5;
		NewObjectArc.EndDiffX = 0e5;
		NewObjectArc.EndDiffY = -100e5;
		Mode = 10;
		break;

	case 6:
		NewObjectArc.StartDiffX = -100e5;
		NewObjectArc.StartDiffY = 0e5;
		NewObjectArc.EndDiffX = 100e5;
		NewObjectArc.EndDiffY = 0e5;
		Mode = 10;
		break;

	case 8:
		NewObjectArc.StartDiffX = 0e5;
		NewObjectArc.StartDiffY = 100e5;
		NewObjectArc.EndDiffX = -100e5;
		NewObjectArc.EndDiffY = 0e5;
		Mode = 10;
		break;

	case 9:
		NewObjectArc.StartDiffX = 100e5;
		NewObjectArc.StartDiffY = 0e5;
		NewObjectArc.EndDiffX = -100e5;
		NewObjectArc.EndDiffY = 0e5;
		Mode = 10;
		break;

	case 12:
		NewObjectArc.StartDiffX = 0e5;
		NewObjectArc.StartDiffY = 100e5;
		NewObjectArc.EndDiffX = 0e5;
		NewObjectArc.EndDiffY = -100e5;
		Mode = 10;
		break;

	case 15:
		NewObjectArc.StartDiffX = 0e5;
		NewObjectArc.StartDiffY = 100e5;
		NewObjectArc.EndDiffX = 0e5;
		NewObjectArc.EndDiffY = 100e5;
		Mode = 10;
		break;

	case 100:
		NewObjectArc.StartDiffX = 0e5;
		NewObjectArc.StartDiffY = 100e5;
		NewObjectArc.EndDiffX = 0e5;
		NewObjectArc.EndDiffY = 100e5;
		NewObjectArc.Info = OBJECT_FILLED;
		NewObjectArc.LineThickNess = 0.0;
		Mode = 10;
		break;
	}


	ClipMouseCursor();
	DrawTryingObjectArc(CurrentX, CurrentY, Mode);
	SystemBusyMode = 13;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectArc;
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
				DrawTryingObjectArc(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawTryingObjectArc(OldX, OldY, Mode);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawTryingObjectArc(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawTryingObjectArc(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawTryingObjectArc(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			}

			InitInfoStr(CurrentX, CurrentY, Mode);
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
			DrawTryingObjectArc(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectArc(CurrentX, CurrentY, Mode);

			if (CircleMode != 0)
			{
				if ((Mode > 10) && (NotInRange(NewObjectArc.Width, 0.0)))
				{
					CommandAddTryingObjectArc(&NewObjectArc);
					SelectionEsc = 1;
				}
			}
			else
			{
				if ((Mode >= 3) && (Mode < 5))
				{
					if ((NotInRange(NewObjectArc.Width, 0.0)) && (NotInRange(NewObjectArc.Height, 0.0)))
						CommandAddTryingObjectArc(&NewObjectArc);

					SelectionEsc = 1;
				}
			}

			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;
			OldX = CurrentX;
			OldY = CurrentY;
			CheckInputMessages(0);

			if (CircleMode == 0)
			{
				if ((Mode == 1) && (((InRange(NewObjectArc.Width, 0.0)) || (InRange(NewObjectArc.Height, 0.0)))))
					MessageBoxOwn(PCBWindow, SC(1109, "Arc width or height is zero"), "Error", MB_APPLMODAL | MB_OK);
				else
				{
					if (Mode != 5)
						Mode++;
					else
						Mode = 2;
				}
			}
			else
			{
				if (Mode == 10)
					Mode++;
			}

			RelX = CurrentX;
			RelY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			/*
			   if (CircleMode==0) {
			   DrawTryingObjectArc(CurrentX,CurrentY,Mode);
			   PopUpMenu=CreatePopupMenu();
			   AppendMenuUTF8(PopUpMenu,MF_ENABLED|MF_STRING,ID_ESCAPE,SC(493,"Escape"));
			   if (LinesAllDirection) {
			   AppendMenuUTF8(PopUpMenu,MF_ENABLED|MF_STRING,ID_LINES_45_DIR,SC(991,"Draw with 45/90 degrees directions"));
			   } else {
			   AppendMenuUTF8(PopUpMenu,MF_ENABLED|MF_STRING,ID_LINES_ALL_DIR,SC(992,"Draw in all directions"));
			   }
			   TrackPopupMenu(PopUpMenu,TPM_LEFTALIGN+TPM_RIGHTBUTTON,
			   RealWindow.left+MousePosX+5,RealWindow.top+MousePosY+40,0,PCBWindow,NULL);
			   DestroyMenu(PopUpMenu);
			   RightButtonPressed=0;
			   CheckInputMessages(0);
			   if (!SelectionEsc) DrawTryingObjectArc(CurrentX,CurrentY,Mode);
			   }
			 */
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectArc(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectTextRecord));

				switch (Mode)
				{
				case 0:
					if (LineInputDialog
					        (&TypeObject, SC(1072, "Add arc (width,height), [CenterX,CenterY], [StartAngle,EndAngle])"),
					         0) == 1)
					{
						if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 0)
						{
							if (NrParams == 2)
							{
								NewObjectArc.Width = (float) ParamsFloat[0];
								NewObjectArc.Height = (float) ParamsFloat[1];
								Mode = 5;
							}

							if (NrParams == 4)
							{
								NewObjectArc.Width = (float) ParamsFloat[0];
								NewObjectArc.Height = (float) ParamsFloat[1];

								if (!ParametersRelative)
								{
									NewObjectArc.CentreX = (float) ParamsFloat[2];
									NewObjectArc.CentreY = (float) ParamsFloat[3];
								}
								else
								{
									NewObjectArc.CentreX = (float) (RelX + ParamsFloat[2]);
									NewObjectArc.CentreY = (float) (RelY + ParamsFloat[3]);
								}

								Mode = 2;
							}

							if (NrParams == 6)
							{
								NewObjectArc.Width = (float) ParamsFloat[0];
								NewObjectArc.Height = (float) ParamsFloat[1];

								if (!ParametersRelative)
								{
									NewObjectArc.CentreX = (float) ParamsFloat[2];
									NewObjectArc.CentreY = (float) ParamsFloat[3];
								}
								else
								{
									NewObjectArc.CentreX = (float) (RelX + ParamsFloat[2]);
									NewObjectArc.CentreY = (float) (RelY + ParamsFloat[3]);
								}

								NewObjectArc.StartDiffX =
								    (float) cos(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[4])));
								NewObjectArc.StartDiffY =
								    (float) sin(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[4])));
								NewObjectArc.EndDiffX =
								    (float) cos(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[5])));
								NewObjectArc.EndDiffY =
								    (float) sin(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[5])));
								NewObjectArc.StartDiffX *= NewObjectArc.Width;
								NewObjectArc.StartDiffY *= NewObjectArc.Height;
								NewObjectArc.EndDiffX *= NewObjectArc.Width;
								NewObjectArc.EndDiffY *= NewObjectArc.Height;
								CommandAddTryingObjectArc(&NewObjectArc);
								SelectionEsc = 1;
							}
						}
					}

					break;

				case 1:
					if (LineInputDialog(&TypeObject, SC(1073, "Add arc (width,height), [StartAngle,EndAngle]"), 0) == 1)
					{
						if (((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 2)
						        && (NotInRange(ParamsFloat[0], 0.0)) && (NotInRange(ParamsFloat[1], 0.0)))
						{
							if (NrParams == 2)
							{
								NewObjectArc.Width = (float) ParamsFloat[0];
								NewObjectArc.Height = (float) ParamsFloat[1];
								Mode = 2;
							}

							if (NrParams == 4)
							{
								NewObjectArc.Width = (float) ParamsFloat[0];
								NewObjectArc.Height = (float) ParamsFloat[1];

								if (!ParametersRelative)
								{
									NewObjectArc.CentreX = (float) ParamsFloat[2];
									NewObjectArc.CentreY = (float) ParamsFloat[3];
								}
								else
								{
									NewObjectArc.CentreX = (float) (RelX + ParamsFloat[2]);
									NewObjectArc.CentreY = (float) (RelY + ParamsFloat[3]);
								}

								NewObjectArc.StartDiffX =
								    (float) cos(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[4])));
								NewObjectArc.StartDiffY =
								    (float) sin(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[4])));
								NewObjectArc.EndDiffX =
								    (float) cos(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[5])));
								NewObjectArc.EndDiffY =
								    (float) sin(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[5])));
								NewObjectArc.StartDiffX *= NewObjectArc.Width;
								NewObjectArc.StartDiffY *= NewObjectArc.Height;
								NewObjectArc.EndDiffX *= NewObjectArc.Width;
								NewObjectArc.EndDiffY *= NewObjectArc.Height;
								CommandAddTryingObjectArc(&NewObjectArc);
								SelectionEsc = 1;
							}
						}
					}

					break;

				case 2:
					if (LineInputDialog(&TypeObject, SC(1074, "Add arc (StartAngle,EndAngle)"), 1) == 1)
					{
						if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) == 2)
						{
							NewObjectArc.StartDiffX = (float) cos(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[0])));
							NewObjectArc.StartDiffY = (float) sin(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[0])));
							NewObjectArc.EndDiffX = (float) cos(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[1])));
							NewObjectArc.EndDiffY = (float) sin(ANGLE_CONVERT(ConvertToUnits(Units, ParamsFloat[1])));
							NewObjectArc.StartDiffX *= NewObjectArc.Width;
							NewObjectArc.StartDiffY *= NewObjectArc.Height;
							NewObjectArc.EndDiffX *= NewObjectArc.Width;
							NewObjectArc.EndDiffY *= NewObjectArc.Height;
							CommandAddTryingObjectArc(&NewObjectArc);
							SelectionEsc = 1;
						}
					}

					break;

				case 10:
					if (LineInputDialog(&TypeObject, SC(1075, "Add circle (diameter,[CenterX,CenterY])"), 0) == 1)
					{
						if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 0)
						{
							if (NrParams == 1)
							{
								NewObjectArc.Width = (float) ParamsFloat[0];
								NewObjectArc.Height = (float) ParamsFloat[0];
								Mode = 12;
							}

							if (NrParams == 3)
							{
								NewObjectArc.Width = (float) ParamsFloat[0];
								NewObjectArc.Height = (float) ParamsFloat[0];

								if (!ParametersRelative)
								{
									NewObjectArc.CentreX = (float) ParamsFloat[1];
									NewObjectArc.CentreY = (float) ParamsFloat[2];
								}
								else
								{
									NewObjectArc.CentreX = (float) (RelX + ParamsFloat[1]);
									NewObjectArc.CentreY = (float) (RelY + ParamsFloat[2]);
								}

								if (NotInRange(NewObjectArc.Width, 0.0))
								{
									CommandAddTryingObjectArc(&NewObjectArc);
									SelectionEsc = 1;
								}
							}
						}
						else
						{
						}
					}

					break;

				case 11:
				case 12:
					if (LineInputDialog(&TypeObject, SC(1076, "Add circle (CenterX,CenterY)"), 0) == 1)
					{
						if (((NrParams = ScanParameters(-1, TypeObject.Text, 0)) == 2)
						        && (NotInRange(NewObjectArc.Width, 0.0)))
						{
							if (!ParametersRelative)
							{
								NewObjectArc.CentreX = (float) ParamsFloat[0];
								NewObjectArc.CentreY = (float) ParamsFloat[1];
							}
							else
							{
								NewObjectArc.CentreX = (float) (RelX + ParamsFloat[0]);
								NewObjectArc.CentreY = (float) (RelY + ParamsFloat[1]);
							}

							CommandAddTryingObjectArc(&NewObjectArc);
							SelectionEsc = 1;
						}
					}

					break;
				}
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				Help("special_objects.htm", 0);
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

			if (!SelectionEsc)
				DrawTryingObjectArc(CurrentX, CurrentY, Mode);

			ClipMouseCursor();
		}
	}

	UnClipMouseCursor();
	strcpy(InfoStr, CopyInfoStr);
	RedrawInfoStr(1);
	SystemBusyMode = 0;
	CrossHairMode = 0;
	DrawCrossHair(2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingObjectText2(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	NewObjectText2.X = (float) CurrentX;
	NewObjectText2.Y = (float) CurrentY;

	if (Mode != -1)
		NewObjectText2.Rotation = (float) (Mode * 45.0);

	DrawObjectText2(&NewObjectText2, 0.0, 0.0, 0.0, 1);
	DrawCrossHair(8);
	ExitDrawing();
	EndDrawingEditingWindow();

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddTryingObjectText2(ObjectTextRecord2 * ObjectText2)
{
	StartDrawingEditingWindow();
	DrawCrossHair(2);

	if (AddObjectText2(ObjectText2))
		DrawObjectText2(ObjectText2, 0.0, 0.0, 0.0, 0);

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddObjectTexts2(double LineThickNess, int32 Layer, int32 Mode)
{
	double OldX, OldY, CurrentX, CurrentY;
	ObjectTextRecord2 TypeObject;
	DrawXorFunctionRecord DrawXorFunction;

	SelectionEsc = 0;
	strcpy(InfoStr, CopyInfoStr);

	memset(&NewObjectText2, 0, sizeof(ObjectTextRecord2));
	NewObjectText2.FontHeight = (60 * 2540.0);
	NewObjectText2.LineThickNess = (float) LineThickNess;

	if ((Layer == 0) || (Layer == SILKSCREEN_BOTTOM))
		NewObjectText2.TextMode = 0x10;

	if (TextInputDialog(&NewObjectText2, 2) == 2)
		return;

	if (strlen(NewObjectText2.Text) == 0)
		return;

	CheckInputMessages(200);
	NewObjectText2.Layer = Layer;

	switch (Layer)
	{
	default:
		NewObjectText2.NetNr = -1;
	}

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	NewObjectLine.LineThickNess = (float) LineThickNess;
	Mode = -1;

	ClipMouseCursor();
	DrawTryingObjectText2(CurrentX, CurrentY, Mode);

	SystemBusyMode = 14;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectText2;
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
				DrawTryingObjectText2(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawTryingObjectText2(OldX, OldY, Mode);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawTryingObjectText2(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawTryingObjectText2(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawTryingObjectText2(OldX, OldY, Mode);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
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
			DrawTryingObjectText2(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectText2(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectText2(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectText2(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectText2(CurrentX, CurrentY, Mode);
			CommandAddTryingObjectText2(&NewObjectText2);
			CheckInputMessages(0);
			SelectionEsc = 1;

			if (!SelectionEsc)
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawTryingObjectText2(CurrentX, CurrentY, Mode);

			if (AltPressed)
				Mode = (Mode + 1) & 7;
			else
				Mode = (Mode + 2) & 6;

			RightButtonPressed = 0;
			CheckInputMessages(0);
			DrawTryingObjectText2(CurrentX, CurrentY, Mode);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectText2(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectTextRecord));

				if (Mode == 0)
				{
					if (LineInputDialog(&TypeObject, SC(1077, "Add text (x,y)"), 0) == 1)
					{
						if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) == 2)
						{
							if (!ParametersRelative)
							{
								NewObjectText2.X = (float) ParamsFloat[0];
								NewObjectText2.Y = (float) ParamsFloat[1];
							}
							else
							{
								NewObjectText2.X = (float) (RelX + ParamsFloat[0]);
								NewObjectText2.Y = (float) (RelY + ParamsFloat[1]);
							}

							CommandAddTryingObjectText2(&NewObjectText2);
							SelectionEsc = 1;
						}
					}
				}
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				Help("special_objects.htm", 0);
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

			if (!SelectionEsc)
				DrawTryingObjectText2(CurrentX, CurrentY, Mode);

			ClipMouseCursor();
		}
	}

	UnClipMouseCursor();
	strcpy(InfoStr, CopyInfoStr);
	RedrawInfoStr(1);
	CrossHairMode = 0;
	DrawCrossHair(2);
	SystemBusyMode = 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
