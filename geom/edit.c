/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: edit.c
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
#include "graphics.h"
#include "files2.h"
#include "string.h"
#include "edit.h"
#include "line2.h"
#include "rect.h"
#include "files.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "geom.h"
#include "calc.h"
#include "toets.h"
#include "calcdef.h"
#include "math.h"
#include "graphics.h"
#include "calcrect.h"
#include "mainloop.h"
#include "insdel.h"
#include "select.h"
#include "dialogs.h"
#include "stdio.h"
#include "resource.h"
#include "movecomp.h"
#include "menus.h"
#include "help.h"
#include "polygon.h"
#include "utf8.h"


typedef struct
{
	BITMAPINFOHEADER bmiHeader;
	RGBQUAD bmiColors[256];
} BITMAPINFO_Own;

ObjectRecord NewObjectText2;

static double CentreSelectedX, CentreSelectedY, CurrentX2, CurrentY2;

static int32 OldDir, ArcMode, LineOldX1, LineOldX2, ThickNess;
static char ObjectTextLine[MAX_LENGTH_STRING];

static int32 SilkScreenDraw;
int32 LinesAllDirection = 1;
int32 CheckFilenameForSaving = 0;


// *******************************************************************************************************
// *******************************************************************************************************

extern int32 TraceBackWardsKeyPressed;
extern HDC OutputDisplay;
extern double CrossHairX, CrossHairY;
extern uint32 ClipBoardGeomObjectsID;

extern COLORREF TextColor;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingObject(double CurrentX, double CurrentY, int32 Mode)
{
	int32 cnt, SpecialMode, OldObjectType;
	int32 OkToDraw = 0;

	OldObjectType = NewObject.ObjectType;

	if (OldObjectType == OBJECT_TEXT)
		OkToDraw = 1;

	SpecialMode = Mode & 0x40;
	Mode &= 0x3f;

	if (Mode > 0)
		OkToDraw = 1;

	switch (OldObjectType)
	{
	case OBJECT_POLYGON:
		cnt = NewObjectPolygon->NrVertices;
		NewObjectPolygon->Points[cnt].x = CurrentX;
		NewObjectPolygon->Points[cnt].y = CurrentY;
		break;

	case OBJECT_LINE:
		if (Mode > 0)
		{
			NewObject.x1 = (float) CurrentX2;
			NewObject.y1 = (float) CurrentY2;
			NewObject.x2 = (float) CurrentX;
			NewObject.y2 = (float) CurrentY;
		}

		break;

	case OBJECT_RECT:
		if (Mode > 0)
		{
			switch (Mode)
			{
			case 1:
				switch (NewObject.Layer)
				{

             //case PLACEMENT_OUTLINE_LAYER:
             //case SILKSCREEN_LAYER:
             //case COMP_OUTLINE_LAYER:

				default:
					NewObject.x1 = (float) ((CurrentX2 + CurrentX) * 0.5);
					NewObject.y1 = (float) ((CurrentY2 + CurrentY) * 0.5);
					NewObject.x2 = (float) (fabs(CurrentX - CurrentX2));
					NewObject.y2 = (float) (fabs(CurrentY - CurrentY2));
					break;

					/*
					              default:
					                NewObject.x1=CurrentX2;
					                NewObject.y1=CurrentY2;
					                NewObject.x2=(fabs(CurrentX-CurrentX2)*2);
					                NewObject.y2=(fabs(CurrentY-CurrentY2)*2);
					                break;
					*/

				}

				break;

			case 2:
				NewObject.x1 = (float) CurrentX;
				NewObject.y1 = (float) CurrentY;
				break;
			}
		}

		break;

	case OBJECT_CIRCLE:
		if (Mode > 0)
		{
			switch (Mode)
			{
			case 1:
				NewObject.x1 = (float) CurrentX2;
				NewObject.y1 = (float) CurrentY2;
				NewObject.x2 = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
				NewObject.y2 = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
				break;

			case 2:
				NewObject.x1 = (float) CurrentX;
				NewObject.y1 = (float) CurrentY;
				break;
			}
		}

		break;

	case OBJECT_ARC:
		if (Mode > 0)
		{
			switch (Mode)
			{
			case 1:
				NewObject.x1 = (float) CurrentX2;
				NewObject.y1 = (float) CurrentY2;

				switch (NewObject.Layer)
				{
				case SOLD_MASK_BOTTOM_LAYER:
				case SOLD_MASK_TOP_LAYER:
				case PASTE_MASK_BOTTOM_LAYER:
				case PASTE_MASK_TOP_LAYER:
					NewObject.x2 = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
					NewObject.y2 = NewObject.x2;
					break;

				default:
					if (NewObject.Layer < 32)
					{
						NewObject.x2 = (float) (max(fabs(CurrentX - CurrentX2), fabs(CurrentY - CurrentY2)) * 2.0);
						NewObject.y2 = NewObject.x2;
					}
					else
					{
						NewObject.x2 = (float) (fabs(CurrentX - CurrentX2) * 2.0);
						NewObject.y2 = (float) (fabs(CurrentY - CurrentY2) * 2.0);
					}

					break;
				}

				NewObject.x3 = (float) (CurrentX - CurrentX2);
				NewObject.y3 = (float) (CurrentY - CurrentY2);
				NewObject.x4 = (float) (CurrentX - CurrentX2);
				NewObject.y4 = (float) (CurrentY - CurrentY2);
				break;

			case 2:

                //NewObject.x3=CurrentX-NewObject.x1;
                //NewObject.y3=CurrentY-NewObject.y1;

				NewObject.x3 = (float) (CurrentX - NewObject.x1);
				NewObject.y3 = (float) (CurrentY - NewObject.y1);
				NewObject.x4 = (float) (CurrentX - NewObject.x1);
				NewObject.y4 = (float) (CurrentY - NewObject.y1);
				break;

			case 3:
				NewObject.x4 = (float) (CurrentX - NewObject.x1);
				NewObject.y4 = (float) (CurrentY - NewObject.y1);
				break;

			case 4:
				NewObject.x1 = (float) CurrentX2;
				NewObject.y1 = (float) CurrentY2;
				break;

			case 5:
				NewObject.x1 = (float) CurrentX;
				NewObject.y1 = (float) CurrentY;
				break;
			}
		}

		break;

	case OBJECT_TEXT:
		NewObject.x1 = (float) CurrentX;
		NewObject.y1 = (float) CurrentY;

		if (Mode == 0)
			NewObject.RotationAngle = 0.0;
		else
			NewObject.RotationAngle = 90.0;

		break;

		/*
		    case TRACE:
		      if (Mode==1) {
		        GetDirection(CurrentX2,CurrentY2,CurrentX,CurrentY,&TraceObject);
		        NewObject.ObjectType=TraceObject.ObjectType;
		        NewObject.Info2=TraceObject.ObjectType;
		        NewObject.x1=TraceObject.x1;
		        NewObject.y1=TraceObject.y1;
		        NewObject.x2=TraceObject.x2;
		      }
		        //NewObject.ObjectType=0;
		      break;
		*/

	}

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	if (OkToDraw)
	{
		if (NewObject.ObjectType != OBJECT_POLYGON)
			DrawObject(&NewObject, (float) 0.0, (float) 0.0, 4);
		else
		{
			if (SpecialMode == 0)
				DrawObjectPolygon(NewObjectPolygon, (float) 0.0, (float) 0.0, 4);
			else
				DrawObjectPolygon(NewObjectPolygon, (float) 0.0, (float) 0.0, 0x20 + 4);
		}
	}

	switch (NewObject.ObjectType)
	{
	case OBJECT_LINE:
		if (OkToDraw)
		{
			if (GetDimensionTextFromLine(CurrentX2, CurrentY2, CurrentX, CurrentY, &NewObjectText2, NewObject.Info2) == 0)
				DrawObject(&NewObjectText2, 0.0, 0.0, 4);
		}

		break;

	case OBJECT_RECT:
		if (Mode > 0)
		{
			InitDrawingColorWhite(0);
			DrawLine(MultX(NewObject.x1) - 3, MultY(NewObject.y1) - 3, MultX(NewObject.x1) + 3,
			         MultY(NewObject.y1) + 3);
			DrawLine(MultX(NewObject.x1) - 3, MultY(NewObject.y1) + 3, MultX(NewObject.x1) + 3,
			         MultY(NewObject.y1) - 3);
		}

		break;

	case OBJECT_CIRCLE:
		if (Mode > 0)
		{
			InitDrawingColorWhite(0);
			DrawLine(MultX(NewObject.x1) - 3, MultY(NewObject.y1) - 3, MultX(NewObject.x1) + 3,
			         MultY(NewObject.y1) + 3);
			DrawLine(MultX(NewObject.x1) - 3, MultY(NewObject.y1) + 3, MultX(NewObject.x1) + 3,
			         MultY(NewObject.y1) - 3);
		}

		break;

	case OBJECT_ARC:
		InitDrawingColorWhite(0);

		switch (Mode)
		{
		case 0:
			break;

		case 1:
			DrawLine(MultX(NewObject.x1) - 3, MultY(NewObject.y1) - 3, MultX(NewObject.x1) + 3,
			         MultY(NewObject.y1) + 3);
			DrawLine(MultX(NewObject.x1) - 3, MultY(NewObject.y1) + 3, MultX(NewObject.x1) + 3,
			         MultY(NewObject.y1) - 3);
			break;

		case 2:
		case 3:
			DrawLine(MultX(NewObject.x1), MultY(NewObject.y1), MultX(CurrentX), MultY(CurrentY));
			break;
		}

		break;
	}

	NewObject.ObjectType = (int16) OldObjectType;

  //if (SpecialMode==0) {

	DrawCrossHair(16 + 8);

  //}

	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawTryingObject2a(double CurrentX, double CurrentY, int32 Mode)
{
	if (NewObject.ObjectType == OBJECT_POLYGON)
		DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);

	DrawTryingObject(CurrentX, CurrentY, Mode);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawTryingObject2b(double CurrentX, double CurrentY, int32 Mode)
{
	DrawTryingObject(CurrentX, CurrentY, Mode);

	if (NewObject.ObjectType == OBJECT_POLYGON)
		DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddTryingObject(int32 Mode)
{
	int32 ok, cnt, Found, count;
	double x1, y1, x2, y2, x3, y3, x4, y4, Angle1, Angle2, Length1, Length2;

	ObjectRecord NewObjectLine, *Object;
	ObjectPolygonRecord *ObjectPolygon2;

	StartDrawingEditingWindow();
	DrawCrossHair(16 + 8 + 2);
	NewObject.Info = 0;

	if ((NewObject.ObjectType == OBJECT_RECT) && (NewObject.Info2 == 1))
	{
		memset(&NewObjectLine, 0, sizeof(ObjectRecord));
		NewObjectLine.ObjectType = OBJECT_LINE;
		NewObjectLine.Layer = NewObject.Layer;
		NewObjectLine.PinNr = -1;
		NewObjectLine.Thickness = NewObject.Thickness;
		NewObjectLine.Clearance = NewObject.Clearance;
		NewObjectLine.x1 = NewObject.x1 - (float) (NewObject.x2 * 0.5);
		NewObjectLine.y1 = NewObject.y1 - (float) (NewObject.y2 * 0.5);
		NewObjectLine.x2 = NewObject.x1 - (float) (NewObject.x2 * 0.5);
		NewObjectLine.y2 = NewObject.y1 + (float) (NewObject.y2 * 0.5);

		if (AddObject(&NewObjectLine))
			DrawObject(&NewObjectLine, 0.0, 0.0, 0);

		NewObjectLine.x1 = NewObject.x1 - (float) (NewObject.x2 * 0.5);
		NewObjectLine.y1 = NewObject.y1 + (float) (NewObject.y2 * 0.5);
		NewObjectLine.x2 = NewObject.x1 + (float) (NewObject.x2 * 0.5);
		NewObjectLine.y2 = NewObject.y1 + (float) (NewObject.y2 * 0.5);

		if (AddObject(&NewObjectLine))
			DrawObject(&NewObjectLine, 0.0, 0.0, 0);

		NewObjectLine.x1 = NewObject.x1 + (float) (NewObject.x2 * 0.5);
		NewObjectLine.y1 = NewObject.y1 + (float) (NewObject.y2 * 0.5);
		NewObjectLine.x2 = NewObject.x1 + (float) (NewObject.x2 * 0.5);
		NewObjectLine.y2 = NewObject.y1 - (float) (NewObject.y2 * 0.5);

		if (AddObject(&NewObjectLine))
			DrawObject(&NewObjectLine, 0.0, 0.0, 0);

		NewObjectLine.x1 = NewObject.x1 + (float) (NewObject.x2 * 0.5);
		NewObjectLine.y1 = NewObject.y1 - (float) (NewObject.y2 * 0.5);
		NewObjectLine.x2 = NewObject.x1 - (float) (NewObject.x2 * 0.5);
		NewObjectLine.y2 = NewObject.y1 - (float) (NewObject.y2 * 0.5);

		if (AddObject(&NewObjectLine))
			DrawObject(&NewObjectLine, 0.0, 0.0, 0);
	}
	else
	{
		switch (NewObject.ObjectType)
		{
		case OBJECT_POLYGON:
			if (NewObject.Info2 == 0)
			{
				NewObjectPolygon->Layer = NewObject.Layer;
				NewObjectPolygon->Info = 0;
				NewObjectPolygon->PinNr = -1;

				if (AddObjectPolygon(NewObjectPolygon))
				{
					ObjectPolygon2 =
					    (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[NrObjectPolygons - 1]]);
					DrawObjectPolygon(ObjectPolygon2, 0.0, 0.0, 0);
				}
			}
			else
			{
				memset(&NewObjectLine, 0, sizeof(ObjectRecord));
				NewObjectLine.ObjectType = OBJECT_LINE;
				NewObjectLine.Layer = NewObject.Layer;
				NewObjectLine.PinNr = -1;
				NewObjectLine.Thickness = NewObject.Thickness;
				NewObjectLine.Clearance = NewObject.Clearance;
				count = NewObjectPolygon->NrVertices;

				if ((NewObjectPolygon->NrSubPolygons > 0) && (NewObjectPolygon->NrVerticesMainPolygon > 0))
					count = NewObjectPolygon->NrVerticesMainPolygon;

				for (cnt = 0; cnt < count; cnt++)
				{
					NewObjectLine.x1 = (float) NewObjectPolygon->Points[cnt].x;
					NewObjectLine.y1 = (float) NewObjectPolygon->Points[cnt].y;

					if (cnt < NewObjectPolygon->NrVertices - 1)
					{
						NewObjectLine.x2 = (float) NewObjectPolygon->Points[cnt + 1].x;
						NewObjectLine.y2 = (float) NewObjectPolygon->Points[cnt + 1].y;
					}
					else
					{
						NewObjectLine.x2 = (float) NewObjectPolygon->Points[0].x;
						NewObjectLine.y2 = (float) NewObjectPolygon->Points[0].y;
					}

					if (AddObject(&NewObjectLine))
						DrawObject(&NewObjectLine, 0.0, 0.0, 0);
				}
			}

			break;

		case OBJECT_LINE:
			if (AddObject(&NewObject))
				DrawObject(&NewObject, 0.0, 0.0, 0);

			if ((NewObject.Info2 != 0) && (NewObjectText2.Text[0] != 0))
			{
				if (AddObject(&NewObjectText2))
					DrawObject(&NewObjectText2, 0.0, 0.0, 0);
			}

			break;

		default:
			if ((NewObject.Layer == GEOM_NAME_LAYER) && (NewObject.ObjectType == OBJECT_TEXT))
			{
				Found = -1;

				for (cnt = 0; cnt < NrObjects; cnt++)
				{
					Object = &((*Objects)[cnt]);

					if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if ((Object->Layer == GEOM_NAME_LAYER) && (Object->ObjectType == OBJECT_TEXT))
							Found = cnt;
					}
				}

				if (Found != -1)
				{
					if (AddObject(&NewObject))
					{
						Object = &((*Objects)[Found]);
						SetBackGroundActive(0);
						DrawObject(Object, 0.0, 0.0, 0);
						Object->Info |= OBJECT_NOT_VISIBLE;
						Object->DeleteNr = (int16) LastActionNr;
						DrawObject(&NewObject, 0.0, 0.0, 0);
						sprintf(EditFile, "%s\\%s.shp", DesignPath, NewObject.Text);
						CheckFilenameForSaving = 1;
					}
				}
				else
				{
					if (AddObject(&NewObject))
						DrawObject(&NewObject, 0.0, 0.0, 0);
				}
			}
			else
			{
				if (NewObject.ObjectType == OBJECT_ARC)
				{
					x1 = NewObject.x1;
					y1 = NewObject.y1;
					x2 = NewObject.x2;
					y2 = NewObject.y2;
					x3 = NewObject.x3;
					y3 = NewObject.y3;
					x4 = NewObject.x4;
					y4 = NewObject.y4;
					ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &Length1);
					ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &Length2);

					if (InRangeSpecial(Angle1, Angle2, 0.001))
					{
						NewObject.x3 = 0.0;
						NewObject.y3 = (float) x2;
						NewObject.x4 = 0.0;
						NewObject.y4 = (float) x2;
					}

					ok = 1;
				}

				if (AddObject(&NewObject))
					DrawObject(&NewObject, 0.0, 0.0, 0);
			}

			break;
		}
	}

	SetScrollPageSize();
	SetScrollPosition();
	ExitDrawing();
	EndDrawingEditingWindow();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ConvertPoint(double CurrentX, double CurrentY, double *NewX, double *NewY)
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

void InitInfoStr(double CurrentX, double CurrentY, int32 ObjectType, int32 mode)
{
	char str[MAX_LENGTH_STRING];
	double x4, x3, y3, x, y, Length, Angle;
	int32 OkToDraw = 0;

  //int32  mode=0;

	ConvertPointToPolar(CurrentX - CurrentX2, CurrentY - CurrentY2, &Length, &Angle);
	x = fabs(CurrentX - CurrentX2);
	y = fabs(CurrentY - CurrentY2);
	x3 = ConvertUnits(x, Units);
	y3 = ConvertUnits(y, Units);
	x4 = max(x3 * 2, y3 * 2);
	Length = ConvertUnits(Length, Units);

	switch (ObjectType)
	{
	case OBJECT_LINE:
	case OBJECT_POLYGON:
		if (mode > 0)
		{
			switch (Units)
			{
			case 0:
				sprintf(str, "%.1f , %.1f", Length, Angle);
				break;

			case 1:
				sprintf(str, "%.4f , %.1f", Length, Angle);
				break;
			}

			OkToDraw = 1;
		}

		break;

	case OBJECT_RECT:
		if (mode == 1)
		{
			switch (Units)
			{
			case 0:
				sprintf(str, "%.1f , %.1f", x3, y3);
				break;

			case 1:
				sprintf(str, "%.4f , %.4f", x3, y3);
				break;
			}

			OkToDraw = 1;
		}

		break;

	case OBJECT_ARC:
		switch (mode)
		{
		case 1:
			switch (NewObject.Layer)
			{
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
				switch (Units)
				{
				case 0:
					sprintf(str, "%.1f", x4);
					break;

				case 1:
					sprintf(str, "%.4f", x4);
					break;
				}

				break;

			default:
				if (NewObject.Layer < 32)
				{
					switch (Units)
					{
					case 0:
						sprintf(str, "%.1f", x4);
						break;

					case 1:
						sprintf(str, "%.4f", x4);
						break;
					}
				}
				else
				{
					switch (Units)
					{
					case 0:
						sprintf(str, "%.1f , %.1f", x3 * 2, y3 * 2);
						break;

					case 1:
						sprintf(str, "%.4f , %.4f", x3 * 2, y3 * 2);
						break;
					}
				}

				break;
			}

			OkToDraw = 1;
			break;

		case 2:
		case 3:
			ConvertPointToPolar(CurrentX - NewObject.x1, CurrentY - NewObject.y1, &Length, &Angle);
			sprintf(str, "%.1f", Angle);
			OkToDraw = 1;
			break;
		}

		break;

	case OBJECT_CIRCLE:
		if (mode == 1)
		{
			switch (Units)
			{
			case 0:
				sprintf(str, "%.1f", x4);
				break;

			case 1:
				sprintf(str, "%.4f", x4);
				break;
			}

			OkToDraw = 1;
		}

		break;
	}

	if (OkToDraw)
	{
		strcpy(InfoStr, str);
		RedrawInfoStr(1);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddObjects(int32 ObjectType, double LineThickNess, int32 Layer, int32 ObjectMode)
{
	int32 cnt, cnt2, cnt3, Mode, NrParams, ok, InsertMode, Mode2, Found, count, result;
	double OldX, OldY, CurrentX, CurrentY, NewX, NewY, FirstX, FirstY, CursorX, CursorY, ShiftOffsetX, ShiftOffsetY;
	float Scale;
	int32 OkToAddObjectPolygon;
	ObjectRecord2 TypeObject;
	ObjectRecord *Object, ChangedTextObject;
	HMENU PopUpMenu;
	int32 DisplayObjectOnEscape = 1;
	int32 LeftButtonFinishPolygon = 0;
	char str[4096];
	char TextLine[4096];
	LPSTR TextP, TextP2;
	uint8 Buf[4096];
	DrawXorFunctionRecord DrawXorFunction;

    //PopUpMenu=CreatePopupMenu();
    //AppendMenu(PopUpMenu,MF_ENABLED|MF_STRING,ID_ESCAPE,SC(154,"Escape"));

	SelectionActive = 1;
	SelectionEsc = 0;
	FinishPolygon = 0;
	RepeatMode = 0;

    //CurrentDrawMode=1;

	memset(&NewObject, 0, sizeof(ObjectRecord));
	memset(&NewObjectText2, 0, sizeof(ObjectRecord));
	NewObject.ObjectType = (int16) ObjectType;
	NewObject.Layer = Layer;
	NewObject.PinNr = -1;
	NewObjectText2.ObjectType = OBJECT_TEXT;
	NewObjectText2.Layer = Layer;
	NewObjectText2.PinNr = -1;
	NewObjectText2.x2 = (float) DimensionHeight;
	NewObjectText2.Thickness = (float) CurrentSilkscreenLine;
	InsertMode = 0;
	OkToAddObjectPolygon = 0;
	FirstX = 0.0;
	FirstY = 0.0;
	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	switch (Layer)
	{
	case SOLD_MASK_BOTTOM_LAYER:
	case SOLD_MASK_TOP_LAYER:
	case PASTE_MASK_BOTTOM_LAYER:
	case PASTE_MASK_TOP_LAYER:
		NewObject.Thickness = (float) LineThickNess;
		NewObject.Clearance = 0.0;
		break;

	case SILKSCREEN_TOP_LAYER:
	case SILKSCREEN_BOTTOM_LAYER:
		if (LineThickNess == 0)
			NewObject.Thickness = 0.0;
		else
			NewObject.Thickness = (float) CurrentSilkscreenLine;

		break;

	case COMP_OUTLINE_LAYER:
		if (LineThickNess == 0)
			NewObject.Thickness = 0.0;
		else
			NewObject.Thickness = (float) CurrentCompOutLine;

		break;

	case GEOM_NAME_LAYER:
		NewObject.Thickness = (float) CurrentSilkscreenLine;
		break;

	case BOARD_OUTLINE_LAYER:
		NewObject.Thickness = (float) CurrentBoardOutLine;
		break;

	case INFO_LAYER:
	case INFO_LAYER2:
	case INFO_LAYER3:
	case INFO_LAYER4:
		if (LineThickNess == 0)
			NewObject.Thickness = 0.0;
		else
			NewObject.Thickness = (float) CurrentInfoLine;

		break;

	case DRILL_LAYER:
		NewObject.Clearance = (float) CurrentClearance;
		break;

	default:
		if ((Layer >= 0) && (Layer < 32))
		{
			NewObject.Thickness = (float) LineThickNess;
			NewObject.Clearance = (float) CurrentClearance;
		}

		break;
	}

	Mode = 0;

	switch (NewObject.ObjectType)
	{
	case OBJECT_POLYGON:
		memset(NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));
		NewObjectPolygon->Layer = POLYGON_DRAW_LAYER;
		NewObjectPolygon->PinNr = -1;
		NewObjectPolygon->Info = OBJECT_SPECIAL_DRAW;
		NewObjectPolygon->minx = -1000000000.0;
		NewObjectPolygon->miny = -1000000000.0;
		NewObjectPolygon->maxx = 1000000000.0;
		NewObjectPolygon->maxy = 1000000000.0;
		NewObject.Info2 = ObjectMode;

		/*
		      if ((ObjectMode & 0x0f)==1) {
		        Mode=1;
		      }
		*/

		break;

	case OBJECT_LINE:
		if ((ObjectMode & 0x100) == 0x100)
			Mode = 1;

		NewObject.Info2 = (int16) (ObjectMode & 0x0f);
		break;

	case OBJECT_CIRCLE:
		if ((ObjectMode & 0x10) == 0x10)
			Mode = 1;

		NewObject.Info2 = (int16) (ObjectMode & 0x0f);
		break;

	case OBJECT_RECT:
		NewObject.Info2 = (int16) ((ObjectMode >> 1) & 0x0f);

		if ((ObjectMode & 1) == 1)
			Mode = 1;

		break;

	case OBJECT_TEXT:
		if (NewObject.x2 == 0.0)
			NewObject.x2 = CurrentTextHeight;

		if (Layer == GEOM_NAME_LAYER)
		{
			if (EditFile[0] != 0)
			{
				GetSymbolNameFromFileName(EditFile, str);
				strcpy(NewObject.Text, str);
			}

			Found = -1;

			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if ((Object->Layer == GEOM_NAME_LAYER) && (Object->ObjectType == OBJECT_TEXT))
						Found = cnt;
				}
			}

			if (Found != -1)
			{
				Object = &((*Objects)[Found]);
				NewObject.x2 = Object->x2;
			}
		}

		if (TextInputDialog(&NewObject, 0) == 2)
			return;

		if (strlen(NewObject.Text) == 0)
			return;

		CurrentTextHeight = NewObject.x2;
		NewObject.Text[sizeof(NewObject.Text) - 1] = 0;
		NewObject.Info2 = (int16) (ObjectMode & 0x0f);
		break;

	}

	ShiftOffsetX = 0.0;
	ShiftOffsetY = 0.0;
	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	ClipMouseCursor();
	CrossHairType = 0;
	DrawTryingObject(CurrentX, CurrentY, Mode);

	if (NewObject.ObjectType == OBJECT_POLYGON)
		DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);

	SystemBusyMode = 1;
	Mode2 = Mode | 0x40;
	DrawXorFunction.Function10a = (FUNCP1) DrawTryingObject2a;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Mode2;
	DrawXorFunction.Mode = 9;
	DrawXorFunction.Function10b = (FUNCP1) DrawTryingObject2b;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Mode2;
	ZoomInOutProcessed = 0;

 // *******************************************************************************************************
	while ((!SelectionEsc) && ((NewObject.ObjectType != OBJECT_POLYGON) || (!FinishPolygon)))
	{
     // *******************************************************************************************************
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				DrawTryingObject(OldX, OldY, Mode | 0x40);

				if (((NewObject.ObjectType == OBJECT_LINE) || (NewObject.ObjectType == OBJECT_POLYGON))
				        && (!LinesAllDirection) && (Mode > 0))
				{
					ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}

         // *******************************************************************************************************
			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(OldX, OldY, Mode | 0x40);

				DrawTryingObject(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
				{
					ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}

         // *******************************************************************************************************
			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(OldX, OldY, Mode | 0x40);

				DrawTryingObject(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
				{
					ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}

         // *******************************************************************************************************
			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(OldX, OldY, Mode | 0x40);

				DrawTryingObject(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
				{
					ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}

         // *******************************************************************************************************
			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(OldX, OldY, Mode | 0x40);

				DrawTryingObject(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

				if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
				{
					ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
					CurrentX = NewX;
					CurrentY = NewY;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}

			InitInfoStr(CurrentX, CurrentY, NewObject.ObjectType, Mode);
			DisplayCursorPosition();
			MouseChanged = 0;
		}

     // *******************************************************************************************************

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObject(CurrentX, CurrentY, Mode);

			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);

			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(OldX, OldY, Mode | 0x40);

			DrawTryingObject(OldX, OldY, Mode);

			/*
			      if (NewObject.ObjectType==OBJECT_POLYGON) {
			        DrawTryingObject(CurrentX,CurrentY,Mode|0x40);
			      }
			*/

			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
			{
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}
			else
				DisplayObjectOnEscape = 0;
		}

     // *******************************************************************************************************
		if ((ZoomActive()) && (!SelectionEsc))
		{
			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(OldX, OldY, Mode | 0x40);

			DrawTryingObject(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
			{
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}
			else
				DisplayObjectOnEscape = 0;
		}

     // *******************************************************************************************************
		if ((PanActive()) && (!SelectionEsc))
		{
			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(OldX, OldY, Mode | 0x40);

			DrawTryingObject(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
			{
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}
			else
				DisplayObjectOnEscape = 0;
		}

     // *******************************************************************************************************
     // *******************************************************************************************************
		if (TraceBackWardsKeyPressed)
		{
			if (NewObjectPolygon->NrVertices > 0)
			{
				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(OldX, OldY, Mode | 0x40);

				DrawTryingObject(OldX, OldY, Mode);
				count = NewObjectPolygon->NrVertices;
				CurrentX2 = NewObjectPolygon->Points[count - 1].x;
				CurrentY2 = NewObjectPolygon->Points[count - 1].y;
				NewObjectPolygon->NrVertices--;
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}

			TraceBackWardsKeyPressed = 0;
		}

     // *******************************************************************************************************
     // *******************************************************************************************************
		if (CheckLeftButton())
		{
#ifdef _DEBUG

			if (NewObjectPolygon->NrVertices == 1)
				ok = 1;

#endif

			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(OldX, OldY, Mode | 0x40);

			DrawTryingObject(OldX, OldY, Mode);

			if ((SnapMode & 1) == 1)
			{
				ShiftOffsetX = 0.0;
				ShiftOffsetY = 0.0;
				CursorX = PixelToRealOffX(MousePosX);
				CursorY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
				AdjustOffsetForSnap(CursorX, CursorY, CurrentX, CurrentY, 0.0, 0.0, &ShiftOffsetX, &ShiftOffsetY, 3);
				CurrentX -= ShiftOffsetX;
				CurrentY -= ShiftOffsetY;
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);

				OldX = CurrentX;
				OldY = CurrentY;

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(OldX, OldY, Mode | 0x40);

				DrawTryingObject(OldX, OldY, Mode);
			}

			switch (NewObject.ObjectType)
			{
			case OBJECT_POLYGON:
				if (Mode > 0)
				{
					cnt = NewObjectPolygon->NrVertices - 1;

					if ((NotInRange(NewObjectPolygon->Points[cnt].x, CurrentX))
					        || (NotInRange(NewObjectPolygon->Points[cnt].x, CurrentY)))
					{
						if ((NewObjectPolygon->NrVertices > 1) && (InRange(FirstX, CurrentX))
						        && (InRange(FirstY, CurrentY)))
						{
							FinishPolygon = 1;
							LeftButtonFinishPolygon = 1;
							DisplayObjectOnEscape = 0;
							InsertMode = 1;
						}
						else
						{
							cnt = NewObjectPolygon->NrVertices;
							NewObjectPolygon->Points[cnt].x = CurrentX;
							NewObjectPolygon->Points[cnt].y = CurrentY;
							NewObjectPolygon->NrVertices++;

							if (NewObjectPolygon->NrVertices == 512)
							{
								FinishPolygon = 1;
								LeftButtonFinishPolygon = 1;
								DisplayObjectOnEscape = 0;
								InsertMode = 1;
							}
						}
					}

					CurrentX2 = CurrentX;
					CurrentY2 = CurrentY;
				}
				else
				{
					FirstX = CurrentX;
					FirstY = CurrentY;
					NewObjectPolygon->Points[0].x = CurrentX;
					NewObjectPolygon->Points[0].y = CurrentY;
					NewObjectPolygon->NrVertices++;
				}

				Mode = 1;
#ifdef _DEBUG

				if (NewObjectPolygon->NrVertices == 2)
					ok = 1;

#endif
				Mode2 = Mode | 0x40;
				break;

			case OBJECT_LINE:
				if (Mode >= 1)
				{
					RelX = CurrentX2;
					RelY = CurrentY2;

					if ((NotInRange(NewObject.x1, NewObject.x2)) || (NotInRange(NewObject.y1, NewObject.y2)))
						CommandAddTryingObject(0);

					if (NewObject.Info2 != 0)
						SelectionEsc = 1;
				}
				else
				{
					RelX = CurrentX;
					RelY = CurrentY;
				}

				Mode++;
				Mode2 = Mode | 0x40;
				break;

			case OBJECT_RECT:
				if ((Mode >= 2) || ((NewObject.Thickness > 0.0) && (Mode >= 1)))
				{
					if ((NotInRange(NewObject.x2, 0.0)) && (NotInRange(NewObject.y2, 0.0)))
					{
						CommandAddTryingObject(0);
						SelectionEsc = 1;
						DisplayObjectOnEscape = 0;
					}
				}

				Mode++;
				Mode2 = Mode | 0x40;
				break;

			case OBJECT_CIRCLE:
				if ((Mode >= 2) || ((NewObject.Thickness > 0.0) && (Mode >= 1)))
				{
					if (NotInRange(NewObject.x2, 0.0))
					{
						CommandAddTryingObject(0);
						SelectionEsc = 1;
						DisplayObjectOnEscape = 0;
					}
				}

				Mode++;
				Mode2 = Mode | 0x40;
				break;

			case OBJECT_ARC:
				if ((Mode >= 3) && (Mode < 5))
				{
					if (NotInRange(NewObject.x2, 0.0))
						CommandAddTryingObject(0);

					SelectionEsc = 1;
					DisplayObjectOnEscape = 0;
				}

				OldX = CurrentX;
				OldY = CurrentY;
				CurrentX2 = CurrentX;
				CurrentY2 = CurrentY;

				if ((Mode == 1) && (((InRange(NewObject.x2, 0.0)) || (InRange(NewObject.y2, 0.0)))))
					MessageBoxUTF8(GEOMWindow, SC(441, "Arc width or height is zero"), SC(48, "Error"), MB_APPLMODAL | MB_OK);
				else
				{
					if (Mode != 5)
						Mode++;
					else
						Mode = 2;
				}

				Mode2 = Mode | 0x40;
				break;

			case OBJECT_TEXT:
				if (NewObject.x2 > 1000.0)
				{
					CommandAddTryingObject(0);
					SelectionEsc = 1;
					DisplayObjectOnEscape = 0;
				}

				break;
			}

			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;

			OldX = CurrentX;
			OldY = CurrentY;

			if (((NewObject.ObjectType == OBJECT_LINE) || (NewObject.ObjectType == OBJECT_POLYGON))
			        && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldDir = -1;
			CheckInputMessages(0);

			if (DisplayObjectOnEscape)
			{
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}
		}

     // *******************************************************************************************************
     // *******************************************************************************************************

		if (CheckRightButton(&DrawXorFunction) == 1)
		{

            //if (RightButtonPressed) {
            //TrackPopupMenu(PopUpMenu,TPM_RIGHTBUTTON,
            //MousePosX+5,MousePosY+40,0,SCHWindow,NULL);

			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(OldX, OldY, Mode | 0x40);

			DrawTryingObject(OldX, OldY, Mode);

			if (NewObject.ObjectType == OBJECT_TEXT)
				Mode ^= 1;

			if ((NewObject.ObjectType == OBJECT_LINE) || (NewObject.ObjectType == OBJECT_POLYGON))
			{
				PopUpMenu = CreatePopupMenu();

				if (NewObject.ObjectType == OBJECT_POLYGON)
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_FINISH_POLYGON, SC(148, "Finish"));

				if ((SnapMode & 1) == 0)
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_SNAP_MODE_ON, SC(149, "Snap on"));
				else
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_CHANGE_SNAP_MODE_OFF, SC(150, "Snap off"));

				if (LinesAllDirection)
				{
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_45_DIR,
					           SC(151, "Draw with 45/90 degrees directions"));
				}
				else
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_LINES_ALL_DIR, SC(152, "Draw in all directions"));

				if (NewObjectPolygon->NrVertices > 0)
					AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_TRACE_BACKWARDS, SC(153, "Goto previous point"));

				TrackPopupMenu(PopUpMenu, TPM_LEFTALIGN + TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5,
				               RealWindow.top + MousePosY + 40, 0, GEOMWindow, NULL);
				DestroyMenu(PopUpMenu);
			}

            //RightButtonPressed=0;

			CheckInputMessages(0);
			DrawTryingObject(CurrentX, CurrentY, Mode);

			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
		}

     // *******************************************************************************************************
     // *******************************************************************************************************
		if (NrFunctionsInBuf > 0)
		{
			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(OldX, OldY, Mode | 0x40);

			DrawTryingObject(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				UnClipMouseCursor();

				switch (NewObject.ObjectType)
				{
				case OBJECT_LINE:
					memset(&TypeObject, 0, sizeof(ObjectRecord));

					if ((Mode & 1) == 0)
					{
						if (LineInputDialog(&TypeObject, SC(191, "Add line (x1,y1,x2,y2,x3,y3,x4,y4, .... )")) == 1)
						{
							if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) >= 0)
							{
								if (NrParams == 2)
								{
									if (!ParametersRelative)
									{
										NewObject.x1 = (float) ParamsFloat[0];
										NewObject.y1 = (float) ParamsFloat[1];
									}
									else
									{
										NewObject.x1 = (float) (RelX + ParamsFloat[0]);
										NewObject.y1 = (float) (RelY + ParamsFloat[1]);
									}

									CurrentX2 = NewObject.x1;
									CurrentY2 = NewObject.y1;
									Mode = 1;
								}

								if ((NrParams > 2) && ((NrParams & 1) == 0))
								{
									for (cnt = 0; cnt < NrParams / 2 - 1; cnt++)
									{
										if (!ParametersRelative)
										{
											NewObject.x1 = (float) ParamsFloat[cnt * 2];
											NewObject.y1 = (float) ParamsFloat[cnt * 2 + 1];
											NewObject.x2 = (float) ParamsFloat[cnt * 2 + 2];
											NewObject.y2 = (float) ParamsFloat[cnt * 2 + 3];
										}
										else
										{
											NewObject.x1 = (float) (RelX + ParamsFloat[cnt * 2]);
											NewObject.y1 = (float) (RelY + ParamsFloat[cnt * 2 + 1]);
											NewObject.x2 = (float) (RelX + ParamsFloat[cnt * 2 + 2]);
											NewObject.y2 = (float) (RelY + ParamsFloat[cnt * 2 + 3]);
										}

										CommandAddTryingObject(0);
									}

									CurrentX2 = NewObject.x2;
									CurrentY2 = NewObject.y2;
									Mode = 1;
								}
							}
						}
					}
					else
					{
						if (LineInputDialog(&TypeObject, SC(192, "Add line to (x2,y2)")) == 1)
						{
							if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 2)
							{
								if (!ParametersRelative)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];
								}
								else
								{
									NewObject.x2 = (float) (CurrentX2 + ParamsFloat[0]);
									NewObject.y2 = (float) (CurrentY2 + ParamsFloat[1]);
								}

								CurrentX2 = NewObject.x2;
								CurrentY2 = NewObject.y2;
								CommandAddTryingObject(0);
							}
						}
					}

					break;

				case OBJECT_RECT:
					memset(&TypeObject, 0, sizeof(ObjectRecord));

					switch (Mode)
					{
					case 0:
						if (LineInputDialog(&TypeObject, SC(193, "Add rectangle (width,height,[CenterX,CenterY])")) ==
						        1)
						{
							if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) >= 0)
							{
								if (NrParams == 2)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];
									Mode = 2;
								}

								if (NrParams == 4)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];

									if (!ParametersRelative)
									{
										NewObject.x1 = (float) ParamsFloat[2];
										NewObject.y1 = (float) ParamsFloat[3];
									}
									else
									{
										NewObject.x1 = (float) (RelX + ParamsFloat[2]);
										NewObject.y1 = (float) (RelY + ParamsFloat[3]);
									}

									if ((NotInRange(NewObject.x2, 0.0)) || (NotInRange(NewObject.y2, 0.0)))
									{
										CommandAddTryingObject(0);
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
						if (LineInputDialog(&TypeObject, SC(194, "Add rectangle (width,height)")) == 1)
						{
							if (((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 2)
							        && (NotInRange(NewObject.x2, 0.0)) && (NotInRange(NewObject.y2, 0.0)))
							{
								NewObject.x2 = (float) ParamsFloat[0];
								NewObject.y2 = (float) ParamsFloat[1];
								CommandAddTryingObject(0);
								SelectionEsc = 1;
							}
						}

						ok = 1;
						break;

					case 2:
						if (LineInputDialog(&TypeObject, SC(195, "Add rectangle (CenterX,CenterY)")) == 1)
						{
							if (((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 2)
							        && (NotInRange(NewObject.x2, 0.0)) && (NotInRange(NewObject.y2, 0.0)))
							{
								if (!ParametersRelative)
								{
									NewObject.x1 = (float) ParamsFloat[0];
									NewObject.y1 = (float) ParamsFloat[1];
								}
								else
								{
									NewObject.x1 = (float) (RelX + ParamsFloat[0]);
									NewObject.y1 = (float) (RelY + ParamsFloat[1]);
								}

								CommandAddTryingObject(0);
								SelectionEsc = 1;
							}
						}

						ok = 1;
					}

					break;

				case OBJECT_CIRCLE:
					memset(&TypeObject, 0, sizeof(ObjectRecord2));

					switch (Mode)
					{
					case 0:
						switch (Layer)
						{
						case DRILL_LAYER:
						case DRILL_UNPLATED_LAYER:
							result =
							    LineInputDialog(&TypeObject, SC(196, "Add drill hole (diameter,[CenterX,CenterY])"));
							break;

						default:
							result = LineInputDialog(&TypeObject, SC(197, "Add circle (diameter,[CenterX,CenterY])"));
						}

						if (result == 1)
						{
							if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) >= 0)
							{
								if (NrParams == 1)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[0];
									Mode = 2;
								}

								if (NrParams == 3)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[0];

									if (!ParametersRelative)
									{
										NewObject.x1 = (float) ParamsFloat[1];
										NewObject.y1 = (float) ParamsFloat[2];
									}
									else
									{
										NewObject.x1 = (float) (RelX + ParamsFloat[1]);
										NewObject.y1 = (float) (RelY + ParamsFloat[2]);
									}

									if (NotInRange(NewObject.x2, 0.0))
									{
										CommandAddTryingObject(0);
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
						switch (Layer)
						{
						case DRILL_LAYER:
						case DRILL_UNPLATED_LAYER:
							result = LineInputDialog(&TypeObject, SC(198, "Add drill hole (diameter)"));
							break;

						default:
							result = LineInputDialog(&TypeObject, SC(199, "Add circle (diameter)"));
						}

						if (result == 1)
						{
							if (((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 1)
							        && (NotInRange(ParamsFloat[0], 0.0)))
							{
								NewObject.x2 = (float) ParamsFloat[0];
								NewObject.y2 = (float) ParamsFloat[0];
								CommandAddTryingObject(0);
								SelectionEsc = 1;
							}
						}

						ok = 1;
						break;

					case 2:
						switch (Layer)
						{
						case DRILL_LAYER:
						case DRILL_UNPLATED_LAYER:
							result = LineInputDialog(&TypeObject, SC(200, "Add drill hole (CenterX,CenterY)"));
							break;

						default:
							result = LineInputDialog(&TypeObject, SC(201, "Add circle (CenterX,CenterY)"));
						}

						if (result == 1)
						{
							if (((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 2)
							        && (NotInRange(NewObject.x2, 0.0)))
							{
								if (!ParametersRelative)
								{
									NewObject.x1 = (float) ParamsFloat[0];
									NewObject.y1 = (float) ParamsFloat[1];
								}
								else
								{
									NewObject.x1 = (float) (RelX + ParamsFloat[0]);
									NewObject.y1 = (float) (RelY + ParamsFloat[1]);
								}

								CommandAddTryingObject(0);
								SelectionEsc = 1;
							}
						}

						ok = 1;
					}

					break;

				case OBJECT_ARC:
					memset(&TypeObject, 0, sizeof(ObjectRecord));

					switch (Mode)
					{
					case 0:
						if (LineInputDialog
						        (&TypeObject,
						         SC(202, "Add arc (width,height), [CenterX,CenterY], [StartAngle,EndAngle])")) == 1)
						{
							if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) >= 0)
							{
								if (NrParams == 2)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];
									Mode = 5;
								}

								if (NrParams == 4)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];

									if (!ParametersRelative)
									{
										NewObject.x1 = (float) ParamsFloat[2];
										NewObject.x1 = (float) ParamsFloat[3];
									}
									else
									{
										NewObject.x1 = (float) (RelX + ParamsFloat[2]);
										NewObject.x1 = (float) (RelY + ParamsFloat[3]);
									}

									Mode = 2;
								}

								if (NrParams == 6)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];

									if (!ParametersRelative)
									{
										NewObject.x1 = (float) ParamsFloat[2];
										NewObject.x1 = (float) ParamsFloat[3];
									}
									else
									{
										NewObject.x1 = (float) (RelX + ParamsFloat[2]);
										NewObject.x1 = (float) (RelY + ParamsFloat[3]);
									}

									NewObject.x3 = (float) cos(ANGLE_CONVERT(ConvertUnits(ParamsFloat[4], Units)));
									NewObject.y3 = (float) sin(ANGLE_CONVERT(ConvertUnits(ParamsFloat[4], Units)));
									NewObject.x4 = (float) cos(ANGLE_CONVERT(ConvertUnits(ParamsFloat[5], Units)));
									NewObject.y4 = (float) sin(ANGLE_CONVERT(ConvertUnits(ParamsFloat[5], Units)));
									NewObject.x3 *= NewObject.x2;
									NewObject.y3 *= NewObject.y2;
									NewObject.x4 *= NewObject.x2;
									NewObject.y4 *= NewObject.y2;
									CommandAddTryingObject(0);
									SelectionEsc = 1;
								}
							}
						}

						break;

					case 1:
						if (LineInputDialog(&TypeObject, SC(203, "Add arc (width,height), [StartAngle,EndAngle]")) == 1)
						{
							if (((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) >= 2)
							        && (NotInRange(ParamsFloat[0], 0.0)) && (NotInRange(ParamsFloat[1], 0.0)))
							{
								if (NrParams == 2)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];
									Mode = 2;
								}

								if (NrParams == 4)
								{
									NewObject.x2 = (float) ParamsFloat[0];
									NewObject.y2 = (float) ParamsFloat[1];

									if (!ParametersRelative)
									{
										NewObject.x1 = (float) ParamsFloat[2];
										NewObject.x1 = (float) ParamsFloat[3];
									}
									else
									{
										NewObject.x1 = (float) (RelX + ParamsFloat[2]);
										NewObject.x1 = (float) (RelY + ParamsFloat[3]);
									}

									NewObject.x3 = (float) cos(ANGLE_CONVERT(ConvertUnits(ParamsFloat[4], Units)));
									NewObject.y3 = (float) sin(ANGLE_CONVERT(ConvertUnits(ParamsFloat[4], Units)));
									NewObject.x4 = (float) cos(ANGLE_CONVERT(ConvertUnits(ParamsFloat[5], Units)));
									NewObject.y4 = (float) sin(ANGLE_CONVERT(ConvertUnits(ParamsFloat[5], Units)));
									NewObject.x3 *= NewObject.x2;
									NewObject.y3 *= NewObject.y2;
									NewObject.x4 *= NewObject.x2;
									NewObject.y4 *= NewObject.y2;
									CommandAddTryingObject(0);
									SelectionEsc = 1;
								}
							}
						}

						break;

					case 2:
						if (LineInputDialog(&TypeObject, SC(204, "Add arc (StartAngle,EndAngle)")) == 1)
						{
							if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 2)
							{
								NewObject.x3 = (float) cos(ANGLE_CONVERT(ConvertUnits(ParamsFloat[0], Units)));
								NewObject.y3 = (float) sin(ANGLE_CONVERT(ConvertUnits(ParamsFloat[0], Units)));
								NewObject.x4 = (float) cos(ANGLE_CONVERT(ConvertUnits(ParamsFloat[1], Units)));
								NewObject.y4 = (float) sin(ANGLE_CONVERT(ConvertUnits(ParamsFloat[1], Units)));
								NewObject.x3 *= NewObject.x2;
								NewObject.y3 *= NewObject.y2;
								NewObject.x4 *= NewObject.x2;
								NewObject.y4 *= NewObject.y2;
								CommandAddTryingObject(0);
								SelectionEsc = 1;
							}
						}

						break;
					}

					break;

				case OBJECT_TEXT:
					memset(&TypeObject, 0, sizeof(ObjectRecord));

					if (Mode == 0)
					{
						if (LineInputDialog(&TypeObject, SC(205, "Add text (x,y)")) == 1)
						{
							if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 2)
							{
								if (!ParametersRelative)
								{
									NewObject.x1 = (float) ParamsFloat[0];
									NewObject.y1 = (float) ParamsFloat[1];
								}
								else
								{
									NewObject.x1 = (float) (RelX + ParamsFloat[0]);
									NewObject.y1 = (float) (RelY + ParamsFloat[1]);
								}

								CommandAddTryingObject(0);
								SelectionEsc = 1;
							}
						}
					}

					break;

				case OBJECT_POLYGON:
					memset(&TextLine, 0, sizeof(TextLine));
					TextP = (LPSTR) & Buf[0];
					*TextP = 0;

					if (TextInputDialog2
					        ((LPSTR) & Buf[0], SC(206, "Add polygon points x1,y1,x2,y2,x3,y3,x4,y4, .... )"), 0) == 1)
					{
						int32 polyParametersRelative = 0;

						cnt = 0;
						NewObjectPolygon->NrVertices = 0;

						while ((*TextP != 0) && (NewObjectPolygon->NrVertices < 512))
						{
							TextP2 = TextP;
							memset(&str, 0, sizeof(str));
							cnt3 = 0;

							while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
							{
								str[cnt3++] = *TextP;
								str[cnt3] = 0;
								TextP++;
							}

							if (*TextP == ',')
								TextP++;

							if (*TextP == '\r')
								TextP++;

							if (*TextP != 0)
							{
								str[cnt3++] = ',';
								str[cnt3] = 0;

								while ((*TextP != 0) && (*TextP != ',') && (*TextP != '\r'))
								{
									str[cnt3++] = *TextP;
									str[cnt3] = 0;
									TextP++;
								}

								if (*TextP == ',')
									TextP++;

								if (*TextP == '\r')
									TextP++;

								if (((NrParams = ScanParameters(-1, (char *) str, 0)) >= 2) && ((NrParams & 1) == 0))
								{
									if (ParametersRelative)
										polyParametersRelative = 1;

									for (cnt2 = 0; cnt2 < NrParams / 2; cnt2++)
									{
										if (!polyParametersRelative)
										{
											if (NewObjectPolygon->NrVertices < 200)
											{
												NewObjectPolygon->Points[cnt].x = ParamsFloat[cnt2 * 2];
												NewObjectPolygon->Points[cnt].y = ParamsFloat[cnt2 * 2 + 1];
												cnt++;
												NewObjectPolygon->NrVertices++;
											}
										}
										else
										{
											if (NewObjectPolygon->NrVertices < 200)
											{
												NewObjectPolygon->Points[cnt].x = RelX + ParamsFloat[cnt2 * 2];
												NewObjectPolygon->Points[cnt].y = RelY + ParamsFloat[cnt2 * 2 + 1];
												cnt++;
												NewObjectPolygon->NrVertices++;
											}
										}
									}
								}
							}
						}

						if (NewObjectPolygon->NrVertices > 2)
						{
							FinishPolygon = 1;
							InsertMode = 2;
							memset(&ChangedTextObject, 0, sizeof(ChangedTextObject));
							strcpy(ChangedTextObject.Text, "1.00000");
							TextInputDialog(&ChangedTextObject, 0x13);
							sscanf(ChangedTextObject.Text, "%f", &Scale);

							if ((Scale < 0.0001) || (Scale > 10000.0))
								Scale = 1.0;

							if (!InRange7(Scale, 1.0))
							{
								for (cnt = 0; cnt < NewObjectPolygon->NrVertices; cnt++)
								{
									NewObjectPolygon->Points[cnt].x *= Scale;
									NewObjectPolygon->Points[cnt].y *= Scale;
								}
							}
						}
					}

					break;
				}

				ClipMouseCursor();
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				switch (NewObject.ObjectType)
				{
				case OBJECT_LINE:
					Help("add_line_objects.htm", 0);
					break;

				case OBJECT_RECT:
					Help("add_rectange_objects.htm", 0);
					break;

				case OBJECT_CIRCLE:
					Help("add_circle_objects.htm", 0);
					break;

				case OBJECT_ARC:
					Help("add_arc_objects.htm", 0);
					break;

				case OBJECT_TEXT:
					Help("add_text_objects.htm", 0);
					break;
				}

				switch (NewObject.Layer)
				{
				case DRILL_LAYER:
				case DRILL_UNPLATED_LAYER:
					Help("add_drill.htm", 0);
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

			if ((NewObject.ObjectType == OBJECT_LINE) && (!LinesAllDirection))
			{
				ConvertPoint(CurrentX, CurrentY, &NewX, &NewY);
				CurrentX = NewX;
				CurrentY = NewY;
			}

			OldX = CurrentX;
			OldY = CurrentY;

			if (SelectionEsc)
				DisplayObjectOnEscape = 0;
			else
			{
				DrawTryingObject(CurrentX, CurrentY, Mode);

				if (NewObject.ObjectType == OBJECT_POLYGON)
					DrawTryingObject(CurrentX, CurrentY, Mode | 0x40);
			}

			ClipMouseCursor();
		}
	}

 // *******************************************************************************************************
 // *******************************************************************************************************
	if (FinishPolygon)
	{
		DisplayObjectOnEscape = 0;

		if (InsertMode == 0)
		{
			if (NewObject.ObjectType == OBJECT_POLYGON)
				DrawTryingObject(OldX, OldY, Mode | 0x40);

			DrawTryingObject(OldX, OldY, Mode);

			/*
			      if ((!LeftButtonFinishPolygon)
			         ||
			         (NotInRange(NewObjectLine.X1,NewObjectLine.X2))
			         ||
			         (NotInRange(NewObjectLine.Y1,NewObjectLine.Y2))) {
			        CommandAddTryingObject();
			      }
			*/

		}

		if ((ObjectMode & 32) == 0)
		{
			if (InsertMode < 2)
			{
				if (NewObjectPolygon->NrVertices > 2)
				{

                    //DrawTryingPolygon(Mult(NewObjectLine.LineThickNess),1);

					OkToAddObjectPolygon = 1;
				}
			}
			else
				OkToAddObjectPolygon = 1;
		}
	}

	if (OkToAddObjectPolygon == 1)
		CommandAddTryingObject(0);

	SelectionActive = 0;
	SystemBusyMode = 0;
	RePaint();
	UnClipMouseCursor();
	InfoStr[0] = 0;
	RedrawInfoStr(1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotateObjects2()
{
	int32 cnt;
	double hulp;
	ObjectRecord *Object;

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);
		RotateFlipPoint2(&Object->x1, &Object->y1, 0.0, 0.0, 1);

		if (Object->ObjectType == OBJECT_RECT)
		{
			hulp = Object->x2;
			Object->x2 = Object->y2;
			Object->y2 = (float) hulp;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandAddMultipleObjects(int32 mode)
{
	int32 NrParams, Mode2;
	double OldX, OldY, CurrentX, CurrentY, ShiftX, ShiftY;
	ObjectRecord2 TypeObject;
	int32 FirstShift;
	DrawXorFunctionRecord DrawXorFunction;

    //PopUpMenu=CreatePopupMenu();
    //AppendMenu(PopUpMenu,MF_ENABLED|MF_STRING,ID_ROTATE_MULTIPLE,"Rotate");

	SelectionActive = 1;
	SelectionEsc = 0;

    //CurrentDrawMode=1;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	CentreSelectedX = 0.0;
	CentreSelectedY = 0.0;
	ShiftX = 0.0;
	ShiftY = 0.0;

	ClipMouseCursor();
	DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
	FirstShift = 1;
	SystemBusyMode = 2;
	Mode2 = 1;
	DrawXorFunction.Function11 = (FUNCP10) DrawObjects2a;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &CentreSelectedX;
	DrawXorFunction.Param1[3] = &CentreSelectedY;
	DrawXorFunction.Param1[4] = &Mode2;
	DrawXorFunction.Mode = 10;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &CentreSelectedX;
	DrawXorFunction.Param2[3] = &CentreSelectedY;
	DrawXorFunction.Param2[4] = &Mode2;
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
					DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollRight(ScrollSize);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollDown(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollLeft(ScrollSize);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollUp(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{

                //DrawSelectedObjects(OldX,OldY,1);
				CentreSelectedX -= ShiftX - CurrentX;
				CentreSelectedY -= ShiftY - CurrentY;
				FirstShift = 1;
                //DrawSelectedObjects(OldX,OldY,1);

			}
		}
		else
		{
			if (FirstShift)
			{
				ShiftX = CurrentX;
				ShiftY = CurrentY;
				FirstShift = 0;

                //DrawSelectedObjects(CurrentX,CurrentY,1);

			}
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			DisplayCursorPosition();
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
		}

		if (CheckLeftButton())
		{
			DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;

			OldX = CurrentX;
			OldY = CurrentY;
			InsertObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY);

            //LastActionNr++;

			SelectionEsc = 1;

            //DrawObjects2(CurrentX-CentreSelectedX,CurrentY-CentreSelectedY,1);

		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{

            //if (RightButtonPressed) {

			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			RotateObjects2();

            //RightButtonPressed=0;

			DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectRecord));

				if (LineInputDialog(&TypeObject, SC(207, "Add objects (x1,y1)")) == 1)
				{
					if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) == 2)
					{
						if (!ParametersRelative)
							InsertObjects2(ParamsFloat[0] - CentreSelectedX, ParamsFloat[1] - CentreSelectedY);
						else
						{
							InsertObjects2(RelX + ParamsFloat[0] - CentreSelectedX,
							               RelY + ParamsFloat[1] - CentreSelectedY);
						}

						SelectionEsc = 1;
					}
				}
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				switch (mode)
				{
				case 0:
					Help("add_through_hole_pads.htm", 0);
					break;

				case 1:
					Help("add_rectangle_SMD_pads.htm", 0);
					break;

				case 2:
					Help("add_circle_SMD_pads.htm", 0);
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
			DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			ClipMouseCursor();
		}
	}

	SelectionActive = 0;
	SystemBusyMode = 0;
	DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
	UnClipMouseCursor();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawTryingObjectCross(double CurrentX, double CurrentY, int32 Mode)
{
	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);
	InitDrawingColorWhite(0);
	DrawLine(Mult(CurrentX - Xoffset) + DrawWindowMinX, 4000, Mult(CurrentX - Xoffset) + DrawWindowMinX, -4000);
	DrawLine(4000, DrawWindowMaxY - Mult(CurrentY - Yoffset) - 1, -4000, DrawWindowMaxY - Mult(CurrentY - Yoffset) - 1);
	ExitDrawing();
	EndDrawingEditingWindow();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CommandChangedCrosses(double OX, double OY, int32 Mode)
{
	int32 NrParams;
	double OldX, OldY, CurrentX, CurrentY;
	ObjectRecord2 TypeObject;
	int32 LeftButtonExit;
	DrawXorFunctionRecord DrawXorFunction;

    //PopUpMenu=CreatePopupMenu();
    //AppendMenu(PopUpMenu,MF_ENABLED|MF_STRING,ID_ESCAPE,SC(154,"Escape"));

	if (Mode == 2)
	{
		MoveAllObjects(OX, OY);
		DataBaseChanged = 1;
		UnselectAllObjects();
		PostMessage(GEOMWindow, WM_COMMAND, (WPARAM) ID_VIEW_VIEWFULL, (LPARAM) NULL);
		return;
	}

	SelectionActive = 1;
	SelectionEsc = 0;

	memset(&NewObject, 0, sizeof(ObjectRecord));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	LeftButtonExit = 0;

	OldX = CurrentX;
	OldY = CurrentY;
	OldDir = -1;
	DrawCrossHair(2);
	DrawTryingObjectCross(CurrentX, CurrentY, Mode);
	ClipMouseCursor();
	SystemBusyMode = 3;
	DrawXorFunction.Function1 = (FUNCP1) DrawTryingObjectCross;
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
				DrawTryingObjectCross(OldX, OldY, Mode);
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawTryingObjectCross(OldX, OldY, Mode);
				ScrollRight(ScrollSize);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawTryingObjectCross(OldX, OldY, Mode);
				ScrollDown(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawTryingObjectCross(OldX, OldY, Mode);
				ScrollLeft(ScrollSize);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawTryingObjectCross(OldX, OldY, Mode);
				ScrollUp(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
			}

         //InitInfoStr(CurrentX,CurrentY);
         //DisplayCursorPosition();

			MouseChanged = 0;
		}

		DisplayCursorPosition();

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawTryingObjectCross(CurrentX, CurrentY, Mode);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawTryingObjectCross(OldX, OldY, Mode);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawTryingObjectCross(OldX, OldY, Mode);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawTryingObjectCross(OldX, OldY, Mode);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawTryingObjectCross(CurrentX, CurrentY, Mode);
		}

		if (CheckLeftButton())
		{
			DrawTryingObjectCross(CurrentX, CurrentY, Mode);

			switch (Mode)
			{
			case 0:
				MoveAllObjects(CurrentX, CurrentY);
				DataBaseChanged = 1;
				break;

			case 1:
				Shape.InsertionX = (float) CurrentX;
				Shape.InsertionY = (float) CurrentY;
				DataBaseChanged = 1;
				break;
			}

			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;
			SelectionEsc = 1;
			OldX = CurrentX;
			OldY = CurrentY;
			CheckInputMessages(0);
		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{

         //if (RightButtonPressed) {

		}

		if (NrFunctionsInBuf > 0)
		{
			DrawTryingObjectCross(OldX, OldY, Mode);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectRecord));

				if (((Mode == 0) && (LineInputDialog(&TypeObject, SC(208, "Move origin to")) == 1))
				        || ((Mode != 0) && (LineInputDialog(&TypeObject, SC(209, "Move insertion point to")) == 1)))
				{
					if ((NrParams = ScanParameters(-1, (char *) TypeObject.Text, 0)) >= 0)
					{
						if (NrParams == 2)
						{
							switch (Mode)
							{
							case 0:
								MoveAllObjects(ParamsFloat[0], ParamsFloat[1]);
								DataBaseChanged = 1;
								break;

							case 1:
								Shape.InsertionX = (float) ParamsFloat[0];
								Shape.InsertionY = (float) ParamsFloat[1];
								DataBaseChanged = 1;
								break;
							}

							SelectionEsc = 1;
						}
					}
				}
			}

			SpacePressed = 0;

			if (HelpAsked)
			{
				Help(0, 0);
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
			DrawTryingObjectCross(CurrentX, CurrentY, Mode);
			ClipMouseCursor();
		}
	}

	SelectionActive = 0;
	ViewInsertionPoint = 1;
	UnClipMouseCursor();
	SystemBusyMode = 0;
	RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyObjectsToClipBoard(int32 mode)
{
	int32 cnt, NrPolygonObjects, TotalMemSize, count;
	uint8 *BufP, *OldBufP;
	ObjectRecord *Object;
	ObjectPolygonRecord *ObjectPolygon;

	if (!OpenClipboard(GEOMWindow))
		return -1;

	if (!EmptyClipboard())
		return -1;

	count = 0;
	TotalMemSize = 0;
	NrPolygonObjects = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			count++;
			TotalMemSize += sizeof(ObjectRecord);
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			NrPolygonObjects++;
			TotalMemSize += MemSizeObjectPolygon(ObjectPolygon);
		}
	}

	if (count + NrPolygonObjects == 0)
		return -1;

	if ((GlobalClipBoardMem = GlobalAlloc(GHND | GMEM_DDESHARE, TotalMemSize + 4096)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -1;

	*((int32 *) (ClipBoardMem)) = count;
	*((int32 *) (ClipBoardMem + 4)) = NrPolygonObjects;
	*((int32 *) (ClipBoardMem + 8)) = TotalMemSize + 12;
	BufP = ClipBoardMem + 12;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			memmove(BufP, Object, sizeof(ObjectRecord));
			BufP += sizeof(ObjectRecord);
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			memmove(BufP, ObjectPolygon, MemSizeObjectPolygon(ObjectPolygon));
			OldBufP = BufP;
			BufP += MemSizeObjectPolygon(ObjectPolygon);
		}
	}

	GlobalUnlock(GlobalClipBoardMem);

	if (SetClipboardData(ClipBoardGeomObjectsID, GlobalClipBoardMem) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		GlobalFree(GlobalClipBoardMem);
		return -1;
	}

	CloseClipboard();
	UnselectAllObjects();
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CopyObjectsFromClipBoard(int32 mode)
{
	typedef uint8 ByteArray[1000];

	int32 NrPolygonObjects, TotalMemSize, Count, TempBufP, Mode2;
	uint8 *BufP;
	HGLOBAL NewGlobalClipBoardMem;
	uint8 *NewClipBoardMem;
	ObjectRecord *Object;
	double OldX, OldY, CurrentX, CurrentY, ShiftX, ShiftY;
	double ObjectMinX, ObjectMinY, ObjectMaxX, ObjectMaxY;
	ObjectPolygonRecord *ObjectPolygon, *ObjectPolygon2;
	int32 FirstShift;
	DrawXorFunctionRecord DrawXorFunction;

	if (!OpenClipboard(GEOMWindow))
		return -2;

	if ((GlobalClipBoardMem = GetClipboardData(ClipBoardGeomObjectsID)) == NULL)
		return -1;

	if ((ClipBoardMem = GlobalLock(GlobalClipBoardMem)) == NULL)
		return -2;

	Count = *((int32 *) (ClipBoardMem));
	NrPolygonObjects = *((int32 *) (ClipBoardMem + 4));
	TotalMemSize = *((int32 *) (ClipBoardMem + 8));

	if ((NewGlobalClipBoardMem = GlobalAlloc(GHND, TotalMemSize)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	if ((NewClipBoardMem = GlobalLock(NewGlobalClipBoardMem)) == NULL)
	{
		GlobalUnlock(GlobalClipBoardMem);
		CloseClipboard();
		return -2;
	}

	memmove(NewClipBoardMem, ClipBoardMem, TotalMemSize);
	GlobalUnlock(GlobalClipBoardMem);
	CloseClipboard();

	BufP = NewClipBoardMem + 12;
	AllocateMemTemp(TotalMemSize);

	if (Count <= 0)
		return -1;

	if (AllocateMemObjects2(Count + NrPolygonObjects) == -1)
		return -1;

	ObjectMinX = 1e9;
	ObjectMinY = 1e9;
	NrObjects2 = 0;

	while (NrObjects2 < Count)
	{
		Object = &((*Objects2)[NrObjects2]);
		memmove(Object, BufP, sizeof(ObjectRecord));
		GetObjectSize(Object, &ObjectMinX, &ObjectMinY, &ObjectMaxX, &ObjectMaxY);
		Object->PinNr = -1;
		Object->Info &= ~(OBJECT_SELECTED | 3);
		BufP += sizeof(ObjectRecord);
		NrObjects2++;
	}

	TempBufP = 0;

	while (NrObjects2 < Count + NrPolygonObjects)
	{
		Object = &((*Objects2)[NrObjects2]);
		ObjectPolygon = (ObjectPolygonRecord *) & TempMem[TempBufP];
		ObjectPolygon2 = (ObjectPolygonRecord *) BufP;
		memmove(ObjectPolygon, BufP, MemSizeObjectPolygon(ObjectPolygon2));
		ObjectPolygon->PinNr = -1;
		ObjectMinX = min(ObjectMinX, ObjectPolygon->minx);
		ObjectMinY = min(ObjectMinY, ObjectPolygon->miny);
		memset(Object, 0, sizeof(ObjectRecord));
		Object->ObjectType = OBJECT_POLYGON;
		Object->Address = (uint8 *) ObjectPolygon;
		ObjectPolygon->Info &= ~OBJECT_SELECTED;
		BufP += MemSizeObjectPolygon(ObjectPolygon2);
		TempBufP += MemSizeObjectPolygon(ObjectPolygon2);
		NrObjects2++;
	}

	GlobalUnlock(NewGlobalClipBoardMem);
	GlobalFree(NewGlobalClipBoardMem);

 // ********************************************************************************************************
 // ********************************************************************************************************

	SelectionActive = 1;
	SelectionEsc = 0;

    //CurrentDrawMode=1;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	OldX = CurrentX;
	OldY = CurrentY;
	CentreSelectedX = ObjectMinX;
	CentreSelectedY = ObjectMinY;
	ShiftX = 0.0;
	ShiftY = 0.0;

	ClipMouseCursor();
	DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
	FirstShift = 1;
	SystemBusyMode = 4;
	Mode2 = 1;
	DrawXorFunction.Function11 = (FUNCP10) DrawObjects2a;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &CentreSelectedX;
	DrawXorFunction.Param1[3] = &CentreSelectedY;
	DrawXorFunction.Param1[4] = &Mode2;
	DrawXorFunction.Mode = 10;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &CentreSelectedX;
	DrawXorFunction.Param2[3] = &CentreSelectedY;
	DrawXorFunction.Param2[4] = &Mode2;
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
					DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollRight(ScrollSize);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollDown(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollLeft(ScrollSize);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
				ScrollUp(ScrollSize);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		if (!ShiftPressed)
		{
			if (!FirstShift)
			{

             //DrawSelectedObjects(OldX,OldY,1);

				CentreSelectedX -= ShiftX - CurrentX;
				CentreSelectedY -= ShiftY - CurrentY;
				FirstShift = 1;

             //DrawSelectedObjects(OldX,OldY,1);

			}
		}
		else
		{
			if (FirstShift)
			{
				ShiftX = CurrentX;
				ShiftY = CurrentY;
				FirstShift = 0;

                //DrawSelectedObjects(CurrentX,CurrentY,1);

			}
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			DisplayCursorPosition();
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			UnClipMouseCursor();
			CheckInputMessages(0);

			while (!Focused)
				CheckInputMessages(0);

			CheckInputMessages(0);
			ClipMouseCursor();

			if (!SelectionEsc)
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
		}

		if (CheckLeftButton())
		{
			DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			CurrentX2 = CurrentX;
			CurrentY2 = CurrentY;

			OldX = CurrentX;
			OldY = CurrentY;
			InsertObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY);

            //LastActionNr++;

			SelectionEsc = 1;

            //DrawObjects2(CurrentX-CentreSelectedX,CurrentY-CentreSelectedY,1);

		}

		if (CheckRightButton(&DrawXorFunction) == 1)
		{

         //if (RightButtonPressed) {
         //RightButtonPressed=0;

		}

		if (NrFunctionsInBuf > 0)
		{
			DrawObjects2(OldX - CentreSelectedX, OldY - CentreSelectedY, 1);
			UnClipMouseCursor();
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			FirstShift = 1;
			DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
			ClipMouseCursor();
		}
	}

	SelectionActive = 0;
	SystemBusyMode = 0;
	DrawObjects2(CurrentX - CentreSelectedX, CurrentY - CentreSelectedY, 1);
	UnClipMouseCursor();
	DeAllocateMemTemp();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeGeometryHeight(int32 mode)
{
	ObjectRecord TypeObject;
	float value, OldHeight, NewHeight = -1.0;

	OldHeight = Shape.ShapeHeight;
	memset(&TypeObject, 0, sizeof(ObjectRecord));

	if (Shape.ShapeHeight == 0)
		Shape.ShapeHeight = (float) 100000.0;

	switch (Units)
	{
	case 0:
		sprintf(TypeObject.Text, "%.1f thou", Shape.ShapeHeight / 2540.0);
		break;

	case 1:
		sprintf(TypeObject.Text, "%.4f mm", Shape.ShapeHeight / 100000.0);
		break;
	}

	if (TextInputDialog(&TypeObject, 0x17) == 1)
	{
		sscanf(TypeObject.Text, "%f", &value);

		switch (Units)
		{
		case 0:
			NewHeight = value * (float) 2540.0;
			break;

		case 1:
			NewHeight = value * (float) 100000.0;
			break;
		}

		if (NewHeight == OldHeight)
			return 0;

		Shape.ShapeHeight = NewHeight;
		DataBaseChanged = 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeNrLayers(int32 mode)
{
	ObjectRecord TypeObject, *Object, *Object2;
	ObjectPolygonRecord *ObjectPolygon;
	int32 OldNrPadLayers, cnt, cnt2, TempNrObjects, TempNrObjectPolygons;
	char str[MAX_LENGTH_STRING];

	OldNrPadLayers = NrPadLayers;

	memset(&TypeObject, 0, sizeof(ObjectRecord));
	sprintf(TypeObject.Text, "%d", NrPadLayers);

	if (TextInputDialog(&TypeObject, 0x15) == 1)
	{
		sscanf(TypeObject.Text, "%d", &NrPadLayers);
		NrPadLayers = min(16, NrPadLayers);
		NrPadLayers = max(2, NrPadLayers);
	}

	if (NrPadLayers == OldNrPadLayers)
		return 0;

	strcpy(str, SC(210, "This operation can not be undone\r\n\r\n Do you want to continue ?"));

	if (MessageBoxUTF8(GEOMWindow, str, SC(4, "Message"), MB_APPLMODAL | MB_YESNO) != IDYES)
	{
		NrPadLayers = OldNrPadLayers;
		return -1;
	}

	TempNrObjects = NrObjects;
	TempNrObjectPolygons = NrObjectPolygons;

	if (NrPadLayers > OldNrPadLayers)
	{	// Increase nr layers
		for (cnt = 0; cnt < TempNrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if (Object->Layer == OldNrPadLayers - 1)
			{	// Old top layer
				Object->Layer = NrPadLayers - 1;
			}

			if (Object->Layer == ROUTING_KEEPOUT_LAYER + OldNrPadLayers - 1)
			{	// Old routing keepout top layer
				Object->Layer = ROUTING_KEEPOUT_LAYER + NrPadLayers - 1;
			}

			if ((OldNrPadLayers == 2) && (Object->Layer == INNER_PAD_LAYER))
			{
				memcpy(&NewObject, Object, sizeof(ObjectRecord));
				Object->Info = OBJECT_NOT_VISIBLE;
				Object->AddNr = 0;
				Object->DeleteNr = 0;

				for (cnt2 = 1; cnt2 < NrPadLayers - 1; cnt2++)
				{
					NewObject.Layer = cnt2;
					NewObject.Info = 0;
					AddObject(&NewObject);
					Object2 = &((*Objects)[NrObjects - 1]);
					Object2->AddNr = 0;
				}
			}
		}

		for (cnt = 0; cnt < TempNrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (ObjectPolygon->Layer == OldNrPadLayers - 1)
			{	// Old top layer
				ObjectPolygon->Layer = NrPadLayers - 1;
			}

			if (ObjectPolygon->Layer == ROUTING_KEEPOUT_LAYER + OldNrPadLayers - 1)
			{	// Old routing keepout top layer
				ObjectPolygon->Layer = ROUTING_KEEPOUT_LAYER + NrPadLayers - 1;
			}
		}
	}
	else
	{
		if (NrPadLayers < OldNrPadLayers)
		{	// Decrease nr layers
			for (cnt = 0; cnt < TempNrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if ((Object->Layer > 0) && (Object->Layer < OldNrPadLayers - 1))
				{
					Object->Info = OBJECT_NOT_VISIBLE;
					Object->AddNr = 0;
					Object->DeleteNr = 0;

					if ((Object->Layer == 1) && (Object->ObjectType == OBJECT_CIRCLE))
					{
						memcpy(&NewObject, Object, sizeof(ObjectRecord));
						NewObject.Layer = INNER_PAD_LAYER;
						NewObject.Info = 0;
						AddObject(&NewObject);
						Object2 = &((*Objects)[NrObjects - 1]);
						Object2->AddNr = 0;
					}
				}

				if (Object->Layer == OldNrPadLayers - 1)
				{	// Old top layer
					Object->Layer = NrPadLayers - 1;
				}

				if ((Object->Layer > ROUTING_KEEPOUT_LAYER)
				        && (Object->Layer < ROUTING_KEEPOUT_LAYER + OldNrPadLayers - 1))
				{
					Object->Info = OBJECT_NOT_VISIBLE;
					Object->AddNr = 0;
					Object->DeleteNr = 0;
				}

				if (Object->Layer == ROUTING_KEEPOUT_LAYER + OldNrPadLayers - 1)
				{	// Old top layer
					Object->Layer = ROUTING_KEEPOUT_LAYER + NrPadLayers - 1;
				}
			}

			for (cnt = 0; cnt < TempNrObjectPolygons; cnt++)
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

				if ((ObjectPolygon->Layer > 0) && (ObjectPolygon->Layer < OldNrPadLayers - 1))
				{
					ObjectPolygon->Info = OBJECT_NOT_VISIBLE;
					ObjectPolygon->AddNr = 0;
					ObjectPolygon->DeleteNr = 0;
				}

				if (ObjectPolygon->Layer == OldNrPadLayers - 1)
				{	// Old top layer
					ObjectPolygon->Layer = NrPadLayers - 1;
				}

				if ((ObjectPolygon->Layer > ROUTING_KEEPOUT_LAYER)
				        && (ObjectPolygon->Layer < ROUTING_KEEPOUT_LAYER + OldNrPadLayers - 1))
				{
					ObjectPolygon->Info = OBJECT_NOT_VISIBLE;
					ObjectPolygon->AddNr = 0;
					ObjectPolygon->DeleteNr = 0;
				}

				if (ObjectPolygon->Layer == ROUTING_KEEPOUT_LAYER + OldNrPadLayers - 1)
				{	// Old top layer
					ObjectPolygon->Layer = ROUTING_KEEPOUT_LAYER + NrPadLayers - 1;
				}
			}
		}
	}

	DataBaseChanged = 1;
	RePaint();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PrintScreenToBitmap(int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, ok, fp, result, BytesPerLine, NrBitmapColors, MaxNrBitmapColors, ColorIndex,
	      ColorPixelCount, NrCodedBytes, NrBytesBitmapData;
	char FileStr[MAX_LENGTH_STRING];
	HBITMAP ViewBitmap;
	BITMAPINFO_Own *BitmapInfo;
	uint8 CodedLine[4096], BitmapInfoMem[1024];
	uint32 LinePixels[4096];
	uint32 BitmapColors[256], CurrentColor;
	BITMAPV4HEADER *BitmapHeader;
	int32 res;
	HDC CurrentDeviceContext;

	double Resolution;
	int32 OldDrawWindowMinX, OldDrawWindowMinY, OldDrawWindowMaxX, OldDrawWindowMaxY, BitmapMemsize, BitPerPixel,
	      NrPlanes, PixelsX, PixelsY;

	BmpHeaderRecord BmpHeader;

	OldDrawWindowMinX = DrawWindowMinX;
	OldDrawWindowMinY = DrawWindowMinY;
	OldDrawWindowMaxX = DrawWindowMaxX;
	OldDrawWindowMaxY = DrawWindowMaxY;

	CurrentDeviceContext = GetDC(GEOMWindow);
	OutputDisplay = CreateCompatibleDC(CurrentDeviceContext);
	CurrentObjectCode = 0;

	PixelsX = GetDeviceCaps(OutputDisplay, HORZRES);
	PixelsY = GetDeviceCaps(OutputDisplay, VERTRES);
	BitPerPixel = GetDeviceCaps(OutputDisplay, BITSPIXEL);
	NrPlanes = GetDeviceCaps(OutputDisplay, PLANES);

	memset(&BmpHeader, 0, sizeof(BmpHeader));
	MaxNrBitmapColors = 256;
	BmpHeader.Identifier = 0x4d42;
	BmpHeader.StartOfDataOffset = 54 + MaxNrBitmapColors * 4;
	BmpHeader.BitmapHeaderSize = 40;
	BmpHeader.Width = PixelsX;
	BmpHeader.Height = PixelsY;
	BmpHeader.NrOfPlanes = 1;
	BmpHeader.CompressionType = 1;
	BmpHeader.BitsPerPixel = 8;
	BmpHeader.NrColors1 = MaxNrBitmapColors;
	BmpHeader.NrImportedColors = 0;
	Resolution = (2540000.0 / 96);	// dpi

	BmpHeader.HResolutionInPixelsPerMeter = (int32) (1000.0 / (25.4 / Resolution));
	BmpHeader.VResolutionInPixelsPerMeter = (int32) (1000.0 / (25.4 / Resolution));

	memset(&BitmapInfoMem, 0, sizeof(BitmapInfoMem));

    //BitmapInfo=(BITMAPINFO *)BitmapInfoMem;

	BitmapInfo = (BITMAPINFO_Own *) BitmapInfoMem;
	BitmapHeader = (BITMAPV4HEADER *) & BitmapInfo->bmiHeader;
	BitmapHeader->bV4Size = sizeof(BITMAPV4HEADER);
	BitmapHeader->bV4Planes = 1;
	BitmapHeader->bV4BitCount = 32;

    //BitmapHeader->bV4V4Compression=BI_RLE8;

	BitmapHeader->bV4V4Compression = BI_RGB;
	BitmapHeader->bV4Width = PixelsX;
	BitmapHeader->bV4Height = PixelsY;

	DrawWindowMinX = 0;
	DrawWindowMinY = 0;
	DrawWindowMaxX = PixelsX;
	DrawWindowMaxY = PixelsY;
	SetBkMode(OutputDisplay, TRANSPARENT);
	SetROP2(OutputDisplay, R2_COPYPEN);

	ViewBitmap = CreateCompatibleBitmap(CurrentDeviceContext, PixelsX, PixelsY);

    //ViewBitmap = CreateCompatibleBitmap(OutputDisplay,PixelsX,PixelsY);

	SelectObject(OutputDisplay, ViewBitmap);

	CurrentFontCode = 0;
	CurrentObjectCode = 0;
	TextColor = (COLORREF) - 1;
	BackGroundActive = 0;

	BytesPerLine = PixelsX * 4;
	BitmapMemsize = BytesPerLine * PixelsY;

	AllocateMemTemp(BitmapMemsize);

    //Rectangle(OutputDisplay,0,0,PixelsX+1,PixelsY+1);

	ViewWholeDesign(0);

	ViewMinX = PixelToRealOffX(DrawWindowMinX - 1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(DrawWindowMinY - 1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

	DrawObjects(0);
	DrawObjectPolygons(0);

	InitDrawingColorGray();
	SetROP2(OutputDisplay, R2_XORPEN);

	DrawLine(-1, DrawWindowMaxY - Mult(-Yoffset) - 1, 4000, DrawWindowMaxY - Mult(-Yoffset) - 1);
	DrawLine(Mult(-Xoffset) + DrawWindowMinX, -10, Mult(-Xoffset) + DrawWindowMinX, 4000);

	if (ViewInsertionPoint)
		DrawInsertionPoint(0);

	if (GridVisible)
		DrawGrid();

    //res=GetDIBits(OutputDisplay,ViewBitmap,0,1,TempMem,(BITMAPINFO *)BitmapInfo,DIB_RGB_COLORS);

	res = GetDIBits(OutputDisplay, ViewBitmap, 0, PixelsY, TempMem, (BITMAPINFO *) BitmapInfo, DIB_RGB_COLORS);

	memset(&BitmapColors, 0, sizeof(BitmapColors));
	BitmapColors[0] = GEOMColors[BackGroundColorNr];
	NrBitmapColors = 1;
	NrBytesBitmapData = 0;

	strcpy(FileStr, "c:\\geom\\screen_geom1.bmp");
	fp = FileOpenWriteUTF8(FileStr);
	FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
	FileWrite(fp, &BitmapColors, sizeof(BitmapColors), &result);

	for (cnt = 0; cnt < PixelsY; cnt++)
	{
		memmove(LinePixels, (uint8 *) & TempMem[cnt * BytesPerLine], BytesPerLine);
		CurrentColor = (uint32) - 1;
		ColorPixelCount = 0;
		NrCodedBytes = 0;
		cnt3 = 0;
		ColorIndex = 0;

		for (cnt2 = 0; cnt2 < PixelsX; cnt2++)
		{
			cnt3 = 0;

			while ((cnt3 < NrBitmapColors) && (LinePixels[cnt2] != BitmapColors[cnt3]))
				cnt3++;

			if (cnt3 == NrBitmapColors)
			{	// Add color
				if (NrBitmapColors < 255)
					BitmapColors[NrBitmapColors++] = LinePixels[cnt2];
			}

			if (CurrentColor == (uint32) - 1)
			{
				ColorPixelCount = 1;
				CurrentColor = LinePixels[cnt2];
				ColorIndex = cnt3;
			}
			else
			{
				if (CurrentColor == LinePixels[cnt2])
					ColorPixelCount++;
				else
				{	// Found a different color, store the previous color pixels
					cnt4 = ColorPixelCount;

					while (cnt4 > 0)
					{
						if (cnt4 >= 255)
						{
							CodedLine[NrCodedBytes++] = 255;
							cnt4 -= 255;
						}
						else
						{
							CodedLine[NrCodedBytes++] = (uint8) cnt4;
							cnt4 = 0;
						}

						CodedLine[NrCodedBytes++] = (uint8) ColorIndex;
					}

					ColorPixelCount = 1;
					CurrentColor = LinePixels[cnt2];
					ColorIndex = cnt3;
				}
			}
		}

		if (ColorPixelCount > 0)
		{
			cnt4 = ColorPixelCount;

			while (cnt4 > 0)
			{
				if (cnt4 >= 255)
				{
					CodedLine[NrCodedBytes++] = 255;
					cnt4 -= 255;
				}
				else
				{
					CodedLine[NrCodedBytes++] = (uint8) cnt4;
					cnt4 = 0;
				}

				CodedLine[NrCodedBytes++] = (uint8) ColorIndex;
			}
		}

		CodedLine[NrCodedBytes++] = 0;
		CodedLine[NrCodedBytes++] = 0;
		NrBytesBitmapData += NrCodedBytes;
		FileWrite(fp, (uint8 *) CodedLine, NrCodedBytes, &result);
	}

	FileSeek(fp, 54);
	FileWrite(fp, &BitmapColors, sizeof(BitmapColors), &result);
	FileSeek(fp, 0);
	BmpHeader.BitmapDataSize = NrBytesBitmapData;
	BmpHeader.FileSize = sizeof(BmpHeader) + NrBitmapColors * 4 + BmpHeader.BitmapDataSize;
	FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);

    //FileWrite(fp,(uint8 *)&TempMem[cnt*BytesPerLine],BytesPerLine,&result);

	FileClose(fp);


	DeleteObject(ViewBitmap);
	DeleteDC(OutputDisplay);
	ReleaseDC(GEOMWindow, CurrentDeviceContext);

	DeAllocateMemTemp();
	ok = 1;

	DrawWindowMinX = OldDrawWindowMinX;
	DrawWindowMinY = OldDrawWindowMinY;
	DrawWindowMaxX = OldDrawWindowMaxX;
	DrawWindowMaxY = OldDrawWindowMaxY;

	CurrentFontCode = 0;
	CurrentObjectCode = 0;
	TextColor = (COLORREF) - 1;
	BackGroundActive = 0;
	RePaint();
	return;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RenamePin(int32 mode)
{
	int32 cnt, PinNr;
	ObjectRecord *Object, *FoundObject, ChangedTextObject;
	ObjectPolygonRecord *ObjectPolygon, *FoundObjectPolygon;
	PinInfoRecord *PinInfo;

	FoundObject = NULL;
	FoundObjectPolygon = NULL;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			switch (Object->Layer)
			{
			case BOARD_OUTLINE_LAYER:
			case INFO_LAYER:
			case INFO_LAYER2:
			case INFO_LAYER3:
			case INFO_LAYER4:
			case SOLD_MASK_BOTTOM_LAYER:
			case SOLD_MASK_TOP_LAYER:
			case PASTE_MASK_BOTTOM_LAYER:
			case PASTE_MASK_TOP_LAYER:
			case SILKSCREEN_TOP_LAYER:
			case SILKSCREEN_BOTTOM_LAYER:
				break;

			case DRILL_LAYER:
				FoundObject = Object;
				break;

			default:
				if ((Object->Layer >= 0) && (Object->Layer < 32))
					FoundObject = Object;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if ((ObjectPolygon->Layer >= 0) && (ObjectPolygon->Layer < 32))
				FoundObjectPolygon = ObjectPolygon;
		}
	}

	if ((!FoundObject) && (!FoundObjectPolygon))
		return -1;

	if (FoundObject)
		PinNr = FoundObject->PinNr;
	else
		PinNr = FoundObjectPolygon->PinNr;

	if (PinNr == -1)
		return -1;

	DataBaseChanged = 1;
	PinInfo = &((*PinInfos)[PinNr]);
	memset(&ChangedTextObject, 0, sizeof(ChangedTextObject));
	strcpy(ChangedTextObject.Text, PinInfo->PinText);

	if (TextInputDialog(&ChangedTextObject, 6) != 1)
		return -1;

	memset(PinInfo->PinText, 0, sizeof(PinInfo->PinText));
	strncpy(PinInfo->PinText, ChangedTextObject.Text, 9);
	RePaint();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
