/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw2.c
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
#include "draw2.h"
#include "draw.h"
#include "calc.h"
#include "calcdef.h"
#include "line2.h"
#include "rect.h"
#include "geom.h"
#include "math.h"
#include "ellipss.h"
#include "string.h"
#include "graphics.h"
#include "resource.h"
#include "polygon.h"
#include "select.h"
#include "mainloop.h"

extern COLORREF GridColor, LineColor;
extern HDC OutputDisplay;
extern int32 Printing, ReverseY;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern int32 PixelsPerInchX, SelectColorMode;

int32 SelectedColorNr = -1, ok;


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void DrawPinTextObject(ObjectRecord * Object, int32 mode)
{
	int32 Layer, PinNr, ObjectType;
	int32 OkToDraw;
	double x1, y1, x2, y2, x3, y3, x4, y4;
	double Length1, Angle1, Length2, Angle2, Angle;
	PinInfoRecord *PinInfo;
#ifdef _DEBUG
	int32 ok;
#endif

	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

//  SetROP2(OutputDisplay,R2_XORPEN);
	if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
	{
		ObjectType = Object->ObjectType;
		Layer = Object->Layer;
		PinNr = Object->PinNr;

		if (PinNr != -1)
		{
			OkToDraw = 0;
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;

			switch (Layer)
			{
			case POWER_PAD_LAYER:
				if (PowerPadsVisible)
					OkToDraw = 1;

				break;

			/*
			        case INNER_PAD_LAYER:
			          if (InnerPadsVisible) {
			            OkToDraw=1;
			          }
			          break;
			*/
			case DRILL_LAYER:
				if (DrillVisible)
					OkToDraw = 1;

				break;

			default:
				if (Layer < 32)
				{
					if (PadsVisible[Layer])
						OkToDraw = 1;
				}

				break;
			}

			if (OkToDraw)
			{
				PinInfo = &((*PinInfos)[PinNr]);

				switch (ObjectType)
				{
				case OBJECT_LINE:
					x1 = (x1 + x2) / 2;
					y1 = (y1 + y2) / 2;
					break;

				case OBJECT_ARC:
					ConvNormalCoorToPolar(x1, y1, x1 + Object->x3, y1 + Object->y3, &Angle1, &Length1);
					ConvNormalCoorToPolar(x1, y1, x1 + Object->x4, y1 + Object->y4, &Angle2, &Length2);

					if (Angle2 < Angle1)
						Angle2 += PI * 2;

					Angle = (Angle2 + Angle1) / 2;
					x1 = (x1 + x2 * cos(Angle) * 0.5);
					y1 = (y1 + y2 * sin(Angle) * 0.5);
					break;
				}

				GetMinMaxText(x1, y1, PixelToReal(18), 0, 0, 0, 0, PinInfo->PinText);
#ifdef _DEBUG

				if (stricmp(PinInfo->PinText, "1") == 0)
					ok = 1;

#endif

				if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY)
				        && (TextMinY <= ViewMaxY))
				{
					InitDrawingObject(0, PIN_TEXT_LAYER, 0, mode);
					DrawText2(x1, y1, PinInfo->PinText);
				}
			}
		}
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawObjects(int32 Mode)
{
	int32 cnt, ObjectLayer, ObjectType, SpecialObject;
	ObjectRecord *Object;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectType = Object->ObjectType;
			ObjectLayer = Object->Layer;
			SpecialObject = 0;

			if ((ObjectLayer != DRILL_LAYER) && (ObjectLayer != DRILL_UNPLATED_LAYER)
			        && (ObjectLayer != COMP_OUTLINE_LAYER))
			{
				if (((Object->Info & OBJECT_SELECTED) == OBJECT_SELECTED) && ((Mode & 4) == 0))
				{
					switch (ObjectLayer)
					{
					case SOLD_MASK_BOTTOM_LAYER:
					case SOLD_MASK_TOP_LAYER:
					case PASTE_MASK_BOTTOM_LAYER:
					case PASTE_MASK_TOP_LAYER:
					case POWER_PAD_LAYER:
					case INNER_PAD_LAYER:
						SpecialObject = 1;
						break;

					default:
						if ((ObjectLayer >= ROUTING_KEEPOUT_LAYER) && (ObjectLayer < ROUTING_KEEPOUT_LAYER + 32))
							SpecialObject = 1;

						break;
					}
				}

				if (SpecialObject)
				{
					/*
					          Object->Info&=~OBJECT_SELECTED;
					          DrawObject(Object,0.0,0.0,Mode);
					          DrawObject(Object,0.0,0.0,Mode|16);
					          Object->Info|=OBJECT_SELECTED;
					*/
					DrawObject(Object, 0.0, 0.0, Mode);
				}
				else
					DrawObject(Object, 0.0, 0.0, Mode);
			}
		}
	}



	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectType = Object->ObjectType;
			ObjectLayer = Object->Layer;

			if ((ObjectLayer == DRILL_LAYER) || (ObjectLayer == DRILL_UNPLATED_LAYER))
				DrawObject(Object, 0.0, 0.0, Mode);
		}
	}

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectType = Object->ObjectType;
			ObjectLayer = Object->Layer;

			if (ObjectLayer == COMP_OUTLINE_LAYER)
				DrawObject(Object, 0.0, 0.0, Mode);
		}
	}

	if (PinNamesVisible)
	{
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
				DrawPinTextObject(Object, 0);
		}
	}

	if (ClearanceVisible)
	{
		for (cnt = 0; cnt < NrObjects; cnt++)
		{
			Object = &((*Objects)[cnt]);

			if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
				DrawObjectWithClearance(Object, 0.0, 0.0, Mode);
		}
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawObjects2(double ox, double oy, int32 Mode)
{
	int32 cnt;
	ObjectRecord *Object;

	StartDrawingEditingWindow();
	SetROP2(OutputDisplay, R2_XORPEN);

	if (Mode == 1)
		Mode = 4;

	for (cnt = 0; cnt < NrObjects2; cnt++)
	{
		Object = &((*Objects2)[cnt]);
		DrawObject(Object, ox, oy, Mode);
	}

	DrawCrossHair(16 + 8);
	ExitDrawing();
	EndDrawingEditingWindow();
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawObjects2a(double ox, double oy, double ox2, double oy2, int32 Mode)
{
	DrawObjects2(ox - ox2, oy - oy2, Mode);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OX, double OY, int32 Mode)
{
	/*
	Mode  1 -> Double line
	      2 -> Clearance
	      4 -> Object xor
	      8 -> Set no draw mode
	*/

	typedef POINT PointsArray[200];
	double x1, y1, LastX, LastY;
	double cx, cy;
	int32 x1a, y1a, x2a, y2a, cnt2, cnt3, cnt4, x, y, count, Info, Layer, MemSize, ok;
	PointsArray *Points;
	int32 ObjectPolygonLength;
	uint8 *PBuf;
	ObjectSubPolygonRecord *ObjectSubPolygon;

	Layer = ObjectPolygon->Layer;

	if (!ObjectLayerVisible(Layer))
		return;

#ifdef _DEBUG

	if (ObjectPolygon->NrSubPolygons == 279)
		ok = 1;

#endif
	count = ObjectPolygon->NrVertices;
	MemSize = count * sizeof(POINT) + ObjectPolygon->NrSubPolygons * sizeof(ObjectSubPolygonRecord) + 16384;
	AllocateSpecialMem(MEM_POINTS, MemSize, (void **) &Points);

	if ((ObjectPolygon->maxx + OX >= ViewMinX) && (ObjectPolygon->minx + OX <= ViewMaxX)
	        && (ObjectPolygon->maxy + OY >= ViewMinY) && (ObjectPolygon->miny + OY <= ViewMaxY))
	{
		if ((Mode & 2) == 0)
		{
			if ((Mode & 0x20) == 0)
				InitDrawingObject(OBJECT_CIRCLE, Layer, 0, 0);
			else
				InitDrawingObject(OBJECT_CIRCLE, Layer, 0, 2);
		}

		Info = ObjectPolygon->Info;

		if (((Mode & 4) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		count = ObjectPolygon->NrVertices;

		if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
			count = ObjectPolygon->NrVerticesMainPolygon;

		if ((Mode & 0x20) == 0)
		{
			if ((ObjectPolygon->Info & OBJECT_SPECIAL_DRAW) == 0)
			{
				for (cnt3 = 0; cnt3 < count; cnt3++)
				{
					x = MultX(ObjectPolygon->Points[cnt3].x + OX);

					if (ReverseY)
						y = MultY(ObjectPolygon->Points[cnt3].y + OY);
					else
						y = MultY2(ObjectPolygon->Points[cnt3].y + OY);

					(*Points)[cnt3].x = x;
					(*Points)[cnt3].y = y;
				}

				Polygon(OutputDisplay, (POINT *) Points, count);

				ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

				if ((!BackGroundActive) && (ObjectPolygon->NrSubPolygons > 0)
				        && (ObjectPolygon->NrVerticesMainPolygon > 0))
				{
					if ((Mode & 4) == 0)
						InitDrawingBackGround(0, 0);
					else
					{
//            InitDrawingObject(OBJECT_CIRCLE,Layer,0,2);
					}

					PBuf = (uint8 *) ObjectPolygon;
					PBuf += sizeof(ObjectPolygonInitRecord);
					PBuf += count * sizeof(PointRecord);
					cnt4 = 0;

					for (cnt2 = 0; cnt2 < ObjectPolygon->NrSubPolygons; cnt2++)
					{
						if ((PBuf - (uint8 *) ObjectPolygon) >=
						        (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
							break;

						ObjectSubPolygon = (ObjectSubPolygonRecord *) PBuf;

						if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
						{
							cnt4++;
							break;
						}

						count = ObjectSubPolygon->NrVertices;

						if (count >= 65536)
							break;

						if ((ObjectSubPolygon->maxx + OX >= ViewMinX) && (ObjectSubPolygon->minx + OX <= ViewMaxX)
						        && (ObjectSubPolygon->maxy + OY >= ViewMinY) && (ObjectSubPolygon->miny + OY <= ViewMaxY))
						{
							for (cnt3 = 0; cnt3 < count; cnt3++)
							{
								x = MultX(ObjectSubPolygon->Points[cnt3].x + OX);

								if (ReverseY)
									y = MultY(ObjectSubPolygon->Points[cnt3].y + OY);
								else
									y = MultY2(ObjectSubPolygon->Points[cnt3].y + OY);

								(*Points)[cnt3].x = x;
								(*Points)[cnt3].y = y;
							}

							Polygon(OutputDisplay, (POINT *) Points, count);
						}
						else
							ok = 1;

						PBuf += sizeof(ObjectSubPolygonInitRecord);
						PBuf += count * sizeof(PointRecord);
					}
				}

				if ((Mode & 4) == 4)
				{
					cx = (ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5 + OX;
					cy = (ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5 + OY;
					InitDrawingColorWhite(0);
					DrawLine(MultX(cx) - 3, MultY(cy) - 3, MultX(cx) + 3, MultY(cy) + 3);
					DrawLine(MultX(cx) - 3, MultY(cy) + 3, MultX(cx) + 3, MultY(cy) - 3);
				}
			}
			else
			{
				LastX = (ObjectPolygon->Points[0].x + OX);
				LastY = (ObjectPolygon->Points[0].y + OX);

				for (cnt3 = 0; cnt3 < count - 1; cnt3++)
				{
					x1 = (ObjectPolygon->Points[cnt3 + 1].x + OX);
					y1 = (ObjectPolygon->Points[cnt3 + 1].y + OY);
					DrawLine(MultX(LastX), MultY(LastY), MultX(x1), MultY(y1));
					LastX = x1;
					LastY = y1;
				}
			}
		}
		else
		{
			x1a = MultX(ObjectPolygon->Points[count].x + OX);
			y1a = MultY(ObjectPolygon->Points[count].y + OY);

			if (count > 1)
			{
				x2a = MultX(ObjectPolygon->Points[count - 1].x + OX);
				y2a = MultY(ObjectPolygon->Points[count - 1].y + OY);
				DrawLine(x1a, y1a, x2a, y2a);
			}

			x2a = MultX(ObjectPolygon->Points[0].x + OX);
			y2a = MultY(ObjectPolygon->Points[0].y + OY);
			DrawLine(x1a, y1a, x2a, y2a);
		}

		if (((Mode & 4) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawPolygonObjectWithClearance(ObjectPolygonRecord * ObjectPolygon, double OX, double OY, int32 Mode)
/*
Mode  1 -> Double line
      2 -> Clearance
      4 -> Object xor
      8 -> Set no draw mode
*/
{
	typedef POINT PointsArray[200];
	int32 Layer, cnt2, cnt3, x, y, count, Info, ObjectPolygonLength;
	PointsArray *Points;
	uint8 Buf[32768];
	uint8 Buf2[32768];
	uint8 Buf3[32768];
	uint8 *PBuf;
	ObjectPolygonRecord *NewObjectPolygon2;
	ObjectPolygonRecord *NewObjectPolygon3;
	ObjectSubPolygonRecord *ObjectSubPolygon;

//  Layer=Object->Layer;
//  if ((Layer!=PAD_BOTTOM_LAYER)
//     &&
//     (Layer!=PAD_TOP_LAYER)) return;


	Layer = ObjectPolygon->Layer;

	switch (Layer)
	{
	case SOLD_MASK_BOTTOM_LAYER:
	case SOLD_MASK_TOP_LAYER:
	case PASTE_MASK_TOP_LAYER:
	case PASTE_MASK_BOTTOM_LAYER:
	case SILKSCREEN_TOP_LAYER:
	case SILKSCREEN_BOTTOM_LAYER:
	case PLACEMENT_OUTLINE_LAYER:
	case COMP_OUTLINE_LAYER:
	case GEOM_NAME_LAYER:
		return;

	default:
		if (Layer < 32)
		{
			if (!PadsVisible[Layer])
				return;
		}
		else
		{
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
				return;
		}

		break;
	}

	Points = (PointsArray *) & Buf;
	NewObjectPolygon2 = (ObjectPolygonRecord *) & Buf2;
	NewObjectPolygon3 = (ObjectPolygonRecord *) & Buf3;

	if ((ObjectPolygon->maxx + ObjectPolygon->Clearance + OX >= ViewMinX)
	        && (ObjectPolygon->minx - ObjectPolygon->Clearance + OX <= ViewMaxX)
	        && (ObjectPolygon->maxy + ObjectPolygon->Clearance + OY >= ViewMinY)
	        && (ObjectPolygon->miny - ObjectPolygon->Clearance + OY <= ViewMaxY) && (ObjectPolygon->Clearance != 0.0))
	{
		if (MakeBiggerSmallerObjectPolygon(ObjectPolygon, NewObjectPolygon2, ObjectPolygon->Clearance * 2.0, 0) == -1)
			return;

		Layer = ObjectPolygon->Layer;

		if (CurrentObjectCode != ClearanceObjectCode)
			InitDrawingClearance();

		Info = ObjectPolygon->Info;

		if (((Mode & 4) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, SelectColorMode);
		}

		count = NewObjectPolygon2->NrVertices;

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			x = MultX(NewObjectPolygon2->Points[cnt3].x + OX);

			if (ReverseY)
				y = MultY(NewObjectPolygon2->Points[cnt3].y + OY);
			else
				y = MultY2(NewObjectPolygon2->Points[cnt3].y + OY);

			(*Points)[cnt3].x = x;
			(*Points)[cnt3].y = y;
		}

		Polygon(OutputDisplay, (POINT *) Points, count);

		ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);

		if (ObjectPolygon->NrSubPolygons > 0)
		{
			PBuf = (uint8 *) ObjectPolygon;
			PBuf += sizeof(ObjectPolygonInitRecord);
			PBuf += count * sizeof(PointRecord);

			for (cnt2 = 0; cnt2 < ObjectPolygon->NrSubPolygons; cnt2++)
			{
				if ((PBuf - (uint8 *) ObjectPolygon) >=
				        (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
					break;

				ObjectSubPolygon = (ObjectSubPolygonRecord *) PBuf;

				if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
					break;

				count = ObjectSubPolygon->NrVertices;

				if (count >= 65536)
					break;

				memset(NewObjectPolygon2, 0, sizeof(ObjectPolygonRecord));
				NewObjectPolygon2->NrVertices = count;

				for (cnt3 = 0; cnt3 < count; cnt3++)
				{
					NewObjectPolygon2->Points[cnt3].x = ObjectSubPolygon->Points[cnt3].x;
					NewObjectPolygon2->Points[cnt3].y = ObjectSubPolygon->Points[cnt3].y;
				}

				if (MakeBiggerSmallerObjectPolygon
				        (NewObjectPolygon2, NewObjectPolygon3, ObjectPolygon->Clearance * 2.0, 1) == -1)
					return;

				for (cnt3 = 0; cnt3 < count; cnt3++)
				{
					x = MultX(NewObjectPolygon3->Points[cnt3].x + OX);

					if (ReverseY)
						y = MultY(NewObjectPolygon3->Points[cnt3].y + OY);
					else
						y = MultY2(NewObjectPolygon3->Points[cnt3].y + OY);

					(*Points)[cnt3].x = x;
					(*Points)[cnt3].y = y;
				}

				Polygon(OutputDisplay, (POINT *) Points, count);
				PBuf += sizeof(ObjectSubPolygonInitRecord);
				PBuf += count * sizeof(PointRecord);
			}
		}

		if (((Mode & 4) == 0) && ((Info & OBJECT_SELECTED) == OBJECT_SELECTED))
		{
			if (!Printing)
				SetROP2(OutputDisplay, R2_COPYPEN);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawObjectPolygons(int32 Mode)
{
	int32 cnt;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectRecord PinObject;

	if (!Printing)
		SetROP2(OutputDisplay, R2_COPYPEN);

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0)
			DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
	}

	memset(&PinObject, 0, sizeof(PinObject));

	if (PinNamesVisible)
	{
		for (cnt = 0; cnt < NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0)
			        && ((ObjectPolygon->Layer == 0) || (ObjectPolygon->Layer == NrPadLayers - 1)))
			{
				PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
				PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
				PinObject.Layer = ObjectPolygon->Layer;
				PinObject.PinNr = ObjectPolygon->PinNr;
				DrawPinTextObject(&PinObject, 0);
			}
		}
	}

	if (ClearanceVisible)
	{
		for (cnt = 0; cnt < NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (((ObjectPolygon->Info & OBJECT_NOT_VISIBLE) == 0)
			        && ((ObjectPolygon->Layer == 0) || (ObjectPolygon->Layer == NrPadLayers - 1)))
				DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, Mode);
		}
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawObject(ObjectRecord * Object, double ox, double oy, int32 Mode)
/*

Mode  1 -> Double line
      2 -> Clearance
      4 -> Object xor
      8 -> Set no draw mode
     16 => Hatched objects selected
*/
{
	double xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4, xx2a, yy2a, Xmax, Xmin, Ymax, Ymin, Thickness2;
	int32 dikte, Layer, DrawMode, ObjectSelected, SpecialMode, LineSegments, SegmentCount, cnt2, x1a, y1a, x2a, y2a,
	      ObjectType, CircleMode;
	PolygonRecord *PolygonObject;
	ObjectPolygonRecord *ObjectPolygon;
	uint8 PolygonBuf[8192];
	double LineBuf[128];

	PolygonObject = (PolygonRecord *) & PolygonBuf;

	if (OutputDisplay == (HGDIOBJ) NULL)
		ok = 1;

	Layer = Object->Layer;

	if (!ObjectLayerVisible(Layer))
		return;

	ObjectType = Object->ObjectType;

	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	yy2 = Object->y2;
	DrawMode = 0;
	ObjectSelected = 0;
	SpecialMode = 0;

	if ((Mode & 16) == 0)
	{
		if (((Object->Info & OBJECT_SELECTED) == OBJECT_SELECTED) && ((Mode & 4) == 0))
			SetROP2(OutputDisplay, SelectColorMode);
	}
	else
		SetROP2(OutputDisplay, R2_NOTCOPYPEN);

	switch (ObjectType)
	{
	case OBJECT_LINE:
		switch (Object->Info & 3)
		{
		case 0:
		case 3:
			xx1 += ox;
			yy1 += oy;
			xx2 += ox;
			yy2 += oy;
			break;

		case 1:
			xx1 += ox;
			yy1 += oy;
			break;

		case 2:
			xx2 += ox;
			yy2 += oy;
			break;
		}

		Thickness2 = Object->Thickness * 0.5;
		Xmin = min(xx1, xx2) - Thickness2;
		Ymin = min(yy1, yy2) - Thickness2;
		Xmax = max(xx1, xx2) + Thickness2;
		Ymax = max(yy1, yy2) + Thickness2;
#ifdef _DEBUG

		if ((uint32) Object == 0x15b698)
			ok = 1;

		if (Object->Layer == COMP_OUTLINE_LAYER)
			ok = 1;

#endif

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			dikte = max(1, Mult(Object->Thickness));
#ifdef _DEBUG

			if (Layer == PASTE_MASK_BOTTOM_LAYER)
				ok = 1;

#endif
			InitDrawingObject(ObjectType, Layer, dikte, 0);

			if (Object->Info2 != 0)
			{
				LineSegments = DimensionToLineSegments(xx1, yy1, xx2, yy2, (double *) &LineBuf, Object->Info2);
				SegmentCount = 0;

				for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
				{
					x1a = MultX(LineBuf[SegmentCount]);
					SegmentCount++;
					y1a = MultY(LineBuf[SegmentCount]);
					SegmentCount++;
					x2a = MultX(LineBuf[SegmentCount]);
					SegmentCount++;
					y2a = MultY(LineBuf[SegmentCount]);
					SegmentCount++;
					DrawLine(x1a, y1a, x2a, y2a);
				}
			}
			else
			{
				x1a = MultX(xx1);
				y1a = MultY(yy1);
				x2a = MultX(xx2);
				y2a = MultY(yy2);
				DrawLine(x1a, y1a, x2a, y2a);

				if ((Mode & 1) == 1)
				{
					if (fabs(xx2 - xx1) > fabs(yy2 - yy1))
						DrawLine(x1a, y1a - 1, x2a, y2a - 1);
					else
						DrawLine(x1a + 1, y1a, x2a + 1, y2a);
				}
			}
		}

		break;

	case OBJECT_RECT:
		xx1 += ox;
		yy1 += oy;
#ifdef _DEBUG

		if ((uint32) Object == 0x15c388)
			ok = 1;

		if ((Object->Info & OBJECT_SELECTED) != 0)
			ok = 1;

		if (Object->Layer == ROUTING_KEEPOUT_LAYER)
			ok = 1;

#endif
		xx2a = xx2;
		yy2a = yy2;

		if (Object->Thickness != 0.0)
		{
			xx2a += Object->Thickness;
			yy2a += Object->Thickness;
		}

		xx2a = xx2a / 2;
		yy2a = yy2a / 2;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = MultX(xx1);

			if (ReverseY)
				y1a = MultY(yy1);
			else
				y1a = Mult(yy1 - Yoffset);

			dikte = 0;

			if ((Object->Thickness == 0.0) && (Object->Layer != PLACEMENT_OUTLINE_LAYER))
				InitDrawingObject(ObjectType, Layer, 0, 0);
			else
			{
				dikte = max(1, Mult(Object->Thickness));
				InitDrawingObject(ObjectType, Layer, dikte, 0);
			}

			/*
			        if ((Object->Thickness==0.0)
			           &&
			           ((Mode & 4)==4)) {
			          InitDrawingEmptyPen();
			        }
			*/
			if (((Mode & 4) == 0) || (dikte > 0))
			{
				if ((Object->Info & OBJECT_SELECTED) == 0)
					rect3(x1a, y1a, max(2, Mult(xx2)), max(2, Mult(yy2)));
				else
					rect4(x1a, y1a, max(2, Mult(xx2)), max(2, Mult(yy2)));
			}
			else
				RectangleXor2(OutputDisplay, x1a, y1a, max(2, Mult(xx2)), max(2, Mult(yy2)));

			if ((Mode & 1) == 1)
				rect3(x1a, y1a, Mult(xx2) - 2, Mult(yy2) - 2);
		}

		break;

	case OBJECT_CIRCLE:
#ifdef _DEBUG
		if (Object->Layer == ROUTING_KEEPOUT_LAYER)
			ok = 1;

#endif
		xx1 += ox;
		yy1 += oy;
		xx2a = xx2;

		if (Object->Thickness != 0.0)
			xx2a += Object->Thickness;

		xx2a = xx2a / 2;
		yy2a = xx2a;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = MultX(xx1);

			if (ReverseY)
				y1a = MultY(yy1);
			else
				y1a = Mult(yy1 - Yoffset);

#ifdef _DEBUG
			ok = 1;

			if ((Mode & 4) == 4)
				ok = 1;

			if ((Object->Info & OBJECT_SELECTED) != 0)
				ok = 1;

			if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
				ok = 1;

			if (Layer == DRILL_UNPLATED_LAYER)
				ok = 1;

#endif
			CircleMode = (Object->Info2 & 15);

			if ((Object->Thickness == 0.0) && (Object->Layer != PLACEMENT_OUTLINE_LAYER))
			{
				InitDrawingObject(ObjectType, Layer, 0, 0);
				CircleMode = 15;
			}
			else
			{
				dikte = max(1, Mult(Object->Thickness));
				InitDrawingObject(ObjectType, Layer, dikte, 0);
			}

			/*
			        switch (Layer) {
			          case SILKSCREEN_LAYER:
			          case PLACEMENT_OUTLINE_LAYER:
			          case COMP_OUTLINE_LAYER:
			            CircleMode=(Object->Info2 & 15);
			            break;
			          default:
			            if ((Mode & 12)==12) InitDrawingEmptyPen();
			            CircleMode=15;
			        }
			        if ((Object->Thickness==0.0)
			           &&
			           ((Mode & 4)==4)) {
			          InitDrawingEmptyPen();
			        }
			*/

			ellips2(x1a, y1a, Mult(xx2), Mult(xx2), CircleConv[CircleMode]);

			if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
			{
				if (GetROP2(OutputDisplay) == SelectColorMode)
				{
					InitDrawingBackGroundBrush();
					SetROP2(OutputDisplay, R2_COPYPEN);
					ellips2(x1a, y1a, max(1, Mult(xx2) - 2), max(1, Mult(xx2) - 2), 255);
					CurrentObjectCode = 0;
				}
			}
			else
			{
				if ((Mode & 1) == 1)
					ellips2(x1a, y1a, Mult(xx2) - 2, Mult(xx2) - 2, CircleConv[CircleMode]);
			}
		}

		break;

	case OBJECT_ARC:
#ifdef _DEBUG
		if (Object->Layer == ROUTING_KEEPOUT_LAYER)
			ok = 1;

#endif
		xx1 += ox;
		yy1 += oy;
		xx3 = Object->x3;
		yy3 = Object->y3;
		xx4 = Object->x4;
		yy4 = Object->y4;
		xx2a = xx2;
		yy2a = yy2;

		if (Object->Thickness != 0.0)
		{
			xx2a += Object->Thickness;
			yy2a += Object->Thickness;
		}

		xx2a = xx2a / 2;
		yy2a = yy2a / 2;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = MultX(xx1);

			if (ReverseY)
				y1a = MultY(yy1);
			else
				y1a = Mult(yy1 - Yoffset);

			if ((Object->Thickness == 0.0) && (Layer != PLACEMENT_OUTLINE_LAYER))
			{
				InitDrawingObject(ObjectType, Layer, 0, 0);
				ellips2(x1a, y1a, Mult(xx2), Mult(xx2), 255);
			}
			else
			{
				dikte = max(1, Mult(Object->Thickness));
				InitDrawingObject(ObjectType, Layer, dikte, 0);

				if (ReverseY)
				{
					SpecialArc(x1a, y1a, Mult(xx2), Mult(yy2), Mult(xx1 + xx3 - Xoffset) + DrawWindowMinX,
					           DrawWindowMaxY - Mult(yy1 + yy3 - Yoffset) - 1,
					           Mult(xx1 + xx4 - Xoffset) + DrawWindowMinX,
					           DrawWindowMaxY - Mult(yy1 + yy4 - Yoffset) - 1);

					if ((Mode & 1) == 1)
						SpecialArc(x1a, y1a, Mult(xx2) + dikte - 2, Mult(yy2) + dikte - 2,
						           Mult(xx1 + xx3 - Xoffset) + DrawWindowMinX,
						           DrawWindowMaxY - Mult(yy1 + yy3 - Yoffset) - 1,
						           Mult(xx1 + xx4 - Xoffset) + DrawWindowMinX,
						           DrawWindowMaxY - Mult(yy1 + yy4 - Yoffset) - 1);
				}
				else
				{
					SpecialArc(x1a, y1a, Mult(xx2) + dikte, Mult(yy2) + dikte,
					           Mult(xx1 + xx3 - Xoffset) + DrawWindowMinX, Mult(yy1 + yy3 - Yoffset),
					           Mult(xx1 + xx4 - Xoffset) + DrawWindowMinX, Mult(yy1 + yy4 - Yoffset));

					if ((Mode & 1) == 1)
						SpecialArc(x1a, y1a, Mult(xx2) + dikte - 2, Mult(yy2) + dikte - 2,
						           Mult(xx1 + xx3 - Xoffset) + DrawWindowMinX, Mult(yy1 + yy3 - Yoffset),
						           Mult(xx1 + xx4 - Xoffset) + DrawWindowMinX, Mult(yy1 + yy4 - Yoffset));
				}
			}
		}

		break;

	case OBJECT_TEXT:
		xx1 += ox;
		yy1 += oy;

		if (Object->RotationAngle > 1000.0)
			GetMinMaxText(xx1, yy1, xx2, 0, Object->RotationAngle - 2000.0, 0, 1, Object->Text);
		else
			GetMinMaxText(xx1, yy1, xx2, 0, Object->RotationAngle, 0, 0, Object->Text);

		if ((TextMaxX >= ViewMinX) && (TextMinX <= ViewMaxX) && (TextMaxY >= ViewMinY) && (TextMinY <= ViewMaxY))
		{
			dikte = max(1, Mult(Object->Thickness));
			InitDrawingObject(ObjectType, Layer, dikte, 0);

			if (Object->RotationAngle > 1000.0)
				DrawStrWithRotation(xx1, yy1, xx2, Object->RotationAngle - 2000.0, 0, 1, Object->Text);
			else
				DrawStrWithRotation(xx1, yy1, xx2, Object->RotationAngle, 0, 0, Object->Text);
		}

		break;

	case OBJECT_POLYGON:
		ObjectPolygon = (ObjectPolygonRecord *) Object->Address;
		DrawObjectPolygon(ObjectPolygon, ox, oy, Mode);
		break;
	}

	if (((Object->Info & OBJECT_SELECTED) == OBJECT_SELECTED) && ((Mode & 4) == 0))
		SetROP2(OutputDisplay, R2_COPYPEN);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawObjectWithClearance(ObjectRecord * Object, double ox, double oy, int32 Mode)
/*

Mode  1 -> Double line
      4 -> Object xor

*/
{
	double xx1, yy1, xx2, yy2, xx2a, yy2a, Xmax, Xmin, Ymax, Ymin;
	int32 Layer, DrawMode, ObjectSelected, x1a, y1a, ObjectType, CircleMode;
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf[8192];

	PolygonObject = (PolygonRecord *) & PolygonBuf;

//  Layer=Object->Layer;
//  if ((Layer!=PAD_BOTTOM_LAYER)
//     &&
//     (Layer!=PAD_TOP_LAYER)) return;

	ObjectType = Object->ObjectType;

	switch (ObjectType)
	{
	case OBJECT_TEXT:
		return;
	}


	Layer = Object->Layer;

	switch (Layer)
	{
	case SOLD_MASK_BOTTOM_LAYER:
	case SOLD_MASK_TOP_LAYER:
	case PASTE_MASK_TOP_LAYER:
	case PASTE_MASK_BOTTOM_LAYER:
	case SILKSCREEN_TOP_LAYER:
	case SILKSCREEN_BOTTOM_LAYER:
	case PLACEMENT_OUTLINE_LAYER:
	case COMP_OUTLINE_LAYER:
	case GEOM_NAME_LAYER:
	case INFO_LAYER:
	case INFO_LAYER2:
	case INFO_LAYER3:
	case INFO_LAYER4:
		return;

	/*
	    case INNER_PAD_LAYER:
	      if (!InnerPadsVisible) return;
	      break;
	*/
	case POWER_PAD_LAYER:
		if (!PowerPadsVisible)
			return;

		break;

	case DRILL_LAYER:
		if (!DrillVisible)
			return;

		break;

	case DRILL_UNPLATED_LAYER:
		if (!DrillUnplatedVisible)
			return;

		break;

	default:
		if (Layer < 32)
		{
			if (!PadsVisible[Layer])
				return;
		}
		else
		{
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			{
				if (!RoutingKeepoutVisible[Layer - ROUTING_KEEPOUT_LAYER])
					return;
			}
			else
				return;
		}

		break;
	}


	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	yy2 = Object->y2;
	DrawMode = 0;
	ObjectSelected = 0;

	if (((Object->Info & OBJECT_SELECTED) == OBJECT_SELECTED) && ((Mode & 4) == 0))
		SetROP2(OutputDisplay, R2_WHITE);

	switch (ObjectType)
	{
	case OBJECT_LINE:
		if (CurrentObjectCode != ClearanceObjectCode)
			InitDrawingClearance();

		MakePolygonFromPlotObject(Object, PolygonObject, Object->Clearance, 16, 32);
		DrawFilledPolygon(PolygonObject, 0);
		break;

	case OBJECT_RECT:
		xx1 += ox;
		yy1 += oy;
		xx2 += Object->Clearance * 2;
		yy2 += Object->Clearance * 2;
		xx2a = xx2 / 2;
		yy2a = yy2 / 2;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = Mult(xx1 - Xoffset) + DrawWindowMinX;

			if (ReverseY)
				y1a = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
			else
				y1a = Mult(yy1 - Yoffset);

			if (CurrentObjectCode != ClearanceObjectCode)
				InitDrawingClearance();

			if ((Mode & 4) == 0)
				rect3(x1a, y1a, Mult(xx2), Mult(yy2));
			else
				RectangleXor2(OutputDisplay, x1a, y1a, Mult(xx2), Mult(yy2));
		}

		break;

	case OBJECT_CIRCLE:
		xx1 += ox;
		yy1 += oy;
		xx2 += Object->Clearance * 2;
		xx2a = xx2 / 2;
		yy2a = xx2 / 2;
		Xmin = xx1 - xx2a;
		Ymin = yy1 - yy2a;
		Xmax = xx1 + xx2a;
		Ymax = yy1 + yy2a;

		if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
		{
			x1a = Mult(xx1 - Xoffset) + DrawWindowMinX;

			if (ReverseY)
				y1a = DrawWindowMaxY - Mult(yy1 - Yoffset) - 1;
			else
				y1a = Mult(yy1 - Yoffset);

			if (CurrentObjectCode != ClearanceObjectCode)
				InitDrawingClearance();

			CircleMode = 15;
			ellips2(x1a, y1a, Mult(xx2), Mult(xx2), CircleConv[CircleMode]);
		}

		break;

	case OBJECT_ARC:
		if (CurrentObjectCode != ClearanceObjectCode)
			InitDrawingClearance();

		MakePolygonFromPlotObject(Object, PolygonObject, Object->Clearance, 16, 32);
		DrawFilledPolygon(PolygonObject, 0);
		break;
	}

	if (((Object->Info & OBJECT_SELECTED) == OBJECT_SELECTED) && ((Mode & 4) == 0))
		SetROP2(OutputDisplay, R2_COPYPEN);
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawText2(double x1, double y1, LPSTR TextStr)
{
	int32 y1a, y2a;

	if (!Printing)
	{
		y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;
		TextOut(OutputDisplay, Mult(x1 - Xoffset) + DrawWindowMinX + 1, y1a - 19, TextStr, strlen(TextStr));
	}
	else
	{
		y1a = DrawWindowMaxY - Mult(y1 - Yoffset) - 1;

		if (PixelsPerInchX < 400)
			y2a = 60;
		else
		{
			if (PixelsPerInchX < 800)
				y2a = 120;
			else
			{
				if (PixelsPerInchX < 1600)
					y2a = 240;
			}
		}

		TextOut(OutputDisplay, Mult(x1 - Xoffset) + DrawWindowMinX + 1, y1a - 40, TextStr, strlen(TextStr));
	}
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawGrid()
//  round
{
	double Grid2;
	uint8 GridBits[2048];
	HBITMAP GridBitmap, old;
	HDC PCBMemoryDC;
	int32 Xmin2, Xmax2, Ymin2, Ymax2;
	int32 Xpixels, Ypixels, cntx, cnty, x, y;

	Grid2 = GridSize;

	Xmin2 = (int32) (PixelToRealOffX(-40) / GridSize) - 1;
	Ymin2 = (int32) (PixelToRealOffY(-40) / GridSize) - 1;
	Xmax2 = (int32) (PixelToRealOffX(DrawWindowMaxX + 40) / GridSize) + 1;
	Ymax2 = (int32) (PixelToRealOffY(DrawWindowMaxY + 40) / GridSize) + 1;

	/*
	  if (Xmin2<-10000) Xmin2=-10000;
	  if (Ymin2<-10000) Ymin2=-10000;
	  if (Xmax2>10000) Xmax2=10000;
	  if (Ymax2>10000) Ymax2=10000;
	*/
	Xpixels = min(max(Xmax2 - Xmin2, 1), 3000);
	Ypixels = min(max(Ymax2 - Ymin2, 1), 3000);
	SetTextColor(OutputDisplay, GridColor);
	SetBkColor(OutputDisplay, RGB(0, 0, 0));

	if (DrawWindowMaxX / Xpixels > 3)
	{
		memset(&GridBits, 0xff, sizeof(GridBits));

		for (cntx = Xmin2; cntx < Xmax2; cntx++)
		{
			x = Mult(cntx * GridSize - Xoffset) + DrawWindowMinX;

			if ((x >= DrawWindowMinX) && (x < DrawWindowMaxX))
				GridBits[x >> 3] &= 255 - (128 >> (x & 7));
		}

		SetROP2(OutputDisplay, R2_XORPEN);
		GridBitmap = CreateBitmap(3072, 1, 1, 1, &GridBits);
		PCBMemoryDC = CreateCompatibleDC(OutputDisplay);
		old = SelectObject(PCBMemoryDC, GridBitmap);

		for (cnty = Ymin2; cnty < Ymax2; cnty++)
		{
			y = Mult(cnty * GridSize - Yoffset);
			y = DrawWindowMaxY - y - 1;

			if ((y >= DrawWindowMinY) && (y < DrawWindowMaxY))
			{
				BitBlt(OutputDisplay, 0, y, 3072, 1, PCBMemoryDC, 0, 0, SRCINVERT);
				/*
				        for (cntx=Xmin2;cntx<Xmax2;cntx++) {
				          x=Mult(cntx*GridSize-Xoffset);
				          if ((x>=DrawWindowMinX)
				             &&
				             (x<DrawWindowMaxX))
				            SetPixel(OutputDisplay,x,y,White);
				        }
				*/
			}
		}

		SelectObject(PCBMemoryDC, old);
		DeleteDC(PCBMemoryDC);
		DeleteObject(GridBitmap);
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void InitSpecialDraw(MEASUREITEMSTRUCT * MeasureItem)
{
	/*
	MEASUREITEMSTRUCT

	    uint32  CtlType;      // type of control
	    uint32  CtlID;        // combo box, list box, or button identifier
	    uint32  itemID;       // menu item, variable-height list box, or combo box identifier
	    uint32  itemWidth;    // width of menu item, in pixels
	    uint32  itemHeight;   // height of single item in list box menu, in pixels
	    DWORD itemData;     // application-defined 32-bit value
	*/
	switch (MeasureItem->CtlType)
	{
	case ODT_MENU:
		break;

	case ODT_LISTBOX:

//    case ODT_LISTVIEW:
		switch (MeasureItem->CtlID)
		{
		case IDC_LIST1:
			MeasureItem->itemHeight = 28;
			break;
		}

		break;
	}
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void DrawSpecialItem(DRAWITEMSTRUCT * DrawItem)
{
	/*
	DRAWITEMSTRUCT

	    uint32  CtlType;
	    uint32  CtlID;
	    uint32  itemID;
	    uint32  itemAction;
	    uint32  itemState;
	    HWND  hwndItem;
	    HDC   hDC;
	    RECT  rcItem;
	    DWORD itemData;
	*/

	HGDIOBJ SavePen, SaveBrush, MenuBrush, SaveFont;
	LOGBRUSH BrushObject;
	HPEN MenuPen, WhitePen, Pen2;
	int32 ItemNr, ok;
	int32 Inverted = 0;
	int32 Redraw;
	char TextStr[MAX_LENGTH_STRING];
	HPALETTE OldPalette;

// OnDrawItem

	OldPalette = NULL;

	switch (DrawItem->CtlType)
	{
// **********************************************************************************
// Drawing user definied menus
// **********************************************************************************
	case ODT_MENU:
		break;

	case ODT_LISTBOX:
		switch (DrawItem->CtlID)
		{
		case IDC_LIST1:
			ItemNr = DrawItem->itemData;
//          ItemNr=DrawItem->itemID;
			Redraw = 0;

			switch (DrawItem->itemAction)
			{
			case ODA_DRAWENTIRE:
				if (SelectedColorNr == -1)
					Inverted = 0;
				else
				{
					if (SelectedColorNr == ItemNr)
						Inverted = 1;
				}

				Redraw = 1;
				break;

			case ODA_SELECT:
				if ((DrawItem->itemState & ODS_SELECTED) == 0)
					Inverted = 0;
				else
				{
					SelectedColorNr = ItemNr;
					Inverted = 1;
				}

				Redraw = 1;
				break;

			case ODA_FOCUS:
				if (SelectedColorNr != -1)
				{
					Redraw = 1;
					Inverted = 1;
				}
				else
				{
					if ((DrawItem->itemState & ODS_SELECTED) == ODS_SELECTED)
					{
						Inverted = 1;
						Redraw = 1;
					}
				}

				ok = 1;
				break;
			}

			if (Redraw)
			{
				SaveFont = SelectObject(DrawItem->hDC, GetStockObject(DEFAULT_GUI_FONT));
//            SaveFont=SelectObject(DrawItem->hDC,GetStockObject(SYSTEM_FONT));
				SetBkColor(DrawItem->hDC, RGB(0, 0, 0));
				SetTextColor(DrawItem->hDC, RGB(255, 255, 255));
				WhitePen = CreatePen(PS_SOLID, 1, RGB(255, 255, 255));
				Pen2 = CreatePen(PS_SOLID, 2, RGB(192, 192, 192));
				SetROP2(DrawItem->hDC, R2_COPYPEN);
				BrushObject.lbColor = GEOMColors[ItemNr];
				BrushObject.lbStyle = BS_SOLID;
				BrushObject.lbHatch = (LONG) NULL;

				MenuBrush = CreateBrushIndirect(&BrushObject);
				MenuPen = CreatePen(PS_SOLID, 1, GEOMColors[ItemNr]);
				SaveBrush = SelectObject(DrawItem->hDC, MenuBrush);
				SavePen = SelectObject(DrawItem->hDC, MenuPen);
				Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top, DrawItem->rcItem.left + 180,
				          DrawItem->rcItem.bottom + 1);
				SelectObject(DrawItem->hDC, GetStockObject(WHITE_BRUSH));
				SelectObject(DrawItem->hDC, WhitePen);
				Rectangle(DrawItem->hDC, DrawItem->rcItem.left + 180, DrawItem->rcItem.top, DrawItem->rcItem.right,
				          DrawItem->rcItem.bottom + 1);

				if (Inverted)
				{
					SelectObject(DrawItem->hDC, GetStockObject(NULL_BRUSH));
					SetROP2(DrawItem->hDC, R2_XORPEN);
					SelectObject(DrawItem->hDC, Pen2);
					Rectangle(DrawItem->hDC, DrawItem->rcItem.left, DrawItem->rcItem.top + 1, DrawItem->rcItem.right,
					          DrawItem->rcItem.bottom + 1);
				}

//            SaveFont=SelectObject(DrawItem->hDC,MenuFont);
				SetBkMode(DrawItem->hDC, TRANSPARENT);
//            SetBkColor(DrawItem->hDC,RGB(0,0,0));
				SetTextColor(DrawItem->hDC, RGB(0, 0, 0));

				switch (ItemNr)
				{
				case BackGroundColorNr:
					strcpy(TextStr, SC(135, "Background"));
					break;

				case ShapeSilkScreenTopColorNr:
					strcpy(TextStr, SC(9, "Silkscreen top"));
					break;

				case ShapeSilkScreenBottomColorNr:
					strcpy(TextStr, SC(10, "Silkscreen bottom"));
					break;

				case ShapeCompOutlineColorNr:
					strcpy(TextStr, SC(11, "Component outline"));
					break;

				case ShapePlacementOutLineColorNr:
					strcpy(TextStr, SC(13, "Placement outline"));
					break;

				case ShapePinsDrillColorNr:
					strcpy(TextStr, SC(14, "Drill plated"));
					break;

				case ShapePinsDrillUnplatedColorNr:
					strcpy(TextStr, SC(15, "Drill unplated"));
					break;

				case ShapePinsTopColorNr:
					strcpy(TextStr, SC(18, "Pads top"));
					break;

				case ShapePinsBottomColorNr:
					strcpy(TextStr, SC(19, "Pads bottom"));
					break;

				case ShapePinsInnerColorNr:
					strcpy(TextStr, SC(17, "Inner pad"));
					break;

				case ShapePasteMaskTopColorNr:
					strcpy(TextStr, SC(7, "Paste mask top"));
					break;

				case ShapePasteMaskBottomColorNr:
					strcpy(TextStr, SC(8, "Paste mask bottom"));
					break;

				case ShapeSoldMaskTopColorNr:
					strcpy(TextStr, SC(5, "Solder mask top"));
					break;

				case ShapeSoldMaskBottomColorNr:
					strcpy(TextStr, SC(6, "Solder mask bottom"));
					break;

				case ShapeGeomNameColorNr:
					strcpy(TextStr, SC(97, "Geometry name"));
					break;

				case ShapePowerPadColorNr:
					strcpy(TextStr, SC(16, "Anti power pad"));
					break;

				case ShapeBoardOutlineColorNr:
					strcpy(TextStr, SC(12, "Board outline"));
					break;

				case ShapeInfo1ColorNr:
					strcpy(TextStr, "Info 1");
					break;

				case ShapeInfo2ColorNr:
					strcpy(TextStr, "Info 2");
					break;

				case ShapeInfo3ColorNr:
					strcpy(TextStr, "Info 3");
					break;

				case ShapeInfo4ColorNr:
					strcpy(TextStr, "Info 4");
					break;

				case ClearanceColorNr:
					strcpy(TextStr, SC(58, "Clearance"));
					break;

				case GridColorNr:
					strcpy(TextStr, SC(103, "Grid"));
					break;

				case ShapeRoutingKeepoutTopColorNr:
					strcpy(TextStr, SC(144, "Routing keepout top"));
					break;

				case ShapeRoutingKeepoutBottomColorNr:
					strcpy(TextStr, SC(145, "Routing keepout bottom"));
					break;

				case ShapeRoutingKeepoutInnerColorNr:
					strcpy(TextStr, SC(146, "Routing keepout inner"));
					break;

				case ButtonInfoColorNr:
					strcpy(TextStr, SC(147, "Button info"));
					break;
				}

				TextOut(DrawItem->hDC, 185, DrawItem->rcItem.top + 6, TextStr, strlen(TextStr));
				SelectObject(DrawItem->hDC, SavePen);
				SelectObject(DrawItem->hDC, SaveBrush);
				SelectObject(DrawItem->hDC, SaveFont);
				DeleteObject(WhitePen);
				DeleteObject(Pen2);
				DeleteObject(MenuPen);
				DeleteObject(MenuBrush);
			}

			break;
		}

		break;
	}
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
