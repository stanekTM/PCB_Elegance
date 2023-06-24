/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calc.c
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
#include "calc3.h"
#include "calc4.h"
#include "stdio.h"
#include "calcdef.h"
#include "calcrect.h"
#include "string.h"
#include "math.h"
#include "graphics.h"
#include "font.h"

extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern int32 DebugTest1;
extern HWND PCBWindow;
extern HGLOBAL ObjectsGlobal;

int32 ok, ObjectPrintCount = 1;
uint32 ShapeCrcs[512];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetBoardPosComps()
{
	int32 cnt;
	CompRecord *Comp;

	Design.MaximumClearance = 0.0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		SetBoardPosComp(Comp, 0);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void SetBoardPosComp(CompRecord * Comp, int32 mode)

/*

  mode :

  bit 0  : Only pin objects
  bit 1  : Set comp min,max to search min,max
  bit 2  : No clearance

*/

{
	int32 cnt2, Mirror;
	double xmin, ymin, xmax, ymax, xmin2, ymin2, xmax2, ymax2, x1, y1, x2, y2, MaxCl, Clearance, Thickness, Thickness2;
	ObjectRecord *Object;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;
#ifdef _DEBUG

	if (stricmp(Comp->Name, "P1101") == 0)
	{
		ok = 1;

		if ((mode & 2) == 0)
			ok = 1;

		if ((mode & 1) == 1)
			ok = 1;
	}

#endif

	PolygonObject = (PolygonRecord *) & PolygonBuf;
	NrObjects = 0;

	if ((mode & 1) == 0)
	{
		ShapeCompOutLineToObject(Comp, 0.0, 0.0, 0.0);
		ShapeCompSilkScreenToObject(Comp, 0.0, 0.0, 0.0);
		ShapePlacementOutLineToObject(Comp, 0.0, 0.0, 0.0);
		ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, -1, 0);
	}

	ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

	MaxCl = 0.0;
	xmin = 1e9;
	ymin = 1e9;
	xmax = -1e9;
	ymax = -1e9;
	xmin2 = 1e9;
	ymin2 = 1e9;
	xmax2 = -1e9;
	ymax2 = -1e9;

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

		if (stricmp(Comp->Name, "P1101") == 0)
		{
		}

		if (stricmp(Comp->Name, "P1101") == 0)
		{
			if (cnt2 == 24)
				ok = 1;
		}

#endif
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;
		Clearance = Object->Clearance;

		if ((mode & 4) == 4)
			Clearance = 0.0;

		Thickness = Object->Thickness;
		Thickness2 = Thickness * 0.5;

		switch (Object->ObjectType)
		{
// *******************************************************************************************************
		case PIN_SMD_ROUND:
		case DRILL:
		case DRILL_UNPLATED:
			x2 = x2 / 2 + Clearance;
			xmin = min(xmin, x1 - x2);
			ymin = min(ymin, y1 - x2);
			xmax = max(xmax, x1 + x2);
			ymax = max(ymax, y1 + x2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_SMD_RECT:
			x2 = x2 / 2 + Clearance;
			y2 = y2 / 2 + Clearance;
			xmin = min(xmin, x1 - x2);
			ymin = min(ymin, y1 - y2);
			xmax = max(xmax, x1 + x2);
			ymax = max(ymax, y1 + y2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_PUT_THROUGH_ROUND:
		case PIN_PUT_THROUGH_SQUARE:
			x2 = x2 / 2 + Clearance;
			xmin = min(xmin, x1 - x2);
			ymin = min(ymin, y1 - x2);
			xmax = max(xmax, x1 + x2);
			ymax = max(ymax, y1 + x2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_PUT_THROUGH_POLYGON:
		case PIN_SMD_POLYGON:
			MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
			Thickness2 = Clearance;
			xmin = min(xmin, PolygonObject->minx - Thickness2);
			ymin = min(ymin, PolygonObject->miny - Thickness2);
			xmax = max(xmax, PolygonObject->maxx + Thickness2);
			ymax = max(ymax, PolygonObject->maxy + Thickness2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_LINE_HOR:		// pin line hor
			y2 = y2 / 2 + Clearance;
			xmin = min(xmin, x1 - y2);
			ymin = min(ymin, y1 - y2);
			xmax = max(xmax, x1 + x2 + y2);
			ymax = max(ymax, y1 + y2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_LINE_VER:		// pin line ver
			y2 = y2 / 2 + Clearance;
			xmin = min(xmin, x1 - y2);
			ymin = min(ymin, y1 - y2);
			xmax = max(xmax, x1 + y2);
			ymax = max(ymax, y1 + x2 + y2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_LINE_DIAG1:	// pin line diag1
			y2 = y2 / 2 + Clearance;
			xmin = min(xmin, x1 - y2);
			ymin = min(ymin, y1 - x2 - y2);
			xmax = max(xmax, x1 + x2 + y2);
			ymax = max(ymax, y1 + y2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_LINE_DIAG2:	// pin line diag2
			y2 = y2 / 2 + Clearance;
			xmin = min(xmin, x1 - y2);
			ymin = min(ymin, y1 - y2);
			xmax = max(xmax, x1 + x2 + y2);
			ymax = max(ymax, y1 + x2 + y2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_LINE_ALL_ANGLE:	// pin line
			xmin = min(xmin, x1 - Thickness2);
			xmax = max(xmax, x1 + Thickness2);
			xmin = min(xmin, x2 - Thickness2);
			xmax = max(xmax, x2 + Thickness2);
			ymin = min(ymin, y1 - Thickness2);
			ymax = max(ymax, y1 + Thickness2);
			ymin = min(ymin, y2 - Thickness2);
			ymax = max(ymax, y2 + Thickness2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

		case PIN_ARC:			// Pin arc
			x2 = x2 / 2;
			y2 = x2 / 2;
			xmin = min(xmin, x1 - x2 - Thickness2);
			ymin = min(ymin, y1 - y2 - Thickness2);
			xmax = max(xmax, x1 + x2 + Thickness2);
			ymax = max(ymax, y1 + y2 + Thickness2);
			MaxCl = max(Object->Clearance, MaxCl);
			break;

// *******************************************************************************************************
		case OBJECT_POLYGON:
			MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

			if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
				Thickness2 = 0.0;

			xmin = min(xmin, PolygonObject->minx - Thickness2);
			ymin = min(ymin, PolygonObject->miny - Thickness2);
			xmax = max(xmax, PolygonObject->maxx + Thickness2);
			ymax = max(ymax, PolygonObject->maxy + Thickness2);

			switch (Object->Layer)
			{
			case PLACEMENT_OUTLINE_TOP:
			case PLACEMENT_OUTLINE_TOP2:
			case PLACEMENT_OUTLINE_BOTTOM:
			case PLACEMENT_OUTLINE_BOTTOM2:
				xmin2 = min(xmin2, PolygonObject->minx - Thickness2);
				ymin2 = min(ymin2, PolygonObject->miny - Thickness2);
				xmax2 = max(xmax2, PolygonObject->maxx + Thickness2);
				ymax2 = max(ymax2, PolygonObject->maxy + Thickness2);
				break;
			}

			break;

		case OBJECT_LINE:		// outline line
			xmin = min(xmin, x1 - Thickness2);
			xmax = max(xmax, x1 + Thickness2);
			xmin = min(xmin, x2 - Thickness2);
			xmax = max(xmax, x2 + Thickness2);
			ymin = min(ymin, y1 - Thickness2);
			ymax = max(ymax, y1 + Thickness2);
			ymin = min(ymin, y2 - Thickness2);
			ymax = max(ymax, y2 + Thickness2);

			switch (Object->Layer)
			{
			case PLACEMENT_OUTLINE_TOP:
			case PLACEMENT_OUTLINE_TOP2:
			case PLACEMENT_OUTLINE_BOTTOM:
			case PLACEMENT_OUTLINE_BOTTOM2:
				xmin2 = min(xmin2, x1);
				xmax2 = max(xmax2, x1);
				xmin2 = min(xmin2, x2);
				xmax2 = max(xmax2, x2);
				ymin2 = min(ymin2, y1);
				ymax2 = max(ymax2, y1);
				ymin2 = min(ymin2, y2);
				ymax2 = max(ymax2, y2);
				break;
			}

			break;

		case OBJECT_RECT:		// outline rect
			x2 = x2 / 2;
			y2 = y2 / 2;

			if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
				Thickness2 = 0.0;

			xmin = min(xmin, x1 - x2 - Thickness2);
			ymin = min(ymin, y1 - y2 - Thickness2);
			xmax = max(xmax, x1 + x2 + Thickness2);
			ymax = max(ymax, y1 + y2 + Thickness2);

			switch (Object->Layer)
			{
			case PLACEMENT_OUTLINE_TOP:
			case PLACEMENT_OUTLINE_TOP2:
			case PLACEMENT_OUTLINE_BOTTOM:
			case PLACEMENT_OUTLINE_BOTTOM2:
				xmin2 = min(xmin2, x1 - x2);
				ymin2 = min(ymin2, y1 - y2);
				xmax2 = max(xmax2, x1 + x2);
				ymax2 = max(ymax2, y1 + y2);
				break;
			}

			break;

		case OBJECT_CIRCLE:	// outline circle
			if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
				Thickness2 = 0.0;

			x2 = x2 / 2;

			if (((uint8) y2 & 0x0C) > 0)
			{
				xmin = min(xmin, x1 - x2 - Thickness2);
				xmin2 = min(xmin2, x1 - x2);
			}

			if (((uint8) y2 & 0x03) > 0)
			{
				xmax = max(xmax, x1 + x2 + Thickness2);
				xmax2 = max(xmax2, x1 + x2);
			}

			if (((uint8) y2 & 0x06) > 0)
			{
				ymin = min(ymin, y1 - x2 - Thickness2);
				ymin2 = min(ymin2, y1 - x2);
			}

			if (((uint8) y2 & 0x09) > 0)
			{
				ymax = max(ymax, y1 + x2 + Thickness2);
				ymax2 = max(ymax2, y1 + x2);
			}

			switch (Object->Layer)
			{
			case PLACEMENT_OUTLINE_TOP:
			case PLACEMENT_OUTLINE_TOP2:
			case PLACEMENT_OUTLINE_BOTTOM:
			case PLACEMENT_OUTLINE_BOTTOM2:
				xmin2 = min(xmin2, x1 - x2);
				ymin2 = min(ymin2, y1 - y2);
				xmax2 = max(xmax2, x1 + x2);
				ymax2 = max(ymax2, y1 + y2);
				break;
			}

			break;

		case OBJECT_ARC:		// outline arc
			x2 = x2 / 2;
			y2 = y2 / 2;

			if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
				Thickness2 = 0.0;

			xmin = min(xmin, x1 - x2 - Thickness2);
			ymin = min(ymin, y1 - y2 - Thickness2);
			xmax = max(xmax, x1 + x2 + Thickness2);
			ymax = max(ymax, y1 + y2 + Thickness2);

			switch (Object->Layer)
			{
			case PLACEMENT_OUTLINE_TOP:
			case PLACEMENT_OUTLINE_TOP2:
			case PLACEMENT_OUTLINE_BOTTOM:
			case PLACEMENT_OUTLINE_BOTTOM2:
				xmin2 = min(xmin2, x1 - x2);
				ymin2 = min(ymin2, y1 - y2);
				xmax2 = max(xmax2, x1 + x2);
				ymax2 = max(ymax2, y1 + y2);
				break;
			}

			break;

		case OBJECT_TEXT:		// outline text
			Mirror = Object->Mirror;
			GetMinMaxText2(x1, y1, x2, 0, Object->RotationAngle, 0, Mirror, (LPSTR) Object->TraceNr);
			xmin = min(xmin, TextMinX);
			xmax = max(xmax, TextMaxX);
			ymin = min(ymin, TextMinY);
			ymax = max(ymax, TextMaxY);
			break;
// *******************************************************************************************************
		}

#ifdef _DEBUG

		if (ymin < 10e5)
			ok = 1;

#endif
	}

#ifdef _DEBUG

	if (xmax > 200e5)
		ok = 1;

#endif

	if ((mode & 2) == 0)
	{
		Comp->BoardPosMinX = (float) xmin;
		Comp->BoardPosMinY = (float) ymin;
		Comp->BoardPosMaxX = (float) xmax;
		Comp->BoardPosMaxY = (float) ymax;
		Comp->PlacementOriginX = (float) ((xmax2 + xmin2) / 2);
		Comp->PlacementOriginY = (float) ((ymax2 + ymin2) / 2);
		Comp->PlacementWidth = (float) fabs((xmax2 - xmin2));
		Comp->PlacementHeight = (float) fabs((ymax2 - ymin2));
		Comp->PinMaximumClearance = (float) MaxCl;

		if ((Comp->PlacementWidth > 1900e5) && (Comp->PlacementHeight > 1900e5))
		{
			ok = 1;
			Comp->PlacementOriginX = (float) ((xmax + xmin) / 2);
			Comp->PlacementOriginY = (float) ((ymax + ymin) / 2);
			Comp->PlacementWidth = (float) fabs((xmax - xmin));
			Comp->PlacementHeight = (float) fabs((ymax - ymin));
		}
	}
	else
	{
		SearchMinX = xmin;
		SearchMinY = ymin;
		SearchMaxX = xmax;
		SearchMaxY = ymax;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckValue(double value1, double value2, double value3, double value4)
{
	if ((value1 < -0.5e9) || (value1 > 0.5e9) || (value2 < -0.5e9) || (value2 > 0.5e9) || (value3 < -0.5e9)
	        || (value3 > 0.5e9) || (value4 < -0.5e9) || (value4 > 0.5e9))
		ok = 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindMinMaxBoard(double *MinX, double *MinY, double *MaxX, double *MaxY, int32 mode)

{

#define    InRangePCB(x)  ( (((x>-1.0e9) && (x<1.0e9))) ? (1) : (0) )

	/*
	  mode:

	  0:
	  1: All objects
	  3:

	*/
	
	CompRecord *Comp;

	int32 cnt, Layer, cnt2, TextMode, TextAlignment, Mirror, FontNr, MaxCountX, NrLines, Changed,
	      BoardOutlineSizeChanged;
	char str[MAX_LENGTH_STRING];
	double x1, y1, x2, y2, x4, y4, MinX1, MinY1, MaxX1, MaxY1, dikte, lengte, MinX2, MinY2, MaxX2, MaxY2, Thickness,
	       Rotation, Xmax, Xmin, Ymax, Ymin;
	ObjectRecord *Object;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;

	TraceRecord *Trace;
	ViaRecord *Via;
	AreaFillRecord *AreaFill;

	Changed = 0;
	BoardOutlineSizeChanged = 0;

	*MinX = 1000000000.0;
	*MinY = 1000000000.0;
	*MaxX = -1000000000.0;
	*MaxY = -1000000000.0;
	MinX2 = 1000000000.0;
	MinY2 = 1000000000.0;
	MaxX2 = -1000000000.0;
	MaxY2 = -1000000000.0;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = ObjectLine->X1;
			y1 = ObjectLine->Y1;
			x2 = ObjectLine->X2;
			y2 = ObjectLine->Y2;

			Thickness = 0.0;

			if (ObjectLine->Layer == BOARD_OUTLINE_LAYER)
			{
				*MinX = min(*MinX, x1 - Thickness);
				*MaxX = max(*MaxX, x1 + Thickness);
				*MinX = min(*MinX, x2 - Thickness);
				*MaxX = max(*MaxX, x2 + Thickness);
				*MinY = min(*MinY, y1 - Thickness);
				*MaxY = max(*MaxY, y1 + Thickness);
				*MinY = min(*MinY, y2 - Thickness);
				*MaxY = max(*MaxY, y2 + Thickness);
				BoardOutlineSizeChanged = 1;
			}

			MinX2 = min(MinX2, x1 - Thickness);
			MaxX2 = max(MaxX2, x1 + Thickness);
			MinX2 = min(MinX2, x2 - Thickness);
			MaxX2 = max(MaxX2, x2 + Thickness);
			MinY2 = min(MinY2, y1 - Thickness);
			MaxY2 = max(MaxY2, y1 + Thickness);
			MinY2 = min(MinY2, y2 - Thickness);
			MaxY2 = max(MaxY2, y2 + Thickness);
			Changed = 1;
		}
	}

	ok = 1;

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = ObjectRect->CentreX;
			y1 = ObjectRect->CentreY;
			x2 = ObjectRect->Width;
			y2 = ObjectRect->Height;

			Thickness = 0.0;

			if (ObjectRect->Layer == BOARD_OUTLINE_LAYER)
			{
				*MinX = min(*MinX, x1 - x2 * 0.5 - Thickness);
				*MaxX = max(*MaxX, x1 + x2 * 0.5 + Thickness);
				*MinY = min(*MinY, y1 - y2 * 0.5 - Thickness);
				*MaxY = max(*MaxY, y1 + y2 * 0.5 + Thickness);
				BoardOutlineSizeChanged = 1;
			}

			MinX2 = min(MinX2, x1 - x2 * 0.5) - Thickness;
			MaxX2 = max(MaxX2, x1 + x2 * 0.5) + Thickness;
			MinY2 = min(MinY2, y1 - y2 * 0.5) - Thickness;
			MaxY2 = max(MaxY2, y1 + y2 * 0.5) + Thickness;
			Changed = 1;
		}
	}

	ok = 1;

	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = ObjectArc->CentreX;
			y1 = ObjectArc->CentreY;
			x2 = ObjectArc->Width;
			y2 = ObjectArc->Width;

			Thickness = 0.0;

			if (ObjectArc->Layer == BOARD_OUTLINE_LAYER)
			{
				GetMinMaxArc(x1, y1, x2, y2, ObjectArc->StartDiffX, ObjectArc->StartDiffY, ObjectArc->EndDiffX,
				             ObjectArc->EndDiffY, &MinX1, &MinY1, &MaxX1, &MaxY1);
				*MinX = min(*MinX, MinX1 - Thickness);
				*MaxX = max(*MaxX, MaxX1 + Thickness);
				*MinY = min(*MinY, MinY1 - Thickness);
				*MaxY = max(*MaxY, MaxY1 + Thickness);
				BoardOutlineSizeChanged = 1;
			}

			GetMinMaxArc(x1, y1, x2, y2, ObjectArc->StartDiffX, ObjectArc->StartDiffY, ObjectArc->EndDiffX,
			             ObjectArc->EndDiffY, &MinX1, &MinY1, &MaxX1, &MaxY1);
			MinX2 = min(MinX2, MinX1 - Thickness);
			MaxX2 = max(MaxX2, MaxX1 + Thickness);
			MinY2 = min(MinY2, MinY1 - Thickness);
			MaxY2 = max(MaxY2, MaxY1 + Thickness);
			Changed = 1;
		}
	}

	ok = 1;

#ifdef _DEBUG

	if (MaxX2 > 200e5)
		ok = 1;

#endif

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = ObjectText2->X;
			y1 = ObjectText2->Y;
			x2 = ObjectText2->FontHeight;
			TextMode = ObjectText2->TextMode;
			Rotation = ObjectText2->Rotation;
			TextAlignment = (TextMode & 0x0f);
			Mirror = (ObjectText2->TextMode & 0x10) >> 4;
			FontNr = ObjectText2->FontNr;
			Thickness = ObjectText2->LineThickNess;

			if ((NrLines = ConvertObjectTextToStrings(ObjectText2->Text, FontNr, &MaxCountX, ObjectText2->Layer)) > 0)
			{
				x4 = x1;
				y4 = y1;
				Xmin = 1e9;
				Xmax = -1e9;
				Ymin = 1e9;
				Ymax = -1e9;

				for (cnt2 = 0; cnt2 < NrLines; cnt2++)
				{
					if (FontNr == 0)
						GetMinMaxText2(x4, y4, x2, FontNr, Rotation, 0, Mirror, TextStrings2[cnt2]);
					else
						GetMinMaxText2b(x4, y4, x2, FontNr, Rotation, 0, Mirror, TextStrings[cnt2]);

					Xmin = min(Xmin, TextMinX);
					Ymin = min(Ymin, TextMinY);
					Xmax = max(Xmax, TextMaxX);
					Ymax = max(Ymax, TextMaxY);

					if (FontNr == 0)
					{
						if (Mirror == 0)
							x4 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
						else
							x4 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;

						y4 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
					}
					else
					{
						if (Mirror == 0)
							x4 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
						else
							x4 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;

						y4 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
					}
				}

				if (FontNr > 0)
					Thickness = 0.0;

				MinX2 = min(MinX2, Xmin - Thickness);
				MaxX2 = max(MaxX2, Xmax + Thickness);
				MinY2 = min(MinY2, Ymin - Thickness);
				MaxY2 = max(MaxY2, Ymax + Thickness);
				Changed = 1;
#ifdef _DEBUG

				if (MaxX2 > 200e5)
					ok = 1;

#endif
			}
		}
	}

	ok = 1;

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			SetMinMaxObjectPolygon(ObjectPolygon, 0);
			MinX2 = min(MinX2, ObjectPolygon->minx);
			MinY2 = min(MinY2, ObjectPolygon->miny);
			MaxX2 = max(MaxX2, ObjectPolygon->maxx);
			MaxY2 = max(MaxY2, ObjectPolygon->maxy);
		}
	}

#ifdef _DEBUG

	if (MaxX2 > 200e5)
		ok = 1;

#endif

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 3);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
				FillPositionObjectWithClearance(Object, Object->Thickness);
				*MinX = min(*MinX, Object->minx);
				*MaxX = max(*MaxX, Object->maxx);
				*MinY = min(*MinY, Object->miny);
				*MaxY = max(*MaxY, Object->maxy);
				BoardOutlineSizeChanged = 1;
				MinX2 = min(MinX2, Object->minx);
				MinY2 = min(MinY2, Object->miny);
				MaxX2 = max(MaxX2, Object->maxx);
				MaxY2 = max(MaxY2, Object->maxy);
			}
		}
	}

	if (((mode & 1) == 0) && (BoardOutlineSizeChanged))
		return 0;

#ifdef _DEBUG

	if (MaxX2 > 200e5)
		ok = 1;

#endif

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			MinX2 = min(MinX2, Comp->BoardPosMinX);
			MinY2 = min(MinY2, Comp->BoardPosMinY);
			MaxX2 = max(MaxX2, Comp->BoardPosMaxX);
			MaxY2 = max(MaxY2, Comp->BoardPosMaxY);

			if (0)
			{
				if ((!InRangePCB(MinX2)) || (!InRangePCB(MinY2)) || (!InRangePCB(MaxX2)) || (!InRangePCB(MaxY2)))
				{
					sprintf(str, "%.1f , %.1f - %.1f , %.1f mm  %s", MinX2 / 100000.0, MinY2 / 100000.0,
					        MaxX2 / 100000.0, MaxY2 / 100000.0, Comp->Name);
					MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
				}
			}

			Changed = 1;
		}
	}


	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Trace->X;
				x2 = x1;
				y1 = Trace->Y;
				lengte = Trace->Length;
				y2 = y1 + lengte;
				dikte = Trace->ThickNess * 0.5;
				MinX2 = min(MinX2, x1 - dikte);
				MaxX2 = max(MaxX2, x2 + dikte);
				MinY2 = min(MinY2, y1 - dikte);
				MaxY2 = max(MaxY2, y2 + dikte);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Trace->X;
				y1 = Trace->Y;
				lengte = Trace->Length;
				x2 = x1 + lengte;
				y2 = y1;
				dikte = Trace->ThickNess * 0.5;
				MinX2 = min(MinX2, x1 - dikte);
				MaxX2 = max(MaxX2, x2 + dikte);
				MinY2 = min(MinY2, y1 - dikte);
				MaxY2 = max(MaxY2, y2 + dikte);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Trace->X;
				y1 = Trace->Y;
				lengte = Trace->Length;
				x2 = x1 + lengte;
				y2 = y1 - lengte;
				dikte = Trace->ThickNess * 0.5;
				MinX2 = min(MinX2, x1 - dikte);
				MaxX2 = max(MaxX2, x2 + dikte);
				MinY2 = min(MinY2, y2 - dikte);
				MaxY2 = max(MaxY2, y1 + dikte);
				Changed = 1;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if ((Trace->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				x1 = Trace->X;
				y1 = Trace->Y;
				lengte = Trace->Length;
				x2 = x1 + lengte;
				y2 = y1 + lengte;
				dikte = Trace->ThickNess * 0.5;
				MinX2 = min(MinX2, x1 - dikte);
				MaxX2 = max(MaxX2, x2 + dikte);
				MinY2 = min(MinY2, y1 - dikte);
				MaxY2 = max(MaxY2, y2 + dikte);
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if ((Via->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Via->X;
			y1 = Via->Y;
			dikte = Via->ThickNess * 0.5;
			MinX2 = min(MinX2, x1 - dikte);
			MaxX2 = max(MaxX2, x1 + dikte);
			MinY2 = min(MinY2, y1 - dikte);
			MaxY2 = max(MaxY2, y1 + dikte);
			Changed = 1;
		}
	}

#ifdef _DEBUG

	if (MaxX2 > 200e5)
		ok = 1;

#endif

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			MinX2 = min(MinX2, AreaFill->minx);
			MaxX2 = max(MaxX2, AreaFill->maxx);
			MinY2 = min(MinY2, AreaFill->miny);
			MaxY2 = max(MaxY2, AreaFill->maxy);
			Changed = 1;
		}
	}


	if (!Changed)
	{
		*MinX = -25.4e5;
		*MaxX = 150e5;
		*MinY = -25.4e5;
		*MaxY = 120e5;
		return -1;
	}

	if (0)
	{
		sprintf(str, "%.1f , %.1f - %.1f , %.1f mm", MinX2 / 100000.0, MinY2 / 100000.0, MaxX2 / 100000.0, MaxY2 / 100000.0);
		MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
	}

	if (!BoardOutlineSizeChanged)
	{
		*MinX = MinX2;
		*MinY = MinY2;
		*MaxX = MaxX2;
		*MaxY = MaxY2;
		return 2;
	}

	if ((mode & 2) == 0)
	{
		*MinX = MinX2;
		*MinY = MinY2;
		*MaxX = MaxX2;
		*MaxY = MaxY2;
		return 1;
	}
	else
	{
		if ((MinX2 < *MinX) || (MinY2 < *MinY))
		{
			*MinX = MinX2;
			*MinY = MinY2;
			*MaxX = MaxX2;
			*MaxY = MaxY2;
			return 3;
		}

		*MinX = MinX2;
		*MinY = MinY2;
		*MaxX = MaxX2;
		*MaxY = MaxY2;
		return 1;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapePinsToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation, int32 ObjectArrayNr,
                       int32 SelectedLayer, int32 mode)

// mode bit 0 ==  0  -> Object->x3 = Power pad
// mode bit 0 ==  1  -> Object->x3 = Inner pad
// mode bit 1 ==  0  -> All Objects
// mode bit 1 ==  1  -> Only objects with drill holes
// mode bit 2 ==  1  -> Only include layer objects
{
	int32 PinOffset, MemPos, CompLayer, SoldLayer, NrPins, PinNr, Rotation, NetPinCount, MemPos2, MemDiv,
	      PolygonVertices, NrLayers, MirroredLayer, CompObjectCount, ShapePos, PinsMemPos, Layer, MemPosComp, ShapeInfo,
	      NrPinShapes, ShapeType, ShapeNr, Mirror, NetNr;
	double x, y, Width, Height, OX, OY, OriginX, OriginY, hulp, RotationAngle, x1, y1, x2, y2, x3, y3, x4, y4;
	int32 ObjectInclude;
	LPCSTR PinText;


	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	NetRecord *Net;
	ObjectRecord *Object;
	ShapeRecord *Shape;
	CompPinRecord *CompPin;
	GeomPolygonRecord *GeomPolygon;
	ShapeLinesArray *ShapeLines;

	MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
	PinsMemPos = MemPosComp + sizeof(CompRecord);

	ShapeNr = (int32) Comp->ShapeNr;

	if (ShapeNr == -1)
		return;

	OriginX = Comp->CompOriginX;
	OriginY = Comp->CompOriginY;

	Mirror = ((Comp->CompMode & 8) >> 3);

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	MemPos2 = (*Shapes)[ShapeNr + 1].ShapePos;
	MemDiv = MemPos2 - MemPos;

	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	NrLayers = Shape->NrLayers;
#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "z100") == 0)
		ok = 1;

	if (stricmpOwn(Shape->ShapeName, "TO-220UP") == 0)
		ok = 1;

	if (TempRotation != 0.0)
		ok = 1;

#endif
	RotationAngle = Comp->Rotation + TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);
	MirroredLayer = 0;
	NetPinCount = 0;
	CompLayer = Design.NrBoardLayers;
	SoldLayer = 1;
	OX = OriginX;
	OY = OriginY;
	PinOffset = Shape->PinOffset;
	ShapeInfo = Shape->Info;

	if (ObjectArrayNr == 0)
	{
		if (MaxNrObjects < 1024)
			AllocateMemObjects(1024);
	}
	else
	{
		if (MaxNrObjects2 < 1024)
			AllocateMemObjects2(1024);
	}

	CompObjectCount = 0;
	PolygonVertices = 0;
	NrPins = Shape->NrPins;
	MemPos += PinOffset;
	PinNr = 0;

	while (NrPins > 0)
	{

		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		NrPinShapes = ShapePad->NrPinShapes;
		MemPos += sizeof(ShapePadRecord);

		if (PinNr < Comp->NrPins)
		{
			CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
			NetNr = CompPin->NetNr;
			NetPinCount = 1;

			if (NetNr >= 0)
			{
				Net = &((*Nets)[NetNr]);
				NetPinCount = max(1, Net->NrPins);
#ifdef _DEBUG

				if (stricmpOwn(Net->Name, "N$1026") == 0)
					ok = 1;

				if (stricmpOwn(Net->Name, "A0") == 0)
					ok = 1;

#endif
			}

			ok = 1;
		}
		else
			NetNr = -1;

		PinText = (LPCSTR) & (ShapePad->Name);
#ifdef _DEBUG

		if (strcmp(PinText, SC(629, "Layer 1")) == 0)
			ok = 1;

#endif

		while (NrPinShapes > 0)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);
			Layer = Pad->Layer;
			ObjectInclude = CheckGeometryLayer(&Layer, NrLayers, Mirror);

			if (Objects == NULL)
				Objects = (ObjectArray *) GlobalLock(ObjectsGlobal);

			if (ObjectArrayNr == 0)
				Object = &((*Objects)[NrObjects]);
			else
				Object = &((*Objects2)[NrObjects2]);

			if (((mode & 2) == 2) && (Layer != -1) && (SelectedLayer != -1))
			{
				if (Layer != SelectedLayer)
					ObjectInclude = 0;
			}

			Object->Layer = Layer;
			ShapeType = Pad->ShapeType;

			if (ObjectInclude)
			{
				x = Pad->X;
				y = Pad->Y;
				Width = Pad->Width;
				Height = Pad->Height;

				x2 = Width;
				y2 = Height;
				Object->PinNr = (int32) PinNr;
				Object->NetNr = NetNr;
				Object->CompNr = 0;
				Object->TraceNr = (int32) Comp;
				Object->Info = 0;
				Object->ObjectType2 = 0;
				Object->PinCount = NetPinCount;

				switch (ShapeType)
				{
				case DRILL_UNPLATED:
					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->y3 = 0.0;	// Inner pad
					}
					else
					{
						Object->y3 = Pad->Extra2;	// Anti power pad
					}

					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->ObjectType = ShapeType;
					break;

				case DRILL:
					if (Mirror == 1)
						x = -x;

					Object->Layer = -1;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->ObjectType = ShapeType;
					break;

				case PIN_PUT_THROUGH_ROUND:	// Put through pin round
					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_PUT_THROUGH_ROUND;
					break;

				case PIN_PUT_THROUGH_SQUARE:	// Put through pin square
					if (Mirror == 1)
						x = -x;

					Object->ObjectType = PIN_PUT_THROUGH_SQUARE;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						Object->RotationAngle = RotationAngle;
						Object->Mirror = Mirror;
						Object->Info = OBJECT_FILLED;
						Object->ObjectType2 = PIN_PUT_THROUGH_SQUARE;
						Object->ObjectType = PIN_PUT_THROUGH_POLYGON;
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					break;

				case PIN_SMD_RECT:	// SMD Pad rect
					Object->ObjectType = PIN_SMD_RECT;

					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						hulp = Width;
						Width = Height;
						Height = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						hulp = Width;
						Width = Height;
						Height = hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						Object->RotationAngle = RotationAngle;
						Object->Mirror = Mirror;
						Object->Info = OBJECT_FILLED;
						Object->ObjectType2 = PIN_SMD_RECT;
						Object->ObjectType = PIN_SMD_POLYGON;
						break;
					}

					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;
					break;

				case PIN_SMD_ROUND:	// SMD Pad round
					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_SMD_ROUND;
					break;

				case PIN_LINE_HOR:	// Hor Line
					if (Mirror == 1)
						x = -x - Width;

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);

					switch (Rotation)
					{
					case 0:
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 2:
						x = -x - Width;
						y = -y;
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp - Width;
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 4:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG2;
						break;

					case 5:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y += Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG1;
						break;

					case 6:
						RotatePoint2(&x, &y, RotationAngle);
						Object->ObjectType = PIN_LINE_DIAG2;
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y -= Width * SQRT05;
						break;

					case 7:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG1;
						break;

					case -1:
						Object->Thickness = Height;
						x2 = Width;

						if (Mirror == 1)
						{
							//                x2=-x2;
						}

						y2 = 0;
						RotatePoint2(&x, &y, RotationAngle);
						RotatePoint2(&x2, &y2, RotationAngle);
						Object->ObjectType = PIN_LINE_ALL_ANGLE;
						Object->x1 = x + OX + OffsetX;
						Object->y1 = y + OY + OffsetY;
						Object->x2 = Object->x1 + x2;
						Object->y2 = Object->y1 + y2;
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_VER:	// Ver Line
					if (Mirror == 1)
						x = -x;

#ifdef _DEBUG

					if (InRange4(Width, 96 * 2540.0))
						ok = 1;

#endif
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);

					switch (Rotation)
					{
					case 0:
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						x = x - Width;
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 2:
						x = -x;
						y = -y - Width;
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 4:
						RotatePoint2(&x, &y, RotationAngle);
						Object->ObjectType = PIN_LINE_DIAG1;
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y += Width * SQRT05;
						break;

					case 5:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y -= Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG2;
						break;

					case 6:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG1;
						break;

					case 7:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG2;
						break;

					case -1:
						Object->Thickness = Height;
						x2 = 0.0;
						y2 = Width;
						RotatePoint2(&x, &y, RotationAngle);
						RotatePoint2(&x2, &y2, RotationAngle);
						Object->ObjectType = PIN_LINE_ALL_ANGLE;
						Object->x1 = x + OX + OffsetX;
						Object->y1 = y + OY + OffsetY;
						Object->x2 = Object->x1 + x2;
						Object->y2 = Object->y1 + y2;
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_DIAG1:	// Diag1 Line
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);

					if (Mirror == 0)
					{
						switch (Rotation)
						{
						case 0:
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 1:
							hulp = x;
							x = -y;
							y = hulp;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 2:
							x = -x - Width;
							y = -y + Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 3:
							hulp = x;
							x = y - Width;
							y = -hulp - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 4:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_HOR;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 5:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 6:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							x -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 7:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							y -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case -1:
							Object->Thickness = Height;
							x2 = Width;
							y2 = -Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}
					else
					{
						switch (Rotation)
						{
						case 0:
							x = -x - Width;
							y = y - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 1:
							hulp = x;
							x = -y;
							y = -hulp;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 2:
							y = -y;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 3:
							hulp = x;
							x = y - Width;
							y = hulp + Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 4:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_VER;
							Width *= SQRT2;
							y -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 5:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 6:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 7:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							x -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case -1:
							Object->Thickness = Height;
							x = -x;
							x2 = -Width;
							y2 = -Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_DIAG2:	// Diag2 Line
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);

					if (Mirror == 0)
					{
						switch (Rotation)
						{
						case 0:
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 1:
							hulp = x;
							x = -y - Width;
							y = hulp + Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 2:
							x = -x - Width;
							y = -y - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 3:
							hulp = x;
							x = y;
							y = -hulp;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 4:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_VER;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 5:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_HOR;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							x -= Width;
							break;

						case 6:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_VER;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							y -= Width;
							break;

						case 7:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case -1:
							Object->Thickness = Height;
							x2 = Width;
							y2 = Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}
					else
					{
						switch (Rotation)
						{
						case 0:
							x = -x - Width;
							y += Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 1:
							hulp = x;
							x = -y - Width;
							y = -hulp - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 2:
							y = -y;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 3:
							hulp = x;
							x = y;
							y = hulp;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 4:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							x -= Width;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							break;

						case 5:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							y -= Width;
							Object->x2 = Width;
							break;

						case 6:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							break;

						case 7:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							Object->x2 = Width;
							break;

						case -1:
							Object->Thickness = Height;
							x = -x;
							x2 = -Width;
							y2 = Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_ALL_ANGLE:
#ifdef _DEBUG
					if (InRange4(96 * 2540.0, CalcLengthLine(x, y, x2, y2)))
						ok = 1;

#endif

					if (Mirror == 1)
					{
						x = -x;
						x2 = -x2;
					}

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						hulp = x2;
						x2 = -y2;
						y2 = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						x2 = -x2;
						y2 = -y2;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						hulp = x2;
						x2 = y2;
						y2 = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						RotatePoint2(&x2, &y2, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = x2 + OX + OffsetX;
					Object->y2 = y2 + OY + OffsetY;
					Object->Thickness = Pad->Special.Extra1;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->ObjectType = ShapeType;
					break;

				case PIN_ARC:	// arc
					ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
					x1 = (*ShapeLines)[2];
					y1 = (*ShapeLines)[3];
					x2 = (*ShapeLines)[4];
					y2 = (*ShapeLines)[5];
					x3 = (*ShapeLines)[6];
					y3 = (*ShapeLines)[7];
					x4 = (*ShapeLines)[8];
					y4 = (*ShapeLines)[9];

					if (x2 == 0)
						x2 = y2;

					if (y2 == 0)
						y2 = x2;

					if (Mirror == 1)
					{
						x1 = -x1;
						x3 = -x3;
						x4 = -x4;
						hulp = x4;
						x4 = x3;
						x3 = hulp;
						hulp = y4;
						y4 = y3;
						y3 = hulp;
					}

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x1;
						x1 = -y1;
						y1 = hulp;
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						hulp = x3;
						x3 = -y3;
						y3 = hulp;
						hulp = x4;
						x4 = -y4;
						y4 = hulp;
						break;

					case 2:
						x1 = -x1;
						y1 = -y1;
						x3 = -x3;
						y3 = -y3;
						x4 = -x4;
						y4 = -y4;
						break;

					case 3:
						hulp = x1;
						x1 = y1;
						y1 = -hulp;
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						hulp = x3;
						x3 = y3;
						y3 = -hulp;
						hulp = x4;
						x4 = y4;
						y4 = -hulp;
						break;

					case 4:	// 45
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;

					case 5:	// 135
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;

					case 6:	// 225
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;

					case 7:	// 315
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						break;

					case -1:	// Any angle
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;
					}

					Object->x1 = x1 + OX + OffsetX;
					Object->y1 = y1 + OY + OffsetY;
					Object->x2 = x2;
					Object->y2 = y2;
					Object->x3 = x3;
					Object->y3 = y3;
					Object->x4 = x4;
					Object->y4 = y4;
					Object->ObjectType = ShapeType;
					Object->Thickness = (*ShapeLines)[10];
					Object->Clearance = max(Design.StandardClearance, (*ShapeLines)[11]);
					break;

				case OBJECT_POLYGON:
				case PIN_SMD_POLYGON:
					GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + Pad->Special.AddressOffset]);
					PolygonVertices = GeomPolygon->NrVertices;

					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->RotationAngle = RotationAngle;
					Object->Mirror = Mirror;
					Object->x2 = 0.0;
					Object->y2 = 0.0;
					Object->x3 = 0.0;
					Object->Address = (uint8 *) GeomPolygon;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_SMD_POLYGON;
					break;

				case PIN_PUT_THROUGH_POLYGON:
					GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + Pad->Special.AddressOffset]);
					PolygonVertices = GeomPolygon->NrVertices;

					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->RotationAngle = RotationAngle;
					Object->Mirror = Mirror;
					Object->x2 = 0.0;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					Object->Address = (uint8 *) GeomPolygon;
					Object->Clearance = max(Design.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_PUT_THROUGH_POLYGON;
					break;

				default:
					ObjectInclude = 0;
					break;
				}

				Object->Clearance = max(Design.StandardClearance, Object->Clearance);
			}

			if (ShapeType != PIN_ARC)
				MemPos += sizeof(PadRecord);
			else
				MemPos += 48;

#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "z100") == 0)
			{

			}

#endif
			NrPinShapes--;

			if (ObjectInclude)
			{
#ifdef _DEBUG

				if ((InRangeSpecial(Object->x1, 2.54e5, 0.05e5)) && (InRangeSpecial(Object->y1, 11.05e5, 0.05e5)))
					ok = 1;

				if (Object->Layer == 4301)
					ok = 1;

				if (stricmpOwn(Comp->Name, "D1") == 0)
				{
					ok = 1;

					if (Object->y1 < 10e5)
						ok = 1;

					if (CompObjectCount == 450)
						ok = 1;
				}

#endif

				if (ObjectArrayNr == 0)
				{
					if (NrObjects >= MaxNrObjects - 1)
					{
						if (AllocateMemObjects(MaxNrObjects + 1024) == 0)
							NrObjects++;
					}
					else
						NrObjects++;
				}
				else
				{
					if (NrObjects2 >= MaxNrObjects2 - 1)
					{
						if (AllocateMemObjects2(MaxNrObjects2 + 128) == 0)
							NrObjects2++;
					}
					else
						NrObjects2++;
				}

				CompObjectCount++;
			}
		}

		NrPins--;
		PinsMemPos += sizeof(CompPinRecord);
		PinNr++;
	}

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "u2") == 0)
		ok = 1;

#endif
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapePlacementOutLineToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation)
{
	int32 MemPos, *ShapeInfoP, NrLines, ShapeInfo2, Rotation, ShapeInfo, TextLength, ShapeNr, Mirror;
	double hulp, x1, y1, x2, y2, x3, y3, x4, y4, OX, OY, OriginX, OriginY, RotationAngle;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	uint8 y4a;
	ShapeRecord *Shape;

// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored


	if (MaxNrObjects < 1024)
		AllocateMemObjects(1024);

	ShapeNr = (int32) Comp->ShapeNr;
	OriginX = Comp->CompOriginX;
	OriginY = Comp->CompOriginY;
	Mirror = ((Comp->CompMode & 8) >> 3);

	if (ShapeNr == -1)
		return;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "U100") == 0)
		ok = 1;

	if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
		ok = 1;

	if (Mirror == 1)
		ok = 1;

#endif
	ShapeInfo2 = Shape->Info;

	RotationAngle = Comp->Rotation + TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	OX = OriginX;
	OY = OriginY;
	MemPos += sizeof(ShapeRecord);
	NrLines = min(16384, Shape->NrPlacementOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeInfoP = (int32 *) ShapeLines;
		ShapeInfo = *ShapeInfoP;
		TextLength = ShapeInfo & 0xff;
		ShapeInfo &= 0x0000ff00;

		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		Object = &((*Objects)[NrObjects]);
		Object->CompNr = 0;
		Object->Info = 0;
		Object->ObjectType2 = 0;

		if (Mirror == 0)
		{
			if ((Comp->Info & (COMPONENT_PROTECTED)) == 0)
				Object->Layer = PLACEMENT_OUTLINE_TOP;
			else
				Object->Layer = PLACEMENT_OUTLINE_TOP2;
		}
		else
		{
			if ((Comp->Info & (COMPONENT_PROTECTED)) == 0)
				Object->Layer = PLACEMENT_OUTLINE_BOTTOM;
			else
				Object->Layer = PLACEMENT_OUTLINE_BOTTOM2;
		}

		switch (ShapeInfo)
		{
		case OBJECT_LINE:		// line
			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2 + OX + OffsetX;
			Object->y2 = y2 + OY + OffsetY;
			Object->ObjectType = OBJECT_LINE;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			Object->Thickness = 0.0;

			if (Mirror == 1)
				x1 = -x1;

			Object->ObjectType = OBJECT_RECT;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->Clearance = 0.0;
			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			x3 = (*ShapeLines)[5];
			y3 = (*ShapeLines)[6];
			x4 = (*ShapeLines)[7];
			y4 = (*ShapeLines)[8];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			MemPos += 40;
			break;
		}

		if (NrObjects >= MaxNrObjects - 1)
		{
			if (AllocateMemObjects(MaxNrObjects + 1024) == 0)
				NrObjects++;
		}
		else
			NrObjects++;

		NrLines--;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetShapeCompPinNrByPinText(ShapeRecord * Shape, LPSTR PinText)
{
	int32 MemPos, cnt, cnt2, NrPins, NrPinShapes, ShapeType;

	ShapePadRecord *ShapePad;
	PadRecord *Pad;

	MemPos = (int32) ((int32) Shape - (int32) ShapesMem);
	NrPins = Shape->NrPins;
	MemPos += Shape->PinOffset;

	for (cnt = 0; cnt < NrPins; cnt++)
	{
		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		NrPinShapes = ShapePad->NrPinShapes;
		MemPos += sizeof(ShapePadRecord);

		if (stricmpOwn(PinText, ShapePad->Name) == 0)
			return cnt;

		for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);
			ShapeType = Pad->ShapeType;

			if (ShapeType != PIN_ARC)
				MemPos += sizeof(PadRecord);
			else
				MemPos += 48;
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MemSizeComp(CompRecord * Comp)
{
	int32 MemPos;
	int32 ShapeNr;
	ShapeRecord *Shape;

	ShapeNr = (int32) Comp->ShapeNr;

	if (ShapeNr == -1)
		return sizeof(CompRecord);

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

	return sizeof(CompRecord) + (int32) Comp->NrPins * sizeof(CompPinRecord);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapeCompOutLineToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation)
{
	int32 MemPos, NrLines, Rotation, ShapeType, TextLength, ShapeMirror, NewMirror, *ShapeTypeP, PolygonVertices,
	      ShapePos, *PolygonPointer, ShapeNr, Mirror, ShapeInfo2;
	double hulp, x1, y1, x2, y2, x3, y3, x4, y4, OX, OY, OriginX, OriginY, RotationAngle, RotationAngle2, TextRotation;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	uint8 y4a;
	ShapeRecord *Shape;
	int32 ObjectInclude;
	GeomPolygonRecord *GeomPolygon;

// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored

	if (MaxNrObjects < 1024)
		AllocateMemObjects(1024);

	PolygonVertices = 0;
	ShapeNr = (int32) Comp->ShapeNr;

	if (ShapeNr == -1)
		return;

	OriginX = Comp->CompOriginX;
	OriginY = Comp->CompOriginY;
	Mirror = ((Comp->CompMode & 8) >> 3);
	MemPos = (*Shapes)[ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	ShapeInfo2 = Shape->Info;
#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "R999") == 0)
		ok = 1;

	if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
		ok = 1;

#endif
	RotationAngle = Comp->Rotation + TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	OX = OriginX;
	OY = OriginY;
	MemPos += Shape->CompOutLineOffset;
	NrLines = min(16384, Shape->NrCompOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeTypeP = (int32 *) ShapeLines;
		ShapeType = *ShapeTypeP;
		TextLength = ShapeType & 0xff;
		ShapeType &= 0x0000ff00;
		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		ObjectInclude = 1;

		if (Objects == NULL)
			Objects = (ObjectArray *) GlobalLock(ObjectsGlobal);

		Object = &((*Objects)[NrObjects]);
		Object->CompNr = 0;
		Object->Info = 0;
		Object->ObjectType2 = 0;
		Object->Layer = COMP_OUTLINE_LAYER;

		switch (ShapeType)
		{
		case OBJECT_LINE:		// line
			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2 + OX + OffsetX;
			Object->y2 = y2 + OY + OffsetY;
			Object->ObjectType = OBJECT_LINE;
			Object->Thickness = (*ShapeLines)[5];
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			Object->x2 = x2;
			Object->y2 = y2;

			if (Mirror == 1)
			{
				x1 = -x1;
				Object->Layer ^= 1;
			}

			Object->ObjectType = OBJECT_RECT;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			x3 = (*ShapeLines)[5];
			y3 = (*ShapeLines)[6];
			x4 = (*ShapeLines)[7];
			y4 = (*ShapeLines)[8];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[9];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 40;
			break;

		case OBJECT_TEXT:		// text
			GetRotationAndMirrorFromShapeText(y2, &TextRotation, &ShapeMirror);

			RotationAngle = Comp->Rotation + TempRotation;
			RotationAngle2 = TextRotation;
			NewMirror = Mirror ^ ShapeMirror;

			if (Mirror == 0)
				RotationAngle2 += RotationAngle;
			else
			{
				x1 = -x1;
				RotationAngle2 -= RotationAngle;
				Object->Layer ^= 1;
			}

			if (RotationAngle2 > 360)
				RotationAngle2 -= 360;

			if (RotationAngle2 < 0)
				RotationAngle2 += 360;

			Rotation = GetRotationFromFloat(RotationAngle);

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->RotationAngle = RotationAngle2;
			Object->Mirror = Mirror;
			Object->ObjectType = OBJECT_TEXT;
			Object->Thickness = (*ShapeLines)[5];
			Object->TraceNr = (int32) & ((*ShapeLines)[6]);
			MemPos += 24 + 64;
			break;

		case PIN_SMD_POLYGON:
		case OBJECT_POLYGON:
			PolygonPointer = (int32 *) & (*ShapeLines)[3];
			GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + *PolygonPointer]);
			PolygonVertices = GeomPolygon->NrVertices;

			if (Mirror == 1)
			{
				x1 = -x1;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->RotationAngle = RotationAngle;
			Object->Mirror = Mirror;
			Object->Address = (uint8 *) GeomPolygon;
			Object->Info = OBJECT_FILLED;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			Object->ObjectType = OBJECT_POLYGON;
			MemPos += 16;
			break;
		}

		if (ObjectInclude)
		{
			if (NrObjects >= MaxNrObjects - 1)
			{
				if (AllocateMemObjects(MaxNrObjects + 1024) == 0)
					NrObjects++;
			}
			else
				NrObjects++;
		}

		NrLines--;
	}

	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapeCompSilkScreenToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation)
{
	int32 MemPos, NrLines, Rotation, ShapeType, TextLength, PolygonVertices, ShapeMirror, *ShapeTypeP, ShapePos,
	      *PolygonPointer, NewMirror, ShapeNr, Mirror, ShapeInfo2;
	double hulp, x1, y1, x2, y2, x3, y3, x4, y4, OX, OY, OriginX, OriginY, RotationAngle, RotationAngle2, TextRotation;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	uint8 y4a;
	ShapeRecord *Shape;
	int32 ObjectInclude;
	GeomPolygonRecord *GeomPolygon;

// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored

	if (MaxNrObjects < 1024)
		AllocateMemObjects(1024);

	PolygonVertices = 0;
	ShapeNr = (int32) Comp->ShapeNr;
	OriginX = Comp->CompOriginX;
	OriginY = Comp->CompOriginY;
	Mirror = ((Comp->CompMode & 8) >> 3);

	if (ShapeNr == -1)
		return;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	ShapeInfo2 = Shape->Info;
#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "Z100") == 0)
		ok = 1;

	if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
		ok = 1;

#endif

	RotationAngle = Comp->Rotation + TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	OX = OriginX;
	OY = OriginY;
	MemPos += Shape->SilkScreenOffset;
	NrLines = min(16384, Shape->NrSilkScreenOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeTypeP = (int32 *) ShapeLines;
		ShapeType = *ShapeTypeP;
		TextLength = ShapeType & 0xff;
		ShapeType &= 0x0000ff00;
		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		ObjectInclude = 1;

		if (Objects == NULL)
			Objects = (ObjectArray *) GlobalLock(ObjectsGlobal);

		Object = &((*Objects)[NrObjects]);
		Object->CompNr = 0;
		Object->Info = 0;
		Object->ObjectType2 = 0;
		Object->Layer = SILKSCREEN_TOP;

		switch (ShapeType)
		{
		case OBJECT_LINE:		// line
			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2 + OX + OffsetX;
			Object->y2 = y2 + OY + OffsetY;
			Object->ObjectType = OBJECT_LINE;
			Object->Clearance = 0.0;
			Object->Thickness = max(Design.SilkScreenWidth, (*ShapeLines)[5]);
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			Object->x2 = x2;
			Object->y2 = y2;

			if (Mirror == 1)
			{
				x1 = -x1;
				Object->Layer ^= 1;
			}

			Object->ObjectType = OBJECT_RECT;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				Object->x2 = x2;
				Object->y2 = y2;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				Object->x2 = x2;
				Object->y2 = y2;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->Clearance = 0.0;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			x3 = (*ShapeLines)[5];
			y3 = (*ShapeLines)[6];
			x4 = (*ShapeLines)[7];
			y4 = (*ShapeLines)[8];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[9];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 40;
			break;

		case OBJECT_TEXT:		// text

#ifdef _DEBUG
			if (stricmpOwn((LPSTR) & ((*ShapeLines)[6]), "text0") == 0)
				ok = 1;

			if (TempRotation == 90.0)
				ok = 1;

#endif

			GetRotationAndMirrorFromShapeText(y2, &TextRotation, &ShapeMirror);

			RotationAngle = Comp->Rotation + TempRotation;
			RotationAngle2 = TextRotation;
			NewMirror = Mirror ^ ShapeMirror;

			if (Mirror == 0)
				RotationAngle2 += RotationAngle;
			else
			{
				x1 = -x1;
				RotationAngle2 -= RotationAngle;
				Object->Layer ^= 1;
			}

			if (RotationAngle2 > 360)
				RotationAngle2 -= 360;

			if (RotationAngle2 < 0)
				RotationAngle2 += 360;

			Rotation = GetRotationFromFloat(RotationAngle);

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->RotationAngle = RotationAngle2;
			Object->Mirror = Mirror;
			Object->ObjectType = OBJECT_TEXT;
			Object->Thickness = (*ShapeLines)[5];
			Object->TraceNr = (int32) & ((*ShapeLines)[6]);
			MemPos += 24 + 64;
			break;

		case OBJECT_POLYGON:
			PolygonPointer = (int32 *) & ((*ShapeLines)[3]);
			GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + *PolygonPointer]);
			PolygonVertices = GeomPolygon->NrVertices;

			if (Mirror == 1)
			{
				Object->Layer ^= 1;
				x1 = -x1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->RotationAngle = RotationAngle;
			Object->Mirror = Mirror;
			Object->Address = (uint8 *) GeomPolygon;
			Object->Info = OBJECT_FILLED;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			Object->ObjectType = OBJECT_POLYGON;
			MemPos += 16;
			break;
		}

		if (ObjectInclude)
		{
			if (NrObjects >= MaxNrObjects - 1)
			{
				if (AllocateMemObjects(MaxNrObjects + 1024) == 0)
					NrObjects++;
			}
			else
				NrObjects++;
		}

		NrLines--;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapeOtherToObject(CompRecord * Comp, double OffsetX, double OffsetY, double TempRotation, int32 SelectedLayer,
                        int32 mode)
{

	/*
	mode :

	0:  Everything
	1:  Paste/solder masks
	2:  Info objects
	3:  Boardoutline objects
	4:  Keepout
	5:  Silkscreen

	*/

	int32 MemPos, NrLayers, NrLines, Rotation, Layer, PolygonVertices, ShapePos, ShapeNr, ShapeMirror, NewMirror,
	      Mirror, ShapeType;
	double hulp, x, y, OriginX, OriginY, Width, Height, RotationAngle, x1, y1, x2, y2, x3, y3, x4, y4, RotationAngle2,
	       TextRotation;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	PadRecord *Pad;
	uint8 y4a;
	int32 OkToAddObject, ObjectInclude;
	LPSTR TextP;
	ShapeRecord *Shape;
	GeomPolygonRecord *GeomPolygon;

// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored

	x1 = 0.0;
	y1 = 0.0;
	x2 = 0.0;
	y2 = 0.0;

	if (MaxNrObjects < 1024)
		AllocateMemObjects(1024);

	PolygonVertices = 0;
	ShapeNr = (int32) Comp->ShapeNr;

	if (ShapeNr == -1)
		return;

	OriginX = Comp->CompOriginX;
	OriginY = Comp->CompOriginY;
	Mirror = ((Comp->CompMode & 8) >> 3);
	MemPos = (*Shapes)[ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	NrLayers = Shape->NrLayers;

#ifdef _DEBUG

	if (stricmpOwn(Comp->Name, "J1") == 0)
		ok = 1;

	if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
		ok = 1;

#endif

	RotationAngle = Comp->Rotation + TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	MemPos += Shape->OtherObjectsOffset;
	NrLines = min(65536, Shape->NrOtherObjects);

	while (NrLines > 0)
	{
		Pad = (PadRecord *) & (ShapesMem[MemPos]);
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		Layer = Pad->Layer;
#ifdef _DEBUG

		if (Layer == BOARD_OUTLINE_LAYER)
			ok = 1;

#endif
		ObjectInclude = 1;

		if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		{
			Layer -= ROUTING_KEEPOUT_LAYER;
			ObjectInclude = CheckGeometryLayer(&Layer, NrLayers, Mirror);
			Layer += ROUTING_KEEPOUT_LAYER;
		}

		ShapeType = Pad->ShapeType;
		OkToAddObject = 0;
		x = Pad->X;
		y = Pad->Y;
		Width = Pad->Width;
		Height = Pad->Height;

		if (NrObjects >= MaxNrObjects - 1)
			AllocateMemObjects(MaxNrObjects + 1024);

		if (Objects == NULL)
			Objects = (ObjectArray *) GlobalLock(ObjectsGlobal);

		Object = &((*Objects)[NrObjects]);
		Object->Layer = Layer;
		Object->Info = 0;
		Object->TraceNr = 0;
		Object->ObjectType = ShapeType;
		Object->ObjectType2 = 0;

		switch (ShapeType)
		{
		case OBJECT_LINE:		// line
		case PIN_LINE_ALL_ANGLE:	// line
		case PIN_LINE_VER:
		case PIN_LINE_HOR:
		case PIN_LINE_DIAG1:
		case PIN_LINE_DIAG2:
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;

			switch (ShapeType)
			{
			case PIN_LINE_VER:
				x2 = x;
				y2 = y + Width;
				break;

			case PIN_LINE_HOR:
				x2 = x + Width;
				y2 = y;
				break;

			case PIN_LINE_DIAG1:
				x2 = x + Width;
				y2 = y - Width;
				break;

			case PIN_LINE_DIAG2:
				x2 = x + Width;
				y2 = y + Width;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2 + OffsetX + OriginX;
			Object->y2 = y2 + OffsetY + OriginY;
			Object->ObjectType = OBJECT_LINE;
			Object->Thickness = Pad->Special.Thickness;

			if (ShapeType == OBJECT_LINE)
				Object->Info2 = (int32) (Pad->Extra2 + 0.1);

			switch (ShapeType)
			{
			case PIN_LINE_VER:
			case PIN_LINE_HOR:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
				Object->Thickness = Height;
				break;
			}

			break;

		case OBJECT_RECT:
		case PIN_SMD_RECT:
			Object->ObjectType = OBJECT_RECT;

			if (Mirror == 1)
			{
				x = -x;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			Object->Clearance = 0.0;
			Object->Thickness = Pad->Special.Thickness;

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x;
				x = -y;
				y = hulp;
				hulp = Width;
				Width = Height;
				Height = hulp;
				break;

			case 2:
				x = -x;
				y = -y;
				break;

			case 3:
				hulp = x;
				x = y;
				y = -hulp;
				hulp = Width;
				Width = Height;
				Height = hulp;
				break;

			default:
				RotatePoint2(&x, &y, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x + OffsetX + OriginX;
			Object->y1 = y + OffsetY + OriginY;
			Object->x2 = Width;
			Object->y2 = Height;
			break;

		case OBJECT_CIRCLE:
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->Clearance = 0.0;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = Pad->Special.Thickness;

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			break;

		case OBJECT_ARC:		// arc
		case PIN_ARC:			// arc
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;
			x3 = (*ShapeLines)[6];
			y3 = (*ShapeLines)[7];
			x4 = (*ShapeLines)[8];
			y4 = (*ShapeLines)[9];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Clearance = 0.0;
			Object->Thickness = (*ShapeLines)[10];
			Object->Info = 0;

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			break;

		case OBJECT_TEXT:		// text
			TextP = (LPSTR) & ((*ShapeLines)[7]);
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "J1") == 0)
				ok = 1;

			if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
				ok = 1;

			if (stricmpOwn(TextP, "   Info1_0") == 0)
			{
				ok = 1;

				if (Pad->Height > 1000.0)
					ok = 1;
			}

#endif
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;

			GetRotationAndMirrorFromShapeText(Pad->Height, &TextRotation, &ShapeMirror);

			RotationAngle = Comp->Rotation + TempRotation;
			RotationAngle2 = TextRotation;
			NewMirror = Mirror ^ ShapeMirror;

			if (Mirror == 0)
			{
				RotationAngle2 += RotationAngle;

				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				x1 = -x1;
				RotationAngle2 -= RotationAngle;
			}

			if (RotationAngle2 > 360)
				RotationAngle2 -= 360;

			if (RotationAngle2 < 0)
				RotationAngle2 += 360;

			Rotation = GetRotationFromFloat(RotationAngle);

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2;
			Object->RotationAngle = RotationAngle2;
			Object->Mirror = Mirror;
			Object->ObjectType = OBJECT_TEXT;
			Object->Thickness = (*ShapeLines)[6];
			Object->Clearance = 0.0;
			Object->TraceNr = (int32) TextP;
			break;

		case PIN_SMD_POLYGON:
		case OBJECT_POLYGON:
#ifdef _DEBUG
			ok = 1;				// ShapePos
#endif
			GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + Pad->Special.AddressOffset]);
#ifdef _DEBUG

			if (GeomPolygon == NULL)
			{
				ok = 1;			// ShapePos
			}

#endif
			PolygonVertices = GeomPolygon->NrVertices;

			if (Mirror == 1)
			{
				x = -x;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "Z100") == 0)
			{
				ok = 1;

				if (Object->Layer == PASTE_MASK_TOP)
				{
					ok = 1;		// ShapePos
				}
			}

#endif

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x;
				x = -y;
				y = hulp;
				break;

			case 2:
				x = -x;
				y = -y;
				break;

			case 3:
				hulp = x;
				x = y;
				y = -hulp;
				break;

			default:
				RotatePoint2(&x, &y, RotationAngle);
				break;
			}

			Object->x1 = x + OffsetX + OriginX;
			Object->y1 = y + OffsetY + OriginY;
#ifdef _DEBUG

			if ((InRange9(Object->x1, 3.5e5)) && (InRange9(Object->y1, 244.8e5)))
				ok = 1;

#endif
			Object->RotationAngle = RotationAngle;
			Object->Mirror = Mirror;
			Object->Address = (uint8 *) GeomPolygon;
			Object->Thickness = 0.0;
			Object->Info = OBJECT_FILLED;
			Object->Clearance = 0.0;
			Object->ObjectType = OBJECT_POLYGON;
			break;
		}

		switch (ShapeType)
		{
		case OBJECT_ARC:
		case PIN_ARC:
			MemPos += 48;
			break;

		case OBJECT_TEXT:
			MemPos += 28 + 64;
			break;

		default:
			MemPos += sizeof(PadRecord);
			break;
		}

		switch (mode & 15)
		{
		case 0:				// Everything
			OkToAddObject = 1;
			break;

		case 1:				// Paste/solder masks
			if ((Layer == SOLD_MASK_BOTTOM) || (Layer == SOLD_MASK_TOP) || (Layer == PASTE_MASK_BOTTOM)
			        || (Layer == PASTE_MASK_TOP))
				OkToAddObject = 1;

			break;

		case 2:				// Info objects
			if ((Layer == INFO_LAYER) || (Layer == INFO_LAYER2) || (Layer == INFO_LAYER3) || (Layer == INFO_LAYER4))
				OkToAddObject = 1;

			break;

		case 3:				// Boardoutline objects
			if (Layer == BOARD_OUTLINE_LAYER)
				OkToAddObject = 1;

			break;

		case 4:				// Keepout
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			{
				if (SelectedLayer == -1)
					OkToAddObject = 1;
				else
				{
					if (Layer == SelectedLayer)
						OkToAddObject = 1;
				}
			}

			break;

		case 5:				// Silkscreen
			if ((Layer == SILKSCREEN_TOP) || (Layer == SILKSCREEN_BOTTOM))
				OkToAddObject = 1;

			break;
		}

		if ((ObjectInclude) && (OkToAddObject))
		{
			if (NrObjects >= MaxNrObjects - 1)
			{
				if (AllocateMemObjects(MaxNrObjects + 1024) == 0)
					NrObjects++;
			}
			else
				NrObjects++;
		}

		NrLines--;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double DistancePointToConnection(double px, double py, double ConnectionX1, double ConnectionY1, double ConnectionX2,
                                 double ConnectionY2)
{
	/*

	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	    (y2-y1)
	a = -------
	    (x2-x1)

	           (y2-y1)                     (y2-y1)
	y = (x-x1)*------- + y1       = (x-x2)*------- + y2
	           (x2-x1)                     (x2-x1)


	y = (x-x1)*a + y1

	           (x2-x1)                     (x2-x1)
	x = (y-y1)*------- + x1       = (y-y2)*------- + x2
	           (y2-y1)                     (y2-y1)

	x = (y-y1)/a + x1

	-----------------------------------------------------------------------

	a = 1

	y = x-x1 + y1     b = -x1 + y1

	x = y-y1 + x1

	a = -1

	y = x1-x + y1     b =  x1 + y1

	x = y1-y + x1

	memcmp

	*/


	double minx, maxx, miny, maxy, DistConnection1, DistConnection2, a, x2, y2, DistCentre;

	minx = min(ConnectionX1, ConnectionX2);
	maxx = max(ConnectionX1, ConnectionX2);
	miny = min(ConnectionY1, ConnectionY2);
	maxy = max(ConnectionY1, ConnectionY2);

	DistConnection1 = sqrt((ConnectionX1 - px) * (ConnectionX1 - px) + (ConnectionY1 - py) * (ConnectionY1 - py));
	DistConnection2 = sqrt((ConnectionX2 - px) * (ConnectionX2 - px) + (ConnectionY2 - py) * (ConnectionY2 - py));

	if (ConnectionX2 == ConnectionX1)
	{
		DistCentre = fabs(ConnectionX1 - px);

		if ((py >= miny) && (py <= maxy) && (DistCentre < DistConnection1) && (DistCentre < DistConnection2))
			return DistCentre;

	}
	else
	{
		if (ConnectionY2 == ConnectionY1)
		{
			DistCentre = fabs(ConnectionY1 - py);

			if ((px >= minx) && (px <= maxx) && (DistCentre < DistConnection1) && (DistCentre < DistConnection2))
				return DistCentre;

		}
		else
		{
			a = ((ConnectionY2 - ConnectionY1) / (ConnectionX2 - ConnectionX1));
			x2 = (ConnectionX1 * a - ConnectionY1 + (px / a) + py) / (a + 1 / a);

			if ((x2 >= minx) && (x2 <= maxx))
			{
				y2 = ((x2 - px) / -a) + py;
				DistCentre = sqrt((x2 - px) * (x2 - px) + (y2 - py) * (y2 - py));

				if ((DistCentre < DistConnection1) && (DistCentre < DistConnection2))
					return DistCentre;
			}
		}
	}

	return min(DistConnection1, DistConnection2);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double DistancePointToTrace(double px, double py, double TraceX1, double TraceY1, double TraceX2, double TraceY2)
{
	/*

	Line with two points (x1,y1) and (x2,y2)


	y=ax+b

	    (y2-y1)
	a = -------
	    (x2-x1)

	           (y2-y1)                     (y2-y1)
	y = (x-x1)*------- + y1       = (x-x2)*------- + y2
	           (x2-x1)                     (x2-x1)


	y = (x-x1)*a + y1

	           (x2-x1)                     (x2-x1)
	x = (y-y1)*------- + x1       = (y-y2)*------- + x2
	           (y2-y1)                     (y2-y1)

	x = (y-y1)/a + x1

	-----------------------------------------------------------------------

	a = 1

	y = x-x1 + y1     b = -x1 + y1

	x = y-y1 + x1

	a = -1

	y = x1-x + y1     b =  x1 + y1

	x = y1-y + x1

	memcmp

	*/


	double minx, maxx, miny, maxy, DistTrace1, DistTrace2, a, x2, y2, DistCentre;

	minx = min(TraceX1, TraceX2);
	maxx = max(TraceX1, TraceX2);
	miny = min(TraceY1, TraceY2);
	maxy = max(TraceY1, TraceY2);

	DistTrace1 = sqrt((TraceX1 - px) * (TraceX1 - px) + (TraceY1 - py) * (TraceY1 - py));
	DistTrace2 = sqrt((TraceX2 - px) * (TraceX2 - px) + (TraceY2 - py) * (TraceY2 - py));

	if (InRange(TraceX2, TraceX1))
	{
		DistCentre = fabs(TraceX1 - px);

		if ((py >= miny) && (py <= maxy) && (DistCentre < DistTrace1) && (DistCentre < DistTrace2))
			return DistCentre;

	}
	else
	{
		if (InRange(TraceY2, TraceY1))
		{
			DistCentre = fabs(TraceY1 - py);

			if ((px >= minx) && (px <= maxx) && (DistCentre < DistTrace1) && (DistCentre < DistTrace2))
				return DistCentre;

		}
		else
		{
			a = ((TraceY2 - TraceY1) / (TraceX2 - TraceX1));
			x2 = (TraceX1 * a - TraceY1 + (px / a) + py) / (a + 1 / a);

			if ((x2 >= minx) && (x2 <= maxx))
			{
				y2 = ((x2 - px) / -a) + py;
				DistCentre = sqrt((x2 - px) * (x2 - px) + (y2 - py) * (y2 - py));

				if ((DistCentre < DistTrace1) && (DistCentre < DistTrace2))
					return DistCentre;
			}
		}
	}

	return min(DistTrace1, DistTrace2);

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double DistancePointToArc(double px, double py, double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4)
{
	int32 cnt2, LineSegments, SegmentCount;
	double xx1, yy1, xx2, yy2, LineBuf[4096], Distance;

	Distance = 1e9;
	LineSegments = ArcToLineSegments(x1, y1, x2, y2, x3, y3, x4, y4, (double *) &LineBuf, 0);
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{
		xx1 = LineBuf[SegmentCount];
		SegmentCount++;
		yy1 = LineBuf[SegmentCount];
		SegmentCount++;
		xx2 = LineBuf[SegmentCount];
		SegmentCount++;
		yy2 = LineBuf[SegmentCount];
		SegmentCount++;
		Distance = min(Distance, DistancePointToTrace(px, py, xx1, yy1, xx2, yy2));
	}

	return Distance;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDirection(double x1, double y1, double x2, double y2, int32 mode)
{
	/*

	Direction from x1,y1 to x2,y2

	                  0
	                31  1
	          29      ^       3
	        28        |        4
	       27 \       |      /  5
	            \     |    /
	              \   |  /
	     25         \ |/          7
	     24 <---------+----------> 8
	     23         / |\          9
	              /   |  \
	            /     |    \
	       21 /       |      \  11
	       20         |       12
	         19       v     13
	                17 15
	                 16

	*/

	double divx, divy, divxabs, divyabs;
	divx = x1 - x2;
	divy = y1 - y2;
	divxabs = fabs(divx);
	divyabs = fabs(divy);

	if (mode == 1)
	{
		if (InRange(divx, 0.0))
		{
			if (InRange(divy, 0.0))
				return -1;
			else
			{
				if (divy < 0.0)
					return 0;
				else
					return 16;
			}
		}

		if (InRange(divy, 0.0))
		{
			if (divx < 0.0)
				return 8;
			else
				return 24;
		}

		if (divx < 0)
		{
			if (divy < 0)
				return 4;
			else
				return 12;
		}
		else
		{
			if (divy < 0)
				return 28;
			else
				return 20;
		}
	}


	if (InRange(divx, 0))
	{
		if (InRange(divy, 0))
			return -1;
		else
		{
			if (X1SmallerThenX2(divy, 0))
				return 0;
			else
				return 16;
		}
	}

	if (InRange(divy, 0))
	{
		if (X1SmallerThenX2(divx, 0))
			return 8;
		else
			return 24;
	}

	if ((InRange(divx, divy)) && (X1SmallerThenX2(divx, 0)))
		return 4;

	if ((InRange(divx, divy)) && (X1GreaterThenX2(divx, 0)))
		return 20;

	if ((InRange(divx, -divy)) && (X1SmallerThenX2(divx, 0)))
		return 12;

	if ((InRange(divx, -divy)) && (X1GreaterThenX2(divx, 0)))
		return 28;

	if (X1SmallerThenX2(divx, 0))
	{
		if (X1SmallerThenX2(divy, 0))
		{
			if ((5 * divxabs) / 2 < divyabs)
				return 1;
			else
			{
				if (X1SmallerThenX2(divxabs, divyabs))
					return 3;
				else
				{
					if (divxabs < (5 * divyabs) / 2)
						return 5;
					else
						return 7;
				}
			}
		}
		else
		{
			if (divxabs > (5 * divyabs) / 2)
				return 9;
			else
			{
				if (X1GreaterThenX2(divxabs, divyabs))
					return 11;
				else
				{
					if ((5 * divxabs) / 2 > divyabs)
						return 13;
					else
						return 15;
				}
			}
		}
	}

	if (X1GreaterThenX2(divx, 0))
	{
		if (X1GreaterThenX2(divy, 0))
		{
			if ((5 * divxabs) / 2 < divyabs)
				return 17;
			else
			{
				if (X1SmallerThenX2(divxabs, divyabs))
					return 19;
				else
				{
					if (divxabs < (5 * divyabs) / 2)
						return 21;
					else
						return 23;
				}
			}
		}
		else
		{
			if (divxabs > (5 * divyabs) / 2)
				return 25;
			else
			{
				if (X1GreaterThenX2(divxabs, divyabs))
					return 27;
				else
				{
					if ((5 * divxabs) / 2 > divyabs)
						return 29;
					else
						return 31;
				}
			}
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNewDirection(double x1, double y1, double x2, double y2, int32 mode)
{
	/*

	Direction from x1,y1 to x2,y2

	                  0

	                  ^
	        28        |        4
	          \       |      /
	            \     |    /
	              \   |  /
	                \ |/
	     24 <---------+----------> 8
	                / |\
	              /   |  \
	            /     |    \
	          /       |      \
	       20         |       12
	                  v

	                 16

	*/
	double divx, divy, divxabs, divyabs;
	divx = x1 - x2;
	divy = y1 - y2;
	divxabs = fabs(divx);
	divyabs = fabs(divy);

	if (InRange(divx, 0))
	{
		if (InRange(divy, 0))
			return -1;
		else
		{
			if (X1SmallerThenX2(divy, 0))
				return 0;
			else
				return 16;
		}
	}

	if (InRange(divy, 0))
	{
		if (X1SmallerThenX2(divx, 0))
			return 8;
		else
			return 24;
	}

	if ((InRange(divx, divy)) && (X1SmallerThenX2(divx, 0)))
		return 4;

	if ((InRange(divx, divy)) && (X1GreaterThenX2(divx, 0)))
		return 20;

	if ((InRange(divx, -divy)) && (X1SmallerThenX2(divx, 0)))
		return 12;

	if ((InRange(divx, -divy)) && (X1GreaterThenX2(divx, 0)))
		return 28;

	if (X1SmallerThenX2(divx, 0))
	{
		if (X1SmallerThenX2(divy, 0))
		{
			if ((5 * divxabs) / 2 < divyabs)
				return 0;
			else
			{
				if (X1SmallerThenX2(divxabs, divyabs))
					return 4;
				else
				{
					if (divxabs < (5 * divyabs) / 2)
						return 4;
					else
						return 8;
				}
			}
		}
		else
		{
			if (divxabs > (5 * divyabs) / 2)
				return 8;
			else
			{
				if (X1GreaterThenX2(divxabs, divyabs))
					return 12;
				else
				{
					if ((5 * divxabs) / 2 > divyabs)
						return 12;
					else
						return 16;
				}
			}
		}
	}

	if (X1GreaterThenX2(divx, 0))
	{
		if (X1GreaterThenX2(divy, 0))
		{
			if ((5 * divxabs) / 2 < divyabs)
				return 16;
			else
			{
				if (X1SmallerThenX2(divxabs, divyabs))
					return 20;
				else
				{
					if (divxabs < (5 * divyabs) / 2)
						return 20;
					else
						return 24;
				}
			}
		}
		else
		{
			if (divxabs > (5 * divyabs) / 2)
				return 24;
			else
			{
				if (X1GreaterThenX2(divxabs, divyabs))
					return 28;
				else
				{
					if ((5 * divxabs) / 2 > divyabs)
						return 28;
					else
						return 0;
				}
			}
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CalcDirection(double x1, double y1, double x2, double y2)
{
	int32 dir;

	dir = GetDirection(x1, y1, x2, y2, 0);

	if (dir == -1)
	{
		CurrentDirection = dir;
		return;
	}

	switch (CurrentDirection)
	{
	case 0:
		if ((dir != 29) && (dir != 31) && (dir != 0) && (dir != 1) && (dir != 3))
			CurrentDirection = dir;

		break;

	case 4:
		if ((dir != 1) && (dir != 3) && (dir != 4) && (dir != 5) && (dir != 7))
			CurrentDirection = dir;

		break;

	case 8:
		if ((dir != 5) && (dir != 7) && (dir != 8) && (dir != 9) && (dir != 11))
			CurrentDirection = dir;

		break;

	case 12:
		if ((dir != 9) && (dir != 11) && (dir != 12) && (dir != 13) && (dir != 15))
			CurrentDirection = dir;

		break;

	case 16:
		if ((dir != 13) && (dir != 15) && (dir != 16) && (dir != 17) && (dir != 19))
			CurrentDirection = dir;

		break;

	case 20:
		if ((dir != 17) && (dir != 19) && (dir != 20) && (dir != 21) && (dir != 23))
			CurrentDirection = dir;

		break;

	case 24:
		if ((dir != 21) && (dir != 23) && (dir != 24) && (dir != 25) && (dir != 27))
			CurrentDirection = dir;

		break;

	case 28:
		if ((dir != 25) && (dir != 27) && (dir != 28) && (dir != 29) && (dir != 31))
			CurrentDirection = dir;

		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetMinMaxText2(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,
                    LPSTR Str)
{
	int32 Length;
	double x1, y1, x2, y2, x3, y3, x4, y4, Xmax, Xmin, Ymax, Ymin, Width;

#ifdef _DEBUG

	if (strlen(Str) > 40)
		ok = 1;

#endif
	Length = strlen(Str);

	if ((Length == 0) || (FontNr != 0))
	{
		TextMinX = X;
		TextMinY = Y;
		TextMaxX = X;
		TextMaxY = Y;
		return;
	}

	Xmin = 1e9;
	Xmax = -1e9;
	Ymin = 1e9;
	Ymax = -1e9;

	x2 = X;
	y2 = Y;

	Width = FontSize * 0.9 * DefFontSize * Length;
	x1 = 0.0;
	y1 = 0.0;
	x2 = 0.0;
	y2 = FontSize;
	x3 = Width;
	y3 = FontSize;
	x4 = Width;
	y4 = -FontSize * 0.4;
	RotatePoint2(&x1, &y1, Rotation);
	RotatePoint2(&x2, &y2, Rotation);
	RotatePoint2(&x3, &y3, Rotation);
	RotatePoint2(&x4, &y4, Rotation);

	if (Mirror == 1)
	{
		x1 = -x1;
		x2 = -x2;
		x3 = -x3;
		x4 = -x4;
	}

	TextMinX = (X + min(x1, min(x2, min(x3, x4))));
	TextMinY = (Y + min(y1, min(y2, min(y3, y4))));
	TextMaxX = (X + max(x1, max(x2, max(x3, x4))));
	TextMaxY = (Y + max(y1, max(y2, max(y3, y4))));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetMinMaxText2b(double X, double Y, double FontSize, int32 FontNr, double Rotation, int32 Alignment, int32 Mirror,WCHAR * Str)
{
	int32 code, cnt, Length;
	double x1, y1, x2, y2, x3, y3, x4, y4, Width, divx;
	WCHAR *TextP;
	char *FontStr;
	AreaFillRecord *AreaFill;

#ifdef _DEBUG
	Length = (int32) wcslen(Str);

	if (Length > 40)
		ok = 1;

#else
	
	Length = (int32) wcslen(Str);

#endif

	if ((Length == 0) || (FontNr <= 0) || (FontNr > 16))
	{
		TextMinX = -1000000000.0;
		TextMinY = -1000000000.0;
		TextMaxX = 1000000000.0;
		TextMaxY = 1000000000.0;
		return;
	}

	AllocateSpecialMem(MEM_TRUETYPE_AREAFILL, 128 * 1024, (void **) &AreaFill);
	FontStr = Design.UsedFontStr[FontNr - 1];
	Length = (int32) wcslen(Str);
	TextP = Str;
	Width = 0.0;

	for (cnt = 0; cnt < Length; cnt++)
	{
		code = *TextP++;

		if ((code != ' ') && (code != '\t'))
		{
			if (GetAreaFillFontChar((int32) code, FontNr, AreaFill) == 0)
				break;

			divx = (AreaFill->maxx - AreaFill->minx + TRUETYPE_FONT_ADD_EXTRA_X) * FontSize;
		}
		else
			divx = TRUETYPE_FONT_SPACE_EXTRA_X * FontSize;

		Width += divx;
	}

	x1 = 0.0;
	y1 = 0.0;
	x2 = 0.0;
	y2 = FontSize;
	x3 = Width;
	y3 = FontSize;
	x4 = Width;
	y4 = -FontSize * 0.4;
	RotatePoint2(&x1, &y1, Rotation);
	RotatePoint2(&x2, &y2, Rotation);
	RotatePoint2(&x3, &y3, Rotation);
	RotatePoint2(&x4, &y4, Rotation);

	if (Mirror == 1)
	{
		x1 = -x1;
		x2 = -x2;
		x3 = -x3;
		x4 = -x4;
	}

	TextMinX = (X + min(x1, min(x2, min(x3, x4))));
	TextMinY = (Y + min(y1, min(y2, min(y3, y4))));
	TextMaxX = (X + max(x1, max(x2, max(x3, x4))));
	TextMaxY = (Y + max(y1, max(y2, max(y3, y4))));
	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectCount(int32 MaxObjectCount, int32 mode, int32 * NrPinsUsed)
{
	int32 MemPos, cnt, ObjectCount, PinsMemPos, MemPosComp, cc, ShapeNr;
	double cc2, cc3;
#ifdef _DEBUG
	char str2[MAX_LENGTH_STRING];
#endif

	ShapeRecord *Shape;
	CompRecord *Comp;
	ObjectArcRecord *ObjectArc;

	ObjectCount = 0;

	if (mode == 0)
	{
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
				PinsMemPos = MemPosComp + sizeof(CompRecord);
				ShapeNr = (int32) Comp->ShapeNr;

				if (ShapeNr != -1)
				{
					MemPos = (*Shapes)[ShapeNr].ShapePos;
					Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
					ObjectCount += Shape->NrPins;
#ifdef _DEBUG
					sprintf(str2, "%s  %s  %d\r\n", Comp->Name, Shape->ShapeName, Shape->NrPins);
#endif
				}
			}
		}

		if (NrPinsUsed)
			*NrPinsUsed = ObjectCount;

		cc2 = MaxObjectCount;
		cc3 = sqrt(cc2 / 314);
		cc2 = cc3 * cc3;
		cc = (int32) cc2;

		if (ObjectCount > cc)
			return 1;
	}

	if (mode == 1)
	{
		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & OBJECT_NOT_VISIBLE) == 0)
			{
				if ((!CheckObjectArcIsCircle(ObjectArc, 0)) && (ObjectArc->NetNr >= 0) && (ObjectArc->Layer >= 0)
				        && (ObjectArc->Layer < 32))
					ObjectCount++;
			}
		}

		cc2 = MaxObjectCount;
		cc3 = sqrt(cc2 / 314);
		cc2 = cc3 * cc3;
		cc = (int32) cc2;

		if (ObjectCount > cc)
			return 1;
	}

	return 0;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetTextMinMaxCompReference(CompRecord * Comp)
{
	int32 MemPos, lengte, Mirror, ShapeNr, TextVisibility;
	double x2, y2, TextRotation, h2;
	ShapeRecord *Shape;

	TextVisibility = Comp->TextVisibility;
	Mirror = (Comp->TextVisibility & 8) >> 3;
	TextRotation = (GetRotationFromComp(Comp->CompMode) + GetReferenceRotationFromComp(Comp->TextVisibility)) & 7;

	ShapeNr = Comp->ShapeNr;
	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	lengte = strlen(Comp->Name);
	h2 = Comp->CompNameHeight;

	if (h2 == 0)
		h2 = Shape->ShapeNameHeight;

	x2 = Comp->CompNameOriginX;
	y2 = Comp->CompNameOriginY;

	if (Mirror == 0)
		TextRotation = Comp->CompNameRotation + Comp->Rotation;
	else
	{
		TextRotation = Comp->CompNameRotation - Comp->Rotation;
		x2 = -x2;
	}

	x2 = x2 + Comp->CompOriginX;
	y2 = y2 + Comp->CompOriginY;
	GetMinMaxText2(x2, y2, h2, 0, TextRotation, 0, Mirror, Comp->Name);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetTextMinMaxCompValue(CompRecord * Comp)
{
	int32 MemPos, lengte, Mirror, ShapeNr, TextVisibility;
	double x2, y2, TextRotation, h2;
	ShapeRecord *Shape;

	TextVisibility = Comp->TextVisibility;


	ShapeNr = Comp->ShapeNr;
	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	lengte = strlen(Comp->Value);
	h2 = Comp->CompValueHeight;

	if (h2 == 0)
		h2 = Shape->ShapeNameHeight;

	x2 = Comp->CompValueOriginX;
	y2 = Comp->CompValueOriginX;
	Mirror = (Comp->TextVisibility & 0x80) >> 7;

	if (Mirror == 0)
		TextRotation = Comp->CompValueRotation + Comp->Rotation;
	else
	{
		TextRotation = Comp->CompValueRotation - Comp->Rotation;
		x2 = -x2;
	}

	RotatePoint2(&x2, &y2, Comp->Rotation);

	x2 = x2 + Comp->CompOriginX;
	y2 = y2 + Comp->CompOriginY;
	GetMinMaxText2(x2, y2, h2, 0, TextRotation, 0, Mirror, Comp->Value);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 OkToDrawObjectsLayer(int32 Layer)
{
	if (Layer < 32)
	{
		if ((DrawLayerCode[Layer] & 0x10) == 0x10)
			return -1;

		return 0;
	}

	if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
	{
		Layer -= ROUTING_KEEPOUT_LAYER;

		if ((DrawLayerCode[Layer] & 0x10) == 0x10)
			return -1;

		if ((Layer == 0) && (!OkToDrawRoutingKeepoutBottom))
			return -1;

		if ((Layer == Design.NrBoardLayers - 1) && (!OkToDrawRoutingKeepoutTop))
			return -1;

		if ((Layer > 0) && (Layer < Design.NrBoardLayers - 1) && (!OkToDrawRoutingKeepoutInner))
			return -1;

		return 0;
	}

	switch (Layer)
	{
	case SILKSCREEN_TOP:
		if (!OkToDrawSilkScreenTop)
			return -1;

		break;

	case SILKSCREEN_BOTTOM:
		if (!OkToDrawSilkScreenBottom)
			return -1;

		break;

	case INFO_LAYER:
		if (!OkToDrawInfoObjects)
			return -1;

		break;

	case INFO_LAYER2:
		if (!OkToDrawInfo2Objects)
			return -1;

		break;

	case INFO_LAYER3:
		if (!OkToDrawInfo3Objects)
			return -1;

		break;

	case INFO_LAYER4:
		if (!OkToDrawInfo4Objects)
			return -1;

		break;

	case BOARD_OUTLINE_LAYER:
		if (!OkToDrawBoardOutline)
			return -1;

		break;

	case SOLD_MASK_BOTTOM:
		if (DrawSoldMaskBottomMode == 0)
			return -1;

		break;

	case SOLD_MASK_TOP:
		if ((Design.NrBoardLayers == 1) || (DrawSoldMaskTopMode == 0))
			return -1;

		break;

	case PASTE_MASK_BOTTOM:
		if (DrawPasteMaskBottomMode == 0)
			return -1;

		break;

	case PASTE_MASK_TOP:
		if ((Design.NrBoardLayers == 1) || (DrawPasteMaskTopMode == 0))
			return -1;

		break;

	case DRILL_LAYER:
		if (DrawDrillMode == 0)
			return -1;

		break;

	case DRILL_UNPLATED_LAYER:
		if (DrawDrillMode == 0)
			return -1;

		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CircleToLineSegments(double x1, double y1, double Thickness, int32 CircleMode, double *LineSegments)
{
	int32 SegmentCount, NrSegments, cnt2, Start, Stop, Step;
	double x3, y3, x4, y4, CircleDiam;

	SegmentCount = 0;


	NrSegments = 32;

	if (Thickness > (400 * 2540))
		NrSegments = 64;

	if (Thickness > (1000 * 2540))
		NrSegments = 128;

	Step = 128 / NrSegments;

	switch (CircleMode)
	{
	case 1:
		Start = 0;
		Stop = NrSegments / 4;
		break;

	case 2:
		Start = (NrSegments * 3) / 4;
		Stop = NrSegments;
		break;

	case 3:
		Start = (NrSegments * 3) / 4;
		Stop = (NrSegments * 5) / 4;
		break;

	case 4:
		Start = NrSegments / 2;
		Stop = (NrSegments * 3) / 4;
		break;

	case 6:
		Start = NrSegments / 2;
		Stop = NrSegments;
		break;

	case 8:
		Start = NrSegments / 4;
		Stop = NrSegments / 2;
		break;

	case 9:
		Start = 0;
		Stop = NrSegments / 2;
		break;

	case 12:
		Start = NrSegments / 4;
		Stop = (NrSegments * 3) / 4;
		break;

	case 15:
		Start = 0;
		Stop = NrSegments;
		break;

	default:
		return -1;
	}

	CircleDiam = Thickness * 0.5;
	x4 = x1 + CircleDiam * (CircleCos[Start * Step]);
	y4 = y1 + CircleDiam * (CircleSin[Start * Step]);

	for (cnt2 = Start + 1; cnt2 < Stop + 1; cnt2++)
	{
		x3 = x4;
		y3 = y4;
		x4 = x1 + CircleDiam * (CircleCos[cnt2 * Step]);
		y4 = y1 + CircleDiam * (CircleSin[cnt2 * Step]);
		LineSegments[SegmentCount++] = x3;
		LineSegments[SegmentCount++] = y3;
		LineSegments[SegmentCount++] = x4;
		LineSegments[SegmentCount++] = y4;
	}

	return SegmentCount / 4;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ArcToLineSegments(double x1, double y1, double Width, double Height, double x2a, double y2a, double x2b,
                        double y2b, double *LineSegments, int32 mode)
{
	int32 NrSegments, cnt2, count, SegmentCount;
	double x3, y3, x4, y4, x5, y5, hoek1, hoek2, hoek, hoek_inc, Length, sinx, sinx2, lengte1, lengte2;
#ifdef _DEBUG
	double hoek1a, hoek2a;
#endif

	NrSegments = 32;
	SegmentCount = 0;

	if (mode == 1)
		NrSegments = 64;

	if (max(Width, Height) > 10e5)
		NrSegments *= 2;

	if (max(Width, Height) > 25e5)
		NrSegments *= 4;

	if (max(Width, Height) > 100e5)
		NrSegments *= 8;

	if ((InRange(x2a, x2b)) && (InRange(y2a, y2b)))
	{
		hoek1 = ANGLE_180;
		hoek2 = hoek1 + ANGLE_360;
	}
	else
	{
		x2a += x1;
		y2a += y1;
		x2b += x1;
		y2b += y1;
		ConvNormalCoorToPolar(x1, y1, x2a, y2a, &hoek1, &lengte1);
		ConvNormalCoorToPolar(x1, y1, x2b, y2b, &hoek2, &lengte2);

		if (hoek2 < hoek1)
			hoek2 += ANGLE_360;
	}

	hoek = hoek1;
	count = (int32) ((hoek2 - hoek1) / (ANGLE_360 / NrSegments));
	count = max(1, count);
	hoek_inc = ((hoek2 - hoek1) / count);
#ifdef _DEBUG
	hoek1a = hoek1 * 180 / PI;
	hoek2a = hoek2 * 180 / PI;
#endif

	sinx = sin(hoek);
	sinx2 = sinx * sinx;
	Length =
	    (Width * 0.5) * (Height * 0.5) * sqrt(1 / (SQR((Height * 0.5)) * (1 - sinx2) + SQR((Width * 0.5)) * sinx2));
	x5 = Length * cos(hoek);
	y5 = Length * sin(hoek);

	x4 = x1 + x5;
	y4 = y1 + y5;

	for (cnt2 = 0; cnt2 < count; cnt2++)
	{
		hoek += hoek_inc;
		x3 = x4;
		y3 = y4;
		sinx = sin(hoek);
		sinx2 = sinx * sinx;
		Length =
		    (Width * 0.5) * (Height * 0.5) * sqrt(1 / (SQR((Height * 0.5)) * (1 - sinx2) + SQR((Width * 0.5)) * sinx2));
		x5 = Length * cos(hoek);
		y5 = Length * sin(hoek);

		x4 = x1 + x5;
		y4 = y1 + y5;
		LineSegments[SegmentCount++] = x3;
		LineSegments[SegmentCount++] = y3;
		LineSegments[SegmentCount++] = x4;
		LineSegments[SegmentCount++] = y4;
	}

	return SegmentCount / 4;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DimensionToLineSegments(double x1, double y1, double x2, double y2, double *LineSegments, int32 mode)
{
	double Angle, Length, Length2, x1old, y1old, x2old, y2old, x3, y3, x4, y4;
	char str[MAX_LENGTH_STRING];
	int32 SegmentCount = 0;

	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

	if (Length > 0.0)
	{
		if ((mode & 12) == 0)
		{
			LineSegments[SegmentCount++] = x1;
			LineSegments[SegmentCount++] = y1;
			LineSegments[SegmentCount++] = x2;
			LineSegments[SegmentCount++] = y2;

			if ((mode & 1) == 1)
			{
				x3 = x1 + cos(Angle) * Design.ArrowLength;
				y3 = y1 + sin(Angle) * Design.ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x1, y1, 30.0);
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x1, y1, -30.0);
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}

			if ((mode & 2) == 2)
			{
				x3 = x2 - cos(Angle) * Design.ArrowLength;
				y3 = y2 - sin(Angle) * Design.ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2, y2, 30.0);
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2, y2, -30.0);
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}
		}
		else
		{
			if ((mode & 8) == 0)
			{
				GetUnitsValue(Units, Length, str, 6);
				GetMinMaxText2(0.0, 0.0, Design.DimensionHeight, 0, 0.0, 0, 0, str);

				if (Length < (TextMaxX - TextMinX) + Design.DimensionHeight * 2.0)
				{
					x1old = x1;
					y1old = y1;
					x2old = x2;
					y2old = y2;
					x1 -= cos(Angle) * Design.ArrowLength;
					y1 -= sin(Angle) * Design.ArrowLength;
					Length2 = Length + (TextMaxX - TextMinX) + Design.DimensionHeight * 2.0;
					x2 = x1old + cos(Angle) * Length2;
					y2 = y1old + sin(Angle) * Length2;
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;

					x3 = x1old - cos(Angle) * Design.ArrowLength;
					y3 = y1old - sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1old, y1old, 30.0);
					LineSegments[SegmentCount++] = x1old;
					LineSegments[SegmentCount++] = y1old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1old, y1old, -30.0);
					LineSegments[SegmentCount++] = x1old;
					LineSegments[SegmentCount++] = y1old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;

					x3 = x2old + cos(Angle) * Design.ArrowLength;
					y3 = y2old + sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, 30.0);
					LineSegments[SegmentCount++] = x2old;
					LineSegments[SegmentCount++] = y2old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, -30.0);
					LineSegments[SegmentCount++] = x2old;
					LineSegments[SegmentCount++] = y2old;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
				}
				else
				{
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					x3 = x1 + cos(Angle) * Design.ArrowLength;
					y3 = y1 + sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1, y1, 30.0);
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x1, y1, -30.0);
					LineSegments[SegmentCount++] = x1;
					LineSegments[SegmentCount++] = y1;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x3 = x2 - cos(Angle) * Design.ArrowLength;
					y3 = y2 - sin(Angle) * Design.ArrowLength;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2, y2, 30.0);
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint2(&x4, &y4, x2, y2, -30.0);
					LineSegments[SegmentCount++] = x2;
					LineSegments[SegmentCount++] = y2;
					LineSegments[SegmentCount++] = x4;
					LineSegments[SegmentCount++] = y4;
				}
			}
			else
			{
				GetUnitsValue(Units, Length, str, 6);
				GetMinMaxText2(0.0, 0.0, Design.DimensionHeight, 0, 0.0, 0, 0, str);
				x2old = x2;
				y2old = y2;
				Length2 = Length + (TextMaxX - TextMinX) + Design.DimensionHeight * 2.0;
				x2 = x1 + cos(Angle) * Length2;
				y2 = y1 + sin(Angle) * Length2;
				LineSegments[SegmentCount++] = x1;
				LineSegments[SegmentCount++] = y1;
				LineSegments[SegmentCount++] = x2;
				LineSegments[SegmentCount++] = y2;
				x3 = x2old + cos(Angle) * Design.ArrowLength;
				y3 = y2old + sin(Angle) * Design.ArrowLength;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, 30.0);
				LineSegments[SegmentCount++] = x2old;
				LineSegments[SegmentCount++] = y2old;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
				x4 = x3;
				y4 = y3;
				RotatePointFromOtherPoint2(&x4, &y4, x2old, y2old, -30.0);
				LineSegments[SegmentCount++] = x2old;
				LineSegments[SegmentCount++] = y2old;
				LineSegments[SegmentCount++] = x4;
				LineSegments[SegmentCount++] = y4;
			}
		}
	}

	return SegmentCount / 4;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDimensionTextFromLine(double x1, double y1, double x2, double y2, ObjectTextRecord2 * ObjectText2, int32 mode)
{
	double Angle, Length, cx, cy, tx, ty, x3, y3, dx, dy, TextAngle, Angle2;
	int32 Quadrant;
	char str[MAX_LENGTH_STRING];


	x3 = 0.0;
	y3 = 0.0;
	ObjectText2->Text[0] = 0;
	ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

	if (((mode & 12) != 0) && (Length > 0.0))
	{
		Quadrant = (int32) ((Angle + ANGLE_CONVERT(22.5)) / ANGLE_CONVERT(45.0));
		GetUnitsValue(Units, Length, str, 6);
		GetMinMaxText2(0.0, 0.0, ObjectText2->FontHeight, 0, 0.0, 0, 0, str);
		dx = TextMaxX - TextMinX;
		dy = TextMaxY - TextMinY;
		TextAngle = Angle;

		if ((mode & 8) == 0)
		{
			if ((mode & 4) == 4)
			{
				if (Length < (TextMaxX - TextMinX) + ObjectText2->FontHeight * 2.0)
				{
					cx = x1 + cos(Angle) * (Length + dx * 0.5 + ObjectText2->FontHeight * 2.0);
					cy = y1 + sin(Angle) * (Length + dx * 0.5 + ObjectText2->FontHeight * 2.0) +
					     ObjectText2->FontHeight * 0.1;
					Angle2 = atan(ObjectText2->FontHeight * 0.6 / max(Length + dx * 0.5, 1000000.0)) * 180 / PI;
				}
				else
				{
					cx = (x1 + x2) * 0.5;
					cy = (y1 + y2) * 0.5 + ObjectText2->FontHeight * 0.1;
					Angle2 = atan(ObjectText2->FontHeight * 0.7 / (Length * 0.5)) * 180 / PI;
				}

				tx = cx;
				ty = cy;

				switch (Quadrant)
				{
				case 8:
				case 0:
				case 1:
				case 2:
				case 7:
					RotatePointFromOtherPoint2(&tx, &ty, x1, y1, Angle2);
					x3 = tx - dx * 0.5;
					y3 = ty - dy * 0.5;
					RotatePointFromOtherPoint2(&x3, &y3, tx, ty, Angle * 180 / PI);
					break;

				case 3:
				case 4:
				case 5:
				case 6:
					RotatePointFromOtherPoint2(&tx, &ty, x1, y1, -Angle2);
					x3 = tx + dx * 0.5;
					y3 = ty + dy * 0.5;
					RotatePointFromOtherPoint2(&x3, &y3, tx, ty, Angle * 180 / PI);
					TextAngle += ANGLE_CONVERT(180.0);
					break;
				}
			}
		}
		else
		{
			Angle2 = atan(ObjectText2->FontHeight * 0.6 / max(Length + dx * 0.5, 1000000.0)) * 180 / PI;
			tx = x1 + cos(Angle) * (Length + dx * 0.5 + ObjectText2->FontHeight * 2.0);
			ty = y1 + sin(Angle) * (Length + dx * 0.5 + ObjectText2->FontHeight * 2.0) + ObjectText2->FontHeight * 0.1;

			switch (Quadrant)
			{
			case 8:
			case 0:
			case 1:
			case 2:
			case 7:
				RotatePointFromOtherPoint2(&tx, &ty, x1, y1, Angle2);
				x3 = tx - dx * 0.5;
				y3 = ty - dy * 0.5;
				RotatePointFromOtherPoint2(&x3, &y3, tx, ty, Angle * 180 / PI);
				break;

			case 3:
			case 4:
			case 5:
			case 6:
				RotatePointFromOtherPoint2(&tx, &ty, x1, y1, -Angle2);
				x3 = tx + dx * 0.5;
				y3 = ty + dy * 0.5;
				RotatePointFromOtherPoint2(&x3, &y3, tx, ty, Angle * 180 / PI);
				TextAngle += ANGLE_CONVERT(180.0);
				break;
			}
		}

		ObjectText2->X = (float) x3;
		ObjectText2->Y = (float) y3;
		strcpy(ObjectText2->Text, str);
		ObjectText2->Rotation = (float) (TextAngle * 180 / PI);
		return 0;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 TextStringToLineSegments2(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror,
                                LPSTR str, double *LineSegments)
{
	char code, *str2;
	int32 cnt, cnt2, cnt3, cnt4, NrPolyLines, lengte, count, oldx, oldy, SegmentCount;
	double incX, incY, x1d, y1d, x1, y1, lengte2, NewLine, PointX[20], PointY[20];

	lengte2 = strlen(str);

	SegmentCount = 0;
	Size *= DefFontSize;
	incX = 0.9 * Size;
	incY = 0.0;
	RotatePoint2(&incX, &incY, Rotation);
	str2 = str;
	lengte = (int32) strlen(str);

	for (cnt4 = 0; cnt4 < lengte; cnt4++)
	{
		code = (*str2);

		oldx = -100000000;
		oldy = -100000000;
		count = 0;

		if ((code >= 32) && (code < 127))
		{
			if (code > 32)
			{
				code -= 33;
				NrPolyLines = (*Chars)[code].NrPolyLines;
				cnt2 = 0;

				for (cnt = 0; cnt < NrPolyLines; cnt++)
				{
					count = 0;

					do
					{
						x1d = ((*Chars)[code].Line[cnt2 + 1]);
						y1d = ((*Chars)[code].Line[cnt2 + 2]);
						y1d -= 0.4;
						x1 = x1d * Size;
						y1 = y1d * Size;

						RotatePoint2(&x1, &y1, Rotation);

						if (Mirror == 1)
							x1 = -x1;

						PointX[count] = x1 + x;
						PointY[count] = y1 + y;
						count++;
						cnt2 += 3;
						NewLine = ((*Chars)[code].Line[cnt2]);
					}
					while (InRange2(NewLine, 0.0));

					for (cnt3 = 0; cnt3 < count - 1; cnt3++)
					{
						LineSegments[SegmentCount++] = PointX[cnt3];
						LineSegments[SegmentCount++] = PointY[cnt3];
						LineSegments[SegmentCount++] = PointX[cnt3 + 1];
						LineSegments[SegmentCount++] = PointY[cnt3 + 1];
					}
				}
			}

			if (Mirror == 0)
			{
				x += incX;
				y += incY;
			}
			else
			{
				x -= incX;
				y += incY;
			}
		}

		str2++;
	}

	return SegmentCount / 4;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectArcIsCircle(ObjectArcRecord * ObjectArc, int32 mode)
{

	if ((ObjectArc->Info & (OBJECT_FILLED)) == OBJECT_FILLED)
	{
		if (mode == 0)
			return 1;
	}

	if ((InRange(ObjectArc->Width, ObjectArc->Height)) && (InRange(ObjectArc->StartDiffX, ObjectArc->EndDiffX))
	        && (InRange(ObjectArc->StartDiffY, ObjectArc->EndDiffY)))
		return 1;


	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PrintObjectInText(ObjectRecord * Object)
{
	char str[MAX_LENGTH_STRING];

	sprintf(str, "  NewObject%d.ObjectType     = %d;\r\n", ObjectPrintCount, Object->ObjectType);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Test           = %d;\r\n", ObjectPrintCount, Object->Test);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Layer          = %d;\r\n", ObjectPrintCount, Object->Layer);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.x1             = %f;\r\n", ObjectPrintCount, Object->x1);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.y1             = %f;\r\n", ObjectPrintCount, Object->y1);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.x2             = %f;\r\n", ObjectPrintCount, Object->x2);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.y2             = %f;\r\n", ObjectPrintCount, Object->y2);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.x3             = %f;\r\n", ObjectPrintCount, Object->x3);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.y3             = %f;\r\n", ObjectPrintCount, Object->y3);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.x4             = %f;\r\n", ObjectPrintCount, Object->x4);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.y4             = %f;\r\n", ObjectPrintCount, Object->y4);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Clearance      = %f;\r\n", ObjectPrintCount, Object->Clearance);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.minx           = %f;\r\n", ObjectPrintCount, Object->minx);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.miny           = %f;\r\n", ObjectPrintCount, Object->miny);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.maxx           = %f;\r\n", ObjectPrintCount, Object->maxx);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.maxy           = %f;\r\n", ObjectPrintCount, Object->maxy);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Thickness      = %f;\r\n", ObjectPrintCount, Object->Thickness);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Address        = %p;\r\n", ObjectPrintCount, Object->Address);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.ObjectType2    = %d;\r\n", ObjectPrintCount, Object->ObjectType2);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.RotationAngle  = %f;\r\n", ObjectPrintCount, Object->RotationAngle);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Mirror         = %d;\r\n", ObjectPrintCount, Object->Mirror);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.PinCount       = %d;\r\n", ObjectPrintCount, Object->PinCount);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.TraceNr        = %d;\r\n", ObjectPrintCount, Object->TraceNr);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Info           = %d;\r\n", ObjectPrintCount, Object->Info);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.Info2          = %d;\r\n", ObjectPrintCount, Object->Info2);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.NetNr          = %d;\r\n", ObjectPrintCount, Object->NetNr);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.CompNr         = %d;\r\n", ObjectPrintCount, Object->CompNr);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.PinNr          = %d;\r\n", ObjectPrintCount, Object->PinNr);
	OutputDebugString(str);
	sprintf(str, "  NewObject%d.CompNr2        = %d;\r\n", ObjectPrintCount, Object->CompNr2);
	OutputDebugString(str);
	OutputDebugString("\r\n");
	ObjectPrintCount++;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestText2(double x1, double y1, double x2, double Rotation, int32 Mirror, LPSTR Text)
{
	double tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4;
	PolygonRecord *PolygonPointObject, *PolygonObject;
	uint8 PolygonBuf1[1024], PolygonBuf2[1024];

#ifdef _DEBUG

	if ((strchr(Text, 10)) || (strchr(Text, 13)))
		ok = 1;

#endif
	GetMinMaxText2(x1, y1, x2, 0, Rotation, 0, Mirror, Text);

	if ((TextMaxX < SearchMinX) || (TextMinX > SearchMaxX) || (TextMaxY < SearchMinY) || (TextMinY > SearchMaxY))
		return 0;

	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	GetMinMaxText2(x1, y1, x2, 0, 0.0, 0, Mirror, Text);
	tx1 = TextMinX;
	ty1 = TextMinY;
	tx2 = TextMaxX;
	ty2 = TextMinY;
	tx3 = TextMaxX;
	ty3 = TextMaxY;
	tx4 = TextMinX;
	ty4 = TextMaxY;

	if (Mirror == 1)
		Rotation = -Rotation;

	RotatePointFromOtherPoint2(&tx1, &ty1, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx2, &ty2, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx3, &ty3, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx4, &ty4, x1, y1, Rotation);

	PolygonObject->NrVertices = 4;
	PolygonObject->Points[0].x = tx1;
	PolygonObject->Points[0].y = ty1;
	PolygonObject->Points[1].x = tx2;
	PolygonObject->Points[1].y = ty2;
	PolygonObject->Points[2].x = tx3;
	PolygonObject->Points[2].y = ty3;
	PolygonObject->Points[3].x = tx4;
	PolygonObject->Points[3].y = ty4;

	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 1;

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestText2b(double x1, double y1, double x2, double Rotation, int32 Mirror, int32 FontNr, WCHAR * Text)
{
	double tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4;
	PolygonRecord *PolygonPointObject, *PolygonObject;
	uint8 PolygonBuf1[1024], PolygonBuf2[1024];

#ifdef _DEBUG

	if ((wcschr(Text, 10)) || (wcschr(Text, 13)))
		ok = 1;

#endif
	GetMinMaxText2b(x1, y1, x2, FontNr, Rotation, 0, Mirror, Text);

	if ((TextMaxX < SearchMinX) || (TextMinX > SearchMaxX) || (TextMaxY < SearchMinY) || (TextMinY > SearchMaxY))
		return 0;

	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	GetMinMaxText2b(x1, y1, x2, FontNr, 0.0, 0, Mirror, Text);
	tx1 = TextMinX;
	ty1 = TextMinY;
	tx2 = TextMaxX;
	ty2 = TextMinY;
	tx3 = TextMaxX;
	ty3 = TextMaxY;
	tx4 = TextMinX;
	ty4 = TextMaxY;

	if (Mirror == 1)
		Rotation = -Rotation;

	RotatePointFromOtherPoint2(&tx1, &ty1, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx2, &ty2, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx3, &ty3, x1, y1, Rotation);
	RotatePointFromOtherPoint2(&tx4, &ty4, x1, y1, Rotation);

	PolygonObject->NrVertices = 4;
	PolygonObject->Points[0].x = tx1;
	PolygonObject->Points[0].y = ty1;
	PolygonObject->Points[1].x = tx2;
	PolygonObject->Points[1].y = ty2;
	PolygonObject->Points[2].x = tx3;
	PolygonObject->Points[2].y = ty3;
	PolygonObject->Points[3].x = tx4;
	PolygonObject->Points[3].y = ty4;

	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 1;

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestArc2(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4,
                   double Thickness)
{
	double ArcXmin, ArcXmax, ArcYmin, ArcYmax, Thickness2;
	ObjectRecord NewObject;
	PolygonRecord *PolygonPointObject, *PolygonObject;
	uint8 PolygonBuf1[10240], PolygonBuf2[10240];

	Thickness2 = Thickness / 2;

	ArcXmin = x1 - x2 * 0.5 - Thickness2;
	ArcYmin = y1 - y2 * 0.5 - Thickness2;
	ArcXmax = x1 + x2 * 0.5 + Thickness2;
	ArcYmax = y1 + y2 * 0.5 + Thickness2;

	if ((ArcXmax < SearchMinX) || (ArcXmin > SearchMaxX) || (ArcYmax < SearchMinY) || (ArcYmin > SearchMaxY))
		return 0;


	if ((ArcXmax < SearchMaxX) && (ArcXmin > SearchMinX) && (ArcYmax < SearchMaxY) && (ArcYmin > SearchMinY))
		return 1;

	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	memset(&NewObject, 0, sizeof(NewObject));
	NewObject.x1 = x1;
	NewObject.y1 = y1;
	NewObject.x2 = x2;
	NewObject.y2 = x2;
	NewObject.x3 = x3;
	NewObject.y3 = y3;
	NewObject.x4 = x4;
	NewObject.y4 = y4;
	NewObject.ObjectType = OBJECT_ARC;
	NewObject.Thickness = max(Thickness, 0.1e5);

	MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RectTestPolygon(PolygonRecord * PolygonObject)
{
	PolygonRecord *PolygonPointObject;
	uint8 PolygonBuf1[10240];

	SetMinMaxPolygon(PolygonObject, 0);
	PolygonPointObject = (PolygonRecord *) & PolygonBuf1;

	PolygonPointObject->NrVertices = 4;
	PolygonPointObject->Points[0].x = SearchMinX;
	PolygonPointObject->Points[0].y = SearchMinY;
	PolygonPointObject->Points[1].x = SearchMaxX;
	PolygonPointObject->Points[1].y = SearchMinY;
	PolygonPointObject->Points[2].x = SearchMaxX;
	PolygonPointObject->Points[2].y = SearchMaxY;
	PolygonPointObject->Points[3].x = SearchMinX;
	PolygonPointObject->Points[3].y = SearchMaxY;

	if (CheckPolygonCompleetlyOutsidePolygon(PolygonPointObject, PolygonObject) != 1)
		return 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CompPinText(CompRecord * Comp, int32 PinNrToFind, double x, double y, LPSTR PinText)
{
	int32 PinOffset, MemPos, cnt, PolygonVertices, NrPins, PinNr, ShapePos, NrLayers, MemPosComp, Layer, ShapeInfo,
	      NrPinShapes, ShapeType, ShapeNr, Mirror;
	int32 Found;

	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	ShapeRecord *Shape;
	LPSTR PinTextFound;

	PinText[0] = 0;
	MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
	ShapeNr = (int32) Comp->ShapeNr;

	if (ShapeNr == -1)
		return -1;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	PinOffset = Shape->PinOffset;
	ShapeInfo = Shape->Info;
	NrLayers = Shape->NrLayers;
	Mirror = ((Comp->CompMode & 8) >> 3);

	PolygonVertices = 0;
	NrPins = Shape->NrPins;
	MemPos += PinOffset;
	PinNr = 0;
	Found = 0;

	do
	{
		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		MemPos += sizeof(ShapePadRecord);
		NrPinShapes = ShapePad->NrPinShapes;
		PinTextFound = (ShapePad->Name);

		for (cnt = 0; cnt < NrPinShapes; cnt++)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);

			if (PinNr == PinNrToFind)
			{
				Layer = Pad->Layer;
				Found = CheckGeometryLayer(&Layer, NrLayers, Mirror);
			}

			ShapeType = Pad->ShapeType;

			if (ShapeType != PIN_ARC)
				MemPos += sizeof(PadRecord);
			else
				MemPos += 48;
		}

		NrPins--;
		PinNr++;
	}
	while ((NrPins > 0) && (PinNr - 1 != PinNrToFind));

	if (Found)
	{
		strcpy(PinText, PinTextFound);
		return 0;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 CompPinNr(CompRecord * Comp, LPSTR PinText)
{
	int32 PinOffset, MemPos, cnt, cnt2, NrPins, MemPosComp, NrPinShapes, ShapeType, ShapeNr;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	ShapeRecord *Shape;

	MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
	ShapeNr = (int32) Comp->ShapeNr;

	if (ShapeNr == -1)
		return -1;

	MemPos = (*Shapes)[ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	PinOffset = Shape->PinOffset;

	NrPins = Shape->NrPins;
	MemPos += PinOffset;

	for (cnt = 0; cnt < NrPins; cnt++)
	{
		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		NrPinShapes = ShapePad->NrPinShapes;
		MemPos += sizeof(ShapePadRecord);

		if (stricmpOwn(PinText, ShapePad->Name) == 0)
			return cnt;

		for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);
			ShapeType = Pad->ShapeType;

			if (ShapeType != PIN_ARC)
				MemPos += sizeof(PadRecord);
			else
				MemPos += 48;
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetComponentPinLayer(CompRecord * Comp)
{
	int32 cnt2, Layer;
	ObjectRecord *Object;

	Layer = -1;
	NrObjects = 0;
	ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

	for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
	{
		Object = &((*Objects)[cnt2]);

		if (Object->Layer == -1)
			return -1;			// Pads on top/bottom

		if (Layer != -1)
		{
			if (Object->Layer != Layer)
				return -1;		// Pads on top/bottom
		}
		else
			Layer = Object->Layer;
	}

	return Layer;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ConvertObjectTextToStrings(LPSTR TextP, int32 FontNr, int32 * MaxCountX, int32 Layer)
{
	int32 NrLines, cnt2, cnt3, Length;
	WCHAR StrW16[512];
	char TextStr[MAX_LENGTH_STRING];

	if (ConvertTextString(TextP, TextStr, Layer) == 0)
		TextP = TextStr;

	if (FontNr == 0)
	{
		*MaxCountX = 0;
		NrLines = 0;
		Length = strlen(TextP);

		if (Length == 0)
			return 0;

		memset(TextStrings2, 0, sizeof(TextStrings2));
		cnt3 = 0;
		cnt2 = cnt3;

		while (cnt3 < Length + 1)
		{
			if ((TextP[cnt3] == '\r') || ((cnt3 == Length) && (TextP[cnt3 - 1] != '\n')))
			{
				if (NrLines < 64)
				{
					if (cnt3 - cnt2 > 0)
					{
						strncpy((char *) &TextStrings2[NrLines], (LPSTR) & TextP[cnt2], min(127, cnt3 - cnt2));
						*MaxCountX = max(*MaxCountX, cnt3 - cnt2);
					}

					NrLines++;
				}

				cnt3 += 1;
				cnt2 = cnt3 + 1;
			}

			cnt3++;
		}
	}
	else
	{
		Length = strlen(TextP);
		Length = MultiByteToWideChar(CP_UTF8, 0, TextP, Length, StrW16, 511);
		StrW16[Length] = 0;
		*MaxCountX = 0;
		NrLines = 0;
		Length = wcslen(StrW16);

		if (Length == 0)
			return 0;

		memset(TextStrings, 0, sizeof(TextStrings));
		cnt3 = 0;
		cnt2 = cnt3;

		while (cnt3 < Length + 1)
		{
			if ((StrW16[cnt3] == '\r') || ((cnt3 == Length) && (StrW16[cnt3 - 1] != '\n')))
			{
				if (NrLines < 64)
				{
					if (cnt3 - cnt2 > 0)
					{
						wcsncpy((WCHAR *) & TextStrings[NrLines], (WCHAR *) & StrW16[cnt2], min(127, cnt3 - cnt2));
						*MaxCountX = max(*MaxCountX, cnt3 - cnt2);
					}

					NrLines++;
				}

				cnt3 += 1;
				cnt2 = cnt3 + 1;
			}

			cnt3++;
		}
	}

	return NrLines;
}

//***********************************************************************************************************************************
//***************************************** informace myši **************************************************************************
//***********************************************************************************************************************************

int32 GetPinTextFromObject(ObjectRecord * Object, LPSTR NetText, LPSTR PinText, LPSTR LayerText, LPSTR ClearanceText)
{
	NetRecord *Net;
	char str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING],
	     str6[MAX_LENGTH_STRING], str7[MAX_LENGTH_STRING], str8[MAX_LENGTH_STRING];
	double x1, y1, x2 = 0.0, y2 = 0.0;

	str2[0] = 0;

	if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
	{
		Net = &((*Nets)[Object->NetNr]);
		strcpy(str2, Net->Name);
	}
	else
	{
		if ((Object->NetNr == 32767) || (Object->NetNr == -1))
			strcpy(str2, SC(108, "Unconnected"));
	}

	if (Object->Layer != -1)
		GetLayerText(Object->Layer, LayerText, 16 + 4);
	else
		strcpy(LayerText, SC(107, "All layers"));


	str3[0] = 0;

	switch (Object->ObjectType)
	{
	case DRILL_UNPLATED:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		sprintf(str3, SC(97, "%s , %s ( drill unplated %s )"), str4, str5, str6);
		break;

	case DRILL:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		sprintf(str3, SC(98, "%s , %s ( drill %s )"), str4, str5, str6);
		break;

	case PIN_PUT_THROUGH_ROUND:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		GetUnitsValue2(Units, Object->y2, str7, 2);
		sprintf(str3, SC(99, "%s , %s\r\nObject : THT Round pad\r\nDiameter : %s\r\nDrill : %s"), str4, str5, str6, str7); //THT kruhový pad
		break;

	case PIN_SMD_ROUND:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		sprintf(str3, SC(100, "%s , %s\r\nObject : SMD Round pad\r\nDiameter : %s"), str4, str5, str6); //SMD kruhový pad
		break;

	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
		x1 = Object->x1;
		y1 = Object->x1;

		switch (Object->ObjectType)
		{
		case PIN_LINE_HOR:
			x2 = Object->x1 + Object->x2;
			y2 = y1;
			break;

		case PIN_LINE_VER:
			x2 = x1;
			y2 = Object->y1 + Object->x2;
			break;

		case PIN_LINE_DIAG1:
			x2 = Object->x1 + Object->x2;
			y2 = Object->y1 - Object->x2;
			break;

		case PIN_LINE_DIAG2:
			x2 = Object->x1 + Object->x2;
			y2 = Object->y1 + Object->x2;
			break;
		}

		GetUnitsValue2(Units, x1, str4, 2);
		GetUnitsValue2(Units, y1, str5, 2);
		GetUnitsValue2(Units, x2, str6, 2);
		GetUnitsValue2(Units, y2, str7, 2);
		GetUnitsValue2(Units, Object->y2, str8, 2);
		sprintf(str3, SC(101, "%s , %s - %s , %s\r\nObject : Pin trase\r\nTracewidth : %s"), str4, str5, str6, str7, str8);
		break;

	case PIN_LINE_ALL_ANGLE:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		GetUnitsValue2(Units, Object->y2, str7, 2);
		GetUnitsValue2(Units, Object->Thickness, str8, 2);
		sprintf(str3, SC(101, "%s , %s - %s , %s\r\nObject : Pin trase\r\nTracewidth : %s"), str4, str5, str6, str7, str8);
		break;

	case PIN_ARC:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		GetUnitsValue2(Units, Object->y2, str7, 2);
		GetUnitsValue2(Units, Object->Thickness, str8, 2);
		sprintf(str3, SC(102, "PINARC %s , %s ( %s , %s )"), str4, str5, str6, str8);
		break;

	case PIN_SMD_POLYGON:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		sprintf(str3, SC(103, "%s , %s\r\nObject : SMD Polygon pad"), str4, str5); //SMD polygon
		break;

	case PIN_PUT_THROUGH_SQUARE:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		GetUnitsValue2(Units, Object->y2, str7, 2);
		sprintf(str3, SC(104, "%s , %s\r\nObject : SMD Square pad\r\nSize : %s\r\nDrill : %s"), str4, str5, str6, str7); //THT ètvercový pad
		break;

	case PIN_SMD_RECT:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->x2, str6, 2);
		GetUnitsValue2(Units, Object->y2, str7, 2);
		sprintf(str3, SC(105, "%s , %s\r\nObject : SMD Rect pad\r\nSize : %s x %s"), str4, str5, str6, str7); //SMD obdélníkový pad
		break;

	case PIN_PUT_THROUGH_POLYGON:
		GetUnitsValue2(Units, Object->x1, str4, 2);
		GetUnitsValue2(Units, Object->y1, str5, 2);
		GetUnitsValue2(Units, Object->y2, str6, 2);
		sprintf(str3, SC(106, "POLYGON THROUGH HOLE ( %s , %s ), drill %s"), str4, str5, str6);
		break;
	}

	GetUnitsValue2(Units, Object->Clearance, ClearanceText, 2);
	strcpy(NetText, str2);
	strcpy(PinText, str3);
	return 0;
}

//*************************************************************************************************************************************
//*************************************************************************************************************************************
//*************************************************************************************************************************************
