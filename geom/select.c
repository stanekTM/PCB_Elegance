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
#include "graphics.h"
#include "toets.h"
#include "calcrect.h"
#include "calc.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "line2.h"
#include "geom.h"
#include "math.h"
#include "rect.h"
#include "stdio.h"
#include "string.h"
#include "mainloop.h"
#include "edit.h"
#include "dialogs.h"
#include "resource.h"
#include "polygon.h"

extern HDC OutputDisplay;
int32 ok;

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ObjectLayerVisible(int32 Layer)
{
	switch (Layer)
	{
	case SOLD_MASK_BOTTOM_LAYER:
		if (!SoldMaskPadsBottomVisible)
			return 0;

		break;

	case SOLD_MASK_TOP_LAYER:
		if (!SoldMaskPadsTopVisible)
			return 0;

		break;

	case PASTE_MASK_TOP_LAYER:
		if (!PastePadsTopVisible)
			return 0;

		break;

	case PASTE_MASK_BOTTOM_LAYER:
		if (!PastePadsBottomVisible)
			return 0;

		break;

	case SILKSCREEN_TOP_LAYER:
		if (!SilkScreenTopVisible)
			return 0;

		break;

	case SILKSCREEN_BOTTOM_LAYER:
		if (!SilkScreenBottomVisible)
			return 0;

		break;

	case PLACEMENT_OUTLINE_LAYER:
		if (!PlacementVisible)
			return 0;

		break;

	case COMP_OUTLINE_LAYER:
		if (!CompOutlineVisible)
			return 0;

		break;

	case BOARD_OUTLINE_LAYER:
		if (!BoardOutlineVisible)
			return 0;

		break;

	case GEOM_NAME_LAYER:
		if (!GeomNameVisible)
			return 0;

		break;

	case POWER_PAD_LAYER:
		if (!PowerPadsVisible)
			return 0;

		break;

	case INNER_PAD_LAYER:
		if (NrPadLayers == 2)
		{
			if (!InnerPadsVisible)
				return 0;
		}

		break;

	case DRILL_LAYER:
		if (!DrillVisible)
			return 0;

		break;

	case DRILL_UNPLATED_LAYER:
		if (!DrillUnplatedVisible)
			return 0;

		break;

	case INFO_LAYER:
		if (!Info1Visible)
			return 0;

		break;

	case INFO_LAYER2:
		if (!Info2Visible)
			return 0;

		break;

	case INFO_LAYER3:
		if (!Info3Visible)
			return 0;

		break;

	case INFO_LAYER4:
		if (!Info4Visible)
			return 0;

		break;

	case POLYGON_DRAW_LAYER:
		break;

	default:
		if (Layer < 32)
		{
			if (!PadsVisible[Layer])
				return 0;
		}
		else
		{
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			{
				if (!RoutingKeepoutVisible[Layer - ROUTING_KEEPOUT_LAYER])
					return 0;
			}
			else
				return 0;
		}

		break;
	}

	return 1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void CheckObjectSelected(ObjectRecord * Object)
{
	int32 ObjectInfo, TestResult;
	int32 ObjectChanged = 0;
	double x1, y1, x2, y2, x3, y3, x4, y4;

	ObjectInfo = Object->Info;

	switch (Object->ObjectType)
	{
	case OBJECT_LINE:
		if (UnselectAll)
		{
			if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
			{
				ObjectInfo &= ~(OBJECT_SELECTED | 3);
				ObjectChanged = 1;
			}
		}
		else
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;

			if ((TestResult = RectTestLine2(x1, y1, x2, y2, Object->Thickness)) != 0)
			{
				if (!ReplaceSelections)
				{
					if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						if (ShiftPressed)
							ObjectInfo &= ~OBJECT_SELECTED;
					}
					else
					{
						ObjectInfo &= ~3;
						ObjectInfo |= (OBJECT_SELECTED | TestResult);
					}
				}
				else
				{
					if (ShiftPressed)
					{
						if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							ObjectInfo &= ~(OBJECT_SELECTED | 3);
						else
						{
							ObjectInfo &= ~3;
							ObjectInfo |= (OBJECT_SELECTED | TestResult);
						}
					}
					else
					{
						ObjectInfo &= ~3;
						ObjectInfo |= (OBJECT_SELECTED | TestResult);
					}
				}

				ObjectChanged = 1;
			}
		}

		break;

	case OBJECT_RECT:
		if (UnselectAll)
		{
			if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
			{
				ObjectInfo &= ~OBJECT_SELECTED;
				ObjectChanged = 1;
			}
		}
		else
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;
#ifdef _DEBUG

			if ((InRange5(x1, -24.08e5)) && (InRange5(y1, 12.4e5)))
				ok = 1;

#endif

			if (RectTestRect2(x1, y1, x2, y2))
			{
				if (!ReplaceSelections)
				{
					if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						if (ShiftPressed)
							ObjectInfo &= ~OBJECT_SELECTED;
					}
					else
						ObjectInfo |= (OBJECT_SELECTED);
				}
				else
				{
					if (ShiftPressed)
						ObjectInfo ^= OBJECT_SELECTED;
					else
						ObjectInfo |= OBJECT_SELECTED;
				}

				ObjectChanged = 1;
			}
		}

		break;

	case OBJECT_CIRCLE:
		if (UnselectAll)
		{
			if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
			{
				ObjectInfo &= ~OBJECT_SELECTED;
				ObjectChanged = 1;
			}
		}
		else
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;

//            CircleMode=CircleConv[Object->Info2];
			if (((Object->Thickness == 0.0) && (Object->Info2 == 0) && (RectTestCircle(x1, y1, x2, 255)))
			        || (RectTestCircle2(Object)))
			{
				if (!ReplaceSelections)
				{
					if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						if (ShiftPressed)
							ObjectInfo &= ~OBJECT_SELECTED;
					}
					else
						ObjectInfo |= (OBJECT_SELECTED);
				}
				else
				{
					if (ShiftPressed)
						ObjectInfo ^= OBJECT_SELECTED;
					else
						ObjectInfo |= OBJECT_SELECTED;
				}

				ObjectChanged = 1;
			}
		}

		break;

	/*
	    case DRILL:
	    case DRILL_UNPLATED:
	    case PIN_PUT_THROUGH_ROUND_POWER:
	    case PIN_PUT_THROUGH_ROUND_INNER_PAD:
	      if (UnselectAll) {
	        if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED) {
	          ObjectInfo&=~OBJECT_SELECTED;
	          ObjectChanged=1;
	        }
	      } else {
	        x1=Object->x1;
	        y1=Object->y1;
	        x2=Object->x2;
	//            CircleMode=CircleConv[Object->Info2];
	        if (RectTestCircle(x1,y1,x2,255)) {
	          if (!ReplaceSelections) {
	            if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED) {
	              if (ShiftPressed) {
	                ObjectInfo&=~OBJECT_SELECTED;
	              }
	            } else {
	              ObjectInfo|=(OBJECT_SELECTED);
	            }
	          } else {
	            if (ShiftPressed) {
	              ObjectInfo^=OBJECT_SELECTED;
	            } else {
	              ObjectInfo|=OBJECT_SELECTED;
	            }
	          }
	          ObjectChanged=1;
	        }
	      }
	      break;
	*/
	case OBJECT_ARC:
		if (UnselectAll)
		{
			if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
			{
				ObjectInfo &= ~OBJECT_SELECTED;
				ObjectChanged = 1;
			}
		}
		else
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;
			x3 = Object->x3;
			y3 = Object->y3;
			x4 = Object->x4;
			y4 = Object->y4;

			if (((Object->Thickness == 0.0) && (RectTestCircle(x1, y1, x2, 255))) || (RectTestArc2(Object)))
			{
				if (!ReplaceSelections)
				{
					if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						if (ShiftPressed)
							ObjectInfo &= ~OBJECT_SELECTED;
					}
					else
						ObjectInfo |= (OBJECT_SELECTED);
				}
				else
				{
					if (ShiftPressed)
						ObjectInfo ^= OBJECT_SELECTED;
					else
						ObjectInfo |= OBJECT_SELECTED;
				}

				ObjectChanged = 1;
			}
		}

		break;

	case OBJECT_TEXT:
		if (UnselectAll)
		{
			if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
			{
				ObjectInfo &= ~OBJECT_SELECTED;
				ObjectChanged = 1;
			}
		}
		else
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;

			if (Object->RotationAngle > 1000.0)
				TestResult = RectTestText2(x1, y1, x2, Object->RotationAngle - 2000.0, 1, Object->Text);
			else
				TestResult = RectTestText2(x1, y1, x2, Object->RotationAngle, 0, Object->Text);

			if (TestResult)
			{
				if (!ReplaceSelections)
				{
					if ((ObjectInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						if (ShiftPressed)
							ObjectInfo &= ~OBJECT_SELECTED;
					}
					else
						ObjectInfo |= (OBJECT_SELECTED);
				}
				else
				{
					if (ShiftPressed)
						ObjectInfo ^= OBJECT_SELECTED;
					else
						ObjectInfo |= OBJECT_SELECTED;
				}

				ObjectChanged = 1;
			}
		}

		break;
	}

	if (ObjectChanged)
	{
		Object->Info = (int16) ObjectInfo;
		DrawObject(Object, 0.0, 0.0, 0);
		DrawPinTextObject(Object, 0);

		if (ClearanceVisible)
			DrawObjectWithClearance(Object, 0.0, 0.0, 0);
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SelectObjectsFromWindow(double x1a, double y1a, double x2a, double y2a, int32 mode)
{
	int32 cnt, hulpx, hulpy, ObjectInfo, ObjectType, ObjectPolygonInfo, ObjectChanged;
	double cx, cy;
	ObjectRecord *Object, PinObject;
	ObjectPolygonRecord *ObjectPolygon;


	StartDrawingEditingWindow();

	if ((InRange(x1a, x2a)) && (InRange(y1a, y2a)))
	{
		hulpx = MultX(x1a);
		hulpy = MultY(y1a);
		SearchMinX = PixelToRealOffX(hulpx - 2);
		SearchMinY = PixelToRealOffY(DrawWindowMaxY - (hulpy + 2) - 1);
		SearchMaxX = PixelToRealOffX(hulpx + 2);
		SearchMaxY = PixelToRealOffY(DrawWindowMaxY - (hulpy - 2) - 1);
	}
	else
	{
		SearchMinX = min(x1a, x2a);
		SearchMinY = min(y1a, y2a);
		SearchMaxX = max(x1a, x2a);
		SearchMaxY = max(y1a, y2a);
	}

	LastAction = 1;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectType = Object->ObjectType;

			if ((Object->Layer != DRILL_LAYER) && (Object->Layer != DRILL_UNPLATED_LAYER))
			{
				if (ObjectLayerVisible(Object->Layer))
					CheckObjectSelected(Object);
			}
		}
	}

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ObjectType = Object->ObjectType;

			if ((Object->Layer == DRILL_LAYER) || (Object->Layer == DRILL_UNPLATED_LAYER))
			{
				if (ObjectLayerVisible(Object->Layer))
					CheckObjectSelected(Object);
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
		ObjectPolygonInfo = ObjectPolygon->Info;

		if ((ObjectPolygonInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			if (ObjectLayerVisible(ObjectPolygon->Layer))
			{
				ObjectChanged = 0;

				if (UnselectAll)
				{
					if ((ObjectPolygonInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
					{
						ObjectPolygonInfo &= ~OBJECT_SELECTED;
						ObjectChanged = 1;
					}
				}
				else
				{
					cx = (SearchMinX + SearchMaxX) * 0.5;
					cy = (SearchMinY + SearchMaxY) * 0.5;

					if (((PointInObjectPolygon(ObjectPolygon, cx, cy) & 1) == 1)
					        || (ObjectPolygonInSearchArea(ObjectPolygon) == 1))
					{
						if (!ReplaceSelections)
						{
							if ((ObjectPolygonInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
							{
								if (ShiftPressed)
									ObjectPolygonInfo &= ~OBJECT_SELECTED;
							}
							else
								ObjectPolygonInfo |= OBJECT_SELECTED;
						}
						else
						{
							if (ShiftPressed)
							{
								if ((ObjectPolygonInfo & OBJECT_SELECTED) == OBJECT_SELECTED)
									ObjectPolygonInfo &= ~(OBJECT_SELECTED);
								else
									ObjectPolygonInfo |= OBJECT_SELECTED;
							}
							else
								ObjectPolygonInfo |= OBJECT_SELECTED;
						}

						ObjectChanged = 1;
					}
				}

				if (ObjectChanged)
				{
					ObjectPolygon->Info = (int16) ObjectPolygonInfo;
					DrawObjectPolygon(ObjectPolygon, 0.0, 0.0, 0);
					PinObject.x1 = (float) ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
					PinObject.y1 = (float) ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
					PinObject.Layer = ObjectPolygon->Layer;
					PinObject.PinNr = ObjectPolygon->PinNr;
					DrawPinTextObject(&PinObject, 0);

					if (ClearanceVisible)
						DrawPolygonObjectWithClearance(ObjectPolygon, 0.0, 0.0, 0);
				}
			}
		}
	}

	ExitDrawing();
	EndDrawingEditingWindow();

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FindFirstObjectUnderCursor(double x, double y)
{
	int32 cnt, ObjectInfo;
	double x1, y1, x2, y2;

	ObjectRecord *Object;

	SearchMinX = x - 10.0;
	SearchMinY = y - 10.0;
	SearchMaxX = x + 10.0;
	SearchMaxY = y + 10.0;


	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			switch (Object->ObjectType)
			{
			case OBJECT_RECT:
				x1 = Object->x1;
				y1 = Object->y1;
				x2 = Object->x2;
				y2 = Object->y2;

				if (RectTestRect2(x1, y1, x2, y2))
					return cnt;

				break;

			case OBJECT_CIRCLE:
				x1 = Object->x1;
				y1 = Object->y1;
				x2 = Object->x2;

				if (RectTestCircle(x1, y1, x2, 255))
					return cnt;

				break;
			}
		}
	}

	return -1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetNrSelectObjects()
{

	int32 cnt, count, Layer, ObjectType, ObjectChanged;
	ObjectRecord *Object;
	ObjectPolygonRecord *ObjectPolygon;

	for (cnt = 0; cnt < 32; cnt++)
	{
		NrPadLayerObjectsSelected[cnt] = 0;
		NrRoutingKeepoutsSelected[cnt] = 0;
	}

	NrDrillsSelected = 0;
	NrDrillsUnplatedSelected = 0;
	NrAntiPowerpadsSelected = 0;
	NrPadsInnerSelected = 0;
	NrSilkTopObjectsSelected = 0;
	NrSilkBottomObjectsSelected = 0;
	NrCompOutlinesSelected = 0;
	NrPlacemOutlinesSelected = 0;
	NrMaskTopObjectsSelected = 0;
	NrMaskBottomObjectsSelected = 0;
	NrPasteTopObjectsSelected = 0;
	NrPasteBottomObjectsSelected = 0;
	NrLinesSelected = 0;
	NrRectsSelected = 0;
	NrCirclesSelected = 0;
	NrArcsSelected = 0;
	NrTextsSelected = 0;
	NrPolygonsSelected = 0;
	GeomNameSelected = 0;
	NrBoardOutlinesSelected = 0;
	NrInfo1ObjectsSelected = 0;
	NrInfo2ObjectsSelected = 0;
	NrInfo3ObjectsSelected = 0;
	NrInfo4ObjectsSelected = 0;

	count = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectType = Object->ObjectType;

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Layer = Object->Layer;
			ObjectChanged = 0;
			count++;

			switch (Layer)
			{
			case SILKSCREEN_TOP_LAYER:
				NrSilkTopObjectsSelected++;
				ObjectChanged = 1;
				break;

			case SILKSCREEN_BOTTOM_LAYER:
				NrSilkBottomObjectsSelected++;
				ObjectChanged = 1;
				break;

			case COMP_OUTLINE_LAYER:
				NrCompOutlinesSelected++;
				ObjectChanged = 1;
				break;

			case PLACEMENT_OUTLINE_LAYER:
				NrPlacemOutlinesSelected++;
				ObjectChanged = 1;
				break;

			case SOLD_MASK_TOP_LAYER:
				NrMaskTopObjectsSelected++;
				ObjectChanged = 1;
				break;

			case SOLD_MASK_BOTTOM_LAYER:
				NrMaskBottomObjectsSelected++;
				ObjectChanged = 1;
				break;

			case PASTE_MASK_TOP_LAYER:
				NrPasteTopObjectsSelected++;
				ObjectChanged = 1;
				break;

			case PASTE_MASK_BOTTOM_LAYER:
				NrPasteBottomObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER:
				NrInfo1ObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER2:
				NrInfo2ObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER3:
				NrInfo3ObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER4:
				NrInfo4ObjectsSelected++;
				ObjectChanged = 1;
				break;

			case GEOM_NAME_LAYER:
				GeomNameSelected = 1;
				ObjectChanged = 1;
				break;

			case POWER_PAD_LAYER:
				NrAntiPowerpadsSelected++;
				NrCirclesSelected++;
				ObjectChanged = 1;
				break;

			case INNER_PAD_LAYER:
				if (NrPadLayers == 2)
				{
					NrPadsInnerSelected++;
					ObjectChanged = 1;
				}

				break;

			case DRILL_LAYER:
				NrDrillsSelected++;
				NrCirclesSelected++;
				ObjectChanged = 1;
				break;

			case DRILL_UNPLATED_LAYER:
				NrDrillsUnplatedSelected++;
				NrCirclesSelected++;
				ObjectChanged = 1;
				break;

			case BOARD_OUTLINE_LAYER:
				NrBoardOutlinesSelected++;
				ObjectChanged = 1;
				break;

			default:
				if (Layer < 32)
				{
					NrPadLayerObjectsSelected[Layer]++;
					ObjectChanged = 1;
				}
				else
				{
					if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					{
						NrRoutingKeepoutsSelected[Layer - ROUTING_KEEPOUT_LAYER]++;
						ObjectChanged = 1;
					}
				}

				break;
			}

			switch (ObjectType)
			{
			case OBJECT_LINE:
				NrLinesSelected++;
				break;

			case OBJECT_RECT:
				NrRectsSelected++;
				break;

			case OBJECT_CIRCLE:
				NrCirclesSelected++;
				break;

			case OBJECT_ARC:
				NrArcsSelected++;
				break;

			case OBJECT_TEXT:
				NrTextsSelected++;
				break;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Layer = ObjectPolygon->Layer;
			NrPolygonsSelected++;
			count++;

			switch (Layer)
			{
			case SILKSCREEN_TOP_LAYER:
				NrSilkTopObjectsSelected++;
				ObjectChanged = 1;
				break;

			case SILKSCREEN_BOTTOM_LAYER:
				NrSilkBottomObjectsSelected++;
				ObjectChanged = 1;
				break;

			case COMP_OUTLINE_LAYER:
				NrCompOutlinesSelected++;
				ObjectChanged = 1;
				break;

			case SOLD_MASK_TOP_LAYER:
				NrMaskTopObjectsSelected++;
				ObjectChanged = 1;
				break;

			case SOLD_MASK_BOTTOM_LAYER:
				NrMaskBottomObjectsSelected++;
				ObjectChanged = 1;
				break;

			case PASTE_MASK_TOP_LAYER:
				NrPasteTopObjectsSelected++;
				ObjectChanged = 1;
				break;

			case PASTE_MASK_BOTTOM_LAYER:
				NrPasteBottomObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER:
				NrInfo1ObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER2:
				NrInfo2ObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER3:
				NrInfo3ObjectsSelected++;
				ObjectChanged = 1;
				break;

			case INFO_LAYER4:
				NrInfo4ObjectsSelected++;
				ObjectChanged = 1;
				break;

			default:
				if (Layer < 32)
				{
					NrPadLayerObjectsSelected[Layer]++;
					ObjectChanged = 1;
				}
				else
				{
					if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
					{
						NrRoutingKeepoutsSelected[Layer - ROUTING_KEEPOUT_LAYER]++;
						ObjectChanged = 1;
					}
				}

				break;
			}
		}
	}

	return count;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void ChangeSelections(int32 Modifier)
{
	int32 cnt, Layer, ObjectInfo, ObjectType, ObjectChanged;
	ObjectRecord *Object;
	ObjectPolygonRecord *ObjectPolygon;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ObjectChanged = 0;
			ObjectType = Object->ObjectType;
			Layer = Object->Layer;

			switch (Modifier)
			{
// ***************************************************************************************************
			case ID_SELECT_ONLY_PADS:
			case ID_SELECT_ONLY_PADS_1:
			case ID_SELECT_ONLY_PADS_2:
			case ID_SELECT_ONLY_PADS_3:
			case ID_SELECT_ONLY_PADS_4:
			case ID_SELECT_ONLY_PADS_5:
			case ID_SELECT_ONLY_PADS_6:
			case ID_SELECT_ONLY_PADS_7:
			case ID_SELECT_ONLY_PADS_8:
			case ID_SELECT_ONLY_PADS_9:
			case ID_SELECT_ONLY_PADS_10:
			case ID_SELECT_ONLY_PADS_11:
			case ID_SELECT_ONLY_PADS_12:
			case ID_SELECT_ONLY_PADS_13:
			case ID_SELECT_ONLY_PADS_14:
			case ID_SELECT_ONLY_PADS_15:
			case ID_SELECT_ONLY_PADS_16:
			case ID_SELECT_ONLY_PADS_17:
			case ID_SELECT_ONLY_PADS_18:
			case ID_SELECT_ONLY_PADS_19:
			case ID_SELECT_ONLY_PADS_20:
			case ID_SELECT_ONLY_PADS_21:
			case ID_SELECT_ONLY_PADS_22:
			case ID_SELECT_ONLY_PADS_23:
			case ID_SELECT_ONLY_PADS_24:
			case ID_SELECT_ONLY_PADS_25:
			case ID_SELECT_ONLY_PADS_26:
			case ID_SELECT_ONLY_PADS_27:
			case ID_SELECT_ONLY_PADS_28:
			case ID_SELECT_ONLY_PADS_29:
			case ID_SELECT_ONLY_PADS_30:
			case ID_SELECT_ONLY_PADS_31:
				if (Layer != Modifier - ID_SELECT_ONLY_PADS)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_ANTI_POWERPADS:
				if (Layer != POWER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INNERPADS:
				if (Layer != INNER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO1:
				if (Layer != INFO_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO2:
				if (Layer != INFO_LAYER2)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO3:
				if (Layer != INFO_LAYER3)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO4:
				if (Layer != INFO_LAYER4)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_DRILL:
				if (Layer != DRILL_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_DRILL_UNPLATED:
				if (Layer != DRILL_UNPLATED_LAYER)
					ObjectChanged = 1;

				break;

			/*
			        case ID_SELECT_ONLY_BUTTERFLY:
			          if (ObjectType!=PIN_BUTTERFLY) ObjectChanged=1;
			          break;
			*/
			case ID_SELECT_ONLY_SILK_TOP:
				if (Layer != SILKSCREEN_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_SILK_BOTTOM:
				if (Layer != SILKSCREEN_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_COMP_OUTLINE:
				if (Layer != COMP_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_PLACEM_OUTLINE:
				if (Layer != PLACEMENT_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_MASK_TOP:
				if (Layer != SOLD_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_MASK_BOTTOM:
				if (Layer != SOLD_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_PASTE_TOP:
				if (Layer != PASTE_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_PASTE_BOTTOM:
				if (Layer != PASTE_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_LINES:
				if (ObjectType != OBJECT_LINE)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_RECTS:
				if (ObjectType != OBJECT_RECT)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_CIRCLES:
				if (ObjectType != OBJECT_CIRCLE)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_ARCS:
				if (ObjectType != OBJECT_ARC)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_TEXT:
				if (ObjectType != OBJECT_TEXT)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_ROUT_KEEPOUT:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_1:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_2:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_3:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_4:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_5:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_6:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_7:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_8:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_9:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_10:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_11:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_12:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_13:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_14:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_15:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_16:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_17:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_18:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_19:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_20:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_21:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_22:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_23:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_24:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_25:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_26:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_27:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_28:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_29:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_30:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_31:
				if (Layer != Modifier - ID_SELECT_ONLY_ROUT_KEEPOUT + ROUTING_KEEPOUT_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_BOARD_OUTLINE:
				if (Layer != BOARD_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

// ***************************************************************************************************
			case ID_UNSELECT_PADS:
			case ID_UNSELECT_PADS_1:
			case ID_UNSELECT_PADS_2:
			case ID_UNSELECT_PADS_3:
			case ID_UNSELECT_PADS_4:
			case ID_UNSELECT_PADS_5:
			case ID_UNSELECT_PADS_6:
			case ID_UNSELECT_PADS_7:
			case ID_UNSELECT_PADS_8:
			case ID_UNSELECT_PADS_9:
			case ID_UNSELECT_PADS_10:
			case ID_UNSELECT_PADS_11:
			case ID_UNSELECT_PADS_12:
			case ID_UNSELECT_PADS_13:
			case ID_UNSELECT_PADS_14:
			case ID_UNSELECT_PADS_15:
			case ID_UNSELECT_PADS_16:
			case ID_UNSELECT_PADS_17:
			case ID_UNSELECT_PADS_18:
			case ID_UNSELECT_PADS_19:
			case ID_UNSELECT_PADS_20:
			case ID_UNSELECT_PADS_21:
			case ID_UNSELECT_PADS_22:
			case ID_UNSELECT_PADS_23:
			case ID_UNSELECT_PADS_24:
			case ID_UNSELECT_PADS_25:
			case ID_UNSELECT_PADS_26:
			case ID_UNSELECT_PADS_27:
			case ID_UNSELECT_PADS_28:
			case ID_UNSELECT_PADS_29:
			case ID_UNSELECT_PADS_30:
			case ID_UNSELECT_PADS_31:
				if (Layer == Modifier - ID_UNSELECT_PADS)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_ANTI_POWERPADS:
				if (Layer == POWER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INNER_PADS:
				if (Layer == INNER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO1:
				if (Layer == INFO_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO2:
				if (Layer == INFO_LAYER2)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO3:
				if (Layer == INFO_LAYER3)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO4:
				if (Layer == INFO_LAYER4)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_DRILL:
				if (Layer == DRILL_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_DRILL_UNPLATED:
				if (Layer == DRILL_UNPLATED_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_SILK_TOP:
				if (Layer == SILKSCREEN_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_SILK_BOTTOM:
				if (Layer == SILKSCREEN_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_COMP_OUTLINE:
				if (Layer == COMP_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_PLACEM_OUTLINE:
				if (Layer == PLACEMENT_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_MASK_TOP:
				if (Layer == SOLD_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_MASK_BOTTOM:
				if (Layer == SOLD_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_PASTE_TOP:
				if (Layer == PASTE_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_PASTE_BOTTOM:
				if (Layer == PASTE_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_LINES:
				if (ObjectType == OBJECT_LINE)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_RECTS:
				if (ObjectType == OBJECT_RECT)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_CIRCLES:
				if (ObjectType == OBJECT_CIRCLE)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_ARCS:
				if (ObjectType == OBJECT_ARC)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_TEXT:
				if (ObjectType == OBJECT_TEXT)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_ROUT_KEEPOUT:
			case ID_UNSELECT_ROUT_KEEPOUT_1:
			case ID_UNSELECT_ROUT_KEEPOUT_2:
			case ID_UNSELECT_ROUT_KEEPOUT_3:
			case ID_UNSELECT_ROUT_KEEPOUT_4:
			case ID_UNSELECT_ROUT_KEEPOUT_5:
			case ID_UNSELECT_ROUT_KEEPOUT_6:
			case ID_UNSELECT_ROUT_KEEPOUT_7:
			case ID_UNSELECT_ROUT_KEEPOUT_8:
			case ID_UNSELECT_ROUT_KEEPOUT_9:
			case ID_UNSELECT_ROUT_KEEPOUT_10:
			case ID_UNSELECT_ROUT_KEEPOUT_11:
			case ID_UNSELECT_ROUT_KEEPOUT_12:
			case ID_UNSELECT_ROUT_KEEPOUT_13:
			case ID_UNSELECT_ROUT_KEEPOUT_14:
			case ID_UNSELECT_ROUT_KEEPOUT_15:
			case ID_UNSELECT_ROUT_KEEPOUT_16:
			case ID_UNSELECT_ROUT_KEEPOUT_17:
			case ID_UNSELECT_ROUT_KEEPOUT_18:
			case ID_UNSELECT_ROUT_KEEPOUT_19:
			case ID_UNSELECT_ROUT_KEEPOUT_20:
			case ID_UNSELECT_ROUT_KEEPOUT_21:
			case ID_UNSELECT_ROUT_KEEPOUT_22:
			case ID_UNSELECT_ROUT_KEEPOUT_23:
			case ID_UNSELECT_ROUT_KEEPOUT_24:
			case ID_UNSELECT_ROUT_KEEPOUT_25:
			case ID_UNSELECT_ROUT_KEEPOUT_26:
			case ID_UNSELECT_ROUT_KEEPOUT_27:
			case ID_UNSELECT_ROUT_KEEPOUT_28:
			case ID_UNSELECT_ROUT_KEEPOUT_29:
			case ID_UNSELECT_ROUT_KEEPOUT_30:
			case ID_UNSELECT_ROUT_KEEPOUT_31:
				if (Layer != Modifier - ID_UNSELECT_ROUT_KEEPOUT + ROUTING_KEEPOUT_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_BOARD_OUTLINE:
				if (Layer == BOARD_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;
// ***************************************************************************************************
			}

			if (ObjectChanged)
			{
				ObjectInfo &= ~(OBJECT_SELECTED | 3);
				Object->Info = (int16) ObjectInfo;
			}
		}
	}

// ***************************************************************************************************
// ***************************************************************************************************
	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			ObjectChanged = 0;
			Layer = ObjectPolygon->Layer;

			switch (Modifier)
			{
// ***************************************************************************************************
			case ID_SELECT_ONLY_PADS:
			case ID_SELECT_ONLY_PADS_1:
			case ID_SELECT_ONLY_PADS_2:
			case ID_SELECT_ONLY_PADS_3:
			case ID_SELECT_ONLY_PADS_4:
			case ID_SELECT_ONLY_PADS_5:
			case ID_SELECT_ONLY_PADS_6:
			case ID_SELECT_ONLY_PADS_7:
			case ID_SELECT_ONLY_PADS_8:
			case ID_SELECT_ONLY_PADS_9:
			case ID_SELECT_ONLY_PADS_10:
			case ID_SELECT_ONLY_PADS_11:
			case ID_SELECT_ONLY_PADS_12:
			case ID_SELECT_ONLY_PADS_13:
			case ID_SELECT_ONLY_PADS_14:
			case ID_SELECT_ONLY_PADS_15:
			case ID_SELECT_ONLY_PADS_16:
			case ID_SELECT_ONLY_PADS_17:
			case ID_SELECT_ONLY_PADS_18:
			case ID_SELECT_ONLY_PADS_19:
			case ID_SELECT_ONLY_PADS_20:
			case ID_SELECT_ONLY_PADS_21:
			case ID_SELECT_ONLY_PADS_22:
			case ID_SELECT_ONLY_PADS_23:
			case ID_SELECT_ONLY_PADS_24:
			case ID_SELECT_ONLY_PADS_25:
			case ID_SELECT_ONLY_PADS_26:
			case ID_SELECT_ONLY_PADS_27:
			case ID_SELECT_ONLY_PADS_28:
			case ID_SELECT_ONLY_PADS_29:
			case ID_SELECT_ONLY_PADS_30:
			case ID_SELECT_ONLY_PADS_31:
				if (Layer != Modifier - ID_SELECT_ONLY_PADS)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_ANTI_POWERPADS:
				if (Layer != POWER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INNERPADS:
				if (Layer != INNER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO1:
				if (Layer != INFO_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO2:
				if (Layer != INFO_LAYER2)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO3:
				if (Layer != INFO_LAYER3)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_INFO4:
				if (Layer != INFO_LAYER4)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_DRILL:
				if (Layer != DRILL_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_DRILL_UNPLATED:
				if (Layer != DRILL_UNPLATED_LAYER)
					ObjectChanged = 1;

				break;

			/*
			        case ID_SELECT_ONLY_BUTTERFLY:
			          if (ObjectType!=PIN_BUTTERFLY) ObjectChanged=1;
			          break;
			*/
			case ID_SELECT_ONLY_SILK_TOP:
				if (Layer != SILKSCREEN_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_SILK_BOTTOM:
				if (Layer != SILKSCREEN_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_COMP_OUTLINE:
				if (Layer != COMP_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_PLACEM_OUTLINE:
				if (Layer != PLACEMENT_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_MASK_TOP:
				if (Layer != SOLD_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_MASK_BOTTOM:
				if (Layer != SOLD_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_PASTE_TOP:
				if (Layer != PASTE_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_PASTE_BOTTOM:
				if (Layer != PASTE_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_ROUT_KEEPOUT:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_1:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_2:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_3:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_4:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_5:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_6:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_7:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_8:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_9:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_10:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_11:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_12:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_13:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_14:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_15:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_16:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_17:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_18:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_19:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_20:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_21:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_22:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_23:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_24:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_25:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_26:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_27:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_28:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_29:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_30:
			case ID_SELECT_ONLY_ROUT_KEEPOUT_31:
				if (Layer != Modifier - ID_SELECT_ONLY_ROUT_KEEPOUT + ROUTING_KEEPOUT_LAYER)
					ObjectChanged = 1;

				break;

			case ID_SELECT_ONLY_BOARD_OUTLINE:
				if (Layer != BOARD_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

// ***************************************************************************************************
			case ID_UNSELECT_PADS:
			case ID_UNSELECT_PADS_1:
			case ID_UNSELECT_PADS_2:
			case ID_UNSELECT_PADS_3:
			case ID_UNSELECT_PADS_4:
			case ID_UNSELECT_PADS_5:
			case ID_UNSELECT_PADS_6:
			case ID_UNSELECT_PADS_7:
			case ID_UNSELECT_PADS_8:
			case ID_UNSELECT_PADS_9:
			case ID_UNSELECT_PADS_10:
			case ID_UNSELECT_PADS_11:
			case ID_UNSELECT_PADS_12:
			case ID_UNSELECT_PADS_13:
			case ID_UNSELECT_PADS_14:
			case ID_UNSELECT_PADS_15:
			case ID_UNSELECT_PADS_16:
			case ID_UNSELECT_PADS_17:
			case ID_UNSELECT_PADS_18:
			case ID_UNSELECT_PADS_19:
			case ID_UNSELECT_PADS_20:
			case ID_UNSELECT_PADS_21:
			case ID_UNSELECT_PADS_22:
			case ID_UNSELECT_PADS_23:
			case ID_UNSELECT_PADS_24:
			case ID_UNSELECT_PADS_25:
			case ID_UNSELECT_PADS_26:
			case ID_UNSELECT_PADS_27:
			case ID_UNSELECT_PADS_28:
			case ID_UNSELECT_PADS_29:
			case ID_UNSELECT_PADS_30:
			case ID_UNSELECT_PADS_31:
				if (Layer == Modifier - ID_UNSELECT_PADS)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_ANTI_POWERPADS:
				if (Layer == POWER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INNER_PADS:
				if (Layer == INNER_PAD_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO1:
				if (Layer == INFO_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO2:
				if (Layer == INFO_LAYER2)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO3:
				if (Layer == INFO_LAYER3)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_INFO4:
				if (Layer == INFO_LAYER4)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_DRILL:
				if (Layer == DRILL_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_DRILL_UNPLATED:
				if (Layer == DRILL_UNPLATED_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_SILK_TOP:
				if (Layer == SILKSCREEN_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_SILK_BOTTOM:
				if (Layer == SILKSCREEN_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_COMP_OUTLINE:
				if (Layer == COMP_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_PLACEM_OUTLINE:
				if (Layer == PLACEMENT_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_MASK_TOP:
				if (Layer == SOLD_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_MASK_BOTTOM:
				if (Layer == SOLD_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_PASTE_TOP:
				if (Layer == PASTE_MASK_TOP_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_PASTE_BOTTOM:
				if (Layer == PASTE_MASK_BOTTOM_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_ROUT_KEEPOUT:
			case ID_UNSELECT_ROUT_KEEPOUT_1:
			case ID_UNSELECT_ROUT_KEEPOUT_2:
			case ID_UNSELECT_ROUT_KEEPOUT_3:
			case ID_UNSELECT_ROUT_KEEPOUT_4:
			case ID_UNSELECT_ROUT_KEEPOUT_5:
			case ID_UNSELECT_ROUT_KEEPOUT_6:
			case ID_UNSELECT_ROUT_KEEPOUT_7:
			case ID_UNSELECT_ROUT_KEEPOUT_8:
			case ID_UNSELECT_ROUT_KEEPOUT_9:
			case ID_UNSELECT_ROUT_KEEPOUT_10:
			case ID_UNSELECT_ROUT_KEEPOUT_11:
			case ID_UNSELECT_ROUT_KEEPOUT_12:
			case ID_UNSELECT_ROUT_KEEPOUT_13:
			case ID_UNSELECT_ROUT_KEEPOUT_14:
			case ID_UNSELECT_ROUT_KEEPOUT_15:
			case ID_UNSELECT_ROUT_KEEPOUT_16:
			case ID_UNSELECT_ROUT_KEEPOUT_17:
			case ID_UNSELECT_ROUT_KEEPOUT_18:
			case ID_UNSELECT_ROUT_KEEPOUT_19:
			case ID_UNSELECT_ROUT_KEEPOUT_20:
			case ID_UNSELECT_ROUT_KEEPOUT_21:
			case ID_UNSELECT_ROUT_KEEPOUT_22:
			case ID_UNSELECT_ROUT_KEEPOUT_23:
			case ID_UNSELECT_ROUT_KEEPOUT_24:
			case ID_UNSELECT_ROUT_KEEPOUT_25:
			case ID_UNSELECT_ROUT_KEEPOUT_26:
			case ID_UNSELECT_ROUT_KEEPOUT_27:
			case ID_UNSELECT_ROUT_KEEPOUT_28:
			case ID_UNSELECT_ROUT_KEEPOUT_29:
			case ID_UNSELECT_ROUT_KEEPOUT_30:
			case ID_UNSELECT_ROUT_KEEPOUT_31:
				if (Layer != Modifier - ID_UNSELECT_ROUT_KEEPOUT + ROUTING_KEEPOUT_LAYER)
					ObjectChanged = 1;

				break;

			case ID_UNSELECT_BOARD_OUTLINE:
				if (Layer == BOARD_OUTLINE_LAYER)
					ObjectChanged = 1;

				break;
// ***************************************************************************************************
			}

			if (ObjectChanged)
				ObjectPolygon->Info &= ~(OBJECT_SELECTED | 3);
		}
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 AddToMessageBuf(LPSTR Line)
{
	int32 lengte;

	lengte = strlen(Line);

	if (lengte + 2 + MessageBufPos >= MessageBufMemSize)
	{
		if (AllocateMemMessageBuf(MessageBufMemSize * 2) != 0)
			return -1;
	}

	if (MessageBufPos == 0)
		MessageBuf[0] = 0;

	if (lengte > 0)
		strcat((LPSTR) MessageBuf, Line);

	strcat((LPSTR) MessageBuf, "\r\n");
	MessageBufPos = strlen((LPSTR) MessageBuf);
	return 0;

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetCircleAngleString(int32 CircleInfo2, LPSTR String)
{
	strcpy(String, "");

	switch (CircleInfo2)
	{
	case 1:
		strcpy(String, "( 0.0, 45.0 )");
		break;

	case 2:
		strcpy(String, "( 270.0, 0.0 )");
		break;

	case 3:
		strcpy(String, "( 270.0, 90.0 )");
		break;

	case 4:
		strcpy(String, "( 180.0, 270.0 )");
		break;

	case 6:
		strcpy(String, "( 180.0, 0.0 )");
		break;

	case 8:
		strcpy(String, "( 90.0, 180.0 )");
		break;

	case 9:
		strcpy(String, "( 0.0, 180.0 )");
		break;

	case 12:
		strcpy(String, "( 90.0, 270.0 )");
		break;
	}

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void ObjectsInfo()
{
	int32 cnt, PinNr, count, ObjectInfo, Layer, PinObject;
	double x1, y1, x2, x2a, y2, y2a, dikte, cl, Rotation, Dist1, Dist2, Angle1, Angle2, lengte;
	PinInfoRecord *PinInfo;
	ObjectRecord *Object;
	char str[MAX_LENGTH_STRING], PinStr[MAX_LENGTH_STRING], Val1[MAX_LENGTH_STRING], Val2[MAX_LENGTH_STRING],
	     Val2a[MAX_LENGTH_STRING], Val2b[MAX_LENGTH_STRING], Val3[MAX_LENGTH_STRING], Val4[MAX_LENGTH_STRING],
	     Val4a[MAX_LENGTH_STRING], Val6[MAX_LENGTH_STRING], Val6a[MAX_LENGTH_STRING], Val7[MAX_LENGTH_STRING],
	     Val8[MAX_LENGTH_STRING], CircleAngleString[MAX_LENGTH_STRING], LayerStr[MAX_LENGTH_STRING],
	     ObjectStr[MAX_LENGTH_STRING];
	ObjectPolygonRecord *ObjectPolygon;

	Dist1 = 0.0;
	Dist2 = 0.0;
	Angle2 = 0.0;
	MessageBufPos = 0;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);
		ObjectInfo = Object->Info;

		if ((ObjectInfo & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			PinObject = 0;
			Layer = Object->Layer;
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;
			lengte = sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
			ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle1, &lengte);
			Angle1 *= 180 / PI;
			x2a = Object->x2;
			y2a = Object->y2;
			Rotation = Object->RotationAngle;

			if (Rotation > 1000.0)
				Rotation -= 2000.0;

			str[0] = 0;
			dikte = Object->Thickness;

			if (Object->ObjectType == OBJECT_ARC)
			{
				ConvNormalCoorToPolar(0.0, 0.0, Object->x3, Object->y3, &Angle1, &Dist1);
				Angle1 *= 180 / PI;
				ConvNormalCoorToPolar(0.0, 0.0, Object->x4, Object->y4, &Angle2, &Dist2);
				Angle2 *= 180 / PI;
			}

			cl = Object->Clearance;
			PinNr = Object->PinNr;
			PinStr[0] = 0;

			if (PinNr != -1)
			{
				PinInfo = &((*PinInfos)[PinNr]);
				strcpy(PinStr, PinInfo->PinText);
			}

			memset(&Val1, 0, sizeof(Val1));
			memset(&Val2, 0, sizeof(Val2));
			memset(&Val3, 0, sizeof(Val3));
			memset(&Val4, 0, sizeof(Val4));
			memset(&Val6, 0, sizeof(Val6));
			memset(&Val7, 0, sizeof(Val7));
			memset(&Val8, 0, sizeof(Val8));
			memset(&Val2a, 0, sizeof(Val2a));
			memset(&Val2b, 0, sizeof(Val2b));
			memset(&Val4a, 0, sizeof(Val4a));
			memset(&Val6a, 0, sizeof(Val6a));
			x1 = ConvertUnits(x1, Units);
			y1 = ConvertUnits(y1, Units);
			x2 = ConvertUnits(x2, Units);
			y2 = ConvertUnits(y2, Units);
			x2a = ConvertUnits(x2a, Units);
			y2a = ConvertUnits(y2a, Units);
			cl = ConvertUnits(cl, Units);

			if (Layer == PLACEMENT_OUTLINE_LAYER)
				dikte = 0.0;

			dikte = ConvertUnits(dikte, Units);
			Dist1 = ConvertUnits(Dist1, Units);
			Dist2 = ConvertUnits(Dist2, Units);
			lengte = ConvertUnits(lengte, Units);

			if (Units == 0)
			{
				if (PinStr[0] == 0)
					sprintf(Val1, "%.2f,%.2f\t%.2f,%.2f", x1, y1, x2, y2);
				else
					sprintf(Val1, "%.2f,%.2f\t%.2f,%.2f\t\t%.2f", x1, y1, x2, y2, cl);

				sprintf(Val2, "%.2f,%.2f\t%.2f\t\t%.2f", x1, y1, x2, cl);
				sprintf(Val2a, SC(411, "%.2f,%.2f\t%.2f (%.2f mm)\t\t%.2f"), x1, y1, x2, x2 / 39.37, cl);

				if (Object->Info2 == 0)
					sprintf(Val2b, "%.2f,%.2f\t%.2f\t\t%.2f", x1, y1, x2, dikte);
				else
				{
					GetCircleAngleString(Object->Info2, (LPSTR) & CircleAngleString);
					sprintf(Val2b, "%.2f,%.2f\t%.2f %s\t%.2f", x1, y1, x2, CircleAngleString, dikte);
				}

				sprintf(Val3, "%.2f,%.2f\t%.2f,%.2f\t%.2f", x1, y1, x2, y2, dikte);

				if (PinStr[0] == 0)
					sprintf(Val4, "%.2f,%.2f\t%.2f", x1, y1, x2);
				else
					sprintf(Val4, "%.2f,%.2f\t%.2f\t\t%.2f", x1, y1, x2, cl);

				sprintf(Val4a, SC(412, "%.2f,%.2f\t%.2f ( Rotation %.2f )\t%.2f"), x1, y1, x2, Rotation, dikte);
				sprintf(Val6, "%.2f,%.2f\t%.2f,%.2f\t%.2f", x1, y1, x2, y2, dikte);

				if (PinStr[0] == 0)
				{
					sprintf(Val7, SC(413, "%.2f,%.2f - %.2f,%.2f\tLength %.2f ( Angle %.2f )\t%.2f"), x1, y1, x2, y2,
					        lengte, Angle1, dikte);
					sprintf(Val8, "%.2f,%.2f\t%.2f,%.2f ( %.2f,%.2f )\t%.2f", x1, y1, x2, y2, Angle1, Angle2, dikte);
				}
				else
				{
					sprintf(Val7, SC(414, "%.2f,%.2f - %.2f,%.2f\tLength %.2f ( Angle %.2f )\t%.2f\t%.2f"), x1, y1, x2,
					        y2, lengte, Angle1, dikte, cl);
					sprintf(Val8, "%.2f,%.2f\t%.2f,%.2f ( %.2f,%.2f )\t%.2f\t%.2f", x1, y1, x2, y2, Angle1, Angle2,
					        dikte, cl);
				}
			}
			else
			{
				if (PinStr[0] == 0)
					sprintf(Val1, "%.4f,%.4f\t%.4f,%.4f", x1, y1, x2, y2);
				else
					sprintf(Val1, "%.4f,%.4f\t%.4f,%.4f\t\t%.4f", x1, y1, x2, y2, cl);

				sprintf(Val2, "%.4f,%.4f\t%.4f\t\t%.4f", x1, y1, x2, cl);
				sprintf(Val2a, "%.4f,%.4f\t%.4f\t\t%.4f", x1, y1, x2, cl);

				if (Object->Info2 == 0)
					sprintf(Val2b, "%.4f,%.4f\t%.4f\t\t%.4f", x1, y1, x2, dikte);
				else
				{
					GetCircleAngleString(Object->Info2, (LPSTR) & CircleAngleString);
					sprintf(Val2b, "%.4f,%.4f\t%.4f %s\t%.4f", x1, y1, x2, CircleAngleString, dikte);
				}

				sprintf(Val3, "%.4f,%.4f\t%.4f,%.4f\t%.4f", x1, y1, x2, y2, dikte);

				if (PinStr[0] == 0)
					sprintf(Val4, "%.4f,%.4f\t%.4f", x1, y1, x2);
				else
					sprintf(Val4, "%.4f,%.4f\t%.4f\t\t%.4f", x1, y1, x2, cl);

				sprintf(Val4a, SC(415, "%.4f,%.4f\t%.4f ( Rotation %.2f )\t%.4f"), x1, y1, x2, Rotation, dikte);
				sprintf(Val6, "%.4f,%.4f\t%.4f,%.4f\t%.4f", x1, y1, x2, y2, dikte);

				if (PinStr[0] == 0)
				{
					sprintf(Val7, SC(416, "%.4f,%.4f - %.4f,%.4f\tLength %.4f ( Angle %.2f )\t%.4f\t%.4f"), x1, y1, x2,
					        y2, lengte, Angle1, dikte, cl);
					sprintf(Val8, "%.4f,%.4f\t%.4f,%.4f ( %.2f,%.2f )\t%.4f", x1, y1, x2, y2, Angle1, Angle2, dikte);
				}
				else
				{
					sprintf(Val7, SC(417, "%.4f,%.4f - %.4f,%.4f\tLength %.4f ( Angle %.2f )\t%.4f"), x1, y1, x2, y2,
					        lengte, Angle1, dikte);
					sprintf(Val8, "%.4f,%.4f\t%.4f,%.4f ( %.2f,%.2f )\t%.4f\t%.4f", x1, y1, x2, y2, Angle1, Angle2,
					        dikte, cl);
				}
			}

			GetLayerText(Layer, LayerStr, 0);
			str[0] = 0;

			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
				strcpy(ObjectStr, SC(418, "Line"));
				sprintf(str, "%s\t%s\t%s\t%s", ObjectStr, LayerStr, PinStr, Val7);
				break;

			case OBJECT_RECT:
				if ((dikte == 0.0) && (Layer != PLACEMENT_OUTLINE_LAYER))
				{
					strcpy(ObjectStr, SC(419, "Rect pad"));
					sprintf(str, "%s\t%s\t%s\t%s", ObjectStr, LayerStr, PinStr, Val1);
				}
				else
				{
					strcpy(ObjectStr, SC(420, "Rectangle"));
					sprintf(str, "%s\t%s\t\t%s", ObjectStr, LayerStr, Val3);
				}

				break;

			case OBJECT_CIRCLE:
				if ((dikte == 0.0) && (Object->Info2 == 0))
				{
					strcpy(ObjectStr, SC(421, "Circle pad"));
					sprintf(str, "%s\t%s\t%s\t%s", ObjectStr, LayerStr, PinStr, Val4);
				}
				else
				{
					strcpy(ObjectStr, SC(422, "Circle arc"));
					sprintf(str, "%s\t%s\t\t%s", ObjectStr, LayerStr, Val2b);
				}

				break;

			case OBJECT_ARC:
				strcpy(ObjectStr, SC(423, "Arc"));
				sprintf(str, "%s\t%s\t%s\t%s", ObjectStr, LayerStr, PinStr, Val8);
				break;

			case OBJECT_TEXT:
				if (Object->Layer != GEOM_NAME_LAYER)
				{
					strcpy(ObjectStr, SC(424, "Text"));
					sprintf(str, "%s\t%s\t\t%s", ObjectStr, LayerStr, Val4a);
				}
				else
				{
					strcpy(ObjectStr, SC(97, "Geometry name"));
					sprintf(str, "%s\t\t\t%s", ObjectStr, Val4a);
				}

				break;
				/*
				        case DRILL:
				          strcpy(ObjectStr,"DRILL PLATED");
				          sprintf(str,"%s\t%s\t%s\t%s",ObjectStr,LayerStr,PinStr,Val2a);
				          break;
				        case DRILL_UNPLATED:
				          strcpy(ObjectStr,"DRILL UNPLATED");
				          sprintf(str,"%s\t%s\t\t%s",ObjectStr,LayerStr,Val2a);
				          break;
				        case PIN_PUT_THROUGH_ROUND_POWER:
				          strcpy(ObjectStr,"PAD POWER");
				          sprintf(str,"%s\t%s\t%s\t%s",ObjectStr,LayerStr,PinStr,Val4);
				          break;
				        case PIN_PUT_THROUGH_ROUND_INNER_PAD:
				          strcpy(ObjectStr,"PAD INNER");
				          sprintf(str,"%s\t%s\t%s\t%s",ObjectStr,LayerStr,PinStr,Val2);
				          break;
				*/
			}

			if (str[0] != 0)
			{
				if (AddToMessageBuf(str) != 0)
					return;
			}
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			Layer = ObjectPolygon->Layer;
			count = ObjectPolygon->NrVertices;

			if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
				count = ObjectPolygon->NrVerticesMainPolygon;

			PinNr = ObjectPolygon->PinNr;
			cl = ObjectPolygon->Clearance;
			PinStr[0] = 0;

			if (PinNr != -1)
			{
				PinInfo = &((*PinInfos)[PinNr]);
				strcpy(PinStr, PinInfo->PinText);
			}

			GetLayerText(Layer, LayerStr, 0);
			x1 = ((ObjectPolygon->minx + ObjectPolygon->maxx) * 0.5);
			y1 = ((ObjectPolygon->miny + ObjectPolygon->maxy) * 0.5);
			x1 = ConvertUnits(x1, Units);
			y1 = ConvertUnits(y1, Units);
			cl = ConvertUnits(cl, Units);

			if (Units == 0)
			{
				sprintf(Val1, "%.2f,%.2f", x1, y1);
				sprintf(Val2, "%.2f", cl);
			}
			else
			{
				sprintf(Val1, "%.4f,%.4f", x1, y1);
				sprintf(Val2, "%.4f", cl);
			}

			if (PinStr[0] == 0)
				sprintf(str, SC(425, "Polygon\t%s\t%s\t%s\t%d vertices"), LayerStr, PinStr, Val1, count);
			else
			{
#ifdef _DEBUG
				sprintf(str, SC(426, "Polygon\t%s\t%s\t%s\t%d vertices (%d)\t\t%s"), LayerStr, PinStr, Val1, count,
				        ObjectPolygon->NrSubPolygons, Val2);
#else
				sprintf(str, SC(426, "Polygon\t%s\t%s\t%s\t%d vertices\t\t%s"), LayerStr, PinStr, Val1, count, Val2);
#endif
			}

			if (AddToMessageBuf(str) != 0)
				return;
		}
	}

	if (MessageBufPos == 0)
		return;

	if (Units == 0)
		MessageDialog("OBJECT TYPE\tLAYER\tPIN NAME\tORIGIN (thou)\tSIZE (thou)\tTHICKN\tCLEAR", 0);
	else
		MessageDialog(SC(428, "OBJECT TYPE\tLAYER\tPIN NAME\tORIGIN (mm)\tSIZE (mm)\tTHICKN\tCLEAR"), 0);

	DeAllocateMemMessageBuf();
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void GetMinMaxSelectedObjects()
{
//  ObjectRecord *Object,*Object2;
	int32 cnt;
	double x1, y1, x2, y2, Thickness;

	ObjectRecord *Object;

	SelectedMinX = 10000.0e5;
	SelectedMinY = 10000.0e5;
	SelectedMaxX = -10000.0e5;
	SelectedMaxY = -10000.0e5;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;
			Thickness = Object->Thickness * 0.5;

			switch (Object->ObjectType)
			{
			case OBJECT_LINE:
				SelectedMinX = min(SelectedMinX, x1 - Thickness);
				SelectedMaxX = max(SelectedMaxX, x1 + Thickness);
				SelectedMinX = min(SelectedMinX, x2 - Thickness);
				SelectedMaxX = max(SelectedMaxX, x2 + Thickness);
				SelectedMinY = min(SelectedMinY, y1 - Thickness);
				SelectedMaxY = max(SelectedMaxY, y1 + Thickness);
				SelectedMinY = min(SelectedMinY, y2 - Thickness);
				SelectedMaxY = max(SelectedMaxY, y2 + Thickness);
				break;

			case OBJECT_RECT:
				SelectedMinX = min(SelectedMinX, x1 - x2 * 0.5 - Thickness);
				SelectedMaxX = max(SelectedMaxX, x1 + x2 * 0.5 + Thickness);
				SelectedMinY = min(SelectedMinY, y1 - y2 * 0.5 - Thickness);
				SelectedMaxY = max(SelectedMaxY, y1 + y2 * 0.5 + Thickness);
				break;

			case OBJECT_CIRCLE:
			case OBJECT_ARC:
				SelectedMinX = min(SelectedMinX, x1 - x2 * 0.5 - Thickness);
				SelectedMaxX = max(SelectedMaxX, x1 + x2 * 0.5 + Thickness);
				SelectedMinY = min(SelectedMinY, y1 - x2 * 0.5 - Thickness);
				SelectedMaxY = max(SelectedMaxY, y1 + x2 * 0.5 + Thickness);
				break;

			case OBJECT_TEXT:
				if (Object->RotationAngle > 1000.0)
					GetMinMaxText(x1, y1, x2, 0, Object->RotationAngle - 2000.0, 0, 1, Object->Text);
				else
					GetMinMaxText(x1, y1, x2, 0, Object->RotationAngle, 0, 0, Object->Text);

				SelectedMinX = min(SelectedMinX, TextMinX - Thickness);
				SelectedMaxX = max(SelectedMaxX, TextMaxX + Thickness);
				SelectedMinY = min(SelectedMinY, TextMinY - Thickness);
				SelectedMaxY = max(SelectedMaxY, TextMaxY + Thickness);
				break;
			}
		}
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetObjectSize(ObjectRecord * Object, double *MinX, double *MinY, double *MaxX, double *MaxY)
{
	double x1, y1, x2, y2, Thickness;
	boolean Changed = 0;

	x1 = Object->x1;
	y1 = Object->y1;
	x2 = Object->x2;
	y2 = Object->y2;
	Thickness = Object->Thickness * 0.5;

	switch (Object->ObjectType)
	{
	case OBJECT_LINE:
		*MinX = min(*MinX, x1 - Thickness);
		*MaxX = max(*MaxX, x1 + Thickness);
		*MinX = min(*MinX, x2 - Thickness);
		*MaxX = max(*MaxX, x2 + Thickness);
		*MinY = min(*MinY, y1 - Thickness);
		*MaxY = max(*MaxY, y1 + Thickness);
		*MinY = min(*MinY, y2 - Thickness);
		*MaxY = max(*MaxY, y2 + Thickness);
		Changed = 1;
		break;

	case OBJECT_RECT:
		*MinX = min(*MinX, x1 - x2 * 0.5 - Thickness);
		*MaxX = max(*MaxX, x1 + x2 * 0.5 + Thickness);
		*MinY = min(*MinY, y1 - y2 * 0.5 - Thickness);
		*MaxY = max(*MaxY, y1 + y2 * 0.5 + Thickness);
		Changed = 1;
		break;

	case OBJECT_CIRCLE:
	case OBJECT_ARC:
		*MinX = min(*MinX, x1 - x2 * 0.5 - Thickness);
		*MaxX = max(*MaxX, x1 + x2 * 0.5 + Thickness);
		*MinY = min(*MinY, y1 - x2 * 0.5 - Thickness);
		*MaxY = max(*MaxY, y1 + x2 * 0.5 + Thickness);
		Changed = 1;
		break;

	case OBJECT_TEXT:
		if (Object->RotationAngle > 1000.0)
			GetMinMaxText(x1, y1, x2, 0, Object->RotationAngle - 2000.0, 0, 1, Object->Text);
		else
			GetMinMaxText(x1, y1, x2, 0, Object->RotationAngle, 0, 0, Object->Text);
		
		*MinX = min(*MinX, TextMinX - Thickness);
		*MaxX = max(*MaxX, TextMaxX + Thickness);
		*MinY = min(*MinY, TextMinY - Thickness);
		*MaxY = max(*MaxY, TextMaxY + Thickness);
		Changed = 1;
		break;
	}

	return Changed;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void FindMinMaxBoard(double *OriginX, double *OriginY, double *Width, double *Height, int32 mode)
{
	int32 cnt;
	double MinX, MaxX, MinY, MaxY;
	ObjectPolygonRecord *ObjectPolygon;
	boolean Changed = 0;

	ObjectRecord *Object;

	MinX = 10000.0e5;
	MinY = 10000.0e5;
	MaxX = -10000.0e5;
	MaxY = -10000.0e5;

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if ((Object->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (GetObjectSize(Object, &MinX, &MinY, &MaxX, &MaxY))
				Changed = 1;
		}
	}

	for (cnt = 0; cnt < NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			MinX = min(MinX, ObjectPolygon->minx);
			MaxX = max(MaxX, ObjectPolygon->maxx);
			MinY = min(MinY, ObjectPolygon->miny);
			MaxY = max(MaxY, ObjectPolygon->maxy);
		}
	}

	if (!Changed)
	{
		MinX = -15.0e5;
		MaxX = 15.0e5;
		MinY = -9.0e5;
		MaxY = 9.0e5;
	}

	if (mode == 0)
	{
		*OriginX = (MinX + MaxX) / 2;
		*OriginY = (MinY + MaxY) / 2;
		*Width = MaxX - MinX;
		*Height = MaxY - MinY;
		VisibleMinX = MinX;
		VisibleMinY = MinY;
		VisibleMaxX = MaxX;
		VisibleMaxY = MaxY;
	}
	else
	{
		*OriginX = MinX;
		*OriginY = MinY;
		*Width = MaxX;
		*Height = MaxY;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
