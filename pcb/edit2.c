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
#include "calcdef.h"
#include "pcb.h"
#include "nets.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "edit.h"
#include "stdio.h"
#include "mainloop.h"
#include "files.h"
#include "files2.h"
#include "dialogs.h"
#include "insdel.h"
#include "resource.h"
#include "select3.h"

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AddDots(int32 mode)
{
	int32 res, cnt, cntx, cnty, Layer, cntx2, cnty2, ok, NrSectionsX, NrSectionsY, Found, NrSectionDots;
	double MinX, MaxX, MinY, MaxY, StartX, StartY, DotSize, DotSection, DotDistance, MaxScreenX, MaxScreenY;
	AreaFillRecord *BoardOutlineAreaFill;
	char InfoCopy[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	PolygonRecord *MainPolygon, *PolygonObject;
	ObjectRecord *Object, NewObject;
	uint8 PolygonBuf2[10240];
	ObjectArcRecord NewObjectArc;

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	res = FindMinMaxBoard(&MinX, &MinY, &MaxX, &MaxY, 1);
	memset(&NewObject, 0, sizeof(ObjectRecord));
	memset(&NewObjectArc, 0, sizeof(ObjectArcRecord));
	NewObject.ObjectType = PIN_SMD_ROUND;
	AllocateMemTemp(256 * 1024);
	strcpy(InfoCopy, InfoStr);

	SetWaitCursor();
	SystemBusyMode = 410;

	BoardOutlineAreaFill = (AreaFillRecord *) TempMem;
	res = GetBoardOutlineAreaFill(BoardOutlineAreaFill, Design.BoardOutlineKeepOut, 1);
	MainPolygon = (PolygonRecord *) ((uint8 *) BoardOutlineAreaFill + sizeof(AreaFillRecord));
	sprintf(InfoStr, SC(1023, "Bitmap output - %s"), str);
	RedrawInfoStr(1);
	DotSize = 1.0e5;
	NewObject.x2 = DotSize;
	NewObjectArc.Width = (float) DotSize;
	NewObjectArc.Height = (float) DotSize;
	NewObjectArc.Clearance = Design.StandardClearance;
	NewObjectArc.LineThickNess = 0.0;
	NewObjectArc.NetNr = -3;
	NewObjectArc.Info = OBJECT_FILLED;
	NrSectionDots = 40;
	DotDistance = DotSize * 1.2;
	DotSection = DotDistance * NrSectionDots;
	MaxScreenX = MaxX - MinX;
	MaxScreenY = MaxY - MinY;

	NrSectionsX = (int32) ((MaxScreenX + DotSection * 0.99) / DotSection);
	NrSectionsY = (int32) ((MaxScreenY + DotSection * 0.99) / DotSection);
//  for (Layer=0;Layer<Design.NrBoardLayers;Layer++) {
	Layer = 0;
	NewObjectArc.Layer = Layer;

	for (cnty = 0; cnty < NrSectionsY; cnty++)
	{
		for (cntx = 0; cntx < NrSectionsX; cntx++)
		{
			StartX = MinX + cntx * DotSection;
			StartY = MinY + cnty * DotSection;
			SearchMinX = StartX;
			SearchMinY = StartY;
			SearchMaxX = StartX + DotSection;
			SearchMaxY = StartY + DotSection;
			Found = CopyCopperObjectsFromRectWindowToObjects4(Layer, 32 + 4);
			ok = 1;

			for (cnt = 0; cnt < NrObjects4; cnt++)
			{
				Object = &((*Objects4)[cnt]);
				Object->Info2 = 1;

				if ((CheckObjectIsBigPolygon(Object)) || (Object->ObjectType == AREAFILL))
					Object->Info2 = 0;
				else
					FillPositionObject(Object);
			}

			for (cnty2 = 0; cnty2 < NrSectionDots; cnty2++)
			{
				for (cntx2 = 0; cntx2 < NrSectionDots; cntx2++)
				{
					NewObject.x1 = StartX + cntx2 * DotDistance;
					NewObject.y1 = StartY + cnty2 * DotDistance;
					FillPositionObject(&NewObject);
					MakePolygonFromObject(&NewObject, PolygonObject, 0.0, 0.0, 1, 1);

					if ((CheckPolygonInsideAreaFill(PolygonObject, BoardOutlineAreaFill, 0) == 1)
					        || (CheckPolygonOverlapAreaFill(PolygonObject, BoardOutlineAreaFill) == 0))
						continue;

					ok = 1;

					for (cnt = 0; cnt < NrObjects4; cnt++)
					{
						Object = &((*Objects4)[cnt]);

						if ((Object->maxx >= NewObject.minx) && (Object->minx <= NewObject.maxx)
						        && (Object->maxy >= NewObject.miny) && (Object->miny <= NewObject.maxy))
							continue;

						NewObjectArc.CentreX = (float) NewObject.x1;
						NewObjectArc.CentreY = (float) NewObject.y1;
						AddObjectArc(&NewObjectArc);
					}
				}
			}
		}
	}

//  }
	DeAllocateMemObjects4();
	DeAllocateMemTemp();
	SetNormalCursor();
	SystemBusyMode = 0;
	strcpy(InfoStr, InfoCopy);
	RedrawInfoStr(1);
	RePaint();
	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
