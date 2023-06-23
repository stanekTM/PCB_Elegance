/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: move3.c
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
#include "menus.h"
#include "pcb.h"
#include "math.h"
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
#include "move3.h"
#include "calc4.h"
#include "resource.h"
#include "dialogs.h"
#include "help.h"
#include "stdio.h"
#include "files.h"
#include "files2.h"
#include "polygon.h"
#include "dialogs.h"
#include "settings.h"

int32 AreafillNrToMove;

double CentreSelectedX, CentreSelectedY, CurrentX, CurrentY;

double CopyArrayRotationCentreX = 0.0;
double CopyArrayRotationCentreY = 0.0;
double CopyArrayRotationValue = 0.0;
double CopyArrayDistanceValueX = (100 * 2540.0);
double CopyArrayDistanceValueY = (100 * 2540.0);
int32 CopyArrayRotationMode = 0;
int32 CopyArrayRotationCopyNr = 1;
int32 CopyArrayCopyNrX = 1;
int32 CopyArrayCopyNrY = 1;

char DialogTextLine[MAX_LENGTH_STRING];

extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern HDC OutputDisplay;
extern int32 TempUnits;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ChangePCB(int32 mode)
{
	double divx, divy, *nx1, *ny1;
	int32 Layer, cnt, cnt2, cnt3, count;
	double *dx1, *dy1;

	CompRecord *Comp;
	TraceRecord *Trace;
	ViaRecord *Via;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	ConnectionsRecord *Connection;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;
	ObjectTextRecord2 TypeObject;

	memset(&TypeObject, 0, sizeof(TypeObject));

	if (LineInputDialog(&TypeObject, SC(811, "Move entire PCB (This operation can not be undone)"), 0) == 1)
	{
		if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) != 2)
			return;
	}

	if ((InRange(ParamsFloat[0], 0.0)) && (InRange(ParamsFloat[1], 0.0)))
		return;

	divx = ParamsFloat[0];
	divy = ParamsFloat[1];

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->CompOriginX += (float) divx;
		Comp->CompOriginY += (float) divy;
		SetBoardPosComp(Comp, 0);
	}

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);
			Trace->X += (float) divx;
			Trace->Y += (float) divy;
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);
			Trace->X += (float) divx;
			Trace->Y += (float) divy;
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);
			Trace->X += (float) divx;
			Trace->Y += (float) divy;
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);
			Trace->X += (float) divx;
			Trace->Y += (float) divy;
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		Via->X += (float) divx;
		Via->Y += (float) divy;
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);
		Connection->x1 += (float) divx;
		Connection->y1 += (float) divy;
		Connection->x2 += (float) divx;
		Connection->y2 += (float) divy;
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
		AreaPos = (uint8 *) AreaFill;
		count = sizeof(AreaFillRecord);
		DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
		PolygonPos = (uint8 *) DrawPolygon;

		for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
		{
			count = DrawPolygon->NrVertices;
			dx1 = (double *) &((*DrawPolygon).Points);
			dy1 = dx1 + 1;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				*dx1 += divx;
				*dy1 += divy;
				dx1 += 2;
				dy1 += 2;
			}

			SetMinMaxPolygon(DrawPolygon, 0);
			PolygonPos += sizeof(PolygonInitRecord) + count * sizeof(PointRecord);
			DrawPolygon = (PolygonRecord *) PolygonPos;
		}

		nx1 = (double *) AreaFill->StartPolygon;
		ny1 = nx1 + 1;

		for (cnt2 = 0; cnt2 < AreaFill->NrVerticesStartPolygon; cnt2++)
		{
			*nx1 += divx;
			*ny1 += divy;
			nx1 += 2;
			ny1 += 2;
		}

		AreaFill->minx += divx;
		AreaFill->miny += divy;
		AreaFill->maxx += divx;
		AreaFill->maxy += divy;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);
		ObjectLine->X1 += (float) divx;
		ObjectLine->Y1 += (float) divy;
		ObjectLine->X2 += (float) divx;
		ObjectLine->Y2 += (float) divy;
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);
		ObjectRect->CentreX += (float) divx;
		ObjectRect->CentreY += (float) divy;
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    ObjectCircle->CentreX+=divx;
	    ObjectCircle->CentreY+=divy;
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);
		ObjectArc->CentreX += (float) divx;
		ObjectArc->CentreY += (float) divy;
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);
		ObjectText2->X += (float) divx;
		ObjectText2->Y += (float) divy;
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			count = ObjectPolygon->NrVertices;
			dx1 = (double *) &((*ObjectPolygon).Points);
			dy1 = dx1 + 1;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				*dx1 += divx;
				*dy1 += divy;
				dx1 += 2;
				dy1 += 2;
			}
		}

		ObjectPolygon->minx += divx;
		ObjectPolygon->miny += divy;
		ObjectPolygon->maxx += divx;
		ObjectPolygon->maxy += divy;
	}

	Design.BoardOriginX += (float) divx;
	Design.BoardOriginY += (float) divy;
	DataBaseChanged = 1;
	ViewFull();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectTrace(ObjectRecord * ObjectTrace)
{
	double x1, y1, x2, y2, TraceLength;
	ObjectLineRecord NewObjectLine;

	x1 = ObjectTrace->x1;
	y1 = ObjectTrace->y1;
	x2 = ObjectTrace->x2;
	y2 = ObjectTrace->y2;

	if ((InRange(x1, x2)) && (InRange(y1, y2)))
		return 1;
	else
	{
		if (InRange3(x1, x2))
		{	// Ver trace
			ObjectTrace->ObjectType = TRACE_VER;
			TraceLength = fabs(y1 - y2);
			y1 = min(y1, y2);
		}
		else
		{
			if (InRange3(y1, y2))
			{	// Hor trace
				ObjectTrace->ObjectType = TRACE_HOR;
				TraceLength = fabs(x1 - x2);
				x1 = min(x1, x2);
			}
			else
			{
				if (InRange3(x1 - x2, y1 - y2))
				{	// diag2 trace
					ObjectTrace->ObjectType = TRACE_DIAG2;
					TraceLength = fabs(x1 - x2);

					if (x2 < x1)
					{
						x1 = x2;
						y1 = y2;
					}
				}
				else
				{
					if (InRange3(x1 - x2, y2 - y1))
					{	// diag1 trace
						ObjectTrace->ObjectType = TRACE_DIAG1;
						TraceLength = fabs(x1 - x2);

						if (x2 < x1)
						{
							x1 = x2;
							y1 = y2;
						}
					}
					else
					{
						memset(&NewObjectLine, 0, sizeof(NewObjectLine));
						NewObjectLine.X1 = (float) x1;
						NewObjectLine.Y1 = (float) y1;
						NewObjectLine.X2 = (float) x2;
						NewObjectLine.Y2 = (float) y2;
						NewObjectLine.Clearance = (float) ObjectTrace->Clearance;
						NewObjectLine.NetNr = (int16) ObjectTrace->NetNr;
						NewObjectLine.Layer = ObjectTrace->Layer;
						NewObjectLine.LineThickNess = (float) ObjectTrace->Thickness;
						return AddObjectLine(&NewObjectLine);
					}
				}
			}
		}
	}

	ObjectTrace->x1 = x1;
	ObjectTrace->y1 = y1;
	ObjectTrace->x2 = TraceLength;
	ObjectTrace->y2 = ObjectTrace->Thickness;
	return AddTrace(ObjectTrace);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePCB(double Rotation)
{
	double x1;
	int32 Layer, MemSize, TempLastActionNr, PolygonSize, cnt, cnt2, count, mode;

	CompRecord *Comp, *NewComp;
	TraceRecord *Trace;
	ObjectRecord ObjectTrace;
	ViaRecord *Via, NewVia;
	ObjectLineRecord *ObjectLine, NewObjectLine, NewObjectLine2, NewObjectLine3, NewObjectLine4;
	ObjectRectRecord *ObjectRect, NewObjectRect;
	ObjectArcRecord *ObjectArc, NewObjectArc;
	ObjectTextRecord2 *ObjectText2, NewObjectText2;
	ObjectPolygonRecord *ObjectPolygon, NewObjectPolygon;
	ConnectionsRecord *Connection, NewConnection;
	AreaFillRecord *AreaFill;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;

//  Rotation=25.0;
	mode = 0;

	if (InRangeSpecial(Rotation, 90.0, 0.001))
		mode = 1;

	if (InRangeSpecial(Rotation, 180.0, 0.001))
		mode = 2;

	if (InRangeSpecial(Rotation, 270.0, 0.001))
		mode = 3;

	TempLastActionNr = (int16) LastActionNr - 1;
	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Comp->AddNr <= TempLastActionNr))
		{
			MemSize = MemSizeComp(Comp);
			memmove(NewComp, Comp, MemSize);
			RotatePoint(&NewComp->CompOriginX, &NewComp->CompOriginY, Rotation);
			NewComp->Rotation += (float) Rotation;

			if (NewComp->Rotation >= 360.0)
				NewComp->Rotation -= 360.0;

			SetBoardPosComp(NewComp, 0);

			if (AddComp(NewComp))
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				Comp->DeleteNr = (int16) LastActionNr;
				Comp->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	memset(&ObjectTrace, 0, sizeof(ObjectTrace));

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->AddNr <= TempLastActionNr))
			{
				ObjectTrace.x1 = Trace->X;
				ObjectTrace.y1 = Trace->Y;
				ObjectTrace.x2 = Trace->X;
				ObjectTrace.y2 = Trace->Y + Trace->Length;
				ObjectTrace.NetNr = Trace->NetNr;
				ObjectTrace.Layer = Layer;
				ObjectTrace.Clearance = Trace->Clearance;
				ObjectTrace.Thickness = Trace->ThickNess;
				RotatePoint2(&ObjectTrace.x1, &ObjectTrace.y1, Rotation);
				RotatePoint2(&ObjectTrace.x2, &ObjectTrace.y2, Rotation);

				if (AddObjectTrace(&ObjectTrace))
				{
					Trace = &((*VerTraces[Layer])[cnt]);
					Trace->DeleteNr = (int16) LastActionNr;
					Trace->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->AddNr <= TempLastActionNr))
			{
				ObjectTrace.x1 = Trace->X;
				ObjectTrace.y1 = Trace->Y;
				ObjectTrace.x2 = Trace->X + Trace->Length;
				ObjectTrace.y2 = Trace->Y;
				ObjectTrace.NetNr = Trace->NetNr;
				ObjectTrace.Layer = Layer;
				ObjectTrace.Clearance = Trace->Clearance;
				ObjectTrace.Thickness = Trace->ThickNess;
				RotatePoint2(&ObjectTrace.x1, &ObjectTrace.y1, Rotation);
				RotatePoint2(&ObjectTrace.x2, &ObjectTrace.y2, Rotation);

				if (AddObjectTrace(&ObjectTrace))
				{
					Trace = &((*HorTraces[Layer])[cnt]);
					Trace->DeleteNr = (int16) LastActionNr;
					Trace->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->AddNr <= TempLastActionNr))
			{
				ObjectTrace.x1 = Trace->X;
				ObjectTrace.y1 = Trace->Y;
				ObjectTrace.x2 = Trace->X + Trace->Length;
				ObjectTrace.y2 = Trace->Y - Trace->Length;
				ObjectTrace.NetNr = Trace->NetNr;
				ObjectTrace.Layer = Layer;
				ObjectTrace.Clearance = Trace->Clearance;
				ObjectTrace.Thickness = Trace->ThickNess;
				RotatePoint2(&ObjectTrace.x1, &ObjectTrace.y1, Rotation);
				RotatePoint2(&ObjectTrace.x2, &ObjectTrace.y2, Rotation);

				if (AddObjectTrace(&ObjectTrace))
				{
					Trace = &((*Diag1Traces[Layer])[cnt]);
					Trace->DeleteNr = (int16) LastActionNr;
					Trace->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Trace->AddNr <= TempLastActionNr))
			{
				ObjectTrace.x1 = Trace->X;
				ObjectTrace.y1 = Trace->Y;
				ObjectTrace.x2 = Trace->X + Trace->Length;
				ObjectTrace.y2 = Trace->Y + Trace->Length;
				ObjectTrace.NetNr = Trace->NetNr;
				ObjectTrace.Layer = Layer;
				ObjectTrace.Clearance = Trace->Clearance;
				ObjectTrace.Thickness = Trace->ThickNess;
				RotatePoint2(&ObjectTrace.x1, &ObjectTrace.y1, Rotation);
				RotatePoint2(&ObjectTrace.x2, &ObjectTrace.y2, Rotation);

				if (AddObjectTrace(&ObjectTrace))
				{
					Trace = &((*Diag2Traces[Layer])[cnt]);
					Trace->DeleteNr = (int16) LastActionNr;
					Trace->Info |= OBJECT_NOT_VISIBLE;
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if (((Via->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Via->AddNr <= TempLastActionNr))
		{
			memmove(&NewVia, Via, sizeof(ViaRecord));
			RotatePoint(&NewVia.X, &NewVia.Y, Rotation);

			if (AddVia(&NewVia))
			{
				Via = &((*Vias)[cnt]);
				Via->DeleteNr = (int16) LastActionNr;
				Via->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);

		if (((Connection->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Connection->AddNr <= TempLastActionNr))
		{
			memmove(&NewConnection, Connection, sizeof(ConnectionsRecord));
			RotatePoint(&NewConnection.x1, &NewConnection.y1, Rotation);
			RotatePoint(&NewConnection.x2, &NewConnection.y2, Rotation);

			if (AddConnection(&NewConnection))
			{
				Connection = (ConnectionsRecord *) & ((*Connections)[cnt]);
				Connection->DeleteNr = (int16) LastActionNr;
				Connection->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	MemSize = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->AddNr <= TempLastActionNr))
			MemSize = max(MemSize, AreaFill->MemSize);
	}

	if (AllocateMemAreaFillMemoryTemp(MemSize + 3172) != 0)
		return;

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->AddNr <= TempLastActionNr))
		{
			memmove(NewAreaFill, AreaFill, AreaFill->MemSize);
			AreaPos = (uint8 *) NewAreaFill;
			count = sizeof(AreaFillRecord);
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
			PolygonPos = (uint8 *) DrawPolygon;

			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				RotatePolygon(DrawPolygon, 0.0, 0.0, Rotation, 0);
				SetMinMaxPolygon(DrawPolygon, 0);
				PolygonPos += MemSizePolygon(DrawPolygon);
				DrawPolygon = (PolygonRecord *) PolygonPos;
			}

			count = NewAreaFill->NrVerticesStartPolygon;

			for (cnt2 = 0; cnt2 < count; cnt2++)
				RotatePoint(&NewAreaFill->StartPolygon[cnt2].x, &NewAreaFill->StartPolygon[cnt2].y, Rotation);

			if (AddAreaFill(0))
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);
				AreaFill->DeleteNr = (int16) LastActionNr;
				AreaFill->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectLine->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			RotatePoint(&NewObjectLine.X1, &NewObjectLine.Y1, Rotation);
			RotatePoint(&NewObjectLine.X2, &NewObjectLine.Y2, Rotation);

			if (AddObjectLine(&NewObjectLine))
			{
				ObjectLine = &((*ObjectLines)[cnt]);
				ObjectLine->DeleteNr = (int16) LastActionNr;
				ObjectLine->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectRect->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			RotatePoint(&NewObjectRect.CentreX, &NewObjectRect.CentreY, Rotation);

			if ((mode == 1) || (mode == 3))
			{
				x1 = NewObjectRect.Width;
				NewObjectRect.Width = NewObjectRect.Height;
				NewObjectRect.Height = (float) x1;
			}

			if (mode > 0)
			{
				if (AddObjectRect(&NewObjectRect))
				{
					ObjectRect = &((*ObjectRects)[cnt]);
					ObjectRect->DeleteNr = (int16) LastActionNr;
					ObjectRect->Info |= OBJECT_NOT_VISIBLE;
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
					RotatePoint(&NewObjectLine.X1, &NewObjectLine.Y1, Rotation);
					RotatePoint(&NewObjectLine.X2, &NewObjectLine.Y2, Rotation);
					RotatePoint(&NewObjectLine2.X1, &NewObjectLine2.Y1, Rotation);
					RotatePoint(&NewObjectLine2.X2, &NewObjectLine2.Y2, Rotation);
					RotatePoint(&NewObjectLine3.X1, &NewObjectLine3.Y1, Rotation);
					RotatePoint(&NewObjectLine3.X2, &NewObjectLine3.Y2, Rotation);
					RotatePoint(&NewObjectLine4.X1, &NewObjectLine4.Y1, Rotation);
					RotatePoint(&NewObjectLine4.X2, &NewObjectLine4.Y2, Rotation);

					if ((AddObjectLine(&NewObjectLine)) && (AddObjectLine(&NewObjectLine2))
					        && (AddObjectLine(&NewObjectLine3)) && (AddObjectLine(&NewObjectLine4)))
					{
						ObjectRect = &((*ObjectRects)[cnt]);
						ObjectRect->Info |= OBJECT_NOT_VISIBLE;
						ObjectRect->DeleteNr = (int16) LastActionNr;
					}
				}
				else
				{
					memset(&NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));
					NewObjectPolygon.NrVertices = 4;
					NewObjectPolygon.Layer = ObjectRect->Layer;
					NewObjectPolygon.Points[0].x = ObjectRect->CentreX - ObjectRect->Width * (float) 0.5;
					NewObjectPolygon.Points[0].y = ObjectRect->CentreY - ObjectRect->Height * (float) 0.5;
					NewObjectPolygon.Points[1].x = ObjectRect->CentreX - ObjectRect->Width * (float) 0.5;
					NewObjectPolygon.Points[1].y = ObjectRect->CentreY + ObjectRect->Height * (float) 0.5;
					NewObjectPolygon.Points[2].x = ObjectRect->CentreX + ObjectRect->Width * (float) 0.5;
					NewObjectPolygon.Points[2].y = ObjectRect->CentreY + ObjectRect->Height * (float) 0.5;
					NewObjectPolygon.Points[3].x = ObjectRect->CentreX + ObjectRect->Width * (float) 0.5;
					NewObjectPolygon.Points[3].y = ObjectRect->CentreY - ObjectRect->Height * (float) 0.5;

					RotateObjectPolygon(&NewObjectPolygon, 0.0, 0.0, Rotation, 0);
					SetMinMaxObjectPolygon(&NewObjectPolygon, 0);

					if (AddObjectPolygon(&NewObjectPolygon))
					{
						ObjectRect = &((*ObjectRects)[cnt]);
						ObjectRect->Info |= OBJECT_NOT_VISIBLE;
						ObjectRect->DeleteNr = (int16) LastActionNr;
					}
				}
			}
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    ObjectCircle->CentreX+=divx;
	    ObjectCircle->CentreY+=divy;
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectArc->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			RotatePoint(&NewObjectArc.CentreX, &NewObjectArc.CentreY, Rotation);
			RotatePoint(&NewObjectArc.StartDiffX, &NewObjectArc.StartDiffY, Rotation);
			RotatePoint(&NewObjectArc.EndDiffX, &NewObjectArc.EndDiffY, Rotation);

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->DeleteNr = (int16) LastActionNr;
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectText2->AddNr <= TempLastActionNr))
		{
			memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			RotatePoint(&NewObjectText2.X, &NewObjectText2.Y, Rotation);

			if (NewObjectText2.TextMode & 0x10)
			{	// Mirror
				NewObjectText2.Rotation -= (float) Rotation;

				if (NewObjectText2.Rotation <= 360.0)
					NewObjectText2.Rotation += 360.0;
			}
			else
			{
				NewObjectText2.Rotation += (float) Rotation;

				if (NewObjectText2.Rotation >= 360.0)
					NewObjectText2.Rotation -= 360.0;
			}

			if (AddObjectText2(&NewObjectText2))
			{
				ObjectText2 = &((*ObjectTexts2)[cnt]);
				ObjectText2->DeleteNr = (int16) LastActionNr;
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			PolygonSize = MemSizeObjectPolygon(ObjectPolygon);
			memmove(&NewObjectPolygon, ObjectPolygon, PolygonSize);
			RotateObjectPolygon(&NewObjectPolygon, 0.0, 0.0, Rotation, 0);
			SetMinMaxObjectPolygon(&NewObjectPolygon, 0);

			if (AddObjectPolygon(&NewObjectPolygon))
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);
				ObjectPolygon->DeleteNr = (int16) LastActionNr;
				ObjectPolygon->Info |= OBJECT_NOT_VISIBLE;
			}
		}
	}

	DataBaseChanged = 1;
	ViewFull();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MirrorText(int32 mode)
{
	int32 cnt, TempLastActionNr;
	ObjectTextRecord2 *ObjectText2, ChangedObjectText2;

	TempLastActionNr = (int16) LastActionNr - 1;

	/*
	  for (cnt=0;cnt<Design.NrObjectTexts;cnt++) {
	    ObjectText=&((*ObjectTexts)[cnt]);
	    if (((ObjectText->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectText->AddNr<=TempLastActionNr)) {
	      memmove(&ChangedObjectText,ObjectText,sizeof(ObjectTextRecord));
	      ChangedObjectText.Info&=~OBJECT_SELECTED;
	      ChangedObjectText.TextMode^=0x10;
	      if (AddObjectText(&ChangedObjectText)) {
	        ObjectText=&((*ObjectTexts)[cnt]);
	        SetBackGroundActive(0);
	        DrawObjectText(ObjectText,0.0,0.0,1);
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
			memmove(&ChangedObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			ChangedObjectText2.Info &= ~OBJECT_SELECTED;
			ChangedObjectText2.TextMode ^= 0x10;

			if (AddObjectText2(&ChangedObjectText2))
			{
				ObjectText2 = &((*ObjectTexts2)[cnt]);
				ObjectText2->Info |= OBJECT_NOT_VISIBLE;
				ObjectText2->DeleteNr = (int16) LastActionNr;
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

void ChangeGeometryComp(int32 Mode)
{
	int32 cnt, cnt2, CompPinNr, TempLastActionNr, MemSize, NewShapeNr, OldShapeNr, OkToRepaint, MemPos, res, CompInfo;
	CompRecord *Comp, *NewComp;
	int fp;
	char Geometrie[MAX_LENGTH_STRING], PinText[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	CompPinRecord *OldCompPin, *NewCompPin;
	ShapeRecord *NewShape, *OldShape;
	uint8 *NewCompBuf;

	if (GetNrCompsSelected() != 1)
		return;

	MessageBufPos = 0;
	Geometrie[0] = 0;
	SelectGeometrie(Geometrie);

	if (Geometrie[0] == 0)
		return;

	NewShapeNr = LoadShape(Geometrie);
	OkToRepaint = 0;

	if (NewShapeNr == -1)
		return;

	MemPos = (*Shapes)[NewShapeNr].ShapePos;
	NewShape = (ShapeRecord *) & (ShapesMem[MemPos]);

	TempLastActionNr = (int16) LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if (((Comp->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Comp->AddNr <= TempLastActionNr))
		{
			OldShapeNr = Comp->ShapeNr;

			if (OldShapeNr == -1)
				return;

			MemPos = (*Shapes)[OldShapeNr].ShapePos;
			OldShape = (ShapeRecord *) & (ShapesMem[MemPos]);

			if (OldShapeNr == NewShapeNr)
			{
				MessageBoxOwn(PCBWindow, SC(812, "Geometries are the same"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
				return;
			}

			OldCompPin = (CompPinRecord *) & (CompsMem[(*Comps)[cnt] + sizeof(CompRecord)]);

			for (cnt2 = 0; cnt2 < Comp->NrPins; cnt2++)
			{
				CompPinText(Comp, cnt2, 0.0, 0.0, PinText);
				CompPinNr = GetShapeCompPinNrByPinText(NewShape, PinText);

				if ((CompPinNr == -1) && (OldCompPin->NetNr != -1))
				{
					sprintf(str, "%s", PinText);
					AddToMessageBuf(str);
				}

				OldCompPin += 1;
			}

			if (MessageBufPos != 0)
			{
				MessageDialog(SC(813, "To following pins do not exist on the new geometry"), 0, 0);
				DeAllocateMemMessageBuf();
				return;
			}

			MemSize = MemSizeComp(Comp);
			AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &NewComp);
			NewCompBuf = (uint8 *) NewComp;
			Comp->Info &= ~(OBJECT_SELECTED | 0x10);
			Comp->TextVisibility &= ~(1 + 16);
			memmove(NewComp, Comp, MemSize);
			NewComp->NrPins = (int16) NewShape->NrPins;
			NewComp->ShapeNr = (int16) NewShapeNr;
			strcpy(NewComp->ShapeName, Geometrie);
			NewCompPin = (CompPinRecord *) & NewCompBuf[sizeof(CompRecord)];

			for (cnt2 = 0; cnt2 < NewComp->NrPins; cnt2++)
			{
				NewCompPin->NetNr = -1;
				NewCompPin += 1;
			}

			OldCompPin = (CompPinRecord *) & (CompsMem[(*Comps)[cnt] + sizeof(CompRecord)]);

			for (cnt2 = 0; cnt2 < Comp->NrPins; cnt2++)
			{
				CompPinText(Comp, cnt2, 0.0, 0.0, PinText);
				CompPinNr = GetShapeCompPinNrByPinText(NewShape, PinText);
				NewCompPin = (CompPinRecord *) & NewCompBuf[sizeof(CompRecord) + CompPinNr * sizeof(CompPinRecord)];
				NewCompPin->NetNr = OldCompPin->NetNr;
				OldCompPin += 1;
			}

			if (AddComp(NewComp))
			{
				NewComp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
				OkToRepaint = 1;
				Comp->Info |= OBJECT_NOT_VISIBLE;
				Comp->DeleteNr = (int16) LastActionNr;

// *******************************************************************************************************

				sprintf(str, "%s\\pcb\\gatepin.ban", DesignPath);

				if (FileExistsUTF8(str) == 0)
				{
					res = FileSizeUTF8(str);

					if ((fp = FileOpenUTF8(str)) <= 0)
						return;

					FileSeek(fp, res);
				}
				else
				{
					if ((fp = FileOpenWriteUTF8(str)) <= 0)
						return;
				}

				sprintf(str, "CHANGE GEOM %s WITH %s", NewComp->Name, NewComp->ShapeName);
				WriteLn(fp, str);
				FileClose(fp);
			}
		}
	}

	if (OkToRepaint)
		RePaint();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void GetStringKomma(LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

	cnt = 0;

	while ((cnt < l) && ((Str[cnt] == ' ')))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	pos = cnt;
	cnt++;

	while ((cnt < l) && (Str[cnt] != ','))
		cnt++;

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos);

	Result[cnt - pos] = 0;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt], l - cnt + 1);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AddSpecialObjectsFromAsciiFile(int32 Layer, int32 mode)
{
	int32 cnt, fp, Length, CurrentUnits, OkToInclude;
	double UnitMult;
	char NewFile[MAX_LENGTH_STRING], LineBuf[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING],
	     str4[MAX_LENGTH_STRING], Params[10][MAX_LENGTH_STRING];

	if (GetNewFileUTF8(PCBWindow, NULL, NewFile, ExportDir, SC(390, "All files"), NULL, SC(390, "All files"), "*", 0))
		return;

	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	NewObjectLine.Layer = Layer;
	NewObjectLine.Info = OBJECT_SELECTED;
	memset(&NewObjectRect, 0, sizeof(ObjectRectRecord));
	NewObjectRect.Layer = Layer;
	NewObjectRect.Info = OBJECT_SELECTED;
	memset(&NewObjectCircle, 0, sizeof(ObjectCircleRecord));
	NewObjectCircle.Layer = Layer;
	NewObjectCircle.Info = OBJECT_SELECTED;
	NewObjectCircle.CircleMode = 15;
	memset(&NewObjectArc, 0, sizeof(ObjectArcRecord));
	NewObjectArc.Layer = Layer;
	NewObjectArc.Info = OBJECT_SELECTED;
	memset(&NewObjectText2, 0, sizeof(ObjectTextRecord2));
	NewObjectText2.Layer = Layer;
	NewObjectText2.Info = OBJECT_SELECTED;

	CurrentUnits = 0;
	UnitMult = 2540.0;

	if ((fp = TextFileOpenUTF8(NewFile)) < 0)
		return;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;
		strcpy(str4, LineBuf);

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString(LineBuf, str);	// Command
			GetString(LineBuf, str2);	// Parameters

			if (stricmpOwn(str, SC(814, "Units")) == 0)
			{
				if (stricmpOwn(str2, "mm") == 0)
				{
					CurrentUnits = 1;
					UnitMult = 100000.0;
				}

				if (stricmpOwn(str2, SC(816, "mils")) == 0)
				{
					CurrentUnits = 0;
					UnitMult = 2540.0;
				}

				if (stricmpOwn(str2, "thou") == 0)
				{
					CurrentUnits = 0;
					UnitMult = 2540.0;
				}

				if (stricmpOwn(str2, SC(817, "inch")) == 0)
				{
					CurrentUnits = 2;
					UnitMult = 2540000.0;
				}

				if (stricmpOwn(str2, "cm") == 0)
				{
					CurrentUnits = 3;
					UnitMult = 1000000.0;
				}
			}

			if (stricmpOwn(str, "Line") == 0)
			{
				OkToInclude = 1;

				for (cnt = 0; cnt < 5; cnt++)
				{
					Params[cnt][0] = 0;
					ParamsFloat[cnt] = 0.0;
					GetStringKomma(str2, Params[cnt]);

					if ((Params[cnt][0] == 0) || (sscanf(Params[cnt], "%f", &ParamsFloat[cnt]) != 1))
						OkToInclude = 0;
					else
						ParamsFloat[cnt] = (float) (ParamsFloat[cnt] * UnitMult);
				}

				if (OkToInclude)
				{
					NewObjectLine.X1 = ParamsFloat[0];
					NewObjectLine.Y1 = ParamsFloat[1];
					NewObjectLine.X2 = ParamsFloat[2];
					NewObjectLine.Y2 = ParamsFloat[3];
					NewObjectLine.LineThickNess = ParamsFloat[4];

					if (AddObjectLine(&NewObjectLine))
					{
					}
				}
			}

			if ((stricmpOwn(str, "Rect") == 0) || (stricmpOwn(str, "Rectangle") == 0))
			{
				OkToInclude = 1;

				for (cnt = 0; cnt < 5; cnt++)
				{
					Params[cnt][0] = 0;
					ParamsFloat[cnt] = 0.0;
					GetStringKomma(str2, Params[cnt]);

					if ((Params[cnt][0] == 0) || (sscanf(Params[cnt], "%f", &ParamsFloat[cnt]) != 1))
						OkToInclude = 0;
					else
						ParamsFloat[cnt] = (float) (ParamsFloat[cnt] * UnitMult);
				}

				if (OkToInclude)
				{
					NewObjectRect.CentreX = ParamsFloat[0];
					NewObjectRect.CentreY = ParamsFloat[1];
					NewObjectRect.Width = ParamsFloat[2];
					NewObjectRect.Height = ParamsFloat[3];
					NewObjectRect.LineThickNess = ParamsFloat[4];

					if (AddObjectRect(&NewObjectRect))
					{
					}
				}
			}

			if (stricmpOwn(str, "Circle") == 0)
			{
				OkToInclude = 1;

				for (cnt = 0; cnt < 4; cnt++)
				{
					Params[cnt][0] = 0;
					ParamsFloat[cnt] = 0.0;
					GetStringKomma(str2, Params[cnt]);

					if ((Params[cnt][0] == 0) || (sscanf(Params[cnt], "%f", &ParamsFloat[cnt]) != 1))
						OkToInclude = 0;
					else
						ParamsFloat[cnt] = (float) (ParamsFloat[cnt] * UnitMult);
				}

				if (OkToInclude)
				{
					NewObjectArc.CentreX = ParamsFloat[0];
					NewObjectArc.CentreY = ParamsFloat[1];
					NewObjectArc.Width = ParamsFloat[2];
					NewObjectArc.Height = ParamsFloat[2];
					NewObjectArc.StartDiffX = 0.0;
					NewObjectArc.StartDiffY = 100e5;
					NewObjectArc.EndDiffX = 0.0;
					NewObjectArc.EndDiffY = 100e5;
					NewObjectArc.LineThickNess = ParamsFloat[3];

					if (AddObjectArc(&NewObjectArc))
					{
					}
				}
			}

			if (stricmpOwn(str, "Arc") == 0)
			{
				OkToInclude = 1;

				for (cnt = 0; cnt < 8; cnt++)
				{
					Params[cnt][0] = 0;
					ParamsFloat[cnt] = 0.0;
					GetStringKomma(str2, Params[cnt]);

					if ((Params[cnt][0] == 0) || (sscanf(Params[cnt], "%f", &ParamsFloat[cnt]) != 1))
						OkToInclude = 0;
					else
						ParamsFloat[cnt] = (float) (ParamsFloat[cnt] * UnitMult);
				}

				if (OkToInclude)
				{
					NewObjectArc.CentreX = ParamsFloat[0];
					NewObjectArc.CentreY = ParamsFloat[1];
					NewObjectArc.Width = ParamsFloat[2];
					NewObjectArc.Height = ParamsFloat[2];
					NewObjectArc.StartDiffX = ParamsFloat[3];
					NewObjectArc.StartDiffY = ParamsFloat[4];
					NewObjectArc.EndDiffX = ParamsFloat[5];
					NewObjectArc.EndDiffY = ParamsFloat[6];
					NewObjectArc.LineThickNess = ParamsFloat[7];

					if (AddObjectArc(&NewObjectArc))
					{
					}
				}
			}

			if (stricmpOwn(str, "Text") == 0)
			{
				OkToInclude = 1;

				for (cnt = 0; cnt < 5; cnt++)
				{
					Params[cnt][0] = 0;
					ParamsFloat[cnt] = 0.0;
					GetStringKomma(str2, Params[cnt]);

					if ((Params[cnt][0] == 0) || (sscanf(Params[cnt], "%f", &ParamsFloat[cnt]) != 1))
						OkToInclude = 0;
					else
						ParamsFloat[cnt] = (float) (ParamsFloat[cnt] * UnitMult);
				}

				Params[5][0] = 0;
				GetQuoteString(str2, Params[5]);

				if (Params[5][0] == 0)
					OkToInclude = 0;

				Params[5][sizeof(NewObjectText2.Text) - 1] = 0;

				if (OkToInclude)
				{
					NewObjectText2.X = ParamsFloat[0];
					NewObjectText2.Y = ParamsFloat[1];
					NewObjectText2.FontHeight = ParamsFloat[2];
					NewObjectText2.Rotation = ParamsFloat[3];
					NewObjectText.LineThickNess = ParamsFloat[4];
					strcpy(NewObjectText.Text, Params[5]);

					if (AddObjectText2(&NewObjectText2))
					{
					}
				}
			}
		}
	}

	TextFileClose(fp);
	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void FormatValue(LPSTR Text, int32 Units, double value)
{
	switch (Units)
	{
	case 0:
		sprintf(Text, "%.2f", value / 2540.0);
		break;

	case 1:
		sprintf(Text, "%.4f", value / 100000.0);
		break;

	case 2:
		sprintf(Text, "%.5f", value / 2540000.0);
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PutSpecialObjectsInAsciiFile(int32 Layer, int32 mode)
{
	int32 cnt, fp, CurrentUnits, Mask;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	char NewFile[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	if (GetNewFileUTF8(PCBWindow, NULL, NewFile, ExportDir, SC(390, "All files"), NULL, SC(390, "All files"), "*", 0))
		return;

	Mask = 0;

	if ((mode & 0x10) == 0x10)
		Mask = OBJECT_SELECTED;

	CurrentUnits = mode & 0x0f;

	if ((fp = FileOpenWriteUTF8(NewFile)) < 0)
		return;

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | Mask)) == Mask) && (ObjectLine->Layer == Layer))
		{
			sprintf(str, "LINE   ");
			FormatValue(str2, CurrentUnits, ObjectLine->X1);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectLine->Y1);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectLine->X2);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectLine->Y2);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectLine->LineThickNess);
			strcat(str, str2);
			WriteLn(fp, str);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | Mask)) == Mask) && (ObjectRect->Layer == Layer))
		{
			sprintf(str, "RECT   ");
			FormatValue(str2, CurrentUnits, ObjectRect->CentreX);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectRect->CentreY);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectRect->Width);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectRect->Height);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectRect->LineThickNess);
			strcat(str, str2);
			WriteLn(fp, str);
		}
	}

	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|Mask)) == Mask)
	       &&
	       (ObjectCircle->Layer==Layer)) {
	      sprintf(str,"CIRCLE ");
	      FormatValue(str2,CurrentUnits,ObjectCircle->CentreX);
	      strcat(str,str2);
	      strcat(str,",");
	      FormatValue(str2,CurrentUnits,ObjectCircle->CentreX);
	      strcat(str,str2);
	      strcat(str,",");
	      FormatValue(str2,CurrentUnits,ObjectCircle->Diam);
	      strcat(str,str2);
	      strcat(str,",");
	      FormatValue(str2,CurrentUnits,ObjectCircle->LineThickNess);
	      strcat(str,str2);
	      WriteLn(fp,str);
	    }
	  }
	*/
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | Mask)) == Mask) && (ObjectArc->Layer == Layer))
		{
			sprintf(str, "ARC    ");
			FormatValue(str2, CurrentUnits, ObjectArc->CentreX);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectArc->CentreX);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectArc->Width);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectArc->StartDiffX);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectArc->StartDiffY);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectArc->EndDiffX);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectArc->EndDiffY);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectArc->LineThickNess);
			strcat(str, str2);
			WriteLn(fp, str);
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
	{
		ObjectText = &((*ObjectTexts)[cnt]);

		if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | Mask)) == Mask) && (ObjectText->Layer == Layer))
		{
			sprintf(str, "TEXT   ");
			FormatValue(str2, CurrentUnits, ObjectText->X);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectText->Y);
			strcat(str, str2);
			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectText->FontHeight);
			strcat(str, str2);
			strcat(str, ",");

			if ((ObjectText->TextMode & 256) == 0)
				strcat(str, "0");
			else
				strcat(str, "90");

			strcat(str, ",");
			FormatValue(str2, CurrentUnits, ObjectText->LineThickNess);
			strcat(str, str2);
			WriteLn(fp, str);
		}
	}

	FileClose(fp);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawSelectedAreafills(double CurrentX, double CurrentY, double CurrentX2, double CurrentY2,
                            double CentreSelectedX, double CentreSelectedY, double Rotation, int32 mode)
{
	AreaFillRecord *AreaFill;
	PolygonRecord *Polygon, *FirstPolygon;
	int32 count, cnt, cnt2, cnt3, AreaFillCount;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;
	uint8 *PolygonPos;

	AreaFillCount = 0;
	FirstPolygon = NULL;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		if ((mode & 1) == 0)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
				FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
		}
		else
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNrToMove]]);
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
			AreaFillCount++;
		}

		if ((AreaFillCount < 2) && ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		        && ((FirstPolygon->PolygonType & 2) == 2))
		{
			PolygonPos = (uint8 *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
			Polygon = (PolygonRecord *) PolygonPos;

			if ((mode & 1) == 1)
			{
				StartDrawingEditingWindow(0);
				SetROP2(OutputDisplay, R2_XORPEN);
			}

			DrawCode = DrawLayerCode[AreaFill->Layer];
			InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				if ((cnt2 == 0) || ((cnt2 > 0) && ((Polygon->PolygonType & 4) == 4)))
				{
					count = Polygon->NrVertices;
					x1 = (*Polygon).Points[0].x;
					y1 = (*Polygon).Points[0].y;
					RotatePointFromOtherPoint2(&x1, &y1, CentreSelectedX, CentreSelectedY, Rotation);
//          x1+=CurrentX-CentreSelectedX-CurrentX2;
//          y1+=CurrentY-CentreSelectedY-CurrentY2;
					x1 += CurrentX - CurrentX2;
					y1 += CurrentY - CurrentY2;

					for (cnt3 = 0; cnt3 < count; cnt3++)
					{
						if (cnt3 < count - 1)
						{
							x2 = (*Polygon).Points[cnt3 + 1].x;
							y2 = (*Polygon).Points[cnt3 + 1].y;
						}
						else
						{
							x2 = (*Polygon).Points[0].x;
							y2 = (*Polygon).Points[0].y;
						}

						RotatePointFromOtherPoint2(&x2, &y2, CentreSelectedX, CentreSelectedY, Rotation);
//            x2+=CurrentX-CentreSelectedX-CurrentX2;
//            y2+=CurrentY-CentreSelectedY-CurrentY2;
						x2 += CurrentX - CurrentX2;
						y2 += CurrentY - CurrentY2;

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
							DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));

						x1 = x2;
						y1 = y2;
					}
				}

				PolygonPos += MemSizePolygon(Polygon);
				Polygon = (PolygonRecord *) PolygonPos;
			}
		}
	}

	if ((mode & 2) == 0)
		DrawCrossHair(8);

	if ((mode & 1) == 1)
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 MoveSelectedAreafill(int32 mode)
{
	int32 cnt, CompPlaced, FirstShift, NewMode;
	double OldX, OldY, CurrentX, CurrentY, divx, divy, ShiftX, ShiftY, x1, y1, Rotation, CurrentX2, CurrentY2;
	AreaFillRecord *AreaFill;
	char TempInfoStr[MAX_LENGTH_STRING];
	PolygonRecord *FirstPolygon;
	HMENU PopUpMenu;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;
	PopUpMenu = CreatePopupMenu();
	AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(493, "Escape"));

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;
	AreafillNrToMove = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if (((FirstPolygon->PolygonType & 2) == 2) && (AreafillNrToMove == -1))
				AreafillNrToMove = cnt;
		}
	}

	if (AreafillNrToMove == -1)
		return -1;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNrToMove]]);
	FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	CentreSelectedX = AreaFill->StartPolygon[0].x;
	CentreSelectedY = AreaFill->StartPolygon[0].y;

//  CentreSelectedX=CurrentX;
//  CentreSelectedY=CurrentY;
	strcpy(TempInfoStr, InfoStr);

	/*
	  CentreSelectedX=CurrentX;
	  CentreSelectedY=CurrentY;
	  GetMinMaxSelectedSpecialObjects();

	  if ((NrRectanglesSelected!=0)
	     ||
	     (NrCirclesSelected!=0)
	     ||
	     (NrArcsSelected!=0)
	     ||
	     (NrTextsSelected!=0)) {
	    CentreSelectedX=SelectedMinX;
	    CentreSelectedY=SelectedMinY;
	  }
	*/
	OldX = CurrentX;
	OldY = CurrentY;

	ClipMouseCursor();
	Rotation = 0.0;
	DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
	FirstShift = 1;
	CompPlaced = 0;
	SystemBusyMode = 3;
	NewMode = 1;
	DrawXorFunction.Function6 = (FUNCP6) DrawSelectedAreafills;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &CurrentX2;
	DrawXorFunction.Param1[3] = &CurrentY2;
	DrawXorFunction.Param1[4] = &CentreSelectedX;
	DrawXorFunction.Param1[5] = &CentreSelectedY;
	DrawXorFunction.Param1[6] = &Rotation;
	DrawXorFunction.Param1[7] = &NewMode;
	DrawXorFunction.Mode = 5;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &CurrentX2;
	DrawXorFunction.Param2[3] = &CurrentY2;
	DrawXorFunction.Param2[4] = &CentreSelectedX;
	DrawXorFunction.Param2[5] = &CentreSelectedY;
	DrawXorFunction.Param2[6] = &Rotation;
	DrawXorFunction.Param2[7] = &NewMode;
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
				{
					DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation,
					                      1);
				}

				if (Units == 0)
				{
					x1 = (CurrentX - CentreSelectedX - CurrentX2) / 2540.0;
					y1 = (CurrentY - CentreSelectedY - CurrentY2) / 2540.0;
					sprintf(InfoStr, "%s ( %.2f , %.2f thou )", TempInfoStr, x1, y1);
				}
				else
				{
					x1 = (CurrentX - CentreSelectedX - CurrentX2) / 100000.0;
					y1 = (CurrentY - CentreSelectedY - CurrentY2) / 100000.0;
					sprintf(InfoStr, "5%s ( %.4f , %.4f mm )", TempInfoStr, x1, y1);
				}

				RedrawInfoStr(1);
				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
				{
					DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
					                      Rotation, 1);
				}
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
				                      Rotation, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
				                      Rotation, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
				                      Rotation, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
				                      Rotation, 1);
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
			DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation,
			                      1);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
			CompPlaced = 1;
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
			{
				DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
				                      Rotation, 1);
			}
			else
				CompPlaced = 1;
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
			{
				DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
				                      Rotation, 1);
			}
			else
				CompPlaced = 1;
		}

		if (CheckLeftButton())
		{
			CompPlaced = 1;
			divx = CurrentX - CentreSelectedX;
			divy = CurrentY - CentreSelectedY;
			DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation,
			                      1);
			PlaceMovedAreafill(AreafillNrToMove, CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX,
			                   CentreSelectedY, Rotation, mode * 2 + 1);
			CheckInputMessages(0);
			SelectionEsc = 1;
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);

			if (!AltPressed)
				Rotation += 90.0;
			else
				Rotation += 45.0;

			if (Rotation >= 360.0)
				Rotation -= 360.0;

			DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation,
			                      1);
			RightButtonPressed = 0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawSelectedAreafills(OldX, OldY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (SpacePressed)
				SpacePressed = 0;

			if (HelpAsked)
			{
				if (mode == 0)
					Help("move_areafill.htm", 0);
				else
					Help("copy_areafill.htm", 0);

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
			{
				DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY,
				                      Rotation, 1);
			}
		}
	}

	strcpy(InfoStr, TempInfoStr);
	RedrawInfoStr(1);
	DrawCrossHair(2);

	if (!CompPlaced)
		DrawSelectedAreafills(CurrentX, CurrentY, CurrentX2, CurrentY2, CentreSelectedX, CentreSelectedY, Rotation, 1);

	UnClipMouseCursor();
	SystemBusyMode = 0;
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DrawStretchedAreafill(int32 AreafillNrToMove, double CurrentX, double CurrentY, int32 mode)
{
	AreaFillRecord *AreaFill;
	PolygonRecord *Polygon, *FirstPolygon;
	int32 count, cnt3;
	double x1, y1, x2, y2, Xmin, Xmax, Ymin, Ymax;
	uint8 *PolygonPos;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNrToMove]]);
	FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

	PolygonPos = (uint8 *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	Polygon = (PolygonRecord *) PolygonPos;

	if (mode == 1)
	{
		StartDrawingEditingWindow(0);
		SetROP2(OutputDisplay, R2_XORPEN);
	}

	DrawCode = DrawLayerCode[AreaFill->Layer];

	if ((AreaFill->Info & POWERPLANE) == 0)
	{
		InitDrawingObject(TRACE_HOR, DrawCode, 0, NORMAL_FILLED_AND_PEN1);

		count = AreaFill->NrVerticesStartPolygon;
		x1 = AreaFill->StartPolygon[0].x;
		y1 = AreaFill->StartPolygon[0].y;

		if ((x1 >= SearchMinX) && (x1 <= SearchMaxX) && (y1 >= SearchMinY) && (y1 <= SearchMaxY))
		{
			x1 += CurrentX;
			y1 += CurrentY;
		}

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			if (cnt3 < count - 1)
			{
				x2 = AreaFill->StartPolygon[cnt3 + 1].x;
				y2 = AreaFill->StartPolygon[cnt3 + 1].y;
			}
			else
			{
				x2 = AreaFill->StartPolygon[0].x;
				y2 = AreaFill->StartPolygon[0].y;
			}

			if ((x2 >= SearchMinX) && (x2 <= SearchMaxX) && (y2 >= SearchMinY) && (y2 <= SearchMaxY))
			{
				x2 += CurrentX;
				y2 += CurrentY;
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
				DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));

			x1 = x2;
			y1 = y2;
		}
	}

	InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_WHITE + DRAW_WITH_PEN_AND_NOT_FILLED);
	count = Polygon->NrVertices;
	x1 = (*Polygon).Points[0].x;
	y1 = (*Polygon).Points[0].y;

	if ((x1 >= SearchMinX) && (x1 <= SearchMaxX) && (y1 >= SearchMinY) && (y1 <= SearchMaxY))
	{
		x1 += CurrentX;
		y1 += CurrentY;
	}

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		if (cnt3 < count - 1)
		{
			x2 = (*Polygon).Points[cnt3 + 1].x;
			y2 = (*Polygon).Points[cnt3 + 1].y;
		}
		else
		{
			x2 = (*Polygon).Points[0].x;
			y2 = (*Polygon).Points[0].y;
		}

		if ((x2 >= SearchMinX) && (x2 <= SearchMaxX) && (y2 >= SearchMinY) && (y2 <= SearchMaxY))
		{
			x2 += CurrentX;
			y2 += CurrentY;
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
			DrawLine(MultX(x1), MultY(y1), MultX(x2), MultY(y2));

		x1 = x2;
		y1 = y2;
	}

	DrawCrossHair(8);

	if (mode == 1)
	{
		ExitDrawing();
		EndDrawingEditingWindow(0);
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 MoveStretchedAreafill(int32 mode)
{
	int32 cnt, Rotation, CompPlaced, FirstShift, NewMode;
	double OldX, OldY, CurrentX, CurrentY, ShiftX, ShiftY, x1, y1, CurrentX2, CurrentY2, SpecialDivX1, SpecialDivY1,
	       SpecialDivX2, SpecialDivY2;
	AreaFillRecord *AreaFill;
	char TempInfoStr[MAX_LENGTH_STRING];
	PolygonRecord *FirstPolygon;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;

	AreafillNrToMove = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if (((FirstPolygon->PolygonType & 2) == 2) && (AreafillNrToMove == -1))
				AreafillNrToMove = cnt;
		}
	}

	if (AreafillNrToMove == -1)
		return -1;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[AreafillNrToMove]]);
	FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

	/*
	  StartDrawingEditingWindow(0);
	  SetROP2(OutputDisplay,R2_XORPEN);
	  DrawTestPolygon(FirstPolygon,4);
	  ExitDrawing();
	  EndDrawingEditingWindow(0);
	*/

	FirstPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
	CentreSelectedX = 0.0;
	CentreSelectedY = 0.0;

	if (GetRectWindow() == 0)
		return -1;

	CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
	CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;
	
	strcpy(TempInfoStr, InfoStr);
	
	OldX = CurrentX;
	OldY = CurrentY;

	ClipMouseCursor();
	Rotation = 0;
	DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
	                      CurrentY - CurrentY2 - CentreSelectedY, 1);
	FirstShift = 1;
	CompPlaced = 0;
	SystemBusyMode = 6;
	NewMode = 1;
	DrawXorFunction.Function7 = (FUNCP7) DrawStretchedAreafill;
	DrawXorFunction.Param1[0] = &AreafillNrToMove;
	DrawXorFunction.Param1[1] = &SpecialDivX1;
	DrawXorFunction.Param1[2] = &SpecialDivX1;
	DrawXorFunction.Param1[3] = &NewMode;
	DrawXorFunction.Mode = 6;
	DrawXorFunction.Param2[0] = &AreafillNrToMove;
	DrawXorFunction.Param2[1] = &SpecialDivX2;
	DrawXorFunction.Param2[2] = &SpecialDivY2;
	DrawXorFunction.Param2[3] = &NewMode;
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
				{
					DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
					                      OldY - CurrentY2 - CentreSelectedY, 1);
				}

				if (Units == 0)
				{
					x1 = (CurrentX - CentreSelectedX - CurrentX2) / 2540.0;
					y1 = (CurrentY - CentreSelectedY - CurrentY2) / 2540.0;
					sprintf(InfoStr, "%s ( %.2f , %.2f thou )", TempInfoStr, x1, y1);
				}
				else
				{
					x1 = (CurrentX - CentreSelectedX - CurrentX2) / 100000.0;
					y1 = (CurrentY - CentreSelectedY - CurrentY2) / 100000.0;
					sprintf(InfoStr, "6%s ( %.4f , %.4f mm )", TempInfoStr, x1, y1);
				}

				RedrawInfoStr(1);
				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
				{
					DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
					                      CurrentY - CurrentY2 - CentreSelectedY, 1);
				}
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
				                      OldY - CurrentY2 - CentreSelectedY, 1);
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
				                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
				                      OldY - CurrentY2 - CentreSelectedY, 1);
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
				                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
				                      OldY - CurrentY2 - CentreSelectedY, 1);
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
				                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
				                      OldY - CurrentY2 - CentreSelectedY, 1);
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
				                      CurrentY - CurrentY2 - CentreSelectedY, 1);
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
			DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
			                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
			                      OldY - CurrentY2 - CentreSelectedY, 1);
			CompPlaced = 1;
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
			                      OldY - CurrentY2 - CentreSelectedY, 1);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
			{
				DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
				                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			}
			else
				CompPlaced = 1;
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
			                      OldY - CurrentY2 - CentreSelectedY, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
			{
				DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
				                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			}
			else
				CompPlaced = 1;
		}

		if (CheckLeftButton())
		{
			CompPlaced = 1;
			DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
			                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			PlaceStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
			                       CurrentY - CurrentY2 - CentreSelectedY, 1);
			CheckInputMessages(0);
			SelectionEsc = 1;
		}

		SpecialDivX1 = OldX - CurrentX2 - CentreSelectedX;
		SpecialDivY1 = OldY - CurrentY2 - CentreSelectedY;
		SpecialDivX2 = CurrentX - CurrentX2 - CentreSelectedX;
		SpecialDivY2 = CurrentY - CurrentY2 - CentreSelectedY;

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			RightButtonPressed = 0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawStretchedAreafill(AreafillNrToMove, OldX - CurrentX2 - CentreSelectedX,
			                      OldY - CurrentY2 - CentreSelectedY, 1);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (SpacePressed)
				SpacePressed = 0;

			if (HelpAsked)
			{
				Help("stretch_areafill.htm", 0);
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
			{
				DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
				                      CurrentY - CurrentY2 - CentreSelectedY, 1);
			}
		}
	}

	strcpy(InfoStr, TempInfoStr);
	RedrawInfoStr(1);
	DrawCrossHair(2);

	if (!CompPlaced)
	{
		DrawStretchedAreafill(AreafillNrToMove, CurrentX - CurrentX2 - CentreSelectedX,
		                      CurrentY - CurrentY2 - CentreSelectedY, 1);
	}

	UnClipMouseCursor();
	SystemBusyMode = 0;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK CopyArrayDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	double Value1 = 0.0, Value2;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetWindowTextUTF8(Dialog, SC(823, "Copy array objects"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(819, "Distance Y"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(820, "Nr copies Y"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(821, "Distance X"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(822, "Nr copies X"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogValue(Dialog, IDC_EDIT2, CopyArrayDistanceValueX);
		SetDialogValue(Dialog, IDC_EDIT1, CopyArrayDistanceValueY);
		SetDialogIntValue(Dialog, IDC_EDIT6, CopyArrayCopyNrX);
		SetDialogIntValue(Dialog, IDC_EDIT5, CopyArrayCopyNrY);
		TempUnits = Units;

		if (TempUnits == 0)
		{
			SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		}
		else
		{
			SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			Value1 = GetDialogValue(Dialog, IDC_EDIT1);
			Value2 = GetDialogValue(Dialog, IDC_EDIT2);
			TempUnits ^= 1;
			SetDialogValue(Dialog, IDC_EDIT1, Value1);
			SetDialogValue(Dialog, IDC_EDIT2, Value2);

			if (TempUnits == 0)
			{
				SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			}
			else
			{
				SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SendDlgItemMessageOwn(Dialog, IDC_EDIT4, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			}

			break;

		case IDOK:
			if ((GetDialogIntValue(Dialog, IDC_EDIT6, &CopyArrayCopyNrX) == 0) || (CopyArrayCopyNrX < 1)
			        || (CopyArrayCopyNrX > 100))
			{
				MessageBoxOwn(PCBWindow, SC(824, "Nr X copies"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if ((GetDialogIntValue(Dialog, IDC_EDIT5, &CopyArrayCopyNrY) == 0) || (CopyArrayCopyNrY < 1)
			        || (CopyArrayCopyNrY > 100))
			{
				MessageBoxOwn(PCBWindow, SC(825, "Nr Y copies"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			if ((CopyArrayCopyNrX == 1) && (CopyArrayCopyNrY == 1))
			{
				MessageBoxOwn(PCBWindow, SC(826, "Nothing to copy"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			CopyArrayDistanceValueX = GetDialogValue(Dialog, IDC_EDIT2);
			CopyArrayDistanceValueY = GetDialogValue(Dialog, IDC_EDIT1);
			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
			Help("copy_objects_array.htm", 0);
			return about;

		case IDCANCEL:
			memset(DialogTextLine, 0, 200);
			EndDialog(Dialog, 10);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 CopyArrayDialog(double *DistanceX, double *DistanceY, int32 * NrCopiesX, int32 * NrCopiesY)
{
	int res;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COPY_ARRAY1), PCBWindow,
	                 (DLGPROC) CopyArrayDialog2);
	*DistanceX = CopyArrayDistanceValueX;
	*DistanceY = CopyArrayDistanceValueY;
	*NrCopiesX = CopyArrayCopyNrX;
	*NrCopiesY = CopyArrayCopyNrY;

	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK CopyArrayRotationDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res;
	double Value1 = 0.0, Value2;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetWindowTextUTF8(Dialog, SC(823, "Copy array objects"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(827, "Rotation angle"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(828, "degrees (counter clockwise)"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(829, "Nr copies"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(830, "Rotation centre"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(831, "Rotation individual objects"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(832, "Rotate around user centre"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(204, "Rotate around coordinate centre"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(833, "Rotate individual objects"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm");
		SetDialogValue(Dialog, IDC_EDIT1, CopyArrayRotationCentreX);
		SetDialogValue(Dialog, IDC_EDIT2, CopyArrayRotationCentreY);
		SetDialogIntValue(Dialog, IDC_EDIT5, CopyArrayRotationCopyNr);
		SetDialogFloatValue(Dialog, IDC_EDIT4, CopyArrayRotationValue, 2);

		if ((CopyArrayRotationMode & 1) == 0)
		{
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, BST_CHECKED, 0);
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, BST_UNCHECKED, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
		}
		else
		{
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, BST_CHECKED, 0);
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, BST_UNCHECKED, 0);
		}

		TempUnits = Units;

		if (TempUnits == 0)
			SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
		else
			SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");

		if ((CopyArrayRotationMode & 2) == 2)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDC_RADIO1:
			SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_SETREADONLY, 1, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
			break;

		case IDC_RADIO2:
			SendDlgItemMessageOwn(Dialog, IDC_EDIT1, EM_SETREADONLY, 0, 0);
			SendDlgItemMessageOwn(Dialog, IDC_EDIT2, EM_SETREADONLY, 0, 0);
			break;

		case IDD_UNITS:
			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0)) == 1)
			{
				Value1 = GetDialogValue(Dialog, IDC_EDIT1);
				Value2 = GetDialogValue(Dialog, IDC_EDIT2);
				TempUnits ^= 1;
				SetDialogValue(Dialog, IDC_EDIT1, Value1);
				SetDialogValue(Dialog, IDC_EDIT2, Value2);

				if (TempUnits == 0)
					SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				else
					SendDlgItemMessageOwn(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			}

			break;

		case IDOK:
			if ((GetDialogIntValue(Dialog, IDC_EDIT5, &CopyArrayRotationCopyNr) == 0) || (CopyArrayRotationCopyNr < 1)
			        || (CopyArrayRotationCopyNr > 100))
			{
				MessageBoxOwn(PCBWindow, SC(829, "Nr copies"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			res = 1;

			if ((GetDialogFloatValue(Dialog, IDC_EDIT4, &CopyArrayRotationValue) == 0)
			        || (CopyArrayRotationValue == 0.0))
			{
				MessageBoxOwn(PCBWindow, SC(205, "Rotation is zero"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
				return about;
			}

			CopyArrayRotationCentreX = GetDialogValue(Dialog, IDC_EDIT1);
			CopyArrayRotationCentreY = GetDialogValue(Dialog, IDC_EDIT2);
			CopyArrayRotationMode = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0))
				CopyArrayRotationMode |= 2;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0)) == 1)
				EndDialog(Dialog, 1);

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0)) == 1)
			{
				CopyArrayRotationMode |= 1;
				EndDialog(Dialog, 1);
			}

			return about;

		case IDHELP:
			Help("copy_objects_polar.htm", 0);
			return about;

		case IDCANCEL:
			memset(DialogTextLine, 0, 200);
			EndDialog(Dialog, 10);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 CopyArrayRotationDialog(double *RotationX, double *RotationY, double *Rotation, int32 * NrCopies,
                              int32 * RotateMode)
{
	int res;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_COPY_ARRAY2), PCBWindow,
	                 (DLGPROC) CopyArrayRotationDialog2);
	*RotationX = CopyArrayRotationCentreX;
	*RotationY = CopyArrayRotationCentreY;
	*Rotation = CopyArrayRotationValue;
	*NrCopies = CopyArrayRotationCopyNr;
	*RotateMode = CopyArrayRotationMode;
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyArrayObjects(int32 mode)
{
	int32 cntx, cnty, cnt, cnt2, cnt3, TempLastActionNr, PolygonSize, count, NrCopiesX, NrCopiesY, NrCopies, RotateMode,
	      NrObjectsSelected;
	double x1, y1, minx, miny, maxx, maxy, CentreSelectedX, CentreSelectedY, DistanceX, DistanceY, RotationCentreX,
	       RotationCentreY, Rotation, RotationAngle;
	int32 Changed, Changed2;
	ObjectLineRecord *ObjectLine, ObjectLine2, NewObjectLine2, NewObjectLine3, NewObjectLine4;
	ObjectRectRecord *ObjectRect, ObjectRect2;
	ObjectArcRecord *ObjectArc, ObjectArc2;
	ObjectTextRecord2 *ObjectText2, ObjectText2a;
	ObjectPolygonRecord *ObjectPolygon, ObjectPolygon2;

	ObjectRecord NewObject;

	NrCopiesX = 0;
	NrCopiesY = 0;
	DistanceX = 0.0;
	DistanceY = 0.0;
	Rotation = 0.0;
	RotateMode = 0;
	NrCopies = 0;
	RotationCentreX = 0.0;
	RotationCentreY = 0.0;
	minx = 0.0;
	miny = 0.0;
	maxx = 0.0;
	maxy = 0.0;
	CentreSelectedX = 0.0;
	CentreSelectedY = 0.0;

	if (mode == 0)
	{
		if (CopyArrayDialog(&DistanceX, &DistanceY, &NrCopiesX, &NrCopiesY) != 1)
			return -1;
	}
	else
	{
		if (CopyArrayRotationDialog(&RotationCentreX, &RotationCentreY, &Rotation, &NrCopies, &RotateMode) != 1)
			return -1;

		if ((RotateMode & 2) == 0)
		{	// Do not rotate individual objects
			GetMaxRectSelectedObjects(0, &minx, &miny, &maxx, &maxy);
			CentreSelectedX = (minx + maxx) * 0.5;
			CentreSelectedY = (miny + maxy) * 0.5;
			NrObjectsSelected = GetNrObjectSelections(0, -1);

			if ((NrObjectsSelected == 1) && (GetNrObjectSelections(0, 4) > 0))
			{
				for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
				{
					ObjectText2 = &((*ObjectTexts2)[cnt]);

					if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						CentreSelectedX = ObjectText2->X;
						CentreSelectedY = ObjectText2->Y;
					}
				}
			}
		}

		if ((RotateMode & 1) == 0)
		{
			if ((RotateMode & 2) == 0)
			{	// Do not rotate individual objects
				if (CommandSelectPoint(&RotationCentreX, &RotationCentreY, minx, miny, maxx, maxy, 2) == 0)
					return -2;
			}
			else
			{
				if (CommandSelectPoint(&RotationCentreX, &RotationCentreY, minx, miny, maxx, maxy, 0) == 0)
					return -2;
			}
		}
	}

	CurrentObjectCode = -1;
	memset(&NewObject, 0, sizeof(NewObject));

	TempLastActionNr = (int16) LastActionNr - 1;
	Changed = 0;
	Changed2 = 0;

// *******************************************************************************************************
// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if (((ObjectLine->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectLine->AddNr <= TempLastActionNr))
		{
			ObjectLine->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectLine, ObjectLine, sizeof(ObjectLineRecord));
			memmove(&ObjectLine2, ObjectLine, sizeof(ObjectLineRecord));
			NewObjectLine.Info = 0;

			if (mode == 0)
			{
				for (cntx = 0; cntx < NrCopiesX; cntx++)
				{
					for (cnty = 0; cnty < NrCopiesY; cnty++)
					{
						if ((cntx > 0) || (cnty > 0))
						{
							NewObjectLine.X1 = (float) (ObjectLine2.X1 + cntx * DistanceX);
							NewObjectLine.Y1 = (float) (ObjectLine2.Y1 + cnty * DistanceY);
							NewObjectLine.X2 = (float) (ObjectLine2.X2 + cntx * DistanceX);
							NewObjectLine.Y2 = (float) (ObjectLine2.Y2 + cnty * DistanceY);

							if (AddObjectLine(&NewObjectLine))
								Changed = 1;
						}
					}
				}
			}
			else
			{
				RotationAngle = 0.0;

				for (cnt2 = 0; cnt2 < NrCopies; cnt2++)
				{
					RotationAngle += Rotation;

					if ((RotateMode & 2) == 0)
					{	// Do not rotate individual objects
						x1 = CentreSelectedX;
						y1 = CentreSelectedY;
						RotatePointFromOtherPoint2(&x1, &y1, RotationCentreX, RotationCentreY, RotationAngle);
						NewObjectLine.X1 = (float) (ObjectLine2.X1 + (x1 - CentreSelectedX));
						NewObjectLine.Y1 = (float) (ObjectLine2.Y1 + (y1 - CentreSelectedY));
						NewObjectLine.X2 = (float) (ObjectLine2.X2 + (x1 - CentreSelectedX));
						NewObjectLine.Y2 = (float) (ObjectLine2.Y2 + (y1 - CentreSelectedY));
					}
					else
					{	// Rotate individual objects
						NewObjectLine.X1 = (float) ObjectLine2.X1;
						NewObjectLine.Y1 = (float) ObjectLine2.Y1;
						NewObjectLine.X2 = (float) ObjectLine2.X2;
						NewObjectLine.Y2 = (float) ObjectLine2.Y2;
						RotatePointFromOtherPoint(&NewObjectLine.X1, &NewObjectLine.Y1, RotationCentreX,
						                          RotationCentreY, RotationAngle);
						RotatePointFromOtherPoint(&NewObjectLine.X2, &NewObjectLine.Y2, RotationCentreX,
						                          RotationCentreY, RotationAngle);
					}

					if (AddObjectLine(&NewObjectLine))
						Changed = 1;
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if (((ObjectRect->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectRect->AddNr <= TempLastActionNr))
		{
			ObjectRect->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectRect, ObjectRect, sizeof(ObjectRectRecord));
			memmove(&ObjectRect2, ObjectRect, sizeof(ObjectRectRecord));
			NewObjectRect.Info &= OBJECT_FILLED;

			if (mode == 0)
			{
				for (cntx = 0; cntx < NrCopiesX; cntx++)
				{
					for (cnty = 0; cnty < NrCopiesY; cnty++)
					{
						if ((cntx > 0) || (cnty > 0))
						{
							NewObjectRect.CentreX = (float) (ObjectRect2.CentreX + cntx * DistanceX);
							NewObjectRect.CentreY = (float) (ObjectRect2.CentreY + cnty * DistanceY);

							if (AddObjectRect(&NewObjectRect))
								Changed = 1;
						}
					}
				}
			}
			else
			{
				RotationAngle = 0.0;

				if ((RotateMode & 2) == 0)
				{	// Do not rotate individual objects
					for (cnt2 = 0; cnt2 < NrCopies; cnt2++)
					{
						RotationAngle += Rotation;
						x1 = CentreSelectedX;
						y1 = CentreSelectedY;
						RotatePointFromOtherPoint2(&x1, &y1, RotationCentreX, RotationCentreY, RotationAngle);
						NewObjectRect.CentreX = (float) (ObjectRect2.CentreX + (x1 - CentreSelectedX));
						NewObjectRect.CentreY = (float) (ObjectRect2.CentreY + (y1 - CentreSelectedY));

						if (AddObjectRect(&NewObjectRect))
							Changed = 1;
					}
				}
				else
				{
					if ((ObjectRect->Info & OBJECT_FILLED) == 0)
					{
						for (cnt2 = 0; cnt2 < NrCopies; cnt2++)
						{
							RotationAngle += Rotation;
							memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
							NewObjectLine.Layer = ObjectRect->Layer;
							NewObjectLine.LineThickNess = ObjectRect->LineThickNess;
							memmove(&NewObjectLine2, &NewObjectLine, sizeof(ObjectLineRecord));
							memmove(&NewObjectLine3, &NewObjectLine, sizeof(ObjectLineRecord));
							memmove(&NewObjectLine4, &NewObjectLine, sizeof(ObjectLineRecord));
							NewObjectLine.X1 = ObjectRect2.CentreX - ObjectRect2.Width * (float) 0.5;
							NewObjectLine.Y1 = ObjectRect2.CentreY - ObjectRect2.Height * (float) 0.5;
							NewObjectLine.X2 = ObjectRect2.CentreX - ObjectRect2.Width * (float) 0.5;
							NewObjectLine.Y2 = ObjectRect2.CentreY + ObjectRect2.Height * (float) 0.5;

							NewObjectLine2.X1 = ObjectRect2.CentreX - ObjectRect2.Width * (float) 0.5;
							NewObjectLine2.Y1 = ObjectRect2.CentreY + ObjectRect2.Height * (float) 0.5;
							NewObjectLine2.X2 = ObjectRect2.CentreX + ObjectRect2.Width * (float) 0.5;
							NewObjectLine2.Y2 = ObjectRect2.CentreY + ObjectRect2.Height * (float) 0.5;

							NewObjectLine3.X1 = ObjectRect2.CentreX + ObjectRect2.Width * (float) 0.5;
							NewObjectLine3.Y1 = ObjectRect2.CentreY + ObjectRect2.Height * (float) 0.5;
							NewObjectLine3.X2 = ObjectRect2.CentreX + ObjectRect2.Width * (float) 0.5;
							NewObjectLine3.Y2 = ObjectRect2.CentreY - ObjectRect2.Height * (float) 0.5;

							NewObjectLine4.X1 = ObjectRect2.CentreX + ObjectRect2.Width * (float) 0.5;
							NewObjectLine4.Y1 = ObjectRect2.CentreY - ObjectRect2.Height * (float) 0.5;
							NewObjectLine4.X2 = ObjectRect2.CentreX - ObjectRect2.Width * (float) 0.5;
							NewObjectLine4.Y2 = ObjectRect2.CentreY - ObjectRect2.Height * (float) 0.5;
							RotatePointFromOtherPoint(&NewObjectLine.X1, &NewObjectLine.Y1, RotationCentreX,
							                          RotationCentreY, RotationAngle);
							RotatePointFromOtherPoint(&NewObjectLine.X2, &NewObjectLine.Y2, RotationCentreX,
							                          RotationCentreY, RotationAngle);
							RotatePointFromOtherPoint(&NewObjectLine2.X1, &NewObjectLine2.Y1, RotationCentreX,
							                          RotationCentreY, RotationAngle);
							RotatePointFromOtherPoint(&NewObjectLine2.X2, &NewObjectLine2.Y2, RotationCentreX,
							                          RotationCentreY, RotationAngle);
							RotatePointFromOtherPoint(&NewObjectLine3.X1, &NewObjectLine3.Y1, RotationCentreX,
							                          RotationCentreY, RotationAngle);
							RotatePointFromOtherPoint(&NewObjectLine3.X2, &NewObjectLine3.Y2, RotationCentreX,
							                          RotationCentreY, RotationAngle);
							RotatePointFromOtherPoint(&NewObjectLine4.X1, &NewObjectLine4.Y1, RotationCentreX,
							                          RotationCentreY, RotationAngle);
							RotatePointFromOtherPoint(&NewObjectLine4.X2, &NewObjectLine4.Y2, RotationCentreX,
							                          RotationCentreY, RotationAngle);

							if ((AddObjectLine(&NewObjectLine)) && (AddObjectLine(&NewObjectLine2))
							        && (AddObjectLine(&NewObjectLine3)) && (AddObjectLine(&NewObjectLine4)))
								Changed = 1;
						}
					}
					else
					{
						for (cnt2 = 0; cnt2 < NrCopies; cnt2++)
						{
							RotationAngle += Rotation;
							memset(&NewObjectPolygon, 0, sizeof(ObjectPolygonRecord));
							NewObjectPolygon.NrVertices = 4;
							NewObjectPolygon.Layer = ObjectRect2.Layer;
							NewObjectPolygon.Points[0].x = ObjectRect2.CentreX - ObjectRect2.Width * (float) 0.5;
							NewObjectPolygon.Points[0].y = ObjectRect2.CentreY - ObjectRect2.Height * (float) 0.5;
							NewObjectPolygon.Points[1].x = ObjectRect2.CentreX - ObjectRect2.Width * (float) 0.5;
							NewObjectPolygon.Points[1].y = ObjectRect2.CentreY + ObjectRect2.Height * (float) 0.5;
							NewObjectPolygon.Points[2].x = ObjectRect2.CentreX + ObjectRect2.Width * (float) 0.5;
							NewObjectPolygon.Points[2].y = ObjectRect2.CentreY + ObjectRect2.Height * (float) 0.5;
							NewObjectPolygon.Points[3].x = ObjectRect2.CentreX + ObjectRect2.Width * (float) 0.5;
							NewObjectPolygon.Points[3].y = ObjectRect2.CentreY - ObjectRect2.Height * (float) 0.5;

							RotateObjectPolygon(&NewObjectPolygon, RotationCentreX, RotationCentreY, RotationAngle, 0);
							SetMinMaxObjectPolygon(&NewObjectPolygon, 0);

							if (AddObjectPolygon(&NewObjectPolygon))
								Changed = 1;
						}
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	/*
	  for (cnt=0;cnt<Design.NrObjectCircles;cnt++) {
	    ObjectCircle=&((*ObjectCircles)[cnt]);
	    if (((ObjectCircle->Info & (OBJECT_NOT_VISIBLE|OBJECT_SELECTED)) == OBJECT_SELECTED)
	       &&
	       (ObjectCircle->AddNr<=TempLastActionNr)) {
	      ObjectCircle->Info&=~OBJECT_SELECTED;
	      DrawObjectCircle(ObjectCircle,0.0,0.0,0);
	      memmove(&NewObjectCircle,ObjectCircle,sizeof(ObjectCircleRecord));
	      memmove(&ObjectCircle2,ObjectCircle,sizeof(ObjectCircleRecord));
	      NewObjectCircle.Info&=OBJECT_FILLED;
	      if (mode==0) {
	        for (cntx=0;cntx<NrCopiesX;cntx++) {
	          for (cnty=0;cnty<NrCopiesY;cnty++) {
	            if ((cntx>0)
	               ||
	               (cnty>0)) {
	              NewObjectCircle.CentreX=ObjectCircle2.CentreX+cntx*DistanceX;
	              NewObjectCircle.CentreY=ObjectCircle2.CentreY+cnty*DistanceY;
	              if (AddObjectCircle(&NewObjectCircle)) {
	                DrawObjectCircle(&NewObjectCircle,0.0,0.0,0);
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
	        }
	      } else {
	        RotationAngle=0.0;
	        for (cnt2=0;cnt2<NrCopies;cnt2++) {
	          RotationAngle+=Rotation;
	          if (((RotateMode & 2) == 0)  // Do not rotate individual objects
	             ||
	             (ObjectCircle2.CircleMode == 15)
	             ||
	             (((ObjectCircle2.Info & OBJECT_FILLED) != 0)
	             &&
	             ((ObjectCircle2.Layer==DRILL_UNPLATED_LAYER)
	             ||
	             (ObjectCircle2.Layer==DRILL_LAYER)))) {
	            if ((RotateMode & 2) == 0) { // Do not rotate individual objects
	              x1=CentreSelectedX;
	              y1=CentreSelectedY;
	              RotatePointFromOtherPoint2(&x1,&y1,RotationCentreX,RotationCentreY,RotationAngle);
	              NewObjectCircle.CentreX=ObjectCircle2.CentreX+(x1-CentreSelectedX);
	              NewObjectCircle.CentreY=ObjectCircle2.CentreY+(y1-CentreSelectedY);
	            } else {
	              NewObjectCircle.CentreX=ObjectCircle2.CentreX;
	              NewObjectCircle.CentreY=ObjectCircle2.CentreY;
	              RotatePointFromOtherPoint2(&NewObjectCircle.CentreX,&NewObjectCircle.CentreY,
	                                        RotationCentreX,RotationCentreY,RotationAngle);
	            }
	            if (AddObjectCircle(&NewObjectCircle)) {
	              DrawObjectCircle(&NewObjectCircle,0.0,0.0,0);
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
	            NewObjectArc.Layer=ObjectCircle2.Layer;
	            NewObjectArc.CentreX=ObjectCircle2.CentreX;
	            NewObjectArc.CentreY=ObjectCircle2.CentreY;
	            NewObjectArc.Width=ObjectCircle2.Diam;
	            NewObjectArc.Height=ObjectCircle2.Diam;
	            NewObjectArc.LineThickNess=ObjectCircle2.LineThickNess;
	            switch (ObjectCircle2.CircleMode) {
	              case 1:
	                NewObjectArc.StartDiffX=ObjectCircle2.Diam;
	                NewObjectArc.StartDiffY=0.0;
	                NewObjectArc.EndDiffX=0.0;
	                NewObjectArc.EndDiffY=ObjectCircle2.Diam;
	                break;
	              case 2:
	                NewObjectArc.StartDiffX=0.0;
	                NewObjectArc.StartDiffY=-ObjectCircle2.Diam;
	                NewObjectArc.EndDiffX=ObjectCircle2.Diam;
	                NewObjectArc.EndDiffY=0.0;
	                break;
	              case 3:
	                NewObjectArc.StartDiffX=0.0;
	                NewObjectArc.StartDiffY=-ObjectCircle2.Diam;
	                NewObjectArc.EndDiffX=0.0;
	                NewObjectArc.EndDiffY=ObjectCircle2.Diam;
	                break;
	              case 4:
	                NewObjectArc.StartDiffX=-ObjectCircle2.Diam;
	                NewObjectArc.StartDiffY=0.0;
	                NewObjectArc.EndDiffX=0.0;
	                NewObjectArc.EndDiffY=-ObjectCircle2.Diam;
	                break;
	              case 6:
	                NewObjectArc.StartDiffX=-ObjectCircle2.Diam;
	                NewObjectArc.StartDiffY=0.0;
	                NewObjectArc.EndDiffX=ObjectCircle2.Diam;
	                NewObjectArc.EndDiffY=0.0;
	                break;
	              case 8:
	                NewObjectArc.StartDiffX=0.0;
	                NewObjectArc.StartDiffY=ObjectCircle2.Diam;
	                NewObjectArc.EndDiffX=-ObjectCircle2.Diam;
	                NewObjectArc.EndDiffY=0.0;
	                break;
	              case 9:
	                NewObjectArc.StartDiffX=ObjectCircle2.Diam;
	                NewObjectArc.StartDiffY=0.0;
	                NewObjectArc.EndDiffX=-ObjectCircle2.Diam;
	                NewObjectArc.EndDiffY=0.0;
	                break;
	              case 12:
	                NewObjectArc.StartDiffX=0.0;
	                NewObjectArc.StartDiffY=ObjectCircle2.Diam;
	                NewObjectArc.EndDiffX=0.0;
	                NewObjectArc.EndDiffY=-ObjectCircle2.Diam;
	                break;
	            }
	            RotatePointFromOtherPoint2(&NewObjectArc.CentreX,&NewObjectArc.CentreY,
	                                      RotationCentreX,RotationCentreY,RotationAngle);
	            RotatePointFromOtherPoint2(&NewObjectArc.StartDiffX,&NewObjectArc.StartDiffY,
	                                      0.0,0.0,RotationAngle);
	            RotatePointFromOtherPoint2(&NewObjectArc.EndDiffX,&NewObjectArc.EndDiffY,
	                                      0.0,0.0,RotationAngle);
	            if (AddObjectArc(&NewObjectArc)) {
	              DrawObjectArc(&NewObjectArc,0.0,0.0,0);
	              Changed=1;
	            }
	          }
	        }
	      }
	    }
	  }
	*/
// *******************************************************************************************************
// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectArc->AddNr <= TempLastActionNr))
		{
			ObjectArc->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectArc, ObjectArc, sizeof(ObjectArcRecord));
			memmove(&ObjectArc2, ObjectArc, sizeof(ObjectArcRecord));
			NewObjectArc.Info = 0;

			if (mode == 0)
			{
				for (cntx = 0; cntx < NrCopiesX; cntx++)
				{
					for (cnty = 0; cnty < NrCopiesY; cnty++)
					{
						if ((cntx > 0) || (cnty > 0))
						{
							NewObjectArc.CentreX = (float) (ObjectArc2.CentreX + cntx * DistanceX);
							NewObjectArc.CentreY = (float) (ObjectArc2.CentreY + cnty * DistanceY);

							if (AddObjectArc(&NewObjectArc))
								Changed = 1;
						}
					}
				}
			}
			else
			{
				RotationAngle = 0.0;

				for (cnt2 = 0; cnt2 < NrCopies; cnt2++)
				{
					RotationAngle += Rotation;

					if ((RotateMode & 2) == 0)
					{	// Do not rotate individual objects
						x1 = CentreSelectedX;
						y1 = CentreSelectedY;
						RotatePointFromOtherPoint2(&x1, &y1, RotationCentreX, RotationCentreY, RotationAngle);
						NewObjectArc.CentreX = (float) (ObjectArc2.CentreX + (x1 - CentreSelectedX));
						NewObjectArc.CentreY = (float) (ObjectArc2.CentreY + (y1 - CentreSelectedY));
					}
					else
					{
						NewObjectArc.CentreX = ObjectArc2.CentreX;
						NewObjectArc.CentreY = ObjectArc2.CentreY;
						NewObjectArc.StartDiffX = ObjectArc2.StartDiffX;
						NewObjectArc.StartDiffY = ObjectArc2.StartDiffY;
						NewObjectArc.EndDiffX = ObjectArc2.EndDiffX;
						NewObjectArc.EndDiffY = ObjectArc2.EndDiffY;
						RotatePointFromOtherPoint(&NewObjectArc.CentreX, &NewObjectArc.CentreY, RotationCentreX,
						                          RotationCentreY, RotationAngle);
						RotatePointFromOtherPoint(&NewObjectArc.StartDiffX, &NewObjectArc.StartDiffY, 0.0, 0.0,
						                          RotationAngle);
						RotatePointFromOtherPoint(&NewObjectArc.EndDiffX, &NewObjectArc.EndDiffY, 0.0, 0.0,
						                          RotationAngle);
					}

					if (AddObjectArc(&NewObjectArc))
						Changed = 1;
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->AddNr <= TempLastActionNr))
		{
			ObjectText2->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));
			memmove(&ObjectText2a, ObjectText2, sizeof(ObjectTextRecord2));
			NewObjectText2.Info = 0;

			if (mode == 0)
			{
				for (cntx = 0; cntx < NrCopiesX; cntx++)
				{
					for (cnty = 0; cnty < NrCopiesY; cnty++)
					{
						if ((cntx > 0) || (cnty > 0))
						{
							NewObjectText2.X = (float) (ObjectText2a.X + cntx * DistanceX);
							NewObjectText2.Y = (float) (ObjectText2a.Y + cnty * DistanceY);

							if (AddObjectText2(&NewObjectText2))
								Changed = 1;
						}
					}
				}
			}
			else
			{
				RotationAngle = 0.0;

				for (cnt2 = 0; cnt2 < NrCopies; cnt2++)
				{
					RotationAngle += Rotation;

					if ((RotateMode & 2) == 0)
					{	// Do not rotate individual objects
						x1 = CentreSelectedX;
						y1 = CentreSelectedY;
						RotatePointFromOtherPoint2(&x1, &y1, RotationCentreX, RotationCentreY, RotationAngle);
						NewObjectText2.X = (float) (ObjectText2a.X + (x1 - CentreSelectedX));
						NewObjectText2.Y = (float) (ObjectText2a.Y + (y1 - CentreSelectedY));
					}
					else
					{
						NewObjectText2.X = (float) ObjectText2a.X;
						NewObjectText2.Y = (float) ObjectText2a.Y;
						RotatePointFromOtherPoint(&NewObjectText2.X, &NewObjectText2.Y, RotationCentreX,
						                          RotationCentreY, RotationAngle);
						NewObjectText2.Rotation = ObjectText2a.Rotation + (float) RotationAngle;
					}

					if (AddObjectText2(&NewObjectText2))
						Changed = 1;
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectPolygon->AddNr <= TempLastActionNr))
		{
			ObjectPolygon->Info &= ~OBJECT_SELECTED;
			PolygonSize = MemSizeObjectPolygon(ObjectPolygon);
			count = ObjectPolygon->NrVertices;
			memmove(&NewObjectPolygon, ObjectPolygon, PolygonSize);
			memmove(&ObjectPolygon2, ObjectPolygon, PolygonSize);

			if (mode == 0)
			{
				for (cntx = 0; cntx < NrCopiesX; cntx++)
				{
					for (cnty = 0; cnty < NrCopiesY; cnty++)
					{
						if ((cntx > 0) || (cnty > 0))
						{
							memmove(&NewObjectPolygon, &ObjectPolygon2, PolygonSize);
							MoveObjectPolygon(&NewObjectPolygon, cntx * DistanceX, cnty * DistanceY, 0);

							if (AddObjectPolygon(&NewObjectPolygon))
								Changed = 1;
						}
					}
				}
			}
			else
			{
				RotationAngle = 0.0;

				for (cnt2 = 0; cnt2 < NrCopies; cnt2++)
				{
					RotationAngle += Rotation;
					memmove(&NewObjectPolygon.Points, &ObjectPolygon2.Points, count * sizeof(PointRecord));

					if ((RotateMode & 2) == 0)
					{	// Do not rotate individual objects
						x1 = CentreSelectedX;
						y1 = CentreSelectedY;
						RotatePointFromOtherPoint2(&x1, &y1, RotationCentreX, RotationCentreY, RotationAngle);
						MoveObjectPolygon(&NewObjectPolygon, (x1 - CentreSelectedX), (y1 - CentreSelectedY), 0);
					}
					else
					{
						for (cnt3 = 0; cnt3 < count; cnt3++)
						{
							RotatePointFromOtherPoint2(&NewObjectPolygon.Points[cnt3].x,
							                           &NewObjectPolygon.Points[cnt3].y, RotationCentreX,
							                           RotationCentreY, RotationAngle);
						}
					}

					if (AddObjectPolygon(&NewObjectPolygon))
						Changed = 1;
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

void MirrorObjects(int32 mode)
{
	int32 cnt, TempLastActionNr, PolygonSize, count, cnt3;
	double minx, maxx, miny, maxy;
	int32 Changed;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectTextRecord2 *ObjectText2;
	ObjectRecord NewObject;

	GetMaxRectSelectedObjects(0, &minx, &miny, &maxx, &maxy);
	CentreSelectedX = AdjustToGrid((minx + maxx) * 0.5, GridSize);
	CentreSelectedY = AdjustToGrid((miny + maxy) * 0.5, GridSize);

	CurrentObjectCode = -1;
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

			if (mode == 0)
			{	// Horizontal mirror
				NewObjectLine.X1 += (float) (2.0 * (CentreSelectedX - NewObjectLine.X1));
				NewObjectLine.X2 += (float) (2.0 * (CentreSelectedX - NewObjectLine.X2));
			}
			else
			{	// Vertical mirror
				NewObjectLine.Y1 += (float) (2.0 * (CentreSelectedY - NewObjectLine.Y1));
				NewObjectLine.Y2 += (float) (2.0 * (CentreSelectedY - NewObjectLine.Y2));
			}

			NewObjectLine.Info = OBJECT_SELECTED | 3;

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

			if (mode == 0)
			{	// Horizontal mirror
				NewObjectRect.CentreX += (float) (2.0 * (CentreSelectedX - NewObjectRect.CentreX));
			}
			else
			{	// Vertical mirror
				NewObjectRect.CentreY += (float) (2.0 * (CentreSelectedY - NewObjectRect.CentreY));
			}

			NewObjectRect.Info &= OBJECT_FILLED;
			NewObjectRect.Info |= OBJECT_SELECTED;

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
	      if (mode==0) { // Horizontal mirror
	        NewObjectCircle.CentreX+=2*(CentreSelectedX-NewObjectCircle.CentreX);
	        NewObjectCircle.CircleMode=CircleMirrorX[(int32)ObjectCircle->CircleMode];
	      } else {       // Vertical mirror
	        NewObjectCircle.CentreY+=2*(CentreSelectedY-NewObjectCircle.CentreY);
	        NewObjectCircle.CircleMode=CircleMirrorY[(int32)ObjectCircle->CircleMode];
	      }
	      NewObjectCircle.Info&=OBJECT_FILLED;
	      NewObjectCircle.Info|=OBJECT_SELECTED;
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

			if (mode == 0)
			{	// Horizontal mirror
				NewObjectArc.CentreX += (float) (2.0 * (CentreSelectedX - NewObjectArc.CentreX));
				NewObjectArc.StartDiffX = -ObjectArc->EndDiffX;
				NewObjectArc.StartDiffY = ObjectArc->EndDiffY;
				NewObjectArc.EndDiffX = -ObjectArc->StartDiffX;
				NewObjectArc.EndDiffY = ObjectArc->StartDiffY;
			}
			else
			{	// Vertical mirror
				NewObjectArc.CentreY += (float) (2.0 * (CentreSelectedY - NewObjectArc.CentreY));
				NewObjectArc.StartDiffX = ObjectArc->EndDiffX;
				NewObjectArc.StartDiffY = -ObjectArc->EndDiffY;
				NewObjectArc.EndDiffX = ObjectArc->StartDiffX;
				NewObjectArc.EndDiffY = -ObjectArc->StartDiffY;
			}

			NewObjectArc.Info = OBJECT_SELECTED;

			if (AddObjectArc(&NewObjectArc))
			{
				ObjectArc = &((*ObjectArcs)[cnt]);
				ObjectArc->Info |= OBJECT_NOT_VISIBLE;
				ObjectArc->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if (((ObjectText2->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (ObjectText2->AddNr <= TempLastActionNr))
		{
			ObjectText2->Info &= ~OBJECT_SELECTED;
			memmove(&NewObjectText2, ObjectText2, sizeof(ObjectTextRecord2));

			if (mode == 0)
			{	// Horizontal mirror
				NewObjectText2.X += (float) (2.0 * (CentreSelectedX - NewObjectText2.X));
		        NewObjectText2.TextMode ^= 0x10;
			}
			else
			{	// Vertical mirror
				NewObjectText2.Y += (float) (2.0 * (CentreSelectedY - NewObjectText2.Y));
				NewObjectText2.TextMode ^= 0x10;
				NewObjectText2.Rotation += (float) 180;
			}

			NewObjectText2.Info = OBJECT_SELECTED;

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

			count = ObjectPolygon->NrVertices;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				if (mode == 0)
				{	// Horizontal mirror
					NewObjectPolygon.Points[cnt3].x += 2 * (CentreSelectedX - NewObjectPolygon.Points[cnt3].x);
				}
				else
				{	// Vertical mirror
					NewObjectPolygon.Points[cnt3].y += 2 * (CentreSelectedY - NewObjectPolygon.Points[cnt3].y);
				}
			}

			NewObjectPolygon.Info = OBJECT_SELECTED;

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
	return;
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AreafillToObjectLines(AreaFillRecord * AreaFill, int32 ObjectLayer1, PolygonRecord * TestPolygon,
                           int32 ObjectLayer2, int32 mode)
{

	int32 cnt, cnt3, count;
	double *FirstX, *FirstY, *dx1, *dy1, *dx2, *dy2;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;
	ObjectLineRecord NewObjectLine;
	ObjectTextRecord2 NewObjectText2;

	memset(&NewObjectLine, 0, sizeof(NewObjectLine));
	NewObjectLine.Layer = ObjectLayer1;
	NewObjectLine.LineThickNess = 1.0;
	memset(&NewObjectText2, 0, sizeof(NewObjectText2));
	NewObjectText2.Layer = ObjectLayer1;
	NewObjectText2.LineThickNess = 0.01e5;
	NewObjectText2.FontHeight = 0.1e5;
	AreaPos = (uint8 *) AreaFill;

	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt = 0; cnt < AreaFill->NrPolygons; cnt++)
	{
		count = DrawPolygon->NrVertices;

		dx1 = (double *) &((*DrawPolygon).Points);
		dy1 = dx1 + 1;

		if ((mode & 7) == 1)
		{
			sprintf(NewObjectText2.Text, "%d , %d", cnt, DrawPolygon->PolygonType & 15);
			NewObjectText2.X = (float) *dx1;
			NewObjectText2.Y = (float) *dy1;
			AddObjectText2(&NewObjectText2);
		}

		if ((mode & 7) == 2)
		{
			sprintf(NewObjectText2.Text, "%d", cnt);
			NewObjectText2.X = (float) *dx1;
			NewObjectText2.Y = (float) *dy1;
			AddObjectText2(&NewObjectText2);
		}

		FirstX = dx1;
		FirstY = dy1;

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			if (cnt3 < count - 1)
			{
				dx2 = dx1 + 2;
				dy2 = dy1 + 2;
			}
			else
			{
				dx2 = FirstX;
				dy2 = FirstY;
			}

			NewObjectLine.X1 = (float) *dx1;
			NewObjectLine.Y1 = (float) *dy1;
			NewObjectLine.X2 = (float) *dx2;
			NewObjectLine.Y2 = (float) *dy2;
			AddObjectLine(&NewObjectLine);
			dx1 += 2;
			dy1 += 2;
		}

		PolygonPos += sizeof(PolygonInitRecord) + count * sizeof(PointRecord);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	NewObjectLine.Layer = ObjectLayer2;

	if (!TestPolygon)
		return;

	count = TestPolygon->NrVertices;
	dx1 = (double *) &((*TestPolygon).Points);
	dy1 = dx1 + 1;
	FirstX = dx1;
	FirstY = dy1;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		if (cnt3 < count - 1)
		{
			dx2 = dx1 + 2;
			dy2 = dy1 + 2;
		}
		else
		{
			dx2 = FirstX;
			dy2 = FirstY;
		}

		NewObjectLine.X1 = (float) *dx1;
		NewObjectLine.Y1 = (float) *dy1;
		NewObjectLine.X2 = (float) *dx2;
		NewObjectLine.Y2 = (float) *dy2;
		AddObjectLine(&NewObjectLine);
		dx1 += 2;
		dy1 += 2;
	}

}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PolygonToObjectLines(PolygonRecord * DrawPolygon, int32 ObjectLayer1, PolygonRecord * TestPolygon,
                          int32 ObjectLayer2, int32 mode)
{

	int32 cnt3, count;
	double *FirstX, *FirstY, *dx1, *dy1, *dx2, *dy2;
	ObjectLineRecord NewObjectLine;

	memset(&NewObjectLine, 0, sizeof(NewObjectLine));
	NewObjectLine.Layer = ObjectLayer1;
	NewObjectLine.LineThickNess = 1.0;

	count = DrawPolygon->NrVertices;

	dx1 = (double *) &((*DrawPolygon).Points);
	dy1 = dx1 + 1;
	FirstX = dx1;
	FirstY = dy1;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		if (cnt3 < count - 1)
		{
			dx2 = dx1 + 2;
			dy2 = dy1 + 2;
		}
		else
		{
			dx2 = FirstX;
			dy2 = FirstY;
		}

		NewObjectLine.X1 = (float) *dx1;
		NewObjectLine.Y1 = (float) *dy1;
		NewObjectLine.X2 = (float) *dx2;
		NewObjectLine.Y2 = (float) *dy2;
		AddObjectLine(&NewObjectLine);
		dx1 += 2;
		dy1 += 2;
	}

	NewObjectLine.Layer = ObjectLayer2;

	if (!TestPolygon)
		return;

	count = TestPolygon->NrVertices;
	dx1 = (double *) &((*TestPolygon).Points);
	dy1 = dx1 + 1;
	FirstX = dx1;
	FirstY = dy1;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		if (cnt3 < count - 1)
		{
			dx2 = dx1 + 2;
			dy2 = dy1 + 2;
		}
		else
		{
			dx2 = FirstX;
			dy2 = FirstY;
		}

		NewObjectLine.X1 = (float) *dx1;
		NewObjectLine.Y1 = (float) *dy1;
		NewObjectLine.X2 = (float) *dx2;
		NewObjectLine.Y2 = (float) *dy2;
		AddObjectLine(&NewObjectLine);
		dx1 += 2;
		dy1 += 2;
	}

}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PolygonVerticesToMessage(PolygonRecord * DrawPolygon, PolygonRecord * DrawPolygon2)
{

	int32 cnt3, count;
	char str[MAX_LENGTH_STRING];

	count = DrawPolygon->NrVertices;
	MessageBufPos = 0;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		sprintf(str, "[%d] %.12f,%.12f", cnt3, (*DrawPolygon).Points[cnt3].x / 1e5,
		        (*DrawPolygon).Points[cnt3].y / 1e5);
		AddToMessageBuf(str);
	}

	if (DrawPolygon2)
	{
		count = DrawPolygon2->NrVertices;
		AddToMessageBuf(SC(834, "\r\nSecond polygon\r\n"));

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			sprintf(str, "[%d] %.12f,%.12f", cnt3, (*DrawPolygon2).Points[cnt3].x / 1e5,
			        (*DrawPolygon2).Points[cnt3].y / 1e5);
			AddToMessageBuf(str);
		}
	}

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(1, "Message"), 0, 0);
		DeAllocateMemMessageBuf();
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void CopyAreafillStartPolygonToInfo4(int32 mode)
{
	int32 cnt, count, FoundAreafillNr;
	AreaFillRecord *AreaFill;
	PolygonRecord *SubPolygon;

	FoundAreafillNr = -1;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((AreaFill->Info & (OBJECT_NOT_VISIBLE | POWERPLANE)) == 0)
		{
			SubPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));

			if ((SubPolygon->PolygonType & 2) == 2)
			{
				FoundAreafillNr = cnt;
				break;
			}
		}
	}

	if (FoundAreafillNr == -1)
		return;

	AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[FoundAreafillNr]]);

	count = AreaFill->NrVerticesStartPolygon;
	memset(&NewObjectPolygon, 0, sizeof(NewObjectPolygon));
	NewObjectPolygon.NrVertices = count;
	NewObjectPolygon.Layer = INFO_LAYER4;

	for (cnt = 0; cnt < count; cnt++)
	{
		NewObjectPolygon.Points[cnt].x = AreaFill->StartPolygon[cnt].x;
		NewObjectPolygon.Points[cnt].y = AreaFill->StartPolygon[cnt].y;
	}

	AddObjectPolygon(&NewObjectPolygon);
	RePaint();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
