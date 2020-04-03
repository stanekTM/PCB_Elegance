/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: odb2.c
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
#include "stdio.h"
#include "memory.h"
#include "calcdef.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "plot.h"
#include "odb2.h"
#include "polygon.h"
#include "math.h"
#include "files2.h"

/*
typedef struct {
          int32 AperTureNr,AperTureCode,
                Info,Info2,Info3,Info4,
                Used,Used2,Mirror,SpecialType;
          double x,y,x2,y2,x3,Rotation,HoleSize;
          uint8  *Address;
        } AperTureRecord ;
*/

AperTureRecord OdbLastRoundPadAperTure, OdbLastRectPadAperTure, OdbLastPolygonPadAperTure, OdbLastDrillAperTure,
               OdbLastThermalReliefAperTure, NewAperTure;

int32 ok, NrOdbApertureMacroObjects, MaxNrOdbApertureMacroObjects, ActivePolygonVertices;
double ActiveRotation;
ApertureMacroObjectObjectRecord *ApertureMacroObjects;
char ActiveComp[200], ActiveShape[200];

extern double CurrentPlotPenSize;
extern int32 MaxNrAperTures, NrAperTures, NrDrillAperTures;
extern int32 ChangeSilkScreenPen, ChangeSilkScreenPenAsk;
extern HWND PCBWindow;
extern int32 OdbPrepareErrors;
extern double DesignBoardOriginX, DesignBoardOriginY, DesignBoardWidth, DesignBoardHeight;
extern AperTureArray *AperTures, *AperTures2, *DrillAperTures;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 OdbCheckRoundPadAperTure(double ThickNess)
{
	int32 cnt;
	AperTureRecord *AperTure;

	if (ThickNess < 100.0)
		ok = 1;

	if ((NrAperTures > 0) && (InRangeSpecial(OdbLastRoundPadAperTure.x, ThickNess, 100.0)))
	{
		AperTure = &((*AperTures)[OdbLastRoundPadAperTure.AperTureNr]);
		return OdbLastRoundPadAperTure.AperTureNr;
	}

	cnt = 0;

	while ((cnt < NrAperTures)
	        && (((*AperTures)[cnt].Info != ODB_PAD_ROUND) || (NotInRangeSpecial((*AperTures)[cnt].x, ThickNess, 100.0))))
		cnt++;

	if (cnt < NrAperTures)
		AperTure = &((*AperTures)[cnt]);
	else
	{
		if (NrAperTures + 1 >= MaxNrAperTures)
		{
			if (AllocateMemAperTures(NrAperTures + 64) != 0)
				return -1;
		}

		AperTure = &((*AperTures)[cnt]);
		AperTure->Info = ODB_PAD_ROUND;
		AperTure->Info3 = 0;
		AperTure->x = ThickNess;
		AperTure->y = 0.0;
		AperTure->x2 = 0.0;
		AperTure->y2 = 0.0;
		AperTure->AperTureNr = cnt;

		if (NrAperTures == 0)
		{
			OdbLastThermalReliefAperTure.x = 0.0;
			OdbLastThermalReliefAperTure.y = 0.0;
			OdbLastThermalReliefAperTure.x2 = 0.0;
			OdbLastRectPadAperTure.x = 0.0;
			OdbLastRectPadAperTure.y = 0.0;
			OdbLastPolygonPadAperTure.Address = NULL;
			OdbLastPolygonPadAperTure.x2 = 0.0;
			OdbLastPolygonPadAperTure.y2 = 0.0;
			OdbLastPolygonPadAperTure.SpecialType = 0;
		}

		NrAperTures++;
	}

	OdbLastRoundPadAperTure.x = ThickNess;
	OdbLastRoundPadAperTure.AperTureNr = cnt;
	return cnt;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 OdbCheckThermalReliefAperTure(double InsideDiameter, double OutsideDiameter, double CrossHairThickness)
{
	int32 cnt;
	AperTureRecord *AperTure;

	if ((NrAperTures > 0) && (InRangeSpecial(OdbLastThermalReliefAperTure.x, InsideDiameter, 100.0))
	        && (InRangeSpecial(OdbLastThermalReliefAperTure.y, OutsideDiameter, 100.0))
	        && (InRangeSpecial(OdbLastThermalReliefAperTure.x2, CrossHairThickness, 100.0)))
	{
		AperTure = &((*AperTures)[OdbLastThermalReliefAperTure.AperTureNr]);
		return OdbLastThermalReliefAperTure.AperTureNr;
	}

	cnt = 0;

	while ((cnt < NrAperTures)
	        && (((*AperTures)[cnt].Info != ODB_PAD_THERMAL_RELIEF)
	            || (NotInRangeSpecial((*AperTures)[cnt].x, InsideDiameter, 100.0))
	            || (NotInRangeSpecial((*AperTures)[cnt].y, OutsideDiameter, 100.0))
	            || (NotInRangeSpecial((*AperTures)[cnt].x2, CrossHairThickness, 100.0))))
		cnt++;

	if (cnt < NrAperTures)
		AperTure = &((*AperTures)[cnt]);
	else
	{
		if (NrAperTures + 1 >= MaxNrAperTures)
		{
			if (AllocateMemAperTures(NrAperTures + 64) != 0)
				return -1;
		}

		AperTure = &((*AperTures)[cnt]);
		AperTure->Info = ODB_PAD_THERMAL_RELIEF;
		AperTure->Info3 = 0;
		AperTure->x = InsideDiameter;
		AperTure->y = OutsideDiameter;
		AperTure->x2 = CrossHairThickness;
		AperTure->y2 = 0.0;
		AperTure->AperTureNr = cnt;

		if (NrAperTures == 0)
		{
			OdbLastRoundPadAperTure.x = 0.0;
			OdbLastRectPadAperTure.x = 0.0;
			OdbLastRectPadAperTure.y = 0.0;
			OdbLastPolygonPadAperTure.Address = NULL;
			OdbLastPolygonPadAperTure.x2 = 0.0;
			OdbLastPolygonPadAperTure.y2 = 0.0;
			OdbLastPolygonPadAperTure.SpecialType = 0;
		}

		NrAperTures++;
	}

	OdbLastThermalReliefAperTure.x = InsideDiameter;
	OdbLastThermalReliefAperTure.y = OutsideDiameter;
	OdbLastThermalReliefAperTure.x2 = CrossHairThickness;
	OdbLastThermalReliefAperTure.AperTureNr = cnt;
	return cnt;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 OdbCheckRectPadAperTure(double x, double y)
{
	int32 cnt;
	AperTureRecord *AperTure;

	if (x < 100.0)
		ok = 1;

	if (y < 100.0)
		ok = 1;

	if ((NrAperTures > 0) && (InRangeSpecial(OdbLastRectPadAperTure.x, x, 100.0))
	        && (InRangeSpecial(OdbLastRectPadAperTure.y, y, 100.0)))
	{
		AperTure = &((*AperTures)[OdbLastRectPadAperTure.AperTureNr]);
		AperTure->Used++;
		return OdbLastRectPadAperTure.AperTureNr;
	}

	cnt = 0;

	while ((cnt < NrAperTures)
	        && (((*AperTures)[cnt].Info != ODB_PAD_RECT) || (NotInRangeSpecial((*AperTures)[cnt].x, x, 100.0))
	            || (NotInRangeSpecial((*AperTures)[cnt].y, y, 100.0))))
		cnt++;

	if (cnt < NrAperTures)
		AperTure = &((*AperTures)[cnt]);
	else
	{
		if (NrAperTures + 1 >= MaxNrAperTures)
		{
			if (AllocateMemAperTures(NrAperTures + 64) != 0)
				return -1;
		}

		AperTure = &((*AperTures)[cnt]);
#ifdef _DEBUG

		if (cnt == 1)
			ok = 1;

#endif
		AperTure->Info = ODB_PAD_RECT;
		AperTure->x = x;
		AperTure->y = y;
		AperTure->x2 = 0.0;
		AperTure->y2 = 0.0;
		AperTure->AperTureNr = cnt;
		AperTure->Info3 = 0;

		if (NrAperTures == 0)
		{
			OdbLastRectPadAperTure.AperTureNr = 0;
			OdbLastRectPadAperTure.x = x;
			OdbLastRectPadAperTure.y = y;
			OdbLastThermalReliefAperTure.x = 0.0;
			OdbLastRoundPadAperTure.x = 0.0;
			OdbLastPolygonPadAperTure.Address = NULL;
			OdbLastPolygonPadAperTure.x2 = 0.0;
			OdbLastPolygonPadAperTure.y2 = 0.0;
			OdbLastPolygonPadAperTure.SpecialType = 0;
		}

		NrAperTures++;
	}

	OdbLastRectPadAperTure.x = x;
	OdbLastRectPadAperTure.y = y;
	OdbLastRectPadAperTure.AperTureNr = cnt;
	return cnt;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 OdbCheckPolygonAperTure(int32 ObjectType, double x2, double y2, double Rotation, int32 Mirror, uint8 * Address)
{
	int32 cnt, cnt2, Found, Found2;
	AperTureRecord *AperTure;

	if (NrAperTures > 0)
	{
		if (ObjectType == 0)
		{
			if ((InRangeSpecial(OdbLastPolygonPadAperTure.Rotation, Rotation, 0.01))
			        && (OdbLastPolygonPadAperTure.SpecialType == 0) && (OdbLastPolygonPadAperTure.Mirror == Mirror)
			        && (OdbLastPolygonPadAperTure.Address == Address))
			{
				AperTure = &((*AperTures)[OdbLastPolygonPadAperTure.AperTureNr]);
				return OdbLastPolygonPadAperTure.AperTureNr;
			}
		}
		else
		{
			if ((InRangeSpecial(OdbLastPolygonPadAperTure.Rotation, Rotation, 0.01))
			        && (OdbLastPolygonPadAperTure.Mirror == Mirror)
			        && (InRangeSpecial(OdbLastPolygonPadAperTure.x2, x2, 10.0))
			        && (InRangeSpecial(OdbLastPolygonPadAperTure.y2, y2, 10.0))
			        && (OdbLastPolygonPadAperTure.SpecialType == ObjectType))
			{
				AperTure = &((*AperTures)[OdbLastPolygonPadAperTure.AperTureNr]);
				return OdbLastPolygonPadAperTure.AperTureNr;
			}
		}
	}

	Found = -1;

	for (cnt = 0; cnt < NrAperTures; cnt++)
	{
		if (ObjectType == 0)
		{
			if ((InRangeSpecial((*AperTures)[cnt].Rotation, Rotation, 0.01)) && ((*AperTures)[cnt].Mirror == Mirror)
			        && ((*AperTures)[cnt].Address == Address))
			{
				if (Found == -1)
				{
					Found = cnt;
					break;
				}
			}
		}
		else
		{
			if ((InRangeSpecial(OdbLastPolygonPadAperTure.Rotation, Rotation, 0.01))
			        && (OdbLastPolygonPadAperTure.Mirror == Mirror)
			        && (InRangeSpecial(OdbLastPolygonPadAperTure.x2, x2, 10.0))
			        && (InRangeSpecial(OdbLastPolygonPadAperTure.y2, y2, 10.0))
			        && (OdbLastPolygonPadAperTure.SpecialType == ObjectType))
			{
				if (Found == -1)
				{
					Found = cnt;
					break;
				}
			}
		}
	}

	cnt = Found;

	if (Found == -1)
		cnt = NrAperTures;

	if (cnt < NrAperTures)
		AperTure = &((*AperTures)[cnt]);
	else
	{
		if (NrAperTures + 1 >= MaxNrAperTures)
		{
			if (AllocateMemAperTures(NrAperTures + 64) != 0)
				return -1;
		}

		AperTure = &((*AperTures)[cnt]);
		AperTure->Info = ODB_PAD_POLYGON;
		AperTure->Info3 = 0;
		AperTure->x2 = x2;
		AperTure->y2 = y2;
		AperTure->SpecialType = ObjectType;
		AperTure->Rotation = Rotation;
		AperTure->Mirror = Mirror;
		AperTure->Address = Address;
		AperTure->AperTureNr = cnt;
		Found2 = -1;

		for (cnt2 = 0; cnt2 < NrOdbApertureMacroObjects; cnt2++)
		{
			if (ObjectType == 0)
			{
				if ((InRangeSpecial(ApertureMacroObjects[cnt].Rotation, Rotation, 0.01))
				        && (ApertureMacroObjects[cnt].Mirror == Mirror) && (ApertureMacroObjects[cnt].Address == Address)
				        && (ApertureMacroObjects[cnt].SpecialType == ObjectType))
				{
					if (Found2 == -1)
					{
						Found2 = cnt2;
						break;
					}
				}
			}
			else
			{
				if ((InRangeSpecial(ApertureMacroObjects[cnt].Rotation, Rotation, 0.01))
				        && (ApertureMacroObjects[cnt].Mirror == Mirror)
				        && (InRangeSpecial(ApertureMacroObjects[cnt].x2, x2, 10.0))
				        && (InRangeSpecial(ApertureMacroObjects[cnt].y2, y2, 10.0))
				        && (ApertureMacroObjects[cnt].SpecialType == ObjectType))
				{
					if (Found2 == -1)
					{
						Found2 = cnt;
						break;
					}
				}
			}
		}

		if (Found2 == -1)
		{
			if (NrOdbApertureMacroObjects >= MaxNrOdbApertureMacroObjects)
			{
				MaxNrOdbApertureMacroObjects = NrOdbApertureMacroObjects + 128;
				AllocateMemTemp2(MaxNrOdbApertureMacroObjects * sizeof(ApertureMacroObjectObjectRecord));
				ApertureMacroObjects = (ApertureMacroObjectObjectRecord *) TempMem2;
			}

			ApertureMacroObjects[NrOdbApertureMacroObjects].Mirror = Mirror;
			ApertureMacroObjects[NrOdbApertureMacroObjects].Address = Address;
			ApertureMacroObjects[NrOdbApertureMacroObjects].Rotation = Rotation;
			ApertureMacroObjects[NrOdbApertureMacroObjects].x2 = x2;
			ApertureMacroObjects[NrOdbApertureMacroObjects].y2 = y2;
			ApertureMacroObjects[NrOdbApertureMacroObjects].SpecialType = ObjectType;
//      sprintf(ApertureMacroObjects[NrOdbApertureMacroObjects].Name,"POLYGON%d",NrOdbApertureMacroObjects+1);

#ifdef _DEBUG

			if (stricmp(ActiveComp, "R110") == 0)
				ok = 1;

#endif
			sprintf(ApertureMacroObjects[NrOdbApertureMacroObjects].Name, "polygon_%s_%d", ActiveShape,
			        NrOdbApertureMacroObjects + 1);
			strlwrUTF8(ApertureMacroObjects[NrOdbApertureMacroObjects].Name);
			Found2 = NrOdbApertureMacroObjects;
			NrOdbApertureMacroObjects++;
		}

		AperTure->Info3 = Found2;

		if (NrAperTures == 0)
		{
			OdbLastPolygonPadAperTure.x2 = x2;
			OdbLastPolygonPadAperTure.y2 = y2;
			OdbLastPolygonPadAperTure.SpecialType = ObjectType;
			OdbLastPolygonPadAperTure.Rotation = Rotation;
			OdbLastPolygonPadAperTure.Mirror = Mirror;
			OdbLastPolygonPadAperTure.Address = Address;
			OdbLastPolygonPadAperTure.AperTureNr = cnt;
			OdbLastRectPadAperTure.x = 0.0;
			OdbLastRectPadAperTure.y = 0.0;
			OdbLastThermalReliefAperTure.x = 0.0;
			OdbLastRoundPadAperTure.x = 0.0;
		}

		NrAperTures++;
	}

	return cnt;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareTraces(int32 Layer, int32 mode)
{
	int32 cnt, TraceInfo, cnt2, mode2;
	TraceRecord *Trace;
	ObjectRecord *Object5;
#ifdef _DEBUG

	if (Layer == 0)
		ok = 1;

#endif

	mode2 = 7;
	Object5 = NULL;
	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
	{
		Trace = &((*VerTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object5 = GetNewObject5(Layer)) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object5, TRACE_VER, Layer, cnt, mode2);
			Object5->Info2 = OdbCheckRoundPadAperTure(Trace->ThickNess);
			Object5->Info = 0xffff;
			cnt2++;
		}
	}

	for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
	{
		Trace = &((*HorTraces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object5 = GetNewObject5(Layer)) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object5, TRACE_HOR, Layer, cnt, mode2);
			Object5->Info2 = OdbCheckRoundPadAperTure(Trace->ThickNess);
			Object5->Info = 0xffff;
			cnt2++;
		}
	}

	for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
	{
		Trace = &((*Diag1Traces[Layer])[cnt]);
#ifdef _DEBUG

		if (InRange4(Trace->ThickNess, 3.048e5))
			ok = 1;

#endif
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object5 = GetNewObject5(Layer)) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object5, TRACE_DIAG1, Layer, cnt, mode2);
			Object5->Info2 = OdbCheckRoundPadAperTure(Trace->ThickNess);
			Object5->Info = 0xffff;
			cnt2++;
		}
	}

	for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
	{
		Trace = &((*Diag2Traces[Layer])[cnt]);
		TraceInfo = Trace->Info;

		if ((TraceInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((Object5 = GetNewObject5(Layer)) == NULL)
				return -1;

			CreateTraceObjectFromTrace(Trace, Object5, TRACE_DIAG2, Layer, cnt, mode2);
			Object5->Info2 = OdbCheckRoundPadAperTure(Trace->ThickNess);
			Object5->Info = 0xffff;
			cnt2++;
		}
	}

	return cnt2;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareVias(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt3, ViaInfo, Found;
	ViaRecord *Via;
	ObjectRecord *Object5;
	int32 PowerPlaneLayer;
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf2[10240];
	AreaFillRecord *AreaFill;

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	cnt3 = 0;

	Object5 = NULL;

	if ((mode & 1) == 0)
	{
		PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);

		if ((Layer == 0) || (Layer == Design.NrBoardLayers - 1))
		{
			for (cnt = 0; cnt < Design.NrVias; cnt++)
			{
				Via = &((*Vias)[cnt]);
				ViaInfo = Via->Info;

				if ((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if ((Object5 = GetNewObject5(Layer)) == NULL)
						return -1;

					CreateViaObjectFromVia(Via, Object5, 3);
					Object5->TraceNr = -1;
					Object5->Info2 = OdbCheckRoundPadAperTure(Via->ThickNess);
					cnt3++;
				}
			}
		}
		else
		{
// ****************************************************************************
// ****************************************************************************
			for (cnt = 0; cnt < Design.NrVias; cnt++)
			{
				Via = &((*Vias)[cnt]);
				ViaInfo = Via->Info;

				if ((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Found = -1;

					if ((Object5 = GetNewObject5(Layer)) == NULL)
						return -1;

					CreateViaObjectFromVia(Via, Object5, 3);
					Object5->x3 = Via->ThermalInner;
					Object5->NetNr = -1;
					Object5->TraceNr = -1;

					if (PowerPlaneLayer)
					{
						MakePolygonFromObject(Object5, PolygonObject, 0.0, 0.00001, 1, 0);

						for (cnt2 = 0; cnt2 < Design.NrAreaFills; cnt2++)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt2]]);

							if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
							        && (AreaFill->maxx >= Object5->minx) && (AreaFill->minx <= Object5->maxx)
							        && (AreaFill->maxy >= Object5->miny) && (AreaFill->miny <= Object5->maxy))
							{
								if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
									Found = cnt2;
							}
						}
					}

					if (Found != -1)
					{
						AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
						Object5->TraceNr = Found;

						if (AreaFill->NetNr == Via->NetNr)
						{
							if ((Via->DrillThickNess > MinDrillForThermalRelief)
							        && ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) == AREAFILL_WITH_THERMAL_RELIEF)
							        && ((AreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF) == 0)
							        && (NotInRange(AreaFill->ThermalReliefDistance, 0.0))
							        && (NotInRange(AreaFill->ThermalReliefThickness, 0.0)))
							{
// Add thermal relief if diameter greater MinDrillForThermalRelief
								Object5->Info2 =
								    OdbCheckThermalReliefAperTure(Via->DrillThickNess,
								                                  Via->DrillThickNess +
								                                  AreaFill->ThermalReliefThickness * 2.0,
								                                  AreaFill->ThermalReliefDistance);
//                  Object5->Info2=OdbCheckRoundPadAperTure(AreaFill->ThermalReliefThickness);
								Object5->ObjectType = THERMAL_RELIEF_ROUND;
								Object5->x2 = Via->DrillThickNess + AreaFill->ThermalReliefThickness * 2.0;
								Object5->y2 = Via->DrillThickNess;
								Object5->x3 = AreaFill->ThermalReliefThickness * 2.0;
								FillPositionObject(Object5);
								cnt3++;
							}
							else
							{
// Do not add pad
								NrLayerPlotObjects[Layer]--;
								NrObjects5--;
							}
						}
						else
						{
// Add anti power pad
							if (NotInRange(Object5->x3, 0.0))
								Object5->x2 = Object5->x3;
							else
								Object5->x2 += max(Via->Clearance, Design.StandardClearance) * 2;

							Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
							cnt3++;
						}
					}
					else
					{
// Add normal pad
						Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
						cnt3++;
					}
				}
			}
		}
	}
	else
	{
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// Via drill holes
		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);

			if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if ((Object5 = GetNewObject5(Layer)) == NULL)
					return -1;

				Object5->x1 = Via->X;
				Object5->y1 = Via->Y;
				Object5->x2 = Via->DrillThickNess;
				Object5->ObjectType = DRILL;
				Object5->NetNr = Via->NetNr;
#ifdef _DEBUG

				if (Via->NetNr == 0)
					ok = 1;

#endif
				Object5->Info2 = OdbCheckRoundPadAperTure(Via->DrillThickNess);
				Object5->Info = 0xfffe;
				FillPositionObject(Object5);
				cnt3++;
			}
		}
	}

	return cnt3;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareComponentPins(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, Found, MemPos, LayerCopy, ShapeNr, PowerPlaneLayer;
	double x2a, y2a;
	CompRecord *Comp;
	ShapeRecord *Shape;
	ObjectRecord *Object, *Object5, *Object2;
	AreaFillRecord *AreaFill;
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf2[10240];
	ObjectArcRecord *ObjectArc;

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	Object5 = NULL;
	cnt3 = 0;

	if ((mode & 1) == 0)
	{
		PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);

		if ((Layer == 0) || (Layer == Design.NrBoardLayers - 1))
		{
			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					NrObjects = 0;
					ShapeNr = Comp->ShapeNr;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "Z101") == 0)
						ok = 1;

					strcpy(ActiveComp, Comp->Name);
#endif

					if (ShapeNr != -1)
					{
						MemPos = (*Shapes)[ShapeNr].ShapePos;
						Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
						strcpy(ActiveShape, Shape->ShapeName);
					}
					else
						strcpy(ActiveShape, "undef_shape");

// ****************************************************************************

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
#ifdef _DEBUG

						if (stricmpOwn(Comp->Name, "Z100") == 0)
						{
							ok = 1;

							if (Layer == 1)
							{
								ok = 1;

								if (cnt2 == 11)
									ok = 1;
							}
						}

#endif

						if ((Object->Layer == -1) || (Object->Layer == Layer))
						{
							if ((Object5 = GetNewObject5(Layer)) == NULL)
								return -1;

							LayerCopy = Object5->Layer;
							memcpy(Object5, Object, sizeof(ObjectRecord));
							Object5->Layer = LayerCopy;
							Object5->TraceNr = cnt;
							Object5->Info = Comp->ShapeNr;
							Object5->Test = 0;

							switch (Object->ObjectType)
							{
							case DRILL:
							case DRILL_UNPLATED:
// Do not add pad
								NrLayerPlotObjects[Layer]--;
								NrObjects5--;
								break;

							case PIN_PUT_THROUGH_ROUND:
								Object5->Info2 = OdbCheckRoundPadAperTure(Object->x2);
								FillPositionObject(Object5);
								cnt3++;
								break;

							case PIN_SMD_ROUND:
							case FIDUCIAL:
								Object5->Info2 = OdbCheckRoundPadAperTure(Object->x2);
								cnt3++;
								FillPositionObject(Object5);
								break;

							case PIN_PUT_THROUGH_SQUARE:
								Object5->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->x2);
								FillPositionObject(Object5);
								cnt3++;
								break;

							case PIN_SMD_RECT:
								Object5->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
								cnt3++;
								FillPositionObject(Object5);
								break;

							case PIN_LINE_HOR:
							case PIN_LINE_VER:
							case PIN_LINE_DIAG1:
							case PIN_LINE_DIAG2:
								Object5->Info2 = OdbCheckRoundPadAperTure(Object->y2);
								FillPositionObject(Object5);
								cnt3++;
								break;

							case PIN_LINE_ALL_ANGLE:
							case OBJECT_LINE:
								Object5->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
								FillPositionObject(Object5);
								cnt3++;
								break;

							case PIN_ARC:
								Object5->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
								FillPositionObject(Object5);
								cnt3++;
								break;

							case OBJECT_ARC:
								Object5->Info2 = -1;
								FillPositionObject(Object5);
								cnt3++;
								break;

							case OBJECT_POLYGON:
								ok = 1;
								break;

							case PIN_SMD_POLYGON:
								ActiveRotation = Object->RotationAngle;

								if (Object->ObjectType2 != 0)
								{
									Object5->ObjectType = PIN_SMD_POLYGON;
									ActivePolygonVertices = 4;
									Object5->Info2 =
									    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
									                            Object->RotationAngle, Object->Mirror, 0);
								}
								else
								{
									Object5->ObjectType = PIN_SMD_POLYGON;
#ifdef _DEBUG

									if (stricmpOwn(Comp->Name, "Z101") == 0)
										ok = 1;

#endif

									if (CheckObjectIsBigPolygon(Object5))
										Object5->Info2 = -1;
									else
									{
										ActivePolygonVertices = ((GeomPolygonRecord *) Object->Address)->NrVertices;
										Object5->Info2 =
										    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
										                            Object->Address);
									}
								}

								FillPositionObject(Object5);
								cnt3++;
								break;

							case PIN_PUT_THROUGH_POLYGON:
								Object5->ObjectType = PIN_PUT_THROUGH_POLYGON;

								if (Object->ObjectType2 != 0)
								{
									Object5->Info2 =
									    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->x2,
									                            Object->RotationAngle, Object->Mirror, 0);
								}
								else
								{
									Object5->Info2 =
									    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
									                            Object->Address);
								}

								FillPositionObject(Object5);
								cnt3++;
								break;
							}
						}
					}
				}
			}

			// ****************************************************************************
			// ****************************************************************************
		}
		else
		{
			// Inner layers
			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					NrObjects = 0;
					NrObjects2 = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);	// With anti power pads -> Objects
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 1, 0, 1);	// With inner pads      -> Objects2
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "Z100") == 0)
						ok = 1;

#endif

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);
						Object2 = &((*Objects2)[cnt2]);
#ifdef _DEBUG

						if (stricmpOwn(Comp->Name, "J501") == 0)
						{
							ok = 1;

							if (Layer == 2)
							{
								ok = 1;

								if (cnt2 == 12)
									ok = 1;
							}
						}

						if ((InRange9(Object->x1, 176.0e5)) && (InRange9(Object->y1, 70.6e5)))
							ok = 1;

#endif

						switch (Object->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:
						case PIN_PUT_THROUGH_SQUARE:
						case DRILL:
						case PIN_PUT_THROUGH_POLYGON:
							Found = -1;

							if ((Object5 = GetNewObject5(Layer)) == NULL)
								return -1;

							Object5->x1 = Object->x1;
							Object5->y1 = Object->y1;
							x2a = Object->x2;
							y2a = Object->y2;

							if (Object->ObjectType == DRILL)
							{
								//                      Object->y2=Object->x2;
								Object->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
								Object->y2 = Object->x2;
							}

							Object5->x2 = Object->x2;
							Object5->ObjectType = PIN_SMD_ROUND;
							Object5->TraceNr = -1;
							Object5->NetNr = Object->NetNr;
							Object5->Info = 0xffff;
							FillPositionObject(Object5);

							if (PowerPlaneLayer)
							{
								Object5->NetNr = -1;
								MakePolygonFromObject(Object5, PolygonObject, 0.0, 0.00001, 1, 0);

								for (cnt4 = 0; cnt4 < Design.NrAreaFills; cnt4++)
								{
									AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt4]]);

									if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
									        && (AreaFill->maxx >= Object5->minx) && (AreaFill->minx <= Object5->maxx)
									        && (AreaFill->maxy >= Object5->miny) && (AreaFill->miny <= Object5->maxy))
									{
										if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
											Found = cnt4;
									}
								}
							}

// Object inside a areafill (powerplane)
							if (Found != -1)
							{
								AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
								Object5->TraceNr = Found;

								if (AreaFill->NetNr == Object->NetNr)
								{
// Add thermal relief if diameter greater MinDrillForThermalRelief
									if ((Object->y2 > MinDrillForThermalRelief)
									        && ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) ==
									            AREAFILL_WITH_THERMAL_RELIEF)
									        && (NotInRange(AreaFill->ThermalReliefDistance, 0.0))
									        && (NotInRange(AreaFill->ThermalReliefThickness, 0.0)))
									{
//                        Object5->x2=Object->x2;
//                        Object5->x3=AreaFill->ThermalReliefDistance;
										Object5->Info2 =
										    OdbCheckThermalReliefAperTure(Object->y2,
										                                  Object->y2 +
										                                  AreaFill->ThermalReliefThickness * 2.0,
										                                  AreaFill->ThermalReliefDistance);
//                        Object5->Info2=OdbCheckRoundPadAperTure(AreaFill->ThermalReliefThickness);
										Object5->Info |= 4;	// Add Component pin as a thermal relief
										Object5->x2 = Object->y2 + AreaFill->ThermalReliefThickness * 2.0;
										Object5->ObjectType = THERMAL_RELIEF_ROUND;
										Object5->NetNr = Object->NetNr;

#ifdef _DEBUG

										if ((InRange9(Object->x1, 176.0e5)) && (InRange9(Object->y1, 70.6e5)))
										{
											ok = 1;

											if (Layer == 2)
												ok = 1;
										}

#endif

										FillPositionObject(Object5);
										cnt3++;
									}
									else
									{
// Do not add pad
										NrLayerPlotObjects[Layer]--;
										NrObjects5--;
									}
								}
								else
								{
// Add anti power pad
									if (Object->ObjectType != DRILL)
									{
										if (InRange(Object->x3, 0.0))
											Object5->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
										else
											Object5->x2 = Object->x3;
									}
									else
									{
										if (InRange(y2a, 0.0))
											Object5->x2 = x2a;
										else
										{
											Object5->x2 = y2a;	// anti power pad
										}
									}

									Object5->NetNr = Object->NetNr;
									Object5->ObjectType = PIN_PUT_THROUGH_ROUND_POWER;
									Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
									cnt3++;
								}
							}
							else
							{
// Check inner pad
								if (Object->ObjectType != DRILL)
								{
									if (NotInRange(Object2->x3, 0.0))
										Object5->x2 = Object2->x3;
								}
								else
									Object5->x2 = x2a;

								Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
								Object5->NetNr = Object->NetNr;
								cnt3++;
							}

							break;

// ****************************************************************************
						case DRILL_UNPLATED:
							if (PowerPlaneLayer)
							{
// Add anti power pad
								if ((Object5 = GetNewObject5(Layer)) == NULL)
									return -1;

								Object5->x1 = Object->x1;
								Object5->y1 = Object->y1;
								Object5->x2 = Object->x2;

								if (NotInRange(Object->y2, 0.0))
									Object5->x2 = Object->y2;
								else
								{
									// Create pad
									Object5->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
								}

								Object5->ObjectType = PIN_SMD_ROUND;
								Object5->Info = 0xffff;
								Object5->NetNr = -1;
								Object5->TraceNr = -1;
								FillPositionObject(Object5);
								Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
								cnt3++;
							}

							break;

// ****************************************************************************
						default:
							if (Object->Layer == Layer)
							{
								if ((Object5 = GetNewObject5(Layer)) == NULL)
									return -1;

								LayerCopy = Object5->Layer;
								memcpy(Object5, Object, sizeof(ObjectRecord));
								Object5->Layer = LayerCopy;
								Object5->Test = 0;
								Object5->Info = 0xffff;

								switch (Object->ObjectType)
								{
								case PIN_SMD_ROUND:
									Object5->Info2 = OdbCheckRoundPadAperTure(Object->x2);
									FillPositionObject(Object5);
									cnt3++;
									break;

								case PIN_SMD_RECT:
									Object5->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
									FillPositionObject(Object5);
									cnt3++;
									break;

								case PIN_LINE_HOR:
								case PIN_LINE_VER:
								case PIN_LINE_DIAG1:
								case PIN_LINE_DIAG2:
									Object5->Info2 = OdbCheckRoundPadAperTure(Object->y2);
									FillPositionObject(Object5);
									cnt3++;
									break;

								case PIN_LINE_ALL_ANGLE:
								case OBJECT_LINE:
									Object5->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
									FillPositionObject(Object5);
									cnt3++;
									break;
								}
							}

							break;
						}
					}
				}
			}

			if (PowerPlaneLayer)
			{
// Drill objects in a powerplane
				for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
				{
					ObjectArc = &((*ObjectArcs)[cnt]);

					if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
					        && ((ObjectArc->Layer == DRILL_UNPLATED_LAYER) || (ObjectArc->Layer == DRILL_LAYER)))
					{
						if ((Object5 = GetNewObject5(Layer)) == NULL)
							return -1;

						Object5->x1 = ObjectArc->CentreX;
						Object5->y1 = ObjectArc->CentreY;
						Object5->x2 = ObjectArc->Width + Design.StandardClearance * 2;
						Object5->ObjectType = PIN_SMD_ROUND;
						Object5->Info = 0xffff;
						Object5->TraceNr = -1;
						Object5->NetNr = -1;
						FillPositionObject(Object5);
						Found = -1;
						MakePolygonFromObject(Object5, PolygonObject, 0.0, 0.00001, 1, 0);

						for (cnt4 = 0; cnt4 < Design.NrAreaFills; cnt4++)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt4]]);

							if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
							        && (AreaFill->maxx >= Object5->minx) && (AreaFill->minx <= Object5->maxx)
							        && (AreaFill->maxy >= Object5->miny) && (AreaFill->miny <= Object5->maxy))
							{
								if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
									Found = cnt4;
							}
						}

// Object inside a areafill (powerplane)
						if (Found != -1)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
							Object5->TraceNr = Found;
							Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
							cnt3++;
						}
						else
						{
// Do not add pad
							NrLayerPlotObjects[Layer]--;
							NrObjects5--;
						}
					}
				}
			}
		}
	}
	else
	{
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// Component drill holes

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					switch (Object->ObjectType)
					{
					case DRILL_UNPLATED:
						if ((Object5 = GetNewObject5(Layer)) == NULL)
							return -1;

						if (Object->ObjectType == DRILL_UNPLATED)
							Object->y2 = Object->x2;

						LayerCopy = Object5->Layer;
						memcpy(Object5, Object, sizeof(ObjectRecord));
						Object5->Layer = LayerCopy;
						Object5->ObjectType = DRILL_UNPLATED;
						Object5->Test = 0;
						Object5->x2 = Object->y2;
						Object5->Info2 = OdbCheckRoundPadAperTure(Object->y2);
						Object5->Info = Comp->ShapeNr;
						FillPositionObject(Object5);
						cnt3++;
						break;

					case DRILL:
						if ((Object5 = GetNewObject5(Layer)) == NULL)
							return -1;

						if (Object->ObjectType == DRILL)
							Object->y2 = Object->x2;

						LayerCopy = Object5->Layer;
						memcpy(Object5, Object, sizeof(ObjectRecord));
						Object5->Layer = LayerCopy;
						Object5->ObjectType = DRILL;
						Object5->Test = 0;
						Object5->x2 = Object->y2;
						Object5->Info2 = OdbCheckRoundPadAperTure(Object->y2);
						Object5->Info = Comp->ShapeNr;
						FillPositionObject(Object5);
						cnt3++;
						break;

					case PIN_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_SQUARE:
					case PIN_PUT_THROUGH_POLYGON:
						if ((Object5 = GetNewObject5(Layer)) == NULL)
							return -1;

						LayerCopy = Object5->Layer;
						memcpy(Object5, Object, sizeof(ObjectRecord));
						Object5->Layer = LayerCopy;
						Object5->ObjectType = DRILL;
						Object5->Test = 0;
						Object5->x2 = Object->y2;
						Object5->Info2 = OdbCheckRoundPadAperTure(Object->y2);
						Object5->Info = Comp->ShapeNr;
						FillPositionObject(Object5);
						cnt3++;
						break;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (ObjectArc->Layer == DRILL_LAYER)
				{
					if ((Object5 = GetNewObject5(Layer)) == NULL)
						return -1;

					Object5->x1 = ObjectArc->CentreX;
					Object5->y1 = ObjectArc->CentreY;
					Object5->x2 = ObjectArc->Width;
					Object5->NetNr = ObjectArc->NetNr;
					Object5->ObjectType = DRILL;
					Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
					Object5->Info = 0xffff;
					FillPositionObject(Object5);
					cnt3++;
				}

				if (ObjectArc->Layer == DRILL_UNPLATED_LAYER)
				{
					if ((Object5 = GetNewObject5(Layer)) == NULL)
						return -1;

					Object5->x1 = ObjectArc->CentreX;
					Object5->y1 = ObjectArc->CentreY;
					Object5->x2 = ObjectArc->Width;
					Object5->NetNr = -1;
					Object5->ObjectType = DRILL_UNPLATED;
					Object5->Info2 = OdbCheckRoundPadAperTure(Object5->x2);
					Object5->Info = 0xffff;
					FillPositionObject(Object5);
					cnt3++;
				}
			}
		}
	}

	return cnt3;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareComponentPinsViasSolderPasteObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, ViaInfo, ViaType;
	CompRecord *Comp;
	ViaRecord *Via;
	ObjectRecord *Object, *Object4;
	int32 PowerPlaneLayer;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	switch (Layer)
	{
	case SOLD_MASK_BOTTOM:
	case SOLD_MASK_TOP:
		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);
			ViaInfo = Via->Info;
			ViaType = Via->ViaType & 3;

			if (Layer == SOLD_MASK_BOTTOM)
				ViaType &= ~VIA_SOLDMASK_BOTTOM;

			if (Layer == SOLD_MASK_TOP)
				ViaType &= ~VIA_SOLDMASK_TOP;

			if (((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0) && (ViaType == 0) && (NotInRange(Via->SoldMask, 0.0)))
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				Object4->x1 = Via->X;
				Object4->y1 = Via->Y;
				Object4->x2 = Via->SoldMask;
				Object4->ObjectType = PIN_SMD_ROUND;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->TraceNr = -1;
				Object4->Clearance = 0.0;
				FillPositionObject(Object4);
				Object4->Info2 = OdbCheckRoundPadAperTure(Via->SoldMask);
				NrObjects4++;
			}
		}

		break;
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 1);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memcpy(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					Object4->TraceNr = -1;

					switch (Object->ObjectType)
					{
					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
						else
						{
							Object4->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_RECT;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_HOR:
					case TRACE_HOR:
						Object4->Info2 = OdbCheckRoundPadAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_HOR;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_VER:
					case TRACE_VER:
						Object4->Info2 = OdbCheckRoundPadAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_VER;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_DIAG1:
					case TRACE_DIAG1:
						Object4->Info2 = OdbCheckRoundPadAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_DIAG1;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_LINE_DIAG2:
					case TRACE_DIAG2:
						Object4->Info2 = OdbCheckRoundPadAperTure(Object->y2);
						Object4->ObjectType = PIN_LINE_DIAG2;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                            Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                            Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareComponentInfoObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, PowerPlaneLayer;
	double SpecialLineThickNess;
	LPSTR TextP;
	CompRecord *Comp;
	ObjectRecord *Object, *Object4;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 2);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memcpy(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
#ifdef _DEBUG

							if (InRange4(Object->x2, 300 * 2540.0))
							{
								ok = 1;

								if (Layer == INFO_LAYER3)
									ok = 1;
							}

#endif
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
#ifdef _DEBUG

							if ((InRange4(Object->x2, 170 * 2540.0)) && (InRange4(Object->y2, 75 * 2540.0)))
							{
								ok = 1;

								if (Layer == INFO_LAYER3)
									ok = 1;
							}

#endif
							Object4->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_RECT;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                            Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                            Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_TEXT:
					case OBJECT_TEXT2:
						TextP = (LPSTR) Object4->TraceNr;
#ifdef _DEBUG

						if (stricmpOwn(TextP, "info1_0") == 0)
							ok = 1;

#endif
						Object4->Test = Object->Info2;
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareComponentOutlineObjects(int32 Layer, int32 CompOutlineLayers, int32 mode)
{
	int32 cnt, cnt2, PowerPlaneLayer;
	double SpecialLineThickNess;
	CompRecord *Comp;
	LPSTR TextP;
	ObjectRecord *Object, *Object4;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			/*
			      CompOutlineOnBottom=0;
			      if (CompOutlineLayers==2) {
			        if (GetComponentPinLayer(Comp)==0) {
			#ifdef _DEBUG
			          if (Layer==COMP_OUTLINE_LAYER+1) {
			            ok=1;
			          }
			#endif
			          CompOutlineOnBottom=1;
			        }
			      }
			*/
			NrObjects = 0;
			ShapeCompOutLineToObject(Comp, 0.0, 0.0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memcpy(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
							Object4->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_RECT;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                            Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                            Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_TEXT:
					case OBJECT_TEXT2:
						TextP = (LPSTR) Object4->TraceNr;
#ifdef _DEBUG

						if (stricmpOwn(TextP, "info1_0") == 0)
							ok = 1;

#endif
						Object4->Test = Object->Info2;
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareComponentPlacementOutlineObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, PowerPlaneLayer;
	double SpecialLineThickNess;
	CompRecord *Comp;
	LPSTR TextP;
	ObjectRecord *Object, *Object4;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "U100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			/*
			      CompOutlineOnBottom=0;
			      if (CompOutlineLayers==2) {
			        if (GetComponentPinLayer(Comp)==0) {
			#ifdef _DEBUG
			          if (Layer==COMP_OUTLINE_LAYER+1) {
			            ok=1;
			          }
			#endif
			          CompOutlineOnBottom=1;
			        }
			      }
			*/
			NrObjects = 0;
			ShapePlacementOutLineToObject(Comp, 0.0, 0.0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memcpy(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case PIN_LINE_ALL_ANGLE:
					case OBJECT_LINE:
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						else
						{
							Object4->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_RECT;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                            Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                            Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_TEXT:
					case OBJECT_TEXT2:
						TextP = (LPSTR) Object4->TraceNr;
#ifdef _DEBUG

						if (stricmpOwn(TextP, "info1_0") == 0)
							ok = 1;

#endif
						Object4->Test = Object->Info2;
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareComponentRoutingKeepoutObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, PowerPlaneLayer;
	double SpecialLineThickNess;
	CompRecord *Comp;
	ObjectRecord *Object, *Object4;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmp(Comp->Name, "Z100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif
			NrObjects = 0;
			ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 4);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memcpy(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Test = 0;
					Object4->Clearance = 0.0;
					SpecialLineThickNess = Object4->Thickness;

					switch (Object->ObjectType)
					{
					case OBJECT_ARC:
						Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
						Object4->ObjectType = PIN_SMD_ROUND;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						Object4->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
						Object4->ObjectType = PIN_SMD_RECT;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
						if ((Object->Info & OBJECT_FILLED) == 0)
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
						else
							Object4->Info2 = -1;

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                            Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							Object4->ObjectType = PIN_SMD_POLYGON;
							Object4->Info2 =
							    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
							                            Object->Address);
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}

#ifdef _DEBUG

					if (Object4->Info2 == 411)
						ok = 1;

#endif
				}
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareComponentSilkScreenObjects(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, ShapeNr, MemPos, Mirror, Layer2 = 0, PowerPlaneLayer, OkToAddObject;
	double SpecialLineThickNess;
	LPSTR TextP;
	CompRecord *Comp;
	ShapeRecord *Shape;
	ObjectRecord *Object, *Object4, NewObject;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;

	switch (Layer)
	{
	case SILKSCREEN_BOTTOM:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_REFS:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_REFS:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_VALUES:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_VALUES:
		Layer2 = SILKSCREEN_TOP;
		break;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			NrObjects = 0;
			ShapeCompSilkScreenToObject(Comp, 0.0, 0.0, 0);
			ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, -1, 5);
#ifdef _DEBUG

			if (stricmp(Comp->Name, "Z100") == 0)
			{
				ok = 1;

				if (Layer == SOLD_MASK_TOP)
					ok = 1;
			}

#endif

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (Object->Layer == Layer2)
				{
					if ((Object4 = GetNewObject4()) == NULL)
						return -1;

					memcpy(Object4, Object, sizeof(ObjectRecord));
					Object4->Info &= OBJECT_FILLED;
					Object4->Clearance = 0.0;
					Object4->Test = 0;
					SpecialLineThickNess = Object->Thickness;

					switch (Object->ObjectType)
					{
					case OBJECT_LINE:
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_RECT:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
							Object4->Thickness = SpecialLineThickNess;
						}
						else
						{
							Object4->Info2 = OdbCheckRectPadAperTure(Object->x2, Object->y2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_CIRCLE:
					case OBJECT_ARC:
						if ((Object->Info & OBJECT_FILLED) == 0)
						{
							SpecialLineThickNess = Object4->Thickness;
							Object4->Thickness = SpecialLineThickNess;
							Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						}
						else
						{
							Object4->Info2 = OdbCheckRoundPadAperTure(Object->x2);
							Object4->ObjectType = PIN_SMD_ROUND;
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_TEXT:
					case OBJECT_TEXT2:
						TextP = (LPSTR) Object4->TraceNr;
#ifdef _DEBUG

						if (stricmpOwn(TextP, "info1_0") == 0)
							ok = 1;

#endif
						Object4->Test = Object->Info2;
						Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
						Object4->Thickness = SpecialLineThickNess;
						FillPositionObject(Object4);
						NrObjects4++;
						break;

					/*
					            case OBJECT_POLYGON:
					              if ((Object->Info & OBJECT_FILLED) == 0) {
					                Object4->Info2=OdbCheckRoundPadAperTure(Object->Thickness);
					              } else {
					                Object4->Info2=-1;
					              }
					              FillPositionObject(Object4);
					              NrObjects4++;
					              break;
					*/


					case PIN_SMD_POLYGON:
						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                            Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
								                            Object->Address);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;

					case OBJECT_POLYGON:
#ifdef _DEBUG
						if (stricmp(Comp->Name, "Z100") == 0)
						{
							ok = 1;

							if (Layer == SOLD_MASK_TOP)
								ok = 1;
						}

#endif

						if (Object->ObjectType2 != 0)
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(Object->ObjectType2, Object->x2, Object->y2,
								                            Object->RotationAngle, Object->Mirror, 0);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}
						else
						{
							if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
							{
								Object4->ObjectType = PIN_SMD_POLYGON;
								Object4->Info2 =
								    OdbCheckPolygonAperTure(0, 0.0, 0.0, Object->RotationAngle, Object->Mirror,
								                            Object->Address);
							}
							else
							{
								Object4->ObjectType = OBJECT_POLYGON;
								Object4->Info2 = OdbCheckRoundPadAperTure(Object->Thickness);
							}
						}

						FillPositionObject(Object4);
						NrObjects4++;
						break;
					}
				}
			}

// ****************************************************************************
			ShapeNr = Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "u101") == 0)
			{
				ok = 1;

				if (Layer == SILKSCREEN_BOTTOM_REFS)
					ok = 1;

				if (Layer == SILKSCREEN_TOP_REFS)
					ok = 1;
			}

			if (Layer == SILKSCREEN_TOP_VALUES)
				ok = 1;

#endif
			memset(&NewObject, 0, sizeof(NewObject));
			OkToAddObject = 1;
			Mirror = ((Comp->CompMode & 8) >> 3);

			switch (Layer)
			{
			case SILKSCREEN_BOTTOM_REFS:
				if (((Comp->TextVisibility & 0x100) != 0)	// Reference
				        || (!Mirror) || ((Comp->TextVisibility & 0x8) == 0)
				        || (MakeObjectFromCompRef(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;

			case SILKSCREEN_TOP_REFS:
				if (((Comp->TextVisibility & 0x100) != 0)	// Reference
				        || (Mirror) || (Comp->TextVisibility & 0x8) || (MakeObjectFromCompRef(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;

			case SILKSCREEN_BOTTOM_VALUES:
				if (((Comp->TextVisibility & 0x200) != 0)	// value
				        || (!Mirror) || ((Comp->TextVisibility & 0x80) == 0)
				        || (MakeObjectFromCompValue(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;

			case SILKSCREEN_TOP_VALUES:
				if (((Comp->TextVisibility & 0x200) != 0)	// value
				        || (Mirror) || (Comp->TextVisibility & 0x80) || (MakeObjectFromCompValue(Comp, &NewObject, 0) != 0))
					OkToAddObject = 0;

				break;
			}

			if (OkToAddObject)
			{
				if ((Object4 = GetNewObject4()) == NULL)
					return -1;

				memcpy(Object4, &NewObject, sizeof(ObjectRecord));
				SpecialLineThickNess = Object4->Thickness;
				Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
				Object4->Thickness = SpecialLineThickNess;
				Object4->Info = 0;
				FillPositionObject(Object4);
				NrObjects4++;
			}
		}
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 OdbPrepareSpecialObjects(int32 Layer, int32 mode)
{
	int32 cnt, Layer2, CopperLayer, PowerPlaneLayer;
	double SpecialLineThickNess;
	ObjectRecord *Object4;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;

	PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
	Object4 = NULL;
	CopperLayer = 0;

	switch (Layer)
	{
	case SILKSCREEN_BOTTOM:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_REFS:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_REFS:
		Layer2 = SILKSCREEN_TOP;
		break;

	case SILKSCREEN_BOTTOM_VALUES:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP_VALUES:
		Layer2 = SILKSCREEN_TOP;
		break;

	default:
		Layer2 = Layer;
		break;
	}

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectLine->Layer == Layer2)
			{
				if ((Layer2 >= 0) && (Layer2 < Design.NrBoardLayers))
				{
					if ((Object4 = GetNewObject5(Layer2)) == NULL)
						return -1;

					CopperLayer = 1;
				}
				else
				{
					CopperLayer = 0;

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;
				}

				Object4->x1 = ObjectLine->X1;
				Object4->y1 = ObjectLine->Y1;
				Object4->x2 = ObjectLine->X2;
				Object4->y2 = ObjectLine->Y2;
				Object4->Info = 0;
				Object4->Clearance = 0.0;

				if (!CopperLayer)
					Object4->Layer = ObjectLine->Layer;

				Object4->NetNr = ObjectLine->NetNr;
				Object4->ObjectType = OBJECT_LINE;
				Object4->Thickness = ObjectLine->LineThickNess;
				SpecialLineThickNess = ObjectLine->LineThickNess;
				Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
				Object4->Info = 0xffff;
				Object4->Test = ObjectLine->LineMode;
				Object4->Thickness = SpecialLineThickNess;
				FillPositionObject(Object4);

				if (!CopperLayer)
					NrObjects4++;
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
	{
		ObjectRect = &((*ObjectRects)[cnt]);

		if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectRect->Layer == Layer2)
			{
				if ((Layer2 >= 0) && (Layer2 < Design.NrBoardLayers))
				{
					if ((Object4 = GetNewObject5(Layer2)) == NULL)
						return -1;

					CopperLayer = 1;
				}
				else
				{
					CopperLayer = 0;

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;
				}

				Object4->x1 = ObjectRect->CentreX;
				Object4->y1 = ObjectRect->CentreY;
				Object4->x2 = ObjectRect->Width;
				Object4->y2 = ObjectRect->Height;
				Object4->Thickness = ObjectRect->LineThickNess;
				Object4->NetNr = ObjectRect->NetNr;
				Object4->Info = 0xffff;
				Object4->Test = 0;
				Object4->Clearance = 0.0;

				if (!CopperLayer)
					Object4->Layer = ObjectRect->Layer;

				SpecialLineThickNess = ObjectRect->LineThickNess;
#ifdef _DEBUG

				if ((InRange9(Object4->x1, -88.7e5)) && (InRange9(Object4->y1, 85.7e5)))
					ok = 1;

#endif

				if ((ObjectRect->Info & OBJECT_FILLED) == 0)
				{
					Object4->ObjectType = OBJECT_RECT;
					Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
					Object4->Thickness = SpecialLineThickNess;
				}
				else
				{
					Object4->Info = OBJECT_FILLED;
					Object4->ObjectType = PIN_SMD_RECT;
#ifdef _DEBUG

					if ((InRange4(ObjectRect->Width, 170 * 2540.0)) && (InRange4(ObjectRect->Height, 75 * 2540.0)))
					{
						ok = 1;

						if (Layer == INFO_LAYER3)
							ok = 1;
					}

#endif
					Object4->Info2 = OdbCheckRectPadAperTure(ObjectRect->Width, ObjectRect->Height);
				}

				FillPositionObject(Object4);

				if (!CopperLayer)
					NrObjects4++;
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
	{
		ObjectArc = &((*ObjectArcs)[cnt]);

		if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectArc->Layer == Layer2)
			{
				if ((Layer2 >= 0) && (Layer2 < Design.NrBoardLayers))
				{
					if ((Object4 = GetNewObject5(Layer2)) == NULL)
						return -1;

					CopperLayer = 1;
				}
				else
				{
					CopperLayer = 0;

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;
				}

#ifdef _DEBUG

				if (Layer2 == 1)
					ok = 1;

#endif
				Object4->x1 = ObjectArc->CentreX;
				Object4->y1 = ObjectArc->CentreY;
				Object4->x2 = ObjectArc->Width;
				Object4->y2 = ObjectArc->Height;

				if (NotInRangeSpecial(Object4->x2, Object4->y2, 100.0))
				{
					if (CopperLayer)
					{
						NrLayerPlotObjects[Layer2]--;
						NrObjects5--;
						continue;
					}
				}

				Object4->x3 = ObjectArc->StartDiffX;
				Object4->y3 = ObjectArc->StartDiffY;
				Object4->x4 = ObjectArc->EndDiffX;
				Object4->y4 = ObjectArc->EndDiffY;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->NetNr = ObjectArc->NetNr;
				Object4->TraceNr = -1;

				if (!CopperLayer)
					Object4->Layer = ObjectArc->Layer;

				Object4->Clearance = 0.0;
				Object4->ObjectType = OBJECT_ARC;
				Object4->Thickness = ObjectArc->LineThickNess;
				SpecialLineThickNess = ObjectArc->LineThickNess;

				if ((ObjectArc->Info & OBJECT_FILLED) == 0)
				{
					Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
					Object4->Thickness = SpecialLineThickNess;
				}
				else
				{
					if ((Object4->Info & FIDUCIAL_OPTION) == 0)
						Object4->ObjectType = PIN_SMD_ROUND;
					else
						Object4->ObjectType = FIDUCIAL;

					Object4->Info2 = OdbCheckRoundPadAperTure(ObjectArc->Width);
					Object4->Info = OBJECT_FILLED;
				}

				FillPositionObject(Object4);

				if (!CopperLayer)
					NrObjects4++;
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
	{
		ObjectText2 = &((*ObjectTexts2)[cnt]);

		if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
#ifdef _DEBUG

			if (stricmpOwn(ObjectText2->Text, "cdio") == 0)
			{
				ok = 1;

				if (Layer2 == 1)
					ok = 1;
			}

#endif

			if (ObjectText2->Layer == Layer2)
			{
				if ((Layer2 >= 0) && (Layer2 < Design.NrBoardLayers))
				{
					if ((Object4 = GetNewObject5(Layer2)) == NULL)
						return -1;

					CopperLayer = 1;
				}
				else
				{
					CopperLayer = 0;

					if ((Object4 = GetNewObject4()) == NULL)
						return -1;
				}

				Object4->x1 = ObjectText2->X;
				Object4->y1 = ObjectText2->Y;
				Object4->x2 = ObjectText2->FontHeight;
				Object4->y2 = 0.0;
				Object4->Thickness = ObjectText2->LineThickNess;
				Object4->Info = 0;

				if (!CopperLayer)
					Object4->Layer = ObjectText2->Layer;

				Object4->Test = 0;
				Object4->ObjectType = OBJECT_TEXT;
				Object4->RotationAngle = ObjectText2->Rotation;
				Object4->Mirror = (ObjectText2->TextMode & 0x10) >> 4;
				Object4->Test = ObjectText2->TextMode + (ObjectText2->FontNr << 16);
				Object4->TraceNr = (int32) & (ObjectText2->Text);
				SpecialLineThickNess = ObjectText2->LineThickNess;
				Object4->Info2 = OdbCheckRoundPadAperTure(SpecialLineThickNess);
				Object4->Thickness = SpecialLineThickNess;
				FillPositionObject(Object4);

				if (!CopperLayer)
					NrObjects4++;
			}
		}
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PlotPolygonToOdb(PolygonRecord * DrawPolygon, int32 mode)
{
	typedef uint8 ByteArray[1000];

	int32 cnt3, count, Direction;
	double x11, y11, *p;
	char str[MAX_LENGTH_STRING];
	char strx2[MAX_LENGTH_STRING], stry2[MAX_LENGTH_STRING];

	count = DrawPolygon->NrVertices;
	Direction = GetPolygonDirection(DrawPolygon);

	if (mode == 1)
	{
		// Reverse direction for invers polygons
		if (Direction == 0)
			Direction = 1;
		else
			Direction = 0;
	}

	if (Direction == 0)
	{	// Clock wise
		p = (double *) DrawPolygon->Points;
	}
	else
		p = (double *) &DrawPolygon->Points[count - 1].y;

	for (cnt3 = 0; cnt3 < count; cnt3++)
	{
		if (Direction == 0)
		{	// Clock wise
			x11 = *p++;
			y11 = *p++;
		}
		else
		{
			y11 = *p--;
			x11 = *p--;
		}

		GetUnitsValue(UNITS_INCH, x11, strx2, 9);
		GetUnitsValue(UNITS_INCH, y11, stry2, 9);

		if (cnt3 == 0)
		{
			sprintf(str, "OB %s %s", strx2, stry2);

			if (mode == 1)
				strcat(str, " H");
			else
				strcat(str, " I");
		}
		else
			sprintf(str, "OS %s %s", strx2, stry2);

		AddToMessageBuf(str);
	}

	if (Direction == 0)
	{	// Clock wise
		x11 = (*DrawPolygon).Points[0].x;
		y11 = (*DrawPolygon).Points[0].y;
	}
	else
	{
		x11 = (*DrawPolygon).Points[count - 1].x;
		y11 = (*DrawPolygon).Points[count - 1].y;
	}

	GetUnitsValue(UNITS_INCH, x11, strx2, 9);
	GetUnitsValue(UNITS_INCH, y11, stry2, 9);
	sprintf(str, "OS %s %s", strx2, stry2);
	AddToMessageBuf(str);
	AddToMessageBuf("OE");
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PlotAreaFillToOdb(AreaFillRecord * AreaFill)
{
	typedef uint8 ByteArray[1000];

	int32 cnt2, cnt3, count, Direction;
	double x11, y11, *p;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	char strx2[MAX_LENGTH_STRING], stry2[MAX_LENGTH_STRING];

	strcpy(str2, InfoStr);


// *******************************************************************************************************
// *******************************************************************************************************

	AddToMessageBuf("S P 0");
	AreaPos = (uint8 *) AreaFill;
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
	{
		count = DrawPolygon->NrVertices;
		Direction = GetPolygonDirection(DrawPolygon);

		if (cnt2 > 0)
		{
			// Reverse direction for invers polygons
			if (Direction == 0)
				Direction = 1;
			else
				Direction = 0;
		}

		if (Direction == 0)
		{	// Clock wise
			p = (double *) DrawPolygon->Points;
		}
		else
			p = (double *) &DrawPolygon->Points[count - 1].y;

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			if (Direction == 0)
			{	// Clock wise
				x11 = *p++;
				y11 = *p++;
			}
			else
			{
				y11 = *p--;
				x11 = *p--;
			}

			GetUnitsValue(UNITS_INCH, x11, strx2, 9);
			GetUnitsValue(UNITS_INCH, y11, stry2, 9);

			if (cnt3 == 0)
			{
				sprintf(str, "OB %s %s", strx2, stry2);

				if (cnt2 == 0)
					strcat(str, " I");
				else
					strcat(str, " H");
			}
			else
				sprintf(str, "OS %s %s", strx2, stry2);

			AddToMessageBuf(str);
		}

		if (Direction == 0)
		{	// Clock wise
			x11 = (*DrawPolygon).Points[0].x;
			y11 = (*DrawPolygon).Points[0].y;
		}
		else
		{
			x11 = (*DrawPolygon).Points[count - 1].x;
			y11 = (*DrawPolygon).Points[count - 1].y;
		}

		GetUnitsValue(UNITS_INCH, x11, strx2, 9);
		GetUnitsValue(UNITS_INCH, y11, stry2, 9);
		sprintf(str, "OS %s %s", strx2, stry2);
		AddToMessageBuf(str);
		AddToMessageBuf("OE");
		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	AddToMessageBuf("SE");
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PowerPlaneToOdb(int32 Layer)
{
	int32 cnt, cnt2, cnt3, count, NrLayerPolygons, NrDeletionPolygons, PowerPlaneNr, LayerPolygonsInfo[256],
	      DeletionPolygonsInfo[256];
	double x11, y11, xmin, xmax, ymin, ymax;
	PolygonRecord *LayerPolygons[256], *DeletionPolygons[256], *DeletionPolygon, *DrawPolygon, *PowerPlanePolygon,
	              *TempPolygon;
	uint8 *AreaPos, *TempAreaPos, *PolygonPos;
	AreaFillRecord *AreaFill, *PowerAreaFill, *TempAreaFill;

	AllocateSpecialMem(MEM_POWERPLANE_AREAFILL, 128 * 1024, (void **) &TempAreaFill);
	TempAreaFill->NrPolygons = 2;
	TempAreaFill->NetNr = 0;
	TempAreaFill->Layer = Layer;
	TempAreaFill->Clearance = 0.0;
	TempAreaFill->MemSize = sizeof(AreaFillRecord);
	xmin = DesignBoardOriginX - 100 * 2540.0;
	ymin = DesignBoardOriginY - 100 * 2540.0;
	xmax = DesignBoardOriginX + DesignBoardWidth + 100 * 2540.0;
	ymax = DesignBoardOriginY + DesignBoardHeight + 100 * 2540.0;
	TempAreaPos = (uint8 *) TempAreaFill;
	TempAreaPos += sizeof(AreaFillRecord);
	TempPolygon = (PolygonRecord *) TempAreaPos;
	TempPolygon->Points[0].x = xmin;
	TempPolygon->Points[0].y = ymin;
	TempPolygon->Points[1].x = xmin;
	TempPolygon->Points[1].y = ymax;
	TempPolygon->Points[2].x = xmax;
	TempPolygon->Points[2].y = ymax;
	TempPolygon->Points[3].x = xmax;
	TempPolygon->Points[3].y = ymin;
	TempAreaFill->minx = xmin;
	TempAreaFill->miny = ymin;
	TempAreaFill->maxx = xmax;
	TempAreaFill->maxy = ymax;
	TempPolygon->NrVertices = 4;
	TempAreaPos += MemSizePolygon(TempPolygon);
	TempAreaFill->MemSize += MemSizePolygon(TempPolygon);

	TempPolygon = (PolygonRecord *) TempAreaPos;

#ifdef _DEBUG

	if (Layer == 1)
		ok = 1;

#endif
	PowerPlaneNr = GetPowerPlaneByLayer(Layer);

	if (PowerPlaneNr == -1)
		return -1;

	PowerAreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[PowerPlaneNr]]);

	NrDeletionPolygons = 0;
	AreaPos = (uint8 *) PowerAreaFill;
	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	for (cnt2 = 0; cnt2 < PowerAreaFill->NrPolygons; cnt2++)
	{
		if (cnt2 > 0)
		{
			DeletionPolygons[NrDeletionPolygons] = DrawPolygon;
			DeletionPolygonsInfo[NrDeletionPolygons++] = 0;
		}

		PolygonPos += MemSizePolygon(DrawPolygon);
		DrawPolygon = (PolygonRecord *) PolygonPos;
	}

	AreaPos = (uint8 *) PowerAreaFill;
	PowerPlanePolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	CopyPolygonToPolygon(PowerPlanePolygon, TempPolygon);
	TempAreaFill->MemSize += MemSizePolygon(TempPolygon);
	TempAreaPos += MemSizePolygon(TempPolygon);
	TempPolygon = (PolygonRecord *) TempAreaPos;

	NrLayerPolygons = 0;

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if ((cnt != PowerPlaneNr) && ((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
		{
			AreaPos = (uint8 *) AreaFill;
			DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));


			for (cnt2 = 0; cnt2 < AreaFill->NrPolygons; cnt2++)
			{
				if (cnt2 == 0)
				{
					LayerPolygons[NrLayerPolygons] = DrawPolygon;
					LayerPolygonsInfo[NrLayerPolygons++] = 0;
				}
				else
				{
//           ((DrawPolygon->PolygonType & 1) == 1)) {
					DeletionPolygons[NrDeletionPolygons] = DrawPolygon;
					DeletionPolygonsInfo[NrDeletionPolygons++] = 0;
				}

				count = DrawPolygon->NrVertices;
				PolygonPos = (uint8 *) DrawPolygon;
				PolygonPos += MemSizePolygon(DrawPolygon);
				DrawPolygon = (PolygonRecord *) PolygonPos;
			}
		}
	}

#ifdef _DEBUG

	if (Layer == 1)
		ok = 1;

#endif

	for (cnt = 0; cnt < NrLayerPolygons; cnt++)
	{
		if (LayerPolygonsInfo[cnt] == 0)
		{
			DrawPolygon = LayerPolygons[cnt];

			if (CheckPolygonOverlapAreaFill(DrawPolygon, TempAreaFill))
			{
				CopyPolygonToPolygon(DrawPolygon, TempPolygon);
				TempAreaFill->MemSize += MemSizePolygon(TempPolygon);
				TempAreaPos += MemSizePolygon(TempPolygon);
				TempPolygon = (PolygonRecord *) TempAreaPos;
				TempAreaFill->NrPolygons++;
				LayerPolygonsInfo[cnt] = 1;
			}
		}
	}

#ifdef _DEBUG

	if (Layer == 1)
		ok = 1;

#endif
	PlotAreaFillToOdb(TempAreaFill);

	ok = 1;

	for (cnt2 = 0; cnt2 < NrDeletionPolygons; cnt2++)
	{
		if (DeletionPolygonsInfo[cnt2] == 0)
		{
			DeletionPolygon = DeletionPolygons[cnt2];
			count = DeletionPolygon->NrVertices;
			xmin = 1e9;
			ymin = 1e9;
			xmax = -1e9;
			ymax = -1e9;

			for (cnt3 = 0; cnt3 < count; cnt3++)
			{
				x11 = DeletionPolygon->Points[cnt3].x;
				y11 = DeletionPolygon->Points[cnt3].y;
				xmin = min(xmin, x11);
				ymin = min(ymin, y11);
				xmax = max(xmax, x11);
				ymax = max(ymax, y11);
			}

			TempAreaFill->NrPolygons = 1;
			TempAreaFill->NetNr = 0;
			TempAreaFill->Layer = Layer;
			TempAreaFill->Clearance = 0.0;
			TempAreaFill->MemSize = sizeof(AreaFillRecord);
			TempAreaFill->minx = xmin;
			TempAreaFill->miny = ymin;
			TempAreaFill->maxx = xmax;
			TempAreaFill->maxy = ymax;
			TempAreaPos = (uint8 *) TempAreaFill;
			TempAreaPos += sizeof(AreaFillRecord);
			TempPolygon = (PolygonRecord *) TempAreaPos;
			CopyPolygonToPolygon(DeletionPolygon, TempPolygon);
			TempAreaPos += MemSizePolygon(DeletionPolygon);
			TempAreaFill->MemSize += MemSizePolygon(DeletionPolygon);

			for (cnt = 0; cnt < NrLayerPolygons; cnt++)
			{
				if (LayerPolygonsInfo[cnt] == 0)
				{
					DrawPolygon = LayerPolygons[cnt];

					if (CheckPolygonCompleetlyInsidePolygon(DrawPolygon, DeletionPolygon))
					{
						TempPolygon = (PolygonRecord *) TempAreaPos;
						CopyPolygonToPolygon(DrawPolygon, TempPolygon);
						TempAreaFill->MemSize += MemSizePolygon(DrawPolygon);
						TempAreaPos += MemSizePolygon(DrawPolygon);
						LayerPolygonsInfo[cnt] = 1;
						TempAreaFill->NrPolygons++;
					}
				}
			}

			PlotAreaFillToOdb(TempAreaFill);
			DeletionPolygonsInfo[cnt2] = 1;
		}
	}

	return 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 DrawOdbStr(double x, double y, double Size, int32 ApertureNr, double Rotation, int32 Alignment, int32 Mirror,
                 char *str)
{
	int32 cnt2, LineSegments, SegmentCount;
	char strx1[200], stry1[200], strx2[200], stry2[200], str2[200];
	double x1, y1, x2, y2, LineBuf[10240];

	LineSegments = TextStringToLineSegments2(x, y, Size, Rotation, 0, Mirror, str, (double *) &LineBuf);
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{
		x1 = LineBuf[SegmentCount];
		SegmentCount++;
		y1 = LineBuf[SegmentCount];
		SegmentCount++;
		x2 = LineBuf[SegmentCount];
		SegmentCount++;
		y2 = LineBuf[SegmentCount];
		SegmentCount++;
#ifdef _DEBUG

		if (CalcLengthLine(x1, y1, x2, y2) == 0.0)
			ok = 1;

#endif
		GetUnitsValue(UNITS_INCH, x1, strx1, 0);
		GetUnitsValue(UNITS_INCH, y1, stry1, 0);
		GetUnitsValue(UNITS_INCH, x2, strx2, 0);
		GetUnitsValue(UNITS_INCH, y2, stry2, 0);
		sprintf(str2, "L %s %s %s %s %d P 0 0;0=0", strx1, stry1, strx2, stry2, ApertureNr);
		AddToMessageBuf(str2);
//    GerberWriteLine(x1,y1,x2,y2);
	}

	return LineSegments;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ExportSpecialOdbSymbols(LPSTR OdbDir, int32 mode)
{
	int32 cnt;
	char str[200], Filename[200];
	GeomPolygonRecord *GeomPolygon;
	ObjectRecord NewObject;
	PolygonRecord *DrawPolygon;
	uint8 PolygonBuf[16384];

	DrawPolygon = (PolygonRecord *) & PolygonBuf;
	memset(DrawPolygon, 0, sizeof(PolygonBuf));
	memset(&NewObject, 0, sizeof(NewObject));

	for (cnt = 0; cnt < NrOdbApertureMacroObjects; cnt++)
	{
#ifdef _DEBUG
		ok = 1;

		if (cnt == 32)
			ok = 1;

		if (ApertureMacroObjects[cnt].NewName[0] == 0)
			ok = 1;

		if (strcmp(ApertureMacroObjects[cnt].NewName, "TARGET100") == 0)
			ok = 1;

#endif

		if (ApertureMacroObjects[cnt].Name[0] == 0)
			continue;

		if (ApertureMacroObjects[cnt].SpecialType)
		{
			sprintf(str, "%s\\symbols\\%s", OdbDir, ApertureMacroObjects[cnt].Name);
			CreateDirectoryUTF8(str);
			sprintf(Filename, "%s\\symbols\\%s\\features", OdbDir, ApertureMacroObjects[cnt].Name);
			DeleteFileUTF8(Filename);
			MessageBufPos = 0;
			AddToMessageBuf("#");
			AddToMessageBuf("#Layer features");
			AddToMessageBuf("#");
			AddToMessageBuf("S P 0");
			NewObject.ObjectType = ApertureMacroObjects[cnt].SpecialType;
			NewObject.x2 = ApertureMacroObjects[cnt].x2;
			NewObject.y2 = ApertureMacroObjects[cnt].y2;
			MakePolygonFromObject(&NewObject, DrawPolygon, 0.0, 0, 0, 1);

			if (ApertureMacroObjects[cnt].Mirror)
				MirrorPolygon(DrawPolygon, 0);

			if (ApertureMacroObjects[cnt].Rotation != 0.0)
				RotatePolygon(DrawPolygon, 0.0, 0.0, ApertureMacroObjects[cnt].Rotation, 0);

			PlotPolygonToOdb(DrawPolygon, 0);
			AddToMessageBuf("SE");
			AppendStringToTextFile(Filename, MessageBuf);
		}
		else
		{
			GeomPolygon = (GeomPolygonRecord *) ApertureMacroObjects[cnt].Address;

			if ((GeomPolygon->NrVertices < 500) && (GeomPolygon->NrSubPolygons == 0)
			        && (GeomPolygon->NrVerticesMainPolygon == 0))
			{
				sprintf(str, "%s\\symbols\\%s", OdbDir, ApertureMacroObjects[cnt].Name);
				CreateDirectoryUTF8(str);
				sprintf(Filename, "%s\\symbols\\%s\\features", OdbDir, ApertureMacroObjects[cnt].Name);
				DeleteFileUTF8(Filename);
				MessageBufPos = 0;
				AddToMessageBuf("#");
				AddToMessageBuf("#Layer features");
				AddToMessageBuf("#");
				AddToMessageBuf("S P 0");
				DrawPolygon->NrVertices = GeomPolygon->NrVertices;
				memcpy(&DrawPolygon->Points, &GeomPolygon->Points, GeomPolygon->NrVertices * 2 * sizeof(double));

				if (ApertureMacroObjects[cnt].Mirror)
					MirrorPolygon(DrawPolygon, 0);

				if (ApertureMacroObjects[cnt].Rotation != 0.0)
					RotatePolygon(DrawPolygon, 0.0, 0.0, ApertureMacroObjects[cnt].Rotation, 0);

				PlotPolygonToOdb(DrawPolygon, 0);
				AddToMessageBuf("SE");
				AppendStringToTextFile(Filename, MessageBuf);
			}
		}

		/*
		S P 0
		OB -0.015748031496 -0.053149606299 I
		OS -0.015748031496 0.053149606299
		OS 0.035433070866 0.053149606299
		OS 0.015748031496 0
		OS 0.035433070866 -0.053149606299
		OS -0.015748031496 -0.053149606299
		OE
		SE
		*/
	}

	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
