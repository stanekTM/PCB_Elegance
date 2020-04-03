/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: mentor.c
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



#include "mentor.h"
#include "stdio.h"
#include "memory.h"
#include "design.h"
#include "pcb.h"
#include "time.h"
#include "files2.h"
#include "utf8.h"
#include "math.h"
#include "files.h"

#define    InRange2(x1,x2) ( (((x1>x2-10.0) && (x1<x2+10.0))) ? (1) : (0) )
#define    InRange3(x1,x2) ( (((x1>x2-10000.0) && (x1<x2+10000.0))) ? (1) : (0) )
#define    NotInRange2(x1,x2) ( (((x1>x2-10.0) && (x1<x2+10.0))) ? (0) : (1) )
#define    CalcLengthLine2(x1,y1,x2,y2) (SQR((x2)-(x1))+SQR((y2)-(y1)))

#define    MultTextOffset             (0.175*0.75)
#define    MultTextY                  0.75
#define    MentorAspectRatio          "1.0"
#define    SILKSCREEN_TEXT_THICKNESS  0.0
#define    UNITS_MM                   100000.0

typedef struct
{
	double x, y;
	char PinName[32];
	int count, Layer, info, PinNr;
} MainPinRecord;


typedef struct
{
	int32 TopObjectType, BottomObjectType, UnplatedDrill, Info, Info2, Info3, Info4, Info5, NetNr, PinInfo, PinNr,
	      Layer, CompNr, Mirror, PinCount, TraceNr;
	double TopPadWidth, TopPadHeight, BottomPadWidth, BottomPadHeight, InnerPadWidth, PowerPadWidth, DrillWidth,
	       TopSolderMaskWidth, TopSolderMaskHeight, BottomSolderMaskWidth, BottomSolderMaskHeight, TopPasteMaskWidth,
	       TopPasteMaskHeight, BottomPasteMaskWidth, BottomPasteMaskHeight;
	char PadStackName[32];
} MentorPinRecord;

typedef struct
{
	int32 TopObjectType, BottomObjectType, UnplatedDrill, Info, Info2, Info3, Info4, Info5, NetNr, PinInfo, PinNr,
	      Layer, CompNr, Mirror, PinCount, TraceNr;
	double OX, OY;
	LPSTR ShapeName;
	char PadStackName[32];
} MentorSpecialPinRecord;

typedef struct
{
	int32 Info, NrTraces;
	double x, y;
} TracePointRecord;

typedef TracePointRecord TracePointsArray[1000];

extern ShapesArray *Shapes;
extern uint8 *ShapesMem;

int32 MentorUnits, NrShapeNames, NrMainShapePins, NrMentorPins, NrMentorSpecialPins, ok;
int32 ExtendedPinsPos[1024], ExtendedPinsCount[1024], NrShapeLayers, UsedRefLayers[16];
uint8 TracePointBuf[16384];

PCBDesignRecord PcbDesign;
CompRecord Comp;
ShapeRecord *CurrentShape;

MainPinRecord MainShapePins[4096];
MentorPinRecord MentorPins[512];
MentorSpecialPinRecord MentorSpecialPins[512];

char TopLayer[] = "PAD_1";
char InnerLayers[6][10] = { "INNER_1", "INNER_2", "INNER_3", "INNER_4", "INNER_5", "INNER_6" };
char BottomLayer[] = "PAD_2";
char PasteMaskTopLayer[] = "PASTE_MASK_1";
char PasteMaskBottomLayer[] = "PASTE_MASK_2";
char SolderMaskTopLayer[] = "SOLDER_MASK_1";
char SolderMaskBottomLayer[] = "SOLDER_MASK_2";
char SilkscreenLayer[] = "SILKSCREEN";
char SilkscreenTopLayer[] = "SILKSCREEN_1";
char SilkscreenBottomLayer[] = "SILKSCREEN_2";
char PlacementOutlineLayer[] = "COMPONENT_PLACEMENT_OUTLINE";
char ComponentOutlineLayer[] = "ASSEMBLY";
char RoutingKeepoutLayer[] = "ROUTING_KEEPOUT";
char SpecialText[200], *CurrentShapeNameP;

int32 TracePointsNumbers[1024], NrTracePoints;
TracePointsArray *TracePoints;

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetTracePointNr(double x, double y)
{
	int32 cnt;

	cnt = 0;

	while (cnt < NrTracePoints)
	{
		if ((InRangeSpecial(x, (*TracePoints)[cnt].x, 100.0)) && (InRangeSpecial(y, (*TracePoints)[cnt].y, 100.0)))
			return cnt;

		cnt++;
	}

	return -1;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void InsertObjectPoint(double x1, double y1)
{
	TracePointRecord *TracePoint;

	TracePoint = &((*TracePoints)[NrTracePoints]);
	memset(TracePoint, 0, sizeof(TracePointRecord));
	TracePoint->x = x1;
	TracePoint->y = y1;
	NrTracePoints++;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetTraceObjectNrFromEndPoint(double x, double y, double *NewX, double *NewY)
{
	int32 cnt, Mask;
	double x1, y1, x2, y2;
	ObjectRecord *Object;

	Mask = 1;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object = &((*Objects6)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;

		if ((Object->Info & Mask) == 0)
		{
			if ((InRangeSpecial(x, x1, 100.0)) && (InRangeSpecial(y, y1, 100.0)))
			{
				*NewX = x2;
				*NewY = y2;
				return cnt;
			}

			if ((InRangeSpecial(x, x2, 100.0)) && (InRangeSpecial(y, y2, 100.0)))
			{
				*NewX = x1;
				*NewY = y1;
				return cnt;
			}
		}
	}

	return -1;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 GetTraceChain(PointRecord * PolygonPoints)
{
	int32 cnt, cnt2, NrTracePointsNumbers, ObjectNr3, MinNrTraces, TraceCnt, Found, Stop;

	ObjectRecord *Object;
	double NewX, NewY, TraceLength, x1, y1, x2, y2;
	TracePointRecord *TracePoint;


	TracePoints = (TracePointsArray *) & TracePointBuf;
	NrTracePoints = 0;
	NrTracePointsNumbers = 0;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object = &((*Objects6)[cnt]);
		x1 = Object->x1;
		y1 = Object->y1;
		x2 = Object->x2;
		y2 = Object->y2;
		Object->Info = 0;

		if ((cnt2 = GetTracePointNr(x1, y1)) == -1)
		{
			InsertObjectPoint(x1, y1);
			TracePoint = &((*TracePoints)[NrTracePoints - 1]);
		}
		else
			TracePoint = &((*TracePoints)[cnt2]);

		TracePoint->NrTraces++;

		if ((cnt2 = GetTracePointNr(x2, y2)) == -1)
		{
			InsertObjectPoint(x2, y2);
			TracePoint = &((*TracePoints)[NrTracePoints - 1]);
		}
		else
			TracePoint = &((*TracePoints)[cnt2]);

		TracePoint->NrTraces++;
	}

// Find first point which is connected to two traces
	Found = 0;
	TraceLength = 0.0;
	TraceCnt = -1;
	MinNrTraces = 10000;

	for (cnt = 0; cnt < NrTracePoints; cnt++)
	{
		TracePoint = &((*TracePoints)[cnt]);

		if (TracePoint->NrTraces < MinNrTraces)
		{
			MinNrTraces = TracePoint->NrTraces;
			TraceCnt = cnt;
		}
	}

	if (TraceCnt == -1)
		return 0;

	cnt = TraceCnt;
	TracePointsNumbers[NrTracePointsNumbers++] = cnt;
//  Object=&((*Objects2)[cnt]);
//  Object->Info|=1;

	Stop = 0;

	while (!Stop)
	{
		TracePoint = &((*TracePoints)[cnt]);
		ObjectNr3 = GetTraceObjectNrFromEndPoint(TracePoint->x, TracePoint->y, &NewX, &NewY);

		if (ObjectNr3 != -1)
		{
			Object = &((*Objects6)[ObjectNr3]);
			Object->Info |= 1;
			cnt = GetTracePointNr(NewX, NewY);

			if (cnt != -1)
				TracePointsNumbers[NrTracePointsNumbers++] = cnt;
			else
				Stop = 1;
		}
		else
			Stop = 1;
	}

	ok = 1;
	return NrTracePointsNumbers;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LPSTR GetMentorLayer(int32 Layer)
{
	int32 ok;

	switch (Layer)
	{
	case 0:
		return BottomLayer;

	case SOLD_MASK_TOP:
		return SolderMaskTopLayer;

	case SOLD_MASK_BOTTOM:
		return SolderMaskBottomLayer;

	case PASTE_MASK_TOP:
		return PasteMaskTopLayer;

	case PASTE_MASK_BOTTOM:
		return PasteMaskBottomLayer;

	case SILKSCREEN_BOTTOM:
		if (CurrentShape->Info & 1)
		{
			// SMD
			return SilkscreenLayer;
		}
		else
		{
			// Pads on top/bottom
			return SilkscreenBottomLayer;
		}

		break;

	case SILKSCREEN_TOP:
		if (CurrentShape->Info & 1)
		{
			// SMD
			return SilkscreenLayer;
		}
		else
		{
			// Pads on top/bottom
			return SilkscreenTopLayer;
		}

		break;

	case PLACEMENT_OUTLINE_TOP:
		return PlacementOutlineLayer;

	case COMP_OUTLINE_LAYER:
		return ComponentOutlineLayer;

	case ROUTING_KEEPOUT_LAYER:
		return RoutingKeepoutLayer;

	case INFO_LAYER:
		return "INFO_1";

	case INFO_LAYER2:
		return "INFO_2";

	case INFO_LAYER3:
		return "INFO_3";

	case INFO_LAYER4:
		return "INFO_4";

	case INNER_PAD_LAYER:
		ok = 1;
		break;

	case POWER_PAD_LAYER:
		ok = 1;
		break;
	}

	if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		return RoutingKeepoutLayer;

	if (Layer == NrShapeLayers - 1)
		return TopLayer;

	if ((Layer > 0) && (Layer < NrShapeLayers - 1))
		return InnerLayers[NrShapeLayers - 2 - Layer];

	return "Not found";
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintCircle(int32 fp, LPSTR Layer, double x, double y, double Width, double LineWidth)
{
	char str2[200];

	/*
	$$circle( "SIGNAL", 0.0, 0.0, 0.044, 0.0);
	*/

	sprintf(str2, "$$circle( \"%s\", %.4f, %.4f, %.4f, %.4f );", Layer, x / UNITS_MM, y / UNITS_MM, Width / UNITS_MM,
	        LineWidth / UNITS_MM);
	WriteLn(fp, str2);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintArc(int32 fp, LPSTR Layer, double x1, double y1, double x3, double y3, double x4, double y4,
                     double LineWidth, int32 mode)
{
	char str2[200];

	/*
	                sprintf(str2,"$$arc( \"%s\", %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, \"\", @center, @radius );",
	                        GetMentorLayer(PinObject->Layer),
	                        (PinObject->x1+PinObject->x3)/UNITS_MM,
	                        (PinObject->y1+PinObject->y3)/UNITS_MM,
	                        (PinObject->x1+PinObject->x4)/UNITS_MM,
	                        (PinObject->y1+PinObject->y4)/UNITS_MM,
	                        PinObject->x1/UNITS_MM,PinObject->y1/UNITS_MM,
	                        PinObject->Thickness/UNITS_MM);


	*/

	sprintf(str2, "$$arc( \"%s\", %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, \"\", @center, @radius );", Layer,
	        (x1 + x3) / UNITS_MM, (y1 + y3) / UNITS_MM, (x1 + x4) / UNITS_MM, (y1 + y4) / UNITS_MM, x1 / UNITS_MM,
	        y1 / UNITS_MM, LineWidth / UNITS_MM);
	WriteLn(fp, str2);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintPolygon(int32 fp, LPSTR Layer, PointRecord * Points, double ox, double oy, int32 NrVertices,
                         int32 mode)
{
	char str2[200];
	int32 cnt;

	/*
	$$initial([216.0,230.0], , @nosnap );
	$$terminal([447.4,423.7] );
	$$polygon( "DRAWING_1" );
	*/
	for (cnt = 0; cnt < NrVertices; cnt++)
	{
		if (cnt == 0)
		{
			sprintf(str2, "$$initial([%.4f,%.4f], , @nosnap );", (Points->x + ox) / UNITS_MM,
			        (Points->y + oy) / UNITS_MM);
		}
		else
			sprintf(str2, "$$terminal([%.4f,%.4f] );", (Points->x + ox) / UNITS_MM, (Points->y + oy) / UNITS_MM);

		WriteLn(fp, str2);
		Points++;
	}

	if (mode == 0)
	{
		sprintf(str2, "$$polygon( \"%s\" );", Layer);
		WriteLn(fp, str2);
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintRectPad(int32 fp, LPSTR Layer, double x, double y, double Width, double Height)
{
	double x1, y1;
	char str2[200];
	/*
	$$polygon( "PAD_1", , [-0.012, -0.03, 0.012, -0.03, 0.012, 0.03, -0.012, 0.03] );
	*/
	x1 = Width * 0.5 / UNITS_MM;
	y1 = Height * 0.5 / UNITS_MM;
	x /= UNITS_MM;
	y /= UNITS_MM;
	sprintf(str2, "$$polygon( \"%s\", , [%.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f] );", Layer, x - x1, y - y1,
	        x + x1, y - y1, x + x1, y + y1, x - x1, y + y1);
	WriteLn(fp, str2);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintRectPad2(int32 fp, double x, double y, double Width, double Height)
{
	int32 cnt;
	char str2[200], Buf[1024];
	PointRecord *Points;
	Points = (PointRecord *) & Buf;
	/*
	$$polygon( "PAD_1", , [-0.012, -0.03, 0.012, -0.03, 0.012, 0.03, -0.012, 0.03] );
	*/

	Width *= 0.5;
	Height *= 0.5;
	Points[0].x = x - Width;
	Points[0].y = y - Height;
	Points[1].x = x + Width;
	Points[1].y = y - Height;
	Points[2].x = x + Width;
	Points[2].y = y + Height;
	Points[3].x = x - Width;
	Points[3].y = y + Height;

	for (cnt = 0; cnt < 4; cnt++)
	{
		if (cnt == 0)
			sprintf(str2, "$$initial([%.4f,%.4f], , @nosnap );", Points[cnt].x / UNITS_MM, Points[cnt].y / UNITS_MM);
		else
			sprintf(str2, "$$terminal([%.4f,%.4f] );", Points[cnt].x / UNITS_MM, Points[cnt].y / UNITS_MM);

		WriteLn(fp, str2);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintDrill(int32 fp, double x, double y, double Width, int32 mode)
{
	char str2[200];

	/*
	$$attribute( "TERMINAL_DRILL_SIZE", "", , @scale , , [26.0, 0.0]);
	*/
	if (mode == 0)
	{
		// Plated
		if ((x == 0.0) && (y == 0.0))
		{
			sprintf(str2, "$$attribute( \"TERMINAL_DRILL_SIZE\", \"\", , @scale , , [%.4f, %.4f]);", Width / UNITS_MM,
			        0.0);
		}
		else
		{
			sprintf(str2, "$$attribute( \"TERMINAL_DRILL_SIZE\", \"%.2f\", , @scale , , [%.4f, %.4f]);",
			        Width / UNITS_MM, x / UNITS_MM, y / UNITS_MM);
		}
	}
	else
	{
		// Unplated
		/*
		$$attribute( "DRILL_DEFINITION_UNPLATED", "119.0", , @scale , , [1670.0, -197.0]);
		*/
		if ((x == 0.0) && (y == 0.0))
		{
			sprintf(str2, "$$attribute( \"DRILL_DEFINITION_UNPLATED\", \"\", , @scale , , [%.4f, %.4f]);",
			        Width / UNITS_MM, 0.0);
		}
		else
		{
			sprintf(str2, "$$attribute( \"DRILL_DEFINITION_UNPLATED\", \"%.1f\", , @scale , , [%.4f, %.4f]);",
			        Width / UNITS_MM, x / UNITS_MM, y / UNITS_MM);
		}
	}

	WriteLn(fp, str2);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintPath(int32 fp, LPSTR Layer, double x1, double y1, double x2, double y2, double Width)
{
	/*
	$$path( "VIA_HELP", 0.0, , [7.108, 3.358, 7.06, 3.3] );
	*/
	char str2[200];

	if (MentorUnits == 1)
	{
		sprintf(str2, "$$path( \"%s\", %.1f, , [%.1f, %.1f, %.1f, %.1f]);", Layer, Width / 2540.0, x1 / 2540.0,
		        y1 / 2540.0, x2 / 2540.0, y2 / 2540.0);
	}
	else
	{
		sprintf(str2, "$$path( \"%s\", %.4f, , [%.4f, %.4f, %.4f, %.4f]);", Layer, Width / UNITS_MM, x1 / UNITS_MM,
		        y1 / UNITS_MM, x2 / UNITS_MM, y2 / UNITS_MM);
	}

	WriteLn(fp, str2);
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MentorPrintText(int32 fp, ObjectRecord * PinObject)
{
	char str3[200], str2[200], str[200], AlignmentStr[200], str4[40], *TextP;
	double JustX, JustY, Thickness;
	int32 BottomLayer = 0, cnt;

	if ((PinObject->RotationAngle < 90.001) || (PinObject->RotationAngle > 270.0))
	{
		strcpy(AlignmentStr, "@BL");
		strcpy(str4, "");

		if ((PinObject->RotationAngle < 45.001) || (PinObject->RotationAngle > 314.999))
		{
			JustX = 1.0;
			JustY = 0.0;
		}
		else
		{
			JustX = 0.0;
			JustY = 1.0;
		}

		sprintf(str3, "%.1f", PinObject->RotationAngle);
	}
	else
	{
//    strcpy(AlignmentStr,"@TR");
		strcpy(AlignmentStr, "@BL");
		strcpy(str4, "@noright_reading");

		if ((PinObject->RotationAngle > 134.999) && (PinObject->RotationAngle < 225.0))
		{
			JustX = -1.0;
			JustY = 0.0;
		}
		else
		{
			JustX = 0.0;
			JustY = -1.0;
		}

		sprintf(str3, "%.1f", PinObject->RotationAngle);
//    sprintf(str3,"%.1f",PinObject->RotationAngle-180.0);
	}

#ifdef _DEBUG

	if (strcmp(CurrentShapeNameP, "HEADER2X3_BOX2") == 0)
	{
		ok = 1;

		if (stricmp(PinObject->Text1, "") == 0)
			ok = 1;
	}

#endif

	if (CurrentShape->Info & 1)
	{
		// SMD
		if (PinObject->Layer == SILKSCREEN_BOTTOM)
			return -1;
	}

	TextP = PinObject->Text1;

	if (stricmpUTF8(CurrentShapeNameP, TextP) == 0)
	{
		switch (PinObject->Layer)
		{
		case COMP_OUTLINE_LAYER_TOP:
		case COMP_OUTLINE_LAYER_BOTTOM:
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM:
			return -1;
		}

		TextP = "^$ref";
	}

	if (strcmp(TextP, "^$ref") == 0)
	{
		switch (PinObject->Layer)
		{
		case COMP_OUTLINE_LAYER_TOP:
		case COMP_OUTLINE_LAYER_BOTTOM:
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM:
			return -1;
		}

		for (cnt = 0; cnt < 16; cnt++)
		{
			if ((UsedRefLayers[cnt] != 0) && (UsedRefLayers[cnt] == PinObject->Layer))
			{
				// On each individual layer only one ^$ref
				return -1;
			}
		}

		for (cnt = 0; cnt < 16; cnt++)
		{
			if (UsedRefLayers[cnt] == 0)
			{
				UsedRefLayers[cnt] = PinObject->Layer;
				break;
			}
		}
	}

	Thickness = PinObject->Thickness / UNITS_MM;

	if ((PinObject->Layer == SILKSCREEN_TOP) || (PinObject->Layer == SILKSCREEN_BOTTOM))
		Thickness = SILKSCREEN_TEXT_THICKNESS;

	sprintf(str2, "$$text( \"%s\", \"%s\", %.4f, %.4f, %.4f, %s, %s, %s, %.4f, \"std\", \"None\", 0.0, 0.0",
	        GetMentorLayer(PinObject->Layer), TextP,
	        PinObject->x1 / UNITS_MM - PinObject->x2 / UNITS_MM * MultTextOffset * JustX,
	        PinObject->y1 / UNITS_MM - PinObject->x2 / UNITS_MM * MultTextOffset * JustY,
	        (PinObject->x2 * MultTextY) / UNITS_MM, AlignmentStr, str3, MentorAspectRatio, Thickness);

	switch (PinObject->Layer)
	{
	case SOLD_MASK_TOP:
	case PASTE_MASK_TOP:
	case SILKSCREEN_TOP:
	case PLACEMENT_OUTLINE_TOP:
	case COMP_OUTLINE_LAYER:
	case ROUTING_KEEPOUT_LAYER:
		break;

	case 0:
	case SOLD_MASK_BOTTOM:
	case PASTE_MASK_BOTTOM:
	case SILKSCREEN_BOTTOM:
		BottomLayer = 1;
		break;

	case INFO_LAYER:
	case INFO_LAYER2:
	case INFO_LAYER3:
	case INFO_LAYER4:
		break;
	}

	if (BottomLayer == 0)
	{
		if (PinObject->Mirror == 0)
		{
			if (str4[0] != 0)
				sprintf(str, "%s, , ,%s);", str2, str4);
			else
				sprintf(str, "%s);", str2);
		}
		else
		{
			if (str4[0] != 0)
				sprintf(str, "%s, ,@mirror,%s);", str2, str4);
			else
				sprintf(str, "%s, ,@mirror);", str2);
		}
	}
	else
	{
		if (PinObject->Mirror == 0)
		{
			if (str4[0] != 0)
				sprintf(str, "%s, ,@nomirror ,%s);", str2, str4);
			else
				sprintf(str, "%s, ,@nomirror);", str2);
		}
		else
		{
			if (str4[0] != 0)
				sprintf(str, "%s, , ,%s);", str2, str4);
			else
				sprintf(str, "%s);", str2);
		}
	}

	WriteLn(fp, str);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddMentorShape(LPSTR ShapeName)
{
	int32 res;

	res = LoadShape(ShapeName);
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SetPinName(int32 index, LPSTR PinName)
{
	char str3[200];
	int32 cnt2, cnt3;

	cnt3 = 0;

	while (1)
	{
		if (cnt3 > 0)
			sprintf(str3, "%s_%d", PinName, cnt3 + 1);
		else
			strcpy(str3, PinName);

		for (cnt2 = 0; cnt2 < NrMentorPins; cnt2++)
		{
			if (strcmpUTF8(MentorPins[cnt2].PadStackName, str3) == 0)
				break;
		}

		if (cnt2 == NrMentorPins)
		{
			strcpy(MentorPins[index].PadStackName, str3);
			break;
		}

		cnt3++;

		if (cnt3 == 1000)
			break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreatePadStackNames(int32 mode)
{
	int32 cnt;
	char str2[200];

	for (cnt = 0; cnt < NrMentorPins; cnt++)
	{
#ifdef _DEBUG

		if (cnt == 13)
			ok = 1;

#endif

		if (MentorPins[cnt].DrillWidth)
		{
			// Through hole pin
			if (MentorPins[cnt].TopObjectType == PIN_SMD_ROUND)
			{
				sprintf(str2, "%03drd%03d", (int32) ((MentorPins[cnt].TopPadWidth + 500.0) / 1000.0),
				        (int32) ((MentorPins[cnt].DrillWidth + 500.0) / 1000.0));
			}

			if (MentorPins[cnt].TopObjectType == PIN_SMD_RECT)
			{
				if (InRange2(MentorPins[cnt].TopPadWidth, MentorPins[cnt].TopPadHeight))
				{
					sprintf(str2, "%03dsq%03d", (int32) (MentorPins[cnt].TopPadWidth / 1000.0),
					        (int32) (MentorPins[cnt].DrillWidth / 1000.0));
				}
				else
				{
					sprintf(str2, "%03dx%03dre%03d", (int32) (MentorPins[cnt].TopPadWidth / 1000.0),
					        (int32) (MentorPins[cnt].TopPadHeight / 1000.0),
					        (int32) (MentorPins[cnt].DrillWidth / 1000.0));
				}
			}
		}
		else
		{
			// SMD pad
			if (MentorPins[cnt].TopObjectType)
			{
				if (MentorPins[cnt].TopObjectType == PIN_SMD_ROUND)
					sprintf(str2, "smd_%03d", (int32) (MentorPins[cnt].TopPadWidth / 1000.0));
				else
				{
					sprintf(str2, "smd_%03dx%03d", (int32) (MentorPins[cnt].TopPadWidth / 1000.0),
					        (int32) (MentorPins[cnt].TopPadHeight / 1000.0));
				}
			}
			else
			{
				if (MentorPins[cnt].BottomObjectType == PIN_SMD_ROUND)
					sprintf(str2, "smd_%03d", (int32) (MentorPins[cnt].BottomPadWidth / 1000.0));
				else
				{
					sprintf(str2, "smd_%03dx%03d", (int32) (MentorPins[cnt].BottomPadWidth / 1000.0),
					        (int32) (MentorPins[cnt].BottomPadHeight / 1000.0));
				}
			}
		}

#ifdef _DEBUG

		if (stricmp(str2, "smd_000x000") == 0)
			ok = 1;

#endif
		SetPinName(cnt, str2);
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 OutputPadStack(int32 fp, LPSTR ShapeName, MentorPinRecord * MentorPin)
{
	/*
	$$create_pin("040rd026");
	$$page(0.0, 0.0, 0.0, @mils, 0.0, 0.0, [0.0,0.0,'PI$040rd026'] );
	$$point_mode(@vertex);
	*/
	char str3[200];
	int32 Printed;
	double InnerPadWidth;

	if (MentorPin->Info & 1)
		return 0;

#ifdef _DEBUG

	if (MentorPin->Info3 == 1000000)
		ok = 1;

	if (stricmp(MentorPin->PadStackName, "smd_220x330_2") == 0)
		ok = 1;

#endif
	sprintf(str3, "$$create_pin(\"%s\");", MentorPin->PadStackName);
	WriteLn(fp, str3);

	if (MentorUnits == 1)
	{
		sprintf(str3, "$$page(0.0, 0.0, 0.0, @mils, 0.0, 0.0, [0.0,0.0,'PI$%s'] );", MentorPin->PadStackName);
		WriteLn(fp, str3);
	}
	else
	{
		sprintf(str3, "$$page(0.0000, 0.0000, 0.0000, @mm, 0.0000, 0.0000, [0.0000,0.0000,'PI$%s'] );",
		        MentorPin->PadStackName);
		WriteLn(fp, str3);
	}

	WriteLn(fp, "$$point_mode(@vertex);");

	if (MentorPin->DrillWidth)
	{
		/*
		$$attribute( "TERMINAL_THRUHOLE_DEFINITION", "");
		*/
		WriteLn(fp, "$$attribute( \"TERMINAL_THRUHOLE_DEFINITION\", \"\");");
		// Through hole pin
		MentorPrintDrill(fp, 0.0, 0.0, MentorPin->DrillWidth, 0);

		Printed = 0;

		if ((MentorPin->TopPadWidth) && (MentorPin->BottomPadWidth))
		{
			if ((MentorPin->TopObjectType == PIN_SMD_ROUND) && (MentorPin->BottomObjectType == PIN_SMD_ROUND))
			{
				if (InRange(MentorPin->TopPadWidth, MentorPin->BottomPadWidth))
				{
					MentorPrintCircle(fp, "PAD", 0.0, 0.0, MentorPin->TopPadWidth, 0.0);
					Printed = 1;
				}
				else
				{
					MentorPrintCircle(fp, "PAD_1", 0.0, 0.0, MentorPin->TopPadWidth, 0.0);
					MentorPrintCircle(fp, "PAD_2", 0.0, 0.0, MentorPin->BottomPadWidth, 0.0);
					Printed = 1;
				}
			}

			if ((MentorPin->TopObjectType == PIN_SMD_RECT) && (MentorPin->BottomObjectType == PIN_SMD_RECT))
			{
				if ((InRange(MentorPin->TopPadWidth, MentorPin->BottomPadWidth))
				        && (InRange(MentorPin->TopPadHeight, MentorPin->BottomPadHeight)))
				{
					MentorPrintRectPad(fp, "PAD", 0.0, 0.0, MentorPin->TopPadWidth, MentorPin->TopPadHeight);
					Printed = 1;
				}
				else
				{
					MentorPrintRectPad(fp, "PAD_1", 0.0, 0.0, MentorPin->TopPadWidth, MentorPin->TopPadHeight);
					MentorPrintRectPad(fp, "PAD_2", 0.0, 0.0, MentorPin->BottomPadWidth, MentorPin->BottomPadHeight);
					Printed = 1;
				}
			}
		}

		if (!Printed)
		{
			if (MentorPin->TopPadWidth)
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
				{
					MentorPrintCircle(fp, "PAD_1", 0.0, 0.0, MentorPin->TopPadWidth, 0.0);
					Printed = 1;
				}

				if (MentorPin->TopObjectType == PIN_SMD_RECT)
				{
					MentorPrintRectPad(fp, "PAD_1", 0.0, 0.0, MentorPin->TopPadWidth, MentorPin->TopPadHeight);
					Printed = 1;
				}
			}

			if (MentorPin->BottomPadWidth)
			{
				if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
				{
					MentorPrintCircle(fp, "PAD_2", 0.0, 0.0, MentorPin->BottomPadWidth, 0.0);
					Printed = 1;
				}

				if (MentorPin->BottomObjectType == PIN_SMD_RECT)
				{
					MentorPrintRectPad(fp, "PAD_2", 0.0, 0.0, MentorPin->BottomPadWidth, MentorPin->BottomPadHeight);
					Printed = 1;
				}
			}
		}

		InnerPadWidth = MentorPin->InnerPadWidth;

		if (InnerPadWidth == 0.0)
			InnerPadWidth = max(MentorPin->TopPadWidth, MentorPin->BottomPadWidth);

		MentorPrintCircle(fp, "SIGNAL", 0.0, 0.0, InnerPadWidth, 0.0);

		if (MentorPin->PowerPadWidth)
			MentorPrintCircle(fp, "POWER", 0.0, 0.0, MentorPin->PowerPadWidth, 0.0);
		else
			MentorPrintCircle(fp, "POWER", 0.0, 0.0, InnerPadWidth, 0.0);

		Printed = 0;

		if ((MentorPin->TopSolderMaskWidth) && (MentorPin->BottomSolderMaskWidth))
		{
			if ((MentorPin->TopObjectType == PIN_SMD_ROUND) && (MentorPin->BottomObjectType == PIN_SMD_ROUND))
			{
				if (InRange(MentorPin->TopSolderMaskWidth, MentorPin->BottomSolderMaskWidth))
				{
					MentorPrintCircle(fp, "SOLDER_MASK", 0.0, 0.0, MentorPin->TopSolderMaskWidth, 0.0);
					Printed = 1;
				}
				else
				{
					MentorPrintCircle(fp, "SOLDER_MASK_1", 0.0, 0.0, MentorPin->TopSolderMaskWidth, 0.0);
					MentorPrintCircle(fp, "SOLDER_MASK_2", 0.0, 0.0, MentorPin->BottomSolderMaskWidth, 0.0);
					Printed = 1;
				}
			}

			if ((MentorPin->TopObjectType == PIN_SMD_RECT) && (MentorPin->BottomObjectType == PIN_SMD_RECT))
			{
				if ((InRange(MentorPin->TopSolderMaskWidth, MentorPin->BottomSolderMaskWidth))
				        && (InRange(MentorPin->TopSolderMaskHeight, MentorPin->BottomSolderMaskHeight)))
				{
					MentorPrintRectPad(fp, "SOLDER_MASK", 0.0, 0.0, MentorPin->TopSolderMaskWidth,
					                   MentorPin->TopSolderMaskHeight);
					Printed = 1;
				}
				else
				{
					MentorPrintRectPad(fp, "SOLDER_MASK_1", 0.0, 0.0, MentorPin->TopSolderMaskWidth,
					                   MentorPin->TopSolderMaskHeight);
					MentorPrintRectPad(fp, "SOLDER_MASK_2", 0.0, 0.0, MentorPin->BottomSolderMaskWidth,
					                   MentorPin->BottomSolderMaskHeight);
					Printed = 1;
				}
			}
		}

		if (!Printed)
		{
			if (MentorPin->TopSolderMaskWidth)
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
				{
					MentorPrintCircle(fp, "SOLDER_MASK_1", 0.0, 0.0, MentorPin->TopSolderMaskWidth, 0.0);
					Printed = 1;
				}

				if (MentorPin->TopObjectType == PIN_SMD_RECT)
				{
					MentorPrintRectPad(fp, "SOLDER_MASK_1", 0.0, 0.0, MentorPin->TopSolderMaskWidth,
					                   MentorPin->TopSolderMaskHeight);
					Printed = 1;
				}
			}

			if (MentorPin->BottomSolderMaskWidth)
			{
				if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
				{
					MentorPrintCircle(fp, "SOLDER_MASK_2", 0.0, 0.0, MentorPin->BottomSolderMaskWidth, 0.0);
					Printed = 1;
				}

				if (MentorPin->BottomObjectType == PIN_SMD_RECT)
				{
					MentorPrintRectPad(fp, "SOLDER_MASK_2", 0.0, 0.0, MentorPin->BottomSolderMaskWidth,
					                   MentorPin->BottomSolderMaskHeight);
					Printed = 1;
				}
			}
		}

		Printed = 0;

		if ((MentorPin->TopPasteMaskWidth) && (MentorPin->BottomPasteMaskWidth))
		{
			if ((MentorPin->TopObjectType == PIN_SMD_ROUND) && (MentorPin->BottomObjectType == PIN_SMD_ROUND))
			{
				if (InRange(MentorPin->TopPasteMaskWidth, MentorPin->BottomPasteMaskWidth))
				{
					MentorPrintCircle(fp, "PASTE_MASK", 0.0, 0.0, MentorPin->TopPasteMaskWidth, 0.0);
					Printed = 1;
				}
				else
				{
					MentorPrintCircle(fp, "PASTE_MASK_1", 0.0, 0.0, MentorPin->TopPasteMaskWidth, 0.0);
					MentorPrintCircle(fp, "PASTE_MASK_2", 0.0, 0.0, MentorPin->BottomPasteMaskWidth, 0.0);
					Printed = 1;
				}
			}

			if ((MentorPin->TopObjectType == PIN_SMD_RECT) && (MentorPin->BottomObjectType == PIN_SMD_RECT))
			{
				if ((InRange(MentorPin->TopPasteMaskWidth, MentorPin->BottomPasteMaskWidth))
				        && (InRange(MentorPin->TopPasteMaskHeight, MentorPin->BottomPasteMaskHeight)))
				{
					MentorPrintRectPad(fp, "PASTE_MASK", 0.0, 0.0, MentorPin->TopPasteMaskWidth,
					                   MentorPin->TopPasteMaskHeight);
					Printed = 1;
				}
				else
				{
					MentorPrintRectPad(fp, "PASTE_MASK_1", 0.0, 0.0, MentorPin->TopPasteMaskWidth,
					                   MentorPin->TopPasteMaskHeight);
					MentorPrintRectPad(fp, "PASTE_MASK_2", 0.0, 0.0, MentorPin->BottomPasteMaskWidth,
					                   MentorPin->BottomPasteMaskHeight);
					Printed = 1;
				}
			}
		}

		if (!Printed)
		{
			if (MentorPin->TopPasteMaskWidth)
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
				{
					MentorPrintCircle(fp, "PASTE_MASK_1", 0.0, 0.0, MentorPin->TopPasteMaskWidth, 0.0);
					Printed = 1;
				}

				if (MentorPin->TopObjectType == PIN_SMD_RECT)
				{
					MentorPrintRectPad(fp, "PASTE_MASK_1", 0.0, 0.0, MentorPin->TopPasteMaskWidth,
					                   MentorPin->TopPasteMaskHeight);
					Printed = 1;
				}
			}

			if (MentorPin->BottomPasteMaskWidth)
			{
				if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
				{
					MentorPrintCircle(fp, "PASTE_MASK_2", 0.0, 0.0, MentorPin->BottomPasteMaskWidth, 0.0);
					Printed = 1;
				}

				if (MentorPin->BottomObjectType == PIN_SMD_RECT)
				{
					MentorPrintRectPad(fp, "PASTE_MASK_2", 0.0, 0.0, MentorPin->BottomPasteMaskWidth,
					                   MentorPin->BottomPasteMaskHeight);
					Printed = 1;
				}
			}
		}
	}
	else
	{
		// SMD pad
		if (MentorPin->Info & 2)
		{
			// SMD pad
			WriteLn(fp, "$$attribute( \"TERMINAL_SURFACE_DEFINITION\", \"\");");

			if (MentorPin->TopPadWidth)
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "PAD", 0.0, 0.0, MentorPin->TopPadWidth, 0.0);
				else
					MentorPrintRectPad(fp, "PAD", 0.0, 0.0, MentorPin->TopPadWidth, MentorPin->TopPadHeight);

				if (MentorPin->TopPasteMaskWidth)
				{
					if (MentorPin->TopObjectType == PIN_SMD_ROUND)
						MentorPrintCircle(fp, "PASTE_MASK", 0.0, 0.0, MentorPin->TopPasteMaskWidth, 0.0);
					else
					{
						MentorPrintRectPad(fp, "PASTE_MASK", 0.0, 0.0, MentorPin->TopPasteMaskWidth,
						                   MentorPin->TopPasteMaskHeight);
					}
				}

				if (MentorPin->TopSolderMaskWidth)
				{
					if (MentorPin->TopObjectType == PIN_SMD_ROUND)
						MentorPrintCircle(fp, "SOLDER_MASK", 0.0, 0.0, MentorPin->TopSolderMaskWidth, 0.0);
					else
					{
						MentorPrintRectPad(fp, "SOLDER_MASK", 0.0, 0.0, MentorPin->TopSolderMaskWidth,
						                   MentorPin->TopSolderMaskHeight);
					}
				}
			}

			if (MentorPin->BottomPadWidth)
			{
				if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "PAD", 0.0, 0.0, MentorPin->BottomPadWidth, 0.0);
				else
					MentorPrintRectPad(fp, "PAD", 0.0, 0.0, MentorPin->BottomPadWidth, MentorPin->BottomPadHeight);

				if (MentorPin->BottomPasteMaskWidth)
				{
					if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
						MentorPrintCircle(fp, "PASTE_MASK", 0.0, 0.0, MentorPin->BottomPasteMaskWidth, 0.0);
					else
					{
						MentorPrintRectPad(fp, "PASTE_MASK", 0.0, 0.0, MentorPin->BottomPasteMaskWidth,
						                   MentorPin->BottomPasteMaskHeight);
					}
				}

				if (MentorPin->BottomSolderMaskWidth)
				{
					if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
						MentorPrintCircle(fp, "SOLDER_MASK", 0.0, 0.0, MentorPin->BottomSolderMaskWidth, 0.0);
					else
					{
						MentorPrintRectPad(fp, "SOLDER_MASK", 0.0, 0.0, MentorPin->BottomSolderMaskWidth,
						                   MentorPin->BottomSolderMaskHeight);
					}
				}
			}
		}
		else
		{
			// Shape with top/bottom SMD pads
			if (MentorPin->TopObjectType)
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "PAD_1", 0.0, 0.0, MentorPin->TopPadWidth, 0.0);
				else
					MentorPrintRectPad(fp, "PAD_1", 0.0, 0.0, MentorPin->TopPadWidth, MentorPin->TopPadHeight);
			}

			if (MentorPin->BottomObjectType)
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "PAD_2", 0.0, 0.0, MentorPin->TopPadWidth, 0.0);
				else
					MentorPrintRectPad(fp, "PAD_2", 0.0, 0.0, MentorPin->TopPadWidth, MentorPin->TopPadHeight);
			}

			if ((MentorPin->TopObjectType) && (MentorPin->TopPasteMaskWidth))
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "PASTE_MASK_1", 0.0, 0.0, MentorPin->TopPasteMaskWidth, 0.0);
				else
				{
					MentorPrintRectPad(fp, "PASTE_MASK_1", 0.0, 0.0, MentorPin->TopPasteMaskWidth,
					                   MentorPin->TopPasteMaskHeight);
				}
			}

			if ((MentorPin->BottomObjectType) && (MentorPin->BottomPasteMaskWidth))
			{
				if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "PASTE_MASK_2", 0.0, 0.0, MentorPin->TopPasteMaskWidth, 0.0);
				else
				{
					MentorPrintRectPad(fp, "PASTE_MASK_2", 0.0, 0.0, MentorPin->TopPasteMaskWidth,
					                   MentorPin->TopPasteMaskHeight);
				}
			}

			if ((MentorPin->TopObjectType) && (MentorPin->TopSolderMaskWidth))
			{
				if (MentorPin->TopObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "SOLDER_MASK_1", 0.0, 0.0, MentorPin->TopSolderMaskWidth, 0.0);
				else
				{
					MentorPrintRectPad(fp, "SOLDER_MASK_1", 0.0, 0.0, MentorPin->TopSolderMaskWidth,
					                   MentorPin->TopSolderMaskHeight);
				}
			}

			if ((MentorPin->BottomObjectType) && (MentorPin->BottomSolderMaskWidth))
			{
				if (MentorPin->BottomObjectType == PIN_SMD_ROUND)
					MentorPrintCircle(fp, "SOLDER_MASK_2", 0.0, 0.0, MentorPin->BottomSolderMaskWidth, 0.0);
				else
				{
					MentorPrintRectPad(fp, "SOLDER_MASK_2", 0.0, 0.0, MentorPin->BottomSolderMaskWidth,
					                   MentorPin->BottomSolderMaskHeight);
				}
			}
		}
	}

	MentorPin->Info |= 1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddMainShapePin(ObjectRecord * PinObject, int32 mode)
{
	int32 cnt;

	if (((mode & 1) == 1) && (!PinObject->Text2))
	{
		// Objects with no pinname are being skipped
		return -1;
	}

	switch (PinObject->ObjectType)
	{
	case PIN_PUT_THROUGH_ROUND:
	case PIN_PUT_THROUGH_SQUARE:
	case PIN_SMD_RECT:
	case DRILL:
	case PIN_PUT_THROUGH_POLYGON:
	case PIN_LINE_HOR:
	case PIN_LINE_VER:
	case PIN_LINE_DIAG1:
	case PIN_LINE_DIAG2:
	case PIN_ARC:
	case PIN_LINE_ALL_ANGLE:
		break;

	case PIN_SMD_ROUND:
		break;

	case PIN_SMD_POLYGON:
		cnt = 1;
		break;

	case DRILL_UNPLATED:
	case OBJECT_LINE:
	case OBJECT_RECT:
	case OBJECT_ARC:
	case OBJECT_TEXT:
	case OBJECT_TEXT2:
	case OBJECT_POLYGON:
		return -1;
	}

	for (cnt = 0; cnt < NrMainShapePins; cnt++)
	{
		if (stricmpUTF8(MainShapePins[cnt].PinName, PinObject->Text2) == 0)
		{
			if ((PinObject->Layer == -1) && (MainShapePins[cnt].Layer != -1))
			{
				MainShapePins[cnt].Layer = -1;
				MainShapePins[cnt].x = PinObject->x1;
				MainShapePins[cnt].y = PinObject->y1;
			}

			return cnt;
		}
	}

	MainShapePins[NrMainShapePins].x = PinObject->x1;
	MainShapePins[NrMainShapePins].y = PinObject->y1;
	strcpy(MainShapePins[NrMainShapePins].PinName, PinObject->Text2);
	MainShapePins[NrMainShapePins].Layer = PinObject->Layer;
	MainShapePins[NrMainShapePins].PinNr = -1;
	NrMainShapePins++;
	return NrMainShapePins - 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateMentorGeometries(int32 mode)
{
	ShapeRecord *Shape;
	struct tm *today;
	time_t ltime;
	int32 fp, cnt, cnt2 =
	    0, cnt3, cnt4, res, ok, NrPinObjects, NrMainPinObjects, Found, ExtendedPin, NrExtendedPins, Layer,
	    FoundThroughHolePin, ShapeSMD = 0, MainPinObjectsPos[1024], ActiveLayers, FirstObject;
	ObjectRecord *PinObject =
	    NULL, *PinObject2, TopPadObjectTemp, BottomPadObjectTemp, InnerPadObjectTemp, PowerPadObjectTemp,
	    DrillObjectTemp, *TopPadObject, *BottomPadObject, *ThroughHoleObject, *DrillObject, *InnerPadObject =
	        NULL, *PowerPadObject =
	            NULL, *SoldMaskTopObject, *SoldMaskBottomObject, *PasteMaskTopObject, *PasteMaskBottomObject;
	GeomPolygonRecord *GeomPolygon;
	double x, y, x1, y1, x2 = 0.0, y2 = 0.0, OX = 0.0, OY = 0.0, x3, y3, InnerPadWidth;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], *ShapeNameP, *PinNameP =
	    NULL, *DefaultPadStackName, *GeomLayer;
	PointRecord *PolygonPoints;

	NrObjects = 0;
	NrExtendedPins = 0;
	NrMentorPins = 0;
	NrMentorSpecialPins = 0;
	memset(MentorPins, 0, sizeof(MentorPins));
	memset(MentorSpecialPins, 0, sizeof(MentorSpecialPins));

	for (cnt = 0; cnt < PcbDesign.NrShapes; cnt++)
	{
		ShapeNameP = (*Shapes)[cnt].ShapeName;
		CurrentShapeNameP = ShapeNameP;
#ifdef _DEBUG

//    sprintf(str,"shape %s\r\n",(*Shapes)[cnt].ShapeName);
//    OutputDebugString(str);
		if (stricmp(ShapeNameP, "totx173") == 0)
			ok = 1;

#endif

		if (LoadShape(ShapeNameP) < 0)
		{
			sprintf(str, "Geometry %s could not be found\r\n", ShapeNameP);
			AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
			continue;
		}

		Shape = (ShapeRecord *) & ShapesMem[(*Shapes)[cnt].ShapePos];
		CurrentShape = Shape;
		(*Shapes)[cnt].ObjectPos = NrObjects;
		NrShapeLayers = Shape->NrLayers;
		ShapePinsToObject(Shape, 0.0, 0.0, 0.0, 0, 0, 1);	// Object->x3 = Inner pad, Object->y3 = Power pad
		NrPinObjects = NrObjects;
		NrMainShapePins = 0;
		ShapeOtherToObject(Shape, 0.0, 0.0, 0.0, 0, 1);	// sold/paste mask
		(*Shapes)[cnt].NrObjects = NrObjects - (*Shapes)[cnt].ObjectPos;
#ifdef _DEBUG

		if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
		{
			ok = 1;

			if (cnt2 == 20)
				ok = 1;
		}

#endif
		ShapeSMD = 0;
		ActiveLayers = 0;

		for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < NrObjects; cnt2++)
		{
			PinObject = &((*Objects)[cnt2]);

			switch (PinObject->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case DRILL:
			case PIN_PUT_THROUGH_POLYGON:
			case DRILL_UNPLATED:
				break;

			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
			case PIN_ARC:
			case PIN_LINE_ALL_ANGLE:
			case PIN_SMD_POLYGON:
			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
				if (PinObject->Layer == 0)
				{
					ShapeSMD = 1;
					ActiveLayers |= 1;
				}

				if (PinObject->Layer == NrShapeLayers - 1)
				{
					ShapeSMD = 1;
					ActiveLayers |= 2;
				}

				break;
			}
		}

		if (ShapeSMD)
			Shape->Info |= 1;
		else
			Shape->Info &= ~1;

#ifdef _DEBUG

		if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
		{
			ok = 1;

			if (cnt2 == 20)
				ok = 1;
		}

#endif

		for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < NrObjects; cnt2++)
		{
			PinObject = &((*Objects)[cnt2]);
#ifdef _DEBUG

			if (cnt2 == 24)
				ok = 1;

			if (stricmp(ShapeNameP, "pcat-bus") == 0)
			{
				ok = 1;

				if (cnt2 == 20)
					ok = 1;
			}

			if ((PinObject->Text2) && (strcmp(PinObject->Text2, "Layer2") == 0))
				ok = 1;

			switch (PinObject->ObjectType)
			{
			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case DRILL:
			case PIN_PUT_THROUGH_POLYGON:
			case DRILL_UNPLATED:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
			case PIN_ARC:
			case PIN_LINE_ALL_ANGLE:
				break;

			case PIN_SMD_POLYGON:
				ok = 1;
				break;

			case OBJECT_LINE:
			case OBJECT_RECT:
			case OBJECT_ARC:
			case OBJECT_TEXT:
			case OBJECT_TEXT2:
			case OBJECT_POLYGON:
				break;
			}

#endif
			PinObject->Info2 = AddMainShapePin(PinObject, 1);
			PinObject->Info3 = -1;
			PinObject->Info4 = -1;
			PinObject->Info &= ~1;

			if (ShapeSMD)
				PinObject->Info |= 2;
			else
				PinObject->Info &= ~2;

			PinObject->ShapeName = ShapeNameP;

			switch (PinObject->Layer)
			{
			case SOLD_MASK_TOP:
			case SOLD_MASK_BOTTOM:
			case PASTE_MASK_TOP:
			case PASTE_MASK_BOTTOM:
				switch (PinObject->ObjectType)
				{
				case OBJECT_ARC:
					if (PinObject->Info & OBJECT_FILLED)
						PinObject->ObjectType = PIN_SMD_ROUND;

					break;

				case OBJECT_RECT:
					if (PinObject->Info & OBJECT_FILLED)
						PinObject->ObjectType = PIN_SMD_RECT;

					break;
				}

				break;
			}

			res = 1;
		}

#ifdef _DEBUG

		if (cnt2 == 24)
			ok = 1;

		if (stricmp(ShapeNameP, "K0603") == 0)
		{
			ok = 1;

			if ((PinNameP) && (strcmp(PinNameP, "Layer2") == 0))
				ok = 1;
		}

#endif

		for (cnt3 = 0; cnt3 < NrMainShapePins; cnt3++)
		{
			ok = 1;
			x = MainShapePins[cnt3].x;
			y = MainShapePins[cnt3].y;
			Layer = MainShapePins[cnt3].Layer;
			NrMainPinObjects = 0;
			SoldMaskTopObject = NULL;
			SoldMaskBottomObject = NULL;
			PasteMaskTopObject = NULL;
			PasteMaskBottomObject = NULL;
			TopPadObject = NULL;
			BottomPadObject = NULL;
			DrillObject = NULL;
			InnerPadObject = NULL;
			ThroughHoleObject = NULL;
			PinNameP = MainShapePins[cnt3].PinName;
#ifdef _DEBUG

			if (cnt2 == 24)
				ok = 1;

			if (stricmp(ShapeNameP, "Mountinghole_pcat") == 0)
			{
				ok = 1;

				/*
				        sprintf(str,"- shape %s,pad %s,layer %d,xy %.0f,%.0f\r\n",PinObject->ShapeName,
				                PinNameP,Layer,x,y);
				        OutputDebugString(str);
				*/
				if (strcmp(PinNameP, "1") == 0)
					ok = 1;
			}

#endif
			ExtendedPin = 0;

			for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < NrObjects; cnt2++)
			{
				PinObject = &((*Objects)[cnt2]);
#ifdef _DEBUG

				if (cnt2 == 24)
					ok = 1;

#endif

				if (PinObject->Info2 == cnt3)
				{
					MainPinObjectsPos[NrMainPinObjects++] = cnt2;

					if (PinObject->Layer == 0)
					{
						if (!BottomPadObject)
							BottomPadObject = PinObject;
						else
							ExtendedPin = 1;
					}

					if (PinObject->Layer == NrShapeLayers - 1)
					{
						if (!TopPadObject)
							TopPadObject = PinObject;
						else
							ExtendedPin = 1;
					}

					if ((NrShapeLayers > 2) && (PinObject->Layer > 0) && (PinObject->Layer < NrShapeLayers - 1))
					{
						if (PinObject->ObjectType == PIN_SMD_ROUND)
						{
							if (!InnerPadObject)
								InnerPadObject = PinObject;
							else
							{
								if (PinObject->x2 > InnerPadObject->x2)
								{
									InnerPadObject->Info3 = 1000000;
									InnerPadObject = PinObject;
								}
								else
									PinObject->Info3 = 1000000;
							}
						}
						else
							ExtendedPin = 1;

						ok = 1;
					}

					switch (PinObject->ObjectType)
					{
					case PIN_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_SQUARE:
						if (!ThroughHoleObject)
							ThroughHoleObject = PinObject;
						else
							ExtendedPin = 1;

						break;

					case DRILL:
						if (!DrillObject)
							DrillObject = PinObject;
						else
							ExtendedPin = 1;

						break;

					case PIN_SMD_POLYGON:
						ExtendedPin = 1;
						break;

					case PIN_PUT_THROUGH_POLYGON:
					case PIN_ARC:
					case PIN_LINE_ALL_ANGLE:
					case PIN_LINE_HOR:
					case PIN_LINE_VER:
					case PIN_LINE_DIAG1:
					case PIN_LINE_DIAG2:
						ExtendedPin = 1;
						break;
					}

					ok = 1;
				}

#ifdef _DEBUG

				if (cnt2 == 24)
					ok = 1;

				if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
				{
					ok = 1;

					if ((InRange3(-8.0e5, PinObject->x1)) && (InRange3(-2.4e5, PinObject->y1)))
					{
						ok = 1;

						if (PinObject->Layer == PASTE_MASK_TOP)
							ok = 1;

						if (PinObject->ObjectType == DRILL)
							ok = 1;
					}

					if (strcmp(PinNameP, "Layer2") == 0)
						ok = 1;
				}

#endif

				if ((InRange2(x, PinObject->x1)) && (InRange2(y, PinObject->y1)))
				{
					switch (PinObject->Layer)
					{
					case SOLD_MASK_TOP:
						if ((Layer == -1) || (Layer == NrShapeLayers - 1))
						{
							if (PasteMaskBottomObject)
							{
								sprintf(str, "Geometry solder mask warning on postion (%.2f,%.2f mm) for shape %s\r\n",
								        x / UNITS_MM, y / UNITS_MM, ShapeNameP);
								AddMessage(str);
//                  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
							}

							SoldMaskTopObject = PinObject;
						}

						break;

					case SOLD_MASK_BOTTOM:
						if ((Layer == -1) || (Layer == 0))
						{
							if (PasteMaskBottomObject)
							{
								sprintf(str, "Geometry solder mask warning on postion (%.2f,%.2f mm) for shape %s\r\n",
								        x / UNITS_MM, y / UNITS_MM, ShapeNameP);
								AddMessage(str);
//                  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
							}

							SoldMaskBottomObject = PinObject;
						}

						break;

					case PASTE_MASK_TOP:
						if ((Layer == -1) || (Layer == NrShapeLayers - 1))
						{
							if (PasteMaskBottomObject)
							{
								sprintf(str, "Geometry paste mask warning on postion (%.2f,%.2f mm) for shape %s\r\n",
								        x / UNITS_MM, y / UNITS_MM, ShapeNameP);
								AddMessage(str);
//                  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
							}

							PasteMaskTopObject = PinObject;
						}

						break;

					case PASTE_MASK_BOTTOM:
						if ((Layer == -1) || (Layer == 0))
						{
							if (PasteMaskBottomObject)
							{
								sprintf(str, "Geometry paste mask warning on postion (%.2f,%.2f mm) for shape %s\r\n",
								        x / UNITS_MM, y / UNITS_MM, ShapeNameP);
								AddMessage(str);
//                  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
							}

							PasteMaskBottomObject = PinObject;
						}

						break;
					}
				}
			}

#ifdef _DEBUG

			if (cnt3 == 1)
				ok = 1;

#endif
#ifdef _DEBUG

			if (cnt2 == 24)
				ok = 1;

			if (stricmp(ShapeNameP, "pcat-bus") == 0)
			{
				ok = 1;			// NrMentorPins
			}

			if (stricmp(ShapeNameP, "so20") == 0)
			{
				ok = 1;

				if ((x == 0.0) && (y == 0.0))
					ok = 1;
			}

#endif

			if (!ExtendedPin)
			{
				if (ThroughHoleObject)
				{
					memset(&TopPadObjectTemp, 0, sizeof(ObjectRecord));
					memset(&BottomPadObjectTemp, 0, sizeof(ObjectRecord));
					memset(&InnerPadObjectTemp, 0, sizeof(ObjectRecord));
					memset(&PowerPadObjectTemp, 0, sizeof(ObjectRecord));
					memset(&DrillObjectTemp, 0, sizeof(ObjectRecord));

					switch (ThroughHoleObject->ObjectType)
					{
					case PIN_PUT_THROUGH_ROUND:
						TopPadObjectTemp.ObjectType = PIN_SMD_ROUND;
						TopPadObjectTemp.x2 = ThroughHoleObject->x2;
						TopPadObjectTemp.y2 = ThroughHoleObject->x2;
						BottomPadObjectTemp.ObjectType = PIN_SMD_ROUND;
						BottomPadObjectTemp.x2 = ThroughHoleObject->x2;
						BottomPadObjectTemp.y2 = ThroughHoleObject->x2;
						DrillObjectTemp.x2 = ThroughHoleObject->y2;

						if (ThroughHoleObject->x3 > 0.0)
						{
							InnerPadObject = &InnerPadObjectTemp;
							InnerPadObjectTemp.x2 = ThroughHoleObject->x3;
						}

						if (ThroughHoleObject->y3 > 0.0)
						{
							PowerPadObject = &PowerPadObjectTemp;
							PowerPadObjectTemp.x2 = ThroughHoleObject->y3;
						}

						break;

					case PIN_PUT_THROUGH_SQUARE:
						TopPadObjectTemp.ObjectType = PIN_SMD_RECT;
						TopPadObjectTemp.x2 = ThroughHoleObject->x2;
						TopPadObjectTemp.y2 = ThroughHoleObject->x2;
						BottomPadObjectTemp.ObjectType = PIN_SMD_RECT;
						BottomPadObjectTemp.x2 = ThroughHoleObject->x2;
						BottomPadObjectTemp.y2 = ThroughHoleObject->x2;
						DrillObjectTemp.x2 = ThroughHoleObject->y2;

						if (ThroughHoleObject->x3 > 0.0)
						{
							InnerPadObject = &InnerPadObjectTemp;
							InnerPadObjectTemp.x2 = ThroughHoleObject->x3;
						}

						if (ThroughHoleObject->y3 > 0.0)
						{
							PowerPadObject = &PowerPadObjectTemp;
							PowerPadObjectTemp.x2 = ThroughHoleObject->y3;
						}

						break;
					}

					TopPadObject = &TopPadObjectTemp;
					BottomPadObject = &BottomPadObjectTemp;
					DrillObject = &DrillObjectTemp;
				}

// *******************************************************************************************************
// *******************************************************************************************************
				Found = 0;

				for (cnt4 = 0; cnt4 < NrMentorPins; cnt4++)
				{
					if (MentorPins[cnt4].TopObjectType)
					{
						if ((!TopPadObject) || (TopPadObject->ObjectType != MentorPins[cnt4].TopObjectType)
						        || (NotInRange2(TopPadObject->x2, MentorPins[cnt4].TopPadWidth))
						        || (NotInRange2(TopPadObject->y2, MentorPins[cnt4].TopPadHeight)))
							continue;
					}

					if (MentorPins[cnt4].BottomObjectType)
					{
						if ((!BottomPadObject) || (BottomPadObject->ObjectType != MentorPins[cnt4].BottomObjectType)
						        || (NotInRange2(BottomPadObject->x2, MentorPins[cnt4].BottomPadWidth))
						        || (NotInRange2(BottomPadObject->y2, MentorPins[cnt4].BottomPadHeight)))
							continue;
					}

					if (MentorPins[cnt4].DrillWidth > 0.0)
					{
						if ((!DrillObject) || (NotInRange2(DrillObject->x2, MentorPins[cnt4].DrillWidth)))
							continue;
					}

					if (MentorPins[cnt4].InnerPadWidth > 0.0)
					{
						if ((!InnerPadObject) || (NotInRange2(InnerPadObject->x2, MentorPins[cnt4].InnerPadWidth)))
							continue;
					}

					if (MentorPins[cnt4].PowerPadWidth > 0.0)
					{
						if ((!PowerPadObject) || (NotInRange2(PowerPadObject->x2, MentorPins[cnt4].PowerPadWidth)))
							continue;
					}

					if (MentorPins[cnt4].TopSolderMaskWidth > 0.0)
					{
						if ((!SoldMaskTopObject) || (SoldMaskTopObject->ObjectType != MentorPins[cnt4].TopObjectType)
						        || (NotInRange2(SoldMaskTopObject->x2, MentorPins[cnt4].TopSolderMaskWidth))
						        || (NotInRange2(SoldMaskTopObject->y2, MentorPins[cnt4].TopSolderMaskHeight)))
							continue;
					}

					if (MentorPins[cnt4].BottomSolderMaskWidth > 0.0)
					{
						if ((!SoldMaskBottomObject)
						        || (SoldMaskBottomObject->ObjectType != MentorPins[cnt4].BottomObjectType)
						        || (NotInRange2(SoldMaskBottomObject->x2, MentorPins[cnt4].BottomSolderMaskWidth))
						        || (NotInRange2(SoldMaskBottomObject->y2, MentorPins[cnt4].BottomSolderMaskHeight)))
							continue;
					}

					if (MentorPins[cnt4].TopPasteMaskWidth > 0.0)
					{
						if ((!PasteMaskTopObject) || (PasteMaskTopObject->ObjectType != MentorPins[cnt4].TopObjectType)
						        || (NotInRange2(PasteMaskTopObject->x2, MentorPins[cnt4].TopPasteMaskWidth))
						        || (NotInRange2(PasteMaskTopObject->y2, MentorPins[cnt4].TopPasteMaskHeight)))
							continue;
					}

					if (MentorPins[cnt4].BottomPasteMaskWidth > 0.0)
					{
						if ((!PasteMaskBottomObject)
						        || (PasteMaskBottomObject->ObjectType != MentorPins[cnt4].BottomObjectType)
						        || (NotInRange2(PasteMaskBottomObject->x2, MentorPins[cnt4].BottomPasteMaskWidth))
						        || (NotInRange2(PasteMaskBottomObject->y2, MentorPins[cnt4].BottomPasteMaskHeight)))
							continue;
					}

					Found = 1;
					break;
				}

				if (!Found)
				{
#ifdef _DEBUG

					if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
					{
						ok = 1;	// NrMentorPins

						if (NrMentorPins == 13)
							ok = 1;
					}

					if (NrMentorPins == 13)
						ok = 1;

#endif

					if (TopPadObject)
					{
						MentorPins[NrMentorPins].TopObjectType = TopPadObject->ObjectType;
						MentorPins[NrMentorPins].TopPadWidth = TopPadObject->x2;
						MentorPins[NrMentorPins].TopPadHeight = TopPadObject->y2;
						TopPadObject->Info3 = NrMentorPins;
					}

					if (BottomPadObject)
					{
						MentorPins[NrMentorPins].BottomObjectType = BottomPadObject->ObjectType;
						MentorPins[NrMentorPins].BottomPadWidth = BottomPadObject->x2;
						MentorPins[NrMentorPins].BottomPadHeight = BottomPadObject->y2;
						BottomPadObject->Info3 = NrMentorPins;
					}

					if (DrillObject)
					{
						MentorPins[NrMentorPins].DrillWidth = DrillObject->x2;
						DrillObject->Info3 = NrMentorPins;

						if (DrillObject->x3)
						{
							MentorPins[NrMentorPins].InnerPadWidth =
							    max(MentorPins[NrMentorPins].InnerPadWidth, DrillObject->x3);
						}

						if (DrillObject->y3)
						{
							MentorPins[NrMentorPins].PowerPadWidth =
							    max(MentorPins[NrMentorPins].InnerPadWidth, DrillObject->y3);
						}
					}

					if (InnerPadObject)
					{
						MentorPins[NrMentorPins].InnerPadWidth =
						    max(MentorPins[NrMentorPins].InnerPadWidth, InnerPadObject->x2);
						InnerPadObject->Info3 = NrMentorPins;
					}

					if (PowerPadObject)
					{
						MentorPins[NrMentorPins].PowerPadWidth =
						    max(MentorPins[NrMentorPins].PowerPadWidth, PowerPadObject->x2);
						PowerPadObject->Info3 = NrMentorPins;
					}

					if (SoldMaskTopObject)
					{
						MentorPins[NrMentorPins].TopSolderMaskWidth = SoldMaskTopObject->x2;
						MentorPins[NrMentorPins].TopSolderMaskHeight = SoldMaskTopObject->y2;
						SoldMaskTopObject->Info3 = NrMentorPins;
					}

					if (SoldMaskBottomObject)
					{
						MentorPins[NrMentorPins].BottomSolderMaskWidth = SoldMaskBottomObject->x2;
						MentorPins[NrMentorPins].BottomSolderMaskHeight = SoldMaskBottomObject->y2;
						SoldMaskBottomObject->Info3 = NrMentorPins;
					}

					if (PasteMaskTopObject)
					{
						MentorPins[NrMentorPins].TopPasteMaskWidth = PasteMaskTopObject->x2;
						MentorPins[NrMentorPins].TopPasteMaskHeight = PasteMaskTopObject->y2;
						PasteMaskTopObject->Info3 = NrMentorPins;
					}

					if (PasteMaskBottomObject)
					{
						MentorPins[NrMentorPins].BottomPasteMaskWidth = PasteMaskBottomObject->x2;
						MentorPins[NrMentorPins].BottomPasteMaskHeight = PasteMaskBottomObject->y2;
						PasteMaskBottomObject->Info3 = NrMentorPins;
					}

					if (ThroughHoleObject)
						ThroughHoleObject->Info3 = NrMentorPins;

//          MentorPins[NrMentorPins].Info=PinObject->Info & 2;
					if (ShapeSMD)
						MentorPins[NrMentorPins].Info = 2;

					NrMentorPins++;
				}
				else
				{
					if (TopPadObject)
						TopPadObject->Info3 = cnt4;

					if (BottomPadObject)
						BottomPadObject->Info3 = cnt4;

					if (DrillObject)
						DrillObject->Info3 = cnt4;

					if (InnerPadObject)
						InnerPadObject->Info3 = cnt4;

					if (PowerPadObject)
						PowerPadObject->Info3 = cnt4;

					if (SoldMaskTopObject)
						SoldMaskTopObject->Info3 = cnt4;

					if (SoldMaskBottomObject)
						SoldMaskBottomObject->Info3 = cnt4;

					if (PasteMaskTopObject)
						PasteMaskTopObject->Info3 = cnt4;

					if (PasteMaskBottomObject)
						PasteMaskBottomObject->Info3 = cnt4;

					if (ThroughHoleObject)
						ThroughHoleObject->Info3 = cnt4;
				}
			}
			else
			{
// *******************************************************************************************************
// *******************************************************************************************************
				// Extended pin
				for (cnt2 = 0; cnt2 < NrMainPinObjects; cnt2++)
				{
					PinObject = &((*Objects)[MainPinObjectsPos[cnt2]]);
					PinObject->Info4 = NrExtendedPins;

					switch (PinObject->ObjectType)
					{
					case PIN_PUT_THROUGH_POLYGON:
						break;

					case PIN_SMD_POLYGON:
						break;
					}
				}

				NrExtendedPins++;
				ok = 1;			// ShapeNameP PinNameP
			}

			ok = 1;
		}

// *******************************************************************************************************
// *******************************************************************************************************
		// Search for unplated drills
		/*
		    for (cnt2=(*Shapes)[cnt].ObjectPos;cnt2<NrObjects;cnt2++) {
		      PinObject=&((*Objects)[cnt2]);
		      if ((PinObject->Info3==-1)
		         &&
		         (PinObject->Info4==-1)) {
		        switch (PinObject->ObjectType) {
		          case DRILL_UNPLATED:
		            Found=0;
		            for (cnt4=0;cnt4<NrMentorPins;cnt4++) {
		              if ((MentorPins[cnt4].UnplatedDrill)
		                 &&
		                 (InRange2(PinObject->x2,MentorPins[cnt4].DrillWidth))) {
		                Found=1;
		                break;
		              }
		            }
		            if (!Found) {
		              MentorPins[NrMentorPins].UnplatedDrill=1;
		              MentorPins[NrMentorPins].DrillWidth=PinObject->x2;
		              PinObject->Info3=NrMentorPins;
		              NrMentorPins++;
		            } else {
		              PinObject->Info3=cnt4;
		            }
		            break;
		        }
		        ok=1;
		      }
		    }
		*/
// *******************************************************************************************************
// *******************************************************************************************************

		for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < NrObjects; cnt2++)
		{
			PinObject = &((*Objects)[cnt2]);

			if ((PinObject->Info3 == -1) && (PinObject->Info4 == -1))
			{
				// Solder/paste objects not being used
				switch (PinObject->Layer)
				{
				case SOLD_MASK_TOP:
				case SOLD_MASK_BOTTOM:
				case PASTE_MASK_TOP:
				case PASTE_MASK_BOTTOM:
					switch (PinObject->ObjectType)
					{
					case PIN_SMD_ROUND:
					case PIN_SMD_RECT:
						for (cnt3 = (*Shapes)[cnt].ObjectPos; cnt3 < NrObjects; cnt3++)
						{
							if (cnt2 != cnt3)
							{
								PinObject2 = &((*Objects)[cnt3]);

								if ((PinObject->Info4 == -1) && (PinObject2->Info4 >= 0))
								{	// Extended pin
									switch (PinObject2->ObjectType)
									{
									case PIN_PUT_THROUGH_ROUND:
									case PIN_PUT_THROUGH_SQUARE:
									case DRILL:
									case PIN_SMD_ROUND:
									case PIN_SMD_RECT:
										if ((InRange2(PinObject->x1, PinObject2->x1))
										        && (InRange2(PinObject->y1, PinObject2->y1)))
										{
											ok = 1;	// ShapeNameP PinNameP
											PinObject->Info4 = PinObject2->Info4;
										}

										break;
									}

									ok = 1;
								}
							}
						}

						if ((PinObject->Info3 == -1) && (PinObject->Info4 == -1))
							ok = 1;

						ok = 1;
						break;
					}

					break;

				default:
					ok = 1;
					break;
				}

				ok = 1;
			}
		}

		ok = 1;
	}

	(*Shapes)[cnt].ObjectPos = NrObjects;
	/*
	  for (cnt=0;cnt<NrObjects;cnt++) {
	    PinObject=&((*Objects)[cnt]);
	    if ((PinObject->Info3==-1)
	       &&
	       (PinObject->Info4==-1)) {
	      if (PinObject->Text2) {
	        sprintf(str,"shape %s,pad %s,layer %d,objecttype %d,xy %.0f,%.0f\r\n",PinObject->ShapeName,
	                PinObject->Text2,PinObject->Layer,PinObject->ObjectType,
	                PinObject->x1,PinObject->y1);
	      } else {
	        sprintf(str,"shape %s,layer %d,objecttype %d,xy %.0f,%.0f\r\n",PinObject->ShapeName,
	                PinObject->Layer,PinObject->ObjectType,
	                PinObject->x1,PinObject->y1);
	      }
	      OutputDebugString(str);
	      ok=1; // ShapeNameP PinNameP
	    }
	  }
	  cnt2=0;
	  for (cnt=0;cnt<NrObjects;cnt++) {
	    PinObject=&((*Objects)[cnt]);
	    if (PinObject->Info3!=-1) {
	      cnt2++;
	    }
	  }
	  ok=1; // NrObjects

	    ShapePlacementOutLineToObject(Shape,0.0,0.0,0.0);
	    ShapeCompOutLineToObject(Shape,0.0,0.0,0.0);
	    ShapeCompSilkScreenToObject(Shape,0.0,0.0,0.0);
	*/

	CreatePadStackNames(0);

	sprintf(str, "%s\\%s_geom", DesignPath, DesignName);
	fp = FileOpenWriteUTF8(str);

	if (fp <= 0)
		return -1;

	sprintf(str2, "//");
	WriteLn(fp, str2);
	sprintf(str2, "// PCB elegance mentor geometry generator 1.0");
	WriteLn(fp, str2);
	sprintf(str2, "// PCB elegance design %s", DesignName);
	WriteLn(fp, str2);
	time(&ltime);
	today = localtime(&ltime);
	strftime(str3, 200, "%B %d, %Y %X", today);
	sprintf(str2, "// date : %s", str3);
	WriteLn(fp, str2);
	sprintf(str2, "//");
	WriteLn(fp, str2);
	sprintf(str2, "$$lock_windows(@on);");
	WriteLn(fp, str2);
	cnt3 = 0;

	for (cnt = 0; cnt < PcbDesign.NrShapes; cnt++)
	{
		Shape = (ShapeRecord *) & ShapesMem[(*Shapes)[cnt].ShapePos];
		ShapeNameP = (*Shapes)[cnt].ShapeName;
		CurrentShapeNameP = ShapeNameP;
		NrShapeLayers = Shape->NrLayers;
		memset(UsedRefLayers, 0, sizeof(UsedRefLayers));
#ifdef _DEBUG

		if (cnt2 == 24)
			ok = 1;

		if (stricmp(ShapeNameP, "new_geom_4") == 0)
		{
			ok = 1;				// NrMentorPins
		}

#endif

		/*
		    FoundThroughHolePin=0;
		    for (cnt2=(*Shapes)[cnt].ObjectPos;cnt2<(*Shapes)[cnt+1].ObjectPos;cnt2++) {
		      PinObject=&((*Objects)[cnt2]);
		      if (PinObject->Info3!=-1) {
		        if (MentorPins[PinObject->Info3].DrillWidth>0.0) {
		          if (!FoundThroughHolePin) {
		            FoundThroughHolePin=1;
		          } else {
		            if (FoundThroughHolePin==1) {
		              sprintf(str2,"Warning: Mentor padstack %s in shape %s contains more than one drill hole\r\n",
		                      MentorPins[PinObject->Info3].PadStackName,ShapeNameP);
		              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str2);
		            }
		            FoundThroughHolePin++;
		          }
		        }
		      }
		    }
		*/
		for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < (*Shapes)[cnt + 1].ObjectPos; cnt2++)
		{
			PinObject = &((*Objects)[cnt2]);

			if ((PinObject->Info3 != -1) && (PinObject->Info3 != 1000000))
				OutputPadStack(fp, ShapeNameP, &MentorPins[PinObject->Info3]);
		}

		while (1)
		{
			Found = -1;

			for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < (*Shapes)[cnt + 1].ObjectPos; cnt2++)
			{
				PinObject = &((*Objects)[cnt2]);

				if (((PinObject->Info & 1) == 0) && (PinObject->Info4 != -1))
				{
					Found = PinObject->Info4;
					break;
				}
			}

			if (Found != -1)
			{	// NrExtendedPins
				/*
				$$create_pin("055rd027");
				*/
				cnt3 = 0;
				NrMainPinObjects = 0;
				MentorSpecialPins[Found].OX = 1e9;
				MentorSpecialPins[Found].OY = 1e9;
				FoundThroughHolePin = 0;

				for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < (*Shapes)[cnt + 1].ObjectPos; cnt2++)
				{
					PinObject = &((*Objects)[cnt2]);

					if (PinObject->Info4 == Found)
					{
						MainPinObjectsPos[NrMainPinObjects++] = cnt2;

						switch (PinObject->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:
						case PIN_PUT_THROUGH_SQUARE:
						case PIN_SMD_ROUND:
						case PIN_SMD_RECT:
						case DRILL:
						case PIN_PUT_THROUGH_POLYGON:
						case DRILL_UNPLATED:
						case PIN_LINE_HOR:
						case PIN_LINE_VER:
						case PIN_LINE_DIAG1:
						case PIN_LINE_DIAG2:
						case PIN_ARC:
						case PIN_SMD_POLYGON:
							if ((PinObject->x1 < MentorSpecialPins[Found].OX)
							        && (PinObject->y1 < MentorSpecialPins[Found].OY))
							{
								MentorSpecialPins[Found].OX = PinObject->x1;
								MentorSpecialPins[Found].OY = PinObject->y1;
							}

							break;

						case PIN_LINE_ALL_ANGLE:
							if ((PinObject->x1 < MentorSpecialPins[Found].OX)
							        && (PinObject->y1 < MentorSpecialPins[Found].OY))
							{
								MentorSpecialPins[Found].OX = PinObject->x1;
								MentorSpecialPins[Found].OY = PinObject->y1;
							}

							if ((PinObject->x2 < MentorSpecialPins[Found].OX)
							        && (PinObject->y2 < MentorSpecialPins[Found].OY))
							{
								MentorSpecialPins[Found].OX = PinObject->x2;
								MentorSpecialPins[Found].OY = PinObject->y2;
							}

							break;
						}
					}
				}

				OX = MentorSpecialPins[Found].OX;
				OY = MentorSpecialPins[Found].OY;
				InnerPadWidth = 0.0;

				for (cnt2 = 0; cnt2 < NrMainPinObjects; cnt2++)
				{
					PinObject = &((*Objects)[MainPinObjectsPos[cnt2]]);

					if ((PinObject->Info4 == Found) && (PinObject->ObjectType == PIN_SMD_ROUND)
					        && (PinObject->Layer > 0) && (PinObject->Layer < NrShapeLayers - 1))
						InnerPadWidth = max(InnerPadWidth, PinObject->x2);
				}

				for (cnt2 = 0; cnt2 < NrMainPinObjects; cnt2++)
				{
					PinObject = &((*Objects)[MainPinObjectsPos[cnt2]]);

					if (PinObject->Info4 == Found)
					{
#ifdef _DEBUG

						if (cnt2 == 24)
							ok = 1;

						if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
						{
							ok = 1;	// NrMentorPins

							if (stricmp(ShapeNameP, "smd_150_2") == 0)
							{
								ok = 1;	// NrMentorPins
							}
						}

#endif
						PinObject->Info |= 1;

						if (cnt3 == 0)
						{
							sprintf(str2, "$$create_pin(\"%s_%s\");", ShapeNameP, PinObject->Text2);
							sprintf(MentorSpecialPins[Found].PadStackName, "%s_%s", ShapeNameP, PinObject->Text2);
#ifdef _DEBUG

							if (stricmp(MentorSpecialPins[Found].PadStackName, "smd_150_2") == 0)
							{
								ok = 1;	// NrMentorPins
							}

#endif
							WriteLn(fp, str2);

							if (MentorUnits == 1)
							{
								sprintf(str3, "$$page(0.0, 0.0, 0.0, @mils, 0.0, 0.0, [0.0,0.0,'PI$%s_%s] );",
								        ShapeNameP, PinObject->Text2);
								WriteLn(fp, str3);
							}
							else
							{
								sprintf(str3,
								        "$$page(0.0000, 0.0000, 0.0000, @mm, 0.0000, 0.0000, [0.0000,0.0000,'PI$%s_%s'] );",
								        ShapeNameP, PinObject->Text2);
								WriteLn(fp, str3);
							}

							WriteLn(fp, "$$point_mode(@vertex);");
						}

//            sprintf(str2,"//");
//            WriteLn(fp,str2);
						switch (PinObject->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:
							if (!FoundThroughHolePin)
							{
								MentorPrintCircle(fp, "PAD", OX, OY, PinObject->x2, 0.0);
								MentorPrintDrill(fp, OX, OY, PinObject->y2, 0);

								if (PinObject->x3)
									MentorPrintCircle(fp, "SIGNAL", OX, OY, PinObject->x3, 0.0);

								if (PinObject->y3)
									MentorPrintCircle(fp, "POWER", OX, OY, PinObject->y3, 0.0);
							}
							else
							{
								sprintf(str2,
								        "***************  Warning: Mentor padstack %s in shape %s contains more than one drill hole\r\n",
								        MentorSpecialPins[Found].PadStackName, ShapeNameP);
								AddMessage(str2);
//                  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str2);
							}

							FoundThroughHolePin++;
							break;

						case PIN_PUT_THROUGH_SQUARE:
							if (!FoundThroughHolePin)
							{
								MentorPrintRectPad(fp, "PAD", OX, OY, PinObject->x2, PinObject->x2);
								MentorPrintDrill(fp, OX, OY, PinObject->y2, 0);

								if (PinObject->x3)
									MentorPrintCircle(fp, "SIGNAL", OX, OY, PinObject->x3, 0.0);

								if (PinObject->y3)
									MentorPrintCircle(fp, "POWER", OX, OY, PinObject->y3, 0.0);
							}
							else
							{
								sprintf(str2,
								        "***************  Warning: Mentor padstack %s in shape %s contains more than one drill hole\r\n",
								        MentorSpecialPins[Found].PadStackName, ShapeNameP);
								AddMessage(str2);
//                  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str2);
							}

							FoundThroughHolePin++;
							break;

						case PIN_SMD_ROUND:
							if ((PinObject->Layer > 0) && (PinObject->Layer < NrShapeLayers - 1))
							{
							}
							else
								MentorPrintCircle(fp, GetMentorLayer(PinObject->Layer), OX, OY, PinObject->x2, 0.0);

							break;

						case PIN_SMD_RECT:
#ifdef _DEBUG
							if (cnt2 == 24)
								ok = 1;

							if (stricmp(ShapeNameP, "SOT23_6L") == 0)
							{
								ok = 1;	// NrMentorPins
							}

#endif
							MentorPrintRectPad(fp, GetMentorLayer(PinObject->Layer), OX, OY, PinObject->x2,
							                   PinObject->y2);
							break;

						case DRILL:
							MentorPrintDrill(fp, OX, OY, PinObject->y2, 0);
							break;

						case PIN_PUT_THROUGH_POLYGON:
							MentorPrintDrill(fp, OX, OY, PinObject->y2, 0);
							break;

						case DRILL_UNPLATED:
							MentorPrintDrill(fp, OX, OY, PinObject->y2, 1);
							break;

						case PIN_LINE_HOR:
						case PIN_LINE_VER:
						case PIN_LINE_DIAG1:
						case PIN_LINE_DIAG2:
							x1 = PinObject->x1 - OX;
							y1 = PinObject->y1 - OY;

							switch (PinObject->ObjectType)
							{
							case PIN_LINE_HOR:
								x2 = x1 + PinObject->x2;
								y2 = y1;
								break;

							case PIN_LINE_VER:
								x2 = x1;
								y2 = y1 + PinObject->x2;
								break;

							case PIN_LINE_DIAG1:
								x2 = x1 + PinObject->x2;
								y2 = y1 - PinObject->x2;
								break;

							case PIN_LINE_DIAG2:
								x2 = x1 + PinObject->x2;
								y2 = y1 + PinObject->x2;
								break;
							}

							MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1, y1, x2, y2, PinObject->y2);
							break;

						case PIN_ARC:
							if (MentorUnits == 1)
							{
								sprintf(str2,
								        "$$arc( \"%s\", %.1f, %.1f, %.1f, %.1f, %.1f, %.1f, %.1f, \"\", @center, @radius );",
								        GetMentorLayer(PinObject->Layer), (PinObject->x1 + PinObject->x3) / 2540.0,
								        (PinObject->y1 + PinObject->y3) / 2540.0,
								        (PinObject->x1 + PinObject->x4) / 2540.0,
								        (PinObject->y1 + PinObject->y4) / 2540.0, PinObject->x1 / 2540.0,
								        PinObject->y1 / 2540.0, PinObject->Thickness / 2540.0);
							}
							else
							{
								sprintf(str2,
								        "$$arc( \"%s\", %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, \"\", @center, @radius );",
								        GetMentorLayer(PinObject->Layer), (PinObject->x1 + PinObject->x3) / UNITS_MM,
								        (PinObject->y1 + PinObject->y3) / UNITS_MM,
								        (PinObject->x1 + PinObject->x4) / UNITS_MM,
								        (PinObject->y1 + PinObject->y4) / UNITS_MM, PinObject->x1 / UNITS_MM,
								        PinObject->y1 / UNITS_MM, PinObject->Thickness / UNITS_MM);
							}

							WriteLn(fp, str2);
							break;

						case PIN_LINE_ALL_ANGLE:
							MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), PinObject->x1 - OX,
							                PinObject->y1 - OY, PinObject->x2 - OX, PinObject->y2 - OY,
							                PinObject->Thickness);
							break;

						case PIN_SMD_POLYGON:
							GeomPolygon = (GeomPolygonRecord *) PinObject->Address;
							PolygonPoints = (PointRecord *) GeomPolygon->Points;
							MentorPrintPolygon(fp, GetMentorLayer(PinObject->Layer), PolygonPoints, PinObject->x1 - OX,
							                   PinObject->y1 - OY, GeomPolygon->NrVertices, 0);
							break;
						}

						cnt3++;
					}
				}

				if (InnerPadWidth)
					MentorPrintCircle(fp, "SIGNAL", OX, OY, InnerPadWidth, 0.0);
			}
			else
				break;
		}

// *******************************************************************************************************
// *******************************************************************************************************


		/*
		$$create_component("batt_br2032_103_032");
		$$page(0.0, 0.0, 20.0, @mils, 0.0, 0.0, [0.0,0.0,'CO$bar_code_sticker_non'] );
		$$point_mode(@vertex);
		*/
		sprintf(str2, "$$create_component(\"%s\");", ShapeNameP);
		WriteLn(fp, str2);

		if (MentorUnits == 1)
			sprintf(str2, "$$page(0.0, 0.0, 20.0, @mils, 0.0, 0.0, [0.0,0.0,'CO$%s'] );", ShapeNameP);
		else
			sprintf(str2, "$$page(0.0, 0.0, 0.2, @mm, 0.0, 0.0, [0.0,0.0,'CO$%s'] );", ShapeNameP);

		WriteLn(fp, str2);
		sprintf(str2, "$$point_mode(@vertex);");
		WriteLn(fp, str2);
		sprintf(str2, "$$template_line_style( @Solid );");
		WriteLn(fp, str2);

		/*
		$$attribute( "COMPONENT_PLACEMENT_OUTLINE", "", , @scale , , [-50.0, 50.0, 450.0, 50.0, 450.0, -50.0, -50.0, -50.0]);

		$$initial([98.5,170.0], , @nosnap );
		$$terminal([98.5,170.0] );
		$$attribute( "COMPONENT_PLACEMENT_OUTLINE", "", @mark, @scale );

		$$attribute( "COMPONENT_LAYOUT_TYPE", "surface");
		$$attribute( "COMPONENT_HEIGHT", "", , @scale , , [52.0, 0.0]);

		$$attribute( "COMPONENT_PIN_DEFINITION", "1", , @scale , , [0.0, 0.0]);
		$$attribute( "COMPONENT_DEFAULT_PADSTACK", "smd_040x056");
		$$attribute( "COMPONENT_PADSTACK_OVERRIDE", "1,smd_040x056");

		*/
		memset(MainPinObjectsPos, 0, sizeof(MainPinObjectsPos));
		PinNameP = "";

		for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < (*Shapes)[cnt + 1].ObjectPos; cnt2++)
		{
			PinObject = &((*Objects)[cnt2]);

			if (PinObject->Info3 != -1)
			{
				if ((PinObject->Text2) && (stricmpUTF8(PinNameP, PinObject->Text2)))
				{
					MainPinObjectsPos[PinObject->Info3]++;
					PinNameP = PinObject->Text2;
				}
			}
		}

		cnt3 = 0;
		cnt4 = 0;

		for (cnt2 = 0; cnt2 < 1024; cnt2++)
		{
			if (MainPinObjectsPos[cnt2] > cnt3)
			{
				cnt3 = MainPinObjectsPos[cnt2];
				cnt4 = cnt2;
			}
		}

		DefaultPadStackName = MentorPins[cnt4].PadStackName;
		sprintf(str2, "$$attribute( \"COMPONENT_DEFAULT_PADSTACK\", \"%s\");", DefaultPadStackName);
		WriteLn(fp, str2);
		PinNameP = "";

		for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < (*Shapes)[cnt + 1].ObjectPos; cnt2++)
		{
			PinObject = &((*Objects)[cnt2]);

			if (PinObject->Info3 != -1)
			{
				if ((PinObject->Text2) && (stricmpUTF8(PinNameP, PinObject->Text2)))
				{
					PinNameP = PinObject->Text2;

					if (PinObject->Info3 != 1000000)
					{
						if (stricmpUTF8(MentorPins[PinObject->Info3].PadStackName, DefaultPadStackName) == 0)
						{
							if (MentorUnits == 1)
							{
								sprintf(str2,
								        "$$attribute( \"COMPONENT_PIN_DEFINITION\", \"%s\", , @scale , , [%.1f,%.1f]);",
								        PinNameP, PinObject->x1 / 2540.0, PinObject->y1 / 2540.0);
							}
							else
							{
								sprintf(str2,
								        "$$attribute( \"COMPONENT_PIN_DEFINITION\", \"%s\", , @scale , , [%.4f,%.4f]);",
								        PinNameP, PinObject->x1 / UNITS_MM, PinObject->y1 / UNITS_MM);
							}

							WriteLn(fp, str2);
						}
						else
						{
							sprintf(str2, "$$attribute( \"COMPONENT_PADSTACK_OVERRIDE\", \"%s,%s\");", PinNameP,
							        MentorPins[PinObject->Info3].PadStackName);
							WriteLn(fp, str2);

							if (MentorUnits == 1)
							{
								sprintf(str2,
								        "$$attribute( \"COMPONENT_PIN_DEFINITION\", \"%s\", , @scale , , [%.1f,%.1f]);",
								        PinNameP, PinObject->x1 / 2540.0, PinObject->y1 / 2540.0);
							}
							else
							{
								sprintf(str2,
								        "$$attribute( \"COMPONENT_PIN_DEFINITION\", \"%s\", , @scale , , [%.4f,%.4f]);",
								        PinNameP, PinObject->x1 / UNITS_MM, PinObject->y1 / UNITS_MM);
							}

							WriteLn(fp, str2);
						}
					}
					else
						ok = 1;
				}
			}

			if (PinObject->Info4 != -1)
			{
				if ((PinObject->Text2) && (stricmpUTF8(PinNameP, PinObject->Text2)))
				{
					PinNameP = PinObject->Text2;
					sprintf(str2, "$$attribute( \"COMPONENT_PADSTACK_OVERRIDE\", \"%s,%s\");", PinNameP,
					        MentorSpecialPins[PinObject->Info4].PadStackName);
					WriteLn(fp, str2);

					if (MentorUnits == 1)
					{
						sprintf(str2, "$$attribute( \"COMPONENT_PIN_DEFINITION\", \"%s\", , @scale , , [%.1f,%.1f]);",
						        PinNameP, (PinObject->x1 - MentorSpecialPins[PinObject->Info3].OX) / 2540.0,
						        (PinObject->y1 - MentorSpecialPins[PinObject->Info3].OY) / 2540.0);
					}
					else
					{
						sprintf(str2, "$$attribute( \"COMPONENT_PIN_DEFINITION\", \"%s\", , @scale , , [%.4f,%.4f]);",
						        PinNameP, (PinObject->x1 - MentorSpecialPins[PinObject->Info3].OX) / UNITS_MM,
						        (PinObject->y1 - MentorSpecialPins[PinObject->Info3].OY) / UNITS_MM);
					}

					WriteLn(fp, str2);
				}
			}
		}

		/*
		$$attribute( "COMPONENT_HEIGHT", "", , @scale , , [52.0, 0.0]);
		*/
#ifdef _DEBUG

		if (cnt2 == 24)
			ok = 1;

		if (stricmp(ShapeNameP, "so20") == 0)
		{
			ok = 1;

			if (strcmp(PinNameP, "Layer2") == 0)
				ok = 1;
		}

#endif
		y = Shape->ShapeHeight;

		if (y == 0.0)
			y = UNITS_MM;

		if (MentorUnits == 1)
			sprintf(str2, "$$attribute( \"COMPONENT_HEIGHT\", \"\", , @scale , , [%.1f, 0.0]);", y / 2540.0);
		else
			sprintf(str2, "$$attribute( \"COMPONENT_HEIGHT\", \"\", , @scale , , [%.4f, 0.0]);", y / UNITS_MM);

		WriteLn(fp, str2);

		/*

		SMD

		$$attribute( "COMPONENT_LAYOUT_SURFACE", "both");
		$$attribute( "COMPONENT_LAYOUT_TYPE", "surface");


		PUT THROUGH

		$$attribute( "COMPONENT_LAYOUT_SURFACE", "both");

		*/
		if (Shape->Info & 1)
		{
			// SMD
			sprintf(str2, "$$attribute( \"COMPONENT_LAYOUT_TYPE\", \"surface\");");
			WriteLn(fp, str2);
			sprintf(str2, "$$attribute( \"COMPONENT_LAYOUT_SURFACE\", \"both\");");
			WriteLn(fp, str2);
		}
		else
		{
			sprintf(str2, "$$attribute( \"COMPONENT_LAYOUT_SURFACE\", \"both\");");
			WriteLn(fp, str2);
		}

		sprintf(str2, "$$attribute( \"COMPONENT_OUTLINE_OVERHANG\", \"no\");");
		WriteLn(fp, str2);
		sprintf(str2, "$$attribute( \"COMPONENT_DIAGONAL_ALLOWED\", \"\");");
		WriteLn(fp, str2);
		/*
		$$attribute( "COMPONENT_INSERT_TYPE", "SMD");
		$$attribute( "COMPONENT_INSERT_TYPE", "OTHER");
		*/
		/*
		$$attribute( "COMPONENT_INSERT_CENTER", "", , @scale , , [37.5, 0.0]);
		*/
		/*
		    if (MentorUnits==1) {
		      sprintf(str2,"$$attribute( \"COMPONENT_INSERT_CENTER\", \"\", , @scale , , [%.1f, %.1f]);",
		              Shape->InsertionX/2540.0,Shape->InsertionY/2540.0);
		    } else {
		      sprintf(str2,"$$attribute( \"COMPONENT_INSERT_CENTER\", \"\", , @scale , , [%.4f, %.4f]);",
		              Shape->InsertionX/UNITS_MM,Shape->InsertionY/UNITS_MM);
		    }
		    WriteLn(fp,str2);
		    sprintf(str2,"$$attribute( \"COMPONENT_INSERT_ANGLE\", \"'\", @mark, , \"\");");
		    WriteLn(fp,str2);
		*/
		ok = 1;

// *******************************************************************************************************
// *******************************************************************************************************

		for (cnt2 = (*Shapes)[cnt].ObjectPos; cnt2 < (*Shapes)[cnt + 1].ObjectPos; cnt2++)
		{
			PinObject = &((*Objects)[cnt2]);

			if ((PinObject->Info3 == -1) && (PinObject->Info4 == -1))
			{
				x1 = PinObject->x1;
				y1 = PinObject->y1;

				switch (PinObject->ObjectType)
				{
				case PIN_SMD_ROUND:
					MentorPrintCircle(fp, GetMentorLayer(PinObject->Layer), x1, y1, PinObject->x2, 0.0);
					break;

				case PIN_SMD_RECT:
#ifdef _DEBUG
					if (cnt2 == 24)
						ok = 1;

					if (stricmp(ShapeNameP, "SOT23_6L") == 0)
					{
						ok = 1;	// NrMentorPins
					}

#endif
					MentorPrintRectPad(fp, GetMentorLayer(PinObject->Layer), x1, y1, PinObject->x2, PinObject->y2);
					break;

				case DRILL_UNPLATED:
					sprintf(str2, "$$attribute( \"DRILL_DEFINITION_UNPLATED\", \"%.2f\", , @scale , , [%.4f, %.4f]);",
					        PinObject->x2 / UNITS_MM, x1 / UNITS_MM, y1 / UNITS_MM);
					WriteLn(fp, str2);
					ok = 1;
					break;

				case OBJECT_LINE:
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1, y1, PinObject->x2, PinObject->y2,
					                PinObject->Thickness);
					break;

				case OBJECT_RECT:
					x2 = PinObject->x2 * 0.5;
					y2 = PinObject->y2 * 0.5;
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 - x2, y1 - y2, x1 - x2, y1 + y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 - x2, y1 + y2, x1 + x2, y1 + y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 + x2, y1 + y2, x1 + x2, y1 - y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 + x2, y1 - y2, x1 - x2, y1 - y2,
					                PinObject->Thickness);
					break;

				case OBJECT_ARC:
					MentorPrintArc(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1, PinObject->x3,
					               PinObject->y3, PinObject->x4, PinObject->y4, PinObject->Thickness, 0);
					break;

				case OBJECT_TEXT:
					MentorPrintText(fp, PinObject);
					break;

				case OBJECT_POLYGON:
					GeomPolygon = (GeomPolygonRecord *) PinObject->Address;
					PolygonPoints = (PointRecord *) GeomPolygon->Points;
					MentorPrintPolygon(fp, GetMentorLayer(PinObject->Layer), PolygonPoints, -OX, -OY,
					                   GeomPolygon->NrVertices, 0);
					break;

				default:
					ok = 1;
					break;
				}
			}
		}

		/*
		$$text( "COMP_OPST", "^$ref", -50.0, 150.0, 60.0, @TL, 0, 1.00, 10.0, "std", "None", 0.0, 0.0 );
		*/
		sprintf(str2,
		        "$$text( \"ASSEMBLY\", \"^$ref\", %.4f, %.4f, %.4f, @BL, 0, %s, %.4f, \"std\", \"None\", 0.0, 0.0 );",
		        Shape->ShapeNameOriginX / UNITS_MM - PinObject->x2 / UNITS_MM * MultTextOffset,
		        Shape->ShapeNameOriginY / UNITS_MM, (Shape->ShapeNameHeight * MultTextY) / UNITS_MM, MentorAspectRatio,
		        0.25);
		WriteLn(fp, str2);
		sprintf(str2,
		        "$$text( \"SILKSCREEN\", \"^$ref\", %.4f, %.4f, %.4f, @BL, 0, %s, %.4f, \"std\", \"None\", 0.0, 0.0 );",
		        Shape->ShapeNameOriginX / UNITS_MM - PinObject->x2 / UNITS_MM * MultTextOffset,
		        Shape->ShapeNameOriginY / UNITS_MM, (Shape->ShapeNameHeight * MultTextY) / UNITS_MM, MentorAspectRatio,
		        SILKSCREEN_TEXT_THICKNESS);
		WriteLn(fp, str2);

// *******************************************************************************************************
// *******************************************************************************************************

		cnt3 = 0;
		cnt4 = 0;
		FirstObject = 0;
		NrObjects6 = 0;			// ShapeNameP
		ShapePlacementOutLineToObject(Shape, 0.0, 0.0, 0.0);
#ifdef _DEBUG

		if (cnt2 == 24)
			ok = 1;

		if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
		{
			ok = 1;				// NrMentorPins
		}

#endif

		for (cnt2 = 0; cnt2 < NrObjects6; cnt2++)
		{
			PinObject = &((*Objects6)[cnt2]);

			switch (PinObject->ObjectType)
			{
			case OBJECT_LINE:
				if ((FirstObject) && (FirstObject != OBJECT_LINE))
				{
					sprintf(str2,
					        "***************  Warning: There is more than one COMPONENT_PLACEMENT_OUTLINE object in shape %s\r\n",
					        ShapeNameP);
					AddMessage(str2);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str2);
					break;
				}

				if (CalcLengthLine2(PinObject->x1, PinObject->y1, PinObject->x2, PinObject->y2) > 0.0)
					MainPinObjectsPos[cnt3++] = cnt2;

				/*
				          MentorPrintPath(fp,GetMentorLayer(PinObject->Layer),
				                          PinObject->x1,PinObject->y1,
				                          PinObject->x2,PinObject->y2,PinObject->Thickness);
				*/
				if (!FirstObject)
					FirstObject = OBJECT_LINE;

				break;

			case OBJECT_RECT:
				if (FirstObject)
				{
					sprintf(str2,
					        "***************  Warning: There is more than one COMPONENT_PLACEMENT_OUTLINE object in shape %s\r\n",
					        ShapeNameP);
					AddMessage(str2);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str2);
					break;
				}

				/*
				$$attribute( "COMPONENT_PLACEMENT_OUTLINE", "", , @scale , , [-50.0, 50.0, 450.0, 50.0, 450.0, -50.0, -50.0, -50.0]);
				*/
				x1 = PinObject->x1;
				y1 = PinObject->y1;
				x2 = PinObject->x2 * 0.5;
				y2 = PinObject->y2 * 0.5;
				sprintf(str2,
				        "$$attribute( \"COMPONENT_PLACEMENT_OUTLINE\", \"\", , @scale , , [%.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f]);",
				        (x1 - x2) / UNITS_MM, (y1 - y2) / UNITS_MM, (x1 - x2) / UNITS_MM, (y1 + y2) / UNITS_MM,
				        (x1 + x2) / UNITS_MM, (y1 + y2) / UNITS_MM, (x1 + x2) / UNITS_MM, (y1 - y2) / UNITS_MM);
				WriteLn(fp, str2);

				/*
				          MentorPrintPath(fp,GetMentorLayer(PinObject->Layer),
				                          x1-x2,y1-y2,x1-x2,y1+y2,PinObject->Thickness);
				          MentorPrintPath(fp,GetMentorLayer(PinObject->Layer),
				                          x1-x2,y1+y2,x1+x2,y1+y2,PinObject->Thickness);
				          MentorPrintPath(fp,GetMentorLayer(PinObject->Layer),
				                          x1+x2,y1+y2,x1+x2,y1-y2,PinObject->Thickness);
				          MentorPrintPath(fp,GetMentorLayer(PinObject->Layer),
				                          x1+x2,y1-y2,x1-x2,y1-y2,PinObject->Thickness);
				*/
				if (!FirstObject)
					FirstObject = OBJECT_RECT;

				break;

			case OBJECT_ARC:
				if (FirstObject)
				{
					sprintf(str2,
					        "***************  Warning: There is more than one COMPONENT_PLACEMENT_OUTLINE object in shape %s\r\n",
					        ShapeNameP);
					AddMessage(str2);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str2);
					break;
				}

				if ((InRange(PinObject->x3, PinObject->x4)) && (InRange(PinObject->y3, PinObject->y4)))
				{
					x1 = PinObject->x1;
					y1 = PinObject->y1;
					x2 = PinObject->x2 * 0.5;

					for (cnt3 = 0; cnt3 < 32; cnt3++)
					{
						x3 = x1 + x2 * cos((2 * PI * cnt3) / 32);
						y3 = y1 + x2 * sin((2 * PI * cnt3) / 32);

						if (cnt3 == 0)
						{
							if (MentorUnits == 1)
								sprintf(str2, "$$initial([%.1f,%.1f], , @nosnap );", x3 / 2540.0, y3 / 2540.0);
							else
								sprintf(str2, "$$initial([%.4f,%.4f], , @nosnap );", x3 / UNITS_MM, y3 / UNITS_MM);
						}
						else
						{
							if (MentorUnits == 1)
								sprintf(str2, "$$terminal([%.1f,%.1f] );", x3 / 2540.0, y3 / 2540.0);
							else
								sprintf(str2, "$$terminal([%.4f,%.4f] );", x3 / UNITS_MM, y3 / UNITS_MM);
						}

						WriteLn(fp, str2);
					}

					sprintf(str2, "$$attribute( \"COMPONENT_PLACEMENT_OUTLINE\", \"\", @mark, @scale );");
					WriteLn(fp, str2);
				}

				if (!FirstObject)
					FirstObject = OBJECT_ARC;

				/*
				          if (MentorUnits==1) {
				            sprintf(str2,"$$arc( \"%s\", %.1f, %.1f, %.1f, %.1f, %.1f, %.1f, %.1f, \"\", @center, @radius );",
				                    GetMentorLayer(PinObject->Layer),
				                    (PinObject->x1+PinObject->x3)/2540.0,
				                    (PinObject->y1+PinObject->y3)/2540.0,
				                    (PinObject->x1+PinObject->x4)/2540.0,
				                    (PinObject->y1+PinObject->y4)/2540.0,
				                    PinObject->x1/2540.0,PinObject->y1/2540.0,
				                    0.0);
				          } else {
				            sprintf(str2,"$$arc( \"%s\", %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.4f, \"\", @center, @radius );",
				                    GetMentorLayer(PinObject->Layer),
				                    (PinObject->x1+PinObject->x3)/UNITS_MM,
				                    (PinObject->y1+PinObject->y3)/UNITS_MM,
				                    (PinObject->x1+PinObject->x4)/UNITS_MM,
				                    (PinObject->y1+PinObject->y4)/UNITS_MM,
				                    PinObject->x1/UNITS_MM,PinObject->y1/UNITS_MM,
				                    0.0);
				          }
				          WriteLn(fp,str2);
				*/
				break;
			}
		}

		switch (FirstObject)
		{
		case OBJECT_LINE:
			if (cnt3 == 4)
			{
				for (cnt3 = 0; cnt3 < 4; cnt3++)
				{
					PinObject = &((*Objects6)[cnt3]);
					PinObject2 = &((*Objects6)[MainPinObjectsPos[cnt3]]);
					memcpy(PinObject, PinObject2, sizeof(ObjectRecord));
				}

				NrObjects6 = 4;
				cnt4 = GetTraceChain(0);

				if (cnt4 == 5)
				{
					str3[0] = 0;

					for (cnt3 = 0; cnt3 < 4; cnt3++)
					{
						x1 = (*TracePoints)[TracePointsNumbers[cnt3]].x;
						y1 = (*TracePoints)[TracePointsNumbers[cnt3]].y;
						sprintf(str2, "%.4f, %.4f", x1 / UNITS_MM, y1 / UNITS_MM);
						strcat(str3, str2);

						if (cnt3 < 3)
							strcat(str3, ", ");
					}

					sprintf(str2, "$$attribute( \"COMPONENT_PLACEMENT_OUTLINE\", \"\", , @scale , , [%s]);", str3);
					WriteLn(fp, str2);
				}
			}
			else
			{
				if (cnt3 > 0)
				{
					ok = 1;
#ifdef _DEBUG

					if (cnt2 == 24)
						ok = 1;

					if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
					{
						ok = 1;	// NrMentorPins
					}

#endif

					for (cnt2 = 0; cnt2 < cnt3; cnt2++)
					{
						PinObject = &((*Objects6)[cnt2]);
						PinObject2 = &((*Objects6)[MainPinObjectsPos[cnt2]]);
						memcpy(PinObject, PinObject2, sizeof(ObjectRecord));
					}

					NrObjects6 = cnt3;
					cnt4 = GetTraceChain(0);

					for (cnt2 = 0; cnt2 < cnt4; cnt2++)
					{
						x1 = (*TracePoints)[TracePointsNumbers[cnt2]].x;
						y1 = (*TracePoints)[TracePointsNumbers[cnt2]].y;

						if (cnt2 == cnt4 - 1)
						{
							if ((InRangeSpecial(x1, (*TracePoints)[TracePointsNumbers[0]].x, 100.0))
							        && (InRangeSpecial(y1, (*TracePoints)[TracePointsNumbers[0]].y, 100.0)))
								continue;
						}

						if (cnt2 == 0)
							sprintf(str2, "$$initial([%.4f,%.4f], , @nosnap );", x1 / UNITS_MM, y1 / UNITS_MM);
						else
							sprintf(str2, "$$terminal([%.4f,%.4f] );", x1 / UNITS_MM, y1 / UNITS_MM);

						WriteLn(fp, str2);
					}

					sprintf(str2, "$$attribute( \"COMPONENT_PLACEMENT_OUTLINE\", \"\", @mark, @scale );");
					WriteLn(fp, str2);
				}
			}

			break;

		case OBJECT_RECT:
		case OBJECT_ARC:
			break;
		}

		NrObjects6 = 0;
		ShapeCompOutLineToObject(Shape, 0.0, 0.0, 0.0);

		for (cnt2 = 0; cnt2 < NrObjects6; cnt2++)
		{
			PinObject = &((*Objects6)[cnt2]);

			switch (PinObject->ObjectType)
			{
			case OBJECT_LINE:
				if (CalcLengthLine2(PinObject->x1, PinObject->y1, PinObject->x2, PinObject->y2) > 0.0)
				{
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1, PinObject->x2,
					                PinObject->y2, PinObject->Thickness);
				}

				break;

			case OBJECT_RECT:
				x1 = PinObject->x1;
				y1 = PinObject->y1;

				if (PinObject->Thickness != 0.0)
				{
					x2 = PinObject->x2 * 0.5;
					y2 = PinObject->y2 * 0.5;
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 - x2, y1 - y2, x1 - x2, y1 + y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 - x2, y1 + y2, x1 + x2, y1 + y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 + x2, y1 + y2, x1 + x2, y1 - y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 + x2, y1 - y2, x1 - x2, y1 - y2,
					                PinObject->Thickness);
				}
				else
				{
					x2 = PinObject->x2;
					y2 = PinObject->y2;
					MentorPrintRectPad(fp, GetMentorLayer(PinObject->Layer), x1, y1, x2, y2);
				}

				break;

			case OBJECT_ARC:
				if (PinObject->Thickness == 0.0)
				{
					MentorPrintCircle(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1, PinObject->x2,
					                  PinObject->Thickness);
				}
				else
				{
					if ((InRange(PinObject->x3, PinObject->x4)) && (InRange(PinObject->y3, PinObject->y4)))
					{
						MentorPrintCircle(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1,
						                  PinObject->x2, PinObject->Thickness);
					}
					else
					{
						MentorPrintArc(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1,
						               PinObject->x3, PinObject->y3, PinObject->x4, PinObject->y4, PinObject->Thickness,
						               0);
					}
				}

				break;

			case OBJECT_TEXT:
				MentorPrintText(fp, PinObject);
				ok = 1;
				break;

			case OBJECT_POLYGON:
				GeomPolygon = (GeomPolygonRecord *) PinObject->Address;
				PolygonPoints = (PointRecord *) GeomPolygon->Points;
				MentorPrintPolygon(fp, GetMentorLayer(PinObject->Layer), PolygonPoints, PinObject->x1, PinObject->y1,
				                   GeomPolygon->NrVertices, 0);
				break;

			default:
				ok = 1;
				break;
			}
		}

// *******************************************************************************************************
		NrObjects6 = 0;
//    ShapeCompSilkScreenToObject(Shape,0.0,0.0,0.0);
		ShapeOtherToObject(Shape, 0.0, 0.0, 0.0, 0, 16 + 5);	// silkscreen
#ifdef _DEBUG

		if (cnt2 == 24)
			ok = 1;

		if (stricmp(ShapeNameP, "con4a_usb_a_angle_f_lowprofile") == 0)
		{
			ok = 1;				// NrMentorPins
		}

#endif

		for (cnt2 = 0; cnt2 < NrObjects6; cnt2++)
		{
			PinObject = &((*Objects6)[cnt2]);

			switch (PinObject->ObjectType)
			{
			case OBJECT_LINE:
				if (CalcLengthLine2(PinObject->x1, PinObject->y1, PinObject->x2, PinObject->y2) > 0.0)
				{
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1, PinObject->x2,
					                PinObject->y2, PinObject->Thickness);
				}

				break;

			case OBJECT_RECT:
				x1 = PinObject->x1;
				y1 = PinObject->y1;

				if (PinObject->Thickness != 0.0)
				{
					x2 = PinObject->x2 * 0.5;
					y2 = PinObject->y2 * 0.5;
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 - x2, y1 - y2, x1 - x2, y1 + y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 - x2, y1 + y2, x1 + x2, y1 + y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 + x2, y1 + y2, x1 + x2, y1 - y2,
					                PinObject->Thickness);
					MentorPrintPath(fp, GetMentorLayer(PinObject->Layer), x1 + x2, y1 - y2, x1 - x2, y1 - y2,
					                PinObject->Thickness);
				}
				else
				{
					x2 = PinObject->x2;
					y2 = PinObject->y2;
					MentorPrintRectPad(fp, GetMentorLayer(PinObject->Layer), x1, y1, x2, y2);
				}

				break;

			case OBJECT_ARC:
				if (PinObject->Thickness == 0.0)
				{
					MentorPrintCircle(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1, PinObject->x2,
					                  PinObject->Thickness);
				}
				else
				{
					if ((InRange(PinObject->x3, PinObject->x4)) && (InRange(PinObject->y3, PinObject->y4)))
					{
						MentorPrintCircle(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1,
						                  PinObject->x2, PinObject->Thickness);
					}
					else
					{
						MentorPrintArc(fp, GetMentorLayer(PinObject->Layer), PinObject->x1, PinObject->y1,
						               PinObject->x3, PinObject->y3, PinObject->x4, PinObject->y4, PinObject->Thickness,
						               0);
					}
				}

				break;

			case OBJECT_TEXT:
				MentorPrintText(fp, PinObject);
				ok = 1;
				break;

			case OBJECT_POLYGON:
				GeomPolygon = (GeomPolygonRecord *) PinObject->Address;
				PolygonPoints = (PointRecord *) GeomPolygon->Points;
				MentorPrintPolygon(fp, GetMentorLayer(PinObject->Layer), PolygonPoints, PinObject->x1, PinObject->y1,
				                   GeomPolygon->NrVertices, 0);
				break;

			default:
				ok = 1;
				break;
			}
		}

// *******************************************************************************************************
		NrObjects6 = 0;
		ShapeOtherToObject(Shape, 0.0, 0.0, 0.0, -1, 16 + 4);	// Routing keepout
		GeomLayer = GetMentorLayer(ROUTING_KEEPOUT_LAYER);
#ifdef _DEBUG

		if (cnt2 == 24)
			ok = 1;

		if (stricmp(ShapeNameP, "new_geom_4") == 0)
		{
			ok = 1;				// NrMentorPins
		}

#endif

		for (cnt2 = 0; cnt2 < NrObjects6; cnt2++)
		{
			PinObject = &((*Objects6)[cnt2]);

			switch (PinObject->ObjectType)
			{
			case OBJECT_RECT:
				x1 = PinObject->x1;
				y1 = PinObject->y1;
				x2 = PinObject->x2;
				y2 = PinObject->y2;
				MentorPrintRectPad2(fp, x1, y1, x2, y2);

				if (PinObject->Layer == ROUTING_KEEPOUT_LAYER + NrShapeLayers - 1)
					sprintf(str2, "$$attribute( \"%s\", \"PAD_1\", @mark, @scale );", GeomLayer);
				else
				{
					if (PinObject->Layer == ROUTING_KEEPOUT_LAYER)
						sprintf(str2, "$$attribute( \"%s\", \"PAD_2\", @mark, @scale );", GeomLayer);
					else
						sprintf(str2, "$$attribute( \"%s\", \"%s\", @mark, @scale );", GeomLayer, GeomLayer);
				}

				WriteLn(fp, str2);
				break;

			case OBJECT_ARC:
				x1 = PinObject->x1;
				y1 = PinObject->y1;
				x2 = PinObject->x2 * 0.5;

				for (cnt3 = 0; cnt3 < 32; cnt3++)
				{
					x3 = x1 + x2 * cos((2 * PI * cnt3) / 32);
					y3 = y1 + x2 * sin((2 * PI * cnt3) / 32);

					if (cnt3 == 0)
						sprintf(str2, "$$initial([%.4f,%.4f], , @nosnap );", x3 / UNITS_MM, y3 / UNITS_MM);
					else
						sprintf(str2, "$$terminal([%.4f,%.4f] );", x3 / UNITS_MM, y3 / UNITS_MM);

					WriteLn(fp, str2);
				}

				if (PinObject->Layer == ROUTING_KEEPOUT_LAYER + NrShapeLayers - 1)
					sprintf(str2, "$$attribute( \"%s\", \"PAD_1\", @mark, @scale );", GeomLayer);
				else
				{
					if (PinObject->Layer == ROUTING_KEEPOUT_LAYER)
						sprintf(str2, "$$attribute( \"%s\", \"PAD_2\", @mark, @scale );", GeomLayer);
					else
						sprintf(str2, "$$attribute( \"%s\", \"%s\", @mark, @scale );", GeomLayer, GeomLayer);
				}

				WriteLn(fp, str2);
				break;

			case OBJECT_POLYGON:
				GeomPolygon = (GeomPolygonRecord *) PinObject->Address;
				PolygonPoints = (PointRecord *) GeomPolygon->Points;
				MentorPrintPolygon(fp, GetMentorLayer(PinObject->Layer), PolygonPoints, PinObject->x1, PinObject->y1,
				                   GeomPolygon->NrVertices, 1);

				if (PinObject->Layer == ROUTING_KEEPOUT_LAYER + NrShapeLayers - 1)
					sprintf(str2, "$$attribute( \"%s\", \"PAD_1\", @mark, @scale );", GeomLayer);
				else
				{
					if (PinObject->Layer == ROUTING_KEEPOUT_LAYER)
						sprintf(str2, "$$attribute( \"%s\", \"PAD_2\", @mark, @scale );", GeomLayer);
					else
						sprintf(str2, "$$attribute( \"%s\", \"%s\", @mark, @scale );", GeomLayer, GeomLayer);
				}

				WriteLn(fp, str2);
				break;

			default:
				ok = 1;
				break;
			}
		}

		/*
		    NrObjects6=0;
		    ShapeOtherToObject(Shape,0.0,0.0,0.0,3,16);
		*/
// *******************************************************************************************************
		NrObjects6 = 0;
		ShapeOtherToObject(Shape, 0.0, 0.0, 0.0, 0, 16 + 2);	// Info layers
#ifdef _DEBUG

		if (cnt2 == 24)
			ok = 1;

		if (stricmp(ShapeNameP, "new_geom_4") == 0)
		{
			ok = 1;				// NrMentorPins
		}

#endif

		for (cnt2 = 0; cnt2 < NrObjects6; cnt2++)
		{
			PinObject = &((*Objects6)[cnt2]);
			GeomLayer = GetMentorLayer(PinObject->Layer);

			switch (PinObject->ObjectType)
			{
			case OBJECT_LINE:
				if (CalcLengthLine2(PinObject->x1, PinObject->y1, PinObject->x2, PinObject->y2) > 0.0)
				{
					MentorPrintPath(fp, GeomLayer, PinObject->x1, PinObject->y1, PinObject->x2, PinObject->y2,
					                PinObject->Thickness);
				}

				break;

			case OBJECT_RECT:
				x1 = PinObject->x1;
				y1 = PinObject->y1;

				if (PinObject->Thickness != 0.0)
				{
					x2 = PinObject->x2 * 0.5;
					y2 = PinObject->y2 * 0.5;
					MentorPrintPath(fp, GeomLayer, x1 - x2, y1 - y2, x1 - x2, y1 + y2, PinObject->Thickness);
					MentorPrintPath(fp, GeomLayer, x1 - x2, y1 + y2, x1 + x2, y1 + y2, PinObject->Thickness);
					MentorPrintPath(fp, GeomLayer, x1 + x2, y1 + y2, x1 + x2, y1 - y2, PinObject->Thickness);
					MentorPrintPath(fp, GeomLayer, x1 + x2, y1 - y2, x1 - x2, y1 - y2, PinObject->Thickness);
				}
				else
				{
					x2 = PinObject->x2;
					y2 = PinObject->y2;
					MentorPrintRectPad(fp, GetMentorLayer(PinObject->Layer), x1, y1, x2, y2);
				}

				break;

			case OBJECT_ARC:
				if (PinObject->Thickness == 0.0)
					MentorPrintCircle(fp, GeomLayer, PinObject->x1, PinObject->y1, PinObject->x2, PinObject->Thickness);
				else
				{
					if ((InRange(PinObject->x3, PinObject->x4)) && (InRange(PinObject->y3, PinObject->y4)))
					{
						MentorPrintCircle(fp, GeomLayer, PinObject->x1, PinObject->y1, PinObject->x2,
						                  PinObject->Thickness);
					}
					else
					{
						MentorPrintArc(fp, GeomLayer, PinObject->x1, PinObject->y1, PinObject->x3, PinObject->y3,
						               PinObject->x4, PinObject->y4, PinObject->Thickness, 0);
					}
				}

				break;

			case OBJECT_TEXT:
				MentorPrintText(fp, PinObject);
				break;

			case OBJECT_POLYGON:
				GeomPolygon = (GeomPolygonRecord *) PinObject->Address;
				PolygonPoints = (PointRecord *) GeomPolygon->Points;
				MentorPrintPolygon(fp, GetMentorLayer(PinObject->Layer), PolygonPoints, PinObject->x1, PinObject->y1,
				                   GeomPolygon->NrVertices, 0);
				break;

			default:
				ok = 1;
				break;
			}
		}
	}

	FileClose(fp);
	sprintf(str2, "Mentor geometry file %s\\%s_geom created\r\n", DesignPath, DesignName);
	AddMessage(str2);
//  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str2);
	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
