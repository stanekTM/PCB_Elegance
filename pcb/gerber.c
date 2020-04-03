/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: gerber.c
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
#include "graphics.h"
#include "string.h"
#include "select4.h"
#include "select2.h"
#include "trace2.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "ellipss.h"
#include "nets.h"
#include "pcb.h"
#include "calc.h"
#include "files2.h"
#include "gerber.h"
#include "gerber2.h"
#include "gerber3.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "calc4.h"
#include "calcdef.h"
#include "calcrect.h"
#include "calcdiag.h"
#include "insdel.h"
#include "select3.h"
#include "polygon.h"
#include "mainloop.h"
#include "toets.h"
#include "rect.h"
#include "math.h"
#include "dialogs.h"
#include "time.h"
#include "plot.h"
#include "owntime.h"
#include "ctype.h"
#include "../functionsc/version.h"

#define  MAX_APERTURE_POLYGON_DEFS        1024

typedef struct
{
	uint8 *Address;
	int32 PolygonType;
	int32 PolygonNr;
	int32 Mirror;
} AperturePolygonUsageRecord;

typedef struct
{
	int32 Layer;
	int32 Visible;
	uint32 ColourRed, ColourGreen, ColourBlue;
} GerbvProjectRecord;

AperTureArray *AperTures, *AperTures2, *DrillAperTures;
HGLOBAL AperTuresGlobal, DrillAperTuresGlobal;
AperTureArray *LoadedAperTures;
HGLOBAL LoadedAperTuresGlobal;

AperTureRecord LastRoundPadAperTure, LastRectPadAperTure, LastPolygonPadAperTure, LastPlatedDrillAperTure,
               LastUnPlatedDrillAperTure, LastThermalReliefAperTure, LastTraceAperTure, NewAperTure;

AperTureRecord LastButterflyPadAperTure;


int32 MaxNrAperTures, NrAperTures, NrDrillAperTures, GerberLineairInterpolation, NrAperTures2, NrLoadedAperTures;

int32 GerberMode = 1;
int32 PlotErrors;

int32 DcodeRoundPad;
int32 DcodeRectPad;
int32 DcodeTrace;
int32 DcodeThermalRelief;
int32 DcodePolygonPad;

int32 DcodeDrill;
int32 PlotError;
int32 GerberPrepareErrors;
int32 Gerberfp;

int32 ChangeSilkScreenPen, ChangeSilkScreenPenAsk;

double SpecialLineThickNess, TextPen;
double AreafillPenSize1, AreafillPenSize2, PowerPlanePenSize1, PowerPlanePenSize2;

char GerberListFilename[MAX_LENGTH_STRING];
char GerbvProjectFilename[MAX_LENGTH_STRING];



extern double LastGerberX, LastGerberY, CurrentPlotPenSize, DesignBoardOriginX, DesignBoardOriginY, DesignBoardWidth,
       DesignBoardHeight;

extern int Plotfp;
extern int32 PlotDrawingOk, PenPlotMode;
extern char LineBufGerber[MAX_LENGTH_STRING];

extern int32 ProjectIndexNr;
extern ProjectInfoRecord *ProjectInfo;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PenPlotOutput(int32 mode)
{
	char InfoCopy[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	int32 res;

	GerberInfo.ScaleFactor = 1.0;

	if (PlotDialog(1) == 2)
		return -1;

	res = FindMinMaxBoard(&DesignBoardOriginX, &DesignBoardOriginY, &DesignBoardWidth, &DesignBoardHeight, 3);
	DesignBoardWidth -= DesignBoardOriginX;
	DesignBoardHeight -= DesignBoardOriginY;
	SetWaitCursor();
	PenPlotMode = 1;
	AreafillPenSize1 = GerberInfo.PenSizes[0];
	AreafillPenSize2 = AreafillPenSize1;
	PowerPlanePenSize1 = GerberInfo.PenSizes[0];
	PowerPlanePenSize2 = GerberInfo.PenSizes[1];

	if (PowerPlanePenSize2 == 0.0)
		PowerPlanePenSize2 = PowerPlanePenSize1 * 7.0;

	strcpy(InfoCopy, InfoStr);

	if (NrGerberLayers > 0)
	{
		if ((NrGerberLayers > 1) || (GerberLayers[0] != DRILL_LAYER))
			GerberOutput(0);

		sprintf(str, SC(398, "Penplot output ready.\n\nHPGL files are located in directory\n\n%s\\pcb\\hpgl"), DesignPath);
		MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
	}

	strcpy(InfoStr, InfoCopy);
	RedrawInfoStr(1);
	SetNormalCursor();
	return 0;
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

void GerbvProjectOutput(void)
{
	int32 Layer, NrLayers = 0, Found = 0, cnt, cnt2;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], FilePart[MAX_LENGTH_STRING];
	// *INDENT-OFF*
	GerbvProjectRecord GerbvProjectObj[] = 
	{ //Layer,				Displayed,	Red,	Green,	Blue
	{	DRILL_LAYER,				1,	46260,	7710,	7710,	},
	{ BOARD_OUTLINE_LAYER,		1,	0,		65535,	46049,	},
	{ COMP_OUTLINE_LAYER_TOP,		0,	65535,	32639,	29555,	},
	{ SILKSCREEN_TOP_REFS,		0,	43079,	58081,	65535,	},
	{ SILKSCREEN_TOP_VALUES,		0,	43079,	58081,	65535,	},
	{ SILKSCREEN_TOP,				0,	43079,	58081,	65535,	},
	{ PASTE_MASK_TOP,				0,	17969,	17969,	17969,	},
	{ SOLD_MASK_TOP,				0,	65535,	46903,	0,		},
	{ ROUTING_KEEPOUT_LAYER + 15,	0,	44975,	44975,	11565,	},
	{ 15,							0,	49601,	0,		57568,	},
	{ ROUTING_KEEPOUT_LAYER + 14,	0,	44975,	44975,	11565,	},
	{ 14,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 13,	0,	44975,	44975,	11565,	},
	{ 13,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 12,	0,	44975,	44975,	11565,	},
	{ 12,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 11,	0,	44975,	44975,	11565,	},
	{ 11,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 10,	0,	44975,	44975,	11565,	},
	{ 10,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 9,	0,	44975,	44975,	11565,	},
	{ 9,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 8,	0,	44975,	44975,	11565,	},
	{ 8,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 7,	0,	44975,	44975,	11565,	},
	{ 7,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 6,	0,	44975,	44975,	11565,	},
	{ 6,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 5,	0,	44975,	44975,	11565,	},
	{ 5,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 4,	0,	44975,	44975,	11565,	},
	{ 4,							0,	49601,	24000,	57568,	},
	{ ROUTING_KEEPOUT_LAYER + 3,	0,	44975,	44975,	11565,	},
	{ 3,							0,	15789,	55934,	58547,	},
	{ ROUTING_KEEPOUT_LAYER + 2,	0,	44975,	44975,	11565,	},
	{ 2,							0,	36729,	22379,	65535,	},
	{ ROUTING_KEEPOUT_LAYER + 1,	0,	44975,	44975,	11565,	},
	{ 1,							0,	11565,	23387,	65535,	},
	{ COMP_OUTLINE_LAYER_BOTTOM,	0,	65535,	32639,	29555,	},
	{ SILKSCREEN_BOTTOM_REFS,		0,	59448,	48225,	65335,	},
	{ SILKSCREEN_BOTTOM_VALUES,	0,	59448,	48225,	65335,	},
	{ SILKSCREEN_BOTTOM,			0,	59448,	48225,	65335,	},
	{ PASTE_MASK_BOTTOM,			0,	17969,	17969,	17969,	},
	{ SOLD_MASK_BOTTOM,			0,	65535,	46903,	0,		},
	{ ROUTING_KEEPOUT_LAYER,		0,	44975,	44975,	11565,	},
	{ 0,							0,	65535,	32896,	65535,	},
	{ INFO_LAYER,					0,	65535,	32639,	29555,	},
	{ INFO_LAYER2,				0,	65535,	32639,	29555,	},
	{ INFO_LAYER3,				0,	65535,	32639,	29555,	},
	{ INFO_LAYER4,				0,	65535,	32639,	29555,	},
	};
	int32 NrGerbvProjectObj = sizeof(GerbvProjectObj) / sizeof(GerbvProjectRecord);
	// *INDENT-ON*

	sprintf(str, "(gerbv-file-version! \"2.0A\")");
	AppendStringToTextFileUTF8(GerbvProjectFilename, str);
	sprintf(str, "(set-render-type! 2)");
	AppendStringToTextFileUTF8(GerbvProjectFilename, str);

	for (cnt = 0; cnt < NrGerbvProjectObj; cnt++)
	{
		Layer = GerbvProjectObj[cnt].Layer;

		for (cnt2 = 0; cnt2 < NrGerberLayers; cnt2++)
		{
			if (GerberLayers[cnt2] == Layer)
			{
				Found = 1;
				break;
			}
		}

		if ((Found == 1) && (Layer == DRILL_LAYER))
		{
			Found = 0;
			sprintf(str, "(define-layer! %d ", NrLayers++);
			GetFilePartFromFileName(FilePart, EditFile);
			CutExtensionFileName(FilePart);

			if (GerberInfo.DrillOutputOption == 0)
				sprintf(str2, "(cons \'filename \"Drills.drl\")", FilePart); //název v souboru GerbvProject.gvp
			else
				sprintf(str2, "(cons \'filename \"Drills.ncd\")", FilePart); //název v souboru GerbvProject.gvp

			strcat(str, str2);

			if (GerbvProjectObj[cnt].Visible)
				sprintf(str2, "(cons \'visible #t)");
			else
				sprintf(str2, "(cons \'visible #f)");

			strcat(str, str2);
			sprintf(str2, "(cons \'color #(%d %d %d))",
				GerbvProjectObj[cnt].ColourRed, GerbvProjectObj[cnt].ColourGreen,
				GerbvProjectObj[cnt].ColourBlue);
			strcat(str, str2);
			sprintf(str2, "(cons \'attribs (list (list \'autodetect \'Boolean 0) ");
			strcat(str, str2);
			sprintf(str2, "(list \'zero_supression \'Enum 0) ");
			strcat(str, str2);

			if ((GerberInfo.GerberNumberMode & 8) == 0)
				sprintf(str2, "(list \'units \'Enum 0) "); //inch
			else
				sprintf(str2, "(list \'units \'Enum 1) "); //mm

			strcat(str, str2);
			sprintf(str2, "(list \'digits \'Integer %d))))", GerberInfo.GerberNumberMode & 7);
			strcat(str, str2);
			AppendStringToTextFileUTF8(GerbvProjectFilename, str);
		}

		if ( (Found == 1) || ((Layer == DRILL_LAYER) && (GerberInfo.DrillAsGerber == 1)) )
		{
			Found = 0;

			if (!GerberInfo.ReverseLayerNumbering)
				GetLayerTextObjects(Layer, str2, 64 + 5);
			else
				GetLayerTextObjects(Layer, str2, 64 + 16 + 5);

			sprintf(str, "(define-layer! %d (cons \'filename \"%s.ger\")", NrLayers++, str2);

			if (GerbvProjectObj[cnt].Visible)
				sprintf(str2, "(cons \'visible #t)");
			else
				sprintf(str2, "(cons \'visible #f)");

			strcat(str, str2);
			sprintf(str2, "(cons \'color #(%d %d %d)))",
				GerbvProjectObj[cnt].ColourRed, GerbvProjectObj[cnt].ColourGreen,
				GerbvProjectObj[cnt].ColourBlue);
			strcat(str, str2);
			AppendStringToTextFileUTF8(GerbvProjectFilename, str);
		}
	}
}

//*********************************************************************************************************************
//************************************* generování souboru "GerberFiles.txt" ve složce gerber *************************
//*********************************************************************************************************************

void OutputGerberDrill()
{
	char InfoCopy[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	int32 cnt3, Layer, res;


	if (AreafillPenSize1 == 0.0)
	{
		AreafillPenSize1 = min(Design.StandardTraceWidth / 2, Design.StandardClearance / 2);

		if (AreafillPenSize1 < 2 * 2540.0)
			AreafillPenSize1 = 2 * 2540.0;

		PowerPlanePenSize2 = (100 * 2540.0);
		PowerPlanePenSize1 = AreafillPenSize1;
		AreafillPenSize2 = AreafillPenSize1 * 4;
	}

	strcpy(InfoCopy, InfoStr);

	if (GerberDialog(0) == 1)
	{
		res = FindMinMaxBoard(&DesignBoardOriginX, &DesignBoardOriginY, &DesignBoardWidth, &DesignBoardHeight, 3);
		DesignBoardWidth -= DesignBoardOriginX;
		DesignBoardHeight -= DesignBoardOriginY;

		switch (res)
		{
		case 2:
			break;

		case 3:
			sprintf(str, SC(400, "There are objects outside the board outline\r\n"));
			strcat(str, SC(401, "Do you want to continue ?"));

			if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDNO)
				return;

			break;
		}

		SetWaitCursor();
		PenPlotMode = 0;

		DcodeRoundPad = GERBER_PAD_ROUND;
		DcodeRectPad = GERBER_PAD_RECT;
		DcodeTrace = GERBER_TRACE;
		DcodeThermalRelief = GERBER_PAD_THERMAL_RELIEF;
		DcodePolygonPad = GERBER_PAD_POLYGON;
		DcodeDrill = 1;


		if (NrGerberLayers > 0)
		{
			sprintf(GerberListFilename, "%s\\pcb\\gerber\\GerberFiles.txt", DesignPath); //název souboru
			DeleteFileUTF8(GerberListFilename);
			
			if ((NrGerberLayers > 1) || (GerberLayers[0] != DRILL_LAYER) || (GerberInfo.DrillAsGerber == 1))
			{
				if (GerberInfo.GerberOutputMode == 0)
					GerberOutput(0);
				else
					GerberOutput(2);
			}
			
			for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
			{
				if ((Layer = GerberLayers[cnt3]) == DRILL_LAYER)
					DrillOutput();
			}

			if (GerberInfo.GerbvProject == 1)
			{
				sprintf(GerbvProjectFilename, "%s\\pcb\\gerber\\GerbvProject.gvp", DesignPath); //název souboru
				DeleteFileUTF8(GerbvProjectFilename);
				GerbvProjectOutput();
			}
		}

		if (GerberInfo.OutputNeutral == 1)
		{
			sprintf(InfoStr, SC(402, "Writing neutral file"));
			RedrawInfoStr(1);

			if ((GerberInfo.GerberNumberMode & 8) == 0)
			{	// INCH
				WriteNeutralFile(0);
			}
			else
				WriteNeutralFile(1);
		}

		sprintf(str, SC(403, "Gerber/drill output ready.\n\nOutput files are located in directory\n\n%s\\pcb\\gerber"), DesignPath);
		MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
		SetNormalCursor();
	}

	strcpy(InfoStr, InfoCopy);
	RedrawInfoStr(1);
}

// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************
// ******************************************************************************************************************************

int32 GerberOutput(int32 mode)
{
	int32 Layer, ok, cnt, cnt2, cnt3, MemSizeAreaFill, Mirror, ApertureMacroPolygons, AperturePolygonUsageCount,
	      NrTraceObjects, count, DCode, CompOutlineLayers, Layer2, NrCheckObjects, ButterFlyMacroWritten, TempUnits, res,
	      NrTempGerberLayers, TempGerberLayers[64], ApertureIndex[2048], PowerPlaneLayer, UseMacros, WriteThermalReliefs;
	double x1, y1, Rotation;

	ObjectRecord *Object, *Object4, *Object4a;
	ObjectLineRecord *ObjectLine;
	ObjectPolygonRecord *ObjectPolygon;

	AperturePolygonUsageRecord AperturePolygonUsage[MAX_APERTURE_POLYGON_DEFS];
	AreaFillRecord *AreaFill;
	AperTureRecord *AperTure;
	PolygonRecord *Polygon;
	GeomPolygonRecord *GeomPolygon;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     FileStr[MAX_LENGTH_STRING];

	Object = NULL;
	Object4 = NULL;
	Object4a = NULL;
	ObjectLine = NULL;
	TempUnits = UNITS_INCH;
	GerberPrepareErrors = 0;
	ResetPerformanceStrings();

	if (AllocateMemAperTures(1024) != 0)
		return -1;

	DeleteAperTures();
	MaxNrAperTures = 0;
	NrLoadedAperTures = 0;

	if (AllocateMemObjects4(4096) == -1)
		return -1;

	if (PenPlotMode)
		CurrentPlotPenSize = GerberInfo.PenSizes[0];

	ChangeSilkScreenPen = 0;
	ChangeSilkScreenPenAsk = 0;
	memmove(&TempGerberLayers, &GerberLayers, sizeof(GerberLayers));
//  NrGerberLayers = 4;
	NrTempGerberLayers = 0;

	for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
	{
		if (GerberLayers[cnt3] != DRILL_LAYER)
			TempGerberLayers[NrTempGerberLayers++] = GerberLayers[cnt3];
	}

	if (GerberInfo.DrillAsGerber)
		TempGerberLayers[NrTempGerberLayers++] = DRILL_LAYER;

	CompOutlineLayers = 0;

	for (cnt3 = 0; cnt3 < NrTempGerberLayers; cnt3++)
	{
		Layer = TempGerberLayers[cnt3];

		switch (Layer)
		{
		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			CompOutlineLayers++;
			break;
		}
	}


	SystemBusyMode = 400;
	SetTimer0();

// ****************************************************************************
// ****************************************************************************

	for (cnt3 = 0; cnt3 < NrTempGerberLayers; cnt3++)
	{
		Layer = TempGerberLayers[cnt3];
		LastGerberX = -1e9;
		LastGerberY = -1e9;
#ifdef _DEBUG

		if (Layer == 1)
			ok = 1;

#endif
		PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
		LineBufGerber[0] = 0;

		if (!PenPlotMode)
			sprintf(FileStr, "%s\\pcb\\gerber\\", DesignPath);
		else
			sprintf(FileStr, "%s\\pcb\\hpgl\\", DesignPath);

		if (!GerberInfo.ReverseLayerNumbering)
			GetLayerTextObjects(Layer, str2, 64 + 5);
		else
			GetLayerTextObjects(Layer, str2, 64 + 16 + 5);

		strcat(FileStr, str2);

		if (!PenPlotMode)
		{
			strcat(FileStr, ".ger");
			sprintf(str3, "\"%s.ger\"", str2);
			AppendStringToTextFileUTF8(GerberListFilename, str3);
		}
		else
			strcat(FileStr, ".hgl");

		//********************************************** nelze vytvoøit soubor *********************************************************
		if ((Gerberfp = FileOpenWriteUTF8(FileStr)) <= 0)
		{
			sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
			return -2;
		}
		//******************************************************************************************************************************

		if (!GerberInfo.ReverseLayerNumbering)
			GetLayerTextObjects(Layer, str, 64);
		else
			GetLayerTextObjects(Layer, str, 64 + 16);

		str[0] = (char) tolower(str[0]);
		sprintf(InfoStr, "Plot %s", str);
		RedrawInfoStr(1);
		CheckInputMessages(0);

// ****************************************************************************
// ****************************************************************************
		if (!PenPlotMode)
		{
			// Gerber initialisation code
			if (!GerberInfo.ReverseLayerNumbering)
				GetLayerTextObjects(Layer, str2, 64 + 5);
			else
				GetLayerTextObjects(Layer, str2, 64 + 16 + 5);

			sprintf(str, "G04 %s *", str2);
			WriteGerberString(str, 1);
			GetFilePartFromFileName(str3, EditFile);
			sprintf(str, "G04 Layout %s *", str3);
			WriteGerberString(str, 1);
			sprintf(str, "G04 Designed with PCB elegance %d.%d *", VER_VERSION / 100, VER_VERSION % 100);
			WriteGerberString(str, 1);
			sprintf(str, "G04 *");
			WriteGerberString(str, 1);
			sprintf(str, "G04 *");
			WriteGerberString(str, 1);

			if ((mode & 2) == 0)
			{	// RS274D gerber mode
				sprintf(str, "G90*");
				WriteGerberString(str, 1);
				sprintf(str, "G70*");
				WriteGerberString(str, 1);
			}

			GerberLineairInterpolation = 1;
		}
		else
		{
			// Penplot initialisation code
			sprintf(str, "VS%i;SP1;PU0,0;", (int32) GerberInfo.PenSpeed);
			WriteGerberString(str, 0);
		}

		if ((mode & 2) == 2)
		{	// RS274X gerber mode
			DeleteAperTures();
			NrLoadedAperTures = 0;
			DcodeRoundPad = GERBER_PAD_ROUND;
			DcodeRectPad = GERBER_PAD_RECT;
			DcodeTrace = GERBER_TRACE;
			DcodeThermalRelief = GERBER_PAD_THERMAL_RELIEF;
			DcodePolygonPad = GERBER_PAD_POLYGON;
			DcodeDrill = 1;
		}

		PlotErrors = 0;

		sprintf(str, SC(407, "Get info layer %d"), Layer);
		AddPerformanceValue(str);

// ****************************************************************************
// ****************************************************************************

// Collect all layer objects to Objects4 and fill in the aperture table
		NrObjects4 = 0;
		NrTraceObjects = 0;
		NrCheckObjects = 0;

		if (Layer < 32)
		{
			NrCheckObjects = GerberPrepareComponentPins(Layer, 0);

			if (!PenPlotMode)
				NrTraceObjects = GerberPrepareTraces(Layer, 0);
			else
				NrTraceObjects = GerberPrepareTraces(Layer, 1);

			NrCheckObjects += NrTraceObjects;
			NrCheckObjects += GerberPrepareVias(Layer, 0);
		}

		switch (Layer)
		{
		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
			GerberPrepareComponentPinsViasSolderPasteObjects(Layer, 0);
			break;

		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
			GerberPrepareComponentInfoObjects(Layer, 0);
			break;

		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			GerberPrepareComponentOutlineObjects(Layer, CompOutlineLayers, 0);
			break;

		case BOARD_OUTLINE_LAYER:
			if (!GerberInfo.PlotBoardOutline)
				GerberPrepareComponentBoardOutlineObjects(Layer, 0);

			break;

		case SILKSCREEN_BOTTOM:
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM_REFS:
		case SILKSCREEN_TOP_REFS:
		case SILKSCREEN_BOTTOM_VALUES:
		case SILKSCREEN_TOP_VALUES:
			GerberPrepareComponentSilkScreenObjects(Layer, 0);
			break;

		case DRILL_LAYER:
			NrCheckObjects = GerberPrepareComponentPins(Layer, 1);
			NrCheckObjects += GerberPrepareVias(Layer, 1);
			break;
		}

		if (GerberInfo.PlotBoardOutline)
			GerberPrepareComponentBoardOutlineObjects(BOARD_OUTLINE_LAYER, 0);

		if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			GerberPrepareComponentRoutingKeepoutObjects(Layer, 0);

		GerberPrepareSpecialObjects(Layer, 0);

		if (!PenPlotMode)
		{
			if (GerberInfo.PlotMode & 1)
			{
				if (PowerPlaneLayer)
				{
					CheckTraceAperTure(PowerPlanePenSize1);
					CheckTraceAperTure(PowerPlanePenSize2);
				}

				CheckTraceAperTure(AreafillPenSize1);
				CheckTraceAperTure(AreafillPenSize2);
			}

			/*    Line for design/layer/date text */
			TextPen = max((6.0 * 2540.0), Design.SilkScreenWidth);
			ok = CheckTraceAperTure(TextPen);
		}

// ****************************************************************************
// ****************************************************************************

		if ((mode & 2) == 2)
		{	// RS274X gerber mode
// Write gerber 274X apertures
			if ((GerberInfo.GerberNumberMode & 8) == 0)
			{	// INCH
				TempUnits = UNITS_INCH;

				switch (GerberInfo.GerberNumberMode & 7)
				{
				case 2:
					sprintf(str, "%%FSLAX22Y22*%%");
					WriteGerberString(str, 1);
					break;

				case 3:
					sprintf(str, "%%FSLAX23Y23*%%");
					WriteGerberString(str, 1);
					break;

				case 4:
					sprintf(str, "%%FSLAX24Y24*%%");
					WriteGerberString(str, 1);
					break;

				case 5:
					sprintf(str, "%%FSLAX25Y25*%%");
					WriteGerberString(str, 1);
					break;

				case 6:
					sprintf(str, "%%FSLAX26Y26*%%");
					WriteGerberString(str, 1);
					break;
				}

				sprintf(str, "%%MOIN*%%");
				WriteGerberString(str, 1);
			}
			else
			{	// MM
				TempUnits = UNITS_MM;

				switch (GerberInfo.GerberNumberMode & 7)
				{
				case 2:
					sprintf(str, "%%FSLAX32Y32*%%");
					WriteGerberString(str, 1);
					break;

				case 3:
					sprintf(str, "%%FSLAX33Y33*%%");
					WriteGerberString(str, 1);
					break;

				case 4:
					sprintf(str, "%%FSLAX34Y34*%%");
					WriteGerberString(str, 1);
					break;

				case 5:
					sprintf(str, "%%FSLAX42Y42*%%");
					WriteGerberString(str, 1);
					break;

				case 6:
					sprintf(str, "%%FSLAX43Y43*%%");
					WriteGerberString(str, 1);
					break;

				case 7:
					sprintf(str, "%%FSLAX44Y44*%%");
					WriteGerberString(str, 1);
					break;
				}

				sprintf(str, "%%MOMM*%%");
				WriteGerberString(str, 1);
			}

			WriteGerberString("G75*", 1);
			sprintf(str, "G01*");
			WriteGerberString(str, 1);

			if (!PowerPlaneLayer)
			{
				sprintf(str, "%%IPPOS*%%");
				WriteGerberString(str, 1);
			}
			else
			{
				sprintf(str, "%%IPNEG*%%");
				WriteGerberString(str, 1);
			}

#ifdef _DEBUG

			if (Layer == 1)
				ok = 1;

#endif

// Write gerber 274X macro apertures definitions if available
			UseMacros = 0;

			for (cnt = 0; cnt < NrAperTures; cnt++)
			{
				AperTure = &((*AperTures)[cnt]);

				if (AperTure->Info == GERBER_PAD_POLYGON)
					UseMacros = 1;
			}

			ButterFlyMacroWritten = 0;
			ApertureMacroPolygons = 0;
			AperturePolygonUsageCount = 0;

			for (cnt = 0; cnt < MAX_APERTURE_POLYGON_DEFS; cnt++)
				AperturePolygonUsage[cnt].PolygonNr = -1;

			if (UseMacros)
			{
				for (cnt = 0; cnt < NrAperTures; cnt++)
				{
					AperTure = &((*AperTures)[cnt]);

					switch (AperTure->Info)
					{
					case GERBER_PAD_POLYGON:

						/*

						%AMMPOLYGON1*
						4,1,NrVertices+1,
						X1,Y1,
						..
						..
						X(NrVertices-1),Y(NrVertices-1),
						X1,Y1,
						$1*%


						%ADD18MPOLYGON1,Rotation*%

						%ADD18MPOLYGON1,45.0*%


						  AperturePolygonUsageRecord AperturePolygonUsage[MAX_APERTURE_POLYGON_DEFS];

						typedef struct {
						          uint8   *Address;
						          int32  PolygonNr;
						        } AperturePolygonUsageRecord;

						*/
						if (AperTure->SpecialType == 0)
						{
							cnt2 = 0;

							while ((cnt2 < AperturePolygonUsageCount)
							        && ((AperturePolygonUsage[cnt2].Mirror != AperTure->Mirror)
							            || (AperturePolygonUsage[cnt2].Address != AperTure->Address)))
								cnt2++;

							if ((cnt2 == AperturePolygonUsageCount) && (cnt2 < MAX_APERTURE_POLYGON_DEFS))
							{
								AperturePolygonUsage[cnt2].Address = AperTure->Address;
								AperturePolygonUsage[cnt2].Mirror = AperTure->Mirror;
								AperturePolygonUsageCount++;
								sprintf(str, "%%AMPOLYGON%d*", AperturePolygonUsageCount);
								WriteGerberString(str, 1);
								GeomPolygon = (GeomPolygonRecord *) AperTure->Address;
								count = GeomPolygon->NrVertices;
								sprintf(str, "4,1,%d,", count);
								WriteGerberString(str, 1);
								Mirror = AperTure->Mirror;

								for (cnt2 = 0; cnt2 < min(count, 512); cnt2++)
								{
									x1 = GeomPolygon->Points[cnt2].x;
									y1 = GeomPolygon->Points[cnt2].y;

									if (Mirror == 1)
										x1 = -x1;

									switch (TempUnits)
									{
									case UNITS_INCH:
										sprintf(str, "%.7f,%.7f,", x1 / 2540000, y1 / 2540000);
										break;

									case UNITS_MM:
										sprintf(str, "%.5f,%.5f,", x1 / 100000, y1 / 100000);
										break;
									}

									WriteGerberString(str, 1);
								}

								x1 = GeomPolygon->Points[0].x;
								y1 = GeomPolygon->Points[0].y;

								if (Mirror == 1)
									x1 = -x1;

								switch (TempUnits)
								{
								case UNITS_INCH:
									sprintf(str, "%.7f,%.7f,", x1 / 2540000, y1 / 2540000);
									break;

								case UNITS_MM:
									sprintf(str, "%.5f,%.5f,", x1 / 100000, y1 / 100000);
									break;
								}

								WriteGerberString(str, 1);
								WriteGerberString("$1*%", 1);
							}
						}
						else
						{
							if (!ButterFlyMacroWritten)
							{
								sprintf(str, "%%AMBUTTERFLY*");
								WriteGerberString(str, 1);
								sprintf(str, "21,1,$1,$2,0,0,$3*%%");
								WriteGerberString(str, 1);
								ButterFlyMacroWritten = 1;
							}
						}

						break;
					}
				}
			}

			WriteThermalReliefs = 0;

			for (cnt = 0; cnt < NrAperTures; cnt++)
			{
				AperTure = &((*AperTures)[cnt]);

				if (AperTure->Info == GERBER_PAD_THERMAL_RELIEF)
					WriteThermalReliefs = 1;
			}

			if (WriteThermalReliefs)
			{
				sprintf(str, "%%AMTHERMAL*");
				WriteGerberString(str, 1);
				sprintf(str, "7,0.0,0.0,$1,$2,$3,0*%%");
				WriteGerberString(str, 1);
			}

// ****************************************************************************
// ****************************************************************************

			for (cnt = 0; cnt < 2048; cnt++)
				ApertureIndex[cnt] = -1;

			for (cnt = 0; cnt < NrAperTures; cnt++)
			{
				AperTure = &((*AperTures)[cnt]);
				DCode = AperTure->AperTureCode;
#ifdef _DEBUG

				if (Layer == INFO_LAYER3)
				{
					ok = 1;

					if (AperTure->AperTureCode == 252)
						ok = 1;
				}

#endif

				if (DCode < 2048)
				{
#ifdef _DEBUG

					if (ApertureIndex[DCode] != -1)
						ok = 1;

#endif
					ApertureIndex[DCode] = cnt;
				}
			}

// Write gerber 274X macro apertures definitions
//      for (cnt=0;cnt<NrAperTures;cnt++)

			for (cnt = 0; cnt < 2048; cnt++)
			{
				if (ApertureIndex[cnt] != -1)
				{
					AperTure = &((*AperTures)[ApertureIndex[cnt]]);
#ifdef _DEBUG

					if (Layer == INFO_LAYER3)
					{
						ok = 1;

						if (AperTure->AperTureCode == 252)
							ok = 1;
					}

					if (ApertureIndex[cnt] == 9)
						ok = 1;

#endif
					str[0] = 0;

					switch (TempUnits)
					{
					case UNITS_INCH:
						sprintf(str2, "%.6f", ConvertToUnits(UNITS_INCH, AperTure->x));
						sprintf(str3, "%.6f", ConvertToUnits(UNITS_INCH, AperTure->y));
						break;

					case UNITS_MM:
						sprintf(str2, "%.4f", AperTure->x / 100000);
						sprintf(str3, "%.4f", AperTure->y / 100000);
						break;
					}

					switch (AperTure->Info)
					{
					case GERBER_TRACE:
					case GERBER_PAD_ROUND:
						sprintf(str, "%%ADD%iC,%s*%%", AperTure->AperTureCode, str2);
						break;

					case GERBER_PAD_RECT:
						sprintf(str, "%%ADD%iR,%sX%s*%%", AperTure->AperTureCode, str2, str3);
						break;

					case GERBER_PAD_THERMAL_RELIEF:
						switch (TempUnits)
						{
						case UNITS_INCH:
							sprintf(str2, "%.6fX%.6fX%.6f", ConvertToUnits(UNITS_INCH, AperTure->y),
							        ConvertToUnits(UNITS_INCH, AperTure->x), ConvertToUnits(UNITS_INCH, AperTure->x2));
							break;

						case UNITS_MM:
							sprintf(str2, "%.4fX%.4fX%.4f", ConvertToUnits(UNITS_MM, AperTure->y),
							        ConvertToUnits(UNITS_MM, AperTure->x), ConvertToUnits(UNITS_MM, AperTure->x2));
							break;
						}

						sprintf(str, "%%ADD%iTHERMAL,%s*%%", AperTure->AperTureCode, str2);
						break;

					case GERBER_PAD_POLYGON:
						if (AperTure->SpecialType == 0)
						{
							cnt2 = 0;

							while ((cnt2 < AperturePolygonUsageCount)
							        && ((AperturePolygonUsage[cnt2].Mirror != AperTure->Mirror)
							            || (AperturePolygonUsage[cnt2].Address != AperTure->Address)))
								cnt2++;

							if (cnt2 < AperturePolygonUsageCount)
							{
								Rotation = AperTure->Rotation;

								if (Rotation < 0)
									Rotation += 360.0;

								if (Rotation > 360.0)
									Rotation -= 360.0;

								sprintf(str2, "%.2f", Rotation);
								/*
								%ADD18MPOLYGON1,45.0*%
								*/
								sprintf(str, "%%ADD%iPOLYGON%d,%s*%%", AperTure->AperTureCode, cnt2 + 1, str2);
							}
						}
						else
						{
							switch (AperTure->SpecialType)
							{
							case OBJECT_RECT:
							case PIN_SMD_RECT:
							case PIN_PUT_THROUGH_SQUARE:
								switch (TempUnits)
								{
								case UNITS_INCH:
									sprintf(str2, "%.6f", ConvertToUnits(UNITS_INCH, AperTure->x2));
									sprintf(str3, "%.6f", ConvertToUnits(UNITS_INCH, AperTure->y2));
									break;

								case UNITS_MM:
									sprintf(str2, "%.4f", ConvertToUnits(UNITS_MM, AperTure->x2));
									sprintf(str3, "%.4f", ConvertToUnits(UNITS_MM, AperTure->y2));
									break;
								}

								Rotation = AperTure->Rotation;

								if (Rotation < 0)
									Rotation += 360.0;

								if (Rotation > 360.0)
									Rotation -= 360.0;

								if (AperTure->Mirror == 0)
									sprintf(str4, "%.2f", Rotation + 0.0005);
								else
								{
									sprintf(str4, "%.2f", Rotation + 0.0005);
									//                  sprintf(str4,"%.2f",180.0-AperTure->Rotation);
								}

								sprintf(str, "%%ADD%iBUTTERFLY,%sX%sX%s*%%", AperTure->AperTureCode, str2, str3, str4);
								break;
							}
						}

						break;
					}

					WriteGerberString(str, 1);
				}
			}

//        WriteGerberString("%LPD*%",1);
		}

// ****************************************************************************
// ****************************************************************************

// Write gerber data from objects4
// First the areafills/powerplanes/other polygons
		if ((Layer >= 0) && (Layer < 32))
		{
			for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

				if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
				{
					sprintf(InfoStr, SC(408, "Areafill(s) layer %i "), Layer);
					RedrawInfoStr(1);

					if (!IsLayerPowerPlane(Layer))
					{
						if ((!PenPlotMode) && ((GerberInfo.PlotMode & 1) == 0))
							res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 1);
						else
							res = PlotAreaFillToGerber(AreaFill, AreafillPenSize1, AreafillPenSize2, 0);

						if (res < 0)
						{
							sprintf(str, SC(409, "Error %i in plotting an areafill"), res);
							MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_OK);
						}
					}
				}
			}

// ****************************************************************************
// ****************************************************************************


			if (PowerPlaneLayer)
			{
				sprintf(InfoStr, SC(410, "PowerPlane layer %i "), Layer);
				RedrawInfoStr(1);

				if (((GerberInfo.PlotMode & 1) == 0) && (!PenPlotMode))
					PowerPlaneToGerber(Layer, PowerPlanePenSize2, PowerPlanePenSize1, 1);
				else
					PowerPlaneToGerber(Layer, PowerPlanePenSize2, PowerPlanePenSize1, 0);
			}
		}

// ****************************************************************************
// ****************************************************************************
// Plot addtional polygons

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

		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectPolygon->Layer == Layer2))
			{
				MemSizeAreaFill = MemSizeObjectPolygon(ObjectPolygon) + 8192;

				if (MemSizeAreaFill >= MaxAreaFillMemoryTemp)
				{
					if (AllocateMemAreaFillMemoryTemp(MemSizeAreaFill) != 0)
						return -105;

					NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
					TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
				}

				memset(NewAreaFill, 0, MemSizeAreaFill);
				Polygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
				Polygon->NrVertices = ObjectPolygon->NrVertices;
				memmove(&(*Polygon).Points, &(*ObjectPolygon).Points, ObjectPolygon->NrVertices * sizeof(PointRecord));
				NewAreaFill->NrPolygons = 1;
				SetMinMaxPolygon(Polygon, 0);
				NewAreaFill->minx = Polygon->minx;
				NewAreaFill->miny = Polygon->miny;
				NewAreaFill->maxx = Polygon->maxx;
				NewAreaFill->maxy = Polygon->maxy;

				if ((!PenPlotMode) && ((GerberInfo.PlotMode & 1) == 0))
					res = PlotAreaFillToGerber(NewAreaFill, AreafillPenSize1, AreafillPenSize2, 1);
				else
					res = PlotAreaFillToGerber(NewAreaFill, AreafillPenSize1, AreafillPenSize2, 0);

				if (res < 0)
				{
					sprintf(str, SC(411, "Error %i in plotting a polygon"), res);
					MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_OK);
				}
			}
		}

// ****************************************************************************
// ****************************************************************************

		sprintf(str, SC(412, "Plot layer %d"), Layer);
		AddPerformanceValue(str);

		if (PenPlotMode)
			TracePenPlotOutput(Layer, NrCheckObjects);

		if (PenPlotMode)
			ObjectsGerberOutput(Layer, NrTraceObjects);
		else
		{	// NrObjects4
			ObjectsGerberOutput(Layer, 0);
		}


// ****************************************************************************
// ****************************************************************************

		WriteGerberText(Layer, 0);

		if (!PenPlotMode)
		{
			if ((mode & 2) == 2)
			{
//          WriteGerberString("%LPC*%",1);
			}

			sprintf(str, "M02*");
			WriteGerberString(str, 1);
		}
		else
		{
			sprintf(str, "PU0,0;");
			WriteGerberString(str, 0);
		}

		if (LineBufGerber[0] != 0)
			WriteLn(Gerberfp, LineBufGerber);

		FileClose(Gerberfp);

		if (PenPlotMode)
		{
			if (PlotErrors > 0)
			{
				MessageBufPos = 0;

				if (PlotErrors > 0)
				{
					sprintf(str2, SC(413, "%i  objects"), PlotErrors);

					if (AddToMessageBuf(str2) != 0)
						return -1;
				}

				MessageDialog(SC(414, "Errors/warnings in the following objects, because the pen is to thick"), 0, 0);
				DeAllocateMemMessageBuf();
			}

			if (AddToMessageBuf(str2) != 0)
				return -1;
		}

		//********************************************** nelze vytvoøit soubor *********************************************************
		if (WriteLnError != 0)
		{
			sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		}
		//******************************************************************************************************************************
	}

	DeAllocateMemObjects4();
	DeAllocateMemObjects5();

	if ((!PenPlotMode) && ((mode & 2) == 0))
		WriteApertureFile(0);

	WritePerformanceStrings();
	ok = 1;
	DeAllocateMemAperTures();

	DeallocateSpecialMem(MEM_PLOT_LINES_BUF);
	DeallocateSpecialMem(MEM_PLOT_POINTS_MAP);
	DeallocateSpecialMem(MEM_AREAFILL1);
	DeallocateSpecialMem(MEM_POWERPLANE_AREAFILL);

	WriteLayerInfo();

	if (!PenPlotMode)
		sprintf(InfoStr, SC(421, "Gerber output ready"));
	else
		sprintf(InfoStr, SC(422, "Penplot output ready"));

//  PerformanceStrings
//  PerformanceValues
//  PerformanceCount
#ifdef _DEBUG
//  WritePerformanceStrings();
#endif
	RedrawInfoStr(1);

	SystemBusyMode = 0;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
