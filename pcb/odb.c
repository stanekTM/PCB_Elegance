/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: odb.c
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


#include "owntypes.h"
#include "types.h"
#include "stdio.h"
#include "memory.h"
#include "odb2.h"
#include "polygon.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "pcb.h"
#include "mainloop.h"
#include "owntime.h"
#include "calcdef.h"
#include "font.h"
#include "math.h"
#include "files2.h"
#include "tar.h"
#include "stddef.h"
#include "plot.h"
#include "utf8.h"
#include "resource.h"
#include "dialogs.h"
#include "ctype.h"
#include "../functionsc/version.h"

typedef struct
{
	int32 LayerNr;
	int32 LayerType;
	int32 LayerType2;
	int32 Layer1;
	int32 Layer2;
	int32 Inverted;
	char LayerName[128];
	char TypeName[128];
	char StartLayer[128];
	char EndLayer[128];
	char ContextName[128];
} OdbLayerStruct;

int32 ok, NrTempOdbLayers, TempOdbLayers[64], NrOdbLayers, OdbExportUnits, TempUnits, TempUnits2,
      OdbLayersForExport[64], OdbLayersForExport2[64], NrOdbExportLayers2, OdbConvertLayer[64], PowerPlaneLayers[64],
      NrOdbExportLayers, OdbLayerIndex[64], OdbPrepareErrors;
char OdbDir[MAX_LENGTH_STRING], OdbFile[MAX_LENGTH_STRING], MatrixFile[MAX_LENGTH_STRING];

OdbLayerStruct OdbLayers[64];

extern int32 NrAperTures, NrDrillAperTures, NrOdbApertureMacroObjects, MaxNrOdbApertureMacroObjects;
extern AperTureArray *AperTures, *DrillAperTures;
extern double DesignBoardOriginX, DesignBoardOriginY, DesignBoardWidth, DesignBoardHeight;

extern ApertureMacroObjectObjectRecord *ApertureMacroObjects;
extern HBITMAP BitMapLeft;
extern HBITMAP BitMapRight;

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 ObjectSortCompare(const char *arg1, const char *arg2)
{
	int32 StructOffset = offsetof(ObjectRecord, NetNr);

	if (*(int32 *) (arg1 + StructOffset) > *(int32 *) (arg2 + StructOffset))
		return 1;

	if (*(int32 *) (arg1 + StructOffset) < *(int32 *) (arg2 + StructOffset))
		return -1;

	return 0;
}



// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 FillListBoxOdbExport(HWND Dialog)
{
	int32 cnt, cnt2, res, Layer;
	char str[MAX_LENGTH_STRING];

	SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
	SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);

	for (cnt = 0; cnt < NrOdbExportLayers; cnt++)
	{
		if ((OdbLayersForExport[cnt] & 0x10000) == 0)
		{
			Layer = OdbLayersForExport[cnt] & 0xffff;
			str[0] = 0;
			GetLayerTextObjects(Layer, str, 0x10);
#ifdef _DEBUG

			if (str[0] == 0)
				ok = 1;

#endif
			cnt2 = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) str);

			if (cnt2 >= 0)
				res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_SETITEMDATA, cnt2, (LPARAM) Layer);

			ok = 1;
		}
	}

	for (cnt = 0; cnt < NrOdbExportLayers2; cnt++)
	{
		if (OdbLayersForExport2[cnt] != -1)
		{
			Layer = OdbLayersForExport2[cnt] & 0xffff;
			str[0] = 0;
			GetLayerTextObjects(Layer, str, 0x10);
#ifdef _DEBUG

			if (str[0] == 0)
				ok = 1;

			if (Layer == 0)
				ok = 1;

#endif
			cnt2 = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) str);

			if (cnt2 >= 0)
				SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_SETITEMDATA, cnt2, (LPARAM) Layer);
		}
	}

	return 0;
}

//***************************************************************************************************************
//***************************** Exportovat ODB++ ****************************************************************
//***************************************************************************************************************

int32 CALLBACK OdbExportDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about, cnt, cnt2, res, Layer, AllLayers[64], TabStops[10], Mirror, NrTopComps, NrBottomComps;
	char str[MAX_LENGTH_STRING], Extension[MAX_LENGTH_STRING];
	CompRecord *Comp;

	res = 0;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(282, "Export ODB++"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help")); //zatím nevyužito
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, SC(1307, "inch/mm")); //nevyužito

		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(303, "ODB++ layers for export"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(313, "Available ODB++ layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(367, "Filename"));
		
		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_DESELECTALL, SC(161, "Deselect all"));

		SendDlgItemMessageUTF8(Dialog, IDC_BUTTON1, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) BitMapRight);
		SendDlgItemMessageUTF8(Dialog, IDC_BUTTON2, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) BitMapLeft);

		TabStops[0] = 150;

		SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);
		OdbExportUnits = UNITS_INCH;
		TempUnits = OdbExportUnits;

		if (TempUnits == UNITS_INCH)
			TempUnits2 = UNITS_MILS;
		else
			TempUnits2 = TempUnits;
		
		SetUnitText(Dialog, IDC_EDIT10, TempUnits);
		
		strcpy(str, EditFile);
		CutExtensionFileName(str);
//		sprintf(OdbFile, "%s.tgz", str); //pùvodní název souboru, jen zobrazení
		sprintf(OdbFile, "%s\\pcb\\ODB++.tgz", DesignPath); //nový název souboru, jen zobrazení
		
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) OdbFile);

		NrTopComps = 0;
		NrBottomComps = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Mirror = ((Comp->CompMode & 8) >> 3);

				if (Mirror == 0)
					NrTopComps++;
				else
					NrBottomComps++;
			}
		}

		if (OdbLayersForExport[0] == 0)
		{
			for (cnt = 0; cnt < 64; cnt++)
			{
				AllLayers[cnt] = 0;
				OdbLayersForExport[cnt] = -1;
				OdbLayersForExport2[cnt] = -1;
				OdbLayerIndex[cnt] = -1;
			}

			cnt = 0;
			NrOdbExportLayers = 0;
			NrOdbExportLayers2 = 0;

			if (NrTopComps)
				AllLayers[cnt++] = TOP_COMP_LAYER;

			if (NrBottomComps)
				AllLayers[cnt++] = BOTTOM_COMP_LAYER;

			AllLayers[cnt++] = SILKSCREEN_TOP;
			AllLayers[cnt++] = PASTE_MASK_TOP;
			AllLayers[cnt++] = SOLD_MASK_TOP;

			//      AllLayers[cnt++]=ROUTING;
			for (cnt2 = 0; cnt2 < Design.NrBoardLayers; cnt2++)
				AllLayers[cnt++] = Design.NrBoardLayers - cnt2 - 1;

			AllLayers[cnt++] = DRILL_LAYER;
			AllLayers[cnt++] = SOLD_MASK_BOTTOM;
			AllLayers[cnt++] = PASTE_MASK_BOTTOM;

			if (NrBottomComps)
				AllLayers[cnt++] = SILKSCREEN_BOTTOM;

			for (cnt2 = 0; cnt2 < cnt; cnt2++)
				OdbLayersForExport2[NrOdbExportLayers2++] = AllLayers[cnt2];

			AllLayers[cnt++] = INFO_LAYER;
			AllLayers[cnt++] = INFO_LAYER2;
			AllLayers[cnt++] = INFO_LAYER3;
			AllLayers[cnt++] = INFO_LAYER4;
			AllLayers[cnt++] = ROUTING_KEEPOUT_BOTTOM;
			AllLayers[cnt++] = ROUTING_KEEPOUT_TOP;
			AllLayers[cnt++] = PLACEMENT_OUTLINE_TOP;
			AllLayers[cnt++] = PLACEMENT_OUTLINE_BOTTOM;
			AllLayers[cnt++] = COMP_OUTLINE_LAYER_TOP;
			AllLayers[cnt++] = COMP_OUTLINE_LAYER_BOTTOM;
			AllLayers[cnt++] = SILKSCREEN_TOP_REFS;
			AllLayers[cnt++] = SILKSCREEN_TOP_VALUES;
			AllLayers[cnt++] = SILKSCREEN_BOTTOM_REFS;
			AllLayers[cnt++] = SILKSCREEN_BOTTOM_VALUES;

			for (cnt2 = 0; cnt2 < cnt; cnt2++)
			{
				if (cnt2 < NrOdbExportLayers2)
					OdbLayersForExport[NrOdbExportLayers++] = AllLayers[cnt2] | 0x10000;
				else
					OdbLayersForExport[NrOdbExportLayers++] = AllLayers[cnt2];
			}
		}

		FillListBoxOdbExport(Dialog);
		ok = 1;
// ************************************************************************************************
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, 200, (LPARAM) str);

			if (res == 0)
			{
				MessageBoxOwn(PCBWindow, SC(296, "Missing odb export filename"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				break;
			}

			GetExtensionFileName(Extension, str);

			if (Extension[0] != 0)
			{
				if (stricmp(Extension, "gz") == 0)
				{
					CutExtensionFileName(str);
					GetExtensionFileName(Extension, str);

					if (stricmp(Extension, "tar") == 0)
						CutExtensionFileName(str);
				}
				else
				{
					if (stricmp(Extension, "tgz") == 0)
						CutExtensionFileName(str);
				}
			}

			OdbExportUnits = TempUnits;
			EndDialog(Dialog, 1);
			return about;

		case IDC_BUTTON1:		// Add layer
			res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);

			if (res >= 0)
			{
				Layer = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETITEMDATA, res, 0);

				if (Layer)
				{
					for (cnt = 0; cnt < NrOdbExportLayers; cnt++)
					{
						if (OdbLayersForExport[cnt] == Layer)
							break;
					}

					if (cnt < NrOdbExportLayers)
					{
						OdbLayersForExport[cnt] |= 0x10000;
						OdbLayersForExport2[NrOdbExportLayers2++] = Layer;
						FillListBoxOdbExport(Dialog);
					}
				}

				ok = 1;
			}

			break;

		case IDC_BUTTON2:		// remove layer
			res = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);

			if (res >= 0)
			{
				Layer = SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_GETITEMDATA, res, 0);

				if (Layer)
				{
					for (cnt = 0; cnt < NrOdbExportLayers2; cnt++)
					{
						if (OdbLayersForExport2[cnt] == Layer)
							break;
					}

					if (cnt < NrOdbExportLayers2)
					{
						memmove(&OdbLayersForExport2[cnt], &OdbLayersForExport2[cnt + 1],
						        (NrOdbExportLayers2 - cnt + 2) * sizeof(int32));
						NrOdbExportLayers2--;

						for (cnt = 0; cnt < NrOdbExportLayers; cnt++)
						{
							if ((OdbLayersForExport[cnt] & 0xffff) == Layer)
								break;
						}

						if (cnt < NrOdbExportLayers)
							OdbLayersForExport[cnt] = Layer;

						FillListBoxOdbExport(Dialog);
					}
				}

				ok = 1;
			}

			break;

		case IDD_UNITS:
//          value1=GetDialogValue(Dialog,IDC_EDIT1,TempUnits);
			TempUnits = SetNextUnits(TempUnits, 6);	// inch/mm

			if (TempUnits == UNITS_INCH)
				TempUnits2 = UNITS_MILS;
			else
				TempUnits2 = TempUnits;

			SetUnitText(Dialog, IDC_EDIT10, TempUnits);
//          SetDialogValue(Dialog,IDC_EDIT1,value1,TempUnits);
			break;

		case IDD_SELECTALL:
			NrOdbExportLayers2 = NrOdbExportLayers;

			for (cnt = 0; cnt < NrOdbExportLayers; cnt++)
			{
				OdbLayersForExport[cnt] |= 0x10000;
				OdbLayersForExport2[cnt] = OdbLayersForExport[cnt] & 0xffff;
			}

			FillListBoxOdbExport(Dialog);
			break;

		case IDD_DESELECTALL:
			NrOdbExportLayers2 = 0;

			for (cnt = 0; cnt < NrOdbExportLayers; cnt++)
				OdbLayersForExport[cnt] &= 0xffff;

			FillListBoxOdbExport(Dialog);
			break;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
 //           Help("exportDXF.htm", 0); //zatím nevytvoøeno
			return about;
        }

		break;
	}

	about = 0;
	return about;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ExportToOdb(int32 mode)
{
	int32 Layer, cnt, cnt2, cnt3, cnt4, cnt5, cnt6, cnt7, NrCheckObjects, NrTraceObjects, PowerPlaneLayer, count,
	      count2, code, lengte, FontNr, NrLines, MaxCountX, Mirror, Length, TempUnits, ApertureNr, RotationNr, PadMode,
	      OdbLayer, NrDocLayers, NrTopComps, NrBottomComps, PreviousCnt, MemSize, DrawChar, res, CompOutlineLayers,
	      PlotErrors, PreviousLayer, OdbPowerPlaneLayer, ObjectNr, Layer2, PreviousObject, StartCnt, PowerPlaneNr,
	      MountType, MemPos, ShapeNr, ShapesUsed[4096], PinsUsed[4096], NrCompProperties, SegmentCount, LineSegments;
	AperTureRecord *AperTure;
	GeomPolygonRecord *GeomPolygon;
	AreaFillRecord *AreaFill, *BoardOutlineAreaFill, *PowerAreaFill;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectRecord *Object, *Object2, *Object6, *Object5;
	PolygonRecord *DrawPolygon, *PolygonObject;
	char str[1024], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING], strx1[MAX_LENGTH_STRING],
	     stry1[MAX_LENGTH_STRING], strx2[MAX_LENGTH_STRING], stry2[MAX_LENGTH_STRING], strx3[MAX_LENGTH_STRING],
	     stry3[MAX_LENGTH_STRING], PolarityCh, Filename[MAX_LENGTH_STRING], JobName[MAX_LENGTH_STRING], PadPolarity,
	     InfoStrCopy[MAX_LENGTH_STRING], PropertyID[MAX_LENGTH_STRING], PropertyValue[MAX_LENGTH_STRING];
	WCHAR OdbDirW[MAX_LENGTH_STRING];
	double LineBuf[2048], incX, incY, x1, y1, x2, y2, x3, y3, x4, y4, OriginX, OriginY, MinX, MaxX, MinY, MaxY, xx1,
	       yy1, xx2, yy2, MinPitch, x1a, y1a, x2a, y2a, RotationAngle;
	ShapeRecord *Shape;
	uint8 PolygonBuf2[10240], *PolygonPos;
	WCHAR *str2;
	LPSTR TextP;
	NetRecord *Net;
	CompRecord *Comp, NewComp;
	SYSTEMTIME CurrentDate;

	strcpy(InfoStrCopy, InfoStr);
	memset(&NewComp, 0, sizeof(NewComp));
	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	PolarityCh = 'P';
	RotationNr = 0;
	PadMode = 0;
	TempUnits = UNITS_INCH;

	for (cnt = 0; cnt < 33; cnt++)
	{
		NrLayerPlotObjects[cnt] = 0;
		OdbConvertLayer[cnt] = -1;
		PowerPlaneLayers[cnt] = 0;
	}

	NrObjects5 = 0;
	res = FindMinMaxBoard(&DesignBoardOriginX, &DesignBoardOriginY, &DesignBoardWidth, &DesignBoardHeight, 3);
	DesignBoardWidth -= DesignBoardOriginX;
	DesignBoardHeight -= DesignBoardOriginY;

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_ODB_EXPORT), PCBWindow,
	                 (DLGPROC) OdbExportDialogBody);

	if (res == 2)
		return -1;

	NrTopComps = 0;
	NrBottomComps = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Mirror = ((Comp->CompMode & 8) >> 3);

			if (Mirror == 0)
				NrTopComps++;
			else
				NrBottomComps++;
		}
	}

	/*
	  strcpy(OdbDir,"e:\\pcb35\\odbtemp");
	*/
	GetFilePartFromFileName(str4, EditFile);
	CutExtensionFileName(str4);
	strcpy(JobName, str4);
	strlwrUTF8(JobName);
	GetDirFromFileName(OdbDir, EditFile);
	strcat(OdbDir, "\\");
	strcat(OdbDir, JobName);
	Utf8ToUnicode(OdbDir, OdbDirW, MAX_LENGTH_STRING - 100);
	DeleteDirectoryUnicode(OdbDirW);
	CreateDirectoryUTF8(OdbDir);
	NrDocLayers = 0;
	NrOdbLayers = 0;
	NrOdbApertureMacroObjects = 0;
	MaxNrOdbApertureMacroObjects = 0;

	for (cnt = 0; cnt < NrOdbExportLayers2; cnt++)
		OdbLayers[NrOdbLayers++].LayerNr = OdbLayersForExport2[cnt];

	CompOutlineLayers = 0;

	for (cnt3 = 0; cnt3 < NrOdbLayers; cnt3++)
	{
		Layer = OdbLayers[cnt3].LayerNr;

		switch (Layer)
		{
		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			CompOutlineLayers++;
			break;
		}

		PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
		OdbLayers[cnt3].Inverted = 0;

		if (PowerPlaneLayer)
		{
			OdbLayers[cnt3].Inverted = 1;
			PowerPlaneLayers[Layer] = 1;
		}

		switch (Layer)
		{
		case TOP_COMP_LAYER:
			/*
			    ROW=1
			    CONTEXT=BOARD
			    TYPE=COMPONENT
			    NAME=COMP_+_TOP
			    POLARITY=POSITIVE
			    START_NAME=
			    END_NAME=
			    OLD_NAME=
			*/
			strcpy(OdbLayers[cnt3].LayerName, "COMP_+_TOP");
			strcpy(OdbLayers[cnt3].TypeName, "COMPONENT");
			break;

		case BOTTOM_COMP_LAYER:
			/*
			    ROW=13
			    CONTEXT=BOARD
			    TYPE=COMPONENT
			    NAME=COMP_+_BOT
			    POLARITY=POSITIVE
			    START_NAME=
			    END_NAME=
			    OLD_NAME=
			*/
			strcpy(OdbLayers[cnt3].LayerName, "COMP_+_BOT");
			strcpy(OdbLayers[cnt3].TypeName, "COMPONENT");
			break;

		case SOLD_MASK_BOTTOM:
			strcpy(OdbLayers[cnt3].LayerName, "SMB");
			strcpy(OdbLayers[cnt3].TypeName, "SOLDER_MASK");
			break;

		case SOLD_MASK_TOP:
			strcpy(OdbLayers[cnt3].LayerName, "SMT");
			strcpy(OdbLayers[cnt3].TypeName, "SOLDER_MASK");
			break;

		case PASTE_MASK_BOTTOM:
			strcpy(OdbLayers[cnt3].LayerName, "SPB");
			strcpy(OdbLayers[cnt3].TypeName, "SOLDER_PASTE");
			break;

		case PASTE_MASK_TOP:
			strcpy(OdbLayers[cnt3].LayerName, "SPT");
			strcpy(OdbLayers[cnt3].TypeName, "SOLDER_PASTE");
			break;

		case INFO_LAYER:
			sprintf(OdbLayers[cnt3].LayerName, "INFO1");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case INFO_LAYER2:
			sprintf(OdbLayers[cnt3].LayerName, "INFO2");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case INFO_LAYER3:
			sprintf(OdbLayers[cnt3].LayerName, "INFO3");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case INFO_LAYER4:
			sprintf(OdbLayers[cnt3].LayerName, "INFO4");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case COMP_OUTLINE_LAYER:
			sprintf(OdbLayers[cnt3].LayerName, "CMPT");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case COMP_OUTLINE_LAYER + 1:
			sprintf(OdbLayers[cnt3].LayerName, "CMPB");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case ROUTING_KEEPOUT_TOP:
			sprintf(OdbLayers[cnt3].LayerName, "RKOT");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case ROUTING_KEEPOUT_BOTTOM:
			sprintf(OdbLayers[cnt3].LayerName, "RKOB");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case PLACEMENT_OUTLINE_TOP:
			sprintf(OdbLayers[cnt3].LayerName, "PLOT");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case PLACEMENT_OUTLINE_BOTTOM:
			sprintf(OdbLayers[cnt3].LayerName, "PLOB");
			strcpy(OdbLayers[cnt3].TypeName, "DOCUMENT");
			break;

		case SILKSCREEN_BOTTOM:
			strcpy(OdbLayers[cnt3].LayerName, "SSB");
			strcpy(OdbLayers[cnt3].TypeName, "SILK_SCREEN");
			break;

		case SILKSCREEN_TOP:
			strcpy(OdbLayers[cnt3].LayerName, "SST");
			strcpy(OdbLayers[cnt3].TypeName, "SILK_SCREEN");
			break;

		case SILKSCREEN_BOTTOM_REFS:
			strcpy(OdbLayers[cnt3].LayerName, "SSBR");
			strcpy(OdbLayers[cnt3].TypeName, "SILK_SCREEN");
			break;

		case SILKSCREEN_TOP_REFS:
			strcpy(OdbLayers[cnt3].LayerName, "SSTR");
			strcpy(OdbLayers[cnt3].TypeName, "SILK_SCREEN");
			break;

		case SILKSCREEN_BOTTOM_VALUES:
			strcpy(OdbLayers[cnt3].LayerName, "SSBV");
			strcpy(OdbLayers[cnt3].TypeName, "SILK_SCREEN");
			break;

		case SILKSCREEN_TOP_VALUES:
			strcpy(OdbLayers[cnt3].LayerName, "SSTV");
			strcpy(OdbLayers[cnt3].TypeName, "SILK_SCREEN");
			break;

		case DRILL_LAYER:
			strcpy(OdbLayers[cnt3].LayerName, "DRL");
			strcpy(OdbLayers[cnt3].TypeName, "DRILL");
			break;

		default:
			if (Layer < 32)
			{
				if (PowerPlaneLayer)
					strcpy(OdbLayers[cnt3].TypeName, "POWER_GROUND");
				else
					strcpy(OdbLayers[cnt3].TypeName, "SIGNAL");

				if (Layer == 0)
					sprintf(OdbLayers[cnt3].LayerName, "BOTTOM");
				else
				{
					if (Layer == Design.NrBoardLayers - 1)
						sprintf(OdbLayers[cnt3].LayerName, "TOP");
					else
					{
						if (PowerPlaneLayer)
							sprintf(OdbLayers[cnt3].LayerName, "POW%02d", Design.NrBoardLayers - Layer);
						else
							sprintf(OdbLayers[cnt3].LayerName, "SIG%02d", Design.NrBoardLayers - Layer);
					}
				}

				OdbLayers[cnt3].Inverted = 0;

				if (PowerPlaneLayer)
					OdbLayers[cnt3].Inverted = 1;
			}

			break;
		}
	}

	sprintf(str, "%s\\matrix", OdbDir);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\fonts", OdbDir);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\steps", OdbDir);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\misc", OdbDir);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\symbols", OdbDir);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\steps\\%s", OdbDir, JobName);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\steps\\%s\\layers", OdbDir, JobName);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\steps\\%s\\eda", OdbDir, JobName);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\steps\\%s\\netlists", OdbDir, JobName);
	CreateDirectoryUTF8(str);
	sprintf(str, "%s\\steps\\%s\\netlists\\cadnet", OdbDir, JobName);
	CreateDirectoryUTF8(str);

	sprintf(MatrixFile, "%s\\matrix\\matrix", OdbDir);
	DeleteFileUTF8(MatrixFile);
	/*

	STEP {
	    COL=1
	    NAME=PCB
	}

	*/
	AppendStringToTextFileUTF8(MatrixFile, "");
	AppendStringToTextFileUTF8(MatrixFile, "STEP {");
	AppendStringToTextFileUTF8(MatrixFile, "    COL=1");
	GetFilePartFromFileName(str, EditFile);
	CutExtensionFileName(str);
//  sprintf(str3,"    NAME=%s",str);
	strcpy(str4, JobName);
	struprUTF8(str4);
	sprintf(str3, "    NAME=%s", str4);
	AppendStringToTextFileUTF8(MatrixFile, str3);
	AppendStringToTextFileUTF8(MatrixFile, "}");

	for (OdbLayer = 0; OdbLayer < NrOdbLayers; OdbLayer++)
	{
		Layer = OdbLayers[OdbLayer].LayerNr;
		/*

		LAYER {
		    ROW=1
		    CONTEXT=BOARD
		    TYPE=COMPONENT
		    NAME=COMP_+_TOP
		    POLARITY=POSITIVE
		    START_NAME=
		    END_NAME=
		    OLD_NAME=
		}
		*/
		strlwrUTF8(OdbLayers[OdbLayer].LayerName);
		AppendStringToTextFileUTF8(MatrixFile, "");
		AppendStringToTextFileUTF8(MatrixFile, "LAYER {");
		sprintf(str, "    ROW=%d", OdbLayer + 1);
		AppendStringToTextFileUTF8(MatrixFile, str);

		switch (Layer)
		{
//      case COPPER_LAYER:
//      case DRILL_LAYER:
//      case ROUTING_LAYER:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
		case SOLD_MASK_TOP:
		case SOLD_MASK_BOTTOM:
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM:
		case SILKSCREEN_BOTTOM_REFS:
		case SILKSCREEN_TOP_REFS:
		case SILKSCREEN_BOTTOM_VALUES:
		case SILKSCREEN_TOP_VALUES:
			AppendStringToTextFileUTF8(MatrixFile, "    CONTEXT=BOARD");
			break;

		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
		case COMP_OUTLINE_LAYER_TOP:
		case COMP_OUTLINE_LAYER_BOTTOM:
		case PLACEMENT_OUTLINE_TOP:
		case PLACEMENT_OUTLINE_BOTTOM:
		case ROUTING_KEEPOUT_BOTTOM:
		case ROUTING_KEEPOUT_TOP:
//      case ROUTING_KEEPOUT_INNER:
			AppendStringToTextFileUTF8(MatrixFile, "    CONTEXT=MISC");
			break;

		case DRILL_LAYER:
		case TOP_COMP_LAYER:
		case BOTTOM_COMP_LAYER:
			AppendStringToTextFileUTF8(MatrixFile, "    CONTEXT=BOARD");
			break;

		default:
			if (Layer < 32)
				AppendStringToTextFileUTF8(MatrixFile, "    CONTEXT=BOARD");

			break;
		}

		sprintf(str, "    TYPE=%s", OdbLayers[OdbLayer].TypeName);
		AppendStringToTextFileUTF8(MatrixFile, str);
		sprintf(str, "%s\\steps\\%s\\layers\\%s", OdbDir, JobName, OdbLayers[OdbLayer].LayerName);
		CreateDirectoryUTF8(str);
		strcpy(str4, OdbLayers[OdbLayer].LayerName);
		struprUTF8(str4);
		sprintf(str, "    NAME=%s", str4);
		AppendStringToTextFileUTF8(MatrixFile, str);

		if (OdbLayers[OdbLayer].Inverted == 0)
			sprintf(str, "    POLARITY=POSITIVE");
		else
			sprintf(str, "    POLARITY=NEGATIVE");

		AppendStringToTextFileUTF8(MatrixFile, str);
		sprintf(str, "    START_NAME=%s", OdbLayers[OdbLayer].StartLayer);
		AppendStringToTextFileUTF8(MatrixFile, str);
		sprintf(str, "    END_NAME=%s", OdbLayers[OdbLayer].EndLayer);
		AppendStringToTextFileUTF8(MatrixFile, str);
		sprintf(str, "    OLD_NAME=");
		AppendStringToTextFileUTF8(MatrixFile, str);
		AppendStringToTextFileUTF8(MatrixFile, "}");
	}

// *******************************************************************************************************
// *******************************************************************************************************

	sprintf(str, "%s\\fonts\\standard", OdbDir);
	DeleteFileUTF8(str);
	AppendStringToTextFileUTF8(str, "XSIZE 0.0000000");
	AppendStringToTextFileUTF8(str, "YSIZE 0.0000000");
	AppendStringToTextFileUTF8(str, "OFFSET 0.0000000");

	sprintf(str, "%s\\misc\\attrlist", OdbDir);
	DeleteFileUTF8(str);
	AppendStringToTextFileUTF8(str, ".customer=");
	AppendStringToTextFileUTF8(str, ".comment=Version 2005");
	AppendStringToTextFileUTF8(str, ".technology=");
	AppendStringToTextFileUTF8(str, ".global_camtek_aoiset=");
	AppendStringToTextFileUTF8(str, ".drc_route_keepin_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_comp_keepin_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_tp_keepin_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_route_keepout_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_via_keepout_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_trace_keepout_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_plane_keepout_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_pad_keepout_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_comp_keepout_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_comp_height_lyr=");
	AppendStringToTextFileUTF8(str, ".drc_tp_keepout_lyr=");
	AppendStringToTextFileUTF8(str, ".variant_list=");
	AppendStringToTextFileUTF8(str, ".primary_side=Top");
	AppendStringToTextFileUTF8(str, ".design_origin_x=0");
	AppendStringToTextFileUTF8(str, ".design_origin_y=0");
	AppendStringToTextFileUTF8(str, ".board_thickness=0");

	sprintf(str, "%s\\steps\\%s\\stephdr", OdbDir, JobName);
	DeleteFileUTF8(str);
	AppendStringToTextFileUTF8(str, "X_DATUM=0");
	AppendStringToTextFileUTF8(str, "Y_DATUM=0");
	AppendStringToTextFileUTF8(str, "X_ORIGIN=0");
	AppendStringToTextFileUTF8(str, "Y_ORIGIN=0");
	AppendStringToTextFileUTF8(str, "");
	AppendStringToTextFileUTF8(str, "TOP_ACTIVE=0");
	AppendStringToTextFileUTF8(str, "BOTTOM_ACTIVE=0");
	AppendStringToTextFileUTF8(str, "RIGHT_ACTIVE=0");
	AppendStringToTextFileUTF8(str, "LEFT_ACTIVE=0");
	AppendStringToTextFileUTF8(str, "ONLINE_DRC_NAME=");
	AppendStringToTextFileUTF8(str, "ONLINE_DRC_MODE=DISABLED");
	AppendStringToTextFileUTF8(str, "ONLINE_DRC_STAT=RED");
	AppendStringToTextFileUTF8(str, "ONLINE_DRC_TIME=0");
	AppendStringToTextFileUTF8(str, "ONLINE_DRC_BEEP_VOL=2");
	AppendStringToTextFileUTF8(str, "ONLINE_DRC_BEEP_TONE=500");
	AppendStringToTextFileUTF8(str, "ONLINE_NET_MODE=DISABLED");
	AppendStringToTextFileUTF8(str, "ONLINE_NET_STAT=RED");
	AppendStringToTextFileUTF8(str, "ONLINE_NET_TIME=0");
	AppendStringToTextFileUTF8(str, "ONLINE_NET_BEEP_VOL=2");
	AppendStringToTextFileUTF8(str, "ONLINE_NET_BEEP_TONE=1000");
	AppendStringToTextFileUTF8(str, "AFFECTING_BOM=");
	AppendStringToTextFileUTF8(str, "AFFECTING_BOM_CHANGED=0");

// *******************************************************************************************************
// *******************************************************************************************************

	SetWaitCursor();

	for (cnt3 = 0; cnt3 < NrOdbLayers; cnt3++)
	{
		Layer = OdbLayers[cnt3].LayerNr;
#ifdef _DEBUG

		if (Layer == INFO_LAYER4)
			ok = 1;

		if (Layer == 1)
			ok = 1;

#endif
		PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
		MessageBufPos = 0;
		GetLayerTextObjects(Layer, str, 0);
		str[0] = (char) tolower(str[0]);
		sprintf(InfoStr, "Plot %s", str);
		RedrawInfoStr(1);
		CheckInputMessages(0);

		PlotErrors = 0;

		sprintf(str, SC(407, "Get info layer %d"), Layer);
		AddPerformanceValue(str);

// ****************************************************************************
// ****************************************************************************

// Collect all layer objects to Objects5 and fill in the aperture table
		NrObjects4 = 0;
		NrTraceObjects = 0;
		NrCheckObjects = 0;
		NrAperTures = 0;
		NrDrillAperTures = 0;

		if (Layer < 32)
		{
#ifdef _DEBUG

			if (Layer == INFO_LAYER4)
				ok = 1;

			if (Layer == 0)
				ok = 1;

#endif


			if (PowerPlaneLayer)
			{
				if ((Object5 = GetNewObject5(Layer)) == NULL)
					return -1;

				PowerPlaneNr = GetPowerPlaneByLayer(Layer);

				if (PowerPlaneNr == -1)
					return -1;

				PowerAreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[PowerPlaneNr]]);
				Object5->ObjectType = AREAFILL2;
				Object5->TraceNr = PowerPlaneNr;
				Object5->NetNr = PowerAreaFill->NetNr;
//        PowerPlaneToOdb(Layer);
			}
			else
			{
				for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
				{
					AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

					if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
					{
						if (!IsLayerPowerPlane(Layer))
						{
//              res=PlotAreaFillToOdb(AreaFill);
							if ((Object5 = GetNewObject5(Layer)) == NULL)
								return -1;

							Object5->ObjectType = AREAFILL;
							Object5->TraceNr = cnt;
							Object5->NetNr = AreaFill->NetNr;
//              FillPositionObject(Object5);
						}
					}
				}
			}

			NrCheckObjects = OdbPrepareComponentPins(Layer, 0);
			NrTraceObjects = OdbPrepareTraces(Layer, 0);
			NrCheckObjects += NrTraceObjects;
			NrCheckObjects += OdbPrepareVias(Layer, 0);
		}

#ifdef _DEBUG

		if (Layer == INFO_LAYER4)
			ok = 1;

		if (Layer == 0)
			ok = 1;

#endif

		switch (Layer)
		{
		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
			OdbPrepareComponentPinsViasSolderPasteObjects(Layer, 0);
			break;

		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
			OdbPrepareComponentInfoObjects(Layer, 0);
			break;

		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			OdbPrepareComponentOutlineObjects(Layer, CompOutlineLayers, 0);
			break;

		case PLACEMENT_OUTLINE_TOP:
		case PLACEMENT_OUTLINE_BOTTOM:
			OdbPrepareComponentPlacementOutlineObjects(Layer, 0);
			break;

		case ROUTING_KEEPOUT_BOTTOM:
			OdbPrepareComponentRoutingKeepoutObjects(ROUTING_KEEPOUT_LAYER, 0);
			break;

		case ROUTING_KEEPOUT_TOP:
//      case ROUTING_KEEPOUT_INNER:
			OdbPrepareComponentRoutingKeepoutObjects(ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1, 0);
			break;

		case SILKSCREEN_BOTTOM:
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM_REFS:
		case SILKSCREEN_TOP_REFS:
		case SILKSCREEN_BOTTOM_VALUES:
		case SILKSCREEN_TOP_VALUES:
			OdbPrepareComponentSilkScreenObjects(Layer, 0);
			break;

		case DRILL_LAYER:
			NrCheckObjects = OdbPrepareComponentPins(32, 1);
			NrCheckObjects += OdbPrepareVias(32, 1);
			break;
		}

		OdbPrepareSpecialObjects(Layer, 0);

		for (cnt = 0; cnt < NrAperTures; cnt++)
		{
			AperTure = &((*AperTures)[cnt]);
			ApertureNr = AperTure->AperTureNr;

			switch (AperTure->Info)
			{
			case ODB_TRACE:
				break;

			case ODB_PAD_ROUND:
				GetUnitsValue(UNITS_MILS, AperTure->x, str, 0);
				sprintf(str3, "$%d r%s", ApertureNr, str);
				AddToMessageBuf(str3);
				break;

			case ODB_PAD_RECT:
				GetUnitsValue(UNITS_MILS, AperTure->x, str, 0);
				GetUnitsValue(UNITS_MILS, AperTure->y, str3, 0);
				sprintf(str4, "$%d rect%sx%s", ApertureNr, str, str3);
				AddToMessageBuf(str4);
				break;

			case ODB_PAD_BUTTERFLY:
				break;

			case ODB_PAD_THERMAL_RELIEF:
				GetUnitsValue(UNITS_MILS, AperTure->x, strx1, 0);
				GetUnitsValue(UNITS_MILS, AperTure->y, stry1, 0);
				GetUnitsValue(UNITS_MILS, AperTure->x2, strx2, 0);
				sprintf(str, "$%d thr%sx%sx0x4x%s", ApertureNr, stry1, strx1, strx2);
				AddToMessageBuf(str);
				break;

			case ODB_DRILL:
				GetUnitsValue(UNITS_MILS, AperTure->x, str, 0);
				sprintf(str3, "$%d r%s", ApertureNr, str);
				AddToMessageBuf(str3);
				break;

			case ODB_PAD_POLYGON:
				cnt2 = AperTure->Info3;

				if ((NrOdbApertureMacroObjects > 0) && (cnt2 < NrOdbApertureMacroObjects))
					sprintf(str3, "$%d %s", ApertureNr, ApertureMacroObjects[cnt2].Name);
				else
					sprintf(str3, "$%d r2.0", ApertureNr);

				AddToMessageBuf(str3);
				break;
			}
		}


		if ((Layer >= 0) && (Layer < 32))
		{
			AddToMessageBuf("#");
			AddToMessageBuf("#Feature attribute names");
			AddToMessageBuf("#");
			AddToMessageBuf("@0 .smd");
			AddToMessageBuf("@1 .nomenclature");
			AddToMessageBuf("@2 .test_point");
			AddToMessageBuf("@3 .geometry");
			AddToMessageBuf("@4 .pad_usage");
			/*
			0 - toeprint        = pin contact area
			1 - via
			2 - g_fiducial
			3 - l_fiducial
			4 - tooling_hole
			*/
		}

		if (Layer == DRILL_LAYER)
		{
			AddToMessageBuf("#");
			AddToMessageBuf("#Feature attribute names");
			AddToMessageBuf("#");
			AddToMessageBuf("@0 .geometry");
			AddToMessageBuf("@1 .drill");
			AddToMessageBuf("@2 .pad_usage");
			/*
			0 - toeprint        = pin contact area
			1 - via
			2 - g_fiducial
			3 - l_fiducial
			4 - tooling_hole
			*/
		}

		AddToMessageBuf("#");
		AddToMessageBuf("#Layer features");
		AddToMessageBuf("#");

#ifdef _DEBUG

		if (Layer == 0)
			ok = 1;

#endif
		Object = NULL;
		StartCnt = 0;

		if ((Layer >= 0) && (Layer < 32))
		{
			for (cnt2 = 31; cnt2 > Layer; cnt2--)
				StartCnt += NrLayerPlotObjects[cnt2];

			count = NrLayerPlotObjects[Layer];
			Object = &((*Objects5)[StartCnt]);
		}
		else
		{
			if (Layer == DRILL_LAYER)
			{
				for (cnt2 = 0; cnt2 < 32; cnt2++)
					StartCnt += NrLayerPlotObjects[cnt2];

				count = NrLayerPlotObjects[32];
				Object = &((*Objects5)[StartCnt]);
			}
			else
			{
				count = NrObjects4;
				Object = &((*Objects4)[0]);
			}
		}

		cnt = 0;
		cnt2 = 0;

		while (1)
		{
#ifdef _DEBUG

			if (Layer == 2)
			{
				ok = 1;

				if (cnt == 1826)
					ok = 1;
			}

			if (cnt > count)
				ok = 1;

#endif
			PreviousCnt = cnt;

			if (cnt >= count)
				break;

			ApertureNr = Object->Info2;
#ifdef _DEBUG

			if (ApertureNr >= NrAperTures)
			{
				ok = 1;
				cnt++;
				Object++;
				continue;
			}

			if ((cnt2 % 10) == 0)
			{
				sprintf(str, "# object nr %d", cnt2 + 1);
				AddToMessageBuf(str);
			}

			if (cnt2 == 57)
			{
				ok = 1;

				if (Layer == DRILL_LAYER)
					ok = 1;
			}

#endif
			OdbPowerPlaneLayer = 0;
			PadPolarity = 'P';

			if (PowerPlaneLayers[Layer])
				OdbPowerPlaneLayer = 1;

			x1 = Object->x1;
			y1 = Object->y1;
			x2 = Object->x2;
			y2 = Object->y2;
#ifdef _DEBUG

			if ((InRange9(Object->x1, 176.0e5)) && (InRange9(Object->y1, 70.6e5)))
			{
				ok = 1;

				if (OdbPowerPlaneLayer)
					ok = 1;
			}

#endif

			switch (Object->ObjectType)
			{
			case TRACE_HOR:
				/*
				L -2.4015748 0.9350394 -2.4015748 1.0531496 1 P 0 ;2=7,3=8
				L -2.4015748 1.0531496 -2.5590551 0.9940945 1 P 0 ;2=7,3=8
				*/
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2, strx2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry1, ApertureNr, PadPolarity);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case PIN_LINE_HOR:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2, strx2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0,4=1", strx1, stry1, strx2, stry1, ApertureNr, PadPolarity);

				if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
				{
					sprintf(str3, ";3=%d", Object->Info & 0xffff);
					strcat(str, str3);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case TRACE_VER:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, y1 + x2, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry1, ApertureNr, PadPolarity);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case PIN_LINE_VER:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, y1 + x2, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0,4=1", strx1, stry1, strx1, stry2, ApertureNr, PadPolarity);

				if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
				{
					sprintf(str3, ";3=%d", Object->Info & 0xffff);
					strcat(str, str3);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case TRACE_DIAG1:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2, strx2, 0);
				GetUnitsValue(TempUnits, y1 - x2, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case PIN_LINE_DIAG1:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2, strx2, 0);
				GetUnitsValue(TempUnits, y1 - x2, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0,4=1", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);

				if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
				{
					sprintf(str3, ";3=%d", Object->Info & 0xffff);
					strcat(str, str3);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case TRACE_DIAG2:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2, strx2, 0);
				GetUnitsValue(TempUnits, y1 + x2, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case PIN_LINE_DIAG2:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2, strx2, 0);
				GetUnitsValue(TempUnits, y1 + x2, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0,4=1", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);

				if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
				{
					sprintf(str3, ";3=%d", Object->Info & 0xffff);
					strcat(str, str3);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case OBJECT_LINE:
#ifdef _DEBUG
				if (Layer == 0)
					ok = 1;

#endif

				if (Object->Test != 0)
				{
					LineSegments = DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, Object->Test);
					SegmentCount = 0;

					for (cnt4 = 0; cnt4 < LineSegments; cnt4++)
					{
						x3 = LineBuf[SegmentCount++];
						y3 = LineBuf[SegmentCount++];
						x4 = LineBuf[SegmentCount++];
						y4 = LineBuf[SegmentCount++];
						GetUnitsValue(TempUnits, x3, strx1, 0);
						GetUnitsValue(TempUnits, y3, stry1, 0);
						GetUnitsValue(TempUnits, x4, strx2, 0);
						GetUnitsValue(TempUnits, y4, stry2, 0);
						sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);
						AddToMessageBuf(str);
						cnt2++;
					}
				}
				else
				{
					GetUnitsValue(TempUnits, x1, strx1, 0);
					GetUnitsValue(TempUnits, y1, stry1, 0);
					GetUnitsValue(TempUnits, x2, strx2, 0);
					GetUnitsValue(TempUnits, y2, stry2, 0);
					sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);
					AddToMessageBuf(str);
					cnt2++;
				}

				break;

			case TRACE_ALL_ANGLE:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);

				if (ApertureNr != -1)
				{
					GetUnitsValue(TempUnits, x2, strx2, 0);
					GetUnitsValue(TempUnits, y2, stry2, 0);
					sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);
					AddToMessageBuf(str);
					cnt2++;
				}

				break;

			case PIN_LINE_ALL_ANGLE:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);

				if (ApertureNr != -1)
				{
					GetUnitsValue(TempUnits, x2, strx2, 0);
					GetUnitsValue(TempUnits, y2, stry2, 0);
					sprintf(str, "L %s %s %s %s %d %c 0 0;0;4=1", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity);

					if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
					{
						sprintf(str3, ";3=%d", Object->Info & 0xffff);
						strcat(str, str3);
					}

					AddToMessageBuf(str);
					cnt2++;
				}

				break;

			case VIA_PUT_THROUGH_ROUND:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				sprintf(str, "P %s %s %d %c 0 0;4=1", strx1, stry1, ApertureNr, PadPolarity);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case PIN_PUT_THROUGH_ROUND_POWER:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				sprintf(str, "P %s %s %d %c 0 0", strx1, stry1, ApertureNr, PadPolarity);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case THERMAL_RELIEF_ROUND:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				sprintf(str, "P %s %s %d %c 0 0", strx1, stry1, ApertureNr, PadPolarity);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case FIDUCIAL:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				sprintf(str, "P %s %s %d %c 0 0;0;4=2", strx1, stry1, ApertureNr, PadPolarity);

				if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
				{
					sprintf(str3, ";3=%d", Object->Info & 0xffff);
					strcat(str, str3);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				sprintf(str, "P %s %s %d %c 0 0;4=0", strx1, stry1, ApertureNr, PadPolarity);

				if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
				{
					sprintf(str3, ";3=%d", Object->Info & 0xffff);
					strcat(str, str3);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);
				sprintf(str, "P %s %s %d %c 0 0;4=0", strx1, stry1, ApertureNr, PadPolarity);

				if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
				{
					sprintf(str3, ";3=%d", Object->Info & 0xffff);
					strcat(str, str3);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			/*
			@0 .geometry
			@1 .drill

			0=plated,1=non-plated,2=via

			@2 .pad_usage

			0 - toeprint        = pin contact area
			1 - via
			2 - g_fiducial
			3 - l_fiducial
			4 - tooling_hole

			*/
			case DRILL:

// P 7.65 7.015 0 P 0 0;0=0,1=0,2=0

				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);

				if ((Object->Info & 0xfff0) == 0xfff0)
				{
					if ((Object->Info & 0xffff) == 0xfffe)
					{
						// via
						sprintf(str, "P %s %s %d %c 0 0;1=2,2=1", strx1, stry1, ApertureNr, PadPolarity);
					}
					else
					{
						// extra drill hole
						sprintf(str, "P %s %s %d %c 0 0;1=2,2=4", strx1, stry1, ApertureNr, PadPolarity);
					}
				}
				else
				{
					// Component drill hole
					sprintf(str, "P %s %s %d %c 0 0;1=0,2=0", strx1, stry1, ApertureNr, PadPolarity);
//            sprintf(str,"P %s %s %d P 0 0;0=%d,1=0,2=0",strx1,stry1,ApertureNr,Object->Info & 0xffff);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case DRILL_UNPLATED:
				GetUnitsValue(TempUnits, x1, strx1, 0);
				GetUnitsValue(TempUnits, y1, stry1, 0);

				if ((Object->Info & 0xffff) == 0xffff)
				{
					// Extra unplated drill hole
					sprintf(str, "P %s %s %d %c 0 0;1=1", strx1, stry1, ApertureNr, PadPolarity);
				}
				else
				{
					// Component unplated drill hole
					sprintf(str, "P %s %s %d %c 0 0;1=1", strx1, stry1, ApertureNr, PadPolarity);
//            sprintf(str,"P %s %s %d P 0 0;0=%d,1=1",strx1,stry1,ApertureNr,Object->Info & 0xffff);
				}

				AddToMessageBuf(str);
				cnt2++;
				break;

			case OBJECT_RECT:
#ifdef _DEBUG
				if (Layer == SILKSCREEN_TOP)
					ok = 1;

#endif
				GetUnitsValue(TempUnits, x1 - x2 * 0.5, strx1, 0);
				GetUnitsValue(TempUnits, y1 - y2 * 0.5, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2 * 0.5, strx2, 0);
				GetUnitsValue(TempUnits, y1 - y2 * 0.5, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity,
				        PadMode);
				AddToMessageBuf(str);
				cnt2++;
				GetUnitsValue(TempUnits, x1 + x2 * 0.5, strx1, 0);
				GetUnitsValue(TempUnits, y1 - y2 * 0.5, stry1, 0);
				GetUnitsValue(TempUnits, x1 + x2 * 0.5, strx2, 0);
				GetUnitsValue(TempUnits, y1 + y2 * 0.5, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity,
				        PadMode);
				AddToMessageBuf(str);
				cnt2++;
				GetUnitsValue(TempUnits, x1 + x2 * 0.5, strx1, 0);
				GetUnitsValue(TempUnits, y1 + y2 * 0.5, stry1, 0);
				GetUnitsValue(TempUnits, x1 - x2 * 0.5, strx2, 0);
				GetUnitsValue(TempUnits, y1 + y2 * 0.5, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity,
				        PadMode);
				AddToMessageBuf(str);
				cnt2++;
				GetUnitsValue(TempUnits, x1 - x2 * 0.5, strx1, 0);
				GetUnitsValue(TempUnits, y1 + y2 * 0.5, stry1, 0);
				GetUnitsValue(TempUnits, x1 - x2 * 0.5, strx2, 0);
				GetUnitsValue(TempUnits, y1 - y2 * 0.5, stry2, 0);
				sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr, PadPolarity,
				        PadMode);
				AddToMessageBuf(str);
				cnt2++;
				break;

			case OBJECT_CIRCLE:
				if ((Object->Info & OBJECT_FILLED) == 0)
				{
				}
				else
				{
				}

				break;

			case PIN_ARC:
				if ((Object->Info & OBJECT_FILLED) == 0)
				{
#ifdef _DEBUG

					if (Layer == 1)
						ok = 1;

#endif
					GetArcEndPoints(Object, &x1a, &y1a, &x2a, &y2a, 0);
					GetUnitsValue(TempUnits, x1, strx1, 0);
					GetUnitsValue(TempUnits, y1, stry1, 0);
					GetUnitsValue(TempUnits, x1a, strx2, 0);
					GetUnitsValue(TempUnits, y1a, stry2, 0);
					GetUnitsValue(TempUnits, x2a, strx3, 0);
					GetUnitsValue(TempUnits, y2a, stry3, 0);
					sprintf(str, "A %s %s %s %s %s %s %d %c 0 N;4=1", strx2, stry2, strx3, stry3, strx1, stry1,
					        ApertureNr, PadPolarity);

					if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
					{
						sprintf(str3, ";3=%d", Object->Info & 0xffff);
						strcat(str, str3);
					}

					AddToMessageBuf(str);
				}
				else
				{
					GetUnitsValue(TempUnits, x1, strx1, 0);
					GetUnitsValue(TempUnits, y1, stry1, 0);
					sprintf(str, "P %s %s %d %c 0 0;4=1", strx1, stry1, ApertureNr, PadPolarity);

					if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
					{
						sprintf(str3, ";3=%d", Object->Info & 0xffff);
						strcat(str, str3);
					}

					AddToMessageBuf(str);
				}

				cnt2++;
				break;

			case OBJECT_ARC:
			case TRACE_ARC:
				if ((Object->Info & OBJECT_FILLED) == 0)
				{
#ifdef _DEBUG

					if (Layer == 1)
						ok = 1;

#endif

					if (NotInRangeSpecial(x2, y2, 100.0))
					{
						SegmentCount = 0;
						LineSegments =
						    ArcToLineSegments(Object->x1, Object->y1, Object->x2, Object->y2, Object->x3, Object->y3,
						                      Object->x4, Object->y4, (double *) &LineBuf, 1);

						for (cnt4 = 0; cnt4 < LineSegments; cnt4++)
						{
							xx1 = LineBuf[SegmentCount++];
							yy1 = LineBuf[SegmentCount++];
							xx2 = LineBuf[SegmentCount++];
							yy2 = LineBuf[SegmentCount++];
							GetUnitsValue(TempUnits, xx1, strx1, 0);
							GetUnitsValue(TempUnits, yy1, stry1, 0);
							GetUnitsValue(TempUnits, xx2, strx2, 0);
							GetUnitsValue(TempUnits, yy2, stry2, 0);
							sprintf(str, "L %s %s %s %s %d %c 0 0", strx1, stry1, strx2, stry2, ApertureNr,
							        PadPolarity);
							AddToMessageBuf(str);
							cnt2++;
						}
					}
					else
					{
						GetArcEndPoints(Object, &x1a, &y1a, &x2a, &y2a, 0);
						GetUnitsValue(TempUnits, x1, strx1, 0);
						GetUnitsValue(TempUnits, y1, stry1, 0);
						GetUnitsValue(TempUnits, x1a, strx2, 0);
						GetUnitsValue(TempUnits, y1a, stry2, 0);
						GetUnitsValue(TempUnits, x2a, strx3, 0);
						GetUnitsValue(TempUnits, y2a, stry3, 0);
						sprintf(str, "A %s %s %s %s %s %s %d %c 0 N;0=%d", strx2, stry2, strx3, stry3, strx1, stry1,
						        ApertureNr, PadPolarity, PadMode);
						AddToMessageBuf(str);
					}
				}
				else
				{
					GetUnitsValue(TempUnits, x1, strx1, 0);
					GetUnitsValue(TempUnits, y1, stry1, 0);
					sprintf(str, "P %s %s %d %c 0 %d;0=%d", strx1, stry1, ApertureNr, PadPolarity, RotationNr, PadMode);
					AddToMessageBuf(str);
				}

				cnt2++;
				break;

			case OBJECT_TEXT:
			case OBJECT_TEXT2:
				TextP = (LPSTR) Object->TraceNr;
#ifdef _DEBUG

				if (stricmpOwn(TextP, "u101") == 0)
					ok = 1;

				if (Layer == 3)
					ok = 1;

#endif
				RotationAngle = Object->RotationAngle;
				Mirror = Object->Mirror;
				memset(&str, 0, sizeof(str));
				strncpy(str, TextP, 512);
				Length = strlen(str);
				x1 = Object->x1;
				y1 = Object->y1;
				x2 = Object->x2;
				x4 = 0.0;
				y4 = 0.0;
				FontNr = Object->Test >> 16;
				NrLines = ConvertObjectTextToStrings(TextP, FontNr, &MaxCountX, Object->Layer);

				for (cnt7 = 0; cnt7 < NrLines; cnt7++)
				{
					if (FontNr == 0)
						cnt2 += DrawOdbStr(x1, y1, x2, ApertureNr, RotationAngle, 0, Mirror, TextStrings2[cnt7]);
					else
					{
						incX = 0.0;
						incY = 0.0;
						x4 = x1;
						y4 = y1;
						str2 = (WCHAR *) & TextStrings[cnt7];
						lengte = (int32) wcslen(str2);
						AllocateSpecialMem(MEM_TRUETYPE_AREAFILL, 128 * 1024, (void **) &AreaFill);

						for (cnt4 = 0; cnt4 < lengte; cnt4++)
						{
							code = (*str2);
							//    code='+';
							DrawChar = 0;

							//    code=127;
							if ((code != ' ') && (code != '\t'))
							{
								if (GetAreaFillFontChar(code, FontNr, AreaFill) == 0)
									break;

								DrawChar = 1;
							}
							else
							{
								incX = TRUETYPE_FONT_SPACE_EXTRA_X * x2;
								incY = 0.0;
								RotatePoint2(&incX, &incY, RotationAngle);
							}

							if (DrawChar)
							{
								incX = (AreaFill->maxx - AreaFill->minx + TRUETYPE_FONT_ADD_EXTRA_X) * x2;
								incY = 0.0;
								RotatePoint2(&incX, &incY, RotationAngle);
								DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
								PolygonPos = (uint8 *) DrawPolygon;

								for (cnt5 = 0; cnt5 < AreaFill->NrPolygons; cnt5++)
								{
									count2 = DrawPolygon->NrVertices;
#ifdef _DEBUG

									if ((DrawPolygon->PolygonType & 8) == 8)
										ok = 1;

#endif

									for (cnt6 = 0; cnt6 < count2; cnt6++)
									{
										DrawPolygon->Points[cnt6].x *= x2;
										DrawPolygon->Points[cnt6].y *= x2;
										RotatePoint2(&DrawPolygon->Points[cnt6].x, &DrawPolygon->Points[cnt6].y,
										             RotationAngle);

										if (Mirror == 1)
											DrawPolygon->Points[cnt6].x = -DrawPolygon->Points[cnt6].x;

										DrawPolygon->Points[cnt6].x += x4;
										DrawPolygon->Points[cnt6].y += y4;
									}

									if ((DrawPolygon->PolygonType & 8) == 0)
									{
										if (cnt5 > 0)
										{
											AddToMessageBuf("SE");
											cnt2++;
										}

										AddToMessageBuf("S P 0");
										PlotPolygonToOdb(DrawPolygon, 0);
									}
									else
										PlotPolygonToOdb(DrawPolygon, 1);

									PolygonPos += MemSizePolygon(DrawPolygon);
									DrawPolygon = (PolygonRecord *) PolygonPos;
								}

								AddToMessageBuf("SE");
								cnt2++;
							}

							if (Mirror == 0)
							{
								x4 += incX;
								y4 += incY;
							}
							else
							{
								x4 -= incX;
								y4 += incY;
							}

							str2++;
						}
					}

					if (FontNr == 0)
					{
						if (Mirror == 0)
							x1 += sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.1;
						else
							x1 -= sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.1;

						y1 -= cos(ANGLE_CONVERT(RotationAngle)) * x2 * 1.1;
					}
					else
					{
						if (Mirror == 0)
							x1 += sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.4;
						else
							x1 -= sin(ANGLE_CONVERT(RotationAngle)) * x2 * 1.4;

						y1 -= cos(ANGLE_CONVERT(RotationAngle)) * x2 * 1.4;
					}
				}

				break;

			case OBJECT_POLYGON:
				if (CheckObjectIsBigPolygon(Object))
				{
					GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
					res = PlotAreaFillToOdb(AreaFill);
					//          res=PlotAreaFillToGerber(AreaFill,AreafillPenSize1,AreafillPenSize2,1);
				}
				else
				{
					MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

					if ((Object->ObjectType2 != 0) && (Object->ObjectType != PIN_PUT_THROUGH_POLYGON)
					        && ((Object->Info & OBJECT_FILLED) == 0))
					{
#ifdef _DEBUG

						if (Layer == SILKSCREEN_TOP)
							ok = 1;

#endif
						GetUnitsValue(TempUnits, PolygonObject->Points[0].x, strx1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[0].y, stry1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[1].x, strx2, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[1].y, stry2, 0);
						sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr,
						        PadPolarity, PadMode);
						AddToMessageBuf(str);
						cnt2++;
						GetUnitsValue(TempUnits, PolygonObject->Points[1].x, strx1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[1].y, stry1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[2].x, strx2, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[2].y, stry2, 0);
						sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr,
						        PadPolarity, PadMode);
						AddToMessageBuf(str);
						cnt2++;
						GetUnitsValue(TempUnits, PolygonObject->Points[2].x, strx1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[2].y, stry1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[3].x, strx2, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[3].y, stry2, 0);
						sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr,
						        PadPolarity, PadMode);
						AddToMessageBuf(str);
						cnt2++;
						GetUnitsValue(TempUnits, PolygonObject->Points[3].x, strx1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[3].y, stry1, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[0].x, strx2, 0);
						GetUnitsValue(TempUnits, PolygonObject->Points[0].y, stry2, 0);
						sprintf(str, "L %s %s %s %s %d %c 0 0;0=%d", strx1, stry1, strx2, stry2, ApertureNr,
						        PadPolarity, PadMode);
						AddToMessageBuf(str);
						cnt2++;
					}
					else
					{
						AddToMessageBuf("S P 0");
						PlotPolygonToOdb(PolygonObject, 0);
						AddToMessageBuf("SE");
						cnt2++;
					}
				}

				break;

			case PIN_SMD_POLYGON:
				if (ApertureNr >= 0)
				{
					GetUnitsValue(TempUnits, x1, strx1, 0);
					GetUnitsValue(TempUnits, y1, stry1, 0);
					sprintf(str, "P %s %s %d %c 0 0;4=1", strx1, stry1, ApertureNr, PadPolarity);

					if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
					{
						sprintf(str3, ";3=%d", Object->Info & 0xffff);
						strcat(str, str3);
					}

					AddToMessageBuf(str);
					cnt2++;
				}
				else
				{


					if (CheckObjectIsBigPolygon(Object))
					{
						GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
						res = PlotAreaFillToOdb(AreaFill);
						cnt2++;
					}
				}

				ok = 1;
				break;

			case PIN_PUT_THROUGH_POLYGON:
				if (ApertureNr >= 0)
				{
					GetUnitsValue(TempUnits, x1, strx1, 0);
					GetUnitsValue(TempUnits, y1, stry1, 0);
					sprintf(str, "P %s %s %d %c 0 0;4=1", strx1, stry1, ApertureNr, PadPolarity);

					if ((Layer >= 0) && (Layer < 32) && ((Object->Info & 0xffff) != 0xffff))
					{
						sprintf(str3, ";3=%d", Object->Info & 0xffff);
						strcat(str, str3);
					}

					AddToMessageBuf(str);
				}
				else
				{
					if (CheckObjectIsBigPolygon(Object))
					{
						GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
						res = PlotAreaFillToOdb(AreaFill);
					}
					else
					{
						AddToMessageBuf("S P 0");
						PlotPolygonToOdb(PolygonObject, 0);
						AddToMessageBuf("SE");
					}
				}

				cnt2++;
				ok = 1;
				break;

			case AREAFILL:
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Object->TraceNr]]);
				res = PlotAreaFillToOdb(AreaFill);
				cnt2++;
				break;

			case AREAFILL2:
				PowerPlaneToOdb(Layer);
				cnt2++;
				break;

			default:
				ok = 1;
				break;
			}

			cnt++;
			Object++;
		}


		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectPolygon->Layer == Layer))
			{
#ifdef _DEBUG

				if (Layer == 3)
					ok = 1;

#endif
				MemSize = MemSizeObjectPolygon(ObjectPolygon) + 8192;

				if (MemSize >= MaxAreaFillMemoryTemp)
				{
					if (AllocateMemAreaFillMemoryTemp(MemSize) != 0)
						return -105;

					NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
					TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
				}

				memset(NewAreaFill, 0, MemSize);
				DrawPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
				DrawPolygon->NrVertices = ObjectPolygon->NrVertices;
				memcpy(&(*DrawPolygon).Points, &(*ObjectPolygon).Points,
				       ObjectPolygon->NrVertices * sizeof(PointRecord));
				AddToMessageBuf("S P 0");
				PlotPolygonToOdb(DrawPolygon, 0);
				AddToMessageBuf("SE");
			}
		}

		sprintf(Filename, "%s\\steps\\%s\\layers\\%s\\features", OdbDir, JobName, OdbLayers[cnt3].LayerName);
		DeleteFileUTF8(Filename);
		AppendStringToTextFileUTF8(Filename, "#");
		AppendStringToTextFileUTF8(Filename, "#Feature symbol names");
		AppendStringToTextFileUTF8(Filename, "#");
		AppendStringToTextFileUTF8(Filename, MessageBuf);
	}


	if (AllocateMemObjects6(MaxNrObjects5) == -1)
		return -1;

	memcpy(&((*Objects6)[0]), &((*Objects5)[0]), NrObjects5 * sizeof(ObjectRecord));
	NrObjects6 = NrObjects5;
	qsort(Objects6, NrObjects6, sizeof(ObjectRecord), ObjectSortCompare);

// *******************************************************************************************************
// *******************************************************************************************************

	sprintf(Filename, "%s\\steps\\%s\\netlists\\cadnet\\netlist", OdbDir, JobName);
	DeleteFileUTF8(Filename);
	MessageBufPos = 0;
	AddToMessageBuf("H optimize n staggered n");

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		sprintf(str, "$%d %s", cnt, Net->Name);
		AddToMessageBuf(str);
	}

	AddToMessageBuf("#");
	AddToMessageBuf("#Netlist points");
	AddToMessageBuf("#");
	GetUnitsValue(TempUnits, 0.05e5, strx2, 1);

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

				if (Object->NetNr == -1)
					continue;

				GetUnitsValue(TempUnits, Object->x1, strx1, 1);
				GetUnitsValue(TempUnits, Object->y1, stry1, 1);
				sprintf(str, "%d %s %s %s", Object->NetNr, strx2, strx1, stry1);

				if (Object->Layer == -1)
					strcat(str, " B e e staggered 0 0 0");
				else
				{
					if (Object->Layer == 0)
						strcat(str, " D e e staggered 0 0 0");
					else
						strcat(str, " T e e staggered 0 0 0");
				}

				AddToMessageBuf(str);
			}
		}
	}

	AddToMessageBuf("#Feature symbol names");
	AddToMessageBuf("#");
	AppendStringToTextFileUTF8(Filename, MessageBuf);


// *******************************************************************************************************
// *******************************************************************************************************

	sprintf(Filename, "%s\\steps\\%s\\eda\\data", OdbDir, JobName);
	DeleteFileUTF8(Filename);
	MessageBufPos = 0;
	sprintf(str, "HDR PCB elegance %d.%d database", VER_VERSION / 100, (VER_VERSION % 100) / 10);
	AddToMessageBuf(str);
	strcpy(str, "LYR");
	strcpy(str4, "#   ");
	cnt2 = 0;

	for (OdbLayer = 0; OdbLayer < NrOdbLayers; OdbLayer++)
	{
		Layer = OdbLayers[OdbLayer].LayerNr;

		switch (Layer)
		{
		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
		case SILKSCREEN_BOTTOM:
		case SILKSCREEN_TOP:
		case SILKSCREEN_BOTTOM_REFS:
		case SILKSCREEN_TOP_REFS:
		case SILKSCREEN_BOTTOM_VALUES:
		case SILKSCREEN_TOP_VALUES:
		case DRILL_LAYER:
			strcat(str, " ");
			strcpy(str3, OdbLayers[OdbLayer].LayerName);
			strlwrUTF8(str3);
			strcat(str, str3);

			if (Layer == DRILL_LAYER)
				OdbConvertLayer[32] = cnt2;

			sprintf(str5, "%-*d", strlen(str3) + 1, cnt2);
			strcat(str4, str5);
			cnt2++;
			break;

		default:
			if (Layer < 32)
			{
				strcat(str, " ");
				strcpy(str3, OdbLayers[OdbLayer].LayerName);
				strlwrUTF8(str3);
				strcat(str, str3);
				OdbConvertLayer[Layer] = cnt2;
				sprintf(str5, "%-*d", strlen(str3) + 1, cnt2);
				strcat(str4, str5);
				cnt2++;
//          OdbLayers[cnt3].Inverted=1;
			}

			break;
		}
	}

	AddToMessageBuf(str4);
	AddToMessageBuf(str);
	AddToMessageBuf("#");
	AddToMessageBuf("#Net attribute names");
	AddToMessageBuf("#");

	cnt2 = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		sprintf(str, "# NET %d", cnt);
		AddToMessageBuf(str);
		sprintf(str, "NET %s", Net->Name);
		AddToMessageBuf(str);
		PreviousObject = 0;

		while (1)
		{
			if (cnt2 == NrObjects6)
				break;

			Object6 = &((*Objects6)[cnt2]);

			if (Object6->NetNr == cnt)
				break;

			cnt2++;
		}

		ok = 1;
		PreviousLayer = -1;

		while (1)
		{
			if (cnt2 == NrObjects6)
				break;

			Object6 = &((*Objects6)[cnt2]);
			Layer = Object6->Layer >> 24;
#ifdef _DEBUG

			if ((InRange9(Object6->x1, 176.0e5)) && (InRange9(Object6->y1, 70.6e5)))
			{
				ok = 1;

				if (Layer == 1)
					ok = 1;
			}

#endif

			if (Object6->NetNr != cnt)
				break;

			if ((Layer < 0) || ((Layer >= Design.NrBoardLayers) && (Layer != 32)))
			{
				cnt2++;
				continue;
			}

			OdbPowerPlaneLayer = 0;

			if (PowerPlaneLayers[Layer])
			{
				OdbPowerPlaneLayer = 1;
//        cnt2++;
//        continue;
			}

			Layer2 = -1;

			if ((Layer >= 0) && (Layer < 64))
				Layer2 = OdbConvertLayer[Layer];

			if (Layer2 == -1)
				ok = 1;

			ObjectNr = Object6->Layer & 0xffffff;

			switch (Object6->ObjectType)
			{
			case OBJECT_LINE:
				if (PreviousObject != ODB_SUB_NET_OBJECT)
				{
					sprintf(str, "SNT TRC");
					AddToMessageBuf(str);
				}

				if (!OdbPowerPlaneLayer)
					sprintf(str, "FID C %d %d", Layer2, ObjectNr);
				else
					sprintf(str, "FID L %d %d", Layer2, ObjectNr);

				AddToMessageBuf(str);
				PreviousObject = ODB_SUB_NET_OBJECT;
				PreviousLayer = -1;
				break;

			case VIA_PUT_THROUGH_ROUND:
				if (PreviousObject != ODB_VIA_OBJECT)
				{
					sprintf(str, "SNT VIA");
					AddToMessageBuf(str);
					PreviousObject = ODB_VIA_OBJECT;
				}

				if (!OdbPowerPlaneLayer)
					sprintf(str, "FID C %d %d", Layer2, ObjectNr);
				else
					sprintf(str, "FID L %d %d", Layer2, ObjectNr);

				AddToMessageBuf(str);
				PreviousLayer = -1;
				break;

			case PIN_PUT_THROUGH_ROUND_POWER:
				if (PreviousObject != ODB_SUB_NET_OBJECT)
				{
					sprintf(str, "SNT TRC");
					AddToMessageBuf(str);
				}

				sprintf(str, "FID L %d %d", Layer2, ObjectNr);
				AddToMessageBuf(str);
				PreviousObject = ODB_SUB_NET_OBJECT;
				break;

			case AREAFILL:
				if (PreviousObject != ODB_PLANE_OBJECT)
				{
					sprintf(str, "SNT PLN S E 0.0");
					AddToMessageBuf(str);
				}

				if (!OdbPowerPlaneLayer)
					sprintf(str, "FID C %d %d", Layer2, ObjectNr);
				else
					sprintf(str, "FID L %d %d", Layer2, ObjectNr);

				AddToMessageBuf(str);
				PreviousObject = ODB_PLANE_OBJECT;
				PreviousLayer = -1;
				break;

			case DRILL:
				if (PreviousObject != ODB_VIA_OBJECT)
				{
					sprintf(str, "SNT VIA");
					AddToMessageBuf(str);
					PreviousObject = ODB_VIA_OBJECT;
				}

				sprintf(str, "FID H %d %d", Layer2, ObjectNr);
				AddToMessageBuf(str);
				PreviousLayer = -1;
				break;

			case DRILL_UNPLATED:
				PreviousLayer = -1;
				break;

			case PIN_PUT_THROUGH_ROUND:
			case PIN_PUT_THROUGH_SQUARE:
			case PIN_PUT_THROUGH_POLYGON:
				if ((Layer == Design.NrBoardLayers - 1) || (Layer == 0))
				{
					sprintf(str, "SNT TOP T %d %d", Object6->TraceNr, Object6->PinNr);
					AddToMessageBuf(str);
					PreviousLayer = Layer;
					PreviousObject = ODB_COMP_OBJECT;
					sprintf(str, "FID C %d %d", Layer2, ObjectNr);
					AddToMessageBuf(str);
				}
				else
				{
					if (PreviousObject != ODB_VIA_OBJECT)
					{
						sprintf(str, "SNT VIA");
						AddToMessageBuf(str);
						PreviousObject = ODB_VIA_OBJECT;
					}

					if (!OdbPowerPlaneLayer)
						sprintf(str, "FID C %d %d", Layer2, ObjectNr);
					else
						sprintf(str, "FID L %d %d", Layer2, ObjectNr);

					AddToMessageBuf(str);
				}

				break;

			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case PIN_LINE_HOR:
			case PIN_LINE_VER:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
			case PIN_LINE_ALL_ANGLE:
			case PIN_ARC:
			case PIN_SMD_POLYGON:
			case THERMAL_RELIEF_ROUND:
				if (Layer == 0)
				{
					sprintf(str, "SNT TOP B %d %d", Object6->TraceNr, Object6->PinNr);
					AddToMessageBuf(str);
					PreviousObject = ODB_COMP_OBJECT;
				}
				else
				{
					if (Layer == Design.NrBoardLayers - 1)
					{
						sprintf(str, "SNT TOP T %d %d", Object6->TraceNr, Object6->PinNr);
						AddToMessageBuf(str);
						PreviousObject = ODB_COMP_OBJECT;
					}
					else
					{
						if (PreviousObject != ODB_SUB_NET_OBJECT)
						{
							sprintf(str, "SNT TRC");
							AddToMessageBuf(str);
						}

						PreviousObject = ODB_SUB_NET_OBJECT;
						PreviousLayer = -1;
					}
				}

				if (!OdbPowerPlaneLayer)
					sprintf(str, "FID C %d %d", Layer2, ObjectNr);
				else
					sprintf(str, "FID L %d %d", Layer2, ObjectNr);

				AddToMessageBuf(str);
				break;
			}

			cnt2++;
		}
	}

	AddToMessageBuf("#");
	cnt = 0;
	memset(&ShapesUsed, 0, sizeof(ShapesUsed));

	for (cnt2 = 0; cnt2 < Design.NrComps; cnt2++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt2]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			ShapeNr = (int32) Comp->ShapeNr;

			if (ShapeNr == -1)
				continue;

			if (ShapesUsed[ShapeNr])
				continue;

			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			ShapesUsed[ShapeNr] = 1;
			sprintf(str, "# PKG %d", cnt);
			AddToMessageBuf(str);
			NewComp.ShapeNr = (int16) ShapeNr;
			NrObjects = 0;
			ShapePinsToObject(&NewComp, 0.0, 0.0, 0, 0, 0, 0);
			ShapeCompOutLineToObject(&NewComp, 0.0, 0.0, 0.0);
			ShapeCompSilkScreenToObject(&NewComp, 0.0, 0.0, 0.0);
			ShapePlacementOutLineToObject(&NewComp, 0.0, 0.0, 0.0);
			ShapeOtherToObject(&NewComp, 0.0, 0.0, 0.0, -1, 0);
			MinX = 1e9;
			MinY = 1e9;
			MaxX = -1e9;
			MaxY = -1e9;

			for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
			{
				Object = &((*Objects)[cnt4]);
				FillPositionObject(Object);
				MinX = min(MinX, Object->minx);
				MaxX = max(MaxX, Object->maxx);
				MinY = min(MinY, Object->miny);
				MaxY = max(MaxY, Object->maxy);
			}

			MinPitch = 1e9;
			NrObjects = 0;
			ShapePinsToObject(&NewComp, 0.0, 0.0, 0, 0, 0, 0);

			for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
			{
				Object = &((*Objects)[cnt4]);

				for (cnt5 = cnt4 + 1; cnt5 < NrObjects; cnt5++)
				{
					Object2 = &((*Objects)[cnt5]);
					MinPitch = min(MinPitch, CalcLengthLine(Object->x1, Object->y1, Object2->x1, Object2->y1));
				}
			}

			GetUnitsValue(TempUnits, MinX, strx1, 1);
			GetUnitsValue(TempUnits, MinY, stry1, 1);
			GetUnitsValue(TempUnits, MaxX - MinX, strx2, 1);
			GetUnitsValue(TempUnits, MaxY - MinY, stry2, 1);
			GetUnitsValue(TempUnits, MinPitch, strx3, 1);
#ifdef _DEBUG

			if (stricmp(Shape->ShapeName, "new_geom_4") == 0)
				ok = 1;

#endif
			sprintf(str, "PKG %s %s %s %s %s %s", Shape->ShapeName, strx3, strx1, stry1, strx2, stry2);
			AddToMessageBuf(str);
			sprintf(str, "RC %s %s %s %s", strx1, stry1, strx2, stry2);
			AddToMessageBuf(str);
			memset(&PinsUsed, 0, sizeof(PinsUsed));
			NrObjects = 0;
			ShapePinsToObject(&NewComp, 0.0, 0.0, 0, 0, 0, 0);

			for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
			{
				Object = &((*Objects)[cnt4]);
#ifdef _DEBUG
				CompPinText(&NewComp, Object->PinNr, 0.0, 0.0, str3);

				if (stricmp(Shape->ShapeName, "new_geom_4") == 0)
				{
					ok = 1;

					if (stricmp(str3, "bottom2") == 0)
						ok = 1;

					if (Object->PinNr == 4)
						ok = 1;
				}

#endif

				if (Object->PinNr < 0)
					continue;

				if (PinsUsed[Object->PinNr])
					continue;

				FillPositionObject(Object);
				PinsUsed[Object->PinNr] = 1;
				CompPinText(&NewComp, Object->PinNr, 0.0, 0.0, str3);
				GetUnitsValue(TempUnits, Object->x1, strx1, 1);
				GetUnitsValue(TempUnits, Object->y1, stry1, 1);

				if (Object->Layer != -1)
					sprintf(str, "PIN %s S %s %s 0 U U", str3, strx1, stry1);
				else
					sprintf(str, "PIN %s T %s %s 0 U U", str3, strx1, stry1);

				AddToMessageBuf(str);

				switch (Object->ObjectType)
				{
				case PIN_SMD_ROUND:
				case DRILL:
				case DRILL_UNPLATED:
				case PIN_PUT_THROUGH_ROUND:
					GetUnitsValue(TempUnits, Object->x2 * 0.5, strx2, 1);
					sprintf(str, "CR %s %s %s", strx1, stry1, strx2);
					AddToMessageBuf(str);
					break;

				case PIN_PUT_THROUGH_SQUARE:
					GetUnitsValue(TempUnits, Object->x2 * 0.5, strx2, 1);
					sprintf(str, "SQ %s %s %s", strx1, stry1, strx2);
					AddToMessageBuf(str);
					break;

				case PIN_SMD_RECT:
					x1 = Object->x1 - Object->x2 * 0.5;
					y1 = Object->y1 - Object->y2 * 0.5;
					GetUnitsValue(TempUnits, x1, strx1, 1);
					GetUnitsValue(TempUnits, y1, stry1, 1);
					GetUnitsValue(TempUnits, Object->x2, strx2, 1);
					GetUnitsValue(TempUnits, Object->y2, stry2, 1);
					sprintf(str, "RC %s %s %s %s", strx1, stry1, strx2, stry2);
					AddToMessageBuf(str);
					break;

				case PIN_SMD_POLYGON:
					if (!CheckObjectIsBigPolygon(Object))
					{
						GeomPolygon = (GeomPolygonRecord *) Object->Address;
						MemSize = GeomPolygon->NrVertices * 16 + 8192;

						if (MemSize >= MaxAreaFillMemoryTemp)
						{
							if (AllocateMemAreaFillMemoryTemp(MemSize) != 0)
								return -105;

							NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
							TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
						}

						memset(NewAreaFill, 0, MemSize);
						DrawPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
						MakePolygonFromObject(Object, DrawPolygon, 0.0, 0, 0, 1);
						AddToMessageBuf("CT");
						PlotPolygonToOdb(DrawPolygon, 0);
						AddToMessageBuf("CE");
					}

					break;

				case PIN_PUT_THROUGH_POLYGON:
					MemSize = 8192;

					if (MemSize >= MaxAreaFillMemoryTemp)
					{
						if (AllocateMemAreaFillMemoryTemp(MemSize) != 0)
							return -105;

						NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
						TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;
					}

					memset(NewAreaFill, 0, MemSize);
					DrawPolygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
					MakePolygonFromObject(Object, DrawPolygon, 0.0, 0, 0, 1);
					AddToMessageBuf("CT");
					PlotPolygonToOdb(DrawPolygon, 0);
					AddToMessageBuf("CE");
					break;

				default:
					GetUnitsValue(TempUnits, 0.5e5, strx2, 1);
					sprintf(str, "CR %s %s %s", strx1, stry1, strx2);
					AddToMessageBuf(str);
					break;
				}
			}

			cnt++;
		}
	}

	AppendStringToTextFileUTF8(Filename, MessageBuf);


// *******************************************************************************************************
// *******************************************************************************************************

#if 0
	sprintf(Filename, "%s\\steps\\%s\\eda\\net_prp", OdbDir);
	DeleteFileUTF8(Filename);
	MessageBufPos = 0;
	/*
	NET_TYPE_CLEARANCES {
	    net_type1 = EAP
	    net_type2 = *
	    layers = *
	    via2via = 0.007874
	    via2trace = 0.007874
	    via2pin = 0.005906
	    via2plane = 0.031496
	    trace2trace = 0.019685
	    trace2pin = 0.007874
	    trace2plane = 0.031496
	    pin2pin = 0.007874
	    pin2plane = 0.031496
	    plane2plane = 0.031496
	}
	*/

	AppendStringToTextFileUTF8(Filename, MessageBuf);
#endif

// *******************************************************************************************************
// *******************************************************************************************************

	/*
	#
	#Component attribute names
	#
	@0 .comp_mount_type
	@1 .comp_height

	# CMP 0
	CMP 28 3.475 1.8 180.0 N D1 DIODE_PHOTO_EMIT ;0=2,1=0.001000
	PRP .type_name 'cap'
	PRP VALUE '100nF'
	PRP MODEL 'NULL'
	TOP 0 3.475 1.8 180.0 N 164 14 1
	TOP 1 3.375 1.8 180.0 N 118 0 2
	*/

	cnt3 = 0;

	if (NrTopComps > 0)
	{
		sprintf(Filename, "%s\\steps\\%s\\layers\\comp_+_top\\components", OdbDir, JobName);
		DeleteFileUTF8(Filename);
		MessageBufPos = 0;
		AddToMessageBuf("#");
		AddToMessageBuf("#Component attribute names");
		AddToMessageBuf("#");
		AddToMessageBuf("@0 .comp_mount_type");
		AddToMessageBuf("");
//    AddToMessageBuf("@1 .comp_height");
		AddToMessageBuf("#");

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Mirror = ((Comp->CompMode & 8) >> 3);

				if (Mirror == 1)
					continue;

				sprintf(str, "# CMP %d", cnt2);
				AddToMessageBuf(str);
				OriginX = Comp->CompOriginX;
				OriginY = Comp->CompOriginY;
				GetUnitsValue(TempUnits, OriginX, strx1, 0);
				GetUnitsValue(TempUnits, OriginY, stry1, 0);
				ShapeNr = (int32) Comp->ShapeNr;

				if (ShapeNr == -1)
					continue;

				MemPos = (*Shapes)[ShapeNr].ShapePos;
				Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

				if (Shape->Info & SMD_DEVICE)
					MountType = 1;
				else
					MountType = 2;

				str3[0] = 0;

				if (Comp->PartNr[0])
					strcpy(str3, Comp->PartNr);
				else
				{
//            strcpy(str3,"NONE");
				}

				sprintf(str, "CMP %d %s %s %.1f N %s %s ;0=%d", ShapeNr, strx1, stry1, 360.0 - Comp->Rotation,
				        Comp->Name, str3, MountType);
				AddToMessageBuf(str);
				/*
				PRP .type_name '74LVC14A'
				*/
				sprintf(str, "PRP VALUE '%s'", Comp->Value);
				AddToMessageBuf(str);
				NrCompProperties = GetCompProperties(Comp, NULL, NULL, 0x40);

				if (NrCompProperties > 0)
				{
					for (cnt4 = 0; cnt4 < NrCompProperties; cnt4++)
					{
						if (GetCompProperties(Comp, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt4) == 0)
						{
							sprintf(str, "PRP %s '%s'", PropertyID, PropertyValue);
							AddToMessageBuf(str);
						}
					}
				}

				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

				for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
				{
					Object = &((*Objects)[cnt4]);

					if (CompPinText(Comp, Object->PinNr, 0.0, 0.0, str3) == 0)
					{
						GetUnitsValue(TempUnits, Object->x1, strx1, 0);
						GetUnitsValue(TempUnits, Object->y1, stry1, 0);
						sprintf(str, "TOP %d %s %s 0.0 N %d %d %s-%s", Object->PinNr, strx1, stry1, Object->NetNr, 0,
						        Comp->Name, str3);
						AddToMessageBuf(str);
					}
				}
			}

			cnt2++;
			cnt3++;
		}

		AppendStringToTextFileUTF8(Filename, MessageBuf);
	}

	if (NrBottomComps > 0)
	{
		sprintf(Filename, "%s\\steps\\%s\\layers\\comp_+_bot\\components", OdbDir, JobName);
		DeleteFileUTF8(Filename);
		MessageBufPos = 0;
		AddToMessageBuf("#");
		AddToMessageBuf("#Component attribute names");
		AddToMessageBuf("#");
		AddToMessageBuf("@0 .comp_mount_type");
		AddToMessageBuf("");
//    AddToMessageBuf("@1 .comp_height");
		AddToMessageBuf("#");

		cnt2 = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				Mirror = ((Comp->CompMode & 8) >> 3);

				if (Mirror == 0)
					continue;

				sprintf(str, "# CMP %d", cnt2);
				AddToMessageBuf(str);
				OriginX = Comp->CompOriginX;
				OriginY = Comp->CompOriginY;
				GetUnitsValue(TempUnits, OriginX, strx1, 0);
				GetUnitsValue(TempUnits, OriginY, stry1, 0);
				ShapeNr = (int32) Comp->ShapeNr;

				if (ShapeNr == -1)
					continue;

				MemPos = (*Shapes)[ShapeNr].ShapePos;
				Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

				if (Shape->Info & SMD_DEVICE)
					MountType = 1;
				else
					MountType = 2;

				str3[0] = 0;

				if (Comp->PartNr[0])
					strcpy(str3, Comp->PartNr);
				else
					strcpy(str3, "NONE");

				sprintf(str, "CMP %d %s %s %.1f N %s %s ;0=%d", ShapeNr, strx1, stry1, Comp->Rotation, Comp->Name, str3,
				        MountType);
				AddToMessageBuf(str);
				/*
				PRP .type_name '74LVC14A'
				*/
				sprintf(str, "PRP VALUE '%s'", Comp->Value);
				AddToMessageBuf(str);
				NrCompProperties = GetCompProperties(Comp, NULL, NULL, 0x40);

				if (NrCompProperties > 0)
				{
					for (cnt4 = 0; cnt4 < NrCompProperties; cnt4++)
					{
						if (GetCompProperties(Comp, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt4) == 0)
						{
							sprintf(str, "PRP %s '%s'", PropertyID, PropertyValue);
							AddToMessageBuf(str);
						}
					}
				}

				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

				for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
				{
					Object = &((*Objects)[cnt4]);

					if (CompPinText(Comp, Object->PinNr, 0.0, 0.0, str3) == 0)
					{
						GetUnitsValue(TempUnits, Object->x1, strx1, 0);
						GetUnitsValue(TempUnits, Object->y1, stry1, 0);
						sprintf(str, "TOP %d %s %s 0.0 N %d %d %s-%s", Object->PinNr, strx1, stry1, Object->NetNr, 0,
						        Comp->Name, str3);
						AddToMessageBuf(str);
					}
				}

				cnt2++;
				cnt3++;
			}
		}

		AppendStringToTextFileUTF8(Filename, MessageBuf);
	}

// *******************************************************************************************************
// *******************************************************************************************************

	MessageBufPos = 0;
	AllocateMemTemp(256 * 1024);
	BoardOutlineAreaFill = (AreaFillRecord *) TempMem;
	res = GetBoardOutlineAreaFill(BoardOutlineAreaFill, 0.0, 1);

	if (res == 0)
	{
		sprintf(Filename, "%s\\steps\\%s\\profile", OdbDir, JobName);
		DeleteFileUTF8(Filename);
		AddToMessageBuf("#");
		AddToMessageBuf("#Layer features");
		AddToMessageBuf("#");
		AddToMessageBuf("S P 0");
		DrawPolygon = (PolygonRecord *) ((uint8 *) BoardOutlineAreaFill + sizeof(AreaFillRecord));
		PlotPolygonToOdb(DrawPolygon, 0);
		AddToMessageBuf("SE");
		AppendStringToTextFileUTF8(Filename, MessageBuf);
	}

// *******************************************************************************************************
// *******************************************************************************************************

	ExportSpecialOdbSymbols(OdbDir, 0);

// *******************************************************************************************************
// *******************************************************************************************************

	sprintf(str, "%s\\misc\\info", OdbDir);
	DeleteFileUTF8(str);
	sprintf(str3, "JOB_NAME=%s", JobName);
	AppendStringToTextFileUTF8(str, str3);
	AppendStringToTextFileUTF8(str, "ODB_VERSION_MAJOR=6");
	AppendStringToTextFileUTF8(str, "ODB_VERSION_MINOR=3");
	AppendStringToTextFileUTF8(str, "ODB_SOURCE=");
	GetLocalTime(&CurrentDate);
	sprintf(str3, "CREATION_DATE=%d%02d%02d.%02d%02d%02d", CurrentDate.wYear, CurrentDate.wMonth, CurrentDate.wDay,
	        CurrentDate.wHour, CurrentDate.wMinute, CurrentDate.wSecond);
	AppendStringToTextFileUTF8(str, str3);
	sprintf(str3, "SAVE_DATE=%d%02d%02d.%02d%02d%02d", CurrentDate.wYear, CurrentDate.wMonth, CurrentDate.wDay,
	        CurrentDate.wHour, CurrentDate.wMinute, CurrentDate.wSecond);
	AppendStringToTextFileUTF8(str, str3);
	sprintf(str3, "SAVE_APP=PCB ELEGANCE %d.%d", VER_VERSION / 100, (VER_VERSION % 100) / 10);
	AppendStringToTextFileUTF8(str, str3);
//  AppendStringToTextFileUTF8(str,"SAVE_USER=GW2ODB");

// *******************************************************************************************************
// *******************************************************************************************************

	sprintf(str, "%s\\misc\\job_name", OdbDir);
	DeleteFileUTF8(str);
	AppendStringToTextFileUTF8(str, JobName);

// *******************************************************************************************************
// *******************************************************************************************************

	strcpy(InfoStr, InfoStrCopy);
	RedrawInfoStr(1);

	strcpy(str4, EditFile);
	CutExtensionFileName(str4);
	strcpy(OdbFile, str4);
	TarOdbDirectory(OdbFile, OdbDir);
	DeleteDirectoryUnicode(OdbDirW);
	SetNormalCursor();
	MaxNrOdbApertureMacroObjects = 0;
	DeAllocateMemObjects4();
	DeAllocateMemObjects5();
	DeAllocateMemObjects6();
	DeAllocateMemMessageBuf();
	DeAllocateMemAperTures();
	DeAllocateMemTemp();
	DeAllocateMemTemp2();
	
	sprintf(str, SC(294, "ODB++ output ready.\n\nOutput file is\n\n%s\\pcb\\ODB++.tgz"), DesignPath); //nový název souboru
//	sprintf(str, SC(294, "ODB++ output ready\n\nOutput file is %s.tgz", OdbFile)); //pùvodní název souboru

	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
	
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
