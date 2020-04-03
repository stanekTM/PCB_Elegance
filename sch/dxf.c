/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: dxf.c
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
#include "dxf.h"
#include "files2.h"
#include "calcdef.h"
#include "calc.h"
#include "time.h"
#include "memory.h"
#include "resource.h"
#include "math.h"
#include "insdel.h"
#include "files.h"
#include "mainloop.h"
#include "utf8.h"
#include "property.h"
#include "ctype.h"

#define   DXFCharWidthFactor       1.1
#define   DxfFontSizeMult          0.8

#define   WIRE_LAYER               100
#define   BUS_LAYER                200
#define   JUNCTION_LAYER           300
#define   BUSCONNECTIONS_LAYER     400
#define   GLOBALCONNECTIONS_LAYER  500
#define   NETLABEL_LAYER           600
#define   SYMBOLS_LAYER            700
#define   OBJECTS_LAYER            800
#define   PIN_LAYER                900
#define   POWERPIN_LAYER           1000
#define   PINBUS_LAYER             1100
#define   REFERENCE_LAYER          1200
#define   VALUE_LAYER              1300
#define   ONEPINNET_LAYER          1400

#define  ClearObject(Object) (memset(&Object,0,sizeof(Object)))
#define  MAX_BLOCKS         512
#define  COMP_REF_LAYER     4100
#define  COMP_VALUE_LAYER   4200
#define  MAX_NR_LAYERS      32
#define  DXF_OUTPUT_INCH    2


double DXFminx, DXFminy, DXFmaxx, DXFmaxy;

int32 DXFOptions, ok, NrBlocks, NrDXFLayers, SelectedLayers[128], DialogMode;

char ExportDir[MAX_LENGTH_STRING];

char DXFLayers[128][64];
char DXFLayer[MAX_LENGTH_STRING], LayerNames[32][128];

int32 NrBlocks, NrDXFLayers, SelectedLayers[128], DialogMode, BlockStartP[MAX_BLOCKS], BlockEndP[MAX_BLOCKS], NrBlocks,
      BlockNr[5], BlockP[5], BlockSize[5], BlockLineNr[MAX_BLOCKS], FileSizeDXF, FilePointerDXF, NrGeomLayers,
      BlockNestingLevel, FilePosition[5], LayerIDs[32], NrSelectedDXFLayers;

char BlockNames[MAX_BLOCKS][MAX_NR_LAYERS], BlockNestingNames[5][MAX_NR_LAYERS], BlockLayerName[5][MAX_NR_LAYERS],
     ImportDXFFile[MAX_LENGTH_STRING], DXFLayers[128][64], DXFLayer[MAX_LENGTH_STRING], LayerNames[32][128],
     DXFPath[MAX_LENGTH_STRING] = "", ExportDXFFileName[MAX_LENGTH_STRING] = "", *DXFBuffer;

double BlockInsertX[5], BlockInsertY[5], BlockInsertZ[5], BlockRotation[5], BlockBaseX[5], BlockBaseY[5], BlockBaseZ[5],
       BlockRotation[5], BlockInsertScaleX[5], BlockInsertScaleY[5], StartAngle, EndAngle, PolyLineX[1000],
       PolyLineY[1000], PolyLineZ[1000], StartWidth, EndWidth, PolyLineStartWidth, PolyLineEndWidth,
       VertexStartWidth[1000], VertexEndWidth[1000], AngleBetweenLines[1000], PointsX[1000], PointsY[1000], MinX, MinY,
       MaxX, MaxY, VertexStartWidthTemp[1000], VertexEndWidthTemp[1000];


int32 NrOutputLayers, LayerInfo[128], ExportLayers[128], NrSelectedLayers;

//************************************************************************************************************************
//**************************** IDD_DIALOG_EXPORT_DXF *********************************************************************
//************************************************************************************************************************

int32 CALLBACK DxfDialog2(HWND Dialog, uint16 Message, uint16 WParam, int32 LParam)
{
	int32 about;
	int32 cnt, Layer;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(384, "Export to DXF format"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(144, "Select objects"));
		SetDialogItemTextUTF8(Dialog, ID_SELECT_ALL, SC(417, "Select all"));
		SetDialogItemTextUTF8(Dialog, ID_UNSELECT_ALL, SC(418, "Unselect all"));

		for (cnt = 0; cnt < NrOutputLayers; cnt++)
		{
			Layer = ExportLayers[cnt];
			SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 1, MAKELPARAM(cnt, cnt + 1));
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case ID_SELECT_ALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 1, MAKELPARAM(0, 100));
			break;

		case ID_UNSELECT_ALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 0, MAKELPARAM(0, 100));
			break;

		case IDOK:
			NrSelectedLayers =
				SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, NrOutputLayers, (LPARAM) & SelectedLayers);

			for (cnt = 0; cnt < NrOutputLayers; cnt++)
				LayerInfo[cnt] &= ~1;

			for (cnt = 0; cnt < NrSelectedLayers; cnt++)
				LayerInfo[SelectedLayers[cnt]] |= 1;

			EndDialog(Dialog, 1);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int WriteInitDXF(int fp, int32 NrLayers)
{
	int32 Layer, ColorNr;
	char str[MAX_LENGTH_STRING];
	double minx, miny, maxx, maxy;

	FindMinMaxDesign(&minx, &miny, &maxx, &maxy);

	DXFminx = minx - 10.0;
	DXFminy = miny - 10.0;
	DXFmaxx = maxx + 10.0;
	DXFmaxy = maxy + 10.0;

	WriteLn(fp, "0");
	WriteLn(fp, "SECTION");
	WriteLn(fp, "2");
	WriteLn(fp, "HEADER");
	WriteLn(fp, "9");
	WriteLn(fp, "$ACADVER");
	WriteLn(fp, "1");
	WriteLn(fp, "AC1006");
	WriteLn(fp, "9");
	WriteLn(fp, "$INSBASE");
	WriteLn(fp, "10");
	WriteLn(fp, "0");
	WriteLn(fp, "20");
	WriteLn(fp, "0");
	WriteLn(fp, "30");
	WriteLn(fp, "0");
	WriteLn(fp, "9");
	WriteLn(fp, "$EXTMIN");
	WriteLn(fp, "10");
	sprintf(str, "%.3f", DXFminx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", DXFminy);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$EXTMAX");
	WriteLn(fp, "10");
	sprintf(str, "%.3f", DXFmaxx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", DXFmaxy);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$LIMMIN");
	WriteLn(fp, "10");
	sprintf(str, "%.3f", DXFminx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", DXFminy);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$LIMMAX");
	WriteLn(fp, "10");
	sprintf(str, "%.3f", DXFmaxx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", DXFmaxy);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$REGENMODE");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$FILLMODE");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$MIRRTEXT");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$DRAGMODE");
	WriteLn(fp, "70");
	WriteLn(fp, "2");
	WriteLn(fp, "9");
	WriteLn(fp, "$LTSCALE");
	WriteLn(fp, "40");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$ATTMODE");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$TEXTSIZE");
	WriteLn(fp, "40");
	WriteLn(fp, "0.2");
	WriteLn(fp, "9");
	WriteLn(fp, "$TRACEWID");
	WriteLn(fp, "40");
	WriteLn(fp, "0.05");
	WriteLn(fp, "9");
	WriteLn(fp, "$TEXTSTYLE");
	WriteLn(fp, "7");
	WriteLn(fp, "STANDARD");
	WriteLn(fp, "9");
	WriteLn(fp, "$CLAYER");
	WriteLn(fp, "8");
	WriteLn(fp, "0");
	WriteLn(fp, "9");
	WriteLn(fp, "$CELTYPE");
	WriteLn(fp, "6");
	WriteLn(fp, "BYLAYER");
	WriteLn(fp, "9");
	WriteLn(fp, "$CECOLOR");
	WriteLn(fp, "62");
	WriteLn(fp, "256");
	WriteLn(fp, "9");
	WriteLn(fp, "$LUNITS");
	WriteLn(fp, "70");
	WriteLn(fp, "2");
	WriteLn(fp, "9");
	WriteLn(fp, "$LUPREC");
	WriteLn(fp, "70");
	WriteLn(fp, "3");
	WriteLn(fp, "9");
	WriteLn(fp, "$AXISUNIT");
	WriteLn(fp, "10");
	WriteLn(fp, "0");
	WriteLn(fp, "20");
	WriteLn(fp, "0");
	WriteLn(fp, "9");
	WriteLn(fp, "$SKETCHINC");
	WriteLn(fp, "40");
	WriteLn(fp, "0.1");
	WriteLn(fp, "9");
	WriteLn(fp, "$MENU");
	WriteLn(fp, "1");
	WriteLn(fp, "acad");
	WriteLn(fp, "9");
	WriteLn(fp, "$LIMCHECK");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$BLIPMODE");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$COORDS");
	WriteLn(fp, "70");
	WriteLn(fp, "2");
	WriteLn(fp, "9");
	WriteLn(fp, "$SPLINETYPE");
	WriteLn(fp, "70");
	WriteLn(fp, "6");
	WriteLn(fp, "9");
	WriteLn(fp, "$SPLINESEGS");
	WriteLn(fp, "70");
	WriteLn(fp, "8");
	WriteLn(fp, "9");
	WriteLn(fp, "$ATTREQ");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "9");
	WriteLn(fp, "$UCSNAME");
	WriteLn(fp, "2");
	WriteLn(fp, "");
	WriteLn(fp, "9");
	WriteLn(fp, "$UCSORG");
	WriteLn(fp, "10");
	WriteLn(fp, "0");
	WriteLn(fp, "20");
	WriteLn(fp, "0");
	WriteLn(fp, "30");
	WriteLn(fp, "0");
	WriteLn(fp, "9");
	WriteLn(fp, "$UCSXDIR");
	WriteLn(fp, "10");
	WriteLn(fp, "1");
	WriteLn(fp, "20");
	WriteLn(fp, "0");
	WriteLn(fp, "30");
	WriteLn(fp, "0");
	WriteLn(fp, "9");
	WriteLn(fp, "$UCSYDIR");
	WriteLn(fp, "10");
	WriteLn(fp, "0");
	WriteLn(fp, "20");
	WriteLn(fp, "1");
	WriteLn(fp, "30");
	WriteLn(fp, "0");
	WriteLn(fp, "9");
	WriteLn(fp, "$WORLDVIEW");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "0");
	WriteLn(fp, "ENDSEC");
	WriteLn(fp, "0");
	WriteLn(fp, "SECTION");
	WriteLn(fp, "2");
	WriteLn(fp, "TABLES");
	WriteLn(fp, "0");
	WriteLn(fp, "TABLE");
	WriteLn(fp, "2");
	WriteLn(fp, "VPORT");
	WriteLn(fp, "70");
	WriteLn(fp, "2");
	WriteLn(fp, "0");
	WriteLn(fp, "VPORT");
	WriteLn(fp, "2");
	WriteLn(fp, "*ACTIVE");
	WriteLn(fp, "70");
	WriteLn(fp, "0");
	WriteLn(fp, "10");
	WriteLn(fp, "0");
	WriteLn(fp, "20");
	WriteLn(fp, "0");
	WriteLn(fp, "11");
	WriteLn(fp, "1");
	WriteLn(fp, "21");
	WriteLn(fp, "1");
	WriteLn(fp, "12");
	WriteLn(fp, "0");
	WriteLn(fp, "22");
	WriteLn(fp, "0");
	WriteLn(fp, "13");
	WriteLn(fp, "0");
	WriteLn(fp, "23");
	WriteLn(fp, "0");
	WriteLn(fp, "14");
	WriteLn(fp, "64");
	WriteLn(fp, "24");
	WriteLn(fp, "64");
	WriteLn(fp, "15");
	WriteLn(fp, "1");			// 1 mm grid
	WriteLn(fp, "25");
	WriteLn(fp, "1");			// 1 mm grid
	WriteLn(fp, "16");
	WriteLn(fp, "0");
	WriteLn(fp, "26");
	WriteLn(fp, "0");
	WriteLn(fp, "36");
	WriteLn(fp, "1");
	WriteLn(fp, "17");
	WriteLn(fp, "0");
	WriteLn(fp, "27");
	WriteLn(fp, "0");
	WriteLn(fp, "37");
	WriteLn(fp, "0");
	WriteLn(fp, "40");
	WriteLn(fp, "300");
	WriteLn(fp, "41");
	WriteLn(fp, "1.6");
	WriteLn(fp, "42");
	WriteLn(fp, "50");
	WriteLn(fp, "43");
	WriteLn(fp, "0");
	WriteLn(fp, "44");
	WriteLn(fp, "0");
	WriteLn(fp, "50");
	WriteLn(fp, "0");
	WriteLn(fp, "51");
	WriteLn(fp, "0");
	WriteLn(fp, "71");
	WriteLn(fp, "0");
	WriteLn(fp, "72");
	WriteLn(fp, "800");			// Circle smoothness
	WriteLn(fp, "73");
	WriteLn(fp, "1");
	WriteLn(fp, "74");
	WriteLn(fp, "1");
	WriteLn(fp, "75");
	WriteLn(fp, "0");			// No snapmode on grid
	WriteLn(fp, "76");
	WriteLn(fp, "1");
	WriteLn(fp, "77");
	WriteLn(fp, "0");
	WriteLn(fp, "78");
	WriteLn(fp, "0");
	WriteLn(fp, "0");
	WriteLn(fp, "ENDTAB");
	WriteLn(fp, "0");
	WriteLn(fp, "TABLE");
	WriteLn(fp, "2");
	WriteLn(fp, "LTYPE");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "0");
	WriteLn(fp, "LTYPE");
	WriteLn(fp, "2");
	WriteLn(fp, "CONTINUOUS");
	WriteLn(fp, "70");
	WriteLn(fp, "64");
	WriteLn(fp, "3");
	WriteLn(fp, "Solidline");
	WriteLn(fp, "72");
	WriteLn(fp, "65");
	WriteLn(fp, "73");
	WriteLn(fp, "0");
	WriteLn(fp, "40");
	WriteLn(fp, "0");
	WriteLn(fp, "0");
	WriteLn(fp, "ENDTAB");
	WriteLn(fp, "0");
	WriteLn(fp, "TABLE");
	WriteLn(fp, "2");
	WriteLn(fp, "LAYER");
	WriteLn(fp, "70");
	WriteLn(fp, "19");
	WriteLn(fp, "0");
	WriteLn(fp, "LAYER");
	WriteLn(fp, "2");
	WriteLn(fp, "0");
	WriteLn(fp, "70");
	WriteLn(fp, "64");
	WriteLn(fp, "62");
	WriteLn(fp, "5");
	WriteLn(fp, "6");
	WriteLn(fp, "CONTINUOUS");

	ColorNr = 1;

	for (Layer = 0; Layer < NrLayers; Layer++)
	{
		WriteLn(fp, "0");
		WriteLn(fp, "LAYER");
		WriteLn(fp, "2");
		WriteLn(fp, DXFLayers[SelectedLayers[Layer]]);
		WriteLn(fp, "70");
		WriteLn(fp, "64");
		WriteLn(fp, "62");
		sprintf(str, "%d", ColorNr);
		WriteLn(fp, str);
		WriteLn(fp, "6");
		WriteLn(fp, "CONTINUOUS");

		ColorNr++;

		if (ColorNr == 7)
			ColorNr += 2;
	}

	WriteLn(fp, "0");
	WriteLn(fp, "ENDTAB");
	WriteLn(fp, "0");
	WriteLn(fp, "TABLE");
	WriteLn(fp, "2");
	WriteLn(fp, "STYLE");
	WriteLn(fp, "70");
	WriteLn(fp, "1");
	WriteLn(fp, "0");
	WriteLn(fp, "STYLE");
	WriteLn(fp, "2");
	WriteLn(fp, "STANDARD");
	WriteLn(fp, "70");
	WriteLn(fp, "0");
	WriteLn(fp, "40");
	WriteLn(fp, "0");
	WriteLn(fp, "41");
	WriteLn(fp, "1");
	WriteLn(fp, "50");
	WriteLn(fp, "0");
	WriteLn(fp, "71");
	WriteLn(fp, "0");
	WriteLn(fp, "42");
	WriteLn(fp, "0.2");
	WriteLn(fp, "3");
	WriteLn(fp, "txt");
	WriteLn(fp, "4");
	WriteLn(fp, "");
	WriteLn(fp, "0");
	WriteLn(fp, "ENDTAB");
	WriteLn(fp, "0");
	WriteLn(fp, "TABLE");
	WriteLn(fp, "2");
	WriteLn(fp, "VIEW");
	WriteLn(fp, "70");
	WriteLn(fp, "0");
	WriteLn(fp, "0");
	WriteLn(fp, "ENDTAB");
	WriteLn(fp, "0");
	WriteLn(fp, "TABLE");
	WriteLn(fp, "2");
	WriteLn(fp, "UCS");
	WriteLn(fp, "70");
	WriteLn(fp, "0");
	WriteLn(fp, "0");
	WriteLn(fp, "ENDTAB");
	WriteLn(fp, "0");
	WriteLn(fp, "TABLE");
	WriteLn(fp, "2");
	WriteLn(fp, "DWGMGR");
	WriteLn(fp, "70");
	WriteLn(fp, "0");
	WriteLn(fp, "0");
	WriteLn(fp, "ENDTAB");
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DXFWriteTrace(double x1, double y1, double x2, double y2, double Thickness, LPSTR DXFLayer, int32 fp, int32 mode)
{
	char str[MAX_LENGTH_STRING];
	double x1a, y1a, x2a, y2a, x3a;
	int32 ok;

	if (Thickness > 20)
		ok = 1;

	if ((DXFOptions & 8) == 8)
	{	// Mirror X
		x1 = -x1 + DXFminx + DXFmaxx;
		x2 = -x2 + DXFminx + DXFmaxx;
	}

	if ((mode & 2) == 0)
	{
		Thickness = 0.0;
		sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 66\r\n      1\r\n 40\r\n%.3f\r\n 41\r\n%.3f\r\n", Thickness, Thickness);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x2, y2);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
	}
	else
	{	// Filled trace
		sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 66\r\n      1\r\n 40\r\n%.3f\r\n 41\r\n%.3f\r\n", Thickness, Thickness);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x2, y2);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);

		if ((mode & 1) == 1)
		{
			x1a = x1;
			y1a = y1;
			x2a = x1a;
			y2a = y1a;
			x1a -= Thickness * 0.25;
			x2a += Thickness * 0.25;
			x3a = Thickness * 0.5;
			sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 66\r\n1\r\n 40\r\n%.3f\r\n 41\r\n%.3f\r\n 70\r\n1\r\n", x3a, x3a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1a, y1a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x2a, y2a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
			WriteToFile(fp, str);
			x1a = x2;
			y1a = y2;
			x2a = x1a;
			y2a = y1a;
			x1a -= Thickness * 0.25;
			x2a += Thickness * 0.25;
			x3a = Thickness * 0.5;
			sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 66\r\n1\r\n 40\r\n%.3f\r\n 41\r\n%.3f\r\n 70\r\n1\r\n", x3a, x3a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1a, y1a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x2a, y2a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
			WriteToFile(fp, str);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DXFWriteCircle(double x1, double y1, double x2, double Thickness, LPSTR DXFLayer, int32 fp, int32 mode)
{
	char str[MAX_LENGTH_STRING];
	double x1a, y1a, x2a, y2a, x3a;
	int32 ok;
	/*
	  mode:

	    0  -> Object not filled
	    2  -> Object filled
	*/

	if ((DXFOptions & 8) == 8)
	{	// Mirror X
		x1 = -x1 + DXFminx + DXFmaxx;
	}

	if (((mode & 2) == 0)		// Open circle
	        || ((DXFOptions & 16) == 0))
	{	// Do not fill object
		sprintf(str, "  0\r\nCIRCLE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1, x2 * 0.5);
		WriteToFile(fp, str);
	}
	else
	{	// Filled circle
		if (Thickness > 20)
			ok = 1;

		x1a = x1;
		y1a = y1;
		x2a = x1a;
		y2a = y1a;
		Thickness = x2;
		x1a -= Thickness * 0.25;
		x2a += Thickness * 0.25;
		x3a = Thickness * 0.5;
		sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 66\r\n1\r\n 40\r\n%.3f\r\n 41\r\n%.3f\r\n 70\r\n1\r\n", x3a, x3a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1a, y1a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x2a, y2a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DXFWriteRectangle(double x1, double y1, double x2, double y2, double Thickness, LPSTR DXFLayer, int32 fp,
                       int32 mode)
/*
  mode:

    0  -> Object not filled
    2  -> Object filled
*/
{
	char str[MAX_LENGTH_STRING];
	double x1a, y1a, x2a, y2a, x3a, y3a, x4a, y4a;
	int32 ok;

	if ((DXFOptions & 8) == 8)
	{	// Mirror X
		x1 = -x1 + DXFminx + DXFmaxx;
	}

	x2 *= 0.5;
	y2 *= 0.5;
	x1a = x1 - x2;
	y1a = y1 - y2;
	x2a = x1 + x2;
	y2a = y1 - y2;
	x3a = x1 + x2;
	y3a = y1 + y2;
	x4a = x1 - x2;
	y4a = y1 + y2;

	if (((mode & 2) == 0)		// Open rectangle
	        || ((DXFOptions & 16) == 0))
	{	// Do not fill object
		if ((DXFOptions & 16) == 0)
		{	// Do not fill object
			Thickness = 0.0;
		}

		if (Thickness > 20)
			ok = 1;

		sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 66\r\n1\r\n 40\r\n%.3f\r\n 41\r\n%.3f\r\n 70\r\n1\r\n", Thickness, Thickness);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1a, y1a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x2a, y2a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x3a, y3a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x4a, y4a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);

	}
	else
	{	// Filled rectangle
		sprintf(str, "  0\r\nTRACE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1a, y1a);
		WriteToFile(fp, str);
		sprintf(str, " 11\r\n%.3f\r\n 21\r\n%.3f\r\n 31\r\n0.0\r\n", x2a, y2a);
		WriteToFile(fp, str);
		sprintf(str, " 12\r\n%.3f\r\n 22\r\n%.3f\r\n 32\r\n0.0\r\n", x4a, y4a);
		WriteToFile(fp, str);
		sprintf(str, " 13\r\n%.3f\r\n 23\r\n%.3f\r\n 33\r\n0.0\r\n", x3a, y3a);
		WriteToFile(fp, str);
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DXFWriteText(double x1, double y1, double Height, double Rotation, int32 TextAlignment, LPSTR DXFLayer, int32 fp,
                  LPSTR TextString)
{
	char str[MAX_LENGTH_STRING];
	double x, y, Size;
	int32 lengte2;

	x = x1;
	y = y1;
	Size = Height * 0.7;
	lengte2 = strlen(TextString);

	if (Rotation == 0.0)
	{
//      y-=Size*0.2*DxfFontSizeMult;
		switch (TextAlignment)
		{
		case ALIGN_LEFT_BOTTOM:
			x += Size * 0.1 * DxfFontSizeMult;
			y += Size * 0.1;
			break;

		case ALIGN_RIGHT_BOTTOM:
			x -= lengte2 * Size * DxfFontSizeMult * DXFCharWidthFactor;
			y += Size * 0.1;
			break;

		case ALIGN_LEFT_CENTRE:
			x += Size * 0.1 * DxfFontSizeMult;
			y -= Size * 0.45 * DxfFontSizeMult;
			break;

		case ALIGN_RIGHT_CENTRE:
			y -= Size * 0.45 * DxfFontSizeMult;
			x -= lengte2 * Size * DxfFontSizeMult * DXFCharWidthFactor;
			break;

		case ALIGN_LEFT_TOP:
			x += Size * 0.1 * DxfFontSizeMult;
			y -= Size * 1.15 * DxfFontSizeMult;
			break;

		case ALIGN_RIGHT_TOP:
			y -= Size * 1.15 * DxfFontSizeMult;
			x -= lengte2 * Size * DxfFontSizeMult * DXFCharWidthFactor;
			break;
		}
	}
	else
	{
		if (InRangeSpecial(Rotation, 90.0, 0.01))
		{
			x += Size * -0.3 * DxfFontSizeMult;

			switch (TextAlignment)
			{
			case ALIGN_LEFT_BOTTOM:
				y += Size * 0.1 * DxfFontSizeMult;
				break;

			case ALIGN_RIGHT_BOTTOM:
				y -= lengte2 * Size * DxfFontSizeMult * DXFCharWidthFactor;
				break;

			case ALIGN_LEFT_CENTRE:
				y += Size * 0.1 * DxfFontSizeMult;
				x += Size * 0.45 * DxfFontSizeMult;
				break;

			case ALIGN_RIGHT_CENTRE:
				x += Size * 0.45 * DxfFontSizeMult;
				y -= lengte2 * Size * DxfFontSizeMult * DXFCharWidthFactor;
				break;

			case ALIGN_LEFT_TOP:
				y += Size * 0.1 * DxfFontSizeMult;
				x += Size * 1.15 * DxfFontSizeMult;
				break;

			case ALIGN_RIGHT_TOP:
				y -= lengte2 * Size * DxfFontSizeMult * DXFCharWidthFactor;
				x += Size * 1.15 * DxfFontSizeMult;
				break;
			}
		}
	}

	sprintf(str, "  0\r\nTEXT\r\n  8\r\n%s\r\n", DXFLayer);
	WriteToFile(fp, str);
	sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x, y, Size);
	WriteToFile(fp, str);
	sprintf(str, "  1\r\n%s\r\n", TextString);
	WriteToFile(fp, str);
	sprintf(str, " 41\r\n0.9\r\n");
	WriteToFile(fp, str);
	sprintf(str, " 50\r\n%.1f\r\n", Rotation);
	WriteToFile(fp, str);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 ExportDXF(int32 Mode)
{
	double x1, y1, x1a, y1a, x2, y2, x3, y3, x4, y4, hulp, Angle1, Angle2, Length2, Thickness, Rotation, x11, y11, x22,
	       y22;
	int32 cnt, cnt2, cnt3, cnt4, cnt5, TextAlignment, BusConnectionType, AddMode, ok, NrLines, lengte, ConnectionType,
	      CircleMode, TextMode, Alignment, fp, Layer, InstanceRotation, TextRotation, MemPos, res, SymbolNr, NrProperties,
	      ObjectMirrorX, ObjectMirrorY;
	char str[MAX_LENGTH_STRING], DXFLayer[MAX_LENGTH_STRING];
	double Angle, Length;
	PointRecord LinePoints[16];
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;
	int32 OkToPrint;
	WireRecord *Wire;
	BusRecord *Bus;
	JunctionRecord *Junction;
	OnePinNetRecord *OnePinNet;
	NetLabelRecord *NetLabel;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectCircleRecord *ObjectCircle;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord *ObjectText;
	char PinBusStrings[16][64], TextStr[1024], TextStr2[1024], ExportFileName[MAX_LENGTH_STRING], NewPinBusStr[512],
	     str2[MAX_LENGTH_STRING];
	int32 LinePos[16];
	LPSTR LabelText;
	InstanceRecord *Instance;
	ObjectRecord *Object;
	SymbolRecord *Symbol;
	SymbolsPosRecord *SymbolPos;

	x11 = 0.0;
	y11 = 0.0;
	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

	NrOutputLayers = 0;

	if (!EditingSymbol)
	{
		ExportLayers[NrOutputLayers] = WIRE_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(185, "Wires"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = BUS_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(204, "Busses"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = JUNCTION_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(299, "Junctions"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = BUSCONNECTIONS_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(318, "Busconnections"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = GLOBALCONNECTIONS_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(334, "Globalconnections"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = ONEPINNET_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(355, "OnePinNets"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = NETLABEL_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(362, "Netlabels"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = SYMBOLS_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(190, "Symbols"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = REFERENCE_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(373, "SymbolReferences"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = VALUE_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(375, "SymbolValues"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = OBJECTS_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(377, "OtherObjects"));
		NrOutputLayers++;
	}
	else
	{
		ExportLayers[NrOutputLayers] = PIN_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(78, "Pins"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = POWERPIN_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(408, "Powerpins"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = PINBUS_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(413, "Pinbusses"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = REFERENCE_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(373, "SymbolReferences"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = VALUE_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(375, "SymbolValues"));
		NrOutputLayers++;
		ExportLayers[NrOutputLayers] = OBJECTS_LAYER;
		strcpy(DXFLayers[NrOutputLayers], SC(377, "OtherObjects"));
		NrOutputLayers++;
	}

	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_EXPORT_DXF), SCHWindow, (DLGPROC) DxfDialog2);

	if (res == 2)
		return -1;

	if (!EditingSymbol)
	{
		GetFilePartFromFileName(str, EditFile);
		CutExtensionFileName(str);
		sprintf(ExportFileName, "%s\\sch\\%s.dxf", DesignPath, str);
	}
	else
	{
		strcpy(str, EditFile);
		CutExtensionFileName(str);
		sprintf(ExportFileName, "%s.dxf", str);
	}

	if (FileExistsUTF8(ExportFileName) == 0)
	{
		sprintf(str2, SC(414, "File %s exist overwrite ?"), ExportFileName);

		if (MessageBoxUTF8(SCHWindow, str2, SC(20, "Message"), MB_OKCANCEL | MB_APPLMODAL) != IDOK)
			return -1;
	}

	if ((fp = FileOpenWriteUTF8(ExportFileName)) <= 0)
		return -1;

	WriteInitDXF(fp, NrOutputLayers);

	WriteToFile(fp, "  0\r\n");
	WriteToFile(fp, "ENDSEC\r\n");
	WriteToFile(fp, "  0\r\nSECTION\r\n  2\r\nENTITIES\r\n");

	for (cnt4 = 0; cnt4 < NrOutputLayers; cnt4++)
	{
		Layer = ExportLayers[cnt4];
		strcpy(DXFLayer, DXFLayers[cnt4]);

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == WIRE_LAYER)
		{
			for (cnt = 0; cnt < Design.NrWires; cnt++)
			{
				Wire = &((*Wires)[cnt]);

				if ((Wire->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Wire->X1;
					y1 = Wire->Y1;
					x2 = Wire->X2;
					y2 = Wire->Y2;
					DXFWriteTrace(x1, y1, x2, y2, 0.0, DXFLayer, fp, 0);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == BUS_LAYER)
		{
			for (cnt = 0; cnt < Design.NrBusses; cnt++)
			{
				Bus = &((*Busses)[cnt]);

				if ((Bus->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Bus->X1;
					y1 = Bus->Y1;
					x2 = Bus->X2;
					y2 = Bus->Y2;
					DXFWriteTrace(x1, y1, x2, y2, 0.3, DXFLayer, fp, 2);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == JUNCTION_LAYER)
		{
			for (cnt = 0; cnt < Design.NrJunctions; cnt++)
			{
				Junction = &((*Junctions)[cnt]);

				if ((Junction->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Junction->X;
					y1 = Junction->Y;
					Thickness = 0.5;
					DXFWriteCircle(x1, y1, Thickness, 0.0, DXFLayer, fp, 0);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == ONEPINNET_LAYER)
		{
			for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
			{
				OnePinNet = &((*OnePinNets)[cnt]);

				if ((OnePinNet->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					Thickness = 0.4;
					x1 = OnePinNet->X - Thickness;
					y1 = OnePinNet->Y - Thickness;
					x2 = OnePinNet->X + Thickness;
					y2 = OnePinNet->Y + Thickness;
					DXFWriteTrace(x1, y1, x2, y2, 0.0, DXFLayer, fp, 0);
					DXFWriteTrace(x1, y2, x2, y1, 0.0, DXFLayer, fp, 0);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == NETLABEL_LAYER)
		{
			for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
			{
				NetLabel = &((*NetLabels)[cnt]);

				if ((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = NetLabel->ConnectX;
					y1 = NetLabel->ConnectY;
					x2 = x1 + NetLabel->TextX;
					y2 = y1 + NetLabel->TextY;
					TextAlignment = (NetLabel->Alignment & 0x0f);
					TextRotation = (NetLabel->Alignment >> 8) & 0x01;
					NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1);

					if (NrProperties > 0)
						sprintf(str, "%s (...)", NetLabel->Name);
					else
						strcpy(str, NetLabel->Name);

					DXFWriteText(x2, y2, 1.0, TextRotation * 90.0, TextAlignment, DXFLayer, fp, str);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == BUSCONNECTIONS_LAYER)
		{
			for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
			{
				BusConnection = &((*BusConnections)[cnt]);

				if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					BusConnectionType = (BusConnection->Alignment >> 14) & 3;
					x1 = BusConnection->X;
					y1 = BusConnection->Y;

					switch (BusConnectionType)
					{
					case 0:	/*  \    */
						DXFWriteRectangle(x1 - BusSizeX * 0.75, y1, BusSizeX * 0.5, BusSizeY, 0.0, DXFLayer, fp, 0);
						x11 = x1 - BusSizeX * 0.5;
						y11 = y1;
						break;

					case 1:	/* rotate 90   */
						/*  /\         */
						/*             */
						DXFWriteRectangle(x1, y1 - BusSizeX * 0.75, BusSizeY, BusSizeX * 0.5, 0.0, DXFLayer, fp, 0);
						x11 = x1;
						y11 = y1 - BusSizeX * 0.5;
						break;

					case 2:	/* rotate 180  */
						/*  /          */
						/*  \          */
						DXFWriteRectangle(x1 + BusSizeX * 0.75, y1, BusSizeX * 0.5, BusSizeY, 0.0, DXFLayer, fp, 0);
						x11 = x1 + BusSizeX * 0.5;
						y11 = y1;
						break;

					case 3:	/* rotate 270  */
						/*             */
						/*  \/         */
						DXFWriteRectangle(x1 + BusSizeX * 0.75, y1, BusSizeX * 0.5, BusSizeY, 0.0, DXFLayer, fp, 0);
						x11 = x1 + BusSizeX * 0.5;
						y11 = y1;
						break;
					}

					x22 = x1;
					y22 = y1;
					DXFWriteTrace(x11, y11, x22, y22, 0.0, DXFLayer, fp, 0);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == GLOBALCONNECTIONS_LAYER)
		{
			for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
			{
				GlobalConnection = &((*GlobalConnections)[cnt]);

				if ((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == 0)
				{
					ConnectionType = GlobalConnection->ConnectionType;
					x1 = GlobalConnection->X;
					y1 = GlobalConnection->Y;
					x1a = GlobalConnection->NameX;
					y1a = GlobalConnection->NameY;

					switch (ConnectionType >> 1)
					{
					case 0:	// input
						LinePoints[0].x = 0.0;
						LinePoints[0].y = 0.0;
						LinePoints[1].x = -0.8;
						LinePoints[1].y = -0.4;
						LinePoints[2].x = -0.8;
						LinePoints[2].y = 0.4;

						if ((ConnectionType & 1) == 1)
						{
							LinePoints[0].x *= -1;
							LinePoints[1].x *= -1;
							LinePoints[2].x *= -1;
						}

						sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

						for (cnt3 = 0; cnt3 < 3; cnt3++)
						{
							x1 = LinePoints[cnt3].x + x1;
							y1 = LinePoints[cnt3].y + y1;

							if ((DXFOptions & 8) == 8)
							{	// Mirror X
								x1 = -x1 + DXFminx + DXFmaxx;
							}

							sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
							WriteToFile(fp, str);
						}

						sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						break;

					case 1:	// output
						LinePoints[0].x = 0.0;
						LinePoints[0].y = -0.4;
						LinePoints[1].x = 0.0;
						LinePoints[1].y = 0.4;
						LinePoints[2].x = -0.8;
						LinePoints[2].y = 0.0;

						if ((ConnectionType & 1) == 1)
						{
							LinePoints[0].x *= -1;
							LinePoints[1].x *= -1;
							LinePoints[2].x *= -1;
						}

						sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

						for (cnt3 = 0; cnt3 < 3; cnt3++)
						{
							x1 = LinePoints[cnt3].x + x1;
							y1 = LinePoints[cnt3].y + y1;

							if ((DXFOptions & 8) == 8)
							{	// Mirror X
								x1 = -x1 + DXFminx + DXFmaxx;
							}

							sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
							WriteToFile(fp, str);
						}

						sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						break;

					case 2:	// input/output
						LinePoints[0].x = 0.0;
						LinePoints[0].y = 0.0;
						LinePoints[1].x = -0.8;
						LinePoints[1].y = -0.4;
						LinePoints[2].x = -0.8;
						LinePoints[2].y = 0.4;

						if ((ConnectionType & 1) == 1)
						{
							LinePoints[0].x *= -1;
							LinePoints[1].x *= -1;
							LinePoints[2].x *= -1;
						}

						sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

						for (cnt3 = 0; cnt3 < 3; cnt3++)
						{
							x1 = LinePoints[cnt3].x + x1;
							y1 = LinePoints[cnt3].y + y1;

							if ((DXFOptions & 8) == 8)
							{	// Mirror X
								x1 = -x1 + DXFminx + DXFmaxx;
							}

							sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
							WriteToFile(fp, str);
						}

						sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);

						LinePoints[0].x = -2.0;
						LinePoints[0].y = -0.4;
						LinePoints[1].x = -2.0;
						LinePoints[1].y = 0.4;
						LinePoints[2].x = -2.8;
						LinePoints[2].y = 0.0;

						if ((ConnectionType & 1) == 1)
						{
							LinePoints[0].x *= -1;
							LinePoints[1].x *= -1;
							LinePoints[2].x *= -1;
						}

						sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

						for (cnt3 = 0; cnt3 < 3; cnt3++)
						{
							x1 = LinePoints[cnt3].x + x1;
							y1 = LinePoints[cnt3].y + y1;

							if ((DXFOptions & 8) == 8)
							{	// Mirror X
								x1 = -x1 + DXFminx + DXFmaxx;
							}

							sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
							WriteToFile(fp, str);
						}

						sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);

						if ((ConnectionType & 1) == 0)
						{
							x11 = x1 - 2.0;
							y11 = y1;
							x22 = x1 - 0.8;
							y22 = y1;
						}
						else
						{
							x11 = x1 + 2.0;
							y11 = y1;
							x22 = x1 + 0.8;
							y22 = y1;
						}

						DXFWriteTrace(x11, y11, x22, y22, 0.0, DXFLayer, fp, 0);
						break;
					}

					DXFWriteText(x1a, y1a, 1.0, 0, (GlobalConnection->NameInfo & 15), DXFLayer, fp,
					             GlobalConnection->Text);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == OBJECTS_LAYER)
		{
			for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
			{
				ObjectLine = &((*ObjectLines)[cnt]);

				if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x1 = ObjectLine->X1;
					y1 = ObjectLine->Y1;
					x2 = ObjectLine->X2;
					y2 = ObjectLine->Y2;
					DXFWriteTrace(x1, y1, x2, y2, 0.0, DXFLayer, fp, 0);

					if (ObjectLine->LineMode != 0)
					{
						ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

						if ((ObjectLine->LineMode & 1) == 1)
						{
							x3 = x1 + cos(Angle) * ARROW_LENGTH;
							y3 = y1 + sin(Angle) * ARROW_LENGTH;
							x4 = x3;
							y4 = y3;
							RotatePointFromOtherPoint(&x4, &y4, x1, y1, 30.0);
							DXFWriteTrace(x1, y1, x4, y4, 0.0, DXFLayer, fp, 0);
							x4 = x3;
							y4 = y3;
							RotatePointFromOtherPoint(&x4, &y4, x1, y1, -30.0);
							DXFWriteTrace(x1, y1, x4, y4, 0.0, DXFLayer, fp, 0);
						}

						if ((ObjectLine->LineMode & 2) == 2)
						{
							x3 = x2 - cos(Angle) * ARROW_LENGTH;
							y3 = y2 - sin(Angle) * ARROW_LENGTH;
							x4 = x3;
							y4 = y3;
							RotatePointFromOtherPoint(&x4, &y4, x2, y2, 30.0);
							DXFWriteTrace(x2, y2, x4, y4, 0.0, DXFLayer, fp, 0);
							x4 = x3;
							y4 = y3;
							RotatePointFromOtherPoint(&x4, &y4, x2, y2, -30.0);
							DXFWriteTrace(x2, y2, x4, y4, 0.0, DXFLayer, fp, 0);
						}
					}
				}
			}

// ****************************************************************************************************
// ****************************************************************************************************

			for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
			{
				ObjectRect = &((*ObjectRects)[cnt]);

				if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x1 = ObjectRect->CentreX;
					y1 = ObjectRect->CentreY;
					x2 = ObjectRect->Width;
					y2 = ObjectRect->Height;

					if (ObjectRect->Thickness == 0.0)
						DXFWriteRectangle(x1, y1, x2, y2, 0.0, DXFLayer, fp, 2);
					else
						DXFWriteRectangle(x1, y1, x2, y2, 0.0, DXFLayer, fp, 0);
				}
			}

// ****************************************************************************************************
// ****************************************************************************************************
			for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
			{
				ObjectCircle = &((*ObjectCircles)[cnt]);

				if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x1 = ObjectCircle->CentreX;
					y1 = ObjectCircle->CentreY;
					x2 = ObjectCircle->Diam;

					if (ObjectCircle->Thickness == 0.0)
						DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
					else
					{
						switch (ObjectCircle->CircleMode)
						{
						case 1:
							Angle1 = 0.0;
							Angle2 = 90.0;
							break;

						case 2:
							Angle1 = 90.0;
							Angle2 = 180.0;
							break;

						case 3:
							Angle1 = 0.0;
							Angle2 = 180.0;
							break;

						case 4:
							Angle1 = 180.0;
							Angle2 = 270.0;
							break;

						case 6:
							Angle1 = 90.0;
							Angle2 = 270.0;
							break;

						case 8:
							Angle1 = 270.0;
							Angle2 = 0.0;
							break;

						case 9:
							Angle1 = 270.0;
							Angle2 = 90.0;
							break;

						case 12:
							Angle1 = 180.0;
							Angle2 = 0.0;
							break;

						default:
							Angle1 = 90.0;
							Angle2 = 90.0;
							break;
						}

						if (ObjectCircle->CircleMode == 15)
							DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 0);
						else
						{
							sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1,
							        x2 * 0.5);
							WriteToFile(fp, str);
							sprintf(str, " 50\r\n%.3f\r\n 51\r\n%.3f\r\n", Angle1, Angle2);
							WriteToFile(fp, str);
						}
					}
				}
			}

// ****************************************************************************************************
// ****************************************************************************************************

			for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x1 = ObjectArc->CentreX;
					y1 = ObjectArc->CentreY;
					x2 = ObjectArc->Width;
					y2 = ObjectArc->Height;
					x3 = ObjectArc->StartDiffX;
					y3 = ObjectArc->StartDiffY;
					x4 = ObjectArc->EndDiffX;
					y4 = ObjectArc->EndDiffY;
					ConvertPointToPolar(x3, y3, &Length2, &Angle1);
					ConvertPointToPolar(x4, y4, &Length2, &Angle2);
					sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);
					sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1, x2 * 0.5);
					WriteToFile(fp, str);
					sprintf(str, " 50\r\n%.3f\r\n 51\r\n%.3f\r\n", Angle1, Angle2);
					WriteToFile(fp, str);
				}
			}

// ****************************************************************************************************
// ****************************************************************************************************
			for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
			{
				ObjectText = &((*ObjectTexts)[cnt]);

				if ((ObjectText->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x1 = ObjectText->X;
					y1 = ObjectText->Y;
					x2 = ObjectText->FontHeight;
					TextMode = ObjectText->TextMode;
					Rotation = ObjectText->Rotation;
					TextAlignment = (TextMode & 0x0f);

					if (strlen(ObjectText->Text) > 0)
					{
						if (ConvertTextString(ObjectText->Text, TextStr) == -1)
							strcpy(TextStr, ObjectText->Text);

						Length = strlen(TextStr);
						cnt3 = 0;
						cnt2 = cnt3;

						while (cnt3 < Length + 1)
						{
							if ((TextStr[cnt3] == '\r') || ((cnt3 == Length) && (TextStr[cnt3 - 1] != '\n')))
							{
								if (cnt3 - cnt2 > 0)
								{
									memset(TextStr2, 0, sizeof(TextStr2));
									strncpy(TextStr2, (LPSTR) & TextStr[cnt2], min(127, cnt3 - cnt2));
									DXFWriteText(x1, y1, x2, Rotation, TextAlignment, DXFLayer, fp, TextStr2);
								}

								x1 += sin(ANGLE_CONVERT(Rotation)) * x2;
								y1 -= cos(ANGLE_CONVERT(Rotation)) * x2;
								cnt3 += 1;
								cnt2 = cnt3 + 1;
							}

							cnt3++;
						}
					}
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == PIN_LAYER)
		{
			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if ((Pin->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = Pin->X;
					x1a = Pin->NameX;
					y1 = Pin->Y;
					y1a = Pin->NameY;
					LinePoints[0].x = 0.25;
					LinePoints[0].y = 0.0;
					LinePoints[1].x = 0.0;
					LinePoints[1].y = 0.25;
					LinePoints[2].x = -0.25;
					LinePoints[2].y = 0.0;
					LinePoints[3].x = 0.0;
					LinePoints[3].y = -0.25;

					sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);
					WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

					for (cnt3 = 0; cnt3 < 4; cnt3++)
					{
						x11 = LinePoints[cnt3].x + x1;
						y11 = LinePoints[cnt3].y + y1;

						if ((DXFOptions & 8) == 8)
						{	// Mirror X
							x11 = -x11 + DXFminx + DXFmaxx;
						}

						sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x11, y11);
						WriteToFile(fp, str);
					}

					sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);
					TextRotation = (Pin->NameInfo >> 8) & 1;

					if (!EditingSheetSymbol)
						DXFWriteText(x1a, y1a, 1.0, TextRotation * 90.0, (Pin->NameInfo & 15), DXFLayer, fp, Pin->Name);
					else
					{
						DXFWriteText(x1a, y1a, 1.0, TextRotation * 90.0, (Pin->NameInfo & 15), DXFLayer, fp,
						             Pin->Label);
					}
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == POWERPIN_LAYER)
		{
			for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
			{
				PowerPin = &((*PowerPins)[cnt]);

				if ((PowerPin->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					x1 = PowerPin->NameX;
					y1 = PowerPin->NameY;
					x2 = 1.0;
					strcpy(str, PowerPin->NetName);
					strcat(str, " : ");
					strcat(str, PowerPin->Text);
					TextRotation = (PowerPin->NameInfo >> 8) & 1;
					TextAlignment = PowerPin->NameInfo & 0x0f;
					DXFWriteText(x1, y1, x2, TextRotation * 90.0, TextAlignment, DXFLayer, fp, str);
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************
		if (Layer == PINBUS_LAYER)
		{
			for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
			{
				PinBus = &((*PinBusses)[cnt]);

				if ((PinBus->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					x1 = PinBus->X;
					x1a = PinBus->NameX;
					y1 = PinBus->Y;
					y1a = PinBus->NameY;
					x2 = 1.0;
					LabelText = (PinBus->Label);

#ifdef _DEBUG

					if (stricmpUTF8(LabelText, "DQ[56:63]") == 0)
						ok = 1;

#endif
					LinePoints[0].x = 0.25;
					LinePoints[0].y = 0.0;
					LinePoints[1].x = 0.0;
					LinePoints[1].y = 0.25;
					LinePoints[2].x = -0.25;
					LinePoints[2].y = 0.0;
					LinePoints[3].x = 0.0;
					LinePoints[3].y = -0.25;

					sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);
					WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

					for (cnt3 = 0; cnt3 < 4; cnt3++)
					{
						x11 = LinePoints[cnt3].x + x1;
						y11 = LinePoints[cnt3].y + y1;

						if ((DXFOptions & 8) == 8)
						{	// Mirror X
							x11 = -x11 + DXFminx + DXFmaxx;
						}

						sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x11, y11);
						WriteToFile(fp, str);
					}

					sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);

					cnt2 = 0;
					NrLines = 0;
					lengte = strlen(PinBus->Text);
					memset(LinePos, 0, sizeof(LinePos));
					memset(PinBusStrings, 0, sizeof(PinBusStrings));
					LinePos[0] = 0;

					while (cnt2 < lengte)
					{
						if (PinBus->Text[cnt2] == '\\')
						{
							memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]],
							        (int) (cnt2 - LinePos[NrLines]));
							NrLines++;
							LinePos[NrLines] = cnt2 + 1;
						}

						cnt2++;
					}

					if (PinBus->Text[lengte - 1] != '\\')
					{
						memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]],
						        (int) (cnt2 - LinePos[NrLines]));
						LinePos[NrLines] = cnt2;
						NrLines++;
					}

					x3 = x1a;
					y3 = y1a;
					TextRotation = (PinBus->NameInfo >> 8) & 1;
					TextAlignment = PinBus->NameInfo & 0x0f;
					AddMode = 0;

					if (TextRotation)
						AddMode += 4;

					if ((TextAlignment == 6) || (TextAlignment == 8))
						AddMode += 1;	// Mirror X

					if ((TextAlignment == 2) || (TextAlignment == 8))
						AddMode += 2;	// Mirror Y


					switch (AddMode)
					{
					case 2:	// Rotation = 0 , MirrorY = 1 , MirrorX = 0
					case 3:	// Rotation = 0 , MirrorY = 1 , MirrorX = 1
						y3 += (NrLines - 1);
						break;

					case 6:	// Rotation = 1 , MirrorY = 1 , MirrorX = 0
					case 7:	// Rotation = 1 , MirrorY = 1 , MirrorX = 1
						x3 -= (NrLines - 1);
						break;
					}

					for (cnt2 = 0; cnt2 < NrLines; cnt2++)
					{
						//        GetMinMaxText(xx3,yy3,x2,0,TextRotation,TextAlignment,PinBusStrings[cnt2]);
						DXFWriteText(x3, y3, x2, TextRotation * 90.0, TextAlignment, DXFLayer, fp, PinBusStrings[cnt2]);

						switch (AddMode)
						{
						case 0:	// Rotation = 0 , MirrorY = 0 , MirrorX = 0
						case 1:	// Rotation = 0 , MirrorY = 0 , MirrorX = 1
						case 2:	// Rotation = 0 , MirrorY = 1 , MirrorX = 0
						case 3:	// Rotation = 0 , MirrorY = 1 , MirrorX = 1
							y3 -= 1.0;
							break;

						case 4:	// Rotation = 1 , MirrorY = 0 , MirrorX = 0
						case 5:	// Rotation = 1 , MirrorY = 0 , MirrorX = 1
						case 6:	// Rotation = 1 , MirrorY = 1 , MirrorX = 0
						case 7:	// Rotation = 1 , MirrorY = 1 , MirrorX = 1
							x3 += 1.0;
							break;
						}
					}
				}
			}
		}

// ****************************************************************************************************
// ****************************************************************************************************

		for (cnt = 0; cnt < min(MaxNrInstances, Design.NrInstances); cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
			{

// ****************************************************************************************************

				if (Layer == REFERENCE_LAYER)
				{
					OkToPrint = 1;

					if ((!EditingSymbol)
					        && (((Instance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
					            || ((Instance->Info & SHEET_SYMBOL) == SHEET_SYMBOL)))
						OkToPrint = 0;
					else
					{
						strcpy(str, Instance->Reference);

						if (!EditingSymbol)
						{
							SymbolNr = -1;

							for (cnt2 = 0; cnt2 < Design.NrSymbols; cnt2++)
							{
								SymbolPos = &((*SymbolsPos)[cnt2]);

								if (stricmpUTF8(SymbolPos->SymbolName, Instance->SymbolName) == 0)
									SymbolNr = cnt2;
							}

							if (SymbolNr == -1)
								OkToPrint = 0;
							else
							{
								MemPos = (*SymbolsPos)[SymbolNr].Pos;

								if (MemPos == -1)
									OkToPrint = 0;
								else
								{
									Symbol = (SymbolRecord *) & (SymbolsMem[MemPos]);

									if ((Symbol->Info & MULTIPLE_SYMBOLS) == 0)
									{
										if (Symbol->NrPartsPerPackage > 1)
										{
											strcat(str, " ");
											str[strlen(str) - 1] = (char) ('A' - 1 + Instance->PackagePartNr);
										}
									}
								}
							}
						}
					}

					if (OkToPrint)
					{
#ifdef _DEBUG

						if (stricmpUTF8(str, "U103") == 0)
							res = 1;

#endif
						InstanceRotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
						ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
						ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);
						TextRotation = (((Instance->RefInfo >> 8) & 1) + InstanceRotation) & 1;
						Alignment = Instance->RefInfo & 0x0f;
						x1 = Instance->RefOriginX;
						y1 = Instance->RefOriginY;

						x2 = Instance->OriginX;
						y2 = Instance->OriginY;

						switch (InstanceRotation)
						{
						case 0:
							if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
							{
								if (ObjectMirrorX == 1)
								{
									x1 = -x1;

									if (TextRotation == 0)
										Alignment = TextMirrorX[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorY[Alignment];
								}

								if (ObjectMirrorY == 1)
								{
									if (TextRotation == 0)
										Alignment = TextMirrorY[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorX[Alignment];

									y1 = -y1;
								}
							}
							else
							{
								//        if (TextRotation==0) Alignment=TextMirrorX[Alignment];
								//        if (TextRotation==1) Alignment=TextMirrorX[Alignment];
							}

							break;

						case 1:	//  90
							hulp = x1;
							x1 = -y1;
							y1 = hulp;

							if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
							{
								if (ObjectMirrorX == 1)
								{
									x1 = -x1;

									if (TextRotation == 0)
										Alignment = TextMirrorX[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorY[Alignment];
								}

								if (ObjectMirrorY == 1)
								{
									y1 = -y1;

									if (TextRotation == 0)
										Alignment = TextMirrorY[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorX[Alignment];
								}
							}
							else
							{
								if (TextRotation == 0)
									Alignment = TextMirrorX[Alignment];

								if (TextRotation == 1)
									Alignment = TextMirrorY[Alignment];
							}

							break;
						}

						x1 += Instance->OriginX;
						y1 += Instance->OriginY;
						DXFWriteText(x1, y1, 0.9, TextRotation * 90.0, Alignment, DXFLayer, fp, str);
					}
				}

// ****************************************************************************************************

				if (Layer == VALUE_LAYER)
				{
					if ((!EditingSymbol) && ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE))
					{
					}
					else
					{
						InstanceRotation = (Instance->SymbolInfo) & OBJECT_ROTATE90;
						ObjectMirrorX = ((Instance->SymbolInfo & OBJECT_MIRRORX) >> 4);
						ObjectMirrorY = ((Instance->SymbolInfo & OBJECT_MIRRORY) >> 5);
						TextRotation = (((Instance->ValueInfo >> 8) & 1) + InstanceRotation) & 1;
						Alignment = Instance->ValueInfo & 0x0f;
						x1 = Instance->ValueOriginX;
						y1 = Instance->ValueOriginY;
						x1a = x1;
						y1a = y1;

						switch (InstanceRotation)
						{
						case 0:
							if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
							{
								if (ObjectMirrorX == 1)
								{
									x1 = -x1;
									x1a = -x1a;

									if (TextRotation == 0)
										Alignment = TextMirrorX[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorY[Alignment];
								}

								if (ObjectMirrorY == 1)
								{
									if (TextRotation == 0)
										Alignment = TextMirrorY[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorX[Alignment];

									y1 = -y1;
									y1a = -y1a;
								}
							}
							else
							{
								//        if (TextRotation==0) Alignment=TextMirrorX[Alignment];
								//        if (TextRotation==1) Alignment=TextMirrorX[Alignment];
							}

							break;

						case 1:	//  90
							hulp = x1;
							x1 = -y1;
							y1 = hulp;
							hulp = x1a;
							x1a = -y1a;
							y1a = hulp;

							if ((ObjectMirrorX == 1) || (ObjectMirrorY == 1))
							{
								if (ObjectMirrorX == 1)
								{
									x1 = -x1;
									x1a = -x1a;

									if (TextRotation == 0)
										Alignment = TextMirrorX[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorY[Alignment];
								}

								if (ObjectMirrorY == 1)
								{
									y1 = -y1;
									y1a = -y1a;

									if (TextRotation == 0)
										Alignment = TextMirrorY[Alignment];

									if (TextRotation == 1)
										Alignment = TextMirrorX[Alignment];
								}
							}
							else
							{
								if (TextRotation == 0)
									Alignment = TextMirrorX[Alignment];

								if (TextRotation == 1)
									Alignment = TextMirrorY[Alignment];
							}

							break;
						}

						x1 += Instance->OriginX;
						y1 += Instance->OriginY;
						strcpy(str, Instance->Value);
						/*
						            if ((!EditingSymbol)
						               &&
						               (Instance->PlacingOption!=-1)) strcat(str,"(*)");
						*/
						DXFWriteText(x1, y1, 0.9, TextRotation * 90.0, Alignment, DXFLayer, fp, str);

						if (Instance->PlacingOption != -1)
						{
							x1a += Instance->OriginX;
							y1a += Instance->OriginY;
							strcpy(str, "NP");
							DXFWriteText(x1a, y1a, 0.9, TextRotation * 90.0, Alignment, DXFLayer, fp, str);
						}
					}
				}

// ****************************************************************************************************

				if (Layer == SYMBOLS_LAYER)
				{
					NrObjects = 0;
					InstanceToObject(Instance, 0.0, 0.0, 0);

					for (cnt5 = 0; cnt5 < NrObjects; cnt5++)
					{
						Object = &((*Objects)[cnt5]);
						x1 = Object->x1;
						y1 = Object->y1;
						x2 = Object->x2;
						y2 = Object->y2;

						switch (Object->ObjectType)
						{
						case SYMBOL_LINE:
							DXFWriteTrace(x1, y1, x2, y2, 0.0, DXFLayer, fp, 0);

							if (Object->Info3 != 0)
							{
								ConvNormalCoorToPolar(x1, y1, x2, y2, &Angle, &Length);

								if ((Object->Info3 & 1) == 1)
								{
									x3 = x1 + cos(Angle) * ARROW_LENGTH;
									y3 = y1 + sin(Angle) * ARROW_LENGTH;
									x4 = x3;
									y4 = y3;
									RotatePointFromOtherPoint(&x4, &y4, x1, y1, 30.0);
									DXFWriteTrace(x1, y1, x4, y4, 0.0, DXFLayer, fp, 0);
									x4 = x3;
									y4 = y3;
									RotatePointFromOtherPoint(&x4, &y4, x1, y1, -30.0);
									DXFWriteTrace(x1, y1, x4, y4, 0.0, DXFLayer, fp, 0);
								}

								if ((Object->Info3 & 2) == 2)
								{
									x3 = x2 - cos(Angle) * ARROW_LENGTH;
									y3 = y2 - sin(Angle) * ARROW_LENGTH;
									x4 = x3;
									y4 = y3;
									RotatePointFromOtherPoint(&x4, &y4, x2, y2, 30.0);
									DXFWriteTrace(x2, y2, x4, y4, 0.0, DXFLayer, fp, 0);
									x4 = x3;
									y4 = y3;
									RotatePointFromOtherPoint(&x4, &y4, x2, y2, -30.0);
									DXFWriteTrace(x2, y2, x4, y4, 0.0, DXFLayer, fp, 0);
								}
							}

							break;

						case SYMBOL_RECT:
							if (Object->Thickness == 0.0)
								DXFWriteRectangle(x1, y1, x2, y2, 0.0, DXFLayer, fp, 2);
							else
								DXFWriteRectangle(x1, y1, x2, y2, 0.0, DXFLayer, fp, 0);

							break;

						case SYMBOL_CIRCLE:
							if (Object->Thickness == 0.0)
								DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
							else
							{
								CircleMode =
								    ((Object->Info2 & 0x80) >> 4) + ((Object->Info2 & 0x20) >> 3) +
								    ((Object->Info2 & 0x08) >> 2) + ((Object->Info2 & 0x02) >> 1);

								switch (CircleMode)
								{
								case 1:
									Angle1 = 0.0;
									Angle2 = 90.0;
									break;

								case 2:
									Angle1 = 270.0;
									Angle2 = 0.0;
									break;

								case 3:
									Angle1 = 270.0;
									Angle2 = 90.0;
									break;

								case 4:
									Angle1 = 180.0;
									Angle2 = 270.0;
									break;

								case 6:
									Angle1 = 180.0;
									Angle2 = 0.0;
									break;

								case 8:
									Angle1 = 90.0;
									Angle2 = 180.0;
									break;

								case 9:
									Angle1 = 0.0;
									Angle2 = 180.0;
									break;

								case 12:
									Angle1 = 90.0;
									Angle2 = 270.0;
									break;

								default:
									Angle1 = 90.0;
									Angle2 = 90.0;
									break;
								}

								if (CircleMode == 15)
									DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 0);
								else
								{
									sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
									sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1,
									        x2 * 0.5);
									WriteToFile(fp, str);
									sprintf(str, " 50\r\n%.3f\r\n 51\r\n%.3f\r\n", Angle1, Angle2);
									WriteToFile(fp, str);
								}
							}

							break;

						case SYMBOL_ARC:
							x3 = Object->x3;
							y3 = Object->y3;
							x4 = Object->x4;
							y4 = Object->y4;
							ConvertPointToPolar(x3, y3, &Length2, &Angle1);
							ConvertPointToPolar(x4, y4, &Length2, &Angle2);
							sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1,
							        x2 * 0.5);
							WriteToFile(fp, str);
							sprintf(str, " 50\r\n%.3f\r\n 51\r\n%.3f\r\n", Angle1, Angle2);
							WriteToFile(fp, str);
							break;

						case SYMBOL_TEXT:
							TextAlignment = (Object->Info2 & 0x0f);
							TextRotation = (Object->Info2 >> 4) & 3;

							if (ConvertTextString(Object->Text1, TextStr) == -1)
								strcpy(TextStr, Object->Text1);

							Rotation = TextRotation * 90.0;
							Length = strlen(TextStr);
							cnt3 = 0;
							cnt2 = cnt3;

							while (cnt3 < Length + 1)
							{
								if ((TextStr[cnt3] == '\r') || ((cnt3 == Length) && (TextStr[cnt3 - 1] != '\n')))
								{
									if (cnt3 - cnt2 > 0)
									{
										memset(TextStr2, 0, sizeof(TextStr2));
										strncpy(TextStr2, (LPSTR) & TextStr[cnt2], min(127, cnt3 - cnt2));
										DXFWriteText(x1, y1, x2, Rotation, TextAlignment, DXFLayer, fp, TextStr2);
									}

									x1 += sin(ANGLE_CONVERT(Rotation)) * x2;
									y1 -= cos(ANGLE_CONVERT(Rotation)) * x2;
									cnt3 += 1;
									cnt2 = cnt3 + 1;
								}

								cnt3++;
							}

							break;

						case SYMBOL_PIN_TEXT:
							TextAlignment = (Object->Info2 & 0x0f);
							TextRotation = (Object->Info2 >> 4) & 3;

							if (InRange(x2, 1.0))
								x2 = 0.9;

							DXFWriteText(x1, y1, x2, TextRotation * 90.0, TextAlignment, DXFLayer, fp, Object->Text1);
							break;

						case SYMBOL_POWERPIN_TEXT:
							TextAlignment = (Object->Info2 & 0x0f);
							TextRotation = (Object->Info2 >> 4) & 3;
							strcpy(str, Object->Text1);
							strcat(str, " : ");
							strcat(str, Object->Text2);

							if (InRange(x2, 1.0))
								x2 = 0.9;

							DXFWriteText(x1, y1, x2, TextRotation * 90.0, TextAlignment, DXFLayer, fp, str);
							break;

						case SYMBOL_PINBUS_TEXT:
							TextAlignment = (Object->Info2 & 0x0f);
							TextRotation = (Object->Info2 >> 4) & 1;
							AddMode = 0;

							if (TextRotation)
								AddMode += 4;

							if ((TextAlignment == 6) || (TextAlignment == 8))
								AddMode += 1;	// Mirror X

							if ((TextAlignment == 2) || (TextAlignment == 8))
								AddMode += 2;	// Mirror Y

							strcpy(NewPinBusStr, Object->Text1);
							ConvertPinBusStr(Object, NewPinBusStr, 0);
							lengte = strlen(NewPinBusStr);
							cnt2 = 0;
#ifdef _DEBUG

							if (stricmpUTF8(Object->Text2, "a[0:15]") == 0)
								ok = 1;

#endif
							NrLines = 0;
							memset(LinePos, 0, sizeof(LinePos));
							memset(PinBusStrings, 0, sizeof(PinBusStrings));
							LinePos[0] = 0;

							while (cnt2 < lengte)
							{
								if (NewPinBusStr[cnt2] == '\\')
								{
									memmove(&PinBusStrings[NrLines], &NewPinBusStr[LinePos[NrLines]],
									        cnt2 - LinePos[NrLines]);

									if (NrLines < 7)
										NrLines++;

									LinePos[NrLines] = cnt2 + 1;
								}

								cnt2++;
							}

							if (NewPinBusStr[lengte - 1] != '\\')
							{
								memmove(&PinBusStrings[NrLines], &NewPinBusStr[LinePos[NrLines]],
								        cnt2 - LinePos[NrLines]);
								LinePos[NrLines] = cnt2;
								NrLines++;
							}

							x3 = x1;
							y3 = y1;

							switch (AddMode)
							{
							case 2:	// Rotation = 0 , MirrorY = 1 , MirrorX = 0
							case 3:	// Rotation = 0 , MirrorY = 1 , MirrorX = 1
								y3 += (NrLines - 1);
								break;

							case 6:	// Rotation = 1 , MirrorY = 1 , MirrorX = 0
							case 7:	// Rotation = 1 , MirrorY = 1 , MirrorX = 1
								x3 -= (NrLines - 1);
								break;
							}

							for (cnt3 = 0; cnt3 < NrLines; cnt3++)
							{
								if (InRange(x2, 1.0))
									x2 = 0.9;

								DXFWriteText(x3, y3, x2, TextRotation * 90.0, TextAlignment, DXFLayer, fp,
								             PinBusStrings[cnt3]);

								switch (AddMode)
								{
								case 0:	// Rotation = 0 , MirrorY = 0 , MirrorX = 0
								case 1:	// Rotation = 0 , MirrorY = 0 , MirrorX = 1
								case 2:	// Rotation = 0 , MirrorY = 1 , MirrorX = 0
								case 3:	// Rotation = 0 , MirrorY = 1 , MirrorX = 1
									y3 -= 1.0;
									break;

								case 4:	// Rotation = 1 , MirrorY = 0 , MirrorX = 0
								case 5:	// Rotation = 1 , MirrorY = 0 , MirrorX = 1
								case 6:	// Rotation = 1 , MirrorY = 1 , MirrorX = 0
								case 7:	// Rotation = 1 , MirrorY = 1 , MirrorX = 1
									x3 += 1.0;
									break;
								}
							}

							ok = 1;
							break;

						}

					}
				}
			}
		}
	}

	WriteToFile(fp, "  0\r\nENDSEC\r\n  0\r\nEOF\r\n");

	FileClose(fp);

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SearchBlockName(LPSTR BlockName)
{
	int32 cnt;

	cnt = 0;

	while ((cnt < NrBlocks) && (stricmpUTF8(BlockNames[cnt], BlockName) != 0))
		cnt++;

	if (cnt < NrBlocks)
		return cnt;

	return -1;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int ReadLnDXF2(LPSTR LineBuf, int32 * LineStartFilePos, int32 * NextLineStartFilePos)
{
	int32 cnt, count, LineLength;
	char kar;
	int32 Stop = 0;
	LineLength = 0;
	count = 0;

	*LineStartFilePos = FilePointerDXF;

	while (!Stop)
	{
		if (FilePointerDXF < FileSizeDXF)
		{
			kar = DXFBuffer[FilePointerDXF];
			FilePointerDXF++;
			count++;

			if (kar == 10)
			{
				if (LineLength > 0)
				{
					cnt = LineLength - 1;

					while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
						cnt--;

					LineLength = cnt + 1;
				}

				Stop = 1;
			}
			else
			{
				if (LineLength < 1024)
					LineBuf[LineLength++] = kar;
			}
		}
		else
			Stop = 1;
	}

	LineBuf[LineLength] = 0;
	*NextLineStartFilePos = FilePointerDXF;

	if (count > 0)
		return LineLength;

	return -1;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ReadLnDxf(int fp, LPSTR RegelBuf)
{
#ifdef _DEBUG
	int32 res;
#endif

	if (BlockNestingLevel == -1)
	{
		// TextLineNr
#ifdef _DEBUG
		if (TextLineNr == 2121)
			res = 1;

#endif
		return ReadLn(fp, RegelBuf);
	}


// int32 BlockStartP[128],BlockEndP[128],NrBlocks,BlockNr[5],BlockP[5];

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetLayersDXF(LPSTR FileName)
{
	int32 FoundStr, BlockStart, fp, Found, res, result, ReadMode, FilePos, FilePos2, LineNr, Nesting, CommandCode,
	      ValueInt, NrPolylinePoints, LineLength, cnt;
	double ValueDouble;
	char NumberString[MAX_LENGTH_STRING], ValueStr[MAX_LENGTH_STRING], CurrentCommandString[MAX_LENGTH_STRING],
	     str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], LayerName[MAX_LENGTH_STRING], RegelBuf[4096],
	     CopyStr[MAX_LENGTH_STRING];

//  BitmapBuf2=TempMem;

	if ((FileSizeDXF = FileSizeUTF8(FileName)) < 0)
		return -1;

	if ((fp = FileOpenReadOnlyUTF8(FileName)) <= 0)
		return -1;

	if (FileSizeDXF + 65536 > MaxTempMemory)
		AllocateMemTemp(FileSizeDXF + 65536);

	DXFBuffer = TempMem;
	FilePointerDXF = 0;
	FileRead(fp, DXFBuffer, FileSizeDXF, &result);
	FileClose(fp);

	CommandCode = 0;
	Found = 0;
	Nesting = 0;
	NrBlocks = 0;
	ReadMode = 0;
	NrDXFLayers = 0;
	LineNr = 0;
	BlockNestingLevel = -1;
	BlockStart = 0;
	LineLength = 10;

	while ((LineLength >= 0) && (LineLength != -10))
	{

// ***************************************************************************************************
// ***************************************************************************************************
#ifdef _DEBUG
		if (LineNr == 196118)
			ok = 1;

#endif
		LineLength = ReadLnDXF2(RegelBuf, &FilePos, &FilePos2);

// ***************************************************************************************************
// ***************************************************************************************************
		if (LineLength >= 0)
		{
			LineNr++;
			strcpy(CopyStr, RegelBuf);

			if ((Found & 1) == 0)
			{
				GetString(CopyStr, NumberString);
				CommandCode = atoi(NumberString);
			}
			else
			{
				cnt = 0;

				while ((cnt < LineLength) && (RegelBuf[cnt] == ' '))
					cnt++;

				strcpy(str3, (LPSTR) & RegelBuf[cnt]);
				strcpy(str4, str3);
				GetString(str4, ValueStr);
				ValueDouble = 0.0;
				ValueInt = 0;
				ValueInt = atoi(ValueStr);
				ValueDouble = atof(ValueStr);
				FoundStr = 0;

				if (CommandCode == 0)
				{

					strcpy(CurrentCommandString, ValueStr);

					if (stricmpUTF8(ValueStr, "SECTION") == 0)
					{
						Nesting++;
						FoundStr = 1;
					}

					if (stricmpUTF8(ValueStr, "SEQEND") == 0)
					{
						// TextLineNr
					}

					if (stricmpUTF8(ValueStr, "VERTEX") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							res = 1;
					}

					if (stricmpUTF8(ValueStr, "POLYLINE") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							NrPolylinePoints = 0;
					}

					if (stricmpUTF8(ValueStr, "ENDSEC") == 0)
					{
						Nesting--;

						if (Nesting == -1)
							res = 1;
					}

					if (stricmpUTF8(ValueStr, "TABLE") == 0)
						Nesting++;

					if (stricmpUTF8(ValueStr, "ENDTAB") == 0)
					{
						Nesting--;

						if (Nesting == 0)
							res = 1;
					}

					if (stricmpUTF8(ValueStr, "BLOCK") == 0)
					{
						Nesting++;
						BlockStartP[NrBlocks] = FileP;
					}

					if (stricmpUTF8(ValueStr, "ENDBLK") == 0)
					{
						Nesting--;

						if (Nesting == -1)
							res = 1;
					}
				}

				switch (CommandCode)
				{
				case 8:
					strcpy(LayerName, ValueStr);
					cnt = 0;

					while ((cnt < NrDXFLayers) && (stricmpUTF8(DXFLayers[cnt], ValueStr) != 0))
						cnt++;

					if (cnt == NrDXFLayers)
						strcpy(DXFLayers[NrDXFLayers++], ValueStr);

					break;
				}
			}

			Found++;
		}
	}

	return 0;
}

//**************************************************************************************************************************
//*********************************** IDD_DIALOG_DXFLAYER ******************************************************************
//**************************************************************************************************************************

int32 CALLBACK DXFLayerDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, NrSelectedLayers;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(385, "Import from DXF file"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(144, "Select objects"));
		SetDialogItemTextUTF8(Dialog, ID_SELECT_ALL, SC(417, "Select all"));
		SetDialogItemTextUTF8(Dialog, ID_UNSELECT_ALL, SC(418, "Unselect all"));
		

		for (cnt = 0; cnt < NrDXFLayers; cnt++)
			res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);
    		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, (WPARAM) 1, (LPARAM) MAKELPARAM(0, NrDXFLayers));
			return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case ID_SELECT_ALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 1, MAKELPARAM(0, 100));
			break;

		case ID_UNSELECT_ALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 0, MAKELPARAM(0, 100));
			break;

		case IDOK:
			NrSelectedLayers =
				SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, MAX_NR_LAYERS, (LPARAM) & SelectedLayers);
			EndDialog(Dialog, NrSelectedLayers);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
			/*
			        case IDHELP:
			          Help("ImportDXF,0);
			          return about;
			*/
		}

		break;
	}

	about = 0;
	return about;
}

int32 SelectImportDXFLayer(int32 mode)
{
	int32 res;

	DialogMode = 0;
	res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_DXFLAYER), SCHWindow, (DLGPROC) DXFLayerDialogBody);

	if (res <= 0)
		return -1;

	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK LayerDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, cnt;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		if (DialogMode == 0)
		{
			SetWindowTextUTF8(Dialog, "Select DXF layer");

			for (cnt = 0; cnt < NrDXFLayers; cnt++)
				res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);
		}
		else
		{
			SetWindowTextUTF8(Dialog, "Select layer");

			for (cnt = 0; cnt < NrGeomLayers; cnt++)
				res = SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) LayerNames[cnt]);
		}

		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(323, "Layers"));
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
			EndDialog(Dialog, res);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
			/*
			        case IDHELP:
			          Help("ImportDXF,0);
			          return about;
			*/
		}

		break;
	}

	about = 0;
	return about;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 CheckDXFLayer(LPSTR DXFLayerToCheck, int32 NrLayersSelected)
{
	int32 cnt;

	for (cnt = 0; cnt < NrLayersSelected; cnt++)
	{
		if (strcmp(DXFLayerToCheck, DXFLayers[SelectedLayers[cnt]]) == 0)
			return cnt;
	}

	return -1;
}

//**************************************************************************************************************************
//********************** Importovat ze souboru DXF *************************************************************************
//**************************************************************************************************************************

int32 ImportDXF(int32 mode)
{
	int32 FoundStr, StartPoint, BlockStart, HeaderStart, BlocksStart, TableStart, ArcCounterClockWise, AngleInDegrees;
	int32 Attrib40;
	int32 Attrib41;
	int32 Attrib42;
	int32 Attrib50;
	int32 Attrib51;
	int32 Attrib60;
	int32 Attrib66;
	int32 Attrib70;
	int32 Attrib71;
	int32 Attrib72;

	int32 Found, res, TextAlignment, fp, PolyLineFlags, cnt, cnt2, LineLength, Units, LineNr, LastEntityCommand,
	      Attributes, ReadMode, FilePos, FilePos2, PreviousFilePos, Nesting, CommandCode, ValueInt, NrPolylinePoints, ok,
	      LineNrP[5], BulgeFlags, LayerCnt, NrSelectedDXFLayers;
	double ValueDouble, X[10], Y[10], Z[10], Width, Height, Thickness, Diameter, Angle, LengthLine, Length2,
	       BlockAngles[5], BlockInsertScaleValueX, BlockInsertScaleValueY, dx, dy;
	char NumberString[MAX_LENGTH_STRING], ValueStr[MAX_LENGTH_STRING], CurrentCommandString[MAX_LENGTH_STRING],
	     AttributeName[MAX_LENGTH_STRING], LayerName[MAX_LENGTH_STRING], TextString[MAX_LENGTH_STRING],
	     BlockName[MAX_LENGTH_STRING], LayerNameToCheck[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING],
	     str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], RegelBuf[4096],
	     CopyStr[MAX_LENGTH_STRING], VarName[MAX_LENGTH_STRING], PolyLineLayerName[MAX_LENGTH_STRING];
	char EenString[10] = " ";

	DialogMode = 1;
	NrGeomLayers = 0;
	NrGeomLayers = 0;

	LastEntityCommand = -1;
	ClearObject(X);
	ClearObject(Y);
	ClearObject(Z);

//  Layer=INFO_LAYER2;

	ClearObject(NewObjectLine);
	ClearObject(NewObjectRect);
	ClearObject(NewObjectArc);
	ClearObject(NewObjectCircle);
	ClearObject(NewObjectText);
	Thickness = 0.0;

	NewObjectLine.Info = (OBJECT_SELECTED + 3);
	NewObjectRect.Info = (OBJECT_SELECTED + 3);
	NewObjectArc.Info = (OBJECT_SELECTED + 3);
	NewObjectCircle.Info = (OBJECT_SELECTED + 3);
	NewObjectText.Info = (OBJECT_SELECTED + 3);
	NrPolylinePoints = 0;
	CommandCode = 0;
	TextAlignment = 0;
	Width = 0.0;
	Height = 0.0;
	Diameter = 0.0;
	Angle = 0.0;
	StartAngle = 0.0;
	EndAngle = 0.0;
	Attrib40 = 0;
	Attrib41 = 0;
	Attrib42 = 0;
	Attrib50 = 0;
	Attrib70 = 0;
	ArcCounterClockWise = 1;
	PolyLineFlags = 0;
	ClearObject(BlockAngles);

	Width = 0.0;
	Height = 0.0;
	Diameter = 0.0;
	Angle = 0.0;
	ArcCounterClockWise = 1;
	PolyLineFlags = 0;
	ClearObject(BlockAngles);

	if (ExportDir[0] == 0)
		strcpy(ExportDir, DesignPath); //pùvodnì ExePath

	if (GetNewFileUTF8
	(SCHWindow, NULL, ImportDXFFile, ExportDir, SC(415, "DXF file"), NULL, SC(385, "Import from DXF file"), "dxf", 0) != 0)
		return -1;

	CheckInputMessages(0);
	strcpy(FileName, ImportDXFFile);

	GetLayersDXF(FileName);

	NrSelectedDXFLayers = SelectImportDXFLayer(0);

	if (NrSelectedDXFLayers <= 0)
		return -1;

	if ((fp = FileOpenReadOnlyUTF8(FileName)) <= 0)
	{
		return -1;
	}

	FilePointerDXF = 0;
	NewObjectLine.Thickness = (float) 0.1;
	NewObjectArc.Thickness = (float) 0.1;
	NewObjectCircle.Thickness = (float) 0.1;
	NewObjectText.Thickness = (float) 0.1;
	LineNr = 0;
//  Units=2; // inch
	Units = 1;					// mm
//  Units=0; // thou
	Found = 0;
	Nesting = 0;
	NrBlocks = 0;
	ReadMode = 0;
	BlockNestingLevel = -1;
	BlockStart = 0;
	BlocksStart = 0;
	StartPoint = 0;
	TableStart = 0;
	AngleInDegrees = 1;
	VarName[0] = 0;
	LineLength = 10;
	FilePos = 0;
	BulgeFlags = 0;
	StartWidth = 0.0;
	PolyLineStartWidth = 0.0;
	PolyLineEndWidth = 0.0;
	BlockInsertScaleValueX = 1.0;
	BlockInsertScaleValueY = 1.0;
	EndWidth = 0.0;
	PreviousFilePos = FilePos;

	while ((LineLength >= 0) && (LineLength != -10))
	{
		LineLength = ReadLnDXF2(RegelBuf, &FilePos, &FilePos2);

		if (LineLength >= 0)
		{
			LineNr++;
#ifdef _DEBUG

			if (LineNr == 1986)
			{
				ok = FilePointerDXF;
				res = 1;
			}

#endif
			strcpy(CopyStr, RegelBuf);

			if ((Found & 1) == 0)
			{
				GetString(CopyStr, NumberString);
				CommandCode = atoi(NumberString);

				if (StartPoint)
				{
					if (LastEntityCommand != -1)
						res = 1;

					LastEntityCommand = CommandCode;
				}

				strcpy(str2, NumberString);
				strcat(str2, "        ");
				str2[5] = 0;

				if (CommandCode == 0)
				{
               
                  //****************************************************************************************************
                  //********* Práce na parametrech pøedchozí pøíkazové sekvence ****************************************
                  //****************************************************************************************************

					if (BlockStart)
					{
						res = 1;
						strcpy(BlockNames[NrBlocks], AttributeName);
					}

					if (Nesting >= 1)
					{
						if (stricmpUTF8(CurrentCommandString, "VERTEX") == 0)
						{
							if (NrPolylinePoints < 1000)
							{
#ifdef _DEBUG

								if (LineNr == 843)
									res = 1;

#endif
								dx = max(0.000000001, (fabs(X[0]) / 1000000.0));
								dy = max(0.000000001, (fabs(Y[0]) / 1000000.0));

								if ((NrPolylinePoints == 0)
								        || (!InRangeSpecial(X[0], PolyLineX[NrPolylinePoints - 1], dx))
								        || (!InRangeSpecial(Y[0], PolyLineY[NrPolylinePoints - 1], dy)))
								{
									PolyLineX[NrPolylinePoints] = X[0];
									PolyLineY[NrPolylinePoints] = Y[0];
									PolyLineZ[NrPolylinePoints] = Z[0];

									if (Attrib40)
										VertexStartWidth[NrPolylinePoints] = StartWidth;
									else
										VertexStartWidth[NrPolylinePoints] = PolyLineStartWidth;

									if (Attrib41)
										VertexEndWidth[NrPolylinePoints] = EndWidth;
									else
										VertexEndWidth[NrPolylinePoints] = PolyLineEndWidth;

									NrPolylinePoints++;
								}
							}
						}

                      //***************************************************************************************************
                      //***************************************************************************************************

						if ((stricmpUTF8(CurrentCommandString, "SEQEND") == 0)
						        || (stricmpUTF8(CurrentCommandString, "LINE") == 0))
						{

#ifdef _DEBUG
							if (LineNr == 843)
								res = 1;

#endif

							if (stricmpUTF8(CurrentCommandString, "LINE") == 0)
							{
								PolyLineX[0] = X[0];
								PolyLineY[0] = Y[0];
								PolyLineZ[0] = Z[0];
								PolyLineX[1] = X[1];
								PolyLineY[1] = Y[1];
								PolyLineZ[1] = Z[1];
								VertexStartWidth[0] = 0.0;
								VertexEndWidth[0] = 0.0;
								VertexStartWidth[1] = 0.0;
								VertexEndWidth[1] = 0.0;
								NrPolylinePoints = 2;
								res = 1;
							}

							if (((PolyLineFlags & 1) == 1) && (NrPolylinePoints > 2))
							{
								if (NrPolylinePoints < 1000)
								{
									PolyLineX[NrPolylinePoints] = PolyLineX[0];
									PolyLineY[NrPolylinePoints] = PolyLineY[0];
									PolyLineZ[NrPolylinePoints] = PolyLineZ[0];
									NrPolylinePoints++;
								}
							}

							if (stricmpUTF8(CurrentCommandString, "LINE") == 0)
								strcpy(LayerNameToCheck, LayerName);
							else
								strcpy(LayerNameToCheck, PolyLineLayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								ok = 1;

								for (cnt = 0; cnt < NrPolylinePoints; cnt++)
								{
									PointsX[cnt] = PolyLineX[cnt];
									PointsY[cnt] = PolyLineY[cnt];
									VertexStartWidthTemp[cnt] = VertexStartWidth[cnt];
									VertexEndWidthTemp[cnt] = VertexEndWidth[cnt];
								}

								if (BlockNestingLevel >= 0)
								{
									for (cnt = 0; cnt < NrPolylinePoints; cnt++)
									{
										PointsX[cnt] *= BlockInsertScaleX[BlockNestingLevel];
										PointsY[cnt] *= BlockInsertScaleY[BlockNestingLevel];
										VertexStartWidthTemp[cnt] *= BlockInsertScaleY[BlockNestingLevel];
										VertexEndWidthTemp[cnt] *= BlockInsertScaleY[BlockNestingLevel];

										if (AngleInDegrees)
											RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[BlockNestingLevel]);

										PointsX[cnt] += BlockInsertX[BlockNestingLevel];
										PointsY[cnt] += BlockInsertY[BlockNestingLevel];
									}

									if (BlockNestingLevel == 1)
									{
										for (cnt = 0; cnt < NrPolylinePoints; cnt++)
										{
											PointsX[cnt] *= BlockInsertScaleX[0];
											PointsY[cnt] *= BlockInsertScaleY[0];
											VertexStartWidth[cnt] *= BlockInsertScaleY[0];
											VertexEndWidth[cnt] *= BlockInsertScaleY[0];

											if (AngleInDegrees)
												RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[0]);

											PointsX[cnt] += BlockInsertX[0];
											PointsY[cnt] += BlockInsertY[0];
										}
									}
								}

                              //*********************************************************************************************
								for (cnt = 0; cnt < NrPolylinePoints - 1; cnt++)
								{
									if ((cnt == 0) || (cnt == NrPolylinePoints - 1))
										AngleBetweenLines[cnt] = 10000.0;
									else
									{
										AngleBetweenLines[cnt] =
										    GetAngleBetweenLines(PointsX[cnt - 1], PointsY[cnt - 1], PointsX[cnt],
										                         PointsY[cnt], PointsX[cnt + 1], PointsY[cnt + 1]);
									}
								}

								ok = 1;

								for (cnt = 0; cnt < NrPolylinePoints - 1; cnt++)
								{
									dx = PointsX[cnt + 1] - PointsX[cnt];
									dy = PointsY[cnt + 1] - PointsY[cnt];
									LengthLine =
									    CalcLengthLine(PointsX[cnt], PointsY[cnt], PointsX[cnt + 1], PointsY[cnt + 1]);

									if (VertexStartWidthTemp[cnt] != 0.0)
									{
										if (BulgeFlags == 1)
										{
											if (InRangeSpecial
											        (LengthLine, VertexStartWidthTemp[cnt],
											         max(0.0000001, (fabs(VertexStartWidthTemp[cnt]) / 1000000))))
											{	// Round filled circle
												NewObjectCircle.CentreX =
												    (float) ((PointsX[cnt] + PointsX[cnt + 1]) * 0.5);
												NewObjectCircle.CentreY =
												    (float) ((PointsY[cnt] + PointsY[cnt + 1]) * 0.5);
												NewObjectCircle.Diam = (float) (VertexStartWidthTemp[cnt] * 2.0);
												NewObjectCircle.CircleMode = 15;
												AddObjectCircle(&NewObjectCircle);
											}
											else
											{	// Trace
												ConvNormalCoorToPolar(PointsX[cnt], PointsY[cnt], PointsX[cnt + 1],
												                      PointsY[cnt + 1], &Angle, &Length2);
												NewObjectLine.X1 =
												    (float) (PointsX[cnt] +
												             VertexStartWidthTemp[cnt] * 0.5 * cos(Angle));
												NewObjectLine.Y1 =
												    (float) (PointsY[cnt] +
												             VertexStartWidthTemp[cnt] * 0.5 * sin(Angle));
												NewObjectLine.X2 =
												    (float) (PointsX[cnt + 1] -
												             VertexStartWidthTemp[cnt] * 0.5 * cos(Angle));
												NewObjectLine.Y2 =
												    (float) (PointsY[cnt + 1] -
												             VertexStartWidthTemp[cnt] * 0.5 * sin(Angle));
												LengthLine =
												    CalcLengthLine(NewObjectLine.X1, NewObjectLine.Y1, NewObjectLine.X2,
												                   NewObjectLine.Y2);

												if (LengthLine < 0.01e6)
												{	// Round pad
													NewObjectCircle.CentreX =
													    (float) (PointsX[cnt] +
													             VertexStartWidthTemp[cnt] * 0.5 * cos(Angle));
													NewObjectCircle.CentreY =
													    (float) (PointsY[cnt] +
													             VertexStartWidthTemp[cnt] * 0.5 * sin(Angle));
													NewObjectCircle.Diam = (float) (VertexStartWidthTemp[cnt] * 2.0);
													NewObjectCircle.CircleMode = 15;
													AddObjectCircle(&NewObjectCircle);
												}
												else
													AddObjectLine(&NewObjectLine);
											}
										}
										else
										{
											if (InRangeSpecial
											        (VertexStartWidthTemp[cnt], VertexEndWidthTemp[cnt],
											         max(0.00000001, (fabs(VertexStartWidthTemp[cnt]) / 1000000))))
											{
												if (dx == 0.0)
												{	// Vertical block
													NewObjectRect.CentreX =
													    (float) ((PointsX[cnt] + PointsX[cnt + 1]) * 0.5);
													NewObjectRect.CentreY =
													    (float) ((PointsY[cnt] + PointsY[cnt + 1]) * 0.5);
													NewObjectRect.Width = (float) VertexStartWidthTemp[cnt];
													NewObjectRect.Height = (float) LengthLine;
													AddObjectRect(&NewObjectRect);
												}
												else
												{
													if (dy == 0.0)
													{	// Horizontal block
														NewObjectRect.CentreX =
														    (float) ((PointsX[cnt] + PointsX[cnt + 1]) * 0.5);
														NewObjectRect.CentreY =
														    (float) ((PointsY[cnt] + PointsY[cnt + 1]) * 0.5);
														NewObjectRect.Width = (float) LengthLine;
														NewObjectRect.Height = (float) VertexStartWidthTemp[cnt];
														AddObjectRect(&NewObjectRect);
													}
													else
													{	// Diagonal polygon
														ok = 1;
												
														NewObjectLine.X1 = (float) PointsX[cnt];
														NewObjectLine.Y1 = (float) PointsY[cnt];
														NewObjectLine.X2 = (float) PointsX[cnt + 1];
														NewObjectLine.Y2 = (float) PointsY[cnt + 1];
														AddObjectLine(&NewObjectLine);
													}
												}

												if (AngleBetweenLines[cnt] < ANGLE_CONVERT(179.0))
												{
													NewObjectCircle.CentreX = (float) PointsX[cnt];
													NewObjectCircle.CentreY = (float) PointsY[cnt];
													NewObjectCircle.Diam = (float) VertexStartWidthTemp[cnt];
													NewObjectCircle.CircleMode = 15;
													AddObjectCircle(&NewObjectCircle);
												}
											}
											else
											{	// Different start and end width -> Make polygon
												ok = 1;
												NewObjectLine.X1 = (float) PointsX[cnt];
												NewObjectLine.Y1 = (float) PointsY[cnt];
												NewObjectLine.X2 = (float) PointsX[cnt + 1];
												NewObjectLine.Y2 = (float) PointsY[cnt + 1];
												AddObjectLine(&NewObjectLine);
										
											}
										}
									}
									else
									{
										NewObjectLine.X1 = (float) PointsX[cnt];
										NewObjectLine.Y1 = (float) PointsY[cnt];
										NewObjectLine.X2 = (float) PointsX[cnt + 1];
										NewObjectLine.Y2 = (float) PointsY[cnt + 1];

										AddObjectLine(&NewObjectLine);
									}
								}
							}

							res = 1;
						}

                      // ***************************************************************************************************
                      // ***************************************************************************************************
						if ((stricmpUTF8(CurrentCommandString, "TRACE") == 0)
						        || (stricmpUTF8(CurrentCommandString, "SOLID") == 0))
						{
                          // filled polygon (4 points)
                          //    LayerName
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								for (cnt = 0; cnt < 4; cnt++)
								{
									PointsX[cnt] = X[cnt];
									PointsY[cnt] = Y[cnt];
								}

								if (BlockNestingLevel >= 0)
								{
									if (AngleInDegrees)
									{
										for (cnt = 0; cnt < 4; cnt++)
											RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[BlockNestingLevel]);
									}

									for (cnt = 0; cnt < 4; cnt++)
									{
										PointsX[cnt] *= BlockInsertScaleX[BlockNestingLevel];
										PointsY[cnt] *= BlockInsertScaleY[BlockNestingLevel];
										PointsX[cnt] += BlockInsertX[BlockNestingLevel];
										PointsY[cnt] += BlockInsertY[BlockNestingLevel];
									}

									if (BlockNestingLevel == 1)
									{
										if (AngleInDegrees)
										{
											for (cnt = 0; cnt < 4; cnt++)
												RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[0]);
										}

										for (cnt = 0; cnt < 4; cnt++)
										{
											PointsX[cnt] *= BlockInsertScaleX[0];
											PointsY[cnt] *= BlockInsertScaleY[0];
											PointsX[cnt] += BlockInsertX[0];
											PointsY[cnt] += BlockInsertY[0];
										}
									}
								}

								for (cnt = 0; cnt < 4; cnt++)
								{
									NewObjectLine.X1 = (float) PointsX[cnt];
									NewObjectLine.Y1 = (float) PointsY[cnt];

									if (cnt < 3)
									{
										NewObjectLine.X2 = (float) PointsX[cnt + 1];
										NewObjectLine.Y2 = (float) PointsY[cnt + 1];
									}
									else
									{
										NewObjectLine.X2 = (float) PointsX[0];
										NewObjectLine.Y2 = (float) PointsY[0];
									}

									AddObjectLine(&NewObjectLine);
								}
							}

							res = 1;
						}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmpUTF8(CurrentCommandString, "POLYLINE") == 0)
						{
							strcpy(PolyLineLayerName, LayerName);

						}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmpUTF8(CurrentCommandString, "CIRCLE") == 0)
						{
// non filled cirkel with thickness of 0
//    Diameter
//    LayerName
						
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								ok = 1;

								for (cnt = 0; cnt < 1; cnt++)
								{
									PointsX[cnt] = X[cnt];
									PointsY[cnt] = Y[cnt];
								}

								if (BlockNestingLevel >= 0)
								{
									if (AngleInDegrees)
									{
										for (cnt = 0; cnt < 1; cnt++)
											RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[BlockNestingLevel]);
									}

									for (cnt = 0; cnt < 1; cnt++)
									{
										PointsX[cnt] *= BlockInsertScaleX[BlockNestingLevel];
										PointsY[cnt] *= BlockInsertScaleY[BlockNestingLevel];
										PointsX[cnt] += BlockInsertX[BlockNestingLevel];
										PointsY[cnt] += BlockInsertY[BlockNestingLevel];
									}

									Diameter *= BlockInsertScaleY[BlockNestingLevel];

									if (BlockNestingLevel == 1)
									{
										if (AngleInDegrees)
										{
											for (cnt = 0; cnt < 1; cnt++)
												RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[0]);
										}

										for (cnt = 0; cnt < 1; cnt++)
										{
											PointsX[cnt] *= BlockInsertScaleX[0];
											PointsY[cnt] *= BlockInsertScaleY[0];
											PointsX[cnt] += BlockInsertX[0];
											PointsY[cnt] += BlockInsertY[0];
										}

										Diameter *= BlockInsertScaleY[0];
									}
								}

								NewObjectCircle.CentreX = (float) PointsX[0];
								NewObjectCircle.CentreY = (float) PointsY[0];
								NewObjectCircle.Diam = (float) (Diameter * 2.0);
								NewObjectCircle.CircleMode = 15;
								AddObjectCircle(&NewObjectCircle);
							}

							res = 1;
						}

// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmpUTF8(CurrentCommandString, "ARC") == 0)
						{
// Parameters :
//    StartAngle
//    EndAngle
//    Diameter
//    LayerName   atan2
						
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								Angle = 0.0;

								for (cnt = 0; cnt < 1; cnt++)
								{
									PointsX[cnt] = X[cnt];
									PointsY[cnt] = Y[cnt];
								}

								if (BlockNestingLevel >= 0)
								{
									if (AngleInDegrees)
									{
										for (cnt = 0; cnt < 1; cnt++)
											RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[BlockNestingLevel]);

										StartAngle += BlockAngles[BlockNestingLevel];
										EndAngle += BlockAngles[BlockNestingLevel];
									}

									for (cnt = 0; cnt < 1; cnt++)
									{
										PointsX[cnt] *= BlockInsertScaleX[BlockNestingLevel];
										PointsY[cnt] *= BlockInsertScaleY[BlockNestingLevel];
										PointsX[cnt] += BlockInsertX[BlockNestingLevel];
										PointsY[cnt] += BlockInsertY[BlockNestingLevel];
									}

									Diameter *= BlockInsertScaleY[BlockNestingLevel];

									if (BlockNestingLevel == 1)
									{
										if (AngleInDegrees)
										{
											for (cnt = 0; cnt < 1; cnt++)
												RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[0]);

											StartAngle += BlockAngles[0];
											EndAngle += BlockAngles[0];
										}

										for (cnt = 0; cnt < 1; cnt++)
										{
											PointsX[cnt] *= BlockInsertScaleX[0];
											PointsY[cnt] *= BlockInsertScaleY[0];
											PointsX[cnt] += BlockInsertX[0];
											PointsY[cnt] += BlockInsertY[0];
										}

										Diameter *= BlockInsertScaleY[0];
									}
								}

								NewObjectArc.CentreX = (float) (PointsX[0]);
								NewObjectArc.CentreY = (float) (PointsY[0]);
								NewObjectArc.Width = (float) (Diameter * 2.0);
								NewObjectArc.Height = (float) (Diameter * 2.0);
								NewObjectArc.StartDiffX = (float) (cos(ANGLE_CONVERT(StartAngle)) * NewObjectArc.Width);
								NewObjectArc.StartDiffY = (float) (sin(ANGLE_CONVERT(StartAngle)) * NewObjectArc.Width);
								NewObjectArc.EndDiffX = (float) (cos(ANGLE_CONVERT(EndAngle)) * NewObjectArc.Width);
								NewObjectArc.EndDiffY = (float) (sin(ANGLE_CONVERT(EndAngle)) * NewObjectArc.Width);
								AddObjectArc(&NewObjectArc);
							}

							res = 1;
						}

// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmpUTF8(CurrentCommandString, "INSERT") == 0)
						{
// Insert block
// Parameters:
//    BlockName
//    Angle
							res = 1;
#ifdef _DEBUG

							if (LineNr == 2253)
								res = 1;

#endif

							if (StartPoint)
							{
#ifdef _DEBUG

								if (BlockNestingLevel == 1)
									res = 1;

#endif
								BlockNestingLevel++;
								FilePosition[BlockNestingLevel] = FilePos;
								BlockInsertX[BlockNestingLevel] = X[0];
								BlockInsertY[BlockNestingLevel] = Y[0];
								BlockInsertZ[BlockNestingLevel] = Z[0];

								if (!Attrib50)
									Angle = 0.0;

								BlockAngles[BlockNestingLevel] = Angle;
								BlockP[BlockNestingLevel] = -1;
								strcpy(BlockLayerName[BlockNestingLevel], LayerName);
								res = SearchBlockName(AttributeName);
								LineNrP[BlockNestingLevel] = LineNr - 1;
								BlockInsertScaleX[BlockNestingLevel] = BlockInsertScaleValueX;
								BlockInsertScaleY[BlockNestingLevel] = BlockInsertScaleValueY;
#ifdef _DEBUG

								if (stricmpUTF8(AttributeName, "CONN4") == 0)
									ok = 0;

#endif

								if (res != -1)
								{
									BlockNr[BlockNestingLevel] = res;
									FilePointerDXF = BlockStartP[res];
									LineNr = BlockLineNr[res];
#ifdef _DEBUG

									if (LineNr == 2240)
										ok = 0;

									if (BlockNestingLevel == -1)
										ok = 1;

#endif
									CurrentCommandString[0] = 0;
									Found = -1;

									Attrib40 = 0;
									Attrib41 = 0;
									Attrib42 = 0;
									Attrib50 = 0;
									Attrib51 = 0;
									Attrib60 = 0;
									Attrib66 = 0;
									Attrib70 = 0;
									Attrib71 = 0;
									Attrib72 = 0;

								}
								else
									res = 1;
							}
						}

// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmpUTF8(CurrentCommandString, "TEXT") == 0)
						{
// Text with parameters:
//   Height , Angle , TextString , LayerName
//   TextAlignment
//      0 = Left
//      1 = Baseline centered (Centre X)
//      2 = Right
//      4 = Centre

							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								Height *= BlockInsertScaleValueX;
								GetMinMaxText(PointsX[0], PointsY[0], Height, 0, 0, 0, TextString);

								switch (TextAlignment)
								{
								case 1:
									PointsX[0] -= ((TextMaxX - TextMinX) * 0.5);
									break;

								case 2:
									PointsX[0] -= TextMaxX - TextMinX;
									break;

								case 4:
									PointsX[0] -= ((TextMaxX - TextMinX) * 0.5);
									PointsY[0] -= ((TextMaxY - TextMinY) * 0.5);
									break;
								}

								for (cnt = 0; cnt < 1; cnt++)
								{
									PointsX[cnt] = X[cnt];
									PointsY[cnt] = Y[cnt];
								}

								if (BlockNestingLevel >= 0)
								{
									if (AngleInDegrees)
									{
										for (cnt = 0; cnt < 1; cnt++)
											RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[BlockNestingLevel]);

										Angle += BlockAngles[BlockNestingLevel];
									}

									for (cnt = 0; cnt < 1; cnt++)
									{
										PointsX[cnt] *= BlockInsertScaleX[BlockNestingLevel];
										PointsY[cnt] *= BlockInsertScaleY[BlockNestingLevel];
										PointsX[cnt] += BlockInsertX[BlockNestingLevel];
										PointsY[cnt] += BlockInsertY[BlockNestingLevel];
									}

									Height *= BlockInsertScaleY[BlockNestingLevel];

									if (BlockNestingLevel == 1)
									{
										if (AngleInDegrees)
										{
											for (cnt = 0; cnt < 1; cnt++)
												RotatePoint2(&PointsX[cnt], &PointsY[cnt], BlockAngles[0]);
										}

										for (cnt = 0; cnt < 1; cnt++)
										{
											PointsX[cnt] *= BlockInsertScaleX[0];
											PointsY[cnt] *= BlockInsertScaleY[0];
											PointsX[cnt] += BlockInsertX[0];
											PointsY[cnt] += BlockInsertY[0];
										}

										Height *= BlockInsertScaleY[0];
										Angle += BlockAngles[0];
									}
								}

#ifdef _DEBUG

								if (stricmpUTF8(TextString, "produktie") == 0)
									ok = 0;

								if (TextString[0] == 0)
									ok = 1;

#endif
								NewObjectText.X = (float) PointsX[0];
								NewObjectText.Y = (float) PointsY[0];
								NewObjectText.FontHeight = (float) Height;
								memset(&NewObjectText.Text, 0, sizeof(NewObjectText.Text));
								strncpy(NewObjectText.Text, TextString, sizeof(NewObjectText.Text) - 1);
								NewObjectText.Rotation = (float) Angle;
								AddObjectText(&NewObjectText);
							}

							res = 1;
						}
					}

// ***************************************************************************************************
// ***************************************************************************************************

					Attrib40 = 0;
					Attrib41 = 0;
					Attrib42 = 0;
					Attrib50 = 0;
					Attrib51 = 0;
					Attrib60 = 0;
					Attrib66 = 0;
					Attrib70 = 0;
					Attrib71 = 0;
					Attrib72 = 0;
					BlockStart = 0;
					StartAngle = 0.0;
					EndAngle = 0.0;
					Height = 0.0;
					Diameter = 0.0;
					Angle = 0.0;
				}
			}
			else
			{

// ***************************************************************************************************
// ***************************************************************************************************
//
// Prepare parameters next command sequence
//
// ***************************************************************************************************
// ***************************************************************************************************

				cnt = 0;

				while ((cnt < LineLength) && (RegelBuf[cnt] == ' '))
					cnt++;

				memset(&str3, 0, sizeof(str3));
				strcpy(str3, (LPSTR) & RegelBuf[cnt]);
				strcpy(str4, str3);
				GetString(str4, ValueStr);
				ValueDouble = 0.0;
				ValueInt = 0;
				ValueInt = atoi(ValueStr);
				ValueDouble = atof(ValueStr);
				FoundStr = 0;

				if (CommandCode == 0)
				{

					strcpy(CurrentCommandString, ValueStr);

					if (stricmpUTF8(ValueStr, "SECTION") == 0)
						Nesting++;

					if (stricmpUTF8(ValueStr, "SEQEND") == 0)
					{
						// TextLineNr
					}

					if (stricmpUTF8(ValueStr, "VERTEX") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							res = 1;
					}

					if (stricmpUTF8(ValueStr, "LINE") == 0)
					{
					}

					if (stricmpUTF8(ValueStr, "POLYLINE") == 0)
					{
						// TextLineNr
						NrPolylinePoints = 0;
						BulgeFlags = 0;
						PolyLineFlags = 0;
						StartWidth = 0.0;
						EndWidth = 0.0;
						PolyLineStartWidth = 0.0;
						PolyLineEndWidth = 0.0;
					}

					if ((stricmpUTF8(ValueStr, "INSERT") == 0) || (stricmpUTF8(ValueStr, "TEXT") == 0))
					{
						// TextLineNr
						BlockInsertScaleValueX = 1.0;
						BlockInsertScaleValueY = 1.0;
					}

					if (stricmpUTF8(ValueStr, "ENDSEC") == 0)
					{
						Nesting--;

						if (Nesting == -1)
							res = 1;

						if (StartPoint)
						{
							res = 1;
						}
					}

					if (stricmpUTF8(ValueStr, "TABLE") == 0)
						Nesting++;

					if (stricmpUTF8(ValueStr, "ENDTAB") == 0)
					{
						Nesting--;

						if (Nesting == 0)
							res = 1;
					}

					if (stricmpUTF8(ValueStr, "BLOCK") == 0)
					{
						Nesting++;

						if (BlockNestingLevel == -1)
						{
							BlockStart = 1;
							BlockStartP[NrBlocks] = PreviousFilePos;
							BlockLineNr[NrBlocks] = LineNr - 2;
						}
					}

					if (stricmpUTF8(ValueStr, "ENDBLK") == 0)
					{
						Nesting--;

						if (Nesting == -1)
							res = 1;

						if (Nesting == 1)
							res = 1;

						if (BlockNestingLevel == 1)
							res = 1;

						if (BlockNestingLevel >= 0)
						{
							FilePointerDXF = FilePosition[BlockNestingLevel];
							LineNr = LineNrP[BlockNestingLevel];
							CurrentCommandString[0] = 0;
							Found = -1;
							BlockNestingLevel--;
							Attrib40 = 0;
							Attrib41 = 0;
							Attrib42 = 0;
							Attrib50 = 0;
							Attrib51 = 0;
							Attrib60 = 0;
							Attrib66 = 0;
							Attrib70 = 0;
							Attrib71 = 0;
							Attrib72 = 0;
#ifdef _DEBUG

							if (LineNr == 2240)
								ok = 0;

							if (BlockNestingLevel == -1)
								ok = 1;

#endif
						}
						else
							NrBlocks++;
					}
				}

				switch (CommandCode)
				{
				case 1:
// Check %% for characters
					cnt2 = 0;
					memset(&TextString, 0, sizeof(TextString));
					cnt = 0;

					while (cnt < (int32) strlen(RegelBuf))
					{
						if (RegelBuf[cnt] != '%')
							TextString[cnt2++] = RegelBuf[cnt++];
						else
						{
							if (RegelBuf[cnt + 1] == '%')
							{
								if (RegelBuf[cnt + 2] == 'C')
								{
									TextString[cnt2++] = (unsigned char) 0x80;
									cnt += 3;
								}
								else
								{
									if (isdigit(RegelBuf[cnt + 2]))
									{
										TextString[cnt2++] = (unsigned char) 0x80;
										cnt += 5;
									}
									else
									{
										TextString[cnt2++] = RegelBuf[cnt++];
										TextString[cnt2++] = RegelBuf[cnt++];
									}
								}
							}
							else
								TextString[cnt2++] = RegelBuf[cnt++];
						}
					}

					break;

				case 2:
					if (stricmpUTF8(CurrentCommandString, "INSERT") == 0)
						strcpy(BlockName, ValueStr);

					if (Nesting == 1)
					{
						if (stricmpUTF8(ValueStr, "HEADER") == 0)
						{
							HeaderStart = 1;
							BlockStart = 0;
							BlocksStart = 0;
							StartPoint = 0;
							TableStart = 0;
						}

						if (stricmpUTF8(ValueStr, "TABLES") == 0)
						{
							HeaderStart = 0;
							TableStart = 1;
							BlocksStart = 0;
							BlockStart = 0;
							StartPoint = 0;
						}

						if (stricmpUTF8(ValueStr, "BLOCKS") == 0)
						{
							HeaderStart = 0;
							TableStart = 0;
							BlocksStart = 1;
							BlockStart = 0;
							StartPoint = 0;
						}

						if (stricmpUTF8(ValueStr, "ENTITIES") == 0)
						{
							HeaderStart = 0;
							TableStart = 0;
							BlocksStart = 0;
							BlockStart = 0;
							StartPoint = 1;
						}
					}

					strcpy(AttributeName, ValueStr);
					break;

				case 7:
					break;

				case 8:
				
					strcpy(LayerName, ValueStr);
					break;

				case 9:
					if (VarName[0] != 0)
					{	// Previous variable name
						if (stricmpUTF8(VarName, "$ANGDIR") == 0)
						{
							if (Attrib70)
							{
								if (ValueInt != 0)
									ArcCounterClockWise = 0;
							}
						}
					}

					strcpy(VarName, ValueStr);
					break;

				case 10:
				case 11:
				case 12:
				case 13:
				case 14:
				case 15:
				case 16:
				case 17:
				case 18:
				case 19:
					strcat(str2, "X");
					EenString[0] = (char) (CommandCode + 39);
					strcat(str2, EenString);
					X[CommandCode - 10] = ValueDouble;
					break;

				case 20:
				case 21:
				case 22:
				case 23:
				case 24:
				case 25:
				case 26:
				case 27:
				case 28:
				case 29:
					strcat(str2, "Y");
					EenString[0] = (char) (CommandCode + 29);
					strcat(str2, EenString);
					Y[CommandCode - 20] = ValueDouble;
					break;

				case 30:
				case 31:
				case 32:
				case 33:
				case 34:
				case 35:
				case 36:
				case 37:
				case 38:
				case 39:
					strcat(str2, "Z");
					EenString[0] = (char) (CommandCode + 19);
					strcat(str2, EenString);
					Z[CommandCode - 30] = ValueDouble;
					break;

				case 40:
					Attrib40 = 1;

					if (stricmpUTF8(CurrentCommandString, "CIRCLE") == 0)
						Diameter = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "ARC") == 0)
						Diameter = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "TEXT") == 0)
						Height = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "POLYLINE") == 0)
						PolyLineStartWidth = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "VERTEX") == 0)
						StartWidth = ValueDouble;

					break;

				case 41:
					Attrib41 = 1;

					if (stricmpUTF8(CurrentCommandString, "POLYLINE") == 0)
						PolyLineEndWidth = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "VERTEX") == 0)
						EndWidth = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "INSERT") == 0)
						BlockInsertScaleValueX = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "TEXT") == 0)
						BlockInsertScaleValueX = ValueDouble;

					break;

				case 42:
					Attrib42 = 1;

					if (stricmpUTF8(CurrentCommandString, "POLYLINE") == 0)
						Width = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "INSERT") == 0)
						BlockInsertScaleValueY = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "VERTEX") == 0)
						BulgeFlags = ValueInt;

					break;

				case 50:
					Attrib50 = 1;

					if (stricmpUTF8(CurrentCommandString, "ARC") == 0)
						StartAngle = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "TEXT") == 0)
						Angle = ValueDouble;

					if (stricmpUTF8(CurrentCommandString, "INSERT") == 0)
						Angle = ValueDouble;

					break;

				case 51:
					Attrib51 = 1;

					if (stricmpUTF8(CurrentCommandString, "ARC") == 0)
						EndAngle = ValueDouble;

					break;

				case 66:
					Attrib66 = 1;
					Attributes = ValueInt;

					if (stricmpUTF8(CurrentCommandString, "POLYLINE") == 0)
					{
					}

					break;

				case 70:
					Attrib70 = 1;

					if (stricmpUTF8(CurrentCommandString, "POLYLINE") == 0)
						PolyLineFlags = ValueInt;

					break;

				case 71:
					Attrib71 = 1;
					break;

				case 72:
					Attrib72 = 1;

					if (stricmpUTF8(CurrentCommandString, "TEXT") == 0)
						TextAlignment = ValueInt;

					break;
				}
			}

			Found++;
		}

		PreviousFilePos = FilePos;
	}

	DeAllocateMemTemp();

	ViewFull(1);
	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
