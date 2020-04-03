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
#include "io.h"
#include "time.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "memory.h"
#include "string.h"
#include "calc.h"
//#include "calc4.h"
#include "memory.h"
#include "menus.h"
#include "geom.h"
#include "math.h"
#include "files2.h"
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
#include "dxf.h"
#include "resource.h"
#include "dialogs.h"
#include "help.h"
#include "stdio.h"
#include "files.h"
#include "files2.h"
#include "polygon.h"
#include "utf8.h"
#include "ctype.h"


#define  ClearObject(Object) (memset(&Object,0,sizeof(Object)))
#define  MAX_BLOCKS         512
#define  COMP_REF_LAYER     4100
#define  COMP_VALUE_LAYER   4200
#define  MAX_NR_LAYERS      32
#define  DXF_OUTPUT_INCH    2

double CentreSelectedX, CentreSelectedY;

double SelectedMinX, SelectedMinY, SelectedMaxX, SelectedMaxY;

char ExportFileName[MAX_LENGTH_STRING];
char ImportDXFFile[MAX_LENGTH_STRING];
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern HDC OutputDisplay;

int32 NrBlocks, NrDXFLayers, SelectedLayers[128], DialogMode;
int32 BlockStartP[MAX_BLOCKS], BlockEndP[MAX_BLOCKS], NrBlocks, BlockNr[5], BlockP[5], BlockSize[5],
      BlockLineNr[MAX_BLOCKS], FileSizeDXF, FilePointerDXF;
char BlockNames[MAX_BLOCKS][MAX_NR_LAYERS], BlockNestingNames[5][MAX_NR_LAYERS], BlockLayerName[5][MAX_NR_LAYERS];
char ImportDXFFile[MAX_LENGTH_STRING];
char DXFLayers[128][64];
char DXFLayer[MAX_LENGTH_STRING], LayerNames[32][128];
char DXFPath[MAX_LENGTH_STRING] = "";
int32 NrGeomLayers, BlockNestingLevel, FilePosition[5], LayerIDs[32], NrSelectedDXFLayers;
double BlockInsertX[5], BlockInsertY[5], BlockInsertZ[5], BlockRotation[5];
double BlockBaseX[5], BlockBaseY[5], BlockBaseZ[5], BlockRotation[5];
double BlockInsertScaleX[5], BlockInsertScaleY[5], StartAngle, EndAngle, PolyLineX[1000], PolyLineY[1000],
       PolyLineZ[1000], StartWidth, EndWidth, PolyLineStartWidth, PolyLineEndWidth, VertexStartWidth[1000],
       VertexEndWidth[1000], AngleBetweenLines[1000], DXFminx, DXFminy, DXFmaxx, DXFmaxy;
double PointsX[1000], PointsY[1000], MinX, MinY, MaxX, MaxY, VertexStartWidthTemp[1000], VertexEndWidthTemp[1000];

char ExportDXFFileName[MAX_LENGTH_STRING] = "";
char *DXFBuffer;

char DXFFilePath[MAX_LENGTH_STRING];

int32 DXFOptions = 0;
int32 NrOutputLayers, LayerInfo[128], LayerInfo2[128], ExportLayers[128], NrSelectedLayers;

//************************************************************************************************************************************
//********************** IDD_DIALOG_EXPORT_DXF ***************************************************************************************
//************************************************************************************************************************************

int32 CALLBACK DxfDialog2(HWND Dialog, uint16 Message, uint16 WParam, int32 LParam)
{
	int32 about;
	int32 count, cnt, cnt2, cnt3, Layer;
	ObjectRecord *Object;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(167, "Export to DXF"));
		SendDlgItemMessageUTF8(Dialog, IDOK, WM_SETTEXT, 0, (LPARAM) "OK");
		SendDlgItemMessageUTF8(Dialog, IDCANCEL, WM_SETTEXT, 0, (LPARAM) SC(46, "Cancel"));
		SendDlgItemMessageUTF8(Dialog, IDC_STATIC1, WM_SETTEXT, 0, (LPARAM) SC(173, "Select layer"));
		SendDlgItemMessageUTF8(Dialog, ID_UNSELECTALL, WM_SETTEXT, 0, (LPARAM) SC(109, "Unselect all"));
		SendDlgItemMessageUTF8(Dialog, ID_SELECTALL, WM_SETTEXT, 0, (LPARAM) SC(110, "Select all"));
		SendDlgItemMessageUTF8(Dialog, IDC_CHECK1, WM_SETTEXT, 0, (LPARAM) SC(168, "Filled objects"));
		SendDlgItemMessageUTF8(Dialog, IDC_CHECK2, WM_SETTEXT, 0, (LPARAM) SC(169, "Mirror x"));

		if ((DXFOptions & 16) == 16)
			SendDlgItemMessage(Dialog, IDC_CHECK1, BM_SETCHECK, 1, 0);

		if ((DXFOptions & 8) == 8)
			SendDlgItemMessage(Dialog, IDC_CHECK2, BM_SETCHECK, 1, 0);

//		SetWindowTextUTF8(Dialog, SC(167, "Export to DXF"));

		cnt3 = 0;

		for (cnt = 0; cnt < NrOutputLayers; cnt++)
			LayerInfo2[cnt] = 0;

		for (cnt = 0; cnt < NrOutputLayers; cnt++)
		{
			Layer = ExportLayers[cnt];
			LayerInfo[cnt] = 0;
			count = 0;

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);

				if (((Object->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Object->Layer == Layer))
					count++;
			}

			if (count > 0)
			{
				SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);
				SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 1, MAKELPARAM(cnt, cnt + 1));
				LayerInfo2[cnt3++] = cnt;
			}
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case ID_SELECTALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 1, MAKELPARAM(0, 100));
			break;

		case ID_UNSELECTALL:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, 0, MAKELPARAM(0, 100));
			break;

		case IDOK:
			NrSelectedLayers =
			    SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, NrOutputLayers, (LPARAM) & SelectedLayers);

			for (cnt = 0; cnt < NrOutputLayers; cnt++)
				LayerInfo[cnt] &= ~1;

			for (cnt = 0; cnt < NrSelectedLayers; cnt++)
				LayerInfo[LayerInfo2[SelectedLayers[cnt]]] |= 1;

			DXFOptions = 0;

			if (SendDlgItemMessage(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				DXFOptions |= 16;

			if (SendDlgItemMessage(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0) == 1)
				DXFOptions |= 8;

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

	FindMinMaxBoard(&minx, &miny, &maxx, &maxy, 1);

	minx /= 1e5;
	miny /= 1e5;
	maxx /= 1e5;
	maxy /= 1e5;

	DXFminx = minx;
	DXFminy = miny;
	DXFmaxx = maxx;
	DXFmaxy = maxy;

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
	sprintf(str, "%.3f", minx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", miny);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$EXTMAX");
	WriteLn(fp, "10");
	sprintf(str, "%.3f", maxx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", maxy);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$LIMMIN");
	WriteLn(fp, "10");
	sprintf(str, "%.3f", minx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", miny);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$LIMMAX");
	WriteLn(fp, "10");
	sprintf(str, "%.3f", maxx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.3f", maxy);
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
	WriteLn(fp, "10");			// 10 mm grid
	WriteLn(fp, "25");
	WriteLn(fp, "10");			// 10 mm grid
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
		strcpy(DXFLayers[Layer], DXFLayers[LayerInfo2[SelectedLayers[Layer]]]);
		WriteLn(fp, "0");
		WriteLn(fp, "LAYER");
		WriteLn(fp, "2");
		WriteLn(fp, DXFLayers[Layer]);
		WriteLn(fp, "70");
		WriteLn(fp, "64");
		WriteLn(fp, "62");
		sprintf(str, "%d", ColorNr);
		WriteLn(fp, str);
		WriteLn(fp, "6");
		WriteLn(fp, "CONTINUOUS");

		ColorNr++;
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

void DXFWriteLine(double x1, double y1, double x2, double y2, double Thickness, LPSTR DXFLayer, int32 fp, int32 mode)
{
	char str[MAX_LENGTH_STRING];
	double x1a, y1a, x2a, y2a, x3a;
	int32 ok;

	if (Thickness > 20)
		ok = 1;

	if ((DXFOptions & 8) == 8)
	{	// Mirror X
		x1 = -x1 + (DXFminx + DXFmaxx);
		x2 = -x2 + (DXFminx + DXFmaxx);
	}

	if ((DXFOptions & 16) == 0)
	{	// Do not fill object
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
		x1 = -x1 + (DXFminx + DXFmaxx);
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
		x1 = -x1 + (DXFminx + DXFmaxx);
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

int32 ExportDXF(int32 Mode)
{
	int32 cnt, cnt2, cnt3, cnt4, res, NrLayers, Layer, SegmentCount, ObjectPolygonLength, SelectMask1, SelectMask2,
	      Mirror, LineSegmentCount, count, TextRotation, lengte, CircleMode;
	double x1, y1, x2, y2, x3, y3, x4, y4, h2, Rotation, DXFOutputResolution, x3a, Thickness;
	int fp;
	double Angle1, Angle2, lengte1, lengte2;
	LPSTR CompStr;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], DXFLayer[MAX_LENGTH_STRING];
	ObjectRecord *Object, NewObject;
	ObjectPolygonRecord *ObjectPolygon;
	ObjectSubPolygonRecord *ObjectSubPolygon;
	double LineSegments[4096];
	uint8 *Buf;

	memset(&NewObject, 0, sizeof(NewObject));
	x2 = 0.0;
	x3 = 0.0;
	y2 = 0.0;
	y3 = 0.0;
	h2 = 0.0;
	TextRotation = 0;
	Mirror = 0;
	CompStr = str;
	str[0] = 0;

	NrOutputLayers = 0;
	ExportLayers[NrOutputLayers] = SILKSCREEN_TOP_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = PASTE_MASK_TOP_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = SOLD_MASK_TOP_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;

	for (cnt = 0; cnt < NrPadLayers; cnt++)
	{
		ExportLayers[NrOutputLayers] = NrPadLayers - 1 - cnt;
		GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
		NrOutputLayers++;
	}

	for (cnt = 0; cnt < NrPadLayers; cnt++)
	{
		ExportLayers[NrOutputLayers] = ROUTING_KEEPOUT_LAYER + NrPadLayers - 1 - cnt;
		GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
		NrOutputLayers++;
	}

	ExportLayers[NrOutputLayers] = SOLD_MASK_BOTTOM_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = PASTE_MASK_BOTTOM_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = SILKSCREEN_BOTTOM_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = BOARD_OUTLINE_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = INFO_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = INFO_LAYER2;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = INFO_LAYER3;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = INFO_LAYER4;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = DRILL_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = DRILL_UNPLATED_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = PLACEMENT_OUTLINE_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;
	ExportLayers[NrOutputLayers] = COMP_OUTLINE_LAYER;
	GetLayerText(ExportLayers[NrOutputLayers], DXFLayers[NrOutputLayers], 0);
	NrOutputLayers++;

	for (cnt = 0; cnt < NrOutputLayers; cnt++)
	{
		lengte = strlen(DXFLayers[cnt]);
		strcpy(str, DXFLayers[cnt]);
		cnt3 = 0;

		for (cnt2 = 0; cnt2 < lengte; cnt2++)
		{
			switch (DXFLayers[cnt][cnt2])
			{
			case ' ':
				DXFLayers[cnt][cnt3++] = '_';
				break;

			case '(':
			case ')':
				break;

			default:
				DXFLayers[cnt][cnt3++] = str[cnt2];
				break;
			}
		}

		DXFLayers[cnt][cnt3] = 0;
	}

	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_EXPORT_DXF), GEOMWindow, (DLGPROC) DxfDialog2);

	if (res == 2)
		return -1;

// SelectedLayers

	/*
	  0
	SECTION
	  2
	ENTITIES
	*/


	if (EditFile[0] == 0)
		sprintf(str, "%s\\noname", DesignPath);
	else
		strcpy(str, EditFile);


	CutExtensionFileName(str);

	sprintf(ExportFileName, "%s.dxf", str);

	if (FileExistsUTF8(ExportFileName) == 0)
	{
		sprintf(str2, SC(170, "File %s exist overwrite ?"), ExportFileName);

		if (MessageBoxUTF8(GEOMWindow, str2, SC(4, "Message"), MB_OKCANCEL | MB_APPLMODAL) != IDOK)
			return -1;
	}

	if ((fp = FileOpenWriteUTF8(ExportFileName)) <= 0)
		return -1;

	NrLayers = 0;

	for (cnt4 = 0; cnt4 < NrOutputLayers; cnt4++)
	{
		if ((LayerInfo[cnt4] & 1) == 1)
			NrLayers++;
	}

	WriteInitDXF(fp, NrLayers);

	WriteToFile(fp, "  0\r\n");
	WriteToFile(fp, "ENDSEC\r\n");
	WriteToFile(fp, "  0\r\nSECTION\r\n  2\r\nENTITIES\r\n");
	DXFOutputResolution = 100000.0;
	SelectMask1 = 0;
	SelectMask2 = 0;

	for (cnt4 = 0; cnt4 < NrOutputLayers; cnt4++)
	{
		Layer = ExportLayers[cnt4];
#ifdef _DEBUG

		if (Layer == COMP_OUTLINE_LAYER)
			res = 1;

#endif
		strcpy(DXFLayer, DXFLayers[cnt4]);

		if ((LayerInfo[cnt4] & 1) == 1)
		{
			for (cnt = 0; cnt < NrObjects; cnt++)
			{
				Object = &((*Objects)[cnt]);

				if (((Object->Info & (OBJECT_NOT_VISIBLE)) == 0) && (Object->Layer == Layer))
				{
#ifdef _DEBUG

					if (Object->Layer == COMP_OUTLINE_LAYER)
						res = 1;

#endif
					x1 = Object->x1 / DXFOutputResolution;
					y1 = Object->y1 / DXFOutputResolution;
					x2 = Object->x2 / DXFOutputResolution;
					y2 = Object->y2 / DXFOutputResolution;
					Thickness = Object->Thickness / DXFOutputResolution;

					switch (Object->ObjectType)
					{
					case OBJECT_LINE:
						if (Object->Info2 != 0)
						{
							x1 = Object->x1;
							y1 = Object->y1;
							x2 = Object->x2;
							y2 = Object->y2;
							LineSegmentCount =
							    DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineSegments, Object->Info2);
							SegmentCount = 0;

							for (cnt2 = 0; cnt2 < LineSegmentCount; cnt2++)
							{
								x1 = LineSegments[SegmentCount++] / DXFOutputResolution;
								y1 = LineSegments[SegmentCount++] / DXFOutputResolution;
								x2 = LineSegments[SegmentCount++] / DXFOutputResolution;
								y2 = LineSegments[SegmentCount++] / DXFOutputResolution;
								DXFWriteLine(x1, y1, x2, y2, Thickness, DXFLayer, fp, 0);
							}
						}
						else
							DXFWriteLine(x1, y1, x2, y2, Thickness, DXFLayer, fp, 0);

						break;

					case OBJECT_RECT:
						if ((Object->Thickness == 0.0) && (Layer != PLACEMENT_OUTLINE_LAYER))
							DXFWriteRectangle(x1, y1, x2, y2, Thickness, DXFLayer, fp, 2);
						else
							DXFWriteRectangle(x1, y1, x2, y2, Thickness, DXFLayer, fp, 0);

						break;

					case OBJECT_CIRCLE:
						x3 = Object->x2 / DXFOutputResolution;
						x3a = x3 * 0.5;
						CircleMode = (Object->Info2 & 15);

						if (((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER) || (Object->Thickness == 0.0))
						        && (Layer != PLACEMENT_OUTLINE_LAYER))
							DXFWriteCircle(x1, y1, x2, Thickness, DXFLayer, fp, 2);
						else
						{
							if ((CircleMode == 0) || (CircleMode == 15))
								DXFWriteCircle(x1, y1, x2, Thickness, DXFLayer, fp, 0);
							else
							{
								GetArcAngle(Object, &Angle1, &Angle2);
								sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
								WriteToFile(fp, str);

								if ((DXFOptions & DXF_OUTPUT_INCH) == 0)
								{
									sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1,
									        x3a);
								}
								else
								{
									sprintf(str, " 10\r\n%.5f\r\n 20\r\n%.5f\r\n 30\r\n0.0\r\n 40\r\n%.5f\r\n", x1, y1,
									        x3a);
								}

								WriteToFile(fp, str);
								sprintf(str, " 50\r\n%.3f\r\n 51\r\n%.3f\r\n", Angle1, Angle2);
								WriteToFile(fp, str);
							}
						}

						break;

					case OBJECT_ARC:
#ifdef _DEBUG
						if (Object->Layer == 0)
							res = 1;

#endif
						x2 = Object->x2 * 0.5 / DXFOutputResolution;
						y2 = Object->y2 * 0.5 / DXFOutputResolution;
						x3 = Object->x3 / DXFOutputResolution;
						y3 = Object->y3 / DXFOutputResolution;
						x4 = Object->x4 / DXFOutputResolution;
						y4 = Object->y4 / DXFOutputResolution;

						if ((InRange(Object->x3, Object->x4)) && (InRange(Object->y3, Object->y4)))
						{
							sprintf(str, "  0\r\nCIRCLE\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1, x2);
							WriteToFile(fp, str);
						}
						else
						{
							ConvertPointToPolar(x3, y3, &lengte1, &Angle1);
							ConvertPointToPolar(x4, y4, &lengte2, &Angle2);
							sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1, x2);
							WriteToFile(fp, str);
							sprintf(str, " 50\r\n%.3f\r\n 51\r\n%.3f\r\n", Angle1, Angle2);
							WriteToFile(fp, str);
						}

						break;

					case OBJECT_TEXT:
						x2 = Object->x2 / (DXFOutputResolution / 0.75);
						sprintf(str, "  0\r\nTEXT\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);

						if ((DXFOptions & DXF_OUTPUT_INCH) == 0)
						{
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n 40\r\n%.3f\r\n", x1, y1, x2);
							WriteToFile(fp, str);
						}
						else
						{
							sprintf(str, " 10\r\n%.5f\r\n 20\r\n%.5f\r\n 30\r\n0.0\r\n 40\r\n%.5f\r\n", x1, y1, x2);
							WriteToFile(fp, str);
						}

#ifdef _DEBUG

						if (stricmp((char *) Object->Text, "Text45") == 0)
							res = 1;

#endif
						sprintf(str, "  1\r\n%s\r\n", Object->Text);
						WriteToFile(fp, str);
						sprintf(str, " 41\r\n1.06\r\n");
						WriteToFile(fp, str);
						Rotation = Object->RotationAngle;

						if (Rotation > 1000.0)
						{
							Rotation -= 2000.0;	// Mirrored
							Rotation = 360.0 - Rotation;
							sprintf(str, " 50\r\n%.2f\r\n", Rotation);
							WriteToFile(fp, str);
							WriteToFile(fp, " 71\r\n2\r\n");
						}
						else
						{
							sprintf(str, " 50\r\n%.2f\r\n", Rotation);
							WriteToFile(fp, str);
						}

						break;
					}
				}
			}


			// ****************************************************************************
			// ****************************************************************************

			for (cnt = 0; cnt < NrObjectPolygons; cnt++)
			{
				ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

				if (((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0) && (ObjectPolygon->Layer == Layer))
				{
					sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);
					WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");
					ObjectPolygonLength = MemSizeObjectPolygon(ObjectPolygon);
					count = ObjectPolygon->NrVertices;

					if ((ObjectPolygon->NrSubPolygons > 0) && (ObjectPolygon->NrVerticesMainPolygon > 0))
						count = ObjectPolygon->NrVerticesMainPolygon;

					for (cnt3 = 0; cnt3 < count; cnt3++)
					{
						x1 = (float) (ObjectPolygon->Points[cnt3].x / DXFOutputResolution);
						y1 = (float) (ObjectPolygon->Points[cnt3].y / DXFOutputResolution);
						sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);

						if ((DXFOptions & DXF_OUTPUT_INCH) == 0)
							sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
						else
							sprintf(str, " 10\r\n%.5f\r\n 20\r\n%.5f\r\n 30\r\n0.0\r\n", x1, y1);

						WriteToFile(fp, str);
					}

					sprintf(str, "  0\r\nSEQEND\r\n");
					WriteToFile(fp, str);

					if (ObjectPolygon->NrSubPolygons > 0)
					{
						Buf = (uint8 *) ObjectPolygon;
						Buf += sizeof(ObjectPolygonInitRecord);
						Buf += count * sizeof(PointRecord);

						for (cnt2 = 0; cnt2 < ObjectPolygon->NrSubPolygons; cnt2++)
						{
							if ((Buf - (uint8 *) ObjectPolygon) >=
							        (int32) (ObjectPolygonLength - sizeof(ObjectSubPolygonInitRecord)))
								break;

							ObjectSubPolygon = (ObjectSubPolygonRecord *) Buf;

							if (ObjectSubPolygon->Magic != SUB_POLYGON_MAGIC)
								break;

							count = ObjectSubPolygon->NrVertices;

							if (count >= 65536)
								break;

							sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

							for (cnt3 = 0; cnt3 < count; cnt3++)
							{
								x1 = (float) (ObjectSubPolygon->Points[cnt3].x / DXFOutputResolution);
								y1 = (float) (ObjectSubPolygon->Points[cnt3].y / DXFOutputResolution);
								sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
								WriteToFile(fp, str);

								if ((DXFOptions & DXF_OUTPUT_INCH) == 0)
									sprintf(str, " 10\r\n%.3f\r\n 20\r\n%.3f\r\n 30\r\n0.0\r\n", x1, y1);
								else
									sprintf(str, " 10\r\n%.5f\r\n 20\r\n%.5f\r\n 30\r\n0.0\r\n", x1, y1);

								WriteToFile(fp, str);
							}

							sprintf(str, "  0\r\nSEQEND\r\n");
							WriteToFile(fp, str);
							Buf += sizeof(ObjectSubPolygonInitRecord);
							Buf += count * sizeof(PointRecord);
						}
					}

				}
			}
		}
	}

// ****************************************************************************
// ****************************************************************************

	/*
	  0
	ENDSEC
	  0
	EOF
	*/
	WriteToFile(fp, "  0\r\nENDSEC\r\n  0\r\nEOF\r\n");
	FileClose(fp);

// ****************************************************************************
// ****************************************************************************

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

	while ((cnt < NrBlocks) && (stricmp(BlockNames[cnt], BlockName) != 0))
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
	int cnt, count;
	int32 LineLength;
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
	int32 FoundStr, BlockStart;

	int32 fp, Found, res, result, ReadMode, FilePos, FilePos2, LineNr, Nesting, CommandCode, ValueInt, NrPolylinePoints,
	      LineLength, ok, cnt;
	double ValueDouble;
	char NumberString[MAX_LENGTH_STRING], ValueStr[MAX_LENGTH_STRING], CurrentCommandString[MAX_LENGTH_STRING],
	     str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], LayerName[MAX_LENGTH_STRING], RegelBuf[4096],
	     CopyStr[MAX_LENGTH_STRING];


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
		if (LineNr == 196118)
			ok = 1;

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

					if (stricmp(ValueStr, "SECTION") == 0)
					{
						Nesting++;
						FoundStr = 1;
					}

					if (stricmp(ValueStr, "SEQEND") == 0)
					{
						// TextLineNr
					}

					if (stricmp(ValueStr, "VERTEX") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							res = 1;
					}

					if (stricmp(ValueStr, "POLYLINE") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							NrPolylinePoints = 0;
					}

					if (stricmp(ValueStr, "ENDSEC") == 0)
					{
						Nesting--;

						if (Nesting == -1)
							res = 1;
					}

					if (stricmp(ValueStr, "TABLE") == 0)
						Nesting++;

					if (stricmp(ValueStr, "ENDTAB") == 0)
					{
						Nesting--;

						if (Nesting == 0)
							res = 1;
					}

					if (stricmp(ValueStr, "BLOCK") == 0)
					{
						Nesting++;
						BlockStartP[NrBlocks] = FileP;
					}

					if (stricmp(ValueStr, "ENDBLK") == 0)
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

					while ((cnt < NrDXFLayers) && (stricmp(DXFLayers[cnt], ValueStr) != 0))
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
//************************ IDD_DIALOG_DXFLAYER *****************************************************************************
//**************************************************************************************************************************

int32 CALLBACK DXFLayerDialogBody(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, NrSelectedLayers, cnt;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(171, "Select DXF layers"));
		SendDlgItemMessageUTF8(Dialog, IDC_STATIC1, WM_SETTEXT, 0, (LPARAM) SC(173, "Select layer"));
		SendDlgItemMessageUTF8(Dialog, IDOK, WM_SETTEXT, 0, (LPARAM) "OK");
		SendDlgItemMessageUTF8(Dialog, IDCANCEL, WM_SETTEXT, 0, (LPARAM) SC(46, "Cancel"));

		for (cnt = 0; cnt < NrDXFLayers; cnt++)
			res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);

		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SELITEMRANGE, (WPARAM) 1, (LPARAM) MAKELPARAM(0, NrDXFLayers));

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			NrSelectedLayers =
			    SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, MAX_NR_LAYERS, (LPARAM) & SelectedLayers);
			EndDialog(Dialog, NrSelectedLayers);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
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
	res =
	    DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_DXFLAYER), GEOMWindow, (DLGPROC) DXFLayerDialogBody);

	if (res <= 0)
		return -1;

	return res;
}

//*********************************************************************************************************************************
//********************* IDD_DIALOG_LAYER ******************************************************************************************
//*********************************************************************************************************************************

int32 CALLBACK LayerDialogBody(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
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
			SetWindowTextUTF8(Dialog, SC(172, "Select DXF layer"));

			for (cnt = 0; cnt < NrDXFLayers; cnt++)
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);
		}
		else
		{
			SetWindowTextUTF8(Dialog, SC(166, "Import DXF file"));

			for (cnt = 0; cnt < NrGeomLayers; cnt++)
				res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) LayerNames[cnt]);
		}

		SendDlgItemMessageUTF8(Dialog, IDOK, WM_SETTEXT, 0, (LPARAM) "OK");
		SendDlgItemMessageUTF8(Dialog, IDCANCEL, WM_SETTEXT, 0, (LPARAM) SC(46, "Cancel"));
		SendDlgItemMessageUTF8(Dialog, IDC_STATIC1, WM_SETTEXT, 0, (LPARAM) SC(173, "Select layer"));
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


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 ImportDXF()
{
	int32 FoundStr, StartPoint, BlockStart, HeaderStart, FoundRectangle, BlocksStart, TableStart, ArcCounterClockWise,
	      AngleInDegrees;
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

	int32 Found, res, TextAlignment, fp, PolyLineFlags, cnt, cnt2, cnt3, LineLength, Units, LineNr, LastEntityCommand,
	      Attributes, ReadMode, FilePos, FilePos2, PreviousFilePos, Nesting, CommandCode, ValueInt, NrPolylinePoints, ok,
	      LineNrP[5], BulgeFlags, LayerCnt, NrSelectedDXFLayers, Layer;
	double ValueDouble, X[10], Y[10], Z[10], Width, Height, Thickness, Diameter, Angle, LengthLine, Length2,
	       AnglesAreafill[256], BlockAngles[5], BlockInsertScaleValueX, BlockInsertScaleValueY, dx, dy;
	char NumberString[MAX_LENGTH_STRING], ValueStr[MAX_LENGTH_STRING], CurrentCommandString[MAX_LENGTH_STRING],
	     AttributeName[MAX_LENGTH_STRING], LayerName[MAX_LENGTH_STRING], TextString[MAX_LENGTH_STRING],
	     BlockName[MAX_LENGTH_STRING], LayerNameToCheck[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING],
	     str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     RegelBuf[4096], CopyStr[MAX_LENGTH_STRING], VarName[MAX_LENGTH_STRING], PolyLineLayerName[MAX_LENGTH_STRING];
	char EenString[10] = " ";
	double x1, y1, x2, y2, x3, y3;
	ObjectRecord NewObject;

	DialogMode = 1;
	NrGeomLayers = 0;
	/*
	  strcpy(LayerNames[NrGeomLayers++],SC(12,"Board outline"));
	  strcpy(LayerNames[NrGeomLayers++],SC(18,"Pads top"));
	  strcpy(LayerNames[NrGeomLayers++],SC(19,"Pads bottom"));
	  strcpy(LayerNames[NrGeomLayers++],"Soldermask pads top");
	  strcpy(LayerNames[NrGeomLayers++],"Soldermask pads bottom");
	  strcpy(LayerNames[NrGeomLayers++],"Pastemask pads top");
	  strcpy(LayerNames[NrGeomLayers++],"Pastemask pads bottom");
	  strcpy(LayerNames[NrGeomLayers++],SC(11,"Component outline"));
	  strcpy(LayerNames[NrGeomLayers++],SC(13,"Placement outline"));
	  strcpy(LayerNames[NrGeomLayers++],SC(95,"Silkscreen"));
	  strcpy(LayerNames[NrGeomLayers++],"Routing keepout");
	  strcpy(LayerNames[NrGeomLayers++],SC(14,"Drill plated"));
	  strcpy(LayerNames[NrGeomLayers++],SC(15,"Drill unplated"));
	*/
	strcpy(LayerNames[NrGeomLayers++], "Info 1");
	strcpy(LayerNames[NrGeomLayers++], "Info 2");
	strcpy(LayerNames[NrGeomLayers++], "Info 3");
	strcpy(LayerNames[NrGeomLayers++], "Info 4");

	NrGeomLayers = 0;
	/*
	  LayerIDs[NrGeomLayers++]=BOARD_OUTLINE_LAYER;
	  LayerIDs[NrGeomLayers++]=PAD_TOP_LAYER;
	  LayerIDs[NrGeomLayers++]=PAD_BOTTOM_LAYER;
	  LayerIDs[NrGeomLayers++]=SOLD_MASK_TOP_LAYER;
	  LayerIDs[NrGeomLayers++]=SOLD_MASK_BOTTOM_LAYER;
	  LayerIDs[NrGeomLayers++]=PASTE_MASK_TOP_LAYER;
	  LayerIDs[NrGeomLayers++]=PASTE_MASK_BOTTOM_LAYER;
	  LayerIDs[NrGeomLayers++]=COMP_OUTLINE_LAYER;
	  LayerIDs[NrGeomLayers++]=PLACEMENT_OUTLINE_LAYER;
	  LayerIDs[NrGeomLayers++]=SILKSCREEN_LAYER;
	  LayerIDs[NrGeomLayers++]=ROUTING_KEEPOUT_LAYER;
	  LayerIDs[NrGeomLayers++]=DRILL_LAYER;
	  LayerIDs[NrGeomLayers++]=DRILL_UNPLATED_LAYER;
	*/
	LayerIDs[NrGeomLayers++] = INFO_LAYER;
	LayerIDs[NrGeomLayers++] = INFO_LAYER2;
	LayerIDs[NrGeomLayers++] = INFO_LAYER3;
	LayerIDs[NrGeomLayers++] = INFO_LAYER4;

	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LAYER), GEOMWindow, (DLGPROC) LayerDialogBody);

	if (res < 0)
		return -1;

	Layer = LayerIDs[res];
	LastEntityCommand = -1;
	ClearObject(X);
	ClearObject(Y);
	ClearObject(Z);

//  Layer=INFO_LAYER2;

	ClearObject(NewObject);

	switch (Layer)
	{
	/*
	    case PAD_BOTTOM_LAYER:
	      break;
	    case PAD_TOP_LAYER:
	      break;
	    case SOLD_MASK_BOTTOM_LAYER:
	      break;
	    case SOLD_MASK_TOP_LAYER:
	      break;
	    case PASTE_MASK_BOTTOM_LAYER:
	      break;
	    case PASTE_MASK_TOP_LAYER:
	      break;
	    case ROUTING_KEEPOUT_LAYER:
	      break;
	    case BOARD_OUTLINE_LAYER:
	      break;
	    case SILKSCREEN_LAYER:
	      break;
	    case PLACEMENT_OUTLINE_LAYER:
	      break;
	    case COMP_OUTLINE_LAYER:
	      break;
	    case PIN_TEXT_LAYER:
	      break;
	    case DRILL_LAYER:
	      break;
	    case DRILL_UNPLATED_LAYER:
	      break;
	*/
	case INFO_LAYER:
		break;

	case INFO_LAYER2:
		break;

	case INFO_LAYER3:
		break;

	case INFO_LAYER4:
		break;

	default:
		return -1;
	}

	Thickness = CurrentInfoLine;

	NewObject.Layer = Layer;
	NewObject.Info = (OBJECT_SELECTED + 3);
	(*NewObjectPolygon).Info = OBJECT_SELECTED;
	(*NewObjectPolygon).NrVertices = 4;
	NewObject.PinNr = -1;
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

	if (GetNewFileUTF8
	        (GEOMWindow, NULL, ImportDXFFile, DXFFilePath, SC(175, "DXF files"), NULL, SC(166, "Import DXF file"), "dxf", 0) != 0)
		return -1;

	CheckInputMessages(0);
	strcpy(FileName, ImportDXFFile);

//  strcpy(FileName,"c:\\pcb_elegance\\hs80_rev3\\hs80_rev3.dxf");
//  strcpy(FileName,"c:\\own\\dxf\\smallpcb.dxf");
//  strcpy(FileName,"c:\\own\\dxf\\tube_preamp.dxf");
//  strcpy(FileName,"c:\\viewplot\\test_files\\dxf\\smallpcb.dxf ");


	GetLayersDXF(FileName);
// NrDXFLayers DXFLayers

//  strcpy(ImportLayerName,DXFLayers[res]);
	NrSelectedDXFLayers = SelectImportDXFLayer(0);

	if (NrSelectedDXFLayers <= 0)
		return -1;

	/*
	  ReWriteDXF(FileName);
	  return 0;
	*/

//  strcpy(ImportLayerName,"SILKSCREEN");
//  strcpy(ImportLayerName,"OUTLINE");
//  strcpy(ImportLayerName,"Arbrkwall");
//  strcpy(ImportLayerName,"ARINTELEV");

//  strcpy(ImportLayerName,"PADS");
//  strcpy(ImportLayerName,"LAYER1");
//  strcpy(ImportLayerName,"LAYER2");
//  strcpy(ImportLayerName,"VIAS");



	if ((fp = FileOpenReadOnlyUTF8(FileName)) <= 0)
	{
//    printf("File %s not found\n",SearchReplaceStringsFileName);
		return -1;
	}

	FilePointerDXF = 0;

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

// ***************************************************************************************************
// ***************************************************************************************************

		LineLength = ReadLnDXF2(RegelBuf, &FilePos, &FilePos2);
		/*
		    if (LineLength!=0) {
		      if (BlockNestingLevel>=0) {
		        BlockNestingLevel--;
		      }
		    }
		*/
#ifdef _DEBUG
		/*
		    switch (BlockNestingLevel) {
		      case 0:
		        sprintf(DebugString,"     %d",LineNr+1);
		        break;
		      case 1:
		        sprintf(DebugString,"          %d",LineNr+1);
		        break;
		      case 2:
		        sprintf(DebugString,"               %d",LineNr+1);
		        break;
		      default:
		        sprintf(DebugString,"%d",LineNr+1);
		        break;
		    }
		    WriteLn(fp2,DebugString);
		*/
#endif

// ***************************************************************************************************
// ***************************************************************************************************
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

// ***************************************************************************************************
// ***************************************************************************************************
//
// Work on the parameters of the previous command sequence
//
// ***************************************************************************************************
// ***************************************************************************************************

					if (BlockStart)
					{
						res = 1;
						strcpy(BlockNames[NrBlocks], AttributeName);
					}

					if (Nesting >= 1)
					{
						if (stricmp(CurrentCommandString, "VERTEX") == 0)
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

// ***************************************************************************************************
// ***************************************************************************************************
						if ((stricmp(CurrentCommandString, "SEQEND") == 0)
						        || (stricmp(CurrentCommandString, "LINE") == 0))
						{
// Process polyline/polygon points
//              PolyLineFlags & 1 == 1 -> Polygon
//              Width
//              LayerName
#ifdef _DEBUG
							if (LineNr == 843)
								res = 1;

#endif

							if (stricmp(CurrentCommandString, "LINE") == 0)
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

							/*
							              if (BlockNestingLevel==-1) {
							                strcpy(LayerNameToCheck,LayerName);
							              } else {
							                strcpy(LayerNameToCheck,BlockLayerName[0]);
							              }
							*/
							if (stricmp(CurrentCommandString, "LINE") == 0)
								strcpy(LayerNameToCheck, LayerName);
							else
								strcpy(LayerNameToCheck, PolyLineLayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								ok = 1;

								for (cnt = 0; cnt < NrPolylinePoints; cnt++)
								{
									PointsX[cnt] = UnitConvert(PolyLineX[cnt], Units);
									PointsY[cnt] = UnitConvert(PolyLineY[cnt], Units);
									VertexStartWidthTemp[cnt] = UnitConvert(VertexStartWidth[cnt], Units);
									VertexEndWidthTemp[cnt] = UnitConvert(VertexEndWidth[cnt], Units);
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

										PointsX[cnt] += UnitConvert(BlockInsertX[BlockNestingLevel], Units);
										PointsY[cnt] += UnitConvert(BlockInsertY[BlockNestingLevel], Units);
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

											PointsX[cnt] += UnitConvert(BlockInsertX[0], Units);
											PointsY[cnt] += UnitConvert(BlockInsertY[0], Units);
										}
									}
								}

// ***************************************************************************************************
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
									NewObject.ObjectType = OBJECT_LINE;
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
												NewObject.ObjectType = OBJECT_CIRCLE;
												NewObject.x1 = (float) ((PointsX[cnt] + PointsX[cnt + 1]) * 0.5);
												NewObject.y1 = (float) ((PointsY[cnt] + PointsY[cnt + 1]) * 0.5);
												NewObject.x2 = (float) (VertexStartWidthTemp[cnt] * 2.0);
												NewObject.y2 = 0.0;
												NewObject.x3 = 0.0;
												NewObject.Thickness = 0.0;
												NewObject.Layer = Layer;
												AddObject(&NewObject);
											}
											else
											{	// Trace
												NewObject.ObjectType = OBJECT_LINE;
												ConvNormalCoorToPolar(PointsX[cnt], PointsY[cnt], PointsX[cnt + 1],
												                      PointsY[cnt + 1], &Angle, &Length2);
												NewObject.x1 =
												    (float) (PointsX[cnt] +
												             VertexStartWidthTemp[cnt] * 0.5 * cos(Angle));
												NewObject.y1 =
												    (float) (PointsY[cnt] +
												             VertexStartWidthTemp[cnt] * 0.5 * sin(Angle));
												NewObject.x2 =
												    (float) (PointsX[cnt + 1] -
												             VertexStartWidthTemp[cnt] * 0.5 * cos(Angle));
												NewObject.y2 =
												    (float) (PointsY[cnt + 1] -
												             VertexStartWidthTemp[cnt] * 0.5 * sin(Angle));
												LengthLine =
												    CalcLengthLine(NewObject.x1, NewObject.y1, NewObject.x2,
												                   NewObject.y2);

												if (LengthLine < 0.01e6)
												{	// Round pad
													NewObject.ObjectType = OBJECT_CIRCLE;
													NewObject.x2 = (float) (VertexStartWidthTemp[cnt] * 2.0);
													NewObject.y2 = 0.0;
													NewObject.x3 = 0.0;
													NewObject.Info2 = 0;
													NewObject.Thickness = 0.0;
												}
												else
													NewObject.Thickness = (float) (VertexStartWidthTemp[cnt] * 2.0);

												NewObject.Layer = Layer;
												AddObject(&NewObject);
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
													NewObject.x1 = (float) ((PointsX[cnt] + PointsX[cnt + 1]) * 0.5);
													NewObject.y1 = (float) ((PointsY[cnt] + PointsY[cnt + 1]) * 0.5);
													NewObject.x2 = (float) VertexStartWidthTemp[cnt];
													NewObject.y2 = (float) LengthLine;
													NewObject.x3 = 0.0;
													NewObject.y3 = 0.0;
													NewObject.ObjectType = OBJECT_RECT;
													NewObject.Layer = Layer;
													NewObject.Thickness = 0.0;
#ifdef _DEBUG

													if (NewObject.Layer == 1)
														ok = 1;

#endif
													AddObject(&NewObject);
												}
												else
												{
													if (dy == 0.0)
													{	// Horizontal block
														NewObject.ObjectType = OBJECT_RECT;
														NewObject.x1 =
														    (float) ((PointsX[cnt] + PointsX[cnt + 1]) * 0.5);
														NewObject.y1 =
														    (float) ((PointsY[cnt] + PointsY[cnt + 1]) * 0.5);
														NewObject.x2 = (float) LengthLine;
														NewObject.y2 = (float) VertexStartWidthTemp[cnt];
														NewObject.x3 = 0.0;
														NewObject.y3 = 0.0;
														NewObject.Layer = Layer;
														NewObject.Thickness = 0.0;
#ifdef _DEBUG

														if (NewObject.Layer == 1)
															ok = 1;

#endif
														AddObject(&NewObject);
													}
													else
													{	// Diagonal polygon
														ok = 1;
														MakePolygonFromSpecialLine(PointsX[cnt], PointsY[cnt],
														                           PointsX[cnt + 1], PointsY[cnt + 1],
														                           NewObjectPolygon,
														                           VertexStartWidthTemp[cnt],
														                           VertexStartWidthTemp[cnt], 0);
														NewObjectPolygon->Layer = Layer;
														NewObjectPolygon->NrVertices = 4;
														AddObjectPolygon(NewObjectPolygon);
													}
												}

												if (AngleBetweenLines[cnt] < ANGLE_CONVERT(179.0))
												{
													NewObject.ObjectType = OBJECT_CIRCLE;
													NewObject.x1 = (float) PointsX[cnt];
													NewObject.y1 = (float) PointsY[cnt];
													NewObject.x2 = (float) VertexStartWidthTemp[cnt];
													NewObject.y2 = 0.0;
													NewObject.x3 = 0.0;
													NewObject.Thickness = 0.0;
													NewObject.Info2 = 0;
													NewObject.Layer = Layer;
													AddObject(&NewObject);
												}
											}
											else
											{	// Different start and end width -> Make polygon
												ok = 1;

												if (dx == 0.0)
												{	// Vertical aligned polygon
													NewObjectPolygon->Points[0].x =
													    PointsX[cnt] - VertexStartWidthTemp[cnt] * 0.5;
													NewObjectPolygon->Points[0].y = PointsY[cnt];
													NewObjectPolygon->Points[1].x =
													    PointsX[cnt] + VertexStartWidthTemp[cnt] * 0.5;
													NewObjectPolygon->Points[1].y = PointsY[cnt];
													NewObjectPolygon->Points[2].x =
													    PointsX[cnt + 1] + VertexEndWidthTemp[cnt] * 0.5;
													NewObjectPolygon->Points[2].y = PointsY[cnt + 1];
													NewObjectPolygon->Points[3].x =
													    PointsX[cnt + 1] - VertexEndWidthTemp[cnt] * 0.5;
													NewObjectPolygon->Points[3].y = PointsY[cnt + 1];
												}
												else
												{
													if (dy == 0.0)
													{	// Horizontal aligned polygon
														NewObjectPolygon->Points[0].x = PointsX[cnt];
														NewObjectPolygon->Points[0].y =
														    PointsY[cnt] - VertexStartWidthTemp[cnt] * 0.5;
														NewObjectPolygon->Points[1].x = PointsX[cnt];
														NewObjectPolygon->Points[1].y =
														    PointsY[cnt] + VertexStartWidthTemp[cnt] * 0.5;
														NewObjectPolygon->Points[2].x = PointsX[cnt + 1];
														NewObjectPolygon->Points[2].y =
														    PointsY[cnt + 1] + VertexEndWidthTemp[cnt] * 0.5;
														NewObjectPolygon->Points[3].x = PointsX[cnt + 1];
														NewObjectPolygon->Points[3].y =
														    PointsY[cnt + 1] - VertexEndWidthTemp[cnt] * 0.5;
													}
													else
													{	// Diagonal polygon
														ok = 1;
														MakePolygonFromSpecialLine(PointsX[cnt], PointsY[cnt],
														                           PointsX[cnt + 1], PointsY[cnt + 1],
														                           NewObjectPolygon,
														                           VertexStartWidthTemp[cnt],
														                           VertexEndWidthTemp[cnt], 0);
													}
												}

												NewObjectPolygon->Layer = Layer;
												NewObjectPolygon->NrVertices = 4;
												AddObjectPolygon(NewObjectPolygon);
											}
										}
									}
									else
									{
										NewObject.x1 = (float) PointsX[cnt];
										NewObject.y1 = (float) PointsY[cnt];
										NewObject.x2 = (float) PointsX[cnt + 1];
										NewObject.y2 = (float) PointsY[cnt + 1];
										NewObject.ObjectType = OBJECT_LINE;
										NewObject.Thickness = (float) VertexStartWidthTemp[cnt];
										NewObject.Layer = Layer;
										AddObject(&NewObject);
									}
								}
							}

							res = 1;
						}

// ***************************************************************************************************
// ***************************************************************************************************
						if ((stricmp(CurrentCommandString, "TRACE") == 0)
						        || (stricmp(CurrentCommandString, "SOLID") == 0))
						{
// filled polygon (4 points)
//    LayerName
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								for (cnt = 0; cnt < 4; cnt++)
								{
									PointsX[cnt] = UnitConvert(X[cnt], Units);
									PointsY[cnt] = UnitConvert(Y[cnt], Units);
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
										PointsX[cnt] += UnitConvert(BlockInsertX[BlockNestingLevel], Units);
										PointsY[cnt] += UnitConvert(BlockInsertY[BlockNestingLevel], Units);
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
											PointsX[cnt] += UnitConvert(BlockInsertX[0], Units);
											PointsY[cnt] += UnitConvert(BlockInsertY[0], Units);
										}
									}
								}

								for (cnt = 0; cnt < 4; cnt++)
								{
									NewObjectPolygon->Points[cnt].x = PointsX[cnt];
									NewObjectPolygon->Points[cnt].y = PointsY[cnt];
								}

								if (!CheckNoCrossesInObjectPolygon(NewObjectPolygon))
								{
									res = 1;
									NewObjectPolygon->Points[0].x = PointsX[1];
									NewObjectPolygon->Points[0].y = PointsY[1];
									NewObjectPolygon->Points[1].x = PointsX[0];
									NewObjectPolygon->Points[1].y = PointsY[0];
								}

								SetMinMaxObjectPolygon(NewObjectPolygon, 0);

								for (cnt = 0; cnt < 4; cnt++)
								{
									cnt2 = (cnt + 1) % 4;
									cnt3 = (cnt + 2) % 4;
									x1 = NewObjectPolygon->Points[cnt].x;
									y1 = NewObjectPolygon->Points[cnt].y;
									x2 = NewObjectPolygon->Points[cnt2].x;
									y2 = NewObjectPolygon->Points[cnt2].y;
									x3 = NewObjectPolygon->Points[cnt3].x;
									y3 = NewObjectPolygon->Points[cnt3].y;
									AnglesAreafill[cnt] = GetAngleBetweenLines(x1, y1, x2, y2, x3, y3);
								}

								FoundRectangle = 1;

								for (cnt = 0; cnt < 4; cnt++)
								{
									if (!InRangeSpecial(AnglesAreafill[cnt], PI * 0.5, 0.01))
										FoundRectangle = 0;
								}

								if (FoundRectangle)
								{
									NewObject.x1 = (float) ((NewObjectPolygon->maxx + NewObjectPolygon->minx) * 0.5);
									NewObject.y1 = (float) ((NewObjectPolygon->maxy + NewObjectPolygon->miny) * 0.5);
									NewObject.x2 = (float) (NewObjectPolygon->maxx - NewObjectPolygon->minx);
									NewObject.y2 = (float) (NewObjectPolygon->maxy - NewObjectPolygon->miny);
									NewObject.x3 = 0.0;
									NewObject.y3 = 0.0;
									NewObject.ObjectType = OBJECT_RECT;
									NewObject.Thickness = 0.0;
									NewObject.Layer = Layer;
									AddObject(&NewObject);
								}
								else
								{
									NewObjectPolygon->Layer = Layer;
									AddObjectPolygon(NewObjectPolygon);
								}

							}

							res = 1;
						}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmp(CurrentCommandString, "POLYLINE") == 0)
						{
							strcpy(PolyLineLayerName, LayerName);
// Start of polyline with the parameter:
//    Width
//    PolyLineLayerName
						}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmp(CurrentCommandString, "CIRCLE") == 0)
						{
// non filled cirkel with thickness of 0
//    Diameter
//    LayerName
							/*
							              if ((BlockNestingLevel==-1)
							                 ||
							                 (strcmp(BlockLayerName[BlockNestingLevel],"0")==0)) {
							                strcpy(LayerNameToCheck,LayerName);
							              } else {
							                strcpy(LayerNameToCheck,BlockLayerName[BlockNestingLevel]);
							              }
							*/
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								ok = 1;

								for (cnt = 0; cnt < 1; cnt++)
								{
									PointsX[cnt] = UnitConvert(X[cnt], Units);
									PointsY[cnt] = UnitConvert(Y[cnt], Units);
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
										PointsX[cnt] += UnitConvert(BlockInsertX[BlockNestingLevel], Units);
										PointsY[cnt] += UnitConvert(BlockInsertY[BlockNestingLevel], Units);
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
											PointsX[cnt] += UnitConvert(BlockInsertX[0], Units);
											PointsY[cnt] += UnitConvert(BlockInsertY[0], Units);
										}

										Diameter *= BlockInsertScaleY[0];
									}
								}

								NewObject.ObjectType = OBJECT_CIRCLE;
								NewObject.x1 = (float) PointsX[0];
								NewObject.y1 = (float) PointsY[0];
								NewObject.x2 = (float) UnitConvert((Diameter * 2.0), Units);
								NewObject.y2 = 0.0;
								NewObject.Thickness = 1.0;
								NewObject.Layer = Layer;
								NewObject.Info2 = 15;
								AddObject(&NewObject);
							}

							res = 1;
						}

// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmp(CurrentCommandString, "ARC") == 0)
						{
// Parameters :
//    StartAngle
//    EndAngle
//    Diameter
//    LayerName   atan2
							/*
							              if (BlockNestingLevel==-1) {
							                strcpy(LayerNameToCheck,LayerName);
							              } else {
							                strcpy(LayerNameToCheck,BlockLayerName[0]);
							              }
							*/
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								Angle = 0.0;

								for (cnt = 0; cnt < 1; cnt++)
								{
									PointsX[cnt] = UnitConvert(X[cnt], Units);
									PointsY[cnt] = UnitConvert(Y[cnt], Units);
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
										PointsX[cnt] += UnitConvert(BlockInsertX[BlockNestingLevel], Units);
										PointsY[cnt] += UnitConvert(BlockInsertY[BlockNestingLevel], Units);
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
											PointsX[cnt] += UnitConvert(BlockInsertX[0], Units);
											PointsY[cnt] += UnitConvert(BlockInsertY[0], Units);
										}

										Diameter *= BlockInsertScaleY[0];
									}
								}

								NewObject.ObjectType = OBJECT_ARC;
								NewObject.x1 = (float) PointsX[0];
								NewObject.y1 = (float) PointsY[0];
								NewObject.x2 = (float) UnitConvert((Diameter * 2.0), Units);
								NewObject.y2 = (float) UnitConvert((Diameter * 2.0), Units);
								NewObject.Thickness = 1.0;

								if (StartAngle - 0.01 > 360.0)
									StartAngle -= 360.0;

								NewObject.x3 = (float) (cos(ANGLE_CONVERT(StartAngle)) * NewObject.x2);
								NewObject.y3 = (float) (sin(ANGLE_CONVERT(StartAngle)) * NewObject.x2);
								NewObject.x4 = (float) (cos(ANGLE_CONVERT(EndAngle)) * NewObject.x2);
								NewObject.y4 = (float) (sin(ANGLE_CONVERT(EndAngle)) * NewObject.x2);
								NewObject.Layer = Layer;
								AddObject(&NewObject);
							}

							res = 1;
						}

// ***************************************************************************************************
// ***************************************************************************************************
						if (stricmp(CurrentCommandString, "INSERT") == 0)
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

								if (stricmp(AttributeName, "CONN4") == 0)
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
						if (stricmp(CurrentCommandString, "TEXT") == 0)
						{
// Text with parameters:
//   Height , Angle , TextString , LayerName
//   TextAlignment
//      0 = Left
//      1 = Baseline centered (Centre X)
//      2 = Right
//      4 = Centre

							/*
							              if (BlockNestingLevel==-1) {
							                strcpy(LayerNameToCheck,LayerName);
							              } else {
							                strcpy(LayerNameToCheck,BlockLayerName[0]);
							              }
							*/
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								Height *= BlockInsertScaleValueX;
								GetMinMaxText(PointsX[0], PointsY[0], Height, 0, 0.0, 0, 0, TextString);

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
									PointsX[cnt] = UnitConvert(X[cnt], Units);
									PointsY[cnt] = UnitConvert(Y[cnt], Units);
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
										PointsX[cnt] += UnitConvert(BlockInsertX[BlockNestingLevel], Units);
										PointsY[cnt] += UnitConvert(BlockInsertY[BlockNestingLevel], Units);
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
											PointsX[cnt] += UnitConvert(BlockInsertX[0], Units);
											PointsY[cnt] += UnitConvert(BlockInsertY[0], Units);
										}

										Height *= BlockInsertScaleY[0];
										Angle += BlockAngles[0];
									}
								}

#ifdef _DEBUG

								if (stricmp(TextString, "produktie") == 0)
									ok = 0;

								if (TextString[0] == 0)
									ok = 1;

#endif
								NewObject.ObjectType = OBJECT_TEXT;
								NewObject.x1 = (float) PointsX[0];
								NewObject.y1 = (float) PointsY[0];
								NewObject.x2 = (float) UnitConvert(Height, Units);
								NewObject.RotationAngle = (float) Angle;
								NewObject.Layer = Layer;
								NewObject.Thickness = (float) Thickness;
								memset(&NewObject.Text, 0, sizeof(NewObject.Text));
								strncpy(NewObject.Text, TextString, sizeof(NewObject.Text) - 1);
								AddObject(&NewObject);
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

					if (stricmp(ValueStr, "SECTION") == 0)
						Nesting++;

					if (stricmp(ValueStr, "SEQEND") == 0)
					{
						// TextLineNr
					}

					if (stricmp(ValueStr, "VERTEX") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							res = 1;
					}

					if (stricmp(ValueStr, "LINE") == 0)
					{
					}

					if (stricmp(ValueStr, "POLYLINE") == 0)
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

					if ((stricmp(ValueStr, "INSERT") == 0) || (stricmp(ValueStr, "TEXT") == 0))
					{
						// TextLineNr
						BlockInsertScaleValueX = 1.0;
						BlockInsertScaleValueY = 1.0;
					}

					if (stricmp(ValueStr, "ENDSEC") == 0)
					{
						Nesting--;

						if (Nesting == -1)
							res = 1;

						if (StartPoint)
						{
// ***************************************************************************************************
							res = 1;
// ***************************************************************************************************

						}
					}

					if (stricmp(ValueStr, "TABLE") == 0)
						Nesting++;

					if (stricmp(ValueStr, "ENDTAB") == 0)
					{
						Nesting--;

						if (Nesting == 0)
							res = 1;
					}

					if (stricmp(ValueStr, "BLOCK") == 0)
					{
						Nesting++;

						if (BlockNestingLevel == -1)
						{
							BlockStart = 1;
							BlockStartP[NrBlocks] = PreviousFilePos;
							BlockLineNr[NrBlocks] = LineNr - 2;
						}
					}

					if (stricmp(ValueStr, "ENDBLK") == 0)
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
					if (stricmp(CurrentCommandString, "INSERT") == 0)
						strcpy(BlockName, ValueStr);

					if (Nesting == 1)
					{
						if (stricmp(ValueStr, "HEADER") == 0)
						{
							HeaderStart = 1;
							BlockStart = 0;
							BlocksStart = 0;
							StartPoint = 0;
							TableStart = 0;
						}

						if (stricmp(ValueStr, "TABLES") == 0)
						{
							HeaderStart = 0;
							TableStart = 1;
							BlocksStart = 0;
							BlockStart = 0;
							StartPoint = 0;
						}

						if (stricmp(ValueStr, "BLOCKS") == 0)
						{
							HeaderStart = 0;
							TableStart = 0;
							BlocksStart = 1;
							BlockStart = 0;
							StartPoint = 0;
						}

						if (stricmp(ValueStr, "ENTITIES") == 0)
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
					/*
					            if (BlockStart) {
					              if (BlockNestingLevel>=0) {
					                strcpy(BlockLayerName[BlockNestingLevel],ValueStr);
					              }
					            } else {
					            }
					*/
					strcpy(LayerName, ValueStr);
					break;

				case 9:
					if (VarName[0] != 0)
					{	// Previous variable name
						if (stricmp(VarName, "$ANGDIR") == 0)
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

					if (stricmp(CurrentCommandString, "CIRCLE") == 0)
						Diameter = ValueDouble;

					if (stricmp(CurrentCommandString, "ARC") == 0)
						Diameter = ValueDouble;

					if (stricmp(CurrentCommandString, "TEXT") == 0)
						Height = ValueDouble;

					if (stricmp(CurrentCommandString, "POLYLINE") == 0)
						PolyLineStartWidth = ValueDouble;

					if (stricmp(CurrentCommandString, "VERTEX") == 0)
						StartWidth = ValueDouble;

					break;

				case 41:
					Attrib41 = 1;

					if (stricmp(CurrentCommandString, "POLYLINE") == 0)
						PolyLineEndWidth = ValueDouble;

					if (stricmp(CurrentCommandString, "VERTEX") == 0)
						EndWidth = ValueDouble;

					if (stricmp(CurrentCommandString, "INSERT") == 0)
						BlockInsertScaleValueX = ValueDouble;

					if (stricmp(CurrentCommandString, "TEXT") == 0)
						BlockInsertScaleValueX = ValueDouble;

					break;

				case 42:
					Attrib42 = 1;

					if (stricmp(CurrentCommandString, "POLYLINE") == 0)
						Width = ValueDouble;

					if (stricmp(CurrentCommandString, "INSERT") == 0)
						BlockInsertScaleValueY = ValueDouble;

					if (stricmp(CurrentCommandString, "VERTEX") == 0)
						BulgeFlags = ValueInt;

					break;

				case 50:
					Attrib50 = 1;

					if (stricmp(CurrentCommandString, "ARC") == 0)
					{
						StartAngle = ValueDouble;
#ifdef _DEBUG

						if (InRange2(ValueDouble, 45.0))
							res = 1;

#endif
					}

					if (stricmp(CurrentCommandString, "TEXT") == 0)
						Angle = ValueDouble;

					if (stricmp(CurrentCommandString, "INSERT") == 0)
						Angle = ValueDouble;

					break;

				case 51:
					Attrib51 = 1;

					if (stricmp(CurrentCommandString, "ARC") == 0)
						EndAngle = ValueDouble;

					break;

				case 66:
					Attrib66 = 1;
					Attributes = ValueInt;

					if (stricmp(CurrentCommandString, "POLYLINE") == 0)
					{
					}

					break;

				case 70:
					Attrib70 = 1;

					if (stricmp(CurrentCommandString, "POLYLINE") == 0)
						PolyLineFlags = ValueInt;

					break;

				case 71:
					Attrib71 = 1;
					break;

				case 72:
					Attrib72 = 1;

					if (stricmp(CurrentCommandString, "TEXT") == 0)
						TextAlignment = ValueInt;

					break;
				}
			}

			Found++;
		}

		PreviousFilePos = FilePos;
	}

	DeAllocateMemTemp();

	/*
	  SendMessage(SelectLayerWindow,CB_SETCURSEL,CurrentDrawingLayer,0);
	  SetWindowName(1);

	//  GetMinMaxSelectedObjects();
	  ViewFull();
	*/
	/*
	  sprintf(str,SC(620,"minx,miny = %.4f,%.4f : maxx,maxy = %.4f,%.4f"),
	              SearchMinX/100000.0,SearchMinY/100000.0,
	              SearchMaxX/100000.0,SearchMaxY/100000.0);
	  MessageBoxUTF8(VIEWPLOTWindow,str,SC(621,SC(182,"Info message")),MB_APPLMODAL|MB_OK);
	*/

//  GetMaxRectSelectedObjects(0,&MinX,&MinY,&MaxX,&MaxY);
	GetMinMaxSelectedObjects();
	ViewFull();
	sprintf(str,
	        SC(181, "minx,miny = %.4f,%.4f : maxx,maxy = %.4f,%.4f\n\nFor scaling use the right mouse button menu"),
	        SelectedMinX / 100000.0, SelectedMinY / 100000.0, SelectedMaxX / 100000.0, SelectedMaxY / 100000.0);
	MessageBoxUTF8(GEOMWindow, str, SC(182, "Info message"), MB_APPLMODAL | MB_OK);
	return 0;
}
