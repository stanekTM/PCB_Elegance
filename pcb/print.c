/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: print.c
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
#include "graphics.h"
#include "windows.h"
#include "memory.h"
#include "print.h"
#include "stdio.h"
#include "draw2.h"
#include "calcdef.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "line2.h"
#include "rect.h"
#include "pcb.h"
#include "math.h"
#include "files2.h"
#include "resource.h"
#include "draw.h"
#include "ellipss.h"
#include "string.h"
#include "toets.h"
#include "mainloop.h"
#include "commdlg.h"
#include "select.h"
#include "calc4.h"
#include "stdio.h"
#include "dialogs.h"
#include "gerber.h"
#include "plot.h"
#include "polygon.h"
#include "time.h"
#include "font.h"
#include "own_zlib.h"
#include "ctype.h"
#include "../functionsc/version.h"

//#define  Mult2(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr))) : ((int32)(Factor*(Nr))) )
#define  Mult2(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr)+0.5)) : ((int32)(Factor*(Nr)-0.5)) )
//#define  Mult2(Nr) ( (((Nr))>(0)) ? ((int32)(Factor*(Nr)+0.5)) : ((int32)(Factor*(Nr)+0.5)) )

#define  PDFConvX(x)  (((x-PDFStartX)*PDFMultFactor+PDFPageOffsetX)*PDFMultFactor2);
#define  PDFConvY(y)  (((y-PDFStartY)*PDFMultFactor+PDFPageOffsetY)*PDFMultFactor2);
#define  PDFConv(x)   (x*PDFMultFactor);

//#define  MIN_PLOT_DRILL        50000.0
#define  MIN_PLOT_DRILL        80000.0

#pragma pack(2)

typedef struct
{
	int32 TagNr, FieldType;
	int32 Count, Value;
} TiffTagRecord;

/*

FieldType:

 1 = uint8
 2 = string with zero
 3 = int32
 4 = int32
 5 = Pointer to file position (On this position are int32 numbers (value1,value2 stored)
        the result = value1/value2

*/

typedef struct
{
	int32 Identifier, Version;
	int32 FirstImageFileDirectoryOffset;
	TiffTagRecord Tags[30];
} TIFFHeaderRecord;

#pragma pack(1)

typedef struct
{
	uint8 Separator;
	int32 ImageLeft;
	int32 ImageTop;
	int32 ImageWidth;
	int32 ImageHeight;
	uint8 ColorMode;
} GifImageDescriptorRecord;

typedef struct
{
	uint8 Separator;
	uint8 FunctionCode;
	uint8 ByteCount;
	uint8 FunctionData[4];
	uint8 Zero;
} GifExtensionBlockRecord;

#pragma pack()

HPEN OldPen, PlotPen, PlotWhitePen;
HBRUSH PlotBlackBrush, PlotWhiteBrush, NewEmptyBrush, OldBrush;
LOGBRUSH PlotBlackBrushObject, PlotWhiteBrushObject, NewEmptyBrushObject;

HDC OutputDisplayCopy;

int32 ReverseY = 1;
int32 ReverseX, Printing, Font90;

double TextHeight = (200 * 2540);

char PDFCreatorOrganisation[MAX_LENGTH_STRING], PDFCreatorName[MAX_LENGTH_STRING], PDFSubject[MAX_LENGTH_STRING],
     PDFTitle[MAX_LENGTH_STRING];



double PrintOffsetX, PrintOffsetY, TempScale, PDFMultFactor, PDFMultFactor2, PDFPageOffsetX, PDFPageOffsetY, PDFStartX,
       PDFStartY, PDFNewLineWidth, PDFLineWidth;
int32 PDFReverse, PDFCurrentColor, StringCount, PrintToBitmap, ok, CurrentPlotThickNess, BrushType, PixelsPerInchY,
      RoundingNr, RoundingCnt, PolygonNr, StartPdfPos, pdffp, NrPDFObjects, *PDFObjectPos, Plotfp;



extern COLORREF LineColor;
extern HDC OutputDisplay;
extern HPEN EmptyPen;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY, LastGerberX, LastGerberY, CurrentPlotPenSize, DesignBoardOriginX,
       DesignBoardOriginY, DesignBoardWidth, DesignBoardHeight;

// ***************************************************************************************

PRINTDLG Pd;
DOCINFO PdDocInfo;
LPSTR PrinterDocName = "Printer document";

//LPSTR                       Font1Str="Arial";
LPSTR Font1Str = "Courier New";
//LPSTR                       Font1Str="Timer New Roman";
//LPSTR                       Font1Str="Century Gothic";

//LPSTR                       Font2Str="Arial";
//LPSTR                       Font2Str="Courier New";
//LPSTR                       Font2Str="MS Serif";

TEXTMETRIC Font1Metrics, Font2Metrics;
HFONT PrinterFont, PrinterFont90;


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PowerPlaneToPlot(int32 Layer, int32 mode);

int32 AreaFillToPlot(AreaFillRecord * AreaFill, int32 mode);

int32 WritePlotText(int32 Layer, double BoardMinX, double BoardMinY, double BoardMaxX, double BoardMaxY, int32 mode);

int32 LayerPlotOutput(int32 Layer, int32 mode);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CALLBACK ScaleDialog2(HWND Dialog, uint16 Message, uint16 WParam, int32 LParam)
{
	int32 about;
	double value;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetWindowTextUTF8(Dialog, SC(301, "Scale"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDD_SCALE1X1, SC(306, "Scale 1X"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(185, "Scale factor"));
		
		sprintf(str, "%.4f", TempScale);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & str);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDD_SCALE1X1:
			TempScale = 1.0;
			EndDialog(Dialog, 1);
			return about;

		case IDOK:
			if ((GetDialogFloatValue(Dialog, IDC_EDIT1, &value) == 0) || (value < 0.1) || (value > 10.0))
			{
				MessageBoxOwn(PCBWindow, SC(1020, "Scale factor between 0.1 .. 10"), SC(24, "Error"),
				              MB_APPLMODAL | MB_OK);
				return about;
			}

			TempScale = value;
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

int32 ScaleDialog(double *Scale, int32 Mode)
{
	int res;

	TempScale = *Scale;
	res = DialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PLOT_SCALE), PCBWindow, (DLGPROC) ScaleDialog2);

	if (res == 1)
	{
		*Scale = TempScale;
		return 1;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void OutputToPrinter(int32 mode)
{
	int32 PixelsPerInchX, PixelsPerInchY, res2, PageX, PageY, minx, maxx, miny, maxy, PagePixelsY, OldDrawWindowMinX,
	      OldDrawWindowMinY, OldDrawWindowMaxX, OldDrawWindowMaxY;
	double Xoffset2, Yoffset2, Factor2, MinX, MaxX, MinY, MaxY, Factor1x, Factor3, TempFactor, FactorX, FactorY;
	int32 res;

	memset(&Pd, 0, sizeof(PRINTDLG));
	Pd.lStructSize = sizeof(PRINTDLG);
	Pd.hwndOwner = PCBWindow;
	Pd.Flags = PD_RETURNDC;
	PdDocInfo.cbSize = sizeof(DOCINFO);
	PdDocInfo.lpszDocName = PrinterDocName;

	if ((res = PrintDlg(&Pd)) != 0)
	{

		UnselectAll = 1;
		SelectObjectsFromWindow(0.0, 0.0, 0.0, 0.0, 0);
		UnselectAll = 0;

		OldDrawWindowMinX = DrawWindowMinX;
		OldDrawWindowMinY = DrawWindowMinY;
		OldDrawWindowMaxX = DrawWindowMaxX;
		OldDrawWindowMaxY = DrawWindowMaxY;
		Xoffset2 = Xoffset;
		Yoffset2 = Yoffset;
		Factor2 = Factor;

		res2 = SetMapMode(Pd.hDC, MM_TEXT);	// 0.1 mm
//    res2=SetMapMode(Pd.hDC,MM_LOMETRIC);   // 0.1 mm
		res2 = StartDoc(Pd.hDC, &PdDocInfo);
		res2 = StartPage(Pd.hDC);


		PixelsPerInchX = GetDeviceCaps(Pd.hDC, LOGPIXELSX);
		PixelsPerInchY = GetDeviceCaps(Pd.hDC, LOGPIXELSY);
		PageX = GetDeviceCaps(Pd.hDC, HORZSIZE);
		PageY = GetDeviceCaps(Pd.hDC, VERTSIZE);
		PagePixelsY = GetDeviceCaps(Pd.hDC, VERTRES);

		minx = max(0, DrawWindowMinX);
		maxx = min(10000, DrawWindowMaxX);
		miny = max(0, DrawWindowMinY);
		maxy = min(10000, DrawWindowMaxY);
		ViewMinX = PixelToRealOffX(minx - 1);
		ViewMaxX = PixelToRealOffX(maxx + 1);
		ViewMinY = PixelToRealOffY(DrawWindowMaxY - (maxy + 1) - 1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY - (miny - 1) - 1);

		MinX = ViewMinX;
		MinY = ViewMinY;
		MaxX = ViewMaxX;
		MaxY = ViewMaxY;

		ReverseY = 1;

		FactorX = (PageX / (MaxX - MinX)) * PixelsPerInchX * 0.9 / 25.4;
		FactorY = (PageY / (MaxY - MinY)) * PixelsPerInchX * 0.9 / 25.4;
		Factor3 = min(FactorX, FactorY);
		/*
		    Fact2=((MaxX-MinX)*210)/((MaxY-MinY)*297);
		    if (Fact2<1.0) {
		      Factor*=Fact2;
		    }
		*/
		Factor1x = (PixelsPerInchY / 2540000.0);
		TempFactor = Factor3 / Factor1x;

		if (ScaleDialog(&TempFactor, 0) == 1)
			Factor3 = Factor1x * TempFactor;
		else
		{
//      EndPage(Pd.hDC);
//      EndDoc(Pd.hDC);
			DeleteDC((HDC) & Pd);
			return;
		}


// *******************************************************************

		CheckInputMessages(0);

		DrawWindowMinX = 0L;
		DrawWindowMinY = -100000L;
		DrawWindowMaxX = 100000L;
		DrawWindowMaxY = PagePixelsY;
		Xoffset = MinX - ((MaxX - MinX) * 0.05);
		Yoffset = MinY - ((MaxY - MinY) * 0.05);


// *******************************************************************

		Factor = Factor3;

		DeleteGraphicObjects();
		Printing = 1;
		CreateDrawObjects(0);

		SetBkMode(OutputDisplay, TRANSPARENT);
		OutputDisplay = Pd.hDC;

		DrawObjectLines(32);
		DrawObjectRects(32);
		DrawObjectArcs(32, 0);
		DrawObjectTexts2(32);
		DrawObjectPolygons(32);

		if ((DrawSoldMaskBottomMode == 2) || (DrawSoldMaskTopMode == 2))
			DrawViaSoldMaskPads(0);

// ********************************************************************************************************
// Draw areafills layer != CurrentDrawingLayer
		DrawAreaFills(16);

		if (OkToDrawClearances)
			DrawAreaFills(16 + 8);

// Draw pads on layer != CurrentDrawingLayer
		DrawComps(16);

		if (OkToDrawClearances)
			DrawPinsComps(16 + 8);

// Draw traces layer != CurrentDrawingLayer
		DrawTraces(16);

		if (OkToDrawClearances)
			DrawTracesWithClearance(16);

// Draw arc/all angle traces layer != CurrentDrawingLayer
		DrawObjectLines(16);
		DrawObjectRects(16);
		DrawObjectArcs(16, 0);
		DrawObjectTexts2(16);
		DrawObjectPolygons(16);

		if (OkToDrawClearances)
		{
			DrawObjectLines(16 + 8);
			DrawObjectRects(16 + 8);
			DrawObjectArcs(16 + 8, 0);
			DrawObjectPolygons(16 + 8);
		}

// ********************************************************************************************************
// Draw areafills layer == CurrentDrawingLayer
		DrawAreaFills(0);
		CurrentObjectCode = -1;

		if (OkToDrawClearances)
			DrawAreaFills(8);

// Draw pads on layer == CurrentDrawingLayer
		DrawComps(0);

		if (OkToDrawClearances)
			DrawPinsComps(8);

// Draw traces layer == CurrentDrawingLayer
		DrawTraces(0);

		if (OkToDrawClearances)
			DrawTracesWithClearance(0);

// Draw arc/all angle traces layer == CurrentDrawingLayer
		DrawObjectLines(0);
		DrawObjectRects(0);
		DrawObjectArcs(0, 0);
		DrawObjectTexts2(0);
		DrawObjectPolygons(0);

		if (OkToDrawClearances)
		{
			DrawObjectLines(8);
			DrawObjectRects(8);
			DrawObjectArcs(8, 0);
			DrawObjectPolygons(8);
		}

// ********************************************************************************************************
		if (OkToDrawVias)
			DrawVias(0);

		if ((OkToDrawVias) && ((OkToDrawClearances) || (OkToDrawViaClearances)))
			DrawVias(8);

		if (OkToDrawConnections)
			DrawConnections(0);

		DrawErrorObjects();

		if (DrawDrillMode > 0)
		{
			DrawObjectArcs(0, 1);
			DrawDrillsComps(0);
			DrawViaDrills(0);

			if (OkToDrawClearances)
			{
				DrawObjectArcs(8, 1);
				DrawDrillsComps(8);
			}
		}

		InitDrawingObject(0, FIXED_COLOR_LAYER, 1, GRAPHICS_GRAY + DRAW_WITH_PEN_AND_NOT_FILLED);
//    rect3(Mult2(Design.BoardOriginX+Design.BoardWidth/2-Xoffset)+DrawWindowMinX,
//          DrawWindowMaxY-Mult2(Design.BoardOriginY+Design.BoardHeight/2-Yoffset)-1,
//          Mult2(Design.BoardWidth),Mult2(Design.BoardHeight));
		DrawLine(Mult2(-Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult2(25400.0 - Yoffset) - 1,
		         Mult2(-Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult2(-25400.0 - Yoffset) - 1);
		DrawLine(Mult2(-25400.0 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult2(-Yoffset) - 1,
		         Mult2(25400.0 - Xoffset) + DrawWindowMinX, DrawWindowMaxY - Mult2(-Yoffset) - 1);

// *************************************************************************************

		Printing = 0;

		DrawWindowMinX = OldDrawWindowMinX;
		DrawWindowMinY = OldDrawWindowMinY;
		DrawWindowMaxX = OldDrawWindowMaxX;
		DrawWindowMaxY = OldDrawWindowMaxY;

		Xoffset = Xoffset2;
		Yoffset = Yoffset2;
		Factor = Factor2;

		EndPage(Pd.hDC);
		//        Escape(Pd.hDC,NEWFRAME,0,NULL,NULL);
		EndDoc(Pd.hDC);
		//        Escape(Pd.hDC,ENDDOC,0,NULL,NULL);
		DeleteDC((HDC) & Pd);
		DeleteGraphicObjects();
		CreateDrawObjects(0);
		MessageBoxOwn(PCBWindow, SC(1021, "Printing ready"), SC(1, "Message"), MB_APPLMODAL | MB_OK);

	}
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************


void InitDrawingPlot(int32 ThickNess)
{
	if (ThickNess < 1)
		ThickNess = 1;

	if (PlotPen == (HGDIOBJ) NULL)
	{
		CurrentPlotThickNess = ThickNess;
		ThickNess = (ThickNess - 1) | 1;
		PlotPen = CreatePen(PS_SOLID, ThickNess, RGB_Black);
	}
	else
	{
		if (CurrentPlotThickNess != ThickNess)
		{
			CurrentPlotThickNess = ThickNess;
			ThickNess = (ThickNess - 1) | 1;
			SelectObject(OutputDisplay, OldPen);
			DeleteObject(PlotPen);
			PlotPen = CreatePen(PS_SOLID, ThickNess, RGB_Black);
		}
	}

	if (OldPen == 0)
		OldPen = SelectObject(OutputDisplay, PlotPen);
	else
		SelectObject(OutputDisplay, PlotPen);

	LineColor = RGB_Black;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************


void InitDrawingNoPen()
{
	if (OldPen == 0)
		OldPen = SelectObject(OutputDisplay, EmptyPen);
	else
		SelectObject(OutputDisplay, EmptyPen);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitDrawingNewEmptyBrush()
{
	if (NewEmptyBrush == (HGDIOBJ) NULL)
	{
		NewEmptyBrushObject.lbHatch = (LONG) NULL;
		NewEmptyBrushObject.lbStyle = BS_NULL;
		NewEmptyBrushObject.lbColor = (LONG) NULL;
		NewEmptyBrush = CreateBrushIndirect(&NewEmptyBrushObject);

		if (OldBrush == 0)
			OldBrush = SelectObject(OutputDisplay, NewEmptyBrush);
		else
			SelectObject(OutputDisplay, NewEmptyBrush);

		BrushType = 1;
	}
	else
		SelectObject(OutputDisplay, NewEmptyBrush);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitDrawingSolidBrushBlack()
{
	if (PlotBlackBrush == (HGDIOBJ) NULL)
	{
		PlotBlackBrushObject.lbHatch = (LONG) NULL;
		PlotBlackBrushObject.lbStyle = BS_SOLID;
		PlotBlackBrushObject.lbColor = RGB_Black;
		PlotBlackBrush = CreateBrushIndirect(&PlotBlackBrushObject);

		if (OldBrush == 0)
			OldBrush = SelectObject(OutputDisplay, PlotBlackBrush);
		else
			SelectObject(OutputDisplay, PlotBlackBrush);

		BrushType = 2;
	}
	else
		SelectObject(OutputDisplay, PlotBlackBrush);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InitDrawingSolidBrushWhite()
{
	if (PlotWhiteBrush == (HGDIOBJ) NULL)
	{
		PlotWhiteBrushObject.lbHatch = (LONG) NULL;
		PlotWhiteBrushObject.lbStyle = BS_SOLID;
		PlotWhiteBrushObject.lbColor = RGB_White;
		PlotWhiteBrush = CreateBrushIndirect(&PlotWhiteBrushObject);

		if (OldBrush == 0)
			OldBrush = SelectObject(OutputDisplay, PlotWhiteBrush);
		else
			SelectObject(OutputDisplay, PlotWhiteBrush);

		BrushType = 3;
	}
	else
		SelectObject(OutputDisplay, PlotWhiteBrush);

	if (PlotWhitePen == NULL)
		PlotWhitePen = CreatePen(PS_SOLID, 1, RGB_White);

	if (OldPen == 0)
		OldPen = SelectObject(OutputDisplay, PlotWhitePen);
	else
		SelectObject(OutputDisplay, PlotWhitePen);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeletePlotObjects()
{

	if (BrushType != 0)
	{
		SelectObject(OutputDisplay, OldBrush);
		OldBrush = 0;
	}

	if (NewEmptyBrush != 0)
	{
		DeleteObject(NewEmptyBrush);
		NewEmptyBrush = 0;
	}

	if (PlotWhiteBrush != 0)
	{
		DeleteObject(PlotWhiteBrush);
		PlotWhiteBrush = 0;
	}

	if (PlotBlackBrush != 0)
	{
		DeleteObject(PlotBlackBrush);
		PlotBlackBrush = 0;
	}

	if (CurrentPlotThickNess != 0)
	{
		SelectObject(OutputDisplay, OldPen);
		DeleteObject(PlotPen);
		PlotPen = 0;
		CurrentPlotThickNess = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AreaFillToPlot(AreaFillRecord * AreaFill, int32 Mode)
{
	typedef POINT AreafillPointsArray[MAX_LENGTH_STRING];

	int32 cnt2, cnt3, count, Layer, x, y;
	double x11, y11;
	PolygonRecord *DrawPolygon;
	uint8 *AreaPos, *PolygonPos;
	char str[MAX_LENGTH_STRING];
	AreafillPointsArray *Points;

	Layer = AreaFill->Layer;
//  DrawCode=DrawLayerCode[Layer];
//  InitDrawingObject(TRACE_HOR,DrawCode,0,NORMAL_FILLED_AND_PEN1);

	AreaPos = (uint8 *) AreaFill;
	count = sizeof(AreaFillRecord);

	DrawPolygon = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
	PolygonPos = (uint8 *) DrawPolygon;

	count = DrawPolygon->NrVertices;
	AllocateSpecialMem(MEM_POINTS, count * sizeof(POINT) + 16384, (void **) &Points);

	if ((Mode & 1) == 0)
	{
		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			if (!ReverseY)
				y = Mult2((*DrawPolygon).Points[cnt3].y - PrintOffsetY);
			else
				y = DrawWindowMaxY - Mult2((*DrawPolygon).Points[cnt3].y - PrintOffsetY);

			if (!ReverseX)
				x = Mult2((*DrawPolygon).Points[cnt3].x - PrintOffsetX);
			else
				x = DrawWindowMaxX - Mult2((*DrawPolygon).Points[cnt3].x - PrintOffsetX);

			(*Points)[cnt3].x = x;
			(*Points)[cnt3].y = y;
		}

		InitDrawingPlot(1);
		InitDrawingSolidBrushBlack();
		Polygon(OutputDisplay, (POINT *) Points, count);
	}
	else
	{
		if (PDFCurrentColor == 0)
		{
			AddToMessageBuf("0 g");
			AddToMessageBuf("0 G");
			PDFCurrentColor = 1;
		}

		for (cnt3 = 0; cnt3 < count; cnt3++)
		{
			x11 = PDFConvX(DrawPolygon->Points[cnt3].x);
			y11 = PDFConvY(DrawPolygon->Points[cnt3].y);
			sprintf(str, "%.3f %.3f", x11, y11);

			if (cnt3 == 0)
				strcat(str, " m");
			else
				strcat(str, " l");

			AddToMessageBuf(str);
		}

		AddToMessageBuf("h f");
	}

// **************************************************************************************

	PolygonPos += MemSizePolygon(DrawPolygon);
	DrawPolygon = (PolygonRecord *) PolygonPos;

	if ((Mode & 1) == 0)
	{
		InitDrawingSolidBrushWhite();

		for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
		{
			count = DrawPolygon->NrVertices;
			AllocateSpecialMem(MEM_POINTS, count * sizeof(POINT) + 16384, (void **) &Points);

			if ((DrawPolygon->PolygonType & 8) == 0)
			{
				for (cnt3 = 0; cnt3 < count; cnt3++)
				{
					if (!ReverseY)
						y = Mult2((*DrawPolygon).Points[cnt3].y - PrintOffsetY);
					else
						y = DrawWindowMaxY - Mult2((*DrawPolygon).Points[cnt3].y - PrintOffsetY);

					if (!ReverseX)
						x = Mult2((*DrawPolygon).Points[cnt3].x - PrintOffsetX);
					else
						x = DrawWindowMaxX - Mult2((*DrawPolygon).Points[cnt3].x - PrintOffsetX);

					(*Points)[cnt3].x = x;
					(*Points)[cnt3].y = y;
				}

				Polygon(OutputDisplay, (POINT *) Points, count);
			}

			PolygonPos += MemSizePolygon(DrawPolygon);
			DrawPolygon = (PolygonRecord *) PolygonPos;
		}
	}
	else
	{
		PDFReverse = 1;
		AddToMessageBuf("1 g");
		PDFCurrentColor = 0;

		for (cnt2 = 1; cnt2 < AreaFill->NrPolygons; cnt2++)
		{
			if ((DrawPolygon->PolygonType & 8) == 0)
			{
				count = DrawPolygon->NrVertices;

				for (cnt3 = 0; cnt3 < count; cnt3++)
				{
					x11 = PDFConvX(DrawPolygon->Points[cnt3].x);
					y11 = PDFConvY(DrawPolygon->Points[cnt3].y);
					sprintf(str, "%.3f %.3f", x11, y11);

					if (cnt3 == 0)
						strcat(str, " m");
					else
						strcat(str, " l");

					AddToMessageBuf(str);
				}

				AddToMessageBuf("h f");
			}

			PolygonPos += MemSizePolygon(DrawPolygon);
			DrawPolygon = (PolygonRecord *) PolygonPos;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PowerPlaneToPlot(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt3, res, count, NrLayerPolygons, NrDeletionPolygons, PowerPlaneNr, LayerPolygonsInfo[256],
	      DeletionPolygonsInfo[256];
	double xmin, xmax, ymin, ymax, x11, y11, Xmin, Ymin, Xmax, Ymax;
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
	res = FindMinMaxBoard(&Xmin, &Ymin, &Xmax, &Ymax, 3);

	xmin = Xmin - 100 * 2540.0;
	ymin = Ymin - 100 * 2540.0;
	xmax = Xmax + 100 * 2540.0;
	ymax = Ymax + 100 * 2540.0;
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

	AreaFillToPlot(TempAreaFill, mode & 1);
//  AreaFillToPlot(TempAreaFill,Thickness1,Thickness2,0);

	for (cnt2 = 0; cnt2 < NrDeletionPolygons; cnt2++)
	{
		if (DeletionPolygonsInfo[cnt2] == 0)
		{
			DeletionPolygon = DeletionPolygons[cnt2];
			count = DeletionPolygon->NrVertices;
			xmin = 1000000000;
			ymin = 1000000000;
			xmax = -1000000000;
			ymax = -1000000000;

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

			AreaFillToPlot(TempAreaFill, mode & 1);
//      AreaFillToPlot(TempAreaFill,Thickness1,Thickness2,0);
			DeletionPolygonsInfo[cnt2] = 1;
		}
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlotTraces(int32 Layer, int32 Mode)
{
	TraceRecord *Trace;
	double xx1, yy1, xx2, yy2, x11, y11, x22, y22, yy2a, dikte2;
	int32 x1, y1, x2, y2, dikte, cnt;
	char str[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 ok;
#endif

	if ((Mode & 1) == 0)
	{
		InitDrawingPlot(1);
		InitDrawingSolidBrushBlack();
	}

	for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
	{
		Trace = &((*VerTraces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			xx1 = Trace->X;
			xx2 = xx1;
			yy2a = Trace->ThickNess * 0.5;
			yy1 = Trace->Y;
			yy2 = yy1 + Trace->Length;
			dikte2 = Trace->ThickNess;

			if ((xx1 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
			{
				if ((yy2 + yy2a >= ViewMinY) && (yy1 - yy2a <= ViewMaxY))
				{
					if ((Mode & 1) == 0)
					{
						x1 = Mult2(xx1 - PrintOffsetX);
						y1 = Mult2(yy1 - PrintOffsetY);
						y2 = Mult2(yy2 - PrintOffsetY);
						x2 = Mult2(xx2 - PrintOffsetX);
						dikte = Mult2(dikte2);

						if (!ReverseX)
							DrawVerLine(x1, y1, y2, dikte, 2);
						else
							DrawVerLine(DrawWindowMaxX - x1, y1, y2, dikte, 2);
					}
					else
					{
						PDFNewLineWidth = Trace->ThickNess * PDFMultFactor * PDFMultFactor2;

						if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
						{
							PDFLineWidth = PDFNewLineWidth;
							sprintf(str, "%.3f w", PDFLineWidth);
							AddToMessageBuf(str);
						}

						x11 = PDFConvX(xx1);
						y11 = PDFConvY(yy1);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(xx2);
						y22 = PDFConvY(yy2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);
					}
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
	{
		Trace = &((*HorTraces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			yy1 = Trace->Y;
			yy2 = yy1;
			yy2a = Trace->ThickNess * 0.5;
			xx1 = Trace->X;
			xx2 = xx1 + Trace->Length;
			dikte2 = Trace->ThickNess;

			if ((yy1 + yy2a >= ViewMinY) && (yy1 - yy2a <= ViewMaxY))
			{
				if ((xx2 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
				{
					if ((Mode & 1) == 0)
					{
						x1 = Mult2(xx1 - PrintOffsetX);
						y1 = Mult2(yy1 - PrintOffsetY);
						x2 = Mult2(xx2 - PrintOffsetX);
						y2 = Mult2(yy2 - PrintOffsetY);
						dikte = Mult2(dikte2);

						if (!ReverseX)
							DrawHorLine(x1, y1, x2, dikte, 0);
						else
							DrawHorLine(DrawWindowMaxX - x2, y1, DrawWindowMaxX - x1, dikte, 2);
					}
					else
					{
						PDFNewLineWidth = Trace->ThickNess * PDFMultFactor * PDFMultFactor2;

						if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
						{
							PDFLineWidth = PDFNewLineWidth;
							sprintf(str, "%.3f w", PDFLineWidth);
							AddToMessageBuf(str);
						}

						x11 = PDFConvX(xx1);
						y11 = PDFConvY(yy1);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(xx2);
						y22 = PDFConvY(yy2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);
					}
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
	{
		Trace = &((*Diag1Traces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{

			xx1 = Trace->X;
			xx2 = xx1 + Trace->Length;
			yy1 = Trace->Y;
			yy2 = yy1 - Trace->Length;
			yy2a = Trace->ThickNess * 0.5;
			dikte2 = Trace->ThickNess;

			if ((xx2 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
			{
				if ((yy1 + yy2a >= ViewMinY) && (yy1 - (xx2 - xx1) - yy2a <= ViewMaxY))
				{
#ifdef _DEBUG

					if ((InRange9(xx1, 166.7e5)) && (InRange9(yy1, 128.2e5)))
						ok = 1;

#endif

					if ((Mode & 1) == 0)
					{
						x1 = Mult2(xx1 - PrintOffsetX);
						y1 = Mult2(yy1 - PrintOffsetY);
						x2 = Mult2(xx2 - PrintOffsetX);
						y2 = Mult2(yy2 - PrintOffsetY);

						if (ReverseY)
						{
							if (y2 - y1 < x2 - x1)
								x2--;
						}

						dikte = Mult2(dikte2);

						if (!ReverseX)
							DrawDiag1Line(x1, y1, x2, dikte, 2);
						else
							DrawDiag2Line(DrawWindowMaxX - x2, y2, DrawWindowMaxX - x1, dikte, 2);
					}
					else
					{
						PDFNewLineWidth = Trace->ThickNess * PDFMultFactor * PDFMultFactor2;

						if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
						{
							PDFLineWidth = PDFNewLineWidth;
							sprintf(str, "%.3f w", PDFLineWidth);
							AddToMessageBuf(str);
						}

						x11 = PDFConvX(xx1);
						y11 = PDFConvY(yy1);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(xx2);
						y22 = PDFConvY(yy2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);
					}
				}
			}
		}
	}

	for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
	{
		Trace = &((*Diag2Traces[Layer])[cnt]);

		if ((Trace->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			xx1 = Trace->X;
			xx2 = xx1 + Trace->Length;
			dikte2 = Trace->ThickNess;
			yy2a = Trace->ThickNess * 0.5;
			yy1 = Trace->Y;
			yy2 = yy1 + Trace->Length;

			if ((xx2 + yy2a >= ViewMinX) && (xx1 - yy2a <= ViewMaxX))
			{
				if ((yy1 + (xx2 - xx1) + yy2a >= ViewMinY) && (yy1 - yy2a <= ViewMaxY))
				{
#ifdef _DEBUG

					if ((InRange9(xx1, 50.72e5)) && (InRange9(yy1, 198.1e5)))
						ok = 1;

#endif

					if ((Mode & 1) == 0)
					{
						x1 = Mult2(xx1 - PrintOffsetX);
						y1 = Mult2(yy1 - PrintOffsetY);
						x2 = Mult2(xx2 - PrintOffsetX);
						y2 = Mult2(yy2 - PrintOffsetY);

						if (ReverseY)
						{
							if (y2 - y1 < x2 - x1)
								x2--;
						}

						dikte = Mult2(dikte2);

						if (!ReverseX)
							DrawDiag2Line(x1, y1, x2, dikte, 2);
						else
							DrawDiag1Line(DrawWindowMaxX - x2, y2, DrawWindowMaxX - x1, dikte, 2);
					}
					else
					{
						PDFNewLineWidth = Trace->ThickNess * PDFMultFactor * PDFMultFactor2;

						if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
						{
							PDFLineWidth = PDFNewLineWidth;
							sprintf(str, "%.3f w", PDFLineWidth);
							AddToMessageBuf(str);
						}

						x11 = PDFConvX(xx1);
						y11 = PDFConvY(yy1);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(xx2);
						y22 = PDFConvY(yy2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);
					}
				}
			}
		}
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlotVias(int32 Layer, int32 Mode)
{
	double xx1, yy1, yy2, x11, y11, dikte, Xmax, Xmin, Ymax, Ymin, y2;
	int32 cnt, ViaInfo;
	ViaRecord *Via;
	char str[MAX_LENGTH_STRING];

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);
		ViaInfo = Via->Info;

		if ((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0)
		{
			xx1 = Via->X;
			yy1 = Via->Y;
			dikte = Via->ThickNess;
			//  Drill=Via->DrillThickNess;
			yy2 = dikte / 2;
			Xmin = xx1 - yy2;
			Ymin = yy1 - yy2;
			Xmax = xx1 + yy2;
			Ymax = yy1 + yy2;

			if ((Xmax >= ViewMinX) && (Xmin <= ViewMaxX) && (Ymax >= ViewMinY) && (Ymin <= ViewMaxY))
			{
				if ((Mode & 1) == 0)
				{
					if (!ReverseY)
					{
						ellips2(Mult2(Via->X - PrintOffsetX), Mult2(Via->Y - PrintOffsetY), Mult2(Via->ThickNess),
						        Mult2(Via->ThickNess), 255);
					}
					else
					{
						if (!ReverseX)
						{
							InitDrawingPlot(1);
							InitDrawingSolidBrushBlack();
							ellips2(Mult2(Via->X - PrintOffsetX), DrawWindowMaxY - Mult2(Via->Y - PrintOffsetY) - 1,
							        Mult2(Via->ThickNess), Mult2(Via->ThickNess), 255);

							if ((Mode & 2) == 0)
							{
								InitDrawingSolidBrushWhite();
								y2 = min(Via->DrillThickNess, MIN_PLOT_DRILL);
								ellips2(Mult2(Via->X - PrintOffsetX), DrawWindowMaxY - Mult2(Via->Y - PrintOffsetY) - 1,
								        Mult2(y2), Mult2(y2), 255);
							}
						}
						else
						{
							InitDrawingPlot(1);
							InitDrawingSolidBrushBlack();
							ellips2(DrawWindowMaxX - Mult2(Via->X - PrintOffsetX),
							        DrawWindowMaxY - Mult2(Via->Y - PrintOffsetY) - 1, Mult2(Via->ThickNess),
							        Mult2(Via->ThickNess), 255);

							if ((Mode & 2) == 0)
							{
								InitDrawingSolidBrushWhite();
								y2 = min(Via->DrillThickNess, MIN_PLOT_DRILL);
								ellips2(DrawWindowMaxX - Mult2(Via->X - PrintOffsetX),
								        DrawWindowMaxY - Mult2(Via->Y - PrintOffsetY) - 1, Mult2(y2), Mult2(y2), 255);
							}
						}
					}
				}
				else
				{
					if (PDFCurrentColor == 0)
					{
						AddToMessageBuf("0 g");
						AddToMessageBuf("0 G");
						PDFCurrentColor = 1;
					}

					PDFNewLineWidth = dikte * PDFMultFactor * PDFMultFactor2;

					if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
					{
						PDFLineWidth = PDFNewLineWidth;
						sprintf(str, "%.3f w", PDFLineWidth);
						AddToMessageBuf(str);
					}

					x11 = PDFConvX(xx1);
					y11 = PDFConvY(yy1);
					sprintf(str, "%.3f %.3f m", x11, y11);
					AddToMessageBuf(str);
					sprintf(str, "%.3f %.3f l S", x11, y11);
					AddToMessageBuf(str);

					if ((Mode & 2) == 2)
					{
						AddToMessageBuf("1 g");
						AddToMessageBuf("1 G");
						PDFCurrentColor = 0;
						PDFNewLineWidth = Via->DrillThickNess * PDFMultFactor * PDFMultFactor2;

						if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
						{
							PDFLineWidth = PDFNewLineWidth;
							sprintf(str, "%.3f w", PDFLineWidth);
							AddToMessageBuf(str);
						}

						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						sprintf(str, "%.3f %.3f l S", x11, y11);
						AddToMessageBuf(str);
					}
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlotStr(double x, double y, double Size, int32 Rotation, int32 Alignment, int32 Mirror, char *str, int32 mode)
{
	char PDFstr[MAX_LENGTH_STRING];
	int32 cnt2, xx1, yy1, xx2, yy2, SegmentCount, LineSegments;
	double x1, y1, x2, y2, LineBuf[4096];


	if (str[0] == 0)
		return;

	LineSegments = TextStringToLineSegments2(x, y, Size, 0.0, 0, Mirror, str, (double *) &LineBuf);
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{
		if ((mode & 1) == 0)
		{
			xx1 = MultX(LineBuf[SegmentCount]);

			if (!ReverseX)
				xx1 = Mult2(LineBuf[SegmentCount] - PrintOffsetX);
			else
				xx1 = DrawWindowMaxX - Mult2(LineBuf[SegmentCount] - PrintOffsetX);

			SegmentCount++;

			if (ReverseY)
				yy1 = DrawWindowMaxY - Mult2(LineBuf[SegmentCount] - PrintOffsetY) - 1;
			else
				yy1 = Mult2(LineBuf[SegmentCount] - PrintOffsetY);

			SegmentCount++;

			if (!ReverseX)
				xx2 = Mult2(LineBuf[SegmentCount] - PrintOffsetX);
			else
				xx2 = DrawWindowMaxX - Mult2(LineBuf[SegmentCount] - PrintOffsetX);

			SegmentCount++;

			if (ReverseY)
				yy2 = DrawWindowMaxY - Mult2(LineBuf[SegmentCount] - PrintOffsetY) - 1;
			else
				yy2 = Mult2(LineBuf[SegmentCount] - PrintOffsetY);

			SegmentCount++;
			MoveToEx(OutputDisplay, xx1, yy1, NULL);
			LineTo(OutputDisplay, xx2, yy2);
			SetPixel(OutputDisplay, xx2, yy2, LineColor);
		}
		else
		{
			x1 = PDFConvX(LineBuf[SegmentCount++]);
			y1 = PDFConvY(LineBuf[SegmentCount++]);
			sprintf(PDFstr, "%.3f %.3f m", x1, y1);
			AddToMessageBuf(PDFstr);
			x2 = PDFConvX(LineBuf[SegmentCount++]);
			y2 = PDFConvY(LineBuf[SegmentCount++]);
			sprintf(PDFstr, "%.3f %.3f l S", x2, y2);
			AddToMessageBuf(PDFstr);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlotStr2(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror, char *str, int32 Mode)
{
	char PDFstr[MAX_LENGTH_STRING];
	int32 cnt2, xx1, yy1, xx2, yy2, SegmentCount, LineSegments;
	double x1, y1, x2, y2, LineBuf[4096];
#ifdef _DEBUG
	int32 ok;
#endif

	if (str[0] == 0)
		return;
	
	StringCount++;

	LineSegments = TextStringToLineSegments2(x, y, Size, Rotation, 0, Mirror, str, (double *) &LineBuf);
#ifdef _DEBUG

	if (LineSegments > 4096)
		ok = 1;

#endif
	SegmentCount = 0;

	for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
	{
		if ((Mode & 1) == 0)
		{
			xx1 = MultX(LineBuf[SegmentCount]);

			if (!ReverseX)
				xx1 = Mult2(LineBuf[SegmentCount] - PrintOffsetX);
			else
				xx1 = DrawWindowMaxX - Mult2(LineBuf[SegmentCount] - PrintOffsetX);

			SegmentCount++;

			if (ReverseY)
				yy1 = DrawWindowMaxY - Mult2(LineBuf[SegmentCount] - PrintOffsetY) - 1;
			else
				yy1 = Mult2(LineBuf[SegmentCount] - PrintOffsetY);

			SegmentCount++;

			if (!ReverseX)
				xx2 = Mult2(LineBuf[SegmentCount] - PrintOffsetX);
			else
				xx2 = DrawWindowMaxX - Mult2(LineBuf[SegmentCount] - PrintOffsetX);

			SegmentCount++;

			if (ReverseY)
				yy2 = DrawWindowMaxY - Mult2(LineBuf[SegmentCount] - PrintOffsetY) - 1;
			else
				yy2 = Mult2(LineBuf[SegmentCount] - PrintOffsetY);

			SegmentCount++;
			MoveToEx(OutputDisplay, xx1, yy1, NULL);
			LineTo(OutputDisplay, xx2, yy2);
			SetPixel(OutputDisplay, xx2, yy2, LineColor);
		}
		else
		{
			x1 = PDFConvX(LineBuf[SegmentCount++]);
			y1 = PDFConvY(LineBuf[SegmentCount++]);
			sprintf(PDFstr, "%.3f %.3f m", x1, y1);
			AddToMessageBuf(PDFstr);
			x2 = PDFConvX(LineBuf[SegmentCount++]);
			y2 = PDFConvY(LineBuf[SegmentCount++]);
			sprintf(PDFstr, "%.3f %.3f l S", x2, y2);
			AddToMessageBuf(PDFstr);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawPrintPolygon(PolygonRecord * DrawPolygon, int32 Filled, int32 mode)
{
	int32 cnt, x, y, count;
	PointsArray3 *Points;
	double x11, y11;
	char str[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 ok;
#endif

	count = DrawPolygon->NrVertices;
	AllocateSpecialMem(MEM_POINTS, count * sizeof(POINT) + 16384, (void **) &Points);

	if ((mode & 1) == 0)
	{
		if ((mode & 2) == 0)
		{
		}
		else
		{
			if ((DrawPolygon->PolygonType & 8) == 0)
			{
				InitDrawingPlot(1);
				InitDrawingSolidBrushBlack();
			}
			else
			{
				InitDrawingPlot(1);
				InitDrawingSolidBrushWhite();
			}
		}

		for (cnt = 0; cnt < count; cnt++)
		{
			if (!ReverseX)
				x = Mult2((*DrawPolygon).Points[cnt].x - PrintOffsetX);
			else
				x = DrawWindowMaxX - Mult2((*DrawPolygon).Points[cnt].x - PrintOffsetX);

			y = DrawWindowMaxY - Mult2((*DrawPolygon).Points[cnt].y - PrintOffsetY) - 1;
			(*Points)[cnt].x = x;
			(*Points)[cnt].y = y;
		}

		Polygon(OutputDisplay, (POINT *) Points, count);
	}
	else
	{
		if ((mode & 2) == 0)
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}
		}
		else
		{
			if ((DrawPolygon->PolygonType & 8) == 0)
			{
				if (PDFCurrentColor == 0)
				{
					AddToMessageBuf("0 g");
					AddToMessageBuf("0 G");
					PDFCurrentColor = 1;
				}
			}
			else
			{
				if (PDFCurrentColor == 1)
				{
					AddToMessageBuf("1 g");
					AddToMessageBuf("1 G");
					PDFCurrentColor = 0;
				}
			}
		}

		for (cnt = 0; cnt < count; cnt++)
		{
			x11 = PDFConvX(DrawPolygon->Points[cnt].x);
			y11 = PDFConvY(DrawPolygon->Points[cnt].y);
#ifdef _DEBUG

			if ((x11 < -1e9) || (x11 > 1e9))
				ok = 1;

#endif
			sprintf(str, "%.3f %.3f", x11, y11);

			if (cnt == 0)
				strcat(str, " m");
			else
				strcat(str, " l");

			AddToMessageBuf(str);
		}

		if (Filled == 1)
			AddToMessageBuf("h f");
		else
			AddToMessageBuf("h");
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PrintLineFromFloat(double x1, double y1, double x2, double y2)
{
	int32 x1a, y1a, x2a, y2a;

	if (!ReverseX)
		x1a = Mult2(x1 - PrintOffsetX);
	else
		x1a = DrawWindowMaxX - Mult2(x1 - PrintOffsetX);

	if (!ReverseY)
		y1a = Mult2(y1 - PrintOffsetY);
	else
		y1a = DrawWindowMaxY - Mult2(y1 - PrintOffsetY) - 1;

	if (!ReverseY)
		y2a = Mult2(y2 - PrintOffsetY);
	else
		y2a = DrawWindowMaxY - Mult2(y2 - PrintOffsetY) - 1;

	if (!ReverseX)
		x2a = Mult2(x2 - PrintOffsetX);
	else
		x2a = DrawWindowMaxX - Mult2(x2 - PrintOffsetX);

	DrawLine(x1a, y1a, x2a, y2a);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PrintPDFLineFromFloat(double x1, double y1, double x2, double y2, double Thickness)
{
	double x11, y11, x22, y22;
	char str[MAX_LENGTH_STRING];

	PDFNewLineWidth = Thickness * PDFMultFactor * PDFMultFactor2;

	if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
	{
		PDFLineWidth = PDFNewLineWidth;
		sprintf(str, "%.3f w", PDFLineWidth);
		AddToMessageBuf(str);
	}

	x11 = PDFConvX(x1);
	y11 = PDFConvY(y1);
	sprintf(str, "%.3f %.3f m", x11, y11);
	AddToMessageBuf(str);
	x22 = PDFConvX(x2);
	y22 = PDFConvY(y2);
	sprintf(str, "%.3f %.3f l S", x22, y22);
	AddToMessageBuf(str);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddPDFObject(int32 mode)
{
	char str[MAX_LENGTH_STRING];

	AllocateSpecialMem(MEM_PDFBUF, (NrPDFObjects + 1) * sizeof(int32), (void **) &PDFObjectPos);

	if (mode == 0)
	{
		PDFObjectPos[NrPDFObjects++] = strlen(MessageBuf) + StartPdfPos;
		sprintf(str, "%d 0 obj", NrPDFObjects);
		AddToMessageBuf(str);
	}
	else
	{
		PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(pdffp);
		sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
		WriteToFile(pdffp, str);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlotObject(ObjectRecord * Object, int32 Mode)
{
	double xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4, xx2a, yy2a, incX, incY, x11, y11, x22, y22, xxx2, yyy2, yyy3,
	       Rotation, LineBuf[4096], Angle1, Angle2, Length1, Length2;
	int32 x3, y3, x4, y4, cnt2, cnt3, cnt4, Layer, SegmentCount, LineSegments, ok, MaxCountX, Thickness, Mirror,
	      NrLines, FontNr, count, cnt5, cnt6, code, DrawChar, lengte, TextAlignment, x1a, y1a, ArcMode;
	PolygonRecord *DrawPolygon;
	AreaFillRecord *AreaFill;
	WCHAR *str2;
	uint8 *PolygonPos;
	char str[1024], *TextP;
#ifdef _DEBUG
	LOGPEN *CurrentPen;
	HGDIOBJ CurrentPenObject;
	uint8 PenBuf[16];
	CurrentPen = (LOGPEN *) PenBuf;

	if (ViewMaxX < 1e20)
		ok = 1;

#endif

	AllocateSpecialMem(MEM_POLYGON, 128 * 1024, (void **) &DrawPolygon);

	if ((ViewMaxX < Object->minx) || (ViewMinX > Object->maxx) || (ViewMaxY < Object->miny)
	        || (ViewMinY > Object->maxy))
		return;

	Layer = Object->Layer;
	xx1 = Object->x1;
	yy1 = Object->y1;
	xx2 = Object->x2;
	xx3 = Object->x3;
#ifdef _DEBUG

	if ((InRange9(xx1, -88.4e5)) && (InRange9(yy1, 928e5)))
	{
		ok = 1;
		CurrentPenObject = GetCurrentObject(OutputDisplay, OBJ_PEN);
		GetObject(CurrentPenObject, 16, PenBuf);
	}

	CurrentPenObject = GetCurrentObject(OutputDisplay, OBJ_PEN);
	GetObject(CurrentPenObject, 16, PenBuf);

	if (CurrentPen->lopnColor = RGB_White)
		ok = 1;

#endif

	if (!ReverseX)
		x1a = Mult2(xx1 - PrintOffsetX);
	else
		x1a = DrawWindowMaxX - Mult2(xx1 - PrintOffsetX);

	if (!ReverseY)
		y1a = Mult2(yy1 - PrintOffsetY);
	else
		y1a = DrawWindowMaxY - Mult2(yy1 - PrintOffsetY) - 1;

	Thickness = Mult2(Object->Thickness);
	xxx2 = xx2;
	yy2 = Object->y2;
	yyy2 = yy2;

	switch (Object->ObjectType)
	{
	case DRILL:
	case DRILL_UNPLATED:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();
			ellips2(x1a, y1a, Mult2(xxx2), Mult2(xxx2), 255);

			if ((Mode & 2) == 2)
			{
				InitDrawingSolidBrushWhite();
				yyy3 = min(xx2 * 0.65, MIN_PLOT_DRILL);
				ellips2(x1a, y1a, Mult2(yyy3), Mult2(yyy3), 255);
			}
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = xx2 * PDFMultFactor * PDFMultFactor2;

			if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
			{
				PDFLineWidth = PDFNewLineWidth;
				sprintf(str, "%.3f w", PDFLineWidth);
				AddToMessageBuf(str);
			}

			x11 = PDFConvX(xx1);
			y11 = PDFConvY(yy1);
			sprintf(str, "%.3f %.3f m", x11, y11);
			AddToMessageBuf(str);
			sprintf(str, "%.3f %.3f l S", x11, y11);
			AddToMessageBuf(str);

			if ((Mode & 2) == 2)
			{
				AddToMessageBuf("1 g");
				AddToMessageBuf("1 G");
				PDFCurrentColor = 0;
				PDFNewLineWidth = xx2 * 0.65 * PDFMultFactor * PDFMultFactor2;

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

					if (PDFLineWidth < 0.0)
						ok = 1;

#endif
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}

				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				sprintf(str, "%.3f %.3f l S", x11, y11);
				AddToMessageBuf(str);
			}
		}

		break;

	case PIN_PUT_THROUGH_ROUND:
	case VIA_PUT_THROUGH_ROUND:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();
			ellips2(x1a, y1a, Mult2(xxx2), Mult2(xxx2), 255);

			if ((Mode & 2) == 2)
			{
				InitDrawingPlot(1);
				InitDrawingSolidBrushWhite();
				yyy3 = min(yy2, MIN_PLOT_DRILL);
				ellips2(x1a, y1a, Mult2(yyy3), Mult2(yyy3), 255);
			}
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = xx2 * PDFMultFactor * PDFMultFactor2;

			if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
			{
				PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

				if (PDFLineWidth < 0.0)
					ok = 1;

#endif
				sprintf(str, "%.3f w", PDFLineWidth);
				AddToMessageBuf(str);
			}

			x11 = PDFConvX(xx1);
			y11 = PDFConvY(yy1);
			sprintf(str, "%.3f %.3f m", x11, y11);
			AddToMessageBuf(str);
			sprintf(str, "%.3f %.3f l S", x11, y11);
			AddToMessageBuf(str);

			if ((Mode & 2) == 2)
			{
				AddToMessageBuf("1 g");
				AddToMessageBuf("1 G");
				PDFCurrentColor = 0;
				PDFNewLineWidth = yy2 * PDFMultFactor * PDFMultFactor2;

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

					if (PDFLineWidth < 0.0)
						ok = 1;

#endif
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}

				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				sprintf(str, "%.3f %.3f l S", x11, y11);
				AddToMessageBuf(str);
			}
		}

		break;

	case PIN_PUT_THROUGH_SQUARE:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();
			rect3(x1a, y1a, Mult2(xxx2), Mult2(xxx2));

			if ((Mode & 2) == 2)
			{
				InitDrawingSolidBrushWhite();
				yyy3 = min(yy2, MIN_PLOT_DRILL);

				if ((Mode & 1) == 0)
					ellips2(x1a, y1a, Mult2(yyy3), Mult2(yyy3), 255);
			}
		}
		else
		{
			x11 = PDFConvX(xx1 - xx2 * 0.5);
			y11 = PDFConvY(yy1 - xx2 * 0.5);

			x22 = xx2 * PDFMultFactor * PDFMultFactor2;
			y22 = xx2 * PDFMultFactor * PDFMultFactor2;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
			AddToMessageBuf(str);

			if ((Mode & 2) == 2)
			{
				AddToMessageBuf("1 g");
				AddToMessageBuf("1 G");
				PDFCurrentColor = 0;
				PDFNewLineWidth = yy2 * PDFMultFactor * PDFMultFactor2;

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

					if (PDFLineWidth < 0.0)
						ok = 1;

#endif
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}

				x11 = PDFConvX(xx1);
				y11 = PDFConvY(yy1);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				sprintf(str, "%.3f %.3f l S", x11, y11);
				AddToMessageBuf(str);
			}
		}

		break;

	case PIN_PUT_THROUGH_POLYGON:
		if (CheckObjectIsBigPolygon(Object))
		{
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
			AreaFillToPlot(AreaFill, 1);
		}
		else
		{
			MakePolygonFromObject(Object, DrawPolygon, 0.0, 0.0, 1, 1);

			if ((Mode & 1) == 0)
			{
				InitDrawingPlot(1);
				InitDrawingSolidBrushBlack();
				DrawPrintPolygon(DrawPolygon, 1, Mode & 1);

				if ((Mode & 2) == 2)
				{
					InitDrawingSolidBrushWhite();
					yyy3 = min(yy2, MIN_PLOT_DRILL);
					ellips2(x1a, y1a, Mult2(yyy3), Mult2(yyy3), 255);
				}
			}
			else
			{
				if (PDFCurrentColor == 0)
				{
					AddToMessageBuf("0 g");
					AddToMessageBuf("0 G");
					PDFCurrentColor = 1;
				}

				DrawPrintPolygon(DrawPolygon, 1, Mode & 1);

				if ((Mode & 2) == 2)
				{
					AddToMessageBuf("1 g");
					AddToMessageBuf("1 G");
					PDFCurrentColor = 0;
					PDFNewLineWidth = yy2 * PDFMultFactor * PDFMultFactor2;

					if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
					{
						PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

						if (PDFLineWidth < 0.0)
							ok = 1;

#endif
						sprintf(str, "%.3f w", PDFLineWidth);
						AddToMessageBuf(str);
					}

					x11 = PDFConvX(xx1);
					y11 = PDFConvY(yy1);
					sprintf(str, "%.3f %.3f m", x11, y11);
					AddToMessageBuf(str);
					sprintf(str, "%.3f %.3f l S", x11, y11);
					AddToMessageBuf(str);
				}
			}
		}

		break;

	case PIN_SMD_ROUND:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();
			ellips2(x1a, y1a, Mult2(xxx2), Mult2(xxx2), 255);
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = xx2 * PDFMultFactor * PDFMultFactor2;

			if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
			{
				PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

				if (PDFLineWidth < 0.0)
					ok = 1;

#endif
				sprintf(str, "%.3f w", PDFLineWidth);
				AddToMessageBuf(str);
			}

			x11 = PDFConvX(xx1);
			y11 = PDFConvY(yy1);
			sprintf(str, "%.3f %.3f m", x11, y11);
			AddToMessageBuf(str);
			sprintf(str, "%.3f %.3f l S", x11, y11);
			AddToMessageBuf(str);
		}

		break;

	case PIN_SMD_RECT:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();
			rect3(x1a, y1a, Mult2(xxx2), Mult2(yyy2));
		}
		else
		{
			x11 = PDFConvX(xx1 - xx2 * 0.5);
			y11 = PDFConvY(yy1 - yy2 * 0.5);

			x22 = xx2 * PDFMultFactor * PDFMultFactor2;
			y22 = yy2 * PDFMultFactor * PDFMultFactor2;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
			AddToMessageBuf(str);
		}

		break;

	case PIN_SMD_POLYGON:
		if (CheckObjectIsBigPolygon(Object))
		{
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
			AreaFillToPlot(AreaFill, Mode & 1);
		}
		else
		{
			MakePolygonFromObject(Object, DrawPolygon, 0.0, 0.0, 1, 1);

			if ((Mode & 1) == 0)
			{
				InitDrawingPlot(1);
				InitDrawingSolidBrushBlack();
			}
			else
			{
			}

			DrawPrintPolygon(DrawPolygon, 1, Mode & 1);
		}

		break;

	case PIN_LINE_HOR:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();

			if (!ReverseX)
			{
				DrawHorLine(Mult2(xx1 - PrintOffsetX), Mult2(yy1 - PrintOffsetY), Mult2(xx1 + xx2 - PrintOffsetX),
				            Mult2(yyy2), 0);
			}
			else
			{
				DrawHorLine(DrawWindowMaxX - Mult2(xx1 + xx2 - PrintOffsetX), Mult2(yy1 - PrintOffsetY),
				            DrawWindowMaxX - Mult2(xx1 - PrintOffsetX), Mult2(yyy2), 0);
			}
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PrintPDFLineFromFloat(xx1, yy1, xx1 + xx2, yy1, yy2);
		}

		break;

	case PIN_LINE_VER:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();

			if (!ReverseX)
			{
				DrawVerLine(Mult2(xx1 - PrintOffsetX), Mult2(yy1 - PrintOffsetY), Mult2(yy1 + xx2 - PrintOffsetY),
				            Mult2(yyy2), 0);
			}
			else
			{
				DrawVerLine(DrawWindowMaxX - Mult2(xx1 - PrintOffsetX), Mult2(yy1 - PrintOffsetY),
				            Mult2(yy1 + xx2 - PrintOffsetY), Mult2(yyy2), 0);
			}
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PrintPDFLineFromFloat(xx1, yy1, xx1, yy1 + xx2, yy2);
		}

		break;

	case PIN_LINE_DIAG1:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(1);
			InitDrawingSolidBrushBlack();

			if (!ReverseX)
			{
				DrawDiag1Line(Mult2(xx1 - PrintOffsetX), Mult2(yy1 - PrintOffsetY), Mult2(xx1 + xx2 - PrintOffsetX),
				              Mult2(yyy2), 0);
			}
			else
			{
				DrawDiag2Line(DrawWindowMaxX - Mult2(xx1 + xx2 - PrintOffsetX), Mult2(yy1 - xx2 - PrintOffsetY),
				              DrawWindowMaxX - Mult2(xx1 - PrintOffsetX), Mult2(yyy2), 0);
			}
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PrintPDFLineFromFloat(xx1, yy1, xx1 + xx2, yy1 - xx2, yy2);
		}

		break;

	case PIN_LINE_DIAG2:
		InitDrawingPlot(1);
		InitDrawingSolidBrushBlack();

		if ((Mode & 1) == 0)
		{
			if (!ReverseX)
			{
				DrawDiag2Line(Mult2(xx1 - PrintOffsetX), Mult2(yy1 - PrintOffsetY), Mult2(xx1 + xx2 - PrintOffsetX),
				              Mult2(yyy2), 0);
			}
			else
			{
				DrawDiag1Line(DrawWindowMaxX - Mult2(xx1 + xx2 - PrintOffsetX), Mult2(yy1 + xx2 - PrintOffsetY),
				              DrawWindowMaxX - Mult2(xx1 - PrintOffsetX), Mult2(yyy2), 0);
			}
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PrintPDFLineFromFloat(xx1, yy1, xx1 + xx2, yy1 + xx2, yy2);
		}

		break;

// ***********************************************************************************
// ***********************************************************************************
	case OBJECT_LINE:
	case PIN_LINE_ALL_ANGLE:
		if ((Mode & 1) == 0)
		{
			InitDrawingPlot(Thickness);
			InitDrawingNewEmptyBrush();
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}
		}

		if (Object->Test != 0)
		{
			LineSegments = DimensionToLineSegments(xx1, yy1, xx2, yy2, (double *) &LineBuf, Object->Test);
			SegmentCount = 0;

			for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
			{
				xx3 = LineBuf[SegmentCount++];
				yy3 = LineBuf[SegmentCount++];
				xx4 = LineBuf[SegmentCount++];
				yy4 = LineBuf[SegmentCount++];

				if ((Mode & 1) == 0)
					PrintLineFromFloat(xx3, yy3, xx4, yy4);
				else
					PrintPDFLineFromFloat(xx3, yy3, xx4, yy4, Object->Thickness);
			}
		}
		else
		{
			if ((Mode & 1) == 0)
				PrintLineFromFloat(xx1, yy1, xx2, yy2);
			else
				PrintPDFLineFromFloat(xx1, yy1, xx2, yy2, Object->Thickness);
		}

		break;

	case OBJECT_RECT:
		if ((Mode & 1) == 0)
		{
			if (Object->Thickness != 0.0)
			{
				InitDrawingPlot(Thickness);
				InitDrawingNewEmptyBrush();
			}
			else
			{
				InitDrawingPlot(1);
				InitDrawingSolidBrushBlack();
			}

			xx2a = xx2 / 2;
			yy2a = yy2 / 2;
			rect3(x1a, y1a, Mult2(xx2), Mult2(yy2));
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			if (Object->Thickness == 0.0)
			{
				x11 = PDFConvX(xx1 - xx2 * 0.5);
				y11 = PDFConvY(yy1 - yy2 * 0.5);

				x22 = xx2 * PDFMultFactor * PDFMultFactor2;
				y22 = yy2 * PDFMultFactor * PDFMultFactor2;

				sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
				AddToMessageBuf(str);
			}
			else
			{
				PrintPDFLineFromFloat(xx1 - xx2 * 0.5, yy1 - yy2 * 0.5, xx1 + xx2 * 0.5, yy1 - yy2 * 0.5,
				                      Object->Thickness);
				PrintPDFLineFromFloat(xx1 + xx2 * 0.5, yy1 - yy2 * 0.5, xx1 + xx2 * 0.5, yy1 + yy2 * 0.5,
				                      Object->Thickness);
				PrintPDFLineFromFloat(xx1 + xx2 * 0.5, yy1 + yy2 * 0.5, xx1 - xx2 * 0.5, yy1 + yy2 * 0.5,
				                      Object->Thickness);
				PrintPDFLineFromFloat(xx1 - xx2 * 0.5, yy1 + yy2 * 0.5, xx1 - xx2 * 0.5, yy1 - yy2 * 0.5,
				                      Object->Thickness);
			}
		}

		break;

	/*
	    case OBJECT_CIRCLE:
	      if (Object->Thickness!=0.0) {
	        InitDrawingPlot(1);
	        InitDrawingSolidBrushBlack();
	        ellips2(x1a,y1a,Mult2(xxx2),Mult2(xxx2),255);
	      } else {
	        InitDrawingPlot(Thickness);
	        InitDrawingNewEmptyBrush();
	        xx2a=xx2/2;
	        yy2a=xx2/2;
	        if ((Mode & 1)==0) {
	          ellips2(x1a,y1a,Mult2(xx2),Mult2(xx2),CircleConv[(uint8)yy2]);
	        } else {
	        }
	      }
	      break;
	*/
	case OBJECT_ARC:
	case PIN_ARC:
		if ((Mode & 1) == 0)
		{
			if (Object->Thickness == 0.0)
			{
				InitDrawingPlot(1);
				InitDrawingSolidBrushBlack();
				ellips2(x1a, y1a, Mult2(xxx2), Mult2(xxx2), 255);
			}
			else
			{
				InitDrawingPlot(Thickness);
				InitDrawingNewEmptyBrush();
				xx3 = Object->x3;
				yy3 = Object->y3;
				xx4 = Object->x4;
				yy4 = Object->y4;
				x3 = MultX(xx1 + xx3);
				y3 = MultY(yy1 + yy3);
				x4 = MultX(xx1 + xx4);
				y4 = MultY(yy1 + yy4);
				ConvNormalCoorToPolar(xx1, yy1, xx1 + xx3, yy1 + yy3, &Angle1, &Length1);
				ConvNormalCoorToPolar(xx1, yy1, xx1 + xx4, yy1 + yy4, &Angle2, &Length2);

				if (Angle2 < Angle1)
					Angle2 += ANGLE_360;

				if (Angle2 - Angle1 < ANGLE_CONVERT(2.0))
					ok = 1;

				if ((Angle2 - Angle1 < ANGLE_90) && (Angle2 - Angle1 > 0.0)
				        && ((abs(x3 - x4) < 2) || (abs(y3 - y4) < 2)))
				{
					if (((abs(x3 - x4) < 2) && (abs(y3 - y4) < 2)) || (Angle2 - Angle1 < ANGLE_CONVERT(10.0)))
						ArcMode = 0;
					else
						ArcMode = 1;
				}
				else
				{
					if ((Angle2 - Angle1 > 0.0) && (Angle2 - Angle1 < ANGLE_CONVERT(3.0)))
						ArcMode = 0;
					else
						ArcMode = 1;
				}

				if (ArcMode == 0)
					PrintLineFromFloat(xx1, yy1, xx2, yy2);
				else
				{
					if (ReverseY)
					{
						if (!ReverseX)
						{
							SpecialArc(x1a, y1a, Mult2(xx2) + 1, Mult2(yy2) + 1, Mult2(xx1 + xx3 - PrintOffsetX),
							           DrawWindowMaxY - Mult2(yy1 + yy3 - PrintOffsetY) - 1,
							           Mult2(xx1 + xx4 - PrintOffsetX),
							           DrawWindowMaxY - Mult2(yy1 + yy4 - PrintOffsetY) - 1);
						}
						else
						{
							SpecialArc(x1a, y1a, Mult2(xx2) + 1, Mult2(yy2) + 1,
							           DrawWindowMaxX - Mult2(xx1 + xx4 - PrintOffsetX),
							           DrawWindowMaxY - Mult2(yy1 + yy4 - PrintOffsetY) - 1,
							           DrawWindowMaxX - Mult2(xx1 + xx3 - PrintOffsetX),
							           DrawWindowMaxY - Mult2(yy1 + yy3 - PrintOffsetY) - 1);
						}
					}
					else
					{
						SpecialArc(x1a, y1a, Mult(xx2) + 1, Mult(yy2) + 1, Mult2(xx1 + xx3 - PrintOffsetX),
						           Mult2(yy1 + yy3 - PrintOffsetY), Mult2(xx1 + xx4 - PrintOffsetX),
						           Mult2(yy1 + yy4 - PrintOffsetY));
					}
				}
			}
		}
		else
		{
			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			if (Object->Thickness == 0.0)
			{
				PDFNewLineWidth = xx2 * PDFMultFactor * PDFMultFactor2;

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

					if (PDFLineWidth < 0.0)
						ok = 1;

#endif
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}

				x11 = PDFConvX(xx1);
				y11 = PDFConvY(yy1);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				sprintf(str, "%.3f %.3f l S", x11, y11);
				AddToMessageBuf(str);
			}
			else
			{
				LineSegments =
				    ArcToLineSegments(Object->x1, Object->y1, Object->x2, Object->y2, Object->x3, Object->y3,
				                      Object->x4, Object->y4, (double *) &LineBuf, 1);
				SegmentCount = 0;

				for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
				{
					xx1 = LineBuf[SegmentCount++];
					yy1 = LineBuf[SegmentCount++];
					xx2 = LineBuf[SegmentCount++];
					yy2 = LineBuf[SegmentCount++];
					PrintPDFLineFromFloat(xx1, yy1, xx2, yy2, Object->Thickness);
				}
			}
		}

		break;

	/*
	    case OBJECT_TEXT:
	      InitDrawingPlot(Thickness);
	      InitDrawingNewEmptyBrush();
	      Rotation=Object->RotationAngle;
	      TextAlignment=Object->Test & 0x0f;
	      Mirror=Object->Mirror;
	//      GetMinMaxText(xx1,yy1,xx2,0,TextRotation,TextAlignment,0,(LPSTR)Object->TraceNr);
	//      InitDrawingLayer(Layer,Mult2(Clearance));
	      if ((Mode & 1)==0) {
	        PlotStr2(xx1,yy1,xx2,Rotation,TextAlignment,Mirror,(LPSTR)Object->TraceNr);
	      } else {
	      }
	      break;
	*/
	case OBJECT_TEXT:
		Rotation = Object->RotationAngle;
		TextAlignment = Object->Test & 0x0f;
		Mirror = Object->Mirror;
		FontNr = Object->Test >> 16;
//      GetMinMaxText(xx1,yy1,xx2,0,TextRotation,TextAlignment,0,(LPSTR)Object->TraceNr);
//      InitDrawingLayer(Layer,Mult2(Clearance));
		TextP = (LPSTR) Object->TraceNr;
#ifdef _DEBUG

		if (stricmpOwn((LPSTR) Object->TraceNr, "PasteTop") == 0)
			ok = 1;

#endif

		if ((Mode & 1) == 0)
		{
			if (FontNr == 0)
			{
				InitDrawingPlot(Thickness);
				InitDrawingNewEmptyBrush();
			}
		}
		else
		{
			if (FontNr == 0)
			{
				PDFNewLineWidth = Object->Thickness * PDFMultFactor * PDFMultFactor2;

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}
			}
		}

		NrLines = ConvertObjectTextToStrings(TextP, FontNr, &MaxCountX, Object->Layer);

		for (cnt3 = 0; cnt3 < NrLines; cnt3++)
		{
			if (FontNr == 0)
				PlotStr2(xx1, yy1, xx2, Rotation, TextAlignment, Mirror, TextStrings2[cnt3], Mode & 1);
			else
			{
				xx4 = xx1;
				yy4 = yy1;
				incX = 0.0;
				incY = 0.0;
				str2 = (WCHAR *) & TextStrings[cnt3];
				lengte = (int32) wcslen(str2);
				AllocateSpecialMem(MEM_TRUETYPE_AREAFILL, 128 * 1024, (void **) &AreaFill);

				for (cnt4 = 0; cnt4 < lengte; cnt4++)
				{
					code = (*str2);
					//    code='+';
					count = 0;
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
						incX = TRUETYPE_FONT_SPACE_EXTRA_X * xx2;
						incY = 0.0;
						RotatePoint2(&incX, &incY, Rotation);
					}

					if (DrawChar)
					{
						incX = (AreaFill->maxx - AreaFill->minx + TRUETYPE_FONT_ADD_EXTRA_X) * xx2;
						incY = 0.0;
						RotatePoint2(&incX, &incY, Rotation);
						DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
						PolygonPos = (uint8 *) DrawPolygon;

						for (cnt5 = 0; cnt5 < AreaFill->NrPolygons; cnt5++)
						{
							count = DrawPolygon->NrVertices;
#ifdef _DEBUG

							if ((DrawPolygon->PolygonType & 8) == 8)
								ok = 1;

#endif

							for (cnt6 = 0; cnt6 < count; cnt6++)
							{
								DrawPolygon->Points[cnt6].x *= xx2;
								DrawPolygon->Points[cnt6].y *= xx2;
								RotatePoint2(&DrawPolygon->Points[cnt6].x, &DrawPolygon->Points[cnt6].y, Rotation);

								if (Mirror == 1)
									DrawPolygon->Points[cnt6].x = -DrawPolygon->Points[cnt6].x;

								DrawPolygon->Points[cnt6].x += xx4;
								DrawPolygon->Points[cnt6].y += yy4;
							}

							DrawPrintPolygon(DrawPolygon, 1, 2 + (Mode & 1));
							PolygonPos += MemSizePolygon(DrawPolygon);
							DrawPolygon = (PolygonRecord *) PolygonPos;
						}
					}

					if (Mirror == 0)
					{
						xx4 += incX;
						yy4 += incY;
					}
					else
					{
						xx4 -= incX;
						yy4 += incY;
					}

					str2++;
				}
			}

			if (FontNr == 0)
			{
				if (Mirror == 0)
					xx1 += sin(ANGLE_CONVERT(Rotation)) * xx2 * 1.1;
				else
					xx1 -= sin(ANGLE_CONVERT(Rotation)) * xx2 * 1.1;

				yy1 -= cos(ANGLE_CONVERT(Rotation)) * xx2 * 1.1;
			}
			else
			{
				if (Mirror == 0)
					xx1 += sin(ANGLE_CONVERT(Rotation)) * xx2 * 1.4;
				else
					xx1 -= sin(ANGLE_CONVERT(Rotation)) * xx2 * 1.4;

				yy1 -= cos(ANGLE_CONVERT(Rotation)) * xx2 * 1.4;
			}
		}



		/*


		            if (Mirror==0) {
		              x1+=sin(ANGLE_CONVERT(RotationAngle))*x2*1.1;
		            } else {
		              x1-=sin(ANGLE_CONVERT(RotationAngle))*x2*1.1;
		            }
		            y1-=cos(ANGLE_CONVERT(RotationAngle))*x2*1.1;
		          }

		        }
		      }

		      cnt3=0;
		      cnt2=cnt3;
		      while (cnt3<Length+1) {
		        if ((str[cnt3]=='\r')
		           ||
		           ((cnt3==Length)
		           &&
		           (str[cnt3-1]!='\n'))) {
		          if (cnt3-cnt2>0) {
		            memset(TextString,0,sizeof(TextString));
		            strncpy(TextString,(LPSTR)&str[cnt2],min(127,cnt3-cnt2));
		            PlotStr2(xx1,yy1,xx2,Rotation,TextAlignment,Mirror,TextString,Mode & 1);
		          }
		          if (Mirror==0) {
		            xx1+=sin(ANGLE_CONVERT(Rotation))*xx2;
		          } else {
		            xx1-=sin(ANGLE_CONVERT(Rotation))*xx2;
		          }
		          yy1-=cos(ANGLE_CONVERT(Rotation))*xx2;
		          cnt3+=1;
		          cnt2=cnt3+1;
		        }
		        cnt3++;
		      }
		*/
		break;

	case OBJECT_POLYGON:

		/*
		      if ((Object->ObjectType2!=0)
		         ||
		         (Object->Address!=0)) {
		*/
		if (CheckObjectIsBigPolygon(Object))
		{
			GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
			AreaFillToPlot(AreaFill, Mode & 1);
			ok = 1;
		}
		else
		{
			MakePolygonFromObject(Object, DrawPolygon, 0.0, 0.0, 1, 1);

			if ((Mode & 1) == 0)
			{
				if (Object->Thickness == 0.0)
				{
					InitDrawingPlot(1);
					InitDrawingSolidBrushBlack();
				}
				else
				{
					InitDrawingPlot(Thickness);
					InitDrawingNewEmptyBrush();
				}
			}
			else
			{
				if (Object->Thickness != 0.0)
				{
					PDFNewLineWidth = Object->Thickness * PDFMultFactor * PDFMultFactor2;

					if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
					{
						PDFLineWidth = PDFNewLineWidth;
#ifdef _DEBUG

						if (PDFLineWidth < 0.0)
							ok = 1;

#endif
						sprintf(str, "%.3f w", PDFLineWidth);
						AddToMessageBuf(str);
					}
				}
			}

			if (Object->Thickness == 0.0)
				DrawPrintPolygon(DrawPolygon, 1, Mode & 1);
			else
				DrawPrintPolygon(DrawPolygon, 0, Mode & 1);
		}

		break;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PlotThermalRelief(ObjectRecord * Object, int32 mode)
{
	int32 cnt;
	uint8 PolygonBuf[10240];
	PolygonRecord *PolygonObject;

	PolygonObject = (PolygonRecord *) PolygonBuf;

	for (cnt = 0; cnt < 4; cnt++)
	{
		CopyThermalReliefInPolygon(Object, PolygonObject, Object->y2, 0.0, 1, cnt);
		DrawPrintPolygon(PolygonObject, 1, mode & 1);
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckPlotLine(double x1, double y1, double x2, double y2)
{
	int32 ok;
	double rico;
	char str[MAX_LENGTH_STRING];


	sprintf(str, "%14.5f,%14.5f - %14.5f,%14.5f  %i  %i  %i", x1, y1, x2, y2, PolygonNr, RoundingNr, RoundingCnt);
	WriteLn(Plotfp, str);

	if (x1 < -260878940.0)
		ok = 1;

	if (InRange(x1, x2))
		return 0;

	if (InRange(y1, y2))
		return 0;

	if (InRange(fabs(x1 - y1), fabs(x2 - y2)))
		return 0;

	rico = fabs((y2 - y1) / (x2 - x1));

	if ((rico > 0.9) && (rico < 1.1))
		return 0;

//  MessageBoxOwn(PCBWindow,"Plotting line",SC(24,"Error"),MB_APPLMODAL|MB_OK);
	ok = 1;

	return -1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void OutputPlots()
{
	int32 cnt3, Layer;

	if (GerberDialog(0) == 1)
	{
		if (NrGerberLayers > 0)
		{
			if ((NrGerberLayers > 1) || (GerberLayers[0] != DRILL_LAYER))
				PlotOutput(0);

			for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
			{
				if ((Layer = GerberLayers[cnt3]) == DRILL_LAYER)
					DrillOutput();
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WritePlotText(int32 Layer, double BoardMinX, double BoardMinY, double BoardMaxX, double BoardMaxY, int32 mode)
{
	int32 cnt;
	int32 FoundText = 0;
	double x1, y1, MaxDivX;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], TextLine[8][MAX_LENGTH_STRING];
	struct tm *today;
	time_t ltime;

	for (cnt = 0; cnt < 8; cnt++)
	{
		if (GerberInfo.TextLine[0] != 0)
			FoundText = 1;
	}

	if (!FoundText)
		return -1;

//  CurrentAperTureNr=CheckTraceAperTure(Design.StandardTraceWidth);
	if ((mode & 1) == 0)
	{
		InitDrawingPlot(Mult2(Design.SilkScreenWidth));
		InitDrawingNewEmptyBrush();
	}
	else
	{
		if (PDFCurrentColor == 0)
		{
			AddToMessageBuf("0 g");
			AddToMessageBuf("0 G");
			PDFCurrentColor = 1;
		}

		PDFNewLineWidth = 20 * 2540 * PDFMultFactor * PDFMultFactor2;

		if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
		{
			PDFLineWidth = PDFNewLineWidth;
			sprintf(str, "%.3f w", PDFLineWidth);
			AddToMessageBuf(str);
		}

	}

	for (cnt = 0; cnt < 8; cnt++)
	{
		strcpy(TextLine[cnt], GerberInfo.TextLine[cnt]);

		if (GerberInfo.TextLine[cnt] != 0)
		{
			if (stricmpOwn(GerberInfo.TextLine[cnt], "$DesignName") == 0)
			{
				GetFilePartFromFileName(str2, EditFile);
				CutExtensionFileName(str2);
				strcpy(TextLine[cnt], str2);
			}

			if (stricmpOwn(GerberInfo.TextLine[cnt], "$Date") == 0)
			{
				time(&ltime);
				today = localtime(&ltime);
				strftime(str2, 100, "%b %d, %Y    %X", today);
				strcpy(TextLine[cnt], str2);
			}

			if (stricmpOwn(GerberInfo.TextLine[cnt], "$Layer") == 0)
			{
				if (!GerberInfo.ReverseLayerNumbering)
					GetLayerTextObjects(Layer, TextLine[cnt], 64);
				else
					GetLayerTextObjects(Layer, TextLine[cnt], 64 + 16);
			}
		}
	}

	x1 = BoardMinX;
	y1 = BoardMinY - (TextHeight * 1.5);

	for (cnt = 0; cnt < 4; cnt++)
	{
		if (TextLine[cnt] != 0)
			PlotStr(x1, y1, TextHeight * 0.8, 0, 0, 0, (LPSTR) TextLine[cnt], mode & 1);

		y1 -= TextHeight;
	}

	MaxDivX = -10000;

	for (cnt = 0; cnt < 4; cnt++)
	{
		if (TextLine[cnt + 4] != 0)
		{
			GetMinMaxText2(x1, y1, TextHeight * 0.8, 0, 0.0, 0, 0, (LPSTR) TextLine[cnt + 4]);
			MaxDivX = max(MaxDivX, TextMaxX - TextMinX);
		}
	}

	if (MaxDivX < -1000)
		return 0;

	x1 = BoardMaxX - MaxDivX;
	y1 = BoardMinY - (TextHeight * 1.5);

	for (cnt = 0; cnt < 4; cnt++)
	{
		if (TextLine[cnt + 4] != 0)
			PlotStr(x1, y1, TextHeight * 0.8, 0, 0, 0, (LPSTR) TextLine[cnt + 4], mode & 1);

		y1 -= TextHeight;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LayerPlotOutput(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, cnt4, ViaType, Found, ViaInfo, Layer2, Mirror, PowerPlaneLayer, OkToAddObject;
	ObjectRecord *Object, *Object4, *Object2, MemObject, NewObject;
	ViaRecord *Via;
	CompRecord *Comp;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	AreaFillRecord *AreaFill;
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf2[10240];
	double x2a;

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	Object4 = &MemObject;
	Object = NULL;
	ObjectLine = NULL;
	PowerPlaneLayer = 0;
	Layer2 = Layer;

	switch (Layer)
	{
	case SILKSCREEN_BOTTOM:
	case SILKSCREEN_BOTTOM_REFS:
	case SILKSCREEN_BOTTOM_VALUES:
		Layer2 = SILKSCREEN_BOTTOM;
		break;

	case SILKSCREEN_TOP:
	case SILKSCREEN_TOP_REFS:
	case SILKSCREEN_TOP_VALUES:
		Layer2 = SILKSCREEN_TOP;
		break;
	}

	if (Layer < 32)
	{
		for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
		{
			AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

			if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer))
			{
				if (!IsLayerPowerPlane(Layer))
					AreaFillToPlot(AreaFill, mode & 1);
			}
		}

		if (IsLayerPowerPlane(Layer))
		{
			PowerPlaneLayer = 1;
			PowerPlaneToPlot(Layer, mode & 1);
		}

		if (PDFCurrentColor == 0)
		{
			AddToMessageBuf("0 g");
			AddToMessageBuf("0 G");
			PDFCurrentColor = 1;
		}


// ****************************************************************************
// ****************************************************************************
		PlotTraces(Layer, mode & 1);

// ****************************************************************************
// ****************************************************************************
		if ((Layer == 0) || (Layer == Design.NrBoardLayers - 1))
		{
//            sprintf(InfoStr,"Plot output component pins/pads layer %i ",Layer);
			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					NrObjects = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
#ifdef _DEBUG

					if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
						ok = 1;

					if (stricmpOwn(Comp->Name, "Z102") == 0)
					{
						ok = 1;
//            NrObjects=0;
					}

#endif

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if ((CheckObjectIsPolygonWithOpenSpots(Object))
						        && ((Object->Layer == -1) || (Object->Layer == Layer)))
						{
							FillPositionObject(Object);
							PlotObject(Object, (mode & 3) + 2);
						}
					}
				}
			}

// ****************************************************************************
			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					NrObjects = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
#ifdef _DEBUG

					if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
						ok = 1;

					if (stricmpOwn(Comp->Name, "Z102") == 0)
					{
						ok = 1;
//            NrObjects=0;
					}

#endif

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if ((!CheckObjectIsPolygonWithOpenSpots(Object))
						        && ((Object->Layer == -1) || (Object->Layer == Layer)))
						{
							FillPositionObject(Object);
							PlotObject(Object, (mode & 3) + 2);
						}
					}
				}
			}

// ****************************************************************************
//            sprintf(InfoStr,"Plot output vias layer %i ",Layer);

			PlotVias(Layer, mode & 3);
		}


// ****************************************************************************
// ****************************************************************************
		if ((Layer > 0) && (Layer < Design.NrBoardLayers - 1))
		{

			for (cnt = 0; cnt < Design.NrVias; cnt++)
			{
				Via = &((*Vias)[cnt]);

				if ((Via->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					Found = -1;

					Object4->x1 = Via->X;
					Object4->y1 = Via->Y;
					Object4->x2 = Via->ThickNess;
					Object4->y2 = Via->DrillThickNess;
					Object4->x3 = Via->ThermalInner;
					Object4->ObjectType = VIA_PUT_THROUGH_ROUND;
					Object4->TraceNr = -1;
					Object4->Info = 0;
					Object4->Test = 0;
					FillPositionObject(Object4);

					if ((PowerPlaneLayer) && (ViewMaxX >= Object4->minx) && (ViewMinX <= Object4->maxx)
					        && (ViewMaxY >= Object4->miny) && (ViewMinY <= Object4->maxy))
					{
						MakePolygonFromObject(Object4, PolygonObject, 0.0, 0.00001, 1, 0);

						for (cnt2 = 0; cnt2 < Design.NrAreaFills; cnt2++)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt2]]);

							if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
							        && (AreaFill->maxx >= Object4->minx) && (AreaFill->minx <= Object4->maxx)
							        && (AreaFill->maxy >= Object4->miny) && (AreaFill->miny <= Object4->maxy))
							{
								if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
									Found = cnt2;
							}
						}
					}

					if (Found != -1)
					{
						AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[Found]]);
						Object4->TraceNr = Found;

						if (AreaFill->NetNr == Via->NetNr)
						{
							if ((Via->DrillThickNess > MinDrillForThermalRelief)
							        && ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) == AREAFILL_WITH_THERMAL_RELIEF)
							        && ((AreaFill->Info & AREAFILL_WITH_NO_VIA_THERMAL_RELIEF) == 0)
							        && (NotInRange(AreaFill->ThermalReliefDistance, 0.0))
							        && (NotInRange(AreaFill->ThermalReliefThickness, 0.0)))
							{
								// Add thermal relief if diameter greater MinDrillForThermalRelief
								Object4->x2 = Object4->y2;
								Object4->y2 = AreaFill->ThermalReliefThickness;
								Object4->x3 = AreaFill->ThermalReliefDistance;
								Object4->Clearance = AreaFill->ThermalReliefThickness;
								PlotThermalRelief(Object4, 0);
							}
						}
						else
						{
							// Add anti power pad
							if (NotInRange(Object4->x3, 0.0))
								Object4->x2 = Object4->x3;
							else
								Object4->x2 += max(Object->Clearance, Design.StandardClearance) * 2;

							PlotObject(Object4, mode & 1);
						}
					}
					else
					{
						// Add normal pad
						PlotObject(Object4, mode & 1);
					}
				}
			}

// ****************************************************************************
//            sprintf(InfoStr,"Plot output component pins/pads layer %i ",Layer);
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

					if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
						ok = 1;

					if (stricmpOwn(Comp->Name, "Z102") != 0)
					{
						ok = 1;
//            NrObjects=0;
//            NrObjects2=0;
					}

#endif

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						switch (Object->ObjectType)
						{
						case PIN_PUT_THROUGH_ROUND:
						case PIN_PUT_THROUGH_SQUARE:
						case DRILL:
						case PIN_PUT_THROUGH_POLYGON:
							Found = -1;
							Object4->x1 = Object->x1;
							Object4->y1 = Object->y1;
							x2a = Object->x2;

							if (Object->ObjectType == DRILL)
							{
								Object->y2 = Object->x2;
								Object->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
								Object4->y2 = Object4->x2;
							}

							Object4->x2 = Object->x2;
							Object4->ObjectType = PIN_SMD_ROUND;
							Object4->Info = 0;
							Object4->Test = 0;
							Object4->TraceNr = -1;
							FillPositionObject(Object4);

							if ((PowerPlaneLayer) && (ViewMaxX >= Object4->minx) && (ViewMinX <= Object4->maxx)
							        && (ViewMaxY >= Object4->miny) && (ViewMinY <= Object4->maxy))
							{
								MakePolygonFromObject(Object4, PolygonObject, 0.0, 0.00001, 1, 0);

								for (cnt4 = 0; cnt4 < Design.NrAreaFills; cnt4++)
								{
									AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt4]]);

									if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
									        && (AreaFill->maxx >= Object4->minx) && (AreaFill->minx <= Object4->maxx)
									        && (AreaFill->maxy >= Object4->miny) && (AreaFill->miny <= Object4->maxy))
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
								Object4->TraceNr = Found;

								if (AreaFill->NetNr == Object->NetNr)
								{
									// Add thermal relief if diameter greater MinDrillForThermalRelief
									if ((Object->y2 > MinDrillForThermalRelief)
									        && ((AreaFill->Info & AREAFILL_WITH_THERMAL_RELIEF) ==
									            AREAFILL_WITH_THERMAL_RELIEF)
									        && (NotInRange(AreaFill->ThermalReliefDistance, 0.0))
									        && (NotInRange(AreaFill->ThermalReliefThickness, 0.0)))
									{
										Object4->ObjectType = PIN_PUT_THROUGH_ROUND;
										Object4->x2 = Object->y2;
										Object4->y2 = AreaFill->ThermalReliefThickness;
										Object4->x3 = AreaFill->ThermalReliefDistance;
										PlotThermalRelief(Object4, mode & 1);
									}
								}
								else
								{
									// Add anti power pad
									if (Object->ObjectType != DRILL)
									{
										if (NotInRange(Object->x3, 0.0))
											Object4->x2 = Object->x3;
										else
											Object4->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
									}

									PlotObject(Object4, mode & 1);
								}
							}
							else
							{
								// Check inner pad
								if (Object->ObjectType != DRILL)
								{
									Object2 = &((*Objects2)[cnt2]);

									if (NotInRange(Object2->x3, 0.0))
										Object4->x2 = Object2->x3;
								}
								else
									Object4->x2 = x2a;

								PlotObject(Object4, mode & 1);
							}

							break;

// ****************************************************************************
						case DRILL_UNPLATED:
							if (PowerPlaneLayer)
							{
								// Add anti power pad
								Object4->x1 = Object->x1;
								Object4->y1 = Object->y1;
								Object4->x2 = Object->x2;
								Object4->x2 += max(Object->Clearance, Design.StandardClearance) * 2;
								Object4->ObjectType = PIN_SMD_ROUND;
								FillPositionObject(Object4);
								PlotObject(Object4, mode & 1);
							}
							else
							{
								Object4->x1 = Object->x1;
								Object4->y1 = Object->y1;
								Object4->x2 = Object->x2;
								Object4->ObjectType = PIN_SMD_ROUND;
								Object4->Info = 0;
								Object4->Test = 0;
								Object4->TraceNr = -1;
								FillPositionObject(Object4);
								PlotObject(Object4, mode & 1);
							}

							break;

						default:
							if (Object->Layer == Layer)
							{
								if ((Object4 = GetNewObject4()) == NULL)
									return -1;

								memmove(Object4, Object, sizeof(ObjectRecord));

								switch (Object->ObjectType)
								{
								case PIN_SMD_ROUND:
									FillPositionObject(Object4);
									PlotObject(Object4, mode & 1);
									break;

								case PIN_SMD_RECT:
									FillPositionObject(Object4);
									PlotObject(Object4, mode & 1);
									break;

								case PIN_LINE_HOR:
								case PIN_LINE_VER:
								case PIN_LINE_DIAG1:
								case PIN_LINE_DIAG2:
									FillPositionObject(Object4);
									PlotObject(Object4, mode & 1);
									break;

								case PIN_LINE_ALL_ANGLE:
								case OBJECT_LINE:
									FillPositionObject(Object4);
									PlotObject(Object4, mode & 1);
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
				for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
				{
					ObjectArc = &((*ObjectArcs)[cnt]);

					if (((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
					        && ((ObjectArc->Layer == DRILL_UNPLATED_LAYER) || (ObjectArc->Layer == DRILL_LAYER)))
					{
						Object4->x1 = ObjectArc->CentreX;
						Object4->y1 = ObjectArc->CentreY;
						Object4->x2 = ObjectArc->Width + Design.StandardClearance * 2;
						Object4->y2 = 0.0;
						Object4->ObjectType = PIN_SMD_ROUND;
						Object4->Info = 0;
						Object4->Test = 0;
						Object4->TraceNr = -1;
						Object4->NetNr = -1;
						FillPositionObject(Object4);
						Found = -1;
						MakePolygonFromObject(Object4, PolygonObject, 0.0, 0.00001, 1, 0);

						for (cnt4 = 0; cnt4 < Design.NrAreaFills; cnt4++)
						{
							AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt4]]);

							if (((AreaFill->Info & (OBJECT_NOT_VISIBLE)) == 0) && (AreaFill->Layer == Layer)
							        && (AreaFill->maxx >= Object4->minx) && (AreaFill->minx <= Object4->maxx)
							        && (AreaFill->maxy >= Object4->miny) && (AreaFill->miny <= Object4->maxy))
							{
								if (CheckPolygonOverlapAreaFill(PolygonObject, AreaFill) == 1)
									Found = cnt4;
							}
						}

// Object inside a areafill (powerplane)
						if (Found != -1)
							PlotObject(Object4, mode & 1);
					}
				}
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if ((ObjectLine->Layer == Layer2)
			        || ((ObjectLine->Layer == BOARD_OUTLINE_LAYER) && (GerberInfo.PlotBoardOutline)))
			{
				Object4->x1 = ObjectLine->X1;
				Object4->y1 = ObjectLine->Y1;
				Object4->x2 = ObjectLine->X2;
				Object4->y2 = ObjectLine->Y2;
				Object4->Info = 0;
				Object4->Test = ObjectLine->LineMode;
				Object4->TraceNr = -1;
				Object4->Layer = ObjectLine->Layer;
				Object4->ObjectType = OBJECT_LINE;
				Object4->Thickness = ObjectLine->LineThickNess;
				Object4->x3 = ObjectLine->LineThickNess;
				FillPositionObject(Object4);
				PlotObject(Object4, mode & 1);
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
			if ((ObjectRect->Layer == Layer2)
			        || ((ObjectRect->Layer == BOARD_OUTLINE_LAYER) && ((ObjectRect->Info & OBJECT_FILLED) == 0)
			            && (GerberInfo.PlotBoardOutline)))
			{
				Object4->x1 = ObjectRect->CentreX;
				Object4->y1 = ObjectRect->CentreY;
				Object4->x2 = ObjectRect->Width;
				Object4->y2 = ObjectRect->Height;
				Object4->Thickness = ObjectRect->LineThickNess;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->TraceNr = -1;
				Object4->Layer = ObjectRect->Layer;

				if ((ObjectRect->Info & OBJECT_FILLED) == 0)
					Object4->ObjectType = OBJECT_RECT;
				else
					Object4->ObjectType = PIN_SMD_RECT;

				FillPositionObject(Object4);
				PlotObject(Object4, mode & 1);
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
			if ((ObjectArc->Layer == Layer2)
			        || ((ObjectArc->Layer == BOARD_OUTLINE_LAYER) && ((ObjectArc->Info & OBJECT_FILLED) == 0)
			            && (GerberInfo.PlotBoardOutline)))
			{
				Object4->x1 = ObjectArc->CentreX;
				Object4->y1 = ObjectArc->CentreY;
				Object4->x2 = ObjectArc->Width;
				Object4->y2 = ObjectArc->Height;
				Object4->x3 = ObjectArc->StartDiffX;
				Object4->y3 = ObjectArc->StartDiffY;
				Object4->x4 = ObjectArc->EndDiffX;
				Object4->y4 = ObjectArc->EndDiffY;
				Object4->Info = 0;
				Object4->Test = 0;
				Object4->TraceNr = -1;
				Object4->Layer = ObjectLine->Layer;
				Object4->Thickness = ObjectArc->LineThickNess;

				if ((ObjectArc->Info & OBJECT_FILLED) == 0)
					Object4->ObjectType = OBJECT_ARC;
				else
				{
					Object4->ObjectType = PIN_SMD_ROUND;
					Object4->Thickness = 0.0;
				}

				Object4->ObjectType = OBJECT_ARC;
				FillPositionObject(Object4);
				PlotObject(Object4, mode & 1);
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
			if (ObjectText2->Layer == Layer2)
			{
				Object4->x1 = ObjectText2->X;
				Object4->y1 = ObjectText2->Y;
				Object4->x2 = ObjectText2->FontHeight;
				Object4->y2 = 0.0;
				Object4->RotationAngle = ObjectText2->Rotation;
				Object4->Thickness = ObjectText2->LineThickNess;
				Object4->Info = 0;
				Object4->Layer = ObjectText2->Layer;
				Object4->Test = ObjectText2->FontNr << 16;
				Object4->ObjectType = OBJECT_TEXT;
				Mirror = (ObjectText2->TextMode & 0x10) >> 4;
				Object4->Mirror = Mirror;
				Object4->TraceNr = (int32) & (ObjectText2->Text);
				FillPositionObject(Object4);
				PlotObject(Object4, mode & 1);
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectPolygon->Layer == Layer2)
			{
				Object4->ObjectType = OBJECT_POLYGON;
				Object4->TraceNr = cnt;
				Object4->ObjectType2 = 0;
				Object4->Address = 0;
				Object4->Thickness = 0.0;
				FillPositionObject(Object4);
				PlotObject(Object4, mode & 1);
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

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
				Object4->x1 = Via->X;
				Object4->y1 = Via->Y;
				Object4->x2 = Via->SoldMask;
				Object4->ObjectType = PIN_SMD_ROUND;
				FillPositionObject(Object4);
				PlotObject(Object4, mode & 1);
			}
		}

		break;
	}

// ****************************************************************************
// ****************************************************************************
	switch (Layer)
	{
	case SOLD_MASK_BOTTOM:
	case SOLD_MASK_TOP:
	case PASTE_MASK_BOTTOM:
	case PASTE_MASK_TOP:
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
				ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 1);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (Object->Layer == Layer)
					{
						memcpy(Object4, Object, sizeof(ObjectRecord));
						FillPositionObject(Object4);
						PlotObject(Object4, mode & 1);
					}
				}
			}
		}

// ****************************************************************************
// ****************************************************************************
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (ObjectLine->Layer == Layer)
				{
					Object4->x1 = ObjectLine->X1;
					Object4->y1 = ObjectLine->Y1;
					Object4->x2 = ObjectLine->X2;
					Object4->y2 = ObjectLine->Y2;
					Object4->ObjectType = OBJECT_LINE;
					Object4->Clearance = ObjectLine->LineThickNess;
					Object4->Test = ObjectLine->LineMode;
					FillPositionObject(Object4);
					PlotObject(Object4, mode & 1);
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
				if (ObjectRect->Layer == Layer)
				{
					Object4->x1 = ObjectRect->CentreX;
					Object4->y1 = ObjectRect->CentreY;
					Object4->x2 = ObjectRect->Width;
					Object4->y2 = ObjectRect->Height;
					Object4->ObjectType = PIN_SMD_RECT;
					FillPositionObject(Object4);
					PlotObject(Object4, mode & 1);
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
				if (ObjectArc->Layer == Layer)
				{
					Object4->x1 = ObjectArc->CentreX;
					Object4->y1 = ObjectArc->CentreY;
					Object4->x2 = ObjectArc->Width;
					Object4->y2 = 0.0;
					Object4->ObjectType = PIN_SMD_ROUND;
					FillPositionObject(Object4);
					PlotObject(Object4, mode & 1);
				}
			}
		}

// ****************************************************************************
// ****************************************************************************
		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (ObjectPolygon->Layer == Layer)
				{
					Object4->ObjectType = OBJECT_POLYGON;
					Object4->TraceNr = cnt;
					Object4->ObjectType2 = 0;
					Object4->Address = 0;
					Object4->Thickness = 0.0;
					FillPositionObject(Object4);
					PlotObject(Object4, mode & 1);
				}
			}
		}

		break;
	}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
	switch (Layer)
	{
	case INFO_LAYER:
	case INFO_LAYER2:
	case INFO_LAYER3:
	case INFO_LAYER4:
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "U100") == 0)
				{
					ok = 1;

					if (Layer == SOLD_MASK_BOTTOM)
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
						memcpy(Object4, Object, sizeof(ObjectRecord));
						FillPositionObject(Object4);
						PlotObject(Object4, mode & 1);
					}
				}
			}
		}

		break;

	case SILKSCREEN_BOTTOM:
	case SILKSCREEN_TOP:
	case SILKSCREEN_BOTTOM_REFS:
	case SILKSCREEN_TOP_REFS:
	case SILKSCREEN_BOTTOM_VALUES:
	case SILKSCREEN_TOP_VALUES:
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "Y1") == 0)
					ok = 1;

#endif
				ShapeCompSilkScreenToObject(Comp, 0.0, 0.0, 0);
				ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, -1, 5);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (Object->Layer == Layer2)
					{
						memcpy(Object4, Object, sizeof(ObjectRecord));
						FillPositionObject(Object4);
						PlotObject(Object4, mode & 1);
					}
				}

// ****************************************************************************
				if ((Layer != SILKSCREEN_BOTTOM) && (Layer != SILKSCREEN_TOP))
				{
					OkToAddObject = 1;

					if ((((Layer == SILKSCREEN_TOP_REFS) || (Layer == SILKSCREEN_BOTTOM_REFS))
					        && ((Comp->TextVisibility & 0x100) == 0)) || (((Layer == SILKSCREEN_TOP_VALUES)
					                || (Layer == SILKSCREEN_BOTTOM_VALUES))
					                && ((Comp->TextVisibility & 0x200) == 0)))
					{
						if ((Layer == SILKSCREEN_TOP_REFS) || (Layer == SILKSCREEN_BOTTOM_REFS))
						{
							if (MakeObjectFromCompRef(Comp, &NewObject, 0) != 0)
								OkToAddObject = 0;
						}
						else
						{
							if (MakeObjectFromCompValue(Comp, &NewObject, 0) != 0)
								OkToAddObject = 0;
						}

						if (OkToAddObject)
						{
							if (((NewObject.Mirror == 0)
							        && ((Layer == SILKSCREEN_TOP_REFS) || (Layer == SILKSCREEN_TOP_VALUES)))
							        || ((NewObject.Mirror == 1)
							            && ((Layer == SILKSCREEN_BOTTOM_REFS) || (Layer == SILKSCREEN_BOTTOM_VALUES))))
							{
								memcpy(Object4, &NewObject, sizeof(ObjectRecord));
								FillPositionObject(Object4);
								PlotObject(Object4, mode & 1);
							}
						}
					}
				}
			}
		}

		break;
	}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
	if (Layer == DRILL_LAYER)
	{
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);	// With inner pads      -> Objects

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					switch (Object->ObjectType)
					{
					case PIN_PUT_THROUGH_ROUND:
					case PIN_PUT_THROUGH_SQUARE:
					case PIN_PUT_THROUGH_POLYGON:
						Object4->x1 = Object->x1;
						Object4->y1 = Object->y1;
						Object4->x2 = Object->y2;
						Object4->ObjectType = PIN_SMD_ROUND;
						FillPositionObject(Object4);
						PlotObject(Object4, mode & 1);
						break;

					case DRILL:
					case DRILL_UNPLATED:
						Object4->x1 = Object->x1;
						Object4->y1 = Object->y1;
						Object4->x2 = Object->x2;
						Object4->ObjectType = PIN_SMD_ROUND;
						FillPositionObject(Object4);
						PlotObject(Object4, mode & 1);
						break;
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrVias; cnt++)
		{
			Via = &((*Vias)[cnt]);
			ViaInfo = Via->Info;

			if ((ViaInfo & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if (NotInRange(Via->SoldMask, 0.0))
				{
					Object4->x1 = Via->X;
					Object4->y1 = Via->Y;
					Object4->x2 = Via->DrillThickNess;
					Object4->ObjectType = PIN_SMD_ROUND;
					FillPositionObject(Object4);
					PlotObject(Object4, mode & 1);
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				if ((ObjectArc->Layer == DRILL_LAYER) || (ObjectArc->Layer == DRILL_UNPLATED_LAYER))
				{
					Object4->x1 = ObjectArc->CentreX;
					Object4->y1 = ObjectArc->CentreY;
					Object4->x2 = ObjectArc->Width;
					Object4->y2 = ObjectArc->Height;
					Object4->x3 = ObjectArc->StartDiffX;
					Object4->y3 = ObjectArc->StartDiffY;
					Object4->x4 = ObjectArc->EndDiffX;
					Object4->y4 = ObjectArc->EndDiffY;
					Object4->Info = 0;
					Object4->Test = 0;
					Object4->TraceNr = -1;
					Object4->Layer = ObjectLine->Layer;
					Object4->Thickness = ObjectArc->LineThickNess;

					if ((ObjectArc->Info & OBJECT_FILLED) == 0)
						Object4->ObjectType = OBJECT_ARC;
					else
					{
						Object4->ObjectType = PIN_SMD_ROUND;
						Object4->Thickness = 0.0;
					}

					Object4->ObjectType = OBJECT_ARC;
					FillPositionObject(Object4);
					PlotObject(Object4, mode & 1);
				}
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
	ok = 1;

	if ((Layer == BOARD_OUTLINE_LAYER) || (GerberInfo.PlotBoardOutline))
	{
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

					if ((Object->Layer == Layer) || (GerberInfo.PlotBoardOutline))
					{
						FillPositionObject(Object);
						PlotObject(Object, mode & 1);
					}
				}
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
	if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
	{
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
				ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 4);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (Object->Layer == Layer)
					{
						FillPositionObject(Object);
						PlotObject(Object, mode & 1);
					}
				}
			}
		}
	}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
	if ((Layer == COMP_OUTLINE_LAYER) || (Layer == COMP_OUTLINE_LAYER + 1))
	{
		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			{
				NrObjects = 0;
				ShapeCompOutLineToObject(Comp, 0.0, 0.0, 0);

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);

					if (Object->Layer == Layer)
					{
						FillPositionObject(Object);
						PlotObject(Object, mode & 1);
					}
				}
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LayerPlotOutput2(int32 Layer, int32 mode)
{
	int32 cnt, cnt2, ObjectsAdded, MemPos, ShapeNr;
	char str[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING],
	     str7[MAX_LENGTH_STRING];
	double x1a, y1a, x2a, y2a;
	ObjectRecord *Object;
	CompRecord *Comp;
	ShapeRecord *Shape;

	ObjectsAdded = 0;

	if (mode & 1)
	{
		if ((Layer == 0) || (Layer == Design.NrBoardLayers - 1))
		{
//            sprintf(InfoStr,"Plot output component pins/pads layer %i ",Layer);
			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					NrObjects = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
					ShapeNr = (int32) Comp->ShapeNr;
					MemPos = (*Shapes)[ShapeNr].ShapePos;
					Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

					if ((Comp->Info & OBJECT_SELECTED) == OBJECT_SELECTED)
						ok = 1;

					if (stricmpOwn(Comp->Name, "Z102") == 0)
					{
						ok = 1;
//            NrObjects=0;
					}

#endif

					for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
					{
						Object = &((*Objects)[cnt2]);

						if ((Object->Layer == -1) || (Object->Layer == Layer))
						{
							FillPositionObject(Object);
//              PlotObject(Object,(mode & 3)+2);
							str4[0] = 0;
							str5[0] = 0;
							str6[0] = 0;
							str7[0] = 0;
							/*
							int32 GetPinTextFromObject(ObjectRecord *Object,LPSTR NetText,LPSTR PinText,LPSTR LayerText,
							                           LPSTR ClearanceText)
							*/
							GetPinTextFromObject(Object, str4, str5, str6, str7);

							if (CompPinText(Comp, Object->PinNr, 0.0, 0.0, str6))
								str6[0] = 0;

							ObjectsAdded++;
							AddPDFObject(0);
//              PDFObjectPos[NrPDFObjects++]=FileCurrentPointer(fp)-NrIntroBytes;
							AddToMessageBuf("<<");
							/*
							/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\( \
							 'IC3E' ,\n\t['Properties:',\n' Reference : IC3E',\n' Part No    : ',\n\
							' value      : 74HCT14' ,\n' Part description : ',\n' Geometry    : so14',\n]\
							 \); )>>
							*/

							AddToMessageBuf("/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\\( \\");

							if (str6[0] == 0)
								sprintf(str, " '%s' ,\\n\\", Comp->Name);
							else
								sprintf(str, " '%s-%s' ,\\n\\", Comp->Name, str6);

							AddToMessageBuf(str);
#if 1

							if (str4[0])
							{
								sprintf(str, " ' Net   : %s',\\n\\", str4);
								AddToMessageBuf(str);
							}

							sprintf(str, "' Geometry : %s',\\n\\", Shape->ShapeName);
							AddToMessageBuf(str);
							sprintf(str, " ' Pad   : %s',\\n\\", str5);
							AddToMessageBuf(str);
							AddToMessageBuf("\\t['',\\n]\\); )>>");
#else
							sprintf(str, "\\t['Properties:',\\n\\");
							AddToMessageBuf(str);

							/*
							              sprintf(str,"' Geometry : %s',\\n\\",Shape->ShapeName);
							              AddToMessageBuf(str);
							              if (Comp->PartNr[0]) {
							                sprintf(str,"' Part No    : %s',\\n\\",Comp->PartNr);
							                AddToMessageBuf(str);
							              }
							*/
							if (str4[0])
							{
								sprintf(str, "' Net   : %s',\\n\\", str4);
								AddToMessageBuf(str);
							}

							sprintf(str, "' Pad   : %s',\\n\\", str5);
							AddToMessageBuf(str);

							AddToMessageBuf("]\\); )>>");
#endif
							AddToMessageBuf("/Type /Annot ");
							AddToMessageBuf("/Subtype /Link ");
#if 0
							x1 = (int32) PDFConvX(Object->minx);
							y1 = (int32) PDFConvY(Object->miny);
							x2 = (int32) PDFConvX(Object->maxx);
							y2 = (int32) PDFConvY(Object->maxy);
							x2 = max(x2, x1 + 1);
							y2 = max(y2, y1 + 1);
							sprintf(str, "/Rect [ %d %d %d %d ]", x1, y1, x2, y2);
#else
							x1a = PDFConvX(Object->minx);
							y1a = PDFConvY(Object->miny);
							x2a = PDFConvX(Object->maxx);
							y2a = PDFConvY(Object->maxy);
							sprintf(str, "/Rect [ %.2f %.2f %.2f %.2f ]", x1a, y1a, x2a, y2a);
#endif
							AddToMessageBuf(str);
							AddToMessageBuf("/Border [ 0 0 0 [ 4 2 ] ] ");
							AddToMessageBuf("/F 64 ");
							AddToMessageBuf("/C [ 1 1 0 ] ");
							AddToMessageBuf(">>");
							AddToMessageBuf("endobj");
						}
					}
				}
			}
		}
	}

	return ObjectsAdded;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PlotOutput(int32 mode)
{
	int32 cnt3, ObjectCount, CompOutlineLayers, res2, PageX, PageY, OldDrawWindowMinX, OldDrawWindowMinY,
	      OldDrawWindowMaxX, OldDrawWindowMaxY, Layer, PixelsPerInchX, PagePixelsY, PagePixelsX;
	ObjectRecord *Object, *Object4, MemObject;
	char str[MAX_LENGTH_STRING];
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf2[10240];
	char InfoCopy[MAX_LENGTH_STRING];
	double Factor2, Factor300, Factor360, Factor600, Factor720, Factor1200, MinX, MaxX, MinY, MaxY, MinY2;
	int32 res;

	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	Object4 = &MemObject;
	Object = NULL;
	GerberInfo.ScaleFactor = 1.0;

	if (PlotDialog(0) == 2)
		return -1;

	res = FindMinMaxBoard(&MinX, &MinY, &MaxX, &MaxY, 1);

	switch (res)
	{
	case 2:
		break;

	case 3:
		sprintf(str, SC(400, "There are objects outside the board outline\n\n"));
		strcat(str, SC(401, "Do you want to continue ?"));

		if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDNO)
			return -1;

		break;
	}

	MinY2 = MinY;
	MinY -= TextHeight * 4.5;

// *******************************************************************************************************

	memset(&Pd, 0, sizeof(PRINTDLG));
	Pd.lStructSize = sizeof(PRINTDLG);
	Pd.hwndOwner = PCBWindow;
	Pd.Flags = PD_RETURNDC;
	PdDocInfo.cbSize = sizeof(DOCINFO);
	PdDocInfo.lpszDocName = PrinterDocName;
	ObjectCount = 0;
	Factor2 = Factor;

	ReverseX = GerberInfo.Invert;

	if ((res = PrintDlg(&Pd)) != 0)
	{

		strcpy(InfoCopy, InfoStr);

		OldDrawWindowMinX = DrawWindowMinX;
		OldDrawWindowMinY = DrawWindowMinY;
		OldDrawWindowMaxX = DrawWindowMaxX;
		OldDrawWindowMaxY = DrawWindowMaxY;

		CheckInputMessages(0);

#ifdef _DEBUG
		sprintf(str, "%s\\pcb\\Plot.txt", DesignPath); //název souboru

		if ((Plotfp = FileOpenWriteUTF8(str)) <= 0)
			return -1;

#endif

		PixelsPerInchX = GetDeviceCaps(Pd.hDC, LOGPIXELSX);
		PixelsPerInchY = GetDeviceCaps(Pd.hDC, LOGPIXELSY);
		PageX = GetDeviceCaps(Pd.hDC, HORZSIZE);
		PageY = GetDeviceCaps(Pd.hDC, VERTSIZE);

		res2 = StartDoc(Pd.hDC, &PdDocInfo);
		OutputDisplay = Pd.hDC;



		MinY -= (1000 * 2540);
		PrintOffsetX = MinX - ((MaxX - MinX) * 0.025);
//      PrintOffsetY=MaxY+((MaxY-MinY)*0.025);
//      ReverseY=0;

		PrintOffsetY = MinY - ((MaxY - MinY) * 0.025);
		ReverseY = 1;

#ifdef _DEBUG
		sprintf(str, "OffsetX,OffsetY %.5f,%.5f", PrintOffsetX, PrintOffsetY);
		WriteLn(Plotfp, str);
#endif

		Factor = (PixelsPerInchY / 2540000.0);
		Factor300 = (300 / 2540000.0);
		Factor360 = (360 / 2540000.0);
		Factor600 = (600 / 2540000.0);
		Factor720 = (720 / 2540000.0);
		Factor1200 = (1200 / 2540000.0);
//      Factor=(PageX/(MaxX-MinX))*9.5;
//      Fact2=((MaxX-MinX)*210)/((MaxY-MinY)*297);
//      if (Fact2<1.0) {
//        Factor*=Fact2;
//      }
		Factor *= GerberInfo.ScaleFactor;

		CompOutlineLayers = 0;

		for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
		{
			Layer = GerberLayers[cnt3];

			switch (Layer)
			{
			case COMP_OUTLINE_LAYER:
			case COMP_OUTLINE_LAYER + 1:
				CompOutlineLayers++;
				break;
			}
		}

		SetWaitCursor();

		for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
		{

//      ReverseY=!Printing;

			res2 = StartPage(Pd.hDC);
			res2 = SetMapMode(Pd.hDC, MM_TEXT);

			DrawWindowMinX = 0L;
			DrawWindowMinY = -100000L;
			DrawWindowMaxX = 100000L;
//      DrawWindowMaxY=0;
			PagePixelsX = GetDeviceCaps(Pd.hDC, HORZRES);
			PagePixelsY = GetDeviceCaps(Pd.hDC, VERTRES);

			if (ReverseX)
				DrawWindowMaxX = PagePixelsX;

			DrawWindowMaxY = PagePixelsY;
//      DrawWindowMaxY=100000L;

			ViewMinX = -1.0e30;
			ViewMaxX = 1.0e30;
			ViewMinY = -1.0e30;
			ViewMaxY = 1.0e30;

//      EndPage(Pd.hDC);
//      EndDoc(Pd.hDC);

			Layer = GerberLayers[cnt3];
			GetLayerTextObjects(Layer, str, 5);
			str[0] = (char) tolower(str[0]);
			sprintf(InfoStr, "Plot %s", str);
			OutputDisplayCopy = OutputDisplay;
			RedrawInfoStr(1);
			OutputDisplay = OutputDisplayCopy;
			LayerPlotOutput(Layer, 0);
			WritePlotText(Layer, MinX, MinY2, MaxX, MaxY, 0);

			DeletePlotObjects();
			EndPage(Pd.hDC);
		}


#ifdef _DEBUG
		FileClose(Plotfp);
#endif
		SetNormalCursor();
		EndDoc(Pd.hDC);
		DeleteDC((HDC) & Pd);
		DeallocateSpecialMem(MEM_POWERPLANE_AREAFILL);
		OutputDisplay = 0;
		ReverseY = 1;
		Factor = Factor2;
		DrawWindowMinX = OldDrawWindowMinX;
		DrawWindowMinY = OldDrawWindowMinY;
		DrawWindowMaxX = OldDrawWindowMaxX;
		DrawWindowMaxY = OldDrawWindowMaxY;
		ViewMinX = PixelToRealOffX(-1);
		ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
		ViewMinY = PixelToRealOffY(-1);
		ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
		strcpy(InfoStr, InfoCopy);
		RedrawInfoStr(1);
		MessageBoxOwn(PCBWindow, SC(1022, "Plot output ready"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ExportOutput(int32 mode)
{
#define BIT_ACTIVE(LineBytes,x) (((LineBytes[((x) >> 3)] & ((0x80 >> ((x) & 7)))) != 0) ? 1 : 0)
	/*
	  BitmapMode=0; // Save monochrome bitmap uncompressed
	  BitmapMode=1; // Save compressed bitmap 8 bit per pixel
	*/

	int32 result, fp, Layer, cnt, cnt2, cnt3, cnt4, cnt5, NrBitmapColors, ColorIndex, ColorPixelCount, NrCodedBytes,
	      NrBytesBitmapData, cntx, cnty, SectionsX, SectionsY, OldDrawWindowMinX, OldDrawWindowMinY, CompOutlineLayers,
	      OldDrawWindowMaxX, OldDrawWindowMaxY, MemPos, MaxScreenX, MaxScreenY, res, LinesToMove, LineBytesToMove,
	      PixelsX, PixelsY, NrBytesSubBitmap, SubBitmapMemsize, BitmapMemsize, PowerPlaneLayer;
	ObjectRecord *Object, *Object4, MemObject;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING], InfoCopy[MAX_LENGTH_STRING];
	PolygonRecord *PolygonObject;
	uint8 PolygonBuf2[10240], BitmapInfoMem[1024], *LineBytes, CodedLine[4096];
	double hulp, Factor2, Xoffsets[32], Yoffsets[32], Resolution, MinX, MaxX, MinY, MaxY, MinY2, ScreenX, ScreenY;

	HBITMAP ViewBitmap;
	BITMAPINFO *BitmapInfo;
	BITMAPV4HEADER *BitmapHeader;
	HDC OutputDisplayCopy;
	uint32 CurrentColor, TempColor;

	BmpHeaderRecord BmpHeader;
	uint8 TwoColors[8] = { 0, 0, 0, 0, 255, 255, 255, 0 };

	strcpy(InfoCopy, InfoStr);

	LinesToMove = 0;
	PolygonObject = (PolygonRecord *) & PolygonBuf2;
	Object4 = &MemObject;
	Object = NULL;
	ReverseY = 1;

	if (ExportBitmapDialog(0) == 2)
		return -1;


	res = FindMinMaxBoard(&MinX, &MinY, &MaxX, &MaxY, 1);
	hulp = MaxX - MinX;

	if (hulp < 80.0e5)
		MaxX = MinX + 80.0e5;

	switch (res)
	{
	case 2:
		break;

	case 3:
		sprintf(str, SC(400, "There are objects outside the board outline\n\n"));
		strcat(str, SC(401, "Do you want to continue ?"));

		if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDNO)
			return -1;

		break;
	}

	MinY2 = MinY;
	MinY -= TextHeight * 4.5;

	Factor2 = Factor;
//  ReverseX=GerberInfo.Invert;
	OldDrawWindowMinX = DrawWindowMinX;
	OldDrawWindowMinY = DrawWindowMinY;
	OldDrawWindowMaxX = DrawWindowMaxX;
	OldDrawWindowMaxY = DrawWindowMaxY;

	CheckInputMessages(0);

	sprintf(str, "%s\\pcb\\bmp", DesignPath);
	CreateDirectoryUTF8(str);
#ifdef _DEBUG
	sprintf(str, "%s\\pcb\\Plot.txt", DesignPath); //název souboru

	if ((Plotfp = FileOpenWriteUTF8(str)) <= 0)
		return -1;

#endif

	SetWaitCursor();
//  DeleteGraphicObjects();
//  CreateDrawObjects(1);
	Printing = 0;


	memset(&BitmapInfoMem, 0, sizeof(BitmapInfoMem));
	BitmapInfo = (BITMAPINFO *) BitmapInfoMem;
	BitmapHeader = (BITMAPV4HEADER *) & BitmapInfo->bmiHeader;
	BitmapHeader->bV4Size = sizeof(BITMAPV4HEADER);
	BitmapHeader->bV4Planes = 1;
	BitmapHeader->bV4BitCount = 1;
	BitmapHeader->bV4V4Compression = BI_RGB;

	OutputDisplay = CreateCompatibleDC(0);

	PixelsX = GetDeviceCaps(OutputDisplay, HORZRES);
	PixelsY = GetDeviceCaps(OutputDisplay, VERTRES);
	BitmapHeader->bV4Width = PixelsX;
	BitmapHeader->bV4Height = PixelsY;

	if (ReverseY)
	{
		DrawWindowMinX = 0;
		DrawWindowMinY = 0;
		DrawWindowMaxX = PixelsX;
		DrawWindowMaxY = PixelsY;
	}
	else
	{
		DrawWindowMinX = 0;
		DrawWindowMinY = 0;
		DrawWindowMaxX = PixelsX;
		DrawWindowMaxY = PixelsY;

//    DrawWindowMinX=-20000;
//    DrawWindowMinY=-20000;
//    DrawWindowMaxX= 20000;
//    DrawWindowMaxY= 20000;
	}

	SetBkMode(OutputDisplay, TRANSPARENT);
	SetROP2(OutputDisplay, R2_COPYPEN);

	NrBytesSubBitmap = PixelsX / 8;
	ViewBitmap = CreateCompatibleBitmap(OutputDisplay, PixelsX, PixelsY);
	SelectObject(OutputDisplay, ViewBitmap);

	Factor = (1 / GerberInfo.BitmapExportResolution);
	Resolution = (2540000.0 * Factor);	// dpi
	MaxScreenX = Mult2((MaxX - MinX) * 1.05);
	MaxScreenX = ((MaxScreenX + 31) / 32) * 32;
	MaxScreenY = Mult2((MaxY - MinY) * 1.05);
	Xoffsets[0] = MinX - ((MaxX - MinX) * 0.025);
	Yoffsets[0] = MinY - ((MaxY - MinY) * 0.025);
	SectionsX = (MaxScreenX + PixelsX - 1) / PixelsX;
	SectionsY = (MaxScreenY + PixelsY - 1) / PixelsY;
	ScreenX = PixelToReal(PixelsX);
	ScreenY = PixelToReal(PixelsY);

	for (cnty = 1; cnty < SectionsY; cnty++)
		Yoffsets[cnty] = Yoffsets[0] + cnty * ScreenY;

	for (cntx = 1; cntx < SectionsX; cntx++)
		Xoffsets[cntx] = Xoffsets[0] + cntx * ScreenX;

	SubBitmapMemsize = NrBytesSubBitmap * PixelsY;
	AllocateMemTemp(SubBitmapMemsize);
	BitmapMemsize = (MaxScreenX / 8) * MaxScreenY;
	AllocateMemTemp2(SubBitmapMemsize * SectionsX + (256 + 32) * 1024 + 128 * 1024);

	memset(&BmpHeader, 0, sizeof(BmpHeader));

	if ((GerberInfo.BitmapExportSaveMode & 1) == 1)
		NrBitmapColors = 2;
	else
		NrBitmapColors = 256;

	BmpHeader.Identifier = 0x4d42;
	BmpHeader.StartOfDataOffset = 54 + NrBitmapColors * 4;
	BmpHeader.BitmapHeaderSize = 40;
	BmpHeader.Width = MaxScreenX;
	BmpHeader.Height = MaxScreenY;


	if ((GerberInfo.BitmapExportSaveMode & 1) == 1)
	{
		BmpHeader.BitmapDataSize = BitmapMemsize;
		BmpHeader.FileSize = sizeof(BmpHeader) + NrBitmapColors * 4 + BmpHeader.BitmapDataSize;
		BmpHeader.BitsPerPixel = 1;
	}
	else
	{
		BmpHeader.BitsPerPixel = 8;
		BmpHeader.CompressionType = 1;	// RLE
	}

	BmpHeader.NrOfPlanes = 1;
	BmpHeader.NrColors1 = NrBitmapColors;
	BmpHeader.NrImportedColors = 0;
	BmpHeader.HResolutionInPixelsPerMeter = (int32) (1000.0 / (25.4 / Resolution));
	BmpHeader.VResolutionInPixelsPerMeter = (int32) (1000.0 / (25.4 / Resolution));

	CompOutlineLayers = 0;

	for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
	{
		Layer = GerberLayers[cnt3];

		switch (Layer)
		{
		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			CompOutlineLayers++;
			break;
		}
	}

	PrintToBitmap = 1;
	SystemBusyMode = 400;

	for (cnt5 = 0; cnt5 < NrGerberLayers; cnt5++)
	{
		Layer = GerberLayers[cnt5];

		if (!GerberInfo.ReverseLayerNumbering)
			GetLayerTextObjects(Layer, str, 0);
		else
			GetLayerTextObjects(Layer, str, 16);

		str[0] = (char) tolower(str[0]);
		sprintf(InfoStr, SC(1023, "Bitmap output - %s"), str);
		OutputDisplayCopy = OutputDisplay;
		RedrawInfoStr(1);
		CheckInputMessages(0);
//    WindowPaint();
		OutputDisplay = OutputDisplayCopy;
		PowerPlaneLayer = CheckIfLayerIsAPowerPlane(Layer);
		sprintf(FileStr, "%s\\pcb\\bmp\\", DesignPath);

		if (!GerberInfo.ReverseLayerNumbering)
			GetLayerTextObjects(Layer, str2, 64 + 5);
		else
			GetLayerTextObjects(Layer, str2, 64 + 16 + 5);

		strcat(FileStr, str2);
		strcat(FileStr, ".bmp");

#ifdef _DEBUG
		sprintf(str, "OffsetX,OffsetY %.5f,%.5f", PrintOffsetX, PrintOffsetY);
		WriteLn(Plotfp, str);
#endif


// ****************************************************************************
// ****************************************************************************

//    SectionsX=1;
//    SectionsY=1;

		NrBytesBitmapData = 0;

		fp = FileOpenWriteUTF8(FileStr);

		FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
		FileWrite(fp, &TwoColors, 8, &result);

		if ((GerberInfo.BitmapExportSaveMode & 1) == 0)
		{
			for (cnt = 2; cnt < 256; cnt++)
				FileWrite(fp, &TwoColors, 4, &result);
		}

		for (cnty = 0; cnty < SectionsY; cnty++)
		{
			if (ReverseY)
				PrintOffsetY = Yoffsets[cnty];
			else
				PrintOffsetY = Yoffsets[SectionsY - cnty - 1];

			for (cntx = 0; cntx < SectionsX; cntx++)
			{
				PrintOffsetX = Xoffsets[cntx];
				ViewMinX = (PrintOffsetX - 100000.0);
				ViewMaxX = (ViewMinX + ScreenX + 100000.0);
				ViewMinY = (PrintOffsetY - 100000.0);
				ViewMaxY = (ViewMinY + ScreenY + 100000.0);
				/*
				        ViewMinX=-1.0e30;
				        ViewMaxX= 1.0e30;
				        ViewMinY=-1.0e30;
				        ViewMaxY= 1.0e30;
				*/

				InitDrawingSolidBrushWhite();
//        InitDrawingEmptyPen();
				Rectangle(OutputDisplay, 0, 0, PixelsX + 1, PixelsY + 1);
//        FillRect(OutputDisplay,&Rect,GraphicsObjectBrush[BackGroundObjectNr]);

				InitDrawingPlot(1);
				InitDrawingSolidBrushBlack();
				LayerPlotOutput(Layer, 0);
				WritePlotText(Layer, MinX, MinY2, MaxX, MaxY, 0);

// ****************************************************************************

				CurrentObjectCode = -1;

				res = GetDIBits(OutputDisplay, ViewBitmap, 0, PixelsY, TempMem, BitmapInfo, DIB_RGB_COLORS);

				if (cntx < SectionsX - 1)
					LineBytesToMove = NrBytesSubBitmap;
				else
				{
					LineBytesToMove = (MaxScreenX / 8) % NrBytesSubBitmap;

					if (LineBytesToMove == 0)
						LineBytesToMove = NrBytesSubBitmap;
				}

				if (ReverseY)
				{
					if (cnty < SectionsY - 1)
						LinesToMove = PixelsY;
					else
					{
						LinesToMove = MaxScreenY % PixelsY;

						if (LinesToMove == 0)
							LinesToMove = PixelsY;
					}

					for (cnt = 0; cnt < LinesToMove; cnt++)
					{
						MemPos = cntx * NrBytesSubBitmap;
						MemPos += cnt * (MaxScreenX / 8);
						memmove(&TempMem2[MemPos], &TempMem[cnt * NrBytesSubBitmap], LineBytesToMove);
					}
				}
				else
				{
					if (cnty > 0)
					{
						LinesToMove = PixelsY;

						for (cnt = 0; cnt < LinesToMove; cnt++)
						{
							MemPos = cntx * NrBytesSubBitmap;
							MemPos += cnt * (MaxScreenX / 8);
							memmove(&TempMem2[MemPos], &TempMem[cnt * NrBytesSubBitmap], LineBytesToMove);
						}
					}
					else
					{
						LinesToMove = MaxScreenY % PixelsY;

						if (LinesToMove == 0)
							LinesToMove = PixelsY;

						cnt4 = PixelsY - LinesToMove;

//            cnt4=0;
						for (cnt = 0; cnt < LinesToMove; cnt++)
						{
							MemPos = cntx * NrBytesSubBitmap;
							MemPos += cnt * (MaxScreenX / 8);
							memmove(&TempMem2[MemPos], &TempMem[(cnt4 + cnt) * NrBytesSubBitmap], LineBytesToMove);
						}
					}
				}

				res = 1;
			}

// ****************************************************************************
			if ((GerberInfo.BitmapExportSaveMode & 1) == 1)
			{
				if (!GerberInfo.Invert)
					FileWrite(fp, TempMem2, LinesToMove * (MaxScreenX / 8), &result);
				else
				{
					// Swap the bits
					for (cnt = 0; cnt < LinesToMove; cnt++)
					{
						LineBytes = &TempMem2[cnt * (MaxScreenX / 8)];
						SwapBitsLine((uint8 *) LineBytes, MaxScreenX / 8);
					}
				}
			}
			else
			{
				for (cnt = 0; cnt < LinesToMove; cnt++)
				{
					LineBytes = &TempMem2[cnt * (MaxScreenX / 8)];

					if (GerberInfo.Invert)
						SwapBitsLine((uint8 *) LineBytes, MaxScreenX / 8);

					CurrentColor = (uint32) - 1;
					ColorPixelCount = 0;
					NrCodedBytes = 0;
					cnt3 = 0;
					ColorIndex = 0;

					for (cnt2 = 0; cnt2 < MaxScreenX; cnt2++)
					{
						if (BIT_ACTIVE(LineBytes, cnt2))
						{	// White
							TempColor = 0xffffff;
							cnt3 = 1;
						}
						else
						{
							TempColor = 0;
							cnt3 = 0;
						}

						if (CurrentColor == (uint32) - 1)
						{
							ColorPixelCount = 1;
							CurrentColor = TempColor;
							ColorIndex = cnt3;
						}
						else
						{
							if (CurrentColor == TempColor)
								ColorPixelCount++;
							else
							{	// Found a different color, store the previous color pixels
								cnt4 = ColorPixelCount;

								while (cnt4 > 0)
								{
									if (cnt4 >= 255)
									{
										CodedLine[NrCodedBytes++] = (uint8) 255;
										cnt4 -= 255;
									}
									else
									{
										CodedLine[NrCodedBytes++] = (uint8) cnt4;
										cnt4 = 0;
									}

									CodedLine[NrCodedBytes++] = (uint8) ColorIndex;
								}

								ColorPixelCount = 1;
								CurrentColor = TempColor;
								ColorIndex = cnt3;
							}
						}
					}

					if (ColorPixelCount > 0)
					{
						cnt4 = ColorPixelCount;

						while (cnt4 > 0)
						{
							if (cnt4 >= 255)
							{
								CodedLine[NrCodedBytes++] = 255;
								cnt4 -= 255;
							}
							else
							{
								CodedLine[NrCodedBytes++] = (uint8) cnt4;
								cnt4 = 0;
							}

							CodedLine[NrCodedBytes++] = (uint8) ColorIndex;
						}
					}

					CodedLine[NrCodedBytes++] = 0;
					CodedLine[NrCodedBytes++] = 0;
					NrBytesBitmapData += NrCodedBytes;
					FileWrite(fp, (uint8 *) CodedLine, NrCodedBytes, &result);
				}
			}
		}

// ****************************************************************************

		if ((GerberInfo.BitmapExportSaveMode & 1) == 0)
		{
			FileSeek(fp, 0);
			BmpHeader.BitmapDataSize = NrBytesBitmapData;
			BmpHeader.FileSize = sizeof(BmpHeader) + NrBitmapColors * 4 + BmpHeader.BitmapDataSize;
			FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
		}

		FileClose(fp);
		res = 1;				// Layer

	}

	DeleteObject(ViewBitmap);
	DeleteDC(OutputDisplay);

	DeletePlotObjects();
	DeAllocateMemTemp();
	DeAllocateMemTemp2();
	ok = 1;

#ifdef _DEBUG
	FileClose(Plotfp);
#endif
	SetNormalCursor();
	Printing = 0;
	ReverseY = 1;
	OutputDisplay = NULL;

	Factor = Factor2;
	DrawWindowMinX = OldDrawWindowMinX;
	DrawWindowMinY = OldDrawWindowMinY;
	DrawWindowMaxX = OldDrawWindowMaxX;
	DrawWindowMaxY = OldDrawWindowMaxY;
	ViewMinX = PixelToRealOffX(-1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(-1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);
	PrintToBitmap = 0;
	SystemBusyMode = 0;
	strcpy(InfoStr, InfoCopy);
	RedrawInfoStr(1);
	sprintf(str, SC(428, "Bitmap output ready.\n\nOutput files are located in dierctory\n\n%s\\pcb\\bmp"), DesignPath);
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);

	return 0;
}

//*************************************************************************************************************************
//********************************* export pdf ****************************************************************************
//*************************************************************************************************************************

int32 ExportToPDF(int32 PaperSize, int32 Orientation, int32 FitToPage, int32 mode)
{
	double LineWidth, PageSizeX, PageSizeY, dx, dy, NewLineWidth, PageSizeX2, PageSizeY2, hulp, MinX, MaxX, MinY, MaxY,
	       MinY2, AnnotationsAdded, hulpx, hulpy, DisplX, DisplY;
	int32 res, fp, cnt, cnt2, Layer, cnt3, hulp3, PageSizeUnitsX, PageSizeUnitsY, CompOutlineLayers, pos, PageCount,
	      NrPages, CurrentColor, PageObjectNr, NrPagesWritten, WritePageObjects, PageObjectStartNr[64];
	char str[4096], str2[4096], PageTitles[64][MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING],
	     InfoCopy[MAX_LENGTH_STRING];

	ObjectRecord NewObject;
	struct tm *today;
	time_t ltime;

	memset(&NewObject, 0, sizeof(NewObject));
	strcpy(InfoCopy, InfoStr);

	if (mode == 0)
	{
		if ((res = (ExportPDFDialog(0))) == 2)
			return -1;

		Orientation = PDFInfo.PaperOrientation;
		PaperSize = PDFInfo.PaperSize;
		FitToPage = PDFInfo.PaperFitToPage;
	}
	else
	{
	}

	FindMinMaxBoard(&MinX, &MinY, &MaxX, &MaxY, 1);

	MinY2 = MinY;
	MinY -= TextHeight * 4.5;

	PDFPageOffsetX = 10e5;
	PDFPageOffsetY = 10e5;
	PageSizeX = 0.0;
	PageSizeY = 0.0;
	PageSizeUnitsX = 0;
	PageSizeUnitsY = 0;

// 0.35277777
	switch (PaperSize)
	{
	case PAPERSIZE_A1:
		PageSizeX = 840e5;
		PageSizeY = 594e5;
		PageSizeUnitsX = 2381;
		PageSizeUnitsY = 1684;
		break;

	case PAPERSIZE_A2:
		PageSizeX = 594e5;
		PageSizeY = 420e5;
		PageSizeUnitsX = 1684;
		PageSizeUnitsY = 1191;
		break;

	case PAPERSIZE_A3:
		PageSizeX = 420e5;
		PageSizeY = 297e5;
		PageSizeUnitsX = 1191;
		PageSizeUnitsY = 842;
		break;

	case PAPERSIZE_A4:
		PageSizeX = 297e5;
		PageSizeY = 210e5;
		PageSizeUnitsX = 842;
		PageSizeUnitsY = 596;
		break;

	case PAPERSIZE_A5:
		PageSizeX = 210e5;
		PageSizeY = 147e5;
		PageSizeUnitsX = 596;
		PageSizeUnitsY = 421;
		break;

	case PAPERSIZE_B4:
		PageSizeX = 353e5;
		PageSizeY = 250e5;
		PageSizeUnitsX = 612;
		PageSizeUnitsY = 792;
		break;

	case PAPERSIZE_B5:
		PageSizeX = 250e5;
		PageSizeY = 176e5;
		PageSizeUnitsX = 612;
		PageSizeUnitsY = 792;
		break;

	case PAPERSIZE_B4_JIS:
		PageSizeX = 364e5;
		PageSizeY = 257e5;
		PageSizeUnitsX = 612;
		PageSizeUnitsY = 792;
		break;

	case PAPERSIZE_B5_JIS:
		PageSizeX = 257e5;
		PageSizeY = 182e5;
		PageSizeUnitsX = 612;
		PageSizeUnitsY = 792;
		break;

	case PAPERSIZE_LEGAL:
		PageSizeX = 355.6e5;
		PageSizeY = 215.9e5;
		PageSizeUnitsX = 1008;
		PageSizeUnitsY = 612;
		break;

	case PAPERSIZE_LETTER:
		PageSizeX = 279.4e5;
		PageSizeY = 215.9e5;
		PageSizeUnitsX = 792;
		PageSizeUnitsY = 612;
		break;
	}

	PageSizeX2 = PageSizeX;
	PageSizeY2 = PageSizeY;
	PageSizeX2 -= 2 * PDFPageOffsetX;
	PageSizeY2 -= 2 * PDFPageOffsetY;
	PDFStartX = MinX;
	PDFStartY = MinY;
	dx = MaxX - MinX;
	dy = MaxY - MinY;
	hulp = 0.0;
	hulp3 = 0;
	PDFMultFactor = 1.0;

	switch (Orientation)
	{
	case ORIENTATION_PORTRAIT:
		hulp = PageSizeY2;
		PageSizeY2 = PageSizeX2;
		PageSizeX2 = hulp;
		hulp3 = PageSizeUnitsX;
		PageSizeUnitsX = PageSizeUnitsY;
		PageSizeUnitsY = hulp3;
		break;

	case ORIENTATION_LANDSCAPE:
		break;

	case ORIENTATION_AUTO:
		if (dx < dy)
		{	// Set portrait
			hulp = PageSizeY2;
			PageSizeY2 = PageSizeX2;
			PageSizeX2 = hulp;
			hulp3 = PageSizeUnitsX;
			PageSizeUnitsX = PageSizeUnitsY;
			PageSizeUnitsY = hulp3;
		}

		break;
	}

	hulpx = dx / PageSizeX2;
	hulpy = dy / PageSizeY2;

	if (hulpx > hulpy)
	{
		DisplX = dx;
		DisplY = DisplX * PageSizeY2 / PageSizeX2;
		PDFMultFactor = PageSizeX2 / dx;
	}
	else
	{
		DisplY = dy;
		DisplX = DisplY * PageSizeX2 / PageSizeY2;
		PDFMultFactor = PageSizeY2 / dy;
	}

	if (FitToPage == 0)
		PDFMultFactor = 1.0;

	PDFMultFactor2 = (72 / 2540000.0);

	LineWidth = 1.0;
	CurrentColor = 1;
	NewLineWidth = 0.0;
	PageCount = 0;
	NrPagesWritten = 0;
	WritePageObjects = 1;
	NrPDFObjects = 0;

	ViewMinX = -1e9;
	ViewMinY = -1e9;
	ViewMaxX = 1e9;
	ViewMaxY = 1e9;

	strcpy(FileStr, EditFile);
	CutExtensionFileName(FileStr);
	strcat(FileStr, ".pdf"); //název souboru

	//********************************** soubor již existuje ***********************************************
	if (FileExistsUTF8(FileStr) == 0)
	{
		sprintf(str2, SC(788, "File already exists.\n\n%s\n\nDo you want to overwrite it ?"), FileStr);

		if (MessageBoxOwn(PCBWindow, str2, SC(1, "Message"), MB_OKCANCEL | MB_APPLMODAL) != IDOK)
			return -1;
	}
	//******************************************************************************************************

	fp = FileOpenWriteUTF8(FileStr);

	if (fp <= 0)
	{
		sprintf(str, SC(1024, "Can not open file %s"), FileStr);
		MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	pdffp = fp;
	SetWaitCursor();

	WriteToFile(fp, "%PDF-1.2\n");
	WriteToFile(fp, "\x25\xe2\xe3\xcf\xd3\n");
	NrPages = NrGerberLayers;

	AllocateMemTemp(1024 * 1024);
	CompOutlineLayers = 0;

	for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
	{
		Layer = GerberLayers[cnt3];

		switch (Layer)
		{
		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			CompOutlineLayers++;
			break;
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************

	for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
	{
		Layer = GerberLayers[cnt3];

		if (!GerberInfo.ReverseLayerNumbering)
			GetLayerTextObjects(Layer, str, 64);
		else
			GetLayerTextObjects(Layer, str, 64 + 16);

		str[0] = (char) tolower(str[0]);
		sprintf(InfoStr, "Plot %s", str);
		RedrawInfoStr(1);

		if (!GerberInfo.ReverseLayerNumbering)
			GetLayerTextObjects(Layer, str, 64);
		else
			GetLayerTextObjects(Layer, str, 64 + 16);

		strcpy(PageTitles[cnt3], str);

		NrPagesWritten++;
		MessageBufPos = 0;
		AddToMessageBuf("1 J");

		if (PDFCurrentColor == 0)
		{
			AddToMessageBuf("0 g");
			AddToMessageBuf("0 G");
			PDFCurrentColor = 1;
		}

		LayerPlotOutput(Layer, 1);
		WritePlotText(Layer, MinX, MinY2, MaxX, MaxY, 1);
		AddPDFObject(1);
		PageObjectStartNr[PageCount] = NrPDFObjects;
#if 0
		AllocateMemTemp(MessageBufPos);

		res = zlib_inflate(MessageBuf, MessageBufPos);

//    res=zlib_inflate_ascii85(MessageBuf,MessageBufPos);
		if (res)
		{
			sprintf(str, "<< /Length %d\r\n", res);
			WriteToFile(fp, str);
			WriteToFile(fp, "/Filter /FlateDecode\r\n");
			//    WriteToFile(fp,"/Filter [/ASCII85Decode /FlateDecode]\r\n");
			WriteToFile(fp, ">>\r\n");
			WriteToFile(fp, "stream\r\n");
			WriteToFile(fp, (LPSTR) TempMem);
		}
		else
		{
			sprintf(str, "<< /Length %d >>\r\n", MessageBufPos);
			WriteToFile(fp, str);
			WriteToFile(fp, "stream\r\n");

			if (MessageBufPos > 0)
				WriteToFile(fp, (LPSTR) MessageBuf);
		}

#else
		sprintf(str, "<< /Length %d >>\r\n", MessageBufPos);
		WriteToFile(fp, str);
		WriteToFile(fp, "stream\r\n");

		if (MessageBufPos > 0)
			WriteToFile(fp, (LPSTR) MessageBuf);

#endif
		WriteToFile(fp, "endstream\r\n");
		WriteToFile(fp, "endobj\r\n");

		if ((Layer == 0) || (Layer == Design.NrBoardLayers - 1))
		{
			MessageBufPos = 0;
			StartPdfPos = FileCurrentPointer(fp);
			AnnotationsAdded = LayerPlotOutput2(Layer, 1);

			if ((AnnotationsAdded > 0) && (MessageBufPos > 0))
				WriteToFile(fp, (LPSTR) MessageBuf);
		}

		PageCount++;
	}

// ****************************************************************************************************
// Writing main contents (page object nrs)
	PageObjectNr = NrPDFObjects;
	AddPDFObject(1);
	PageObjectStartNr[PageCount] = NrPDFObjects;

	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Pages\r\n");
	sprintf(str, "/Count %d\r\n", NrPages);
	WriteToFile(fp, str);
	str[0] = 0;

	for (cnt = 0; cnt < NrPages; cnt++)
	{
		if (cnt == 0)
			sprintf(str2, "/Kids [%d 0 R", PageObjectNr + cnt + 2);
		else
			sprintf(str2, " %d 0 R", PageObjectNr + cnt + 2);

		strcat(str, str2);
	}

	strcat(str, "]\r\n");
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************
// Writing page info
	for (cnt3 = 0; cnt3 < NrGerberLayers; cnt3++)
	{
		Layer = GerberLayers[cnt3];
		AddPDFObject(1);
		WriteToFile(fp, "<<\r\n");
		WriteToFile(fp, "/Type /Page\r\n");
		sprintf(str, "/Parent %d 0 R\r\n", PageObjectNr + 1);
		WriteToFile(fp, str);
		sprintf(str, "/Resources << /Procset %d 0 R\r\n", PageObjectNr + NrPages * 2 + 5);
		WriteToFile(fp, str);
		sprintf(str, "              /Font << /F1 %d 0 R >>\r\n", PageObjectNr + NrPages * 2 + 6);
		WriteToFile(fp, str);
		WriteToFile(fp, "           >>\r\n");
		sprintf(str, "/MediaBox [0 0 %d %d]\r\n", PageSizeUnitsX, PageSizeUnitsY);
		WriteToFile(fp, str);
		sprintf(str, "/Contents %d 0 R\r\n", PageObjectStartNr[cnt3]);
		WriteToFile(fp, str);

// Writing annotation object refs
		res = PageObjectStartNr[cnt3 + 1] - PageObjectStartNr[cnt3] - 1;

		if (res > 0)
		{
			str[0] = 0;

			for (cnt2 = 0; cnt2 < res; cnt2++)
			{
				if (cnt2 == 0)
					strcat(str, "/Annots [ ");

				if (strlen(str) + 7 > 80)
				{
					strcat(str, "\r\n");
					WriteToFile(fp, str);
					str[0] = 0;
				}

				sprintf(str2, "%d 0 R ", PageObjectStartNr[cnt3] + cnt2 + 1);
				strcat(str, str2);
				/*
				/Annots [ 5 0 R 6 0 R 7 0 R 8 0 R 9 0 R 10 0 R 11 0 R 12 0 R 13 0 R 14 0 R
				15 0 R 16 0 R 17 0 R 18 0 R 19 0 R 20 0 R 21 0 R 22 0 R 23 0 R 24 0 R
				25 0 R 26 0 R 27 0 R 28 0 R 29 0 R 30 0 R 31 0 R 32 0 R 33 0 R 34 0 R
				35 0 R 36 0 R 37 0 R 38 0 R 39 0 R 40 0 R ]
				*/
			}

			strcat(str, "]\r\n");
			WriteToFile(fp, str);
		}

		WriteToFile(fp, ">>\r\n");
		WriteToFile(fp, "endobj\r\n");
	}

// ****************************************************************************************************
// Writing table of contents
	for (cnt = 0; cnt < NrPages; cnt++)
	{
		AddPDFObject(1);
		WriteToFile(fp, "<<\r\n");
		sprintf(str, "/Dest [%d 0 R /Fit]\r\n", PageObjectNr + cnt + 2);
		WriteToFile(fp, str);
		sprintf(str, "/Parent %d 0 R\r\n", PageObjectNr + NrPages * 2 + 2);
		WriteToFile(fp, str);
		sprintf(str, "/Title (%s)\r\n", PageTitles[cnt]);
		WriteToFile(fp, str);

		if (cnt > 0)
		{
			sprintf(str, "/Prev %d 0 R\r\n", PageObjectNr + NrPages + cnt + 1);
			WriteToFile(fp, str);
		}

		if ((NrPages > 1) && (cnt < NrPages - 1))
		{
			sprintf(str, "/Next %d 0 R\r\n", PageObjectNr + NrPages + cnt + 3);
			WriteToFile(fp, str);
		}

		WriteToFile(fp, ">>\r\n");
		WriteToFile(fp, "endobj\r\n");
	}

// ****************************************************************************************************

	AddPDFObject(1);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Outlines\r\n");
	sprintf(str, "/First %d 0 R\r\n", PageObjectNr + NrPages + 2);
	WriteToFile(fp, str);
	sprintf(str, "/Last %d 0 R\r\n", PageObjectNr + NrPages + 2 + NrPages - 1);
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************
	AddPDFObject(1);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Catalog\r\n");
	sprintf(str, "/Pages %d 0 R\r\n", PageObjectNr + 1);
	WriteToFile(fp, str);
	sprintf(str, "/Outlines %d 0 R\r\n", PageObjectNr + NrPages * 2 + 2);
	WriteToFile(fp, str);

	if (NrPages > 1)
		WriteToFile(fp, "/PageMode /UseOutlines\r\n");

	WriteToFile(fp, "/PageLayout /SinglePage\r\n");
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************
	AddPDFObject(1);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/CreationDate ");
	time(&ltime);
	today = localtime(&ltime);
	strftime(str, 100, "(D:%Y%m%d%H%M%S)\r\n", today);
	WriteToFile(fp, str);

	if (PDFCreatorOrganisation[0] != 0)
	{
		sprintf(str, "/Author (%s)\r\n", PDFCreatorOrganisation);
		WriteToFile(fp, str);
	}

	if (PDFCreatorName[0] != 0)
	{
		sprintf(str, "/Creator (%s)\r\n", PDFCreatorName);
		WriteToFile(fp, str);
	}

	if (PDFSubject[0] != 0)
	{
		sprintf(str, "/Subject (%s)\r\n", PDFSubject);
		WriteToFile(fp, str);
	}

	if (PDFTitle[0] != 0)
	{
		sprintf(str, "/Title (%s)\r\n", PDFTitle);
		WriteToFile(fp, str);
	}

	sprintf(str, "/Producer (PCB elegance %d.%d)\r\n", VER_VERSION / 100, VER_VERSION % 100);
	WriteToFile(fp, str);

	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");
// ****************************************************************************************************
	AddPDFObject(1);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "[ /PDF /Text ]\r\n");
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");
// ****************************************************************************************************
	AddPDFObject(1);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Font\r\n");
	WriteToFile(fp, "/Subtype /Type1\r\n");
	WriteToFile(fp, "/Name /F1\r\n");
	WriteToFile(fp, "/BaseFont /Courier\r\n");
	WriteToFile(fp, "/Encoding /MacRomanEncoding\r\n");
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");
// ****************************************************************************************************

	pos = FileCurrentPointer(fp);
	WriteToFile(fp, "xref\r\n");
	sprintf(str, "0 %d\r\n", NrPDFObjects + 1);
	WriteToFile(fp, str);
	WriteToFile(fp, "0000000000 65535 f \r\n");

	for (cnt = 0; cnt < NrPDFObjects; cnt++)
	{
		sprintf(str, "%010d 00000 n \r\n", PDFObjectPos[cnt]);
		WriteToFile(fp, str);
	}

	WriteToFile(fp, "trailer\r\n");
	WriteToFile(fp, "<<\r\n");
	sprintf(str, "/Size %d\r\n", NrPDFObjects + 1);
	WriteToFile(fp, str);
	sprintf(str, "/Root %d 0 R\r\n", PageObjectNr + NrPages * 2 + 3);
	WriteToFile(fp, str);
	sprintf(str, "/Info %d 0 R\r\n", PageObjectNr + NrPages * 2 + 4);
	WriteToFile(fp, str);

	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "startxref\r\n");
	sprintf(str, "%d\r\n", pos);
	WriteToFile(fp, str);
	WriteToFile(fp, "%%EOF\r\n");
	FileClose(fp);

// *******************************************************************************************************

	ViewMinX = PixelToRealOffX(-1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(-1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

	SetNormalCursor();
	DeAllocateMemMessageBuf();
	DeallocateSpecialMem(MEM_PDFBUF);

	strcpy(InfoStr, InfoCopy);
	RedrawInfoStr(1);
	DeAllocateMemTemp();
// StringCount
	sprintf(str, SC(429, "PDF output ready.\n\nOutput file is\n\n%s"), FileStr);
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


void CommandCalculateAreaPolygon(int32 mode)
{
	int32 OldDrawWindowMinX, OldDrawWindowMinY, OldDrawWindowMaxX, OldDrawWindowMaxY, FoundPolygonNr, PixelsX, PixelsY,
	      NrBytesSubBitmap, SubBitmapMemsize, cnt, count, x, ok;
	ObjectRecord Object4;
	char str[MAX_LENGTH_STRING];
	ObjectPolygonRecord *ObjectPolygon;
	HBITMAP ViewBitmap;
	BITMAPINFO *BitmapInfo;
	uint8 BitmapInfoMem[1024], value;
	BITMAPV4HEADER *BitmapHeader;
	int32 res;
	RECT Rect;
	double Factor2, divx, divy, hulpx, hulpy, area;

	FoundPolygonNr = -1;

	for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
	{
		ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

		if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
		{
			if (FoundPolygonNr == -1)
				FoundPolygonNr = cnt;
		}
	}

	if (FoundPolygonNr == -1)
		return;


	Factor2 = Factor;
	OldDrawWindowMinX = DrawWindowMinX;
	OldDrawWindowMinY = DrawWindowMinY;
	OldDrawWindowMaxX = DrawWindowMaxX;
	OldDrawWindowMaxY = DrawWindowMaxY;

//  DeleteGraphicObjects();
//  CreateDrawObjects(1);
	Printing = 0;

	memset(&BitmapInfoMem, 0, sizeof(BitmapInfoMem));
	BitmapInfo = (BITMAPINFO *) BitmapInfoMem;
	BitmapHeader = (BITMAPV4HEADER *) & BitmapInfo->bmiHeader;
	BitmapHeader->bV4Size = sizeof(BITMAPV4HEADER);
	BitmapHeader->bV4Planes = 1;
	BitmapHeader->bV4BitCount = 1;
	BitmapHeader->bV4V4Compression = BI_RGB;

	OutputDisplay = CreateCompatibleDC(0);

	PixelsX = GetDeviceCaps(OutputDisplay, HORZRES);
	PixelsY = GetDeviceCaps(OutputDisplay, VERTRES);
	BitmapHeader->bV4Width = PixelsX;
	BitmapHeader->bV4Height = PixelsY;
	DrawWindowMinX = 0;
	DrawWindowMinY = 0;
	DrawWindowMaxX = PixelsX;
	DrawWindowMaxY = PixelsY;
	SetBkMode(OutputDisplay, TRANSPARENT);
	SetROP2(OutputDisplay, R2_COPYPEN);

	NrBytesSubBitmap = PixelsX / 8;
	ViewBitmap = CreateCompatibleBitmap(OutputDisplay, PixelsX, PixelsY);
	SelectObject(OutputDisplay, ViewBitmap);

	SubBitmapMemsize = NrBytesSubBitmap * PixelsY;
	AllocateMemTemp(SubBitmapMemsize);

	ViewMinX = -1e9;
	ViewMinY = -1e9;
	ViewMaxX = 1e9;
	ViewMaxY = 1e9;

	memset(&Object4, 0, sizeof(Object4));
	Object4.ObjectType = OBJECT_POLYGON;
	Object4.TraceNr = FoundPolygonNr;
//  Object4.ObjectType=PIN_SMD_ROUND;
//  Object4.x1=1e7;
//  Object4.y1=1e7;
//  Object4.x2=1e6;
	FillPositionObject(&Object4);

	PrintOffsetX = Object4.minx;
	PrintOffsetY = Object4.miny;

	divx = Object4.maxx - Object4.minx;
	divy = Object4.maxy - Object4.miny;
	hulpx = divx / PixelsX;
	hulpy = divy / PixelsY;

	if (hulpx > hulpy)
	{
		hulpx = PixelsX;
		Factor = hulpx / divx;
	}
	else
	{
		hulpy = PixelsY;
		Factor = hulpy / divy;
	}

	InitDrawingObject(0, BACKGROUND_LAYER, 0, NORMAL_FILLED_AND_PEN1);
	Rect.left = 0;
	Rect.top = 0;
	Rect.right = PixelsX;
	Rect.bottom = PixelsY;
//  Rectangle(OutputDisplay,0,0,PixelsX,PixelsY);
	FillRect(OutputDisplay, &Rect, GetStockObject(WHITE_BRUSH));
	InitDrawingPlot(1);
	InitDrawingSolidBrushBlack();

	PlotObject(&Object4, 0);

	res = GetDIBits(OutputDisplay, ViewBitmap, 0, PixelsY, TempMem, BitmapInfo, DIB_RGB_COLORS);

	count = 0;

	for (cnt = 0; cnt < PixelsY; cnt++)
	{
		for (x = 0; x < NrBytesSubBitmap; x++)
		{
			value = TempMem[cnt * NrBytesSubBitmap + x];

			if ((value & 0x80) == 0)
				count++;

			if ((value & 0x40) == 0)
				count++;

			if ((value & 0x20) == 0)
				count++;

			if ((value & 0x10) == 0)
				count++;

			if ((value & 0x08) == 0)
				count++;

			if ((value & 0x04) == 0)
				count++;

			if ((value & 0x02) == 0)
				count++;

			if ((value & 0x01) == 0)
				count++;
		}
	}

	area = (SQR(1 / (Factor * 1e6)) * count);
	DeleteObject(ViewBitmap);
	DeleteDC(OutputDisplay);


	DeAllocateMemTemp();
	ok = 1;

	DeleteGraphicObjects();
	CreateDrawObjects(0);
	ReverseY = 1;

	OutputDisplay = NULL;
	Factor = Factor2;
	DrawWindowMinX = OldDrawWindowMinX;
	DrawWindowMinY = OldDrawWindowMinY;
	DrawWindowMaxX = OldDrawWindowMaxX;
	DrawWindowMaxY = OldDrawWindowMaxY;
	ViewMinX = PixelToRealOffX(-1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(-1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

	sprintf(str, SC(1025, "Area is %.5f cm² ( %.6f inch²)"), area, area / (SQR(2.54)));
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);

//  ReDisplay();
	return;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
