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
#include "line2.h"
#include "rect.h"
#include "geom.h"
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
#include "stdio.h"
#include "dialogs.h"
#include "time.h"
#include "utf8.h"

#define  MIN_PLOT_DRILL        0.5e5


int32 CurrentPlotThickNess, BrushType, PixelsPerInchX, PixelsPerInchY, RoundingNr, RoundingCnt, PolygonNr, ReverseX,
      Printing, Font90, Plotfp;

HPEN OldPen, PlotPen;
HBRUSH PlotBlackBrush, PlotWhiteBrush, NewEmptyBrush, OldBrush;
LOGBRUSH PlotBlackBrushObject, PlotWhiteBrushObject, NewEmptyBrushObject;

HDC OutputDisplayCopy;

double PrintOffsetX, PrintOffsetY, TempScale;

// *******************************************************************************************************
// *******************************************************************************************************

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

extern COLORREF LineColor;
extern HDC OutputDisplay;
extern double TextMinX, TextMinY, TextMaxX, TextMaxY;

//***********************************************************************************************************************************
//************************ IDD_DIALOG_PLOT_SCALE ************************************************************************************
//***********************************************************************************************************************************

int32 CALLBACK ScaleDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	float value;
	char str[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(407, "Scale"));
		sprintf(str, "%.4f", TempScale);
		SendDlgItemMessage(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) & str);
		SendDlgItemMessage(Dialog, IDOK, WM_SETTEXT, 0, (LPARAM) "OK");
		SendDlgItemMessage(Dialog, IDD_SCALE1X1, WM_SETTEXT, 0, (LPARAM) SC(408, "Scale 1X"));
		SendDlgItemMessage(Dialog, IDCANCEL, WM_SETTEXT, 0, (LPARAM) SC(46, "Cancel"));
		SendDlgItemMessage(Dialog, IDC_STATIC1, WM_SETTEXT, 0, (LPARAM) SC(38, "Scale factor"));
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
			if (SendDlgItemMessage(Dialog, IDC_EDIT1, WM_GETTEXT, 20, (LPARAM) str) > 0)
			{
				if ((sscanf(str, "%f", &value) != 1) || (value < 0.1) || (value > 100.0))
				{
					MessageBoxUTF8(GEOMWindow, SC(409, "Scale factor between 0.1 .. 10"), SC(48, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				TempScale = value;
				EndDialog(Dialog, 1);
			}
			else
				EndDialog(Dialog, 2);

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
	res = DialogBox(GEOMClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PLOT_SCALE), GEOMWindow, (DLGPROC) ScaleDialog2);

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

void Print(int32 mode)
{
	int32 res2, PageX, PageY, minx, maxx, miny, maxy, PagePixelsY, OldDrawWindowMinX, OldDrawWindowMinY,
	      OldDrawWindowMaxX, OldDrawWindowMaxY;
	double Xoffset2, Yoffset2, Factor2;
	int32 res;
	double MinX, MaxX, MinY, MaxY, Factor1x, Factor3, TempFactor, FactorX, FactorY;

	memset(&Pd, 0, sizeof(PRINTDLG));
	Pd.lStructSize = sizeof(PRINTDLG);
	Pd.hwndOwner = GEOMWindow;
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
		CreateDrawObjects();

		SetBkMode(OutputDisplay, TRANSPARENT);
		OutputDisplay = Pd.hDC;

		DrawObjects(0);

		InitDrawingColorGray();
		DrawLine(-1, DrawWindowMaxY - Mult(-Yoffset) - 1, 4000, DrawWindowMaxY - Mult(-Yoffset) - 1);
		DrawLine(Mult(-Xoffset) + DrawWindowMinX, -10, Mult(-Xoffset) + DrawWindowMinX, 4000);

		if (ViewInsertionPoint)
			DrawInsertionPoint(0);


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
		CreateDrawObjects();
		MessageBoxUTF8(GEOMWindow, SC(410, "Printing ready"), SC(4, "Message"), MB_APPLMODAL | MB_OK);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
