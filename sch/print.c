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
#include "locale.h"
#include "graphics.h"
#include "windows.h"
#include "memory.h"
#include "print.h"
#include "draw2.h"
#include "calcdef.h"
#include "calc.h"
#include "line2.h"
#include "rect.h"
#include "calc2.h"
#include "sch.h"
#include "files2.h"
#include "stdio.h"
#include "math.h"
#include "draw.h"
#include "print.h"
#include "ellipss.h"
#include "string.h"
#include "toets.h"
#include "mainloop.h"
#include "commdlg.h"
#include "dialogs.h"
#include "resource.h"
#include "time.h"
#include "utf8.h"
#include "property.h"
#include "../functionsc/version.h"


#define  PDFConvX(x)  (((((x)*254000.0)-PDFStartX)*PDFMultFactor+PDFPageOffsetX)*PDFMultFactor2)
#define  PDFConvY(y)  (((((y)*254000.0)-PDFStartY)*PDFMultFactor+PDFPageOffsetY)*PDFMultFactor2)
#define  PDFConv(x)   (((x)*254000.0)*PDFMultFactor*PDFMultFactor2)

#define NotInRange2(x1,x2)  ( ((((x1)>(x2)-0.01)     && ((x1)<(x2)+0.01)))     ? (0) : (1) )

#pragma pack(2)

typedef struct
{
	int16 Identifier;
	int32 FileSize;
	int32 Reserved1;
	int32 StartOfDataOffset;
	int32 BitmapHeaderSize;
	int32 Width;
	int32 Height;
	int16 NrOfPlanes;
	int16 BitsPerPixel;
	int32 CompressionType, BitmapDataSize, HResolutionInPixelsPerMeter, VResolutionInPixelsPerMeter, NrColors1,
	      NrImportedColors;
} BmpHeaderRecord;


typedef struct
{
	int32 PaperSize;
	int32 PaperOrientation;
	int32 PaperFitToPage;
	int32 Color;
	int32 Invert;
} PDFInfoRecord;


#define MultY2(Nr) (Mult(Nr-Yoffset))


PDFInfoRecord PDFInfo = {
	PAPERSIZE_A4,
	ORIENTATION_AUTO,
	1,
	0
};
PDFInfoRecord PrintInfo = {
	PAPERSIZE_A4,
	ORIENTATION_AUTO,
	1,
	0
};


double BitmapExportResolution = (double) (2540000.0 / 600.0);

int32 NrPageOutputIds = 11;
int32 PageOutputIds[11] = { PAPERSIZE_A1,
                            PAPERSIZE_A2,
                            PAPERSIZE_A3,
                            PAPERSIZE_A4,
                            PAPERSIZE_A5,
                            PAPERSIZE_B4,
                            PAPERSIZE_B5,
                            PAPERSIZE_B4_JIS,
                            PAPERSIZE_B5_JIS,
                            PAPERSIZE_LEGAL,
                            PAPERSIZE_LETTER
                          };

int32 DialogMode, ok, pos, PDFReverse, PDFCurrentColor, Font90;

double PDFMultFactor, PDFMultFactor2, PDFPageOffsetX, PDFPageOffsetY, PDFStartX, PDFStartY, PDFNewLineWidth,
       PDFLineWidth, PDFDefaultLineWidth;

char PDFCreatorOrganisation[MAX_LENGTH_STRING];
char PDFCreatorName[MAX_LENGTH_STRING];

char PDFSubject[MAX_LENGTH_STRING];
char PDFTitle[MAX_LENGTH_STRING];

extern COLORREF LineColor;
extern int32 PrintingThickness, TempUnits;
extern int32 DirectPrinting;
extern ProjectInfoRecord *ProjectInfo;

// ***************************************************************************************


//LPSTR                       Font1Str="Arial";
LPSTR Font1Str = "Courier New";
//LPSTR                       Font1Str="Timer New Roman";
//LPSTR                       Font1Str="Century Gothic";

//LPSTR                       Font2Str="Arial";
//LPSTR                       Font2Str="Courier New";
//LPSTR                       Font2Str="MS Serif";

TEXTMETRIC Font1Metrics, Font2Metrics;
HFONT PrinterFont, PrinterFont_90;
HFONT PrinterFont2, PrinterFont2_90;
HFONT PrinterFont3, PrinterFont3_90;


int32 BitmapExportResolutions[20] = {
	300, 400, 508, 600, 1000, 1016, 1200, 2000, 2400, 4000, 4800
};
int32 NrBitmapExportResolutions = 11;

// ********************************************************************************************************
// ********************************************************************************************************

PRINTDLG Pd;
DOCINFO PdDocInfo;
char PrintFileName[MAX_LENGTH_STRING];
char PrinterName[MAX_LENGTH_STRING] = "";
int32 PrintOptions = -1;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 PrinterDialog(int32 DefaultPaperSize, int32 * NewPaperSize, int32 PaperOrientation, LPSTR PrinterName, int32 mode)
{
	HGLOBAL DeviceModeGlobal;
	DEVMODE *DeviceMode, *DeviceMode2;
	int32 SystemPaperSize, res;

	res = 0;
	SystemPaperSize = 0;

	if ((mode & 1) == 1)
	{	// Force paper settings from the function parameters
		switch (*NewPaperSize)
		{
		case PAPERSIZE_A3:
			SystemPaperSize = DMPAPER_A3;
			break;

		case PAPERSIZE_A4:
			SystemPaperSize = DMPAPER_A4;
			break;

		case PAPERSIZE_A5:
			SystemPaperSize = DMPAPER_A5;
			break;

		case PAPERSIZE_B4:
			SystemPaperSize = DMPAPER_B4;
			break;

		case PAPERSIZE_B5:
			SystemPaperSize = DMPAPER_B5;
			break;

		case PAPERSIZE_LEGAL:
			SystemPaperSize = DMPAPER_LEGAL;
			break;

		case PAPERSIZE_LETTER:
			SystemPaperSize = DMPAPER_LETTER;
			break;

		default:
			SystemPaperSize = DMPAPER_A4;
			break;
		}
	}
	else
	{
		if (DefaultPaperSize != -1)
			SystemPaperSize = DefaultPaperSize;
	}

	DeviceModeGlobal = GlobalAlloc(GHND, sizeof(DEVMODE));
	DeviceMode = GlobalLock(DeviceModeGlobal);
	memset(DeviceMode, 0, sizeof(DEVMODE));
	DeviceMode->dmSize = sizeof(DEVMODE);
	DeviceMode->dmSpecVersion = DM_SPECVERSION;

	if (PrinterName[0] != 0)
		strcpy(DeviceMode->dmDeviceName, PrinterName);

	DeviceMode->dmFields = DM_ORIENTATION;

	if (PaperOrientation == ORIENTATION_LANDSCAPE)
		DeviceMode->dmOrientation = DMORIENT_LANDSCAPE;
	else
		DeviceMode->dmOrientation = DMORIENT_PORTRAIT;

	if (SystemPaperSize != -1)
	{
		DeviceMode->dmPaperSize = (int16) SystemPaperSize;
		DeviceMode->dmFields |= DM_PAPERSIZE;
	}

	GlobalUnlock(DeviceModeGlobal);

	memset(&Pd, 0, sizeof(PRINTDLG));
	Pd.lStructSize = sizeof(PRINTDLG);
	Pd.hwndOwner = SCHWindow;
	Pd.Flags = PD_RETURNDC;

	if ((mode & 2) == 2)
		Pd.Flags |= PD_RETURNDEFAULT;
	else
		Pd.hDevMode = DeviceModeGlobal;

	if (PrintDlg(&Pd) != 0)
	{
//  if (PrinterDialog(&Pd,0)) {
// ********************************************************************************************************

		DeviceMode2 = GlobalLock(Pd.hDevMode);
		strcpy(PrinterName, DeviceMode2->dmDeviceName);
		*NewPaperSize = DeviceMode2->dmPaperSize;
		res = 1;
	}

	if ((mode & 2) == 0)
	{
		GlobalUnlock(DeviceModeGlobal);
		GlobalFree(DeviceModeGlobal);
	}

	return res;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void Print(int32 PaperSize, int32 Color, LPSTR CurrentPrinterName, int32 mode)
{

#define     PRINT_TOLERANCE             0.06

	char PrinterFontText[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING],
	     NewPrinterName[MAX_LENGTH_STRING];

	int32 res;

	TEXTMETRIC PrinterFontMetrics;
	int32 h, h2, FontSize, x1, y1, x2, y2, CharSize, hoek, PixelsPerInch, OldDrawWindowMinX, OldDrawWindowMinY,
	      NewPaperSize, NewOrientation, OldDrawWindowMaxX, OldDrawWindowMaxY, res2, ok, fp;
	double MinX, MaxX, MinY, MaxY, FactorX, FactorY, Width, Height, PrintPageX, PrintPageY, PageX2, PageY2,
	       ExtraPageOffsetX, ExtraPageOffsetY, PageOffsetX, PageOffsetX2, PageOffsetY, PrintWidth, PrintHeight, DivX, DivY,
	       px1, px2, px3, px4, py1, py2, py3, py4, PixelSize, Xoffset2, Yoffset2, Factor2;

	OldDrawWindowMinX = DrawWindowMinX;
	OldDrawWindowMinY = DrawWindowMinY;
	OldDrawWindowMaxX = DrawWindowMaxX;
	OldDrawWindowMaxY = DrawWindowMaxY;

	if (ProjectInfo != NULL)
		ProjectInfo->PrintingError = 0;

	if (!EditingSymbol)
		SetBoardPosInstances();

	FindMinMaxDesign(&MinX, &MinY, &MaxX, &MaxY);
	Width = MaxX - MinX;
	Height = MaxY - MinY;

	PdDocInfo.cbSize = sizeof(DOCINFO);
	GetFilePartFromFileName(PrintFileName, EditFile);
	CutExtensionFileName(PrintFileName);
	PdDocInfo.lpszDocName = PrintFileName;

	NewPaperSize = PaperSize;

	if (Width > Height)
		NewOrientation = ORIENTATION_LANDSCAPE;
	else
		NewOrientation = ORIENTATION_PORTRAIT;

	strcpy(NewPrinterName, CurrentPrinterName);

	if (PrinterDialog(PaperSize, &NewPaperSize, NewOrientation, NewPrinterName, mode) == 1)
	{

		PrintOptions = NewPaperSize;
		strcpy(PrinterName, NewPrinterName);

		if (ProjectInfo != NULL)
		{
			strcpy(ProjectInfo->PrinterName, PrinterName);
			ProjectInfo->PaperSize = NewPaperSize;
		}

//    res2=SetMapMode(Pd->hDC,MM_TEXT);   // 0.1 mm

//    res2=GetMapMode(Pd->hDC);
//    res2=SetMapMode(Pd->hDC,MM_LOMETRIC);   // 0.1 mm

//    Beep(1000,200);
//    sprintf(str,"%08X",Pd.hDC);
//    MessageBoxUTF8(NULL,str,"Pd.hdc",MB_APPLMODAL|MB_OK);

		res2 = StartDoc(Pd.hDC, &PdDocInfo);
//    Beep(2000,200);
		res2 = StartPage(Pd.hDC);

		if (ProjectInfo != NULL)
		{
			ProjectInfo->PrintingError = 0;
			ProjectInfo->PrintingBusy = 2;
		}

		if (!DirectPrinting)
		{
			sprintf(InfoStr, "Start printing");
			RedrawInfoStr(1);

			SetWaitCursor();
		}

		Printing = 1;

		OutputDisplay = Pd.hDC;
		PixelsPerInch = GetDeviceCaps(Pd.hDC, LOGPIXELSY);
		PrintPageX = (double) GetDeviceCaps(Pd.hDC, HORZSIZE);
		PrintPageY = (double) GetDeviceCaps(Pd.hDC, VERTSIZE);
		PageX2 = (double) GetDeviceCaps(Pd.hDC, PHYSICALWIDTH);
		PageY2 = (double) GetDeviceCaps(Pd.hDC, PHYSICALHEIGHT);
		PageOffsetX = (double) GetDeviceCaps(Pd.hDC, PHYSICALOFFSETX);
		PageOffsetY = (double) GetDeviceCaps(Pd.hDC, PHYSICALOFFSETY);


		ViewMinX = (double) -1e9;
		ViewMaxX = (double) 1e9;
		ViewMinY = (double) -1e9;
		ViewMaxY = (double) 1e9;

		Xoffset2 = Xoffset;
		Yoffset2 = Yoffset;
		Factor2 = Factor;

		if (Color == 0)
		{
			PrintingThickness = 1;

			if (PixelsPerInch > 500)
				PrintingThickness = 2;

			if (PixelsPerInch > 1000)
				PrintingThickness = 4;
		}
		else
		{
			PrintingThickness = 2;

			if (PixelsPerInch > 500)
				PrintingThickness = 3;

			if (PixelsPerInch > 1000)
				PrintingThickness = 6;
		}

		DeleteGraphicObjects();
		CreateDrawObjects(Color);

//    Factor=(double)25.4;
//    Factor=(double)16.5;
		PixelSize = (double) (25.4 / (double) PixelsPerInch);
		PrintPageX /= PixelSize;
		PrintPageY /= PixelSize;

		if (PrintPageX > PrintPageY)
			PageOffsetX2 = PageOffsetX - (100 * PixelsPerInch) / 600;
		else
			PageOffsetX2 = PageOffsetX;

		px1 = 0;
		px2 = PageOffsetX;
//    px3=PrintPageX;
		px3 = PageOffsetX + PrintPageX;
		px4 = PageX2;
		py1 = 0;
		py2 = PageOffsetY;
//    py3=PrintPageY-300;
		py3 = PageOffsetY + PrintPageY;
		py4 = PageY2;

//    py4=py3;
		if (px4 - px3 > px2)
		{
			ExtraPageOffsetX = (px4 - px3 - px2);
			PrintWidth = px3 - ExtraPageOffsetX;
		}
		else
		{
			ExtraPageOffsetX = px2;
			PrintWidth = px4 - px2 * 2;
		}

		if (PrintPageX > PrintPageY)
			ExtraPageOffsetX -= (75 * PixelsPerInch) / 600;

		if (py4 - py3 > py2)
		{
			ExtraPageOffsetY = (py4 - py3 - py2);
			PrintHeight = py3 - ExtraPageOffsetY;
		}
		else
		{
			ExtraPageOffsetY = py2;
			PrintHeight = py4 - py2 * 2;
		}

		DrawWindowMinX = 0;
		DrawWindowMinY = -100000;
		DrawWindowMaxX = 100000;
		DrawWindowMaxY = (int32) (py3);
		FactorX = (double) (PrintWidth * (1 - PRINT_TOLERANCE) / Width);
		FactorY = (double) (PrintHeight * (1 - PRINT_TOLERANCE) / Height);
		Factor = min(FactorX, FactorY);
		DivY = (double) Mult(Height);
		DivX = (double) Mult(Width);

		if (FactorX < FactorY)
		{
			/* Print in landscape
			   Center the Y-axis   */
			Xoffset = (double) (-PixelToReal((int32) ExtraPageOffsetX) + MinX - Width * PRINT_TOLERANCE * 0.5);
			Yoffset = (double) (-PixelToReal((int32) (ExtraPageOffsetY + ((PrintHeight - DivY) / 2))) + MinY);
		}
		else
		{
			/* Print in portrait
			   Center the X-axis   */
			Xoffset = (double) (-PixelToReal((int32) (ExtraPageOffsetX + (PrintWidth - DivX) / 2)) + MinX);
			Yoffset = (double) (-PixelToReal((int32) ExtraPageOffsetY) + MinY - Height * PRINT_TOLERANCE * 0.5);
		}

		x1 = MultX(MinX);
		y1 = MultY(MinY);
		x2 = MultX(MaxX);
		y2 = MultY(MaxY);

//    if (CheckSpecialVersion(0)) {
		if (0)
		{
			str2[0] = 0;
			sprintf(str, SC(410, "Page maxx,maxy  = %d,%d\r\n"), (int32) PageX2, (int32) PageY2);
			strcat(str2, str);
			sprintf(str, SC(411, "Page offsetx,offsety maxx,maxy  = %d,%d  %d,%d\r\n"), (int32) PageOffsetX,
			        (int32) PageOffsetY, (int32) PrintPageX, (int32) PrintPageY);
			strcat(str2, str);
			sprintf(str, SC(412, "Designs print minx,miny maxx,maxy  = %d,%d  %d,%d\r\n"), x1, y1, x2, y2);
			strcat(str2, str);

			if ((fp = FileOpenWriteUTF8("c:\\sch\\PrintSize.txt")) > 0)
			{
				FileWrite(fp, str2, strlen(str2), &ok);
				FileClose(fp);
			}
		}

//    CreateObjectColorsPrint(0);

		hoek = 90;
		CharSize = Mult(10.0);
		FontSize = (CharSize * 12) / 100 + 1;
		PrinterFont =
		    CreateFont(FontSize, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
		               FIXED_PITCH, Font1Str);
		PrinterFont_90 =
		    CreateFont(FontSize, 0, hoek * 10, hoek * 10, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
		               PROOF_QUALITY, FIXED_PITCH, Font1Str);
		PrinterFont2 =
		    CreateFont(FontSize * 2, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
		               PROOF_QUALITY, FIXED_PITCH, Font1Str);
		PrinterFont2_90 =
		    CreateFont(FontSize * 2, 0, hoek * 10, hoek * 10, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS,
		               CLIP_DEFAULT_PRECIS, PROOF_QUALITY, FIXED_PITCH, Font1Str);
		PrinterFont3 =
		    CreateFont(FontSize * 3, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
		               PROOF_QUALITY, FIXED_PITCH, Font1Str);
		PrinterFont3_90 =
		    CreateFont(FontSize * 3, 0, hoek * 10, hoek * 10, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS,
		               CLIP_DEFAULT_PRECIS, PROOF_QUALITY, FIXED_PITCH, Font1Str);

		Font90 = 0;
		SetBkMode(OutputDisplay, TRANSPARENT);

		/*
		    TestPen = CreatePen(PS_SOLID,10,RGB(0,0,0));
		    SelectObject(OutputDisplay,TestPen);
		    res=MoveToEx(OutputDisplay,1000,1000,NULL);
		    res=LineTo(OutputDisplay,2000,2000);
		*/
//    Beep(4000,200);
		SelectObject(OutputDisplay, PrinterFont);
		DrawInstances(0);

		if (!EditingSymbol)
		{
			DrawWires(0);
			DrawBusses(0);
			DrawJunctions(0);
			DrawOnePinNets(0);
			DrawBusConnections(0);
			DrawGlobalConnections(0);
			DrawNetLabels(0);
		}
		else
		{
			DrawPins(0);
			DrawPowerPins(0);
			DrawPinBusses(0);
		}

		DrawObjectLines(0);
		DrawObjectRects(0);
		DrawObjectCircles(0);
		DrawObjectArcs(0);


		GetTextFace(OutputDisplay, 80, PrinterFontText);
		SetTextColor(OutputDisplay, RGB(0, 0, 0));
		GetTextMetrics(Pd.hDC, &PrinterFontMetrics);
		h = PrinterFontMetrics.tmDescent;
		h2 = PrinterFontMetrics.tmAscent;

		DrawObjectTexts(0);

		DeleteObject(PrinterFont);
		DeleteObject(PrinterFont_90);
		DeleteObject(PrinterFont2);
		DeleteObject(PrinterFont2_90);
		DeleteObject(PrinterFont3);
		DeleteObject(PrinterFont3_90);

		Printing = 0;
		PrintingThickness = 1;

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

		if (!DirectPrinting)
		{
			CreateDrawObjects(0);
			CheckZoomFactor();
			ReverseY = 1;
			SetNormalCursor();

			sprintf(InfoStr, "Printing done");
			RedrawInfoStr(1);
		}

		if (ProjectInfo != NULL)
			ProjectInfo->PrintingBusy = 0;

// ********************************************************************************************************
// ********************************************************************************************************

	}
	else
	{
		if (ProjectInfo != NULL)
			ProjectInfo->PrintingError = 1;

		res = CommDlgExtendedError();

//    res2=CDERR_CHOOSECOLORCODES;
		if (res == CDERR_DIALOGFAILURE)
			ok = 1;

		if (res == CDERR_FINDRESFAILURE)
			ok = 1;

		if (res == CDERR_GENERALCODES)
			ok = 1;

		if (res == CDERR_INITIALIZATION)
			ok = 1;

		if (res == CDERR_LOADRESFAILURE)
			ok = 1;

		if (res == CDERR_LOADSTRFAILURE)
			ok = 1;

		if (res == CDERR_LOCKRESFAILURE)
			ok = 1;

		if (res == CDERR_MEMALLOCFAILURE)
			ok = 1;

		if (res == CDERR_MEMLOCKFAILURE)
			ok = 1;

		if (res == CDERR_NOHINSTANCE)
			ok = 1;

		if (res == CDERR_NOHOOK)
			ok = 1;

		if (res == CDERR_NOTEMPLATE)
			ok = 1;

		if (res == CDERR_REGISTERMSGFAIL)
			ok = 1;

		if (res == CDERR_STRUCTSIZE)
			ok = 1;

		if (res == CFERR_CHOOSEFONTCODES)
			ok = 1;

		if (res == CFERR_MAXLESSTHANMIN)
			ok = 1;

		if (res == CFERR_NOFONTS)
			ok = 1;

		if (res == FNERR_BUFFERTOOSMALL)
			ok = 1;

		if (res == FNERR_FILENAMECODES)
			ok = 1;

		if (res == FNERR_INVALIDFILENAME)
			ok = 1;

		if (res == FNERR_SUBCLASSFAILURE)
			ok = 1;

		if (res == FRERR_BUFFERLENGTHZERO)
			ok = 1;

		if (res == FRERR_FINDREPLACECODES)
			ok = 1;

		if (res == PDERR_CREATEICFAILURE)
			ok = 1;

		if (res == PDERR_DEFAULTDIFFERENT)
			ok = 1;

		if (res == PDERR_DNDMMISMATCH)
			ok = 1;

		if (res == PDERR_GETDEVMODEFAIL)
			ok = 1;

		if (res == PDERR_INITFAILURE)
			ok = 1;

		if (res == PDERR_LOADDRVFAILURE)
			ok = 1;

		if (res == PDERR_NODEFAULTPRN)
			ok = 1;

		if (res == PDERR_NODEVICES)
			ok = 1;

		if (res == PDERR_PARSEFAILURE)
			ok = 1;

		if (res == PDERR_PRINTERCODES)
			ok = 1;

		if (res == PDERR_PRINTERNOTFOUND)
			ok = 1;

		if (res == PDERR_RETDEFFAILURE)
			ok = 1;

		if (res == PDERR_SETUPFAILURE)
			ok = 1;
	}
}

//****************************************************************************************************************
//************************** IDD_DIALOG_BITMAP_OUTPUT ************************************************************
//****************************************************************************************************************

int32 CALLBACK ExportBitmapDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
#define    InRange20(x1,x2) ( (((x1>x2-1.0) && (x1<x2+1.0))) ? (1) : (0) )

	int32 about;
	int32 cnt, res, ok, Found;
	char str[MAX_LENGTH_STRING];
	double value1;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(382, "Export to bitmap"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(426, "Resolution"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(431, "User"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, "thou/mm/µm");

		TempUnits = 1;
		TempUnits = Units;
		Found = -1;

		for (cnt = 0; cnt < NrBitmapExportResolutions; cnt++)
		{
			if (InRange20(BitmapExportResolution, 2540000.0 / (double) BitmapExportResolutions[cnt]))
				Found = cnt;

			sprintf(str, "%d dpi", BitmapExportResolutions[cnt]);
			SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) str);
		}

		if (Found != -1)
		{
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, (WPARAM) Found, 0);
			SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
			SendDlgItemMessage(Dialog, IDC_EDIT7, EM_SETREADONLY, 1, 0);
		}
		else
		{
			SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);

			if (TempUnits == 0)
			{
				SetDialogFloatValue(Dialog, IDC_EDIT2, (double) (BitmapExportResolution / 2540.0), 4);
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
			}
			else
			{
				SetDialogFloatValue(Dialog, IDC_EDIT2, (double) (BitmapExportResolution / 100000.0), 6);
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
			}
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			if (SendDlgItemMessage(Dialog, IDC_RADIO3, BM_GETCHECK, 0, 0) == 1)
			{
				GetDialogFloatValue(Dialog, IDC_EDIT2, &value1);

				switch (TempUnits)
				{
				case 0:
					value1 *= 2540.0;
					break;

				case 1:
					value1 *= 100000.0;
					break;

				case 2:
					value1 *= 100.0;
					break;
				}

				if ((value1 < (double) 1.0) || (value1 > (double) 100000.0))
				{
					MessageBoxUTF8(SCHWindow, SC(421, "Wrong dot size"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
					return about;
				}

				BitmapExportResolution = value1;
			}
			else
			{
				res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

				if (res == -1)
				{
					MessageBoxUTF8(SCHWindow, SC(422, "A value should be selected"), SC(38, "Error"),
					               MB_APPLMODAL | MB_OK);
					return about;
				}

				BitmapExportResolution = (double) (2540000.0 / BitmapExportResolutions[res]);
			}

			EndDialog(Dialog, 1);
			return about;

		case IDC_RADIO2:
			SendDlgItemMessage(Dialog, IDC_EDIT2, EM_SETREADONLY, 1, 0);
			SendDlgItemMessage(Dialog, IDC_EDIT7, EM_SETREADONLY, 1, 0);
			SendDlgItemMessage(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
			SendDlgItemMessage(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) (LPSTR) "");
			ok = 1;
			break;

		case IDC_RADIO3:
			SendDlgItemMessage(Dialog, IDC_RADIO2, BM_SETCHECK, 0, 0);
			SendDlgItemMessage(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
			SendDlgItemMessage(Dialog, IDC_EDIT2, EM_SETREADONLY, 0, 0);
			SendDlgItemMessage(Dialog, IDC_EDIT7, EM_SETREADONLY, 0, 0);
			SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, (WPARAM) - 1, 0);
			value1 = BitmapExportResolution;

			switch (TempUnits)
			{
			case 0:
				value1 /= 2540.0;
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 4);
				break;

			case 1:
				value1 /= 100000.0;
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 6);
				break;

			case 2:
				value1 /= 100.0;
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "µm");
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 3);
				break;
			}

			ok = 1;
			break;

		case IDD_UNITS:
			GetDialogFloatValue(Dialog, IDC_EDIT2, &value1);

			switch (TempUnits)
			{
			case 0:
				value1 *= 2540.0;
				break;

			case 1:
				value1 *= 100000.0;
				break;

			case 2:
				value1 *= 100.0;
				break;
			}

			TempUnits = (TempUnits + 1) % 3;

			switch (TempUnits)
			{
			case 0:
				value1 /= 2540.0;
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "thou");
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 4);
				break;

			case 1:
				value1 /= 100000.0;
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "mm");
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 6);
				break;

			case 2:
				value1 /= 100.0;
				SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_SETTEXT, 0, (LPARAM) (LPSTR) "µm");
				SetDialogFloatValue(Dialog, IDC_EDIT2, value1, 3);
				break;
			}

			break;

		case IDHELP:
//          Help("Plot_output_to_printer,0);
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

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ExportToBitmap(int32 mode)
{
#define BIT_ACTIVE(LineBytes,x) (((LineBytes[((x) >> 3)] & ((0x80 >> ((x) & 7)))) != 0) ? 1 : 0)

	HBITMAP ViewBitmap;
	BITMAPINFO *BitmapInfo;
	uint8 BitmapInfoMem[1024];
	char PrinterFontText[MAX_LENGTH_STRING], DirPart[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING],
	     Name[MAX_LENGTH_STRING];
	BITMAPV4HEADER *BitmapHeader;
	double Xoffset2, Yoffset2, Factor2, Xoffsets[16], Yoffsets[16], MinX, MaxX, MinY, MaxY, ScreenX, ScreenY;
	int32 fp, result, PixelsX, PixelsY, cnt, cnt2, cnt3, cnt4, cntx, cnty, hoek, res, CharSize, FontSize,
	      NrBitmapColors, OldDrawWindowMinX, OldDrawWindowMinY, OldDrawWindowMaxX, OldDrawWindowMaxY, NrBytesSubBitmap,
	      SectionsX, SectionsY, MemPos, MaxScreenX, MaxScreenY, NrBytesBitmapData, LinesToMove, LineBytesToMove,
	      NrCodedBytes, ColorIndex, ColorPixelCount, SubBitmapMemsize, BitmapMemsize;

	uint8 CodedLine[4096];
	uint8 *LineBytes;
	uint32 CurrentColor, TempColor;

	BmpHeaderRecord BmpHeader;
	uint8 TwoColors[8] = { 0, 0, 0, 0, 255, 255, 255, 0 };

	LinesToMove = 0;

	res =
	    DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_BITMAP_OUTPUT), SCHWindow,
	              (DLGPROC) ExportBitmapDialogBody);

	if (res == 2)
		return;

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

	NrBytesSubBitmap = PixelsX / 8;
	ViewBitmap = CreateCompatibleBitmap(OutputDisplay, PixelsX, PixelsY);
	SelectObject(OutputDisplay, ViewBitmap);

// ****************************************************************************************************

	FindMinMaxDesign(&MinX, &MinY, &MaxX, &MaxY);

	OldDrawWindowMinX = DrawWindowMinX;
	OldDrawWindowMinY = DrawWindowMinY;
	OldDrawWindowMaxX = DrawWindowMaxX;
	OldDrawWindowMaxY = DrawWindowMaxY;
	Xoffset2 = Xoffset;
	Yoffset2 = Yoffset;
	Factor2 = Factor;



	DeleteGraphicObjects();
	PrintingThickness = 1;

	if (2540000.0 / BitmapExportResolution > 200.0)
		PrintingThickness = 2;

	if (2540000.0 / BitmapExportResolution > 500.0)
		PrintingThickness = 4;

	if (2540000.0 / BitmapExportResolution > 1000.0)
		PrintingThickness = 7;

	CreateDrawObjects(2);

	SetWaitCursor();
	SetBkMode(OutputDisplay, TRANSPARENT);
	SetROP2(OutputDisplay, R2_COPYPEN);
	hoek = 90;
	Printing = 1;

	DrawWindowMinX = 0;
	DrawWindowMinY = 0;
	DrawWindowMaxX = PixelsX;
	DrawWindowMaxY = PixelsY;
	NrBytesBitmapData = 0;
	Factor = (double) ((2540000.0 / BitmapExportResolution) / 10.0);
	MaxScreenX = Mult((MaxX - MinX) * 1.1);
	MaxScreenX = ((MaxScreenX + 31) / 32) * 32;
	MaxScreenY = Mult((MaxY - MinY) * 1.1);
	Xoffsets[0] = (MinX - ((MaxX - MinX) * (double) 0.05));
	Yoffsets[0] = (MinY - ((MaxY - MinY) * (double) 0.05));
	SectionsX = (MaxScreenX + PixelsX - 1) / PixelsX;
	SectionsY = (MaxScreenY + PixelsY - 1) / PixelsY;
	ScreenX = PixelToReal(PixelsX);
	ScreenY = PixelToReal(PixelsY);

	for (cnty = 1; cnty < SectionsY; cnty++)
		Yoffsets[cnty] = Yoffsets[0] + (double) cnty *ScreenY;

	for (cntx = 1; cntx < SectionsX; cntx++)
		Xoffsets[cntx] = Xoffsets[0] + (double) cntx *ScreenX;

	SubBitmapMemsize = NrBytesSubBitmap * PixelsY;
	AllocateMemTemp(SubBitmapMemsize);
	BitmapMemsize = (MaxScreenX / 8) * MaxScreenY;
	AllocateMemTemp2(SubBitmapMemsize * SectionsX);

// ****************************************************************************************************

	CharSize = Mult(10.0);
	FontSize = (CharSize * 12) / 100 + 1;
	PrinterFont =
	    CreateFont(FontSize, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
	               FIXED_PITCH, Font1Str);
	PrinterFont_90 =
	    CreateFont(FontSize, 0, hoek * 10, hoek * 10, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
	               PROOF_QUALITY, FIXED_PITCH, Font1Str);
	PrinterFont2 =
	    CreateFont(FontSize * 2, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
	               FIXED_PITCH, Font1Str);
	PrinterFont2_90 =
	    CreateFont(FontSize * 2, 0, hoek * 10, hoek * 10, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
	               PROOF_QUALITY, FIXED_PITCH, Font1Str);
	PrinterFont3 =
	    CreateFont(FontSize * 3, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
	               FIXED_PITCH, Font1Str);
	PrinterFont3_90 =
	    CreateFont(FontSize * 3, 0, hoek * 10, hoek * 10, 0, 0, 0, 0, ANSI_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
	               PROOF_QUALITY, FIXED_PITCH, Font1Str);

	Font90 = 0;


	if (GetDirFromFileName(DirPart, EditFile) != 0)
		return;

	GetNameWithOutExtensionFromFileName(EditFile, Name);
	sprintf(FileName, "%s\\%s.bmp", DirPart, Name);
	fp = FileOpenWriteUTF8(FileName);

	if (fp <= 0)
	{
		MessageBoxUTF8(SCHWindow, FileName, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
		return;
	}

	memset(&BmpHeader, 0, sizeof(BmpHeader));

	if ((mode & 1) == 0)
		NrBitmapColors = 2;
	else
		NrBitmapColors = 256;

	BmpHeader.Identifier = 0x4d42;
	BmpHeader.StartOfDataOffset = 62;
	BmpHeader.BitmapHeaderSize = 40;
	BmpHeader.Width = MaxScreenX;
	BmpHeader.Height = MaxScreenY;

	if ((mode & 1) == 0)
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
	BmpHeader.NrImportedColors = 2;

	BmpHeader.HResolutionInPixelsPerMeter = (int32) ((double) 1e8 / BitmapExportResolution);
	BmpHeader.VResolutionInPixelsPerMeter = (int32) ((double) 1e8 / BitmapExportResolution);


	FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
	FileWrite(fp, &TwoColors, 8, &result);

	if ((mode & 1) == 0)
	{
		for (cnt = 2; cnt < 256; cnt++)
			FileWrite(fp, &TwoColors, 4, &result);
	}


// ****************************************************************************************************

	for (cnty = 0; cnty < SectionsY; cnty++)
	{
		Yoffset = Yoffsets[cnty];

		for (cntx = 0; cntx < SectionsX; cntx++)
		{
			Xoffset = Xoffsets[cntx];
			ViewMinX = (double) (Xoffset - 1.0);
			ViewMaxX = (double) (ViewMinX + ScreenX + 1.0);
			ViewMinY = (double) (Yoffset - 1.0);
			ViewMaxY = (double) (ViewMinY + ScreenY + 1.0);
			InitDrawingBackGround(0, 0);
			Rectangle(OutputDisplay, 0, 0, PixelsX, PixelsY);

			SelectObject(OutputDisplay, PrinterFont);
			DrawInstances(0);

			if (!EditingSymbol)
			{
				DrawWires(0);
				DrawBusses(0);
				DrawJunctions(0);
				DrawOnePinNets(0);
				DrawBusConnections(0);
				DrawGlobalConnections(0);
				DrawNetLabels(0);
			}
			else
			{
				DrawPins(0);
				DrawPowerPins(0);
				DrawPinBusses(0);
			}

			DrawObjectLines(0);
			DrawObjectRects(0);
			DrawObjectCircles(0);
			DrawObjectArcs(0);


			GetTextFace(OutputDisplay, 80, PrinterFontText);
			SetTextColor(OutputDisplay, RGB(0, 0, 0));

			DrawObjectTexts(0);


// *************************************************************************************

			res = GetDIBits(OutputDisplay, ViewBitmap, 0, PixelsY, TempMem, BitmapInfo, DIB_RGB_COLORS);

			if (cnty < SectionsY - 1)
				LinesToMove = PixelsY;
			else
				LinesToMove = MaxScreenY % PixelsY;

			if (cntx < SectionsX - 1)
				LineBytesToMove = NrBytesSubBitmap;
			else
				LineBytesToMove = (MaxScreenX / 8) % NrBytesSubBitmap;

			for (cnt = 0; cnt < LinesToMove; cnt++)
			{
				MemPos = cntx * NrBytesSubBitmap;
				MemPos += cnt * (MaxScreenX / 8);
				memmove(&TempMem2[MemPos], &TempMem[cnt * NrBytesSubBitmap], LineBytesToMove);
			}
		}

		if ((mode & 1) == 0)
			FileWrite(fp, TempMem2, LinesToMove * (MaxScreenX / 8), &result);
		else
		{
			for (cnt = 0; cnt < LinesToMove; cnt++)
			{
				LineBytes = &TempMem2[cnt * (MaxScreenX / 8)];
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

	if ((mode & 1) == 1)
	{
		FileSeek(fp, 0);
		BmpHeader.BitmapDataSize = NrBytesBitmapData;
		BmpHeader.FileSize = sizeof(BmpHeader) + NrBitmapColors * 4 + BmpHeader.BitmapDataSize;
		FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
	}

	DeleteObject(PrinterFont);
	DeleteObject(PrinterFont_90);
	DeleteObject(PrinterFont2);
	DeleteObject(PrinterFont2_90);
	DeleteObject(PrinterFont3);
	DeleteObject(PrinterFont3_90);


// ****************************************************************************************************

	FileClose(fp);
	DeleteObject(ViewBitmap);
	DeleteDC(OutputDisplay);

// ****************************************************************************************************

	OutputDisplay = 0;
	Printing = 0;
	Factor = Factor2;
	Xoffset = Xoffset2;
	Yoffset = Yoffset2;
	DrawWindowMinX = OldDrawWindowMinX;
	DrawWindowMinY = OldDrawWindowMinY;
	DrawWindowMaxX = OldDrawWindowMaxX;
	DrawWindowMaxY = OldDrawWindowMaxY;
	ViewMinX = PixelToRealOffX(-1);
	ViewMaxX = PixelToRealOffX(DrawWindowMaxX + 1);
	ViewMinY = PixelToRealOffY(-1);
	ViewMaxY = PixelToRealOffY(DrawWindowMaxY + 1 - DrawWindowMinY);

	SetNormalCursor();
	DeAllocateMemTemp();
	DeAllocateMemTemp2();
	DeleteGraphicObjects();
	PrintingThickness = 1;
	CreateDrawObjects(0);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ExportToBitmap2(int32 mode, int32 ExportPixelsX, int32 ExportPixelsY)
{
	HBITMAP ViewBitmap;
	BITMAPINFO *BitmapInfo;
	uint8 BitmapInfoMem[1024];
	char DirPart[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], Name[MAX_LENGTH_STRING];
	BITMAPV4HEADER *BitmapHeader;
	double Xoffset2, Yoffset2, Factor2, MinX, MaxX, MinY, MaxY, ScreenX, ScreenY;
	int32 fp, result, PixelsX, PixelsY, ok, cnt, cnt2, cnt3, cnt4, res, NrBitmapColors, OldDrawWindowMinX,
	      OldDrawWindowMinY, OldDrawWindowMaxX, OldDrawWindowMaxY, NrBytesSubBitmap, MaxScreenX, MaxScreenY,
	      NrBytesBitmapData, NrCodedBytes, ColorIndex, ColorPixelCount, ColorsUsed[256], NrUsedColors, SubBitmapMemsize,
	      *LineBytes2 = NULL, CurrentColor, TempColor;

	uint8 CodedLine[16384], TwoColors[8] = { 0, 0, 0, 0, 255, 255, 255, 0 };

	BmpHeaderRecord BmpHeader;

	OutputDisplay = CreateCompatibleDC(0);

	ExportPixelsX = (ExportPixelsX + 15) & ~15;
	ExportPixelsY = (ExportPixelsY + 7) & ~7;
	PixelsX = ExportPixelsX;
	PixelsY = ExportPixelsY;
	NrBytesSubBitmap = PixelsX * 4;
	memset(&BitmapInfoMem, 0, sizeof(BitmapInfoMem));
	BitmapInfo = (BITMAPINFO *) BitmapInfoMem;

	BitmapHeader = (BITMAPV4HEADER *) & BitmapInfo->bmiHeader;
	BitmapHeader->bV4Size = sizeof(BITMAPV4HEADER);
	BitmapHeader->bV4Planes = 1;
	BitmapHeader->bV4V4Compression = BI_RGB;
	BitmapHeader->bV4BitCount = 32;
	BitmapHeader->bV4Width = PixelsX;
	BitmapHeader->bV4Height = PixelsY;

	/*
	  BitmapHeader2=(BITMAPINFOHEADER *)&BitmapInfo->bmiHeader;
	  BitmapHeader2->biSize=sizeof(BITMAPINFOHEADER);
	  BitmapHeader2->biPlanes=1;
	  BitmapHeader2->biCompression=BI_RGB;
	  BitmapHeader2->biBitCount=32;
	  BitmapHeader2->biWidth=PixelsX;
	  BitmapHeader2->biHeight=PixelsY;
	*/
	if ((mode & 2) == 2)
		ViewBitmap = CreateCompatibleBitmap(OutputDisplay, PixelsX, PixelsY);
	else
		ViewBitmap = CreateBitmap(PixelsX, PixelsY, 1, 32, NULL);

	res = (int) SelectObject(OutputDisplay, ViewBitmap);

// ****************************************************************************************************

	FindMinMaxDesign(&MinX, &MinY, &MaxX, &MaxY);

	OldDrawWindowMinX = DrawWindowMinX;
	OldDrawWindowMinY = DrawWindowMinY;
	OldDrawWindowMaxX = DrawWindowMaxX;
	OldDrawWindowMaxY = DrawWindowMaxY;
	Xoffset2 = Xoffset;
	Yoffset2 = Yoffset;
	Factor2 = Factor;

	if (((MaxX - MinX) / (double) PixelsX) > ((MaxY - MinY) / (double) PixelsY))
	{
		Factor = (double) PixelsX / ((MaxX - MinX) * 1.1);
		BitmapExportResolution = ((MaxX - MinX) * 1.1 * 254000.0) / PixelsX;
	}
	else
	{
		Factor = (double) PixelsY / ((MaxY - MinY) * 1.1);
		BitmapExportResolution = ((MaxY - MinY) * 1.1 * 254000.0) / PixelsY;
	}

	CreateDrawObjects(0);
	PrintingThickness = 1;

	if (2540000.0 / BitmapExportResolution > 200.0)
		PrintingThickness = 2;

	if (2540000.0 / BitmapExportResolution > 500.0)
		PrintingThickness = 4;

	if (2540000.0 / BitmapExportResolution > 1000.0)
		PrintingThickness = 7;

	SetBkMode(OutputDisplay, TRANSPARENT);
	SetROP2(OutputDisplay, R2_COPYPEN);
	Printing = 0;
	DrawWindowMinX = 0;
	DrawWindowMinY = 0;
	DrawWindowMaxX = PixelsX;
	DrawWindowMaxY = PixelsY;
	NrBytesBitmapData = 0;
	Factor = (double) ((2540000.0 / BitmapExportResolution) / 10.0);
	/*
	  MaxScreenX=Mult((MaxX-MinX)*1.1);
	  MaxScreenX=((MaxScreenX+31)/32)*32;
	  MaxScreenY=Mult((MaxY-MinY)*1.1);
	*/
	MaxScreenX = PixelsX;
	MaxScreenY = PixelsY;
	Xoffset = (MinX - ((MaxX - MinX) * (double) 0.05));
	Yoffset = (MinY - ((MaxY - MinY) * (double) 0.05));
	ScreenX = PixelToReal(PixelsX);
	ScreenY = PixelToReal(PixelsY);

	SubBitmapMemsize = PixelsX * 4 * PixelsY;
//  SubBitmapMemsize=PixelsX*4*1024;
	AllocateMemTemp(SubBitmapMemsize);
	memset(TempMem, 0, SubBitmapMemsize);

// ****************************************************************************************************

	if (GetDirFromFileName(DirPart, EditFile) != 0)
		return;

	GetNameWithOutExtensionFromFileName(EditFile, Name);
	sprintf(FileName, "%s\\%s.bmp", DirPart, Name);
	fp = FileOpenWriteUTF8(FileName);

	if (fp <= 0)
	{
		MessageBoxUTF8(SCHWindow, FileName, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
		return;
	}

	memset(&BmpHeader, 0, sizeof(BmpHeader));

	if ((mode & 1) == 0)
	{
		NrBitmapColors = 0;
		BmpHeader.StartOfDataOffset = 62;
	}
	else
	{
		NrBitmapColors = 256;
		BmpHeader.StartOfDataOffset = 54 + NrBitmapColors * 4;
	}

	BmpHeader.Identifier = 0x4d42;
	BmpHeader.BitmapHeaderSize = 40;
	BmpHeader.Width = MaxScreenX;
	BmpHeader.Height = MaxScreenY;
	memset(&ColorsUsed, 0, sizeof(ColorsUsed));

	if ((mode & 1) == 0)
	{
		BmpHeader.BitmapDataSize = MaxScreenX * 4 * MaxScreenY;
		BmpHeader.FileSize = sizeof(BmpHeader) + NrBitmapColors * 4 + BmpHeader.BitmapDataSize;
		BmpHeader.BitsPerPixel = 32;
	}
	else
	{
		BmpHeader.BitsPerPixel = 8;
		BmpHeader.CompressionType = 1;	// RLE
	}

	BmpHeader.NrOfPlanes = 1;
	BmpHeader.NrColors1 = NrBitmapColors;

	BmpHeader.HResolutionInPixelsPerMeter = (int32) ((double) 1e8 / BitmapExportResolution);
	BmpHeader.VResolutionInPixelsPerMeter = (int32) ((double) 1e8 / BitmapExportResolution);


	FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);

	if ((mode & 1) == 1)
	{
		for (cnt = 0; cnt < 256; cnt++)
			FileWrite(fp, &TwoColors, 4, &result);
	}

	NrUsedColors = 0;

// ****************************************************************************************************

	ViewMinX = (double) (Xoffset - 1.0);
	ViewMaxX = (double) (ViewMinX + ScreenX + 1.0);
	ViewMinY = (double) (Yoffset - 1.0);
	ViewMaxY = (double) (ViewMinY + ScreenY + 1.0);
//  InitDrawingJunctions();
	InitDrawingBackGround(0, 0);
	Rectangle(OutputDisplay, 0, 0, PixelsX, PixelsY);

	DrawInstances(0);

	if (!EditingSymbol)
	{
		DrawWires(0);
		DrawBusses(0);
		DrawJunctions(0);
		DrawOnePinNets(0);
		DrawBusConnections(0);
		DrawGlobalConnections(0);
		DrawNetLabels(0);
	}
	else
	{
		DrawPins(0);
		DrawPowerPins(0);
		DrawPinBusses(0);
	}

	DrawObjectLines(0);
	DrawObjectRects(0);
	DrawObjectCircles(0);
	DrawObjectArcs(0);
	DrawObjectTexts(0);

//  InitDrawingJunctions();
//  ellips2(500,300,200,200,255);

// *************************************************************************************

	if ((mode & 2) == 2)
	{
//    res=GetDIBits(OutputDisplay,ViewBitmap,0,PixelsY,TempMem,BitmapInfo,DIB_RGB_COLORS);
		res = GetDIBits(OutputDisplay, ViewBitmap, 0, PixelsY, TempMem, BitmapInfo, DIB_RGB_COLORS);
	}
	else
		res = GetBitmapBits(ViewBitmap, MaxScreenY * MaxScreenX * 4, TempMem);

	ok = 1;
#ifdef _DEBUG

	if (ExportPixelsX > 0)
	{
		for (cnt = 0; cnt < MaxScreenY; cnt++)
		{
			pos = cnt * MaxScreenX * 4;

			for (cnt2 = 0; cnt2 < MaxScreenX * 4; cnt2++)
			{
				if (TempMem[pos + cnt2] != 0)
					ok = 1;
			}
		}
	}

#endif

	if ((mode & 1) == 0)
	{
		if ((mode & 2) == 2)
			FileWrite(fp, TempMem, MaxScreenY * MaxScreenX * 4, &result);
		else
		{
			for (cnt = 0; cnt < MaxScreenY; cnt++)
			{
				LineBytes2 = (int32 *) & TempMem[(MaxScreenY - cnt - 1) * MaxScreenX * 4];
				FileWrite(fp, LineBytes2, MaxScreenX * 4, &result);
			}
		}
	}
	else
	{
		for (cnt = 0; cnt < MaxScreenY; cnt++)
		{
			if ((mode & 2) == 2)
				LineBytes2 = (int32 *) & TempMem[cnt * MaxScreenX * 4];
			else
				LineBytes2 = (int32 *) & TempMem[(MaxScreenY - cnt - 1) * MaxScreenX * 4];

//      memp=(int32 *)&TempMem[cnt*MaxScreenX*4];
//      CurrentColor=(uint32)-1;
			CurrentColor = LineBytes2[0] & 0xffffff;
			ColorPixelCount = 0;
			NrCodedBytes = 0;
			cnt3 = 0;
			ColorIndex = 0;

			for (cnt2 = 0; cnt2 < MaxScreenX; cnt2++)
			{
				TempColor = LineBytes2[cnt2] & 0xffffff;

//        TempColor=*memp & 0xffffff;
				if (CurrentColor == TempColor)
					ColorPixelCount++;
				else
				{	// Found a different color, store the previous color pixels
					cnt3 = 0;

					while ((cnt3 < NrUsedColors) && (ColorsUsed[cnt3] != CurrentColor))
						cnt3++;

					if ((cnt3 == NrUsedColors) && (cnt3 < 255))
					{
						ColorsUsed[cnt3] = CurrentColor;
						NrUsedColors++;
					}

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

						CodedLine[NrCodedBytes++] = (uint8) cnt3;
					}

					ColorPixelCount = 1;
					CurrentColor = TempColor;
				}

//        memp+=4;
			}

			if (ColorPixelCount > 0)
			{
				cnt3 = 0;

				while ((cnt3 < NrUsedColors) && (ColorsUsed[cnt3] != CurrentColor))
					cnt3++;

				if ((cnt3 == NrUsedColors) && (cnt3 < 255))
				{
					ColorsUsed[cnt3] = CurrentColor;
					NrUsedColors++;
				}

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

					CodedLine[NrCodedBytes++] = (uint8) cnt3;
				}
			}

			CodedLine[NrCodedBytes++] = 0;
			CodedLine[NrCodedBytes++] = 0;
			NrBytesBitmapData += NrCodedBytes;
			FileWrite(fp, (uint8 *) CodedLine, NrCodedBytes, &result);
		}
	}

// ****************************************************************************

	if ((mode & 1) == 1)
	{
		FileSeek(fp, 0);
		BmpHeader.BitmapDataSize = NrBytesBitmapData;
		BmpHeader.FileSize = sizeof(BmpHeader) + NrBitmapColors * 4 + BmpHeader.BitmapDataSize;
		FileWrite(fp, &BmpHeader, sizeof(BmpHeader), &result);
		FileWrite(fp, &ColorsUsed, sizeof(ColorsUsed), &result);
	}

// ****************************************************************************************************

	FileClose(fp);
	DeleteObject(ViewBitmap);
	DeleteDC(OutputDisplay);
}

//***************************************************************************************************************************
//********************************** IDD_DIALOG_PDF_OUTPUT ******************************************************************
//***************************************************************************************************************************

int32 CALLBACK ExportPDFDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
#define    InRange20(x1,x2) ( (((x1>x2-1.0) && (x1<x2+1.0))) ? (1) : (0) )

	int32 about;
	int32 cnt, res;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(383, "Export to PDF"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(423, "Paper options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(424, "Orientation"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(328, "Scale"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(425, "Paper size"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO6, "Auto");
		SetDialogItemTextUTF8(Dialog, IDC_RADIO7, SC(427, "Portrait"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO8, SC(428, "Landscape"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO9, SC(429, "Scale 1x"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO10, SC(430, "Fit to page"));

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A1");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A2");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A3");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A4");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "A5");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B4");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B5");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B4_JIS");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "B5_JIS");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "LEGAL");
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) "LETTER");

		for (cnt = 0; cnt < NrPageOutputIds; cnt++)
		{
			if (PageOutputIds[cnt] == PDFInfo.PaperSize)
				SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_SETCURSEL, (WPARAM) cnt, 0);
		}

		switch (PDFInfo.PaperOrientation)
		{
		case ORIENTATION_AUTO:
			SendDlgItemMessage(Dialog, IDC_RADIO6, BM_SETCHECK, 1, 0);
			break;

		case ORIENTATION_PORTRAIT:
			SendDlgItemMessage(Dialog, IDC_RADIO7, BM_SETCHECK, 1, 0);
			break;

		case ORIENTATION_LANDSCAPE:
			SendDlgItemMessage(Dialog, IDC_RADIO8, BM_SETCHECK, 1, 0);
			break;
		}

		if (PDFInfo.PaperFitToPage == 0)
			SendDlgItemMessage(Dialog, IDC_RADIO9, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessage(Dialog, IDC_RADIO10, BM_SETCHECK, 1, 0);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			if (SendDlgItemMessage(Dialog, IDC_RADIO6, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_AUTO;

			if (SendDlgItemMessage(Dialog, IDC_RADIO7, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_PORTRAIT;

			if (SendDlgItemMessage(Dialog, IDC_RADIO8, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperOrientation = ORIENTATION_LANDSCAPE;

			if (SendDlgItemMessage(Dialog, IDC_RADIO9, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperFitToPage = 0;

			if (SendDlgItemMessage(Dialog, IDC_RADIO10, BM_GETCHECK, 0, 0) == 1)
				PDFInfo.PaperFitToPage = 1;

			res = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

			if (res != -1)
				PDFInfo.PaperSize = PageOutputIds[res];

			EndDialog(Dialog, 1);
			return about;

		case IDHELP:
//          Help("Plot_output_to_printer,0);
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


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void DrawPDFStr(double x1, double y1, double Size, double Rotation, int32 TextAlignment, LPSTR TextString)
{
	/*
	  TextRotation:

	    0 Normal
	    1 Rotated 90 degrees CCW


	*/
	double FontSize, x11, y11, x, y;
	char PDFstr[MAX_LENGTH_STRING], ConvTextStr[MAX_LENGTH_STRING];
	int32 lengte2, cnt, cnt2;

	if (PDFCurrentColor == 0)
	{
		AddToMessageBuf("0 g");
		AddToMessageBuf("0 G");
		PDFCurrentColor = 1;
	}

	FontSize = PDFConv(Size * 1.1);
	lengte2 = strlen(TextString);
	AddToMessageBuf("BT");
	sprintf(PDFstr, "  /F1 %.3f Tf", FontSize);
	AddToMessageBuf(PDFstr);
	x = x1;
	y = y1;

	if (Rotation == 0)
	{
		switch (TextAlignment)
		{
		case ALIGN_LEFT_BOTTOM:
			x += Size * 0.1 * DefFontSize;
			break;

		case ALIGN_RIGHT_BOTTOM:
			x -= lengte2 * Size * DefFontSize * PDFCharWidthFactor;
			break;

		case ALIGN_LEFT_CENTRE:
			x += Size * 0.1 * DefFontSize;
			y -= Size * 0.45 * DefFontSize;
			break;

		case ALIGN_RIGHT_CENTRE:
			y -= Size * 0.45 * DefFontSize;
			x -= lengte2 * Size * DefFontSize * PDFCharWidthFactor;
			break;

		case ALIGN_LEFT_TOP:
			x += Size * 0.1 * DefFontSize;
			y -= Size * 1.15 * DefFontSize;
			break;

		case ALIGN_RIGHT_TOP:
			y -= Size * 1.15 * DefFontSize;
			x -= lengte2 * Size * DefFontSize * PDFCharWidthFactor;
			break;
		}
	}
	else
	{
		if (InRangeSpecial(Rotation, 90.0, 0.01))
		{
			x += Size * -0.3 * DefFontSize;

			switch (TextAlignment)
			{
			case ALIGN_LEFT_BOTTOM:
				y += Size * 0.1 * DefFontSize;
				break;

			case ALIGN_RIGHT_BOTTOM:
				y -= lengte2 * Size * DefFontSize * PDFCharWidthFactor;
				break;

			case ALIGN_LEFT_CENTRE:
				y += Size * 0.1 * DefFontSize;
				x += Size * 0.45 * DefFontSize;
				break;

			case ALIGN_RIGHT_CENTRE:
				x += Size * 0.45 * DefFontSize;
				y -= lengte2 * Size * DefFontSize * PDFCharWidthFactor;
				break;

			case ALIGN_LEFT_TOP:
				y += Size * 0.1 * DefFontSize;
				x += Size * 1.15 * DefFontSize;
				break;

			case ALIGN_RIGHT_TOP:
				y -= lengte2 * Size * DefFontSize * PDFCharWidthFactor;
				x += Size * 1.15 * DefFontSize;
				break;
			}
		}
	}

	if (Rotation == 0)
	{
		x11 = PDFConvX(x);
		y11 = PDFConvY(y + 0.1);
		sprintf(PDFstr, "  %.3f %.3f Td", x11, y11);
		AddToMessageBuf(PDFstr);
	}
	else
	{
		if (InRangeSpecial(Rotation, 90.0, 0.01))
		{
			x11 = PDFConvX(x);
			y11 = PDFConvY(y);
			sprintf(PDFstr, "  0.0 1.0 -1.0 0.0 %.3f %.3f Tm", x11, y11);
			AddToMessageBuf(PDFstr);
		}
		else
		{
			x11 = PDFConvX(x);
			y11 = PDFConvY(y);
			sprintf(PDFstr, "  %.4f %.4f %.4f %.4f %.3f %.3f Tm", cos(ANGLE_CONVERT(Rotation)),
			        sin(ANGLE_CONVERT(Rotation)), -sin(ANGLE_CONVERT(Rotation)), cos(ANGLE_CONVERT(Rotation)), x11,
			        y11);
			AddToMessageBuf(PDFstr);
		}
	}

	cnt2 = 0;

	for (cnt = 0; cnt < (int) strlen(TextString); cnt++)
	{
		if (TextString[cnt] != '\\')
			ConvTextStr[cnt2++] = TextString[cnt];
		else
		{
			ConvTextStr[cnt2++] = TextString[cnt];
			ConvTextStr[cnt2++] = TextString[cnt];
		}
	}

	ConvTextStr[cnt2++] = 0;
	sprintf(PDFstr, "  (%s) Tj", ConvTextStr);
	AddToMessageBuf(PDFstr);
	AddToMessageBuf("ET");

}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 PDFOutput(int32 mode)
{
	double x1, y1, x1a, y1a, x2, y2, x3, y3, x4, y4, hulp, Rotation, ThickNess, x11, y11, x22, y22, x33, y33;
	int32 cnt, cnt2, cnt3, cnt4, TextAlignment, TextRotation, BusConnectionType, AddMode, ok, NrLines, lengte,
	      ConnectionType, CircleMode, TextMode, Alignment, Length2, MemPos, SymbolNr, SegmentCount, LineSegments,
	      InstanceRotation, ObjectMirrorX, ObjectMirrorY, NrProperties;
	double Angle, Length;
	PointRecord LinePoints[16];
	PinRecord *Pin;
	PowerPinRecord *PowerPin;
	PinBusRecord *PinBus;
	double LineBuf[4096];
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
	char PinBusStrings[16][64], TextStr[1024], TextStr2[1024], NewPinBusStr[512], str[MAX_LENGTH_STRING];
	int32 LinePos[16];
	LPSTR LabelText;
	InstanceRecord *Instance;
	ObjectRecord *Object;
	SymbolRecord *Symbol;
	SymbolsPosRecord *SymbolPos;
#ifdef _DEBUG
	int32 res;
#endif

	x11 = 0.0;
	y11 = 0.0;
	x3 = 0.0;
	y3 = 0.0;
	x4 = 0.0;
	y4 = 0.0;

// ****************************************************************************************************
// ****************************************************************************************************
	for (cnt = 0; cnt < Design.NrWires; cnt++)
	{
		Wire = &((*Wires)[cnt]);

		if ((Wire->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Wire->X1;
			y1 = Wire->Y1;
			x2 = Wire->X2;
			y2 = Wire->Y2;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

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
	}

// ****************************************************************************************************
// ****************************************************************************************************
	for (cnt = 0; cnt < Design.NrBusses; cnt++)
	{
		Bus = &((*Busses)[cnt]);

		if ((Bus->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Bus->X1;
			y1 = Bus->Y1;
			x2 = Bus->X2;
			y2 = Bus->Y2;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = PDFConv(PDFDefaultLineWidth * 4.0);

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
	}

// ****************************************************************************************************
// ****************************************************************************************************
	for (cnt = 0; cnt < Design.NrJunctions; cnt++)
	{
		Junction = &((*Junctions)[cnt]);

		if ((Junction->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = Junction->X;
			y1 = Junction->Y;
			ThickNess = 0.5;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = PDFConv(ThickNess);

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
			sprintf(str, "%.3f %.3f l S", x11, y11);
			AddToMessageBuf(str);
		}
	}

	for (cnt = 0; cnt < Design.NrOnePinNets; cnt++)
	{
		OnePinNet = &((*OnePinNets)[cnt]);

		if ((OnePinNet->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			x1 = OnePinNet->X;
			y1 = OnePinNet->Y;
			ThickNess = 0.4;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

			if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
			{
				PDFLineWidth = PDFNewLineWidth;
				sprintf(str, "%.3f w", PDFLineWidth);
				AddToMessageBuf(str);
			}

			x11 = PDFConvX(x1 - ThickNess);
			y11 = PDFConvY(y1 - ThickNess);
			x22 = PDFConvX(x1 + ThickNess);
			y22 = PDFConvY(y1 + ThickNess);
			sprintf(str, "%.3f %.3f m", x11, y11);
			AddToMessageBuf(str);
			sprintf(str, "%.3f %.3f l S", x22, y22);
			AddToMessageBuf(str);
			sprintf(str, "%.3f %.3f m", x11, y22);
			AddToMessageBuf(str);
			sprintf(str, "%.3f %.3f l S", x22, y11);
			AddToMessageBuf(str);
		}
	}

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

			DrawPDFStr(x2, y2, 1.0, (double) TextRotation * 90.0, TextAlignment, str);
		}
	}

	for (cnt = 0; cnt < Design.NrBusConnections; cnt++)
	{
		BusConnection = &((*BusConnections)[cnt]);

		if ((BusConnection->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			BusConnectionType = (BusConnection->Alignment >> 14) & 3;
			x1 = BusConnection->X;
			y1 = BusConnection->Y;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			switch (BusConnectionType)
			{
			case 0:			/*  \    */
				x11 = PDFConvX(x1 - BusSizeX);
				y11 = PDFConvY(y1 - BusSizeY * 0.5);
				x22 = PDFConv(BusSizeX * 0.5);
				y22 = PDFConv(BusSizeY);
				sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
				AddToMessageBuf(str);
				x11 = PDFConvX(x1 - BusSizeX * 0.5);
				y11 = PDFConvY(y1);
				break;

			case 1:			/* rotate 90   */
				/*  /\         */
				/*             */
				x11 = PDFConvX(x1 - BusSizeY * 0.5);
				y11 = PDFConvY(y1 - BusSizeX);
				x22 = PDFConv(BusSizeY);
				y22 = PDFConv(BusSizeX * 0.5);
				sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
				AddToMessageBuf(str);
				x11 = PDFConvX(x1);
				y11 = PDFConvY(y1 - BusSizeX * 0.5);
				break;

			case 2:			/* rotate 180  */
				/*  /          */
				/*  \          */
				x11 = PDFConvX(x1 + BusSizeX * 0.5);
				y11 = PDFConvY(y1 - BusSizeY * 0.5);
				x22 = PDFConv(BusSizeX * 0.5);
				y22 = PDFConv(BusSizeY);
				sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
				AddToMessageBuf(str);
				x11 = PDFConvX(x1 + BusSizeX * 0.5);
				y11 = PDFConvY(y1);
				break;

			case 3:			/* rotate 270  */
				/*             */
				/*  \/         */
				x11 = PDFConvX(x1 - BusSizeY * 0.5);
				y11 = PDFConvY(y1 + BusSizeX * 0.5);
				x22 = PDFConv(BusSizeY);
				y22 = PDFConv(BusSizeX * 0.5);
				sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
				AddToMessageBuf(str);
				x11 = PDFConvX(x1);
				y11 = PDFConvY(y1 + BusSizeX * 0.5);

				break;
			}

			PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

			if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
			{
				PDFLineWidth = PDFNewLineWidth;
				sprintf(str, "%.3f w", PDFLineWidth);
				AddToMessageBuf(str);
			}

			x22 = PDFConvX(x1);
			y22 = PDFConvY(y1);
			sprintf(str, "%.3f %.3f m", x11, y11);
			AddToMessageBuf(str);
			sprintf(str, "%.3f %.3f l S", x22, y22);
			AddToMessageBuf(str);
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************
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

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			switch (ConnectionType >> 1)
			{
			case 0:			// input
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

				for (cnt2 = 0; cnt2 < 3; cnt2++)
				{
					x11 = PDFConvX(LinePoints[cnt2].x + x1);
					y11 = PDFConvY(LinePoints[cnt2].y + y1);
					sprintf(str, "%.3f %.3f", x11, y11);

					if (cnt2 == 0)
						strcat(str, " m");
					else
						strcat(str, " l");

					AddToMessageBuf(str);
				}

				AddToMessageBuf("h f");
				break;

			case 1:			// output
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

				for (cnt2 = 0; cnt2 < 3; cnt2++)
				{
					x11 = PDFConvX(LinePoints[cnt2].x + x1);
					y11 = PDFConvY(LinePoints[cnt2].y + y1);
					sprintf(str, "%.3f %.3f", x11, y11);

					if (cnt2 == 0)
						strcat(str, " m");
					else
						strcat(str, " l");

					AddToMessageBuf(str);
				}

				AddToMessageBuf("h f");
				break;

			case 2:			// input/output
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

				for (cnt2 = 0; cnt2 < 3; cnt2++)
				{
					x11 = PDFConvX(LinePoints[cnt2].x + x1);
					y11 = PDFConvY(LinePoints[cnt2].y + y1);
					sprintf(str, "%.3f %.3f", x11, y11);

					if (cnt2 == 0)
						strcat(str, " m");
					else
						strcat(str, " l");

					AddToMessageBuf(str);
				}

				AddToMessageBuf("h f");

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

				for (cnt2 = 0; cnt2 < 3; cnt2++)
				{
					x11 = PDFConvX(LinePoints[cnt2].x + x1);
					y11 = PDFConvY(LinePoints[cnt2].y + y1);
					sprintf(str, "%.3f %.3f", x11, y11);

					if (cnt2 == 0)
						strcat(str, " m");
					else
						strcat(str, " l");

					AddToMessageBuf(str);
				}

				AddToMessageBuf("h f");

				PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}

				if ((ConnectionType & 1) == 0)
				{
					x11 = PDFConvX(x1 - 2.0);
					y11 = PDFConvY(y1);
					x22 = PDFConvX(x1 - 0.8);
					y22 = PDFConvY(y1);
				}
				else
				{
					x11 = PDFConvX(x1 + 2.0);
					y11 = PDFConvY(y1);
					x22 = PDFConvX(x1 + 0.8);
					y22 = PDFConvY(y1);
				}

				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				sprintf(str, "%.3f %.3f l S", x22, y22);
				AddToMessageBuf(str);
				break;
			}

			DrawPDFStr(x1a, y1a, 1.0, 0.0, (GlobalConnection->NameInfo & 15), GlobalConnection->Text);
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************

	for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
	{
		ObjectLine = &((*ObjectLines)[cnt]);

		if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			x1 = ObjectLine->X1;
			y1 = ObjectLine->Y1;
			x2 = ObjectLine->X2;
			y2 = ObjectLine->Y2;

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = PDFConv(ObjectLine->Thickness);

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
					x33 = PDFConvX(x4);
					y33 = PDFConvY(y4);
					sprintf(str, "%.3f %.3f m", x11, y11);
					AddToMessageBuf(str);
					sprintf(str, "%.3f %.3f l S", x33, y33);
					AddToMessageBuf(str);
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x1, y1, -30.0);
					x33 = PDFConvX(x4);
					y33 = PDFConvY(y4);
					sprintf(str, "%.3f %.3f m", x11, y11);
					AddToMessageBuf(str);
					sprintf(str, "%.3f %.3f l S", x33, y33);
					AddToMessageBuf(str);
				}

				if ((ObjectLine->LineMode & 2) == 2)
				{
					x3 = x2 - cos(Angle) * ARROW_LENGTH;
					y3 = y2 - sin(Angle) * ARROW_LENGTH;
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x2, y2, 30.0);
					x33 = PDFConvX(x4);
					y33 = PDFConvY(y4);
					sprintf(str, "%.3f %.3f m", x22, y22);
					AddToMessageBuf(str);
					sprintf(str, "%.3f %.3f l S", x33, y33);
					AddToMessageBuf(str);
					x4 = x3;
					y4 = y3;
					RotatePointFromOtherPoint(&x4, &y4, x2, y2, -30.0);
					x33 = PDFConvX(x4);
					y33 = PDFConvY(y4);
					sprintf(str, "%.3f %.3f m", x22, y22);
					AddToMessageBuf(str);
					sprintf(str, "%.3f %.3f l S", x33, y33);
					AddToMessageBuf(str);
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

			if (ObjectRect->Thickness == 0.0)
			{
				x2 = ObjectRect->Width;
				y2 = ObjectRect->Height;
				x11 = PDFConvX(x1 - x2 * 0.5);
				y11 = PDFConvY(y1 - y2 * 0.5);
				x22 = PDFConv(x2);
				y22 = PDFConv(y2);

				sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
				AddToMessageBuf(str);

				sprintf(str, "%.3f %.3f l S", x22, y22);
				AddToMessageBuf(str);
			}
			else
			{
				x2 = ObjectRect->Width * 0.5;
				y2 = ObjectRect->Height * 0.5;

				if (PDFCurrentColor == 0)
				{
					AddToMessageBuf("0 g");
					AddToMessageBuf("0 G");
					PDFCurrentColor = 1;
				}

				PDFNewLineWidth = PDFConv(ObjectRect->Thickness);

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}

				x11 = PDFConvX(x1 - x2);
				y11 = PDFConvY(y1 - y2);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				x22 = PDFConvX(x1 + x2);
				y22 = PDFConvY(y1 - y2);
				sprintf(str, "%.3f %.3f l S", x22, y22);
				AddToMessageBuf(str);

				x11 = PDFConvX(x1 + x2);
				y11 = PDFConvY(y1 - y2);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				x22 = PDFConvX(x1 + x2);
				y22 = PDFConvY(y1 + y2);
				sprintf(str, "%.3f %.3f l S", x22, y22);
				AddToMessageBuf(str);

				x11 = PDFConvX(x1 + x2);
				y11 = PDFConvY(y1 + y2);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				x22 = PDFConvX(x1 - x2);
				y22 = PDFConvY(y1 + y2);
				sprintf(str, "%.3f %.3f l S", x22, y22);
				AddToMessageBuf(str);

				x11 = PDFConvX(x1 - x2);
				y11 = PDFConvY(y1 + y2);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				x22 = PDFConvX(x1 - x2);
				y22 = PDFConvY(y1 - y2);
				sprintf(str, "%.3f %.3f l S", x22, y22);
				AddToMessageBuf(str);
			}
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************
	for (cnt = 0; cnt < Design.NrObjectCircles; cnt++)
	{
		ObjectCircle = &((*ObjectCircles)[cnt]);

		if ((ObjectCircle->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			if (ObjectCircle->Thickness == 0.0)
			{
				x1 = ObjectCircle->CentreX;
				y1 = ObjectCircle->CentreY;
				x2 = ObjectCircle->Diam;

				PDFNewLineWidth = PDFConv(x2);

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

				x11 = PDFConvX(x1);
				y11 = PDFConvY(y1);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				sprintf(str, "%.3f %.3f l S", x11, y11);
				AddToMessageBuf(str);
			}
			else
			{
				x1 = ObjectCircle->CentreX;
				y1 = ObjectCircle->CentreY;
				x2 = ObjectCircle->Diam;

				switch (ObjectCircle->CircleMode)
				{
				case 1:
					x3 = x2;
					y4 = x2;
					break;

				case 2:
					y3 = x2;
					x4 = -x2;
					break;

				case 3:
					y3 = -x2;
					y4 = x2;
					break;

				case 4:
					x3 = -x2;
					y4 = -x2;
					break;

				case 6:
					x3 = -x2;
					x4 = x2;
					break;

				case 8:
					y3 = -x2;
					x4 = x2;
					break;

				case 9:
					x3 = x2;
					x4 = -x2;
					break;

				case 12:
					y3 = x2;
					y4 = -x2;
					break;

				default:
					y3 = x2;
					y4 = x2;
					break;
				}

				LineSegments = ArcToLineSegments(x1, y1, x2, x2, x3, y3, x4, y4, (double *) &LineBuf);

				if (PDFCurrentColor == 0)
				{
					AddToMessageBuf("0 g");
					AddToMessageBuf("0 G");
					PDFCurrentColor = 1;
				}

				PDFNewLineWidth = PDFConv(ObjectCircle->Thickness);

				if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
				{
					PDFLineWidth = PDFNewLineWidth;
					sprintf(str, "%.3f w", PDFLineWidth);
					AddToMessageBuf(str);
				}

				SegmentCount = 0;

				for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
				{
					x11 = PDFConvX(LineBuf[SegmentCount++]);
					y11 = PDFConvY(LineBuf[SegmentCount++]);
					x22 = PDFConvX(LineBuf[SegmentCount++]);
					y22 = PDFConvY(LineBuf[SegmentCount++]);
					sprintf(str, "%.3f %.3f m", x11, y11);
					AddToMessageBuf(str);
					sprintf(str, "%.3f %.3f l S", x22, y22);
					AddToMessageBuf(str);
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
			LineSegments = ArcToLineSegments(x1, y1, x2, y2, x3, y3, x4, y4, (double *) &LineBuf);

			if (PDFCurrentColor == 0)
			{
				AddToMessageBuf("0 g");
				AddToMessageBuf("0 G");
				PDFCurrentColor = 1;
			}

			PDFNewLineWidth = PDFConv(ObjectArc->Thickness);

			if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
			{
				PDFLineWidth = PDFNewLineWidth;
				sprintf(str, "%.3f w", PDFLineWidth);
				AddToMessageBuf(str);
			}

			SegmentCount = 0;

			for (cnt2 = 0; cnt2 < LineSegments; cnt2++)
			{
				x11 = PDFConvX(LineBuf[SegmentCount++]);
				y11 = PDFConvY(LineBuf[SegmentCount++]);
				x22 = PDFConvX(LineBuf[SegmentCount++]);
				y22 = PDFConvY(LineBuf[SegmentCount++]);
				sprintf(str, "%.3f %.3f m", x11, y11);
				AddToMessageBuf(str);
				sprintf(str, "%.3f %.3f l S", x22, y22);
				AddToMessageBuf(str);
			}

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
							DrawPDFStr(x1, y1, x2, Rotation, TextAlignment, TextStr2);
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

// ****************************************************************************************************
// ****************************************************************************************************

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

			for (cnt2 = 0; cnt2 < 4; cnt2++)
			{
				x11 = PDFConvX(LinePoints[cnt2].x + x1);
				y11 = PDFConvY(LinePoints[cnt2].y + y1);
				sprintf(str, "%.3f %.3f", x11, y11);

				if (cnt2 == 0)
					strcat(str, " m");
				else
					strcat(str, " l");

				AddToMessageBuf(str);
			}

			AddToMessageBuf("h f");
			TextRotation = (Pin->NameInfo >> 8) & 1;

			if (!EditingSheetSymbol)
				DrawPDFStr(x1a, y1a, 1.0, (double) TextRotation * 90.0, (int16) (Pin->NameInfo & 15), Pin->Name);
			else
				DrawPDFStr(x1a, y1a, 1.0, (double) TextRotation * 90.0, (int16) (Pin->NameInfo & 15), Pin->Label);
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************

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
			DrawPDFStr(x1, y1, x2, (double) TextRotation * 90.0, TextAlignment, str);
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************
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

			if (stricmp(LabelText, "DQ[56:63]") == 0)
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

			for (cnt2 = 0; cnt2 < 4; cnt2++)
			{
				x11 = PDFConvX(LinePoints[cnt2].x + x1);
				y11 = PDFConvY(LinePoints[cnt2].y + y1);
				sprintf(str, "%.3f %.3f", x11, y11);

				if (cnt2 == 0)
					strcat(str, " m");
				else
					strcat(str, " l");

				AddToMessageBuf(str);
			}

			AddToMessageBuf("h f");

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
					memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], (int) (cnt2 - LinePos[NrLines]));
					NrLines++;
					LinePos[NrLines] = cnt2 + 1;
				}

				cnt2++;
			}

			if (PinBus->Text[lengte - 1] != '\\')
			{
				memmove(&PinBusStrings[NrLines], &PinBus->Text[LinePos[NrLines]], (int) (cnt2 - LinePos[NrLines]));
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
			case 2:			// Rotation = 0 , MirrorY = 1 , MirrorX = 0
			case 3:			// Rotation = 0 , MirrorY = 1 , MirrorX = 1
				y3 += (double) (NrLines - 1);
				break;

			case 6:			// Rotation = 1 , MirrorY = 1 , MirrorX = 0
			case 7:			// Rotation = 1 , MirrorY = 1 , MirrorX = 1
				x3 -= (double) (NrLines - 1);
				break;
			}

			for (cnt2 = 0; cnt2 < NrLines; cnt2++)
			{
//        GetMinMaxText(xx3,yy3,x2,0,TextRotation,TextAlignment,PinBusStrings[cnt2]);
				DrawPDFStr(x3, y3, x2, (double) TextRotation * 90.0, TextAlignment, PinBusStrings[cnt2]);

				switch (AddMode)
				{
				case 0:		// Rotation = 0 , MirrorY = 0 , MirrorX = 0
				case 1:		// Rotation = 0 , MirrorY = 0 , MirrorX = 1
				case 2:		// Rotation = 0 , MirrorY = 1 , MirrorX = 0
				case 3:		// Rotation = 0 , MirrorY = 1 , MirrorX = 1
					y3 -= 1.0;
					break;

				case 4:		// Rotation = 1 , MirrorY = 0 , MirrorX = 0
				case 5:		// Rotation = 1 , MirrorY = 0 , MirrorX = 1
				case 6:		// Rotation = 1 , MirrorY = 1 , MirrorX = 0
				case 7:		// Rotation = 1 , MirrorY = 1 , MirrorX = 1
					x3 += 1.0;
					break;
				}
			}
		}
	}

// ****************************************************************************************************
// ****************************************************************************************************
	ok = 1;						// NrLines

	for (cnt = 0; cnt < min(MaxNrInstances, Design.NrInstances); cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & OBJECT_NOT_VISIBLE) == 0)
		{

// ****************************************************************************************************

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

				if (stricmp(str, "CN2") == 0)
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

				case 1:		//  90
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
				DrawPDFStr(x1, y1, 0.9, (double) TextRotation * 90.0, Alignment, str);
			}


// ****************************************************************************************************

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
				y1a = y1 - 1.0;

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

				case 1:		//  90
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
				DrawPDFStr(x1, y1, 0.9, (double) TextRotation * 90.0, Alignment, str);

				if (Instance->PlacingOption != -1)
				{
					x1a += Instance->OriginX;
					y1a += Instance->OriginY;
					strcpy(str, "NP");
					DrawPDFStr(x1a, y1a, 0.9, (double) TextRotation * 90.0, Alignment, str);
				}
			}

// ****************************************************************************************************

			NrObjects = 0;
			InstanceToObject(Instance, 0.0, 0.0, 0);

			for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
			{
				Object = &((*Objects)[cnt2]);
				x1 = Object->x1;
				y1 = Object->y1;
				x2 = Object->x2;
				y2 = Object->y2;
				x3 = 0.0;
				y3 = 0.0;
				x4 = 0.0;
				y4 = 0.0;

				switch (Object->ObjectType)
				{
				case SYMBOL_LINE:
					if (PDFCurrentColor == 0)
					{
						AddToMessageBuf("0 g");
						AddToMessageBuf("0 G");
						PDFCurrentColor = 1;
					}

					PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

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
							x33 = PDFConvX(x4);
							y33 = PDFConvY(y4);
							sprintf(str, "%.3f %.3f m", x11, y11);
							AddToMessageBuf(str);
							sprintf(str, "%.3f %.3f l S", x33, y33);
							AddToMessageBuf(str);
							x4 = x3;
							y4 = y3;
							RotatePointFromOtherPoint(&x4, &y4, x1, y1, -30.0);
							x33 = PDFConvX(x4);
							y33 = PDFConvY(y4);
							sprintf(str, "%.3f %.3f m", x11, y11);
							AddToMessageBuf(str);
							sprintf(str, "%.3f %.3f l S", x33, y33);
							AddToMessageBuf(str);
						}

						if ((Object->Info3 & 2) == 2)
						{
							x3 = x2 - cos(Angle) * ARROW_LENGTH;
							y3 = y2 - sin(Angle) * ARROW_LENGTH;
							x4 = x3;
							y4 = y3;
							RotatePointFromOtherPoint(&x4, &y4, x2, y2, 30.0);
							x33 = PDFConvX(x4);
							y33 = PDFConvY(y4);
							sprintf(str, "%.3f %.3f m", x22, y22);
							AddToMessageBuf(str);
							sprintf(str, "%.3f %.3f l S", x33, y33);
							AddToMessageBuf(str);
							x4 = x3;
							y4 = y3;
							RotatePointFromOtherPoint(&x4, &y4, x2, y2, -30.0);
							x33 = PDFConvX(x4);
							y33 = PDFConvY(y4);
							sprintf(str, "%.3f %.3f m", x22, y22);
							AddToMessageBuf(str);
							sprintf(str, "%.3f %.3f l S", x33, y33);
							AddToMessageBuf(str);
						}
					}

					break;

				case SYMBOL_RECT:
					if (Object->Thickness == 0.0)
					{
						x11 = PDFConvX(x1 - x2 * 0.5);
						y11 = PDFConvY(y1 - y2 * 0.5);

						x22 = PDFConv(x2);
						y22 = PDFConv(y2);

						sprintf(str, "%.3f %.3f %.3f %.3f re f", x11, y11, x22, y22);
						AddToMessageBuf(str);

						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);
					}
					else
					{
						x2 *= 0.5;
						y2 *= 0.5;

						if (PDFCurrentColor == 0)
						{
							AddToMessageBuf("0 g");
							AddToMessageBuf("0 G");
							PDFCurrentColor = 1;
						}

						PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

						if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
						{
							PDFLineWidth = PDFNewLineWidth;
							sprintf(str, "%.3f w", PDFLineWidth);
							AddToMessageBuf(str);
						}

						x11 = PDFConvX(x1 - x2);
						y11 = PDFConvY(y1 - y2);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(x1 + x2);
						y22 = PDFConvY(y1 - y2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);

						x11 = PDFConvX(x1 + x2);
						y11 = PDFConvY(y1 - y2);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(x1 + x2);
						y22 = PDFConvY(y1 + y2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);

						x11 = PDFConvX(x1 + x2);
						y11 = PDFConvY(y1 + y2);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(x1 - x2);
						y22 = PDFConvY(y1 + y2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);

						x11 = PDFConvX(x1 - x2);
						y11 = PDFConvY(y1 + y2);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						x22 = PDFConvX(x1 - x2);
						y22 = PDFConvY(y1 - y2);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);
					}

					break;

				case SYMBOL_CIRCLE:
					if (Object->Thickness == 0.0)
					{
						PDFNewLineWidth = PDFConv(x2);

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

						x11 = PDFConvX(x1);
						y11 = PDFConvY(y1);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						sprintf(str, "%.3f %.3f l S", x11, y11);
						AddToMessageBuf(str);
					}
					else
					{
						CircleMode =
						    ((Object->Info2 & 0x80) >> 4) + ((Object->Info2 & 0x20) >> 3) +
						    ((Object->Info2 & 0x08) >> 2) + ((Object->Info2 & 0x02) >> 1);

						switch (CircleMode)
						{
						case 1:
							x3 = x2;
							y4 = x2;
							break;

						case 2:
							y3 = -x2;
							x4 = x2;
							break;

						case 3:
							y3 = -x2;
							y4 = x2;
							break;

						case 4:
							x3 = -x2;
							y4 = -x2;
							break;

						case 6:
							x3 = -x2;
							x4 = x2;
							break;

						case 8:
							y3 = x2;
							x4 = -x2;
							break;

						case 9:
							x3 = x2;
							x4 = -x2;
							break;

						case 12:
							y3 = x2;
							y4 = -x2;
							break;

						default:
							y3 = x2;
							y4 = x2;
							break;
						}

						LineSegments = ArcToLineSegments(x1, y1, x2, x2, x3, y3, x4, y4, (double *) &LineBuf);

						if (PDFCurrentColor == 0)
						{
							AddToMessageBuf("0 g");
							AddToMessageBuf("0 G");
							PDFCurrentColor = 1;
						}

						PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

						if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
						{
							PDFLineWidth = PDFNewLineWidth;
							sprintf(str, "%.3f w", PDFLineWidth);
							AddToMessageBuf(str);
						}

						SegmentCount = 0;

						for (cnt3 = 0; cnt3 < LineSegments; cnt3++)
						{
							x11 = PDFConvX(LineBuf[SegmentCount++]);
							y11 = PDFConvY(LineBuf[SegmentCount++]);
							x22 = PDFConvX(LineBuf[SegmentCount++]);
							y22 = PDFConvY(LineBuf[SegmentCount++]);
							sprintf(str, "%.3f %.3f m", x11, y11);
							AddToMessageBuf(str);
							sprintf(str, "%.3f %.3f l S", x22, y22);
							AddToMessageBuf(str);
						}
					}

					break;

				case SYMBOL_ARC:
					x3 = Object->x3;
					y3 = Object->y3;
					x4 = Object->x4;
					y4 = Object->y4;
					LineSegments = ArcToLineSegments(x1, y1, x2, y2, x3, y3, x4, y4, (double *) &LineBuf);

					if (PDFCurrentColor == 0)
					{
						AddToMessageBuf("0 g");
						AddToMessageBuf("0 G");
						PDFCurrentColor = 1;
					}

					PDFNewLineWidth = PDFConv(PDFDefaultLineWidth);

					if ((PDFLineWidth == 0.0) || (NotInRange2(PDFNewLineWidth, PDFLineWidth)))
					{
						PDFLineWidth = PDFNewLineWidth;
						sprintf(str, "%.3f w", PDFLineWidth);
						AddToMessageBuf(str);
					}

					SegmentCount = 0;

					for (cnt3 = 0; cnt3 < LineSegments; cnt3++)
					{
						x11 = PDFConvX(LineBuf[SegmentCount++]);
						y11 = PDFConvY(LineBuf[SegmentCount++]);
						x22 = PDFConvX(LineBuf[SegmentCount++]);
						y22 = PDFConvY(LineBuf[SegmentCount++]);
						sprintf(str, "%.3f %.3f m", x11, y11);
						AddToMessageBuf(str);
						sprintf(str, "%.3f %.3f l S", x22, y22);
						AddToMessageBuf(str);
					}

					break;

				case SYMBOL_TEXT:
					TextAlignment = (Object->Info2 & 0x0f);
					TextRotation = (Object->Info2 >> 4) & 3;

					if (ConvertTextString(Object->Text1, TextStr) == -1)
						strcpy(TextStr, Object->Text1);

					Length2 = strlen(TextStr);
					cnt3 = 0;
					cnt4 = cnt3;

					while (cnt3 < Length2 + 1)
					{
						if ((TextStr[cnt3] == '\r') || ((cnt3 == Length2) && (TextStr[cnt3 - 1] != '\n')))
						{
							if (cnt3 - cnt4 > 0)
							{
								memset(TextStr2, 0, sizeof(TextStr2));
								strncpy(TextStr2, (LPSTR) & TextStr[cnt4], min(127, cnt3 - cnt4));
								DrawPDFStr(x1, y1, x2, (double) TextRotation * 90.0, TextAlignment, TextStr2);
							}

							x1 += sin(ANGLE_CONVERT((double) TextRotation * 90.0)) * x2;
							y1 -= cos(ANGLE_CONVERT((double) TextRotation * 90.0)) * x2;
							cnt3 += 1;
							cnt4 = cnt3 + 1;
						}

						cnt3++;
					}

					break;

				case SYMBOL_PIN_TEXT:
					TextAlignment = (Object->Info2 & 0x0f);
					TextRotation = (Object->Info2 >> 4) & 3;

					if (InRange(x2, 1.0))
						x2 = 0.9;

					DrawPDFStr(x1, y1, x2, (double) TextRotation * 90.0, TextAlignment, Object->Text1);
					break;

				case SYMBOL_POWERPIN_TEXT:
					TextAlignment = (Object->Info2 & 0x0f);
					TextRotation = (Object->Info2 >> 4) & 3;
					strcpy(str, Object->Text1);
					strcat(str, " : ");
					strcat(str, Object->Text2);

					if (InRange(x2, 1.0))
						x2 = 0.9;

					DrawPDFStr(x1, y1, x2, (double) TextRotation * 90.0, TextAlignment, str);
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
					cnt3 = 0;
#ifdef _DEBUG

					if (stricmp(Object->Text2, "a[0:15]") == 0)
						ok = 1;

#endif
					NrLines = 0;
					memset(LinePos, 0, sizeof(LinePos));
					memset(PinBusStrings, 0, sizeof(PinBusStrings));
					LinePos[0] = 0;

					while (cnt3 < lengte)
					{
						if (NewPinBusStr[cnt3] == '\\')
						{
							memmove(&PinBusStrings[NrLines], &NewPinBusStr[LinePos[NrLines]], cnt3 - LinePos[NrLines]);

							if (NrLines < 7)
								NrLines++;

							LinePos[NrLines] = cnt3 + 1;
						}

						cnt3++;
					}

					if (NewPinBusStr[lengte - 1] != '\\')
					{
						memmove(&PinBusStrings[NrLines], &NewPinBusStr[LinePos[NrLines]], cnt3 - LinePos[NrLines]);
						LinePos[NrLines] = cnt3;
						NrLines++;
					}

					x3 = x1;
					y3 = y1;

					switch (AddMode)
					{
					case 2:	// Rotation = 0 , MirrorY = 1 , MirrorX = 0
					case 3:	// Rotation = 0 , MirrorY = 1 , MirrorX = 1
						y3 += (double) (NrLines - 1);
						break;

					case 6:	// Rotation = 1 , MirrorY = 1 , MirrorX = 0
					case 7:	// Rotation = 1 , MirrorY = 1 , MirrorX = 1
						x3 -= (double) (NrLines - 1);
						break;
					}

					for (cnt3 = 0; cnt3 < NrLines; cnt3++)
					{
						if (InRange(x2, 1.0))
							x2 = 0.9;

						DrawPDFStr(x3, y3, x2, (double) TextRotation * 90.0, TextAlignment, PinBusStrings[cnt3]);

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

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 ExportToPDF(int32 PaperSize, int32 Orientation, int32 FitToPage, int32 PdfObjectStart, int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, res, PageSizeUnitsX, PageSizeUnitsY, hulp3, CurrentColor, NrPDFObjects, Rotation,
	      NrProperties, x1, y1, x2, y2, NrPDFObjectsRootDir, NrIntroBytes, WritePageObjects, NrPagesWritten, PageCount,
	      NrCompProperties, PDFObjectPos[4096], pos;
	double MinX, MaxX, MinY, MaxY, hulpx, hulpy, x, y, FontSize, PageSizeX, PageSizeY, PageSizeX2, PageSizeY2,
	       NewLineWidth, dx, dy, hulp, LineWidth, ExtraOffsetAnnotation;
	InstanceRecord *Instance;
	NetLabelRecord *NetLabel;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING],
	     PropertyID[MAX_LENGTH_STRING], PropertyValue[MAX_LENGTH_STRING];
	int fp;
	struct tm *today;
	time_t ltime;

	if (mode == 0)
	{
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PDF_OUTPUT), SCHWindow,
		              (DLGPROC) ExportPDFDialogBody);

		if (res == 2)
			return -1;

		Orientation = PDFInfo.PaperOrientation;
		PaperSize = PDFInfo.PaperSize;
		FitToPage = PDFInfo.PaperFitToPage;
	}
	else
	{
	}

	FindMinMaxDesign(&MinX, &MinY, &MaxX, &MaxY);


	MinX *= 254000.0;
	MinY *= 254000.0;
	MaxX *= 254000.0;
	MaxY *= 254000.0;

	PDFPageOffsetX = 12e5;		// 12 mm
	PDFPageOffsetY = 12e5;		// 12 mm
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
		// Landscape
		DisplX = dx;
		DisplY = DisplX * PageSizeY2 / PageSizeX2;
		PDFMultFactor = PageSizeX2 / dx;

		if (FitToPage == 1)
		{
			DisplX = PageSizeX2;
			DisplY = PageSizeX2 * dy / dx;
			PDFPageOffsetY += (PageSizeY2 - DisplY) * 0.5;
		}
	}
	else
	{
		// Portrait
		DisplY = dy;
		DisplX = DisplY * PageSizeX2 / PageSizeY2;
		PDFMultFactor = PageSizeY2 / dy;

		if (FitToPage == 1)
		{
			DisplY = PageSizeY2;
			DisplX = PageSizeY2 * dx / dy;
			PDFPageOffsetX += (PageSizeX2 - DisplX) * 0.5;
		}
	}

	if (FitToPage == 0)
		PDFMultFactor = 1.0;

	PDFMultFactor2 = 72.0 / 2540000.0;
	FontSize = 0.9;
	ExtraOffsetAnnotation = 0.3;

	memset(PDFObjectPos, 0, sizeof(PDFObjectPos));
	LineWidth = 1.0;
	CurrentColor = 1;
	NewLineWidth = 0.0;
	PageCount = 0;
	NrPagesWritten = 0;
	WritePageObjects = 1;
	NrPDFObjects = 0;
	PDFLineWidth = 0.0;
	PDFDefaultLineWidth = (30000.0 / 254000.0);	// 0.3 mm

	GetFilePartFromFileName(str, EditFile);
	CutExtensionFileName(str);

	if (mode == 1)
	{
		sprintf(FileStr, "%s\\sch\\%s.asc", DesignPath, str);
		fp = FileOpenWriteUTF8(FileStr);

		if (fp <= 0)
		{
			sprintf(str, SC(474, "Can not open temporary print file"));
			MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		cnt4 = 0;

		for (cnt2 = 0; cnt2 < min(MaxNrInstances, Design.NrInstances); cnt2++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt2]);

			if (((Instance->Info & OBJECT_NOT_VISIBLE) == 0) && (Instance->Reference[0] != 0)
			        && (GetInstanceRefInfo(Instance, &x, &y, &Rotation, str3, 0) == 0))
				cnt4++;
		}

		for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
		{
			NetLabel = &((*NetLabels)[cnt2]);

			if (((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
			        && ((NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1)) > 0))
				cnt4++;
		}

		NrIntroBytes = (cnt4 + 3) * sizeof(int32);
		FileSeek(fp, NrIntroBytes);
		MessageBufPos = 0;
		AddToMessageBuf("1 J");
		PDFOutput(0);
		NrPDFObjects = PdfObjectStart;
		PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp) - NrIntroBytes;
		sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
		WriteToFile(fp, str);
		sprintf(str, "<< /Length %d >>\r\n", MessageBufPos);
		WriteToFile(fp, str);
		WriteToFile(fp, "stream\r\n");

		if (MessageBufPos > 0)
			WriteToFile(fp, (LPSTR) MessageBuf);

		WriteToFile(fp, "endstream\r\n");
		WriteToFile(fp, "endobj\r\n");


		if (!EditingSymbol)
		{
			for (cnt2 = 0; cnt2 < min(MaxNrInstances, Design.NrInstances); cnt2++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt2]);

				if (((Instance->Info & OBJECT_NOT_VISIBLE) == 0) && (Instance->Reference[0] != 0)
				        && (GetInstanceRefInfo(Instance, &x, &y, &Rotation, str3, 1) == 0))
				{
					PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp) - NrIntroBytes;
					sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
					WriteToFile(fp, str);
					WriteToFile(fp, "<<\r\n");
					WriteToFile(fp, "/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\\( \\\r\n");
					sprintf(str, " '%s' ,\\n\\t['Properties:',\\n' Reference : %s',\\n' Part No    : %s',\\n\\\r\n",
					        str3, str3, Instance->PartNr);
					WriteToFile(fp, str);
					NrCompProperties = GetCompProperties(Instance, NULL, NULL, 0x40);

					if (NrCompProperties > 0)
					{
						sprintf(str, "' value      : %s' ,\\n' Part description : %s',\\n' Geometry    : %s',\\n\\\r\n",
						        Instance->Value, Instance->PartDescription, Instance->Geometry);
						WriteToFile(fp, str);
						str[0] = 0;

						for (cnt3 = 0; cnt3 < NrCompProperties; cnt3++)
						{
							GetCompProperties(Instance, PropertyID, PropertyValue, 0x20 + cnt3);
							sprintf(str2, "' %s   : %s',\\n", PropertyID, PropertyValue);

							if (strlen(str) + strlen(str2) > 80)
							{
								strcat(str, "\r\n");
								WriteToFile(fp, str);
								str[0] = 0;
							}

							strcat(str, str2);
						}

						strcat(str, "]");
					}
					else
					{
						sprintf(str,
						        "' value      : %s' ,\\n' Part description : %s',\\n' Geometry    : %s',\\n]\\\r\n",
						        Instance->Value, Instance->PartDescription, Instance->Geometry);
						WriteToFile(fp, str);
						str[0] = 0;
					}

					strcat(str, " \\); )>>\r\n");
					WriteToFile(fp, str);
					WriteToFile(fp, "/Type /Annot \r\n");
					WriteToFile(fp, "/Subtype /Link \r\n");

					if (Rotation == 0)
					{
						x1 = (int32) (PDFConvX(x - ExtraOffsetAnnotation));
						y1 = (int32) (PDFConvY(y - ExtraOffsetAnnotation));
						x2 = (int32) (x1 +
						              PDFConv(strlen(str3) * PDFCharWidthFactor * FontSize * DefFontSize +
						                      ExtraOffsetAnnotation));
						y2 = (int32) (y1 + PDFConv(FontSize + ExtraOffsetAnnotation * 2));
					}
					else
					{
						x1 = (int32) (PDFConvX(x - ExtraOffsetAnnotation * 3));
						y1 = (int32) (PDFConvY(y));
						x2 = (int32) (x1 + PDFConv(FontSize + ExtraOffsetAnnotation));
						y2 = (int32) (y1 +
						              PDFConv(strlen(str3) * PDFCharWidthFactor * FontSize * DefFontSize +
						                      ExtraOffsetAnnotation * 2));
					}

					sprintf(str, "/Rect [ %d %d %d %d ]\r\n", x1, y1, x2, y2);
					WriteToFile(fp, str);
					WriteToFile(fp, "/Border [ 0 0 0 [ 4 2 ] ] \r\n");
					WriteToFile(fp, "/F 64 \r\n");
					WriteToFile(fp, "/C [ 1 1 0 ] \r\n");
					WriteToFile(fp, ">>\r\n");
					WriteToFile(fp, "endobj\r\n");
				}
			}

			res = 1;

			for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
			{
				NetLabel = &((*NetLabels)[cnt2]);

				if (((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
				        && ((NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1)) > 0))
				{
					PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp) - NrIntroBytes;
					sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
					WriteToFile(fp, str);
					WriteToFile(fp, "<<\r\n");
					WriteToFile(fp, "/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\\( \\\r\n");
					sprintf(str, " '%s' ,\\n\\t['Properties:',\\n", NetLabel->Name);
					WriteToFile(fp, str);
					str[0] = 0;

					for (cnt3 = 0; cnt3 < NrProperties; cnt3++)
					{
						GetProperty(NetLabel->Name, PropertyID, PropertyValue, cnt3);
						sprintf(str2, "' %s   : %s',\\n", PropertyID, PropertyValue);

						if (strlen(str) + strlen(str2) > 80)
						{
							strcat(str, "\r\n");
							WriteToFile(fp, str);
							str[0] = 0;
						}

						strcat(str, str2);
					}

					strcat(str, "] \\); )>>\r\n");
					WriteToFile(fp, str);
					WriteToFile(fp, "/Type /Annot \r\n");
					WriteToFile(fp, "/Subtype /Link \r\n");
					x = NetLabel->ConnectX + NetLabel->TextX;
					y = NetLabel->ConnectY + NetLabel->TextY;
					x1 = (int32) (PDFConvX(x - ExtraOffsetAnnotation));
					y1 = (int32) (PDFConvY(y - ExtraOffsetAnnotation));
					x2 = (int32) (x1 +
					              PDFConv(strlen(NetLabel->Name) * PDFCharWidthFactor * FontSize * DefFontSize +
					                      ExtraOffsetAnnotation));
					y2 = (int32) (y1 + PDFConv(FontSize + ExtraOffsetAnnotation));
					sprintf(str, "/Rect [ %d %d %d %d ]\r\n", x1, y1, x2, y2);
					WriteToFile(fp, str);
					WriteToFile(fp, "/Border [ 0 0 0 [ 4 2 ] ] \r\n");
					WriteToFile(fp, "/F 64 \r\n");
					WriteToFile(fp, "/C [ 1 1 0 ] \r\n");
					WriteToFile(fp, ">>\r\n");
					WriteToFile(fp, "endobj\r\n");
				}
			}
		}

		PDFObjectPos[NrPDFObjects] = FileCurrentPointer(fp) - NrIntroBytes;
		FileSeek(fp, 0);
		FileWrite(fp, &NrPDFObjects, 4, &res);
		FileWrite(fp, &PDFObjectPos[PdfObjectStart], (cnt4 + 2) * sizeof(int32), &res);
		FileClose(fp);

		return 1;
	}

// ****************************************************************************************************
// ****************************************************************************************************

	sprintf(FileStr, "%s\\sch\\%s.pdf", DesignPath, str);
	fp = FileOpenWriteUTF8(FileStr);

	if (fp <= 0)
	{
		sprintf(str, SC(475, "Can not create pdf file %s"), FileStr);
		MessageBoxUTF8(SCHWindow, str, SC(38, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

//  WriteToFile(fp,"%PDF-1.4\n");
//  WriteToFile(fp,"\x25\xe2\xe3\xcf\xd3\n");
	WriteToFile(fp, "%PDF-1.2\r\n");

	NrPagesWritten++;
	MessageBufPos = 0;
	AddToMessageBuf("1 J");

	if (PDFCurrentColor == 0)
	{
		AddToMessageBuf("0 g");
		AddToMessageBuf("0 G");
		PDFCurrentColor = 1;
	}

	PDFOutput(0);

// *INDENT-OFF*
#if 0

4 0 obj
<<
/Type /Page
/Parent 54 0 R
/Resources 41 0 R
/Contents 42 0 R
/Annots [ 5 0 R 6 0 R 7 0 R 8 0 R 9 0 R 10 0 R 11 0 R 12 0 R 13 0 R 14 0 R
15 0 R 16 0 R 17 0 R 18 0 R 19 0 R 20 0 R 21 0 R 22 0 R 23 0 R 24 0 R
25 0 R 26 0 R 27 0 R 28 0 R 29 0 R 30 0 R 31 0 R 32 0 R 33 0 R 34 0 R
35 0 R 36 0 R 37 0 R 38 0 R 39 0 R 40 0 R ]
/Thumb 47 0 R
/MediaBox [ 0 0 1191 842 ]
/CropBox [ 0 0 1191 842 ]
/Rotate 0
>>
endobj
5 0 obj
<<
/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\( 'C7320' ,\n\t['Properties:',\n' Reference  \
  : C7320',\n' Part No     : 2324104',\n' Value        : 100nF',\n' Tole\
rance   : 10%',\n' Nom. Voltage : 10V',\n' Nom. Power   : ',\n' Geom Nam\
e    : C0402',\n' Comp. Height : undefined',\n' Variant      : ?',\n' Li\
brary      : BS Rlslib',\n],\t[ 'Comment:',\n'undefined'\n] \);  )>>
/Type /Annot
/Subtype /Link
/Rect [ 178 607 190 610 ]
/Border [ 0 0 0 [ 4 2 ] ]
/F 64
/C [ 1 1 0 ]
>>
endobj

#endif
// *INDENT-ON*

// ****************************************************************************************************


	        PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	sprintf(str, "<< /Length %d >>\r\n", MessageBufPos);
	WriteToFile(fp, str);
	WriteToFile(fp, "stream\r\n");

	if (MessageBufPos > 0)
	{
		WriteToFile(fp, (LPSTR) MessageBuf);
//    WriteToFile(fp,"\r\n");
	}

	WriteToFile(fp, "endstream\r\n");
	WriteToFile(fp, "endobj\r\n");
	PageCount++;

// ****************************************************************************************************

	if (!EditingSymbol)
	{
		for (cnt2 = 0; cnt2 < min(MaxNrInstances, Design.NrInstances); cnt2++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt2]);
#ifdef _DEBUG

			if (stricmp(Instance->Reference, "R502") == 0)
				ok = 1;

#endif

			if (((Instance->Info & OBJECT_NOT_VISIBLE) == 0) && (Instance->Reference[0] != 0)
			        && (GetInstanceRefInfo(Instance, &x, &y, &Rotation, str3, 1) == 0))
			{
#ifdef _DEBUG

				if (stricmp(Instance->Reference, "R502") == 0)
					ok = 1;

#endif
				PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
				sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
				WriteToFile(fp, str);
				WriteToFile(fp, "<<\r\n");
				WriteToFile(fp, "/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\\( \\\r\n");
				sprintf(str, " '%s' ,\\n\\t['Properties:',\\n' Reference : %s',\\n' Part No    : %s',\\n\\\r\n", str3,
				        str3, Instance->PartNr);
				WriteToFile(fp, str);
				NrCompProperties = GetCompProperties(Instance, NULL, NULL, 0x40);

				if (NrCompProperties > 0)
				{
					sprintf(str, "' value      : %s' ,\\n' Part description : %s',\\n' Geometry    : %s',\\n\\\r\n",
					        Instance->Value, Instance->PartDescription, Instance->Geometry);
					WriteToFile(fp, str);
					str[0] = 0;

					for (cnt3 = 0; cnt3 < NrCompProperties; cnt3++)
					{
						GetCompProperties(Instance, PropertyID, PropertyValue, 0x20 + cnt3);

						if (PropertyID[0] != '~')
							sprintf(str2, "' %s   : %s',\\n", PropertyID, PropertyValue);
						else
							sprintf(str2, "' Net %s -> %s'\\n", (LPSTR) & PropertyID[1], PropertyValue);

						if (strlen(str) + strlen(str2) > 80)
						{
							strcat(str, "\r\n");
							WriteToFile(fp, str);
							str[0] = 0;
						}

						strcat(str, str2);
					}

					strcat(str, "]");
				}
				else
				{
					sprintf(str, "' value      : %s' ,\\n' Part description : %s',\\n' Geometry    : %s',\\n]\\\r\n",
					        Instance->Value, Instance->PartDescription, Instance->Geometry);
					WriteToFile(fp, str);
					str[0] = 0;
				}

				strcat(str, " \\); )>>\r\n");
				WriteToFile(fp, str);
				WriteToFile(fp, "/Type /Annot \r\n");
				WriteToFile(fp, "/Subtype /Link \r\n");

				if (Rotation == 0)
				{
					x1 = (int32) (PDFConvX(x - ExtraOffsetAnnotation));
					y1 = (int32) (PDFConvY(y - ExtraOffsetAnnotation));
					x2 = (int32) (x1 +
					              PDFConv(strlen(str3) * PDFCharWidthFactor * FontSize * DefFontSize +
					                      ExtraOffsetAnnotation));
					y2 = (int32) (y1 + PDFConv(FontSize + ExtraOffsetAnnotation * 2));
				}
				else
				{
					x1 = (int32) (PDFConvX(x - ExtraOffsetAnnotation * 3));
					y1 = (int32) (PDFConvY(y));
					x2 = (int32) (x1 + PDFConv(FontSize + ExtraOffsetAnnotation));
					y2 = (int32) (y1 +
					              PDFConv(strlen(str3) * PDFCharWidthFactor * FontSize * DefFontSize +
					                      ExtraOffsetAnnotation * 2));
				}

				sprintf(str, "/Rect [ %d %d %d %d ]\r\n", x1, y1, x2, y2);
				WriteToFile(fp, str);
				WriteToFile(fp, "/Border [ 0 0 0 [ 4 2 ] ] \r\n");
				WriteToFile(fp, "/F 64 \r\n");
				WriteToFile(fp, "/C [ 1 1 0 ] \r\n");
				WriteToFile(fp, ">>\r\n");
				WriteToFile(fp, "endobj\r\n");
// *INDENT-OFF*
#if 0
5 0 obj
<<
/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\( 'C7320' ,\n\t['Properties:',\n' Reference  \
  : C7320',\n' Part No     : 2324104',\n' Value        : 100nF',\n' Tole\
rance   : 10%',\n' Nom. Voltage : 10V',\n' Nom. Power   : ',\n' Geom Nam\
e    : C0402',\n' Comp. Height : undefined',\n' Variant      : ?',\n' Li\
brary      : BS Rlslib',\n],\t[ 'Comment:',\n'undefined'\n] \);  )>>
/Type /Annot
/Subtype /Link
/Rect [ 178 607 190 610 ]
/Border [ 0 0 0 [ 4 2 ] ]
/F 64
/C [ 1 1 0 ]
>>
endobj


150 0 obj
<<
/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\( \
 'C1067' ,\n\t['Properties:',\n' Reference : C1067',\n' Part No    : ',\n\
' value      : 10u' ,\n' Part description : Capacitor tant elco 10 uF',\n' Geometry    : ELCO_SMD35x28',\n\
 \); )>>
/Type /Annot
/Subtype /Link
/Rect [ 438 331 462 335 ]
/Border [ 0 0 0 [ 4 2 ] ]
/F 64
/C [ 1 1 0 ]
>>
endobj

#endif
// *INDENT-ON*
			}
		}

		for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
		{
			NetLabel = &((*NetLabels)[cnt2]);

			if (((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
			        && ((NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1)) > 0))
			{
				PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
				sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
				WriteToFile(fp, str);
				WriteToFile(fp, "<<\r\n");
				WriteToFile(fp, "/A << /S /JavaScript /Type /Action /JS ( var citem = app.popUpMenu\\( \\\r\n");
				sprintf(str, " '%s' ,\\n\\t['Properties:',\\n", NetLabel->Name);
				WriteToFile(fp, str);
				str[0] = 0;

				for (cnt3 = 0; cnt3 < NrProperties; cnt3++)
				{
					GetProperty(NetLabel->Name, PropertyID, PropertyValue, cnt3);
					sprintf(str2, "' %s   : %s',\\n", PropertyID, PropertyValue);

					if (strlen(str) + strlen(str2) > 80)
					{
						strcat(str, "\r\n");
						WriteToFile(fp, str);
						str[0] = 0;
					}

					strcat(str, str2);
				}

				strcat(str, "] \\); )>>\r\n");
				WriteToFile(fp, str);
				WriteToFile(fp, "/Type /Annot \r\n");
				WriteToFile(fp, "/Subtype /Link \r\n");
				x = NetLabel->ConnectX + NetLabel->TextX;
				y = NetLabel->ConnectY + NetLabel->TextY;
				x1 = (int32) (PDFConvX(x - ExtraOffsetAnnotation));
				y1 = (int32) (PDFConvY(y - ExtraOffsetAnnotation));
				x2 = (int32) (x1 +
				              PDFConv(strlen(NetLabel->Name) * PDFCharWidthFactor * FontSize * DefFontSize +
				                      ExtraOffsetAnnotation));
				y2 = (int32) (y1 + PDFConv(FontSize + ExtraOffsetAnnotation));
				sprintf(str, "/Rect [ %d %d %d %d ]\r\n", x1, y1, x2, y2);
				WriteToFile(fp, str);
				WriteToFile(fp, "/Border [ 0 0 0 [ 4 2 ] ] \r\n");
				WriteToFile(fp, "/F 64 \r\n");
				WriteToFile(fp, "/C [ 1 1 0 ] \r\n");
				WriteToFile(fp, ">>\r\n");
				WriteToFile(fp, "endobj\r\n");
			}
		}
	}

// ****************************************************************************************************
	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	NrPDFObjectsRootDir = NrPDFObjects;
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Pages\r\n");
	sprintf(str, "/Count 1\r\n");
	WriteToFile(fp, str);
	str[0] = 0;
	sprintf(str2, "/Kids [%d 0 R", NrPDFObjectsRootDir + 1);
	strcat(str, str2);
	strcat(str, "]\r\n");
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************
	cnt = 1;
	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Page\r\n");
	sprintf(str, "/Parent %d 0 R\r\n", NrPDFObjectsRootDir);
	WriteToFile(fp, str);
	sprintf(str, "/Resources << /Procset %d 0 R\r\n", NrPDFObjectsRootDir + 6);
	WriteToFile(fp, str);
	sprintf(str, "              /Font << /F1 %d 0 R >>\r\n", NrPDFObjectsRootDir + 7);
	WriteToFile(fp, str);
	WriteToFile(fp, "           >>\r\n");
	sprintf(str, "/MediaBox [0 0 %d %d]\r\n", PageSizeUnitsX, PageSizeUnitsY);
	WriteToFile(fp, str);
	sprintf(str, "/Contents 1 0 R\r\n");
	WriteToFile(fp, str);

	if (!EditingSymbol)
	{
		str[0] = 0;
		cnt3 = 0;

		for (cnt2 = 0; cnt2 < min(MaxNrInstances, Design.NrInstances); cnt2++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt2]);

			if (((Instance->Info & OBJECT_NOT_VISIBLE) == 0) && (Instance->Reference[0] != 0)
			        && (GetInstanceRefInfo(Instance, &x, &y, &Rotation, str3, 0) == 0))
				cnt3++;
		}

		for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
		{
			NetLabel = &((*NetLabels)[cnt2]);

			if (((NetLabel->Info & OBJECT_NOT_VISIBLE) == 0)
			        && ((NrProperties = GetProperty(NetLabel->Name, NULL, NULL, -1)) > 0))
				cnt3++;
		}

		for (cnt2 = 0; cnt2 < cnt3; cnt2++)
		{
			if (cnt2 == 0)
				strcat(str, "/Annots [ ");

			if (strlen(str) + 7 > 80)
			{
				strcat(str, "\r\n");
				WriteToFile(fp, str);
				str[0] = 0;
			}

			sprintf(str2, "%d 0 R ", cnt2 + 2);
			strcat(str, str2);
// *INDENT-OFF*
/*
/Annots [ 5 0 R 6 0 R 7 0 R 8 0 R 9 0 R 10 0 R 11 0 R 12 0 R 13 0 R 14 0 R
15 0 R 16 0 R 17 0 R 18 0 R 19 0 R 20 0 R 21 0 R 22 0 R 23 0 R 24 0 R
25 0 R 26 0 R 27 0 R 28 0 R 29 0 R 30 0 R 31 0 R 32 0 R 33 0 R 34 0 R
35 0 R 36 0 R 37 0 R 38 0 R 39 0 R 40 0 R ]
*/
// *INDENT-ON*
		}

		strcat(str, "]\r\n");
		WriteToFile(fp, str);
	}

	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");
// ****************************************************************************************************
	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\r\n");
	sprintf(str, "/Dest [%d 0 R /Fit]\r\n", NrPDFObjectsRootDir + 1);
	WriteToFile(fp, str);
	sprintf(str, "/Parent %d 0 R\r\n", NrPDFObjectsRootDir + 3);
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");


// ****************************************************************************************************

	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Outlines\r\n");
	sprintf(str, "/First %d 0 R\r\n", NrPDFObjectsRootDir + 2);
	WriteToFile(fp, str);
	sprintf(str, "/Last %d 0 R\r\n", NrPDFObjectsRootDir + 2);
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************
	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Catalog\r\n");
	sprintf(str, "/Pages %d 0 R\r\n", NrPDFObjectsRootDir);
	WriteToFile(fp, str);
	sprintf(str, "/Outlines %d 0 R\r\n", NrPDFObjectsRootDir + 3);
	WriteToFile(fp, str);
	WriteToFile(fp, "/PageLayout /SinglePage\r\n");
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************
	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
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

	GetFilePartFromFileName(str, EditFile);

	if (!EditingSymbol)
		sprintf(PDFTitle, "Schematic file %s", str);
	else
	{
		if (!EditingSheetSymbol)
			sprintf(PDFTitle, "Symbol file %s", str);
		else
			sprintf(PDFTitle, "Sheet symbol file %s", str);
	}

	sprintf(str, "/Title (%s)\r\n", PDFTitle);
	WriteToFile(fp, str);
	sprintf(str, "/Producer (PCB elegance %d.%d\n", VER_VERSION / 100, VER_VERSION % 100);
	WriteToFile(fp, str);
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************

	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "[ /PDF /Text ]\r\n");
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");

// ****************************************************************************************************

	PDFObjectPos[NrPDFObjects++] = FileCurrentPointer(fp);
	sprintf(str, "%d 0 obj\r\n", NrPDFObjects);
	WriteToFile(fp, str);
	WriteToFile(fp, "<<\r\n");
	WriteToFile(fp, "/Type /Font\r\n");
	WriteToFile(fp, "/Subtype /Type1\r\n");
	WriteToFile(fp, "/Name /F1\r\n");
	WriteToFile(fp, "/BaseFont /Courier\r\n");
//  WriteToFile(fp,"/BaseFont /Helvetica\r\n");
	WriteToFile(fp, "/Encoding /MacRomanEncoding\r\n");
	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "endobj\r\n");
// ****************************************************************************************************

	pos = FileCurrentPointer(fp);
	WriteToFile(fp, "xref\r\n");
	sprintf(str, "0 %d\r\n", NrPDFObjects + 1);
	WriteToFile(fp, str);
	WriteToFile(fp, "0000000000 65535 f\r\n");

	for (cnt = 0; cnt < NrPDFObjects; cnt++)
	{
		sprintf(str, "%010d 00000 n\r\n", PDFObjectPos[cnt]);
		WriteToFile(fp, str);
	}

	WriteToFile(fp, "trailer\r\n");
	WriteToFile(fp, "<<\r\n");
	sprintf(str, "/Size %d\r\n", NrPDFObjects + 1);
	WriteToFile(fp, str);
	sprintf(str, "/Root %d 0 R\r\n", NrPDFObjectsRootDir + 4);
	WriteToFile(fp, str);
	sprintf(str, "/Info %d 0 R\r\n", NrPDFObjectsRootDir + 5);
	WriteToFile(fp, str);

	WriteToFile(fp, ">>\r\n");
	WriteToFile(fp, "startxref\r\n");
	sprintf(str, "%d\r\n", pos);
	WriteToFile(fp, str);
	WriteToFile(fp, "%%EOF\r\n");

	FileClose(fp);

	DeAllocateMemMessageBuf();

	return 0;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
