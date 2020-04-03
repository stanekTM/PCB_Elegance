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



#include "io.h"
#include "time.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "types.h"
#include "memory.h"
#include "string.h"
#include "calc.h"
#include "calc3.h"
#include "calc4.h"
#include "memory.h"
#include "menus.h"
#include "pcb.h"
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
#include "font.h"
#include "ctype.h"


#define  ClearObject(Object) (memset(&Object,0,sizeof(Object)))
#define  MAX_BLOCKS     256

extern double TextMinX, TextMinY, TextMaxX, TextMaxY;
extern HDC OutputDisplay;


double CentreSelectedX, CentreSelectedY, SelectedMinX, SelectedMinY, SelectedMaxX, SelectedMaxY, DXFminx, DXFminy,
       DXFmaxx, DXFmaxy;



int32 NrBlocks, NrDXFLayers, SelectedLayers[32], DialogMode, ExportLayers[32], BlockStartP[MAX_BLOCKS],
      BlockEndP[MAX_BLOCKS], NrBlocks, BlockNr[5], BlockP[5], BlockSize[5], BlockLineNr[MAX_BLOCKS], FilePointerDXF,
      FileSizeDXF, ok, BlockNestingLevel, FilePosition[5];
char ExportFileName[MAX_LENGTH_STRING], ImportDXFFile[MAX_LENGTH_STRING], BlockNames[MAX_BLOCKS][64],
     BlockNestingNames[5][64], BlockLayerName[5][64], DXFLayers[128][64], ExportDXFFileName[MAX_LENGTH_STRING] =
         "", *DXFBuffer;

double BlockInsertX[5], BlockInsertY[5], BlockInsertZ[5], BlockRotation[5], BlockBaseX[5], BlockBaseY[5], BlockBaseZ[5],
       BlockInsertScaleX[5], BlockInsertScaleY[5], StartAngle, EndAngle, PolyLineX[1000], PolyLineY[1000], PolyLineZ[1000],
       StartWidth, EndWidth, PolyLineStartWidth, PolyLineEndWidth, VertexStartWidth[1000], VertexEndWidth[1000],
       VertexBulge[1000], AngleBetweenLines[1000], PointsX[1000], PointsY[1000], MinX, MinY, MaxX, MaxY,
       VertexStartWidthTemp[1000], VertexEndWidthTemp[1000];


int32 DXFOptions = 0;


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 SearchBlockName(LPSTR BlockName)
{
	int32 cnt;

	cnt = 0;

	while ((cnt < NrBlocks) && (stricmpOwn(BlockNames[cnt], BlockName) != 0))
		cnt++;

	if (cnt < NrBlocks)
		return cnt;

	return -1;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ReadLnDXF2(LPSTR LineBuf, int32 * LineStartFilePos, int32 * NextLineStartFilePos)
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

int32 ReadLnDxf(int32 fp, LPSTR RegelBuf)
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

	if ((FileSizeDXF = FileSizeUTF8(FileName)) < 0)
		return -1;

	if ((fp = FileOpenReadOnlyUTF8(FileName)) <= 0)
		return -1;

	if (FileSizeDXF + 65536 > MaxTempMemory)
		AllocateMemTemp(FileSizeDXF + 65536);

	DXFBuffer = (char *) TempMem;
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

					if (stricmpOwn(ValueStr, "SECTION") == 0)
					{
						Nesting++;
						FoundStr = 1;
					}

					if (stricmpOwn(ValueStr, "SEQEND") == 0)
					{
						// TextLineNr
					}

					if (stricmpOwn(ValueStr, "VERTEX") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							res = 1;
					}

					if (stricmpOwn(ValueStr, "POLYLINE") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							NrPolylinePoints = 0;
					}

					if (stricmpOwn(ValueStr, "ENDSEC") == 0)
					{
						Nesting--;

						if (Nesting == -1)
							res = 1;
					}

					if (stricmpOwn(ValueStr, "TABLE") == 0)
						Nesting++;

					if (stricmpOwn(ValueStr, "ENDTAB") == 0)
					{
						Nesting--;

						if (Nesting == 0)
							res = 1;
					}

					if (stricmpOwn(ValueStr, "BLOCK") == 0)
					{
						Nesting++;
						BlockStartP[NrBlocks] = FileP;
					}

					if (stricmpOwn(ValueStr, "ENDBLK") == 0)
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

					while ((cnt < NrDXFLayers) && (stricmpOwn(DXFLayers[cnt], ValueStr) != 0))
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

//*******************************************************************************************
//***************************** Vybrat vrstvu DXF *******************************************
//*******************************************************************************************


int32 CALLBACK DXFLayerDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, NrSelectedLayers;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(266, "Layers"));

		for (cnt = 0; cnt < NrDXFLayers; cnt++)
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);

		if (DialogMode == 0)
		{
			SetWindowTextUTF8(Dialog, SC(362, "Select DXF layers"));
			res =
			    SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SELITEMRANGE, (WPARAM) 1,
			                          (LPARAM) MAKELPARAM(0, NrDXFLayers));
		}
		else
			SetWindowTextUTF8(Dialog, SC(327, "Select layers")); //nevyužité

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			NrSelectedLayers = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, 32, (LPARAM) & SelectedLayers);
			EndDialog(Dialog, NrSelectedLayers);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;
			/*
			        case IDHELP:
			          Help(IDH_ImportDXF,0);
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

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 SelectImportDXFLayer(int32 mode)
{
	int32 res;

	DialogMode = 0;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LAYER), PCBWindow, (DLGPROC) DXFLayerDialogBody);

	if (res <= 0)
		return -1;

	return res;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 ImportDXF(int32 Layer)
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

	int32 Found, res, TextAlignment, PolyLineFlags, cnt, cnt2, cnt3, LineLength, Units, LineNr, LastEntityCommand,
	      Attributes, ReadMode, FilePos, FilePos2, PreviousFilePos, Nesting, CommandCode, ValueInt, NrPolylinePoints, ok,
	      LineNrP[5], BulgeFlags, LayerCnt, NrSelectedDXFLayers, TextMirror;
	double ValueDouble, X[10], Y[10], Z[10], Width, Height, Thickness, Diameter, Angle, Length2, LengthLine, dx, dy,
	       BlockAngles[5], StartAngle, EndAngle, BlockInsertScaleValueX, BlockInsertScaleValueY, MinX, MinY, MaxX, MaxY,
	       AnglesAreafill[32], x1, y1, x2, y2, x3, y3;
	char NumberString[MAX_LENGTH_STRING], ValueStr[MAX_LENGTH_STRING], CurrentCommandString[MAX_LENGTH_STRING],
	     AttributeName[MAX_LENGTH_STRING], LayerName[MAX_LENGTH_STRING], TextString[MAX_LENGTH_STRING],
	     BlockName[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING],
	     str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], RegelBuf[4096], CopyStr[MAX_LENGTH_STRING],
	     VarName[MAX_LENGTH_STRING], PolyLineLayerName[MAX_LENGTH_STRING], LayerNameToCheck[MAX_LENGTH_STRING],
	     EenString[10] = " ";

#ifdef _DEBUG
	char ImportLayerName[MAX_LENGTH_STRING];
#endif

	LastEntityCommand = -1;
	ClearObject(Params);
	ClearObject(X);
	ClearObject(Y);
	ClearObject(Z);

	for (cnt = 0; cnt < 5; cnt++)
	{
		BlockInsertScaleX[cnt] = 1.0;
		BlockInsertScaleY[cnt] = 1.0;
	}

	BlockInsertScaleValueX = 1.0;
	BlockInsertScaleValueY = 1.0;

//  Layer=INFO_LAYER2;

	ClearObject(NewObjectLine);
	ClearObject(NewObjectRect);
	ClearObject(NewObjectCircle);
	ClearObject(NewObjectArc);
	ClearObject(NewObjectText2);
	ClearObject(NewObjectPolygon);

	switch (Layer)
	{
	case SILKSCREEN_BOTTOM:
	case SILKSCREEN_TOP:
	case INFO_LAYER:
	case INFO_LAYER2:
	case INFO_LAYER3:
	case INFO_LAYER4:
		break;

	default:
		return -1;
	}

	NewObjectLine.Layer = Layer;
	NewObjectRect.Layer = Layer;
	NewObjectCircle.Layer = Layer;
	NewObjectArc.Layer = Layer;
	NewObjectText2.Layer = Layer;
	NewObjectPolygon.Layer = Layer;
	NewObjectLine.Info = (OBJECT_SELECTED + 3);
	NewObjectRect.Info = OBJECT_SELECTED;
	NewObjectCircle.Info = OBJECT_SELECTED;
	NewObjectArc.Info = OBJECT_SELECTED;
	NewObjectText2.Info = OBJECT_SELECTED;
	NewObjectPolygon.Info = OBJECT_SELECTED;

	NewObjectCircle.CircleMode = 15;
	NewObjectPolygon.NrVertices = 4;

	NrPolylinePoints = 0;
	CommandCode = 0;
	TextAlignment = 0;
	Width = 0.0;
	Height = 0.0;
	Thickness = 0.0;
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

	if (GetNewFileUTF8
	        (PCBWindow, NULL, ImportDXFFile, ExportDir, SC(363, "DXF file"), NULL, SC(364, "Import DXF file"), "dxf", 0))
		return -1;

//  if (LoadNewFile3(ImportDXFFile,SC(363,"DXF file"),SC(364,"Import DXF file"),"dxf",0)!=0) return -1;
	PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_OBJECTS, (LPARAM) NULL);
	CheckInputMessages(0);
	strcpy(FileName, ImportDXFFile);
//  strcpy(FileName,"c:\\pcb_elegance\\hs80_rev3\\hs80_rev3.dxf");
//  strcpy(FileName,"c:\\own\\dxf\\smallpcb.dxf");
//  strcpy(FileName,"c:\\own\\dxf\\tube_preamp.dxf");
//  strcpy(FileName,"c:\\own\\dxf\\wilhome.dxf");

	SetWaitCursor();

	GetLayersDXF(FileName);

	SetNormalCursor();

// NrDXFLayers DXFLayers

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


	FilePointerDXF = 0;

	LineNr = 0;
//  Units=2; // inch
	Units = 1;					// mm
//  Units=0; // thou
	Found = 0;
	Nesting = 0;
	NrBlocks = 0;
	ReadMode = 0;
	TextMirror = 0;
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
// ***************************************************************************************************
// ***************************************************************************************************
		if (LineLength >= 0)
		{
			LineNr++;
#ifdef _DEBUG

			if (LineNr == 121907)
				res = 1;

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

#ifdef _DEBUG

					if ((StartPoint) && (stricmpOwn(ImportLayerName, LayerName) == 0))
						res = 1;

#endif

					if (Nesting >= 1)
					{
// ***************************************************************************************************
						if (stricmpOwn(CurrentCommandString, "VERTEX") == 0)
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

//                  VertexBulge[NrPolylinePoints]=Bulge;
									NrPolylinePoints++;
								}
							}
						}

// ***************************************************************************************************
						if ((stricmpOwn(CurrentCommandString, "SEQEND") == 0)
						        || (stricmpOwn(CurrentCommandString, "LINE") == 0))
						{
// Process polyline/polygon points
//              PolyLineFlags & 1 == 1 -> Polygon
//              Width
//              LayerName
#ifdef _DEBUG
							if (LineNr == 843)
								res = 1;

#endif

							if (stricmpOwn(CurrentCommandString, "LINE") == 0)
							{
								PolyLineX[0] = X[0];
								PolyLineY[0] = Y[0];
								PolyLineZ[0] = Z[0];
								PolyLineX[1] = X[1];
								PolyLineY[1] = Y[1];
								PolyLineZ[1] = Z[1];
								VertexBulge[0] = 0.0;
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
							if (stricmpOwn(CurrentCommandString, "LINE") == 0)
								strcpy(LayerNameToCheck, LayerName);
							else
								strcpy(LayerNameToCheck, PolyLineLayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								ok = 1;

								for (cnt = 0; cnt < NrPolylinePoints; cnt++)
								{
									PointsX[cnt] = UnitsConvert(Units, PolyLineX[cnt]);
									PointsY[cnt] = UnitsConvert(Units, PolyLineY[cnt]);
									VertexStartWidthTemp[cnt] = UnitsConvert(Units, VertexStartWidth[cnt]);
									VertexEndWidthTemp[cnt] = UnitsConvert(Units, VertexEndWidth[cnt]);
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

										PointsX[cnt] += UnitsConvert(Units, BlockInsertX[BlockNestingLevel]);
										PointsY[cnt] += UnitsConvert(Units, BlockInsertY[BlockNestingLevel]);
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

											PointsX[cnt] += UnitsConvert(Units, BlockInsertX[0]);
											PointsY[cnt] += UnitsConvert(Units, BlockInsertY[0]);
										}
									}
								}

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
										if (VertexBulge[cnt] != 0.0)
										{
											if (InRangeSpecial(VertexBulge[cnt], 1.0, 0.001))
											{
												if (InRangeSpecial
												        (LengthLine, VertexStartWidthTemp[cnt],
												         max(0.0000001, (fabs(VertexStartWidthTemp[cnt]) / 1000000))))
												{	// Round filled circle
													NewObjectArc.CentreX =
													    (float) ((PointsX[cnt] + PointsX[cnt + 1]) * 0.5);
													NewObjectArc.CentreY =
													    (float) ((PointsY[cnt] + PointsY[cnt + 1]) * 0.5);
													NewObjectArc.Width = (float) (VertexStartWidthTemp[cnt] * 2.0);
													NewObjectArc.Height = (float) (VertexStartWidthTemp[cnt] * 2.0);
													NewObjectArc.LineThickNess = 0.0;
													NewObjectArc.StartDiffX = 0.0;
													NewObjectArc.StartDiffY = NewObjectArc.Width;
													NewObjectArc.EndDiffX = 0.0;
													NewObjectArc.EndDiffY = NewObjectArc.Width;
													NewObjectArc.Info |= OBJECT_FILLED;
													AddObjectArc(&NewObjectArc);
												}
											}
											else
											{	// Trace
												if (dx == 0.0)
												{	// Vertical trace
													NewObjectLine.X1 = (float) PointsX[cnt];
													NewObjectLine.X2 = (float) PointsX[cnt];

													if (PointsY[cnt] < PointsY[cnt + 1])
													{
														NewObjectLine.Y1 =
														    (float) (PointsY[cnt] + VertexStartWidthTemp[cnt] * 0.5);
														NewObjectLine.Y2 =
														    (float) (PointsY[cnt + 1] -
														             VertexStartWidthTemp[cnt] * 0.5);
													}
													else
													{
														NewObjectLine.Y1 =
														    (float) (PointsY[cnt] - VertexStartWidthTemp[cnt] * 0.5);
														NewObjectLine.Y2 =
														    (float) (PointsY[cnt + 1] +
														             VertexStartWidthTemp[cnt] * 0.5);
													}
												}
												else
												{
													if (dy == 0.0)
													{	// Horizontal trace
														NewObjectLine.Y1 = (float) PointsY[cnt];
														NewObjectLine.Y2 = (float) PointsY[cnt];

														if (PointsX[cnt] < PointsX[cnt + 1])
														{
															NewObjectLine.X1 =
															    (float) (PointsX[cnt] +
															             VertexStartWidthTemp[cnt] * 0.5);
															NewObjectLine.X2 =
															    (float) (PointsX[cnt + 1] -
															             VertexStartWidthTemp[cnt] * 0.5);
														}
														else
														{
															NewObjectLine.X1 =
															    (float) (PointsX[cnt] -
															             VertexStartWidthTemp[cnt] * 0.5);
															NewObjectLine.X2 =
															    (float) (PointsX[cnt + 1] +
															             VertexStartWidthTemp[cnt] * 0.5);
														}
													}
													else
													{	// Trace at any angle
														ConvNormalCoorToPolar(PointsX[cnt], PointsY[cnt],
														                      PointsX[cnt + 1], PointsY[cnt + 1],
														                      &Angle, &Length2);
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
													}
												}

												NewObjectLine.LineThickNess = (float) (VertexStartWidthTemp[cnt] * 2.0);

												if (NewObjectLine.LineThickNess == 0.0)
													NewObjectLine.LineThickNess = Design.SilkScreenWidth;

												LengthLine =
												    CalcLengthLine(NewObjectLine.X1, NewObjectLine.Y1, NewObjectLine.X2,
												                   NewObjectLine.Y2);

												if (LengthLine < 0.01e6)
												{	// Round pad
													NewObjectArc.CentreX = (float) NewObjectLine.X1;
													NewObjectArc.CentreY = (float) NewObjectLine.Y1;
													NewObjectArc.Width = (float) (VertexStartWidthTemp[cnt] * 2.0);
													NewObjectArc.Height = (float) (VertexStartWidthTemp[cnt] * 2.0);
													NewObjectArc.StartDiffX = 0.0;
													NewObjectArc.StartDiffY = NewObjectArc.Width;
													NewObjectArc.EndDiffX = 0.0;
													NewObjectArc.EndDiffY = NewObjectArc.Width;
													NewObjectArc.LineThickNess = 0.0;
													NewObjectArc.Info |= OBJECT_FILLED;
													AddObjectArc(&NewObjectArc);
												}
												else
												{
													NewObjectLine.LineThickNess =
													    (float) (VertexStartWidthTemp[cnt] * 2.0);
													AddObjectLine(&NewObjectLine);
												}
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
													NewObjectRect.Info |= OBJECT_FILLED;
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
														NewObjectRect.Info |= OBJECT_FILLED;
														AddObjectRect(&NewObjectRect);
													}
													else
													{	// Diagonal polygon
														ok = 1;
														MakePolygonFromSpecialLine(PointsX[cnt], PointsY[cnt],
														                           PointsX[cnt + 1], PointsY[cnt + 1],
														                           &NewObjectPolygon,
														                           VertexStartWidthTemp[cnt],
														                           VertexStartWidthTemp[cnt], 0);
														SetMinMaxObjectPolygon(&NewObjectPolygon, 0);
														AddObjectPolygon(&NewObjectPolygon);
													}
												}

												if (AngleBetweenLines[cnt] < ANGLE_CONVERT(179.0))
												{
													NewObjectArc.CentreX = (float) PointsX[cnt];
													NewObjectArc.CentreY = (float) PointsY[cnt];
													NewObjectArc.Width = (float) VertexStartWidthTemp[cnt];
													NewObjectArc.Height = (float) VertexStartWidthTemp[cnt];
													NewObjectArc.LineThickNess = 0.0;
													NewObjectArc.StartDiffX = 0.0;
													NewObjectArc.StartDiffY = NewObjectArc.Width;
													NewObjectArc.EndDiffX = 0.0;
													NewObjectArc.EndDiffY = NewObjectArc.Width;
													NewObjectArc.LineThickNess = 0.0;
													NewObjectArc.Info |= OBJECT_FILLED;
													AddObjectArc(&NewObjectArc);
												}
											}
											else
											{	// Different start and end width -> Make polygon
												ok = 1;
												NewObjectPolygon.NrVertices = 4;

												if (dx == 0.0)
												{	// Vertical aligned polygon
													NewObjectPolygon.Points[0].x =
													    PointsX[cnt] - VertexStartWidthTemp[cnt] * 0.5;
													NewObjectPolygon.Points[0].y = PointsY[cnt];
													NewObjectPolygon.Points[1].x =
													    PointsX[cnt] + VertexStartWidthTemp[cnt] * 0.5;
													NewObjectPolygon.Points[1].y = PointsY[cnt];
													NewObjectPolygon.Points[2].x =
													    PointsX[cnt + 1] + VertexEndWidthTemp[cnt] * 0.5;
													NewObjectPolygon.Points[2].y = PointsY[cnt + 1];
													NewObjectPolygon.Points[3].x =
													    PointsX[cnt + 1] - VertexEndWidthTemp[cnt] * 0.5;
													NewObjectPolygon.Points[3].y = PointsY[cnt + 1];
												}
												else
												{
													if (dy == 0.0)
													{	// Horizontal aligned polygon
														NewObjectPolygon.Points[0].x = PointsX[cnt];
														NewObjectPolygon.Points[0].y =
														    PointsY[cnt] - VertexStartWidthTemp[cnt] * 0.5;
														NewObjectPolygon.Points[1].x = PointsX[cnt];
														NewObjectPolygon.Points[1].y =
														    PointsY[cnt] + VertexStartWidthTemp[cnt] * 0.5;
														NewObjectPolygon.Points[2].x = PointsX[cnt + 1];
														NewObjectPolygon.Points[2].y =
														    PointsY[cnt + 1] + VertexEndWidthTemp[cnt] * 0.5;
														NewObjectPolygon.Points[3].x = PointsX[cnt + 1];
														NewObjectPolygon.Points[3].y =
														    PointsY[cnt + 1] - VertexEndWidthTemp[cnt] * 0.5;
													}
													else
													{	// Diagonal polygon
														ok = 1;
														MakePolygonFromSpecialLine(PointsX[cnt], PointsY[cnt],
														                           PointsX[cnt + 1], PointsY[cnt + 1],
														                           &NewObjectPolygon,
														                           VertexStartWidthTemp[cnt],
														                           VertexStartWidthTemp[cnt], 0);
													}
												}

												SetMinMaxObjectPolygon(&NewObjectPolygon, 0);
												AddObjectPolygon(&NewObjectPolygon);
											}
										}
									}
									else
									{
										NewObjectLine.X1 = (float) PointsX[cnt];
										NewObjectLine.Y1 = (float) PointsY[cnt];
										NewObjectLine.X2 = (float) PointsX[cnt + 1];
										NewObjectLine.Y2 = (float) PointsY[cnt + 1];
										NewObjectLine.LineThickNess = (float) VertexStartWidthTemp[cnt];

										if (NewObjectLine.LineThickNess == 0.0)
											NewObjectLine.LineThickNess = Design.SilkScreenWidth;

#ifdef _DEBUG

										if (CalcLengthLine
										        (NewObjectLine.X1, NewObjectLine.Y1, NewObjectLine.X2,
										         NewObjectLine.Y2) == 0.0)
											ok = 1;

#endif
										AddObjectLine(&NewObjectLine);
									}
								}

								/*
								                if (NrPolylinePoints==3) {
								                  if ((InRangeSpecial(VertexStartWidthTemp[0],VertexStartWidthTemp[1],
								                                     max(0.0000001,(fabs(VertexStartWidthTemp[0])/100.0))))
								                     &&
								                     (InRangeSpecial(VertexStartWidthTemp[1],VertexStartWidthTemp[2],
								                                     max(0.0000001,(fabs(VertexStartWidthTemp[0])/100.0))))
								                     &&
								                     (InRangeSpecial(VertexBulge[0],1.0,0.001))
								                     &&
								                     (InRangeSpecial(VertexBulge[1],1.0,0.001))) {
								                    x1=PointsX[0];
								                    y1=PointsY[0];
								                    x2=PointsX[1];
								                    y2=PointsY[1];

								                    NewObjectArc.CentreX=(x1+x2)*0.5;
								                    NewObjectArc.CentreY=(y1+y2)*0.5;
								                    NewObjectArc.Width=CalcLengthLine(x1,y1,x2,y2);
								                    NewObjectArc.Height=CalcLengthLine(x1,y1,x2,y2);
								                    NewObjectArc.StartDiffX=0.0;
								                    NewObjectArc.StartDiffY=NewObject.x2*0.5;
								                    NewObjectArc.EndDiffX=0.0;
								                    NewObjectArc.EndDiffY=NewObject.x2*0.5;
								                    NewObjectArc.LineThickNess=VertexStartWidthTemp[0];
								                    NewObjectArc.Info&=~OBJECT_FILLED;
								                    AddObjectArc(&NewObjectArc);
								                  }
								                }
								*/
							}

							res = 1;
						}


// ***************************************************************************************************
						if (stricmpOwn(CurrentCommandString, "POLYLINE") == 0)
						{
							strcpy(PolyLineLayerName, LayerName);
// Start of polyline with the parameter:
//    Width
//    PolyLineLayerName
						}

// ***************************************************************************************************
						if ((stricmpOwn(CurrentCommandString, "TRACE") == 0)
						        || (stricmpOwn(CurrentCommandString, "SOLID") == 0))
						{
// filled polygon (4 points)
//    LayerName
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint) && ((LayerCnt = CheckDXFLayer(LayerName, NrSelectedDXFLayers)) != -1))
							{
								for (cnt = 0; cnt < 4; cnt++)
								{
									PointsX[cnt] = UnitsConvert(Units, X[cnt]);
									PointsY[cnt] = UnitsConvert(Units, Y[cnt]);
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
										PointsX[cnt] += UnitsConvert(Units, BlockInsertX[BlockNestingLevel]);
										PointsY[cnt] += UnitsConvert(Units, BlockInsertY[BlockNestingLevel]);
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
											PointsX[cnt] += UnitsConvert(Units, BlockInsertX[0]);
											PointsY[cnt] += UnitsConvert(Units, BlockInsertY[0]);
										}
									}
								}

								for (cnt = 0; cnt < 4; cnt++)
								{
									NewObjectPolygon.Points[cnt].x = PointsX[cnt];
									NewObjectPolygon.Points[cnt].y = PointsY[cnt];
								}

								if (!CheckNoCrossesInObjectPolygon(&NewObjectPolygon))
								{
									res = 1;
									NewObjectPolygon.Points[0].x = PointsX[1];
									NewObjectPolygon.Points[0].y = PointsY[1];
									NewObjectPolygon.Points[1].x = PointsX[0];
									NewObjectPolygon.Points[1].y = PointsY[0];
								}

								SetMinMaxObjectPolygon(&NewObjectPolygon, 0);

								for (cnt = 0; cnt < 4; cnt++)
								{
									cnt2 = (cnt + 1) % 4;
									cnt3 = (cnt + 2) % 4;
									x1 = NewObjectPolygon.Points[cnt].x;
									y1 = NewObjectPolygon.Points[cnt].y;
									x2 = NewObjectPolygon.Points[cnt2].x;
									y2 = NewObjectPolygon.Points[cnt2].y;
									x3 = NewObjectPolygon.Points[cnt3].x;
									y3 = NewObjectPolygon.Points[cnt3].y;
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
									NewObjectRect.CentreX =
									    (float) ((NewObjectPolygon.maxx + NewObjectPolygon.minx) * 0.5);
									NewObjectRect.CentreY =
									    (float) ((NewObjectPolygon.maxy + NewObjectPolygon.miny) * 0.5);
									NewObjectRect.Width = (float) (NewObjectPolygon.maxx - NewObjectPolygon.minx);
									NewObjectRect.Height = (float) (NewObjectPolygon.maxy - NewObjectPolygon.miny);
									NewObjectRect.Info |= OBJECT_FILLED;
									NewObjectRect.LineThickNess = 0.0;
									AddObjectRect(&NewObjectRect);
								}
								else
								{
									NewObjectPolygon.Layer = Layer;
									AddObjectPolygon(&NewObjectPolygon);
								}

							}

							res = 1;
						}

// ***************************************************************************************************
						if (stricmpOwn(CurrentCommandString, "CIRCLE") == 0)
						{
// non filled cirkel with thickness of 0
//    Diameter
//    LayerName
							strcpy(LayerNameToCheck, LayerName);

							if ((StartPoint)
							        && ((LayerCnt = CheckDXFLayer(LayerNameToCheck, NrSelectedDXFLayers)) != -1))
							{
								for (cnt = 0; cnt < 1; cnt++)
								{
									PointsX[cnt] = UnitsConvert(Units, X[cnt]);
									PointsY[cnt] = UnitsConvert(Units, Y[cnt]);
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
										PointsX[cnt] += UnitsConvert(Units, BlockInsertX[BlockNestingLevel]);
										PointsY[cnt] += UnitsConvert(Units, BlockInsertY[BlockNestingLevel]);
									}

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
											PointsX[cnt] += UnitsConvert(Units, BlockInsertX[0]);
											PointsY[cnt] += UnitsConvert(Units, BlockInsertY[0]);
										}
									}
								}

								NewObjectArc.CentreX = (float) PointsX[0];
								NewObjectArc.CentreY = (float) PointsY[0];
								NewObjectArc.LineThickNess = (float) Design.SilkScreenWidth;
								NewObjectArc.Width = (float) UnitsConvert(Units, (Diameter * 2.0));
								NewObjectArc.Height = NewObjectArc.Width;
								NewObjectArc.StartDiffX = 0.0;
								NewObjectArc.StartDiffY = NewObjectArc.Width;
								NewObjectArc.EndDiffX = 0.0;
								NewObjectArc.EndDiffY = NewObjectArc.Width;
								NewObjectArc.Info &= ~OBJECT_FILLED;
								AddObjectArc(&NewObjectArc);
							}

							res = 1;
						}

// ***************************************************************************************************
						if (stricmpOwn(CurrentCommandString, "ARC") == 0)
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
									PointsX[cnt] = UnitsConvert(Units, X[cnt]);
									PointsY[cnt] = UnitsConvert(Units, Y[cnt]);
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
										PointsX[cnt] += UnitsConvert(Units, BlockInsertX[BlockNestingLevel]);
										PointsY[cnt] += UnitsConvert(Units, BlockInsertY[BlockNestingLevel]);
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
											PointsX[cnt] += UnitsConvert(Units, BlockInsertX[0]);
											PointsY[cnt] += UnitsConvert(Units, BlockInsertY[0]);
										}

										Diameter *= BlockInsertScaleY[0];
									}
								}

								NewObjectArc.CentreX = (float) PointsX[0];
								NewObjectArc.CentreY = (float) PointsY[0];
								NewObjectArc.Width = (float) UnitsConvert(Units, (Diameter * 2.0));
								NewObjectArc.Height = (float) UnitsConvert(Units, (Diameter * 2.0));
								NewObjectArc.LineThickNess = (float) Design.SilkScreenWidth;

								if (StartAngle - 0.01 > 360.0)
									StartAngle -= 360.0;

								NewObjectArc.StartDiffX = (float) (cos(ANGLE_CONVERT(StartAngle)) * NewObjectArc.Width);
								NewObjectArc.StartDiffY = (float) (sin(ANGLE_CONVERT(StartAngle)) * NewObjectArc.Width);
								NewObjectArc.EndDiffX = (float) (cos(ANGLE_CONVERT(EndAngle)) * NewObjectArc.Width);
								NewObjectArc.EndDiffY = (float) (sin(ANGLE_CONVERT(EndAngle)) * NewObjectArc.Width);
								NewObjectArc.Info &= ~OBJECT_FILLED;
								AddObjectArc(&NewObjectArc);
							}

							res = 1;
						}

// ***************************************************************************************************
						if (stricmpOwn(CurrentCommandString, "INSERT") == 0)
						{
// Insert block
// Parameters:
//    BlockName
//    Angle
							res = 1;

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
								res = SearchBlockName(AttributeName);
								LineNrP[BlockNestingLevel] = LineNr - 1;
#ifdef _DEBUG

								if (stricmpOwn(AttributeName, "FILTCAP") == 0)
									ok = 0;

#endif

								if (res != -1)
								{
									BlockNr[BlockNestingLevel] = res;
									FilePointerDXF = BlockStartP[res];
									LineNr = BlockLineNr[res];
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
						if (stricmpOwn(CurrentCommandString, "TEXT") == 0)
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
#ifdef _DEBUG

								if (stricmpOwn(TextString, "12-11-1998 15:32") == 0)
									ok = 0;

#endif
								Height *= BlockInsertScaleValueX;
								GetMinMaxText2(PointsX[0], PointsY[0], Height, 0, 0.0, 0, 0, TextString);

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
									PointsX[cnt] = UnitsConvert(Units, X[cnt]);
									PointsY[cnt] = UnitsConvert(Units, Y[cnt]);
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
										PointsX[cnt] += UnitsConvert(Units, BlockInsertX[BlockNestingLevel]);
										PointsY[cnt] += UnitsConvert(Units, BlockInsertY[BlockNestingLevel]);
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
											PointsX[cnt] += UnitsConvert(Units, BlockInsertX[0]);
											PointsY[cnt] += UnitsConvert(Units, BlockInsertY[0]);
										}

										Height *= BlockInsertScaleY[0];
										Angle += BlockAngles[0];
									}
								}

								NewObjectText2.X = (float) PointsX[0];
								NewObjectText2.Y = (float) PointsY[0];
								NewObjectText2.FontHeight = (float) UnitsConvert(Units, Height);
								NewObjectText2.LineThickNess = (float) Design.SilkScreenWidth;
								memset(&NewObjectText2.Text, 0, sizeof(NewObjectText2.Text));
								memmove(&NewObjectText2.Text, TextString,
								        max(strlen(TextString), sizeof(NewObjectText2.Text) - 1));

								if (Angle < 0.0)
									Angle += 360.0;

								if (Angle > 360.0)
									Angle -= 360.0;

								NewObjectText2.TextMode = 0;

								if (TextMirror == 2)
								{
									NewObjectText2.TextMode = 0x10;
									Angle = 360 - Angle;

									if (Angle < 0.0)
										Angle += 360.0;

									if (Angle > 360.0)
										Angle -= 360.0;
								}

								NewObjectText2.Rotation = (float) Angle;
								AddObjectText2(&NewObjectText2);

							}

							res = 1;
						}
					}

// ***************************************************************************************************
// ***************************************************************************************************

					Attrib40 = 0;
					Attrib41 = 0;
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
					TextMirror = 0;
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

					if (stricmpOwn(ValueStr, "SECTION") == 0)
						Nesting++;

					if (stricmpOwn(ValueStr, "SEQEND") == 0)
					{
						// TextLineNr
					}

					if (stricmpOwn(ValueStr, "VERTEX") == 0)
					{
						// TextLineNr
						if (Nesting == 1)
							res = 1;
					}

					if (stricmpOwn(ValueStr, "LINE") == 0)
					{
					}

					if (stricmpOwn(ValueStr, "POLYLINE") == 0)
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

					if ((stricmpOwn(ValueStr, "INSERT") == 0) || (stricmpOwn(ValueStr, "TEXT") == 0))
					{
						// TextLineNr
						BlockInsertScaleValueX = 1.0;
						BlockInsertScaleValueY = 1.0;
					}

					if (stricmpOwn(ValueStr, "ENDSEC") == 0)
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

					if (stricmpOwn(ValueStr, "TABLE") == 0)
						Nesting++;

					if (stricmpOwn(ValueStr, "ENDTAB") == 0)
					{
						Nesting--;

						if (Nesting == 0)
							res = 1;
					}

					if (stricmpOwn(ValueStr, "BLOCK") == 0)
					{
						Nesting++;

						if (BlockNestingLevel == -1)
						{
							BlockStart = 1;
							BlockStartP[NrBlocks] = PreviousFilePos;
							BlockLineNr[NrBlocks] = LineNr - 2;
						}
					}

					if (stricmpOwn(ValueStr, "ENDBLK") == 0)
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
						{
							if (NrBlocks < 256 - 1)
								NrBlocks++;
						}
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
					if (stricmpOwn(CurrentCommandString, "INSERT") == 0)
						strcpy(BlockName, ValueStr);

					if (Nesting == 1)
					{
						if (stricmpOwn(ValueStr, "HEADER") == 0)
						{
							HeaderStart = 1;
							BlockStart = 0;
							BlocksStart = 0;
							StartPoint = 0;
							TableStart = 0;
						}

						if (stricmpOwn(ValueStr, "TABLES") == 0)
						{
							HeaderStart = 0;
							TableStart = 1;
							BlocksStart = 0;
							BlockStart = 0;
							StartPoint = 0;
						}

						if (stricmpOwn(ValueStr, "BLOCKS") == 0)
						{
							HeaderStart = 0;
							TableStart = 0;
							BlocksStart = 1;
							BlockStart = 0;
							StartPoint = 0;
						}

						if (stricmpOwn(ValueStr, "ENTITIES") == 0)
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
						if (stricmpOwn(VarName, "$ANGDIR") == 0)
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

					if (stricmpOwn(CurrentCommandString, "CIRCLE") == 0)
						Diameter = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "ARC") == 0)
						Diameter = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "TEXT") == 0)
						Height = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "POLYLINE") == 0)
						PolyLineStartWidth = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "VERTEX") == 0)
						StartWidth = ValueDouble;

					break;

				case 41:
					Attrib41 = 1;

					if (stricmpOwn(CurrentCommandString, "POLYLINE") == 0)
						PolyLineEndWidth = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "VERTEX") == 0)
						EndWidth = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "INSERT") == 0)
						BlockInsertScaleValueX = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "TEXT") == 0)
						BlockInsertScaleValueX = ValueDouble;

					break;

				case 42:
					Attrib42 = 1;

					if (stricmpOwn(CurrentCommandString, "POLYLINE") == 0)
						Width = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "INSERT") == 0)
						BlockInsertScaleValueY = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "VERTEX") == 0)
						BulgeFlags = ValueInt;

					break;

				case 50:
					Attrib50 = 1;

					if (stricmpOwn(CurrentCommandString, "ARC") == 0)
					{
						StartAngle = ValueDouble;
#ifdef _DEBUG

						if (InRange2(ValueDouble, 45.0))
							res = 1;

#endif
					}

					if (stricmpOwn(CurrentCommandString, "TEXT") == 0)
						Angle = ValueDouble;

					if (stricmpOwn(CurrentCommandString, "INSERT") == 0)
						Angle = ValueDouble;

					break;

				case 51:
					Attrib51 = 1;

					if (stricmpOwn(CurrentCommandString, "ARC") == 0)
						EndAngle = ValueDouble;

					break;

				case 66:
					Attrib66 = 1;
					Attributes = ValueInt;

					if (stricmpOwn(CurrentCommandString, "POLYLINE") == 0)
					{
					}

					break;

				case 70:
					Attrib70 = 1;

					if (stricmpOwn(CurrentCommandString, "POLYLINE") == 0)
						PolyLineFlags = ValueInt;

					break;

				case 71:
					Attrib71 = 1;

					if (stricmpOwn(CurrentCommandString, "TEXT") == 0)
						TextMirror = ValueInt;

					break;

				case 72:
					Attrib72 = 1;

					if (stricmpOwn(CurrentCommandString, "TEXT") == 0)
						TextAlignment = ValueInt;

					break;
				}
			}

			Found++;
		}

		PreviousFilePos = FilePos;
	}

	SetScrollPageSize();
	SetScrollPosition();
	GetMaxRectSelectedObjects(0, &MinX, &MinY, &MaxX, &MaxY);
	ViewFull();
	sprintf(str, SC(365, "min x,min y = %.4f , %.4f : max x,max y = %.4f , %.4f\n\nFor scaling use the right mouse menu"),
	        MinX / 100000.0, MinY / 100000.0, MaxX / 100000.0, MaxY / 100000.0);
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);
	return 0;
}

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

int32 WriteInitDXF(int32 fp, int32 NrLayers)
{
	int32 Layer, ColorNr;
	char str[MAX_LENGTH_STRING];
	double minx, miny, maxx, maxy;

	FindMinMaxBoard(&minx, &miny, &maxx, &maxy, 3);

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
	sprintf(str, "%.4f", minx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.4f", miny);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$EXTMAX");
	WriteLn(fp, "10");
	sprintf(str, "%.4f", maxx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.4f", maxy);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$LIMMIN");
	WriteLn(fp, "10");
	sprintf(str, "%.4f", minx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.4f", miny);
	WriteLn(fp, str);
	WriteLn(fp, "9");
	WriteLn(fp, "$LIMMAX");
	WriteLn(fp, "10");
	sprintf(str, "%.4f", maxx);
	WriteLn(fp, str);
	WriteLn(fp, "20");
	sprintf(str, "%.4f", maxy);
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


int32 SelectExportDXFLayers(int32 mode)
{
	int32 res;

	DialogMode = 1;
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_LAYER), PCBWindow, (DLGPROC) DXFLayerDialogBody);

	if (res == -1)
		return -1;

	return res;
}

// ***************************************************************************************************
// ****************************************** export dxf *********************************************
// ***************************************************************************************************

int32 CALLBACK DXFExportLayerDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, res, ok, NrSelectedLayers, Layer;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;

		SetWindowTextUTF8(Dialog, SC(369, "DXF export"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SetDialogItemTextUTF8(Dialog, IDD_SELECTALL, SC(160, "Select all"));
		SetDialogItemTextUTF8(Dialog, IDD_DESELECTALL, SC(161, "Deselect all"));
		
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(266, "Layers"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(349, "Options"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(367, "Filename"));
		
		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(83, "Component references on silkscreen"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(82, "Component values on silkscreen "));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(368, "Component refs/values separate"));
		
		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(293, "Plot board outline on each layer"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK2, SC(272, "Mirror X"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK3, SC(309, "Objects filled"));
		SetDialogItemTextUTF8(Dialog, IDC_CHECK5, SC(322, "Reverse layer numbering"));

		switch (DXFOptions & 3)
		{
		case 0:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, BST_CHECKED, 0);
			break;

		case 1:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, BST_CHECKED, 0);
			break;

		case 2:
			SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, BST_CHECKED, 0);
			break;
		}

		if ((DXFOptions & 4) == 0)
			SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, BST_CHECKED, 0);
		else
			SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_SETCHECK, BST_UNCHECKED, 0);

		if ((DXFOptions & 8) == 0)
		{	// Mirror X
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, BST_UNCHECKED, 0);
		}
		else
			SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_SETCHECK, BST_CHECKED, 0);

		if ((DXFOptions & 16) == 0)
		{	// Objects filled
			SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_SETCHECK, BST_UNCHECKED, 0);
		}
		else
			SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_SETCHECK, BST_CHECKED, 0);

		strcpy(str, EditFile);
		GetDirFromFileName(str3, EditFile);
		strcat(str3, "\\dxf");
		GetFilePartFromFileName(str2, EditFile);
		CutExtensionFileName(str2);

		if (DirectoryExists(str3) == 0)
			sprintf(ExportFileName, "%s\\%s.dxf", str3, str2);
		else
		{
			GetDirFromFileName(str3, EditFile);
			sprintf(ExportFileName, "%s\\%s.dxf", str3, str2); //pùvodní název souboru
//			sprintf(ExportFileName, "%s\\DXF.dxf", str3, str2); //nový název souboru
		}

		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) ExportFileName);


		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);

		for (cnt = 0; cnt < NrDXFLayers; cnt++)
		{
			Layer = ExportLayers[cnt];

			if (Layer < 32)
			{
				if (!GerberInfo.ReverseLayerNumbering)
					GetLayerText(Layer, DXFLayers[cnt], 8);
				else
					GetLayerText(Layer, DXFLayers[cnt], 16 + 8);
			}
			else
			{
				if (!GerberInfo.ReverseLayerNumbering)
					GetLayerTextObjects(Layer, DXFLayers[cnt], 5);
				else
					GetLayerTextObjects(Layer, DXFLayers[cnt], 16 + 5);
			}
		}

		for (cnt = 0; cnt < NrDXFLayers; cnt++)
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_DESELECTALL:
			for (cnt = 0; cnt < NrDXFLayers; cnt++)
				SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 0, (LPARAM) cnt);

			break;

		case IDD_SELECTALL:
			for (cnt = 0; cnt < NrDXFLayers; cnt++)
				SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETSEL, 1, (LPARAM) cnt);

			break;

		case IDOK:
			NrSelectedLayers = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETSELITEMS, 32, (LPARAM) & SelectedLayers);

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_GETCHECK, 0, 0)) == 1)
				DXFOptions &= ~3;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0)) == 1)
			{
				DXFOptions &= ~3;
				DXFOptions |= 1;
			}

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0)) == 1)
				DXFOptions &= ~4;
			else
				DXFOptions |= 4;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_CHECK2, BM_GETCHECK, 0, 0)) == 1)
			{
				DXFOptions |= 8;	// Mirror X
			}
			else
				DXFOptions &= ~8;

			if ((res = SendDlgItemMessageOwn(Dialog, IDC_CHECK3, BM_GETCHECK, 0, 0)) == 1)
			{
				DXFOptions |= 16;	// Objects filled
			}
			else
				DXFOptions &= ~16;

			ExportFileName[0] = 0;
			SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) ExportFileName);
			EndDialog(Dialog, NrSelectedLayers);
			return about;

		case IDC_CHECK5:
			GerberInfo.ReverseLayerNumbering = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK5, BM_GETCHECK, 0, 0) == 1)
				GerberInfo.ReverseLayerNumbering = 1;

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);

			for (cnt = 0; cnt < NrDXFLayers; cnt++)
			{
				Layer = ExportLayers[cnt];

				if (Layer < 32)
				{
					if (!GerberInfo.ReverseLayerNumbering)
						GetLayerText(Layer, DXFLayers[cnt], 8);
					else
						GetLayerText(Layer, DXFLayers[cnt], 16 + 8);
				}
				else
				{
					if (!GerberInfo.ReverseLayerNumbering)
						GetLayerTextObjects(Layer, DXFLayers[cnt], 5);
					else
						GetLayerTextObjects(Layer, DXFLayers[cnt], 16 + 5);
				}
			}

			for (cnt = 0; cnt < NrDXFLayers; cnt++)
				res = SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DXFLayers[cnt]);

			break;

		case IDC_BUTTON1:
			if (!GetNewFileUTF8
			        (PCBWindow, NULL, ExportFileName, ExportDir, SC(363, "DXF file"), NULL, SC(364, "Import DXF file"), "dxf", 1))
				SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) ExportFileName);

			ok = 1;
			break;

		case IDCANCEL:
			EndDialog(Dialog, -1);
			return about;

		case IDHELP:
			Help("exportDXF.htm", 0);
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

	if ((DXFOptions & 16) == 0)
	{	// Do not fill object
		Thickness = 0.0;
		sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 66\r\n      1\r\n 40\r\n%.4f\r\n 41\r\n%.4f\r\n", Thickness, Thickness);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x2, y2);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
	}
	else
	{	// Filled trace
		sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 66\r\n      1\r\n 40\r\n%.4f\r\n 41\r\n%.4f\r\n", Thickness, Thickness);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x2, y2);
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
			sprintf(str, " 66\r\n1\r\n 40\r\n%.4f\r\n 41\r\n%.4f\r\n 70\r\n1\r\n", x3a, x3a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1a, y1a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x2a, y2a);
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
			sprintf(str, " 66\r\n1\r\n 40\r\n%.4f\r\n 41\r\n%.4f\r\n 70\r\n1\r\n", x3a, x3a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1a, y1a);
			WriteToFile(fp, str);
			sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x2a, y2a);
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
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n 40\r\n%.4f\r\n", x1, y1, x2 * 0.5);
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
		sprintf(str, " 66\r\n1\r\n 40\r\n%.4f\r\n 41\r\n%.4f\r\n 70\r\n1\r\n", x3a, x3a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1a, y1a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n1\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x2a, y2a);
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
		sprintf(str, " 66\r\n1\r\n 40\r\n%.4f\r\n 41\r\n%.4f\r\n 70\r\n1\r\n", Thickness, Thickness);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1a, y1a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x2a, y2a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x3a, y3a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n 42\r\n0\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x4a, y4a);
		WriteToFile(fp, str);
		sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);

	}
	else
	{	// Filled rectangle
		sprintf(str, "  0\r\nTRACE\r\n  8\r\n%s\r\n", DXFLayer);
		WriteToFile(fp, str);
		sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1a, y1a);
		WriteToFile(fp, str);
		sprintf(str, " 11\r\n%.4f\r\n 21\r\n%.4f\r\n 31\r\n0.0\r\n", x2a, y2a);
		WriteToFile(fp, str);
		sprintf(str, " 12\r\n%.4f\r\n 22\r\n%.4f\r\n 32\r\n0.0\r\n", x4a, y4a);
		WriteToFile(fp, str);
		sprintf(str, " 13\r\n%.4f\r\n 23\r\n%.4f\r\n 33\r\n0.0\r\n", x3a, y3a);
		WriteToFile(fp, str);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DXFWriteText(double x1, double y1, double x2, double Rotation, double Thickness, int32 FontNr, int32 Alignment,
                  int32 Mirror, LPSTR Text, LPSTR DXFLayer, int32 fp, int32 mode)
{
	int32 Length, cnt3, cnt5, cnt6, code, DrawChar, lengte, MaxCountX, count, NrLines, cnt4;
	char str[1024];
	double NewRotation, x11, y11, x22, incX, incY, x4, y4, x5, y5;
	PolygonRecord *DrawPolygon;
	AreaFillRecord *AreaFill;
	uint8 *PolygonPos;
	WCHAR *str2;

	if (stricmp(Text, "$DESIGNNAME") == 0)
		Text = DXFLayer;

	Length = strlen(Text);
	NrLines = ConvertObjectTextToStrings(Text, FontNr, &MaxCountX, 0);

	for (cnt3 = 0; cnt3 < NrLines; cnt3++)
	{
		if (FontNr == 0)
		{
			x11 = x1 / 100000.0;
			y11 = y1 / 100000.0;
			x22 = x2 / (100000.0 / 0.75);

			if ((DXFOptions & 8) == 8)
			{	// Mirror X
				x11 = -x11 + DXFminx + DXFmaxx;
			}

			sprintf(str, "  0\r\nTEXT\r\n  8\r\n%s\r\n", DXFLayer);
			WriteToFile(fp, str);
			sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n 40\r\n%.4f\r\n", x11, y11, x22);
			WriteToFile(fp, str);
			sprintf(str, "  1\r\n%s\r\n", TextStrings2[cnt3]);
			WriteToFile(fp, str);
			sprintf(str, " 41\r\n1.06\r\n");
			WriteToFile(fp, str);

			if ((((DXFOptions & 8) == 0) && (Mirror == 1)) || (((DXFOptions & 8) == 8) && (Mirror == 0)))
			{
				NewRotation = 360.0 - Rotation;
				sprintf(str, " 50\r\n%.2f\r\n", NewRotation);
				WriteToFile(fp, str);
				WriteToFile(fp, " 71\r\n2\r\n");
			}
			else
			{
				sprintf(str, " 50\r\n%.2f\r\n", Rotation);
				WriteToFile(fp, str);
			}
		}
		else
		{
			incX = 0.0;
			incY = 0.0;
			x4 = x1;
			y4 = y1;
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
					incX = TRUETYPE_FONT_SPACE_EXTRA_X * x2;
					incY = 0.0;
					RotatePoint2(&incX, &incY, Rotation);
				}

				if (DrawChar)
				{
					incX = (AreaFill->maxx - AreaFill->minx + TRUETYPE_FONT_ADD_EXTRA_X) * x2;
					incY = 0.0;
					RotatePoint2(&incX, &incY, Rotation);
					DrawPolygon = (PolygonRecord *) ((uint8 *) AreaFill + sizeof(AreaFillRecord));
					PolygonPos = (uint8 *) DrawPolygon;

					for (cnt5 = 0; cnt5 < AreaFill->NrPolygons; cnt5++)
					{
						count = DrawPolygon->NrVertices;
						sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

						for (cnt6 = 0; cnt6 < count; cnt6++)
						{
							DrawPolygon->Points[cnt6].x *= x2;
							DrawPolygon->Points[cnt6].y *= x2;
							RotatePoint2(&DrawPolygon->Points[cnt6].x, &DrawPolygon->Points[cnt6].y, Rotation);

							if (Mirror == 1)
								DrawPolygon->Points[cnt6].x = -DrawPolygon->Points[cnt6].x;

							DrawPolygon->Points[cnt6].x += x4;
							DrawPolygon->Points[cnt6].y += y4;
							x5 = (DrawPolygon->Points[cnt6].x / 100000.0);
							y5 = (DrawPolygon->Points[cnt6].y / 100000.0);

							if ((DXFOptions & 8) == 8)
							{	// Mirror X
								x5 = -x5 + DXFminx + DXFmaxx;
							}

							sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x5, y5);
							WriteToFile(fp, str);
						}

						sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						PolygonPos += MemSizePolygon(DrawPolygon);
						DrawPolygon = (PolygonRecord *) PolygonPos;
					}
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
				x1 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
			else
				x1 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.1;

			y1 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.1;
		}
		else
		{
			if (Mirror == 0)
				x1 += sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
			else
				x1 -= sin(ANGLE_CONVERT(Rotation)) * x2 * 1.4;

			y1 -= cos(ANGLE_CONVERT(Rotation)) * x2 * 1.4;
		}
	}



#if 0

	Length = strlen(str2);
	cnt3 = 0;
	cnt2 = cnt3;

	while (cnt3 < Length + 1)
	{
		if ((str2[cnt3] == '\r') || ((cnt3 == Length) && (str2[cnt3 - 1] != '\n')))
		{
			if (cnt3 - cnt2 > 0)
			{
				memset(TextString, 0, sizeof(TextString));
				strncpy(TextString, (LPSTR) & str2[cnt2], min(127, cnt3 - cnt2));

				x11 = x1 / 100000.0;
				y11 = y1 / 100000.0;
				x22 = x2 / (100000.0 / 0.75);

				if ((DXFOptions & 8) == 8)
				{	// Mirror X
					x11 = -x11 + DXFminx + DXFmaxx;
				}

				sprintf(str, "  0\r\nTEXT\r\n  8\r\n%s\r\n", DXFLayer);
				WriteToFile(fp, str);
				sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n 40\r\n%.4f\r\n", x11, y11, x22);
				WriteToFile(fp, str);
#ifdef _DEBUG

				if (stricmpOwn(TextString, "Multiline text line1") == 0)
					res = 1;

#endif
				sprintf(str, "  1\r\n%s\r\n", TextString);
				WriteToFile(fp, str);
				sprintf(str, " 41\r\n1.06\r\n");
				WriteToFile(fp, str);

				if ((((DXFOptions & 8) == 0) && (Mirror == 1)) || (((DXFOptions & 8) == 8) && (Mirror == 0)))
				{
					NewRotation = 360.0 - Rotation;
					sprintf(str, " 50\r\n%.2f\r\n", NewRotation);
					WriteToFile(fp, str);
					WriteToFile(fp, " 71\r\n2\r\n");
				}
				else
				{
					sprintf(str, " 50\r\n%.2f\r\n", Rotation);
					WriteToFile(fp, str);
				}

			}

			if (Mirror == 0)
				x1 += sin(ANGLE_CONVERT(Rotation)) * x2;
			else
				x1 -= sin(ANGLE_CONVERT(Rotation)) * x2;

			y1 -= cos(ANGLE_CONVERT(Rotation)) * x2;
			cnt3 += 1;
			cnt2 = cnt3 + 1;
		}

		cnt3++;
	}

#endif
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ExportDXF(int32 Mode)
{
	int32 cnt, cnt2, cnt3, cnt4, cnt5, cnt6, NrLayers, Layer, SelectMask1, SelectMask2, Mirror, ok, CompOutlineOnBottom,
	      CompOutlineLayers, count, TextRotation, fp, ViaType, SegmentCount, LineSegments;
	double x1, y1, x2, y2, x3, y3, x4, y4, h2, Thickness, LineBuf[128];
	uint8 *AreaPos, *PolygonPos;
	LPSTR CompStr;
	ObjectLineRecord *ObjectLine;
	ObjectRectRecord *ObjectRect;
	ObjectArcRecord *ObjectArc;
	ObjectTextRecord2 *ObjectText2;
	ObjectPolygonRecord *ObjectPolygon;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], DXFLayer[MAX_LENGTH_STRING], InfoCopy[MAX_LENGTH_STRING];
	int32 OkToPrint, LayerOk;
	PolygonRecord *PolygonObject;
	TraceRecord *Trace;
	ViaRecord *Via;
	CompRecord *Comp;
	ObjectRecord *Object, NewObject;
	AreaFillRecord *AreaFill;
	uint8 PolygonBuf[10240];
#ifdef _DEBUG
	int32 res;
#endif

	strcpy(InfoCopy, InfoStr);
	PolygonObject = (PolygonRecord *) & PolygonBuf;
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

	NrDXFLayers = 0;

	if (Design.NrBoardLayers > 1)
	{
		ExportLayers[NrDXFLayers++] = SILKSCREEN_TOP;
		ExportLayers[NrDXFLayers++] = PASTE_MASK_TOP;
		ExportLayers[NrDXFLayers++] = SOLD_MASK_TOP;
	}

	for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
		ExportLayers[NrDXFLayers++] = Design.NrBoardLayers - 1 - cnt;

	for (cnt = 0; cnt < Design.NrBoardLayers; cnt++)
		ExportLayers[NrDXFLayers++] = ROUTING_KEEPOUT_LAYER + Design.NrBoardLayers - 1 - cnt;

	ExportLayers[NrDXFLayers++] = SOLD_MASK_BOTTOM;
	ExportLayers[NrDXFLayers++] = PASTE_MASK_BOTTOM;
	ExportLayers[NrDXFLayers++] = SILKSCREEN_BOTTOM;
	ExportLayers[NrDXFLayers++] = BOARD_OUTLINE_LAYER;
	ExportLayers[NrDXFLayers++] = INFO_LAYER;
	ExportLayers[NrDXFLayers++] = INFO_LAYER2;
	ExportLayers[NrDXFLayers++] = INFO_LAYER3;
	ExportLayers[NrDXFLayers++] = INFO_LAYER4;
	ExportLayers[NrDXFLayers++] = DRILL_LAYER;
	ExportLayers[NrDXFLayers++] = DRILL_UNPLATED_LAYER;
	ExportLayers[NrDXFLayers++] = PLACEMENT_OUTLINE_TOP;
	ExportLayers[NrDXFLayers++] = PLACEMENT_OUTLINE_BOTTOM;
	ExportLayers[NrDXFLayers++] = COMP_OUTLINE_LAYER;
	ExportLayers[NrDXFLayers++] = COMP_OUTLINE_LAYER + 1;
	ExportLayers[NrDXFLayers++] = COMP_REF_LAYER;
	ExportLayers[NrDXFLayers++] = COMP_VALUE_LAYER;


	NrLayers =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_DXF_EXPORT35), PCBWindow,
	                 (DLGPROC) DXFExportLayerDialogBody);

	if (NrLayers <= 0)
		return -1;

// SelectedLayers

	if (FileExistsUTF8(ExportFileName) == 0)
	{
		sprintf(str2, SC(788, "File already exists.\n\n%s\n\nDo you want to overwrite it ?"), ExportFileName); //soubor již existuje

		if (MessageBoxOwn(PCBWindow, str2, SC(1, "Message"), MB_OKCANCEL | MB_APPLMODAL) != IDOK)
			return -1;
	}

	if ((fp = FileOpenWriteUTF8(ExportFileName)) <= 0)
		return -1;

	SetWaitCursor();
	WriteInitDXF(fp, NrLayers);

	WriteToFile(fp, "  0\r\n");
	WriteToFile(fp, "ENDSEC\r\n");
	WriteToFile(fp, "  0\r\nSECTION\r\n  2\r\nENTITIES\r\n");

	SelectMask1 = 0;
	SelectMask2 = 0;
	CompOutlineLayers = 0;

	for (cnt2 = 0; cnt2 < NrLayers; cnt2++)
	{
		Layer = ExportLayers[SelectedLayers[cnt2]];

		switch (Layer)
		{
		case COMP_OUTLINE_LAYER:
		case COMP_OUTLINE_LAYER + 1:
			CompOutlineLayers++;
			break;
		}
	}

	for (cnt2 = 0; cnt2 < NrLayers; cnt2++)
	{
		Layer = ExportLayers[SelectedLayers[cnt2]];

		if (!GerberInfo.ReverseLayerNumbering)
			GetLayerTextObjects(Layer, str, 0);
		else
			GetLayerTextObjects(Layer, str, 16);

		str[0] = (char) tolower(str[0]);
		sprintf(InfoStr, SC(371, "DXF output - %s"), str);
		RedrawInfoStr(1);
		CompOutlineOnBottom = 0;
		strcpy(DXFLayer, DXFLayers[SelectedLayers[cnt2]]);
		ok = 1;

// *******************************************************************************************************
		for (cnt = 0; cnt < Design.NrObjectLines; cnt++)
		{
			ObjectLine = &((*ObjectLines)[cnt]);

			if ((ObjectLine->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
			{
				if ((ObjectLine->Layer == Layer)
				        || ((ObjectLine->Layer == BOARD_OUTLINE_LAYER) && ((DXFOptions & 4) == 0)))
				{
					Thickness = ObjectLine->LineThickNess / 100000.0;

					if (ObjectLine->LineMode != 0)
					{
						x1 = ObjectLine->X1;
						y1 = ObjectLine->Y1;
						x2 = ObjectLine->X2;
						y2 = ObjectLine->Y2;
						LineSegments =
						    DimensionToLineSegments(x1, y1, x2, y2, (double *) &LineBuf, ObjectLine->LineMode);
						SegmentCount = 0;

						for (cnt3 = 0; cnt3 < LineSegments; cnt3++)
						{
							x3 = LineBuf[SegmentCount++] / 100000.0;
							y3 = LineBuf[SegmentCount++] / 100000.0;
							x4 = LineBuf[SegmentCount++] / 100000.0;
							y4 = LineBuf[SegmentCount++] / 100000.0;

							if ((DXFOptions & 16) == 16)
							{	// Filled objects
								DXFWriteTrace(x3, y3, x4, y4, Thickness, DXFLayer, fp, 3);
							}
							else
								DXFWriteTrace(x3, y3, x4, y4, Thickness, DXFLayer, fp, 0);
						}
					}
					else
					{
						x1 = ObjectLine->X1 / 100000.0;
						y1 = ObjectLine->Y1 / 100000.0;
						x2 = ObjectLine->X2 / 100000.0;
						y2 = ObjectLine->Y2 / 100000.0;

						if ((DXFOptions & 16) == 16)
						{	// Filled objects
							DXFWriteTrace(x1, y1, x2, y2, Thickness, DXFLayer, fp, 3);
						}
						else
							DXFWriteTrace(x1, y1, x2, y2, Thickness, DXFLayer, fp, 0);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectRects; cnt++)
		{
			ObjectRect = &((*ObjectRects)[cnt]);

			if ((ObjectRect->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
			{
				if ((ObjectRect->Layer == Layer)
				        || ((ObjectRect->Layer == BOARD_OUTLINE_LAYER) && ((ObjectRect->Info & OBJECT_FILLED) == 0)
				            && ((DXFOptions & 4) == 0)))
				{
					if ((ObjectRect->Info & OBJECT_FILLED) == 0)
					{
						DXFWriteRectangle(ObjectRect->CentreX / 100000, ObjectRect->CentreY / 100000,
						                  ObjectRect->Width / 100000, ObjectRect->Height / 100000,
						                  ObjectRect->LineThickNess / 100000, DXFLayer, fp, 0);
					}
					else
					{
						DXFWriteRectangle(ObjectRect->CentreX / 100000, ObjectRect->CentreY / 100000,
						                  ObjectRect->Width / 100000, ObjectRect->Height / 100000,
						                  ObjectRect->LineThickNess / 100000, DXFLayer, fp, 2);
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
		{
			ObjectArc = &((*ObjectArcs)[cnt]);

			if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
			{
				if ((ObjectArc->Layer == Layer)
				        || ((ObjectArc->Layer == BOARD_OUTLINE_LAYER) && ((ObjectArc->Info & OBJECT_FILLED) == 0)
				            && ((DXFOptions & 4) == 0)))
				{
					if (((ObjectArc->Info & OBJECT_FILLED) != 0)
					        || ((InRange(ObjectArc->StartDiffX, ObjectArc->EndDiffX))
					            && (InRange(ObjectArc->StartDiffY, ObjectArc->EndDiffY))
					            && (InRange(ObjectArc->Width, ObjectArc->Height))))
					{
						if ((ObjectArc->Info & OBJECT_FILLED) != 0)
						{
							DXFWriteCircle(ObjectArc->CentreX / 100000, ObjectArc->CentreY / 100000,
							               ObjectArc->Width / 100000, ObjectArc->LineThickNess / 100000, DXFLayer, fp,
							               2);
						}
						else
						{
							DXFWriteCircle(ObjectArc->CentreX / 100000, ObjectArc->CentreY / 100000,
							               ObjectArc->Width / 100000, ObjectArc->LineThickNess / 100000, DXFLayer, fp,
							               0);
						}
					}
					else
					{
						if (InRange2(ObjectArc->Width, ObjectArc->Height))
						{
							x1 = ObjectArc->CentreX / 100000.0;
							y1 = ObjectArc->CentreY / 100000.0;
							x2 = ObjectArc->Width / 100000.0;

							if ((DXFOptions & 8) == 8)
							{	// Mirror X
								x1 = -x1 + DXFminx + DXFmaxx;
								ConvertPointToPolar(-ObjectArc->EndDiffX, ObjectArc->EndDiffY, &x4, &x3);
								ConvertPointToPolar(-ObjectArc->StartDiffX, ObjectArc->StartDiffY, &x4, &y3);
							}
							else
							{
								ConvertPointToPolar(ObjectArc->StartDiffX, ObjectArc->StartDiffY, &x4, &x3);
								ConvertPointToPolar(ObjectArc->EndDiffX, ObjectArc->EndDiffY, &x4, &y3);
							}

							sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n 40\r\n%.4f\r\n", x1, y1,
							        x2 * 0.5);
							WriteToFile(fp, str);
							sprintf(str, " 50\r\n%.4f\r\n 51\r\n%.4f\r\n", x3, y3);
							WriteToFile(fp, str);
						}
						else
						{
							LineSegments =
							    ArcToLineSegments(ObjectArc->CentreX, ObjectArc->CentreY, ObjectArc->Width,
							                      ObjectArc->Height, ObjectArc->StartDiffX, ObjectArc->StartDiffY,
							                      ObjectArc->EndDiffX, ObjectArc->EndDiffY, (double *) &LineBuf, 1);
							SegmentCount = 0;

							for (cnt5 = 0; cnt5 < LineSegments; cnt5++)
							{
								x1 = LineBuf[SegmentCount] / 100000.0;
								SegmentCount++;
								y1 = LineBuf[SegmentCount] / 100000.0;
								SegmentCount++;
								x2 = LineBuf[SegmentCount] / 100000.0;
								SegmentCount++;
								y2 = LineBuf[SegmentCount] / 100000.0;
								SegmentCount++;
								DXFWriteTrace(x1, y1, x2, y2, ObjectArc->LineThickNess / 100000, DXFLayer, fp, 3);
							}
						}
					}
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectTexts2; cnt++)
		{
			ObjectText2 = &((*ObjectTexts2)[cnt]);

			if ((ObjectText2->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
			{
				if (ObjectText2->Layer == Layer)
				{
					Mirror = (ObjectText2->TextMode & 0x10) >> 4;
					DXFWriteText(ObjectText2->X, ObjectText2->Y, ObjectText2->FontHeight, ObjectText2->Rotation,
					             ObjectText2->LineThickNess, ObjectText2->FontNr, 0, Mirror, ObjectText2->Text,
					             DXFLayer, fp, 0);
				}
			}
		}

		for (cnt = 0; cnt < Design.NrObjectPolygons; cnt++)
		{
			ObjectPolygon = (ObjectPolygonRecord *) & (ObjectPolygonMem[(*ObjectPolygons)[cnt]]);

			if ((ObjectPolygon->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
			{
				if (ObjectPolygon->Layer == Layer)
				{
					sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);
					WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

					for (cnt3 = 0; cnt3 < ObjectPolygon->NrVertices; cnt3++)
					{
						x1 = (ObjectPolygon->Points[cnt3].x / 100000.0);
						y1 = (ObjectPolygon->Points[cnt3].y / 100000.0);

						if ((DXFOptions & 8) == 8)
						{	// Mirror X
							x1 = -x1 + DXFminx + DXFmaxx;
						}

						sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
						WriteToFile(fp, str);
						sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
						WriteToFile(fp, str);
					}

					sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
					WriteToFile(fp, str);
				}
			}
		}

// *******************************************************************************************************
		if ((Layer == DRILL_LAYER) || (Layer == DRILL_UNPLATED_LAYER))
		{
			for (cnt3 = 0; cnt3 < Design.NrComps; cnt3++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt3]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "U100") == 0)
						res = 1;

#endif
					NrObjects = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

					for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
					{
						Object = &((*Objects)[cnt4]);
						x1 = Object->x1 / 100000.0;
						y1 = Object->y1 / 100000.0;
						x2 = Object->x2 / 100000.0;
						y2 = Object->y2 / 100000.0;
						Thickness = Object->Thickness / 100000.0;

						switch (Object->ObjectType)
						{
						case DRILL:
							if (Layer == DRILL_LAYER)
								DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);

							break;

						case DRILL_UNPLATED:
							if (Layer == DRILL_UNPLATED_LAYER)
								DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);

							break;

						case PIN_PUT_THROUGH_ROUND:
						case PIN_PUT_THROUGH_SQUARE:
						case PIN_PUT_THROUGH_POLYGON:
							if (Layer == DRILL_LAYER)
								DXFWriteCircle(x1, y1, y2, 0.0, DXFLayer, fp, 2);

							break;
						}
					}
				}
			}

			if (Layer == DRILL_LAYER)
			{
				for (cnt3 = 0; cnt3 < Design.NrVias; cnt3++)
				{
					Via = &((*Vias)[cnt3]);

					if ((Via->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
					{
						x1 = Via->X / 100000.0;
						y1 = Via->Y / 100000.0;
						x2 = (Via->DrillThickNess) / 100000.0;
						DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
					}
				}
			}

			for (cnt = 0; cnt < Design.NrObjectArcs; cnt++)
			{
				ObjectArc = &((*ObjectArcs)[cnt]);

				if ((ObjectArc->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (ObjectArc->Layer == Layer)
					{
						x1 = ObjectArc->CentreX / 100000.0;
						y1 = ObjectArc->CentreY / 100000.0;
						x2 = (ObjectArc->Width) / 100000.0;
						DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
					}
				}
			}
		}

// *******************************************************************************************************
		if (Layer < 32)
		{
			for (cnt3 = 0; cnt3 < Design.NrVerTraces[Layer]; cnt3++)
			{
				Trace = &((*VerTraces[Layer])[cnt3]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
					x1 = Trace->X / 100000.0;
					x2 = Trace->X / 100000.0;
					y1 = Trace->Y / 100000.0;
					y2 = (Trace->Y + Trace->Length) / 100000.0;
					x3 = Trace->ThickNess / 100000.0;
					DXFWriteTrace(x1, y1, x2, y2, x3, DXFLayer, fp, 3);
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrHorTraces[Layer]; cnt3++)
			{
				Trace = &((*HorTraces[Layer])[cnt3]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
					x1 = Trace->X / 100000.0;
					y1 = Trace->Y / 100000.0;
					x2 = (Trace->X + Trace->Length) / 100000.0;
					y2 = Trace->Y / 100000.0;
					x3 = Trace->ThickNess / 100000.0;
					DXFWriteTrace(x1, y1, x2, y2, x3, DXFLayer, fp, 3);
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrDiag1Traces[Layer]; cnt3++)
			{
				Trace = &((*Diag1Traces[Layer])[cnt3]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
					x1 = Trace->X / 100000.0;
					y1 = Trace->Y / 100000.0;
					x2 = (Trace->X + Trace->Length) / 100000.0;
					y2 = (Trace->Y - Trace->Length) / 100000.0;
					x3 = Trace->ThickNess / 100000.0;
					DXFWriteTrace(x1, y1, x2, y2, x3, DXFLayer, fp, 3);
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrDiag2Traces[Layer]; cnt3++)
			{
				Trace = &((*Diag2Traces[Layer])[cnt3]);

				if ((Trace->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
					x1 = Trace->X / 100000.0;
					y1 = Trace->Y / 100000.0;
					x2 = (Trace->X + Trace->Length) / 100000.0;
					y2 = (Trace->Y + Trace->Length) / 100000.0;
					x3 = Trace->ThickNess / 100000.0;
					DXFWriteTrace(x1, y1, x2, y2, x3, DXFLayer, fp, 3);
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrVias; cnt3++)
			{
				Via = &((*Vias)[cnt3]);

				if ((Via->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
					x1 = Via->X / 100000.0;
					y1 = Via->Y / 100000.0;
					x2 = Via->ThickNess / 100000.0;
					DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrComps; cnt3++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt3]]);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "U100") == 0)
						res = 1;

#endif
					NrObjects = 0;
					ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 1);

					for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
					{
						Object = &((*Objects)[cnt4]);
						x1 = Object->x1 / 100000.0;
						y1 = Object->y1 / 100000.0;
						x2 = Object->x2 / 100000.0;
						y2 = Object->y2 / 100000.0;
						x3 = Object->x3 / 100000.0;
						Thickness = Object->Thickness / 100000.0;

						switch (Object->ObjectType)
						{
						case PIN_SMD_ROUND:
							if (Object->Layer == Layer)
								DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);

							break;

						case PIN_PUT_THROUGH_ROUND:
							if ((Layer > 0) && (Layer < Design.NrBoardLayers - 1))
							{
								if (Object->x3 != 0)
									x2 = Object->x3 / 100000.0;
							}

							DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
							break;

						case DRILL:
							DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
							break;

						case PIN_PUT_THROUGH_SQUARE:
							if ((Layer > 0) && (Layer < Design.NrBoardLayers - 1))
							{
								if (Object->x3 != 0)
									x2 = Object->x3 / 100000.0;

								DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
							}
							else
								DXFWriteRectangle(x1, y1, x2, x2, 0.0, DXFLayer, fp, 2);

							break;

						case PIN_PUT_THROUGH_POLYGON:
							if ((Layer > 0) && (Layer < Design.NrBoardLayers - 1))
							{
								if (Object->x3 != 0)
									x2 = Object->x3 / 100000.0;

								DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
							}
							else
							{
								if (CheckObjectIsBigPolygon(Object))
								{
									GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
									AreaPos = (uint8 *) AreaFill;
									PolygonObject = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
									PolygonPos = (uint8 *) PolygonObject;

									for (cnt6 = 0; cnt6 < AreaFill->NrPolygons; cnt6++)
									{
										count = PolygonObject->NrVertices;

										if ((PolygonObject->PolygonType & 8) == 0)
										{
											sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
											WriteToFile(fp, str);
											WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

											for (cnt5 = 0; cnt5 < PolygonObject->NrVertices; cnt5++)
											{
												x1 = (PolygonObject->Points[cnt5].x / 100000.0);
												y1 = (PolygonObject->Points[cnt5].y / 100000.0);

												if ((DXFOptions & 8) == 8)
												{	// Mirror X
													x1 = -x1 + DXFminx + DXFmaxx;
												}

												sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
												WriteToFile(fp, str);
												sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
												WriteToFile(fp, str);
											}

											sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
											WriteToFile(fp, str);
										}

										PolygonPos += MemSizePolygon(PolygonObject);
										PolygonObject = (PolygonRecord *) PolygonPos;
									}
								}
								else
								{
									MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
									sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
									WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

									for (cnt5 = 0; cnt5 < PolygonObject->NrVertices; cnt5++)
									{
										x1 = (PolygonObject->Points[cnt5].x / 100000.0);
										y1 = (PolygonObject->Points[cnt5].y / 100000.0);

										if ((DXFOptions & 8) == 8)
										{	// Mirror X
											x1 = -x1 + DXFminx + DXFmaxx;
										}

										sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
										WriteToFile(fp, str);
										sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
										WriteToFile(fp, str);
									}

									sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
								}
							}

							break;

						case PIN_SMD_RECT:
							if (Object->Layer == Layer)
								DXFWriteRectangle(x1, y1, x2, y2, 0.0, DXFLayer, fp, 2);

							break;

						case PIN_SMD_POLYGON:
							if (Object->Layer == Layer)
							{
								if (CheckObjectIsBigPolygon(Object))
								{
									GetAreaFillFromBigPolygonObject(Object, &AreaFill, 0.0, 0);
									AreaPos = (uint8 *) AreaFill;
									PolygonObject = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
									PolygonPos = (uint8 *) PolygonObject;

									for (cnt6 = 0; cnt6 < AreaFill->NrPolygons; cnt6++)
									{
										count = PolygonObject->NrVertices;

										if ((PolygonObject->PolygonType & 8) == 0)
										{
											sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
											WriteToFile(fp, str);
											WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

											for (cnt5 = 0; cnt5 < PolygonObject->NrVertices; cnt5++)
											{
												x1 = (PolygonObject->Points[cnt5].x / 100000.0);
												y1 = (PolygonObject->Points[cnt5].y / 100000.0);

												if ((DXFOptions & 8) == 8)
												{	// Mirror X
													x1 = -x1 + DXFminx + DXFmaxx;
												}

												sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
												WriteToFile(fp, str);
												sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
												WriteToFile(fp, str);
											}

											sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
											WriteToFile(fp, str);
										}

										PolygonPos += MemSizePolygon(PolygonObject);
										PolygonObject = (PolygonRecord *) PolygonPos;
									}
								}
								else
								{
									sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
									WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");
									PolygonObject = (PolygonRecord *) & PolygonBuf;
									MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

									for (cnt5 = 0; cnt5 < PolygonObject->NrVertices; cnt5++)
									{
										x1 = (PolygonObject->Points[cnt5].x / 100000.0);
										y1 = (PolygonObject->Points[cnt5].y / 100000.0);

										if ((DXFOptions & 8) == 8)
										{	// Mirror X
											x1 = -x1 + DXFminx + DXFmaxx;
										}

										sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
										WriteToFile(fp, str);
										sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
										WriteToFile(fp, str);
									}

									sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
								}
							}

							break;

						case OBJECT_POLYGON:
							if (Object->Layer == Layer)
							{
								sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
								WriteToFile(fp, str);
								WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

								if ((Object->ObjectType2 != 0) || (Object->Address != 0))
								{
									PolygonObject = (PolygonRecord *) & PolygonBuf;
									MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);

									for (cnt5 = 0; cnt5 < PolygonObject->NrVertices; cnt5++)
									{
										x1 = (PolygonObject->Points[cnt5].x / 100000.0);
										y1 = (PolygonObject->Points[cnt5].y / 100000.0);

										if ((DXFOptions & 8) == 8)
										{	// Mirror X
											x1 = -x1 + DXFminx + DXFmaxx;
										}

										sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
										WriteToFile(fp, str);
										sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
										WriteToFile(fp, str);
									}

									sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
								}
								else
								{
									ObjectPolygon =
									    (ObjectPolygonRecord *) &
									    (ObjectPolygonMem[(*ObjectPolygons)[Object->TraceNr]]);
									count = ObjectPolygon->NrVertices;

									for (cnt5 = 0; cnt5 < count; cnt5++)
									{
										x1 = (*ObjectPolygon).Points[cnt5].x / 100000.0;
										y1 = (*ObjectPolygon).Points[cnt5].y / 100000.0;
										sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
										WriteToFile(fp, str);
										sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
										WriteToFile(fp, str);
									}

									sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
								}
							}

							break;

						case PIN_ARC:
						case OBJECT_ARC:
							if (Object->Layer == Layer)
							{
								if ((DXFOptions & 8) == 8)
								{	// Mirror X
									x1 = -x1 + DXFminx + DXFmaxx;
								}

								if ((InRange(Object->x3, Object->x4)) && (InRange(Object->y3, Object->y4))
								        && (InRange(Object->x2, Object->y2)))
									DXFWriteCircle(x1, y1, x2, Thickness, DXFLayer, fp, 0);
								else
								{
									if (InRange(Object->x2, Object->y2))
									{
										x2 = (Object->x2 * 0.5) / 100000.0;

										if ((DXFOptions & 8) == 8)
										{	// Mirror X
											ConvertPointToPolar(-Object->x4, Object->y4, &x4, &x3);
											ConvertPointToPolar(-Object->x3, Object->y3, &x4, &y3);
										}
										else
										{
											ConvertPointToPolar(Object->x3, Object->y3, &x4, &x3);
											ConvertPointToPolar(Object->x4, Object->y4, &x4, &y3);
										}

										sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
										WriteToFile(fp, str);
										sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n 40\r\n%.4f\r\n", x1,
										        y1, x2);
										WriteToFile(fp, str);
										sprintf(str, " 50\r\n%.4f\r\n 51\r\n%.4f\r\n", x3, y3);
										WriteToFile(fp, str);
									}
									else
									{
										LineSegments =
										    ArcToLineSegments(Object->x1, Object->y1, Object->x2, Object->y2,
										                      Object->x3, Object->y3, Object->x4, Object->y4,
										                      (double *) &LineBuf, 1);
										SegmentCount = 0;

										for (cnt5 = 0; cnt5 < LineSegments; cnt5++)
										{
											x1 = LineBuf[SegmentCount] / 100000.0;
											SegmentCount++;
											y1 = LineBuf[SegmentCount] / 100000.0;
											SegmentCount++;
											x2 = LineBuf[SegmentCount] / 100000.0;
											SegmentCount++;
											y2 = LineBuf[SegmentCount] / 100000.0;
											SegmentCount++;
											DXFWriteTrace(x1, y1, x2, y2, 0.0, DXFLayer, fp, 3);
										}
									}
								}
							}

							break;

						case OBJECT_LINE:
						case PIN_LINE_HOR:	// pin line hor
						case PIN_LINE_VER:	// pin line ver
						case PIN_LINE_DIAG1:	// pin line diag1
						case PIN_LINE_DIAG2:	// pin line diag2
						case PIN_LINE_ALL_ANGLE:
							x1 = 0.0;
							y1 = 0.0;

							if (Object->Layer == Layer)
							{
								switch (Object->ObjectType)
								{
								case PIN_LINE_HOR:	// pin line hor
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1 + Object->x2) / 100000.0;
									y2 = (Object->y1) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_VER:	// pin line ver
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1) / 100000.0;
									y2 = (Object->y1 + Object->x2) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_DIAG1:	// pin line diag1
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1 + Object->x2) / 100000.0;
									y2 = (Object->y1 - Object->x2) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_DIAG2:	// pin line diag2
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1 + Object->x2) / 100000.0;
									y2 = (Object->y1 + Object->x2) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_ALL_ANGLE:
								case OBJECT_LINE:
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = Object->x2 / 100000.0;
									y2 = Object->y2 / 100000.0;
									x3 = Object->Thickness / 100000.0;
									break;
								}

								DXFWriteTrace(x1, y1, x2, y2, x3, DXFLayer, fp, 3);
							}

							break;
						}
					}
				}
			}

			for (cnt3 = 0; cnt3 < Design.NrAreaFills; cnt3++)
			{
				AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt3]]);

				if (((AreaFill->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				        && (AreaFill->Layer == Layer))
				{
					AreaPos = (uint8 *) AreaFill;
					PolygonObject = (PolygonRecord *) (AreaPos + sizeof(AreaFillRecord));
					PolygonPos = (uint8 *) PolygonObject;

					for (cnt4 = 0; cnt4 < AreaFill->NrPolygons; cnt4++)
					{
						count = PolygonObject->NrVertices;

						if ((PolygonObject->PolygonType & 8) == 0)
						{
							sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
							WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

							for (cnt5 = 0; cnt5 < PolygonObject->NrVertices; cnt5++)
							{
								x1 = (PolygonObject->Points[cnt5].x / 100000.0);
								y1 = (PolygonObject->Points[cnt5].y / 100000.0);

								if ((DXFOptions & 8) == 8)
								{	// Mirror X
									x1 = -x1 + DXFminx + DXFmaxx;
								}

								sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
								WriteToFile(fp, str);
								sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
								WriteToFile(fp, str);
							}

							sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
							WriteToFile(fp, str);
						}

						PolygonPos += MemSizePolygon(PolygonObject);
						PolygonObject = (PolygonRecord *) PolygonPos;
					}
				}
			}
		}

// *******************************************************************************************************
// *******************************************************************************************************

		switch (Layer)
		{
		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
			for (cnt3 = 0; cnt3 < Design.NrVias; cnt3++)
			{
				Via = &((*Vias)[cnt3]);
				ViaType = Via->ViaType & 3;

				if (Layer == SOLD_MASK_BOTTOM)
					ViaType &= ~VIA_SOLDMASK_BOTTOM;

				if (Layer == SOLD_MASK_TOP)
					ViaType &= ~VIA_SOLDMASK_TOP;

				if (((Via->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2) && (ViaType == 0))
				{
					x1 = Via->X / 100000.0;
					y1 = Via->Y / 100000.0;
					x2 = Via->ThickNess / 100000.0;
					DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
				}
			}

			break;
		}

		OkToPrint = 0;

		switch (Layer)
		{
		case SILKSCREEN_BOTTOM:
		case SILKSCREEN_TOP:
		case PLACEMENT_OUTLINE_TOP:
		case PLACEMENT_OUTLINE_BOTTOM:
		case PLACEMENT_OUTLINE_TOP2:
		case PLACEMENT_OUTLINE_BOTTOM2:
		case COMP_OUTLINE_LAYER_TOP:
		case COMP_OUTLINE_LAYER_BOTTOM:
		case COMP_REF_LAYER:
		case COMP_VALUE_LAYER:
		case SOLD_MASK_BOTTOM:
		case SOLD_MASK_TOP:
		case PASTE_MASK_BOTTOM:
		case PASTE_MASK_TOP:
		case INFO_LAYER:
		case INFO_LAYER2:
		case INFO_LAYER3:
		case INFO_LAYER4:
		case BOARD_OUTLINE_LAYER:
		case DRILL_LAYER:
		case DRILL_UNPLATED_LAYER:
			OkToPrint = 1;
			break;

		default:
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
				OkToPrint = 1;

			if (Layer < 32)
				OkToPrint = 1;

			break;
		}

		if (OkToPrint)
		{
			for (cnt3 = 0; cnt3 < Design.NrComps; cnt3++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt3]]);
				Mirror = ((Comp->CompMode & 8) >> 3);

				if ((Comp->Info & (OBJECT_NOT_VISIBLE | SelectMask1)) == SelectMask2)
				{
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "Z100") == 0)
						res = 1;

#endif
					/*
					          switch (Layer) {
					            case COMP_OUTLINE_LAYER:
					              CompOutlineOnBottom=0;
					              if (CompOutlineLayers==2) {
					                if (GetComponentPinLayer(Comp)==0) {
					                  CompOutlineOnBottom=1;
					                }
					              }
					              break;
					            case COMP_OUTLINE_LAYER+1:
					              CompOutlineOnBottom=1;
					              if (CompOutlineLayers==2) {
					                if (GetComponentPinLayer(Comp)==0) {
					                  CompOutlineOnBottom=0;
					                }
					              }
					              break;
					          }
					*/
					NrObjects = 0;

					switch (Layer)
					{
					case SILKSCREEN_BOTTOM:
					case SILKSCREEN_TOP:
						ShapeCompSilkScreenToObject(Comp, 0.0, 0.0, 0);
						ShapeOtherToObject(Comp, 0.0, 0.0, 0.0, -1, 5);
						break;

					case PLACEMENT_OUTLINE_TOP:
					case PLACEMENT_OUTLINE_BOTTOM:
					case PLACEMENT_OUTLINE_TOP2:
					case PLACEMENT_OUTLINE_BOTTOM2:
						ShapePlacementOutLineToObject(Comp, 0.0, 0.0, 0);
						break;

					case COMP_OUTLINE_LAYER_TOP:
					case COMP_OUTLINE_LAYER_BOTTOM:
						ShapeCompOutLineToObject(Comp, 0.0, 0.0, 0);
						break;

					case SOLD_MASK_BOTTOM:
					case SOLD_MASK_TOP:
					case PASTE_MASK_BOTTOM:
					case PASTE_MASK_TOP:
						ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 1);
						break;

					case INFO_LAYER:
					case INFO_LAYER2:
					case INFO_LAYER3:
					case INFO_LAYER4:
						ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 2);
						break;

					case BOARD_OUTLINE_LAYER:
						if ((DXFOptions & 4) == 4)
							ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 3);

						break;

					default:
						if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
							ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 4);

						break;
					}

					if ((DXFOptions & 4) == 0)
					{
						ShapeOtherToObject(Comp, 0.0, 0.0, 0, -1, 3);	// Board outline
					}

					for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
					{
						Object = &((*Objects)[cnt4]);
#ifdef _DEBUG

						if (Object->ObjectType == OBJECT_TEXT)
							ok = 1;

#endif
						LayerOk = 0;

						switch (Layer)
						{
						/*
						              case COMP_OUTLINE_LAYER:
						              case COMP_OUTLINE_LAYER+1:
						                if ((CompOutlineLayers==1)
						                   ||
						                   ((CompOutlineOnBottom==0)
						                   &&
						                   (Layer==COMP_OUTLINE_LAYER))
						                   ||
						                   ((CompOutlineOnBottom==1)
						                   &&
						                   (Layer==COMP_OUTLINE_LAYER+1))) {
						                  LayerOk=1;
						                }
						                if ((Object->Layer==BOARD_OUTLINE_LAYER)
						                   &&
						                   ((DXFOptions & 4) == 0)) {
						                  LayerOk=1;
						                }
						                break;
						*/
						default:
							if (((Object->Layer == BOARD_OUTLINE_LAYER) && ((DXFOptions & 4) == 0))
							        || (Object->Layer == Layer))
								LayerOk = 1;

							break;
						}

						if (LayerOk)
						{
							x1 = Object->x1 / 100000.0;
							y1 = Object->y1 / 100000.0;
							x2 = Object->x2 / 100000.0;
							y2 = Object->y2 / 100000.0;
							Thickness = Object->Thickness / 100000.0;

							switch (Object->ObjectType)
							{
							case OBJECT_LINE:
								DXFWriteTrace(x1, y1, x2, y2, Thickness, DXFLayer, fp, 3);
								break;

							case OBJECT_RECT:
								if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
									DXFWriteRectangle(x1, y1, x2, y2, 0.0, DXFLayer, fp, 2);
								else
									DXFWriteRectangle(x1, y1, x2, y2, Thickness, DXFLayer, fp, 0);

								break;

							case OBJECT_POLYGON:
								PolygonObject = (PolygonRecord *) & PolygonBuf;
								MakePolygonFromObject(Object, PolygonObject, 0.0, 0.0, 1, 1);
								sprintf(str, "  0\r\nPOLYLINE\r\n  8\r\n%s\r\n", DXFLayer);
								WriteToFile(fp, str);
								WriteToFile(fp, " 66\r\n      1\r\n 70\r\n      1\r\n");

								for (cnt5 = 0; cnt5 < PolygonObject->NrVertices; cnt5++)
								{
									x1 = (PolygonObject->Points[cnt5].x / 100000.0);
									y1 = (PolygonObject->Points[cnt5].y / 100000.0);

									if ((DXFOptions & 8) == 8)
									{	// Mirror X
										x1 = -x1 + DXFminx + DXFmaxx;
									}

									sprintf(str, "  0\r\nVERTEX\r\n  8\r\n%s\r\n", DXFLayer);
									WriteToFile(fp, str);
									sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n", x1, y1);
									WriteToFile(fp, str);
								}

								sprintf(str, "  0\r\nSEQEND\r\n  8\r\n%s\r\n", DXFLayer);
								WriteToFile(fp, str);
								break;

							case PIN_LINE_HOR:
							case TRACE_HOR:
							case PIN_LINE_VER:
							case TRACE_VER:
							case PIN_LINE_DIAG1:
							case TRACE_DIAG1:
							case PIN_LINE_DIAG2:
							case TRACE_DIAG2:
							case PIN_LINE_ALL_ANGLE:
								x1 = 0.0;
								y1 = 0.0;

								switch (Object->ObjectType)
								{
								case PIN_LINE_HOR:
								case TRACE_HOR:
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1 + Object->x2) / 100000.0;
									y2 = (Object->y1) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_VER:
								case TRACE_VER:
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1) / 100000.0;
									y2 = (Object->y1 + Object->x2) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_DIAG1:
								case TRACE_DIAG1:
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1 + Object->x2) / 100000.0;
									y2 = (Object->y1 - Object->x2) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_DIAG2:
								case TRACE_DIAG2:
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = (Object->x1 + Object->x2) / 100000.0;
									y2 = (Object->y1 + Object->x2) / 100000.0;
									x3 = Object->y2 / 100000.0;
									break;

								case PIN_LINE_ALL_ANGLE:
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;
									x2 = Object->x2 / 100000.0;
									y2 = Object->y2 / 100000.0;
									x3 = Object->Thickness / 100000.0;
									break;
								}

								DXFWriteTrace(x1, y1, x2, y2, x3, DXFLayer, fp, 3);
								break;

							case OBJECT_CIRCLE:
								if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
									DXFWriteCircle(x1, y1, x2 * 0.5, 0.0, DXFLayer, fp, 2);
								else
								{
									if (((uint8) Object->y2 == 15) || ((uint8) Object->y2 == 0))
										DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 0);
									else
									{
										x1 = Object->x1 / 100000.0;
										y1 = Object->y1 / 100000.0;

										if ((DXFOptions & 8) == 8)
										{	// Mirror X
											x1 = -x1 + DXFminx + DXFmaxx;
										}

										x2 = (Object->x2 * 0.5) / 100000.0;
										sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
										WriteToFile(fp, str);
										sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n 40\r\n%.4f\r\n", x1,
										        y1, x2);
										WriteToFile(fp, str);

										switch ((uint8) Object->y2)
										{
										case 1:
											x3 = 0.0;
											y3 = 90.0;
											break;

										case 2:
											x3 = 270.0;
											y3 = 0.0;
											break;

										case 3:
											x3 = 270.0;
											y3 = 90.0;
											break;

										case 4:
											x3 = 180.0;
											y3 = 280.0;
											break;

										case 6:
											x3 = 180.0;
											y3 = 0.0;
											break;

										case 8:
											x3 = 90.0;
											y3 = 180.0;
											break;

										case 9:
											x3 = 0.0;
											y3 = 180.0;
											break;

										case 12:
											x3 = 90.0;
											y3 = 270.0;
											break;
										}

										if ((DXFOptions & 8) == 8)
										{	// Mirror X
											x3 = 180.0 - x3;
											y3 = 180.0 - y3;

											if (x3 < 0.0)
												x3 += 360.0;

											if (y3 < 0.0)
												y3 += 360.0;
										}

										sprintf(str, " 50\r\n%.4f\r\n 51\r\n%.4f\r\n", x3, y3);
										WriteToFile(fp, str);
									}
								}

								break;

							case OBJECT_ARC:
							case PIN_ARC:
								if ((Object->Info & OBJECT_FILLED) == OBJECT_FILLED)
									DXFWriteCircle(x1, y1, x2, 0.0, DXFLayer, fp, 2);
								else
								{
									x1 = Object->x1 / 100000.0;
									y1 = Object->y1 / 100000.0;

									if ((InRange(Object->x3, Object->x4)) && (InRange(Object->y3, Object->y4))
									        && (InRange(Object->x2, Object->y2)))
										DXFWriteCircle(x1, y1, x2, Thickness, DXFLayer, fp, 2);
									else
									{
										if (InRange(Object->x2, Object->y2))
										{
											if ((DXFOptions & 8) == 8)
											{	// Mirror X
												x1 = -x1 + DXFminx + DXFmaxx;
											}

											ConvertPointToPolar(Object->x3, Object->y3, &x4, &x3);
											ConvertPointToPolar(Object->x4, Object->y4, &x4, &y3);

											if (x3 == y3)
												y3 -= 0.1;

											if ((DXFOptions & 8) == 8)
											{	// Mirror X
												x3 = 180.0 - y3;
												y3 = 180.0 - x3;

												if (x3 < 0.0)
													x3 += 360.0;

												if (y3 < 0.0)
													y3 += 360.0;
											}

											sprintf(str, "  0\r\nARC\r\n  8\r\n%s\r\n", DXFLayer);
											WriteToFile(fp, str);
											sprintf(str, " 10\r\n%.4f\r\n 20\r\n%.4f\r\n 30\r\n0.0\r\n 40\r\n%.4f\r\n",
											        x1, y1, x2 * 0.5);
											WriteToFile(fp, str);
											sprintf(str, " 50\r\n%.4f\r\n 51\r\n%.4f\r\n", x3, y3);
											WriteToFile(fp, str);
										}
										else
										{
											LineSegments =
											    ArcToLineSegments(Object->x1, Object->y1, Object->x2, Object->y2,
											                      Object->x3, Object->y3, Object->x4, Object->y4,
											                      (double *) &LineBuf, 1);
											SegmentCount = 0;

											for (cnt5 = 0; cnt5 < LineSegments; cnt5++)
											{
												x1 = LineBuf[SegmentCount] / 100000.0;
												SegmentCount++;
												y1 = LineBuf[SegmentCount] / 100000.0;
												SegmentCount++;
												x2 = LineBuf[SegmentCount] / 100000.0;
												SegmentCount++;
												y2 = LineBuf[SegmentCount] / 100000.0;
												SegmentCount++;
												DXFWriteTrace(x1, y1, x2, y2, 0.0, DXFLayer, fp, 3);
											}
										}
									}
								}

								break;

							case OBJECT_TEXT:
								Mirror = Object->Mirror;
								DXFWriteText(Object->x1, Object->y1, Object->x2, Object->RotationAngle, 0.0,
								             Object->Test >> 16, 0, Mirror, (LPSTR) Object->TraceNr, DXFLayer, fp, 0);
								break;
							}
						}
					}

// *******************************************************************************************************
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "U100") == 0)
						res = 1;

#endif
					LayerOk = 0;

					switch (Layer)
					{
					case SILKSCREEN_BOTTOM:
						if ((DXFOptions & 3) == 0)
						{	// Reference on silkscreen
							if (Mirror == 1)
							{
								if (MakeObjectFromCompRef(Comp, &NewObject, 0) == 0)
									LayerOk = 1;
							}
						}
						else
						{	// Value on silkscreen
							if (Mirror == 1)
							{
								if (MakeObjectFromCompValue(Comp, &NewObject, 0) == 0)
									LayerOk = 1;
							}
						}

						break;

					case SILKSCREEN_TOP:
#ifdef _DEBUG
						if (stricmpOwn(Comp->Name, "U100") == 0)
							res = 1;

#endif

						if ((DXFOptions & 3) == 0)
						{	// Reference on silkscreen
							if (Mirror == 0)
							{
								if (MakeObjectFromCompRef(Comp, &NewObject, 0) == 0)
									LayerOk = 1;
							}
						}
						else
						{	// Value on silkscreen
							if (Mirror == 0)
							{
								if (MakeObjectFromCompValue(Comp, &NewObject, 0) == 0)
									LayerOk = 1;
							}
						}

						break;

					case COMP_REF_LAYER:
						if (MakeObjectFromCompRef(Comp, &NewObject, 0) == 0)
							LayerOk = 1;

						break;

					case COMP_VALUE_LAYER:
						if (MakeObjectFromCompValue(Comp, &NewObject, 0) == 0)
							LayerOk = 1;

						break;
					}

					if (LayerOk)
					{
						Mirror = NewObject.Mirror;
						DXFWriteText(NewObject.x1, NewObject.y1, NewObject.x2, NewObject.RotationAngle, 0.0, 0, 0,
						             Mirror, (LPSTR) NewObject.TraceNr, DXFLayer, fp, 0);
					}
				}
			}
		}
	}

	/*
	  0
	ENDSEC
	  0
	EOF
	*/
	WriteToFile(fp, "  0\r\nENDSEC\r\n  0\r\nEOF\r\n");

	FileClose(fp);
	SetNormalCursor();

	strcpy(InfoStr, InfoCopy);
	RedrawInfoStr(1);

	sprintf(str, SC(452, "DXF output ready.\n\nOutput file is\n\n%s"), ExportFileName); //pøidána zpráva kam se uložilo
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK); //pøidána zpráva kam se uložilo

	return 0;
}
