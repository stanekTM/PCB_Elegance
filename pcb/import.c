/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: import.c
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
#include "memory.h"
#include "string.h"
#include "insdel.h"
#include "calc.h"
#include "calcdef.h"
#include "graphics.h"
#include "dialogs.h"
#include "draw.h"
#include "draw2.h"
#include "pcb.h"
#include "calc2.h"
#include "calc3.h"
#include "nets.h"
#include "stdio.h"
#include "files.h"
#include "files2.h"
#include "import.h"
#include "mainloop.h"
#include "polygon.h"
#include "select3.h"
#include "gerber.h"
#include "plot.h"
#include "resource.h"
#include "ctype.h"


#define   COMPONENT_EXIST     0x0800

typedef struct
{
	int32 Width, Height, Layer, DrillLayer, Info, NrLineBytes, BitmapScaled;
	double Rotation, Scale;
	int32 Unused[12];
	double Xoffset, Yoffset, StandardResolution, HResolution, VResolution, Other;
	double CenterX, CenterY;
	char FileName[MAX_LENGTH_STRING];
} SpecialBitmapRecord;


extern char *TextBuf;

typedef int16 CompInfoArray[50000];
typedef int16 CompInfoPosArray[10000];

NetArray *Nets2;
HGLOBAL Nets2Global;

CompInfoPosArray *CompInfoPos, *CompInfoPos2;
CompInfoArray *CompInfo;
OPENFILENAME FileInputInfo2;

int32 *NrNetItemsP, MaxNrNets2, NrNets2, NrCurrentNetItems, NetPinsConnected, NetPinsNotConnected;

int32 DebugMode = 0;

int32 ReadNetList2(LPSTR FileName, int32 mode);

int Debugfp2;

#define MaxBitmapMemory  (8*1024*1024)
uint8 BitmapMem[MaxBitmapMemory];

extern int32 WriteLnError;
extern int32 ProjectIndexNr;
extern ProjectInfoRecord *ProjectInfo;
extern int32 TimerUpdateNetlist;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ReArrangeTracesVias(void);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AllocateMemNets2(int32 count)
{
	HGLOBAL NewMem;

	if (count > 16384)
		return -2;

	if (MaxNrNets2 == 0)
	{
		count = max(count, 256);

		if ((Nets2Global = GlobalAlloc(GHND, count * sizeof(NetRecord))) == NULL)
			return -1;

		if ((Nets2 = (NetArray *) GlobalLock(Nets2Global)) == NULL)
			return -1;

		MaxNrNets2 = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(Nets2Global, count * sizeof(NetRecord), GHND)) == NULL)
			return -1;

		Nets2Global = NewMem;

		if ((Nets2 = (NetArray *) GlobalLock(Nets2Global)) == NULL)
			return -1;

		MaxNrNets2 = count;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DeAllocateMemNets2()
{
	if (Nets2Global != NULL)
	{
		GlobalUnlock(Nets2Global);
		GlobalFree(Nets2Global);
		Nets2Global = NULL;
		MaxNrNets2 = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindCompNr(LPSTR CompName, int32 mode)
{
	int32 cnt;
	CompRecord *Comp;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			switch (mode)
			{
			case 0:
				if (stricmpOwn(CompName, Comp->Name) == 0)
					return cnt;

				break;

			case 1:
				if (stricmpOwn(CompName, Comp->PartNr) == 0)
					return cnt;

				break;

			}
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ReadNetList(LPSTR FileName, int32 mode)
{
	char LineBuf[512], LineBufCopy[512], NetName[MAX_LENGTH_STRING], OldNetName[MAX_LENGTH_STRING],
	     CompName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     NetItemStr[MAX_LENGTH_STRING];
	int32 fp, InfoPos[MAX_LENGTH_STRING], cnt, cnt2, cnt3, Length, lengte, count, Found, MemPos, CompNr, ShapeNr,
	      MemPosComp, PinOffset, NrPins, PinsMemPos, NrPinShapes, Lines, Layer, PolygonVertices, ShapeType, ShapePos,
	      PropertiesPos, NrProperties, Mirror, CompInfoIndex, pos, NrNets, ok, weg;
	double NewTraceWidth, NewClearance;
	LPSTR PinText;
	CompRecord *Comp;
	ShapeRecord *Shape;
	NetRecord *Net = NULL;
	NetItemsRecord *NetItem;
	int32 First = 1;
	int32 ObjectInclude;
	ShapePadRecord *ShapePad;
	CompPinRecord *CompPin;
	PadRecord *Pad;
	ConnectionsRecord *Connection;
	uint8 *CompInfoPosBuf;

	count = 0;
	NrNets = 0;
	NrNetItems = 0;
	MaxNrNetItems = 0;
	OldNetName[0] = 0;
	Lines = 0;
	NrNetItemsP = &NrNetItems;
	NewTraceWidth = 0.0;
	NewClearance = 0.0;
	PropertiesPos = 0;
	NrProperties = 0;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return -1;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		Lines++;
		LineBuf[Length] = 0;

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString2a(LineBuf, str);

			if (stricmpOwn(str, "net") == 0)
			{
				GetString2a(LineBuf, NetName);

				if (stricmpOwn(NetName, OldNetName) != 0)
				{
					if (Design.NrNets + 1 >= MaxNrNets)
					{
						if (AllocateMemNets(Design.NrNets + 128) != 0)
							return -1;
					}

					NrProperties = 0;
					PropertiesPos = 0;
					Net = &((*Nets)[Design.NrNets]);
					strcpy(OldNetName, NetName);
					memset(Net, 0, sizeof(NetRecord));
					cnt3 = min(strlen(NetName) - 2, sizeof(Net->Name) - 1);
					strncpy(Net->Name, (LPSTR) & NetName[1], cnt3);

					if (NewTraceWidth == 0.0)
					{
						if (InRange(Design.StandardTraceWidth, 0.0))
							Net->TraceWidth = (8 * 2540);
						else
							Net->TraceWidth = Design.StandardTraceWidth;
					}
					else
						Net->TraceWidth = (float) NewTraceWidth;

					if (NewClearance == 0.0)
					{
						if (InRange(Design.StandardClearance, 0.0))
							Net->TraceClearance = (8 * 2540);
						else
							Net->TraceClearance = Design.StandardClearance;
					}
					else
						Net->TraceClearance = (float) NewClearance;

					Design.NrNets++;
					NrNets++;
					NewTraceWidth = 0.0;
					NewClearance = 0.0;
				}

				count = 0;

				while ((LineBuf[0] != 0) && (count < 20))
				{
					count++;
					strcpy(LineBufCopy, LineBuf);
					GetString(LineBuf, NetItemStr);

					if (NetItemStr[0] == '(')
					{
						/*
						   (TRACEWIDTH,"8 mil")
						   (CLEARANCE,"8 mil")
						*/

						strcpy(LineBuf, LineBufCopy);
						GetParenthesisString(LineBuf, str);
						GetCommaString(str, str2);
						GetQuoteString(str, str4);

						if (PropertiesPos + strlen(str2) + strlen(str4) + 3 < 256)
						{
							strcpy((LPSTR) & Net->Properties[PropertiesPos], str2);
							PropertiesPos += strlen(str2) + 1;
							strcpy((LPSTR) & Net->Properties[PropertiesPos], str4);
							PropertiesPos += strlen(str4) + 1;
							NrProperties++;
						}

						if (ScanParameters(1, str4, 0) == 1)
						{
							if (stricmp(str2, "TRACEWIDTH") == 0)
								NewTraceWidth = ParamsFloat[0];

							if (stricmp(str2, "CLEARANCE") == 0)
								NewClearance = ParamsFloat[0];
						}
					}
					else
					{
#ifdef _DEBUG

						if (stricmpOwn(NetItemStr, "D1--DC") == 0)
							ok = 1;

#endif
						strcpy(CompName, NetItemStr);
						lengte = strlen(NetItemStr);
						cnt2 = 0;

						while ((cnt2 < lengte) && (CompName[cnt2] != '-'))
							cnt2++;

						if (cnt2 < lengte)
						{
							cnt3 = min(cnt2, sizeof(Comp->Name) - 1);
							CompName[cnt3] = 0;

							if ((CompNr = FindCompNr(CompName, 0)) != -1)
							{
								if (NrNetItems >= MaxNrNetItems)
								{
									if (AllocateMemNetItems(NrNetItems + 128) != 0)
										return -1;
								}

								NetItem = &((*NetItems)[NrNetItems++]);
								memset(NetItem, 0, sizeof(NetItemsRecord));
								NetItem->NetNr = NrNets - 1;
								NetItem->CompNr = CompNr;
								NetItem->Info = 0;
								Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);

//                              strcpy(NetItem->PinStr,NetItemStr);

								cnt3 = min(lengte - cnt2 - 1, sizeof(NetItem->PinStr) - 1);
								strncpy(NetItem->PinStr, (LPSTR) & CompName[cnt2 + 1], cnt3);
							}
						}
					}
				}
			}
		}
	}

	TextFileClose(fp);
	weg = NrNetItems;

//  sprintf(InfoStr,"Reading finished");
//  RedrawInfoStr(1);

//  if (AllocateMemNetInfos(Design.NrNets+128)!=0) return -1;

	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &CompInfo);
	AllocateSpecialMem(MEM_COMP_INFO, 128 * 1024, (void **) &CompInfoPos);
	CompInfoPosBuf = (uint8 *) CompInfoPos;
	memset(CompInfo, 0, 128 * 1024);
	memset(CompInfoPos, 0, 128 * 1024);
	CompInfoPos2 = (CompInfoPosArray *) & CompInfoPosBuf[16384];

	for (cnt = 0; cnt < NrNetItems; cnt++)
	{
		NetItem = &((*NetItems)[cnt]);
		(*CompInfoPos)[NetItem->CompNr + 1]++;
		(*CompInfoPos2)[NetItem->CompNr + 1]++;
	}

	for (cnt = 1; cnt < Design.NrComps; cnt++)
	{
		(*CompInfoPos)[cnt + 1] = (int16) ((*CompInfoPos)[cnt + 1] + (*CompInfoPos)[cnt]);
		(*CompInfoPos2)[cnt + 1] = (int16) ((*CompInfoPos2)[cnt + 1] + (*CompInfoPos2)[cnt]);
	}

	memmove(&InfoPos, &((*CompInfoPos2)[0]), 200 * sizeof(int16));

	for (cnt = 0; cnt < NrNetItems; cnt++)
	{
		NetItem = &((*NetItems)[cnt]);
		CompNr = NetItem->CompNr;
		pos = (*CompInfoPos)[CompNr];
		(*CompInfo)[pos] = (int16) cnt;
		(*CompInfoPos)[CompNr]++;
	}

	memmove(&InfoPos, &((*CompInfoPos2)[0]), 200 * sizeof(int16));

	/*
	  for (cnt=0;cnt<Design.NrComps;cnt++) {
	    Comp=(CompRecord *)&(CompsMem[(*Comps)[cnt]]);
	    if (stricmpOwn(Comp->Name,"U100")==0) {
	      ok=1;
	    }
	  }

	  return 0;
	*/

	NetPinsConnected = 0;
	NetPinsNotConnected = 0;
	PolygonVertices = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{

//      for (cnt=519;cnt<520;cnt++) {

		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

		if (stricmpOwn(Comp->Name, "D1") == 0)
			ok = 1;

#endif
		strcpy(InfoStr, Comp->Name);
		RedrawInfoStr(1);
		CompInfoIndex = (*CompInfoPos2)[cnt];
		count = (*CompInfoPos2)[cnt + 1] - (*CompInfoPos2)[cnt];
		Mirror = ((Comp->CompMode & 8) >> 3);

		ShapeNr = Comp->ShapeNr;
		MemPos = (*Shapes)[ShapeNr].ShapePos;
		ShapePos = MemPos;
		Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
		MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
		PinsMemPos = (*Comps)[cnt] + sizeof(CompRecord);
		NrPins = Shape->NrPins;
		PinOffset = Shape->PinOffset;
		MemPos += PinOffset;

		while (NrPins > 0)
		{
			ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);

//          NrPinShapes=ShapePad->NrPinShapes;

			MemPos += sizeof(ShapePadRecord);
			CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);

//          NetNr=CompPin->NetNr;

			PinText = (ShapePad->Name);

#ifdef _DEBUG

			if (PinText[0] == 0)
				ok = 1;

#endif
			NrPinShapes = ShapePad->NrPinShapes;
			ObjectInclude = 0;

			for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
			{
				Pad = (PadRecord *) & (ShapesMem[MemPos]);
				Layer = Pad->Layer;

				if (CheckGeometryLayer(&Layer, Shape->NrLayers, Mirror))
					ObjectInclude = 1;

				ShapeType = Pad->ShapeType;

				if (ShapeType != PIN_ARC)
					MemPos += sizeof(PadRecord);
				else
					MemPos += 48;
			}

			Found = -1;

			if (ObjectInclude)
			{
				cnt2 = 0;

				while ((cnt2 < count) && (Found == -1))
				{
					NetItem = &((*NetItems)[(*CompInfo)[CompInfoIndex + cnt2]]);

					if (stricmpOwn(NetItem->PinStr, PinText) == 0)
						Found = cnt2;

					cnt2++;
				}
			}

			if (Found != -1)
			{
				NetItem = &((*NetItems)[(*CompInfo)[CompInfoIndex + Found]]);
				CompPin->NetNr = NetItem->NetNr;
				NetPinsConnected++;

//              sprintf(InfoStr,"eqrf ");
//              RedrawInfoStr(1);

				NetItem->Info++;

				if (NetItem->Info == 2)
					ok = 1;
			}
			else
				NetPinsNotConnected++;

			NrPins--;
			PinsMemPos += sizeof(CompPinRecord);
		}

		ok = 1;
	}

	First = 1;
	MessageBufPos = 0;

	for (cnt = 0; cnt < NrNetItems; cnt++)
	{
		NetItem = &((*NetItems)[cnt]);

		if (NetItem->Info != 1)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[NetItem->CompNr]]);

			if (First)
			{
				if (AddToMessageBuf(SC(430, "The following geometry pins do not exist\r\n")) != 0)
					return -1;

				First = 0;
			}

			if (NetItem->Info == 0)
				sprintf(str2, "%s\t%s", Comp->Name, NetItem->PinStr);
			else
				sprintf(str2, SC(431, "%s\t%s more then once"), Comp->Name, NetItem->PinStr);

			if (AddToMessageBuf(str2) != 0)
				return -1;

			ok = 1;
		}
		else
		{
			Net = &((*Nets)[NetItem->NetNr]);
			Net->NrPins++;
		}
	}

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(24, "Error"), 0, 0);
		DeAllocateMemMessageBuf();
	}

	if (Design.NrComps > 0)
	{
		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			if (cnt == 1000)
				cnt2 = cnt;

			Net = &((*Nets)[cnt]);
			strcpy(InfoStr, Net->Name);
			RedrawInfoStr(1);
			DebugMode = 1;
			CheckNet((int32) cnt, 0);
			DebugMode = 0;
			CheckForEscape();
			InsertConnections((int32) cnt, 0);
		}
	}

	for (cnt = 0; cnt < Design.NrConnections; cnt++)
	{
		Connection = &((*Connections)[cnt]);
		Connection->AddNr = 0;
		Connection->DeleteNr = 0;
		Connection->Info = 0;
	}

//  InsertAreaFillPowerPlanes();

	DeAllocateMemNetItems();
	LastActionNr = 1;
	MaxLastActionNr = 1;
	ViewFull();
#ifdef _DEBUG
	sprintf(str, "NetItems %i", weg);
//  WriteLn(Debugfp2,str);
#endif

	ok = 1;
	DeallocateSpecialMem(MEM_COMP_INFO);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ImportNetList(int32 mode)
{
	char FileName[MAX_LENGTH_STRING], LineBuf[512], Ref[MAX_LENGTH_STRING], Geometrie[MAX_LENGTH_STRING],
	     str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], Properties[32][128];
	int32 ok, fp, ShapeCompNrs[512], cnt, cnt2, Length, count, Found, MemPos, NrProperties, NrTries, NrShapeAreas,
	      MinAreaResistorShapeNr, MinAreaCapacitorShapeNr;
	uint8 ShapeAreaPresence[512], *CompBuf;
	double ShapeArea[512], Area, MinY, MaxY, MaxX, PlaceX, PlaceY, PlaceX2, PlaceY2, MinAreaResistorShape,
	       MinAreaCapacitorShape, HeightResistorShape, WidthResistorShape, HeightShape, WidthShape, HeightCapacitorShape,
	       WidthCapacitorShape;
	CompRecord *Comp;
	ShapeRecord *Shape;
	CompPinRecord *CompPin;
	int32 Error = 0;
	int32 First = 1;

	HeightResistorShape = 0.0;
	WidthResistorShape = 0.0;
	HeightCapacitorShape = 0.0;
	WidthCapacitorShape = 0.0;
	HeightShape = 0.0;
	WidthShape = 0.0;
	MaxX = 0.0;
	MinY = 0.0;
	MaxY = 0.0;
	PlaceX = Design.BoardOriginX;
	PlaceX2 = Design.BoardOriginX;
	PlaceY = Design.BoardOriginY;
	PlaceY2 = Design.BoardOriginX;
	memset(Properties, 0, sizeof(Properties));
	NrProperties = 0;

	if (Design.NrBoardLayers == 0)
	{
		MessageBoxOwn(PCBWindow, SC(432, "Number of layers is 0"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	count = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
			count++;
	}

	if (count > 0)
	{
		if
			(MessageBoxOwn(PCBWindow, SC(433, "The whole design will be deleted, Ok to go on ?"), SC(118, "Warning"),
				MB_APPLMODAL | MB_OKCANCEL) != IDOK)
			
			return -1;
	}

	TimerUpdateNetlist = 0;
	FileName[0] = 0;
	sprintf(str, "%s\\pcb", DesignPath);

	if (GetNewFileUTF8
	        (PCBWindow, NULL, FileName, str, SC(419, "Netlist file"), NULL, SC(418, "Import netlist file"), "net", 0))
	{

//    MessageBoxOwn(PCBWindow,"ImportNetList",SC(24,"Error"),MB_APPLMODAL|MB_OK);

		return -1;
	}
	
	if (ProjectIndexNr != -1)
		ProjectInfo->OtherInfos[0] = 0;

	LastActionNr = 1;
	MaxLastActionNr = 1;
	MessageBufPos = 0;

	if (Design.NrComps > 0)
	{
		count = 0;

		for (cnt = 0; cnt < 32; cnt++)
		{
			Design.NrHorTraces[cnt] = 0;
			Design.NrVerTraces[cnt] = 0;
			Design.NrDiag1Traces[cnt] = 0;
			Design.NrDiag2Traces[cnt] = 0;
		}

		Design.NrVias = 0;
		Design.NrAreaFills = 0;
		Design.NrComps = 0;
		Design.NrNets = 0;
		Design.NrShapes = 0;
		Design.NrConnections = 0;
		Design.CompsMem = 0;
		Design.ShapesMem = 0;
		Design.AreaFillMem = 0;
		Design.NrObjectLines = 0;
		Design.NrObjectCircles = 0;
		Design.NrObjectRects = 0;
		Design.NrObjectArcs = 0;
		Design.NrObjectTexts = 0;
	}

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return -1;

	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &Comp);
	CompBuf = (uint8 *) Comp;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString2a(LineBuf, str);

			if (stricmp(str, "COMP") == 0)
			{
				GetString2a(LineBuf, Ref);
				GetString2a(LineBuf, Geometrie);
				Found = LoadShape(Geometrie);

//            TextBuf

				if (Found == -1)
				{
					if (First)
					{
						if (AddToMessageBuf(SC(378, "The following geometries were not found\r\n\r\n")) != 0)
							return 0;

						First = 0;
					}

					strcpy(str2, Ref);
					strcat(str2, "\t");
					strcat(str2, Geometrie);

					if (AddToMessageBuf(str2) != 0)
						return 0;
				}
				else
				{
					MemPos = (*Shapes)[Found].ShapePos;
					Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
					memset(Comp, 0, (sizeof(CompRecord) + Shape->NrPins) * sizeof(CompPinRecord));
					CompPin = (CompPinRecord *) & (CompBuf[sizeof(CompRecord)]);

					for (cnt = 0; cnt < Shape->NrPins; cnt++)
					{
						CompPin->NetNr = -1;
						CompPin += 1;
					}

					Comp->NrPins = (int16) Shape->NrPins;
					Comp->ShapeNr = (int16) Found;
					memmove(Comp->Name, &Ref, min(sizeof(Comp->Name) - 1, strlen(Ref)));
					memmove(Comp->ShapeName, &Geometrie, min(sizeof(Comp->ShapeName) - 1, strlen(Geometrie)));
					GetString2a(LineBuf, str);

					if (str[0] != 0)
						memmove(Comp->Value, &str, min(sizeof(Comp->Value) - 1, strlen(str)));

					/*
					COMP  J100                     pcat-bus                           "PC-AT" (prop1,"value1")
					*/

					NrTries = 0;
					NrProperties = 0;

					while (NrTries < 31)
					{
						GetParenthesisString(LineBuf, Properties[NrProperties]);

						if (Properties[NrProperties][0] == 0)
							break;

						NrProperties++;
						NrTries++;
					}

					cnt2 = 0;

					for (cnt = 0; cnt < NrProperties; cnt++)
					{
						strcpy(str, Properties[cnt]);
						GetCommaString(str, str2);
						GetQuoteString(str, str3);

						if (stricmp(str2, "PARTNR") == 0)
							memmove(Comp->PartNr, str3, min(sizeof(Comp->PartNr) - 1, strlen(str3)));
						else
						{
							if (stricmp(str2, "PARTDESCR") == 0)
							{
								memmove(Comp->PartDescription, str3,
								        min(sizeof(Comp->PartDescription) - 1, strlen(str3)));
							}
							else
							{
								if (cnt2 + strlen(str2) + strlen(str3) + 3 < 256)
								{
									strcpy(&Comp->Properties[cnt2], str2);
									cnt2 += strlen(str2) + 1;
									strcpy(&Comp->Properties[cnt2], str3);
									cnt2 += strlen(str3) + 1;
								}
								else
								{
									sprintf(str2,
									        "For component %s some properties will not be used because of lack of space\r\n",
									        Comp->Name);

									if (AddToMessageBuf(str2) != 0)
										return 0;

									break;
								}
							}
						}
					}

					Comp->CompNamePenThickNess = Design.SilkScreenWidth;
					Comp->CompValuePenThickNess = Design.SilkScreenWidth;
					Comp->CompNameOriginX = Shape->ShapeNameOriginX;
					Comp->CompNameOriginY = Shape->ShapeNameOriginY;
					Comp->CompNameHeight = Shape->ShapeNameHeight;
					Comp->CompValueOriginX = Shape->ShapeNameOriginX;
					Comp->CompValueOriginY = Shape->ShapeNameOriginY;
					Comp->CompValueHeight = Shape->ShapeNameHeight;

					if ((Design.NrBoardLayers == 1) && ((Shape->Info & SMD_DEVICE) == SMD_DEVICE))
					{
						Comp->CompMode |= 8;
						Comp->TextVisibility |= 8 * 0x11;
					}

					if (isdigit(Comp->Name[1]))
					{
						if ((Comp->Name[0] == 'r') || (Comp->Name[0] == 'R'))
							Comp->Info2 = 1;

						if ((Comp->Name[0] == 'c') || (Comp->Name[0] == 'C'))
							Comp->Info2 = 2;
					}

#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "D38") == 0)
						ok = 1;

#endif

					if (!AddComp(Comp))
					{
						Comp->AddNr = 0;
						Error = 1;
					}

				}

				count++;
			}
		}
	}

	TextFileClose(fp);

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(24, "Error"), 0, 0);
		DeAllocateMemMessageBuf();
	}

// ********************************************************************************
// ********************************************************************************
	if (!Error)
	{
		if ((InRange(Design.BoardWidth, 0.0)) || (InRange(Design.BoardHeight, 0.0)))
		{
			Design.BoardWidth = 200e5;
			Design.BoardHeight = 150e5;
			Design.BoardOriginX = (1000 * 2540.0);
			Design.BoardOriginY = (1000 * 2540.0);
		}


		PlaceX2 = Design.BoardOriginX;
		PlaceX2 = AdjustToGrid(PlaceX2, (5 * 2540.0));

// ********************************************************************************
// ********************************************************************************

		memset(&ShapeCompNrs, 0, sizeof(ShapeCompNrs));
		memset(&ShapeAreaPresence, 0, 512);
		MinAreaResistorShapeNr = -1;
		MinAreaCapacitorShapeNr = -1;
		MinAreaResistorShape = 1000.0e5 * 1000.0e5;
		MinAreaCapacitorShape = 1000.0e5 * 1000.0e5;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (Comp->Info2 == 1)
			{	// Resistor
				Area = Comp->PlacementWidth * Comp->PlacementHeight;

				if (Area < MinAreaResistorShape)
				{
					MinAreaResistorShapeNr = Comp->ShapeNr;
					MinAreaResistorShape = Area;
					HeightResistorShape = Comp->PlacementHeight;
					WidthResistorShape = Comp->PlacementWidth;
				}
			}

			if (Comp->Info2 == 2)
			{	// Capacitor
				Area = Comp->PlacementWidth * Comp->PlacementHeight;
#ifdef _DEBUG

				if (Area == 0.0)
					ok = 1;

#endif

				if (Area < MinAreaCapacitorShape)
				{
					MinAreaCapacitorShapeNr = Comp->ShapeNr;
					MinAreaCapacitorShape = Area;
					HeightCapacitorShape = Comp->PlacementHeight;
					WidthCapacitorShape = Comp->PlacementWidth;
				}
			}
		}

// ********************************************************************************
// ********************************************************************************
		PlaceY = Design.BoardOriginY;
		First = 1;

		if (MinAreaResistorShapeNr != -1)
		{
			ShapeAreaPresence[MinAreaResistorShapeNr] = 1;

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 1) && (Comp->ShapeNr == MinAreaResistorShapeNr))
				{
					if (First)
					{
						First = 0;
						PlaceX = Design.BoardOriginX;
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceX2 = PlaceX;
						PlaceY -= HeightResistorShape;
						MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
					}
					else
					{
						if (PlaceX > Design.BoardWidth + Design.BoardOriginX)
						{
							PlaceY -= HeightResistorShape + (5 * 2540.0);
							PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
							MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
							PlaceX = PlaceX2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceX += WidthResistorShape + (5 * 2540.0);
					PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
				}
			}
		}

//    *******************************************************************************************
//    ******************** Other resistors ******************************************************
//    *******************************************************************************************

		memset(&ShapeArea, 0, sizeof(ShapeArea));
		NrShapeAreas = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info2 == 1)	// Resistor
			        && (Comp->Info3 == 0) && (ShapeAreaPresence[Comp->ShapeNr] == 0))
			{
				Area = Comp->PlacementWidth * Comp->PlacementHeight;
				cnt2 = 0;

				while ((cnt2 < NrShapeAreas) && (Area > ShapeArea[cnt2]))
					cnt2++;

				if (cnt2 == NrShapeAreas)
				{
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					NrShapeAreas++;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
				}
				else
				{
					memmove(&ShapeArea[cnt2 + 1], &ShapeArea[cnt2], (NrShapeAreas - cnt2) * 4);
					memmove(&ShapeCompNrs[cnt2 + 1], &ShapeCompNrs[cnt2], (NrShapeAreas - cnt2) * 4);
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
					NrShapeAreas++;
				}
			}
		}


		for (cnt2 = 0; cnt2 < NrShapeAreas; cnt2++)
		{
			First = 1;

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 1)	// Resistor
				        && (Comp->ShapeNr == ShapeCompNrs[cnt2]))
				{
					if (First)
					{
						First = 0;
						HeightShape = Comp->PlacementHeight;
						WidthShape = Comp->PlacementWidth;
						PlaceX = Design.BoardOriginX;
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceX2 = PlaceX;
						PlaceY = MinY - HeightShape - (Comp->CompOriginY - Comp->BoardPosMaxY) - (5 * 2540.0);
						MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
					}
					else
					{
						if (PlaceX > Design.BoardWidth + Design.BoardOriginX)
						{
							PlaceY -= HeightShape + (5 * 2540.0);
							PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
							MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
							PlaceX = PlaceX2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceX += WidthShape + (5 * 2540.0);
					PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ********************************************************************************

		PlaceX = Design.BoardOriginX + Design.BoardWidth;
		First = 1;

		if (MinAreaCapacitorShapeNr != -1)
		{
			ShapeAreaPresence[MinAreaCapacitorShapeNr] = 1;

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 2) && (Comp->ShapeNr == MinAreaCapacitorShapeNr))
				{
					if (First)
					{
						First = 0;
						PlaceX += WidthCapacitorShape;
						MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceY = Design.BoardOriginY;
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
						PlaceY2 = PlaceY;
					}
					else
					{
						if (PlaceY > Design.BoardHeight + Design.BoardOriginY)
						{
							PlaceX += WidthCapacitorShape + (5 * 2540.0);
							MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
							PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
							PlaceY = PlaceY2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					SetBoardPosComp(Comp, 0);
					Comp->Info3 = 1;
					PlaceY += HeightCapacitorShape + (5 * 2540.0);
					PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ********* Other capacitors *****************************************************
//    ********************************************************************************

		memset(&ShapeArea, 0, sizeof(ShapeArea));
		NrShapeAreas = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info2 == 2)	// Capacitor
			        && (Comp->Info3 == 0) && (ShapeAreaPresence[Comp->ShapeNr] == 0))
			{
				Area = Comp->PlacementWidth * Comp->PlacementHeight;
				cnt2 = 0;

				while ((cnt2 < NrShapeAreas) && (Area > ShapeArea[cnt2]))
					cnt2++;

				if (cnt2 == NrShapeAreas)
				{
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					NrShapeAreas++;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
				}
				else
				{
					memmove(&ShapeArea[cnt2 + 1], &ShapeArea[cnt2], (NrShapeAreas - cnt2) * 4);
					memmove(&ShapeCompNrs[cnt2 + 1], &ShapeCompNrs[cnt2], (NrShapeAreas - cnt2) * 4);
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
					NrShapeAreas++;
				}
			}
		}

//    ********************************************************************************

		for (cnt2 = 0; cnt2 < NrShapeAreas; cnt2++)
		{
			First = 1;

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 2)	// Capacitor
				        && (Comp->ShapeNr == ShapeCompNrs[cnt2]))
				{
					if (First)
					{
						First = 0;
						HeightShape = Comp->PlacementHeight;
						WidthShape = Comp->PlacementWidth;
						PlaceX = MaxX + WidthShape + (Comp->CompOriginX - Comp->BoardPosMaxX) + (5 * 2540.0);
						MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceY = Design.BoardOriginY;
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
						PlaceY2 = PlaceY;
					}
					else
					{
						if (PlaceY > Design.BoardHeight + Design.BoardOriginY)
						{
							PlaceX += WidthShape + (5 * 2540.0);
							MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
							PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
							PlaceY = PlaceY2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceY += HeightShape + (5 * 2540.0);
					PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    *********** Other shapes *******************************************************
//    ********************************************************************************

		memset(&ShapeArea, 0, sizeof(ShapeArea));
		NrShapeAreas = 0;

		for (cnt = 0; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

			if (stricmpOwn(Comp->Name, "T3") == 0)
				ok = 1;

#endif

			if ((Comp->Info3 == 0) && (Comp->Info2 == 0) && (ShapeAreaPresence[Comp->ShapeNr] == 0))
			{
				Area = Comp->PlacementWidth * Comp->PlacementHeight;
				cnt2 = 0;

				while ((cnt2 < NrShapeAreas) && (Area > ShapeArea[cnt2]))
					cnt2++;

				if (cnt2 == NrShapeAreas)
				{
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					NrShapeAreas++;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
				}
				else
				{
					memmove(&ShapeArea[cnt2 + 1], &ShapeArea[cnt2], (NrShapeAreas - cnt2) * 4);
					memmove(&ShapeCompNrs[cnt2 + 1], &ShapeCompNrs[cnt2], (NrShapeAreas - cnt2) * 4);
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
					NrShapeAreas++;
				}
			}
		}

//    ********************************************************************************

		MaxY = Design.BoardOriginY + Design.BoardHeight;

		for (cnt2 = 0; cnt2 < NrShapeAreas; cnt2++)
		{
			First = 1;

			for (cnt = 0; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

				if (stricmpOwn(Comp->Name, "t3") == 0)
					ok = 1;

#endif

				if ((Comp->Info3 == 0) && (Comp->Info2 == 0) && (Comp->ShapeNr == ShapeCompNrs[cnt2]))
				{
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "t3") == 0)
						ok = 1;

#endif

					if (First)
					{
						First = 0;
						HeightShape = Comp->PlacementHeight;
						WidthShape = Comp->PlacementWidth;
						PlaceX = Design.BoardOriginX;
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceY = MaxY + HeightShape + (Comp->CompOriginY - Comp->BoardPosMaxY) + (5 * 2540.0);
						MaxY = PlaceY - (Comp->CompOriginY - Comp->BoardPosMaxY);
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
						PlaceY2 = PlaceY;
					}
					else
					{
						if (PlaceX > Design.BoardWidth + Design.BoardOriginX)
						{
							PlaceY += HeightShape + (5 * 2540.0);
							MaxY = PlaceY - (Comp->CompOriginY - Comp->BoardPosMaxY);
							PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
							PlaceX = PlaceX2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceX += WidthShape + (5 * 2540.0);
					PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ********************************************************************************

		ok = 1;
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		Comp->Info &= 0x00ff;
		Comp->AddNr = 0;
		Comp->DeleteNr = 0;
	}

	ViewFull();
	CheckInputMessages(0);
	ReadNetList(FileName, 0);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 UpdateNetList(int32 mode)
{
	char FileName[MAX_LENGTH_STRING], LineBuf[512], Ref[MAX_LENGTH_STRING], Geometrie[MAX_LENGTH_STRING],
	     str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], Properties[32][128];
	int32 fp, ShapeCompNrs[512], NrProperties, NrTries, cnt, cnt2, cnt3, Length, count, Found, MemPos, ok, TempNrComps,
	      NrShapeAreas, FoundCompNr, res, MinAreaResistorShapeNr, MinAreaCapacitorShapeNr, ChangedCompNrByShape;
	uint8 ShapeAreaPresence[512], *CompBuf;
	double ShapeArea[512], Area, MinY, MaxY, MaxX, PlaceX, PlaceY, PlaceX2, PlaceY2, MinAreaResistorShape,
	       MinAreaCapacitorShape, HeightResistorShape, WidthResistorShape, HeightShape, WidthShape, HeightCapacitorShape,
	       WidthCapacitorShape, BoardMinX, BoardMinY, BoardMaxX, BoardMaxY;

	CompRecord *Comp, *SearchComp;
	ShapeRecord *Shape;
	CompPinRecord *CompPin;
	int32 Error = 0;
	int32 First = 1;
	int32 ComponentAdded = 0;

	HeightResistorShape = 0.0;
	WidthResistorShape = 0.0;
	HeightCapacitorShape = 0.0;
	WidthCapacitorShape = 0.0;
	HeightShape = 0.0;
	WidthShape = 0.0;
	MaxX = 0.0;
	MinY = 0.0;
	MaxY = 0.0;
	PlaceX = Design.BoardOriginX;
	PlaceX2 = Design.BoardOriginX;
	PlaceY = Design.BoardOriginY;
	PlaceY2 = Design.BoardOriginX;

	if (ProjectIndexNr != -1)
		ProjectInfo->OtherInfos[0] = 0;

	TimerUpdateNetlist = 0;

	if (MessageBoxOwn(PCBWindow, SC(435, "Update the netlist ?"), SC(1, "Message"), MB_APPLMODAL | MB_OKCANCEL) != IDOK)
		return -1;

	if (Design.NrBoardLayers == 0)
	{
		MessageBoxOwn(PCBWindow, SC(432, "Number of layers is 0"), SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	res = 0;
	
	if (FileChanged)
	{
		sprintf(str, SC(142, "The layout file has changed.\n\n%s\n\nDo you want to update it ?"), EditFile);

		res = MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OKCANCEL == IDOK);
		
		if (res == IDOK)
		{
			res = SaveFile(0);

			if (res == -1)
				MessageBoxOwn(PCBWindow, EditFile, SC(147, "Error in saving file"), MB_APPLMODAL | MB_OK);

			return 1;
		}
		else
		{
			if (res == IDCANCEL)
				
				return 0;
		}
	}

	Length = strlen(EditFile);
	strcpy(FileName, EditFile);

	if (Length > 4)
	{
		FileName[Length - 3] = 'n';
		FileName[Length - 2] = 'e';
		FileName[Length - 1] = 't';
	}

	if (FileExistsUTF8(FileName) != 0)
	{
		MessageBoxOwn(PCBWindow, FileName, SC(436, "Netlist file does not exist"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	ZeroObjects();

	MessageBufPos = 0;
	count = 0;
	memset(Properties, 0, sizeof(Properties));
	NrProperties = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
			Comp->Info &= ~COMPONENT_EXIST;
	}

//  MakeNetlistFromDesign(0);

	TempNrComps = Design.NrComps;
	AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &Comp);
	CompBuf = (uint8 *) Comp;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return -1;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString2a(LineBuf, str);

			if (stricmp(str, "COMP") == 0)
			{
				GetString(LineBuf, Ref);
#ifdef _DEBUG

				if (stricmp(Ref, "U2") == 0)
					ok = 1;

#endif
				GetString2a(LineBuf, Geometrie);
				GetString2a(LineBuf, str);
				NrTries = 0;
				NrProperties = 0;

				while (NrTries < 31)
				{
					GetParenthesisString(LineBuf, Properties[NrProperties]);

					if (Properties[NrProperties][0] == 0)
						break;

					NrProperties++;
					NrTries++;
				}

				Found = LoadShape(Geometrie);

				// TextBuf
				if (Found == -1)
				{
					if (First)
					{
						if (AddToMessageBuf(SC(378, "The following geometries were not found\r\n\r\n")) != 0)
							return 0;

						First = 0;
					}

					strcpy(str2, Ref);
					strcat(str2, "\t");
					strcat(str2, Geometrie);

					if (AddToMessageBuf(str2) != 0)
						return 0;
				}
				else
				{
					FoundCompNr = -1;
					AllocateSpecialMem(MEM_POINTS, 128 * 1024, (void **) &Comp);
					MemPos = (*Shapes)[Found].ShapePos;
					Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
					ChangedCompNrByShape = -1;

					for (cnt = 0; cnt < TempNrComps; cnt++)
					{
						SearchComp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

						if ((FoundCompNr == -1) && ((SearchComp->Info & (OBJECT_NOT_VISIBLE)) == 0)
						        && (stricmpUTF8(Ref, SearchComp->Name) == 0)
						        && (stricmpUTF8(SearchComp->ShapeName, Geometrie) == 0))
						{
							FoundCompNr = cnt;
							SearchComp->Info |= COMPONENT_EXIST;

							if (str[0] != 0)
							{
								memset(SearchComp->Value, 0, sizeof(SearchComp->Value));
								memmove(SearchComp->Value, &str, min(sizeof(Comp->Value) - 1, strlen(str)));
							}

							cnt2 = 0;
							memset(SearchComp->Properties, 0, sizeof(SearchComp->Properties));

							for (cnt3 = 0; cnt3 < NrProperties; cnt3++)
							{
								strcpy(str, Properties[cnt3]);
								GetCommaString(str, str2);
								GetQuoteString(str, str3);

								if (stricmp(str2, "PARTNR") == 0)
									memmove(SearchComp->PartNr, str3, min(sizeof(Comp->PartNr) - 1, strlen(str3)));
								else
								{
									if (stricmp(str2, "PARTDESCR") == 0)
									{
										memmove(SearchComp->PartDescription, str3,
										        min(sizeof(Comp->PartDescription) - 1, strlen(str3)));
									}
									else
									{
										if (cnt2 + strlen(str2) + strlen(str3) + 3 < 256)
										{
											strcpy(&SearchComp->Properties[cnt2], str2);
											cnt2 += strlen(str2) + 1;
											strcpy(&SearchComp->Properties[cnt2], str3);
											cnt2 += strlen(str3) + 1;
										}
										else
										{
											sprintf(str2,
											        "For component %s some properties will not be used because of lack of space\r\n",
											        Comp->Name);

											if (AddToMessageBuf(str2) != 0)
												return 0;

											break;
										}
									}
								}
							}
						}
					}

					if (FoundCompNr == -1)
					{
						ComponentAdded = 1;
						memset(Comp, 0, (sizeof(CompRecord) + Shape->NrPins) * sizeof(CompPinRecord));
						CompPin = (CompPinRecord *) & (CompBuf[sizeof(CompRecord)]);

						for (cnt = 0; cnt < Shape->NrPins; cnt++)
						{
							CompPin->NetNr = -1;
							CompPin += 1;
						}

						Comp->NrPins = (int16) Shape->NrPins;
						Comp->ShapeNr = (int16) Found;
						memset(Comp->Name, 0, sizeof(Comp->Name));
						memmove(Comp->Name, &Ref, min(sizeof(Comp->Name) - 1, strlen(Ref)));
						memset(Comp->ShapeName, 0, sizeof(Comp->ShapeName));
						memmove(Comp->ShapeName, &Geometrie, min(sizeof(Comp->ShapeName) - 1, strlen(Geometrie)));
						memset(Comp->Value, 0, sizeof(Comp->Value));
						memmove(Comp->Value, &str, min(sizeof(Comp->Value) - 1, strlen(str)));
						cnt2 = 0;

						for (cnt = 0; cnt < NrProperties; cnt++)
						{
							strcpy(str, Properties[cnt]);
							GetCommaString(str, str2);
							GetQuoteString(str, str3);

							if (stricmp(str2, "PARTNR") == 0)
								memmove(Comp->PartNr, str3, min(sizeof(Comp->PartNr) - 1, strlen(str3)));
							else
							{
								if (stricmp(str2, "PARTDESCR") == 0)
								{
									memmove(Comp->PartDescription, str3,
									        min(sizeof(Comp->PartDescription) - 1, strlen(str3)));
								}
								else
								{
									if (cnt2 + strlen(str2) + strlen(str3) + 3 < 256)
									{
										strcpy(&Comp->Properties[cnt2], str2);
										cnt2 += strlen(str2) + 1;
										strcpy(&Comp->Properties[cnt2], str3);
										cnt2 += strlen(str3) + 1;
									}
									else
									{
										sprintf(str2,
										        "For component %s some properties will not be used because of lack of space\r\n",
										        Comp->Name);

										if (AddToMessageBuf(str2) != 0)
											return 0;

										break;
									}
								}
							}
						}

						Comp->CompNamePenThickNess = Design.SilkScreenWidth;
						Comp->CompValuePenThickNess = Design.SilkScreenWidth;
						Comp->CompNameOriginX = Shape->ShapeNameOriginX;
						Comp->CompNameOriginY = Shape->ShapeNameOriginY;
						Comp->CompNameHeight = Shape->ShapeNameHeight;
						Comp->CompNameRotation = (float) Shape->ShapeNameRotation;
						Comp->CompValueOriginX = Shape->ShapeNameOriginX;
						Comp->CompValueOriginY = Shape->ShapeNameOriginY;
						Comp->CompValueHeight = Shape->ShapeNameHeight;
						Comp->CompValueRotation = (float) Shape->ShapeNameRotation;

						if ((Design.NrBoardLayers == 1) && ((Shape->Info & SMD_DEVICE) == SMD_DEVICE))
						{
							Comp->CompMode |= 8;
							Comp->TextVisibility |= 8 * 0x11;
						}

						if (isdigit(Comp->Name[1]))
						{
							if ((Comp->Name[0] == 'r') || (Comp->Name[0] == 'R'))
								Comp->Info2 = 1;

							if ((Comp->Name[0] == 'c') || (Comp->Name[0] == 'C'))
								Comp->Info2 = 2;
						}

#ifdef _DEBUG

						if (stricmpOwn(Comp->Name, "R84") == 0)
							ok = 1;

#endif

						if (!AddComp(Comp))
							Error = 1;
						else
						{
							Comp = (CompRecord *) & (CompsMem[(*Comps)[Design.NrComps - 1]]);
							Comp->Info |= COMPONENT_EXIST;
						}
					}
				}

				count++;
			}
		}
	}

	TextFileClose(fp);

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(24, "Error"), 0, 0);
		DeAllocateMemMessageBuf();
	}

//  LoadShapes(1);

	for (cnt = 0; cnt < TempNrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE | COMPONENT_EXIST)) == 0)
		{
			ZeroUnusedObjects(0);
			Comp->Info |= OBJECT_NOT_VISIBLE;
			Comp->DeleteNr = (int16) LastActionNr;
			DataBaseChanged = 1;
		}
	}

// ********************************************************************************
// ********************************************************************************

	if ((!Error) && (ComponentAdded))
	{
		if ((InRange(Design.BoardWidth, 0.0)) || (InRange(Design.BoardHeight, 0.0)))
		{
			Design.BoardWidth = 200e5;
			Design.BoardHeight = 150e5;
			Design.BoardOriginX = (1000 * 2540.0);
			Design.BoardOriginY = (1000 * 2540.0);
		}

//    ********************************************************************************
//    ********************************************************************************

		FindMinMaxBoard(&BoardMinX, &BoardMinY, &BoardMaxX, &BoardMaxY, 1);


		PlaceX = BoardMaxX;

//      PlaceX=Design.BoardOriginX+Design.BoardWidth;

		First = 1;

		memset(&ShapeCompNrs, 0, sizeof(ShapeCompNrs));
		memset(&ShapeAreaPresence, 0, 512);
		MinAreaResistorShapeNr = -1;
		MinAreaCapacitorShapeNr = -1;
		MinAreaResistorShape = 1000.0e5 * 1000.0e5;
		MinAreaCapacitorShape = 1000.0e5 * 1000.0e5;

		for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if (Comp->Info2 == 1)
			{	// Resistor
				Area = Comp->PlacementWidth * Comp->PlacementHeight;

				if (Area < MinAreaResistorShape)
				{
					MinAreaResistorShapeNr = Comp->ShapeNr;
					MinAreaResistorShape = Area;
					HeightResistorShape = Comp->PlacementHeight;
					WidthResistorShape = Comp->PlacementWidth;
				}
			}

			if (Comp->Info2 == 2)
			{	// Capacitor
				Area = Comp->PlacementWidth * Comp->PlacementHeight;

				if (Area < MinAreaCapacitorShape)
				{
					MinAreaCapacitorShapeNr = Comp->ShapeNr;
					MinAreaCapacitorShape = Area;
					HeightCapacitorShape = Comp->PlacementHeight;
					WidthCapacitorShape = Comp->PlacementWidth;
				}
			}
		}

//    ********************************************************************************
//    ************ Place resistors ***************************************************
//    ********************************************************************************

		PlaceY = BoardMinY;

//      PlaceY=Design.BoardOriginY;

		First = 1;

		if (MinAreaResistorShapeNr != -1)
		{
			ShapeAreaPresence[MinAreaResistorShapeNr] = 1;

			for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 1) && (Comp->ShapeNr == MinAreaResistorShapeNr))
				{
					if (First)
					{
						First = 0;
						PlaceX = BoardMinX;

//                      PlaceX=Design.BoardOriginX;

						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceX2 = PlaceX;
						PlaceY -= HeightResistorShape;
						MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
					}
					else
					{
						if (PlaceX > Design.BoardWidth + Design.BoardOriginX)
						{
							PlaceY -= HeightResistorShape + (5 * 2540.0);
							PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
							MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
							PlaceX = PlaceX2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceX += WidthResistorShape + (5 * 2540.0);
					PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ********** Other resistors *****************************************************
//    ********************************************************************************

		memset(&ShapeArea, 0, sizeof(ShapeArea));
		NrShapeAreas = 0;

		for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info2 == 1)	// Resistor
			        && (Comp->Info3 == 0) && (ShapeAreaPresence[Comp->ShapeNr] == 0))
			{
				Area = Comp->PlacementWidth * Comp->PlacementHeight;
				cnt2 = 0;

				while ((cnt2 < NrShapeAreas) && (Area > ShapeArea[cnt2]))
					cnt2++;

				if (cnt2 == NrShapeAreas)
				{
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					NrShapeAreas++;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
				}
				else
				{
					memmove(&ShapeArea[cnt2 + 1], &ShapeArea[cnt2], (NrShapeAreas - cnt2) * 4);
					memmove(&ShapeCompNrs[cnt2 + 1], &ShapeCompNrs[cnt2], (NrShapeAreas - cnt2) * 2);
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
					NrShapeAreas++;
				}
			}
		}


		for (cnt2 = 0; cnt2 < NrShapeAreas; cnt2++)
		{
			First = 1;

			for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 1)	// Resistor
				        && (Comp->ShapeNr == ShapeCompNrs[cnt2]))
				{
					if (First)
					{
						First = 0;
						HeightShape = Comp->PlacementHeight;
						WidthShape = Comp->PlacementWidth;
						PlaceX = Design.BoardOriginX;
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceX2 = PlaceX;
						PlaceY = MinY - HeightShape - (Comp->CompOriginY - Comp->BoardPosMaxY) - (5 * 2540.0);
						MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
					}
					else
					{
						if (PlaceX > Design.BoardWidth + Design.BoardOriginX)
						{
							PlaceY -= HeightShape + (5 * 2540.0);
							PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
							MinY = PlaceY + (Comp->CompOriginY - Comp->BoardPosMaxY);
							PlaceX = PlaceX2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceX += WidthShape + (5 * 2540.0);
					PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ******** Place capacitors ******************************************************
//    ********************************************************************************

		PlaceX = Design.BoardOriginX + Design.BoardWidth;
		First = 1;

		if (MinAreaCapacitorShapeNr != -1)
		{
			ShapeAreaPresence[MinAreaCapacitorShapeNr] = 1;

			for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 2) && (Comp->ShapeNr == MinAreaCapacitorShapeNr))
				{
					if (First)
					{
						First = 0;
						PlaceX += WidthCapacitorShape;
						MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceY = BoardMinY;

//                      PlaceY=Design.BoardOriginY;

						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
						PlaceY2 = PlaceY;
					}
					else
					{
						if (PlaceY > Design.BoardHeight + Design.BoardOriginY)
						{
							PlaceX += WidthCapacitorShape + (5 * 2540.0);
							MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
							PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
							PlaceY = PlaceY2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					SetBoardPosComp(Comp, 0);
					Comp->Info3 = 1;
					PlaceY += HeightCapacitorShape + (5 * 2540.0);
					PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ********** Other capacitors ****************************************************
//    ********************************************************************************

		memset(&ShapeArea, 0, sizeof(ShapeArea));
		NrShapeAreas = 0;

		for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info2 == 2)	// Capacitor
			        && (Comp->Info3 == 0) && (ShapeAreaPresence[Comp->ShapeNr] == 0))
			{
				Area = Comp->PlacementWidth * Comp->PlacementHeight;
				cnt2 = 0;

				while ((cnt2 < NrShapeAreas) && (Area > ShapeArea[cnt2]))
					cnt2++;

				if (cnt2 == NrShapeAreas)
				{
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					NrShapeAreas++;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
				}
				else
				{
					memmove(&ShapeArea[cnt2 + 1], &ShapeArea[cnt2], (NrShapeAreas - cnt2) * 4);
					memmove(&ShapeCompNrs[cnt2 + 1], &ShapeCompNrs[cnt2], (NrShapeAreas - cnt2) * 2);
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
					NrShapeAreas++;
				}
			}
		}

//    ********************************************************************************

		for (cnt2 = 0; cnt2 < NrShapeAreas; cnt2++)
		{
			First = 1;

			for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info2 == 2)	// Capacitor
				        && (Comp->ShapeNr == ShapeCompNrs[cnt2]))
				{
					if (First)
					{
						First = 0;
						HeightShape = Comp->PlacementHeight;
						WidthShape = Comp->PlacementWidth;
						PlaceX = MaxX + WidthShape + (Comp->CompOriginX - Comp->BoardPosMaxX) + (5 * 2540.0);
						MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceY = Design.BoardOriginY;
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
						PlaceY2 = PlaceY;
					}
					else
					{
						if (PlaceY > Design.BoardHeight + Design.BoardOriginY)
						{
							PlaceX += WidthShape + (5 * 2540.0);
							MaxX = PlaceX + (Comp->BoardPosMaxX - Comp->CompOriginX);
							PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
							PlaceY = PlaceY2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceY += HeightShape + (5 * 2540.0);
					PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ************ Other shapes ******************************************************
//    ********************************************************************************

		PlaceX = Design.BoardOriginX + Design.BoardWidth;
		First = 1;
		memset(&ShapeArea, 0, sizeof(ShapeArea));
		NrShapeAreas = 0;

		for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

			if ((Comp->Info3 == 0) && (Comp->Info2 == 0) && (ShapeAreaPresence[Comp->ShapeNr] == 0))
			{
				Area = Comp->PlacementWidth * Comp->PlacementHeight;
				cnt2 = 0;

				while ((cnt2 < NrShapeAreas) && (Area > ShapeArea[cnt2]))
					cnt2++;

				if (cnt2 == NrShapeAreas)
				{
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					NrShapeAreas++;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
				}
				else
				{
					memmove(&ShapeArea[cnt2 + 1], &ShapeArea[cnt2], (NrShapeAreas - cnt2) * 4);
					memmove(&ShapeCompNrs[cnt2 + 1], &ShapeCompNrs[cnt2], (NrShapeAreas - cnt2) * 2);
					ShapeArea[cnt2] = Area;
					ShapeCompNrs[cnt2] = Comp->ShapeNr;
					ShapeAreaPresence[Comp->ShapeNr] = 1;
					NrShapeAreas++;
				}
			}
		}

//    ********************************************************************************

		MaxY = BoardMaxY;

//      MaxY=Design.BoardOriginY+Design.BoardHeight;

		for (cnt2 = 0; cnt2 < NrShapeAreas; cnt2++)
		{
			First = 1;

			for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
			{
				Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

				if ((Comp->Info3 == 0) && (Comp->Info2 == 0) && (Comp->ShapeNr == ShapeCompNrs[cnt2]))
				{
#ifdef _DEBUG

					if (stricmpOwn(Comp->Name, "D38") == 0)
					{
						ok = 1;
					}

#endif

					if (First)
					{
						First = 0;
						HeightShape = Comp->PlacementHeight;
						WidthShape = Comp->PlacementWidth;
						PlaceX = BoardMinX;
						PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
						PlaceX2 = PlaceX;
						PlaceY = MaxY + HeightShape + (Comp->CompOriginY - Comp->BoardPosMaxY) + (5 * 2540.0);
						MaxY = PlaceY - (Comp->CompOriginY - Comp->BoardPosMaxY);
						PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
						PlaceY2 = PlaceY;
					}
					else
					{
						if (PlaceX > Design.BoardWidth + Design.BoardOriginX)
						{
							PlaceY += HeightShape + (5 * 2540.0);
							MaxY = PlaceY - (Comp->CompOriginY - Comp->BoardPosMaxY);
							PlaceY = AdjustToGrid(PlaceY, (5 * 2540.0));
							PlaceX = PlaceX2;
						}
					}

					Comp->CompOriginX = (float) PlaceX;
					Comp->CompOriginY = (float) PlaceY;
					Comp->Info3 = 1;
					SetBoardPosComp(Comp, 0);
					PlaceX += WidthShape + (5 * 2540.0);
					PlaceX = AdjustToGrid(PlaceX, (5 * 2540.0));
				}
			}
		}

//    ********************************************************************************
//    ********************************************************************************

		ok = 1;
	}

	for (cnt = TempNrComps; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & (OBJECT_NOT_VISIBLE)) == 0)
		{
			Comp->Info &= ((~COMPONENT_EXIST) | 0x00ff);
			Comp->DeleteNr = 0;
		}
	}

#ifdef _DEBUG
	Comp = (CompRecord *) & (CompsMem[(*Comps)[162]]);
#endif

//  ViewWholeDesign(1);
//  CheckInputMessages();

	ReadNetList2(FileName, 0);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

		if (stricmpOwn(Comp->Name, "R84") == 0)
		{
			MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			ok = 1;
		}

#endif
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeNetlistFromDesign(int32 mode)
{
	int32 Count, cnt2, pos, LengthLine, count2, PinOffset, MemPos, cnt, ShapePos, NrPins, PinNr, PinsMemPos, MemPosComp,
	      NetNr, ShapeInfo, NrPinShapes, ShapeType, ShapeNr, fp;
	int32 Found;
	CompRecord *Comp;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	ShapeRecord *Shape;
	CompPinRecord *CompPin;
	NetRecord *Net;
	NetItemsRecord *NetItem;
	char Line2[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], FileStr[MAX_LENGTH_STRING], str[2048];
	struct tm* today;
	time_t ltime;

#ifdef _DEBUG
	int32 PolygonVertices, ok;
#endif

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Count = 0;
		Net->Pos = 0;
	}

//  cnt3=LoadShape("pci_slot2");

#ifdef _DEBUG
	Comp = (CompRecord *) & (CompsMem[(*Comps)[162]]);
	Comp = (CompRecord *) & (CompsMem[(*Comps)[1]]);
	ShapeNr = (int32) Comp->ShapeNr;
	PolygonVertices = 0;
	MemPos = (*Shapes)[ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#endif

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
#ifdef _DEBUG

		if (cnt == 161)
			ok = 1;

#endif

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
			PinsMemPos = MemPosComp + sizeof(CompRecord);
			ShapeNr = (int32) Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			ShapePos = MemPos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
#ifdef _DEBUG

			if (ShapeNr < 0)
				ok = 1;

			if (stricmpOwn(Comp->Name, "L1") == 0)
				ok = 1;

#endif

//        if (stricmpOwn(Shape->ShapeName,"pci_slot")==0) {
//        ShapeNr=cnt3;
//        MemPos=(*Shapes)[ShapeNr].ShapePos;
//        Shape=(ShapeRecord *)&(ShapesMem[MemPos]);
//        }

			PinOffset = Shape->PinOffset;
			NrPins = Shape->NrPins;
			MemPos += PinOffset;
			PinNr = 0;
			Found = 0;

			while (NrPins > 0)
			{
				ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
				NrPinShapes = ShapePad->NrPinShapes;
				MemPos += sizeof(ShapePadRecord);
				CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				NetNr = CompPin->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);
					Net->Count++;
				}
				else
				{
					if (NetNr != -1)
						CompPin->NetNr = -1;
				}

//              PinTextFound=(ShapePad->Name);

				for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
				{
					Pad = (PadRecord *) & (ShapesMem[MemPos]);
					ShapeType = Pad->ShapeType;

					if (ShapeType != PIN_ARC)
						MemPos += sizeof(PadRecord);
					else
						MemPos += 48;
				}

				PinsMemPos += sizeof(CompPinRecord);
				NrPins--;

//              PinNr++;

			}
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	AllocateMemNetItems(Count);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
			PinsMemPos = MemPosComp + sizeof(CompRecord);
			ShapeNr = (int32) Comp->ShapeNr;
			MemPos = (*Shapes)[ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

//        if (stricmpOwn(Shape->ShapeName,"pci_slot")==0) {
//        ShapeNr=cnt3;
//        MemPos=(*Shapes)[ShapeNr].ShapePos;
//        Shape=(ShapeRecord *)&(ShapesMem[MemPos]);
//        }

			PinOffset = Shape->PinOffset;
			ShapeInfo = Shape->Info;
			NrPins = Shape->NrPins;
			MemPos += PinOffset;
			ShapePos = MemPos;
			PinNr = 0;
			Found = 0;

			while (NrPins > 0)
			{
				ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
				NrPinShapes = ShapePad->NrPinShapes;
				MemPos += sizeof(ShapePadRecord);
				CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
				NetNr = CompPin->NetNr;

				if ((NetNr >= 0) && (NetNr < Design.NrNets))
				{
					Net = &((*Nets)[NetNr]);
					NetItem = &((*NetItems)[Net->Pos]);
					sprintf(NetItem->PinStr, "%s-%s", Comp->Name, (ShapePad->Name));
					NetItem->CompNr = cnt;
					NetItem->Info = 0;
					Net->Pos++;
				}

				for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
				{
					Pad = (PadRecord *) & (ShapesMem[MemPos]);
					ShapeType = Pad->ShapeType;

					if (ShapeType != PIN_ARC)
						MemPos += sizeof(PadRecord);
					else
						MemPos += 48;
				}

				PinsMemPos += sizeof(CompPinRecord);
				NrPins--;
				PinNr++;
			}
		}
	}

	Count = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->Pos = Count;
		Count += Net->Count;
	}

	if ((mode & 1) == 1)
	{

		sprintf(FileStr, "%s\\pcb\\Netlist.net", DesignPath); //nový název souboru

//		sprintf(FileStr, "%s\\pcb\\design.net", DesignPath); //pùvodní název souboru

		//********************************************** nelze vytvoøit soubor *********************************************************
		if ((fp = FileOpenWriteUTF8(FileStr)) <= 0)
		{
			sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
			return -1;
		}
		//*****************************************************************************************************************************

	    //***** tisk netlistu *********************************************************************************************************
		WriteLn(fp, "# PCB elegance components and netlist file");
		WriteLn(fp, "#");

		time(&ltime);
		today = localtime(&ltime);
		strftime(str, 100, "# Date : %B %d, %Y %X", today);
		WriteLn(fp, str);

		WriteLn(fp, "#");
		WriteComponents(fp);

		WriteLn(fp, "#");
		WriteLn(fp, "# Netlist");

		for (cnt = 0; cnt < Design.NrNets; cnt++)
		{
			Net = &((*Nets)[cnt]);

			if ((Net->Name[0] != 0) && (Net->Count > 0))
			{
				pos = Net->Pos;
				LengthLine = 100;

				for (cnt2 = 0; cnt2 < Net->Count; cnt2++)
				{
					NetItem = &((*NetItems)[pos + cnt2]);

					if (LengthLine > 75)
					{
						sprintf(Line2, "NET   '%s' ", Net->Name);
						count2 = max(0, 30 - (int32) strlen(Line2));
						strncat(Line2, "                               ", count2);
					}

					strcat(Line2, " ");
					strcat(Line2, NetItem->PinStr);
					LengthLine = strlen(Line2);

					if (LengthLine > 75)
						WriteLn(fp, Line2);
				}

				if (LengthLine <= 75)
					WriteLn(fp, Line2);
			}
		}

		FileClose(fp);

		//********************************************** nelze vytvoøit soubor *********************************************************
		if (WriteLnError != 0)
		{
			sprintf(str2, SC(405, "Could not write to file\n\n%s"), FileStr);
			MessageBoxOwn(PCBWindow, str2, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		}
		//******************************************************************************************************************************

		DeAllocateMemNetItems();
	}

    //************************************* Soubor síového seznamu NET vygenerován ****************************************************
	
	if ((mode & 1) == 1)
		
		MessageBoxOwn(PCBWindow, SC(437, "Netlist output ready"), SC(1, "Message"), MB_APPLMODAL | MB_OK);
		
		NrCurrentNetItems = Count;
	
		return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DeleteNetFromComponents(int32 NetNr)
{
	CompRecord *Comp;
	CompPinRecord *CompPin;
	ShapeRecord *Shape;
	int32 cnt, cnt2, MemPos;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			CompPin = (CompPinRecord *) & (CompsMem[(*Comps)[cnt] + sizeof(CompRecord)]);

			for (cnt2 = 0; cnt2 < Shape->NrPins; cnt2++)
			{
				if (CompPin->NetNr == NetNr)
					CompPin->NetNr = -1;

				CompPin += 1;
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CountNrPinsNet(int32 NetNr)
{
	CompRecord *Comp;
	CompPinRecord *CompPin;
	ShapeRecord *Shape;
	int32 cnt, cnt2, MemPos, count;

	count = 0;

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if (((Comp->Info & OBJECT_NOT_VISIBLE) == 0) && (Comp->ShapeNr != -1))
		{
			MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			CompPin = (CompPinRecord *) & (CompsMem[(*Comps)[cnt] + sizeof(CompRecord)]);

			for (cnt2 = 0; cnt2 < Shape->NrPins; cnt2++)
			{
				if (CompPin->NetNr == NetNr)
					count++;

				CompPin += 1;
			}
		}
	}

	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DeleteTracesConnectedToNetItem(int32 NetNr, NetItemsRecord * NetItem)
{
	CompRecord *Comp;
	CompPinRecord *CompPin;
	ShapeRecord *Shape;
	ObjectRecord *Object, *Object4;
	ShapePadRecord *ShapePad;
	TraceRecord *Trace;
	int32 cnt, cnt2, MemPos, Length, NrPins, PinsMemPos, PinOffset, ShapePos, count, PolygonVertices, ShapeType, Found,
	      FoundPinNr, PinNr, MemPosComp, NrPinShapes;
	LPSTR PinText;
	PadRecord *Pad;
	ObjectLineRecord *ObjectLine;
	ObjectArcRecord *ObjectArc;

	Trace = NULL;
	PolygonVertices = 0;
	Comp = (CompRecord *) & (CompsMem[(*Comps)[NetItem->CompNr]]);
	MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
	PinsMemPos = MemPosComp + sizeof(CompRecord);
	MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	Length = strlen(NetItem->PinStr);
	cnt2 = 0;
	count = 0;

	while ((cnt2 < Length) && (NetItem->PinStr[cnt2] != '-'))
		cnt2++;

	if (cnt2 == Length)
		return -1;

	PinText = &NetItem->PinStr[cnt2 + 1];

	NrPins = Shape->NrPins;
	PinNr = 0;
	FoundPinNr = -1;
	PinOffset = Shape->PinOffset;
	MemPos += PinOffset;

	while (NrPins > 0)
	{
		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
		NrPinShapes = ShapePad->NrPinShapes;

		if (stricmpOwn(ShapePad->Name, PinText) == 0)
			FoundPinNr = PinNr;

		MemPos += sizeof(ShapePadRecord);

		for (cnt = 0; cnt < NrPinShapes; cnt++)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);
			ShapeType = Pad->ShapeType;

			if (ShapeType != PIN_ARC)
				MemPos += sizeof(PadRecord);
			else
				MemPos += 48;
		}

		PinsMemPos += sizeof(CompPinRecord);
		NrPins--;
		PinNr++;
	}


	if (FoundPinNr == -1)
		return -2;

	NrObjects = 0;
	ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);

	for (cnt = 0; cnt < NrObjects; cnt++)
	{
		Object = &((*Objects)[cnt]);

		if (Object->PinNr == FoundPinNr)
		{
			FillPositionObject(Object);
			SearchMinX = Object->minx - 100000.0;
			SearchMinY = Object->miny - 100000.0;
			SearchMaxX = Object->maxx + 100000.0;
			SearchMaxY = Object->maxy + 100000.0;
			NrObjects4 = 0;
			Found = CopyTracesFromRectWindowToObjects4(-1, 0);
			Found += CopyOtherObjectsFromRectWindowToObjects4(-1, 0);

			for (cnt2 = 0; cnt2 < NrObjects4; cnt2++)
			{
				Object4 = &((*Objects4)[cnt2]);

				if (ObjectsConnected(Object, Object4))
				{
					switch (Object4->ObjectType)
					{
					case TRACE_VER:
						Trace = &((*VerTraces[Object4->Layer])[Object4->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						count++;
						break;

					case TRACE_HOR:
						Trace = &((*HorTraces[Object4->Layer])[Object4->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						count++;
						break;

					case TRACE_DIAG1:
						Trace = &((*Diag1Traces[Object4->Layer])[Object4->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						count++;
						break;

					case TRACE_DIAG2:
						Trace = &((*Diag2Traces[Object4->Layer])[Object4->TraceNr]);
						Trace->Info |= OBJECT_NOT_VISIBLE;
						count++;
						break;

					case TRACE_ALL_ANGLE:
						ObjectLine = &((*ObjectLines)[Object4->TraceNr]);
						ObjectLine->Info |= OBJECT_NOT_VISIBLE;
						count++;
						break;

					case TRACE_ARC:
						ObjectArc = &((*ObjectArcs)[Object4->TraceNr]);
						ObjectArc->Info |= OBJECT_NOT_VISIBLE;
						count++;
						break;

					case DRILL:
						ObjectArc = &((*ObjectArcs)[Object4->TraceNr]);
						ObjectArc->Info |= OBJECT_NOT_VISIBLE;
						count++;
						break;
					}

//          Trace->DeleteNr=LastActionNr;
				}
			}
		}
	}

	DeAllocateMemObjects4();
	return count;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AssignNetToCompPin(int32 NetNr, NetItemsRecord * NetItem, int32 mode)
{
	CompRecord *Comp;
	CompPinRecord *CompPin;
	ShapeRecord *Shape;
	int32 ObjectInclude;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	int32 cnt2, MemPos, Length, NrPins, PinsMemPos, PinOffset, PolygonVertices, ShapeType, ShapePos, Mirror, Layer,
	      MemPosComp, NrPinShapes;
	LPSTR PinText;

	PolygonVertices = 0;
	Comp = (CompRecord *) & (CompsMem[(*Comps)[NetItem->CompNr]]);
	Mirror = ((Comp->CompMode & 8) >> 3);
	MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
	PinsMemPos = MemPosComp + sizeof(CompRecord);
	MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
	Length = strlen(NetItem->PinStr);
	cnt2 = 0;

	while ((cnt2 < Length) && (NetItem->PinStr[cnt2] != '-'))
		cnt2++;

	if (cnt2 == Length)
		return -1;

	PinText = &NetItem->PinStr[cnt2 + 1];
	NrPins = Shape->NrPins;
	PinOffset = Shape->PinOffset;
	MemPos += PinOffset;

	while (NrPins > 0)
	{
		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		CompPin = (CompPinRecord *) & (CompsMem[PinsMemPos]);
		MemPos += sizeof(ShapePadRecord);
		NrPinShapes = ShapePad->NrPinShapes;
		ObjectInclude = 0;

		for (cnt2 = 0; cnt2 < NrPinShapes; cnt2++)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);
			Layer = Pad->Layer;

			if (CheckGeometryLayer(&Layer, Shape->NrLayers, Mirror))
				ObjectInclude = 1;

			ShapeType = Pad->ShapeType;

			if (ShapeType != PIN_ARC)
				MemPos += sizeof(PadRecord);
			else
				MemPos += 48;
		}

		if (ObjectInclude)
		{
			if (stricmpOwn(ShapePad->Name, PinText) == 0)
			{
				CompPin->NetNr = NetNr;
				return 1;
			}
		}
		else
		{
			if (stricmpOwn(ShapePad->Name, PinText) == 0)
				return 2;
		}

		PinsMemPos += sizeof(CompPinRecord);
		NrPins--;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNetNr(LPSTR NetName)
{
	int32 cnt;
	NetRecord *Net;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if (stricmpOwn(NetName, Net->Name) == 0)
			return cnt;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ReadNetList2(LPSTR FileName, int32 mode)
{
	char LineBuf[512], LineBufCopy[512], NetName[MAX_LENGTH_STRING], OldNetName[MAX_LENGTH_STRING], CompName[10],
	     str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], Properties[256],
	     NetItemStr[MAX_LENGTH_STRING];
	int32 fp, cnt, cnt2, cnt3, cnt4, Length, lengte, count, CompNr, res, lengte2, BufPos2, NrNets2, Count,
	      NetItemEqualNr, OldNrPins, Test, result, NrProperties, PropertiesPos, NrEqualNetItems, LineCount, NrEqualNets,
	      MaxEqualNetItems, NetNrMaxEqualNetItems, pos, pos1a, pos2, pos3, ok, NetListBufSize;
	uint8 *NetListBuf;
	CompRecord *Comp;
	NetRecord *Net, *Net2, *Net1a;
	NetItemsRecord *NetItem, *NetItem2;
	int32 NetEqual, NetItemEqual, Found;
	double NewTraceWidth, NewClearance;
	
	NetListBufSize = 256 * 1024;
	Net2 = NULL;
	count = 0;
	NrNets2 = 0;
	NrNetItems2 = 0;
	MaxNrNetItems2 = 0;
	NetNrMaxEqualNetItems = 0;
	OldNetName[0] = 0;
	MessageBufPos = 0;
	BufPos2 = 0;
	PropertiesPos = 0;
	NewTraceWidth = 0.0;
	NewClearance = 0.0;
	AllocateSpecialMem(MEM_POINTS, NetListBufSize, (void **) &NetListBuf);
	NetListBuf[0] = 0;
	NetListBuf[1] = 0;
	memset(Properties, 0, sizeof(Properties));
	Test = 0;

	if ((fp = TextFileOpenUTF8(FileName)) < 0)
		return -1;

	LineCount = 0;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			LineCount++;
			GetString(LineBuf, str);

			if (stricmp(str, "net") == 0)
			{
				GetString(LineBuf, NetName);

				if (stricmpUTF8(NetName, OldNetName) != 0)
				{
					if (NrNets2 + 1 >= MaxNrNets2)
					{
						if (AllocateMemNets2(NrNets2 + 128) != 0)
							return -1;
					}

#ifdef _DEBUG

					if (Properties[0] != 0)
						ok = 1;

#endif
					Net2 = &((*Nets2)[NrNets2]);
					strcpy(OldNetName, NetName);
					PropertiesPos = 0;
					memset(Net2, 0, sizeof(NetRecord));
					cnt3 = min(sizeof(Net2->Name) - 1, strlen(NetName) - 2);
					strncpy(Net2->Name, (LPSTR) & NetName[1], cnt3);
#ifdef _DEBUG

					if (stricmpOwn(Net2->Name, "vin") == 0)
						ok = 1;

#endif

					if (NewTraceWidth == 0.0)
					{
						if (InRange(Design.StandardTraceWidth, 0.0))
							Net2->TraceWidth = (8 * 2540);
						else
							Net2->TraceWidth = Design.StandardTraceWidth;
					}
					else
						Net2->TraceWidth = (float) NewTraceWidth;

					if (NewClearance == 0.0)
					{
						if (InRange(Design.StandardClearance, 0.0))
							Net2->TraceClearance = (8 * 2540);
						else
							Net2->TraceClearance = Design.StandardClearance;
					}
					else
						Net2->TraceClearance = (float) NewClearance;

					NrNets2++;
					NewTraceWidth = 0.0;
					NewClearance = 0.0;
				}

				count = 0;
				NrProperties = 0;
				
				while ((LineBuf[0] != 0) && (count < 20))
				{
					count++;
					strcpy(LineBufCopy, LineBuf);
					GetString(LineBuf, NetItemStr);

					if (NetItemStr[0] == '(')
					{

#ifdef _DEBUG
						if (stricmpOwn(NetItemStr, "R28-1") == 0)
							ok = 1;

						if (stricmpOwn(Net2->Name, "vin") == 0)
							ok = 1;

#endif
						strcpy(LineBuf, LineBufCopy);
						GetParenthesisString(LineBuf, str);
						GetCommaString(str, str2);
						GetQuoteString(str, str4);

						if (PropertiesPos + strlen(str2) + strlen(str4) + 3 < 256)
						{
							strcpy((LPSTR) & Net2->Properties[PropertiesPos], str2);
							PropertiesPos += strlen(str2) + 1;
							strcpy((LPSTR) & Net2->Properties[PropertiesPos], str4);
							PropertiesPos += strlen(str4) + 1;
							NrProperties++;
						}

						if (ScanParameters(1, str4, 0) == 1)
						{
							if (stricmp(str2, "TRACEWIDTH") == 0)
								Net2->TraceWidth = ParamsFloat[0];

							if (stricmp(str2, "CLEARANCE") == 0)
								Net2->TraceClearance = ParamsFloat[0];
						}

#ifdef _DEBUG

						if (stricmpOwn(NetItemStr, "R28-1") == 0)
							ok = 1;

						if (stricmpOwn(NetName, "'12.288_MHz'") == 0)
							ok = 1;

#endif
					}
					else
					{
#ifdef _DEBUG

						if (stricmpOwn(NetItemStr, "R28-1") == 0)
							ok = 1;

						if (stricmpOwn(NetName, "12.288_MHz") == 0)
							ok = 1;

#endif
						strcpy(CompName, NetItemStr);
						lengte = strlen(NetItemStr);
						cnt2 = 0;

						while ((cnt2 < lengte) && (CompName[cnt2] != '-'))
							cnt2++;

						if (cnt2 < lengte)
						{
							cnt3 = min(sizeof(Comp->Name) - 1, cnt2);
							CompName[cnt3] = 0;

							if ((CompNr = FindCompNr(CompName, 0)) != -1)
							{
								if (NrNetItems2 >= MaxNrNetItems2)
								{
									if (AllocateMemNetItems2(NrNetItems2 + 128) != 0)
										return -1;
								}

								NetItem = &((*NetItems2)[NrNetItems2++]);
								memset(NetItem, 0, sizeof(NetItemsRecord));
								NetItem->NetNr = NrNets2 - 1;
								NetItem->CompNr = CompNr;
								Net2->Count++;
								Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
								cnt3 = min(sizeof(NetItem->PinStr) - 1, strlen(NetItemStr));
								strncpy(NetItem->PinStr, NetItemStr, cnt3);
							
							}
							else
							{
								result = 1;
								pos3 = 0;

								while ((pos3 < BufPos2) && (stricmpOwn((LPSTR) & NetListBuf[pos3], CompName) != 0))
									pos3 += strlen((LPSTR) & NetListBuf[pos3]) + 1;

								lengte2 = strlen(CompName) + 1;

								if (pos3 == BufPos2)
								{
									if (lengte2 + BufPos2 < NetListBufSize)
									{
										memmove(&NetListBuf[BufPos2], CompName, lengte2);
										BufPos2 += lengte2;
									}

									result = 0;
								}

								if (result)
								{
									sprintf(str2, SC(438, "Component  %s  does not exist"), CompName);

									if (AddToMessageBuf(str2) != 0)
										return -1;
								}
							}
						}
					}
				}
			}
		}
	}

	TextFileClose(fp);

	Count = 0;

	for (cnt = 0; cnt < NrNets2; cnt++)
	{
		Net2 = &((*Nets2)[cnt]);
		Net2->Pos = Count;
		Net2->NetInfo = 0;
		Count += Net2->Count;
	}

	if (MessageBufPos != 0)
	{
		if (AddToMessageBuf("") != 0)
			return -1;

		if (AddToMessageBuf("------------------------------------------------------------") != 0)
			return -1;

		if (AddToMessageBuf("") != 0)
			return -1;
	}

#ifdef _DEBUG
	Comp = (CompRecord *) & (CompsMem[(*Comps)[162]]);
#endif
	NrNetItems = MakeNetlistFromDesign(0);


#ifdef _DEBUG

	for (cnt2 = 0; cnt2 < NrNetItems; cnt2++)
	{
		NetItem2 = &((*NetItems2)[cnt2]);

		if (stricmpOwn(NetItem2->PinStr, "R28-1") == 0)
			ok = 1;
	}

#endif
	
	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
		Net->NetInfo = 0;
	}


    //***************************************************************************************
    //****** Compare the two NetLists on equal nets *****************************************
	//***************************************************************************************

	NrEqualNets = 0;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);
#ifdef _DEBUG

		if (stricmpOwn(Net->Name, "N$1014") == 0)
			ok = 1;

#endif
		pos = Net->Pos;
		NetEqual = 1;
		MaxEqualNetItems = 0;
		strcpy(InfoStr, Net->Name);
		
		RedrawInfoStr(1);

		for (cnt2 = 0; cnt2 < NrNets2; cnt2++)
		{
			NrEqualNetItems = 0;
			Net2 = &((*Nets2)[cnt2]);
			pos2 = Net2->Pos;

			if ((Net2->NetInfo & 1) == 0)
			{
				for (cnt3 = 0; cnt3 < Net->Count; cnt3++)
				{
					NetItem = &((*NetItems)[pos + cnt3]);
					NetItemEqual = 0;

					for (cnt4 = 0; cnt4 < Net2->Count; cnt4++)
					{
						NetItem2 = &((*NetItems2)[pos2 + cnt4]);

						if ((NetItem2->Info & 1) == 0)
						{
							if (stricmpOwn(NetItem->PinStr, NetItem2->PinStr) == 0)
								NrEqualNetItems++;
						}
					}
				}

				ok = 1;
			}

			ok = 1;

			if (NrEqualNetItems > MaxEqualNetItems)
			{
				MaxEqualNetItems = NrEqualNetItems;
				NetNrMaxEqualNetItems = cnt2;
			}

			if ((NrEqualNetItems == Net->NrPins) && (NrEqualNetItems == Net2->Count))
			{
				Net2->NetInfo |= 3;
				Net->NetInfo |= 3;
#ifdef _DEBUG

				if (stricmpOwn(Net->Name, "vin") == 0)
					ok = 1;

#endif
				strcpy(Net->Name, Net2->Name);
				memcpy(Net->Properties, Net2->Properties, sizeof(Net2->Properties));
				Net->TraceWidth = Net2->TraceWidth;
				Net->TraceClearance = Net2->TraceClearance;
				NrEqualNets++;
			}
		}

		ok = 1;
	}

    //*****************************************************************************************
    //****** Replace the current net with a net with the most similar netitems ****************
	//*****************************************************************************************

	for (cnt2 = 0; cnt2 < NrNets2; cnt2++)
	{
		Net2 = &((*Nets2)[cnt2]);
#ifdef _DEBUG

		if (stricmp(Net2->Name, "+12A1") == 0)
			ok = 1;

		if (stricmp(Net2->Name, "+12A1") == 0)
			ok = 1;

#endif
		MaxEqualNetItems = 0;
		NetNrMaxEqualNetItems = -1;

		if ((Net2->NetInfo & 1) == 0)
		{
			pos2 = Net2->Pos;

			for (cnt = 0; cnt < Design.NrNets; cnt++)
			{
				Net = &((*Nets)[cnt]);
				NrEqualNetItems = 0;

				if ((Net->NetInfo & 1) == 0)
				{
#ifdef _DEBUG

					if (stricmp(Net->Name, "+15V") == 0)
						ok = 1;

#endif
					pos = Net->Pos;

					for (cnt3 = 0; cnt3 < Net->Count; cnt3++)
					{
						NetItem = &((*NetItems)[pos + cnt3]);
						NetItemEqual = 0;

						for (cnt4 = 0; cnt4 < Net2->Count; cnt4++)
						{
							NetItem2 = &((*NetItems2)[pos2 + cnt4]);

							if ((NetItem2->Info & 1) == 0)
							{
								if (stricmpOwn(NetItem->PinStr, NetItem2->PinStr) == 0)
								{
									NrEqualNetItems++;
									NetItemEqual = 1;
									NetItem2->Info |= 1;
									NetItemEqualNr = cnt4;
								}
							}
						}

#ifdef _DEBUG

						if (stricmpOwn(NetItem->PinStr, "JP1-1") == 0)
							ok = 1;

#endif

						if (NetItemEqual)
						{
#ifdef _DEBUG

							if (stricmpOwn(NetItem->PinStr, "JP1-1") == 0)
								ok = 1;

#endif
							NetItem->Info |= 1;
						}
						else
							ok = 1;
					}

					ok = 1;

					if (NrEqualNetItems > MaxEqualNetItems)
					{
						MaxEqualNetItems = NrEqualNetItems;

						if (NetNrMaxEqualNetItems != -1)
						{
							Net1a = &((*Nets)[NetNrMaxEqualNetItems]);
							pos1a = Net1a->Pos;

							for (cnt3 = 0; cnt3 < Net1a->Count; cnt3++)
							{
								NetItem = &((*NetItems)[pos1a + cnt3]);
#ifdef _DEBUG

								if (NetItem->Info & 1)
									ok = 1;

#endif
								NetItem->Info &= ~1;
							}
						}

						NetNrMaxEqualNetItems = cnt;
					}
					else
					{
						if (NrEqualNetItems > 0)
						{
							for (cnt3 = 0; cnt3 < Net->Count; cnt3++)
							{
								NetItem = &((*NetItems)[pos + cnt3]);
								NetItem->Info &= ~1;
							}
						}
					}
				}
			}

			ok = 1;
			Net = &((*Nets)[NetNrMaxEqualNetItems]);
#ifdef _DEBUG

			if (stricmp(Net2->Name, "+15V") == 0)
				ok = 1;

#endif

			if ((MaxEqualNetItems > 0) && (MaxEqualNetItems > (Net->NrPins / 4)))
			{
				OldNrPins = Net->NrPins;
				Net->NrPins = 0;
				pos = Net->Pos;

				for (cnt3 = 0; cnt3 < Net->Count; cnt3++)
				{
					NetItem = &((*NetItems)[pos + cnt3]);
#ifdef _DEBUG

					if (stricmpOwn(NetItem->PinStr, "JP1-1") == 0)
						ok = 1;

					if (stricmpOwn(NetItem->PinStr, "JP1-2") == 0)
						ok = 1;

#endif

					if ((NetItem->Info & 1) == 0)
						res = DeleteTracesConnectedToNetItem(NetNrMaxEqualNetItems, NetItem);
				}

				DeleteNetFromComponents(NetNrMaxEqualNetItems);

				for (cnt4 = 0; cnt4 < Net2->Count; cnt4++)
				{
					NetItem2 = &((*NetItems2)[pos2 + cnt4]);
#ifdef _DEBUG

					if (stricmpOwn(NetItem2->PinStr, "R20-1") == 0)
						ok = 1;

#endif

					if ((res = AssignNetToCompPin(NetNrMaxEqualNetItems, NetItem2, 0)) != 1)
					{
						if (res == 0)
						{
							sprintf(str2, SC(439, "Netitem  %s  Net  %s  does not exist"), NetItem2->PinStr,
							        Net2->Name);
						}
						else
							sprintf(str2, SC(440, "No pad for netitem  %s  Net  %s"), NetItem2->PinStr, Net2->Name);

						if (AddToMessageBuf(str2) != 0)
							return -1;
					}
					else
						Net->NrPins++;
				}

				Net2->NetInfo |= 1;
				Net->NetInfo |= 1;

#ifdef _DEBUG

				if (stricmpOwn(Net->Name, "vin") == 0)
					ok = 1;

#endif
				strcpy(Net->Name, Net2->Name);
				memcpy(Net->Properties, Net2->Properties, sizeof(Net2->Properties));
				Net->TraceWidth = Net2->TraceWidth;
				Net->TraceClearance = Net2->TraceClearance;

				for (cnt4 = 0; cnt4 < NrNetItems; cnt4++)
				{
					NetItem = &((*NetItems)[cnt4]);
					NetItem->Info &= ~1;
				}
			}
		}
	}

    //*****************************************************************************************
    //******** Place the new unprocessed nets on the current nets not used ********************
	//*****************************************************************************************

	for (cnt = 0; cnt < NrNetItems; cnt++)
	{
		NetItem = &((*NetItems)[cnt]);
		NetItem->Info &= ~1;
	}

	for (cnt = 0; cnt < NrNetItems2; cnt++)
	{
		NetItem = &((*NetItems2)[cnt]);
		NetItem->Info &= ~1;
	}

	cnt2 = 0;

	while (cnt2 < NrNets2)
	{
		Net2 = &((*Nets2)[cnt2]);
		pos2 = Net2->Pos;

		if ((Net2->NetInfo & 1) == 0)
		{
			Found = 0;
			cnt = 0;

			while ((cnt < Design.NrNets) && (!Found))
			{
				Net = &((*Nets)[cnt]);

				if ((Net->NetInfo & 1) == 0)
				{
					Found = 1;
					DeleteNetFromComponents(cnt);
					DeleteNet((int32) cnt);
					Net->NrPins = 0;

					for (cnt4 = 0; cnt4 < Net2->Count; cnt4++)
					{
						NetItem2 = &((*NetItems2)[pos2 + cnt4]);
#ifdef _DEBUG

						if (stricmpOwn(NetItem2->PinStr, "R20-1") == 0)
							ok = 1;

						Test = 0;

						if (stricmpOwn(NetItem2->PinStr, "R23-1") == 0)
						{
						
						
						}

#endif

						if ((res = AssignNetToCompPin(cnt, NetItem2, Test)) != 1)
						{
							if (res == 0)
							{
								sprintf(str2, SC(439, "Netitem  %s  Net  %s  does not exist"), NetItem2->PinStr,
								        Net2->Name);
							}
							else
								sprintf(str2, SC(440, "No pad for netitem  %s  Net  %s"), NetItem2->PinStr, Net2->Name);

							if (AddToMessageBuf(str2) != 0)
								return -1;
						}
						else
							Net->NrPins++;
					}

					Net2->NetInfo |= 1;
					Net->NetInfo |= 1;
					Net->Info = 0;
					strcpy(Net->Name, Net2->Name);
					
					Net->TraceWidth = Net2->TraceWidth;
					Net->TraceClearance = Net2->TraceClearance;
				}

				cnt++;
			}

			if (!Found)
			{
				if (Design.NrNets + 1 >= MaxNrNets)
				{
					if (AllocateMemNets(Design.NrNets + 32) != 0)
						return -1;
				}

				Net = &((*Nets)[Design.NrNets]);
				memset(Net, 0, sizeof(NetRecord));

				strcpy(Net->Name, Net2->Name);
				memcpy(Net->Properties, Net2->Properties, sizeof(Net2->Properties));

				Net->TraceWidth = Net2->TraceWidth;
				Net->TraceClearance = Net2->TraceClearance;

				for (cnt4 = 0; cnt4 < Net2->Count; cnt4++)
				{
					NetItem2 = &((*NetItems2)[pos2 + cnt4]);

					if ((res = AssignNetToCompPin(Design.NrNets, NetItem2, 0)) != 1)
					{
						if (res == 0)
						{
							sprintf(str2, SC(439, "Netitem  %s  Net  %s  does not exist"), NetItem2->PinStr,
							        Net2->Name);
						}
						else
							sprintf(str2, SC(440, "No pad for netitem  %s  Net  %s"), NetItem2->PinStr, Net2->Name);

						if (AddToMessageBuf(str2) != 0)
							return -1;
					}
					else
						Net->NrPins++;
				}

				Net2->NetInfo |= 1;
				Net->NetInfo |= 1;
				Design.NrNets++;
			}
		}

		cnt2++;
	}

    //***************************************************************************************
    //*********** Delete the nets not used anymore ******************************************
	//***************************************************************************************
	
	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

		if ((Net->NetInfo & 1) == 0)
		{
			DeleteNetFromComponents(cnt);
			DeleteNet((int32) cnt);
			
			memset(Net, 0, sizeof(NetRecord));
		}
	}

  //***************************************************************************************
  //***************************************************************************************
  //***************************************************************************************

    DeAllocateMemNetItems();
	DeAllocateMemNets2();

	if (MessageBufPos != 0)
	{
		MessageDialog(SC(1, "Message"), 0, 0);
		DeAllocateMemMessageBuf();
	}

	SetWaitCursor();
	
	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		Net = &((*Nets)[cnt]);

#ifdef _DEBUG

		if (stricmpOwn(Net->Name, "N$1000") == 0)
			ok = 1;
#endif

		if (Net->Name[0] != 0)
		{
			cnt3 = CountNrPinsNet(cnt);

			if (cnt3 < 1)
				ok = 1;
		}

		if (Net->Name[0] != 0)
		{
			strcpy(InfoStr, Net->Name);
			RedrawInfoStr(1);
			ReCalcConnectionsNet((int32) cnt, 0, 1);
			CheckForEscape();
		}
	}

	PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_CHECK_CONNECTIVITY, (LPARAM) 0);
	DataBaseChanged = 1;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ReArrangeTracesVias()
{
	int32 cnt, cnt2, Layer, MemPos, ok, NetNr2, NetNr3;

	TraceRecord *Trace;
	ViaRecord *Via;
	ShapeRecord *Shape;
	NetRecord *Net;
	CompRecord *Comp;
	AreaFillRecord *AreaFill;
	CompPinRecord *CompPin;

	for (Layer = 0; Layer < 32; Layer++)
	{
		for (cnt = 0; cnt < Design.NrVerTraces[Layer]; cnt++)
		{
			Trace = &((*VerTraces[Layer])[cnt]);

			if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);

				if (Net->Dummy != -1)
					Trace->NetNr = Net->Dummy;
			}
		}

		for (cnt = 0; cnt < Design.NrHorTraces[Layer]; cnt++)
		{
			Trace = &((*HorTraces[Layer])[cnt]);

			if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);

				if (Trace->NetNr == 301)
					ok = 1;

				if (Net->Dummy != -1)
					Trace->NetNr = Net->Dummy;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag1Traces[Layer]; cnt++)
		{
			Trace = &((*Diag1Traces[Layer])[cnt]);

			if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);

				if (Net->Dummy != -1)
					Trace->NetNr = Net->Dummy;
			}
		}

		for (cnt = 0; cnt < Design.NrDiag2Traces[Layer]; cnt++)
		{
			Trace = &((*Diag2Traces[Layer])[cnt]);

			if (((Trace->Info & OBJECT_NOT_VISIBLE) == 0) && (Trace->NetNr >= 0) && (Trace->NetNr < Design.NrNets))
			{
				Net = &((*Nets)[Trace->NetNr]);

				if (Net->Dummy != -1)
					Trace->NetNr = Net->Dummy;
			}
		}

	}

	for (cnt = 0; cnt < Design.NrVias; cnt++)
	{
		Via = &((*Vias)[cnt]);

		if (((Via->Info & OBJECT_NOT_VISIBLE) == 0) && (Via->NetNr >= 0) && (Via->NetNr < Design.NrNets))
		{
			Net = &((*Nets)[Via->NetNr]);

			if (Net->Dummy != -1)
				Via->NetNr = Net->Dummy;
		}
	}

	for (cnt = 0; cnt < Design.NrAreaFills; cnt++)
	{
		AreaFill = (AreaFillRecord *) & (AreaFillMem[(*AreaFills)[cnt]]);

		if (((AreaFill->Info & OBJECT_NOT_VISIBLE) == 0) && (AreaFill->NetNr >= 0) && (AreaFill->NetNr < Design.NrNets))
		{
			Net = &((*Nets)[AreaFill->NetNr]);

			if (Net->Dummy != -1)
				AreaFill->NetNr = Net->Dummy;
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			CompPin = (CompPinRecord *) & (CompsMem[(*Comps)[cnt] + sizeof(CompRecord)]);

			for (cnt2 = 0; cnt2 < Shape->NrPins; cnt2++)
			{
				CompPin->NetNr <<= 16;
				CompPin->NetNr += 65535;
				CompPin++;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			CompPin = (CompPinRecord *) & (CompsMem[(*Comps)[cnt] + sizeof(CompRecord)]);

			for (cnt2 = 0; cnt2 < Shape->NrPins; cnt2++)
			{
				NetNr2 = CompPin->NetNr >> 16;
				NetNr3 = CompPin->NetNr & 0x0000ffff;

				if (CompPin->NetNr >= 65536)
					ok = 1;

				if ((NetNr3 == 0xffff) && (NetNr2 >= 0) && (NetNr2 < Design.NrNets))
				{
					if (NetNr2 == 301)
						ok = 1;

					Net = &((*Nets)[NetNr2]);

					if (Net->Dummy != -1)
						CompPin->NetNr = (CompPin->NetNr & 0xffff0000) + Net->Dummy;
				}

				CompPin++;
			}
		}
	}

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);

		if ((Comp->Info & OBJECT_NOT_VISIBLE) == 0)
		{
			MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
			Shape = (ShapeRecord *) & (ShapesMem[MemPos]);
			CompPin = (CompPinRecord *) & (CompsMem[(*Comps)[cnt] + sizeof(CompRecord)]);

			for (cnt2 = 0; cnt2 < Shape->NrPins; cnt2++)
			{
				NetNr3 = CompPin->NetNr & 0x0000ffff;
				NetNr2 = CompPin->NetNr >> 16;

				if (NetNr3 == 0xffff)
				{
					if (NetNr2 == 0xffff)
						CompPin->NetNr = -1;
					else
						CompPin->NetNr = NetNr3;
				}
				else
				{
					CompPin->NetNr &= 0x0000ffff;

					if (CompPin->NetNr == 0x0000ffff)
						CompPin->NetNr = -1;
				}

				CompPin++;
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CountBitsBitmapFile(LPSTR FileName, int32 mode)
{
#define BitMapLineXY(x,y) ((BitmapMem[((y)*NrLineBytes)+((x) >> 3)] & ((0x80 >> ((x) & 7)))))
#define SetBitMapLinePointX(x)   (LineBytes[(x) >> 3]|=(0x80 >> ((x) & 7)))

	BmpHeaderRecord BmpHeader;
	char str[1024];
	uint8 LineBytes[16384], value;
	COLORREF PaletteColors[256];
	int32 MemSize, fp, result, cnt, cnt2, NrLineBytes, x, res, DataOffset, count;

	SpecialBitmapRecord SpecialBitmap;

	if ((fp = FileOpenReadOnlyUTF8(FileName)) <= 0)
		return -1;

	FileRead(fp, &BmpHeader, sizeof(BmpHeaderRecord), &result);
	DataOffset = BmpHeader.StartOfDataOffset;
	FileRead(fp, &PaletteColors, sizeof(COLORREF) * BmpHeader.NrColors1, &result);

	if (BmpHeader.HResolutionInPixelsPerMeter == 11811)
		SpecialBitmap.StandardResolution = (2540000.0 / 300);
	else
	{
		if (BmpHeader.HResolutionInPixelsPerMeter == 23622)
			SpecialBitmap.StandardResolution = (2540000.0 / 600);
		else
		{
			if (BmpHeader.HResolutionInPixelsPerMeter != 0)
				SpecialBitmap.StandardResolution = (100000000 / BmpHeader.HResolutionInPixelsPerMeter);
			else
				SpecialBitmap.StandardResolution = (2540000.0 / 300);
		}
	}

	SpecialBitmap.HResolution = SpecialBitmap.StandardResolution;
	SpecialBitmap.VResolution = SpecialBitmap.StandardResolution;

	if (BmpHeader.NrColors1 != 2)
	{
	}

	SpecialBitmap.Xoffset = (SpecialBitmap.StandardResolution * 0);
	SpecialBitmap.Yoffset = (SpecialBitmap.StandardResolution * 0);
	SpecialBitmap.Width = BmpHeader.Width;
	SpecialBitmap.Height = BmpHeader.Height;
	cnt2 = BmpHeader.FileSize / BmpHeader.Height;
	NrLineBytes = ((SpecialBitmap.Width + 15) & ~15) / 8;
	SpecialBitmap.NrLineBytes = NrLineBytes;
	MemSize = NrLineBytes * SpecialBitmap.Height + 16384;
	memset(BitmapMem, 0xff, MemSize);
	FileSeek(fp, DataOffset);
	res = FileCurrentPointer(fp);

	for (cnt = 0; cnt < SpecialBitmap.Height; cnt++)
	{
		FileRead(fp, &LineBytes, cnt2, &result);

		if ((SpecialBitmap.Width & 15) > 0)
		{
			for (x = (SpecialBitmap.Width & 15); x < 16; x++)
				SetBitMapLinePointX((SpecialBitmap.Width & ~15) + x);
		}

		memmove(&BitmapMem[cnt * NrLineBytes], &LineBytes, NrLineBytes);
	}

	count = 0;

	for (cnt = 0; cnt < SpecialBitmap.Height; cnt++)
	{
		for (x = 0; x < NrLineBytes; x++)
		{
			memmove(&value, &BitmapMem[cnt * NrLineBytes + x], 1);

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

	FileClose(fp);
	sprintf(str, SC(441, "Nr bits is %d"), count);
	MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_OK);

	return 0;
}
