/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: netlist.c
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
#include "time.h"
#include "memory.h"
#include "calc.h"
#include "files2.h"
#include "files.h"
#include "string.h"
#include "math.h"
#include "check.h"
#include "stdio.h"
#include "io.h"
#include "instance.h"
#include "nets.h"
#include "netlist.h"
#include "utf8.h"
#include "mentor.h"
#include "resource.h"
#include "ctype.h"

//#define _DEBUG
#define  MAX_NR_GLOBAL_NETS    400


int16 GlobalNetsPos[MAX_NR_GLOBAL_NETS], GlobalNetsInfo[MAX_NR_GLOBAL_NETS], NrGlobalNets;

typedef char NetItemsArray[10000][20];

typedef struct
{
	int32 NetNr, Progress;
} NetConnectionRecord;

typedef struct
{
	LPSTR PowerNetName, NetName;
	int32 NetNr;
} SpecialPowerNetNamesRecord;

typedef NetConnectionRecord NetConnectionArray[1000];

typedef struct
{
	int32 GlobalPowerNetNr, NrPowerNames, PowerNetNrs[8];
} PowerNetNrRecord;

typedef PowerNetNrRecord PowerNetNrArray[128];

int32 GlobalNets[1024];

uint8 NetItemsBuf[256 * 1024];
HGLOBAL NetItemsBufGlobal;

PowerNetNrArray PowerNetNrs;

SpecialPowerNetNamesRecord SpecialPowerNetNames[1024];

int32 NrGlobalPowerNetNrs, NrPinsForLicenseCheck, NrSpecialPowerNetNames, UnNamedGlobalNetNr;


char GlobalConnectionNetsStrings[512][64];
int32 NrGlobalConnectionNetsStrings, ok;

extern int32 WriteLnError;
extern PCBDesignRecord PcbDesign;
extern ProjectInfoRecord *ProjectInfo;
extern ShapesArray *Shapes;


int32 CheckGlobals(int32 * SheetErrorNr, double *x, double *y);

int32 CheckEmptyNets(int32 SheetNr, double *x, double *y);

int32 AddPowerPinNets(void);

int32 MakeNets(void);

int32 MakeNets2(void);

int32 WriteNetInfo(void);

int32 PrepareNetOutput(void);

int32 CheckGeometries(void);

int32 WriteGeometries(void);

int32 CheckERC(void);

int32 WriteNetlist(void);

int32 GetNetNameGlobalBus(int32 GlobalNetIndex, LPSTR GlobalNetName);

int32 GetNetName(int32 NetNr, LPSTR NetName);

int32 CollectBussesAndWires(void);

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckSheets(int32 Mode, double *cx, double *cy)
{
	int32 cnt2, TotalErrors, NeedAnnotation;
	char FileName[MAX_LENGTH_STRING];

//  MessageBuf[0]=0;

	NeedAnnotation = 0;
	TotalErrors = 0;

//  sprintf(str,"Checking sheets\r\n");
//  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);
		Sheets[cnt2].Info = 0;
		LoadSheetInMemory(cnt2, 0);

		if (LoadSheetInMemory(cnt2, 0) < 0)
			return -2;

//    sprintf(str,"Checking sheet %s\r\n",FileName);
//    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		if (CheckSheetForErrors(FileName, 0, cx, cy))
			return cnt2;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectsPosition1(int32 value)
{
	if ((NrInt32Objects1 + 1 >= MaxNrInt32Objects1) && (AllocateMemObjectsPosition1(MaxNrInt32Objects1 + 128) != 0))
		return -1;

	(*ObjectsPosition1)[NrInt32Objects1] = value;
	NrInt32Objects1++;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddObjectsInfo(int32 NetLabelNr, int32 pos, int32 PinBusNr)
{
	if ((NrObjectsInfo + 1 >= MaxNrObjectsInfo) && (AllocateMemObjectsInfo(MaxNrObjectsInfo + 128) != 0))
		return -1;

//  if (NrObjectsInfo==548) {
//    ok=1;
//  }
	(*ObjectsInfo)[NrObjectsInfo].Name = NetLabelNr;
	(*ObjectsInfo)[NrObjectsInfo].Info = (int16) PinBusNr;
	(*ObjectsInfo)[NrObjectsInfo + 1].Pos = pos;
	NrObjectsInfo++;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PinTypeToText(int32 ConnectionType, LPSTR Text)
{
	char Line[MAX_LENGTH_STRING];

	switch (ConnectionType & 0xff)
	{
	case CONNECTION_INPUT:
		strcpy(Line, "IN ");
		break;

	case CONNECTION_OUTPUT:
		strcpy(Line, "OUT ");
		break;

	case CONNECTION_IO:
		strcpy(Line, "IO ");
		break;

	case CONNECTION_TRISTATE:
		strcpy(Line, "TRI ");
		break;

	case CONNECTION_OC:
		strcpy(Line, "OC ");
		break;

	case CONNECTION_PASSIVE:
		strcpy(Line, "PAS ");
		break;

	case CONNECTION_POWER:
		strcpy(Line, "POW ");
		break;
	}

	switch ((ConnectionType >> 8) & 0xff)
	{
	case CONNECTION_UNDEFINED:
		strcat(Line, "UNDEF");
		break;

	case CONNECTION_TTL:
		strcat(Line, "TTL");
		break;

	case CONNECTION_LVTTL:
		strcat(Line, "LVTTL");
		break;

	case CONNECTION_CMOS:
		strcat(Line, "CMOS");
		break;

	case CONNECTION_ANALOG:
		strcat(Line, "ANALOG");
		break;

	case CONNECTION_POWER:
//      strcat(Line," ");
		break;
	}

	strcpy(Text, Line);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckObjectCount(int32 MaxObjectCount, int32 mode)
{
	double cc2, cc3;
	int32 cc;

	if (mode == 0)
	{
		cc2 = MaxObjectCount;
		cc3 = sqrt(cc2 / 314);
		cc2 = cc3 * cc3;
		cc = (int32) cc2;

		if (NrPinsForLicenseCheck > cc)
			return 1;
	}

	return 0;
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNumberOfUsedPins(int32 Mode)
{
	int32 res, cnt, cnt2, start, stop, TotalNrPins;
	char FileName[MAX_LENGTH_STRING];
	Object2Record *Object2;
	double x, y;
	NrObjects2 = 0;
	TotalNrNets = 0;
	NrInt32Objects1 = 0;
	NrObjects4 = 0;
//  ObjectTextBufPos=0;
	Sheets[0].NetPos = 0;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
//    MessageBuf[0]=0;
		strcpy(FileName, SheetDir);
		strcat(FileName, Sheets[cnt2].SheetName);
		strcat(FileName, ".sch");
//    sprintf(str,"Checking nets sheet %s\r\n",FileName);
//    SendMessage(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		Sheets[cnt2].Info = 0;
		LoadSheetInMemory(cnt2, 5);
		res = nets1(cnt2, &x, &y);

		if (res == -2)
			return -1;

//    return 0;
	}

	TotalNrPins = 0;

	for (cnt = 0; cnt < TotalNrNets; cnt++)
	{
		start = (*NetInfos)[cnt].Pos;
		stop = (*NetInfos)[cnt + 1].Pos;

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			Object2 = &((*Objects2)[cnt2]);

			switch (Object2->ObjectType)
			{
			case SYMBOL_PIN:
				if (Object2->Info3 == 0)
					TotalNrPins++;

				break;

			case SYMBOL_PINBUS:
				TotalNrPins += Object2->Info3;
				break;

			case SYMBOL_POWERPIN:
				TotalNrPins++;
				break;
			}
		}
	}

	return TotalNrPins;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BuildNets(int32 Mode, double *x, double *y)
{
	int32 res, cnt, cnt2, start, stop, SheetNr, NrObjectPinBusses, TotalNrPins;
	char FileName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	Object2Record *Object2;

	sprintf(str, SC(175, "Checking nets\r\n"));
	AddMessage(str);

	NrObjects2 = 0;
	TotalNrNets = 0;
	NrObjects4 = 0;
	UnNamedGlobalNetNr = 10000;

//  ObjectTextBufPos=0;
	Sheets[0].NetPos = 0;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
//    MessageBuf[0]=0;
		strcpy(FileName, SheetDir);
		strcat(FileName, Sheets[cnt2].SheetName);
		strcat(FileName, ".sch");

//    sprintf(str,"Checking nets sheet %s\r\n",FileName);

		Sheets[cnt2].Info = 0;
		LoadSheetInMemory(cnt2, 5);
		res = nets1(cnt2, x, y);

		if (res == -2)
			return 1000 + cnt2;

		if (!CheckEmptyNets(cnt2, x, y))
			return 1000 + cnt2;
	}

	sprintf(str, SC(173, "Checking nets OK\r\n"));
	AddMessage(str);

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

	NrObjectPinBusses = 0;
	TotalNrPins = 0;

	for (cnt = 0; cnt < TotalNrNets; cnt++)
	{
		start = (*NetInfos)[cnt].Pos;
		stop = (*NetInfos)[cnt + 1].Pos;

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			Object2 = &((*Objects2)[cnt2]);

			switch (Object2->ObjectType)
			{
			case SYMBOL_PIN:
				if (Object2->Info3 == 0)
					TotalNrPins++;

				break;

			case SYMBOL_PINBUS:
				NrObjectPinBusses++;
				TotalNrPins += Object2->Info3;
				break;

			case SYMBOL_POWERPIN:
				TotalNrPins++;
				break;
			}
		}
	}

	NrPinsForLicenseCheck = TotalNrPins;

// **********************************************************************************

	if (!CheckGlobals(&SheetNr, x, y))
		return 1000 + SheetNr;

	if (!AddPowerPinNets())
		return -1;

#ifdef _DEBUG

	if (WriteNetInfo() == -1)
		return -1;

#endif

	if (!MakeNets())
		return -1;

	if (MakeNets2() == -1)
		return -1;

	if (PrepareNetOutput() == -1)
		return -1;

	if (CheckGeometries() == -1)
		return -1;

//  if (CheckERC()) return -1;
	if (!WriteNetlist())
		return -1;

	sprintf(str, SC(176, "Components and netlist (%s\\pcb\\%s.net) are generated\r\n"), DesignPath, LayoutFile);

	AddMessage(str);

	if (CreateGatePinSwapInfo(0) < 0)
		return -1;

	strcpy(str, SC(177, "Output to netlist/component is ready\r\n"));

	AddMessage(str);

	CollectBussesAndWires();
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteNetInfo()
{
	int32 cnt, cnt2, start, stop, ShCnt, fp;
	char FileName[MAX_LENGTH_STRING], *StrPos, Line[MAX_LENGTH_STRING], Line2[MAX_LENGTH_STRING],
	     str[MAX_LENGTH_STRING], SheetStr[MAX_LENGTH_STRING];
	Object2Record *Object2;

	sprintf(str, "%s\\nets.net", DesignPath); //název souboru

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return -1;

	ShCnt = 0;

	for (cnt = 0; cnt < TotalNrNets; cnt++)
	{
		if (cnt == Sheets[ShCnt].NetPos)
		{
			sprintf(FileName, "%s%s.sch", SheetDir, Sheets[ShCnt].SheetName);
			sprintf(SheetStr, "SHEET %2d %s                                 ", ShCnt, Sheets[ShCnt].SheetName);
			SheetStr[24] = '|';
			SheetStr[25] = ' ';
			SheetStr[26] = 0;
			WriteLn(fp, "------------------------------------------------------------------------------------------");
			sprintf(Line, "SHEET %s", FileName);
			WriteLn(fp, Line);
			WriteLn(fp, "------------------------------------------------------------------------------------------");
			ShCnt++;
		}

		start = (*NetInfos)[cnt].Pos;
		stop = (*NetInfos)[cnt + 1].Pos;

		if (cnt == PowerPinNetPos)
		{
			sprintf(Line, "\r\nPOWER PINS");
			WriteLn(fp, Line);
			SheetStr[0] = 0;
		}

		if (cnt < PowerPinNetPos)
		{
			sprintf(Line, "%sNET %i ITEMS %i    %08X", SheetStr, cnt, stop - start, (*NetInfos)[cnt].Info);
			WriteLn(fp, Line);
		}
		else
		{
			Object2 = &((*Objects2)[start]);
			sprintf(Line, "%sNET %i %s ITEMS %i    %08X", SheetStr, cnt, GetObjectText(Object2->Text1), stop - start,
			        (*NetInfos)[cnt].Info);
			WriteLn(fp, Line);
		}

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			Object2 = &((*Objects2)[cnt2]);

			switch (Object2->ObjectType)
			{
			case SYMBOL_PIN:
				PinTypeToText(Object2->Info2, Line2);

				if (Object2->Info3 == 0)
				{
					sprintf(Line, "%sCOMP     %s PIN %s %s %s", SheetStr, GetObjectText(Object2->RefNum),
					        GetObjectText(Object2->Text1), GetObjectText(Object2->Text2), Line2);
				}
				else
				{
					sprintf(Line, "%sCOMP     %s SHEETPIN %s %s %s NET %i", SheetStr, GetObjectText(Object2->RefNum),
					        GetObjectText(Object2->Text1), GetObjectText(Object2->Text2), Line2, Object2->Info4);
				}

				WriteLn(fp, Line);
				break;

			case SYMBOL_PINBUS:
				PinTypeToText(Object2->Info2, Line2);
				StrPos = GetObjectText(Object2->RefNum);
				sprintf(Line, "%sCOMP     %s PINBUS %s %i %s %s", SheetStr, GetObjectText(Object2->RefNum),
				        GetObjectText(Object2->Text1), Object2->Info3, GetObjectText(Object2->Text2), Line2);
				WriteLn(fp, Line);
				break;

			case SYMBOL_POWERPIN:
				sprintf(Line, "%sCOMP     %s POWERPINS %s", SheetStr, GetObjectText(Object2->RefNum),
				        GetObjectText(Object2->Text2));
				WriteLn(fp, Line);
				break;

			case WIRE:
				sprintf(Line, "%sWIRE     %i,%i - %i,%i", SheetStr, (int32) Object2->x1, (int32) Object2->y1,
				        (int32) Object2->x2, (int32) Object2->y2);
				WriteLn(fp, Line);
				break;

			case BUS:
				sprintf(Line, "%sBUS     %i,%i - %i,%i", SheetStr, (int32) Object2->x1, (int32) Object2->y1,
				        (int32) Object2->x2, (int32) Object2->y2);
				WriteLn(fp, Line);
				break;

			case BUS_CONNECTION:
				if (Object2->Info3 == 0)
				{
					sprintf(Line, "%sBUSCON   %i,%i TO NET %d", SheetStr, (int32) Object2->x1, (int32) Object2->y1,
					        Object2->Info4);
				}
				else
					sprintf(Line, "%sBUSCON   %i,%i", SheetStr, (int32) Object2->x1, (int32) Object2->y1);

				WriteLn(fp, Line);
				break;

			case GLOBAL_CONNECTION:
				switch (Object2->Info2 >> 1)
				{
				case 0:		// input
					sprintf(Line2, "INPUT");
					break;

				case 1:		// output
					sprintf(Line2, "OUTPUT");
					break;

				case 2:		// i/o
					sprintf(Line2, "IO");
					break;
				}

				sprintf(Line, "%sGLOBAL   %s %s NET %i", SheetStr, Line2, GetObjectText(Object2->Text1),
				        Object2->Info4);
				WriteLn(fp, Line);
				break;

			case NET_LABEL:
				sprintf(Line, "%sNETLABEL %s", SheetStr, GetObjectText(Object2->Text1));
				WriteLn(fp, Line);
				break;
			}
		}

		sprintf(Line, "%sEND", SheetStr);
		WriteLn(fp, Line);
		sprintf(Line, "%s", SheetStr);
		WriteLn(fp, Line);
	}

	FileClose(fp);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckGlobals(int32 * SheetErrorNr, double *x, double *y)
{
	int32 cnt, cnt2, start, stop, PinNetNr, count, SheetNr, SheetAbove;
	char SheetName[MAX_LENGTH_STRING], Line[MAX_LENGTH_STRING];
	Object2Record *Object2, *Object2a;

	for (cnt2 = 0; cnt2 < NrObjects2; cnt2++)
	{
		Object2 = &((*Objects2)[cnt2]);

		if ((Object2->ObjectType == GLOBAL_CONNECTION) && ((SheetNr = Object2->SheetNr) != -1))
		{
			SheetAbove = Sheets[SheetNr].LinkToAbove;

			if (SheetAbove >= 0)
			{
				strcpy(SheetName, Sheets[SheetNr].SheetName);
				start = (*NetInfos)[Sheets[SheetAbove].NetPos].Pos;
				stop = (*NetInfos)[Sheets[SheetAbove].NetPos + Sheets[SheetAbove].NrNets].Pos;
				count = 0;

				for (cnt = start; cnt < stop; cnt++)
				{
					Object2a = &((*Objects2)[cnt]);

					if ((Object2a->ObjectType == SYMBOL_PIN) && (Object2a->Info3 == 1))
					{	// sheet symbol pin
						if ((stricmpUTF8(SheetName, GetObjectText(Object2a->Text1)) == 0)
						        && (stricmpUTF8(GetObjectText(Object2->Text1), GetObjectText(Object2a->Text2)) == 0))
						{
							Object2->Info4 = Object2a->NetNr;
							Object2a->Info4 = Object2->NetNr;
							PinNetNr = Object2a->NetNr;
							ok = 1;
							count++;
						}
					}
				}

				if (count == 0)
				{
					sprintf(Line, SC(178, "External connection %s (%i,%i) sheet %s.sch is not connected\r\n"),
					        GetObjectText(Object2->Text1), (int32) (Object2->x1), (int32) (Object2->y1), SheetName);
					AddMessage(Line);
					*SheetErrorNr = SheetNr;
					*x = Object2->x1;
					*y = Object2->y1;
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
					return 0;
				}
			}
			else
			{
				sprintf(Line, SC(179, "External connection %s (%i,%i) sheet %s.sch "), GetObjectText(Object2->Text1),
				        (int32) (Object2->x1), (int32) (Object2->y1), SheetName);
				strcat(Line, SC(180, "is not connected to a corresponding sheetsymbol pin\r\n"));
				AddMessage(Line);
				*SheetErrorNr = SheetNr;
				*x = Object2->x1;
				*y = Object2->y1;
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
				return 0;
			}
		}
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckEmptyNets(int32 SheetNr, double *x, double *y)
{
	int32 Pos, cnt, cnt2, start, stop, Info, First;
	char Line[MAX_LENGTH_STRING], Line2[MAX_LENGTH_STRING];
	Object2Record *Object2;

	First = 1;
	Pos = Sheets[SheetNr].NetPos;

	for (cnt = Pos; cnt < Pos + Sheets[SheetNr].NrNets; cnt++)
	{
		Info = (*NetInfos)[cnt].Info;

		if ((Info & 0x100000) == 0x100000)
		{	// Power pin
			if (((Info & 0x40000) == 0x00000)	// Sheetpins=0
			        && ((Info & 0x100) == 0x000))
			{	// pins=0
				start = (*NetInfos)[cnt].Pos;
				stop = (*NetInfos)[cnt + 1].Pos;

				for (cnt2 = start; cnt2 < stop; cnt2++)
				{
					Object2 = &((*Objects2)[cnt2]);

					if (Object2->ObjectType == SYMBOL_PIN)
					{
						*x = Object2->x1;
						*y = Object2->y1;
						sprintf(Line2, SC(213, "Sheet %s : "), Sheets[SheetNr].SheetName);
						sprintf(Line, SC(181, "Unconnected power pin (%i,%i)\r\n"), (int32) Object2->x1,
						        (int32) Object2->y1);
						strcat(Line2, Line);
						AddMessage(Line2);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line2);
						return 0;
					}
				}
			}

			ok = 1;
		}

		if (((Info & 0xD3300) == 0x10000) || ((Info & 0xD3300) == 0x11000) || ((Info & 0xDB300) == 0x50000)
//       ||
//       ((Info & 0xDB300) == 0x58000)
		        || ((Info & 0xDB300) == 0x10100))
		{
			start = (*NetInfos)[cnt].Pos;
			stop = (*NetInfos)[cnt + 1].Pos;

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				Object2 = &((*Objects2)[cnt2]);

				if (Object2->ObjectType == WIRE)
				{
					*x = Object2->x1;
					*y = Object2->y1;
					sprintf(Line2, SC(213, "Sheet %s : "), Sheets[SheetNr].SheetName);
					sprintf(Line, SC(182, "Unconnected wire (%i,%i - %i,%i)\r\n"), (int32) Object2->x1,
					        (int32) Object2->y1, (int32) Object2->x2, (int32) Object2->y2);
					strcat(Line2, Line);
					AddMessage(Line2);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line2);
					return 0;
				}
			}

			ok = 1;
		}

		if (((Info & 0xE7C00) == 0x20400) || ((Info & 0xE7C00) == 0x21000) || ((Info & 0xE7C00) == 0x20000)
		        || ((Info & 0xE7C00) == 0x60000))
		{
			start = (*NetInfos)[cnt].Pos;
			stop = (*NetInfos)[cnt + 1].Pos;

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				Object2 = &((*Objects2)[cnt2]);

				if (Object2->ObjectType == BUS)
				{
					*x = Object2->x1;
					*y = Object2->y1;
					sprintf(Line2, SC(213, "Sheet %s : "), Sheets[SheetNr].SheetName);
					sprintf(Line, SC(183, "Unconnected bus (%i,%i - %i,%i)\r\n"), (int32) Object2->x1,
					        (int32) Object2->y1, (int32) Object2->x2, (int32) Object2->y2);
					strcat(Line2, Line);
					AddMessage(Line2);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line2);
					return 0;
				}
			}

			ok = 1;
		}

		/*
		    if (((Info & 0x00400000) == 0)
		       &&
		       (((Info & 0x000C0000) == 0x00040000)
		       ||
		       ((Info & 0x00000300) == 0x00000100))) {
		      start=(*NetInfos)[cnt].Pos;
		      stop=(*NetInfos)[cnt+1].Pos;
		      for (cnt2=start;cnt2<stop;cnt2++) {
		        Object2=&((*Objects2)[cnt2]);
		        if (Object2->ObjectType==SYMBOL_PIN) {
		          sprintf(Line2,SC(213,"Sheet %s : "),Sheets[SheetNr].SheetName);
		          sprintf(Line,"Unconnected pin (%i,%i)\r\n",
		                               (int32)Object2->x1,(int32)Object2->y1);
		          strcat(Line2,Line);
		          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line2);
		          return 0;
		        }
		      }
		      ok=1;
		    }
		*/
	}

	return 1;

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SearchGlobalConnNet(int32 mode)
{
	int32 cnt, Info;

	if (mode == 0)
	{
		for (cnt = 0; cnt < TotalNrNets; cnt++)
		{
			Info = (*NetInfos)[cnt].Info;
			Info &= ~0xff;

			if ((((*NetInfos)[cnt].Progress & 5) == 0) && ((Info & 0x1000) == 0x1000))
				return cnt;		// External connection
		}
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeNets()
{
	int32 cnt, cnt2, cnt3, cnt4, start, stop, NetLabelObjectNr, NetNr, NetNr2, Info, StartNet, EndNet, NetCount,
	      SheetNr2, GlobalConnObjectNr;
	char GlobalNetName[MAX_LENGTH_STRING], Line[MAX_LENGTH_STRING], Line2[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	LPSTR GlobalConnectionStr, TempGlobalConnectionStr;
	int32 TotalErrors, Stop, GlobalNetIsBus;
	Object2Record *Object2a, *Object2b, *Object2c, *NetLabelObject;
#ifdef _DEBUG
	char str[MAX_LENGTH_STRING];
	int32 fp;
#endif

	TotalErrors = 0;
	GlobalNetsPos[0] = 0;
	NrGlobalNets = 0;
	NetCount = 0;
	NrGlobalConnectionNetsStrings = 0;

	while ((NetNr = SearchGlobalConnNet(0)) != -1)
	{
		ok = 1;
// ********************************************************************************
// Search for highest sheet level which has no external connection
// This sheet netnr will be the top of the tree
		Stop = 0;
		NetNr2 = NetNr;
		cnt = 0;

		while ((!Stop) && ((GlobalConnObjectNr = (*NetInfos)[NetNr].GlobalConnObjectNr) != -1))
		{
			Object2a = &((*Objects2)[GlobalConnObjectNr]);
			NetNr = Object2a->Info4;

			if (NetNr >= Sheets[NrSheets].NetPos)
				ok = 1;

			cnt++;

			if (cnt == 1000)
				Stop = 1;

			ok = 1;
		}

		cnt = NetCount;
		(*NetInfos)[NetNr].Progress |= 1;
		GlobalNets[NetCount] = NetNr;
		NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr;

		if (NetLabelObjectNr == -1)
		{
			sprintf(str2, "N$%i", UnNamedGlobalNetNr);
			GlobalConnectionStr = str2;
			UnNamedGlobalNetNr++;
		}
		else
		{
			NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
			GlobalConnectionStr = GetObjectText(NetLabelObject->Text1);
		}

#ifdef _DEBUG

		if (stricmp(GlobalConnectionStr, "Vdr") == 0)
			ok = 1;

#endif
		cnt2 = 0;

		while ((cnt2 < NrGlobalConnectionNetsStrings)
		        && (stricmpUTF8(GlobalConnectionNetsStrings[cnt2], GlobalConnectionStr) != 0))
			cnt2++;

		if (cnt2 < NrGlobalConnectionNetsStrings)
		{
			sprintf(Line,
			        SC(251,
			           "Error: There are two or more global nets with the same name (%s) who are not connected\r\n"),
			        GlobalConnectionStr);
			AddMessage(Line);
		}

#ifdef _DEBUG

		if (stricmp(GlobalConnectionStr, "vee") == 0)
			ok = 1;

#endif

		NetCount++;
		GlobalNetIsBus = 0;

		do
		{
// ********************************************************************************
// Search for sheet pins on net NetNr
			NetNr = GlobalNets[cnt];
#ifdef _DEBUG

			if (NetNr == 16)
				ok = 1;

#endif
			Info = (*NetInfos)[NetNr].Info;

			if ((Info & 0x40000) == 0x40000)
			{	// ObjectSheetPins
// Sheet pins found
				if ((Info & 0x20000) == 0x20000)
					GlobalNetIsBus = 1;

				start = (*NetInfos)[NetNr].Pos;
				stop = (*NetInfos)[NetNr + 1].Pos;

				for (cnt2 = start; cnt2 < stop; cnt2++)
				{
					Object2b = &((*Objects2)[cnt2]);

					if ((Object2b->ObjectType == SYMBOL_PIN) && (Object2b->Info3 == 1))
					{
						NetNr2 = Object2b->Info4;

						if (NetNr2 != -1)
						{
							if (((*NetInfos)[NetNr2].Progress & 1) == 0)
							{
								(*NetInfos)[NetNr2].Progress |= 1;

								// Insert net sheet pin into GlobalNets
								if (NetCount < 1000)
								{
									GlobalNets[NetCount] = NetNr2;
									NetCount++;
									// ********************************************************************************
									// Disable all other nets in sheet with NetNr2 with the same global connection
									SheetNr2 = (*NetInfos)[NetNr2].SheetNr;
									StartNet = Sheets[SheetNr2].NetPos;
									EndNet = StartNet + Sheets[SheetNr2].NrNets;

									for (cnt4 = StartNet; cnt4 < EndNet; cnt4++)
									{
										Info = (*NetInfos)[cnt4].Info;

										if ((((*NetInfos)[cnt4].Progress & 1) == 0) && ((Info & 0x1000) == 0x1000))
										{
											GlobalConnObjectNr = (*NetInfos)[cnt4].GlobalConnObjectNr;
											Object2c = &((*Objects2)[GlobalConnObjectNr]);
											TempGlobalConnectionStr = GetObjectText(Object2c->Text1);

											if (stricmpUTF8(TempGlobalConnectionStr, GlobalConnectionStr) == 0)
											{
												(*NetInfos)[cnt4].Progress |= 4;
												/*
												                        if (NetCount<1000) {
												                          GlobalNets[NetCount]=cnt4;
												                          NetCount++;
												                        }
												*/
											}
										}
									}

									// ********************************************************************************
								}
								else
									ok = 1;
							}
						}
					}
				}
			}

			cnt++;
		}
		while (cnt < NetCount);

		if (NrGlobalNets < MAX_NR_GLOBAL_NETS)
		{
			GlobalNetsInfo[NrGlobalNets] = 0;

			if (GlobalNetIsBus)
				GlobalNetsInfo[NrGlobalNets] = 1;

			if (GlobalConnectionStr[0])
			{
				if (NrGlobalConnectionNetsStrings < 512)
					strcpy(GlobalConnectionNetsStrings[NrGlobalConnectionNetsStrings++], GlobalConnectionStr);
			}

			NrGlobalNets++;
#ifdef _DEBUG

			if (NrGlobalNets == 39)
				ok = 1;

#endif
			GlobalNetsPos[NrGlobalNets] = (int16) NetCount;
		}
	}

	ok = 1;

// ********************************************************************************
// Search for local busses to be included in the global nets
	for (cnt = 0; cnt < TotalNrNets; cnt++)
	{
		Info = (*NetInfos)[cnt].Info;
#ifdef _DEBUG

		if (cnt == 595)
			ok = 1;

#endif

		if (((Info & 0x2C000) == 0x2C000) && (((*NetInfos)[cnt].Progress & 1) == 0))
		{
			ok = 1;

			if ((Info & 0x400) == 0)
				(*NetInfos)[cnt].Progress = 1;

			if (NrGlobalNets < MAX_NR_GLOBAL_NETS)
			{
				GlobalNets[NetCount] = cnt;
				NetCount++;
				GlobalNetsInfo[NrGlobalNets] = 5;
				NrGlobalNets++;
				GlobalNetsPos[NrGlobalNets] = (int16) NetCount;
			}
		}
	}

// ********************************************************************************
// Search for local pinbusses to be included in the global nets
	for (cnt = 0; cnt < TotalNrNets; cnt++)
	{
		Info = (*NetInfos)[cnt].Info;
#ifdef _DEBUG

		if (cnt == 595)
			ok = 1;

#endif

		if (((Info & 0x2A400) == 0x28400) && (((*NetInfos)[cnt].Progress & 1) == 0))
		{
//      (*NetInfos)[cnt].Progress=1;
			if (NrGlobalNets < MAX_NR_GLOBAL_NETS)
			{
				GlobalNets[NetCount] = cnt;
				NetCount++;
				GlobalNetsInfo[NrGlobalNets] = 5;
				NrGlobalNets++;
				GlobalNetsPos[NrGlobalNets] = (int16) NetCount;
			}
		}
	}



	for (cnt2 = 0; cnt2 < NrGlobalNets; cnt2++)
	{
		if ((GlobalNetsInfo[cnt2] & 1) == 1)
		{
			for (cnt3 = GlobalNetsPos[cnt2]; cnt3 < GlobalNetsPos[cnt2 + 1]; cnt3++)
			{
				NetNr = GlobalNets[cnt3];
#ifdef _DEBUG

				if (NetNr == 595)
					ok = 1;

#endif

				if (((*NetInfos)[NetNr].Info & 0x400) == 0x000)
					(*NetInfos)[NetNr].Progress = 1;
				else
				{
					(*NetInfos)[NetNr].Progress = 0;
					GlobalNetsInfo[cnt2] |= 2;
				}
			}
		}
		else
		{
			for (cnt3 = GlobalNetsPos[cnt2]; cnt3 < GlobalNetsPos[cnt2 + 1]; cnt3++)
			{
				NetNr = GlobalNets[cnt3];
#ifdef _DEBUG

				if (NetNr == 595)
					ok = 1;

#endif
				(*NetInfos)[NetNr].Progress = 0;
			}
		}
	}


#ifdef _DEBUG
	sprintf(str, "%s\\global.net", DesignPath); //název souboru

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return 0;

#endif

	for (cnt2 = 0; cnt2 < NrGlobalNets; cnt2++)
	{
		GlobalNetName[0] = 0;

		for (cnt3 = GlobalNetsPos[cnt2]; cnt3 < GlobalNetsPos[cnt2 + 1]; cnt3++)
		{
			if (GetNetName(GlobalNets[cnt3], Line2))
			{
				if (GlobalNetName[0] == 0)
				{
					strcpy(GlobalNetName, Line2);

#ifdef _DEBUG

					if ((GlobalNetsInfo[cnt2] & 1) == 0)
					{
						sprintf(str, "GLOBALNET %s", GlobalNetName);

						if (stricmp(GlobalNetName, "TXD0") == 0)
							ok = 1;
					}
					else
					{
						if ((GlobalNetsInfo[cnt2] & 4) == 0)
							sprintf(str, "GLOBAL BUS %s", GlobalNetName);
						else
							sprintf(str, "LOCAL BUS %s", GlobalNetName);
					}

					if ((GlobalNetsInfo[cnt2] & 2) == 2)
						strcat(str, " (Only pinbus)");

					WriteLn(fp, str);
#endif
				}
				else
				{
					if (stricmpUTF8(GlobalNetName, Line2) != 0)
					{
//            TotalErrors=1;
						if ((GlobalNetsInfo[cnt2] & 1) == 0)
						{
							sprintf(Line, SC(184, "Warning: Different name %s for global net %s\r\n"), Line2,
							        GlobalNetName);
							AddMessage(Line);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
						}
						else
						{
							sprintf(Line, SC(185, "Warning: Different name %s for global bus %s\r\n"), Line2,
							        GlobalNetName);
							AddMessage(Line);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
						}
					}
				}
			}
			else
				ok = 1;

#ifdef _DEBUG
			sprintf(str, "NET %i", GlobalNets[cnt3]);
			WriteLn(fp, str);
#endif
		}

#ifdef _DEBUG
		sprintf(str, "END");
		WriteLn(fp, str);
#endif
	}

#ifdef _DEBUG
	FileClose(fp);
#endif
	ok = 1;
	return !TotalErrors;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BusConnectionConnectedToGlobalBus(int32 NetInfoNr, int32 GlobalNetIndex)
{
	int32 cnt3, GlobalNetNr, BusConnectionObjectNr;
	Object2Record *BusConnectionObject;

	if ((BusConnectionObjectNr = (*NetInfos)[NetInfoNr].BusConnectionObjectNr) == -1)
		return 0;

	BusConnectionObject = &((*Objects2)[BusConnectionObjectNr]);

	for (cnt3 = GlobalNetsPos[GlobalNetIndex]; cnt3 < GlobalNetsPos[GlobalNetIndex + 1]; cnt3++)
	{
		GlobalNetNr = GlobalNets[cnt3];

		if (BusConnectionObject->Info4 == GlobalNetNr)
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNetNameGlobalBus(int32 GlobalNetIndex, LPSTR GlobalNetName)
{
	int32 cnt3, GlobalNetNr, NetLabelNr, NetLabelObjectNr;
	Object2Record *NetLabelObject;
	LPSTR NetLabelName;

	for (cnt3 = GlobalNetsPos[GlobalNetIndex]; cnt3 < GlobalNetsPos[GlobalNetIndex + 1]; cnt3++)
	{
		GlobalNetNr = GlobalNets[cnt3];

		if ((NetLabelObjectNr = (*NetInfos)[GlobalNetNr].NetLabelObjectNr) != -1)
		{
			NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
			NetLabelNr = NetLabelObject->Text1;
			NetLabelName = GetObjectText(NetLabelNr);
			strcpy(GlobalNetName, NetLabelName);
			return 1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetNetName(int32 NetNr, LPSTR NetName)
{
	int32 NetLabelNr, NetLabelObjectNr;
	Object2Record *NetLabelObject;
	LPSTR NetLabelName;

	if ((NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr) == -1)
		return 0;

	NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
	NetLabelNr = NetLabelObject->Text1;
	NetLabelName = GetObjectText(NetLabelNr);
	strcpy(NetName, NetLabelName);
	return 1;
}

// ********************************************************************************
// ********************************************************************************
// ********************************************************************************
// ********************************************************************************

void CheckCascadedNetObjects(int32 NetNr, LPSTR NetLabelName, int fp)
{
	int32 cnt6, cnt7, FoundNr, GlobalNetNr2;
	char str[MAX_LENGTH_STRING];

	FoundNr = -1;

	for (cnt6 = 0; cnt6 < NrGlobalNets; cnt6++)
	{
		for (cnt7 = GlobalNetsPos[cnt6]; cnt7 < GlobalNetsPos[cnt6 + 1]; cnt7++)
		{
			if ((GlobalNetNr2 = GlobalNets[cnt7]) == NetNr)
				FoundNr = cnt6;
		}
	}

	if (FoundNr != -1)
	{
		for (cnt6 = GlobalNetsPos[FoundNr]; cnt6 < GlobalNetsPos[FoundNr + 1]; cnt6++)
		{
			if ((GlobalNetNr2 = GlobalNets[cnt6]) != NetNr)
			{
				if (((*NetInfos)[GlobalNetNr2].Info & 0x2000) == 0)
				{	// NrBusConnections1=0
					if (AddObjectsPosition1(GlobalNetNr2 << 16) == -1)
						return;

					(*NetInfos)[GlobalNetNr2].Progress |= 2;
#ifdef _DEBUG
					sprintf(str, SC(187, "nr %i"), GlobalNetNr2);
					WriteLn(fp, str);
#endif
				}
				else
				{
					sprintf(str, SC(186, "Warning: Can not connect net %s into two busses\r\n"), NetLabelName);
					AddMessage(str);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				}
			}
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AddNetNr(int32 fp, int32 NetNr, LPSTR NetLabelName, int32 PinBusNr, int32 mode)
{
	char str[MAX_LENGTH_STRING];

#ifdef _DEBUG
	int32 ok;

	if (NetNr == 372)
		ok = 1;

	if (((mode == 2) || (mode == 3) || (mode == 4)) && (stricmp(NetLabelName, "TAG7") == 0))
		ok = 1;

#endif

	switch (mode)
	{
	case 0:
		sprintf(str, SC(187, "nr %i"), NetNr);
		WriteLn(fp, str);
		break;

	case 1:
		sprintf(str, SC(188, "nr %i %i"), NetNr, PinBusNr);
		WriteLn(fp, str);
		break;

	case 2:
		sprintf(str, "NET %s", NetLabelName);
		WriteLn(fp, str);
		sprintf(str, SC(187, "nr %i"), NetNr);
		WriteLn(fp, str);
		break;

	case 3:
		sprintf(str, "NET %s", NetLabelName);
		WriteLn(fp, str);
		sprintf(str, SC(188, "nr %i %i"), NetNr, PinBusNr);
		WriteLn(fp, str);
		break;

	case 4:
		sprintf(str, "NET %i %s", NetNr, NetLabelName);
		WriteLn(fp, str);
		break;
	}

}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 FindAndInsertNets(int32 NetNr, LPSTR NetLabelName, int32 GlobalNetIndex, int fp)
{
	int32 Changed = 0;
	int32 cnt3, cnt4, cnt5, GlobalNetNr, NetLabelObjectNr2, BusConnectionObjectNr2, BusConnectionObjectNr, PinBusNetNr,
	      Info, result, NetLabelObjectNr, SheetNr2, StartNet, EndNet, ok;
	Object2Record *BusConnectionObject, *BusConnectionObject2, *NetLabelObject2;
	LPSTR NetLabelStr, PinBusStr;
	char str[MAX_LENGTH_STRING];
	int64 Presence;

	BusConnectionObject2 = NULL;
	NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr;
	result = 0;

	for (cnt3 = GlobalNetsPos[GlobalNetIndex]; cnt3 < GlobalNetsPos[GlobalNetIndex + 1]; cnt3++)
	{
		GlobalNetNr = GlobalNets[cnt3];

		SheetNr2 = (*NetInfos)[GlobalNetNr].SheetNr;
		StartNet = Sheets[SheetNr2].NetPos;
		EndNet = StartNet + Sheets[SheetNr2].NrNets;

// ********************************************************************************
// Start search for objects in sheet SheetNr2
// ********************************************************************************
		for (cnt4 = StartNet; cnt4 < EndNet; cnt4++)
		{
			Info = (*NetInfos)[cnt4].Info;
#ifdef _DEBUG

			if (cnt4 == 223)
				ok = 1;

#endif

// BusConnection
// Netlabel
			if (((Info & 0xa000) == 0xa000)	// NrBusConnections1>0 , NrNetLabels>0
			        && (((*NetInfos)[cnt4].Progress & 3) == 0))
			{
// ********************************************************************************
// Label with a bus connection (Bus is GlobalNetNr)
// ********************************************************************************
				NetLabelObjectNr2 = (*NetInfos)[cnt4].NetLabelObjectNr;
				NetLabelObject2 = &((*Objects2)[NetLabelObjectNr2]);
				BusConnectionObjectNr2 = (*NetInfos)[cnt4].BusConnectionObjectNr;
				BusConnectionObject2 = &((*Objects2)[BusConnectionObjectNr2]);

				if ((BusConnectionObject2->Info4 == GlobalNetNr) && (NetLabelObjectNr2 != NetLabelObjectNr))
				{
					if (((*NetInfos)[cnt4].Info & 0x400) == 0)
					{
						NetLabelStr = GetObjectText(NetLabelObject2->Text1);

						if (stricmpUTF8(NetLabelName, NetLabelStr) == 0)
						{
#ifdef _DEBUG
							AddNetNr(fp, cnt4, 0, 0, 0);
//              sprintf(str,SC(187,"nr %i"),cnt4);
//              WriteLn(fp,str);
#endif

							if (((Info & 0x40000) == 0x40000)	// SheetPin
							        || ((Info & 0x1000) == 0x1000))
							{	// External connection
								CheckCascadedNetObjects(cnt4, NetLabelName, fp);
							}

							if (AddObjectsPosition1(cnt4 << 16) == -1)
								return -1;

							(*NetInfos)[cnt4].Progress |= 2;
							Changed = 1;
							ok = 1;
						}
					}
					else
					{
// Label name with a busconnection is in pin bus
						PinBusStr = GetObjectText(NetLabelObject2->Text1);
						ok = 1;

						if (strnicmpUTF8(PinBusStr, NetLabelName, 1) == 0)
						{
							for (cnt5 = 0; cnt5 < (*NetInfos)[cnt4].NrPinsPinBus; cnt5++)
							{
								if ((GetLabelBusNameByIndex(PinBusStr, str, cnt5) != -1)
								        && (((*NetInfos)[cnt4].Presence & INT64SHIFT(cnt5)) == 0))
								{
									ok = 1;

									if (stricmpUTF8(NetLabelName, str) == 0)
									{
#ifdef _DEBUG
										AddNetNr(fp, cnt4, 0, cnt5, 1);
//                    sprintf(str,SC(188,"nr %i %i"),cnt4,cnt5);
//                    WriteLn(fp,str);
#endif

										if (AddObjectsPosition1((cnt4 << 16) + cnt5) == -1)
											return -1;

										(*NetInfos)[cnt4].PinBusCount++;

										if ((*NetInfos)[cnt4].NrPinsPinBus == (*NetInfos)[cnt4].PinBusCount)
										{
											(*NetInfos)[cnt4].Progress |= 2;
// All signal names inside a pinbus are evaluated, so pinbus is evaluated
										}

										(*NetInfos)[cnt4].Presence |= INT64SHIFT(cnt5);
										Changed = 1;
										ok = 1;
									}
								}
							}
						}

						ok = 1;
					}
				}
			}

// Pinbus
// Check in busses
			if (((Info & 0x20400) == 0x20400)	// NrPinBusses>0 , NrBusses>0
			        && (((*NetInfos)[cnt4].Progress & 3) == 0))
			{
				NetLabelObjectNr2 = (*NetInfos)[cnt4].NetLabelObjectNr;
				NetLabelObject2 = &((*Objects2)[NetLabelObjectNr2]);
				BusConnectionObjectNr2 = (*NetInfos)[cnt4].BusConnectionObjectNr;

				if (BusConnectionObjectNr2 != -1)
					BusConnectionObject2 = &((*Objects2)[BusConnectionObjectNr2]);

				PinBusStr = GetObjectText(NetLabelObject2->Text1);
				ok = 1;

				if ((BusConnectionObjectNr2 == -1) || (BusConnectionObject2->Info4 == GlobalNetNr))
				{
					if (strnicmpUTF8(PinBusStr, NetLabelName, 1) == 0)
					{
						for (cnt5 = 0; cnt5 < (*NetInfos)[cnt4].NrPinsPinBus; cnt5++)
						{
							if ((GetLabelBusNameByIndex(PinBusStr, str, cnt5) != -1)
							        && (((*NetInfos)[cnt4].Presence & INT64SHIFT(cnt5)) == 0))
							{
								ok = 1;

								if (stricmpUTF8(NetLabelName, str) == 0)
								{
#ifdef _DEBUG
									AddNetNr(fp, cnt4, 0, cnt5, 1);
									//                sprintf(str,SC(188,"nr %i %i"),cnt4,cnt5);
									//                WriteLn(fp,str);
#endif

									if (AddObjectsPosition1((cnt4 << 16) + cnt5) == -1)
										return -1;

									(*NetInfos)[cnt4].PinBusCount++;

									if ((*NetInfos)[cnt4].NrPinsPinBus == (*NetInfos)[cnt4].PinBusCount)
									{
										(*NetInfos)[cnt4].Progress |= 2;
										// All signal names inside a pinbus are evaluated, so pinbus is evaluated
									}

									(*NetInfos)[cnt4].Presence |= INT64SHIFT(cnt5);
									Changed = 1;
									ok = 1;
								}
							}
						}
					}
				}

				ok = 1;
			}

			if (((Info & 0xa000) == 0x8000)	// NrBusConnections1=0 , NrNetLabels>0
			        && (((*NetInfos)[cnt4].Progress & 3) == 0))
			{
// ********************************************************************************
// Label
// ********************************************************************************
				NetLabelObjectNr2 = (*NetInfos)[cnt4].NetLabelObjectNr;
				NetLabelObject2 = &((*Objects2)[NetLabelObjectNr2]);

				if (NetLabelObjectNr2 != NetLabelObjectNr)
				{
					if (((*NetInfos)[cnt4].Info & 0x400) == 0)
					{	// NrPinBusses=0
						NetLabelStr = GetObjectText(NetLabelObject2->Text1);

						if (stricmpUTF8(NetLabelName, NetLabelStr) == 0)
						{
#ifdef _DEBUG
							AddNetNr(fp, cnt4, 0, 0, 0);
//              sprintf(str,SC(187,"nr %i"),cnt4);
//              WriteLn(fp,str);
#endif

							if (AddObjectsPosition1(cnt4 << 16) == -1)
								return -1;

							if ((Info & 0x1000) == 0x1000)
							{	// External connection
								CheckCascadedNetObjects(cnt4, NetLabelName, fp);
							}

							(*NetInfos)[cnt4].Progress |= 2;
						}
					}
				}
			}
		}
	}

// ********************************************************************************
// ********************************************************************************
	if ((BusConnectionObjectNr = (*NetInfos)[NetNr].BusConnectionObjectNr) != -1)
	{
		BusConnectionObject = &((*Objects2)[BusConnectionObjectNr]);
		PinBusNetNr = BusConnectionObject->Info4;

		if ((PinBusNetNr != -1) && (((*NetInfos)[PinBusNetNr].Info & 0x400) == 0x400))
		{	// NrPinBusses>0
			NetLabelObjectNr2 = (*NetInfos)[PinBusNetNr].NetLabelObjectNr;
			NetLabelObject2 = &((*Objects2)[NetLabelObjectNr2]);
			PinBusStr = GetObjectText(NetLabelObject2->Text1);
			Presence = (*NetInfos)[PinBusNetNr].Presence;

//      Presence=(__int64)0;
			for (cnt5 = 0; cnt5 < (*NetInfos)[PinBusNetNr].NrPinsPinBus; cnt5++)
			{
				if ((GetLabelBusNameByIndex(PinBusStr, str, cnt5) != -1) && ((Presence & INT64SHIFT(cnt5)) == 0))
				{
					ok = 1;

					if (stricmpUTF8(NetLabelName, str) == 0)
					{
#ifdef _DEBUG
						AddNetNr(fp, PinBusNetNr, 0, cnt5, 1);
//            sprintf(str,SC(188,"nr %i %i"),PinBusNetNr,cnt5);
//            WriteLn(fp,str);
#endif

						if (AddObjectsPosition1((PinBusNetNr << 16) + cnt5) == -1)
							return -1;

						(*NetInfos)[PinBusNetNr].PinBusCount++;

						if ((*NetInfos)[PinBusNetNr].NrPinsPinBus == (*NetInfos)[PinBusNetNr].PinBusCount)
						{
							(*NetInfos)[PinBusNetNr].Progress |= 2;
// All signal names inside a pinbus are evaluated, so pinbus is evaluated
						}

						(*NetInfos)[PinBusNetNr].Presence |= INT64SHIFT(cnt5);
						Changed = 1;
						ok = 1;
					}
				}
			}

			ok = 1;
		}

	}

	if (Changed)
		result |= 1;

	return result;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InsertOtherLabels(int32 NetNr, int32 NetLabelObjectNr, LPSTR NetLabelName, int32 fp, int32 mode)
{
	int32 cnt4, SheetNr2, StartNet, EndNet, Info, BusConnectionObjectNr, BusConnectionObjectNr2, NetLabelObjectNr2;
	Object2Record *BusConnectionObject, *BusConnectionObject2, *NetLabelObject2;
	LPSTR NetLabelStr;
#ifdef _DEBUG
	int32 ok;
#endif

#ifdef _DEBUG

	if (NetNr == 9)
		ok = 1;

#endif
	BusConnectionObject = NULL;
	SheetNr2 = (*NetInfos)[NetNr].SheetNr;
	StartNet = Sheets[SheetNr2].NetPos;
	EndNet = StartNet + Sheets[SheetNr2].NrNets;

	if ((BusConnectionObjectNr = (*NetInfos)[NetNr].BusConnectionObjectNr) != -1)
		BusConnectionObject = &((*Objects2)[BusConnectionObjectNr]);

// ********************************************************************************
// Start search for objects in sheet SheetNr2
// ********************************************************************************
	for (cnt4 = StartNet; cnt4 < EndNet; cnt4++)
	{
		Info = (*NetInfos)[cnt4].Info;
#ifdef _DEBUG

		if (cnt4 == 3)
			ok = 1;

#endif

		if (((Info & 0xa000) == 0xa000)	// NrBusConnections1>0 , NrNetLabels>0
		        && (((*NetInfos)[cnt4].Progress & 3) == 0) && (BusConnectionObjectNr != -1))
		{
// ********************************************************************************
// Label with a bus connection (Bus is GlobalNetNr)
// ********************************************************************************

			NetLabelObjectNr2 = (*NetInfos)[cnt4].NetLabelObjectNr;
			NetLabelObject2 = &((*Objects2)[NetLabelObjectNr2]);
			BusConnectionObjectNr2 = (*NetInfos)[cnt4].BusConnectionObjectNr;
			BusConnectionObject2 = &((*Objects2)[BusConnectionObjectNr2]);

			if ((BusConnectionObject2->Info4 == BusConnectionObject->Info4) && (NetLabelObjectNr2 != NetLabelObjectNr))
			{
				NetLabelStr = GetObjectText(NetLabelObject2->Text1);

				if ((stricmpUTF8(NetLabelName, NetLabelStr) == 0) && (mode == 0))
				{
#ifdef _DEBUG
					AddNetNr(fp, cnt4, 0, 0, 0);
//          sprintf(str,SC(187,"nr %i"),cnt4);
//          WriteLn(fp,str);
#endif

					if (AddObjectsPosition1(cnt4 << 16) == -1)
						return;

					(*NetInfos)[cnt4].Progress |= 2;
				}
			}
		}

		if (((Info & 0xa000) == 0x8000)	// NrBusConnections1=0 , NrNetLabels>0
		        && (((*NetInfos)[cnt4].Progress & 3) == 0))
		{
// ********************************************************************************
// Label without a bus connection
// ********************************************************************************
			NetLabelObjectNr2 = (*NetInfos)[cnt4].NetLabelObjectNr;

			if (NetLabelObjectNr2 != -1)
			{
				NetLabelObject2 = &((*Objects2)[NetLabelObjectNr2]);

				if (NetLabelObjectNr2 != NetLabelObjectNr)
				{
					if (((*NetInfos)[cnt4].Info & 0x400) == 0)
					{
						NetLabelStr = GetObjectText(NetLabelObject2->Text1);

						if ((stricmpUTF8(NetLabelName, NetLabelStr) == 0) && (mode == 0))
						{
#ifdef _DEBUG
							AddNetNr(fp, cnt4, 0, 0, 0);
//              sprintf(str,SC(187,"nr %i"),cnt4);
//              WriteLn(fp,str);
#endif
							AddObjectsPosition1(cnt4 << 16);
							(*NetInfos)[cnt4].Progress |= 2;
						}
					}
				}
			}
		}

#if 0

		if (((Info & 0xb000) == 0x1000)	// NrBusConnections1=0 , NrNetLabels=0, NrGlobalConnections>0
		        && (((*NetInfos)[cnt4].Progress & 3) == 0))
			&&(mode == 0))
		{
// ********************************************************************************
// Net with no label and no bus connection and a global connection
// ********************************************************************************
#ifdef _DEBUG
			AddNetNr(fp, cnt4, 0, 0, 0);
//              sprintf(str,SC(187,"nr %i"),cnt4);
//              WriteLn(fp,str);
#endif
			AddObjectsPosition1(cnt4 << 16);
			(*NetInfos)[cnt4].Progress |= 2;
		}

#endif
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckSpecialPowerNetNames(LPSTR NetName)
{
	int32 cnt;

	for (cnt = 0; cnt < NrSpecialPowerNetNames; cnt++)
	{
		if (stricmpUTF8(SpecialPowerNetNames[cnt].NetName, NetName) == 0)
			return cnt;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MakeNets2()
{
	int32 cnt2, cnt3, cnt4, cnt5, cnt6, cnt7, start, stop, FoundNr, NetLabelObjectNr, Changed, NetNr, Info, SearchNetNr,
	      result, StartNet, EndNet, NetLabelObjectNr2, GlobalConnObjectNr, PowerNetName, NetLabelNr, UnNamedNetNr,
	      OldNrObjectsInfo, FoundNr2, GlobalNetNr2, fp, GlobalNetNumbers[100], NrGlobalNetNumbers, PowerObjectNr,
	      SheetNr2, cnt3a, Evaluate, TempPowerNetName[MAX_LENGTH_STRING], NrTempPowerNetNames;
	char str2[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], *NetLabelName, *PinBusStr, *PinBusStr2 =
	    NULL, *NetLabelStr, *NetLabelName2 = NULL;
	LPSTR Text2, T1, T2, PowerNetStr;
	int64 Presence;
	Object2Record *Object2, *Object2a, *Object2b, *NetLabelObject, *NetLabelObject2;

#ifdef _DEBUG
	int32 res;
#endif

	fp = -1;

#ifdef _DEBUG
	sprintf(str, "%s\\nets2.net", DesignPath); //název souboru

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return -1;

#endif
	NrInt32Objects1 = 0;
	NrInt32Objects2 = 0;
	FoundNr = 0;
	UnNamedNetNr = 1000;
	NrSpecialPowerNetNames = 0;

	if ((NrObjectsInfo + 1 >= MaxNrObjectsInfo) && (AllocateMemObjectsInfo(MaxNrObjectsInfo + 128) != 0))
		return -1;

	(*ObjectsInfo)[0].Pos = 0;
	NrObjectsInfo = 0;

// ********************************************************************************
// ********************************************************************************
// Check which nets have a power conncetion
// ********************************************************************************
// ********************************************************************************

	for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
	{
		Info = (*NetInfos)[NetNr].Info;
#ifdef _DEBUG
		res = (*NetInfos)[NetNr].Progress & 3;

		if (NetNr == 372)
			ok = 1;

#endif
		// NrBusConnections1>0   -> 0x2000
		// NrNetLabels>0         -> 0x8000
		// NrPowerPins>0         -> 0x100000

		if ((Info & 0x10A000) == 0x10A000)
		{	// NrPowerPins>0 , NrNetLabels>0  , NrBusConnections1>0
			NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr;

			if (NetLabelObjectNr != -1)
			{
				NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
				NetLabelName = GetObjectText(NetLabelObject->Text1);
				start = (*NetInfos)[NetNr].Pos;
				stop = (*NetInfos)[NetNr + 1].Pos;

				for (cnt2 = start; cnt2 < stop; cnt2++)
				{
					Object2 = &((*Objects2)[cnt2]);

					if ((Object2->ObjectType == SYMBOL_PIN) && (Object2->Info2 == POWER_CONNECTION))
					{
						(*NetInfos)[NetNr].PowerNetName = GetObjectText(Object2->Text2);

						if (NrSpecialPowerNetNames < 1024)
						{
							SpecialPowerNetNames[NrSpecialPowerNetNames].PowerNetName = GetObjectText(Object2->Text2);
							SpecialPowerNetNames[NrSpecialPowerNetNames].NetName = GetObjectText(NetLabelObject->Text1);
							SpecialPowerNetNames[NrSpecialPowerNetNames].NetNr = NetNr;
							NrSpecialPowerNetNames++;
						}
					}
				}
			}
		}
	}

// ********************************************************************************
// ********************************************************************************
// All nets who are part of a global net will be processed first
// ********************************************************************************
// ********************************************************************************

	for (cnt2 = 0; cnt2 < NrGlobalNets; cnt2++)
	{

// ********************************************************************************
// ********************************************************************************
// Search for Global nets with busses first
// ********************************************************************************
// ********************************************************************************
		if ((GlobalNetsInfo[cnt2] & 1) == 1)
		{
			for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
			{
				Info = (*NetInfos)[NetNr].Info;
#ifdef _DEBUG

				if (NetNr == 372)
					ok = 1;

				if (NetNr == 832)
					ok = 1;

#endif

				if ((((*NetInfos)[NetNr].Progress & 3) == 0) && ((Info & 0xA000) == 0xA000)	// NrBusConnections1>0 , NrNetLabels>0
				        && (BusConnectionConnectedToGlobalBus(NetNr, cnt2)))
				{
// ********************************************************************************
// ********************************************************************************
// Netlabel
// Busconnection1

					if (((Info & 0x0400) == 0x0000)	// NrPinBusses=0
					        && ((NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr) != -1))
					{
#ifdef _DEBUG

						if (NetNr == 594)
							ok = 1;

#endif
						NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
						NetLabelNr = NetLabelObject->Text1;
						NetLabelName = GetObjectText(NetLabelNr);

						if (CheckSpecialPowerNetNames(NetLabelName) == -1)
						{

#ifdef _DEBUG
							AddNetNr(fp, NetNr, NetLabelName, 0, 2);
							//            sprintf(str,"NET %s",NetLabelName);
							//            WriteLn(fp,str);
							//            sprintf(str,SC(187,"nr %i"),NetNr);
							//            WriteLn(fp,str);
#endif

							if (AddObjectsPosition1(NetNr << 16) == -1)
								return -1;

							result = FindAndInsertNets(NetNr, NetLabelName, cnt2, fp);

							if (((Info & 0x40000) == 0x40000)	// SheetPin
							        || ((Info & 0x1000) == 0x1000))
							{	// External connection
								CheckCascadedNetObjects(NetNr, NetLabelName, fp);
							}

							(*NetInfos)[NetNr].Progress |= 2;

							if (AddObjectsInfo(NetLabelNr, NrInt32Objects1, -1) == -1)
								return -1;

#ifdef _DEBUG
							WriteLn(fp, "END");
#endif
						}
					}
					else
						ok = 1;

// ********************************************************************************
// ********************************************************************************
// ********************************************************************************
// ********************************************************************************

					if (((Info & 0x20400) == 0x20400)	// NrPinBusses>0 , NrBusses>0
					        && ((NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr) != -1))
					{
// Pinbus
#ifdef _DEBUG
						if (NetNr == 223)
							ok = 1;

#endif
						NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
						PinBusStr2 = GetObjectText(NetLabelObject->Text1);

// ********************************************************************************
// Search for indidual net labels pin bus
// ********************************************************************************
						for (cnt6 = 0; cnt6 < (*NetInfos)[NetNr].NrPinsPinBus; cnt6++)
						{
							Presence = (*NetInfos)[NetNr].Presence;

//              Presence=(__int64)0;
							if ((GetLabelBusNameByIndex(PinBusStr2, str2, cnt6) != -1)
							        && ((Presence & INT64SHIFT(cnt6)) == 0))
							{
								if (CheckSpecialPowerNetNames(str2) == -1)
								{
									NetLabelName = str2;
									NetLabelNr = ObjectTextToBuf(NetLabelName);
#ifdef _DEBUG
									AddNetNr(fp, NetNr, NetLabelName, cnt6, 3);
#endif
									(*NetInfos)[NetNr].Presence |= INT64SHIFT(cnt6);

									if (AddObjectsPosition1((NetNr << 16) + cnt6) == -1)
										return -1;

									result = FindAndInsertNets(NetNr, NetLabelName, cnt2, fp);

									if (AddObjectsInfo(NetLabelNr, NrInt32Objects1, cnt6) == -1)
										return -1;

#ifdef _DEBUG
									WriteLn(fp, "END");
#endif

									if ((result & 1) == 1)
									{
										(*NetInfos)[NetNr].PinBusCount++;

										if ((*NetInfos)[NetNr].NrPinsPinBus == (*NetInfos)[NetNr].PinBusCount)
										{
											(*NetInfos)[NetNr].Progress |= 2;
// All signal names inside a pinbus are evaluated, so pinbus is evaluated
										}
									}
								}
								else
									ok = 1;
							}
						}
					}
				}
			}
		}
		else
		{

// ********************************************************************************
// ********************************************************************************
// Search for Global nets without busses
// ********************************************************************************
// ********************************************************************************

			for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
			{
				Info = (*NetInfos)[NetNr].Info;
#ifdef _DEBUG
				res = (*NetInfos)[NetNr].Progress & 3;

				if (NetNr == 32)
					ok = 1;

#endif

				if ((((*NetInfos)[NetNr].Progress & 3) == 0) && (((Info & 0x68000) == 0x48000)	// NrSheetPins>0 , NrNetLabels>0  , NrBusses=0
				        || ((Info & 0x29000) == 0x9000)))
				{	// NrNetLabels>0 , NrGlobalConnections>0 , NrBusses=0
// ********************************************************************************
// ********************************************************************************
// Netlabel
// Pin
					if ((NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr) != -1)
					{
#ifdef _DEBUG

						if (NetNr == 3)
							ok = 1;

#endif
						NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
						NetLabelNr = NetLabelObject->Text1;
						NetLabelName = GetObjectText(NetLabelNr);

						if ((Info & 0x100000) == 0x000000)
						{	// NrPowerPins = 0
#ifdef _DEBUG
							AddNetNr(fp, NetNr, NetLabelName, 0, 2);
#endif

							if (AddObjectsPosition1(NetNr << 16) == -1)
								return -1;

							InsertOtherLabels(NetNr, NetLabelObjectNr, NetLabelName, fp, 0);
						}
						else
							InsertOtherLabels(NetNr, NetLabelObjectNr, NetLabelName, fp, 1);

						FoundNr = -1;

						for (cnt6 = 0; cnt6 < NrGlobalNets; cnt6++)
						{
							for (cnt7 = GlobalNetsPos[cnt6]; cnt7 < GlobalNetsPos[cnt6 + 1]; cnt7++)
							{
								if ((GlobalNetNr2 = GlobalNets[cnt7]) == NetNr)
									FoundNr = cnt6;
							}
						}

						if (FoundNr != -1)
						{
							for (cnt6 = GlobalNetsPos[FoundNr]; cnt6 < GlobalNetsPos[FoundNr + 1]; cnt6++)
							{
								if ((GlobalNetNr2 = GlobalNets[cnt6]) != NetNr)
								{
									if (((*NetInfos)[GlobalNetNr2].Info & 0x2000) == 0)
									{	// NrBusConnections1 = 0
										if ((Info & 0x100000) == 0x000000)
										{	// NrPowerPins = 0
											if (AddObjectsPosition1(GlobalNetNr2 << 16) == -1)
												return -1;

											(*NetInfos)[GlobalNetNr2].Progress |= 2;
#ifdef _DEBUG
											AddNetNr(fp, GlobalNetNr2, 0, 0, 0);
#endif
											InsertOtherLabels(GlobalNetNr2, NetLabelObjectNr, NetLabelName, fp, 0);
										}
										else
											InsertOtherLabels(GlobalNetNr2, NetLabelObjectNr, NetLabelName, fp, 1);
									}
									else
									{
										sprintf(str, SC(186, "Warning: Can not connect net %s into two busses\r\n"),
										        NetLabelName);
										AddMessage(str);
//                    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
									}
								}
							}
						}

						if ((Info & 0x100000) == 0x000000)
						{	// NrPowerPins = 0
							(*NetInfos)[NetNr].Progress |= 2;

							if (AddObjectsInfo(NetLabelNr, NrInt32Objects1, -1) == -1)
								return -1;

#ifdef _DEBUG
							WriteLn(fp, "END");
#endif
						}
					}
				}
			}
		}
	}

// ********************************************************************************
// ********************************************************************************
// Search for unconnected pin busses with out a busconnection
// ********************************************************************************
// ********************************************************************************

	for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
	{
		Info = (*NetInfos)[NetNr].Info;
#ifdef _DEBUG

		if (NetNr == 93)
			ok = 1;

#endif

		if ((((*NetInfos)[NetNr].Progress & 3) == 0) && ((Info & 0x22400) == 0x20400))
		{	// NrPinBusses>0 , NrBusses>0 , NrBusConnections1=0
			NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr;
			GlobalConnObjectNr = (*NetInfos)[NetNr].GlobalConnObjectNr;

			if (NetLabelObjectNr != -1)
			{
				Object2a = &((*Objects2)[NetLabelObjectNr]);
				PinBusStr2 = GetObjectText(Object2a->Text1);
				Changed = 0;

// ********************************************************************************
// Search for indidual net labels pin bus
// ********************************************************************************
				for (cnt6 = 0; cnt6 < (*NetInfos)[NetNr].NrPinsPinBus; cnt6++)
				{
					Changed = 0;

					if ((GetLabelBusNameByIndex(PinBusStr2, str2, cnt6) != -1)
					        && (((*NetInfos)[NetNr].Presence & INT64SHIFT(cnt6)) == 0))
					{
						if (CheckSpecialPowerNetNames(str2) == -1)
						{
							OldNrObjectsInfo = NrObjectsInfo;
							NetLabelName = str2;
							NetLabelNr = ObjectTextToBuf(NetLabelName);
							//            PinBusStr2=GetObjectText(Object2a->Text1);
#ifdef _DEBUG
							AddNetNr(fp, NetNr, NetLabelName, cnt6, 3);
							//            sprintf(str,"NET %s",NetLabelName);
							//            WriteLn(fp,str);
							//            sprintf(str,SC(188,"nr %i %i"),NetNr,cnt6);
							//            WriteLn(fp,str);
#endif
							(*NetInfos)[NetNr].Presence |= INT64SHIFT(cnt6);
							(*ObjectsInfo)[NrObjectsInfo].Name = ObjectTextToBuf(NetLabelName);
							PinBusStr2 = GetObjectText(Object2a->Text1);

							if (AddObjectsPosition1((NetNr << 16) + cnt6) == -1)
								return -1;

							SearchNetNr = NetNr;
							FoundNr = -1;

							for (cnt2 = 0; cnt2 < NrGlobalNets; cnt2++)
							{
// search NrGlobalNets for BusNetNr
								for (cnt3 = GlobalNetsPos[cnt2]; cnt3 < GlobalNetsPos[cnt2 + 1]; cnt3++)
								{
									if (GlobalNets[cnt3] == SearchNetNr)
										FoundNr = cnt2;
								}
							}

							NrGlobalNetNumbers = 0;

							if (FoundNr != -1)
							{
								for (cnt3 = GlobalNetsPos[FoundNr]; cnt3 < GlobalNetsPos[FoundNr + 1]; cnt3++)
								{
									if (NrGlobalNetNumbers < 100)
										GlobalNetNumbers[NrGlobalNetNumbers++] = GlobalNets[cnt3];
								}
							}
							else
								GlobalNetNumbers[NrGlobalNetNumbers++] = SearchNetNr;

							for (cnt3 = 0; cnt3 < NrGlobalNetNumbers; cnt3++)
							{
								cnt4 = GlobalNetNumbers[cnt3];

// ********************************************************************************
// pin bus label name is in other pin bus connected to a external connection
								if (((*NetInfos)[cnt4].Info & 0x21400) == 0x21400)
								{	// NrPinBusses>0 , NrBusses>0 , NrWires>0
									NetLabelObjectNr2 = (*NetInfos)[cnt4].NetLabelObjectNr;
									Object2b = &((*Objects2)[NetLabelObjectNr2]);

									if (NetLabelObjectNr2 != NetLabelObjectNr)
									{
										PinBusStr = GetObjectText(Object2b->Text1);
										ok = 1;

										if (strnicmpUTF8(PinBusStr, NetLabelName, 1) == 0)
										{
											Presence = (*NetInfos)[cnt4].Presence;

											//                    Presence=(__int64)0;
											for (cnt5 = 0; cnt5 < (*NetInfos)[cnt4].NrPinsPinBus; cnt5++)
											{
												if ((GetLabelBusNameByIndex(PinBusStr, str, cnt5) != -1)
												        && ((Presence & INT64SHIFT(cnt5)) == 0))
												{
													ok = 1;

													if (stricmpUTF8(NetLabelName, str) == 0)
													{
#ifdef _DEBUG
														AddNetNr(fp, cnt4, 0, cnt5, 1);
														//                          sprintf(str,SC(188,"nr %i %i"),cnt4,cnt5);
														//                          WriteLn(fp,str);
#endif

														if (AddObjectsPosition1((cnt4 << 16) + cnt5) == -1)
															return -1;

														(*NetInfos)[cnt4].PinBusCount++;

														if ((*NetInfos)[cnt4].NrPinsPinBus ==
														        (*NetInfos)[cnt4].PinBusCount)
														{
															(*NetInfos)[cnt4].Progress |= 2;
// All signal names inside a pinbus are evaluated, so pinbus is evaluated
														}

														(*NetInfos)[cnt4].Presence |= INT64SHIFT(cnt5);
														Changed = 1;
														ok = 1;
													}
												}

												ok = 1;
											}
										}
									}
								}

// ********************************************************************************
// pin bus label name is in other pin bus connected to the same sheet
								if (((*NetInfos)[cnt4].Info & 0x21400) == 0x20400)
								{	// NrPinBusses>0 , NrBusses>0 , NrWires=0
									Changed = 1;
								}
							}

#ifdef _DEBUG
							WriteLn(fp, "END");
#endif

							if (AddObjectsInfo(NetLabelNr, NrInt32Objects1, cnt6) == -1)
								return -1;

							if (Changed)
							{
								(*NetInfos)[NetNr].PinBusCount++;

								if ((*NetInfos)[NetNr].NrPinsPinBus == (*NetInfos)[NetNr].PinBusCount)
									(*NetInfos)[NetNr].Progress |= 2;
							}
							else
							{
								//            NrInt32Objects1--;
								//            NrObjectsInfo=OldNrObjectsInfo;
							}
						}
						else
							ok = 1;
					}
				}
			}

			ok = 1;
		}
	}

// ********************************************************************************
// ********************************************************************************
// Check for power nets
// ********************************************************************************
// ********************************************************************************

	NrGlobalPowerNetNrs = 0;

	for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
	{
		Info = (*NetInfos)[NetNr].Info;

		if ((((*NetInfos)[NetNr].Progress & 3) == 0) && ((Info & 0x300000) == 0x300000))
		{	// NrPowerPins>1
#ifdef _DEBUG
			if (NetNr == 372)
				ok = 1;

#endif
			NrTempPowerNetNames = 0;
			start = (*NetInfos)[NetNr].Pos;
			stop = (*NetInfos)[NetNr + 1].Pos;

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				Object2 = &((*Objects2)[cnt2]);

				if ((Object2->ObjectType == SYMBOL_PIN) && (Object2->Info2 == POWER_CONNECTION))
				{
					PowerNetName = Object2->Text2;
					Text2 = GetObjectText(Object2->RefNum);
					NetLabelName = GetObjectText(PowerNetName);

					if ((Text2[0] == 0) && (NrTempPowerNetNames < 8))
					{
						TempPowerNetName[NrTempPowerNetNames] = PowerNetName;
						NrTempPowerNetNames++;
					}
				}
			}

// ********************************************************************************
			if (NrTempPowerNetNames > 0)
			{
				NetLabelName = GetObjectText(TempPowerNetName[0]);

				FoundNr = -1;

				for (cnt2 = 0; cnt2 < NrGlobalPowerNetNrs; cnt2++)
				{
					for (cnt3 = 0; cnt3 < PowerNetNrs[cnt2].NrPowerNames; cnt3++)
					{
						for (cnt4 = 0; cnt4 < NrTempPowerNetNames; cnt4++)
						{
							T1 = GetObjectText(PowerNetNrs[cnt2].PowerNetNrs[cnt3]);
							T2 = GetObjectText(TempPowerNetName[cnt4]);

							if (stricmpUTF8(T1, T2) == 0)
								FoundNr = cnt2;
						}
					}
				}

				if (FoundNr == -1)
				{
					if (NrGlobalPowerNetNrs < 128)
					{
						PowerNetNrs[NrGlobalPowerNetNrs].GlobalPowerNetNr = TempPowerNetName[0];
						PowerNetNrs[NrGlobalPowerNetNrs].NrPowerNames = NrTempPowerNetNames;
						memmove(&PowerNetNrs[NrGlobalPowerNetNrs].PowerNetNrs, &TempPowerNetName,
						        NrTempPowerNetNames * sizeof(int32));
						FoundNr = NrGlobalPowerNetNrs;
						/*
						            for (cnt4=1;cnt4<NrTempPowerNetNames;cnt4++) {
						              if (PowerNetNrs[FoundNr].NrPowerNames<8) {
						                PowerNetNrs[FoundNr].PowerNetNrs[PowerNetNrs[FoundNr].NrPowerNames]=TempPowerNetName[cnt4];
						                PowerNetNrs[FoundNr].NrPowerNames++;
						              }
						            }
						*/
						NrGlobalPowerNetNrs++;
					}
				}
				else
				{
					for (cnt4 = 0; cnt4 < NrTempPowerNetNames; cnt4++)
					{
						FoundNr2 = -1;

						for (cnt3 = 0; cnt3 < PowerNetNrs[FoundNr].NrPowerNames; cnt3++)
						{
							T1 = GetObjectText(PowerNetNrs[FoundNr].PowerNetNrs[cnt3]);
							T2 = GetObjectText(TempPowerNetName[cnt4]);

							if (stricmpUTF8(T1, T2) == 0)
								FoundNr2 = 0;
						}

						if (FoundNr2 == -1)
						{
							if (PowerNetNrs[FoundNr].NrPowerNames < 8)
							{
								PowerNetNrs[FoundNr].PowerNetNrs[PowerNetNrs[FoundNr].NrPowerNames] =
								    TempPowerNetName[cnt4];
								PowerNetNrs[FoundNr].NrPowerNames++;
							}
						}
					}
				}
			}

// ********************************************************************************
			if (FoundNr != -1)
			{
				if (Info == 0x100000)
				{	// NrPowerPins=1
					start = (*NetInfos)[NetNr].Pos;
					Object2 = &((*Objects2)[start]);
					Object2->Info5 = (int16) FoundNr;
				}
				else
				{
					start = (*NetInfos)[NetNr].Pos;
					stop = (*NetInfos)[NetNr + 1].Pos;

					for (cnt2 = start; cnt2 < stop; cnt2++)
					{
						Object2 = &((*Objects2)[cnt2]);

						if ((Object2->ObjectType == SYMBOL_PIN) && (Object2->Info2 == POWER_CONNECTION))
							Object2->Info5 = (int16) FoundNr;
					}
				}
			}

// ********************************************************************************
		}
	}

// ********************************************************************************
// ********************************************************************************
	for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
	{
		Info = (*NetInfos)[NetNr].Info;

		if ((((*NetInfos)[NetNr].Progress & 3) == 0) && ((Info & 0x300000) == 0x100000))
		{	// NrPowerPins=1
#ifdef _DEBUG
			if (NetNr == 528)
				ok = 1;

#endif
			NrTempPowerNetNames = 0;

			if (Info == 0x100000)
			{	// NrPowerPins=1
				start = (*NetInfos)[NetNr].Pos;
				Object2 = &((*Objects2)[start]);
				PowerNetName = Object2->Text1;
				NetLabelName = GetObjectText(PowerNetName);
				TempPowerNetName[NrTempPowerNetNames] = PowerNetName;
				NrTempPowerNetNames++;
			}
			else
			{
				start = (*NetInfos)[NetNr].Pos;
				stop = (*NetInfos)[NetNr + 1].Pos;

				for (cnt2 = start; cnt2 < stop; cnt2++)
				{
					Object2 = &((*Objects2)[cnt2]);

					if ((Object2->ObjectType == SYMBOL_PIN) && (Object2->Info2 == POWER_CONNECTION))
					{
						PowerNetName = Object2->Text2;
						NetLabelName = GetObjectText(PowerNetName);
						TempPowerNetName[0] = PowerNetName;
						NrTempPowerNetNames++;
					}
				}
			}

// ********************************************************************************
			if (NrTempPowerNetNames > 0)
			{
				NetLabelName = GetObjectText(TempPowerNetName[0]);

				FoundNr = -1;

				for (cnt2 = 0; cnt2 < NrGlobalPowerNetNrs; cnt2++)
				{
					for (cnt3 = 0; cnt3 < PowerNetNrs[cnt2].NrPowerNames; cnt3++)
					{
						T1 = GetObjectText(PowerNetNrs[cnt2].PowerNetNrs[cnt3]);
						T2 = GetObjectText(TempPowerNetName[0]);

						if (stricmpUTF8(T1, T2) == 0)
							FoundNr = cnt2;
					}
				}

				if (FoundNr == -1)
				{
					if (NrGlobalPowerNetNrs < 128)
					{
						PowerNetNrs[NrGlobalPowerNetNrs].GlobalPowerNetNr = TempPowerNetName[0];
						PowerNetNrs[NrGlobalPowerNetNrs].NrPowerNames = 1;
						PowerNetNrs[NrGlobalPowerNetNrs].PowerNetNrs[0] = TempPowerNetName[0];
						FoundNr = NrGlobalPowerNetNrs;
						NrGlobalPowerNetNrs++;
					}
				}
			}

// ********************************************************************************
			if (FoundNr != -1)
			{
				if (Info == 0x100000)
				{	// NrPowerPins=1
					start = (*NetInfos)[NetNr].Pos;
					Object2 = &((*Objects2)[start]);
					Object2->Info5 = (int16) FoundNr;
				}
				else
				{
					start = (*NetInfos)[NetNr].Pos;
					stop = (*NetInfos)[NetNr + 1].Pos;

					for (cnt2 = start; cnt2 < stop; cnt2++)
					{
						Object2 = &((*Objects2)[cnt2]);

						if ((Object2->ObjectType == SYMBOL_PIN) && (Object2->Info2 == POWER_CONNECTION))
							Object2->Info5 = (int16) FoundNr;
					}
				}
			}

// ********************************************************************************
		}
	}

// ********************************************************************************
// ********************************************************************************

	for (cnt2 = 0; cnt2 < NrGlobalPowerNetNrs; cnt2++)
	{
		NetLabelName = GetObjectText(PowerNetNrs[cnt2].GlobalPowerNetNr);
#ifdef _DEBUG
		sprintf(str, "NET %s [POWER]", NetLabelName);
		WriteLn(fp, str);
//    AddNetNr(fp,NetNr,NetLabelName,0,2);
#endif

		for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
		{
			Info = (*NetInfos)[NetNr].Info;
#ifdef _DEBUG

			if (NetNr == 373)
				ok = 1;

#endif
			Evaluate = 0;
			cnt3a = -1;

			if (((*NetInfos)[NetNr].Progress & 3) == 0)
			{
				if ((Info & 0x100000) == 0x100000)
				{	// NrPowerPins>0
					Evaluate = 1;
				}

				if (((*NetInfos)[NetNr].Info & 0x20400) == 0x20400)
				{	// NrPinBusses>0 , NrBusses>0
					if ((NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr) != -1)
					{
// Pinbus
#ifdef _DEBUG
						if (NetNr == 223)
							ok = 1;

#endif
						NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
						PinBusStr2 = GetObjectText(NetLabelObject->Text1);
// ********************************************************************************
// Search for indidual net labels pin bus
// ********************************************************************************
						Presence = (*NetInfos)[NetNr].Presence;

						for (cnt6 = 0; cnt6 < (*NetInfos)[NetNr].NrPinsPinBus; cnt6++)
						{
							//              Presence=(__int64)0;
							if ((GetLabelBusNameByIndex(PinBusStr2, str2, cnt6) != -1)
							        && ((Presence & INT64SHIFT(cnt6)) == 0))
							{
								if ((cnt3 = CheckSpecialPowerNetNames(str2)) >= 0)
								{
									if (stricmpUTF8(SpecialPowerNetNames[cnt3].PowerNetName, NetLabelName) == 0)
									{
										Evaluate = 2;
										NetLabelName2 = str2;
									}
								}
							}
						}
					}
				}

				for (cnt3 = 0; cnt3 < NrGlobalNets; cnt3++)
				{
					if (((GlobalNetsInfo[cnt3] & 1) == 1) && ((Info & 0xA000) == 0xA000)	// NrBusConnections1>0 , NrNetLabels>0
					        && (BusConnectionConnectedToGlobalBus(NetNr, cnt3)))
					{
// ********************************************************************************
// ********************************************************************************
// Netlabel
// Busconnection1
						if (((Info & 0x0400) == 0x0000)	// NrPinBusses=0
						        && ((NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr) != -1))
						{
#ifdef _DEBUG

							if (NetNr == 594)
								ok = 1;

#endif
							NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
							NetLabelNr = NetLabelObject->Text1;
							NetLabelName2 = GetObjectText(NetLabelNr);

							if (CheckSpecialPowerNetNames(NetLabelName2) >= 0)
							{
								if (cnt3a == -1)
									cnt3a = cnt3;

								Evaluate = 3;
							}
						}
						else
							ok = 1;
					}
				}

// ********************************************************************************
// ********************************************************************************
				if (Evaluate == 1)
				{
					PowerObjectNr = -1;

					if (Info == 0x100000)
					{	// NrPowerPins=1
						start = (*NetInfos)[NetNr].Pos;
						Object2 = &((*Objects2)[start]);
						PowerNetName = Object2->Text1;
						PowerNetStr = GetObjectText(PowerNetName);
						PowerObjectNr = start;
					}
					else
					{
						start = (*NetInfos)[NetNr].Pos;
						stop = (*NetInfos)[NetNr + 1].Pos;

						for (cnt3 = start; cnt3 < stop; cnt3++)
						{
							Object2 = &((*Objects2)[cnt3]);

							if ((Object2->ObjectType == SYMBOL_PIN) && (Object2->Info2 == POWER_CONNECTION))
							{
								PowerNetName = Object2->Text2;
								PowerNetStr = GetObjectText(PowerNetName);
								PowerObjectNr = cnt3;
							}
						}
					}

					if (PowerObjectNr != -1)
					{
						Object2 = &((*Objects2)[PowerObjectNr]);

						if (Object2->Info5 == cnt2)
						{
// ********************************************************************************
							if ((((Info & 0x68000) == 0x48000)	// NrSheetPins>0 , NrNetLabels>0  , NrBusses=0
							        || ((Info & 0x29000) == 0x9000)))
							{	// NrNetLabels>0 , NrGlobalConnections>0 , NrBusses=0
								if ((NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr) != -1)
								{
#ifdef _DEBUG

									if (NetNr == 3)
										ok = 1;

#endif
									NetLabelObject = &((*Objects2)[NetLabelObjectNr]);
									NetLabelNr = NetLabelObject->Text1;
									NetLabelName = GetObjectText(NetLabelNr);

									if (AddObjectsPosition1(NetNr << 16) == -1)
										return -1;

									InsertOtherLabels(NetNr, NetLabelObjectNr, NetLabelName, fp, 0);

									FoundNr = -1;

									for (cnt6 = 0; cnt6 < NrGlobalNets; cnt6++)
									{
										for (cnt7 = GlobalNetsPos[cnt6]; cnt7 < GlobalNetsPos[cnt6 + 1]; cnt7++)
										{
											if ((GlobalNetNr2 = GlobalNets[cnt7]) == NetNr)
												FoundNr = cnt6;
										}
									}

									if (FoundNr != -1)
									{
										for (cnt6 = GlobalNetsPos[FoundNr]; cnt6 < GlobalNetsPos[FoundNr + 1]; cnt6++)
										{
											if ((GlobalNetNr2 = GlobalNets[cnt6]) != NetNr)
											{
												if (((*NetInfos)[GlobalNetNr2].Info & 0x2000) == 0)
												{	// NrBusConnections1 = 0
													if (AddObjectsPosition1(GlobalNetNr2 << 16) == -1)
														return -1;

													(*NetInfos)[GlobalNetNr2].Progress |= 2;
													InsertOtherLabels(GlobalNetNr2, NetLabelObjectNr, NetLabelName, fp,
													                  0);
												}
												else
												{
													sprintf(str,
													        SC(186,
													           "Warning: Can not connect net %s into two busses\r\n"),
													        NetLabelName);
													AddMessage(str);
//                          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
												}
											}
										}
									}

									(*NetInfos)[NetNr].Progress |= 2;
								}
							}
							else
							{	// Normal power pins
// ********************************************************************************
#ifdef _DEBUG
								AddNetNr(fp, Object2->NetNr, 0, 0, 0);
#endif

								if (AddObjectsPosition1(Object2->NetNr << 16) == -1)
									return -1;

								(*NetInfos)[Object2->NetNr].Progress |= 2;
							}
						}
					}
				}

				if (Evaluate == 2)
				{
// ********************************************************************************
// Search for indidual net labels pin bus
// ********************************************************************************
					Presence = (*NetInfos)[NetNr].Presence;

					for (cnt6 = 0; cnt6 < (*NetInfos)[NetNr].NrPinsPinBus; cnt6++)
					{
//              Presence=(__int64)0;
						if ((GetLabelBusNameByIndex(PinBusStr2, str2, cnt6) != -1)
						        && ((Presence & INT64SHIFT(cnt6)) == 0))
						{
							if ((cnt3 = CheckSpecialPowerNetNames(str2)) >= 0)
							{
								if (stricmpUTF8(SpecialPowerNetNames[cnt3].PowerNetName, NetLabelName) == 0)
								{
									(*NetInfos)[NetNr].Presence |= INT64SHIFT(cnt6);

									if (AddObjectsPosition1((NetNr << 16) + cnt6) == -1)
										return -1;

									result = FindAndInsertNets(NetNr, NetLabelName2, cnt2, fp);

									if ((result & 1) == 1)
									{
										(*NetInfos)[NetNr].PinBusCount++;

										if ((*NetInfos)[NetNr].NrPinsPinBus == (*NetInfos)[NetNr].PinBusCount)
										{
											(*NetInfos)[NetNr].Progress |= 2;
// All signal names inside a pinbus are evaluated, so pinbus is evaluated
										}
									}
								}
								else
									ok = 1;
							}
						}
					}
				}

				if (Evaluate == 3)
				{
					if (AddObjectsPosition1(NetNr << 16) == -1)
						return -1;

					result = FindAndInsertNets(NetNr, NetLabelName2, cnt3a, fp);

					if (((Info & 0x40000) == 0x40000)	// SheetPin
					        || ((Info & 0x1000) == 0x1000))
					{	// External connection
						CheckCascadedNetObjects(NetNr, NetLabelName2, fp);
					}

					(*NetInfos)[NetNr].Progress |= 2;
				}
			}
		}

		if (AddObjectsInfo(PowerNetNrs[cnt2].GlobalPowerNetNr, NrInt32Objects1, -1) == -1)
			return -1;

#ifdef _DEBUG
		WriteLn(fp, "END");
#endif
	}

	ok = 1;

// ********************************************************************************
// ********************************************************************************
// Print the nets not connected to other objects and is not a bus
// ********************************************************************************
// ********************************************************************************

	for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
	{
		Info = (*NetInfos)[NetNr].Info;
#ifdef _DEBUG

		if (NetNr == 40)
			ok = 1;

#endif

		if ((((*NetInfos)[NetNr].Progress & 3) == 0) && ((Info & 0x20000) == 0))
		{	// NrBusses = 0
			NetLabelObjectNr = (*NetInfos)[NetNr].NetLabelObjectNr;

			if (NetLabelObjectNr != -1)
			{
				Object2a = &((*Objects2)[NetLabelObjectNr]);
				NetLabelName = GetObjectText(Object2a->Text1);
#ifdef _DEBUG
				AddNetNr(fp, NetNr, NetLabelName, 0, 4);
//        sprintf(str,"NET %i %s",NetNr,NetLabelName);
//        WriteLn(fp,str);
#endif
				AddObjectsPosition1(NetNr << 16);

				SheetNr2 = (*NetInfos)[NetNr].SheetNr;
				StartNet = Sheets[SheetNr2].NetPos;
				EndNet = StartNet + Sheets[SheetNr2].NrNets;

// ********************************************************************************
// Start search for objects in sheet SheetNr2 with the same netname
// ********************************************************************************
				for (cnt4 = StartNet; cnt4 < EndNet; cnt4++)
				{
					Info = (*NetInfos)[cnt4].Info;
#ifdef _DEBUG

					if (cnt4 == 223)
						ok = 1;

#endif

					if (((Info & 0xa000) == 0x8000)	// NrBusConnections1>0 , NrNetLabels>0
					        && (((*NetInfos)[cnt4].Progress & 3) == 0))
					{
// ********************************************************************************
// Label and no busconnection
// ********************************************************************************
						NetLabelObjectNr2 = (*NetInfos)[cnt4].NetLabelObjectNr;
						NetLabelObject2 = &((*Objects2)[NetLabelObjectNr2]);

						if (NetLabelObjectNr2 != NetLabelObjectNr)
						{
							if (((*NetInfos)[cnt4].Info & 0x400) == 0)
							{
								NetLabelStr = GetObjectText(NetLabelObject2->Text1);

								if (stricmpUTF8(NetLabelName, NetLabelStr) == 0)
								{
#ifdef _DEBUG
									AddNetNr(fp, cnt4, 0, 0, 0);
									sprintf(str, SC(187, "nr %i"), cnt4);
									WriteLn(fp, str);
#endif
									AddObjectsPosition1(cnt4 << 16);
									(*NetInfos)[cnt4].Progress |= 2;
								}
							}
						}
					}
				}

				if (AddObjectsInfo(Object2a->Text1, NrInt32Objects1, -1) == -1)
					return -1;

			}
			else
			{
				sprintf(str2, "N$%i", UnNamedNetNr);
#ifdef _DEBUG
				AddNetNr(fp, NetNr, str2, 0, 4);
//        sprintf(str,"NET %i %s",NetNr,str2);
//        WriteLn(fp,str);
#endif

				if (AddObjectsPosition1(NetNr << 16) == -1)
					return -1;

				NetLabelNr = ObjectTextToBuf(str2);

				if (AddObjectsInfo(NetLabelNr, NrInt32Objects1, -1) == -1)
					return -1;

				UnNamedNetNr++;
			}
		}
	}

#ifdef _DEBUG
	FileClose(fp);
#endif
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddPowerPinNets()
{
	int32 cnt, cnt2, FoundNr, NrPowerNetNames, PowerNetNames[128];
	char *PowerNetName;

	LPSTR StrPos, RefName;
	Object2Record *Object2, *Object3;

	NrPowerNetNames = 0;
	PowerPinNetPos = TotalNrNets;

	for (cnt = 0; cnt < NrObjects3; cnt++)
	{
		Object3 = &((*Objects3)[cnt]);
		Object3->Info4 = -1;
		PowerNetName = GetObjectText(Object3->Text1);
		FoundNr = -1;

		for (cnt2 = 0; cnt2 < NrPowerNetNames; cnt2++)
		{
			if (stricmpUTF8(GetObjectText(Object3->Text1), GetObjectText(PowerNetNames[cnt2])) == 0)
				FoundNr = cnt2;
		}

		if (FoundNr == -1)
		{
			if (NrPowerNetNames < 128)
			{
				Object3->Info4 = NrPowerNetNames;
				PowerNetNames[NrPowerNetNames] = Object3->Text1;
				StrPos = GetObjectText(Object3->Text1);
				RefName = GetObjectText(Object3->RefNum);
				NrPowerNetNames++;
			}
		}
		else
			Object3->Info4 = FoundNr;

		ok = 1;
	}

	ok = 1;

	for (cnt2 = 0; cnt2 < NrPowerNetNames; cnt2++)
	{
		for (cnt = 0; cnt < NrObjects3; cnt++)
		{
			Object3 = &((*Objects3)[cnt]);

			if (Object3->Info4 == cnt2)
			{
				if ((NrObjects2 + 1 >= MaxNrObjects2) && (AllocateMemObjects2(MaxNrObjects2 + 128) != 0))
					return 0;

				Object2 = &((*Objects2)[NrObjects2]);
				memset(Object2, 0, sizeof(Object2Record));
				Object2->ObjectType = SYMBOL_POWERPIN;
				Object2->Text1 = Object3->Text1;
				Object2->Text2 = Object3->Text2;
				Object2->RefNum = Object3->RefNum;
				Object2->SheetNr = -1;
				Object2->NetNr = (int16) TotalNrNets;
				NrObjects2++;
			}
		}

		if (TotalNrNets + 1 >= NetInfoSize)
		{
			if (AllocateMemNetInfo(TotalNrNets + 128) != 0)
				return -1;
		}

		(*NetInfos)[TotalNrNets].Info = 0x00100000;
		(*NetInfos)[TotalNrNets].Progress = 0;
		(*NetInfos)[TotalNrNets].NetLabelObjectNr = -1;
		(*NetInfos)[TotalNrNets + 1].Pos = NrObjects2;
		TotalNrNets++;
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 PrepareNetOutput()
{
	int32 cnt, cnt2, NetCount, start, stop, Info, NetNr, NetNr1, PinBusNr, SheetNr, SheetNr1, NrPins, NrNetLabels;
	LPSTR NetLabelName;
	int32 NetWithNetLabel, TotalError;
	char Line[MAX_LENGTH_STRING];
	Object2Record *Object2;

	SheetNr1 = -1;
	TotalError = 0;

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);
		start = (*ObjectsInfo)[cnt].Pos;
		NetCount = (*ObjectsInfo)[cnt + 1].Pos - start;
		stop = start + NetCount;
		NrPins = 0;
		NrNetLabels = 0;

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			NetNr = ((*ObjectsPosition1)[cnt2]) >> 16;
			Info = (*NetInfos)[NetNr].Info;

			if ((Info & 0x8100) == 0x8100)
			{
				NrNetLabels++;

				if (NrNetLabels == 1)
				{
					Object2 = &((*Objects2)[(*NetInfos)[NetNr].NetLabelObjectNr]);
					NetLabelName = GetObjectText(Object2->Text1);
					SheetNr1 = (*NetInfos)[NetNr].SheetNr;
				}
				else
				{
					Object2 = &((*Objects2)[(*NetInfos)[NetNr].NetLabelObjectNr]);

					if (stricmpUTF8(GetObjectText(Object2->Text1), NetLabelName) != 0)
					{
						if (NrNetLabels == 2)
						{
							sprintf(Line, SC(189, "Warning: Different labels for the same net on sheets\r\n"));
							AddMessage(Line);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
							sprintf(Line, "   %s : %s%s.sch\r\n", NetLabelName, SheetDir, Sheets[SheetNr1].SheetName);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
							AddMessage(Line);
						}

//            TotalError=1;
						SheetNr = (*NetInfos)[NetNr].SheetNr;
						sprintf(Line, "   %s : %s%s.sch\r\n", NetLabelName, SheetDir, Sheets[SheetNr1].SheetName);
						AddMessage(Line);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
					}
				}
			}
		}

		ok = 1;
	}

	if (TotalError)
		return -1;

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		if (cnt == 315)
			ok = 1;

		NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);
		PinBusNr = ((*ObjectsInfo)[cnt]).Info;
		start = (*ObjectsInfo)[cnt].Pos;
		NetCount = (*ObjectsInfo)[cnt + 1].Pos - start;
		stop = start + NetCount;
		NrPins = 0;
		NetWithNetLabel = 0;

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			NetNr = ((*ObjectsPosition1)[cnt2]) >> 16;
			Info = (*NetInfos)[NetNr].Info;

			if ((Info & 0x8000) == 0x8000)
			{
				NetWithNetLabel = 1;
				NetNr1 = NetNr;
			}

			if ((Info & 0x100) == 0x100)
				NrPins++;

			if ((Info & 0x200) == 0x200)
				NrPins++;

			if ((Info & 0x400) == 0x400)
				NrPins++;

			if ((Info & 0x800) == 0x800)
				NrPins++;

			ok = 1;
		}

		/*
		    if ((NetWithNetLabel)
		       &&
		       (NrPins<2)) {
		      Object2=&((*Objects2)[(*NetInfos)[NetNr1].NetLabelObjectNr]);
		      NetLabelName=GetObjectText(Object2->Text1);
		      NetLabelName2=NetLabelName;
		      if (PinBusNr!=-1) {
		        if (GetLabelBusNameByIndex(NetLabelName,str2,PinBusNr)!=-1) {
		          NetLabelName=str2;
		        }
		      }

		      SheetNr=(*NetInfos)[NetNr1].SheetNr;
		      strcpy(FileName,Sheets[SheetNr].SheetName);
		      sprintf(Line,"One pin net %s [sheet %s]\r\n",NetLabelName,FileName);
		      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
		      TotalError=1;

		    }
		*/
		ok = 1;
	}

//  if (TotalError) return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckGeometries()
{
	int32 cnt, cnt2, FoundNr, NrLibEntries, NrGeometries, result, Libfp, res, Geometries[2048];
	LPSTR RefName, GeomName;
	int32 TotalError;
	char Line[MAX_LENGTH_STRING], DirName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	Object2Record *Object4;
	LibRecord Lib;
	LibNameRecord LibName;
	HANDLE FileSearchHandle;
	WIN32_FIND_DATAW FileInfo;

	NrGeometries = 0;
	sprintf(DirName, "%s\\pcb\\shapes\\*.shp", DesignPath);

	FileSearchHandle = FindFirstFileUTF8(DirName, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		memset(&str, 0, sizeof(str));
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		strncpy(str, str2, strlen(str2) - 4);

		if (NrGeometries < 2048)
			Geometries[NrGeometries++] = ObjectTextToBuf(str);

		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);


	/*
	  first=res=_findfirst(DirName,&fileinfo);
	  while (res!=-1) {
	    memset(&str,0,sizeof(str));
	    strncpy(str,fileinfo.name,strlen(fileinfo.name)-4);
	    if (NrGeometries<2048) {
	      Geometries[NrGeometries++]=ObjectTextToBuf(str);
	    }
	    res=_findnext(first,&fileinfo);
	  }
	*/
	sprintf(DirName, "%s\\shapes\\*.shp", ProjectPath);

	FileSearchHandle = FindFirstFileUTF8(DirName, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		memset(&str, 0, sizeof(str));
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		strncpy(str, str2, strlen(str2) - 4);

		if (NrGeometries < 2048)
			Geometries[NrGeometries++] = ObjectTextToBuf(str);

		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	/*
	  first=res=_findfirst(DirName,&fileinfo);
	  while (res!=-1) {
	    memset(&str,0,sizeof(str));
	    strncpy(str,fileinfo.name,strlen(fileinfo.name)-4);
	    if (NrGeometries<2048) {
	      Geometries[NrGeometries++]=ObjectTextToBuf(str);
	    }
	    res=_findnext(first,&fileinfo);
	  }
	*/
	sprintf(DirName, "%s\\shplib\\*.slb", ProjectPath);

	FileSearchHandle = FindFirstFileUTF8(DirName, &FileInfo);
	res = 1;

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		res = 0;

	while (res)
	{
		memset(&str, 0, sizeof(str));
		UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
		sprintf(str, "%s\\shplib\\%s", ProjectPath, str2);

		if ((Libfp = FileOpenReadOnlyUTF8(str)) != -1)
		{
			if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == 0)
			{
				if (strcmp(Lib.Identification, LibraryCode2) == 0)
				{
					NrLibEntries = Lib.NrLibEntries;

					for (cnt = 0; cnt < NrLibEntries; cnt++)
					{
						if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == 0)
						{
							if (NrGeometries < 2048)
								Geometries[NrGeometries++] = ObjectTextToBuf(LibName.Text);
						}
					}
				}
			}

			FileClose(Libfp);
		}

		res = FindNextFileW(FileSearchHandle, &FileInfo);
	}

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
		FindClose(FileSearchHandle);

	/*
	  first=res=_findfirst(DirName,&fileinfo);
	  while (res!=-1) {
	    memset(&str,0,sizeof(str));
	    sprintf(str,"%s\\shplib\\%s",ExePath,fileinfo.name);
	    if ((Libfp=FileOpenReadOnlyUTF8(str))!=-1) {
	      if (FileRead(Libfp,&Lib,sizeof(LibRecord),&result)==0) {
	        if (strcmp(Lib.Identification,LibraryCode2)==0) {
	          NrLibEntries=Lib.NrLibEntries;
	          for (cnt=0;cnt<NrLibEntries;cnt++) {
	            if (FileRead(Libfp,&LibName,sizeof(LibNameRecord),&result)==0) {
	              if (NrGeometries<2048) {
	                Geometries[NrGeometries++]=ObjectTextToBuf(LibName.Text);
	              }
	            }
	          }
	        }
	      }
	      FileClose(Libfp);
	    }
	    res=_findnext(first,&fileinfo);
	  }
	*/

	TotalError = 0;

	for (cnt = 0; cnt < NrObjects4; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);
		RefName = GetObjectText(Object4->RefNum);

		if (strlen(RefName) > 0)
		{
			if (Object4->Text1 == -1)
				continue;
			else
				GeomName = GetObjectText(Object4->Text1);

			if (GeomName[0] == 0)
			{
				sprintf(Line, SC(190, "No geometry for component %s\r\n"), RefName);
				AddMessage(Line);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
			}
			else
			{
				FoundNr = -1;

				for (cnt2 = 0; cnt2 < NrGeometries; cnt2++)
				{
					if (stricmpUTF8(GetObjectText(Geometries[cnt2]), GeomName) == 0)
						FoundNr = cnt2;
				}

				if (FoundNr == -1)
				{
					sprintf(Line, SC(191, "Geometry %s not found for component %s\r\n"), GetObjectText(Object4->Text1),
					        RefName);
					AddMessage(Line);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
				}
			}
		}
		else
		{
			sprintf(Line, SC(192, "Component without a reference name (%.0f,%.0f)\r\n"), Object4->x1, Object4->y1);
			AddMessage(Line);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
			TotalError = 1;
		}

		ok = 1;
	}

	ok = 1;

	if (TotalError)
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckEqualNetItems(int32 Start, int32 Stop)
{
	int32 cnt, cnt2, cnt3, start2, stop2, NetNr, NrNetItems, PinBusNr, PinCount;
	Object2Record *Object2;
	LPSTR RefName, PinName, PinBusName;
	char Line[MAX_LENGTH_STRING], PinName2[20], NetItemStr[MAX_LENGTH_STRING];
	NetItemsArray *NetItemsPerNet;
#ifdef _DEBUG
	int32 ok;
#endif

	NrNetItems = 0;
	NetItemsPerNet = (NetItemsArray *) & NetItemsBuf;

	for (cnt2 = Start; cnt2 < Stop; cnt2++)
	{
		NetNr = ((*ObjectsPosition1)[cnt2]) >> 16;
		PinBusNr = ((*ObjectsPosition1)[cnt2]) & 0xffff;
#ifdef _DEBUG

		if (NetNr == 1590)
			ok = 1;

#endif
		start2 = (*NetInfos)[NetNr].Pos;
		stop2 = (*NetInfos)[NetNr + 1].Pos;

		for (cnt3 = start2; cnt3 < stop2; cnt3++)
		{
			Object2 = &((*Objects2)[cnt3]);
			Object2->NewNetNr = (int16) cnt2;

			switch (Object2->ObjectType)
			{
			case SYMBOL_PIN:
				RefName = GetObjectText(Object2->RefNum);

				if ((Object2->Info3 == 0) && ((RefName[0] != 0) || (Object2->Info2 != POWER_CONNECTION)))
				{
					PinName = GetObjectText(Object2->Text1);
					sprintf(NetItemStr, "%s-%s", RefName, PinName);
					cnt = 0;

					while ((cnt < NrNetItems) && (stricmpUTF8(NetItemStr, (*NetItemsPerNet)[cnt]) != 0))
						cnt++;

					if (cnt == NrNetItems)
						strcpy((*NetItemsPerNet)[NrNetItems++], NetItemStr);
					else
						Object2->ObjectType = 0;
				}

				break;

			case SYMBOL_POWERPIN:
				RefName = GetObjectText(Object2->RefNum);

				if ((Object2->Info3 == 0) && ((RefName[0] != 0) || (Object2->Info2 != POWER_CONNECTION)))
				{
					PinName = GetObjectText(Object2->Text2);
					PinCount = 0;

					while (GetPinNameFromPinBus(PinName, PinName2, 100, PinCount) >= 0)
					{
						sprintf(NetItemStr, "%s-%s", RefName, PinName2);
						cnt = 0;

						while ((cnt < NrNetItems) && (stricmpUTF8(NetItemStr, (*NetItemsPerNet)[cnt]) != 0))
							cnt++;

						if (cnt == NrNetItems)
							strcpy((*NetItemsPerNet)[NrNetItems++], NetItemStr);
						else
							Object2->ObjectType = 0;

						PinCount++;
					}
				}

				break;

			case SYMBOL_PINBUS:
				RefName = GetObjectText(Object2->RefNum);
				PinBusName = GetObjectText(Object2->Text1);
				Line[0] = 0;

				if (GetPinNameFromPinBus(PinBusName, Line, Object2->Info3, PinBusNr) >= 0)
				{
					sprintf(NetItemStr, "%s-%s", RefName, Line);
					cnt = 0;

					while ((cnt < NrNetItems) && (stricmpUTF8(NetItemStr, (*NetItemsPerNet)[cnt]) != 0))
						cnt++;

					if (cnt == NrNetItems)
						strcpy((*NetItemsPerNet)[NrNetItems++], NetItemStr);
					else
						Object2->ObjectType = 0;
				}

				break;

			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteNetlist2(int fp)
{
	int32 cnt, cnt2, cnt3, NetCount, start, stop, start2, stop2, NetNr, PinBusNr, res, NrLetsPos, LetPos[2048],
	      LengthLine, cnt5, Length, PinCount, NrPins, fp2, FirstOpen, result, num1, num2;
	LPSTR NetLabelName, RefName, PinName, PinBusName, Net1, Net2;
	int32 OkToWrite, NetError;
	char Letter, Line[MAX_LENGTH_STRING], Line2[MAX_LENGTH_STRING], Line3[MAX_LENGTH_STRING], PinName2[20],
	     PropertyFileName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING],
	     str4[MAX_LENGTH_STRING], LineBuf[MAX_LENGTH_STRING];
	Object2Record *Object2;
#ifdef _DEBUG
	LPSTR StrPos;
#endif

	if ((NetItemsBufGlobal = GlobalAlloc(GHND, 256 * 1024)) == NULL)
		return 0;

	if (GlobalLock(NetItemsBufGlobal) == NULL)
		return 0;

	FirstOpen = 0;
	fp2 = 0;

	GetFilePartFromFileName(str, DesignFile);
	CutExtensionFileName(str);
	sprintf(PropertyFileName, "%s\\%s.prp", DesignPath, str);

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
		(*ObjectsInfo)[cnt].Info2 = 0;

	NetError = 0;

	for (cnt2 = 0; cnt2 < 26; cnt2++)
	{
		NrLetsPos = 0;

		if (cnt2 == 23)
			ok = 1;

		for (cnt = 0; cnt < NrObjectsInfo; cnt++)
		{
			NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);
			Letter = NetLabelName[0];

			if (isalpha(Letter) && ((Letter & 0xdf) == ('A' + cnt2)))
			{
				if (NrLetsPos < 2048)
				{
					LetPos[NrLetsPos++] = (*ObjectsInfo)[cnt].Name;
//          Net1=GetObjectText((*ObjectsInfo)[cnt].Name);
				}

				(*ObjectsInfo)[cnt].Info2 = 1;
			}
		}

		for (cnt = 0; cnt < NrLetsPos - 1; cnt++)
		{
			for (cnt3 = cnt + 1; cnt3 < NrLetsPos; cnt3++)
			{
				if (stricmpUTF8(GetObjectText(LetPos[cnt]), GetObjectText(LetPos[cnt3])) == 0)
				{
					Net1 = GetObjectText(LetPos[cnt]);
					Net2 = GetObjectText(LetPos[cnt3]);
					NetError = 1;
					sprintf(Line, SC(193, "Netname %s in two or more different nets\r\n"), Net1);
					AddMessage(Line);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
					ok = 1;
				}
			}
		}

		ok = 1;
	}

	NrLetsPos = 0;

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		if ((*ObjectsInfo)[cnt].Info2 == 0)
		{
			NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);

			if (NrLetsPos < 2048)
				LetPos[NrLetsPos++] = (*ObjectsInfo)[cnt].Name;
		}
	}

	for (cnt = 0; cnt < NrLetsPos - 1; cnt++)
	{
		for (cnt3 = cnt + 1; cnt3 < NrLetsPos; cnt3++)
		{
			if (stricmpUTF8(GetObjectText(LetPos[cnt]), GetObjectText(LetPos[cnt3])) == 0)
			{
				Net1 = GetObjectText(LetPos[cnt]);
				Net2 = GetObjectText(LetPos[cnt3]);
				NetError = 1;
				sprintf(Line, SC(193, "Warning: Netname %s in two or more different nets\r\n"), Net1);
				AddMessage(Line);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
				ok = 1;
			}
		}
	}

	if (NetError)
		return 0;

	WriteLn(fp, "# Netlist");


// ********************************************************************************
// ********************************************************************************


	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		if (cnt == 549)
			ok = 1;

		(*ObjectsInfo)[cnt].Info2 = 0;
		start = (*ObjectsInfo)[cnt].Pos;

		if ((NetCount = (*ObjectsInfo)[cnt + 1].Pos - start) > 0)
		{
			NrPins = 0;
			stop = start + NetCount;

			for (cnt2 = start; cnt2 < stop; cnt2++)
			{
				NetNr = ((*ObjectsPosition1)[cnt2]) >> 16;
				PinBusNr = ((*ObjectsPosition1)[cnt2]) & 0xffff;
				start2 = (*NetInfos)[NetNr].Pos;
				stop2 = (*NetInfos)[NetNr + 1].Pos;

				for (cnt3 = start2; cnt3 < stop2; cnt3++)
				{
					Object2 = &((*Objects2)[cnt3]);

					switch (Object2->ObjectType)
					{
					case SYMBOL_PIN:
						RefName = GetObjectText(Object2->RefNum);

						if ((Object2->Info3 == 0) && (RefName[0] != 0) && (Object2->Info2 != POWER_CONNECTION))
							NrPins++;

						break;

					case SYMBOL_PINBUS:
						RefName = GetObjectText(Object2->RefNum);
						PinBusName = GetObjectText(Object2->Text1);
						Line[0] = 0;

						if (GetPinNameFromPinBus(PinBusName, Line, Object2->Info3, PinBusNr) >= 0)
							NrPins++;
					}
				}
			}

			if (NrPins < 2)
				(*ObjectsInfo)[cnt].Info2 = 1;
		}
		else
			ok = 1;
	}

// ********************************************************************************
// ********************************************************************************

	for (cnt3 = 0; cnt3 < NrObjects2; cnt3++)
	{
		Object2 = &((*Objects2)[cnt3]);
		Object2->Info5 = 0;
		Object2->NewNetNr = -1;
	}

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		LengthLine = 100;
		NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);
		start = (*ObjectsInfo)[cnt].Pos;
		NetCount = (*ObjectsInfo)[cnt + 1].Pos - start;
		stop = start + NetCount;

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			res = CheckEqualNetItems(start, stop);
			NetNr = ((*ObjectsPosition1)[cnt2]) >> 16;
			PinBusNr = ((*ObjectsPosition1)[cnt2]) & 0xffff;

			if (NetNr == 3)
				ok = 1;

			start2 = (*NetInfos)[NetNr].Pos;
			stop2 = (*NetInfos)[NetNr + 1].Pos;

			for (cnt3 = start2; cnt3 < stop2; cnt3++)
			{
				Object2 = &((*Objects2)[cnt3]);
				Object2->NewNetNr = (int16) cnt;
				OkToWrite = 0;

				switch (Object2->ObjectType)
				{
				case SYMBOL_PIN:
					RefName = GetObjectText(Object2->RefNum);

					if ((Object2->Info3 == 0) && (Object2->Info2 != POWER_CONNECTION))
					{
						if (RefName[0] != 0)
						{
							PinName = GetObjectText(Object2->Text1);
							OkToWrite = 1;
							sprintf(Line3, " %s-%s", RefName, PinName);
						}
						else
							ok = 1;
					}

					break;

				case SYMBOL_POWERPIN:
					RefName = GetObjectText(Object2->RefNum);

					if ((Object2->Info3 == 0) && (RefName[0] != 0) && (Object2->Info2 != POWER_CONNECTION))
					{
						PinName = GetObjectText(Object2->Text2);
						PinCount = 0;

						while (GetPinNameFromPinBus(PinName, PinName2, 100, PinCount) >= 0)
						{
							sprintf(Line3, " %s-%s", RefName, PinName2);

							if (LengthLine > 75)
							{
								sprintf(Line2, "NET   '%s' ", NetLabelName);
								cnt5 = 30 - strlen(Line2);
								strncat(Line2, "                                ", max(0, cnt5));
							}

							strcat(Line2, Line3);
							LengthLine = strlen(Line2);

							if (LengthLine > 75)
								WriteLn(fp, Line2);

							PinCount++;
						}

						OkToWrite = 0;
					}

					break;

				case SYMBOL_PINBUS:
					RefName = GetObjectText(Object2->RefNum);
					PinBusName = GetObjectText(Object2->Text1);
					Line[0] = 0;

					if (GetPinNameFromPinBus(PinBusName, Line, Object2->Info3, PinBusNr) >= 0)
					{
						sprintf(Line3, " %s-%s", RefName, Line);
						OkToWrite = 1;
					}

					break;
				}

				if (OkToWrite)
				{
					if (LengthLine > 75)
					{
						sprintf(Line2, "NET   '%s' ", NetLabelName);
						cnt5 = 30 - strlen(Line2);
						strncat(Line2, "                                ", max(0, cnt5));
					}

					strcat(Line2, Line3);
					LengthLine = strlen(Line2);

					if (LengthLine > 75)
						WriteLn(fp, Line2);
				}
			}
		}

#ifdef _DEBUG

		if (stricmp(NetLabelName, "READ") == 0)
			ok = 1;

#endif
		result = 0;

		if (!FirstOpen)
		{
			if ((fp2 = TextFileOpenUTF8(PropertyFileName)) > 0)
			{
				result = 1;
				FirstOpen = 1;
			}
		}
		else
		{
			TextFileReset(fp2, 0);
			result = 1;
		}

		if (result)
		{
			while ((Length = ReadLn(fp2, LineBuf)) >= 0)
			{
				if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
				{
					GetString(LineBuf, str);
					GetQuoteString(LineBuf, str2);
					GetQuoteString(LineBuf, str3);
					res = strlen(str);

					if (str[res - 1] == ']')
					{
						GetString3('[', str, str4);

						if (sscanf(str, "[%d:%d]", &num1, &num2) == 2)
						{
							for (cnt3 = num1; cnt3 <= num2; cnt3++)
							{
								sprintf(str, "%s%d", str4, cnt3);

								if (stricmpUTF8(NetLabelName, str) == 0)
								{
									sprintf(Line3, " (%s,\"%s\")", str2, str3);

									if (LengthLine > 75)
									{
										sprintf(Line2, "NET   '%s' ", NetLabelName);
										cnt5 = 29 - strlen(Line2);
										strncat(Line2, "                                ", max(0, cnt5));
									}

									strcat(Line2, Line3);
									LengthLine = strlen(Line2);

									if (LengthLine > 75)
										WriteLn(fp, Line2);
								}
							}
						}
					}
					else
					{
						if (stricmpUTF8(NetLabelName, str) == 0)
						{
							sprintf(Line3, " (%s,\"%s\")", str2, str3);

							if (LengthLine > 75)
							{
								sprintf(Line2, "NET   '%s' ", NetLabelName);
								cnt5 = 29 - strlen(Line2);
								strncat(Line2, "                                ", max(0, cnt5));
							}

							strcat(Line2, Line3);
							LengthLine = strlen(Line2);

							if (LengthLine > 75)
								WriteLn(fp, Line2);
						}
					}
				}
			}
		}

		if (LengthLine <= 75)
			WriteLn(fp, Line2);

		ok = 1;
	}

	if (fp2 > 0)
		TextFileClose(fp2);

// ********************************************************************************
// ********************************************************************************

	sprintf(str, "%s\\net.nr", DesignPath);

	if ((fp2 = FileOpenWriteUTF8(str)) <= 0)
		return 0;

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		LengthLine = 100;
		NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);
		sprintf(Line2, "%i %s", cnt, NetLabelName);
		WriteLn(fp2, Line2);
	}

	FileClose(fp2);

	if (WriteLnError != 0)
	{
		sprintf(str2, SC(85, "Could not write to file %s"), str);
		MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
	}

#ifdef _DEBUG
	sprintf(str, "%s\\design.rst", DesignPath);

	if ((fp2 = FileOpenWriteUTF8(str)) <= 0)
		return 0;

	for (cnt3 = 0; cnt3 < NrObjects2; cnt3++)
	{
		Object2 = &((*Objects2)[cnt3]);

		if (Object2->NewNetNr == -1)
		{
			switch (Object2->ObjectType)
			{
			case SYMBOL_PIN:
				PinTypeToText(Object2->Info2, Line2);

				if (Object2->Info3 == 0)
				{
					sprintf(Line, "COMP %s PIN %s %s %s", GetObjectText(Object2->RefNum), GetObjectText(Object2->Text1),
					        GetObjectText(Object2->Text2), Line2);
				}
				else
				{
					sprintf(Line, "COMP %s SHEETPIN %s %s %s NET %i", GetObjectText(Object2->RefNum),
					        GetObjectText(Object2->Text1), GetObjectText(Object2->Text2), Line2, Object2->Info4);
				}

				WriteLn(fp2, Line);
				break;

			case SYMBOL_PINBUS:
				PinTypeToText(Object2->Info2, Line2);
				StrPos = GetObjectText(Object2->RefNum);
				sprintf(Line, "COMP %s PINBUS %s %i %s %s", GetObjectText(Object2->RefNum),
				        GetObjectText(Object2->Text1), Object2->Info3, GetObjectText(Object2->Text2), Line2);
				WriteLn(fp2, Line);
				break;

			case SYMBOL_POWERPIN:
				sprintf(Line, "COMP %s POWERPINS %s", GetObjectText(Object2->RefNum), GetObjectText(Object2->Text2));
				WriteLn(fp2, Line);
				break;

			case WIRE:
				sprintf(Line, "WIRE %i,%i - %i,%i", (int32) Object2->x1, (int32) Object2->y1, (int32) Object2->x2,
				        (int32) Object2->y2);
				WriteLn(fp2, Line);
				break;

			case BUS:
				sprintf(Line, "BUS %i,%i - %i,%i", (int32) Object2->x1, (int32) Object2->y1, (int32) Object2->x2,
				        (int32) Object2->y2);
				WriteLn(fp2, Line);
				break;

			case BUS_CONNECTION:
				if (Object2->Info3 == 0)
					sprintf(Line, "BUSCON %i,%i TO NET %d", (int32) Object2->x1, (int32) Object2->y1, Object2->Info4);
				else
					sprintf(Line, "BUSCON %i,%i", (int32) Object2->x1, (int32) Object2->y1);

				WriteLn(fp2, Line);
				break;

			case GLOBAL_CONNECTION:
				switch (Object2->Info2 >> 1)
				{
				case 0:		// input
					sprintf(Line2, "INPUT");
					break;

				case 1:		// output
					sprintf(Line2, "OUTPUT");
					break;

				case 2:		// i/o
					sprintf(Line2, "IO");
					break;
				}

				sprintf(Line, "GLOBAL %s %s NET %i", Line2, GetObjectText(Object2->Text1), Object2->Info4);
				WriteLn(fp2, Line);
				break;

			case NET_LABEL:
				sprintf(Line, "NETLABEL %s", GetObjectText(Object2->Text1));
				WriteLn(fp2, Line);
				break;

			case ONE_PIN_NET:
				sprintf(Line, "ONE_PIN_NET %i,%i", (int32) Object2->x1, (int32) Object2->y1);
				WriteLn(fp2, Line);
				break;
			}
		}
	}

	FileClose(fp2);
#endif

	GlobalUnlock(NetItemsBufGlobal);
	GlobalFree(NetItemsBufGlobal);
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteNetlist()
{
	int32 cnt, cnt2, cnt3, MinNr, pos, pos2, LengthIdent, LengthValue, NrRefNums, MinObjectNr, LastNr, LastObject4Nr,
	      NrProperties, LengthStr, NrRefCodes, fp, TotalError;
	LPSTR RefName, GeomName, PartValue, InstanceAttrBuf, AttributeValues[64], RefName2, PartNr, AttributeIdents[64],
	      ErrorRef;
	int16 RefNums[16384];
	InstanceRecord *Instance;
	uint8 PartInfo[16];
	char Line[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], str[2048], str2[MAX_LENGTH_STRING], RefCodes[100][6];
	Object2Record *Object4, *Object4a;
	struct tm *today;
	time_t ltime;
#ifdef _DEBUG
	LPSTR p1, p2;
#endif

	Instance = NULL;
	TotalError = 0;
	NrRefCodes = 0;
	MinObjectNr = -1;
	LastObject4Nr = -1;
	ErrorRef = NULL;
#ifdef _DEBUG
	sprintf(FileName, "%s\\compinfo.txt", DesignPath);

	if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
		return 0;

#endif

	for (cnt = 0; cnt < NrObjects4; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);

		if (Object4->Properties != -1)
		{
			RefName = GetObjectText(Object4->RefNum);

			for (cnt2 = 0; cnt2 < NrObjects4; cnt2++)
			{
				if (cnt2 != cnt)
				{
					Object4a = &((*Objects4)[cnt2]);
					RefName2 = GetObjectText(Object4a->RefNum);

					if ((stricmpUTF8(RefName, RefName2) == 0) && (Object4a->Properties == -1))
						Object4a->Properties = Object4->Properties;
				}
			}
		}
	}

	for (cnt = 0; cnt < NrObjects4; cnt++)
	{
		Object4 = &((*Objects4)[cnt]);
		Object4->Info = 0;
		Object4->Info4 = -1;
		RefName = GetObjectText(Object4->RefNum);
#ifdef _DEBUG
		WriteLn(fp, RefName);
#endif
		LengthStr = strlen(RefName);

		if (LengthStr > 0)
		{
			cnt2 = 0;

			while ((cnt2 < LengthStr) && (isalpha(RefName[cnt2])))
				cnt2++;

			memset(&str, 0, 100);
			strncpy(str, RefName, cnt2);
			cnt3 = 0;

			while ((cnt3 < NrRefCodes) && (stricmpUTF8(RefCodes[cnt3], str) != 0))
				cnt3++;

			if (cnt3 >= NrRefCodes)
			{
				strcpy(RefCodes[NrRefCodes], str);
				Object4->Info5 = (int16) NrRefCodes;
				NrRefCodes++;
			}
			else
				Object4->Info5 = (int16) cnt3;

			sscanf(&(RefName[cnt2]), "%i", &(Object4->Info4));
		}
		else
			TotalError = 1;
	}

#ifdef _DEBUG
	FileClose(fp);
#endif

	if (TotalError)
		return 0;

	PcbDesign.NrShapes = 0;

	sprintf(FileName, "%s\\pcb\\%s___.net", DesignPath, LayoutFile); //název souboru

	sprintf(str2, "%s\\pcb\\backup\\%s.net", DesignPath, LayoutFile); //název souboru

	CopyFileUTF8(FileName, str2, 0);

	if ((fp = FileOpenWriteUTF8(FileName)) <= 0)

		return 0;

	//***********************************************************************************************************************

	WriteLn(fp, "# PCB elegance components and netlist file");
	WriteLn(fp, "# Format 3.5");
	time(&ltime);
	today = localtime(&ltime);
	strftime(str, 100, "# Date : %B %d, %Y %X", today);
	WriteLn(fp, str);
	WriteLn(fp, "#");
	WriteLn(fp, "# Property is presented in the format (prop_name,\"prop_value\")");
	WriteLn(fp, "#");
	WriteLn(fp, "# COMP  Reference              Geometry                           Value  Properties");
	WriteLn(fp, "# NET   Netname                Nodes     Properties");
	WriteLn(fp, "#");

	/*
	# date : Wednesday April 25, 2007; 15:17:57
	#
	# Property is presented in the format (prop_name, prop_value).
	#
	# COMP  Reference  Geometry  Value  Properties
	# NET      Netname  Nodes  Properties
	#
	*/
	for (cnt2 = 0; cnt2 < NrRefCodes; cnt2++)
	{
		NrRefNums = 0;

		for (cnt = 0; cnt < NrObjects4; cnt++)
		{
			Object4 = &((*Objects4)[cnt]);

			if ((Object4->Info5 == cnt2) && (NrRefNums < 16384))
				RefNums[NrRefNums++] = (int16) cnt;
		}

#ifdef _DEBUG

		if (stricmp(RefCodes[cnt2], "U") == 0)
			ok = 1;

		if (stricmp(RefCodes[cnt2], "U") == 0)
			ok = 1;

#endif
		LastNr = -1;

		for (cnt = 0; cnt < NrRefNums; cnt++)
		{
			MinNr = 1000000;

			for (cnt3 = 0; cnt3 < NrRefNums; cnt3++)
			{
				Object4 = &((*Objects4)[RefNums[cnt3]]);

				if (Object4->Info == 0)
				{
					if ((Object4->Info4 != -1) && (Object4->Info4 < MinNr))
					{
						MinObjectNr = RefNums[cnt3];
						MinNr = Object4->Info4;
					}
				}
			}

			if (MinNr != 1000000)
			{
#ifdef _DEBUG

				if (MinNr == 3)
					ok = 1;

#endif
				Object4 = &((*Objects4)[MinObjectNr]);
				Object4->Info = 1;

				if (MinNr != LastNr)
				{
					LastNr = MinNr;
					LastObject4Nr = MinObjectNr;
					RefName = GetObjectText(Object4->RefNum);

					if (Object4->Text1 == -1)
						continue;
					else
						GeomName = GetObjectText(Object4->Text1);

					PartValue = GetObjectText(Object4->Value);
					PartNr = GetObjectText(Object4->Text3);
					NrProperties = 0;

					if (Object4->Properties >= 0)
					{
						InstanceAttrBuf = (LPSTR) GetObjectText(Object4->Properties);
						pos = 0;

						while ((pos < sizeof(Instance->Properties) - 2) && (InstanceAttrBuf[pos] != 0))
						{
							LengthIdent = strlen((LPSTR) & InstanceAttrBuf[pos]);

							if ((LengthIdent > 0) && (LengthIdent < 64)
							        && (pos + LengthIdent < sizeof(Instance->Properties) - 1))
							{
								pos2 = pos + LengthIdent + 1;
								LengthValue = strlen((LPSTR) & InstanceAttrBuf[pos2]);

								if ((LengthValue > 0) && (LengthValue < 64)
								        && (pos2 + LengthValue < sizeof(Instance->Properties) - 1))
								{
									if (InstanceAttrBuf[pos] != '~')
									{
										AttributeIdents[NrProperties] = &InstanceAttrBuf[pos];
										AttributeValues[NrProperties] = &InstanceAttrBuf[pos2];
										NrProperties++;
									}
								}

								pos += LengthIdent + LengthValue + 2;
							}
							else
								pos = sizeof(Instance->Properties);
						}
					}

					strcpy(str, "COMP");
					memset(&str[4], ' ', 100);
					memmove(&str[6], RefName, strlen(RefName));
					memmove(&str[31], GeomName, strlen(GeomName));
					AddMentorShape(GeomName);
					str[66] = 0;
					strcat(str, "\"");
					strcat(str, PartValue);
					strcat(str, "\"");

					if ((PartNr) && (PartNr[0] != 0))
					{
						strcat(str, " (PARTNR,\"");
						strcat(str, PartNr);
						strcat(str, "\")");
					}

					for (cnt3 = 0; cnt3 < NrProperties; cnt3++)
					{
						sprintf(str2, " (%s,\"%s\")", AttributeIdents[cnt3], AttributeValues[cnt3]);

						if (strlen(str2) + strlen(str) > 2000)
							break;

						strcat(str, str2);
					}

					if (Object4->PlacingOption != -1)
						strcat(str, " (MULTI_ASSY,\"NP\")");

//          (MULTI_ASSY,"NP")
//          if (stricmp(GetObjectText(Object4->Text2),"POWER_CONNECT")!=0) {          // symbolname
					WriteLn(fp, str);

//          }
					if (Object4->Info3 > 0)
					{	// Multiple parts
						memset(&PartInfo, 0, sizeof(PartInfo));
						PartInfo[Object4->Info3] = 1;
					}
				}
				else
				{
					Object4a = &((*Objects4)[LastObject4Nr]);
#ifdef _DEBUG
					p1 = GetObjectText(Object4->Text2);
					p2 = GetObjectText(Object4a->Text2);

					if (stricmp(p2, "u3") == 0)
						ok = 1;

#endif

//            RefName=GetObjectText(Object4->RefNum);
//            RefName2=GetObjectText(Object4a->RefNum);
					if ((stricmpUTF8(GetObjectText(Object4->Text2),	// symbolname
					                 GetObjectText(Object4a->Text2)) != 0) && ((Object4->Info2 & MULTIPLE_SYMBOLS) == 0)
					        && ((Object4a->Info2 & MULTIPLE_SYMBOLS) == 0))
					{
#ifdef _DEBUG
						p1 = GetObjectText(Object4->Text2);
						p2 = GetObjectText(Object4a->Text2);

						if (stricmp(p1, "u2") == 0)
							ok = 1;

#endif
						sprintf(Line, SC(194, "Different symbols for components with the same reference %s\r\n"),
						        GetObjectText(Object4->RefNum));
						AddMessage(Line);
//            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
						ErrorRef = GetObjectText(Object4->RefNum);
						TotalError = 1;
					}
					else
					{
						if ((Object4->Text1) && (Object4a->Text1)
						        && (stricmpUTF8(GetObjectText(Object4->Text1), GetObjectText(Object4a->Text1)) != 0))
						{
							sprintf(Line, SC(195, "Different geometries for components with the same reference %s\r\n"),
							        GetObjectText(Object4->RefNum));
							AddMessage(Line);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
							ErrorRef = GetObjectText(Object4->RefNum);
							TotalError = 1;
						}
						else
						{
							if ((Object4->Info2 & MULTIPLE_SYMBOLS) != (Object4a->Info2 & MULTIPLE_SYMBOLS))
							{
								sprintf(Line, SC(196, "Error in multiple symbols for the same reference %s\r\n"),
								        GetObjectText(Object4->RefNum));
								AddMessage(Line);
//                SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
								ErrorRef = GetObjectText(Object4->RefNum);
								TotalError = 1;
							}
							else
							{
								if (((Object4->Info3 == 0) && (Object4a->Info3 > 0))
								        || ((Object4a->Info3 == 0) && (Object4->Info3 > 0)))
								{
									sprintf(Line, SC(197, "Error in package part nr for the same reference %s\r\n"),
									        GetObjectText(Object4->RefNum));
									AddMessage(Line);
//                  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
									ErrorRef = GetObjectText(Object4->RefNum);
									TotalError = 1;
								}
								else
								{
									if ((Object4->Info3 > 0) && (PartInfo[Object4->Info3] == 1))
									{
										sprintf(Line,
										        SC(198, "Error in package part nr for the same reference %s%c\r\n"),
										        GetObjectText(Object4->RefNum), Object4->Info3 + 64);
										AddMessage(Line);
//                    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
										ErrorRef = GetObjectText(Object4->RefNum);
										TotalError = 1;
									}
									else
									{
										if ((Object4->Info3 == 0) && (Object4a->Info3 == 0)
										        && ((Object4->Info2 & MULTIPLE_SYMBOLS) == 0)
										        && ((Object4a->Info2 & MULTIPLE_SYMBOLS) == 0))
										{
											sprintf(Line, SC(199, "Double reference %s\r\n"),
											        GetObjectText(Object4->RefNum));
											AddMessage(Line);
//                      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)Line);
											ErrorRef = GetObjectText(Object4->RefNum);
											TotalError = 1;
										}
									}
								}
							}
						}
					}

					PartInfo[Object4->Info3] = 1;
					ok = 1;
				}
			}
		}
	}

	if (!TotalError)
	{
//    CreateMentorGeometries(0);
		if (!WriteNetlist2(fp))
			TotalError = 1;
	}

	FileClose(fp);

	if (WriteLnError != 0)
	{
		sprintf(str2, SC(85, "Could not write to file %s"), FileName);
		MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_APPLMODAL | MB_OK);
	}

	if (TotalError)
	{
#ifndef _DEBUG
		DeleteFileUTF8(FileName);
#endif

		if (ErrorRef)
		{
			sprintf(str2, SC(451, "Do you want to open the sheet with reference %s"), ErrorRef);

			if (MessageBoxUTF8(DESIGNWindow, str2, SC(8, "Message"), MB_APPLMODAL | MB_OKCANCEL) == IDOK)
			{
				strcpy(ProjectInfo->TempStr1, ErrorRef);
				PostMessage(DESIGNWindow, WM_COMMAND, ID_SHEET_OPEN_REF, 1);
			}
		}

		return 0;
	}
	else
	{

		sprintf(FileName, "%s\\pcb\\%s.net", DesignPath, LayoutFile); //název souboru

		sprintf(str2, "%s\\pcb\\backup\\%s.net", DesignPath, LayoutFile); //název souboru

		CopyFileUTF8(FileName, str2, 0);

		sprintf(str2, "%s\\pcb\\%s___.net", DesignPath, LayoutFile); //název souboru

		CopyFileUTF8(str2, FileName, 0);
		DeleteFileUTF8(str2);
	}

	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CollectBussesAndWires()
{
	int32 cnt, cnt2, cnt3, cnt4, cnt5, stop, start, start2, stop2, FoundNr, NetNr, NetNr3, NetNr4, Info, BusNetNr,
	      Objects5Written, PreviousNetNr, Object5WireNr, StartNet, EndNet, BusConnectionObjectNr2, NetLabelObjectNr2,
	      WireCount, BusCount, PinBusCount, WireNetNr, Object5BusPos, Object5BusNr, PreviousBusNr, SheetNr2;
	char FileName[MAX_LENGTH_STRING], GlobalNetName[MAX_LENGTH_STRING];
	char *NetLabelName;
	int32 OkToWrite;
	int fp, result;
	Object2Record *Object2, *Object2b, *Object2c;

	Object5Record *Object5;

#ifdef _DEBUG
	char Line[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	int32 fp2;
	sprintf(str, "%s\\design.bus", DesignPath);

	if ((fp = FileOpenWriteUTF8(str)) <= 0)
		return -1;

#endif

	for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
		(*NetInfos)[NetNr].Progress = 0;

	Object5BusNr = 0;

// ********************************************************************************
// ********************************************************************************
// All nets who are part of a global net will be processed first
// ********************************************************************************
// ********************************************************************************


	for (cnt2 = 0; cnt2 < NrGlobalNets; cnt2++)
	{

// ********************************************************************************
// ********************************************************************************
// Search for Global nets with busses first
// ********************************************************************************
// ********************************************************************************
		if ((GlobalNetsInfo[cnt2] & 1) == 1)
		{
			GlobalNetName[0] = 0;
			GetNetNameGlobalBus(cnt2, GlobalNetName);

			for (cnt = 0; cnt < NrObjectsInfo; cnt++)
				(*ObjectsInfo)[cnt].Info2 = 0;

#ifdef _DEBUG
			sprintf(Line, "*************************** BUS %i %s **********************", Object5BusNr, GlobalNetName);
			WriteLn(fp, Line);
#endif

			for (cnt3 = GlobalNetsPos[cnt2]; cnt3 < GlobalNetsPos[cnt2 + 1]; cnt3++)
			{
				BusNetNr = GlobalNets[cnt3];
				Info = (*NetInfos)[BusNetNr].Info;
				start = (*NetInfos)[BusNetNr].Pos;
				stop = (*NetInfos)[BusNetNr + 1].Pos;

				for (cnt = start; cnt < stop; cnt++)
				{
					Object2 = &((*Objects2)[cnt]);

					switch (Object2->ObjectType)
					{
					case BUS:
						if ((NrObjects5 + 1 >= MaxNrObjects5) && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
							return 0;

						Object5 = &((*Objects5)[NrObjects5]);
						Object5->ObjectType = BUS;
						Object5->x1 = (float) Object2->x1;
						Object5->y1 = (float) Object2->y1;
						Object5->x2 = (float) Object2->x2;
						Object5->y2 = (float) Object2->y2;
						Object5->BusNr = Object5BusNr;
						Object5->SheetNr = (int16) Object2->SheetNr;
						Object5->NetNr = -1;
						NrObjects5++;
#ifdef _DEBUG
						sprintf(Line, "BUS %i,%i - %i,%i      %s", (int32) Object2->x1, (int32) Object2->y1,
						        (int32) Object2->x2, (int32) Object2->y2, Sheets[Object2->SheetNr].SheetName);
						WriteLn(fp, Line);
#endif
						break;

					case GLOBAL_CONNECTION:
						if ((NrObjects5 + 1 >= MaxNrObjects5) && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
							return 0;

						Object5 = &((*Objects5)[NrObjects5]);
						Object5->ObjectType = GLOBAL_CONNECTION;
						Object5->x1 = (float) Object2->x1;
						Object5->y1 = (float) Object2->y1;
						Object5->BusNr = Object5BusNr;
						Object5->SheetNr = (int16) Object2->SheetNr;
						Object5->NetNr = -1;
						NrObjects5++;
#ifdef _DEBUG
						sprintf(Line, "GLOBAL %i,%i      %s", (int32) Object2->x1, (int32) Object2->y1,
						        Sheets[Object2->SheetNr].SheetName);
						WriteLn(fp, Line);
#endif
						break;
					}
				}

				if ((GlobalNetsInfo[cnt2] & 2) == 0)
				{
					SheetNr2 = (*NetInfos)[BusNetNr].SheetNr;
					StartNet = Sheets[SheetNr2].NetPos;
					EndNet = StartNet + Sheets[SheetNr2].NrNets;

// ********************************************************************************
// Start search for objects in sheet SheetNr2
// ********************************************************************************
					for (cnt4 = StartNet; cnt4 < EndNet; cnt4++)
					{
						if (((*NetInfos)[cnt4].Info & 0xa000) == 0xa000)
						{
// ********************************************************************************
// Label with a bus connection (Bus is BusNetNr)
// ********************************************************************************
							BusConnectionObjectNr2 = (*NetInfos)[cnt4].BusConnectionObjectNr;
							Object2c = &((*Objects2)[BusConnectionObjectNr2]);
							NetNr3 = Object2c->NetNr;

							if (Object2c->Info4 == BusNetNr)
							{
								if (((*NetInfos)[NetNr3].Info & 0x400) == 0x400)
								{
									ok = 1;
									PinBusCount = (*NetInfos)[NetNr3].NrPinsPinBus;

									for (cnt = 0; cnt < NrObjectsInfo; cnt++)
									{
										start = (*ObjectsInfo)[cnt].Pos;
										stop = (*ObjectsInfo)[cnt + 1].Pos;

										for (cnt5 = start; cnt5 < stop; cnt5++)
										{
											NetNr4 = ((*ObjectsPosition1)[cnt5]) >> 16;

											if ((NetNr4 == NetNr3) && ((*ObjectsInfo)[cnt].Info2 == 0))
											{
												if ((NrObjects5 + 1 >= MaxNrObjects5)
												        && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
													return 0;

												Object5 = &((*Objects5)[NrObjects5]);
												Object5->ObjectType = BUS_INFO;
												Object5->BusNr = Object5BusNr;
												Object5->NetNr = cnt;
												Object5->SheetNr = -1;
												NrObjects5++;

#ifdef _DEBUG
												sprintf(Line, "Object pinbus netnr  %i           %i", cnt4, cnt);
												WriteLn(fp, Line);
#endif
												(*ObjectsInfo)[cnt].Info2 = 1;
												ok = 1;
											}
										}
									}
								}
								else
								{
									NetNr4 = Object2c->NewNetNr;

									if ((NetNr4 != -1) && ((*ObjectsInfo)[NetNr4].Info2 == 0))
									{
										(*ObjectsInfo)[NetNr4].Info2 = 1;

										if ((NrObjects5 + 1 >= MaxNrObjects5)
										        && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
											return 0;

										Object5 = &((*Objects5)[NrObjects5]);
										Object5->ObjectType = BUS_INFO;
										Object5->BusNr = Object5BusNr;
										Object5->NetNr = NetNr4;
										Object5->SheetNr = -1;
										NrObjects5++;

#ifdef _DEBUG
										sprintf(Line, "Object netnr %i             %i", Object2c->NetNr, NetNr4);
										WriteLn(fp, Line);
#endif
									}
								}

							}
						}
					}
				}
				else
				{
// ********************************************************************************
// ********************************************************************************
// Global/local net is a pure pinbus

					ok = 1;

					if ((Info & 0x400) == 0x400)
					{
						for (cnt = 0; cnt < NrObjectsInfo; cnt++)
						{
							start = (*ObjectsInfo)[cnt].Pos;
							stop = (*ObjectsInfo)[cnt + 1].Pos;

							for (cnt5 = start; cnt5 < stop; cnt5++)
							{
								NetNr4 = ((*ObjectsPosition1)[cnt5]) >> 16;

								if ((NetNr4 == BusNetNr) && ((*ObjectsInfo)[cnt].Info2 == 0))
								{
									if ((NrObjects5 + 1 >= MaxNrObjects5)
									        && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
										return 0;

									Object5 = &((*Objects5)[NrObjects5]);
									Object5->ObjectType = BUS_INFO;
									Object5->BusNr = Object5BusNr;
									Object5->NetNr = cnt;
									Object5->SheetNr = -1;
									NrObjects5++;

#ifdef _DEBUG
									sprintf(Line, "Object pinbus netnr  %i           %i", BusNetNr, cnt);
									WriteLn(fp, Line);
#endif
									(*ObjectsInfo)[cnt].Info2 = 1;
									ok = 1;
								}
							}
						}
					}
				}

				(*NetInfos)[BusNetNr].Progress = 1;
			}

#ifdef _DEBUG
			sprintf(Line, "*************************** END *******************************");
			WriteLn(fp, Line);
#endif
			Object5BusNr++;
		}
		else
		{
// ********************************************************************************
// ********************************************************************************
// Global net single wire



		}
	}

#ifdef _DEBUG
	sprintf(Line, "***************************************************************");
	WriteLn(fp, Line);
	WriteLn(fp, Line);
	WriteLn(fp, Line);
#endif

// ********************************************************************************
// ********************************************************************************
// ********************************************************************************
// ********************************************************************************

	NetLabelName = NULL;
	NetNr = 0;

	for (NetNr = 0; NetNr < TotalNrNets; NetNr++)
	{
		if ((*NetInfos)[NetNr].Progress == 0)
		{
			Info = (*NetInfos)[NetNr].Info;

			if ((Info & 0x20400) == 0x20400)
			{
				FoundNr = -1;
				BusNetNr = NetNr;

				NetLabelObjectNr2 = (*NetInfos)[BusNetNr].NetLabelObjectNr;

				if (NetLabelObjectNr2 != -1)
				{
					Object2b = &((*Objects2)[NetLabelObjectNr2]);
					NetLabelName = GetObjectText(Object2b->Text1);
#ifdef _DEBUG
					sprintf(Line, "***************** PINBUS %i %s ***********************", Object5BusNr, NetLabelName);
#endif
				}
				else
				{
#ifdef _DEBUG
					sprintf(Line, "***************** PINBUS %i ***********************", Object5BusNr);
					WriteLn(fp, Line);
#endif
				}

				start = (*NetInfos)[BusNetNr].Pos;
				stop = (*NetInfos)[BusNetNr + 1].Pos;

				for (cnt2 = start; cnt2 < stop; cnt2++)
				{
					Object2 = &((*Objects2)[cnt2]);

					if (Object2->ObjectType == BUS)
					{
						if ((NrObjects5 + 1 >= MaxNrObjects5) && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
							return 0;

						Object5 = &((*Objects5)[NrObjects5]);
						Object5->ObjectType = BUS;
						Object5->x1 = (float) Object2->x1;
						Object5->y1 = (float) Object2->y1;
						Object5->x2 = (float) Object2->x2;
						Object5->y2 = (float) Object2->y2;
						Object5->BusNr = Object5BusNr;
						Object5->SheetNr = (int16) Object2->SheetNr;
						Object5->NetNr = -1;
						NrObjects5++;
#ifdef _DEBUG
						sprintf(Line, "PINBUS  %s %i,%i - %i,%i      %s", NetLabelName, (int32) Object2->x1,
						        (int32) Object2->y1, (int32) Object2->x2, (int32) Object2->y2,
						        Sheets[Object2->SheetNr].SheetName);
						WriteLn(fp, Line);
#endif
					}
				}

				for (cnt = 0; cnt < NrObjectsInfo; cnt++)
					(*ObjectsInfo)[cnt].Info2 = 0;

				for (cnt = 0; cnt < NrObjectsInfo; cnt++)
				{
					start = (*ObjectsInfo)[cnt].Pos;
					stop = (*ObjectsInfo)[cnt + 1].Pos;

					for (cnt2 = start; cnt2 < stop; cnt2++)
					{
						NetNr4 = ((*ObjectsPosition1)[cnt2]) >> 16;

						if ((NetNr4 == BusNetNr) && ((*ObjectsInfo)[cnt].Info2 == 0))
						{
							if ((NrObjects5 + 1 >= MaxNrObjects5) && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
								return 0;

							Object5 = &((*Objects5)[NrObjects5]);
							Object5->ObjectType = BUS_INFO;
							Object5->BusNr = Object5BusNr;
							Object5->NetNr = cnt;
							Object5->SheetNr = -1;
							NrObjects5++;
#ifdef _DEBUG
							sprintf(Line, "Object pinbus netnr             %i", cnt);
							WriteLn(fp, Line);
#endif
							(*ObjectsInfo)[cnt].Info2 = 1;
							ok = 1;
						}
					}
				}

				(*NetInfos)[BusNetNr].Progress = 1;
#ifdef _DEBUG
				sprintf(Line, "*************************** END *******************************");
				WriteLn(fp, Line);
#endif
				Object5BusNr++;
			}
		}
	}


#ifdef _DEBUG
	sprintf(Line, "***************************************************************");
	WriteLn(fp, Line);
	WriteLn(fp, Line);
	WriteLn(fp, Line);
#endif


// ********************************************************************************
// ********************************************************************************
// ********************************************************************************
// ********************************************************************************

	Object5WireNr = NrObjects5;

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		start = (*ObjectsInfo)[cnt].Pos;
		stop = (*ObjectsInfo)[cnt + 1].Pos;
#ifdef _DEBUG
		sprintf(Line, "*************************** WIRE *******************************");
		WriteLn(fp, Line);
#endif

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			WireNetNr = ((*ObjectsPosition1)[cnt2]) >> 16;
			start2 = (*NetInfos)[WireNetNr].Pos;
			stop2 = (*NetInfos)[WireNetNr + 1].Pos;

			for (cnt3 = start2; cnt3 < stop2; cnt3++)
			{
				Object2 = &((*Objects2)[cnt3]);

				switch (Object2->ObjectType)
				{
				case WIRE:
					if ((NrObjects5 + 1 >= MaxNrObjects5) && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
						return 0;

					Object5 = &((*Objects5)[NrObjects5]);
					Object5->ObjectType = WIRE;
					Object5->x1 = (float) Object2->x1;
					Object5->y1 = (float) Object2->y1;
					Object5->x2 = (float) Object2->x2;
					Object5->y2 = (float) Object2->y2;
					Object5->BusNr = -1;
					Object5->SheetNr = (int16) Object2->SheetNr;
					Object5->NetNr = cnt;
					NrObjects5++;
#ifdef _DEBUG
					sprintf(Line, "WIRE %i,%i - %i,%i    %s", (int32) Object2->x1, (int32) Object2->y1,
					        (int32) Object2->x2, (int32) Object2->y2, Sheets[Object2->SheetNr].SheetName);
					WriteLn(fp, Line);
#endif
					break;

				case GLOBAL_CONNECTION:
					if ((NrObjects5 + 1 >= MaxNrObjects5) && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
						return 0;

					Object5 = &((*Objects5)[NrObjects5]);
					Object5->ObjectType = GLOBAL_CONNECTION;
					Object5->x1 = (float) Object2->x1;
					Object5->y1 = (float) Object2->y1;
					Object5->x2 = (float) Object2->x2;
					Object5->y2 = (float) Object2->y2;
					Object5->BusNr = -1;
					Object5->SheetNr = (int16) Object2->SheetNr;
					Object5->NetNr = cnt;
					NrObjects5++;
#ifdef _DEBUG
					sprintf(Line, "GLOBAL %i,%i   %s", (int32) Object2->x1, (int32) Object2->y1,
					        Sheets[Object2->SheetNr].SheetName);
					WriteLn(fp, Line);
#endif
					break;
				}
			}
		}

		NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);
#ifdef _DEBUG
		sprintf(Line, "Net %s -> netnr                %i", NetLabelName, cnt);
		WriteLn(fp, Line);
		sprintf(Line, "*************************** END *******************************");
		WriteLn(fp, Line);
#endif
	}

#ifdef _DEBUG
	FileClose(fp);
#endif

// ********************************************************************************
// ********************************************************************************
// ********************************************************************************
// ********************************************************************************

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(FileName, "%s\\sch\\", DesignPath);
//    strcpy(FileName,"c:\\design\\");
		strcat(FileName, Sheets[cnt2].SheetName);
		strcat(FileName, ".wir");

		if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
			return 0;

#ifdef _DEBUG
		sprintf(FileName, "%s\\sch\\", DesignPath);
		strcat(FileName, Sheets[cnt2].SheetName);
		strcat(FileName, ".wtx");

		if ((fp2 = FileOpenWriteUTF8(FileName)) <= 0)
			return 0;

#endif
		WireCount = 0;
		BusCount = 0;
		Object5BusPos = 0;
		PreviousBusNr = 0;
		Objects5Written = 0;
		PreviousNetNr = -1;

		for (cnt3 = 0; cnt3 < Object5BusNr; cnt3++)
		{
			OkToWrite = 0;

			for (cnt = 0; cnt < Object5WireNr; cnt++)
			{
				Object5 = &((*Objects5)[cnt]);

				if ((Object5->SheetNr == cnt2) && (Object5->BusNr == cnt3))
					OkToWrite = 1;
			}

			Object5BusPos = Objects5Written;

			for (cnt = 0; cnt < Object5WireNr; cnt++)
			{
				Object5 = &((*Objects5)[cnt]);

				if ((Object5->BusNr == cnt3)
				        && (((OkToWrite) && (Object5->SheetNr == -1)) || (Object5->SheetNr == cnt2)))
				{
					Object5->FirstObjectNr = Object5BusPos;
					FileWrite(fp, Object5, sizeof(Object5Record), &result);
#ifdef _DEBUG
					sprintf(str3, "BUS  %3.0f ,%3.0f  - %3.0f ,%3.0f    %i", Object5->x1, Object5->y1, Object5->x2,
					        Object5->y2, Object5->NetNr);
					WriteLn(fp2, str3);
#endif
					Objects5Written++;
				}
			}
		}

		for (cnt = Object5WireNr; cnt < NrObjects5; cnt++)
		{
			Object5 = &((*Objects5)[cnt]);

			if (Object5->SheetNr == cnt2)
			{
				if (Object5->NetNr != PreviousNetNr)
				{
					Object5BusPos = Objects5Written;
					PreviousNetNr = Object5->SheetNr;
				}

				Object5->FirstObjectNr = Object5BusPos;
				FileWrite(fp, Object5, sizeof(Object5Record), &result);
#ifdef _DEBUG
				sprintf(str3, "WIRE %3.0f ,%3.0f  - %3.0f ,%3.0f    %i", Object5->x1, Object5->y1, Object5->x2,
				        Object5->y2, Object5->NetNr);
				WriteLn(fp2, str3);
#endif
				Objects5Written++;
			}
		}

		FileClose(fp);

#ifdef _DEBUG
		FileClose(fp2);
#endif
	}

	ok = 1;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckERC(void)
{
	int32 cnt, cnt2, cnt3, NetCount, start, stop, start2, stop2, NetNr, PinBusNr, OutputPins[256], PowerPins[256],
	      LengthLine, cnt5, PinCount, NrOutputPins, NrPowerPins, NrInputPins;
	LPSTR NetLabelName, RefName, PinName, PinBusName;
	int32 OkToWrite;
	char Line[MAX_LENGTH_STRING], Line2[MAX_LENGTH_STRING], Line3[MAX_LENGTH_STRING], PinName2[MAX_LENGTH_STRING];
	Object2Record *Object2;

//  Object2->Info2    pintype,pinconnection
//  switch (Object2->Info2 & 0xff) {
//    case CONNECTION_INPUT:
//    case CONNECTION_OUTPUT:
//    case CONNECTION_IO:
//    case CONNECTION_TRISTATE:
//    case CONNECTION_OC:
//    case CONNECTION_PASSIVE:
//    case CONNECTION_POWER:
//  switch ((Object2->Info2 >> 8) & 0xff) {
//    case CONNECTION_UNDEFINED:
//    case CONNECTION_TTL:
//    case CONNECTION_LVTTL:
//    case CONNECTION_CMOS:
//    case CONNECTION_ANALOG:

	for (cnt = 0; cnt < NrObjectsInfo; cnt++)
	{
		LengthLine = 100;
		NetLabelName = GetObjectText((*ObjectsInfo)[cnt].Name);
		NrOutputPins = 0;
		NrPowerPins = 0;
		NrInputPins = 0;
		start = (*ObjectsInfo)[cnt].Pos;
		NetCount = (*ObjectsInfo)[cnt + 1].Pos - start;
		stop = start + NetCount;

		for (cnt2 = start; cnt2 < stop; cnt2++)
		{
			NetNr = ((*ObjectsPosition1)[cnt2]) >> 16;
			PinBusNr = ((*ObjectsPosition1)[cnt2]) & 0xffff;
#ifdef _DEBUG

			if (NetNr == 1590)
				ok = 1;

#endif
			start2 = (*NetInfos)[NetNr].Pos;
			stop2 = (*NetInfos)[NetNr + 1].Pos;

			for (cnt3 = start2; cnt3 < stop2; cnt3++)
			{
				Object2 = &((*Objects2)[cnt3]);
				Object2->NewNetNr = (int16) cnt;
				OkToWrite = 0;

				switch (Object2->ObjectType)
				{
				case SYMBOL_PIN:
					RefName = GetObjectText(Object2->RefNum);

					if ((Object2->Info3 == 0) && ((RefName[0] != 0) || (Object2->Info2 != POWER_CONNECTION)))
					{
						PinName = GetObjectText(Object2->Text1);
						OkToWrite = 1;
						sprintf(Line3, " %s-%s", RefName, PinName);

						switch (Object2->Info2 & 0xff)
						{
						case CONNECTION_INPUT:
							NrInputPins++;
							break;

						case CONNECTION_OUTPUT:
							OutputPins[NrOutputPins] = cnt3;
							NrOutputPins++;
							break;

						case CONNECTION_POWER:
							PowerPins[NrPowerPins] = cnt3;
							NrPowerPins++;
							break;
						}
					}

					break;

				case SYMBOL_POWERPIN:
					RefName = GetObjectText(Object2->RefNum);

					if ((Object2->Info3 == 0) && ((RefName[0] != 0) || (Object2->Info2 != POWER_CONNECTION)))
					{
						PinName = GetObjectText(Object2->Text2);
						PinCount = 0;

						while (GetPinNameFromPinBus(PinName, PinName2, 100, PinCount) >= 0)
						{
							sprintf(Line3, " %s-%s", RefName, PinName2);

							if (LengthLine > 75)
							{
								sprintf(Line2, "NET   '%s' ", NetLabelName);
								cnt5 = 30 - strlen(Line2);
								strncat(Line2, "                                       ", max(0, cnt5));
							}

							strcat(Line2, Line3);
							LengthLine = strlen(Line2);
//                if (LengthLine>75) {
//                  WriteLn(fp,Line2);
//                }
							PinCount++;

							switch (Object2->Info2 & 0xff)
							{
							case CONNECTION_INPUT:
								NrInputPins++;
								break;

							case CONNECTION_OUTPUT:
								OutputPins[NrOutputPins] = cnt3;
								NrOutputPins++;
								break;

							case CONNECTION_POWER:
								PowerPins[NrPowerPins] = cnt3;
								NrPowerPins++;
								break;
							}
						}

						OkToWrite = 0;
					}

					break;

				case SYMBOL_PINBUS:
					RefName = GetObjectText(Object2->RefNum);
					PinBusName = GetObjectText(Object2->Text1);
					Line[0] = 0;

					if (GetPinNameFromPinBus(PinBusName, Line, Object2->Info3, PinBusNr) >= 0)
					{
//              sprintf(Line3," %s-%s",RefName,Line);
						OkToWrite = 1;
					}

					break;
				}

				if (OkToWrite)
				{
					if (LengthLine > 75)
					{
						sprintf(Line2, "NET   '%s' ", NetLabelName);
						cnt5 = 30 - strlen(Line2);
						strncat(Line2, "                                      ", max(0, cnt5));
					}

					strcat(Line2, Line3);
					LengthLine = strlen(Line2);
//          if (LengthLine>75) {
//            WriteLn(fp,Line2);
//          }
				}
			}
		}

//    if (LengthLine<=75) {
//      WriteLn(fp,Line2);
//    }
		ok = 1;

		if (NrOutputPins > 1)
		{
			if (NrPowerPins > 0)
			{
			}
			else
			{
			}
		}

		if ((NrOutputPins == 1) && (NrPowerPins > 0))
		{
		}
	}

	return 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
