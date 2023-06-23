/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: gateswap.c
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
#include "windows.h"
#include "memory.h"
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "nets.h"
#include "calc.h"
#include "calc2.h"
#include "calc3.h"
#include "toets.h"
#include "calcdef.h"
#include "time.h"
#include "fcntl.h"
#include "draw2.h"
#include "draw3.h"
#include "errno.h"
#include "sys/stat.h"
#include "pcb.h"
#include "files2.h"
#include "gateswap.h"
#include "dialogs.h"
#include "menus.h"
#include "graphics.h"
#include "mainloop.h"
#include "resource.h"
#include "import.h"
#include "utf8.h"


HGLOBAL ObjectTextBufGlobal = 0;
HGLOBAL GatePinSwapGlobal = 0;
char *ObjectTextBuf;
int32 ObjectTextBufMemSize = 0;
int32 ObjectTextBufPos = 0;
int32 MaxNrGatePinSwaps = 0;
int32 NrGatePinSwaps = 0;
int32 GateSwapResult, PinSwapResult, CurrentPartNr, CurrentPartNr2;
int32 GatePinSwapMode = 0;
extern int32 SelectColorMode;
char GatePinSwapReference[64], GatePinSwapPinName[64];

GatePinSwapArray *GatePinSwaps;

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 AllocateMemObjectTextBuf(int32 MemSize)
{
	HGLOBAL NewMem;


	if (ObjectTextBufMemSize == 0)
	{
		MemSize = max(4096, MemSize);

		if ((ObjectTextBufGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((ObjectTextBuf = (char *) GlobalLock(ObjectTextBufGlobal)) == NULL)
			return -1;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(ObjectTextBufGlobal, MemSize, GHND)) == NULL)
			return -1;

		ObjectTextBufGlobal = NewMem;

		if ((ObjectTextBuf = (char *) GlobalLock(ObjectTextBufGlobal)) == NULL)
			return -1;
	}

	ObjectTextBufMemSize = MemSize;
	return 0;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 AllocateMemGatePinSwaps(int32 count)
{
	HGLOBAL NewMem;

	if (MaxNrGatePinSwaps == 0)
	{
		count = max(count, 256);

		if ((GatePinSwapGlobal = GlobalAlloc(GHND, count * sizeof(GatePinSwapRecord))) == NULL)
			return -1;

		if ((GatePinSwaps = (GatePinSwapArray *) GlobalLock(GatePinSwapGlobal)) == NULL)
			return -1;

		MaxNrGatePinSwaps = count;
	}
	else
	{
		if ((NewMem = GlobalReAlloc(GatePinSwapGlobal, count * sizeof(GatePinSwapRecord), GHND)) == NULL)
			return -1;

		GatePinSwapGlobal = NewMem;

		if ((GatePinSwaps = (GatePinSwapArray *) GlobalLock(GatePinSwapGlobal)) == NULL)
			return -1;

		MaxNrGatePinSwaps = count;
	}

	return 0;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 DeallocateMemObjectTextBuf()
{
	if (ObjectTextBufGlobal != NULL)
	{
		GlobalUnlock(ObjectTextBufGlobal);
		GlobalFree(ObjectTextBufGlobal);
		ObjectTextBufGlobal = NULL;
		ObjectTextBufMemSize = 0;
		ObjectTextBufPos = 0;
	}

	return 0;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

void DeallocateMemGatePinSwap()
{
	if (GatePinSwapGlobal != NULL)
	{
		GlobalUnlock(GatePinSwapGlobal);
		GlobalFree(GatePinSwapGlobal);
		GatePinSwapGlobal = NULL;
		MaxNrGatePinSwaps = 0;
	}
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 ObjectTextToBuf(LPSTR ObjectText)
{
	int32 lengte, hulp;
	char *TextPos;

	if (ObjectText[0] == 0)
		lengte = 0;
	else
		lengte = strlen(ObjectText);

	if (lengte + 1 + ObjectTextBufPos > ObjectTextBufMemSize)
	{
		if (AllocateMemObjectTextBuf(ObjectTextBufMemSize + 4096) == -1)
			return 0;
	}

	hulp = ObjectTextBufPos;
	TextPos = ObjectTextBuf + ObjectTextBufPos;
	memmove(TextPos, ObjectText, lengte + 1);
	ObjectTextBufPos += lengte + 1;
	return hulp;

}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

LPSTR GetObjectText(int32 Pos)
{
	char *TextPos;

	TextPos = ObjectTextBuf + Pos;
	return TextPos;

}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

void GetString2(LPSTR Str, LPSTR Result, int32 MaxLength)
{
	int32 cnt, l, pos;

	memset(Result, 0, MaxLength);
	l = strlen(Str);
	cnt = 0;

	while ((cnt < l) && ((Str[cnt] == ' ') || (Str[cnt] == '\t')))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	pos = cnt;
	cnt++;

	while ((cnt < l) && (Str[cnt] != ' ') && (Str[cnt] != '\t'))
		cnt++;

	memmove(Result, &Str[pos], cnt - pos);

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt], l - cnt + 1);
}


// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 LoadGatePinSwapInfo(int32 mode)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[1024], str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING],
	     LineBuf[1024];
	int32 fp, Length, hulp, GateNr, ok;
	GatePinSwapRecord *GatePinSwap;

	GateNr = 0;
	sprintf(str, "%s\\pcb\\gatepin.swp", DesignPath);

	if (FileExistsUTF8(str) != 0)
		return -1;

	if ((fp = TextFileOpenUTF8(str)) <= 0)
		return -1;

	memset(&LineBuf, 0, 1024);

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, 512)) >= 0)
	{
		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
#ifdef _DEBUG

			if (TextLineNr == 927)
				ok = 1;

#endif

			GetString2(LineBuf, str, 200);
			GetString2(LineBuf, str2, 200);
			GetString2(LineBuf, str3, 1024);
			GetString2(LineBuf, str4, 200);
			GetString2(LineBuf, str5, 200);

			if (stricmpOwn(str, "GATE") == 0)
			{
				GateNr = 0;

				if (NrGatePinSwaps == MaxNrGatePinSwaps)
					AllocateMemGatePinSwaps(MaxNrGatePinSwaps + 16);

				GatePinSwap = &((*GatePinSwaps)[NrGatePinSwaps]);
				GatePinSwap->ObjectType = 1;
				GatePinSwap->Info2 = ObjectTextToBuf(str2);	// Reference

				if (sscanf(str3, "%i", &hulp) != 1)
				{
					NrGatePinSwaps = 0;
					return -1;
				}

				if (sscanf(str3, "%i", &hulp) != 1)
				{
					NrGatePinSwaps = 0;
					return -1;
				}

				GatePinSwap->Info3 = hulp;	//  Part nr

				if (sscanf(str4, "%i", &hulp) != 1)
				{
					NrGatePinSwaps = 0;
					return -1;
				}

				GateNr = hulp;
				GatePinSwap->GateNr = GateNr;	//  Gate code nr

				GatePinSwap->Info4 = ObjectTextToBuf(str5);
				GatePinSwap->SwapInfo = 0;
				NrGatePinSwaps++;
			}
			else
			{
				if (stricmpOwn(str, "PIN") == 0)
				{
					if (NrGatePinSwaps == MaxNrGatePinSwaps)
						AllocateMemGatePinSwaps(MaxNrGatePinSwaps + 16);

					GatePinSwap = &((*GatePinSwaps)[NrGatePinSwaps]);
					GatePinSwap->ObjectType = 2;
					hulp = strlen(str2);

					if ((hulp < 3) || (str2[0] != '\'') || (str2[hulp - 1] != '\''))
					{
						NrGatePinSwaps = 0;
						return -1;
					}

					memmove(&str2[0], &str2[1], 100);
					str2[hulp - 2] = 0;
					GatePinSwap->Info2 = ObjectTextToBuf(str2);

					if (sscanf(str3, "%i", &hulp) != 1)
					{
						NrGatePinSwaps = 0;
						return -1;
					}

					GatePinSwap->SwapInfo = hulp;
					GatePinSwap->GateNr = GateNr;
					GatePinSwap->Info3 = 0;
					GatePinSwap->Info4 = 0;
					NrGatePinSwaps++;
				}
				else
				{
					if (stricmpOwn(str, "PINBUS") == 0)
					{
						if (NrGatePinSwaps == MaxNrGatePinSwaps)
							AllocateMemGatePinSwaps(MaxNrGatePinSwaps + 16);

						GatePinSwap = &((*GatePinSwaps)[NrGatePinSwaps]);
						GatePinSwap->ObjectType = 3;

						if (sscanf(str2, "%i", &hulp) != 1)
						{
							NrGatePinSwaps = 0;
							return -1;
						}

						GatePinSwap->Info3 = hulp;	// Nr pins
						hulp = strlen(str3);

						if ((hulp < 3) || (str3[0] != '\'') || (str3[hulp - 1] != '\''))
						{
							NrGatePinSwaps = 0;
							return -1;
						}

						memmove(&str3[0], &str3[1], 100);
						str3[hulp - 2] = 0;
						GatePinSwap->Info2 = ObjectTextToBuf(str3);

						if (sscanf(str4, "%i", &hulp) != 1)
						{
							NrGatePinSwaps = 0;
							return -1;
						}

						GatePinSwap->SwapInfo = hulp;
						GatePinSwap->GateNr = GateNr;
						GatePinSwap->Info4 = ObjectTextToBuf(str5);	// LabelName
						NrGatePinSwaps++;
					}
				}
			}
		}

		memset(&LineBuf, 0, 1024);
	}

	TextFileClose(fp);
	ok = 1;
	return 0;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 GetPinNameFromPinBus(LPSTR PinBusName, LPSTR PinName, int32 NrPins, int32 Index)
{
	int32 lengte, cnt, cnt2, pos1, pos2, LineNr;
	int32 FoundKomma;
	char PreviousChar;

	LineNr = 0;
	lengte = strlen(PinBusName);

	if (lengte == 0)
		return -1;

	if (Index < 0)
		return -1;

	cnt = 0;
	cnt2 = 0;
	pos1 = 0;
	pos2 = 0;
	PreviousChar = 0;
	FoundKomma = 1;

	while ((cnt < lengte) && (cnt2 < Index + 1))
	{
		if ((PinBusName[cnt] == ',') || (PinBusName[cnt] == '\\'))
		{
			FoundKomma = 1;
			pos2 = cnt;

			if ((cnt < lengte - 1) && (PinBusName[cnt + 1] == '\\'))
				cnt++;

			cnt2++;
		}
		else
		{
			if (FoundKomma)
			{
				pos1 = cnt;
				FoundKomma = 0;

				if (PreviousChar == '\\')
					LineNr++;
			}
		}

		PreviousChar = PinBusName[cnt];

		if (cnt2 != Index + 1)
			cnt++;
	}

	if (cnt == lengte)
	{
		pos2 = cnt;
		cnt2++;
	}

	if (cnt2 != Index + 1)
		return -1;

	memmove(PinName, &PinBusName[pos1], pos2 - pos1);
	PinName[pos2 - pos1] = 0;
	return LineNr;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 GetGateSwapInfo(LPSTR Reference, LPSTR PinName, LPSTR Reference2, LPSTR PinName2, LPSTR SrcPins, LPSTR DestPins,
                      int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, res, GateNr, CurrentGateNr, CurrentGateNr2, GateNrCount, PartNr, FirstCntGateNr,
	      LastCntGateNr, FirstCntGateNr2, LastCntGateNr2, GateNr2, PartNr2, GateSpecialNr, DisplayMode, CurrentCntGateNr,
	      CurrentCntGateNr2, PinNr, CompNr, GroupIndex[16], SwapInfo, PinGroupNr, CurrentPinGroupNr, CurrentPinGroupPos,
	      PinGroupPos, PinGroupLine, CurrentPinGroupLine, SwapInfo2, CurrentPinGroupNr2, CurrentPinGroupPos2,
	      CurrentPinGroupLine2;
	GatePinSwapRecord *GatePinSwap;
	int32 Found, Found2, OkToAdd, PinSwappable;
	char RefName[MAX_LENGTH_STRING], str[500], str1[500];
	LPSTR PP, PinStr, PinStrPos1, PinStrPos2;
	CompRecord *Comp;
	ObjectRecord *Object;

// *******************************************************************************************

	OkToAdd = 0;
	GateNr = 0;
	PartNr = 0;
	cnt2 = 0;
	CurrentGateNr = 0;
	CurrentPinGroupNr = 0;
	CurrentPinGroupLine = 0;
	CurrentPinGroupPos = 0;
	CurrentGateNr2 = 0;
	CurrentPinGroupNr2 = 0;
	CurrentPinGroupLine2 = 0;
	CurrentPinGroupPos2 = 0;
	FirstCntGateNr2 = 0;
	FirstCntGateNr = 0;
	CurrentCntGateNr = 0;
	GateNr2 = 0;
	PartNr2 = 0;
	Comp = NULL;

	if ((mode & 4) == 0)
	{
		strcpy(GatePinSwapReference, Reference);
		strcpy(GatePinSwapPinName, PinName);
	}
	else
	{
		if (GatePinSwapReference[0] == 0)
			return -1;
	}

	GateSpecialNr = 0;
	PinStrPos1 = SrcPins;
	PinStrPos2 = DestPins;

	if ((mode & 2) == 2)
	{
		PinStrPos1[0] = 0;
		PinStrPos2[0] = 0;
	}
	else
	{
		CurrentPartNr = 0;
		CurrentPartNr2 = 0;
	}

	LastCntGateNr = -1;
	Found = 0;
	RefName[0] = 0;

	for (cnt = 0; cnt < NrGatePinSwaps; cnt++)
	{
		GatePinSwap = &((*GatePinSwaps)[cnt]);

		if (GatePinSwap->ObjectType == 1)
		{
			strcpy(RefName, GetObjectText(GatePinSwap->Info2));
			GateNr = GatePinSwap->GateNr;
			PartNr = GatePinSwap->Info3;
			cnt2 = cnt;

			if ((Found) && (LastCntGateNr == -1))
				LastCntGateNr = cnt;
		}
		else
		{
			if ((!Found) && (stricmpOwn(RefName, GatePinSwapReference) == 0))
			{
				if (GatePinSwap->ObjectType == 2)
				{
					if (stricmpOwn(GetObjectText(GatePinSwap->Info2), GatePinSwapPinName) == 0)
					{
						if (!Found)
						{
							CurrentGateNr = GateNr;
							SwapInfo = GatePinSwap->SwapInfo;
							CurrentPinGroupNr = SwapInfo >> 12;
							CurrentPinGroupLine = (SwapInfo >> 8) & 0x0f;
							CurrentPinGroupPos = (SwapInfo >> 1) & 0x3f;
							CurrentPartNr = PartNr;
							Found = 1;
							FirstCntGateNr = cnt2;
							CurrentCntGateNr = cnt;
						}
					}
				}

				if (GatePinSwap->ObjectType == 3)
				{
					PP = GetObjectText(GatePinSwap->Info2);

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							if (stricmpOwn(str, GatePinSwapPinName) == 0)
							{
								if (!Found)
								{
									CurrentGateNr = GateNr;
									SwapInfo = GatePinSwap->SwapInfo;
									CurrentPinGroupNr = SwapInfo >> 12;
									CurrentPinGroupLine = (SwapInfo >> 8) & 0x0f;
									CurrentPinGroupPos = (SwapInfo >> 1) & 0x3f;
									Found = 1;
									CurrentPartNr = PartNr;
									FirstCntGateNr = cnt2;
									CurrentCntGateNr = cnt;
								}
							}
						}
					}
				}
			}
		}
	}

	if (!Found)
		return -1;

// *******************************************************************************************

	if ((mode & 2) == 2)
	{
		LastCntGateNr2 = -1;
		Found2 = 0;
		RefName[0] = 0;

		for (cnt = 0; cnt < NrGatePinSwaps; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);

			if (GatePinSwap->ObjectType == 1)
			{
				strcpy(RefName, GetObjectText(GatePinSwap->Info2));
				GateNr2 = GatePinSwap->GateNr;
				PartNr2 = GatePinSwap->Info3;
				cnt2 = cnt;

				if ((Found2) && (LastCntGateNr2 == -1))
					LastCntGateNr2 = cnt;
			}
			else
			{
				if ((!Found2) && (stricmpOwn(RefName, Reference2) == 0))
				{
					if (GatePinSwap->ObjectType == 2)
					{
						if (stricmpOwn(GetObjectText(GatePinSwap->Info2), PinName2) == 0)
						{
							if (!Found2)
							{
								CurrentGateNr2 = GateNr2;
								SwapInfo2 = GatePinSwap->SwapInfo;
								CurrentPinGroupNr2 = SwapInfo2 >> 12;
								CurrentPinGroupLine2 = (SwapInfo2 >> 8) & 0x0f;
								CurrentPinGroupPos2 = (SwapInfo2 >> 1) & 0x3f;
								CurrentPartNr2 = PartNr2;
								Found2 = 1;
								FirstCntGateNr2 = cnt2;
								CurrentCntGateNr2 = cnt;
							}
						}
					}

					if (GatePinSwap->ObjectType == 3)
					{
						PP = GetObjectText(GatePinSwap->Info2);

						for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
						{
							if ((res =
							            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
							                                 cnt4)) >= 0)
							{
								if (stricmpOwn(str, PinName2) == 0)
								{
									if (!Found2)
									{
										CurrentGateNr2 = GateNr2;
										SwapInfo2 = GatePinSwap->SwapInfo;
										CurrentPinGroupNr2 = SwapInfo2 >> 12;
										CurrentPinGroupLine2 = (SwapInfo2 >> 8) & 0x0f;
										CurrentPinGroupPos2 = (SwapInfo2 >> 1) & 0x3f;
										CurrentPartNr2 = PartNr2;
										Found2 = 1;
										FirstCntGateNr2 = cnt2;
										CurrentCntGateNr2 = cnt;
									}
								}
							}
						}
					}
				}
			}
		}

		if (!Found2)
			return -1;
	}

	GatePinSwap = &((*GatePinSwaps)[CurrentCntGateNr]);
//  if (GatePinSwap->SwapInfo==0) return -2;

// *******************************************************************************************
// *******************************************************************************************
// Swap pins inside a component
	if (CurrentGateNr == 0)
	{

		if ((mode & 2) == 2)
		{
			if ((CurrentGateNr != CurrentGateNr2) || (CurrentPinGroupNr != CurrentPinGroupNr2)
			        || (CurrentPinGroupLine == CurrentPinGroupLine2) || (CurrentPinGroupPos != CurrentPinGroupPos2))
				return -3;
		}
		else
		{
			memset(&GroupIndex, 0, sizeof(GroupIndex));
			cnt3 = 0;

			for (cnt = FirstCntGateNr; cnt < LastCntGateNr; cnt++)
			{
				GatePinSwap = &((*GatePinSwaps)[cnt]);
				SwapInfo = GatePinSwap->SwapInfo;

				if (GatePinSwap->ObjectType > 1)
				{
					PinGroupNr = SwapInfo >> 12;
					PinGroupLine = (SwapInfo >> 8) & 0x0f;
					PinGroupPos = (SwapInfo >> 1) & 0x3f;
					PinSwappable = SwapInfo & 1;

					if ((PinGroupNr > 0) && (CurrentPinGroupNr == PinGroupNr))
					{
						cnt3++;
						GroupIndex[PinGroupLine] = 1;
					}
				}
			}

			GroupIndex[CurrentPinGroupLine] = 2;

			if (cnt3 < 2)
				return -100;
		}

		if ((CompNr = FindCompNr(GatePinSwapReference, 0)) != -1)
		{
			Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
			NrObjects = 0;
			ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
		}
		else
			return -8;

// *******************************************************************************************

		if ((mode & 2) == 0)
		{
			if ((mode & 4) == 0)
				StartDrawingEditingWindow(0);

			SetROP2(OutputDisplay, R2_COPYPEN);

			for (cnt2 = 0; cnt2 < 16; cnt2++)
			{
				if (GroupIndex[cnt2] > 0)
				{
					for (cnt = FirstCntGateNr; cnt < LastCntGateNr; cnt++)
					{
						GatePinSwap = &((*GatePinSwaps)[cnt]);
						SwapInfo = GatePinSwap->SwapInfo;
						PinGroupNr = SwapInfo >> 12;
						PinGroupLine = (SwapInfo >> 8) & 0x0f;

						if ((GatePinSwap->ObjectType > 1) && (CurrentPinGroupNr == PinGroupNr)
						        && (PinGroupLine == cnt2))
						{
							if (GatePinSwap->ObjectType == 2)
							{
								PinStr = GetObjectText(GatePinSwap->Info2);

								if ((mode & 2) == 0)
								{
									if (GroupIndex[cnt2] == 1)
									{
										PinNr = CompPinNr(Comp, PinStr);

										for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
										{
											Object = &((*Objects)[cnt3]);

											if (Object->PinNr == PinNr)
											{
												SetROP2(OutputDisplay, R2_COPYPEN);
												InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
												DrawSpecialObject(Object, GateSpecialNr, 2);
												InitDrawingObject(0, SWAP_PINS_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);
												DrawSpecialObject(Object, 0, 1);
											}
										}
									}
									else
									{
										if (stricmpOwn(PinStr, GatePinSwapPinName) == 0)
										{
											PinNr = CompPinNr(Comp, PinStr);

											for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
											{
												Object = &((*Objects)[cnt3]);

												if (Object->PinNr == PinNr)
												{
													InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
													SetROP2(OutputDisplay, SelectColorMode);
													DrawObject(Object, 2);
												}
											}
										}
									}
								}
							}
							else
							{
								for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
								{
									if ((res =
									            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str,
									                                 GatePinSwap->Info3, cnt4)) >= 0)
									{
										if ((mode & 2) == 0)
										{
											if (GroupIndex[cnt2] == 1)
											{
												PinNr = CompPinNr(Comp, str);

												for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
												{
													Object = &((*Objects)[cnt3]);

													if (Object->PinNr == PinNr)
													{
														SetROP2(OutputDisplay, R2_COPYPEN);
														InitDrawingObject(0, SWAP_PINS_LAYER, 0,
														                  NORMAL_FILLED_AND_PEN1);
														DrawSpecialObject(Object, GateSpecialNr, 2);
														InitDrawingObject(0, SWAP_PINS_LAYER, 0,
														                  DRAW_WITH_PEN_AND_NOT_FILLED);
														DrawSpecialObject(Object, 0, 1);
													}
												}
											}
											else
											{
												if (stricmpOwn(str, GatePinSwapPinName) == 0)
												{
													PinNr = CompPinNr(Comp, str);

													for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
													{
														Object = &((*Objects)[cnt3]);

														if (Object->PinNr == PinNr)
														{
															InitDrawingObject(0, SWAP_PINS_LAYER, 0,
															                  NORMAL_FILLED_AND_PEN1);
															SetROP2(OutputDisplay, SelectColorMode);
															DrawObject(Object, 2);
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}

					GateSpecialNr++;
				}
			}

			if ((mode & 4) == 0)
			{
				ExitDrawing();
				EndDrawingEditingWindow(0);
			}

			return 0;
		}

// *******************************************************************************************

		for (cnt = FirstCntGateNr; cnt < LastCntGateNr; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);
			SwapInfo = GatePinSwap->SwapInfo;
			PinGroupNr = SwapInfo >> 12;
			PinGroupLine = (SwapInfo >> 8) & 0x0f;

			if ((GatePinSwap->ObjectType > 1) && (PinGroupNr > 0) && (CurrentPinGroupNr == PinGroupNr)
			        && (PinGroupLine == CurrentPinGroupLine))
			{
				if (GatePinSwap->ObjectType == 2)
				{
					PinStr = GetObjectText(GatePinSwap->Info2);
					sprintf(str, "PIN %s", PinStr);
					strcpy(PinStrPos1, str);
					PinStrPos1 += strlen(str) + 1;
				}
				else
				{
					sprintf(str1, "PINBUS 1 %i %s", GatePinSwap->Info3, GetObjectText(GatePinSwap->Info4));

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							strcat(str1, " ");
							strcat(str1, str);
						}
					}

					strcpy(PinStrPos1, str1);
					PinStrPos1 += strlen(str1) + 1;
				}
			}
		}

		for (cnt = FirstCntGateNr; cnt < LastCntGateNr; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);
			SwapInfo = GatePinSwap->SwapInfo;
			PinGroupNr = SwapInfo >> 12;
			PinGroupLine = (SwapInfo >> 8) & 0x0f;

			if ((GatePinSwap->ObjectType > 1) && (PinGroupNr > 0) && (CurrentPinGroupNr == PinGroupNr)
			        && (PinGroupLine == CurrentPinGroupLine2))
			{
				if (GatePinSwap->ObjectType == 2)
				{
					PinStr = GetObjectText(GatePinSwap->Info2);
					sprintf(str, "PIN %s", PinStr);
					strcpy(PinStrPos2, str);
					PinStrPos2 += strlen(str) + 1;
				}
				else
				{
					sprintf(str1, "PINBUS 1 %i %s", GatePinSwap->Info3, GetObjectText(GatePinSwap->Info4));

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							strcat(str1, " ");
							strcat(str1, str);
						}
					}

					strcpy(PinStrPos2, str1);
					PinStrPos2 += strlen(str1) + 1;
				}
			}
		}

		PinStrPos1[0] = 0;
		PinStrPos1++;
		PinStrPos2[0] = 0;
		PinStrPos2++;

		return 0;

	}

// *******************************************************************************************
// *******************************************************************************************
// Swap gate pins between components

	if ((mode & 2) == 0)
	{
		GateNrCount = 0;

		for (cnt = 0; cnt < NrGatePinSwaps; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);

			if ((GatePinSwap->GateNr == CurrentGateNr) && (GatePinSwap->ObjectType == 1))
				GateNrCount++;
		}

		if (GateNrCount < 2)
			return -3;

		GatePinSwap = &((*GatePinSwaps)[FirstCntGateNr]);

		GateSpecialNr = -1;
		NrObjects = 0;
		OkToAdd = 0;

		if ((mode & 4) == 0)
			StartDrawingEditingWindow(0);

		SetROP2(OutputDisplay, R2_COPYPEN);
		DisplayMode = 0;

		for (cnt = 0; cnt < NrGatePinSwaps; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);
			SwapInfo = GatePinSwap->SwapInfo;

			if (GatePinSwap->GateNr == CurrentGateNr)
			{
				switch (GatePinSwap->ObjectType)
				{
				case 1:
					OkToAdd = 1;
					Comp = NULL;

					if (cnt == FirstCntGateNr)
						DisplayMode = 1;
					else
						DisplayMode = 0;

					strcpy(RefName, GetObjectText(GatePinSwap->Info2));

					if ((CompNr = FindCompNr(RefName, 0)) != -1)
					{
						Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
						GateSpecialNr++;
						NrObjects = 0;
						ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
					}

					break;

				case 2:
					if ((OkToAdd) && (Comp != NULL))
					{
						PinGroupNr = SwapInfo >> 12;
						PinGroupLine = (SwapInfo >> 8) & 0x0f;
						PinGroupPos = (SwapInfo >> 1) & 0x3f;
						PinSwappable = SwapInfo & 1;

						if (DisplayMode == 0)
						{
							PinStr = GetObjectText(GatePinSwap->Info2);
							PinNr = CompPinNr(Comp, PinStr);

							for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
							{
								Object = &((*Objects)[cnt2]);

								if (Object->PinNr == PinNr)
								{
									SetROP2(OutputDisplay, R2_COPYPEN);
									InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
									DrawSpecialObject(Object, GateSpecialNr, 2);
									InitDrawingObject(0, SWAP_PINS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
									DrawSpecialObject(Object, 0, 1);
								}
							}
						}
						else
						{
							PinStr = GetObjectText(GatePinSwap->Info2);

							if (stricmpOwn(PinStr, GatePinSwapPinName) == 0)
							{
								PinNr = CompPinNr(Comp, PinStr);

								for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
								{
									Object = &((*Objects)[cnt2]);

									if (Object->PinNr == PinNr)
									{
										InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
										SetROP2(OutputDisplay, SelectColorMode);
										DrawObject(Object, 2);
									}
								}
							}
						}
					}

					break;

				case 3:
					if ((OkToAdd) && (Comp != NULL))
					{
						PinGroupNr = SwapInfo >> 12;
						PinGroupLine = (SwapInfo >> 8) & 0x0f;
						PinGroupPos = (SwapInfo >> 1) & 0x3f;
						PinSwappable = SwapInfo & 1;

						if (DisplayMode == 0)
						{
							for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
							{
								if ((res =
								            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
								                                 cnt4)) >= 0)
								{
									PinNr = CompPinNr(Comp, str);

									for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
									{
										Object = &((*Objects)[cnt3]);

										if (Object->PinNr == PinNr)
										{
											SetROP2(OutputDisplay, R2_COPYPEN);
											InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
											DrawSpecialObject(Object, GateSpecialNr, 2);
											InitDrawingObject(0, SWAP_PINS_LAYER, 0, DRAW_WITH_PEN_AND_NOT_FILLED);
											DrawSpecialObject(Object, 0, 1);
										}
									}
								}
							}
						}
						else
						{
							for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
							{
								if ((res =
								            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
								                                 cnt4)) >= 0)
								{
									if (stricmpOwn(str, GatePinSwapPinName) == 0)
									{
										PinNr = CompPinNr(Comp, str);

										for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
										{
											Object = &((*Objects)[cnt3]);

											if (Object->PinNr == PinNr)
											{
												InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
												SetROP2(OutputDisplay, SelectColorMode);
												DrawObject(Object, 2);
											}
										}
									}
								}
							}
						}
					}

					break;
				}
			}
		}

		if ((mode & 4) == 0)
		{
			ExitDrawing();
			EndDrawingEditingWindow(0);
		}

		return 0;
	}

// *******************************************************************************************

	if ((CurrentGateNr != CurrentGateNr2) || (FirstCntGateNr == FirstCntGateNr2))
		return -3;

	sprintf(str, "GATE %i", CurrentPartNr);
	strcpy(PinStrPos1, str);
	PinStrPos1 += strlen(str) + 1;
	sprintf(str, "GATE %i", CurrentPartNr2);
	strcpy(PinStrPos2, str);
	PinStrPos2 += strlen(str) + 1;

	for (cnt = 0; cnt < NrGatePinSwaps; cnt++)
	{
		GatePinSwap = &((*GatePinSwaps)[cnt]);
		SwapInfo = GatePinSwap->SwapInfo;

		if (GatePinSwap->GateNr == CurrentGateNr)
		{
			switch (GatePinSwap->ObjectType)
			{
			case 1:
				OkToAdd = 0;
				Comp = NULL;

				if (cnt == FirstCntGateNr)
				{
					OkToAdd = 1;
					strcpy(RefName, GetObjectText(GatePinSwap->Info2));

					if ((CompNr = FindCompNr(GatePinSwapReference, 0)) != -1)
					{
						Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
						NrObjects = 0;
						ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
					}
				}

				break;

			case 2:
				if ((OkToAdd) && (Comp != NULL))
				{
					PinGroupNr = SwapInfo >> 12;
					PinGroupLine = (SwapInfo >> 8) & 0x0f;
					PinGroupPos = (SwapInfo >> 1) & 0x3f;
					PinSwappable = SwapInfo & 1;
					PinStr = GetObjectText(GatePinSwap->Info2);
					sprintf(str, "PIN %s", PinStr);
					strcpy(PinStrPos1, str);
					PinStrPos1 += strlen(str) + 1;
				}

				break;

			case 3:
				if ((OkToAdd) && (Comp != NULL))
				{
					PinGroupNr = SwapInfo >> 12;
					PinGroupLine = (SwapInfo >> 8) & 0x0f;
					PinGroupPos = (SwapInfo >> 1) & 0x3f;
					PinSwappable = SwapInfo & 1;
					sprintf(str1, "PINBUS 1 %i %s", GatePinSwap->Info3, GetObjectText(GatePinSwap->Info4));

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							strcat(str1, " ");
							strcat(str1, str);
						}
					}

					strcpy(PinStrPos1, str1);
					PinStrPos1 += strlen(str1) + 1;
				}

				break;
			}
		}
	}

// *******************************************************************************************

	for (cnt = 0; cnt < NrGatePinSwaps; cnt++)
	{
		GatePinSwap = &((*GatePinSwaps)[cnt]);
		SwapInfo = GatePinSwap->SwapInfo;

		if (GatePinSwap->GateNr == CurrentGateNr)
		{
			switch (GatePinSwap->ObjectType)
			{
			case 1:
				OkToAdd = 0;
				Comp = NULL;

				if (cnt == FirstCntGateNr2)
				{
					OkToAdd = 1;
					strcpy(RefName, GetObjectText(GatePinSwap->Info2));

					if ((CompNr = FindCompNr(GatePinSwapReference, 0)) != -1)
					{
						Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
						NrObjects = 0;
						ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
					}
				}

				break;

			case 2:
				if ((OkToAdd) && (Comp != NULL))
				{
					PinGroupNr = SwapInfo >> 12;
					PinGroupLine = (SwapInfo >> 8) & 0x0f;
					PinGroupPos = (SwapInfo >> 1) & 0x3f;
					PinSwappable = SwapInfo & 1;
					PinStr = GetObjectText(GatePinSwap->Info2);
					sprintf(str, "PIN %s", PinStr);
					strcpy(PinStrPos2, str);
					PinStrPos2 += strlen(str) + 1;
				}

				break;

			case 3:
				if ((OkToAdd) && (Comp != NULL))
				{
					PinGroupNr = SwapInfo >> 12;
					PinGroupLine = (SwapInfo >> 8) & 0x0f;
					PinGroupPos = (SwapInfo >> 1) & 0x3f;
					PinSwappable = SwapInfo & 1;
					sprintf(str1, "PINBUS 1 %i %s", GatePinSwap->Info3, GetObjectText(GatePinSwap->Info4));

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							strcat(str1, " ");
							strcat(str1, str);
						}
					}

					strcpy(PinStrPos2, str1);
					PinStrPos2 += strlen(str1) + 1;
				}

				break;
			}
		}
	}

	/*
	  GatePinSwap=&((*GatePinSwaps)[FirstCntGateNr]);
	  GatePinSwap2=&((*GatePinSwaps)[FirstCntGateNr2]);
	  memmove(&NewGatePinSwap,GatePinSwap,sizeof(GatePinSwapRecord));
	  memmove(GatePinSwap,GatePinSwap2,sizeof(GatePinSwapRecord));
	  memmove(GatePinSwap2,&NewGatePinSwap,sizeof(GatePinSwapRecord));
	*/
	PinStrPos1[0] = 0;
	PinStrPos1++;
	PinStrPos2[0] = 0;
	PinStrPos2++;

	return 0;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 GetPinSwapInfo(LPSTR Reference, LPSTR PinName, LPSTR Reference2, LPSTR PinName2, LPSTR SrcPins, LPSTR DestPins,
                     int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, res, GateNr, PartNr, FirstCntGateNr, LastCntGateNr, CurrentCntGateNr,
	      CurrentCntGateNr2, PinNr, CompNr;
	GatePinSwapRecord *GatePinSwap;
	int32 SwapInfo, PinGroupNr, CurrentPinGroupNr, CurrentPinGroupNr2, CurrentPinGroupPos, PinGroupPos, PinGroupLine,
	      CurrentPinGroupLine, CurrentPinGroupPos2, CurrentPinGroupLine2;
	int32 Found, Found2, PinSwappable;
	char RefName[MAX_LENGTH_STRING], str[500], str1[500], str2[500];
	LPSTR PP, PinStr, PinStrPos1, PinStrPos2;
	CompRecord *Comp;
	ObjectRecord *Object;

	PinStrPos1 = SrcPins;
	PinStrPos2 = DestPins;
	FirstCntGateNr = 0;

	if ((mode & 2) == 2)
	{
		PinStrPos1[0] = 0;
		PinStrPos2[0] = 0;
	}

	if ((mode & 4) == 0)
	{
		if ((mode & 2) == 2)
		{
			if (stricmpOwn(Reference, Reference2) != 0)
				return -1;

			if (stricmpOwn(GatePinSwapPinName, PinName2) == 0)
				return -1;
		}

		strcpy(GatePinSwapReference, Reference);
		strcpy(GatePinSwapPinName, PinName);
	}
	else
	{
		if (GatePinSwapReference[0] == 0)
			return -1;
	}

	cnt = 0;
	cnt2 = 0;
	LastCntGateNr = -1;
	CurrentPinGroupNr = 0;
	CurrentPinGroupLine = 0;
	CurrentPinGroupPos = 0;
	LastCntGateNr = 0;
	CurrentCntGateNr = 0;
	CurrentPinGroupNr2 = 0;
	CurrentPinGroupLine2 = 0;
	CurrentPinGroupPos2 = 0;
	CurrentCntGateNr2 = 0;
	Found = 0;
	RefName[0] = 0;

	for (cnt = 0; cnt < NrGatePinSwaps; cnt++)
	{
		GatePinSwap = &((*GatePinSwaps)[cnt]);

		if (GatePinSwap->ObjectType == 1)
		{
			strcpy(RefName, GetObjectText(GatePinSwap->Info2));
			GateNr = GatePinSwap->GateNr;
			PartNr = GatePinSwap->Info3;
			cnt2 = cnt;

			if ((Found) && (LastCntGateNr == -1))
				LastCntGateNr = cnt;
		}
		else
		{
			if ((!Found) && (stricmpOwn(RefName, GatePinSwapReference) == 0))
			{
				if (GatePinSwap->ObjectType == 2)
				{
					if (stricmpOwn(GetObjectText(GatePinSwap->Info2), GatePinSwapPinName) == 0)
					{
						if (!Found)
						{
							SwapInfo = GatePinSwap->SwapInfo;
							CurrentPinGroupNr = SwapInfo >> 12;
							CurrentPinGroupLine = (SwapInfo >> 8) & 0x0f;
							CurrentPinGroupPos = (SwapInfo >> 1) & 0x3f;
							Found = 1;
							FirstCntGateNr = cnt2;
							CurrentCntGateNr = cnt;
						}
					}
				}

				if (GatePinSwap->ObjectType == 3)
				{
					PP = GetObjectText(GatePinSwap->Info2);

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							if (stricmpOwn(str, GatePinSwapPinName) == 0)
							{
								if (!Found)
								{
									SwapInfo = GatePinSwap->SwapInfo;
									CurrentPinGroupNr = SwapInfo >> 12;
									CurrentPinGroupLine = (SwapInfo >> 8) & 0x0f;
									CurrentPinGroupPos = (SwapInfo >> 1) & 0x3f;
									Found = 1;
									FirstCntGateNr = cnt2;
									CurrentCntGateNr = cnt;
								}
							}
						}
					}
				}
			}
		}
	}

	if (!Found)
		return -1;

	GatePinSwap = &((*GatePinSwaps)[CurrentCntGateNr]);

	if (GatePinSwap->SwapInfo == 0)
		return -2;

	if ((GatePinSwap->SwapInfo & 1) == 0)
		return -3;

// *******************************************************************************************

	Found2 = 0;

	if ((mode & 2) == 2)
	{
		for (cnt = FirstCntGateNr; cnt < LastCntGateNr; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);
			SwapInfo = GatePinSwap->SwapInfo;
			PinGroupNr = SwapInfo >> 12;
			PinGroupLine = (SwapInfo >> 8) & 0x0f;
			PinGroupPos = (SwapInfo >> 1) & 0x3f;
			PinSwappable = SwapInfo & 1;

			if (!Found2)
			{
				if (GatePinSwap->ObjectType == 2)
				{
					if (stricmpOwn(GetObjectText(GatePinSwap->Info2), PinName2) == 0)
					{
						if (!Found2)
						{
							SwapInfo = GatePinSwap->SwapInfo;
							CurrentPinGroupNr2 = SwapInfo >> 12;
							CurrentPinGroupLine2 = (SwapInfo >> 8) & 0x0f;
							CurrentPinGroupPos2 = (SwapInfo >> 1) & 0x3f;
							Found2 = 1;
							CurrentCntGateNr2 = cnt;
						}
					}
				}

				if (GatePinSwap->ObjectType == 3)
				{
					PP = GetObjectText(GatePinSwap->Info2);

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							if (stricmpOwn(str, PinName2) == 0)
							{
								if (!Found2)
								{
									SwapInfo = GatePinSwap->SwapInfo;
									CurrentPinGroupNr2 = SwapInfo >> 12;
									CurrentPinGroupLine2 = (SwapInfo >> 8) & 0x0f;
									CurrentPinGroupPos2 = (SwapInfo >> 1) & 0x3f;
									Found2 = 1;
									CurrentCntGateNr2 = cnt;
								}
							}
						}
					}
				}
			}
		}

		if (!Found2)
			return -1;

		GatePinSwap = &((*GatePinSwaps)[CurrentCntGateNr2]);

		if (GatePinSwap->SwapInfo == 0)
			return -2;

		if ((GatePinSwap->SwapInfo & 1) == 0)
			return -3;

		if ((CurrentPinGroupNr != CurrentPinGroupNr2) || (CurrentPinGroupLine != CurrentPinGroupLine2)
		        || (CurrentPinGroupPos != CurrentPinGroupPos2))
			return -4;

	}

// *******************************************************************************************
// *******************************************************************************************

	if ((CompNr = FindCompNr(GatePinSwapReference, 0)) != -1)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[CompNr]]);
		NrObjects = 0;
		ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
	}
	else
		return -8;

	if ((mode & 2) == 0)
	{
		if ((mode & 4) == 0)
			StartDrawingEditingWindow(0);

		SetROP2(OutputDisplay, R2_COPYPEN);

		for (cnt = FirstCntGateNr; cnt < LastCntGateNr; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);
			SwapInfo = GatePinSwap->SwapInfo;
			PinGroupNr = SwapInfo >> 12;
			PinGroupLine = (SwapInfo >> 8) & 0x0f;
			PinGroupPos = (SwapInfo >> 1) & 0x3f;
			PinSwappable = SwapInfo & 1;

			if ((GatePinSwap->ObjectType > 1) && (CurrentPinGroupNr == PinGroupNr)
			        && (PinGroupLine == CurrentPinGroupLine) && (PinGroupPos == CurrentPinGroupPos) && (PinSwappable))
			{
				if (GatePinSwap->ObjectType == 2)
				{
					PinStr = GetObjectText(GatePinSwap->Info2);

					if (stricmpOwn(PinStr, GatePinSwapPinName) != 0)
					{
						PinNr = CompPinNr(Comp, PinStr);

						for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
						{
							Object = &((*Objects)[cnt3]);

							if (Object->PinNr == PinNr)
							{
								SetROP2(OutputDisplay, R2_COPYPEN);
								InitDrawingObject(0, SWAP_GATE_LAYER, 0, NORMAL_FILLED_AND_PEN1);
								DrawSpecialObject(Object, 0, 0);
								InitDrawingObject(0, SWAP_GATE_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);
								DrawSpecialObject(Object, 0, 1);
							}
						}
					}
					else
					{
						PinNr = CompPinNr(Comp, PinStr);

						for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
						{
							Object = &((*Objects)[cnt3]);

							if (Object->PinNr == PinNr)
							{
								InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
								SetROP2(OutputDisplay, SelectColorMode);
								DrawObject(Object, 2);
							}
						}
					}
				}
				else
				{
					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							if (stricmpOwn(str, GatePinSwapPinName) != 0)
							{
								PinNr = CompPinNr(Comp, str);

								for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
								{
									Object = &((*Objects)[cnt3]);

									if (Object->PinNr == PinNr)
									{
										SetROP2(OutputDisplay, R2_COPYPEN);
										InitDrawingObject(0, SWAP_GATE_LAYER, 0, NORMAL_FILLED_AND_PEN1);
										DrawSpecialObject(Object, 0, 0);
										InitDrawingObject(0, SWAP_GATE_LAYER, 1, DRAW_WITH_PEN_AND_NOT_FILLED);
										DrawSpecialObject(Object, 0, 1);
									}
								}

							}
							else
							{
								PinNr = CompPinNr(Comp, str);

								for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
								{
									Object = &((*Objects)[cnt3]);

									if (Object->PinNr == PinNr)
									{
										InitDrawingObject(0, SWAP_PINS_LAYER, 0, NORMAL_FILLED_AND_PEN1);
										SetROP2(OutputDisplay, SelectColorMode);
										DrawObject(Object, 2);
									}
								}
							}
						}
					}
				}
			}
		}

		if ((mode & 4) == 0)
		{
			ExitDrawing();
			EndDrawingEditingWindow(0);
		}
	}
	else
	{
// *******************************************************************************************

		for (cnt = FirstCntGateNr; cnt < LastCntGateNr; cnt++)
		{
			GatePinSwap = &((*GatePinSwaps)[cnt]);
			SwapInfo = GatePinSwap->SwapInfo;
			PinGroupNr = SwapInfo >> 12;
			PinGroupLine = (SwapInfo >> 8) & 0x0f;
			PinGroupPos = (SwapInfo >> 1) & 0x3f;
			PinSwappable = SwapInfo & 1;

			if ((GatePinSwap->ObjectType > 1) && (CurrentPinGroupNr == PinGroupNr)
			        && (PinGroupLine == CurrentPinGroupLine) && (PinGroupPos == CurrentPinGroupPos) && (PinSwappable))
			{
				if (GatePinSwap->ObjectType == 2)
				{
					PinStr = GetObjectText(GatePinSwap->Info2);

					if (stricmpOwn(PinStr, GatePinSwapPinName) == 0)
					{
						sprintf(str, "PIN %s", PinStr);
						strcpy(PinStrPos1, str);
						PinStrPos1 += strlen(str) + 1;
					}

					if (stricmpOwn(PinStr, PinName2) == 0)
					{
						sprintf(str, "PIN %s", PinStr);
						strcpy(PinStrPos2, str);
						PinStrPos2 += strlen(str) + 1;
					}
				}
				else
				{
					sprintf(str1, "PINBUS 0 %i %s", GatePinSwap->Info3, GetObjectText(GatePinSwap->Info4));
					strcpy(str2, str1);

					for (cnt4 = 0; cnt4 < GatePinSwap->Info3; cnt4++)
					{
						if ((res =
						            GetPinNameFromPinBus(GetObjectText(GatePinSwap->Info2), str, GatePinSwap->Info3,
						                                 cnt4)) >= 0)
						{
							if (stricmpOwn(str, GatePinSwapPinName) == 0)
							{
								strcat(str1, " ");
								strcat(str1, str);
							}

							if (stricmpOwn(str, PinName2) == 0)
							{
								strcat(str2, " ");
								strcat(str2, str);
							}
						}
					}

					strcpy(PinStrPos1, str1);
					PinStrPos1 += strlen(str1) + 1;
					strcpy(PinStrPos2, str2);
					PinStrPos2 += strlen(str2) + 1;
				}
			}
		}

// *******************************************************************************************
		PinStrPos1[0] = 0;
		PinStrPos1++;
		PinStrPos2[0] = 0;
		PinStrPos2++;
	}

	return 0;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 GetCurrentPin(LPSTR Reference, LPSTR PinStr)
{
	int32 cnt, cnt2, CompInfo;
	double x1a, x2a, y1a, y2a, PosX, PosY;
	int32 Found = 0;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	ObjectRecord *Object, NewObject;
	CompRecord *Comp;
	NetRecord *Net;

	PosX = PixelToRealOffX(MousePosX);
	PosY = PixelToRealOffY(DrawWindowMaxY - MousePosY);

	for (cnt = 0; cnt < Design.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (CompsMem[(*Comps)[cnt]]);
		CompInfo = Comp->Info;

		if ((CompInfo & OBJECT_NOT_VISIBLE) == 0)
		{
			x1a = Comp->BoardPosMinX;
			y1a = Comp->BoardPosMinY;
			x2a = Comp->BoardPosMaxX;
			y2a = Comp->BoardPosMaxY;

			if ((PosX > x1a) && (PosX < x2a) && (PosY > y1a) && (PosY < y2a))
			{
				NrObjects = 0;
				ShapePinsToObject(Comp, 0.0, 0.0, 0, 0, 0, 0);
				cnt2 = 0;
				NewObject.x1 = PosX;
				NewObject.y1 = PosY;
				NewObject.x2 = 100;
				NewObject.ObjectType = PIN_SMD_ROUND;

				for (cnt2 = 0; cnt2 < NrObjects; cnt2++)
				{
					Object = &((*Objects)[cnt2]);
					FillPositionObject(Object);
					NewObject.Layer = Object->Layer;

					if ((!Found) && (ObjectsConnected(&NewObject, Object)))
					{
						if (CompPinText(Comp, Object->PinNr, PosX, PosY, str) == 0)
						{
							Found = 1;

							if ((Object->NetNr >= 0) && (Object->NetNr < Design.NrNets))
							{
								Net = &((*Nets)[Object->NetNr]);
								strcpy(str2, Net->Name);
								strcpy(Reference, Comp->Name);
								strcpy(PinStr, str);
								return 1;
							}
							else
							{
								if (Object->NetNr == -1)
								{
									strcpy(Reference, Comp->Name);
									strcpy(PinStr, str);
									return 0;
								}

								return -2;
							}
						}
					}
				}
			}
		}
	}

	return -1;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 AssignNetToCompPin2(int32 NetNr, CompRecord * Comp, LPSTR PinText)
{
	CompPinRecord *CompPin;
	ShapeRecord *Shape;
	int32 ObjectInclude;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	int32 cnt2, MemPos, NrPins, PinsMemPos, PinOffset, PolygonVertices, ShapeType, ShapePos, Mirror, Layer, MemPosComp,
	      NrPinShapes;

	PolygonVertices = 0;
	Mirror = ((Comp->CompMode & 8) >> 3);
	MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
	PinsMemPos = MemPosComp + sizeof(CompRecord);
	MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
	ShapePos = MemPos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

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

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 GetNetNrCompPin(CompRecord * Comp, LPSTR PinText)
{
	CompPinRecord *CompPin;
	ShapeRecord *Shape;
	int32 ObjectInclude;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
	int32 cnt2, MemPos, NrPins, PinsMemPos, PinOffset, ShapeType, Mirror, Layer, MemPosComp, NrPinShapes;

	Mirror = ((Comp->CompMode & 8) >> 3);
	MemPosComp = (uint8 *) Comp - &(CompsMem[0]);
	PinsMemPos = MemPosComp + sizeof(CompRecord);
	MemPos = (*Shapes)[Comp->ShapeNr].ShapePos;
	Shape = (ShapeRecord *) & (ShapesMem[MemPos]);

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
				return CompPin->NetNr;
		}

		PinsMemPos += sizeof(CompPinRecord);
		NrPins--;
	}

	return -2;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 SwapPinFunction(LPSTR Reference1, LPSTR PinName1, LPSTR Reference2, LPSTR PinName2, LPSTR PinStrPos1,
                      LPSTR PinStrPos2)
{
	char str[500], str1[500], str2[500], str3a[500], str4a[500], str4b[500], str5[500], str6a[500], str6b[500],
	     PinBusLabel1[MAX_LENGTH_STRING], PinBusLabel2[MAX_LENGTH_STRING];
	int32 cnt, le1, le2, NrPins, res, ok, CompNr1, CompNr2, NetNr1, NetNr2, PinBusMode, SwapMode, PartNr1, PartNr2,
	      NrPins2;
	CompRecord *Comp1, *Comp2;
	uint8 *SelectedNets;
	int fp;
	struct tm *today;
	time_t ltime;
#ifdef _DEBUG
	LPSTR Pos1, Pos2;
#endif

	AllocateSpecialMem(MEM_NET_SELECTED, (Design.NrNets + 10) * sizeof(uint8), (void **) &SelectedNets);

	memset(SelectedNets, 0, Design.NrNets);
	sprintf(str, "%s\\pcb\\gatepin.ban", DesignPath);

	if (FileExistsUTF8(str) == 0)
	{
		res = FileSizeUTF8(str);

		if ((fp = FileOpenUTF8(str)) <= 0)
		{
			DeallocateSpecialMem(MEM_NET_SELECTED);
			return -1;
		}

		FileSeek(fp, res);
	}
	else
	{
		if ((fp = FileOpenWriteUTF8(str)) <= 0)
		{
			DeallocateSpecialMem(MEM_NET_SELECTED);
			return -1;
		}

		WriteLn(fp, SC(392, "Gate/pin swap file PCB elegance"));

	}

	time(&ltime);
	today = localtime(&ltime);
	strftime(str2, 100, "%B %d, %Y %X", today);
	sprintf(str, SC(393, "Changes made on %s"), str2);
	WriteLn(fp, str);

	CompNr1 = FindCompNr(Reference1, 0);
	CompNr2 = FindCompNr(Reference2, 0);

	if ((CompNr1 < 0) || (CompNr2 < 0))
		return -1;

	Comp1 = (CompRecord *) & (CompsMem[(*Comps)[CompNr1]]);
	Comp2 = (CompRecord *) & (CompsMem[(*Comps)[CompNr2]]);

#ifdef _DEBUG
	Pos1 = PinStrPos1;
	Pos2 = PinStrPos2;

	while (Pos1[0] != 0)
	{
		sprintf(str, "%s\n", Pos1);
		OutputDebugString(str);
		Pos1 += strlen(Pos1) + 1;
	}

	while (Pos2[0] != 0)
	{
		sprintf(str, "%s\n", Pos2);
		OutputDebugString(str);
		Pos2 += strlen(Pos2) + 1;
	}

	sprintf(str, "\n");
	OutputDebugString(str);
#endif
	SwapMode = 0;

	while (PinStrPos1[0] != 0)
	{
		le1 = strlen(PinStrPos1);
		le2 = strlen(PinStrPos2);
		strcpy(str1, PinStrPos1);
		strcpy(str2, PinStrPos2);
		GetString2(str1, str3a, 500);
		GetString2(str2, str4a, 500);

		if (stricmpOwn(str3a, "GATE") == 0)
		{
			SwapMode = 1;
			GetString2(str1, str5, 500);

			if ((res = sscanf(str5, "%i", &PartNr1)) != 1)
			{
				DeallocateSpecialMem(MEM_NET_SELECTED);
				return -5;
			}

			GetString2(str2, str5, 500);

			if ((res = sscanf(str5, "%i", &PartNr2)) != 1)
			{
				DeallocateSpecialMem(MEM_NET_SELECTED);
				return -5;
			}

			sprintf(str, "SWAP GATE %s %i WITH %s %i", Reference1, PartNr1, Reference2, PartNr2);
			WriteLn(fp, str);
#ifdef _DEBUG
			strcat(str, "\n");
			OutputDebugString(str);
#endif
		}

		if (stricmpOwn(str3a, "PIN") == 0)
		{
			GetString2(str1, str6a, 500);
			NetNr1 = GetNetNrCompPin(Comp1, str6a);

			if ((NetNr1 >= 0) && (NetNr1 < Design.NrNets))
				SelectedNets[NetNr1] = 1;

			GetString2(str2, str6b, 500);
			NetNr2 = GetNetNrCompPin(Comp2, str6b);

			if ((NetNr2 >= 0) && (NetNr2 < Design.NrNets))
				SelectedNets[NetNr2] = 1;

			if (((NetNr1 >= 0) && (NetNr1 < Design.NrNets)) || ((NetNr2 >= 0) && (NetNr2 < Design.NrNets)))
			{
				if (SwapMode == 0)
				{
					sprintf(str, "SWAP PIN %s %s WITH %s %s", Reference1, str6a, Reference2, str6b);
					WriteLn(fp, str);
#ifdef _DEBUG
					strcat(str, "\n");
					OutputDebugString(str);
#endif
				}

				res = AssignNetToCompPin2(NetNr1, Comp2, str6b);
				res = AssignNetToCompPin2(NetNr2, Comp1, str6a);
				/*
				        NrObjects=0;
				        ShapePinsToObject(Comp1,0.0,0.0,0,0,0);
				        for (cnt=0;cnt<100;cnt++) {
				          Object=&((*Objects)[cnt]);
				          ObjectPos[cnt]=Object;
				        }
				*/
				ok = 1;
			}
		}

		if (stricmpOwn(str3a, "PINBUS") == 0)
		{
			GetString2(str1, str5, 500);

			if (stricmpOwn(str5, "0") == 0)
				PinBusMode = 0;
			else
			{
				if (stricmpOwn(str5, "1") == 0)
					PinBusMode = 1;
				else
				{
					DeallocateSpecialMem(MEM_NET_SELECTED);
					return -5;
				}
			}

			GetString2(str1, str5, 500);

			if ((res = sscanf(str5, "%i", &NrPins)) != 1)
				return -5;

			NrPins2 = NrPins;
			GetString2(str1, str5, 500);
			strcpy(PinBusLabel1, str5);
			GetString2(str2, str5, 500);
			GetString2(str2, str5, 500);
			GetString2(str2, str5, 500);
			strcpy(PinBusLabel2, str5);
			strcpy(str4a, str1);
			strcpy(str4b, str2);

			if (PinBusMode == 0)
				NrPins = 1;
			else
			{
				if (SwapMode == 0)
				{
					sprintf(str, "SWAP PINBUS1 %s %i PINS %s WITH %s", Reference1, NrPins2, PinBusLabel1, PinBusLabel2);
					WriteLn(fp, str);
#ifdef _DEBUG
					strcat(str, "\n");
					OutputDebugString(str);
#endif
				}
			}

			for (cnt = 0; cnt < NrPins; cnt++)
			{
				GetString2(str4a, str6a, 500);
				NetNr1 = -1;

				if (str6a[0] != 0)
				{
					NetNr1 = GetNetNrCompPin(Comp1, str6a);

					if ((NetNr1 >= 0) && (NetNr1 < Design.NrNets))
						SelectedNets[NetNr1] = 1;
				}

				GetString2(str4b, str6b, 500);
				NetNr2 = -1;

				if (str6b[0] != 0)
				{
					NetNr2 = GetNetNrCompPin(Comp2, str6b);

					if ((NetNr2 >= 0) && (NetNr2 < Design.NrNets))
						SelectedNets[NetNr2] = 1;
				}

				if (((NetNr1 >= 0) && (NetNr1 < Design.NrNets)) || ((NetNr2 >= 0) && (NetNr2 < Design.NrNets)))
				{
					if ((PinBusMode == 0) && (SwapMode == 0))
					{
						sprintf(str, "SWAP PINBUS0 %s %i %s PIN %s WITH %s", Reference1, NrPins2, PinBusLabel1, str6a,
						        str6b);
						WriteLn(fp, str);
#ifdef _DEBUG
						strcat(str, "\n");
						OutputDebugString(str);
#endif
					}

					res = AssignNetToCompPin2(NetNr1, Comp2, str6b);
					res = AssignNetToCompPin2(NetNr2, Comp1, str6a);
				}
				else
					res = 1;
			}
		}

		PinStrPos1 += strlen(PinStrPos1) + 1;
		PinStrPos2 += strlen(PinStrPos2) + 1;
	}

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (SelectedNets[cnt] == 1)
		{
			ReCalcConnectionsNet((int32) cnt, 0, 1);
			CheckForEscape();
			DataBaseChanged = 1;
		}
	}

	FileClose(fp);
	RePaint();
	return 0;
}

// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************

int32 SelectGatePinSwap(int32 mode)
{
	char ReferenceStr[MAX_LENGTH_STRING], PinStr[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	int32 res, res2, ok;
	uint8 *GatePinSwapBuf;
	int32 OkToSwap = 0;

	if (mode == 1)
	{
		GatePinSwapMode = 0;
		return 0;
	}

	AllocateSpecialMem(MEM_POINTS, 256 * 1024, (void **) &GatePinSwapBuf);

	res = GetCurrentPin(ReferenceStr, PinStr);

	if (res >= 0)
	{
		if (GatePinSwapMode)
		{
			res = -1;

			if (GateSwapResult >= 0)
			{
				res =
				    GetGateSwapInfo(GatePinSwapReference, GatePinSwapPinName, ReferenceStr, PinStr,
				                    (LPSTR) GatePinSwapBuf, (LPSTR) & GatePinSwapBuf[131072], 2);

				if (res == 0)
				{
					OkToSwap = 1;
					GatePinSwapMode = 0;
				}
			}

			if ((PinSwapResult >= 0) && (res != 0))
			{
				res2 =
				    GetPinSwapInfo(GatePinSwapReference, GatePinSwapPinName, ReferenceStr, PinStr,
				                   (LPSTR) GatePinSwapBuf, (LPSTR) & GatePinSwapBuf[131072], 2);

				if (res2 == 0)
				{
					OkToSwap = 1;
					GatePinSwapMode = 0;
				}
			}

			ok = 1;

			if (OkToSwap)
			{
				SwapPinFunction(GatePinSwapReference, GatePinSwapPinName, ReferenceStr, PinStr, (LPSTR) GatePinSwapBuf,
				                (LPSTR) & GatePinSwapBuf[131072]);
			}

			if (!GatePinSwapMode)
			{
				OkToSwap = 0;
				GatePinSwapReference[0] = 0;
				GatePinSwapPinName[0] = 0;
				PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_VIEW_REPAINT, (LPARAM) NULL);
				CheckInputMessages(0);
				SetInfoStr(0);
			}
		}
		else
		{
			if (res == 1)
			{
				GateSwapResult = GetGateSwapInfo(ReferenceStr, PinStr, 0, 0, 0, 0, 0);
				PinSwapResult = GetPinSwapInfo(ReferenceStr, PinStr, 0, 0, 0, 0, 0);

				if ((GateSwapResult >= 0) || (PinSwapResult >= 0))
				{
					GatePinSwapMode = 1;
					sprintf(str, SC(394, "Gate/pin swap  [ Component %s-%s ]"), GatePinSwapReference,
					        GatePinSwapPinName);

					if (CurrentPartNr > 0)
					{
						sprintf(str2, SC(395, " ( Part nr %i )"), CurrentPartNr);
						strcat(str, str2);
					}

					strcpy(InfoStr, str);
					RedrawInfoStr(1);
				}
				else
				{
					strcpy(InfoStr, SC(396, "Gate/pin swap (No swappable pin found)"));
					RedrawInfoStr(1);
					Beep(1000, 200);
				}
			}
			else
			{
				strcpy(InfoStr, SC(397, "Gate/pin swap (Pin not connected)"));
				RedrawInfoStr(1);
				Beep(1000, 200);
			}
		}
	}

	return 0;
}


// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
// *******************************************************************************************
