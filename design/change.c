/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: change.c
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


#include "stdio.h"
#include "types.h"
#include "stddef.h"
#include "calc.h"
#include "memory.h"
#include "design.h"
#include "change.h"
#include "files.h"
#include "files2.h"
#include "resource.h"
#include "utf8.h"
#include "help.h"

#define MaxGatesPinSwapInfos      512

typedef struct
{
	int16 SwapMode, PartNr1, PartNr2, Swapped1, Swapped2, NrPinsPinBus;
	char Ref1[8], Ref2[8], PinStr1[12], PinStr2[12], PinBusLabel1[32], PinBusLabel2[32], NewGeometry[32];
} GateSwapRecord;

typedef GateSwapRecord GateSwapArray[MaxGatesPinSwapInfos];


int32 ChangedSymbolMode;
char StringToChange[MAX_LENGTH_STRING], NewSymbolName[MAX_LENGTH_STRING], NewPartNr[MAX_LENGTH_STRING],
     NewValue[MAX_LENGTH_STRING], NewGeometry[MAX_LENGTH_STRING], NewDescription[MAX_LENGTH_STRING],
     NewPropertyID[MAX_LENGTH_STRING], NewPropertyValue[MAX_LENGTH_STRING];

//GateSwapArray *GateSwapInfos;
//GateSwapRecord *GateSwapInfo;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BackAnnotate(int32 mode)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING],
	     str5[MAX_LENGTH_STRING], NewGeometry[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING], str7[MAX_LENGTH_STRING],
	     str8[MAX_LENGTH_STRING], str9[MAX_LENGTH_STRING], LineBuf[MAX_LENGTH_STRING], Ref1[MAX_LENGTH_STRING],
	     Ref2[MAX_LENGTH_STRING], PinStr1[MAX_LENGTH_STRING], PinStr2[MAX_LENGTH_STRING],
	     PinBusLabel1[MAX_LENGTH_STRING], PinBusLabel2[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING],
	     NewPinBusStr[500], SwapFileName[MAX_LENGTH_STRING];
	int32 Found1, Found2, FoundCnt, MaxMem, res, result, Designfp, cnt, cnt2, cnt3, cnt4, cnt5, cnt6, MemPos, MemPos2,
	      fp, Length, PartNr1, PartNr2, SwapMode, NrGatePinSwaps, NrPinsPinBus, MemSize, SymbolNr, InstMemSize, Pos1,
	      Pos2, LineNr;

	double FoundX1, FoundY1, FoundX2, FoundY2;
	SymbolsPos2Record *SymbolPos2;
	SymbolRecord *Symbol;
	DesignRecord OldDesign;
	int32 FileChanged;
	InstanceRecord *Instance, *FirstChangedInstance;
	ObjectRecord *Object;
	GateSwapArray *GateSwapInfos;
	GateSwapRecord *GateSwapInfo, *GateSwapInfoPos[32];
	WireRecord *Wire;
	BusRecord *Bus;
	BusConnectionRecord *BusConnection;
	GlobalConnectionRecord *GlobalConnection;
	JunctionRecord *Junction;
	NetLabelRecord *NetLabel;
	RedefinedPinBusRecord *RedefinedPinBus;

// **********************************************************************************
// **********************************************************************************

	PartNr1 = 0;
	PartNr2 = 0;
	Pos1 = 0;
	Pos2 = 0;
	FoundX1 = 0.0;
	FoundY1 = 0.0;
	FoundX2 = 0.0;
	FoundY2 = 0.0;
	NrPinsPinBus = 0;
	sprintf(SwapFileName, "%s\\pcb\\gatepin.ban", DesignPath);

	if (FileExistsUTF8(SwapFileName) < 0)
	{
		strcpy(str, SC(124, "Back annotation file does not exist\r\n"));
		AddMessage(str);
//    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)SC(124,"Back annotation file does not exist\r\n"));
		return -1;
	}

	GetDesignSheets();
	MaxMem = 0;

	for (cnt = 0; cnt < NrSheets; cnt++)
		MaxMem = max(MaxMem, Sheets[cnt].SheetMemSize + Sheets[cnt].SymbolMemSize);

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		strcpy(FileName, SheetDir);
		strcat(FileName, Sheets[cnt2].SheetName);
		strcat(FileName, ".sch");
		MemPos = 0;

		if ((Designfp = FileOpenReadOnlyUTF8(FileName)) == -1)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, SC(17, "Error in opening file"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, SC(125, "Error in reading file"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		FileClose(Designfp);
	}

	InstMemSize = ((MaxMem + 65536) & ~7) + MaxGatesPinSwapInfos * sizeof(GateSwapRecord);

	if ((InstMemSize > DesignBufMemSize) && (AllocateMemDesignBuf(InstMemSize) != 0))
		return -1;

	GateSwapInfos = (GateSwapArray *) & DesignBuf[(MaxMem + 65536) & ~7];
	memset(GateSwapInfos, 0, MaxGatesPinSwapInfos * sizeof(GateSwapRecord));
// **********************************************************************************

	if ((fp = TextFileOpenUTF8(SwapFileName)) < 0)
		return -1;

	for (cnt = 0; cnt < 10; cnt++)
	{
		GateSwapInfo = (GateSwapRecord *) & ((*GateSwapInfos)[cnt]);
		GateSwapInfoPos[cnt] = GateSwapInfo;
	}

	NrGatePinSwaps = 0;

	while ((Length = ReadLnWithMaxLength(fp, LineBuf, MAX_LENGTH_STRING - 50)) >= 0)
	{
		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString(LineBuf, str);
			GetString(LineBuf, str2);
			GetString(LineBuf, str3);
			GetString(LineBuf, str4);
			GetString(LineBuf, str5);
			GetString(LineBuf, str6);
			GetString(LineBuf, str7);
			GetString(LineBuf, str8);
			GetString(LineBuf, str9);
			SwapMode = -1;
			strcpy(Ref1, str3);
			PinStr1[0] = 0;
			PinStr2[0] = 0;
			PinBusLabel1[0] = 0;
			PinBusLabel2[0] = 0;
			Ref2[0] = 0;

//      sprintf(str,"SWAP GATE %s %i WITH %s %i",Reference1,PartNr1,Reference2,PartNr2);
//      sprintf(str,"SWAP PIN %s %s WITH %s %s",Reference1,str6a,Reference2,str6b);
//      sprintf(str,"SWAP PINBUS1 %s %i PINS %s WITH %s",Reference1,NrPins,PinBusLabel1,PinBusLabel2);
//      sprintf(str,"SWAP PINBUS0 %s %i %s PIN %s WITH %s",Reference1,NrPins,PinBusLabel1,str6a,str6b);
//      sprintf(str,"CHANGE GEOM U100 WITH %s"
			if (stricmp(str, "SWAP") == 0)
			{
				if (stricmp(str2, "GATE") == 0)
				{
					SwapMode = 0;
					strcpy(Ref2, str6);
					sscanf(str4, "%i", &PartNr1);
					sscanf(str7, "%i", &PartNr2);
				}
				else
				{
					if (stricmp(str2, "PIN") == 0)
					{
						SwapMode = 1;
						strcpy(PinStr1, str4);
						strcpy(PinStr2, str7);
					}
					else
					{
						if (stricmp(str2, "PINBUS1") == 0)
						{
							SwapMode = 2;
							sscanf(str4, "%i", &NrPinsPinBus);
							strcpy(PinBusLabel1, str6);
							strcpy(PinBusLabel2, str8);
						}
						else
						{
							if (stricmp(str2, "PINBUS0") == 0)
							{
								SwapMode = 3;
								sscanf(str4, "%i", &NrPinsPinBus);
								strcpy(PinBusLabel1, str5);
								strcpy(PinStr1, str7);
								strcpy(PinStr2, str9);
							}
						}
					}
				}
			}
			else
			{
				if (stricmp(str, "CHANGE") == 0)
				{
					if (stricmp(str2, "GEOM") == 0)
					{
						SwapMode = 4;
						strcpy(NewGeometry, str5);
					}
				}
			}

			if ((SwapMode != -1) && (NrGatePinSwaps < MaxGatesPinSwapInfos - 1))
			{
				GateSwapInfo = (GateSwapRecord *) & ((*GateSwapInfos)[NrGatePinSwaps]);
				GateSwapInfo->PartNr1 = (int16) PartNr1;
				GateSwapInfo->PartNr2 = (int16) PartNr2;
				strcpy(GateSwapInfo->Ref1, Ref1);
				strcpy(GateSwapInfo->Ref2, Ref2);
				strcpy(GateSwapInfo->PinStr1, PinStr1);
				strcpy(GateSwapInfo->PinStr2, PinStr2);
				strcpy(GateSwapInfo->PinBusLabel1, PinBusLabel1);
				strcpy(GateSwapInfo->PinBusLabel2, PinBusLabel2);
				strcpy(GateSwapInfo->NewGeometry, NewGeometry);
				GateSwapInfo->SwapMode = (int16) SwapMode;
				GateSwapInfo->Swapped1 = 0;
				GateSwapInfo->Swapped2 = 0;
				GateSwapInfo->NrPinsPinBus = (int16) NrPinsPinBus;
				NrGatePinSwaps++;
			}
		}
	}

	TextFileClose(fp);
	DeleteFileUTF8(SwapFileName);
//  GateSwapInfoPos;

// **********************************************************************************
// **********************************************************************************
	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		FileChanged = 0;
		memset(DesignBuf, 0, MaxMem + 65536 - 8);

		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);

		if ((res = LoadSheetInMemory(cnt2, 0)) < 0)
			return -2;

		memmove(&OldDesign, &Design, sizeof(DesignRecord));
// **********************************************************************************
// **********************************************************************************


		for (cnt5 = 0; cnt5 < NrGatePinSwaps; cnt5++)
		{
			GateSwapInfo = (GateSwapRecord *) & ((*GateSwapInfos)[cnt5]);
			Found1 = 0;
			Found2 = 0;
			FirstChangedInstance = 0;

			switch (GateSwapInfo->SwapMode)
			{
			case 0:
				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt]);

					if ((stricmpUTF8(GateSwapInfo->Ref1, Instance->Reference) == 0) && (GateSwapInfo->Swapped1 == 0))
					{
						SymbolNr = -1;

						for (cnt3 = 0; cnt3 < Design.NrSymbols; cnt3++)
						{
							SymbolPos2 = &((*SymbolsPos2)[cnt3]);

							if (stricmpUTF8(SymbolPos2->SymbolName, Instance->SymbolName) == 0)
								SymbolNr = cnt3;
						}

						if (SymbolNr != -1)
						{
							SymbolPos2 = &((*SymbolsPos2)[SymbolNr]);
							MemPos = (*SymbolsPos2)[SymbolNr].Pos;
							Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);

							if (((Symbol->Info & SHEET_SYMBOL) == 0)
							        && (Instance->PackagePartNr == GateSwapInfo->PartNr1))
							{
								memset(&Instance->Reference, 0, sizeof(Instance->Reference));
								strcpy(Instance->Reference, GateSwapInfo->Ref2);
								Instance->PackagePartNr = GateSwapInfo->PartNr2;
								FileChanged = 1;
								GateSwapInfo->Swapped1 = 1;
								FirstChangedInstance = Instance;
							}
						}
					}
				}

				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt]);

					if ((stricmpUTF8(GateSwapInfo->Ref2, Instance->Reference) == 0) && (GateSwapInfo->Swapped2 == 0))
					{
						SymbolNr = Instance->AddNr;

						if (SymbolNr != -1)
						{
							SymbolPos2 = &((*SymbolsPos2)[SymbolNr]);
							MemPos = (*SymbolsPos2)[SymbolNr].Pos;
							Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);

							if (((Symbol->Info & SHEET_SYMBOL) == 0)
							        && (Instance->PackagePartNr == GateSwapInfo->PartNr2))
							{
								if (Instance != FirstChangedInstance)
								{
									memset(&Instance->Reference, 0, sizeof(Instance->Reference));
									strcpy(Instance->Reference, GateSwapInfo->Ref1);
									Instance->PackagePartNr = GateSwapInfo->PartNr1;
									FileChanged = 1;
									GateSwapInfo->Swapped2 = 1;
								}
							}
						}
					}
				}

				break;

// **********************************************************************************
// **********************************************************************************
			case 1:
			case 2:
			case 3:
				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt]);

					if (stricmpUTF8(GateSwapInfo->Ref1, Instance->Reference) == 0)
					{
						SymbolNr = Instance->AddNr;

						if (SymbolNr != -1)
						{
							SymbolPos2 = &((*SymbolsPos2)[SymbolNr]);
							MemPos = (*SymbolsPos2)[SymbolNr].Pos;
							Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);

							if ((Symbol->Info & SHEET_SYMBOL) == 0)
							{
								NrObjects = 0;
								InstancePinsToObject(Instance, 0);

								for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
								{
									Object = &((*Objects)[cnt4]);

									switch (GateSwapInfo->SwapMode)
									{
									case 1:
										if (Object->ObjectType == SYMBOL_PIN)
										{
											if (GateSwapInfo->Swapped1 == 0)
											{
												if (stricmpUTF8(Object->Text1, GateSwapInfo->PinStr1) == 0)
												{
													FoundX1 = Object->x1;
													FoundY1 = Object->y1;
													GateSwapInfo->Swapped1 = 1;
													Found1 = 1;
												}
											}

											if (GateSwapInfo->Swapped2 == 0)
											{
												if (stricmpUTF8(Object->Text1, GateSwapInfo->PinStr2) == 0)
												{
													FoundX2 = Object->x1;
													FoundY2 = Object->y1;
													GateSwapInfo->Swapped2 = 1;
													Found2 = 1;
												}
											}
										}

										break;

									case 2:
										if (Object->ObjectType == SYMBOL_PINBUS)
										{
											if (GateSwapInfo->Swapped1 == 0)
											{
												if (stricmpUTF8(Object->Text2, GateSwapInfo->PinBusLabel1) == 0)
												{
													FoundX1 = Object->x1;
													FoundY1 = Object->y1;
													GateSwapInfo->Swapped1 = 1;
													Found1 = 1;
												}
											}

											if (GateSwapInfo->Swapped2 == 0)
											{
												if (stricmpUTF8(Object->Text2, GateSwapInfo->PinBusLabel2) == 0)
												{
													FoundX2 = Object->x1;
													FoundY2 = Object->y1;
													GateSwapInfo->Swapped2 = 1;
													Found2 = 1;
												}
											}
										}

										break;

									case 3:
										if ((GateSwapInfo->Swapped1 == 0) && (Object->ObjectType == SYMBOL_PINBUS)
										        && (Object->Info3 == GateSwapInfo->NrPinsPinBus)
										        && (stricmpUTF8(Object->Text2, GateSwapInfo->PinBusLabel1) == 0))
										{
											for (cnt6 = 0; cnt6 < GateSwapInfo->NrPinsPinBus; cnt6++)
											{
												res =
												    GetPinNameFromPinBus(Object->Text1, str2,
												                         GateSwapInfo->NrPinsPinBus, cnt6);

												if (res >= 0)
												{
													if (stricmpUTF8(str2, GateSwapInfo->PinStr1) == 0)
													{
														Found1 = 1;
														Pos1 = cnt6;
													}

													if (stricmpUTF8(str2, GateSwapInfo->PinStr2) == 0)
													{
														Found2 = 1;
														Pos2 = cnt6;
													}
												}
											}

											if ((Found1) && (Found2))
											{
												FoundCnt = -1;

												for (cnt6 = 0; cnt6 < Design.NrRedefinedPinBusses; cnt6++)
												{
													RedefinedPinBus = &((*RedefinedPinBusses)[cnt6]);

													if ((stricmpUTF8(GateSwapInfo->Ref1, RedefinedPinBus->Reference) ==
													        0)
													        &&
													        (stricmpUTF8(GateSwapInfo->PinBusLabel1, RedefinedPinBus->Name)
													         == 0))
														FoundCnt = cnt6;
												}

												FileChanged = 1;

												if (FoundCnt != -1)
												{
													RedefinedPinBus = &((*RedefinedPinBusses)[FoundCnt]);
													NewPinBusStr[0] = 0;

													for (cnt6 = 0; cnt6 < Object->Info3; cnt6++)
													{
														res =
														    GetPinNameFromPinBus(Object->Text1, str2, Object->Info3,
														                         RedefinedPinBus->Order[cnt6]);

														if (cnt6 > 0)
															strcat(NewPinBusStr, ",");

														strcat(NewPinBusStr, str2);
														LineNr = res;
													}

													for (cnt6 = 0; cnt6 < Object->Info3; cnt6++)
													{
														res =
														    GetPinNameFromPinBus(NewPinBusStr, str2, Object->Info3,
														                         cnt6);

														if (res >= 0)
														{
															if (stricmpUTF8(str2, GateSwapInfo->PinStr1) == 0)
															{
																Found1 = 1;
																Pos1 = cnt6;
															}

															if (stricmpUTF8(str2, GateSwapInfo->PinStr2) == 0)
															{
																Found2 = 1;
																Pos2 = cnt6;
															}
														}
													}

													if ((Found1) && (Found2))
													{
														res = RedefinedPinBus->Order[Pos1];
														RedefinedPinBus->Order[Pos1] = RedefinedPinBus->Order[Pos2];
														RedefinedPinBus->Order[Pos2] = (uint8) res;
													}
												}
												else
												{
													RedefinedPinBus =
													    &((*RedefinedPinBusses)[Design.NrRedefinedPinBusses]);
													memset(RedefinedPinBus, 0, sizeof(RedefinedPinBusRecord));
													strcpy(RedefinedPinBus->Reference, GateSwapInfo->Ref1);
													strcpy(RedefinedPinBus->Name, GateSwapInfo->PinBusLabel1);
													RedefinedPinBus->Info2 = GateSwapInfo->NrPinsPinBus;

													for (cnt6 = 0; cnt6 < GateSwapInfo->NrPinsPinBus; cnt6++)
														RedefinedPinBus->Order[cnt6] = (uint8) cnt6;

													RedefinedPinBus->Order[Pos1] = (uint8) Pos2;
													RedefinedPinBus->Order[Pos2] = (uint8) Pos1;
													Design.NrRedefinedPinBusses++;
												}
											}
										}

										break;
									}
								}
							}
						}
					}
				}

				break;

// **********************************************************************************
// **********************************************************************************
			case 4:			// Change geometry
				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					Instance = (InstanceRecord *) & ((*Instances)[cnt]);

					if (stricmpUTF8(GateSwapInfo->Ref1, Instance->Reference) == 0)
					{
						memset(&Instance->Geometry, 0, sizeof(Instance->Geometry));
						strncpy(Instance->Geometry, GateSwapInfo->NewGeometry, sizeof(Instance->Geometry) - 1);
						FileChanged = 1;
					}
				}

				break;
			}

// **********************************************************************************
// **********************************************************************************

			switch (GateSwapInfo->SwapMode)
			{
			case 1:
			case 2:
				if ((Found1) && (Found2))
				{
					Pos1 = -1;
					Pos2 = -1;

					for (cnt3 = 0; cnt3 < Design.NrWires; cnt3++)
					{
						Wire = &((*Wires)[cnt3]);

						if ((InRange(FoundX1, Wire->X1)) && (InRange(FoundY1, Wire->Y1)))
							Pos1 = cnt3;

						if ((InRange(FoundX1, Wire->X2)) && (InRange(FoundY1, Wire->Y2)))
							Pos1 = cnt3 | 0x10000;

						if ((InRange(FoundX2, Wire->X1)) && (InRange(FoundY2, Wire->Y1)))
							Pos2 = cnt3;

						if ((InRange(FoundX2, Wire->X2)) && (InRange(FoundY2, Wire->Y2)))
							Pos2 = cnt3 | 0x10000;
					}

					if (Pos1 != -1)
					{
						FileChanged = 1;
						Wire = &((*Wires)[Pos1 & 0xffff]);

						if (Pos1 <= 0x10000)
						{
							Wire->X1 = (float) FoundX2;
							Wire->Y1 = (float) FoundY2;
						}
						else
						{
							Wire->X2 = (float) FoundX2;
							Wire->Y2 = (float) FoundY2;
						}
					}

					if (Pos2 != -1)
					{
						Wire = &((*Wires)[Pos2 & 0xffff]);

						if (Pos2 <= 0x10000)
						{
							Wire->X1 = (float) FoundX1;
							Wire->Y1 = (float) FoundY1;
						}
						else
						{
							Wire->X2 = (float) FoundX1;
							Wire->Y2 = (float) FoundY1;
						}
					}

					Pos1 = -1;
					Pos2 = -1;

					for (cnt3 = 0; cnt3 < Design.NrBusses; cnt3++)
					{
						Bus = &((*Busses)[cnt3]);

						if ((InRange(FoundX1, Bus->X1)) && (InRange(FoundY1, Bus->Y1)))
							Pos1 = cnt3;

						if ((InRange(FoundX1, Bus->X2)) && (InRange(FoundY1, Bus->Y2)))
							Pos1 = cnt3 | 0x10000;

						if ((InRange(FoundX2, Bus->X1)) && (InRange(FoundY2, Bus->Y1)))
							Pos2 = cnt3;

						if ((InRange(FoundX2, Bus->X2)) && (InRange(FoundY2, Bus->Y2)))
							Pos2 = cnt3 | 0x10000;
					}

					if (Pos1 != -1)
					{
						FileChanged = 1;
						Bus = &((*Busses)[Pos1 & 0xffff]);

						if (Pos1 <= 0x10000)
						{
							Bus->X1 = (float) FoundX2;
							Bus->Y1 = (float) FoundY2;
						}
						else
						{
							Bus->X2 = (float) FoundX2;
							Bus->Y2 = (float) FoundY2;
						}
					}

					if (Pos2 != -1)
					{
						Bus = &((*Busses)[Pos2 & 0xffff]);

						if (Pos2 <= 0x10000)
						{
							Bus->X1 = (float) FoundX1;
							Bus->Y1 = (float) FoundY1;
						}
						else
						{
							Bus->X2 = (float) FoundX1;
							Bus->Y2 = (float) FoundY1;
						}
					}

					Pos1 = -1;
					Pos2 = -1;

					for (cnt3 = 0; cnt3 < Design.NrGlobalConnections; cnt3++)
					{
						GlobalConnection = &((*GlobalConnections)[cnt3]);

						if ((InRange(FoundX1, GlobalConnection->X)) && (InRange(FoundY1, GlobalConnection->Y)))
							Pos1 = cnt3;

						if ((InRange(FoundX2, GlobalConnection->X)) && (InRange(FoundY2, GlobalConnection->Y)))
							Pos2 = cnt3;
					}

					if (Pos1 != -1)
					{
						GlobalConnection = &((*GlobalConnections)[Pos1]);
						GlobalConnection->X = (float) FoundX2;
						GlobalConnection->Y = (float) FoundY2;
						FileChanged = 1;
					}

					if (Pos2 != -1)
					{
						GlobalConnection = &((*GlobalConnections)[Pos2]);
						GlobalConnection->X = (float) FoundX1;
						GlobalConnection->Y = (float) FoundY1;
						FileChanged = 1;
					}

					Pos1 = -1;
					Pos2 = -1;

					for (cnt3 = 0; cnt3 < Design.NrBusConnections; cnt3++)
					{
						BusConnection = &((*BusConnections)[cnt3]);

						if ((InRange(FoundX1, BusConnection->X)) && (InRange(FoundY1, BusConnection->Y)))
							Pos1 = cnt3;

						if ((InRange(FoundX2, BusConnection->X)) && (InRange(FoundY2, BusConnection->Y)))
							Pos2 = cnt3;
					}

					if (Pos1 != -1)
					{
						BusConnection = &((*BusConnections)[Pos1]);
						BusConnection->X = (float) FoundX2;
						BusConnection->Y = (float) FoundY2;
						FileChanged = 1;
					}

					if (Pos2 != -1)
					{
						BusConnection = &((*BusConnections)[Pos2]);
						BusConnection->X = (float) FoundX1;
						BusConnection->Y = (float) FoundY1;
						FileChanged = 1;
					}

					Pos1 = -1;
					Pos2 = -1;

					for (cnt3 = 0; cnt3 < Design.NrJunctions; cnt3++)
					{
						Junction = &((*Junctions)[cnt3]);

						if ((InRange(FoundX1, Junction->X)) && (InRange(FoundY1, Junction->Y)))
						{
							Pos1 = cnt3;
							FileChanged = 1;
						}

						if ((InRange(FoundX2, Junction->X)) && (InRange(FoundY2, Junction->Y)))
						{
							Pos2 = cnt3;
							FileChanged = 1;
						}
					}

					if (Pos1 != -1)
					{
						Junction = &((*Junctions)[Pos1]);
						Junction->X = (float) FoundX2;
						Junction->Y = (float) FoundY2;
						FileChanged = 1;
					}

					if (Pos2 != -1)
					{
						Junction = &((*Junctions)[Pos2]);
						Junction->X = (float) FoundX1;
						Junction->Y = (float) FoundY1;
						FileChanged = 1;
					}

					Pos1 = -1;
					Pos2 = -1;

					for (cnt3 = 0; cnt3 < Design.NrNetLabels; cnt3++)
					{
						NetLabel = &((*NetLabels)[cnt3]);

						if ((InRange(FoundX1, NetLabel->ConnectX)) && (InRange(FoundY1, NetLabel->ConnectY)))
							Pos1 = cnt3;

						if ((InRange(FoundX2, NetLabel->ConnectX)) && (InRange(FoundY2, NetLabel->ConnectY)))
							Pos2 = cnt3;
					}

					if (Pos1 != -1)
					{
						NetLabel = &((*NetLabels)[Pos1]);
						NetLabel->ConnectX = (float) FoundX2;
						NetLabel->ConnectY = (float) FoundY2;
						FileChanged = 1;
					}

					if (Pos2 != -1)
					{
						NetLabel = &((*NetLabels)[Pos2]);
						NetLabel->ConnectX = (float) FoundX1;
						NetLabel->ConnectY = (float) FoundY1;
						FileChanged = 1;
					}

				}

				break;
			}

		}

// **********************************************************************************
// **********************************************************************************

		if (FileChanged)
		{
// OldDesign
			MakeBackup(FileName);

			if ((Designfp = FileOpenWriteUTF8(FileName)) <= 0)
				return -4;

			MemPos = Design.NrSymbols * sizeof(SymbolsPosRecord);

			if ((strcmp(Design.Identification, SheetCode3) == 0) || (strcmp(Design.Identification, SheetCode4) == 0)
			        || (strcmp(Design.Identification, SheetCode5) == 0))
			{
				if ((strcmp(Design.Identification, SheetCode3) == 0)
				        || (strcmp(Design.Identification, SheetCode4) == 0))
				{
					strcpy(Design.Identification, SheetCode4);
					FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
					FileWrite(Designfp, &DesignBuf[0], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);

					for (cnt = 0; cnt < Design.NrInstances; cnt++)
					{
						MemPos2 = MemPos;
						FileWrite(Designfp, &DesignBuf[MemPos2], offsetof(OldInstanceRecord, Geometry), &result);
						MemPos2 += offsetof(OldInstanceRecord, Geometry);
						FileWrite(Designfp, &DesignBuf[MemPos2],
						          sizeof(OldInstanceRecord) - offsetof(OldInstanceRecord, Geometry), &result);
						MemPos += sizeof(InstanceRecord);
					}
				}
				else
				{
					strcpy(Design.Identification, SheetCode5);
					FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
					FileWrite(Designfp, &DesignBuf[0], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);
					FileWrite(Designfp, &DesignBuf[MemPos], Design.NrInstances * sizeof(InstanceRecord), &result);
				}
			}
			else
			{
				strcpy(Design.Identification, SheetCode2);
				Design.NrOnePinNets = 0;
				FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
				FileWrite(Designfp, &DesignBuf[0], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);

				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					MemPos2 = MemPos;
					FileWrite(Designfp, &DesignBuf[MemPos2], offsetof(OldOldInstanceRecord, Geometry), &result);
					MemPos2 += offsetof(OldOldInstanceRecord, Geometry);
					FileWrite(Designfp, &DesignBuf[MemPos2],
					          sizeof(OldOldInstanceRecord) - offsetof(OldOldInstanceRecord, Geometry), &result);
					MemPos += sizeof(InstanceRecord);
				}
			}

			MemPos = Design.NrSymbols * sizeof(SymbolsPosRecord);
			MemPos += Design.NrInstances * sizeof(InstanceRecord);

			MemSize = Design.NrWires * sizeof(WireRecord);
			MemSize += Design.NrBusses * sizeof(BusRecord);
			MemSize += Design.NrJunctions * sizeof(JunctionRecord);
			MemSize += Design.NrNetLabels * sizeof(NetLabelRecord);
			MemSize += Design.NrBusConnections * sizeof(BusConnectionRecord);
			MemSize += Design.NrGlobalConnections * sizeof(GlobalConnectionRecord);
			MemSize += Design.NrObjectLines * sizeof(ObjectLineRecord);
			MemSize += Design.NrObjectRects * sizeof(ObjectRectRecord);
			MemSize += Design.NrObjectCircles * sizeof(ObjectCircleRecord);
			MemSize += Design.NrObjectArcs * sizeof(ObjectArcRecord);
			MemSize += Design.NrObjectTexts * sizeof(ObjectTextRecord);
			MemSize += Design.NrRedefinedPinBusses * sizeof(RedefinedPinBusRecord);
			MemSize += Design.NrOnePinNets * sizeof(OnePinNetRecord);

			FileWrite(Designfp, &DesignBuf[MemPos], MemSize, &result);
			FileClose(Designfp);

			sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);
			sprintf(str, SC(126, "Sheet %s back annotated\r\n"), FileName);
			AddMessage(str);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
		}
	}

	strcpy(str, SC(127, "Back annotation done\r\n"));
	AddMessage(str);
//  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)SC(127,"Back annotation done\r\n"));

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetComponentFromPartNr(LPSTR PartNr, LPSTR SymbolName, LPSTR Geometry, LPSTR Value, LPSTR Description)
{

	/*
	;Symbolname     Code1  Code2 PartNr        Value             Geometry             Description
	R                   1  100   ""            "0R"              K0402                "RESISTOR 0 OHM"
	R                   1  100   ""            "1R0"             K0402                "RESISTOR 1.0 OHM"
	*/
	char FileStr[MAX_LENGTH_STRING], LineBuf[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING],
	     str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING], str6[MAX_LENGTH_STRING],
	     str7[MAX_LENGTH_STRING];
	int32 fp, Length;

	sprintf(FileStr, "%s\\comp.txt", ExePath);

//      strcpy(FileStr,"c:\\sch\\comp.txt");
	if ((fp = TextFileOpenUTF8(FileStr)) < 0)
	{
		MessageBoxUTF8(DESIGNWindow, FileStr, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/'))
		{
			GetString(LineBuf, str1);
			GetString(LineBuf, str2);
			GetString(LineBuf, str3);
			GetQuoteString(LineBuf, str4);
			GetQuoteString(LineBuf, str5);
			GetString(LineBuf, str6);
			GetQuoteString(LineBuf, str7);

			if (stricmpUTF8(PartNr, str4) == 0)
			{
				strcpy(SymbolName, str1);
				strcpy(Geometry, str6);
				strcpy(Value, str5);
				strcpy(Description, str7);
				TextFileClose(fp);
				return 1;
			}
		}
	}

	TextFileClose(fp);
	return 0;
}

//*******************************************************************************************************
//************************** Zmìnit komponenty IDD_DIALOG_CHANGE_SYMBOLS ********************************
//*******************************************************************************************************

int32 CALLBACK ChangeSymbolDialogBody(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(263, "Change components"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(36, "Help"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(37, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(253, "Symbol name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(31, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(33, "Part number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(32, "Geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(255, "ID"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(31, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(256, "Search string"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(257, "Replace"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(258, "Search"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(259, "Search for"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(260, "Property"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(261, "Part description"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(262, "Get part number info"));

		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(253, "Symbol name"));
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(31, "Value"));
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(32, "Geometry"));
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(33, "Part number"));
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(254, "Add property"));
		SendDlgItemMessageUTF8(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) SC(250, "Change property"));

		if (NewSymbolName[0] == 0)
			strcpy(NewSymbolName, SC(252, "<Do not change>"));

		if (NewPartNr[0] == 0)
			strcpy(NewPartNr, SC(252, "<Do not change>"));

		if (NewValue[0] == 0)
			strcpy(NewValue, SC(252, "<Do not change>"));

		if (NewGeometry[0] == 0)
			strcpy(NewGeometry, SC(252, "<Do not change>"));

		if (NewDescription[0] == 0)
			strcpy(NewDescription, SC(252, "<Do not change>"));

		if (NewPropertyID[0] == 0)
			strcpy(NewPropertyID, SC(252, "<Do not change>"));

		if (NewPropertyValue[0] == 0)
			strcpy(NewPropertyValue, SC(252, "<Do not change>"));

		SetDialogItemTextUTF8(Dialog, IDC_EDIT7, StringToChange);
		SetDialogItemTextUTF8(Dialog, IDC_EDIT1, NewSymbolName);
		SetDialogItemTextUTF8(Dialog, IDC_EDIT3, NewPartNr);
		SetDialogItemTextUTF8(Dialog, IDC_EDIT2, NewValue);
		SetDialogItemTextUTF8(Dialog, IDC_EDIT4, NewGeometry);
		SetDialogItemTextUTF8(Dialog, IDC_EDIT8, NewDescription);
		SetDialogItemTextUTF8(Dialog, IDC_EDIT5, NewPropertyID);
		SetDialogItemTextUTF8(Dialog, IDC_EDIT6, NewPropertyValue);
		SendDlgItemMessage(Dialog, IDC_COMBO1, CB_SETCURSEL, ChangedSymbolMode, 0);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			memset(StringToChange, 0, sizeof(StringToChange));
			memset(NewSymbolName, 0, sizeof(NewSymbolName));
			memset(NewPartNr, 0, sizeof(NewPartNr));
			memset(NewValue, 0, sizeof(NewValue));
			memset(NewGeometry, 0, sizeof(NewGeometry));
			memset(NewDescription, 0, sizeof(NewDescription));
			memset(NewPropertyID, 0, sizeof(NewPropertyID));
			memset(NewPropertyValue, 0, sizeof(NewPropertyValue));
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT7, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) StringToChange);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) NewSymbolName);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) NewPartNr);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) NewValue);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT4, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) NewGeometry);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT8, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) NewDescription);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT5, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) NewPropertyID);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT6, WM_GETTEXT, MAX_LENGTH_STRING - 1, (LPARAM) NewPropertyValue);
			ChangedSymbolMode = SendDlgItemMessage(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

			if ((StringToChange[0] == 0) && (ChangedSymbolMode < 4))
			{
				MessageBoxUTF8(DESIGNWindow, SC(267, "Search string is empty"), SC(19, "Error"), MB_APPLMODAL | MB_OK);
				break;
			}

			if (((NewSymbolName[0] == 0) || (stricmpUTF8(NewSymbolName, SC(252, "<Do not change>")) == 0))
			        && ((NewPartNr[0] == 0) || (stricmpUTF8(NewPartNr, SC(252, "<Do not change>")) == 0))
			        && ((NewValue[0] == 0) || (stricmpUTF8(NewValue, SC(252, "<Do not change>")) == 0))
			        && ((NewDescription[0] == 0) || (stricmpUTF8(NewValue, SC(252, "<Do not change>")) == 0))
			        && ((NewPropertyID[0] == 0) || (stricmpUTF8(NewPropertyID, SC(252, "<Do not change>")) == 0))
			        && ((NewPropertyValue[0] == 0) || (stricmpUTF8(NewPropertyValue, SC(252, "<Do not change>")) == 0))
			        && ((NewGeometry[0] == 0) || (stricmpUTF8(NewGeometry, SC(252, "<Do not change>")) == 0)))
			{
				MessageBoxUTF8(DESIGNWindow, SC(268, "At least one item should be filled in"), SC(19, "Error"),
				               MB_APPLMODAL | MB_OK);
				break;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDC_BUTTON1:
			if (GetComponentFromPartNr(NewPartNr, NewSymbolName, NewGeometry, NewValue, NewDescription) == 1)
			{
				SetDialogItemTextUTF8(Dialog, IDC_EDIT1, NewSymbolName);
				SetDialogItemTextUTF8(Dialog, IDC_EDIT3, NewPartNr);
				SetDialogItemTextUTF8(Dialog, IDC_EDIT2, NewValue);
				SetDialogItemTextUTF8(Dialog, IDC_EDIT4, NewGeometry);
				SetDialogItemTextUTF8(Dialog, IDC_EDIT8, NewDescription);
			}

			break;
		
		case IDHELP:
//			Help("edit_change_components.htm", 0);
			ShellExecute(0, 0, "http://www.pcbelegance.org/docs/current/design/text/edit_change_components.html", 0, 0, SW_SHOW);
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

int32 ChangeInstances(int32 mode)
{
	char str[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], StringToCompare[MAX_LENGTH_STRING],
	     PropertyID[MAX_LENGTH_STRING], PropertyValue[MAX_LENGTH_STRING], NewProperties[1024];
	SymbolsPosRecord NewSymbolsPos[256];
	int32 Found, res, result, Designfp, cnt, cnt2, cnt3, MemPos, Changed, count, MaxMem, NrProperties, MemSize, MemPos2,
	      NrNewSymbols, pos;

	DesignRecord OldDesign;
	InstanceRecord *Instance;

	res = DialogBox(DESIGNClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_CHANGE_SYMBOLS), DESIGNWindow,
	              (DLGPROC) ChangeSymbolDialogBody);

	if (res != 1)
		return -1;

	GetDesignSheets();
	MaxMem = 0;

	for (cnt = 0; cnt < NrSheets; cnt++)
		MaxMem = max(MaxMem, Sheets[cnt].SheetMemSize + Sheets[cnt].SymbolMemSize);

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		strcpy(FileName, SheetDir);
		strcat(FileName, Sheets[cnt2].SheetName);
		strcat(FileName, ".sch");
		MemPos = 0;

		if ((Designfp = FileOpenReadOnlyUTF8(FileName)) == -1)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, SC(17, "Error in opening file"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, SC(125, "Error in reading file"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		FileClose(Designfp);
	}

	MaxMem += 65536;

	if ((MaxMem > DesignBufMemSize) && (AllocateMemDesignBuf(MaxMem) != 0))
		return -1;

// **********************************************************************************
// **********************************************************************************
	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		memset(DesignBuf, 0, MaxMem);
		memset(NewSymbolsPos, 0, sizeof(NewSymbolsPos));
		count = 0;

		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);

		if ((res = LoadSheetInMemory(cnt2, 0)) < 0)
			return -2;

		NrNewSymbols = Design.NrSymbols;
		memcpy(NewSymbolsPos, SymbolsPos, NrNewSymbols * sizeof(SymbolsPosRecord));
		memmove(&OldDesign, &Design, sizeof(DesignRecord));

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			StringToCompare[0] = 0;
			Changed = 0;

			switch (ChangedSymbolMode)
			{
			case 0:
				strcpy(StringToCompare, Instance->SymbolName);
				break;

			case 1:
				strcpy(StringToCompare, Instance->Value);
				break;

			case 2:
				strcpy(StringToCompare, Instance->PartNr);
				/*
				          if (GetComponentFromPartNr(Instance->PartNr,NewSymbolName,NewGeometry,
				                                     NewValue,NewDescription)==1) {
				          }
				*/
				break;

			case 3:
				strcpy(StringToCompare, Instance->Geometry);
				break;
			}

			if ((ChangedSymbolMode == 4) || (ChangedSymbolMode == 5)
			        || (stricmpUTF8(StringToChange, StringToCompare) == 0))
			{
				if ((NewSymbolName[0]) && (stricmpUTF8(NewSymbolName, SC(252, "<Do not change>")))
				        && (stricmpUTF8(Instance->SymbolName, NewSymbolName)))
				{
					memset(&Instance->SymbolName, 0, sizeof(Instance->SymbolName));
					strncpy(Instance->SymbolName, NewSymbolName, sizeof(Instance->SymbolName) - 1);
					cnt3 = 0;

					while (stricmpUTF8(NewSymbolsPos[cnt3].SymbolName, NewSymbolName) != 0)
						cnt3++;

					if (cnt3 == NrNewSymbols)
					{
						strncpy(NewSymbolsPos[cnt3].SymbolName, NewSymbolName,
						        sizeof(NewSymbolsPos[cnt3].SymbolName) - 1);
						NrNewSymbols++;
						sprintf(str, SC(264, "!!!! Symbol name %s will be changed in %s\r\n"), Instance->SymbolName,
						        NewSymbolName);
						AddMessage(str);
					}

					Changed = 1;
				}

				if ((NewValue[0]) && (stricmpUTF8(NewValue, SC(252, "<Do not change>")))
				        && (stricmpUTF8(Instance->Value, NewValue)))
				{
					memset(&Instance->Value, 0, sizeof(Instance->Value));
					strncpy(Instance->Value, NewValue, sizeof(Instance->Value) - 1);
					Changed = 1;
				}

				if ((NewPartNr[0]) && (stricmpUTF8(NewPartNr, SC(252, "<Do not change>")))
				        && (stricmpUTF8(Instance->PartNr, NewPartNr)))
				{
					memset(&Instance->PartNr, 0, sizeof(Instance->PartNr));
					strncpy(Instance->PartNr, NewPartNr, sizeof(Instance->PartNr) - 1);
					Changed = 1;
				}

				if ((NewGeometry[0]) && (stricmpUTF8(NewGeometry, SC(252, "<Do not change>")))
				        && (stricmpUTF8(Instance->Geometry, NewGeometry)))
				{
					memset(&Instance->Geometry, 0, sizeof(Instance->Geometry));
					strncpy(Instance->Geometry, NewGeometry, sizeof(Instance->Geometry) - 1);
					Changed = 1;
				}

				if ((NewDescription[0]) && (stricmpUTF8(NewDescription, SC(252, "<Do not change>")))
				        && (stricmpUTF8(Instance->PartDescription, NewDescription)))
				{
					memset(&Instance->PartDescription, 0, sizeof(Instance->PartDescription));
					strncpy(Instance->PartDescription, NewDescription, sizeof(Instance->PartDescription) - 1);
					Changed = 1;
				}

				if ((NewPropertyID[0]) && (stricmpUTF8(NewPropertyID, SC(252, "<Do not change>")))
				        && (stricmpUTF8(NewPropertyValue, SC(252, "<Do not change>"))))
				{
					NrProperties = GetProperties(Instance->Properties, NULL, NULL, 0x40);
					memset(NewProperties, 0, sizeof(Instance->Properties));
					pos = 0;
					Found = 0;

					for (cnt3 = 0; cnt3 < NrProperties; cnt3++)
					{
						if (GetProperties
						        (Instance->Properties, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt3) == 0)
						{
							if (strcmpUTF8(PropertyID, NewPropertyID) == 0)
							{
								if ((NewPropertyValue[0] != 0)
								        && (pos + strlen(PropertyID) + 1 + strlen(NewPropertyValue) + 2 <
								            sizeof(Instance->Properties)))
								{
									strcpy((LPSTR) & NewProperties[pos], PropertyID);
									pos += strlen(PropertyID) + 1;
									strcpy((LPSTR) & NewProperties[pos], NewPropertyValue);
									pos += strlen(NewPropertyValue) + 1;
								}

								Found = 1;
							}
							else
							{
								if (pos + strlen(PropertyID) + 1 + strlen(PropertyValue) + 2 <
								        sizeof(Instance->Properties))
								{
									strcpy((LPSTR) & NewProperties[pos], PropertyID);
									pos += strlen(PropertyID) + 1;
									strcpy((LPSTR) & NewProperties[pos], PropertyValue);
									pos += strlen(PropertyValue) + 1;
								}
							}
						}
					}

					if ((!Found) && (NewPropertyValue[0] != 0)
					        && (pos + strlen(NewPropertyID) + 1 + strlen(NewPropertyValue) + 2 <
					            sizeof(Instance->Properties)))
					{
						strcpy((LPSTR) & NewProperties[pos], NewPropertyID);
						pos += strlen(PropertyID) + 1;
						strcpy((LPSTR) & NewProperties[pos], NewPropertyValue);
						pos += strlen(NewPropertyValue) + 1;
					}

					if (memcmp(Instance->Properties, NewProperties, sizeof(Instance->Properties)) != 0)
					{
						Changed = 1;
						memcpy(Instance->Properties, NewProperties, sizeof(Instance->Properties));
					}
				}
			}

			if (Changed)
				count++;
		}

// **********************************************************************************
// **********************************************************************************


		if ((count > 0)
		        && ((NrNewSymbols == Design.NrSymbols)
		            ||
		            (MessageBoxUTF8
		             (DESIGNWindow, SC(345, "Symbol names will be changed\r\nDo you want to continue ?"), SC(8, "Message"),
		              MB_APPLMODAL | MB_OKCANCEL) == IDOK)))
		{
			MakeBackup(FileName);

			if ((Designfp = FileOpenWriteUTF8(FileName)) <= 0)
				return -4;

			if (Design.NrSymbols != NrNewSymbols)
			{
				memmove(&DesignBuf[NrNewSymbols * sizeof(SymbolsPosRecord)],
				        &DesignBuf[Design.NrSymbols * sizeof(SymbolsPosRecord)],
				        MaxMem - Design.NrSymbols * sizeof(SymbolsPosRecord));
			}

			memcpy(&DesignBuf[0], NewSymbolsPos, NrNewSymbols * sizeof(SymbolsPosRecord));
			Design.NrSymbols = NrNewSymbols;

			MemPos = Design.NrSymbols * sizeof(SymbolsPosRecord);

			if ((strcmp(Design.Identification, SheetCode3) == 0) || (strcmp(Design.Identification, SheetCode4) == 0)
			        || (strcmp(Design.Identification, SheetCode5) == 0))
			{
				if ((strcmp(Design.Identification, SheetCode3) == 0)
				        || (strcmp(Design.Identification, SheetCode4) == 0))
				{
					strcpy(Design.Identification, SheetCode4);
					FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
					FileWrite(Designfp, &DesignBuf[0], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);

					for (cnt = 0; cnt < Design.NrInstances; cnt++)
					{
						MemPos2 = MemPos;
						FileWrite(Designfp, &DesignBuf[MemPos2], offsetof(OldInstanceRecord, Geometry), &result);
						MemPos2 += offsetof(OldInstanceRecord, Geometry);
						FileWrite(Designfp, &DesignBuf[MemPos2],
						          sizeof(OldInstanceRecord) - offsetof(OldInstanceRecord, Geometry), &result);
						MemPos += sizeof(InstanceRecord);
					}
				}
				else
				{
					strcpy(Design.Identification, SheetCode5);
					FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
					FileWrite(Designfp, &DesignBuf[0], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);
					FileWrite(Designfp, &DesignBuf[MemPos], Design.NrInstances * sizeof(InstanceRecord), &result);
				}
			}
			else
			{
				strcpy(Design.Identification, SheetCode2);
				Design.NrOnePinNets = 0;
				FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
				FileWrite(Designfp, &DesignBuf[0], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);

				for (cnt = 0; cnt < Design.NrInstances; cnt++)
				{
					MemPos2 = MemPos;
					FileWrite(Designfp, &DesignBuf[MemPos2], offsetof(OldOldInstanceRecord, Geometry), &result);
					MemPos2 += offsetof(OldOldInstanceRecord, Geometry);
					FileWrite(Designfp, &DesignBuf[MemPos2],
					          sizeof(OldOldInstanceRecord) - offsetof(OldOldInstanceRecord, Geometry), &result);
					MemPos += sizeof(InstanceRecord);
				}
			}

			MemPos = Design.NrSymbols * sizeof(SymbolsPosRecord);
			MemPos += Design.NrInstances * sizeof(InstanceRecord);

			MemSize = Design.NrWires * sizeof(WireRecord);
			MemSize += Design.NrBusses * sizeof(BusRecord);


			MemSize += Design.NrWires * sizeof(WireRecord);
			MemSize += Design.NrBusses * sizeof(BusRecord);
			MemSize += Design.NrJunctions * sizeof(JunctionRecord);
			MemSize += Design.NrNetLabels * sizeof(NetLabelRecord);
			MemSize += Design.NrBusConnections * sizeof(BusConnectionRecord);
			MemSize += Design.NrGlobalConnections * sizeof(GlobalConnectionRecord);
			MemSize += Design.NrObjectLines * sizeof(ObjectLineRecord);
			MemSize += Design.NrObjectRects * sizeof(ObjectRectRecord);
			MemSize += Design.NrObjectCircles * sizeof(ObjectCircleRecord);
			MemSize += Design.NrObjectArcs * sizeof(ObjectArcRecord);
			MemSize += Design.NrObjectTexts * sizeof(ObjectTextRecord);
			MemSize += Design.NrRedefinedPinBusses * sizeof(RedefinedPinBusRecord);
			MemSize += Design.NrOnePinNets * sizeof(OnePinNetRecord);

			FileWrite(Designfp, &DesignBuf[MemPos], MemSize, &result);
			FileClose(Designfp);

			sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);
			sprintf(str, SC(264, "%d symbols changed in sheet %s\r\n"), count, FileName);
			AddMessage(str);
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ChangeUserVar(LPSTR UserVarID, LPSTR UserVarValue, int32 mode)
{
	int32 Length, count, fp, fp2, changed;
	char LineBuf[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING],
	     UserVarFileName[MAX_LENGTH_STRING], UserVarFileName2[MAX_LENGTH_STRING];

	changed = 0;
	count = 0;
	strcpy(str, DesignFile);
	CutExtensionFileName(str);
	sprintf(UserVarFileName, "%s.var", str);
	sprintf(UserVarFileName2, "%s_new.var", str);
	fp2 = FileOpenWriteUTF8(UserVarFileName2);

	if (fp2 <= 0)
		return -1;

	fp = TextFileOpenUTF8(UserVarFileName);

	if (fp > 0)
	{
		while ((Length = ReadLnWithMaxLength(fp, LineBuf, 200)) >= 0)
		{
			if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#')
			        && (GetStringValue(LineBuf, str, str2)))
			{
				if (stricmpUTF8(UserVarID, str) == 0)
				{
					if (strcmpUTF8(UserVarValue, str2) == 0)
					{
						changed = 1;
						strcpy(str2, UserVarValue);
					}
				}

				sprintf(str3, "%s=\"%s\"", str, str2);
				WriteLn(fp2, str3);
				count++;
			}
			else
				WriteLn(fp2, LineBuf);
		}

		TextFileClose(fp);
	}
	else
	{
		sprintf(str3, "%s=\"%s\"", UserVarID, UserVarValue);
		WriteLn(fp2, str3);
		changed = 1;
	}

	FileClose(fp2);

	if (changed)
	{
		DeleteFileUTF8(UserVarFileName);
		MoveFileUTF8(UserVarFileName2, UserVarFileName);
	}
	else
		DeleteFileUTF8(UserVarFileName2);

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
