/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: instance.c
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
#include "calc.h"
#include "design.h"
#include "files.h"
#include "files2.h"
#include "string.h"
#include "math.h"
#include "check.h"
#include "stdio.h"
#include "nets.h"
#include "io.h"
#include "time.h"
#include "stddef.h"
#include "instance.h"
#include "ctype.h"
#include "utf8.h"

#define CompPartMemSize           65536
#define MAX_NR_CODES_PER_REF      32768
#define MAX_DOUBLE_REFS           1024
#define PCBCode                   "PCB definition 1.0"
#define PCBCode2                  "PCB definition 2.0"
#define PCBCode3                  "PCB definition 2.5"
#define PCBCode4                  "PCB definition 3.0"
#define MAX_REF_TYPES             128
#define DASH_STRING               "---------------------------------------------------------------------------------------------------------------------------------"
#define SPACE_STRING              "                                                                                                                                 "
#define DOT_STRING                "  ..............................................................................................................................."

typedef uint8 RefBufArray[100][MAX_NR_CODES_PER_REF];
typedef uint8 ByteArray[CompPartMemSize];


typedef struct
{
	int32 Index, StartNumber;
} SheetIndexRecord;

int32 NrSpecialComponents, NrMultipleSymbolsComps, ok, NrUsableComps, NrPartNames, MaxMem;
ComponentRecord *CompPos[MAX_REF_TYPES];

MultipleSymbolArray MultipleSymbols;
PartNameArray *PartNames;
PartNameRecord *PartPos[MAX_REF_TYPES];
OldCompRecord OldComp;
PCBDesignRecord LayoutDesign;
CompsArray *LayoutComps;
uint8 *LayoutCompsMem;

char RefCodes[MAX_REF_TYPES][8];
char RefCodes2[MAX_REF_TYPES][8];
char PCBFile[MAX_LENGTH_STRING], PCBFileBackup[MAX_LENGTH_STRING];
int32 NrRefCodes, NrCodesPerRef[MAX_REF_TYPES], MaxCodesRef[MAX_REF_TYPES], ok, NrSymbNames, ComponentLineOutputCount,
      UsedLayoutCompRefPtr;

char SheetRefCodes[MAX_REF_TYPES][8];
int32 NrSheetRefCodes, NrSheetCodesPerRef[MAX_REF_TYPES], RefCodeInfo[MAX_REF_TYPES], SheetMaxCodesRef[MAX_REF_TYPES],
      SheetMinCodesRef[MAX_REF_TYPES];

RefBufArray *RefBuf;

ByteArray *CompOccupation;

InstancesArray *NewInstances;

int32 LoadLayout(int32 Mode);

extern int32 WriteLnError;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void MakeString(LPSTR outstr, LPSTR instr, int32 length, int32 mode)
{
	char str[MAX_LENGTH_STRING];

	if (length < 2)
	{
		outstr[0] = 0;
		return;
	}

	memset(str, 0, sizeof(str));

	if (length - (int32) strlen(instr) > 0)
	{
		switch (mode)
		{
		case 0:
		case 2:
			strncpy(str, SPACE_STRING, length - strlen(instr));
			break;

		case 4:
			strncpy(str, DOT_STRING, length - strlen(instr));
			break;

		case 1:
		case 3:
		case 5:
			str[0] = ' ';
			break;
		}

		sprintf(outstr, "%s%s", instr, str);
	}
	else
	{
		strncpy(outstr, instr, length - 1);
		outstr[length - 1] = 0;
		strcat(outstr, " ");
	}

	switch (mode)
	{
	case 0:
		outstr[strlen(outstr) - 1] = ' ';
		break;

	case 1:
		outstr[strlen(outstr) - 1] = '\t';
		break;

	case 2:
	case 4:
		outstr[strlen(outstr) - 1] = '|';
		break;

	case 3:
		outstr[strlen(outstr) - 1] = '+';
		break;

	case 5:
		outstr[strlen(outstr) - 1] = ',';
		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetRefCode(LPSTR Ref, LPSTR RefAlpha, int32 * Num)
{
	int32 lengte, pos1, hulp, cnt, FoundNr;

	RefAlpha[0] = 0;
	lengte = strlen(Ref);

	if (lengte < 2)
		return -1;

	if (!isalpha(Ref[0]))
		return -1;

	if ((!isdigit(Ref[lengte - 1])) && (Ref[lengte - 1] != '?'))
		return -1;

	pos1 = 0;

	while ((pos1 < lengte) && (isalpha(Ref[pos1])))
		pos1++;

	strncpy(RefAlpha, Ref, pos1);
	RefAlpha[pos1] = 0;

	hulp = -1;

	if ((Ref[pos1] != '?') && (sscanf((LPSTR) & Ref[pos1], "%i", &hulp) != 1))
		return -1;

	*Num = hulp;

	FoundNr = -1;

	for (cnt = 0; cnt < NrRefCodes; cnt++)
	{
		if (stricmpUTF8(RefCodes[cnt], RefAlpha) == 0)
			FoundNr = cnt;
	}

	if (FoundNr == -1)
	{
		strcpy(RefCodes[NrRefCodes], RefAlpha);
		_strupr(RefCodes[NrRefCodes]);

		if (NrRefCodes < MAX_REF_TYPES)
		{
			NrCodesPerRef[NrRefCodes] = 1;
			MaxCodesRef[NrRefCodes] = -1;

			if (hulp != -1)
				MaxCodesRef[NrRefCodes] = hulp;

			NrRefCodes++;
		}

		return NrRefCodes - 1;
	}
	else
	{
		if (hulp != -1)
			MaxCodesRef[FoundNr] = max(MaxCodesRef[FoundNr], hulp);

		NrCodesPerRef[FoundNr]++;
		return FoundNr;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetRefCode2(LPSTR Ref, LPSTR RefAlpha, int32 * Num)
{
	int32 lengte, pos1, hulp;

	RefAlpha[0] = 0;
	lengte = strlen(Ref);

	if (lengte < 2)
		return -1;

	if (!isalpha(Ref[0]))
		return -1;

	if ((!isdigit(Ref[lengte - 1])) && (Ref[lengte - 1] != '?'))
		return -1;

	pos1 = 0;

	while ((pos1 < lengte) && (isalpha(Ref[pos1])))
		pos1++;

	strncpy(RefAlpha, Ref, pos1);
	RefAlpha[pos1] = 0;

	hulp = -1;

	if ((Ref[pos1] != '?') && (sscanf((LPSTR) & Ref[pos1], "%i", &hulp) != 1))
		return -1;

	*Num = hulp;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetSheetRefIndex(LPSTR Ref)
{
	int32 cnt;

	for (cnt = 0; cnt < NrSheetRefCodes; cnt++)
	{
		if (stricmpUTF8(SheetRefCodes[cnt], Ref) == 0)
			return cnt;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void AddSheetRefCodes(ComponentRecord * Component)
{
	int32 RefNum;

	RefNum = Component->RefNum;

	if (RefNum != -1)
	{
		SheetMaxCodesRef[Component->RefCodeNr] = max(SheetMaxCodesRef[Component->RefCodeNr], RefNum);
		SheetMinCodesRef[Component->RefCodeNr] = min(SheetMinCodesRef[Component->RefCodeNr], RefNum);
		NrSheetCodesPerRef[Component->RefCodeNr]++;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 RegroupComponents(int32 mode)
{
	ComponentRecord *Component, *Component2, NewComponent;
	int32 cnt, CompCnt;

	cnt = NrComponents - 1;
	CompCnt = 0;

	while (cnt > CompCnt)
	{
		Component = &((*Components)[cnt]);

		if (((Component->SymbolInfo & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS) || (Component->NrPartsPerPackage > 1))
		{
			if (cnt > CompCnt)
			{
				Component2 = &((*Components)[CompCnt]);

				if (((Component2->SymbolInfo & MULTIPLE_SYMBOLS) == 0) && (Component2->NrPartsPerPackage < 2))
				{
					memmove(&NewComponent, Component, sizeof(ComponentRecord));
					memmove(Component, Component2, sizeof(ComponentRecord));
					memmove(Component2, &NewComponent, sizeof(ComponentRecord));
					cnt--;
				}

				CompCnt++;
			}
		}
		else
			cnt--;
	}

	NrSpecialComponents = -1;
	cnt = 0;

	for (cnt = 0; cnt < NrComponents; cnt++)
	{
		Component = &((*Components)[cnt]);

		if (((Component->SymbolInfo & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS) || (Component->NrPartsPerPackage > 1))
			NrSpecialComponents = cnt;
	}

	NrSpecialComponents++;

	if (NrSpecialComponents > 0)
	{
		cnt = NrSpecialComponents - 1;
		CompCnt = 0;

		while (cnt > CompCnt)
		{
			Component = &((*Components)[cnt]);

			if ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS)
			{
				if (cnt > CompCnt)
				{
					Component2 = &((*Components)[CompCnt]);

					if ((Component2->SymbolInfo & MULTIPLE_SYMBOLS) == 0)
					{
						memmove(&NewComponent, Component, sizeof(ComponentRecord));
						memmove(Component, Component2, sizeof(ComponentRecord));
						memmove(Component2, &NewComponent, sizeof(ComponentRecord));
						cnt--;
					}

					CompCnt++;
				}
			}
			else
				cnt--;
		}

		NrMultipleSymbolsComps = -1;
		cnt = 0;

		for (cnt = 0; cnt < NrSpecialComponents; cnt++)
		{
			Component = &((*Components)[cnt]);

			if ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS)
				NrMultipleSymbolsComps = cnt;
		}

		NrMultipleSymbolsComps++;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckMultipleSymbolsComps()
{
	ComponentRecord *Component;
	int32 cnt, cnt2, FoundNr, cnt3, cnt4;

	NrSymbNames = 0;

	for (cnt = 0; cnt < NrMultipleSymbolsComps; cnt++)
	{
		Component = &((*Components)[cnt]);
		Component->Info = 0;
		FoundNr = -1;
		cnt2 = 0;

		while ((cnt2 < NrSymbNames) && (stricmpUTF8(Component->InterfaceName, MultipleSymbols[cnt2].SymbName) != 0))
			cnt2++;

		if (cnt2 < NrSymbNames)
		{
			if (stricmpUTF8(MultipleSymbols[cnt2].Reference, Component->Reference) == 0)
			{
				cnt3 = 0;

				while ((cnt3 < MultipleSymbols[cnt2].NrSymbInterfaceNames)
				        && (stricmpUTF8(MultipleSymbols[cnt2].SymbInterfaceNames[cnt3], Component->SymbolName) != 0))
					cnt3++;

				if (cnt3 == MultipleSymbols[cnt2].NrSymbInterfaceNames)
				{
					if (cnt3 < 8)
					{
						strcpy(MultipleSymbols[cnt2].SymbInterfaceNames[cnt3], Component->SymbolName);
						MultipleSymbols[cnt2].NrSymbInterfaceNames++;
						Component->PackagePartNr = cnt3;
					}
				}
				else
					Component->PackagePartNr = cnt3;

				Component->MultiSymbolNr = cnt2;
			}
			else
				Component->Error = 10;
		}
		else
		{
			if (NrSymbNames < 32)
			{
				memset(&MultipleSymbols[NrSymbNames], 0, sizeof(MultipleSymbols[0]));

				strcpy(MultipleSymbols[NrSymbNames].SymbName, Component->InterfaceName);
				strcpy(MultipleSymbols[NrSymbNames].SymbInterfaceNames[0], Component->SymbolName);
				MultipleSymbols[NrSymbNames].NrSymbInterfaceNames = 1;
				strcpy(MultipleSymbols[NrSymbNames].Reference, Component->Reference);

				for (cnt4 = 0; cnt4 < 8; cnt4++)
					MultipleSymbols[NrSymbNames].ReferenceNr[cnt4] = -1;

				Component->MultiSymbolNr = NrSymbNames;
				Component->PackagePartNr = 0;
				NrSymbNames++;
			}
		}
	}

	/*
	  for (cnt2=0;cnt2<NrSymbNames;cnt2++) {
	    if (SymbCount[cnt2]==1) {
	      Error=1;
	      for (cnt=0;cnt<NrMultipleSymbolsComps;cnt++) {
	        Component=&((*Components)[cnt]);
	        if (Component->MultiSymbolNr==cnt2) Component->Error=10;
	      }
	    }
	  }
	*/
//  if (Error) return -1;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SearchMultiPartComp(int32 CompIndex, int32 PartsPerPackage, int32 Start)
{
	int32 cnt, cnt2, cnt3, Count, MemSize;
	int16 *CompRef, *CompRef2, *CompPos[32];
	ComponentRecord *Component;
	uint8(*Occupation)[];
	int32 Found = 0;

	cnt2 = 0;
//
//  Compref       (2 bytes)
//  Occupation 1  (1 byte)
//  Occupation 2  (1 byte)
//  Occupation 3  (1 byte)
//  Occupation .. (1 byte)
//
//
	CompOccupation = (ByteArray *) & DesignBuf[DesignBufMemSize - CompPartMemSize];
	memset(CompOccupation, 0, CompPartMemSize);
	Count = 0;
	MemSize = 2 + (PartsPerPackage / 2);

	for (cnt = NrMultipleSymbolsComps; cnt < NrSpecialComponents; cnt++)
	{
		Component = &((*Components)[cnt]);

		if ((Component->MultiPartNr == CompIndex) && (Component->RefNum != -1))
		{
			CompRef = (int16 *) CompOccupation;
			cnt3 = 0;

			while ((cnt3 < Count) && (*CompRef < Component->RefNum))
			{
				cnt3++;
				CompRef += MemSize;
			}

			Occupation = (uint8(*)[])(CompRef + 1);

			if ((Count == 0) || (*CompRef != Component->RefNum))
			{
				if (Count < (CompPartMemSize / MemSize))
				{
					if (cnt3 < Count)
					{
						CompRef2 = CompRef + MemSize;
						memmove(CompRef2, CompRef, (Count - cnt3) * MemSize);
					}

					*CompRef = (int16) Component->RefNum;
					(*Occupation)[Component->PackagePartNr - 1] = 1;
					Count++;
				}
			}
			else
				(*Occupation)[Component->PackagePartNr - 1] = 1;
		}
	}

	CompRef = (int16 *) CompOccupation;

//  BytePos=(BArray *)(CompRef);
	for (cnt = 0; cnt < 31; cnt++)
	{
		CompPos[cnt] = CompRef;
		CompRef += MemSize;
	}

	CompRef = (int16 *) CompOccupation;
	cnt = 0;

	while ((cnt < Count) && (!Found))
	{
		if (*CompRef >= Start)
		{
			Occupation = (uint8(*)[])(CompRef + 1);
			cnt2 = 0;

			while ((cnt2 < PartsPerPackage) && (!Found))
			{
				if ((*Occupation)[cnt2] == 0)
					Found = 1;
				else
					cnt2++;
			}
		}

		if (!Found)
		{
			CompRef += MemSize;
			cnt++;
		}
	}

	if (!Found)
		return -1;

	return (((*CompRef) << 12) + cnt2);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetComponentsSheets(int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, MemPos, res, MemPos2, FoundNr, RefNum, Index, NrUsedMultiPartRefs, CompCnt, SymbolNr,
	      InstMemSize, NrPartsPerRef[4096];
	uint8 MultiplePartsCodeRef[4096][8];
	char FileName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	char UsedMultiPartRefs[4096][16];
	uint32 crc = 0xffffffff;

	SymbolsPos2Record *SymbolPos2;
	SubPinDefsArray *SubPinDefs;
	SymbolRecord *Symbol;
	ComponentRecord *Component;
	PartNameRecord *PartName;
	int32 ComponentError = 0;
	int32 ComponentError2;
	int32 OnlyPowerPins;
	InstanceRecord *Instance;
	ObjectRecord *Object;

	GetDesignSheets();

	MaxMem = 0;

	for (cnt = 0; cnt < NrSheets; cnt++)
		MaxMem = max(MaxMem, Sheets[cnt].SheetMemSize + Sheets[cnt].SymbolMemSize);

	CompCnt = 0;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);

		if (LoadSheetInMemory(cnt2, 3) < 0)
			return -1;

		CompCnt += Design.NrInstances;
	}

	InstMemSize = ((MaxMem + 7) & ~7) + CompCnt * sizeof(PartNameRecord) + CompPartMemSize;

	if ((InstMemSize > DesignBufMemSize) && (AllocateMemDesignBuf(InstMemSize) != 0))
		return -1;

	if (CompCnt + 1 > MaxNrComponents)
		AllocateMemComponents(CompCnt);

	NrComponents = CompCnt;
	CompCnt = 0;
	cnt4 = 0;
	PartNames = (PartNameArray *) & DesignBuf[MaxMem];

	NrRefCodes = 0;

	for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
		MaxCodesRef[cnt] = -1;

// **********************************************************************************
// **********************************************************************************

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		LoadSheetInMemory(cnt2, mode);

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
#ifdef _DEBUG

			if (cnt == 68)
				res = 1;

			if (stricmp(Instance->SymbolName, "J103") == 0)
			{
//      if (stricmp(Instance->SymbolName,"MOUNTINGHOLE")==0) {
				res = 1;
			}

#endif
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
				MemPos2 = MemPos + sizeof(SymbolRecord);
				Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);
				SubPinDefs = (SubPinDefsArray *) & ((*SymbolsMem)[MemPos2]);
				NrObjects = 0;
				InstancePinsToObject(Instance, 0);

				if ((Symbol->Info & (SHEET_SYMBOL)) == 0)
				{
					Component = &((*Components)[CompCnt]);
#ifdef _DEBUG

					if (CompCnt == 68)
						res = 1;

#endif
					memset(Component, 0, sizeof(ComponentRecord));
					strcpy(Component->SymbolName, Instance->SymbolName);
					strcpy(Component->InterfaceName, Symbol->InterfaceName);
					strcpy(Component->Value, Instance->Value);
					strcpy(Component->Geometry, Instance->Geometry);
					Component->SymbolInfo = Symbol->Info;
					Component->InstanceNr = cnt;
					Component->SheetNr = cnt2;
					Component->MultiPartNr = -1;
					Component->MultiSymbolNr = -1;
					Component->x = -10000.0;
					Component->y = -10000.0;
					Component->CrcPackageBytes = 0;

					if (NrObjects > 0)
					{
						OnlyPowerPins = 1;

						for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
						{
							Object = &((*Objects)[cnt3]);

							switch (Object->ObjectType)
							{
							case SYMBOL_PIN:
								if (Object->Info2 != POWER_CONNECTION)
									OnlyPowerPins = 0;

								Component->x = (float) Object->x1;
								Component->y = (float) Object->y1;
								break;

							case SYMBOL_PINBUS:
								OnlyPowerPins = 0;
								break;
							}
						}

						if (!OnlyPowerPins)
						{
							strcpy(str, Instance->Reference);
							struprUTF8(str);

							if ((Index = GetRefCode(str, str2, &RefNum)) != -1)
							{
								strcpy(Component->Reference, str2);
								Component->RefNum = RefNum;
								Component->RefCodeNr = Index;
								cnt4++;
							}
							else
								Component->Error = 5;
						}
						else
							Component->Error = 6;

						CompCnt++;
					}
					else
					{
// **********************************************************************************
// Components with a symbol with no pins
						res = 1;

						if (Instance->Reference[0] != 0)
						{
							res = 1;
							strcpy(str, Instance->Reference);
							struprUTF8(str);
							cnt3 = 0;

							while ((cnt3 < 3) && (str[cnt3] != 0) && (str[cnt3] == '0'))
								cnt3++;

							switch (cnt3)
							{
							case 1:
								Component->Info |= REF_WITH_LEADING_ZERO;
								break;

							case 2:
								Component->Info |= REF_WITH_TWO_LEADING_ZEROS;
								break;

							case 3:
								Component->Info |= REF_WITH_THREE_LEADING_ZEROS;
								break;
							}

							if ((Index = GetRefCode(str, str2, &RefNum)) != -1)
							{
								strcpy(Component->Reference, str2);
								Component->RefNum = RefNum;
								Component->RefCodeNr = Index;
								cnt4++;
								CompCnt++;
							}
							else
								Component->Error = 5;
						}
					}

					if ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == 0)
					{
						Component->PackagePartNr = Instance->PackagePartNr;
						Component->NrPartsPerPackage = Symbol->NrPartsPerPackage;
					}
				}
			}
		}
	}

// **********************************************************************************
// **********************************************************************************

	NrUsableComps = CompCnt;
	RegroupComponents(0);

	for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
	{
		CompPos[cnt] = &((*Components)[cnt]);
		PartPos[cnt] = &((*PartNames)[cnt]);
	}

	CheckMultipleSymbolsComps();

// **********************************************************************************
// **********************************************************************************

	NrPartNames = 0;

	for (cnt = NrMultipleSymbolsComps; cnt < NrSpecialComponents; cnt++)
	{
		Component = &((*Components)[cnt]);
		FoundNr = -1;

		for (cnt2 = 0; cnt2 < NrPartNames; cnt2++)
		{
			PartName = &((*PartNames)[cnt2]);

			if ((stricmpUTF8(Component->SymbolName, PartName->SymbolName) == 0)
			        && (stricmpUTF8(Component->Value, PartName->Value) == 0))
			{
				if (FoundNr == -1)
				{
					FoundNr = cnt2;
					break;
				}
			}
		}

		if (FoundNr != -1)
		{
			PartName = &((*PartNames)[FoundNr]);

			if (stricmpUTF8(PartName->RefName, Component->Reference) == 0)
			{
				Component->MultiPartNr = FoundNr;
				PartName->Count++;
			}
			else
				Component->Error = 7;
		}
		else
		{
			PartName = &((*PartNames)[NrPartNames]);
			strcpy(PartName->SymbolName, Component->SymbolName);
			strcpy(PartName->Value, Component->Value);
			PartName->LastRef = 0;
			memset(PartName->RefName, 0, sizeof(PartName->RefName));
			strcpy(PartName->RefName, Component->Reference);
			PartName->Count = 1;
			PartName->UsedParts = 1;
			PartName->PartsPerPackage = Component->NrPartsPerPackage;
			Component->MultiPartNr = NrPartNames;
			NrPartNames++;
		}
	}

	if (mode == 0)
		return 0;

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt2 = 0; cnt2 < NrPartNames; cnt2++)
	{
		PartName = &((*PartNames)[cnt2]);
		NrUsedMultiPartRefs = 0;
		memset(&NrPartsPerRef, 0, sizeof(NrPartsPerRef));
		memset(&UsedMultiPartRefs, 0, sizeof(UsedMultiPartRefs));
		cnt4 = 0;

		for (cnt = 0; cnt < NrUsableComps; cnt++)
		{
			Component = &((*Components)[cnt]);

			if ((Component->NrPartsPerPackage > 1) && (Component->MultiPartNr == cnt2))
			{
				sprintf(str2, "%s%d", Component->Reference, Component->RefNum);

				if (cnt4 == 0)
					crc = Component->CrcPackageBytes;
				else
				{
					/*
					          if (crc!=Component->CrcPackageBytes) {
					            sprintf(str,"Changed symbol for component %s\r\n",str2);
					            SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
					            ComponentError=1;
					          }
					*/
				}

				cnt3 = 0;

				while ((cnt3 < NrUsedMultiPartRefs) && (stricmpUTF8(str2, UsedMultiPartRefs[cnt3]) != 0))
					cnt3++;

				if (NrUsedMultiPartRefs < 4095)
				{
					NrPartsPerRef[cnt3]++;

					if (cnt3 == NrUsedMultiPartRefs)
					{
						strcpy(UsedMultiPartRefs[cnt3], str2);
						NrUsedMultiPartRefs++;
					}
				}

				cnt4++;
			}
		}

		for (cnt = 0; cnt < NrUsedMultiPartRefs; cnt++)
		{
			if (NrPartsPerRef[cnt] > PartName->PartsPerPackage)
			{
				sprintf(str, SC(86, "Too much parts for component %s\r\n"), UsedMultiPartRefs[cnt]);
				AddMessage(str);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				ComponentError = 1;
				ok = 1;
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt2 = 0; cnt2 < NrSymbNames; cnt2++)
	{
		NrUsedMultiPartRefs = 0;
		memset(&MultiplePartsCodeRef, 0, sizeof(MultiplePartsCodeRef));
		memset(&UsedMultiPartRefs, 0, sizeof(UsedMultiPartRefs));

		for (cnt = 0; cnt < NrMultipleSymbolsComps; cnt++)
		{
			Component = &((*Components)[cnt]);

			if (Component->MultiSymbolNr == cnt2)
			{
				sprintf(str2, "%s%d", Component->Reference, Component->RefNum);
				cnt3 = 0;

				while ((cnt3 < MultipleSymbols[cnt2].NrSymbInterfaceNames)
				        && (stricmpUTF8(MultipleSymbols[cnt2].SymbInterfaceNames[cnt3], Component->SymbolName) != 0))
					cnt3++;

				if (cnt3 < MultipleSymbols[cnt2].NrSymbInterfaceNames)
				{
					cnt4 = 0;

					while ((cnt4 < NrUsedMultiPartRefs) && (stricmpUTF8(str2, UsedMultiPartRefs[cnt4]) != 0))
						cnt4++;

					if (NrUsedMultiPartRefs < 4095)
					{
						MultiplePartsCodeRef[cnt4][cnt3]++;

						if (cnt4 == NrUsedMultiPartRefs)
						{
							strcpy(UsedMultiPartRefs[cnt4], str2);
							NrUsedMultiPartRefs++;
						}
					}
				}

			}
		}

		if (MultipleSymbols[cnt2].NrSymbInterfaceNames > 1)
		{
			ComponentError2 = 0;

			for (cnt = 0; cnt < NrUsedMultiPartRefs; cnt++)
			{
				for (cnt3 = 0; cnt3 < MultipleSymbols[cnt2].NrSymbInterfaceNames; cnt3++)
				{
					switch (MultiplePartsCodeRef[cnt][cnt3])
					{
					case 0:
						sprintf(str, SC(87, "Warning symbol %s of component %s does not exist\r\n"),
						        MultipleSymbols[cnt2].SymbInterfaceNames[cnt3], UsedMultiPartRefs[cnt]);
						AddMessage(str);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
						break;

					case 2:
					case 3:
					case 4:
					case 5:
					case 6:
					case 7:
					case 8:
						ComponentError = 1;
						sprintf(str, SC(88, "Error symbol %s of component %s exists more than once\r\n"),
						        MultipleSymbols[cnt2].SymbInterfaceNames[cnt3], UsedMultiPartRefs[cnt]);
						AddMessage(str);
//              SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
						break;
					}
				}
			}
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	for (cnt = 0; cnt < NrUsableComps; cnt++)
	{
		Component = &((*Components)[cnt]);

		switch (Component->Error)
		{
		case 5:
		case 7:
		case 10:
		case 20:
			if (InRangeSpecial(Component->x, -10000.0, 1.0))
				str3[0] = 0;
			else
				sprintf(str3, " [%.1f,%.1f]", Component->x, Component->y);

			if (Component->Reference[0])
				sprintf(str2, "%s%d (%s)", Component->Reference, Component->RefNum, Component->SymbolName);
			else
				sprintf(str2, " (%s)", Component->SymbolName);

			sprintf(str, SC(89, "Error in component "));
			strcat(str, str2);
			strcat(str, str3);
			strcat(str, "\r\n");
			AddMessage(str);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
			ComponentError = 1;
			break;
		}
	}

	if (ComponentError)
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 Annotate(int32 mode, int32 MaxNrRefsPerSheet)
{
	int32 res = 0;

	if (GetComponentsSheets(0) == -1)
		return -10;

	switch (mode & 3)
	{
	case 0:
		res = AnnotateRestart(((mode & 4) >> 1) + 0, MaxNrRefsPerSheet);
		break;

	case 1:
		res = AnnotateAppend(((mode & 4) >> 1) + 0, MaxNrRefsPerSheet);
		break;

	case 2:
		res = AnnotateRestart(((mode & 4) >> 1) + 1, MaxNrRefsPerSheet);
		break;

	case 3:
		res = AnnotateAppend(((mode & 4) >> 1) + 1, MaxNrRefsPerSheet);
		break;
	}

	DeAllocateMemTemp2();
	return res;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SetCompRefLayout(int32 CompIndex, LPSTR NewReference, int32 LayoutMode)
{
	int32 pos, Designfp, res, result;
	char str[MAX_LENGTH_STRING], OldRef[MAX_LENGTH_STRING];
#ifdef _DEBUG
	char str3[MAX_LENGTH_STRING];
#endif
	CompRecord *LayoutComp;
	OldCompRecord *OldLayoutComp;

	memset(&str, 0, sizeof(str));
	strcpy(str, NewReference);

	if (strstr((char *) TempMem2, str))
		return -3;

	sprintf((char *) &TempMem2[UsedLayoutCompRefPtr], "%s ", str);
	UsedLayoutCompRefPtr += strlen(str) + 1;

	if (LayoutDesign.NrComps == 0)
		return -1;

	pos = sizeof(LayoutDesign) + sizeof(int32) * (int) LayoutDesign.NrComps;

	res = -1;
	memset(OldRef, 0, sizeof(OldRef));

	if (LayoutMode == 1)
	{
		OldLayoutComp = (OldCompRecord *) & (LayoutCompsMem[(*LayoutComps)[CompIndex]]);

//    if (OldLayoutComp->Info2==0) {
		if ((Designfp = FileOpenUTF8(PCBFile)) == -1)
			return -1;

		pos += (*LayoutComps)[CompIndex];
		pos += offsetof(OldCompRecord, Name);
		FileSeek(Designfp, pos);

		if (FileCurrentPointer(Designfp) == pos)
		{
			FileRead(Designfp, &OldRef, sizeof(OldLayoutComp->Name), &result);
			FileSeek(Designfp, pos);
			res = FileWrite(Designfp, &str, sizeof(OldLayoutComp->Name), &result);
#ifdef _DEBUG

			if (strnicmp(str, "RP", 2) == 0)
			{
				sprintf(str3, "Comp %s -> %s\r\n", OldRef, str);
				AddMessage(str3);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str3);
			}

#endif
		}

		FileClose(Designfp);

		if (res < 0)
			return -2;

		OldLayoutComp->Info2 = 1;
//    }
	}
	else
	{
		LayoutComp = (CompRecord *) & (LayoutCompsMem[(*LayoutComps)[CompIndex]]);

//    if (LayoutComp->Info2==0) {
		if ((Designfp = FileOpenUTF8(PCBFile)) == -1)
			return -1;

		pos += (*LayoutComps)[CompIndex];
		pos += offsetof(CompRecord, Name);
		FileSeek(Designfp, pos);

		if (FileCurrentPointer(Designfp) == pos)
		{
			FileRead(Designfp, &OldRef, sizeof(LayoutComp->Name), &result);
			FileSeek(Designfp, pos);
			res = FileWrite(Designfp, &str, sizeof(LayoutComp->Name), &result);
#ifdef _DEBUG

			if (strnicmp(str, "RP", 2) == 0)
			{
				sprintf(str3, "Comp %s -> %s\r\n", OldRef, str);
				AddMessage(str3);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str3);
			}

#endif
		}

		FileClose(Designfp);

		if (res < 0)
			return -2;

		LayoutComp->Info2 = 1;
//    }
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ChangeComponentReference(ComponentRecord * Component, int32 SheetNr, int32 PartNr, int fp, int32 mode)
{
	char str[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], OldRef[32];
	int32 InstPos, res, result, PartNr2, cnt, ok, LayoutMode;
	CompRecord *LayoutComp;
	OldCompRecord *OldLayoutComp;


	LayoutMode = 0;

	if ((mode & 2) == 2)
	{
		if ((strcmp(LayoutDesign.Identification, PCBCode) == 0) || (strcmp(LayoutDesign.Identification, PCBCode2) == 0))
			LayoutMode = 1;
	}

	FileSeek(fp, 0);
	res = FileRead(fp, &Design, sizeof(DesignRecord), &result);

	PartNr2 = PartNr;

	memset(&str, 0, sizeof(str));

	switch (Component->Info & (16 + 8))
	{
	case REF_WITH_LEADING_ZERO:
		sprintf(str, "%s0%i", Component->Reference, Component->RefNum);
		break;

	case REF_WITH_TWO_LEADING_ZEROS:
		sprintf(str, "%s00%i", Component->Reference, Component->RefNum);
		break;

	case REF_WITH_THREE_LEADING_ZEROS:
		sprintf(str, "%s000%i", Component->Reference, Component->RefNum);
		break;

	default:
		sprintf(str, "%s%i", Component->Reference, Component->RefNum);
		break;
	}

#ifdef _DEBUG

	if (stricmp(str, "U0") == 0)
		ok = 1;

#endif

	if (strlen(str) < sizeof(NewInstance.Reference))
	{
		if ((mode & 1) == 1)
		{
			sprintf(str3, SC(90, "Append reference %s\r\n"), str);
			AddMessage(str3);
//      SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str3);
		}

		if ((stricmp(Design.Identification, SheetCode3) == 0) || (stricmp(Design.Identification, SheetCode4) == 0)
		        || (stricmp(Design.Identification, SheetCode5) == 0))
		{
			if ((stricmp(Design.Identification, SheetCode3) == 0) || (stricmp(Design.Identification, SheetCode4) == 0))
				InstPos = Component->InstanceNr * sizeof(OldInstanceRecord);
			else
				InstPos = Component->InstanceNr * sizeof(InstanceRecord);
		}
		else
			InstPos = Component->InstanceNr * sizeof(OldOldInstanceRecord);

		InstPos += Sheets[SheetNr].InstancePos;
		FileSeek(fp, InstPos);
		memset(OldRef, 0, sizeof(OldRef));

		if (FileCurrentPointer(fp) == InstPos)
		{
			res = FileRead(fp, &OldRef, sizeof(NewInstance.Reference), &result);
			FileSeek(fp, InstPos);

			if ((mode & 2) == 2)
			{
				for (cnt = 0; cnt < LayoutDesign.NrComps; cnt++)
				{
					if (LayoutMode == 0)
					{
						LayoutComp = (CompRecord *) & (LayoutCompsMem[(*LayoutComps)[cnt]]);

						if ((LayoutComp->Info2 == 0) && (stricmpUTF8(LayoutComp->Name, OldRef) == 0)
						        && (stricmpUTF8(LayoutComp->ShapeName, Component->Geometry) == 0))
						{
							ok = 1;
							SetCompRefLayout(cnt, str, 0);
						}
					}
					else
					{
						OldLayoutComp = (OldCompRecord *) & (LayoutCompsMem[(*LayoutComps)[cnt]]);

						if ((OldLayoutComp->Info2 == 0) && (stricmpUTF8(OldLayoutComp->Name, OldRef) == 0)
						        && (stricmpUTF8(OldLayoutComp->ShapeName, Component->Geometry) == 0))
						{
							ok = 1;
							SetCompRefLayout(cnt, str, 1);
						}
					}
				}
			}

			res = FileWrite(fp, str, sizeof(NewInstance.Reference), &result);
		}

		if (PartNr2 != -1)
		{
			if ((stricmp(Design.Identification, SheetCode3) == 0) || (stricmp(Design.Identification, SheetCode4) == 0)
			        || (stricmp(Design.Identification, SheetCode5) == 0))
			{
				if ((stricmp(Design.Identification, SheetCode3) == 0)
				        || (stricmp(Design.Identification, SheetCode4) == 0))
					InstPos = Component->InstanceNr * sizeof(OldInstanceRecord);
				else
					InstPos = Component->InstanceNr * sizeof(InstanceRecord);
			}
			else
				InstPos = Component->InstanceNr * sizeof(OldOldInstanceRecord);

			InstPos += Sheets[SheetNr].InstancePos2;
			FileSeek(fp, InstPos);

			if (FileCurrentPointer(fp) == InstPos)
				res = FileWrite(fp, &PartNr2, 2, &result);
		}

		Component->Info |= OBJECT_DONE;
	}
	else
	{
		Component->Error = 20;
		return -1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckComponentsLayoutAfterAnnotation(int32 mode)
{
	char str[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	int32 result, cnt, LayoutMode, AnnotationError;
	CompRecord *LayoutComp;
	OldCompRecord *OldLayoutComp;

	AnnotationError = 0;
	LayoutMode = 0;

	if ((mode & 2) == 0)
	{
		if ((strcmp(LayoutDesign.Identification, PCBCode) == 0) || (strcmp(LayoutDesign.Identification, PCBCode2) == 0))
			LayoutMode = 1;
	}

	for (cnt = 0; cnt < LayoutDesign.NrComps; cnt++)
	{
		if (LayoutMode == 0)
		{
			LayoutComp = (CompRecord *) & (LayoutCompsMem[(*LayoutComps)[cnt]]);

			if (LayoutComp->Info2 == 0)
			{
				if (AnnotationError == 0)
				{
					AnnotationError = 1;
					sprintf(str3, SC(91, "The following layout components are not renamed:\r\n\r\n"));
					AddMessage(str3);
				}

				sprintf(str3, "%s\r\n", LayoutComp->Name);
				AddMessage(str3);
			}
		}
		else
		{
			OldLayoutComp = (OldCompRecord *) & (LayoutCompsMem[(*LayoutComps)[cnt]]);

			if (OldLayoutComp->Info2 == 0)
			{
				if (AnnotationError == 0)
				{
					AnnotationError = 1;
					sprintf(str3, SC(91, "The following layout components are not renamed:\r\n\r\n"));
					AddMessage(str3);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str3);
				}

				sprintf(str3, "%s\r\n", OldLayoutComp->Name);
				AddMessage(str3);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str3);
			}
		}
	}

	result = 0;

	if (AnnotationError)
	{
		result = -1;
		strcpy(str, "\r\n");
		AddMessage(str);
//    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)"\r\n");
		strcpy(str, SC(92, "There are layout components not renamed\r\n\r\n"));
		strcat(str, SC(93, "Do you still want to use the modified layout file"));

		if (MessageBoxUTF8(DESIGNWindow, str, SC(16, "Warning"), MB_APPLMODAL | MB_YESNOCANCEL) != IDYES)
		{
			CopyFileUTF8(PCBFileBackup, PCBFile, 0);
			result = 1;
		}

		DeleteFileUTF8(PCBFileBackup);
	}

	return result;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AnnotateRestart(int32 mode, int32 MaxNrRefsPerSheet)
{
	int32 res, result, Designfp, cnt, cnt2, cnt3, cnt4, cnt5, StartNumber, ok, SheetNr, Start, MaxRefs, MaxOccupation, MinDiv;
	SheetIndexRecord SheetStartAnnotate[128];
	uint8 Occupation;
	char FileName[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	ComponentRecord *Component;
	PartNameRecord *PartName;
	int32 Changed;

	if (MessageBoxUTF8(DESIGNWindow, SC(94, "All references will be renamed, Ok to go on"), SC(8, "Message"), MB_OKCANCEL) != IDOK)
		return -2;

	if ((mode & 2) == 2)
		LoadLayout(0);

	sprintf(str3, SC(95, "Restart annotation\r\n"));
	AddMessage(str3);

	Start = 1;

	if ((mode & 1) == 1)
	{
		for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
		{
			SheetMaxCodesRef[cnt] = Start;
			NrSheetCodesPerRef[cnt] = 0;
		}
	}

	for (cnt5 = 0; cnt5 < NrSheets; cnt5++)
	{
		SheetStartAnnotate[cnt5].Index = cnt5;
		SheetStartAnnotate[cnt5].StartNumber = 0;
	}

	for (cnt5 = 0; cnt5 < NrSheets; cnt5++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt5].SheetName);
		sprintf(str3, SC(96, "Sheet\t\t\t\t%s\r\n"), FileName);
		AddMessage(str3);

		if ((Designfp = FileOpenReadOnlyUTF8(FileName)) < 0)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, SC(17, "Error in opening file"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, "Could not read from file", MB_APPLMODAL | MB_OK);
			return -1;
		}

		FileClose(Designfp);

		if (Design.AnnotateStartNumber >= 1000000)
		{
			SheetStartAnnotate[cnt5].StartNumber = Design.AnnotateStartNumber;

			if (Design.AnnotateStartNumber == 1000000)
				SheetStartAnnotate[cnt5].StartNumber++;
		}
	}

	for (cnt5 = 0; cnt5 < NrSheets; cnt5++)
	{
		MinDiv = 1000000000;
		SheetNr = -1;

		for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
		{
			if (SheetStartAnnotate[cnt2].StartNumber >= 1000000)
			{
				StartNumber = SheetStartAnnotate[cnt2].StartNumber % 1000000;

				if (StartNumber - Start <= (MaxNrRefsPerSheet - 1))
				{
					if (StartNumber - Start < MinDiv)
					{
						MinDiv = StartNumber - Start;
						SheetNr = cnt2;
					}
				}
			}
		}

		if (SheetNr == -1)
		{
			for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
			{
				if (SheetStartAnnotate[cnt2].StartNumber != -1)
				{
					SheetNr = cnt2;
					break;
				}
			}
		}

		if (SheetNr == -1)
			continue;

		SheetStartAnnotate[SheetNr].StartNumber = -1;
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[SheetNr].SheetName);
		sprintf(str3, SC(96, "Sheet\t\t\t\t%s\r\n"), FileName);
		AddMessage(str3);

		if ((Designfp = FileOpenUTF8(FileName)) <= 0)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, SC(17, "Error in opening file"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		if (FileRead(Designfp, &Design, sizeof(DesignRecord), &result) == -1)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, "Could not read from file", MB_APPLMODAL | MB_OK);
			return -1;
		}

		if (Design.AnnotateStartNumber >= 1000000)
		{
			StartNumber = Design.AnnotateStartNumber % 1000000;

			if (StartNumber >= Start)
				Start = StartNumber;
			else
			{
				sprintf(str3,
				        SC(225,
				           "On sheet %s annotation can not start at %d\r\nIncrease the max number of refs per sheet"),
				        FileName, StartNumber);
				MessageBoxUTF8(DESIGNWindow, str3, SC(19, "Error"), MB_APPLMODAL | MB_OK);
				FileClose(Designfp);
				return -1;
			}
		}

		if ((mode & 1) == 0)
		{
			for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
			{
				SheetMaxCodesRef[cnt] = Start;
				NrSheetCodesPerRef[cnt] = 0;
			}
		}

// **********************************************************************************
// **********************************************************************************

        // Normal symbols

		for (cnt = NrSpecialComponents; cnt < NrUsableComps; cnt++)
		{
			Component = &((*Components)[cnt]);

			if ((Component->SheetNr == SheetNr) && (Component->Error == 0))
			{
				Component->RefNum = SheetMaxCodesRef[Component->RefCodeNr];

				if (ChangeComponentReference(Component, SheetNr, -1, Designfp, mode & 2) == 0)
				{
					SheetMaxCodesRef[Component->RefCodeNr]++;
					NrSheetCodesPerRef[Component->RefCodeNr]++;
				}
			}
		}

// **********************************************************************************
// **********************************************************************************

        // Multiple Symbols

		ok = 1;

		for (cnt = 0; cnt < NrMultipleSymbolsComps; cnt++)
		{
			Component = &((*Components)[cnt]);
			Occupation = (uint8) (1 << Component->PackagePartNr);
			cnt4 = Component->MultiSymbolNr;
			MaxOccupation = (1 << MultipleSymbols[cnt4].NrSymbInterfaceNames) - 1;

			if ((Component->SheetNr == SheetNr) && (Component->Error == 0) && ((Component->Info & OBJECT_DONE) == 0))
			{
				cnt3 = 0;

				while ((cnt3 < 32) && ((MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3] & Occupation) != 0))
					cnt3++;

// **********************************************************************************
				if (cnt3 < 32)
				{
					MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3] |= Occupation;

					if (MultipleSymbols[cnt4].RefName[cnt3][0] == 0)
					{
						Component->RefNum = SheetMaxCodesRef[Component->RefCodeNr];
						strcpy(MultipleSymbols[cnt4].RefName[cnt3], Component->Reference);
						MultipleSymbols[cnt4].ReferenceNr[cnt3] = (int16) SheetMaxCodesRef[Component->RefCodeNr];

						if (ChangeComponentReference(Component, SheetNr, -1, Designfp, mode & 2) == 0)
						{
							SheetMaxCodesRef[Component->RefCodeNr]++;
							NrSheetCodesPerRef[Component->RefCodeNr]++;
						}
					}
					else
					{
						Component->RefNum = MultipleSymbols[cnt4].ReferenceNr[cnt3];

						if (MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3] == MaxOccupation)
						{
							if (cnt3 < 31)
							{
								memmove(&MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3],
								        &MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3 + 1], 32 - cnt3 - 1);
								memmove(&MultipleSymbols[cnt4].RefName[cnt3], &MultipleSymbols[cnt4].RefName[cnt3 + 1],
								        (32 - cnt3 - 1) * 8);
								memmove(&MultipleSymbols[cnt4].ReferenceNr[cnt3],
								        &MultipleSymbols[cnt4].ReferenceNr[cnt3 + 1], (32 - cnt3 - 1) * 2);
							}

							MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[31] = 0;
							MultipleSymbols[cnt4].RefName[31][0] = 0;
							MultipleSymbols[cnt4].ReferenceNr[31] = -1;
						}

						ChangeComponentReference(Component, SheetNr, -1, Designfp, mode & 2);
					}
				}
			}
		}

// **********************************************************************************
// **********************************************************************************

        // Multiple parts per package

		Changed = 0;

		for (cnt = NrMultipleSymbolsComps; cnt < NrSpecialComponents; cnt++)
		{
			Component = &((*Components)[cnt]);

			if ((Component->SheetNr == SheetNr) && (Component->Error == 0))
				Changed = 1;
		}

		if (Changed)
		{
			for (cnt = NrMultipleSymbolsComps; cnt < NrSpecialComponents; cnt++)
			{
				Component = &((*Components)[cnt]);

				if ((Component->SheetNr == SheetNr) && (Component->Error == 0))
				{
					PartName = &((*PartNames)[Component->MultiPartNr]);

					if (PartName->UsedParts == 1)
					{	// Start new reference
						Component->RefNum = SheetMaxCodesRef[Component->RefCodeNr];
						PartName->LastRef = SheetMaxCodesRef[Component->RefCodeNr];
						SheetMaxCodesRef[Component->RefCodeNr]++;
						NrSheetCodesPerRef[Component->RefCodeNr]++;
					}
					else
						Component->RefNum = PartName->LastRef;

					ChangeComponentReference(Component, SheetNr, PartName->UsedParts, Designfp, mode & 2);
					PartName->UsedParts++;

					if (PartName->UsedParts == (PartName->PartsPerPackage + 1))
						PartName->UsedParts = 1;
				}
			}
		}

// **********************************************************************************
// **********************************************************************************

		if ((mode & 1) == 0)
		{
			MaxRefs = 0;

			for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
				MaxRefs = max(MaxRefs, NrSheetCodesPerRef[cnt]);

			if (Start == 1)
				MaxRefs = max(1, (MaxRefs + MaxNrRefsPerSheet) / MaxNrRefsPerSheet) * MaxNrRefsPerSheet - 1;
			else
				MaxRefs = max(1, (MaxRefs + (MaxNrRefsPerSheet - 1)) / MaxNrRefsPerSheet) * MaxNrRefsPerSheet;

			Start += MaxRefs;
		}

		FileClose(Designfp);
	}

	//********************** Restart annotation systémové oznaèení ***************************************
	sprintf(str3, SC(98, "Annotation done\r\n"));
	AddMessage(str3);

	if ((mode & 2) == 2)
	{
		res = CheckComponentsLayoutAfterAnnotation(0);

		if (res != 1)
		{
			sprintf(str3, SC(97, "The netlist needs also to be regenerated !\r\n"));
			AddMessage(str3);
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SearchRefNum(ComponentRecord * Component, int32 StartPoint)
{
	int32 cnt;

	cnt = StartPoint + 1;

	if (cnt == 0)
		cnt++;

//  if (cnt<100) cnt=100;
	if ((Component->RefCodeNr < 0) || (Component->RefCodeNr >= NrRefCodes))
		return -1;

	while ((cnt < MAX_NR_CODES_PER_REF) && ((*RefBuf)[Component->RefCodeNr][cnt] == 1))
		cnt++;

	if (cnt < MAX_NR_CODES_PER_REF)
		return cnt;

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AnnotateAppend(int32 mode, int32 MaxNrRefsPerSheet)
{

	int32 res, Designfp, cnt, cnt3, cnt4, SearchedComp, Occupation, MaxOccupation, SheetNr, RefNum, Start, RefCount,
	      PartNr;
	char FileName[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	ComponentRecord *Component;
	int32 Changed, MakeChange;

	if ((mode & 2) == 2)
		LoadLayout(0);

	sprintf(str3, SC(99, "Appending annotation\r\n"));
	AddMessage(str3);

	AllocateSpecialMem(MEM_ANNOTATE, NrRefCodes * MAX_NR_CODES_PER_REF, (void *) &RefBuf);
	RefCount = 0;

	for (cnt = 0; cnt < NrUsableComps; cnt++)
	{
		Component = &((*Components)[cnt]);

		if (Component->Error == 0)
		{
			if ((Component->RefCodeNr >= 0) && (Component->RefCodeNr < NrRefCodes))
			{
				if ((Component->RefNum > 0) && (Component->RefNum < MAX_NR_CODES_PER_REF))
					(*RefBuf)[Component->RefCodeNr][Component->RefNum] = 1;
				else
					RefCount++;
			}
			else
				Component->Error = 20;
		}
	}

	if (RefCount == 0)
		return 0;

// **********************************************************************************
// **********************************************************************************

	cnt4 = 0;
	Start = 1;

	if ((mode & 1) == 1)
	{
		for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
		{
			SheetMaxCodesRef[cnt] = -1;
			SheetMinCodesRef[cnt] = 100000;
		}

		for (cnt = 0; cnt < NrUsableComps; cnt++)
		{
			Component = &((*Components)[cnt]);

			if (Component->Error == 0)
			{
				if (Component->RefNum == -1)
					Changed = 1;
				else
					AddSheetRefCodes(Component);
			}
		}
	}

// **********************************************************************************
	for (SheetNr = 0; SheetNr < NrSheets; SheetNr++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[SheetNr].SheetName);
		sprintf(str3, SC(96, "Sheet %s\r\n"), FileName);
		AddMessage(str3);

		if ((Designfp = FileOpenUTF8(FileName)) < 0)
		{
			MessageBoxUTF8(DESIGNWindow, FileName, SC(17, "Error in opening file"), MB_APPLMODAL | MB_OK);
			return -1;
		}

// **********************************************************************************
// **********************************************************************************

        // Normal symbols

		if ((mode & 1) == 0)
		{
			for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
			{
				SheetMaxCodesRef[cnt] = -1;
				SheetMinCodesRef[cnt] = 100000;
			}

			for (cnt = NrSpecialComponents; cnt < NrUsableComps; cnt++)
			{
				Component = &((*Components)[cnt]);

				if ((Component->SheetNr == SheetNr) && (Component->Error == 0))
				{
					if (Component->RefNum == -1)
						Changed = 1;
					else
						AddSheetRefCodes(Component);
				}
			}
		}

		for (cnt = NrSpecialComponents; cnt < NrUsableComps; cnt++)
		{
			Component = &((*Components)[cnt]);
#ifdef _DEBUG

			if (cnt == 86)
				res = 1;

#endif

			if (Component->SheetNr == SheetNr)
			{
				if ((Component->Error == 0) && (Component->RefNum == -1))
				{
					if ((RefNum = SearchRefNum(Component, SheetMaxCodesRef[Component->RefCodeNr])) != -1)
					{
						Component->RefNum = RefNum;

//                      SheetMaxCodesRef[Component->RefCodeNr]=RefNum;

						AddSheetRefCodes(Component);
						(*RefBuf)[Component->RefCodeNr][Component->RefNum] = 1;
						ChangeComponentReference(Component, SheetNr, -1, Designfp, mode | 1);
					}
					else
						Component->Error = 20;
				}
			}
		}

// **********************************************************************************
// **********************************************************************************

        // Multiple Symbols

		if ((mode & 1) == 0)
		{
			for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
			{
				SheetMaxCodesRef[cnt] = -1;
				SheetMinCodesRef[cnt] = 100000;
			}
		}

		Changed = 0;

		for (cnt = 0; cnt < NrMultipleSymbolsComps; cnt++)
		{
			Component = &((*Components)[cnt]);

			if ((Component->SheetNr == SheetNr) && (Component->Error == 0))
			{
				if (Component->RefNum == -1)
					Changed = 1;
				else
				{
					if ((mode & 1) == 0)
						AddSheetRefCodes(Component);
				}
			}
		}

		if (Changed)
		{
// **********************************************************************************
// **********************************************************************************
			for (cnt = 0; cnt < NrMultipleSymbolsComps; cnt++)
			{
				Component = &((*Components)[cnt]);
				Occupation = (1 << Component->PackagePartNr);
				cnt4 = Component->MultiSymbolNr;

				if ((Component->SheetNr == SheetNr) && (Component->Error == 0) && ((Component->Info & OBJECT_DONE) == 0)
				        && (Component->RefNum != -1))
				{
					cnt3 = 0;

					while ((cnt3 < 32)
					        && ((stricmpUTF8(MultipleSymbols[cnt4].RefName[cnt3], Component->Reference) != 0)
					            || (MultipleSymbols[cnt4].ReferenceNr[cnt3] != Component->RefNum)))
						cnt3++;

					if (cnt3 == 32)
					{
						cnt3 = 0;
						memmove(&MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3 + 1],
						        &MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3], 32 - cnt3 - 1);
						memmove(&MultipleSymbols[cnt4].RefName[cnt3 + 1], &MultipleSymbols[cnt4].RefName[cnt3],
						        (32 - cnt3 - 1) * 8);
						memmove(&MultipleSymbols[cnt4].ReferenceNr[cnt3 + 1], &MultipleSymbols[cnt4].ReferenceNr[cnt3],
						        (32 - cnt3 - 1) * 2);
						MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[0] = 0;
						strcpy(MultipleSymbols[cnt4].RefName[0], Component->Reference);
						MultipleSymbols[cnt4].ReferenceNr[0] = (int16) Component->RefNum;
					}

					MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3] |= Occupation;
				}
			}

// **********************************************************************************
// **********************************************************************************
			for (cnt = 0; cnt < NrMultipleSymbolsComps; cnt++)
			{
				Component = &((*Components)[cnt]);
				Occupation = (1 << Component->PackagePartNr);
				cnt4 = Component->MultiSymbolNr;
				MaxOccupation = (1 << MultipleSymbols[cnt4].NrSymbInterfaceNames) - 1;

				if ((Component->SheetNr == SheetNr) && (Component->Error == 0) && ((Component->Info & OBJECT_DONE) == 0)
				        && (Component->RefNum == -1))
				{
// **********************************************************************************
					cnt3 = 0;

					while ((cnt3 < 32)
					        && ((MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3] & Occupation) != 0))
						cnt3++;

// **********************************************************************************
					if (cnt3 < 32)
					{
						MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3] |= Occupation;

						if (MultipleSymbols[cnt4].RefName[cnt3][0] == 0)
						{
							if ((RefNum = SearchRefNum(Component, SheetMaxCodesRef[Component->RefCodeNr])) != -1)
							{
								Component->RefNum = RefNum;
								strcpy(MultipleSymbols[cnt4].RefName[cnt3], Component->Reference);
								MultipleSymbols[cnt4].ReferenceNr[cnt3] = (int16) RefNum;
								SheetMaxCodesRef[Component->RefCodeNr] = RefNum;
								ChangeComponentReference(Component, SheetNr, -1, Designfp, mode | 1);
								(*RefBuf)[Component->RefCodeNr][Component->RefNum] = 1;
							}
							else
								Component->Error = 20;
						}
						else
						{
							Component->RefNum = MultipleSymbols[cnt4].ReferenceNr[cnt3];

							if (MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3] == MaxOccupation)
							{
								if (cnt3 < 31)
								{
									memmove(&MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3],
									        &MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[cnt3 + 1],
									        32 - cnt3 - 1);
									memmove(&MultipleSymbols[cnt4].RefName[cnt3],
									        &MultipleSymbols[cnt4].RefName[cnt3 + 1], (32 - cnt3 - 1) * 8);
									memmove(&MultipleSymbols[cnt4].ReferenceNr[cnt3],
									        &MultipleSymbols[cnt4].ReferenceNr[cnt3 + 1], (32 - cnt3 - 1) * 2);
								}

								MultipleSymbols[cnt4].SymbInterfaceNamesOccupation[31] = 0;
								MultipleSymbols[cnt4].RefName[31][0] = 0;
								MultipleSymbols[cnt4].ReferenceNr[31] = -1;
							}

							AddSheetRefCodes(Component);
							(*RefBuf)[Component->RefCodeNr][Component->RefNum] = 1;
							ChangeComponentReference(Component, SheetNr, -1, Designfp, mode | 1);
						}
					}
				}
			}
		}

// **********************************************************************************
// **********************************************************************************

        // Multiple parts per package

		if ((mode & 1) == 0)
		{
			for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
			{
				SheetMaxCodesRef[cnt] = -1;
				SheetMinCodesRef[cnt] = 100000;
			}
		}

		Changed = 0;

		for (cnt = NrMultipleSymbolsComps; cnt < NrSpecialComponents; cnt++)
		{
			Component = &((*Components)[cnt]);

			if ((Component->SheetNr == SheetNr) && (Component->Error == 0))
			{
				if (Component->RefNum == -1)
					Changed = 1;
				else
				{
					if ((mode & 1) == 0)
						AddSheetRefCodes(Component);
				}
			}
		}

		if (Changed)
		{
			for (cnt = NrMultipleSymbolsComps; cnt < NrSpecialComponents; cnt++)
			{
				Component = &((*Components)[cnt]);
#ifdef _DEBUG

				if (stricmp(Component->Geometry, "MOUTINGHOLE") == 0)
					res = 1;

#endif

				if ((Component->SheetNr == SheetNr) && (Component->Error == 0) && (Component->RefNum == -1))
				{
					MakeChange = 1;
					SearchedComp =
					    SearchMultiPartComp(Component->MultiPartNr, Component->NrPartsPerPackage,
					                        SheetMaxCodesRef[Component->RefCodeNr]);

					if (SearchedComp == -1)
					{
						if ((RefNum = SearchRefNum(Component, SheetMaxCodesRef[Component->RefCodeNr])) != -1)
						{
							Component->RefNum = RefNum;
							PartNr = 1;
							Component->PackagePartNr = PartNr;
							(*RefBuf)[Component->RefCodeNr][Component->RefNum] = 1;
							AddSheetRefCodes(Component);
							ChangeComponentReference(Component, SheetNr, PartNr, Designfp, mode | 1);
						}
						else
							Component->Error = 20;
					}
					else
					{
						RefNum = SearchedComp >> 12;
						PartNr = (int16) ((SearchedComp & 0xfff) + 1);
						Component->PackagePartNr = PartNr;
						Component->RefNum = RefNum;
						AddSheetRefCodes(Component);
						(*RefBuf)[Component->RefCodeNr][Component->RefNum] = 1;
						ChangeComponentReference(Component, SheetNr, PartNr, Designfp, mode | 1);
					}
				}
			}
		}

		FileClose(Designfp);
	}

	//******************** Appending annotation oznaèení podle schématu **********************************
	sprintf(str3, SC(98, "Annotation done\r\n"));
	AddMessage(str3);

	if ((mode & 2) == 2)
		
		res = CheckComponentsLayoutAfterAnnotation(0);

	return 0;
}

//************************************************************************************************************************************
//************************************************************************************************************************************
//************************************************************************************************************************************

int32 CheckRefInstances(int32 Mode)
{
	int32 lengte, cnt, cnt2, RefNum, SheetError, TotalErrors, NeedAnnotation;
	char FileName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], SymbolName[MAX_LENGTH_STRING];
	InstanceRecord *Instance;
	LPSTR Ref;

	NeedAnnotation = 0;
	TotalErrors = 0;

	sprintf(str, SC(100, "Checking references\r\n"));
	AddMessage(str);

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		Sheets[cnt2].Info = 0;
		LoadSheetInMemory(cnt2, 1);
		SheetError = 0;
		
		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			
			if ((Instance->Info & (SHEET_SYMBOL | OBJECT_PROTECTED)) == 0)
			{
				Ref = Instance->Reference;
				lengte = strlen(Ref);
				
				if (lengte > 0)
				{
					if (lengte == 1)
					{
						  SheetError = 1;
						  sprintf(str, SC(101, "Error in reference %s\r\n"), Ref);
						  AddMessage(str);
					}

					if ((!SheetError) && (!isalpha(Ref[0])))
					{
						  SheetError = 1;
						  sprintf(str, SC(101, "Error in reference %s\r\n"), Ref);
						  AddMessage(str);
					}

					if ((!SheetError) && (!isdigit(Ref[lengte - 1])) && (Ref[lengte - 1] != '?'))
					{
						  SheetError = 1;
						  sprintf(str, SC(101, "Error in reference %s\r\n"), Ref);
						  AddMessage(str);
					}

					if ((!SheetError) && (Ref[lengte - 1] == '?'))
				    {
					      Sheets[cnt2].Info = 1;
					 	  NeedAnnotation = 1;
					}
					else
					{
						if (GetRefCode2(Ref, str, &RefNum) == -1)
						{
							SheetError = 1;
							
							if (strcmp(Ref, "?") == 0)
							{                                                                                       //Instance->SymbolName,Value,Reference
								 sprintf(str, SC(102, "Error in reference %s. The %s character is not allowed!\r\n"), Instance->Reference, Ref);
							}
							else
								sprintf(str, SC(101, "Error in reference %s\r\n"), Ref);
							    AddMessage(str);
						}
						else
						{
							if (RefNum >= MAX_NR_CODES_PER_REF - 1)
							{
								  SheetError = 1;
								  sprintf(str, SC(104, "Max of %i component numbers per reference family (%s) in file %s\r\n"),
								          MAX_NR_CODES_PER_REF - 1, Ref, FileName);
								  AddMessage(str);
							}
							else
							{
								if (RefNum < 0)
								{
									   SheetError = 1;
									   sprintf(str, SC(105, "Negative component number (%s) in file %s\r\n"), Ref, FileName);
									   AddMessage(str);
								}
							}
						}
			        }
				}
			}
		}
		
		if (SheetError)
			TotalErrors = 1;
	}

	if ((Mode == 0) && (TotalErrors))
	{
		return -1;
	}

	if (NeedAnnotation)
	{
		for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
		{
			strcpy(FileName, SheetDir);
			strcat(FileName, Sheets[cnt2].SheetName);
			strcat(FileName, ".sch");

			if (Sheets[cnt2].Info == 1)
			{
				sprintf(str, SC(106, "Sheet %s needs to be annotated\r\n"), FileName);
				AddMessage(str);
			}
		}

		if (Mode == 0)
			return 1;
	}

	if (Mode == 1)
		return 0;

	if (GetComponentsSheets(1) == -1)
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetSheetByReference(LPSTR Reference)
{
	int32 cnt, cnt2;
	char FileName[MAX_LENGTH_STRING];

	InstanceRecord *Instance;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);

		if (LoadSheetInMemory(cnt2, 1) < 0)
			return -2;

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (stricmpUTF8(Instance->Reference, Reference) == 0)
			{
				DeallocateMem();
				return cnt2;
			}
		}
	}

	DeallocateMem();
	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetSheetByPartnr(LPSTR Partnr)
{
	int32 cnt, cnt2;
	char FileName[MAX_LENGTH_STRING];

	InstanceRecord *Instance;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);

		if (LoadSheetInMemory(cnt2, 1) < 0)
			return -2;

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);

			if (stricmpUTF8(Instance->PartNr, Partnr) == 0)
			{
				DeallocateMem();
				return cnt2;
			}
		}
	}

	DeallocateMem();
	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WriteCompLine(LPSTR fpstr, int32 CompNr, int32 MaxComps, int32 count, int32 NrColumns, int32 mode)
{
	ComponentRecord *Component;
	char str6[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], *InstanceProperties;
	int32 ok, cnt, cnt2, FoundSystemColumn;

	if ((CompNr >= 0) && (CompNr < MaxComps))
	{
		Component = &((*Components)[CompNr]);
		InstanceProperties = Component->Properties;
		/*
		#define BOM2_PLACED_STR_POS       6
		#define BOM2_REFERENCES_STR_POS   8
		#define BOM2_VALUE_STR_POS        19
		#define BOM2_GEOMETRY_STR_POS     49
		#define BOM2_PARTNR_STR_POS       77
		*/

		if ((mode & 8) == 8)
		{
			switch (Component->Info & (16 + 8))
			{
			case REF_WITH_LEADING_ZERO:
				sprintf(fpstr, "%s0%i", Component->Reference, Component->RefNum);
				break;

			case REF_WITH_TWO_LEADING_ZEROS:
				sprintf(fpstr, "%s00%i", Component->Reference, Component->RefNum);
				break;

			case REF_WITH_THREE_LEADING_ZEROS:
				sprintf(fpstr, "%s000%i", Component->Reference, Component->RefNum);
				break;

			default:
				sprintf(fpstr, "%s%i", Component->Reference, Component->RefNum);
				break;
			}

			return;
		}

#ifdef _DEBUG
		sprintf(str6, "%s%i", Component->Reference, Component->RefNum);

		if (stricmp(str6, "J103") == 0)
			ok = 1;

		if (stricmp(Component->Value, "CS8402") == 0)
			ok = 1;

#endif
		fpstr[0] = 0;

		for (cnt = 0; cnt < NrColumns; cnt++)
		{
			FoundSystemColumn = 0;
			str6[0] = 0;
			cnt2 = BOMInfo.ColumnsUsed[cnt];

			if (stricmpUTF8(BOMInfo.ColumnStr[cnt2], SC(29, "Position")) == 0)
			{
				sprintf(str6, "%i", count);
				FoundSystemColumn = 1;
			}

			if (stricmpUTF8(BOMInfo.ColumnStr[cnt2], SC(30, "Reference")) == 0)
			{
				if (Component->RefNum >= 0)
				{
					switch (Component->Info & (16 + 8))
					{
					case REF_WITH_LEADING_ZERO:
						sprintf(str6, "%s0%i", Component->Reference, Component->RefNum);
						break;

					case REF_WITH_TWO_LEADING_ZEROS:
						sprintf(str6, "%s00%i", Component->Reference, Component->RefNum);
						break;

					case REF_WITH_THREE_LEADING_ZEROS:
						sprintf(str6, "%s000%i", Component->Reference, Component->RefNum);
						break;

					default:
						sprintf(str6, "%s%i", Component->Reference, Component->RefNum);
						break;
					}
				}
				else
					sprintf(str6, "?%?", Component->Reference);

				FoundSystemColumn = 1;
			}

			if (stricmpUTF8(BOMInfo.ColumnStr[cnt2], SC(31, "Value")) == 0)
			{
				if ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == 0)
					strcpy(str6, Component->Value);
				else
					strcpy(str6, Component->InterfaceName);

				FoundSystemColumn = 1;
				ok = 1;
			}

			if (stricmpUTF8(BOMInfo.ColumnStr[cnt2], SC(32, "Geometry")) == 0)
			{
				FoundSystemColumn = 1;
				strcpy(str6, Component->Geometry);
			}

			if (stricmpUTF8(BOMInfo.ColumnStr[cnt2], SC(33, "PartNr")) == 0)
			{
#ifdef _DEBUG
				sprintf(str6, "%s%i", Component->Reference, Component->RefNum);

				if (stricmp(str6, "J103") == 0)
					ok = 1;

				if (stricmp(Component->Value, "CS8402") == 0)
					ok = 1;

#endif
				FoundSystemColumn = 1;
				strcpy(str6, Component->PartNr);
			}

			if (stricmpUTF8(BOMInfo.ColumnStr[cnt2], SC(110, "Description")) == 0)
			{
				FoundSystemColumn = 1;
				strcpy(str6, Component->Description);
			}

			if (!FoundSystemColumn)
			{
				if (GetInstanceAttribute(InstanceProperties, BOMInfo.ColumnStr[cnt2], str6, 1) != 0)
					str6[0] = 0;
				else
					ok = 1;
			}

			BOMInfo.ColumnWidth[cnt] = max(BOMInfo.ColumnWidth[cnt], (int) strlen(str6));
			strcat(fpstr, str6);
			strcat(fpstr, "\t");
		}
	}
	else
	{
		strcpy(str, SC(108, "Comp error"));
		AddMessage(str);
//    SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)(LPSTR)SC(108,"Comp error"));
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

double GetValueFromCompValue(LPSTR CompValueStr, int32 mode)
{
	double value = 0.0;
	char NumStr1[MAX_LENGTH_STRING], NumStr2[MAX_LENGTH_STRING], ValueStr[MAX_LENGTH_STRING];
	int32 num1, num2, cnt, cnt2, length;
	char separatorChar = 0;

	num1 = 0;
	num2 = 0;
	cnt2 = 0;
	length = strlen(CompValueStr);
	memset(NumStr1, 0, sizeof(NumStr1));
	memset(NumStr2, 0, sizeof(NumStr2));
	cnt = 0;

	while ((cnt < length) && (isdigit(CompValueStr[cnt])))
		cnt++;

	if (cnt < length - 1)
	{
		strncpy(NumStr1, CompValueStr, cnt);
		separatorChar = CompValueStr[cnt];

		if (isdigit(CompValueStr[cnt + 1]))
		{
			cnt2 = cnt + 1;

			while ((cnt2 < length) && (isdigit(CompValueStr[cnt2])))
				cnt2++;

			strncpy(NumStr2, (LPSTR) & CompValueStr[cnt + 1], cnt2 - (cnt + 1));
		}

		switch (separatorChar)
		{
		case '.':
		case ',':
			if (cnt2 < length)
				separatorChar = CompValueStr[cnt2];
			else
				separatorChar = 0;

			break;
		}

		sprintf(ValueStr, "%s.%s", NumStr1, NumStr2);
		value = atof(ValueStr);
	}
	else
	{
		if (cnt == length - 1)
		{
			strncpy(NumStr1, CompValueStr, cnt);
			value = atoi(NumStr1);
			separatorChar = CompValueStr[cnt];
		}
		else
			value = atoi(CompValueStr);
	}

	switch (separatorChar)
	{
	case 'n':
	case 'N':
		value /= 1e9;
		break;

	case 'p':
	case 'P':
		value /= 1e12;
		break;

	case 'u':
	case 'U':
		value /= 1000000.0;
		break;

	case 'k':
	case 'K':
		value *= 1000.0;
		break;

	case 'm':
	case 'M':
		switch (mode)
		{
		case 0:				// Resistor
			value *= 1000000.0;
			break;

		case 1:				// Capacitor
			value /= 1000.0;
			break;

		default:				// Rest
			value *= 1000000.0;
			break;
		}

		break;

	default:
		break;
	}

	return value;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

void ConvertTabToComma(LPSTR str)
{
	uint32 cnt;

	for (cnt = 0; cnt < strlen(str); cnt++)
	{
		if (str[cnt] == '\t')
			str[cnt] = ',';
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


int32 QsortCompare(const char *arg1, const char *arg2)
{
	int32 l1 = strchr(arg1, '\t') - arg1;
	int32 l2 = strchr(arg2, '\t') - arg2;
	return strncmp(arg1, arg2, min(l1, l2));
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 BillOfMaterials(int32 mode)
{
	int32 cnt, cnt2, cnt3, cnt4, MemPos, fp, fp2, fp3, CompLineCount, ok, ColumnCount =
	    0, FoundNr, RefNum, Index, NrRefComps, FoundCompNrR, FoundCompNrC, RefMode, CompCnt, SymbolNr,
	    FoundSystemColumn, InstMemSize, CharDelimiterMode, length, RefError, ExcludeFromBOM,
	    DoubleRefComponentLineOutputCount, mode2, ComponentTypeCount, NrValueComps, NrGeomComps, NrProperties,
	    NrWriteModes, WriteMode, CompIndex[MAX_NR_CODES_PER_REF], DoubleRefs[MAX_DOUBLE_REFS], NrDoubleRefs,
	    AllCompsPlaced, CompValueIndex[4096], CompGeomIndex[4096];
	double LowestCompValue;
	char SymbolName[MAX_LENGTH_STRING], *CompStr, FileName[MAX_LENGTH_STRING], FileName2[MAX_LENGTH_STRING],
	     str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], RefCompare[80],
	     str4[MAX_LENGTH_STRING], str5[MAX_LENGTH_STRING], PartStr[MAX_LENGTH_STRING], *InstanceAttrBuf, PropertyID[128],
	     PropertyValue[128], ValueStr[MAX_LENGTH_STRING], GeomStr[MAX_LENGTH_STRING], WriteString[MAX_LENGTH_STRING];
#ifdef _DEBUG
	int32 res;
#endif

	SymbolsPos2Record *SymbolPos2;
	SymbolRecord *Symbol;
	ComponentRecord *Component;
	int32 OnlyPowerPins, Stop, Stop2;
	InstanceRecord *Instance;
	ObjectRecord *Object;
	struct tm *today;
	time_t ltime;

//  mode|=4;


	fp = -1;
	fp2 = -1;
	fp3 = -1;
	CompLineCount = 0;
	GetDesignSheets();
	MaxMem = 0;

	for (cnt = 0; cnt < NrSheets; cnt++)
		MaxMem = max(MaxMem, Sheets[cnt].SheetMemSize + Sheets[cnt].SymbolMemSize);

	CompCnt = 0;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		if (LoadSheetInMemory(cnt2, 3) < 0)
			return -2;

		CompCnt += Design.NrInstances;
	}

	InstMemSize = MaxMem + CompCnt * sizeof(PartNameRecord) + CompPartMemSize;

	if ((InstMemSize > DesignBufMemSize) && (AllocateMemDesignBuf(InstMemSize) != 0))
		return -1;

	if (CompCnt + 1 > MaxNrComponents)
		AllocateMemComponents(CompCnt);

	NrComponents = CompCnt;
	CompCnt = 0;
	cnt4 = 0;
	BOMInfo.NrUniqueProperties = 0;
	PartNames = (PartNameArray *) & DesignBuf[MaxMem];

	NrRefCodes = 0;

	for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
		MaxCodesRef[cnt] = -1;

// **********************************************************************************
// **********************************************************************************

	AllCompsPlaced = 1;
	ok = 1;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);
		LoadSheetInMemory(cnt2, 0);
		ok = 1;

		for (cnt = 0; cnt < Design.NrInstances; cnt++)
		{
			Instance = (InstanceRecord *) & ((*Instances)[cnt]);
			strcpy(SymbolName, Instance->SymbolName);
			SymbolNr = Instance->AddNr;;

			if (SymbolNr != -1)
			{
				SymbolPos2 = &((*SymbolsPos2)[SymbolNr]);
				MemPos = (*SymbolsPos2)[SymbolNr].Pos;
				Symbol = (SymbolRecord *) & ((*SymbolsMem)[MemPos]);
				NrObjects = 0;
				InstancePinsToObject(Instance, 0);

				if (((Symbol->Info & (SHEET_SYMBOL)) == 0) && (Instance->Reference[0] != 0)
				        && (stricmp(Instance->Reference, "?") != 0))
				{
					Component = &((*Components)[CompCnt]);
					memset(Component, 0, sizeof(ComponentRecord));
					strcpy(Component->SymbolName, Instance->SymbolName);
#ifdef _DEBUG

					if (stricmp(Instance->Reference, "R2") == 0)
						res = 1;

#endif
					strcpy(Component->PartNr, Instance->PartNr);
					strcpy(Component->InterfaceName, Symbol->InterfaceName);
					strcpy(Component->Value, Instance->Value);
					strcpy(Component->Geometry, Instance->Geometry);
					strcpy(Component->Description, Instance->PartDescription);
					memcpy(Component->Properties, Instance->Properties, sizeof(Instance->Properties));

					ExcludeFromBOM = 0;
					InstanceAttrBuf = (LPSTR) Instance->Properties;
					NrProperties = GetProperties(Instance->Properties, NULL, NULL, 0x40);

					for (cnt3 = 0; cnt3 < NrProperties; cnt3++)
					{
						if ((GetProperties
						        (Instance->Properties, (LPSTR) & PropertyID, (LPSTR) & PropertyValue, 0x20 + cnt3) == 0)
						        && (PropertyID[0] != '~'))
						{
							for (cnt4 = 0; cnt4 < BOMInfo.NrUniqueProperties; cnt4++)
							{
								if (strcmpUTF8(BOMInfo.UniqueProperties[cnt4], PropertyID) == 0)
									break;
							}

							if ((cnt4 == BOMInfo.NrUniqueProperties) && (BOMInfo.NrUniqueProperties < 16))
								strcpy(BOMInfo.UniqueProperties[BOMInfo.NrUniqueProperties++], PropertyID);

							if ((strcmpUTF8(PropertyID, ExcludeInBOMID) == 0)
							        && (strcmpUTF8(PropertyValue, ExcludeInBOMValue) == 0))
								ExcludeFromBOM = 1;
						}
					}

					Component->SymbolInfo = Symbol->Info;
					Component->InstanceNr = cnt;
					Component->SheetNr = cnt2;
					Component->PlacingOption = Instance->PlacingOption;
					Component->Info = 0;

					if (Component->PlacingOption != -1)
						AllCompsPlaced = 0;

					OnlyPowerPins = 1;

					for (cnt3 = 0; cnt3 < NrObjects; cnt3++)
					{
						Object = &((*Objects)[cnt3]);

						switch (Object->ObjectType)
						{
						case SYMBOL_PIN:
							if (Object->Info2 != POWER_CONNECTION)
								OnlyPowerPins = 0;

							break;

						case SYMBOL_PINBUS:
							OnlyPowerPins = 0;
							break;
						}
					}

					if (!OnlyPowerPins)
					{
						strcpy(str, Instance->Reference);
#ifdef _DEBUG

						if (stricmp(str, "R003") == 0)
							ok = 1;

#endif
						cnt3 = 0;
						length = strlen(str);

						while ((cnt3 < length) && (!isdigit(str[cnt3])))
							cnt3++;

						if (str[cnt3] == '0')
						{
							cnt4 = 0;

							while ((cnt3 < length) && (str[cnt3] == '0'))
							{
								cnt3++;
								cnt4++;
							}

							switch (cnt4)
							{
							case 1:
								Component->Info |= REF_WITH_LEADING_ZERO;
								break;

							case 2:
								Component->Info |= REF_WITH_TWO_LEADING_ZEROS;
								break;

							case 3:
								Component->Info |= REF_WITH_THREE_LEADING_ZEROS;
								break;
							}

#ifdef _DEBUG
							ok = 1;
#endif
						}

#ifdef _DEBUG

						if (stricmp(str, "R?") == 0)
							ok = 1;

#endif
						struprUTF8(str);

						if ((Index = GetRefCode(str, str2, &RefNum)) != -1)
						{
							strcpy(Component->Reference, str2);
							Component->RefNum = RefNum;
							Component->RefCodeNr = Index;
							cnt4++;
						}
						else
							Component->Error = 5;
					}
					else
					{
						if (NrObjects > 0)
							Component->Error = 6;
						else
						{
							strcpy(str, Instance->Reference);
							struprUTF8(str);

							if ((Index = GetRefCode(str, str2, &RefNum)) != -1)
							{
								strcpy(Component->Reference, str2);
								Component->RefNum = RefNum;
								Component->RefCodeNr = Index;
								cnt4++;
							}
							else
								Component->Error = 5;
						}
					}

					if ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == 0)
					{
						Component->PackagePartNr = Instance->PackagePartNr;
						Component->NrPartsPerPackage = Symbol->NrPartsPerPackage;
					}

					if (!ExcludeFromBOM)
						CompCnt++;
				}
			}
		}
	}

	if (mode == -1)
	{
		BOMInfo.PropertyIndex = 0;
		BOMInfo.ColumnUsed = 0;
		cnt = 6;
		strcpy(BOMInfo.ColumnStr[0], SC(29, "Position"));
		strcpy(BOMInfo.ColumnStr[1], SC(30, "Reference"));
		strcpy(BOMInfo.ColumnStr[2], SC(31, "Value"));
		strcpy(BOMInfo.ColumnStr[3], SC(32, "Geometry"));
		strcpy(BOMInfo.ColumnStr[4], SC(33, "PartNr"));
		strcpy(BOMInfo.ColumnStr[5], SC(110, "Description"));

		for (cnt4 = 0; cnt4 < BOMInfo.NrUniqueProperties; cnt4++)
			strcpy(BOMInfo.ColumnStr[cnt++], BOMInfo.UniqueProperties[cnt4]);

		BOMInfo.ColumnStr[cnt][0] = 0;

		if (BOMInfo.NrColumnsUsed == 0)
		{
			BOMInfo.ColumnsUsed[0] = 0;
			BOMInfo.ColumnsUsed[1] = 1;
			BOMInfo.ColumnsUsed[2] = 2;
			BOMInfo.ColumnsUsed[3] = 3;
			BOMInfo.ColumnsUsed[4] = 4;
			BOMInfo.NrColumnsUsed = 5;
		}

		return 0;
	}

	if (mode == 0)
	{
		if (BOMInfo.NrColumnsUsed == 0)
		{
			strcpy(BOMInfo.ColumnStr[0], SC(29, "Position"));
			strcpy(BOMInfo.ColumnStr[1], SC(30, "Reference"));
			strcpy(BOMInfo.ColumnStr[2], SC(31, "Value"));
			strcpy(BOMInfo.ColumnStr[3], SC(32, "Geometry"));
			strcpy(BOMInfo.ColumnStr[4], SC(33, "PartNr"));
			BOMInfo.ColumnStr[5][0] = 0;
			BOMInfo.ColumnsUsed[0] = 0;
			BOMInfo.ColumnsUsed[1] = 1;
			BOMInfo.ColumnsUsed[2] = 2;
			BOMInfo.ColumnsUsed[3] = 3;
			BOMInfo.ColumnsUsed[4] = 4;
			BOMInfo.NrColumnsUsed = 5;
		}
	}

// ***************************************************************************************************************
// ***************************************************************************************************************
	if ((mode & 8) == 0)
		CharDelimiterMode = 1;
	else
		CharDelimiterMode = 5;

	AllocateMemTemp(CompCnt * 2 * 512);
	memset(TempMem, 0, CompCnt * 2 * 512);

	for (cnt3 = 0; cnt3 < MAX_NR_BOM_COLUMNS; cnt3++)
		BOMInfo.ColumnWidth[cnt3] = 0;

	mode2 = 0;

	switch (mode & 3)
	{
	case 0:
		sprintf(str2, "%s\\backup\\component.txt", DesignPath);
		CopyFileUTF8(FileName, str2, 0);
		sprintf(FileName, "%s\\component.txt", DesignPath);

		if ((mode & 8) == 0)
		{
			sprintf(FileName2, "%s\\component.tdl", DesignPath);
			sprintf(str2, "%s\\backup\\component.tdl", DesignPath);
			CopyFileUTF8(FileName, str2, 0);
		}
		else
		{
			sprintf(FileName2, "%s\\component.csv", DesignPath);
			sprintf(str2, "%s\\backup\\component.csv", DesignPath);
			CopyFileUTF8(FileName, str2, 0);
		}

		if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
			return -1;

		if ((fp2 = FileOpenWriteUTF8(FileName2)) <= 0)
			return -1;

		break;

	case 1:
	case 2:
	case 3:
		sprintf(FileName, "%s\\%s.bom", DesignPath, LayoutFile);

		if ((mode & 8) == 0)
			sprintf(FileName2, "%s\\%s.tdl", DesignPath, LayoutFile);
		else
			sprintf(FileName2, "%s\\%s.csv", DesignPath, LayoutFile);

		sprintf(str2, "%s\\backup\\%s.bom", DesignPath, LayoutFile);
		CopyFileUTF8(FileName, str2, 0);

		if ((fp = FileOpenWriteUTF8(FileName)) <= 0)
			return -1;

		if ((fp2 = FileOpenWriteUTF8(FileName2)) <= 0)
			return -1;

		WriteLn(fp, SC(52, "Bill Of Materials"));
		WriteLn(fp, "");
		sprintf(str, SC(10, "Design %s"), DesignPath);
		WriteLn(fp, str);
		WriteLn(fp, "");
		time(&ltime);
		today = localtime(&ltime);
		strftime(str2, 100, "%B %d, %Y %X", today);
		WriteLn(fp, str2);
		WriteLn(fp, "");
		break;
	}

	DoubleRefComponentLineOutputCount = -1;
	NrWriteModes = 1;
	RefError = 0;

	if (AllCompsPlaced == 0)
		NrWriteModes++;

	for (WriteMode = 0; WriteMode < NrWriteModes; WriteMode++)
	{
		for (cnt = 0; cnt < MAX_REF_TYPES; cnt++)
		{
			memset(&RefCodes2[cnt], ' ', 8);
			RefCodes2[cnt][7] = 0;
			memmove(&RefCodes2[cnt], &RefCodes[cnt], min(strlen(RefCodes[cnt]), 7));
			RefCodeInfo[cnt] = 0;
		}

		NrDoubleRefs = 0;
		ComponentLineOutputCount = 0;
		ComponentTypeCount = 1;

		for (cnt = 0; cnt < NrRefCodes; cnt++)
		{
			RefCompare[7] = 0;
			memset(&RefCompare, 'Z', 7);
			FoundNr = -1;
			FoundCompNrR = -1;
			FoundCompNrC = -1;
			NrRefComps = 0;

			for (cnt2 = 0; cnt2 < NrRefCodes; cnt2++)
			{
				if (RefCodeInfo[cnt2] == 0)
				{
					if (stricmp(RefCodes[cnt2], "R      ") == 0)
						FoundCompNrR = cnt2;

					if (stricmp(RefCodes[cnt2], "C      ") == 0)
						FoundCompNrC = cnt2;

					if (stricmpUTF8(RefCodes[cnt2], RefCompare) < 0)
					{
						FoundNr = cnt2;
						memmove(&RefCompare, &RefCodes[cnt2], 7);
					}
				}
			}

			//    if (FoundCompNrR!=-1) FoundNr=FoundCompNrR;
			//    else if (FoundCompNrC!=-1) FoundNr=FoundCompNrC;

#ifdef _DEBUG

			if (WriteMode == 1)
				ok = 1;

#endif

			if (FoundNr != -1)
			{
				RefCodeInfo[FoundNr] = 1;

				switch (mode & 3)
				{
				// **********************************************************************************
				// **********************************************************************************
				case 0:		// List per component
					for (cnt2 = 0; cnt2 < MAX_NR_CODES_PER_REF; cnt2++)
						CompIndex[cnt2] = -1;

					cnt3 = 0;

					for (cnt2 = 0; cnt2 < CompCnt; cnt2++)
					{
						Component = &((*Components)[cnt2]);
#ifdef _DEBUG

						if (Component->PlacingOption != -1)
							ok = 1;

#endif

						if (Component->Error == 0)
						{
							if ((((WriteMode == 0) && (Component->PlacingOption == -1))
							        || ((WriteMode == 1) && (Component->PlacingOption != -1)))
							        && (Component->RefCodeNr == FoundNr) && (Component->RefNum > 0)
							        && (Component->RefNum < MAX_NR_CODES_PER_REF))
							{
								if (CompIndex[Component->RefNum] == -1)
								{
									CompIndex[Component->RefNum] = cnt2;
									cnt3++;
								}
								else
								{
									if ((Component->NrPartsPerPackage <= 1)
									        && ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == 0)
									        && (NrDoubleRefs < MAX_DOUBLE_REFS))
										DoubleRefs[NrDoubleRefs++] = cnt2;

									ok = 1;
								}
							}
						}
					}

					ok = 1;

					for (cnt2 = 0; cnt2 < MAX_NR_CODES_PER_REF; cnt2++)
					{
						if (CompIndex[cnt2] != -1)
						{
							Component = &((*Components)[CompIndex[cnt2]]);

							if (((WriteMode == 0) && (Component->PlacingOption == -1))
							        || ((WriteMode == 1) && (Component->PlacingOption != -1)))
							{
#ifdef _DEBUG

								if (WriteMode == 1)
									ok = 1;

#endif
								WriteCompLine((LPSTR) & TempMem[ComponentLineOutputCount * 256], CompIndex[cnt2],
								              CompCnt, ComponentLineOutputCount + 1, BOMInfo.NrColumnsUsed, 0);
								ComponentLineOutputCount++;
							}
						}
					}

					for (cnt2 = 0; cnt2 < CompCnt; cnt2++)
					{
						Component = &((*Components)[cnt2]);
#ifdef _DEBUG

						if (Component->PlacingOption != -1)
							ok = 1;

#endif

						if (Component->Error == 0)
						{
							if ((((WriteMode == 0) && (Component->PlacingOption == -1))
							        || ((WriteMode == 1) && (Component->PlacingOption != -1)))
							        && (Component->RefCodeNr == FoundNr) && (Component->RefNum < 0))
							{
								WriteCompLine((LPSTR) & TempMem[ComponentLineOutputCount * 256], cnt2, CompCnt,
								              ComponentLineOutputCount + 1, BOMInfo.NrColumnsUsed, 4);
								ComponentLineOutputCount++;
								RefError = 1;
							}
						}
					}

					if (cnt == NrRefCodes - 1)
					{
						DoubleRefComponentLineOutputCount = ComponentLineOutputCount;

						for (cnt2 = 0; cnt2 < NrDoubleRefs; cnt2++)
						{
							WriteCompLine((LPSTR) & TempMem[ComponentLineOutputCount * 256], DoubleRefs[cnt2], CompCnt,
							              ComponentLineOutputCount + 1, BOMInfo.NrColumnsUsed, 8);
							ComponentLineOutputCount++;
						}
					}

					break;

				// **********************************************************************************
				// **********************************************************************************
				case 1:		// pos,value,#,geometry
				case 2:		// pos,value,#,geometry,part nr
				case 3:		// pos,value,#,geometry,part nr,description
					for (cnt2 = 0; cnt2 < MAX_NR_CODES_PER_REF; cnt2++)
						CompIndex[cnt2] = -1;

					for (cnt2 = 0; cnt2 < CompCnt; cnt2++)
					{
						Component = &((*Components)[cnt2]);

						if (((WriteMode == 0) && (Component->PlacingOption == -1))
						        || ((WriteMode == 1) && (Component->PlacingOption != -1)))
						{
							if ((Component->Error == 0) && (Component->RefCodeNr == FoundNr) && (Component->RefNum > 0)
							        && (Component->RefNum < MAX_NR_CODES_PER_REF))
							{
#ifdef _DEBUG

								if (Component->RefNum == 5)
									ok = 1;

#endif

								if ((Component->Info & 1) == 0)
								{
									if (CompIndex[Component->RefNum] == -1)
										CompIndex[Component->RefNum] = cnt2;
									else
									{
										if ((Component->NrPartsPerPackage <= 1)
										        && ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == 0)
										        && (NrDoubleRefs < MAX_DOUBLE_REFS))
											DoubleRefs[NrDoubleRefs++] = cnt2;
									}
								}
							}
						}
					}

					ok = 1;

					// **********************************************************************************
					for (cnt2 = 0; cnt2 < MAX_NR_CODES_PER_REF; cnt2++)
					{
						if (CompIndex[cnt2] != -1)
						{
							Component = &((*Components)[CompIndex[cnt2]]);

							if (((WriteMode == 0) && (Component->PlacingOption == -1))
							        || ((WriteMode == 1) && (Component->PlacingOption != -1)))
								CompIndex[NrRefComps++] = CompIndex[cnt2];
						}
					}

					for (cnt2 = 0; cnt2 < NrDoubleRefs; cnt2++)
					{
						CompIndex[NrRefComps++] = DoubleRefs[cnt2];
						Component = &((*Components)[DoubleRefs[cnt2]]);
						Component->Info |= 2;
					}

					for (cnt2 = 0; cnt2 < CompCnt; cnt2++)
					{
						Component = &((*Components)[cnt2]);

						if (((WriteMode == 0) && (Component->PlacingOption == -1))
						        || ((WriteMode == 1) && (Component->PlacingOption != -1)))
						{
							if ((Component->Error == 0) && (Component->RefCodeNr == FoundNr) && (Component->RefNum < 0))
							{
								CompIndex[NrRefComps++] = cnt2;
								Component->Info |= 4;
								RefError = 1;
							}
						}
					}

					// **********************************************************************************
					for (cnt2 = 0; cnt2 < NrRefComps; cnt2++)
					{
						Component = &((*Components)[CompIndex[cnt2]]);
						RefMode = 2;

						if ((islower(Component->Reference[0] == 'r')) && (isdigit(Component->Reference[1])))
						{
							RefMode = 0;	// Resistor
						}

						if ((islower(Component->Reference[0] == 'c')) && (isdigit(Component->Reference[1])))
						{
							RefMode = 1;	// Capacitor
						}

						Component->RealCompValue = (float) GetValueFromCompValue(Component->Value, RefMode);
					}

					// **********************************************************************************
					Stop = 0;

					while (!Stop)
					{
						LowestCompValue = 1e20;
						FoundNr = -1;

						for (cnt2 = 0; cnt2 < NrRefComps; cnt2++)
						{
							Component = &((*Components)[CompIndex[cnt2]]);

							if (((Component->Info & 1) == 0) && (Component->RealCompValue < LowestCompValue))
							{
								LowestCompValue = Component->RealCompValue;
								FoundNr = CompIndex[cnt2];
							}
						}

						// **********************************************************************************
						/*
						   cnt2=0;
						   while ((cnt2<NrRefComps)
						   &&
						   (FoundNr==-1)) {
						   Component=&((*Components)[CompIndex[cnt2]]);
						   if ((Component->Info & 1)==0) FoundNr=CompIndex[cnt2];
						   cnt2++;
						   }
						 */
						if (FoundNr != -1)
						{
							Component = &((*Components)[FoundNr]);
							strcpy(ValueStr, Component->Value);
#ifdef _DEBUG

							if (stricmp(ValueStr, "68K") == 0)
								res = 1;

#endif
							NrValueComps = 0;

							for (cnt2 = 0; cnt2 < NrRefComps; cnt2++)
							{
								Component = &((*Components)[CompIndex[cnt2]]);

								if (((Component->Info & 1) == 0) && (stricmpUTF8(ValueStr, Component->Value) == 0)
								        && (NrValueComps < 4096))
								{
									CompValueIndex[NrValueComps] = CompIndex[cnt2];
									NrValueComps++;
								}
							}

							// **********************************************************************************
							Stop2 = 0;

							while (!Stop2)
							{
								FoundNr = -1;
								cnt2 = 0;

								while ((cnt2 < NrValueComps) && (FoundNr == -1))
								{
									Component = &((*Components)[CompValueIndex[cnt2]]);

									if ((Component->Info & 1) == 0)
										FoundNr = CompValueIndex[cnt2];

									cnt2++;
								}

								if (FoundNr != -1)
								{
									Component = &((*Components)[FoundNr]);
									strcpy(GeomStr, Component->Geometry);
									NrGeomComps = 0;
									PartStr[0] = 0;
									Instance = (InstanceRecord *) & ((*Instances)[Component->InstanceNr]);

									for (cnt2 = 0; cnt2 < NrValueComps; cnt2++)
									{
										Component = &((*Components)[CompValueIndex[cnt2]]);

										if (((Component->Info & 1) == 0)
										        && (stricmpUTF8(GeomStr, Component->Geometry) == 0) && (NrGeomComps < 4096))
										{
											CompGeomIndex[NrGeomComps] = CompValueIndex[cnt2];
											NrGeomComps++;
											Component->Info |= 1;

											if ((PartStr[0] == 0) && (Component->PartNr[0] != 0))
												strcpy(PartStr, Component->PartNr);
										}
									}

// **********************************************************************************
									Component = &((*Components)[CompGeomIndex[0]]);
#ifdef _DEBUG

									if (stricmp(Component->Value, "MOER") == 0)
										ok = 1;

#endif

									if ((Component->SymbolInfo & MULTIPLE_SYMBOLS) == 0)
										strcpy(str2, Component->Value);
									else
										strcpy(str2, Component->InterfaceName);

									switch (mode & 3)
									{
									case 1:	// pos,value,#,geometry
										BOMInfo.ColumnWidth[0] = max(BOMInfo.ColumnWidth[0], (int32) strlen(str2));
										BOMInfo.ColumnWidth[2] =
										    max(BOMInfo.ColumnWidth[2], (int32) strlen(Component->Geometry));
										sprintf((LPSTR) & TempMem[ComponentLineOutputCount * 256], "%s\t%d\t%s\t", str2,
										        NrGeomComps, Component->Geometry);
										break;

									case 2:	// pos,value,#,geometry,part nr

										// pos,part nr,#,geometry,value
										if (BOMInfo.BOMSortNr == 0)
										{
											BOMInfo.ColumnWidth[0] = max(BOMInfo.ColumnWidth[0], (int32) strlen(str2));
											BOMInfo.ColumnWidth[3] =
											    max(BOMInfo.ColumnWidth[3], (int32) strlen(Component->PartNr));
										}
										else
										{
											BOMInfo.ColumnWidth[0] =
											    max(BOMInfo.ColumnWidth[0], (int32) strlen(Component->PartNr));
											BOMInfo.ColumnWidth[3] = max(BOMInfo.ColumnWidth[3], (int32) strlen(str2));
										}

										BOMInfo.ColumnWidth[2] =
										    max(BOMInfo.ColumnWidth[2], (int32) strlen(Component->Geometry));

										if (BOMInfo.BOMSortNr == 0)
										{
											sprintf((LPSTR) & TempMem[ComponentLineOutputCount * 256],
											        "%s\t%d\t%s\t%s\t", str2, NrGeomComps, Component->Geometry,
											        Component->PartNr);
										}
										else
										{
											sprintf((LPSTR) & TempMem[ComponentLineOutputCount * 256],
											        "%s\t%d\t%s\t%s\t", Component->PartNr, NrGeomComps,
											        Component->Geometry, str2);
										}

										break;

									case 3:	// pos,value,#,geometry,part nr,description

										// pos,part nr,#,geometry,value,description
										if (BOMInfo.BOMSortNr == 0)
										{
											BOMInfo.ColumnWidth[0] = max(BOMInfo.ColumnWidth[0], (int32) strlen(str2));
											BOMInfo.ColumnWidth[3] =
											    max(BOMInfo.ColumnWidth[3], (int32) strlen(Component->PartNr));
										}
										else
										{
											BOMInfo.ColumnWidth[0] =
											    max(BOMInfo.ColumnWidth[0], (int32) strlen(Component->PartNr));
											BOMInfo.ColumnWidth[3] = max(BOMInfo.ColumnWidth[3], (int32) strlen(str2));
										}

										BOMInfo.ColumnWidth[2] =
										    max(BOMInfo.ColumnWidth[2], (int32) strlen(Component->Geometry));
										BOMInfo.ColumnWidth[4] =
										    max(BOMInfo.ColumnWidth[4], (int32) strlen(Component->Description));

										if (BOMInfo.BOMSortNr == 0)
										{
											sprintf((LPSTR) & TempMem[ComponentLineOutputCount * 256],
											        "%s\t%d\t%s\t%s\t%s\t", str2, NrGeomComps, Component->Geometry,
											        Component->PartNr, Component->Description);
										}
										else
										{
											sprintf((LPSTR) & TempMem[ComponentLineOutputCount * 256],
											        "%s\t%d\t%s\t%s\t%s\t", Component->PartNr, NrGeomComps,
											        Component->Geometry, str2, Component->Description);
										}

										break;
									}

									ComponentLineOutputCount++;
								}
								else
									Stop2 = 1;
							}
						}
						else
							Stop = 1;
					}

					break;
				}
			}
		}

		ok = 1;

		switch (mode & 3)
		{
		case 0:
			for (cnt2 = 0; cnt2 < BOMInfo.NrColumnsUsed; cnt2++)
			{
				FoundSystemColumn = 0;

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(29, "Position")) == 0)
				{	// 4 characters
					BOMInfo.ColumnWidth[cnt2] = max(BOMInfo.ColumnWidth[cnt2], (int32) strlen(SC(29, "pos")));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(30, "Reference")) == 0)
				{	// 9 characters
					BOMInfo.ColumnWidth[cnt2] = max(BOMInfo.ColumnWidth[cnt2], (int32) strlen(SC(30, "Reference")));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(31, "Value")) == 0)
				{
					BOMInfo.ColumnWidth[cnt2] = max(BOMInfo.ColumnWidth[cnt2], (int32) strlen(SC(31, "Value")));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(32, "Geometry")) == 0)
				{
					FoundSystemColumn = 1;
					BOMInfo.ColumnWidth[cnt2] = max(BOMInfo.ColumnWidth[cnt2], (int32) strlen(SC(32, "Geometry")));
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(33, "PartNr")) == 0)
				{
					BOMInfo.ColumnWidth[cnt2] = max(BOMInfo.ColumnWidth[cnt2], (int32) strlen(SC(33, "Part nr")));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(110, "Description")) == 0)
				{
					BOMInfo.ColumnWidth[cnt2] = max(BOMInfo.ColumnWidth[cnt2], (int32) strlen(SC(110, "Description")));
					FoundSystemColumn = 1;
				}

				if (!FoundSystemColumn)
				{
					BOMInfo.ColumnWidth[cnt2] =
					    max(BOMInfo.ColumnWidth[cnt2], (int32) strlen(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]]));
				}
			}


			if (WriteMode == 0)
			{
				WriteLn(fp, SC(113, "Bill Of Materials (Listed per component)"));
				WriteLn(fp, "");
				sprintf(str, SC(10, "Design %s"), DesignPath);
				WriteLn(fp, str);
				WriteLn(fp, "");
				time(&ltime);
				today = localtime(&ltime);
				strftime(str2, 100, "%B %d, %Y %X", today);
				WriteLn(fp, str2);
				WriteLn(fp, "");

				if (RefError)
				{
					WriteLn(fp, "");
					WriteLn(fp, "!!!!!!!!!!! Some components needs to be annotated");
					WriteLn(fp, "");
					WriteLn(fp, "");
				}

				if (NrWriteModes == 2)
					WriteLn(fp, "");
			}
			else
			{
				WriteLn(fp, "");
				WriteLn(fp, "Not placed components");
				WriteLn(fp, "");
				WriteLn(fp2, "");
				WriteLn(fp2, "Not placed components");
				WriteLn(fp2, "");
			}

			strcpy(WriteString, "+");
			strcpy(str, "|");
			str2[0] = 0;

			for (cnt2 = 0; cnt2 < BOMInfo.NrColumnsUsed; cnt2++)
			{
				FoundSystemColumn = 0;
				MakeString(str4, DASH_STRING, BOMInfo.ColumnWidth[cnt2] + 1, 3);
				strcat(WriteString, str4);
				str3[0] = 0;

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(29, "Position")) == 0)
				{
					strcpy(str3, SC(29, "pos"));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(30, "Reference")) == 0)
				{
					strcpy(str3, SC(30, "Reference"));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(31, "Value")) == 0)
				{
					strcpy(str3, SC(31, "Value"));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(32, "Geometry")) == 0)
				{
					strcpy(str3, SC(32, "Geometry"));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(33, "PartNr")) == 0)
				{
					strcpy(str3, SC(33, "Part nr"));
					FoundSystemColumn = 1;
				}

				if (stricmpUTF8(BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]], SC(110, "Description")) == 0)
				{
					strcpy(str3, SC(110, "Description"));
					FoundSystemColumn = 1;
				}

				if (!FoundSystemColumn)
					strcpy(str3, BOMInfo.ColumnStr[BOMInfo.ColumnsUsed[cnt2]]);

				MakeString(str4, SPACE_STRING, BOMInfo.ColumnWidth[cnt2], 0);
				memcpy(str4, str3, strlen(str3));
				strcat(str, str4);
				strcat(str, "|");
				strcat(str2, str3);

				if ((mode & 8) == 0)
				{
					if (cnt2 < BOMInfo.NrColumnsUsed - 1)
						strcat(str2, "\t");
				}
				else
					strcat(str2, ",");
			}

			WriteLn(fp, WriteString);
			WriteLn(fp, str);
			WriteLn(fp2, str2);

			WriteLn(fp, WriteString);

			for (cnt = 0; cnt < ComponentLineOutputCount; cnt++)
			{
				if (cnt == DoubleRefComponentLineOutputCount)
					break;

				CompStr = (LPSTR) & TempMem[cnt * 256];
				length = strlen(CompStr);
				strcpy(str, "|");
				cnt3 = 0;

				for (cnt2 = 0; cnt2 < BOMInfo.NrColumnsUsed; cnt2++)
				{
					cnt4 = cnt3;

					while ((cnt4 < length) && (CompStr[cnt4] != '\t'))
						cnt4++;

					memcpy(str3, &CompStr[cnt3], cnt4 - cnt3);
					str3[cnt4 - cnt3] = 0;
					cnt3 = cnt4 + 1;
					MakeString(str4, SPACE_STRING, BOMInfo.ColumnWidth[cnt2], 0);
					memcpy(str4, str3, strlen(str3));
					strcat(str, str4);
					strcat(str, "|");
				}

				WriteLn(fp, str);

				if (mode & 8)
					ConvertTabToComma(CompStr);

				WriteLn(fp2, CompStr);
			}

			if (NrDoubleRefs == 0)
				WriteLn(fp, WriteString);
			else
			{
				WriteLn(fp, WriteString);
				WriteLn(fp, "");
				WriteLn(fp, "");
				WriteLn(fp, "!!!!!!!!!!!!! Double refs");
				WriteLn(fp, "");

				for (cnt = DoubleRefComponentLineOutputCount; cnt < ComponentLineOutputCount; cnt++)
				{
					CompStr = (LPSTR) & TempMem[cnt * 256];

					if (mode & 8)
						ConvertTabToComma(CompStr);

					WriteLn(fp, CompStr);
				}

				WriteLn(fp, "");
			}

			if (WriteMode == NrWriteModes - 1)
			{
				FileClose(fp2);
				FileClose(fp);
			}

			break;

		case 1:				// pos,value,#,geometry
		case 2:				// pos,value,#,geometry,part nr
		case 3:				// pos,value,#,geometry,part nr,description
			if (WriteMode == 0)
			{
				if (RefError)
				{
					WriteLn(fp, "");
					WriteLn(fp, "!!!!!!!!!!! Some components needs to be annotated");
					WriteLn(fp, "");
					WriteLn(fp, "");
				}
			}

			BOMInfo.ColumnWidth[1] = 3;
			BOMInfo.ColumnWidth[2] = max(BOMInfo.ColumnWidth[2], (int32) strlen(SC(32, "Geometry")));

			if (BOMInfo.BOMSortNr == 0)
			{
				BOMInfo.ColumnWidth[0] = max(BOMInfo.ColumnWidth[0], (int32) strlen(SC(31, "Value")));
				BOMInfo.ColumnWidth[3] = max(BOMInfo.ColumnWidth[3], (int32) strlen(SC(33, "Part nr")));
			}
			else
			{
				BOMInfo.ColumnWidth[0] = max(BOMInfo.ColumnWidth[0], (int32) strlen(SC(33, "Part nr")));
				BOMInfo.ColumnWidth[3] = max(BOMInfo.ColumnWidth[3], (int32) strlen(SC(31, "Value")));
			}

			BOMInfo.ColumnWidth[4] = max(BOMInfo.ColumnWidth[4], (int32) strlen(SC(110, "Description")));
			strcpy(WriteString, "+");
			strcpy(str, "|");
			str2[0] = 0;
			str5[0] = 0;
			MakeString(str4, DASH_STRING, 4, 3);
			strcat(WriteString, str4);
			MakeString(str4, SPACE_STRING, 3, 0);
			strcpy(str3, SC(29, "pos"));
			memcpy(str4, str3, strlen(str3));
			strcat(str, str4);
			strcat(str, "|");
			strcat(str2, str3);
			strcat(str5, str3);

			if ((mode & 8) == 0)
				strcat(str5, "\t");
			else
				strcat(str5, ",");

			/*
			        if (cnt2<BOMInfo.NrColumnsUsed-1) {
			          strcat(str2,"\t");
			        }
			*/
			switch (mode & 3)
			{
			case 1:
				ColumnCount = 3;
				break;

			case 2:
				ColumnCount = 4;
				break;

			case 3:
				ColumnCount = 5;
				break;
			}

			for (cnt2 = 0; cnt2 < ColumnCount; cnt2++)
			{
				MakeString(str4, DASH_STRING, BOMInfo.ColumnWidth[cnt2] + 1, 3);
				strcat(WriteString, str4);

				switch (cnt2)
				{
				case 0:
					if ((BOMInfo.BOMSortNr == 0) || ((mode & 3) == 1))
						strcpy(str3, SC(31, "Value"));
					else
						strcpy(str3, SC(33, "Part nr"));

					break;

				case 1:
					strcpy(str3, "#");
					break;

				case 2:
					strcpy(str3, SC(32, "Geometry"));
					break;

				case 3:
					if (BOMInfo.BOMSortNr == 1)
						strcpy(str3, SC(31, "Value"));
					else
						strcpy(str3, SC(33, "Part nr"));

					break;

				case 4:
					strcpy(str3, SC(110, "Description"));
					break;
				}

				MakeString(str4, SPACE_STRING, BOMInfo.ColumnWidth[cnt2], 0);
				memcpy(str4, str3, strlen(str3));
				strcat(str, str4);
				strcat(str, "|");
				strcat(str2, str3);
				strcat(str5, str3);

				if ((mode & 8) == 0)
					strcat(str5, "\t");
				else
					strcat(str5, ",");

				/*
				          if (cnt2<BOMInfo.NrColumnsUsed-1) {
				            strcat(str2,"\t");
				          }
				*/
			}

			WriteLn(fp2, str5);

			if (WriteMode == 1)
			{
				WriteLn(fp, "");
				WriteLn(fp, "Not placed components");
				WriteLn(fp, "");
				WriteLn(fp2, "");
				WriteLn(fp2, "Not placed components");
				WriteLn(fp2, "");
			}

			WriteLn(fp, WriteString);
			WriteLn(fp, str);
			WriteLn(fp, WriteString);

			if (BOMInfo.BOMSortNr == 1)
				qsort(TempMem, ComponentLineOutputCount, 256, QsortCompare);

			for (cnt = 0; cnt < ComponentLineOutputCount; cnt++)
			{
				CompStr = (LPSTR) & TempMem[cnt * 256];
				length = strlen(CompStr);
				strcpy(str, "|");

				MakeString(str4, SPACE_STRING, 3, 0);
				sprintf(str3, "%d", cnt + 1);
				memcpy(str4, str3, strlen(str3));
				strcat(str, str4);
				strcat(str, "|");

				cnt3 = 0;

				for (cnt2 = 0; cnt2 < ColumnCount; cnt2++)
				{
					cnt4 = cnt3;

					while ((cnt4 < length) && (CompStr[cnt4] != '\t'))
						cnt4++;

					memcpy(str3, &CompStr[cnt3], cnt4 - cnt3);
					str3[cnt4 - cnt3] = 0;
					cnt3 = cnt4 + 1;
					MakeString(str4, SPACE_STRING, BOMInfo.ColumnWidth[cnt2], 0);
					memcpy(str4, str3, strlen(str3));
					strcat(str, str4);
					strcat(str, "|");
				}

				WriteLn(fp, str);

				if (mode & 8)
				{
					ConvertTabToComma(CompStr);
					sprintf(str3, "%d,%s", cnt + 1, CompStr);
				}
				else
					sprintf(str3, "%d\t%s", cnt + 1, CompStr);

				WriteLn(fp2, str3);
			}

			WriteLn(fp, WriteString);

			if (WriteMode == NrWriteModes - 1)
			{
				FileClose(fp);
				FileClose(fp2);
			}

			break;
		}
	}

	if (WriteLnError != 0)
	{
		sprintf(str2, SC(85, "Could not write to file %s"), FileName);
		MessageBoxUTF8(DESIGNWindow, str2, SC(19, "Error"), MB_OK);
	}

	DeallocateMem();

	switch (mode & 3)
	{
	case 0:
		sprintf(str2, SC(115, "Bill of materials ready\t%s\\component.txt\r\n"), DesignPath);
		AddMessage(str2);

		if ((mode & 8) == 0)
			sprintf(str2, "Bill of materials ready\t%s\\component.csv ( tab delimited list )\r\n", DesignPath);
		else
			sprintf(str2, SC(116, "Bill of materials ready\t%s\\component.csv\r\n"), DesignPath);

		AddMessage(str2);

		break;

	case 1:
	case 2:
	case 3:
		sprintf(FileName, "%s\\%s.bom", DesignPath, LayoutFile);
		sprintf(str, SC(117, "Bill of materials ready\t%s\r\n"), FileName);
		AddMessage(str);

		if ((mode & 8) == 0)
			sprintf(str, SC(118, "Bill of materials ready\t%s ( tab delimited list )\r\n"), FileName2);
		else
			sprintf(FileName2, "%s\\%s.csv", DesignPath, LayoutFile); //pøidán, špatný výpis pøekladu
			sprintf(str, SC(117, "Bill of materials ready\t%s\r\n"), FileName2);
		AddMessage(str);
		break;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopySymbolsToProject(int32 Mode)
{
	int32 res, cnt2, cnt3, fp2, MaxMem;
	char SymbolName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], SymbolFileName[MAX_LENGTH_STRING];
	SymbolsPos2Record *SymbolPos2;
	SymbolRecord *Symbol;

	sprintf(str, "----------------------------------------------------------------------\r\n");
	AddMessage(str);

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		MaxMem = Sheets[cnt2].SheetMemSize + Sheets[cnt2].SymbolMemSize;

		if (LoadSheetInMemory(cnt2, 0) < 0)
		{
			MessageBoxUTF8(DESIGNWindow, SC(275, "Error in loading sheets"), SC(19, "Error"), MB_APPLMODAL | MB_OK);
			return -1;
		}

		for (cnt3 = 0; cnt3 < Design.NrSymbols; cnt3++)
		{
			SymbolPos2 = &((*SymbolsPos2)[cnt3]);
			Symbol = (SymbolRecord *) & ((*SymbolsMem)[SymbolPos2->Pos]);
			strcpy(SymbolName, SymbolPos2->SymbolName);
			sprintf(SymbolFileName, "%s\\sym\\%s.sym", DesignPath, SymbolName);
			fp2 = -1;
			strlwrUTF8(SymbolFileName);

			if (FileExistsUTF8(SymbolFileName) == -1)
			{
				fp2 = FileOpenWriteUTF8(SymbolFileName);
				FileWrite(fp2, Symbol, SymbolPos2->Length, &res);
				sprintf(str, SC(119, "Symbol  %s  copied from %s\r\n"), SymbolName, SymbolPos2->LibName);
				AddMessage(str);
//        SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				FileClose(fp2);
			}
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CopyShapesToProject(int32 Mode)
{
	char FileName[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING],
	     SearchFileName[MAX_LENGTH_STRING], ShapeFileName[MAX_LENGTH_STRING], GeometryLibNames[64][MAX_LENGTH_STRING];
	int32 result, Designfp, cnt, Libfp, niks, CompsMemLength, PinsLength, cnt2, res, NrLibEntries, *CompPos,
	      NrGeometryLibFiles, cnt3, TempMaxCompsMemory, Pos, SizeFile, fp, Found;
	CompRecord *Comp, *NewComp;
	uint8 *CompP, CopyBuf[8192];
	LibRecord Lib;
	LibNameRecord LibName;
	HANDLE FileSearchHandle;
	WIN32_FIND_DATAW FileInfo;

	sprintf(str, "----------------------------------------------------------------------\r\n");
	AddMessage(str);

	sprintf(PCBFile, "%s\\pcb\\%s.pcb", DesignPath, LayoutFile);

	if ((Designfp = FileOpenReadOnlyUTF8(PCBFile)) == -1)
		return -3;

	if (FileRead(Designfp, &LayoutDesign, sizeof(PCBDesignRecord), &result) == -1)
		return -3;

	if ((stricmp(LayoutDesign.Identification, PCBCode) != 0) && (stricmp(LayoutDesign.Identification, PCBCode2) != 0)
	        && (stricmp(LayoutDesign.Identification, PCBCode3) != 0)
	        && (stricmp(LayoutDesign.Identification, PCBCode4) != 0))
		return -4;

//  if (FileSeek(Designfp,sizeof(PCBDesignRecord)+Design.BoardOutlineSize)==-1) return -3;
	if ((strcmp(LayoutDesign.Identification, PCBCode) == 0) || (strcmp(LayoutDesign.Identification, PCBCode2) == 0))
		TempMaxCompsMemory = ((LayoutDesign.CompsMem * 3) / 2) + 65536 + LayoutDesign.NrComps * 1024;
	else
		TempMaxCompsMemory = ((LayoutDesign.CompsMem * 3) / 2) + 65536;

	if (AllocateMemTemp(TempMaxCompsMemory) == -1)
		return -1;

	LayoutComps = (CompsArray *) & TempMem[0];
	LayoutCompsMem = &TempMem[sizeof(int32) * (int) LayoutDesign.NrComps];

	if (FileRead(Designfp, LayoutComps, sizeof(int32) * (int) LayoutDesign.NrComps, &result) == -1)
		return -3;

	if ((strcmp(LayoutDesign.Identification, PCBCode) == 0) || (strcmp(LayoutDesign.Identification, PCBCode2) == 0))
	{
		CompsMemLength = 0;

		for (cnt = 0; cnt < LayoutDesign.NrComps; cnt++)
		{
			if (FileRead(Designfp, &OldComp, sizeof(OldCompRecord), &result) == -1)
				return -3;

			CompPos = &((*LayoutComps)[cnt]);
			PinsLength = OldComp.NrPins * sizeof(CompPinRecord);
			*CompPos = CompsMemLength;
			CompP = &(LayoutCompsMem[CompsMemLength]);
			NewComp = (CompRecord *) CompP;
			memset((uint8 *) NewComp, 0, sizeof(CompRecord));
			memmove((uint8 *) & NewComp->Name, (uint8 *) & OldComp.Name, sizeof(OldComp.Name));
			memmove((uint8 *) & NewComp->Value, (uint8 *) & OldComp.Value, sizeof(OldComp.Value));
			memmove((uint8 *) & NewComp->PartNr, (uint8 *) & OldComp.PartNr, sizeof(OldComp.PartNr));
			memmove((uint8 *) & NewComp->PartDescription, (uint8 *) & OldComp.PartDescription,
			        sizeof(OldComp.PartDescription));
			memmove((uint8 *) & NewComp->ShapeName, (uint8 *) & OldComp.ShapeName, sizeof(OldComp.ShapeName));
			CompP += sizeof(CompRecord);
			CompsMemLength += sizeof(CompRecord);

			if (FileRead(Designfp, CompP, PinsLength, &result) == -1)
				return -3;

			CompsMemLength += PinsLength;
		}
	}
	else
	{
		if (FileRead(Designfp, LayoutCompsMem, (int) LayoutDesign.CompsMem, &result) == -1)
			return -3;
	}

//  Comp=CompsMem[0];

	sprintf(str, "%s\\pcb\\shapes", DesignPath);

	if (DirectoryExistsUTF8(str))
		CreateDirectoryUTF8(str);

	for (cnt = 0; cnt < LayoutDesign.NrComps; cnt++)
	{
		Comp = (CompRecord *) & (LayoutCompsMem[(*LayoutComps)[cnt]]);
#ifdef _DEBUG

		if (stricmp(Comp->ShapeName, "TO-220UP_1") == 0)
			ok = 1;

#endif
		sprintf(ShapeFileName, "%s\\pcb\\shapes\\%s.shp", DesignPath, Comp->ShapeName);
		strlwrUTF8(ShapeFileName);

		if (FileExistsUTF8(ShapeFileName) == -1)
		{
			sprintf(FileName, "%s\\shapes\\%s.shp", ProjectPath, Comp->ShapeName);

			if (FileExistsUTF8(FileName) == 0)
			{
				if (CopyFileUTF8(FileName, ShapeFileName, 1))
				{
					sprintf(str, SC(120, "Geometry  %s  copied from %s\r\n"), Comp->ShapeName, FileName);
					AddMessage(str);
//          SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);
				}
			}
			else
			{	// search shape libraries
				// Search in own libraries first
				NrGeometryLibFiles = 0;

				for (cnt2 = 0; cnt2 < NrGeometryLibraries; cnt2++)
				{
					if (NrGeometryLibFiles < 64)
						strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt2]);
				}

				if (ProjectPath[0] != 0)
				{
					sprintf(SearchFileName, "%s\\shplib\\*.slb", ProjectPath);
					FileSearchHandle = FindFirstFileUTF8(SearchFileName, &FileInfo);
					res = 1;

					if (FileSearchHandle == INVALID_HANDLE_VALUE)
						res = 0;

					while (res)
					{
						UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
						sprintf(GeometryLibNames[NrGeometryLibFiles++], "%s\\shplib\\%s", ProjectPath, str2);
						res = FindNextFileW(FileSearchHandle, &FileInfo);
					}

					if (FileSearchHandle != INVALID_HANDLE_VALUE)
						FindClose(FileSearchHandle);

					/*
					          first=res=_findfirst(SearchFileName,&fileinfo);
					          while ((res!=-1)
					                &&
					                (NrGeometryLibFiles<64)) {
					            sprintf(GeometryLibNames[NrGeometryLibFiles++],"%s\\shplib\\%s",ExePath,fileinfo.name);
					            res=_findnext(first,&fileinfo);
					          }
					*/
				}

				Found = 0;
				cnt3 = 0;

				while ((!Found) && (cnt3 < NrGeometryLibFiles))
				{
					if ((Libfp = FileOpenReadOnlyUTF8(GeometryLibNames[cnt3])) == -1)
						return -1;

					if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
						return -1;

					if (strcmp(Lib.Identification, LibraryCode2) == 0)
					{
						NrLibEntries = Lib.NrLibEntries;
						cnt2 = 0;

						while ((!Found) && (cnt2 < NrLibEntries))
						{
							if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
								return -1;

							if (stricmpUTF8(LibName.Text, Comp->ShapeName) == 0)
							{
								Found = 1;
								Pos = LibName.Pos;
								SizeFile = LibName.Length;

								if ((fp = FileOpenWriteUTF8(ShapeFileName)) > 0)
								{
									FileSeek(Libfp, Pos);
									result = 1;

									while (SizeFile > 0)
									{
										FileRead(Libfp, &CopyBuf, min(SizeFile, sizeof(CopyBuf)), &result);

										if (result > 0)
										{
											FileWrite(fp, &CopyBuf, result, &niks);
											SizeFile -= result;
										}
									}

									FileClose(fp);
								}
							}

							cnt2++;
						}
					}

					if (FileClose(Libfp) == -1)
						return -1;

					cnt3++;
				}

				if (Found)
					sprintf(str, SC(120, "Geometry  %s  copied from %s\r\n"), Comp->ShapeName, GeometryLibNames[cnt3]);
				else
					sprintf(str, SC(121, "Geometry  %s  could not be found\r\n"), Comp->ShapeName);

				AddMessage(str);
			}
		}
	}

	FileClose(Designfp);
	sprintf(str, "----------------------------------------------------------------------\r\n");
	AddMessage(str);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadLayout(int32 Mode)
{
	char str[MAX_LENGTH_STRING];
	int32 result, Designfp, cnt;
	CompRecord *LayoutComp;
	OldCompRecord *OldLayoutComp;

	AllocateMemTemp2(256 * 1024);
	memset(TempMem2, 0, MaxTempMemory2);
	UsedLayoutCompRefPtr = 0;

	memset(&LayoutDesign, 0, sizeof(LayoutDesign));

	sprintf(str, SC(244, "Annotation of layout components included\r\n"));
	AddMessage(str);

	sprintf(PCBFile, "%s\\pcb\\%s.pcb", DesignPath, LayoutFile);
	sprintf(PCBFileBackup, "%s\\pcb\\backup\\%s_annotate.pcb", DesignPath, LayoutFile);
	CopyFileUTF8(PCBFile, PCBFileBackup, 0);

	if ((Designfp = FileOpenReadOnlyUTF8(PCBFile)) == -1)
		return -3;

	if (FileRead(Designfp, &LayoutDesign, sizeof(PCBDesignRecord), &result) == -1)
		return -3;

	if ((stricmp(LayoutDesign.Identification, PCBCode) != 0) && (stricmp(LayoutDesign.Identification, PCBCode2) != 0)
	        && (stricmp(LayoutDesign.Identification, PCBCode3) != 0)
	        && (stricmp(LayoutDesign.Identification, PCBCode4) != 0))
		return -4;

//  if (FileSeek(Designfp,sizeof(PCBDesignRecord)+LayoutDesign.BoardOutlineSize)==-1) return -3;

	if (AllocateMemTemp(sizeof(int32) * (int) LayoutDesign.NrComps + LayoutDesign.CompsMem) == -1)
		return -1;

	LayoutComps = (CompsArray *) & TempMem[0];
	LayoutCompsMem = &TempMem[sizeof(int32) * (int) LayoutDesign.NrComps];

	if (FileRead(Designfp, LayoutComps, sizeof(int32) * (int) LayoutDesign.NrComps, &result) == -1)
		return -3;

	if (FileRead(Designfp, LayoutCompsMem, (int) LayoutDesign.CompsMem, &result) == -1)
		return -3;

//  Comp=CompsMem[0];
	for (cnt = 0; cnt < LayoutDesign.NrComps; cnt++)
	{
		if ((strcmp(LayoutDesign.Identification, PCBCode) == 0) || (strcmp(LayoutDesign.Identification, PCBCode2) == 0))
		{
			LayoutComp = (CompRecord *) & (LayoutCompsMem[(*LayoutComps)[cnt]]);
			LayoutComp->Info2 = 0;
		}
		else
		{
			OldLayoutComp = (OldCompRecord *) & (LayoutCompsMem[(*LayoutComps)[cnt]]);
			OldLayoutComp->Info2 = 0;
		}

//    Comp->ShapeName);
	}

	FileClose(Designfp);
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void WriteGatePins(int fp2, LPSTR Reference)
{
	int32 cnt2, cnt3, cnt4, res, LineNr;
	ObjectRecord *Object;
	char str[1000], NewPinBusStr[512], str2[MAX_LENGTH_STRING];
	RedefinedPinBusRecord *RedefinedPinBus;
	int32 ChangedPinBus;

	for (cnt4 = 0; cnt4 < NrObjects; cnt4++)
	{
		Object = &((*Objects)[cnt4]);

		switch (Object->ObjectType)
		{
		case SYMBOL_PIN:
			sprintf(str, "PIN     '%s'  %i", Object->Text1, Object->Info2);
			WriteLn(fp2, str);
			break;

		case SYMBOL_PINBUS:
			ChangedPinBus = 0;

			for (cnt2 = 0; cnt2 < Design.NrRedefinedPinBusses; cnt2++)
			{
				RedefinedPinBus = &((*RedefinedPinBusses)[cnt2]);

				if ((RedefinedPinBus->Info & OBJECT_NOT_VISIBLE) == 0)
				{
					if ((stricmpUTF8(RedefinedPinBus->Reference, Reference) == 0)
					        && (stricmpUTF8(RedefinedPinBus->Name, Object->Text2) == 0))
					{
						LineNr = 0;
						NewPinBusStr[0] = 0;

						for (cnt3 = 0; cnt3 < Object->Info3; cnt3++)
						{
							res =
							    GetPinNameFromPinBus(Object->Text1, str2, Object->Info3, RedefinedPinBus->Order[cnt3]);

							if (res > LineNr)
								strcat(NewPinBusStr, "\\");

							strcat(NewPinBusStr, str2);
							LineNr = res;
							ChangedPinBus = 1;
						}
					}
				}
			}

			if (ChangedPinBus)
				sprintf(str, "PINBUS  %i '%s'  %i  %s", Object->Info3, NewPinBusStr, Object->Info2, Object->Text2);
			else
				sprintf(str, "PINBUS  %i '%s'  %i  %s", Object->Info3, Object->Text1, Object->Info2, Object->Text2);

			WriteLn(fp2, str);
			break;
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CreateGatePinSwapInfo(int32 Mode)
{
	int32 ok, cnt, cnt2, cnt3, NrGates, GateNr, SymbolNr, fp2;
	char FileName[MAX_LENGTH_STRING], str[500], str2[500];
	InstanceRecord *NewInstance, *NewInstance2, NewInstance3;
	LPSTR Ref;
	int32 Found;
	SymbolRecord *Symbol;

	NewInstance2 = NULL;
	NrGates = 0;
	sprintf(FileName, "%s\\pcb\\gatepin.swp", DesignPath);

	if ((fp2 = FileOpenWriteUTF8(FileName)) <= 0)
		return 0;

	sprintf(str, SC(122, ";Gate/pin swap info design %s"), DesignName);
	WriteLn(fp2, str);

	NewSymbolsMemSize = 0;
	NrNewSymbols = 0;
	NrNewInstances = 0;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);

//      sprintf(str,"Checking references %s\r\n",FileName);

		Sheets[cnt2].Info = 0;
		LoadSheetInMemory(cnt2, 9);
	}

// **********************************************************************************
// **********************************************************************************

	for (cnt = 0; cnt < NrNewInstances; cnt++)
	{
		NewInstance = &((*NewInstances)[cnt]);
		NewInstance->Info &= ~(2 + 1);
	}


	GateNr = 1;

	for (cnt = 0; cnt < NrNewInstances; cnt++)
	{
#ifdef _DEBUG

		if (cnt == 75)
		{
			ok = 1;
// NrObjects
		}

#endif
		NewInstance = &((*NewInstances)[cnt]);

		if (NewInstance->AddNr >= 0)
		{
			Symbol = (SymbolRecord *) & ((*NewSymbolsMem)[(*NewSymbolsPos2)[NewInstance->AddNr].Pos]);
			NrObjects = 0;
			InstancePinsToObject(NewInstance, 2);

			if ((NewInstance->Info & 1) == 0)
			{
				if ((NewInstance->Info & MULTIPLE_SYMBOLS) == MULTIPLE_SYMBOLS)
				{
					ok = 1;
					strcpy(str2, "        ");
					memmove(&str2, &NewInstance->Reference, strlen(NewInstance->Reference));
					sprintf(str, "GATE    %s     0    0   %s  %i", str2, Symbol->InterfaceName, NrGates);
					NrGates++;
					//        WriteLn(fp2,";---------------------------------------------------------------");
					WriteLn(fp2, str);

					for (cnt2 = cnt + 1; cnt2 < NrNewInstances; cnt2++)
					{
						NewInstance2 = &((*NewInstances)[cnt2]);

						if (((NewInstance2->Info & (MULTIPLE_SYMBOLS | 1)) == MULTIPLE_SYMBOLS)
						        && (stricmpUTF8(NewInstance2->Reference, NewInstance->Reference) == 0))
						{
							InstancePinsToObject(NewInstance2, 2);
							NewInstance2->Info |= 1;
						}
					}

					WriteGatePins(fp2, NewInstance->Reference);
				}
				else
				{
					if (Symbol->NrPartsPerPackage > 1)
					{
						SymbolNr = NewInstance->AddNr;
						strcpy(str2, "        ");
						memmove(&str2, &NewInstance->Reference, strlen(NewInstance->Reference));
						sprintf(str, "GATE    %s   %3i  %3i   %s  %i", str2, NewInstance->PackagePartNr, GateNr,
						        NewInstance->Value, NrGates);
						NewInstance->Code1 = GateNr;
						NrGates++;
						//          WriteLn(fp2,";---------------------------------------------------------------");
						WriteLn(fp2, str);
						WriteGatePins(fp2, NewInstance->Reference);

						for (cnt2 = cnt + 1; cnt2 < NrNewInstances; cnt2++)
						{
							NewInstance2 = &((*NewInstances)[cnt2]);

							if (((NewInstance2->Info & 1) == 0) && (NewInstance2->AddNr == SymbolNr)
							        && (stricmpUTF8(NewInstance2->Value, NewInstance->Value) == 0))
							{
								NrObjects = 0;
								InstancePinsToObject(NewInstance2, 2);
								strcpy(str2, "        ");
								memmove(&str2, &NewInstance2->Reference, strlen(NewInstance2->Reference));
								sprintf(str, "GATE    %s   %3i  %3i   %s  %i", str2, NewInstance2->PackagePartNr,
								        GateNr, NewInstance2->Value, NrGates);
								NewInstance2->Code1 = GateNr;
								NrGates++;
								//              WriteLn(fp2,";---------------------------------------------------------------");
								WriteLn(fp2, str);
								WriteGatePins(fp2, NewInstance2->Reference);

								NewInstance2->Info |= 1;
							}
						}

						GateNr++;
					}
					else
					{
						strcpy(str2, "        ");
						memmove(&str2, &NewInstance->Reference, strlen(NewInstance->Reference));
						sprintf(str, "GATE    %s     0    0   %s  %i", str2, NewInstance->Value, NrGates);
						NrGates++;
						//          WriteLn(fp2,";---------------------------------------------------------------");
						WriteLn(fp2, str);

						if (NrObjects > 0)
							ok = 1;

						WriteGatePins(fp2, NewInstance->Reference);
					}
				}

				NewInstance->Info |= 1;
			}
		}
	}

	ok = 1;

// **********************************************************************************
// **********************************************************************************

	for (cnt = 0; cnt < NrNewInstances; cnt++)
	{
		NewInstance = &((*NewInstances)[cnt]);

		if (NewInstance->AddNr >= 0)
		{
			Symbol = (SymbolRecord *) & ((*NewSymbolsMem)[(*NewSymbolsPos2)[NewInstance->AddNr].Pos]);

			if (((NewInstance->Info & (MULTIPLE_SYMBOLS | 2)) == 0) && (Symbol->NrPartsPerPackage > 1))
			{
				memmove(&NewInstance3, NewInstance, sizeof(InstanceRecord));
				SymbolNr = NewInstance->AddNr;
				Ref = NewInstance->Reference;

				for (cnt3 = 1; cnt3 <= Symbol->NrPartsPerPackage; cnt3++)
				{
					Found = 0;

					for (cnt2 = 0; cnt2 < NrNewInstances; cnt2++)
					{
						NewInstance2 = &((*NewInstances)[cnt2]);

						if (((NewInstance2->Info & (MULTIPLE_SYMBOLS | 2)) == 0)
						        && (NewInstance2->PackagePartNr == cnt3) && (NewInstance2->AddNr == SymbolNr)
						        && (stricmpUTF8(NewInstance->Reference, NewInstance2->Reference) == 0)
						        && (stricmpUTF8(NewInstance2->Value, NewInstance->Value) == 0))
						{
							Found = 1;
							NewInstance2->Info |= 2;
						}
					}

					if (!Found)
					{
						NrObjects = 0;
						NewInstance3.PackagePartNr = (int16) cnt3;
						InstancePinsToObject(&NewInstance3, 2);
						strcpy(str2, "        ");
						memmove(&str2, &NewInstance->Reference, strlen(NewInstance3.Reference));
						sprintf(str, "GATE    %s   %3i  %3i   %s  %i", str2, NewInstance3.PackagePartNr,
						        NewInstance->Code1, NewInstance3.Value, NrGates);
						WriteLn(fp2, str);
						NrGates++;
						WriteGatePins(fp2, NewInstance3.Reference);

						NewInstance2->Info |= 1;
					}
				}
			}
		}
	}

	FileClose(fp2);
	sprintf(str, SC(123, "Gate/pinswap file %s\\pcb\\gatepin.swp is generated\r\n"), DesignPath);
	AddMessage(str);
//  SendMessageUTF8(EditWindow,EM_REPLACESEL,0,(LPARAM)str);

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 WriteSchematics(int32 mode)
{
	int32 res, result, Designfp, cnt, cnt2, MemPos, MemSize;
	char FileName[MAX_LENGTH_STRING];

	int32 FileChanged;

	GetDesignSheets();
	MaxMem = 0;

	for (cnt = 0; cnt < NrSheets; cnt++)
		MaxMem = max(MaxMem, Sheets[cnt].SheetMemSize + Sheets[cnt].SymbolMemSize);

	if (MaxMem + 65536 > DesignBufMemSize)
	{
		if (AllocateMemDesignBuf(MaxMem + 65536) == -1)
			return -1;
	}

	ok = 1;

	for (cnt2 = 0; cnt2 < NrSheets; cnt2++)
	{
		FileChanged = 0;
		memset(DesignBuf, 0, MaxMem + 65536 - 8);

		sprintf(FileName, "%s%s.sch", SheetDir, Sheets[cnt2].SheetName);

		if ((res = LoadSheetInMemory(cnt2, 0)) < 0)
			return -2;


		CutExtensionFileName(FileName);
		strcat(FileName, "_new.sch");

		if ((Designfp = FileOpenWriteUTF8(FileName)) <= 0)
			return -4;


		MemPos = Design.NrSymbols * sizeof(SymbolsPosRecord);
		strcpy(Design.Identification, SheetCode5);
		FileWrite(Designfp, &Design, sizeof(DesignRecord), &result);
		FileWrite(Designfp, &DesignBuf[0], Design.NrSymbols * sizeof(SymbolsPosRecord), &result);
		FileWrite(Designfp, &DesignBuf[MemPos], Design.NrInstances * sizeof(InstanceRecord), &result);

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
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
