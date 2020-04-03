/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: debug.c
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



#include "owntypes.h"
#include "stdio.h"
#include "windows.h"
#include "string.h"

#define  MEMORY_MAPPED_DEBUG_STRING    "MERCO_DEBUG_MAP_FILE"
#define  MAX_DEBUG_STRING_LENGTH       512


//uint8  DebugMagic[16]={0x90,0xc1,0x72,0x4b,0x4c,0xdf,0x09,0xb2};

#define  DEBUG_MAGIC1   0x4b72c190
#define  DEBUG_MAGIC2   0xb209df4c


extern int64 CurrentFrequency;

static HANDLE SharedMemoryDebugHandle;
static int64 SpecialCounterProgramStart;
static int32 SharedMemoryLength, *SharedMemoryPos, MaxDebugStringLength, *StringLengthP, MaxDebugStringLength,
       *OptionsP;
static SYSTEMTIME CurrentDateProgramStart;

static char DebugTempStr[64];
static char DebugTempStr2[MAX_DEBUG_STRING_LENGTH + 64];
static char *DebugSharedMemory;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

#if 0

/*
SYSTEMTIME                  CurrentDateProgramStart;
int64                       SpecialCounterProgramStart;
*/

int32 GetTimeString(int32 mode, LPSTR TimeString)
{
	int64 SpecialCounter, hulp;
	int32 micro_seconds;
	uint32 hour, min, sec, temp_seconds;

	QueryPerformanceCounter((LARGE_INTEGER *) & SpecialCounter);
	hulp = (SpecialCounter - SpecialCounterProgramStart) * 1000000;
	hulp /= CurrentFrequency;

	switch (mode)
	{
	case 0:
		temp_seconds = CurrentDateProgramStart.wHour * 3600;
		temp_seconds += CurrentDateProgramStart.wMinute * 60;
		temp_seconds += CurrentDateProgramStart.wSecond;
		temp_seconds += (int32) (hulp / 1000000);
		hour = temp_seconds / 3600;
		temp_seconds %= 3600;
		min = temp_seconds / 60;
		temp_seconds %= 60;
		sec = temp_seconds;
		micro_seconds = (int32) (hulp % 1000000);
		sprintf(TimeString, "%02d:%02d:%02d.%06d", hour, min, sec, micro_seconds);
		break;

	case 1:
		temp_seconds = CurrentDateProgramStart.wHour * 3600;
		temp_seconds += CurrentDateProgramStart.wMinute * 60;
		temp_seconds += CurrentDateProgramStart.wSecond;
		temp_seconds += (int32) (hulp / 1000000);
		hour = temp_seconds / 3600;
		temp_seconds %= 3600;
		min = temp_seconds / 60;
		temp_seconds %= 60;
		sec = temp_seconds;
		micro_seconds = (int32) (hulp % 1000000);
		sprintf(TimeString, "%02d-%02d-%4d %02d:%02d:%02d.%06d", CurrentDateProgramStart.wDay,
		        CurrentDateProgramStart.wMonth, CurrentDateProgramStart.wYear, hour, min, sec, micro_seconds);
		break;

	case 2:
		sec = (int32) (hulp / 1000000);
		micro_seconds = (int32) (hulp % 1000000);
		sprintf(TimeString, "%04d.%06d", sec, micro_seconds);
		break;
	}

	return 0;
}

#endif

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 AddToDebugLog(char *Str, int32 mode)
{
	char *SharedMemoryP, *StrP;
	int64 SpecialCounter, hulp;
	int32 micro_seconds, sec, cnt, cnt2, cnt3, length, length_string, length_string2;
	int32 *int32p1, *int32p2;

	if ((DebugSharedMemory == NULL) || (!Str) || ((length_string = (int32) strlen(Str)) == 0))
		return -1;

	*(int32 *) & Str[length_string] = 0x0a0d;
	length_string += 2;
	QueryPerformanceCounter((LARGE_INTEGER *) & SpecialCounter);
	hulp = (SpecialCounter - SpecialCounterProgramStart) * 1000000;
	hulp /= CurrentFrequency;

	sec = (int32) (hulp / 1000000);
	micro_seconds = (int32) (hulp % 1000000);
	StrP = &DebugTempStr[8 + 4 + 5];

	for (cnt = 0; cnt < 6; cnt++)
	{
		*StrP = '0' + (char) (sec % 10);
		StrP--;
		sec /= 10;
	}

	StrP = &DebugTempStr[8 + 4 + 6 + 1 + 5];

	for (cnt = 0; cnt < 6; cnt++)
	{
		*StrP = '0' + (char) (micro_seconds % 10);
		StrP--;
		micro_seconds /= 10;
	}

	// pos=8+6+1+6+3 = 24
//  sprintf(str2,"%04d.%06d",sec,micro_seconds);


	if (length_string > MaxDebugStringLength)
		length_string = MaxDebugStringLength;

	length_string2 = (length_string + 3) & ~3;
	*(int32 *) & Str[length_string] = 0;
	*StringLengthP = length_string2 + 16;
	length = 28 + length_string2 + 4;

	if (*SharedMemoryPos + length <= SharedMemoryLength)
	{
		SharedMemoryP = &DebugSharedMemory[*SharedMemoryPos];
		int32p1 = (int32 *) SharedMemoryP;
		int32p2 = (int32 *) DebugTempStr;

		for (cnt = 0; cnt < 7; cnt++)
		{
			*int32p1 = *int32p2;
			int32p1++;
			int32p2++;
		}

		SharedMemoryP = (char *) int32p1;
		memcpy(SharedMemoryP, Str, length_string2);
		SharedMemoryP += length_string2;
		*(int32 *) SharedMemoryP = length_string2 + 16;
		(*SharedMemoryPos) += length;

		if (*SharedMemoryPos == SharedMemoryLength)
			*SharedMemoryPos = 4096;

		return 0;
	}
	else
	{
		if (*OptionsP & 1)
		{
			// No circular buffer used -> Only record buf lines until buffer is full
			return 0;
		}

		int32p1 = (int32 *) DebugTempStr2;
		int32p2 = (int32 *) DebugTempStr;

		for (cnt = 0; cnt < 7; cnt++)
		{
			*int32p1 = *int32p2;
			int32p1++;
			int32p2++;
		}

		StrP = (char *) int32p1;
		memcpy(StrP, Str, length_string2);
		StrP += length_string2;
		*(int32 *) StrP = length_string2 + 16;
		cnt3 = cnt2 = SharedMemoryLength - (*SharedMemoryPos);
		SharedMemoryP = &DebugSharedMemory[*SharedMemoryPos];
		memcpy(SharedMemoryP, DebugTempStr2, cnt2);
		SharedMemoryP = &DebugSharedMemory[4096];
		cnt2 = length - cnt2;
		memcpy(SharedMemoryP, &DebugTempStr2[cnt3], cnt2);
		*SharedMemoryPos = 4096 + cnt2;
		return 0;
	}
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 InitDebugLog(int32 mode)
{
//  int32 v1,v2;

	QueryPerformanceCounter((LARGE_INTEGER *) & SpecialCounterProgramStart);
	GetLocalTime(&CurrentDateProgramStart);

	if ((SharedMemoryDebugHandle = OpenFileMapping(FILE_MAP_WRITE, 0, MEMORY_MAPPED_DEBUG_STRING)))
		DebugSharedMemory = (char *) MapViewOfFile(SharedMemoryDebugHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
	else
		return -1;

	*(uint32 *) & DebugTempStr[0] = DEBUG_MAGIC1;
	*(uint32 *) & DebugTempStr[4] = DEBUG_MAGIC2;
	StringLengthP = (int32 *) & DebugTempStr[8];
	DebugTempStr[18] = '.';
	DebugTempStr[25] = ' ';
	DebugTempStr[26] = ':';
	DebugTempStr[27] = ' ';
	SharedMemoryPos = (int32 *) & DebugSharedMemory[0];
	SharedMemoryLength = *(int32 *) & DebugSharedMemory[4];
	OptionsP = (int32 *) & DebugSharedMemory[12];
//  v1=*(int32 *)&DebugSharedMemory[32];
//  v2=*(int32 *)&DebugSharedMemory[36];
	MaxDebugStringLength = MAX_DEBUG_STRING_LENGTH;
	*(int32 *) & DebugSharedMemory[8] = MaxDebugStringLength - 1;

	if (*OptionsP & 1)
		SharedMemoryLength -= 4;

	return 0;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
