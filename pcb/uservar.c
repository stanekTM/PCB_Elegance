/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: uservar.c
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
#include "files2.h"
#include "calc.h"
#include "files.h"
#include "calcdef.h"
#include "stdio.h"
#include "utf8.h"
#include "resource.h"

#define MAX_USER_VARS                  64
#define MAX_LENGTH_USER_VAR_STRING     128


LPSTR NewNetLabelStr;
char CachedUserVarID[MAX_USER_VARS][MAX_LENGTH_USER_VAR_STRING],
     CachedUserVarValue[MAX_USER_VARS][MAX_LENGTH_USER_VAR_STRING], CachedNetLabelStr[MAX_LENGTH_STRING],
     UserVarFileName[MAX_LENGTH_STRING], UserVarFileName2[MAX_LENGTH_STRING];

int32 CachedUserVarCount, NrUserVars, UserVarFileCached, UserVarFilePos, UserVarBufLength;
uint64 LatestUserVarFileTimeForCache, LatestUserVarFileTime;
int64 UserVarTimeStamp;
uint8 *UserVarBuf;
WIN32_FIND_DATA UserVarFileData;

extern int64 CurrentFrequency;

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

int32 LoadUserVarFileName(int32 mode)
{
	char str[MAX_LENGTH_STRING];

	if (UserVarFileName[0] == 0)
	{
		if (DesignPath[0] == 0)
		{
			LatestUserVarFileTimeForCache = 0;
			UserVarFileCached = 0;
			return -1;
		}

		if (DesignFile[0] != 0)
		{
			GetFilePartFromFileName(str, DesignFile);
			CutExtensionFileName(str);
		}
		else
		{
			if (EditFile[0] != 0)
				GetFilePartFromFileName(str, DesignPath);
			else
			{
				LatestUserVarFileTimeForCache = 0;
				UserVarFileCached = 0;
				return -2;
			}
		}

		sprintf(UserVarFileName, "%s\\%s.var", DesignPath, str);
		sprintf(UserVarFileName2, "%s\\%s_new.var", DesignPath, str);
//    MessageBox(NULL,UserVarFileName,"user var filename",MB_APPLMODAL+MB_OK);
	}

	return 0;
}

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

int32 ReadLnUserVar(LPSTR FileName, LPSTR LineBuf)
{
	int32 BytesRead = 0, cnt, fp;
	int32 LineLength, OldPos, count, count2;
	char *BufP;
	LineLength = 0;

	if ((UserVarBufLength > 0) && (UserVarFilePos >= UserVarBufLength))
	{
		UserVarFilePos = 0;
		return -1;
	}

	cnt = 1;

	if (UserVarFileCached == 0)
	{
		fp = FileOpenReadOnlyUTF8(FileName);

		if (fp <= 0)
			return -1;

		BytesRead = 0;

		if (UserVarBuf == NULL)
			AllocateSpecialMem(MEM_USERVARS, 32768, (void *) &UserVarBuf);

		if ((FileRead(fp, UserVarBuf, 32768, &BytesRead) == -1) || (BytesRead == 0))
		{
			FileClose(fp);
			return -2;
		}

		FileClose(fp);
		UserVarFilePos = 0;
		UserVarFileCached = 1;
		UserVarBufLength = BytesRead;
	}

	OldPos = UserVarFilePos;
	BufP = &UserVarBuf[UserVarFilePos];
	count = UserVarBufLength - UserVarFilePos;

	while ((count > 0) && (*BufP != 10))
	{
		count--;
		BufP++;
		UserVarFilePos++;
	}

	count2 = min(256, UserVarFilePos - OldPos);
	memcpy(&LineBuf[0], &UserVarBuf[OldPos], count2);
	LineLength = count2;

	if (LineLength > 0)
	{
		cnt = LineLength - 1;

		while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
			cnt--;

		LineLength = cnt + 1;
	}

	UserVarFilePos++;
	LineBuf[LineLength] = 0;
	return LineLength;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 FileDateUserVarFileChanged(int32 mode)
{
	WIN32_FIND_DATAW FileData;
	int64 Counter, TimeDivMilliSeconds;
	HANDLE FileSearch;

	if (UserVarTimeStamp == 0)
	{
		QueryPerformanceCounter((LARGE_INTEGER *) & UserVarTimeStamp);

		if ((FileSearch = FindFirstFileUTF8(UserVarFileName, &FileData)) == INVALID_HANDLE_VALUE)
			return 0;

		FindClose(FileSearch);
		memcpy(&LatestUserVarFileTime, &FileData.ftLastWriteTime, 8);

		if (mode == 1)
			return 0;
		else
			return 1;
	}

	if (mode == 1)
	{
		if ((FileSearch = FindFirstFileUTF8(UserVarFileName, &FileData)) == INVALID_HANDLE_VALUE)
			return 0;

		FindClose(FileSearch);

		if (memcmp(&FileData.ftLastWriteTime, &LatestUserVarFileTime, 8) != 0)
			return 1;
	}
	else
	{
		QueryPerformanceCounter((LARGE_INTEGER *) & Counter);
		TimeDivMilliSeconds = (Counter - UserVarTimeStamp) * 1000;
		TimeDivMilliSeconds /= CurrentFrequency;

		if (TimeDivMilliSeconds > 100)
		{
			QueryPerformanceCounter((LARGE_INTEGER *) & UserVarTimeStamp);

			if ((FileSearch = FindFirstFileUTF8(UserVarFileName, &FileData)) == INVALID_HANDLE_VALUE)
				return 0;

			FindClose(FileSearch);

			if (memcmp(&FileData.ftLastWriteTime, &LatestUserVarFileTime, 8) != 0)
			{
				memcpy(&LatestUserVarFileTime, &FileData.ftLastWriteTime, 8);
				return 1;
			}
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetUserVar(LPSTR UserVarID, LPSTR UserVarValue, int32 mode)
{
	int32 Length, count, Found, cnt;
	char LineBuf[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	if (LoadUserVarFileName(0) < 0)
		return 0;

	if (FileDateUserVarFileChanged(0))
	{
		// Read file for the first time, or when changed
		UserVarFileCached = 0;
		count = 0;
		Found = 0;
		UserVarFilePos = 0;

		while ((Length = ReadLnUserVar(UserVarFileName, LineBuf)) >= 0)
		{
			if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#')
			        && (count < MAX_USER_VARS))
			{
				if (GetStringValue(LineBuf, str, str2))
				{
					memset(&CachedUserVarID[count], 0, MAX_LENGTH_USER_VAR_STRING);
					memset(&CachedUserVarValue[count], 0, MAX_LENGTH_USER_VAR_STRING);
					strncpy(CachedUserVarID[count], str, MAX_LENGTH_USER_VAR_STRING - 1);
					strncpy(CachedUserVarValue[count], str2, MAX_LENGTH_USER_VAR_STRING - 1);
					count++;
					CachedUserVarCount = count;
				}
			}
		}
	}

	for (cnt = 0; cnt < CachedUserVarCount; cnt++)
	{
		if (stricmpUTF8(CachedUserVarID[cnt], UserVarID) == 0)
		{
			strcpy(UserVarValue, CachedUserVarValue[cnt]);
			return 1;
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

	if (LoadUserVarFileName(0) < 0)
		return -1;

	changed = 0;
	count = 0;
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
					changed = 1;
					strcpy(str2, UserVarValue);
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

int32 CheckNewUserVars(int32 mode)
{
	if (LoadUserVarFileName(0) < 0)
		return 0;

	if (FileDateUserVarFileChanged(1))
		return 1;

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
