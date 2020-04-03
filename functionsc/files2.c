/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: files2.c
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
#include "ctype.h"
#include "windows.h"
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "time.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "files2.h"
#include "utf8.h"
#include "shlobj.h"

#define  TextBufSize          262144
#define  CR                   13
#define  LF                   10


typedef int32(WINAPI * PGETDISKFREESPACEEX) (LPCSTR, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER);

char *TextBuf, EndOfLineChar;
HGLOBAL TextBufGlobal;
int32 EndOfLineMode;
int32 TextBufPos, TextBufLength, TextLineNr, FileP, FileP2, WriteLnError;
int32 CountedCR, CountedLF;
int32 MaxLineLength = 190;

STRING_DECLARE(WriteTextLine);

ALIGN32PRE WCHAR FileStringUC[MAX_LENGTH_UC_STRING] ALIGN32POST;

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileOpen(LPSTR FileName)
{
	int32 fp;

	WriteLnError = 0;
	fp = open(FileName, O_RDWR | O_BINARY);

	if (fp != -1)
		return fp;

	switch (errno)
	{
	case ENOENT:				// File not found
		fp = open(FileName, O_RDWR | O_CREAT | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				return -2;

			default:			// Unknown error occured
				return -3;
			}
		}

		break;

	case EACCES:				// Cannot access file
		return -2;
	}

	return -3;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 FileOpenUTF8(LPSTR FileName)
{
#ifdef UC
	int32 fp;

	WriteLnError = 0;

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_UC_STRING - 1) == -1)
		return -2;

	fp = _wopen(FileStringUC, O_RDWR | O_BINARY);

	if (fp != -1)
		return fp;

	switch (errno)
	{
	case ENOENT:				// File not found
		fp = _wopen(FileStringUC, O_RDWR | O_CREAT | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				return -2;

			default:			// Unknown error occured
				return -3;
			}
		}

		break;

	case EACCES:				// Cannot access file
		return -2;
	}

	return -3;
}

#else

	return FileOpen(FileName);
}

#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 CheckReadOnly(LPSTR FileName)
{
	uint32 res;

	res = GetFileAttributes(FileName);

	if (res == 0xffffffff)
		return -2;

	if ((res & FILE_ATTRIBUTE_READONLY) == 0)
		return 0;

	return -1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileOpenWrite(LPSTR FileName)
{
	int32 fp;

	EndOfLineMode = 0;
	WriteTextLine[0] = 0;
	fp = open(FileName, O_RDWR | O_BINARY | O_TRUNC);

	if (fp != -1)
		return fp;

	switch (errno)
	{
	case ENOENT:				// File not found
		fp = open(FileName, O_RDWR | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				return -2;

			default:			// Unknown error occured
				return -3;
			}
		}

		return fp;

	case EACCES:				// Cannot access file
		return -2;
	}

	return -3;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 FileOpenWriteUTF8(LPSTR FileName)
{
#ifdef UC
	int32 fp;

	EndOfLineMode = 0;
	WriteTextLine[0] = 0;

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_STRING - 1) == 0)
		return -2;

	fp = _wopen(FileStringUC, O_RDWR | O_BINARY | O_TRUNC);

	if (fp != -1)
		return fp;

	switch (errno)
	{
	case ENOENT:				// File not found
		fp = _wopen(FileStringUC, O_RDWR | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				return -2;

			default:			// Unknown error occured
				return -3;
			}
		}

		return fp;

	case EACCES:				// Cannot access file
		return -2;
	}

	return -3;
}

#else

	return FileOpenWrite(FileName);
}

#endif


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileOpenWriteAppend(LPSTR FileName)
{
	int32 fp;

	EndOfLineMode = 0;
	WriteTextLine[0] = 0;
	fp = open(FileName, O_RDWR | O_BINARY);

	if (fp != -1)
	{
		lseek(fp, 0, SEEK_END);	// Set file pointer to end of file
		return fp;
	}

	switch (errno)
	{
	case ENOENT:				// File not found
		fp = open(FileName, O_RDWR | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				return -2;

			default:			// Unknown error occured
				return -3;
			}
		}

		return fp;

	case EACCES:				// Cannot access file
		return -2;
	}

	return -3;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 FileOpenWriteAppendUTF8(LPSTR FileName)
{
#ifdef UC
	int32 fp;

	EndOfLineMode = 0;
	WriteTextLine[0] = 0;

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_STRING - 1) == 0)
		return -2;

	fp = _wopen(FileStringUC, O_RDWR | O_BINARY);

	if (fp != -1)
	{
		lseek(fp, 0, SEEK_END);	// Set file pointer to end of file
		return fp;
	}

	switch (errno)
	{
	case ENOENT:				// File not found
		fp = _wopen(FileStringUC, O_RDWR | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				return -2;

			default:			// Unknown error occured
				return -3;
			}
		}

		return fp;

	case EACCES:				// Cannot access file
		return -2;
	}

	return -3;
}

#else

	return FileOpenWriteAppend(FileName);
}

#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileOpenReadOnly(LPSTR FileName)
{
	WriteLnError = 0;
	return open(FileName, O_RDONLY | O_BINARY);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 FileOpenReadOnlyUTF8(LPSTR FileName)
{
#ifdef UC
	WriteLnError = 0;

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_STRING - 1) == 0)
		return -2;

	return _wopen(FileStringUC, O_RDONLY | O_BINARY);
}

#else

	return FileOpenReadOnly(FileName);
}

#endif


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 TextFileOpen(LPSTR FileName)
{
	int32 fp;

	WriteLnError = 0;
	CountedCR = 0;
	CountedLF = 0;
	TextLineNr = 0;
	EndOfLineChar = LF;

	if (TextBuf != NULL)
		return -4;

	if ((TextBufGlobal = GlobalAlloc(GHND, TextBufSize)) == NULL)
		return -1;

	if ((TextBuf = (LPSTR) GlobalLock(TextBufGlobal)) == NULL)
		return -1;

	fp = open(FileName, O_RDONLY | O_BINARY);

	if (fp == -1)
	{
		switch (errno)
		{
		case ENOENT:			// File not found
			if (TextBuf != NULL)
			{
				GlobalUnlock(TextBufGlobal);
				GlobalFree(TextBufGlobal);
				TextBufGlobal = NULL;
				TextBuf = NULL;
			}

			return -1;

		case EACCES:			// Cannot access file
			if (TextBuf != NULL)
			{
				GlobalUnlock(TextBufGlobal);
				GlobalFree(TextBufGlobal);
				TextBufGlobal = NULL;
				TextBuf = NULL;
			}

			return -2;

		default:				// Unknown error occured
			if (TextBuf != NULL)
			{
				GlobalUnlock(TextBufGlobal);
				GlobalFree(TextBufGlobal);
				TextBufGlobal = NULL;
				TextBuf = NULL;
			}

			return -3;

		}
	}

	TextBufPos = 0;
	TextBufLength = 0;
	TextLineNr = 0;
	FileP = 0;
	FileP2 = 0;
	return fp;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 TextFileOpenAgain(LPSTR FileName, int32 mode)
{
	int32 fp = 0;

	WriteLnError = 0;
	CountedCR = 0;
	CountedLF = 0;
	TextLineNr = 0;
	EndOfLineChar = LF;

	if (!TextBufGlobal)
	{
		if ((TextBufGlobal = GlobalAlloc(GHND, TextBufSize)) == NULL)
			return -1;

		if ((TextBuf = (LPSTR) GlobalLock(TextBufGlobal)) == NULL)
			return -1;
	}

	if (mode == 0)
	{
		fp = open(FileName, O_RDONLY | O_BINARY);

		if (fp == -1)
		{
			switch (errno)
			{
			case ENOENT:		// File not found
				return -1;

			case EACCES:		// Cannot access file
				return -2;

			default:			// Unknown error occured
				return -3;

			}
		}
	}

	TextBufPos = 0;
	TextBufLength = 0;
	TextLineNr = 0;
	FileP = 0;
	FileP2 = 0;

	if (mode == 0)
		return fp;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 TextFileReset(int32 fp, int32 mode)
{
	WriteLnError = 0;
	CountedCR = 0;
	CountedLF = 0;
	TextLineNr = 0;
	EndOfLineChar = LF;

	FileSeek(fp, 0);
	TextBufPos = 0;
	TextBufLength = 0;
	TextLineNr = 0;
	FileP = 0;
	FileP2 = 0;
	return 0;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 TextFileOpenUTF8(LPSTR FileName)
{
#ifdef UC
	int32 fp;

	WriteLnError = 0;
	CountedCR = 0;
	CountedLF = 0;
	TextLineNr = 0;
	EndOfLineChar = LF;

	if (TextBuf != NULL)
		return -4;

	if ((TextBufGlobal = GlobalAlloc(GHND, TextBufSize)) == NULL)
		return -1;

	if ((TextBuf = (LPSTR) GlobalLock(TextBufGlobal)) == NULL)
		return -1;

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_STRING - 1) == 0)
	{
		if (TextBuf != NULL)
		{
			GlobalUnlock(TextBufGlobal);
			GlobalFree(TextBufGlobal);
			TextBufGlobal = NULL;
			TextBuf = NULL;
		}

		return -3;
	}

	fp = _wopen(FileStringUC, O_RDONLY | O_BINARY);

	if (fp == -1)
	{
		switch (errno)
		{
		case ENOENT:			// File not found
			if (TextBuf != NULL)
			{
				GlobalUnlock(TextBufGlobal);
				GlobalFree(TextBufGlobal);
				TextBufGlobal = NULL;
				TextBuf = NULL;
			}

			return -1;

		case EACCES:			// Cannot access file
			if (TextBuf != NULL)
			{
				GlobalUnlock(TextBufGlobal);
				GlobalFree(TextBufGlobal);
				TextBufGlobal = NULL;
				TextBuf = NULL;
			}

			return -2;

		default:				// Unknown error occured
			if (TextBuf != NULL)
			{
				GlobalUnlock(TextBufGlobal);
				GlobalFree(TextBufGlobal);
				TextBufGlobal = NULL;
				TextBuf = NULL;
			}

			return -3;

		}
	}

	TextBufPos = 0;
	TextBufLength = 0;
	TextLineNr = 0;
	FileP = 0;
	FileP2 = 0;
	return fp;
}

#else

	return TextFileOpen(FileName);
}
#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 TextFileWrite(LPSTR FileName)
{
	EndOfLineMode = 0;
	WriteLnError = 0;
	WriteTextLine[0] = 0;
	return FileOpenWrite(FileName);
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 TextFileWriteUTF8(LPSTR FileName)
{
#ifdef UC
	EndOfLineMode = 0;
	WriteLnError = 0;
	WriteTextLine[0] = 0;
	return FileOpenWriteUTF8(FileName);
}

#else

	return TextFileWrite(FileName);
}
#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 TextFileClose(int32 fp)
{
	int32 cnt;

	if (TextBuf != NULL)
	{
		GlobalUnlock(TextBufGlobal);
		GlobalFree(TextBufGlobal);
	}

	cnt = close(fp);
	TextBufGlobal = NULL;
	TextBuf = NULL;

	if (cnt == -1)
		return -1;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 TextFileWriteClose(int32 fp)
{
	int32 cnt;

	if (strlen(WriteTextLine) > 0)
		WriteLn(fp, "");

	cnt = close(fp);

	if (cnt == -1)
		return -1;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileClose(int32 fp)
{
	if (close(fp) == -1)
		return -1;

	return 0;

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileRead(int32 fp, void *Buf, int32 count, int32 * BytesRead)
{
	int32 cnt;

	*BytesRead = 0;
	cnt = read(fp, Buf, count);

	if (cnt == -1)
		return -1;

	*BytesRead = cnt;
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileWrite(int32 fp, void *Buf, int32 count, int32 * BytesWritten)
{
	int32 cnt;

	*BytesWritten = 0;
	cnt = write(fp, Buf, count);

	if (cnt == -1)
		return -1;

	*BytesWritten = cnt;
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 WriteLn2(int32 fp, int32 EndOfLineMode, LPSTR LineBuf)
{
	int32 lengte, niks;
	char EndOfLine[] = "\r\n";

	/*
	  if ((lengte=strlen(WriteTextLine))>0) {
	    if (FileWrite(fp,WriteTextLine,lengte,&niks)==-1) {
	      return -1;
	      WriteLnError=1;
	    }
	    WriteTextLine[0]=0;
	  }
	*/
	if ((lengte = strlen(LineBuf)) > 0)
	{
		if (FileWrite(fp, LineBuf, lengte, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}
	}

	switch (EndOfLineMode)
	{
	case 0:
		if (FileWrite(fp, &EndOfLine, 2, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}

		break;

	case 1:
		if (FileWrite(fp, &EndOfLine[0], 1, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}

		break;

	case 2:
		if (FileWrite(fp, &EndOfLine[1], 1, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}

		break;
	}

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 WriteLn(int32 fp, LPSTR LineBuf)
{
	int32 lengte, niks;
	char EndOfLine[] = "\r\n";

	/*
	  if ((lengte=strlen(WriteTextLine))>0) {
	    if (FileWrite(fp,WriteTextLine,lengte,&niks)==-1) {
	      return -1;
	      WriteLnError=1;
	    }
	    WriteTextLine[0]=0;
	  }
	*/
	if ((lengte = strlen(LineBuf)) > 0)
	{
		if (FileWrite(fp, LineBuf, lengte, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}
	}

	switch (EndOfLineMode)
	{
	case 0:
		if (FileWrite(fp, &EndOfLine, 2, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}

		break;

	case 1:
		if (FileWrite(fp, &EndOfLine[0], 1, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}

		break;

	case 2:
		if (FileWrite(fp, &EndOfLine[1], 1, &niks) == -1)
		{
			WriteLnError = 1;
			return -1;
		}

		break;
	}

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 WriteStr(int32 fp, LPSTR LineBuf)
{
	if ((strlen(WriteTextLine) + strlen(LineBuf)) > 85)
	{
		WriteLn(fp, "");
		WriteTextLine[0] = 0;
	}

	strcat(WriteTextLine, LineBuf);
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 WriteToFile(int32 fp, LPSTR LineBuf)
{
	int32 lengte, niks;

	if ((lengte = strlen(LineBuf)) > 0)
	{
		if (FileWrite(fp, LineBuf, lengte, &niks) == -1)
			return -1;
	}

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ReadLnToChar(int32 fp, char EndOfLineChar, LPSTR LineBuf)
{
	int32 BytesRead, LineLength, OldPos, count, count2, Stop;
	char *BufP;

	Stop = 0;
	LineLength = 0;
	OldPos = TextBufPos;

	while (!Stop)
	{
		if (TextBufLength == 0)
		{
			BytesRead = 0;

			if ((FileRead(fp, TextBuf, TextBufSize, &BytesRead) == -1) || (BytesRead == 0))
			{
				if (LineLength > 0)
				{
					TextLineNr++;
					LineBuf[LineLength] = 0;
					return LineLength;
				}
				else
					return -2;
			}

			TextBufLength = BytesRead;
			TextBufPos = 0;
			OldPos = TextBufPos;
			FileP2++;
		}

		BufP = &TextBuf[TextBufPos];
		count = TextBufLength - TextBufPos;

		while ((count > 0) && (*BufP != EndOfLineChar))
		{
			count--;
			BufP++;
			TextBufPos++;
		}

		if (LineLength < MaxLineLength)
		{
			count2 = min(MaxLineLength - LineLength, TextBufPos - OldPos);
			memmove(&LineBuf[LineLength], &TextBuf[OldPos], count2);
			LineLength += count2;
		}

		if (count == 0)
			TextBufLength = 0;
		else
		{
			OldPos = TextBufPos;
			Stop = 1;
		}
	}

	TextBufPos++;

	if (TextBufPos == TextBufLength)
		TextBufLength = 0;

	TextLineNr++;
	FileP = (FileP2 - 1) * TextBufSize + TextBufPos;
	LineBuf[LineLength] = 0;
	return LineLength;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ReadLn2(int32 fp, LPSTR LineBuf)
{
	int32 BytesRead, cnt, LineLength, OldPos, count, count2, Stop;
	char *BufP, kar;

	LineLength = 0;
	Stop = 0;
	BytesRead = 0;
	OldPos = TextBufPos;

	cnt = 1;

	while (!Stop)
	{
		if (TextBufLength == 0)
		{
			BytesRead = 0;

			if ((FileRead(fp, TextBuf, TextBufSize, &BytesRead) == -1) || (BytesRead == 0))
			{
				if (LineLength > 0)
				{
					cnt = LineLength - 1;

					while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
						cnt--;

					LineLength = cnt + 1;
					TextLineNr++;
					LineBuf[LineLength] = 0;
					return LineLength;
				}
				else
					return -2;
			}

			TextBufLength = BytesRead;
			TextBufPos = 0;
			OldPos = TextBufPos;
			FileP2++;
		}

		if (TextLineNr == 0)
		{
			for (cnt = 0; cnt < min(1024, BytesRead); cnt++)
			{
				kar = TextBuf[cnt];

				switch (kar)
				{
				case CR:
					CountedCR++;
					break;

				case LF:
					CountedLF++;
					break;
				}
			}

			EndOfLineChar = LF;

			if (CountedCR == 0)
				EndOfLineMode = 1;
			else
			{
				if (CountedLF == 0)
				{
					EndOfLineMode = 2;
					EndOfLineChar = CR;
				}
			}
		}

		BufP = &TextBuf[TextBufPos];
		count = TextBufLength - TextBufPos;

		while ((count > 0) && (*BufP != EndOfLineChar))
		{
			count--;
			BufP++;
			TextBufPos++;
		}

		if (LineLength < MaxLineLength)
		{
			count2 = min(MaxLineLength - LineLength, TextBufPos - OldPos);
			memmove(&LineBuf[LineLength], &TextBuf[OldPos], count2);
			LineLength += count2;
		}

		if (count == 0)
			TextBufLength = 0;
		else
		{
			OldPos = TextBufPos;
			Stop = 1;
		}
	}

	if (LineLength > 0)
	{
		cnt = LineLength - 1;

		while ((cnt >= 0) && (LineBuf[cnt] == 13))
			cnt--;

		LineLength = cnt + 1;
	}

	TextBufPos++;

	if (TextBufPos == TextBufLength)
		TextBufLength = 0;

	TextLineNr++;
	FileP = (FileP2 - 1) * TextBufSize + TextBufPos;
	LineBuf[LineLength] = 0;
	return LineLength;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 ReadLn(int32 fp, LPSTR LineBuf)
{
	int32 BytesRead, cnt, LineLength, OldPos, count, count2, Stop;
	char *BufP, kar;

	Stop = 0;
	LineLength = 0;
	BytesRead = 0;
	OldPos = TextBufPos;

	cnt = 1;

	while (!Stop)
	{
		if (TextBufLength == 0)
		{
			BytesRead = 0;

			if ((FileRead(fp, TextBuf, TextBufSize, &BytesRead) == -1) || (BytesRead == 0))
			{
				if (LineLength > 0)
				{
					cnt = LineLength - 1;

					while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
						cnt--;

					LineLength = cnt + 1;
					TextLineNr++;
					LineBuf[LineLength] = 0;
					return LineLength;
				}
				else
					return -2;
			}

			TextBufLength = BytesRead;
			TextBufPos = 0;
			OldPos = TextBufPos;
			FileP2++;
		}

		if (TextLineNr == 0)
		{
			for (cnt = 0; cnt < min(1024, BytesRead); cnt++)
			{
				kar = TextBuf[cnt];

				switch (kar)
				{
				case CR:
					CountedCR++;
					break;

				case LF:
					CountedLF++;
					break;
				}
			}

			EndOfLineChar = LF;

			if (CountedCR == 0)
				EndOfLineMode = 1;
			else
			{
				if (CountedLF == 0)
				{
					EndOfLineMode = 2;
					EndOfLineChar = CR;
				}
			}
		}

		BufP = &TextBuf[TextBufPos];
		count = TextBufLength - TextBufPos;

		while ((count > 0) && (*BufP != EndOfLineChar))
		{
			count--;
			BufP++;
			TextBufPos++;
		}

		if (LineLength < MaxLineLength)
		{
			count2 = min(MaxLineLength - LineLength, TextBufPos - OldPos);
			memmove(&LineBuf[LineLength], &TextBuf[OldPos], count2);
			LineLength += count2;
		}

		if (count == 0)
			TextBufLength = 0;
		else
		{
			OldPos = TextBufPos;
			Stop = 1;
		}
	}

	if (LineLength > 0)
	{
		cnt = LineLength - 1;

		while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
			cnt--;

		LineLength = cnt + 1;
	}

	TextBufPos++;

	if (TextBufPos == TextBufLength)
		TextBufLength = 0;

	TextLineNr++;
	FileP = (FileP2 - 1) * TextBufSize + TextBufPos;
	LineBuf[LineLength] = 0;
	return LineLength;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ReadLnWithMaxLength(int32 fp, LPSTR LineBuf, int32 MaxCurrentLineLength)
{
	int32 BytesRead, cnt, LineLength, OldPos, count, count2, Stop;
	char *BufP, kar;

	Stop = 0;
	BytesRead = 0;
	LineLength = 0;
	OldPos = TextBufPos;

	cnt = 1;

	while (!Stop)
	{
// ***************************************************************************************************
		if (TextBufLength == 0)
		{
			BytesRead = 0;

			if ((FileRead(fp, TextBuf, TextBufSize, &BytesRead) == -1) || (BytesRead == 0))
			{
				if (LineLength > 0)
				{
					cnt = LineLength - 1;

					while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
						cnt--;

					LineLength = cnt + 1;
					TextLineNr++;
					LineBuf[LineLength] = 0;
					return LineLength;
				}
				else
					return -2;
			}

			TextBufLength = BytesRead;
			TextBufPos = 0;
			OldPos = TextBufPos;
			FileP2++;
		}

// ***************************************************************************************************
		if (TextLineNr == 0)
		{
			for (cnt = 0; cnt < min(1024, BytesRead); cnt++)
			{
				kar = TextBuf[cnt];

				switch (kar)
				{
				case CR:
					CountedCR++;
					break;

				case LF:
					CountedLF++;
					break;
				}
			}

			EndOfLineChar = LF;

			if (CountedCR == 0)
				EndOfLineMode = 1;
			else
			{
				if (CountedLF == 0)
				{
					EndOfLineMode = 2;
					EndOfLineChar = CR;
				}
			}
		}

// ***************************************************************************************************
		BufP = &TextBuf[TextBufPos];
		count = TextBufLength - TextBufPos;

		while ((count > 0) && (*BufP != EndOfLineChar))
		{
			count--;
			BufP++;
			TextBufPos++;
		}

		if (LineLength < MaxCurrentLineLength)
		{
			count2 = min(MaxCurrentLineLength - LineLength, TextBufPos - OldPos);
			memmove(&LineBuf[LineLength], &TextBuf[OldPos], count2);
			LineLength += count2;
		}

		if (count == 0)
			TextBufLength = 0;
		else
		{
			OldPos = TextBufPos;
			Stop = 1;
		}
	}

	if ((LineLength < MaxCurrentLineLength) && (LineLength > 0))
	{
		cnt = LineLength - 1;

		while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
			cnt--;

		LineLength = cnt + 1;
	}

	TextBufPos++;

	if (TextBufPos == TextBufLength)
		TextBufLength = 0;

	TextLineNr++;
	FileP = (FileP2 - 1) * TextBufSize + TextBufPos;

	if (LineLength < MaxCurrentLineLength)
		LineBuf[LineLength] = 0;

	return LineLength;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int CheckForWritingAndOpen(LPSTR FileName, int32 FileSize, HWND Window)
{
	int32 fp, lengte;
	int64 BytesFree;
	int64 i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;
	char Dir[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	PGETDISKFREESPACEEX pGetDiskFreeSpaceEx;
	DWORD dwSectPerClust, dwBytesPerSect, dwFreeClusters, dwTotalClusters;
	int32 fResult;

	lengte = strlen(FileName);
	BytesFree = 0;
	strcpy(Dir, FileName);
	Dir[3] = 0;

	if ((lengte < 3) || (strncmp(FileName, "\\\\", 2) != 0))
	{
		pGetDiskFreeSpaceEx =
		    (PGETDISKFREESPACEEX) GetProcAddress(GetModuleHandle("kernel32.dll"), "GetDiskFreeSpaceExA");

		if (pGetDiskFreeSpaceEx)
		{
			fResult =
			    pGetDiskFreeSpaceEx(Dir, (PULARGE_INTEGER) & i64FreeBytesToCaller, (PULARGE_INTEGER) & i64TotalBytes,
			                        (PULARGE_INTEGER) & i64FreeBytes);

			// Process GetDiskFreeSpaceEx results.
			if (fResult)
				BytesFree = i64FreeBytes;
		}
		else
		{
			fResult = GetDiskFreeSpaceA(Dir, &dwSectPerClust, &dwBytesPerSect, &dwFreeClusters, &dwTotalClusters);

			// Process GetDiskFreeSpace results.
			if (fResult)
				BytesFree = dwFreeClusters * dwSectPerClust * dwBytesPerSect;
		}

		if (BytesFree < (int64) FileSize)
		{
			if (Window != NULL)
				MessageBox(Window, "Out of diskspace", "Error", MB_OK);
			else
				printf("Out of diskspace (%i)\n", FileSize);

			return -5;
		}
	}

	fp = open(FileName, O_RDWR | O_BINARY | O_TRUNC);

	if (fp != -1)
		return fp;

	switch (errno)
	{
	case ENOENT:
		fp = open(FileName, O_RDWR | O_CREAT | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				sprintf(str, "File %s is read only, or an other error occured", FileName);

				if (Window != NULL)
					MessageBox(Window, str, "Error", MB_OK);
				else
					printf("%s\n", str);

				return -2;

			default:			// Unknown error occured
				if (Window != NULL)
					MessageBox(Window, "An unknown error occured", "Error", MB_OK);
				else
					printf("An unknown error occured\n");

				return -3;
			}
		}

		return fp;

	case EACCES:				// Cannot access file
		sprintf(str, "File %s is read only, or an other error occured", FileName);

		if (Window != NULL)
			MessageBox(Window, str, "Error", MB_OK);
		else
			printf("%s\n", str);

		return -2;
	}

	return -3;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int CheckForWritingAndOpenUTF8(LPSTR FileName, int32 FileSize, HWND Window)
{
#ifdef UC
	int32 fp;
	int64 BytesFree;
	int64 i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;
	char Dir[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
	PGETDISKFREESPACEEX pGetDiskFreeSpaceEx;
	DWORD dwSectPerClust, dwBytesPerSect, dwFreeClusters, dwTotalClusters;
	int32 fResult;

	BytesFree = 0;
	strcpy(Dir, FileName);
	Dir[3] = 0;

	pGetDiskFreeSpaceEx = (PGETDISKFREESPACEEX) GetProcAddress(GetModuleHandle("kernel32.dll"), "GetDiskFreeSpaceExA");

	if (pGetDiskFreeSpaceEx)
	{
		fResult =
		    pGetDiskFreeSpaceEx(Dir, (PULARGE_INTEGER) & i64FreeBytesToCaller, (PULARGE_INTEGER) & i64TotalBytes,
		                        (PULARGE_INTEGER) & i64FreeBytes);

		// Process GetDiskFreeSpaceEx results.
		if (fResult)
			BytesFree = i64FreeBytes;
	}
	else
	{
		fResult = GetDiskFreeSpaceA(Dir, &dwSectPerClust, &dwBytesPerSect, &dwFreeClusters, &dwTotalClusters);

		// Process GetDiskFreeSpace results.
		if (fResult)
			BytesFree = dwFreeClusters * dwSectPerClust * dwBytesPerSect;
	}

	if (BytesFree < (int64) FileSize)
	{
		if (Window != NULL)
			MessageBox(Window, "Out of diskspace", "Error", MB_OK);
		else
			printf("Out of diskspace (%i)\n", FileSize);

		return -5;
	}

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_UC_STRING - 1) == -1)
		return -2;

	fp = _wopen(FileStringUC, O_RDWR | O_BINARY | O_TRUNC);

	if (fp != -1)
		return fp;

	switch (errno)
	{
	case ENOENT:
		fp = _wopen(FileStringUC, O_RDWR | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE);

		if (fp == -1)
		{
			switch (errno)
			{
			case EACCES:		// Cannot access file
				sprintf(str, "File %s is read only, or an other error occured", FileName);

				if (Window != NULL)
					MessageBox(Window, str, "Error", MB_OK);
				else
					printf("%s\n", str);

				return -2;

			default:			// Unknown error occured
				if (Window != NULL)
					MessageBox(Window, "An unknown error occured", "Error", MB_OK);
				else
					printf("An unknown error occured\n");

				return -3;
			}
		}

		return fp;

	case EACCES:				// Cannot access file
		sprintf(str, "File %s is read only, or an other error occured", FileName);

		if (Window != NULL)
			MessageBox(Window, str, "Error", MB_OK);
		else
			printf("%s\n", str);

		return -2;
	}

	return -3;
}

#else

	return CheckForWritingAndOpen(FileName, FileSize, Window);
}
#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileSeek(int32 fp, int32 FilePos)
{
	return lseek(fp, FilePos, SEEK_SET);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileSize(LPSTR FileName)
{

	int32 pos2, fp;

	fp = open(FileName, O_RDONLY | O_BINARY);

	if (fp == -1)
		return -1;

	if (lseek(fp, 0, SEEK_END) == -1)
		return -2;

	pos2 = tell(fp);

	if (pos2 == -1)
		return -3;

	if (close(fp))
		return -1;

	return pos2;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 FileSizeUTF8(LPSTR FileName)
{
#ifdef UC
	int32 pos2, fp;

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_UC_STRING - 1) == -1)
		return -2;

	fp = _wopen(FileStringUC, O_RDONLY | O_BINARY);

	if (fp == -1)
		return -1;

	if (lseek(fp, 0, SEEK_END) == -1)
		return -2;

	pos2 = tell(fp);

	if (pos2 == -1)
		return -3;

	if (close(fp))
		return -1;

	return pos2;
}

#else

	return FileSize(FileName);
}
#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileExists(LPSTR FileName)
{
	int32 fp;

	fp = open(FileName, O_RDONLY | O_BINARY);

	if (fp == -1)
	{
		switch (errno)
		{
		case ENOENT:			// File not found
			return -1;
			break;

		case EACCES:			// Cannot access file
			return -2;

		default:				// Unknown error occured
			return -3;

		}
	}

	close(fp);
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 FileExistsW(LPWSTR FileName)
{
	int32 fp;

	fp = _wopen(FileName, O_RDONLY | O_BINARY);

	if (fp == -1)
	{
		switch (errno)
		{
		case ENOENT:			// File not found
			return -1;
			break;

		case EACCES:			// Cannot access file
			return -2;

		default:				// Unknown error occured
			return -3;

		}
	}

	close(fp);
	return 0;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 FileExistsUTF8(LPSTR FileName)
{
#ifdef UC
	int32 fp;

	if (Utf8ToUnicode(FileName, FileStringUC, MAX_LENGTH_UC_STRING - 1) == -1)
		return -2;

	fp = _wopen(FileStringUC, O_RDONLY | O_BINARY);

	if (fp == -1)
	{
		switch (errno)
		{
		case ENOENT:			// File not found
			return -1;
			break;

		case EACCES:			// Cannot access file
			return -2;

		default:				// Unknown error occured
			return -3;

		}
	}

	close(fp);
	return 0;
}

#else

	return FileExists(FileName);
}
#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 DirectoryExists(LPSTR Dir)
{
	char CurrentDir[MAX_LENGTH_STRING];

	GetCurrentDirectory(MAX_LENGTH_STRING - 10, CurrentDir);

	if (SetCurrentDirectory(Dir))
	{
		SetCurrentDirectory(CurrentDir);
		return 0;
	}

	return -1;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 DirectoryExistsUTF8(LPSTR Dir)
{
#ifdef UC
	WCHAR CurrentDir[MAX_LENGTH_STRING];

	GetCurrentDirectoryW(MAX_LENGTH_STRING - 10, CurrentDir);

	if (Utf8ToUnicode(Dir, FileStringUC, MAX_LENGTH_UC_STRING - 1) == -1)
		return -2;

	if (SetCurrentDirectoryW(FileStringUC))
	{
		SetCurrentDirectoryW(CurrentDir);
		return 0;
	}

	return -1;
}

#else

	return DirectoryExists(Dir);
}
#endif

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetCurrentDirectoryUTF8(int32 MaxLength, LPSTR Dir)
{
	int32 res;

#ifdef UC

	res = GetCurrentDirectoryW(MaxLength - 10, FileStringUC);
	Dir[0] = 0;

	if (UnicodeToUtf8(FileStringUC, Dir, MAX_LENGTH_UC_STRING - 1) == -1)
		return -2;

#else

	res = GetCurrentDirectory(MaxLength, Dir);

#endif

	return res;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 FileCurrentPointer(int32 fp)
{
	return tell(fp);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetDirFromFileName(LPSTR Dir, LPSTR FileName)
{
	int32 cnt, lengte;

	Dir[0] = 0;
	lengte = strlen(FileName);

	if (lengte < 2)
		return -1;

	cnt = lengte - 1;

	while ((cnt > 1) && (FileName[cnt] != '\\'))
		cnt--;

	if (cnt > 1)
	{
		strncpy(Dir, FileName, cnt);
		Dir[cnt] = 0;
		return 0;
	}

	return -1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetFilePartFromFileName(LPSTR FilePart, LPSTR FileName)
{
	int32 cnt, lengte, Found;

	FilePart[0] = 0;
	lengte = strlen(FileName);

	if (lengte == 0)
		return -1;

	Found = -1;

	for (cnt = lengte - 1; cnt >= 0; cnt--)
	{
		if ((Found == -1) && (FileName[cnt] == '\\'))
			Found = cnt;
	}

	strcpy(FilePart, (LPSTR) & FileName[Found + 1]);
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void CutExtensionFileName(LPSTR FileName)
{
	int32 cnt, lengte;

	lengte = strlen(FileName);

	if (lengte < 3)
		return;

	cnt = lengte - 1;

	while ((cnt > 0) && (FileName[cnt] != '.'))
		cnt--;

	if (cnt > 0)
		FileName[cnt] = 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void ConvertPathToWindowsStyle(LPSTR FileName)
{
	int32 cnt, lengte;

	lengte = strlen(FileName);

	for (cnt = 0; cnt < lengte; cnt++)
	{
		if (FileName[cnt] == '/')
			FileName[cnt] = '\\';
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void GetExtensionFileName(LPSTR Extension, LPSTR FileName)
{
	int32 cnt, lengte;

	Extension[0] = 0;
	lengte = strlen(FileName);

	if (lengte < 2)
		return;

	cnt = lengte - 1;

	while ((cnt >= 0) && (FileName[cnt] != '.'))
		cnt--;

	if (cnt >= 0)
		strcpy(Extension, (LPSTR) & FileName[cnt + 1]);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void GetNameWithOutExtensionFromFileName(LPSTR FileName, LPSTR Name)
{
	int32 lengte, cnt, cnt2, res;

	Name[0] = 0;
	lengte = strlen(FileName);
	cnt = lengte - 1;

	while ((cnt >= 0) && (FileName[cnt] != '.'))
		cnt--;

	if (cnt == -1)
		cnt = lengte;

	cnt2 = lengte - 1;

	while ((cnt2 >= 0) && (FileName[cnt2] != '\\'))
		cnt2--;

	if (cnt2 == lengte - 1)
		return;

	res = cnt - cnt2 - 1;

	if (res > 0)
	{
		memmove(Name, &FileName[cnt2 + 1], res);
		Name[res] = 0;
	}

}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void ReplaceExtensionFileName(LPSTR FileName, LPSTR Extension)
{
	int32 cnt, lengte;

	lengte = strlen(FileName);

	if (lengte == 0)
		return;

	cnt = lengte - 1;

	while ((cnt > 1) && (FileName[cnt] != '.'))
		cnt--;

	if (cnt > 0)
		FileName[cnt] = 0;

	strcat(FileName, ".");
	strcat(FileName, Extension);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void AddExtensionToFileName(LPSTR FileName, LPSTR ExtensionToAdd)
{
	char Extension[MAX_LENGTH_STRING];

	GetExtensionFileName(Extension, FileName);

	if (Extension[0] == 0)
	{
		strcat(FileName, ".");
		strcat(FileName, ExtensionToAdd);
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void AddBackSlash(LPSTR FileName)
{
	if (FileName[strlen(FileName) - 1] != '\\')
		strcat(FileName, "\\");
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void CutBackSlashDir(LPSTR Dir)
{
	int32 lengte;

	lengte = strlen(Dir);

	if (lengte > 3)
	{
		if (Dir[lengte - 1] == '\\')
			Dir[lengte - 1] = 0;
	}
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetString(LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

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

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos);

	Result[cnt - pos] = 0;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt], l - cnt + 1);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetStrings(LPSTR Str, int32 MaxLength, char **StrP, int32 MaxStrings)
{
	int32 cnt, cnt2, StringLength, pos, StrCnt, StartCnt;

	StringLength = min(MaxLength - 1, (int32) strlen(Str));

	if (StringLength == 0)
	{
		Str[0] = 0;
		StrP[0] = &Str[0];
		return 0;
	}

	StrCnt = 0;
	cnt = 0;
	pos = 0;

	while (StrCnt < MaxStrings)
	{
		StartCnt = cnt;

		while ((cnt < StringLength) && ((Str[cnt] == ' ') || (Str[cnt] == '\t')))
			cnt++;

		if (cnt > StartCnt)
		{
			Str[StartCnt] = 0;
			pos = StartCnt;
		}

		if (cnt == StringLength)
		{
			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
				StrP[cnt2] = &Str[pos];

			return StrCnt;
		}

		StrP[StrCnt] = &Str[cnt];

		while ((cnt < StringLength) && (Str[cnt] != ' ') && (Str[cnt] != '\t'))
			cnt++;

		pos = cnt;
		Str[cnt] = 0;
		cnt++;
		StrCnt++;

		if (cnt >= StringLength)
		{
			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
				StrP[cnt2] = &Str[pos];

			return StrCnt;
		}
	}

	return StrCnt;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

/*
1=0,4=0


  1 = 0 , 4 = 0
  1 = 0 , 4 = 0 , 5
  1 = 0 , 4 = 0 , 5 , 7=3
  1 = 0 , 4 = 0 , 5 =

*/

int32 GetAttrNamesAndValues(LPSTR Str, int32 MaxLength, char **AttrNamesP, char **AttrValuesP, int32 MaxStrings)
{
	int32 cnt, cnt2, StringLength, pos, StrCnt;

	StrCnt = 0;
	StringLength = min(MaxLength - 1, (int32) strlen(Str));

	if (StringLength == 0)
	{
		pos = 0;

		for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
		{
			AttrNamesP[cnt2] = &Str[pos];
			AttrValuesP[cnt2] = &Str[pos];
		}

		return 0;
	}

	cnt = 0;
	pos = 0;

	while (StrCnt < MaxStrings)
	{
		pos = cnt;

		while ((cnt < StringLength) && ((Str[cnt] == ' ') || (Str[cnt] == '\t')))
			cnt++;

		if (cnt == StringLength)
		{
			pos = cnt;

			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
			{
				AttrNamesP[cnt2] = &Str[pos];
				AttrValuesP[cnt2] = &Str[pos];
			}

			return StrCnt;
		}

		pos = cnt;

		if (Str[cnt] == ',')
		{
			cnt++;
			continue;
		}

		// Found an attribute name
		AttrNamesP[StrCnt] = &Str[cnt];

		while ((cnt < StringLength) && (Str[cnt] != ' ') && (Str[cnt] != '\t') && (Str[cnt] != ',')
		        && (Str[cnt] != '='))
			cnt++;

		if (cnt == StringLength)
		{
			// No attribute value -> zero string and exit
			AttrValuesP[StrCnt] = &Str[cnt];
			StrCnt++;
			pos = cnt;

			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
			{
				AttrNamesP[cnt2] = &Str[pos];
				AttrValuesP[cnt2] = &Str[pos];
			}

			return StrCnt;
		}

		if (Str[cnt] == ',')
		{
			Str[cnt] = 0;		// terminate attribute name
			AttrValuesP[StrCnt] = &Str[cnt];
			StrCnt++;
			cnt++;
			continue;
		}

		if (Str[cnt] != '=')
		{
			/*
			      name  =value
			*/
			Str[cnt] = 0;		// terminate attribute name
			pos = cnt;
			cnt++;

			// Skip spaces and tabs until =
			while ((cnt < StringLength) && (Str[cnt] != ',') && (Str[cnt] != '='))
				cnt++;

			if (cnt == StringLength)
			{
				AttrValuesP[StrCnt] = &Str[cnt];
				StrCnt++;
				pos = cnt;

				for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
				{
					AttrNamesP[cnt2] = &Str[pos];
					AttrValuesP[cnt2] = &Str[pos];
				}

				return StrCnt;
			}

			if (Str[cnt] == ',')
			{
				Str[cnt] = 0;	// terminate attribute name
				AttrValuesP[StrCnt] = &Str[cnt];
				StrCnt++;
				cnt++;
				continue;
			}
		}
		else
		{
			// Found a = character
			Str[cnt] = 0;		// terminate attribute name
		}

		cnt++;
		pos = cnt;

		if (Str[cnt] == ',')
		{
			Str[cnt] = 0;		// terminate attribute value
			AttrValuesP[StrCnt] = &Str[cnt];
			StrCnt++;
			cnt++;
			continue;
		}

		// Skip spaces and tabs
		while ((cnt < StringLength) && ((Str[cnt] == ' ') || (Str[cnt] == '\t')))
			cnt++;

		if (cnt == StringLength)
		{
			AttrValuesP[StrCnt] = &Str[cnt];
			pos = cnt;

			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
			{
				AttrNamesP[cnt2] = &Str[pos];
				AttrValuesP[cnt2] = &Str[pos];
			}

			return StrCnt;
		}

		if (Str[cnt] == ',')
		{
			Str[cnt] = 0;		// terminate attribute value
			AttrValuesP[StrCnt] = &Str[cnt];
			StrCnt++;
			cnt++;
			continue;
		}

		pos = cnt;
		// Found an attribute value
		AttrValuesP[StrCnt] = &Str[cnt];
		StrCnt++;
		cnt++;

		while ((cnt < StringLength) && (Str[cnt] != ' ') && (Str[cnt] != '\t') && (Str[cnt] != ','))
			cnt++;

		if (cnt == StringLength)
		{
			pos = cnt;

			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
			{
				AttrNamesP[cnt2] = &Str[pos];
				AttrValuesP[cnt2] = &Str[pos];
			}

			return StrCnt;
		}

		if (Str[cnt] != ',')
		{
			Str[cnt] = 0;		// terminate attribute value
			cnt++;

			// Skip spaces and tabs until ,
			while ((cnt < StringLength) && (Str[cnt] != ','))
				cnt++;
		}
		else
		{
			Str[cnt] = 0;		// terminate attribute value
		}

		if (cnt == StringLength)
		{
			pos = cnt;

			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
			{
				AttrNamesP[cnt2] = &Str[pos];
				AttrValuesP[cnt2] = &Str[pos];
			}

			return StrCnt;
		}

		cnt++;
	}

	pos = StringLength;

	for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
	{
		AttrNamesP[cnt2] = &Str[pos];
		AttrValuesP[cnt2] = &Str[pos];
	}

	return StrCnt;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetStringsWithQuotes(LPSTR Str, int32 MaxLength, char **StrP, int32 MaxStrings)
{
	int32 cnt, cnt2, StringLength, pos, StrCnt, StartCnt;

	StringLength = min(MaxLength - 1, (int32) strlen(Str));

	if (StringLength == 0)
	{
		Str[0] = 0;
		StrP[0] = &Str[0];
		return 0;
	}

	StrCnt = 0;
	cnt = 0;
	pos = 0;

	while (StrCnt < MaxStrings)
	{
		StartCnt = cnt;

		while ((cnt < StringLength) && ((Str[cnt] == ' ') || (Str[cnt] == '\t')))
			cnt++;

		if (cnt > StartCnt)
		{
			Str[StartCnt] = 0;
			pos = StartCnt;
		}

		if (cnt == StringLength)
		{
			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
				StrP[cnt2] = &Str[pos];

			return StrCnt;
		}

		if (Str[cnt] == '\"')
		{
			cnt++;
			StrP[StrCnt] = &Str[cnt];

			while ((cnt < StringLength) && (Str[cnt] != '\"'))
				cnt++;
		}
		else
		{
			StrP[StrCnt] = &Str[cnt];

			while ((cnt < StringLength) && (Str[cnt] != ' ') && (Str[cnt] != '\"') && (Str[cnt] != '\t'))
				cnt++;
		}

		pos = cnt;
		Str[cnt] = 0;
		cnt++;
		StrCnt++;

		if (cnt >= StringLength)
		{
			for (cnt2 = StrCnt; cnt2 < MaxStrings; cnt2++)
				StrP[cnt2] = &Str[pos];

			return StrCnt;
		}
	}

	return StrCnt;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetCommaString(LPSTR Str, LPSTR Result)
{
	int32 cnt, lengte;

	Result[0] = 0;
	lengte = strlen(Str);

	if (lengte == 0)
		return;

	cnt = 0;

	while ((cnt < lengte) && (Str[cnt] != ','))
		cnt++;

	if (cnt > 0)
		memmove(Result, Str, cnt);

	Result[cnt] = 0;

	if (cnt == lengte)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt + 1], lengte - cnt);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetStringTab(LPSTR Str, LPSTR Result)
{
	int32 cnt, l;
	memset(Result, 0, MAX_LENGTH_STRING);
	l = strlen(Str);
	cnt = 0;

	while ((cnt < l) && (Str[cnt] != '\t'))
		cnt++;

	if (cnt > 0)
		memmove(Result, Str, cnt);

	if (cnt >= l - 1)
	{
		Str[0] = 0;
		return;
	}

	if (l - cnt > 0)
		memmove(Str, &Str[cnt + 1], l - cnt);

	l = strlen(Result);
	cnt = l - 1;

	while ((cnt >= 0) && (Result[cnt] == ' '))
		cnt--;

	Result[cnt + 1] = 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void GetString3(char EscapeChar, LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

//  memset(Result,0,MAX_LENGTH_STRING);
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

	while ((cnt < l) && (Str[cnt] != EscapeChar))
		cnt++;

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos);

	Result[cnt - pos] = 0;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt], l - cnt + 1);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetQuoteString(LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

	cnt = 0;

	while ((cnt < l) && (Str[cnt] != '\"'))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	cnt++;
	pos = cnt;

	while ((cnt < l) && (Str[cnt] != '\"'))
		cnt++;

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos);

	Result[cnt - pos] = 0;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt + 1], l - cnt);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void GetQuoteString2(LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

	cnt = 0;

	while ((cnt < l) && (Str[cnt] != '\"'))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	pos = cnt;
	cnt++;

	while ((cnt < l) && ((Str[cnt] != '\"') || (Str[cnt - 1] == '\\')))
		cnt++;

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos + 1);

	if (cnt == l)
		Result[cnt - pos] = '\"';

	Result[cnt - pos + 1] = 0;

	if (cnt >= l - 1)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt + 1], l - cnt);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetBracesString(LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

	cnt = 0;

	while ((cnt < l) && (Str[cnt] != '['))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	cnt++;
	pos = cnt;

	while ((cnt < l) && (Str[cnt] != ']'))
		cnt++;

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos);

	Result[cnt - pos] = 0;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt + 1], l - cnt);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


void GetPercentageString(LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

	cnt = 0;

	while ((cnt < l) && (Str[cnt] != '%'))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	cnt++;
	pos = cnt;

	while ((cnt < l) && (Str[cnt] != '%'))
		cnt++;

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos);

	Result[cnt - pos] = 0;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt + 1], l - cnt);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetParenthesisString(LPSTR Str, LPSTR Result)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

	cnt = 0;

	while ((cnt < l) && (Str[cnt] != '('))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	cnt++;
	pos = cnt;

	while ((cnt < l) && (Str[cnt] != ')'))
		cnt++;

	if (cnt > pos)
		memmove(Result, &Str[pos], cnt - pos);

	Result[cnt - pos] = 0;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	memmove(Str, &Str[cnt + 1], l - cnt);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetSpecialString(LPSTR Str, LPSTR Result, int32 mode)
{
	int32 cnt, l, pos;

	Result[0] = 0;
	l = strlen(Str);

	if (l == 0)
		return;

	if ((mode & 0x100) == 0)
	{	// Check for maximum length of MAX_LENGTH_STRING characters
		if (l >= MAX_LENGTH_STRING)
			return;
	}

	cnt = 0;

	while ((cnt < l) && ((Str[cnt] == ' ') || (Str[cnt] == '\t')))
		cnt++;

	if (cnt == l)
	{
		Str[0] = 0;
		return;
	}

	if (Str[cnt] == '\"')
		GetQuoteString((LPSTR) & Str[cnt], Result);
	else
	{
		pos = cnt;
		cnt++;

		while ((cnt < l) && (Str[cnt] != ' ') && (Str[cnt] != '\t'))
			cnt++;

		if (cnt > pos)
			memmove(Result, &Str[pos], cnt - pos);

		Result[cnt - pos] = 0;

		if (cnt == l)
		{
			Str[0] = 0;
			return;
		}

		memmove(Str, &Str[cnt], l - cnt + 1);
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 GetStringValue(LPSTR Line, LPSTR Key, LPSTR StrValue)
{
	int32 cnt, cnt2, lengte;

	lengte = min(199, strlen(Line));

	Key[0] = 0;
	StrValue[0] = 0;
	cnt = 0;

	while ((cnt < lengte) && (Line[cnt] != '='))
		cnt++;

	if (cnt == lengte)
		return 0;

	strncpy(Key, Line, 199);
	Key[199] = 0;
	Key[cnt] = 0;
	lengte = strlen(Key);
	cnt2 = lengte - 1;

	while ((cnt > 0) && (Key[cnt2] == ' '))
	{
		Key[cnt2] = 0;
		cnt2--;
	}

	lengte = strlen((LPSTR) & Line[cnt + 1]);

	if (lengte >= MAX_LENGTH_STRING)
		return 0;

	GetSpecialString((LPSTR) & Line[cnt + 1], StrValue, 0);
	return 1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 DecodeQuotedString(LPSTR Source, LPSTR Dest)
{
	int32 cnt, Length, cnt2;
	char ByteString[] = { 0, 0, 0 };
	char *endp;

	cnt2 = 0;
	Length = strlen(Source);
	Source[Length + 1] = 0;
	cnt = 1;

	while (cnt < Length - 1)
	{
		if (Source[cnt] == '\\')
		{
			Dest[cnt2++] = Source[cnt];

			switch (Source[cnt + 1])
			{
			case '0':
				Dest[cnt2 - 1] = 0;
				cnt++;
				break;

			case '\\':
				Dest[cnt2 - 1] = '\\';
				cnt++;
				break;

			case 't':
				Dest[cnt2 - 1] = '\t';
				cnt++;
				break;

			case 'f':
				Dest[cnt2 - 1] = '\f';
				cnt++;
				break;

			case '\"':
				Dest[cnt2 - 1] = '\"';
				cnt++;
				break;

			case 'r':
				Dest[cnt2 - 1] = '\r';
				cnt++;
				break;

			case 'n':
				Dest[cnt2 - 1] = '\n';
				cnt++;
				break;

			case 'x':
				ByteString[0] = Source[cnt + 2];
				ByteString[1] = Source[cnt + 3];
				Dest[cnt2 - 1] = (unsigned char) strtoul(ByteString, &endp, 16);
				cnt += 3;
				break;
			}
		}
		else
			Dest[cnt2++] = Source[cnt];

		cnt++;
	}

	Dest[cnt2] = 0;
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ExpandWithEnvironmentStrings(LPSTR str)
{
	char str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], str4[MAX_LENGTH_STRING];
	int32 cnt, lengte, Stop;
	char *env;

	Stop = 0;

	if (str[0] == 0)
		return 0;

	str3[0] = 0;

	while (!Stop)
	{
		lengte = strlen(str);
		cnt = 0;

		while ((cnt < lengte) && (str[cnt] != '%'))
			cnt++;

		if (cnt < lengte)
		{
			strcpy(str4, str);
			str4[cnt] = 0;
			strcat(str3, str4);
			GetPercentageString(str, str2);
			env = getenv(str2);

			if (env != NULL)
				strcat(str3, env);

			if (str[0] == 0)
				Stop = 1;
		}
		else
		{
			strcat(str3, str);
			Stop = 1;
		}
	}

	strcpy(str, str3);

	return 0;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 CharInString(char ch, LPSTR TextString)
{
	int32 cnt, Length;

	Length = strlen(TextString);

	for (cnt = 0; cnt < Length; cnt++)
	{
		if (TextString[cnt] == ch)
			return 1;
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void StripAppendingZeros(LPSTR str, int32 mode)
{
	int32 cnt, Length, FoundDot;

	Length = strlen(str);

	if (mode == 0)
	{
		// Strip zeros until xx.0
		cnt = Length - 1;

		while ((cnt > 1) && (str[cnt - 1] != '.') && (str[cnt] == '0'))
			cnt--;

		str[cnt + 1] = 0;
	}
	else
	{
		// Strip zeros and dot if necessary
		FoundDot = 0;

		for (cnt = 0; cnt < Length; cnt++)
		{
			if (str[cnt] == '.')
				FoundDot = 1;
		}

		if (FoundDot)
		{
			cnt = Length - 1;

			while ((cnt > 1) && (str[cnt] == '0'))
				cnt--;

			if ((cnt > 0) && (str[cnt] == '.'))
				cnt--;

			str[cnt + 1] = 0;
		}
	}
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 AppendStringToTextFile(LPSTR Filename, LPSTR TextToAppend)
{
	int32 fp;

	if ((fp = FileOpenWriteAppend(Filename)) <= 0)
		return -1;

	WriteLn(fp, TextToAppend);
	FileClose(fp);

	return 0;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 AppendStringToTextFileUTF8(LPSTR Filename, LPSTR TextToAppend)
{
	int32 fp;

#ifdef UC

	if ((fp = FileOpenWriteAppendUTF8(Filename)) <= 0)
		return -1;

#else

	if ((fp = FileOpenWriteAppend(Filename)) <= 0)
		return -1;

#endif
	WriteLn(fp, TextToAppend);
	FileClose(fp);

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void strcatzero(LPSTR str, LPSTR addstr)
{
	int32 cnt, lengte;

	lengte = strlen(addstr);

	for (cnt = 0; cnt < MAX_LENGTH_STRING - 50; cnt++)
	{
		if ((str[cnt] == 0) && (str[cnt + 1] == 0))
		{
			if (cnt == 0)
				memmove(&str[cnt], addstr, lengte);
			else
				memmove(&str[cnt + 1], addstr, lengte);

			return;
		}
	}
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void strcatzeroW(LPWSTR str, LPWSTR addstr)
{
	int32 cnt, lengte;

	lengte = wcslen(addstr);

	for (cnt = 0; cnt < MAX_LENGTH_STRING - 50; cnt++)
	{
		if ((str[cnt] == 0) && (str[cnt + 1] == 0))
		{
			if (cnt == 0)
				memmove(&str[cnt], addstr, lengte * 2);
			else
				memmove(&str[cnt + 1], addstr, lengte * 2);

			return;
		}
	}
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetNewFile(HWND Window, HINSTANCE hInstance, LPSTR OwnFile, LPSTR DirPath, LPSTR FileInfoStr, LPSTR FileInfoStr2,
                 LPSTR DialogInfo, LPSTR Extension, int32 mode)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], SearchStr[MAX_LENGTH_STRING],
	     DirPathTemp[MAX_LENGTH_STRING];
	int32 result, CommError;
	LPSTR Niks;
	OPENFILENAME FileInputInfo;

	memset(&FileInputInfo, 0, sizeof(OPENFILENAME));
	FileInputInfo.lStructSize = sizeof(OPENFILENAME);

	FileInputInfo.hwndOwner = Window;
	FileInputInfo.hInstance = hInstance;
	FileInputInfo.nMaxFile = MAX_LENGTH_STRING - 50;
	FileInputInfo.nMaxFileTitle = 32;

	memset(&str, 0, sizeof(str));
	memset(&str3, 0, sizeof(str3));
	sprintf(str3, "*.%s", Extension);

	if (strcmp(Extension, "*"))
		FileInputInfo.lpstrDefExt = Extension;

	DirPathTemp[0] = 0;

	if (DirPath)
		strcpy(DirPathTemp, DirPath);

	if ((mode & 1) == 1)
	{
		if (strlen(OwnFile) > 0)
			GetFilePartFromFileName(str, OwnFile);
	}

	memset(&str2, 0, sizeof(str2));

	if (FileInfoStr)
	{
		sprintf(SearchStr, "%s (%s)", FileInfoStr, str3);
		strcatzero(str2, SearchStr);
		strcatzero(str2, str3);

		if (FileInfoStr2)
		{
			sprintf(SearchStr, "%s (*.*)", FileInfoStr2);
			strcatzero(str2, SearchStr);
			strcatzero(str2, "*.*");
		}
	}

	FileInputInfo.lpstrTitle = DialogInfo;
	FileInputInfo.lpstrFile = str;
	FileInputInfo.lpstrFilter = str2;
	FileInputInfo.Flags = OFN_LONGNAMES | OFN_EXPLORER;

	if ((mode & 1) == 1)
		FileInputInfo.Flags |= OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST;
	else
		FileInputInfo.Flags |= OFN_FILEMUSTEXIST;

	if (DirPathTemp[0] == 0)
		strcpy(DirPathTemp, "c:\\");

	FileInputInfo.lpstrInitialDir = DirPathTemp;

	if ((mode & 1) == 0)
		result = GetOpenFileName(&FileInputInfo);
	else
		result = GetSaveFileName(&FileInputInfo);

	if (result)
	{
		if ((mode & 1) == 0)
		{
			strcpy(OwnFile, FileInputInfo.lpstrFile);
			GetDirFromFileName(DirPath, OwnFile);
		}
		else
		{
			if (GetFullPathName(FileInputInfo.lpstrFile, MAX_LENGTH_STRING - 50, str2, &Niks) > 0)
				strcpy(OwnFile, str2);
		}

		return 0;
	}
	else
	{
		CommError = CommDlgExtendedError();
		return -2;
	}
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 GetNewFileUTF8(HWND Window, HINSTANCE hInstance, LPSTR OwnFile, LPSTR DirPath, LPSTR FileInfoStr,
                     LPSTR FileInfoStr2, LPSTR DialogInfo, LPSTR Extension, int32 mode)
{
	WCHAR str[8192], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], *StrP, *TempFileName,
	      FileInfoStrUC[MAX_LENGTH_STRING], FileInfoStrUC2[MAX_LENGTH_STRING], *NiksW, ExtStr[40],
	      DialogInfoStrUC[MAX_LENGTH_STRING], SearchStr[40], DirPathTempW[MAX_LENGTH_STRING];
	char DirPathTemp[MAX_LENGTH_STRING], *TempFileName2;
	int32 CommError, result, cnt, lengte, cnt2, MaxSize;
	OPENFILENAMEW FileInputInfo;

	memset(&FileInputInfo, 0, sizeof(OPENFILENAMEW));
	FileInputInfo.lStructSize = sizeof(OPENFILENAMEW);

	MaxSize = 8192 - 50;

	if (mode & 4)
		MaxSize = 32768 - 50;

	if (mode & 8)
		MaxSize = 128 * 1024 - 50;

	FileInputInfo.hwndOwner = Window;
	FileInputInfo.hInstance = hInstance;

	if ((mode & 2) == 0)
		FileInputInfo.nMaxFile = MAX_LENGTH_STRING - 50;
	else
		FileInputInfo.nMaxFile = MaxSize;

	FileInputInfo.nMaxFileTitle = 32;

	memset(&str, 0, sizeof(str));
	memset(&str2, 0, sizeof(str2));

	if (Utf8ToUnicode(Extension, ExtStr, sizeof(ExtStr) / 2 - 1) == 0)
		return -1;

#ifdef GCC_COMP
	swprintf(str, L"*.%s", ExtStr);
#else
	swprintf(str, MAX_LENGTH_STRING, L"*.%s", ExtStr);
#endif

	if (strcmp(Extension, "*"))
		FileInputInfo.lpstrDefExt = ExtStr;

	if (strlen(OwnFile) > 0)
	{
		if (OwnFile[strlen(OwnFile) - 1] != '\\')
		{
			GetDirFromFileName(DirPathTemp, OwnFile);

			if (Utf8ToUnicode(DirPathTemp, DirPathTempW, MAX_LENGTH_STRING - 1) == 0)
				return -1;
		}
		else
		{
			if (Utf8ToUnicode(OwnFile, str3, MAX_LENGTH_STRING - 1) == 0)
				return -1;

			wcscpy(DirPathTempW, str3);
		}
	}
	else
	{
		if (Utf8ToUnicode(DirPath, DirPathTempW, MAX_LENGTH_STRING - 1) == 0)
			return -1;
	}

	if (FileInfoStr)
	{
		if (Utf8ToUnicode(FileInfoStr, FileInfoStrUC, MAX_LENGTH_STRING - 1) == 0)
			return -1;

#ifdef GCC_COMP
		swprintf(SearchStr, L" (%s)", str);
#else
		swprintf(SearchStr, MAX_LENGTH_STRING, L" (%s)", str);
#endif

		if ((mode & 1) == 0)
		{
			wcscat(FileInfoStrUC, SearchStr);
			strcatzeroW(str2, FileInfoStrUC);
			strcatzeroW(str2, str);

			if (FileInfoStr2)
			{
				if (Utf8ToUnicode(FileInfoStr2, FileInfoStrUC2, MAX_LENGTH_STRING - 1) == 0)
					return -1;

				wcscat(FileInfoStrUC2, L" (*.*)");
				strcatzeroW(str2, FileInfoStrUC2);
				strcatzeroW(str2, L"*.*");
			}
		}
		else
		{
			if (!wcschr(FileInfoStrUC, L'\t'))
			{
				wcscat(FileInfoStrUC, SearchStr);
				strcatzeroW(str2, FileInfoStrUC);
				strcatzeroW(str2, str);
			}
			else
			{
				cnt2 = 0;
				StrP = FileInfoStrUC;
				lengte = wcslen(FileInfoStrUC);

				for (cnt = 0; cnt < lengte + 1; cnt++)
				{
					if ((FileInfoStrUC[cnt] == L'\t') || (FileInfoStrUC[cnt] == 0))
					{
						FileInfoStrUC[cnt] = 0;
						strcatzeroW(str2, StrP);
						StrP = (WCHAR *) & FileInfoStrUC[cnt + 1];

						if (cnt2 == 0)
							strcatzeroW(str2, str);
						else
							strcatzeroW(str2, L"*.*");

						cnt2++;
					}
				}
			}
		}
	}

	if (Utf8ToUnicode(DialogInfo, DialogInfoStrUC, MAX_LENGTH_STRING - 1) == 0)
		return -1;

	FileInputInfo.lpstrTitle = DialogInfoStrUC;
	str[0] = 0;
	FileInputInfo.lpstrFile = str;
	FileInputInfo.lpstrFilter = str2;
	FileInputInfo.Flags = OFN_LONGNAMES | OFN_EXPLORER;

	if ((mode & 1) == 1)
		FileInputInfo.Flags |= OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST;
	else
		FileInputInfo.Flags |= OFN_FILEMUSTEXIST;

	if ((mode & 2) == 2)
		FileInputInfo.Flags |= OFN_ALLOWMULTISELECT;

	if (DirPathTempW[0] == 0)
		wcscpy(DirPathTempW, L"c:\\");

	FileInputInfo.lpstrInitialDir = DirPathTempW;

	if ((mode & 1) == 0)
		result = GetOpenFileNameW(&FileInputInfo);
	else
	{
		FileInputInfo.nFilterIndex = 0;
		result = GetSaveFileNameW(&FileInputInfo);
	}

	if (result)
	{
		if ((mode & 1) == 0)
		{
			if ((mode & 2) == 0)
			{
				if (UnicodeToUtf8(FileInputInfo.lpstrFile, OwnFile, MAX_LENGTH_STRING - 50) == 0)
					return -1;

				GetDirFromFileName(DirPath, OwnFile);
			}
			else
			{
				TempFileName = FileInputInfo.lpstrFile;
				lengte = wcslen(TempFileName);
				TempFileName += lengte + 1;

				if (TempFileName[0] == 0)
				{
					if (UnicodeToUtf8(FileInputInfo.lpstrFile, OwnFile, MaxSize) == 0)
						return -1;

					GetDirFromFileName(DirPath, OwnFile);
				}
				else
				{
					while (TempFileName[0])
					{
						lengte = wcslen(TempFileName);
						TempFileName[-1] = L'\t';
						TempFileName += lengte + 1;
					}

					if (UnicodeToUtf8(FileInputInfo.lpstrFile, OwnFile, MaxSize) == 0)
						return -1;

					TempFileName2 = OwnFile;
					lengte = strlen(TempFileName2);

					for (cnt = 0; cnt < lengte; cnt++)
						if (TempFileName2[cnt] == '\t')
							TempFileName2[cnt] = 0;

					strcpy(DirPath, OwnFile);
				}
			}
		}
		else
		{
			result = FileInputInfo.nFilterIndex - 1;

			if (GetFullPathNameW(FileInputInfo.lpstrFile, MAX_LENGTH_STRING - 50, str3, &NiksW) > 0)
			{
				if (UnicodeToUtf8(str3, OwnFile, MAX_LENGTH_STRING - 1) == 0)
					return -1;

				return result;
			}
		}

		return 0;
	}
	else
	{
		CommError = CommDlgExtendedError();
		return -2;
	}
}


// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 DeleteFileUTF8(LPSTR Src)
{
#ifdef UC
	WCHAR SrcW[MAX_LENGTH_STRING];

	if (Utf8ToUnicode(Src, SrcW, MAX_LENGTH_STRING - 1) == 0)
		return 0;

	return DeleteFileW(SrcW);
}

#else

	return DeleteFile(Src);
}
#endif

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 CopyFileUTF8(LPSTR Src, LPSTR Dest, int32 bFailIfExists)
{
#ifdef UC
	WCHAR SrcW[MAX_LENGTH_STRING], DestW[MAX_LENGTH_STRING];

	if (Utf8ToUnicode(Src, SrcW, MAX_LENGTH_STRING - 1) == 0)
		return 0;

	if (Utf8ToUnicode(Dest, DestW, MAX_LENGTH_STRING - 1) == 0)
		return 0;

	return CopyFileW(SrcW, DestW, bFailIfExists);
}

#else

	return CopyFile(Src, Dest, bFailIfExists);
}
#endif

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 MoveFileUTF8(LPSTR Src, LPSTR Dest)
{
#ifdef UC
	WCHAR SrcW[MAX_LENGTH_STRING], DestW[MAX_LENGTH_STRING];

	if (Utf8ToUnicode(Src, SrcW, MAX_LENGTH_STRING - 1) == 0)
		return 0;

	if (Utf8ToUnicode(Dest, DestW, MAX_LENGTH_STRING - 1) == 0)
		return 0;

	return MoveFileW(SrcW, DestW);
}

#else

	return MoveFile(Src, Dest);
}
#endif

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 SetCurrentDirectoryUTF8(LPSTR Dir)
{
#ifdef UC
	WCHAR DirW[MAX_LENGTH_STRING];

	if (Utf8ToUnicode(Dir, DirW, MAX_LENGTH_STRING - 1) == 0)
		return 0;

	return SetCurrentDirectoryW(DirW);
}

#else

	return SetCurrentDirectory(Dir);
}
#endif

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 CreateDirectoryUTF8(LPSTR Dir)
{
#ifdef UC
	WCHAR DirW[MAX_LENGTH_STRING];

	if (Utf8ToUnicode(Dir, DirW, MAX_LENGTH_STRING - 1) == 0)
		return 0;

	return CreateDirectoryW(DirW, NULL);
}

#else

	return CreateDirectory(Dir, NULL);
}
#endif

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 GetTempDir(LPSTR Dir)
{
	if (!GetTempPath(MAX_LENGTH_STRING - 50, Dir))
	{
		if (!SHGetSpecialFolderPath(NULL, Dir, CSIDL_PERSONAL, 0))
			return 0;
	}

	return 1;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 GetTempDirUTF8(LPSTR Dir)
{
#ifdef UC
	WCHAR DirW[MAX_LENGTH_STRING];

	if (!GetTempPathW(MAX_LENGTH_STRING - 50, DirW))
	{
		if (!SHGetSpecialFolderPathW(NULL, DirW, CSIDL_PERSONAL, 0))
			return 0;
	}

	return UnicodeToUtf8(DirW, Dir, MAX_LENGTH_STRING);
}


#else

	return GetTempDir(Dir);
}
#endif


// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 DeleteDirectory(LPSTR Dir)
{
	WIN32_FIND_DATA FileData;
	int32 res, MaxNrDirs, NrDirs, NrDirsFound, cnt, lengte;
	HANDLE FileSearchHandle;
	char Search[300], FileName[300], Dirs[128][300];

	MaxNrDirs = 128;

	if (!Dir)
		return 0;

	lengte = strlen(Dir);

	if ((lengte == 0) || (Dir[1] != ':') || (!isalpha(Dir[0])) || ((lengte > 2) && (Dir[2] != '\\')))
		return 0;

	NrDirs = 0;
	NrDirsFound = 0;

	if ((strlen(Dir) > 3) || (strlen(Dir) == 2))
		sprintf(Search, "%s\\*.*", Dir);
	else
		sprintf(Search, "%s*.*", Dir);

	FileSearchHandle = FindFirstFile(Search, &FileData);

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		return 0;

	res = 1;

	if (stricmp(FileData.cFileName, ".") == 0)
	{
		res = FindNextFile(FileSearchHandle, &FileData);
		res = FindNextFile(FileSearchHandle, &FileData);
	}

	while (res)
	{
		if (FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		{
			if (NrDirs < MaxNrDirs)
			{
				strcpy(Dirs[NrDirs], FileData.cFileName);
				NrDirs++;
			}

			NrDirsFound++;
		}

		res = FindNextFile(FileSearchHandle, &FileData);
	}

	FindClose(FileSearchHandle);

	if (NrDirs > 0)
	{
		for (cnt = 0; cnt < NrDirs; cnt++)
		{
			sprintf(Search, "%s\\%s", Dir, Dirs[cnt]);
			DeleteDirectory(Search);
		}
	}

	if (NrDirsFound > NrDirs)
		DeleteDirectory(Dir);

	if ((strlen(Dir) > 3) || (strlen(Dir) == 2))
		sprintf(Search, "%s\\*.*", Dir);
	else
		sprintf(Search, "%s*.*", Dir);

	FileSearchHandle = FindFirstFile(Search, &FileData);

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
	{
		res = 1;

		while (res)
		{
			if ((FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
			{
				sprintf(FileName, "%s\\%s", Dir, FileData.cFileName);
				DeleteFile(FileName);
			}

			res = FindNextFile(FileSearchHandle, &FileData);
		}

		FindClose(FileSearchHandle);
	}

	if (strlen(Dir) > 3)
	{
		res = RemoveDirectory(Dir);

		if (!res)
			res = GetLastError();
	}

	return 0;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 DeleteDirectoryUnicode(WCHAR * Dir)
{
	WIN32_FIND_DATAW FileData;
	int32 res, MaxNrDirs, NrDirs, NrDirsFound, cnt, lengte;
	HANDLE FileSearchHandle;
	WCHAR Search[300], FileName[300], Dirs[128][300];

	MaxNrDirs = 128;

	if (!Dir)
		return 0;

	lengte = wcslen(Dir);

	if ((lengte == 0) || (Dir[1] != ':') || (!isalpha(Dir[0])) || ((lengte > 2) && (Dir[2] != '\\')))
		return 0;

	NrDirs = 0;
	NrDirsFound = 0;

	if ((wcslen(Dir) > 3) || (wcslen(Dir) == 2))
	{
#ifdef GCC_COMP
		swprintf(Search, L"%s\\*.*", Dir);
#else
		swprintf(Search, 200, L"%s\\*.*", Dir);
#endif
	}
	else
	{
#ifdef GCC_COMP
		swprintf(Search, L"%s*.*", Dir);
#else
		swprintf(Search, 200, L"%s*.*", Dir);
#endif
	}

	FileSearchHandle = FindFirstFileW(Search, &FileData);

	if (FileSearchHandle == INVALID_HANDLE_VALUE)
		return 0;

	res = 1;

	if (wcscmp(FileData.cFileName, L".") == 0)
	{
		res = FindNextFileW(FileSearchHandle, &FileData);
		res = FindNextFileW(FileSearchHandle, &FileData);
	}

	while (res)
	{
		if (FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		{
			if (NrDirs < MaxNrDirs)
			{
				wcscpy(Dirs[NrDirs], FileData.cFileName);
				NrDirs++;
			}

			NrDirsFound++;
		}

		res = FindNextFileW(FileSearchHandle, &FileData);
	}

	FindClose(FileSearchHandle);

	if (NrDirs > 0)
	{
		for (cnt = 0; cnt < NrDirs; cnt++)
		{
#ifdef GCC_COMP
			swprintf(Search, L"%s\\%s", Dir, Dirs[cnt]);
#else
			swprintf(Search, 200, L"%s\\%s", Dir, Dirs[cnt]);
#endif
			DeleteDirectoryUnicode(Search);
		}
	}

	if (NrDirsFound > NrDirs)
		DeleteDirectoryUnicode(Dir);

	if ((wcslen(Dir) > 3) || (wcslen(Dir) == 2))
	{
#ifdef GCC_COMP
		swprintf(Search, L"%s\\*.*", Dir);
#else
		swprintf(Search, 200, L"%s\\*.*", Dir);
#endif
	}
	else
	{
#ifdef GCC_COMP
		swprintf(Search, L"%s*.*", Dir);
#else
		swprintf(Search, 200, L"%s*.*", Dir);
#endif
	}

	FileSearchHandle = FindFirstFileW(Search, &FileData);

	if (FileSearchHandle != INVALID_HANDLE_VALUE)
	{
		res = 1;

		while (res)
		{
			if ((FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
			{
#ifdef GCC_COMP
				swprintf(FileName, L"%s\\%s", Dir, FileData.cFileName);
#else
				swprintf(FileName, 200, L"%s\\%s", Dir, FileData.cFileName);
#endif
				DeleteFileW(FileName);
			}

			res = FindNextFileW(FileSearchHandle, &FileData);
		}

		FindClose(FileSearchHandle);
	}

	if (wcslen(Dir) > 3)
	{
		res = RemoveDirectoryW(Dir);

		if (!res)
			res = GetLastError();
	}

	return 0;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

#ifdef GCC_COMP

#define ALIGN_DIFF(x, n) (((intptr_t)((x)+(n) - 1L) & ~((intptr_t)(n) - 1L)) - (intptr_t)(x))
#define ALIGN_DOWN(x, n) (((intptr_t)(x)) & ~((intptr_t)(n) - 1L))
#define ALIGN_DOWN_DIFF(x, n) (((intptr_t)(x)) & ((intptr_t)(n) - 1L))

#define  SOV16                16
#define  SOV8                 8

int32 strlen_SSE42(const char *s)
{
	size_t len, t;
	const char *p;
	asm volatile ("prefetcht0 (%0)"::"r" (s));
	/*
	 * even if nehalem can handle unaligned load much better
	 * (so they promised), we still align hard to get in
	 * swing with the page boundery.
	 */
	asm("pxor %%xmm0, %%xmm0\n\t" "movdqa (%1), %%xmm1\n\t" "pcmpeqb  %%xmm0, %%xmm1\n\t" "pmovmskb %%xmm1, %0\n\t"
	    "shr  %b2, %0\n\t" "bsf  %0, %0\n\t" "jnz  2f\n\t" "mov  $0xFF01, %k0\n\t" "movd %k0, %%xmm0\n\t" ".p2align 1\n"
	    "1:\n\t" "add  $16, %1\n\t" "prefetcht0 64(%1)\n\t"
	    /* LSB,Invert,Range,Bytes */
	    /*             6543210 */
	    "pcmpistri  $0b0010100, (%1), %%xmm0\n\t" "jnz  1b\n\t" "lea  (%1,%2),%0\n\t" "sub  %3, %0\n" "2:":	/* %0 */
	    "=&r"(len),
	    /* %1 */ "=&r"(p),
	    /* %2 */ "=&c"(t)
	    :							/* %3 */ "m"(s),
	    /* %4 */ "2"(ALIGN_DOWN_DIFF(s, SOV16)),
	    /* %5 */ "1"(ALIGN_DOWN(s, SOV16))
	    :	"xmm0", "xmm1");
	return len;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 strlen_SSE2(const char *s)
{
	size_t len;
	const char *p;
	asm volatile ("prefetcht0 (%0)"::"r" (s));
	asm("pxor %%xmm1, %%xmm1\n\t" "movdqa (%1), %%xmm0\n\t" "pcmpeqb  %%xmm1, %%xmm0\n\t" "pmovmskb %%xmm0, %0\n\t" "shr  %b3, %0\n\t" "bsf  %0, %0\n\t" "jnz  2f\n\t" ".p2align 1\n" "1:\n\t" "movdqa 16(%1), %%xmm0\n\t" "add  $16, %1\n\t" "prefetcht0 64(%1)\n\t" "pcmpeqb  %%xmm1, %%xmm0\n\t" "pmovmskb %%xmm0, %0\n\t" "test %0, %0\n\t" "jz 1b\n\t" "bsf  %0, %0\n\t" "add  %1, %0\n\t" "sub  %2, %0\n\t" "2:":	/* %0 */
	    "=&r"(len),
	    /* %1 */
	    "=&r"
	    (p)
	    :							/* %2 */ "m"(s),
	    /* %3 */
	    "c"
	    (ALIGN_DOWN_DIFF
	     (s,
	      SOV16)),
	    /* %4 */
	    "1"
	    (ALIGN_DOWN
	     (s,
	      SOV16))
	    :																																																																																																						"xmm0", "xmm1");
	return len;
}

#endif

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

typedef int32(WINAPI * LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);

LPFN_ISWOW64PROCESS fnIsWow64Process;

int32 IsWow64()
{
	int32 bIsWow64 = 0;

	//IsWow64Process is not available on all supported versions of Windows.
	//Use GetModuleHandle to get a handle to the DLL that contains the function
	//and GetProcAddress to get a pointer to the function if available.

	fnIsWow64Process = (LPFN_ISWOW64PROCESS) GetProcAddress(GetModuleHandle(TEXT("kernel32")), "IsWow64Process");

	if (NULL != fnIsWow64Process)
	{
		if (!fnIsWow64Process(GetCurrentProcess(), &bIsWow64))
		{
			//handle error
		}
	}

	return bIsWow64;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
