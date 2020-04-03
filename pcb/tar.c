/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: tar.c
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
#include "files2.h"
#include "stdio.h"
#include "stdlib.h"
#include "tar.h"
#include "io.h"
#include "memory.h"
#include "utf8.h"
#include "calc.h"
#include "string.h"
#include "assert.h"
#include "zlib.h"
#include "fcntl.h"
#include "pcb.h"
#include "calcdef.h"

#define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)


#define    BUF_SIZE   32768
typedef struct
{
	union
	{
		/* ustar header, Posix 1003.1 */
		unsigned char raw[512];
		struct
		{
			char name[100];		/*   0- 99 */
			char mode[8];		/* 100-107 */
			char uid[8];		/* 108-115 */
			char gid[8];		/* 116-123 */
			char size[12];		/* 124-135 */
			char mtime[12];		/* 136-147 */
			char chksum[8];		/* 148-155 */
			char typeflag;		/* 156-156 */
			char linkname[100];	/* 157-256 */
			char magic[6];		/* 257-262 */
			char version[2];	/* 263-264 */
			char uname[32];		/* 265-296 */
			char gname[32];		/* 297-328 */
			char devmajor[8];	/* 329-336 */
			char devminor[8];	/* 337-344 */
			char prefix[155];	/* 345-499 */
			char padding[12];	/* 500-512 */
		} formated;
	};
} TarStruct;

union
{
	unsigned char raw[8];
	struct
	{
		unsigned char method;
		unsigned char flags;
		unsigned int mtime;
		unsigned char xtra_flags;
		unsigned char os_flags;
	} formated;
} TargzHeader;

TarFileHeaderStruct TarFileHeader;


TarStruct *OdbTarFiles, tar;
int32 NrOdbTarFiles, Tarfp;
char *ReadBuf2;

uint8 Zeros[512];

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadTarFiles(LPSTR MainDirectory, LPSTR Directory)
{
	int32 i, sum, result, result2, Written, cnt, length, res, DirRead;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];
	HANDLE Handle;
	HANDLE CurrentFile;
	WIN32_FIND_DATAW FindFileData;
	TarStruct *TarHeader;
	SYSTEMTIME FileDate;

	sprintf(str, "%s\\*.*", Directory);
	res = 1;
	CurrentFile = FindFirstFileUTF8(str, &FindFileData);

	if (CurrentFile == INVALID_HANDLE_VALUE)
		return -1;

	DirRead = 0;

	while (res)
	{
		if (FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		{
			if ((wcscmp(FindFileData.cFileName, L".")) && (wcscmp(FindFileData.cFileName, L"..")))
			{
				UnicodeToUtf8(FindFileData.cFileName, str3, MAX_LENGTH_STRING - 100);
				sprintf(str2, "%s\\%s", Directory, str3);
				LoadTarFiles(MainDirectory, str2);
			}
			else
			{
				if (!wcscmp(FindFileData.cFileName, L"."))
				{
					FileTimeToSystemTime(&FindFileData.ftLastWriteTime, &FileDate);
					res = strlen(MainDirectory) + 1;
					length = strlen(Directory) - res;

					if (length > 98)
						return -1;

					TarHeader = &OdbTarFiles[NrOdbTarFiles++];
					memset(TarHeader, 0, sizeof(TarStruct));
					strcpy(TarHeader->formated.name, &Directory[res]);
					strcat(TarHeader->formated.name, "\\");
					length = strlen(TarHeader->formated.name);

					for (cnt = 0; cnt < length; cnt++)
					{
						if (TarHeader->formated.name[cnt] == '\\')
							TarHeader->formated.name[cnt] = '/';
					}

#if 0
					char name[100];	/*   0- 99 */
					char mode[8];	/* 100-107 */
					char uid[8];	/* 108-115 */
					char gid[8];	/* 116-123 */
					char size[12];	/* 124-135 */
					char mtime[12];	/* 136-147 */
					char chksum[8];	/* 148-155 */
					char typeflag;	/* 156-156 */
					char linkname[100];	/* 157-256 */
					char magic[6];	/* 257-262 */
					char version[2];	/* 263-264 */
					char uname[32];	/* 265-296 */
					char gname[32];	/* 297-328 */
					char devmajor[8];	/* 329-336 */
					char devminor[8];	/* 337-344 */
					char prefix[155];	/* 345-499 */
					char padding[12];	/* 500-512 */
#endif
					strcpy(TarHeader->formated.mode, "  40755");
					strcpy(TarHeader->formated.uid, "      0");
					strcpy(TarHeader->formated.gid, "      0");
					strcpy(TarHeader->formated.magic, "ustar");
					strcpy(TarHeader->formated.uname, "user");
					strcpy(TarHeader->formated.gname, "group");

					sprintf(TarHeader->formated.size, "%12o%12o        5", 0,
					        mktime2(FileDate.wYear, FileDate.wMonth, FileDate.wDay, FileDate.wHour, FileDate.wMinute,
					                FileDate.wSecond) - 2);
					/* Do checksum on headers */
					sum = 0;

					for (i = 0; i < 512; i++)
						sum += TarHeader->raw[i];

					sprintf(TarHeader->formated.chksum, "%6o", sum);

					FileWrite(Tarfp, TarHeader, sizeof(TarStruct), &result2);
				}
			}
		}
		else
		{
			UnicodeToUtf8(FindFileData.cFileName, str3, MAX_LENGTH_STRING - 100);
			sprintf(str2, "%s\\%s", Directory, str3);
//      printf("Reading %s\\%s\n",Directory,FindFileData.cFileName);
			res = strlen(MainDirectory) + 1;
			length = strlen(str2) - res;

			if (length < 99)
			{
				TarHeader = &OdbTarFiles[NrOdbTarFiles++];
				memset(TarHeader, 0, sizeof(TarStruct));
				strcpy(TarHeader->formated.name, &str2[res]);
				length = strlen(TarHeader->formated.name);

				for (cnt = 0; cnt < length; cnt++)
				{
					if (TarHeader->formated.name[cnt] == '\\')
						TarHeader->formated.name[cnt] = '/';
				}

				strcpy(TarHeader->formated.mode, "  10644");
				strcpy(TarHeader->formated.uid, "      0");
				strcpy(TarHeader->formated.gid, "      0");
				FileTimeToSystemTime(&FindFileData.ftLastWriteTime, &FileDate);
				sprintf(TarHeader->formated.size, "%12o%12o        0", FindFileData.nFileSizeLow,
				        mktime2(FileDate.wYear, FileDate.wMonth, FileDate.wDay, FileDate.wHour, FileDate.wMinute,
				                FileDate.wSecond));
				strcpy(TarHeader->formated.magic, "ustar");
				strcpy(TarHeader->formated.uname, "user");
				strcpy(TarHeader->formated.gname, "group");
				/* Do checksum on headers */
				sum = 0;

				for (i = 0; i < 512; i++)
					sum += TarHeader->raw[i];

				sprintf(TarHeader->formated.chksum, "%6o", sum);
				FileWrite(Tarfp, TarHeader, sizeof(TarStruct), &result2);
				Handle = CreateFile(str2, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
				result = 1;
				Written = 0;

				while (result > 0)
				{
					ReadFile(Handle, ReadBuf2, BUF_SIZE, (PDWORD) & result, NULL);
					Written += result;
					FileWrite(Tarfp, ReadBuf2, result, &result2);
				}

				if (Written & 0x1ff)
					FileWrite(Tarfp, Zeros, 512 - (Written & 0x1ff), &result2);

				CloseHandle(Handle);
			}
		}

		res = FindNextFileW(CurrentFile, &FindFileData);
	}

	FindClose(CurrentFile);
	return 0;
}

//*******************************************************************************************************
//****************************** generování ODB++ *******************************************************
//*******************************************************************************************************

int32 TarOdbDirectory(LPSTR OdbFile, LPSTR TarDirectory)
{
	int32 length;
	char str[300], str2[300], TarFile[300];

	length = strlen(TarDirectory);

	if ((length > 3) && (TarDirectory[length - 1] == '\\'))
	{
		TarDirectory[length - 1] = 0;	// Strip appending back slash
	}

	SetWaitCursor();
	AllocateSpecialMem(MEM_POINTS, BUF_SIZE + sizeof(TarStruct) * 1024, (void **) &ReadBuf2);
	OdbTarFiles = (TarStruct *) & ReadBuf2[BUF_SIZE];
	NrOdbTarFiles = 0;
	sprintf(TarFile, "%s.tar", OdbFile);

	if (FileExistsUTF8(TarFile) == 0)
		DeleteFileUTF8(TarFile);

	Tarfp = FileOpenWriteUTF8(TarFile);
	GetDirFromFileName(str, TarDirectory);
	LoadTarFiles(str, TarDirectory);
	FileClose(Tarfp);
	
//	sprintf(str2, "%s.tgz", OdbFile); //pùvodní název souboru
	sprintf(str2, "%s\\pcb\\ODB++.tgz", DesignPath); //nový název souboru

	//****************************************** soubor již existuje *******************************************************
	if (FileExistsUTF8(str2) == 0)
	{
		sprintf(str, SC(788, "File already exists.\n\n%s\n\nDo you want to overwrite it ?"), str2);
		SetNormalCursor();

		if (MessageBoxOwn(PCBWindow, str, SC(1, "Message"), MB_APPLMODAL | MB_YESNO) == IDNO)
			return -1;

		SetWaitCursor();
		DeleteFileUTF8(str2);
	}
	//**********************************************************************************************************************

	if (CompressFile(TarFile, str2, 0) == 0)
		DeleteFileUTF8(TarFile);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

#define CHUNK   16384

int32 CompressFile(LPSTR SrcFile, LPSTR DestFile, int32 mode)
{
	FILE *source, *dest;
	uint8 buf[CHUNK];
	int len;

	source = fopen(SrcFile, "rb");

	if (!source)
		return -1;

	dest = gzopen(DestFile, "wb");

	if (!dest)
		return -1;

	for (;;)
	{
		len = fread(buf, 1, sizeof(buf), source);

		if (ferror(source))
			return -1;

		if (len == 0)
			break;

		if (gzwrite(dest, buf, (unsigned) len) != len)
			return -1;
	}

	fclose(source);

	if (gzclose(dest) != Z_OK)
		return -1;

	return Z_OK;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 DecompressFile(LPSTR SrcFile, LPSTR DestFile, int32 mode)
{
	FILE *source, *dest;
	int32 len;
	uint8 buf[CHUNK];

	source = gzopen(SrcFile, "rb");

	if (!source)
		return -1;

	dest = fopen(DestFile, "wb");

	if (!dest)
		return -1;

	for (;;)
	{
		len = gzread(source, buf, sizeof(buf));

		if (len < 0)
			return -1;

		if (len == 0)
			break;

		if ((int) fwrite(buf, 1, (unsigned) len, dest) != len)
			return -1;
	}

	if (fclose(dest))
	{
		// error("failed fclose");
	}

	if (gzclose(source) != Z_OK)
		return -1;

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
