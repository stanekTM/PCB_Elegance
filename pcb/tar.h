/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: tar.h
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


#ifndef _TARGZ

#define _TARGZ

#include "owntypes.h"
#include "files2.h"

typedef struct
{
	int32 Uid;
	int32 Gid;
	int32 FileType;				// 0 = File, 1 = Directory
	int32 FileSize;
	char FileName[200];
	SYSTEMTIME FileDate;
} TarFileHeaderStruct;


int32 GetHeaderTar(int32 fp, TarFileHeaderStruct * TarFileHeader);

int32 UntarOdbFile(LPSTR OdbTarFile, LPSTR TarDirectory);

int32 TarOdbDirectory(LPSTR OdbFile, LPSTR TarDirectory);

int32 DecompressFile(LPSTR SrcFile, LPSTR DestFile, int32 mode);

int32 CompressFile(LPSTR SrcFile, LPSTR DestFile, int32 mode);

#endif // _TARGZ
