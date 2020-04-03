/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: own_process.h
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



#ifndef _PROCESS_H

#define _PROCESS_H

#include "owntypes.h"


typedef struct
{
	char Name[96];
	char ClassName[64];
	HWND WindowHandle;
	HWND Parent;
	uint32 Style;
	uint32 ExStyle;
	ATOM atomWindowType;
	WNDCLASS Class;
	uint32 TopWindow;
	int32 WindowCount;
	int32 Level;
	int32 SizeX;
	int32 SizeY;
	uint32 Status;
	DWORD ProcessID;
	int32 PosX;
	int32 PosY;
} WindowInfoRecord;


typedef struct
{
	char ExeName[96];
	int32 ProcessID;
	int32 ParentProcessID;
	int32 UsedTime;
	int32 NrThreads;
	int32 MinMemUsedInKb;
	int32 MaxMemUsedInKb;
} ProcessInfoRecord;


typedef struct
{
	int32 ProcessID;
	int32 StartAddress;
	int32 Length;
	int32 LockCount;
} ProcessHeapInfoRecord;


int32 GetWindowProperties(WindowInfoRecord ** OwnWindowInfo, int32 mode);

void GetWindowPropertiesToFile(LPSTR Filename, int32 mode);

int32 GetProcessListToFile(LPSTR Filename);

void GetProcessPropertiesToFile(LPSTR Filename, int32 mode);

int32 GetProcessHeapProperties(int32 ProcessID, ProcessHeapInfoRecord ** OwnProcessHeapInfo);

int32 GetProcessPidFromExefile(LPSTR Filename);

int32 GetProcessProperties(ProcessInfoRecord ** OwnProcessInfo);

int32 DummyReadProcessMemory(int32 pid, int32 NrMegaBytes, int32 mode);

void GetCPUID(uint32 op, uint32 * EAX, uint32 * EBX, uint32 * ECX, uint32 * EDX);

#endif // _PROCESS_H
