/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: own_process.c
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


#define WINVER     0x0500

#include "owntypes.h"
#include "windows.h"
#include "files2.h"
#include "stdio.h"
//#include "psapi.h"
#include "own_process.h"
#include <tlhelp32.h>

WindowInfoRecord *WindowProperties;
ProcessInfoRecord *ProcessProperties;
ProcessHeapInfoRecord *ProcessHeapProperties;
HWND TopActiveWindows[1024];

int32 WindowCount, MaxWindowCount, TopWindowCount, NestingLevel;
int32 MaxProcessesCount;
int32 MaxProcessHeapsCount;

HGLOBAL WindowPropertiesGlobal;
HGLOBAL ProcessPropertiesGlobal;
HGLOBAL ProcessHeapPropertiesGlobal;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemWindowProperties(int32 NrWindows)
{
	HGLOBAL NewMem;
	int32 MemSize;

	if (NrWindows == 0)
	{
		NrWindows = max(256, NrWindows);
		MemSize = NrWindows * sizeof(WindowInfoRecord);

		if ((WindowPropertiesGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((WindowProperties = (WindowInfoRecord *) GlobalLock(WindowPropertiesGlobal)) == NULL)
			return -1;
	}
	else
	{
		if (NrWindows > MaxWindowCount)
		{
			MemSize = NrWindows * sizeof(WindowInfoRecord);

//      GlobalUnlock(WindowPropertiesGlobal);
			if ((NewMem = GlobalReAlloc(WindowPropertiesGlobal, MemSize, GHND)) == NULL)
			{
				GlobalFree(WindowPropertiesGlobal);

				if ((WindowPropertiesGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
					return -1;

				if ((WindowProperties = (WindowInfoRecord *) GlobalLock(WindowPropertiesGlobal)) == NULL)
					return -1;
			}
			else
			{
				WindowPropertiesGlobal = NewMem;

				if ((WindowProperties = (WindowInfoRecord *) GlobalLock(WindowPropertiesGlobal)) == NULL)
					return -1;
			}
		}
	}

	MaxWindowCount = NrWindows;
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemProcessProperties(int32 NrProcesses)
{
	HGLOBAL NewMem;
	int32 MemSize;

	if (NrProcesses == 0)
	{
		NrProcesses = max(256, NrProcesses);
		MemSize = NrProcesses * sizeof(ProcessInfoRecord);

		if ((ProcessPropertiesGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((ProcessProperties = (ProcessInfoRecord *) GlobalLock(ProcessPropertiesGlobal)) == NULL)
			return -1;
	}
	else
	{
		if (NrProcesses > MaxProcessesCount)
		{
			MemSize = NrProcesses * sizeof(ProcessInfoRecord);

//      GlobalUnlock(WindowPropertiesGlobal);
			if ((NewMem = GlobalReAlloc(ProcessPropertiesGlobal, MemSize, GHND)) == NULL)
			{
				GlobalFree(ProcessPropertiesGlobal);

				if ((ProcessPropertiesGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
					return -1;

				if ((ProcessProperties = (ProcessInfoRecord *) GlobalLock(ProcessPropertiesGlobal)) == NULL)
					return -1;
			}
			else
			{
				ProcessPropertiesGlobal = NewMem;

				if ((ProcessProperties = (ProcessInfoRecord *) GlobalLock(ProcessPropertiesGlobal)) == NULL)
					return -1;
			}
		}
	}

	MaxProcessesCount = NrProcesses;
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemProcessHeapProperties(int32 NrProcessHeaps)
{
	HGLOBAL NewMem;
	int32 MemSize;

	if (NrProcessHeaps == 0)
	{
		NrProcessHeaps = max(256, NrProcessHeaps);
		MemSize = NrProcessHeaps * sizeof(ProcessHeapInfoRecord);

		if ((ProcessHeapPropertiesGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((ProcessHeapProperties = (ProcessHeapInfoRecord *) GlobalLock(ProcessHeapPropertiesGlobal)) == NULL)
			return -1;
	}
	else
	{
		if (NrProcessHeaps > MaxProcessHeapsCount)
		{
			MemSize = NrProcessHeaps * sizeof(ProcessHeapInfoRecord);

//      GlobalUnlock(WindowPropertiesGlobal);
			if ((NewMem = GlobalReAlloc(ProcessHeapPropertiesGlobal, MemSize, GHND)) == NULL)
			{
				GlobalFree(ProcessHeapPropertiesGlobal);

				if ((ProcessHeapPropertiesGlobal = GlobalAlloc(GHND, MemSize)) == NULL)
					return -1;

				if ((ProcessHeapProperties = (ProcessHeapInfoRecord *) GlobalLock(ProcessHeapPropertiesGlobal)) == NULL)
					return -1;
			}
			else
			{
				ProcessHeapPropertiesGlobal = NewMem;

				if ((ProcessHeapProperties = (ProcessHeapInfoRecord *) GlobalLock(ProcessHeapPropertiesGlobal)) == NULL)
					return -1;
			}
		}
	}

	MaxProcessHeapsCount = NrProcessHeaps;
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 CALLBACK EnumTopWindowsProcFunction(HWND Window, LPARAM LParam)
{
	TopActiveWindows[TopWindowCount] = Window;
	TopWindowCount++;
	return 1;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************


int32 CALLBACK EnumWindowsProcFunction(HWND Window, LPARAM LParam)
{
	if (WindowCount + 32 >= MaxWindowCount)
		AllocateMemWindowProperties(MaxWindowCount + 256);

	WindowProperties[WindowCount].WindowHandle = Window;
	WindowProperties[WindowCount].Level = NestingLevel;
	/*
	  if ((int)Window==0x5b090a) {
	    ok=1;
	  }
	*/
	WindowCount++;
	return 1;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetWindowProperties(WindowInfoRecord ** OwnWindowInfo, int32 mode)
{
	int32 cnt, TopWindowIndex, res;
	WINDOWINFO WindowInfo;

	if (!WindowProperties)
		AllocateMemWindowProperties(0);

	WindowCount = 0;
	TopWindowCount = 0;
	EnumDesktopWindows(NULL, &EnumTopWindowsProcFunction, 0);

	for (cnt = 0; cnt < TopWindowCount; cnt++)
	{
		WindowProperties[WindowCount].WindowHandle = TopActiveWindows[cnt];
		/*
		    if (WindowProperties[WindowCount].WindowHandle==(HANDLE)0x002403c4) {
		      ok=1;
		    }
		*/
		TopWindowIndex = WindowCount;

		if (WindowCount + 32 >= MaxWindowCount)
			res = AllocateMemWindowProperties(MaxWindowCount + 256);

		WindowProperties[WindowCount++].TopWindow = 1;

//    printf("Top window: handle 0x%08x\n",TopActiveWindows[cnt]);
		if (mode == 0)
		{
			NestingLevel = 1;
			EnumChildWindows(TopActiveWindows[cnt], &EnumWindowsProcFunction, 0);
			WindowProperties[TopWindowIndex].WindowCount = WindowCount - TopWindowIndex;
		}
	}

	/*
	  GetTempDir(str2);
	  sprintf(str3,"%s\\ProcessList.txt",str2);
	  sprintf(str,"WMIC /OUTPUT:%s PROCESS get Commandline,Processid",str3);
	*/

	WindowInfo.cbSize = sizeof(WINDOWINFO);
	NestingLevel = 0;

	for (cnt = 0; cnt < WindowCount; cnt++)
	{
		/*
		    if (WindowProperties[cnt].WindowHandle==(HANDLE)0x0040be6) {
		      ok=1;
		    }
		*/
		GetWindowText(WindowProperties[cnt].WindowHandle, WindowProperties[cnt].Name, 95);
		GetWindowInfo(WindowProperties[cnt].WindowHandle, &WindowInfo);
		GetWindowThreadProcessId(WindowProperties[cnt].WindowHandle, &WindowProperties[cnt].ProcessID);
		WindowProperties[cnt].Parent = GetParent(WindowProperties[cnt].WindowHandle);
		WindowProperties[cnt].Style = WindowInfo.dwStyle;
//    WindowProperties[cnt].Status=WindowInfo.dwWindowStatus;
		WindowProperties[cnt].ExStyle = WindowInfo.dwExStyle;
		WindowProperties[cnt].atomWindowType = WindowInfo.atomWindowType;
		WindowProperties[cnt].SizeX = WindowInfo.rcWindow.right - WindowInfo.rcWindow.left;
		WindowProperties[cnt].SizeY = WindowInfo.rcWindow.bottom - WindowInfo.rcWindow.top;
		WindowProperties[cnt].PosX = WindowInfo.rcWindow.left;
		WindowProperties[cnt].PosY = WindowInfo.rcWindow.top;
		GetClassName(WindowProperties[cnt].WindowHandle, WindowProperties[cnt].ClassName, 63);
		res = GetClassInfo(NULL, (LPCTSTR) (uint32) WindowProperties[cnt].atomWindowType, &WindowProperties[cnt].Class);

		if ((WindowProperties[cnt].ClassName) && (WindowProperties[cnt].Name[0] == 0)
		        && (strcmp(WindowProperties[cnt].ClassName, "Edit") == 0))
			SendMessage(WindowProperties[cnt].WindowHandle, WM_GETTEXT, 95, (LPARAM) WindowProperties[cnt].Name);
	}

// ***************************************************************************************************
// ***************************************************************************************************

	if (OwnWindowInfo)
		*OwnWindowInfo = WindowProperties;

	return WindowCount;
}



// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetWindowPropertiesToFile(LPSTR Filename, int32 mode)
{
	int32 cnt, cnt2, fp, *PID = NULL;
	char str[400];

	if (GetWindowProperties(NULL, mode) == 0)
		return;

	if ((mode & 3) == 3)
	{
		PID = malloc(16384 * sizeof(int32));

		if (!PID)
			return;

		memset(PID, 0, 16384 * sizeof(int32));

		for (cnt = 0; cnt < WindowCount; cnt++)
		{
			if (WindowProperties[cnt].TopWindow)
				PID[WindowProperties[cnt].ProcessID] = 1;
		}
	}

	fp = FileOpenWrite(Filename);

	for (cnt = 0; cnt < WindowCount; cnt++)
	{
		if ((mode & 1) == 0)
		{
			sprintf(str, "%-50s : %-50s : %3d : 0x%08x : 0x%08x : %d,%d %d,%d", WindowProperties[cnt].Name,
			        WindowProperties[cnt].ClassName, WindowProperties[cnt].Level,
			        (int32) WindowProperties[cnt].WindowHandle, WindowProperties[cnt].Style,
			        WindowProperties[cnt].SizeX, WindowProperties[cnt].SizeY, WindowProperties[cnt].PosX,
			        WindowProperties[cnt].PosY);
			WriteLn(fp, str);
		}
		else
		{
			if (WindowProperties[cnt].TopWindow)
			{
				if ((mode & 2) == 0)
				{
					sprintf(str, "%-50s : %-50s : %4ld : 0x%08x : 0x%08x : %d,%d %d,%d", WindowProperties[cnt].Name,
					        WindowProperties[cnt].ClassName, WindowProperties[cnt].ProcessID,
					        (int32) WindowProperties[cnt].WindowHandle, WindowProperties[cnt].Style,
					        WindowProperties[cnt].SizeX, WindowProperties[cnt].SizeY, WindowProperties[cnt].PosX,
					        WindowProperties[cnt].PosY);
					WriteLn(fp, str);
				}
			}
		}
	}

	if ((mode & 3) == 3)
	{
		for (cnt2 = 0; cnt2 < 16384; cnt2++)
		{
			if (PID[cnt2] == 0)
				continue;

			WriteLn(fp,
			        "*********************************************************************************************************************************************");

			for (cnt = 0; cnt < WindowCount; cnt++)
			{
				if ((WindowProperties[cnt].TopWindow) && (WindowProperties[cnt].ProcessID == cnt2))
				{
					sprintf(str, "%-50s : %-50s : %4ld : 0x%08x : 0x%08x : %d,%d %d,%d", WindowProperties[cnt].Name,
					        WindowProperties[cnt].ClassName, WindowProperties[cnt].ProcessID,
					        (int32) WindowProperties[cnt].WindowHandle, WindowProperties[cnt].Style,
					        WindowProperties[cnt].SizeX, WindowProperties[cnt].SizeY, WindowProperties[cnt].PosX,
					        WindowProperties[cnt].PosY);
					WriteLn(fp, str);
				}
			}
		}

		WriteLn(fp,
		        "*********************************************************************************************************************************************");
	}

	FileClose(fp);

	if ((mode & 3) == 3)
		free(PID);
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetProcessModule(DWORD dwPID, DWORD dwModuleID, LPMODULEENTRY32 lpMe32, DWORD cbMe32)
{
	int32 bRet = 0;
	int32 bFound = 0;
	HANDLE hModuleSnap = NULL;
	MODULEENTRY32 me32 = { 0 };

	// Take a snapshot of all modules in the specified process.

	hModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, dwPID);

	if (hModuleSnap == INVALID_HANDLE_VALUE)
		return (0);

	// Fill the size of the structure before using it.

	me32.dwSize = sizeof(MODULEENTRY32);

	// Walk the module list of the process, and find the module of
	// interest. Then copy the information to the buffer pointed
	// to by lpMe32 so that it can be returned to the caller.

	if (Module32First(hModuleSnap, &me32))
	{
		do
		{
			if (me32.th32ModuleID == dwModuleID)
			{
				CopyMemory(lpMe32, &me32, cbMe32);
				bFound = 1;
			}
		}
		while (!bFound && Module32Next(hModuleSnap, &me32));

		bRet = bFound;			// if this sets bRet to 0, dwModuleID
		// no longer exists in specified process
	}
	else
		bRet = 0;				// could not walk module list

	// Do not forget to clean up the snapshot object.

	CloseHandle(hModuleSnap);
	return (bRet);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetProcessListToFile(LPSTR Filename)
{
	char str[200];
	int32 fp;
	HANDLE ProcessSnap = NULL, hProcess = NULL;
	int32 bRet = 0;
	PROCESSENTRY32 pe32 = { 0 };
	FILETIME CreationTime, ExitTime, KernelTime, UserTime;

	//  Take a snapshot of all processes in the system.

	ProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);

	if (ProcessSnap == INVALID_HANDLE_VALUE)
		return (0);


	fp = FileOpenWrite(Filename);

	//  Fill in the size of the structure before using it.

	pe32.dwSize = sizeof(PROCESSENTRY32);

	//  Walk the snapshot of the processes, and for each process,
	//  display information.

	if (Process32First(ProcessSnap, &pe32))
	{
		do
		{

			/*
			          bGotModule = GetProcessModule(pe32.th32ProcessID, pe32.th32ModuleID, &me32, sizeof(MODULEENTRY32));

			            if (bGotModule)
			            {
			                HANDLE hProcess;

			                // Get the actual priority class.
			                hProcess = OpenProcess (PROCESS_ALL_ACCESS,
			                    0, pe32.th32ProcessID);
			                dwPriorityClass = GetPriorityClass (hProcess);
			                CloseHandle (hProcess);

			                // Print the process's information.
			                sprintf(str,"\nPriority Class Base\t%d\n",pe32.pcPriClassBase);
			                WriteLn(fp,str);
			                sprintf(str,"PID\t\t\t%d\n", pe32.th32ProcessID);
			                WriteLn(fp,str);
			                sprintf(str,"Thread Count\t\t%d\n", pe32.cntThreads);
			                WriteLn(fp,str);
			                sprintf(str,"Module Name\t\t%s\n", me32.szModule);
			                WriteLn(fp,str);
			                sprintf(str,"Full Path\t\t%s\n\n", me32.szExePath);
			                WriteLn(fp,str);
			            }
			*/
			// Print the process's information.
			WriteLn(fp, "--------------------------------------------------------------");
			sprintf(str, "Priority Class Base\t%d", (int32) pe32.pcPriClassBase);
			WriteLn(fp, str);
			sprintf(str, "PID\t\t\t%d", (int32) pe32.th32ProcessID);
			WriteLn(fp, str);
			sprintf(str, "Thread Count\t\t%d", (int32) pe32.cntThreads);
			WriteLn(fp, str);
			sprintf(str, "Full Path\t\t%s", pe32.szExeFile);
			WriteLn(fp, str);
			hProcess = OpenProcess(PROCESS_ALL_ACCESS, 0, pe32.th32ProcessID);

			if (hProcess)
			{
				GetProcessTimes(hProcess, &CreationTime, &ExitTime, &KernelTime, &UserTime);
				sprintf(str, "Process usertime %d msec",
				        (int32) (KernelTime.dwLowDateTime + UserTime.dwLowDateTime) / 10000);
				WriteLn(fp, str);
				CloseHandle(hProcess);
			}

		}
		while (Process32Next(ProcessSnap, &pe32));

		bRet = 1;
	}
	else
		bRet = 0;				// could not walk the list of processes

	// Do not forget to clean up the snapshot object.
	FileClose(fp);

	CloseHandle(ProcessSnap);
	return (bRet);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetProcessProperties(ProcessInfoRecord ** OwnProcessInfo)
{
	int32 ProcessesCount;
	SIZE_T MinMem, MaxMem;
	HANDLE ProcessSnap = NULL, ProcessHandle = NULL;
	PROCESSENTRY32 pe32 = { 0 };
//  MODULEENTRY32  me32      = {0};
	FILETIME CreationTime, ExitTime, KernelTime, UserTime;

	//  Take a snapshot of all processes in the system.
	ProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	if (ProcessSnap == INVALID_HANDLE_VALUE)
		return 0;

	if (!ProcessProperties)
		AllocateMemProcessProperties(0);

	//  Fill in the size of the structure before using it.

	pe32.dwSize = sizeof(PROCESSENTRY32);

	//  Walk the snapshot of the processes, and for each process,
	//  display information.
	ProcessesCount = 0;

	if (Process32First(ProcessSnap, &pe32))
	{
		do
		{
			ProcessProperties[ProcessesCount].ParentProcessID = pe32.th32ParentProcessID;
			strcpy(ProcessProperties[ProcessesCount].ExeName, pe32.szExeFile);
			ProcessProperties[ProcessesCount].NrThreads = pe32.cntThreads;
			ProcessProperties[ProcessesCount].ProcessID = pe32.th32ProcessID;
			ProcessProperties[ProcessesCount].UsedTime = 0;
			ProcessProperties[ProcessesCount].MinMemUsedInKb = 0;
			ProcessProperties[ProcessesCount].MaxMemUsedInKb = 0;
			ProcessHandle = OpenProcess(PROCESS_ALL_ACCESS, 0, pe32.th32ProcessID);

			if (ProcessHandle)
			{
				GetProcessTimes(ProcessHandle, &CreationTime, &ExitTime, &KernelTime, &UserTime);
				ProcessProperties[ProcessesCount].UsedTime =
				    (KernelTime.dwLowDateTime + UserTime.dwLowDateTime) / 10000;
				GetProcessWorkingSetSize(ProcessHandle, &MinMem, &MaxMem);
				ProcessProperties[ProcessesCount].MinMemUsedInKb = MinMem / 1024;
				ProcessProperties[ProcessesCount].MaxMemUsedInKb = MaxMem / 1024;
				CloseHandle(ProcessHandle);
			}

			if (ProcessesCount + 32 >= MaxProcessesCount)
				AllocateMemProcessProperties(MaxProcessesCount + 256);

			ProcessesCount++;
		}
		while (Process32Next(ProcessSnap, &pe32));
	}

	CloseHandle(ProcessSnap);

	if (OwnProcessInfo)
		*OwnProcessInfo = ProcessProperties;

	return ProcessesCount;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetProcessHeapProperties(int32 ProcessID, ProcessHeapInfoRecord ** OwnProcessHeapInfo)
{
	int32 HeapCount;
	static int32 res;
	HANDLE ProcessHeapSnap = NULL;
	HEAPLIST32 heaplist32 = { 0 };
	HEAPENTRY32 he32 = { 0 };


	if (!ProcessHeapProperties)
	{
//    AllocateMemProcessHeapProperties(0);
	}

	//  Take a snapshot of all heaps in the system.
	ProcessHeapSnap = CreateToolhelp32Snapshot(TH32CS_SNAPHEAPLIST, ProcessID);

	if (ProcessHeapSnap == INVALID_HANDLE_VALUE)
		return 0;

	//  Fill in the size of the structure before using it.

	heaplist32.dwSize = sizeof(HEAPLIST32);
	HeapCount = 0;

	//  Walk the snapshot of the heaps for the process,
	if (Heap32ListFirst(ProcessHeapSnap, &heaplist32))
	{
		do
		{
			he32.dwSize = sizeof(HEAPENTRY32);

			if (Heap32First(&he32, heaplist32.th32ProcessID, heaplist32.th32HeapID))
			{
				do
				{
					/*
					          ProcessHeapProperties[HeapCount].ProcessID=he32.th32ProcessID;
					          ProcessHeapProperties[HeapCount].StartAddress=he32.dwAddress;
					          ProcessHeapProperties[HeapCount].Length=he32.dwBlockSize;
					          if (HeapCount+32>=MaxProcessHeapsCount) {
					            AllocateMemProcessHeapProperties(MaxProcessHeapsCount+256);
					          }
					          HeapCount++;
					*/
					res = Heap32Next(&he32);
				}
				while (res);
			}
			else
				break;
		}
		while (Heap32ListNext(ProcessHeapSnap, &heaplist32));
	}

	CloseHandle(ProcessHeapSnap);

	if (OwnProcessHeapInfo)
		*OwnProcessHeapInfo = ProcessHeapProperties;

	return HeapCount;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void GetProcessPropertiesToFile(LPSTR Filename, int32 mode)
{
	int32 cnt, fp, ProcessesCount;
	char str[200];

	if ((ProcessesCount = GetProcessProperties(NULL)) == 0)
		return;

	fp = FileOpenWrite(Filename);

	for (cnt = 0; cnt < ProcessesCount; cnt++)
	{
		if (mode == 0)
		{
			sprintf(str, "%-50s : %4d : %6d ms : %6d kb : %6d kb", ProcessProperties[cnt].ExeName,
			        ProcessProperties[cnt].ProcessID, ProcessProperties[cnt].UsedTime,
			        ProcessProperties[cnt].MinMemUsedInKb, ProcessProperties[cnt].MaxMemUsedInKb);
			WriteLn(fp, str);
		}
		else
		{
		}
	}

	FileClose(fp);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetProcessPidFromExefile(LPSTR Filename)
{
	HANDLE ProcessSnap = NULL;
	PROCESSENTRY32 pe32 = { 0 };

	//  Take a snapshot of all processes in the system.

	ProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);

	if (ProcessSnap == INVALID_HANDLE_VALUE)
		return 0;

	//  Fill in the size of the structure before using it.

	pe32.dwSize = sizeof(PROCESSENTRY32);

	//  Walk the snapshot of the processes

	if (Process32First(ProcessSnap, &pe32))
	{
		do
		{
			if (stricmp(pe32.szExeFile, Filename) == 0)
			{
				CloseHandle(ProcessSnap);
				return pe32.th32ProcessID;
			}
		}
		while (Process32Next(ProcessSnap, &pe32));
	}

	CloseHandle(ProcessSnap);
	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 DummyReadProcessMemory(int32 pid, int32 NrMegaBytes, int32 mode)
{
	int32 cnt, cnt2, fp = 0, length, NrModules;
	uint32 memp;
	char str2[200], page_buf[4096];
	HANDLE ProcessId;
	HANDLE ModuleSnap = NULL;
	MODULEENTRY32 me32 = { 0 };

	ProcessId = OpenProcess(PROCESS_ALL_ACCESS, 0, pid);

	if (!ProcessId)
		return -1;

	if (mode == 1)
	{
		sprintf(str2, "procesmem_%d.txt", pid);
		fp = FileOpenWrite(str2);
	}

	memp = 0;

	for (cnt = 0; cnt < NrMegaBytes * 16; cnt++)
	{
		if (mode == 1)
			sprintf(str2, "%08X ................", cnt * 65536);

		for (cnt2 = 0; cnt2 < 16; cnt2++)
		{
			if (ReadProcessMemory(ProcessId, (LPCVOID) memp, page_buf, 4096, NULL))
			{
				if (mode == 1)
					str2[9 + cnt2] = '*';
			}

			memp += 4096;
		}

		if (mode == 1)
			WriteLn(fp, str2);
	}

	ModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, pid);
	me32.dwSize = sizeof(MODULEENTRY32);

	if (mode == 1)
		WriteLn(fp, "\r\nModule memory\r\n");

	NrModules = 0;

	if (Module32First(ModuleSnap, &me32))
	{
		do
		{
			memp = (uint32) me32.modBaseAddr;
			length = me32.modBaseSize;

			if (mode == 1)
			{
				sprintf(str2, "%08X : %08X   ; %s", memp, length, me32.szExePath);
				WriteLn(fp, str2);
			}

			if (NrModules > 0)
			{
				while (length > 0)
				{
					if (ReadProcessMemory(ProcessId, (LPCVOID) memp, page_buf, min(length, 4096), NULL))
					{
					}

					length -= 4096;
					memp += 4096;
				}
			}

			NrModules++;
		}
		while (Module32Next(ModuleSnap, &me32));
	}

	CloseHandle(ModuleSnap);
	CloseHandle(ProcessId);

	/*
	  if (mode==1) {
	    WriteLn(fp,"\r\nHeap memory\r\n");
	  }
	  NrProcessHeaps=GetProcessHeapProperties(pid,NULL);
	  for (cnt=0;cnt<NrProcessHeaps;cnt++) {
	    memp=ProcessHeapProperties[cnt].StartAddress;
	    length=ProcessHeapProperties[cnt].Length;
	    if (mode==1) {
	      sprintf(str2,"%08X : %08X ",memp,length);
	      WriteLn(fp,str2);
	    }

	    while (length>0) {
	      if (ReadProcessMemory(ProcessId,(LPCVOID)memp,page_buf,min(4096,length),NULL)) {
	      }
	      length-=4096;
	      memp+=4096;
	    }
	  }
	*/
	if (mode == 1)
		FileClose(fp);

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *INDENT-OFF*
void GetCPUID(uint32 op, uint32 *EAX, uint32 *EBX, uint32 *ECX, uint32 *EDX)

{
#ifdef GCC_COMP
	uint32 regA, regB, regC, regD;


	asm("movl   $0,%%ecx\n\t"
		"cpuid\n\t"
		: "=a" (regA),
		"=b" (regB),
		"=c" (regC),
		"=d" (regD)
		: "a" (op));

	*EAX = regA;
	*EBX = regB;
	*ECX = regC;
	*EDX = regD;
#else
	DWORD A, B, C, D;
	_asm {
		mov eax, op
		mov ecx, 0
		cpuid
		mov A, eax
		mov B, ebx
		mov C, ecx
		mov D, edx
	}
	*EAX = A;
	*EBX = B;
	*ECX = C;
	*EDX = D;
#endif
}

// *INDENT-ON*
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
