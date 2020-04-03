/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: property.c
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
#include "edit.h"
#include "edit2.h"
#include "files2.h"
#include "rect.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "sch.h"
#include "calc.h"
#include "files.h"
#include "toets.h"
#include "calcdef.h"
#include "math.h"
#include "calc2.h"
#include "graphics.h"
#include "calcrect.h"
#include "mainloop.h"
#include "insdel.h"
#include "select.h"
#include "dialogs.h"
#include "stdio.h"
#include "utf8.h"
#include "resource.h"
#include "property.h"
#include "help.h"


LPSTR NewNetLabelStr;
char NewPropertyID[MAX_LENGTH_STRING], NewPropertyValue[MAX_LENGTH_STRING], CachedPropertyID[32][100],
     CachedPropertyValue[32][100], TempPropertyID[32][100], TempPropertyValue[32][100],
     CachedNetLabelStr[MAX_LENGTH_STRING], TempNetName[MAX_LENGTH_STRING], PropertyFileName[MAX_LENGTH_STRING],
     PropertyFileName2[MAX_LENGTH_STRING];

int32 CachedPropertyCount, NrProperties, PropertyFileCached, PropertyFilePos, PropertyBufLength;
uint64 LatestPropertyFileTimeForCache, LatestPropertyFileTime;
int64 PropertyTimeStamp;
char *PropertyBuf;
WIN32_FIND_DATAW PropertyFileData;

extern int64 CurrentFrequency;

// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************

int32 ReadLnProp(LPSTR FileName, LPSTR LineBuf)
{
	int32 BytesRead = 0, cnt, fp;
	int32 LineLength, OldPos, count, count2;
	char *BufP;
	LineLength = 0;

	if ((PropertyBufLength > 0) && (PropertyFilePos >= PropertyBufLength))
	{
		PropertyFilePos = 0;
		return -1;
	}

	cnt = 1;

	if (PropertyFileCached == 0)
	{
		fp = FileOpenUTF8(FileName);

		if (fp <= 0)
			return -1;

		BytesRead = 0;

		if (PropertyBuf == NULL)
			AllocateSpecialMem(MEM_PROPERTIES, 32768, (void *) &PropertyBuf);

		if ((FileRead(fp, PropertyBuf, 32768, &BytesRead) == -1) || (BytesRead == 0))
		{
			FileClose(fp);
			return -2;
		}

		FileClose(fp);
		PropertyFilePos = 0;
		PropertyFileCached = 1;
		PropertyBufLength = BytesRead;
	}

	OldPos = PropertyFilePos;
	BufP = &PropertyBuf[PropertyFilePos];
	count = PropertyBufLength - PropertyFilePos;

	while ((count > 0) && (*BufP != 10))
	{
		count--;
		BufP++;
		PropertyFilePos++;
	}

	count2 = min(256, PropertyFilePos - OldPos);
	memcpy(&LineBuf[0], &PropertyBuf[OldPos], count2);
	LineLength = count2;

	if (LineLength > 0)
	{
		cnt = LineLength - 1;

		while ((cnt >= 0) && ((LineBuf[cnt] == 13) || (LineBuf[cnt] == ' ')))
			cnt--;

		LineLength = cnt + 1;
	}

	PropertyFilePos++;
	LineBuf[LineLength] = 0;
	return LineLength;
}

//*************************************************************************************************************************
//******************************* IDD_DIALOG_PROPERTY *********************************************************************
//*************************************************************************************************************************

int32 CALLBACK PropertyDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	char str[2048], str2[2048];
	int32 res, cnt, NrLines;
	uint16 *Lengte;
	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(330, "Add/modify net properties"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(124, "ID"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(100, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(108, "Net labels"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(123, "Properties"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON1, SC(247, "Delete"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON2, SC(457, "Add TRACEWIDTH property"));
		SetDialogItemTextUTF8(Dialog, IDC_BUTTON3, SC(458, "Add CLEARANCE property"));

		str[0] = 0;

		for (cnt = 0; cnt < NrProperties; cnt++)
		{
			strcat(str, TempPropertyID[cnt]);
			strcat(str, "\r\n");
		}

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str);
		str[0] = 0;

		for (cnt = 0; cnt < NrProperties; cnt++)
		{
			strcat(str, TempPropertyValue[cnt]);
			strcat(str, "\r\n");
		}

		SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) str);
		SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) TempNetName);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			NrLines = -1;
			Lengte = (uint16 *) & str;

			for (cnt = 31; cnt >= 0; cnt--)
			{
				memset(str, 0, MAX_LENGTH_STRING);
				*Lengte = 80;

				if ((res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) str)) > 0)
				{
					if (NrLines == -1)
						NrLines = cnt + 1;
				}
			}

			SendDlgItemMessageUTF8(Dialog, IDC_EDIT3, WM_GETTEXT, 80, (LPARAM) TempNetName);
			NrProperties = 0;
			memset(TempPropertyID, 0, sizeof(TempPropertyID));
			memset(TempPropertyValue, 0, sizeof(TempPropertyValue));

			if (NrLines <= 0)
				EndDialog(Dialog, 1);

			for (cnt = 0; cnt < NrLines; cnt++)
			{
				memset(str, 0, MAX_LENGTH_STRING);
				*Lengte = 80;
				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, EM_GETLINE, cnt, (LPARAM) str);
				str[res] = 0;
				strcpy(TempPropertyID[NrProperties], str);
				memset(str, 0, MAX_LENGTH_STRING);
				*Lengte = 80;
				res = SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, EM_GETLINE, cnt, (LPARAM) str);
				str[res] = 0;
				strcpy(TempPropertyValue[NrProperties], str);
				NrProperties++;
			}

			EndDialog(Dialog, 1);
			return about;

		case IDC_BUTTON1:
			EndDialog(Dialog, 2);
			return about;

		case IDC_BUTTON2:
		case IDC_BUTTON3:
			str[0] = 0;
			str2[0] = 0;
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT1, WM_GETTEXT, 2000, (LPARAM) str);
			SendDlgItemMessageUTF8(Dialog, IDC_EDIT2, WM_GETTEXT, 2000, (LPARAM) str2);

			if (((res = strlen(str)) > 1) && (str[res - 2] != '\r') && (str[res - 1] != '\n'))
				strcat(str, "\r\n");

			if (((res = strlen(str2)) > 1) && (str2[res - 2] != '\r') && (str2[res - 1] != '\n'))
				strcat(str2, "\r\n");

			switch (LOWORD(WParam))
			{
			case IDC_BUTTON2:
				strcat(str, "TRACEWIDTH\r\n");
				strcat(str2, "8 mil\r\n");
				break;

			case IDC_BUTTON3:
				strcat(str, "CLEARANCE\r\n");
				strcat(str2, "8 mil\r\n");
				break;
			}

			SendDlgItemMessage(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) str);
			SendDlgItemMessage(Dialog, IDC_EDIT2, WM_SETTEXT, 0, (LPARAM) str2);
			break;

		case IDCANCEL:
			EndDialog(Dialog, 3);
			return about;

		case IDHELP:
			Help("netlabel_properties.htm", 0);
			break;
		}

		break;
	}

	about = 0;
	return about;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 LoadPropertyFileName(int32 mode)
{
	char str[MAX_LENGTH_STRING];

	if (PropertyFileName[0] == 0)
	{
		if (DesignPath[0] == 0)
			return -1;

		if (DesignFile[0] != 0)
		{
//      MessageBoxUTF8(SCHWindow,DesignFile,"DesignFile",MB_APPLMODAL|MB_OK);
			GetFilePartFromFileName(str, DesignFile);
			CutExtensionFileName(str);
//      MessageBoxUTF8(SCHWindow,str,"str1",MB_APPLMODAL|MB_OK);
		}
		else
		{
			if (EditFile[0] != 0)
			{
				GetFilePartFromFileName(str, DesignPath);
//        MessageBoxUTF8(SCHWindow,str,"str2",MB_APPLMODAL|MB_OK);
			}
			else
				return -1;
		}

		sprintf(PropertyFileName, "%s\\%s.prp", DesignPath, str);
		sprintf(PropertyFileName2, "%s\\%s_new.prp", DesignPath, str);
//    MessageBoxUTF8(SCHWindow,PropertyFileName,"PropertyFileName",MB_APPLMODAL|MB_OK);
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetFileDatePropertyFileName(WIN32_FIND_DATAW * FileData)
{
	int64 Counter, TimeDivMilliSeconds;
	HANDLE FileSearch;

	if (PropertyTimeStamp == 0)
	{
		QueryPerformanceCounter((LARGE_INTEGER *) & PropertyTimeStamp);

		if ((FileSearch = FindFirstFileUTF8(PropertyFileName, FileData)) == INVALID_HANDLE_VALUE)
			return -1;

		FindClose(FileSearch);
		memcpy(&PropertyFileData, FileData, sizeof(WIN32_FIND_DATA));
		return 0;
	}

	QueryPerformanceCounter((LARGE_INTEGER *) & Counter);
	TimeDivMilliSeconds = (Counter - PropertyTimeStamp) * 1000;
	TimeDivMilliSeconds /= CurrentFrequency;

	if (TimeDivMilliSeconds > 100)
	{
		QueryPerformanceCounter((LARGE_INTEGER *) & PropertyTimeStamp);

		if ((FileSearch = FindFirstFileUTF8(PropertyFileName, FileData)) == INVALID_HANDLE_VALUE)
			return -1;

		FindClose(FileSearch);
	}

	memcpy(FileData, &PropertyFileData, sizeof(WIN32_FIND_DATA));
	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ModifyNetProperty(int32 NetLabelIndex, int32 mode)
{
	char PropertyID[MAX_LENGTH_STRING], PropertyValue[MAX_LENGTH_STRING], LineBuf[MAX_LENGTH_STRING],
	     str[MAX_LENGTH_STRING], NetLabelStr[MAX_LENGTH_STRING];
	int32 count, cnt, cnt2, Length, Found, fp, Modification, Changed, res;
	NetLabelRecord *NetLabel, NewNetLabel;
	WireRecord *Wire;
	BusRecord *Bus;

	Changed = 0;

	if (mode == 0)
	{
		Found = -1;

		if (WiresSelected == 1)
		{
			for (cnt = 0; cnt < Design.NrWires; cnt++)
			{
				Wire = &((*Wires)[cnt]);

				if ((Wire->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				{
					Found = cnt;
					break;
				}
			}

			if (Found == -1)
				return -1;

			Wire = &((*Wires)[Found]);
			Wire->Info &= ~OBJECT_SELECTED;
			Found = -1;

			for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
			{
				NetLabel = &((*NetLabels)[cnt2]);

				if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
				{
					if (((InRange(Wire->X1, NetLabel->ConnectX)) && (InRange(Wire->Y1, NetLabel->ConnectY)))
					        || ((InRange(Wire->X2, NetLabel->ConnectX)) && (InRange(Wire->Y2, NetLabel->ConnectY))))
						Found = cnt2;
				}
			}
		}
		else
		{
			if (BussesSelected == 1)
			{
				for (cnt = 0; cnt < Design.NrBusses; cnt++)
				{
					Bus = &((*Busses)[cnt]);

					if ((Bus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
					{
						Found = cnt;
						break;
					}
				}

				if (Found == -1)
					return -1;

				Bus = &((*Busses)[Found]);
				Bus->Info &= ~OBJECT_SELECTED;
				Found = -1;

				for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
				{
					NetLabel = &((*NetLabels)[cnt2]);

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE)) == 0)
					{
						if (((InRange(Bus->X1, NetLabel->ConnectX)) && (InRange(Bus->Y1, NetLabel->ConnectY)))
						        || ((InRange(Bus->X2, NetLabel->ConnectX)) && (InRange(Bus->Y2, NetLabel->ConnectY))))
						{
							NetLabel = &((*NetLabels)[cnt2]);
							res = strlen(NetLabel->Name);

							if (NetLabel->Name[res - 1] == ']')
							{
								Found = cnt2;
								break;
							}
						}
					}
				}
			}
			else
			{
				for (cnt2 = 0; cnt2 < Design.NrNetLabels; cnt2++)
				{
					NetLabel = &((*NetLabels)[cnt2]);

					if ((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
						Found = cnt2;
				}
			}
		}

		if (Found == -1)
		{
			MessageBoxUTF8(SCHWindow, SC(446, "No netlabel found"), SC(38, "Error"), MB_APPLMODAL | MB_OK);
			RePaint();
			return -1;
		}

		NetLabelIndex = Found;
	}

	NetLabel = &((*NetLabels)[NetLabelIndex]);
	NetLabel->Info &= ~OBJECT_SELECTED;
	NewNetLabelStr = NetLabel->Name;

	if (LoadPropertyFileName(0) < 0)
	{
		LatestPropertyFileTimeForCache = 0;
		return -1;
	}

	memset(TempPropertyID, 0, sizeof(TempPropertyID));
	memset(PropertyValue, 0, sizeof(PropertyValue));
	NrProperties = GetProperty(NewNetLabelStr, NULL, NULL, -1);

	for (cnt = 0; cnt < NrProperties; cnt++)
	{
		GetProperty(NewNetLabelStr, PropertyID, PropertyValue, cnt);
		strcpy(TempPropertyID[cnt], PropertyID);
		strcpy(TempPropertyValue[cnt], PropertyValue);
	}

	strcpy(TempNetName, NewNetLabelStr);

	if ((Modification = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_PROPERTY), SCHWindow,
	                      (DLGPROC) PropertyDialog2)) == 3)
	{
		RePaint();
		return 0;
	}

	if ((fp = FileOpenUTF8(PropertyFileName)) < 0)
	{
		switch (Modification)
		{
		case 1:				// Add,replace
			for (cnt = 0; cnt < NrProperties; cnt++)
			{
				sprintf(str, "%s \"%s\"=\"%s\"", TempNetName, TempPropertyID[cnt], TempPropertyValue[cnt]);
				AppendStringToTextFileUTF8(PropertyFileName, str);
			}

			break;

		case 2:				// Delete
			break;
		}

		LatestPropertyFileTimeForCache = 0;
		return 0;
	}
	else
		FileClose(fp);

	/*

	netname:"Property ID"="Property value"

	*/

	DeleteFileUTF8(PropertyFileName2);
	Found = 0;
	count = 0;
	PropertyFilePos = 0;

	while ((Length = ReadLnProp(PropertyFileName, LineBuf)) >= 0)
	{
		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString(LineBuf, NetLabelStr);
			GetQuoteString(LineBuf, PropertyID);
			GetQuoteString(LineBuf, PropertyValue);

			if (stricmpUTF8(TempNetName, NewNetLabelStr) == 0)
			{
				if (stricmpUTF8(NetLabelStr, NewNetLabelStr) == 0)
				{
					if (count == 0)
					{
						switch (Modification)
						{
						case 1:	// Add,replace
							for (cnt = 0; cnt < NrProperties; cnt++)
							{
								sprintf(str, "%s \"%s\"=\"%s\"", NewNetLabelStr, TempPropertyID[cnt],
								        TempPropertyValue[cnt]);
								AppendStringToTextFile(PropertyFileName2, str);
							}

							break;
						}
					}

					Found = 1;
					count++;
				}
				else
				{
					sprintf(str, "%s \"%s\"=\"%s\"", NetLabelStr, PropertyID, PropertyValue);
					AppendStringToTextFileUTF8(PropertyFileName2, str);
					count = 0;
				}
			}
			else
			{
				Found = 2;

				if ((stricmpUTF8(NetLabelStr, NewNetLabelStr) != 0) && (stricmpUTF8(NetLabelStr, TempNetName) != 0))
				{
					// Only copy properties from netnames not equal to TempNetName and NewNetLabelStr
					sprintf(str, "%s \"%s\"=\"%s\"", NetLabelStr, PropertyID, PropertyValue);
					AppendStringToTextFileUTF8(PropertyFileName2, str);
				}
			}
		}
		else
			AppendStringToTextFileUTF8(PropertyFileName2, LineBuf);
	}

	switch (Modification)
	{
	case 1:					// Add,replace
		switch (Found)
		{
		case 0:
			for (cnt = 0; cnt < NrProperties; cnt++)
			{
				sprintf(str, "%s \"%s\"=\"%s\"", TempNetName, TempPropertyID[cnt], TempPropertyValue[cnt]);
				AppendStringToTextFileUTF8(PropertyFileName, str);
			}

			DeleteFileUTF8(PropertyFileName2);
			break;

		case 1:
			DeleteFileUTF8(PropertyFileName);
			MoveFileUTF8(PropertyFileName2, PropertyFileName);
			break;

		case 2:
			for (cnt = 0; cnt < NrProperties; cnt++)
			{
				sprintf(str, "%s \"%s\"=\"%s\"", TempNetName, TempPropertyID[cnt], TempPropertyValue[cnt]);
				AppendStringToTextFileUTF8(PropertyFileName2, str);
			}

			DeleteFileUTF8(PropertyFileName);
			MoveFileUTF8(PropertyFileName2, PropertyFileName);
			break;
		}

		break;

	case 2:					// Delete
		DeleteFileUTF8(PropertyFileName);
		MoveFileUTF8(PropertyFileName2, PropertyFileName);
		break;
	}

	LatestPropertyFileTimeForCache = 0;
	PropertyFileCached = 0;

	if (stricmpUTF8(TempNetName, NetLabel->Name) != 0)
	{
		memcpy(&NewNetLabel, NetLabel, sizeof(NetLabelRecord));
		NewNetLabel.Info &= ~OBJECT_SELECTED;
		strcpy(NewNetLabel.Name, TempNetName);
		AddNetLabel(&NewNetLabel);
		NetLabel = &((*NetLabels)[NetLabelIndex]);
		NetLabel->Info |= OBJECT_NOT_VISIBLE;
		NetLabel->DeleteNr = (int16) LastActionNr;
		Changed = 1;
	}

	if (mode == 0)
		RePaint();
	else
	{
		if (Changed)
			return 1;
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 GetProperty(LPSTR NetLabelStr, LPSTR PropertyID, LPSTR PropertyValue, int32 mode)
{
	WIN32_FIND_DATAW FileData;
	int32 Length, count, Found;
	char LineBuf[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING];

	if (LoadPropertyFileName(0) < 0)
	{
		if (mode != -1)
		{
			PropertyID[0] = 0;
			PropertyValue[0] = 0;
		}

		LatestPropertyFileTimeForCache = 0;
		PropertyFileCached = 0;
		return -1;
	}

	if (GetFileDatePropertyFileName(&FileData) == -1)
	{
		if (mode != -1)
		{
			PropertyID[0] = 0;
			PropertyValue[0] = 0;
		}

		LatestPropertyFileTimeForCache = 0;
		PropertyFileCached = 0;
		return -1;
	}

	if (LatestPropertyFileTimeForCache == 0)
	{
		if (mode != -1)
			memcpy(&LatestPropertyFileTimeForCache, &FileData.ftLastWriteTime, 8);
	}
	else
	{
		if (memcmp(&LatestPropertyFileTimeForCache, &FileData.ftLastWriteTime, 8) == 0)
		{
			if (stricmpUTF8(NetLabelStr, CachedNetLabelStr) == 0)
			{
				if ((mode >= 0) && (mode < 32))
				{
					strcpy(PropertyID, CachedPropertyID[mode]);
					strcpy(PropertyValue, CachedPropertyValue[mode]);
					return 1;
				}

				if (mode == -1)
					return CachedPropertyCount;
			}

			PropertyFileCached = 0;
		}
		else
			LatestPropertyFileTimeForCache = 0;
	}

	if (mode != -1)
	{
		PropertyID[0] = 0;
		PropertyValue[0] = 0;
	}

	count = 0;
	Found = 0;
	PropertyFilePos = 0;

	while ((Length = ReadLnProp(PropertyFileName, LineBuf)) >= 0)
	{
		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (LineBuf[0] != '#'))
		{
			GetString(LineBuf, str);
			GetQuoteString(LineBuf, str2);
			GetQuoteString(LineBuf, str3);

			if (mode == -1)
			{
				if (stricmpUTF8(NetLabelStr, str) == 0)
					count++;
				else
				{
					if (count > 0)
						return count;
				}
			}
			else
			{
				if (stricmpUTF8(NetLabelStr, str) == 0)
				{
					if (stricmpUTF8(NetLabelStr, CachedNetLabelStr) != 0)
					{
						memset(CachedPropertyID, 0, sizeof(CachedPropertyID));
						memset(CachedPropertyValue, 0, sizeof(CachedPropertyValue));
					}

					if (count < 32)
					{
						strcpy(CachedPropertyID[count], str2);
						strcpy(CachedPropertyValue[count], str3);
					}

					strcpy(CachedNetLabelStr, str);

					if (mode == count)
					{
						strcpy(PropertyID, str2);
						strcpy(PropertyValue, str3);
						Found = 1;
					}

					count++;
					CachedPropertyCount = count;
				}
				else
				{
					if (Found)
						break;

					count = 0;
				}
			}
		}
	}

	if (mode == -1)
		return count;

	if (Found)
		return 1;

	CachedNetLabelStr[0] = 0;
	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 CheckNewProperties(int32 mode)
{
	WIN32_FIND_DATAW FileData;
	HANDLE FileSearch;

	if (LoadPropertyFileName(0) < 0)
	{
		LatestPropertyFileTime = 0;
		return 0;
	}

	if ((FileSearch = FindFirstFileUTF8(PropertyFileName, &FileData)) == INVALID_HANDLE_VALUE)
	{
		LatestPropertyFileTime = 0;
		return 0;
	}

	FindClose(FileSearch);

	if (LatestPropertyFileTime == 0)
		memcpy(&LatestPropertyFileTime, &FileData.ftLastWriteTime, 8);
	else
	{
		if (memcmp(&LatestPropertyFileTime, &FileData.ftLastWriteTime, 8) != 0)
		{
			memcpy(&LatestPropertyFileTime, &FileData.ftLastWriteTime, 8);
			return 1;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
