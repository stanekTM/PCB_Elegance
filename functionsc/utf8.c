/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: utf8.c
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
#include "utf8.h"


WCHAR TempStrUC[MAX_LENGTH_UC_STRING];
WCHAR TempStrUC2[MAX_LENGTH_UC_STRING];

HGLOBAL GlobalTempMemUTF8;
int32 MaxTempMemoryUTF8, MaxLengthBigUcString;
uint8 *TempMemUTF8;

HGLOBAL GlobalTemp2MemUTF8;
int32 MaxTemp2MemoryUTF8, MaxLength2BigUcString;
uint8 *Temp2MemUTF8;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemTempUTF8(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxTempMemoryUTF8 == 0)
	{
		if ((GlobalTempMemUTF8 = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((TempMemUTF8 = GlobalLock(GlobalTempMemUTF8)) == NULL)
			return -1;

		MaxTempMemoryUTF8 = MemSize;
		MaxLengthBigUcString = MemSize / 2;
	}
	else
	{
		if (MemSize > MaxTempMemoryUTF8)
		{
			if ((NewMem = GlobalReAlloc(GlobalTempMemUTF8, MemSize, GHND)) == NULL)
				return -1;

			GlobalTempMemUTF8 = NewMem;

			if ((TempMemUTF8 = GlobalLock(GlobalTempMemUTF8)) == NULL)
				return -1;

			MaxTempMemoryUTF8 = MemSize;
			MaxLengthBigUcString = MemSize / 2;
		}
	}

	return 0;
}


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AllocateMemTemp2_UTF8(int32 MemSize)
{
	HGLOBAL NewMem;

	MemSize = max(MemSize, 16384);

	if (MaxTemp2MemoryUTF8 == 0)
	{
		if ((GlobalTemp2MemUTF8 = GlobalAlloc(GHND, MemSize)) == NULL)
			return -1;

		if ((Temp2MemUTF8 = GlobalLock(GlobalTemp2MemUTF8)) == NULL)
			return -1;

		MaxTemp2MemoryUTF8 = MemSize;
		MaxLength2BigUcString = MemSize / 2;
	}
	else
	{
		if (MemSize > MaxTemp2MemoryUTF8)
		{
			if ((NewMem = GlobalReAlloc(GlobalTemp2MemUTF8, MemSize, GHND)) == NULL)
				return -1;

			GlobalTemp2MemUTF8 = NewMem;

			if ((Temp2MemUTF8 = GlobalLock(GlobalTemp2MemUTF8)) == NULL)
				return -1;

			MaxTemp2MemoryUTF8 = MemSize;
			MaxLength2BigUcString = MemSize / 2;
		}
	}

	return 0;
}



// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

__inline int32 CheckForUtf8Chars(LPSTR str)
{
	int32 cnt;
	uint8 ch;

	for (cnt = 0; cnt < (int32) strlen(str); cnt++)
	{
		ch = (uint8) * str++ & 0xe0;

		if ((ch == 0xc0) || (ch == 0xd0) || (ch == 0xe0))
			return 1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


LRESULT SendMessageUTF8(HWND hWnd, uint32 Msg, WPARAM wParam, LPARAM lParam)
{
	int32 res;

	if ((Msg == WM_SETTEXT) || (Msg == LB_GETTEXT) || (Msg == LB_ADDSTRING) || (Msg == LB_FINDSTRING)
	        || (Msg == EM_REPLACESEL))
	{
		if (Msg == LB_GETTEXT)
		{
			res = SendMessageW(hWnd, Msg, wParam, (LPARAM) TempStrUC);

			if (res == LB_ERR)
				return res;

			if (UnicodeToUtf8((WCHAR *) TempStrUC, (char *) lParam, MAX_LENGTH_UC_STRING - 1) == 0)
				return 0;

			return strlen((LPSTR) lParam);
		}
		else
		{
			if (Utf8ToUnicode((char *) lParam, TempStrUC, MAX_LENGTH_UC_STRING - 1) == 0)
				return 0;

			return SendMessageW(hWnd, Msg, wParam, (LPARAM) TempStrUC);
		}
	}
	else
		return SendMessage(hWnd, Msg, wParam, lParam);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SetWindowTextUTF8(HWND hWnd, LPSTR lpString)
{
	WCHAR TempStrUC[MAX_LENGTH_UC_STRING];

	/*
	  if (!CheckForUtf8Chars(lpString)) {
	    return SetWindowText(hWnd,lpString);
	  }
	*/
	if (Utf8ToUnicode((char *) lpString, TempStrUC, MAX_LENGTH_UC_STRING - 1) == 0)
		return 0;

//  MessageBoxW(hWnd,TempStrUC,L"Window text",MB_APPLMODAL+MB_OK);
	return SetWindowTextW(hWnd, TempStrUC);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LRESULT SendDlgItemMessageUTF8(HWND hDlg, int32 nIDDlgItem, uint32 Msg, WPARAM wParam, LPARAM lParam)
{
	WCHAR TempStrUC[MAX_LENGTH_UC_STRING];
	int32 result, lengte;
	uint16 *SrcMem16, *DestMem16;

	switch (Msg)
	{
	case LB_ADDSTRING:
	case LB_FINDSTRING:
	case CB_ADDSTRING:
	case CB_SELECTSTRING:
	case EM_REPLACESEL:
	case WM_SETTEXT:
		if (Utf8ToUnicode((char *) lParam, TempStrUC, MAX_LENGTH_UC_STRING - 1) == 0)
			return 0;

		return SendDlgItemMessageW(hDlg, nIDDlgItem, Msg, wParam, (LPARAM) TempStrUC);

	case EM_GETLINE:
		SrcMem16 = (uint16 *) lParam;
		DestMem16 = (uint16 *) TempStrUC;
		lengte = min(MAX_LENGTH_UC_STRING - 1, *SrcMem16);
		*DestMem16 = (uint16) lengte;
		result = SendDlgItemMessageW(hDlg, nIDDlgItem, Msg, wParam, (LPARAM) TempStrUC);

		if (result == 0)
			return 0;

		TempStrUC[result] = 0;

		if (UnicodeToUtf8((WCHAR *) TempStrUC, (char *) lParam, lengte) == 0)
			return 0;

		return strlen((LPSTR) lParam);

	case CB_GETLBTEXT:
	case LB_GETTEXT:
		result = SendDlgItemMessageW(hDlg, nIDDlgItem, Msg, wParam, (LPARAM) TempStrUC);

		if (result == 0)
			return 0;

		TempStrUC[result] = 0;

		if (UnicodeToUtf8(TempStrUC, (char *) lParam, MAX_LENGTH_UC_STRING - 1) == 0)
			return 0;

		return strlen((LPSTR) lParam);

	case WM_GETTEXT:
		result = SendDlgItemMessageW(hDlg, nIDDlgItem, Msg, min(MAX_LENGTH_UC_STRING - 1, wParam), (LPARAM) TempStrUC);

		if (result == 0)
			return 0;

		if (UnicodeToUtf8(TempStrUC, (char *) lParam, min(MAX_LENGTH_UC_STRING - 1, wParam) - 1) == 0)
			return 0;

		return strlen((LPSTR) lParam);

	default:
		return SendDlgItemMessage(hDlg, nIDDlgItem, Msg, wParam, lParam);
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

LRESULT SendDlgItemBigMessageUTF8(HWND hDlg, int32 nIDDlgItem, uint32 Msg, WPARAM wParam, LPARAM lParam)
{
	int32 result, lengte;

//  uint16 *SrcMem16,*DestMem16;
	switch (Msg)
	{
	case LB_ADDSTRING:
	case LB_FINDSTRING:
	case CB_ADDSTRING:
	case CB_SELECTSTRING:
	case EM_REPLACESEL:
	case WM_SETTEXT:
		lengte = strlen((LPSTR) lParam);
		AllocateMemTempUTF8((lengte + 1) * 2);

		if (Utf8ToUnicode((char *) lParam, (WCHAR *) TempMemUTF8, MaxLengthBigUcString - 1) == 0)
			return 0;

		return SendDlgItemMessageW(hDlg, nIDDlgItem, Msg, wParam, (LPARAM) TempMemUTF8);

	/*
	    case EM_GETLINE:
	      SrcMem16=(uint16 *)lParam;
	      lengte=*SrcMem16;
	      AllocateMemTempUTF8(lengte*2);
	      DestMem16=(uint16 *)TempMemUTF8;
	      *DestMem16=lengte;
	      result=SendDlgItemMessageW(hDlg,nIDDlgItem,Msg,wParam,(LPARAM)TempMemUTF8);
	      if (result==0) return 0;
	      if (UnicodeToUtf8((WCHAR *)TempMemUTF8,(LPSTR)lParam,lengte)==0) return 0;
	      return strlen((LPSTR)lParam);
	*/
	case WM_GETTEXT:
		AllocateMemTempUTF8(wParam * 2);
		result = SendDlgItemMessageW(hDlg, nIDDlgItem, Msg, wParam, (LPARAM) TempMemUTF8);

		if (result == 0)
			return 0;

		if (UnicodeToUtf8((WCHAR *) TempMemUTF8, (char *) lParam, wParam) == 0)
			return 0;

		return strlen((LPSTR) lParam);

	default:
		return SendDlgItemMessage(hDlg, nIDDlgItem, Msg, wParam, lParam);
	}
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

void SetDialogItemTextUTF8(HWND Dialog, int32 DlgItem, LPSTR Text)
{
	WCHAR TempStrUC[MAX_LENGTH_UC_STRING];

	if (Utf8ToUnicode(Text, TempStrUC, MAX_LENGTH_UC_STRING - 1) == 0)
		SendDlgItemMessage(Dialog, DlgItem, WM_SETTEXT, 0, (LPARAM) Text);
	else
		SendDlgItemMessageW(Dialog, DlgItem, WM_SETTEXT, 0, (LPARAM) TempStrUC);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 MessageBoxUTF8(HWND Window, LPSTR Text, LPSTR Caption, uint32 MessageBoxType)
{
	int32 lengte;

	lengte = strlen(Text);
	/*
	  if ((!CheckForUtf8Chars(Text))
	     &&
	     (!CheckForUtf8Chars(Caption))) {
	    return MessageBox(Window,Text,Caption,MessageBoxType);
	  }
	  MultiByteToWideChar(CP_UTF8,0,
	*/
	AllocateMemTempUTF8(lengte * 2);

	if ((Utf8ToUnicode(Caption, TempStrUC2, MAX_LENGTH_UC_STRING - 1) == 0)
	        || (Utf8ToUnicode(Text, (WCHAR *) TempMemUTF8, MaxLengthBigUcString - 1) == 0))
		return MessageBox(Window, Text, Caption, MessageBoxType);

	return MessageBoxW(Window, (WCHAR *) TempMemUTF8, TempStrUC2, MessageBoxType);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 TextOutUTF8(HDC hdc, int32 nXStart, int32 nYStart, LPSTR lpString, int32 cbString)
{
	WCHAR TempStrUC[MAX_LENGTH_UC_STRING];

	if (Utf8ToUnicode(lpString, TempStrUC, MAX_LENGTH_UC_STRING - 1) > 0)
		return TextOutW(hdc, nXStart, nYStart, TempStrUC, wcslen(TempStrUC));
	else
		return TextOut(hdc, nXStart, nYStart, lpString, strlen(lpString));
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 AppendMenuUTF8(HMENU Menu, uint32 MenuOptions, uint32 MenuId, LPSTR Text)
{
	WCHAR TempStrUC[MAX_LENGTH_UC_STRING];

	if (((MenuOptions & (MF_SEPARATOR | MF_BITMAP | MF_OWNERDRAW)) != 0) || (!Text)
	        || (Utf8ToUnicode(Text, TempStrUC, MAX_LENGTH_UC_STRING - 1) == 0))
		return AppendMenu(Menu, MenuOptions, MenuId, Text);
	else
		return AppendMenuW(Menu, MenuOptions, MenuId, TempStrUC);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

int32 ModifyMenuUTF8(HMENU hMnu, uint32 uPosition, uint32 uFlags, uint32 uIDNewItem, LPSTR lpNewItem)
{
	if (Utf8ToUnicode(lpNewItem, TempStrUC, MAX_LENGTH_UC_STRING - 1) == 0)
		return ModifyMenu(hMnu, uPosition, uFlags, uIDNewItem, lpNewItem);
	else
		return ModifyMenuW(hMnu, uPosition, uFlags, uIDNewItem, TempStrUC);
}

// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************
// *****************************************************************************************

int32 CopyStrToClipboardUTF8(HWND Window, LPSTR StrToCopy)
{
	HANDLE hGlobalMemory;
	LPSTR ClipBuf;
	int32 Length, MemSize;

	Length = strlen(StrToCopy) * 2;

	if (OpenClipboard(Window))
	{
		MemSize = max(16 * 1024, Length + 1024);
		hGlobalMemory = GlobalAlloc(GHND, MemSize);
		ClipBuf = GlobalLock(hGlobalMemory);
		Utf8ToUnicode(StrToCopy, (WCHAR *) ClipBuf, MemSize / 2 - 1);
//    strcpy(ClipBuf,StrToCopy);
		GlobalUnlock(hGlobalMemory);
		EmptyClipboard();
		SetClipboardData(CF_UNICODETEXT, hGlobalMemory);
		CloseClipboard();
	}

	return 0;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 strcmpUTF8(LPSTR Src, LPSTR Dest)
{
	int32 lengte, lengte2;

	lengte = strlen(Src);

	if (lengte == 0)
		return -1;

	AllocateMemTempUTF8(lengte * 2);
	lengte2 = strlen(Dest);

	if (lengte2 == 0)
		return 1;

	AllocateMemTemp2_UTF8(lengte2 * 2);

	if (Utf8ToUnicode(Src, (WCHAR *) TempMemUTF8, MaxLengthBigUcString - 1) == 0)
		return 0;

	if (Utf8ToUnicode(Dest, (WCHAR *) Temp2MemUTF8, MaxLength2BigUcString - 1) == 0)
		return 0;

	return wcscmp((WCHAR *) TempMemUTF8, (WCHAR *) Temp2MemUTF8);
}


// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 stricmpUTF8(LPSTR Src, LPSTR Dest)
{
	int32 lengte, lengte2;

	lengte = strlen(Src);

	if (lengte == 0)
		return -1;

	AllocateMemTempUTF8(lengte * 2);
	lengte2 = strlen(Dest);

	if (lengte2 == 0)
		return 1;

	AllocateMemTemp2_UTF8(lengte2 * 2);

	if (Utf8ToUnicode(Src, (WCHAR *) TempMemUTF8, MaxLengthBigUcString - 1) == 0)
		return 0;

	if (Utf8ToUnicode(Dest, (WCHAR *) Temp2MemUTF8, MaxLength2BigUcString - 1) == 0)
		return 0;

	return _wcsicmp((WCHAR *) TempMemUTF8, (WCHAR *) Temp2MemUTF8);
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 strnicmpUTF8(LPSTR Src, LPSTR Dest, int32 count)
{
	int32 lengte, lengte2;

	lengte = strlen(Src);

	if (count == 0)
		return 0;

	if (lengte == 0)
		return -1;

	AllocateMemTempUTF8(lengte * 2);
	lengte2 = strlen(Dest);

	if (lengte2 == 0)
		return 1;

	AllocateMemTemp2_UTF8(lengte2 * 2);

	if (Utf8ToUnicode(Src, (WCHAR *) TempMemUTF8, MaxLengthBigUcString - 1) == 0)
		return 0;

	if (Utf8ToUnicode(Dest, (WCHAR *) Temp2MemUTF8, MaxLength2BigUcString - 1) == 0)
		return 0;

	return _wcsnicmp((WCHAR *) TempMemUTF8, (WCHAR *) Temp2MemUTF8, count);
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 struprUTF8(LPSTR Src)
{
	int32 lengte;

	lengte = strlen(Src);

	if (lengte == 0)
		return -1;

	AllocateMemTempUTF8(lengte * 2);

	if (Utf8ToUnicode(Src, (WCHAR *) TempMemUTF8, MaxLengthBigUcString - 1) == 0)
		return 0;

	wcsupr((WCHAR *) TempMemUTF8);

	if (UnicodeToUtf8((WCHAR *) TempMemUTF8, Src, MaxLengthBigUcString - 1) == 0)
		return 0;

	return 1;
}

// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************
// ********************************************************************************************

int32 strlwrUTF8(LPSTR Src)
{
	int32 lengte;

	lengte = strlen(Src);

	if (lengte == 0)
		return -1;

	AllocateMemTempUTF8(lengte * 2);

	if (Utf8ToUnicode(Src, (WCHAR *) TempMemUTF8, MaxLengthBigUcString - 1) == 0)
		return 0;

	wcslwr((WCHAR *) TempMemUTF8);

	if (UnicodeToUtf8((WCHAR *) TempMemUTF8, Src, MaxLengthBigUcString - 1) == 0)
		return 0;

	return 1;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

HANDLE FindFirstFileUTF8(LPSTR Src, WIN32_FIND_DATAW * FindFileData)
{
	if (Utf8ToUnicode(Src, TempStrUC, MAX_LENGTH_UC_STRING - 1) == 0)
		return NULL;

	return FindFirstFileW(TempStrUC, FindFileData);
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 ConvertFileNameFromCommandLineUTF8(LPSTR Src, LPSTR Dest)
{
	int32 cnt, length;
	char NewStr[MAX_LENGTH_STRING];
	WCHAR *FileName;

	if (Utf8ToUnicode(Src, TempStrUC, MAX_LENGTH_UC_STRING - 50) == 0)
		return -1;

	length = wcslen(TempStrUC);

	for (cnt = 0; cnt < length; cnt++)
	{
		if ((int32) TempStrUC[cnt] >= 256)
			break;
	}

	if (cnt == length)
	{
		for (cnt = 0; cnt < length; cnt++)
			NewStr[cnt] = (char) TempStrUC[cnt];

		NewStr[length] = 0;

		if (Utf8ToUnicode(NewStr, TempStrUC, MAX_LENGTH_STRING - 50) == 0)
			return -1;
	}

	if (GetFullPathNameW(TempStrUC, MAX_LENGTH_STRING - 50, TempStrUC2, &FileName) == 0)
		return -1;

	if (UnicodeToUtf8(TempStrUC2, Dest, MAX_LENGTH_STRING - 50) == 0)
		return -1;

	return 0;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 Utf8ToUnicode(char *utf8, uint16 * unicode16, int32 MaxUCLength)
{
	int32 Utf8Length, result;

	if ((!utf8) || (!unicode16))
		return 0;

	Utf8Length = strlen(utf8);
	result = MultiByteToWideChar(CP_UTF8, 0, (char *) utf8, Utf8Length, unicode16, MaxUCLength);

	if (result >= 0)
		unicode16[result] = 0;

	return result;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 UnicodeToUtf8(uint16 * unicode16, char *utf8, int32 MaxUtf8Length)
{
	int32 UCLength, result;

	if ((!utf8) || (!unicode16))
		return 0;

	UCLength = wcslen(unicode16);
	result = WideCharToMultiByte(CP_UTF8, 0, unicode16, UCLength, (char *) utf8, MaxUtf8Length, NULL, NULL);

	if (result >= 0)
		utf8[result] = 0;

	return result;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 Utf8ToUnicode_old(uint8 * utf8, uint16 * unicode16, int32 MaxUCLength)
{
	int32 count = 0, UCLength = 0, Utf8Length;
	uint8 c0, c1;
	uint16 scalar;

	if ((!utf8) || (!unicode16))
		return 0;

	Utf8Length = strlen((char *) utf8);

	while (--Utf8Length >= 0)
	{
		c0 = *utf8++;
		/*DPRINTF("Trying: %02x\n",c0); */

		if (c0 < 0x80)
		{
			/* Plain ASCII character, simple translation :-) */
			*unicode16++ = c0;

			if (UCLength == MaxUCLength - 1)
			{
				*unicode16++ = 0;
				return count;
			}

			count++;
			continue;
		}

		if ((c0 & 0xc0) == 0x80)
			/* Illegal; starts with 10xxxxxx */
			return -1;

		/* c0 must be 11xxxxxx if we get here => at least 2 bytes */
		scalar = c0;

		if (--Utf8Length < 0)
			return -1;

		c1 = *utf8++;

		/*DPRINTF("c1=%02x\n",c1); */
		if ((c1 & 0xc0) != 0x80)
			/* Bad byte */
			return -1;

		scalar <<= 6;
		scalar |= (c1 & 0x3f);

		if (!(c0 & 0x20))
		{
			/* Two bytes UTF-8 */
			if ((scalar != 0) && (scalar < 0x80))
				return -1;		/* Overlong encoding */

			*unicode16++ = (unsigned short) (scalar & 0x7ff);

			if (UCLength == MaxUCLength - 1)
			{
				*unicode16++ = 0;
				return count;
			}

			count++;
			continue;
		}

		/* c0 must be 111xxxxx if we get here => at least 3 bytes */
		if (--Utf8Length < 0)
			return -1;

		c1 = *utf8++;

		/*DPRINTF("c1=%02x\n",c1); */
		if ((c1 & 0xc0) != 0x80)
			/* Bad byte */
			return -1;

		scalar <<= 6;
		scalar |= (c1 & 0x3f);

		if (!(c0 & 0x10))
		{
			/*DPRINTF("####\n"); */
			/* Three bytes UTF-8 */
			if (scalar < 0x800)
				return -1;		/* Overlong encoding */

			if (scalar >= 0xd800 && scalar < 0xe000)
				return -1;		/* UTF-16 high/low halfs */

			*unicode16++ = (unsigned short) (scalar & 0xffff);

			if (UCLength == MaxUCLength - 1)
			{
				*unicode16++ = 0;
				return count;
			}

			count++;
			continue;
		}

		/* c0 must be 1111xxxx if we get here => at least 4 bytes */
		*unicode16++ = 0;
		return -1;				/* No support for more than three byte UTF-8 */
	}

	*unicode16++ = 0;
	return count;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 UnicodeToUtf8_old(uint16 * unicode16, uint8 * utf8, int32 MaxUtf8Length)
{
	int32 count, count2 = 0, UCLength;
	uint16 UCChar;

	if ((!utf8) || (!unicode16))
		return 0;

	UCLength = wcslen(unicode16);

	for (count = 0; count < UCLength; count++)
	{
		UCChar = *unicode16++;

		if (UCChar == 0)
		{
			*utf8++ = 0;
			return count2;
		}

		if (UCChar < 0x0080)
		{
			if (count2 >= MaxUtf8Length - 1)
			{
				*utf8++ = 0;
				return -1;
			}

			*utf8++ = (uint8) UCChar;
			count2++;
			continue;
		}

		if (UCChar < 0x0800)
		{
			if (count2 >= MaxUtf8Length - 2)
			{
				*utf8++ = 0;
				return -1;
			}

			/*
			c0 = | 0000 | 0xxx | xx00 | 0000 |  + 0xc0  (5 bits)
			c1 = | 0000 | 0000 | 00xx | xxxx |  + 0x80  (6 bits)
			*/
			*utf8++ = (uint8) (0xc0 + ((UCChar >> 6) & 0x1f));
			count2++;
			*utf8++ = (uint8) (0x80 + (UCChar & 0x3f));
			count2++;
			continue;
		}

		if (count2 >= MaxUtf8Length - 3)
		{
			*utf8++ = 0;
			return -1;
		}

		/*
		c0 = | xxxx | 0000 | 0000 | 0000 |  + 0xe0  (4 bits)
		c1 = | 0000 | xxxx | xx00 | 0000 |  + 0x80  (6 bits)
		c2 = | 0000 | 0000 | 00xx | xxxx |  + 0x80  (6 bits)
		*/
		*utf8++ = (uint8) (0xe0 + ((UCChar >> 12) & 0x0f));
		count2++;
		*utf8++ = (uint8) (0x80 + ((UCChar >> 6) & 0x3f));
		count2++;
		*utf8++ = (uint8) (0x80 + (UCChar & 0x3f));
		count2++;
	}

	*utf8++ = 0;
	return count2;
}


// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 GetPosUTF8(uint8 * utf8, int32 pos)
{
	int32 cc, count = 0, index = 0, index2 = 0;
	uint8 c0, c1;
	uint32 scalar;

	cc = strlen((char *) utf8);

	while (--cc >= 0)
	{
		if (count == pos)
			return index;

		index2 = index;
		index++;
		c0 = *utf8++;
		/*DPRINTF("Trying: %02x\n",c0); */

		if (c0 < 0x80)
		{
			/* Plain ASCII character, simple translation :-) */
			count++;
			continue;
		}

		if ((c0 & 0xc0) == 0x80)
			/* Illegal; starts with 10xxxxxx */
			return index2;

		/* c0 must be 11xxxxxx if we get here => at least 2 bytes */
		scalar = c0;

		if (--cc < 0)
			return index2;

		index++;
		c1 = *utf8++;

		/*DPRINTF("c1=%02x\n",c1); */
		if ((c1 & 0xc0) != 0x80)
			/* Bad byte */
			continue;

		scalar <<= 6;
		scalar |= (c1 & 0x3f);

		if (!(c0 & 0x20))
		{
			/* Two bytes UTF-8 */
			if (scalar < 0x80)
				continue;		/* Overlong encoding */

			count++;
			continue;
		}

		/* c0 must be 111xxxxx if we get here => at least 3 bytes */
		if (--cc < 0)
			return index2;

		c1 = *utf8++;

		/*DPRINTF("c1=%02x\n",c1); */
		if ((c1 & 0xc0) != 0x80)
			/* Bad byte */
			return index2;

		scalar <<= 6;
		scalar |= (c1 & 0x3f);

		if (!(c0 & 0x10))
		{
			/*DPRINTF("####\n"); */
			/* Three bytes UTF-8 */
			count++;
			continue;
		}

		/* c0 must be 1111xxxx if we get here => at least 4 bytes */
		c1 = *utf8++;

		if (--cc < 0)
			return index2;
	}

	return index2;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 StrlenUTF8(uint8 * utf8)
{
	int32 cc, count = 0;
	uint8 c0, c1;
	uint32 scalar;

	cc = strlen((char *) utf8);

	while (--cc >= 0)
	{
		c0 = *utf8++;
		/*DPRINTF("Trying: %02x\n",c0); */

		if (c0 < 0x80)
		{
			/* Plain ASCII character, simple translation :-) */
			count++;
			continue;
		}

		if ((c0 & 0xc0) == 0x80)
			/* Illegal; starts with 10xxxxxx */
			continue;

		/* c0 must be 11xxxxxx if we get here => at least 2 bytes */
		scalar = c0;

		if (--cc < 0)
			return count;

		c1 = *utf8++;

		/*DPRINTF("c1=%02x\n",c1); */
		if ((c1 & 0xc0) != 0x80)
			/* Bad byte */
			return count;

		scalar <<= 6;
		scalar |= (c1 & 0x3f);

		if (!(c0 & 0x20))
		{
			/* Two bytes UTF-8 */
			count++;
			continue;
		}

		/* c0 must be 111xxxxx if we get here => at least 3 bytes */
		if (--cc < 0)
			return count;

		c1 = *utf8++;

		/*DPRINTF("c1=%02x\n",c1); */
		if ((c1 & 0xc0) != 0x80)
			/* Bad byte */
			continue;

		scalar <<= 6;
		scalar |= (c1 & 0x3f);

		if (!(c0 & 0x10))
		{
			/*DPRINTF("####\n"); */
			/* Three bytes UTF-8 */
			count++;
			continue;
		}

		/* c0 must be 1111xxxx if we get here => at least 4 bytes */
		c1 = *utf8++;

		if (--cc < 0)
			return count;

		count++;
	}

	return count;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
