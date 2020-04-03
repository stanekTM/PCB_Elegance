/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: utf8.h
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


#ifndef _UTF8_

#define _UTF8_

#include  "owntypes.h"
#include  "windows.h"


#define MAX_LENGTH_UC_STRING          4096

#ifdef UC

int32 SetWindowTextUTF8(HWND hWnd, LPSTR lpString);

LRESULT SendDlgItemMessageUTF8(HWND hDlg, int32 nIDDlgItem, uint32 Msg, WPARAM wParam, LPARAM lParam);

LRESULT SendDlgItemBigMessageUTF8(HWND hDlg, int32 nIDDlgItem, uint32 Msg, WPARAM wParam, LPARAM lParam);

void SetDialogItemTextUTF8(HWND Dialog, int32 DlgItem, LPSTR Text);

int32 MessageBoxUTF8(HWND Window, LPSTR Text, LPSTR Caption, uint32 MessageBoxType);

LRESULT SendMessageUTF8(HWND hWnd, uint32 Msg, WPARAM wParam, LPARAM lParam);

int32 TextOutUTF8(HDC hdc, int32 nXStart, int32 nYStart, LPSTR lpString, int32 cbString);

int32 AppendMenuUTF8(HMENU Menu, uint32 MenuOptions, uint32 MenuId, LPSTR Text);

int32 ModifyMenuUTF8(HMENU hMnu, uint32 uPosition, uint32 uFlags, uint32 uIDNewItem, LPSTR lpNewItem);

int32 strcmpUTF8(LPSTR Src, LPSTR Dest);

int32 stricmpUTF8(LPSTR Src, LPSTR Dest);

int32 strnicmpUTF8(LPSTR Src, LPSTR Dest, int32 count);

int32 struprUTF8(LPSTR Src);

int32 strlwrUTF8(LPSTR Src);

HANDLE FindFirstFileUTF8(LPSTR Src, WIN32_FIND_DATAW * FindFileData);

int32 ConvertFileNameFromCommandLineUTF8(LPSTR Src, LPSTR Dest);

#define   SetWindowTextOwn            SetWindowTextUTF8
#define   SendDlgItemMessageOwn       SendDlgItemMessageUTF8
#define   SendDlgItemBigMessageOwn    SendDlgItemBigMessageUTF8
#define   MessageBoxOwn               MessageBoxUTF8
#define   AppendMenuOwn               AppendMenuUTF8
#define   ModifyMenuOwn               ModifyMenuUTF8
#define   SendMessageOwn              SendMessageUTF8
#define   CopyStrToClipboardOwn       CopyStrToClipboardUTF8
#define   strcmpOwn                   strcmpUTF8
#define   stricmpOwn                  stricmpUTF8

#else

#define   SendDlgItemBigMessageOwn    SendDlgItemMessage
#define   SetWindowTextOwn            SetWindowText
#define   SendDlgItemMessageOwn       SendDlgItemMessage
#define   SendDlgItemBigMessageOwn    SendDlgItemMessage
#define   MessageBoxOwn               MessageBox
#define   AppendMenuOwn               AppendMenu
#define   ModifyMenuOwn               ModifyMenu
#define   SendMessageOwn              SendMessage
#define   CopyStrToClipboardOwn       CopyStrToClipboard
#define   stricmpOwn                  stricmp
#define   strcmpOwn                   strcmp


#endif

int32 Utf8ToUnicode(char *utf8, uint16 * unicode16, int32 MaxUCLength);

int32 UnicodeToUtf8(uint16 * unicode16, char *utf8, int32 MaxUtf8Length);

#endif // _UTF8_
