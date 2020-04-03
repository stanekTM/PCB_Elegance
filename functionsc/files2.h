/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: files2.h
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


#ifndef _FILES2

#define _FILES2

#include  "owntypes.h"
#include  "windows.h"


extern int32 EndOfLineMode, TextLineNr, WriteLnError, MaxLineLength, FileP;
extern char EndOfLineChar;

int32 FileOpen(LPSTR FileName);

int32 FileOpenWrite(LPSTR FileName);

int32 FileOpenWriteAppend(LPSTR FileName);

int32 CheckForWritingAndOpen(LPSTR FileName, int32 FileSize, HWND Window);

int32 FileOpenReadOnly(LPSTR FileName);

int32 TextFileOpen(LPSTR FileName);

int32 TextFileOpenAgain(LPSTR FileName, int32 mode);

int32 TextFileReset(int32 fp, int32 mode);

int32 TextFileWrite(LPSTR FileName);

int32 TextFileWriteClose(int32 fp);

int32 ReadLnToChar(int32 fp, char EndOfLineChar, LPSTR LineBuf);

int32 ReadLn(int32 fp, LPSTR Buf);

int32 ReadLn2(int32 fp, LPSTR LineBuf);

int32 ReadLnWithMaxLength(int32 fp, LPSTR LineBuf, int32 MaxCurrentLineLength);

int32 TextFileClose(int32 fp);

int32 FileCurrentPointer(int32 fp);

int32 FileClose(int32 fp);

int32 CheckReadOnly(LPSTR FileName);

int32 CheckReadOnlyW(LPSTR FileName);

int32 FileSeek(int32 fp, int32 FilePos);

int32 FileRead(int32 fp, void *Buf, int32 count, int32 * BytesRead);

int32 FileWrite(int32 fp, void *Buf, int32 count, int32 * BytesWritten);

int32 WriteLn(int32 fp, LPSTR LineBuf);

int32 WriteLn2(int32 fp, int32 EndOfLineMode, LPSTR LineBuf);

// int32  WriteStr(int32 fp,LPSTR LineBuf);

int32 WriteToFile(int32 fp, LPSTR LineBuf);

void GetSpecialString(LPSTR Str, LPSTR Result, int32 mode);

void GetString(LPSTR Str, LPSTR Result);

int32 GetStrings(LPSTR Str, int32 MaxLength, char **StrP, int32 MaxStrings);

int32 GetAttrNamesAndValues(LPSTR Str, int32 MaxLength, char **AttrNamesP, char **AttrValuesP, int32 MaxStrings);

int32 GetStringsWithQuotes(LPSTR Str, int32 MaxLength, char **StrP, int32 MaxStrings);

void GetCommaString(LPSTR Str, LPSTR Result);

void GetStringTab(LPSTR Str, LPSTR Result);

void GetString3(char EscapeChar, LPSTR Str, LPSTR Result);

void GetQuoteString(LPSTR Str, LPSTR Result);

void GetQuoteString2(LPSTR Str, LPSTR Result);

void GetBracesString(LPSTR Str, LPSTR Result);

void GetParenthesisString(LPSTR Str, LPSTR Result);

int32 GetStringValue(LPSTR Line, LPSTR Key, LPSTR StrValue);

int32 FileExists(LPSTR FileName);

int32 DirectoryExists(LPSTR Dir);

int32 FileSize(LPSTR FileName);

int32 FileCurrentPointer(int32 fp);

int32 GetDirFromFileName(LPSTR Dir, LPSTR FileName);

int32 GetFilePartFromFileName(LPSTR FilePart, LPSTR FileName);

void CutExtensionFileName(LPSTR FileName);

void ConvertPathToWindowsStyle(LPSTR FileName);

void GetExtensionFileName(LPSTR Extension, LPSTR FileName);

void ReplaceExtensionFileName(LPSTR FileName, LPSTR Extension);

void AddExtensionToFileName(LPSTR FileName, LPSTR ExtensionToAdd);

void GetNameWithOutExtensionFromFileName(LPSTR FileName, LPSTR Name);

void AddBackSlash(LPSTR FileName);

void CutBackSlashDir(LPSTR Dir);

int32 DecodeQuotedString(LPSTR Source, LPSTR Dest);

void GetPercentageString(LPSTR Str, LPSTR Result);

int32 ExpandWithEnvironmentStrings(LPSTR str);

int32 CharInString(char ch, LPSTR TextString);

void StripAppendingZeros(LPSTR str, int32 mode);

int32 AppendStringToTextFile(LPSTR Filename, LPSTR TextToAppend);

void strcatzero(LPSTR str, LPSTR addstr);

int32 GetNewFile(HWND Window, HINSTANCE hInstance, LPSTR OwnFile, LPSTR DirPath, LPSTR FileInfoStr, LPSTR FileInfoStr2,
                 LPSTR DialogInfo, LPSTR Extension, int32 mode);

// *******************************************************************************************************

int32 FileOpenUTF8(LPSTR FileName);

int32 FileOpenWriteUTF8(LPSTR FileName);

int32 FileOpenWriteAppendUTF8(LPSTR FileName);

int32 CheckForWritingAndOpenUTF8(LPSTR FileName, int32 FileSize, HWND Window);

int32 FileOpenReadOnlyUTF8(LPSTR FileName);

int32 AppendStringToTextFileUTF8(LPSTR Filename, LPSTR TextToAppend);

int32 TextFileOpenUTF8(LPSTR FileName);

int32 TextFileWriteUTF8(LPSTR FileName);

int32 CopyFileUTF8(LPSTR Src, LPSTR Dest, int32 bFailIfExists);

int32 DeleteFileUTF8(LPSTR Src);

int32 MoveFileUTF8(LPSTR Src, LPSTR Dest);

int32 DirectoryExistsUTF8(LPSTR Dir);

int32 FileExistsUTF8(LPSTR FileName);

int32 FileExistsW(LPWSTR FileName);

int32 FileSizeUTF8(LPSTR FileName);

int32 SetCurrentDirectoryUTF8(LPSTR Dir);

int32 CreateDirectoryUTF8(LPSTR Dir);

int32 GetCurrentDirectoryUTF8(int32 MaxLength, LPSTR Dir);

int32 GetTempDir(LPSTR Dir);

int32 GetTempDirUTF8(LPSTR Dir);

int32 DeleteDirectory(LPSTR Dir);

int32 DeleteDirectoryUnicode(WCHAR * Dir);

int32 GetNewFileUTF8(HWND Window, HINSTANCE hInstance, LPSTR OwnFile, LPSTR DirPath, LPSTR FileInfoStr,
                     LPSTR FileInfoStr2, LPSTR DialogInfo, LPSTR Extension, int32 mode);

#ifdef GCC_COMP

int32 strlen_SSE42(const char *s);

int32 strlen_SSE2(const char *s);

#endif

int32 IsWow64(void);

#endif
