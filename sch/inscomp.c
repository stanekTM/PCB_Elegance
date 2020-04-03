/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: inscomp.c
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
#include "files2.h"
#include "files.h"
#include "inscomp.h"
#include "stdio.h"
#include "sch.h"
#include "help.h"
#include "edit2.h"
#include "memory.h"
#include "resource.h"
#include "utf8.h"


HMENU CompMenu1, CompMenu2[20], CompMenu3[30];

int32 NrCompSelects, DialogMode2, FoundComps1, FoundComps2;

extern char DialogTextLine[MAX_LENGTH_STRING];
extern int32 OkToAddSymbol, EscapeInsertSymbol;

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void DeleteCompMenu()
{
	int32 cnt;

	for (cnt = 0; cnt < 20; cnt++)
	{
		if (CompMenu2[cnt] != 0)
			DestroyMenu(CompMenu2[cnt]);
	}

	for (cnt = 0; cnt < 3; cnt++)
	{
		if (CompMenu3[cnt] != 0)
			DestroyMenu(CompMenu3[cnt]);
	}

	if (CompMenu1 != 0)
		DestroyMenu(CompMenu1);
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

HMENU MakeCompMenu()
{
	char LineBuf[512], FileStr[MAX_LENGTH_STRING], Str1[MAX_LENGTH_STRING], FirstChar;
	int32 Length, fp, Code1, Num, cnt, cnt2, cnt3, MenuPos1, MenuPos2, MenuCode;

	cnt3 = 0;

	for (cnt = 0; cnt < 20; cnt++)
		CompMenu2[cnt] = 0;

	for (cnt = 0; cnt < 20; cnt++)
		CompMenu3[cnt] = 0;

	CompMenu1 = 0;
	strcpy(FileStr, ProjectPath);
	AddBackSlash(FileStr);
	strcat(FileStr, "compmenu.txt");

	if (FileExistsUTF8(FileStr) != 0)
	{
		strcpy(FileStr, DesignPath);
		AddBackSlash(FileStr);
		strcat(FileStr, "compmenu.txt");

		if (FileExistsUTF8(FileStr) != 0)
		{
			MessageBoxUTF8(SCHWindow, FileStr, SC(195, "Can not find file"), MB_APPLMODAL | MB_OK);
			return (HMENU) 0;
		}
	}

	if ((fp = TextFileOpenUTF8(FileStr)) < 0)
	{
		MessageBoxUTF8(SCHWindow, FileStr, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
		return (HMENU) 0;
	}



#ifdef _DEBUG
	cnt2 = sizeof(CompSelectRecord);
#endif
	AllocateSpecialMem(MEM_COMPSELECT, MaxCompSelect * sizeof(CompSelectRecord), (void *) &CompSelects);
	NrCompSelects = 0;
	Code1 = 0;
	MenuPos1 = -1;
	MenuPos2 = -1;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		LineBuf[Length] = 0;

		if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/') && (NrCompSelects < MaxCompSelect))
		{
			FirstChar = LineBuf[0];

			switch (FirstChar)
			{
			case '$':
				GetString((LPSTR) & LineBuf[1], Str1);

				if (sscanf(Str1, "%d", &Num) == 1)
				{
					GetQuoteString(LineBuf, Str1);

					if ((Num > 0) && (Num < 10) && (Str1[0] != 0))
					{
						if (Num == 9)
						{
							Code1 = Num;
							CompSelect = &((*CompSelects)[NrCompSelects]);
							CompSelect->Info = 10;
							NrCompSelects++;
						}

						Code1 = Num;
						CompSelect = &((*CompSelects)[NrCompSelects]);
						CompSelect->Info = 1;
						CompSelect->Code1 = Code1;
						MenuPos2 = -1;
						memmove(CompSelect->Name, &Str1, sizeof(CompSelect->Name) - 1);
						NrCompSelects++;
					}
				}

				break;

			case '#':
				if (Code1 > 0)
				{
					GetString((LPSTR) & LineBuf[1], Str1);

					if (sscanf(Str1, "%d", &Num) == 1)
					{
						GetQuoteString(LineBuf, Str1);

						if ((Num > 0) && (Num < 1000) && (Str1[0] != 0))
						{
							CompSelect = &((*CompSelects)[NrCompSelects]);
							CompSelect->Info = 2;
							CompSelect->Info2 = 0;
							CompSelect->Code1 = Code1;
							CompSelect->Code2 = Num;
							MenuPos2 = NrCompSelects;
							memmove(CompSelect->Name, &Str1, sizeof(CompSelect->Name) - 1);
							NrCompSelects++;
						}
					}
				}

				break;

			case '^':
				if (Code1 > 0)
				{
					GetString((LPSTR) & LineBuf[1], Str1);

					if (sscanf(Str1, "%d", &Num) == 1)
					{
						GetQuoteString(LineBuf, Str1);

						if ((Num > 0) && (Num < 1000) && (Str1[0] != 0))
						{
							CompSelect = &((*CompSelects)[NrCompSelects]);
							CompSelect->Info = 3;
							CompSelect->Code1 = Code1;
							CompSelect->Code2 = Num;
							memmove(CompSelect->Name, &Str1, sizeof(CompSelect->Name) - 1);

							if (MenuPos2 != -1)
							{
								CompSelect = &((*CompSelects)[MenuPos2]);
								CompSelect->Info2++;
							}

							NrCompSelects++;
						}
					}
				}

				break;
			}
		}

//      OutputDebugStr(LineBuf);
//      OutputDebugStr("\n");
	}

	/*
	  if (Length<-1) {
	    MessageBoxUTF8(SCHWindow,FileStr,SC(274,"Error in reading file"),MB_APPLMODAL|MB_OK);
	    return (HMENU)0;
	  }
	*/
	if (TextFileClose(fp) != 0)
	{
		MessageBoxUTF8(SCHWindow, FileStr, SC(196, "Error in closing file"), MB_APPLMODAL | MB_OK);
		return (HMENU) 0;
	}

	CompMenu1 = CreatePopupMenu();
	cnt2 = -1;

	for (cnt = 0; cnt < NrCompSelects; cnt++)
	{
		CompSelect = &((*CompSelects)[cnt]);

		switch (CompSelect->Info)
		{
		case 1:
			cnt3 = -1;
			cnt2++;

			if (cnt2 < 20)
			{
				CompMenu2[cnt2] = CreatePopupMenu();
				AppendMenu(CompMenu1, MF_ENABLED | MF_POPUP, (UINT) CompMenu2[cnt2], CompSelect->Name);
			}

			break;

		case 2:
			if (CompSelect->Info2 > 0)
			{
				if (cnt3 < 30)
				{
					cnt3++;
					CompMenu3[cnt3] = CreatePopupMenu();
					AppendMenu(CompMenu2[cnt2], MF_ENABLED | MF_POPUP, (UINT) CompMenu3[cnt3], CompSelect->Name);
				}
			}
			else
			{
				MenuCode = ID_POPUP_COMP_SELECT + CompSelect->Code1 * 1000 + CompSelect->Code2;
				AppendMenu(CompMenu2[cnt2], MF_ENABLED | MF_STRING, MenuCode, CompSelect->Name);
			}

			break;

		case 3:
			MenuCode = ID_POPUP_COMP_SELECT + CompSelect->Code1 * 1000 + CompSelect->Code2;
			AppendMenu(CompMenu3[cnt3], MF_ENABLED | MF_STRING, MenuCode, CompSelect->Name);
			break;

		case 10:
			AppendMenu(CompMenu1, MF_ENABLED | MF_SEPARATOR, 0, 0);
			break;
		}
	}

//  TrackPopupMenu(Menu1,TPM_RIGHTBUTTON,
//                 RealWindow.left+MousePosX+5,RealWindow.top+MousePosY+40,0,SCHWindow,NULL);
	return CompMenu1;
}

//*************************************************************************************************************************
//***************************** IDD_DIALOG_SELECTCOMP *********************************************************************
//*************************************************************************************************************************

int32 CALLBACK SelectCompDialog2(HWND Dialog, uint32 Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 res, cnt, Comp1, Comp2, Length, Code1, Code2, Selected, lengte, pos, fp, dialogunitX, TabStops[5];
	char LineBuf[512], Name[MAX_LENGTH_STRING], PartNr[MAX_LENGTH_STRING], Value[MAX_LENGTH_STRING],
	     Geometry[MAX_LENGTH_STRING], CompInfo[MAX_LENGTH_STRING], Code1Str[MAX_LENGTH_STRING],
	     Code2Str[MAX_LENGTH_STRING], FileName[MAX_LENGTH_STRING], FileStr[100];
	char DialogTextLine[1024];

	about = 1;
// LB_SETCOLUMNWIDTH
	Comp1 = DialogMode2 / 10000;
	Comp2 = DialogMode2 % 10000;

	switch (Message)
	{
	case WM_INITDIALOG:
		SelectionEsc = 0;
		SetWindowTextUTF8(Dialog, SC(202, "Select component"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(2, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDHELP, SC(3, "Help"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(301, "System components"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(99, "Symbol name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(101, "Part number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(100, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(102, "Geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(107, "Component info"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(302, "User components"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(99, "Symbol name"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(101, "Part number"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(100, "Value"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(102, "Geometry"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(107, "Component info"));

		TabStops[0] = 58 - 9;
		TabStops[1] = 108 - 8;
		TabStops[2] = 159 - 8;
		TabStops[3] = 230 - 8;
		dialogunitX = (100 * 4) / LOWORD(GetDialogBaseUnits());

		FoundComps1 = 0;
		res = SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		sprintf(FileStr, "%s\\comp.txt", ProjectPath);

//      strcpy(FileStr,"c:\\sch\\comp.txt");
		if ((fp = TextFileOpenUTF8(FileStr)) < 0)
		{
			switch (fp)
			{
			default:
				MessageBoxUTF8(SCHWindow, FileStr, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
				return about;
			}
		}

		while ((Length = ReadLn(fp, LineBuf)) >= 0)
		{
			LineBuf[Length] = 0;

			if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/'))
			{
				GetString(LineBuf, Name);
				GetString(LineBuf, Code1Str);
				GetString(LineBuf, Code2Str);
				Code1 = 0;
				Code2 = 0;

				if ((sscanf(Code1Str, "%d", &Code1) == 1) && (sscanf(Code2Str, "%d", &Code2) == 1) && (Code1 >= 1)
				        && (Code1 <= 9) && (Code2 >= 1) && (Code2 < 1000) && (Code1 == Comp1) && (Code2 == Comp2))
				{
					GetQuoteString(LineBuf, PartNr);
					GetQuoteString(LineBuf, Value);
					GetString(LineBuf, Geometry);
					GetQuoteString(LineBuf, CompInfo);
					DialogTextLine[0] = 0;
//            strcpy(PartNr,"321313");
					sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s", Name, PartNr, Value, Geometry, CompInfo);
					SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
					FoundComps1++;
				}
			}
		}

		/*
		      if (Length<-1) {
		        switch (Length) {
		          default:
		            MessageBoxUTF8(SCHWindow,FileStr,SC(274,"Error in reading file"),MB_APPLMODAL|MB_OK);
		            EndDialog(Dialog, 2);
		        }
		      }
		*/
		if (TextFileClose(fp) != 0)
		{
			MessageBoxUTF8(SCHWindow, FileStr, SC(196, "Error in closing file"), MB_APPLMODAL | MB_OK);
			EndDialog(Dialog, 2);
		}

// *****************************************************************************************
// *****************************************************************************************
		FoundComps2 = 0;
		res = SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETTABSTOPS, 4, (LPARAM) (LPINT) & TabStops);
		sprintf(FileStr, "%s\\comp.txt", ProjectPath);

//      strcpy(FileStr,"c:\\sch\\comp.txt");
		if ((fp = TextFileOpenUTF8(FileStr)) < 0)
		{
			switch (fp)
			{
			default:
				MessageBoxUTF8(SCHWindow, FileStr, SC(197, "Error in opening file"), MB_APPLMODAL | MB_OK);
				return about;
			}
		}

		while ((Length = ReadLn(fp, LineBuf)) >= 0)
		{
			LineBuf[Length] = 0;

			if ((Length > 1) && (LineBuf[0] != ';') && (LineBuf[0] != '/'))
			{
				GetString(LineBuf, Name);
				GetString(LineBuf, Code1Str);
				GetString(LineBuf, Code2Str);
				Code1 = 0;
				Code2 = 0;

				if ((sscanf(Code1Str, "%d", &Code1) == 1) && (sscanf(Code2Str, "%d", &Code2) == 1) && (Code1 >= 1)
				        && (Code1 <= 9) && (Code2 >= 1) && (Code2 < 1000) && (Code1 == Comp1) && (Code2 >= Comp2 + 1)
				        && (Code2 < Comp2 + 10))
				{
					GetQuoteString(LineBuf, PartNr);
					GetQuoteString(LineBuf, Value);
					GetString(LineBuf, Geometry);
					GetQuoteString(LineBuf, CompInfo);
					DialogTextLine[0] = 0;
//            strcpy(PartNr,"321313");
					sprintf(DialogTextLine, "%s\t%s\t%s\t%s\t%s", Name, PartNr, Value, Geometry, CompInfo);
					SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) DialogTextLine);
					FoundComps2++;
				}
			}
		}

		/*
		      if (Length<-1) {
		        switch (Length) {
		          default:
		            MessageBoxUTF8(SCHWindow,FileStr,SC(274,"Error in reading file"),MB_APPLMODAL|MB_OK);
		            EndDialog(Dialog, 2);
		        }
		      }
		*/
		if (TextFileClose(fp) != 0)
		{
			MessageBoxUTF8(SCHWindow, FileStr, SC(196, "Error in closing file"), MB_APPLMODAL | MB_OK);
			EndDialog(Dialog, 2);
		}

		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (WParam)
		{
		case IDC_LIST1:
			SendDlgItemMessage(Dialog, IDC_LIST2, LB_SETCURSEL, (WPARAM) - 1, 0);
			break;

		case IDC_LIST2:
			SendDlgItemMessage(Dialog, IDC_LIST1, LB_SETCURSEL, (WPARAM) - 1, 0);
			break;

		case IDOK:
			Selected = SendDlgItemMessage(Dialog, IDC_LIST1, LB_GETCURSEL, 0, 0);
			DialogTextLine[0] = 0;

			if ((Selected >= 0) && (Selected < FoundComps1))
			{
				SendDlgItemMessageUTF8(Dialog, IDC_LIST1, LB_GETTEXT, Selected, (LPARAM) DialogTextLine);
				strcpy(LineBuf, DialogTextLine);

				if (DialogTextLine[0] != 0)
				{
					Length = strlen(DialogTextLine);
//              for (cnt=0;cnt<Length;cnt++) {
//                if (DialogTextLine[cnt]=='\t') DialogTextLine[cnt]=' ';
//              }
					cnt = 0;
					GetStringTab(DialogTextLine, Name);
					GetStringTab(DialogTextLine, PartNr);
					GetStringTab(DialogTextLine, Value);
					GetStringTab(DialogTextLine, Geometry);
					GetStringTab(DialogTextLine, CompInfo);
					memset(&NewInstance, 0, sizeof(InstanceRecord));
					memmove(&NewInstance.Value, Value, sizeof(NewInstance.Value) - 1);
					memmove(&NewInstance.PartNr, PartNr, sizeof(NewInstance.PartNr) - 1);
					memmove(&NewInstance.Geometry, Geometry, sizeof(NewInstance.Geometry) - 1);
					memmove(&NewInstance.PartDescription, CompInfo, sizeof(NewInstance.PartDescription) - 1);

					if (SearchSymbol(Name, FileName, &pos, &lengte, 0) == 1)
					{
						OkToAddSymbol = 1;

						if ((pos == -1) || (pos == -2))
						{
//                  GetSymbol(Name,FileName,"",1);
							strcpy(NewSymbolName, Name);
							strcpy(NewSymbolDir, FileName);
							strcpy(NewLibName, "");
						}
						else
						{
							strcpy(NewSymbolName, Name);
							strcpy(NewSymbolDir, "");
							strcpy(NewLibName, FileName);
//                  GetSymbol(Name,"",FileName,1);
						}
					}
					else
					{
						sprintf(FileStr, SC(203, "Symbol %s not found"), Name);
						MessageBoxUTF8(SCHWindow, FileStr, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						break;
					}
				}

				EndDialog(Dialog, 1);
				return about;
			}

// *****************************************************************************************
// *****************************************************************************************
			Selected = SendDlgItemMessage(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);
			DialogTextLine[0] = 0;

			if ((Selected >= 0) && (Selected < FoundComps2))
			{
				SendDlgItemMessageUTF8(Dialog, IDC_LIST2, LB_GETTEXT, Selected, (LPARAM) DialogTextLine);
				strcpy(LineBuf, DialogTextLine);

				if (DialogTextLine[0] != 0)
				{
					Length = strlen(DialogTextLine);
//              for (cnt=0;cnt<Length;cnt++) {
//                if (DialogTextLine[cnt]=='\t') DialogTextLine[cnt]=' ';
//              }
					cnt = 0;
					GetStringTab(DialogTextLine, Name);
					GetStringTab(DialogTextLine, PartNr);
					GetStringTab(DialogTextLine, Value);
					GetStringTab(DialogTextLine, Geometry);
					GetStringTab(DialogTextLine, CompInfo);
					memset(&NewInstance, 0, sizeof(InstanceRecord));
					memmove(&NewInstance.Value, Value, sizeof(NewInstance.Value) - 1);
					memmove(&NewInstance.PartNr, PartNr, sizeof(NewInstance.PartNr) - 1);
					memmove(&NewInstance.Geometry, Geometry, sizeof(NewInstance.Geometry) - 1);
					memmove(&NewInstance.PartDescription, CompInfo, sizeof(NewInstance.PartDescription) - 1);

					if (SearchSymbol(Name, FileName, &pos, &lengte, 0) == 1)
					{
						OkToAddSymbol = 1;

						if ((pos == -1) || (pos == -2))
						{
//                  GetSymbol(Name,FileName,"",1);
							strcpy(NewSymbolName, Name);
							strcpy(NewSymbolDir, FileName);
							strcpy(NewLibName, "");
						}
						else
						{
							strcpy(NewSymbolName, Name);
							strcpy(NewSymbolDir, "");
							strcpy(NewLibName, FileName);
//                  GetSymbol(Name,"",FileName,1);
						}
					}
					else
					{
						sprintf(FileStr, SC(203, "Symbol %s not found"), Name);
						MessageBoxUTF8(SCHWindow, FileStr, SC(38, "Error"), MB_APPLMODAL | MB_OK);
						break;
					}
				}

				EndDialog(Dialog, 1);
				return about;
			}

			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
			Help("add_component.htm", 0);
			break;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

int32 SelectCompDialog(int32 Comp1, int32 Comp2)
{
	int res, ok;

//  InitDialogTextLine=TextLine;
//  DialogWindowText=DialogText;
	DialogMode2 = Comp1 * 10000 + Comp2;
	OkToAddSymbol = 0;
	EscapeInsertSymbol = 0;

	if (!EditingSymbol)
	{
		res = DialogBox(SCHClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_SELECTCOMP), SCHWindow,
		              (DLGPROC) SelectCompDialog2);

		if (OkToAddSymbol)
			GetSymbol(NewSymbolName, NewSymbolDir, NewLibName, 1);
	}
	else
		res = 0;

	ok = 1;
	return res;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
