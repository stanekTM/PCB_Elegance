/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: inputgerber.c
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
#include "stdio.h"
#include "windows.h"
#include "memory.h"
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "calc.h"
#include "calc4.h"
#include "help.h"
#include "direct.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "pcb.h"
#include "files.h"
#include "files2.h"
#include "calcdef.h"
#include "draw3.h"
#include "select.h"
#include "insdel.h"
#include "dialogs.h"
#include "mainloop.h"
#include "resource.h"
#include "graphics.h"
#include "math.h"
#include "InputGerber.h"
#include "draw2.h"
#include "calc2.h"
#include "calc3.h"
#include "select3.h"
#include "nets.h"
#include "toets.h"
#include "menus.h"
#include "edit.h"
#include "dialogs.h"
#include "ctype.h"

#define  MAX_GERBER_LINE_LENGTH     65536
#define  MaxNrColums                15

typedef struct
{
	char GerberApertureFileName[MAX_LENGTH_STRING];
	int32 XDigits1;
	int32 XDigits2;
	int32 YDigits1;
	int32 YDigits2;
	int32 ZeroMode;
	int32 Incremental;
	int32 NumberFormat;
	int32 Invert;
	int32 SkipLines;
	int32 TotalLines;
	int32 MaxLineNr;
	int32 NrApertureDefs;
	int32 LineNrColumn;
	int32 DcodeColumn;
	int32 ApertureTypeColumn;
	int32 SizeXColumn;
	int32 SizeYColumn;
	int32 Units;
	int32 AperTureUnits;
	int32 Info;
	int32 GerberNumberMode;
	int32 GerberOutputMode;
	int32 AutoApertureGeneration;
	int32 ReverseRectangleXY;
	double ScaleFactor, Xoffset, Yoffset;
} InputGerberRecord;


typedef struct
{
	uint8 NrStrings, Column[MaxNrColums];
	float Values[MaxNrColums];
} ApertureLineRecord;

typedef char AperTureTextRecord[1600][160];

int32 GerberBufPos, GerberBufLength, GerberLineNr, GerberFileP, GerberFileP2, GerberLineNr;
int32 GerberBufSize = 128 * 1024;
int32 OwnNetNr;
int32 ActiveAperTureMacro, InvertedGerber;

char *LineBuf;
char LocalAperTureFile[MAX_LENGTH_STRING] = "";
char RetryStr[MAX_LENGTH_STRING];
char GerberFileName[MAX_LENGTH_STRING] = "";
uint8 *GerberBuf;
AperTureTextRecord *AperTureText;
ApertureLineRecord *ApertureLines;

InputGerberRecord TempGerberInfo;
ObjectRecord NewObject, *ApertureObject, DefaultApertureObject, DefaultApertureObject2;


extern HDC OutputDisplay;
extern int32 AreafillDrawMode, SelectColorMode;

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CheckString(LPSTR str, int32 mode)
{
	int32 Length, res, res2, res3, cnt, Value;
	float Value1, Value2;
	Length = strlen(str);

	switch (mode)
	{
	case 1:					// Check if dcode
		if ((Length >= 2) && (Length < 5))
		{
			if ((str[0] == 'd') || (str[0] == 'D'))
			{
				if ((isdigit(str[1])) && (isdigit(str[2])))
					return 1;
			}
		}

		return 0;
		break;

	case 2:					// Check if integer
		for (cnt = 0; cnt < Length; cnt++)
		{
			if (!isdigit(str[cnt]))
				return 0;
		}

		if (sscanf(str, "%d", &Value) == 1)
		{
			if (Value == 0)
				return 2;
		}

		return 1;

	case 3:					// Check if floating point
		Value1 = 0.0;
		Value2 = 0.0;
		res = sscanf(str, "%fx%f", &Value1, &Value2);
		res2 = sscanf(str, "%fX%f", &Value1, &Value2);
		res3 = sscanf(str, "%f", &Value1);

		if ((res == 2) || (res2 == 2))
		{
			res = 4;

			if (Value1 == 0)
				res |= 2;

			if (Value2 == 0)
				res |= 1;

			return res;
		}

		if ((res == 1) || (res2 == 1) || (res3 == 1))
		{
			res = 2;

			if (Value1 == 0)
				res |= 1;

			return res;
		}

		return 0;
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CountNrStrings(void *Buffer, int32 LengthBuffer, LPSTR SearchString, int32 mode)
{
	int32 cnt, cnt2, count;
	int32 LengthString = strlen(SearchString);
	LPSTR TextBuf;
	char FirstChar;
	char SearchString2[MAX_LENGTH_STRING];

	if (LengthString == 0)
		return 0;

	count = 0;
	TextBuf = (LPSTR) Buffer;

	if (mode == 0)
	{
		FirstChar = SearchString[0];
		cnt = 0;

		while (cnt < LengthBuffer - LengthString + 1)
		{
			if (TextBuf[cnt] == FirstChar)
			{
				cnt2 = 0;

				while ((cnt2 < LengthString) && (TextBuf[cnt + cnt2] == SearchString[cnt2]))
					cnt2++;

				if (cnt2 == LengthString)
					count++;
			}

			cnt++;
		}
	}
	else
	{
		strcpy(SearchString2, SearchString);
		strupr(SearchString2);
		cnt = 0;
		FirstChar = SearchString2[0];

		while (cnt < LengthBuffer - LengthString + 1)
		{
			if (toupper(TextBuf[cnt]) == FirstChar)
			{
				cnt2 = 0;

				while ((cnt2 < LengthString) && (toupper(TextBuf[cnt + cnt2]) == SearchString2[cnt2]))
					cnt2++;

				if (cnt2 == LengthString)
					count++;
			}

			cnt++;
		}
	}

	return count;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetSpecialValue(LPSTR str, float *Value, int32 mode)
{
	char str2[MAX_LENGTH_STRING];
	int32 lengte;

	if (sscanf(str, "%f", Value) == 0)
	{
		*Value = (float) -1.0;
		return -1;
	}

	lengte = strlen(str);

	if (lengte > 2)
	{
		strcpy(str2, (LPSTR) & str[lengte - 2]);

		if (stricmpOwn(str2, SC(548, "in")) == 0)
		{
			*Value *= 2540000.0;
			return 1;
		}

		if (stricmpOwn(str2, "mm") == 0)
		{
			*Value *= 100000.0;
			return 1;
		}

		if (lengte > 4)
		{
			strcpy(str2, (LPSTR) & str[lengte - 4]);

			if (stricmpOwn(str2, "thou") == 0)
			{
				*Value *= 2540.0;
				return 1;
			}
		}
	}

	return 0;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CheckFileIsGerber(LPSTR FileName)
{
	int32 fp, CheckLength, CountD01, CountD02, CountD03, result, Check1, Check2, Check3, Check4, Check5;

	AllocateSpecialMem(MEM_POINTS, GerberBufSize * 2, (void **) &GerberBuf);

	if ((fp = FileOpenReadOnlyUTF8(FileName)) < 0)
		return 0;

	FileRead(fp, GerberBuf, GerberBufSize * 2, &result);
	FileClose(fp);
	CheckLength = result;

	CountD01 = CountNrStrings(GerberBuf, CheckLength, "D01*", 0);
	CountD01 += CountNrStrings(GerberBuf, CheckLength, "D1*", 0);
	CountD02 = CountNrStrings(GerberBuf, CheckLength, "D02*", 0);
	CountD02 += CountNrStrings(GerberBuf, CheckLength, "D2*", 0);
	CountD03 = CountNrStrings(GerberBuf, CheckLength, "D03*", 0);
	CountD03 += CountNrStrings(GerberBuf, CheckLength, "D3*", 0);

	if (CountD01 + CountD02 + CountD03 > 3)
		return 1;

	Check1 = CountNrStrings(GerberBuf, CheckLength, "%ADD", 0);
	Check2 = CountNrStrings(GerberBuf, CheckLength, "%IP", 0);
	Check3 = CountNrStrings(GerberBuf, CheckLength, "%AM", 0);
	Check4 = CountNrStrings(GerberBuf, CheckLength, "%FSL", 0);
	Check5 = CountNrStrings(GerberBuf, CheckLength, "%MO", 0);

	if (Check1 + Check2 + Check3 + Check4 + Check5 > 2)
		return 1;

	return 0;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CheckFileIsAperTure(LPSTR FileName)
{
	int32 fp, cnt, result, CheckLength, CountRound, CountRounded, CountRectangle, CountRectangular, CountRect,
	      CountSquare, CountFlash, CountDCodes, CountDCode, CountShape, CountAperture, CountCircle, CountG04, CountHeader,
	      CountSection, CountEntities, CountEndSections, CountTables;

	char str[MAX_LENGTH_STRING];

	AllocateSpecialMem(MEM_POINTS, GerberBufSize * 2, (void **) &GerberBuf);

	if ((fp = FileOpenReadOnlyUTF8(FileName)) < 0)
		return 0;

	FileRead(fp, GerberBuf, GerberBufSize, &result);
	FileClose(fp);
	CheckLength = result;


	CountG04 = CountNrStrings(GerberBuf, CheckLength, "G04", 1);
	CountRound = CountNrStrings(GerberBuf, CheckLength, SC(442, "round"), 1);
	CountRounded = CountNrStrings(GerberBuf, CheckLength, SC(588, "rounded"), 1);
	CountRectangle = CountNrStrings(GerberBuf, CheckLength, SC(446, "rectangle"), 1);
	CountRectangular = CountNrStrings(GerberBuf, CheckLength, SC(447, "rectangular"), 1);
	CountRect = CountNrStrings(GerberBuf, CheckLength, SC(448, "rect"), 1);
	CountSquare = CountNrStrings(GerberBuf, CheckLength, SC(450, "square"), 1);
	CountCircle = CountNrStrings(GerberBuf, CheckLength, SC(451, "circle"), 1);
	CountFlash = CountNrStrings(GerberBuf, CheckLength, SC(453, "flash"), 1);
	CountShape = CountNrStrings(GerberBuf, CheckLength, SC(454, "shape"), 1);
	CountDCode = CountNrStrings(GerberBuf, CheckLength, SC(455, "dcode"), 1);
	CountDCode += CountNrStrings(GerberBuf, CheckLength, SC(456, "d-code"), 1);
	CountAperture = CountNrStrings(GerberBuf, CheckLength, SC(457, "aperture"), 1);

// DXF stuff
	CountHeader = CountNrStrings(GerberBuf, CheckLength, "HEADER", 0);
	CountSection = CountNrStrings(GerberBuf, CheckLength, "SECTION", 0);
	CountEntities = CountNrStrings(GerberBuf, CheckLength, "ENTITIES", 0);
	CountTables = CountNrStrings(GerberBuf, CheckLength, "TABLES", 0);
	CountEndSections = CountNrStrings(GerberBuf, CheckLength, "ENDSEC", 0);
// End DXF stuff

	if (CountHeader + CountSection + CountEntities + CountTables + CountEndSections > 1)
		return 0;

	CountDCodes = 0;

	for (cnt = 10; cnt < 400; cnt++)
	{
		sprintf(str, "D%d", cnt);
		CountDCodes += CountNrStrings(GerberBuf, CheckLength, str, 0);
	}

	if (CountDCodes > 3)
	{
		if (CountG04 == 0)
			return 1;
	}

	if (CountDCode > 0)
	{
		if (CountG04 == 0)
			return 1;
	}

	if (CountAperture > 0)
	{
		if (CountG04 == 0)
			return 1;
	}

	if (CountRound + CountRounded + CountRectangle + CountRectangular + CountRect + CountSquare + CountFlash +
	        CountShape + CountCircle > 5)
	{
		if (CountG04 == 0)
			return 1;
	}


	return 0;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CALLBACK RetryDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;

	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
		SetWindowTextUTF8(Dialog, SC(1, "Message"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));

		SendDlgItemMessageOwn(Dialog, IDC_EDIT1, WM_SETTEXT, 0, (LPARAM) (LPSTR) RetryStr);
		return about;

	case WM_MOVE:
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDOK:
			EndDialog(Dialog, 2);
			return about;

		case IDCANCEL:
			EndDialog(Dialog, 1);
			return about;
		}

		break;
	}

	about = 0;
	return about;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void AddAperTure(int32 ObjectType, int32 DCode, double Value1, double Value2)
{
	ObjectRecord *ApertureObject;

	if (NrObjects4 >= MaxNrObjects4 - 1)
	{
		if (AllocateMemObjects4(MaxNrObjects4 + 1024) == -1)
			return;
	}

	ApertureObject = &((*Objects4)[NrObjects4]);
	memset(ApertureObject, 0, sizeof(ObjectRecord));
	ApertureObject->ObjectType = ObjectType;
	ApertureObject->Info2 = DCode;
	ApertureObject->x1 = Value1;
	ApertureObject->y1 = Value2;
	NrObjects4++;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void AddGerberObject(ObjectRecord * NewObject, int32 mode)
{
	ObjectRecord *Object;

	if (NrObjects6 >= MaxNrObjects6 - 1)
	{
		if (AllocateMemObjects6(MaxNrObjects6 + 1024) == -1)
			return;
	}

	Object = &((*Objects6)[NrObjects6]);
	NrObjects6++;
	memmove(Object, NewObject, sizeof(ObjectRecord));
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 GetAperTureNr(int32 DCode)
{
	int32 cnt;
	ObjectRecord *ApertureObject;

	for (cnt = 0; cnt < NrObjects4; cnt++)
	{
		ApertureObject = &((*Objects4)[cnt]);

		if (ApertureObject->Info2 == DCode)
			return cnt;
	}

	return -1;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************


int32 ReadApertures(int32 mode)
{
	int32 cnt, cnt2, cnt3, NrStrings, res, ok, Length, ExtraAdd, ApertureType, DCode;
	char LineBuf[MAX_LENGTH_STRING], str1[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING], Strings[32][MAX_LENGTH_STRING];
	float ValueX, ValueY, hulp;
	int32 Error, OkToUseUnits;

	if ((TempGerberInfo.DcodeColumn == -1) || (TempGerberInfo.SizeXColumn == -1))
		return -1;

	MessageBufPos = 0;
	cnt2 = 0;
	cnt3 = 0;
	TempGerberInfo.NrApertureDefs = 0;

	for (cnt = 0; cnt < TempGerberInfo.TotalLines; cnt++)
	{
		strcpy(LineBuf, (LPSTR) & (*AperTureText)[cnt]);
		Error = 0;

		if ((cnt >= TempGerberInfo.SkipLines) && (cnt < TempGerberInfo.MaxLineNr))
		{
			Length = strlen(LineBuf);
			strcpy(str1, LineBuf);
			GetString(str1, str2);

			if ((Length > 0) && (str2[0] != 0)
			        && ((isdigit(LineBuf[0])) || (isalpha(LineBuf[0])) || (LineBuf[0] == ' ')))
			{
				strcpy(str1, LineBuf);
				NrStrings = 0;
				memset(&Strings, 0, sizeof(Strings));

				while ((str1[0] != 0) && (NrStrings < MaxNrColums))
				{
					if (NrStrings < MaxNrColums)
						GetString2a(str1, Strings[NrStrings]);

					NrStrings++;
				}

				ok = 1;

				if (((ApertureLines[cnt3].Column[TempGerberInfo.DcodeColumn] == 1)
				        || ((ApertureLines[cnt3].Column[TempGerberInfo.DcodeColumn] & 2) == 2)))
				{
					ok = 1;
					ExtraAdd = 0;
					DCode = -1;
					ValueX = -1.0;
					ValueY = -1.0;

					if (ApertureLines[cnt3].Column[TempGerberInfo.DcodeColumn] == 1)
					{
						if ((Strings[TempGerberInfo.DcodeColumn][0] == 'd')
						        || (Strings[TempGerberInfo.DcodeColumn][0] == 'D'))
							sscanf((LPSTR) & Strings[TempGerberInfo.DcodeColumn][1], "%d", &DCode);
						else
							sscanf(Strings[TempGerberInfo.DcodeColumn], "%d", &DCode);
					}
					else
						sscanf(Strings[TempGerberInfo.DcodeColumn], "%d", &DCode);

					if (DCode == 25)
						ok = 1;

					OkToUseUnits = 1;

					if ((ApertureLines[cnt3].Column[TempGerberInfo.SizeXColumn] & 8) == 8)
					{
						res = GetSpecialValue(Strings[TempGerberInfo.SizeXColumn], &ValueX, 0);

						if (res == 1)
							OkToUseUnits = 0;

//            if (sscanf(Strings[TempGerberInfo.SizeXColumn],"%f",&ValueX)==0) ValueX=-1.0;
					}
					else
					{
						if ((ApertureLines[cnt3].Column[TempGerberInfo.SizeXColumn] & 16) == 0)
						{
							if ((ApertureLines[cnt3].Column[TempGerberInfo.SizeXColumn + 1] & 8) == 8)
							{
								res = GetSpecialValue(Strings[TempGerberInfo.SizeXColumn + 1], &ValueX, 0);

								if (res == 1)
									OkToUseUnits = 0;

//                if (sscanf(Strings[TempGerberInfo.SizeXColumn+1],"%f",&ValueX)==0) ValueX=-1.0;
								ExtraAdd++;
							}
						}
						else
						{
							if (sscanf(Strings[TempGerberInfo.SizeXColumn], "%fx%f", &ValueX, &ValueY) == 0)
							{
								ValueX = -1.0;
								ValueY = -1.0;
							}

							if (ValueY < 0.0)
							{
								if (sscanf(Strings[TempGerberInfo.SizeXColumn], "%fX%f", &ValueX, &ValueY) != 2)
								{
									ValueX = -1.0;
									ValueY = -1.0;
								}
							}
						}
					}

					if (OkToUseUnits)
						ValueX = (float) UnitsConvert(TempGerberInfo.AperTureUnits, ValueX);

					ApertureType = 0;

					if (TempGerberInfo.ApertureTypeColumn != -1)
						ApertureType = ApertureLines[cnt3].Column[TempGerberInfo.ApertureTypeColumn];

					if ((DCode >= 10) && (DCode < 1000))
					{
						switch (ApertureType)
						{
						case 64:	// Rectangle
						case 96:	// Oblong
							OkToUseUnits = 1;

							if (TempGerberInfo.SizeYColumn == -1)
							{
								if (ValueY < 0.0)
								{
									if ((ApertureLines[cnt3].Column[TempGerberInfo.SizeXColumn + ExtraAdd + 1] & 12) ==
									        8)
									{
										res =
										    GetSpecialValue(Strings[TempGerberInfo.SizeXColumn + ExtraAdd + 1], &ValueY,
										                    0);

										if (res == 1)
											OkToUseUnits = 0;

//                      if (sscanf(Strings[TempGerberInfo.SizeXColumn+ExtraAdd+1],"%f",&ValueY)==0) ValueY=-1.0;
									}
									else
									{
										if ((ApertureLines[cnt3].
										        Column[TempGerberInfo.SizeXColumn + ExtraAdd + 2] & 12) == 8)
										{
											res =
											    GetSpecialValue(Strings[TempGerberInfo.SizeXColumn + ExtraAdd + 2],
											                    &ValueY, 0);

											if (res == 1)
												OkToUseUnits = 0;

//                        if (sscanf(Strings[TempGerberInfo.SizeXColumn+ExtraAdd+2],"%f",&ValueY)==0) ValueY=-1.0;
										}
									}
								}
							}
							else
							{
								res = GetSpecialValue(Strings[TempGerberInfo.SizeYColumn], &ValueY, 0);

								if (res == -1)
									res = GetSpecialValue(Strings[TempGerberInfo.SizeYColumn + 1], &ValueY, 0);

								if (res == 1)
									OkToUseUnits = 0;

//                  if (sscanf(Strings[TempGerberInfo.SizeYColumn],"%f",&ValueY)==0) ValueY=-1.0;
							}

							if (ValueY < 0.0)
								ValueY = 0.0;

							if (OkToUseUnits)
								ValueY = (float) UnitsConvert(TempGerberInfo.AperTureUnits, ValueY);

							switch (ApertureType)
							{
							case 64:	// Rectangle
								if (ValueX < 0.0)
								{
									Error = 1;
									ValueX = 0.0;
								}

								if (ValueY < 0.0)
								{
									Error = 1;
									ValueY = 0.0;
								}

								if (TempGerberInfo.ReverseRectangleXY == 1)
								{
									hulp = ValueX;
									ValueX = ValueY;
									ValueY = hulp;
								}

								AddAperTure(PIN_SMD_RECT, DCode, ValueX, ValueY);
								break;

							case 96:	// Oblong
								if (TempGerberInfo.ReverseRectangleXY == 1)
								{
									hulp = ValueX;
									ValueX = ValueY;
									ValueY = hulp;
								}

								if (ValueX > ValueY)
									AddAperTure(PIN_LINE_HOR, DCode, ValueX, ValueY);
								else
									AddAperTure(PIN_LINE_VER, DCode, ValueX, ValueY);

								if (ValueX < 0.0)
									Error = 1;

								if (ValueY < 0.0)
									Error = 1;

								break;
							}

							break;

						case 128:	// square
							if (ValueX < 0.0)
							{
								Error = 1;
								ValueX = 0.0;
							}

							AddAperTure(PIN_SMD_RECT, DCode, ValueX, ValueX);
							break;

						case 0:	// Round
						case 32:
						default:
							if (ValueX < 0.0)
							{
								Error = 1;
								ValueX = 0.0;
							}

							OkToUseUnits = 1;

							if (TempGerberInfo.SizeYColumn == -1)
							{
								if (ValueY < 0.0)
								{
									if ((ApertureLines[cnt3].Column[TempGerberInfo.SizeXColumn + ExtraAdd + 1] & 12) ==
									        8)
									{
										res =
										    GetSpecialValue(Strings[TempGerberInfo.SizeXColumn + ExtraAdd + 1], &ValueY,
										                    0);

										if (res == 1)
											OkToUseUnits = 0;

//                      if (sscanf(Strings[TempGerberInfo.SizeXColumn+ExtraAdd+1],"%f",&ValueY)==0) ValueY=-1.0;
									}
									else
									{
										if ((ApertureLines[cnt3].
										        Column[TempGerberInfo.SizeXColumn + ExtraAdd + 2] & 12) == 8)
										{
											res =
											    GetSpecialValue(Strings[TempGerberInfo.SizeXColumn + ExtraAdd + 2],
											                    &ValueY, 0);

											if (res == 1)
												OkToUseUnits = 0;

//                        if (sscanf(Strings[TempGerberInfo.SizeXColumn+ExtraAdd+2],"%f",&ValueY)==0) ValueY=-1.0;
										}
									}
								}
							}
							else
							{
								res = GetSpecialValue(Strings[TempGerberInfo.SizeYColumn], &ValueY, 0);

								if (res == 1)
									OkToUseUnits = 0;

//                  if (sscanf(Strings[TempGerberInfo.SizeYColumn],"%f",&ValueY)==0) ValueY=-1.0;
							}

							if (ValueY < 0.0)
								ValueY = 0.0;

							if (OkToUseUnits)
								ValueY = (float) UnitsConvert(TempGerberInfo.AperTureUnits, ValueY);

							if ((ValueY == 0.0) || (TempGerberInfo.SizeYColumn == -1) || (InRange(ValueX, ValueY)))
								AddAperTure(PIN_SMD_ROUND, DCode, ValueX, 0.0);
							else
							{
								if (TempGerberInfo.ReverseRectangleXY == 1)
								{
									hulp = ValueX;
									ValueX = ValueY;
									ValueY = hulp;
								}

								if (ValueX > ValueY)
									AddAperTure(PIN_LINE_HOR, DCode, ValueX, ValueY);
								else
									AddAperTure(PIN_LINE_VER, DCode, ValueX, ValueY);

								if (ValueX < 0.0)
									Error = 1;

								if (ValueY < 0.0)
									Error = 1;
							}

							break;
						}
					}
					else
						Error = 1;

					if (Error)
						AddToMessageBuf(LineBuf);
					else
						TempGerberInfo.NrApertureDefs++;
				}

				cnt2++;
			}
		}

		if (TextLineNr == TempGerberInfo.TotalLines)
			ok = 1;

		cnt3++;
	}

//  TextFileClose(fp);
	if (MessageBufPos > 0)
	{
		MessageDialog(SC(380, "The following apertures could not be decoded"), 0, 0);
		DeAllocateMemMessageBuf();
	}

// NrAperTures
	sprintf(RetryStr, SC(381, "%i apertures have been found"), TempGerberInfo.NrApertureDefs);
	res = OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_READ_RETRY), PCBWindow, (DLGPROC) RetryDialog2);

	if (res == 1)
		return -1;

	ok = 1;
	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************


int32 InitAperturesText(HWND Dialog)
{
	int32 cnt, cnt2;
	char LineBuf[MAX_LENGTH_STRING], str[11][MAX_LENGTH_STRING], NewStr[MAX_LENGTH_STRING];

	SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_RESETCONTENT, 0, 0);
	SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);

	for (cnt = 0; cnt < TempGerberInfo.MaxLineNr; cnt++)
	{
		strcpy(LineBuf, (LPSTR) & (*AperTureText)[cnt]);

		if (cnt < TempGerberInfo.SkipLines)
		{
			if (cnt > TempGerberInfo.SkipLines - 4)
			{
				sprintf(NewStr, "%i\t%s", cnt + 1, LineBuf);
				SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, 0, (LPARAM) NewStr);
			}
		}
		else
		{
			memset(&str, 0, sizeof(str));

			for (cnt2 = 0; cnt2 < 11; cnt2++)
			{
				if (LineBuf[0] != 0)
				{
					GetString2a(LineBuf, str[cnt2]);
					str[cnt2][9] = 0;
				}
			}

			sprintf(NewStr, "%i", cnt + 1);

			for (cnt2 = 0; cnt2 < 11; cnt2++)
			{
				strcat(NewStr, "\t");
				strcat(NewStr, str[cnt2]);
			}

			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) NewStr);
		}
	}

	return 0;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 SetApertureColumns(HWND Dialog)
{
	char str[MAX_LENGTH_STRING];

	sprintf(str, "%i", TempGerberInfo.SkipLines);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	sprintf(str, "%i", TempGerberInfo.DcodeColumn + 1);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	if (TempGerberInfo.ApertureTypeColumn >= 0)
		sprintf(str, "%i", TempGerberInfo.ApertureTypeColumn + 1);
	else
		sprintf(str, "*");

	SendDlgItemMessageOwn(Dialog, IDC_COMBO6, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	sprintf(str, "%i", TempGerberInfo.SizeXColumn + 1);
	SendDlgItemMessageOwn(Dialog, IDC_COMBO7, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);

	if (TempGerberInfo.SizeYColumn >= 0)
		sprintf(str, "%i", TempGerberInfo.SizeYColumn + 1);
	else
		sprintf(str, "*");

	SendDlgItemMessageOwn(Dialog, IDC_COMBO8, CB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);
	return 0;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 InitAperturesColumn(HWND Dialog)
{
	int32 cnt;
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];

	SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_RESETCONTENT, 0, 0);
	sprintf(str, SC(462, "Line"));

	for (cnt = 1; cnt < 12; cnt++)
	{
		if (TempGerberInfo.DcodeColumn == cnt - 1)
			strcat(str, SC(463, "\tD code"));
		else
		{
			if (TempGerberInfo.ApertureTypeColumn == cnt - 1)
				strcat(str, SC(464, "\tShape"));
			else
			{
				if (TempGerberInfo.SizeXColumn == cnt - 1)
					strcat(str, SC(465, "\tSize X"));
				else
				{
					if (TempGerberInfo.SizeYColumn == cnt - 1)
						strcat(str, SC(466, "\tSize Y"));
					else
					{
						sprintf(str2, "\t%i", cnt);
						strcat(str, str2);
					}
				}
			}
		}
	}

	SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_ADDSTRING, 0, (LPARAM) str);
	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CALLBACK ApertureDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 cnt, ok, res, res2, hulp, TabStops[20];
	char str[MAX_LENGTH_STRING];


	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
// ************************************************************************************************
		SetWindowTextUTF8(Dialog, SC(467, "Load apertures"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(468, "Aperture file"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(469, "Info lines"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(470, "Columns"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(471, "Apertures"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC5, SC(469, "Info lines"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC6, SC(472, "Skip first"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC7, SC(473, "lines"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC8, SC(470, "Columns"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC9, SC(474, "DCode"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC10, SC(475, "Shape"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC11, SC(476, "X size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC12, SC(477, "Y size"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC13, SC(814, "Units"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC14, SC(814, "Units"));

		SetDialogItemTextUTF8(Dialog, IDC_CHECK1, SC(478, "Reverse X and Y when rectangle or oblong"));

		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDOK, SC(479, "Read apertures"));
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, SC(480, "thou/mm/inch"));
		SetDialogItemTextUTF8(Dialog, IDD_HINT, SC(481, "Hint by program"));
// ************************************************************************************************
		TabStops[0] = 20;

		for (cnt = 1; cnt < 12; cnt++)
			TabStops[cnt] = TabStops[0] + 35 * cnt;

		SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_SETTABSTOPS, 12, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessageOwn(Dialog, IDC_LIST3, LB_SETTABSTOPS, 12, (LPARAM) (LPINT) & TabStops);
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SETTABSTOPS, 1, (LPARAM) (LPINT) & TabStops);
		InitAperturesText(Dialog);

		for (cnt = 0; cnt < TempGerberInfo.TotalLines; cnt++)
		{
			sprintf(str, "%i", cnt);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM) str);
		}

		sprintf(str, "*");
		SendDlgItemMessageOwn(Dialog, IDC_COMBO6, CB_ADDSTRING, 0, (LPARAM) str);
		SendDlgItemMessageOwn(Dialog, IDC_COMBO8, CB_ADDSTRING, 0, (LPARAM) str);

		for (cnt = 0; cnt < 11; cnt++)
		{
			sprintf(str, "%i", cnt + 1);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM) str);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO6, CB_ADDSTRING, 0, (LPARAM) str);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO7, CB_ADDSTRING, 0, (LPARAM) str);
			SendDlgItemMessageOwn(Dialog, IDC_COMBO8, CB_ADDSTRING, 0, (LPARAM) str);
		}

		SetApertureColumns(Dialog);

		InitAperturesColumn(Dialog);
		TempGerberInfo.AperTureUnits = SetNextUnits(TempGerberInfo.AperTureUnits, 16 + 0);	// thou/mm/inch
		SetUnitText(Dialog, IDC_EDIT2, TempGerberInfo.AperTureUnits);
		SendDlgItemMessageOwn(Dialog, IDC_EDIT3, WM_SETTEXT, 0, (LPARAM) (LPSTR) TempGerberInfo.GerberApertureFileName);
		return about;

	case WM_MOVE:
		break;

	case WM_SIZE:
		res = 1;
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			TempGerberInfo.AperTureUnits = SetNextUnits(TempGerberInfo.AperTureUnits, 0);	// thou/mm/inch
			SetUnitText(Dialog, IDC_EDIT2, TempGerberInfo.AperTureUnits);
			break;

		case IDOK:
			TempGerberInfo.ReverseRectangleXY = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_CHECK1, BM_GETCHECK, 0, 0) == 1)
				TempGerberInfo.ReverseRectangleXY = 1;

			if (ReadApertures(0) == 0)
				EndDialog(Dialog, 1);

			break;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDD_HINT:
			SetApertureColumns(Dialog);
			InitAperturesText(Dialog);
			InitAperturesColumn(Dialog);
			SetUnitText(Dialog, IDC_EDIT2, TempGerberInfo.AperTureUnits);
			break;

		case IDHELP:
//          Help(IDH_Load_default_apertures,0);
			break;

		case IDC_COMBO2:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO2, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					TempGerberInfo.SkipLines = res;
					SetApertureColumns(Dialog);
					InitAperturesText(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO1:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO1, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					TempGerberInfo.DcodeColumn = res;
					SetApertureColumns(Dialog);
					InitAperturesColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO6:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO6, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					if (res == 0)
						TempGerberInfo.ApertureTypeColumn = -1;
					else
						TempGerberInfo.ApertureTypeColumn = res - 1;

					SetApertureColumns(Dialog);
					InitAperturesColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO7:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO7, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					TempGerberInfo.SizeXColumn = res;
					SetApertureColumns(Dialog);
					InitAperturesColumn(Dialog);
				}

				ok = 1;
			}

			break;

		case IDC_COMBO8:
			res2 = HIWORD(WParam);

			if (res2 == 9)
			{
				res = SendDlgItemMessageOwn(Dialog, IDC_COMBO8, CB_GETCURSEL, 0, 0);

				if (res >= 0)
				{
					if (res == 0)
						TempGerberInfo.SizeYColumn = -1;
					else
						TempGerberInfo.SizeYColumn = res - 1;

					SetApertureColumns(Dialog);
					InitAperturesColumn(Dialog);
				}

				ok = 1;
			}

			break;
		}

		break;

	case WM_PARENTNOTIFY:
		hulp = 1;
		break;
	}

	about = 0;
	return about;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 LoadApertureFile(LPSTR AperTureFile, int32 mode)
{
	int32 fp, cnt, cnt2, Length, Dcode, NrStrings, res1, res2, res3, res, NrAperTextLines;
	char LineBuf[512], str1[MAX_LENGTH_STRING], Strings[MaxNrColums][MAX_LENGTH_STRING];
	double LastX, LastY, MinSizeX, MaxSizeX;
	int32 Stop, Stop2;
#ifdef _DEBUG
	char str2[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING];
#endif

	MaxSizeX = 0.0;

	LastX = 0.0;
	LastY = 0.0;

	AllocateSpecialMem(MEM_APERTURE_LINES, 1024 * sizeof(ApertureLineRecord), (void **) &ApertureLines);
	AllocateSpecialMem(MEM_APERTURE_TEXT, sizeof(AperTureTextRecord), (void **) &AperTureText);
	memset(AperTureText, 0, sizeof(AperTureTextRecord));

	NrAperTextLines = 0;
	memset(ApertureLines, 0, 1024 * sizeof(ApertureLineRecord));

	if ((fp = TextFileOpenUTF8(AperTureFile)) < 0)
		return -1;

	while ((Length = ReadLn(fp, LineBuf)) >= 0)
	{
		for (cnt = 0; cnt < Length; cnt++)
		{
			if ((LineBuf[cnt] < 32) || (LineBuf[cnt] == '|') || (LineBuf[cnt] == '_') || (LineBuf[cnt] == '(')
			        || (LineBuf[cnt] == ')') || (LineBuf[cnt] == '*') || (LineBuf[cnt] == '=') || (LineBuf[cnt] > 125))
				LineBuf[cnt] = ' ';
		}

		LineBuf[Length + 1] = 0;
		LineBuf[Length + 2] = 0;
		LineBuf[160 - 1] = 0;

		if (NrAperTextLines < 1600)
			strcpy((LPSTR) & ((*AperTureText)[NrAperTextLines]), LineBuf);

		if ((Length > 0) && ((isdigit(LineBuf[0])) || (isalpha(LineBuf[0])) || (LineBuf[0] == ' ')))
		{
			strcpy(str1, LineBuf);
			NrStrings = 0;
			memset(&Strings, 0, sizeof(Strings));

			while ((str1[0] != 0) && (NrStrings < MaxNrColums))
			{
				if (NrStrings < MaxNrColums)
					GetString2a(str1, Strings[NrStrings]);

				NrStrings++;
			}

			for (cnt = 0; cnt < min(MaxNrColums, NrStrings); cnt++)
			{
				res1 = CheckString(Strings[cnt], 1);

				if (res1 == 1)
				{
					ApertureLines[NrAperTextLines].Column[cnt] = 1;	// Dcode
				}
				else
				{
					res2 = CheckString(Strings[cnt], 2);

					if (res2 == 1)
					{
						ApertureLines[NrAperTextLines].Column[cnt] |= 2;	// integer
					}

					res3 = CheckString(Strings[cnt], 3);

					switch (res3)
					{
					case 2:
						ApertureLines[NrAperTextLines].Column[cnt] |= 8;	// floating point
						break;

					case 3:
						ApertureLines[NrAperTextLines].Column[cnt] |= 12;	// floating point       x == 0
						break;

					case 4:
						ApertureLines[NrAperTextLines].Column[cnt] |= 16;	// two floating points
						break;

					case 5:
						ApertureLines[NrAperTextLines].Column[cnt] |= 16 + 4;	// two floating points  x != 0 , y == 0
						break;

					case 6:
						ApertureLines[NrAperTextLines].Column[cnt] |= 16 + 8;	// two floating points  x == 0 , y != 0
						break;

					case 7:
						ApertureLines[NrAperTextLines].Column[cnt] |= 16 + 8 + 4;	// two floating points  x == 0 , y == 0
						break;
					}

					if ((res3 >= 2) && (res3 < 8))
						sscanf(Strings[cnt], "%f", &ApertureLines[NrAperTextLines].Values[cnt]);

					if (res1 + res2 + res3 == 0)
					{
						if (stricmpOwn(Strings[cnt], "circle") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "kreis") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "c") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "line") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "linie") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "l") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "round") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "rund") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "rd") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "draw") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "octagon") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "rnd") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "r") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "rounded") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle
						}

						if (stricmpOwn(Strings[cnt], "donut") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 32;	// aperture type circle with hole
						}

						if (stricmpOwn(Strings[cnt], "rect") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 64;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "rechteck") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 64;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "rechteckig") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 64;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "rec") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 64;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "rectangle") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 64;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "rectangular") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 64;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "oblong") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 96;	// aperture type oblong
						}

						if (stricmpOwn(Strings[cnt], "obround") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 96;	// aperture type oblong
						}

						if (stricmpOwn(Strings[cnt], "o") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 96;	// aperture type oblong
						}

						if (stricmpOwn(Strings[cnt], "oval") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 96;	// aperture type oblong
						}

						if (stricmpOwn(Strings[cnt], "square") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 128;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "quadrat") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 128;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "sq") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 128;	// aperture type rectangle
						}

						if (stricmpOwn(Strings[cnt], "sqr") == 0)
						{
							ApertureLines[NrAperTextLines].Column[cnt] |= 128;	// aperture type rectangle
						}
					}
				}
			}

			ApertureLines[NrAperTextLines].NrStrings = (uint8) NrStrings;
		}

		NrAperTextLines++;
	}

	TextFileClose(fp);

// ************************************************************************************************
// ************************************************************************************************

	Dcode = -1;

	for (cnt = 0; cnt < NrAperTextLines; cnt++)
	{
		for (cnt2 = 0; cnt2 < ApertureLines[cnt].NrStrings; cnt2++)
		{
			if ((Dcode == -1) && ((ApertureLines[cnt].Column[cnt2] & 1) == 1))
				Dcode = cnt2;
		}
	}

	if (Dcode == -1)
	{
		cnt = 0;
		Stop = 0;

		while ((!Stop) && (cnt < NrAperTextLines))
		{
			cnt2 = 0;

			while ((cnt2 < ApertureLines[cnt].NrStrings) && ((ApertureLines[cnt].Column[cnt2] & (128 + 64 + 32)) == 0))
				cnt2++;

			if (cnt2 < ApertureLines[cnt].NrStrings)
			{
				res = 1;

				if ((ApertureLines[cnt].Column[0] & 2) == 2)
				{
					ApertureLines[cnt].Column[0] = 1;

					if ((ApertureLines[cnt].Column[1] & 2) == 2)
					{
						ApertureLines[cnt].Column[0] = 2;
						ApertureLines[cnt].Column[1] = 1;
					}
				}
			}

			cnt++;
		}

		res = 1;
	}

#ifdef _DEBUG

	for (cnt = 0; cnt < NrAperTextLines; cnt++)
	{
		str[0] = 0;

		for (cnt2 = 0; cnt2 < 12; cnt2++)
		{
			sprintf(str2, "%4d", ApertureLines[cnt].Column[cnt2]);
			strcat(str, str2);
		}

		strcat(str, "\n");
		OutputDebugString(str);
	}

#endif

	cnt = 0;

	while ((cnt < NrAperTextLines) && (ApertureLines[cnt].Column[0] != 1) && (ApertureLines[cnt].Column[1] != 1)
	        && (ApertureLines[cnt].Column[2] != 1) && (ApertureLines[cnt].Column[3] != 1)
	        && (ApertureLines[cnt].Column[4] != 1) && (ApertureLines[cnt].Column[5] != 1)
	        && (ApertureLines[cnt].Column[6] != 1) && (ApertureLines[cnt].Column[7] != 1)
	        && (ApertureLines[cnt].Column[8] != 1) && (ApertureLines[cnt].Column[9] != 1)
	        && (ApertureLines[cnt].Column[10] != 1))
		cnt++;

	if (cnt == NrAperTextLines)
		cnt = 0;

//  return -2;
	TempGerberInfo.SkipLines = cnt;
	cnt = NrAperTextLines - 1;
	TempGerberInfo.MaxLineNr = NrAperTextLines;
	TempGerberInfo.MaxLineNr = NrAperTextLines;
	TempGerberInfo.TotalLines = NrAperTextLines;
	TempGerberInfo.TotalLines = NrAperTextLines;

	while ((cnt > TempGerberInfo.SkipLines) && (ApertureLines[cnt].Column[0] != 1)
	        && (ApertureLines[cnt].Column[1] != 1) && (ApertureLines[cnt].Column[2] != 1)
	        && (ApertureLines[cnt].Column[3] != 1) && (ApertureLines[cnt].Column[4] != 1)
	        && (ApertureLines[cnt].Column[5] != 1) && (ApertureLines[cnt].Column[6] != 1)
	        && (ApertureLines[cnt].Column[7] != 1) && (ApertureLines[cnt].Column[8] != 1)
	        && (ApertureLines[cnt].Column[9] != 1) && (ApertureLines[cnt].Column[10] != 1))
		cnt--;

	TempGerberInfo.SkipLines = 0;
	TempGerberInfo.DcodeColumn = 0;
	TempGerberInfo.ApertureTypeColumn = 1;
	TempGerberInfo.SizeXColumn = 2;
	TempGerberInfo.SizeYColumn = 3;
	TempGerberInfo.AperTureUnits = 1;

//  TempGerberInfo.SkipLines=0;
	TempGerberInfo.MaxLineNr = cnt + 1;
	TempGerberInfo.LineNrColumn = -1;
	TempGerberInfo.DcodeColumn = -1;
	TempGerberInfo.ApertureTypeColumn = -1;
	TempGerberInfo.SizeXColumn = -1;
	TempGerberInfo.SizeYColumn = -1;

// ************************************************************************************************
	for (cnt = 0; cnt < ApertureLines[TempGerberInfo.SkipLines].NrStrings; cnt++)
	{
		if ((ApertureLines[TempGerberInfo.SkipLines].Column[cnt] & 1) == 1)
		{
			if (TempGerberInfo.DcodeColumn == -1)
				TempGerberInfo.DcodeColumn = cnt;
		}

		if ((ApertureLines[TempGerberInfo.SkipLines].Column[cnt] & (128 + 64 + 32)) != 0)
		{
			if (TempGerberInfo.ApertureTypeColumn == -1)
				TempGerberInfo.ApertureTypeColumn = cnt;
		}
	}


	if (TempGerberInfo.DcodeColumn == -1)
	{
		if ((ApertureLines[TempGerberInfo.SkipLines].Column[0] == 10)
		        && (ApertureLines[TempGerberInfo.SkipLines].Column[1] == 10))
		{
			cnt = 1;
			cnt2 = TempGerberInfo.SkipLines;
			Stop2 = 0;

			while ((cnt2 < TempGerberInfo.MaxLineNr) && (!Stop2))
			{
				if ((ApertureLines[cnt2].Column[cnt] & 2) == 0)
					Stop2 = 1;
				else
					cnt2++;
			}

			if (!Stop2)
			{
				TempGerberInfo.DcodeColumn = cnt;
				Stop = 1;
				TempGerberInfo.LineNrColumn = 0;
			}

			if (TempGerberInfo.DcodeColumn == -1)
			{
				cnt = 0;
				cnt2 = TempGerberInfo.SkipLines;
				Stop2 = 0;

				while ((cnt2 < TempGerberInfo.MaxLineNr) && (!Stop2))
				{
					if ((ApertureLines[cnt2].Column[cnt] & 2) == 0)
						Stop2 = 1;
					else
						cnt2++;
				}

				if (!Stop2)
				{
					TempGerberInfo.DcodeColumn = cnt;
					Stop = 1;
				}
			}
		}
		else
		{
			Stop = 0;
			cnt = 0;

			while ((cnt < ApertureLines[TempGerberInfo.SkipLines].NrStrings) && (!Stop))
			{
				if ((ApertureLines[TempGerberInfo.SkipLines].Column[cnt] & 2) == 2)
				{
					cnt2 = TempGerberInfo.SkipLines;
					Stop2 = 0;

					while ((cnt2 < TempGerberInfo.MaxLineNr) && (!Stop2))
					{
						if ((ApertureLines[cnt2].Column[cnt] & 2) == 0)
							Stop2 = 1;
						else
							cnt2++;
					}

					if (!Stop2)
					{
						TempGerberInfo.DcodeColumn = cnt;
						Stop = 1;
					}
				}

				if (!Stop)
					cnt++;
			}
		}
	}

// ************************************************************************************************
	cnt = 0;

	if (TempGerberInfo.DcodeColumn != -1)
		cnt = TempGerberInfo.DcodeColumn + 1;

	while ((cnt < ApertureLines[TempGerberInfo.SkipLines].NrStrings)
	        && ((ApertureLines[TempGerberInfo.SkipLines].Column[cnt] & (16 + 8)) == 0))
		cnt++;

	if (cnt < ApertureLines[TempGerberInfo.SkipLines].NrStrings)
	{
		if ((TempGerberInfo.DcodeColumn != cnt) && (TempGerberInfo.ApertureTypeColumn != cnt))
			TempGerberInfo.SizeXColumn = cnt;
	}

	if (TempGerberInfo.SizeXColumn == -1)
	{
		if (TempGerberInfo.ApertureTypeColumn != -1)
			TempGerberInfo.SizeXColumn = TempGerberInfo.ApertureTypeColumn + 1;
		else
			TempGerberInfo.SizeXColumn = TempGerberInfo.DcodeColumn + 1;
	}

	if (TempGerberInfo.SizeXColumn != -1)
		TempGerberInfo.SizeYColumn = TempGerberInfo.SizeXColumn + 1;

	if (TempGerberInfo.SizeYColumn != -1)
	{
		cnt = TempGerberInfo.SkipLines;
		Stop = 0;

		while ((!Stop) && (cnt < TempGerberInfo.MaxLineNr))
		{
			cnt2 = 0;

			while ((cnt2 < ApertureLines[cnt].NrStrings) && ((ApertureLines[cnt].Column[cnt2] & (64 + 32)) < 64))
				cnt2++;

			if (cnt2 < ApertureLines[cnt].NrStrings)
			{
				if (((ApertureLines[cnt].Column[TempGerberInfo.SizeYColumn] & (16 + 8)) == 0)
				        && (TempGerberInfo.SizeYColumn + 1 < ApertureLines[cnt].NrStrings))
				{
					TempGerberInfo.SizeYColumn++;
					Stop = 1;

					if (((ApertureLines[cnt].Column[TempGerberInfo.SizeYColumn] & (16 + 8)) == 0)
					        && (TempGerberInfo.SizeYColumn + 1 < ApertureLines[cnt].NrStrings))
						TempGerberInfo.SizeYColumn++;
				}
			}

			cnt++;
		}
	}

	if (TempGerberInfo.ApertureTypeColumn == -1)
		TempGerberInfo.SizeYColumn = -1;

// ************************************************************************************************

	if (TempGerberInfo.SizeXColumn != -1)
	{
		MinSizeX = 1000000.0;
		MaxSizeX = -1000000.0;

		for (cnt = TempGerberInfo.SkipLines; cnt < TempGerberInfo.MaxLineNr; cnt++)
		{
			if (ApertureLines[cnt].Values[TempGerberInfo.SizeXColumn] < MinSizeX)
				MinSizeX = ApertureLines[cnt].Values[TempGerberInfo.SizeXColumn];

			if (ApertureLines[cnt].Values[TempGerberInfo.SizeXColumn] > MaxSizeX)
				MaxSizeX = ApertureLines[cnt].Values[TempGerberInfo.SizeXColumn];
		}
	}

	TempGerberInfo.AperTureUnits = 0;

	if (MaxSizeX < 10.0)
	{
		TempGerberInfo.AperTureUnits = 1;

		if (MaxSizeX < 0.4)
			TempGerberInfo.AperTureUnits = 2;
	}

	res =
	    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_APERTURE_IMPORT), PCBWindow,
	                 (DLGPROC) ApertureDialog2);
	return res;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

__inline int32 GetGerberFileChar(int fp, char *ch, int32 mode)
{
	if (GerberBufPos == GerberBufLength)
	{
		if ((FileRead(fp, GerberBuf, GerberBufSize, &GerberBufLength) == -1) || (GerberBufLength == 0))
		{
			GerberBufPos = GerberBufLength;
			return -1;
		}

		GerberBufPos = 0;
	}

	*ch = GerberBuf[GerberBufPos];

	if (mode == 0)
	{
		GerberBufPos++;
		GerberFileP++;
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

__inline int32 ReadGerberCommand(int fp, LPSTR GerberCommand)
{

	char ch, ch2;
	int32 ok, LineLength;
	int32 Stop;

	ch = 0;
	ch2 = 0;
	Stop = 0;
	LineLength = 0;

	while (!Stop)
	{
		if (GetGerberFileChar(fp, &ch, 0) == 0)
		{
			if (ch == '%')
			{
				if (!ActiveAperTureMacro)
					ActiveAperTureMacro = 1;
				else
					ActiveAperTureMacro = 0;
			}
			else
			{
				if (ch != '*')
				{
					if ((ch != 13) && (ch != 10) && (ch != ' ') && (ch != '\t'))
					{
						if (ActiveAperTureMacro)
						{
							if ((LineLength == 0) && ((isdigit(ch)) || (ch == '$')))
							{
								GerberCommand[LineLength++] = 'M';
								GerberCommand[LineLength++] = 'A';
								GerberCommand[LineLength++] = 'C';
							}

							if (LineLength < MAX_GERBER_LINE_LENGTH - 1)
								GerberCommand[LineLength++] = ch;
						}
						else
						{
							if (LineLength < MAX_GERBER_LINE_LENGTH - 1)
								GerberCommand[LineLength++] = ch;
						}
					}
				}
				else
				{
					Stop = 1;

					if (ActiveAperTureMacro)
					{
						ok = 1;

						if (GetGerberFileChar(fp, &ch2, 1) == 0)
						{
							ok = 1;

							if (ch2 == '%')
							{
								GetGerberFileChar(fp, &ch2, 0);
								ActiveAperTureMacro = 0;
							}
							else
								ok = 1;
						}
					}
				}
			}
		}
		else
		{
			Stop = 1;

			if (LineLength == 0)
				return -1;
		}
	}

	GerberCommand[LineLength] = 0;
	GerberLineNr++;
	return LineLength;
}



// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 AddLine(double x1, double y1, double x2, double y2, int32 AperNr, int32 Layer, int32 mode)
{
	ObjectRecord *AperTureObject;
	double PenSize;
	ObjectRecord NewObject;
	int32 cnt2;

	memset(&NewObject, 0, sizeof(ObjectRecord));

	if ((cnt2 = GetAperTureNr(AperNr)) >= 0)
		AperTureObject = &((*Objects4)[cnt2]);
	else
		AperTureObject = &DefaultApertureObject2;

	PenSize = AperTureObject->x1;
	NewObject.Info2 = AperNr;
	NewObject.Layer = Layer;

	if (InRangeSpecial(x1, x2, 2540.0))
	{	// Vert
		if (AperTureObject->ObjectType == PIN_SMD_ROUND)
		{
			NewObject.y1 = min(y2, y1);
			NewObject.x1 = x1;
			NewObject.x2 = fabs(y2 - y1);
			NewObject.y2 = PenSize;
			NewObject.ObjectType = TRACE_VER;
			AddGerberObject(&NewObject, 1);
		}

		if (AperTureObject->ObjectType == PIN_SMD_RECT)
		{
			NewObject.x2 = AperTureObject->x1;
			NewObject.y2 = (fabs(y2 - y1) + AperTureObject->y1);
			NewObject.x1 = x1;
			NewObject.y1 = ((y2 + y1) * 0.5);
			NewObject.ObjectType = PIN_SMD_RECT;
			AddGerberObject(&NewObject, 1);
		}
	}
	else
	{
		if (InRangeSpecial(y1, y2, 2540.0))
		{	// hor
			if (AperTureObject->ObjectType == PIN_SMD_ROUND)
			{
				NewObject.x1 = min(x2, x1);
				NewObject.y1 = y1;
				NewObject.x2 = fabs(x2 - x1);
				NewObject.y2 = PenSize;
				NewObject.ObjectType = TRACE_HOR;
				AddGerberObject(&NewObject, 1);
			}

			if (AperTureObject->ObjectType == PIN_SMD_RECT)
			{
				NewObject.x2 = (fabs(x2 - x1) + AperTureObject->x1);
				NewObject.y2 = AperTureObject->y1;
				NewObject.x1 = ((x2 + x1) * 0.5);
				NewObject.y1 = y1;
				NewObject.ObjectType = PIN_SMD_RECT;
				AddGerberObject(&NewObject, 1);
			}
		}
		else
		{
			if (InRangeSpecial(x1 - x2, y1 - y2, 2540.0))
			{	// diag2
				if (AperTureObject->ObjectType == PIN_SMD_ROUND)
				{
					if (x1 > x2)
					{
						NewObject.x1 = x2;
						NewObject.y1 = y2;
						NewObject.x2 = x1 - x2;
					}
					else
					{
						NewObject.x1 = x1;
						NewObject.y1 = y1;
						NewObject.x2 = x2 - x1;
					}

					NewObject.y2 = PenSize;
					NewObject.ObjectType = TRACE_DIAG2;
					AddGerberObject(&NewObject, 1);
				}

				if (AperTureObject->ObjectType == PIN_SMD_RECT)
				{
					if (x1 > x2)
					{
						NewObject.x1 = x2;
						NewObject.y1 = y2;
						NewObject.x2 = x1;
						NewObject.y2 = y1;
					}
					else
					{
						NewObject.x1 = x1;
						NewObject.y1 = y1;
						NewObject.x2 = x2;
						NewObject.y2 = y2;
					}

					NewObject.y2 = min(AperTureObject->x1, AperTureObject->y1);
					NewObject.ObjectType = TRACE_DIAG2;
					AddGerberObject(&NewObject, 1);
				}
			}
			else
			{
				if (InRangeSpecial(x1 - x2, y2 - y1, 2540.0))
				{	// diag1
					if (AperTureObject->ObjectType == PIN_SMD_ROUND)
					{
						if (x1 > x2)
						{
							NewObject.x1 = x2;
							NewObject.y1 = y2;
							NewObject.x2 = x1 - x2;
						}
						else
						{
							NewObject.x1 = x1;
							NewObject.y1 = y1;
							NewObject.x2 = x2 - x1;
						}

						NewObject.y2 = PenSize;
						NewObject.ObjectType = TRACE_DIAG1;
						AddGerberObject(&NewObject, 1);
					}

					if (AperTureObject->ObjectType == PIN_SMD_RECT)
					{
						if (x1 > x2)
						{
							NewObject.x1 = x2;
							NewObject.y1 = y2;
							NewObject.x2 = x1;
							NewObject.y2 = y1;
						}
						else
						{
							NewObject.x1 = x1;
							NewObject.y1 = y1;
							NewObject.x2 = x2;
							NewObject.y2 = y2;
						}

						NewObject.y2 = min(AperTureObject->x1, AperTureObject->y1);
						NewObject.ObjectType = TRACE_DIAG1;
						AddGerberObject(&NewObject, 1);
					}
				}
				else
				{	// Other trace
					NewObject.x1 = x1;
					NewObject.y1 = y1;
					NewObject.x2 = x2;
					NewObject.y2 = y2;
					NewObject.Thickness = PenSize;
					NewObject.ObjectType = TRACE_ALL_ANGLE;
					AddGerberObject(&NewObject, 1);
				}
			}
		}
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

void SetGerberParameters(HWND Dialog)
{
	char str[MAX_LENGTH_STRING];

	SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_RESETCONTENT, 0, 0);

	SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 0, 0);
	SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 0, 0);
	SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 0, 0);

	SetUnitText(Dialog, IDC_EDIT1, TempGerberInfo.Units);

	switch (TempGerberInfo.Units)
	{
	case 0:					// thou
		TempGerberInfo.XDigits1 = -1;
		TempGerberInfo.YDigits1 = -1;
		TempGerberInfo.XDigits2 = -1;
		TempGerberInfo.YDigits2 = -1;
		TempGerberInfo.NumberFormat = 1;
		break;

	case 1:					// mm
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "3  2");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "3  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "3  4");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "4  2");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "4  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "4  4");

		if (TempGerberInfo.ZeroMode == 0)
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);

		if ((TempGerberInfo.XDigits1 < 3) || (TempGerberInfo.XDigits1 > 4) || (TempGerberInfo.XDigits2 < 2)
		        || (TempGerberInfo.XDigits2 > 4))
		{
			TempGerberInfo.XDigits1 = 3;
			TempGerberInfo.YDigits1 = 3;
			TempGerberInfo.XDigits2 = 2;
			TempGerberInfo.YDigits2 = 2;
		}

		TempGerberInfo.NumberFormat = 0;
		sprintf(str, "%i  %i", TempGerberInfo.XDigits1, TempGerberInfo.XDigits2);
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);
		break;

	case 2:					// inch
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "1  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "2  2");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "2  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "2  4");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "2  5");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "3  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "3  4");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "3  5");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "4  3");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "4  4");
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_ADDSTRING, (WPARAM) 0, (LPARAM) "4  5");

		if ((TempGerberInfo.XDigits1 < 2) || (TempGerberInfo.XDigits1 > 4) || (TempGerberInfo.XDigits2 < 2)
		        || (TempGerberInfo.XDigits2 > 5))
		{
			TempGerberInfo.XDigits1 = 2;
			TempGerberInfo.YDigits1 = 2;
			TempGerberInfo.XDigits2 = 3;
			TempGerberInfo.YDigits2 = 3;
		}

		TempGerberInfo.NumberFormat = 0;
		sprintf(str, "%i  %i", TempGerberInfo.XDigits1, TempGerberInfo.XDigits2);
		SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_SELECTSTRING, (WPARAM) - 1, (LPARAM) str);
		break;

	case 3:					// 0.01 mm
		TempGerberInfo.NumberFormat = 1;
		break;
	}

	if (TempGerberInfo.NumberFormat == 1)
	{
		SendDlgItemMessageOwn(Dialog, IDC_RADIO3, BM_SETCHECK, 1, 0);
		TempGerberInfo.XDigits1 = -1;
		TempGerberInfo.YDigits1 = -1;
		TempGerberInfo.XDigits2 = -1;
		TempGerberInfo.YDigits2 = -1;
	}
	else
	{
		if (TempGerberInfo.ZeroMode == 0)
			SendDlgItemMessageOwn(Dialog, IDC_RADIO1, BM_SETCHECK, 1, 0);
		else
			SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_SETCHECK, 1, 0);
	}
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 CALLBACK GerberDialog2(HWND Dialog, UINT Message, WPARAM WParam, LPARAM LParam)
{
	int32 about;
	int32 hulp, cnt, res;
	char str[MAX_LENGTH_STRING];


	about = 1;

	switch (Message)
	{
	case WM_INITDIALOG:
// ************************************************************************************************
		SetWindowTextUTF8(Dialog, SC(482, "Load gerber file"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC1, SC(483, "First gerber data"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC2, SC(484, "Gerber format"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC3, SC(262, "Number format"));
		SetDialogItemTextUTF8(Dialog, IDC_STATIC4, SC(814, "Units"));

		SetDialogItemTextUTF8(Dialog, IDC_RADIO1, SC(485, "Leading zero suppression"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO2, SC(486, "Trailing zero suppression"));
		SetDialogItemTextUTF8(Dialog, IDC_RADIO3, SC(487, "Floating point"));

		SetDialogItemTextUTF8(Dialog, IDHELP, SC(156, "Help"));
		SetDialogItemTextUTF8(Dialog, IDOK, "OK");
		SetDialogItemTextUTF8(Dialog, IDCANCEL, SC(157, "Cancel"));
		SetDialogItemTextUTF8(Dialog, IDD_UNITS, SC(488, "thou/mm/inch/0.01mm"));
		SetDialogItemTextUTF8(Dialog, IDD_HINT, SC(481, "Hint by program"));

// ********************************************************************
		for (cnt = 0; cnt < TempGerberInfo.TotalLines; cnt++)
			SendDlgItemMessageOwn(Dialog, IDC_LIST1, LB_ADDSTRING, 0, (LPARAM) (LPSTR) & ((*AperTureText)[cnt]));

		SetGerberParameters(Dialog);
		res = 1;
		return about;

	case WM_MOVE:
		break;

	case WM_SIZE:
		res = 1;
		break;

	case WM_COMMAND:
		switch (LOWORD(WParam))
		{
		case IDD_UNITS:
			TempGerberInfo.Units = (TempGerberInfo.Units + 1) % 4;
			SetGerberParameters(Dialog);
			break;

		case IDOK:
			res = SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETCURSEL, 0, 0);

			if (res >= 0)
			{
				SendDlgItemMessageOwn(Dialog, IDC_LIST2, LB_GETTEXT, res, (LPARAM) str);
				TempGerberInfo.XDigits1 = str[0] - 48;
				TempGerberInfo.XDigits2 = str[3] - 48;
			}

			TempGerberInfo.YDigits1 = TempGerberInfo.XDigits1;
			TempGerberInfo.YDigits2 = TempGerberInfo.XDigits2;
			TempGerberInfo.ZeroMode = 0;

			if (SendDlgItemMessageOwn(Dialog, IDC_RADIO2, BM_GETCHECK, 0, 0) == 1)
				TempGerberInfo.ZeroMode = 1;

			EndDialog(Dialog, 1);
			break;

		case IDC_RADIO1:
			TempGerberInfo.ZeroMode = 0;
			TempGerberInfo.NumberFormat = 0;
			TempGerberInfo.Units = 2;
			SetGerberParameters(Dialog);
			break;

		case IDC_RADIO2:
			TempGerberInfo.ZeroMode = 1;
			TempGerberInfo.NumberFormat = 0;
			TempGerberInfo.Units = 2;
			SetGerberParameters(Dialog);
			break;

		case IDC_RADIO3:
			TempGerberInfo.NumberFormat = 1;
			TempGerberInfo.Units = 0;
			SetGerberParameters(Dialog);
			break;

		case IDCANCEL:
			EndDialog(Dialog, 2);
			return about;

		case IDHELP:
//          Help(IDH_Load_gerber_file,0);
			break;
		}

		break;

	case WM_PARENTNOTIFY:
		hulp = 1;
		break;
	}

	about = 0;
	return about;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************


int32 LoadGerberFile(LPSTR GerberFile, int32 Layer, int32 mode)
{
	int32 fp, cnt, cnt2, cnt3, FoundCnt, Length, AperNr, res, ok, GerberAction, Temp, NewAperNr, DigitCountX[10],
	      DigitCountY[10], NrMacroNames, NrGerberLayers, ValueXint, ValueYint, ValueSides, GerberObjectCount,
	      NrPolygonPoints, NrPolygonPoints2, CntX, CntY, StepAndRepeatMode, StepCountX, StepCountY, Length2, FilePosK,
	      NrGerberCommands;
	char str1[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], DCodeStr[MAX_LENGTH_STRING], StrX[MAX_LENGTH_STRING],
	     StrY[MAX_LENGTH_STRING], StrX2[MAX_LENGTH_STRING], StrY2[MAX_LENGTH_STRING], CopyInfoStr[MAX_LENGTH_STRING],
	     ObjectChar;
	double LastX, LastY, MinX, MinY, MaxX, MaxY, x1, y1, x2, y2, x3, y3, x4, y4, AperTureParameters[10], Angle1, Angle2,
	       LengthArc1, LengthArc2, DiffAngle, DiffAngles[4];
	float Value1, Value2, Value3, Value4, ValueX, ValueY, ValueX2, ValueY2, StepX, StepY;
	uint8 AperUsage[1000];

	double Mult10[8] = { 1.0,
	                     10.0,
	                     100.0,
	                     1000.0,
	                     10000.0,
	                     100000.0,
	                     1000000.0,
	                     10000000.0
	                   };
	ObjectRecord *AperTureObject;
	PolygonRecord *Polygon;
	int32 LightActive, Stop, AutoAperTures, SingleQuadrantInterpolation, CircularInterpolation, FoundFirstLayer,
	      StepAndRepeat, G7XCode, CircularInterpolationCW, LoadExtraPoint, PolygonMode;

	if (GerberFile[0] == 0)
		return -1;

	if ((fp = TextFileOpenUTF8(GerberFile)) < 0)
	{
		MessageBoxOwn(PCBWindow, GerberFile, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		return -1;
	}

	AllocateSpecialMem(MEM_POINTS, GerberBufSize, (void **) &GerberBuf);
	AllocateSpecialMem(MEM_APERTURE_TEXT, sizeof(AperTureTextRecord), (void **) &AperTureText);
	AllocateSpecialMem(MEM_GERBER_LINEBUF, MAX_GERBER_LINE_LENGTH, (void **) &LineBuf);

	memset(AperTureText, 0, sizeof(AperTureTextRecord));

	ValueX2 = 0.0;
	ValueY2 = 0.0;
	memset(&DefaultApertureObject, 0, sizeof(DefaultApertureObject));
	memset(&DefaultApertureObject2, 0, sizeof(DefaultApertureObject2));
	DefaultApertureObject.x1 = (40 * 2540.0);
	DefaultApertureObject.ObjectType = PIN_SMD_ROUND;
	DefaultApertureObject2.x1 = (10 * 2540.0);
	DefaultApertureObject2.ObjectType = PIN_SMD_ROUND;
	GerberLineNr = 0;
	GerberBufPos = 0;
	GerberBufLength = 0;
	GerberFileP = 0;
	GerberFileP2 = 0;
	MessageBufPos = 0;
	NrObjects6 = 0;
	NrObjects4 = 0;
//  SingleQuadrantInterpolation=0;
	SingleQuadrantInterpolation = 1;
	CircularInterpolation = 0;
	FoundFirstLayer = 1;
	CircularInterpolationCW = 0;
	NrGerberLayers = 0;
	LastX = 0.0;
	LastY = 0.0;
	memset(&AperUsage, 0, sizeof(AperUsage));
//  memmove(&TempGerberInfo,&Design.DefaultGerberInfo,sizeof(GerberInfoRecord));
	memset(GerberBuf, 0, GerberBufSize);

	TempGerberInfo.TotalLines = 0;
	Stop = 0;

	while (((Length = ReadLnWithMaxLength(fp, LineBuf, 1024)) >= 0) && (!Stop))
	{
		LineBuf[Length + 1] = 0;
		LineBuf[Length + 2] = 0;
		LineBuf[160 - 1] = 0;
		strcpy((LPSTR) & ((*AperTureText)[TempGerberInfo.TotalLines]), LineBuf);
		TempGerberInfo.TotalLines++;

		if (TempGerberInfo.TotalLines > 800)
			Stop = 1;
	}

	TextFileClose(fp);

// ************************************************************************************************

	TempGerberInfo.Units = -1;
	TempGerberInfo.NumberFormat = 0;
	TempGerberInfo.ZeroMode = 0;
	TempGerberInfo.Invert = 0;	// Positive film

	TempGerberInfo.XDigits1 = -1;
	TempGerberInfo.XDigits2 = -1;
	TempGerberInfo.YDigits1 = -1;
	TempGerberInfo.YDigits2 = -1;

	SetWaitCursor();
	AutoAperTures = 0;
	memset(&DigitCountX, 0, sizeof(DigitCountX));
	memset(&DigitCountY, 0, sizeof(DigitCountY));
	MinX = 100000000.0;
	MinY = 100000000.0;
	MaxX = -100000000.0;
	MaxY = -100000000.0;
	FilePosK = 0;
	GerberFileP = 0;
	GerberFileP2 = 0;
	GerberBufPos = 0;

	SetWaitCursor();

	if ((fp = FileOpenReadOnlyUTF8(GerberFile)) < 0)
		return -1;

	Stop = 0;
	NrGerberCommands = 0;
	strcpy(CopyInfoStr, InfoStr);

	while (((Length = ReadGerberCommand(fp, LineBuf)) >= 0) && (!Stop))
	{
		LineBuf[Length + 1] = 0;
		LineBuf[Length + 2] = 0;

		if (Length > 0)
		{
			ok = 1;

			if (Length == 3)
			{
				if (stricmpOwn(LineBuf, "G70") == 0)
				{
					TempGerberInfo.Units = 2;	// inch
				}

				if (stricmpOwn(LineBuf, "G71") == 0)
				{
					TempGerberInfo.Units = 1;	// mm
				}
			}

			if ((Length > 3) && (LineBuf[0] == 'G')
			        && ((LineBuf[1] == '1') || (LineBuf[1] == '2') || (LineBuf[1] == '3')) && ((LineBuf[2] == 'X')
			                || (LineBuf[2] == 'Y')))
			{
				memmove(&LineBuf[0], &LineBuf[2], Length);
				Length -= 2;
			}

			if ((Length > 4) && (LineBuf[0] == 'G') && (LineBuf[1] == '0')
			        && ((LineBuf[2] == '1') || (LineBuf[2] == '2') || (LineBuf[2] == '3')) && ((LineBuf[3] == 'X')
			                || (LineBuf[3] == 'Y')))
			{
				memmove(&LineBuf[0], &LineBuf[3], Length);
				Length -= 3;
			}

			if ((Length > 4) && (LineBuf[0] == 'G') && (LineBuf[1] == '7') && (LineBuf[2] == '4')
			        && ((LineBuf[3] == 'X') || (LineBuf[3] == 'Y')))
			{
				memmove(&LineBuf[0], &LineBuf[3], Length);
				Length -= 3;
			}

			if ((Length > 4) && (LineBuf[0] == 'G') && (LineBuf[1] == '7') && (LineBuf[2] == '5')
			        && ((LineBuf[3] == 'X') || (LineBuf[3] == 'Y')))
			{
				memmove(&LineBuf[0], &LineBuf[3], Length);
				Length -= 3;
			}

// ************************************************************************************************
// ************************************************************************************************
			switch (LineBuf[0])
			{
			case 'A':
				if ((Length > 6) && (LineBuf[1] == 'D') && (LineBuf[2] == 'D'))
					AutoAperTures = 1;

				break;

// ************************************************************************************************
// ************************************************************************************************
			case 'F':
				if ((Length > 7) && (LineBuf[1] == 'S'))
				{
					AutoAperTures = 1;
					cnt = 2;

					switch (LineBuf[cnt])
					{
					case 'L':
						TempGerberInfo.ZeroMode = 0;	// Leading zeros omitted
						cnt++;
						break;

					case 'T':
						TempGerberInfo.ZeroMode = 1;	// Trailing zeros omitted
						cnt++;
						break;

					case 'D':
//                TempGerberInfo.NumberFormat=1;  // Floating point coordinates
						TempGerberInfo.ZeroMode = 0;	// Leading zeros omitted
						cnt++;
						break;
					}

					switch (LineBuf[cnt])
					{
					case 'A':
						TempGerberInfo.Incremental = 0;	// Absolute coordinates
						cnt++;
						break;

					case 'I':
						TempGerberInfo.Incremental = 1;	// Incremental coordinates
						cnt++;
						break;
					}

					while ((cnt < Length - 5) && (LineBuf[cnt] != 'X'))
						cnt++;

					if ((LineBuf[cnt] == 'X') && (cnt < Length - 5) && (LineBuf[cnt + 3] == 'Y'))
					{
						TempGerberInfo.XDigits1 = LineBuf[cnt + 1] - 48;
						TempGerberInfo.XDigits2 = LineBuf[cnt + 2] - 48;
						TempGerberInfo.YDigits1 = LineBuf[cnt + 4] - 48;
						TempGerberInfo.YDigits2 = LineBuf[cnt + 5] - 48;
					}
				}

				break;

// ************************************************************************************************
// ************************************************************************************************
			case 'G':
				if (strcmp(LineBuf, "G70") == 0)
				{
					TempGerberInfo.Units = 2;	// inches
					break;
				}

				if (strcmp(LineBuf, "G71") == 0)
				{
					TempGerberInfo.Units = 1;	// mm
					break;
				}

// ************************************************************************************************
// ************************************************************************************************
			case 'I':
				if (strcmp(LineBuf, "IPPOS") == 0)
				{
					TempGerberInfo.Invert = 0;	// Positive film
					break;
				}

				if (strcmp(LineBuf, "IPNEG") == 0)
				{
					TempGerberInfo.Invert = 1;	// Negative film
					break;
				}

				break;

// ************************************************************************************************
// ************************************************************************************************
			case 'L':
				ok = 1;

				if (strcmp(LineBuf, "LPD") == 0)
					InvertedGerber = 0;

				if (strcmp(LineBuf, "LPC") == 0)
					InvertedGerber = 1;

				if ((Length > 2) && (LineBuf[1] == 'N'))
				{
//            NrGerberLayers++;
//            FoundFirstLayer=0;
				}

				break;

			case 'M':
				if (strcmp(LineBuf, "MOIN") == 0)
				{
					TempGerberInfo.Units = 2;	// inches
					break;
				}

				if (strcmp(LineBuf, "MOMM") == 0)
				{
					TempGerberInfo.Units = 1;	// mm
					break;
				}

				break;

			case 'X':
				StrX[0] = 0;
				StrY[0] = 0;
				cnt2 = 1;
				cnt = 1;

				while ((cnt2 < Length) && (LineBuf[cnt2] != 'Y') && (LineBuf[cnt2] != 'I') && (LineBuf[cnt2] != 'D'))
					cnt2++;

				strcpy(StrX, (LPSTR) & LineBuf[cnt]);
				StrX[cnt2 - cnt] = 0;

				if (LineBuf[cnt2] == 'Y')
				{
					cnt2++;
					cnt = cnt2;

					while ((cnt2 < Length) && (LineBuf[cnt2] != 'I') && (LineBuf[cnt2] != 'D'))
						cnt2++;

					strcpy(StrY, (LPSTR) & LineBuf[cnt]);
					StrY[cnt2 - cnt] = 0;
				}

				Length2 = strlen(StrX);

				if (Length2 > 0)
				{
					if (StrX[0] == '-')
						DigitCountX[Length2 - 1]++;
					else
						DigitCountX[Length2]++;

					sscanf(StrX, "%f", &ValueX);
					MinX = min(MinX, ValueX);
					MaxX = max(MaxX, ValueX);
				}

				Length2 = strlen(StrY);

				if (Length2 > 0)
				{
					if (StrY[0] == '-')
						DigitCountY[Length2 - 1]++;
					else
						DigitCountY[Length2]++;

					sscanf(StrY, "%f", &ValueY);
					MinY = min(MinY, ValueY);
					MaxY = max(MaxY, ValueY);
				}

				break;
			}
		}

		NrGerberCommands++;

		if (NrGerberCommands > 10000)
			Stop = 1;
	}

	FileClose(fp);
	SetNormalCursor();

	res = 2;

	if (!AutoAperTures)
	{
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
		cnt = 8;
		Stop = 0;

		while ((cnt > 4) && (!Stop))
		{
			if ((DigitCountX[cnt] > 0) || (DigitCountY[cnt] > 0))
			{
				switch (cnt)
				{
				case 5:
					switch (TempGerberInfo.Units)
					{
					case -1:
						if ((MaxX - MinX > 15000.0) || (MaxY - MinY > 15000.0))
						{
							TempGerberInfo.Units = 1;	// mm
							TempGerberInfo.XDigits1 = 3;
							TempGerberInfo.YDigits1 = 3;
							TempGerberInfo.XDigits2 = 2;
							TempGerberInfo.YDigits2 = 2;
						}
						else
						{
							TempGerberInfo.Units = 2;	// inch
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 3;
							TempGerberInfo.YDigits2 = 3;
						}

						break;

					case 1:	// mm
						if ((MaxX - MinX > 40000.0) || (MaxY - MinY > 40000.0))
						{	// 400 mm
							TempGerberInfo.XDigits1 = 3;
							TempGerberInfo.YDigits1 = 3;
							TempGerberInfo.XDigits2 = 2;
							TempGerberInfo.YDigits2 = 2;
						}
						else
						{
							TempGerberInfo.XDigits1 = 3;
							TempGerberInfo.YDigits1 = 3;
							TempGerberInfo.XDigits2 = 3;
							TempGerberInfo.YDigits2 = 3;
						}

						break;

					case 2:	// inch
						if ((MaxX - MinX > 25000.0) || (MaxY - MinY > 25000.0))
						{	// 25 inch
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 4;
							TempGerberInfo.YDigits2 = 4;
						}
						else
						{
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 3;
							TempGerberInfo.YDigits2 = 3;
						}

						break;
					}

					break;

				case 6:
					switch (TempGerberInfo.Units)
					{
					case -1:
						if ((MaxX - MinX > 150000.0) || (MaxY - MinY > 150000.0))
						{
							TempGerberInfo.Units = 1;	// mm
							TempGerberInfo.XDigits1 = 3;
							TempGerberInfo.YDigits1 = 3;
							TempGerberInfo.XDigits2 = 3;
							TempGerberInfo.YDigits2 = 3;
						}
						else
						{
							TempGerberInfo.Units = 2;	// inch
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 4;
							TempGerberInfo.YDigits2 = 4;
						}

						break;

					case 1:	// mm
						if ((MaxX - MinX > 400000.0) || (MaxY - MinY > 400000.0))
						{	// 400 mm
							TempGerberInfo.XDigits1 = 4;
							TempGerberInfo.YDigits1 = 4;
							TempGerberInfo.XDigits2 = 2;
							TempGerberInfo.YDigits2 = 2;
						}
						else
						{
							if ((MaxX - MinX > 40000.0) || (MaxY - MinY > 40000.0))
							{	// 40 mm
								TempGerberInfo.XDigits1 = 3;
								TempGerberInfo.YDigits1 = 3;
								TempGerberInfo.XDigits2 = 3;
								TempGerberInfo.YDigits2 = 3;
							}
							else
							{
								TempGerberInfo.XDigits1 = 4;
								TempGerberInfo.YDigits1 = 4;
								TempGerberInfo.XDigits2 = 2;
								TempGerberInfo.YDigits2 = 2;
							}
						}

						break;

					case 2:	// inch
						if ((MaxX - MinX > 150000.0) || (MaxY - MinY > 150000.0))
						{	// 15 inch
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 5;
							TempGerberInfo.YDigits2 = 5;
						}
						else
						{
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 4;
							TempGerberInfo.YDigits2 = 4;
						}

						break;
					}

					Stop = 1;
					break;

				case 7:
					switch (TempGerberInfo.Units)
					{
					case -1:
						if ((MaxX - MinX > 1500000.0) || (MaxY - MinY > 1500000.0))
						{
							TempGerberInfo.Units = 2;	// inch
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 5;
							TempGerberInfo.YDigits2 = 5;
						}
						else
						{
							TempGerberInfo.Units = 1;	// mm
							TempGerberInfo.XDigits1 = 3;
							TempGerberInfo.YDigits1 = 3;
							TempGerberInfo.XDigits2 = 4;
							TempGerberInfo.YDigits2 = 4;
						}

						break;

					case 1:
						TempGerberInfo.XDigits1 = 3;
						TempGerberInfo.YDigits1 = 3;
						TempGerberInfo.XDigits2 = 4;
						TempGerberInfo.YDigits2 = 4;
						break;

					case 2:
						if ((MaxX - MinX > 1500000.0) || (MaxY - MinY > 1500000.0))
						{	// 15 inch
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 6;
							TempGerberInfo.YDigits2 = 6;
						}
						else
						{
							TempGerberInfo.XDigits1 = 2;
							TempGerberInfo.YDigits1 = 2;
							TempGerberInfo.XDigits2 = 5;
							TempGerberInfo.YDigits2 = 5;
						}

						break;
					}

					Stop = 1;
					break;

				case 8:
					switch (TempGerberInfo.Units)
					{
					case -1:
						if ((MaxX - MinX > 10000000.0) || (MaxY - MinY > 10000000.0))
						{
							TempGerberInfo.Units = 2;	// inch
							TempGerberInfo.XDigits1 = 3;
							TempGerberInfo.YDigits1 = 3;
							TempGerberInfo.XDigits2 = 5;
							TempGerberInfo.YDigits2 = 5;
						}
						else
						{
							TempGerberInfo.Units = 1;	// mm
							TempGerberInfo.XDigits1 = 4;
							TempGerberInfo.YDigits1 = 4;
							TempGerberInfo.XDigits2 = 4;
							TempGerberInfo.YDigits2 = 4;
						}

						break;

					case 1:
						TempGerberInfo.XDigits1 = 4;
						TempGerberInfo.YDigits1 = 4;
						TempGerberInfo.XDigits2 = 4;
						TempGerberInfo.YDigits2 = 4;
						break;

					case 2:
						if ((MaxX - MinX > 00200000.0) || (MaxY - MinY > 00200000.0))
						{	// 200 mm
							TempGerberInfo.XDigits1 = 4;
							TempGerberInfo.YDigits1 = 4;
							TempGerberInfo.XDigits2 = 4;
							TempGerberInfo.YDigits2 = 4;
						}
						else
						{
							TempGerberInfo.XDigits1 = 3;
							TempGerberInfo.YDigits1 = 3;
							TempGerberInfo.XDigits2 = 5;
							TempGerberInfo.YDigits2 = 5;
						}

						break;
					}

					Stop = 1;
					break;
				}
			}

			cnt--;
		}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
		/*
		    if (StartUp.GerberDigits1!=-1) {
		      TempGerberInfo.XDigits1=StartUp.GerberDigits1;
		      TempGerberInfo.XDigits2=StartUp.GerberDigits2;
		    } else {
		      TempGerberInfo.XDigits1=TempGerberInfo.XDigits1;
		      TempGerberInfo.XDigits2=TempGerberInfo.XDigits2;
		    }
		    if (StartUp.GerberDigits2!=-1) {
		      TempGerberInfo.YDigits1=StartUp.GerberDigits1;
		      TempGerberInfo.YDigits2=StartUp.GerberDigits2;
		    } else {
		      TempGerberInfo.YDigits1=TempGerberInfo.YDigits1;
		      TempGerberInfo.YDigits2=TempGerberInfo.YDigits2;
		    }
		    if (StartUp.GerberUnits!=-1) {
		      TempGerberInfo.Units=StartUp.GerberUnits;
		    } else {
		      TempGerberInfo.Units=TempGerberInfo.Units;
		    }
		    if (StartUp.GerberZeroMode!=-1) {
		      TempGerberInfo.ZeroMode=StartUp.GerberZeroMode;
		    }
		    if (StartUp.GerberNumberFormat!=-1) {
		      TempGerberInfo.NumberFormat=StartUp.GerberNumberFormat;
		    }
		*/
		res =
		    OwnDialogBox(PCBClass.hInstance, MAKEINTRESOURCE(IDD_DIALOG_GERBER_INPUT), PCBWindow,
		                 (DLGPROC) GerberDialog2);

		if (res == 2)
			return -3;

		if (LocalAperTureFile[0] == 0)
			strcpy(LocalAperTureFile, GerberFile);

		if (GetNewFileUTF8
		        (PCBWindow, NULL, LocalAperTureFile, ExportDir, SC(489, "Load aperture file"), NULL,
		         SC(489, "Load aperture file"), "*", 0))
			return -1;

//    if (LoadNewFile3(LocalAperTureFile,SC(489,"Load aperture file"),SC(490,"Aperture files"),0,0)!=0) return -1;
		if (CheckFileIsAperTure(LocalAperTureFile))
			LoadApertureFile(LocalAperTureFile, 0);
		else
		{
			sprintf(str, SC(491, "File %s is not an aperture file"));
			MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_OK);
		}

		/*
		    if (GetNrAperturesLayer(Layer,0)==0) {
		      Design.LayerInfo[Layer]&=~GERBER_FILE_OWN_APERTURES;
		      Design.DefaultGerberInfo.XDigits1=TempGerberInfo.XDigits1;
		      Design.DefaultGerberInfo.XDigits2=TempGerberInfo.XDigits2;
		      Design.DefaultGerberInfo.YDigits1=TempGerberInfo.YDigits1;
		      Design.DefaultGerberInfo.YDigits2=TempGerberInfo.YDigits2;
		      Design.DefaultGerberInfo.Units=TempGerberInfo.Units;
		      Design.DefaultGerberInfo.ZeroMode=TempGerberInfo.ZeroMode;
		      Design.DefaultGerberInfo.NumberFormat=TempGerberInfo.NumberFormat;
		      Design.SpecialGerberInfo[Layer]=0;
		    } else {
		      Design.LayerInfo[Layer]|=GERBER_FILE_OWN_APERTURES;
		      Design.SpecialGerberInfo[Layer]=(TempGerberInfo.XDigits1     << 16) +
		                                      (TempGerberInfo.XDigits2     << 12) +
		                                      (TempGerberInfo.Units        << 8)  +
		                                      (TempGerberInfo.ZeroMode     << 7)  +
		                                      (TempGerberInfo.NumberFormat << 6);
		    }
		*/
	}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
	/*
	    if (StartUp.GerberDigits1!=-1) {
	      TempGerberInfo.XDigits1=StartUp.GerberDigits1;
	      TempGerberInfo.XDigits2=StartUp.GerberDigits2;
	    }
	    if (StartUp.GerberDigits2!=-1) {
	      TempGerberInfo.YDigits1=StartUp.GerberDigits1;
	      TempGerberInfo.YDigits2=StartUp.GerberDigits2;
	    }
	    if (StartUp.GerberUnits!=-1) {
	      TempGerberInfo.Units=StartUp.GerberUnits;
	    }
	    if (StartUp.GerberZeroMode!=-1) {
	      TempGerberInfo.ZeroMode=StartUp.GerberZeroMode;
	    }
	    if (StartUp.GerberNumberFormat!=-1) {
	      TempGerberInfo.NumberFormat=StartUp.GerberNumberFormat;
	    }

	    if (TempGerberInfo.XDigits1!=-1) {
	      TempGerberInfo.XDigits1=TempGerberInfo.XDigits1;
	      TempGerberInfo.YDigits1=TempGerberInfo.XDigits1;
	    }
	    if (TempGerberInfo.XDigits2!=-1) {
	      TempGerberInfo.XDigits2=TempGerberInfo.XDigits2;
	      TempGerberInfo.YDigits2=TempGerberInfo.XDigits2;
	    }
	    if (TempGerberInfo.Units!=-1) {
	      TempGerberInfo.Units=TempGerberInfo.Units;
	    }
	    TempGerberInfo.ZeroMode=TempGerberInfo.ZeroMode;
	    TempGerberInfo.Invert=TempGerberInfo.Invert;
	    if (TempGerberInfo.NumberFormat!=-1) {
	      TempGerberInfo.NumberFormat=TempGerberInfo.NumberFormat;
	    }

	    DeleteAperturesLayer(Layer,0);
	    Design.LayerInfo[Layer]|=GERBER_FILE_OWN_APERTURES;
	    Design.SpecialGerberInfo[Layer]=(TempGerberInfo.XDigits1 << 16) +
	                                    (TempGerberInfo.XDigits2 << 12) +
	                                    (TempGerberInfo.Units    << 8)  +
	                                    (TempGerberInfo.ZeroMode << 7)  + GERBER_FILE_AUTO_APERTURES;


	  Design.LayerInfo[Layer]&=~7;
	  Design.LayerInfo[Layer]|=GERBER_FILE;
	  Design.LayerInfo[Layer]&=~(APERTURES_LAYER_CHANGED); // Reset changed apertures
	  LastUsedSpecialGerberInfo=(TempGerberInfo.XDigits1     << 16) +
	                            (TempGerberInfo.XDigits2     << 12) +
	                            (TempGerberInfo.Units        << 8)  +
	                            (TempGerberInfo.ZeroMode     << 7)  +
	                            (TempGerberInfo.NumberFormat << 6);
	  strcpy(Design.FileNames[Layer],GerberFile);
	  Design.FileNames[Layer][192]=0;
	*/

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

	FilePosK = 0;
	GerberBufLength = 0;
	GerberFileP = 0;
	GerberFileP2 = 0;
	GerberBufPos = 0;
	SetWaitCursor();
	GerberAction = 2;
	cnt3 = 0;
	AperTureObject = NULL;
	LightActive = 0;
	NrMacroNames = 0;
	LoadExtraPoint = 0;
	FoundFirstLayer = 1;
	PolygonMode = 0;
	StepAndRepeat = 0;
	ActiveAperTureMacro = 0;
	InvertedGerber = 0;
	StepAndRepeatMode = 0;
	StepCountX = 0;
	StepCountY = 0;
	StepX = 0.0;
	StepY = 0.0;
	NrPolygonPoints = 0;
	NrPolygonPoints2 = 0;

	if (MaxAreaFillMemoryTemp == 0)
		AllocateMemAreaFillMemoryTemp(0);

	NewAreaFill = (AreaFillRecord *) AreaFillMemTemp;
	TempAreaFill = (AreaFillRecord *) AreaFillMemTemp2;

	Polygon = (PolygonRecord *) ((uint8 *) NewAreaFill + sizeof(AreaFillRecord));
	Polygon->minx = 1000000000.0;
	Polygon->miny = 1000000000.0;
	Polygon->maxx = -1000000000.0;
	Polygon->maxy = -1000000000.0;
	memset(NewAreaFill, 0, sizeof(AreaFillRecord));
	NewAreaFill->NrPolygons = 1;
	AperNr = 0;
	GerberObjectCount = 0;

	if ((fp = FileOpenReadOnlyUTF8(GerberFile)) < 0)
		return -1;

	while ((Length = ReadGerberCommand(fp, LineBuf)) >= 0)
	{
		LineBuf[Length + 1] = 0;
		LineBuf[Length + 2] = 0;
#ifdef _DEBUG

		if (stricmpOwn(LineBuf, "G03X031086Y014335I000476J000445D01") == 0)
			ok = 1;

#endif

		if ((GerberFileP >> 16) > FilePosK)
		{
			FilePosK = GerberFileP >> 16;
			sprintf(InfoStr, "%i k", FilePosK << 6);
			RedrawInfoStr(1);
		}

		if (Length > 0)
		{
#ifdef _DEBUG

			if (GerberFileP == 50806)
				ok = 1;

			if (GerberObjectCount == 84)
				ok = 1;

			if (cnt3 == 24)
				ok = 1;

#endif
			ok = 1;

			if (LineBuf[0] == 'G')
			{
				if ((Length > 3) && ((LineBuf[1] == '1') || (LineBuf[1] == '2') || (LineBuf[1] == '3'))
				        && ((LineBuf[2] == 'X') || (LineBuf[2] == 'Y') || (LineBuf[2] == 'I') || (LineBuf[2] == 'J')))
				{
					G7XCode = 0;

					switch (LineBuf[1])
					{
					case '1':
//              SingleQuadrantInterpolation=0;
//              SingleQuadrantInterpolation=1;
						CircularInterpolation = 0;
						CircularInterpolationCW = 0;
						break;

					case '2':
						CircularInterpolation = 1;
						CircularInterpolationCW = 1;
						LoadExtraPoint = 1;
						break;

					case '3':
						CircularInterpolation = 1;
						CircularInterpolationCW = 0;
						LoadExtraPoint = 1;
						break;
					}

					memmove(&LineBuf[0], &LineBuf[2], Length);
					Length -= 2;
				}
				else
				{
					if ((Length > 4) && (LineBuf[1] == '0')
					        && ((LineBuf[2] == '1') || (LineBuf[2] == '2') || (LineBuf[2] == '3')) && ((LineBuf[3] == 'X')
					                || (LineBuf[3] ==
					                    'Y')
					                || (LineBuf[3] ==
					                    'I')
					                || (LineBuf[3] ==
					                    'J')))
					{
						switch (LineBuf[2])
						{
						case '1':
							//              SingleQuadrantInterpolation=0;
//                SingleQuadrantInterpolation=1;
							CircularInterpolation = 0;
							CircularInterpolationCW = 0;
							break;

						case '2':
							CircularInterpolation = 1;
							CircularInterpolationCW = 1;
							LoadExtraPoint = 1;
							break;

						case '3':
							CircularInterpolation = 1;
							CircularInterpolationCW = 0;
							LoadExtraPoint = 1;
							break;
						}

						memmove(&LineBuf[0], &LineBuf[3], Length);
						Length -= 3;
					}
					else
					{
						if ((Length > 4) && (LineBuf[1] == '7') && (LineBuf[2] == '4')
						        && ((LineBuf[3] == 'X') || (LineBuf[3] == 'Y')))
						{
							G7XCode = 1;
							SingleQuadrantInterpolation = 1;
							CircularInterpolation = 1;
							LoadExtraPoint = 1;
							memmove(&LineBuf[0], &LineBuf[3], Length);
							Length -= 3;
						}

						if ((Length > 4) && (LineBuf[1] == '7') && (LineBuf[2] == '5')
						        && ((LineBuf[3] == 'X') || (LineBuf[3] == 'Y')))
						{
							G7XCode = 1;
							SingleQuadrantInterpolation = 0;
							CircularInterpolation = 1;
							LoadExtraPoint = 1;
							memmove(&LineBuf[0], &LineBuf[3], Length);
							Length -= 3;
						}
					}
				}
			}

// ************************************************************************************************
// ************************************************************************************************
			switch (LineBuf[0])
			{
			case 'A':
				if ((Length > 6) && (LineBuf[1] == 'D') && (LineBuf[2] == 'D'))
				{
					cnt = 3;
					NewAperNr = 0;

					while ((cnt < Length) && (isdigit(LineBuf[cnt])))
					{
						NewAperNr = NewAperNr * 10 + LineBuf[cnt] - 48;
						cnt++;
					}

#ifdef _DEBUG

					if (NewAperNr == 22)
						ok = 1;

#endif

					if (cnt < Length - 1)
					{
						ObjectChar = LineBuf[cnt];

						if (LineBuf[cnt + 1] == ',')
						{
							switch (ObjectChar)
							{
							case 'C':
							case 'R':
							case 'O':
								for (cnt3 = 0; cnt3 < 10; cnt3++)
									AperTureParameters[cnt3] = 0.0;

								cnt2 = cnt + 2;

								while ((cnt2 < Length) && (LineBuf[cnt2] != 'X'))
									cnt2++;

								strcpy(str1, (LPSTR) & LineBuf[cnt + 2]);
								str1[cnt2 - cnt - 2] = 0;
								Value1 = 0.0;
								Value2 = 0.0;
								Value3 = 0.0;
								Value4 = 0.0;
								sscanf(str1, "%f", &Value1);
								Value1 = (float) UnitsConvert(TempGerberInfo.Units, Value1);

								if (cnt2 < Length)
								{
									cnt2++;
									cnt = cnt2;

									while ((cnt2 < Length) && (LineBuf[cnt2] != 'X'))
										cnt2++;

									strcpy(str1, (LPSTR) & LineBuf[cnt]);
									str1[cnt2 - cnt] = 0;

									if (strcmp(str1, "0") != 0)
										sscanf(str1, "%f", &Value2);

									Value2 = (float) UnitsConvert(TempGerberInfo.Units, Value2);

									if (cnt2 < Length)
									{
										cnt2++;
										cnt = cnt2;

										while ((cnt2 < Length) && (LineBuf[cnt2] != 'X'))
											cnt2++;

										strcpy(str1, (LPSTR) & LineBuf[cnt]);
										str1[cnt2 - cnt] = 0;

										if (strcmp(str1, "0") != 0)
											sscanf(str1, "%f", &Value3);

										Value3 = (float) UnitsConvert(TempGerberInfo.Units, Value3);

										if (cnt2 < Length)
										{
											cnt2++;
											cnt = cnt2;

											while ((cnt2 < Length) && (LineBuf[cnt2] != 'X'))
												cnt2++;

											strcpy(str1, (LPSTR) & LineBuf[cnt]);
											str1[cnt2 - cnt] = 0;

											if (strcmp(str1, "0") != 0)
												sscanf(str1, "%f", &Value4);

											Value4 = (float) UnitsConvert(TempGerberInfo.Units, Value4);
											AperTureParameters[0] = Value3;
											AperTureParameters[1] = Value4;
										}
									}
								}

								switch (ObjectChar)
								{
								case 'C':
									if (Value1 > Value2)
									{
										Value1 = max((float) 10.0, Value1);
										AddAperTure(PIN_SMD_ROUND, NewAperNr, Value1, 0.0);
									}
									else
									{
										Value2 = max((float) 10.0, Value2);
										AddAperTure(PIN_SMD_ROUND, NewAperNr, Value2, 0.0);
									}

									break;

								case 'R':
									AddAperTure(PIN_SMD_RECT, NewAperNr, Value1, Value2);
									break;

								case 'O':
									if (Value1 < Value2)
										AddAperTure(PIN_LINE_VER, NewAperNr, Value1, Value2);
									else
										AddAperTure(PIN_LINE_HOR, NewAperNr, Value1, Value2);

									break;
								}

								break;

							case 'P':
								cnt2 = cnt + 2;

								while ((cnt2 < Length) && (LineBuf[cnt2] != 'X'))
									cnt2++;

								strcpy(str1, (LPSTR) & LineBuf[cnt + 2]);
								str1[cnt2 - cnt - 2] = 0;
								Value1 = 0.0;
								sscanf(str1, "%f", &Value1);

								if (TempGerberInfo.Units == 1)
									Value1 *= 100000.0;
								else
									Value1 *= 2540000.0;

								cnt2++;
								cnt = cnt2;

								while ((cnt2 < Length) && (LineBuf[cnt2] != 'X'))
									cnt2++;

								strcpy(str1, (LPSTR) & LineBuf[cnt]);
								str1[cnt2 - cnt] = 0;
								ValueSides = 0;

								if (strcmp(str1, "0") != 0)
									sscanf(str1, "%d", &ValueSides);

								cnt2++;
								cnt = cnt2;

								while ((cnt2 < Length) && (LineBuf[cnt2] != 'X'))
									cnt2++;

								strcpy(str1, (LPSTR) & LineBuf[cnt]);
								str1[cnt2 - cnt] = 0;
								Value2 = 0.0;

								if (str1[0] != 0)
									sscanf(str1, "%f", &Value2);

								AddAperTure(PIN_SMD_ROUND, NewAperNr, Value1, 0.0);
								break;
							}
						}
						else
						{
#ifdef _DEBUG

							if (stricmpOwn(LineBuf, "AMCOM25") == 0)
								ok = 1;

#endif
							AddAperTure(PIN_SMD_ROUND, NewAperNr, 40 * 2540.0, 0.0);
							ok = 1;
						}
					}
				}

				if ((Length > 3) && (LineBuf[1] == 'M'))
				{
#ifdef _DEBUG

					if (stricmpOwn(LineBuf, "AMCOM25") == 0)
						ok = 1;

#endif
				}

				break;

// ************************************************************************************************
// ************************************************************************************************
			case 'G':
				if (Length > 4)
				{
					strcpy(DCodeStr, (LPSTR) & LineBuf[4]);
					LineBuf[4] = 0;

					if (strcmp(LineBuf, "G54D") == 0)
					{
						if (sscanf(DCodeStr, "%d", &Temp) == 1)
						{
							if (AperNr == 0)
							{
//                  SingleQuadrantInterpolation=0;
//                  SingleQuadrantInterpolation=1;
								CircularInterpolation = 0;
								CircularInterpolationCW = 0;
							}

							AperNr = Temp;
						}
					}

					break;
				}

				if (strcmp(LineBuf, "G90") == 0)
				{
					TempGerberInfo.Incremental = 0;	// Absolute coordinates
					break;
				}

				if (strcmp(LineBuf, "G91") == 0)
				{
					TempGerberInfo.Incremental = 1;	// Incremental coordinates
					break;
				}

				if (strcmp(LineBuf, "G01") == 0)
				{
					G7XCode = 0;
//            SingleQuadrantInterpolation=0;
//            SingleQuadrantInterpolation=1;
					CircularInterpolation = 0;
					LoadExtraPoint = 0;
					break;
				}

				if (strcmp(LineBuf, "G02") == 0)
				{
					G7XCode = 0;
					CircularInterpolation = 1;
					CircularInterpolationCW = 1;
					ValueX2 = 0.0;
					ValueY2 = 0.0;
					LoadExtraPoint = 1;
					break;
				}

				if (strcmp(LineBuf, "G03") == 0)
				{
					G7XCode = 0;
					CircularInterpolation = 1;
					CircularInterpolationCW = 0;
					ValueX2 = 0.0;
					ValueY2 = 0.0;
					LoadExtraPoint = 1;
					break;
				}

				if (strcmp(LineBuf, "G74") == 0)
				{
					G7XCode = 1;
					SingleQuadrantInterpolation = 1;
					CircularInterpolation = 1;
					ValueX2 = 0.0;
					ValueY2 = 0.0;
//            LoadExtraPoint=1;
					break;
				}

				if (strcmp(LineBuf, "G75") == 0)
				{
					G7XCode = 1;
					SingleQuadrantInterpolation = 0;
					CircularInterpolation = 1;
//            LoadExtraPoint=1;
					break;
				}

				if (strcmp(LineBuf, "G36") == 0)
				{
					PolygonMode = 1;
					NrPolygonPoints = 0;
					break;
				}

				if (strcmp(LineBuf, "G37") == 0)
				{
					PolygonMode = 0;
					/*
					            if (NrPolygonPoints>0) {
					              Polygon=(PolygonRecord *)((uint8 *)NewAreaFill+sizeof(AreaFillRecord));
					              NewAreaFill->minx=Polygon->minx;
					              NewAreaFill->miny=Polygon->miny;
					              NewAreaFill->maxx=Polygon->maxx;
					              NewAreaFill->maxy=Polygon->maxy;
					              NewAreaFill->Info=GERBER_AREAFILL;
					              if ((InRange5(Polygon->Points[0].x,Polygon->Points[NrPolygonPoints-1].x))
					                 &&
					                 (InRange5(Polygon->Points[0].y,Polygon->Points[NrPolygonPoints-1].y))) {
					                NrPolygonPoints--;
					              }
					              Polygon->NrVertices=NrPolygonPoints;
					              NewAreaFill->NrVerticesStartPolygon=NrPolygonPoints;
					              if (InvertedGerber) {
					                NewAreaFill->Info|=OBJECT_POLYGON_INVERTED;
					              } else {
					                NewAreaFill->Info&=~OBJECT_POLYGON_INVERTED;
					              }
					              NewAreaFill->MemSize=sizeof(AreaFillRecord)+MemSizePolygon(Polygon);
					              AddAreaFill3(Layer,1);
					            }
					            Polygon->minx= 1000000000.0;
					            Polygon->miny= 1000000000.0;
					            Polygon->maxx=-1000000000.0;
					            Polygon->maxy=-1000000000.0;
					*/
					NrPolygonPoints = 0;
					NrPolygonPoints2 = 0;
					break;
				}

				break;

// ************************************************************************************************
// ************************************************************************************************
			case 'L':
				ok = 1;

				if (strcmp(LineBuf, "LPD") == 0)
					InvertedGerber = 0;

				if (strcmp(LineBuf, "LPC") == 0)
					InvertedGerber = 1;

				if ((Length > 2) && (LineBuf[1] == 'N'))
				{
				}

				break;

			case 'M':
				if ((Length > 4) && (LineBuf[1] == 'A') && (LineBuf[2] == 'C'))
				{
					ok = 1;
					/*
					            cnt2=NrApertureMacros-1;
					            if (NrApertureMacroItems+Length>=MaxNrAperTureMacroItems) {
					              if (AllocateMemAperTuresMacroItems(NrApertureMacroItems+Length+256*1024)!=0) {
					                return -1;
					              }
					            }
					            if (Length>1024) {
					              ok=1;
					            }
					            strcpy((LPSTR)&AperTureMacroItems[NrApertureMacroItems],
					                    (LPSTR)&LineBuf[3]);
					            if (ApertureMacros[cnt2].ApertureMacroItemsSize==0) {
					              ApertureMacros[cnt2].ApertureMacroItemStart=NrApertureMacroItems;
					            }
					            NrApertureMacroItems+=Length-3;
					            AperTureMacroItems[NrApertureMacroItems]='*';
					            NrApertureMacroItems++;
					            ApertureMacros[cnt2].ApertureMacroItemsSize+=Length-2;
					*/
				}

				break;

// ************************************************************************************************
// ************************************************************************************************
			case 'S':
				if ((Length > 4) && (LineBuf[1] == 'R'))
				{	// Step and repeat
					ok = 1;
					StepAndRepeat = 1;
					memmove(&LineBuf[0], &LineBuf[2], Length);
				}

// ************************************************************************************************
// ************************************************************************************************
			case 'X':
			case 'Y':
			case 'I':
			case 'J':
			case 'D':
				if (LineBuf[0] == 'D')
				{
					if (Length > 1)
					{
						if (sscanf((LPSTR) & LineBuf[1], "%d", &Temp) == 1)
						{
#ifdef _DEBUG

							if (Temp == 43)
								ok = 1;

#endif

							if (Temp > 9)
							{
								if (AperNr == 0)
								{
									CircularInterpolation = 0;
									CircularInterpolationCW = 0;
								}

								AperNr = Temp;
								break;
							}
						}
					}
					else
						break;
				}

				if (LineBuf[0] == 'I')
				{
					if (strcmp(LineBuf, "IPPOS") == 0)
					{
						TempGerberInfo.Invert = 0;	// Positive film
						break;
					}

					if (strcmp(LineBuf, "IPNEG") == 0)
					{
						TempGerberInfo.Invert = 1;	// Negative film
						break;
					}
				}

				cnt3++;
				ValueX = (float) LastX;
				ValueY = (float) LastY;
				StrX[0] = 0;
				StrY[0] = 0;
				StrX2[0] = 0;
				StrY2[0] = 0;
				cnt2 = 0;

				if (LineBuf[0] == 'X')
				{
					cnt2 = 1;
					cnt = 1;

					while ((cnt2 < Length) && (LineBuf[cnt2] != 'Y') && (LineBuf[cnt2] != 'D') && (LineBuf[cnt2] != 'I')
					        && (LineBuf[cnt2] != 'J'))
						cnt2++;

					strcpy(StrX, (LPSTR) & LineBuf[cnt]);
					StrX[cnt2 - cnt] = 0;
				}

				if (LineBuf[cnt2] == 'Y')
				{
					cnt2++;
					cnt = cnt2;

					while ((cnt2 < Length) && (LineBuf[cnt2] != 'D') && (LineBuf[cnt2] != 'I')
					        && (LineBuf[cnt2] != 'J'))
						cnt2++;

					strcpy(StrY, (LPSTR) & LineBuf[cnt]);
					StrY[cnt2 - cnt] = 0;
				}

				if ((LineBuf[cnt2] == 'I') || (LineBuf[cnt2] == 'J'))
				{
//          if (LoadExtraPoint)
					LoadExtraPoint = 1;
					ValueX2 = 0.0;
					ValueY2 = 0.0;

					if (LineBuf[cnt2] == 'I')
					{
						cnt2++;
						cnt = cnt2;

						while ((cnt2 < Length) && (LineBuf[cnt2] != 'J') && (LineBuf[cnt2] != 'D'))
							cnt2++;

						strcpy(StrX2, (LPSTR) & LineBuf[cnt]);
						StrX2[cnt2 - cnt] = 0;
					}

					if (LineBuf[cnt2] != 'D')
					{
						cnt2++;
						cnt = cnt2;

						while ((cnt2 < Length) && (LineBuf[cnt2] != 'D'))
							cnt2++;

						strcpy(StrY2, (LPSTR) & LineBuf[cnt]);
						StrY2[cnt2 - cnt] = 0;
					}
				}
				else
					CircularInterpolation = 0;

// ************************************************************************************************
				if (LineBuf[cnt2] == 'D')
				{
					if (cnt2 + 2 <= Length)
					{
						cnt2++;

						if (LineBuf[cnt2] == '0')
							cnt2++;

						switch (LineBuf[cnt2])
						{
						case '1':
							GerberAction = 1;
							LightActive = 1;
							break;

						case '2':
							GerberAction = 2;
							LightActive = 0;
							break;

						case '3':
							GerberAction = 3;
							break;
						}
					}
				}

				if (TempGerberInfo.NumberFormat == 0)
				{	// Normal gerber
					Length2 = strlen(StrX);

					if (Length2 > 0)
					{
						ValueXint = atol(StrX);
//              sscanf(StrX,"%d",&ValueXint);
						ValueX = (float) ValueXint;

						if ((StrX[0] == '-') || (StrX[0] == '+'))
							Length2--;

						if (TempGerberInfo.ZeroMode == 1)
						{	// Trailing zeros omitted
							if (TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 > Length2)
								ValueX *= (float) Mult10[TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 - Length2];
						}

						ValueX /= (float) Mult10[TempGerberInfo.XDigits2];
					}

					Length2 = strlen(StrY);

					if (Length2 > 0)
					{
						ValueYint = atol(StrY);
//              sscanf(StrY,"%d",&ValueYint);
						ValueY = (float) ValueYint;

						if ((StrY[0] == '-') || (StrY[0] == '+'))
							Length2--;

						if (TempGerberInfo.ZeroMode == 1)
						{	// Trailing zeros omitted
							if (TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 > Length2)
								ValueY *= (float) Mult10[TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 - Length2];
						}

						ValueY /= (float) Mult10[TempGerberInfo.YDigits2];
					}

					if (LoadExtraPoint)
					{
						Length2 = strlen(StrX2);

						if (Length2 > 0)
						{
							ValueX2 = 0.0;
							sscanf(StrX2, "%f", &ValueX2);

							if ((StrX2[0] == '-') || (StrX2[0] == '+'))
								Length2--;

							if (TempGerberInfo.ZeroMode == 1)
							{	// Trailing zeros omitted
								if (TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 > Length2)
								{
									ValueX2 *=
									    (float) Mult10[TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 - Length2];
								}
							}

							ValueX2 /= (float) Mult10[TempGerberInfo.XDigits2];
						}

						Length2 = strlen(StrY2);

						if (Length2 > 0)
						{
							ValueY2 = 0.0;
							sscanf(StrY2, "%f", &ValueY2);

							if ((StrY2[0] == '-') || (StrY2[0] == '+'))
								Length2--;

							if (TempGerberInfo.ZeroMode == 1)
							{	// Trailing zeros omitted
								if (TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 > Length2)
								{
									ValueY2 *=
									    (float) Mult10[TempGerberInfo.XDigits1 + TempGerberInfo.XDigits2 - Length2];
								}
							}

							ValueY2 /= (float) Mult10[TempGerberInfo.YDigits2];
						}
					}
				}
				else
				{	// Floating point gerber
					if (StrX[0] != 0)
					{
						ValueX = 0.0;

						if (strcmp(StrX, "0") != 0)
							sscanf(StrX, "%f", &ValueX);
					}

					if (StrY[0] != 0)
					{
						ValueY = 0.0;

						if (strcmp(StrY, "0") != 0)
							sscanf(StrY, "%f", &ValueY);
					}
				}

				if (!StepAndRepeat)
				{
					if (StrX[0] != 0)
						ValueX = (float) UnitsConvert(TempGerberInfo.Units, ValueX);

					if (StrY[0] != 0)
						ValueY = (float) UnitsConvert(TempGerberInfo.Units, ValueY);
				}

				if (LoadExtraPoint)
				{
					if (StrX2[0] != 0)
						ValueX2 = (float) UnitsConvert(TempGerberInfo.Units, ValueX2);

					if (StrY2[0] != 0)
						ValueY2 = (float) UnitsConvert(TempGerberInfo.Units, ValueY2);

					LoadExtraPoint = 0;
				}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
				if (!StepAndRepeat)
				{
					memset(&NewObject, 0, sizeof(ObjectRecord));
					NewObject.Info2 = AperNr;

					switch (GerberAction)
					{
					case 0:	// Draw line

// ************************************************************************************************
// ************************************************************************************************
						if (LightActive)
						{
							if (!InvertedGerber)
								AddLine(LastX, LastY, ValueX, ValueY, AperNr, Layer, 0);
							else
							{
//                    AddGerberObject(&NewObject,8+1);
							}
						}

						LastX = ValueX;
						LastY = ValueY;
						break;

					case 1:	// Draw line,arc,polygon coordinate

// ************************************************************************************************
// ************************************************************************************************
						if (!PolygonMode)
						{
							if ((cnt2 = GetAperTureNr(AperNr)) >= 0)
							{
								AperTureObject = &((*Objects4)[cnt2]);
								AperUsage[AperNr] |= 1;
							}
							else
							{
								AperTureObject = &DefaultApertureObject;

								if (AperUsage[AperNr] == 0)
								{
									AperUsage[AperNr] |= 2;
									sprintf(str, "D%i", AperNr);
									AddToMessageBuf(str);
								}
							}
						}

						if (CircularInterpolation)
						{
							/*
							G1X117475Y6580D2*
							G3X117975Y7080I0J500D1*
							G1X116975Y7080D2*
							G3X117475Y6580I500J0D1*
							G1X117475Y7580D2*
							*/
							NewObject.x2 = (2.0 * sqrt(SQR(ValueX2) + SQR(ValueY2)));
							x2 = NewObject.x2 * 0.5;
							NewObject.y2 = NewObject.x2;
							NewObject.ObjectType = TRACE_ARC;
							FoundCnt = -1;

							if (SingleQuadrantInterpolation)
							{	// Single quadrant
// ************************************************************************************************

								if ((NotInRange(LastX, ValueX)) || (NotInRange(LastY, ValueY)))
								{
									for (cnt2 = 0; cnt2 < 4; cnt2++)
									{
										switch (cnt2)
										{
										case 0:
											NewObject.x1 = LastX + ValueX2;
											NewObject.y1 = LastY + ValueY2;
											break;

										case 1:
											NewObject.x1 = LastX - ValueX2;
											NewObject.y1 = LastY + ValueY2;
											break;

										case 2:
											NewObject.x1 = LastX - ValueX2;
											NewObject.y1 = LastY - ValueY2;
											break;

										case 3:
											NewObject.x1 = LastX + ValueX2;
											NewObject.y1 = LastY - ValueY2;
											break;
										}

										if (CircularInterpolationCW)
										{
											NewObject.x3 = ValueX - NewObject.x1;
											NewObject.y3 = ValueY - NewObject.y1;
											NewObject.x4 = LastX - NewObject.x1;
											NewObject.y4 = LastY - NewObject.y1;
										}
										else
										{	// CCW Windows95 default
											NewObject.x3 = LastX - NewObject.x1;
											NewObject.y3 = LastY - NewObject.y1;
											NewObject.x4 = ValueX - NewObject.x1;
											NewObject.y4 = ValueY - NewObject.y1;
										}

										x1 = NewObject.x1;
										y1 = NewObject.y1;
										x3 = NewObject.x3;
										y3 = NewObject.y3;
										x4 = NewObject.x4;
										y4 = NewObject.y4;
										ConvNormalCoorToPolar(x1, y1, x1 + x3, y1 + y3, &Angle1, &LengthArc1);
										ConvNormalCoorToPolar(x1, y1, x1 + x4, y1 + y4, &Angle2, &LengthArc2);
										DiffAngle = Angle2 - Angle1;

										if (DiffAngle < 0.0)
											DiffAngle += PI * 2;

										DiffAngles[cnt2] = DiffAngle;

										if (InRange9(LengthArc1, LengthArc2))
										{
											if ((DiffAngle - 0.2 < PI / 2) && (DiffAngle + 0.2 > 0))
											{
												if (FoundCnt == -1)
													FoundCnt = cnt2;
											}
										}
									}

									if (FoundCnt != -1)
									{
										switch (FoundCnt)
										{
										case 0:
											NewObject.x1 = LastX + ValueX2;
											NewObject.y1 = LastY + ValueY2;
											break;

										case 1:
											NewObject.x1 = LastX - ValueX2;
											NewObject.y1 = LastY + ValueY2;
											break;

										case 2:
											NewObject.x1 = LastX - ValueX2;
											NewObject.y1 = LastY - ValueY2;
											break;

										case 3:
											NewObject.x1 = LastX + ValueX2;
											NewObject.y1 = LastY - ValueY2;
											break;
										}

										if (CircularInterpolationCW)
										{
											NewObject.x3 = ValueX - NewObject.x1;
											NewObject.y3 = ValueY - NewObject.y1;
											NewObject.x4 = LastX - NewObject.x1;
											NewObject.y4 = LastY - NewObject.y1;
										}
										else
										{	// CCW Windows95 default
											NewObject.x3 = LastX - NewObject.x1;
											NewObject.y3 = LastY - NewObject.y1;
											NewObject.x4 = ValueX - NewObject.x1;
											NewObject.y4 = ValueY - NewObject.y1;
										}
									}
									else
										NewObject.ObjectType = 0;
								}
								else
								{
									NewObject.ObjectType = 0;	// LineBuf
								}
							}
							else
							{	// Multiple quadrant
// ************************************************************************************************

								NewObject.x1 = LastX + ValueX2;
								NewObject.y1 = LastY + ValueY2;

								/*

								                    NewObject.x3=ValueX-NewObject.x1;
								                    NewObject.y3=ValueY-NewObject.y1;
								                    NewObject.x4=LastX-NewObject.x1;
								                    NewObject.y4=LastY-NewObject.y1;
								                    Angle1=GetArcAngle(&NewObject);
								                    NewObject.x3=LastX-NewObject.x1;
								                    NewObject.y3=LastY-NewObject.y1;
								                    NewObject.x4=ValueX-NewObject.x1;
								                    NewObject.y4=ValueY-NewObject.y1;
								                    Angle2=GetArcAngle(&NewObject);
								*/
								if (CircularInterpolationCW)
								{
									NewObject.x3 = ValueX - NewObject.x1;
									NewObject.y3 = ValueY - NewObject.y1;
									NewObject.x4 = LastX - NewObject.x1;
									NewObject.y4 = LastY - NewObject.y1;
								}
								else
								{
									NewObject.x3 = LastX - NewObject.x1;
									NewObject.y3 = LastY - NewObject.y1;
									NewObject.x4 = ValueX - NewObject.x1;
									NewObject.y4 = ValueY - NewObject.y1;
								}

								/*
								                    if ((InRangeSpecial(NewObject.x3,NewObject.x4,(2*2540.0)))
								                       &&
								                       (InRangeSpecial(NewObject.y3,NewObject.y4,(2*2540.0)))) {
								                      NewObject.ObjectType=OBJECT_CIRCLE;
								#ifdef _DEBUG
								                      if (NewObject.x2<0.5e5) {
								                        ok=1;
								                        NewObject.ObjectType=0;
								                      }
								#endif
								                      NewObject.y2=15.1;
								                    }
								*/
//                    NewObject.ObjectType=0;
							}

							if (!PolygonMode)
								NewObject.Thickness = AperTureObject->x1;

							x1 = NewObject.x1;
							y1 = NewObject.y1;
							NewObject.Layer = Layer;

							if (!PolygonMode)
							{
								if (!StepAndRepeatMode)
								{
									if (!InvertedGerber)
										AddGerberObject(&NewObject, 1);
									else
										AddGerberObject(&NewObject, 8 + 1);
								}
								else
								{
									for (CntX = 0; CntX < StepCountX; CntX++)
									{
										for (CntY = 0; CntY < StepCountY; CntY++)
										{
											NewObject.x1 = x1 + CntX * StepX;
											NewObject.y1 = y1 + CntY * StepY;

											if (!InvertedGerber)
												AddGerberObject(&NewObject, 1);
											else
												AddGerberObject(&NewObject, 8 + 1);
										}
									}
								}
							}
						}
						else
						{
// ************************************************************************************************
							if (!PolygonMode)
							{
								if (LightActive)
								{
									if (!StepAndRepeatMode)
									{
										if (!InvertedGerber)
											AddLine(LastX, LastY, ValueX, ValueY, AperNr, Layer, 0);
										else
											AddLine(LastX, LastY, ValueX, ValueY, AperNr, Layer, 2);
									}
									else
									{
										for (CntX = 0; CntX < StepCountX; CntX++)
										{
											for (CntY = 0; CntY < StepCountY; CntY++)
											{
												x1 = LastX + CntX * StepX;
												y1 = LastY + CntY * StepY;
												x2 = ValueX + CntX * StepX;
												y2 = ValueY + CntY * StepY;

												if (!InvertedGerber)
													AddLine(x1, y1, x2, y2, AperNr, Layer, 0);
												else
													AddLine(x1, y1, x2, y2, AperNr, Layer, 2);
											}
										}
									}
								}
								else
									ok = 1;
							}
						}

// ************************************************************************************************
						if (PolygonMode)
						{
							/*
							                  if (NrPolygonPoints*16+8192>MaxAreaFillMemoryTemp) {
							                    AllocateMemAreaFillMemoryTemp(MaxAreaFillMemoryTemp+128*1024);
							                    NewAreaFill=(AreaFillRecord *)AreaFillMemTemp;
							                    TempAreaFill=(AreaFillRecord *)AreaFillMemTemp2;
							                    Polygon=(PolygonRecord *)((uint8 *)NewAreaFill+sizeof(AreaFillRecord));
							                  }
							                  NewAreaFill->Layer=Layer;
							                  if (!CircularInterpolation) {
							                    if ((NrPolygonPoints==0)
							                       ||
							                       (NotInRange(Polygon->Points[NrPolygonPoints-1].x,ValueX))
							                       ||
							                       (NotInRange(Polygon->Points[NrPolygonPoints-1].y,ValueY))) {
							                      Polygon->Points[NrPolygonPoints].x=ValueX;
							                      Polygon->Points[NrPolygonPoints].y=ValueY;
							                      Polygon->minx=min(Polygon->minx,ValueX);
							                      Polygon->miny=min(Polygon->miny,ValueY);
							                      Polygon->maxx=max(Polygon->maxx,ValueX);
							                      Polygon->maxy=max(Polygon->maxy,ValueY);
							                      NrPolygonPoints++;
							                      NrPolygonPoints2++;
							                      Polygon->NrVertices=NrPolygonPoints;
							                      NewAreaFill->NrVerticesStartPolygon=NrPolygonPoints;
							                    } else {
							                      ok=1;
							                    }
							                  } else {
							#ifdef _DEBUG
							                    if (InvertedGerber) {
							                      ok=1;
							                    }
							#endif
							                    count=ArcToLineSegments2(NewObject.x1,NewObject.y1,
							                                             NewObject.x2,NewObject.y2,
							                                             NewObject.x3,NewObject.y3,
							                                             NewObject.x4,NewObject.y4,
							                                             (double *)&ArcLinesBuf,1);
							                    // ValueX,ValueY is the endpoint of the arc
							                    for (cnt2=0;cnt2<count;cnt2++) {
							                      if (CircularInterpolationCW) {
							                        x1=ArcLinesBuf[(count-1-cnt2)*2];
							                        y1=ArcLinesBuf[(count-1-cnt2)*2+1];
							                      } else {
							                        x1=ArcLinesBuf[cnt2*2];
							                        y1=ArcLinesBuf[cnt2*2+1];
							                      }
							                      ValueX=x1;
							                      ValueY=y1;
							                      if ((NrPolygonPoints==0)
							                         ||
							                         (NotInRange(Polygon->Points[NrPolygonPoints-1].x,ValueX))
							                         ||
							                         (NotInRange(Polygon->Points[NrPolygonPoints-1].y,ValueY))) {
							                        Polygon->Points[NrPolygonPoints].x=ValueX;
							                        Polygon->Points[NrPolygonPoints].y=ValueY;
							                        Polygon->minx=min(Polygon->minx,ValueX);
							                        Polygon->miny=min(Polygon->miny,ValueY);
							                        Polygon->maxx=max(Polygon->maxx,ValueX);
							                        Polygon->maxy=max(Polygon->maxy,ValueY);
							                        NrPolygonPoints++;
							                        NrPolygonPoints2++;
							                        Polygon->NrVertices=NrPolygonPoints;
							                        NewAreaFill->NrVerticesStartPolygon=NrPolygonPoints;
							                      }
							                    }
							                  }
							*/
						}

						LastX = ValueX;
						LastY = ValueY;
						break;

// ************************************************************************************************
// ************************************************************************************************
					case 2:	// Move to
						if (PolygonMode)
						{
							/*
							                  if (NrPolygonPoints>0) {
							                    Polygon=(PolygonRecord *)((uint8 *)NewAreaFill+sizeof(AreaFillRecord));
							                    NewAreaFill->minx=Polygon->minx;
							                    NewAreaFill->miny=Polygon->miny;
							                    NewAreaFill->maxx=Polygon->maxx;
							                    NewAreaFill->maxy=Polygon->maxy;
							                    NewAreaFill->Info=GERBER_AREAFILL;
							                    if ((InRange5(Polygon->Points[0].x,Polygon->Points[NrPolygonPoints-1].x))
							                       &&
							                       (InRange5(Polygon->Points[0].y,Polygon->Points[NrPolygonPoints-1].y))) {
							                      NrPolygonPoints--;
							                    }
							                    Polygon->NrVertices=NrPolygonPoints;
							                    if (InvertedGerber) {
							                      NewAreaFill->Info|=OBJECT_POLYGON_INVERTED;
							                    } else {
							                      NewAreaFill->Info&=~OBJECT_POLYGON_INVERTED;
							                    }
							                    NewAreaFill->NrVerticesStartPolygon=NrPolygonPoints;

							                    NewAreaFill->MemSize=sizeof(AreaFillRecord)+MemSizePolygon(Polygon);
							                    AddAreaFill3(Layer,1);
							                  }
							                  Polygon->minx= 1000000000.0;
							                  Polygon->miny= 1000000000.0;
							                  Polygon->maxx=-1000000000.0;
							                  Polygon->maxy=-1000000000.0;
							                  NrPolygonPoints=0;
							                  NrPolygonPoints2=0;
							                  if (NrPolygonPoints*16+8192>MaxAreaFillMemoryTemp) {
							                    AllocateMemAreaFillMemoryTemp(MaxAreaFillMemoryTemp+128*1024);
							                    NewAreaFill=(AreaFillRecord *)AreaFillMemTemp;
							                    TempAreaFill=(AreaFillRecord *)AreaFillMemTemp2;
							                    Polygon=(PolygonRecord *)((uint8 *)NewAreaFill+sizeof(AreaFillRecord));
							                  }
							                  NewAreaFill->Layer=Layer;
							*/
						}

						LastX = ValueX;
						LastY = ValueY;
						break;

// ************************************************************************************************
// ************************************************************************************************
					case 3:	// Flash
#ifdef _DEBUG
						if (AperNr == 31)
							ok = 1;

#endif

						if ((cnt2 = GetAperTureNr(AperNr)) >= 0)
						{
							AperTureObject = &((*Objects4)[cnt2]);
							AperUsage[AperNr] |= 1;
						}
						else
						{
							AperTureObject = &DefaultApertureObject;

							if (AperUsage[AperNr] == 0)
							{
								AperUsage[AperNr] |= 2;
								sprintf(str, "D%i", AperNr);
								AddToMessageBuf(str);
							}
						}

						switch (AperTureObject->ObjectType)
						{
						case PIN_SMD_ROUND:
						case PIN_SMD_RECT:
						case PIN_LINE_HOR:
						case PIN_LINE_VER:
							NewObject.x1 = ValueX;
							NewObject.y1 = ValueY;
							NewObject.x2 = AperTureObject->x1;
							NewObject.y2 = AperTureObject->y1;
							NewObject.x3 = 0.0;
							NewObject.y3 = 0.0;
							NewObject.Layer = Layer;

							switch (AperTureObject->ObjectType)
							{
							case PIN_SMD_ROUND:
								NewObject.ObjectType = PIN_SMD_ROUND;
								NewObject.y2 = 0.0;
								break;

							case PIN_SMD_RECT:
								NewObject.ObjectType = PIN_SMD_RECT;
								break;

							case PIN_LINE_HOR:
								NewObject.ObjectType = PIN_LINE_HOR;
								break;

							case PIN_LINE_VER:
								NewObject.ObjectType = PIN_LINE_VER;
								break;
							}

							if (!StepAndRepeatMode)
							{
								if (!InvertedGerber)
									AddGerberObject(&NewObject, 1);
								else
									AddGerberObject(&NewObject, 8 + 1);
							}
							else
							{
								for (CntX = 0; CntX < StepCountX; CntX++)
								{
									for (CntY = 0; CntY < StepCountY; CntY++)
									{
										NewObject.x1 = ValueX + CntX * StepX;
										NewObject.y1 = ValueY + CntY * StepY;

										if (!InvertedGerber)
											AddGerberObject(&NewObject, 1);
										else
											AddGerberObject(&NewObject, 8 + 1);
									}
								}
							}
						}

						LastX = ValueX;
						LastY = ValueY;
					}

					break;
// ************************************************************************************************
// ************************************************************************************************
				}
				else
				{
					sscanf(StrX, "%d", &StepCountX);
					sscanf(StrY, "%d", &StepCountY);
					sscanf(StrX2, "%f", &StepX);
					sscanf(StrY2, "%f", &StepY);

					if (StepX == 0)
						StepX = 1;

					if (StepY == 0)
						StepY = 1;

					StepX = (float) UnitsConvert(TempGerberInfo.Units, StepX);
					StepY = (float) UnitsConvert(TempGerberInfo.Units, StepY);

					if ((StepCountX > 1) || (StepCountY > 1))
						StepAndRepeatMode = 1;
				}

				StepAndRepeat = 0;
				break;

			default:
				break;
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
			}

			GerberObjectCount++;
		}
	}

	ok = 1;
	SetNormalCursor();

	if (MessageBufPos > 0)
	{
		strcpy(str, SC(492, "The following apertures do not exist"));
		strcat(str, " ( ");
		strcat(str, GerberFile);
		strcat(str, " ) ");
		MessageDialog(str, 0, 0);
		DeAllocateMemMessageBuf();
	}

	strcpy(InfoStr, CopyInfoStr);
	RedrawInfoStr(1);

	FileClose(fp);				// NrObjects6 NrObjects4
	ok = 1;

	DeallocateSpecialMem(MEM_APERTURE_TEXT);
	DeallocateSpecialMem(MEM_GERBER_LINEBUF);

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DrawGerberObjects(double CurrentX, double CurrentY, int32 Layer, int32 mode)
{
	int32 cnt;
	ObjectRecord *Object, NewObject;
#ifdef _DEBUG
	int32 ok;
#endif

	StartDrawingEditingWindow();
	CurrentObjectCode = -1;

	SetROP2(OutputDisplay, R2_XORPEN);

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object = &((*Objects6)[cnt]);
		memmove(&NewObject, Object, sizeof(ObjectRecord));
//    NewObject.x1+=CurrentX-CurrentX2;
//    NewObject.y1+=CurrentY-CurrentY2;
//    NewObject.x1+=CurrentX-ShiftOffsetX;
//    NewObject.y1+=CurrentY-ShiftOffsetY;
		NewObject.x1 += CurrentX - CurrentX2 - ShiftOffsetX;
		NewObject.y1 += CurrentY - CurrentY2 - ShiftOffsetY;

		if (NewObject.ObjectType == TRACE_ALL_ANGLE)
		{
			NewObject.x2 += CurrentX - CurrentX2 - ShiftOffsetX;
			NewObject.y2 += CurrentY - CurrentY2 - ShiftOffsetY;
		}

		NewObject.Layer = Layer;
#ifdef _DEBUG

		if (NewObject.ObjectType == TRACE_ALL_ANGLE)
			ok = 1;

#endif

		if (NewObject.ObjectType == PIN_SMD_ROUND)
			NewObject.ObjectType = VIA_PUT_THROUGH_ROUND;

		DrawObject(&NewObject, 0x12);
	}

	ExitDrawing();
	EndDrawingEditingWindow();
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void PlaceMovedGerberObjects(double divx, double divy, int32 Layer, int32 mode)
{
	int32 cnt;
	ObjectRecord *Object, NewObject;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object = &((*Objects6)[cnt]);
		Object->x1 += divx;
		Object->y1 += divy;
	}

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object = &((*Objects6)[cnt]);
		memmove(&NewObject, Object, sizeof(ObjectRecord));
		FillPositionObject(&NewObject);
//    Object->Info|=OBJECT_SELECTED;
		NewObject.Layer = Layer;

		if (NewObject.ObjectType == PIN_SMD_ROUND)
			NewObject.ObjectType = VIA_PUT_THROUGH_ROUND;

//    DrawObject(&NewObject,0);
	}

	RePaint();
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 MoveGerberObjects(int32 Layer, int32 mode)
{
	int32 cnt, FoundMinObject, CompPlaced, FirstShift, NewMode;
	double OldX, OldY, CurrentX, CurrentY, divx, divy, ShiftX, ShiftY, x1, y1, CursorX, CursorY, DiagValue;
	ObjectTextRecord2 TypeObject;
	ObjectRecord *Object;
	HMENU PopUpMenu;
	DrawXorFunctionRecord DrawXorFunction;

	ShiftX = 0.0;
	ShiftY = 0.0;

	CurrentX = AdjustToDrawGrid((ViewMinX + ViewMaxX) * 0.5);
	CurrentY = AdjustToDrawGrid((ViewMinY + ViewMaxY) * 0.5);
	SetNewCursor(&CurrentX, &CurrentY);

	PopUpMenu = CreatePopupMenu();
	AppendMenuUTF8(PopUpMenu, MF_ENABLED | MF_STRING, ID_ESCAPE, SC(493, "Escape"));

	DiagValue = 1e9;
	ShiftOffsetX = 0.0;
	ShiftOffsetY = 0.0;
	FoundMinObject = 0;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object = &((*Objects6)[cnt]);

		switch (Object->ObjectType)
		{
		case PIN_SMD_ROUND:
		case PIN_SMD_RECT:
			if (Object->x1 + Object->y1 < DiagValue)
			{
				ShiftOffsetX = Object->x1;
				ShiftOffsetY = Object->y1;
				DiagValue = Object->x1 + Object->y1;
			}

			FoundMinObject = 1;
			break;
		}
	}

	if (!FoundMinObject)
	{
		for (cnt = 0; cnt < NrObjects6; cnt++)
		{
			Object = &((*Objects6)[cnt]);

			switch (Object->ObjectType)
			{
			case TRACE_ALL_ANGLE:
				if (Object->x1 + Object->y1 < DiagValue)
				{
					ShiftOffsetX = Object->x1;
					ShiftOffsetY = Object->y1;
					DiagValue = Object->x1 + Object->y1;
				}
				else
				{
					if (Object->x2 + Object->y2 < DiagValue)
					{
						ShiftOffsetX = Object->x2;
						ShiftOffsetY = Object->y2;
						DiagValue = Object->x2 + Object->y2;
					}
				}

				break;

			default:
				if (Object->x1 + Object->y1 < DiagValue)
				{
					ShiftOffsetX = Object->x1;
					ShiftOffsetY = Object->y1;
					DiagValue = Object->x1 + Object->y1;
				}

				break;
			}
		}
	}

//  CentreSelectedX=CurrentX;
//  CentreSelectedY=CurrentY;

	ShiftOffsetX -= CurrentX;
	ShiftOffsetY -= CurrentY;

	CurrentX2 = CurrentX;
	CurrentY2 = CurrentY;

	RelX = ShiftOffsetX + CurrentX;
	RelY = ShiftOffsetY + CurrentY;
	/*
	  val=(int32)PixelToReal(10);
	  DrawLineWhite(RelX,RelY+val,RelX,RelY-val);
	  DrawLineWhite(RelX+val,RelY,RelX-val,RelY);
	*/
	OldX = CurrentX;
	OldY = CurrentY;

	ClipMouseCursor();
	DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
	FirstShift = 1;
	CompPlaced = 0;
	SystemBusyMode = 3;
	NewMode = 1;
	DrawXorFunction.Function5 = (FUNCP5) DrawGerberObjects;
	DrawXorFunction.Param1[0] = &OldX;
	DrawXorFunction.Param1[1] = &OldY;
	DrawXorFunction.Param1[2] = &Layer;
	DrawXorFunction.Param1[3] = &NewMode;
	DrawXorFunction.Mode = 4;
	DrawXorFunction.Param2[0] = &CurrentX;
	DrawXorFunction.Param2[1] = &CurrentY;
	DrawXorFunction.Param2[2] = &Layer;
	DrawXorFunction.Param2[3] = &NewMode;
	ZoomInOutProcessed = 0;

	while (!SelectionEsc)
	{
		if (MouseChanged)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));

			if ((OldX != CurrentX) || (OldY != CurrentY))
			{
				if (!ShiftPressed)
					DrawGerberObjects(OldX, OldY, Layer, 1);

				OldX = CurrentX;
				OldY = CurrentY;

				if (!ShiftPressed)
					DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			}

			if (MousePosX > DrawWindowMaxX - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX - ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawGerberObjects(OldX, OldY, Layer, 1);
				ScrollRight(ScrollSize);
				MousePosX -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			}

			if (MousePosY > DrawWindowMaxY - ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY - ScrollSizeDrawing + ClientStartY);
				DrawGerberObjects(OldX, OldY, Layer, 1);
				ScrollDown(ScrollSize);
				MousePosY -= ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			}

			if (MousePosX < DrawWindowMinX + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ScrollSizeDrawing + ClientStartX, MousePosY + ClientStartY);
				DrawGerberObjects(OldX, OldY, Layer, 1);
				ScrollLeft(ScrollSize);
				MousePosX += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			}

			if (MousePosY < DrawWindowMinY + ScrollEndOfWindow)
			{
				SetCursorPos(MousePosX + ClientStartX, MousePosY + ScrollSizeDrawing + ClientStartY);
				DrawGerberObjects(OldX, OldY, Layer, 1);
				ScrollUp(ScrollSize);
				MousePosY += ScrollSizeDrawing;
				CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
				CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
				OldX = CurrentX;
				OldY = CurrentY;
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			}

			DisplayCursorPosition();
			MouseChanged = 0;
		}

		/*
		    if (!ShiftPressed) {
		      if (!FirstShift) {
		        ShiftOffsetX-=ShiftX-CurrentX;
		        ShiftOffsetY-=ShiftY-CurrentY;
		        FirstShift=1;
		      }
		    } else {
		      if (FirstShift) {
		        ShiftX=CurrentX;
		        ShiftY=CurrentY;
		        FirstShift=0;
		      }
		    }
		*/
		if (!ShiftPressed)
		{
			if (!FirstShift)
			{
				DrawGerberObjects(ShiftX, ShiftY, Layer, 1);
				ShiftOffsetX -= ShiftX - CurrentX;
				ShiftOffsetY -= ShiftY - CurrentY;
				FirstShift = 1;

				CursorX = PixelToRealOffX(MousePosX);
				CursorY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
//        divx=CurrentX-CurrentX2;
//        divy=CurrentY-CurrentY2;
				divx = CurrentX - CurrentX2 - ShiftOffsetX;
				divy = CurrentY - CurrentY2 - ShiftOffsetY;
				AdjustOffsetForSnapOnGerberObject(CursorX, CursorY, CurrentX, CurrentY, divx, divy, &ShiftOffsetX,
				                                  &ShiftOffsetY, 0);
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
				DisplayCursorPosition();
			}
		}
		else
		{
			if (FirstShift)
			{
				ShiftX = CurrentX;
				ShiftY = CurrentY;
				FirstShift = 0;
			}
		}

		CheckInputMessages(0);

		if (ZoomInOutProcessed)
		{
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;
			DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			ZoomInOutProcessed = 0;
		}

		if (!Focused)
		{
			DrawGerberObjects(OldX, OldY, Layer, 1);
			CompPlaced = 1;
		}

		if ((ZoomActive()) && (!SelectionEsc))
		{
			DrawGerberObjects(OldX, OldY, Layer, 1);
			ZoomWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			else
				CompPlaced = 1;
		}

		if ((PanActive()) && (!SelectionEsc))
		{
			DrawGerberObjects(OldX, OldY, Layer, 1);
			PanWindow();
			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (!SelectionEsc)
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			else
				CompPlaced = 1;
		}

		if (CheckLeftButton())
		{
			CompPlaced = 1;
			divx = CurrentX - ShiftOffsetX;
			divy = CurrentY - ShiftOffsetY;
			DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
			CursorX = PixelToRealOffX(MousePosX);
			CursorY = PixelToRealOffY(DrawWindowMaxY - MousePosY);
			SearchMinX = CursorX - 2e5;
			SearchMinY = CursorY - 2e5;
			SearchMaxX = CursorX + 2e5;
			SearchMaxY = CursorY + 2e5;
			CopyCopperObjectsFromRectWindowToObjects4(Layer, 0);
			AdjustOffsetForSnapOnGerberObject(CursorX, CursorY, CurrentX, CurrentY, 0.0, 0.0, &ShiftOffsetX,
			                                  &ShiftOffsetY, 4 + 2);
			divx = CurrentX - CurrentX2 - ShiftOffsetX;
			divy = CurrentY - CurrentY2 - ShiftOffsetY;
			PlaceMovedGerberObjects(divx, divy, Layer, 0);
			CheckInputMessages(0);
			SelectionEsc = 1;
		}

		if (CheckRightButton2(&DrawXorFunction) == 1)
		{
//    if (RightButtonPressed) {
			TrackPopupMenu(PopUpMenu, TPM_RIGHTBUTTON, RealWindow.left + MousePosX + 5, RealWindow.top + MousePosY + 40,
			               0, PCBWindow, NULL);
			DestroyMenu(PopUpMenu);
			RightButtonPressed = 0;
			CheckInputMessages(0);
		}

		if (NrFunctionsInBuf > 0)
		{
			DrawGerberObjects(OldX, OldY, Layer, 1);
			ExecuteKeys();
			CheckInputMessages(0);
			CheckInputMessages(0);

			CurrentX = AdjustToDrawGrid(PixelToRealOffX(MousePosX));
			CurrentY = AdjustToDrawGrid(PixelToRealOffY(DrawWindowMaxY - MousePosY));
			OldX = CurrentX;
			OldY = CurrentY;

			if (SpacePressed)
			{
				memset(&TypeObject, 0, sizeof(ObjectTextRecord2));

				if (LineInputDialog(&TypeObject, SC(494, "Move objects to"), 0) == 1)
				{
					if ((NrParams = ScanParameters(-1, TypeObject.Text, 0)) >= 0)
					{
						if (NrParams == 2)
						{
							x1 = ParamsFloat[0];
							y1 = ParamsFloat[1];

							if (!ParametersRelative)
							{
//                PlaceMovedSpecialObjects(x1+CentreSelectedX,y1+CentreSelectedY,(int32)Mode);
								PlaceMovedGerberObjects(x1, y1, Layer, 0);
//  x1   -CentreSelectedX
							}
							else
							{
								PlaceMovedGerberObjects(x1 + ShiftOffsetX, y1 + ShiftOffsetY, Layer, 0);
// x1+CentreSelectedX  -CentreSelectedX
							}

							SelectionEsc = 1;
							CompPlaced = 1;
						}
					}
				}

				SpacePressed = 0;
			}

			if (HelpAsked)
			{
				if (mode == 0)
					Help("copy_traces_vias_from_clipboard.htm", 0);
				else
					Help("import_traces_vias_from_gerber.htm", 0);

				CheckInputMessages(0);

				while (!Focused)
					CheckInputMessages(0);

				CheckInputMessages(0);
				HelpAsked = 0;
			}

			FirstShift = 1;

			if (!SelectionEsc)
				DrawGerberObjects(CurrentX, CurrentY, Layer, 1);
		}
	}

	UnClipMouseCursor();
	SystemBusyMode = 0;
	SelectionEsc = 0;

	if (!CompPlaced)
	{
//    DrawGerberObjects(CurrentX,CurrentY,Layer,1);
		return -1;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 Object6ToObject5(ObjectRecord * Object6, int32 ObjectNr)
{
	ObjectRecord *Object5;

	if ((NrObjects5 + 1 >= MaxNrObjects5) && (AllocateMemObjects5(MaxNrObjects5 + 128) != 0))
		return 0;

	Object5 = &((*Objects5)[NrObjects5]);
	memmove(Object5, Object6, sizeof(ObjectRecord));
	Object5->TraceNr = ObjectNr;
	NrObjects5++;
	return 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SearchUnconnectedGerberObject(int32 * ObjectNr)
{
	int32 cnt;

	ObjectRecord *Object6;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object6 = &((*Objects6)[cnt]);

		if ((Object6->Info & (OBJECT_DONE)) == 0)
		{
			*ObjectNr = cnt;
			return 1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CollectObjectsConnectedToObject(ObjectRecord * Object6, int32 mode)
{
	int32 cnt;
	double minx, miny, maxx, maxy;
	ObjectRecord *Object6a;

	minx = Object6->minx;
	miny = Object6->miny;
	maxx = Object6->maxx;
	maxy = Object6->maxy;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object6a = &((*Objects6)[cnt]);

		if ((Object6a->Info & (OBJECT_DONE)) == 0)
		{
			if ((Object6a->maxx > minx) && (Object6a->minx < maxx) && (Object6a->maxy > miny)
			        && (Object6a->miny < maxy))
			{
				if (ObjectsConnected(Object6, Object6a))
				{
					Object6ToObject5(Object6a, cnt);
					Object6a->NetNr = OwnNetNr;
					Object6a->Info |= OBJECT_DONE;
				}
			}
		}
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 SortGerberObjects(int32 mode)
{
	int32 cnt, cnt5, ObjectNr;
	ObjectRecord *Object5, *Object6, *FoundObject6;
	int32 Stop;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object6 = &((*Objects6)[cnt]);
		Object6->Info &= ~OBJECT_DONE;
		Object6->Info |= 3;
		FillPositionObject(Object6);
	}

	Stop = 0;
	OwnNetNr = 0;
	ObjectNr = 0;

	while (SearchUnconnectedGerberObject(&ObjectNr))
	{
		NrObjects5 = 0;

		FoundObject6 = &((*Objects6)[ObjectNr]);
		Object6ToObject5(FoundObject6, ObjectNr);
//    sprintf(InfoStr,SC(320,"Netnr  %i  Nr Objects %i ( Total Objects %i )"),TotalNrNets,NrObjects3,cnt4);
//    RedrawInfoStr(0);
//    CheckForEscape();

//    DrawObject(FoundObject,0.0,0.0,8);
		FoundObject6->Info |= OBJECT_DONE;
		FoundObject6->NetNr = OwnNetNr;

		if (CollectObjectsConnectedToObject(FoundObject6, 0) == 1)
			Stop = 1;

		cnt5 = 1;

		while ((!Stop) && (cnt5 < NrObjects5))
		{
			Object5 = &((*Objects5)[cnt5]);
			ObjectNr = Object5->TraceNr;
			Object6 = &((*Objects6)[ObjectNr]);
			Object6->Info |= OBJECT_DONE;
			Object6->NetNr = OwnNetNr;

			if (CollectObjectsConnectedToObject(Object6, 0) == -1)
				Stop = 1;

			cnt5++;
		}

		OwnNetNr++;
	}

	return 0;
}

// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 ConvertGerberObjectsToTracesVias(int32 Layer, int32 mode)
{
	int32 cnt, cnt3, cnt4, cnt5, ok, ObjectNr, ObjectNr2, LocalNetNr, NetNr, Found, LoopCount;
	ObjectRecord *Object3, *Object4, *Object5, *Object6, *FoundObject6, NewObject;
	int32 Stop, AddViaPad;
	ViaRecord NewVia;
	NetRecord *Net;
	ObjectLineRecord NewObjectLine;
	ObjectArcRecord NewObjectArc;
	uint8 *HNets;

	AllocateSpecialMem(MEM_POINTS, GerberBufSize, (void **) &HNets);

	memset(HNets, 0, MaxNrNets);

	ObjectNr = 0;
	memset(&NewVia, 0, sizeof(ViaRecord));
	memset(&NewObjectLine, 0, sizeof(ObjectLineRecord));
	memset(&NewObjectArc, 0, sizeof(ObjectArcRecord));

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object6 = &((*Objects6)[cnt]);
		Object6->Info &= ~(OBJECT_DONE | OBJECT_SELECTED);
		Object6->Layer = Layer;
#ifdef _DEBUG

		if (Object6->ObjectType == PIN_SMD_RECT)
		{
			ok = 1;

			if ((InRange9(Object6->x1, 35.57e5)) && (InRange9(Object6->y1, 29.46e5)))
				ok = 1;
		}

#endif
	}

	LoopCount = 0;

	while (SearchUnconnectedGerberObject(&ObjectNr))
	{
#ifdef _DEBUG

		if (LoopCount == 82)
			ok = 1;

#endif
		FoundObject6 = &((*Objects6)[ObjectNr]);
		LocalNetNr = FoundObject6->NetNr;
		NrObjects5 = 0;

		for (cnt = 0; cnt < NrObjects6; cnt++)
		{
			Object6 = &((*Objects6)[cnt]);

			if (Object6->NetNr == LocalNetNr)
				Object6ToObject5(Object6, cnt);
		}

#ifdef _DEBUG

		for (cnt5 = 0; cnt5 < NrObjects5; cnt5++)
		{
			Object5 = &((*Objects5)[cnt5]);

			if (Object5->ObjectType == PIN_SMD_RECT)
			{
				ok = 1;

				if ((InRange9(Object5->x1, 35.57e5)) && (InRange9(Object5->y1, 29.46e5)))
					ok = 1;
			}
		}

#endif

		NetNr = -1;
		Stop = 0;
		cnt5 = 0;

		while ((!Stop) && (cnt5 < NrObjects5))
		{
			Object5 = &((*Objects5)[cnt5]);
			SearchMinX = Object5->minx;
			SearchMinY = Object5->miny;
			SearchMaxX = Object5->maxx;
			SearchMaxY = Object5->maxy;
			Found = 0;

			switch (Object5->ObjectType)
			{
			case PIN_SMD_ROUND:
			case PIN_SMD_RECT:
			case TRACE_HOR:
			case TRACE_VER:
			case TRACE_DIAG1:
			case TRACE_DIAG2:
			case TRACE_ALL_ANGLE:
			case TRACE_ARC:
				Found = CopyCopperObjectsFromRectWindowToObjects4(Layer, 0);
#ifdef _DEBUG

				if (Object5->ObjectType == PIN_SMD_RECT)
					ok = 1;

#endif

				if (Found > 0)
				{
					cnt4 = 0;

					while ((!Stop) && (cnt4 < NrObjects4))
					{
						Object4 = &((*Objects4)[cnt4]);

						if (ObjectsConnected(Object4, Object5))
						{
							Stop = 1;
							NetNr = Object4->NetNr;
						}

						cnt4++;
					}
				}

				break;

			default:
				ok = 1;
				break;
			}

			cnt5++;
		}

		if (NetNr == -1)
		{
			// Search objects on all layers based on a PIN_SMD_ROUND -> Through hole pin
			Stop = 0;
			cnt5 = 0;

			while ((!Stop) && (cnt5 < NrObjects5))
			{
				Object5 = &((*Objects5)[cnt5]);
				SearchMinX = Object5->minx;
				SearchMinY = Object5->miny;
				SearchMaxX = Object5->maxx;
				SearchMaxY = Object5->maxy;
				Found = 0;

				switch (Object5->ObjectType)
				{
				case PIN_SMD_ROUND:
					memmove(&NewObject, Object5, sizeof(ObjectRecord));
					NewObject.Layer = -1;
					NewObject.ObjectType = PIN_PUT_THROUGH_ROUND;
					Found = CopyCopperObjectsFromRectWindowToObjects4(-1, 0);

					if (Found > 0)
					{
						cnt4 = 0;

						while ((!Stop) && (cnt4 < NrObjects4))
						{
							Object4 = &((*Objects4)[cnt4]);
#ifdef _DEBUG

							if (Object4->ObjectType == TRACE_HOR)
								ok = 1;

#endif

							if (ObjectsConnected(Object4, &NewObject))
							{
								Stop = 1;
								NetNr = Object4->NetNr;
							}

							cnt4++;
						}
					}

					break;
				}

				cnt5++;
			}
		}

		if (NetNr == -1)
			ok = 1;

		if (NetNr != -1)
		{
			HNets[NetNr] = 1;

			for (cnt5 = 0; cnt5 < NrObjects5; cnt5++)
			{
				Object5 = &((*Objects5)[cnt5]);
				Object5->Info &= ~OBJECT_DONE;
			}

			GetObjectsNet((int32) NetNr, MODE_OBJECTS3, 0);
			Net = &((*Nets)[NetNr]);
#ifdef _DEBUG

			if (stricmpOwn(Net->Name, "D12") == 0)
				ok = 1;

#endif

			for (cnt5 = 0; cnt5 < NrObjects5; cnt5++)
			{
				Object5 = &((*Objects5)[cnt5]);

				switch (Object5->ObjectType)
				{
				case PIN_SMD_ROUND:
					AddViaPad = 1;

					for (cnt3 = 0; cnt3 < NrObjects3; cnt3++)
					{
						Object3 = &((*Objects3)[cnt3]);

						switch (Object3->ObjectType)
						{
						case PIN_SMD_ROUND:
						case PIN_SMD_RECT:
						case PIN_LINE_HOR:
						case PIN_LINE_VER:
						case PIN_LINE_DIAG1:
						case PIN_LINE_DIAG2:
						case PIN_SMD_POLYGON:
							if ((Object3->Layer == Layer) && (ObjectsConnected(Object3, Object5)))
							{
								// Místo pro nový kruhový blok je již obsazeno
								// pomocí kruhové podložky -> nepøidávat
								Object5->Info |= OBJECT_DONE;
								AddViaPad = 0;
							}

							break;

						case PIN_PUT_THROUGH_ROUND:
						case VIA_PUT_THROUGH_ROUND:
						case DRILL:
						case PIN_PUT_THROUGH_POLYGON:
						case PIN_PUT_THROUGH_SQUARE:
							if (ObjectsConnected(Object3, Object5))
							{
								// Place for the new circle pad is already occupied
								// by a circle pad -> Do not add
								Object5->Info |= OBJECT_DONE;
								AddViaPad = 0;
							}

							break;
						}
					}

					if (AddViaPad)
					{
						memmove(&NewVia, &CurrentVia, sizeof(ViaRecord));
						NewVia.X = (float) Object5->x1;
						NewVia.Y = (float) Object5->y1;
						NewVia.NetNr = (int16) NetNr;
						NewVia.Info = 0;
						NewVia.Layer = -1;
						AddVia(&NewVia);
					}

					break;
				}
			}

			for (cnt5 = 0; cnt5 < NrObjects5; cnt5++)
			{
				Object5 = &((*Objects5)[cnt5]);
				memmove(&NewObject, Object5, sizeof(ObjectRecord));

				if ((Object5->Info & OBJECT_DONE) == 0)
				{
					switch (Object5->ObjectType)
					{
					/*
					            case PIN_SMD_ROUND:
					              break;
					            case PIN_SMD_RECT:
					              break;
					*/
					case TRACE_HOR:
					case TRACE_VER:
					case TRACE_DIAG1:
					case TRACE_DIAG2:
//              Object5->y2=Net->TraceWidth;
						NewObject.Clearance = Net->TraceClearance;
						NewObject.NetNr = (int16) NetNr;
						NewObject.Layer = Layer;
						AddTrace(&NewObject);
						Object5->Info |= OBJECT_DONE;
						break;

					case TRACE_ALL_ANGLE:
						NewObject.Thickness = Net->TraceWidth;
						NewObject.Clearance = Net->TraceClearance;
						NewObject.NetNr = (int16) NetNr;
						NewObject.Layer = Layer;
						AddTrace(&NewObject);
						Object5->Info |= OBJECT_DONE;
						break;

					case TRACE_ARC:
						NewObject.Thickness = (float) Net->TraceWidth;
						NewObject.Clearance = Net->TraceClearance;
						NewObject.NetNr = (int16) NetNr;
						NewObject.Layer = Layer;
						AddTrace(&NewObject);
						Object5->Info |= OBJECT_DONE;
						break;

					case DRILL:
						NewObjectArc.CentreX = (float) Object5->x1;
						NewObjectArc.CentreY = (float) Object5->y1;
						NewObjectArc.Info &= ~OBJECT_FILLED;
						NewObjectArc.Width = (float) Object5->x2;
						NewObjectArc.Height = 0.0;
						NewObjectArc.StartDiffX = 0.0;
						NewObjectArc.StartDiffY = 0.0;
						NewObjectArc.EndDiffX = 0.0;
						NewObjectArc.EndDiffY = 0.0;
						NewObjectArc.Clearance = Net->TraceClearance;
						NewObjectArc.NetNr = (int16) NetNr;
						NewObjectArc.LineThickNess = Net->TraceWidth;
						NewObjectArc.Layer = Layer;
						AddObjectArc(&NewObjectArc);
						Object5->Info |= OBJECT_DONE;
						break;
					}
				}
			}
		}
		else
		{
			if (NrObjects5 > 1)
				ok = 1;
		}

		for (cnt5 = 0; cnt5 < NrObjects5; cnt5++)
		{
			Object5 = &((*Objects5)[cnt5]);
			ObjectNr2 = Object5->TraceNr;
			Object6 = &((*Objects6)[ObjectNr2]);
#ifdef _DEBUG

			if (Object6->ObjectType == PIN_SMD_RECT)
			{
				ok = 1;

				if ((InRange9(Object6->x1, 35.57e5)) && (InRange9(Object6->y1, 29.46e5)))
					ok = 1;
			}

#endif
			Object6->Info |= OBJECT_DONE;

			if (NetNr == -1)
				Object6->Info |= OBJECT_SELECTED;
		}

		LoopCount++;
	}

	/*
	  StartDrawingEditingWindow();
	  SetROP2(OutputDisplay,SelectColorMode);
	  for (cnt=0;cnt<NrObjects6;cnt++) {
	    Object6=&((*Objects6)[cnt]);
	    if ((Object6->Info & OBJECT_SELECTED)!=0) {
	      DrawObject(Object6,0);
	    }
	  }
	  ExitDrawing();
	  EndDrawingEditingWindow();
	*/
	LastActionNr++;

	for (cnt = 0; cnt < Design.NrNets; cnt++)
	{
		if (HNets[cnt] == 1)
		{
			ReCalcConnectionsNet((int32) cnt, 0, 1);
			CheckForEscape();
			SetWaitCursor();
		}
	}

	SetNormalCursor();
	RePaint();
	return 0;
}

//***********************************************************************************************
//******************************* Importovat gerber soubor **************************************
//***********************************************************************************************

int32 ImportTracesVias(int32 mode)
{
	int32 cnt, Layer;
	ObjectRecord *Object;
	char str[MAX_LENGTH_STRING];

	if ((mode & 1) == 0)
	{	// Import traces/vias from gerber file
		Layer = SelectLayer(0);

		if (Layer == -1)
			return -1;

		if (GerberFileName[0] == 0)
			strcpy(GerberFileName, DesignPath);

		/*
		   strcpy(GerberFileName,"c:\\pcb_elegance\\cdio\\pcb\\Top3.gbr");
		 */

		if (GetNewFileUTF8
		        (PCBWindow, NULL, GerberFileName, ExportDir, SC(495, "Gerber file"), NULL, SC(391, "Import gerber file"),
		         "*", 0))
			return -1;

//    if (LoadNewFile3(GerberFileName,SC(495,"Gerber file"),SC(391,"Import gerber file"),0,0)!=0)
		if (!CheckFileIsGerber(GerberFileName))
		{
			sprintf(str, SC(497, "File %s is not a gerber file"), GerberFileName);
			MessageBoxOwn(PCBWindow, str, SC(24, "Error"), MB_APPLMODAL | MB_OK);
			DeallocateSpecialMem(MEM_APERTURE_LINES);
			DeallocateSpecialMem(MEM_APERTURE_TEXT);
			return -1;
		}

		if (LoadGerberFile(GerberFileName, Layer, 0) < 0)
		{
			DeallocateSpecialMem(MEM_APERTURE_LINES);
			DeallocateSpecialMem(MEM_APERTURE_TEXT);
			return -1;
		}
	}
	else
		Layer = CurrentDrawingLayer;

	if (Layer < Design.NrBoardLayers)
	{
		SortGerberObjects(0);

		if (MoveGerberObjects(Layer, mode) == 0)
			ConvertGerberObjectsToTracesVias(Layer, 0);

		DeallocateSpecialMem(MEM_APERTURE_LINES);
		DeallocateSpecialMem(MEM_APERTURE_TEXT);
		return 0;
	}

	/*
	  StartDrawingEditingWindow();
	  for (cnt=0;cnt<NrObjects3;cnt++) {
	    Object=&((*Objects3)[cnt]);
	//    Object->Info|=OBJECT_SELECTED;
	    if (Object->ObjectType==TRACE_DIAG2) {
	      ok=1;
	    }
	    DrawObject(Object,0);
	  }
	  ExitDrawing();
	  EndDrawingEditingWindow();
	*/
	PostMessage(PCBWindow, WM_COMMAND, (WPARAM) ID_ACTION_OBJECTS, (LPARAM) NULL);
	CheckInputMessages(0);

	memset(&NewObjectLine, 0, sizeof(NewObjectLine));
	memset(&NewObjectArc, 0, sizeof(NewObjectArc));
	memset(&NewObjectRect, 0, sizeof(NewObjectRect));
	NewObjectLine.Layer = Layer;
	NewObjectArc.Layer = Layer;
	NewObjectRect.Layer = Layer;
	NewObjectLine.Info = OBJECT_SELECTED | 3;
	NewObjectArc.Info = OBJECT_SELECTED;
	NewObjectRect.Info = OBJECT_SELECTED;

	for (cnt = 0; cnt < NrObjects6; cnt++)
	{
		Object = &((*Objects6)[cnt]);

		switch (Object->ObjectType)
		{
		case PIN_SMD_ROUND:
			NewObjectArc.CentreX = (float) Object->x1;
			NewObjectArc.CentreY = (float) Object->y1;
			NewObjectArc.Info |= OBJECT_FILLED;
			NewObjectArc.Width = (float) Object->x2;
			NewObjectArc.Height = (float) Object->x2;
			NewObjectArc.NetNr = (int16) Object->NetNr;
			NewObjectArc.Clearance = Design.StandardClearance;
			AddObjectArc(&NewObjectArc);
			break;

		case PIN_SMD_RECT:
			NewObjectRect.CentreX = (float) Object->x1;
			NewObjectRect.CentreY = (float) Object->y1;
			NewObjectRect.Info |= OBJECT_FILLED;
			NewObjectRect.Width = (float) Object->x2;
			NewObjectRect.Height = (float) Object->y2;
			NewObjectRect.NetNr = (int16) Object->NetNr;
			NewObjectRect.Clearance = Design.StandardClearance;
			AddObjectRect(&NewObjectRect);
			break;

		case PIN_LINE_HOR:
			NewObjectLine.X1 = (float) (Object->x1 - (Object->x2 - Object->y2) * 0.5);
			NewObjectLine.Y1 = (float) Object->y1;
			NewObjectLine.X2 = (float) (Object->x1 + (Object->x2 - Object->y2) * 0.5);
			NewObjectLine.Y2 = (float) Object->y1;
			NewObjectLine.LineThickNess = (float) Object->y2;
			NewObjectLine.Info &= ~OBJECT_FILLED;
			NewObjectLine.NetNr = (int16) Object->NetNr;
			NewObjectLine.Clearance = Design.StandardClearance;
			AddObjectLine(&NewObjectLine);
			break;

		case PIN_LINE_VER:
			NewObjectLine.X1 = (float) Object->x1;
			NewObjectLine.Y1 = (float) (Object->y1 - (Object->y2 - Object->x2) * 0.5);
			NewObjectLine.X2 = (float) Object->x1;
			NewObjectLine.Y2 = (float) (Object->y1 + (Object->y2 - Object->x2) * 0.5);
			NewObjectLine.LineThickNess = (float) Object->x2;
			NewObjectLine.Info &= ~OBJECT_FILLED;
			NewObjectLine.NetNr = (int16) Object->NetNr;
			NewObjectLine.Clearance = Design.StandardClearance;
			AddObjectLine(&NewObjectLine);
			break;

		case TRACE_VER:
			NewObjectLine.X1 = (float) Object->x1;
			NewObjectLine.Y1 = (float) Object->y1;
			NewObjectLine.X2 = (float) Object->x1;
			NewObjectLine.Y2 = (float) (Object->y1 + Object->x2);
			NewObjectLine.LineThickNess = (float) Object->y2;
			NewObjectLine.Info &= ~OBJECT_FILLED;
			NewObjectLine.NetNr = (int16) Object->NetNr;
			NewObjectLine.Clearance = Design.StandardClearance;
			AddObjectLine(&NewObjectLine);
			break;

		case TRACE_HOR:
			NewObjectLine.X1 = (float) Object->x1;
			NewObjectLine.Y1 = (float) Object->y1;
			NewObjectLine.X2 = (float) (Object->x1 + Object->x2);
			NewObjectLine.Y2 = (float) Object->y1;
			NewObjectLine.LineThickNess = (float) Object->y2;
			NewObjectLine.Info &= ~OBJECT_FILLED;
			NewObjectLine.NetNr = (int16) Object->NetNr;
			NewObjectLine.Clearance = Design.StandardClearance;
			AddObjectLine(&NewObjectLine);
			break;

		case TRACE_DIAG1:
			NewObjectLine.X1 = (float) Object->x1;
			NewObjectLine.Y1 = (float) Object->y1;
			NewObjectLine.X2 = (float) (Object->x1 + Object->x2);
			NewObjectLine.Y2 = (float) (Object->y1 - Object->x2);
			NewObjectLine.LineThickNess = (float) Object->y2;
			NewObjectLine.Info &= ~OBJECT_FILLED;
			NewObjectLine.NetNr = (int16) Object->NetNr;
			NewObjectLine.Clearance = Design.StandardClearance;
			AddObjectLine(&NewObjectLine);
			break;

		case TRACE_DIAG2:
			NewObjectLine.X1 = (float) Object->x1;
			NewObjectLine.Y1 = (float) Object->y1;
			NewObjectLine.X2 = (float) (Object->x1 + Object->x2);
			NewObjectLine.Y2 = (float) (Object->y1 + Object->x2);
			NewObjectLine.LineThickNess = (float) Object->y2;
			NewObjectLine.Info &= ~OBJECT_FILLED;
			NewObjectLine.NetNr = (int16) Object->NetNr;
			NewObjectLine.Clearance = Design.StandardClearance;
			AddObjectLine(&NewObjectLine);
			break;

		case TRACE_ALL_ANGLE:
			NewObjectLine.X1 = (float) Object->x1;
			NewObjectLine.Y1 = (float) Object->y1;
			NewObjectLine.X2 = (float) Object->x2;
			NewObjectLine.Y2 = (float) Object->y2;
			NewObjectLine.LineThickNess = (float) Object->Thickness;
			NewObjectLine.Info &= ~OBJECT_FILLED;
			NewObjectLine.NetNr = (int16) Object->NetNr;
			NewObjectLine.Clearance = Design.StandardClearance;
			AddObjectLine(&NewObjectLine);
			break;

		case OBJECT_ARC:
		case TRACE_ARC:
			NewObjectArc.CentreX = (float) Object->x1;
			NewObjectArc.CentreY = (float) Object->y1;
			NewObjectArc.Info &= ~OBJECT_FILLED;
			NewObjectArc.Width = (float) Object->x2;
			NewObjectArc.Height = (float) Object->x2;
			NewObjectArc.StartDiffX = (float) Object->x3;
			NewObjectArc.StartDiffY = (float) Object->y3;
			NewObjectArc.EndDiffX = (float) Object->x4;
			NewObjectArc.EndDiffY = (float) Object->y4;
			NewObjectArc.LineThickNess = (float) Object->Thickness;
			NewObjectArc.Clearance = (float) Design.StandardClearance;
			NewObjectArc.NetNr = (int16) Object->NetNr;
			AddObjectArc(&NewObjectArc);
			break;

		case DRILL:
			NewObjectArc.CentreX = (float) Object->x1;
			NewObjectArc.CentreY = (float) Object->y1;
			NewObjectArc.Info &= ~OBJECT_FILLED;
			NewObjectArc.Width = (float) Object->x2;
			NewObjectArc.Height = 0.0;
			NewObjectArc.StartDiffX = 0.0;
			NewObjectArc.StartDiffY = 0.0;
			NewObjectArc.EndDiffX = 0.0;
			NewObjectArc.EndDiffY = 0.0;
			NewObjectArc.Clearance = Design.StandardClearance;
			NewObjectArc.NetNr = (int16) Object->NetNr;
			AddObjectArc(&NewObjectArc);
			break;
		}
	}

	DeallocateSpecialMem(MEM_APERTURE_LINES);
	DeallocateSpecialMem(MEM_APERTURE_TEXT);
	RePaint();
	return 0;
}


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
