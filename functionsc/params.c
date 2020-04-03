/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: params.c
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
#include "params.h"
#include "utf8.h"

#define MAX_LENGTH_OWN_COMMAND_LINE    4096

ParameterRecord Parameters[MAX_PARAMETERS];
int32 NrParams;

ALIGN32PRE char OwnCommandLine[MAX_LENGTH_OWN_COMMAND_LINE] ALIGN32POST;

// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************
// **************************************************************************************************


int32 GetParameters(LPSTR CommandLine)
{
	int32 cnt, cnt2, cnt3, lengte, LengthString, OptionFound, mode, result;
	WCHAR ExecutableW[MAX_PARAM_STRING_LENGTH], ExecutableW2[MAX_PARAM_STRING_LENGTH], *FileNameW;

	result = 0;
	mode = 0;
	/*

	  strcpy(str,GetCommandLine());

	ParameterRecord             Parameters[MAX_PARAMETERS];
	          char Option[200];
	          char Parameter[500];
	*/
	NrParams = 0;
	cnt = 0;
	OptionFound = 0;

	memset((uint8 *) & Parameters, 0, sizeof(ParameterRecord) * MAX_PARAMETERS);

	if (!CommandLine)
	{
		mode = 1;

		if (UnicodeToUtf8(GetCommandLineW(), OwnCommandLine, MAX_LENGTH_OWN_COMMAND_LINE) == 0)
			return -2;

		CommandLine = OwnCommandLine;
	}

	lengte = strlen(CommandLine);

	while (cnt < lengte)
	{
		cnt2 = cnt;

		while ((CommandLine[cnt2] == ' ') && (cnt2 < lengte))
			cnt2++;

		if (cnt2 < lengte)
		{
// ********************************************************************************************************
			if (CommandLine[cnt2] == '\"')
			{
				cnt3 = cnt2 + 1;

				while ((CommandLine[cnt3] != '\"') && (cnt3 < lengte))
					cnt3++;

				if (cnt3 < lengte)
					LengthString = cnt3 - cnt2 - 1;
				else
				{
					// Last quoted string found
					LengthString = strlen((LPSTR) & CommandLine[cnt2 + 1]);
				}

				cnt = cnt3;

				if (LengthString > 0)
				{
					if (LengthString > MAX_PARAM_STRING_LENGTH - 1)
						LengthString = MAX_PARAM_STRING_LENGTH - 1;

					strncpy(Parameters[NrParams].Parameter, (LPSTR) & CommandLine[cnt2 + 1], LengthString);
					Parameters[NrParams].Parameter[LengthString] = 0;

					if (OptionFound == 0)
						Parameters[NrParams].Option[0] = 0;

					OptionFound = 0;

					if (cnt3 < lengte)
					{
						NrParams++;

						if (NrParams == MAX_PARAMETERS)
						{
							result = -1;
							break;
						}
					}
					else
					{
						NrParams++;
						break;
					}
				}
			}
			else
			{
// ********************************************************************************************************
				if ((CommandLine[cnt2] == '/') || (CommandLine[cnt2] == '-'))
				{
					// Found an option
					if (OptionFound)
					{
						// Found a previous option -> Add to the parameters
						Parameters[NrParams].Parameter[0] = 0;
						NrParams++;
						OptionFound = 0;

						if (NrParams == MAX_PARAMETERS)
							return -1;
					}

					if (cnt2 + 1 < lengte)
					{
						cnt3 = cnt2 + 1;

						while ((CommandLine[cnt3] != ' ') && (cnt3 < lengte))
							cnt3++;

						LengthString = cnt3 - cnt2 - 1;

						if (LengthString > 0)
						{
							if (LengthString > MAX_OPTION_STRING_LENGTH - 1)
								LengthString = MAX_OPTION_STRING_LENGTH - 1;

							strncpy(Parameters[NrParams].Option, (LPSTR) & CommandLine[cnt2 + 1], LengthString);
							Parameters[NrParams].Parameter[0] = 0;

							if (cnt3 >= lengte)
							{
								Parameters[NrParams].Parameter[0] = 0;
								NrParams++;
								break;
							}

							OptionFound = 1;
						}

						cnt = cnt3;
					}
					else
						break;
				}
				else
				{
// ********************************************************************************************************
					// Found a non space character -> start of non option parameter
					cnt3 = cnt2 + 1;

					while ((CommandLine[cnt3] != ' ') && (cnt3 < lengte))
						cnt3++;

					if (cnt3 < lengte)
						LengthString = cnt3 - cnt2;
					else
					{
						// Last quoted string found
						LengthString = strlen((LPSTR) & CommandLine[cnt2]);
					}

					cnt = cnt3;

					if (LengthString > MAX_PARAM_STRING_LENGTH - 1)
						LengthString = MAX_PARAM_STRING_LENGTH - 1;

					strncpy(Parameters[NrParams].Parameter, (LPSTR) & CommandLine[cnt2], LengthString);
					Parameters[NrParams].Parameter[LengthString] = 0;

					if (OptionFound == 0)
						Parameters[NrParams].Option[0] = 0;

					OptionFound = 0;
					NrParams++;

					if (NrParams == MAX_PARAMETERS)
					{
						result = -1;
						break;
					}

					if (cnt3 >= lengte)
						break;

				}
			}
		}

		cnt++;
	}

	if (Parameters[0].Parameter[0] != 0)
	{
		if (mode == 1)
		{
			if (Utf8ToUnicode(Parameters[0].Parameter, ExecutableW, MAX_PARAM_STRING_LENGTH) == 0)
				return -2;

			GetFullPathNameW(ExecutableW, MAX_PARAM_STRING_LENGTH, ExecutableW2, &FileNameW);
			GetLongPathNameW(ExecutableW2, ExecutableW2, MAX_PARAM_STRING_LENGTH);

			if (UnicodeToUtf8(ExecutableW2, Parameters[0].Parameter, MAX_PARAM_STRING_LENGTH) == 0)
				return -2;
		}
	}

	return result;
}
