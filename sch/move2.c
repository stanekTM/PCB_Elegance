/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: move2.c
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
#include "calc.h"
#include "menus.h"
#include "sch.h"
#include "calcdef.h"
#include "graphics.h"
#include "toets.h"
#include "mainloop.h"
#include "draw3.h"
#include "draw2.h"
#include "draw.h"
#include "select.h"
#include "line2.h"
#include "insdel.h"
#include "movecomp.h"
#include "resource.h"
#include "help.h"
#include "stdio.h"
#include "dialogs.h"
#include "property.h"
#include "ctype.h"

extern ProjectInfoRecord *ProjectInfo;


// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************
// ************************************************************************************************

int32 EditLayoutComponent(int32 mode)
{
	int32 cnt, cnt2, len;
	HWND DesignWindow;
	char str[200];
	InstanceRecord *Instance;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if ((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == (OBJECT_SELECTED))
		{
			DesignWindow = FindWindow("DESIGN", NULL);

			if (ProjectInfo)
			{
				if (DesignWindow)
				{
					memset(&ProjectInfo->TempStr1, 0, sizeof(ProjectInfo->TempStr1));
					strcpy(str, Instance->Reference);
					len = strlen(str);

					if ((len > 0) && (str[len - 1] != '?'))
					{
						if (isdigit(str[len - 1]))
						{
							strncpy(ProjectInfo->TempStr1, str, sizeof(ProjectInfo->TempStr1) - 1);
							SendMessage(DesignWindow, WM_COMMAND, ID_LAYOUT_OPEN_REF, 1);
						}
						else
						{
							cnt2 = len - 1;

							while ((cnt2 > 0) && (isalpha(str[cnt2])))
								cnt2--;

							if (isdigit(str[cnt2]))
							{
								str[cnt2 + 1] = 0;
								strncpy(ProjectInfo->TempStr1, str, sizeof(ProjectInfo->TempStr1) - 1);
								SendMessage(DesignWindow, WM_COMMAND, ID_LAYOUT_OPEN_REF, 1);
							}
						}
					}
					else
					{
					}
				}
			}

			return 0;
		}
	}

	return 0;
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
