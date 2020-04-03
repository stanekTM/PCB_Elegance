/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: help.c
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


#include "help.h"
#include "stdio.h"
#include "memory.h"
#include "htmlhelp.h"

void Help(LPSTR Topic, int32 mode)
{
	char str[400];

	sprintf(str, "%s\\design.chm::/html/%s", ExePath, Topic);

	switch (mode)
	{
	case 0:
		HtmlHelp(DESIGNWindow, (LPCTSTR) str, HH_DISPLAY_TOPIC, 0);
		break;

	case 1:
		break;

	case 2:
		HtmlHelp(0, 0, HH_CLOSE_ALL, 0);
		break;

	case 3:
		HtmlHelp(DESIGNWindow, (LPCTSTR) str, HH_HELP_CONTEXT, (uint32) Topic);
		break;
	}
}
