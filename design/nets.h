/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: nets.h
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


#ifndef _NETS

#define _NETS

#include "types.h"

int32 nets1(int32 SheetNr, double *x, double *y);

int32 ObjectTextToBuf(LPSTR ObjectText);

#ifdef GCC_COMP

static inline LPSTR GetObjectText(int32 Pos)
{
	char *TextPos;

	TextPos = ObjectTextBuf + Pos;
	return TextPos;

}

#else

__inline LPSTR GetObjectText(int32 Pos)
{
	char *TextPos;

	TextPos = ObjectTextBuf + Pos;
	return TextPos;

}

#endif

int32 CheckNets(int32 SheetNr, int32 mode, double *x, double *y);

#endif
