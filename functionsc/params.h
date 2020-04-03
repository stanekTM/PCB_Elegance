/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: params.h
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


#ifndef _PARAMS_H

#define _PARAMS_H

#include "owntypes.h"

#define MAX_PARAMETERS             20

#define MAX_OPTION_STRING_LENGTH   200
#define MAX_PARAM_STRING_LENGTH    400

typedef struct
{
	char Option[MAX_OPTION_STRING_LENGTH];
	char Parameter[MAX_PARAM_STRING_LENGTH];
} ParameterRecord;


extern ParameterRecord Parameters[MAX_PARAMETERS];
extern int32 NrParams;

int32 GetParameters(LPSTR CommandLine);

#endif // _PARAMS_H
