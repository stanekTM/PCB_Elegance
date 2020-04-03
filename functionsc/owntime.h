/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: owntime.h
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


#ifndef _OWNTIME

#define _OWNTIME

#include "owntypes.h"
#include "windows.h"

void SetTimer0(void);

int32 CheckTimeOutTimer0(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer0inMilliSeconds(void);

int32 GetDifferenceTimer0inMicroSeconds(void);

void SetTimer1(void);

int32 CheckTimeOutTimer1(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer1inMilliSeconds(void);

int32 GetDifferenceTimer1inMicroSeconds(void);

void SetTimer2(void);

int32 CheckTimeOutTimer2(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer2inMilliSeconds(void);

int32 GetDifferenceTimer2inMicroSeconds(void);

void SetTimer3(void);

int32 CheckTimeOutTimer3(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer3inMilliSeconds(void);

int32 GetDifferenceTimer3inMicroSeconds(void);

void SetTimer4(void);

int32 CheckTimeOutTimer4(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer4inMilliSeconds(void);

int32 GetDifferenceTimer4inMicroSeconds(void);

void SetTimer5(void);

int32 CheckTimeOutTimer5(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer5inMilliSeconds(void);

int32 GetDifferenceTimer5inMicroSeconds(void);

void SetTimer6(void);

int32 CheckTimeOutTimer6(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer6inMilliSeconds(void);

int32 GetDifferenceTimer6inMicroSeconds(void);

void SetTimer7(void);

int32 CheckTimeOutTimer7(int32 ulNrMilliSeconds);

int32 GetDifferenceTimer7inMilliSeconds(void);

int32 GetDifferenceTimer7inMicroSeconds(void);

void InitTimers(void);

void AddPerformanceValue(LPSTR String);

void AddPerformanceValue2(LPSTR String);

void WritePerformanceStrings(void);

void ResetPerformanceStrings();

#endif
