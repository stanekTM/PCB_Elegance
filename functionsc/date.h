/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: date.h
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


#ifndef _DATE

#define _DATE

#include  "owntypes.h"
#include  "windows.h"

int32 GetDayOfWeek(int32 Day, int32 Month, int32 Year);

int32 GetDayOfYear(int32 Day, int32 Month, int32 Year);

int32 GateDateOfDayOfYear(int32 DayOfYear, int32 Year, int32 * Day, int32 * Month);


int32 NextDays(int32 Day, int32 Month, int32 Year, int32 * NextDay, int32 * NextMonth, int32 * NextYear, int32 NrDays);


int32 DaysAfter1900FromFileName(LPSTR FileName);

uint32 SecondsAfter1970(uint32 year, uint32 mon, uint32 day, uint32 hour, uint32 min, uint32 sec);

int32 ConvertSecondsAfter1970(uint32 SecondsAfter1970, int32 * year, int32 * month, int32 * day, int32 * hour,
                              int32 * minutes, int32 * seconds);

uint32 GetCurrentSecondsAfter1970(void);

#endif
