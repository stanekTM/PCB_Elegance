/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: date.c
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
#include "string.h"
#include "commdlg.h"
#include "io.h"
#include "time.h"
#include "fcntl.h"
#include "errno.h"
#include "sys/stat.h"
#include "date.h"


char MonthStr[12][20] = { "January",
                          "February",
                          "March",
                          "April",
                          "May",
                          "June",
                          "July",
                          "August",
                          "September",
                          "October",
                          "November",
                          "December"
                        };
char MonthStr2[12][20] = { "Januari",
                           "Februari",
                           "Maart",
                           "April",
                           "Mei",
                           "Juni",
                           "Juli",
                           "Augustus",
                           "September",
                           "Oktober",
                           "November",
                           "December"
                         };

char WeekDag[7][20] = { "Zondag",
                        "Maandag",
                        "Dinsdag",
                        "Woensdag",
                        "Donderdag",
                        "Vrijdag",
                        "Zaterdag"
                      };
char WeekDay[7][20] = { "Sunday",
                        "Monday",
                        "Tuesday",
                        "Wednesday",
                        "Thursday",
                        "Friday",
                        "Saturday"
                      };

int32 MonthDays[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
int32 Years[28] =
{ 5, 7, 8, 9, 10, 12, 13, 14, 15, 17, 18, 19, 20, 22, 23, 24, 25, 27, 28, 29, 30, 32, 33, 34, 35, 37, 38, 39 };




// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetDayOfWeek(int32 Day, int32 Month, int32 Year)
{
//  0  = Sunday
//  6  = Saterday


	int32 DayNr, hulp, cnt;

	if (Day < 1)
		return -1;

	if (Month < 1)
		return -1;

	if (Month > 12)
		return -1;

	if (((Year % 4) == 0) && (Month == 2))
	{
		if (Day > 29)
			return -1;
	}
	else
	{
		if (Day > MonthDays[Month - 1])
			return -1;
	}

	hulp = 0;

	if (Month > 1)
	{
		for (cnt = 1; cnt < Month; cnt++)
			hulp += MonthDays[cnt - 1];

		if (((Year % 4) == 0) && (Month > 2))
			hulp++;
	}

	hulp += Day - 1;
	DayNr = ((Years[Year % 28] + hulp) % 7);

	return DayNr;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 GetDayOfYear(int32 Day, int32 Month, int32 Year)
{
//  0    is first of january
//  364  is last of december when not special year (2003)
//  365  is last of december when a special year (2000)


	int32 hulp, cnt;

	if (Day < 1)
		return -1;

	if (Month < 1)
		return -1;

	if (Month > 12)
		return -1;

	if (((Year % 4) == 0) && (Month == 2))
	{
		if (Day > 29)
			return -1;
	}
	else
	{
		if (Day > MonthDays[Month - 1])
			return -1;
	}

	hulp = 0;

	if (Month > 1)
	{
		for (cnt = 1; cnt < Month; cnt++)
			hulp += MonthDays[cnt - 1];

		if (((Year % 4) == 0) && (Month > 2))
			hulp++;
	}

	hulp += Day - 1;
	return hulp;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 GetDateOfDayOfYear(int32 DayOfYear, int32 Year, int32 * Day, int32 * Month)
{
	int32 hulp, cnt, DaysOfMonth;

	if (DayOfYear < 0)
		return -1;

	hulp = DayOfYear;

	for (cnt = 0; cnt < 12; cnt++)
	{
		DaysOfMonth = MonthDays[cnt];

		if (((Year % 4) == 0) && (cnt + 1 == 2))
			DaysOfMonth++;

		if (hulp < DaysOfMonth)
		{
			*Day = hulp + 1;
			*Month = cnt + 1;
			return 0;
		}
		else
			hulp -= DaysOfMonth;
	}

	return -1;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 NextDays(int32 Day, int32 Month, int32 Year, int32 * NextDay, int32 * NextMonth, int32 * NextYear, int32 NrDays)
{
	int32 DayOfYear, res, count;

	if (Day < 1)
		return -1;

	if (Month < 1)
		return -1;

	if (Month > 12)
		return -1;

	if (((Year % 4) == 0) && (Month == 2))
	{
		if (Day > 29)
			return -1;
	}
	else
	{
		if (Day > MonthDays[Month - 1])
			return -1;
	}

	if (NrDays == 0)
	{
		*NextDay = Day;
		*NextMonth = Month;
		*NextYear = Year;
		return 0;
	}

	DayOfYear = GetDayOfYear(Day, Month, Year);

	if (DayOfYear == -1)
		return -1;

	DayOfYear += NrDays;
	*NextYear = Year;
	count = 0;

	if (NrDays < 0)
	{
		while ((count < 100) && ((res = GetDateOfDayOfYear(DayOfYear, *NextYear, NextDay, NextMonth)) == -1))
		{
			*NextYear = *NextYear - 1;

			if ((*NextYear % 4) == 0)
				DayOfYear += 366;
			else
				DayOfYear += 365;

			count++;
		}
	}
	else
	{
		while ((count < 100) && ((res = GetDateOfDayOfYear(DayOfYear, *NextYear, NextDay, NextMonth)) == -1))
		{
			if ((*NextYear % 4) == 0)
				DayOfYear -= 366;
			else
				DayOfYear -= 365;

			*NextYear = *NextYear + 1;
			count++;
		}
	}

	if (count == 100)
		return -1;

	return 0;
}

// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************
// ***************************************************************************************************

int32 DaysAfter1900FromFileName(LPSTR FileName)
{
	/*
	struct _finddata_t {
	    unsigned  attrib;
	    time_t  time_create;  // -1 for FAT file systems
	    time_t  time_access;  // -1 for FAT file systems
	    time_t  time_write;
	    _fsize_t  size;
	    char  name[260];
	};

	*/
	struct _finddata_t fileinfo;
	struct tm *TimeOfFile;
	long res;

	if ((res = _findfirst(FileName, &fileinfo)) == -1)
		return -1;

	/*
	struct tm {
	        int32 tm_sec;     // seconds after the minute - [0,59]
	        int32 tm_min;     // minutes after the hour - [0,59]
	        int32 tm_hour;    // hours since midnight - [0,23]
	        int32 tm_mday;    // day of the month - [1,31]
	        int32 tm_mon;     // months since January - [0,11]
	        int32 tm_year;    // years since 1900
	        int32 tm_wday;    // days since Sunday - [0,6]
	        int32 tm_yday;    // days since January 1 - [0,365]
	        int32 tm_isdst;   // daylight savings time flag
	        };
	*/
	TimeOfFile = localtime(&fileinfo.time_write);
	return ((TimeOfFile->tm_year + 1900) * 10000 + (TimeOfFile->tm_mon + 1) * 100 + TimeOfFile->tm_mday);
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************


uint32 SecondsAfter1970(uint32 year, uint32 mon, uint32 day, uint32 hour, uint32 min, uint32 sec)
{
//  year-=1900;
//  mon--;
	if (0 >= (int) (mon -= 2))
	{	/* 1..12 -> 11,12,1..10 */
		mon += 12;				/* Puts Feb last since it has leap day */
		year -= 1;
	}

	return ((((uint32) (year / 4 - year / 100 + year / 400 + 367 * mon / 12 + day) + year * 365 - 719499) * 24 + hour	/* now have hours */
	        ) * 60 + min		/* now have minutes */
	       ) * 60 + sec;			/* finally seconds */
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

uint32 GetCurrentSecondsAfter1970()
{
	uint32 year, mon, day, hour, min, sec;
	SYSTEMTIME CurrentDate;

	GetLocalTime(&CurrentDate);

	day = CurrentDate.wDay;
	mon = CurrentDate.wMonth;
	year = CurrentDate.wYear;
	hour = CurrentDate.wHour;
	min = CurrentDate.wMinute;
	sec = CurrentDate.wSecond;

//  year-=1900;
//  mon--;
	if (0 >= (int32) (mon -= 2))
	{	/* 1..12 -> 11,12,1..10 */
		mon += 12;				/* Puts Feb last since it has leap day */
		year -= 1;
	}

	return ((((uint32) (year / 4 - year / 100 + year / 400 + 367 * mon / 12 + day) + year * 365 - 719499) * 24 + hour	/* now have hours */
	        ) * 60 + min		/* now have minutes */
	       ) * 60 + sec;			/* finally seconds */
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 ConvertSecondsAfter1970(uint32 SecondsAfter1970, int32 * year, int32 * month, int32 * day, int32 * hour,
                              int32 * minutes, int32 * seconds)
{

#define _FOUR_YEAR_SEC    126230400
#define _YEAR_SEC         31536000
#define _DAY_SEC          86400
#define _BASE_DOW         4		/* 01-01-1970 was a Thursday */

	int32 islpyr = 0;			/* is-current-year-a-leap-year flag */
	int32 tmptim;
	int32 *mdays;				/* pointer to days or lpdays */
	int32 yearday, weekday;
	uint32 caltim_copy = SecondsAfter1970;

	int32 _lpdays[] = {
		-1, 30, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
	};

	int32 _days[] = {
		-1, 30, 58, 89, 119, 150, 180, 211, 242, 272, 303, 333, 364
	};

	if (SecondsAfter1970 < 0L)
		return 0;

	/*
	 * Determine years since 1970. First, identify the four-year interval
	 * since this makes handling leap-years easy (note that 2000 IS a
	 * leap year and 2100 is out-of-range).
	 */
	tmptim = SecondsAfter1970 / _FOUR_YEAR_SEC;
	SecondsAfter1970 -= tmptim * _FOUR_YEAR_SEC;

	/*
	 * Determine which year of the interval
	 */
	tmptim = (tmptim * 4) + 70;	/* 1970, 1974, 1978,...,etc. */

	if (SecondsAfter1970 >= _YEAR_SEC)
	{
		tmptim++;				/* 1971, 1975, 1979,...,etc. */
		SecondsAfter1970 -= _YEAR_SEC;

		if (SecondsAfter1970 >= _YEAR_SEC)
		{
			tmptim++;			/* 1972, 1976, 1980,...,etc. */
			SecondsAfter1970 -= _YEAR_SEC;

			/*
			 * Note, it takes 366 days-worth of seconds to get past a leap
			 * year.
			 */
			if (SecondsAfter1970 >= (_YEAR_SEC + _DAY_SEC))
			{

				tmptim++;		/* 1973, 1977, 1981,...,etc. */
				SecondsAfter1970 -= _YEAR_SEC + _DAY_SEC;
			}
			else
			{
				/*
				 * In a leap year after all, set the flag.
				 */
				islpyr++;
			}
		}
	}

	/*
	 * tmptim now holds the value for tm_year. caltim now holds the
	 * number of elapsed seconds since the beginning of that year.
	 */
	*year = tmptim;

	/*
	 * Determine days since January 1 (0 - 365). This is the tm_yday value.
	 * Leave caltim with number of elapsed seconds in that day.
	 */
	yearday = (int) (SecondsAfter1970 / _DAY_SEC);
	SecondsAfter1970 -= (long) (yearday) * _DAY_SEC;

	/*
	 * Determine months since January (0 - 11) and day of month (1 - 31)
	 */
	if (islpyr)
		mdays = _lpdays;
	else
		mdays = _days;

	for (tmptim = 1; mdays[tmptim] < yearday; tmptim++);

	*month = (--tmptim) + 1;

	*day = yearday - mdays[tmptim];

	/*
	 * Determine days since Sunday (0 - 6)
	 */
	weekday = ((int) (caltim_copy / _DAY_SEC) + _BASE_DOW) % 7;

	/*
	 *  Determine hours since midnight (0 - 23), minutes after the hour
	 *  (0 - 59), and seconds after the minute (0 - 59).
	 */
	*hour = (int) (SecondsAfter1970 / 3600);
	SecondsAfter1970 -= (long) *hour * 3600L;

	*minutes = (int) (SecondsAfter1970 / 60);
	*seconds = (int) (SecondsAfter1970 - (*minutes) * 60);

	*year += 1900;
	return 1;
}


// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
