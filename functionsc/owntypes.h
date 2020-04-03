/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: owntypes.h
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


#ifndef _OWNTYPES

#define _OWNTYPES

#ifndef GCC_COMP

#pragma warning( disable : 4100 )	// Disable warning messages : unreferenced formal parameter
#pragma warning( disable : 4057 )	// xxxx differs in indirection to slightly different base types from xxxx
#pragma warning( disable : 4706 )	// assignment within conditional expression
#pragma warning( disable : 4127 )	// conditional expression is constant
#pragma warning( disable : 4201 )
#pragma warning( disable : 4459 )

#ifndef _DEBUG
#pragma warning( disable : 4711 )	// function 'xxxxx' selected for automatic inline expansion
#endif

#pragma warning( disable : 4996 )
#define asm _asm

#endif

#define MAX_LENGTH_STRING     512


#define PI                    3.14159265358979262

#ifndef GCC_COMP

#define ALIGN8PRE     __declspec(align(8))
#define ALIGN8POST
#define ALIGN16PRE    __declspec(align(16))
#define ALIGN16POST
#define ALIGN32PRE    __declspec(align(32))
#define ALIGN32POST
#define ALIGN256PRE   __declspec(align(256))
#define ALIGN256POST
#define ALIGN4096PRE  __declspec(align(4096))
#define ALIGN4096POST

#define STRING_DECLARE(x)  ALIGN32PRE char x[MAX_LENGTH_STRING]

#else

#define ALIGN8PRE
#define ALIGN8POST    __attribute__ ((aligned (8)))
#define ALIGN16PRE
#define ALIGN16POST   __attribute__ ((aligned (16)))
#define ALIGN32PRE
#define ALIGN32POST   __attribute__ ((aligned (32)))
#define ALIGN256PRE
#define ALIGN256POST  __attribute__ ((aligned (256)))
#define ALIGN4096PRE
#define ALIGN4096POST __attribute__ ((aligned (4096)))

#define STRING_DECLARE(x)   char x[MAX_LENGTH_STRING]  ALIGN32POST

#endif

typedef unsigned char uint8;
typedef unsigned char BYTE;
typedef char int8;

typedef short int16;
typedef int int32;
#ifndef GCC_COMP
typedef __int64 int64;
typedef unsigned __int64 uint64;
#else
typedef long long int64;
typedef unsigned long long uint64;
#endif
typedef unsigned int uint32;
typedef unsigned short uint16;

#ifndef min

#define min(a, b)  (((a) < (b)) ? (a) : (b))

#endif

#ifndef max

#define max(a, b)  (((a) > (b)) ? (a) : (b))

#endif

typedef int GraphicsInt;

#ifndef BOOL
typedef int BOOL;
#endif

#ifndef FALSE
#define FALSE                                   0
#endif

#ifndef TRUE
#define TRUE                                    1
#endif


#endif
