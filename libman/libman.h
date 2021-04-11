/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: libman.h
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


#ifndef _LIBMAN

#define _LIBMAN

#include "owntypes.h"

#define RGB_Black                               RGB(  0,  0,  0)
#define RGB_DarkGray                            RGB( 64, 64, 64)
#define RGB_MidGray                             RGB( 96, 96, 96)
#define RGB_Gray                                RGB(128,128,128)
#define RGB_LightGray                           RGB(192,192,192)
#define RGB_White                               RGB(255,255,255)

#define RGB_Red                                 RGB(255,  0,  0)
#define RGB_Blue                                RGB(  0,  0,255)
#define RGB_Green                               RGB(  0,255,  0)

#define RGB_Yellow                              RGB(255,255,  0)
#define RGB_Cyan                                RGB(  0,255,255)
#define RGB_Magenta                             RGB(255,  0,255)

#define RGB_DarkMagenta                         RGB(170,  0,170)
#define RGB_DarkCyan                            RGB(  0,170,170)
#define RGB_DarkBlue                            RGB(  0,  0,170)
#define RGB_DarkRed                             RGB(170,  0,  0)
#define RGB_DarkGreen                           RGB(  0,170,  0)
#define RGB_DarkMagenta                         RGB(170,  0,170)

#define RGB_LightRed                            RGB(255,120,120)
#define RGB_LightBlue                           RGB(140,140,255)
#define RGB_LightGreen                          RGB(120,255,120)
#define RGB_LightMagenta                        RGB(255,120,255)
#define RGB_LightBrown                          RGB(192,128,64 )
#define RGB_Orange                              RGB(255,165,  0)
#define RGB_Pink                                RGB(255,192,203)
#define RGB_DarkPink                            RGB(207, 37,190)
#define RGB_LightPink                           RGB(255,210,225)
#define RGB_Brown                               RGB(128,64 ,0  )
#define RGB_LightOrange                         RGB(223,188,96 )
#define RGB_Violet                              RGB(238,130,238)

void LoadUserIniFile(void);

#endif
