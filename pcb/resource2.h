/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: resource2.h
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


#ifndef _RESOURCE2

#define _RESOURCE2

#define ID_SWITCH_TO_LAYER              0x000000
#define ID_ADD_AREAFILL                 0x000100
#define ID_ADD_AREAFILL2                0x000200
#define ID_DRAW_ONLY_ON_LAYER           0x000300
#define ID_REBUILD_POWERPLANE           0x000400
#define ID_CHANGE_CLEARANCE_POWERPLANE  0x000500
#define ID_CUT_POLYLINE_POWERPLANE      0x000600
#define ID_CUT_CIRCLE_POWERPLANE        0x000700
#define ID_CUT_RECTANGLE_POWERPLANE     0x000800
#define ID_CHANGE_POWERPLANE            0x000900
#define ID_CHECK_LAYERS                 0x000a00
#define ID_REMOVE_POWERPLANE            0x000b00
#define ID_ADD_POWERPLANE               0x000c00
#define ID_CUT_HOR_TRACE_POWERPLANE     0x000d00
#define ID_CUT_VER_TRACE_POWERPLANE     0x000e00

#define ID_CLEARANCE_WIDTH              0x100000
#define ID_TRACE_WIDTH                  0x110000

#define ID_MOVE_OTHER_LAYER             0x200000
#define ID_COPY_OTHER_LAYER             0x210000
#define ID_COPY_COMP_OTHER_LAYER        0x220000
#define ID_SELECT_LAYER                 0x230000

#define ID_ADD_LINE_OBJECT              0x400000
#define ID_ADD_RECT_OBJECT              0x410000
#define ID_ADD_RECT2_OBJECT             0x420000
#define ID_ADD_RECT_PAD_OBJECT          0x430000
#define ID_ADD_CIRCLE_PAD_OBJECT        0x440000
#define ID_ADD_CIRCLE_OBJECT_1          0x450000
#define ID_ADD_CIRCLE_OBJECT_2          0x460000
#define ID_ADD_CIRCLE_OBJECT_3          0x470000
#define ID_ADD_CIRCLE_OBJECT_4          0x480000
#define ID_ADD_CIRCLE_OBJECT_6          0x490000
#define ID_ADD_CIRCLE_OBJECT_8          0x4A0000
#define ID_ADD_CIRCLE_OBJECT_9          0x4B0000
#define ID_ADD_CIRCLE_OBJECT_C          0x4C0000
#define ID_ADD_CIRCLE_OBJECT_F          0x4D0000
#define ID_ADD_ARC_OBJECT               0x4E0000
#define ID_ADD_TEXT_OBJECT              0x4F0000
#define ID_ADD_TEXT2_OBJECT             0x500000
#define ID_ADD_POLYGON_OBJECT           0x510000
#define ID_ADD_POLYLINE_OBJECT          0x520000
#define ID_ADD_LINE_OBJECT_ARROW1       0x530000
#define ID_ADD_LINE_OBJECT_ARROW2       0x540000
#define ID_ADD_LINE_OBJECT_ARROW3       0x550000
#define ID_ADD_DIMENSION_OBJECT         0x560000
#define ID_ADD_DIMENSION_OBJECT2        0x570000


#endif
