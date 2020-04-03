/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: calcrect.h
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



#ifndef _CALCRECT

#define _CALCRECT

#include "owntypes.h"

int32 RectTestLine(double x1, double y1, double x2, double y2);

int32 RectTestCircle(double CircleX, double CircleY, double CircleThickness, int32 CircleMode);

int32 RectTestCircleOutline(double x1, double y1, double x2, int32 mode);

int32 RectTestArc(double ArcX, double ArcY, double ArcWidth, double ArcHeight, double StartDiffX, double StartDiffY,
                  double EndDiffX, double EndDiffY);

int32 RectTestRect(double Xmin, double Ymin, double Xmax, double Ymax);

int32 RectTestRect2(double x1, double y1, double x2, double y2);

int32 RectTestRectOutline(double x1, double y1, double x2, double y2);

int32 RectTestDiag1(double x1, double y1, double x2, double y2);

int32 RectTestDiag2(double x1, double y1, double x2, double y2);

void GetMinMaxArc(double ArcX, double ArcY, double ArcWidth, double ArcHeight, double StartDiffX, double StartDiffY,
                  double EndDiffX, double EndDiffY, double *ArcXmin, double *ArcYmin, double *ArcXmax, double *ArcYmax);

#endif
