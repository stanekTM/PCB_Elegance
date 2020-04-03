/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: gerber2.h
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


#ifndef _GERBER2

#define _GERBER2

#include "types.h"

int32 WriteGerberString(LPSTR GerberString, int32 mode);

int32 GetGerberValueInString(double ValueX, double ValueY, LPSTR GerberString, int32 mode);

int32 GerberWriteLine(double x1, double y1, double x2, double y2);

int32 GerberPlotObject(ObjectRecord * Object, int32 mode);

int32 DrawGerberLine(double x1, double y1, double x2, double y2, double ThickNess, double PenSize);

int32 DrawGerberCircleOpenPad(double x1, double y1, double ThickNess, double HoleSize, double PenSize);

int32 DrawGerberSquareOpenPad(double x1, double y1, double ThickNess, double HoleSize, double PenSize);

int32 DrawGerberRectPad(double x1, double y1, double PadSizeX, double PadSizeY, double PenSize);

int32 DrawGerberButterflyOpenPad(double x1, double y1, double ThickNess, double HoleSize, double PenSize);

int32 DrawGerberButterflyPad(double x1, double y1, double PadSizeX, double PadSizeY, double PenSize);

int32 DrawGerberCirclePad(double x1, double y1, double ThickNess, double PenSize);

int32 DrawGerberCircle(ObjectRecord * Object);

int32 DrawGerberArc(ObjectRecord * Object);

int32 DrawGerberThermalRelief(ObjectRecord * Object);

int32 PlotAreaFillToGerber(AreaFillRecord * AreaFill, double Thickness1, double Thickness2, int32 mode);

int32 PowerPlaneToGerber(int32 Layer, double Thickness1, double Thickness2, int32 mode);

int32 DrawGerberStr(double x, double y, double Size, double LineThickness, int32 Rotation, int32 Alignment,
                    int32 Mirror, char *str);

int32 DrawGerberStr2(double x, double y, double Size, double LineThickness, double Rotation, int32 Alignment,
                     int32 Mirror, char *str);

int32 GerberPlotPolygon(PolygonRecord * DrawPolygon, int32 mode);

int32 GerberPlotSpecialAreaFill(AreaFillRecord * AreaFill, int32 mode);

#endif
