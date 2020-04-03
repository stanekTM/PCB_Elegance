/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: draw.h
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


#ifndef _DRAW

#define _DRAW

#include "types.h"

void ExitTraceDrawing(void);

void DrawWires(int32 Mode);

void DrawWire(WireRecord * Wire, double OX, double OY, int32 Mode);

void DrawBusses(int32 Mode);

void DrawBus(BusRecord * Bus, double OX, double OY, int32 Mode);

void DrawBusConnections(int32 Mode);

void DrawBusConnection(BusConnectionRecord * BusConnection, double OX, double OY, int32 Mode);

void DrawJunctions(int32 Mode);

void DrawJunction(JunctionRecord * Junction, double OX, double OY, int32 Mode);

void DrawOnePinNets(int32 Mode);

void DrawOnePinNet(OnePinNetRecord * OnePinNet, double OX, double OY, int32 Mode);

void DrawNetLabels(int32 Mode);

void DrawNetLabel(NetLabelRecord * NetLabel, double OX, double OY, int32 Mode);

void DrawObjectLines(int32 Mode);

void DrawObjectLine(ObjectLineRecord * ObjectLine, double OX, double OY, int32 Mode);

void DrawObjectRects(int32 Mode);

void DrawObjectRect(ObjectRectRecord * ObjectRect, double OX, double OY, int32 Mode);

void DrawObjectCircles(int32 Mode);

void DrawObjectCircle(ObjectCircleRecord * ObjectCircle, double OX, double OY, int32 Mode);

void DrawObjectArcs(int32 Mode);

void DrawObjectArc(ObjectArcRecord * ObjectArc, double OX, double OY, int32 Mode);

void DrawObjectTexts(int32 Mode);

void DrawObjectText(ObjectTextRecord * ObjectText, double OX, double OY, int32 Mode);

//void DrawSubSheet(SubSheetRecord *SubSheet,double OX,double OY,int32 Mode);

void DrawStr(double x, double y, double Size, int32 Rotation, int32 Alignment, char *str);

void DrawStrWithRotation(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror, char *str);

void DrawPin(PinRecord * Pin, double OX, double OY, int32 Mode);

void DrawPins(int32 Mode);

void DrawPowerPin(PowerPinRecord * PowerPin, double OX, double OY, int32 Mode);

void DrawPowerPins(int32 Mode);

void DrawPinPowerPins(int32 Mode);

void DrawPinBus(PinBusRecord * PinBus, double OX, double OY, int32 Mode);

void DrawPinBusses(int32 Mode);

void DrawGlobalConnection(GlobalConnectionRecord * GlobalConnection, double OX, double OY, int32 Mode);

void DrawGlobalConnections(int32 Mode);

#endif
