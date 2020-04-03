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

void ViewWholeDesign(int32 mode);

void InitTraceDrawing(int32 DrawCode);

void ExitTraceDrawing(void);

void DrawHorTrace(TraceRecord * Trace);

void DrawVerTrace(TraceRecord * Trace);

void DrawDiag1Trace(TraceRecord * Trace);

void DrawDiag2Trace(TraceRecord * Trace);

void DrawObjectTrace(ObjectRecord * ObjectTrace);

void DrawVia(ViaRecord * Via);

void DrawViaDrill(ViaRecord * Via);

void DrawSoldPastePadsVia(ViaRecord * Via);

void DrawObjectVia(ObjectRecord * ObjectVia);

void DrawHorTrace2(TraceRecord * Trace, int32 mode);

void DrawVerTrace2(TraceRecord * Trace, int32 mode);

void DrawDiag1Trace2(TraceRecord * Trace, int32 mode);

void DrawDiag2Trace2(TraceRecord * Trace, int32 mode);

void DrawVia2(ViaRecord * Via);

void DrawHorTraceWithClearance(TraceRecord * Trace);

void DrawVerTraceWithClearance(TraceRecord * Trace);

void DrawDiag1TraceWithClearance(TraceRecord * Trace);

void DrawDiag2TraceWithClearance(TraceRecord * Trace);

void DrawViaWithClearance(ViaRecord * Via);

void DrawViaWithPowerPads(ViaRecord * Via);

void DrawObjectTrace(ObjectRecord * ObjectTrace);

void DrawObjectTraceXor(ObjectRecord * ObjectTrace);

void DrawObjectTraceWithClearance(ObjectRecord * ObjectTrace);

void DrawHorTraces(int32 Layer, int32 Mode);

void DrawVerTraces(int32 Layer, int32 Mode);

void DrawDiag1Traces(int32 Layer, int32 Mode);

void DrawDiag2Traces(int32 Layer, int32 Mode);

void DrawHorTracesWithClearance(int32 Layer, int32 Mode);

void DrawVerTracesWithClearance(int32 Layer, int32 Mode);

void DrawDiag1TracesWithClearance(int32 Layer, int32 Mode);

void DrawDiag2TracesWithClearance(int32 Layer, int32 Mode);

void DrawTraces(int32 Mode);

void DrawTracesWithClearance(int32 Mode);

void DrawVias(int32 Mode);

void DrawViaDrills(int32 Mode);

void DrawViaSoldMaskPads(int32 Mode);

void DrawConnection(ConnectionsRecord * Connection);

void DrawConnection2(ConnectionsRecord * Connection);

void DrawConnections(int32 Mode);

void DrawObjectLines(int32 Mode);

void DrawObjectLine(ObjectLineRecord * ObjectLine, double OX, double OY, int32 Mode);

void DrawObjectRects(int32 Mode);

void DrawObjectRect(ObjectRectRecord * ObjectRect, double OX, double OY, int32 Mode);

void DrawObjectCircles(int32 Mode);

void DrawObjectCircle(ObjectCircleRecord * ObjectCircle, double OX, double OY, int32 Mode);

void DrawObjectArcs(int32 Mode, int32 Mode2);

void DrawObjectArc(ObjectArcRecord * ObjectArc, double OX, double OY, int32 Mode);

void DrawObjectTexts(int32 Mode);

void DrawObjectText(ObjectTextRecord * ObjectText, double OX, double OY, int32 Mode);

void DrawObjectText2(ObjectTextRecord2 * ObjectText2, double OX, double OY, double NewRotation, int32 Mode);

void DrawObjectTexts2(int32 Mode);

void DrawObjectPolygons(int32 Mode);

void DrawObjectPolygon(ObjectPolygonRecord * ObjectPolygon, double OX, double OY, int32 Mode);

void DrawPolygonDirect(PolygonRecord * ObjectPolygon, int32 mode);

void DrawStr(double x, double y, double Size, int32 Rotation, int32 Alignment, int32 Mirror, char *str);

void DrawStrWithRotation(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror, char *str);

void DrawStrWithRotation2(double x, double y, double Size, double Rotation, int32 Alignment, int32 Mirror, WCHAR * str);

void DrawTrueTypeStrWithRotation(double x, double y, double Size, int32 FontNr, double Rotation, int32 Alignment,
                                 int32 Mirror, WCHAR * str, int32 Layer, int32 mode);

void DrawAreaFill(AreaFillRecord * AreaFill, int32 Mode);

void DrawAreaFillClearance(AreaFillRecord * AreaFill, int32 Mode);

void DrawPinAreaFillClearance(AreaFillRecord * AreaFill, int32 Mode);

void DrawPinAreaFill(AreaFillRecord * AreaFill, int32 Mode);

void DrawAreaFills(int32 Mode);

void SetLayerColors(void);

#endif
