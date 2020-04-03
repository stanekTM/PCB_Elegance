/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: function.h
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



#ifndef _FUNCTION

#define _FUNCTION

#include "windows.h"
#include "types.h"


// ******************************************************************************
// ******************************************************************************

#define FunctionZoomIn                        22000
#define FunctionZoomOut                       22001
#define FunctionViewAll                       22002
#define FunctionViewPan                       22003
#define FunctionPreviousView                  22004
#define FunctionExit                          22005
#define FunctionDelete                        22006
#define FunctionPrint                         22007
#define FunctionOptions                       22008
#define FunctionChangeUnits                   22009
#define FunctionGridOnOff                     22010
#define FunctionInfo                          22011
#define FunctionUndo                          22012
#define FunctionRedo                          22013
#define FunctionRePaint                       22014
#define FunctionUnselectAll                   22015
#define FunctionFileOpen                      22016
#define FunctionFileNew                       22017
#define FunctionFileSave                      22018
#define FunctionFileSaveAs                    22019
#define FunctionViewLayers                    22020
#define FunctionSelectLayer                   22021
#define FunctionCopy                          22022
#define FunctionDrag                          22023
#define FunctionMove                          22024
#define FunctionMirrorX                       22025
#define FunctionMirrorY                       22026
#define FunctionRotate                        22027
#define FunctionRotate180                     22028
#define FunctionRotate270                     22029
#define FunctionRelativePositionOnGrid        22030
#define FunctionSwitchRelativePositionOnGrid  22031
#define FunctionZeroRelativeCursor            22032
#define FunctionViewWith300DPI                22033
#define FunctionViewWith360DPI                22034
#define FunctionViewWith600DPI                22035
#define FunctionViewWith720DPI                22036
#define FunctionViewWith1000DPI               22037
#define FunctionViewWith1200DPI               22038
#define FunctionViewWith1440DPI               22039
#define FunctionViewWith2000DPI               22040
#define FunctionViewWith2400DPI               22041
#define FunctionViewChangeColors              22042
#define FunctionViewLoadDefaultColors         22043
#define FunctionHelp                          22044
#define FunctionAbout                         22045
#define FunctionHelpTopics                    22046
#define FunctionSelectionModeAppend           22047
#define FunctionSelectionModeReplace          22048
#define FunctionSelectError                   22049
#define FunctionAddPasteFromClipBoard         22050
#define FunctionAddCopyToClipBoard            22051
#define FunctionNextError                     22052


// *****************************************************************************
// *****************************************************************************

#define FunctionInsertVia                     22100
#define FunctionTraceBackWards                22101
#define FunctionMoveCompByRef                 22102
#define FunctionShortestGuideRoute            22103
#define FunctionAddExtraTrace                 22104
#define FunctionEditText                      22105
#define FunctionFinishTrace                   22106
#define FunctionSelectComponentMenu           22107
#define FunctionDragTrace                     22108
#define FunctionDragTracesVias                22109
#define FunctionRouteTraces                   22110
#define FunctionSwitchTraceMenu               22111
#define FunctionSwitchToNearestLayer          22112
#define FunctionCenterOnComponent             22113
#define FunctionComponentReferenceOnOff       22114
#define FunctionComponentValueOnOff           22115
#define FunctionSwitchViaClearanceOnOff       22116
#define FunctionSwitchTwoTracesOnOff          22117
#define FunctionSwitchTryingClearanceOnOff    22118
#define FunctionSwitchHilightNetOnOff         22119
#define FunctionEditGeometry                  22120
#define FunctionProtectComponent              22121
#define FunctionSwitchClearanceOnOff          22122
#define FunctionSwapNets                      22123
#define FunctionAreafillMenu                  22124
#define FunctionObjectsMenu                   22125
#define FunctionDrawDrillOnOff                22126
#define FunctionChangeGeometry                22127
#define FunctionChangeTraceWidth              22128
#define FunctionRebuildAreafill               22129
#define FunctionSwitchTraceDrawingMode        22130
#define FunctionNormalTraceDrawingMode        22131
#define FunctionArc45TraceDrawingMode         22132
#define FunctionArc90TraceDrawingMode         22133
#define FunctionAllAngleTraceDrawingMode      22134
#define FunctionCenterViewOnNextConnection    22135
#define FunctionDesignRulesnet                22136
#define FunctionEditDesignRulesNet            22137
#define FunctionViewDisplayInfo               22138
#define FunctionComponentSelections           22139
#define FunctionEditSchematicOnRef            22140
#define FunctionMoveCompsUpOneGrid            22141
#define FunctionMoveCompsDownOneGrid          22142
#define FunctionMoveCompsLeftOneGrid          22143
#define FunctionMoveCompsRightOneGrid         22144
#define FunctionSwapComponents                22145
#endif
