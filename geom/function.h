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
#define FunctionChangeGrid                    22008
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

// *****************************************************************************
// *****************************************************************************

#define FunctionAssignPins                    22100
#define FunctionAssignPinsAuto                22101
#define FunctionAddLinePlacementOutline       22102
#define FunctionAddLineComponentOutline       22103
#define FunctionAddLineSilkScreen             22104
#define FunctionAddRectPlacementOutline       22105
#define FunctionAddRectComponentOutline       22106
#define FunctionAddRectSilkScreen             22107
#define FunctionAddRectPlacementOutline2      22108
#define FunctionAddRectComponentOutline2      22109
#define FunctionAddRectSilkScreen2            22110
#define FunctionAddCirclePlacementOutline     22111
#define FunctionAddCircleComponentOutline     22112
#define FunctionAddCircleSilkScreen           22113
#define FunctionAddArcPlacementOutline        22114
#define FunctionAddArcComponentOutline        22115
#define FunctionAddArcSilkScreen              22116
#define FunctionAddTextCompOutline            22117
#define FunctionAddTextSilkScreen             22118
#define FunctionAddDrillHole                  22119
#define FunctionAddUnplatedDrillHole          22120
#define FunctionAddTraceBottom                22121
#define FunctionAddTraceSolderMaskBottom      22122
#define FunctionAddTracePasteMaskBottom       22123
#define FunctionAddTraceTop                   22124
#define FunctionAddTraceSolderMaskTop         22125
#define FunctionAddTracePasteMaskTop          22126
#define FunctionAddCircleTop                  22127
#define FunctionAddCircleBottom               22128
#define FunctionAddCircleSolderMaskTop        22129
#define FunctionAddCirclePasteMaskTop         22130
#define FunctionAddCircleSolderMaskBottom     22131
#define FunctionAddCirclePasteMaskBottom      22132
#define FunctionAddCircleInnerPad             22133
#define FunctionAddCircleAntiPowerPad         22134
#define FunctionAddRectTop                    22135
#define FunctionAddRectBottom                 22136
#define FunctionAddRectSolderMaskTop          22137
#define FunctionAddRectPasteMaskTop           22138
#define FunctionAddRectSolderMaskBottom       22139
#define FunctionAddRectPasteMaskBottom        22140
#define FunctionEditText                      22141
#define FunctionInsertPoint                   22142
#define FunctionLengthToRelativeCursor        22143
#define FunctionZeroRelativeCursorSnap        22144
#define FunctionTraceBackWards                22145
#define FunctionCopyToClipboard               22146
#define FunctionCopyFromClipboard             22147

#endif
