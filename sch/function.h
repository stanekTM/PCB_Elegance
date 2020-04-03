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
#define FunctionViewOptions                   22049
#define FunctionGotoXY                        22050
#define FunctionCheck                         22051
#define FunctionComponentSelections           22052
#define FunctionEditLayoutOnRef               22053

// *****************************************************************************
// *****************************************************************************

#define FunctionAddPin                        22100
#define FunctionAddPinBus                     22101
#define FunctionAddPowerPin                   22102
#define FunctionAddNetLabel                   22103
#define FunctionAddNetLabel2                  22104
#define FunctionAddWire                       22105
#define FunctionAddBus                        22106
#define FunctionAddBusConnection              22107
#define FunctionAddSymbol                     22108
#define FunctionAddDataBaseComponent          22109
#define FunctionAddExternalInput              22110
#define FunctionAddExternalOutput             22111
#define FunctionAddExternalIO                 22112
#define FunctionAddJunction                   22113
#define FunctionAddNetLabelWire               22114
#define FunctionAddLine                       22115
#define FunctionAddRect                       22116
#define FunctionAddRect4Lines                 22117
#define FunctionAddCircle                     22118
#define FunctionAddArc                        22119
#define FunctionAddText                       22120
#define FunctionAddNumbersIncremental         22121
#define FunctionAddPasteFromClipBoard         22122
#define FunctionAddCopyToClipBoard            22123
#define FunctionSearchForAnyText              22124
#define FunctionEditText                      22125
#define FunctionEditSheet                     22126
#define FunctionGotoHigherSheet               22127
#define FunctionEditSymbol                    22128
#define FunctionAddSymbolShortCut             22129
#define FunctionAddWire2                      22130
#define FunctionAddBus2                       22131
#define FunctionAddDimension                  22132
#define FunctionAddComponent                  22133
#define FunctionAddOnePinNet                  22134
#define FunctionSearchNextText                22135
#define FunctionReplaceTexts                  22136
#endif
