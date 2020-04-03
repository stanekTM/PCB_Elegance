/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: toets.c
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



#include "toets.h"
#include "owntypes.h"
#include "keyswin.h"
#include "string.h"
#include "ctype.h"
#include "stdio.h"
#include "function.h"
#include "resource.h"
#include "resource2.h"


#define  MaxFunctions     180
#define  MaxNrKeysInBuf   513

/*   types */

typedef struct
{
	int32 KeyFunction, SystemFunction, Key, DefaultKey, FunctionMode;
	char FunctionString[48];
} KeyRecord;

typedef struct
{
	LPSTR KeyStringPtr;
	int32 KeyValue;
} KeyStringRecord;

extern LPSTR GeomNamesId[];


/*   Global variables toets */

int32 TranslateKey, AltPressed, CtrlPressed, ShiftPressed;

int32 NrFunctionsInBuf, PreviousKey;

/*   Locals varaibles toets */
// *INDENT-OFF*

int32    Functionbuf[MaxNrKeysInBuf];
uint8    CurrentExpKey;


char     ShiftString[12];
char     CtrlString[12];
char     AltString[12];
char     SpaceString[12];
char     KeyNameString[12];


KeyStringRecord KeyStrings[32]={0,0x0881,
                                0,0x0880,
                                0,0x0883,
                                0,0x0882,
                                0,0x0889,
                                0,0x0888,
                                0,0x0886,
                                0,0x0884,
                                0,0x0885,
                                0,0x0887,
                                0,0x081B,
                                0,0x0808,
                                0,0x080D,
                                0,0x0809,
                                0,0x08A0,
                                0,0x08A1,
                                0,0x08A2,
                                0,0x08A3,
                                0,0x08A4,
                                0,0x08A5,
                                0,0x08A6,
                                0,0x08A7,
                                0,0x08A8,
                                0,0x08A9,
                                0,0x08AA,
                                0,0x08AB,
                                0,0,
                                0,0,
                                0,0,
                                0,0,
                                0,0,
                                0,0};
KeyRecord KeyFunctions[MaxFunctions] =

 {FunctionZoomIn                      ,ID_VIEW_ZOOMIN                   ,(int32)'z'            ,0,-1,"FunctionZoomIn"                      ,
  FunctionZoomOut                     ,ID_VIEW_ZOOMOUT                  ,(int32)'Z'            ,0,-1,"FunctionZoomOut"                     ,
  FunctionViewAll                     ,ID_VIEW_VIEWFULL                 ,Key_Shift_F8          ,0,-1,"FunctionViewAll"                     ,
  FunctionViewPan                     ,ID_VIEW_PAN                      ,(int32)'x'            ,0,-1,"FunctionViewPan"                     ,
  FunctionPreviousView                ,ID_VIEW_PREVIOUS_VIEW            ,(int32)'v'            ,0,-1,"FunctionPreviousView"                ,
  FunctionExit                        ,0x0000                           ,Key_Ctrl_Q            ,0,-1,"FunctionExit"                        ,
  FunctionDelete                      ,ID_EDIT_DELETE                   ,Key_Del               ,0,-1,"FunctionDelete"                      ,
  FunctionPrint                       ,ID_FILE_PRINT                    ,0                     ,0,-1,"FunctionPrint"                       ,
  FunctionChangeGrid                  ,ID_VIEW_CHANGEGRID               ,Key_Ctrl_G            ,0,-1,"FunctionChangeGrid"                  ,
  FunctionInfo                        ,ID_VIEW_INFOSELECTEDOBJECTS      ,(int32)'i'            ,0,-1,"FunctionInfo"                        ,
  FunctionChangeUnits                 ,ID_SETTINGS_CHANGE_UNITS         ,Key_Ctrl_U            ,0,-1,"FunctionChangeUnits"                 ,
  FunctionGridOnOff                   ,ID_VIEW_GRIDONOFF                ,(int32)'g'            ,0,-1,"FunctionGridOnOff"                   ,
  FunctionUndo                        ,ID_EDIT_UNDO                     ,(int32)'u'            ,0,-1,"FunctionUndo"                        ,
  FunctionEditText                    ,ID_EDIT_TEXT                     ,(int32)'e'            ,0,-1,"FunctionEdit"                        ,
  FunctionRedo                        ,ID_EDIT_REDO                     ,(int32)'y'            ,0,-1,"FunctionRedo"                        ,
  FunctionRePaint                     ,ID_VIEW_REPAINT                  ,Key_F5                ,0,-1,"FunctionRePaint"                     ,
  FunctionUnselectAll                 ,ID_UNSELECT_ALL                  ,Key_F2                ,0,-1,"FunctionUnselectAll"                 ,
  FunctionFileOpen                    ,ID_FILE_OPEN                     ,Key_F3                ,0,-1,"FunctionFileOpen"                    ,
  FunctionFileNew                     ,0x0000                           ,0                     ,0,-1,"FunctionFileNew"                     ,
  FunctionFileSave                    ,ID_FILE_SAVE                     ,Key_Ctrl_S            ,0,-1,"FunctionFileSave"                    ,
  FunctionFileSaveAs                  ,ID_FILE_SAVEAS                   ,Key_Shift_F2          ,0,-1,"FunctionFileSaveAs"                  ,
  FunctionExit                        ,ID_FILE_EXIT                     ,Key_Ctrl_Q            ,0,-1,"FunctionExit"                        ,
  FunctionViewLayers                  ,ID_VIEW_LAYERS                   ,Key_Ctrl_A            ,0,-1,"FunctionViewLayers"                  ,
  FunctionSelectLayer                 ,0x0000                           ,0                     ,0,-1,"FunctionSelectLayer"                 ,
  FunctionCopy                        ,ID_COPY_OBJECTS                  ,(int32)'c'            ,0,-1,"FunctionCopy"                        ,
  FunctionDrag                        ,0x0000                           ,(int32)'d'            ,0,-1,"FunctionDrag"                        ,
  FunctionMove                        ,ID_MOVE_OBJECTS                  ,(int32)'m'            ,0,-1,"FunctionMove"                        ,
  FunctionMirrorX                     ,ID_MIRRORX_OBJECTS               ,0                     ,0,-1,"FunctionMirrorX"                     ,
  FunctionMirrorY                     ,ID_MIRRORY_OBJECTS               ,0                     ,0,-1,"FunctionMirrorY"                     ,
  FunctionRotate                      ,ID_ROTATE_OBJECTS                ,(int32)'R'            ,0,-1,"FunctionRotate"                      ,
  FunctionRotate180                   ,0x0000                           ,0                     ,0,-1,"FunctionRotate180"                   ,
  FunctionRotate270                   ,0x0000                           ,0                     ,0,-1,"FunctionRotate270"                   ,
  FunctionRelativePositionOnGrid      ,0x0000                           ,0                     ,0,-1,"FunctionRelativePositionOnGrid"      ,
  FunctionSwitchRelativePositionOnGrid,0x0000                           ,0                     ,0,-1,"FunctionSwitchRelativePositionOnGrid",
  FunctionTraceBackWards              ,ID_TRACE_BACKWARDS               ,(int32)'b'            ,0,-1,"FunctionTraceBackwards"              ,
  FunctionZeroRelativeCursorSnap      ,ID_EDIT_ZERORELATIVECURSOR_SNAP  ,Key_Ctrl_X            ,0,-1,"FunctionZeroRelativeCursorSnap"      ,
  FunctionLengthToRelativeCursor      ,ID_LENGTH_TO_RELATIVE_CURSOR     ,Key_Ctrl_L            ,0,-1,"FunctionLengthToRelativeCursor"      ,
  FunctionZeroRelativeCursor          ,ID_EDIT_ZERORELATIVECURSOR       ,Key_Ctrl_Z            ,0,-1,"FunctionZeroRelativeCursor"          ,
  FunctionViewWith300DPI              ,0x0000                           ,0                     ,0,-1,"FunctionViewWith300DPI"              ,
  FunctionViewWith360DPI              ,0x0000                           ,0                     ,0,-1,"FunctionViewWith360DPI"              ,
  FunctionViewWith600DPI              ,0x0000                           ,0                     ,0,-1,"FunctionViewWith600DPI"              ,
  FunctionViewWith720DPI              ,0x0000                           ,0                     ,0,-1,"FunctionViewWith720DPI"              ,
  FunctionViewWith1000DPI             ,0x0000                           ,0                     ,0,-1,"FunctionViewWith1000DPI"             ,
  FunctionViewWith1200DPI             ,0x0000                           ,0                     ,0,-1,"FunctionViewWith1200DPI"             ,
  FunctionViewWith1440DPI             ,0x0000                           ,0                     ,0,-1,"FunctionViewWith1440DPI"             ,
  FunctionViewWith2000DPI             ,0x0000                           ,0                     ,0,-1,"FunctionViewWith2000DPI"             ,
  FunctionViewWith2400DPI             ,0x0000                           ,0                     ,0,-1,"FunctionViewWith2400DPI"             ,
  FunctionViewChangeColors            ,ID_VIEW_CHANGECOLORS             ,0                     ,0,-1,"FunctionViewChangeColors"            ,
  FunctionViewLoadDefaultColors       ,ID_VIEW_LOADDEFAULTCOLORS        ,0                     ,0,-1,"FunctionViewLoadDefaultColors"       ,
  FunctionSelectionModeAppend         ,ID_MODE_SELECTION_APPEND         ,0                     ,0,-1,"FunctionSelectionModeAppend"         ,
  FunctionSelectionModeReplace        ,ID_MODE_SELECTION_REPLACE        ,0                     ,0,-1,"FunctionSelectionModeReplace"        ,
  FunctionHelp                        ,ID_HELP_ON_COMMAND               ,Key_F1                ,0,-1,"FunctionHelp"                        ,
  FunctionAbout                       ,ID_HELP_ABOUT                    ,0                     ,0,-1,"FunctionAbout"                       ,
  FunctionHelpTopics                  ,ID_HELP_CONTENTS                 ,0                     ,0,-1,"FunctionHelpTopics"                  ,

// ************************************************************************************************************************************************************

  FunctionAssignPins                  ,ID_ASSIGN_PINS                   ,(int32)'a'            ,0,-1,"FunctionAssignPins"                  ,
  FunctionAssignPinsAuto              ,ID_ASSIGN_PINS2                  ,(int32)'A'            ,0,-1,"FunctionAssignPinsAuto"              ,
  FunctionAddLinePlacementOutline     ,ID_ADD_LINE_OBJECT+
                                       PLACEMENT_OUTLINE_LAYER          ,(int32)'l'            ,0,-1,"FunctionAddLinePlacementOutline"     ,
  FunctionAddLineComponentOutline     ,ID_ADD_LINE_OBJECT+
                                       COMP_OUTLINE_LAYER               ,(int32)'L'            ,0,-1,"FunctionAddLineComponentOutline"     ,
  FunctionAddLineSilkScreen           ,ID_ADD_LINE_OBJECT+
                                       SILKSCREEN_TOP_LAYER             ,0                     ,0,-1,"FunctionAddLineSilkScreen"           ,
  FunctionAddRectPlacementOutline     ,ID_ADD_RECT_OBJECT+
                                       PLACEMENT_OUTLINE_LAYER          ,0                     ,0,-1,"FunctionAddRectPlacementOutline"     ,
  FunctionAddRectComponentOutline     ,ID_ADD_RECT_OBJECT+
                                       COMP_OUTLINE_LAYER               ,0                     ,0,-1,"FunctionAddRectComponentOutline"     ,
  FunctionAddRectSilkScreen           ,ID_ADD_RECT_OBJECT+
                                       SILKSCREEN_TOP_LAYER             ,0                     ,0,-1,"FunctionAddRectSilkScreen"           ,
  FunctionAddRectPlacementOutline2    ,ID_ADD_RECT2_OBJECT+
                                       PLACEMENT_OUTLINE_LAYER          ,(int32)'r'            ,0,-1,"FunctionAddRectPlacementOutline2"    ,
  FunctionAddRectComponentOutline2    ,ID_ADD_RECT2_OBJECT+
                                       COMP_OUTLINE_LAYER               ,0                     ,0,-1,"FunctionAddRectComponentOutline2"    ,
  FunctionAddRectSilkScreen2          ,ID_ADD_RECT2_OBJECT+
                                       SILKSCREEN_TOP_LAYER             ,0                     ,0,-1,"FunctionAddRectSilkScreen2"          ,
//  FunctionAddCirclePlacementOutline   ,ID_ADD_CIRCLE_PLACE_F            ,0                     ,0,-1,"FunctionAddCirclePlacementOutline"   ,
//  FunctionAddCircleComponentOutline   ,ID_ADD_CIRCLE_OBJECT_F+
//                                       COMP_OUTLINE_LAYER               ,(int32)'C'            ,0,-1,"FunctionAddCircleComponentOutline"   ,
//  FunctionAddCircleSilkScreen         ,ID_ADD_CIRCLE_SILK_TOP_F         ,0                     ,0,-1,"FunctionAddCircleSilkScreen"         ,
  FunctionAddArcPlacementOutline      ,ID_ADD_ARC_OBJECT+
                                       PLACEMENT_OUTLINE_LAYER          ,0                     ,0,-1,"FunctionAddArcPlacementOutline"      ,
  FunctionAddArcComponentOutline      ,ID_ADD_ARC_OBJECT+
                                       COMP_OUTLINE_LAYER               ,0                     ,0,-1,"FunctionAddArcComponentOutline"      ,
  FunctionAddArcSilkScreen            ,ID_ADD_ARC_OBJECT+
                                       SILKSCREEN_TOP_LAYER             ,0                     ,0,-1,"FunctionAddArcSilkScreen"            ,
  FunctionAddTextCompOutline          ,ID_ADD_TEXT_OBJECT+
                                       COMP_OUTLINE_LAYER               ,(int32)'t'            ,0,-1,"FunctionAddTextCompOutline"          ,
  FunctionAddTextSilkScreen           ,ID_ADD_TEXT_OBJECT+
                                       SILKSCREEN_TOP_LAYER             ,(int32)'T'            ,0,-1,"FunctionAddTextSilkScreen"           ,
  FunctionAddDrillHole                ,ID_ADD_DRILL                     ,0                     ,0,-1,"FunctionAddDrillHole"                ,
  FunctionAddUnplatedDrillHole        ,ID_ADD_DRILL_UNPLATED            ,0                     ,0,-1,"FunctionAddUnplatedDrillHole"        ,
//  FunctionAddTraceBottom              ,ID_ADD_LINE_BOTTOM               ,0                     ,0,-1,"FunctionAddTraceBottom"              ,
//  FunctionAddTraceSolderMaskBottom    ,ID_ADD_LINE_BOTTOM_MASK          ,0                     ,0,-1,"FunctionAddTraceSolderMaskBottom"    ,
//  FunctionAddTracePasteMaskBottom     ,ID_ADD_LINE_BOTTOM_PASTE         ,0                     ,0,-1,"FunctionAddTracePasteMaskBottom"     ,
//  FunctionAddTraceTop                 ,ID_ADD_LINE_TOP                  ,0                     ,0,-1,"FunctionAddTraceTop"                 ,
//  FunctionAddTraceSolderMaskTop       ,ID_ADD_LINE_TOP_MASK             ,0                     ,0,-1,"FunctionAddTraceSolderMaskTop"       ,
//  FunctionAddTracePasteMaskTop        ,ID_ADD_LINE_TOP_PASTE            ,0                     ,0,-1,"FunctionAddTracePasteMaskTop"        ,
//  FunctionAddCircleTop                ,ID_ADD_CIRCLE_PAD_TOP            ,0                     ,0,-1,"FunctionAddCircleTop"                ,
//  FunctionAddCircleBottom             ,ID_ADD_CIRCLE_PAD_BOTTOM         ,0                     ,0,-1,"FunctionAddCircleBottom"             ,
//  FunctionAddCircleSolderMaskTop      ,ID_ADD_CIRCLE_PAD_TOP_MASK       ,0                     ,0,-1,"FunctionAddCircleSolderMaskTop"      ,
//  FunctionAddCirclePasteMaskTop       ,ID_ADD_CIRCLE_PAD_TOP_PASTE      ,0                     ,0,-1,"FunctionAddCirclePasteMaskTop"       ,
//  FunctionAddCircleSolderMaskBottom   ,ID_ADD_CIRCLE_PAD_BOTTOM_MASK    ,0                     ,0,-1,"FunctionAddCircleSolderMaskBottom"   ,
//  FunctionAddCirclePasteMaskBottom    ,ID_ADD_CIRCLE_PAD_BOTTOM_PASTE   ,0                     ,0,-1,"FunctionAddCirclePasteMaskBottom"    ,
//  FunctionAddCircleAntiPowerPad       ,ID_ADD_CIRCLE_PAD_ANTI_POWER     ,0                     ,0,-1,"FunctionAddCircleAntiPowerPad"       ,
//  FunctionAddRectTop                  ,ID_ADD_RECT_PAD_TOP              ,0                     ,0,-1,"FunctionAddRectTop"                  ,
//  FunctionAddRectBottom               ,ID_ADD_RECT_PAD_BOTTOM           ,0                     ,0,-1,"FunctionAddRectBottom"               ,
//  FunctionAddRectSolderMaskTop        ,ID_ADD_RECT_PAD_TOP_MASK         ,0                     ,0,-1,"FunctionAddRectSolderMaskTop"        ,
//  FunctionAddRectPasteMaskTop         ,ID_ADD_RECT_PAD_TOP_PASTE        ,0                     ,0,-1,"FunctionAddRectPasteMaskTop"         ,
//  FunctionAddRectSolderMaskBottom     ,ID_ADD_RECT_PAD_BOTTOM_MASK      ,0                     ,0,-1,"FunctionAddRectSolderMaskBottom"     ,
//  FunctionAddRectPasteMaskBottom      ,ID_ADD_RECT_PAD_BOTTOM_PASTE     ,0                     ,0,-1,"FunctionAddRectPasteMaskBottom"      ,
  FunctionInsertPoint                 ,ID_EDIT_INSERTPOINT              ,(int32)'.'            ,0,-1,"FunctionInsertPoint"                 ,
  FunctionCopyToClipboard             ,ID_COPY_TO_CLIPBOARD             ,Key_Ctrl_Ins          ,0,-1,"FunctionCopyToClipboard"             ,
  FunctionCopyFromClipboard           ,ID_COPY_FROM_CLIPBOARD           ,Key_Shift_Ins         ,0,-1,"FunctionCopyFromClipboard"           ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  0x0000                              ,0x0000                           ,0                     ,0,-1,""                                    ,
  };




int32 KeysExp[26] =
      {0,0,0,0,0,0,0,0,0,0,1,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0};

// *INDENT-ON*
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void DecodeKey(int32 toets)
{
	int32 hulp, hulp2;


	if (CurrentExpKey == 0)
	{
		if ((toets >= 0x200 + 'A') && (toets <= 0x200 + 'Z'))
		{
			hulp2 = (toets & 0xFF) - 0x40;
			CurrentExpKey = (uint8) (KeysExp[hulp2 - 1] * hulp2);

			if (CurrentExpKey > 0)
				return;
		}
	}
	else
	{
		hulp = 0;
		toets &= 0xFF;

		if ((isdigit(toets)) || (isalpha(toets)))
		{
			hulp = toets;

			if (isalpha(toets))
				hulp &= 0xdf;
		}

		toets = 0;

		if (hulp > 0)
			toets = hulp + CurrentExpKey * 0x100 + 0x1000;

		CurrentExpKey = 0;
	}

	/*
	  if ((toets>=32)
	     &&
	     (toets<127)) {
	    InsertFunction(toets);
	    return;
	  }
	*/
	InsertFunction(toets);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 KeyPressed()
{
	MSG M;

	while (PeekMessage(&M, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE))
	{
		TranslateMessage(&M);
		DispatchMessage(&M);
	}

	if (NrFunctionsInBuf > 0)
		return 1;
	else
		return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 ReadKeyFunction()
{
	int32 toets;

	if ((NrFunctionsInBuf == 0) && (!KeyPressed()))
		do
		{
			WaitMessage();
		}
		while (!KeyPressed());

	if (NrFunctionsInBuf > 0)
	{
		toets = Functionbuf[0];
		NrFunctionsInBuf--;
		memmove(&Functionbuf[0], &Functionbuf[1], (MaxNrKeysInBuf - 1) * 4);
		return toets;
	}

	return 0;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 GetFunctionByTranslatedKey(int32 Key, int32 SystemMode)
{
	int32 cnt, FoundFunction[10], FoundModes[10], NrFoundKeys, DefaultFunction;

	NrFoundKeys = 0;
	DefaultFunction = 0;

	for (cnt = 0; cnt < MaxFunctions; cnt++)
	{
		if (Key == KeyFunctions[cnt].Key)
		{
			if (KeyFunctions[cnt].SystemFunction != 0)
			{
				if (KeyFunctions[cnt].FunctionMode == -1)
				{
					if (DefaultFunction == 0)
						DefaultFunction = KeyFunctions[cnt].SystemFunction;
				}

				if (NrFoundKeys < 10)
				{
					FoundFunction[NrFoundKeys] = KeyFunctions[cnt].SystemFunction;
					FoundModes[NrFoundKeys++] = KeyFunctions[cnt].FunctionMode;
				}
			}
		}
	}

	if (NrFoundKeys == 0)
		return 0;

//  if (DefaultFunction!=0) return DefaultFunction;
	for (cnt = 0; cnt < NrFoundKeys; cnt++)
	{
		if (SystemMode == FoundModes[cnt])
			return FoundFunction[cnt];
	}

	return DefaultFunction;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void KeyChar(int32 VtKey)
{
	int32 toets;

	toets = 0;

	if ((VtKey >= ' ') && (VtKey <= '~'))
		toets = VtKey;

	if (isalpha(toets))
	{
		toets &= 0xdf;

		if (!ShiftPressed)
			toets |= 0x20;
	}

	if (toets > 0)
		DecodeKey(toets);
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void KeyDown(int32 VtKey)
{
	int32 toets;


	toets = 0;

	if ((VtKey >= 'A') && (VtKey <= 'Z') && ((AltPressed) || (CtrlPressed)))
	{
		toets = VtKey;

		if (AltPressed)
			toets += 0x400;

		if (CtrlPressed)
			toets += 0x200;

		if (ShiftPressed)
			toets += 0x100;

		DecodeKey(toets);
		return;
	}

	if ((isdigit(VtKey)) && ((AltPressed) || (CtrlPressed)))
	{
		toets = VtKey;

		if (AltPressed)
			toets += 0x400;

		if (CtrlPressed)
			toets += 0x200;

		if (ShiftPressed)
			toets += 0x100;

		DecodeKey(toets);
		return;
	}

	switch (VtKey)
	{
	case vk_Shift:
		if (!ShiftPressed)
			ShiftPressed = 1;

		break;

	case vk_Control:
		if (!CtrlPressed)
			CtrlPressed = 1;

		break;

	case vk_Menu:
		if (!AltPressed)
			AltPressed = 1;

		break;
	}

	switch (VtKey)
	{
	case vk_Back:				/*   0x08   */
	case vk_Tab:				/*   0x09   */
	case vk_Return:			/*   0x0D   */
	case vk_Escape:			/*   0x1B   */
//    case vk_Space:        /*   0x20   */
		toets = VtKey;
		break;

	case vk_Prior:				/*   0x21   */
		toets = 0x80;
		break;

	case vk_Next:				/*   0x22   */
		toets = 0x81;
		break;

	case vk_End:				/*   0x23   */
		toets = 0x82;
		break;

	case vk_Home:				/*   0x24   */
		toets = 0x83;
		break;

	case vk_Left:				/*   0x25   */
		toets = 0x84;
		break;

	case vk_Up:				/*   0x26   */
		toets = 0x85;
		break;

	case vk_Right:				/*   0x27   */
		toets = 0x86;
		break;

	case vk_Down:				/*   0x28   */
		toets = 0x87;
		break;

	case vk_Insert:			/*   0x2D   */
		toets = 0x88;
		break;

	case vk_Delete:			/*   0x2E   */
		toets = 0x89;
		break;

	case 192:					//   `
		toets = '`';

		if (ShiftPressed)
			toets = 0;

		break;

	case 189:					//   -
		toets = '-';

		if (ShiftPressed)
			toets = 0;

		break;

	case 187:					//   =
		toets = '=';

		if (ShiftPressed)
			toets = 0;

		break;

	case 220:					//   '\'
		toets = '\\';

		if (ShiftPressed)
			toets = 0;

		break;

	case 219:					//   [
		toets = '[';

		if (ShiftPressed)
			toets = 0;

		break;

	case 221:					//   ]
		toets = ']';

		if (ShiftPressed)
			toets = 0;

		break;

	case 186:					//   ;
		toets = ';';

		if (ShiftPressed)
			toets = 0;

		break;

	case 222:					//   '
		toets = '\'';

		if (ShiftPressed)
			toets = 0;

		break;

	case 188:					//   ,
		toets = ',';

		if (ShiftPressed)
			toets = 0;

		break;

	case 190:					//   .
		toets = '.';

		if (ShiftPressed)
			toets = 0;

		break;

	case 191:					//   /
		toets = '/';

		if (ShiftPressed)
			toets = 0;

		break;

//    case vk_NumPad0:      /*   0x60   */
//    case vk_NumPad1:      /*   0x61   */
//    case vk_NumPad2:      /*   0x62   */
//    case vk_NumPad3:      /*   0x63   */
//    case vk_NumPad4:      /*   0x64   */
//    case vk_NumPad5:      /*   0x65   */
//    case vk_NumPad6:      /*   0x66   */
//    case vk_NumPad7:      /*   0x67   */
//    case vk_NumPad8:      /*   0x68   */
//    case vk_NumPad9:      /*   0x69   */
//    case vk_Multiply:     /*   0x6A   */
//    case vk_Add:          /*   0x6B   */
//    case vk_Separator:    /*   0x6C   */
//    case vk_Subtract:     /*   0x6D   */
//    case vk_Decimal:      /*   0x6E   */
//    case vk_Divide:       /*   0x6F   */
	case vk_F1:				/*   0x70   */
	case vk_F2:				/*   0x71   */
	case vk_F3:				/*   0x72   */
	case vk_F4:				/*   0x73   */
	case vk_F5:				/*   0x74   */
	case vk_F6:				/*   0x75   */
	case vk_F7:				/*   0x76   */
	case vk_F8:				/*   0x77   */
	case vk_F9:				/*   0x78   */
	case vk_F10:				/*   0x79   */
	case vk_F11:				/*   0x7A   */
	case vk_F12:				/*   0x7B   */
	case vk_F13:				/*   0x7C   */
	case vk_F14:				/*   0x7D   */
	case vk_F15:				/*   0x7E   */
	case vk_F16:				/*   0x7F   */
		toets = 0xA0 + VtKey - 0x70;
		break;
//    case vk_NumLock:      /*   0x90   */
	}


	if (toets == 0)
		return;

	if (ShiftPressed)
		toets += 0x100;

	if (CtrlPressed)
		toets += 0x200;

	if (AltPressed)
		toets += 0x400;

	if ((!ShiftPressed) && (!CtrlPressed) && (!AltPressed))
	{
		if ((toets >= (int32) ' ') && (toets <= (int32) '~'))
			return;

		toets += 0x800;
	}

	DecodeKey(toets);
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void KeyUp(int32 VtKey)
{
	switch (VtKey)
	{
	case vk_Shift:
		if (ShiftPressed)
			ShiftPressed = 0;

		break;

	case vk_Control:
		if (CtrlPressed)
			CtrlPressed = 0;

		break;

	case vk_Menu:
		if (AltPressed)
			AltPressed = 0;

		break;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void InsertFunction(int32 Func)
{
	if ((Func != 0) && (NrFunctionsInBuf < MaxNrKeysInBuf - 1))
	{
		Functionbuf[NrFunctionsInBuf] = Func;
		NrFunctionsInBuf++;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetKeyValue(LPSTR KeyString)
{
	char TempStr[MAX_LENGTH_STRING], str[MAX_LENGTH_STRING], str3[MAX_LENGTH_STRING], KeyNameString2[MAX_LENGTH_STRING];
	int32 Length, cnt, result, l2;

	result = 0;
	memset(&TempStr, 0, MAX_LENGTH_STRING);
	strcpy(TempStr, KeyString);
	strupr(TempStr);
	strcpy(KeyNameString2, KeyNameString);
	strupr(KeyNameString2);

	Length = strlen(TempStr);
	l2 = strlen(KeyNameString2);

	if (Length < l2 + 2)
		return 0;

	for (cnt = 0; cnt < l2; cnt++)
	{
		if (TempStr[cnt] != KeyNameString2[cnt])
			return 0;
	}

	if (TempStr[l2] != '_')
		return 0;

	Length -= l2 + 1;
	memmove(&TempStr[0], &TempStr[l2 + 1], 100);

	if (Length == 1)
		return (int32) KeyString[l2 + 1];

	if (stricmp(TempStr, SpaceString) == 0)
		return (int32) ' ';

// *****************************************************************************
// *****************************************************************************

	for (cnt = 0; cnt < 3; cnt++)
	{
// CTRL_
		strcpy(str, TempStr);
		str[5] = 0;
		strcpy(str3, CtrlString);
		strupr(str3);
		strcat(str3, "_");

		if ((Length > 5) && (strcmp(str, str3) == 0))
		{
			Length -= 5;
			memmove(&TempStr[0], &TempStr[5], 100);
			result |= 0x200;
		}

// SHIFT_
		strcpy(str, TempStr);
		str[6] = 0;
		strcpy(str3, ShiftString);
		strupr(str3);
		strcat(str3, "_");

		if ((Length > 6) && (strcmp(str, str3) == 0))
		{
			Length -= 6;
			memmove(&TempStr[0], &TempStr[6], 100);
			result |= 0x100;
		}

// ALT_
		strcpy(str, TempStr);
		str[4] = 0;
		strcpy(str3, AltString);
		strupr(str3);
		strcat(str3, "_");

		if ((Length > 4) && (strcmp(str, str3) == 0))
		{
			Length -= 4;
			memmove(&TempStr[0], &TempStr[4], 100);
			result |= 0x400;
		}
	}

// *****************************************************************************
// *****************************************************************************

	if (Length == 1)
		return (result + (int32) TempStr[0]);

	if ((Length == 3) && (result == 0x200) && (isalpha(TempStr[0])) && (TempStr[1] == '_')
	        && ((isalpha(TempStr[2])) || (isdigit(TempStr[2]))))
	{
		result = 0x1000 + (TempStr[0] - '@') * 0x100 + TempStr[2];
		return result;
	}

// *****************************************************************************
// *****************************************************************************

	for (cnt = 0; cnt < 32; cnt++)
	{
		if (stricmp(TempStr, KeyStrings[cnt].KeyStringPtr) == 0)
		{
			if (result == 0)
				return (result + KeyStrings[cnt].KeyValue);
			else
				return (result + (KeyStrings[cnt].KeyValue & 0xff));
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 SetKeyOnKeyFunctionString(int32 Key, LPSTR FunctionString)
{
	int32 cnt;
	/*
	  FirstKeyNr=-1;
	  NrKeys=0;
	  for (cnt=0;cnt<MaxFunctions;cnt++) {
	    if (Key==KeyFunctions[cnt].Key) {
	      if (KeyFunctions[cnt].SystemFunction!=0) {
	        if (KeyFunctions[cnt].FunctionMode!=-1) {
	          NrKeys++;
	          if (FirstKeyNr==-1) {
	            FirstKeyNr=cnt;
	          }
	        }
	      }
	    }
	  }
	  if (NrKeys>0) {
	    return -1;
	  }
	*/
	cnt = 0;

	while ((cnt < MaxFunctions) && (stricmp(KeyFunctions[cnt].FunctionString, FunctionString) != 0))
		cnt++;

	if (cnt == MaxFunctions)
		return 0;

	KeyFunctions[cnt].Key = Key;
	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetKeyStringByKey(LPSTR KeyString, int32 Key)
{
	char str[MAX_LENGTH_STRING], str2[MAX_LENGTH_STRING];
	int32 cnt, Key1, Key2;

	KeyString[0] = 0;

	if ((Key >= ' ') && (Key < 128))
	{
		if (Key != ' ')
			sprintf(KeyString, "%s_%c", KeyNameString, Key);
		else
			sprintf(KeyString, "%s_%s", KeyNameString, SpaceString);

		return 1;
	}

	sprintf(str, "%s_", KeyNameString);
	str2[0] = 0;

	if (Key < 0x1000)
	{
		for (cnt = 0; cnt < 32; cnt++)
		{
			if ((Key & 0xff) == (KeyStrings[cnt].KeyValue & 0xff))
				strcpy(str2, KeyStrings[cnt].KeyStringPtr);
		}

		if (str2[0] == 0)
		{
			if (((Key & 0xff) >= ' ') && ((Key & 0xff) < 128))
			{
				str2[0] = (char) (Key & 0xff);
				str2[1] = 0;
			}
		}

		if (str2[0] == 0)
			return 0;

		if ((Key & 0x0800) != 0x0800)
		{
			if ((Key & 0x0200) == 0x0200)
			{
				strcat(str, CtrlString);
				strcat(str, "_");

				if (str2[1] == 0)
					strupr(str2);
			}

			if ((Key & 0x0100) == 0x0100)
			{
				strcat(str, ShiftString);
				strcat(str, "_");

				if (str2[1] == 0)
					strupr(str2);
			}

			if ((Key & 0x0400) == 0x0400)
			{
				strcat(str, AltString);
				strcat(str, "_");

				if (str2[1] == 0)
					strupr(str2);
			}
		}

		strcat(str, str2);
		strcpy(KeyString, str);
		return 1;
	}
	else
	{
		Key1 = ((Key >> 8) & 0xff) - 0x10 + '@';
		Key2 = Key & 0xff;

		if (!isalpha(Key1))
			return 0;

		if ((!isalpha(Key2)) && (!isdigit(Key2)))
			return 0;

		sprintf(KeyString, "_%s_%s_%c_%c", KeyNameString, CtrlString, Key1 + 0x20, Key2 + 0x20);
		return 1;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************


int32 GetKeyString(LPSTR KeyString, int32 Function, int32 mode)
{
	int32 cnt, cnt2, res, Length;

	switch (mode & 0x0f)
	{
	case 0:
		cnt = 0;

		while ((cnt < MaxFunctions) && (Function != KeyFunctions[cnt].KeyFunction))
			cnt++;

		if (cnt < MaxFunctions)
			return GetKeyStringByKey(KeyString, KeyFunctions[cnt].Key);

		return GetKeyStringByKey(KeyString, Function);
		break;

	case 1:
		cnt = 0;

		while ((cnt < MaxFunctions) && (Function != KeyFunctions[cnt].SystemFunction))
			cnt++;

		if (cnt == MaxFunctions)
			return 0;

		res = GetKeyStringByKey(KeyString, KeyFunctions[cnt].Key);

		if (res == 0)
			return res;

		if (KeyString[0] == 0)
			return 0;

		if ((mode & 0x30) != 0)
		{
			Length = strlen(KeyString);

			for (cnt2 = 0; cnt2 < Length; cnt2++)
			{
				if (KeyString[cnt2] == '_')
					KeyString[cnt2] = ' ';
			}

			if ((mode & 0x30) == 0x10)
			{
				memmove(&KeyString[0], &KeyString[3], 100);
				KeyString[0] = '\t';
			}

			if ((mode & 0x30) == 0x20)
			{
				memmove(&KeyString[2], &KeyString[0], 100);
				KeyString[0] = ' ';
				KeyString[1] = ' ';
				KeyString[2] = ' ';
				KeyString[3] = ' ';
				KeyString[4] = ' ';
				KeyString[5] = ' ';
			}
		}

		return 1;

	case 2:

	/*
	      if (Function<MaxFunctions) {
	        return GetKeyStringByKey(KeyString,KeyFunctions[Function].Key);
	      }
	      return -1;
	*/
	case 3:
		if (Function < MaxFunctions)
		{
			strcpy(KeyString, KeyFunctions[Function].FunctionString);
			return 1;
		}

		return 0;

	case 4:
		if (Function < MaxFunctions)
			return KeyFunctions[Function].Key;

		return 0;

	case 5:
		if (Function < MaxFunctions)
			return GetKeyStringByKey(KeyString, KeyFunctions[Function].Key);

		return -1;
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetSystemFunction(int32 KeyFunction, int32 mode)
{
	int32 cnt;

	cnt = 0;

	while ((cnt < MaxFunctions) && (KeyFunction != KeyFunctions[cnt].KeyFunction))
		cnt++;

	if (cnt < MaxFunctions)
		return KeyFunctions[cnt].SystemFunction;

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckExpandedCtrlKeys(int32 mode)
{
	int32 cnt, Key, Key1;

	memset(&KeysExp, 0, sizeof(KeysExp));

	for (cnt = 0; cnt < MaxFunctions; cnt++)
	{
		Key = KeyFunctions[cnt].Key;

		if (Key >= 0x1000)
		{
			Key1 = ((Key >> 8) & 0xff) - 0x10 + '@';
			KeysExp[Key1 - 'A'] = 1;
		}
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadDefaultKeys(int32 mode)
{
	int32 cnt, Key;

	for (cnt = 0; cnt < MaxFunctions; cnt++)
	{
		Key = KeyFunctions[cnt].DefaultKey;
		KeyFunctions[cnt].Key = Key;
	}

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ToetsMain()
{
	int32 cnt;

	KeyStrings[0].KeyStringPtr = "PageDown";
	KeyStrings[1].KeyStringPtr = "PageUp";
	KeyStrings[2].KeyStringPtr = "Home";
	KeyStrings[3].KeyStringPtr = "End";
	KeyStrings[4].KeyStringPtr = "Del";
	KeyStrings[5].KeyStringPtr = "Ins";
	KeyStrings[6].KeyStringPtr = "CursorRight";
	KeyStrings[7].KeyStringPtr = "CursorLeft";
	KeyStrings[8].KeyStringPtr = "CursorUp";
	KeyStrings[9].KeyStringPtr = "CursorDown";
	KeyStrings[10].KeyStringPtr = "Esc";
	KeyStrings[11].KeyStringPtr = "BackSpace";
	KeyStrings[12].KeyStringPtr = "Enter";
	KeyStrings[13].KeyStringPtr = "Tab";
	KeyStrings[14].KeyStringPtr = "F1";
	KeyStrings[15].KeyStringPtr = "F2";
	KeyStrings[16].KeyStringPtr = "F3";
	KeyStrings[17].KeyStringPtr = "F4";
	KeyStrings[18].KeyStringPtr = "F5";
	KeyStrings[19].KeyStringPtr = "F6";
	KeyStrings[20].KeyStringPtr = "F7";
	KeyStrings[21].KeyStringPtr = "F8";
	KeyStrings[22].KeyStringPtr = "F9";
	KeyStrings[23].KeyStringPtr = "F10";
	KeyStrings[24].KeyStringPtr = "F11";
	KeyStrings[25].KeyStringPtr = "F12";
	KeyStrings[26].KeyStringPtr = "";
	KeyStrings[27].KeyStringPtr = "";
	KeyStrings[28].KeyStringPtr = "";
	KeyStrings[29].KeyStringPtr = "";
	KeyStrings[30].KeyStringPtr = "";
	KeyStrings[31].KeyStringPtr = "";

	for (cnt = 0; cnt < MaxFunctions; cnt++)
		KeyFunctions[cnt].DefaultKey = KeyFunctions[cnt].Key;

	strcpy(ShiftString, "Shift");
	strcpy(CtrlString, "Ctrl");
	strcpy(AltString, "Alt");
	strcpy(SpaceString, "Space");
	strcpy(KeyNameString, "Key");

	TranslateKey = 1;
}
