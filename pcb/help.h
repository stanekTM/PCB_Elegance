/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: help.h
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



#ifndef _HELP

#define _HELP

#include "types.h"


#define IDH_Layout_editor                                   30000
#define IDH_Contents                                        30001
#define IDH_Viewable_window                                 30002
#define IDH_Getting_started                                 30003
#define IDH_Make_new_layout                                 30005
#define IDH_Reload_geometries                               30006
#define IDH_Design_rules_printer                            30007
#define IDH_Importing_components_netlist                    30008
#define IDH_Updating_components_netlist                     30009
#define IDH_Plot_output_to_gerber_format                    30010
#define IDH_Plot_output_to_printer                          30011
#define IDH_Initialisation_file_pcb_ini                     30012
#define IDH_Thermal_relief                                  30013
#define IDH_Change_design_rules                             30015
#define IDH_Select_component                                30016
#define IDH_Via_definition                                  30017
#define IDH_Zero_relative_cursor                            30018
#define IDH_Undo                                            30019
#define IDH_Redo                                            30020
#define IDH_Hide_view_layers                                30022
#define IDH_Zoom_in                                         30023
#define IDH_Zoom_out                                        30024
#define IDH_Window_based_zooming                            30025
#define IDH_Pan_window                                      30026
#define IDH_Window_based_panning                            30027
#define IDH_Unhighlight_all                                 30028
#define IDH_Options                                         30029
#define IDH_Return_to_previous_view_window                  30030
#define IDH_Repaint                                         30031
#define IDH_View_whole_design                               30032
#define IDH_Selecting_deselecting_objects                   30033
#define IDH_Deselect_all                                    30034
#define IDH_Info_on_selected_objects                        30035
#define IDH_Dialogbox_selections                            30036
#define IDH_Components                                      30038
#define IDH_Move_components                                 30039
#define IDH_Rotate_components                               30040
#define IDH_Move_components_to_other_layer                  30041
#define IDH_Regroup_components                              30042
#define IDH_Change_component_parameters                     30043
#define IDH_Component_info                                  30044
#define IDH_Delete_traces_vias_nets                         30046
#define IDH_Change_design_rules_nets                        30047
#define IDH_Highlight_unhighlight_nets                      30048
#define IDH_Highlight_unhighlight_net_during_trace_drawing  30049
#define IDH_Disable_connections_nets                        30050
#define IDH_Hide_connections_nets                           30051
#define IDH_Unselect_traces_vias_nets                       30052
#define IDH_Hide_all_connections                            30054
#define IDH_View_all_connections                            30055
#define IDH_Highlight_visible_connections                   30056
#define IDH_Traces_vias                                     30058
#define IDH_Add_trace                                       30059
#define IDH_Trace_popup_menu                                30075
#define IDH_Add_via                                         30060
#define IDH_Trace_drawing_feature                           30061
#define IDH_Add_extra_trace                                 30062
#define IDH_Add_extra_objects_on_top_bottom_layer           30063
#define IDH_Display_clearance                               30064
#define IDH_Display_two_trying_traces                       30065
#define IDH_Display_via_option                              30066
#define IDH_Finish_trace                                    30067
#define IDH_Change_cross_hair_of_the_mouse_cursor           30068
#define IDH_Select_only                                     30069
#define IDH_Change_trace_width2                             30070
#define IDH_Change_trace_clearance2                         30071
#define IDH_Delete_trace                                    30072
#define IDH_Goto_previous_trace_segment                     30073
#define IDH_Switch_to_another_layer                         30074
#define IDH_Change_traces_vias                              30076
#define IDH_Move_traces_vias                                30077
#define IDH_Copy_traces_vias                                30078
#define IDH_Change_trace_width                              30079
#define IDH_Change_clearance_traces_vias                    30080
#define IDH_Change_via                                      30081
#define IDH_Calculate_length_trace                          30082
#define IDH_Swap_traces_vias_two_nets                       30083
#define IDH_Delete_traces_vias_net_selected_trace           30084
#define IDH_Traces_vias_delete                              30085
#define IDH_Drag_one_trace                                  30087
#define IDH_Dragging_traces_vias_components                 30089
#define IDH_Check                                           30091
#define IDH_Check_connectivity                              30092
#define IDH_Check_design_rules                              30093
#define IDH_View_design_rule_errors                         30094
#define IDH_Powerplanes                                     30096
#define IDH_Add_powerplane                                  30097
#define IDH_Cut_from_powerplane                             30098
#define IDH_Change_powerplane                               30099
#define IDH_Delete_powerplane                               30100
#define IDH_Areafills                                       30102
#define IDH_Add_areafills                                   30103
#define IDH_Add_areafill_inside_a_powerplane                30104
#define IDH_Change_areafill                                 30105
#define IDH_Change_hatch_areafill                           30106
#define IDH_Cut_from_areafill                               30107
#define IDH_Delete_areafill                                 30108
#define IDH_Moving_component_references                     30110
#define IDH_Moving_component_values                         30112
#define IDH_Special_objects                                 30114
#define IDH_Schematic_link                                  30116
#define IDH_Change_units                                    30200
#define IDH_Change_special_objects                          30201
#define IDH_Add_special_objects                             30202
#define IDH_Export_component_positions                      30203
#define IDH_Output_netlist                                  30204
#define IDH_Protect_components                              30205
#define IDH_Component_Protection                            30206
#define IDH_Print_screen                                    30207
#define IDH_Delete_components                               30208
#define IDH_Rebuild_areafill                                30209
#define IDH_Penplot_output                                  30210
#define IDH_Change_colors                                   30211
#define IDH_Load_default_colors                             30212
#define IDH_Gate_pin_swap                                   30217
#define IDH_Recalc_areafill_after_insert                    30218
#define IDH_Move_component_by_reference                     30219
#define IDH_Merge_areafills                                 30220
#define IDH_Search_first_unrouted_net                       30221
#define IDH_Change_geometry                                 30222
#define IDH_Insert_layer                                    30223
#define IDH_Remove_layer                                    30224
#define IDH_Swap_layer                                      30225
#define IDH_ImportDXF                                       30240
#define IDH_ExportDXF                                       30241
#define IDH_Stretch_areafill                                30243
#define IDH_Move_areafill                                   30244
#define IDH_Copy_areafill                                   30245
#define IDH_View_vertices_areafill                          30246
#define IDH_Copy_objects_array                              30247
#define IDH_Copy_objects_polar                              30248
#define IDH_Export_pdf                                      30249
#define IDH_Export_bmp                                      30250
#define IDH_Show_next_error                                 30251
#define IDH_Route_all_angle                                 30252
#define IDH_Route_arc90                                     30253
#define IDH_Route_arc45                                     30254
#define IDH_Copy_traces_vias_to_clipboard                   30255
#define IDH_Copy_traces_vias_from_clipboard                 30256
#define IDH_Import_traces_vias_from_gerber                  30257
#define IDH_Add_to_areafill                                 30258
#define IDH_Mirror_areafill_x                               30259
#define IDH_Mirror_areafill_y                               30260
#define IDH_Import_gerber                                   30261
#define IDH_Import_bitmap                                   30262
#define IDH_Assign_net_to_objects                           30263
#define IDH_Assign_no_net_to_objects                        30264
#define IDH_Show_next_connection                            30265
#define IDH_Align_components                                30266
#define IDH_Import_component_positions                      30267
#define IDH_Change_clearance_areafill                       30268
#define IDH_Gotoxy                                          30269
#define IDH_CompSelection                                   30270
#define IDH_CompPlacement                                   30271
#define IDH_Edit_schematic_on_ref                           30272
#define IDH_CompSelectionByList                             30373
#define IDH_Copy_start_polygon_to_info4_layer               30374
#define IDH_Measurement                                     30375

//void Help(int32 Command,int32 mode);
void Help(LPSTR Topic, int32 mode);

#endif
