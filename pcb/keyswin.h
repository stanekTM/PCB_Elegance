/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: keyswin.h
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


#ifndef _KEYSWIN

#define _KEYSWIN

// *****************************************************************************
// *****************************************************************************

#define vk_LButton              0x01
#define vk_RButton              0x02
#define vk_Cancel               0x03
#define vk_MButton              0x04
#define vk_Back                 0x08
#define vk_Tab                  0x09
#define vk_Clear                0x0C
#define vk_Return               0x0D
#define vk_Shift                0x10
#define vk_Control              0x11
#define vk_Menu                 0x12
#define vk_Pause                0x13
#define vk_Capital              0x14
#define vk_Escape               0x1B
#define vk_Space                0x20
#define vk_Prior                0x21
#define vk_Next                 0x22
#define vk_End                  0x23
#define vk_Home                 0x24
#define vk_Left                 0x25
#define vk_Up                   0x26
#define vk_Right                0x27
#define vk_Down                 0x28
#define vk_Select               0x29
#define vk_Print                0x2A
#define vk_Execute              0x2B
#define vk_SnapShot             0x2C
#define vk_Insert               0x2D
#define vk_Delete               0x2E
#define vk_Help                 0x2F
#define vk_NumPad0              0x60
#define vk_NumPad1              0x61
#define vk_NumPad2              0x62
#define vk_NumPad3              0x63
#define vk_NumPad4              0x64
#define vk_NumPad5              0x65
#define vk_NumPad6              0x66
#define vk_NumPad7              0x67
#define vk_NumPad8              0x68
#define vk_NumPad9              0x69
#define vk_Multiply             0x6A
#define vk_Add                  0x6B
#define vk_Separator            0x6C
#define vk_Subtract             0x6D
#define vk_Decimal              0x6E
#define vk_Divide               0x6F
#define vk_F1                   0x70
#define vk_F2                   0x71
#define vk_F3                   0x72
#define vk_F4                   0x73
#define vk_F5                   0x74
#define vk_F6                   0x75
#define vk_F7                   0x76
#define vk_F8                   0x77
#define vk_F9                   0x78
#define vk_F10                  0x79
#define vk_F11                  0x7A
#define vk_F12                  0x7B
#define vk_F13                  0x7C
#define vk_F14                  0x7D
#define vk_F15                  0x7E
#define vk_F16                  0x7F
#define vk_NumLock              0x90
#define vk_BackQuote            0xB0

// *****************************************************************************
// *****************************************************************************

#define Key_Ctrl_A              0x0241
#define Key_Ctrl_B              0x0242
#define Key_Ctrl_C              0x0243
#define Key_Ctrl_D              0x0244
#define Key_Ctrl_E              0x0245
#define Key_Ctrl_F              0x0246
#define Key_Ctrl_G              0x0247
#define Key_Ctrl_H              0x0248
#define Key_Ctrl_I              0x0249
#define Key_Ctrl_J              0x024A
#define Key_Ctrl_K              0x024B
#define Key_Ctrl_L              0x024C
#define Key_Ctrl_M              0x024D
#define Key_Ctrl_N              0x024E
#define Key_Ctrl_O              0x024F
#define Key_Ctrl_P              0x0250
#define Key_Ctrl_Q              0x0251
#define Key_Ctrl_R              0x0252
#define Key_Ctrl_S              0x0253
#define Key_Ctrl_T              0x0254
#define Key_Ctrl_U              0x0255
#define Key_Ctrl_V              0x0256
#define Key_Ctrl_W              0x0257
#define Key_Ctrl_X              0x0258
#define Key_Ctrl_Y              0x0259
#define Key_Ctrl_Z              0x025A

#define Key_Ctrl_1              0x0231
#define Key_Ctrl_2              0x0232
#define Key_Ctrl_3              0x0233
#define Key_Ctrl_4              0x0234
#define Key_Ctrl_5              0x0235
#define Key_Ctrl_6              0x0236
#define Key_Ctrl_7              0x0237
#define Key_Ctrl_8              0x0238
#define Key_Ctrl_9              0x0239
#define Key_Ctrl_0              0x0230

// *****************************************************************************
// *****************************************************************************

#define Key_Ctrl_Shift_A        0x0341
#define Key_Ctrl_Shift_B        0x0342
#define Key_Ctrl_Shift_C        0x0343
#define Key_Ctrl_Shift_D        0x0344
#define Key_Ctrl_Shift_E        0x0345
#define Key_Ctrl_Shift_F        0x0346
#define Key_Ctrl_Shift_G        0x0347
#define Key_Ctrl_Shift_H        0x0348
#define Key_Ctrl_Shift_I        0x0349
#define Key_Ctrl_Shift_J        0x034A
#define Key_Ctrl_Shift_K        0x034B
#define Key_Ctrl_Shift_L        0x034C
#define Key_Ctrl_Shift_M        0x034D
#define Key_Ctrl_Shift_N        0x034E
#define Key_Ctrl_Shift_O        0x034F
#define Key_Ctrl_Shift_P        0x0350
#define Key_Ctrl_Shift_Q        0x0351
#define Key_Ctrl_Shift_R        0x0352
#define Key_Ctrl_Shift_S        0x0353
#define Key_Ctrl_Shift_T        0x0354
#define Key_Ctrl_Shift_U        0x0355
#define Key_Ctrl_Shift_V        0x0356
#define Key_Ctrl_Shift_W        0x0357
#define Key_Ctrl_Shift_X        0x0358
#define Key_Ctrl_Shift_Y        0x0359
#define Key_Ctrl_Shift_Z        0x035A

#define Key_Ctrl_Shift_1        0x0331
#define Key_Ctrl_Shift_2        0x0332
#define Key_Ctrl_Shift_3        0x0333
#define Key_Ctrl_Shift_4        0x0334
#define Key_Ctrl_Shift_5        0x0335
#define Key_Ctrl_Shift_6        0x0336
#define Key_Ctrl_Shift_7        0x0337
#define Key_Ctrl_Shift_8        0x0338
#define Key_Ctrl_Shift_9        0x0339
#define Key_Ctrl_Shift_0        0x0330

// *****************************************************************************
// *****************************************************************************

#define Key_Alt_A               0x0441
#define Key_Alt_B               0x0442
#define Key_Alt_C               0x0443
#define Key_Alt_D               0x0444
#define Key_Alt_E               0x0445
#define Key_Alt_F               0x0446
#define Key_Alt_G               0x0447
#define Key_Alt_H               0x0448
#define Key_Alt_I               0x0449
#define Key_Alt_J               0x044A
#define Key_Alt_K               0x044B
#define Key_Alt_L               0x044C
#define Key_Alt_M               0x044D
#define Key_Alt_N               0x044E
#define Key_Alt_O               0x044F
#define Key_Alt_P               0x0450
#define Key_Alt_Q               0x0451
#define Key_Alt_R               0x0452
#define Key_Alt_S               0x0453
#define Key_Alt_T               0x0454
#define Key_Alt_U               0x0455
#define Key_Alt_V               0x0456
#define Key_Alt_W               0x0457
#define Key_Alt_X               0x0458
#define Key_Alt_Y               0x0459
#define Key_Alt_Z               0x045A

#define Key_Alt_1               0x0431
#define Key_Alt_2               0x0432
#define Key_Alt_3               0x0433
#define Key_Alt_4               0x0434
#define Key_Alt_5               0x0435
#define Key_Alt_6               0x0436
#define Key_Alt_7               0x0437
#define Key_Alt_8               0x0438
#define Key_Alt_9               0x0439
#define Key_Alt_0               0x0430

// *****************************************************************************
// *****************************************************************************

#define Key_F1                  0x08A0
#define Key_F2                  0x08A1
#define Key_F3                  0x08A2
#define Key_F4                  0x08A3
#define Key_F5                  0x08A4
#define Key_F6                  0x08A5
#define Key_F7                  0x08A6
#define Key_F8                  0x08A7
#define Key_F9                  0x08A8
#define Key_F10                 0x08A9
#define Key_F11                 0x08AA
#define Key_F12                 0x08AB

// *****************************************************************************
// *****************************************************************************

#define Key_Shift_F1            0x01A0
#define Key_Shift_F2            0x01A1
#define Key_Shift_F3            0x01A2
#define Key_Shift_F4            0x01A3
#define Key_Shift_F5            0x01A4
#define Key_Shift_F6            0x01A5
#define Key_Shift_F7            0x01A6
#define Key_Shift_F8            0x01A7
#define Key_Shift_F9            0x01A8
#define Key_Shift_F10           0x01A9
#define Key_Shift_F11           0x01AA
#define Key_Shift_F12           0x01AB

// *****************************************************************************
// *****************************************************************************

#define Key_Ctrl_F1             0x02A0
#define Key_Ctrl_F2             0x02A1
#define Key_Ctrl_F3             0x02A2
#define Key_Ctrl_F4             0x02A3
#define Key_Ctrl_F5             0x02A4
#define Key_Ctrl_F6             0x02A5
#define Key_Ctrl_F7             0x02A6
#define Key_Ctrl_F8             0x02A7
#define Key_Ctrl_F9             0x02A8
#define Key_Ctrl_F10            0x02A9
#define Key_Ctrl_F11            0x02AA
#define Key_Ctrl_F12            0x02AB

// *****************************************************************************
// *****************************************************************************

#define Key_Alt_F1              0x04A0
#define Key_Alt_F2              0x04A1
#define Key_Alt_F3              0x04A2
/*      Key_Alt_F4              0x04A3 */
#define Key_Alt_F5              0x04A4
#define Key_Alt_F6              0x04A5
#define Key_Alt_F7              0x04A6
#define Key_Alt_F8              0x04A7
#define Key_Alt_F9              0x04A8
#define Key_Alt_F10             0x04A9
#define Key_Alt_F11             0x04AA
#define Key_Alt_F12             0x04AB

// *****************************************************************************
// *****************************************************************************

#define Key_PgDown              0x0881
#define Key_PgUp                0x0880
#define Key_Home                0x0883
#define Key_End                 0x0882
#define Key_Del                 0x0889
#define Key_Ins                 0x0888
#define Key_Shift_PgDown        0x0181
#define Key_Shift_PgUp          0x0180
#define Key_Shift_Home          0x0183
#define Key_Shift_End           0x0182
#define Key_Shift_Del           0x0189
#define Key_Shift_Ins           0x0188
#define Key_Ctrl_PgDown         0x0281
#define Key_Ctrl_PgUp           0x0280
#define Key_Ctrl_Home           0x0283
#define Key_Ctrl_End            0x0282
#define Key_Ctrl_Del            0x0289
#define Key_Ctrl_Ins            0x0288
#define Key_Alt_PgDown          0x0481
#define Key_Alt_PgUp            0x0480
#define Key_Alt_Home            0x0483
#define Key_Alt_End             0x0482
#define Key_Alt_Del             0x0489
#define Key_Alt_Ins             0x0488

// *****************************************************************************
// *****************************************************************************

#define Key_Cursor_Right        0x0886
#define Key_Cursor_Left         0x0884
#define Key_Cursor_Up           0x0885
#define Key_Cursor_Down         0x0887
#define Key_Shift_Cursor_Right  0x0186
#define Key_Shift_Cursor_Left   0x0184
#define Key_Shift_Cursor_Up     0x0185
#define Key_Shift_Cursor_Down   0x0187
#define Key_Ctrl_Cursor_Right   0x0286
#define Key_Ctrl_Cursor_Left    0x0284
#define Key_Ctrl_Cursor_Up      0x0285
#define Key_Ctrl_Cursor_Down    0x0287
#define Key_Alt_Cursor_Right    0x0486
#define Key_Alt_Cursor_Left     0x0484
#define Key_Alt_Cursor_Up       0x0485
#define Key_Alt_Cursor_Down     0x0487

// *****************************************************************************
// *****************************************************************************

#define Key_Esc                 0x081B
#define Key_Alt_Esc             0x041B
#define Key_BackSpace           0x0808
#define Key_Ctrl_BackSpace      0x0208
#define Key_Alt_BackSpace       0x0408
#define Key_Enter               0x080D
#define Key_Ctrl_Enter          0x020D
#define Key_Alt_Enter           0x040D
#define Key_Tab                 0x0809
#define Key_Shift_Tab           0x0109
#define Key_Ctrl_Tab            0x0209
/*      Key_Alt_Tab             0x0409 */

// *****************************************************************************
// *****************************************************************************

#define Key_Ctrl_A_A            0x1141
#define Key_Ctrl_A_B            0x1142
#define Key_Ctrl_A_C            0x1143
#define Key_Ctrl_A_D            0x1144
#define Key_Ctrl_A_E            0x1145
#define Key_Ctrl_A_F            0x1146
#define Key_Ctrl_A_G            0x1147
#define Key_Ctrl_A_H            0x1148
#define Key_Ctrl_A_I            0x1149
#define Key_Ctrl_A_J            0x114A
#define Key_Ctrl_A_K            0x114B
#define Key_Ctrl_A_L            0x114C
#define Key_Ctrl_A_M            0x114D
#define Key_Ctrl_A_N            0x114E
#define Key_Ctrl_A_O            0x114F
#define Key_Ctrl_A_P            0x1150
#define Key_Ctrl_A_Q            0x1151
#define Key_Ctrl_A_R            0x1152
#define Key_Ctrl_A_S            0x1153
#define Key_Ctrl_A_T            0x1154
#define Key_Ctrl_A_U            0x1155
#define Key_Ctrl_A_V            0x1156
#define Key_Ctrl_A_W            0x1157
#define Key_Ctrl_A_X            0x1158
#define Key_Ctrl_A_Y            0x1159
#define Key_Ctrl_A_Z            0x115A

#define Key_Ctrl_A_0            0x1130
#define Key_Ctrl_A_1            0x1131
#define Key_Ctrl_A_2            0x1132
#define Key_Ctrl_A_3            0x1133
#define Key_Ctrl_A_4            0x1134
#define Key_Ctrl_A_5            0x1135
#define Key_Ctrl_A_6            0x1136
#define Key_Ctrl_A_7            0x1137
#define Key_Ctrl_A_8            0x1138
#define Key_Ctrl_A_9            0x1139

#define Key_Ctrl_B_A            0x1241
#define Key_Ctrl_B_B            0x1242
#define Key_Ctrl_B_C            0x1243
#define Key_Ctrl_B_D            0x1244
#define Key_Ctrl_B_E            0x1245
#define Key_Ctrl_B_F            0x1246
#define Key_Ctrl_B_G            0x1247
#define Key_Ctrl_B_H            0x1248
#define Key_Ctrl_B_I            0x1249
#define Key_Ctrl_B_J            0x124A
#define Key_Ctrl_B_K            0x124B
#define Key_Ctrl_B_L            0x124C
#define Key_Ctrl_B_M            0x124D
#define Key_Ctrl_B_N            0x124E
#define Key_Ctrl_B_O            0x124F
#define Key_Ctrl_B_P            0x1250
#define Key_Ctrl_B_Q            0x1251
#define Key_Ctrl_B_R            0x1252
#define Key_Ctrl_B_S            0x1253
#define Key_Ctrl_B_T            0x1254
#define Key_Ctrl_B_U            0x1255
#define Key_Ctrl_B_V            0x1256
#define Key_Ctrl_B_W            0x1257
#define Key_Ctrl_B_X            0x1258
#define Key_Ctrl_B_Y            0x1259
#define Key_Ctrl_B_Z            0x125A

#define Key_Ctrl_B_0            0x1230
#define Key_Ctrl_B_1            0x1231
#define Key_Ctrl_B_2            0x1232
#define Key_Ctrl_B_3            0x1233
#define Key_Ctrl_B_4            0x1234
#define Key_Ctrl_B_5            0x1235
#define Key_Ctrl_B_6            0x1236
#define Key_Ctrl_B_7            0x1237
#define Key_Ctrl_B_8            0x1238
#define Key_Ctrl_B_9            0x1239

#define Key_Ctrl_C_A            0x1341
#define Key_Ctrl_C_B            0x1342
#define Key_Ctrl_C_C            0x1343
#define Key_Ctrl_C_D            0x1344
#define Key_Ctrl_C_E            0x1345
#define Key_Ctrl_C_F            0x1346
#define Key_Ctrl_C_G            0x1347
#define Key_Ctrl_C_H            0x1348
#define Key_Ctrl_C_I            0x1349
#define Key_Ctrl_C_J            0x134A
#define Key_Ctrl_C_K            0x134B
#define Key_Ctrl_C_L            0x134C
#define Key_Ctrl_C_M            0x134D
#define Key_Ctrl_C_N            0x134E
#define Key_Ctrl_C_O            0x134F
#define Key_Ctrl_C_P            0x1350
#define Key_Ctrl_C_Q            0x1351
#define Key_Ctrl_C_R            0x1352
#define Key_Ctrl_C_S            0x1353
#define Key_Ctrl_C_T            0x1354
#define Key_Ctrl_C_U            0x1355
#define Key_Ctrl_C_V            0x1356
#define Key_Ctrl_C_W            0x1357
#define Key_Ctrl_C_X            0x1358
#define Key_Ctrl_C_Y            0x1359
#define Key_Ctrl_C_Z            0x135A

#define Key_Ctrl_C_0            0x1330
#define Key_Ctrl_C_1            0x1331
#define Key_Ctrl_C_2            0x1332
#define Key_Ctrl_C_3            0x1333
#define Key_Ctrl_C_4            0x1334
#define Key_Ctrl_C_5            0x1335
#define Key_Ctrl_C_6            0x1336
#define Key_Ctrl_C_7            0x1337
#define Key_Ctrl_C_8            0x1338
#define Key_Ctrl_C_9            0x1339

#define Key_Ctrl_D_A            0x1441
#define Key_Ctrl_D_B            0x1442
#define Key_Ctrl_D_C            0x1443
#define Key_Ctrl_D_D            0x1444
#define Key_Ctrl_D_E            0x1445
#define Key_Ctrl_D_F            0x1446
#define Key_Ctrl_D_G            0x1447
#define Key_Ctrl_D_H            0x1448
#define Key_Ctrl_D_I            0x1449
#define Key_Ctrl_D_J            0x144A
#define Key_Ctrl_D_K            0x144B
#define Key_Ctrl_D_L            0x144C
#define Key_Ctrl_D_M            0x144D
#define Key_Ctrl_D_N            0x144E
#define Key_Ctrl_D_O            0x144F
#define Key_Ctrl_D_P            0x1450
#define Key_Ctrl_D_Q            0x1451
#define Key_Ctrl_D_R            0x1452
#define Key_Ctrl_D_S            0x1453
#define Key_Ctrl_D_T            0x1454
#define Key_Ctrl_D_U            0x1455
#define Key_Ctrl_D_V            0x1456
#define Key_Ctrl_D_W            0x1457
#define Key_Ctrl_D_X            0x1458
#define Key_Ctrl_D_Y            0x1459
#define Key_Ctrl_D_Z            0x145A

#define Key_Ctrl_D_0            0x1430
#define Key_Ctrl_D_1            0x1431
#define Key_Ctrl_D_2            0x1432
#define Key_Ctrl_D_3            0x1433
#define Key_Ctrl_D_4            0x1434
#define Key_Ctrl_D_5            0x1435
#define Key_Ctrl_D_6            0x1436
#define Key_Ctrl_D_7            0x1437
#define Key_Ctrl_D_8            0x1438
#define Key_Ctrl_D_9            0x1439

#define Key_Ctrl_E_A            0x1541
#define Key_Ctrl_E_B            0x1542
#define Key_Ctrl_E_C            0x1543
#define Key_Ctrl_E_D            0x1544
#define Key_Ctrl_E_E            0x1545
#define Key_Ctrl_E_F            0x1546
#define Key_Ctrl_E_G            0x1547
#define Key_Ctrl_E_H            0x1548
#define Key_Ctrl_E_I            0x1549
#define Key_Ctrl_E_J            0x154A
#define Key_Ctrl_E_K            0x154B
#define Key_Ctrl_E_L            0x154C
#define Key_Ctrl_E_M            0x154D
#define Key_Ctrl_E_N            0x154E
#define Key_Ctrl_E_O            0x154F
#define Key_Ctrl_E_P            0x1550
#define Key_Ctrl_E_Q            0x1551
#define Key_Ctrl_E_R            0x1552
#define Key_Ctrl_E_S            0x1553
#define Key_Ctrl_E_T            0x1554
#define Key_Ctrl_E_U            0x1555
#define Key_Ctrl_E_V            0x1556
#define Key_Ctrl_E_W            0x1557
#define Key_Ctrl_E_X            0x1558
#define Key_Ctrl_E_Y            0x1559
#define Key_Ctrl_E_Z            0x155A

#define Key_Ctrl_E_0            0x1530
#define Key_Ctrl_E_1            0x1531
#define Key_Ctrl_E_2            0x1532
#define Key_Ctrl_E_3            0x1533
#define Key_Ctrl_E_4            0x1534
#define Key_Ctrl_E_5            0x1535
#define Key_Ctrl_E_6            0x1536
#define Key_Ctrl_E_7            0x1537
#define Key_Ctrl_E_8            0x1538
#define Key_Ctrl_E_9            0x1539

#define Key_Ctrl_F_A            0x1641
#define Key_Ctrl_F_B            0x1642
#define Key_Ctrl_F_C            0x1643
#define Key_Ctrl_F_D            0x1644
#define Key_Ctrl_F_E            0x1645
#define Key_Ctrl_F_F            0x1646
#define Key_Ctrl_F_G            0x1647
#define Key_Ctrl_F_H            0x1648
#define Key_Ctrl_F_I            0x1649
#define Key_Ctrl_F_J            0x164A
#define Key_Ctrl_F_K            0x164B
#define Key_Ctrl_F_L            0x164C
#define Key_Ctrl_F_M            0x164D
#define Key_Ctrl_F_N            0x164E
#define Key_Ctrl_F_O            0x164F
#define Key_Ctrl_F_P            0x1650
#define Key_Ctrl_F_Q            0x1651
#define Key_Ctrl_F_R            0x1652
#define Key_Ctrl_F_S            0x1653
#define Key_Ctrl_F_T            0x1654
#define Key_Ctrl_F_U            0x1655
#define Key_Ctrl_F_V            0x1656
#define Key_Ctrl_F_W            0x1657
#define Key_Ctrl_F_X            0x1658
#define Key_Ctrl_F_Y            0x1659
#define Key_Ctrl_F_Z            0x165A

#define Key_Ctrl_F_0            0x1630
#define Key_Ctrl_F_1            0x1631
#define Key_Ctrl_F_2            0x1632
#define Key_Ctrl_F_3            0x1633
#define Key_Ctrl_F_4            0x1634
#define Key_Ctrl_F_5            0x1635
#define Key_Ctrl_F_6            0x1636
#define Key_Ctrl_F_7            0x1637
#define Key_Ctrl_F_8            0x1638
#define Key_Ctrl_F_9            0x1639

#define Key_Ctrl_G_A            0x1741
#define Key_Ctrl_G_B            0x1742
#define Key_Ctrl_G_C            0x1743
#define Key_Ctrl_G_D            0x1744
#define Key_Ctrl_G_E            0x1745
#define Key_Ctrl_G_F            0x1746
#define Key_Ctrl_G_G            0x1747
#define Key_Ctrl_G_H            0x1748
#define Key_Ctrl_G_I            0x1749
#define Key_Ctrl_G_J            0x174A
#define Key_Ctrl_G_K            0x174B
#define Key_Ctrl_G_L            0x174C
#define Key_Ctrl_G_M            0x174D
#define Key_Ctrl_G_N            0x174E
#define Key_Ctrl_G_O            0x174F
#define Key_Ctrl_G_P            0x1750
#define Key_Ctrl_G_Q            0x1751
#define Key_Ctrl_G_R            0x1752
#define Key_Ctrl_G_S            0x1753
#define Key_Ctrl_G_T            0x1754
#define Key_Ctrl_G_U            0x1755
#define Key_Ctrl_G_V            0x1756
#define Key_Ctrl_G_W            0x1757
#define Key_Ctrl_G_X            0x1758
#define Key_Ctrl_G_Y            0x1759
#define Key_Ctrl_G_Z            0x175A

#define Key_Ctrl_G_0            0x1730
#define Key_Ctrl_G_1            0x1731
#define Key_Ctrl_G_2            0x1732
#define Key_Ctrl_G_3            0x1733
#define Key_Ctrl_G_4            0x1734
#define Key_Ctrl_G_5            0x1735
#define Key_Ctrl_G_6            0x1736
#define Key_Ctrl_G_7            0x1737
#define Key_Ctrl_G_8            0x1738
#define Key_Ctrl_G_9            0x1739

#define Key_Ctrl_H_A            0x1841
#define Key_Ctrl_H_B            0x1842
#define Key_Ctrl_H_C            0x1843
#define Key_Ctrl_H_D            0x1844
#define Key_Ctrl_H_E            0x1845
#define Key_Ctrl_H_F            0x1846
#define Key_Ctrl_H_G            0x1847
#define Key_Ctrl_H_H            0x1848
#define Key_Ctrl_H_I            0x1849
#define Key_Ctrl_H_J            0x184A
#define Key_Ctrl_H_K            0x184B
#define Key_Ctrl_H_L            0x184C
#define Key_Ctrl_H_M            0x184D
#define Key_Ctrl_H_N            0x184E
#define Key_Ctrl_H_O            0x184F
#define Key_Ctrl_H_P            0x1850
#define Key_Ctrl_H_Q            0x1851
#define Key_Ctrl_H_R            0x1852
#define Key_Ctrl_H_S            0x1853
#define Key_Ctrl_H_T            0x1854
#define Key_Ctrl_H_U            0x1855
#define Key_Ctrl_H_V            0x1856
#define Key_Ctrl_H_W            0x1857
#define Key_Ctrl_H_X            0x1858
#define Key_Ctrl_H_Y            0x1859
#define Key_Ctrl_H_Z            0x185A

#define Key_Ctrl_H_0            0x1830
#define Key_Ctrl_H_1            0x1831
#define Key_Ctrl_H_2            0x1832
#define Key_Ctrl_H_3            0x1833
#define Key_Ctrl_H_4            0x1834
#define Key_Ctrl_H_5            0x1835
#define Key_Ctrl_H_6            0x1836
#define Key_Ctrl_H_7            0x1837
#define Key_Ctrl_H_8            0x1838
#define Key_Ctrl_H_9            0x1839

#define Key_Ctrl_I_A            0x1941
#define Key_Ctrl_I_B            0x1942
#define Key_Ctrl_I_C            0x1943
#define Key_Ctrl_I_D            0x1944
#define Key_Ctrl_I_E            0x1945
#define Key_Ctrl_I_F            0x1946
#define Key_Ctrl_I_G            0x1947
#define Key_Ctrl_I_H            0x1948
#define Key_Ctrl_I_I            0x1949
#define Key_Ctrl_I_J            0x194A
#define Key_Ctrl_I_K            0x194B
#define Key_Ctrl_I_L            0x194C
#define Key_Ctrl_I_M            0x194D
#define Key_Ctrl_I_N            0x194E
#define Key_Ctrl_I_O            0x194F
#define Key_Ctrl_I_P            0x1950
#define Key_Ctrl_I_Q            0x1951
#define Key_Ctrl_I_R            0x1952
#define Key_Ctrl_I_S            0x1953
#define Key_Ctrl_I_T            0x1954
#define Key_Ctrl_I_U            0x1955
#define Key_Ctrl_I_V            0x1956
#define Key_Ctrl_I_W            0x1957
#define Key_Ctrl_I_X            0x1958
#define Key_Ctrl_I_Y            0x1959
#define Key_Ctrl_I_Z            0x195A

#define Key_Ctrl_I_0            0x1930
#define Key_Ctrl_I_1            0x1931
#define Key_Ctrl_I_2            0x1932
#define Key_Ctrl_I_3            0x1933
#define Key_Ctrl_I_4            0x1934
#define Key_Ctrl_I_5            0x1935
#define Key_Ctrl_I_6            0x1936
#define Key_Ctrl_I_7            0x1937
#define Key_Ctrl_I_8            0x1938
#define Key_Ctrl_I_9            0x1939

#define Key_Ctrl_J_A            0x1A41
#define Key_Ctrl_J_B            0x1A42
#define Key_Ctrl_J_C            0x1A43
#define Key_Ctrl_J_D            0x1A44
#define Key_Ctrl_J_E            0x1A45
#define Key_Ctrl_J_F            0x1A46
#define Key_Ctrl_J_G            0x1A47
#define Key_Ctrl_J_H            0x1A48
#define Key_Ctrl_J_I            0x1A49
#define Key_Ctrl_J_J            0x1A4A
#define Key_Ctrl_J_K            0x1A4B
#define Key_Ctrl_J_L            0x1A4C
#define Key_Ctrl_J_M            0x1A4D
#define Key_Ctrl_J_N            0x1A4E
#define Key_Ctrl_J_O            0x1A4F
#define Key_Ctrl_J_P            0x1A50
#define Key_Ctrl_J_Q            0x1A51
#define Key_Ctrl_J_R            0x1A52
#define Key_Ctrl_J_S            0x1A53
#define Key_Ctrl_J_T            0x1A54
#define Key_Ctrl_J_U            0x1A55
#define Key_Ctrl_J_V            0x1A56
#define Key_Ctrl_J_W            0x1A57
#define Key_Ctrl_J_X            0x1A58
#define Key_Ctrl_J_Y            0x1A59
#define Key_Ctrl_J_Z            0x1A5A

#define Key_Ctrl_J_0            0x1A30
#define Key_Ctrl_J_1            0x1A31
#define Key_Ctrl_J_2            0x1A32
#define Key_Ctrl_J_3            0x1A33
#define Key_Ctrl_J_4            0x1A34
#define Key_Ctrl_J_5            0x1A35
#define Key_Ctrl_J_6            0x1A36
#define Key_Ctrl_J_7            0x1A37
#define Key_Ctrl_J_8            0x1A38
#define Key_Ctrl_J_9            0x1A39

#define Key_Ctrl_K_A            0x1B41
#define Key_Ctrl_K_B            0x1B42
#define Key_Ctrl_K_C            0x1B43
#define Key_Ctrl_K_D            0x1B44
#define Key_Ctrl_K_E            0x1B45
#define Key_Ctrl_K_F            0x1B46
#define Key_Ctrl_K_G            0x1B47
#define Key_Ctrl_K_H            0x1B48
#define Key_Ctrl_K_I            0x1B49
#define Key_Ctrl_K_J            0x1B4A
#define Key_Ctrl_K_K            0x1B4B
#define Key_Ctrl_K_L            0x1B4C
#define Key_Ctrl_K_M            0x1B4D
#define Key_Ctrl_K_N            0x1B4E
#define Key_Ctrl_K_O            0x1B4F
#define Key_Ctrl_K_P            0x1B50
#define Key_Ctrl_K_Q            0x1B51
#define Key_Ctrl_K_R            0x1B52
#define Key_Ctrl_K_S            0x1B53
#define Key_Ctrl_K_T            0x1B54
#define Key_Ctrl_K_U            0x1B55
#define Key_Ctrl_K_V            0x1B56
#define Key_Ctrl_K_W            0x1B57
#define Key_Ctrl_K_X            0x1B58
#define Key_Ctrl_K_Y            0x1B59
#define Key_Ctrl_K_Z            0x1B5A

#define Key_Ctrl_K_0            0x1B30
#define Key_Ctrl_K_1            0x1B31
#define Key_Ctrl_K_2            0x1B32
#define Key_Ctrl_K_3            0x1B33
#define Key_Ctrl_K_4            0x1B34
#define Key_Ctrl_K_5            0x1B35
#define Key_Ctrl_K_6            0x1B36
#define Key_Ctrl_K_7            0x1B37
#define Key_Ctrl_K_8            0x1B38
#define Key_Ctrl_K_9            0x1B39

#define Key_Ctrl_L_A            0x1C41
#define Key_Ctrl_L_B            0x1C42
#define Key_Ctrl_L_C            0x1C43
#define Key_Ctrl_L_D            0x1C44
#define Key_Ctrl_L_E            0x1C45
#define Key_Ctrl_L_F            0x1C46
#define Key_Ctrl_L_G            0x1C47
#define Key_Ctrl_L_H            0x1C48
#define Key_Ctrl_L_I            0x1C49
#define Key_Ctrl_L_J            0x1C4A
#define Key_Ctrl_L_K            0x1C4B
#define Key_Ctrl_L_L            0x1C4C
#define Key_Ctrl_L_M            0x1C4D
#define Key_Ctrl_L_N            0x1C4E
#define Key_Ctrl_L_O            0x1C4F
#define Key_Ctrl_L_P            0x1C50
#define Key_Ctrl_L_Q            0x1C51
#define Key_Ctrl_L_R            0x1C52
#define Key_Ctrl_L_S            0x1C53
#define Key_Ctrl_L_T            0x1C54
#define Key_Ctrl_L_U            0x1C55
#define Key_Ctrl_L_V            0x1C56
#define Key_Ctrl_L_W            0x1C57
#define Key_Ctrl_L_X            0x1C58
#define Key_Ctrl_L_Y            0x1C59
#define Key_Ctrl_L_Z            0x1C5A

#define Key_Ctrl_L_0            0x1C30
#define Key_Ctrl_L_1            0x1C31
#define Key_Ctrl_L_2            0x1C32
#define Key_Ctrl_L_3            0x1C33
#define Key_Ctrl_L_4            0x1C34
#define Key_Ctrl_L_5            0x1C35
#define Key_Ctrl_L_6            0x1C36
#define Key_Ctrl_L_7            0x1C37
#define Key_Ctrl_L_8            0x1C38
#define Key_Ctrl_L_9            0x1C39

#define Key_Ctrl_M_A            0x1D41
#define Key_Ctrl_M_B            0x1D42
#define Key_Ctrl_M_C            0x1D43
#define Key_Ctrl_M_D            0x1D44
#define Key_Ctrl_M_E            0x1D45
#define Key_Ctrl_M_F            0x1D46
#define Key_Ctrl_M_G            0x1D47
#define Key_Ctrl_M_H            0x1D48
#define Key_Ctrl_M_I            0x1D49
#define Key_Ctrl_M_J            0x1D4A
#define Key_Ctrl_M_K            0x1D4B
#define Key_Ctrl_M_L            0x1D4C
#define Key_Ctrl_M_M            0x1D4D
#define Key_Ctrl_M_N            0x1D4E
#define Key_Ctrl_M_O            0x1D4F
#define Key_Ctrl_M_P            0x1D50
#define Key_Ctrl_M_Q            0x1D51
#define Key_Ctrl_M_R            0x1D52
#define Key_Ctrl_M_S            0x1D53
#define Key_Ctrl_M_T            0x1D54
#define Key_Ctrl_M_U            0x1D55
#define Key_Ctrl_M_V            0x1D56
#define Key_Ctrl_M_W            0x1D57
#define Key_Ctrl_M_X            0x1D58
#define Key_Ctrl_M_Y            0x1D59
#define Key_Ctrl_M_Z            0x1D5A

#define Key_Ctrl_M_0            0x1D30
#define Key_Ctrl_M_1            0x1D31
#define Key_Ctrl_M_2            0x1D32
#define Key_Ctrl_M_3            0x1D33
#define Key_Ctrl_M_4            0x1D34
#define Key_Ctrl_M_5            0x1D35
#define Key_Ctrl_M_6            0x1D36
#define Key_Ctrl_M_7            0x1D37
#define Key_Ctrl_M_8            0x1D38
#define Key_Ctrl_M_9            0x1D39

#define Key_Ctrl_N_A            0x1E41
#define Key_Ctrl_N_B            0x1E42
#define Key_Ctrl_N_C            0x1E43
#define Key_Ctrl_N_D            0x1E44
#define Key_Ctrl_N_E            0x1E45
#define Key_Ctrl_N_F            0x1E46
#define Key_Ctrl_N_G            0x1E47
#define Key_Ctrl_N_H            0x1E48
#define Key_Ctrl_N_I            0x1E49
#define Key_Ctrl_N_J            0x1E4A
#define Key_Ctrl_N_K            0x1E4B
#define Key_Ctrl_N_L            0x1E4C
#define Key_Ctrl_N_M            0x1E4D
#define Key_Ctrl_N_N            0x1E4E
#define Key_Ctrl_N_O            0x1E4F
#define Key_Ctrl_N_P            0x1E50
#define Key_Ctrl_N_Q            0x1E51
#define Key_Ctrl_N_R            0x1E52
#define Key_Ctrl_N_S            0x1E53
#define Key_Ctrl_N_T            0x1E54
#define Key_Ctrl_N_U            0x1E55
#define Key_Ctrl_N_V            0x1E56
#define Key_Ctrl_N_W            0x1E57
#define Key_Ctrl_N_X            0x1E58
#define Key_Ctrl_N_Y            0x1E59
#define Key_Ctrl_N_Z            0x1E5A

#define Key_Ctrl_N_0            0x1E30
#define Key_Ctrl_N_1            0x1E31
#define Key_Ctrl_N_2            0x1E32
#define Key_Ctrl_N_3            0x1E33
#define Key_Ctrl_N_4            0x1E34
#define Key_Ctrl_N_5            0x1E35
#define Key_Ctrl_N_6            0x1E36
#define Key_Ctrl_N_7            0x1E37
#define Key_Ctrl_N_8            0x1E38
#define Key_Ctrl_N_9            0x1E39

#define Key_Ctrl_O_A            0x1F41
#define Key_Ctrl_O_B            0x1F42
#define Key_Ctrl_O_C            0x1F43
#define Key_Ctrl_O_D            0x1F44
#define Key_Ctrl_O_E            0x1F45
#define Key_Ctrl_O_F            0x1F46
#define Key_Ctrl_O_G            0x1F47
#define Key_Ctrl_O_H            0x1F48
#define Key_Ctrl_O_I            0x1F49
#define Key_Ctrl_O_J            0x1F4A
#define Key_Ctrl_O_K            0x1F4B
#define Key_Ctrl_O_L            0x1F4C
#define Key_Ctrl_O_M            0x1F4D
#define Key_Ctrl_O_N            0x1F4E
#define Key_Ctrl_O_O            0x1F4F
#define Key_Ctrl_O_P            0x1F50
#define Key_Ctrl_O_Q            0x1F51
#define Key_Ctrl_O_R            0x1F52
#define Key_Ctrl_O_S            0x1F53
#define Key_Ctrl_O_T            0x1F54
#define Key_Ctrl_O_U            0x1F55
#define Key_Ctrl_O_V            0x1F56
#define Key_Ctrl_O_W            0x1F57
#define Key_Ctrl_O_X            0x1F58
#define Key_Ctrl_O_Y            0x1F59
#define Key_Ctrl_O_Z            0x1F5A

#define Key_Ctrl_O_0            0x1F30
#define Key_Ctrl_O_1            0x1F31
#define Key_Ctrl_O_2            0x1F32
#define Key_Ctrl_O_3            0x1F33
#define Key_Ctrl_O_4            0x1F34
#define Key_Ctrl_O_5            0x1F35
#define Key_Ctrl_O_6            0x1F36
#define Key_Ctrl_O_7            0x1F37
#define Key_Ctrl_O_8            0x1F38
#define Key_Ctrl_O_9            0x1F39

#define Key_Ctrl_P_A            0x2041
#define Key_Ctrl_P_B            0x2042
#define Key_Ctrl_P_C            0x2043
#define Key_Ctrl_P_D            0x2044
#define Key_Ctrl_P_E            0x2045
#define Key_Ctrl_P_F            0x2046
#define Key_Ctrl_P_G            0x2047
#define Key_Ctrl_P_H            0x2048
#define Key_Ctrl_P_I            0x2049
#define Key_Ctrl_P_J            0x204A
#define Key_Ctrl_P_K            0x204B
#define Key_Ctrl_P_L            0x204C
#define Key_Ctrl_P_M            0x204D
#define Key_Ctrl_P_N            0x204E
#define Key_Ctrl_P_O            0x204F
#define Key_Ctrl_P_P            0x2050
#define Key_Ctrl_P_Q            0x2051
#define Key_Ctrl_P_R            0x2052
#define Key_Ctrl_P_S            0x2053
#define Key_Ctrl_P_T            0x2054
#define Key_Ctrl_P_U            0x2055
#define Key_Ctrl_P_V            0x2056
#define Key_Ctrl_P_W            0x2057
#define Key_Ctrl_P_X            0x2058
#define Key_Ctrl_P_Y            0x2059
#define Key_Ctrl_P_Z            0x205A

#define Key_Ctrl_P_0            0x2030
#define Key_Ctrl_P_1            0x2031
#define Key_Ctrl_P_2            0x2032
#define Key_Ctrl_P_3            0x2033
#define Key_Ctrl_P_4            0x2034
#define Key_Ctrl_P_5            0x2035
#define Key_Ctrl_P_6            0x2036
#define Key_Ctrl_P_7            0x2037
#define Key_Ctrl_P_8            0x2038
#define Key_Ctrl_P_9            0x2039

#define Key_Ctrl_Q_A            0x2141
#define Key_Ctrl_Q_B            0x2142
#define Key_Ctrl_Q_C            0x2143
#define Key_Ctrl_Q_D            0x2144
#define Key_Ctrl_Q_E            0x2145
#define Key_Ctrl_Q_F            0x2146
#define Key_Ctrl_Q_G            0x2147
#define Key_Ctrl_Q_H            0x2148
#define Key_Ctrl_Q_I            0x2149
#define Key_Ctrl_Q_J            0x214A
#define Key_Ctrl_Q_K            0x214B
#define Key_Ctrl_Q_L            0x214C
#define Key_Ctrl_Q_M            0x214D
#define Key_Ctrl_Q_N            0x214E
#define Key_Ctrl_Q_O            0x214F
#define Key_Ctrl_Q_P            0x2150
#define Key_Ctrl_Q_Q            0x2151
#define Key_Ctrl_Q_R            0x2152
#define Key_Ctrl_Q_S            0x2153
#define Key_Ctrl_Q_T            0x2154
#define Key_Ctrl_Q_U            0x2155
#define Key_Ctrl_Q_V            0x2156
#define Key_Ctrl_Q_W            0x2157
#define Key_Ctrl_Q_X            0x2158
#define Key_Ctrl_Q_Y            0x2159
#define Key_Ctrl_Q_Z            0x215A

#define Key_Ctrl_Q_0            0x2130
#define Key_Ctrl_Q_1            0x2131
#define Key_Ctrl_Q_2            0x2132
#define Key_Ctrl_Q_3            0x2133
#define Key_Ctrl_Q_4            0x2134
#define Key_Ctrl_Q_5            0x2135
#define Key_Ctrl_Q_6            0x2136
#define Key_Ctrl_Q_7            0x2137
#define Key_Ctrl_Q_8            0x2138
#define Key_Ctrl_Q_9            0x2139

#define Key_Ctrl_R_A            0x2241
#define Key_Ctrl_R_B            0x2242
#define Key_Ctrl_R_C            0x2243
#define Key_Ctrl_R_D            0x2244
#define Key_Ctrl_R_E            0x2245
#define Key_Ctrl_R_F            0x2246
#define Key_Ctrl_R_G            0x2247
#define Key_Ctrl_R_H            0x2248
#define Key_Ctrl_R_I            0x2249
#define Key_Ctrl_R_J            0x224A
#define Key_Ctrl_R_K            0x224B
#define Key_Ctrl_R_L            0x224C
#define Key_Ctrl_R_M            0x224D
#define Key_Ctrl_R_N            0x224E
#define Key_Ctrl_R_O            0x224F
#define Key_Ctrl_R_P            0x2250
#define Key_Ctrl_R_Q            0x2251
#define Key_Ctrl_R_R            0x2252
#define Key_Ctrl_R_S            0x2253
#define Key_Ctrl_R_T            0x2254
#define Key_Ctrl_R_U            0x2255
#define Key_Ctrl_R_V            0x2256
#define Key_Ctrl_R_W            0x2257
#define Key_Ctrl_R_X            0x2258
#define Key_Ctrl_R_Y            0x2259
#define Key_Ctrl_R_Z            0x225A

#define Key_Ctrl_R_0            0x2230
#define Key_Ctrl_R_1            0x2231
#define Key_Ctrl_R_2            0x2232
#define Key_Ctrl_R_3            0x2233
#define Key_Ctrl_R_4            0x2234
#define Key_Ctrl_R_5            0x2235
#define Key_Ctrl_R_6            0x2236
#define Key_Ctrl_R_7            0x2237
#define Key_Ctrl_R_8            0x2238
#define Key_Ctrl_R_9            0x2239

#define Key_Ctrl_S_A            0x2341
#define Key_Ctrl_S_B            0x2342
#define Key_Ctrl_S_C            0x2343
#define Key_Ctrl_S_D            0x2344
#define Key_Ctrl_S_E            0x2345
#define Key_Ctrl_S_F            0x2346
#define Key_Ctrl_S_G            0x2347
#define Key_Ctrl_S_H            0x2348
#define Key_Ctrl_S_I            0x2349
#define Key_Ctrl_S_J            0x234A
#define Key_Ctrl_S_K            0x234B
#define Key_Ctrl_S_L            0x234C
#define Key_Ctrl_S_M            0x234D
#define Key_Ctrl_S_N            0x234E
#define Key_Ctrl_S_O            0x234F
#define Key_Ctrl_S_P            0x2350
#define Key_Ctrl_S_Q            0x2351
#define Key_Ctrl_S_R            0x2352
#define Key_Ctrl_S_S            0x2353
#define Key_Ctrl_S_T            0x2354
#define Key_Ctrl_S_U            0x2355
#define Key_Ctrl_S_V            0x2356
#define Key_Ctrl_S_W            0x2357
#define Key_Ctrl_S_X            0x2358
#define Key_Ctrl_S_Y            0x2359
#define Key_Ctrl_S_Z            0x235A

#define Key_Ctrl_S_0            0x2330
#define Key_Ctrl_S_1            0x2331
#define Key_Ctrl_S_2            0x2332
#define Key_Ctrl_S_3            0x2333
#define Key_Ctrl_S_4            0x2334
#define Key_Ctrl_S_5            0x2335
#define Key_Ctrl_S_6            0x2336
#define Key_Ctrl_S_7            0x2337
#define Key_Ctrl_S_8            0x2338
#define Key_Ctrl_S_9            0x2339

#define Key_Ctrl_T_A            0x2441
#define Key_Ctrl_T_B            0x2442
#define Key_Ctrl_T_C            0x2443
#define Key_Ctrl_T_D            0x2444
#define Key_Ctrl_T_E            0x2445
#define Key_Ctrl_T_F            0x2446
#define Key_Ctrl_T_G            0x2447
#define Key_Ctrl_T_H            0x2448
#define Key_Ctrl_T_I            0x2449
#define Key_Ctrl_T_J            0x244A
#define Key_Ctrl_T_K            0x244B
#define Key_Ctrl_T_L            0x244C
#define Key_Ctrl_T_M            0x244D
#define Key_Ctrl_T_N            0x244E
#define Key_Ctrl_T_O            0x244F
#define Key_Ctrl_T_P            0x2450
#define Key_Ctrl_T_Q            0x2451
#define Key_Ctrl_T_R            0x2452
#define Key_Ctrl_T_S            0x2453
#define Key_Ctrl_T_T            0x2454
#define Key_Ctrl_T_U            0x2455
#define Key_Ctrl_T_V            0x2456
#define Key_Ctrl_T_W            0x2457
#define Key_Ctrl_T_X            0x2458
#define Key_Ctrl_T_Y            0x2459
#define Key_Ctrl_T_Z            0x245A

#define Key_Ctrl_T_0            0x2430
#define Key_Ctrl_T_1            0x2431
#define Key_Ctrl_T_2            0x2432
#define Key_Ctrl_T_3            0x2433
#define Key_Ctrl_T_4            0x2434
#define Key_Ctrl_T_5            0x2435
#define Key_Ctrl_T_6            0x2436
#define Key_Ctrl_T_7            0x2437
#define Key_Ctrl_T_8            0x2438
#define Key_Ctrl_T_9            0x2439

#define Key_Ctrl_U_A            0x2541
#define Key_Ctrl_U_B            0x2542
#define Key_Ctrl_U_C            0x2543
#define Key_Ctrl_U_D            0x2544
#define Key_Ctrl_U_E            0x2545
#define Key_Ctrl_U_F            0x2546
#define Key_Ctrl_U_G            0x2547
#define Key_Ctrl_U_H            0x2548
#define Key_Ctrl_U_I            0x2549
#define Key_Ctrl_U_J            0x254A
#define Key_Ctrl_U_K            0x254B
#define Key_Ctrl_U_L            0x254C
#define Key_Ctrl_U_M            0x254D
#define Key_Ctrl_U_N            0x254E
#define Key_Ctrl_U_O            0x254F
#define Key_Ctrl_U_P            0x2550
#define Key_Ctrl_U_Q            0x2551
#define Key_Ctrl_U_R            0x2552
#define Key_Ctrl_U_S            0x2553
#define Key_Ctrl_U_T            0x2554
#define Key_Ctrl_U_U            0x2555
#define Key_Ctrl_U_V            0x2556
#define Key_Ctrl_U_W            0x2557
#define Key_Ctrl_U_X            0x2558
#define Key_Ctrl_U_Y            0x2559
#define Key_Ctrl_U_Z            0x255A

#define Key_Ctrl_U_0            0x2530
#define Key_Ctrl_U_1            0x2531
#define Key_Ctrl_U_2            0x2532
#define Key_Ctrl_U_3            0x2533
#define Key_Ctrl_U_4            0x2534
#define Key_Ctrl_U_5            0x2535
#define Key_Ctrl_U_6            0x2536
#define Key_Ctrl_U_7            0x2537
#define Key_Ctrl_U_8            0x2538
#define Key_Ctrl_U_9            0x2539

#define Key_Ctrl_V_A            0x2641
#define Key_Ctrl_V_B            0x2642
#define Key_Ctrl_V_C            0x2643
#define Key_Ctrl_V_D            0x2644
#define Key_Ctrl_V_E            0x2645
#define Key_Ctrl_V_F            0x2646
#define Key_Ctrl_V_G            0x2647
#define Key_Ctrl_V_H            0x2648
#define Key_Ctrl_V_I            0x2649
#define Key_Ctrl_V_J            0x264A
#define Key_Ctrl_V_K            0x264B
#define Key_Ctrl_V_L            0x264C
#define Key_Ctrl_V_M            0x264D
#define Key_Ctrl_V_N            0x264E
#define Key_Ctrl_V_O            0x264F
#define Key_Ctrl_V_P            0x2650
#define Key_Ctrl_V_Q            0x2651
#define Key_Ctrl_V_R            0x2652
#define Key_Ctrl_V_S            0x2653
#define Key_Ctrl_V_T            0x2654
#define Key_Ctrl_V_U            0x2655
#define Key_Ctrl_V_V            0x2656
#define Key_Ctrl_V_W            0x2657
#define Key_Ctrl_V_X            0x2658
#define Key_Ctrl_V_Y            0x2659
#define Key_Ctrl_V_Z            0x265A

#define Key_Ctrl_V_0            0x2630
#define Key_Ctrl_V_1            0x2631
#define Key_Ctrl_V_2            0x2632
#define Key_Ctrl_V_3            0x2633
#define Key_Ctrl_V_4            0x2634
#define Key_Ctrl_V_5            0x2635
#define Key_Ctrl_V_6            0x2636
#define Key_Ctrl_V_7            0x2637
#define Key_Ctrl_V_8            0x2638
#define Key_Ctrl_V_9            0x2639

#define Key_Ctrl_W_A            0x2741
#define Key_Ctrl_W_B            0x2742
#define Key_Ctrl_W_C            0x2743
#define Key_Ctrl_W_D            0x2744
#define Key_Ctrl_W_E            0x2745
#define Key_Ctrl_W_F            0x2746
#define Key_Ctrl_W_G            0x2747
#define Key_Ctrl_W_H            0x2748
#define Key_Ctrl_W_I            0x2749
#define Key_Ctrl_W_J            0x274A
#define Key_Ctrl_W_K            0x274B
#define Key_Ctrl_W_L            0x274C
#define Key_Ctrl_W_M            0x274D
#define Key_Ctrl_W_N            0x274E
#define Key_Ctrl_W_O            0x274F
#define Key_Ctrl_W_P            0x2750
#define Key_Ctrl_W_Q            0x2751
#define Key_Ctrl_W_R            0x2752
#define Key_Ctrl_W_S            0x2753
#define Key_Ctrl_W_T            0x2754
#define Key_Ctrl_W_U            0x2755
#define Key_Ctrl_W_V            0x2756
#define Key_Ctrl_W_W            0x2757
#define Key_Ctrl_W_X            0x2758
#define Key_Ctrl_W_Y            0x2759
#define Key_Ctrl_W_Z            0x275A

#define Key_Ctrl_W_0            0x2730
#define Key_Ctrl_W_1            0x2731
#define Key_Ctrl_W_2            0x2732
#define Key_Ctrl_W_3            0x2733
#define Key_Ctrl_W_4            0x2734
#define Key_Ctrl_W_5            0x2735
#define Key_Ctrl_W_6            0x2736
#define Key_Ctrl_W_7            0x2737
#define Key_Ctrl_W_8            0x2738
#define Key_Ctrl_W_9            0x2739

#define Key_Ctrl_X_A            0x2841
#define Key_Ctrl_X_B            0x2842
#define Key_Ctrl_X_C            0x2843
#define Key_Ctrl_X_D            0x2844
#define Key_Ctrl_X_E            0x2845
#define Key_Ctrl_X_F            0x2846
#define Key_Ctrl_X_G            0x2847
#define Key_Ctrl_X_H            0x2848
#define Key_Ctrl_X_I            0x2849
#define Key_Ctrl_X_J            0x284A
#define Key_Ctrl_X_K            0x284B
#define Key_Ctrl_X_L            0x284C
#define Key_Ctrl_X_M            0x284D
#define Key_Ctrl_X_N            0x284E
#define Key_Ctrl_X_O            0x284F
#define Key_Ctrl_X_P            0x2850
#define Key_Ctrl_X_Q            0x2851
#define Key_Ctrl_X_R            0x2852
#define Key_Ctrl_X_S            0x2853
#define Key_Ctrl_X_T            0x2854
#define Key_Ctrl_X_U            0x2855
#define Key_Ctrl_X_V            0x2856
#define Key_Ctrl_X_W            0x2857
#define Key_Ctrl_X_X            0x2858
#define Key_Ctrl_X_Y            0x2859
#define Key_Ctrl_X_Z            0x285A

#define Key_Ctrl_X_0            0x2830
#define Key_Ctrl_X_1            0x2831
#define Key_Ctrl_X_2            0x2832
#define Key_Ctrl_X_3            0x2833
#define Key_Ctrl_X_4            0x2834
#define Key_Ctrl_X_5            0x2835
#define Key_Ctrl_X_6            0x2836
#define Key_Ctrl_X_7            0x2837
#define Key_Ctrl_X_8            0x2838
#define Key_Ctrl_X_9            0x2839

#define Key_Ctrl_Y_A            0x2941
#define Key_Ctrl_Y_B            0x2942
#define Key_Ctrl_Y_C            0x2943
#define Key_Ctrl_Y_D            0x2944
#define Key_Ctrl_Y_E            0x2945
#define Key_Ctrl_Y_F            0x2946
#define Key_Ctrl_Y_G            0x2947
#define Key_Ctrl_Y_H            0x2948
#define Key_Ctrl_Y_I            0x2949
#define Key_Ctrl_Y_J            0x294A
#define Key_Ctrl_Y_K            0x294B
#define Key_Ctrl_Y_L            0x294C
#define Key_Ctrl_Y_M            0x294D
#define Key_Ctrl_Y_N            0x294E
#define Key_Ctrl_Y_O            0x294F
#define Key_Ctrl_Y_P            0x2950
#define Key_Ctrl_Y_Q            0x2951
#define Key_Ctrl_Y_R            0x2952
#define Key_Ctrl_Y_S            0x2953
#define Key_Ctrl_Y_T            0x2954
#define Key_Ctrl_Y_U            0x2955
#define Key_Ctrl_Y_V            0x2956
#define Key_Ctrl_Y_W            0x2957
#define Key_Ctrl_Y_X            0x2958
#define Key_Ctrl_Y_Y            0x2959
#define Key_Ctrl_Y_Z            0x295A

#define Key_Ctrl_Y_0            0x2930
#define Key_Ctrl_Y_1            0x2931
#define Key_Ctrl_Y_2            0x2932
#define Key_Ctrl_Y_3            0x2933
#define Key_Ctrl_Y_4            0x2934
#define Key_Ctrl_Y_5            0x2935
#define Key_Ctrl_Y_6            0x2936
#define Key_Ctrl_Y_7            0x2937
#define Key_Ctrl_Y_8            0x2938
#define Key_Ctrl_Y_9            0x2939

#define Key_Ctrl_Z_A            0x2A41
#define Key_Ctrl_Z_B            0x2A42
#define Key_Ctrl_Z_C            0x2A43
#define Key_Ctrl_Z_D            0x2A44
#define Key_Ctrl_Z_E            0x2A45
#define Key_Ctrl_Z_F            0x2A46
#define Key_Ctrl_Z_G            0x2A47
#define Key_Ctrl_Z_H            0x2A48
#define Key_Ctrl_Z_I            0x2A49
#define Key_Ctrl_Z_J            0x2A4A
#define Key_Ctrl_Z_K            0x2A4B
#define Key_Ctrl_Z_L            0x2A4C
#define Key_Ctrl_Z_M            0x2A4D
#define Key_Ctrl_Z_N            0x2A4E
#define Key_Ctrl_Z_O            0x2A4F
#define Key_Ctrl_Z_P            0x2A50
#define Key_Ctrl_Z_Q            0x2A51
#define Key_Ctrl_Z_R            0x2A52
#define Key_Ctrl_Z_S            0x2A53
#define Key_Ctrl_Z_T            0x2A54
#define Key_Ctrl_Z_U            0x2A55
#define Key_Ctrl_Z_V            0x2A56
#define Key_Ctrl_Z_W            0x2A57
#define Key_Ctrl_Z_X            0x2A58
#define Key_Ctrl_Z_Y            0x2A59
#define Key_Ctrl_Z_Z            0x2A5A

#define Key_Ctrl_Z_0            0x2A30
#define Key_Ctrl_Z_1            0x2A31
#define Key_Ctrl_Z_2            0x2A32
#define Key_Ctrl_Z_3            0x2A33
#define Key_Ctrl_Z_4            0x2A34
#define Key_Ctrl_Z_5            0x2A35
#define Key_Ctrl_Z_6            0x2A36
#define Key_Ctrl_Z_7            0x2A37
#define Key_Ctrl_Z_8            0x2A38
#define Key_Ctrl_Z_9            0x2A39

// *****************************************************************************
// *****************************************************************************


#endif
