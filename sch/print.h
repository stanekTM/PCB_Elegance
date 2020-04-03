/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: print.h
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


#ifndef _PRINT

#define _PRINT

#include "types.h"

void Print(int32 PaperSize, int32 Color, LPSTR CurrentPrinterName, int32 mode);

void ExportToBitmap(int32 mode);

void ExportToBitmap2(int32 mode, int32 ExportPixelsX, int32 ExportPixelsY);

int32 ExportToPDF(int32 PaperSize, int32 Orientation, int32 FitToPage, int32 PdfObjectStart, int32 mode);

#endif
