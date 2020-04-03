/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: pcb.c
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



#include "pcb.h"
#include "stdio.h"
#include "memory.h"
#include "design.h"
#include "files2.h"
#include "utf8.h"
#include "math.h"
/*
#define    InRange(x1,x2) ( (((x1>x2-0.01) && (x1<x2+0.01))) ? (1) : (0) )
#define    InRange9(x1,x2)  ( ((((x1)>(x2)-20000.0)  && ((x1)<(x2)+20000.0)))  ? (1)  : (0) )
*/

#define    InRange4(x1,x2)  ( ((((x1)>(x2)-1000.0)   && ((x1)<(x2)+1000.0)))   ? (1)  : (0) )
#define    InRange11(x1,x2) ( (((x1>x2-0.001) && (x1<x2+0.001))) ? (1) : (0) )
#define CalcLengthLine(x1,y1,x2,y2) (sqrt(SQR((x2)-(x1))+SQR((y2)-(y1))))

extern HGLOBAL ObjectsGlobal, Objects6Global;
extern PCBDesignRecord PcbDesign;

int32 ShapesMemSize, ok;
ShapesArray *Shapes;
uint8 *ShapesMem;

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void GetRotationAndMirrorFromShapeText(double ShapeRotation, double *Rotation, int32 * Mirror)
{
	int32 rot;

	if (ShapeRotation > 10.0)
	{
		// Floating point number
		ShapeRotation -= 2000.0;

		if (ShapeRotation > 1000.0)
		{
			// Mirrored text
			*Rotation = ShapeRotation - 2000.0;
			*Mirror = 1;
		}
		else
		{
			*Rotation = ShapeRotation;
			*Mirror = 0;
		}
	}
	else
	{
		rot = (int32) (ShapeRotation + 0.1);
		rot = ((rot & 3) << 1) + ((rot & 4) >> 2);
		*Rotation = rot * 45;
		*Mirror = 0;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 GetRotationFromFloat(double Rotation)
{
	double rot, diff;
	int32 NewRotation;

	if (Rotation < 0)
		Rotation += 360;

	diff = modf((Rotation + 0.001) / 45.0, &rot);

	if (diff < 0.00003)
	{
		NewRotation = (int32) rot;

		if ((NewRotation & 1) == 0)
		{	// 90 degrees increments
			return (NewRotation >> 1);
		}
		else
			return ((NewRotation >> 1) + 4);
	}

	return -1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 CheckGeometryLayer(int32 * Layer, int32 NrGeomLayers, int32 Mirror)
{
	if ((*Layer >= 32) || (*Layer < -1))
	{
		*Layer = 0;
		return 0;
	}

	if (*Layer == -1)
		return 1;

// *******************************************************************************************************
	if (NrGeomLayers <= 2)
	{
		if (Mirror == 0)
		{
			if (NrGeomLayers == 1)
			{
				if (*Layer >= 1)
					return 0;

				*Layer = 0;
			}
			else
			{
				if (*Layer >= 1)
					*Layer = NrGeomLayers - 1;
				else
					*Layer = 0;
			}
		}
		else
		{
			if (NrGeomLayers == 1)
			{
				if (*Layer == 0)
					return 0;

				*Layer = 0;
			}
			else
			{
				if (*Layer == 0)
					*Layer = NrGeomLayers - 1;
				else
					*Layer = 0;
			}
		}

		return 1;
	}

// *******************************************************************************************************
// Nr geometry layers > 2
// Nr board layers > 2

	if (Mirror == 1)
		*Layer = NrGeomLayers - 1 - *Layer;

	if (*Layer >= NrGeomLayers)
		*Layer = NrGeomLayers - 1;

	return 1;
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void RotatePoint2(double *x, double *y, double Rotation)
{
	double hx, hy, dx, dy, r1, length, hoek;

	if (InRange11(Rotation, 0.0))
		return;
	else if (InRange11(Rotation, ANGLE_90))
	{
		hx = -*y;
		hy = *x;
		*x = (float) hx;
		*y = (float) hy;
		return;
	}
	else if (InRange11(Rotation, ANGLE_180))
	{
		*x = -*x;
		*y = -*y;
		return;
	}
	else if (InRange11(Rotation, ANGLE_270))
	{
		hx = *y;
		hy = -*x;
		*x = (float) hx;
		*y = (float) hy;
		return;
	}
	else
	{
		dx = *x;
		dy = *y;

		if (InRange11(dx, 0.0))
		{
			if (dy > 0.0)
				hoek = ANGLE_90;
			else
				hoek = ANGLE_270;
		}
		else
		{
			r1 = dy / dx;
			hoek = atan(r1);

			if (r1 > 0.0)
			{
				if (dx < 0.0)
					hoek += ANGLE_180;
			}
			else
			{
				if (dx > 0.0)
					hoek += ANGLE_360;
				else
					hoek += ANGLE_180;
			}
		}

		hoek += ANGLE_CONVERT(Rotation);
		length = sqrt(SQR(dx) + SQR(dy));

		if (InRange11(hoek, 0.0))
		{
			*x = (float) length;
			*y = 0.0;
		}
		else if (InRange11(hoek, ANGLE_90))
		{
			*x = 0.0;
			*y = (float) length;
		}
		else if (InRange11(hoek, ANGLE_180))
		{
			*x = (float) -length;
			*y = 0.0;
		}
		else if (InRange11(hoek, ANGLE_270))
		{
			*x = 0.0;
			*y = (float) -length;
		}
		else
		{
			*x = (float) (cos(hoek) * length);
			*y = (float) (sin(hoek) * length);
		}
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

int32 LoadShape(LPSTR ShapeName)
{
	int32 MemPos, cnt, cnt2, SizeFile, Pos, NrLibEntries, NrGeometryLibFiles, *ShapeTypeP, ShapeType, NrLines, Shapefp,
	      result, res, Libfp, Found;
	ShapeLinesArray *ShapeLines;
	char str[MAX_LENGTH_STRING], GeometryLibNames[64][MAX_LENGTH_STRING], SearchFileName[MAX_LENGTH_STRING],
	     str2[MAX_LENGTH_STRING];
	ShapeRecord *Shape;
	ShapeInfoRecord *ShapeInfo;
	WIN32_FIND_DATAW FileInfo;
	HANDLE FileSearchHandle;
	LibRecord Lib;
	LibNameRecord LibName;
#ifdef _DEBUG
	int32 ok;
#endif

	SizeFile = 0;
#ifdef _DEBUG

	if (stricmpOwn(ShapeName, "NEW_GEOM_4") == 0)
		ok = 1;

#endif
	Found = 0;
	cnt2 = 0;

	while ((cnt2 < PcbDesign.NrShapes) && (!Found))
	{
		ShapeInfo = &((*Shapes)[cnt2]);

		if (stricmpOwn(ShapeInfo->ShapeName, ShapeName) == 0)
			return cnt2;

		cnt2++;
	}

#ifdef _DEBUG

	if (stricmpOwn(ShapeName, "NEW_GEOM_4") == 0)
		ok = 1;

#endif
	Pos = -1;
	sprintf(str, "%s\\pcb\\shapes\\%s.shp", DesignPath, ShapeName);

	if (FileExistsUTF8(str) != 0)
	{
		sprintf(str, "%s\\shapes\\%s.shp", ProjectPath, ShapeName);

		if (FileExistsUTF8(str) != 0)
		{

// ************************************************************************************
// ************************************************************************************
// Check shape libraries

			// Search in own libraries first
			NrGeometryLibFiles = 0;

			for (cnt = 0; cnt < NrGeometryLibraries; cnt++)
			{
				if (NrGeometryLibFiles < 64)
					strcpy(GeometryLibNames[NrGeometryLibFiles++], GeometryLibraries[cnt]);
			}

			sprintf(SearchFileName, "%s\\shplib\\*.slb", ProjectPath);
			FileSearchHandle = FindFirstFileUTF8(SearchFileName, &FileInfo);
			res = 1;

			if (FileSearchHandle == INVALID_HANDLE_VALUE)
				res = 0;

			while ((res) && (NrGeometryLibFiles < 64))
			{
				UnicodeToUtf8(FileInfo.cFileName, str2, MAX_LENGTH_STRING - 100);
				sprintf(GeometryLibNames[NrGeometryLibFiles++], "%s\\shplib\\%s", ProjectPath, str2);
				res = FindNextFileW(FileSearchHandle, &FileInfo);
			}

			if (FileSearchHandle != INVALID_HANDLE_VALUE)
				FindClose(FileSearchHandle);

			cnt2 = 0;
			Found = 0;

			while ((!Found) && (cnt2 < NrGeometryLibFiles))
			{
				if ((Libfp = FileOpenReadOnlyUTF8(GeometryLibNames[cnt2])) == -1)
					return -1;

				if (FileRead(Libfp, &Lib, sizeof(LibRecord), &result) == -1)
					return -1;

				if (strcmp(Lib.Identification, LibraryCode2) == 0)
				{
					NrLibEntries = Lib.NrLibEntries;
					cnt = 0;

					while ((!Found) && (cnt < NrLibEntries))
					{
						if (FileRead(Libfp, &LibName, sizeof(LibNameRecord), &result) == -1)
							return -1;

						if (stricmpOwn(LibName.Text, ShapeName) == 0)
						{
							Found = 1;
							Pos = LibName.Pos;
							SizeFile = LibName.Length;
							strcpy(str, GeometryLibNames[cnt2]);
						}

						cnt++;
					}
				}

				if (FileClose(Libfp) == -1)
					return -1;

				cnt2++;
			}

			if (!Found)
				return -1;
		}
	}

	if (Pos == -1)
		SizeFile = FileSizeUTF8(str);

	if (ShapesMemSize + SizeFile > MaxSpecialMemory[MEM_SHAPES_MEM])
		AllocateSpecialMem(MEM_SHAPES_MEM, ShapesMemSize + SizeFile + 256 * 1024, (void *) &ShapesMem);

	if ((PcbDesign.NrShapes + 1) * (int32) sizeof(ShapeInfoRecord) > MaxSpecialMemory[MEM_SHAPES])
		AllocateSpecialMem(MEM_SHAPES, MaxSpecialMemory[MEM_SHAPES] + sizeof(ShapeInfoRecord) * 128, (void *) &Shapes);

	strcpy((*Shapes)[PcbDesign.NrShapes].ShapeName, ShapeName);
	(*Shapes)[PcbDesign.NrShapes].ShapePos = ShapesMemSize;
	PcbDesign.NrShapes++;

	if (Pos == -1)
	{
		if ((Shapefp = FileOpenReadOnlyUTF8(str)) == -1)
			return -3;

		if (FileRead(Shapefp, &ShapesMem[ShapesMemSize], SizeFile, &result) == -1)
			return -3;

		FileClose(Shapefp);
	}
	else
	{
		if ((Shapefp = FileOpenReadOnlyUTF8(str)) == -1)
			return -3;

		FileSeek(Shapefp, Pos);

		if (FileRead(Shapefp, &ShapesMem[ShapesMemSize], SizeFile, &result) == -1)
			return -3;

		FileClose(Shapefp);
	}

	Shape = (ShapeRecord *) & ShapesMem[ShapesMemSize];

	if (stricmpOwn(Shape->Identification, ShapeCode) == 0)
	{	// Old shape definition
		Shape->NrLayers = 2;
		Shape->NrPolygons = 0;
		Shape->PolygonOffset = 0;
		Shape->Dummy2 = 0;
	}

	ShapesMemSize += SizeFile;
#ifdef _DEBUG

	if (stricmpOwn(ShapeName, "k0805") == 0)
		ok = 1;

#endif
	strcpy(Shape->ShapeName, ShapeName);

// *******************************************************************************************************
// *******************************************************************************************************

	if (stricmpOwn(Shape->Identification, ShapeCode) == 0)
	{
		MemPos = Shape->CompOutLineOffset;
		NrLines = min(16384, Shape->NrCompOutLines);

		while (NrLines > 0)
		{
			ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
			ShapeTypeP = (int32 *) ShapeLines;
			ShapeType = *ShapeTypeP;
			ShapeType &= 0x0000ff00;

			switch (ShapeType)
			{
			case OBJECT_LINE:	// line
				MemPos += 24;
				break;

			case OBJECT_RECT:	// rect
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_ARC:	// arc
				if ((*ShapeLines)[9] == 0.0)
					(*ShapeLines)[9] = 10.0;

				MemPos += 40;
				break;

			case OBJECT_TEXT:	// text
				MemPos += 24 + 64;
				break;
			}

			NrLines--;
		}

// *******************************************************************************************************
		MemPos = Shape->SilkScreenOffset;
		NrLines = min(16384, Shape->NrSilkScreenOutLines);

		while (NrLines > 0)
		{
			ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
			ShapeTypeP = (int32 *) ShapeLines;
			ShapeType = *ShapeTypeP;
			ShapeType &= 0x0000ff00;

			switch (ShapeType)
			{
			case OBJECT_LINE:	// line
				MemPos += 24;
				break;

			case OBJECT_RECT:	// rect
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
				if ((*ShapeLines)[5] == 0.0)
					(*ShapeLines)[5] = 10.0;

				MemPos += 24;
				break;

			case OBJECT_ARC:	// arc
				if ((*ShapeLines)[9] == 0.0)
					(*ShapeLines)[9] = 10.0;

				MemPos += 40;
				break;

			case OBJECT_TEXT:	// text
				MemPos += 24 + 64;
				break;
			}

			NrLines--;
		}
	}

// *******************************************************************************************************
// *******************************************************************************************************

	return 0;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapePinsToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation, int32 ObjectArrayNr,
                       int32 SelectedLayer, int32 mode)
// mode bit 0 ==  0  -> Object->x3 = Power pad
// mode bit 0 ==  1  -> Object->x3 = Inner pad
// mode bit 1 ==  0  -> All Objects
// mode bit 1 ==  1  -> Only objects with drill holes
// mode bit 2 ==  1  -> Only include layer objects
{
	int32 PinOffset, MemPos, ObjectInclude, NrPins, PinNr, Rotation, NetPinCount, PolygonVertices, NrLayers,
	      MirroredLayer, CompObjectCount, ShapePos, Layer, ShapeInfo, NrPinShapes, ShapeType, Mirror;
	double x, y, Width, Height, OX, OY, OriginX, OriginY, hulp, RotationAngle, x1, y1, x2, y2, x3, y3, x4, y4;
	LPCSTR PinText;

	uint8 *ShapesMem;
	ShapePadRecord *ShapePad;
	PadRecord *Pad;
//  NetRecord       *Net;
	ObjectRecord *Object;
	GeomPolygonRecord *GeomPolygon;
	ShapeLinesArray *ShapeLines;

	OriginX = 0.0;
	OriginY = 0.0;
	Mirror = 0;
	MemPos = 0;
	ShapePos = MemPos;
	ShapesMem = (uint8 *) Shape;
	NrLayers = Shape->NrLayers;
	RotationAngle = TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);
	MirroredLayer = 0;
	NetPinCount = 0;
	OX = OriginX;
	OY = OriginY;
	PinOffset = Shape->PinOffset;
	ShapeInfo = Shape->Info;

	if (ObjectArrayNr == 0)
	{
		if (MaxNrObjects < 1024)
			AllocateMemObjects(1024);
	}
	else
	{
		if (MaxNrObjects2 < 1024)
			AllocateMemObjects2(1024);
	}

	CompObjectCount = 0;
	PolygonVertices = 0;
	NrPins = Shape->NrPins;
	MemPos += PinOffset;
	PinNr = 0;

	while (NrPins > 0)
	{
#ifdef _DEBUG
		/*
		    crc=0xffffffff;
		    bufp=&(ShapesMem[MemPosOld]);
		    for (cnt=0;cnt<MemDiv;cnt++) {
		      crc = (crc >> 8) ^ crc32_table[(crc & 0xFF) ^ bufp[cnt]];
		    }
		    if (crc!=ShapeCrcs[ShapeNr]) {
		      ok=1;
		    }
		*/
#endif
		ShapePad = (ShapePadRecord *) & (ShapesMem[MemPos]);
		NrPinShapes = ShapePad->NrPinShapes;
		MemPos += sizeof(ShapePadRecord);
//    PinStringPos=CompMemPos+CompPin->PinNamePtr;
//    PinText=&(CompsMem[PinStringPos]);
		PinText = (LPCSTR) & (ShapePad->Name);
#ifdef _DEBUG

		if (strcmp(PinText, SC(629, "Layer1")) == 0)
			ok = 1;

#endif

		while (NrPinShapes > 0)
		{
			Pad = (PadRecord *) & (ShapesMem[MemPos]);
			Layer = Pad->Layer;
			ObjectInclude = CheckGeometryLayer(&Layer, NrLayers, Mirror);

			if (Objects == NULL)
				Objects = (ObjectArray *) GlobalLock(ObjectsGlobal);

			Object = &((*Objects)[NrObjects]);

			if (((mode & 2) == 2) && (Layer != -1) && (SelectedLayer != -1))
			{
				if (Layer != SelectedLayer)
					ObjectInclude = 0;
			}

			Object->Layer = Layer;
			ShapeType = Pad->ShapeType;

			if (ObjectInclude)
			{
				x = Pad->X;
				y = Pad->Y;
				Width = Pad->Width;
				Height = Pad->Height;

				x2 = Width;
				y2 = Height;
				Object->PinNr = (int32) PinNr;
				Object->NetNr = 0;
				Object->CompNr = 0;
				Object->TraceNr = 0;
				Object->Info = 0;
				Object->Info2 = 0;
				Object->Info3 = 0;
				Object->Info4 = 0;
				Object->Info5 = 0;
				Object->ObjectType2 = 0;
				Object->PinCount = NetPinCount;
				Object->Text2 = (LPSTR) PinText;

				switch (ShapeType)
				{
				case DRILL_UNPLATED:
					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->y3 = 0.0;	// Inner pad
					}
					else
					{
						Object->y3 = Pad->Extra2;	// Anti power pad
					}

					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->ObjectType = ShapeType;
					break;

				case DRILL:
					if (Mirror == 1)
						x = -x;

					Object->Layer = -1;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->ObjectType = ShapeType;
					break;

				case PIN_PUT_THROUGH_ROUND:	// Put through pin round
					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_PUT_THROUGH_ROUND;
					break;

				case PIN_PUT_THROUGH_SQUARE:	// Put through pin square
					if (Mirror == 1)
						x = -x;

					Object->ObjectType = PIN_PUT_THROUGH_SQUARE;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						Object->RotationAngle = RotationAngle;
						Object->Mirror = Mirror;
						Object->Info = OBJECT_FILLED;
						Object->ObjectType2 = PIN_PUT_THROUGH_SQUARE;
						Object->ObjectType = PIN_PUT_THROUGH_POLYGON;
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					break;

				case PIN_SMD_RECT:	// SMD Pad rect
					Object->ObjectType = PIN_SMD_RECT;

					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						hulp = Width;
						Width = Height;
						Height = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						hulp = Width;
						Width = Height;
						Height = hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						Object->RotationAngle = RotationAngle;
						Object->Mirror = Mirror;
						Object->Info = OBJECT_FILLED;
						Object->ObjectType2 = PIN_SMD_RECT;
						Object->ObjectType = PIN_SMD_POLYGON;
						break;
					}

					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;
					break;

				case PIN_SMD_ROUND:	// SMD Pad round
					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_SMD_ROUND;
					break;

				case PIN_LINE_HOR:	// Hor Line
					if (Mirror == 1)
						x = -x - Width;

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);

					switch (Rotation)
					{
					case 0:
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 2:
						x = -x - Width;
						y = -y;
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp - Width;
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 4:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG2;
						break;

					case 5:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y += Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG1;
						break;

					case 6:
						RotatePoint2(&x, &y, RotationAngle);
						Object->ObjectType = PIN_LINE_DIAG2;
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y -= Width * SQRT05;
						break;

					case 7:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG1;
						break;

					case -1:
						Object->Thickness = Height;
						x2 = Width;

						if (Mirror == 1)
						{
							//                x2=-x2;
						}

						y2 = 0;
						RotatePoint2(&x, &y, RotationAngle);
						RotatePoint2(&x2, &y2, RotationAngle);
						Object->ObjectType = PIN_LINE_ALL_ANGLE;
						Object->x1 = x + OX + OffsetX;
						Object->y1 = y + OY + OffsetY;
						Object->x2 = Object->x1 + x2;
						Object->y2 = Object->y1 + y2;
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_VER:	// Ver Line
					if (Mirror == 1)
						x = -x;

#ifdef _DEBUG

					if (InRange4(Width, 96 * 2540.0))
						ok = 1;

#endif
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);

					switch (Rotation)
					{
					case 0:
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						x = x - Width;
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 2:
						x = -x;
						y = -y - Width;
						Object->ObjectType = PIN_LINE_VER;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						Object->ObjectType = PIN_LINE_HOR;
						break;

					case 4:
						RotatePoint2(&x, &y, RotationAngle);
						Object->ObjectType = PIN_LINE_DIAG1;
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y += Width * SQRT05;
						break;

					case 5:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						x -= Width * SQRT05;
						y -= Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG2;
						break;

					case 6:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG1;
						break;

					case 7:
						RotatePoint2(&x, &y, RotationAngle);
						Object->x2 = Width * SQRT05;
						Object->ObjectType = PIN_LINE_DIAG2;
						break;

					case -1:
						Object->Thickness = Height;
						x2 = 0.0;
						y2 = Width;
						RotatePoint2(&x, &y, RotationAngle);
						RotatePoint2(&x2, &y2, RotationAngle);
						Object->ObjectType = PIN_LINE_ALL_ANGLE;
						Object->x1 = x + OX + OffsetX;
						Object->y1 = y + OY + OffsetY;
						Object->x2 = Object->x1 + x2;
						Object->y2 = Object->y1 + y2;
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_DIAG1:	// Diag1 Line
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);

					if (Mirror == 0)
					{
						switch (Rotation)
						{
						case 0:
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 1:
							hulp = x;
							x = -y;
							y = hulp;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 2:
							x = -x - Width;
							y = -y + Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 3:
							hulp = x;
							x = y - Width;
							y = -hulp - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 4:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_HOR;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 5:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 6:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							x -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 7:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							y -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case -1:
							Object->Thickness = Height;
							x2 = Width;
							y2 = -Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}
					else
					{
						switch (Rotation)
						{
						case 0:
							x = -x - Width;
							y = y - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 1:
							hulp = x;
							x = -y;
							y = -hulp;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 2:
							y = -y;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 3:
							hulp = x;
							x = y - Width;
							y = hulp + Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 4:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_VER;
							Width *= SQRT2;
							y -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 5:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 6:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 7:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							x -= Width;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case -1:
							Object->Thickness = Height;
							x = -x;
							x2 = -Width;
							y2 = -Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_DIAG2:	// Diag2 Line
					Object->x2 = Width;
					Object->y2 = Height;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);

					if (Mirror == 0)
					{
						switch (Rotation)
						{
						case 0:
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 1:
							hulp = x;
							x = -y - Width;
							y = hulp + Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 2:
							x = -x - Width;
							y = -y - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 3:
							hulp = x;
							x = y;
							y = -hulp;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 4:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_VER;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case 5:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_HOR;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							x -= Width;
							break;

						case 6:
							RotatePoint2(&x, &y, RotationAngle);
							Object->ObjectType = PIN_LINE_VER;
							Width *= SQRT2;
							Object->x2 = Width;
							Object->y2 = Height;
							y -= Width;
							break;

						case 7:
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							Object->y2 = Height;
							break;

						case -1:
							Object->Thickness = Height;
							x2 = Width;
							y2 = Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}
					else
					{
						switch (Rotation)
						{
						case 0:
							x = -x - Width;
							y += Width;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 1:
							hulp = x;
							x = -y - Width;
							y = -hulp - Width;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 2:
							y = -y;
							Object->ObjectType = PIN_LINE_DIAG1;
							break;

						case 3:
							hulp = x;
							x = y;
							y = hulp;
							Object->ObjectType = PIN_LINE_DIAG2;
							break;

						case 4:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							x -= Width;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							break;

						case 5:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							y -= Width;
							Object->x2 = Width;
							break;

						case 6:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_HOR;
							Object->x2 = Width;
							break;

						case 7:
							x = -x;
							RotatePoint2(&x, &y, RotationAngle);
							Width *= SQRT2;
							Object->ObjectType = PIN_LINE_VER;
							Object->x2 = Width;
							break;

						case -1:
							Object->Thickness = Height;
							x = -x;
							x2 = -Width;
							y2 = Width;
							RotatePoint2(&x, &y, RotationAngle);
							RotatePoint2(&x2, &y2, RotationAngle);
							Object->ObjectType = PIN_LINE_ALL_ANGLE;
							Object->x1 = x + OX + OffsetX;
							Object->y1 = y + OY + OffsetY;
							Object->x2 = Object->x1 + x2;
							Object->y2 = Object->y1 + y2;
							break;
						}
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					break;

				case PIN_LINE_ALL_ANGLE:
#ifdef _DEBUG
					if (InRange4(96 * 2540.0, CalcLengthLine(x, y, x2, y2)))
						ok = 1;

#endif

					if (Mirror == 1)
					{
						x = -x;
						x2 = -x2;
					}

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						hulp = x2;
						x2 = -y2;
						y2 = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						x2 = -x2;
						y2 = -y2;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						hulp = x2;
						x2 = y2;
						y2 = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						RotatePoint2(&x2, &y2, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->x2 = x2 + OX + OffsetX;
					Object->y2 = y2 + OY + OffsetY;
					Object->Thickness = Pad->Special.Extra1;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->ObjectType = ShapeType;
					/*
					            if ((LicenseResult >> 8) < 30) {
					              ObjectInclude=0;
					            }
					*/
					break;

				case PIN_ARC:	// arc
					ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
					x1 = (*ShapeLines)[2];
					y1 = (*ShapeLines)[3];
					x2 = (*ShapeLines)[4];
					y2 = (*ShapeLines)[5];
					x3 = (*ShapeLines)[6];
					y3 = (*ShapeLines)[7];
					x4 = (*ShapeLines)[8];
					y4 = (*ShapeLines)[9];

					if (x2 == 0)
						x2 = y2;

					if (y2 == 0)
						y2 = x2;

					if (Mirror == 1)
					{
						x1 = -x1;
						x3 = -x3;
						x4 = -x4;
						hulp = x4;
						x4 = x3;
						x3 = hulp;
						hulp = y4;
						y4 = y3;
						y3 = hulp;
					}

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x1;
						x1 = -y1;
						y1 = hulp;
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						hulp = x3;
						x3 = -y3;
						y3 = hulp;
						hulp = x4;
						x4 = -y4;
						y4 = hulp;
						break;

					case 2:
						x1 = -x1;
						y1 = -y1;
						x3 = -x3;
						y3 = -y3;
						x4 = -x4;
						y4 = -y4;
						break;

					case 3:
						hulp = x1;
						x1 = y1;
						y1 = -hulp;
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						hulp = x3;
						x3 = y3;
						y3 = -hulp;
						hulp = x4;
						x4 = y4;
						y4 = -hulp;
						break;

					case 4:	// 45
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;

					case 5:	// 135
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;

					case 6:	// 225
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;

					case 7:	// 315
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						hulp = x2;
						x2 = y2;
						y2 = hulp;
						break;

					case -1:	// Any angle
						RotatePoint2(&x1, &y1, RotationAngle);
						RotatePoint2(&x3, &y3, RotationAngle);
						RotatePoint2(&x4, &y4, RotationAngle);
						break;
					}

					Object->x1 = x1 + OX + OffsetX;
					Object->y1 = y1 + OY + OffsetY;
					Object->x2 = x2;
					Object->y2 = y2;
					Object->x3 = x3;
					Object->y3 = y3;
					Object->x4 = x4;
					Object->y4 = y4;
					Object->ObjectType = ShapeType;
					Object->Thickness = (*ShapeLines)[10];
					Object->Clearance = max(PcbDesign.StandardClearance, (*ShapeLines)[11]);
					/*
					            if ((LicenseResult >> 8) < 30) {
					              ObjectInclude=0;
					            }
					*/
					break;

				case OBJECT_POLYGON:
				case PIN_SMD_POLYGON:
					GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + Pad->Special.AddressOffset]);
					PolygonVertices = GeomPolygon->NrVertices;

					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->RotationAngle = RotationAngle;
					Object->Mirror = Mirror;
					Object->x2 = 0.0;
					Object->y2 = 0.0;
					Object->x3 = 0.0;
					Object->Address = (uint8 *) GeomPolygon;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_SMD_POLYGON;
					/*
					            if ((LicenseResult >> 8) < 30) {
					              ObjectInclude=0;
					            }
					*/
					break;

				case PIN_PUT_THROUGH_POLYGON:
					GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + Pad->Special.AddressOffset]);
					PolygonVertices = GeomPolygon->NrVertices;

					if (Mirror == 1)
						x = -x;

					switch (Rotation)
					{
					case 0:
						break;

					case 1:
						hulp = x;
						x = -y;
						y = hulp;
						break;

					case 2:
						x = -x;
						y = -y;
						break;

					case 3:
						hulp = x;
						x = y;
						y = -hulp;
						break;

					default:
						RotatePoint2(&x, &y, RotationAngle);
						break;
					}

					Object->x1 = x + OX + OffsetX;
					Object->y1 = y + OY + OffsetY;
					Object->RotationAngle = RotationAngle;
					Object->Mirror = Mirror;
					Object->x2 = 0.0;
					Object->y2 = Height;

					if (mode == 0)
					{
						Object->x3 = Pad->Extra2;	// Anti power pad
						Object->y3 = Pad->Special.Extra1;	// Inner pad
					}
					else
					{
						Object->x3 = Pad->Special.Extra1;	// Inner pad
						Object->y3 = Pad->Extra2;	// Anti power pad

						if (Object->x3 == 0.0)
							Object->x3 = Width;
					}

					Object->Address = (uint8 *) GeomPolygon;
					Object->Clearance = max(PcbDesign.StandardClearance, Pad->Clearance);
					Object->ObjectType = PIN_PUT_THROUGH_POLYGON;
					/*
					            if ((LicenseResult >> 8) < 30) {
					              ObjectInclude=0;
					            }
					*/
					break;

				default:
					ObjectInclude = 0;
					break;
				}

				Object->Clearance = max(PcbDesign.StandardClearance, Object->Clearance);
			}

			if (ShapeType != PIN_ARC)
				MemPos += sizeof(PadRecord);
			else
				MemPos += 48;

			NrPinShapes--;

			if (ObjectInclude)
			{
#ifdef _DEBUG

				if ((InRangeSpecial(Object->x1, 2.54e5, 0.05e5)) && (InRangeSpecial(Object->y1, 11.05e5, 0.05e5)))
					ok = 1;

				if (Object->Layer == 4301)
					ok = 1;

#endif

				if (ObjectArrayNr == 0)
				{
					if (NrObjects >= MaxNrObjects - 1)
					{
						if (AllocateMemObjects(MaxNrObjects + 1024) == 0)
							NrObjects++;
					}
					else
						NrObjects++;
				}
				else
				{
					if (NrObjects2 >= MaxNrObjects2 - 1)
					{
						if (AllocateMemObjects2(MaxNrObjects2 + 128) == 0)
							NrObjects2++;
					}
					else
						NrObjects2++;
				}

				CompObjectCount++;
			}
		}

		NrPins--;
		PinNr++;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapePlacementOutLineToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation)
{
	int32 MemPos, *ShapeInfoP, NrLines, ShapeInfo2, Rotation, ShapeInfo, TextLength, Mirror;
	double hulp, x1, y1, x2, y2, x3, y3, x4, y4, OX, OY, OriginX, OriginY, RotationAngle;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	uint8 y4a, *ShapesMem;
#ifdef _DEBUG
	int32 ok;
#endif

// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored

	if (MaxNrObjects6 < 1024)
		AllocateMemObjects6(1024);

	OriginX = 0.0;
	OriginY = 0.0;
	Mirror = 0;
	MemPos = 0;
	ShapesMem = (uint8 *) Shape;
#ifdef _DEBUG

	if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
		ok = 1;

	if (Mirror == 1)
		ok = 1;

#endif
	ShapeInfo2 = Shape->Info;
//  ShapeInfo2=0;
	RotationAngle = TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	OX = OriginX;
	OY = OriginY;
	MemPos += sizeof(ShapeRecord);
	NrLines = min(16384, Shape->NrPlacementOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeInfoP = (int32 *) ShapeLines;
		ShapeInfo = *ShapeInfoP;
		TextLength = ShapeInfo & 0xff;
		ShapeInfo &= 0x0000ff00;

		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		Object = &((*Objects6)[NrObjects6]);
//      Object->CompNr=CompNr;
		Object->CompNr = 0;
		Object->Info = 0;
		Object->Info2 = 0;
		Object->Info3 = 0;
		Object->Info4 = 0;
		Object->Info5 = 0;
		Object->Text2 = NULL;
		Object->ObjectType2 = 0;

		Object->Layer = PLACEMENT_OUTLINE_TOP;

		switch (ShapeInfo)
		{
		case OBJECT_LINE:		// line
			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2 + OX + OffsetX;
			Object->y2 = y2 + OY + OffsetY;
			Object->ObjectType = OBJECT_LINE;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			Object->Thickness = 0.0;

			if (Mirror == 1)
				x1 = -x1;

			Object->ObjectType = OBJECT_RECT;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->Clearance = 0.0;
			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			x3 = (*ShapeLines)[5];
			y3 = (*ShapeLines)[6];
			x4 = (*ShapeLines)[7];
			y4 = (*ShapeLines)[8];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			MemPos += 40;
			break;
		}

		if (NrObjects6 >= MaxNrObjects6 - 1)
		{
			if (AllocateMemObjects6(MaxNrObjects6 + 1024) == 0)
				NrObjects6++;
		}
		else
			NrObjects6++;

		NrLines--;
	}
}


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapeCompOutLineToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation)
{
	int32 MemPos, NrLines, Rotation, ShapeType, TextLength, ShapeMirror, NewMirror, *ShapeTypeP, PolygonVertices,
	      ShapePos, *PolygonPointer, Mirror, ShapeInfo2, ObjectInclude;
	double hulp, x1, y1, x2, y2, x3, y3, x4, y4, OX, OY, OriginX, OriginY, RotationAngle, RotationAngle2, TextRotation;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	uint8 y4a, *ShapesMem;
	GeomPolygonRecord *GeomPolygon;

// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored

	if (MaxNrObjects6 < 1024)
		AllocateMemObjects6(1024);

	PolygonVertices = 0;

	OriginX = 0.0;
	OriginY = 0.0;
	Mirror = 0;
	MemPos = 0;
	ShapesMem = (uint8 *) Shape;
	ShapePos = MemPos;
	ShapeInfo2 = Shape->Info;
#ifdef _DEBUG

	if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
		ok = 1;

#endif
	RotationAngle = TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	OX = OriginX;
	OY = OriginY;
	MemPos += Shape->CompOutLineOffset;
	NrLines = min(16384, Shape->NrCompOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeTypeP = (int32 *) ShapeLines;
		ShapeType = *ShapeTypeP;
		TextLength = ShapeType & 0xff;
		ShapeType &= 0x0000ff00;
		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		ObjectInclude = 1;

		if (Objects6 == NULL)
			Objects6 = (ObjectArray *) GlobalLock(Objects6Global);

		Object = &((*Objects6)[NrObjects6]);
		Object->CompNr = 0;
		Object->Text2 = NULL;
		Object->Info = 0;
		Object->Info2 = 0;
		Object->Info3 = 0;
		Object->Info4 = 0;
		Object->Info5 = 0;
		Object->ObjectType2 = 0;
		Object->Layer = COMP_OUTLINE_LAYER;

		switch (ShapeType)
		{
		case OBJECT_LINE:		// line
			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2 + OX + OffsetX;
			Object->y2 = y2 + OY + OffsetY;
			Object->ObjectType = OBJECT_LINE;
			Object->Thickness = (*ShapeLines)[5];
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			Object->x2 = x2;
			Object->y2 = y2;

			if (Mirror == 1)
			{
				x1 = -x1;
				Object->Layer ^= 1;
			}

			Object->ObjectType = OBJECT_RECT;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			x3 = (*ShapeLines)[5];
			y3 = (*ShapeLines)[6];
			x4 = (*ShapeLines)[7];
			y4 = (*ShapeLines)[8];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[9];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 40;
			break;

		case OBJECT_TEXT:		// text
			GetRotationAndMirrorFromShapeText(y2, &TextRotation, &ShapeMirror);
//        TextRotation=((GetRotationFromComp((int32)y2)) & 7)*45.0;
			RotationAngle = TempRotation;
			RotationAngle2 = TextRotation;
			NewMirror = Mirror ^ ShapeMirror;

			if (Mirror == 0)
				RotationAngle2 += RotationAngle;
			else
			{
				x1 = -x1;
				RotationAngle2 -= RotationAngle;
				Object->Layer ^= 1;
			}

			if (RotationAngle2 > 360)
				RotationAngle2 -= 360;

			if (RotationAngle2 < 0)
				RotationAngle2 += 360;

			Rotation = GetRotationFromFloat(RotationAngle);

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->RotationAngle = RotationAngle2;
			Object->Mirror = Mirror;
			Object->ObjectType = OBJECT_TEXT;
			Object->Thickness = (*ShapeLines)[5];
			Object->Text1 = (LPSTR) & ((*ShapeLines)[6]);
			MemPos += 24 + 64;
			break;

		case PIN_SMD_POLYGON:
		case OBJECT_POLYGON:
			PolygonPointer = (int32 *) & (*ShapeLines)[3];
			GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + *PolygonPointer]);
			PolygonVertices = GeomPolygon->NrVertices;

			if (Mirror == 1)
			{
				x1 = -x1;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->RotationAngle = RotationAngle;
			Object->Mirror = Mirror;
			Object->Address = (uint8 *) GeomPolygon;
			Object->Info = OBJECT_FILLED;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			Object->ObjectType = OBJECT_POLYGON;
			MemPos += 16;
			/*
			        if ((LicenseResult >> 8) < 30) {
			          ObjectInclude=0;
			        }
			*/
			break;
		}

		if (ObjectInclude)
		{
			if (NrObjects6 >= MaxNrObjects6 - 1)
			{
				if (AllocateMemObjects6(MaxNrObjects6 + 1024) == 0)
					NrObjects6++;
			}
			else
				NrObjects6++;
		}

		NrLines--;
	}

	ok = 1;
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapeCompSilkScreenToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation)
{
	int32 MemPos, NrLines, Rotation, ShapeType, TextLength, PolygonVertices, ShapeMirror, *ShapeTypeP, ShapePos,
	      *PolygonPointer, NewMirror, Mirror, ShapeInfo2, ObjectInclude;
	double hulp, x1, y1, x2, y2, x3, y3, x4, y4, OX, OY, OriginX, OriginY, RotationAngle, RotationAngle2, TextRotation;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	uint8 y4a, *ShapesMem;
	GeomPolygonRecord *GeomPolygon;
#ifdef _DEBUG
	int32 ok;
#endif
// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored

	if (MaxNrObjects6 < 1024)
		AllocateMemObjects6(1024);

	PolygonVertices = 0;
	OriginX = 0.0;
	OriginY = 0.0;
	Mirror = 0;
	MemPos = 0;
	ShapePos = 0;
	ShapesMem = (uint8 *) Shape;
	ShapeInfo2 = Shape->Info;
#ifdef _DEBUG

	if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
		ok = 1;

#endif

	RotationAngle = TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	OX = OriginX;
	OY = OriginY;
	MemPos += Shape->SilkScreenOffset;
	NrLines = min(16384, Shape->NrSilkScreenOutLines);

	while (NrLines > 0)
	{
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		ShapeTypeP = (int32 *) ShapeLines;
		ShapeType = *ShapeTypeP;
		TextLength = ShapeType & 0xff;
		ShapeType &= 0x0000ff00;
		x1 = (*ShapeLines)[1];
		y1 = (*ShapeLines)[2];
		x2 = (*ShapeLines)[3];
		y2 = (*ShapeLines)[4];
		ObjectInclude = 1;

		if (Objects6 == NULL)
			Objects = (ObjectArray *) GlobalLock(Objects6Global);

		Object = &((*Objects6)[NrObjects6]);
		Object->CompNr = 0;
		Object->Info = 0;
		Object->Info2 = 0;
		Object->Info3 = 0;
		Object->Info4 = 0;
		Object->Info5 = 0;
		Object->Text2 = NULL;
		Object->ObjectType2 = 0;
		Object->Layer = SILKSCREEN_TOP;

		switch (ShapeType)
		{
		case OBJECT_LINE:		// line
			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2 + OX + OffsetX;
			Object->y2 = y2 + OY + OffsetY;
			Object->ObjectType = OBJECT_LINE;
			Object->Clearance = 0.0;
			Object->Thickness = max(PcbDesign.SilkScreenWidth, (*ShapeLines)[5]);
			MemPos += 24;
			break;

		case OBJECT_RECT:		// rect
			Object->x2 = x2;
			Object->y2 = y2;

			if (Mirror == 1)
			{
				x1 = -x1;
				Object->Layer ^= 1;
			}

			Object->ObjectType = OBJECT_RECT;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				Object->x2 = x2;
				Object->y2 = y2;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				Object->x2 = x2;
				Object->y2 = y2;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->Clearance = 0.0;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 24;
			break;

		case OBJECT_CIRCLE:	// circle   (X2 = thickness,Y2 circle type)
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[5];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 24;
			break;

		case OBJECT_ARC:		// arc
			x3 = (*ShapeLines)[5];
			y3 = (*ShapeLines)[6];
			x4 = (*ShapeLines)[7];
			y4 = (*ShapeLines)[8];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;
				Object->Layer ^= 1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = (*ShapeLines)[9];

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			MemPos += 40;
			/*
			        if ((LicenseResult >> 8) < 30) {
			          ObjectInclude=0;
			        }
			*/
			break;

		case OBJECT_TEXT:		// text
#ifdef _DEBUG
			if (stricmpOwn((LPSTR) & ((*ShapeLines)[6]), "text0") == 0)
				ok = 1;

			if (TempRotation == 90.0)
				ok = 1;

#endif

			GetRotationAndMirrorFromShapeText(y2, &TextRotation, &ShapeMirror);
//        TextRotation=((GetRotationFromComp((int32)y2)) & 7)*45.0;
			RotationAngle = TempRotation;
			RotationAngle2 = TextRotation;
			NewMirror = Mirror ^ ShapeMirror;

			if (Mirror == 0)
				RotationAngle2 += RotationAngle;
			else
			{
				x1 = -x1;
				RotationAngle2 -= RotationAngle;
				Object->Layer ^= 1;
			}

			if (RotationAngle2 > 360)
				RotationAngle2 -= 360;

			if (RotationAngle2 < 0)
				RotationAngle2 += 360;

			Rotation = GetRotationFromFloat(RotationAngle);

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->x2 = x2;
			Object->RotationAngle = RotationAngle2;
			Object->Mirror = Mirror;
			Object->ObjectType = OBJECT_TEXT;
			Object->Thickness = (*ShapeLines)[5];
			Object->Text1 = (LPSTR) & ((*ShapeLines)[6]);
			MemPos += 24 + 64;
			break;

		case OBJECT_POLYGON:
			PolygonPointer = (int32 *) & ((*ShapeLines)[3]);
			GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + *PolygonPointer]);
			PolygonVertices = GeomPolygon->NrVertices;

			if (Mirror == 1)
			{
				Object->Layer ^= 1;
				x1 = -x1;
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OX + OffsetX;
			Object->y1 = y1 + OY + OffsetY;
			Object->RotationAngle = RotationAngle;
			Object->Mirror = Mirror;
			Object->Address = (uint8 *) GeomPolygon;
			Object->Info = OBJECT_FILLED;
			Object->Clearance = 0.0;
			Object->Thickness = 0.0;
			Object->ObjectType = OBJECT_POLYGON;
			MemPos += 16;
			/*
			        if ((LicenseResult >> 8) < 30) {
			          ObjectInclude=0;
			        }
			*/
			break;
		}

		if (ObjectInclude)
		{
			if (NrObjects6 >= MaxNrObjects6 - 1)
			{
				if (AllocateMemObjects6(MaxNrObjects6 + 1024) == 0)
					NrObjects6++;
			}
			else
				NrObjects6++;
		}

		NrLines--;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

void ShapeOtherToObject(ShapeRecord * Shape, double OffsetX, double OffsetY, double TempRotation, int32 SelectedLayer,
                        int32 mode)
{

	/*
	mode :

	0:  Everything
	1:  Paste/solder masks
	2:  Info objects
	3:  Boardoutline objects
	4:  Routing keepout
	5:  Silkscreen

	*/
	int32 MemPos, NrLayers, NrLines, Rotation, Layer, PolygonVertices, ShapePos, ShapeMirror, NewMirror, Mirror,
	      ShapeType, OkToAddObject, ObjectInclude;
	double hulp, x, y, OriginX, OriginY, Width, Height, RotationAngle, x1, y1, x2, y2, x3, y3, x4, y4, RotationAngle2,
	       TextRotation;
	ShapeLinesArray *ShapeLines;
	ObjectRecord *Object;
	PadRecord *Pad;
	uint8 y4a, *ShapesMem;
	LPSTR TextP;
	GeomPolygonRecord *GeomPolygon;

#ifdef _DEBUG
	int32 ok;
#endif

	/*
	  if ((LicenseResult >> 8) < 30) {
	    if (mode==3) return;
	    if (mode==4) return;
	  }
	*/
// CompMode bits
//
// 0x01   rotation
// 0x02   rotation
// 0x04   rotation (+45)
// 0x08   mirrored

	x1 = 0.0;
	y1 = 0.0;
	x2 = 0.0;
	y2 = 0.0;
	PolygonVertices = 0;
	OriginX = 0.0;
	OriginY = 0.0;
	Mirror = 0;
	MemPos = 0;
	ShapePos = 0;
//  MemPos=(*Shapes)[ShapeNr].ShapePos;
	ShapesMem = (uint8 *) Shape;
	NrLayers = Shape->NrLayers;
#ifdef _DEBUG

	if (stricmpOwn(Shape->ShapeName, "con4a_usb_a_angle_f_lowprofile") == 0)
	{
		ok = 1;

		if ((mode & 15) == 4)
			ok = 1;
	}

#endif
	RotationAngle = TempRotation;
	Rotation = GetRotationFromFloat(RotationAngle);

	MemPos += Shape->OtherObjectsOffset;
	NrLines = min(65536, Shape->NrOtherObjects);

	while (NrLines > 0)
	{
		Pad = (PadRecord *) & (ShapesMem[MemPos]);
		ShapeLines = (ShapeLinesArray *) & (ShapesMem[MemPos]);
		Layer = Pad->Layer;
#ifdef _DEBUG

		if (Layer == BOARD_OUTLINE_LAYER)
			ok = 1;

#endif
		ObjectInclude = 1;

		if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
		{
			Layer -= ROUTING_KEEPOUT_LAYER;
			ObjectInclude = CheckGeometryLayer(&Layer, NrLayers, Mirror);
			Layer += ROUTING_KEEPOUT_LAYER;
		}

		ShapeType = Pad->ShapeType;
		OkToAddObject = 0;
		x = Pad->X;
		y = Pad->Y;
		Width = Pad->Width;
		Height = Pad->Height;

		if ((mode & 16) == 0)
		{
			if (NrObjects >= MaxNrObjects - 1)
				AllocateMemObjects(MaxNrObjects + 1024);

			if (Objects == NULL)
				Objects6 = (ObjectArray *) GlobalLock(ObjectsGlobal);

			Object = &((*Objects)[NrObjects]);
		}
		else
		{
			if (NrObjects6 >= MaxNrObjects6 - 1)
				AllocateMemObjects6(MaxNrObjects6 + 1024);

			if (Objects6 == NULL)
				Objects6 = (ObjectArray *) GlobalLock(Objects6Global);

			Object = &((*Objects6)[NrObjects6]);
		}

		Object->Layer = Layer;
		Object->Info = 0;
		Object->Info2 = 0;
		Object->Info3 = 0;
		Object->Info4 = 0;
		Object->Info5 = 0;
		Object->Text2 = NULL;
		Object->TraceNr = 0;
		Object->ObjectType = ShapeType;
		Object->ObjectType2 = 0;

		switch (ShapeType)
		{
		case OBJECT_LINE:		// line
		case PIN_LINE_ALL_ANGLE:	// line
		case PIN_LINE_VER:
		case PIN_LINE_HOR:
		case PIN_LINE_DIAG1:
		case PIN_LINE_DIAG2:
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;

			switch (ShapeType)
			{
			case PIN_LINE_VER:
				x2 = x;
				y2 = y + Width;
				break;

			case PIN_LINE_HOR:
				x2 = x + Width;
				y2 = y;
				break;

			case PIN_LINE_DIAG1:
				x2 = x + Width;
				y2 = y - Width;
				break;

			case PIN_LINE_DIAG2:
				x2 = x + Width;
				y2 = y + Width;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x2 = -x2;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = -y2;
				y2 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x2 = -x2;
				y2 = -y2;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x2, &y2, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2 + OffsetX + OriginX;
			Object->y2 = y2 + OffsetY + OriginY;
			Object->ObjectType = OBJECT_LINE;
			Object->Thickness = Pad->Special.Thickness;

			if (ShapeType == OBJECT_LINE)
				Object->Info2 = (int32) (Pad->Extra2 + 0.1);

			switch (ShapeType)
			{
			case PIN_LINE_VER:
			case PIN_LINE_HOR:
			case PIN_LINE_DIAG1:
			case PIN_LINE_DIAG2:
				Object->Thickness = Height;
				break;
			}

			break;

		case OBJECT_RECT:
		case PIN_SMD_RECT:
			Object->ObjectType = OBJECT_RECT;

			if (Mirror == 1)
			{
				x = -x;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			Object->Clearance = 0.0;
			Object->Thickness = Pad->Special.Thickness;

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x;
				x = -y;
				y = hulp;
				hulp = Width;
				Width = Height;
				Height = hulp;
				break;

			case 2:
				x = -x;
				y = -y;
				break;

			case 3:
				hulp = x;
				x = y;
				y = -hulp;
				hulp = Width;
				Width = Height;
				Height = hulp;
				break;

			default:
				RotatePoint2(&x, &y, RotationAngle);
				Object->RotationAngle = RotationAngle;
				Object->Mirror = Mirror;
				Object->ObjectType2 = OBJECT_RECT;
				Object->ObjectType = OBJECT_POLYGON;
				break;
			}

			Object->x1 = x + OffsetX + OriginX;
			Object->y1 = y + OffsetY + OriginY;
			Object->x2 = Width;
			Object->y2 = Height;
			break;

		case OBJECT_CIRCLE:
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;
			y4a = (uint8) (y2 + 0.1);

			switch (y4a)
			{
			case 1:
				x3 = x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = x2;
				break;

			case 2:
				x3 = 0.0;
				y3 = -x2;
				x4 = x2;
				y4 = 0.0;
				break;

			case 3:
				x3 = 0.0;
				y3 = -x2;
				x4 = 0.0;
				y4 = x2;
				break;

			case 4:
				x3 = -x2;
				y3 = 0.0;
				x4 = 0.0;
				y4 = -x2;
				break;

			case 6:
				x3 = -x2;
				y3 = 0.0;
				x4 = x2;
				y4 = 0.0;
				break;

			case 8:
				x3 = 0.0;
				y3 = x2;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 9:
				x3 = x2;
				y3 = 0.0;
				x4 = -x2;
				y4 = 0.0;
				break;

			case 12:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = -x2;
				break;

			default:
				x3 = 0.0;
				y3 = x2;
				x4 = 0.0;
				y4 = x2;
				break;
			}

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:			// 90
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:			// 180
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:			// 270
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2;
			Object->y2 = x2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->Clearance = 0.0;
			Object->ObjectType = OBJECT_ARC;
			Object->Thickness = Pad->Special.Thickness;

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			break;

		case OBJECT_ARC:		// arc
		case PIN_ARC:			// arc
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;
			x3 = (*ShapeLines)[6];
			y3 = (*ShapeLines)[7];
			x4 = (*ShapeLines)[8];
			y4 = (*ShapeLines)[9];

			if (x2 == 0)
				x2 = y2;

			if (y2 == 0)
				y2 = x2;

			if (Mirror == 1)
			{
				x1 = -x1;
				x3 = -x3;
				x4 = -x4;
				hulp = x4;
				x4 = x3;
				x3 = hulp;
				hulp = y4;
				y4 = y3;
				y3 = hulp;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = -y3;
				y3 = hulp;
				hulp = x4;
				x4 = -y4;
				y4 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				x3 = -x3;
				y3 = -y3;
				x4 = -x4;
				y4 = -y4;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				hulp = x3;
				x3 = y3;
				y3 = -hulp;
				hulp = x4;
				x4 = y4;
				y4 = -hulp;
				break;

			case 4:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 5:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			case 6:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;

			case 7:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				hulp = x2;
				x2 = y2;
				y2 = hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				RotatePoint2(&x3, &y3, RotationAngle);
				RotatePoint2(&x4, &y4, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2;
			Object->y2 = y2;
			Object->x3 = x3;
			Object->y3 = y3;
			Object->x4 = x4;
			Object->y4 = y4;
			Object->ObjectType = OBJECT_ARC;
			Object->Clearance = 0.0;
			Object->Thickness = (*ShapeLines)[10];
			Object->Info = 0;

			if (Object->Thickness == 0.0)
				Object->Info = OBJECT_FILLED;

			/*
			        if ((LicenseResult >> 8) < 30) {
			          ObjectInclude=0;
			        }
			*/
			break;

		case OBJECT_TEXT:		// text
			TextP = (LPSTR) & ((*ShapeLines)[7]);
#ifdef _DEBUG

			if (stricmpOwn(Shape->ShapeName, "pga321_026_zif") == 0)
				ok = 1;

			if (stricmpOwn(TextP, "   Info1_0") == 0)
			{
				ok = 1;

				if (Pad->Height > 1000.0)
					ok = 1;
			}

#endif
			x1 = x;
			y1 = y;
			x2 = Width;
			y2 = Height;

			GetRotationAndMirrorFromShapeText(Pad->Height, &TextRotation, &ShapeMirror);
//        TextRotation=((GetRotationFromComp((int32)y2)) & 7)*45.0;
			RotationAngle = TempRotation;
			RotationAngle2 = TextRotation;
			NewMirror = Mirror ^ ShapeMirror;

			if (Mirror == 0)
			{
				RotationAngle2 += RotationAngle;

				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				x1 = -x1;
				RotationAngle2 -= RotationAngle;
			}

			if (RotationAngle2 > 360)
				RotationAngle2 -= 360;

			if (RotationAngle2 < 0)
				RotationAngle2 += 360;

			Rotation = GetRotationFromFloat(RotationAngle);

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x1;
				x1 = -y1;
				y1 = hulp;
				break;

			case 2:
				x1 = -x1;
				y1 = -y1;
				break;

			case 3:
				hulp = x1;
				x1 = y1;
				y1 = -hulp;
				break;

			default:
				RotatePoint2(&x1, &y1, RotationAngle);
				break;
			}

			Object->x1 = x1 + OffsetX + OriginX;
			Object->y1 = y1 + OffsetY + OriginY;
			Object->x2 = x2;
			Object->RotationAngle = RotationAngle2;
			Object->Mirror = Mirror;
			Object->ObjectType = OBJECT_TEXT;
			Object->Thickness = (*ShapeLines)[6];
			Object->Clearance = 0.0;
			Object->Text1 = TextP;
			break;

		case PIN_SMD_POLYGON:
		case OBJECT_POLYGON:
#ifdef _DEBUG
			ok = 1;				// ShapePos
#endif
			GeomPolygon = (GeomPolygonRecord *) & (ShapesMem[ShapePos + Pad->Special.AddressOffset]);
#ifdef _DEBUG

			if (GeomPolygon == NULL)
			{
				ok = 1;			// ShapePos
			}

#endif
			PolygonVertices = GeomPolygon->NrVertices;

			if (Mirror == 1)
			{
				x = -x;

				switch (Layer)
				{
				case SOLD_MASK_BOTTOM:
				case SOLD_MASK_TOP:
				case PASTE_MASK_BOTTOM:
				case PASTE_MASK_TOP:
					Object->Layer ^= 1;
					break;
				}
			}
			else
			{
				switch (Layer)
				{
				case SILKSCREEN_TOP:
				case SILKSCREEN_BOTTOM:
					Object->Layer ^= 1;
					break;
				}
			}

			switch (Rotation)
			{
			case 0:
				break;

			case 1:
				hulp = x;
				x = -y;
				y = hulp;
				break;

			case 2:
				x = -x;
				y = -y;
				break;

			case 3:
				hulp = x;
				x = y;
				y = -hulp;
				break;

			default:
				RotatePoint2(&x, &y, RotationAngle);
				break;
			}

			Object->x1 = x + OffsetX + OriginX;
			Object->y1 = y + OffsetY + OriginY;
#ifdef _DEBUG

			if ((InRange9(Object->x1, 3.5e5)) && (InRange9(Object->y1, 244.8e5)))
				ok = 1;

#endif
			Object->RotationAngle = RotationAngle;
			Object->Mirror = Mirror;
			Object->Address = (uint8 *) GeomPolygon;
			Object->Thickness = 0.0;
			Object->Info = OBJECT_FILLED;
			Object->Clearance = 0.0;
			Object->ObjectType = OBJECT_POLYGON;
			/*
			        if ((LicenseResult >> 8) < 30) {
			          ObjectInclude=0;
			        }
			*/
			break;
		}

		switch (ShapeType)
		{
		case OBJECT_ARC:
		case PIN_ARC:
			MemPos += 48;
			break;

		case OBJECT_TEXT:
			MemPos += 28 + 64;
			break;

		default:
			MemPos += sizeof(PadRecord);
			break;
		}

		switch (mode & 15)
		{
		case 0:				// Everything
			OkToAddObject = 1;
			break;

		case 1:				// Paste/solder masks
			if ((Layer == SOLD_MASK_BOTTOM) || (Layer == SOLD_MASK_TOP) || (Layer == PASTE_MASK_BOTTOM)
			        || (Layer == PASTE_MASK_TOP))
				OkToAddObject = 1;

			break;

		case 2:				// Info objects

//        if ((LicenseResult >> 8) >= 30) {
			if ((Layer == INFO_LAYER) || (Layer == INFO_LAYER2) || (Layer == INFO_LAYER3) || (Layer == INFO_LAYER4))
				OkToAddObject = 1;

//        }
			break;

		case 3:				// Boardoutline objects
			if (Layer == BOARD_OUTLINE_LAYER)
			{
//          if ((LicenseResult >> 8) >= 30) {
				OkToAddObject = 1;
			}

//        }
			break;

		case 4:				// Routing keepout

//        if ((LicenseResult >> 8) >= 30) {
			if ((Layer >= ROUTING_KEEPOUT_LAYER) && (Layer < ROUTING_KEEPOUT_LAYER + 32))
			{
				if (SelectedLayer == -1)
					OkToAddObject = 1;
				else
				{
					if (Layer == SelectedLayer)
						OkToAddObject = 1;
				}
			}

//        }
			break;

		case 5:				// Silkscreen
			if ((Layer == SILKSCREEN_TOP) || (Layer == SILKSCREEN_BOTTOM))
				OkToAddObject = 1;

			break;
		}

		if ((ObjectInclude) && (OkToAddObject))
		{
			if ((mode & 16) == 0)
			{
				if (NrObjects >= MaxNrObjects - 1)
				{
					if (AllocateMemObjects(MaxNrObjects + 1024) == 0)
						NrObjects++;
				}
				else
					NrObjects++;
			}
			else
			{
				if (NrObjects6 >= MaxNrObjects6 - 1)
				{
					if (AllocateMemObjects6(MaxNrObjects6 + 1024) == 0)
						NrObjects6++;
				}
				else
					NrObjects6++;
			}
		}

		NrLines--;
	}
}

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
