/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: qspline.c
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

// This routine contains the code to convert a quadratic spline into a
// polyline.  This code is taken from our TT rasterizer source code and
// was origially written by the folks at APPLE.
// Modified for the use in GDI by Gunter Zieber [gunterz]   11/30/90


// DO NOT change these constants without understanding implications:
// overflow, out of range, out of memory, quality considerations, etc...
// COPYRIGHT:
//
//   (C) Copyright Microsoft Corp. 1993.  All rights reserved.
//
//   You have a royalty-free right to use, modify, reproduce and
//   distribute the Sample Files (and/or any modified version) in
//   any way you find useful, provided that you agree that
//   Microsoft has no warranty obligations or liability for any
//   Sample Application Files which are modified.
//

#define PIXELSIZE 64			/* number of units per pixel. It has to be a power of two */
#define PIXSHIFT   6			/* should be 2log of PIXELSIZE */
#define ERRDIV     16			/* maximum error is  (pixel/ERRDIV) */
#define ERRSHIFT 4				/* = 2log(ERRDIV), define only if ERRDIV is a power of 2 */
#define ONE 0x40				/* constants for 26.6 arithmetic */
#define HALF 0x20
#define HALFM 0x1f

// The maximum number of vectors a spline segment is broken down into
// is 2 ^ MAXGY
// MAXGY can at most be:
// (31 - (input range to sc_DrawParabola 15 + PIXSHIFT = 21)) / 2

#define MAXGY 5
#define MAXMAXGY 8				/* related to MAXVECTORS */

// RULE OF THUMB: xPoint and yPoints will run out of space when
//        MAXVECTORS = 176 + ppem/4 (ppem = pixels per EM)
#define MAXVECTORS 257			/* must be at least 257  = (2 ^ MAXMAXGY) + 1  */

typedef signed long int32;
typedef unsigned short uint16;

typedef struct _POINTFX
{	// definition of 16.16 point structure
	int32 pt_x;
	int32 pt_y;
} POINTFX;

typedef struct _QS
{	// definition of QSpline data points
	POINTFX ptfxStartPt;		// structure
	POINTFX ptfxCntlPt;
	POINTFX ptfxEndPt;
} QS;

typedef QS *LPQS;

typedef struct _POINT
{
	int x;
	int y;
} POINT;

typedef POINT *LPPOINT;

int32 TotalCount;

/*
 * This function break up a parabola defined by three points (A,B,C) and breaks it
 * up into straight line vectors given a maximium error. The maximum error is
 * 1/resolution * 1/ERRDIV. ERRDIV is defined in sc.h.
 *
 *
 *         B *-_
 *          /   `-_
 *         /       `-_
 *        /           `-_
 *       /               `-_
 *      /                   `* C
 *   A *
 *
 * PARAMETERS:
 *
 * Ax, Ay contains the x and y coordinates for point A.
 * Bx, By contains the x and y coordinates for point B.
 * Cx, Cy contains the x and y coordinates for point C.
 * hX, hY are handles to the areas where the straight line vectors are going to be put.
 * count is pointer to a count of how much data has been put into *hX, and *hY.
 *
 * F (t) = (1-t)^2 * A + 2 * t * (1-t) * B + t * t * C, t = 0... 1 =>
 * F (t) = t * t * (A - 2B + C) + t * (2B - 2A) + A  =>
 * F (t) = alfa * t * t + beta * t + A
 * Now say that s goes from 0...N, => t = s/N
 * set: G (s) = N * N * F (s/N)
 * G (s) = s * s * (A - 2B + C) + s * N * 2 * (B - A) + N * N * A
 * => G (0) = N * N * A
 * => G (1) = (A - 2B + C) + N * 2 * (B - A) + G (0)
 * => G (2) = 4 * (A - 2B + C) + N * 4 * (B - A) + G (0) =
 *           3 * (A - 2B + C) + 2 * N * (B - A) + G (1)
 *
 * D (G (0)) = G (1) - G (0) = (A - 2B + C) + 2 * N * (B - A)
 * D (G (1)) = G (2) - G (1) = 3 * (A - 2B + C) + 2 * N * (B - A)
 * DD (G)   = D (G (1)) - D (G (0)) = 2 * (A - 2B + C)
 * Also, error = DD (G) / 8 .
 * Also, a subdivided DD = old DD/4.
 */

// int sc_DrawParabola (F26Dot6 Ax,F26Dot6 Ay,F26Dot6 Bx,F26Dot6 By,F26Dot6 Cx,F26Dot6 Cy,F26Dot6 **hX,F26Dot6 **hY,unsigned *count,int inGY)

int QSpline2PolylineNew(LPPOINT lpptBuffer, LPQS lpqsPoints, int inGY, unsigned int *count, int nAscent)
{
	int32 Ax, Ay, Bx, By, Cx, Cy;
	int32 GX, GY, DX, DY, DDX, DDY, nsqs;	/*F26Dot6 */
	int32 tmp, i, i2, ok;		/*F26Dot6 */
	QS qs;

//  start out by converting our 16.16 numbers to 26.6 numbers.  This can be
//  done safely since the numbers we are converting started out as 26.6
//  values at some point.

	if (inGY < 0)
	{
		Ax = lpqsPoints->ptfxStartPt.pt_x >> 10;
		Ay = lpqsPoints->ptfxStartPt.pt_y >> 10;
		Bx = lpqsPoints->ptfxCntlPt.pt_x >> 10;
		By = lpqsPoints->ptfxCntlPt.pt_y >> 10;
		Cx = lpqsPoints->ptfxEndPt.pt_x >> 10;
		Cy = lpqsPoints->ptfxEndPt.pt_y >> 10;
	}
	else
	{
		Ax = lpqsPoints->ptfxStartPt.pt_x;
		Ay = lpqsPoints->ptfxStartPt.pt_y;
		Bx = lpqsPoints->ptfxCntlPt.pt_x;
		By = lpqsPoints->ptfxCntlPt.pt_y;
		Cx = lpqsPoints->ptfxEndPt.pt_x;
		Cy = lpqsPoints->ptfxEndPt.pt_y;
	}

	/* Start calculating the first and 2nd order differences */
	GX = Bx;
	DDX = (DX = (Ax - GX)) - GX + Cx;	/* = alfa-x = half of ddx, DX = Ax - Bx */
	GY = By;					/* GY = By */
	DDY = (DY = (Ay - GY)) - GY + Cy;	/* = alfa-y = half of ddx, DY = Ay - By */
	/* The calculation is not finished but these intermediate results are useful */

	if (inGY < 0)
	{
		/* calculate amount of steps necessary = 1 << GY */
		/* calculate the error, GX and GY used a temporaries */
		GX = DDX < 0 ? -DDX : DDX;
		GY = DDY < 0 ? -DDY : DDY;
		/* approximate GX = sqrt (ddx * ddx + ddy * ddy) = Euclididan distance, DDX = ddx/2 here */
		GX += GX > GY ? GX + GY : GY + GY;	/* GX = 2*distance = error = GX/8 */

		/* error = GX/8, but since GY = 1 below, error = GX/8/4 = GX >> 5, => GX = error << 5 */

		for (GY = 1; GX > (PIXELSIZE << (5 - ERRSHIFT)); GX >>= 2)
		{	// GX = GX >> 2
			GY++;				/* GY used for temporary purposes */
		}

		/* Now GY contains the amount of subdivisions necessary, number of vectors == (1 << GY)*/
		if (GY > MAXMAXGY)
		{
			GY = MAXMAXGY;		/* Out of range => Set to maximum possible. */
		}

		i = 1 << GY;

		if ((*count = *count + (unsigned int) i) > MAXVECTORS)
		{
			/* Overflow, not enough space => return */
			return -1;
		}
	}
	else
	{
		GY = inGY;
		i = 1 << GY;
	}

	if (GY > MAXGY)
	{
		DDX = GY - 1;			/* DDX used as a temporary */
		/* Subdivide, this is nummerically stable. */
#define MIDX GX
#define MIDY GY
		qs.ptfxStartPt.pt_x = Ax;
		qs.ptfxStartPt.pt_y = Ay;
		qs.ptfxCntlPt.pt_x = ((long) Ax + Bx + 1) >> 1;
		qs.ptfxCntlPt.pt_y = ((long) Ay + By + 1) >> 1;
		qs.ptfxEndPt.pt_x = ((long) Ax + Bx + Bx + Cx + 2) >> 2;
		qs.ptfxEndPt.pt_y = ((long) Ay + By + By + Cy + 2) >> 2;
		i2 = QSpline2PolylineNew(lpptBuffer, (LPQS) & qs, (int) DDX, count, nAscent);

		if (i2 > 0)
			lpptBuffer += i2;

		qs.ptfxStartPt.pt_x = qs.ptfxEndPt.pt_x;
		qs.ptfxStartPt.pt_y = qs.ptfxEndPt.pt_y;
		qs.ptfxCntlPt.pt_x = ((long) Cx + Bx + 1) >> 1;
		qs.ptfxCntlPt.pt_y = ((long) Cy + By + 1) >> 1;
		qs.ptfxEndPt.pt_x = Cx;
		qs.ptfxEndPt.pt_y = Cy;
		QSpline2PolylineNew(lpptBuffer, (LPQS) & qs, (int) DDX, count, nAscent);
		return 0;
	}

	nsqs = GY + GY;				/* GY = n shift, nsqs = n*n shift */

	/* Finish calculations of 1st and 2nd order differences */
	DX = DDX - (DX << ++GY);	/* alfa + beta * n */
	DDX += DDX;
	DY = DDY - (DY << GY);
	DDY += DDY;

	GY = (long) Ay << nsqs;		/*  Ay * (n*n) */
	GX = (long) Ax << nsqs;		/*  Ax * (n*n) */
	/* GX and GY used for real now */

	/* OK, now we have the 1st and 2nd order differences,
	   so we go ahead and do the forward differencing loop. */
	tmp = 1L << (nsqs - 1);
	i2 = 0;

	do
	{
		GX += DX;				/* Add first order difference to x coordinate */
		lpptBuffer->x = (int) ((((GX + tmp) >> nsqs) + 0x3) >> 3);
		DX += DDX;				/* Add 2nd order difference to first order difference. */
		GY += DY;				/* Do the same thing for y. */

#if 1
		lpptBuffer->y = ((int) ((((GY + tmp) >> nsqs) + 0x3) >> 3));
#else
		lpptBuffer->y = nAscent - ((int) ((((GY + tmp) >> nsqs) + 0x3) >> 3));
#endif

		lpptBuffer->y >>= 3;
		lpptBuffer->x >>= 3;

		if (TotalCount == 53)
			ok = 1;

		if ((lpptBuffer->x == 145) && (lpptBuffer->y == 557))
			ok = 1;

		if ((lpptBuffer->x == 0) && (lpptBuffer->y == 0))
			ok = 1;

		DY += DDY;
		TotalCount++;
		lpptBuffer++;
		i2++;
	}
	while (--i);

	return i2;
}

#undef MIDX
#undef MIDY
