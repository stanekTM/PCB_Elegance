
#include "stdio.h"
#include "zlib.h"
#include "own_zlib.h"
#include "intrin.h"
#include "memory.h"

#define CHUNK 16384

#if 0

uint32 width = 72;

typedef struct
{
	uint32 tuple;
	uint32 pos;
	int count;
} enc85_data;


3 2(b1 x 256) + (b2 x 256) + (b3 x 256) + b4 =
    4 3 2(c1 x 85) + (c2 x 85) + (c3 x 85) + (c4 x 85) + c5 Specifically,

    ASCII base - 85 encoding produces 5 ASCII characters for every
    4 bytes of binary data.Each group of 4 binary input bytes, (b1 b2 b3 b4), is converted to a group of 5 output bytes,
	    (c1 c2 c3 c4 c5), using the relation In other words,
	    4 bytes of binary data are interpreted as a base - 256 number and then converted into a base -
	    85 number.The five “ digits ” of the base -
	    85 number are then converted to ASCII characters by adding 33(the ASCII code for the character !)
		    to each.

		    The resulting encoded data contains only printable ASCII characters with codes in the range 33(!) to
		    117(u).As a special case, if all five digits are 0, they are represented by the character with code 122(z)
			    instead of by five exclamation points(!!!!!).
			    If the length of the binary data to be encoded is not a multiple of 4 bytes, the last,
			    partial group of 4 is used to produce a last, partial group of 5 output characters.Given n(1, 2, or 3)
			    bytes of binary data,
			    the encoder first appends 4 -
			    n zero bytes to make a complete group of 4. It then encodes this group in the usual way,
			    but without applying the special z case.Finally,
			    it writes only the first n +
			    1 characters of the resulting group of 5. These characters are immediately followed by the ~ >EOD marker.
			    static void enc85(enc85_data * d)
		{
			int i;
			char buf[5], *s = buf;
			i = 5;

			do
			{
				*s++ = d->tuple % 85;
				d->tuple /= 85;
			}
			while (--i > 0);

			i = d->count;

			do
			{
				--s;
				putc(*s + '!', d->fp);

				if (d->pos++ >= width)
				{
					d->pos = 0;
					putc('\n', d->fp);
				}
			}
			while (i-- > 0);
		}

static void put85(unsigned c, enc85_data * d)
{
	switch (d->count++)
	{
	case 0:
		d->tuple |= (c << 24);
		break;

	case 1:
		d->tuple |= (c << 16);
		break;

	case 2:
		d->tuple |= (c << 8);
		break;

	case 3:
		d->tuple |= c;

		if (d->tuple == 0)
		{
			putc('z', d->fp);

			if (d->pos++ >= width)
			{
				d->pos = 0;
				putc('\n', d->fp);
			}
		}
		else
			enc85(d);

		d->tuple = 0;
		d->count = 0;
		break;
	}
}


#endif

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 zlib_inflate_ascii85(char *Src, int32 SrcLength)
{
	int32 cnt, err, BytesLeft, BytesCompressed, SrcPos, BytesWritten, DestPos, Pos1, SpecialPos, IntsToEncode;
	uint8 c1, c2, c3, c4, c5;
	uint32 value;
	char *Dest;
	z_stream d_stream;			/* decompression stream */
	unsigned char out[CHUNK + 16];

	BytesWritten = 0;
	Dest = TempMem;
	AllocateMemTemp(SrcLength);

	SrcPos = 0;
	DestPos = 0;
	SpecialPos = 0;
	BytesLeft = SrcLength;
	d_stream.zalloc = (alloc_func) 0;
	d_stream.zfree = (free_func) 0;
	d_stream.opaque = (voidpf) 0;

	d_stream.avail_out = CHUNK;

	err = deflateInit(&d_stream, Z_SYNC_FLUSH);
//  CHECK_ERR(err, "inflateInit");

	do
	{
		d_stream.next_in = &Src[SrcPos];
		d_stream.next_out = &out[SpecialPos];
		d_stream.avail_in = min(BytesLeft, CHUNK);
		BytesLeft -= d_stream.avail_in;
		SrcPos += d_stream.avail_in;
//    d_stream.next_out = TempBuf;
		err = deflate(&d_stream, Z_NO_FLUSH);

		if (err == Z_STREAM_END)
			break;

		BytesCompressed = CHUNK - d_stream.avail_out + SpecialPos;
		IntsToEncode = BytesCompressed / 4;
		Pos1 = 0;

		for (cnt = 0; cnt < IntsToEncode; cnt++)
		{
			value = _byteswap_ulong(*(uint32 *) & out[Pos1]);

			if ((value) || ((BytesLeft == 0) && (BytesCompressed & 3)))
			{
				c1 = (uint8) ((value % 85) + '!');
				value /= 85;
				c2 = (uint8) ((value % 85) + '!');
				value /= 85;
				c3 = (uint8) ((value % 85) + '!');
				value /= 85;
				c4 = (uint8) ((value % 85) + '!');
				value /= 85;
				c5 = (uint8) ((value % 85) + '!');

				if ((BytesLeft == 0) && (BytesCompressed & 3))
				{
					switch (BytesCompressed & 3)
					{
					case 1:
						*(Dest++) = c5;
						*(Dest++) = c4;
						BytesWritten += 2;
						break;

					case 2:
						*(Dest++) = c5;
						*(Dest++) = c4;
						*(Dest++) = c3;
						BytesWritten += 3;
						break;

					case 3:
						*(Dest++) = c5;
						*(Dest++) = c4;
						*(Dest++) = c3;
						*(Dest++) = c2;
						BytesWritten += 4;
						break;
					}
				}
				else
				{
					*(Dest++) = c5;
					*(Dest++) = c4;
					*(Dest++) = c3;
					*(Dest++) = c2;
					*(Dest++) = c1;
					BytesWritten += 5;
				}
			}
			else
			{
				*(Dest++) = 'z';
				BytesWritten++;
			}

			Pos1 += 4;
		}

		if ((BytesLeft > 0) && (BytesCompressed & 3))
		{
			memcpy(&out[0], &out[IntsToEncode * 4], (BytesCompressed & 3));
			SpecialPos = BytesCompressed & 3;
		}

//      CHECK_ERR(err, "inflate");
	}
	while (BytesLeft);

	err = inflateEnd(&d_stream);

//  AllocateMemTemp2(1024*1024);
	*(Dest++) = '~';
	*(Dest++) = '>';
	BytesWritten += 2;
	return BytesWritten;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************

int32 zlib_inflate(char *Src, int32 SrcLength)
{
	int32 err, BytesLeft, SrcPos, BytesWritten;
	char *Dest;
	z_stream d_stream;			/* decompression stream */

	BytesWritten = 0;
	AllocateMemTemp(SrcLength + 1024);
	Dest = TempMem;

	SrcPos = 0;
	BytesLeft = SrcLength;
	d_stream.zalloc = (alloc_func) 0;
	d_stream.zfree = (free_func) 0;
	d_stream.opaque = (voidpf) 0;

	d_stream.avail_out = SrcLength + 1024;
	d_stream.next_out = TempMem;

	err = deflateInit(&d_stream, Z_SYNC_FLUSH);

	if (err)
		return 0;

#if 1
	d_stream.next_in = Src;
	d_stream.avail_in = BytesLeft;
	err = deflate(&d_stream, Z_FINISH);

	if ((err != Z_STREAM_END) && (err))
	{
		err = inflateEnd(&d_stream);
		return 0;
	}

	BytesWritten = SrcLength + 1024 - d_stream.avail_out;
#else

	do
	{
		d_stream.next_in = &Src[SrcPos];
		d_stream.avail_in = min(BytesLeft, CHUNK);
		BytesLeft -= d_stream.avail_in;
		SrcPos += d_stream.avail_in;
		err = deflate(&d_stream, Z_SYNC_FLUSH);

		if (err == Z_STREAM_END)
			break;

		if (err)
			return 0;

		BytesCompressed = CHUNK - d_stream.avail_out;
		memcpy(&Dest[BytesWritten], out, BytesCompressed);
		BytesWritten += BytesCompressed;
	}
	while (BytesLeft);

#endif
	err = inflateEnd(&d_stream);

	if (err)
		return 0;

	return BytesWritten;
}

// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
// ****************************************************************************************************
