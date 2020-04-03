
#include "windows.h"
#include "types.h"
#include "memory.h"
#include "pcb.h"

int32 NrBitMaps;
HBITMAP BitMaps[16];



void LoadBitMaps()
{

	BitMaps[0] = LoadBitmap(PCBClass.hInstance, "BITMAP1");
	BitMaps[1] = LoadBitmap(PCBClass.hInstance, "BITMAP2");
	NrBitMaps = 2;


}


void DestroyBitMaps()
{
	int32 cnt;

	for (cnt = 0; cnt < NrBitMaps; cnt++)
		DeleteObject(BitMaps[cnt]);


}
