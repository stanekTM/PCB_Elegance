
#ifndef _LICENSE_BASIC

#define _LICENSE_BASIC

#include "owntypes.h"
#include "windows.h"

#define CRC_PASSWORD       "r;h;4ge5]glvew[v"
#define STANDARD_KEY       123456789

#ifdef VC80
#define FILE_POS1  0x12A4
#define FILE_POS2  0x3828
#define FILE_POS3  0x5ED0
#define FILE_ADD   0
#else
#ifdef GCC_COMP
#define FILE_POS1  0x16A4
#define FILE_POS2  0x3C28
#define FILE_POS3  0x7E50
#define FILE_ADD   3*1024
#else
#define FILE_POS1  0x118C
#define FILE_POS2  0x32D1
#define FILE_POS3  0x0
#define FILE_ADD   0
#endif
#endif

// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

#ifdef _DEBUG

#define SPECIALCRCCHECK(mode)

#else

#define SPECIALCRCCHECK(mode)                                                          \
  {                                                                                    \
    int32   CRCCalculateCount,fp,result;                                               \
    int32   NrCrcBytes=0;                                                              \
    int32   ProgramSpecialLength2;                                                     \
    uint8    CRCAddValue;                                                              \
    uint32  CRCCalculateCRC=0xffffffff;                                                \
/*    int32   CRCExcludePos1=-1;    */                                                 \
/*    int32   CRCExcludePos2=-1;    */                                                 \
/*    int32   CRCExcludePos1=0x10b6; */                                                \
/*    int32   CRCExcludePos2=0x1664; */                                                \
    int32   CRCExcludePos1=FILE_POS1;                                                  \
    int32   CRCExcludePos2=FILE_POS2;                                                  \
    int32   CRCCalculateStartPos=FILE_POS3;                                            \
    uint8    *CRCCalculatePos;                                                         \
                                                                                       \
                                                                                       \
/*                                                                                     \
    CRCfp=FileOpenWrite("c:\\pcb\\program_key.bin");                                   \
                                                                                       \
    CRCCalculatePos=(uint8 *)(0x400000);                                               \
    FileWrite(CRCfp,CRCCalculatePos,32768,&CRCCalculateCount);                         \
    FileClose(CRCfp);                                                                  \
*/                                                                                     \
    CRCCalculatePos=(uint8 *)(0x400000+CRCCalculateStartPos);                          \
    ProgramSpecialLength2=ProgramSpecialLength+FILE_ADD;                               \
/*    MessageBox(NULL,"Check special crc","Message",MB_APPLMODAL|MB_OK);   */          \
    for (CRCCalculateCount=CRCCalculateStartPos;                                       \
         CRCCalculateCount<ProgramSpecialLength2;                                      \
         CRCCalculateCount++) {                                                        \
      CRCAddValue=*CRCCalculatePos;                                                    \
      if (CRCCalculateCount==100001) {                                                 \
/*        Value=0x10;   */                                                             \
      }                                                                                \
      if (((CRCCalculateCount<ProgramSpecialCRCPosition+5)                             \
         ||                                                                            \
         (CRCCalculateCount>ProgramSpecialCRCPosition+15))                             \
         &&                                                                            \
         ((CRCCalculateCount<ProgramSpecialCRCPosition2)                               \
         ||                                                                            \
         (CRCCalculateCount>ProgramSpecialCRCPosition2+20))) {                         \
        CRCCalculateCRC = (CRCCalculateCRC >> 8) ^                                     \
                          crc32_table[(CRCCalculateCRC & 0xFF) ^ CRCAddValue];         \
        NrCrcBytes++;                                                                  \
      }                                                                                \
      CRCCalculatePos++;                                                               \
    }                                                                                  \
/*                                                                                     \
    sprintf(str,"Program CRC = 0x%08x, calculated CRC = 0x%08x  file %s  line %d\r\nCrc start,length (count) = %d,%d,%d",     \
            ProgramSpecialCRC,CRCCalculateCRC,__FILE__,__LINE__,CRCCalculateStartPos,ProgramSpecialLength2,NrCrcBytes);       \
    MessageBox(NULL,str,"",MB_APPLMODAL|MB_OK);                                        \
*/                                                                                     \
                                                                                       \
    if (CRCCalculateCRC!=ProgramSpecialCRC) {                                          \
      CRCCalculatePos=(uint8 *)(0x400000);                                             \
/*                                                                                     \
      fp=FileOpenWrite("e:\\mingw\\projects\\viewplot20\\viewplot_saved.exe");         \
      FileWrite(fp,CRCCalculatePos,ProgramSpecialLength,&result);                      \
      FileClose(fp);                                                                   \
*/                                                                                     \
      SpecialExit();                                                                   \
    }                                                                                  \
  }
#endif


// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************

extern uint32 ProgramSpecialCRC;
extern int32 ProgramSpecialCRCPosition;
extern int32 ProgramSpecialCRCPosition2;
extern int32 ProgramSpecialLength;
extern uint32 crc32_table[256];

void Blowfish_encipher(uint32 * xl, uint32 * xr);

void Blowfish_decipher(uint32 * xl, uint32 * xr);

int32 InitializeBlowfish(uint8 * Key);

int32 EncryptBlock(uint8 * Block, int32 BlockSize);

int32 DecryptBlock(uint8 * Block, int32 BlockSize);

int32 CheckCRC(LPSTR FileName, int32 * FileCheck1);

int32 CheckCRC2(LPSTR FileName);

int32 GetCRC2(void);

void ScrambleMessage(LPSTR SrcString, LPSTR DestVar);

void DeScrambleMessage(uint8 * Src, uint8 * Dest);

void SpecialExit(void);

uint32 CalcCrcFile(LPSTR Filename);

int32 ScrambleZipFile(LPSTR FileName, LPSTR NewFile, LPSTR PassWord);

#endif
