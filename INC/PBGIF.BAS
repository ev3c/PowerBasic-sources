REM GIF read/display in QB (a hack of GIFSLOW.PAS)
REM 01-Apr-1991 v1.00
REM Cornel Huth
REM -------------------------------------------------------------------------
REM This is fine to look at but not to actually use in a program. BUT -
REM QBXGRAF v2.0 is a complete assembly language graphics library enhance-
REM ment to QuickBASIC+PDS EGA/VGA graphics. Fast GIF read with configurable
REM display is included as are patterned lines and fills, fast arcade-
REM like bitblt animation, complete 2-D and 3-D transformation routines
REM so fast that it'll make your head spin! 800x600x16, EMS/XMS support.
REM Lots more. Coming soon to a shareware outlet near you. Look for it.
REM -------------------------------------------------------------------------

DEFINT A-Z
%true = -1
%false = 0
%RasterSize = 32000                'BASIC and its lack of unsigned
                                        'integers means we kludge (a lot).
dim Prefix(0 : 4095) AS INTEGER
dim Suffix(0 : 4095) AS INTEGER

dim OutCode(0 : 1024) AS INTEGER

dim palet(0 : 255) AS LONG

dim MaxCodes(0 : 9) AS INTEGER
dim Masks(0 : 9) AS INTEGER

dim Raster(0 : %RasterSize) AS STRING * 1
dim GIFstuff(0 : %RasterSize) AS STRING * 1


CLS
ReInitialize

videomode = 13  'PORSCHE.GIF is 320x200x32 so use mode 13
filename$ = "porsche.gif"
PRINT "Reading file: "; filename$; "...";

OPEN filename$ FOR BINARY AS #1
fsize& = LOF(1)
IF fsize& > %RasterSize THEN fsize& = %RasterSize
bytes = fsize&

ReadFile 1, bytes
CLOSE #1
PRINT "done."
PRINT

FileString$ = ""
FOR i = 1 TO 6
   FileString$ = FileString$ + CHR$(GetByte)
NEXT
IF FileString$ <> "GIF87a" THEN PRINT "Not a GIF": STOP

RWidth = GetWord
RHeight = GetWord
B = GetByte
IF B AND &H80 = &H80 THEN ColorMap = %TRUE ELSE ColorMap = %FALSE
Resolution = (B AND &H70) \ 16 + 1
BitsPerPixel = (B AND 7) + 1
ColorMapSize = 1 * (2 ^ BitsPerPixel)
BitMask = ColorMapSize - 1
BackGround = GetByte
B = GetByte
PRINT "Color Rez:"; Resolution;
PRINT " Number of Colors:"; ColorMapSize;
PRINT " BG:"; BackGround; "  COLOR MAP follows:": PRINT

IF ColorMap THEN
   FOR i = 0 TO ColorMapSize - 1
      Red = GetByte
      Green = GetByte
      Blue = GetByte
      IF videomode < 10 THEN
         DetColor R, Red
         DetColor G, Green
         DetColor B, Blue
        pal = (B AND 1) + (2 * (G AND 1)) + (4 * (R AND 1)) + (8 * (B \ 2)) + ( palet(i) = pal
        PRINT i; HEX$(Red); ","; HEX$(Green); ","; HEX$(Blue); "->"; HEX$(R); ""
      ELSE
         DetColor13 R, Red
         DetColor13 G, Green
         DetColor13 B, Blue
         pal& = 65536 * B + 256 * G + R
         palet(i) = pal&
        PRINT i; HEX$(Red); ","; HEX$(Green); ","; HEX$(Blue); "->"; HEX$(R); ""
      END IF
   NEXT
   palet(0) = BackGround
END IF
PRINT

B = GetByte
LeftOfs = GetWord
TopOfs = GetWord
Wide = GetWord
Height = GetWord
A = GetByte
IF A AND &H40 = &H40 THEN Interlace = %TRUE ELSE Interlace = %FALSE
PRINT
PRINT "Left/top offset:"; LeftOfs; ","; TopOfs
PRINT "Width x height/interlace:"; Wide; "x"; Height; ","; Interlace

CodeSize = GetByte
ClearCode = 2 ^ CodeSize
EOFCode = ClearCode + 1
FirstFree = ClearCode + 2
FreeCode = FirstFree
PRINT
PRINT "Clear code/EOF code:"; ClearCode; ","; EOFCode

REM $STATIC
SUB AddToPixel (index)

IF videomode <> 13 THEN index = index AND 15
PSET (XC, YC), index

XC = XC + 1
IF XC = Wide THEN
   XC = 0
   IF NOT Interlace THEN
      YC = YC + 1
   ELSE
      SELECT CASE Pass
      CASE 0
         YC = YC + 8
         IF YC >= Height THEN
            Pass = Pass + 1
            YC = 4
         END IF
      CASE 1
         YC = YC + 8
         IF YC >= Height THEN
            Pass = Pass + 1
            YC = 2
         END IF
      CASE 2
         YC = YC + 4
         IF YC >= Height THEN
            Pass = Pass + 1
            YC = 1
         END IF
      CASE 3
         YC = YC + 2
      CASE ELSE
      END SELECT
   END IF
END IF

END SUB

SUB DetColor (pvalue, mapvalue)

pvalue = mapvalue \ 64 '(2 bits per color component (R/G/B) on EGA/VGA)
IF pvalue = 1 THEN      'switch for some reason...
   pvalue = 2
ELSEIF pvalue = 2 THEN
   pvalue = 1
END IF

END SUB

SUB DetColor13 (pvalue, mapvalue)

pvalue = mapvalue \ 4 '(6 bits per color component (R/G/B) on VGA13)

END SUB

SUB DoClear

CodeSize = InitCodeSize
MaxCode = MaxCodes(CodeSize - 2)
FreeCode = FirstFree
ReadMask = Masks(CodeSize - 3)

END SUB

FUNCTION GetByte

IF GIFptr > %RasterSize THEN
   PRINT "Maxed-out raster buffer."
   STOP
END IF
GetByte = ASC(GIFstuff(GIFptr))
GIFptr = GIFptr + 1

END FUNCTION

FUNCTION GetWord

A = GetByte
B = GetByte
GetWord = 256 * B + A

END FUNCTION

SUB ReadCode

ByteOffset = BitOffset \ 8

A& = (256& * ASC(Raster(ByteOffset + 1))) + ASC(Raster(ByteOffset))
IF CodeSize > 8 THEN
   B = ASC(Raster(ByteOffset + 2))
   Rawcode& = 65536 * B + A&
ELSE
   Rawcode& = A&
END IF

Rawcode& = Rawcode& \ (2 ^ (BitOffset MOD 8))
Code = Rawcode& AND ReadMask

BitOffset = BitOffset + CodeSize

END SUB

SUB ReadFile (handle, bytes)

FOR i = 0 TO (bytes - 1)
   GET #handle, , char
   GIFstuff(i) = char
NEXT


END SUB

SUB ReadRaster

RasterPtr = 0
DO
   BlockLength = GetByte
   FOR i = 0 TO BlockLength - 1
      IF GIFptr = %RasterSize THEN
         PRINT "File size exceeds program limits"
         STOP
      END IF

      Raster(RasterPtr) = CHR$(GetByte)
      LOCATE , 1: PRINT RasterPtr;

      RasterPtr = RasterPtr + 1
      IF RasterPtr > %RasterSize + 1 THEN
         PRINT "Raster fill maxed-out"
         STOP
      END IF

   NEXT
LOOP UNTIL BlockLength = 0

ERASE GIFstuff

END SUB

SUB ReInitialize

XC = 0
YC = 0
Pass = 0
BitOffset = 0
GIFptr = 0

MaxCodes(0) = 4
MaxCodes(1) = 8
MaxCodes(2) = 16
MaxCodes(3) = 32
MaxCodes(4) = 64
MaxCodes(5) = 128
MaxCodes(6) = 256
MaxCodes(7) = 512
MaxCodes(8) = 1024
MaxCodes(9) = 2048

Masks(0) = 7
Masks(1) = 15
Masks(2) = 31
Masks(3) = 63
Masks(4) = 127
Masks(5) = 255
Masks(6) = 511
Masks(7) = 1023
Masks(8) = 2047
Masks(9) = 4095

END SUB

