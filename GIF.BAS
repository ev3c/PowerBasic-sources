
CodeSize = CodeSize + 1
InitCodeSize = CodeSize
MaxCode = MaxCodes(CodeSize - 2)
ReadMask = Masks(CodeSize - 3)
PRINT
PRINT "Unblocking"

ReadRaster

OutCount = 0

PRINT "  Press a key and wait (a while)..."
DO: LOOP WHILE INKEY$ = ""

'GIFREAD works with any graphics mode provided enough room
'PALETTE USING only for EGA/VGA

SCREEN videomode
IF ColorMap AND videomode > 8 THEN PALETTE USING palet(0)

LOCATE 1, 1: PRINT "Hold"
PRINT "on..."
LOCATE 25, 1: PRINT "This might take a while...";
DO
   ReadCode
   IF Code <> EOFCode THEN
      IF Code = ClearCode THEN
         DoClear
         ReadCode
         CurCode = Code
         OldCode = Code
         FinChar = Code AND BitMask
         AddToPixel FinChar
      ELSE
         CurCode = Code
         InCode = Code
         IF Code >= FreeCode THEN
            CurCode = OldCode
            OutCode(OutCount) = FinChar
            OutCount = OutCount + 1
         END IF
         IF CurCode > BitMask THEN
            DO
               OutCode(OutCount) = Suffix(CurCode)
               OutCount = OutCount + 1
               CurCode = Prefix(CurCode)
            LOOP UNTIL CurCode <= BitMask
         END IF

         FinChar = CurCode AND BitMask
         OutCode(OutCount) = FinChar
         OutCount = OutCount + 1

         FOR i = OutCount - 1 TO 0 STEP -1
            AddToPixel OutCode(i)
         NEXT
         OutCount = 0

         Prefix(FreeCode) = OldCode
         Suffix(FreeCode) = FinChar
         OldCode = InCode

         FreeCode = FreeCode + 1
         IF FreeCode >= MaxCode THEN
            IF CodeSize < 12 THEN
               CodeSize = CodeSize + 1
               MaxCode = MaxCode * 2
               ReadMask = Masks(CodeSize - 3)
            END IF
         END IF
      END IF

      in$ = INKEY$
      IF in$ <> "" THEN Code = EOFCode
   END IF
LOOP UNTIL Code = EOFCode
BEEP
SLEEP
SCREEN 0
SYSTEM

