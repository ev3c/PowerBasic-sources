'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
'³   This program demonstrates several new features of PowerBASIC by         ³
'³   sorting the contents of a PowerBASIC-produced .MAP file.                ³
'³	In order to run this program do the following:			     ³
'³	  1. Load PowerBASIC by typing PB at the DOS prompt. 		     ³
'³	  2. Load the file MAPSORT.BAS from the Load option of the File      ³
'³	     pulldown menu.						     ³
'³	  3. Compile and run by pressing F9				     ³
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

ON ERROR GOTO ErrorHandler

CLS
INPUT "Enter the name of the .MAP file to be sorted: ", F$
IF INSTR(F$,".")=0 THEN F$=F$+".MAP"  'append .MAP if no extension given
INPUT "Enter the name to give the sorted file (include extension): ", SF$

DO
  PRINT "Sort ";UCASE$(F$);" by (A)ddress or (S)ymbol name (A/S)";
  INPUT SortType$
  SortType$ = UCASE$(SortType$)
LOOP UNTIL (SortType$ = "A") OR (SortType$ = "S")

OPEN F$ FOR BINARY AS #1
IF LOF(1) = 0 THEN   'non-existent file
  BEEP
  PRINT "File ";F$;" contains no information!"
  CLOSE
  END
END IF

'first count the number of map entries

LineCount& = 0   'keeps track of the number of entries in the file
LineFeed$ = CHR$(10)   'linefeed appears at the end of each file entry
DO
  BytesLeft& = LOF(1)-LOC(1)
  IF BytesLeft& > 0 THEN
    IF BytesLeft& > 16384 THEN
      GET$ 1, 16384, ReadString$       'get the first 16K into a string
    ELSE
      GET$ 1, BytesLeft&, ReadString$  'otherwise get what's left
    END IF
    INCR LineCount&, TALLY(ReadString$, LineFeed$)
  END IF
LOOP UNTIL BytesLeft& <= 0

CLOSE 1  'close the file
IF LineCount& > 32767 THEN  'check the line count
  BEEP
  PRINT "Sorry, your .MAP file has too many lines for this program to handle."
  END
END IF

OPEN F$ FOR INPUT AS #1    'reopen it as a sequential file

DIM MAPData$(1:LineCount&)  'load into memory
FOR ALine& = 1 TO LineCount&
  LINE INPUT #1, MAPData$(ALine&)
NEXT ALine&

CLOSE 1

IF SortType$ = "A" THEN    'numeric sort by address
  ARRAY SORT MAPData$(), FROM 1 TO 9
ELSE                       'alphabetic sort by symbol name
  ARRAY SORT MAPData$(), FROM 17 TO 255
END IF

OPEN SF$ FOR OUTPUT AS #1     'write the sorted file
FOR ALine& = 1 to LineCount&
  PRINT #1, MAPData$(ALine&)
NEXT ALine&
CLOSE 1
END

ErrorHandler:
PRINT "The program caused an Error";err;"!"
PRINT "Press any key to continue..."
WHILE NOT INSTAT  'wait for a key
WEND
ON ERROR GOTO 0   'let the error have its way

'end of program
