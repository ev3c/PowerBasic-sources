'旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
'�   This  program demonstrates the recursive algorithm commonly             �
'�   known as "Ackerman."                                                    �
'�	In order to run this program do the following:			     �
'�	  1. Load PowerBASIC by typing PB at the DOS prompt. 		     �
'�	  2. Load the file ACKERMAN.BAS from the Load option of the File     �
'�	     pulldown menu.						     �
'�	  3. Compile and run by pressing F9				     �
'읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�

$STACK 32766
PRINT "Running Ackerman"
X# = TIMER
Ans% = FNAck%(3,6)                ' call the recursive function
XX# = TIMER
HowLong# = XX# - X#
PRINT USING "Ackerman complete in ##.###"; HowLong#
END

DEF FNAck%(M%,N%)		' Ackerman recursive function
  IF M% = 0 THEN
    FNAck% = N%+1
  ELSE
    IF N% = 0 then
      FNAck% = FNAck%(M%-1,1)
    ELSE
      FNAck% = FNAck%(M%-1,(FNAck%(M%,N%-1)))
    END IF
  END IF
END DEF
