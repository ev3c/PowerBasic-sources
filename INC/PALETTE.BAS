DEFINT A-Z
RANDOMIZE TIMER
SCREEN 12
DIM Pal%(260)             'The data type "&" is of primary importance


FOR i = 0 TO 15          'make a random palette.
    red = INT(RND * 64)
    green = INT(RND * 64)
    blue = INT(RND * 64)
    newcolor& = red + 256 * green + 65536 * blue
    PALETTE i, newcolor&                    ' Here's one using PALETTE
    COLOR i
    LINE (i, 0)-(i, 200)
NEXT i




' This is the loop that changes the palette each and every time a key
' is pressed. It's makes the "PALETTE USING" command clear enough.

DO
    FOR i = 1 TO 255
        red = INT(RND * 64)
        green = INT(RND * 64)
        blue = INT(RND * 64)
        newcolor& = red + 256 * green + 65536 * blue
	Pal%(i) = newcolor&
    NEXT i
    PALETTE USING Pal%(i)                   ' And one PALETTE USING
    DO
      A$ = INKEY$
    LOOP WHILE A$ = ""
    IF A$ = "x" THEN SCREEN 0: END
LOOP
END

