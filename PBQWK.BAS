DEFINT A-Z
' change every occurance of "print" to "print #2," to write to file
CLS




Text = STRING * 128

ifile% = FREEFILE
ifile$ = "c:\tm\dl\messages.dat"

open ifile$ for random as ifile% len = 128

Records& = LOF(1) / 128

GET$ #1,128 , CopyWright$

ont# = TIMER

DO
  x% = x% + 1

  get #1

  get$ #1,128, mheat$


'  get$ #1, 1, mPrivate$
'  get$ #1, 7, mMessage$
'  get$ #1, 8, mDate$
'  get$ #1, 5, mTime$
'  get$ #1,25, mTo$
'  get$ #1,25, mFrom$
'  get$ #1,25, mSubject$
'  get$ #1,12, mPassword$
'  get$ #1, 8, mMsgRef$
'  get$ #1, 6, mNumBlks$
'  get$ #1, 6, mMsgSts$


  LOCATE 10, 10: PRINT x%;
LOOP UNTIL EOF(1)


oft# = TIMER

PRINT
PRINT oft# - ont#

END:






