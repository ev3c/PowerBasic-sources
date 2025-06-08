    CALL BTRV (OP%, STATUS%, POSITION$, FILE$, DATALEN%,
		      KEYBUF$, KEYNUM%)   where

      OP%       - function number for request
      STATUS%   - return status from request
      POSITION$ - 128 byte data area for FCB and positioning,
		  not to be changed by user
      FILE$     - address of data buffer from FIELD function
      DATALEN%  - length of user data buffer
      KEYBUF$   - string pointer to key buffer
      KEYNUM%   - key number to be processed
