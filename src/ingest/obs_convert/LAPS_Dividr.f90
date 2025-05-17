SUBROUTINE LAPS_DIVIDER

!==============================================================================
!  THIS ROUTINE PRINTS A LINE DIVIDER TO FORMAT AN OUTPUT.
!
!  HISTORY:
!	CREATION:	YUANFU XIE	JUN 2007
!==============================================================================

  CHARACTER(LEN=*), PARAMETER :: SYMBOL = '='
  INTEGER,          PARAMETER :: NCOUNT = 70

  WRITE(6,'(A)') REPEAT(SYMBOL,NCOUNT)

END SUBROUTINE LAPS_DIVIDER
