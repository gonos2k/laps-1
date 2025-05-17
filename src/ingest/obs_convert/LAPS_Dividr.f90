SUBROUTINE LAPS_DIVIDER

!==============================================================================
!  THIS ROUTINE PRINTS A LINE DIVIDER TO FORMAT AN OUTPUT.
!
!  HISTORY:
!	CREATION:	YUANFU XIE	JUN 2007
!==============================================================================

  INTEGER, PARAMETER :: NCOUNT = 70
  ! Use REPEAT to avoid explicit looping
  WRITE(6,'(A)') REPEAT('=', NCOUNT)

END SUBROUTINE LAPS_DIVIDER
