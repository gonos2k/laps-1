subroutine addcon(a, con, result, imax, jmax)
  implicit none
  integer, intent(in) :: imax, jmax
  real, intent(in) :: a(imax,jmax), con
  real, intent(out) :: result(imax,jmax)
  integer :: i, j
  do j=1, jmax
     do i=1, imax
        result(i,j) = a(i,j) + con
     end do
  end do
end subroutine addcon

subroutine addcon_miss(a, con, result, imax, jmax)
  implicit none
  integer, intent(in) :: imax, jmax
  real, intent(in) :: a(imax,jmax), con
  real, intent(out) :: result(imax,jmax)
  real :: r_missing_data
  integer :: istatus, i, j
  call get_r_missing_data(r_missing_data, istatus)
  do j=1, jmax
     do i=1, imax
        if(a(i,j) /= r_missing_data .and. con /= r_missing_data) then
           result(i,j) = a(i,j) + con
        else
           result(i,j) = r_missing_data
        end if
     end do
  end do
end subroutine addcon_miss
