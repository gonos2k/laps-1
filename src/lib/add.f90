subroutine add(a, b, result, imax, jmax)
  implicit none
  integer, intent(in) :: imax, jmax
  real, intent(in) :: a(imax, jmax), b(imax, jmax)
  real, intent(out) :: result(imax, jmax)
  integer :: i, j

!$omp parallel do collapse(2) default(none) private(i,j) &
!$omp shared(imax,jmax,a,b,result)
  do j = 1, jmax
    do i = 1, imax
      result(i, j) = a(i, j) + b(i, j)
    end do
  end do
!$omp end parallel do
end subroutine add

subroutine add_miss(a, b, result, imax, jmax)
  implicit none
  integer, intent(in) :: imax, jmax
  real, intent(in) :: a(imax, jmax), b(imax, jmax)
  real, intent(out) :: result(imax, jmax)
  integer :: i, j, istatus
  real :: r_missing_data

  call get_r_missing_data(r_missing_data, istatus)

!$omp parallel do collapse(2) default(none) private(i,j) &
!$omp shared(imax,jmax,a,b,result,r_missing_data)
  do j = 1, jmax
    do i = 1, imax
      if (a(i, j) /= r_missing_data .and. b(i, j) /= r_missing_data) then
        result(i, j) = a(i, j) + b(i, j)
      else
        result(i, j) = r_missing_data
      endif
    end do
  end do
!$omp end parallel do
end subroutine add_miss
