module mpi_utils
  use mem_grid, ONLY: nPEs, rank, IstartIend, recvcounts, displs
contains
  subroutine mpi_initialize(istatus)
    include 'mpif.h'
    integer, intent(out) :: istatus
    integer :: ierr, istat_alloc
    call MPI_INIT(ierr)
    if (ierr /= 0) then
       istatus = ierr
       return
    end if
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= 0) then
       istatus = ierr
       return
    end if
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nPEs, ierr)
    if (ierr /= 0) then
       istatus = ierr
       return
    end if
    if(.not.allocated(IstartIend)) allocate(IstartIend(2,0:nPEs-1),stat=istat_alloc)
    if(istat_alloc /= 0) then
       istatus = istat_alloc
       return
    end if
    if(.not.allocated(recvcounts)) allocate(recvcounts(0:nPEs-1),stat=istat_alloc)
    if(istat_alloc /= 0) then
       istatus = istat_alloc
       return
    end if
    if(.not.allocated(displs)) allocate(displs(0:nPEs-1),stat=istat_alloc)
    if(istat_alloc /= 0) then
       istatus = istat_alloc
       return
    end if
    istatus = 0
  end subroutine mpi_initialize

  subroutine mpi_finalize()
    include 'mpif.h'
    integer :: ierr
    call MPI_FINALIZE(ierr)
  end subroutine mpi_finalize

  subroutine setup_mpi_decomposition(nx, ny, nz, n_var, istatus)
    use mem_grid, ONLY: nPEs, rank, IstartIend, recvcounts, displs
    include 'mpif.h'
    integer, intent(in) :: nx, ny, nz, n_var
    integer, intent(out) :: istatus
    integer :: ierr
    integer :: PointsPerPE, extra, adjust, n
    integer :: istat_alloc

    if(.not.allocated(IstartIend)) allocate(IstartIend(2,0:nPEs-1),stat=istat_alloc)
    if(istat_alloc /= 0) then
       istatus = istat_alloc
       return
    endif
    if(.not.allocated(recvcounts)) allocate(recvcounts(0:nPEs-1),stat=istat_alloc)
    if(istat_alloc /= 0) then
       istatus = istat_alloc
       return
    endif
    if(.not.allocated(displs)) allocate(displs(0:nPEs-1),stat=istat_alloc)
    if(istat_alloc /= 0) then
       istatus = istat_alloc
       return
    endif

    if(rank == 0) then
       PointsPerPE = nx / nPEs
       if(PointsPerPE < 1) then
          istatus = -1
          return
       endif
       extra = nx - PointsPerPE*nPEs
       if(extra > 0) then
          adjust = 1
          extra = extra - 1
       else
          adjust = 0
       endif
       IstartIend(1,0) = 1
       IstartIend(2,0) = PointsPerPE + adjust
       do n=1,nPEs-1
          if(extra > 0) then
             adjust = 1
             extra  = extra - 1
          else
             adjust = 0
          endif
          IstartIend(1,n) = IstartIend(2,n-1) + 1
          IstartIend(2,n) = IstartIend(1,n) + PointsPerPE - 1 + adjust
       enddo
       do n=0,nPEs-1
          recvcounts(n)=(IstartIend(2,n)-IstartIend(1,n)+1)*ny*nz*n_var
       enddo
       displs(0) = 0
       do n=1,nPEs-1
          displs(n) = displs(n-1) + recvcounts(n-1)
       enddo
    endif

    call mpi_bcast(IstartIend,2*nPEs,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    if(ierr /= 0) then
       istatus = ierr
       return
    endif
    call mpi_bcast(displs,nPEs,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    if(ierr /= 0) then
       istatus = ierr
       return
    endif
    call mpi_bcast(recvcounts,nPEs,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    if(ierr /= 0) then
       istatus = ierr
       return
    endif
    istatus = 0
  end subroutine setup_mpi_decomposition
end module mpi_utils
