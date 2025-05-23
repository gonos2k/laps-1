!*****************************************************************************!
! Subroutine RD_GRIB2                                                         !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB2 file.  Based on the information in  !
!    the GRIB2 header and the user-defined Vtable, decide whether the field in!
!    the GRIB2 record is one to process or to skip.  If the field is one we   !
!    want to keep, extract the data from the GRIB2 record, and store the data !
!    in the degrib memory structure.                                          !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       junit   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we do not do Fortran I/O for the GRIB2   !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It is really just an array index to the    !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       gribflnm     : File name to open, if it is not already open.          !
!       debug_level  : Integer for various amounts of printout.               !
!                                                                             !
!    Output:                                                                  !
!                                                                             !
!       hdate        : The (up to)19-character date of the field to process.  !
!       grib_edition : Version of the gribfile (1 or 2)                       !
!       ireaderr     : Error flag: 0 - no error on read from GRIB2 file.      !
!                              1 - Hit the end of the GRIB2 file.             !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
!                                                                             !
!                                                                             !
! Author: Paula McCaslin, NOAA/FSL,   Sept 2004                               !
! Code is based on code developed by Steve Gilbert NCEP & Kevin Manning NCAR  !
! Adapted for WPS: NCAR/MMM. Sept 2006                                        !
!*****************************************************************************!

      SUBROUTINE rd_grib2(junit, gribflnm, hdate, &
       grib_edition, ireaderr, debug_level)

      use grib_mod
      use params
      use table          ! Included to define g2code
      use gridinfo       ! Included to define map%
      use storage_module ! Included sub put_storage
      use module_debug

      real, allocatable, dimension(:) :: hold_array
      parameter(msk1=32000,msk2=4000)
      character(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
      integer year, month, day, hour, minute, second, fcst
      character(len=*)  :: gribflnm
      character(len=*)  :: hdate
      character(len=8)  :: pabbrev
      character(len=20) :: labbrev
      character(len=80) :: tabbrev
      integer :: lskip, lgrib
      integer :: junit, itot, icount, iseek
      integer :: grib_edition
      integer :: i, j, ireaderr, ith , debug_level
      integer :: currlen
      logical :: unpack, expand
      type(gribfield) :: gfld
      ! For subroutine put_storage
      real :: level
      real :: scale_factor
      integer :: iplvl
      character (len=9) :: my_field
      character (len=8) :: tmp8
      ! For subroutine outout
      integer , parameter :: maxlvl = 100
      real , dimension(maxlvl) :: plvl
      integer :: nlvl
      integer , dimension(maxlvl) :: level_array

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SET ARGUMENTS

      write(6,*)' rd_grib2: call start'
      call start()
      unpack=.true.
      expand=.true.
      hdate = '0000-00-00_00:00:00'
      ierr=0
      itot=0
      icount=0
      iseek=0
      lskip=0
      lgrib=0
      currlen=0
      ith=1
      scale_factor = 1e6
      !ptm call mprintf(.false.,DEBUG,"Begin rd_grib2")

!/* IOS Return Codes from BACIO:  */
!/*  0    All was well                                   */
!/* -1    Tried to open read only _and_ write only       */
!/* -2    Tried to read and write in the same call       */
!/* -3    Internal failure in name processing            */
!/* -4    Failure in opening file                        */
!/* -5    Tried to read on a write-only file             */
!/* -6    Failed in read to find the 'start' location    */
!/* -7    Tried to write to a read only file             */
!/* -8    Failed in write to find the 'start' location   */
!/* -9    Error in close                                 */
!/* -10   Read or wrote fewer data than requested        */

!if ireaderr =1 we have hit the end of a file.
!if ireaderr =2 we have hit the end of all the files.


      ! Open a byte-addressable file.
      write(6,*)' rd_grib2: call BAOPENR'
      CALL BAOPENR(junit,gribflnm,IOS)
      if (ios.eq.0) then
      VERSION: do

         ! Search opend file for the next GRIB2 messege (record).
!        write(6,*)' rd_grib2: call skgb'
         call skgb(junit,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
         if (lgrib.eq.0) then
            exit
         endif

         ! Check size, if needed allocate more memory.
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            !print *,'G2 allocate(cgrib(lgrib)) status: ',IS
            currlen=lgrib
         endif

         ! Read a given number of bytes from unblocked file.
         call baread(junit,lskip,lgrib,lengrib,cgrib)

         iseek=lskip+lgrib
         icount=icount+1

         ! Unpack GRIB2 field
         call gb_info(cgrib,lengrib,listsec0,listsec1,  &
                     numfields,numlocal,maxlocal,ierr)
         itot=itot+numfields

         grib_edition=listsec0(2)
         if (grib_edition.ne.2) then
              exit VERSION
         endif


         ! ----
         ! Once per file fill in date, model and projection values.

         if (lskip.lt.10) then

           ! Build the 19-character date string, based on GRIB2 header date
           ! and time information, including forecast time information:

           n=1
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           year  =gfld%idsect(6)     !(FOUR-DIGIT) YEAR OF THE DATA
           month =gfld%idsect(7)     ! MONTH OF THE DATA
           day   =gfld%idsect(8)     ! DAY OF THE DATA
           hour  =gfld%idsect(9)     ! HOUR OF THE DATA
           minute=gfld%idsect(10)    ! MINUTE OF THE DATA
           second=gfld%idsect(11)    ! SECOND OF THE DATA

           fcst = 0
           ! Parse the forecast time info from Sect 4.
           if (gfld%ipdtnum.eq.0) then  ! Product Definition Template 4.0

             ! Extract forecast time.
             fcst = gfld%ipdtmpl(9)

           endif

           ! Compute valid time.

           !print *, 'ymd',gfld%idsect(6),gfld%idsect(7),gfld%idsect(8)
           !print *, 'hhmm  ',gfld%idsect(9),gfld%idsect(10)

           call build_hdate(hdate,year,month,day,hour,minute,second)
	   !ptm call mprintf(.false.,DEBUG,"G2 hdate = %s ",s1=hdate)
           call geth_newdate(hdate,hdate,3600*fcst)
	   !ptm call mprintf(.false.,DEBUG,"G2 hdate (fcst?) = %s ",s1=hdate)

           !--

           ! Indicator of the source (center) of the data.
           icenter = gfld%idsect(1)

           ! Indicator of model (or whatever) which generated the data.
           iprocess = gfld%ipdtmpl(5)


           if (icenter.eq.7) then
             if (iprocess.eq.83 .or. iprocess.eq.84) then
               map%source = 'NCEP NAM Model'
             elseif (iprocess.eq.81) then
               map%source = 'NCEP GFS Model'
             elseif (iprocess.eq.96) then
               map%source = 'NCEP GFS Model'
             elseif (iprocess.eq.109) then
               map%source = 'NCEP RTMA'
             elseif (iprocess.eq.105) then
               map%source = 'NCEP RUC Model'
             elseif (iprocess.eq.140) then
               map%source = 'NCEP NARR'
             elseif (iprocess.eq.44) then
               map%source = 'NCEP SST Analysis'
             else
               map%source = 'unknown model from NCEP'
             end if
	   else if (icenter .eq. 57) then
	     if (iprocess .eq. 87) then
	       map%source = 'AFWA AGRMET'
	     else
	       map%source = 'AFWA'
	     endif
	   else if (icenter .eq. 98) then
	     map%source = 'ECMWF'
           else
             map%source = 'unknown model and orig center'
           end if

           !--

           ! Store information about the grid on which the data is.
           ! This stuff gets stored in the MAP variable, as defined in
           ! module GRIDINFO.

           map%startloc = 'SWCORNER'
	   map%grid_wind = .true.

           if (gfld%igdtnum.eq.0) then ! Lat/Lon grid aka Cylindrical Equidistant
              map%igrid = 0
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%dx = gfld%igdtmpl(17)
              map%dy = gfld%igdtmpl(18)
              map%lat1 = gfld%igdtmpl(12)
              map%lon1 = gfld%igdtmpl(13)
              map%lat2 = gfld%igdtmpl(15)
              map%lon2 = gfld%igdtmpl(16)
              write(tmp8,'(b8.8)') gfld%igdtmpl(14)
	      if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
	      map%r_earth = earth_radius (gfld%igdtmpl(1))

              ! Scale dx/dy values to degrees, default range is 1e6.
              if (map%dx.gt.10000) then
                 map%dx = map%dx/scale_factor
              endif
              if (map%dy.gt.10000) then
                 map%dy = map%dy/scale_factor
              endif

              ! Scale lat/lon values to 0-180, default range is 1e6.
              if (map%lat1.ge.scale_factor) then
                 map%lat1 = map%lat1/scale_factor
              endif
              if (map%lon1.ge.scale_factor) then
                 map%lon1 = map%lon1/scale_factor
              endif

! The following is needed for NCEP GFS, 0.5 degree output. The j-scan is in the -y direction.
! In WPS, the sign of dy indicates the direction of the scan.
	      write(tmp8,'(b8.8)') gfld%igdtmpl(19)
	      read(tmp8,'(1x,i1)') jscan
              if ( jscan .eq. 0 .and. map%dy .gt. 0. ) then
	        map%dy = -1. * map%dy
	      endif

           elseif (gfld%igdtnum.eq.20) then ! Polar-Stereographic Grid.
              map%igrid = 5
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = gfld%igdtmpl(14) / scale_factor
              map%truelat1 = 60.
              map%truelat2 = 91.
              map%dx = gfld%igdtmpl(15) / scale_factor
              map%dy = gfld%igdtmpl(16) / scale_factor
              map%lat1 = gfld%igdtmpl(10) / scale_factor
              map%lon1 = gfld%igdtmpl(11) / scale_factor
              write(tmp8,'(b8.8)') gfld%igdtmpl(12)
	      if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
	      map%r_earth = earth_radius (gfld%igdtmpl(1))

           elseif (gfld%igdtnum.eq.30) then ! Lambert Conformal Grid
              map%igrid = 3
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = gfld%igdtmpl(14) / scale_factor
              map%truelat1 = gfld%igdtmpl(19) / scale_factor
              map%truelat2 = gfld%igdtmpl(20) / scale_factor
              map%dx = gfld%igdtmpl(15) / scale_factor
              map%dy = gfld%igdtmpl(16) / scale_factor
              map%lat1 = gfld%igdtmpl(10) / scale_factor
              map%lon1 = gfld%igdtmpl(11) / scale_factor
              write(tmp8,'(b8.8)') gfld%igdtmpl(12)
	      if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
	      map%r_earth = earth_radius (gfld%igdtmpl(1))

           elseif(gfld%igdtnum.eq.40) then ! Gaussian Grid (we will call it lat/lon)
              map%igrid = 4
              map%nx = gfld%igdtmpl(8)     ! Ni - # of points along a parallel
              map%ny = gfld%igdtmpl(9)     ! Nj - # of points along meridian
              map%dx = gfld%igdtmpl(17)    ! Di - i direction increment
              map%dy = gfld%igdtmpl(18)    ! N - # of parallels between pole and equator
              map%lat1 = gfld%igdtmpl(12)  ! La1 - lat of 1st grid point
              map%lon1 = gfld%igdtmpl(13)  ! Lo1 - lon of 1st grid point
              map%lat2 = gfld%igdtmpl(15)  ! La2 - lat of last grid point
              map%lon2 = gfld%igdtmpl(16)  ! Lo2 - lon of last grid point
              write(tmp8,'(b8.8)') gfld%igdtmpl(14)  ! resolution/component flag
	      if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
	      map%r_earth = earth_radius (gfld%igdtmpl(1))

              ! Scale dx/dy values to degrees, default range is 1e6.
              if (map%dx.gt.10000) then
                 map%dx = map%dx/scale_factor
              endif
              if (map%dy.gt.10000) then
                 map%dy = (map%dy/scale_factor)*(-1)
              endif

              ! Scale lat/lon values to 0-180, default range is 1e6.
              if (map%lat1.ge.scale_factor) then
                 map%lat1 = map%lat1/scale_factor
              endif
              if (map%lon1.ge.scale_factor) then
                 map%lon1 = map%lon1/scale_factor
              endif

           else

           endif

	   if (icenter.eq.7) then
	     call ncep_grid_num (gfld%igdtnum)
	   endif
         endif

         ! ----

         ! Continue to unpack GRIB2 field.
         do n=1,numfields ! e.g. U and V would =2, otherwise its usually =1
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           if (ierr.ne.0) then
             write(*,*) ' ERROR extracting field gf_getfld = ',ierr
             cycle
           endif

!          if(gfld%ipdtmpl(10) .eq. 103 .or. gfld%ipdtmpl(10) .eq. 106)then
           if(gfld%ipdtmpl(10) .eq. 106)then
             write(6,*)' Potential soil level in GRIB2 file ',gfld%ipdtmpl(10),gfld%ipdtmpl(12),gfld%ipdtmpl(15)
           endif

! ------------------------------------

           MATCH_LOOP: do i=1,maxvar ! Max variables found in Vtable,
                                     ! maxvar is defined in table.mod

            if (gfld%discipline .eq. g2code(1,i) .and. &  !Discipline
                gfld%ipdtmpl(1) .eq. g2code(2,i) .and. &  !Category
                gfld%ipdtmpl(2) .eq. g2code(3,i) .and. &  !Parameter
                gfld%ipdtmpl(10) .eq. g2code(4,i)) then   !Elevation

              pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1), &
                                       gfld%ipdtmpl(2))

              my_field=namvar(i)

! need to match up soil levels with those requested.
! For the Vtable levels, -88 = all levels, -99 = missing. The units
! vary depending on the level code (e.g. 106 = cm, 103 = m).
	      if ( gfld%ipdtmpl(10) .eq. 106 ) then
	        TMP8LOOP: do j = 1, maxvar
		  if ((g2code(4,j) .eq. 106) .and. &
                     (gfld%ipdtmpl(2) .eq. g2code(3,j)) .and. &
                     (gfld%ipdtmpl(12) .eq. level1(j)) .and. &
                     ((gfld%ipdtmpl(15) .eq. level2(j)) .or.  &
                                         (level2(j) .le. -88))) then
		    my_field = namvar(j)
		    exit TMP8LOOP
		  endif
		enddo TMP8LOOP
		if (j .gt. maxvar ) then
		  write(6,'(a,i6,a,i6,a)') 'Subsoil level ', &
                     gfld%ipdtmpl(12),' to ',gfld%ipdtmpl(15), &
                 ' in the GRIB2 file, was not found in the Vtable ',myfield
		endif
	      endif

              ! Level (eg. 10000 mb)
              if(gfld%ipdtmpl(10).eq.100) then
                 ! Pressure level (range from 1000mb to 0mb)
                 level=gfld%ipdtmpl(12)
              elseif(gfld%ipdtmpl(10).eq.105) then
                 ! Hybrid level (range from 1 to N)
                 level=gfld%ipdtmpl(12)
              elseif(gfld%ipdtmpl(10).eq.104) then
                 ! Sigma level (range from 10000 to 0)
                 level=gfld%ipdtmpl(12)
                 !Press Vert Vel (8) & sigma 0.0995 (12)
                 if(gfld%ipdtmpl(2).eq.8) level=200100.
              elseif(gfld%ipdtmpl(10).eq.101) then
                 ! MSL
                 level=201300.
              elseif(gfld%ipdtmpl(10).eq.106.or. &
                     gfld%ipdtmpl(10).eq.1) then
                 ! Misc near ground/surface levels
                 level=200100.
                 write(6,*)' soil level g2code matched ',gfld%ipdtmpl(10),gfld%ipdtmpl(12),gfld%ipdtmpl(15)
              else
                 ! Misc near ground/surface levels
                 level=200100.
              endif
              iplvl = int(level)

              ! Store the unpacked 2D slab from the GRIB2 record
              allocate(hold_array(gfld%ngrdpts))
              do j=1,gfld%ngrdpts
                 hold_array(j)=gfld%fld(j)
              enddo

!   Some grids need to be reordered. Until we get an example, this is
!   a placeholder
!             call reorder_it (hold_array, map%nx, map%ny, map%dx,
!    &                 map%dy, iorder)

              ! When we have reached this point, we have a data array ARRAY
              ! which has some data we want to save, with field name FIELD
              ! at pressure level LEVEL (Pa).  Dimensions of this data are
              ! map%nx and map%ny.  Put this data into storage.

              call put_storage(iplvl,my_field, &
                 reshape(hold_array(1:map%nx*map%ny), &
                 (/map%nx, map%ny/)), map%nx,map%ny)
              deallocate(hold_array)

              ! If Specific Humidity is present on hybrid levels AND
              ! upper-air RH is missing, see if we can compute RH from
              ! Specific Humidity.
              if (.not. is_there(iplvl, 'RH') .and. &
                  is_there(iplvl, 'SH') .and. &
                  is_there(iplvl, 'TT') .and. &
                  is_there(iplvl, 'P')) then
                  call g2_compute_rh_spechumd_upa(map%nx,map%ny,iplvl)
                 !call llstor_remove(iplvl, 'SH') !We are done with SH
              endif

              ith=ith+1
              exit MATCH_LOOP

            endif ! Selected param (matched g2code)

           enddo MATCH_LOOP

         enddo ! 1,numfields


         ! Deallocate arrays decoding GRIB2 record.
         call gf_free(gfld)

      enddo VERSION ! skgb


      CALL BACLOSE(junit,IOS)

       ireaderr=1
      else
       call mprintf (.false.,STDOUT,"open status failed because %i ",i1=ios)
       hdate = '9999-99-99_99:99:99'
       ireaderr=2
      endif ! ireaderr check

      END subroutine rd_grib2

!*****************************************************************************!
! Subroutine edition_num                                                      !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB2 file.  Based on the information in  !
!    the GRIB2 header and the user-defined Vtable, decide whether the field in!
!    the GRIB2 record is one to process or to skip.  If the field is one we   !
!    want to keep, extract the data from the GRIB2 record, and pass the data  !
!    back to the calling routine.                                             !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       JUNIT   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we do not do Fortran I/O for the GRIB2   !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It is really just an array index to the    !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       GRIB2FILE: File name to open, if it is not already open.              !
!                                                                             !
!    Output:                                                                  !
!       GRIB_EDITION: Set to 1 for GRIB and set to 2 for GRIB2                !
!       IERR     : Error flag: 0 - no error on read from GRIB2 file.          !
!                              1 - Hit the end of the GRIB2 file.             !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
! Author: Paula McCaslin                                                      !
! NOAA/FSL                                                                    !
! Sept 2004                                                                   !
!*****************************************************************************!

      SUBROUTINE edition_num(junit, gribflnm, grib_edition, ireaderr)

      use grib_mod
      use params
      use module_debug

      parameter(msk1=32000,msk2=4000)
      character(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
      character(len=*)  :: gribflnm
      integer :: lskip, lgrib
      integer :: junit
      integer :: grib_edition
      integer :: i, j, ireaderr
      integer :: currlen

      character(len=4) :: ctemp
      character(len=4),parameter :: grib='GRIB',c7777='7777'

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SET ARGUMENTS

      call start()
      itot=0
      icount=0
      iseek=0
      lskip=0
      lgrib=0
      currlen=0

!/* IOS Return Codes from BACIO:  */
!/*  0    All was well                                   */
!/* -1    Tried to open read only _and_ write only       */
!/* -2    Tried to read and write in the same call       */
!/* -3    Internal failure in name processing            */
!/* -4    Failure in opening file                        */
!/* -5    Tried to read on a write-only file             */
!/* -6    Failed in read to find the 'start' location    */
!/* -7    Tried to write to a read only file             */
!/* -8    Failed in write to find the 'start' location   */
!/* -9    Error in close                                 */
!/* -10   Read or wrote fewer data than requested        */

!if ireaderr =1 we have hit the end of a file.
!if ireaderr =2 we have hit the end of all the files.
!if ireaderr =3 beginning characters 'GRIB' not found

      ! Open a byte-addressable file.
      CALL BAOPENR(junit,gribflnm,IOS)
      if (ios.eq.0) then

         ! Search opend file for the next GRIB2 messege (record).
         call skgb(junit,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
	 call mprintf((lgrib.eq.0),ERROR, &
           "Grib2 file or date problem, stopping in edition_num.")

         ! Check size, if needed allocate more memory.
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            currlen=lgrib
         endif

         ! Read a given number of bytes from unblocked file.
         call baread(junit,lskip,lgrib,lengrib,cgrib)

         ! Check for beginning of GRIB message in the first 100 bytes
         istart=0
         do j=1,100
            ctemp=cgrib(j)//cgrib(j+1)//cgrib(j+2)//cgrib(j+3)
            if (ctemp.eq.grib ) then
              istart=j
              exit
            endif
         enddo
         if (istart.eq.0) then
            ireaderr=3
            print*, "The beginning 4 characters >GRIB< were not found."
         endif

         ! Unpack Section 0 - Indicator Section to extract GRIB edition field
         iofst=8*(istart+5)
         call gbyte(cgrib,discipline,iofst,8)     ! Discipline
         iofst=iofst+8
         call gbyte(cgrib,grib_edition,iofst,8)   ! GRIB edition number

         !ptm print *, 'degrib - grib edition num',  grib_edition
         call summary()
         CALL BACLOSE(junit,IOS)
         ireaderr=1
      else if (ios .eq. -4) then
	call mprintf(.true.,ERROR, &
          "edition_num: unable to open %s",s1=gribflnm)
      else
         print *,'edition_num: open status failed because',ios,gribflnm
         ireaderr=2
      endif ! ireaderr check

      END subroutine edition_num

!*****************************************************************************!

      SUBROUTINE g2_compute_rh_spechumd_upa(ix, jx, iiplvl)
      ! Compute relative humidity from specific humidity in the upper air.
      use storage_module
      implicit none
      integer :: ix, jx
      integer :: iiplvl
      real :: lat1, lon1, dx, dy
      real, dimension(ix,jx) :: T, P, RH, Q

      real, parameter :: svp1=611.2
      real, parameter :: svp2=17.67
      real, parameter :: svp3=29.65
      real, parameter :: svpt0=273.15
      real, parameter :: eps = 0.622

      real startlat, startlon, deltalat, deltalon

      call get_storage(iiplvl, 'P', P, ix, jx)
      call get_storage(iiplvl, 'TT', T, ix, jx)
      call get_storage(iiplvl, 'SH', Q, ix, jx)

      rh=1.E2*(p*q/(q*(1.-eps)+eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))

      call put_storage(iiplvl, 'RH', rh, ix, jx)

      end subroutine g2_compute_rh_spechumd_upa

!*****************************************************************************!

      subroutine ncep_grid_num (pnum)
!
!  Grib2 doesn't have a grid-number entry, so we have to figure it out
!
      use gridinfo       ! Included to define map%
      integer :: pnum
      real, parameter :: eps = .01
      character (len=8) :: tmp8

!     write(6,*) 'begin ncep_grid_num'
!     write(6,*) 'dx = ',map%dx,' pnum = ',pnum,' nx = ',map%nx
      tmp8 = '        '
      if (pnum .eq. 30) then
        if ( abs(map%dx - 12.19058) .lt. eps .and. map%nx .eq. 614) then
	  write(tmp8,'("GRID 218")')
        else if (abs(map%dx - 40.63525) .lt. eps &
           .and. map%nx .eq. 185) then
	  write(tmp8,'("GRID 212")')
        else if (abs(map%dx - 40.63525) .lt. eps &
           .and. map%nx .eq. 151) then
	  write(tmp8,'("GRID 236")')
        else if (abs(map%dx - 81.2705) .lt. eps &
           .and. map%nx .eq. 93) then
	  write(tmp8,'("GRID 211")')
        else if (abs (map%dx - 32.46341) .lt. eps &
           .and. map%nx .eq. 349) then
	  write(tmp8,'("GRID 221")')
        else if (abs(map%dx - 20.317625) .lt. eps &
           .and. map%nx .eq. 301) then
	  write(tmp8,'("GRID 252")')
        endif
      else if (pnum .eq. 20) then
        if (abs(map%dx - 15.0) .lt. eps) then
	  write(tmp8,'("GRID  88")')
	endif
      else if (pnum .eq. 0) then
        if (abs(map%dx - 1.) .lt. eps .and. map%nx .eq. 360) then
	  write(tmp8,'("GRID   3")')
        else if (abs(map%dx - 0.5) .lt. eps .and. map%nx .eq. 720) then
	  write(tmp8,'("GRID   4")')
	endif
      endif
      map%source(25:32) = tmp8
!     write(6,*) 'map%source = ',map%source
      end subroutine ncep_grid_num
!*****************************************************************************!

      function earth_radius (icode)
! Grib2 Code Table 3.2. Returns the spherical earth's radius in km.
      use module_debug
      real :: earth_radius
      integer :: icode
      if ( icode .eq. 0 ) then
        earth_radius = 6367470. * .001
      else if ( icode .eq. 6 ) then
        earth_radius = 6371229. * .001
      else
	call mprintf(.true.,ERROR, &
         "unknown earth radius for code %i",i1=icode)
      endif
      end function earth_radius
