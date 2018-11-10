cdis    Forecast Systems Laboratory
cdis    NOAA/OAR/ERL/FSL
cdis    325 Broadway
cdis    Boulder, CO     80303
cdis 
cdis    Forecast Research Division
cdis    Local Analysis and Prediction Branch
cdis    LAPS 
cdis 
cdis    This software and its documentation are in the public domain and 
cdis    are furnished "as is."  The United States government, its 
cdis    instrumentalities, officers, employees, and agents make no 
cdis    warranty, express or implied, as to the usefulness of the software 
cdis    and documentation for any purpose.  They assume no responsibility 
cdis    (1) for the use of the software and documentation; or (2) to provide
cdis     technical support to users.
cdis    
cdis    Permission to use, copy, modify, and distribute this software is
cdis    hereby granted, provided that the entire disclaimer notice appears
cdis    in all copies.  All modifications to this software must be clearly
cdis    documented, and are solely the responsibility of the agent making 
cdis    the modifications.  If significant modifications or enhancements 
cdis    are made to this software, the FSL Software Policy Manager  
cdis    (softwaremgr@fsl.noaa.gov) should be notified.
cdis 
cdis 
cdis 
cdis 
cdis 
cdis 
cdis 

      program diff_test

        integer imax,jmax,kmax,nlvl

        include 'lapsparms.for'

!       parameter (imax=79)
!       parameter (jmax=73)
        parameter (imax=NX_L)
        parameter (jmax=NY_L)
        parameter (kmax=300)
        parameter (nlvl=300)
      
      INTEGER*4	I4TIME,		!I4time of data
     1		LVL(nlvl),        !Level of each field (4 digit max)
     1		LVL_AVAIL(nlvl),        !Level of each field (4 digit max)
     1		I,J,K,
     1		ISTATUS
C
      REAL*4	DATA1(imax,jmax,kmax)	!Raw data to be written
      REAL*4	DATA2(imax,jmax,kmax)	!Raw data to be written
C
      CHARACTER*70	DIR_in		!Directory to be written to
      CHARACTER*70	DIR_out		!Directory to be written to
      CHARACTER*31	EXT		!File name ext (up to 31 chars)
      CHARACTER*3	VAR(nlvl) 	        !3 letter ID of each field
      CHARACTER*3	LAPS_VAR_AVAIL(nlvl) 	!3 letter ID of each field
      character*19      VAR_AVAIL(nlvl)
      CHARACTER*4	LVL_COORD(nlvl)	!Vertical coordinate for each field
      CHARACTER*10	UNITS(nlvl)	!units of each field
!     CHARACTER*250	COMMENT1(nlvl)	!Comments for each field
!     CHARACTER*210	COMMENT2(nlvl)	!Comments for each field
      CHARACTER*125	COMMENT1(nlvl)	!Comments for each field
      CHARACTER*125	COMMENT2(nlvl)	!Comments for each field
      CHARACTER*17	ASCTIME
      
      CHARACTER*4	VERSION
      CHARACTER*131	MODEL 		!Meteorological model in file
      CHARACTER*131	ORIGIN		!Location where file was created
      CHARACTER*11    LAPS_DOM_FILE   !Name of domain file e.g. NEST7GRID
      LOGICAL         l_packed_data

      logical l_pass, l_is_vxx

      character*17 filename
      character*9 a9_time
      character*3 var_last

      real machine_factor

      var_last = '   '
      idiff_msg_flag = 0
      diff_max_all = 0.
      diff_max_all_rel = 0.
      n_files = 0
      ndiff_all = 0
      l_pass = .true.
      num_diff_field_thresh = 0

      write(6,*)' Enter 1 if comparing different machines, 0 if same'
      read(5,*)machine_factor

      write(6,*)' filename?'
5     read(5,1)filename
1     format(a)

      if(filename(1:3) .eq. 'end')then
!         write(6,*)' end'
          goto 999
      endif

      read(5,1,err=999)dir_in
      read(5,1,err=999)dir_out

      write(6,*)' machine_factor = ',machine_factor

      if(filename(1:6) .eq. 'static')then
          if(.false.)then
              khmax = 24
              var(1) = 'LAT'
              var(2) = 'LON'
              var(3) = 'AVG'
              var(4) = 'STD'
              var(5) = 'ZIN'
              var(6) = 'LDF'
              var(7) = 'LND'
              var(8) = 'USE'
              var(9) = 'SLN'
              var(10) = 'SLT'
              var(11) = 'STL'
              var(12) = 'SBL'
              var(13) = 'A01'
              var(14) = 'A02'
              var(15) = 'A03'
              var(16) = 'A04'
              var(17) = 'A05'
              var(18) = 'A06'
              var(19) = 'A07'
              var(20) = 'A08'
              var(21) = 'A09'
              var(22) = 'A10'
              var(23) = 'A11'
              var(24) = 'A12'
          else
              khmax = 38
              call get_gridgen_var(nlvl,khmax,var,comment1)
          endif
          ext = 'nest7grid'
          call rd_laps_static(dir_in,ext,imax,jmax,khmax,var,units
     1                     ,comment1,data1,grid_spacing_m,istatus)

          if(istatus .ne. 1)then
              write(6,*)' Bad status reading 1st static file'
              stop
          endif

          call rd_laps_static(dir_out,ext,imax,jmax,khmax,var,units
     1                     ,comment2,data2,grid_spacing_m,istatus)

          if(istatus .ne. 1)then
              write(6,*)' Bad status reading 2nd static file'
              stop
          endif

          thresh_write_pair = .01
          thresh_count_diff = .01

          ihmax = imax
          jhmax = jmax
 
      else

          call s_len(filename,lenf)
          if(lenf .eq. 13)then
              a9_time = filename(1:9)
              ext = filename(11:13)
          else
              a9_time = filename(1:9)
              ext = filename(15:17)
          endif
          call downcase(ext,ext)
          call cv_asc_i4time(a9_time,i4time)

          if(dir_in(1:3) .eq. 'end')then    
!             write(6,*)' end'
              goto 999
          endif

          do i = 1,nlvl
              var(i) = '   '
          enddo

          if(.true.)then
              ihmax = imax
              jhmax = jmax
              call get_laps_dimensions(nk,istatus)
              if(istatus .ne. 1)then
                  write(6,*)' Bad status returned from get_laps_dims'
                  stop
              endif

              call rlh(ext,nk,var,lvl,khmax,istatus)
              if(istatus .ne. 1)then
                  write(6,*)' Bad status returned from rlh'
                  stop
              endif

          else
!             call READ_LAPS_HEADER(
!    1                         I4TIME,DIR_IN,EXT,IHMAX,JHMAX,KHMAX,
!    1                         LAPS_DOM_FILE,ASCTIME,VERSION,
!    1                         MODEL,ORIGIN,VAR,LVL,NUM_VARIABLES,
!    1                         VAR_AVAIL,LAPS_VAR_AVAIL,NUM_LEVELS,
!    1                         LVL_AVAIL,LVL_COORD,UNITS,
!    1                         COMMENT1,L_PACKED_DATA,ISTATUS)

          endif
 
!         For this extension, set default values of:
!         thresh_write_pair, thresh_count_diff, or num_diff_field_thresh

          thresh_write_pair = 1e-05
          thresh_count_diff = 0.
          num_diff_field_thresh = 10

!         For this particular extension, set values of: 
!         thresh_write_pair, thresh_count_diff, or num_diff_field_thresh

          if(ext(1:3) .eq. 'lc3')then
              do i = 1,42
                  var(i) = 'LC3'
                  lvl(i) = i
              enddo
              thresh_write_pair = .01
              thresh_count_diff = .01
              num_diff_field_thresh = 20

          elseif(ext(1:3) .eq. 'lcp')then
              thresh_write_pair = .01
              thresh_count_diff = .01
              num_diff_field_thresh = 20

          elseif(ext(1:3) .eq. 'lcv')then
              thresh_write_pair = .01
              thresh_count_diff = .01
              num_diff_field_thresh = 25

          elseif(ext(1:3) .eq. 'lco')then
              thresh_write_pair = .01
              thresh_count_diff = .01
              num_diff_field_thresh = 25

          elseif(ext(1:3) .eq. 'lcb')then
              thresh_write_pair = 1.0
              thresh_count_diff = 1.0

          elseif(ext(1:3) .eq. 'lwc')then
              thresh_write_pair = .000001
              thresh_count_diff = .000001

          elseif(ext(1:3) .eq. 'lw3')then
              thresh_write_pair = .01
              thresh_count_diff = .1

          elseif(ext(1:3) .eq. 'liw')then
              thresh_write_pair = .001
              thresh_count_diff = .01

          elseif(ext(1:3) .eq. 'lwm')then
              thresh_write_pair = .01
              thresh_count_diff = .1

          elseif(ext(1:3) .eq. 'lps')then
              thresh_write_pair = .01
              thresh_count_diff = .1

          elseif(ext(1:3) .eq. 'lhe')then
              thresh_write_pair = .001
              thresh_count_diff = .001

          elseif(ext(1:3) .eq. 'lil')then
              thresh_write_pair = .0001
              thresh_count_diff = .0001

          elseif(ext(1:3) .eq. 'lst')then
              thresh_write_pair = .05
              thresh_count_diff = .05

          elseif(ext(1:3) .eq. 'lvd')then
              thresh_write_pair = 1.
              thresh_count_diff = 1.

          elseif(ext(1:3) .eq. 'vrc')then
              thresh_write_pair = 0.1
              thresh_count_diff = 0.1

          elseif(ext(1:3) .eq. 'l1s')then
              do i = 1,4
                  lvl(i) = 0
              enddo
              var(1) = 'R01'
              var(2) = 'RTO'
              var(3) = 'S01'
              var(4) = 'STO'
              thresh_write_pair = .0001
              thresh_count_diff = .0001

          elseif(ext(1:3) .eq. 'lps')then
              do i = 1,21
                  lvl(i) = 1150 - 50 * i            
              enddo

          elseif(ext(1:3) .eq. 'lsx')then
              thresh_write_pair = .01
              thresh_count_diff = .01
              num_diff_field_thresh = 20

          endif

!         Adjust values for this extension (dependent on machine) of: 
!         thresh_count_diff and num_diff_field_thresh

          if(machine_factor .eq. 0.)then
              thresh_count_diff = 0.
              num_diff_field_thresh = 0
          else
              thresh_count_diff = thresh_count_diff * machine_factor
          endif

          if(var(1) .eq. 'RH')var(1) = 'LHE'

!         Read first file
          call s_len(dir_in,len_dir_in)

          if(lenf .eq. 13)then
              write(6,*)' Reading: ',dir_in(1:len_dir_in),a9_time
     1                           ,'.',ext(1:3)

              call read_laps_data(i4time,dir_in,ext,ihmax,jhmax,khmax,
     1             khmax,var,lvl,lvl_coord,units,comment1,data1,
     1             istatus)
          else ! lga/lgb files
              write(6,*)' Reading: ',dir_in(1:len_dir_in),a9_time
     1                           ,'.',ext(1:3)
              i4_valid = i4time + 7200
              call read_laps(i4time,i4_valid,dir_in,ext,
     1             ihmax,jhmax,khmax,khmax,var,lvl,lvl_coord,
     1             units,comment1,data1,
     1             istatus)
!             if(istatus .eq. 0)istatus = 1
          endif

          if(istatus .ne. 1)then
            if(.not. l_is_vxx(ext) )then
              if(l_pass)then
                  write(6,*)' READ ERROR: OVERALL CRITERIA FAILURE'
                  l_pass = .false.     
              endif
            else
              write(6,*)' Attempting compressed radar data read'
              do if = 1,3
                  write(6,*)' if = ',if
                  kp = 1 + ((if-1) * nk)
                  call read_laps_compressed(i4time,dir_in,ext
     1                               ,ihmax,jhmax,nk
     1                               ,var(kp),lvl(kp),lvl_coord(kp)
     1                               ,units(kp),comment1(kp)
     1                               ,data1(1,1,kp),istatus)
              enddo ! if
              if(istatus .ne. 1)then
                  write(6,*)' READ ERROR: OVERALL CRITERIA FAILURE'
                  l_pass = .false.     
              endif
            endif
          endif

!         Read second file
          call s_len(dir_out,len_dir_out)

          if(lenf .eq. 13)then  
              write(6,*)' Reading: ',dir_out(1:len_dir_out),a9_time
     1                     ,'.',ext(1:3)
              call read_laps_data(i4time,dir_out,ext,ihmax,jhmax,khmax,
     1             khmax,var,lvl,lvl_coord,units,comment2,data2,
     1             istatus)
          else ! lga/lgb
              write(6,*)' Reading: ',dir_out(1:len_dir_out),a9_time
     1                           ,'.',ext(1:3)
              i4_valid = i4time + 7200
              call read_laps(i4time,i4_valid,dir_out,ext,
     1             ihmax,jhmax,khmax,khmax,var,lvl,lvl_coord,
     1             units,comment2,data2,
     1             istatus)
!             if(istatus .eq. 0)istatus = 1
          endif

          if(istatus .ne. 1)then
            if(.not. l_is_vxx(ext) )then
              if(l_pass)then
                  write(6,*)' READ ERROR: OVERALL CRITERIA FAILURE'
                  l_pass = .false.     
              endif
            else
              write(6,*)' Attempting compressed radar data read'
              do if = 1,3
                  write(6,*)' if = ',if
                  kp = 1 + ((if-1) * nk)
                  call read_laps_compressed(i4time,dir_out,ext
     1                               ,ihmax,jhmax,nk
     1                               ,var(kp),lvl(kp),lvl_coord(kp)
     1                               ,units(kp),comment2(kp)
     1                               ,data2(1,1,kp),istatus)
              enddo ! if
              if(istatus .ne. 1)then
                  write(6,*)' READ ERROR: OVERALL CRITERIA FAILURE'
                  l_pass = .false.     
              endif
            endif
          endif

      endif ! static file

!       write(6,*)' Hit RETURN to CONTINUE'
!	read(5,*)

        thresh_write_pair = thresh_write_pair * machine_factor

        diff_max_file_rel = 0.
        diff_max_file = 0.
        diff_max_var = 0.
        ndiff_file = 0
        nvar = 1
        n_levels = 0

        do k = 1,khmax

!       Test whether we should switch variables within this file
        if(k .gt. 1)then
            if(var(k) .ne. var(k-1))then
                if(n_levels .gt. 1)then
                    write(6,*)' Max diff for variable ',var(k-1)(1:3)
     1                      ,' =',diff_max_var
                    write(6,*)
                    diff_max_var = 0.
                    nvar = nvar + 1
                endif
                n_levels = 0
            endif
        endif

!       Test if new variable
        if(var(k)(1:3) .ne. var_last)then

            write(6,*)
            write(6,*)' New var = ',var(k)(1:3)

!           For this variable, set values of:
!           thresh_write_pair, thresh_count_diff, or num_diff_field_thresh

            if    (ext(1:3) .eq. 'lvd')thresh_write_pair = 1.0

            if    (ext(1:3) .eq. 'lt1' .and. var(k)(1:2) .eq. 'HT')then       
                thresh_count_diff = 2.0 * machine_factor
            elseif(ext(1:3) .eq. 'lt1' .and. var(k)(1:2) .eq. 'T3')then
                thresh_count_diff = .02 * machine_factor
            elseif(ext(1:3) .eq. 'lga' .and. var(k)(1:2) .eq. 'HT')then
                thresh_count_diff = .01 * machine_factor
            elseif(ext(1:3) .eq. 'lga' .and. var(k)(1:2) .eq. 'T3')then
                thresh_count_diff = .001 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'USF')then
                thresh_count_diff = .001 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'VSF')then
                thresh_count_diff = .001 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'TSF')then
                thresh_count_diff = .001 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'DSF')then
                thresh_count_diff = .001 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'TGD')then
                thresh_count_diff = .002 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'SLP')then
                thresh_count_diff = 15.0 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'PSF')then
                thresh_count_diff = 15.0 * machine_factor
            elseif(ext(1:3) .eq. 'lgb' .and. var(k)(1:3) .eq. 'P')then
                thresh_count_diff = 15.0 * machine_factor
            elseif(ext(1:3) .eq. 'lmt' .and. var(k)(1:3) .eq. 'LMT')then
                thresh_count_diff = 2.0 * machine_factor
            elseif(ext(1:3) .eq. 'lmt' .and. var(k)(1:3) .eq. 'LLR')then
                thresh_count_diff = 2.0 * machine_factor
            elseif(ext(1:3) .eq. 'lsx' .and. var(k)(1:1) .eq. 'U')then
                thresh_count_diff = .02 * machine_factor
            elseif(ext(1:3) .eq. 'lsx' .and. var(k)(1:1) .eq. 'V')then
                thresh_count_diff = .02 * machine_factor
            elseif(ext(1:3) .eq. 'lsx' .and. var(k)(1:1) .eq. 'P')then
                thresh_count_diff = 15.0 * machine_factor
            elseif(ext(1:3) .eq. 'lsx' .and. var(k)(1:3) .eq. 'MSL')then
                thresh_count_diff = 40.0 * machine_factor
            elseif(ext(1:3) .eq. 'lhe' .and. var(k)(1:3) .eq. 'LHE')then
                thresh_count_diff = 0.1 * machine_factor
            elseif(ext(1:3) .eq. 'lst' .and. var(k)(1:3) .eq. 'WB0')then
                thresh_count_diff = 5.0 * machine_factor
            elseif(ext(1:3) .eq. 'lvd' .and. var(k)(1:3) .eq. 'SVN')then
                thresh_count_diff = 10.0 * machine_factor
            elseif(ext(1:3) .eq. 'lvd' .and. var(k)(1:3) .eq. 'ALB')then
                thresh_count_diff = 0.01 * machine_factor
                thresh_write_pair = 0.01              !JSmart addition 11-1-96
!           elseif(ext(1:3) .eq. 'lw3' .and. var(k)(1:2) .ne. 'OM')then
!               thresh_count_diff = .1 * machine_factor
            elseif(ext(1:9) .eq. 'nest7grid'
     1                                .and. var(k)(1:3) .eq. 'LAT')then
                thresh_count_diff = .0005 * machine_factor
            elseif(ext(1:9) .eq. 'nest7grid'
     1                                .and. var(k)(1:3) .eq. 'LON')then
                thresh_count_diff = .0005 * machine_factor
            elseif(ext(1:9) .eq. 'nest7grid'
     1                                .and. var(k)(1:3) .eq. 'AVG')then
                thresh_count_diff = 10.0 * machine_factor
            elseif(ext(1:9) .eq. 'nest7grid'
     1                                .and. var(k)(1:3) .eq. 'LDF')then
                thresh_count_diff = .005 * machine_factor
            endif

            write(6,*)
     1      ' Threshold to write (first ten) grid point pairs = '
     1                                               ,thresh_write_pair
            write(6,*)
     1      ' Threshold to count lvl grid point differences   = '
     1                                               ,thresh_count_diff
            write(6,*)
     1      ' Max allowed count of lvl grid point differences = '
     1                                           ,num_diff_field_thresh
            write(6,*)


        endif

        n_levels = n_levels + 1

        diff_max_field = 0.
        diff_max_field_rel = 0.
        abs_value_max = 0.
        imaxd = 0
        jmaxd = 0
        iwrite = 0
        ndiff = 0
        inan = 0
        ndiff_msg = 0
        sumsq = 0.
        n_sq = 0

	do i = 1,ihmax
        do j = 1,jhmax

          call check_nan(data1(i,j,k),istatus_1)
          call check_nan(data2(i,j,k),istatus_2)

          if(istatus_1 .eq. 0 .or. istatus_2 .eq. 0)then
            iwrite = iwrite + 1
            if(iwrite .le. 10)then
                write(6,21)i,j,k,' Nan'
            endif
            inan = inan + 1
          else
            diff     = abs(data1(i,j,k)-data2(i,j,k))

!           Test if one of the points is missing and the other isn't
            if(   (data1(i,j,k) .eq. r_missing_data .or.
     1             data2(i,j,k) .eq. r_missing_data      )        
     1                          .AND.
     1                     diff .gt. 0.                     )then

                ndiff_msg = ndiff_msg + 1
                idiff_msg_flag = 1

            else ! Both data points are non-missing

                diff_max_file = max(diff_max_file,diff)
                diff_max_var = max(diff_max_var,diff)
                if(diff .gt. diff_max_field)then
                    diff_max_field = diff
                    imaxd = i
                    jmaxd = j
                endif

                sumsq = sumsq + diff**2
                n_sq = n_sq + 1

            endif

            if(data1(i,j,k) .ne. r_missing_data)then
                abs_value_max = max(abs_value_max,abs(data1(i,j,k)))
            endif

            if(data2(i,j,k) .ne. r_missing_data)then
                abs_value_max = max(abs_value_max,abs(data2(i,j,k)))
            endif


            if(diff .gt. thresh_count_diff)then
                ndiff = ndiff + 1
                ndiff_file = ndiff_file + 1
                ndiff_all = ndiff_all + 1
            endif

            if(diff .gt. thresh_write_pair)then
                iwrite = iwrite + 1 
                if(iwrite .le. 10)then
                    write(6,21,err=22)i,j,k,data1(i,j,k),data2(i,j,k)
     1                                                  ,diff
21                  format(1x,3i5,2f14.6,f12.6)
22              endif
            endif
          endif ! Nan test
        enddo ! j
        enddo ! i

        if(n_sq .gt. 0)then
           rms = sqrt(sumsq / float(n_sq))
        else
           rms = 0.
        endif

        if(comment1(k)(1:80) .ne. comment2(k)(1:80))then
            write(6,*)' comments differ at level ',k
            write(6,*)'parallel    comment',trim(comment1(k)(1:80))
            write(6,*)'operational comment',trim(comment2(k)(1:80))
        else
            write(6,*)' comments similar at level ',k
            write(6,*)'parallel    comment',trim(comment1(k)(1:80))
            write(6,*)'operational comment',trim(comment2(k)(1:80))
        endif
        if(inan .gt. 0)write(6,*)' # of Nans = ',inan

        if(abs_value_max .gt. 0.)then
!          if(ndiff_msg .eq. 0)then
               diff_max_field_rel = diff_max_field / abs_value_max
!          endif
        else
           diff_max_field_rel = 0.
        endif

        diff_max_file_rel  = max(diff_max_file_rel,diff_max_field_rel)

!	write(6,*)' df_mx - fld #',k,' ',var(k)
!    1  ,lvl(k),' abs/rel/rms/#',diff_max_field,diff_max_field_rel
!    1  ,rms,ndiff,'at',imaxd,jmaxd

 	write(6,101)k,var(k),lvl(k),diff_max_field,diff_max_field_rel
     1             ,rms,ndiff,imaxd,jmaxd

 101    format(' df_mx - fld #',i4,' ',a
     1  ,i6,' abs/rel/rms ',3e13.5,i8,' ndiff   max at',2i4)

        if(k .eq. khmax .and. nvar .gt. 1 
     1                  .and. n_levels .gt. 1)then
                write(6,*)
                write(6,*)' Max diff for variable ',var(k)(1:3),' ='
     1                      ,diff_max_var
                nvar = nvar + 1
        endif

        if(ndiff_msg .gt. 0)then
            write(6,*)
            write(6,*)' WARNING: # OF POINTS DIFFERING '
     1                ,'WRT MISSING DATA = ',
     1                  ndiff_msg
        endif

        if(ndiff + ndiff_msg .gt. num_diff_field_thresh)then
          if(l_pass)then
            write(6,*)' OVERALL CRITERIA FAILURE'
     1                           ,ndiff,ndiff_msg,num_diff_field_thresh
            l_pass = .false.     
          endif
        endif

        write(6,*)

        var_last = var(k)(1:3)

        enddo ! k


      if (istatus .ne. 1) write (6,*)'Error in readlapsdata'

	   write(6,*)' OVERALL FILE diff_max (',ext(1:3)
     1      ,') [abs/rel/#] = ',diff_max_file,diff_max_file_rel
     1               ,ndiff_file
           write(6,*)
           n_files = n_files + 1
           diff_max_all     = max(diff_max_all,diff_max_file)
           diff_max_all_rel = max(diff_max_all_rel,diff_max_file_rel)
           goto 5
      
999     continue

        if(n_files .gt. 1)then
          if(l_pass)then
             write(6,*)' MAX difference (all files)  [abs/rel/#] = '
     1              ,diff_max_all,diff_max_all_rel
     1              ,ndiff_all ! ,' PASSED'
             write(6,*)
             write(6,*)' PASSED'
          else
             write(6,*)' MAX difference (all files)  [abs/rel/#] = '
     1              ,diff_max_all,diff_max_all_rel
     1              ,ndiff_all ! ,' FAILED'
             write(6,*)
             write(6,*)' FAILED'
          endif
           write(6,*)
        else
          if(l_pass)then
            write(6,*)' PASSED'
            write(6,*)
          else
            write(6,*)' FAILED'
            write(6,*)
          endif
        endif
 
 
        if(idiff_msg_flag .eq. 1)then
            write(6,*)
            write(6,*)' WARNING: DIFFERENCES WRT MISSING DATA DETECTED'
            write(6,*)
        endif

        stop

      end
      

      subroutine rlh(ext,nz_l,var,lvl,khmax,istatus)
!                     I   I    O   O    O      O
      
      character*(*) var(*)
      character*3 ext
      integer lvl(*)

      khmax = 0
      istatus = 1
      
      if    (ext .eq. 'lt1')then                                  ! TEMP
          call fill_3d_header('T3',var,lvl,nz_l,khmax)
          call fill_3d_header('HT',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'lsx')then
          khmax = 7
          var(1) = 'T'
          var(2) = 'TD'
          var(3) = 'U'
          var(4) = 'V'
          var(5) = 'P'
          var(6) = 'PS'
          var(7) = 'MSL'
!         var(8) = 'VIS'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0
          lvl(4) = 0
          lvl(5) = 0
          lvl(6) = 0
          lvl(7) = 0
!         lvl(8) = 0

      elseif(ext .eq. 'lst')then
          khmax = 8
          var(1) = 'PBE'
          var(2) = 'NBE'
          var(3) = 'LI'
          var(4) = 'SI'
          var(5) = 'TT'
          var(6) = 'LCL'
          var(7) = 'K'
          var(8) = 'WB0'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0
          lvl(4) = 0
          lvl(5) = 0
          lvl(6) = 0
          lvl(7) = 0
          lvl(8) = 0

      elseif(ext .eq. 'vrc')then
          khmax = 1
          var(1) = 'REF'
          lvl(1) = 0

      elseif(ext .eq. 'lw3')then                                  ! WIND
          call fill_3d_header('U3',var,lvl,nz_l,khmax)
          call fill_3d_header('V3',var,lvl,nz_l,khmax)
          call fill_3d_header('OM',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'lga')then                                  ! LGA
          call fill_3d_header('HT',var,lvl,nz_l,khmax)
          call fill_3d_header('T3',var,lvl,nz_l,khmax)
          call fill_3d_header('SH',var,lvl,nz_l,khmax)
          call fill_3d_header('U3',var,lvl,nz_l,khmax)
          call fill_3d_header('V3',var,lvl,nz_l,khmax)
          call fill_3d_header('OM',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'lgb')then
          khmax = 9  
          var(1) = 'USF'
          var(2) = 'VSF'
          var(3) = 'TSF'
          var(4) = 'TGD'
          var(5) = 'DSF'
          var(6) = 'SLP'
          var(7) = 'PSF'
          var(8) = 'RSF'
          var(9) = 'P'
!         var(10) = 'PCP'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0
          lvl(4) = 0
          lvl(5) = 0
          lvl(6) = 0
          lvl(7) = 0
          lvl(8) = 0
          lvl(9) = 0
!         lvl(10) = 0

      elseif(ext(1:2) .eq. 'v0')then                              ! V0x
          call fill_3d_header('REF',var,lvl,nz_l,khmax)
          call fill_3d_header('VEL',var,lvl,nz_l,khmax)
          call fill_3d_header('NYQ',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'vrz')then                                  
          call fill_3d_header('REF',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'lwm')then
          khmax = 2
          var(1) = 'SU'
          var(2) = 'SV'
          lvl(1) = 0
          lvl(2) = 0

      elseif(ext .eq. 'liw')then
          khmax = 2
          var(1) = 'LIW'
          var(2) = 'UMF'
          lvl(1) = 0
          lvl(2) = 0

      elseif(ext .eq. 'lhe')then
          khmax = 3
          var(1) = 'LHE'
          var(2) = 'MU'
          var(3) = 'MV'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0

      elseif(ext .eq. 'lil')then
          khmax = 2
          var(1) = 'LIL'
          var(2) = 'LIC'
!         var(3) = 'COD'
!         var(4) = 'CLA'
          lvl(1) = 0
          lvl(2) = 0
!         lvl(3) = 0
!         lvl(4) = 0

      elseif(ext .eq. 'lfr')then
          khmax = 4
          var(1) = 'VNT'
          var(2) = 'HAM'
          var(3) = 'HAH'
          var(4) = 'FWI'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0
          lvl(4) = 0

      elseif(ext .eq. 'lc3')then                                  ! CLOUD
          call fill_3d_header('LC3',var,lvl,42,khmax)

      elseif(ext .eq. 'lcp')then
          call fill_3d_header('LCP',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'lco')then
          call fill_3d_header('COM',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'cty')then
          call fill_3d_header('CTY',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'pty')then
          call fill_3d_header('PTY',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'lwc')then
          call fill_3d_header('LWC',var,lvl,nz_l,khmax)
          call fill_3d_header('ICE',var,lvl,nz_l,khmax)
          call fill_3d_header('PCN',var,lvl,nz_l,khmax)
          call fill_3d_header('RAI',var,lvl,nz_l,khmax)
          call fill_3d_header('SNO',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'lct')then                                
          khmax = 3
          var(1) = 'PTT'
          var(2) = 'PTY'
          var(3) = 'SCT'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0

      elseif(ext .eq. 'lcb')then                                
          khmax = 3
          var(1) = 'LCB'
          var(2) = 'LCT'
          var(3) = 'CCE'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0

      elseif(ext .eq. 'lcv')then                                
          khmax = 2
          var(1) = 'LCV'
          var(2) = 'CSC'
          lvl(1) = 0
          lvl(2) = 0

      elseif(ext .eq. 'lmt')then                                
          khmax = 2
          var(1) = 'LMT'
          var(2) = 'LLR'
          lvl(1) = 0
          lvl(2) = 0

      elseif(ext .eq. 'lmr')then                                
          khmax = 1
          var(1) = 'R'
          lvl(1) = 0

      elseif(ext .eq. 'lps')then
          call fill_3d_header('REF',var,lvl,nz_l,khmax)

      elseif(ext .eq. 'l1s')then                                  ! ACCUM
          khmax = 4
          var(1) = 'S01'
          var(2) = 'STO'
          var(3) = 'R01'
          var(4) = 'RTO'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0
          lvl(4) = 0

      elseif(ext .eq. 'lvd')then                                  ! LVD
          khmax = 4
          var(1) = 'ALB'
          var(2) = 'SVS'
          var(3) = 'SVN'
          var(4) = 'S8A'
          lvl(1) = 0
          lvl(2) = 0
          lvl(3) = 0
          lvl(4) = 0

      else
          write(6,*)' ext is ',ext
          istatus = 0

      endif

      return
      end

      subroutine fill_3d_header(varin,var,lvl,nz_l,khmax)
!                                 I    O   O   I     O

      character*(*) varin,var(*)
      integer lvl(*)

      call get_r_missing_data(r_missing_data,istatus)
      call get_laps_dimensions(nk,istatus)

      do k = 1,nz_l
          khmax = khmax + 1
          var(khmax) = varin

          if(k .le. nk)then
              arg = pressure_of_level(k)
              lvl(khmax) = nint(pressure_of_level(k))/100
          endif
      enddo
 
      return
      end


