        use mem_namelist, only: laps_cycle_time,precip_cycle_time

!       1997 Jun        Ken Dritz     Added calls to get_grid_dim_xy and 
!                                     get_laps_dimensions to get values of 
!                                     NX_L, NY_L, NZ_L.
!       1997 Jun        Ken Dritz     Now pass NX_L, NY_L, NZ_L to laps_accum.

c       integer j_status(20),iprod_number(20),i4time_array(20)
        integer j_status(20),iprod_number(20)
        character*9 a9_time

        call get_systime(i4time,a9_time,istatus)
        if(istatus .ne. 1)go to 999

        call get_grid_dim_xy(NX_L,NY_L,istatus)
        if (istatus .ne. 1) then
           write (6,*) 'Error getting horizontal domain dimensions'
           go to 999
        endif

        call get_laps_dimensions(NZ_L,istatus)
        if (istatus .ne. 1) then
           write (6,*) 'Error getting vertical domain dimension'
           go to 999
        endif

        call get_max_radar_files(MAX_RADAR_FILES, istatus)
        if (istatus .ne. 1) then
           write (6,*) 'Error getting max_radar_files'
           go to 999
        endif

        if(i4time .eq. (i4time/precip_cycle_time) *
     1                         precip_cycle_time)then

            call laps_accum_sub(i4time,
     1                          NX_L,NY_L,NZ_L,MAX_RADAR_FILES,
     1                          i_diag,
     1                          n_prods,
     1                          iprod_number,
     1                          j_status)
        else
            write(6,'(" Skipping call to laps_accum_sub")')
        endif

999     continue

        end

