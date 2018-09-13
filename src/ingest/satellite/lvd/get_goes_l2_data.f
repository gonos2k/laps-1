      subroutine get_goes_l2_data
     +                   (i4time_sys,ilaps_cycle_time,NX_L,NY_L
     +                   ,i4time_offset
     +                   ,maxchannel,max_files,nchannels
     +                   ,csat_id,csat_type,chtype
     +                   ,path
     +                   ,image_13,n_ir_elem,n_ir_lines
     +                   ,image_07
     +                   ,image_02,n_vis_elem,n_vis_lines
     +                   ,i4time_goes,istatus)                      ! O

      include 'netcdf.inc'

      character*(*) path
      character*255 filename

      integer nelem, nlines,nf_fid, nf_vid, nf_status
      real image_13(n_ir_elem,n_ir_lines)
      real image_07(n_ir_elem,n_ir_lines)
      real image_02(n_vis_elem,n_vis_lines)

      Character*6  csat_id
      Character*3  csat_type
      Character*3  chtype(maxchannel)
      Character*9  a9time
      Character*13 cfname13,cvt_i4time_wfo_fname13

      write(6,*)' Subroutine get_goes_l2_data ',n_ir_elem,n_ir_lines

      i4time_goes = ((i4time_sys + 0)
     1            / ilaps_cycle_time) * ilaps_cycle_time + i4time_offset

!     Read IR channel 13 data
      call make_fnam_lp(i4time_goes,a9time,istatus)
      filename = trim(path)//'/'//a9time//'_10p4.nc'
      write(6,*)'gr2/13 fname is ',trim(filename)
C
C  Open netcdf File for reading
C
      nf_status=NF_OPEN(filename,NF_NOWRITE,nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status),filename
        istatus=0
!       return
      endif
C
C  Fill all dimension values
C
C
C Get size of nelem
C
      nf_status=NF_INQ_DIMID(nf_fid,'x',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nelem'
      endif
      nf_status=NF_INQ_DIMLEN(nf_fid,nf_vid,nelem)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nelem'
      endif
C
C Get size of nlines
C
      nf_status=NF_INQ_DIMID(nf_fid,'y',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nlines'
      endif
      nf_status=NF_INQ_DIMLEN(nf_fid,nf_vid,nlines)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nlines'
      endif

      call read_goes_np_data(nf_fid,n_ir_elem,n_ir_lines,
     +     csat_id,csat_type,chtype(3),
     +     ilaps_cycle_time, NX_L, NY_L, i4time_earliest,
     +     i4time_latest, lun_out, image_13, istatus)

      nf_status=nf_close(nf_fid)
      
      if(.true.)then

!     Read IR channel 07 data
      call make_fnam_lp(i4time_goes,a9time,istatus)
      filename = trim(path)//'/'//a9time//'_4u.nc'
      write(6,*)'gr2/07 fname is ',trim(filename)
C
C  Open netcdf File for reading
C
      nf_status=NF_OPEN(filename,NF_NOWRITE,nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status),filename
        istatus=0
!       return
      endif
C
C  Fill all dimension values
C
C
C Get size of nelem
C
      nf_status=NF_INQ_DIMID(nf_fid,'x',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nelem'
      endif
      nf_status=NF_INQ_DIMLEN(nf_fid,nf_vid,nelem)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nelem'
      endif
C
C Get size of nlines
C
      nf_status=NF_INQ_DIMID(nf_fid,'y',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nlines'
      endif
      nf_status=NF_INQ_DIMLEN(nf_fid,nf_vid,nlines)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nlines'
      endif

      call read_goes_np_data(nf_fid,n_ir_elem,n_ir_lines,
     +     csat_id,csat_type,chtype(2),
     +     ilaps_cycle_time, NX_L, NY_L, i4time_earliest,
     +     i4time_latest, lun_out, image_07, istatus)

      nf_status=nf_close(nf_fid)

!     Read VIS channel 02 data
      call make_fnam_lp(i4time_goes,a9time,istatus)
      filename = trim(path)//'/'//a9time//'_vis.nc'
      write(6,*)'gr2/02 fname is ',trim(filename)
C
C  Open netcdf File for reading
C
      nf_status=NF_OPEN(filename,NF_NOWRITE,nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status),filename
        istatus=0
        return
      endif
C
C  Fill all dimension values
C
C
C Get size of nelem
C
      nf_status=NF_INQ_DIMID(nf_fid,'x',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nelem'
      endif
      nf_status=NF_INQ_DIMLEN(nf_fid,nf_vid,nelem)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nelem'
      endif
C
C Get size of nlines
C
      nf_status=NF_INQ_DIMID(nf_fid,'y',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nlines'
      endif
      nf_status=NF_INQ_DIMLEN(nf_fid,nf_vid,nlines)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim nlines'
      endif

      call read_goes_np_data(nf_fid,n_vis_elem,n_vis_lines,
     +     csat_id,csat_type,chtype(1),
     +     ilaps_cycle_time, NX_L, NY_L, i4time_earliest,
     +     i4time_latest, lun_out, image_02, istatus)

      endif

      nf_status=nf_close(nf_fid)

      write(6,*)' call qc_goes_vis'

      call qc_goes_vis(image_02,n_vis_elem,n_vis_lines)

      istatus = 1

      return
      end
