      subroutine rdgr2head(fname, Nx, Ny, xmin, ymin,
     +     offset_x, offset_y,
     +     Dx, Dy, sub_lon_degrees)

      integer nf_fid, nf_vid, nf_status
      integer Nx, Ny 
      real Dx, Dy, xmin, ymin, offset_x, offset_y
      character*(*) fname
      include 'netcdf.inc'
C
C  Open netcdf File for reading
C
      call s_len(fname,nf)
      print*,'Open and read file ',fname(1:nf)
      nf_status = NF_OPEN(fname,NF_NOWRITE,nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'NF_OPEN ',fname(1:nf)
      endif


C   Variables of type REAL
C
C     Variable        NETCDF Long Name
C      Dx           "x grid increment"
C
        nf_status = NF_INQ_VARID(nf_fid,'Dx',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Dx'
      endif
        nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,Dx)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Dx'
      endif
C
C     Variable        NETCDF Long Name
C      Dy           "y grid increment"
C
      nf_status = NF_INQ_VARID(nf_fid,'Dy',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Dy'
      endif
        nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,Dy)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Dy'
      endif
C
C     Variable        NETCDF Long Name
C      nominal_satellite_subpoint_lon 
C
      nf_status = NF_INQ_VARID(nf_fid,'nominal_satellite_subpoint_lon'
     +                        ,nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var nominal_satellite_subpoint_lon'
      endif
      nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,sub_lon_degrees)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var nominal_satellite_subpoint_lon'
      endif

C   Variables of type INT
C
C
C     Variable        NETCDF Long Name
C      Nx           "number of x point"
C
        nf_status = NF_INQ_VARID(nf_fid,'Nx',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Nx'
      endif
        nf_status = NF_GET_VAR_INT(nf_fid,nf_vid,Nx)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Nx'
      endif
C
C     Variable        NETCDF Long Name
C      Ny           "number of y points"
C
        nf_status = NF_INQ_VARID(nf_fid,'Ny',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Ny'
      endif
        nf_status = NF_GET_VAR_INT(nf_fid,nf_vid,Ny)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var Ny'
      endif

C   Variables of type DOUBLE
C
C
C     Variable        NETCDF Long Name
C      reftime      "reference time"
C
        nf_status = NF_INQ_VARID(nf_fid,'reftime',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var reftime'
      endif
        nf_status = NF_GET_VAR_DOUBLE(nf_fid,nf_vid,reftime)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var reftime'
      endif
C
C     Variable        NETCDF Long Name
C      valtime      "valid time"
C
        nf_status = NF_INQ_VARID(nf_fid,'valtime',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var valtime'
      endif
        nf_status = NF_GET_VAR_DOUBLE(nf_fid,nf_vid,valtime)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var valtime'
      endif


C   Variables of type CHAR
C
C
C     Variable        NETCDF Long Name
C      earth_shape  
C
        nf_status = NF_INQ_VARID(nf_fid,'earth_shape',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var earth_shape'
      endif
        nf_status = NF_GET_VAR_TEXT(nf_fid,nf_vid,earth_shape)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var earth_shape'
      endif
C
C     Variable        NETCDF Long Name
C      grid_name    
C
        nf_status = NF_INQ_VARID(nf_fid,'grid_name',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var grid_name'
      endif
        nf_status = NF_GET_VAR_TEXT(nf_fid,nf_vid,grid_name)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var grid_name'
      endif
C
C     Variable        NETCDF Long Name
C      grid_type    "GRIB-1 grid type"
C
        nf_status = NF_INQ_VARID(nf_fid,'grid_type',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var grid_type'
      endif
        nf_status = NF_GET_VAR_TEXT(nf_fid,nf_vid,grid_type)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var grid_type'
      endif
C
C     Variable        NETCDF Long Name
C      origin_name  
C
        nf_status = NF_INQ_VARID(nf_fid,'origin_name',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var origin_name'
      endif
        nf_status = NF_GET_VAR_TEXT(nf_fid,nf_vid,origin_name)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var origin_name'
      endif
C
C     Variable        NETCDF Long Name
C      process_name 
C
        nf_status = NF_INQ_VARID(nf_fid,'process_name',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var process_name'
      endif
        nf_status = NF_GET_VAR_TEXT(nf_fid,nf_vid,process_name)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'in var process_name'
      endif

      return
      end
