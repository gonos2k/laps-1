# User options are in src/include/makefile.inc
# This Makefile is designed for gnu make version 3.75 or newer
# Please report problems to lapsbugs@fsl.noaa.gov
#
#

SRCROOT=../../..


include $(SRCROOT)/src/include/makefile.inc

KRCFLAGS=-g $(INC)

DRTUFLAG = -Drtu








.SUFFIXES:
.SUFFIXES: .o .c .f .f90 .F .F90 .a

LIB      = libcloud.a
DEBUGLIB = libdebugcloud.a



ifdef CRTMROOT
SRC= \
    accel.f antialias_ellipse.f90 bilinear_fill.f90 calc_allsky.f clock.f cloud_var.F90 color_routines.f90 compare_analysis_to_radiometer.f crtm_kmatrix.f90 cvr_to_tb8_effective.f get_airglow.f90 get_airmass.f90 get_aod_3d.f90 get_cloud_pf.f90 get_cloud_rays.f get_land_pf.f90 get_cloud_rad.f90 get_cloud_rad_faces.f90 get_cloud_rad_faces2.f90 \
    get_clr_src_dir.f90 get_scat_pf.f90 get_uprad_lyr.f90 get_sky_rgb.f90 get_sky_rgb_routines.f90 greatcircle.f90 hotstart_perturbation.f moon_brwn.f mscat_phase.f90 obliquity.f phase.f phase_func_moon.f90 places_routines.f posin.f posint.f preces.f \
    delta_t.f topo_ff1.f xyztopolarr.f equtoaltazr.f refract.f cjymd.f magnitude.f opac_path.f90 optmiz.f \
    read_camera_clouds.f90 solar_routines.f allsky.f90 get_elong_a.f90 interp_1d.f90 region_grow.f90 sidereal_time.f skyglow_cyl.f90 skyglow_phys.f90 skyglow_phys_twi.f90 sun_moon.f sun_planet.f tdust_sub.f90 topo.f vi.f90 vi2.f90
else
SRC= \
    accel.f antialias_ellipse.f90 bilinear_fill.f90 calc_allsky.f clock.f cloud_var.F90 color_routines.f90 compare_analysis_to_radiometer.f cvr_to_tb8_effective.f get_airglow.f90 get_airmass.f90 get_aod_3d.f90 get_cloud_pf.f90 get_cloud_rad.f90 get_land_pf.f90 get_cloud_rays.f get_cloud_rad_faces.f90 get_cloud_rad_faces2.f90 \
    get_clr_src_dir.f90 get_scat_pf.f90 get_uprad_lyr.f90 get_sky_rgb.f90 get_sky_rgb_routines.f90 greatcircle.f90 hotstart_perturbation.f moon_brwn.f mscat_phase.f90 obliquity.f phase.f phase_func_moon.f90 places_routines.f posin.f posint.f preces.f \
    delta_t.f topo_ff1.f xyztopolarr.f equtoaltazr.f refract.f cjymd.f magnitude.f opac_path.f90 optmiz.f \
    read_camera_clouds.f90 solar_routines.f allsky.f90 get_elong_a.f90 interp_1d.f90 region_grow.f90 sidereal_time.f skyglow_cyl.f90 skyglow_phys.f90 skyglow_phys_twi.f90 sun_moon.f sun_planet.f tdust_sub.f90 topo.f vi.f90 vi2.f90
endif

ARCHIVE = $(LIB)($(addsuffix .o, $(basename $(SRC))))
DBARCHIVE = $(DEBUGLIB)($(addsuffix .o, $(basename $(SRC))))


all: lib

lib: $(LIB)

$(LIB): $(ARCHIVE) 
	$(RANLIB) $@

debug: $(SRC)
	$(MAKE) lib LIB='$(DEBUGLIB)' ARCHIVE='$(DBARCHIVE)' \
	CFLAGS='$(DBCFLAGS)' FFLAGS='$(DBFLAGS)'

install: lib

clean:
	rm -f $(LIB) $(DEBUGLIB) *.o *~ *#


distclean: clean


.f90.o:;    $(FC) $(FFLAGS) $(FREE) $(STINC) -c $*.f90
.F90.o:;    $(FC) $(FFLAGS) $(FREE) $(STINC) -c $*.F90
.f.o:;      $(FC) $(FFLAGS) -c $*.f
.F.o:;      $(FC) $(FFLAGS) -c $*.F
