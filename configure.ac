#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.71])

define([pkgversion], esyscmd([sh -c "grep Version: DESCRIPTION | cut -d' ' -f2 | tr -d '\n'"]))
AC_INIT([RFate],[pkgversion],[maya.gueguen@univ-grenoble-alpes.fr])
AC_CONFIG_SRCDIR([src/main_rcpp.cpp])
#AC_CONFIG_HEADERS([config.h])

# Find R home bin directory
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  AC_MSG_ERROR([cannot determine R_HOME. Make sure you use R CMD INSTALL!])
fi
AC_MSG_NOTICE([R_HOME: ${R_HOME}])
RBIN="${R_HOME}/bin/R"

# Get R version
R_VERSION=`echo 'o <- R.version; cat(paste0(o$major, ".", o$minor))' | ${RBIN} --vanilla --slave`
R_MINOR=`echo $R_VERSION | cut -f2 -d"."`
R_MAJOR=`echo $R_VERSION | cut -f1 -d"."`

# Pick all flags for testing from R
: ${CC=`"${RBIN}" CMD config CC`}
: ${CXX=`"${RBIN}" CMD config CXX`}
: ${CFLAGS=`"${RBIN}" CMD config CFLAGS`}
: ${LDFLAGS=`"${RBIN}" CMD config LDFLAGS`}
: ${CPPFLAGS=`"${RBIN}" CMD config CPPFLAGS`}
: ${CXXFLAGS=`"${RBIN}" CMD config CXXFLAGS`}
# AC_SUBST([CXX],["clang++"])

if test ${R_MAJOR} -ge 4; then
 CXX11=`"${RBIN}" CMD config CXX11`
 CXX11STD=`"${RBIN}" CMD config CXX11STD`
else
 if test ${R_MAJOR} -eq 3 -a ${R_MINOR} -ge 4; then
  CXX11=`"${RBIN}" CMD config CXX11`
  CXX11STD=`"${RBIN}" CMD config CXX11STD`
 else
  CXX11=`"${RBIN}" CMD config CXX1X`
  CXX11STD=`"${RBIN}" CMD config CXX1XSTD`
 fi
fi

CXX="${CXX11} ${CXX11STD}"

if test -n "${CXX11}"; then
  HAVE_CXX11=1
  AC_MSG_NOTICE([C++11 support available])
else
  HAVE_CXX11=0
  AC_MSG_NOTICE([C++11 support not available])
fi

AC_MSG_NOTICE([CXX: ${CXX}])


#GDAL
GDAL_CONFIG="gdal-config"
GDAL_CONFIG_SET="no"

AC_ARG_WITH([gdal-config],
    AS_HELP_STRING([--with-gdal-config=GDAL_CONFIG],[the location of gdal-config]),
           [gdal_config=$withval])
if test [ -n "$gdal_config" ] ; then
    GDAL_CONFIG_SET="yes"
    AC_SUBST([GDAL_CONFIG],["${gdal_config}"])
    AC_MSG_NOTICE(gdal-config set to $GDAL_CONFIG)
fi

if test ["$GDAL_CONFIG_SET" = "no"] ; then
  AC_PATH_PROG([GDAL_CONFIG], ["$GDAL_CONFIG"],["no"])
  if test ["$GDAL_CONFIG" = "no"] ; then
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([gdal-config not found or not executable.])
  fi
else
  AC_MSG_CHECKING(gdal-config exists)
  if test -r "${GDAL_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([gdal-config not found - configure argument error.])
  fi
  AC_MSG_CHECKING(gdal-config executable)
  if test -x "${GDAL_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([gdal-config not executable.])
  fi
fi

AC_MSG_CHECKING(gdal-config usability)
if test `${GDAL_CONFIG} --version`;
then
  GDAL_CPPFLAGS=`${GDAL_CONFIG} --cflags`
  GDAL_LIBS=`${GDAL_CONFIG} --libs`
  GDAL_VERSION=`${GDAL_CONFIG} --version`
  GDAL_DEP_LIBS=`${GDAL_CONFIG} --dep-libs`
  GDAL_DATADIR=`${GDAL_CONFIG} --datadir`
  gdalok=yes
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
	echo "Error: gdal-config not found"
	echo "The gdal-config script distributed with GDAL could not be found."
	echo "If you have not installed the GDAL libraries, you can"
	echo "download the source from  http://www.gdal.org/"
	echo "If you have installed the GDAL libraries, then make sure that"
	echo "gdal-config is in your path. Try typing gdal-config at a"
	echo "shell prompt and see if it runs. If not, use:"
	echo " --configure-args='--with-gdal-config=/usr/local/bin/gdal-config'"
	echo "with appropriate values for your installation."
	echo ""

	exit 1
fi

AC_MSG_NOTICE([GDAL: ${GDAL_VERSION}])
#GDAL_VER_DOT=`echo $GDAL_VERSION | tr -d "."`
GDAL_MAJOR=`echo $GDAL_VERSION | cut -f1 -d"."`
GDAL_MINOR=`echo $GDAL_VERSION | cut -f2 -d"."`
GDAL_PATCH=`echo $GDAL_VERSION | cut -f3 -d"."`

if test ${GDAL_MAJOR} = 2 -a ${GDAL_MINOR} -ge 3 ; then
  AC_MSG_CHECKING([C++11 support for GDAL >= 2.3.0])
  if test ${HAVE_CXX11} = 1 ; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([provide at least CXX11 compiler support for GDAL >= 2.3.0])
  fi
fi

AC_MSG_CHECKING([GDAL version >= 1.11.4])
if test ${GDAL_MAJOR} -gt 1 ; then
  AC_MSG_RESULT(yes)
else
  if test ${GDAL_MAJOR} = 1 -a ${GDAL_MINOR} = 11 -a ${GDAL_PATCH} -ge 4 ; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([upgrade GDAL to 1.11.4 or later])
  fi
fi


AC_MSG_CHECKING([GDAL version <= 2.5 or >= 3.0])
if test ${GDAL_MAJOR} -lt 2 -o ${GDAL_MAJOR} -gt 2; then
  AC_MSG_RESULT(yes)
else
  if test ${GDAL_MAJOR} = 2; then
    if test ${GDAL_MINOR} -le 5 ; then
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
      AC_MSG_ERROR([GDAL version out of bounds])
    fi
  fi
fi


INLIBS="${LIBS}"
INCPPFLAGS="${CPPFLAGS}"
INPKG_CPPFLAGS="${PKG_CPPFLAGS}"
INPKG_LIBS="${PKG_LIBS}"

AC_SUBST([PKG_CPPFLAGS], ["${INPKG_CPPFLAGS} ${GDAL_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${INPKG_LIBS} ${GDAL_LIBS}"])

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${INCPPFLAGS} ${PKG_CPPFLAGS}"


NEED_DEPS=no
LIBS="${INLIBS} ${PKG_LIBS}"
[cat > gdal_test.cc <<_EOCONF
#include <gdal.h>
#ifdef __cplusplus
extern "C" {
#endif
int main() {
GDALAllRegister();
}
#ifdef __cplusplus
}
#endif
_EOCONF]

AC_MSG_CHECKING(gdal: linking with --libs only)
${CXX} ${CPPFLAGS} -o gdal_test gdal_test.cc ${LIBS} 2> errors.txt
if test `echo $?` -ne 0 ; then
  gdalok=no
  AC_MSG_RESULT(no)
else
  AC_MSG_RESULT(yes)
fi

if test "${gdalok}" = no; then
  AC_MSG_CHECKING(gdal: linking with --libs and --dep-libs)
  LIBS="${LIBS} ${GDAL_DEP_LIBS}"
  gdalok=yes
  ${CXX} ${CPPFLAGS} -o gdal_test gdal_test.cc ${LIBS} 2>> errors.txt
  if test `echo $?` -ne 0 ; then
    gdalok=no
  fi
  if test "${gdalok}" = yes; then
    NEED_DEPS=yes
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
  fi
fi

if test "${gdalok}" = no; then
  cat errors.txt
  AC_MSG_NOTICE([Install failure: compilation and/or linkage problems.])
  AC_MSG_ERROR([GDALAllRegister not found in libgdal.])
fi

rm -f gdal_test errors.txt gdal_test.cc

AC_MSG_CHECKING(GDAL: gdal-config data directory readable)
if test -d "${GDAL_DATADIR}" ; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  AC_MSG_CHECKING(GDAL: GDAL_DATA data directory readable)
  if test -z ${GDAL_DATA} ; then
    AC_MSG_ERROR([GDAL data directory not found.])
  else
    if test -d "${GDAL_DATA}" ; then
      AC_MSG_RESULT(yes)
      AC_SUBST([GDAL_DATADIR], [${GDAL_DATA}])
    else
      AC_MSG_RESULT(no)
      AC_MSG_ERROR([GDAL data directory not found.])
    fi
  fi
fi

GDAL_GE_250="no"
GDAL_MAJ_VER=`echo $GDAL_VERSION | cut -d "." -f1`
GDAL_MOD_VER=`echo $GDAL_VERSION | cut -d "." -f2`
if test "${GDAL_MAJ_VER}" = 2 ; then
  if test "${GDAL_MOD_VER}" -ge 5 ; then
    GDAL_GE_250="yes"
  fi
else
  if test "${GDAL_MAJ_VER}" -ge 3 ; then
    GDAL_GE_250="yes"
  fi
fi

GDAL_DATA_TEST_FILE="${GDAL_DATADIR}/pcs.csv"
AC_MSG_CHECKING(GDAL: ${GDAL_DATADIR}/pcs.csv readable)
if test -r "${GDAL_DATA_TEST_FILE}" ; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  if test "${GDAL_GE_250}" = no; then
    AC_MSG_ERROR([pcs.csv not found in GDAL data directory.])
  fi
fi

# PROJ.4

# copied from sf/configure.ac
# updated copy 2022/11/07 to adjust for PROJ7 and PROJ8

PROJ_CONFIG="pkg-config proj"

if `$PROJ_CONFIG --exists` ; then
  AC_MSG_NOTICE([pkg-config proj exists, will use it])
  proj_config_ok=yes
else
  proj_config_ok=no
fi

AC_ARG_WITH([proj-include],
    AS_HELP_STRING([--with-proj-include=DIR],[location of proj header files]),
    [proj_include_path=$withval])
if test [ -n "$proj_include_path" ] ; then
   AC_SUBST([PROJ_CPPFLAGS],["-I${proj_include_path}"])
else
  if test "${proj_config_ok}" = yes; then
    PROJ_INCLUDE_PATH=`${PROJ_CONFIG} --cflags`
    AC_SUBST([PROJ_CPPFLAGS],["${PROJ_INCLUDE_PATH}"])
  fi
fi

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt

AC_ARG_WITH([proj-api],
    AS_HELP_STRING([--with-proj-api=yes/no],[use the deprecated proj_api.h even when PROJ 6 is available; default no]),
               [proj_api=$withval])
PROJ6="no"
PROJH="no"
if test "${proj_config_ok}" = yes; then
  PROJ_VERSION=`${PROJ_CONFIG} --modversion`
  PROJV1=`echo "${PROJ_VERSION}" | cut -c 1`
  if test "${PROJV1}" -ge 6; then
    PROJ6="yes"
    PROJ_CPPFLAGS="${PROJ_CPPFLAGS} -DHAVE_PROJ_H"
    if test "${proj_api}" = yes; then
        AC_MSG_NOTICE([using proj_api.h even with PROJ 5/6])
        PROJ_CPPFLAGS="${PROJ_CPPFLAGS} -DACCEPT_USE_OF_DEPRECATED_PROJ_API_H"
    else
        AC_MSG_NOTICE([using proj.h.])
        PROJH="yes"
    fi
  fi
else
  if test "${PROJH}" = no ; then
    PROJH=yes
    AC_CHECK_HEADERS(proj.h,,PROJH=no)
    if test "${PROJH}" = yes; then
      PROJ6="yes"
      PROJ_CPPFLAGS="${PROJ_CPPFLAGS} -DHAVE_PROJ_H"
    fi
  fi
fi

CPPFLAGS="${INCPPFLAGS} ${PKG_CPPFLAGS} ${PROJ_CPPFLAGS}"

if test "${PROJH}" = no
then
  proj4ok=yes
  AC_CHECK_HEADERS(proj_api.h,,proj4ok=no)
  if test "${proj4ok}" = no; then
     AC_MSG_ERROR([proj_api.h not found in standard or given locations.])
  fi
fi

# dnl ditto for a library path
AC_ARG_WITH([proj-lib],
    AS_HELP_STRING([--with-proj-lib=LIB_PATH],[the location of proj libraries]),
               [proj_lib_path=$withval])
if test [ -n "$proj_lib_path" ] ; then
    AC_SUBST([PROJ_LIBS], ["-L${proj_lib_path} ${INPKG_LIBS} -lproj"])
else
  if test "${proj_config_ok}" = yes; then
    if test `uname` = "Darwin"; then
      PROJ_LIB_PATH=`${PROJ_CONFIG} --libs --static`
    else
      PROJ_LIB_PATH=`${PROJ_CONFIG} --libs`
    fi
    AC_SUBST([PROJ_LIBS], ["${PROJ_LIB_PATH} ${INPKG_LIBS}"])
	proj_version=`${PROJ_CONFIG} --modversion`
	AC_MSG_NOTICE([PROJ: ${proj_version}])
  else
    PROJ_LIBS="${PKG_LIBS} -lproj"
  fi
fi

LIBS="${PROJ_LIBS} ${INLIBS} ${PKG_LIBS}"

if test "${PROJH}" = no; then
  proj4ok=yes
  AC_CHECK_LIB(proj,pj_init_plus,,proj4ok=no)
  if test "${proj4ok}" = no; then
     AC_MSG_ERROR([libproj not found in standard or given locations.])
  fi
[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <stdlib.h>
#include <proj_api.h>
int main(void) {
    printf("%d\n", PJ_VERSION);
    exit(0);
}
_EOCONF]
else
[cat > proj_conf_test.cpp <<_EOCONF
#include <stdio.h>
#include <stdlib.h>
#include <proj.h>
int main(void) {
	proj_context_create();
    exit(0);
}
_EOCONF]
  #AC_CHECK_LIB(proj,proj_context_create,,proj6ok=no)
  AC_MSG_CHECKING(PROJ: checking whether PROJ and sqlite3 are available for linking:)
  ${CXX} ${CPPFLAGS} -o proj_conf_test proj_conf_test.cpp ${LIBS} $SQLITE3_LIBS -lsqlite3 2> errors.txt
  if test `echo $?` -ne 0 ; then
    proj6ok=no
    AC_MSG_RESULT(no)
  else
    proj6ok=yes
    AC_MSG_RESULT(yes)
  fi
  if test "${proj6ok}" = no; then
     AC_MSG_ERROR([libproj or sqlite3 not found in standard or given locations.])
  fi

[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <stdlib.h>
#include <proj.h>
int main(void) {
    printf("%d.%d.%d\n", PROJ_VERSION_MAJOR, PROJ_VERSION_MINOR, PROJ_VERSION_PATCH);
    exit(0);
}
_EOCONF]
fi

#AC_MSG_NOTICE([PKG_LIBS: ${PKG_LIBS}])

${CC} ${CFLAGS} ${CPPFLAGS} -o proj_conf_test proj_conf_test.c ${PROJ_LIBS}

proj_version=`./proj_conf_test`

AC_ARG_WITH([proj-share],
    AS_HELP_STRING([--with-proj-share=SHARE_PATH],[the location of proj metadata files]),
               [proj_share_path=$withval])
if test [ -n "$proj_share_path" ] ; then
    AC_MSG_NOTICE([PROJ_LIB: ${proj_share_path}])
fi

if test ${PROJ6} = "no"; then

[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>
#if PJ_VERSION <= 480
FILE *pj_open_lib(projCtx, const char *, const char *);
#endif
int main(void) {
#if PJ_VERSION <= 480
    FILE *fp;
#else
    PAFile fp;
#endif
    projCtx ctx;
    ctx = pj_get_default_ctx();
    fp = pj_open_lib(ctx, "epsg", "rb");
    if (fp == NULL) exit(1);
#if PJ_VERSION <= 480
    fclose(fp);
#else
    pj_ctx_fclose(ctx, fp);
#endif
    exit(0);
}
_EOCONF]

${CC} ${CFLAGS} ${CPPFLAGS} -o proj_conf_test proj_conf_test.c ${PROJ_LIBS}
if test [ -n "$proj_share_path" ] ; then
  PROJ_LIB="${proj_share_path}" ./proj_conf_test
  proj_share=`echo $?`
else
  ./proj_conf_test
  proj_share=`echo $?`
fi

AC_MSG_CHECKING(PROJ: epsg found and readable)
if test ${proj_share} -eq 1 ; then
    AC_MSG_RESULT(no)
    STOP="stop"
else
    AC_MSG_RESULT(yes)
fi

rm -f proj_conf_test.c proj_conf_test

if test "$STOP" = "stop" ; then
    echo "Error: proj/epsg not found"
    echo "Either install missing proj support files, for example"
    echo "the proj-nad and proj-epsg RPMs on systems using RPMs,"
    echo "or if installed but not autodetected, set PROJ_LIB to the"
    echo "correct path, and if need be use the --with-proj-share="
    echo "configure argument."
    exit 1
fi

else # proj >= 6

if test "${PROJH}" = no; then

[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>
int main(void) {
    PAFile fp;
    projCtx ctx;
    ctx = pj_get_default_ctx();
    fp = pj_open_lib(ctx, "proj.db", "rb");
    if (fp == NULL) exit(1);
    pj_ctx_fclose(ctx, fp);
    exit(0);
}
_EOCONF]

${CC} ${CFLAGS} ${CPPFLAGS} -o proj_conf_test proj_conf_test.c ${PROJ_LIBS}
if test [ -n "$proj_share_path" ] ; then
  PROJ_LIB="${proj_share_path}" ./proj_conf_test
  proj_share=`echo $?`
else
  ./proj_conf_test
  proj_share=`echo $?`
fi

AC_MSG_CHECKING(PROJ: proj.db found and readable)
if test ${proj_share} -eq 1 ; then
    AC_MSG_RESULT(no)
    STOP="stop"
else
    AC_MSG_RESULT(yes)
fi

rm -f proj_conf_test.c proj_conf_test

if test "$STOP" = "stop" ; then
    echo "Error: proj/proj.db not found"
    echo "Either install missing proj support files, set PROJ_LIB to the"
    echo "correct path, and if need be use the --with-proj-share="
    echo "configure argument."
    exit 1
fi

[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>
#if PJ_VERSION <= 480
FILE *pj_open_lib(projCtx, const char *, const char *);
#endif
int main(void) {
#if PJ_VERSION <= 480
    FILE *fp;
#else
    PAFile fp;
#endif
    projCtx ctx;
    ctx = pj_get_default_ctx();
    fp = pj_open_lib(ctx, "conus", "rb");
    if (fp == NULL) exit(1);
#if PJ_VERSION <= 480
    fclose(fp);
#else
    pj_ctx_fclose(ctx, fp);
#endif
    exit(0);
}
_EOCONF]

${CC} ${CFLAGS} ${CPPFLAGS} -o proj_conf_test proj_conf_test.c ${PROJ_LIBS}
if test [ -n "$proj_share_path" ] ; then
  PROJ_LIB="${proj_share_path}" ./proj_conf_test
  proj_share=`echo $?`
else
  ./proj_conf_test
  proj_share=`echo $?`
fi

AC_MSG_CHECKING(PROJ: conus found and readable)
if test ${proj_share} -eq 1 ; then
    WARN="warn"
    AC_MSG_RESULT(no)
else
    AC_MSG_RESULT(yes)
fi

rm -f proj_conf_test.c proj_conf_test

if test "$WARN" = "warn" ; then
    echo "Note: proj/conus not found"
    echo "No support available in PROJ4 for NAD grid datum transformations"
    echo "If required, consider re-installing from source with the contents"
    echo "of proj-datumgrid-1.<latest>.zip from http://download.osgeo.org/proj/ in nad/."
fi

fi # PROJH = no

fi # proj >= 6


# Optional local copy of GDAL datadir and PROJ_LIB

data_copy=no
AC_ARG_WITH([data-copy],
    AS_HELP_STRING([--with-data-copy=yes/no],[local copy of data directories in package, default no]),
               [data_copy=$withval])
if test "${data_copy}" = "yes" ; then
AC_MSG_NOTICE([Copy data for:])
  proj_lib0="${PROJ_LIB}"
  AC_ARG_WITH([proj-data],
    AS_HELP_STRING([--with-proj-data=DIR],[location of PROJ.4 data directory]),
    [proj_lib1=$withval])
  if test -n "${proj_lib0}" ; then
    proj_lib="${proj_lib0}"
  else
    proj_lib="${proj_lib1}"
  fi
  if test -n "${proj_lib}" ; then
    if test -d "${proj_lib}" ; then
      cp -r "${proj_lib}" "${R_PACKAGE_DIR}"
      AC_MSG_NOTICE([  PROJ.4: ${proj_lib}])
    else
      AC_MSG_ERROR([PROJ.4 data files not found; set environment variable PROJ_LIB=DIR or --with-proj-data=DIR.])
    fi
  else
      AC_MSG_ERROR([PROJ.4 data files not found; set environment variable PROJ_LIB=DIR or --with-proj-data=DIR.])
  fi

  if test -d "${GDAL_DATADIR}" ; then
    cp -r "${GDAL_DATADIR}" "${R_PACKAGE_DIR}"
    AC_MSG_NOTICE([  GDAL: ${GDAL_DATADIR}])
  else
    AC_MSG_ERROR([GDAL data files not found.])
  fi
fi


# concluding substitution

AC_SUBST([PKG_CPPFLAGS], ["${INPKG_CPPFLAGS} ${GDAL_CPPFLAGS}"])
if test [ -n "$proj_include_path" ] ; then
  AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS} ${proj_include_path}"])
fi
if test ${PROJ6} = "yes" ; then
  AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS} -DACCEPT_USE_OF_DEPRECATED_PROJ_API_H"])
fi

AC_SUBST([PKG_LIBS], ["${INPKG_LIBS} ${GDAL_LIBS}"])
if test "${NEED_DEPS}" = yes; then
   AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${GDAL_DEP_LIBS}"])
fi
if test [ -n "$proj_lib_path" ] ; then
  AC_SUBST([PKG_LIBS], ["${PKG_LIBS} -L${proj_lib_path}"])
fi
if test [ -n "$PROJ_LIB_PATH" ] ; then
  AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${PROJ_LIB_PATH}"])
else
  AC_SUBST([PKG_LIBS], ["${PKG_LIBS} -lproj"])
fi

AC_MSG_NOTICE([Package CPP flags: ${PKG_CPPFLAGS}])
AC_MSG_NOTICE([Package LIBS: ${PKG_LIBS}])

AC_CONFIG_FILES(src/Makevars)
AC_OUTPUT
