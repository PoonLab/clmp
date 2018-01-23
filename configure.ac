AC_INIT()

CLMP_CPPFLAGS="-I/opt/local/include/igraph -I/usr/local/include/igraph -I/usr/include/igraph"

LIBS="-ligraph -L/usr/local/lib -L/opt/local/lib -lgsl"
if test `uname` = "Darwin" ; then
  LIBS="${LIBS} -ll"
else
  LIBS="${LIBS} -lfl"
fi

AC_SUBST(CLMP_CPPFLAGS)
AC_SUBST(LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
