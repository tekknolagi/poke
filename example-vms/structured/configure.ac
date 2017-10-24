## This is an -*- Autoconf -*- file to be processed with autoconf
## to obtain a configure script.

## Copyright (C) 2016, 2017 Luca Saiu
## Written by Luca Saiu

## This file is part of the Jitter structured-language example,
## distributed along with Jitter under the same license.

## Jitter is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## Jitter is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with Jitter.  If not, see <http://www.gnu.org/licenses/>.


# Global initialization.
################################################################

AC_PREREQ([2.69])
# There is no real need to keep an independent version number for the
# structured-language example.
AC_INIT([Jittery structured language],
        [1.0],
        [bug-epsilon@gnu.org],
        [structured],
        [http://ageinghacker.net/jitter])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([config.h])


# Automake initialization.
################################################################

AM_INIT_AUTOMAKE


# Check for the C compiler and its features.
################################################################

# We need a recent C compiler.
AC_PROG_CC
AC_PROG_CC_C99

# Check for Jitter.
################################################################

AC_JITTER
if test "x$JITTER" = "x"; then
   AC_MSG_WARN([you need Jitter to regenerate C files])
fi
if test "x$JITTER_CONFIG" = "x"; then
   AC_MSG_ERROR([you need jitter-config to compile])
fi


# Check for frontend C-code-generation tools.
################################################################

# Check for flex.
AM_PROG_LEX
if test "x$LEX" != "xflex"; then
  if test -e "$srcdir/jitterc/jitterc-scanner.c"; then
    AC_MSG_WARN([you will need flex (instead of $LEX) if you modify the
                 distributed C scanners])
  else
    AC_MSG_ERROR([you need flex (instead of $LEX) to generate scanners,
                  if you compile from git])
  fi
fi

# Check for Bison.
AC_PROG_YACC
if ! (echo "x$YACC" | grep -q bison); then
  if test -e "$srcdir/jitterc/jitterc-parser.c"; then
    AC_MSG_WARN([you will need GNU Bison (instead of $YACC) if you modify
                 the distributed C parsers])
  else
    AC_MSG_ERROR([you need GNU Bison (instead of $YACC) to generate parsers,
                  if you compile from git])
  fi
fi


# Output.
################################################################

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
